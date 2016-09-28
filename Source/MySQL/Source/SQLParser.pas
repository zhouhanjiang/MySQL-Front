unit SQLParser;

interface {********************************************************************}

// SQL Syntax updated with MySQL 5.7.14

uses
  Classes;

type
  TSQLParser = class
  public
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
      PE_NestedCondCode = 7; // Nested conditional MySQL options

      // Bugs while parsing Root
      PE_UnknownStmt = 8; // Unknown statement

      MaxIdentLength = 64;

      MySQLDatatypes =
        'BIGINT,BINARY,BIT,BLOB,BOOL,BOOLEAN,CHAR,CHARACTER,DEC,DECIMAL,DATE,' +
        'DATETIME,DOUBLE,ENUM,FLOAT,GEOMETRY,GEOMETRYCOLLECTION,INT,INT4,INTEGER,' +
        'LARGEINT,LINESTRING,JSON,LONG,LONGBLOB,LONGTEXT,MEDIUMBLOB,' +
        'MEDIUMINT,MEDIUMTEXT,MULTILINESTRING,MULTIPOINT,MULTIPOLYGON,' +
        'NUMBER,NUMERIC,NCHAR,NVARCHAR,POINT,POLYGON,REAL,SERIAL,SET,SIGNED,' +
        'SMALLINT,TEXT,TIME,TIMESTAMP,TINYBLOB,TINYINT,TINYTEXT,UNSIGNED,' +
        'VARBINARY,VARCHAR,YEAR';

      MySQLEngineTypes =
        'ARCHIVE,BDB,BERKELEYDB,BLACKHOLE,CSV,EXAMPLE,FEDERATED,HEAP,INNOBASE,' +
        'InnoDB,ISAM,MEMORY,MERGE,MRG_ISAM,MRG_MYISAM,MyISAM,NDB,NDBCLUSTER';

      MySQLFunctions =
        'ABS,ACOS,ADDDATE,ADdiME,AES_DECRYPT,AES_ENCRYPT,ANY_VALUE,AREA,' +
        'ASBINARY,ASCII,ASIN,ASTEXT,ASWKBASWKT,ASYMMETRIC_DECRYPT,' +
        'ASYMMETRIC_DERIVE,ASYMMETRIC_ENCRYPT,ASYMMETRIC_SIGN,ASYMMETRIC_VERIFY,' +
        'ATAN,ATAN,ATAN2,AVG,BENCHMARK,BIN,BIT_AND,BIT_COUNT,BIT_LENGTH,BIT_OR,' +
        'BIT_XOR,BUFFER,CAST,CEIL,CEILING,CENTROID,CHAR,CHAR_LENGTH,' +
        'CHARACTER_LENGTH,CHARSET,COALESCE,COERCIBILITY,COLLATION,COMPRESS,' +
        'CONCAT,CONCAT_WS,CONNECTION_ID,CONTAINS,CONV,CONVERT,CONVERT_TZ,' +
        'CONVEXHULL,COS,COT,COUNT,CRC32,CREATE_ASYMMETRIC_PRIV_KEY,' +
        'CREATE_ASYMMETRIC_PUB_KEY,CREATE_DH_PARAMETERS,CREATE_DIGEST,CROSSES,' +
        'CURDATE,CURRENT_DATE,CURRENT_TIME,CURRENT_TIMESTAMP,CURRENT_USER,' +
        'CURTIME,DATABASE,DATE,DATE_ADD,DATE_FORMAT,DATE_SUB,DATEDIFF,DAY,' +
        'DAYNAME,DAYOFMONTH,DAYOFWEEK,DAYOFYEAR,DECODE,DEFAULT,DEGREES,' +
        'DES_DECRYPT,DES_ENCRYPT,DIMENSION,DISJOINT,DISTANCE,ELT,ENCODE,ENCRYPT,' +
        'ENDPOINT,ENVELOPE,EQUALS,EXP,EXPORT_SET,EXTERIORRING,EXTRACT,' +
        'EXTRACTVALUE,FIELD,FIND_IN_SET,FLOOR,FORMAT,FOUND_ROWS,FROM_BASE64,' +
        'FROM_DAYS,FROM_UNIXTIME,GEOMCOLLFROMTEXT,GEOMCOLLFROMWKB,' +
        'GEOMETRYCOLLECTION,GEOMETRYCOLLECTIONFROMTEXT,GEOMETRYCOLLECTIONFROMWKB,' +
        'GEOMETRYFROMTEXT,GEOMETRYFROMWKB,GEOMETRYN,GEOMETRYTYPE,GEOMFROMTEXT,' +
        'GEOMFROMWKB,GET_FORMAT,GET_LOCK,GLENGTH,GREATEST,GROUP_CONCAT,' +
        'GTID_SUBSET,GTID_SUBTRACT,HEX,HOUR,IF,IFNULL,IN,INET_ATON,INET_NTOA,' +
        'INET6_ATON,INET6_NTOA,INSERT,INSTR,INTERIORRINGN,INTERSECTS,INTERVAL,' +
        'IS_FREE_LOCK,IS_IPV4,IS_IPV4_COMPAT,IS_IPV4_MAPPED,IS_IPV6,IS_USED_LOCK,' +
        'ISCLOSED,ISEMPTY,ISNULL,ISSIMPLE,JSON_APPEND,JSON_ARRAY,' +
        'JSON_ARRAY_APPEND,JSON_ARRAY_INSERT,JSON_CONTAINS,JSON_CONTAINS_PATH,' +
        'JSON_DEPTH,JSON_EXTRACT,JSON_INSERT,JSON_KEYS,JSON_LENGTH,JSON_MERGE,' +
        'JSON_OBJECT,JSON_QUOTE,JSON_REMOVE,JSON_REPLACE,JSON_SEARCH,JSON_SET,' +
        'JSON_TYPE,JSON_UNQUOTE,JSON_VALID,LAST_DAY,LAST_INSERT_ID,LCASE,LEAST,' +
        'LEFT,LENGTH,LINEFROMTEXT,LINEFROMWKB,LINESTRING,LINESTRINGFROMTEXT,' +
        'LINESTRINGFROMWKB,LN,LOAD_FILE,LOCALTI,LOCALTIME,LOCALTIMESTAMP,LOCATE,' +
        'LOG,LOG10,LOG2,LOWER,LPAD,LTRIM,MAKE_SET,MAKEDATE,MAKETIME,' +
        'MASTER_POS_WAIT,MAX,MBRCONTAINS,MBRCOVEREDBY,MBRCOVERS,MBRDISJOINT,' +
        'MBREQUAL,MBREQUALS,MBRINTERSECTS,MBROVERLAPS,MBRTOUCHES,MBRWITHIN,MD5,' +
        'MICROSECOND,MID,MIN,MINUTE,MLINEFROMTEXT,MLINEFROMWKB,MOD,MONTH,' +
        'MONTHNAME,MPOINTFROMTEXT,MPOINTFROMWKB,MPOLYFROMTEXT,MPOLYFROMWKB,' +
        'MULTILINESTRING,MULTILINESTRINGFROMTEXT,MULTILINESTRINGFROMWKB,' +
        'MULTIPOINT,MULTIPOINTFROMTEXT,MULTIPOINTFROMWKB,MULTIPOLYGON,' +
        'MULTIPOLYGONFROMTEXT,MULTIPOLYGONFROMWKB,NAME_CONST,NOW,NULLIF,' +
        'NUMGEOMETRIES,NUMINTERIORRINGS,NUMPOINTS,OCT,OCTET_LENGTH,OLD_PASSWORD,' +
        'ORD,OVERLAPS,PASSWORD,PERIOD_ADD,PERIOD_DIFF,PI,POINT,POINTFROMTEXT,' +
        'POINTFROMWKB,POINTN,POLYFROMTEXT,POLYFROMWKB,POLYGON,POLYGONFROMTEXT,' +
        'POLYGONFROMWKB,POSITION,POW,POWER,QUARTER,QUOTE,RADIANS,RAND,' +
        'RANDOM_BYTES,RELEASE_ALL_LOCKS,RELEASE_LOCK,REPEAT,REPLACE,REVERSE,' +
        'RIGHT,ROUND,ROW_COUNT,RPAD,RTRIM,SCHEMA,SEC_TO_TIME,SECOND,SESSION_USER,' +
        'SHA,SHA1,SHA2,SIGN,SIN,SLEEP,SOUNDEX,SPACE,SQRT,SRID,ST_AREA,' +
        'ST_ASBINARY,ST_ASGEOJSON,ST_ASTEXT,ST_ASWKB,ST_ASWKT,ST_BUFFER,' +
        'ST_BUFFER_STRATEGY,ST_CENTROID,ST_CONTAINS,ST_CONVEXHULL,ST_CROSSES,' +
        'ST_DIFFERENCE,ST_DIMENSION,ST_DISJOINT,ST_DISTANCE,ST_DISTANCE_SPHERE,' +
        'ST_ENDPOINT,ST_ENVELOPE,ST_EQUALS,ST_EXTERIORRING,ST_GEOHASH,' +
        'ST_GEOMCOLLFROMTEXT,ST_GEOMCOLLFROMTXT,ST_GEOMCOLLFROMWKB,' +
        'ST_GEOMETRYCOLLECTIONFROMTEXT,ST_GEOMETRYCOLLECTIONFROMWKB,' +
        'ST_GEOMETRYFROMTEXT,ST_GEOMETRYFROMWKB,ST_GEOMETRYN,ST_GEOMETRYTYPE,' +
        'ST_GEOMFROMGEOJSON,ST_GEOMFROMTEXT,ST_GEOMFROMWKB,ST_INTERIORRINGN,' +
        'ST_INTERSECTION,ST_INTERSECTS,ST_ISCLOSED,ST_ISEMPTY,ST_ISSIMPLE,' +
        'ST_ISVALID,ST_LATFROMGEOHASH,ST_LENGTH,ST_LINEFROMTEXT,ST_LINEFROMWKB,' +
        'ST_LINESTRINGFROMTEXT,ST_LINESTRINGFROMWKB,ST_LONGFROMGEOHASH,' +
        'ST_MAKEENVELOPE,ST_MLINEFROMTEXT,ST_MLINEFROMWKB,ST_MPOINTFROMTEXT,' +
        'ST_MPOINTFROMWKB,ST_MPOLYFROMTEXT,ST_MPOLYFROMWKB,' +
        'ST_MULTILINESTRINGFROMTEXT,ST_MULTILINESTRINGFROMWKB,' +
        'ST_MULTIPOINTFROMTEXT,ST_MULTIPOINTFROMWKB,ST_MULTIPOLYGONFROMTEXT,' +
        'ST_MULTIPOLYGONFROMWKB,ST_NUMGEOMETRIES,ST_NUMINTERIORRING,' +
        'ST_NUMINTERIORRINGS,ST_NUMPOINTS,ST_OVERLAPS,ST_POINTFROMGEOHASH,' +
        'ST_POINTFROMTEXT,ST_POINTFROMWKB,ST_POINTN,ST_POLYFROMTEXT,' +
        'ST_POLYFROMWKB,ST_POLYGONFROMTEXT,ST_POLYGONFROMWKB,ST_SIMPLIFY,ST_SRID,' +
        'ST_STARTPOINT,ST_SYMDIFFERENCE,ST_TOUCHES,ST_UNION,ST_VALIDATE,' +
        'ST_WITHIN,ST_X,ST_Y,STARTPOINT,STD,STDDEV,STDDEV_POP,STDDEV_SAMP,' +
        'STR_TO_DATE,STRCMP,SUBDATE,SUBSTR,SUBSTRING,SUBSTRING_INDEX,SUBTIME,SUM,' +
        'SYSDATE,SYSTEM_USER,TAN,TIME,TIME_FORMAT,TIME_TO_SEC,TIMEDIFF,TIMESTAMP,' +
        'TIMESTAMPADD,TIMESTAMPDIFF,TO_BASE64,TO_DAYS,TO_SECONDS,TOUCHES,TRIM,' +
        'TRUNCATE,UCASE,UNCOMPRESS,UNCOMPRESSED_LENGTH,UNHEX,UNIX_TIMESTAMP,' +
        'UPDATEXML,UPPER,USER,UTC_DATE,UTC_TIME,UTC_TIMESTAMP,UUID,UUID_SHORT,' +
        'VALIDATE_PASSWORD_STRENGTH,VALUES,VAR_POP,VAR_SAMP,VARIANCE,VERSION,' +
        'WAIT_FOR_EXECUTED_GTID_SET,WAIT_UNTIL_SQL_THREAD_AFTER_GTIDS,WEEK,' +
        'WEEKDAY,WEEKOFYEAR,WEIGHT_STRING,WITHIN,X,Y,YEAR,YEARWEEK';

      MySQLKeywords =
        'ANY,SOME,' +

        'ACCOUNT,ACTION,ADD,AFTER,AGAINST,ALGORITHM,ALL,ALTER,ALWAYS,ANALYZE,AND,' +
        'AS,ASC,ASCII,AT,AUTO_INCREMENT,AUTHORS,AVG_ROW_LENGTH,BEFORE,BEGIN,' +
        'BETWEEN,BINARY,BINLOG,BLOCK,BOOLEAN,BOTH,BTREE,BY,CACHE,CALL,CASCADE,' +
        'CASCADED,CASE,CATALOG_NAME,CHANGE,CHANGED,CHANNEL,CHAIN,CHARACTER,' +
        'CHARSET,CHECK,CHECKSUM,CLASS_ORIGIN,CLIENT,CLOSE,COALESCE,CODE,COLLATE,' +
        'COLLATION,COLUMN,COLUMN_FORMAT,COLUMN_NAME,COLUMNS,COMMENT,COMMIT,' +
        'COMMITTED,COMPACT,COMPLETION,COMPRESS,COMPRESSED,CONCURRENT,CONDITION,' +
        'CONNECTION,CONSISTENT,CONSTRAINT,CONSTRAINT_CATALOG,CONSTRAINT_NAME,' +
        'CONSTRAINT_SCHEMA,CONTAINS,CONTEXT,CONTINUE,CONTRIBUTORS,CONVERT,COPY,' +
        'CPU,CREATE,CROSS,CURRENT,CURRENT_DATE,CURRENT_TIME,CURRENT_TIMESTAMP,' +
        'CURRENT_USER,CURSOR,CURSOR_NAME,DATA,DATABASE,DATABASES,DATAFILE,DAY,' +
        'DAY_HOUR,DAY_MICROSECOND,DAY_MINUTE,DAY_SECOND,DEALLOCATE,DECLARE,' +
        'DEFAULT,DEFINER,DELAY_KEY_WRITE,DELAYED,DELETE,DESC,DESCRIBE,' +
        'DETERMINISTIC,DIAGNOSTICS,DIRECTORY,DISABLE,DISCARD,DISK,DISTINCT,' +
        'DISTINCTROW,DIV,DO,DROP,DUMPFILE,DUPLICATE,DYNAMIC,EACH,ELSE,ELSEIF,' +
        'ENABLE,ENCLOSED,END,ENDS,ENGINE,ENGINES,ERRORS,ESCAPE,ESCAPED,EVENT,' +
        'EVENTS,EVERY,EXCHANGE,EXCLUSIVE,EXECUTE,EXISTS,EXPANSION,EXPIRE,EXPLAIN,' +
        'EXIT,EXTENDED,FALSE,FAST,FAULTS,FETCH,FILE_BLOCK_SIZE,FLUSH,FIELDS,FILE,' +
        'FIRST,FIXED,FOLLOWS,FOR,FORCE,FORMAT,FOREIGN,FOUND,FROM,FULL,FULLTEXT,' +
        'FUNCTION,GENERATED,GET,GLOBAL,GRANT,GRANTS,GROUP,HANDLER,HASH,HAVING,' +
        'HELP,HIGH_PRIORITY,HOST,HOSTS,HOUR,HOUR_MICROSECOND,HOUR_MINUTE,' +
        'HOUR_SECOND,IDENTIFIED,IF,IGNORE,IGNORE_SERVER_IDS,IMPORT,IN,INDEX,' +
        'INDEXES,INFILE,INITIAL_SIZE,INNER,INNODB,INOUT,INPLACE,INSTANCE,INSERT,' +
        'INSERT_METHOD,INTERVAL,INTO,INVOKER,IO,IPC,IS,ISOLATION,ITERATE,JOIN,' +
        'JSON,KEY,KEY_BLOCK_SIZE,KEYS,KILL,LANGUAGE,LAST,LEADING,LEAVE,LEFT,LESS,' +
        'LEVEL,LIKE,LIMIT,LINEAR,LINES,LIST,LOAD,LOCAL,LOCALTIME,LOCALTIMESTAMP,' +
        'LOCK,LOGS,LOOP,LOW_PRIORITY,MASTER,MASTER_AUTO_POSITION,MASTER_BIND,' +
        'MASTER_CONNECT_RETRY,MASTER_DELAY,MASTER_HEARTBEAT_PERIOD,MASTER_HOST,' +
        'MASTER_LOG_FILE,MASTER_LOG_POS,MASTER_PASSWORD,MASTER_PORT,' +
        'MASTER_RETRY_COUNT,MASTER_SSL,MASTER_SSL_CA,MASTER_SSL_CAPATH,' +
        'MASTER_SSL_CERT,MASTER_SSL_CIPHER,MASTER_SSL_CRL,MASTER_SSL_CRLPATH,' +
        'MASTER_SSL_KEY,MASTER_SSL_VERIFY_SERVER_CERT,MASTER_TLS_VERSION,' +
        'MASTER_USER,MATCH,MAX_QUERIES_PER_HOUR,MAX_ROWS,' +
        'MAX_CONNECTIONS_PER_HOUR,MAX_STATEMENT_TIME,MAX_UPDATES_PER_HOUR,' +
        'MAX_USER_CONNECTIONS,MAXVALUE,MEDIUM,MEMORY,MERGE,MESSAGE_TEXT,' +
        'MICROSECOND,MIGRATE,MIN_ROWS,MINUTE,MINUTE_MICROSECOND,MINUTE_SECOND,' +
        'MOD,MODE,MODIFIES,MODIFY,MONTH,MUTEX,MYSQL_ERRNO,NAME,NAMES,NATIONAL,' +
        'NATURAL,NEVER,NEXT,NO,NONE,NOT,NULL,NO_WRITE_TO_BINLOG,NUMBER,OFFSET,' +
        'OJ,ON,ONE,ONLY,OPEN,OPTIMIZE,OPTION,OPTIONALLY,OPTIONS,OR,ORDER,OUT,' +
        'OUTER,OUTFILE,OWNER,PACK_KEYS,PAGE,PAGE_CHECKSUM,PARSER,PARTIAL,' +
        'PARTITION,PARTITIONING,PARTITIONS,PASSWORD,PERSISTENT,PHASE,PLUGINS,' +
        'PORT,PRECEDES,PREPARE,PRESERVE,PRIMARY,PRIVILEGES,PROCEDURE,PROCESS,' +
        'PROCESSLIST,PROFILE,PROFILES,PROXY,PURGE,QUARTER,QUERY,QUICK,RANGE,READ,' +
        'READS,REBUILD,RECOVER,REDUNDANT,REFERENCES,REGEXP,RELAYLOG,RELEASE,' +
        'RELAY_LOG_FILE,RELAY_LOG_POS,RELOAD,REMOVE,RENAME,REORGANIZE,REPAIR,' +
        'REPEAT,REPEATABLE,REPLACE,REPLICATION,REQUIRE,RESET,RESIGNAL,RESTRICT,' +
        'RESUME,RETURN,RETURNED_SQLSTATE,RETURNS,REVERSE,REVOKE,RIGHT,RLIKE,' +
        'ROLLBACK,ROLLUP,ROTATE,ROUTINE,ROW,ROW_COUNT,ROW_FORMAT,ROWS,SAVEPOINT,' +
        'SCHEDULE,SCHEMA,SCHEMA_NAME,SECOND,SECOND_MICROSECOND,SECURITY,SELECT,' +
        'SEPARATOR,SERIALIZABLE,SERVER,SESSION,SET,SHARE,SHARED,SHOW,SHUTDOWN,' +
        'SIGNAL,SIGNED,SIMPLE,SLAVE,SNAPSHOT,SOCKET,SONAME,SOUNDS,SOURCE,SPATIAL,' +
        'SQL,SQL_BIG_RESULT,SQL_BUFFER_RESULT,SQL_CACHE,SQL_CALC_FOUND_ROWS,' +
        'SQL_NO_CACHE,SQL_SMALL_RESULT,SQLEXCEPTION,SQLSTATE,SQLWARNINGS,STACKED,' +
        'STARTING,START,STARTS,STATS_AUTO_RECALC,STATS_PERSISTENT,STATUS,STOP,' +
        'STORAGE,STORED,STRAIGHT_JOIN,SUBCLASS_ORIGIN,SUBPARTITION,SUBPARTITIONS,' +
        'SUPER,SUSPEND,SWAPS,SWITCHES,TABLE,TABLE_NAME,TABLES,TABLESPACE,' +
        'TEMPORARY,TEMPTABLE,TERMINATED,THAN,THEN,TO,TRADITIONAL,TRAILING,' +
        'TRANSACTION,TRANSACTIONAL,TRIGGER,TRIGGERS,TRUE,TRUNCATE,TYPE,' +
        'UNCOMMITTED,UNDEFINED,UNDO,UNICODE,UNION,UNIQUE,UNKNOWN,UNLOCK,UNSIGNED,' +
        'UNTIL,UPDATE,UPGRADE,USAGE,USE,USE_FRM,USER,USING,VALIDATION,VALUE,' +
        'VALUES,VARIABLES,VIEW,VIRTUAL,WAIT,WARNINGS,WEEK,WHEN,WHERE,WHILE,' +
        'WRAPPER,WRITE,WITH,WITHOUT,WORK,XA,XID,XML,XOR,YEAR,YEAR_MONTH,ZEROFILL';

    type
      TCharArray = array [0 .. MaxIdentLength] of Char;
      TFileType = (ftSQL, ftFormatedSQL, ftDebugHTML);

      TNodeType = (
        ntRoot,            // Root token, one usage by the parser to handle stmt list
        ntToken,           // Token node (smalles logical part of the SQL text)

        ntAnalyzeTableStmt,
        ntAlterDatabaseStmt,
        ntAlterEventStmt,
        ntAlterInstanceStmt,
        ntAlterRoutineStmt,
        ntAlterServerStmt,
        ntAlterTablespaceStmt,
        ntAlterTableStmt,
        ntAlterTableStmtAlterColumn,
        ntAlterTableStmtConvertTo,
        ntAlterTableStmtDropObject,
        ntAlterTableStmtExchangePartition,
        ntAlterTableStmtOrderBy,
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
        ntChangeMasterStmt,
        ntCharFunc,
        ntCheckTableStmt,
        ntCheckTableStmtOption,
        ntChecksumTableStmt,
        ntCloseStmt,
        ntCommitStmt,
        ntCompoundStmt,
        ntConvertFunc,
        ntCreateDatabaseStmt,
        ntCreateEventStmt,
        ntCreateIndexStmt,
        ntCreateRoutineStmt,
        ntCreateServerStmt,
        ntCreateTablespaceStmt,
        ntCreateTableStmt,
        ntCreateTableStmtField,
        ntCreateTableStmtFieldDefaultFunc,
        ntCreateTableStmtForeignKey,
        ntCreateTableStmtKey,
        ntCreateTableStmtKeyColumn,
        ntCreateTableStmtPartition,
        ntCreateTableStmtReference,
        ntCreateTriggerStmt,
        ntCreateUserStmt,
        ntCreateViewStmt,
        ntCurrentTimestamp,
        ntDatatype,
        ntDateAddFunc,
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
        ntDropTablespaceStmt,
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
        ntInOp,
        ntInsertStmt,
        ntInsertStmtSetItem,
        ntIntervalOp,
        ntIterateStmt,
        ntKillStmt,
        ntLeaveStmt,
        ntLikeOp,
        ntList,
        ntLoadDataStmt,
        ntLoadXMLStmt,
        ntLockTableStmt,
        ntLockTableStmtItem,
        ntLoopStmt,
        ntMatchFunc,
        ntPositionFunc,
        ntPrepareStmt,
        ntPurgeStmt,
        ntOpenStmt,
        ntOptimizeTableStmt,
        ntRegExpOp,
        ntRenameStmt,
        ntRenameStmtPair,
        ntReleaseStmt,
        ntRepairTableStmt,
        ntRepeatStmt,
        ntResetStmt,
        ntResignalStmt,
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
        ntShowColumnsStmt,
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
        ntSubquery,
        ntSubstringFunc,
        ntTag,
        ntTrimFunc,
        ntTruncateStmt,
        ntUnaryOp,
        ntUnknownStmt,
        ntUnlockTablesStmt,
        ntUpdateStmt,
        ntUser,
        ntUseStmt,
        ntValue,
        ntVariableIdent,
        ntWeightStringFunc,
        ntWeightStringFuncLevel,
        ntWhileStmt,
        ntXAStmt,
        ntXAStmtID
      );

      TStmtType = (
        stAnalyzeTable,
        stAlterDatabase,
        stAlterEvent,
        stAlterInstance,
        stAlterRoutine,
        stAlterServer,
        stAlterTable,
        stAlterTablespace,
        stAlterView,
        stBegin,
        stCall,
        stCase,
        stChangeMaster,
        stCheckTable,
        stChecksumTable,
        stClose,
        stCommit,
        stCompound,
        stCreateDatabase,
        stCreateEvent,
        stCreateIndex,
        stCreateRoutine,
        stCreateServer,
        stCreateTablespace,
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
        stDropTablespace,
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
        stLockTable,
        stLoop,
        stPrepare,
        stPurge,
        stOpen,
        stOptimizeTable,
        stRename,
        stRelease,
        stRepairTable,
        stRepeat,
        stReset,
        stResignal,
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
        stShowColumns,
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
        stUnlockTables,
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
        utDatatype,
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
        ttSemicolon,              // ";"
        ttComma,                  // ","
        ttOpenBracket,            // "("
        ttCloseBracket,           // ")"
        ttOpenCurlyBracket,       // "{"
        ttCloseCurlyBracket,      // "}"
        ttInteger,                // Integer constant, like -123456
        ttNumeric,                // Numeric (float) constant, like -123.456E78
        ttString,                 // String constant, enclosed in ''
        ttIdent,                  // Ident
        ttDQIdent,                // Ident, enclosed in ""
        ttMySQLIdent,             // Ident, enclosed in ``
        ttMySQLCodeStart,         // MySQL specific code, like /*!50000 SELECT 1; */
        ttMySQLCodeEnd,
        ttOperator,               // Symbol operator like +, -, &&, *=
        ttAt,                     // "@"
        ttIPAddress               // "123.123.123.123"
      );

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
        ditDatabase,
        ditTable,
        ditProcedure,
        ditFunction,
        ditTrigger,
        ditEvent,
        ditKey,
        ditField,
        ditForeignKey,
        ditPartition,
        ditUser,
        ditParameter,
        ditVariable,
        ditServer,
        ditCursor,
        ditEngine,
        ditCharset,
        ditCollation
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
        rtFunction,
        rtProcedure
      );

    const
      NodeTypeToString: array[TNodeType] of PChar = (
        'ntRoot',
        'ntToken',

        'ntAnalyzeTableStmt',
        'ntAlterDatabaseStmt',
        'ntAlterEventStmt',
        'ntAlterInstanceStmt',
        'ntAlterRoutineStmt',
        'ntAlterServerStmt',
        'ntAlterTablespaceStmt',
        'ntAlterTableStmt',
        'ntAlterTableStmtAlterColumn',
        'ntAlterTableStmtConvertTo',
        'ntAlterTableStmtDropObject',
        'ntAlterTableStmtExchangePartition',
        'ntAlterTableStmtOrderBy',
        'ntAlterTableStmtReorganizePartition',
        'ntAlterViewStmt',
        'ntBeginLabel',
        'ntBeginStmt',
        'ntBetweenOp',
        'ntBinaryOp',
        'ntCallStmt',
        'ntCaseOp',
        'ntCaseOpBranch',
        'ntCaseStmt',
        'ntCaseStmtBranch',
        'ntCastFunc',
        'ntChangeMasterStmt',
        'ntCharFunc',
        'ntCheckTableStmt',
        'ntCheckTableStmtOption',
        'ntChecksumTableStmt',
        'ntCloseStmt',
        'ntCommitStmt',
        'ntCompoundStmt',
        'ntConvertFunc',
        'ntCreateDatabaseStmt',
        'ntCreateEventStmt',
        'ntCreateIndexStmt',
        'ntCreateRoutineStmt',
        'ntCreateServerStmt',
        'ntCreateTablespaceStmt',
        'ntCreateTableStmt',
        'ntCreateTableStmtField',
        'ntCreateTableStmtFieldDefaultFunc',
        'ntCreateTableStmtForeignKey',
        'ntCreateTableStmtKey',
        'ntCreateTableStmtKeyColumn',
        'ntCreateTableStmtPartition',
        'ntCreateTableStmtReference',
        'ntCreateTriggerStmt',
        'ntCreateUserStmt',
        'ntCreateViewStmt',
        'ntCurrentTimestamp',
        'ntDatatype',
        'ntDateAddFunc',
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
        'ntDropTablespaceStmt',
        'ntDropTableStmt',
        'ntDropTriggerStmt',
        'ntDropUserStmt',
        'ntDropViewStmt',
        'ntEndLabel',
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
        'ntGetDiagnosticsStmtCondInfo',
        'ntGrantStmt',
        'ntGrantStmtPrivileg',
        'ntGrantStmtUserSpecification',
        'ntGroupConcatFunc',
        'ntGroupConcatFuncExpr',
        'ntHelpStmt',
        'ntIfStmt',
        'ntIfStmtBranch',
        'ntInOp',
        'ntInsertStmt',
        'ntInsertStmtSetItem',
        'ntIntervalOp',
        'ntIterateStmt',
        'ntKillStmt',
        'ntLeaveStmt',
        'ntLikeOp',
        'ntList',
        'ntLoadDataStmt',
        'ntLoadXMLStmt',
        'ntLockTableStmt',
        'ntLockTableStmtItem',
        'ntLoopStmt',
        'ntMatchFunc',
        'ntPositionFunc',
        'ntPrepareStmt',
        'ntPurgeStmt',
        'ntOpenStmt',
        'ntOptimizeTableStmt',
        'ntRegExpOp',
        'ntRenameStmt',
        'ntRenameStmtPair',
        'ntReleaseStmt',
        'ntRepairTableStmt',
        'ntRepeatStmt',
        'ntResetStmt',
        'ntResignalStmt',
        'ntReturnStmt',
        'ntRevokeStmt',
        'ntRollbackStmt',
        'ntRoutineParam',
        'ntSavepointStmt',
        'ntSchedule',
        'ntSecretIdent',
        'ntSelectStmt',
        'ntSelectStmtColumn',
        'ntSelectStmtGroup',
        'ntSelectStmtOrder',
        'ntSelectStmtTableFactor',
        'ntSelectStmtTableFactorIndexHint',
        'ntSelectStmtTableFactorOj',
        'ntSelectStmtTableFactorSelect',
        'ntSelectStmtTableJoin',
        'ntSetNamesStmt',
        'ntSetPasswordStmt',
        'ntSetStmt',
        'ntSetStmtAssignment',
        'ntSetTransactionStmt',
        'ntSetTransactionStmtCharacteristic',
        'ntShowAuthorsStmt',
        'ntShowBinaryLogsStmt',
        'ntShowBinlogEventsStmt',
        'ntShowCharacterSetStmt',
        'ntShowCollationStmt',
        'ntShowColumnsStmt',
        'ntShowContributorsStmt',
        'ntShowCountErrorsStmt',
        'ntShowCountWarningsStmt',
        'ntShowCreateDatabaseStmt',
        'ntShowCreateEventStmt',
        'ntShowCreateFunctionStmt',
        'ntShowCreateProcedureStmt',
        'ntShowCreateTableStmt',
        'ntShowCreateTriggerStmt',
        'ntShowCreateUserStmt',
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
        'ntSubAreaSelect',
        'ntSubPartition',
        'ntSubquery',
        'ntSubstringFunc',
        'ntTag',
        'ntTrimFunc',
        'ntTruncateStmt',
        'ntUnaryOp',
        'ntUnknownStmt',
        'ntUnlockTablesStmt',
        'ntUpdateStmt',
        'ntUser',
        'ntUseStmt',
        'ntValue',
        'ntVariableIdent',
        'ntWeightStringFunc',
        'ntWeightStringFuncLevel',
        'ntWhileStmt',
        'ntXAStmt',
        'ntXAStmtID'
      );

      StmtTypeToString: array[TStmtType] of PChar = (
        'stAnalyzeTable',
        'stAlterDatabase',
        'stAlterEvent',
        'stAlterInstance',
        'stAlterRoutine',
        'stAlterServer',
        'stAlterTable',
        'stAlterTablespace',
        'stAlterView',
        'stBegin',
        'stCall',
        'stCase',
        'stChangeMaster',
        'stCheckTable',
        'stChecksumTable',
        'stClose',
        'stCommit',
        'stCompound',
        'stCreateDatabase',
        'stCreateEvent',
        'stCreateIndex',
        'stCreateRoutine',
        'stCreateServer',
        'stCreateTablespace',
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
        'stDropTablespace',
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
        'stLockTable',
        'stLoop',
        'stPrepare',
        'stPurge',
        'stOpen',
        'stOptimizeTable',
        'stRename',
        'stRelease',
        'stRepairTable',
        'stRepeat',
        'stReset',
        'stResignal',
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
        'stShowColumns',
        'stShowContributors',
        'stShowCountErrors',
        'stShowCountWarnings',
        'stShowCreateDatabase',
        'stShowCreateEvent',
        'stShowCreateFunction',
        'stShowCreateProcedure',
        'stShowCreateTable',
        'stShowCreateTrigger',
        'stShowCreateUser',
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
        'stSubAreaSelect',
        'stTruncate',
        'stUnknown',
        'stUnlockTables',
        'stUpdate',
        'stUse',
        'stWhile',
        'stXA'
      );

      TokenTypeToString: array[TTokenType] of PChar = (
        'ttUnknown',
        'ttError',
        'ttSpace',
        'ttReturn',
        'ttLineComment',
        'ttMultiLineComment',
        'ttDot',
        'ttColon',
        'ttSemicolon',
        'ttComma',
        'ttOpenBracket',
        'ttCloseBracket',
        'ttOpenCurlyBracket',
        'ttCloseCurlyBracket',
        'ttInteger',
        'ttNumeric',
        'ttString',
        'ttIdent',
        'ttDQIdent',
        'ttMySQLIdent',
        'ttMySQLCodeStart',
        'ttMySQLCodeEnd',
        'ttOperator',
        'ttAt',
        'ttIPAddress'
      );

      UsageTypeToString: array[TUsageType] of PChar = (
        'utUnknown',
        'utError',
        'utWhiteSpace',
        'utComment',
        'utSymbol',
        'utKeyword',
        'utLabel',
        'utOperator',
        'utDatatype',
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

        'otAssign',
        'otAssign2',

        'otEscape',

        'otHat',
        'otDoubleDot',
        'otArrow',
        'otParameter'
      );

      DbIdentTypeToString: array[TDbIdentType] of PChar = (
        'ditUnknown',
        'ditDatabase',
        'ditTable',
        'ditProcedure',
        'ditFunction',
        'ditTrigger',
        'ditEvent',
        'ditKey',
        'ditField',
        'ditForeignKey',
        'ditPartition',
        'ditUser',
        'ditParameter',
        'ditVariable',
        'ditServer',
        'ditCursor',
        'ditEngine',
        'ditCharset',
        'ditCollation'
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

      StmtNodeTypes = [
        ntAnalyzeTableStmt,
        ntAlterDatabaseStmt,
        ntAlterEventStmt,
        ntAlterInstanceStmt,
        ntAlterRoutineStmt,
        ntAlterServerStmt,
        ntAlterTablespaceStmt,
        ntAlterTableStmt,
        ntAlterViewStmt,
        ntBeginStmt,
        ntCallStmt,
        ntCaseStmt,
        ntChangeMasterStmt,
        ntCheckTableStmt,
        ntChecksumTableStmt,
        ntCloseStmt,
        ntCommitStmt,
        ntCompoundStmt,
        ntCreateDatabaseStmt,
        ntCreateEventStmt,
        ntCreateIndexStmt,
        ntCreateRoutineStmt,
        ntCreateServerStmt,
        ntCreateTablespaceStmt,
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
        ntDropTablespaceStmt,
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
        ntLockTableStmt,
        ntLoopStmt,
        ntPrepareStmt,
        ntPurgeStmt,
        ntOpenStmt,
        ntOptimizeTableStmt,
        ntRenameStmt,
        ntReleaseStmt,
        ntRepairTableStmt,
        ntRepeatStmt,
        ntResetStmt,
        ntResignalStmt,
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
        ntShowColumnsStmt,
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
        ntStartSlaveStmt,
        ntStartTransactionStmt,
        ntStopSlaveStmt,
        ntSubAreaSelectStmt,
        ntTruncateStmt,
        ntUnknownStmt,
        ntUnlockTablesStmt,
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
        'rtFunction',
        'rtProcedure'
      );

      NodeTypeByStmtType: array[TStmtType] of TNodeType = (
        ntAnalyzeTableStmt,
        ntAlterDatabaseStmt,
        ntAlterEventStmt,
        ntAlterInstanceStmt,
        ntAlterRoutineStmt,
        ntAlterServerStmt,
        ntAlterTableStmt,
        ntAlterTablespaceStmt,
        ntAlterViewStmt,
        ntBeginStmt,
        ntCallStmt,
        ntCaseStmt,
        ntChangeMasterStmt,
        ntCheckTableStmt,
        ntChecksumTableStmt,
        ntCloseStmt,
        ntCommitStmt,
        ntCompoundStmt,
        ntCreateDatabaseStmt,
        ntCreateEventStmt,
        ntCreateIndexStmt,
        ntCreateRoutineStmt,
        ntCreateServerStmt,
        ntCreateTablespaceStmt,
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
        ntDropTablespaceStmt,
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
        ntLockTableStmt,
        ntLoopStmt,
        ntPrepareStmt,
        ntPurgeStmt,
        ntOpenStmt,
        ntOptimizeTableStmt,
        ntRenameStmt,
        ntReleaseStmt,
        ntRepairTableStmt,
        ntRepeatStmt,
        ntResetStmt,
        ntResignalStmt,
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
        ntShowColumnsStmt,
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
        ntStartSlaveStmt,
        ntStartTransactionStmt,
        ntStopSlaveStmt,
        ntSubAreaSelectStmt,
        ntTruncateStmt,
        ntUnknownStmt,
        ntUnlockTablesStmt,
        ntUpdateStmt,
        ntUseStmt,
        ntWhileStmt,
        ntXAStmt
      );

  protected
    type
      TOffset = Integer;

  private
    type
      TCreateTableIndexAdd = (iaAdd, iaCreate, iaNone);
      TIntegerArray = array of Integer;
      POffsetArray = ^TOffsetArray;
      TOffsetArray = array [0..$FFFF] of TOffset;
      TParseFunction = function(): TOffset of object;
      TTableOptionNodes = packed record
        AutoIncrementValue: TOffset;
        AvgRowLengthValue: TOffset;
        CharacterSetValue: TOffset;
        ChecksumValue: TOffset;
        CollateValue: TOffset;
        CommentValue: TOffset;
        CompressValue: TOffset;
        ConnectionValue: TOffset;
        DataDirectoryValue: TOffset;
        DelayKeyWriteValue: TOffset;
        EngineValue: TOffset;
        IndexDirectoryValue: TOffset;
        InsertMethodValue: TOffset;
        KeyBlockSizeValue: TOffset;
        MaxRowsValue: TOffset;
        MinRowsValue: TOffset;
        PackKeysValue: TOffset;
        PageChecksumValue: TOffset;
        PasswordValue: TOffset;
        RowFormatValue: TOffset;
        StatsAutoRecalcValue: TOffset;
        StatsPersistentValue: TOffset;
        TransactionalValue: TOffset;
        UnionList: TOffset;
      end;
      TValueAssign = (vaYes, vaNo, vaAuto);
      TSeparatorType = (stNone, stReturnBefore, stSpaceBefore, stSpaceAfter, stReturnAfter);

      TStringBuffer = class
      private
        Buffer: record
          Mem: PChar;
          MemSize: Integer;
          Write: PChar;
        end;
        function GetData(): Pointer; inline;
        function GetLength(): Integer; inline;
        function GetSize(): Integer; inline;
        function GetText(): PChar; inline;
        procedure Reallocate(const NeededLength: Integer);
      public
        procedure Clear();
        constructor Create(const InitialLength: Integer);
        procedure Delete(const Start: Integer; const Length: Integer);
        destructor Destroy(); override;
        function Read(): string; inline;
        procedure Write(const Text: PChar; const Length: Integer); overload; virtual;
        procedure Write(const Text: string); overload; {$IFNDEF Debug} inline; {$ENDIF}
        procedure Write(const Char: Char); overload; {$IFNDEF Debug} inline; {$ENDIF}
        property Data: Pointer read GetData;
        property Length: Integer read GetLength;
        property Size: Integer read GetSize;
        property Text: PChar read GetText;
      end;

      TWordList = class
      private type
        TIndex = SmallInt;
        TIndices = array [0..6] of TIndex;
      private
        FCount: TIndex;
        FIndex: array of PChar;
        FFirst: array of Integer;
        FParser: TSQLParser;
        FText: string;
        function GetText(): string;
        function GetWord(Index: TIndex): string;
        procedure SetText(AText: string);
      protected
        procedure Clear();
        procedure GetWordText(const Index: TIndex; out Text: PChar; out Length: Integer);
        property Parser: TSQLParser read FParser;
      public
        constructor Create(const ASQLParser: TSQLParser; const AText: string = '');
        destructor Destroy(); override;
        function IndexOf(const Word: PChar; const Length: Integer): Integer; overload;
        function IndexOf(const Word: string): Integer; overload; {$IFNDEF Debug} inline; {$ENDIF}
        property Count: TIndex read FCount;
        property Text: string read GetText write SetText;
        property Word[Index: TIndex]: string read GetWord; default;
      end;

      TFormatBuffer = class(TStringBuffer)
      private
        FNewLine: Boolean;
        Indent: Integer;
        IndentSpaces: array[0 .. 1024 - 1] of Char;
      public
        constructor Create();
        procedure DecreaseIndent();
        destructor Destroy(); override;
        procedure IncreaseIndent();
        procedure Write(const Text: PChar; const Length: Integer); override;
        procedure WriteReturn(); {$IFNDEF Debug} inline; {$ENDIF}
        procedure WriteSpace(); {$IFNDEF Debug} inline; {$ENDIF}
        property NewLine: Boolean read FNewLine;
      end;

    const
      IndentSize = 2;

  public
    type
      TCompletionList = class(Classes.TList)
      public type
        PItem = ^TItem;
        TItem = record
          case ItemType: (itTag, itList) of
            itTag: (
              Tag: TCharArray;
            );
            itList: (
              DatabaseName: TCharArray;
              DbIdentType: TDbIdentType;
              TableName: TCharArray;
            );
        end;
      private
        FParser: TSQLParser;
        function GetItem(Index: Integer): PItem;
      protected
        procedure AddKeyword(const KeywordIndex: TWordList.TIndex;
          const KeywordIndex2: TWordList.TIndex = -1; const KeywordIndex3: TWordList.TIndex = -1;
          const KeywordIndex4: TWordList.TIndex = -1; const KeywordIndex5: TWordList.TIndex = -1;
          const KeywordIndex6: TWordList.TIndex = -1; const KeywordIndex7: TWordList.TIndex = -1);
        procedure AddList(DbIdentType: TDbIdentType;
          const DatabaseName: string = ''; TableName: string = '');
      public
        procedure Clear(); override;
        constructor Create(const AParser: TSQLParser);
        destructor Destroy(); override;
        property Item[Index: Integer]: PItem read GetItem; default;
        property Parser: TSQLParser read FParser;
      end;

      PNode = ^TNode;
      PToken = ^TToken;
      PStmt = ^TStmt;

      { Base nodes ------------------------------------------------------------}

      TNode = packed record
      private
        FNodeType: TNodeType;
        FParser: TSQLParser;
      private
        class function Create(const AParser: TSQLParser; const ANodeType: TNodeType): TOffset; static; {$IFNDEF Debug} inline; {$ENDIF}
        function GetOffset(): TOffset; {$IFNDEF Debug} inline; {$ENDIF}
        property Offset: TOffset read GetOffset;
      public
        property NodeType: TNodeType read FNodeType;
        property Parser: TSQLParser read FParser;
      end;

      PRoot = ^TRoot;
      TRoot = packed record
      private
        Heritage: TNode;
      private
        FErrorCode: Byte;
        FErrorMessage: TOffset;
        FFirstStmt: TOffset; // Cache for speeding
        FFirstTokenAll: TOffset;
        FLastStmt: TOffset; // Cache for speeding
        FLastTokenAll: TOffset;
        class function Create(const AParser: TSQLParser;
          const AErrorCode: Byte; const AErrorMessage: TOffset;
          const AFirstTokenAll, ALastTokenAll: TOffset;
          const ChildCount: Integer; const Children: array of TOffset): TOffset; static;
        function GetErrorMessage(): string;
        function GetFirstStmt(): PStmt; {$IFNDEF Debug} inline; {$ENDIF}
        function GetFirstTokenAll(): PToken; {$IFNDEF Debug} inline; {$ENDIF}
        function GetLastStmt(): PStmt; {$IFNDEF Debug} inline; {$ENDIF}
        function GetLastTokenAll(): PToken; {$IFNDEF Debug} inline; {$ENDIF}
        property Parser: TSQLParser read Heritage.FParser;
      public
        property ErrorCode: Byte read FErrorCode;
        property ErrorMessage: string read GetErrorMessage;
        property FirstStmt: PStmt read GetFirstStmt;
        property FirstTokenAll: PToken read GetFirstTokenAll;
        property LastStmt: PStmt read GetLastStmt;
        property LastTokenAll: PToken read GetLastTokenAll;
        property NodeType: TNodeType read Heritage.FNodeType;
      end;

      PChild = ^TChild;
      TChild = packed record  // Every node, except TRoot
      private
        Heritage: TNode;
      private
        FParentNode: TOffset;
        class function Create(const AParser: TSQLParser; const ANodeType: TNodeType): TOffset; static; {$IFNDEF Debug} inline; {$ENDIF}
        function GetDelimiter(): PToken; {$IFNDEF Debug} inline; {$ENDIF}
        function GetFFirstToken(): TOffset; {$IFNDEF Debug} inline; {$ENDIF}
        function GetFirstToken(): PToken; {$IFNDEF Debug} inline; {$ENDIF}
        function GetFLastToken(): TOffset; {$IFNDEF Debug} inline; {$ENDIF}
        function GetLastToken(): PToken; {$IFNDEF Debug} inline; {$ENDIF}
        function GetNextSibling(): PChild; {$IFNDEF Debug} inline; {$ENDIF}
        function GetParentNode(): PNode; {$IFNDEF Debug} inline; {$ENDIF}
        property FFirstToken: TOffset read GetFFirstToken;
        property FLastToken: TOffset read GetFLastToken;
        property Parser: TSQLParser read Heritage.FParser;
      public
        property Delimiter: PToken read GetDelimiter;
        property FirstToken: PToken read GetFirstToken;
        property LastToken: PToken read GetLastToken;
        property NextSibling: PChild read GetNextSibling;
        property NodeType: TNodeType read Heritage.FNodeType;
        property ParentNode: PNode read GetParentNode;
      end;

      TToken = packed record
      private
        Heritage: TChild;
      private
        FErrorCode: Byte;
        FErrorPos: PChar;
        {$IFDEF Debug}
        FIndex: Integer;
        {$ENDIF}
        FKeywordIndex: TWordList.TIndex;
        FLength: Integer;
        FOperatorType: TOperatorType;
        FText: PChar;
        FTokenType: TTokenType;
        FUsageType: TUsageType;
        NewText: TOffset;
        NewTokenType: TTokenType;
        class function Create(const AParser: TSQLParser;
          const AText: PChar; const ALength: Integer;
          const AErrorCode: Byte; const AErrorPos: PChar;
          const ATokenType: TTokenType; const AOperatorType: TOperatorType;
          const AKeywordIndex: TWordList.TIndex; const AUsageType: TUsageType): TOffset; static; {$IFNDEF Debug} inline; {$ENDIF}
        function GetAsString(): string;
        function GetDbIdentType(): TDbIdentType;
        function GetGeneration(): Integer;
        {$IFNDEF Debug}
        function GetIndex(): Integer;
        {$ENDIF}
        function GetIsUsed(): Boolean; {$IFNDEF Debug} inline; {$ENDIF}
        function GetNextToken(): PToken;
        function GetNextTokenAll(): PToken;
        function GetOffset(): TOffset; {$IFNDEF Debug} inline; {$ENDIF}
        function GetParentNode(): PNode; {$IFNDEF Debug} inline; {$ENDIF}
        function GetTokenType(): TTokenType;
        procedure SetTokenType(ATokenType: TTokenType);
        property ErrorCode: Byte read FErrorCode;
        property ErrorPos: PChar read FErrorPos;
        property Generation: Integer read GetGeneration;
        {$IFDEF Debug}
        property Index: Integer read FIndex;
        {$ELSE}
        property Index: Integer read GetIndex; // VERY slow. Should be used for internal debugging only.
        {$ENDIF}
        property IsUsed: Boolean read GetIsUsed;
        property KeywordIndex: TWordList.TIndex read FKeywordIndex;
        property Length: Integer read FLength;
        property Offset: TOffset read GetOffset;
        property Parser: TSQLParser read Heritage.Heritage.FParser;
      public
        function GetText(): string; overload;
        procedure GetText(out Text: PChar; out Length: Integer); overload;
        procedure SetText(Text: string); overload; {$IFNDEF Debug} inline; {$ENDIF}
        procedure SetText(const Text: PChar; const Length: Integer); overload; {$IFNDEF Debug} inline; {$ENDIF}
        property AsString: string read GetAsString;
        property DbIdentType: TDbIdentType read GetDbIdentType;
        property NextToken: PToken read GetNextToken;
        property NextTokenAll: PToken read GetNextTokenAll;
        property OperatorType: TOperatorType read FOperatorType;
        property ParentNode: PNode read GetParentNode;
        property Text: string read GetText write SetText;
        property TokenType: TTokenType read GetTokenType write SetTokenType;
        property UsageType: TUsageType read FUsageType;
      end;

      PRange = ^TRange;
      TRange = packed record
      private
        Heritage: TChild;
        property FParentNode: TOffset read Heritage.FParentNode write Heritage.FParentNode;
      private
        FFirstToken: TOffset; // Cache for speeding
        FLastToken: TOffset; // Cache for speeding
        class function Create(const AParser: TSQLParser; const ANodeType: TNodeType): TOffset; static; {$IFNDEF Debug} inline; {$ENDIF}
        function GetFirstToken(): PToken; {$IFNDEF Debug} inline; {$ENDIF}
        function GetLastToken(): PToken; {$IFNDEF Debug} inline; {$ENDIF}
        function GetOffset(): TOffset; {$IFNDEF Debug} inline; {$ENDIF}
        function GetParentNode(): PNode; {$IFNDEF Debug} inline; {$ENDIF}
        procedure AddChildren(const Children: POffsetArray; const Count: Integer);
        property Offset: TOffset read GetOffset;
        property Parser: TSQLParser read Heritage.Heritage.FParser;
      public
        property FirstToken: PToken read GetFirstToken;
        property LastToken: PToken read GetLastToken;
        property NodeType: TNodeType read Heritage.Heritage.FNodeType;
        property ParentNode: PNode read GetParentNode;
      end;

      TStmt = packed record
      private
        Heritage: TRange;
        property FFirstToken: TOffset read Heritage.FFirstToken write Heritage.FFirstToken;
        property FLastToken: TOffset read Heritage.FLastToken write Heritage.FLastToken;
      private
        FErrorCode: Byte;
        FErrorMessage: TOffset;
        FErrorToken: TOffset;
        FFirstTokenAll: TOffset;
        FLastTokenAll: TOffset;
        FStmtType: TStmtType; // Cache for speeding
        class function Create(const AParser: TSQLParser; const AStmtType: TStmtType): TOffset; static;
        function GetDelimiter(): PToken; {$IFNDEF Debug} inline; {$ENDIF}
        function GetErrorMessage(): string;
        function GetErrorToken(): PToken; {$IFNDEF Debug} inline; {$ENDIF}
        function GetFirstToken(): PToken; {$IFNDEF Debug} inline; {$ENDIF}
        function GetFirstTokenAll(): PToken; {$IFNDEF Debug} inline; {$ENDIF}
        function GetLastToken(): PToken; {$IFNDEF Debug} inline; {$ENDIF}
        function GetLastTokenAll(): PToken; {$IFNDEF Debug} inline; {$ENDIF}
        function GetNextStmt(): PStmt;
        function GetParentNode(): PNode; {$IFNDEF Debug} inline; {$ENDIF}
        function GetText(): string;
        property Parser: TSQLParser read Heritage.Heritage.Heritage.FParser;
      public
        property Delimiter: PToken read GetDelimiter;
        property ErrorCode: Byte read FErrorCode;
        property ErrorMessage: string read GetErrorMessage;
        property ErrorToken: PToken read GetErrorToken;
        property FirstToken: PToken read GetFirstToken;
        property FirstTokenAll: PToken read GetFirstTokenAll;
        property LastToken: PToken read GetLastToken;
        property LastTokenAll: PToken read GetLastTokenAll;
        property NextStmt: PStmt read GetNextStmt;
        property ParentNode: PNode read GetParentNode;
        property StmtType: TStmtType read FStmtType;
        property Text: string read GetText;
      end;

      { Normal nodes ----------------------------------------------------------}

    protected type
      PAnalyzeTableStmt = ^TAnalyzeTableStmt;
      TAnalyzeTableStmt = packed record
      private type
        TNodes = packed record
          StmtTag: TOffset;
          NoWriteToBinlogTag: TOffset;
          TableTag: TOffset;
          TablesList: TOffset;
        end;
      private
        Heritage: TStmt;
      private
        Nodes: TNodes;
        class function Create(const AParser: TSQLParser; const ANodes: TNodes): TOffset; static;
      public
        property Parser: TSQLParser read Heritage.Heritage.Heritage.Heritage.FParser;
      end;

      PAlterDatabaseStmt = ^TAlterDatabaseStmt;
      TAlterDatabaseStmt = packed record
      private type
        TNodes = packed record
          StmtTag: TOffset;
          Ident: TOffset;
          CharsetValue: TOffset;
          CollateValue: TOffset;
          UpgradeDataDirectoryNameTag: TOffset;
        end;
      private
        Heritage: TStmt;
      private
        Nodes: TNodes;
        class function Create(const AParser: TSQLParser; const ANodes: TNodes): TOffset; static;
      public
        property Parser: TSQLParser read Heritage.Heritage.Heritage.Heritage.FParser;
      end;

      PAlterEventStmt = ^TAlterEventStmt;
      TAlterEventStmt = packed record
      private type
        TNodes = packed record
          AlterTag: TOffset;
          DefinerValue: TOffset;
          EventTag: TOffset;
          EventIdent: TOffset;
          OnSchedule: packed record
            Tag: TOffset;
            Value: TOffset;
          end;
          OnCompletionTag: TOffset;
          RenameValue: TOffset;
          EnableTag: TOffset;
          CommentValue: TOffset;
          DoTag: TOffset;
          Body: TOffset;
        end;
      private
        Heritage: TStmt;
      private
        Nodes: TNodes;
        class function Create(const AParser: TSQLParser; const ANodes: TNodes): TOffset; static;
      public
        property Parser: TSQLParser read Heritage.Heritage.Heritage.Heritage.FParser;
      end;

      PAlterInstanceStmt = ^TAlterInstanceStmt;
      TAlterInstanceStmt = packed record
      private type
        TNodes = packed record
          StmtTag: TOffset;
          RotateTag: TOffset;
        end;
      private
        Heritage: TStmt;
      private
        Nodes: TNodes;
        class function Create(const AParser: TSQLParser; const ANodes: TNodes): TOffset; static;
      public
        property Parser: TSQLParser read Heritage.Heritage.Heritage.Heritage.FParser;
      end;

      PAlterRoutineStmt = ^TAlterRoutineStmt;
      TAlterRoutineStmt = packed record
      private type
        TNodes = packed record
          AlterTag: TOffset;
          Ident: TOffset;
          CommentValue: TOffset;
          LanguageTag: TOffset;
          DeterministicTag: TOffset;
          NatureOfDataTag: TOffset;
          SQLSecurityTag: TOffset;
        end;
      private
        Heritage: TStmt;
      private
        Nodes: TNodes;
        FRoutineType: TRoutineType;
        class function Create(const AParser: TSQLParser; const ARoutineType: TRoutineType; const ANodes: TNodes): TOffset; static;
      public
        property Parser: TSQLParser read Heritage.Heritage.Heritage.Heritage.FParser;
        property RoutineType: TRoutineType read FRoutineType;
      end;

      PAlterServerStmt = ^TAlterServerStmt;
      TAlterServerStmt = packed record
      private type
        TNodes = packed record
          StmtTag: TOffset;
          Ident: TOffset;
          Options: packed record
            Tag: TOffset;
            List: TOffset;
          end;
        end;
      private
        Heritage: TStmt;
      private
        Nodes: TNodes;
        class function Create(const AParser: TSQLParser; const ANodes: TNodes): TOffset; static;
      public
        property Parser: TSQLParser read Heritage.Heritage.Heritage.Heritage.FParser;
      end;

      PAlterTablespaceStmt = ^TAlterTablespaceStmt;
      TAlterTablespaceStmt = packed record
      private type
        TNodes = packed record
          StmtTag: TOffset;
          Ident: TOffset;
          AddDatafileValue: TOffset;
          InitialSizeValue: TOffset;
          EngineValue: TOffset;
        end;
      private
        Heritage: TStmt;
      private
        Nodes: TNodes;
        class function Create(const AParser: TSQLParser; const ANodes: TNodes): TOffset; static;
      public
        property Parser: TSQLParser read Heritage.Heritage.Heritage.Heritage.FParser;
      end;

      PAlterTableStmt = ^TAlterTableStmt;
      TAlterTableStmt = packed record
      private type

        PAlterColumn = ^TAlterColumn;
        TAlterColumn = packed record
        private type
          TNodes = packed record
            AlterTag: TOffset;
            Ident: TOffset;
            SetDefaultValue: TOffset;
            DropDefaultTag: TOffset;
          end;
        private
          Heritage: TRange;
        private
          Nodes: TNodes;
          class function Create(const AParser: TSQLParser; const ANodes: TNodes): TOffset; static;
        public
          property Parser: TSQLParser read Heritage.Heritage.Heritage.FParser;
        end;

        PConvertTo = ^TConvertTo;
        TConvertTo = packed record
        private type
          TNodes = packed record
            ConvertToTag: TOffset;
            CharsetValue: TOffset;
            CollateValue: TOffset;
          end;
        private
          Heritage: TRange;
        private
          Nodes: TNodes;
          class function Create(const AParser: TSQLParser; const ANodes: TNodes): TOffset; static;
        public
          property Parser: TSQLParser read Heritage.Heritage.Heritage.FParser;
        end;

        PDropObject = ^TDropObject;
        TDropObject = packed record
        private type
          TNodes = packed record
            DropTag: TOffset;
            ItemTypeTag: TOffset;
            Ident: TOffset;
          end;
        private
          Heritage: TRange;
        private
          Nodes: TNodes;
          class function Create(const AParser: TSQLParser; const ANodes: TNodes): TOffset; static;
        public
          property Parser: TSQLParser read Heritage.Heritage.Heritage.FParser;
        end;

        PExchangePartition = ^TExchangePartition;
        TExchangePartition = packed record
        private type
          TNodes = packed record
            ExchangePartitionTag: TOffset;
            PartitionIdent: TOffset;
            WithTableTag: TOffset;
            TableIdent: TOffset;
            WithValidationTag: TOffset;
          end;
        private
          Heritage: TRange;
        private
          Nodes: TNodes;
          class function Create(const AParser: TSQLParser; const ANodes: TNodes): TOffset; static;
        public
          property Parser: TSQLParser read Heritage.Heritage.Heritage.FParser;
        end;

        POrderBy = ^TOrderBy;
        TOrderBy = packed record
        private type
          TNodes = packed record
            OrderByTag: TOffset;
            KeyColumnList: TOffset;
          end;
        private
          Heritage: TRange;
        private
          Nodes: TNodes;
          class function Create(const AParser: TSQLParser; const ANodes: TNodes): TOffset; static;
        public
          property Parser: TSQLParser read Heritage.Heritage.Heritage.FParser;
        end;

        PReorganizePartition = ^TReorganizePartition;
        TReorganizePartition = packed record
        private type
          TNodes = packed record
            ReorganizePartitionTag: TOffset;
            IdentList: TOffset;
            IntoTag: TOffset;
            PartitionList: TOffset;
          end;
        private
          Heritage: TRange;
        private
          Nodes: TNodes;
          class function Create(const AParser: TSQLParser; const ANodes: TNodes): TOffset; static;
        public
          property Parser: TSQLParser read Heritage.Heritage.Heritage.FParser;
        end;

        TNodes = packed record
          AlterTag: TOffset;
          IgnoreTag: TOffset;
          TableTag: TOffset;
          Ident: TOffset;
          SpecificationList: TOffset;
          AlgorithmValue: TOffset;
          ConvertToCharacterSetNode: TOffset;
          DiscardTablespaceTag: TOffset;
          EnableKeys: TOffset;
          ForceTag: TOffset;
          ImportTablespaceTag: TOffset;
          LockValue: TOffset;
          OrderByValue: TOffset;
          RenameNode: TOffset;
        end;
      private
        Heritage: TStmt;
      private
        Nodes: TNodes;
        class function Create(const AParser: TSQLParser; const ANodes: TNodes): TOffset; static;
      public
        property Parser: TSQLParser read Heritage.Heritage.Heritage.Heritage.FParser;
      end;

      PAlterViewStmt = ^TAlterViewStmt;
      TAlterViewStmt = packed record
      private type
        TNodes = packed record
          AlterTag: TOffset;
          AlgorithmValue: TOffset;
          DefinerValue: TOffset;
          SQLSecurityTag: TOffset;
          ViewTag: TOffset;
          Ident: TOffset;
          Columns: TOffset;
          AsTag: TOffset;
          SelectStmt: TOffset;
          OptionTag: TOffset;
        end;
      private
        Heritage: TStmt;
      private
        Nodes: TNodes;
        class function Create(const AParser: TSQLParser; const ANodes: TNodes): TOffset; static;
      public
        property Parser: TSQLParser read Heritage.Heritage.Heritage.Heritage.FParser;
      end;

      PBeginLabel = ^TBeginLabel;
      TBeginLabel = packed record
      private type
        TNodes = packed record
          BeginToken: TOffset;
          ColonToken: TOffset;
        end;
      private
        Heritage: TRange;
      private
        Nodes: TNodes;
        class function Create(const AParser: TSQLParser; const ANodes: TNodes): TOffset; static;
        function GetLabelName(): string;
      public
        property LabelName: string read GetLabelName;
        property Parser: TSQLParser read Heritage.Heritage.Heritage.FParser;
      end;

      PBeginStmt = ^TBeginStmt;
      TBeginStmt = packed record
      private type
        TNodes = packed record
          BeginTag: TOffset;
        end;
      private
        Heritage: TStmt;
      private
        Nodes: TNodes;
        class function Create(const AParser: TSQLParser; const ANodes: TNodes): TOffset; static;
      public
        property Parser: TSQLParser read Heritage.Heritage.Heritage.Heritage.FParser;
      end;

      PBetweenOp = ^TBetweenOp;
      TBetweenOp = packed record
      private type
        TNodes = packed record
          Expr: TOffset;
          NotToken: TOffset;
          BetweenToken: TOffset;
          Min: TOffset;
          AndToken: TOffset;
          Max: TOffset;
        end;
      private
        Heritage: TRange;
      private
        Nodes: TNodes;
        class function Create(const AParser: TSQLParser; const AExpr, ANotToken, ABetweenToken, AMin, AAndToken, AMax: TOffset): TOffset; static;
      public
        property Parser: TSQLParser read Heritage.Heritage.Heritage.FParser;
      end;

      PBinaryOp = ^TBinaryOp;
      TBinaryOp = packed record
      private type
        TNodes = packed record
          Operand1: TOffset;
          OperatorToken: TOffset;
          Operand2: TOffset;
        end;
      private
        Heritage: TRange;
      private
        Nodes: TNodes;
        class function Create(const AParser: TSQLParser; const AOperator, AOperand1, AOperand2: TOffset): TOffset; static;
        function GetOperand1(): PChild; {$IFNDEF Debug} inline; {$ENDIF}
        function GetOperand2(): PChild; {$IFNDEF Debug} inline; {$ENDIF}
        function GetOperator(): PChild; {$IFNDEF Debug} inline; {$ENDIF}
      public
        property Operand1: PChild read GetOperand1;
        property Operand2: PChild read GetOperand2;
        property Operator: PChild read GetOperator;
        property Parser: TSQLParser read Heritage.Heritage.Heritage.FParser;
      end;

      PCallStmt = ^TCallStmt;
      TCallStmt = packed record
      private type
        TNodes = packed record
          CallTag: TOffset;
          Ident: TOffset;
          ParamList: TOffset;
        end;
      private
        Heritage: TStmt;
      private
        Nodes: TNodes;
        class function Create(const AParser: TSQLParser; const ANodes: TNodes): TOffset; static;
      public
        property Parser: TSQLParser read Heritage.Heritage.Heritage.Heritage.FParser;
      end;

      PCaseOp = ^TCaseOp;
      TCaseOp = packed record
      private type

        PBranch = ^TBranch;
        TBranch = packed record
        private type
          TNodes = packed record
            WhenTag: TOffset;
            CondExpr: TOffset;
            ThenTag: TOffset;
            ResultExpr: TOffset;
          end;
        private
          Heritage: TRange;
        private
          Nodes: TNodes;
          class function Create(const AParser: TSQLParser; const ANodes: TNodes): TOffset; static;
        public
          property Parser: TSQLParser read Heritage.Heritage.Heritage.FParser;
        end;

        TNodes = packed record
          CaseTag: TOffset;
          CompareExpr: TOffset;
          BranchList: TOffset;
          ElseTag: TOffset;
          ElseExpr: TOffset;
          EndTag: TOffset;
        end;
      private
        Heritage: TRange;
      private
        Nodes: TNodes;
        class function Create(const AParser: TSQLParser; const ANodes: TNodes): TOffset; static;
      public
        property Parser: TSQLParser read Heritage.Heritage.Heritage.FParser;
      end;

      PCaseStmt = ^TCaseStmt;
      TCaseStmt = packed record
      private type

        PBranch = ^TBranch;
        TBranch = packed record
        private type
          TNodes = packed record
            Tag: TOffset;
            ConditionExpr: TOffset;
            ThenTag: TOffset;
            StmtList: TOffset;
          end;
        private
          Heritage: TRange;
        private
          Nodes: TNodes;
          class function Create(const AParser: TSQLParser; const ANodes: TNodes): TOffset; static;
        public
          property Parser: TSQLParser read Heritage.Heritage.Heritage.FParser;
        end;

        TNodes = packed record
          CaseTag: TOffset;
          CompareExpr: TOffset;
          BranchList: TOffset;
          EndTag: TOffset;
        end;
      private
        Heritage: TStmt;
      private
        Nodes: TNodes;
        class function Create(const AParser: TSQLParser; const ANodes: TNodes): TOffset; static;
      public
        property Parser: TSQLParser read Heritage.Heritage.Heritage.Heritage.FParser;
      end;

      PCastFunc = ^TCastFunc;
      TCastFunc = packed record
      private type
        TNodes = packed record
          IdentToken: TOffset;
          OpenBracket: TOffset;
          Expr: TOffset;
          AsTag: TOffset;
          Datatype: TOffset;
          CloseBracket: TOffset;
        end;
      private
        Heritage: TRange;
      private
        Nodes: TNodes;
        class function Create(const AParser: TSQLParser; const ANodes: TNodes): TOffset; static;
      public
        property Parser: TSQLParser read Heritage.Heritage.Heritage.FParser;
      end;

      PChangeMasterStmt = ^TChangeMasterStmt;
      TChangeMasterStmt = packed record
      private type
        TOptionNodes = packed record
          MasterBindValue: TOffset;
          MasterHostValue: TOffset;
          MasterUserValue: TOffset;
          MasterPasswordValue: TOffset;
          MasterPortValue: TOffset;
          MasterConnectRetryValue: TOffset;
          MaysterRetryCountValue: TOffset;
          MasterDelayValue: TOffset;
          MasterHeartbeatPeriodValue: TOffset;
          MasterLogFileValue: TOffset;
          MasterLogPosValue: TOffset;
          MasterAutoPositionValue: TOffset;
          RelayLogFileValue: TOffset;
          RelayLogPosValue: TOffset;
          MasterSSLValue: TOffset;
          MasterSSLCAValue: TOffset;
          MasterSSLCaPathValue: TOffset;
          MasterSSLCertValue: TOffset;
          MasterSSLCRLValue: TOffset;
          MasterSSLCRLPathValue: TOffset;
          MasterSSLKeyValue: TOffset;
          MasterSSLCipherValue: TOffset;
          MasterSSLVerifyServerCertValue: TOffset;
          MasterTLSVersionValue: TOffset;
          IgnoreServerIDsValue: TOffset;
        end;
        TNodes = packed record
          StmtTag: TOffset;
          ToTag: TOffset;
          OptionList: TOffset;
          ChannelOptionValue: TOffset;
        end;
      private
        Heritage: TStmt;
      private
        Nodes: TNodes;
        class function Create(const AParser: TSQLParser; const ANodes: TNodes): TOffset; static;
      public
        property Parser: TSQLParser read Heritage.Heritage.Heritage.Heritage.FParser;
      end;

      PCharFunc = ^TCharFunc;
      TCharFunc = packed record
      private type
        TNodes = packed record
          IdentToken: TOffset;
          OpenBracket: TOffset;
          ValueExpr: TOffset;
          UsingTag: TOffset;
          CharsetIdent: TOffset;
          CloseBracket: TOffset;
        end;
      private
        Heritage: TRange;
      private
        Nodes: TNodes;
        class function Create(const AParser: TSQLParser; const ANodes: TNodes): TOffset; static;
      public
        property Parser: TSQLParser read Heritage.Heritage.Heritage.FParser;
      end;

      PChecksumTableStmt = ^TChecksumTableStmt;
      TChecksumTableStmt = packed record
      private type
        TNodes = packed record
          StmtTag: TOffset;
          TablesList: TOffset;
          OptionTag: TOffset;
        end;
      private
        Heritage: TStmt;
      private
        Nodes: TNodes;
        class function Create(const AParser: TSQLParser; const ANodes: TNodes): TOffset; static;
      public
        property Parser: TSQLParser read Heritage.Heritage.Heritage.Heritage.FParser;
      end;

      PCheckTableStmt = ^TCheckTableStmt;
      TCheckTableStmt = packed record
      private type

        POption = ^TOption;
        TOption = packed record
        private type
          TNodes = packed record
            OptionTag: TOffset;
          end;
        private
          Heritage: TRange;
        private
          Nodes: TNodes;
          class function Create(const AParser: TSQLParser; const ANodes: TNodes): TOffset; overload; static;
        public
          property Parser: TSQLParser read Heritage.Heritage.Heritage.FParser;
        end;

        TNodes = packed record
          StmtTag: TOffset;
          TablesList: TOffset;
          OptionList: TOffset;
        end;
      private
        Heritage: TStmt;
      private
        Nodes: TNodes;
        class function Create(const AParser: TSQLParser; const ANodes: TNodes): TOffset; static;
      public
        property Parser: TSQLParser read Heritage.Heritage.Heritage.Heritage.FParser;
      end;

      PCloseStmt = ^TCloseStmt;
      TCloseStmt = packed record
      private type
        TNodes = packed record
          StmtTag: TOffset;
          Ident: TOffset;
        end;
      private
        Heritage: TStmt;
      private
        Nodes: TNodes;
        class function Create(const AParser: TSQLParser; const ANodes: TNodes): TOffset; static;
      public
        property Parser: TSQLParser read Heritage.Heritage.Heritage.Heritage.FParser;
      end;

      PCommitStmt = ^TCommitStmt;
      TCommitStmt = packed record
      private type
        TNodes = packed record
          StmtTag: TOffset;
          ChainTag: TOffset;
          ReleaseTag: TOffset;
        end;
      private
        Heritage: TStmt;
      private
        Nodes: TNodes;
        class function Create(const AParser: TSQLParser; const ANodes: TNodes): TOffset; static;
      public
        property Parser: TSQLParser read Heritage.Heritage.Heritage.Heritage.FParser;
      end;

      PCompoundStmt = ^TCompoundStmt;
      TCompoundStmt = packed record
      private type
        TNodes = packed record
          BeginLabel: TOffset;
          BeginTag: TOffset;
          StmtList: TOffset;
          EndTag: TOffset;
          EndLabel: TOffset;
        end;
      private
        Heritage: TStmt;
      private
        Nodes: TNodes;
        class function Create(const AParser: TSQLParser; const ANodes: TNodes): TOffset; static;
      public
        property Parser: TSQLParser read Heritage.Heritage.Heritage.Heritage.FParser;
      end;

      PConvertFunc = ^TConvertFunc;
      TConvertFunc = packed record
      private type
        TNodes = packed record
          IdentToken: TOffset;
          OpenBracket: TOffset;
          Expr: TOffset;
          Comma: TOffset;
          Datatype: TOffset;
          UsingTag: TOffset;
          CharsetIdent: TOffset;
          CloseBracket: TOffset;
        end;
      private
        Heritage: TRange;
      private
        Nodes: TNodes;
        class function Create(const AParser: TSQLParser; const ANodes: TNodes): TOffset; static;
      public
        property Parser: TSQLParser read Heritage.Heritage.Heritage.FParser;
      end;

      PCreateDatabaseStmt = ^TCreateDatabaseStmt;
      TCreateDatabaseStmt = packed record
      private type
        TNodes = packed record
          StmtTag: TOffset;
          IfNotExistsTag: TOffset;
          DatabaseIdent: TOffset;
          CharacterSetValue: TOffset;
          CollateValue: TOffset;
        end;
      private
        Heritage: TStmt;
      private
        Nodes: TNodes;
        class function Create(const AParser: TSQLParser; const ANodes: TNodes): TOffset; static;
      public
        property Parser: TSQLParser read Heritage.Heritage.Heritage.Heritage.FParser;
      end;

      PCreateEventStmt = ^TCreateEventStmt;
      TCreateEventStmt = packed record
      private type
        TNodes = packed record
          CreateTag: TOffset;
          DefinerNode: TOffset;
          EventTag: TOffset;
          IfNotExistsTag: TOffset;
          EventIdent: TOffset;
          OnScheduleValue: TOffset;
          OnCompletitionTag: TOffset;
          EnableTag: TOffset;
          CommentValue: TOffset;
          DoTag: TOffset;
          Body: TOffset;
        end;
      private
        Heritage: TStmt;
      private
        Nodes: TNodes;
        class function Create(const AParser: TSQLParser; const ANodes: TNodes): TOffset; static;
      public
        property Parser: TSQLParser read Heritage.Heritage.Heritage.Heritage.FParser;
      end;

      PCreateIndexStmt = ^TCreateIndexStmt;
      TCreateIndexStmt = packed record
      private type
        TNodes = packed record
          CreateTag: TOffset;
          IndexTag: TOffset;
          Ident: TOffset;
          OnTag: TOffset;
          TableIdent: TOffset;
          IndexTypeValue: TOffset;
          KeyColumnList: TOffset;
          AlgorithmLockValue: TOffset;
          CommentValue: TOffset;
          KeyBlockSizeValue: TOffset;
          ParserValue: TOffset;
        end;
      private
        Heritage: TStmt;
      private
        Nodes: TNodes;
        class function Create(const AParser: TSQLParser; const ANodes: TNodes): TOffset; static;
      public
        property Parser: TSQLParser read Heritage.Heritage.Heritage.Heritage.FParser;
      end;

      PCreateRoutineStmt = ^TCreateRoutineStmt;
      TCreateRoutineStmt = packed record
      private type
        TNodes = packed record
          CreateTag: TOffset;
          DefinerNode: TOffset;
          RoutineTag: TOffset;
          Ident: TOffset;
          OpenBracket: TOffset;
          ParameterList: TOffset;
          CloseBracket: TOffset;
          Returns: TOffset;
          CommentValue: TOffset;
          LanguageTag: TOffset;
          DeterministicTag: TOffset;
          NatureOfDataTag: TOffset;
          SQLSecurityTag: TOffset;
          Body: TOffset;
        end;
      private
        Heritage: TStmt;
      private
        Nodes: TNodes;
        FRoutineType: TRoutineType;
        class function Create(const AParser: TSQLParser; const ARoutineType: TRoutineType; const ANodes: TNodes): TOffset; static;
      public
        property Parser: TSQLParser read Heritage.Heritage.Heritage.Heritage.FParser;
        property RoutineType: TRoutineType read FRoutineType;
      end;

      PCreateServerStmt = ^TCreateServerStmt;
      TCreateServerStmt = packed record
      private type
        TNodes = packed record
          StmtTag: TOffset;
          Ident: TOffset;
          ForeignDataWrapperValue: TOffset;
          Options: packed record
            Tag: TOffset;
            List: TOffset;
          end;
        end;
      private
        Heritage: TStmt;
      private
        Nodes: TNodes;
        class function Create(const AParser: TSQLParser; const ANodes: TNodes): TOffset; static;
      public
        property Parser: TSQLParser read Heritage.Heritage.Heritage.Heritage.FParser;
      end;

      PCreateTablespaceStmt = ^TCreateTablespaceStmt;
      TCreateTablespaceStmt = packed record
      private type
        TNodes = packed record
          StmtTag: TOffset;
          Ident: TOffset;
          AddDatafileValue: TOffset;
          FileBlockSizeValue: TOffset;
          EngineValue: TOffset;
        end;
      private
        Heritage: TStmt;
      private
        Nodes: TNodes;
        class function Create(const AParser: TSQLParser; const ANodes: TNodes): TOffset; static;
      public
        property Parser: TSQLParser read Heritage.Heritage.Heritage.Heritage.FParser;
      end;

      PCreateTableStmt = ^TCreateTableStmt;
      TCreateTableStmt = packed record
      private type

        TFieldAdd = (caAdd, caChange, caModify, caNone);

        PCheck = ^TCheck;
        TCheck = packed record
        private type
          TNodes = packed record
            CheckTag: TOffset;
            ExprList: TOffset;
          end;
        private
          Heritage: TRange;
        private
          Nodes: TNodes;
          class function Create(const AParser: TSQLParser; const ANodes: TNodes): TOffset; static;
        public
          property Parser: TSQLParser read Heritage.Heritage.Heritage.FParser;
        end;

        PField = ^TField;
        TField = packed record
        private type

          PDefaultFunc = ^TDefaultFunc;
          TDefaultFunc = packed record
          private type
            TNodes = packed record
              IdentToken: TOffset;
              OpenBracket: TOffset;
              FSP: TOffset;
              CloseBracket: TOffset;
            end;
          private
            Heritage: TRange;
          private
            Nodes: TNodes;
            class function Create(const AParser: TSQLParser; const ANodes: TNodes): TOffset; static;
          public
            property Parser: TSQLParser read Heritage.Heritage.Heritage.FParser;
          end;

          TNodes = packed record
            AddTag: TOffset;
            ColumnTag: TOffset;
            OldNameIdent: TOffset;
            NameIdent: TOffset;
            Datatype: TOffset;
            Real: packed record
              DefaultValue: TOffset;
              OnUpdateTag: TOffset;
              AutoIncrementTag: TOffset;
              ColumnFormat: TOffset;
              StorageTag: TOffset;
              Reference: TOffset;
            end;
            Virtual: packed record
              GernatedAlwaysTag: TOffset;
              AsTag: TOffset;
              Expr: TOffset;
              Virtual: TOffset;
            end;
            NullTag: TOffset;
            KeyTag: TOffset;
            CommentValue: TOffset;
            Position: TOffset;
          end;
        private
          Heritage: TRange;
        private
          Nodes: TNodes;
          class function Create(const AParser: TSQLParser; const ANodes: TNodes): TOffset; static;
        public
          property Parser: TSQLParser read Heritage.Heritage.Heritage.FParser;
        end;

        PForeignKey = ^TForeignKey;
        TForeignKey = packed record
        private type
          TNodes = packed record
            AddTag: TOffset;
            ConstraintTag: TOffset;
            SymbolIdent: TOffset;
            ForeignKeyTag: TOffset;
            NameIdent: TOffset;
            ColumnNameList: TOffset;
            Reference: TOffset;
          end;
        private
          Heritage: TRange;
        private
          Nodes: TNodes;
          class function Create(const AParser: TSQLParser; const ANodes: TNodes): TOffset; static;
        public
          property Parser: TSQLParser read Heritage.Heritage.Heritage.FParser;
        end;

        PKey = ^TKey;
        TKey = packed record
        private type
          TNodes = packed record
            AddTag: TOffset;
            ConstraintTag: TOffset;
            SymbolIdent: TOffset;
            KeyTag: TOffset;
            KeyIdent: TOffset;
            IndexTypeTag: TOffset;
            KeyColumnList: TOffset;
            KeyBlockSizeValue: TOffset;
            WithParserValue: TOffset;
            CommentValue: TOffset;
          end;
        private
          Heritage: TRange;
        private
          Nodes: TNodes;
          class function Create(const AParser: TSQLParser; const ANodes: TNodes): TOffset; static;
        public
          property Parser: TSQLParser read Heritage.Heritage.Heritage.FParser;
        end;

        PKeyColumn = ^TKeyColumn;
        TKeyColumn = packed record
        private type
          TNodes = packed record
            IdentTag: TOffset;
            OpenBracket: TOffset;
            LengthToken: TOffset;
            CloseBracket: TOffset;
            SortTag: TOffset;
          end;
        private
          Heritage: TRange;
        private
          Nodes: TNodes;
          class function Create(const AParser: TSQLParser; const ANodes: TNodes): TOffset; static;
        public
          property Parser: TSQLParser read Heritage.Heritage.Heritage.FParser;
        end;

        PPartition = ^TPartition;
        TPartition = packed record
        private type
          TNodes = packed record
            AddTag: TOffset;
            PartitionTag: TOffset;
            NameIdent: TOffset;
            Values: packed record
              Tag: TOffset;
              Value: TOffset;
            end;
            EngineValue: TOffset;
            CommentValue: TOffset;
            DataDirectoryValue: TOffset;
            IndexDirectoryValue: TOffset;
            MaxRowsValue: TOffset;
            MinRowsValue: TOffset;
            SubPartitionList: TOffset;
          end;
        private
          Heritage: TRange;
        private
          Nodes: TNodes;
          class function Create(const AParser: TSQLParser; const ANodes: TNodes): TOffset; static;
        public
          property Parser: TSQLParser read Heritage.Heritage.Heritage.FParser;
        end;

        PReference = ^TReference;
        TReference = packed record
        private type
          TNodes = packed record
            Tag: TOffset;
            ParentTableIdent: TOffset;
            FieldList: TOffset;
            MatchTag: TOffset;
            OnDeleteTag: TOffset;
            OnUpdateTag: TOffset;
          end;
        private
          Heritage: TRange;
        private
          Nodes: TNodes;
          class function Create(const AParser: TSQLParser; const ANodes: TNodes): TOffset; static;
        public
          property Parser: TSQLParser read Heritage.Heritage.Heritage.FParser;
        end;

        TNodes = packed record
          CreateTag: TOffset;
          TemporaryTag: TOffset;
          TableTag: TOffset;
          IfNotExistsTag: TOffset;
          TableIdent: TOffset;
          OpenBracket: TOffset;
          DefinitionList: TOffset;
          TableOptionsNodes: TTableOptionNodes;
          PartitionOption: packed record
            Tag: TOffset;
            KindTag: TOffset;
            AlgorithmValue: TOffset;
            Expr: TOffset;
            Columns: packed record
              Tag: TOffset;
              List: TOffset;
            end;
            Value: TOffset;
            SubPartition: packed record
              Tag: TOffset;
              KindTag: TOffset;
              Expr: TOffset;
              AlgorithmValue: TOffset;
              ColumnList: TOffset;
              Value: TOffset;
            end;
          end;
          PartitionDefinitionList: TOffset;
          LikeTag: TOffset;
          LikeTableIdent: TOffset;
          SelectStmt1: TOffset;
          CloseBracket: TOffset;
          IgnoreReplaceTag: TOffset;
          AsTag: TOffset;
          SelectStmt2: TOffset;
        end;

      private
        Heritage: TStmt;
      private
        Nodes: TNodes;
        class function Create(const AParser: TSQLParser; const ANodes: TNodes): TOffset; static;
      public
        property Parser: TSQLParser read Heritage.Heritage.Heritage.Heritage.FParser;
      end;

      PCreateTriggerStmt = ^TCreateTriggerStmt;
      TCreateTriggerStmt = packed record
      private type
        TNodes = packed record
          CreateTag: TOffset;
          DefinerNode: TOffset;
          TriggerTag: TOffset;
          TriggerIdent: TOffset;
          TimeTag: TOffset;
          EventTag: TOffset;
          OnTag: TOffset;
          TableIdentNode: TOffset;
          ForEachRowTag: TOffset;
          OrderValue: TOffset;
          Body: TOffset;
        end;
      private
        Heritage: TStmt;
      private
        Nodes: TNodes;
        class function Create(const AParser: TSQLParser; const ANodes: TNodes): TOffset; static;
      public
        property Parser: TSQLParser read Heritage.Heritage.Heritage.Heritage.FParser;
      end;

      PCreateUserStmt = ^TCreateUserStmt;
      TCreateUserStmt = packed record
      private type
        TNodes = packed record
          StmtTag: TOffset;
          IfNotExistsTag: TOffset;
          UserSpecifications: TOffset;
          WithTag: TOffset;
          MaxQueriesPerHour: TOffset;
          MaxUpdatesPerHour: TOffset;
          MaxConnectionsPerHour: TOffset;
          MaxUserConnections: TOffset;
          PasswordOption: TOffset;
          AccountTag: TOffset;
        end;
      private
        Heritage: TStmt;
      private
        Nodes: TNodes;
        class function Create(const AParser: TSQLParser; const ANodes: TNodes): TOffset; static;
      public
        property Parser: TSQLParser read Heritage.Heritage.Heritage.Heritage.FParser;
      end;

      PCreateViewStmt = ^TCreateViewStmt;
      TCreateViewStmt = packed record
      private type
        TNodes = packed record
          CreateTag: TOffset;
          OrReplaceTag: TOffset;
          AlgorithmValue: TOffset;
          DefinerNode: TOffset;
          SQLSecurityTag: TOffset;
          ViewTag: TOffset;
          Ident: TOffset;
          FieldList: TOffset;
          AsTag: TOffset;
          SelectStmt: TOffset;
          OptionTag: TOffset;
        end;
      private
        Heritage: TStmt;
      private
        Nodes: TNodes;
        class function Create(const AParser: TSQLParser; const ANodes: TNodes): TOffset; static;
      public
        property Parser: TSQLParser read Heritage.Heritage.Heritage.Heritage.FParser;
      end;

      PCurrentTimestamp = ^TCurrentTimestamp;
      TCurrentTimestamp = packed record
      private type
        TNodes = packed record
          CurrentTimestampTag: TOffset;
          OpenBracket: TOffset;
          LengthInteger: TOffset;
          CloseBracket: TOffset;
        end;
      private
        Heritage: TRange;
      private
        Nodes: TNodes;
        class function Create(const AParser: TSQLParser; const ANodes: TNodes): TOffset; static;
      public
        property Parser: TSQLParser read Heritage.Heritage.Heritage.FParser;
      end;

      PDatatype = ^TDatatype;
      TDatatype = packed record
      private type
        TNodes = packed record
          NationalToken: TOffset;
          SignedToken: TOffset;
          PreDatatypeToken: TOffset;
          DatatypeToken: TOffset;
          OpenBracket: TOffset;
          LengthToken: TOffset;
          CommaToken: TOffset;
          DecimalsToken: TOffset;
          CloseBracket: TOffset;
          ItemsList: TOffset;
          ZerofillTag: TOffset;
          CharacterSetValue: TOffset;
          CollateValue: TOffset;
          BinaryToken: TOffset;
          ASCIIToken: TOffset;
          UnicodeToken: TOffset;
        end;
      private
        Heritage: TRange;
      private
        Nodes: TNodes;
        class function Create(const AParser: TSQLParser; const ANodes: TNodes): TOffset; static;
      public
        property Parser: TSQLParser read Heritage.Heritage.Heritage.FParser;
      end;

      PDateAddFunc = ^TDateAddFunc;
      TDateAddFunc = packed record
      private type
        TNodes = packed record
          IdentToken: TOffset;
          OpenBracket: TOffset;
          Expr: TOffset;
          CommaToken: TOffset;
          IntervalValue: TOffset;
          Days: TOffset;
          CloseBracket: TOffset;
        end;
      private
        Heritage: TRange;
      private
        Nodes: TNodes;
        class function Create(const AParser: TSQLParser; const ANodes: TNodes): TOffset; static;
      public
        property Parser: TSQLParser read Heritage.Heritage.Heritage.FParser;
      end;

      PDbIdent = ^TDbIdent;
      TDbIdent = packed record
      private type
        TNodes = packed record
          Ident: TOffset;
          DatabaseDot: TOffset;
          DatabaseIdent: TOffset;
          TableDot: TOffset;
          TableIdent: TOffset;
        end;
      private
        Heritage: TRange;
      private
        FDbIdentType: TDbIdentType;
        Nodes: TNodes;
        class function Create(const AParser: TSQLParser; const ADbIdentType: TDbIdentType; const ANodes: TNodes): TOffset; overload; static;
        class function Create(const AParser: TSQLParser; const ADbIdentType: TDbIdentType;
          const AIdent, ADatabaseDot, ADatabaseIdent, ATableDot, ATableIdent: TOffset): TOffset; overload; static;
        function GetDatabaseIdent(): PToken; {$IFNDEF Debug} inline; {$ENDIF}
        function GetIdent(): PToken; {$IFNDEF Debug} inline; {$ENDIF}
        function GetParentNode(): PNode; {$IFNDEF Debug} inline; {$ENDIF}
        function GetTableIdent(): PToken; {$IFNDEF Debug} inline; {$ENDIF}
      public
        property DatabaseIdent: PToken read GetDatabaseIdent;
        property DbIdentType: TDbIdentType read FDbIdentType;
        property Ident: PToken read GetIdent;
        property ParentNode: PNode read GetParentNode;
        property Parser: TSQLParser read Heritage.Heritage.Heritage.FParser;
        property TableIdent: PToken read GetTableIdent;
      end;

      PDeallocatePrepareStmt = ^TDeallocatePrepareStmt;
      TDeallocatePrepareStmt = packed record
      private type
        TNodes = packed record
          StmtTag: TOffset;
          Ident: TOffset;
        end;
      private
        Heritage: TStmt;
      private
        Nodes: TNodes;
        class function Create(const AParser: TSQLParser; const ANodes: TNodes): TOffset; static;
      public
        property Parser: TSQLParser read Heritage.Heritage.Heritage.Heritage.FParser;
      end;

      PDeclareStmt = ^TDeclareStmt;
      TDeclareStmt = packed record
      private type
        TNodes = packed record
          StmtTag: TOffset;
          IdentList: TOffset;
          TypeNode: TOffset;
          DefaultValue: TOffset;
          CursorForTag: TOffset;
          SelectStmt: TOffset;
        end;
      private
        Heritage: TStmt;
      private
        Nodes: TNodes;
        class function Create(const AParser: TSQLParser; const ANodes: TNodes): TOffset; static;
      public
        property Parser: TSQLParser read Heritage.Heritage.Heritage.Heritage.FParser;
      end;

      PDeclareConditionStmt = ^TDeclareConditionStmt;
      TDeclareConditionStmt = packed record
      private type
        TNodes = packed record
          StmtTag: TOffset;
          Ident: TOffset;
          ConditionTag: TOffset;
          ForTag: TOffset;
          ErrorCode: TOffset;
          SQLStateTag: TOffset;
          ErrorString: TOffset;
        end;
      private
        Heritage: TStmt;
      private
        Nodes: TNodes;
        class function Create(const AParser: TSQLParser; const ANodes: TNodes): TOffset; static;
      public
        property Parser: TSQLParser read Heritage.Heritage.Heritage.Heritage.FParser;
      end;

      PDeclareCursorStmt = ^TDeclareCursorStmt;
      TDeclareCursorStmt = packed record
      private type
        TNodes = packed record
          StmtTag: TOffset;
          Ident: TOffset;
          CursorTag: TOffset;
          SelectStmt: TOffset;
        end;
      private
        Heritage: TStmt;
      private
        Nodes: TNodes;
        class function Create(const AParser: TSQLParser; const ANodes: TNodes): TOffset; static;
      public
        property Parser: TSQLParser read Heritage.Heritage.Heritage.Heritage.FParser;
      end;

      PDeclareHandlerStmt = ^TDeclareHandlerStmt;
      TDeclareHandlerStmt = packed record
      private type
        TNodes = packed record
          StmtTag: TOffset;
          ActionTag: TOffset;
          HandlerTag: TOffset;
          ForTag: TOffset;
          ConditionsList: TOffset;
          Stmt: TOffset;
        end;
      private
        Heritage: TStmt;
      private
        Nodes: TNodes;
        class function Create(const AParser: TSQLParser; const ANodes: TNodes): TOffset; static;
      public
        property Parser: TSQLParser read Heritage.Heritage.Heritage.Heritage.FParser;
      end;

      PDeleteStmt = ^TDeleteStmt;
      TDeleteStmt = packed record
      private type
        TNodes = packed record
          DeleteTag: TOffset;
          LowPriorityTag: TOffset;
          QuickTag: TOffset;
          IgnoreTag: TOffset;
          From: packed record
            Tag: TOffset;
            List: TOffset;
          end;
          TableReferenceList: TOffset;
          Partition: packed record
            Tag: TOffset;
            List: TOffset;
          end;
          Using: packed record
            Tag: TOffset;
            List: TOffset;
          end;
          WhereValue: TOffset;
          OrderBy: packed record
            Tag: TOffset;
            Expr: TOffset;
          end;
          Limit: packed record
            Tag: TOffset;
            Expr: TOffset;
          end;
        end;
      private
        Heritage: TStmt;
      private
        Nodes: TNodes;
        class function Create(const AParser: TSQLParser; const ANodes: TNodes): TOffset; static;
      public
        property Parser: TSQLParser read Heritage.Heritage.Heritage.Heritage.FParser;
      end;

      PDoStmt = ^TDoStmt;
      TDoStmt = packed record
      private type
        TNodes = packed record
          DoTag: TOffset;
          ExprList: TOffset;
        end;
      private
        Heritage: TStmt;
      private
        Nodes: TNodes;
        class function Create(const AParser: TSQLParser; const ANodes: TNodes): TOffset; static;
      public
        property Parser: TSQLParser read Heritage.Heritage.Heritage.Heritage.FParser;
      end;

      PDropDatabaseStmt = ^TDropDatabaseStmt;
      TDropDatabaseStmt = packed record
      private type
        TNodes = packed record
          StmtTag: TOffset;
          IfExistsTag: TOffset;
          DatabaseIdent: TOffset;
        end;
      private
        Heritage: TStmt;
      private
        Nodes: TNodes;
        class function Create(const AParser: TSQLParser; const ANodes: TNodes): TOffset; static;
      public
        property Parser: TSQLParser read Heritage.Heritage.Heritage.Heritage.FParser;
      end;

      PDropEventStmt = ^TDropEventStmt;
      TDropEventStmt = packed record
      private type
        TNodes = packed record
          StmtTag: TOffset;
          IfExistsTag: TOffset;
          EventIdent: TOffset;
        end;
      private
        Heritage: TStmt;
      private
        Nodes: TNodes;
        class function Create(const AParser: TSQLParser; const ANodes: TNodes): TOffset; static;
      public
        property Parser: TSQLParser read Heritage.Heritage.Heritage.Heritage.FParser;
      end;

      PDropIndexStmt = ^TDropIndexStmt;
      TDropIndexStmt = packed record
      private type
        TNodes = packed record
          StmtTag: TOffset;
          IndexIdent: TOffset;
          OnTag: TOffset;
          TableIdent: TOffset;
          AlgorithmValue: TOffset;
          LockValue: TOffset;
        end;
      private
        Heritage: TStmt;
      private
        Nodes: TNodes;
        class function Create(const AParser: TSQLParser; const ANodes: TNodes): TOffset; static;
      public
        property Parser: TSQLParser read Heritage.Heritage.Heritage.Heritage.FParser;
      end;

      PDropRoutineStmt = ^TDropRoutineStmt;
      TDropRoutineStmt = packed record
      private type
        TNodes = packed record
          StmtTag: TOffset;
          IfExistsTag: TOffset;
          RoutineIdent: TOffset;
        end;
      private
        Heritage: TStmt;
      private
        Nodes: TNodes;
        FRoutineType: TRoutineType;
        class function Create(const AParser: TSQLParser; const ARoutineType: TRoutineType; const ANodes: TNodes): TOffset; static;
      public
        property Parser: TSQLParser read Heritage.Heritage.Heritage.Heritage.FParser;
        property RoutineType: TRoutineType read FRoutineType;
      end;

      PDropServerStmt = ^TDropServerStmt;
      TDropServerStmt = packed record
      private type
        TNodes = packed record
          StmtTag: TOffset;
          IfExistsTag: TOffset;
          ServerIdent: TOffset;
        end;
      private
        Heritage: TStmt;
      private
        Nodes: TNodes;
        class function Create(const AParser: TSQLParser; const ANodes: TNodes): TOffset; static;
      public
        property Parser: TSQLParser read Heritage.Heritage.Heritage.Heritage.FParser;
      end;

      PDropTablespaceStmt = ^TDropTablespaceStmt;
      TDropTablespaceStmt = packed record
      private type
        TNodes = packed record
          StmtTag: TOffset;
          Ident: TOffset;
          EngineValue: TOffset;
        end;
      private
        Heritage: TStmt;
      private
        Nodes: TNodes;
        class function Create(const AParser: TSQLParser; const ANodes: TNodes): TOffset; static;
      public
        property Parser: TSQLParser read Heritage.Heritage.Heritage.Heritage.FParser;
      end;

      PDropTableStmt = ^TDropTableStmt;
      TDropTableStmt = packed record
      private type
        TNodes = packed record
          StmtTag: TOffset;
          IfExistsTag: TOffset;
          TableIdentList: TOffset;
          RestrictCascadeTag: TOffset;
        end;
      private
        Heritage: TStmt;
      private
        Nodes: TNodes;
        class function Create(const AParser: TSQLParser; const ANodes: TNodes): TOffset; static;
      public
        property Parser: TSQLParser read Heritage.Heritage.Heritage.Heritage.FParser;
      end;

      PDropTriggerStmt = ^TDropTriggerStmt;
      TDropTriggerStmt = packed record
      private type
        TNodes = packed record
          StmtTag: TOffset;
          IfExistsTag: TOffset;
          TriggerIdent: TOffset;
        end;
      private
        Heritage: TStmt;
      private
        Nodes: TNodes;
        class function Create(const AParser: TSQLParser; const ANodes: TNodes): TOffset; static;
      public
        property Parser: TSQLParser read Heritage.Heritage.Heritage.Heritage.FParser;
      end;

      PDropUserStmt = ^TDropUserStmt;
      TDropUserStmt = packed record
      private type
        TNodes = packed record
          StmtTag: TOffset;
          IfExistsTag: TOffset;
          UserList: TOffset;
        end;
      private
        Heritage: TStmt;
      private
        Nodes: TNodes;
        class function Create(const AParser: TSQLParser; const ANodes: TNodes): TOffset; static;
      public
        property Parser: TSQLParser read Heritage.Heritage.Heritage.Heritage.FParser;
      end;

      PDropViewStmt = ^TDropViewStmt;
      TDropViewStmt = packed record
      private type
        TNodes = packed record
          StmtTag: TOffset;
          IfExistsTag: TOffset;
          ViewIdentList: TOffset;
          RestrictCascadeTag: TOffset;
        end;
      private
        Heritage: TStmt;
      private
        Nodes: TNodes;
        class function Create(const AParser: TSQLParser; const ANodes: TNodes): TOffset; static;
      public
        property Parser: TSQLParser read Heritage.Heritage.Heritage.Heritage.FParser;
      end;

      PEndLabel = ^TEndLabel;
      TEndLabel = packed record
      private type
        TNodes = packed record
          LabelToken: TOffset;
        end;
      private
        Heritage: TRange;
      private
        Nodes: TNodes;
        class function Create(const AParser: TSQLParser; const ANodes: TNodes): TOffset; static;
      public
        property Parser: TSQLParser read Heritage.Heritage.Heritage.FParser;
      end;

      PExecuteStmt = ^TExecuteStmt;
      TExecuteStmt = packed record
      private type
        TNodes = packed record
          StmtTag: TOffset;
          StmtVariable: TOffset;
          UsingTag: TOffset;
          VariableIdents: TOffset;
        end;
      private
        Heritage: TStmt;
      private
        Nodes: TNodes;
        class function Create(const AParser: TSQLParser; const ANodes: TNodes): TOffset; static;
      public
        property Parser: TSQLParser read Heritage.Heritage.Heritage.Heritage.FParser;
      end;

      PExistsFunc = ^TExistsFunc;
      TExistsFunc = packed record
      private type
        TNodes = packed record
          IdentToken: TOffset;
          OpenBracket: TOffset;
          SubQuery: TOffset;
          CloseBracket: TOffset;
        end;
      private
        Heritage: TRange;
      private
        Nodes: TNodes;
        class function Create(const AParser: TSQLParser; const ANodes: TNodes): TOffset; static;
      public
        property Parser: TSQLParser read Heritage.Heritage.Heritage.FParser;
      end;

      PExplainStmt = ^TExplainStmt;
      TExplainStmt = packed record
      private type
        TNodes = packed record
          StmtTag: TOffset;
          TableIdent: TOffset;
          FieldIdent: TOffset;
          ExplainType: TOffset;
          AssignToken: TOffset;
          FormatKeyword: TOffset;
          ExplainStmt: TOffset;
          ForConnectionValue: TOffset;
        end;
      private
        Heritage: TStmt;
      private
        Nodes: TNodes;
        class function Create(const AParser: TSQLParser; const ANodes: TNodes): TOffset; static;
      public
        property Parser: TSQLParser read Heritage.Heritage.Heritage.Heritage.FParser;
      end;

      PExtractFunc = ^TExtractFunc;
      TExtractFunc = packed record
      private type
        TNodes = packed record
          IdentToken: TOffset;
          OpenBracket: TOffset;
          UnitTag: TOffset;
          FromTag: TOffset;
          DateExpr: TOffset;
          CloseBracket: TOffset;
        end;
      private
        Heritage: TRange;
      private
        Nodes: TNodes;
        class function Create(const AParser: TSQLParser; const ANodes: TNodes): TOffset; static;
      public
        property Parser: TSQLParser read Heritage.Heritage.Heritage.FParser;
      end;

      PFetchStmt = ^TFetchStmt;
      TFetchStmt = packed record
      private type
        TNodes = packed record
          StmtTag: TOffset;
          FromTag: TOffset;
          Ident: TOffset;
          IntoTag: TOffset;
          VariableList: TOffset;
        end;
      private
        Heritage: TStmt;
      private
        Nodes: TNodes;
        class function Create(const AParser: TSQLParser; const ANodes: TNodes): TOffset; static;
      public
        property Parser: TSQLParser read Heritage.Heritage.Heritage.Heritage.FParser;
      end;

      PFlushStmt = ^TFlushStmt;
      TFlushStmt = packed record
      private type

          POption = ^TOption;
          TOption = packed record
          private type
            TNodes = packed record
              OptionTag: TOffset;
              TablesList: TOffset;
            end;
          private
            Heritage: TRange;
          private
            Nodes: TNodes;
            class function Create(const AParser: TSQLParser; const ANodes: TNodes): TOffset; static;
          public
            property Parser: TSQLParser read Heritage.Heritage.Heritage.FParser;
          end;

        TNodes = packed record
          StmtTag: TOffset;
          NoWriteToBinLogTag: TOffset;
          OptionList: TOffset;
        end;
      private
        Heritage: TStmt;
      private
        Nodes: TNodes;
        class function Create(const AParser: TSQLParser; const ANodes: TNodes): TOffset; static;
      public
        property Parser: TSQLParser read Heritage.Heritage.Heritage.Heritage.FParser;
      end;

      PFunctionCall = ^TFunctionCall;
      TFunctionCall = packed record
      private type
        TNodes = packed record
          Ident: TOffset;
          ArgumentsList: TOffset;
        end;
      private
        Heritage: TRange;
      private
        Nodes: TNodes;
        class function Create(const AParser: TSQLParser; const AIdent, AArgumentsList: TOffset): TOffset; overload; static;
        class function Create(const AParser: TSQLParser; const ANodes: TNodes): TOffset; overload; static;
        function GetArguments(): PChild; {$IFNDEF Debug} inline; {$ENDIF}
        function GetIdent(): PChild; {$IFNDEF Debug} inline; {$ENDIF}
      public
        property Arguments: PChild read GetArguments;
        property Ident: PChild read GetIdent;
        property Parser: TSQLParser read Heritage.Heritage.Heritage.FParser;
      end;

      PFunctionReturns = ^TFunctionReturns;
      TFunctionReturns = packed record
      private type
        TNodes = packed record
          ReturnsTag: TOffset;
          DatatypeNode: TOffset;
          CharsetValue: TOffset;
        end;
      private
        Heritage: TRange;
      private
        Nodes: TNodes;
        class function Create(const AParser: TSQLParser; const ANodes: TNodes): TOffset; static;
      public
        property Parser: TSQLParser read Heritage.Heritage.Heritage.FParser;
      end;

      PGetDiagnosticsStmt = ^TGetDiagnosticsStmt;
      TGetDiagnosticsStmt = packed record
      private type

        PStmtInfo = ^TStmtInfo;
        TStmtInfo = packed record
        private type
          TNodes = packed record
            Target: TOffset;
            EqualOp: TOffset;
            ItemTag: TOffset;
          end;
        private
          Heritage: TRange;
        private
          Nodes: TNodes;
          class function Create(const AParser: TSQLParser; const ANodes: TNodes): TOffset; static;
        public
          property Parser: TSQLParser read Heritage.Heritage.Heritage.FParser;
        end;

        PCondInfo = ^TCondInfo;
        TCondInfo = packed record
        private type
          TNodes = packed record
            Target: TOffset;
            EqualOp: TOffset;
            ItemTag: TOffset;
          end;
        private
          Heritage: TRange;
        private
          Nodes: TNodes;
          class function Create(const AParser: TSQLParser; const ANodes: TNodes): TOffset; static;
        public
          property Parser: TSQLParser read Heritage.Heritage.Heritage.FParser;
        end;

        TNodes = packed record
          StmtTag: TOffset;
          ScopeTag: TOffset;
          DiagnosticsTag: TOffset;
          ConditionTag: TOffset;
          ConditionNumber: TOffset;
          InfoList: TOffset;
        end;
      private
        Heritage: TStmt;
      private
        Nodes: TNodes;
        class function Create(const AParser: TSQLParser; const ANodes: TNodes): TOffset; static;
      public
        property Parser: TSQLParser read Heritage.Heritage.Heritage.Heritage.FParser;
      end;

      PGrantStmt = ^TGrantStmt;
      TGrantStmt = packed record
      private type

        PPrivileg = ^TPrivileg;
        TPrivileg = packed record
        private type
          TNodes = packed record
            PrivilegTag: TOffset;
            ColumnList: TOffset;
          end;
        private
          Heritage: TRange;
        private
          Nodes: TNodes;
          class function Create(const AParser: TSQLParser; const ANodes: TNodes): TOffset; static;
        public
          property Parser: TSQLParser read Heritage.Heritage.Heritage.FParser;
        end;

        PUserSpecification = ^TUserSpecification;
        TUserSpecification = packed record
        private type
          TNodes = packed record
            UserIdent: TOffset;
            IdentifiedToken: TOffset;
            PluginIdent: TOffset;
            AsTag: TOffset;
            AuthString: TOffset;
            WithGrantOptionTag: TOffset;
          end;
        private
          Heritage: TRange;
        private
          Nodes: TNodes;
          class function Create(const AParser: TSQLParser; const ANodes: TNodes): TOffset; static;
        public
          property Parser: TSQLParser read Heritage.Heritage.Heritage.FParser;
        end;

        TNodes = packed record
          StmtTag: TOffset;
          PrivilegesList: TOffset;
          OnTag: TOffset;
          OnUser: TOffset;
          ObjectValue: TOffset;
          ToTag: TOffset;
          UserSpecifications: TOffset;
          RequireTag: TOffset;
          WithTag: TOffset;
          GrantOptionTag: TOffset;
          MaxQueriesPerHourTag: TOffset;
          MaxUpdatesPerHourTag: TOffset;
          MaxConnectionsPerHourTag: TOffset;
          MaxUserConnectionsTag: TOffset;
        end;
      private
        Heritage: TStmt;
      private
        Nodes: TNodes;
        class function Create(const AParser: TSQLParser; const ANodes: TNodes): TOffset; static;
      public
        property Parser: TSQLParser read Heritage.Heritage.Heritage.Heritage.FParser;
      end;

      PGroupConcatFunc = ^TGroupConcatFunc;
      TGroupConcatFunc = packed record
      private type

        PExpr = ^TExpr;
        TExpr = packed record
        private type
          TNodes = packed record
            Expr: TOffset;
            Direction: TOffset;
          end;
        private
          Heritage: TRange;
        private
          Nodes: TNodes;
          class function Create(const AParser: TSQLParser; const ANodes: TNodes): TOffset; static;
        public
          property Parser: TSQLParser read Heritage.Heritage.Heritage.FParser;
        end;

        TNodes = packed record
          IdentToken: TOffset;
          OpenBracket: TOffset;
          DistinctTag: TOffset;
          ExprList: TOffset;
          OrderByTag: TOffset;
          OrderByExprList: TOffset;
          SeparatorValue: TOffset;
          CloseBracket: TOffset;
        end;
      private
        Heritage: TRange;
      private
        Nodes: TNodes;
        class function Create(const AParser: TSQLParser; const ANodes: TNodes): TOffset; static;
      public
        property Parser: TSQLParser read Heritage.Heritage.Heritage.FParser;
      end;

      PHelpStmt = ^THelpStmt;
      THelpStmt = packed record
      private type
        TNodes = packed record
          StmtTag: TOffset;
          HelpString: TOffset;
        end;
      private
        Heritage: TStmt;
      private
        Nodes: TNodes;
        class function Create(const AParser: TSQLParser; const ANodes: TNodes): TOffset; static;
      public
        property Parser: TSQLParser read Heritage.Heritage.Heritage.Heritage.FParser;
      end;

      PIfStmt = ^TIfStmt;
      TIfStmt = packed record
      private type

        PBranch = ^TBranch;
        TBranch = packed record
        private type
          TNodes = packed record
            Tag: TOffset;
            ConditionExpr: TOffset;
            ThenTag: TOffset;
            StmtList: TOffset;
          end;
        private
          Heritage: TRange;
        private
          Nodes: TNodes;
          class function Create(const AParser: TSQLParser; const ANodes: TNodes): TOffset; static;
        public
          property Parser: TSQLParser read Heritage.Heritage.Heritage.FParser;
        end;

        TNodes = packed record
          BranchList: TOffset;
          EndTag: TOffset;
        end;
      private
        Heritage: TStmt;
      private
        Nodes: TNodes;
        class function Create(const AParser: TSQLParser; const ANodes: TNodes): TOffset; static;
      public
        property Parser: TSQLParser read Heritage.Heritage.Heritage.Heritage.FParser;
      end;

      PInOp = ^TInOp;
      TInOp = packed record
      private type
        TNodes = packed record
          Operand: TOffset;
          NotToken: TOffset;
          InToken: TOffset;
          List: TOffset;
        end;
      private
        Heritage: TRange;
      private
        Nodes: TNodes;
        class function Create(const AParser: TSQLParser; const ANodes: TNodes): TOffset; static;
      public
        property Parser: TSQLParser read Heritage.Heritage.Heritage.FParser;
      end;

      PInsertStmt = ^TInsertStmt;
      TInsertStmt = packed record
      private type

        PSetItem = ^TSetItem;
        TSetItem = packed record
        private type
          TNodes = packed record
            FieldToken: TOffset;
            AssignToken: TOffset;
            ValueNode: TOffset;
          end;
        private
          Heritage: TRange;
        private
          Nodes: TNodes;
          class function Create(const AParser: TSQLParser; const ANodes: TNodes): TOffset; static;
        public
          property Parser: TSQLParser read Heritage.Heritage.Heritage.FParser;
        end;

        TNodes = packed record
          InsertTag: TOffset;
          PriorityTag: TOffset;
          IgnoreTag: TOffset;
          IntoTag: TOffset;
          TableIdent: TOffset;
          Partition: packed record
            Tag: TOffset;
            List: TOffset;
          end;
          ColumnList: TOffset;
          Values: packed record
            Tag: TOffset;
            List: TOffset;
          end;
          Set_: packed record
            Tag: TOffset;
            List: TOffset;
          end;
          SelectStmt: TOffset;
          OnDuplicateKeyUpdateTag: TOffset;
          UpdateList: TOffset;
        end;
      private
        Heritage: TStmt;
      private
        Nodes: TNodes;
        class function Create(const AParser: TSQLParser; const ANodes: TNodes): TOffset; static;
      public
        property Parser: TSQLParser read Heritage.Heritage.Heritage.Heritage.FParser;
      end;

      PIntervalOp = ^TIntervalOp;
      TIntervalOp = packed record
      private type

        TNodes = packed record
          QuantityExp: TOffset;
          UnitTag: TOffset;
        end;
      private
        Heritage: TRange;
      private
        Nodes: TNodes;
        class function Create(const AParser: TSQLParser; const ANodes: TNodes): TOffset; static;
      end;

      TIntervalList = array [0..15 - 1] of TOffset;

      PIterateStmt = ^TIterateStmt;
      TIterateStmt = packed record
      private type
        TNodes = packed record
          IterateToken: TOffset;
          LabelToken: TOffset;
        end;
      private
        Heritage: TStmt;
      private
        Nodes: TNodes;
        class function Create(const AParser: TSQLParser; const ANodes: TNodes): TOffset; static;
      public
        property Parser: TSQLParser read Heritage.Heritage.Heritage.Heritage.FParser;
      end;

      PKillStmt = ^TKillStmt;
      TKillStmt = packed record
      private type
        TNodes = packed record
          StmtTag: TOffset;
          ProcessIdToken: TOffset;
        end;
      private
        Heritage: TStmt;
      private
        Nodes: TNodes;
        class function Create(const AParser: TSQLParser; const ANodes: TNodes): TOffset; static;
      public
        property Parser: TSQLParser read Heritage.Heritage.Heritage.Heritage.FParser;
      end;

      PLeaveStmt = ^TLeaveStmt;
      TLeaveStmt = packed record
      private type
        TNodes = packed record
          StmtTag: TOffset;
          LabelToken: TOffset;
        end;
      private
        Heritage: TStmt;
      private
        Nodes: TNodes;
        class function Create(const AParser: TSQLParser; const ANodes: TNodes): TOffset; static;
      public
        property Parser: TSQLParser read Heritage.Heritage.Heritage.Heritage.FParser;
      end;

      PLikeOp = ^TLikeOp;
      TLikeOp = packed record
      private type
        TNodes = packed record
          Operand1: TOffset;
          NotToken: TOffset;
          LikeToken: TOffset;
          Operand2: TOffset;
          EscapeToken: TOffset;
          EscapeCharToken: TOffset;
        end;
      private
        Heritage: TRange;
      private
        Nodes: TNodes;
        class function Create(const AParser: TSQLParser; const ANodes: TNodes): TOffset; static;
      public
        property Parser: TSQLParser read Heritage.Heritage.Heritage.FParser;
      end;

      // PList = ^TList; defined after TList definition, otherwise its assigns Classes.TList
      TList = packed record
      private type
        TNodes = packed record
          OpenBracket: TOffset;
          FirstChild: TOffset;
          CloseBracket: TOffset;
        end;
      private
        Heritage: TRange;
      private
        FDelimiterType: TTokenType;
        FElementCount: Word;
        Nodes: TNodes;
        class function Create(const AParser: TSQLParser;
          const ANodes: TNodes; const ADelimiterType: TTokenType;
          const AChildrenCount: Integer; const AChildren: array of TOffset): TOffset; static;
        function GetFirstChild(): PChild; {$IFNDEF Debug} inline; {$ENDIF}
      public
        property DelimiterType: TTokenType read FDelimiterType;
        property ElementCount: Word read FElementCount;
        property FirstChild: PChild read GetFirstChild;
        property Parser: TSQLParser read Heritage.Heritage.Heritage.FParser;
      end;
      PList = ^TList;

      PLoadDataStmt = ^TLoadDataStmt;
      TLoadDataStmt = packed record
      private type
        TNodes = packed record
          StmtTag: TOffset;
          PriorityTag: TOffset;
          InfileTag: TOffset;
          FilenameString: TOffset;
          ReplaceIgnoreTag: TOffset;
          IntoTableTag: TOffset;
          Ident: TOffset;
          PartitionTag: TOffset;
          PartitionList: TOffset;
          CharacterSetValue: TOffset;
          ColumnsTag: TOffset;
          ColumnsTerminatedByValue: TOffset;
          EnclosedByValue: TOffset;
          EscapedByValue: TOffset;
          LinesTag: TOffset;
          StartingByValue: TOffset;
          LinesTerminatedByValue: TOffset;
          Ignore: packed record
            Tag: TOffset;
            NumberToken: TOffset;
            LinesTag: TOffset;
          end;
          ColumnList: TOffset;
          SetList: TOffset;
        end;
      private
        Heritage: TStmt;
      private
        Nodes: TNodes;
        class function Create(const AParser: TSQLParser; const ANodes: TNodes): TOffset; static;
      public
        property Parser: TSQLParser read Heritage.Heritage.Heritage.Heritage.FParser;
      end;

      PLoadXMLStmt = ^TLoadXMLStmt;
      TLoadXMLStmt = packed record
      private type
        TNodes = packed record
          StmtTag: TOffset;
          PriorityTag: TOffset;
          InfileTag: TOffset;
          FilenameString: TOffset;
          ReplaceIgnoreTag: TOffset;
          IntoTableValue: TOffset;
          CharacterSetValue: TOffset;
          RowsIdentifiedByValue: TOffset;
          Ignore: packed record
            Tag: TOffset;
            NumberToken: TOffset;
            LinesTag: TOffset;
          end;
          FieldList: TOffset;
          SetList: TOffset;
        end;
      private
        Heritage: TStmt;
      private
        Nodes: TNodes;
        class function Create(const AParser: TSQLParser; const ANodes: TNodes): TOffset; static;
      public
        property Parser: TSQLParser read Heritage.Heritage.Heritage.Heritage.FParser;
      end;

      PLockTableStmt = ^TLockTableStmt;
      TLockTableStmt = packed record
      private type

        PItem = ^TItem;
        TItem = packed record
        private type
          TNodes = packed record
            TableIdent: TOffset;
            AsTag: TOffset;
            AliasIdent: TOffset;
            TypeTag: TOffset;
          end;
        private
          Heritage: TRange;
        private
          Nodes: TNodes;
          class function Create(const AParser: TSQLParser; const ANodes: TNodes): TOffset; static;
        public
          property Parser: TSQLParser read Heritage.Heritage.Heritage.FParser;
        end;

        TNodes = packed record
          LockTablesTag: TOffset;
          ItemList: TOffset;
        end;
      private
        Heritage: TStmt;
      private
        Nodes: TNodes;
        class function Create(const AParser: TSQLParser; const ANodes: TNodes): TOffset; static;
      public
        property Parser: TSQLParser read Heritage.Heritage.Heritage.Heritage.FParser;
      end;

      PLoopStmt = ^TLoopStmt;
      TLoopStmt = packed record
      private type
        TNodes = packed record
          BeginLabel: TOffset;
          BeginTag: TOffset;
          StmtList: TOffset;
          EndTag: TOffset;
          EndLabel: TOffset;
        end;
      private
        Heritage: TStmt;
      private
        Nodes: TNodes;
        class function Create(const AParser: TSQLParser; const ANodes: TNodes): TOffset; static;
      public
        property Parser: TSQLParser read Heritage.Heritage.Heritage.Heritage.FParser;
      end;

      PMatchFunc = ^TMatchFunc;
      TMatchFunc = packed record
      private type
        TNodes = packed record
          IdentToken: TOffset;
          MatchList: TOffset;
          AgainstTag: TOffset;
          OpenBracket: TOffset;
          Expr: TOffset;
          SearchModifierTag: TOffset;
          CloseBracket: TOffset;
        end;
      private
        Heritage: TRange;
      private
        Nodes: TNodes;
        class function Create(const AParser: TSQLParser; const ANodes: TNodes): TOffset; static;
      public
        property Parser: TSQLParser read Heritage.Heritage.Heritage.FParser;
      end;

      PPositionFunc = ^TPositionFunc;
      TPositionFunc = packed record
      private type
        TNodes = packed record
          IdentToken: TOffset;
          OpenBracket: TOffset;
          SubStr: TOffset;
          InTag: TOffset;
          Str: TOffset;
          CloseBracket: TOffset;
        end;
      private
        Heritage: TRange;
      private
        Nodes: TNodes;
        class function Create(const AParser: TSQLParser; const ANodes: TNodes): TOffset; static;
      public
        property Parser: TSQLParser read Heritage.Heritage.Heritage.FParser;
      end;

      PPrepareStmt = ^TPrepareStmt;
      TPrepareStmt = packed record
      private type
        TNodes = packed record
          StmtTag: TOffset;
          StmtIdent: TOffset;
          FromTag: TOffset;
          StmtVariable: TOffset;
        end;
      private
        Heritage: TStmt;
      private
        Nodes: TNodes;
        class function Create(const AParser: TSQLParser; const ANodes: TNodes): TOffset; static;
      public
        property Parser: TSQLParser read Heritage.Heritage.Heritage.Heritage.FParser;
      end;

      PPurgeStmt = ^TPurgeStmt;
      TPurgeStmt = packed record
      private type
        TNodes = packed record
          StmtTag: TOffset;
          TypeTag: TOffset;
          LogsTag: TOffset;
          Value: TOffset;
        end;
      private
        Heritage: TStmt;
      private
        Nodes: TNodes;
        class function Create(const AParser: TSQLParser; const ANodes: TNodes): TOffset; static;
      public
        property Parser: TSQLParser read Heritage.Heritage.Heritage.Heritage.FParser;
      end;

      POpenStmt = ^TOpenStmt;
      TOpenStmt = packed record
      private type
        TNodes = packed record
          StmtTag: TOffset;
          Ident: TOffset;
        end;
      private
        Heritage: TStmt;
      private
        Nodes: TNodes;
        class function Create(const AParser: TSQLParser; const ANodes: TNodes): TOffset; static;
      public
        property Parser: TSQLParser read Heritage.Heritage.Heritage.Heritage.FParser;
      end;

      POptimizeTableStmt = ^TOptimizeTableStmt;
      TOptimizeTableStmt = packed record
      private type
        TNodes = packed record
          StmtTag: TOffset;
          OptionTag: TOffset;
          TableTag: TOffset;
          TablesList: TOffset;
        end;
      private
        Heritage: TStmt;
      private
        Nodes: TNodes;
        class function Create(const AParser: TSQLParser; const ANodes: TNodes): TOffset; static;
      public
        property Parser: TSQLParser read Heritage.Heritage.Heritage.Heritage.FParser;
      end;

      PRegExpOp = ^TRegExpOp;
      TRegExpOp = packed record
      private type
        TNodes = packed record
          Operand1: TOffset;
          NotToken: TOffset;
          RegExpToken: TOffset;
          Operand2: TOffset;
        end;
      private
        Heritage: TRange;
      private
        Nodes: TNodes;
        class function Create(const AParser: TSQLParser; const ANodes: TNodes): TOffset; static;
      public
        property Parser: TSQLParser read Heritage.Heritage.Heritage.FParser;
      end;

      PReleaseStmt = ^TReleaseStmt;
      TReleaseStmt = packed record
      private type
        TNodes = packed record
          ReleaseTag: TOffset;
          Ident: TOffset;
        end;
      private
        Heritage: TStmt;
      private
        Nodes: TNodes;
        class function Create(const AParser: TSQLParser; const ANodes: TNodes): TOffset; static;
      public
        property Parser: TSQLParser read Heritage.Heritage.Heritage.Heritage.FParser;
      end;

      PRenameStmt = ^TRenameStmt;
      TRenameStmt = packed record
      private type

        PPair = ^TPair;
        TPair = packed record
        private type
          TNodes = packed record
            OrgNode: TOffset;
            ToTag: TOffset;
            NewNode: TOffset;
          end;
        private
          Heritage: TRange;
        private
          Nodes: TNodes;
          class function Create(const AParser: TSQLParser; const ANodes: TNodes): TOffset; static;
        public
          property Parser: TSQLParser read Heritage.Heritage.Heritage.FParser;
        end;

        TNodes = packed record
          StmtTag: TOffset;
          RenameList: TOffset;
        end;
      private
        Heritage: TStmt;
      private
        Nodes: TNodes;
        class function Create(const AParser: TSQLParser; const ANodes: TNodes): TOffset; static;
      public
        property Parser: TSQLParser read Heritage.Heritage.Heritage.Heritage.FParser;
      end;

      PRepairTableStmt = ^TRepairTableStmt;
      TRepairTableStmt = packed record
      private type
        TNodes = packed record
          StmtTag: TOffset;
          OptionTag: TOffset;
          TableTag: TOffset;
          TablesList: TOffset;
          QuickTag: TOffset;
          ExtendedTag: TOffset;
          UseFrmTag: TOffset;
        end;
      private
        Heritage: TStmt;
      private
        Nodes: TNodes;
        class function Create(const AParser: TSQLParser; const ANodes: TNodes): TOffset; static;
      public
        property Parser: TSQLParser read Heritage.Heritage.Heritage.Heritage.FParser;
      end;

      PRepeatStmt = ^TRepeatStmt;
      TRepeatStmt = packed record
      private type
        TNodes = packed record
          BeginLabel: TOffset;
          RepeatTag: TOffset;
          StmtList: TOffset;
          UntilTag: TOffset;
          SearchConditionExpr: TOffset;
          EndTag: TOffset;
          EndLabel: TOffset;
        end;
      private
        Heritage: TStmt;
      private
        Nodes: TNodes;
        class function Create(const AParser: TSQLParser; const ANodes: TNodes): TOffset; static;
      public
        property Parser: TSQLParser read Heritage.Heritage.Heritage.Heritage.FParser;
      end;

      PResetStmt = ^TResetStmt;
      TResetStmt = packed record
      private type

        POption = ^TOption;
        TOption = packed record
        private type
          TNodes = packed record
            OptionTag: TOffset;
            ChannelValue: TOffset;
          end;
        private
          Heritage: TRange;
        private
          Nodes: TNodes;
          class function Create(const AParser: TSQLParser; const ANodes: TNodes): TOffset; static;
        public
          property Parser: TSQLParser read Heritage.Heritage.Heritage.FParser;
        end;

        TNodes = packed record
          StmtTag: TOffset;
          OptionList: TOffset;
        end;
      private
        Heritage: TStmt;
      private
        Nodes: TNodes;
        class function Create(const AParser: TSQLParser; const ANodes: TNodes): TOffset; static;
      public
        property Parser: TSQLParser read Heritage.Heritage.Heritage.Heritage.FParser;
      end;

      PResignalStmt = ^TResignalStmt;
      TResignalStmt = packed record
      private type
        TNodes = packed record
          StmtTag: TOffset;
        end;
      private
        Heritage: TStmt;
      private
        Nodes: TNodes;
        class function Create(const AParser: TSQLParser; const ANodes: TNodes): TOffset; static;
      public
        property Parser: TSQLParser read Heritage.Heritage.Heritage.Heritage.FParser;
      end;

      PReturnStmt = ^TReturnStmt;
      TReturnStmt = packed record
      private type
        TNodes = packed record
          StmtTag: TOffset;
          Expr: TOffset;
        end;
      private
        Heritage: TStmt;
      private
        Nodes: TNodes;
        class function Create(const AParser: TSQLParser; const ANodes: TNodes): TOffset; static;
      public
        property Parser: TSQLParser read Heritage.Heritage.Heritage.Heritage.FParser;
      end;

      PRevokeStmt = ^TRevokeStmt;
      TRevokeStmt = packed record
      private type
        TNodes = packed record
          StmtTag: TOffset;
          PrivilegesList: TOffset;
          CommaToken: TOffset;
          GrantOptionTag: TOffset;
          OnTag: TOffset;
          OnUser: TOffset;
          ObjectValue: TOffset;
          FromTag: TOffset;
          UserIdentList: TOffset;
        end;
      private
        Heritage: TStmt;
      private
        Nodes: TNodes;
        class function Create(const AParser: TSQLParser; const ANodes: TNodes): TOffset; static;
      public
        property Parser: TSQLParser read Heritage.Heritage.Heritage.Heritage.FParser;
      end;

      PRollbackStmt = ^TRollbackStmt;
      TRollbackStmt = packed record
      private type
        TNodes = packed record
          RollbackTag: TOffset;
          ToValue: TOffset;
          ChainTag: TOffset;
          ReleaseTag: TOffset;
        end;
      private
        Heritage: TStmt;
      private
        Nodes: TNodes;
        class function Create(const AParser: TSQLParser; const ANodes: TNodes): TOffset; static;
      public
        property Parser: TSQLParser read Heritage.Heritage.Heritage.Heritage.FParser;
      end;

      PRoutineParam = ^TRoutineParam;
      TRoutineParam = packed record
      private type
        TNodes = packed record
          DirektionTag: TOffset;
          IdentToken: TOffset;
          DatatypeNode: TOffset;
        end;
      private
        Heritage: TRange;
      private
        Nodes: TNodes;
        class function Create(const AParser: TSQLParser; const ANodes: TNodes): TOffset; static;
      public
        property Parser: TSQLParser read Heritage.Heritage.Heritage.FParser;
      end;

      PSavepointStmt = ^TSavepointStmt;
      TSavepointStmt = packed record
      private type
        TNodes = packed record
          SavepointTag: TOffset;
          Ident: TOffset;
        end;
      private
        Heritage: TStmt;
      private
        Nodes: TNodes;
        class function Create(const AParser: TSQLParser; const ANodes: TNodes): TOffset; static;
      public
        property Parser: TSQLParser read Heritage.Heritage.Heritage.Heritage.FParser;
      end;

      PSchedule = ^TSchedule;
      TSchedule = packed record
      private type
        TNodes = packed record
          AtValue: TOffset;
          EveryValue: TOffset;
          StartsValue: TOffset;
          EndsValue: TOffset;
        end;
      private
        Heritage: TRange;
      private
        Nodes: TNodes;
        class function Create(const AParser: TSQLParser; const ANodes: TNodes): TOffset; static;
      public
        property Parser: TSQLParser read Heritage.Heritage.Heritage.FParser;
      end;

      PSecretIdent = ^TSecretIdent;
      TSecretIdent = packed record
      private type
        TNodes = packed record
          OpenAngleBracket: TOffset;
          ItemToken: TOffset;
          CloseAngleBracket: TOffset;
        end;
      private
        Heritage: TRange;
      private
        Nodes: TNodes;
        class function Create(const AParser: TSQLParser; const ANodes: TNodes): TOffset; static;
      public
        property Parser: TSQLParser read Heritage.Heritage.Heritage.FParser;
      end;

      PSelectStmt = ^TSelectStmt;
      TSelectStmt = packed record
      private type

        PColumn = ^TColumn;
        TColumn = packed record
        private type
          TNodes = packed record
            Expr: TOffset;
            AsTag: TOffset;
            AliasIdent: TOffset;
          end;
        private
          Heritage: TRange;
        private
          Nodes: TNodes;
          class function Create(const AParser: TSQLParser; const ANodes: TNodes): TOffset; static;
        public
          property Parser: TSQLParser read Heritage.Heritage.Heritage.FParser;
        end;

        PTableFactor = ^TTableFactor;
        TTableFactor = packed record
        private type

          PIndexHint = ^TIndexHint;
          TIndexHint = packed record
          public type
            TNodes = record
              HintTag: TOffset;
              ForTag: TOffset;
              IndexList: TOffset;
            end;
          private
            Heritage: TRange;
          private
            Nodes: TNodes;
            class function Create(const AParser: TSQLParser; const ANodes: TNodes): TOffset; static;
          public
            property Parser: TSQLParser read Heritage.Heritage.Heritage.FParser;
          end;

          TNodes = packed record
            TableIdent: TOffset;
            PartitionTag: TOffset;
            Partitions: TOffset;
            AsTag: TOffset;
            AliasIdent: TOffset;
            IndexHintList: TOffset;
            SelectStmt: TOffset;
          end;

        private
          Heritage: TRange;
        private
          Nodes: TNodes;
          class function Create(const AParser: TSQLParser; const ANodes: TNodes): TOffset; static;
        public
          property Parser: TSQLParser read Heritage.Heritage.Heritage.FParser;
        end;

        PTableJoin = ^TTableJoin;
        TTableJoin = packed record
        private type
          TNodes = packed record
            JoinTag: TOffset;
            RightTable: TOffset;
            OnTag: TOffset;
            Condition: TOffset;
          end;
          TKeywordTokens = array [0..3] of Integer;
        private
          Heritage: TRange;
        private
          FJoinType: TJoinType;
          Nodes: TNodes;
          class function Create(const AParser: TSQLParser; const AJoinType: TJoinType; const ANodes: TNodes): TOffset; static;
        public
          property JoinType: TJoinType read FJoinType;
        end;

        PTableFactorOj = ^TTableFactorOj;
        TTableFactorOj = packed record
        private type
          TNodes = packed record
            OpenBracket: TOffset;
            OjTag: TOffset;
            TableReference: TOffset;
            CloseBracket: TOffset;
          end;
        private
          Heritage: TRange;
        private
          Nodes: TNodes;
          class function Create(const AParser: TSQLParser; const ANodes: TNodes): TOffset; static;
        public
          property Parser: TSQLParser read Heritage.Heritage.Heritage.FParser;
        end;

        PTableFactorSelect = ^TTableFactorSelect;
        TTableFactorSelect = packed record
        private type
          TNodes = packed record
            SelectStmt: TOffset;
            AsTag: TOffset;
            AliasIdent: TOffset;
          end;
        private
          Heritage: TRange;
        private
          Nodes: TNodes;
          class function Create(const AParser: TSQLParser; const ANodes: TNodes): TOffset; static;
        public
          property Parser: TSQLParser read Heritage.Heritage.Heritage.FParser;
        end;

        PGroup = ^TGroup;
        TGroup = packed record
        private type
          TNodes = packed record
            Expr: TOffset;
            DirectionTag: TOffset;
          end;
        private
          Heritage: TRange;
        private
          Nodes: TNodes;
          class function Create(const AParser: TSQLParser; const ANodes: TNodes): TOffset; static;
        public
          property Parser: TSQLParser read Heritage.Heritage.Heritage.FParser;
        end;

        POrder = ^TOrder;
        TOrder = packed record
        private type
          TNodes = packed record
            Expr: TOffset;
            DirectionTag: TOffset;
          end;
        private
          Heritage: TRange;
        private
          Nodes: TNodes;
          class function Create(const AParser: TSQLParser; const ANodes: TNodes): TOffset; static;
        public
          property Parser: TSQLParser read Heritage.Heritage.Heritage.FParser;
        end;

        TIntoNodes = packed record
          Tag: TOffset;
          Filename: TOffset;
          CharacterSetValue: TOffset;
          VariableList: TOffset;
          FieldsTerminatedByValue: TOffset;
          EnclosedByValue: TOffset;
          LinesTerminatedByValue: TOffset;
        end;

        TNodes = packed record
          SelectTag: TOffset;
          DistinctTag: TOffset;
          HighPriorityTag: TOffset;
          MaxStatementTime: TOffset;
          StraightJoinTag: TOffset;
          SQLSmallResultTag: TOffset;
          SQLBigResultTag: TOffset;
          SQLBufferResultTag: TOffset;
          SQLNoCacheTag: TOffset;
          SQLCalcFoundRowsTag: TOffset;
          ColumnsList: TOffset;
          Into1: TIntoNodes;
          From: packed record
            Tag: TOffset;
            TableReferenceList: TOffset;
          end;
          Partition: packed record
            Tag: TOffset;
            Ident: TOffset;
          end;
          Where: packed record
            Tag: TOffset;
            Expr: TOffset;
          end;
          GroupBy: packed record
            Tag: TOffset;
            List: TOffset;
            WithRollupTag: TOffset;
          end;
          Having: packed record
            Tag: TOffset;
            Expr: TOffset;
          end;
          OrderBy: packed record
            Tag: TOffset;
            Expr: TOffset;
          end;
          Limit: packed record
            Tag: TOffset;
            OffsetTag: TOffset;
            OffsetToken: TOffset;
            CommaToken: TOffset;
            RowCountToken: TOffset;
          end;
          Proc: packed record
            Tag: TOffset;
            Ident: TOffset;
            ParamList: TOffset;
          end;
          Into2: TIntoNodes;
          ForUpdatesTag: TOffset;
          LockInShareMode: TOffset;
          Union: packed record
            Tag: TOffset;
            SelectStmt: TOffset;
          end;
        end;
      private
        Heritage: TStmt;
      private
        Nodes: TNodes;
        class function Create(const AParser: TSQLParser; const ANodes: TNodes): TOffset; static;
      public
        property Parser: TSQLParser read Heritage.Heritage.Heritage.Heritage.FParser;
      end;

      PSetStmt = ^TSetStmt;
      TSetStmt = packed record
      private type

        PAssignment = ^TAssignment;
        TAssignment = packed record
        private type
          TNodes = packed record
            ScopeTag: TOffset;
            Variable: TOffset;
            AssignToken: TOffset;
            ValueExpr: TOffset;
          end;
        private
          Heritage: TRange;
        private
          Nodes: TNodes;
          class function Create(const AParser: TSQLParser; const ANodes: TNodes): TOffset; static;
        public
          property Parser: TSQLParser read Heritage.Heritage.Heritage.FParser;
        end;

        TNodes = packed record
          SetTag: TOffset;
          ScopeTag: TOffset;
          AssignmentList: TOffset;
        end;
      private
        Heritage: TStmt;
      private
        Nodes: TNodes;
        class function Create(const AParser: TSQLParser; const ANodes: TNodes): TOffset; static;
      public
        property Parser: TSQLParser read Heritage.Heritage.Heritage.Heritage.FParser;
      end;

      PSetNamesStmt = ^TSetNamesStmt;
      TSetNamesStmt = packed record
      private type
        TNodes = packed record
          StmtTag: TOffset;
          CharsetValue: TOffset;
        end;
      private
        Heritage: TStmt;
      private
        Nodes: TNodes;
        class function Create(const AParser: TSQLParser; const ANodes: TNodes): TOffset; static;
      public
        property Parser: TSQLParser read Heritage.Heritage.Heritage.Heritage.FParser;
      end;

      PSetPasswordStmt = ^TSetPasswordStmt;
      TSetPasswordStmt = packed record
      private type
        TNodes = packed record
          StmtTag: TOffset;
          ForValue: TOffset;
          AssignToken: TOffset;
          PasswordExpr: TOffset;
        end;
      private
        Heritage: TStmt;
      private
        Nodes: TNodes;
        class function Create(const AParser: TSQLParser; const ANodes: TNodes): TOffset; static;
      public
        property Parser: TSQLParser read Heritage.Heritage.Heritage.Heritage.FParser;
      end;

      PSetTransactionStmt = ^TSetTransactionStmt;
      TSetTransactionStmt = packed record
      private type

        PCharacteristic = ^TCharacteristic;
        TCharacteristic = packed record
        private type
          TNodes = packed record
            KindTag: TOffset;
            LevelTag: TOffset;
          end;
        private
          Heritage: TRange;
        private
          Nodes: TNodes;
          class function Create(const AParser: TSQLParser; const ANodes: TNodes): TOffset; static;
        public
          property Parser: TSQLParser read Heritage.Heritage.Heritage.FParser;
        end;

        TNodes = packed record
          SetTag: TOffset;
          ScopeTag: TOffset;
          TransactionTag: TOffset;
          CharacteristicList: TOffset;
        end;
      private
        Heritage: TStmt;
      private
        Nodes: TNodes;
        class function Create(const AParser: TSQLParser; const ANodes: TNodes): TOffset; static;
      public
        property Parser: TSQLParser read Heritage.Heritage.Heritage.Heritage.FParser;
      end;

      PShowAuthorsStmt = ^TShowAuthorsStmt;
      TShowAuthorsStmt = packed record
      private type
        TNodes = packed record
          StmtTag: TOffset;
        end;
      private
        Heritage: TStmt;
      private
        Nodes: TNodes;
        class function Create(const AParser: TSQLParser; const ANodes: TNodes): TOffset; static;
      public
        property Parser: TSQLParser read Heritage.Heritage.Heritage.Heritage.FParser;
      end;

      PShowBinaryLogsStmt = ^TShowBinaryLogsStmt;
      TShowBinaryLogsStmt = packed record
      private type
        TNodes = packed record
          StmtTag: TOffset;
        end;
      private
        Heritage: TStmt;
      private
        Nodes: TNodes;
        class function Create(const AParser: TSQLParser; const ANodes: TNodes): TOffset; static;
      public
        property Parser: TSQLParser read Heritage.Heritage.Heritage.Heritage.FParser;
      end;

      PShowBinlogEventsStmt = ^TShowBinlogEventsStmt;
      TShowBinlogEventsStmt = packed record
      private type
        TNodes = packed record
          StmtTag: TOffset;
          InValue: TOffset;
          FromValue: TOffset;
          Limit: packed record
            Tag: TOffset;
            OffsetToken: TOffset;
            CommaToken: TOffset;
            RowCountToken: TOffset;
          end;
        end;
      private
        Heritage: TStmt;
      private
        Nodes: TNodes;
        class function Create(const AParser: TSQLParser; const ANodes: TNodes): TOffset; static;
      public
        property Parser: TSQLParser read Heritage.Heritage.Heritage.Heritage.FParser;
      end;

      PShowCharacterSetStmt = ^TShowCharacterSetStmt;
      TShowCharacterSetStmt = packed record
      private type
        TNodes = packed record
          StmtTag: TOffset;
          LikeValue: TOffset;
          WhereValue: TOffset;
        end;
      private
        Heritage: TStmt;
      private
        Nodes: TNodes;
        class function Create(const AParser: TSQLParser; const ANodes: TNodes): TOffset; static;
      public
        property Parser: TSQLParser read Heritage.Heritage.Heritage.Heritage.FParser;
      end;

      PShowCollationStmt = ^TShowCollationStmt;
      TShowCollationStmt = packed record
      private type
        TNodes = packed record
          StmtTag: TOffset;
          LikeValue: TOffset;
          WhereValue: TOffset;
        end;
      private
        Heritage: TStmt;
      private
        Nodes: TNodes;
        class function Create(const AParser: TSQLParser; const ANodes: TNodes): TOffset; static;
      public
        property Parser: TSQLParser read Heritage.Heritage.Heritage.Heritage.FParser;
      end;

      PShowColumnsStmt = ^TShowColumnsStmt;
      TShowColumnsStmt = packed record
      private type
        TNodes = packed record
          StmtTag: TOffset;
          FullTag: TOffset;
          ColumnsTag: TOffset;
          FromTableTag: TOffset;
          TableIdent: TOffset;
          FromDatabaseValue: TOffset;
          LikeValue: TOffset;
          WhereValue: TOffset;
        end;
      private
        Heritage: TStmt;
      private
        Nodes: TNodes;
        class function Create(const AParser: TSQLParser; const ANodes: TNodes): TOffset; static;
      public
        property Parser: TSQLParser read Heritage.Heritage.Heritage.Heritage.FParser;
      end;

      PShowContributorsStmt = ^TShowContributorsStmt;
      TShowContributorsStmt = packed record
      private type
        TNodes = packed record
          StmtTag: TOffset;
        end;
      private
        Heritage: TStmt;
      private
        Nodes: TNodes;
        class function Create(const AParser: TSQLParser; const ANodes: TNodes): TOffset; static;
      public
        property Parser: TSQLParser read Heritage.Heritage.Heritage.Heritage.FParser;
      end;

      PShowCountErrorsStmt = ^TShowCountErrorsStmt;
      TShowCountErrorsStmt = packed record
      private type
        TNodes = packed record
          StmtTag: TOffset;
          CountFunctionCall: TOffset;
        end;
      private
        Heritage: TStmt;
      private
        Nodes: TNodes;
        class function Create(const AParser: TSQLParser; const ANodes: TNodes): TOffset; static;
      public
        property Parser: TSQLParser read Heritage.Heritage.Heritage.Heritage.FParser;
      end;

      PShowCountWarningsStmt = ^TShowCountWarningsStmt;
      TShowCountWarningsStmt = packed record
      private type
        TNodes = packed record
          StmtTag: TOffset;
          CountFunctionCall: TOffset;
        end;
      private
        Heritage: TStmt;
      private
        Nodes: TNodes;
        class function Create(const AParser: TSQLParser; const ANodes: TNodes): TOffset; static;
      public
        property Parser: TSQLParser read Heritage.Heritage.Heritage.Heritage.FParser;
      end;

      PShowCreateDatabaseStmt = ^TShowCreateDatabaseStmt;
      TShowCreateDatabaseStmt = packed record
      private type
        TNodes = packed record
          StmtTag: TOffset;
          IfNotExistsTag: TOffset;
          Ident: TOffset;
        end;
      private
        Heritage: TStmt;
      private
        Nodes: TNodes;
        class function Create(const AParser: TSQLParser; const ANodes: TNodes): TOffset; static;
      public
        property Parser: TSQLParser read Heritage.Heritage.Heritage.Heritage.FParser;
      end;

      PShowCreateEventStmt = ^TShowCreateEventStmt;
      TShowCreateEventStmt = packed record
      private type
        TNodes = packed record
          StmtTag: TOffset;
          Ident: TOffset;
        end;
      private
        Heritage: TStmt;
      private
        Nodes: TNodes;
        class function Create(const AParser: TSQLParser; const ANodes: TNodes): TOffset; static;
      public
        property Parser: TSQLParser read Heritage.Heritage.Heritage.Heritage.FParser;
      end;

      PShowCreateFunctionStmt = ^TShowCreateFunctionStmt;
      TShowCreateFunctionStmt = packed record
      private type
        TNodes = packed record
          StmtTag: TOffset;
          Ident: TOffset;
        end;
      private
        Heritage: TStmt;
      private
        Nodes: TNodes;
        class function Create(const AParser: TSQLParser; const ANodes: TNodes): TOffset; static;
      public
        property Parser: TSQLParser read Heritage.Heritage.Heritage.Heritage.FParser;
      end;

      PShowCreateProcedureStmt = ^TShowCreateProcedureStmt;
      TShowCreateProcedureStmt = packed record
      private type
        TNodes = packed record
          StmtTag: TOffset;
          Ident: TOffset;
        end;
      private
        Heritage: TStmt;
      private
        Nodes: TNodes;
        class function Create(const AParser: TSQLParser; const ANodes: TNodes): TOffset; static;
      public
        property Parser: TSQLParser read Heritage.Heritage.Heritage.Heritage.FParser;
      end;

      PShowCreateTableStmt = ^TShowCreateTableStmt;
      TShowCreateTableStmt = packed record
      private type
        TNodes = packed record
          StmtTag: TOffset;
          Ident: TOffset;
        end;
      private
        Heritage: TStmt;
      private
        Nodes: TNodes;
        class function Create(const AParser: TSQLParser; const ANodes: TNodes): TOffset; static;
      public
        property Parser: TSQLParser read Heritage.Heritage.Heritage.Heritage.FParser;
      end;

      PShowCreateTriggerStmt = ^TShowCreateTriggerStmt;
      TShowCreateTriggerStmt = packed record
      private type
        TNodes = packed record
          StmtTag: TOffset;
          Ident: TOffset;
        end;
      private
        Heritage: TStmt;
      private
        Nodes: TNodes;
        class function Create(const AParser: TSQLParser; const ANodes: TNodes): TOffset; static;
      public
        property Parser: TSQLParser read Heritage.Heritage.Heritage.Heritage.FParser;
      end;

      PShowCreateUserStmt = ^TShowCreateUserStmt;
      TShowCreateUserStmt = packed record
      private type
        TNodes = packed record
          StmtTag: TOffset;
          User: TOffset;
        end;
      private
        Heritage: TStmt;
      private
        Nodes: TNodes;
        class function Create(const AParser: TSQLParser; const ANodes: TNodes): TOffset; static;
      public
        property Parser: TSQLParser read Heritage.Heritage.Heritage.Heritage.FParser;
      end;

      PShowCreateViewStmt = ^TShowCreateViewStmt;
      TShowCreateViewStmt = packed record
      private type
        TNodes = packed record
          StmtTag: TOffset;
          Ident: TOffset;
        end;
      private
        Heritage: TStmt;
      private
        Nodes: TNodes;
        class function Create(const AParser: TSQLParser; const ANodes: TNodes): TOffset; static;
      public
        property Parser: TSQLParser read Heritage.Heritage.Heritage.Heritage.FParser;
      end;

      PShowDatabasesStmt = ^TShowDatabasesStmt;
      TShowDatabasesStmt = packed record
      private type
        TNodes = packed record
          StmtTag: TOffset;
          LikeValue: TOffset;
          WhereValue: TOffset;
        end;
      private
        Heritage: TStmt;
      private
        Nodes: TNodes;
        class function Create(const AParser: TSQLParser; const ANodes: TNodes): TOffset; static;
      public
        property Parser: TSQLParser read Heritage.Heritage.Heritage.Heritage.FParser;
      end;

      PShowEngineStmt = ^TShowEngineStmt;
      TShowEngineStmt = packed record
      private type
        TNodes = packed record
          StmtTag: TOffset;
          Ident: TOffset;
          KindTag: TOffset;
        end;
      private
        Heritage: TStmt;
      private
        Nodes: TNodes;
        class function Create(const AParser: TSQLParser; const ANodes: TNodes): TOffset; static;
      public
        property Parser: TSQLParser read Heritage.Heritage.Heritage.Heritage.FParser;
      end;

      PShowEnginesStmt = ^TShowEnginesStmt;
      TShowEnginesStmt = packed record
      private type
        TNodes = packed record
          StmtTag: TOffset;
        end;
      private
        Heritage: TStmt;
      private
        Nodes: TNodes;
        class function Create(const AParser: TSQLParser; const ANodes: TNodes): TOffset; static;
      public
        property Parser: TSQLParser read Heritage.Heritage.Heritage.Heritage.FParser;
      end;

      PShowErrorsStmt = ^TShowErrorsStmt;
      TShowErrorsStmt = packed record
      private type
        TNodes = packed record
          StmtTag: TOffset;
          Limit: record
            Tag: TOffset;
            OffsetToken: TOffset;
            CommaToken: TOffset;
            RowCountToken: TOffset;
          end;
        end;
      private
        Heritage: TStmt;
      private
        Nodes: TNodes;
        class function Create(const AParser: TSQLParser; const ANodes: TNodes): TOffset; static;
      public
        property Parser: TSQLParser read Heritage.Heritage.Heritage.Heritage.FParser;
      end;

      PShowEventsStmt = ^TShowEventsStmt;
      TShowEventsStmt = packed record
      private type
        TNodes = packed record
          StmtTag: TOffset;
          FromValue: TOffset;
          LikeValue: TOffset;
          WhereValue: TOffset;
        end;
      private
        Heritage: TStmt;
      private
        Nodes: TNodes;
        class function Create(const AParser: TSQLParser; const ANodes: TNodes): TOffset; static;
      public
        property Parser: TSQLParser read Heritage.Heritage.Heritage.Heritage.FParser;
      end;

      PShowFunctionCodeStmt = ^TShowFunctionCodeStmt;
      TShowFunctionCodeStmt = packed record
      private type
        TNodes = packed record
          StmtTag: TOffset;
        end;
      private
        Heritage: TStmt;
      private
        Nodes: TNodes;
        class function Create(const AParser: TSQLParser; const ANodes: TNodes): TOffset; static;
      public
        property Parser: TSQLParser read Heritage.Heritage.Heritage.Heritage.FParser;
      end;

      PShowFunctionStatusStmt = ^TShowFunctionStatusStmt;
      TShowFunctionStatusStmt = packed record
      private type
        TNodes = packed record
          StmtTag: TOffset;
        end;
      private
        Heritage: TStmt;
      private
        Nodes: TNodes;
        class function Create(const AParser: TSQLParser; const ANodes: TNodes): TOffset; static;
      public
        property Parser: TSQLParser read Heritage.Heritage.Heritage.Heritage.FParser;
      end;

      PShowGrantsStmt = ^TShowGrantsStmt;
      TShowGrantsStmt = packed record
      private type
        TNodes = packed record
          StmtTag: TOffset;
          ForValue: TOffset;
        end;
      private
        Heritage: TStmt;
      private
        Nodes: TNodes;
        class function Create(const AParser: TSQLParser; const ANodes: TNodes): TOffset; static;
      public
        property Parser: TSQLParser read Heritage.Heritage.Heritage.Heritage.FParser;
      end;

      PShowIndexStmt = ^TShowIndexStmt;
      TShowIndexStmt = packed record
      private type
        TNodes = packed record
          StmtTag: TOffset;
          FromTableValue: TOffset;
          FromDatabaseValue: TOffset;
          WhereValue: TOffset;
        end;
      private
        Heritage: TStmt;
      private
        Nodes: TNodes;
        class function Create(const AParser: TSQLParser; const ANodes: TNodes): TOffset; static;
      public
        property Parser: TSQLParser read Heritage.Heritage.Heritage.Heritage.FParser;
      end;

      PShowMasterStatusStmt = ^TShowMasterStatusStmt;
      TShowMasterStatusStmt = packed record
      private type
        TNodes = packed record
          StmtTag: TOffset;
        end;
      private
        Heritage: TStmt;
      private
        Nodes: TNodes;
        class function Create(const AParser: TSQLParser; const ANodes: TNodes): TOffset; static;
      public
        property Parser: TSQLParser read Heritage.Heritage.Heritage.Heritage.FParser;
      end;

      PShowOpenTablesStmt = ^TShowOpenTablesStmt;
      TShowOpenTablesStmt = packed record
      private type
        TNodes = packed record
          StmtTag: TOffset;
          FromDatabaseValue: TOffset;
          LikeValue: TOffset;
          WhereValue: TOffset;
        end;
      private
        Heritage: TStmt;
      private
        Nodes: TNodes;
        class function Create(const AParser: TSQLParser; const ANodes: TNodes): TOffset; static;
      public
        property Parser: TSQLParser read Heritage.Heritage.Heritage.Heritage.FParser;
      end;

      PShowPluginsStmt = ^TShowPluginsStmt;
      TShowPluginsStmt = packed record
      private type
        TNodes = packed record
          StmtTag: TOffset;
        end;
      private
        Heritage: TStmt;
      private
        Nodes: TNodes;
        class function Create(const AParser: TSQLParser; const ANodes: TNodes): TOffset; static;
      public
        property Parser: TSQLParser read Heritage.Heritage.Heritage.Heritage.FParser;
      end;

      PShowPrivilegesStmt = ^TShowPrivilegesStmt;
      TShowPrivilegesStmt = packed record
      private type
        TNodes = packed record
          StmtTag: TOffset;
        end;
      private
        Heritage: TStmt;
      private
        Nodes: TNodes;
        class function Create(const AParser: TSQLParser; const ANodes: TNodes): TOffset; static;
      public
        property Parser: TSQLParser read Heritage.Heritage.Heritage.Heritage.FParser;
      end;

      PShowProcedureCodeStmt = ^TShowProcedureCodeStmt;
      TShowProcedureCodeStmt = packed record
      private type
        TNodes = packed record
          StmtTag: TOffset;
        end;
      private
        Heritage: TStmt;
      private
        Nodes: TNodes;
        class function Create(const AParser: TSQLParser; const ANodes: TNodes): TOffset; static;
      public
        property Parser: TSQLParser read Heritage.Heritage.Heritage.Heritage.FParser;
      end;

      PShowProcedureStatusStmt = ^TShowProcedureStatusStmt;
      TShowProcedureStatusStmt = packed record
      private type
        TNodes = packed record
          StmtTag: TOffset;
          LikeValue: TOffset;
          WhereValue: TOffset;
        end;
      private
        Heritage: TStmt;
      private
        Nodes: TNodes;
        class function Create(const AParser: TSQLParser; const ANodes: TNodes): TOffset; static;
      public
        property Parser: TSQLParser read Heritage.Heritage.Heritage.Heritage.FParser;
      end;

      PShowProcessListStmt = ^TShowProcessListStmt;
      TShowProcessListStmt = packed record
      private type
        TNodes = packed record
          StmtTag: TOffset;
        end;
      private
        Heritage: TStmt;
      private
        Nodes: TNodes;
        class function Create(const AParser: TSQLParser; const ANodes: TNodes): TOffset; static;
      public
        property Parser: TSQLParser read Heritage.Heritage.Heritage.Heritage.FParser;
      end;

      PShowProfileStmt = ^TShowProfileStmt;
      TShowProfileStmt = packed record
      private type
        TNodes = packed record
          StmtTag: TOffset;
          TypeList: TOffset;
          ForQueryValue: TOffset;
          LimitValue: TOffset;
          OffsetValue: TOffset;
        end;
      private
        Heritage: TStmt;
      private
        Nodes: TNodes;
        class function Create(const AParser: TSQLParser; const ANodes: TNodes): TOffset; static;
      public
        property Parser: TSQLParser read Heritage.Heritage.Heritage.Heritage.FParser;
      end;

      PShowProfilesStmt = ^TShowProfilesStmt;
      TShowProfilesStmt = packed record
      private type
        TNodes = packed record
          StmtTag: TOffset;
        end;
      private
        Heritage: TStmt;
      private
        Nodes: TNodes;
        class function Create(const AParser: TSQLParser; const ANodes: TNodes): TOffset; static;
      public
        property Parser: TSQLParser read Heritage.Heritage.Heritage.Heritage.FParser;
      end;

      PShowRelaylogEventsStmt = ^TShowRelaylogEventsStmt;
      TShowRelaylogEventsStmt = packed record
      private type
        TNodes = packed record
          StmtTag: TOffset;
          InValue: TOffset;
          FromValue: TOffset;
          Limit: packed record
            Tag: TOffset;
            OffsetToken: TOffset;
            CommaToken: TOffset;
            RowCountToken: TOffset;
          end;
        end;
      private
        Heritage: TStmt;
      private
        Nodes: TNodes;
        class function Create(const AParser: TSQLParser; const ANodes: TNodes): TOffset; static;
      public
        property Parser: TSQLParser read Heritage.Heritage.Heritage.Heritage.FParser;
      end;

      PShowSlaveHostsStmt = ^TShowSlaveHostsStmt;
      TShowSlaveHostsStmt = packed record
      private type
        TNodes = packed record
          StmtTag: TOffset;
        end;
      private
        Heritage: TStmt;
      private
        Nodes: TNodes;
        class function Create(const AParser: TSQLParser; const ANodes: TNodes): TOffset; static;
      public
        property Parser: TSQLParser read Heritage.Heritage.Heritage.Heritage.FParser;
      end;

      PShowSlaveStatusStmt = ^TShowSlaveStatusStmt;
      TShowSlaveStatusStmt = packed record
      private type
        TNodes = packed record
          StmtTag: TOffset;
        end;
      private
        Heritage: TStmt;
      private
        Nodes: TNodes;
        class function Create(const AParser: TSQLParser; const ANodes: TNodes): TOffset; static;
      public
        property Parser: TSQLParser read Heritage.Heritage.Heritage.Heritage.FParser;
      end;

      PShowStatusStmt = ^TShowStatusStmt;
      TShowStatusStmt = packed record
      private type
        TNodes = packed record
          ShowTag: TOffset;
          ScopeTag: TOffset;
          StatusTag: TOffset;
          LikeValue: TOffset;
          WhereValue: TOffset;
        end;
      private
        Heritage: TStmt;
      private
        Nodes: TNodes;
        class function Create(const AParser: TSQLParser; const ANodes: TNodes): TOffset; static;
      public
        property Parser: TSQLParser read Heritage.Heritage.Heritage.Heritage.FParser;
      end;

      PShowTableStatusStmt = ^TShowTableStatusStmt;
      TShowTableStatusStmt = packed record
      private type
        TNodes = packed record
          ShowTag: TOffset;
          FromDatabaseValue: TOffset;
          LikeValue: TOffset;
          WhereValue: TOffset;
        end;
      private
        Heritage: TStmt;
      private
        Nodes: TNodes;
        class function Create(const AParser: TSQLParser; const ANodes: TNodes): TOffset; static;
      public
        property Parser: TSQLParser read Heritage.Heritage.Heritage.Heritage.FParser;
      end;

      PShowTablesStmt = ^TShowTablesStmt;
      TShowTablesStmt = packed record
      private type
        TNodes = packed record
          ShowTag: TOffset;
          FullTag: TOffset;
          TablesTag: TOffset;
          FromDatabaseValue: TOffset;
          LikeValue: TOffset;
          WhereValue: TOffset;
        end;
      private
        Heritage: TStmt;
      private
        Nodes: TNodes;
        class function Create(const AParser: TSQLParser; const ANodes: TNodes): TOffset; static;
      public
        property Parser: TSQLParser read Heritage.Heritage.Heritage.Heritage.FParser;
      end;

      PShowTriggersStmt = ^TShowTriggersStmt;
      TShowTriggersStmt = packed record
      private type
        TNodes = packed record
          StmtTag: TOffset;
          FromDatabaseValue: TOffset;
          LikeValue: TOffset;
          WhereValue: TOffset;
        end;
      private
        Heritage: TStmt;
      private
        Nodes: TNodes;
        class function Create(const AParser: TSQLParser; const ANodes: TNodes): TOffset; static;
      public
        property Parser: TSQLParser read Heritage.Heritage.Heritage.Heritage.FParser;
      end;

      PShowVariablesStmt = ^TShowVariablesStmt;
      TShowVariablesStmt = packed record
      private type
        TNodes = packed record
          ShowTag: TOffset;
          ScopeTag: TOffset;
          VariablesTag: TOffset;
          LikeValue: TOffset;
          WhereValue: TOffset;
        end;
      private
        Heritage: TStmt;
      private
        Nodes: TNodes;
        class function Create(const AParser: TSQLParser; const ANodes: TNodes): TOffset; static;
      public
        property Parser: TSQLParser read Heritage.Heritage.Heritage.Heritage.FParser;
      end;

      PShowWarningsStmt = ^TShowWarningsStmt;
      TShowWarningsStmt = packed record
      private type
        TNodes = packed record
          StmtTag: TOffset;
          Limit: record
            Tag: TOffset;
            OffsetToken: TOffset;
            CommaToken: TOffset;
            RowCountToken: TOffset;
          end;
        end;
      private
        Heritage: TStmt;
      private
        Nodes: TNodes;
        class function Create(const AParser: TSQLParser; const ANodes: TNodes): TOffset; static;
      public
        property Parser: TSQLParser read Heritage.Heritage.Heritage.Heritage.FParser;
      end;

      PShutdownStmt = ^TShutdownStmt;
      TShutdownStmt = packed record
      private type
        TNodes = packed record
          StmtTag: TOffset;
        end;
      private
        Heritage: TStmt;
      private
        Nodes: TNodes;
        class function Create(const AParser: TSQLParser; const ANodes: TNodes): TOffset; static;
      public
        property Parser: TSQLParser read Heritage.Heritage.Heritage.Heritage.FParser;
      end;

      PSignalStmt = ^TSignalStmt;
      TSignalStmt = packed record
      private type

        PInformation = ^TInformation;
        TInformation = packed record
        private type
          TNodes = packed record
            Value: TOffset;
          end;
        private
          Heritage: TRange;
        private
          Nodes: TNodes;
          class function Create(const AParser: TSQLParser; const ANodes: TNodes): TOffset; static;
        public
          property Parser: TSQLParser read Heritage.Heritage.Heritage.FParser;
        end;

        TNodes = packed record
          StmtTag: TOffset;
          Condition: TOffset;
          SetTag: TOffset;
          InformationList: TOffset;
        end;
      private
        Heritage: TStmt;
      private
        Nodes: TNodes;
        class function Create(const AParser: TSQLParser; const ANodes: TNodes): TOffset; static;
      public
        property Parser: TSQLParser read Heritage.Heritage.Heritage.Heritage.FParser;
      end;

      PSubquery = ^TSubquery;
      TSubquery = packed record
      private type
        TNodes = packed record
          IdentTag: TOffset;
          OpenToken: TOffset;
          Subquery: TOffset;
          CloseToken: TOffset;
        end;
      private
        Heritage: TRange;
      private
        Nodes: TNodes;
        class function Create(const AParser: TSQLParser; const AIdentTag, ASubquery: TOffset): TOffset; overload; static;
        class function Create(const AParser: TSQLParser; const ANodes: TNodes): TOffset; overload; static;
      public
        property Parser: TSQLParser read Heritage.Heritage.Heritage.FParser;
      end;

      PSubstringFunc = ^TSubstringFunc;
      TSubstringFunc = packed record
      private type
        TNodes = packed record
          IdentToken: TOffset;
          OpenBracket: TOffset;
          Str: TOffset;
          FromTag: TOffset;
          Pos: TOffset;
          ForTag: TOffset;
          Len: TOffset;
          CloseBracket: TOffset;
        end;
      private
        Heritage: TRange;
      private
        Nodes: TNodes;
        class function Create(const AParser: TSQLParser; const ANodes: TNodes): TOffset; static;
      public
        property Parser: TSQLParser read Heritage.Heritage.Heritage.FParser;
      end;

      PSoundsLikeOp = ^TSoundsLikeOp;
      TSoundsLikeOp = packed record
      private type
        TNodes = packed record
          Operand1: TOffset;
          Sounds: TOffset;
          Like: TOffset;
          Operand2: TOffset;
        end;
      private
        Heritage: TRange;
      private
        Nodes: TNodes;
        class function Create(const AParser: TSQLParser; const AOperand1, ASounds, ALike, AOperand2: TOffset): TOffset; static;
      public
        property Parser: TSQLParser read Heritage.Heritage.Heritage.FParser;
      end;

      PStartSlaveStmt = ^TStartSlaveStmt;
      TStartSlaveStmt = packed record
      private type
        TNodes = packed record
          StmtTag: TOffset;
        end;
      private
        Heritage: TStmt;
      private
        Nodes: TNodes;
        class function Create(const AParser: TSQLParser; const ANodes: TNodes): TOffset; static;
      public
        property Parser: TSQLParser read Heritage.Heritage.Heritage.Heritage.FParser;
      end;

      PStartTransactionStmt = ^TStartTransactionStmt;
      TStartTransactionStmt = packed record
      private type
        TNodes = packed record
          StartTransactionTag: TOffset;
          RealOnlyTag: TOffset;
          ReadWriteTag: TOffset;
          WithConsistentSnapshotTag: TOffset;
        end;
      private
        Heritage: TStmt;
      private
        Nodes: TNodes;
        class function Create(const AParser: TSQLParser; const ANodes: TNodes): TOffset; static;
      public
        property Parser: TSQLParser read Heritage.Heritage.Heritage.Heritage.FParser;
      end;

      PStopSlaveStmt = ^TStopSlaveStmt;
      TStopSlaveStmt = packed record
      private type
        TNodes = packed record
          StmtTag: TOffset;
        end;
      private
        Heritage: TStmt;
      private
        Nodes: TNodes;
        class function Create(const AParser: TSQLParser; const ANodes: TNodes): TOffset; static;
      public
        property Parser: TSQLParser read Heritage.Heritage.Heritage.Heritage.FParser;
      end;

      PSubArea = ^TSubArea;
      TSubArea = packed record
      private type
        TNodes = packed record
          OpenBracket: TOffset;
          AreaNode: TOffset;
          CloseBracket: TOffset;
        end;
      private
        Heritage: TRange;
      private
        Nodes: TNodes;
        class function Create(const AParser: TSQLParser; const ANodes: TNodes): TOffset; static;
      public
        property Parser: TSQLParser read Heritage.Heritage.Heritage.FParser;
      end;

      PSubAreaSelectStmt = ^TSubAreaSelectStmt;
      TSubAreaSelectStmt = packed record
      private type
        TNodes = packed record
          SelectStmt1: TOffset;
          UnionTag: TOffset;
          HowTag: TOffset;
          SelectStmt2: TOffset;
        end;
      private
        Heritage: TStmt;
      private
        Nodes: TNodes;
        class function Create(const AParser: TSQLParser; const ANodes: TNodes): TOffset; static;
      public
        property Parser: TSQLParser read Heritage.Heritage.Heritage.Heritage.FParser;
      end;

      PSubPartition = ^TSubPartition;
      TSubPartition = packed record
      private type
        TNodes = packed record
          SubPartitionTag: TOffset;
          NameIdent: TOffset;
          EngineValue: TOffset;
          CommentValue: TOffset;
          DataDirectoryValue: TOffset;
          IndexDirectoryValue: TOffset;
          MaxRowsValue: TOffset;
          MinRowsValue: TOffset;
          TablespaceValue: TOffset;
        end;
      private
        Heritage: TRange;
      private
        Nodes: TNodes;
        class function Create(const AParser: TSQLParser; const ANodes: TNodes): TOffset; static;
      public
        property Parser: TSQLParser read Heritage.Heritage.Heritage.FParser;
      end;

      PTag = ^TTag;
      TTag = packed record
      private type
        TNodes = packed record
          KeywordToken1: TOffset;
          KeywordToken2: TOffset;
          KeywordToken3: TOffset;
          KeywordToken4: TOffset;
          KeywordToken5: TOffset;
          KeywordToken6: TOffset;
          KeywordToken7: TOffset;
        end;
      private
        Heritage: TRange;
      private
        Nodes: TNodes;
        class function Create(const AParser: TSQLParser; const ANodes: TNodes): TOffset; static;
      public
        property Parser: TSQLParser read Heritage.Heritage.Heritage.FParser;
      end;

      PTruncateStmt = ^TTruncateStmt;
      TTruncateStmt = packed record
      private type
        TNodes = packed record
          StmtTag: TOffset;
          TableTag: TOffset;
          TableIdent: TOffset;
        end;
      private
        Heritage: TStmt;
      private
        Nodes: TNodes;
        class function Create(const AParser: TSQLParser; const ANodes: TNodes): TOffset; static;
      public
        property Parser: TSQLParser read Heritage.Heritage.Heritage.Heritage.FParser;
      end;

      PTrimFunc = ^TTrimFunc;
      TTrimFunc = packed record
      private type
        TNodes = packed record
          IdentToken: TOffset;
          OpenBracket: TOffset;
          DirectionTag: TOffset;
          RemoveStr: TOffset;
          FromTag: TOffset;
          Str: TOffset;
          CloseBracket: TOffset;
        end;
      private
        Heritage: TRange;
      private
        Nodes: TNodes;
        class function Create(const AParser: TSQLParser; const ANodes: TNodes): TOffset; static;
      public
        property Parser: TSQLParser read Heritage.Heritage.Heritage.FParser;
      end;

      PUnaryOp = ^TUnaryOp;
      TUnaryOp = packed record
      private type
        TNodes = packed record
          Operator: TOffset;
          Operand: TOffset;
        end;
      private
        Heritage: TRange;
      private
        Nodes: TNodes;
        class function Create(const AParser: TSQLParser; const AOperator, AOperand: TOffset): TOffset; static;
      public
        property Parser: TSQLParser read Heritage.Heritage.Heritage.FParser;
      end;

      PUnknownStmt = ^TUnknownStmt;
      TUnknownStmt = packed record
      private
        Heritage: TStmt;
      private
        class function Create(const AParser: TSQLParser; const ATokenCount: Integer; const ATokens: array of TOffset): TOffset; static;
      public
        property Parser: TSQLParser read Heritage.Heritage.Heritage.Heritage.FParser;
      end;

      PUnlockTablesStmt = ^TUnlockTablesStmt;
      TUnlockTablesStmt = packed record
      private type
        TNodes = packed record
          UnlockTablesTag: TOffset;
        end;
      private
        Heritage: TStmt;
      private
        Nodes: TNodes;
        class function Create(const AParser: TSQLParser; const ANodes: TNodes): TOffset; static;
      public
        property Parser: TSQLParser read Heritage.Heritage.Heritage.Heritage.FParser;
      end;

      PUpdateStmt = ^TUpdateStmt;
      TUpdateStmt = packed record
      private type
        TNodes = packed record
          UpdateTag: TOffset;
          PriorityTag: TOffset;
          TableReferenceList: TOffset;
          Set_: packed record
            Tag: TOffset;
            Pairs: TOffset;
          end;
          Where: packed record
            Tag: TOffset;
            Expr: TOffset;
          end;
          OrderBy: packed record
            Tag: TOffset;
            Expr: TOffset;
          end;
          Limit: packed record
            Tag: TOffset;
            Expr: TOffset;
          end;
        end;
      private
        Heritage: TStmt;
      private
        Nodes: TNodes;
        class function Create(const AParser: TSQLParser; const ANodes: TNodes): TOffset; static;
      public
        property Parser: TSQLParser read Heritage.Heritage.Heritage.Heritage.FParser;
      end;

      PUser = ^TUser;
      TUser = packed record
      private type
        TNodes = packed record
          NameToken: TOffset;
          AtToken: TOffset;
          HostToken: TOffset;
        end;
      private
        Heritage: TRange;
      private
        Nodes: TNodes;
        class function Create(const AParser: TSQLParser; const ANodes: TNodes): TOffset; static;
      public
        property Parser: TSQLParser read Heritage.Heritage.Heritage.FParser;
      end;

      PUseStmt = ^TUseStmt;
      TUseStmt = packed record
      private type
        TNodes = packed record
          StmtToken: TOffset;
          Ident: TOffset;
        end;
      private
        Heritage: TStmt;
      private
        Nodes: TNodes;
        class function Create(const AParser: TSQLParser; const ANodes: TNodes): TOffset; static;
      public
        property Parser: TSQLParser read Heritage.Heritage.Heritage.Heritage.FParser;
      end;

      PValue = ^TValue;
      TValue = packed record
      private type
        TNodes = packed record
          IdentTag: TOffset;
          AssignToken: TOffset;
          Expr: TOffset;
        end;
      private
        Heritage: TRange;
      private
        Nodes: TNodes;
        class function Create(const AParser: TSQLParser; const ANodes: TNodes): TOffset; static;
      public
        property Parser: TSQLParser read Heritage.Heritage.Heritage.FParser;
      end;

      PVariable = ^TVariable;
      TVariable = packed record
      private type
        TNodes = packed record
          At1Token: TOffset;
          At2Token: TOffset;
          ScopeTag: TOffset;
          ScopeDotToken: TOffset;
          Ident: TOffset;
        end;
      private
        Heritage: TRange;
      private
        Nodes: TNodes;
        class function Create(const AParser: TSQLParser; const ANodes: TNodes): TOffset; static;
      public
        property Parser: TSQLParser read Heritage.Heritage.Heritage.FParser;
      end;

      PWeightStringFunc = ^TWeightStringFunc;
      TWeightStringFunc = packed record
      private type

        PLevel = ^TLevel;
        TLevel = packed record
        private type
          TNodes = packed record
            CountInt: TOffset;
            DirectionTag: TOffset;
            ReverseTag: TOffset;
          end;
        private
          Heritage: TRange;
        private
          Nodes: TNodes;
          class function Create(const AParser: TSQLParser; const ANodes: TNodes): TOffset; static;
        public
          property Parser: TSQLParser read Heritage.Heritage.Heritage.FParser;
        end;

        TNodes = packed record
          IdentToken: TOffset;
          OpenBracket: TOffset;
          Str: TOffset;
          AsTag: TOffset;
          Datatype: TOffset;
          LevelTag: TOffset;
          LevelList: TOffset;
          CloseBracket: TOffset;
        end;
      private
        Heritage: TRange;
      private
        Nodes: TNodes;
        class function Create(const AParser: TSQLParser; const ANodes: TNodes): TOffset; static;
      public
        property Parser: TSQLParser read Heritage.Heritage.Heritage.FParser;
      end;

      PWhileStmt = ^TWhileStmt;
      TWhileStmt = packed record
      private type
        TNodes = packed record
          BeginLabel: TOffset;
          WhileTag: TOffset;
          SearchConditionExpr: TOffset;
          DoTag: TOffset;
          StmtList: TOffset;
          EndTag: TOffset;
          EndLabel: TOffset;
        end;
      private
        Heritage: TStmt;
      private
        Nodes: TNodes;
        class function Create(const AParser: TSQLParser; const ANodes: TNodes): TOffset; static;
      public
        property Parser: TSQLParser read Heritage.Heritage.Heritage.Heritage.FParser;
      end;

      PXAStmt = ^TXAStmt;
      TXAStmt = packed record
      private type

        PID = ^TID;
        TID = packed record
        private type
          TNodes = packed record
            GTrId: TOffset;
            Comma1: TOffset;
            BQual: TOffset;
            Comma2: TOffset;
            FormatId: TOffset;
          end;
        private
          Heritage: TRange;
        private
          Nodes: TNodes;
          class function Create(const AParser: TSQLParser; const ANodes: TNodes): TOffset; static;
        public
          property Parser: TSQLParser read Heritage.Heritage.Heritage.FParser;
        end;

        TNodes = packed record
          XATag: TOffset;
          ActionTag: TOffset;
          Ident: TOffset;
          RestTag: TOffset;
        end;
      private
        Heritage: TStmt;
      private
        Nodes: TNodes;
        class function Create(const AParser: TSQLParser; const ANodes: TNodes): TOffset; static;
      public
        property Parser: TSQLParser read Heritage.Heritage.Heritage.Heritage.FParser;
      end;

  protected
    diBIGINT,
    diBINARY,
    diBIT,
    diBLOB,
    diBOOL,
    diBOOLEAN,
    diCHAR,
    diCHARACTER,
    diDEC,
    diDECIMAL,
    diDATE,
    diDATETIME,
    diDOUBLE,
    diENUM,
    diFLOAT,
    diGEOMETRY,
    diGEOMETRYCOLLECTION,
    diINT,
    diINT4,
    diJSON,
    diINTEGER,
    diLARGEINT,
    diLINESTRING,
    diLONG,
    diLONGBLOB,
    diLONGTEXT,
    diMEDIUMBLOB,
    diMEDIUMINT,
    diMEDIUMTEXT,
    diMULTILINESTRING,
    diMULTIPOINT,
    diMULTIPOLYGON,
    diNUMBER,
    diNUMERIC,
    diNCHAR,
    diNVARCHAR,
    diPOINT,
    diPOLYGON,
    diREAL,
    diSERIAL,
    diSET,
    diSIGNED,
    diSMALLINT,
    diTEXT,
    diTIME,
    diTIMESTAMP,
    diTINYBLOB,
    diTINYINT,
    diTINYTEXT,
    diUNSIGNED,
    diVARBINARY,
    diVARCHAR,
    diYEAR: Integer;
  
    kiACCOUNT,
    kiACTION,
    kiADD,
    kiAFTER,
    kiAGAINST,
    kiALGORITHM,
    kiALL,
    kiALTER,
    kiALWAYS,
    kiANALYZE,
    kiAND,
    kiANY,
    kiAS,
    kiASC,
    kiASCII,
    kiAT,
    kiAUTO_INCREMENT,
    kiAUTHORS,
    kiAVG_ROW_LENGTH,
    kiBEFORE,
    kiBEGIN,
    kiBETWEEN,
    kiBINARY,
    kiBINLOG,
    kiBLOCK,
    kiBOOLEAN,
    kiBOTH,
    kiBTREE,
    kiBY,
    kiCACHE,
    kiCALL,
    kiCASCADE,
    kiCASCADED,
    kiCASE,
    kiCATALOG_NAME,
    kiCHANGE,
    kiCHANGED,
    kiCHANNEL,
    kiCHAIN,
    kiCHARACTER,
    kiCHARSET,
    kiCHECK,
    kiCHECKSUM,
    kiCLASS_ORIGIN,
    kiCLIENT,
    kiCLOSE,
    kiCOALESCE,
    kiCODE,
    kiCOLLATE,
    kiCOLLATION,
    kiCOLUMN,
    kiCOLUMN_FORMAT,
    kiCOLUMN_NAME,
    kiCOLUMNS,
    kiCOMMENT,
    kiCOMMIT,
    kiCOMMITTED,
    kiCOMPACT,
    kiCOMPLETION,
    kiCOMPRESS,
    kiCOMPRESSED,
    kiCONCURRENT,
    kiCONDITION,
    kiCONNECTION,
    kiCONSISTENT,
    kiCONSTRAINT,
    kiCONSTRAINT_CATALOG,
    kiCONSTRAINT_NAME,
    kiCONSTRAINT_SCHEMA,
    kiCONTAINS,
    kiCONTEXT,
    kiCONTINUE,
    kiCONTRIBUTORS,
    kiCONVERT,
    kiCOPY,
    kiCPU,
    kiCREATE,
    kiCROSS,
    kiCURRENT,
    kiCURRENT_DATE,
    kiCURRENT_TIME,
    kiCURRENT_TIMESTAMP,
    kiCURRENT_USER,
    kiCURSOR,
    kiCURSOR_NAME,
    kiDATA,
    kiDATABASE,
    kiDATABASES,
    kiDATAFILE,
    kiDAY,
    kiDAY_HOUR,
    kiDAY_MICROSECOND,
    kiDAY_MINUTE,
    kiDAY_SECOND,
    kiDEALLOCATE,
    kiDECLARE,
    kiDEFAULT,
    kiDEFINER,
    kiDELAY_KEY_WRITE,
    kiDELAYED,
    kiDELETE,
    kiDESC,
    kiDESCRIBE,
    kiDETERMINISTIC,
    kiDIAGNOSTICS,
    kiDIRECTORY,
    kiDISABLE,
    kiDISCARD,
    kiDISK,
    kiDISTINCT,
    kiDISTINCTROW,
    kiDIV,
    kiDO,
    kiDROP,
    kiDUMPFILE,
    kiDUPLICATE,
    kiDYNAMIC,
    kiEACH,
    kiELSE,
    kiELSEIF,
    kiENABLE,
    kiENCLOSED,
    kiEND,
    kiENDS,
    kiENGINE,
    kiENGINES,
    kiERRORS,
    kiESCAPE,
    kiESCAPED,
    kiEVENT,
    kiEVENTS,
    kiEVERY,
    kiEXCHANGE,
    kiEXCLUSIVE,
    kiEXECUTE,
    kiEXISTS,
    kiEXPANSION,
    kiEXPIRE,
    kiEXPLAIN,
    kiEXIT,
    kiEXTENDED,
    kiFALSE,
    kiFAST,
    kiFAULTS,
    kiFETCH,
    kiFILE_BLOCK_SIZE,
    kiFLUSH,
    kiFIELDS,
    kiFILE,
    kiFIRST,
    kiFIXED,
    kiFOLLOWS,
    kiFOR,
    kiFORCE,
    kiFORMAT,
    kiFOREIGN,
    kiFOUND,
    kiFROM,
    kiFULL,
    kiFULLTEXT,
    kiFUNCTION,
    kiGENERATED,
    kiGET,
    kiGLOBAL,
    kiGRANT,
    kiGRANTS,
    kiGROUP,
    kiHANDLER,
    kiHASH,
    kiHAVING,
    kiHELP,
    kiHIGH_PRIORITY,
    kiHOST,
    kiHOSTS,
    kiHOUR,
    kiHOUR_MICROSECOND,
    kiHOUR_MINUTE,
    kiHOUR_SECOND,
    kiIDENTIFIED,
    kiIF,
    kiIGNORE,
    kiIGNORE_SERVER_IDS,
    kiIMPORT,
    kiIN,
    kiINDEX,
    kiINDEXES,
    kiINFILE,
    kiINITIAL_SIZE,
    kiINNER,
    kiINNODB,
    kiINOUT,
    kiINPLACE,
    kiINSTANCE,
    kiINSERT,
    kiINSERT_METHOD,
    kiINTERVAL,
    kiINTO,
    kiINVOKER,
    kiIO,
    kiIPC,
    kiIS,
    kiISOLATION,
    kiITERATE,
    kiJOIN,
    kiJSON,
    kiKEY,
    kiKEY_BLOCK_SIZE,
    kiKEYS,
    kiKILL,
    kiLANGUAGE,
    kiLAST,
    kiLEADING,
    kiLEAVE,
    kiLEFT,
    kiLESS,
    kiLEVEL,
    kiLIKE,
    kiLIMIT,
    kiLINEAR,
    kiLINES,
    kiLIST,
    kiLOAD,
    kiLOCAL,
    kiLOCALTIME,
    kiLOCALTIMESTAMP,
    kiLOCK,
    kiLOGS,
    kiLOOP,
    kiLOW_PRIORITY,
    kiMASTER,
    kiMASTER_AUTO_POSITION,
    kiMASTER_BIND,
    kiMASTER_CONNECT_RETRY,
    kiMASTER_DELAY,
    kiMASTER_HEARTBEAT_PERIOD,
    kiMASTER_HOST,
    kiMASTER_LOG_FILE,
    kiMASTER_LOG_POS,
    kiMASTER_PASSWORD,
    kiMASTER_PORT,
    kiMASTER_RETRY_COUNT,
    kiMASTER_SSL,
    kiMASTER_SSL_CA,
    kiMASTER_SSL_CAPATH,
    kiMASTER_SSL_CERT,
    kiMASTER_SSL_CIPHER,
    kiMASTER_SSL_CRL,
    kiMASTER_SSL_CRLPATH,
    kiMASTER_SSL_KEY,
    kiMASTER_SSL_VERIFY_SERVER_CERT,
    kiMASTER_TLS_VERSION,
    kiMASTER_USER,
    kiMATCH,
    kiMAX_QUERIES_PER_HOUR,
    kiMAX_ROWS,
    kiMAX_CONNECTIONS_PER_HOUR,
    kiMAX_STATEMENT_TIME,
    kiMAX_UPDATES_PER_HOUR,
    kiMAX_USER_CONNECTIONS,
    kiMAXVALUE,
    kiMEDIUM,
    kiMEMORY,
    kiMERGE,
    kiMESSAGE_TEXT,
    kiMICROSECOND,
    kiMIGRATE,
    kiMIN_ROWS,
    kiMINUTE,
    kiMINUTE_MICROSECOND,
    kiMINUTE_SECOND,
    kiMOD,
    kiMODE,
    kiMODIFIES,
    kiMODIFY,
    kiMONTH,
    kiMUTEX,
    kiMYSQL_ERRNO,
    kiNAME,
    kiNAMES,
    kiNATIONAL,
    kiNATURAL,
    kiNEVER,
    kiNEXT,
    kiNO,
    kiNONE,
    kiNOT,
    kiNULL,
    kiNO_WRITE_TO_BINLOG,
    kiNUMBER,
    kiOFFSET,
    kiOJ,
    kiON,
    kiONE,
    kiONLY,
    kiOPEN,
    kiOPTIMIZE,
    kiOPTION,
    kiOPTIONALLY,
    kiOPTIONS,
    kiOR,
    kiORDER,
    kiOUT,
    kiOUTER,
    kiOUTFILE,
    kiOWNER,
    kiPACK_KEYS,
    kiPAGE,
    kiPAGE_CHECKSUM,
    kiPARSER,
    kiPARTIAL,
    kiPARTITION,
    kiPARTITIONING,
    kiPARTITIONS,
    kiPASSWORD,
    kiPERSISTENT,
    kiPHASE,
    kiPLUGINS,
    kiPORT,
    kiPRECEDES,
    kiPREPARE,
    kiPRESERVE,
    kiPRIMARY,
    kiPRIVILEGES,
    kiPROCEDURE,
    kiPROCESS,
    kiPROCESSLIST,
    kiPROFILE,
    kiPROFILES,
    kiPROXY,
    kiPURGE,
    kiQUARTER,
    kiQUERY,
    kiQUICK,
    kiRANGE,
    kiREAD,
    kiREADS,
    kiREBUILD,
    kiRECOVER,
    kiREDUNDANT,
    kiREFERENCES,
    kiREGEXP,
    kiRELAYLOG,
    kiRELEASE,
    kiRELAY_LOG_FILE,
    kiRELAY_LOG_POS,
    kiRELOAD,
    kiREMOVE,
    kiRENAME,
    kiREORGANIZE,
    kiREPAIR,
    kiREPEAT,
    kiREPEATABLE,
    kiREPLACE,
    kiREPLICATION,
    kiREQUIRE,
    kiRESET,
    kiRESIGNAL,
    kiRESTRICT,
    kiRESUME,
    kiRETURN,
    kiRETURNED_SQLSTATE,
    kiRETURNS,
    kiREVERSE,
    kiREVOKE,
    kiRIGHT,
    kiRLIKE,
    kiROLLBACK,
    kiROLLUP,
    kiROTATE,
    kiROUTINE,
    kiROW,
    kiROW_COUNT,
    kiROW_FORMAT,
    kiROWS,
    kiSAVEPOINT,
    kiSCHEDULE,
    kiSCHEMA,
    kiSCHEMA_NAME,
    kiSECOND,
    kiSECOND_MICROSECOND,
    kiSECURITY,
    kiSELECT,
    kiSEPARATOR,
    kiSERIALIZABLE,
    kiSERVER,
    kiSESSION,
    kiSET,
    kiSHARE,
    kiSHARED,
    kiSHOW,
    kiSHUTDOWN,
    kiSIGNAL,
    kiSIGNED,
    kiSIMPLE,
    kiSLAVE,
    kiSNAPSHOT,
    kiSOCKET,
    kiSOME,
    kiSONAME,
    kiSOUNDS,
    kiSOURCE,
    kiSPATIAL,
    kiSQL,
    kiSQL_BIG_RESULT,
    kiSQL_BUFFER_RESULT,
    kiSQL_CACHE,
    kiSQL_CALC_FOUND_ROWS,
    kiSQL_NO_CACHE,
    kiSQL_SMALL_RESULT,
    kiSQLEXCEPTION,
    kiSQLSTATE,
    kiSQLWARNINGS,
    kiSTACKED,
    kiSTARTING,
    kiSTART,
    kiSTARTS,
    kiSTATS_AUTO_RECALC,
    kiSTATS_PERSISTENT,
    kiSTATUS,
    kiSTOP,
    kiSTORAGE,
    kiSTORED,
    kiSTRAIGHT_JOIN,
    kiSUBCLASS_ORIGIN,
    kiSUBPARTITION,
    kiSUBPARTITIONS,
    kiSUPER,
    kiSUSPEND,
    kiSWAPS,
    kiSWITCHES,
    kiTABLE,
    kiTABLE_NAME,
    kiTABLES,
    kiTABLESPACE,
    kiTEMPORARY,
    kiTEMPTABLE,
    kiTERMINATED,
    kiTHAN,
    kiTHEN,
    kiTO,
    kiTRADITIONAL,
    kiTRAILING,
    kiTRANSACTION,
    kiTRANSACTIONAL,
    kiTRIGGER,
    kiTRIGGERS,
    kiTRUE,
    kiTRUNCATE,
    kiTYPE,
    kiUNCOMMITTED,
    kiUNDEFINED,
    kiUNDO,
    kiUNICODE,
    kiUNION,
    kiUNIQUE,
    kiUNKNOWN,
    kiUNLOCK,
    kiUNSIGNED,
    kiUNTIL,
    kiUPDATE,
    kiUPGRADE,
    kiUSAGE,
    kiUSE,
    kiUSE_FRM,
    kiUSER,
    kiUSING,
    kiVALIDATION,
    kiVALUE,
    kiVALUES,
    kiVARIABLES,
    kiVIEW,
    kiVIRTUAL,
    kiWAIT,
    kiWARNINGS,
    kiWEEK,
    kiWHEN,
    kiWHERE,
    kiWHILE,
    kiWRAPPER,
    kiWRITE,
    kiWITH,
    kiWITHOUT,
    kiWORK,
    kiXA,
    kiXID,
    kiXML,
    kiXOR,
    kiYEAR,
    kiYEAR_MONTH,
    kiZEROFILL: Integer;

  private type
    TSpacer = (sNone, sSpace, sReturn);
  private
    AllowedMySQLVersion: Integer;
    Commands: TFormatBuffer;
    CommentsWritten: Boolean;
    FCompletionList: TCompletionList;
    FCurrentToken: TOffset; // Cache for speeding
    FErrorCode: Byte;
    FErrorLine: Integer;
    FErrorToken: TOffset;
    FInPL_SQL: Integer;
    LastTokenAll: TOffset;
    FPreviousToken: TOffset;
    OperatorTypeByKeywordIndex: array of TOperatorType;
    FUseCompletionList: Boolean;
    function GetDatatypes(): string;
    function GetError(): Boolean; {$IFNDEF Debug} inline; {$ENDIF}
    function GetFunctions(): string;
    function GetKeywords(): string;
    function GetNextToken(Index: Integer): TOffset; {$IFNDEF Debug} inline; {$ENDIF}
    function GetParsedToken(const Index: Integer): TOffset;
    function GetInPL_SQL(): Boolean; {$IFNDEF Debug} inline; {$ENDIF}
    function GetRoot(): PRoot; {$IFNDEF Debug} inline; {$ENDIF}
    procedure GetText(const Offset: TOffset; out Text: PChar; out Length: Integer); {$IFNDEF Debug} inline; {$ENDIF}
    procedure SaveToDebugHTMLFile(const Filename: string);
    procedure SaveToFormatedSQLFile(const Filename: string);
    procedure SaveToSQLFile(const Filename: string);
    procedure SetDatatypes(ADatatypes: string);
    procedure SetFunctions(AFunctions: string);
    procedure SetKeywords(AKeywords: string);
    property CurrentToken: TOffset read FCurrentToken;
    property Error: Boolean read GetError;
    property InPL_SQL: Boolean read GetInPL_SQL;
    property NextToken[Index: Integer]: TOffset read GetNextToken;
    property PreviousToken: TOffset read FPreviousToken;

  protected
    DatatypeList: TWordList;
    FAnsiQuotes: Boolean;
    FunctionList: TWordList;
    InCreateFunctionStmt: Boolean;
    InCreateProcedureStmt: Boolean;
    KeywordList: TWordList;
    FMySQLVersion: Integer;
    Nodes: record
      Mem: PAnsiChar;
      UsedSize: Integer;
      MemSize: Integer;
    end;
    Parse: record
      Pos: PChar;
      Length: Integer;
    end;
    ParsedText: string;
    FRoot: TOffset;
    Texts: record
      Mem: PAnsiChar;
      UsedSize: Integer;
      MemSize: Integer;
    end;
    TokenBuffer: record
      Count: Integer;
      Tokens: array [0 .. 50 - 1] of TOffset;
    end;
    TokenIndex: Integer;

    function ApplyCurrentToken(): TOffset; overload;
    function ApplyCurrentToken(const AUsageType: TUsageType): TOffset; overload;
    procedure BeginPL_SQL(); {$IFNDEF Debug} inline; {$ENDIF}
    function ChildPtr(const ANode: TOffset): PChild; {$IFNDEF Debug} inline; {$ENDIF}
    function EndOfStmt(const Token: PToken): Boolean; overload; {$IFNDEF Debug} inline; {$ENDIF}
    function EndOfStmt(const Token: TOffset): Boolean; overload; {$IFNDEF Debug} inline; {$ENDIF}
    procedure EndPL_SQL(); {$IFNDEF Debug} inline; {$ENDIF}
    procedure FormatAlterDatabaseStmt(const Nodes: TAlterDatabaseStmt.TNodes);
    procedure FormatAlterEventStmt(const Nodes: TAlterEventStmt.TNodes);
    procedure FormatAlterRoutineStmt(const Nodes: TAlterRoutineStmt.TNodes);
    procedure FormatAlterTablespaceStmt(const Nodes: TAlterTablespaceStmt.TNodes);
    procedure FormatAlterTableStmt(const Nodes: TAlterTableStmt.TNodes);
    procedure FormatAlterViewStmt(const Nodes: TAlterViewStmt.TNodes);
    procedure FormatBeginLabel(const Nodes: TBeginLabel.TNodes);
    procedure FormatCallStmt(const Nodes: TCallStmt.TNodes);
    procedure FormatCaseOp(const Nodes: TCaseOp.TNodes);
    procedure FormatCaseStmt(const Nodes: TCaseStmt.TNodes);
    procedure FormatCaseStmtBranch(const Nodes: TCaseStmt.TBranch.TNodes);
    procedure FormatCastFunc(const Nodes: TCastFunc.TNodes);
    procedure FormatCharFunc(const Nodes: TCharFunc.TNodes);
    procedure FormatChangeMasterStmt(const Nodes: TChangeMasterStmt.TNodes);
    procedure FormatCompoundStmt(const Nodes: TCompoundStmt.TNodes);
    procedure FormatConvertFunc(const Nodes: TConvertFunc.TNodes);
    procedure FormatCreateEventStmt(const Nodes: TCreateEventStmt.TNodes);
    procedure FormatCreateIndexStmt(const Nodes: TCreateIndexStmt.TNodes);
    procedure FormatCreateRoutineStmt(const Nodes: TCreateRoutineStmt.TNodes);
    procedure FormatCreateServerStmt(const Nodes: TCreateServerStmt.TNodes);
    procedure FormatCreateTablespaceStmt(const Nodes: TCreateTablespaceStmt.TNodes);
    procedure FormatCreateTableStmt(const Nodes: TCreateTableStmt.TNodes);
    procedure FormatCreateTableStmtField(const Nodes: TCreateTableStmt.TField.TNodes);
    procedure FormatCreateTableStmtFieldDefaultFunc(const Nodes: TCreateTableStmt.TField.TDefaultFunc.TNodes);
    procedure FormatCreateTableStmtKey(const Nodes: TCreateTableStmt.TKey.TNodes);
    procedure FormatCreateTableStmtKeyColumn(const Nodes: TCreateTableStmt.TKeyColumn.TNodes);
    procedure FormatCreateTableStmtPartition(const Nodes: TCreateTableStmt.TPartition.TNodes);
    procedure FormatCreateTableStmtReference(const Nodes: TCreateTableStmt.TReference.TNodes);
    procedure FormatCreateTriggerStmt(const Nodes: TCreateTriggerStmt.TNodes);
    procedure FormatCreateUserStmt(const Nodes: TCreateUserStmt.TNodes);
    procedure FormatCreateViewStmt(const Nodes: TCreateViewStmt.TNodes);
    procedure FormatCurrentTimestamp(const Nodes: TCurrentTimestamp.TNodes);
    procedure FormatComments(const Token: PToken; Start: Boolean = False);
    procedure FormatDatatype(const Nodes: TDatatype.TNodes);
    procedure FormatDateAddFunc(const Nodes: TDateAddFunc.TNodes);
    procedure FormatDbIdent(const Nodes: TDbIdent.TNodes);
    procedure FormatDeclareCursorStmt(const Nodes: TDeclareCursorStmt.TNodes);
    procedure FormatDeclareHandlerStmt(const Nodes: TDeclareHandlerStmt.TNodes);
    procedure FormatDeleteStmt(const Nodes: TDeleteStmt.TNodes);
    procedure FormatDropTablespaceStmt(const Nodes: TDropTablespaceStmt.TNodes);
    procedure FormatExistsFunc(const Nodes: TExistsFunc.TNodes);
    procedure FormatExtractFunc(const Nodes: TExtractFunc.TNodes);
    procedure FormatFunctionCall(const Nodes: TFunctionCall.TNodes);
    procedure FormatGroupConcatFunc(const Nodes: TGroupConcatFunc.TNodes);
    procedure FormatIfStmt(const Nodes: TIfStmt.TNodes);
    procedure FormatIfStmtBranch(const Nodes: TIfStmt.TBranch.TNodes);
    procedure FormatInsertStmt(const Nodes: TInsertStmt.TNodes);
    procedure FormatList(const Nodes: TList.TNodes; const DelimiterType: TTokenType); overload; {$IFNDEF Debug} inline; {$ENDIF}
    procedure FormatList(const Nodes: TList.TNodes; const Spacer: TSpacer); overload;
    procedure FormatList(const Node: TOffset; const Spacer: TSpacer); overload; {$IFNDEF Debug} inline; {$ENDIF}
    procedure FormatLoadDataStmt(const Nodes: TLoadDataStmt.TNodes);
    procedure FormatLoadXMLStmt(const Nodes: TLoadXMLStmt.TNodes);
    procedure FormatLoopStmt(const Nodes: TLoopStmt.TNodes);
    procedure FormatMatchFunc(const Nodes: TMatchFunc.TNodes);
    procedure FormatPositionFunc(const Nodes: TPositionFunc.TNodes);
    procedure FormatNode(const Node: PNode; const Separator: TSeparatorType = stNone); overload;
    procedure FormatNode(const Node: TOffset; const Separator: TSeparatorType = stNone); overload; {$IFNDEF Debug} inline; {$ENDIF}
    procedure FormatRepeatStmt(const Nodes: TRepeatStmt.TNodes);
    procedure FormatRoot(const Node: PNode);
    procedure FormatSchedule(const Nodes: TSchedule.TNodes);
    procedure FormatSecretIdent(const Nodes: TSecretIdent.TNodes);
    procedure FormatSelectStmt(const Nodes: TSelectStmt.TNodes);
    procedure FormatSelectStmtColumn(const Nodes: TSelectStmt.TColumn.TNodes);
    procedure FormatSelectStmtTableFactor(const Nodes: TSelectStmt.TTableFactor.TNodes);
    procedure FormatSelectStmtTableFactorOj(const Nodes: TSelectStmt.TTableFactorOj.TNodes);
    procedure FormatSelectStmtTableJoin(const Nodes: TSelectStmt.TTableJoin.TNodes);
    procedure FormatSetStmt(const Nodes: TSetStmt.TNodes);
    procedure FormatShowBinlogEventsStmt(const Nodes: TShowBinlogEventsStmt.TNodes);
    procedure FormatShowErrorsStmt(const Nodes: TShowErrorsStmt.TNodes);
    procedure FormatShowRelaylogEventsStmt(const Nodes: TShowBinlogEventsStmt.TNodes);
    procedure FormatShowWarningsStmt(const Nodes: TShowWarningsStmt.TNodes);
    procedure FormatSubArea(const Nodes: TSubArea.TNodes);
    procedure FormatSubAreaSelectStmt(const Nodes: TSubAreaSelectStmt.TNodes);
    procedure FormatSubPartition(const Nodes: TSubPartition.TNodes);
    procedure FormatSubquery(const Nodes: TSubquery.TNodes);
    procedure FormatSubstringFunc(const Nodes: TSubstringFunc.TNodes);
    procedure FormatToken(const Token: PToken);
    procedure FormatTrimFunc(const Nodes: TTrimFunc.TNodes);
    procedure FormatUnaryOp(const Nodes: TUnaryOp.TNodes);
    procedure FormatUnknownStmt(const Node: PUnknownStmt);
    procedure FormatUser(const Nodes: TUser.TNodes);
    procedure FormatValue(const Nodes: TValue.TNodes);
    procedure FormatVariableIdent(const Nodes: TVariable.TNodes);
    procedure FormatWeightStringFunc(const Nodes: TWeightStringFunc.TNodes);
    procedure FormatWhileStmt(const Nodes: TWhileStmt.TNodes);
    procedure FormatXID(const Nodes: TXAStmt.TID.TNodes);
    function IsChild(const ANode: PNode): Boolean; overload; {$IFNDEF Debug} inline; {$ENDIF}
    function IsChild(const ANode: TOffset): Boolean; overload; {$IFNDEF Debug} inline; {$ENDIF}
    function IsRange(const ANode: PNode): Boolean; overload; {$IFNDEF Debug} inline; {$ENDIF}
    function IsRange(const ANode: TOffset): Boolean; overload; {$IFNDEF Debug} inline; {$ENDIF}
    function IsRoot(const ANode: PNode): Boolean; overload; {$IFNDEF Debug} inline; {$ENDIF}
    function IsStmt(const ANode: PNode): Boolean; overload; {$IFNDEF Debug} inline; {$ENDIF}
    function IsStmt(const ANode: TOffset): Boolean; overload; {$IFNDEF Debug} inline; {$ENDIF}
    function IsSymbol(const TokenType: TTokenType): Boolean;
    function IsTag(const KeywordIndex1: TWordList.TIndex;
      const KeywordIndex2: TWordList.TIndex = -1; const KeywordIndex3: TWordList.TIndex = -1;
      const KeywordIndex4: TWordList.TIndex = -1; const KeywordIndex5: TWordList.TIndex = -1;
      const KeywordIndex6: TWordList.TIndex = -1; const KeywordIndex7: TWordList.TIndex = -1): Boolean;
    function IsToken(const ANode: PNode): Boolean; overload; {$IFNDEF Debug} inline; {$ENDIF}
    function IsToken(const ANode: TOffset): Boolean; overload; {$IFNDEF Debug} inline; {$ENDIF}
    function NewNode(const ANodeType: TNodeType): TOffset;
    function NewText(const Text: PChar; const Length: Integer): TOffset;
    function NodePtr(const ANode: TOffset): PNode; {$IFNDEF Debug} inline; {$ENDIF}
    function NodeSize(const NodeType: TNodeType): Integer;
    function ParseAnalyzeTableStmt(): TOffset;
    function ParseAliasIdent(): TOffset;
    function ParseAlterDatabaseStmt(): TOffset;
    function ParseAlterEventStmt(): TOffset;
    function ParseAlterInstanceStmt(): TOffset;
    function ParseAlterRoutineStmt(const ARoutineType: TRoutineType): TOffset;
    function ParseAlterServerStmt(): TOffset;
    function ParseAlterStmt(): TOffset;
    function ParseAlterTablespaceStmt(): TOffset;
    function ParseAlterTableStmt(): TOffset;
    function ParseAlterTableStmtAlterColumn(): TOffset;
    function ParseAlterTableStmtConvertTo(): TOffset;
    function ParseAlterTableStmtDropItem(): TOffset;
    function ParseAlterTableStmtExchangePartition(): TOffset;
    function ParseAlterTableStmtOrderBy(): TOffset;
    function ParseAlterTableStmtReorganizePartition(): TOffset;
    function ParseAlterViewStmt(): TOffset;
    function ParseBeginLabel(): TOffset;
    function ParseBeginStmt(): TOffset;
    function ParseCallStmt(): TOffset;
    function ParseCaseOp(): TOffset;
    function ParseCaseOpBranch(): TOffset;
    function ParseCaseStmt(): TOffset;
    function ParseCaseStmtBranch(): TOffset;
    function ParseCastFunc(): TOffset;
    function ParseCharFunc(): TOffset;
    function ParseChangeMasterStmt(): TOffset;
    function ParseCharsetIdent(): TOffset;
    function ParseCheckTableStmt(): TOffset;
    function ParseCheckTableStmtOption(): TOffset;
    function ParseChecksumTableStmt(): TOffset;
    function ParseCloseStmt(): TOffset;
    function ParseCollateIdent(): TOffset; {$IFNDEF Debug} inline; {$ENDIF}
    function ParseCommitStmt(): TOffset;
    function ParseCompoundStmt(): TOffset;
    function ParseConvertFunc(): TOffset;
    function ParseCreateDatabaseStmt(): TOffset;
    function ParseCreateEventStmt(): TOffset;
    function ParseCreateIndexStmt(): TOffset;
    function ParseCreateRoutineStmt(const ARoutineType: TRoutineType): TOffset;
    function ParseCreateServerStmt(): TOffset;
    function ParseCreateServerStmtOptionList(): TOffset;
    function ParseCreateStmt(): TOffset; {$IFNDEF Debug} inline; {$ENDIF}
    function ParseCreateTablespaceStmt(): TOffset;
    function ParseCreateTableStmt(): TOffset;
    function ParseCreateTableStmtCheck(): TOffset;
    function ParseCreateTableStmtField(const Add: TCreateTableStmt.TFieldAdd = caNone): TOffset;
    function ParseCreateTableStmtFieldDefaultFunc(): TOffset;
    function ParseCreateTableStmtDefinition(): TOffset; overload; {$IFNDEF Debug} inline; {$ENDIF}
    function ParseCreateTableStmtDefinition(const AlterTableStmt: Boolean): TOffset; overload;
    function ParseCreateTableStmtDefinitionPartitionNames(): TOffset;
    function ParseCreateTableStmtForeignKey(const Add: Boolean = False): TOffset;
    function ParseCreateTableStmtKey(const AlterTableStmt: Boolean): TOffset;
    function ParseCreateTableStmtKeyColumn(): TOffset;
    function ParseCreateTableStmtPartition(): TOffset; overload; {$IFNDEF Debug} inline; {$ENDIF}
    function ParseCreateTableStmtPartition(const AlterTableStmt: Boolean): TOffset; overload;
    function ParseCreateTableStmtReference(): TOffset;
    function ParseCreateTableStmtUnion(): TOffset;
    function ParseCreateTriggerStmt(): TOffset;
    function ParseCreateUserStmt(const Alter: Boolean): TOffset;
    function ParseCreateViewStmt(): TOffset;
    function ParseCurrentTimestamp(): TOffset;
    function ParseDatabaseIdent(): TOffset; {$IFNDEF Debug} inline; {$ENDIF}
    function ParseDatatype(): TOffset;
    function ParseDateAddFunc(): TOffset;
    function ParseDateUnit(): TOffset;
    function ParseDbIdent(const ADbIdentType: TDbIdentType; const FullQualified: Boolean = True): TOffset;
    function ParseDefinerValue(): TOffset;
    function ParseDeallocatePrepareStmt(): TOffset;
    function ParseDeclareStmt(): TOffset;
    function ParseDeclareConditionStmt(): TOffset;
    function ParseDeclareCursorStmt(): TOffset;
    function ParseDeclareHandlerStmt(): TOffset;
    function ParseDeclareHandlerStmtCondition(): TOffset;
    function ParseDeleteStmt(): TOffset;
    function ParseDoStmt(): TOffset;
    function ParseDropDatabaseStmt(): TOffset;
    function ParseDropEventStmt(): TOffset;
    function ParseDropIndexStmt(): TOffset;
    function ParseDropRoutineStmt(const ARoutineType: TRoutineType): TOffset;
    function ParseDropServerStmt(): TOffset;
    function ParseDropTablespaceStmt(): TOffset;
    function ParseDropTableStmt(): TOffset;
    function ParseDropTriggerStmt(): TOffset;
    function ParseDropUserStmt(): TOffset;
    function ParseDropViewStmt(): TOffset;
    function ParseEventIdent(): TOffset;
    function ParseEndLabel(): TOffset;
    function ParseEngineIdent(): TOffset;
    function ParseExecuteStmt(): TOffset;
    function ParseExistsFunc(): TOffset;
    function ParseExplainStmt(): TOffset;
    function ParseExpr(): TOffset; overload; {$IFNDEF Debug} inline; {$ENDIF}
    function ParseExpr(const InAllowed: Boolean): TOffset; overload;
    function ParseExtractFunc(): TOffset;
    function ParseFetchStmt(): TOffset;
    function ParseFieldIdent(): TOffset;
    function ParseFieldIdentFullQualified(): TOffset;
    function ParseFieldOrVariableIdent(): TOffset;
    function ParseFlushStmt(): TOffset;
    function ParseFlushStmtOption(): TOffset;
    function ParseForeignKeyIdent(): TOffset; {$IFNDEF Debug} inline; {$ENDIF}
    function ParseFunctionCall(): TOffset;
    function ParseFunctionIdent(): TOffset; {$IFNDEF Debug} inline; {$ENDIF}
    function ParseFunctionParam(): TOffset;
    function ParseFunctionReturns(): TOffset;
    function ParseGetDiagnosticsStmt(): TOffset;
    function ParseGetDiagnosticsStmtStmtInfo(): TOffset;
    function ParseGetDiagnosticsStmtConditionInfo(): TOffset;
    function ParseGrantStmt(): TOffset;
    function ParseGrantStmtPrivileg(): TOffset;
    function ParseGrantStmtUserSpecification(): TOffset;
    function ParseGroupConcatFunc(): TOffset;
    function ParseGroupConcatFuncExpr(): TOffset;
    function ParseHelpStmt(): TOffset;
    function ParseIdent(): TOffset;
    function ParseIfStmt(): TOffset;
    function ParseInsertStmt(const Replace: Boolean = False): TOffset;
    function ParseInsertStmtSetItemsList(): TOffset;
    function ParseInsertStmtValuesList(): TOffset;
    function ParseInteger(): TOffset;
    function ParseIntervalOp(): TOffset;
    function ParseIterateStmt(): TOffset;
    function ParseKeyIdent(): TOffset; {$IFNDEF Debug} inline; {$ENDIF}
    function ParseKillStmt(): TOffset;
    function ParseLeaveStmt(): TOffset;
    function ParseList(const Brackets: Boolean; const ParseElement: TParseFunction; const DelimiterType: TTokenType = ttComma): TOffset; overload;
    function ParseLoadDataStmt(): TOffset;
    function ParseLoadStmt(): TOffset;
    function ParseLoadXMLStmt(): TOffset;
    function ParseLockTableStmt(): TOffset;
    function ParseLockStmtItem(): TOffset;
    function ParseLoopStmt(): TOffset;
    function ParseNumeric(): TOffset;
    function ParseMatchFunc(): TOffset;
    function ParseOpenStmt(): TOffset;
    function ParseOptimizeTableStmt(): TOffset;
    function ParsePartitionIdent(): TOffset; {$IFNDEF Debug} inline; {$ENDIF}
    function ParsePL_SQLStmt(): TOffset; {$IFNDEF Debug} inline; {$ENDIF}
    function ParsePositionFunc(): TOffset;
    function ParsePrepareStmt(): TOffset;
    function ParseProcedureIdent(): TOffset; {$IFNDEF Debug} inline; {$ENDIF}
    function ParsePurgeStmt(): TOffset;
    function ParseProcedureParam(): TOffset;
    function ParseReleaseStmt(): TOffset;
    function ParseRenameStmt(): TOffset;
    function ParseRenameStmtTablePair(): TOffset;
    function ParseRenameStmtUserPair(): TOffset;
    function ParseRepairTableStmt(): TOffset;
    function ParseRepeatStmt(): TOffset;
    function ParseResetStmt(): TOffset;
    function ParseResignalStmt(): TOffset;
    function ParseReturnStmt(): TOffset;
    function ParseResetStmtOption(): TOffset;
    function ParseRevokeStmt(): TOffset;
    function ParseRollbackStmt(): TOffset;
    function ParseRoot(): TOffset;
    function ParseSavepointIdent(): TOffset;
    function ParseSavepointStmt(): TOffset;
    function ParseSchedule(): TOffset;
    function ParseSecretIdent(): TOffset;
    function ParseSelectStmt(const SubSelect: Boolean): TOffset; overload;
    function ParseSelectStmtColumn(): TOffset;
    function ParseSelectStmtGroup(): TOffset;
    function ParseSelectStmtOrderBy(): TOffset;
    function ParseSelectStmtTableEscapedReference(): TOffset;
    function ParseSelectStmtTableFactor(): TOffset;
    function ParseSelectStmtTableFactorIndexHint(): TOffset;
    function ParseSelectStmtTableFactorSelect(): TOffset;
    function ParseSelectStmtTableFactorOj(): TOffset;
    function ParseSelectStmtTableReference(): TOffset;
    function ParseSetNamesStmt(): TOffset;
    function ParseSetPasswordStmt(): TOffset;
    function ParseSetStmt(): TOffset;
    function ParseSetStmtAssignment(): TOffset;
    function ParseSetTransactionStmt(): TOffset;
    function ParseSetTransactionStmtCharacterisic(): TOffset;
    function ParseShowAuthorsStmt(): TOffset;
    function ParseShowBinaryLogsStmt(): TOffset;
    function ParseShowBinlogEventsStmt(): TOffset;
    function ParseShowCharacterSetStmt(): TOffset;
    function ParseShowCollationStmt(): TOffset;
    function ParseShowColumnsStmt(): TOffset;
    function ParseShowContributorsStmt(): TOffset;
    function ParseShowCountErrorsStmt(): TOffset;
    function ParseShowCountWarningsStmt(): TOffset;
    function ParseShowCreateDatabaseStmt(): TOffset;
    function ParseShowCreateEventStmt(): TOffset;
    function ParseShowCreateFunctionStmt(): TOffset;
    function ParseShowCreateProcedureStmt(): TOffset;
    function ParseShowCreateTableStmt(): TOffset;
    function ParseShowCreateTriggerStmt(): TOffset;
    function ParseShowCreateUserStmt(): TOffset;
    function ParseShowCreateViewStmt(): TOffset;
    function ParseShowDatabasesStmt(): TOffset;
    function ParseShowEngineStmt(): TOffset;
    function ParseShowEnginesStmt(): TOffset;
    function ParseShowErrorsStmt(): TOffset;
    function ParseShowEventsStmt(): TOffset;
    function ParseShowFunctionCodeStmt(): TOffset;
    function ParseShowFunctionStatusStmt(): TOffset;
    function ParseShowGrantsStmt(): TOffset;
    function ParseShowIndexStmt(): TOffset;
    function ParseShowMasterStatusStmt(): TOffset;
    function ParseShowOpenTablesStmt(): TOffset;
    function ParseShowPluginsStmt(): TOffset;
    function ParseShowPrivilegesStmt(): TOffset;
    function ParseShowProcedureCodeStmt(): TOffset;
    function ParseShowProcedureStatusStmt(): TOffset;
    function ParseShowProcessListStmt(): TOffset;
    function ParseShowProfileStmt(): TOffset;
    function ParseShowProfileStmtType(): TOffset;
    function ParseShowProfilesStmt(): TOffset;
    function ParseShowRelaylogEventsStmt(): TOffset;
    function ParseShowSlaveHostsStmt(): TOffset;
    function ParseShowSlaveStatusStmt(): TOffset;
    function ParseShowStatusStmt(): TOffset;
    function ParseShowTableStatusStmt(): TOffset;
    function ParseShowTablesStmt(): TOffset;
    function ParseShowTriggersStmt(): TOffset;
    function ParseShowVariablesStmt(): TOffset;
    function ParseShowWarningsStmt(): TOffset;
    function ParseShutdownStmt(): TOffset;
    function ParseSignalStmt(): TOffset;
    function ParseSignalStmtInformation(): TOffset;
    function ParseStartSlaveStmt(): TOffset;
    function ParseStartTransactionStmt(): TOffset;
    function ParseStopSlaveStmt(): TOffset;
    function ParseString(): TOffset;
    function ParseSubArea(const ParseArea: TParseFunction): TOffset;
    function ParseSubPartition(): TOffset;
    function ParseSubquery(): TOffset;
    function ParseSubstringFunc(): TOffset;
    function ParseStmt(): TOffset;
    function ParseSymbol(const TokenType: TTokenType): TOffset;
    function ParseTableIdent(): TOffset; {$IFNDEF Debug} inline; {$ENDIF}
    function ParseTag(const KeywordIndex1: TWordList.TIndex;
      const KeywordIndex2: TWordList.TIndex = -1; const KeywordIndex3: TWordList.TIndex = -1;
      const KeywordIndex4: TWordList.TIndex = -1; const KeywordIndex5: TWordList.TIndex = -1;
      const KeywordIndex6: TWordList.TIndex = -1; const KeywordIndex7: TWordList.TIndex = -1): TOffset;
    function ParseToken(): TOffset;
    function ParseTriggerIdent(): TOffset;
    function ParseTrimFunc(): TOffset;
    function ParseTruncateTableStmt(): TOffset;
    function ParseUnknownStmt(): TOffset;
    function ParseUnlockTablesStmt(): TOffset;
    function ParseUpdateStmt(): TOffset;
    function ParseUpdateStmtValue(): TOffset;
    function ParseUserIdent(): TOffset;
    function ParseUseStmt(): TOffset;
    function ParseValue(const KeywordIndex: TWordList.TIndex; const Assign: TValueAssign; const Brackets: Boolean; const ParseItem: TParseFunction): TOffset; overload;
    function ParseValue(const KeywordIndex: TWordList.TIndex; const Assign: TValueAssign; const OptionIndices: TWordList.TIndices): TOffset; overload;
    function ParseValue(const KeywordIndex: TWordList.TIndex; const Assign: TValueAssign; const ParseValueNode: TParseFunction): TOffset; overload;
    function ParseValue(const KeywordIndices: TWordList.TIndices; const Assign: TValueAssign; const Brackets: Boolean; const ParseItem: TParseFunction): TOffset; overload;
    function ParseValue(const KeywordIndices: TWordList.TIndices; const Assign: TValueAssign; const OptionIndices: TWordList.TIndices): TOffset; overload;
    function ParseValue(const KeywordIndices: TWordList.TIndices; const Assign: TValueAssign; const ParseValueNode: TParseFunction): TOffset; overload;
    function ParseValue(const KeywordIndices: TWordList.TIndices; const Assign: TValueAssign; const ValueKeywordIndex1: TWordList.TIndex; const ValueKeywordIndex2: TWordList.TIndex = -1): TOffset; overload;
    function ParseVariableIdent(): TOffset;
    function ParseWeightStringFunc(): TOffset;
    function ParseWeightStringFuncLevel(): TOffset;
    function ParseWhileStmt(): TOffset;
    function ParseXAStmt(): TOffset;
    procedure SetError(const AErrorCode: Byte; const AErrorToken: TOffset = 0);
    function StmtPtr(const Node: TOffset): PStmt; {$IFNDEF Debug} inline; {$ENDIF}
    function TokenPtr(const Token: TOffset): PToken; {$IFNDEF Debug} inline; {$ENDIF}
    property UseCompletionList: Boolean read FUseCompletionList;

  public
    ttIdents: set of TTokenType;
    ttStrings: set of TTokenType;

    procedure Clear();
    constructor Create(const AMySQLVersion: Integer = 0);
    destructor Destroy(); override;
    procedure FormatSQL(out SQL: string);
    function LoadFromFile(const Filename: string): Boolean;
    function ParseSQL(const Text: PChar; const Length: Integer; const AUseCompletionList: Boolean = False): Boolean; overload;
    function ParseSQL(const Text: string; const AUseCompletionList: Boolean = False): Boolean; overload; {$IFNDEF Debug} inline; {$ENDIF}
    procedure SaveToFile(const Filename: string; const FileType: TFileType = ftSQL);
    property AnsiQuotes: Boolean read FAnsiQuotes write FAnsiQuotes;
    property CompletionList: TCompletionList read FCompletionList;
    property Datatypes: string read GetDatatypes write SetDatatypes;
    property Functions: string read GetFunctions write SetFunctions;
    property Keywords: string read GetKeywords write SetKeywords;
    property MySQLVersion: Integer read FMySQLVersion;
    property Root: PRoot read GetRoot;
  end;

implementation {***************************************************************}

uses
  Windows,
  SysUtils, StrUtils, RTLConsts, Math,
  SQLUtils;

resourcestring
  SUnknownError = 'Unknown Error';
  SDatatypeNotFound = 'Datatype "%s" not found';
  SKeywordNotFound = 'Keyword "%s" not found';
  SUnknownOperatorPrecedence = 'Unknown operator precedence for operator "%s"';
  STooManyTokensInExpr = 'Too many tokens (%d) in expression';
  SUnknownNodeType = 'Unknown node type';
  SOutOfMemory = 'Out of memory (%d)';

const
  CP_UNICODE = 1200;
  BOM_UTF8: PAnsiChar = Chr($EF) + Chr($BB) + Chr($BF);
  BOM_UNICODE_LE: PAnsiChar = Chr($FF) + Chr($FE);

  DefaultParsedNodesMemSize = 100 * 1024;
  DefaultTextsMemSize = 10 * 1024;

var
  UsageTypeByTokenType: array[TSQLParser.TTokenType] of TSQLParser.TUsageType = (
    utUnknown,
    utError,
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
    utDbIdent,
    utDbIdent,
    utDbIdent,
    utSymbol,
    utSymbol,
    utSymbol,
    utSymbol,
    utConst
  );

function HTMLEscape(const Value: PChar; const ValueLen: Integer; const Escaped: PChar; const EscapedLen: Integer): Integer; overload;
label
  StartL,
  StringL, String2, String3, String4,
  MoveReplace, MoveReplaceL, MoveReplaceE,
  PosL, PosE,
  FindPos, FindPos2,
  Error,
  Finish;
const
  SearchLen = 6;
  Search: array [0 .. SearchLen - 1] of Char = (#0, #10, #13, '"', '<', '>');
  Replace: array [0 .. SearchLen - 1] of PChar = ('', '<br>' + #13#10, '', '&quot;', '&lt;', '&gt;');
var
  Len: Integer;
  Poss: packed array [0 .. SearchLen - 1] of Cardinal;
begin
  Result := 0;
  Len := EscapedLen;

  asm
        PUSH ES
        PUSH ESI
        PUSH EDI
        PUSH EBX

        PUSH DS                          // string operations uses ES
        POP ES
        CLD                              // string operations uses forward direction

        MOV ESI,PChar(Value)             // Copy characters from Value
        MOV EDI,Escaped                  //   to Escaped
        MOV ECX,ValueLen                 // Length of Value string

      // -------------------

        MOV EBX,0                        // Numbers of characters in Search
      StartL:
        CALL FindPos                     // Find Search character Pos
        INC EBX                          // Next character in Search
        CMP EBX,SearchLen                // All Search characters handled?
        JNE StartL                       // No!

      // -------------------

        CMP ECX,0                        // Empty string?
        JE Finish                        // Yes!
      StringL:
        PUSH ECX

        MOV ECX,0                        // Numbers of characters in Search
        MOV EBX,-1                       // Index of first Pos
        MOV EAX,0                        // Last character
        LEA EDX,Poss
      PosL:
        CMP [EDX + ECX * 4],EAX          // Pos before other Poss?
        JB PosE                          // No!
        MOV EBX,ECX                      // Index of first Pos
        MOV EAX,[EDX + EBX * 4]          // Value of first Pos
      PosE:
        INC ECX                          // Next Pos
        CMP ECX,SearchLen                // All Poss compared?
        JNE PosL                         // No!

        POP ECX

        SUB ECX,EAX                      // Copy normal characters from Value
        JZ String3                       // End of Value!
        ADD @Result,ECX                  // Characters to copy
        CMP Escaped,0                    // Calculate length only?
        JE String2                       // Yes!
        CMP Len,ECX                      // Enough character left in Escaped?
        JB Error                         // No!
        SUB Len,ECX                      // Calc new len space in Escaped
        REPNE MOVSW                      // Copy normal characters to Result
        JMP String3
      String2:
        SHL ECX,1
        ADD ESI,ECX

      String3:
        MOV ECX,EAX
        JECXZ Finish                     // End of Value!

        ADD ESI,2                        // Step of Search character


      MoveReplace:
        PUSH ESI
        LEA EDX,Replace                  // Insert Replace string
        MOV ESI,[EDX + EBX * 4]
      MoveReplaceL:
        LODSW                            // Get Replace character
        CMP AX,0                         // End of Replace?
        JE MoveReplaceE                  // Yes!
        INC @Result                      // One characters in replace
        CMP Escaped,0                    // Calculate length only?
        JE MoveReplaceL                  // Yes!
        CMP Len,ECX                      // Enough character left in Escaped?
        JB Error                         // No!
        STOSW                            // Put character in Result
        JMP MoveReplaceL
      MoveReplaceE:
        POP ESI


      String4:
        DEC ECX                          // Ignore Search character
        JZ Finish                        // All character in Value handled!
        CALL FindPos                     // Find Search character
        JMP StringL

      // -------------------

      FindPos:
        PUSH ECX
        PUSH EDI
        LEA EDI,Search                   // Character to Search
        MOV AX,[EDI + EBX * 2]
        MOV EDI,ESI                      // Search in Value
        REPNE SCASW                      // Find Search character
        JNE FindPos2                     // Search character not found!
        INC ECX
      FindPos2:
        LEA EDI,Poss
        MOV [EDI + EBX * 4],ECX          // Store found Position
        POP EDI
        POP ECX
        RET

      // -------------------

      Error:
        MOV @Result,0                    // Too few space in Escaped

      Finish:
        POP EBX
        POP EDI
        POP ESI
        POP ES
  end;
end;

function HTMLEscape(const Value: string): string; overload;
var
  Len: Integer;
begin
  Len := HTMLEscape(PChar(Value), Length(Value), nil, 0);
  SetLength(Result, Len);
  HTMLEscape(PChar(Value), Length(Value), PChar(Result), Len);
end;

function WordIndices(const Index0: TSQLParser.TWordList.TIndex;
  const Index1: TSQLParser.TWordList.TIndex = -1;
  const Index2: TSQLParser.TWordList.TIndex = -1;
  const Index3: TSQLParser.TWordList.TIndex = -1;
  const Index4: TSQLParser.TWordList.TIndex = -1;
  const Index5: TSQLParser.TWordList.TIndex = -1;
  const Index6: TSQLParser.TWordList.TIndex = -1): TSQLParser.TWordList.TIndices;
begin
  Result[0] := Index0;
  Result[1] := Index1;
  Result[2] := Index2;
  Result[3] := Index3;
  Result[4] := Index4;
  Result[5] := Index5;
  Result[6] := Index6;
end;

{ TSQLParser.TStringBuffer ****************************************************}

procedure TSQLParser.TStringBuffer.Clear();
begin
  Buffer.Write := Buffer.Mem;
end;

constructor TSQLParser.TStringBuffer.Create(const InitialLength: Integer);
begin
  Buffer.Mem := nil;
  Buffer.MemSize := 0;
  Buffer.Write := nil;

  Reallocate(InitialLength);
end;

procedure TSQLParser.TStringBuffer.Delete(const Start: Integer; const Length: Integer);
begin
  Move(Buffer.Mem[Start + Length], Buffer.Mem[Start], Size - Length);
  Buffer.Write := Pointer(Integer(Buffer.Write) - Length);
end;

destructor TSQLParser.TStringBuffer.Destroy();
begin
  FreeMem(Buffer.Mem);

  inherited;
end;

function TSQLParser.TStringBuffer.GetData(): Pointer;
begin
  Result := Pointer(Buffer.Mem);
end;

function TSQLParser.TStringBuffer.GetLength(): Integer;
begin
  Result := (Integer(Buffer.Write) - Integer(Buffer.Mem)) div SizeOf(Buffer.Mem[0]);
end;

function TSQLParser.TStringBuffer.GetSize(): Integer;
begin
  Result := Integer(Buffer.Write) - Integer(Buffer.Mem);
end;

function TSQLParser.TStringBuffer.GetText(): PChar;
begin
  Result := Buffer.Mem;
end;

function TSQLParser.TStringBuffer.Read(): string;
begin
  SetString(Result, PChar(Buffer.Mem), Size div SizeOf(Result[1]));
end;

procedure TSQLParser.TStringBuffer.Reallocate(const NeededLength: Integer);
var
  Index: Integer;
begin
  if (Buffer.MemSize = 0) then
  begin
    Buffer.MemSize := NeededLength * SizeOf(Buffer.Write[0]);
    GetMem(Buffer.Mem, Buffer.MemSize);
    Buffer.Write := Buffer.Mem;
  end
  else if (Size + NeededLength * SizeOf(Buffer.Mem[0]) > Buffer.MemSize) then
  begin
    Index := Size div SizeOf(Buffer.Write[0]);
    Inc(Buffer.MemSize, 2 * (Size + NeededLength * SizeOf(Buffer.Mem[0]) - Buffer.MemSize));
    ReallocMem(Buffer.Mem, Buffer.MemSize);
    Buffer.Write := @Buffer.Mem[Index];
  end;
end;

procedure TSQLParser.TStringBuffer.Write(const Text: PChar; const Length: Integer);
begin
  if (Length > 0) then
  begin
    Reallocate(Length);

    Move(Text^, Buffer.Write^, Length * SizeOf(Buffer.Mem[0]));
    Buffer.Write := @Buffer.Write[Length];
  end;
end;

procedure TSQLParser.TStringBuffer.Write(const Text: string);
begin
  Write(PChar(Text), System.Length(Text));
end;

procedure TSQLParser.TStringBuffer.Write(const Char: Char);
begin
  Write(@Char, 1);
end;

{ TSQLParser.TWordList ********************************************************}

procedure TSQLParser.TWordList.Clear();
begin
  FText := '';

  FCount := 0;
  SetLength(FIndex, 0);

  SetLength(Parser.OperatorTypeByKeywordIndex, 0);
end;

constructor TSQLParser.TWordList.Create(const ASQLParser: TSQLParser; const AText: string = '');
begin
  FParser := ASQLParser;

  FCount := 0;
  SetLength(FIndex, 0);

  Text := AText;
end;

destructor TSQLParser.TWordList.Destroy();
begin
  Clear();

  inherited;
end;

function TSQLParser.TWordList.GetText(): string;
var
  I: Integer;
begin
  Result := '';
  for I := 0 to Length(FIndex) - 1 do
    if (I < Length(FIndex) - 1) then
      Result := Result + StrPas(FIndex[I]) + ','
    else
      Result := Result + StrPas(FIndex[I]);
end;

function TSQLParser.TWordList.GetWord(Index: TWordList.TIndex): string;
begin
  Result := StrPas(FIndex[Index]);
end;

procedure TSQLParser.TWordList.GetWordText(const Index: TIndex; out Text: PChar; out Length: Integer);
begin
  Text := FIndex[Index];
  Length := StrLen(Text);
end;

function TSQLParser.TWordList.IndexOf(const Word: PChar; const Length: Integer): Integer;
var
  Comp: Integer;
  Left: Integer;
  I: Integer;
  Mid: Integer;
  Right: Integer;
  UpcaseWord: array [0..100] of Char;
begin
  Result := -1;

  for I := 0 to Length - 1 do
    UpcaseWord[I] := Upcase(Word[I]);

  if (Length <= System.Length(FFirst) - 2) then
  begin
    Left := FFirst[Length];
    Right := FFirst[Length + 1] - 1;
    while (Left <= Right) do
    begin
      Mid := (Right - Left) div 2 + Left;
      Comp := StrLComp(FIndex[Mid], @UpcaseWord, Length);
      if (Comp < 0) then
        Left := Mid + 1
      else if (Comp = 0) then
        begin Result := Mid; break; end
      else
        Right := Mid - 1;
    end;
  end;
end;

function TSQLParser.TWordList.IndexOf(const Word: string): Integer;
begin
  Result := IndexOf(PChar(Word), Length(Word));
end;

procedure TSQLParser.TWordList.SetText(AText: string);
var
  Counts: array of Integer;
  Words: array of array of PChar;

  function InsertIndex(const Word: PChar; const Len: Integer; out Index: Integer): Boolean;
  var
    Comp: Integer;
    Left: Integer;
    Mid: Integer;
    Right: Integer;
  begin
    Result := True;

    if ((Counts[Len] = 0) or (StrLComp(Word, Words[Len][Counts[Len] - 1], Len) > 0)) then
      Index := Counts[Len]
    else
    begin
      Left := 0;
      Right := Counts[Len] - 1;
      while (Left <= Right) do
      begin
        Mid := (Right - Left) div 2 + Left;
        Comp := StrLComp(Words[Len][Mid], Word, Len);
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
    Index: Integer;
  begin
    if (InsertIndex(Word, Len, Index)) then
    begin
      Move(Words[Len][Index], Words[Len][Index + 1], (Counts[Len] - Index) * SizeOf(Words[Len][0]));
      Words[Len][Index] := Word;
      Inc(Counts[Len]);
    end;
  end;

var
  I: Integer;
  Index: Integer;
  J: Integer;
  Len: Integer;
  MaxLen: Integer;
  OldIndex: Integer;
begin
  Clear();

  if (AText <> '') then
  begin
    FText := UpperCase(ReplaceStr(AText, ',', #0)) + #0;

    OldIndex := 1; Index := 1; MaxLen := 0; FCount := 0;
    while (Index < Length(FText)) do
    begin
      while (FText[Index] <> #0) do Inc(Index);
      Len := Index - OldIndex;
      if (Len > MaxLen) then MaxLen := Len;
      Inc(FCount);
      Inc(Index); // Set over #0
      OldIndex := Index;
    end;

    SetLength(Words, MaxLen + 1);
    SetLength(Counts, MaxLen + 1);
    for I := 1 to MaxLen do
    begin
      Counts[I] := 0;
      SetLength(Words[I], FCount + 1);
      for J := 0 to FCount do
        Words[I][J] := #0;
    end;

    OldIndex := 1; Index := 1;
    while (Index < Length(FText)) do
    begin
      while (FText[Index] <> #0) do Inc(Index);
      Len := Index - OldIndex;
      Add(@FText[OldIndex], Len);
      Inc(Index); // Step over #0
      OldIndex := Index;
    end;

    FCount := 0;
    SetLength(FFirst, MaxLen + 2);
    for I := 1 to MaxLen do
    begin
      FFirst[I] := FCount;
      Inc(FCount, Counts[I]);
    end;
    FFirst[MaxLen + 1] := FCount;

    SetLength(FIndex, FCount);
    Index := 0;
    for I := 1 to MaxLen do
      for J := 0 to Counts[I] - 1 do
      begin
        FIndex[Index] := Words[I][J];
        Inc(Index);
      end;

    // Clear helpers
    for I := 1 to MaxLen do
      SetLength(Words[I], 0);
    SetLength(Counts, 0);
    SetLength(Words, 0);
  end;
end;

{ TSQLParser.TFormatHandle ****************************************************}

constructor TSQLParser.TFormatBuffer.Create();
var
  S: string;
begin
  inherited Create(1024);

  FNewLine := False;
  Indent := 0;
  S := StringOfChar(' ', System.Length(IndentSpaces));
  Move(S[1], IndentSpaces, SizeOf(IndentSpaces));
end;

procedure TSQLParser.TFormatBuffer.DecreaseIndent();
begin
  Assert(Indent >= IndentSize);

  if (Indent >= IndentSize) then
    Dec(Indent, IndentSize);
end;

destructor TSQLParser.TFormatBuffer.Destroy();
begin
  inherited;
end;

procedure TSQLParser.TFormatBuffer.IncreaseIndent();
begin
  Inc(Indent, IndentSize);
end;

procedure TSQLParser.TFormatBuffer.Write(const Text: PChar; const Length: Integer);
begin
  inherited;

  FNewLine := False;
end;

procedure TSQLParser.TFormatBuffer.WriteReturn();
begin
  Write(#13#10);
  if (Indent > 0) then
    Write(@IndentSpaces[0], Indent);
  FNewLine := True;
end;

procedure TSQLParser.TFormatBuffer.WriteSpace();
begin
  Write(' ');
end;

{ TSQLParser.TCompletionList **************************************************}

procedure TSQLParser.TCompletionList.AddKeyword(const KeywordIndex: TWordList.TIndex;
  const KeywordIndex2: TWordList.TIndex = -1; const KeywordIndex3: TWordList.TIndex = -1;
  const KeywordIndex4: TWordList.TIndex = -1; const KeywordIndex5: TWordList.TIndex = -1;
  const KeywordIndex6: TWordList.TIndex = -1; const KeywordIndex7: TWordList.TIndex = -1);
var
  Item: PItem;
  Length: Integer;
  Tag: PChar;
  Text: PChar;
begin
  if (Parser.UseCompletionList) then
  begin
    GetMem(Item, SizeOf(Item^));
    FillChar(Item^, SizeOf(Item^), 0);
    Item^.ItemType := itTag;
    Tag := @Item^.Tag[0];

    Parser.KeywordList.GetWordText(KeywordIndex, Text, Length);
    StrLCopy(Tag, Text, Length); Tag := @Tag[Length];

    if (KeywordIndex2 >= 0) then
    begin
      Tag[0] := ' '; Tag := @Tag[1];
      Parser.KeywordList.GetWordText(KeywordIndex2, Text, Length);
      StrLCopy(Tag, Text, Length); Tag := @Tag[Length];
    end;
    if (KeywordIndex3 >= 0) then
    begin
      Tag[0] := ' '; Tag := @Tag[1];
      Parser.KeywordList.GetWordText(KeywordIndex3, Text, Length);
      StrLCopy(Tag, Text, Length); Tag := @Tag[Length];
    end;
    if (KeywordIndex4 >= 0) then
    begin
      Tag[0] := ' '; Tag := @Tag[1];
      Parser.KeywordList.GetWordText(KeywordIndex4, Text, Length);
      StrLCopy(Tag, Text, Length); Tag := @Tag[Length];
    end;
    if (KeywordIndex5 >= 0) then
    begin
      Tag[0] := ' '; Tag := @Tag[1];
      Parser.KeywordList.GetWordText(KeywordIndex5, Text, Length);
      StrLCopy(Tag, Text, Length); Tag := @Tag[Length];
    end;
    if (KeywordIndex6 >= 0) then
    begin
      Tag[0] := ' '; Tag := @Tag[1];
      Parser.KeywordList.GetWordText(KeywordIndex6, Text, Length);
      StrLCopy(Tag, Text, Length);
    end;

    Add(Item);
  end;
end;

procedure TSQLParser.TCompletionList.AddList(DbIdentType: TDbIdentType;
  const DatabaseName: string = ''; TableName: string = '');
var
  Item: PItem;
begin
  if (Parser.UseCompletionList) then
  begin
    Assert(Length(DatabaseName) < Length(Item^.DatabaseName));
    Assert(Length(TableName) < Length(Item^.TableName));

    GetMem(Item, SizeOf(Item^));
    Item^.ItemType := itList;
    Item^.DbIdentType := DbIdentType;
    StrPLCopy(Item^.DatabaseName, DatabaseName, Length(Item^.DatabaseName) - 1);
    StrPLCopy(Item^.TableName, TableName, Length(Item^.TableName) - 1);
    Add(Item);
  end;
end;

procedure TSQLParser.TCompletionList.Clear();
var
  I: Integer;
begin
  for I := 0 to Count - 1 do
    FreeMem(Items[I]);

  inherited;
end;

constructor TSQLParser.TCompletionList.Create(const AParser: TSQLParser);
begin
  inherited Create();

  FParser := AParser;
end;

destructor TSQLParser.TCompletionList.Destroy();
begin
  Clear();

  inherited;
end;

function TSQLParser.TCompletionList.GetItem(Index: Integer): PItem;
begin
  Result := PItem(inherited Items[Index]);
end;

{ TSQLParser.TNode ************************************************************}

class function TSQLParser.TNode.Create(const AParser: TSQLParser; const ANodeType: TNodeType): TOffset;
begin
  Result := AParser.NewNode(ANodeType);

  with PNode(AParser.NodePtr(Result))^ do
  begin
    FNodeType := ANodeType;
    FParser := AParser;
  end;
end;

function TSQLParser.TNode.GetOffset(): TOffset;
begin
  Result := @Self - Parser.Nodes.Mem;
end;

{ TSQLParser.TChild ***********************************************************}

class function TSQLParser.TChild.Create(const AParser: TSQLParser; const ANodeType: TNodeType): TOffset;
begin
  Result := TNode.Create(AParser, ANodeType);

  with PChild(AParser.NodePtr(Result))^ do
  begin
    FParentNode := 0;
  end;
end;

function TSQLParser.TChild.GetDelimiter(): PToken;
var
  Token: PToken;
begin
  Result := nil;
  if ((ParentNode^.NodeType = ntList) and (PList(ParentNode)^.DelimiterType <> ttUnknown)
    and Assigned(LastToken)) then
  begin
    Token := LastToken^.NextToken;

    if (Assigned(Token) and (Token^.TokenType = PList(ParentNode)^.DelimiterType)) then
      Result := Token;
  end;
end;

function TSQLParser.TChild.GetFFirstToken(): TOffset;
begin
  if (NodeType = ntToken) then
    Result := @Self - Parser.Nodes.Mem
  else
    Result := TSQLParser.PRange(@Self)^.FFirstToken;
end;

function TSQLParser.TChild.GetFirstToken(): PToken;
begin
  if (NodeType = ntToken) then
    Result := @Self
  else
    Result := PRange(@Self)^.FirstToken;
end;

function TSQLParser.TChild.GetFLastToken(): TOffset;
begin
  if (NodeType = ntToken) then
    Result := PNode(@Self)^.Offset
  else
    Result := PRange(@Self)^.FLastToken;
end;

function TSQLParser.TChild.GetLastToken(): PToken;
begin
  if (NodeType = ntToken) then
    Result := @Self
  else
    Result := PRange(@Self)^.LastToken;
end;

function TSQLParser.TChild.GetNextSibling(): PChild;
var
  Child: PChild;
  Token: PToken;
begin
  Result := nil;
  if ((ParentNode^.NodeType = ntList)
    and Assigned(LastToken)) then
  begin
    Token := LastToken^.NextToken;

    if (Assigned(Token)) then
      if (PList(ParentNode)^.DelimiterType = ttUnknown) then
      begin
        Token := Token^.NextToken;

        if (Assigned(Token)) then
        begin
          Child := PChild(Token);

          while (Assigned(Child) and (Child^.ParentNode <> ParentNode)) do
            if (not Parser.IsChild(Child^.ParentNode)) then
              Child := nil
            else
              Child := PChild(Child^.ParentNode);

          Result := Child;
        end;
      end
      else if (Token^.TokenType = PList(ParentNode)^.DelimiterType) then
      begin
        Token := Token^.NextToken;

        if (Assigned(Token)) then
        begin
          Child := PChild(Token);

          while (Assigned(Child) and (Child^.ParentNode <> ParentNode)) do
            if (not Parser.IsChild(Child^.ParentNode)) then
              Child := nil
            else
              Child := PChild(Child^.ParentNode);

          Result := Child;
        end;
      end;
  end;
end;

function TSQLParser.TChild.GetParentNode(): PNode;
begin
  Assert(FParentNode < Parser.Nodes.UsedSize);

  Result := Parser.NodePtr(FParentNode);
end;

{ TSQLParser.TToken ***********************************************************}

class function TSQLParser.TToken.Create(const AParser: TSQLParser;
  const AText: PChar; const ALength: Integer;
  const AErrorCode: Byte; const AErrorPos: PChar;
  const ATokenType: TTokenType; const AOperatorType: TOperatorType;
  const AKeywordIndex: TWordList.TIndex; const AUsageType: TUsageType): TOffset;
begin
  Result := TChild.Create(AParser, ntToken);

  with PToken(AParser.NodePtr(Result))^ do
  begin
    FErrorCode := AErrorCode;
    FErrorPos := AErrorPos;
    {$IFDEF Debug}
    FIndex := 0;
    {$ENDIF}
    FKeywordIndex := AKeywordIndex;
    FOperatorType := AOperatorType;
    FLength := ALength;
    FText := AText;
    FTokenType := ATokenType;
    FUsageType := AUsageType;
    NewText := 0;
    NewTokenType := ttUnknown;
  end;
end;

function TSQLParser.TToken.GetAsString(): string;
var
  Length: Integer;
  S: string;
  Text: PChar;
begin
  GetText(Text, Length);
  SetString(S, Text, Length);
  case (TokenType) of
    ttSLComment:
      if (Copy(S, 1, 1) = '#') then
        Result := Trim(Copy(S, Length - 1, 1))
      else if (Copy(S, 1, 3) = '-- ') then
        Result := Trim(Copy(S, 4, Length - 3))
      else
        raise Exception.Create(SUnknownError);
    ttMLComment:
      if ((Copy(S, 1, 2) = '/*') and (Copy(S, Length - 1, 2) = '*/')) then
        Result := Trim(Copy(S, 3, Length - 4))
      else
        raise Exception.Create(SUnknownError);
    ttString:
      Result := SQLUnescape(S);
    ttDQIdent:
      Result := SQLUnescape(S);
    ttMySQLIdent:
      Result := SQLUnescape(S);
    ttMySQLCodeStart:
      Result := Copy(S, 1, Length - 3);
    else
      Result := S;
  end;
end;

function TSQLParser.TToken.GetDbIdentType(): TDbIdentType;
begin
  if ((OperatorType = otDot)
    or not Assigned(Heritage.ParentNode)
    or (Heritage.ParentNode^.NodeType <> ntDbIdent)) then
    Result := ditUnknown
  else if (PDbIdent(Heritage.ParentNode)^.Nodes.DatabaseIdent = Offset) then
    Result := ditDatabase
  else if (PDbIdent(Heritage.ParentNode)^.Nodes.TableIdent = Offset) then
    Result := ditTable
  else
    Result := PDbIdent(Heritage.ParentNode)^.DbIdentType;
end;

function TSQLParser.TToken.GetGeneration(): Integer;
var
  Node: PNode;
begin
  Result := 0;
  Node := ParentNode;
  while (Parser.IsChild(Node)) do
  begin
    Inc(Result);
    Node := PChild(Node)^.ParentNode;
  end;
end;

{$IFNDEF Debug}
function TSQLParser.TToken.GetIndex(): Integer;
var
  Token: PToken;
begin
  Token := Parser.Root^.FirstTokenAll;
  Result := 0;
  while (Assigned(Token) and (Token <> @Self)) do
  begin
    Inc(Result);
    Token := Token^.NextToken;
  end;
end;
{$ENDIF}

function TSQLParser.TToken.GetIsUsed(): Boolean;
begin
  Result := not (TokenType in [ttSpace, ttReturn, ttSLComment, ttMLComment, ttMySQLCodeStart, ttMySQLCodeEnd])
    and ((Parser.AllowedMySQLVersion = 0) or (Parser.MySQLVersion >= Parser.AllowedMySQLVersion));
end;

function TSQLParser.TToken.GetNextToken(): PToken;
var
  Offset: TOffset;
begin
  Offset := PNode(@Self)^.Offset;
  repeat
    repeat
      Inc(Offset, Parser.NodeSize(Parser.NodePtr(Offset)^.NodeType));
    until ((Offset = Parser.Nodes.UsedSize) or (Parser.NodePtr(Offset)^.NodeType = ntToken));
    if (Offset = Parser.Nodes.UsedSize) then
      Result := nil
    else
      Result := PToken(Parser.NodePtr(Offset));
  until (not Assigned(Result) or Result^.IsUsed);
end;

function TSQLParser.TToken.GetNextTokenAll(): PToken;
var
  Offset: TOffset;
begin
  Offset := PNode(@Self)^.Offset;
  repeat
    repeat
      Inc(Offset, Parser.NodeSize(Parser.NodePtr(Offset)^.NodeType));
    until ((Offset = Parser.Nodes.UsedSize) or (Parser.NodePtr(Offset)^.NodeType = ntToken));
    if (Offset = Parser.Nodes.UsedSize) then
      Result := nil
    else
      Result := PToken(Parser.NodePtr(Offset));
  until (not Assigned(Result) or Parser.IsToken(Offset));
end;

function TSQLParser.TToken.GetOffset(): TOffset;
begin
  Result := Heritage.Heritage.GetOffset();
end;

function TSQLParser.TToken.GetParentNode(): PNode;
begin
  Result := Heritage.GetParentNode();
end;

function TSQLParser.TToken.GetText(): string;
var
  Text: PChar;
  Length: Integer;
begin
  GetText(Text, Length);
  SetString(Result, Text, Length);
end;

procedure TSQLParser.TToken.GetText(out Text: PChar; out Length: Integer);
begin
  if (NewText = 0) then
  begin
    Text := FText;
    Length := FLength;
  end
  else
    Parser.GetText(NewText, Text, Length);
end;

procedure TSQLParser.TToken.SetText(const Text: PChar; const Length: Integer);
begin
  NewText := Parser.NewText(Text, Length);
end;

procedure TSQLParser.TToken.SetText(Text: string);
begin
  SetText(PChar(Text), System.Length(Text));
end;

function TSQLParser.TToken.GetTokenType(): TTokenType;
begin
  if (NewTokenType = ttUnknown) then
    Result := FTokenType
  else
    Result := NewTokenType;
end;

procedure TSQLParser.TToken.SetTokenType(ATokenType: TTokenType);
begin
  if (ATokenType <> FTokenType) then
    NewTokenType := ATokenType;
end;

{ TSQLParser.TRange ***********************************************************}

class function TSQLParser.TRange.Create(const AParser: TSQLParser; const ANodeType: TNodeType): TOffset;
begin
  Result := TChild.Create(AParser, ANodeType);

  with PRange(AParser.NodePtr(Result))^ do
  begin
    FFirstToken := 0;
    FLastToken := 0;
  end;
end;

function TSQLParser.TRange.GetFirstToken(): PToken;
begin
  if (FFirstToken = 0) then
    Result := nil
  else
    Result := PToken(Parser.NodePtr(FFirstToken));
end;

function TSQLParser.TRange.GetLastToken(): PToken;
begin
  if (FLastToken = 0) then
    Result := nil
  else
    Result := PToken(Parser.NodePtr(FLastToken));
end;

function TSQLParser.TRange.GetOffset(): TOffset;
begin
  Result := Heritage.Heritage.Offset;
end;

function TSQLParser.TRange.GetParentNode(): PNode;
begin
  Result := PNode(Parser.NodePtr(FParentNode));
end;

procedure TSQLParser.TRange.AddChildren(const Children: POffsetArray; const Count: Integer);
var
  Child: PChild;
  I: Integer;
begin
  for I := 0 to Count - 1 do
    if (Children^[I] > 0) then
    begin
      Child := Parser.ChildPtr(Children^[I]);
      Child^.FParentNode := Offset;

      if ((FFirstToken = 0) or (0 < Child^.FFirstToken) and (Child^.FFirstToken < FFirstToken)) then
        FFirstToken := Child^.FFirstToken;
      if ((FLastToken = 0) or (Child^.FLastToken > FLastToken)) then
        FLastToken := Child^.FLastToken;
    end;
end;

{ TSQLParser.TRoot ************************************************************}

class function TSQLParser.TRoot.Create(const AParser: TSQLParser;
  const AErrorCode: Byte; const AErrorMessage: TOffset;
  const AFirstTokenAll, ALastTokenAll: TOffset;
  const ChildCount: Integer; const Children: array of TOffset): TOffset;
var
  I: Integer;
begin
  Result := TNode.Create(AParser, ntRoot);

  with PRoot(AParser.NodePtr(Result))^ do
  begin
    FErrorCode := AErrorCode;
    FErrorMessage := AErrorMessage;
    FFirstStmt := 0;
    FFirstTokenAll := AFirstTokenAll;
    FLastStmt := 0;
    FLastTokenAll := ALastTokenAll;

    for I := 0 to ChildCount - 1 do
    begin
      if (AParser.IsChild(Children[I])) then
        AParser.ChildPtr(Children[I])^.FParentNode := Result;
      if (AParser.IsStmt(Children[I])) then
      begin
        if (FFirstStmt = 0) then
          FFirstStmt := Children[I];
        FLastStmt := Children[I];
      end;
    end;
  end;
end;

function TSQLParser.TRoot.GetErrorMessage(): string;
var
  Length: Integer;
  Text: PChar;
begin
  if (FErrorMessage = 0) then
    Result := ''
  else
  begin
    Parser.GetText(FErrorMessage, Text, Length);
    SetString(Result, Text, Length);
  end;
end;

function TSQLParser.TRoot.GetFirstStmt(): PStmt;
begin
  Result := PStmt(Parser.NodePtr(FFirstStmt));
end;

function TSQLParser.TRoot.GetFirstTokenAll(): PToken;
begin
  Result := PToken(Parser.NodePtr(FFirstTokenAll));
end;

function TSQLParser.TRoot.GetLastStmt(): PStmt;
begin
  Result := PStmt(Parser.NodePtr(FLastStmt));
end;

function TSQLParser.TRoot.GetLastTokenAll(): PToken;
begin
  Result := PToken(Parser.NodePtr(FLastTokenAll));
end;

{ TSQLParser.TStmt ************************************************************}

class function TSQLParser.TStmt.Create(const AParser: TSQLParser; const AStmtType: TStmtType): TOffset;
begin
  Result := TRange.Create(AParser, NodeTypeByStmtType[AStmtType]);

  with PStmt(AParser.NodePtr(Result))^ do
  begin
    FStmtType := AStmtType;
    FErrorCode := PE_Success;
    FErrorMessage := 0;
    FErrorToken := 0;
    FFirstTokenAll := 0;
    FLastTokenAll := 0;
  end;
end;

function TSQLParser.TStmt.GetDelimiter(): PToken;
var
  Token: PToken;
begin
  Result := nil;
  if (ParentNode^.NodeType = ntRoot) then
  begin
    Token := LastToken^.NextToken;

    if (Assigned(Token) and (Token^.TokenType = ttSemicolon)) then
      Result := Token;
  end;
end;

function TSQLParser.TStmt.GetErrorMessage(): string;
var
  Length: Integer;
  Text: PChar;
begin
  if (FErrorMessage = 0) then
    Result := ''
  else
  begin
    Parser.GetText(FErrorMessage, Text, Length);
    SetString(Result, Text, Length);
  end;
end;

function TSQLParser.TStmt.GetErrorToken(): PToken;
begin
  Result := PToken(Parser.NodePtr(FErrorToken));
end;

function TSQLParser.TStmt.GetFirstToken(): PToken;
begin
  Result := PToken(Parser.NodePtr(FFirstToken));
end;

function TSQLParser.TStmt.GetFirstTokenAll(): PToken;
begin
  Assert(FFirstTokenAll > 0);

  Result := PToken(Parser.NodePtr(FFirstTokenAll));
end;

function TSQLParser.TStmt.GetLastToken(): PToken;
begin
  Result := PRange(@Self)^.LastToken;
end;

function TSQLParser.TStmt.GetLastTokenAll(): PToken;
begin
  Assert(FLastTokenAll > 0);

  Result := PToken(Parser.NodePtr(FLastTokenAll));
end;

function TSQLParser.TStmt.GetNextStmt(): PStmt;
var
  Token: PToken;
  Child: PChild;
begin
  if (Heritage.FParentNode <> Parser.FRoot) then
    Result := nil
  else
  begin
    Token := LastTokenAll;
    while (Assigned(Token) and (Token^.TokenType <> ttSemicolon)) do
      Token := Token^.NextToken;
    while (Assigned(Token) and (Token^.TokenType = ttSemicolon)) do
      Token := Token^.NextToken;

    Child := PChild(Token);
    while (Assigned(Child) and (Child^.FParentNode <> Parser.FRoot)) do
      Child := PChild(Child^.ParentNode);

    if (not Assigned(Child)) then
      Result := nil
    else
      Result := PStmt(Child);
  end;
end;

function TSQLParser.TStmt.GetText(): string;
var
  Length: Integer;
  StringBuffer: TStringBuffer;
  Text: PChar;
  Token: PToken;
begin
  StringBuffer := TStringBuffer.Create(1024);

  Token := FirstTokenAll;
  while (Assigned(Token)) do
  begin
    Token^.GetText(Text, Length);
    StringBuffer.Write(Text, Length);

    if (Token = LastTokenAll) then
      Token := nil
    else
      Token := Token^.NextTokenAll;
  end;

  Result := StringBuffer.Read();

  StringBuffer.Free();
end;

function TSQLParser.TStmt.GetParentNode(): PNode;
begin
  Result := PNode(Heritage.ParentNode);
end;

{ TSQLParser.TAnalyzeTableStmt ************************************************}

class function TSQLParser.TAnalyzeTableStmt.Create(const AParser: TSQLParser; const ANodes: TNodes): TOffset;
begin
  Result := TStmt.Create(AParser, stAnalyzeTable);

  with PAnalyzeTableStmt(AParser.NodePtr(Result))^ do
  begin
    Nodes := ANodes;

    Heritage.Heritage.AddChildren(@Nodes, SizeOf(Nodes) div SizeOf(TOffset));
  end;
end;

{ TSQLParser.TAlterDatabase ***************************************************}

class function TSQLParser.TAlterDatabaseStmt.Create(const AParser: TSQLParser; const ANodes: TNodes): TOffset;
begin
  Result := TStmt.Create(AParser, stAlterDatabase);

  with PAlterDatabaseStmt(AParser.NodePtr(Result))^ do
  begin
    Nodes := ANodes;

    Heritage.Heritage.AddChildren(@Nodes, SizeOf(Nodes) div SizeOf(TOffset));
  end;
end;

{ TSQLParser.TAlterEvent ******************************************************}

class function TSQLParser.TAlterEventStmt.Create(const AParser: TSQLParser; const ANodes: TNodes): TOffset;
begin
  Result := TStmt.Create(AParser, stAlterEvent);

  with PAlterEventStmt(AParser.NodePtr(Result))^ do
  begin
    Nodes := ANodes;

    Heritage.Heritage.AddChildren(@Nodes, SizeOf(Nodes) div SizeOf(TOffset));
  end;
end;

{ TSQLParser.TAlterRoutine ****************************************************}

class function TSQLParser.TAlterInstanceStmt.Create(const AParser: TSQLParser; const ANodes: TNodes): TOffset;
begin
  Result := TStmt.Create(AParser, stAlterInstance);

  with PAlterInstanceStmt(AParser.NodePtr(Result))^ do
  begin
    Nodes := ANodes;

    Heritage.Heritage.AddChildren(@Nodes, SizeOf(Nodes) div SizeOf(TOffset));
  end;
end;

{ TSQLParser.TAlterRoutine ****************************************************}

class function TSQLParser.TAlterRoutineStmt.Create(const AParser: TSQLParser; const ARoutineType: TRoutineType; const ANodes: TNodes): TOffset;
begin
  Result := TStmt.Create(AParser, stAlterRoutine);

  with PAlterRoutineStmt(AParser.NodePtr(Result))^ do
  begin
    Nodes := ANodes;
    FRoutineType := ARoutineType;

    Heritage.Heritage.AddChildren(@Nodes, SizeOf(Nodes) div SizeOf(TOffset));
  end;
end;

{ TSQLParser.TAlterServerStmt *************************************************}

class function TSQLParser.TAlterServerStmt.Create(const AParser: TSQLParser; const ANodes: TNodes): TOffset;
begin
  Result := TStmt.Create(AParser, stAlterServer);

  with PAlterServerStmt(AParser.NodePtr(Result))^ do
  begin
    Nodes := ANodes;

    Heritage.Heritage.AddChildren(@Nodes, SizeOf(Nodes) div SizeOf(TOffset));
  end;
end;

{ TSQLParser.TAlterTablespaceStmt *********************************************}

class function TSQLParser.TAlterTablespaceStmt.Create(const AParser: TSQLParser; const ANodes: TNodes): TOffset;
begin
  Result := TStmt.Create(AParser, stAlterTablespace);

  with PAlterTablespaceStmt(AParser.NodePtr(Result))^ do
  begin
    Nodes := ANodes;

    Heritage.Heritage.AddChildren(@Nodes, SizeOf(Nodes) div SizeOf(TOffset));
  end;
end;

{ TSQLParser.TAlterTableStmt.TAlterColumn *************************************}

class function TSQLParser.TAlterTableStmt.TAlterColumn.Create(const AParser: TSQLParser; const ANodes: TAlterColumn.TNodes): TOffset;
begin
  Result := TRange.Create(AParser, ntAlterTableStmtAlterColumn);

  with PAlterColumn(AParser.NodePtr(Result))^ do
  begin
    Nodes := ANodes;

    Heritage.AddChildren(@Nodes, SizeOf(Nodes) div SizeOf(TOffset));
  end;
end;

{ TSQLParser.TAlterTableStmt.TConvertTo ***************************************}

class function TSQLParser.TAlterTableStmt.TConvertTo.Create(const AParser: TSQLParser; const ANodes: TConvertTo.TNodes): TOffset;
begin
  Result := TRange.Create(AParser, ntAlterTableStmtConvertTo);

  with PConvertTo(AParser.NodePtr(Result))^ do
  begin
    Nodes := ANodes;

    Heritage.AddChildren(@Nodes, SizeOf(Nodes) div SizeOf(TOffset));
  end;
end;

{ TSQLParser.TAlterTableStmt.TDropObject **************************************}

class function TSQLParser.TAlterTableStmt.TDropObject.Create(const AParser: TSQLParser; const ANodes: TDropObject.TNodes): TOffset;
begin
  Result := TRange.Create(AParser, ntAlterTableStmtDropObject);

  with PDropObject(AParser.NodePtr(Result))^ do
  begin
    Nodes := ANodes;

    Heritage.AddChildren(@Nodes, SizeOf(Nodes) div SizeOf(TOffset));
  end;
end;

{ TSQLParser.TAlterTableStmt.TExchangePartition *******************************}

class function TSQLParser.TAlterTableStmt.TExchangePartition.Create(const AParser: TSQLParser; const ANodes: TExchangePartition.TNodes): TOffset;
begin
  Result := TRange.Create(AParser, ntAlterTableStmtExchangePartition);

  with PExchangePartition(AParser.NodePtr(Result))^ do
  begin
    Nodes := ANodes;

    Heritage.AddChildren(@Nodes, SizeOf(Nodes) div SizeOf(TOffset));
  end;
end;

{ TSQLParser.TAlterTableStmt.TOrderBy ***************************}

class function TSQLParser.TAlterTableStmt.TOrderBy.Create(const AParser: TSQLParser; const ANodes: TOrderBy.TNodes): TOffset;
begin
  Result := TRange.Create(AParser, ntAlterTableStmtOrderBy);

  with POrderBy(AParser.NodePtr(Result))^ do
  begin
    Nodes := ANodes;

    Heritage.AddChildren(@Nodes, SizeOf(Nodes) div SizeOf(TOffset));
  end;
end;

{ TSQLParser.TAlterTableStmt.TReorganizePartition ***************************}

class function TSQLParser.TAlterTableStmt.TReorganizePartition.Create(const AParser: TSQLParser; const ANodes: TReorganizePartition.TNodes): TOffset;
begin
  Result := TRange.Create(AParser, ntAlterTableStmtReorganizePartition);

  with PReorganizePartition(AParser.NodePtr(Result))^ do
  begin
    Nodes := ANodes;

    Heritage.AddChildren(@Nodes, SizeOf(Nodes) div SizeOf(TOffset));
  end;
end;

{ TSQLParser.TAlterTableStmt **************************************************}

class function TSQLParser.TAlterTableStmt.Create(const AParser: TSQLParser; const ANodes: TNodes): TOffset;
begin
  Result := TStmt.Create(AParser, stAlterTable);

  with PAlterTableStmt(AParser.NodePtr(Result))^ do
  begin
    Nodes := ANodes;

    Heritage.Heritage.AddChildren(@Nodes, SizeOf(Nodes) div SizeOf(TOffset));
  end;
end;

{ TSQLParser.TAlterViewStmt ***************************************************}

class function TSQLParser.TAlterViewStmt.Create(const AParser: TSQLParser; const ANodes: TNodes): TOffset;
begin
  Result := TStmt.Create(AParser, stAlterView);

  with PAlterViewStmt(AParser.NodePtr(Result))^ do
  begin
    Nodes := ANodes;

    Heritage.Heritage.AddChildren(@Nodes, SizeOf(Nodes) div SizeOf(TOffset));
  end;
end;

{ TSQLParser.TBeginLabel ******************************************************}

class function TSQLParser.TBeginLabel.Create(const AParser: TSQLParser; const ANodes: TNodes): TOffset;
begin
  Result := TRange.Create(AParser, ntBeginLabel);

  with PBeginLabel(AParser.NodePtr(Result))^ do
  begin
    Nodes := ANodes;

    Heritage.AddChildren(@Nodes, SizeOf(Nodes) div SizeOf(TOffset));
  end;
end;

function TSQLParser.TBeginLabel.GetLabelName(): string;
begin
  if ((Nodes.BeginToken = 0) or (Parser.NodePtr(Nodes.BeginToken)^.NodeType <> ntToken)) then
    Result := ''
  else
    Result := Parser.TokenPtr(Nodes.BeginToken)^.AsString;
end;

{ TSQLParser.TBeginStmt *******************************************************}

class function TSQLParser.TBeginStmt.Create(const AParser: TSQLParser; const ANodes: TNodes): TOffset;
begin
  Result := TStmt.Create(AParser, stBegin);

  with PBeginStmt(AParser.NodePtr(Result))^ do
  begin
    Nodes := ANodes;

    Heritage.Heritage.AddChildren(@Nodes, SizeOf(Nodes) div SizeOf(TOffset));
  end;
end;

{ TSQLParser.TBinaryOp ********************************************************}

class function TSQLParser.TBinaryOp.Create(const AParser: TSQLParser; const AOperator, AOperand1, AOperand2: TOffset): TOffset;
begin
  Result := TRange.Create(AParser, ntBinaryOp);

  with PBinaryOp(AParser.NodePtr(Result))^ do
  begin
    Nodes.OperatorToken := AOperator;
    Nodes.Operand1 := AOperand1;
    Nodes.Operand2 := AOperand2;

    Heritage.AddChildren(@Nodes, SizeOf(Nodes) div SizeOf(TOffset));
  end;
end;

function TSQLParser.TBinaryOp.GetOperand1(): PChild;
begin
  Result := Parser.ChildPtr(Nodes.Operand1);
end;

function TSQLParser.TBinaryOp.GetOperand2(): PChild;
begin
  Result := Parser.ChildPtr(Nodes.Operand2);
end;

function TSQLParser.TBinaryOp.GetOperator(): PChild;
begin
  Result := Parser.ChildPtr(Nodes.OperatorToken);
end;

{ TSQLParser.TBetweenOp *******************************************************}

class function TSQLParser.TBetweenOp.Create(const AParser: TSQLParser; const AExpr, ANotToken, ABetweenToken, AMin, AAndToken, AMax: TOffset): TOffset;
begin
  Result := TRange.Create(AParser, ntBetweenOp);

  with PBetweenOp(AParser.NodePtr(Result))^ do
  begin
    Nodes.Expr := AExpr;
    Nodes.NotToken := ANotToken;
    Nodes.BetweenToken := ABetweenToken;
    Nodes.Min := AMin;
    Nodes.AndToken := AAndToken;
    Nodes.Max := AMax;

    Heritage.AddChildren(@Nodes, SizeOf(Nodes) div SizeOf(TOffset));
  end;
end;

{ TSQLParser.TCallStmt ********************************************************}

class function TSQLParser.TCallStmt.Create(const AParser: TSQLParser; const ANodes: TNodes): TOffset;
begin
  Result := TStmt.Create(AParser, stCall);

  with PCallStmt(AParser.NodePtr(Result))^ do
  begin
    Nodes := ANodes;

    Heritage.Heritage.AddChildren(@Nodes, SizeOf(Nodes) div SizeOf(TOffset));
  end;
end;

{ TSQLParser.TCaseOp.TBranch **************************************************}

class function TSQLParser.TCaseOp.TBranch.Create(const AParser: TSQLParser; const ANodes: TBranch.TNodes): TOffset;
begin
  Result := TRange.Create(AParser, ntCaseOpBranch);

  with PBranch(AParser.NodePtr(Result))^ do
  begin
    Nodes := ANodes;

    Heritage.AddChildren(@Nodes, SizeOf(Nodes) div SizeOf(TOffset));
  end;
end;

{ TSQLParser.TCaseOp **********************************************************}

class function TSQLParser.TCaseOp.Create(const AParser: TSQLParser; const ANodes: TNodes): TOffset;
begin
  Result := TRange.Create(AParser, ntCaseOp);

  with PCaseOp(AParser.NodePtr(Result))^ do
  begin
    Nodes := ANodes;

    Heritage.AddChildren(@Nodes, SizeOf(Nodes) div SizeOf(TOffset));
  end;
end;

{ TSQLParser.TCaseStmt ********************************************************}

class function TSQLParser.TCaseStmt.Create(const AParser: TSQLParser; const ANodes: TNodes): TOffset;
begin
  Result := TStmt.Create(AParser, stCase);

  with PCaseStmt(AParser.NodePtr(Result))^ do
  begin
    Nodes := ANodes;

    Heritage.Heritage.AddChildren(@Nodes, SizeOf(Nodes) div SizeOf(TOffset));
  end;
end;

{ TSQLParser.TCaseStmt.TBranch ************************************************}

class function TSQLParser.TCaseStmt.TBranch.Create(const AParser: TSQLParser; const ANodes: TBranch.TNodes): TOffset;
begin
  Result := TRange.Create(AParser, ntCaseStmtBranch);

  with PBranch(AParser.NodePtr(Result))^ do
  begin
    Nodes := ANodes;

    Heritage.AddChildren(@Nodes, SizeOf(Nodes) div SizeOf(TOffset));
  end;
end;

{ TSQLParser.TCastFunc ********************************************************}

class function TSQLParser.TCastFunc.Create(const AParser: TSQLParser; const ANodes: TNodes): TOffset;
begin
  Result := TRange.Create(AParser, ntCastFunc);

  with PCastFunc(AParser.NodePtr(Result))^ do
  begin
    Nodes := ANodes;

    Heritage.AddChildren(@Nodes, SizeOf(Nodes) div SizeOf(TOffset));
  end;
end;

{ TSQLParser.TChangeMasterStmt ************************************************}

class function TSQLParser.TChangeMasterStmt.Create(const AParser: TSQLParser; const ANodes: TNodes): TOffset;
begin
  Result := TStmt.Create(AParser, stChangeMaster);

  with PChangeMasterStmt(AParser.NodePtr(Result))^ do
  begin
    Nodes := ANodes;

    Heritage.Heritage.AddChildren(@Nodes, SizeOf(Nodes) div SizeOf(TOffset));
  end;
end;

{ TSQLParser.TCharFunc ********************************************************}

class function TSQLParser.TCharFunc.Create(const AParser: TSQLParser; const ANodes: TNodes): TOffset;
begin
  Result := TRange.Create(AParser, ntCharFunc);

  with PCharFunc(AParser.NodePtr(Result))^ do
  begin
    Nodes := ANodes;

    Heritage.AddChildren(@Nodes, SizeOf(Nodes) div SizeOf(TOffset));
  end;
end;

{ TSQLParser.TCheckTableStmt **************************************************}

class function TSQLParser.TCheckTableStmt.Create(const AParser: TSQLParser; const ANodes: TNodes): TOffset;
begin
  Result := TStmt.Create(AParser, stCheckTable);

  with PCheckTableStmt(AParser.NodePtr(Result))^ do
  begin
    Nodes := ANodes;

    Heritage.Heritage.AddChildren(@Nodes, SizeOf(Nodes) div SizeOf(TOffset));
  end;
end;

{ TSQLParser.TCheckStmt.TOption ***********************************************}

class function TSQLParser.TCheckTableStmt.TOption.Create(const AParser: TSQLParser; const ANodes: TOption.TNodes): TOffset;
begin
  Result := TRange.Create(AParser, ntCheckTableStmtOption);

  with POption(AParser.NodePtr(Result))^ do
  begin
    Nodes := ANodes;

    Heritage.AddChildren(@Nodes, SizeOf(Nodes) div SizeOf(TOffset));
  end;
end;

{ TSQLParser.TChecksumTableStmt ***********************************************}

class function TSQLParser.TChecksumTableStmt.Create(const AParser: TSQLParser; const ANodes: TNodes): TOffset;
begin
  Result := TStmt.Create(AParser, stChecksumTable);

  with PChecksumTableStmt(AParser.NodePtr(Result))^ do
  begin
    Nodes := ANodes;

    Heritage.Heritage.AddChildren(@Nodes, SizeOf(Nodes) div SizeOf(TOffset));
  end;
end;

{ TSQLParser.TCloseStmt *******************************************************}

class function TSQLParser.TCloseStmt.Create(const AParser: TSQLParser; const ANodes: TNodes): TOffset;
begin
  Result := TStmt.Create(AParser, stClose);

  with PCloseStmt(AParser.NodePtr(Result))^ do
  begin
    Nodes := ANodes;

    Heritage.Heritage.AddChildren(@Nodes, SizeOf(Nodes) div SizeOf(TOffset));
  end;
end;

{ TSQLParser.TCommitStmt ******************************************************}

class function TSQLParser.TCommitStmt.Create(const AParser: TSQLParser; const ANodes: TNodes): TOffset;
begin
  Result := TStmt.Create(AParser, stCommit);

  with PCommitStmt(AParser.NodePtr(Result))^ do
  begin
    Nodes := ANodes;

    Heritage.Heritage.AddChildren(@Nodes, SizeOf(Nodes) div SizeOf(TOffset));
  end;
end;

{ TSQLParser.TCompoundStmt ****************************************************}

class function TSQLParser.TCompoundStmt.Create(const AParser: TSQLParser; const ANodes: TNodes): TOffset;
begin
  Result := TStmt.Create(AParser, stCompound);

  with PCompoundStmt(AParser.NodePtr(Result))^ do
  begin
    Nodes := ANodes;

    Heritage.Heritage.AddChildren(@Nodes, SizeOf(Nodes) div SizeOf(TOffset));
  end;
end;

{ TSQLParser.TConvertFunc *****************************************************}

class function TSQLParser.TConvertFunc.Create(const AParser: TSQLParser; const ANodes: TNodes): TOffset;
begin
  Result := TRange.Create(AParser, ntConvertFunc);

  with PConvertFunc(AParser.NodePtr(Result))^ do
  begin
    Nodes := ANodes;

    Heritage.AddChildren(@Nodes, SizeOf(Nodes) div SizeOf(TOffset));
  end;
end;

{ TSQLParser.TCreateDatabaseStmt **********************************************}

class function TSQLParser.TCreateDatabaseStmt.Create(const AParser: TSQLParser; const ANodes: TNodes): TOffset;
begin
  Result := TStmt.Create(AParser, stCreateDatabase);

  with PCreateDatabaseStmt(AParser.NodePtr(Result))^ do
  begin
    Nodes := ANodes;

    Heritage.Heritage.AddChildren(@Nodes, SizeOf(Nodes) div SizeOf(TOffset));
  end;
end;

{ TSQLParser.TCreateEventStmt *************************************************}

class function TSQLParser.TCreateEventStmt.Create(const AParser: TSQLParser; const ANodes: TNodes): TOffset;
begin
  Result := TStmt.Create(AParser, stCreateEvent);

  with PCreateEventStmt(AParser.NodePtr(Result))^ do
  begin
    Nodes := ANodes;

    Heritage.Heritage.AddChildren(@Nodes, SizeOf(Nodes) div SizeOf(TOffset));
  end;
end;

{ TSQLParser.TCreateIndexStmt *************************************************}

class function TSQLParser.TCreateIndexStmt.Create(const AParser: TSQLParser; const ANodes: TNodes): TOffset;
begin
  Result := TStmt.Create(AParser, stCreateIndex);

  with PCreateIndexStmt(AParser.NodePtr(Result))^ do
  begin
    Nodes := ANodes;

    Heritage.Heritage.AddChildren(@Nodes, SizeOf(Nodes) div SizeOf(TOffset));
  end;
end;

{ TSQLParser.TCreateRoutineStmt ***********************************************}

class function TSQLParser.TCreateRoutineStmt.Create(const AParser: TSQLParser; const ARoutineType: TRoutineType; const ANodes: TNodes): TOffset;
begin
  Result := TStmt.Create(AParser, stCreateRoutine);

  with PCreateRoutineStmt(AParser.NodePtr(Result))^ do
  begin
    Nodes := ANodes;
    FRoutineType := ARoutineType;

    Heritage.Heritage.AddChildren(@Nodes, SizeOf(Nodes) div SizeOf(TOffset));
  end;
end;

{ TSQLParser.TCreateServerStmt ************************************************}

class function TSQLParser.TCreateServerStmt.Create(const AParser: TSQLParser; const ANodes: TNodes): TOffset;
begin
  Result := TStmt.Create(AParser, stCreateServer);

  with PCreateServerStmt(AParser.NodePtr(Result))^ do
  begin
    Nodes := ANodes;

    Heritage.Heritage.AddChildren(@Nodes, SizeOf(Nodes) div SizeOf(TOffset));
  end;
end;

{ TSQLParser.TCreateTablespaceStmt ********************************************}

class function TSQLParser.TCreateTablespaceStmt.Create(const AParser: TSQLParser; const ANodes: TNodes): TOffset;
begin
  Result := TStmt.Create(AParser, stCreateTablespace);

  with PCreateTablespaceStmt(AParser.NodePtr(Result))^ do
  begin
    Nodes := ANodes;

    Heritage.Heritage.AddChildren(@Nodes, SizeOf(Nodes) div SizeOf(TOffset));
  end;
end;

{ TSQLParser.TCreateTableStmt *************************************************}

class function TSQLParser.TCreateTableStmt.Create(const AParser: TSQLParser; const ANodes: TNodes): TOffset;
begin
  Result := TStmt.Create(AParser, stCreateTable);

  with PCreateTableStmt(AParser.NodePtr(Result))^ do
  begin
    Nodes := ANodes;

    Heritage.Heritage.AddChildren(@Nodes, SizeOf(Nodes) div SizeOf(TOffset));
  end;
end;

{ TSQLParser.TCreateTableStmt.TCheck ******************************************}

class function TSQLParser.TCreateTableStmt.TCheck.Create(const AParser: TSQLParser; const ANodes: TCheck.TNodes): TOffset;
begin
  Result := TRange.Create(AParser, ntCreateTableStmtFieldDefaultFunc);

  with PCheck(AParser.NodePtr(Result))^ do
  begin
    Nodes := ANodes;

    Heritage.AddChildren(@Nodes, SizeOf(Nodes) div SizeOf(TOffset));
  end;
end;

{ TSQLParser.TCreateTableStmt.TField.TDefaultFunc ***************************}

class function TSQLParser.TCreateTableStmt.TField.TDefaultFunc.Create(const AParser: TSQLParser; const ANodes: TDefaultFunc.TNodes): TOffset;
begin
  Result := TRange.Create(AParser, ntCreateTableStmtFieldDefaultFunc);

  with PDefaultFunc(AParser.NodePtr(Result))^ do
  begin
    Nodes := ANodes;

    Heritage.AddChildren(@Nodes, SizeOf(Nodes) div SizeOf(TOffset));
  end;
end;

{ TSQLParser.TCreateTableStmt.TField ******************************************}

class function TSQLParser.TCreateTableStmt.TField.Create(const AParser: TSQLParser; const ANodes: TField.TNodes): TOffset;
begin
  Result := TRange.Create(AParser, ntCreateTableStmtField);

  with PField(AParser.NodePtr(Result))^ do
  begin
    Nodes := ANodes;

    Heritage.AddChildren(@Nodes, SizeOf(Nodes) div SizeOf(TOffset));
  end;
end;

{ TSQLParser.TCreateTableStmt.TForeignKey *************************************}

class function TSQLParser.TCreateTableStmt.TForeignKey.Create(const AParser: TSQLParser; const ANodes: TForeignKey.TNodes): TOffset;
begin
  Result := TRange.Create(AParser, ntCreateTableStmtForeignKey);

  with PForeignKey(AParser.NodePtr(Result))^ do
  begin
    Nodes := ANodes;

    Heritage.AddChildren(@Nodes, SizeOf(Nodes) div SizeOf(TOffset));
  end;
end;

{ TSQLParser.TCreateTableStmt.TKey ********************************************}

class function TSQLParser.TCreateTableStmt.TKey.Create(const AParser: TSQLParser; const ANodes: TKey.TNodes): TOffset;
begin
  Result := TRange.Create(AParser, ntCreateTableStmtKey);

  with PKey(AParser.NodePtr(Result))^ do
  begin
    Nodes := ANodes;

    Heritage.AddChildren(@Nodes, SizeOf(Nodes) div SizeOf(TOffset));
  end;
end;

{ TSQLParser.TCreateTableStmt.TKeyColumn **************************************}

class function TSQLParser.TCreateTableStmt.TKeyColumn.Create(const AParser: TSQLParser; const ANodes: TKeyColumn.TNodes): TOffset;
begin
  Result := TRange.Create(AParser, ntCreateTableStmtKeyColumn);

  with PKeyColumn(AParser.NodePtr(Result))^ do
  begin
    Nodes := ANodes;

    Heritage.AddChildren(@Nodes, SizeOf(Nodes) div SizeOf(TOffset));
  end;
end;

{ TSQLParser.TCreateTableStmt.TPartition **************************************}

class function TSQLParser.TCreateTableStmt.TPartition.Create(const AParser: TSQLParser; const ANodes: TPartition.TNodes): TOffset;
begin
  Result := TRange.Create(AParser, ntCreateTableStmtPartition);

  with PPartition(AParser.NodePtr(Result))^ do
  begin
    Nodes := ANodes;

    Heritage.AddChildren(@Nodes, SizeOf(Nodes) div SizeOf(TOffset));
  end;
end;

{ TSQLParser.TCreateTableStmt.TReferences *************************************}

class function TSQLParser.TCreateTableStmt.TReference.Create(const AParser: TSQLParser; const ANodes: TReference.TNodes): TOffset;
begin
  Result := TRange.Create(AParser, ntCreateTableStmtReference);

  with PReference(AParser.NodePtr(Result))^ do
  begin
    Nodes := ANodes;

    Heritage.AddChildren(@Nodes, SizeOf(Nodes) div SizeOf(TOffset));
  end;
end;

{ TSQLParser.TCreateTriggerStmt *********************************************}

class function TSQLParser.TCreateTriggerStmt.Create(const AParser: TSQLParser; const ANodes: TNodes): TOffset;
begin
  Result := TStmt.Create(AParser, stCreateTrigger);

  with PCreateTriggerStmt(AParser.NodePtr(Result))^ do
  begin
    Nodes := ANodes;

    Heritage.Heritage.AddChildren(@Nodes, SizeOf(Nodes) div SizeOf(TOffset));
  end;
end;

{ TSQLParser.TCreateUserStmt **************************************************}

class function TSQLParser.TCreateUserStmt.Create(const AParser: TSQLParser; const ANodes: TNodes): TOffset;
begin
  Result := TStmt.Create(AParser, stCreateUser);

  with PCreateUserStmt(AParser.NodePtr(Result))^ do
  begin
    Nodes := ANodes;

    Heritage.Heritage.AddChildren(@Nodes, SizeOf(Nodes) div SizeOf(TOffset));
  end;
end;

{ TSQLParser.TCreateViewStmt **************************************************}

class function TSQLParser.TCreateViewStmt.Create(const AParser: TSQLParser; const ANodes: TNodes): TOffset;
begin
  Result := TStmt.Create(AParser, stCreateView);

  with PCreateViewStmt(AParser.NodePtr(Result))^ do
  begin
    Nodes := ANodes;

    Heritage.Heritage.AddChildren(@Nodes, SizeOf(Nodes) div SizeOf(TOffset));
  end;
end;

{ TSQLParser.TCurrentTimestamp ************************************************}

class function TSQLParser.TCurrentTimestamp.Create(const AParser: TSQLParser; const ANodes: TNodes): TOffset;
begin
  Result := TRange.Create(AParser, ntCurrentTimestamp);

  with PCurrentTimestamp(AParser.NodePtr(Result))^ do
  begin
    Nodes := ANodes;

    Heritage.AddChildren(@Nodes, SizeOf(Nodes) div SizeOf(TOffset));
  end;
end;

{ TSQLParser.TDatatype ********************************************************}

class function TSQLParser.TDatatype.Create(const AParser: TSQLParser; const ANodes: TNodes): TOffset;
begin
  Result := TRange.Create(AParser, ntDatatype);

  with PDatatype(AParser.NodePtr(Result))^ do
  begin
    Nodes := ANodes;

    Heritage.AddChildren(@Nodes, SizeOf(Nodes) div SizeOf(TOffset));
  end;
end;

{ TSQLParser.TDateAddFunc *****************************************************}

class function TSQLParser.TDateAddFunc.Create(const AParser: TSQLParser; const ANodes: TNodes): TOffset;
begin
  Result := TRange.Create(AParser, ntDateAddFunc);

  with PDateAddFunc(AParser.NodePtr(Result))^ do
  begin
    Nodes := ANodes;

    Heritage.AddChildren(@Nodes, SizeOf(Nodes) div SizeOf(TOffset));
  end;
end;

{ TSQLParser.TDbIdent *********************************************************}

class function TSQLParser.TDbIdent.Create(const AParser: TSQLParser;
  const ADbIdentType: TDbIdentType; const ANodes: TNodes): TOffset;
begin
  Result := TRange.Create(AParser, ntDbIdent);

  with PDbIdent(AParser.NodePtr(Result))^ do
  begin
    FDbIdentType := ADbIdentType;

    Nodes := ANodes;

    Heritage.AddChildren(@Nodes, SizeOf(Nodes) div SizeOf(TOffset));
  end;
end;

class function TSQLParser.TDbIdent.Create(const AParser: TSQLParser; const ADbIdentType: TDbIdentType;
  const AIdent, ADatabaseDot, ADatabaseIdent, ATableDot, ATableIdent: TOffset): TOffset;
var
  Nodes: TNodes;
begin
  Nodes.Ident := AIdent;
  Nodes.DatabaseDot := ADatabaseDot;
  Nodes.DatabaseIdent := ADatabaseIdent;
  Nodes.TableDot := ATableDot;
  Nodes.TableIdent := ATableIdent;

  Result := Create(AParser, ADbIdentType, Nodes);
end;

function TSQLParser.TDbIdent.GetDatabaseIdent(): PToken;
begin
  Result := Parser.TokenPtr(Nodes.DatabaseIdent);
end;

function TSQLParser.TDbIdent.GetIdent(): PToken;
begin
  Result := Parser.TokenPtr(Nodes.Ident);
end;

function TSQLParser.TDbIdent.GetParentNode(): PNode;
begin
  Result := Heritage.GetParentNode();
end;

function TSQLParser.TDbIdent.GetTableIdent(): PToken;
begin
  Result := Parser.TokenPtr(Nodes.TableIdent);
end;

{ TSQLParser.TDeallocatePrepareStmt *******************************************}

class function TSQLParser.TDeallocatePrepareStmt.Create(const AParser: TSQLParser; const ANodes: TNodes): TOffset;
begin
  Result := TStmt.Create(AParser, stDeallocatePrepare);

  with PDeallocatePrepareStmt(AParser.NodePtr(Result))^ do
  begin
    Nodes := ANodes;

    Heritage.Heritage.AddChildren(@Nodes, SizeOf(Nodes) div SizeOf(TOffset));
  end;
end;

{ TSQLParser.TDeclareStmt *****************************************************}

class function TSQLParser.TDeclareStmt.Create(const AParser: TSQLParser; const ANodes: TNodes): TOffset;
begin
  Result := TStmt.Create(AParser, stDeclare);

  with PDeclareStmt(AParser.NodePtr(Result))^ do
  begin
    Nodes := ANodes;

    Heritage.Heritage.AddChildren(@Nodes, SizeOf(Nodes) div SizeOf(TOffset));
  end;
end;

{ TSQLParser.TDeclareConditionStmt ********************************************}

class function TSQLParser.TDeclareConditionStmt.Create(const AParser: TSQLParser; const ANodes: TNodes): TOffset;
begin
  Result := TStmt.Create(AParser, stDeclareCondition);

  with PDeclareConditionStmt(AParser.NodePtr(Result))^ do
  begin
    Nodes := ANodes;

    Heritage.Heritage.AddChildren(@Nodes, SizeOf(Nodes) div SizeOf(TOffset));
  end;
end;

{ TSQLParser.TDeclareCursorStmt ***********************************************}

class function TSQLParser.TDeclareCursorStmt.Create(const AParser: TSQLParser; const ANodes: TNodes): TOffset;
begin
  Result := TStmt.Create(AParser, stDeclareCursor);

  with PDeclareCursorStmt(AParser.NodePtr(Result))^ do
  begin
    Nodes := ANodes;

    Heritage.Heritage.AddChildren(@Nodes, SizeOf(Nodes) div SizeOf(TOffset));
  end;
end;

{ TSQLParser.TDeclareHandlerStmt **********************************************}

class function TSQLParser.TDeclareHandlerStmt.Create(const AParser: TSQLParser; const ANodes: TNodes): TOffset;
begin
  Result := TStmt.Create(AParser, stDeclareHandler);

  with PDeclareHandlerStmt(AParser.NodePtr(Result))^ do
  begin
    Nodes := ANodes;

    Heritage.Heritage.AddChildren(@Nodes, SizeOf(Nodes) div SizeOf(TOffset));
  end;
end;

{ TSQLParser.TDeleteStmt ******************************************************}

class function TSQLParser.TDeleteStmt.Create(const AParser: TSQLParser; const ANodes: TNodes): TOffset;
begin
  Result := TStmt.Create(AParser, stDelete);

  with PDeleteStmt(AParser.NodePtr(Result))^ do
  begin
    Nodes := ANodes;

    Heritage.Heritage.AddChildren(@Nodes, SizeOf(Nodes) div SizeOf(TOffset));
  end;
end;

{ TSQLParser.TDoStmt **********************************************************}

class function TSQLParser.TDoStmt.Create(const AParser: TSQLParser; const ANodes: TNodes): TOffset;
begin
  Result := TStmt.Create(AParser, stDo);

  with PDoStmt(AParser.NodePtr(Result))^ do
  begin
    Nodes := ANodes;

    Heritage.Heritage.AddChildren(@Nodes, SizeOf(Nodes) div SizeOf(TOffset));
  end;
end;

{ TSQLParser.TDropDatabaseStmt ************************************************}

class function TSQLParser.TDropDatabaseStmt.Create(const AParser: TSQLParser; const ANodes: TNodes): TOffset;
begin
  Result := TStmt.Create(AParser, stDropDatabase);

  with PDropDatabaseStmt(AParser.NodePtr(Result))^ do
  begin
    Nodes := ANodes;

    Heritage.Heritage.AddChildren(@Nodes, SizeOf(Nodes) div SizeOf(TOffset));
  end;
end;

{ TSQLParser.TDropEventStmt ***************************************************}

class function TSQLParser.TDropEventStmt.Create(const AParser: TSQLParser; const ANodes: TNodes): TOffset;
begin
  Result := TStmt.Create(AParser, stDropEvent);

  with PDropEventStmt(AParser.NodePtr(Result))^ do
  begin
    Nodes := ANodes;

    Heritage.Heritage.AddChildren(@Nodes, SizeOf(Nodes) div SizeOf(TOffset));
  end;
end;

{ TSQLParser.TDropIndexStmt ***************************************************}

class function TSQLParser.TDropIndexStmt.Create(const AParser: TSQLParser; const ANodes: TNodes): TOffset;
begin
  Result := TStmt.Create(AParser, stDropIndex);

  with PDropIndexStmt(AParser.NodePtr(Result))^ do
  begin
    Nodes := ANodes;

    Heritage.Heritage.AddChildren(@Nodes, SizeOf(Nodes) div SizeOf(TOffset));
  end;
end;

{ TSQLParser.TDropRoutineStmt *************************************************}

class function TSQLParser.TDropRoutineStmt.Create(const AParser: TSQLParser; const ARoutineType: TRoutineType; const ANodes: TNodes): TOffset;
begin
  Result := TStmt.Create(AParser, stDropRoutine);

  with PDropRoutineStmt(AParser.NodePtr(Result))^ do
  begin
    Nodes := ANodes;
    FRoutineType := ARoutineType;

    Heritage.Heritage.AddChildren(@Nodes, SizeOf(Nodes) div SizeOf(TOffset));
  end;
end;

{ TSQLParser.TDropServerStmt **************************************************}

class function TSQLParser.TDropServerStmt.Create(const AParser: TSQLParser; const ANodes: TNodes): TOffset;
begin
  Result := TStmt.Create(AParser, stDropServer);

  with PDropServerStmt(AParser.NodePtr(Result))^ do
  begin
    Nodes := ANodes;

    Heritage.Heritage.AddChildren(@Nodes, SizeOf(Nodes) div SizeOf(TOffset));
  end;
end;

{ TSQLParser.TDropTablespaceStmt **********************************************}

class function TSQLParser.TDropTablespaceStmt.Create(const AParser: TSQLParser; const ANodes: TNodes): TOffset;
begin
  Result := TStmt.Create(AParser, stDropTable);

  with PDropTablespaceStmt(AParser.NodePtr(Result))^ do
  begin
    Nodes := ANodes;

    Heritage.Heritage.AddChildren(@Nodes, SizeOf(Nodes) div SizeOf(TOffset));
  end;
end;

{ TSQLParser.TDropTableStmt ***************************************************}

class function TSQLParser.TDropTableStmt.Create(const AParser: TSQLParser; const ANodes: TNodes): TOffset;
begin
  Result := TStmt.Create(AParser, stDropTable);

  with PDropTableStmt(AParser.NodePtr(Result))^ do
  begin
    Nodes := ANodes;

    Heritage.Heritage.AddChildren(@Nodes, SizeOf(Nodes) div SizeOf(TOffset));
  end;
end;

{ TSQLParser.TDropTriggerStmt *************************************************}

class function TSQLParser.TDropTriggerStmt.Create(const AParser: TSQLParser; const ANodes: TNodes): TOffset;
begin
  Result := TStmt.Create(AParser, stDropTrigger);

  with PDropTriggerStmt(AParser.NodePtr(Result))^ do
  begin
    Nodes := ANodes;

    Heritage.Heritage.AddChildren(@Nodes, SizeOf(Nodes) div SizeOf(TOffset));
  end;
end;

{ TSQLParser.TDropUserStmt ****************************************************}

class function TSQLParser.TDropUserStmt.Create(const AParser: TSQLParser; const ANodes: TNodes): TOffset;
begin
  Result := TStmt.Create(AParser, stDropUser);

  with PDropUserStmt(AParser.NodePtr(Result))^ do
  begin
    Nodes := ANodes;

    Heritage.Heritage.AddChildren(@Nodes, SizeOf(Nodes) div SizeOf(TOffset));
  end;
end;

{ TSQLParser.TDropViewStmt ****************************************************}

class function TSQLParser.TDropViewStmt.Create(const AParser: TSQLParser; const ANodes: TNodes): TOffset;
begin
  Result := TStmt.Create(AParser, stDropView);

  with PDropViewStmt(AParser.NodePtr(Result))^ do
  begin
    Nodes := ANodes;

    Heritage.Heritage.AddChildren(@Nodes, SizeOf(Nodes) div SizeOf(TOffset));
  end;
end;

{ TSQLParser.TEndLabel ********************************************************}

class function TSQLParser.TEndLabel.Create(const AParser: TSQLParser; const ANodes: TNodes): TOffset;
begin
  Result := TRange.Create(AParser, ntEndLabel);

  with PEndLabel(AParser.NodePtr(Result))^ do
  begin
    Nodes := ANodes;

    Heritage.AddChildren(@Nodes, SizeOf(Nodes) div SizeOf(TOffset));
  end;
end;

{ TSQLParser.TExecuteStmt *****************************************************}

class function TSQLParser.TExecuteStmt.Create(const AParser: TSQLParser; const ANodes: TNodes): TOffset;
begin
  Result := TStmt.Create(AParser, stExecute);

  with PExecuteStmt(AParser.NodePtr(Result))^ do
  begin
    Nodes := ANodes;

    Heritage.Heritage.AddChildren(@Nodes, SizeOf(Nodes) div SizeOf(TOffset));
  end;
end;

{ TSQLParser.TExistsFunc ******************************************************}

class function TSQLParser.TExistsFunc.Create(const AParser: TSQLParser; const ANodes: TNodes): TOffset;
begin
  Result := TRange.Create(AParser, ntExistsFunc);

  with PExistsFunc(AParser.NodePtr(Result))^ do
  begin
    Nodes := ANodes;

    Heritage.AddChildren(@Nodes, SizeOf(Nodes) div SizeOf(TOffset));
  end;
end;

{ TSQLParser.TExplainStmt *****************************************************}

class function TSQLParser.TExplainStmt.Create(const AParser: TSQLParser; const ANodes: TNodes): TOffset;
begin
  Result := TStmt.Create(AParser, stExplain);

  with PExplainStmt(AParser.NodePtr(Result))^ do
  begin
    Nodes := ANodes;

    Heritage.Heritage.AddChildren(@Nodes, SizeOf(Nodes) div SizeOf(TOffset));
  end;
end;

{ TSQLParser.TExtractFunc *****************************************************}

class function TSQLParser.TExtractFunc.Create(const AParser: TSQLParser; const ANodes: TNodes): TOffset;
begin
  Result := TRange.Create(AParser, ntExtractFunc);

  with PExtractFunc(AParser.NodePtr(Result))^ do
  begin
    Nodes := ANodes;

    Heritage.AddChildren(@Nodes, SizeOf(Nodes) div SizeOf(TOffset));
  end;
end;

{ TSQLParser.TFetchStmt *******************************************************}

class function TSQLParser.TFetchStmt.Create(const AParser: TSQLParser; const ANodes: TNodes): TOffset;
begin
  Result := TStmt.Create(AParser, stFetch);

  with PFetchStmt(AParser.NodePtr(Result))^ do
  begin
    Nodes := ANodes;

    Heritage.Heritage.AddChildren(@Nodes, SizeOf(Nodes) div SizeOf(TOffset));
  end;
end;

{ TSQLParser.TFlushStmt *******************************************************}

class function TSQLParser.TFlushStmt.Create(const AParser: TSQLParser; const ANodes: TNodes): TOffset;
begin
  Result := TStmt.Create(AParser, stFlush);

  with PFlushStmt(AParser.NodePtr(Result))^ do
  begin
    Nodes := ANodes;

    Heritage.Heritage.AddChildren(@Nodes, SizeOf(Nodes) div SizeOf(TOffset));
  end;
end;

{ TSQLParser.TFlushStmtOption *************************************************}

class function TSQLParser.TFlushStmt.TOption.Create(const AParser: TSQLParser; const ANodes: TOption.TNodes): TOffset;
begin
  Result := TRange.Create(AParser, ntFlushStmtOption);

  with POption(AParser.NodePtr(Result))^ do
  begin
    Nodes := ANodes;

    Heritage.AddChildren(@Nodes, SizeOf(Nodes) div SizeOf(TOffset));
  end;
end;

{ TSQLParser.TFunctionCall ****************************************************}

class function TSQLParser.TFunctionCall.Create(const AParser: TSQLParser; const AIdent, AArgumentsList: TOffset): TOffset;
var
  Nodes: TNodes;
begin
  Nodes.Ident := AIdent;
  Nodes.ArgumentsList := AArgumentsList;

  Result := Create(AParser, Nodes);
end;

class function TSQLParser.TFunctionCall.Create(const AParser: TSQLParser; const ANodes: TNodes): TOffset;
begin
  Result := TRange.Create(AParser, ntFunctionCall);

  with PFunctionCall(AParser.NodePtr(Result))^ do
  begin
    Nodes := ANodes;

    Heritage.AddChildren(@Nodes, SizeOf(Nodes) div SizeOf(TOffset));
  end;
end;

function TSQLParser.TFunctionCall.GetArguments(): PChild;
begin
  Result := Parser.ChildPtr(Nodes.ArgumentsList);
end;

function TSQLParser.TFunctionCall.GetIdent(): PChild;
begin
  Result := Parser.ChildPtr(Nodes.Ident);
end;

{ TSQLParser.TFunctionReturns *************************************************}

class function TSQLParser.TFunctionReturns.Create(const AParser: TSQLParser; const ANodes: TFunctionReturns.TNodes): TOffset;
begin
  Result := TRange.Create(AParser, ntFunctionReturns);

  with PFunctionReturns(AParser.NodePtr(Result))^ do
  begin
    Nodes := ANodes;

    Heritage.AddChildren(@Nodes, SizeOf(Nodes) div SizeOf(TOffset));
  end;
end;

{ TSQLParser.TGetDiagnosticsStmt **********************************************}

class function TSQLParser.TGetDiagnosticsStmt.Create(const AParser: TSQLParser; const ANodes: TNodes): TOffset;
begin
  Result := TStmt.Create(AParser, stGetDiagnostics);

  with PGetDiagnosticsStmt(AParser.NodePtr(Result))^ do
  begin
    Nodes := ANodes;

    Heritage.Heritage.AddChildren(@Nodes, SizeOf(Nodes) div SizeOf(TOffset));
  end;
end;

{ TSQLParser.TGetDiagnosticsStmt.TStmtInfo ************************************}

class function TSQLParser.TGetDiagnosticsStmt.TStmtInfo.Create(const AParser: TSQLParser; const ANodes: TStmtInfo.TNodes): TOffset;
begin
  Result := TRange.Create(AParser, ntGetDiagnosticsStmtStmtInfo);

  with PStmtInfo(AParser.NodePtr(Result))^ do
  begin
    Nodes := ANodes;

    Heritage.AddChildren(@Nodes, SizeOf(Nodes) div SizeOf(TOffset));
  end;
end;

{ TSQLParser.TGetDiagnosticsStmt.TConditionalInfo ***************************}

class function TSQLParser.TGetDiagnosticsStmt.TCondInfo.Create(const AParser: TSQLParser; const ANodes: TCondInfo.TNodes): TOffset;
begin
  Result := TRange.Create(AParser, ntGetDiagnosticsStmtStmtInfo);

  with PCondInfo(AParser.NodePtr(Result))^ do
  begin
    Nodes := ANodes;

    Heritage.AddChildren(@Nodes, SizeOf(Nodes) div SizeOf(TOffset));
  end;
end;

{ TSQLParser.TGrantStmt *******************************************************}

class function TSQLParser.TGrantStmt.Create(const AParser: TSQLParser; const ANodes: TNodes): TOffset;
begin
  Result := TStmt.Create(AParser, stGrant);

  with PGrantStmt(AParser.NodePtr(Result))^ do
  begin
    Nodes := ANodes;

    Heritage.Heritage.AddChildren(@Nodes, SizeOf(Nodes) div SizeOf(TOffset));
  end;
end;

{ TSQLParser.TGrantStmt.TPrivileg *********************************************}

class function TSQLParser.TGrantStmt.TPrivileg.Create(const AParser: TSQLParser; const ANodes: TPrivileg.TNodes): TOffset;
begin
  Result := TRange.Create(AParser, ntGrantStmtPrivileg);

  with PPrivileg(AParser.NodePtr(Result))^ do
  begin
    Nodes := ANodes;

    Heritage.AddChildren(@Nodes, SizeOf(Nodes) div SizeOf(TOffset));
  end;
end;

{ TSQLParser.TGrantStmt.TUserSpecification ************************************}

class function TSQLParser.TGrantStmt.TUserSpecification.Create(const AParser: TSQLParser; const ANodes: TUserSpecification.TNodes): TOffset;
begin
  Result := TRange.Create(AParser, ntGrantStmtUserSpecification);

  with PUserSpecification(AParser.NodePtr(Result))^ do
  begin
    Nodes := ANodes;

    Heritage.AddChildren(@Nodes, SizeOf(Nodes) div SizeOf(TOffset));
  end;
end;

{ TSQLParser.TGroupConcatFunc *************************************************}

class function TSQLParser.TGroupConcatFunc.Create(const AParser: TSQLParser; const ANodes: TNodes): TOffset;
begin
  Result := TRange.Create(AParser, ntGroupConcatFunc);

  with PGroupConcatFunc(AParser.NodePtr(Result))^ do
  begin
    Nodes := ANodes;

    Heritage.AddChildren(@Nodes, SizeOf(Nodes) div SizeOf(TOffset));
  end;
end;

{ TSQLParser.TGroupConcatFunc.TExpr *******************************************}

class function TSQLParser.TGroupConcatFunc.TExpr.Create(const AParser: TSQLParser; const ANodes: TExpr.TNodes): TOffset;
begin
  Result := TRange.Create(AParser, ntGroupConcatFuncExpr);

  with PExpr(AParser.NodePtr(Result))^ do
  begin
    Nodes := ANodes;

    Heritage.AddChildren(@Nodes, SizeOf(Nodes) div SizeOf(TOffset));
  end;
end;

{ TSQLParser.THelpStmt ********************************************************}

class function TSQLParser.THelpStmt.Create(const AParser: TSQLParser; const ANodes: THelpStmt.TNodes): TOffset;
begin
  Result := TStmt.Create(AParser, stHelp);

  with PHelpStmt(AParser.NodePtr(Result))^ do
  begin
    Nodes := ANodes;

    Heritage.Heritage.AddChildren(@Nodes, SizeOf(Nodes) div SizeOf(TOffset));
  end;
end;

{ TSQLParser.TIfStmt.TBranch **************************************************}

class function TSQLParser.TIfStmt.TBranch.Create(const AParser: TSQLParser; const ANodes: TBranch.TNodes): TOffset;
begin
  Result := TRange.Create(AParser, ntIfStmtBranch);

  with PBranch(AParser.NodePtr(Result))^ do
  begin
    Nodes := ANodes;

    Heritage.AddChildren(@Nodes, SizeOf(Nodes) div SizeOf(TOffset));
  end;
end;

{ TSQLParser.TIfStmt **********************************************************}

class function TSQLParser.TIfStmt.Create(const AParser: TSQLParser; const ANodes: TNodes): TOffset;
begin
  Result := TStmt.Create(AParser, stIf);

  with PIfStmt(AParser.NodePtr(Result))^ do
  begin
    Nodes := ANodes;

    Heritage.Heritage.AddChildren(@Nodes, SizeOf(Nodes) div SizeOf(TOffset));
  end;
end;

{ TSQLParser.TInOp ************************************************************}

class function TSQLParser.TInOp.Create(const AParser: TSQLParser; const ANodes: TNodes): TOffset;
begin
  Result := TRange.Create(AParser, ntInOp);

  with PInOp(AParser.NodePtr(Result))^ do
  begin
    Nodes := ANodes;

    Heritage.AddChildren(@Nodes, SizeOf(Nodes) div SizeOf(TOffset));
  end;
end;

{ TSQLParser.TInsertStmt ******************************************************}

class function TSQLParser.TInsertStmt.Create(const AParser: TSQLParser; const ANodes: TNodes): TOffset;
begin
  Result := TStmt.Create(AParser, stInsert);

  with PInsertStmt(AParser.NodePtr(Result))^ do
  begin
    Nodes := ANodes;

    Heritage.Heritage.AddChildren(@Nodes, SizeOf(Nodes) div SizeOf(TOffset));
  end;
end;

{ TSQLParser.TInsertStmt ******************************************************}

class function TSQLParser.TInsertStmt.TSetItem.Create(const AParser: TSQLParser; const ANodes: TSetItem.TNodes): TOffset;
begin
  Result := TRange.Create(AParser, ntInsertStmtSetItem);

  with PSetItem(AParser.NodePtr(Result))^ do
  begin
    Nodes := ANodes;

    Heritage.AddChildren(@Nodes, SizeOf(Nodes) div SizeOf(TOffset));
  end;
end;

{ TSQLParser.TInterval ********************************************************}

class function TSQLParser.TIntervalOp.Create(const AParser: TSQLParser; const ANodes: TIntervalOp.TNodes): TOffset;
begin
  Result := TRange.Create(AParser, ntIntervalOp);

  with PIntervalOp(AParser.NodePtr(Result))^ do
  begin
    Nodes := ANodes;

    Heritage.AddChildren(@Nodes, SizeOf(Nodes) div SizeOf(TOffset));
  end;
end;

{ TSQLParser.TIterateStmt *****************************************************}

class function TSQLParser.TIterateStmt.Create(const AParser: TSQLParser; const ANodes: TNodes): TOffset;
begin
  Result := TStmt.Create(AParser, stIterate);

  with PIterateStmt(AParser.NodePtr(Result))^ do
  begin
    Nodes := ANodes;

    Heritage.Heritage.AddChildren(@Nodes, SizeOf(Nodes) div SizeOf(TOffset));
  end;
end;

{ TSQLParser.TKillStmt ********************************************************}

class function TSQLParser.TKillStmt.Create(const AParser: TSQLParser; const ANodes: TNodes): TOffset;
begin
  Result := TStmt.Create(AParser, stKill);

  with PKillStmt(AParser.NodePtr(Result))^ do
  begin
    Nodes := ANodes;

    Heritage.Heritage.AddChildren(@Nodes, SizeOf(Nodes) div SizeOf(TOffset));
  end;
end;

{ TSQLParser.TLeaveStmt *******************************************************}

class function TSQLParser.TLeaveStmt.Create(const AParser: TSQLParser; const ANodes: TNodes): TOffset;
begin
  Result := TStmt.Create(AParser, stLeave);

  with PLeaveStmt(AParser.NodePtr(Result))^ do
  begin
    Nodes := ANodes;

    Heritage.Heritage.AddChildren(@Nodes, SizeOf(Nodes) div SizeOf(TOffset));
  end;
end;

{ TSQLParser.TLikeOp **********************************************************}

class function TSQLParser.TLikeOp.Create(const AParser: TSQLParser; const ANodes: TNodes): TOffset;
begin
  Result := TRange.Create(AParser, ntLikeOp);

  with PLikeOp(AParser.NodePtr(Result))^ do
  begin
    Nodes := ANodes;

    Heritage.AddChildren(@Nodes, SizeOf(Nodes) div SizeOf(TOffset));
  end;
end;

{ TSQLParser.TList ************************************************************}

class function TSQLParser.TList.Create(const AParser: TSQLParser;
  const ANodes: TNodes; const ADelimiterType: TTokenType;
  const AChildrenCount: Integer; const AChildren: array of TOffset): TOffset;
var
  I: Integer;
begin
  Result := TRange.Create(AParser, ntList);

  with PList(AParser.NodePtr(Result))^ do
  begin
    Nodes := ANodes;
    if (AChildrenCount > 0) then
      Nodes.FirstChild := AChildren[0];

    FDelimiterType := ADelimiterType;

    if (ADelimiterType = ttUnknown) then
      FElementCount := AChildrenCount
    else if (ANodes.OpenBracket = 0) then
      FElementCount := (AChildrenCount + 1) div 2
    else
      FElementCount := (AChildrenCount - 1) div 2;

    Heritage.AddChildren(@Nodes, SizeOf(Nodes) div SizeOf(TOffset));
    for I := 0 to AChildrenCount - 1 do
      Heritage.AddChildren(@AChildren[I], 1);
  end;
end;

function TSQLParser.TList.GetFirstChild(): PChild;
begin
  Result := PChild(Parser.NodePtr(Nodes.FirstChild));
end;

{ TSQLParser.TLoadDataStmt ****************************************************}

class function TSQLParser.TLoadDataStmt.Create(const AParser: TSQLParser; const ANodes: TNodes): TOffset;
begin
  Result := TStmt.Create(AParser, stLoadData);

  with PLoadDataStmt(AParser.NodePtr(Result))^ do
  begin
    Nodes := ANodes;

    Heritage.Heritage.AddChildren(@Nodes, SizeOf(Nodes) div SizeOf(TOffset));
  end;
end;

{ TSQLParser.TLoadXMLStmt *****************************************************}

class function TSQLParser.TLoadXMLStmt.Create(const AParser: TSQLParser; const ANodes: TNodes): TOffset;
begin
  Result := TStmt.Create(AParser, stLoadXML);

  with PLoadXMLStmt(AParser.NodePtr(Result))^ do
  begin
    Nodes := ANodes;

  end;
end;

{ TSQLParser.TLockTableStmt.TItem *********************************************}

class function TSQLParser.TLockTableStmt.TItem.Create(const AParser: TSQLParser; const ANodes: TItem.TNodes): TOffset;
begin
  Result := TRange.Create(AParser, ntLockTableStmtItem);

  with PItem(AParser.NodePtr(Result))^ do
  begin
    Nodes := ANodes;

    Heritage.AddChildren(@Nodes, SizeOf(Nodes) div SizeOf(TOffset));
  end;
end;

{ TSQLParser.TLockTableStmt ***************************************************}

class function TSQLParser.TLockTableStmt.Create(const AParser: TSQLParser; const ANodes: TNodes): TOffset;
begin
  Result := TStmt.Create(AParser, stLockTable);

  with PLockTableStmt(AParser.NodePtr(Result))^ do
  begin
    Nodes := ANodes;

    Heritage.Heritage.AddChildren(@Nodes, SizeOf(Nodes) div SizeOf(TOffset));
  end;
end;

{ TSQLParser.TLoopStmt ********************************************************}

class function TSQLParser.TLoopStmt.Create(const AParser: TSQLParser; const ANodes: TNodes): TOffset;
begin
  Result := TStmt.Create(AParser, stLoop);

  with PLoopStmt(AParser.NodePtr(Result))^ do
  begin
    Nodes := ANodes;

    Heritage.Heritage.AddChildren(@Nodes, SizeOf(Nodes) div SizeOf(TOffset));
  end;
end;

{ TSQLParser.TMatchFunc *******************************************************}

class function TSQLParser.TMatchFunc.Create(const AParser: TSQLParser; const ANodes: TNodes): TOffset;
begin
  Result := TRange.Create(AParser, ntMatchFunc);

  with PMatchFunc(AParser.NodePtr(Result))^ do
  begin
    Nodes := ANodes;

    Heritage.AddChildren(@Nodes, SizeOf(Nodes) div SizeOf(TOffset));
  end;
end;

{ TSQLParser.TPositionFunc ****************************************************}

class function TSQLParser.TPositionFunc.Create(const AParser: TSQLParser; const ANodes: TNodes): TOffset;
begin
  Result := TRange.Create(AParser, ntPositionFunc);

  with PPositionFunc(AParser.NodePtr(Result))^ do
  begin
    Nodes := ANodes;

    Heritage.AddChildren(@Nodes, SizeOf(Nodes) div SizeOf(TOffset));
  end;
end;

{ TSQLParser.TPrepareStmt *****************************************************}

class function TSQLParser.TPrepareStmt.Create(const AParser: TSQLParser; const ANodes: TNodes): TOffset;
begin
  Result := TStmt.Create(AParser, stPrepare);

  with PPrepareStmt(AParser.NodePtr(Result))^ do
  begin
    Nodes := ANodes;

    Heritage.Heritage.AddChildren(@Nodes, SizeOf(Nodes) div SizeOf(TOffset));
  end;
end;

{ TSQLParser.TPurgeStmt *******************************************************}

class function TSQLParser.TPurgeStmt.Create(const AParser: TSQLParser; const ANodes: TNodes): TOffset;
begin
  Result := TStmt.Create(AParser, stPurge);

  with PPurgeStmt(AParser.NodePtr(Result))^ do
  begin
    Nodes := ANodes;

    Heritage.Heritage.AddChildren(@Nodes, SizeOf(Nodes) div SizeOf(TOffset));
  end;
end;

{ TSQLParser.TOpenStmt ********************************************************}

class function TSQLParser.TOpenStmt.Create(const AParser: TSQLParser; const ANodes: TNodes): TOffset;
begin
  Result := TStmt.Create(AParser, stOpen);

  with POpenStmt(AParser.NodePtr(Result))^ do
  begin
    Nodes := ANodes;

    Heritage.Heritage.AddChildren(@Nodes, SizeOf(Nodes) div SizeOf(TOffset));
  end;
end;

{ TSQLParser.TOptimizeTableStmt ***********************************************}

class function TSQLParser.TOptimizeTableStmt.Create(const AParser: TSQLParser; const ANodes: TNodes): TOffset;
begin
  Result := TStmt.Create(AParser, stOptimizeTable);

  with POptimizeTableStmt(AParser.NodePtr(Result))^ do
  begin
    Nodes := ANodes;

    Heritage.Heritage.AddChildren(@Nodes, SizeOf(Nodes) div SizeOf(TOffset));
  end;
end;

{ TSQLParser.TRenameTableStmt *************************************************}

class function TSQLParser.TRenameStmt.Create(const AParser: TSQLParser; const ANodes: TNodes): TOffset;
begin
  Result := TStmt.Create(AParser, stRename);

  with PRenameStmt(AParser.NodePtr(Result))^ do
  begin
    Nodes := ANodes;

    Heritage.Heritage.AddChildren(@Nodes, SizeOf(Nodes) div SizeOf(TOffset));
  end;
end;

{ TSQLParser.TRenameStmt.TPair ************************************************}

class function TSQLParser.TRenameStmt.TPair.Create(const AParser: TSQLParser; const ANodes: TPair.TNodes): TOffset;
begin
  Result := TRange.Create(AParser, ntRenameStmtPair);

  with PPair(AParser.NodePtr(Result))^ do
  begin
    Nodes := ANodes;

    Heritage.AddChildren(@Nodes, SizeOf(Nodes) div SizeOf(TOffset));
  end;
end;

{ TSQLParser.TRegExpOp ********************************************************}

class function TSQLParser.TRegExpOp.Create(const AParser: TSQLParser; const ANodes: TNodes): TOffset;
begin
  Result := TRange.Create(AParser, ntRegExpOp);

  with PRegExpOp(AParser.NodePtr(Result))^ do
  begin
    Nodes := ANodes;

    Heritage.AddChildren(@Nodes, SizeOf(Nodes) div SizeOf(TOffset));
  end;
end;

{ TSQLParser.TReleaseStmt *****************************************************}

class function TSQLParser.TReleaseStmt.Create(const AParser: TSQLParser; const ANodes: TNodes): TOffset;
begin
  Result := TStmt.Create(AParser, stRelease);

  with PReleaseStmt(AParser.NodePtr(Result))^ do
  begin
    Nodes := ANodes;

    Heritage.Heritage.AddChildren(@Nodes, SizeOf(Nodes) div SizeOf(TOffset));
  end;
end;

{ TSQLParser.TRepairTableStmt *************************************************}

class function TSQLParser.TRepairTableStmt.Create(const AParser: TSQLParser; const ANodes: TNodes): TOffset;
begin
  Result := TStmt.Create(AParser, stRepairTable);

  with PRepairTableStmt(AParser.NodePtr(Result))^ do
  begin
    Nodes := ANodes;

    Heritage.Heritage.AddChildren(@Nodes, SizeOf(Nodes) div SizeOf(TOffset));
  end;
end;

{ TSQLParser.TRepeatStmt ******************************************************}

class function TSQLParser.TRepeatStmt.Create(const AParser: TSQLParser; const ANodes: TNodes): TOffset;
begin
  Result := TStmt.Create(AParser, stRepeat);

  with PRepeatStmt(AParser.NodePtr(Result))^ do
  begin
    Nodes := ANodes;

    Heritage.Heritage.AddChildren(@Nodes, SizeOf(Nodes) div SizeOf(TOffset));
  end;
end;

{ TSQLParser.TRevokeStmt ******************************************************}

class function TSQLParser.TRevokeStmt.Create(const AParser: TSQLParser; const ANodes: TNodes): TOffset;
begin
  Result := TStmt.Create(AParser, stRevoke);

  with PRevokeStmt(AParser.NodePtr(Result))^ do
  begin
    Nodes := ANodes;

    Heritage.Heritage.AddChildren(@Nodes, SizeOf(Nodes) div SizeOf(TOffset));
  end;
end;

{ TSQLParser.TResetStmt *******************************************************}

class function TSQLParser.TResetStmt.Create(const AParser: TSQLParser; const ANodes: TNodes): TOffset;
begin
  Result := TStmt.Create(AParser, stReset);

  with PResetStmt(AParser.NodePtr(Result))^ do
  begin
    Nodes := ANodes;

    Heritage.Heritage.AddChildren(@Nodes, SizeOf(Nodes) div SizeOf(TOffset));
  end;
end;

{ TSQLParser.TResetStmt.TOption ***********************************************}

class function TSQLParser.TResetStmt.TOption.Create(const AParser: TSQLParser; const ANodes: TOption.TNodes): TOffset;
begin
  Result := TStmt.Create(AParser, stReset);

  with POption(AParser.NodePtr(Result))^ do
  begin
    Nodes := ANodes;

    Heritage.AddChildren(@Nodes, SizeOf(Nodes) div SizeOf(TOffset));
  end;
end;

{ TSQLParser.TResignalStmt ****************************************************}

class function TSQLParser.TResignalStmt.Create(const AParser: TSQLParser; const ANodes: TNodes): TOffset;
begin
  Result := TStmt.Create(AParser, stResignal);

  with PResignalStmt(AParser.NodePtr(Result))^ do
  begin
    Nodes := ANodes;

    Heritage.Heritage.AddChildren(@Nodes, SizeOf(Nodes) div SizeOf(TOffset));
  end;
end;

{ TSQLParser.TReturnStmt ******************************************************}

class function TSQLParser.TReturnStmt.Create(const AParser: TSQLParser; const ANodes: TNodes): TOffset;
begin
  Result := TStmt.Create(AParser, stReturn);

  with PReturnStmt(AParser.NodePtr(Result))^ do
  begin
    Nodes := ANodes;

    Heritage.Heritage.AddChildren(@Nodes, SizeOf(Nodes) div SizeOf(TOffset));
  end;
end;

{ TSQLParser.TRollbackStmt ****************************************************}

class function TSQLParser.TRollbackStmt.Create(const AParser: TSQLParser; const ANodes: TNodes): TOffset;
begin
  Result := TStmt.Create(AParser, stRollback);

  with PRollbackStmt(AParser.NodePtr(Result))^ do
  begin
    Nodes := ANodes;

    Heritage.Heritage.AddChildren(@Nodes, SizeOf(Nodes) div SizeOf(TOffset));
  end;
end;

{ TSQLParser.TRoutineParam ****************************************************}

class function TSQLParser.TRoutineParam.Create(const AParser: TSQLParser; const ANodes: TNodes): TOffset;
begin
  Result := TRange.Create(AParser, ntRoutineParam);

  with PRoutineParam(AParser.NodePtr(Result))^ do
  begin
    Nodes := ANodes;

    Heritage.AddChildren(@Nodes, SizeOf(Nodes) div SizeOf(TOffset));
  end;
end;

{ TSQLParser.TSavepointStmt ***************************************************}

class function TSQLParser.TSavepointStmt.Create(const AParser: TSQLParser; const ANodes: TNodes): TOffset;
begin
  Result := TStmt.Create(AParser, stSavepoint);

  with PSavepointStmt(AParser.NodePtr(Result))^ do
  begin
    Nodes := ANodes;

    Heritage.Heritage.AddChildren(@Nodes, SizeOf(Nodes) div SizeOf(TOffset));
  end;
end;

{ TSQLParser.TSecretIdent *****************************************************}

class function TSQLParser.TSecretIdent.Create(const AParser: TSQLParser; const ANodes: TNodes): TOffset;
begin
  Result := TRange.Create(AParser, ntSecretIdent);

  with PSecretIdent(AParser.NodePtr(Result))^ do
  begin
    Nodes := ANodes;

    Heritage.AddChildren(@Nodes, SizeOf(Nodes) div SizeOf(TOffset));
  end;
end;

{ TSQLParser.TSelectStmt.TColumn **********************************************}

class function TSQLParser.TSelectStmt.TColumn.Create(const AParser: TSQLParser; const ANodes: TColumn.TNodes): TOffset;
begin
  Result := TRange.Create(AParser, ntSelectStmtColumn);

  with PColumn(AParser.NodePtr(Result))^ do
  begin
    Nodes := ANodes;

    Heritage.AddChildren(@Nodes, SizeOf(Nodes) div SizeOf(TOffset));
  end;
end;

{ TSQLParser.TSelectStmt.TTableFactor.TIndexHint ****************************}

class function TSQLParser.TSelectStmt.TTableFactor.TIndexHint.Create(const AParser: TSQLParser; const ANodes: TIndexHint.TNodes): TOffset;
begin
  Result := TRange.Create(AParser, ntSelectStmtTableFactorIndexHint);

  with PIndexHint(AParser.NodePtr(Result))^ do
  begin
    Nodes := ANodes;

    Heritage.AddChildren(@Nodes, SizeOf(Nodes) div SizeOf(TOffset));
  end;
end;

{ TSQLParser.TSelectStmt.TTableFactor *****************************************}

class function TSQLParser.TSelectStmt.TTableFactor.Create(const AParser: TSQLParser; const ANodes: TTableFactor.TNodes): TOffset;
begin
  Result := TRange.Create(AParser, ntSelectStmtTableFactor);

  with PTableFactor(AParser.NodePtr(Result))^ do
  begin
    Nodes := ANodes;

    Heritage.AddChildren(@Nodes, SizeOf(Nodes) div SizeOf(TOffset));
  end;
end;

{ TSQLParser.TSelectStmt.TTableFactorOj ***************************************}

class function TSQLParser.TSelectStmt.TTableFactorOj.Create(const AParser: TSQLParser; const ANodes: TTableFactorOj.TNodes): TOffset;
begin
  Result := TRange.Create(AParser, ntSelectStmtTableFactorOj);

  with PTableFactorOj(AParser.NodePtr(Result))^ do
  begin
    Nodes := ANodes;

    Heritage.AddChildren(@Nodes, SizeOf(Nodes) div SizeOf(TOffset));
  end;
end;

{ TSQLParser.TSelectStmt.TTableFactorSelect ***********************************}

class function TSQLParser.TSelectStmt.TTableFactorSelect.Create(const AParser: TSQLParser; const ANodes: TTableFactorSelect.TNodes): TOffset;
begin
  Result := TRange.Create(AParser, ntSelectStmtTableFactorSelect);

  with PTableFactorSelect(AParser.NodePtr(Result))^ do
  begin
    Nodes := ANodes;

    Heritage.AddChildren(@Nodes, SizeOf(Nodes) div SizeOf(TOffset));
  end;
end;

{ TSQLParser.TSelectStmt.TTableJoin *******************************************}

class function TSQLParser.TSelectStmt.TTableJoin.Create(const AParser: TSQLParser; const AJoinType: TJoinType; const ANodes: TTableJoin.TNodes): TOffset;
begin
  Result := TRange.Create(AParser, ntSelectStmtTableJoin);

  with PTableJoin(AParser.NodePtr(Result))^ do
  begin
    FJoinType := AJoinType;

    Nodes := ANodes;

    Heritage.AddChildren(@Nodes, SizeOf(Nodes) div SizeOf(TOffset));
  end;
end;

{ TSQLParser.TSelectStmt.TGroup ***********************************************}

class function TSQLParser.TSelectStmt.TGroup.Create(const AParser: TSQLParser; const ANodes: TGroup.TNodes): TOffset;
begin
  Result := TRange.Create(AParser, ntSelectStmtGroup);

  with PGroup(AParser.NodePtr(Result))^ do
  begin
    Nodes := ANodes;

    Heritage.AddChildren(@Nodes, SizeOf(Nodes) div SizeOf(TOffset));
  end;
end;

{ TSQLParser.TSelectStmt.TOrder ***********************************************}

class function TSQLParser.TSelectStmt.TOrder.Create(const AParser: TSQLParser; const ANodes: TOrder.TNodes): TOffset;
begin
  Result := TRange.Create(AParser, ntSelectStmtOrder);

  with POrder(AParser.NodePtr(Result))^ do
  begin
    Nodes := ANodes;

    Heritage.AddChildren(@Nodes, SizeOf(Nodes) div SizeOf(TOffset));
  end;
end;

{ TSQLParser.TSelectStmt ******************************************************}

class function TSQLParser.TSelectStmt.Create(const AParser: TSQLParser; const ANodes: TNodes): TOffset;
begin
  Result := TStmt.Create(AParser, stSelect);

  with PSelectStmt(AParser.NodePtr(Result))^ do
  begin
    Nodes := ANodes;

    Heritage.Heritage.AddChildren(@Nodes, SizeOf(Nodes) div SizeOf(TOffset));
  end;
end;

{ TSQLParser.TSchedule ********************************************************}

class function TSQLParser.TSchedule.Create(const AParser: TSQLParser; const ANodes: TNodes): TOffset;
begin
  Result := TRange.Create(AParser, ntSchedule);

  with PSchedule(AParser.NodePtr(Result))^ do
  begin
    Nodes := ANodes;

    Heritage.AddChildren(@Nodes, SizeOf(Nodes) div SizeOf(TOffset));
  end;
end;

{ TSQLParser.TSetStmt.TAssignment *********************************************}

class function TSQLParser.TSetStmt.TAssignment.Create(const AParser: TSQLParser; const ANodes: TAssignment.TNodes): TOffset;
begin
  Result := TRange.Create(AParser, ntSetStmtAssignment);

  with PAssignment(AParser.NodePtr(Result))^ do
  begin
    Nodes := ANodes;

    Heritage.AddChildren(@Nodes, SizeOf(Nodes) div SizeOf(TOffset));
  end;
end;

{ TSQLParser.TSetStmt *********************************************************}

class function TSQLParser.TSetStmt.Create(const AParser: TSQLParser; const ANodes: TNodes): TOffset;
begin
  Result := TStmt.Create(AParser, stSet);

  with PSetStmt(AParser.NodePtr(Result))^ do
  begin
    Nodes := ANodes;

    Heritage.Heritage.AddChildren(@Nodes, SizeOf(Nodes) div SizeOf(TOffset));
  end;
end;

{ TSQLParser.TSetNamesStmt ****************************************************}

class function TSQLParser.TSetNamesStmt.Create(const AParser: TSQLParser; const ANodes: TNodes): TOffset;
begin
  Result := TStmt.Create(AParser, stSetNames);

  with PSetNamesStmt(AParser.NodePtr(Result))^ do
  begin
    Nodes := ANodes;

    Heritage.Heritage.AddChildren(@Nodes, SizeOf(Nodes) div SizeOf(TOffset));
  end;
end;

{ TSQLParser.TSetPasswordStmt *************************************************}

class function TSQLParser.TSetPasswordStmt.Create(const AParser: TSQLParser; const ANodes: TNodes): TOffset;
begin
  Result := TStmt.Create(AParser, stSetPassword);

  with PSetPasswordStmt(AParser.NodePtr(Result))^ do
  begin
    Nodes := ANodes;

    Heritage.Heritage.AddChildren(@Nodes, SizeOf(Nodes) div SizeOf(TOffset));
  end;
end;

{ TSQLParser.TSetTransactionStmt.TCharacteristic ****************************}

class function TSQLParser.TSetTransactionStmt.TCharacteristic.Create(const AParser: TSQLParser; const ANodes: TCharacteristic.TNodes): TOffset;
begin
  Result := TRange.Create(AParser, ntSetTransactionStmtCharacteristic);

  with PCharacteristic(AParser.NodePtr(Result))^ do
  begin
    Nodes := ANodes;

    Heritage.AddChildren(@Nodes, SizeOf(Nodes) div SizeOf(TOffset));
  end;
end;

{ TSQLParser.TSetTransactionStmt **********************************************}

class function TSQLParser.TSetTransactionStmt.Create(const AParser: TSQLParser; const ANodes: TNodes): TOffset;
begin
  Result := TStmt.Create(AParser, stSetTransaction);

  with PSetTransactionStmt(AParser.NodePtr(Result))^ do
  begin
    Nodes := ANodes;

    Heritage.Heritage.AddChildren(@Nodes, SizeOf(Nodes) div SizeOf(TOffset));
  end;
end;

{ TSQLParser.TShowAuthorsStmt *************************************************}

class function TSQLParser.TShowAuthorsStmt.Create(const AParser: TSQLParser; const ANodes: TNodes): TOffset;
begin
  Result := TStmt.Create(AParser, stShowAuthors);

  with PShowAuthorsStmt(AParser.NodePtr(Result))^ do
  begin
    Nodes := ANodes;

    Heritage.Heritage.AddChildren(@Nodes, SizeOf(Nodes) div SizeOf(TOffset));
  end;
end;

{ TSQLParser.TShowBinaryLogsStmt **********************************************}

class function TSQLParser.TShowBinaryLogsStmt.Create(const AParser: TSQLParser; const ANodes: TNodes): TOffset;
begin
  Result := TStmt.Create(AParser, stShowBinaryLogs);

  with PShowBinaryLogsStmt(AParser.NodePtr(Result))^ do
  begin
    Nodes := ANodes;

    Heritage.Heritage.AddChildren(@Nodes, SizeOf(Nodes) div SizeOf(TOffset));
  end;
end;

{ TSQLParser.TShowBinlogEventsStmt ********************************************}

class function TSQLParser.TShowBinlogEventsStmt.Create(const AParser: TSQLParser; const ANodes: TNodes): TOffset;
begin
  Result := TStmt.Create(AParser, stShowBinlogEvents);

  with PShowBinlogEventsStmt(AParser.NodePtr(Result))^ do
  begin
    Nodes := ANodes;

    Heritage.Heritage.AddChildren(@Nodes, SizeOf(Nodes) div SizeOf(TOffset));
  end;
end;

{ TSQLParser.TShowCharacterSetStmt ********************************************}

class function TSQLParser.TShowCharacterSetStmt.Create(const AParser: TSQLParser; const ANodes: TNodes): TOffset;
begin
  Result := TStmt.Create(AParser, stShowCharacterSet);

  with PShowCharacterSetStmt(AParser.NodePtr(Result))^ do
  begin
    Nodes := ANodes;

    Heritage.Heritage.AddChildren(@Nodes, SizeOf(Nodes) div SizeOf(TOffset));
  end;
end;

{ TSQLParser.TShowCollationStmt ***********************************************}

class function TSQLParser.TShowCollationStmt.Create(const AParser: TSQLParser; const ANodes: TNodes): TOffset;
begin
  Result := TStmt.Create(AParser, stShowCollation);

  with PShowCollationStmt(AParser.NodePtr(Result))^ do
  begin
    Nodes := ANodes;

    Heritage.Heritage.AddChildren(@Nodes, SizeOf(Nodes) div SizeOf(TOffset));
  end;
end;

{ TSQLParser.TShowColumnsStmt *************************************************}

class function TSQLParser.TShowColumnsStmt.Create(const AParser: TSQLParser; const ANodes: TNodes): TOffset;
begin
  Result := TStmt.Create(AParser, stShowColumns);

  with PShowColumnsStmt(AParser.NodePtr(Result))^ do
  begin
    Nodes := ANodes;

    Heritage.Heritage.AddChildren(@Nodes, SizeOf(Nodes) div SizeOf(TOffset));
  end;
end;

{ TSQLParser.TShowContributorsStmt ********************************************}

class function TSQLParser.TShowContributorsStmt.Create(const AParser: TSQLParser; const ANodes: TNodes): TOffset;
begin
  Result := TStmt.Create(AParser, stShowContributors);

  with PShowContributorsStmt(AParser.NodePtr(Result))^ do
  begin
    Nodes := ANodes;

    Heritage.Heritage.AddChildren(@Nodes, SizeOf(Nodes) div SizeOf(TOffset));
  end;
end;

{ TSQLParser.TShowCountErrorsStmt *********************************************}

class function TSQLParser.TShowCountErrorsStmt.Create(const AParser: TSQLParser; const ANodes: TNodes): TOffset;
begin
  Result := TStmt.Create(AParser, stShowCountErrors);

  with PShowCountErrorsStmt(AParser.NodePtr(Result))^ do
  begin
    Nodes := ANodes;

    Heritage.Heritage.AddChildren(@Nodes, SizeOf(Nodes) div SizeOf(TOffset));
  end;
end;

{ TSQLParser.TShowCountWarningsStmt *******************************************}

class function TSQLParser.TShowCountWarningsStmt.Create(const AParser: TSQLParser; const ANodes: TNodes): TOffset;
begin
  Result := TStmt.Create(AParser, stShowCountWarnings);

  with PShowCountWarningsStmt(AParser.NodePtr(Result))^ do
  begin
    Nodes := ANodes;

    Heritage.Heritage.AddChildren(@Nodes, SizeOf(Nodes) div SizeOf(TOffset));
  end;
end;

{ TSQLParser.TShowCreateDatabaseStmt ******************************************}

class function TSQLParser.TShowCreateDatabaseStmt.Create(const AParser: TSQLParser; const ANodes: TNodes): TOffset;
begin
  Result := TStmt.Create(AParser, stShowCreateDatabase);

  with PShowCreateDatabaseStmt(AParser.NodePtr(Result))^ do
  begin
    Nodes := ANodes;

    Heritage.Heritage.AddChildren(@Nodes, SizeOf(Nodes) div SizeOf(TOffset));
  end;
end;

{ TSQLParser.TShowCreateEventStmt *********************************************}

class function TSQLParser.TShowCreateEventStmt.Create(const AParser: TSQLParser; const ANodes: TNodes): TOffset;
begin
  Result := TStmt.Create(AParser, stShowCreateEvent);

  with PShowCreateEventStmt(AParser.NodePtr(Result))^ do
  begin
    Nodes := ANodes;

    Heritage.Heritage.AddChildren(@Nodes, SizeOf(Nodes) div SizeOf(TOffset));
  end;
end;

{ TSQLParser.TShowCreateFunctionStmt ******************************************}

class function TSQLParser.TShowCreateFunctionStmt.Create(const AParser: TSQLParser; const ANodes: TNodes): TOffset;
begin
  Result := TStmt.Create(AParser, stShowCreateFunction);

  with PShowCreateFunctionStmt(AParser.NodePtr(Result))^ do
  begin
    Nodes := ANodes;

    Heritage.Heritage.AddChildren(@Nodes, SizeOf(Nodes) div SizeOf(TOffset));
  end;
end;

{ TSQLParser.TShowCreateProcedureStmt *****************************************}

class function TSQLParser.TShowCreateProcedureStmt.Create(const AParser: TSQLParser; const ANodes: TNodes): TOffset;
begin
  Result := TStmt.Create(AParser, stShowCreateProcedure);

  with PShowCreateProcedureStmt(AParser.NodePtr(Result))^ do
  begin
    Nodes := ANodes;

    Heritage.Heritage.AddChildren(@Nodes, SizeOf(Nodes) div SizeOf(TOffset));
  end;
end;

{ TSQLParser.TShowCreateTableStmt *********************************************}

class function TSQLParser.TShowCreateTableStmt.Create(const AParser: TSQLParser; const ANodes: TNodes): TOffset;
begin
  Result := TStmt.Create(AParser, stShowCreateTable);

  with PShowCreateTableStmt(AParser.NodePtr(Result))^ do
  begin
    Nodes := ANodes;

    Heritage.Heritage.AddChildren(@Nodes, SizeOf(Nodes) div SizeOf(TOffset));
  end;
end;

{ TSQLParser.TShowCreateTriggerStmt *******************************************}

class function TSQLParser.TShowCreateTriggerStmt.Create(const AParser: TSQLParser; const ANodes: TNodes): TOffset;
begin
  Result := TStmt.Create(AParser, stShowCreateTrigger);

  with PShowCreateTriggerStmt(AParser.NodePtr(Result))^ do
  begin
    Nodes := ANodes;

    Heritage.Heritage.AddChildren(@Nodes, SizeOf(Nodes) div SizeOf(TOffset));
  end;
end;

{ TSQLParser.TShowCreateUserStmt **********************************************}

class function TSQLParser.TShowCreateUserStmt.Create(const AParser: TSQLParser; const ANodes: TNodes): TOffset;
begin
  Result := TStmt.Create(AParser, stShowCreateUser);

  with PShowCreateUserStmt(AParser.NodePtr(Result))^ do
  begin
    Nodes := ANodes;

    Heritage.Heritage.AddChildren(@Nodes, SizeOf(Nodes) div SizeOf(TOffset));
  end;
end;

{ TSQLParser.TShowCreateViewStmt **********************************************}

class function TSQLParser.TShowCreateViewStmt.Create(const AParser: TSQLParser; const ANodes: TNodes): TOffset;
begin
  Result := TStmt.Create(AParser, stShowCreateView);

  with PShowCreateViewStmt(AParser.NodePtr(Result))^ do
  begin
    Nodes := ANodes;

    Heritage.Heritage.AddChildren(@Nodes, SizeOf(Nodes) div SizeOf(TOffset));
  end;
end;

{ TSQLParser.TShowDatabasesStmt ***********************************************}

class function TSQLParser.TShowDatabasesStmt.Create(const AParser: TSQLParser; const ANodes: TNodes): TOffset;
begin
  Result := TStmt.Create(AParser, stShowDatabases);

  with PShowDatabasesStmt(AParser.NodePtr(Result))^ do
  begin
    Nodes := ANodes;

    Heritage.Heritage.AddChildren(@Nodes, SizeOf(Nodes) div SizeOf(TOffset));
  end;
end;

{ TSQLParser.TShowEngineStmt **************************************************}

class function TSQLParser.TShowEngineStmt.Create(const AParser: TSQLParser; const ANodes: TNodes): TOffset;
begin
  Result := TStmt.Create(AParser, stShowEngine);

  with PShowEngineStmt(AParser.NodePtr(Result))^ do
  begin
    Nodes := ANodes;

    Heritage.Heritage.AddChildren(@Nodes, SizeOf(Nodes) div SizeOf(TOffset));
  end;
end;

{ TSQLParser.TShowEnginesStmt *************************************************}

class function TSQLParser.TShowEnginesStmt.Create(const AParser: TSQLParser; const ANodes: TNodes): TOffset;
begin
  Result := TStmt.Create(AParser, stShowEngines);

  with PShowEnginesStmt(AParser.NodePtr(Result))^ do
  begin
    Nodes := ANodes;

    Heritage.Heritage.AddChildren(@Nodes, SizeOf(Nodes) div SizeOf(TOffset));
  end;
end;

{ TSQLParser.TShowErrorsStmt **************************************************}

class function TSQLParser.TShowErrorsStmt.Create(const AParser: TSQLParser; const ANodes: TNodes): TOffset;
begin
  Result := TStmt.Create(AParser, stShowErrors);

  with PShowErrorsStmt(AParser.NodePtr(Result))^ do
  begin
    Nodes := ANodes;

    Heritage.Heritage.AddChildren(@Nodes, SizeOf(Nodes) div SizeOf(TOffset));
  end;
end;

{ TSQLParser.TShowEventsStmt **************************************************}

class function TSQLParser.TShowEventsStmt.Create(const AParser: TSQLParser; const ANodes: TNodes): TOffset;
begin
  Result := TStmt.Create(AParser, stShowEvents);

  with PShowEventsStmt(AParser.NodePtr(Result))^ do
  begin
    Nodes := ANodes;

    Heritage.Heritage.AddChildren(@Nodes, SizeOf(Nodes) div SizeOf(TOffset));
  end;
end;

{ TSQLParser.TShowFunctionCodeStmt ********************************************}

class function TSQLParser.TShowFunctionCodeStmt.Create(const AParser: TSQLParser; const ANodes: TNodes): TOffset;
begin
  Result := TStmt.Create(AParser, stShowFunctionCode);

  with PShowFunctionCodeStmt(AParser.NodePtr(Result))^ do
  begin
    Nodes := ANodes;

    Heritage.Heritage.AddChildren(@Nodes, SizeOf(Nodes) div SizeOf(TOffset));
  end;
end;

{ TSQLParser.TShowFunctionStatusStmt ******************************************}

class function TSQLParser.TShowFunctionStatusStmt.Create(const AParser: TSQLParser; const ANodes: TNodes): TOffset;
begin
  Result := TStmt.Create(AParser, stShowFunctionStatus);

  with PShowFunctionStatusStmt(AParser.NodePtr(Result))^ do
  begin
    Nodes := ANodes;

    Heritage.Heritage.AddChildren(@Nodes, SizeOf(Nodes) div SizeOf(TOffset));
  end;
end;

{ TSQLParser.TShowGrantsStmt **************************************************}

class function TSQLParser.TShowGrantsStmt.Create(const AParser: TSQLParser; const ANodes: TNodes): TOffset;
begin
  Result := TStmt.Create(AParser, stShowGrants);

  with PShowGrantsStmt(AParser.NodePtr(Result))^ do
  begin
    Nodes := ANodes;

    Heritage.Heritage.AddChildren(@Nodes, SizeOf(Nodes) div SizeOf(TOffset));
  end;
end;

{ TSQLParser.TShowIndexStmt ***************************************************}

class function TSQLParser.TShowIndexStmt.Create(const AParser: TSQLParser; const ANodes: TNodes): TOffset;
begin
  Result := TStmt.Create(AParser, stShowIndex);

  with PShowIndexStmt(AParser.NodePtr(Result))^ do
  begin
    Nodes := ANodes;

    Heritage.Heritage.AddChildren(@Nodes, SizeOf(Nodes) div SizeOf(TOffset));
  end;
end;

{ TSQLParser.TShowMasterStatusStmt ********************************************}

class function TSQLParser.TShowMasterStatusStmt.Create(const AParser: TSQLParser; const ANodes: TNodes): TOffset;
begin
  Result := TStmt.Create(AParser, stShowMasterStatus);

  with PShowMasterStatusStmt(AParser.NodePtr(Result))^ do
  begin
    Nodes := ANodes;

    Heritage.Heritage.AddChildren(@Nodes, SizeOf(Nodes) div SizeOf(TOffset));
  end;
end;

{ TSQLParser.TShowOpenTablesStmt **********************************************}

class function TSQLParser.TShowOpenTablesStmt.Create(const AParser: TSQLParser; const ANodes: TNodes): TOffset;
begin
  Result := TStmt.Create(AParser, stShowOpenTables);

  with PShowOpenTablesStmt(AParser.NodePtr(Result))^ do
  begin
    Nodes := ANodes;

    Heritage.Heritage.AddChildren(@Nodes, SizeOf(Nodes) div SizeOf(TOffset));
  end;
end;

{ TSQLParser.TShowPluginsStmt *************************************************}

class function TSQLParser.TShowPluginsStmt.Create(const AParser: TSQLParser; const ANodes: TNodes): TOffset;
begin
  Result := TStmt.Create(AParser, stShowPlugins);

  with PShowPluginsStmt(AParser.NodePtr(Result))^ do
  begin
    Nodes := ANodes;

    Heritage.Heritage.AddChildren(@Nodes, SizeOf(Nodes) div SizeOf(TOffset));
  end;
end;

{ TSQLParser.TShowPrivilegesStmt **********************************************}

class function TSQLParser.TShowPrivilegesStmt.Create(const AParser: TSQLParser; const ANodes: TNodes): TOffset;
begin
  Result := TStmt.Create(AParser, stShowPrivileges);

  with PShowPrivilegesStmt(AParser.NodePtr(Result))^ do
  begin
    Nodes := ANodes;

    Heritage.Heritage.AddChildren(@Nodes, SizeOf(Nodes) div SizeOf(TOffset));
  end;
end;

{ TSQLParser.TShowProcedureCodeStmt *******************************************}

class function TSQLParser.TShowProcedureCodeStmt.Create(const AParser: TSQLParser; const ANodes: TNodes): TOffset;
begin
  Result := TStmt.Create(AParser, stShowProcedureCode);

  with PShowProcedureCodeStmt(AParser.NodePtr(Result))^ do
  begin
    Nodes := ANodes;

    Heritage.Heritage.AddChildren(@Nodes, SizeOf(Nodes) div SizeOf(TOffset));
  end;
end;

{ TSQLParser.TShowProcedureStatusStmt *****************************************}

class function TSQLParser.TShowProcedureStatusStmt.Create(const AParser: TSQLParser; const ANodes: TNodes): TOffset;
begin
  Result := TStmt.Create(AParser, stShowProcedureStatus);

  with PShowProcedureStatusStmt(AParser.NodePtr(Result))^ do
  begin
    Nodes := ANodes;

    Heritage.Heritage.AddChildren(@Nodes, SizeOf(Nodes) div SizeOf(TOffset));
  end;
end;

{ TSQLParser.TShowProcessListStmt *********************************************}

class function TSQLParser.TShowProcessListStmt.Create(const AParser: TSQLParser; const ANodes: TNodes): TOffset;
begin
  Result := TStmt.Create(AParser, stShowProcessList);

  with PShowProcessListStmt(AParser.NodePtr(Result))^ do
  begin
    Nodes := ANodes;

    Heritage.Heritage.AddChildren(@Nodes, SizeOf(Nodes) div SizeOf(TOffset));
  end;
end;

{ TSQLParser.TShowProfileStmt *************************************************}

class function TSQLParser.TShowProfileStmt.Create(const AParser: TSQLParser; const ANodes: TNodes): TOffset;
begin
  Result := TStmt.Create(AParser, stShowProfile);

  with PShowProfileStmt(AParser.NodePtr(Result))^ do
  begin
    Nodes := ANodes;

    Heritage.Heritage.AddChildren(@Nodes, SizeOf(Nodes) div SizeOf(TOffset));
  end;
end;

{ TSQLParser.TShowProfilesStmt ************************************************}

class function TSQLParser.TShowProfilesStmt.Create(const AParser: TSQLParser; const ANodes: TNodes): TOffset;
begin
  Result := TStmt.Create(AParser, stShowProfiles);

  with PShowProfilesStmt(AParser.NodePtr(Result))^ do
  begin
    Nodes := ANodes;

    Heritage.Heritage.AddChildren(@Nodes, SizeOf(Nodes) div SizeOf(TOffset));
  end;
end;

{ TSQLParser.TShowRelaylogEventsStmt ******************************************}

class function TSQLParser.TShowRelaylogEventsStmt.Create(const AParser: TSQLParser; const ANodes: TNodes): TOffset;
begin
  Result := TStmt.Create(AParser, stShowRelaylogEvents);

  with PShowRelaylogEventsStmt(AParser.NodePtr(Result))^ do
  begin
    Nodes := ANodes;

    Heritage.Heritage.AddChildren(@Nodes, SizeOf(Nodes) div SizeOf(TOffset));
  end;
end;

{ TSQLParser.TShowSlaveHostsStmt **********************************************}

class function TSQLParser.TShowSlaveHostsStmt.Create(const AParser: TSQLParser; const ANodes: TNodes): TOffset;
begin
  Result := TStmt.Create(AParser, stShowSlaveHosts);

  with PShowSlaveHostsStmt(AParser.NodePtr(Result))^ do
  begin
    Nodes := ANodes;

    Heritage.Heritage.AddChildren(@Nodes, SizeOf(Nodes) div SizeOf(TOffset));
  end;
end;

{ TSQLParser.TShowSlaveStatusStmt *********************************************}

class function TSQLParser.TShowSlaveStatusStmt.Create(const AParser: TSQLParser; const ANodes: TNodes): TOffset;
begin
  Result := TStmt.Create(AParser, stShowSlaveStatus);

  with PShowSlaveStatusStmt(AParser.NodePtr(Result))^ do
  begin
    Nodes := ANodes;

    Heritage.Heritage.AddChildren(@Nodes, SizeOf(Nodes) div SizeOf(TOffset));
  end;
end;

{ TSQLParser.TShowStatusStmt **************************************************}

class function TSQLParser.TShowStatusStmt.Create(const AParser: TSQLParser; const ANodes: TNodes): TOffset;
begin
  Result := TStmt.Create(AParser, stShowStatus);

  with PShowStatusStmt(AParser.NodePtr(Result))^ do
  begin
    Nodes := ANodes;

    Heritage.Heritage.AddChildren(@Nodes, SizeOf(Nodes) div SizeOf(TOffset));
  end;
end;

{ TSQLParser.TShowTableStatusStmt *********************************************}

class function TSQLParser.TShowTableStatusStmt.Create(const AParser: TSQLParser; const ANodes: TNodes): TOffset;
begin
  Result := TStmt.Create(AParser, stShowTableStatus);

  with PShowTableStatusStmt(AParser.NodePtr(Result))^ do
  begin
    Nodes := ANodes;

    Heritage.Heritage.AddChildren(@Nodes, SizeOf(Nodes) div SizeOf(TOffset));
  end;
end;

{ TSQLParser.TShowTablesStmt **************************************************}

class function TSQLParser.TShowTablesStmt.Create(const AParser: TSQLParser; const ANodes: TNodes): TOffset;
begin
  Result := TStmt.Create(AParser, stShowTables);

  with PShowTablesStmt(AParser.NodePtr(Result))^ do
  begin
    Nodes := ANodes;

    Heritage.Heritage.AddChildren(@Nodes, SizeOf(Nodes) div SizeOf(TOffset));
  end;
end;

{ TSQLParser.TShowTriggersStmt ************************************************}

class function TSQLParser.TShowTriggersStmt.Create(const AParser: TSQLParser; const ANodes: TNodes): TOffset;
begin
  Result := TStmt.Create(AParser, stShowTriggers);

  with PShowTriggersStmt(AParser.NodePtr(Result))^ do
  begin
    Nodes := ANodes;

    Heritage.Heritage.AddChildren(@Nodes, SizeOf(Nodes) div SizeOf(TOffset));
  end;
end;

{ TSQLParser.TShowVariablesStmt ***********************************************}

class function TSQLParser.TShowVariablesStmt.Create(const AParser: TSQLParser; const ANodes: TNodes): TOffset;
begin
  Result := TStmt.Create(AParser, stShowVariables);

  with PShowVariablesStmt(AParser.NodePtr(Result))^ do
  begin
    Nodes := ANodes;

    Heritage.Heritage.AddChildren(@Nodes, SizeOf(Nodes) div SizeOf(TOffset));
  end;
end;

{ TSQLParser.TShowWarningsStmt ************************************************}

class function TSQLParser.TShowWarningsStmt.Create(const AParser: TSQLParser; const ANodes: TNodes): TOffset;
begin
  Result := TStmt.Create(AParser, stShowWarnings);

  with PShowWarningsStmt(AParser.NodePtr(Result))^ do
  begin
    Nodes := ANodes;

    Heritage.Heritage.AddChildren(@Nodes, SizeOf(Nodes) div SizeOf(TOffset));
  end;
end;

{ TSQLParser.TShutdownStmt ****************************************************}

class function TSQLParser.TShutdownStmt.Create(const AParser: TSQLParser; const ANodes: TNodes): TOffset;
begin
  Result := TStmt.Create(AParser, stShutdown);

  with PShutdownStmt(AParser.NodePtr(Result))^ do
  begin
    Nodes := ANodes;

    Heritage.Heritage.AddChildren(@Nodes, SizeOf(Nodes) div SizeOf(TOffset));
  end;
end;

{ TSQLParser.TSignalStmt ******************************************************}

class function TSQLParser.TSignalStmt.Create(const AParser: TSQLParser; const ANodes: TNodes): TOffset;
begin
  Result := TStmt.Create(AParser, stSignal);

  with PSignalStmt(AParser.NodePtr(Result))^ do
  begin
    Nodes := ANodes;

    Heritage.Heritage.AddChildren(@Nodes, SizeOf(Nodes) div SizeOf(TOffset));
  end;
end;

{ TSQLParser.TSignalStmt.TInformation *****************************************}

class function TSQLParser.TSignalStmt.TInformation.Create(const AParser: TSQLParser; const ANodes: TInformation.TNodes): TOffset;
begin
  Result := TRange.Create(AParser, ntSignalStmtInformation);

  with PInformation(AParser.NodePtr(Result))^ do
  begin
    Nodes := ANodes;

    Heritage.AddChildren(@Nodes, SizeOf(Nodes) div SizeOf(TOffset));
  end;
end;

{ TSQLParser.TSubquery ********************************************************}

class function TSQLParser.TSubquery.Create(const AParser: TSQLParser; const AIdentTag, ASubquery: TOffset): TOffset;
var
  Nodes: TNodes;
begin
  Nodes.IdentTag := AIdentTag;
  Nodes.Subquery := ASubquery;

  Result := Create(AParser, Nodes);
end;

class function TSQLParser.TSubquery.Create(const AParser: TSQLParser; const ANodes: TNodes): TOffset;
begin
  Result := TRange.Create(AParser, ntSubquery);

  with PSubquery(AParser.NodePtr(Result))^ do
  begin
    Nodes := ANodes;

    Heritage.AddChildren(@Nodes, SizeOf(Nodes) div SizeOf(TOffset));
  end;
end;

{ TSQLParser.TSubstringFunc ***************************************************}

class function TSQLParser.TSubstringFunc.Create(const AParser: TSQLParser; const ANodes: TNodes): TOffset;
begin
  Result := TRange.Create(AParser, ntSubstringFunc);

  with PSubstringFunc(AParser.NodePtr(Result))^ do
  begin
    Nodes := ANodes;

    Heritage.AddChildren(@Nodes, SizeOf(Nodes) div SizeOf(TOffset));
  end;
end;

{ TSQLParser.TSoundsLikeOp ****************************************************}

class function TSQLParser.TSoundsLikeOp.Create(const AParser: TSQLParser; const AOperand1, ASounds, ALike, AOperand2: TOffset): TOffset;
begin
  Result := TRange.Create(AParser, ntSoundsLikeOp);

  with PSoundsLikeOp(AParser.NodePtr(Result))^ do
  begin
    Nodes.Operand1 := AOperand1;
    Nodes.Sounds := ASounds;
    Nodes.Like := ALike;
    Nodes.Operand2 := AOperand2;

    Heritage.AddChildren(@Nodes, SizeOf(Nodes) div SizeOf(TOffset));
  end;
end;

{ TSQLParser.TStartSlaveStmt **************************************************}

class function TSQLParser.TStartSlaveStmt.Create(const AParser: TSQLParser; const ANodes: TNodes): TOffset;
begin
  Result := TStmt.Create(AParser, stStartSlave);

  with PStartSlaveStmt(AParser.NodePtr(Result))^ do
  begin
    Nodes := ANodes;

    Heritage.Heritage.AddChildren(@Nodes, SizeOf(Nodes) div SizeOf(TOffset));
  end;
end;

{ TSQLParser.TStartTransactionStmt ********************************************}

class function TSQLParser.TStartTransactionStmt.Create(const AParser: TSQLParser; const ANodes: TNodes): TOffset;
begin
  Result := TStmt.Create(AParser, stStartTransaction);

  with PStartTransactionStmt(AParser.NodePtr(Result))^ do
  begin
    Nodes := ANodes;

    Heritage.Heritage.AddChildren(@Nodes, SizeOf(Nodes) div SizeOf(TOffset));
  end;
end;

{ TSQLParser.TStopSlaveStmt ***************************************************}

class function TSQLParser.TStopSlaveStmt.Create(const AParser: TSQLParser; const ANodes: TNodes): TOffset;
begin
  Result := TStmt.Create(AParser, stStopSlave);

  with PStopSlaveStmt(AParser.NodePtr(Result))^ do
  begin
    Nodes := ANodes;

    Heritage.Heritage.AddChildren(@Nodes, SizeOf(Nodes) div SizeOf(TOffset));
  end;
end;

{ TSQLParser.TSubArea *********************************************************}

class function TSQLParser.TSubArea.Create(const AParser: TSQLParser; const ANodes: TNodes): TOffset;
begin
  Result := TRange.Create(AParser, ntSubArea);

  with PSubArea(AParser.NodePtr(Result))^ do
  begin
    Nodes := ANodes;

    Heritage.AddChildren(@Nodes, SizeOf(Nodes) div SizeOf(TOffset));
  end;
end;

{ TSQLParser.TSubAreaSelectStmt ***********************************************}

class function TSQLParser.TSubAreaSelectStmt.Create(const AParser: TSQLParser; const ANodes: TNodes): TOffset;
begin
  Result := TStmt.Create(AParser, stSubAreaSelect);

  with PSubAreaSelectStmt(AParser.NodePtr(Result))^ do
  begin
    Nodes := ANodes;

    Heritage.Heritage.AddChildren(@Nodes, SizeOf(Nodes) div SizeOf(TOffset));
  end;
end;

{ TSQLParser.TSubPartition ****************************************************}

class function TSQLParser.TSubPartition.Create(const AParser: TSQLParser; const ANodes: TNodes): TOffset;
begin
  Result := TRange.Create(AParser, ntSubPartition);

  with PSubPartition(AParser.NodePtr(Result))^ do
  begin
    Nodes := ANodes;

    Heritage.AddChildren(@Nodes, SizeOf(Nodes) div SizeOf(TOffset));
  end;
end;

{ TSQLParser.TTag *************************************************************}

class function TSQLParser.TTag.Create(const AParser: TSQLParser; const ANodes: TNodes): TOffset;
begin
  Result := TRange.Create(AParser, ntTag);

  with PTag(AParser.NodePtr(Result))^ do
  begin
    Nodes := ANodes;

    Heritage.AddChildren(@Nodes, SizeOf(Nodes) div SizeOf(TOffset));
  end;
end;

{ TSQLParser.TTruncateStmt ****************************************************}

class function TSQLParser.TTruncateStmt.Create(const AParser: TSQLParser; const ANodes: TNodes): TOffset;
begin
  Result := TStmt.Create(AParser, stTruncate);

  with PTruncateStmt(AParser.NodePtr(Result))^ do
  begin
    Nodes := ANodes;

    Heritage.Heritage.AddChildren(@Nodes, SizeOf(Nodes) div SizeOf(TOffset));
  end;
end;

{ TSQLParser.TTrimFunc ********************************************************}

class function TSQLParser.TTrimFunc.Create(const AParser: TSQLParser; const ANodes: TNodes): TOffset;
begin
  Result := TRange.Create(AParser, ntTrimFunc);

  with PTrimFunc(AParser.NodePtr(Result))^ do
  begin
    Nodes := ANodes;

    Heritage.AddChildren(@Nodes, SizeOf(Nodes) div SizeOf(TOffset));
  end;
end;

{ TSQLParser.TUnaryOp *********************************************************}

class function TSQLParser.TUnaryOp.Create(const AParser: TSQLParser; const AOperator, AOperand: TOffset): TOffset;
begin
  Result := TRange.Create(AParser, ntUnaryOp);

  with PUnaryOp(AParser.NodePtr(Result))^ do
  begin
    Nodes.Operator := AOperator;
    Nodes.Operand := AOperand;

    Heritage.AddChildren(@Nodes, SizeOf(Nodes) div SizeOf(TOffset));
  end;
end;

{ TSQLParser.TUnknownStmt *****************************************************}

class function TSQLParser.TUnknownStmt.Create(const AParser: TSQLParser; const ATokenCount: Integer; const ATokens: array of TOffset): TOffset;
var
  I: Integer;
begin
  Result := TStmt.Create(AParser, stUnknown);

  with PUnknownStmt(AParser.NodePtr(Result))^ do
  begin
    for I := 0 to ATokenCount - 1 do
      Heritage.Heritage.AddChildren(@ATokens[I], 1);
  end;
end;

{ TSQLParser.TUnlockStmt ******************************************************}

class function TSQLParser.TUnlockTablesStmt.Create(const AParser: TSQLParser; const ANodes: TNodes): TOffset;
begin
  Result := TStmt.Create(AParser, stUnlockTables);

  with PUnlockTablesStmt(AParser.NodePtr(Result))^ do
  begin
    Nodes := ANodes;

    Heritage.Heritage.AddChildren(@Nodes, SizeOf(Nodes) div SizeOf(TOffset));
  end;
end;

{ TSQLParser.TUpdateStmt ******************************************************}

class function TSQLParser.TUpdateStmt.Create(const AParser: TSQLParser; const ANodes: TNodes): TOffset;
begin
  Result := TStmt.Create(AParser, stUpdate);

  with PUpdateStmt(AParser.NodePtr(Result))^ do
  begin
    Nodes := ANodes;

    Heritage.Heritage.AddChildren(@Nodes, SizeOf(Nodes) div SizeOf(TOffset));
  end;
end;

{ TSQLParser.TUser ************************************************************}

class function TSQLParser.TUser.Create(const AParser: TSQLParser; const ANodes: TNodes): TOffset;
begin
  Result := TRange.Create(AParser, ntUser);

  with PUser(AParser.NodePtr(Result))^ do
  begin
    Nodes := ANodes;

    Heritage.AddChildren(@Nodes, SizeOf(Nodes) div SizeOf(TOffset));
  end;
end;

{ TSQLParser.TUseStmt *********************************************************}

class function TSQLParser.TUseStmt.Create(const AParser: TSQLParser; const ANodes: TNodes): TOffset;
begin
  Result := TStmt.Create(AParser, stUse);

  with PUseStmt(AParser.NodePtr(Result))^ do
  begin
    Nodes := ANodes;

    Heritage.Heritage.AddChildren(@Nodes, SizeOf(Nodes) div SizeOf(TOffset));
  end;
end;

{ TSQLParser.TValue ***********************************************************}

class function TSQLParser.TValue.Create(const AParser: TSQLParser; const ANodes: TNodes): TOffset;
begin
  Result := TRange.Create(AParser, ntValue);

  with PValue(AParser.NodePtr(Result))^ do
  begin
    Nodes := ANodes;

    Heritage.AddChildren(@Nodes, SizeOf(Nodes) div SizeOf(TOffset));
  end;
end;

{ TSQLParser.TVariable ********************************************************}

class function TSQLParser.TVariable.Create(const AParser: TSQLParser; const ANodes: TNodes): TOffset;
begin
  Result := TRange.Create(AParser, ntVariableIdent);

  with PVariable(AParser.NodePtr(Result))^ do
  begin
    Nodes := ANodes;

    Heritage.AddChildren(@Nodes, SizeOf(Nodes) div SizeOf(TOffset));
  end;
end;

{ TSQLParser.TWeightStringFunc ************************************************}

class function TSQLParser.TWeightStringFunc.Create(const AParser: TSQLParser; const ANodes: TNodes): TOffset;
begin
  Result := TRange.Create(AParser, ntWeightStringFunc);

  with PWeightStringFunc(AParser.NodePtr(Result))^ do
  begin
    Nodes := ANodes;

    Heritage.AddChildren(@Nodes, SizeOf(Nodes) div SizeOf(TOffset));
  end;
end;

{ TSQLParser.TWeightStringFunc.TLevel *****************************************}

class function TSQLParser.TWeightStringFunc.TLevel.Create(const AParser: TSQLParser; const ANodes: TLevel.TNodes): TOffset;
begin
  Result := TRange.Create(AParser, ntWeightStringFuncLevel);

  with TWeightStringFunc.PLevel(AParser.NodePtr(Result))^ do
  begin
    Nodes := ANodes;

    Heritage.AddChildren(@Nodes, SizeOf(Nodes) div SizeOf(TOffset));
  end;
end;

{ TSQLParser.TWhileStmt *******************************************************}

class function TSQLParser.TWhileStmt.Create(const AParser: TSQLParser; const ANodes: TNodes): TOffset;
begin
  Result := TStmt.Create(AParser, stWhile);

  with PWhileStmt(AParser.NodePtr(Result))^ do
  begin
    Nodes := ANodes;

    Heritage.Heritage.AddChildren(@Nodes, SizeOf(Nodes) div SizeOf(TOffset));
  end;
end;

{ TSQLParser.TXAStmt **********************************************************}

class function TSQLParser.TXAStmt.Create(const AParser: TSQLParser; const ANodes: TNodes): TOffset;
begin
  Result := TStmt.Create(AParser, stXA);

  with PXAStmt(AParser.NodePtr(Result))^ do
  begin
    Nodes := ANodes;

    Heritage.Heritage.AddChildren(@Nodes, SizeOf(Nodes) div SizeOf(TOffset));
  end;
end;

{ TSQLParser.TXAStmt.TID ******************************************************}

class function TSQLParser.TXAStmt.TID.Create(const AParser: TSQLParser; const ANodes: TID.TNodes): TOffset;
begin
  Result := TRange.Create(AParser, ntXAStmtID);

  with PID(AParser.NodePtr(Result))^ do
  begin
    Nodes := ANodes;

    Heritage.AddChildren(@Nodes, SizeOf(Nodes) div SizeOf(TOffset));
  end;
end;

{ TSQLParser ******************************************************************}

function TSQLParser.ApplyCurrentToken(): TOffset;
begin
  Result := ApplyCurrentToken(utUnknown);
end;

function TSQLParser.ApplyCurrentToken(const AUsageType: TUsageType): TOffset;
begin
  Result := CurrentToken;

  if (Result > 0) then
  begin
    if (AUsageType <> utUnknown) then
      TokenPtr(Result)^.FUsageType := AUsageType;

    Dec(TokenBuffer.Count);
    Move(TokenBuffer.Tokens[1], TokenBuffer.Tokens[0], TokenBuffer.Count * SizeOf(TokenBuffer.Tokens[0]));

    FPreviousToken := FCurrentToken;
    FCurrentToken := GetParsedToken(0); // Cache for speeding

    CompletionList.Clear();
  end;
end;

procedure TSQLParser.BeginPL_SQL();
begin
  Inc(FInPL_SQL);
end;

function TSQLParser.ChildPtr(const ANode: TOffset): PChild;
begin
  if (not IsChild(NodePtr(ANode))) then
    Result := nil
  else
    Result := @Nodes.Mem[ANode];
end;

procedure TSQLParser.Clear();
begin
  FCompletionList.Clear();
  FErrorCode := PE_Success;
  FErrorLine := 1;
  FErrorToken := 0;
  {$IFDEF Debug} TokenIndex := 0; {$ENDIF}
  FInPL_SQL := 0;
  if (Nodes.MemSize <> DefaultParsedNodesMemSize) then
  begin
    Nodes.MemSize := DefaultParsedNodesMemSize;
    ReallocMem(Nodes.Mem, Nodes.MemSize);
  end;
  Nodes.UsedSize := 1; // "0" means "not assigned", so we start with "1"
  ParsedText := '';
  if (Texts.MemSize <> DefaultTextsMemSize) then
  begin
    Texts.MemSize := DefaultTextsMemSize;
    ReallocMem(Texts.Mem, Texts.MemSize);
  end;
  Texts.UsedSize := 1; // "0" means "not assigned", so we start with "1"
  FRoot := 0;
  InCreateFunctionStmt := False;
  InCreateProcedureStmt := False;
  AllowedMySQLVersion := 0;
  TokenBuffer.Count := 0;
end;

constructor TSQLParser.Create(const AMySQLVersion: Integer = 0);
begin
  inherited Create();

  AllowedMySQLVersion := 0;
  Commands := nil;
  FCompletionList := TCompletionList.Create(Self);
  DatatypeList := TWordList.Create(Self);
  FAnsiQuotes := False;
  FunctionList := TWordList.Create(Self);
  TokenIndex := 0;
  KeywordList := TWordList.Create(Self);
  FMySQLVersion := AMySQLVersion;
  Nodes.Mem := nil;
  Nodes.UsedSize := 0;
  Nodes.MemSize := 0;
  Texts.Mem := nil;
  Texts.UsedSize := 0;
  Texts.MemSize := 0;
  TokenBuffer.Count := 0;

  Datatypes := MySQLDatatypes;
  Functions := MySQLFunctions;
  Keywords := MySQLKeywords;

  Clear();
end;

destructor TSQLParser.Destroy();
begin
  Clear();

  if (Nodes.MemSize <> 0) then
    FreeMem(Nodes.Mem);
  if (Texts.MemSize <> 0) then
    FreeMem(Texts.Mem, 0);

  FCompletionList.Free();
  DatatypeList.Free();
  FunctionList.Free();
  KeywordList.Free();

  inherited;
end;

function TSQLParser.EndOfStmt(const Token: PToken): Boolean;
begin
  Result := Token^.TokenType = ttSemicolon;
end;

function TSQLParser.EndOfStmt(const Token: TOffset): Boolean;
begin
  Result := (Token = 0) or (TokenPtr(Token)^.TokenType = ttSemicolon);
end;

procedure TSQLParser.EndPL_SQL();
begin
  Assert(FInPL_SQL > 0);

  if (FInPL_SQL > 0) then
    Dec(FInPL_SQL);
end;

procedure TSQLParser.FormatAlterDatabaseStmt(const Nodes: TAlterDatabaseStmt.TNodes);
begin
  FormatNode(Nodes.StmtTag);
  FormatNode(Nodes.Ident, stSpaceBefore);

  if (Nodes.UpgradeDataDirectoryNameTag = 0) then
  begin
    FormatNode(Nodes.CharsetValue, stSpaceBefore);
    FormatNode(Nodes.CollateValue, stSpaceBefore);
  end
  else
    FormatNode(Nodes.UpgradeDataDirectoryNameTag, stSpaceBefore);
end;

procedure TSQLParser.FormatAlterEventStmt(const Nodes: TAlterEventStmt.TNodes);
begin
  FormatNode(Nodes.AlterTag);
  FormatNode(Nodes.DefinerValue, stSpaceBefore);
  FormatNode(Nodes.EventTag, stSpaceBefore);
  FormatNode(Nodes.EventIdent, stSpaceBefore);

  if (Nodes.OnSchedule.Tag > 0) then
  begin
    Assert(Nodes.OnSchedule.Value > 0);

    Commands.IncreaseIndent();
    FormatNode(Nodes.OnSchedule.Tag, stReturnBefore);
    Commands.IncreaseIndent();
    FormatNode(Nodes.OnSchedule.Value, stReturnBefore);
    Commands.DecreaseIndent();
    Commands.DecreaseIndent();
  end;

  if (Nodes.OnCompletionTag > 0) then
  begin
    Commands.IncreaseIndent();
    FormatNode(Nodes.OnCompletionTag, stReturnBefore);
    Commands.DecreaseIndent();
  end;

  if (Nodes.RenameValue > 0) then
  begin
    Commands.IncreaseIndent();
    FormatNode(Nodes.RenameValue, stReturnBefore);
    Commands.DecreaseIndent();
  end;

  if (Nodes.EnableTag > 0) then
  begin
    Commands.IncreaseIndent();
    FormatNode(Nodes.EnableTag, stReturnBefore);
    Commands.DecreaseIndent();
  end;

  if (Nodes.CommentValue > 0) then
  begin
    Commands.IncreaseIndent();
    FormatNode(Nodes.CommentValue, stReturnBefore);
    Commands.DecreaseIndent();
  end;

  if (Nodes.DoTag > 0) then
  begin
    Assert(Nodes.Body > 0);

    Commands.IncreaseIndent();
    FormatNode(Nodes.DoTag, stReturnBefore);
    Commands.IncreaseIndent();
    FormatNode(Nodes.Body, stReturnBefore);
    Commands.DecreaseIndent();
    Commands.DecreaseIndent();
  end;
end;

procedure TSQLParser.FormatAlterRoutineStmt(const Nodes: TAlterRoutineStmt.TNodes);
begin
  FormatNode(Nodes.AlterTag);
  FormatNode(Nodes.Ident, stSpaceBefore);
  Commands.IncreaseIndent();
  FormatNode(Nodes.CommentValue, stReturnBefore);
  FormatNode(Nodes.LanguageTag, stReturnBefore);
  FormatNode(Nodes.DeterministicTag, stReturnBefore);
  FormatNode(Nodes.NatureOfDataTag, stReturnBefore);
  FormatNode(Nodes.SQLSecurityTag, stReturnBefore);
  Commands.DecreaseIndent();
end;

procedure TSQLParser.FormatAlterTablespaceStmt(const Nodes: TAlterTablespaceStmt.TNodes);
begin
  FormatNode(Nodes.StmtTag);
  FormatNode(Nodes.Ident, stSpaceBefore);
  Commands.IncreaseIndent();
  FormatNode(Nodes.AddDatafileValue, stReturnBefore);
  FormatNode(Nodes.InitialSizeValue, stReturnBefore);
  FormatNode(Nodes.EngineValue, stReturnBefore);
  Commands.DecreaseIndent();
end;

procedure TSQLParser.FormatAlterTableStmt(const Nodes: TAlterTableStmt.TNodes);
begin
  FormatNode(Nodes.AlterTag);
  FormatNode(Nodes.IgnoreTag, stSpaceBefore);
  FormatNode(Nodes.TableTag, stSpaceBefore);
  FormatNode(Nodes.Ident, stSpaceBefore);
  if (Nodes.SpecificationList > 0) then
  begin
    Commands.IncreaseIndent();
    Commands.WriteReturn();
    FormatList(Nodes.SpecificationList, sReturn);
    Commands.DecreaseIndent();
  end;
end;

procedure TSQLParser.FormatAlterViewStmt(const Nodes: TAlterViewStmt.TNodes);
begin
  FormatNode(Nodes.AlterTag);
  Commands.IncreaseIndent();
  FormatNode(Nodes.AlgorithmValue, stReturnBefore);
  FormatNode(Nodes.DefinerValue, stReturnBefore);
  FormatNode(Nodes.SQLSecurityTag, stReturnBefore);
  FormatNode(Nodes.ViewTag, stReturnBefore);
  FormatNode(Nodes.Ident, stSpaceBefore);
  FormatNode(Nodes.Columns, stSpaceBefore);
  Commands.IncreaseIndent();
  FormatNode(Nodes.AsTag, stReturnBefore);
  Commands.IncreaseIndent();
  FormatNode(Nodes.SelectStmt, stReturnBefore);
  Commands.DecreaseIndent();
  Commands.DecreaseIndent();
  FormatNode(Nodes.OptionTag, stReturnBefore);
  Commands.DecreaseIndent();
end;

procedure TSQLParser.FormatBeginLabel(const Nodes: TBeginLabel.TNodes);
begin
  FormatNode(Nodes.BeginToken);
  FormatNode(Nodes.ColonToken);
end;

procedure TSQLParser.FormatCallStmt(const Nodes: TCallStmt.TNodes);
begin
  FormatNode(Nodes.CallTag);
  FormatNode(Nodes.Ident, stSpaceBefore);
  FormatNode(Nodes.ParamList);
end;

procedure TSQLParser.FormatCaseOp(const Nodes: TCaseOp.TNodes);
begin
  FormatNode(Nodes.CaseTag);
  FormatNode(Nodes.CompareExpr, stSpaceBefore);
  Commands.IncreaseIndent();
  Commands.WriteReturn();
  FormatList(Nodes.BranchList, sReturn);
  FormatNode(Nodes.ElseTag, stReturnBefore);
  FormatNode(Nodes.ElseExpr, stSpaceBefore);
  Commands.DecreaseIndent();
  FormatNode(Nodes.EndTag, stReturnBefore);
end;

procedure TSQLParser.FormatCaseStmt(const Nodes: TCaseStmt.TNodes);
begin
  FormatNode(Nodes.CaseTag);
  FormatNode(Nodes.CompareExpr, stSpaceBefore);
  Commands.IncreaseIndent();
  Commands.WriteReturn();
  FormatList(Nodes.BranchList, sReturn);
  Commands.DecreaseIndent();
  FormatNode(Nodes.EndTag, stReturnBefore);
end;

procedure TSQLParser.FormatCaseStmtBranch(const Nodes: TCaseStmt.TBranch.TNodes);
begin
  Assert(NodePtr(Nodes.Tag)^.NodeType = ntTag);

  Commands.IncreaseIndent();
  FormatNode(Nodes.Tag);
  if ((TokenPtr(PTag(NodePtr(Nodes.Tag))^.Nodes.KeywordToken1).KeywordIndex = kiWHEN)) then
  begin
    FormatNode(Nodes.ConditionExpr, stSpaceBefore);
    FormatNode(Nodes.ThenTag, stSpaceBefore);
  end;
  Commands.WriteReturn();
  FormatList(Nodes.StmtList, sReturn);
  Commands.DecreaseIndent();
end;

procedure TSQLParser.FormatCastFunc(const Nodes: TCastFunc.TNodes);
begin
  FormatNode(Nodes.IdentToken);
  FormatNode(Nodes.OpenBracket);
  FormatNode(Nodes.Expr);
  FormatNode(Nodes.AsTag, stSpaceBefore);
  FormatNode(Nodes.Datatype, stSpaceBefore);
  FormatNode(Nodes.CloseBracket);
end;

procedure TSQLParser.FormatCharFunc(const Nodes: TCharFunc.TNodes);
begin
  FormatNode(Nodes.IdentToken);
  FormatNode(Nodes.OpenBracket, stSpaceBefore);
  FormatNode(Nodes.ValueExpr, stSpaceBefore);
  if (Nodes.UsingTag > 0) then
  begin
    FormatNode(Nodes.UsingTag, stSpaceBefore);
    FormatNode(Nodes.CharsetIdent, stSpaceBefore);
  end;
  FormatNode(Nodes.CloseBracket, stSpaceBefore);
end;

procedure TSQLParser.FormatChangeMasterStmt(const Nodes: TChangeMasterStmt.TNodes);
begin
  FormatNode(Nodes.StmtTag);
  FormatNode(Nodes.ToTag, stSpaceBefore);
  Commands.IncreaseIndent();
  Commands.WriteReturn();
  FormatList(Nodes.OptionList, sReturn);
  FormatNode(Nodes.ChannelOptionValue, stReturnBefore);
  Commands.DecreaseIndent();
end;

procedure TSQLParser.FormatCompoundStmt(const Nodes: TCompoundStmt.TNodes);
begin
  FormatNode(Nodes.BeginLabel, stSpaceAfter);
  Commands.IncreaseIndent();
  FormatNode(Nodes.BeginTag, stReturnAfter);
  FormatList(Nodes.StmtList, sReturn);
  Commands.DecreaseIndent();
  FormatNode(Nodes.EndTag, stReturnBefore);
  FormatNode(Nodes.EndLabel, stSpaceBefore);
end;

procedure TSQLParser.FormatConvertFunc(const Nodes: TConvertFunc.TNodes);
begin
  FormatNode(Nodes.IdentToken);
  FormatNode(Nodes.OpenBracket);
  FormatNode(Nodes.Expr);
  if (Nodes.Comma > 0) then
  begin
    FormatNode(Nodes.Comma);
    FormatNode(Nodes.Datatype, stSpaceBefore);
  end
  else
  begin
    FormatNode(Nodes.UsingTag, stSpaceBefore);
    FormatNode(Nodes.CharsetIdent, stSpaceBefore);
  end;
  FormatNode(Nodes.CloseBracket);
end;

procedure TSQLParser.FormatCreateEventStmt(const Nodes: TCreateEventStmt.TNodes);
begin
  FormatNode(Nodes.CreateTag);
  FormatNode(Nodes.DefinerNode, stSpaceBefore);
  FormatNode(Nodes.EventTag, stSpaceBefore);
  FormatNode(Nodes.IfNotExistsTag, stSpaceBefore);
  FormatNode(Nodes.EventIdent, stSpaceBefore);
  Commands.IncreaseIndent();
  FormatNode(Nodes.OnScheduleValue, stReturnBefore);
  FormatNode(Nodes.OnCompletitionTag, stReturnBefore);
  FormatNode(Nodes.EnableTag, stReturnBefore);
  FormatNode(Nodes.CommentValue, stReturnBefore);
  FormatNode(Nodes.DoTag, stReturnBefore);
  Commands.DecreaseIndent();
  FormatNode(Nodes.Body, stReturnBefore);
end;

procedure TSQLParser.FormatCreateIndexStmt(const Nodes: TCreateIndexStmt.TNodes);
begin
  FormatNode(Nodes.CreateTag);
  FormatNode(Nodes.IndexTag, stSpaceBefore);
  FormatNode(Nodes.Ident, stSpaceBefore);
  Commands.IncreaseIndent();
  FormatNode(Nodes.IndexTypeValue, stReturnBefore);
  FormatNode(Nodes.OnTag, stReturnBefore);
  FormatNode(Nodes.TableIdent, stSpaceBefore);
  FormatNode(Nodes.KeyColumnList, stSpaceBefore);
  FormatNode(Nodes.AlgorithmLockValue, stReturnBefore);
  FormatNode(Nodes.CommentValue, stReturnBefore);
  FormatNode(Nodes.KeyBlockSizeValue, stReturnBefore);
  FormatNode(Nodes.ParserValue, stReturnBefore);
  Commands.DecreaseIndent();
end;

procedure TSQLParser.FormatCreateRoutineStmt(const Nodes: TCreateRoutineStmt.TNodes);
begin
  FormatNode(Nodes.CreateTag);
  FormatNode(Nodes.DefinerNode, stSpaceBefore);
  FormatNode(Nodes.RoutineTag, stSpaceBefore);
  FormatNode(Nodes.Ident, stSpaceBefore);
  if ((Nodes.ParameterList = 0)
    or (NodePtr(Nodes.ParameterList)^.NodeType <> ntList)
    or (PList(NodePtr(Nodes.ParameterList))^.ElementCount <= 3)) then
  begin
    FormatNode(Nodes.OpenBracket);
    FormatNode(Nodes.ParameterList);
    FormatNode(Nodes.CloseBracket);
  end
  else
  begin
    FormatNode(Nodes.OpenBracket);
    Commands.IncreaseIndent();
    Commands.WriteReturn();
    FormatList(Nodes.ParameterList, sReturn);
    Commands.DecreaseIndent();
    FormatNode(Nodes.CloseBracket, stReturnBefore);
  end;
  FormatNode(Nodes.Returns, stSpaceBefore);
  Commands.IncreaseIndent();
  FormatNode(Nodes.CommentValue, stReturnBefore);
  FormatNode(Nodes.LanguageTag, stReturnBefore);
  FormatNode(Nodes.DeterministicTag, stReturnBefore);
  FormatNode(Nodes.NatureOfDataTag, stReturnBefore);
  FormatNode(Nodes.SQLSecurityTag, stReturnBefore);
  Commands.DecreaseIndent();
  FormatNode(Nodes.Body, stReturnBefore);
end;

procedure TSQLParser.FormatCreateServerStmt(const Nodes: TCreateServerStmt.TNodes);
begin
  FormatNode(Nodes.StmtTag);
  FormatNode(Nodes.Ident, stSpaceBefore);
  Commands.IncreaseIndent();
  FormatNode(Nodes.ForeignDataWrapperValue, stReturnBefore);
  FormatNode(Nodes.Options.Tag, stReturnBefore);
  FormatNode(Nodes.Options.List, stSpaceBefore);
  Commands.DecreaseIndent();
end;

procedure TSQLParser.FormatCreateTablespaceStmt(const Nodes: TCreateTablespaceStmt.TNodes);
begin
  FormatNode(Nodes.StmtTag);
  FormatNode(Nodes.Ident, stSpaceBefore);
  Commands.IncreaseIndent();
  FormatNode(Nodes.AddDatafileValue, stReturnBefore);
  FormatNode(Nodes.FileBlockSizeValue, stReturnBefore);
  FormatNode(Nodes.EngineValue, stReturnBefore);
  Commands.DecreaseIndent();
end;

procedure TSQLParser.FormatCreateTableStmt(const Nodes: TCreateTableStmt.TNodes);
begin
  FormatNode(Nodes.CreateTag);
  FormatNode(Nodes.TemporaryTag, stSpaceBefore);
  FormatNode(Nodes.TableTag, stSpaceBefore);
  FormatNode(Nodes.IfNotExistsTag, stSpaceBefore);
  FormatNode(Nodes.TableIdent, stSpaceBefore);
  FormatNode(Nodes.OpenBracket, stSpaceBefore);
  if (Nodes.DefinitionList > 0) then
  begin
    Commands.IncreaseIndent();
    Commands.WriteReturn();
    FormatList(Nodes.DefinitionList, sReturn);
    Commands.DecreaseIndent();
    Commands.WriteReturn();
  end
  else if (Nodes.LikeTag > 0) then
  begin
    Commands.IncreaseIndent();
    FormatNode(Nodes.LikeTag, stReturnBefore);
    FormatNode(Nodes.LikeTableIdent, stSpaceBefore);
    Commands.DecreaseIndent();
  end
  else if (Nodes.SelectStmt1 > 0) then
  begin
    Commands.IncreaseIndent();
    Commands.WriteReturn();
    FormatNode(Nodes.SelectStmt1);
    Commands.DecreaseIndent();
    Commands.WriteReturn();
  end;
  FormatNode(Nodes.CloseBracket);
  FormatNode(Nodes.TableOptionsNodes.EngineValue, stSpaceBefore);
  FormatNode(Nodes.TableOptionsNodes.AutoIncrementValue, stSpaceBefore);
  FormatNode(Nodes.TableOptionsNodes.AvgRowLengthValue, stSpaceBefore);
  FormatNode(Nodes.TableOptionsNodes.CharacterSetValue, stSpaceBefore);
  FormatNode(Nodes.TableOptionsNodes.ChecksumValue, stSpaceBefore);
  FormatNode(Nodes.TableOptionsNodes.CollateValue, stSpaceBefore);
  FormatNode(Nodes.TableOptionsNodes.CommentValue, stSpaceBefore);
  FormatNode(Nodes.TableOptionsNodes.CompressValue, stSpaceBefore);
  FormatNode(Nodes.TableOptionsNodes.ConnectionValue, stSpaceBefore);
  FormatNode(Nodes.TableOptionsNodes.DataDirectoryValue, stSpaceBefore);
  FormatNode(Nodes.TableOptionsNodes.DelayKeyWriteValue, stSpaceBefore);
  FormatNode(Nodes.TableOptionsNodes.IndexDirectoryValue, stSpaceBefore);
  FormatNode(Nodes.TableOptionsNodes.InsertMethodValue, stSpaceBefore);
  FormatNode(Nodes.TableOptionsNodes.KeyBlockSizeValue, stSpaceBefore);
  FormatNode(Nodes.TableOptionsNodes.MaxRowsValue, stSpaceBefore);
  FormatNode(Nodes.TableOptionsNodes.MinRowsValue, stSpaceBefore);
  FormatNode(Nodes.TableOptionsNodes.PackKeysValue, stSpaceBefore);
  FormatNode(Nodes.TableOptionsNodes.PageChecksumValue, stSpaceBefore);
  FormatNode(Nodes.TableOptionsNodes.PasswordValue, stSpaceBefore);
  FormatNode(Nodes.TableOptionsNodes.RowFormatValue, stSpaceBefore);
  FormatNode(Nodes.TableOptionsNodes.StatsAutoRecalcValue, stSpaceBefore);
  FormatNode(Nodes.TableOptionsNodes.StatsPersistentValue, stSpaceBefore);
  FormatNode(Nodes.TableOptionsNodes.TransactionalValue, stSpaceBefore);
  FormatNode(Nodes.TableOptionsNodes.UnionList, stSpaceBefore);
  if (Nodes.PartitionOption.Tag > 0) then
  begin
    Commands.IncreaseIndent();
    FormatNode(Nodes.PartitionOption.Tag, stReturnBefore);
    Commands.IncreaseIndent();
    FormatNode(Nodes.PartitionOption.KindTag, stReturnBefore);
    FormatNode(Nodes.PartitionOption.Expr, stSpaceBefore);
    FormatNode(Nodes.PartitionOption.AlgorithmValue, stSpaceBefore);
    FormatNode(Nodes.PartitionOption.Columns.Tag, stSpaceBefore);
    FormatNode(Nodes.PartitionOption.Columns.List, stSpaceBefore);
    Commands.DecreaseIndent();
    FormatNode(Nodes.PartitionOption.Value);
    if (Nodes.PartitionOption.SubPartition.Tag > 0) then
    begin
      FormatNode(Nodes.PartitionOption.SubPartition.Tag, stReturnBefore);
      Commands.IncreaseIndent();
      FormatNode(Nodes.PartitionOption.SubPartition.KindTag, stReturnBefore);
      FormatNode(Nodes.PartitionOption.SubPartition.Expr, stSpaceBefore);
      FormatNode(Nodes.PartitionOption.SubPartition.AlgorithmValue, stSpaceBefore);
      FormatNode(Nodes.PartitionOption.SubPartition.ColumnList, stSpaceBefore);
      Commands.DecreaseIndent();
      FormatNode(Nodes.PartitionOption.SubPartition.Value, stReturnBefore);
    end;
    Commands.IncreaseIndent();
    Commands.WriteReturn();
    FormatList(Nodes.PartitionDefinitionList, sReturn);
    Commands.DecreaseIndent();
    Commands.DecreaseIndent();
  end;
  FormatNode(Nodes.IgnoreReplaceTag, stReturnBefore);
  if (Nodes.SelectStmt2 > 0) then
  begin
    FormatNode(Nodes.AsTag);
    Commands.IncreaseIndent();
    FormatNode(Nodes.SelectStmt2, stReturnBefore);
    Commands.DecreaseIndent();
  end;
end;

procedure TSQLParser.FormatCreateTableStmtField(const Nodes: TCreateTableStmt.TField.TNodes);
begin
  FormatNode(Nodes.AddTag, stSpaceAfter);
  FormatNode(Nodes.ColumnTag);
  FormatNode(Nodes.OldNameIdent, stSpaceBefore);
  FormatNode(Nodes.NameIdent, stSpaceBefore);
  FormatNode(Nodes.Datatype, stSpaceBefore);
  if (Nodes.Virtual.AsTag = 0) then
  begin // Real field
    FormatNode(Nodes.NullTag, stSpaceBefore);
    FormatNode(Nodes.Real.DefaultValue, stSpaceBefore);
    FormatNode(Nodes.Real.AutoIncrementTag, stSpaceBefore);
    FormatNode(Nodes.KeyTag, stSpaceBefore);
    FormatNode(Nodes.CommentValue, stSpaceBefore);
    FormatNode(Nodes.Real.ColumnFormat, stSpaceBefore);
    FormatNode(Nodes.Real.StorageTag, stSpaceBefore);
    FormatNode(Nodes.Real.Reference, stSpaceBefore);
  end
  else
  begin // Virtual field
    FormatNode(Nodes.Virtual.GernatedAlwaysTag, stSpaceBefore);
    FormatNode(Nodes.Virtual.AsTag, stSpaceBefore);
    FormatNode(Nodes.Virtual.Expr, stSpaceBefore);
    FormatNode(Nodes.Virtual.Virtual, stSpaceBefore);
    FormatNode(Nodes.KeyTag, stSpaceBefore);
    FormatNode(Nodes.CommentValue, stSpaceBefore);
    FormatNode(Nodes.NullTag, stSpaceBefore);
  end;
  FormatNode(Nodes.Position, stSpaceBefore);
end;

procedure TSQLParser.FormatCreateTableStmtFieldDefaultFunc(const Nodes: TCreateTableStmt.TField.TDefaultFunc.TNodes);
begin
  FormatNode(Nodes.IdentToken);
  FormatNode(Nodes.OpenBracket);
  FormatNode(Nodes.CloseBracket);
end;

procedure TSQLParser.FormatCreateTableStmtKey(const Nodes: TCreateTableStmt.TKey.TNodes);
begin
  FormatNode(Nodes.AddTag, stSpaceAfter);
  FormatNode(Nodes.ConstraintTag, stSpaceAfter);
  FormatNode(Nodes.SymbolIdent, stSpaceAfter);
  FormatNode(Nodes.KeyTag);
  FormatNode(Nodes.KeyIdent, stSpaceBefore);
  FormatNode(Nodes.IndexTypeTag, stSpaceBefore);
  Commands.WriteSpace();
  FormatList(Nodes.KeyColumnList, sNone);
  FormatNode(Nodes.KeyBlockSizeValue, stSpaceBefore);
  FormatNode(Nodes.WithParserValue, stSpaceBefore);
  FormatNode(Nodes.CommentValue, stSpaceBefore);
end;

procedure TSQLParser.FormatCreateTableStmtKeyColumn(const Nodes: TCreateTableStmt.TKeyColumn.TNodes);
begin
  FormatNode(Nodes.IdentTag);

  if (Nodes.OpenBracket > 0) then
  begin
    FormatNode(Nodes.OpenBracket);
    FormatNode(Nodes.LengthToken);
    FormatNode(Nodes.CloseBracket);
  end;

  FormatNode(Nodes.SortTag, stSpaceBefore);
end;

procedure TSQLParser.FormatCreateTableStmtPartition(const Nodes: TCreateTableStmt.TPartition.TNodes);
begin
  FormatNode(Nodes.AddTag, stSpaceAfter);
  FormatNode(Nodes.PartitionTag);
  FormatNode(Nodes.NameIdent, stSpaceBefore);
  Commands.IncreaseIndent();
  FormatNode(Nodes.Values.Tag, stReturnBefore);
  FormatNode(Nodes.Values.Value, stSpaceBefore);
  FormatNode(Nodes.EngineValue, stReturnBefore);
  FormatNode(Nodes.CommentValue, stReturnBefore);
  FormatNode(Nodes.DataDirectoryValue, stReturnBefore);
  FormatNode(Nodes.IndexDirectoryValue, stReturnBefore);
  FormatNode(Nodes.MaxRowsValue, stReturnBefore);
  FormatNode(Nodes.MinRowsValue, stReturnBefore);
  FormatNode(Nodes.SubPartitionList, stReturnBefore);
  Commands.DecreaseIndent();
end;

procedure TSQLParser.FormatCreateTableStmtReference(const Nodes: TCreateTableStmt.TReference.TNodes);
begin
  FormatNode(Nodes.Tag);
  FormatNode(Nodes.ParentTableIdent, stSpaceBefore);
  FormatNode(Nodes.FieldList, stSpaceBefore);
  FormatNode(Nodes.MatchTag, stSpaceBefore);
  FormatNode(Nodes.OnDeleteTag, stSpaceBefore);
  FormatNode(Nodes.OnUpdateTag, stSpaceBefore);
end;

procedure TSQLParser.FormatCreateTriggerStmt(const Nodes: TCreateTriggerStmt.TNodes);
begin
  FormatNode(Nodes.CreateTag);
  Commands.IncreaseIndent();
  FormatNode(Nodes.DefinerNode, stSpaceBefore);
  FormatNode(Nodes.TriggerTag, stSpaceBefore);
  FormatNode(Nodes.TriggerIdent, stSpaceBefore);
  FormatNode(Nodes.TimeTag, stReturnBefore);
  FormatNode(Nodes.EventTag, stSpaceBefore);
  FormatNode(Nodes.OnTag, stReturnBefore);
  FormatNode(Nodes.TableIdentNode, stSpaceBefore);
  FormatNode(Nodes.ForEachRowTag, stReturnBefore);
  FormatNode(Nodes.OrderValue, stReturnBefore);
  Commands.DecreaseIndent();
  FormatNode(Nodes.Body, stReturnBefore);
end;

procedure TSQLParser.FormatCreateUserStmt(const Nodes: TCreateUserStmt.TNodes);
begin
  FormatNode(Nodes.StmtTag);
  FormatNode(Nodes.IfNotExistsTag, stSpaceBefore);
  Commands.IncreaseIndent();
  FormatNode(Nodes.UserSpecifications, stReturnBefore);
  if (Nodes.WithTag > 0) then
  begin
    FormatNode(Nodes.WithTag, stReturnBefore);
    FormatList(Nodes.MaxQueriesPerHour, sSpace);
    FormatList(Nodes.MaxUpdatesPerHour, sSpace);
    FormatList(Nodes.MaxConnectionsPerHour, sSpace);
    FormatList(Nodes.MaxUserConnections, sSpace);
  end;
  FormatNode(Nodes.PasswordOption, stReturnBefore);
  FormatNode(Nodes.AccountTag, stReturnBefore);
  Commands.DecreaseIndent();
end;

procedure TSQLParser.FormatCreateViewStmt(const Nodes: TCreateViewStmt.TNodes);
var
  MultiLines: Boolean;
begin
  MultiLines :=
    (Nodes.OrReplaceTag <> 0)
    or (Nodes.AlgorithmValue <> 0)
    or (Nodes.DefinerNode <> 0)
    or (Nodes.SQLSecurityTag <> 0);

  if (not MultiLines) then
  begin
    FormatNode(Nodes.CreateTag);
    FormatNode(Nodes.ViewTag, stSpaceBefore);
    FormatNode(Nodes.Ident, stSpaceBefore);
    FormatNode(Nodes.FieldList, stSpaceBefore);
    Commands.IncreaseIndent();
    FormatNode(Nodes.AsTag, stReturnBefore);
    Commands.IncreaseIndent();
    FormatNode(Nodes.SelectStmt, stReturnBefore);
    Commands.DecreaseIndent();
    FormatNode(Nodes.OptionTag, stReturnBefore);
    Commands.DecreaseIndent();
  end
  else
  begin
    FormatNode(Nodes.CreateTag);
    Commands.IncreaseIndent();
    FormatNode(Nodes.OrReplaceTag, stReturnBefore);
    FormatNode(Nodes.AlgorithmValue, stReturnBefore);
    FormatNode(Nodes.DefinerNode, stReturnBefore);
    FormatNode(Nodes.SQLSecurityTag, stReturnBefore);
    FormatNode(Nodes.ViewTag, stReturnBefore);
    FormatNode(Nodes.Ident, stSpaceBefore);
    FormatNode(Nodes.FieldList, stSpaceBefore);
    FormatNode(Nodes.AsTag, stReturnBefore);
    Commands.DecreaseIndent();
    FormatNode(Nodes.SelectStmt, stReturnBefore);
    Commands.IncreaseIndent();
    FormatNode(Nodes.OptionTag, stReturnBefore);
    Commands.DecreaseIndent();
  end;
end;

procedure TSQLParser.FormatCurrentTimestamp(const Nodes: TCurrentTimestamp.TNodes);
begin
  FormatNode(Nodes.CurrentTimestampTag);
  FormatNode(Nodes.OpenBracket);
  FormatNode(Nodes.LengthInteger);
  FormatNode(Nodes.CloseBracket);
end;

procedure TSQLParser.FormatComments(const Token: PToken; Start: Boolean = False);
var
  Comment: string;
  Index: Integer;
  Length: Integer;
  ReturnFound: Boolean;
  Spacer: TSpacer;
  T: PToken;
  Text: PChar;
begin
  ReturnFound := False; Spacer := sNone;
  T := Token;
  while (Assigned(T) and not T^.IsUsed) do
  begin
    case (T^.TokenType) of
      ttReturn:
        ReturnFound := True;
      ttSLComment:
        begin
          if (not Start) then
            if (not ReturnFound) then
              Commands.WriteSpace()
            else
              Commands.WriteReturn();
          T^.GetText(Text, Length);
          Commands.Write(Text, Length);
          ReturnFound := False;
          Spacer := sReturn;
          Start := False;
        end;
      ttMLComment:
        begin
          Comment := T^.AsString;
          if (Pos(#13#10, Comment) = 0) then
          begin
            if (not Start) then
              if (ReturnFound) then
                Commands.WriteReturn()
              else
                Commands.WriteSpace();
            Commands.Write('/* ');
            Commands.Write(Comment);
            Commands.Write(' */');

//            if (not Start) then
//              if (ReturnFound) then
//                Spacer := sReturn
//              else
//                Spacer := sSpace;
            ReturnFound := False;
          end
          else
          begin
            if (not Start) then
              Commands.WriteReturn();
            Commands.Write('/*');
            Commands.IncreaseIndent();
            Commands.WriteReturn();

            Index := Pos(#13#10, Comment);
            while (Index > 0) do
            begin
              Commands.Write(Copy(Comment, 1, Index - 1));
              Commands.WriteReturn();
              System.Delete(Comment, 1, Index + 1);
              Index := Pos(#13#10, Comment);
            end;
            Commands.Write(Trim(Comment));

            Commands.DecreaseIndent();
            Commands.WriteReturn();
            Commands.Write('*/');

            ReturnFound := False;
            Spacer := sReturn;
          end;
          Start := False;
        end;
    end;

    T := T^.NextTokenAll;
  end;

  case (Spacer) of
    sSpace:
      begin
        Commands.WriteSpace();
        CommentsWritten := True;
      end;
    sReturn:
      begin
        Commands.WriteReturn();
        CommentsWritten := True;
      end;
  end;
end;

procedure TSQLParser.FormatDatatype(const Nodes: TDatatype.TNodes);
begin
  FormatNode(Nodes.NationalToken, stSpaceAfter);

  FormatNode(Nodes.DatatypeToken);
  if (Nodes.OpenBracket > 0) then
  begin
    FormatNode(Nodes.OpenBracket);
    FormatNode(Nodes.LengthToken);
    FormatNode(Nodes.CommaToken);
    FormatNode(Nodes.DecimalsToken);
    FormatNode(Nodes.CloseBracket);
  end
  else if (Nodes.ItemsList > 0) then
    FormatList(Nodes.ItemsList, sNone);

  FormatNode(Nodes.SignedToken, stSpaceBefore);
  FormatNode(Nodes.ZerofillTag, stSpaceBefore);
  FormatNode(Nodes.CharacterSetValue, stSpaceBefore);
  FormatNode(Nodes.CollateValue, stSpaceBefore);
  FormatNode(Nodes.BinaryToken, stSpaceBefore);
  FormatNode(Nodes.ASCIIToken, stSpaceBefore);
  FormatNode(Nodes.UnicodeToken, stSpaceBefore);
end;

procedure TSQLParser.FormatDateAddFunc(const Nodes: TDateAddFunc.TNodes);
begin
  FormatNode(Nodes.IdentToken);
  FormatNode(Nodes.OpenBracket);
  FormatNode(Nodes.Expr);
  FormatNode(Nodes.CommaToken);
  if (Nodes.IntervalValue > 0) then
    FormatNode(Nodes.IntervalValue, stSpaceBefore)
  else
    FormatNode(Nodes.Days, stSpaceBefore);
  FormatNode(Nodes.CloseBracket);
end;

procedure TSQLParser.FormatDbIdent(const Nodes: TDbIdent.TNodes);
begin
  if (Nodes.DatabaseIdent > 0) then
  begin
    Assert(Nodes.DatabaseDot > 0);

    if (IsToken(Nodes.DatabaseIdent)) then
      if (not AnsiQuotes) then
        TokenPtr(Nodes.DatabaseIdent)^.TokenType := ttMySQLIdent
      else
        TokenPtr(Nodes.DatabaseIdent)^.TokenType := ttDQIdent;
    FormatNode(Nodes.DatabaseIdent);
    FormatNode(Nodes.DatabaseDot);
  end;

  if (Nodes.TableIdent > 0) then
  begin
    if (IsToken(Nodes.TableIdent)) then
      if (not AnsiQuotes) then
        TokenPtr(Nodes.TableIdent)^.TokenType := ttMySQLIdent
      else
        TokenPtr(Nodes.TableIdent)^.TokenType := ttDQIdent;
    FormatNode(Nodes.TableIdent);
    FormatNode(Nodes.TableDot);
  end;

  if (IsToken(Nodes.Ident) and (TokenPtr(Nodes.Ident)^.DbIdentType in [ditDatabase, ditTable, ditField, ditKey, ditForeignKey, ditProcedure, ditFunction, ditTrigger, ditEvent, ditPartition])) then
    if (not AnsiQuotes) then
      TokenPtr(Nodes.Ident)^.TokenType := ttMySQLIdent
    else
      TokenPtr(Nodes.Ident)^.TokenType := ttDQIdent;
  FormatNode(Nodes.Ident);
end;

procedure TSQLParser.FormatDeclareCursorStmt(const Nodes: TDeclareCursorStmt.TNodes);
begin
  FormatNode(Nodes.StmtTag);
  FormatNode(Nodes.Ident, stSpaceBefore);
  FormatNode(Nodes.CursorTag, stSpaceBefore);
  Commands.IncreaseIndent();
  FormatNode(Nodes.SelectStmt, stReturnBefore);
  Commands.DecreaseIndent();
end;

procedure TSQLParser.FormatDeclareHandlerStmt(const Nodes: TDeclareHandlerStmt.TNodes);
begin
  FormatNode(Nodes.StmtTag);
  FormatNode(Nodes.ActionTag, stSpaceBefore);
  FormatNode(Nodes.HandlerTag, stSpaceBefore);
  Commands.IncreaseIndent();
  FormatNode(Nodes.ForTag, stSpaceBefore);
  FormatNode(Nodes.ConditionsList, stSpaceBefore);
  FormatNode(Nodes.Stmt, stReturnBefore);
  Commands.DecreaseIndent();
end;

procedure TSQLParser.FormatDeleteStmt(const Nodes: TDeleteStmt.TNodes);
begin
  FormatNode(Nodes.DeleteTag);
  FormatNode(Nodes.LowPriorityTag, stSpaceBefore);
  FormatNode(Nodes.QuickTag, stSpaceBefore);
  FormatNode(Nodes.IgnoreTag, stSpaceBefore);

  if ((Nodes.From.Tag < Nodes.From.List)
    and (Nodes.Partition.List = 0)
    and (Nodes.Using.List = 0)
    and (Nodes.OrderBy.Tag = 0)
    and (Nodes.Limit.Tag = 0)) then
  begin
    FormatNode(Nodes.From.Tag, stSpaceBefore);
    FormatNode(Nodes.From.List, stSpaceBefore);
    FormatNode(Nodes.WhereValue, stSpaceBefore);
  end
  else if (Nodes.From.Tag < Nodes.From.List) then
  begin
    Commands.IncreaseIndent();
    FormatNode(Nodes.From.Tag, stReturnBefore);
    FormatNode(Nodes.From.List, stSpaceBefore);
    FormatNode(Nodes.Partition.Tag, stReturnBefore);
    FormatNode(Nodes.Partition.List, stSpaceBefore);
    FormatNode(Nodes.Using.Tag, stReturnBefore);
    FormatNode(Nodes.Using.List, stSpaceBefore);
    FormatNode(Nodes.WhereValue, stReturnBefore);
    FormatNode(Nodes.OrderBy.Tag, stReturnBefore);
    FormatNode(Nodes.OrderBy.Expr, stSpaceBefore);
    FormatNode(Nodes.Limit.Tag, stReturnBefore);
    FormatNode(Nodes.Limit.Expr, stSpaceBefore);
    Commands.DecreaseIndent();
  end
  else
  begin
    Commands.IncreaseIndent();
    FormatNode(Nodes.From.List, stReturnBefore);
    Commands.IncreaseIndent();
    Commands.WriteReturn();
    FormatNode(Nodes.From.Tag);
    FormatNode(Nodes.TableReferenceList, stReturnBefore);
    Commands.DecreaseIndent();
    FormatNode(Nodes.WhereValue, stReturnBefore);
    Commands.DecreaseIndent();
  end;
end;

procedure TSQLParser.FormatDropTablespaceStmt(const Nodes: TDropTablespaceStmt.TNodes);
begin
  FormatNode(Nodes.StmtTag);
  FormatNode(Nodes.Ident, stSpaceBefore);
  Commands.IncreaseIndent();
  FormatNode(Nodes.EngineValue, stReturnBefore);
  Commands.DecreaseIndent();
end;

procedure TSQLParser.FormatExistsFunc(const Nodes: TExistsFunc.TNodes);
begin
  FormatNode(Nodes.IdentToken);
  FormatNode(Nodes.OpenBracket);
  FormatNode(Nodes.SubQuery);
  FormatNode(Nodes.CloseBracket);
end;

procedure TSQLParser.FormatExtractFunc(const Nodes: TExtractFunc.TNodes);
begin
  FormatNode(Nodes.IdentToken);
  FormatNode(Nodes.OpenBracket);
  FormatNode(Nodes.UnitTag);
  FormatNode(Nodes.FromTag, stSpaceBefore);
  FormatNode(Nodes.DateExpr, stSpaceBefore);
  FormatNode(Nodes.CloseBracket);
end;

procedure TSQLParser.FormatFunctionCall(const Nodes: TFunctionCall.TNodes);
begin
  FormatNode(Nodes.Ident);
  FormatNode(Nodes.ArgumentsList);
end;

procedure TSQLParser.FormatGroupConcatFunc(const Nodes: TGroupConcatFunc.TNodes);
begin
  FormatNode(Nodes.IdentToken);
  FormatNode(Nodes.OpenBracket);
  FormatNode(Nodes.DistinctTag, stSpaceAfter);
  FormatNode(Nodes.ExprList);
  Commands.IncreaseIndent();
  if (Nodes.OrderByTag > 0) then
  begin
    FormatNode(Nodes.OrderByTag, stSpaceBefore);
    FormatNode(Nodes.OrderByExprList, stSpaceBefore);
  end;
  FormatNode(Nodes.SeparatorValue, stSpaceBefore);
  Commands.DecreaseIndent();
  FormatNode(Nodes.CloseBracket);
end;

procedure TSQLParser.FormatIfStmt(const Nodes: TIfStmt.TNodes);
begin
  FormatNode(Nodes.BranchList);
  FormatNode(Nodes.EndTag, stReturnBefore);
end;

procedure TSQLParser.FormatIfStmtBranch(const Nodes: TIfStmt.TBranch.TNodes);
begin
  Assert(NodePtr(Nodes.Tag)^.NodeType = ntTag);

  if (TokenPtr(PTag(NodePtr(Nodes.Tag))^.Nodes.KeywordToken1)^.KeywordIndex = kiIF) then
  begin
    FormatNode(Nodes.Tag);
    FormatNode(Nodes.ConditionExpr, stSpaceBefore);
    Commands.IncreaseIndent();
    FormatNode(Nodes.ThenTag, stSpaceBefore);
    FormatNode(Nodes.StmtList, stReturnBefore);
    Commands.DecreaseIndent();
  end
  else if (TokenPtr(PTag(NodePtr(Nodes.Tag))^.Nodes.KeywordToken1)^.KeywordIndex = kiELSEIF) then
  begin
    Commands.WriteReturn();
    Commands.IncreaseIndent();
    FormatNode(Nodes.Tag);
    FormatNode(Nodes.ConditionExpr, stSpaceBefore);
    FormatNode(Nodes.ThenTag, stSpaceBefore);
    FormatNode(Nodes.StmtList, stReturnBefore);
    Commands.DecreaseIndent();
  end
  else if (TokenPtr(PTag(NodePtr(Nodes.Tag))^.Nodes.KeywordToken1)^.KeywordIndex = kiELSE) then
  begin
    Commands.WriteReturn();
    Commands.IncreaseIndent();
    FormatNode(Nodes.Tag);
    FormatNode(Nodes.StmtList, stReturnBefore);
    Commands.DecreaseIndent();
  end;
end;

procedure TSQLParser.FormatInsertStmt(const Nodes: TInsertStmt.TNodes);
begin
  if ((Nodes.ColumnList = 0)
    and (Nodes.Values.List > 0)
    and (NodePtr(Nodes.Values.List)^.NodeType = ntList)
    and (PList(NodePtr(Nodes.Values.List))^.ElementCount <= 1)) then
  begin
    FormatNode(Nodes.InsertTag);
    FormatNode(Nodes.PriorityTag, stSpaceBefore);
    FormatNode(Nodes.IgnoreTag, stSpaceBefore);
    FormatNode(Nodes.IntoTag, stSpaceBefore);
    FormatNode(Nodes.TableIdent, stSpaceBefore);
    FormatNode(Nodes.Partition.Tag, stSpaceBefore);
    FormatNode(Nodes.Partition.List, stSpaceBefore);
    FormatNode(Nodes.ColumnList, stSpaceBefore);
    FormatNode(Nodes.Values.Tag, stSpaceBefore);
    FormatNode(Nodes.Values.List, stSpaceBefore);
    FormatNode(Nodes.Set_.Tag, stSpaceBefore);
    FormatNode(Nodes.Set_.List, stSpaceBefore);
    if (Nodes.SelectStmt > 0) then
    begin
      Commands.IncreaseIndent();
      FormatNode(Nodes.SelectStmt, stReturnBefore);
      Commands.DecreaseIndent();
      FormatNode(Nodes.OnDuplicateKeyUpdateTag, stReturnBefore);
    end
    else
      FormatNode(Nodes.OnDuplicateKeyUpdateTag, stSpaceBefore);
    FormatNode(Nodes.UpdateList, stSpaceBefore);
  end
  else
  begin
    FormatNode(Nodes.InsertTag);
    FormatNode(Nodes.PriorityTag, stSpaceBefore);
    FormatNode(Nodes.IgnoreTag, stSpaceBefore);
    Commands.IncreaseIndent();
    FormatNode(Nodes.IntoTag, stReturnBefore);
    FormatNode(Nodes.TableIdent, stSpaceBefore);
    FormatNode(Nodes.Partition.Tag, stSpaceBefore);
    FormatNode(Nodes.Partition.List, stSpaceBefore);
    FormatNode(Nodes.ColumnList, stReturnBefore);
    Commands.DecreaseIndent();
    if (Nodes.Values.Tag > 0) then
    begin
      FormatNode(Nodes.Values.Tag, stReturnBefore);
      Commands.IncreaseIndent();
      Commands.WriteReturn();
      FormatList(Nodes.Values.List, sReturn);
      Commands.DecreaseIndent();
    end;
    if (Nodes.Set_.Tag > 0) then
    begin
      FormatNode(Nodes.Set_.Tag, stReturnBefore);
      Commands.IncreaseIndent();
      Commands.WriteReturn();
      FormatList(Nodes.Set_.List, sReturn);
      Commands.DecreaseIndent();
    end;
    if (Nodes.SelectStmt > 0) then
    begin
      Commands.IncreaseIndent();
      FormatNode(Nodes.SelectStmt, stReturnBefore);
      Commands.DecreaseIndent();
      FormatNode(Nodes.OnDuplicateKeyUpdateTag, stReturnBefore);
    end
    else
      FormatNode(Nodes.OnDuplicateKeyUpdateTag, stSpaceBefore);
    FormatNode(Nodes.UpdateList, stSpaceBefore);
  end;
end;

procedure TSQLParser.FormatList(const Nodes: TList.TNodes; const DelimiterType: TTokenType);
begin
  case (DelimiterType) of
    ttComma: FormatList(Nodes, sSpace);
    ttDot: FormatList(Nodes, sNone);
    ttSemicolon: FormatList(Nodes, sReturn);
    else FormatList(Nodes, sSpace);
  end;
end;

procedure TSQLParser.FormatList(const Nodes: TList.TNodes; const Spacer: TSpacer);
var
  Child: PChild;
  Delimiter: PToken;
begin
  FormatNode(Nodes.OpenBracket);

  Child := ChildPtr(Nodes.FirstChild);

  while (Assigned(Child)) do
  begin
    FormatNode(PNode(Child));

    Delimiter := Child^.Delimiter;
    FormatNode(PNode(Delimiter));

    Child := Child^.NextSibling;

    if (Assigned(Child)) then
      case (Spacer) of
        sSpace:
          Commands.WriteSpace();
        sReturn:
          if (not Commands.NewLine) then
            Commands.WriteReturn();
      end;
  end;

  FormatNode(Nodes.CloseBracket);
end;

procedure TSQLParser.FormatList(const Node: TOffset; const Spacer: TSpacer);
begin
  if (Node > 0) then
  begin
    Assert(NodePtr(Node)^.NodeType = ntList);

    FormatList(PList(NodePtr(Node))^.Nodes, Spacer);
  end;
end;

procedure TSQLParser.FormatLoadDataStmt(const Nodes: TLoadDataStmt.TNodes);
begin
  FormatNode(Nodes.StmtTag);
  FormatNode(Nodes.PriorityTag, stSpaceBefore);
  FormatNode(Nodes.InfileTag, stSpaceBefore);
  FormatNode(Nodes.FilenameString, stSpaceBefore);
  Commands.IncreaseIndent();
  FormatNode(Nodes.ReplaceIgnoreTag, stReturnBefore);
  FormatNode(Nodes.IntoTableTag, stReturnBefore);
  FormatNode(Nodes.Ident, stSpaceBefore);
  FormatNode(Nodes.PartitionTag, stReturnBefore);
  FormatNode(Nodes.PartitionList, stSpaceBefore);
  FormatNode(Nodes.CharacterSetValue, stReturnBefore);
  if (Nodes.ColumnsTag > 0) then
  begin
    FormatNode(Nodes.ColumnsTag, stReturnBefore);
    Commands.IncreaseIndent();
    FormatNode(Nodes.ColumnsTerminatedByValue, stReturnBefore);
    FormatNode(Nodes.EnclosedByValue, stReturnBefore);
    FormatNode(Nodes.EscapedByValue, stReturnBefore);
    Commands.DecreaseIndent();
  end;
  if (Nodes.LinesTag > 0) then
  begin
    FormatNode(Nodes.LinesTag, stReturnBefore);
    Commands.IncreaseIndent();
    FormatNode(Nodes.StartingByValue, stReturnBefore);
    FormatNode(Nodes.LinesTerminatedByValue, stReturnBefore);
    Commands.DecreaseIndent();
  end;
  FormatNode(Nodes.Ignore.Tag, stReturnBefore);
  FormatNode(Nodes.Ignore.NumberToken, stSpaceBefore);
  FormatNode(Nodes.Ignore.LinesTag, stSpaceBefore);
  FormatNode(Nodes.ColumnList, stReturnBefore);
  FormatNode(Nodes.SetList, stReturnBefore);
  Commands.DecreaseIndent();
end;

procedure TSQLParser.FormatLoadXMLStmt(const Nodes: TLoadXMLStmt.TNodes);
begin
  FormatNode(Nodes.StmtTag);
  FormatNode(Nodes.PriorityTag, stSpaceBefore);
  FormatNode(Nodes.InfileTag, stSpaceBefore);
  FormatNode(Nodes.FilenameString, stSpaceBefore);
  Commands.IncreaseIndent();
  FormatNode(Nodes.ReplaceIgnoreTag, stReturnBefore);
  FormatNode(Nodes.IntoTableValue, stReturnBefore);
  FormatNode(Nodes.CharacterSetValue, stReturnBefore);
  FormatNode(Nodes.RowsIdentifiedByValue, stReturnBefore);
  FormatNode(Nodes.Ignore.Tag, stReturnBefore);
  FormatNode(Nodes.Ignore.NumberToken, stSpaceBefore);
  FormatNode(Nodes.Ignore.LinesTag, stSpaceBefore);
  FormatNode(Nodes.FieldList, stReturnBefore);
  FormatNode(Nodes.SetList, stReturnBefore);
  Commands.DecreaseIndent();
end;

procedure TSQLParser.FormatLoopStmt(const Nodes: TLoopStmt.TNodes);
begin
  FormatNode(Nodes.BeginLabel, stSpaceAfter);
  Commands.IncreaseIndent();
  FormatNode(Nodes.BeginTag);
  FormatNode(Nodes.StmtList, stReturnBefore);
  Commands.DecreaseIndent();
  FormatNode(Nodes.EndTag, stReturnBefore);
  FormatNode(Nodes.EndLabel, stSpaceBefore);
end;

procedure TSQLParser.FormatMatchFunc(const Nodes: TMatchFunc.TNodes);
begin
  FormatNode(Nodes.IdentToken);
  FormatNode(Nodes.MatchList, stSpaceBefore);
  FormatNode(Nodes.AgainstTag, stSpaceBefore);
  FormatNode(Nodes.OpenBracket, stSpaceBefore);
  FormatNode(Nodes.Expr);
  FormatNode(Nodes.SearchModifierTag, stSpaceBefore);
  FormatNode(Nodes.CloseBracket);
end;

procedure TSQLParser.FormatPositionFunc(const Nodes: TPositionFunc.TNodes);
begin
  FormatNode(Nodes.IdentToken);
  FormatNode(Nodes.OpenBracket);
  FormatNode(Nodes.SubStr);
  FormatNode(Nodes.InTag, stSpaceBefore);
  FormatNode(Nodes.Str, stSpaceBefore);
  FormatNode(Nodes.CloseBracket);
end;

procedure TSQLParser.FormatNode(const Node: PNode; const Separator: TSeparatorType = stNone);

  procedure DefaultFormatNode(const Nodes: POffsetArray; const Size: Integer);
  var
    FirstNode: Boolean;
    I: Integer;
  begin
    Assert(Size mod SizeOf(TOffset) = 0);

    FirstNode := True;
    for I := 0 to Size div SizeOf(TOffset) - 1 do
      if (Nodes^[I] > 0) then
        if (FirstNode) then
        begin
          FormatNode(Nodes^[I]);
          FirstNode := False;
        end
        else
          FormatNode(Nodes^[I], stSpaceBefore);
  end;

begin
  if (Assigned(Node)) then
  begin
    if (not Commands.NewLine) then
      case (Separator) of
        stReturnBefore: Commands.WriteReturn();
        stSpaceBefore: Commands.WriteSpace();
      end;

    CommentsWritten := False;
    case (Node^.NodeType) of
      ntToken: FormatToken(PToken(Node));
      ntUnknownStmt: FormatUnknownStmt(PUnknownStmt(Node));

      ntAnalyzeTableStmt: DefaultFormatNode(@PAnalyzeTableStmt(Node)^.Nodes, SizeOf(TAnalyzeTableStmt.TNodes));
      ntAlterDatabaseStmt: FormatAlterDatabaseStmt(PAlterDatabaseStmt(Node)^.Nodes);
      ntAlterEventStmt: FormatAlterEventStmt(PAlterEventStmt(Node)^.Nodes);
      ntAlterInstanceStmt: DefaultFormatNode(@PAlterInstanceStmt(Node)^.Nodes, SizeOf(TAlterInstanceStmt.TNodes));
      ntAlterRoutineStmt: FormatAlterRoutineStmt(PAlterRoutineStmt(Node)^.Nodes);
      ntAlterServerStmt: DefaultFormatNode(@PAlterServerStmt(Node)^.Nodes, SizeOf(TAlterServerStmt.TNodes));
      ntAlterTablespaceStmt: FormatAlterTablespaceStmt(PAlterTablespaceStmt(Node)^.Nodes);
      ntAlterTableStmt: FormatAlterTableStmt(PAlterTableStmt(Node)^.Nodes);
      ntAlterTableStmtAlterColumn: DefaultFormatNode(@TAlterTableStmt.PAlterColumn(Node)^.Nodes, SizeOf(TAlterTableStmt.TAlterColumn.TNodes));
      ntAlterTableStmtConvertTo: DefaultFormatNode(@TAlterTableStmt.PConvertTo(Node)^.Nodes, SizeOf(TAlterTableStmt.TConvertTo.TNodes));
      ntAlterTableStmtDropObject: DefaultFormatNode(@TAlterTableStmt.PDropObject(Node)^.Nodes, SizeOf(TAlterTableStmt.TDropObject.TNodes));
      ntAlterTableStmtExchangePartition: DefaultFormatNode(@TAlterTableStmt.PExchangePartition(Node)^.Nodes, SizeOf(TAlterTableStmt.TExchangePartition.TNodes));
      ntAlterTableStmtOrderBy: DefaultFormatNode(@TAlterTableStmt.POrderBy(Node)^.Nodes, SizeOf(TAlterTableStmt.TOrderBy.TNodes));
      ntAlterTableStmtReorganizePartition: DefaultFormatNode(@TAlterTableStmt.PReorganizePartition(Node)^.Nodes, SizeOf(TAlterTableStmt.TReorganizePartition.TNodes));
      ntAlterViewStmt: FormatAlterViewStmt(PAlterViewStmt(Node)^.Nodes);
      ntBeginLabel: FormatBeginLabel(PBeginLabel(Node)^.Nodes);
      ntBeginStmt: DefaultFormatNode(@PBeginStmt(Node)^.Nodes, SizeOf(TBeginStmt.TNodes));
      ntBetweenOp: DefaultFormatNode(@PBetweenOp(Node)^.Nodes, SizeOf(TBetweenOp.TNodes));
      ntBinaryOp: DefaultFormatNode(@PBinaryOp(Node)^.Nodes, SizeOf(TBinaryOp.TNodes));
      ntCallStmt: FormatCallStmt(PCallStmt(Node)^.Nodes);
      ntCaseOp: FormatCaseOp(PCaseOp(Node)^.Nodes);
      ntCaseOpBranch: DefaultFormatNode(@TCaseOp.PBranch(Node)^.Nodes, SizeOf(TCaseOp.TBranch.TNodes));
      ntCaseStmt: FormatCaseStmt(PCaseStmt(Node)^.Nodes);
      ntCaseStmtBranch: FormatCaseStmtBranch(TCaseStmt.PBranch(Node)^.Nodes);
      ntCastFunc: FormatCastFunc(PCastFunc(Node)^.Nodes);
      ntChangeMasterStmt: FormatChangeMasterStmt(PChangeMasterStmt(Node)^.Nodes);
      ntCharFunc: FormatCharFunc(PCharFunc(Node)^.Nodes);
      ntCheckTableStmt: DefaultFormatNode(@PCheckTableStmt(Node)^.Nodes, SizeOf(TCheckTableStmt.TNodes));
      ntCheckTableStmtOption: DefaultFormatNode(@TCheckTableStmt.POption(Node)^.Nodes, SizeOf(TCheckTableStmt.TOption.TNodes));
      ntChecksumTableStmt: DefaultFormatNode(@PChecksumTableStmt(Node)^.Nodes, SizeOf(TChecksumTableStmt.TNodes));
      ntCloseStmt: DefaultFormatNode(@PCloseStmt(Node)^.Nodes, SizeOf(TCloseStmt.TNodes));
      ntCommitStmt: DefaultFormatNode(@PCommitStmt(Node)^.Nodes, SizeOf(TCommitStmt.TNodes));
      ntCompoundStmt: FormatCompoundStmt(PCompoundStmt(Node)^.Nodes);
      ntConvertFunc: FormatConvertFunc(PConvertFunc(Node)^.Nodes);
      ntCreateDatabaseStmt: DefaultFormatNode(@PCreateDatabaseStmt(Node)^.Nodes, SizeOf(TCreateDatabaseStmt.TNodes));
      ntCreateEventStmt: FormatCreateEventStmt(PCreateEventStmt(Node)^.Nodes);
      ntCreateIndexStmt: FormatCreateIndexStmt(PCreateIndexStmt(Node)^.Nodes);
      ntCreateRoutineStmt: FormatCreateRoutineStmt(PCreateRoutineStmt(Node)^.Nodes);
      ntCreateServerStmt: FormatCreateServerStmt(PCreateServerStmt(Node)^.Nodes);
      ntCreateTablespaceStmt: FormatCreateTablespaceStmt(PCreateTablespaceStmt(Node)^.Nodes);
      ntCreateTableStmt: FormatCreateTableStmt(PCreateTableStmt(Node)^.Nodes);
      ntCreateTableStmtField: FormatCreateTableStmtField(TCreateTableStmt.PField(Node)^.Nodes);
      ntCreateTableStmtFieldDefaultFunc: FormatCreateTableStmtFieldDefaultFunc(TCreateTableStmt.TField.PDefaultFunc(Node)^.Nodes);
      ntCreateTableStmtForeignKey: DefaultFormatNode(@TCreateTableStmt.PForeignKey(Node)^.Nodes, SizeOf(TCreateTableStmt.TForeignKey.TNodes));
      ntCreateTableStmtKey: FormatCreateTableStmtKey(TCreateTableStmt.PKey(Node)^.Nodes);
      ntCreateTableStmtKeyColumn: FormatCreateTableStmtKeyColumn(TCreateTableStmt.PKeyColumn(Node)^.Nodes);
      ntCreateTableStmtPartition: FormatCreateTableStmtPartition(TCreateTableStmt.PPartition(Node)^.Nodes);
      ntCreateTableStmtReference: FormatCreateTableStmtReference(TCreateTableStmt.PReference(Node)^.Nodes);
      ntCreateTriggerStmt: FormatCreateTriggerStmt(PCreateTriggerStmt(Node)^.Nodes);
      ntCreateUserStmt: FormatCreateUserStmt(PCreateUserStmt(Node)^.Nodes);
      ntCreateViewStmt: FormatCreateViewStmt(PCreateViewStmt(Node)^.Nodes);
      ntCurrentTimestamp: FormatCurrentTimestamp(PCurrentTimestamp(Node)^.Nodes);
      ntDatatype: FormatDatatype(PDatatype(Node)^.Nodes);
      ntDateAddFunc: FormatDateAddFunc(PDateAddFunc(Node)^.Nodes);
      ntDbIdent: FormatDbIdent(PDbIdent(Node)^.Nodes);
      ntDeallocatePrepareStmt: DefaultFormatNode(@PDeallocatePrepareStmt(Node)^.Nodes, SizeOf(TDeallocatePrepareStmt.TNodes));
      ntDeclareStmt: DefaultFormatNode(@PDeclareStmt(Node)^.Nodes, SizeOf(TDeclareStmt.TNodes));
      ntDeclareConditionStmt: DefaultFormatNode(@PDeclareConditionStmt(Node)^.Nodes, SizeOf(TDeclareConditionStmt.TNodes));
      ntDeclareCursorStmt: FormatDeclareCursorStmt(PDeclareCursorStmt(Node)^.Nodes);
      ntDeclareHandlerStmt: FormatDeclareHandlerStmt(PDeclareHandlerStmt(Node)^.Nodes);
      ntDeleteStmt: FormatDeleteStmt(PDeleteStmt(Node)^.Nodes);
      ntDoStmt: DefaultFormatNode(@PDoStmt(Node)^.Nodes, SizeOf(TDoStmt.TNodes));
      ntDropDatabaseStmt: DefaultFormatNode(@PDropDatabaseStmt(Node)^.Nodes, SizeOf(TDropDatabaseStmt.TNodes));
      ntDropEventStmt: DefaultFormatNode(@PDropEventStmt(Node)^.Nodes, SizeOf(TDropEventStmt.TNodes));
      ntDropIndexStmt: DefaultFormatNode(@PDropIndexStmt(Node)^.Nodes, SizeOf(TDropIndexStmt.TNodes));
      ntDropRoutineStmt: DefaultFormatNode(@PDropRoutineStmt(Node)^.Nodes, SizeOf(TDropRoutineStmt.TNodes));
      ntDropServerStmt: DefaultFormatNode(@PDropServerStmt(Node)^.Nodes, SizeOf(TDropServerStmt.TNodes));
      ntDropTablespaceStmt: FormatDropTablespaceStmt(PDropTablespaceStmt(Node)^.Nodes);
      ntDropTableStmt: DefaultFormatNode(@PDropTableStmt(Node)^.Nodes, SizeOf(TDropTableStmt.TNodes));
      ntDropTriggerStmt: DefaultFormatNode(@PDropTriggerStmt(Node)^.Nodes, SizeOf(TDropTriggerStmt.TNodes));
      ntDropUserStmt: DefaultFormatNode(@PDropUserStmt(Node)^.Nodes, SizeOf(TDropUserStmt.TNodes));
      ntDropViewStmt: DefaultFormatNode(@PDropViewStmt(Node)^.Nodes, SizeOf(TDropViewStmt.TNodes));
      ntEndLabel: DefaultFormatNode(@PEndLabel(Node)^.Nodes, SizeOf(TEndLabel.TNodes));
      ntExecuteStmt: DefaultFormatNode(@PExecuteStmt(Node)^.Nodes, SizeOf(TExecuteStmt.TNodes));
      ntExistsFunc: FormatExistsFunc(PExistsFunc(Node)^.Nodes);
      ntExplainStmt: DefaultFormatNode(@PExplainStmt(Node)^.Nodes, SizeOf(TExplainStmt.TNodes));
      ntExtractFunc: FormatExtractFunc(PExtractFunc(Node)^.Nodes);
      ntFetchStmt: DefaultFormatNode(@PFetchStmt(Node)^.Nodes, SizeOf(TFetchStmt.TNodes));
      ntFlushStmt: DefaultFormatNode(@PFlushStmt(Node)^.Nodes, SizeOf(TFlushStmt.TNodes));
      ntFlushStmtOption: DefaultFormatNode(@TFlushStmt.POption(Node)^.Nodes, SizeOf(TFlushStmt.TOption.TNodes));
      ntFunctionCall: FormatFunctionCall(PFunctionCall(Node)^.Nodes);
      ntFunctionReturns: DefaultFormatNode(@PFunctionReturns(Node)^.Nodes, SizeOf(TFunctionReturns.TNodes));
      ntGetDiagnosticsStmt: DefaultFormatNode(@PGetDiagnosticsStmt(Node)^.Nodes, SizeOf(TGetDiagnosticsStmt.TNodes));
      ntGetDiagnosticsStmtStmtInfo: DefaultFormatNode(@TGetDiagnosticsStmt.PStmtInfo(Node)^.Nodes, SizeOf(TGetDiagnosticsStmt.TStmtInfo.TNodes));
      ntGetDiagnosticsStmtCondInfo: DefaultFormatNode(@TGetDiagnosticsStmt.PCondInfo(Node)^.Nodes, SizeOf(TGetDiagnosticsStmt.TCondInfo.TNodes));
      ntGrantStmt: DefaultFormatNode(@PGrantStmt(Node)^.Nodes, SizeOf(TGrantStmt.TNodes));
      ntGrantStmtPrivileg: DefaultFormatNode(@TGrantStmt.PPrivileg(Node)^.Nodes, SizeOf(TGrantStmt.TPrivileg.TNodes));
      ntGrantStmtUserSpecification: DefaultFormatNode(@TGrantStmt.PUserSpecification(Node)^.Nodes, SizeOf(TGrantStmt.TUserSpecification.TNodes));
      ntGroupConcatFunc: FormatGroupConcatFunc(PGroupConcatFunc(Node)^.Nodes);
      ntGroupConcatFuncExpr: DefaultFormatNode(@TGroupConcatFunc.PExpr(Node)^.Nodes, SizeOf(TGroupConcatFunc.TExpr.TNodes));
      ntHelpStmt: DefaultFormatNode(@PHelpStmt(Node)^.Nodes, SizeOf(THelpStmt.TNodes));
      ntIfStmt: FormatIfStmt(PIfStmt(Node)^.Nodes);
      ntIfStmtBranch: FormatIfStmtBranch(TIfStmt.PBranch(Node)^.Nodes);
      ntInOp: DefaultFormatNode(@PInOp(Node)^.Nodes, SizeOf(TInOp.TNodes));
      ntInsertStmt: FormatInsertStmt(PInsertStmt(Node)^.Nodes);
      ntInsertStmtSetItem: DefaultFormatNode(@TInsertStmt.PSetItem(Node)^.Nodes, SizeOf(TInsertStmt.TSetItem.TNodes));
      ntIntervalOp: DefaultFormatNode(@PIntervalOp(Node)^.Nodes, SizeOf(TIntervalOp.TNodes));
      ntIterateStmt: DefaultFormatNode(@PIterateStmt(Node)^.Nodes, SizeOf(TIterateStmt.TNodes));
      ntKillStmt: DefaultFormatNode(@PKillStmt(Node)^.Nodes, SizeOf(TKillStmt.TNodes));
      ntLeaveStmt: DefaultFormatNode(@PLeaveStmt(Node)^.Nodes, SizeOf(TLeaveStmt.TNodes));
      ntLikeOp: DefaultFormatNode(@PLikeOp(Node)^.Nodes, SizeOf(TLikeOp.TNodes));
      ntList: FormatList(PList(Node)^.Nodes, PList(Node)^.DelimiterType);
      ntLoadDataStmt: FormatLoadDataStmt(PLoadDataStmt(Node)^.Nodes);
      ntLoadXMLStmt: FormatLoadXMLStmt(PLoadXMLStmt(Node)^.Nodes);
      ntLockTableStmt: DefaultFormatNode(@PLockTableStmt(Node)^.Nodes, SizeOf(TLockTableStmt.TNodes));
      ntLockTableStmtItem: DefaultFormatNode(@TLockTableStmt.PItem(Node)^.Nodes, SizeOf(TLockTableStmt.TItem.TNodes));
      ntLoopStmt: FormatLoopStmt(PLoopStmt(Node)^.Nodes);
      ntMatchFunc: FormatMatchFunc(PMatchFunc(Node)^.Nodes);
      ntPositionFunc: FormatPositionFunc(PPositionFunc(Node)^.Nodes);
      ntPrepareStmt: DefaultFormatNode(@PPrepareStmt(Node)^.Nodes, SizeOf(TPrepareStmt.TNodes));
      ntPurgeStmt: DefaultFormatNode(@PPurgeStmt(Node)^.Nodes, SizeOf(TPurgeStmt.TNodes));
      ntOpenStmt: DefaultFormatNode(@POpenStmt(Node)^.Nodes, SizeOf(TOpenStmt.TNodes));
      ntOptimizeTableStmt: DefaultFormatNode(@POptimizeTableStmt(Node)^.Nodes, SizeOf(TOptimizeTableStmt.TNodes));
      ntRegExpOp: DefaultFormatNode(@PRegExpOp(Node)^.Nodes, SizeOf(TRegExpOp.TNodes));
      ntRenameStmt: DefaultFormatNode(@PRenameStmt(Node)^.Nodes, SizeOf(TRenameStmt.TNodes));
      ntRenameStmtPair: DefaultFormatNode(@TRenameStmt.PPair(Node)^.Nodes, SizeOf(TRenameStmt.TPair.TNodes));
      ntReleaseStmt: DefaultFormatNode(@PReleaseStmt(Node)^.Nodes, SizeOf(TReleaseStmt.TNodes));
      ntRepairTableStmt: DefaultFormatNode(@PRepairTableStmt(Node)^.Nodes, SizeOf(TRepairTableStmt.TNodes));
      ntRepeatStmt: FormatRepeatStmt(PRepeatStmt(Node)^.Nodes);
      ntResetStmt: DefaultFormatNode(@PResetStmt(Node)^.Nodes, SizeOf(TResetStmt.TNodes));
      ntResignalStmt: DefaultFormatNode(@PResignalStmt(Node)^.Nodes, SizeOf(TResignalStmt.TNodes));
      ntReturnStmt: DefaultFormatNode(@PReturnStmt(Node)^.Nodes, SizeOf(TReturnStmt.TNodes));
      ntRevokeStmt: DefaultFormatNode(@PRevokeStmt(Node)^.Nodes, SizeOf(TRevokeStmt.TNodes));
      ntRollbackStmt: DefaultFormatNode(@PRollbackStmt(Node)^.Nodes, SizeOf(TRollbackStmt.TNodes));
      ntRoutineParam: DefaultFormatNode(@PRoutineParam(Node)^.Nodes, SizeOf(TRoutineParam.TNodes));
      ntSavepointStmt: DefaultFormatNode(@PSavepointStmt(Node)^.Nodes, SizeOf(TSavepointStmt.TNodes));
      ntSchedule: FormatSchedule(PSchedule(Node)^.Nodes);
      ntSecretIdent: FormatSecretIdent(PSecretIdent(Node)^.Nodes);
      ntSelectStmt: FormatSelectStmt(PSelectStmt(Node)^.Nodes);
      ntSelectStmtColumn: FormatSelectStmtColumn(TSelectStmt.PColumn(Node)^.Nodes);
      ntSelectStmtGroup: DefaultFormatNode(@TSelectStmt.PGroup(Node)^.Nodes, SizeOf(TSelectStmt.TGroup.TNodes));
      ntSelectStmtOrder: DefaultFormatNode(@TSelectStmt.POrder(Node)^.Nodes, SizeOf(TSelectStmt.TOrder.TNodes));
      ntSelectStmtTableFactor: FormatSelectStmtTableFactor(TSelectStmt.PTableFactor(Node)^.Nodes);
      ntSelectStmtTableFactorIndexHint: DefaultFormatNode(@TSelectStmt.TTableFactor.PIndexHint(Node)^.Nodes, SizeOf(TSelectStmt.TTableFactor.TIndexHint.TNodes));
      ntSelectStmtTableFactorOj: FormatSelectStmtTableFactorOj(TSelectStmt.PTableFactorOj(Node)^.Nodes);
      ntSelectStmtTableFactorSelect: DefaultFormatNode(@TSelectStmt.PTableFactorSelect(Node)^.Nodes, SizeOf(TSelectStmt.TTableFactorSelect.TNodes));
      ntSelectStmtTableJoin: FormatSelectStmtTableJoin(TSelectStmt.PTableJoin(Node)^.Nodes);
      ntSetNamesStmt: DefaultFormatNode(@PSetNamesStmt(Node)^.Nodes, SizeOf(TSetNamesStmt.TNodes));
      ntSetPasswordStmt: DefaultFormatNode(@PSetPasswordStmt(Node)^.Nodes, SizeOf(TSetPasswordStmt.TNodes));
      ntSetStmt: FormatSetStmt(PSetStmt(Node)^.Nodes);
      ntSetStmtAssignment: DefaultFormatNode(@TSetStmt.PAssignment(Node)^.Nodes, SizeOf(TSetStmt.TAssignment.TNodes));
      ntSetTransactionStmt: DefaultFormatNode(@PSetTransactionStmt(Node)^.Nodes, SizeOf(TSetTransactionStmt.TNodes));
      ntSetTransactionStmtCharacteristic: DefaultFormatNode(@TSetTransactionStmt.PCharacteristic(Node)^.Nodes, SizeOf(TSetTransactionStmt.TCharacteristic.TNodes));
      ntShowAuthorsStmt: DefaultFormatNode(@PShowAuthorsStmt(Node)^.Nodes, SizeOf(TShowAuthorsStmt.TNodes));
      ntShowBinaryLogsStmt: DefaultFormatNode(@PShowBinaryLogsStmt(Node)^.Nodes, SizeOf(TShowBinaryLogsStmt.TNodes));
      ntShowBinlogEventsStmt: FormatShowBinlogEventsStmt(PShowBinlogEventsStmt(Node)^.Nodes);
      ntShowCharacterSetStmt: DefaultFormatNode(@PShowCharacterSetStmt(Node)^.Nodes, SizeOf(TShowCharacterSetStmt.TNodes));
      ntShowCollationStmt: DefaultFormatNode(@PShowCollationStmt(Node)^.Nodes, SizeOf(TShowCollationStmt.TNodes));
      ntShowColumnsStmt: DefaultFormatNode(@PShowColumnsStmt(Node)^.Nodes, SizeOf(TShowColumnsStmt.TNodes));
      ntShowContributorsStmt: DefaultFormatNode(@PShowContributorsStmt(Node)^.Nodes, SizeOf(TShowContributorsStmt.TNodes));
      ntShowCountErrorsStmt: DefaultFormatNode(@PShowCountErrorsStmt(Node)^.Nodes, SizeOf(TShowCountErrorsStmt.TNodes));
      ntShowCountWarningsStmt: DefaultFormatNode(@PShowCountWarningsStmt(Node)^.Nodes, SizeOf(TShowCountWarningsStmt.TNodes));
      ntShowCreateDatabaseStmt: DefaultFormatNode(@PShowCreateDatabaseStmt(Node)^.Nodes, SizeOf(TShowCreateDatabaseStmt.TNodes));
      ntShowCreateEventStmt: DefaultFormatNode(@PShowCreateEventStmt(Node)^.Nodes, SizeOf(TShowCreateEventStmt.TNodes));
      ntShowCreateFunctionStmt: DefaultFormatNode(@PShowCreateFunctionStmt(Node)^.Nodes, SizeOf(TShowCreateFunctionStmt.TNodes));
      ntShowCreateProcedureStmt: DefaultFormatNode(@PShowCreateProcedureStmt(Node)^.Nodes, SizeOf(TShowCreateProcedureStmt.TNodes));
      ntShowCreateTableStmt: DefaultFormatNode(@PShowCreateTableStmt(Node)^.Nodes, SizeOf(TShowCreateTableStmt.TNodes));
      ntShowCreateTriggerStmt: DefaultFormatNode(@PShowCreateTriggerStmt(Node)^.Nodes, SizeOf(TShowCreateTriggerStmt.TNodes));
      ntShowCreateUserStmt: DefaultFormatNode(@PShowCreateUserStmt(Node)^.Nodes, SizeOf(TShowCreateUserStmt.TNodes));
      ntShowCreateViewStmt: DefaultFormatNode(@PShowCreateViewStmt(Node)^.Nodes, SizeOf(TShowCreateViewStmt.TNodes));
      ntShowDatabasesStmt: DefaultFormatNode(@PShowDatabasesStmt(Node)^.Nodes, SizeOf(TShowDatabasesStmt.TNodes));
      ntShowEngineStmt: DefaultFormatNode(@PShowEngineStmt(Node)^.Nodes, SizeOf(TShowEngineStmt.TNodes));
      ntShowEnginesStmt: DefaultFormatNode(@PShowEnginesStmt(Node)^.Nodes, SizeOf(TShowEnginesStmt.TNodes));
      ntShowErrorsStmt: FormatShowErrorsStmt(PShowErrorsStmt(Node)^.Nodes);
      ntShowEventsStmt: DefaultFormatNode(@PShowEventsStmt(Node)^.Nodes, SizeOf(TShowEventsStmt.TNodes));
      ntShowFunctionCodeStmt: DefaultFormatNode(@PShowFunctionCodeStmt(Node)^.Nodes, SizeOf(TShowFunctionCodeStmt.TNodes));
      ntShowFunctionStatusStmt: DefaultFormatNode(@PShowFunctionStatusStmt(Node)^.Nodes, SizeOf(TShowFunctionStatusStmt.TNodes));
      ntShowGrantsStmt: DefaultFormatNode(@PShowGrantsStmt(Node)^.Nodes, SizeOf(TShowGrantsStmt.TNodes));
      ntShowIndexStmt: DefaultFormatNode(@PShowIndexStmt(Node)^.Nodes, SizeOf(TShowIndexStmt.TNodes));
      ntShowMasterStatusStmt: DefaultFormatNode(@PShowMasterStatusStmt(Node)^.Nodes, SizeOf(TShowMasterStatusStmt.TNodes));
      ntShowOpenTablesStmt: DefaultFormatNode(@PShowOpenTablesStmt(Node)^.Nodes, SizeOf(TShowOpenTablesStmt.TNodes));
      ntShowPluginsStmt: DefaultFormatNode(@PShowPluginsStmt(Node)^.Nodes, SizeOf(TShowPluginsStmt.TNodes));
      ntShowPrivilegesStmt: DefaultFormatNode(@PShowPrivilegesStmt(Node)^.Nodes, SizeOf(TShowPrivilegesStmt.TNodes));
      ntShowProcedureCodeStmt: DefaultFormatNode(@PShowProcedureCodeStmt(Node)^.Nodes, SizeOf(TShowProcedureCodeStmt.TNodes));
      ntShowProcedureStatusStmt: DefaultFormatNode(@PShowProcedureStatusStmt(Node)^.Nodes, SizeOf(TShowProcedureStatusStmt.TNodes));
      ntShowProcessListStmt: DefaultFormatNode(@PShowProcessListStmt(Node)^.Nodes, SizeOf(TShowProcessListStmt.TNodes));
      ntShowProfileStmt: DefaultFormatNode(@PShowProfileStmt(Node)^.Nodes, SizeOf(TShowProfileStmt.TNodes));
      ntShowProfilesStmt: DefaultFormatNode(@PShowProfilesStmt(Node)^.Nodes, SizeOf(TShowProfilesStmt.TNodes));
      ntShowRelaylogEventsStmt: FormatShowRelaylogEventsStmt(PShowBinlogEventsStmt(Node)^.Nodes);
      ntShowSlaveHostsStmt: DefaultFormatNode(@PShowSlaveHostsStmt(Node)^.Nodes, SizeOf(TShowSlaveHostsStmt.TNodes));
      ntShowSlaveStatusStmt: DefaultFormatNode(@PShowSlaveStatusStmt(Node)^.Nodes, SizeOf(TShowSlaveStatusStmt.TNodes));
      ntShowStatusStmt: DefaultFormatNode(@PShowStatusStmt(Node)^.Nodes, SizeOf(TShowStatusStmt.TNodes));
      ntShowTableStatusStmt: DefaultFormatNode(@PShowTableStatusStmt(Node)^.Nodes, SizeOf(TShowTableStatusStmt.TNodes));
      ntShowTablesStmt: DefaultFormatNode(@PShowTablesStmt(Node)^.Nodes, SizeOf(TShowTablesStmt.TNodes));
      ntShowTriggersStmt: DefaultFormatNode(@PShowTriggersStmt(Node)^.Nodes, SizeOf(TShowTriggersStmt.TNodes));
      ntShowVariablesStmt: DefaultFormatNode(@PShowVariablesStmt(Node)^.Nodes, SizeOf(TShowVariablesStmt.TNodes));
      ntShowWarningsStmt: FormatShowWarningsStmt(PShowWarningsStmt(Node)^.Nodes);
      ntShutdownStmt: DefaultFormatNode(@PShutdownStmt(Node)^.Nodes, SizeOf(TShutdownStmt.TNodes));
      ntSignalStmt: DefaultFormatNode(@PSignalStmt(Node)^.Nodes, SizeOf(TSignalStmt.TNodes));
      ntSignalStmtInformation: DefaultFormatNode(@TSignalStmt.PInformation(Node)^.Nodes, SizeOf(TSignalStmt.TInformation.TNodes));
      ntSoundsLikeOp: DefaultFormatNode(@PSoundsLikeOp(Node)^.Nodes, SizeOf(TSoundsLikeOp.TNodes));
      ntStartSlaveStmt: DefaultFormatNode(@PStartSlaveStmt(Node)^.Nodes, SizeOf(TSignalStmt.TNodes));
      ntStartTransactionStmt: DefaultFormatNode(@PStartTransactionStmt(Node)^.Nodes, SizeOf(TStartTransactionStmt.TNodes));
      ntStopSlaveStmt: DefaultFormatNode(@PStopSlaveStmt(Node)^.Nodes, SizeOf(TSignalStmt.TNodes));
      ntSubArea: FormatSubArea(PSubArea(Node)^.Nodes);
      ntSubAreaSelectStmt: FormatSubAreaSelectStmt(PSubAreaSelectStmt(Node)^.Nodes);
      ntSubPartition: FormatSubPartition(PSubPartition(Node)^.Nodes);
      ntSubquery: FormatSubquery(PSubquery(Node)^.Nodes);
      ntSubstringFunc: FormatSubstringFunc(PSubstringFunc(Node)^.Nodes);
      ntTag: DefaultFormatNode(@PTag(Node)^.Nodes, SizeOf(TTag.TNodes));
      ntTrimFunc: FormatTrimFunc(PTrimFunc(Node)^.Nodes);
      ntTruncateStmt: DefaultFormatNode(@PTruncateStmt(Node)^.Nodes, SizeOf(TTruncateStmt.TNodes));
      ntUnaryOp: FormatUnaryOp(PUnaryOp(Node)^.Nodes);
      ntUnlockTablesStmt: DefaultFormatNode(@PUnlockTablesStmt(Node)^.Nodes, SizeOf(TUnlockTablesStmt.TNodes));
      ntUpdateStmt: DefaultFormatNode(@PUpdateStmt(Node)^.Nodes, SizeOf(TUpdateStmt.TNodes));
      ntUser: FormatUser(PUser(Node)^.Nodes);
      ntUseStmt: DefaultFormatNode(@PUseStmt(Node)^.Nodes, SizeOf(TUseStmt.TNodes));
      ntValue: FormatValue(PValue(Node)^.Nodes);
      ntVariableIdent: FormatVariableIdent(PVariable(Node)^.Nodes);
      ntWeightStringFunc: FormatWeightStringFunc(PWeightStringFunc(Node)^.Nodes);
      ntWeightStringFuncLevel: DefaultFormatNode(@TWeightStringFunc.PLevel(Node)^.Nodes, SizeOf(TWeightStringFunc.TLevel.TNodes));
      ntWhileStmt: FormatWhileStmt(PWhileStmt(Node)^.Nodes);
      ntXAStmt: DefaultFormatNode(@PXAStmt(Node)^.Nodes, SizeOf(TXAStmt.TNodes));
      ntXAStmtID: FormatXID(TXAStmt.PID(Node)^.Nodes);
      else raise Exception.Create(SArgumentOutOfRange);
    end;

    if (not CommentsWritten) then
      case (Separator) of
        stReturnAfter: Commands.WriteReturn();
        stSpaceAfter: Commands.WriteSpace();
      end;
  end;
end;

procedure TSQLParser.FormatNode(const Node: TOffset; const Separator: TSeparatorType = stNone);
begin
  FormatNode(NodePtr(Node), Separator);
end;

procedure TSQLParser.FormatRepeatStmt(const Nodes: TRepeatStmt.TNodes);
begin
  FormatNode(Nodes.BeginLabel, stSpaceAfter);
  FormatNode(Nodes.RepeatTag);
  Commands.IncreaseIndent();
  FormatNode(Nodes.StmtList, stReturnBefore);
  Commands.DecreaseIndent();
  FormatNode(Nodes.UntilTag, stReturnBefore);
  FormatNode(Nodes.SearchConditionExpr, stSpaceBefore);
  FormatNode(Nodes.EndTag, stSpaceBefore);
  FormatNode(Nodes.EndLabel, stSpaceBefore);
end;

procedure TSQLParser.FormatRoot(const Node: PNode);
var
  Stmt: PStmt;
begin
  FormatComments(Root^.FirstTokenAll, True);

  Stmt := Root^.FirstStmt;
  while (Assigned(Stmt)) do
  begin
    FormatNode(PNode(Stmt));
    FormatNode(PNode(Stmt^.Delimiter), stReturnAfter);

    Stmt := Stmt^.NextStmt;
  end;
end;

procedure TSQLParser.FormatSchedule(const Nodes: TSchedule.TNodes);
begin
  if (Nodes.AtValue > 0) then
    FormatNode(Nodes.AtValue)
  else if (Nodes.EveryValue > 0) then
  begin
    FormatNode(Nodes.EveryValue);

    if (Nodes.StartsValue > 0) then
    begin
      Commands.IncreaseIndent();
      FormatNode(Nodes.StartsValue, stReturnBefore);
      Commands.DecreaseIndent();
    end;

    if (Nodes.EndsValue > 0) then
    begin
      Commands.IncreaseIndent();
      FormatNode(Nodes.EndsValue, stReturnBefore);
      Commands.DecreaseIndent();
    end;
  end
  else
    raise Exception.Create(SArgumentOutOfRange);
end;

procedure TSQLParser.FormatSecretIdent(const Nodes: TSecretIdent.TNodes);
begin
  FormatNode(Nodes.OpenAngleBracket);
  FormatNode(Nodes.ItemToken);
  FormatNode(Nodes.CloseAngleBracket);
end;

procedure TSQLParser.FormatSelectStmt(const Nodes: TSelectStmt.TNodes);
var
  ItemPerLine: Boolean;
  Separator: TSeparatorType;
  Spacer: TSpacer;

  procedure FormatInto(const Nodes: TSelectStmt.TIntoNodes);
  begin
    if (Nodes.Tag > 0) then
      if (PTag(NodePtr(Nodes.Tag))^.Nodes.KeywordToken2 = kiOUTFILE) then
      begin
        FormatNode(Nodes.Tag, Separator);
        Commands.IncreaseIndent();
        if (Separator = stReturnBefore) then
          Commands.WriteReturn();
        FormatNode(Nodes.CharacterSetValue, stSpaceBefore);
        FormatNode(Nodes.FieldsTerminatedByValue, stSpaceBefore);
        FormatNode(Nodes.EnclosedByValue, stSpaceBefore);
        FormatNode(Nodes.LinesTerminatedByValue, stSpaceBefore);
        Commands.DecreaseIndent();
      end
      else if (PTag(NodePtr(Nodes.Tag))^.Nodes.KeywordToken2 = kiDUMPFILE) then
      begin
        FormatNode(Nodes.Tag, Separator);
        Commands.IncreaseIndent();
        if (Separator = stReturnBefore) then
          Commands.WriteReturn();
        FormatNode(Nodes.Filename, stSpaceBefore);
        Commands.DecreaseIndent();
      end
      else
      begin
        FormatNode(Nodes.Tag, Separator);
        Commands.IncreaseIndent();
        if (not ItemPerLine) then
          FormatNode(Nodes.VariableList, Separator)
        else
        begin
          if (Separator = stReturnBefore) then
            Commands.WriteReturn();
          FormatList(Nodes.VariableList, Spacer);
        end;
        Commands.DecreaseIndent();
      end;
  end;

begin
  ItemPerLine := (Nodes.ColumnsList > 0)
    and (NodePtr(Nodes.ColumnsList)^.NodeType = ntList)
    and (PList(NodePtr(Nodes.ColumnsList))^.ElementCount >= 5);

  if ((Nodes.ColumnsList > 0)
    and (NodePtr(Nodes.ColumnsList)^.NodeType = ntList)
    and (PList(NodePtr(Nodes.ColumnsList))^.ElementCount = 1)
    or (Nodes.From.Tag = 0)) then
  begin
    Separator := stSpaceBefore;
    Spacer := sSpace;
  end
  else
  begin
    Separator := stReturnBefore;
    Spacer := sReturn;
  end;

  FormatNode(Nodes.SelectTag);
  Commands.IncreaseIndent();
  FormatNode(Nodes.DistinctTag, stSpaceBefore);
  FormatNode(Nodes.HighPriorityTag, stSpaceBefore);
  FormatNode(Nodes.MaxStatementTime, stSpaceBefore);
  FormatNode(Nodes.StraightJoinTag, stSpaceBefore);
  FormatNode(Nodes.SQLSmallResultTag, stSpaceBefore);
  FormatNode(Nodes.SQLBigResultTag, stSpaceBefore);
  FormatNode(Nodes.SQLBufferResultTag, stSpaceBefore);
  FormatNode(Nodes.SQLNoCacheTag, stSpaceBefore);
  FormatNode(Nodes.SQLCalcFoundRowsTag, stSpaceBefore);
  if (not ItemPerLine) then
    FormatNode(Nodes.ColumnsList, Separator)
  else
  begin
    if (Separator = stSpaceBefore) then
      Commands.WriteSpace()
    else
      Commands.WriteReturn();
    FormatList(Nodes.ColumnsList, Spacer);
  end;
  Commands.DecreaseIndent();
  FormatInto(Nodes.Into1);
  if (Nodes.From.Tag > 0) then
  begin
    FormatNode(Nodes.From.Tag, Separator);
    Commands.IncreaseIndent();
    if (Separator = stReturnBefore) then
      Commands.WriteReturn()
    else
      Commands.WriteSpace();
    FormatList(Nodes.From.TableReferenceList, Spacer);
    Commands.DecreaseIndent();
  end;
  if (Nodes.Partition.Tag > 0) then
  begin
    FormatNode(Nodes.Partition.Tag, Separator);
    Commands.IncreaseIndent();
    FormatNode(Nodes.Partition.Tag, Separator);
    Commands.DecreaseIndent();
  end;
  if (Nodes.Where.Tag > 0) then
  begin
    FormatNode(Nodes.Where.Tag, Separator);
    Commands.IncreaseIndent();
    FormatNode(Nodes.Where.Expr, Separator);
    Commands.DecreaseIndent();
  end;
  if (Nodes.GroupBy.Tag > 0) then
  begin
    FormatNode(Nodes.GroupBy.Tag, Separator);
    Commands.IncreaseIndent();
    FormatNode(Nodes.GroupBy.List, Separator);
    Commands.DecreaseIndent();
    FormatNode(Nodes.GroupBy.WithRollupTag, stSpaceBefore);
  end;
  if (Nodes.Having.Tag > 0) then
  begin
    FormatNode(Nodes.Having.Tag, Separator);
    Commands.IncreaseIndent();
    FormatNode(Nodes.Having.Expr, Separator);
    Commands.DecreaseIndent();
  end;
  if (Nodes.OrderBy.Tag > 0) then
  begin
    FormatNode(Nodes.OrderBy.Tag, Separator);
    Commands.IncreaseIndent();
    FormatNode(Nodes.OrderBy.Expr, Separator);
    Commands.DecreaseIndent();
  end;
  if (Nodes.Limit.Tag > 0) then
  begin
    FormatNode(Nodes.Limit.Tag, Separator);
    Commands.IncreaseIndent();
    if (Separator = stSpaceBefore) then
      Commands.WriteSpace()
    else
      Commands.WriteReturn();
    if (Nodes.Limit.CommaToken > 0) then
    begin
      FormatNode(Nodes.Limit.OffsetToken);
      FormatNode(Nodes.Limit.CommaToken);
    end;
    FormatNode(Nodes.Limit.RowCountToken);
    Commands.DecreaseIndent();
  end;
  if (Nodes.Proc.Tag > 0) then
  begin
    FormatNode(Nodes.Proc.Tag, Separator);
    Commands.IncreaseIndent();
    FormatNode(Nodes.Proc.Ident, Separator);
    FormatNode(Nodes.Proc.ParamList, stSpaceBefore);
    Commands.DecreaseIndent();
  end;
  FormatInto(Nodes.Into2);
  FormatNode(Nodes.ForUpdatesTag, Separator);
  FormatNode(Nodes.LockInShareMode, Separator);
  FormatNode(Nodes.Union.Tag, stReturnBefore);
  FormatNode(Nodes.Union.SelectStmt, stReturnBefore);
end;

procedure TSQLParser.FormatSelectStmtColumn(const Nodes: TSelectStmt.TColumn.TNodes);
var
  Expr: TOffset;
begin
  Expr := Nodes.Expr;
  if ((Expr > 0) and (NodePtr(Expr)^.NodeType = ntDbIdent)) then
    Expr := PDbIdent(NodePtr(Expr))^.Nodes.Ident;

  FormatNode(Nodes.Expr);
  if ((Nodes.AliasIdent > 0)
    and (not IsToken(Expr) or not IsToken(Nodes.AliasIdent) or (TokenPtr(Expr)^.AsString <> TokenPtr(Nodes.AliasIdent)^.AsString))) then
  begin
    if (Nodes.AsTag > 0) then
      FormatNode(Nodes.AsTag, stSpaceBefore)
    else
      Commands.Write(' AS');
    FormatNode(Nodes.AliasIdent, stSpaceBefore);
  end;
end;

procedure TSQLParser.FormatSelectStmtTableFactor(const Nodes: TSelectStmt.TTableFactor.TNodes);
begin
  FormatNode(Nodes.TableIdent);
  FormatNode(Nodes.PartitionTag, stSpaceBefore);
  FormatNode(Nodes.Partitions, stSpaceBefore);
  if (Nodes.AliasIdent > 0) then
  begin
    if (Nodes.AsTag > 0) then
      FormatNode(Nodes.AsTag, stSpaceBefore)
    else
      Commands.Write(' AS');
    FormatNode(Nodes.AliasIdent, stSpaceBefore);
  end;
  FormatNode(Nodes.IndexHintList, stSpaceBefore);
  FormatNode(Nodes.SelectStmt, stSpaceBefore);
end;

procedure TSQLParser.FormatSelectStmtTableFactorOj(const Nodes: TSelectStmt.TTableFactorOj.TNodes);
begin
  FormatNode(Nodes.OpenBracket);
  FormatNode(Nodes.OjTag);
  FormatNode(Nodes.TableReference, stSpaceBefore);
  FormatNode(Nodes.CloseBracket);
end;

procedure TSQLParser.FormatSelectStmtTableJoin(const Nodes: TSelectStmt.TTableJoin.TNodes);
begin
  Commands.IncreaseIndent();
  FormatNode(Nodes.JoinTag, stReturnBefore);
  FormatNode(Nodes.RightTable, stSpaceBefore);
  FormatNode(Nodes.OnTag, stSpaceBefore);
  FormatNode(Nodes.Condition, stSpaceBefore);
  Commands.DecreaseIndent();
end;

procedure TSQLParser.FormatSetStmt(const Nodes: TSetStmt.TNodes);
var
  ItemPerLine: Boolean;
begin
  ItemPerLine := (Nodes.AssignmentList > 0)
    and (NodePtr(Nodes.AssignmentList)^.NodeType = ntList)
    and (PList(NodePtr(Nodes.AssignmentList))^.ElementCount > 1);

  FormatNode(Nodes.SetTag);
  FormatNode(Nodes.ScopeTag, stSpaceBefore);

  if (not ItemPerLine) then
  begin
    FormatNode(Nodes.AssignmentList, stSpaceBefore);
  end
  else
  begin
    Commands.IncreaseIndent();
    Commands.WriteReturn();
    FormatList(Nodes.AssignmentList, sReturn);
    Commands.DecreaseIndent();
  end;
end;

procedure TSQLParser.FormatShowBinlogEventsStmt(const Nodes: TShowBinlogEventsStmt.TNodes);
begin
  FormatNode(Nodes.StmtTag);
  FormatNode(Nodes.InValue, stSpaceBefore);
  FormatNode(Nodes.FromValue, stSpaceBefore);
  FormatNode(Nodes.Limit.Tag, stSpaceBefore);
  Commands.WriteSpace();
  FormatNode(Nodes.Limit.OffsetToken);
  FormatNode(Nodes.Limit.CommaToken);
  FormatNode(Nodes.Limit.RowCountToken);
end;

procedure TSQLParser.FormatShowErrorsStmt(const Nodes: TShowErrorsStmt.TNodes);
begin
  FormatNode(Nodes.StmtTag);
  FormatNode(Nodes.Limit.Tag, stSpaceBefore);
  Commands.WriteSpace();
  FormatNode(Nodes.Limit.OffsetToken);
  FormatNode(Nodes.Limit.CommaToken);
  FormatNode(Nodes.Limit.RowCountToken);
end;

procedure TSQLParser.FormatShowRelaylogEventsStmt(const Nodes: TShowBinlogEventsStmt.TNodes);
begin
  FormatNode(Nodes.StmtTag);
  FormatNode(Nodes.InValue, stSpaceBefore);
  FormatNode(Nodes.FromValue, stSpaceBefore);
  FormatNode(Nodes.Limit.Tag, stSpaceBefore);
  Commands.WriteSpace();
  FormatNode(Nodes.Limit.OffsetToken);
  FormatNode(Nodes.Limit.CommaToken);
  FormatNode(Nodes.Limit.RowCountToken);
end;

procedure TSQLParser.FormatShowWarningsStmt(const Nodes: TShowWarningsStmt.TNodes);
begin
  FormatNode(Nodes.StmtTag);
  FormatNode(Nodes.Limit.Tag, stSpaceBefore);
  Commands.WriteSpace();
  FormatNode(Nodes.Limit.OffsetToken);
  FormatNode(Nodes.Limit.CommaToken);
  FormatNode(Nodes.Limit.RowCountToken);
end;

procedure TSQLParser.FormatSQL(out SQL: string);
begin
  SQL := '';

  if (Assigned(Root)) then
  begin
    Commands := TFormatBuffer.Create();

    FormatRoot(PNode(Root));

    SQL := Commands.Read();
    Commands.Free(); Commands := nil;
  end;
end;

procedure TSQLParser.FormatSubArea(const Nodes: TSubArea.TNodes);
begin
  FormatNode(Nodes.OpenBracket);
  FormatNode(Nodes.AreaNode);
  FormatNode(Nodes.CloseBracket);
end;

procedure TSQLParser.FormatSubAreaSelectStmt(const Nodes: TSubAreaSelectStmt.TNodes);
begin
  FormatNode(Nodes.SelectStmt1);
  FormatNode(Nodes.UnionTag, stReturnBefore);
  FormatNode(Nodes.SelectStmt2, stReturnBefore);
end;

procedure TSQLParser.FormatSubPartition(const Nodes: TSubPartition.TNodes);
begin
  FormatNode(Nodes.SubPartitionTag);
  FormatNode(Nodes.NameIdent, stSpaceBefore);
  Commands.IncreaseIndent();
  FormatNode(Nodes.EngineValue, stReturnBefore);
  FormatNode(Nodes.CommentValue, stReturnBefore);
  FormatNode(Nodes.DataDirectoryValue, stReturnBefore);
  FormatNode(Nodes.IndexDirectoryValue, stReturnBefore);
  FormatNode(Nodes.MaxRowsValue, stReturnBefore);
  FormatNode(Nodes.MinRowsValue, stReturnBefore);
  FormatNode(Nodes.TablespaceValue, stReturnBefore);
  Commands.DecreaseIndent();
end;

procedure TSQLParser.FormatSubquery(const Nodes: TSubquery.TNodes);
begin
  FormatNode(Nodes.IdentTag);
  FormatNode(Nodes.OpenToken, stSpaceBefore);
  FormatNode(Nodes.Subquery);
  FormatNode(Nodes.CloseToken);
end;

procedure TSQLParser.FormatSubstringFunc(const Nodes: TSubstringFunc.TNodes);
begin
  FormatNode(Nodes.IdentToken);
  FormatNode(Nodes.OpenBracket);
  FormatNode(Nodes.Str);
  if (not IsToken(Nodes.FromTag) or (TokenPtr(Nodes.FromTag)^.TokenType <> ttComma)) then
    FormatNode(Nodes.FromTag, stSpaceBefore)
  else
    FormatNode(Nodes.FromTag);
  FormatNode(Nodes.Pos);
  if (not IsToken(Nodes.ForTag) or (TokenPtr(Nodes.FromTag)^.TokenType <> ttComma)) then
    FormatNode(Nodes.ForTag, stSpaceBefore)
  else
    FormatNode(Nodes.ForTag);
  FormatNode(Nodes.Len, stSpaceBefore);
  FormatNode(Nodes.CloseBracket);
end;

procedure TSQLParser.FormatToken(const Token: PToken);
label
  StringL, StringLE;
var
  Dest: PChar;
  Keyword: array [0 .. 30] of Char;
  Length: Integer;
  Text: PChar;
begin
  if ((Token^.UsageType = utKeyword)
    or (Token^.UsageType = utFunction)
    or (Token^.OperatorType <> otUnknown)
    or (Token^.KeywordIndex = kiNULL)) then
  begin
    Token^.GetText(Text, Length);

    if (Length > 0) then
    begin
      Dest := @Keyword[0];

      asm // Convert SQL to upper case
          PUSH ES
          PUSH ESI
          PUSH EDI

          PUSH DS                          // string operations uses ES
          POP ES
          CLD                              // string operations uses forward direction

          MOV ESI,Text
          MOV ECX,Length
          MOV EDI,Dest

        StringL:
          LODSW                            // Get character to AX
          CMP AX,'a'                       // Small character?
          JB StringLE                      // No!
          CMP AX,'z'                       // Small character?
          JA StringLE                      // No!
          AND AX,$FF - $20                 // Upcase character
        StringLE:
          STOSW                            // Put character from AX
          LOOP StringL                     // Further characters!

          POP EDI
          POP ESI
          POP ES
      end;

      Commands.Write(@Keyword[0], Token^.Length);
    end;
  end
  else if (not (Token^.NewTokenType in [ttUnknown, Token^.FTokenType])) then
  begin
    case (Token^.NewTokenType) of
      ttIdent: Commands.Write(SQLUnescape(Token^.AsString));
      ttMySQLIdent: Commands.Write(SQLEscape(SQLUnescape(Token^.AsString), '`'));
      ttDQIdent: Commands.Write(SQLEscape(SQLUnescape(Token^.AsString), '"'));
      ttString: Commands.Write(SQLEscape(SQLUnescape(Token^.AsString), ''''));
      else Commands.Write(Token^.AsString);
    end;
  end
  else if ((Token^.TokenType = ttDQIdent) and not AnsiQuotes) then
    Commands.Write(SQLEscape(SQLUnescape(Token^.AsString), ''''))
  else if ((Token^.TokenType = ttMySQLIdent) and AnsiQuotes) then
    Commands.Write(SQLEscape(SQLUnescape(Token^.AsString), '"'))
  else
  begin
    Token^.GetText(Text, Length);
    Commands.Write(Text, Length);
  end;

  FormatComments(Token^.NextTokenAll, False);
end;

procedure TSQLParser.FormatTrimFunc(const Nodes: TTrimFunc.TNodes);
begin
  FormatNode(Nodes.IdentToken);
  FormatNode(Nodes.OpenBracket);
  FormatNode(Nodes.DirectionTag, stSpaceAfter);
  FormatNode(Nodes.RemoveStr, stSpaceAfter);
  FormatNode(Nodes.FromTag, stSpaceAfter);
  FormatNode(Nodes.Str);
  FormatNode(Nodes.CloseBracket);
end;

procedure TSQLParser.FormatUnaryOp(const Nodes: TUnaryOp.TNodes);
begin
  FormatNode(Nodes.Operator);
  if (IsToken(Nodes.Operator) and (TokenPtr(Nodes.Operator)^.KeywordIndex >= 0)) then
    Commands.WriteSpace();
  FormatNode(Nodes.Operand);
end;

procedure TSQLParser.FormatUnknownStmt(const Node: PUnknownStmt);
var
  Token: PToken;
begin
  Assert(IsRange(PNode(Node)));

  Token := PRange(Node)^.FirstToken;
  while (Assigned(Token)) do
  begin
    FormatToken(Token);

    if (Token = PRange(Node)^.LastToken) then
      Token := nil
    else
      Token := Token^.NextTokenAll;
  end;
end;

procedure TSQLParser.FormatUser(const Nodes: TUser.TNodes);
begin
  FormatNode(Nodes.NameToken);
  FormatNode(Nodes.AtToken);
  FormatNode(Nodes.HostToken);
end;

procedure TSQLParser.FormatValue(const Nodes: TValue.TNodes);
begin
  FormatNode(Nodes.IdentTag);
  if (Nodes.AssignToken = 0) then
    Commands.WriteSpace()
  else
  begin
    FormatNode(Nodes.AssignToken, stSpaceBefore);
    Commands.WriteSpace();
  end;
  FormatNode(Nodes.Expr);
end;

procedure TSQLParser.FormatVariableIdent(const Nodes: TVariable.TNodes);
begin
  FormatNode(Nodes.At1Token);
  FormatNode(Nodes.At2Token);
  FormatNode(Nodes.ScopeTag);
  FormatNode(Nodes.ScopeDotToken);
  FormatNode(Nodes.Ident);
end;

procedure TSQLParser.FormatWeightStringFunc(const Nodes: TWeightStringFunc.TNodes);
begin
  FormatNode(Nodes.IdentToken);
  FormatNode(Nodes.OpenBracket);
  FormatNode(Nodes.Str);
  FormatNode(Nodes.AsTag, stSpaceBefore);
  FormatNode(Nodes.Datatype, stSpaceBefore);
  FormatNode(Nodes.LevelTag, stSpaceBefore);
  FormatNode(Nodes.LevelList, stSpaceBefore);
  FormatNode(Nodes.CloseBracket);
end;

procedure TSQLParser.FormatWhileStmt(const Nodes: TWhileStmt.TNodes);
begin
  FormatNode(Nodes.BeginLabel, stSpaceAfter);
  FormatNode(Nodes.WhileTag);
  FormatNode(Nodes.SearchConditionExpr, stSpaceBefore);
  Commands.IncreaseIndent();
  FormatNode(Nodes.DoTag, stSpaceBefore);
  FormatNode(Nodes.StmtList, stReturnBefore);
  Commands.DecreaseIndent();
  FormatNode(Nodes.EndTag, stReturnBefore);
  FormatNode(Nodes.EndLabel, stSpaceBefore);
end;

procedure TSQLParser.FormatXID(const Nodes: TXAStmt.TID.TNodes);
begin
  FormatNode(Nodes.GTrId);
  FormatNode(Nodes.Comma1, stSpaceAfter);
  FormatNode(Nodes.BQual);
  FormatNode(Nodes.Comma2, stSpaceAfter);
  FormatNode(Nodes.FormatId);
end;

function TSQLParser.GetDatatypes(): string;
begin
  Result := DatatypeList.Text;
end;

function TSQLParser.GetError(): Boolean;
begin
  Result := FErrorCode <> PE_Success;
end;

function TSQLParser.GetFunctions(): string;
begin
  Result := FunctionList.Text;
end;

function TSQLParser.GetInPL_SQL(): Boolean;
begin
  Result := FInPL_SQL > 0;
end;

function TSQLParser.GetKeywords(): string;
begin
  Result := KeywordList.Text;
end;

function TSQLParser.GetNextToken(Index: Integer): TOffset;
begin
  Assert(Index >= 0);

  Result := GetParsedToken(Index);
end;

function TSQLParser.GetParsedToken(const Index: Integer): TOffset;
var
  Length: Integer;
  S: string;
  Text: PChar;
  Token: TOffset;
  Version: Integer;
begin
  if (Index > TokenBuffer.Count - 1) then
    repeat
      Token := ParseToken();

      if (Token > 0) then
      begin
        if (TokenPtr(Token)^.TokenType = ttMySQLCodeStart) then
          if (AllowedMySQLVersion > 0) then
            SetError(PE_NestedCondCode)
          else
          begin
            TokenPtr(Token)^.GetText(Text, Length);
            SetString(S, PChar(@Text[3]), Length - 3);
            if (not TryStrToInt(S, Version)) then
              TokenPtr(Token)^.FErrorCode := PE_UnexpectedChar
            else
              AllowedMySQLVersion := Version;
          end
        else if (TokenPtr(Token)^.TokenType = ttMySQLCodeEnd) then
          AllowedMySQLVersion := 0;

        if (TokenPtr(Token)^.IsUsed) then
        begin
          {$IFDEF Debug}
          TokenPtr(Token)^.FIndex := TokenIndex; Inc(TokenIndex);
          {$ENDIF}

          if (TokenBuffer.Count = System.Length(TokenBuffer.Tokens)) then
            raise Exception.Create(SUnknownError);
          TokenBuffer.Tokens[TokenBuffer.Count] := Token;
          Inc(TokenBuffer.Count);
        end;

        LastTokenAll := Token;
      end;
    until ((Token = 0) or (TokenBuffer.Count - 1 = Index));

  if (Index >= TokenBuffer.Count) then
    Result := 0
  else
    Result := TokenBuffer.Tokens[Index];
end;

function TSQLParser.GetRoot(): PRoot;
begin
  if (FRoot = 0) then
    Result := nil
  else
    Result := PRoot(NodePtr(FRoot));
end;

procedure TSQLParser.GetText(const Offset: TOffset; out Text: PChar; out Length: Integer);
begin
  Assert((0 < Offset) and (Offset < Texts.UsedSize));

  Text := @Texts.Mem[Offset + SizeOf(Integer)];
  Length := Integer(Texts.Mem[Offset]);
end;

function TSQLParser.IsChild(const ANode: PNode): Boolean;
begin
  Result := Assigned(ANode) and (ANode^.NodeType <> ntRoot);
end;

function TSQLParser.IsChild(const ANode: TOffset): Boolean;
begin
  Result := IsChild(NodePtr(ANode));
end;

function TSQLParser.IsRange(const ANode: PNode): Boolean;
begin
  Result := Assigned(ANode) and not (ANode^.NodeType in [ntRoot, ntToken]);
end;

function TSQLParser.IsRange(const ANode: TOffset): Boolean;
begin
  Result := IsRange(NodePtr(ANode));
end;

function TSQLParser.IsRoot(const ANode: PNode): Boolean;
begin
  Result := Assigned(ANode) and (ANode^.NodeType = ntRoot);
end;

function TSQLParser.IsStmt(const ANode: PNode): Boolean;
begin
  Result := Assigned(ANode) and (ANode^.NodeType in StmtNodeTypes);
end;

function TSQLParser.IsStmt(const ANode: TOffset): Boolean;
begin
  Result := IsStmt(NodePtr(ANode));
end;

function TSQLParser.IsSymbol(const TokenType: TTokenType): Boolean;
begin
  Result := (CurrentToken > 0) and (TokenPtr(CurrentToken)^.TokenType = TokenType);
end;

function TSQLParser.IsTag(const KeywordIndex1: TWordList.TIndex;
  const KeywordIndex2: TWordList.TIndex = -1; const KeywordIndex3: TWordList.TIndex = -1;
  const KeywordIndex4: TWordList.TIndex = -1; const KeywordIndex5: TWordList.TIndex = -1;
  const KeywordIndex6: TWordList.TIndex = -1; const KeywordIndex7: TWordList.TIndex = -1): Boolean;
begin
  Result := False;
  if (EndOfStmt(CurrentToken) or (TokenPtr(CurrentToken)^.KeywordIndex <> KeywordIndex1)) then
    CompletionList.AddKeyword(KeywordIndex1, KeywordIndex2, KeywordIndex3, KeywordIndex4, KeywordIndex5, KeywordIndex6, KeywordIndex7)
  else if (KeywordIndex2 < 0) then
    Result := True
  else if (EndOfStmt(NextToken[1]) or (TokenPtr(NextToken[1])^.KeywordIndex <> KeywordIndex2)) then
    CompletionList.AddKeyword(KeywordIndex2, KeywordIndex3, KeywordIndex4, KeywordIndex5, KeywordIndex6, KeywordIndex7)
  else if (KeywordIndex3 < 0) then
    Result := True
  else if (EndOfStmt(NextToken[2]) or (TokenPtr(NextToken[2])^.KeywordIndex <> KeywordIndex3)) then
    CompletionList.AddKeyword(KeywordIndex3, KeywordIndex4, KeywordIndex5, KeywordIndex6, KeywordIndex7)
  else if (KeywordIndex4 < 0) then
    Result := True
  else if (EndOfStmt(NextToken[3]) or (TokenPtr(NextToken[3])^.KeywordIndex <> KeywordIndex4)) then
    CompletionList.AddKeyword(KeywordIndex4, KeywordIndex5, KeywordIndex6, KeywordIndex7)
  else if (KeywordIndex5 < 0) then
    Result := True
  else if (EndOfStmt(NextToken[4]) or (TokenPtr(NextToken[4])^.KeywordIndex <> KeywordIndex5)) then
    CompletionList.AddKeyword(KeywordIndex5, KeywordIndex6, KeywordIndex7)
  else if (KeywordIndex6 < 0) then
    Result := True
  else if (EndOfStmt(NextToken[5]) or (TokenPtr(NextToken[5])^.KeywordIndex <> KeywordIndex6)) then
    CompletionList.AddKeyword(KeywordIndex6, KeywordIndex7)
  else if (KeywordIndex7 < 0) then
    Result := True
  else if (EndOfStmt(NextToken[6]) or (TokenPtr(NextToken[6])^.KeywordIndex <> KeywordIndex7)) then
    CompletionList.AddKeyword(KeywordIndex7)
  else
    Result := True;
end;

function TSQLParser.IsToken(const ANode: PNode): Boolean;
begin
  Result := Assigned(ANode) and (ANode^.NodeType = ntToken);
end;

function TSQLParser.IsToken(const ANode: TOffset): Boolean;
begin
  Result := IsToken(NodePtr(ANode));
end;

function TSQLParser.LoadFromFile(const Filename: string): Boolean;
var
  BytesPerSector: DWord;
  BytesRead: DWord;
  FileSize: DWord;
  Handle: THandle;
  Len: Integer;
  MemSize: DWord;
  NumberofFreeClusters: DWord;
  SectorsPerCluser: DWord;
  Mem: PAnsiChar;
  TotalNumberOfClusters: DWord;
begin
  FRoot := 0;

  Clear();

  if (not GetDiskFreeSpace(PChar(ExtractFileDrive(Filename)), SectorsPerCluser, BytesPerSector, NumberofFreeClusters, TotalNumberOfClusters)) then
    RaiseLastOSError();

  Handle := CreateFile(PChar(Filename),
                       GENERIC_READ,
                       FILE_SHARE_READ,
                       nil,
                       OPEN_EXISTING, FILE_ATTRIBUTE_NORMAL, 0);

  if (Handle = INVALID_HANDLE_VALUE) then
    RaiseLastOSError()
  else
  begin
    FileSize := GetFileSize(Handle, nil);
    if (FileSize = INVALID_FILE_SIZE) then
    begin
      CloseHandle(Handle);
      RaiseLastOSError();
    end
    else
    begin
      MemSize := ((FileSize div BytesPerSector) + 1) * BytesPerSector;

      GetMem(Mem, MemSize);
      if (not Assigned(Mem)) then
      begin
        CloseHandle(Handle);
        raise Exception.CreateFmt(SOutOfMemory, [MemSize]);
      end
      else
      begin
        Len := 0;
        if (not ReadFile(Handle, Mem^, MemSize, BytesRead, nil)) then
          RaiseLastOSError()
        else if (BytesRead <> FileSize) then
          raise Exception.Create(SUnknownError)
        else if ((BytesRead >= DWord(Length(BOM_UTF8))) and (CompareMem(Mem, BOM_UTF8, StrLen(BOM_UTF8)))) then
        begin
          Len := MultiByteToWideChar(CP_UTF8, MB_ERR_INVALID_CHARS, @Mem[Length(BOM_UTF8)], BytesRead - DWord(Length(BOM_UTF8)), nil, 0);
          SetLength(ParsedText, Len);
          MultiByteToWideChar(CP_UTF8, MB_ERR_INVALID_CHARS, @Mem[Length(BOM_UTF8)], BytesRead - DWord(Length(BOM_UTF8)), @ParsedText[1], Len);
        end
        else if ((BytesRead >= DWord(Length(BOM_UNICODE_LE))) and (CompareMem(Mem, BOM_UNICODE_LE, StrLen(BOM_UNICODE_LE)))) then
        begin
          Len := (BytesRead - DWord(Length(BOM_UNICODE_LE))) div SizeOf(WideChar);
          SetLength(ParsedText, Len);
          MoveMemory(@ParsedText[1], @Mem[Length(BOM_UNICODE_LE)], Len * SizeOf(WideChar));
        end
        else
        begin
          Len := MultiByteToWideChar(CP_ACP, MB_ERR_INVALID_CHARS, Mem, BytesRead, nil, 0);
          SetLength(ParsedText, Len);
          MultiByteToWideChar(CP_ACP, MB_ERR_INVALID_CHARS, Mem, BytesRead, @ParsedText[1], Len);
        end;

        FreeMem(Mem);
        CloseHandle(Handle);

        Parse.Pos := PChar(ParsedText);
        Parse.Length := Len;
        FRoot := ParseRoot();
      end;
    end;
  end;

  Result := Root^.ErrorCode = PE_Success;
end;

function TSQLParser.NewNode(const ANodeType: TNodeType): TOffset;
var
  Size: Integer;
begin
  Size := NodeSize(ANodeType);

  if (Nodes.UsedSize + Size > Nodes.MemSize) then
  begin
    Inc(Nodes.MemSize, Nodes.MemSize);
    ReallocMem(Nodes.Mem, Nodes.MemSize);
  end;

  Result := Nodes.UsedSize;
  Inc(Nodes.UsedSize, Size);
end;

function TSQLParser.NewText(const Text: PChar; const Length: Integer): TOffset;
var
  Size: Integer;
begin
  Size := SizeOf(Integer) + Length * SizeOf(Text[0]);
  if (Texts.UsedSize + Size >= Texts.MemSize) then
  begin
    Texts.MemSize := Max(2 * Texts.MemSize, Texts.UsedSize + Size);
    ReallocMem(Texts.Mem, Texts.MemSize);
  end;

  Move(Length, Texts.Mem[Texts.UsedSize], SizeOf(Integer));
  Move(Text[0], Texts.Mem[Texts.UsedSize + SizeOf(Integer)], Length * SizeOf(Text[0]));

  Result := Texts.UsedSize;
  Inc(Texts.UsedSize, Size);
end;

function TSQLParser.NodePtr(const ANode: TOffset): PNode;
begin
  Assert(ANode < Nodes.UsedSize);

  if (ANode = 0) then
    Result := nil
  else
    Result := @Nodes.Mem[ANode];
end;

function TSQLParser.NodeSize(const NodeType: TNodeType): Integer;
begin
  case (NodeType) of
    ntRoot: Result := SizeOf(TRoot);
    ntToken: Result := SizeOf(TToken);

    ntAnalyzeTableStmt: Result := SizeOf(TAnalyzeTableStmt);
    ntAlterDatabaseStmt: Result := SizeOf(TAlterDatabaseStmt);
    ntAlterEventStmt: Result := SizeOf(TAlterEventStmt);
    ntAlterInstanceStmt: Result := SizeOf(TAlterInstanceStmt);
    ntAlterRoutineStmt: Result := SizeOf(TAlterRoutineStmt);
    ntAlterServerStmt: Result := SizeOf(TAlterServerStmt);
    ntAlterTablespaceStmt: Result := SizeOf(TAlterTablespaceStmt);
    ntAlterTableStmt: Result := SizeOf(TAlterTableStmt);
    ntAlterTableStmtAlterColumn: Result := SizeOf(TAlterTableStmt.TAlterColumn);
    ntAlterTableStmtConvertTo: Result := SizeOf(TAlterTableStmt.TConvertTo);
    ntAlterTableStmtDropObject: Result := SizeOf(TAlterTableStmt.TDropObject);
    ntAlterTableStmtExchangePartition: Result := SizeOf(TAlterTableStmt.TExchangePartition);
    ntAlterTableStmtOrderBy: Result := SizeOf(TAlterTableStmt.TOrderBy);
    ntAlterTableStmtReorganizePartition: Result := SizeOf(TAlterTableStmt.TReorganizePartition);
    ntAlterViewStmt: Result := SizeOf(TAlterViewStmt);
    ntBeginLabel: Result := SizeOf(TBeginLabel);
    ntBeginStmt: Result := SizeOf(TBeginStmt);
    ntBetweenOp: Result := SizeOf(TBetweenOp);
    ntBinaryOp: Result := SizeOf(TBinaryOp);
    ntCallStmt: Result := SizeOf(TCallStmt);
    ntCaseOp: Result := SizeOf(TCaseOp);
    ntCaseOpBranch: Result := SizeOf(TCaseOp.TBranch);
    ntCaseStmt: Result := SizeOf(TCaseStmt);
    ntCaseStmtBranch: Result := SizeOf(TCaseStmt.TBranch);
    ntCastFunc: Result := SizeOf(TCastFunc);
    ntChangeMasterStmt: Result := SizeOf(TChangeMasterStmt);
    ntCharFunc: Result := SizeOf(TCharFunc);
    ntCheckTableStmt: Result := SizeOf(TCheckTableStmt);
    ntCheckTableStmtOption: Result := SizeOf(TCheckTableStmt.TOption);
    ntChecksumTableStmt: Result := SizeOf(TChecksumTableStmt);
    ntCloseStmt: Result := SizeOf(TCloseStmt);
    ntCommitStmt: Result := SizeOf(TCommitStmt);
    ntCompoundStmt: Result := SizeOf(TCompoundStmt);
    ntConvertFunc: Result := SizeOf(TConvertFunc);
    ntCreateDatabaseStmt: Result := SizeOf(TCreateDatabaseStmt);
    ntCreateEventStmt: Result := SizeOf(TCreateEventStmt);
    ntCreateIndexStmt: Result := SizeOf(TCreateIndexStmt);
    ntCreateRoutineStmt: Result := SizeOf(TCreateRoutineStmt);
    ntCreateServerStmt: Result := SizeOf(TCreateServerStmt);
    ntCreateTablespaceStmt: Result := SizeOf(TCreateTablespaceStmt);
    ntCreateTableStmt: Result := SizeOf(TCreateTableStmt);
    ntCreateTableStmtField: Result := SizeOf(TCreateTableStmt.TField);
    ntCreateTableStmtFieldDefaultFunc: Result := SizeOf(TCreateTableStmt.TField.TDefaultFunc);
    ntCreateTableStmtForeignKey: Result := SizeOf(TCreateTableStmt.TForeignKey);
    ntCreateTableStmtKey: Result := SizeOf(TCreateTableStmt.TKey);
    ntCreateTableStmtKeyColumn: Result := SizeOf(TCreateTableStmt.TKeyColumn);
    ntCreateTableStmtPartition: Result := SizeOf(TCreateTableStmt.TPartition);
    ntCreateTableStmtReference: Result := SizeOf(TCreateTableStmt.TReference);
    ntCreateTriggerStmt: Result := SizeOf(TCreateTriggerStmt);
    ntCreateUserStmt: Result := SizeOf(TCreateUserStmt);
    ntCreateViewStmt: Result := SizeOf(TCreateViewStmt);
    ntCurrentTimestamp: Result := SizeOf(TCurrentTimestamp);
    ntDatatype: Result := SizeOf(TDatatype);
    ntDateAddFunc: Result := SizeOf(TDateAddFunc);
    ntDbIdent: Result := SizeOf(TDbIdent);
    ntDeallocatePrepareStmt: Result := SizeOf(TDeallocatePrepareStmt);
    ntDeclareStmt: Result := SizeOf(TDeclareStmt);
    ntDeclareConditionStmt: Result := SizeOf(TDeclareConditionStmt);
    ntDeclareCursorStmt: Result := SizeOf(TDeclareCursorStmt);
    ntDeclareHandlerStmt: Result := SizeOf(TDeclareHandlerStmt);
    ntDeleteStmt: Result := SizeOf(TDeleteStmt);
    ntDoStmt: Result := SizeOf(TDoStmt);
    ntDropDatabaseStmt: Result := SizeOf(TDropDatabaseStmt);
    ntDropEventStmt: Result := SizeOf(TDropEventStmt);
    ntDropIndexStmt: Result := SizeOf(TDropIndexStmt);
    ntDropRoutineStmt: Result := SizeOf(TDropRoutineStmt);
    ntDropServerStmt: Result := SizeOf(TDropServerStmt);
    ntDropTablespaceStmt: Result := SizeOf(TDropTablespaceStmt);
    ntDropTableStmt: Result := SizeOf(TDropTableStmt);
    ntDropTriggerStmt: Result := SizeOf(TDropTriggerStmt);
    ntDropUserStmt: Result := SizeOf(TDropUserStmt);
    ntDropViewStmt: Result := SizeOf(TDropViewStmt);
    ntEndLabel: Result := SizeOf(TEndLabel);
    ntExecuteStmt: Result := SizeOf(TExecuteStmt);
    ntExplainStmt: Result := SizeOf(TExplainStmt);
    ntExistsFunc: Result := SizeOf(TExistsFunc);
    ntExtractFunc: Result := SizeOf(TExtractFunc);
    ntFetchStmt: Result := SizeOf(TFetchStmt);
    ntFlushStmt: Result := SizeOf(TFlushStmt);
    ntFlushStmtOption: Result := SizeOf(TFlushStmt.TOption);
    ntFunctionCall: Result := SizeOf(TFunctionCall);
    ntFunctionReturns: Result := SizeOf(TFunctionReturns);
    ntIfStmt: Result := SizeOf(TIfStmt);
    ntIfStmtBranch: Result := SizeOf(TIfStmt.TBranch);
    ntGetDiagnosticsStmt: Result := SizeOf(TGetDiagnosticsStmt);
    ntGetDiagnosticsStmtStmtInfo: Result := SizeOf(TGetDiagnosticsStmt.TStmtInfo);
    ntGetDiagnosticsStmtCondInfo: Result := SizeOf(TGetDiagnosticsStmt.TStmtInfo);
    ntGrantStmt: Result := SizeOf(TGrantStmt);
    ntGrantStmtPrivileg: Result := SizeOf(TGrantStmt.TPrivileg);
    ntGrantStmtUserSpecification: Result := SizeOf(TGrantStmt.TUserSpecification);
    ntGroupConcatFunc: Result := SizeOf(TGroupConcatFunc);
    ntGroupConcatFuncExpr: Result := SizeOf(TGroupConcatFunc.TExpr);
    ntHelpStmt: Result := SizeOf(THelpStmt);
    ntInsertStmtSetItem: Result := SizeOf(TInsertStmt.TSetItem);
    ntInOp: Result := SizeOf(TInOp);
    ntInsertStmt: Result := SizeOf(TInsertStmt);
    ntIntervalOp: Result := SizeOf(TIntervalOp);
    ntIterateStmt: Result := SizeOf(TIterateStmt);
    ntKillStmt: Result := SizeOf(TKillStmt);
    ntLeaveStmt: Result := SizeOf(TLeaveStmt);
    ntLikeOp: Result := SizeOf(TLikeOp);
    ntList: Result := SizeOf(TList);
    ntLoadDataStmt: Result := SizeOf(TLoadDataStmt);
    ntLoadXMLStmt: Result := SizeOf(TLoadXMLStmt);
    ntLockTableStmt: Result := SizeOf(TLockTableStmt);
    ntLockTableStmtItem: Result := SizeOf(TLockTableStmt.TItem);
    ntLoopStmt: Result := SizeOf(TLoopStmt);
    ntMatchFunc: Result := SizeOf(TMatchFunc);
    ntOpenStmt: Result := SizeOf(TOpenStmt);
    ntOptimizeTableStmt: Result := SizeOf(TOptimizeTableStmt);
    ntPositionFunc: Result := SizeOf(TPositionFunc);
    ntPrepareStmt: Result := SizeOf(TPrepareStmt);
    ntPurgeStmt: Result := SizeOf(TPurgeStmt);
    ntRegExpOp: Result := SizeOf(TRegExpOp);
    ntRenameStmt: Result := SizeOf(TRenameStmt);
    ntRenameStmtPair: Result := SizeOf(TRenameStmt.TPair);
    ntReleaseStmt: Result := SizeOf(TReleaseStmt);
    ntRepairTableStmt: Result := SizeOf(TRepairTableStmt);
    ntRepeatStmt: Result := SizeOf(TRepeatStmt);
    ntResetStmt: Result := SizeOf(TResetStmt);
    ntResignalStmt: Result := SizeOf(TResignalStmt);
    ntReturnStmt: Result := SizeOf(TReturnStmt);
    ntRevokeStmt: Result := SizeOf(TRevokeStmt);
    ntRollbackStmt: Result := SizeOf(TRollbackStmt);
    ntRoutineParam: Result := SizeOf(TRoutineParam);
    ntSavepointStmt: Result := SizeOf(TSavepointStmt);
    ntSchedule: Result := SizeOf(TSchedule);
    ntSecretIdent: Result := SizeOf(TSecretIdent);
    ntSelectStmt: Result := SizeOf(TSelectStmt);
    ntSelectStmtColumn: Result := SizeOf(TSelectStmt.TColumn);
    ntSelectStmtGroup: Result := SizeOf(TSelectStmt.TGroup);
    ntSelectStmtOrder: Result := SizeOf(TSelectStmt.TOrder);
    ntSelectStmtTableFactor: Result := SizeOf(TSelectStmt.TTableFactor);
    ntSelectStmtTableFactorIndexHint: Result := SizeOf(TSelectStmt.TTableFactor.TIndexHint);
    ntSelectStmtTableFactorOj: Result := SizeOf(TSelectStmt.TTableFactorOj);
    ntSelectStmtTableFactorSelect: Result := SizeOf(TSelectStmt.TTableFactorSelect);
    ntSelectStmtTableJoin: Result := SizeOf(TSelectStmt.TTableJoin);
    ntSetNamesStmt: Result := SizeOf(TSetNamesStmt);
    ntSetPasswordStmt: Result := SizeOf(TSetPasswordStmt);
    ntSetStmt: Result := SizeOf(TSetStmt);
    ntSetStmtAssignment: Result := SizeOf(TSetStmt.TAssignment);
    ntSetTransactionStmt: Result := SizeOf(TSetTransactionStmt);
    ntSetTransactionStmtCharacteristic: Result := SizeOf(TSetTransactionStmt.TCharacteristic);
    ntTrimFunc: Result := SizeOf(TTrimFunc);
    ntShowAuthorsStmt: Result := SizeOf(TShowAuthorsStmt);
    ntShowBinaryLogsStmt: Result := SizeOf(TShowBinaryLogsStmt);
    ntShowBinlogEventsStmt: Result := SizeOf(TShowBinlogEventsStmt);
    ntShowCharacterSetStmt: Result := SizeOf(TShowCharacterSetStmt);
    ntShowCollationStmt: Result := SizeOf(TShowCollationStmt);
    ntShowColumnsStmt: Result := SizeOf(TShowColumnsStmt);
    ntShowContributorsStmt: Result := SizeOf(TShowContributorsStmt);
    ntShowCountErrorsStmt: Result := SizeOf(TShowCountErrorsStmt);
    ntShowCountWarningsStmt: Result := SizeOf(TShowCountWarningsStmt);
    ntShowCreateDatabaseStmt: Result := SizeOf(TShowCreateDatabaseStmt);
    ntShowCreateEventStmt: Result := SizeOf(TShowCreateEventStmt);
    ntShowCreateFunctionStmt: Result := SizeOf(TShowCreateFunctionStmt);
    ntShowCreateProcedureStmt: Result := SizeOf(TShowCreateProcedureStmt);
    ntShowCreateTableStmt: Result := SizeOf(TShowCreateTableStmt);
    ntShowCreateTriggerStmt: Result := SizeOf(TShowCreateTriggerStmt);
    ntShowCreateUserStmt: Result := SizeOf(TShowCreateUserStmt);
    ntShowCreateViewStmt: Result := SizeOf(TShowCreateViewStmt);
    ntShowDatabasesStmt: Result := SizeOf(TShowDatabasesStmt);
    ntShowEngineStmt: Result := SizeOf(TShowEngineStmt);
    ntShowEnginesStmt: Result := SizeOf(TShowEnginesStmt);
    ntShowErrorsStmt: Result := SizeOf(TShowErrorsStmt);
    ntShowEventsStmt: Result := SizeOf(TShowEventsStmt);
    ntShowFunctionCodeStmt: Result := SizeOf(TShowFunctionCodeStmt);
    ntShowFunctionStatusStmt: Result := SizeOf(TShowFunctionStatusStmt);
    ntShowGrantsStmt: Result := SizeOf(TShowGrantsStmt);
    ntShowIndexStmt: Result := SizeOf(TShowIndexStmt);
    ntShowMasterStatusStmt: Result := SizeOf(TShowMasterStatusStmt);
    ntShowOpenTablesStmt: Result := SizeOf(TShowOpenTablesStmt);
    ntShowPluginsStmt: Result := SizeOf(TShowPluginsStmt);
    ntShowPrivilegesStmt: Result := SizeOf(TShowPrivilegesStmt);
    ntShowProcedureCodeStmt: Result := SizeOf(TShowProcedureCodeStmt);
    ntShowProcedureStatusStmt: Result := SizeOf(TShowProcedureStatusStmt);
    ntShowProcessListStmt: Result := SizeOf(TShowProcessListStmt);
    ntShowProfileStmt: Result := SizeOf(TShowProfileStmt);
    ntShowProfilesStmt: Result := SizeOf(TShowProfilesStmt);
    ntShowRelaylogEventsStmt: Result := SizeOf(TShowRelaylogEventsStmt);
    ntShowSlaveHostsStmt: Result := SizeOf(TShowSlaveHostsStmt);
    ntShowSlaveStatusStmt: Result := SizeOf(TShowSlaveStatusStmt);
    ntShowStatusStmt: Result := SizeOf(TShowStatusStmt);
    ntShowTableStatusStmt: Result := SizeOf(TShowTableStatusStmt);
    ntShowTablesStmt: Result := SizeOf(TShowTablesStmt);
    ntShowTriggersStmt: Result := SizeOf(TShowTriggersStmt);
    ntShowVariablesStmt: Result := SizeOf(TShowVariablesStmt);
    ntShowWarningsStmt: Result := SizeOf(TShowWarningsStmt);
    ntShutdownStmt: Result := SizeOf(TShutdownStmt);
    ntSignalStmt: Result := SizeOf(TSignalStmt);
    ntSignalStmtInformation: Result := SizeOf(TSignalStmt.TInformation);
    ntSoundsLikeOp: Result := SizeOf(TSoundsLikeOp);
    ntStartSlaveStmt: Result := SizeOf(TStartSlaveStmt);
    ntStartTransactionStmt: Result := SizeOf(TStartTransactionStmt);
    ntStopSlaveStmt: Result := SizeOf(TStopSlaveStmt);
    ntSubArea: Result := SizeOf(TSubArea);
    ntSubAreaSelectStmt: Result := SizeOf(TSubAreaSelectStmt);
    ntSubPartition: Result := SizeOf(TSubPartition);
    ntSubquery: Result := SizeOf(TSubquery);
    ntSubstringFunc: Result := SizeOf(TSubstringFunc);
    ntTag: Result := SizeOf(TTag);
    ntTruncateStmt: Result := SizeOf(TTruncateStmt);
    ntUnaryOp: Result := SizeOf(TUnaryOp);
    ntUnknownStmt: Result := SizeOf(TUnknownStmt);
    ntUnlockTablesStmt: Result := SizeOf(TUnlockTablesStmt);
    ntUpdateStmt: Result := SizeOf(TUpdateStmt);
    ntUser: Result := SizeOf(TUser);
    ntUseStmt: Result := SizeOf(TUseStmt);
    ntValue: Result := SizeOf(TValue);
    ntVariableIdent: Result := SizeOf(TVariable);
    ntWeightStringFunc: Result := SizeOf(TWeightStringFunc);
    ntWeightStringFuncLevel: Result := SizeOf(TWeightStringFunc.TLevel);
    ntWhileStmt: Result := SizeOf(TWhileStmt);
    ntXAStmt: Result := SizeOf(TXAStmt);
    ntXAStmtID: Result := SizeOf(TXAStmt.TID);
    else raise Exception.Create(SArgumentOutOfRange);
  end;
end;

function TSQLParser.ParseAnalyzeTableStmt(): TOffset;
var
  Nodes: TAnalyzeTableStmt.TNodes;
begin
  FillChar(Nodes, SizeOf(Nodes), 0);

  Nodes.StmtTag := ParseTag(kiANALYZE);

  if (not Error) then
    if (IsTag(kiNO_WRITE_TO_BINLOG)) then
      Nodes.NoWriteToBinlogTag := ParseTag(kiNO_WRITE_TO_BINLOG)
    else if (IsTag(kiLOCAL)) then
      Nodes.NoWriteToBinlogTag := ParseTag(kiLOCAL);

  if (not Error) then
    Nodes.TableTag := ParseTag(kiTABLE);

  if (not Error) then
    Nodes.TablesList := ParseList(False, ParseTableIdent);

  Result := TAnalyzeTableStmt.Create(Self, Nodes);
end;

function TSQLParser.ParseAliasIdent(): TOffset;
begin
  Result := 0;
  if (EndOfStmt(CurrentToken)) then
    SetError(PE_IncompleteStmt)
  else if (not (TokenPtr(CurrentToken)^.TokenType in ttIdents + ttStrings)) then
    SetError(PE_UnexpectedToken)
  else
    Result := ApplyCurrentToken(utConst);
end;

function TSQLParser.ParseAlterDatabaseStmt(): TOffset;
var
  Found: Boolean;
  Nodes: TAlterDatabaseStmt.TNodes;
begin
  FillChar(Nodes, SizeOf(Nodes), 0);

  Nodes.StmtTag := ParseTag(kiALTER, TokenPtr(NextToken[1])^.KeywordIndex);

  if (TokenPtr(CurrentToken)^.TokenType in ttIdents) then
    Nodes.Ident := ParseDatabaseIdent();

  if (not Error) then
    if ((Nodes.Ident = 0) or not IsTag(kiUPGRADE, kiDATA, kiDIRECTORY, kiNAME)) then
    begin
      Found := True;
      while (not Error and Found and not EndOfStmt(CurrentToken)) do
        if ((Nodes.CharsetValue = 0) and IsTag(kiCHARACTER)) then
          Nodes.CharsetValue := ParseValue(WordIndices(kiCHARACTER, kiSET), vaAuto, ParseCharsetIdent)
        else if ((Nodes.CharsetValue = 0) and IsTag(kiCHARSET)) then
          Nodes.CharsetValue := ParseValue(kiCHARSET, vaAuto, ParseIdent)
        else if ((Nodes.CollateValue = 0) and IsTag(kiCOLLATE)) then
          Nodes.CollateValue := ParseValue(kiCOLLATE, vaAuto, ParseCollateIdent)
        else if ((Nodes.CharsetValue = 0) and IsTag(kiDEFAULT, kiCHARACTER, kiSET)) then
          Nodes.CharsetValue := ParseValue(WordIndices(kiDEFAULT, kiCHARACTER, kiSET), vaAuto, ParseCharsetIdent)
        else if ((Nodes.CharsetValue = 0) and IsTag(kiDEFAULT, kiCHARSET)) then
          Nodes.CharsetValue := ParseValue(WordIndices(kiDEFAULT, kiCHARSET), vaAuto, ParseCharsetIdent)
        else if ((Nodes.CollateValue = 0) and IsTag(kiDEFAULT, kiCOLLATE)) then
          Nodes.CollateValue := ParseValue(WordIndices(kiDEFAULT, kiCOLLATE), vaAuto, ParseCollateIdent)
        else
        begin
          SetError(PE_UnexpectedToken);
          Found := False;
        end;
    end
    else
    begin
      Nodes.UpgradeDataDirectoryNameTag := ParseTag(kiUPGRADE, kiDATA, kiDIRECTORY, kiNAME);
    end;

  Result := TAlterDatabaseStmt.Create(Self, Nodes);
end;

function TSQLParser.ParseAlterEventStmt(): TOffset;
var
  Nodes: TAlterEventStmt.TNodes;
begin
  FillChar(Nodes, SizeOf(Nodes), 0);

  Nodes.AlterTag := ParseTag(kiALTER);

  if (not Error) then
    if (IsTag(kiDEFINER)) then
      Nodes.DefinerValue := ParseDefinerValue();

  if (not Error) then
    Nodes.EventTag := ParseTag(kiEVENT);

  if (not Error) then
    Nodes.EventIdent := ParseEventIdent();

  if (not Error) then
    if (IsTag(kiON, kiSCHEDULE)) then
    begin
      Nodes.OnSchedule.Tag := ParseTag(kiON, kiSCHEDULE);

      if (not Error) then
        Nodes.OnSchedule.Value := ParseSchedule();
    end;

  if (not Error) then
    if (IsTag(kiON, kiCOMPLETION, kiPRESERVE)) then
      Nodes.OnCompletionTag := ParseTag(kiON, kiCOMPLETION, kiPRESERVE)
    else if (IsTag(kiON, kiCOMPLETION, kiNOT, kiPRESERVE)) then
      Nodes.OnCompletionTag := ParseTag(kiON, kiCOMPLETION, kiNOT, kiPRESERVE);

  if (not Error) then
    if (IsTag(kiRENAME, kiTO)) then
      Nodes.RenameValue := ParseValue(WordIndices(kiRENAME, kiTO), vaNo, ParseEventIdent);

  if (not Error) then
    if (IsTag(kiENABLE)) then
      Nodes.EnableTag := ParseTag(kiENABLE)
    else if (IsTag(kiDISABLE, kiON, kiSLAVE)) then
      Nodes.EnableTag := ParseTag(kiDISABLE, kiON, kiSLAVE)
    else if (IsTag(kiDISABLE)) then
      Nodes.EnableTag := ParseTag(kiDISABLE);

  if (not Error) then
    if (IsTag(kiCOMMENT)) then
      Nodes.CommentValue := ParseValue(kiCOMMENT, vaNo, ParseString);

  if (not Error) then
    if (IsTag(kiDO)) then
    begin
      Nodes.DoTag := ParseTag(kiDO);

      if (not Error) then
        Nodes.Body := ParsePL_SQLStmt();
    end;

  Result := TAlterEventStmt.Create(Self, Nodes);
end;

function TSQLParser.ParseAlterInstanceStmt(): TOffset;
var
  Nodes: TAlterInstanceStmt.TNodes;
begin
  FillChar(Nodes, SizeOf(Nodes), 0);

  Nodes.StmtTag := ParseTag(kiALTER, kiINSTANCE, kiROTATE, kiINNODB, kiMASTER, kiKEY);

  Result := TAlterInstanceStmt.Create(Self, Nodes);
end;

function TSQLParser.ParseAlterRoutineStmt(const ARoutineType: TRoutineType): TOffset;
var
  Found: Boolean;
  Nodes: TAlterRoutineStmt.TNodes;
begin
  FillChar(Nodes, SizeOf(Nodes), 0);

  Nodes.AlterTag := ParseTag(kiALTER, TokenPtr(NextToken[1])^.KeywordIndex);

  if (not Error) then
    if (ARoutineType = rtFunction) then
      Nodes.Ident := ParseFunctionIdent()
    else
      Nodes.Ident := ParseProcedureIdent();

  Found := True;
  while (not Error and Found) do
    if ((Nodes.CommentValue = 0) and IsTag(kiCOMMENT)) then
      Nodes.CommentValue := ParseValue(kiComment, vaNo, ParseString)
    else if ((Nodes.LanguageTag = 0) and IsTag(kiLANGUAGE, kiSQL)) then
      Nodes.LanguageTag := ParseTag(kiLANGUAGE, kiSQL)
    else if ((Nodes.DeterministicTag = 0) and IsTag(kiDETERMINISTIC)) then
      Nodes.DeterministicTag := ParseTag(kiDETERMINISTIC)
    else if ((Nodes.DeterministicTag = 0) and IsTag(kiNOT, kiDETERMINISTIC)) then
      Nodes.DeterministicTag := ParseTag(kiNOT, kiDETERMINISTIC)
    else if ((Nodes.NatureOfDataTag = 0) and IsTag(kiCONTAINS, kiSQL)) then
      Nodes.NatureOfDataTag := ParseTag(kiCONTAINS, kiSQL)
    else if ((Nodes.NatureOfDataTag = 0) and IsTag(kiNO, kiSQL)) then
      Nodes.NatureOfDataTag := ParseTag(kiNO, kiSQL)
    else if ((Nodes.NatureOfDataTag = 0) and IsTag(kiREADS, kiSQL, kiDATA)) then
      Nodes.NatureOfDataTag := ParseTag(kiREADS, kiSQL, kiDATA)
    else if ((Nodes.NatureOfDataTag = 0) and IsTag(kiMODIFIES, kiSQL, kiDATA)) then
      Nodes.NatureOfDataTag := ParseTag(kiMODIFIES, kiSQL, kiDATA)
    else if ((Nodes.NatureOfDataTag = 0) and IsTag(kiSQL, kiSECURITY, kiDEFINER)) then
      Nodes.SQLSecurityTag := ParseTag(kiSQL, kiSECURITY, kiDEFINER)
    else if ((Nodes.NatureOfDataTag = 0) and IsTag(kiSQL, kiSECURITY, kiINVOKER)) then
      Nodes.SQLSecurityTag := ParseTag(kiSQL, kiSECURITY, kiINVOKER)
    else
      Found := False;

  Result := TAlterRoutineStmt.Create(Self, ARoutineType, Nodes);
end;

function TSQLParser.ParseAlterServerStmt(): TOffset;
var
  Nodes: TAlterServerStmt.TNodes;
begin
  FillChar(Nodes, SizeOf(Nodes), 0);

  Nodes.StmtTag := ParseTag(kiALTER, kiSERVER);

  if (not Error) then
    Nodes.Ident := ParseDbIdent(ditServer);

  if (not Error) then
    Nodes.Options.Tag := ParseTag(kiOPTIONS);

  if (not Error) then
    Nodes.Options.List := ParseCreateServerStmtOptionList();

  Result := TAlterServerStmt.Create(Self, Nodes);
end;

function TSQLParser.ParseAlterStmt(): TOffset;
var
  AlgorithmFound: Boolean;
  DefinerFound: Boolean;
  Index: Integer;
  IgnoreFound: Boolean;
  SQLSecurityFound: Boolean;
begin
  Assert(TokenPtr(CurrentToken)^.KeywordIndex = kiALTER);

  AlgorithmFound := False;
  DefinerFound := False;
  IgnoreFound := False;
  SQLSecurityFound := False;

  Index := 1;

  // IGNORE is used for TABLE
  if (not Error and not EndOfStmt(NextToken[Index]) and (TokenPtr(NextToken[Index])^.KeywordIndex = kiIGNORE)) then
  begin
    IgnoreFound := True;

    Inc(Index); // IGNORE
  end;

  // ALGORITHM is used for VIEW
  if (not Error and not EndOfStmt(NextToken[Index]) and (TokenPtr(NextToken[Index])^.KeywordIndex = kiALGORITHM)) then
  begin
    AlgorithmFound := True;

    Inc(Index); // ALGORITHM
    if (EndOfStmt(NextToken[Index])) then
      SetError(PE_IncompleteStmt)
    else if (TokenPtr(NextToken[Index])^.OperatorType <> otEqual) then
      SetError(PE_UnexpectedToken, NextToken[Index])
    else
    begin
      Inc(Index); // =

      if (EndOfStmt(NextToken[Index])) then
      begin
        CompletionList.AddKeyword(kiUNDEFINED);
        CompletionList.AddKeyword(kiMERGE);
        CompletionList.AddKeyword(kiTEMPTABLE);
        SetError(PE_IncompleteStmt);
      end
      else if ((TokenPtr(NextToken[Index])^.KeywordIndex <> kiUNDEFINED)
        and (TokenPtr(NextToken[Index])^.KeywordIndex <> kiMERGE)
        and (TokenPtr(NextToken[Index])^.KeywordIndex <> kiTEMPTABLE)) then
        SetError(PE_UnexpectedToken, NextToken[Index])
      else
        Inc(Index); // UNDEFINED or MERGE or TEMPTABLE
    end;
  end;

  // DEFINER is used for EVENT, VIEW
  if (not Error and not EndOfStmt(NextToken[Index]) and (TokenPtr(NextToken[Index])^.KeywordIndex = kiDEFINER)) then
  begin
    DefinerFound := True;

    Inc(Index); // DEFINER
    if (EndOfStmt(NextToken[Index])) then
      SetError(PE_IncompleteStmt)
    else if (TokenPtr(NextToken[Index])^.OperatorType <> otEqual) then
      SetError(PE_UnexpectedToken, NextToken[Index])
    else
    begin
      Inc(Index); // =

      if (EndOfStmt(NextToken[Index])) then
      begin
        CompletionList.AddKeyword(kiCURRENT_USER);
        CompletionList.AddList(ditUser);
        SetError(PE_IncompleteStmt);
      end
      else if (TokenPtr(NextToken[Index])^.KeywordIndex = kiCURRENT_USER) then
        Inc(Index) // CURRENT_USER
      else
      begin
        Inc(Index); // Username

        if (EndOfStmt(NextToken[Index])) then
          SetError(PE_IncompleteStmt)
        else if (TokenPtr(NextToken[Index])^.TokenType <> ttAt) then
          SetError(PE_UnexpectedToken)
        else
        begin
          Inc(Index); // @

          if (EndOfStmt(NextToken[Index])) then
            SetError(PE_IncompleteStmt)
          else
            Inc(Index); // Servername
        end;
      end;
    end;
  end;

  // SQL SECURITY is used for VIEW
  if (not Error and not EndOfStmt(NextToken[Index]) and (TokenPtr(NextToken[Index])^.KeywordIndex = kiSQL)) then
  begin
    SQLSecurityFound := True;

    Inc(Index); // SQL
    if (EndOfStmt(NextToken[Index])) then
    begin
      CompletionList.AddKeyword(kiSECURITY, kiDEFINER);
      CompletionList.AddKeyword(kiSECURITY, kiINVOKER);
      SetError(PE_IncompleteStmt);
    end
    else if (TokenPtr(NextToken[Index])^.KeywordIndex <> kiSECURITY) then
      SetError(PE_UnexpectedToken, NextToken[Index])
    else
    begin
      Inc(Index); // SECURITY

      if (EndOfStmt(NextToken[Index])) then
      begin
        CompletionList.AddKeyword(kiDEFINER);
        CompletionList.AddKeyword(kiINVOKER);
        SetError(PE_IncompleteStmt);
      end
      else if ((TokenPtr(NextToken[Index])^.KeywordIndex <> kiDEFINER)
        and (TokenPtr(NextToken[Index])^.KeywordIndex <> kiINVOKER)) then
        SetError(PE_UnexpectedToken, NextToken[Index])
      else
        Inc(Index); // DEFINER or INVOKER
    end;
  end;

  Result := 0;
  if (not Error) then
    if (EndOfStmt(NextToken[Index])) then
    begin
      if (not AlgorithmFound and not DefinerFound and not IgnoreFound and not SQLSecurityFound) then
        CompletionList.AddKeyword(kiALGORITHM);
      if (not AlgorithmFound and not DefinerFound and not IgnoreFound and not SQLSecurityFound) then
        CompletionList.AddKeyword(kiDATABASE);
      if (not DefinerFound and not IgnoreFound and not SQLSecurityFound) then
        CompletionList.AddKeyword(kiDEFINER);
      if (not AlgorithmFound and not IgnoreFound and not SQLSecurityFound) then
        CompletionList.AddKeyword(kiEVENT);
      if (not AlgorithmFound and not DefinerFound and not IgnoreFound and not SQLSecurityFound) then
        CompletionList.AddKeyword(kiFUNCTION);
      if (not AlgorithmFound and not DefinerFound and not SQLSecurityFound) then
        CompletionList.AddKeyword(kiIGNORE);
      if (not AlgorithmFound and not DefinerFound and not IgnoreFound and not SQLSecurityFound) then
        CompletionList.AddKeyword(kiINSTANCE);
      if (not AlgorithmFound and not DefinerFound and not IgnoreFound and not SQLSecurityFound) then
        CompletionList.AddKeyword(kiPROCEDURE);
      if (not AlgorithmFound and not DefinerFound and not IgnoreFound and not SQLSecurityFound) then
        CompletionList.AddKeyword(kiSCHEMA);
      if (not AlgorithmFound and not DefinerFound and not IgnoreFound and not SQLSecurityFound) then
        CompletionList.AddKeyword(kiSERVER);
      if (not IgnoreFound and not SQLSecurityFound) then
      begin
        CompletionList.AddKeyword(kiSQL, kiSECURITY, kiDEFINER);
        CompletionList.AddKeyword(kiSQL, kiSECURITY, kiINVOKER);
      end;
      if (not AlgorithmFound and not DefinerFound and not SQLSecurityFound) then
        CompletionList.AddKeyword(kiTABLE);
      if (not AlgorithmFound and not DefinerFound and not IgnoreFound and not SQLSecurityFound) then
        CompletionList.AddKeyword(kiTABLESPACE);
      if (not IgnoreFound) then
        CompletionList.AddKeyword(kiVIEW);
      SetError(PE_IncompleteStmt);
    end
    else if (not AlgorithmFound and not DefinerFound and not IgnoreFound and not SQLSecurityFound
      and (TokenPtr(NextToken[Index])^.KeywordIndex = kiDATABASE)) then
      Result := ParseAlterDatabaseStmt()
    else if (not AlgorithmFound and not IgnoreFound and not SQLSecurityFound
      and (TokenPtr(NextToken[Index])^.KeywordIndex = kiEVENT)) then
      Result := ParseAlterEventStmt()
    else if (not AlgorithmFound and not DefinerFound and not IgnoreFound and not SQLSecurityFound
      and (TokenPtr(NextToken[Index])^.KeywordIndex = kiFUNCTION)) then
      Result := ParseAlterRoutineStmt(rtFunction)
    else if (not AlgorithmFound and not DefinerFound and not IgnoreFound and not SQLSecurityFound
      and (TokenPtr(NextToken[Index])^.KeywordIndex = kiINSTANCE)) then
      Result := ParseAlterInstanceStmt()
    else if (not AlgorithmFound and not DefinerFound and not IgnoreFound and not SQLSecurityFound
      and (TokenPtr(NextToken[Index])^.KeywordIndex = kiPROCEDURE)) then
      Result := ParseAlterRoutineStmt(rtProcedure)
    else if (not AlgorithmFound and not DefinerFound and not IgnoreFound and not SQLSecurityFound
      and (TokenPtr(NextToken[Index])^.KeywordIndex = kiSCHEMA)) then
      Result := ParseAlterDatabaseStmt()
    else if (not AlgorithmFound and not DefinerFound and not IgnoreFound and not SQLSecurityFound
      and (TokenPtr(NextToken[Index])^.KeywordIndex = kiSERVER)) then
      Result := ParseAlterServerStmt()
    else if (not AlgorithmFound and not DefinerFound and not SQLSecurityFound
      and (TokenPtr(NextToken[Index])^.KeywordIndex = kiTABLE)) then
      Result := ParseAlterTableStmt()
    else if (not AlgorithmFound and not DefinerFound and not IgnoreFound and not SQLSecurityFound
      and (TokenPtr(NextToken[Index])^.KeywordIndex = kiTABLESPACE)) then
      Result := ParseAlterTablespaceStmt()
    else if (not AlgorithmFound and not DefinerFound and not IgnoreFound and not SQLSecurityFound
      and (TokenPtr(NextToken[Index])^.KeywordIndex = kiUSER)) then
      Result := ParseCreateUserStmt(True)
    else if (not IgnoreFound
      and (TokenPtr(NextToken[Index])^.KeywordIndex = kiVIEW)) then
      Result := ParseAlterViewStmt()
    else
      SetError(PE_UnexpectedToken, NextToken[Index]);
end;

function TSQLParser.ParseAlterTablespaceStmt(): TOffset;
var
  Nodes: TAlterTablespaceStmt.TNodes;
begin
  FillChar(Nodes, SizeOf(Nodes), 0);

  Nodes.StmtTag := ParseTag(kiALTER, kiTABLESPACE);

  if (not Error) then
    if (IsTag(kiADD, kiDATAFILE)) then
      Nodes.AddDatafileValue := ParseValue(WordIndices(kiADD, kiDATAFILE), vaNo, ParseString)
    else
      Nodes.AddDatafileValue := ParseValue(WordIndices(kiDROP, kiDATAFILE), vaNo, ParseString);

  if (not Error) then
    if (IsTag(kiINITIAL_SIZE)) then
      Nodes.InitialSizeValue := ParseValue(kiINITIAL_SIZE, vaAuto, ParseInteger);

  if (not Error) then
    Nodes.EngineValue := ParseValue(kiENGINE, vaAuto, ParseEngineIdent);

  Result := TAlterTablespaceStmt.Create(Self, Nodes);
end;

function TSQLParser.ParseAlterTableStmt(): TOffset;
var
  Found: Boolean;
  Found2: Boolean;
  ListNodes: TList.TNodes;
  Nodes: TAlterTableStmt.TNodes;
  OldSpecificationsCound: Integer;
  Specifications: Classes.TList;
  TableOptionFound: Boolean;
  TableOptionNodes: TTableOptionNodes;
begin
  FillChar(Nodes, SizeOf(Nodes), 0);
  FillChar(TableOptionNodes, SizeOf(TableOptionNodes), 0);

  Specifications := Classes.TList.Create();

  Nodes.AlterTag := ParseTag(kiALTER);

  if (not Error) then
    if (IsTag(kiIGNORE)) then
      Nodes.IgnoreTag := ParseTag(kiIGNORE);

  if (not Error) then
    Nodes.TableTag := ParseTag(kiTABLE);

  if (not Error) then
    Nodes.Ident := ParseTableIdent();


  Found := True;
  while (not Error and Found) do
  begin

    Found2 := True; OldSpecificationsCound := Specifications.Count;
    while (not Error and Found2) do
    begin
      if ((TableOptionNodes.AutoIncrementValue = 0) and IsTag(kiAUTO_INCREMENT)) then
      begin
        TableOptionNodes.AutoIncrementValue := ParseValue(kiAUTO_INCREMENT, vaAuto, ParseInteger);
        Specifications.Add(Pointer(TableOptionNodes.AutoIncrementValue));
      end
      else if ((TableOptionNodes.AvgRowLengthValue = 0) and IsTag(kiAVG_ROW_LENGTH)) then
      begin
        TableOptionNodes.AvgRowLengthValue := ParseValue(kiAVG_ROW_LENGTH, vaAuto, ParseInteger);
        Specifications.Add(Pointer(TableOptionNodes.AvgRowLengthValue));
      end
      else if ((TableOptionNodes.CharacterSetValue = 0) and IsTag(kiCHARACTER, kiSET)) then
      begin
        TableOptionNodes.CharacterSetValue := ParseValue(WordIndices(kiCHARACTER, kiSET), vaAuto, ParseCharsetIdent);
        Specifications.Add(Pointer(TableOptionNodes.CharacterSetValue));
      end
      else if ((TableOptionNodes.CharacterSetValue = 0) and IsTag(kiCHARSET)) then
      begin
        TableOptionNodes.CharacterSetValue := ParseValue(kiCHARSET, vaAuto, ParseCharsetIdent);
        Specifications.Add(Pointer(TableOptionNodes.CharacterSetValue));
      end
      else if ((TableOptionNodes.ChecksumValue = 0) and IsTag(kiCHECKSUM)) then
      begin
        TableOptionNodes.ChecksumValue := ParseValue(kiCHECKSUM, vaAuto, ParseInteger);
        Specifications.Add(Pointer(TableOptionNodes.ChecksumValue));
      end
      else if ((TableOptionNodes.CollateValue = 0) and IsTag(kiCOLLATE)) then
      begin
        TableOptionNodes.CollateValue := ParseValue(kiCOLLATE, vaAuto, ParseCollateIdent);
        Specifications.Add(Pointer(TableOptionNodes.CollateValue));
      end
      else if ((TableOptionNodes.CharacterSetValue = 0) and IsTag(kiDEFAULT, kiCHARACTER, kiSET)) then
      begin
        TableOptionNodes.CharacterSetValue := ParseValue(WordIndices(kiDEFAULT, kiCHARACTER, kiSET), vaAuto, ParseCharsetIdent);
        Specifications.Add(Pointer(TableOptionNodes.CharacterSetValue));
      end
      else if ((TableOptionNodes.CharacterSetValue = 0) and IsTag(kiDEFAULT, kiCHARSET)) then
      begin
        TableOptionNodes.CharacterSetValue := ParseValue(WordIndices(kiDEFAULT, kiCHARSET), vaAuto, ParseCharsetIdent);
        Specifications.Add(Pointer(TableOptionNodes.CharacterSetValue));
      end
      else if ((TableOptionNodes.CollateValue = 0) and IsTag(kiDEFAULT, kiCOLLATE)) then
      begin
        TableOptionNodes.CollateValue := ParseValue(WordIndices(kiDEFAULT, kiCOLLATE), vaAuto, ParseCollateIdent);
        Specifications.Add(Pointer(TableOptionNodes.CollateValue));
      end
      else if ((TableOptionNodes.CommentValue = 0) and IsTag(kiCOMMENT)) then
      begin
        TableOptionNodes.CommentValue := ParseValue(kiCOMMENT, vaAuto, ParseString);
        Specifications.Add(Pointer(TableOptionNodes.CommentValue));
      end
      else if ((TableOptionNodes.CompressValue = 0) and IsTag(kiCOMPRESS)) then
      begin
        TableOptionNodes.CompressValue := ParseValue(kiCOMPRESS, vaAuto, ParseString);
        Specifications.Add(Pointer(TableOptionNodes.CompressValue));
      end
      else if ((TableOptionNodes.ConnectionValue = 0) and IsTag(kiCONNECTION)) then
      begin
        TableOptionNodes.ConnectionValue := ParseValue(kiCONNECTION, vaAuto, ParseString);
        Specifications.Add(Pointer(TableOptionNodes.ConnectionValue));
      end
      else if ((TableOptionNodes.DataDirectoryValue = 0) and IsTag(kiDATA, kiDIRECTORY)) then
      begin
        TableOptionNodes.DataDirectoryValue := ParseValue(WordIndices(kiDATA, kiDIRECTORY), vaAuto, ParseString);
        Specifications.Add(Pointer(TableOptionNodes.DataDirectoryValue));
      end
      else if ((TableOptionNodes.DelayKeyWriteValue = 0) and IsTag(kiDELAY_KEY_WRITE)) then
      begin
        TableOptionNodes.DelayKeyWriteValue := ParseValue(kiDELAY_KEY_WRITE, vaAuto, ParseInteger);
        Specifications.Add(Pointer(TableOptionNodes.DelayKeyWriteValue));
      end
      else if ((TableOptionNodes.EngineValue = 0) and IsTag(kiENGINE)) then
      begin
        TableOptionNodes.EngineValue := ParseValue(kiENGINE, vaAuto, ParseEngineIdent);
        Specifications.Add(Pointer(TableOptionNodes.EngineValue));
      end
      else if ((TableOptionNodes.IndexDirectoryValue = 0) and IsTag(kiINDEX, kiDIRECTORY)) then
      begin
        TableOptionNodes.IndexDirectoryValue := ParseValue(WordIndices(kiINDEX, kiDIRECTORY), vaAuto, ParseString);
        Specifications.Add(Pointer(TableOptionNodes.IndexDirectoryValue));
      end
      else if ((TableOptionNodes.InsertMethodValue = 0) and IsTag(kiINSERT_METHOD)) then
      begin
        TableOptionNodes.InsertMethodValue := ParseValue(kiINSERT_METHOD, vaAuto, WordIndices(kiNO, kiFIRST, kiLAST));
        Specifications.Add(Pointer(TableOptionNodes.InsertMethodValue));
      end
      else if ((TableOptionNodes.KeyBlockSizeValue = 0) and IsTag(kiKEY_BLOCK_SIZE)) then
      begin
        TableOptionNodes.KeyBlockSizeValue := ParseValue(kiKEY_BLOCK_SIZE, vaAuto, ParseInteger);
        Specifications.Add(Pointer(TableOptionNodes.KeyBlockSizeValue));
      end
      else if ((TableOptionNodes.MaxRowsValue = 0) and IsTag(kiMAX_ROWS)) then
      begin
        TableOptionNodes.MaxRowsValue := ParseValue(kiMAX_ROWS, vaAuto, ParseInteger);
        Specifications.Add(Pointer(TableOptionNodes.MaxRowsValue));
      end
      else if ((TableOptionNodes.MinRowsValue = 0) and IsTag(kiMIN_ROWS)) then
      begin
        TableOptionNodes.MinRowsValue := ParseValue(kiMIN_ROWS, vaAuto, ParseInteger);
        Specifications.Add(Pointer(TableOptionNodes.MinRowsValue));
      end
      else if ((TableOptionNodes.PackKeysValue = 0) and IsTag(kiPACK_KEYS)) then
      begin
        TableOptionNodes.PackKeysValue := ParseValue(kiPACK_KEYS, vaAuto, ParseExpr);
        Specifications.Add(Pointer(TableOptionNodes.PackKeysValue));
      end
      else if ((TableOptionNodes.PageChecksumValue = 0) and IsTag(kiPAGE_CHECKSUM)) then
      begin
        TableOptionNodes.PageChecksumValue := ParseValue(kiPAGE_CHECKSUM, vaAuto, ParseInteger);
        Specifications.Add(Pointer(TableOptionNodes.PageChecksumValue));
      end
      else if ((TableOptionNodes.PasswordValue = 0) and IsTag(kiPASSWORD)) then
      begin
        TableOptionNodes.PasswordValue := ParseValue(kiPASSWORD, vaAuto, ParseString);
        Specifications.Add(Pointer(TableOptionNodes.PasswordValue));
      end
      else if ((TableOptionNodes.RowFormatValue = 0) and IsTag(kiROW_FORMAT)) then
      begin
        TableOptionNodes.RowFormatValue := ParseValue(kiROW_FORMAT, vaAuto, ParseIdent);
        Specifications.Add(Pointer(TableOptionNodes.RowFormatValue));
      end
      else if ((TableOptionNodes.StatsAutoRecalcValue = 0) and IsTag(kiSTATS_AUTO_RECALC, kiDEFAULT)) then
      begin
        TableOptionNodes.StatsAutoRecalcValue := ParseValue(kiSTATS_AUTO_RECALC, vaAuto, WordIndices(kiDEFAULT));
        Specifications.Add(Pointer(TableOptionNodes.StatsAutoRecalcValue));
      end
      else if ((TableOptionNodes.StatsAutoRecalcValue = 0) and IsTag(kiSTATS_AUTO_RECALC)) then
      begin
        TableOptionNodes.StatsAutoRecalcValue := ParseValue(kiSTATS_AUTO_RECALC, vaAuto, ParseInteger);
        Specifications.Add(Pointer(TableOptionNodes.StatsAutoRecalcValue));
      end
      else if ((TableOptionNodes.StatsPersistentValue = 0) and IsTag(kiSTATS_PERSISTENT, kiDEFAULT)) then
      begin
        TableOptionNodes.StatsPersistentValue := ParseValue(WordIndices(kiSTATS_PERSISTENT, kiDEFAULT), vaAuto, WordIndices(kiDEFAULT));
        Specifications.Add(Pointer(TableOptionNodes.StatsPersistentValue));
      end
      else if ((TableOptionNodes.StatsPersistentValue = 0) and IsTag(kiSTATS_PERSISTENT)) then
      begin
        TableOptionNodes.StatsPersistentValue := ParseValue(kiSTATS_PERSISTENT, vaAuto, ParseInteger);
        Specifications.Add(Pointer(TableOptionNodes.StatsPersistentValue));
      end
      else if ((TableOptionNodes.TransactionalValue = 0) and IsTag(kiTRANSACTIONAL)) then
      begin
        TableOptionNodes.TransactionalValue := ParseValue(kiTRANSACTIONAL, vaAuto, ParseInteger);
        Specifications.Add(Pointer(TableOptionNodes.TransactionalValue));
      end
      else if ((TableOptionNodes.EngineValue = 0) and IsTag(kiTYPE)) then
      begin
        TableOptionNodes.EngineValue := ParseValue(kiTYPE, vaAuto, ParseEngineIdent);
        Specifications.Add(Pointer(TableOptionNodes.EngineValue));
      end
      else if ((TableOptionNodes.UnionList = 0) and IsTag(kiUNION)) then
      begin
        TableOptionNodes.UnionList := ParseCreateTableStmtUnion();
        Specifications.Add(Pointer(TableOptionNodes.UnionList));
      end
      else
        Found2 := False;
    end;
    TableOptionFound := Specifications.Count > OldSpecificationsCound;


    if (TableOptionFound) then


    else if (IsTag(kiADD)) then
      Specifications.Add(Pointer(ParseCreateTableStmtDefinition(True)))
    else if (IsTag(kiALTER)) then
      Specifications.Add(Pointer(ParseAlterTableStmtAlterColumn()))
    else if (IsTag(kiCHANGE)) then
      Specifications.Add(Pointer(ParseCreateTableStmtField(caChange)))
    else if (IsTag(kiDROP)) then
      Specifications.Add(Pointer(ParseAlterTableStmtDropItem()))
    else if (IsTag(kiMODIFY)) then
      Specifications.Add(Pointer(ParseCreateTableStmtField(caModify)))


    else if ((Nodes.AlgorithmValue = 0) and IsTag(kiALGORITHM)) then
    begin
      Nodes.AlgorithmValue := ParseValue(kiALGORITHM, vaAuto, WordIndices(kiDEFAULT, kiINPLACE, kiCOPY));
      Specifications.Add(Pointer(Nodes.AlgorithmValue));
    end
    else if ((Nodes.ConvertToCharacterSetNode = 0) and IsTag(kiCONVERT)) then
    begin
      Nodes.ConvertToCharacterSetNode := ParseAlterTableStmtConvertTo();
      Specifications.Add(Pointer(Nodes.ConvertToCharacterSetNode));
    end
    else if ((Nodes.EnableKeys = 0) and IsTag(kiDISABLE, kiKEYS)) then
    begin
      Nodes.EnableKeys := ParseTag(kiDISABLE, kiKEYS);
      Specifications.Add(Pointer(Nodes.EnableKeys));
    end
    else if ((Nodes.DiscardTablespaceTag = 0) and IsTag(kiDISCARD, kiTABLESPACE)) then
    begin
      Nodes.DiscardTablespaceTag := ParseTag(kiDISCARD, kiTABLESPACE);
      Specifications.Add(Pointer(Nodes.DiscardTablespaceTag));
    end
    else if ((Nodes.EnableKeys = 0) and IsTag(kiENABLE, kiKEYS)) then
    begin
      Nodes.EnableKeys := ParseTag(kiENABLE, kiKEYS);
      Specifications.Add(Pointer(Nodes.EnableKeys));
    end
    else if ((Nodes.ForceTag = 0) and IsTag(kiFORCE)) then
    begin
      Nodes.ForceTag := ParseTag(kiFORCE);
      Specifications.Add(Pointer(Nodes.ForceTag));
    end
    else if ((Nodes.ImportTablespaceTag = 0) and IsTag(kiDISCARD, kiTABLESPACE)) then
    begin
      Nodes.ImportTablespaceTag := ParseTag(kiDISCARD, kiTABLESPACE);
      Specifications.Add(Pointer(Nodes.ImportTablespaceTag));
    end
    else if ((Nodes.LockValue = 0) and IsTag(kiLOCK)) then
    begin
      Nodes.LockValue := ParseValue(kiLOCK, vaAuto, WordIndices(kiDEFAULT, kiNONE, kiSHARED, kiEXCLUSIVE));
      Specifications.Add(Pointer(Nodes.LockValue));
    end
    else if ((Nodes.OrderByValue = 0) and IsTag(kiORDER, kiBY)) then
    begin
      Nodes.OrderByValue := ParseAlterTableStmtOrderBy();
      Specifications.Add(Pointer(Nodes.OrderByValue));
    end
    else if ((Nodes.RenameNode = 0) and IsTag(kiRENAME, kiTO)) then
    begin
      Nodes.RenameNode := ParseValue(WordIndices(kiRENAME, kiTO), vaNo, ParseTableIdent);
      Specifications.Add(Pointer(Nodes.RenameNode));
    end
    else if ((Nodes.RenameNode = 0) and IsTag(kiRENAME, kiAS)) then
    begin
      Nodes.RenameNode := ParseValue(WordIndices(kiRENAME, kiAS), vaNo, ParseTableIdent);
      Specifications.Add(Pointer(Nodes.RenameNode));
    end
    else if ((Nodes.RenameNode = 0) and IsTag(kiRENAME)) then
    begin
      Nodes.RenameNode := ParseValue(kiRENAME, vaNo, ParseTableIdent);
      Specifications.Add(Pointer(Nodes.RenameNode));
    end


    else if (IsTag(kiANALYZE, kiPARTITION)
      or IsTag(kiCHECK, kiPARTITION)
      or IsTag(kiOPTIMIZE, kiPARTITION)
      or IsTag(kiREBUILD, kiPARTITION)
      or IsTag(kiREPAIR, kiPARTITION)
      or IsTag(kiTRUNCATE, kiPARTITION)) then
      Specifications.Add(Pointer(ParseValue(TokenPtr(CurrentToken)^.KeywordIndex, vaNo, ParseCreateTableStmtDefinitionPartitionNames)))
    else if (IsTag(kiCOALESCE, kiPARTITION)) then
      Specifications.Add(Pointer(ParseValue(kiCOALESCE, vaNo, ParseInteger)))
    else if (IsTag(kiEXCHANGE, kiPARTITION)) then
      Specifications.Add(Pointer(ParseAlterTableStmtExchangePartition()))
    else if (IsTag(kiREMOVE, kiPARTITION)) then
      Specifications.Add(Pointer(ParseTag(kiREMOVE, kiPARTITIONING)))
    else if (IsTag(kiREORGANIZE, kiPARTITION)) then
      Specifications.Add(Pointer(ParseAlterTableStmtReorganizePartition()))
    else
      Found := False;

    if (not Error and Found and IsSymbol(ttComma)) then
      Specifications.Add(Pointer(ParseSymbol(ttComma)));
  end;

  FillChar(ListNodes, SizeOf(ListNodes), 0);
  Nodes.SpecificationList := TList.Create(Self, ListNodes, ttComma, Specifications.Count, TIntegerArray(Specifications.List));
  Result := TAlterTableStmt.Create(Self, Nodes);

  Specifications.Free();
end;

function TSQLParser.ParseAlterTableStmtAlterColumn(): TOffset;
var
  Nodes: TAlterTableStmt.TAlterColumn.TNodes;
begin
  FillChar(Nodes, SizeOf(Nodes), 0);

  if (IsTag(kiALTER, kiCOLUMN)) then
    Nodes.AlterTag := ParseTag(kiALTER, kiCOLUMN)
  else
    Nodes.AlterTag := ParseTag(kiALTER);

  if (not Error) then
    Nodes.Ident := ParseDbIdent(ditField, False);

  if (not Error) then
    if (IsTag(kiSET, kiDEFAULT)) then
      Nodes.SetDefaultValue := ParseValue(WordIndices(kiSET, kiDEFAULT), vaNo, ParseExpr)
    else
      Nodes.DropDefaultTag := ParseTag(kiDROP, kiDEFAULT);

  Result := TAlterTableStmt.TAlterColumn.Create(Self, Nodes);
end;

function TSQLParser.ParseAlterTableStmtConvertTo(): TOffset;
var
  Nodes: TAlterTableStmt.TConvertTo.TNodes;
begin
  FillChar(Nodes, SizeOf(Nodes), 0);

  Nodes.ConvertToTag := ParseTag(kiCONVERT, kiTO);

  if (not Error) then
    if (IsTag(kiCHARSET)) then
      Nodes.CharsetValue := ParseValue(kiCHARSET, vaNo, ParseCharsetIdent)
    else
      Nodes.CharsetValue := ParseValue(WordIndices(kiCHARACTER, kiSET), vaNo, ParseCharsetIdent);

  if (not Error) then
    if (IsTag(kiCOLLATE)) then
      Nodes.CollateValue := ParseValue(kiCOLLATE, vaNo, ParseCollateIdent);

  Result := TAlterTableStmt.TConvertTo.Create(Self, Nodes);
end;

function TSQLParser.ParseAlterTableStmtDropItem(): TOffset;
var
  Nodes: TAlterTableStmt.TDropObject.TNodes;
begin
  FillChar(Nodes, SizeOf(Nodes), 0);

  Nodes.DropTag := ParseTag(kiDROP);

  if (not Error) then
    if (IsTag(kiFOREIGN, kiKEY)) then
    begin
      Nodes.ItemTypeTag := ParseTag(kiFOREIGN, kiKEY);

      if (not Error) then
        Nodes.Ident := ParseForeignKeyIdent();
    end
    else if (IsTag(kiINDEX)
      or IsTag(kiKEY)) then
    begin
      Nodes.ItemTypeTag := ParseTag(TokenPtr(CurrentToken)^.KeywordIndex);

      if (not Error) then
        Nodes.Ident := ParseKeyIdent();
    end
    else if (IsTag(kiPARTITION)) then
    begin
      Nodes.ItemTypeTag := ParseTag(kiPARTITION);

      if (not Error) then
        Nodes.Ident := ParseList(False, ParsePartitionIdent);
    end
    else if (IsTag(kiPRIMARY, kiKEY)) then
      Nodes.ItemTypeTag := ParseTag(kiPRIMARY, kiKEY)
    else
    begin
      if (IsTag(kiCOLUMN)) then
        Nodes.ItemTypeTag := ParseTag(kiCOLUMN);

      if (not Error) then
        Nodes.Ident := ParseDbIdent(ditField, False);
    end;

  Result := TAlterTableStmt.TDropObject.Create(Self, Nodes);
end;

function TSQLParser.ParseAlterTableStmtExchangePartition(): TOffset;
var
  Nodes: TAlterTableStmt.TExchangePartition.TNodes;
begin
  FillChar(Nodes, SizeOf(Nodes), 0);

  Nodes.ExchangePartitionTag := ParseTag(kiEXCHANGE, kiPARTITION);

  if (not Error) then
    Nodes.PartitionIdent := ParsePartitionIdent();

  if (not Error) then
    Nodes.WithTableTag := ParseTag(kiWITH, kiTABLE);

  if (not Error) then
    Nodes.TableIdent := ParseTableIdent();

  if (not Error) then
    if (IsTag(kiWITH, kiVALIDATION)) then
      Nodes.WithValidationTag := ParseTag(kiWITH, kiVALIDATION)
    else if (IsTag(kiWITHOUT, kiVALIDATION)) then
      Nodes.WithValidationTag := ParseTag(kiWITHOUT, kiVALIDATION);

  Result := TAlterTableStmt.TExchangePartition.Create(Self, Nodes);
end;

function TSQLParser.ParseAlterTableStmtOrderBy(): TOffset;
var
  Nodes: TAlterTableStmt.TOrderBy.TNodes;
begin
  FillChar(Nodes, SizeOf(Nodes), 0);

  Nodes.OrderByTag := ParseTag(kiORDER, kiBY);

  if (not Error) then
    Nodes.KeyColumnList := ParseList(False, ParseCreateTableStmtKeyColumn);

  Result := TAlterTableStmt.TOrderBy.Create(Self, Nodes);
end;

function TSQLParser.ParseAlterTableStmtReorganizePartition(): TOffset;
var
  Nodes: TAlterTableStmt.TReorganizePartition.TNodes;
begin
  FillChar(Nodes, SizeOf(Nodes), 0);

  Nodes.ReorganizePartitionTag := ParseTag(kiREORGANIZE, kiPARTITION);

  if (not Error) then
    Nodes.IdentList := ParseList(False, ParsePartitionIdent);

  if (not Error) then
    Nodes.IntoTag := ParseTag(kiINTO);

  if (not Error) then
    Nodes.PartitionList := ParseList(True, ParseCreateTableStmtPartition);

  Result := TAlterTableStmt.TReorganizePartition.Create(Self, Nodes);
end;

function TSQLParser.ParseAlterViewStmt(): TOffset;
var
  Nodes: TAlterViewStmt.TNodes;
begin
  FillChar(Nodes, SizeOf(Nodes), 0);

  Nodes.AlterTag := ParseTag(kiALTER);

  if (not Error) then
    if (IsTag(kiALGORITHM)) then
      Nodes.AlgorithmValue := ParseValue(kiALGORITHM, vaYes, WordIndices(kiUNDEFINED, kiMERGE, kiTEMPTABLE));

  if (not Error) then
    if (IsTag(kiDEFINER)) then
      Nodes.DefinerValue := ParseDefinerValue();

  if (not Error) then
    if (IsTag(kiSQL, kiSECURITY, kiDEFINER)) then
      Nodes.SQLSecurityTag := ParseTag(kiSQL, kiSECURITY, kiDEFINER)
    else if (IsTag(kiSQL, kiSECURITY, kiINVOKER)) then
      Nodes.SQLSecurityTag := ParseTag(kiSQL, kiSECURITY, kiINVOKER);

  if (not Error) then
    Nodes.ViewTag := ParseTag(kiVIEW);

  if (not Error) then
    Nodes.Ident := ParseTableIdent();

  if (not Error) then
    if (not EndOfStmt(CurrentToken) and (TokenPtr(CurrentToken)^.TokenType = ttOpenBracket)) then
      Nodes.Columns := ParseList(True, ParseFieldIdent);

  if (not Error) then
    Nodes.AsTag := ParseTag(kiAS);

  if (not Error) then
    Nodes.SelectStmt := ParseSelectStmt(True);

  if (not Error) then
    if (IsTag(kiWITH, kiCASCADED, kiCHECK, kiOPTION)) then
      Nodes.OptionTag := ParseTag(kiWITH, kiCASCADED, kiCHECK, kiOPTION)
    else if (IsTag(kiWITH, kiLOCAL, kiCHECK, kiOPTION)) then
      Nodes.OptionTag := ParseTag(kiWITH, kiLOCAL, kiCHECK, kiOPTION)
    else if (IsTag(kiWITH, kiCHECK, kiOPTION)) then
      Nodes.OptionTag := ParseTag(kiWITH, kiCHECK, kiOPTION);

  Result := TAlterViewStmt.Create(Self, Nodes);
end;

function TSQLParser.ParseBeginLabel(): TOffset;
var
  Nodes: TBeginLabel.TNodes;
begin
  FillChar(Nodes, SizeOf(Nodes), 0);

  Nodes.BeginToken := ApplyCurrentToken(utLabel);
  Nodes.ColonToken := ApplyCurrentToken();

  Result := TBeginLabel.Create(Self, Nodes);
end;

function TSQLParser.ParseBeginStmt(): TOffset;
var
  Nodes: TBeginStmt.TNodes;
begin
  FillChar(Nodes, SizeOf(Nodes), 0);

  if (IsTag(kiBEGIN, kiWORK)) then
    Nodes.BeginTag := ParseTag(kiBEGIN, kiWORK)
  else
    Nodes.BeginTag := ParseTag(kiBEGIN);

  Result := TBeginStmt.Create(Self, Nodes);
end;

function TSQLParser.ParseCallStmt(): TOffset;
var
  Nodes: TCallStmt.TNodes;
begin
  FillChar(Nodes, SizeOf(Nodes), 0);

  Nodes.CallTag := ParseTag(kiCALL);

  if (not Error) then
    Nodes.Ident := ParseProcedureIdent();

  if (not Error and IsSymbol(ttOpenBracket)) then
    Nodes.ParamList := ParseList(True, ParseExpr);

  Result := TCallStmt.Create(Self, Nodes);
end;

function TSQLParser.ParseCaseOp(): TOffset;
var
  Branches: array of TOffset;
  ListNodes: TList.TNodes;
  Nodes: TCaseOp.TNodes;
begin
  FillChar(Nodes, SizeOf(Nodes), 0);

  SetLength(Branches, 0);

  Nodes.CaseTag := ParseTag(kiCASE);

  if (not Error) then
    if (not IsTag(kiWHEN)) then
      Nodes.CompareExpr := ParseExpr();

  if (not Error) then
    repeat
      SetLength(Branches, Length(Branches) + 1);
      Branches[Length(Branches) - 1] := ParseCaseOpBranch();
    until (Error or not IsTag(kiWHEN));

  FillChar(ListNodes, SizeOf(ListNodes), 0);
  Nodes.BranchList := TList.Create(Self, ListNodes, ttUnknown, Length(Branches), Branches);
  SetLength(Branches, 0);

  if (not Error) then
    if (IsTag(kiELSE)) then
    begin
      Nodes.ElseTag := ParseTag(kiELSE);

      if (not Error) then
        Nodes.ElseExpr := ParseExpr();
    end;

  if (not Error) then
    Nodes.EndTag := ParseTag(kiEND);

  Result := TCaseOp.Create(Self, Nodes);
end;

function TSQLParser.ParseCaseOpBranch(): TOffset;
var
  Nodes: TCaseOp.TBranch.TNodes;
begin
  FillChar(Nodes, SizeOf(Nodes), 0);

  Nodes.WhenTag := ParseTag(kiWHEN);

  if (not Error) then
    Nodes.CondExpr := ParseExpr();

  if (not Error) then
    Nodes.ThenTag := ParseTag(kiTHEN);

  if (not Error) then
    Nodes.ResultExpr := ParseExpr();

  Result := TCaseOp.TBranch.Create(Self, Nodes);
end;

function TSQLParser.ParseCaseStmt(): TOffset;
var
  Branches: array of TOffset;
  ListNodes: TList.TNodes;
  Nodes: TCaseStmt.TNodes;
begin
  FillChar(Nodes, SizeOf(Nodes), 0);

  SetLength(Branches, 0);

  Nodes.CaseTag := ParseTag(kiCASE);

  if (not Error and not IsTag(kiWHEN)) then
    Nodes.CompareExpr := ParseExpr();

  while (not Error and IsTag(kiWHEN)) do
  begin
    SetLength(Branches, Length(Branches) + 1);
    Branches[Length(Branches) - 1] := ParseCaseStmtBranch();
  end;

  if (not Error) then
    if (IsTag(kiELSE)) then
    begin
      SetLength(Branches, Length(Branches) + 1);
      Branches[Length(Branches) - 1] := ParseCaseStmtBranch();
    end;

  FillChar(ListNodes, SizeOf(ListNodes), 0);
  Nodes.BranchList := TList.Create(Self, ListNodes, ttUnknown, Length(Branches), Branches);
  SetLength(Branches, 0);

  if (not Error) then
    Nodes.EndTag := ParseTag(kiEND, kiCASE);

  Result := TCaseStmt.Create(Self, Nodes);
end;

function TSQLParser.ParseCaseStmtBranch(): TOffset;
var
  Nodes: TCaseStmt.TBranch.TNodes;
begin
  FillChar(Nodes, SizeOf(Nodes), 0);

  if (IsTag(kiWHEN)) then
  begin
    Nodes.Tag := ParseTag(kiWHEN);

    if (not Error) then
      Nodes.ConditionExpr := ParseExpr();

    if (not Error) then
      Nodes.ThenTag := ParseTag(kiTHEN);
  end
  else if (IsTag(kiELSE)) then
    Nodes.Tag := ParseTag(kiELSE);

  if (not IsTag(kiWHEN)
    and not IsTag(kiELSE)
    and not IsTag(kiEND)) then
    Nodes.StmtList := ParseList(False, ParsePL_SQLStmt, ttSemicolon);

  Result := TCaseStmt.TBranch.Create(Self, Nodes);
end;

function TSQLParser.ParseCastFunc(): TOffset;
var
  Nodes: TCastFunc.TNodes;
begin
  FillChar(Nodes, SizeOf(Nodes), 0);

  Nodes.IdentToken := ApplyCurrentToken(utFunction);

  if (not Error) then
    Nodes.OpenBracket := ParseSymbol(ttOpenBracket);

  if (not Error) then
    Nodes.Expr := ParseExpr();

  if (not Error) then
    Nodes.AsTag := ParseTag(kiAS);

  if (not Error) then
    Nodes.Datatype := ParseDatatype();

  if (not Error) then
    Nodes.CloseBracket := ParseSymbol(ttCloseBracket);

  Result := TCastFunc.Create(Self, Nodes);
end;

function TSQLParser.ParseCharFunc(): TOffset;
var
  Nodes: TCharFunc.TNodes;
begin
  FillChar(Nodes, SizeOf(Nodes), 0);

  Nodes.IdentToken := ApplyCurrentToken(utFunction);

  if (not Error) then
    Nodes.OpenBracket := ParseSymbol(ttOpenBracket);

  if (not Error) then
    Nodes.ValueExpr := ParseList(False, ParseExpr);

  if (not Error and not EndOfStmt(CurrentToken) and (TokenPtr(CurrentToken)^.KeywordIndex = kiUSING)) then
  begin
    Nodes.UsingTag := ParseTag(kiUSING);

    if (not Error) then
      Nodes.CharsetIdent := ParseIdent();
  end;

  if (not Error) then
    Nodes.CloseBracket := ParseSymbol(ttCloseBracket);

  Result := TCharFunc.Create(Self, Nodes);
end;

function TSQLParser.ParseChangeMasterStmt(): TOffset;
var
  Comma: TOffset;
  Found: Boolean;
  ListNodes: TList.TNodes;
  Nodes: TChangeMasterStmt.TNodes;
  OptionNodes: TChangeMasterStmt.TOptionNodes;
  Options: Classes.TList;
begin
  FillChar(Nodes, SizeOf(Nodes), 0);
  FillChar(OptionNodes, SizeOf(OptionNodes), 0);

  Nodes.StmtTag := ParseTag(kiCHANGE, kiMASTER);

  if (not Error) then
    Nodes.ToTag := ParseTag(kiTO);

  Options := Classes.TList.Create();
  Found := True;
  while (not Error and Found and not EndOfStmt(CurrentToken)) do
  begin
    if ((OptionNodes.MasterBindValue = 0) and IsTag(kiMASTER_BIND)) then
    begin
      OptionNodes.MasterBindValue := ParseValue(kiMASTER_BIND, vaYes, ParseString);
      Options.Add(Pointer(OptionNodes.MasterBindValue));
    end
    else if ((OptionNodes.MasterHostValue = 0) and IsTag(kiMASTER_HOST)) then
    begin
      OptionNodes.MasterHostValue := ParseValue(kiMASTER_HOST, vaYes, ParseString);
      Options.Add(Pointer(OptionNodes.MasterHostValue));
    end
    else if ((OptionNodes.MasterUserValue = 0) and IsTag(kiMASTER_USER)) then
    begin
      OptionNodes.MasterUserValue := ParseValue(kiMASTER_USER, vaYes, ParseString);
      Options.Add(Pointer(OptionNodes.MasterUserValue));
    end
    else if ((OptionNodes.MasterPasswordValue = 0) and IsTag(kiMASTER_PASSWORD)) then
    begin
      OptionNodes.MasterPasswordValue := ParseValue(kiMASTER_PASSWORD, vaYes, ParseString);
      Options.Add(Pointer(OptionNodes.MasterPasswordValue));
    end
    else if ((OptionNodes.MasterPortValue = 0) and IsTag(kiMASTER_PORT)) then
    begin
      OptionNodes.MasterPortValue := ParseValue(kiMASTER_PORT, vaYes, ParseInteger);
      Options.Add(Pointer(OptionNodes.MasterPortValue));
    end
    else if ((OptionNodes.MasterConnectRetryValue = 0) and IsTag(kiMASTER_CONNECT_RETRY)) then
    begin
      OptionNodes.MasterConnectRetryValue := ParseValue(kiMASTER_CONNECT_RETRY, vaYes, ParseInteger);
      Options.Add(Pointer(OptionNodes.MasterConnectRetryValue));
    end
    else if ((OptionNodes.MasterLogFileValue = 0) and IsTag(kiMASTER_LOG_FILE)) then
    begin
      OptionNodes.MasterLogFileValue := ParseValue(kiMASTER_LOG_FILE, vaYes, ParseString);
      Options.Add(Pointer(OptionNodes.MasterLogFileValue));
    end
    else if ((OptionNodes.MasterLogPosValue = 0) and IsTag(kiMASTER_LOG_POS)) then
    begin
      OptionNodes.MasterLogPosValue := ParseValue(kiMASTER_LOG_POS, vaYes, ParseInteger);
      Options.Add(Pointer(OptionNodes.MasterLogPosValue));
    end
    else if ((OptionNodes.MasterAutoPositionValue = 0) and IsTag(kiMASTER_AUTO_POSITION)) then
    begin
      OptionNodes.MasterAutoPositionValue := ParseValue(kiMASTER_AUTO_POSITION, vaYes, ParseInteger);
      Options.Add(Pointer(OptionNodes.MasterAutoPositionValue));
    end
    else if ((OptionNodes.RelayLogFileValue = 0) and IsTag(kiRELAY_LOG_FILE)) then
    begin
      OptionNodes.RelayLogFileValue := ParseValue(kiRELAY_LOG_FILE, vaYes, ParseString);
      Options.Add(Pointer(OptionNodes.RelayLogFileValue));
    end
    else if ((OptionNodes.RelayLogPosValue = 0) and IsTag(kiRELAY_LOG_POS)) then
    begin
      OptionNodes.RelayLogPosValue := ParseValue(kiRELAY_LOG_POS, vaYes, ParseInteger);
      Options.Add(Pointer(OptionNodes.RelayLogPosValue));
    end
    else if ((OptionNodes.MasterSSLValue = 0) and IsTag(kiMASTER_SSL)) then
    begin
      OptionNodes.MasterSSLValue := ParseValue(kiMASTER_SSL, vaYes, ParseInteger);
      Options.Add(Pointer(OptionNodes.MasterSSLValue));
    end
    else if ((OptionNodes.MasterSSLCAValue = 0) and IsTag(kiMASTER_SSL_CA)) then
    begin
      OptionNodes.MasterSSLCAValue := ParseValue(kiMASTER_SSL_CA, vaYes, ParseString);
      Options.Add(Pointer(OptionNodes.MasterSSLCAValue));
    end
    else if ((OptionNodes.MasterSSLCaPathValue = 0) and IsTag(kiMASTER_SSL_CAPATH)) then
    begin
      OptionNodes.MasterSSLCaPathValue := ParseValue(kiMASTER_SSL_CAPATH, vaYes, ParseString);
      Options.Add(Pointer(OptionNodes.MasterSSLCaPathValue));
    end
    else if ((OptionNodes.MasterSSLCertValue = 0) and IsTag(kiMASTER_SSL_CERT)) then
    begin
      OptionNodes.MasterSSLCertValue := ParseValue(kiMASTER_SSL_CERT, vaYes, ParseString);
      Options.Add(Pointer(OptionNodes.MasterSSLCertValue));
    end
    else if ((OptionNodes.MasterSSLCRLValue = 0) and IsTag(kiMASTER_SSL_CRL)) then
    begin
      OptionNodes.MasterSSLCRLValue := ParseValue(kiMASTER_SSL_CRL, vaYes, ParseString);
      Options.Add(Pointer(OptionNodes.MasterSSLCRLValue));
    end
    else if ((OptionNodes.MasterSSLCRLPathValue = 0) and IsTag(kiMASTER_SSL_CRLPATH)) then
    begin
      OptionNodes.MasterSSLCRLPathValue := ParseValue(kiMASTER_SSL_CRLPATH, vaYes, ParseString);
      Options.Add(Pointer(OptionNodes.MasterSSLCRLPathValue));
    end
    else if ((OptionNodes.MasterSSLKeyValue = 0) and IsTag(kiMASTER_SSL_KEY)) then
    begin
      OptionNodes.MasterSSLKeyValue := ParseValue(kiMASTER_SSL_KEY, vaYes, ParseString);
      Options.Add(Pointer(OptionNodes.MasterSSLKeyValue));
    end
    else if ((OptionNodes.MasterSSLCipherValue = 0) and IsTag(kiMASTER_SSL_CIPHER)) then
    begin
      OptionNodes.MasterSSLCipherValue := ParseValue(kiMASTER_SSL_CIPHER, vaYes, ParseString);
      Options.Add(Pointer(OptionNodes.MasterSSLCipherValue));
    end
    else if ((OptionNodes.MasterSSLVerifyServerCertValue = 0) and IsTag(kiMASTER_SSL_VERIFY_SERVER_CERT)) then
    begin
      OptionNodes.MasterSSLVerifyServerCertValue := ParseValue(kiMASTER_SSL_VERIFY_SERVER_CERT, vaYes, ParseInteger);
      Options.Add(Pointer(OptionNodes.MasterSSLVerifyServerCertValue));
    end
    else if ((OptionNodes.MasterTLSVersionValue = 0) and IsTag(kiMASTER_TLS_VERSION)) then
    begin
      OptionNodes.MasterTLSVersionValue := ParseValue(kiMASTER_TLS_VERSION, vaYes, ParseString);
      Options.Add(Pointer(OptionNodes.MasterTLSVersionValue));
    end
    else if ((OptionNodes.IgnoreServerIDsValue = 0) and IsTag(kiIGNORE_SERVER_IDS)) then
    begin
      OptionNodes.IgnoreServerIDsValue := ParseValue(kiIGNORE_SERVER_IDS, vaYes, ParseString);
      Options.Add(Pointer(OptionNodes.IgnoreServerIDsValue));
    end
    else
      Found := False;

    if (Found and not EndOfStmt(CurrentToken)) then
    begin
      Found := TokenPtr(CurrentToken)^.TokenType = ttComma;
      if (Found) then
      begin
        Comma := ParseSymbol(ttComma);
        Options.Add(Pointer(Comma));
      end;
    end;
  end;
  FillChar(ListNodes, SizeOf(ListNodes), 0);
  Nodes.OptionList := TList.Create(Self, ListNodes, ttComma, Options.Count, TIntegerArray(Options.List));
  Options.Free();

  if (not Error and IsTag(kiFOR, kiCHANNEL)) then
    Nodes.ChannelOptionValue := ParseValue(WordIndices(kiFOR, kiCHANNEL), vaNo, ParseIdent);

  Result := TChangeMasterStmt.Create(Self, Nodes);
end;

function TSQLParser.ParseCharsetIdent(): TOffset;
begin
  Result := ParseDbIdent(ditCharset);
end;

function TSQLParser.ParseCheckTableStmt(): TOffset;
var
  Nodes: TCheckTableStmt.TNodes;
begin
  FillChar(Nodes, SizeOf(Nodes), 0);

  Nodes.StmtTag := ParseTag(kiCHECK, kiTABLE);

  if (not Error) then
    Nodes.TablesList := ParseList(False, ParseTableIdent);

  if (not Error and not EndOfStmt(CurrentToken)) then
    Nodes.OptionList := ParseList(False, ParseCheckTableStmtOption);

  Result := TCheckTableStmt.Create(Self, Nodes);
end;

function TSQLParser.ParseCheckTableStmtOption(): TOffset;
var
  Nodes: TCheckTableStmt.TOption.TNodes;
begin
  FillChar(Nodes, SizeOf(Nodes), 0);

  if (IsTag(kiFOR, kiUPGRADE)) then
    Nodes.OptionTag := ParseTag(kiFOR, kiUPGRADE)
  else if (IsTag(kiQUICK)) then
    Nodes.OptionTag := ParseTag(kiQUICK)
  else if (IsTag(kiFAST)) then
    Nodes.OptionTag := ParseTag(kiFAST)
  else if (IsTag(kiMEDIUM)) then
    Nodes.OptionTag := ParseTag(kiMEDIUM)
  else if (IsTag(kiEXTENDED)) then
    Nodes.OptionTag := ParseTag(kiEXTENDED)
  else if (IsTag(kiCHANGED)) then
    Nodes.OptionTag := ParseTag(kiCHANGED)
  else
    SetError(PE_UnexpectedToken);

  Result := TCheckTableStmt.TOption.Create(Self, Nodes);
end;

function TSQLParser.ParseChecksumTableStmt(): TOffset;
var
  Nodes: TChecksumTableStmt.TNodes;
begin
  FillChar(Nodes, SizeOf(Nodes), 0);

  Nodes.StmtTag := ParseTag(kiCHECKSUM, kiTABLE);

  if (not Error) then
    Nodes.TablesList := ParseList(False, ParseTableIdent);

  if (not Error) then
    if (IsTag(kiQUICK)) then
      Nodes.OptionTag := ParseTag(kiQUICK)
    else if (IsTag(kiEXTENDED)) then
      Nodes.OptionTag := ParseTag(kiEXTENDED);

  Result := TChecksumTableStmt.Create(Self, Nodes);
end;

function TSQLParser.ParseCloseStmt(): TOffset;
var
  Nodes: TCloseStmt.TNodes;
begin
  FillChar(Nodes, SizeOf(Nodes), 0);

  Nodes.StmtTag := ParseTag(kiCLOSE);

  if (not Error) then
    Nodes.Ident := ParseDbIdent(ditCursor);

  Result := TCloseStmt.Create(Self, Nodes);
end;

function TSQLParser.ParseCollateIdent(): TOffset;
begin
  Result := ParseDbIdent(ditCollation);
end;

function TSQLParser.ParseCommitStmt(): TOffset;
var
  Nodes: TCommitStmt.TNodes;
begin
  FillChar(Nodes, SizeOf(Nodes), 0);

  if (IsTag(kiCOMMIT, kiWORK)) then
    Nodes.StmtTag := ParseTag(kiCOMMIT, kiWORK)
  else
    Nodes.StmtTag := ParseTag(kiCOMMIT);

  if (not Error) then
    if (IsTag(kiAND, kiNO, kiCHAIN)) then
      Nodes.ChainTag := ParseTag(kiAND, kiNO, kiCHAIN)
    else if (IsTag(kiAND, kiCHAIN)) then
      Nodes.ChainTag := ParseTag(kiAND, kiCHAIN);

  if (not Error) then
    if (IsTag(kiNO, kiRELEASE)) then
      Nodes.ReleaseTag := ParseTag(kiNO, kiRELEASE)
    else if (IsTag(kiRELEASE)) then
      Nodes.ReleaseTag := ParseTag(kiRELEASE);

  Result := TCommitStmt.Create(Self, Nodes);
end;

function TSQLParser.ParseCompoundStmt(): TOffset;
var
  Nodes: TCompoundStmt.TNodes;
begin
  FillChar(Nodes, SizeOf(Nodes), 0);

  if (not EndOfStmt(CurrentToken) and (TokenPtr(CurrentToken)^.TokenType = ttIdent)
    and not EndOfStmt(NextToken[1]) and (TokenPtr(NextToken[1])^.TokenType = ttColon)) then
    Nodes.BeginLabel := ParseBeginLabel();

  if (not Error) then
    Nodes.BeginTag := ParseTag(kiBEGIN);

  if (not Error) then
    if (not IsTag(kiEND)) then
      Nodes.StmtList := ParseList(False, ParsePL_SQLStmt, ttSemicolon);

  if (not Error) then
    Nodes.EndTag := ParseTag(kiEND);

  if (not Error
    and not EndOfStmt(CurrentToken) and (TokenPtr(CurrentToken)^.TokenType = ttIdent)
    and (Nodes.BeginLabel > 0) and (NodePtr(Nodes.BeginLabel)^.NodeType = ntBeginLabel)) then
    if ((Nodes.BeginLabel = 0) or (lstrcmpi(PChar(TokenPtr(CurrentToken)^.AsString), PChar(PBeginLabel(NodePtr(Nodes.BeginLabel))^.LabelName)) <> 0)) then
      SetError(PE_UnexpectedToken)
    else
      Nodes.EndLabel := ParseEndLabel();

  Result := TCompoundStmt.Create(Self, Nodes);
end;

function TSQLParser.ParseConvertFunc(): TOffset;
var
  Nodes: TConvertFunc.TNodes;
begin
  FillChar(Nodes, SizeOf(Nodes), 0);

  Nodes.IdentToken := ApplyCurrentToken(utFunction);

  if (not Error) then
    Nodes.OpenBracket := ParseSymbol(ttOpenBracket);

  if (not Error) then
    Nodes.Expr := ParseExpr();

  if (not Error) then
    if (IsSymbol(ttComma)) then
    begin
      Nodes.Comma := ParseSymbol(ttComma);

      if (not Error) then
        Nodes.Datatype := ParseDatatype();
    end
    else if (IsTag(kiUSING)) then
    begin
      Nodes.UsingTag := ParseTag(kiUSING);

      if (not Error) then
        Nodes.CharsetIdent := ParseCharsetIdent();
    end
    else
      SetError(PE_UnexpectedToken);

  if (not Error) then
    Nodes.CloseBracket := ParseSymbol(ttCloseBracket);

  Result := TConvertFunc.Create(Self, Nodes);
end;

function TSQLParser.ParseCreateDatabaseStmt(): TOffset;
var
  Found: Boolean;
  Nodes: TCreateDatabaseStmt.TNodes;
begin
  FillChar(Nodes, SizeOf(Nodes), 0);

  if (IsTag(kiCREATE, kiDATABASE)) then
    Nodes.StmtTag := ParseTag(kiCREATE, kiDATABASE)
  else
    Nodes.StmtTag := ParseTag(kiCREATE, kiSCHEMA);

  if (not Error) then
    if (IsTag(kiIF, kiNOT, kiEXISTS)) then
      Nodes.IfNotExistsTag := ParseTag(kiIF, kiNOT, kiEXISTS);

  if (not Error) then
    Nodes.DatabaseIdent := ParseDatabaseIdent();

  Found := True;
  while (not Error and Found) do
    if ((Nodes.CharacterSetValue = 0) and IsTag(kiCHARACTER, kiSET)) then
      Nodes.CharacterSetValue := ParseValue(WordIndices(kiCHARACTER, kiSET), vaNo, ParseCharsetIdent)
    else if ((Nodes.CharacterSetValue = 0) and IsTag(kiCHARSET)) then
      Nodes.CharacterSetValue := ParseValue(WordIndices(kiCHARSET), vaNo, ParseCharsetIdent)
    else if ((Nodes.CollateValue = 0) and IsTag(kiCOLLATE)) then
      Nodes.CollateValue := ParseValue(kiCOLLATE, vaNo, ParseCollateIdent)
    else if ((Nodes.CollateValue = 0) and IsTag(kiDEFAULT, kiCHARACTER, kiSET)) then
      Nodes.CharacterSetValue := ParseValue(WordIndices(kiDEFAULT, kiCHARACTER, kiSET), vaNo, ParseCharsetIdent)
    else if ((Nodes.CollateValue = 0) and IsTag(kiDEFAULT, kiCHARSET)) then
      Nodes.CharacterSetValue := ParseValue(WordIndices(kiDEFAULT, kiCHARSET), vaNo, ParseIdent)
    else if ((Nodes.CollateValue = 0) and IsTag(kiDEFAULT, kiCOLLATE)) then
      Nodes.CollateValue := ParseValue(WordIndices(kiDEFAULT, kiCOLLATE), vaNo, ParseCollateIdent)
    else
      Found := False;

  Result := TCreateDatabaseStmt.Create(Self, Nodes);
end;

function TSQLParser.ParseCreateEventStmt(): TOffset;
var
  Nodes: TCreateEventStmt.TNodes;
begin
  FillChar(Nodes, SizeOf(Nodes), 0);

  Nodes.CreateTag := ParseTag(kiCREATE);

  if (not Error) then
    if (IsTag(kiDEFINER)) then
      Nodes.DefinerNode := ParseDefinerValue();

  if (not Error) then
    Nodes.EventTag := ParseTag(kiEVENT);

  if (not Error) then
    if (IsTag(kiIF, kiNOT, kiEXISTS)) then
      Nodes.IfNotExistsTag := ParseTag(kiIF, kiNOT, kiEXISTS);

  if (not Error) then
    Nodes.EventIdent := ParseEventIdent();

  if (not Error) then
    Nodes.OnScheduleValue := ParseValue(WordIndices(kiON, kiSCHEDULE), vaNo, ParseSchedule);

  if (not Error) then
    if (IsTag(kiON, kiCOMPLETION, kiNOT, kiPRESERVE)) then
      Nodes.OnCompletitionTag := ParseTag(kiON, kiCOMPLETION, kiNOT, kiPRESERVE)
    else if (IsTag(kiON, kiCOMPLETION, kiPRESERVE)) then
      Nodes.OnCompletitionTag := ParseTag(kiON, kiCOMPLETION, kiPRESERVE);

  if (not Error) then
    if (IsTag(kiENABLE)) then
      Nodes.EnableTag := ParseTag(kiENABLE)
    else if (IsTag(kiDISABLE, kiON, kiSLAVE)) then
      Nodes.EnableTag := ParseTag(kiDISABLE, kiON, kiSLAVE)
    else if (IsTag(kiDISABLE)) then
      Nodes.EnableTag := ParseTag(kiDISABLE);

  if (not Error) then
    if (IsTag(kiCOMMENT)) then
      Nodes.CommentValue := ParseValue(kiCOMMENT, vaNo, ParseString);

  if (not Error) then
    Nodes.DoTag := ParseTag(kiDO);

  if (not Error) then
    Nodes.Body := ParsePL_SQLStmt();

  Result := TCreateEventStmt.Create(Self, Nodes);
end;

function TSQLParser.ParseCreateIndexStmt(): TOffset;
var
  Found: Boolean;
  Nodes: TCreateIndexStmt.TNodes;
begin
  FillChar(Nodes, SizeOf(Nodes), 0);

  Nodes.CreateTag := ParseTag(kiCREATE);

  if (not Error) then
    if (IsTag(kiFULLTEXT, kiINDEX)) then
      Nodes.IndexTag := ParseTag(kiFULLTEXT, kiINDEX)
    else if (IsTag(kiSPATIAL, kiINDEX)) then
      Nodes.IndexTag := ParseTag(kiSPATIAL, kiINDEX)
    else if (IsTag(kiUNIQUE, kiINDEX)) then
      Nodes.IndexTag := ParseTag(kiUNIQUE, kiINDEX)
    else
      Nodes.IndexTag := ParseTag(kiINDEX);

  if (not Error) then
    Nodes.Ident := ParseKeyIdent();

  if (not Error) then
    if (IsTag(kiUSING, kiBTREE)) then
      Nodes.IndexTypeValue := ParseTag(kiUSING, kiBTREE)
    else if (IsTag(kiUSING, kiHASH)) then
      Nodes.IndexTypeValue := ParseTag(kiUSING, kiHASH);

  if (not Error) then
    Nodes.OnTag := ParseTag(kiON);

  if (not Error) then
    Nodes.TableIdent := ParseTableIdent();

  if (not Error) then
    Nodes.KeyColumnList := ParseList(True, ParseCreateTableStmtKeyColumn);

  Found := True;
  while (not Error and Found) do
    if ((Nodes.AlgorithmLockValue = 0) and IsTag(kiALGORITHM)) then
      Nodes.AlgorithmLockValue := ParseValue(kiALGORITHM, vaAuto, WordIndices(kiDEFAULT, kiINPLACE, kiCOPY))
    else if ((Nodes.CommentValue = 0) and IsTag(kiCOMMENT)) then
      Nodes.CommentValue := ParseValue(kiCOMMENT, vaAuto, ParseString)
    else if ((Nodes.KeyBlockSizeValue = 0) and IsTag(kiKEY_BLOCK_SIZE)) then
      Nodes.KeyBlockSizeValue := ParseValue(kiKEY_BLOCK_SIZE, vaAuto, ParseInteger)
    else if ((Nodes.AlgorithmLockValue = 0) and IsTag(kiLOCK)) then
      Nodes.AlgorithmLockValue := ParseValue(kiLOCK, vaAuto, WordIndices(kiDEFAULT, kiNONE, kiSHARED, kiEXCLUSIVE))
    else if ((Nodes.IndexTypeValue = 0) and IsTag(kiUSING, kiBTREE)) then
      Nodes.IndexTypeValue := ParseTag(kiUSING, kiHASH)
    else if ((Nodes.IndexTypeValue = 0) and IsTag(kiUSING, kiHASH)) then
      Nodes.IndexTypeValue := ParseTag(kiUSING, kiHASH)
    else if ((Nodes.ParserValue = 0) and IsTag(kiWITH, kiPARSER)) then
      Nodes.ParserValue := ParseValue(WordIndices(kiWITH, kiPARSER), vaNo, ParseIdent)
    else
      Found := False;

  Result := TCreateIndexStmt.Create(Self, Nodes);
end;

function TSQLParser.ParseCreateRoutineStmt(const ARoutineType: TRoutineType): TOffset;
var
  Found: Boolean;
  Nodes: TCreateRoutineStmt.TNodes;
begin
  FillChar(Nodes, SizeOf(Nodes), 0);

  Nodes.CreateTag := ParseTag(kiCREATE);

  if (not Error) then
    if (IsTag(kiDEFINER)) then
      Nodes.DefinerNode := ParseDefinerValue();

  if (not Error) then
    if (ARoutineType = rtFunction) then
      Nodes.RoutineTag := ParseTag(kiFUNCTION)
    else
      Nodes.RoutineTag := ParseTag(kiPROCEDURE);

  if (not Error) then
    if (ARoutineType = rtFunction) then
      Nodes.Ident := ParseFunctionIdent()
    else
      Nodes.Ident := ParseProcedureIdent();

  if (not Error) then
    Nodes.OpenBracket := ParseSymbol(ttOpenBracket);

  if (not Error) then
    if (not IsSymbol(ttCloseBracket)) then
      if (ARoutineType = rtFunction) then
        Nodes.ParameterList := ParseList(False, ParseFunctionParam)
      else
        Nodes.ParameterList := ParseList(False, ParseProcedureParam);

  if (not Error) then
    Nodes.CloseBracket := ParseSymbol(ttCloseBracket);

  if (not Error and (ARoutineType = rtFunction)) then
    Nodes.Returns := ParseFunctionReturns();

  Found := True;
  while (not Error and Found) do
    if ((Nodes.CommentValue = 0) and IsTag(kiCOMMENT)) then
      Nodes.CommentValue := ParseValue(kiComment, vaNo, ParseString)
    else if ((Nodes.LanguageTag = 0) and IsTag(kiLANGUAGE, kiSQL)) then
      Nodes.LanguageTag := ParseTag(kiLANGUAGE, kiSQL)
    else if ((Nodes.DeterministicTag = 0) and IsTag(kiDETERMINISTIC)) then
      Nodes.DeterministicTag := ParseTag(kiDETERMINISTIC)
    else if ((Nodes.DeterministicTag = 0) and IsTag(kiNOT, kiDETERMINISTIC)) then
      Nodes.DeterministicTag := ParseTag(kiNOT, kiDETERMINISTIC)
    else if ((Nodes.NatureOfDataTag = 0) and IsTag(kiCONTAINS, kiSQL)) then
      Nodes.NatureOfDataTag := ParseTag(kiCONTAINS, kiSQL)
    else if ((Nodes.NatureOfDataTag = 0) and IsTag(kiNO, kiSQL)) then
      Nodes.NatureOfDataTag := ParseTag(kiNO, kiSQL)
    else if ((Nodes.NatureOfDataTag = 0) and IsTag(kiREADS, kiSQL, kiDATA)) then
      Nodes.NatureOfDataTag := ParseTag(kiREADS, kiSQL, kiDATA)
    else if ((Nodes.NatureOfDataTag = 0) and IsTag(kiMODIFIES, kiSQL, kiDATA)) then
      Nodes.NatureOfDataTag := ParseTag(kiMODIFIES, kiSQL, kiDATA)
    else if ((Nodes.SQLSecurityTag = 0) and IsTag(kiSQL, kiSECURITY, kiDEFINER)) then
      Nodes.SQLSecurityTag := ParseTag(kiSQL, kiSECURITY, kiDEFINER)
    else if ((Nodes.SQLSecurityTag = 0) and IsTag(kiSQL, kiSECURITY, kiINVOKER)) then
      Nodes.SQLSecurityTag := ParseTag(kiSQL, kiSECURITY, kiINVOKER)
    else
      Found := False;

  if (not Error) then
    begin
      if (InCreateFunctionStmt) then
        raise Exception.Create(SUnknownError);
      InCreateFunctionStmt := ARoutineType = rtFunction;
      if (InCreateProcedureStmt) then
        raise Exception.Create(SUnknownError);
      InCreateProcedureStmt := ARoutineType = rtProcedure;
      Nodes.Body := ParsePL_SQLStmt();
      InCreateFunctionStmt := False;
      InCreateProcedureStmt := False;
    end;

  Result := TCreateRoutineStmt.Create(Self, ARoutineType, Nodes);
end;

function TSQLParser.ParseCreateServerStmt(): TOffset;
var
  Nodes: TCreateServerStmt.TNodes;
begin
  FillChar(Nodes, SizeOf(Nodes), 0);

  Nodes.StmtTag := ParseTag(kiCREATE, kiSERVER);

  if (not Error) then
    Nodes.Ident := ParseDbIdent(ditServer);

  if (not Error) then
    Nodes.ForeignDataWrapperValue := ParseValue(WordIndices(kiFOREIGN, kiDATA, kiWRAPPER), vaNo, ParseString);

  if (not Error) then
    Nodes.Options.Tag := ParseTag(kiOPTIONS);

  if (not Error) then
    Nodes.Options.List := ParseCreateServerStmtOptionList();

  Result := TCreateServerStmt.Create(Self, Nodes);
end;

function TSQLParser.ParseCreateServerStmtOptionList(): TOffset;
var
  Children: array [0 .. 13 - 1] of TOffset;
  ChildrenIndex: Integer;
  DatabaseFound: Boolean;
  DelimiterFound: Boolean;
  HostFound: Boolean;
  ListNodes: TList.TNodes;
  OwnerFound: Boolean;
  PasswordFound: Boolean;
  PortFound: Boolean;
  SocketFound: Boolean;
  UserFound: Boolean;
begin
  FillChar(ListNodes, SizeOf(ListNodes), 0);

  ListNodes.OpenBracket := ParseSymbol(ttOpenBracket);

  ChildrenIndex := 0;
  if (not Error) then
  begin
    DatabaseFound := False;
    HostFound := False;
    OwnerFound := False;
    PasswordFound := False;
    PortFound := False;
    SocketFound := False;
    UserFound := False;
    repeat
      if (not HostFound and IsTag(kiHOST)) then
      begin
        Children[ChildrenIndex] := ParseValue(kiHOST, vaNo, ParseString);
        HostFound := True;
      end
      else if (not DatabaseFound and IsTag(kiDATABASE)) then
      begin
        Children[ChildrenIndex] := ParseValue(kiDATABASE, vaNo, ParseString);
        DatabaseFound := True;
      end
      else if (not UserFound and IsTag(kiUSER)) then
      begin
        Children[ChildrenIndex] := ParseValue(kiUSER, vaNo, ParseString);
        UserFound := True;
      end
      else if (not PasswordFound and IsTag(kiPASSWORD)) then
      begin
        Children[ChildrenIndex] := ParseValue(kiPASSWORD, vaNo, ParseString);
        PasswordFound := True;
      end
      else if (not SocketFound and IsTag(kiSOCKET)) then
      begin
        Children[ChildrenIndex] := ParseValue(kiSOCKET, vaNo, ParseString);
        SocketFound := True;
      end
      else if (not OwnerFound and IsTag(kiOWNER)) then
      begin
        Children[ChildrenIndex] := ParseValue(kiOWNER, vaNo, ParseString);
        OwnerFound := True;
      end
      else if (not PortFound and IsTag(kiPORT)) then
      begin
        Children[ChildrenIndex] := ParseValue(kiPort, vaNo, ParseInteger);
        PortFound := True;
      end
      else if (EndOfStmt(CurrentToken)) then
        SetError(PE_IncompleteStmt)
      else
        SetError(PE_UnexpectedToken);

      if (not Error) then
        Inc(ChildrenIndex);

      DelimiterFound := IsSymbol(ttComma);
      if (DelimiterFound) then
      begin
        Children[ChildrenIndex] := ParseSymbol(ttComma);
        Inc(ChildrenIndex);
      end;
    until (Error or not DelimiterFound);
  end;

  if (not Error) then
    ListNodes.CloseBracket := ParseSymbol(ttCloseBracket);

  Result := TList.Create(Self, ListNodes, ttComma, ChildrenIndex, Children);
end;

function TSQLParser.ParseCreateStmt(): TOffset;
var
  AlgorithmFound: Boolean;
  DefinerFound: Boolean;
  IndexFound: Boolean;
  Index: Integer;
  OrReplaceFound: Boolean;
  SQLSecurityFound: Boolean;
  TemporaryFound: Boolean;
begin
  Result := 0;
  AlgorithmFound := False;
  DefinerFound := False;
  IndexFound := False;
  OrReplaceFound := False;
  SQLSecurityFound := False;
  TemporaryFound := False;

  Index := 1;

  if (EndOfStmt(NextToken[1])) then
  begin
    CompletionList.AddKeyword(kiOR, kiREPLACE);
    CompletionList.AddKeyword(kiALGORITHM);
    CompletionList.AddKeyword(kiDEFINER);
    CompletionList.AddKeyword(kiUNIQUE, kiINDEX);
    CompletionList.AddKeyword(kiFULLTEXT, kiINDEX);
    CompletionList.AddKeyword(kiSPATIAL, kiINDEX);
    CompletionList.AddKeyword(kiINDEX);
    CompletionList.AddKeyword(kiSQL, kiSECURITY, kiDEFINER);
    CompletionList.AddKeyword(kiSQL, kiSECURITY, kiINVOKER);
  end;

  if (not EndOfStmt(NextToken[Index]) and (TokenPtr(NextToken[Index])^.KeywordIndex = kiOR)) then
  begin
    OrReplaceFound := True;

    Inc(Index); // OR
    if (EndOfStmt(NextToken[Index])) then
      SetError(PE_IncompleteStmt)
    else if (TokenPtr(NextToken[Index])^.KeywordIndex <> kiREPLACE) then
      SetError(PE_UnexpectedToken, NextToken[Index])
    else
      Inc(Index); // REPLACE
  end;

  if (not Error and not EndOfStmt(NextToken[Index]) and (TokenPtr(NextToken[Index])^.KeywordIndex = kiALGORITHM)) then
  begin
    AlgorithmFound := True;

    Inc(Index); // ALGORITHM
    if (EndOfStmt(NextToken[Index])) then
      SetError(PE_IncompleteStmt)
    else if (TokenPtr(NextToken[Index])^.OperatorType <> otEqual) then
      SetError(PE_UnexpectedToken, NextToken[Index])
    else
    begin
      Inc(Index); // =

      if (EndOfStmt(NextToken[Index])) then
        SetError(PE_IncompleteStmt)
      else if ((TokenPtr(NextToken[Index])^.KeywordIndex <> kiUNDEFINED)
        and (TokenPtr(NextToken[Index])^.KeywordIndex <> kiMERGE)
        and (TokenPtr(NextToken[Index])^.KeywordIndex <> kiTEMPTABLE)) then
        SetError(PE_UnexpectedToken, NextToken[Index])
      else
        Inc(Index); // UNDEFIND or MERGE or TEMPTABLE
    end;
  end;

  if (not Error and not EndOfStmt(NextToken[Index]) and (TokenPtr(NextToken[Index])^.KeywordIndex = kiDEFINER)) then
  begin
    DefinerFound := True;

    Inc(Index); // DEFINER
    if (EndOfStmt(NextToken[Index])) then
      SetError(PE_IncompleteStmt)
    else if (TokenPtr(NextToken[Index])^.OperatorType <> otEqual) then
      SetError(PE_UnexpectedToken, NextToken[Index])
    else
    begin
      Inc(Index); // =

      if (EndOfStmt(NextToken[Index])) then
        SetError(PE_IncompleteStmt)
      else if (TokenPtr(NextToken[Index])^.KeywordIndex = kiCURRENT_USER) then
        if (((NextToken[Index + 1] = 0) or (TokenPtr(NextToken[Index + 1])^.TokenType <> ttOpenBracket))
          and ((NextToken[Index + 2] = 0) or (TokenPtr(NextToken[Index + 2])^.TokenType <> ttCloseBracket))) then
          Inc(Index) // CURRENT_USER
        else
          Inc(Index, 3)  // CURRENT_USER()
      else
      begin
        Inc(Index); // Username

        if (not Error and not EndOfStmt(NextToken[Index]) and (TokenPtr(NextToken[Index])^.TokenType = ttAt)) then
        begin
          Inc(Index); // @
          if (not Error and not EndOfStmt(NextToken[Index])) then
            Inc(Index); // Servername
        end;
      end;
    end;
  end;

  if (not Error and not EndOfStmt(NextToken[Index])
    and ((TokenPtr(NextToken[Index])^.KeywordIndex = kiUNIQUE) or (TokenPtr(NextToken[Index])^.KeywordIndex = kiFULLTEXT) or (TokenPtr(NextToken[Index])^.KeywordIndex = kiSPATIAL))) then
  begin
    IndexFound := True;

    Inc(Index); // UNIQUE or FULLTEXT or SPATIAL
  end;

  if (not Error and not EndOfStmt(NextToken[Index]) and (TokenPtr(NextToken[Index])^.KeywordIndex = kiSQL)) then
  begin
    SQLSecurityFound := True;

    Inc(Index); // SQL
    if (EndOfStmt(NextToken[Index])) then
    begin
      CompletionList.AddKeyword(kiSECURITY, kiDEFINER);
      CompletionList.AddKeyword(kiSECURITY, kiINVOKER);
      SetError(PE_IncompleteStmt);
    end
    else if (TokenPtr(NextToken[Index])^.KeywordIndex <> kiSECURITY) then
      SetError(PE_UnexpectedToken, NextToken[Index])
    else
    begin
      Inc(Index); // SECURITY

      if (EndOfStmt(NextToken[Index])) then
      begin
        CompletionList.AddKeyword(kiDEFINER);
        CompletionList.AddKeyword(kiINVOKER);
        SetError(PE_IncompleteStmt);
      end
      else if ((TokenPtr(NextToken[Index])^.KeywordIndex <> kiDEFINER)
        and (TokenPtr(NextToken[Index])^.KeywordIndex <> kiINVOKER)) then
        SetError(PE_UnexpectedToken, NextToken[Index])
      else
        Inc(Index); // DEFINIER or INVOKER
    end;
  end;

  if (not Error and not EndOfStmt(NextToken[Index]) and (TokenPtr(NextToken[Index])^.KeywordIndex = kiTEMPORARY)) then
  begin
    TemporaryFound := True;

    Inc(Index); // TEMPORARY
  end;

  if (not Error) then
    if (EndOfStmt(NextToken[Index])) then
    begin
      if (not AlgorithmFound and not DefinerFound and not IndexFound and not OrReplaceFound and not SQLSecurityFound and not TemporaryFound) then
        CompletionList.AddKeyword(kiDATABASE);
      if (not AlgorithmFound and not IndexFound and not OrReplaceFound and not SQLSecurityFound and not TemporaryFound) then
        CompletionList.AddKeyword(kiEVENT);
      if (not AlgorithmFound and not IndexFound and not OrReplaceFound and not SQLSecurityFound and not TemporaryFound) then
        CompletionList.AddKeyword(kiFUNCTION);
      if (not AlgorithmFound and not DefinerFound and not OrReplaceFound and not SQLSecurityFound and not TemporaryFound) then
        CompletionList.AddKeyword(kiINDEX);
      if (not AlgorithmFound and not IndexFound and not OrReplaceFound and not SQLSecurityFound and not TemporaryFound) then
        CompletionList.AddKeyword(kiPROCEDURE);
      if (not AlgorithmFound and not DefinerFound and not IndexFound and not OrReplaceFound and not SQLSecurityFound and not TemporaryFound) then
        CompletionList.AddKeyword(kiSCHEMA);
      if (not AlgorithmFound and not DefinerFound and not IndexFound and not OrReplaceFound and not SQLSecurityFound and not TemporaryFound) then
        CompletionList.AddKeyword(kiSERVER);
      if (not AlgorithmFound and not DefinerFound and not IndexFound and not OrReplaceFound and not SQLSecurityFound) then
        CompletionList.AddKeyword(kiTABLE);
      if (not AlgorithmFound and not DefinerFound and not IndexFound and not OrReplaceFound and not SQLSecurityFound and not TemporaryFound) then
        CompletionList.AddKeyword(kiTABLESPACE);
      if (not AlgorithmFound and not IndexFound and not OrReplaceFound and not SQLSecurityFound and not TemporaryFound) then
        CompletionList.AddKeyword(kiTRIGGER);
      if (not AlgorithmFound and not DefinerFound and not IndexFound and not OrReplaceFound and not SQLSecurityFound and not TemporaryFound) then
        CompletionList.AddKeyword(kiUSER);
      if (not IndexFound and not TemporaryFound) then
        CompletionList.AddKeyword(kiVIEW);

      SetError(PE_IncompleteStmt);
    end
    else if (not AlgorithmFound and not DefinerFound and not IndexFound and not OrReplaceFound and not SQLSecurityFound and not TemporaryFound
      and (TokenPtr(NextToken[Index])^.KeywordIndex = kiDATABASE)) then
      Result := ParseCreateDatabaseStmt()
    else if (not AlgorithmFound and not IndexFound and not OrReplaceFound and not SQLSecurityFound and not TemporaryFound
      and (TokenPtr(NextToken[Index])^.KeywordIndex = kiEVENT)) then
      Result := ParseCreateEventStmt()
    else if (not AlgorithmFound and not IndexFound and not OrReplaceFound and not SQLSecurityFound and not TemporaryFound
      and (TokenPtr(NextToken[Index])^.KeywordIndex = kiFUNCTION)) then
      Result := ParseCreateRoutineStmt(rtFunction)
    else if (not AlgorithmFound and not DefinerFound and not OrReplaceFound and not SQLSecurityFound and not TemporaryFound
      and (TokenPtr(NextToken[Index])^.KeywordIndex = kiINDEX)) then
      Result := ParseCreateIndexStmt()
    else if (not AlgorithmFound and not IndexFound and not OrReplaceFound and not SQLSecurityFound and not TemporaryFound
      and (TokenPtr(NextToken[Index])^.KeywordIndex = kiPROCEDURE)) then
      Result := ParseCreateRoutineStmt(rtProcedure)
    else if (not AlgorithmFound and not DefinerFound and not IndexFound and not OrReplaceFound and not SQLSecurityFound and not TemporaryFound
      and (TokenPtr(NextToken[Index])^.KeywordIndex = kiSCHEMA)) then
      Result := ParseCreateDatabaseStmt()
    else if (not AlgorithmFound and not DefinerFound and not IndexFound and not OrReplaceFound and not SQLSecurityFound and not TemporaryFound
      and (TokenPtr(NextToken[Index])^.KeywordIndex = kiSERVER)) then
      Result := ParseCreateServerStmt()
    else if (not AlgorithmFound and not DefinerFound and not IndexFound and not OrReplaceFound and not SQLSecurityFound
      and (TokenPtr(NextToken[Index])^.KeywordIndex = kiTABLE)) then
      Result := ParseCreateTableStmt()
    else if (not AlgorithmFound and not DefinerFound and not IndexFound and not OrReplaceFound and not SQLSecurityFound and not TemporaryFound
      and (TokenPtr(NextToken[Index])^.KeywordIndex = kiTABLESPACE)) then
      Result := ParseCreateTablespaceStmt()
    else if (not AlgorithmFound and not IndexFound and not OrReplaceFound and not SQLSecurityFound and not TemporaryFound
      and (TokenPtr(NextToken[Index])^.KeywordIndex = kiTRIGGER)) then
      Result := ParseCreateTriggerStmt()
    else if (not AlgorithmFound and not DefinerFound and not IndexFound and not OrReplaceFound and not SQLSecurityFound and not TemporaryFound
      and (TokenPtr(NextToken[Index])^.KeywordIndex = kiUSER)) then
      Result := ParseCreateUserStmt(False)
    else if (not IndexFound and not TemporaryFound
      and (TokenPtr(NextToken[Index])^.KeywordIndex = kiVIEW)) then
      Result := ParseCreateViewStmt()
    else
    begin
      SetError(PE_UnexpectedToken, NextToken[Index]);
      Result := ParseUnknownStmt();
    end;
end;

function TSQLParser.ParseCreateTablespaceStmt(): TOffset;
var
  Nodes: TCreateTablespaceStmt.TNodes;
begin
  FillChar(Nodes, SizeOf(Nodes), 0);

  Nodes.StmtTag := ParseTag(kiCREATE, kiTABLESPACE);

  if (not Error) then
    Nodes.Ident := ParseIdent();

  if (not Error) then
    Nodes.AddDatafileValue := ParseValue(WordIndices(kiADD, kiDATAFILE), vaNo, ParseString);

  if (not Error) then
    if (IsTag(kiFILE_BLOCK_SIZE)) then
      Nodes.FileBlockSizeValue := ParseValue(kiFILE_BLOCK_SIZE, vaAuto, ParseInteger);

  if (not Error) then
    if (IsTag(kiENGINE)) then
      Nodes.EngineValue := ParseValue(kiENGINE, vaAuto, ParseEngineIdent);

  Result := TCreateTablespaceStmt.Create(Self, Nodes);
end;

function TSQLParser.ParseCreateTableStmt(): TOffset;
type
  TPartitionType = (ptUnknown, ptHash, ptKey, ptRANGE, ptList);
var
  Found: Boolean;
  Nodes: TCreateTableStmt.TNodes;
  PartitionType: TPartitionType;
  SubPartitionType: TPartitionType;
begin
  FillChar(Nodes, SizeOf(Nodes), 0);

  Nodes.CreateTag := ParseTag(kiCREATE);

  if (not Error) then
    if (IsTag(kiTEMPORARY)) then
      Nodes.TemporaryTag := ParseTag(kiTEMPORARY);

  if (not Error) then
    Nodes.TableTag := ParseTag(kiTABLE);

  if (not Error) then
    if (IsTag(kiIF)) then
      Nodes.IfNotExistsTag := ParseTag(kiIF, kiNOT, kiEXISTS);

  if (not Error) then
    Nodes.TableIdent := ParseTableIdent();

  if (not Error) then
    if (IsTag(kiLIKE)) then
    begin
      Nodes.LikeTag := ParseTag(kiLIKE);

      if (not Error) then
        Nodes.LikeTableIdent := ParseTableIdent();
    end
    else if (IsSymbol(ttOpenBracket)
      and not EndOfStmt(NextToken[1]) and (TokenPtr(NextToken[1])^.KeywordIndex = kiLIKE)) then
    begin
      Nodes.OpenBracket := ParseSymbol(ttOpenBracket);

      if (not Error) then
        Nodes.LikeTag := ParseTag(kiLIKE);

      if (not Error) then
        Nodes.LikeTableIdent := ParseTableIdent();

      if (not Error) then
        Nodes.CloseBracket := ParseSymbol(ttCloseBracket);
    end
    else if (IsSymbol(ttOpenBracket)
      and (EndOfStmt(NextToken[1]) or (TokenPtr(NextToken[1])^.KeywordIndex <> kiSELECT))) then
    begin
      Nodes.OpenBracket := ParseSymbol(ttOpenBracket);

      if (not Error) then
        Nodes.DefinitionList := ParseList(False, ParseCreateTableStmtDefinition);

      if (not Error) then
        Nodes.CloseBracket := ParseSymbol(ttCloseBracket);
    end;

  if (not Error and (Nodes.LikeTag = 0)) then
  begin
    Found := True;
    while (not Error and Found) do
      if ((Nodes.TableOptionsNodes.AutoIncrementValue = 0) and IsTag(kiAUTO_INCREMENT)) then
        Nodes.TableOptionsNodes.AutoIncrementValue := ParseValue(kiAUTO_INCREMENT, vaAuto, ParseInteger)
      else if ((Nodes.TableOptionsNodes.AvgRowLengthValue = 0) and IsTag(kiAVG_ROW_LENGTH)) then
        Nodes.TableOptionsNodes.AvgRowLengthValue := ParseValue(kiAVG_ROW_LENGTH, vaAuto, ParseInteger)
      else if ((Nodes.TableOptionsNodes.CharacterSetValue = 0) and IsTag(kiCHARACTER, kiSET)) then
        Nodes.TableOptionsNodes.CharacterSetValue := ParseValue(WordIndices(kiCHARACTER, kiSET), vaAuto, ParseCharsetIdent)
      else if ((Nodes.TableOptionsNodes.CharacterSetValue = 0) and IsTag(kiCHARSET)) then
        Nodes.TableOptionsNodes.CharacterSetValue := ParseValue(kiCHARSET, vaAuto, ParseIdent)
      else if ((Nodes.TableOptionsNodes.ChecksumValue = 0) and IsTag(kiCHECKSUM)) then
        Nodes.TableOptionsNodes.ChecksumValue := ParseValue(kiCHECKSUM, vaAuto, ParseInteger)
      else if ((Nodes.TableOptionsNodes.CollateValue = 0) and IsTag(kiCOLLATE)) then
        Nodes.TableOptionsNodes.CollateValue := ParseValue(kiCOLLATE, vaAuto, ParseCollateIdent)
      else if ((Nodes.TableOptionsNodes.CharacterSetValue = 0) and IsTag(kiDEFAULT, kiCHARSET)) then
        Nodes.TableOptionsNodes.CharacterSetValue := ParseValue(WordIndices(kiDEFAULT, kiCHARSET), vaAuto, ParseIdent)
      else if ((Nodes.TableOptionsNodes.CharacterSetValue = 0) and IsTag(kiDEFAULT, kiCHARACTER, kiSET)) then
        Nodes.TableOptionsNodes.CharacterSetValue := ParseValue(WordIndices(kiDEFAULT, kiCHARACTER, kiSET), vaAuto, ParseCharsetIdent)
      else if ((Nodes.TableOptionsNodes.CollateValue = 0) and IsTag(kiDEFAULT, kiCOLLATE)) then
        Nodes.TableOptionsNodes.CollateValue := ParseValue(WordIndices(kiDEFAULT, kiCOLLATE), vaAuto, ParseCollateIdent)
      else if ((Nodes.TableOptionsNodes.CommentValue = 0) and IsTag(kiCOMMENT)) then
        Nodes.TableOptionsNodes.CommentValue := ParseValue(kiCOMMENT, vaAuto, ParseString)
      else if ((Nodes.TableOptionsNodes.CompressValue = 0) and IsTag(kiCOMPRESS)) then
        Nodes.TableOptionsNodes.CompressValue := ParseValue(kiCOMPRESS, vaAuto, ParseString)
      else if ((Nodes.TableOptionsNodes.ConnectionValue = 0) and IsTag(kiCONNECTION)) then
        Nodes.TableOptionsNodes.ConnectionValue := ParseValue(kiCONNECTION, vaAuto, ParseString)
      else if ((Nodes.TableOptionsNodes.DataDirectoryValue = 0) and IsTag(kiDATA, kiDIRECTORY)) then
        Nodes.TableOptionsNodes.DataDirectoryValue := ParseValue(WordIndices(kiDATA, kiDIRECTORY), vaAuto, ParseString)
      else if ((Nodes.TableOptionsNodes.DelayKeyWriteValue = 0) and IsTag(kiDELAY_KEY_WRITE)) then
        Nodes.TableOptionsNodes.DelayKeyWriteValue := ParseValue(kiDELAY_KEY_WRITE, vaAuto, ParseInteger)
      else if ((Nodes.TableOptionsNodes.EngineValue = 0) and IsTag(kiENGINE)) then
        Nodes.TableOptionsNodes.EngineValue := ParseValue(kiENGINE, vaAuto, ParseEngineIdent)
      else if ((Nodes.TableOptionsNodes.IndexDirectoryValue = 0) and IsTag(kiINDEX, kiDIRECTORY)) then
        Nodes.TableOptionsNodes.IndexDirectoryValue := ParseValue(WordIndices(kiINDEX, kiDIRECTORY), vaAuto, ParseString)
      else if ((Nodes.TableOptionsNodes.InsertMethodValue = 0) and IsTag(kiINSERT_METHOD)) then
        Nodes.TableOptionsNodes.InsertMethodValue := ParseValue(kiINSERT_METHOD, vaAuto, WordIndices(kiNO, kiFIRST, kiLAST))
      else if ((Nodes.TableOptionsNodes.KeyBlockSizeValue = 0) and IsTag(kiKEY_BLOCK_SIZE)) then
        Nodes.TableOptionsNodes.KeyBlockSizeValue := ParseValue(kiKEY_BLOCK_SIZE, vaAuto, ParseInteger)
      else if ((Nodes.TableOptionsNodes.MaxRowsValue = 0) and IsTag(kiMAX_ROWS)) then
        Nodes.TableOptionsNodes.MaxRowsValue := ParseValue(kiMAX_ROWS, vaAuto, ParseInteger)
      else if ((Nodes.TableOptionsNodes.MinRowsValue = 0) and IsTag(kiMIN_ROWS)) then
        Nodes.TableOptionsNodes.MinRowsValue := ParseValue(kiMIN_ROWS, vaAuto, ParseInteger)
      else if ((Nodes.TableOptionsNodes.PackKeysValue = 0) and IsTag(kiPACK_KEYS)) then
        Nodes.TableOptionsNodes.PackKeysValue := ParseValue(kiPACK_KEYS, vaAuto, ParseExpr)
      else if ((Nodes.TableOptionsNodes.PageChecksumValue = 0) and IsTag(kiPAGE_CHECKSUM)) then
        Nodes.TableOptionsNodes.PageChecksumValue := ParseValue(kiPAGE_CHECKSUM, vaAuto, ParseInteger)
      else if ((Nodes.TableOptionsNodes.PasswordValue = 0) and IsTag(kiPASSWORD)) then
        Nodes.TableOptionsNodes.PasswordValue := ParseValue(kiPASSWORD, vaAuto, ParseString)
      else if ((Nodes.TableOptionsNodes.RowFormatValue = 0) and IsTag(kiROW_FORMAT)) then
        Nodes.TableOptionsNodes.RowFormatValue := ParseValue(kiROW_FORMAT, vaAuto, ParseIdent)
      else if ((Nodes.TableOptionsNodes.StatsAutoRecalcValue = 0) and IsTag(kiSTATS_AUTO_RECALC, kiDEFAULT)) then
        Nodes.TableOptionsNodes.StatsAutoRecalcValue := ParseValue(kiSTATS_AUTO_RECALC, vaAuto, WordIndices(kiDEFAULT))
      else if ((Nodes.TableOptionsNodes.StatsAutoRecalcValue = 0) and IsTag(kiSTATS_AUTO_RECALC, kiDEFAULT)) then
        Nodes.TableOptionsNodes.StatsAutoRecalcValue := ParseValue(kiSTATS_AUTO_RECALC, vaAuto, ParseInteger)
      else if ((Nodes.TableOptionsNodes.StatsPersistentValue = 0) and IsTag(kiSTATS_PERSISTENT, kiDEFAULT)) then
        Nodes.TableOptionsNodes.StatsPersistentValue := ParseValue(kiSTATS_PERSISTENT, vaAuto, WordIndices(kiDEFAULT))
      else if ((Nodes.TableOptionsNodes.StatsPersistentValue = 0) and IsTag(kiSTATS_PERSISTENT)) then
        Nodes.TableOptionsNodes.StatsPersistentValue := ParseValue(kiSTATS_PERSISTENT, vaAuto, ParseInteger)
      else if ((Nodes.TableOptionsNodes.TransactionalValue = 0) and IsTag(kiTRANSACTIONAL)) then
        Nodes.TableOptionsNodes.TransactionalValue := ParseValue(kiTRANSACTIONAL, vaAuto, ParseInteger)
      else if ((Nodes.TableOptionsNodes.EngineValue = 0) and IsTag(kiTYPE)) then
        Nodes.TableOptionsNodes.EngineValue := ParseValue(kiTYPE, vaAuto, ParseEngineIdent)
      else if ((Nodes.TableOptionsNodes.UnionList = 0) and IsTag(kiUNION)) then
        Nodes.TableOptionsNodes.UnionList := ParseCreateTableStmtUnion()
      else
        Found := False;
  end;

  if (not Error and (Nodes.LikeTag = 0)) then
    if (IsTag(kiPARTITION, kiBY)) then
    begin
      Nodes.PartitionOption.Tag := ParseTag(kiPARTITION, kiBY);

      PartitionType := ptUnknown;
      if (not Error) then
        if (IsTag(kiHASH)) then
        begin
          Nodes.PartitionOption.KindTag := ParseTag(kiHASH);
          PartitionType := ptHash;
        end
        else if (IsTag(kiLINEAR, kiHASH)) then
        begin
          Nodes.PartitionOption.KindTag := ParseTag(kiLINEAR, kiHASH);
          PartitionType := ptHash;
        end
        else if (IsTag(kiLINEAR, kiKEY)) then
        begin
          Nodes.PartitionOption.KindTag := ParseTag(kiLINEAR, kiKEY);
          PartitionType := ptKey;
        end
        else if (IsTag(kiKEY)) then
        begin
          Nodes.PartitionOption.KindTag := ParseTag(kiKEY);
          PartitionType := ptKEY;
        end
        else if (IsTag(kiRANGE)) then
        begin
          Nodes.PartitionOption.KindTag := ParseTag(kiRANGE);
          PartitionType := ptRange;
        end
        else if (IsTag(kiLIST)) then
        begin
          Nodes.PartitionOption.KindTag := ParseTag(kiLIST);
          PartitionType := ptList;
        end
        else if (EndOfStmt(CurrentToken)) then
          SetError(PE_IncompleteStmt)
        else
          SetError(PE_UnexpectedToken);

      if (PartitionType = ptHash) then
      begin
        if (not Error) then
          Nodes.PartitionOption.Expr := ParseSubArea(ParseExpr);
      end
      else if (PartitionType = ptKey) then
      begin
        if (IsTag(kiALGORITHM)) then
          Nodes.PartitionOption.AlgorithmValue := ParseValue(kiALGORITHM, vaAuto, ParseInteger);

        if (not Error) then
          Nodes.PartitionOption.Columns.List := ParseList(True, ParseFieldIdent);
      end
      else if (PartitionType in [ptRange, ptList]) then
      begin
        if (not IsTag(kiCOLUMNS)) then
          Nodes.PartitionOption.Expr := ParseSubArea(ParseExpr)
        else
        begin
          Nodes.PartitionOption.Columns.Tag := ParseTag(kiCOLUMNS);

          if (not Error) then
            Nodes.PartitionOption.Columns.List := ParseList(True, ParseFieldIdent);
        end;
      end;

      if (not Error) then
        if (IsTag(kiPARTITIONS)) then
          Nodes.PartitionOption.Value := ParseValue(kiPARTITIONS, vaNo, ParseInteger);

      if (not Error) then
        if (IsTag(kiSUBPARTITION, kiBY)) then
        begin
          Nodes.PartitionOption.SubPartition.Tag := ParseTag(kiSUBPARTITION, kiBY);

          SubPartitionType := ptUnknown;
          if (not Error) then
            if (IsTag(kiLINEAR, kiHASH)) then
            begin
              Nodes.PartitionOption.SubPartition.KindTag := ParseTag(kiLINEAR, kiHASH);
              SubPartitionType := ptHash;
            end
            else if (IsTag(kiHASH)) then
            begin
              Nodes.PartitionOption.SubPartition.KindTag := ParseTag(kiHASH);
              SubPartitionType := ptHash;
            end
            else if (IsTag(kiLINEAR, kiKEY)) then
            begin
              Nodes.PartitionOption.SubPartition.KindTag := ParseTag(kiLINEAR, kiKEY);
              SubPartitionType := ptKey;
            end
            else if (IsTag(kiKEY)) then
            begin
              Nodes.PartitionOption.SubPartition.KindTag := ParseTag(kiLINEAR, kiKEY);
              SubPartitionType := ptKey;
            end
            else if (EndOfStmt(CurrentToken)) then
              SetError(PE_IncompleteStmt)
            else
              SetError(PE_UnexpectedToken);

          if (not Error) then
            if (SubPartitionType = ptHash) then
            begin
              Nodes.PartitionOption.SubPartition.Expr := ParseExpr();
            end
            else if (SubPartitionType = ptKey) then
            begin
              if (IsTag(kiALGORITHM)) then
                Nodes.PartitionOption.SubPartition.AlgorithmValue := ParseValue(kiALGORITHM, vaAuto, ParseInteger);

              if (not Error) then
                Nodes.PartitionOption.SubPartition.ColumnList := ParseList(True, ParseIdent);
            end;

          if (not Error) then
            if (IsTag(kiSUBPARTITIONS)) then
              Nodes.PartitionOption.SubPartition.Value := ParseValue(kiSUBPARTITIONS, vaNo, ParseInteger);
        end;
    end;

  if (not Error and (Nodes.LikeTag = 0)) then
    if (IsSymbol(ttOpenBracket)
      and (EndOfStmt(NextToken[1]) or (TokenPtr(NextToken[1])^.KeywordIndex <> kiSELECT))) then
      Nodes.PartitionDefinitionList := ParseList(True, ParseCreateTableStmtPartition);

  if (not Error and (Nodes.LikeTag = 0)) then
  begin
    if (IsTag(kiIGNORE)) then
      Nodes.IgnoreReplaceTag := ParseTag(kiIGNORE)
    else if (IsTag(kiREPLACE)) then
      Nodes.IgnoreReplaceTag := ParseTag(kiREPLACE);

    if (not Error) then
      if (IsTag(kiAS)) then
        Nodes.AsTag := ParseTag(kiAS);

    if (not Error) then
      if (EndOfStmt(CurrentToken) and (Nodes.AsTag > 0)) then
        SetError(PE_IncompleteStmt)
      else if (IsTag(kiSELECT)) then
        Nodes.SelectStmt2 := ParseSelectStmt(True)
      else if (IsSymbol(ttOpenBracket)
        and not EndOfStmt(NextToken[1]) and (TokenPtr(NextToken[1])^.KeywordIndex = kiSELECT)) then
        Nodes.SelectStmt2 := ParseSelectStmt(True);
  end;

  Result := TCreateTableStmt.Create(Self, Nodes);
end;

function TSQLParser.ParseCreateTableStmtCheck(): TOffset;
var
  Nodes: TCreateTableStmt.TCheck.TNodes;
begin
  FillChar(Nodes, SizeOf(Nodes), 0);

  Nodes.CheckTag := ParseTag(kiCHECK);

  if (not Error) then
    Nodes.ExprList := ParseList(True, ParseExpr);

  Result := TCreateTableStmt.TCheck.Create(Self, Nodes);
end;

function TSQLParser.ParseCreateTableStmtField(const Add: TCreateTableStmt.TFieldAdd = caNone): TOffset;
var
  Found: Boolean;
  Length: Integer;
  Nodes: TCreateTableStmt.TField.TNodes;
  Text: PChar;
begin
  FillChar(Nodes, SizeOf(Nodes), 0);

  if (not Error) then
    if (Add = caAdd) then
      Nodes.AddTag := ParseTag(kiADD)
    else if (Add = caChange) then
      Nodes.AddTag := ParseTag(kiCHANGE)
    else if (Add = caModify) then
      Nodes.AddTag := ParseTag(kiMODIFY);

  if (not Error and (Add <> caNone)) then
    if (IsTag(kiCOLUMN)) then
      Nodes.ColumnTag := ParseTag(kiCOLUMN);

  if (not Error and (Add in [caChange, caModify])) then
    Nodes.OldNameIdent := ParseFieldIdent();

  if (not Error and (Add in [caNone, caAdd, caChange])) then
    Nodes.NameIdent := ParseFieldIdent();

  if (not Error) then
    Nodes.Datatype := ParseDatatype();

  if (not Error) then
    if (not IsTag(kiGENERATED)
      and not IsTag(kiAS)) then
    begin // real field
      Found := True;
      while (not Error and Found) do
      begin
        if ((Nodes.NullTag = 0) and IsTag(kiNOT, kiNULL)) then
          Nodes.NullTag := ParseTag(kiNOT, kiNULL)
        else if ((Nodes.NullTag = 0) and IsTag(kiNULL)) then
          Nodes.NullTag := ParseTag(kiNULL)
        else if ((Nodes.Real.DefaultValue = 0) and IsTag(kiDEFAULT)) then
        begin
          TokenPtr(NextToken[1])^.GetText(Text, Length);
          if (not EndOfStmt(NextToken[2]) and (TokenPtr(NextToken[2])^.TokenType = ttOpenBracket)
            and ((Length = 17) and (StrLIComp(Text, 'CURRENT_TIMESTAMP', Length) = 0)
              or (Length =  3) and (StrLIComp(Text, 'NOW', Length) = 0)
              or (Length =  9) and (StrLIComp(Text, 'LOCALTIME', Length) = 0)
              or (Length = 14) and (StrLIComp(Text, 'LOCALTIMESTAMP', Length) = 0))) then
            Nodes.Real.DefaultValue := ParseValue(kiDEFAULT, vaNo, ParseCreateTableStmtFieldDefaultFunc)
          else if (not EndOfStmt(NextToken[1])
            and ((Length = 17) and (StrLIComp(Text, 'CURRENT_TIMESTAMP', Length) = 0)
              or (Length =  9) and (StrLIComp(Text, 'LOCALTIME', Length) = 0)
              or (Length = 14) and (StrLIComp(Text, 'LOCALTIMESTAMP', Length) = 0))) then
            Nodes.Real.DefaultValue := ParseValue(kiDEFAULT, vaNo, ApplyCurrentToken)
          else if (not EndOfStmt(NextToken[1]) and (TokenPtr(NextToken[1])^.TokenType in ttStrings)) then
            Nodes.Real.DefaultValue := ParseValue(kiDEFAULT, vaNo, ParseString)
          else if (not EndOfStmt(NextToken[1]) and (TokenPtr(NextToken[1])^.TokenType = ttInteger)) then
            Nodes.Real.DefaultValue := ParseValue(kiDEFAULT, vaNo, ParseInteger)
          else
            Nodes.Real.DefaultValue := ParseValue(kiDEFAULT, vaNo, ParseExpr);
        end
        else if ((Nodes.Real.OnUpdateTag = 0) and IsTag(kiON, kiUPDATE)
          and (not EndOfStmt(NextToken[2]) and ((TokenPtr(NextToken[2])^.KeywordIndex = kiCURRENT_TIMESTAMP) or (TokenPtr(NextToken[2])^.KeywordIndex = kiCURRENT_TIME) or (TokenPtr(NextToken[2])^.KeywordIndex = kiCURRENT_DATE)))) then
          if (EndOfStmt(NextToken[3]) or (TokenPtr(NextToken[3])^.TokenType <> ttOpenBracket)) then
            Nodes.Real.OnUpdateTag := ParseTag(kiON, kiUPDATE, TokenPtr(NextToken[2])^.KeywordIndex)
          else
            Nodes.Real.OnUpdateTag := ParseValue(WordIndices(kiON, kiUPDATE), vaNo, ParseFunctionCall)
        else if ((Nodes.Real.AutoIncrementTag = 0) and IsTag(kiAUTO_INCREMENT)) then
          Nodes.Real.AutoIncrementTag := ParseTag(kiAUTO_INCREMENT)
        else if ((Nodes.KeyTag = 0) and IsTag(kiUNIQUE, kiKEY)) then
          Nodes.KeyTag := ParseTag(kiUNIQUE, kiKEY)
        else if ((Nodes.KeyTag = 0) and IsTag(kiUNIQUE))  then
          Nodes.KeyTag := ParseTag(kiUNIQUE)
        else if ((Nodes.KeyTag = 0) and IsTag(kiPRIMARY, kiKEY)) then
          Nodes.KeyTag := ParseTag(kiPRIMARY, kiKEY)
        else if ((Nodes.KeyTag = 0) and IsTag(kiKEY))  then
          Nodes.KeyTag := ParseTag(kiKEY)
        else if ((Nodes.CommentValue = 0) and IsTag(kiCOMMENT)) then
          Nodes.CommentValue := ParseValue(kiCOMMENT, vaNo, ParseString)
        else if ((Nodes.Real.ColumnFormat = 0) and IsTag(kiCOLUMN_FORMAT)) then
          Nodes.Real.ColumnFormat := ParseValue(kiCOLUMN_FORMAT, vaNo, WordIndices(kiFIXED, kiDYNAMIC, kiDEFAULT))
        else if ((Nodes.Real.StorageTag = 0) and IsTag(kiSTORAGE, kiDISK)) then
          Nodes.Real.StorageTag := ParseTag(kiSTORAGE, kiDISK)
        else if ((Nodes.Real.StorageTag = 0) and IsTag(kiSTORAGE, kiMEMORY)) then
          Nodes.Real.StorageTag := ParseTag(kiSTORAGE, kiMEMORY)
        else if ((Nodes.Real.StorageTag = 0) and IsTag(kiSTORAGE, kiDEFAULT)) then
          Nodes.Real.StorageTag := ParseTag(kiSTORAGE, kiDEFAULT)
        else if ((Nodes.Real.Reference = 0) and IsTag(kiREFERENCES)) then
          Nodes.Real.Reference := ParseCreateTableStmtReference()
        else
          Found := False;
      end;
    end
    else if (IsTag(kiGENERATED, kiALWAYS) or IsTag(kiAS)) then
    begin // virtual field
      if (IsTag(kiGENERATED, kiALWAYS)) then
        Nodes.Virtual.GernatedAlwaysTag := ParseTag(kiGENERATED, kiALWAYS);

      if (not Error) then
        Nodes.Virtual.AsTag := ParseTag(kiAS);

      if (not Error) then
        Nodes.Virtual.Expr := ParseSubArea(ParseExpr);

      if (not Error) then
        if (IsTag(kiVIRTUAL)) then
          Nodes.Virtual.Virtual := ParseTag(kiVIRTUAL)
        else if (IsTag(kiSTORED)) then
          Nodes.Virtual.Virtual := ParseTag(kiSTORED)
        else if (IsTag(kiPERSISTENT)) then
          Nodes.Virtual.Virtual := ParseTag(kiPERSISTENT);

      Found := True;
      while (not Error and Found) do
      begin
        if ((Nodes.NullTag = 0) and IsTag(kiNOT)) then
          Nodes.NullTag := ParseTag(kiNOT, kiNULL)
        else if ((Nodes.NullTag = 0) and IsTag(kiNULL)) then
          Nodes.NullTag := ParseTag(kiNULL)
        else if ((Nodes.KeyTag = 0) and IsTag(kiUNIQUE, kiKEY)) then
          Nodes.KeyTag := ParseTag(kiUNIQUE, kiKEY)
        else if ((Nodes.KeyTag = 0) and IsTag(kiUNIQUE)) then
          Nodes.KeyTag := ParseTag(kiUNIQUE)
        else if ((Nodes.KeyTag = 0) and IsTag(kiPRIMARY, kiKEY)) then
          Nodes.KeyTag := ParseTag(kiPRIMARY, kiKEY)
        else if ((Nodes.KeyTag = 0) and IsTag(kiKEY)) then
          Nodes.KeyTag := ParseTag(kiKEY)
        else if ((Nodes.CommentValue = 0) and (TokenPtr(CurrentToken)^.KeywordIndex = kiCOMMENT)) then
          Nodes.CommentValue := ParseValue(kiCOMMENT, vaNo, ParseString)
        else
          Found := False;
      end;
  end
  else if (EndOfStmt(CurrentToken)) then
    SetError(PE_IncompleteStmt)
  else
    SetError(PE_UnexpectedToken);

  if (not Error and (Add <> caNone)) then
    if (IsTag(kiFIRST)) then
      Nodes.Position := ParseTag(kiFIRST)
    else if (IsTag(kiAFTER)) then
      Nodes.Position := ParseValue(kiAFTER, vaNo, ParseFieldIdent);

  Result := TCreateTableStmt.TField.Create(Self, Nodes);
end;

function TSQLParser.ParseCreateTableStmtFieldDefaultFunc(): TOffset;
var
  Nodes: TSQLParser.TCreateTableStmt.TField.TDefaultFunc.TNodes;
begin
  FillChar(Nodes, SizeOf(Nodes), 0);

  Nodes.IdentToken := ParseIdent();

  if (not Error) then
    Nodes.OpenBracket := ParseSymbol(ttOpenBracket);

  if (not Error) then
    if (not IsSymbol(ttCloseBracket)) then
      Nodes.FSP := ParseInteger();

  if (not Error) then
    Nodes.CloseBracket := ParseSymbol(ttCloseBracket);

  Result := TSQLParser.TCreateTableStmt.TField.TDefaultFunc.Create(Self, Nodes);
end;

function TSQLParser.ParseCreateTableStmtDefinition(): TOffset;
begin
  Result := ParseCreateTableStmtDefinition(False);
end;

function TSQLParser.ParseCreateTableStmtDefinition(const AlterTableStmt: Boolean): TOffset;
var
  Index: Integer;
  SpecificationType: (stField, stKey, stForeignKey, stCheck, stPartition);
begin
  if (not AlterTableStmt) then
    Index := 0
  else
    Index := 1; // "ADD"

  SpecificationType := stField;
  if (EndOfStmt(NextToken[Index])) then
    SetError(PE_IncompleteStmt)
  else if ((TokenPtr(NextToken[Index])^.KeywordIndex = kiFULLTEXT)
    or (TokenPtr(NextToken[Index])^.KeywordIndex = kiINDEX)
    or (TokenPtr(NextToken[Index])^.KeywordIndex = kiKEY)
    or (TokenPtr(NextToken[Index])^.KeywordIndex = kiPRIMARY)
    or (TokenPtr(NextToken[Index])^.KeywordIndex = kiUNIQUE)
    or (TokenPtr(NextToken[Index])^.KeywordIndex = kiSPATIAL)) then
    SpecificationType := stKey
  else if (TokenPtr(NextToken[Index])^.KeywordIndex = kiFOREIGN) then
    SpecificationType := stForeignKey
  else if (TokenPtr(NextToken[Index])^.KeywordIndex = kiCONSTRAINT) then
  begin
    if (not EndOfStmt(NextToken[Index + 1])
      and (TokenPtr(NextToken[Index + 1])^.KeywordIndex <> kiFOREIGN)
      and (TokenPtr(NextToken[Index + 1])^.KeywordIndex <> kiPRIMARY)
      and (TokenPtr(NextToken[Index + 1])^.KeywordIndex <> kiUNIQUE)) then
      Inc(Index); // Symbol identifier
    if (EndOfStmt(NextToken[Index + 1])) then
      SetError(PE_IncompleteStmt)
    else if ((TokenPtr(NextToken[Index + 1])^.KeywordIndex = kiPRIMARY) or (TokenPtr(NextToken[Index + 1])^.KeywordIndex = kiUNIQUE)) then
      SpecificationType := stKey
    else if (TokenPtr(NextToken[Index + 1])^.KeywordIndex = kiFOREIGN) then
      SpecificationType := stForeignKey
    else
      SetError(PE_UnexpectedToken, NextToken[Index + 1]);
  end
  else if (TokenPtr(NextToken[Index])^.KeywordIndex = kiCHECK) then
    SpecificationType := stCheck
  else if (TokenPtr(NextToken[Index])^.KeywordIndex = kiPARTITION) then
    SpecificationType := stPartition;

  Result := 0;
  if (not Error) then
    case (SpecificationType) of
      stField:
        if (not AlterTableStmt) then
          Result := ParseCreateTableStmtField(caNone)
        else
          Result := ParseCreateTableStmtField(caAdd);
      stKey:
        Result := ParseCreateTableStmtKey(AlterTableStmt);
      stForeignKey:
        Result := ParseCreateTableStmtForeignKey(AlterTableStmt);
      stCheck:
        Result := ParseCreateTableStmtCheck();
      stPartition:
        Result := ParseCreateTableStmtPartition(AlterTableStmt);
    end;
end;

function TSQLParser.ParseCreateTableStmtDefinitionPartitionNames(): TOffset;
begin
  if (IsTag(kiALL)) then
    Result := ParseTag(kiALL)
  else
    Result := ParseList(False, ParsePartitionIdent);
end;

function TSQLParser.ParseCreateTableStmtForeignKey(const Add: Boolean = False): TOffset;
var
  Nodes: TCreateTableStmt.TForeignKey.TNodes;
begin
  FillChar(Nodes, SizeOf(Nodes), 0);

  if (Add) then
    Nodes.AddTag := ParseTag(kiADD);

  if (not Error) then
    if (IsTag(kiCONSTRAINT)) then
    begin
      Nodes.ConstraintTag := ParseTag(kiCONSTRAINT);

      if (not Error and not IsTag(kiFOREIGN, kiKEY)) then
        Nodes.SymbolIdent := ParseForeignKeyIdent();
    end;

  if (not Error) then
    Nodes.ForeignKeyTag := ParseTag(kiFOREIGN, kiKEY);

  if (not Error) then
    if (not IsSymbol(ttOpenBracket)) then
      Nodes.NameIdent := ParseDbIdent(ditForeignKey);

  if (not Error) then
    Nodes.ColumnNameList := ParseList(True, ParseFieldIdent);

  if (not Error) then
    Nodes.Reference := ParseCreateTableStmtReference();

  Result := TCreateTableStmt.TForeignKey.Create(Self, Nodes);
end;

function TSQLParser.ParseCreateTableStmtKey(const AlterTableStmt: Boolean): TOffset;
var
  Found: Boolean;
  KeyName: Boolean;
  KeyType: Boolean;
  Nodes: TCreateTableStmt.TKey.TNodes;
begin
  FillChar(Nodes, SizeOf(Nodes), 0);

  if (AlterTableStmt) then
    Nodes.AddTag := ParseTag(kiADD);

  if (not Error) then
    if (IsTag(kiCONSTRAINT)) then
    begin
      Nodes.ConstraintTag := ParseTag(kiCONSTRAINT);

      if (not Error) then
        if (not IsTag(kiPRIMARY, kiKEY)
          and not IsTag(kiUNIQUE, kiINDEX)
          and not IsTag(kiUNIQUE, kiKEY)) then
          if (TokenPtr(CurrentToken)^.TokenType in ttStrings) then
            Nodes.SymbolIdent := ParseString()
          else
            Nodes.SymbolIdent := ParseIdent();
    end;

  KeyName := True; KeyType := False;
  if (not Error) then
    if (IsTag(kiPRIMARY, kiKEY)) then
      begin Nodes.KeyTag := ParseTag(kiPRIMARY, kiKEY); KeyName := False; KeyType := True; end
    else if (IsTag(kiINDEX)) then
      begin Nodes.KeyTag := ParseTag(kiINDEX); KeyType := True; end
    else if (IsTag(kiKEY)) then
      begin Nodes.KeyTag := ParseTag(kiKEY); KeyType := True; end
    else if (IsTag(kiUNIQUE, kiINDEX)) then
      begin Nodes.KeyTag := ParseTag(kiUNIQUE, kiINDEX); KeyType := True; end
    else if (IsTag(kiUNIQUE, kiKEY)) then
      begin Nodes.KeyTag := ParseTag(kiUNIQUE, kiKEY); KeyType := True; end
    else if (IsTag(kiUNIQUE)) then
      begin Nodes.KeyTag := ParseTag(kiUNIQUE); KeyType := True; end
    else if (IsTag(kiFULLTEXT, kiINDEX)) then
      Nodes.KeyTag := ParseTag(kiFULLTEXT, kiINDEX)
    else if (IsTag(kiFULLTEXT, kiKEY)) then
      Nodes.KeyTag := ParseTag(kiFULLTEXT, kiKEY)
    else if (IsTag(kiSPATIAL, kiINDEX)) then
      Nodes.KeyTag := ParseTag(kiSPATIAL, kiINDEX)
    else if (IsTag(kiSPATIAL, kiKEY)) then
      Nodes.KeyTag := ParseTag(kiSPATIAL, kiKEY)
    else if (EndOfStmt(CurrentToken)) then
      SetError(PE_IncompleteStmt)
    else
      SetError(PE_UnexpectedToken);

  if (not Error and KeyName) then
    if (not IsTag(kiUSING) and not IsSymbol(ttOpenBracket)) then
      Nodes.KeyIdent := ParseKeyIdent();

  if (not Error and KeyType) then
    if (IsTag(kiUSING, kiBTREE)) then
      Nodes.IndexTypeTag := ParseTag(kiUSING, kiBTREE)
    else if (IsTag(kiUSING, kiBTREE)) then
      Nodes.IndexTypeTag := ParseTag(kiUSING, kiHASH);

  if (not Error) then
    Nodes.KeyColumnList := ParseList(True, ParseCreateTableStmtKeyColumn);

  Found := True;
  while (not Error and Found) do
    if ((Nodes.CommentValue = 0) and IsTag(kiCOMMENT)) then
      Nodes.CommentValue := ParseValue(kiCOMMENT, vaNo, ParseString)
    else if ((Nodes.KeyBlockSizeValue = 0) and IsTag(kiKEY_BLOCK_SIZE)) then
      Nodes.KeyBlockSizeValue := ParseValue(kiKEY_BLOCK_SIZE, vaAuto, ParseInteger)
    else if ((Nodes.IndexTypeTag = 0) and IsTag(kiUSING, kiBTREE)) then
      Nodes.IndexTypeTag := ParseTag(kiUSING, kiBTREE)
    else if ((Nodes.IndexTypeTag = 0) and IsTag(kiUSING, kiHASH)) then
      Nodes.IndexTypeTag := ParseTag(kiUSING, kiHASH)
    else if ((Nodes.WithParserValue = 0) and IsTag(kiWITH, kiPARSER)) then
      Nodes.WithParserValue := ParseValue(WordIndices(kiWITH, kiPARSER), vaNo, ParseIdent)
    else
      Found := False;

  Result := TCreateTableStmt.TKey.Create(Self, Nodes);
end;

function TSQLParser.ParseCreateTableStmtKeyColumn(): TOffset;
var
  Nodes: TCreateTableStmt.TKeyColumn.TNodes;
begin
  FillChar(Nodes, SizeOf(Nodes), 0);

  Nodes.IdentTag := ParseDbIdent(ditField);

  if (not Error) then
    if (IsSymbol(ttOpenBracket)) then
    begin
      Nodes.OpenBracket := ParseSymbol(ttOpenBracket);

      if (not Error) then
        Nodes.LengthToken := ParseInteger();

      if (not Error) then
        Nodes.CloseBracket := ParseSymbol(ttCloseBracket);
    end;

  if (not Error) then
    if (IsTag(kiASC)) then
      Nodes.SortTag := ParseTag(kiASC)
    else if (IsTag(kiDESC)) then
      Nodes.SortTag := ParseTag(kiDESC);

  Result := TCreateTableStmt.TKeyColumn.Create(Self, Nodes);
end;

function TSQLParser.ParseCreateTableStmtPartition(): TOffset;
begin
  Result := ParseCreateTableStmtPartition(False);
end;

function TSQLParser.ParseCreateTableStmtPartition(const AlterTableStmt: Boolean): TOffset;
var
  Found: Boolean;
  Nodes: TCreateTableStmt.TPartition.TNodes;
  ValueNodes: TValue.TNodes;
begin
  FillChar(Nodes, SizeOf(Nodes), 0);

  if (AlterTableStmt) then
    Nodes.AddTag := ParseTag(kiADD);

  if (not Error) then
    Nodes.PartitionTag := ParseTag(kiPARTITION);

  if (not Error) then
    Nodes.NameIdent := ParsePartitionIdent();

  if (not Error) then
    if (IsTag(kiVALUES)) then
    begin
      Nodes.Values.Tag := ParseTag(kiVALUES);

      if (not Error) then
        if (IsTag(kiLESS, kiTHAN)) then
        begin
          if (not EndOfStmt(NextToken[2]) and (TokenPtr(NextToken[2])^.TokenType = ttOpenBracket)) then
          begin
            FillChar(ValueNodes, SizeOf(ValueNodes), 0);
            ValueNodes.IdentTag := ParseTag(kiLESS, kiTHAN);
            ValueNodes.Expr := ParseList(True, ParseExpr);
            Nodes.Values.Value := TValue.Create(Self, ValueNodes);
          end
          else
            Nodes.Values.Value := ParseValue(WordIndices(kiLESS, kiTHAN), vaNo, kiMAXVALUE)
        end
        else if (IsTag(kiIN)) then
        begin
          FillChar(ValueNodes, SizeOf(ValueNodes), 0);
          ValueNodes.IdentTag := ParseTag(kiIN);
          if (not Error) then
            ValueNodes.Expr := ParseList(True, ParseExpr);
          Nodes.Values.Value := TValue.Create(Self, ValueNodes);
        end
        else if (EndOfStmt(CurrentToken)) then
          SetError(PE_IncompleteStmt)
        else
          SetError(PE_UnexpectedToken);
    end;

  Found := True;
  while (not Error and Found) do
    if ((Nodes.CommentValue = 0) and IsTag(kiCOMMENT)) then
      Nodes.CommentValue := ParseValue(kiCOMMENT, vaAuto, ParseString)
    else if ((Nodes.DataDirectoryValue = 0) and IsTag(kiDATA, kiDIRECTORY)) then
      Nodes.DataDirectoryValue := ParseValue(WordIndices(kiDATA, kiDIRECTORY), vaAuto, ParseString)
    else if ((Nodes.EngineValue = 0) and IsTag(kiSTORAGE, kiENGINE)) then
      Nodes.EngineValue := ParseValue(WordIndices(kiSTORAGE, kiENGINE), vaAuto, ParseIdent)
    else if ((Nodes.EngineValue = 0) and IsTag(kiENGINE)) then
      Nodes.EngineValue := ParseValue(kiENGINE, vaAuto, ParseEngineIdent)
    else if ((Nodes.IndexDirectoryValue = 0) and IsTag(kiINDEX, kiDIRECTORY)) then
      Nodes.IndexDirectoryValue := ParseValue(WordIndices(kiINDEX, kiDIRECTORY), vaAuto, ParseString)
    else if ((Nodes.MaxRowsValue = 0) and IsTag(kiMAX_ROWS)) then
      Nodes.MaxRowsValue := ParseValue(kiMAX_ROWS, vaAuto, ParseInteger)
    else if ((Nodes.MinRowsValue = 0) and IsTag(kiMIN_ROWS)) then
      Nodes.MinRowsValue := ParseValue(kiMIN_ROWS, vaAuto, ParseInteger)
    else if ((Nodes.EngineValue = 0) and IsTag(kiSTORAGE, kiENGINE)) then
      Nodes.EngineValue := ParseValue(WordIndices(kiSTORAGE, kiENGINE), vaAuto, ParseIdent)
    else if ((Nodes.SubPartitionList = 0) and IsSymbol(ttOpenBracket)) then
      Nodes.SubPartitionList := ParseList(True, ParseSubPartition)
    else
      Found := False;

  Result := TCreateTableStmt.TPartition.Create(Self, Nodes);
end;

function TSQLParser.ParseCreateTableStmtReference(): TOffset;
var
  Found: Boolean;
  Nodes: TCreateTableStmt.TReference.TNodes;
begin
  FillChar(Nodes, SizeOf(Nodes), 0);

  if (not Error) then
    Nodes.Tag := ParseTag(kiREFERENCES);

  if (not Error) then
    Nodes.ParentTableIdent := ParseTableIdent();

  if (not Error) then
    Nodes.FieldList := ParseList(True, ParseFieldIdent);

  if (not Error) then
    if (IsTag(kiMATCH, kiFULL)) then
      Nodes.MatchTag := ParseTag(kiMATCH, kiFULL)
    else if (IsTag(kiMATCH, kiPARTIAL)) then
      Nodes.MatchTag := ParseTag(kiMATCH, kiPARTIAL)
    else if (IsTag(kiMATCH, kiSIMPLE)) then
      Nodes.MatchTag := ParseTag(kiMATCH, kiSIMPLE);

  Found := True;
  while (not Error and Found) do
    if ((Nodes.OnDeleteTag = 0) and IsTag(kiON, kiDELETE, kiRESTRICT)) then
      Nodes.OnDeleteTag := ParseTag(kiON, kiDELETE, kiRESTRICT)
    else if ((Nodes.OnDeleteTag = 0) and IsTag(kiON, kiDELETE, kiCASCADE)) then
      Nodes.OnDeleteTag := ParseTag(kiON, kiDELETE, kiCASCADE)
    else if ((Nodes.OnDeleteTag = 0) and IsTag(kiON, kiDELETE, kiSET, kiNULL)) then
      Nodes.OnDeleteTag := ParseTag(kiON, kiDELETE, kiSET, kiNULL)
    else if ((Nodes.OnDeleteTag = 0) and IsTag(kiON, kiDELETE, kiCASCADE)) then
      Nodes.OnDeleteTag := ParseTag(kiON, kiDELETE, kiCASCADE)
    else if ((Nodes.OnDeleteTag = 0) and IsTag(kiON, kiDELETE, kiNO, kiACTION)) then
      Nodes.OnDeleteTag := ParseTag(kiON, kiDELETE, kiNO, kiACTION)
    else if ((Nodes.OnUpdateTag = 0) and IsTag(kiON, kiUPDATE, kiRESTRICT)) then
      Nodes.OnUpdateTag := ParseTag(kiON, kiUPDATE, kiRESTRICT)
    else if ((Nodes.OnUpdateTag = 0) and IsTag(kiON, kiUPDATE, kiCASCADE)) then
      Nodes.OnUpdateTag := ParseTag(kiON, kiUPDATE, kiCASCADE)
    else if ((Nodes.OnUpdateTag = 0) and IsTag(kiON, kiUPDATE, kiSET, kiNULL)) then
      Nodes.OnUpdateTag := ParseTag(kiON, kiUPDATE, kiSET, kiNULL)
    else if ((Nodes.OnUpdateTag = 0) and IsTag(kiON, kiUPDATE, kiCASCADE)) then
      Nodes.OnUpdateTag := ParseTag(kiON, kiUPDATE, kiCASCADE)
    else if ((Nodes.OnUpdateTag = 0) and IsTag(kiON, kiUPDATE, kiNO, kiACTION)) then
      Nodes.OnUpdateTag := ParseTag(kiON, kiUPDATE, kiNO, kiACTION)
    else
      Found := False;

  Result := TCreateTableStmt.TReference.Create(Self, Nodes);
end;

function TSQLParser.ParseCreateTableStmtUnion(): TOffset;
var
  Nodes: TValue.TNodes;
begin
  FillChar(Nodes, SizeOf(Nodes), 0);

  Nodes.IdentTag := ParseTag(kiUNION);

  if (not Error) then
    if (not EndOfStmt(CurrentToken) and (TokenPtr(CurrentToken)^.OperatorType = otEqual)) then
    begin
      TokenPtr(CurrentToken)^.FOperatorType := otAssign;
      Nodes.AssignToken := ApplyCurrentToken();
    end;

  if (not Error) then
    Nodes.Expr := ParseList(True, ParseTableIdent);

  Result := TValue.Create(Self, Nodes);
end;

function TSQLParser.ParseCreateTriggerStmt(): TOffset;
var
  Nodes: TCreateTriggerStmt.TNodes;
begin
  FillChar(Nodes, SizeOf(Nodes), 0);

  Nodes.CreateTag := ParseTag(kiCREATE);

  if (not Error) then
    if (IsTag(kiDEFINER)) then
      Nodes.DefinerNode := ParseDefinerValue();

  if (not Error) then
    Nodes.TriggerTag := ParseTag(kiTRIGGER);

  if (not Error) then
    Nodes.TriggerIdent := ParseTriggerIdent();

  if (not Error) then
    if (IsTag(kiBEFORE)) then
      Nodes.TimeTag := ParseTag(kiBEFORE)
    else if (IsTag(kiAFTER)) then
      Nodes.TimeTag := ParseTag(kiAFTER)
    else if (EndOfStmt(CurrentToken)) then
      SetError(PE_IncompleteStmt)
    else
      SetError(PE_UnexpectedToken);

  if (not Error) then
    if (IsTag(kiINSERT)) then
      Nodes.EventTag := ParseTag(kiINSERT)
    else if (IsTag(kiUPDATE)) then
      Nodes.EventTag := ParseTag(kiUPDATE)
    else if (IsTag(kiDELETE)) then
      Nodes.EventTag := ParseTag(kiDELETE)
    else if (EndOfStmt(CurrentToken)) then
      SetError(PE_IncompleteStmt)
    else
      SetError(PE_UnexpectedToken);

  if (not Error) then
    Nodes.OnTag := ParseTag(kiON);

  if (not Error) then
    Nodes.TableIdentNode := ParseTableIdent();

  if (not Error) then
    Nodes.ForEachRowTag := ParseTag(kiFOR, kiEACH, kiROW);

  if (not Error) then
    if (IsTag(kiFOLLOWS)) then
      Nodes.OrderValue := ParseValue(kiFOLLOWS, vaNo, ParseTriggerIdent)
    else if (IsTag(kiPRECEDES)) then
      Nodes.OrderValue := ParseValue(kiPRECEDES, vaNo, ParseTriggerIdent);

  if (not Error) then
    Nodes.Body := ParsePL_SQLStmt();

  Result := TCreateTriggerStmt.Create(Self, Nodes);
end;

function TSQLParser.ParseCreateUserStmt(const Alter: Boolean): TOffset;
var
  Found: Boolean;
  Nodes: TCreateUserStmt.TNodes;
begin
  FillChar(Nodes, SizeOf(Nodes), 0);

  Nodes.StmtTag := ParseTag(kiCREATE, kiUSER);

  if (not Error) then
    if (IsTag(kiIF, kiNOT, kiEXISTS)) then
      Nodes.IfNotExistsTag := ParseTag(kiIF, kiNOT, kiEXISTS);

  if (not Error) then
    Nodes.UserSpecifications := ParseList(False, ParseGrantStmtUserSpecification);

  if (not Error) then
    if (IsTag(kiWITH)) then
    begin
      Nodes.WithTag := ParseTag(kiWITH);

      Found := True;
      while (not Error and Found) do
        if ((Nodes.MaxQueriesPerHour = 0) and IsTag(kiMAX_QUERIES_PER_HOUR)) then
          Nodes.MaxQueriesPerHour := ParseValue(kiMAX_QUERIES_PER_HOUR, vaNo, ParseInteger)
        else if ((Nodes.MaxUpdatesPerHour = 0) and IsTag(kiMAX_UPDATES_PER_HOUR)) then
          Nodes.MaxUpdatesPerHour := ParseValue(kiMAX_UPDATES_PER_HOUR, vaNo, ParseInteger)
        else if ((Nodes.MaxConnectionsPerHour = 0) and IsTag(kiMAX_CONNECTIONS_PER_HOUR)) then
          Nodes.MaxConnectionsPerHour := ParseValue(kiMAX_CONNECTIONS_PER_HOUR, vaNo, ParseInteger)
        else if ((Nodes.MaxUserConnections = 0) and IsTag(kiMAX_USER_CONNECTIONS)) then
          Nodes.MaxUserConnections := ParseValue(kiMAX_USER_CONNECTIONS, vaNo, ParseInteger)
        else
          Found := False;

      if ((Nodes.MaxQueriesPerHour = 0)
        and (Nodes.MaxUpdatesPerHour = 0)
        and (Nodes.MaxConnectionsPerHour = 0)
        and (Nodes.MaxUserConnections = 0)) then
        if (EndOfStmt(CurrentToken)) then
          SetError(PE_IncompleteStmt)
        else
          SetError(PE_UnexpectedToken);
    end;

  Found := True;
  while (not Error and Found) do
    if ((Nodes.PasswordOption = 0) and IsTag(kiPASSWORD, kiEXPIRE, kiDEFAULT)) then
      Nodes.PasswordOption := ParseTag(kiPASSWORD, kiEXPIRE, kiDEFAULT)
    else if ((Nodes.PasswordOption = 0) and IsTag(kiPASSWORD, kiEXPIRE, kiNEVER)) then
      Nodes.PasswordOption := ParseTag(kiPASSWORD, kiEXPIRE, kiNEVER)
    else if ((Nodes.PasswordOption = 0) and IsTag(kiPASSWORD, kiEXPIRE, kiINTERVAL)) then
      Nodes.PasswordOption := ParseValue(ParseTag(kiPASSWORD, kiEXPIRE, kiINTERVAL), vaNo, ParseIntervalOp)
    else if ((Nodes.PasswordOption = 0) and IsTag(kiPASSWORD, kiEXPIRE)) then
      Nodes.PasswordOption := ParseTag(kiPASSWORD, kiEXPIRE)
    else if ((Nodes.AccountTag = 0) and IsTag(kiACCOUNT, kiLOCK)) then
      Nodes.AccountTag := ParseTag(kiACCOUNT, kiLOCK)
    else if ((Nodes.AccountTag = 0) and IsTag(kiACCOUNT, kiUNLOCK)) then
      Nodes.AccountTag := ParseTag(kiACCOUNT, kiUNLOCK)
    else
      Found := False;

  Result := TCreateUserStmt.Create(Self, Nodes);
end;

function TSQLParser.ParseCreateViewStmt(): TOffset;
var
  Nodes: TCreateViewStmt.TNodes;
begin
  FillChar(Nodes, SizeOf(Nodes), 0);

  Nodes.CreateTag := ParseTag(kiCREATE);

  if (not Error) then
    if (IsTag(kiOR, kiREPLACE)) then
      Nodes.OrReplaceTag := ParseTag(kiOR, kiREPLACE);

  if (not Error) then
    if (IsTag(kiALGORITHM)) then
      Nodes.AlgorithmValue := ParseValue(kiALGORITHM, vaYes, WordIndices(kiUNDEFINED, kiMERGE, kiTEMPTABLE));

  if (not Error) then
    if (IsTag(kiDEFINER)) then
      Nodes.DefinerNode := ParseDefinerValue();

  if (not Error) then
    if (IsTag(kiSQL, kiSECURITY, kiDEFINER)) then
      Nodes.SQLSecurityTag := ParseTag(kiSQL, kiSECURITY, kiDEFINER)
    else if (IsTag(kiSQL, kiSECURITY, kiINVOKER)) then
      Nodes.SQLSecurityTag := ParseTag(kiSQL, kiSECURITY, kiINVOKER);

  if (not Error) then
    Nodes.ViewTag := ParseTag(kiVIEW);

  if (not Error) then
    Nodes.Ident := ParseTableIdent();

  if (not Error) then
    if (IsSymbol(ttOpenBracket)) then
      Nodes.FieldList:= ParseList(True, ParseFieldIdent);

  if (not Error) then
    Nodes.AsTag := ParseTag(kiAS);

  if (not Error) then
    Nodes.SelectStmt := ParseSelectStmt(True);

  if (not Error) then
    if (IsTag(kiWITH, kiCASCADED, kiCHECK, kiOPTION)) then
      Nodes.OptionTag := ParseTag(kiWITH, kiCASCADED, kiCHECK, kiOPTION)
    else if (IsTag(kiWITH, kiLOCAL, kiCHECK, kiOPTION)) then
      Nodes.OptionTag := ParseTag(kiWITH, kiLOCAL, kiCHECK, kiOPTION)
    else if (IsTag(kiWITH, kiCHECK, kiOPTION)) then
      Nodes.OptionTag := ParseTag(kiWITH, kiCHECK, kiOPTION);

  Result := TCreateViewStmt.Create(Self, Nodes);
end;

function TSQLParser.ParseCurrentTimestamp(): TOffset;
var
  Nodes: TCurrentTimestamp.TNodes;
begin
  FillChar(Nodes, SizeOf(Nodes), 0);

  Nodes.CurrentTimestampTag := ParseTag(kiCURRENT_TIMESTAMP);

  if (not Error) then
    if (IsSymbol(ttOpenBracket)) then
    begin
      Nodes.OpenBracket := ParseSymbol(ttOpenBracket);

      if (not Error) then
        Nodes.LengthInteger := ParseInteger();

      if (not Error) then
        Nodes.CloseBracket := ParseSymbol(ttCloseBracket);
    end;

  Result := TCurrentTimestamp.Create(Self, Nodes);
end;

function TSQLParser.ParseDatabaseIdent(): TOffset;
begin
  Result := ParseDbIdent(ditDatabase);
end;

function TSQLParser.ParseDatatype(): TOffset;
var
  DatatypeIndex: Integer;
  DatatypeIndex2: Integer;
  Length: Integer;
  Nodes: TDatatype.TNodes;
  Text: PChar;
begin
  FillChar(Nodes, SizeOf(Nodes), 0);
  DatatypeIndex := -1;

  if (IsTag(kiNATIONAL)) then
    Nodes.NationalToken := ApplyCurrentToken(utDatatype);

  if (not Error) then
    if (EndOfStmt(CurrentToken)) then
      SetError(PE_IncompleteStmt)
    else if (TokenPtr(CurrentToken)^.TokenType <> ttIdent) then
      SetError(PE_UnexpectedToken)
    else
    begin
      TokenPtr(CurrentToken)^.GetText(Text, Length);
      DatatypeIndex := DatatypeList.IndexOf(Text, Length);
      if (DatatypeIndex >= 0) then
        Nodes.DatatypeToken := ApplyCurrentToken(utDatatype)
      else
        SetError(PE_UnexpectedToken);
    end;

  if (not Error
    and ((DatatypeIndex = diSIGNED) or (DatatypeIndex = diUNSIGNED))
    and (TokenPtr(CurrentToken)^.TokenType = ttIdent)) then
  begin
    TokenPtr(CurrentToken)^.GetText(Text, Length);
    DatatypeIndex2 := DatatypeList.IndexOf(Text, Length);
    if ((DatatypeIndex2 = diBIGINT)
      or (DatatypeIndex2 = diINT)
      or (DatatypeIndex2 = diINT4)
      or (DatatypeIndex2 = diINTEGER)
      or (DatatypeIndex2 = diLARGEINT)
      or (DatatypeIndex2 = diLONG)
      or (DatatypeIndex2 = diMEDIUMINT)
      or (DatatypeIndex2 = diSMALLINT)
      or (DatatypeIndex2 = diTINYINT)) then
    begin
      Nodes.SignedToken := Nodes.DatatypeToken;
      Nodes.DatatypeToken := ApplyCurrentToken(utDatatype);
      DatatypeIndex := DatatypeIndex2;
    end;
  end;

  if (not Error
    and (DatatypeIndex = diLONG)
    and (TokenPtr(CurrentToken)^.TokenType = ttIdent)) then
  begin
    TokenPtr(CurrentToken)^.GetText(Text, Length);
    DatatypeIndex2 := DatatypeList.IndexOf(Text, Length);
    if ((DatatypeIndex2 = diVARBINARY)
      or (DatatypeIndex2 = diVARCHAR)) then
    begin
      Nodes.PreDatatypeToken := Nodes.DatatypeToken;
      Nodes.DatatypeToken := ApplyCurrentToken(utDatatype);
      DatatypeIndex := DatatypeIndex2;
    end;
  end;

  if (not Error) then
    if (IsSymbol(ttOpenBracket)
      and ((DatatypeIndex = diBIGINT)
        or (DatatypeIndex = diBINARY)
        or (DatatypeIndex = diBIT)
        or (DatatypeIndex = diBLOB)
        or (DatatypeIndex = diCHAR)
        or (DatatypeIndex = diCHARACTER)
        or (DatatypeIndex = diDATETIME)
        or (DatatypeIndex = diDECIMAL)
        or (DatatypeIndex = diDOUBLE)
        or (DatatypeIndex = diFLOAT)
        or (DatatypeIndex = diINT)
        or (DatatypeIndex = diINTEGER)
        or (DatatypeIndex = diLONGTEXT)
        or (DatatypeIndex = diMEDIUMINT)
        or (DatatypeIndex = diMEDIUMTEXT)
        or (DatatypeIndex = diNCHAR)
        or (DatatypeIndex = diNVARCHAR)
        or (DatatypeIndex = diNUMBER)
        or (DatatypeIndex = diNUMERIC)
        or (DatatypeIndex = diREAL)
        or (DatatypeIndex = diSMALLINT)
        or (DatatypeIndex = diSIGNED)
        or (DatatypeIndex = diTIME)
        or (DatatypeIndex = diTIMESTAMP)
        or (DatatypeIndex = diTINYINT)
        or (DatatypeIndex = diTINYTEXT)
        or (DatatypeIndex = diTEXT)
        or (DatatypeIndex = diUNSIGNED)
        or (DatatypeIndex = diVARBINARY)
        or (DatatypeIndex = diVARCHAR)
        or (DatatypeIndex = diYEAR))) then
    begin
      Nodes.OpenBracket := ParseSymbol(ttOpenBracket);

      if (not Error) then
        Nodes.LengthToken := ParseInteger();

      if (not Error) then
        if (IsSymbol(ttComma)
          and ((DatatypeIndex = diDECIMAL)
            or (DatatypeIndex = diDOUBLE)
            or (DatatypeIndex = diFLOAT)
            or (DatatypeIndex = diNUMERIC)
            or (DatatypeIndex = diREAL))) then
        begin
          Nodes.CommaToken := ParseSymbol(ttComma);

          if (not Error) then
            Nodes.DecimalsToken := ParseInteger();
        end;

      if (not Error) then
        Nodes.CloseBracket := ParseSymbol(ttCloseBracket);
    end;

  if (not Error
    and ((DatatypeIndex = diENUM)
      or (DatatypeIndex = diSET))) then
    Nodes.ItemsList := ParseList(True, ParseString);

  if (not Error) then
    if (IsTag(kiSIGNED)) then
      Nodes.SignedToken := ApplyCurrentToken(utDatatype)
    else if (IsTag(kiUNSIGNED)) then
      Nodes.SignedToken := ApplyCurrentToken(utDatatype);

  if (not Error
    and ((DatatypeIndex = diBIGINT)
      or (DatatypeIndex = diDECIMAL)
      or (DatatypeIndex = diDOUBLE)
      or (DatatypeIndex = diFLOAT)
      or (DatatypeIndex = diINT)
      or (DatatypeIndex = diINTEGER)
      or (DatatypeIndex = diMEDIUMINT)
      or (DatatypeIndex = diNUMBER)
      or (DatatypeIndex = diNUMERIC)
      or (DatatypeIndex = diREAL)
      or (DatatypeIndex = diSMALLINT)
      or (DatatypeIndex = diTINYINT))) then
    if (IsTag(kiZEROFILL)) then
      Nodes.ZerofillTag := ParseTag(kiZEROFILL);

  if ((DatatypeIndex = diCHAR)
    or (DatatypeIndex = diCHARACTER)
    or (DatatypeIndex = diTEXT)
    or (DatatypeIndex = diLONGTEXT)
    or (DatatypeIndex = diMEDIUMTEXT)
    or (DatatypeIndex = diTINYTEXT)
    or (DatatypeIndex = diVARCHAR)) then
  begin
    if (not Error) then
      if (IsTag(kiBINARY)) then
        Nodes.BinaryToken := ApplyCurrentToken(utDatatype);

    if (not Error) then
      if (IsTag(kiASCII)) then
        Nodes.ASCIIToken := ApplyCurrentToken(utDatatype);

    if (not Error) then
      if (IsTag(kiUNICODE)) then
        Nodes.UnicodeToken := ApplyCurrentToken(utDatatype);
  end;

  if (not Error
    and ((DatatypeIndex = diCHAR)
      or (DatatypeIndex = diCHARACTER)
      or (DatatypeIndex = diENUM)
      or (DatatypeIndex = diLONGTEXT)
      or (DatatypeIndex = diMEDIUMTEXT)
      or (DatatypeIndex = diSET)
      or (DatatypeIndex = diTEXT)
      or (DatatypeIndex = diTINYTEXT)
      or (DatatypeIndex = diVARCHAR))) then
  begin
    if (IsTag(kiCHARACTER, kiSET)) then
      Nodes.CharacterSetValue := ParseValue(WordIndices(kiCHARACTER, kiSET), vaNo, ParseCharsetIdent)
    else if (IsTag(kiCHARSET)) then
      Nodes.CharacterSetValue := ParseValue(kiCHARSET, vaNo, ParseIdent);

    if (not Error) then
      if (IsTag(kiCOLLATE)) then
        Nodes.CollateValue := ParseValue(kiCOLLATE, vaNo, ParseCollateIdent);
  end;

  Result := TDatatype.Create(Self, Nodes);
end;

function TSQLParser.ParseDateAddFunc(): TOffset;
var
  Nodes: TDateAddFunc.TNodes;
begin
  FillChar(Nodes, SizeOf(Nodes), 0);

  Nodes.IdentToken := ApplyCurrentToken(utFunction);

  if (not Error) then
    Nodes.OpenBracket := ParseSymbol(ttOpenBracket);

  if (not Error) then
    Nodes.Expr := ParseExpr();

  if (not Error) then
    Nodes.CommaToken := ParseSymbol(ttComma);

  if (not Error) then
    if (EndOfStmt(CurrentToken)) then
    begin
      CompletionList.AddKeyword(kiINTERVAL);
      SetError(PE_IncompleteStmt);
    end
    else if (TokenPtr(CurrentToken)^.KeywordIndex = kiINTERVAL) then
      Nodes.IntervalValue := ParseValue(kiINTERVAL, vaNo, ParseIntervalOp)
    else
      Nodes.Days := ParseExpr();

  if (not Error) then
    Nodes.CloseBracket := ParseSymbol(ttCloseBracket);

  Result := TDateAddFunc.Create(Self, Nodes);
end;

function TSQLParser.ParseDateUnit(): TOffset;
begin
  if (EndOfStmt(CurrentToken)) then
  begin
    CompletionList.AddKeyword(kiMICROSECOND);
    CompletionList.AddKeyword(kiSECOND);
    CompletionList.AddKeyword(kiMINUTE);
    CompletionList.AddKeyword(kiHOUR);
    CompletionList.AddKeyword(kiDAY);
    CompletionList.AddKeyword(kiWEEK);
    CompletionList.AddKeyword(kiMONTH);
    CompletionList.AddKeyword(kiQUARTER);
    CompletionList.AddKeyword(kiYEAR);
    CompletionList.AddKeyword(kiSECOND_MICROSECOND);
    CompletionList.AddKeyword(kiMINUTE_MICROSECOND);
    CompletionList.AddKeyword(kiMINUTE_SECOND);
    CompletionList.AddKeyword(kiHOUR_MICROSECOND);
    CompletionList.AddKeyword(kiHOUR_SECOND);
    CompletionList.AddKeyword(kiHOUR_MINUTE);
    CompletionList.AddKeyword(kiDAY_MICROSECOND);
    CompletionList.AddKeyword(kiDAY_SECOND);
    CompletionList.AddKeyword(kiDAY_MINUTE);
    CompletionList.AddKeyword(kiDAY_HOUR);
    CompletionList.AddKeyword(kiYEAR_MONTH);
    SetError(PE_IncompleteStmt);
    Result := 0;
  end else if ((TokenPtr(CurrentToken)^.KeywordIndex = kiMICROSECOND)
    or (TokenPtr(CurrentToken)^.KeywordIndex = kiSECOND)
    or (TokenPtr(CurrentToken)^.KeywordIndex = kiMINUTE)
    or (TokenPtr(CurrentToken)^.KeywordIndex = kiHOUR)
    or (TokenPtr(CurrentToken)^.KeywordIndex = kiDAY)
    or (TokenPtr(CurrentToken)^.KeywordIndex = kiWEEK)
    or (TokenPtr(CurrentToken)^.KeywordIndex = kiMONTH)
    or (TokenPtr(CurrentToken)^.KeywordIndex = kiQUARTER)
    or (TokenPtr(CurrentToken)^.KeywordIndex = kiYEAR)
    or (TokenPtr(CurrentToken)^.KeywordIndex = kiSECOND_MICROSECOND)
    or (TokenPtr(CurrentToken)^.KeywordIndex = kiMINUTE_MICROSECOND)
    or (TokenPtr(CurrentToken)^.KeywordIndex = kiMINUTE_SECOND)
    or (TokenPtr(CurrentToken)^.KeywordIndex = kiHOUR_MICROSECOND)
    or (TokenPtr(CurrentToken)^.KeywordIndex = kiHOUR_SECOND)
    or (TokenPtr(CurrentToken)^.KeywordIndex = kiHOUR_MINUTE)
    or (TokenPtr(CurrentToken)^.KeywordIndex = kiDAY_MICROSECOND)
    or (TokenPtr(CurrentToken)^.KeywordIndex = kiDAY_SECOND)
    or (TokenPtr(CurrentToken)^.KeywordIndex = kiDAY_MINUTE)
    or (TokenPtr(CurrentToken)^.KeywordIndex = kiDAY_HOUR)
    or (TokenPtr(CurrentToken)^.KeywordIndex = kiYEAR_MONTH)) then
    Result := ApplyCurrentToken()
  else
  begin
    SetError(PE_UnexpectedToken);
    Result := 0;
  end;
end;

function TSQLParser.ParseDbIdent(const ADbIdentType: TDbIdentType; const FullQualified: Boolean = True): TOffset;
var
  Nodes: TDbIdent.TNodes;
begin
  FillChar(Nodes, SizeOf(Nodes), 0);

  if (EndOfStmt(CurrentToken)) then
  begin
    case (ADbIdentType) of
      ditDatabase:
        CompletionList.AddList(ditDatabase);
      ditTable,
      ditProcedure,
      ditFunction,
      ditTrigger,
      ditEvent:
        begin
          CompletionList.AddList(ditDatabase);
          CompletionList.AddList(ADbIdentType);
        end;
      ditKey,
      ditField,
      ditForeignKey:
        begin
          CompletionList.AddList(ditDatabase);
          CompletionList.AddList(ditTable);
          CompletionList.AddList(ADbIdentType);
        end;
      ditServer: ;
      ditEngine,
      ditCharset,
      ditCollation:
        CompletionList.AddList(ADbIdentType);
      else
        raise ERangeError.Create(SArgumentOutOfRange);
    end;
    SetError(PE_IncompleteStmt);
  end
  else if (not (TokenPtr(CurrentToken)^.TokenType in ttIdents + ttStrings)
    and (TokenPtr(CurrentToken)^.OperatorType <> otMulti)) then
    SetError(PE_UnexpectedToken)
  else
  begin
    if (TokenPtr(CurrentToken)^.OperatorType = otMulti) then
      TokenPtr(CurrentToken)^.FTokenType := ttIdent
    else if (ADbIdentType in [ditCharset, ditCollation]) then
      TokenPtr(CurrentToken)^.TokenType := ttIdent;
    TokenPtr(CurrentToken)^.FOperatorType := otUnknown;
    TokenPtr(CurrentToken)^.FUsageType := utDbIdent;
    Nodes.Ident := ApplyCurrentToken(); // Ident
  end;

  if (not Error
    and FullQualified
    and not EndOfStmt(CurrentToken) and (TokenPtr(CurrentToken)^.OperatorType = otDot)) then
    case (ADbIdentType) of
      ditTable,
      ditFunction,
      ditProcedure,
      ditTrigger,
      ditEvent:
        begin
          Nodes.DatabaseIdent := Nodes.Ident;
          Nodes.DatabaseDot := ApplyCurrentToken();

          if (EndOfStmt(CurrentToken)) then
          begin
            CompletionList.AddList(ADbIdentType, SQLUnescape(TokenPtr(Nodes.DatabaseIdent)^.Text));
            SetError(PE_IncompleteStmt);
          end
          else if (TokenPtr(CurrentToken)^.OperatorType = otMulti) then
          begin
            if (TokenPtr(CurrentToken)^.OperatorType = otMulti) then
              TokenPtr(CurrentToken)^.FTokenType := ttIdent;
            TokenPtr(CurrentToken)^.FOperatorType := otUnknown;
            TokenPtr(CurrentToken)^.FUsageType := utDbIdent;
            Nodes.Ident := ApplyCurrentToken();
          end
          else if ((TokenPtr(CurrentToken)^.TokenType <> ttIdent)
            and (not AnsiQuotes or (TokenPtr(CurrentToken)^.TokenType <> ttDQIdent))
            and (AnsiQuotes or (TokenPtr(CurrentToken)^.TokenType <> ttMySQLIdent))) then
            SetError(PE_UnexpectedToken)
          else
            Nodes.Ident := ApplyCurrentToken();
        end;
      ditKey,
      ditField,
      ditForeignKey:
        begin
          Nodes.TableIdent := Nodes.Ident;
          Nodes.TableDot := ApplyCurrentToken();

          if (EndOfStmt(CurrentToken)) then
          begin
            CompletionList.AddList(ADbIdentType, '', SQLUnescape(TokenPtr(Nodes.TableIdent)^.Text));
            SetError(PE_IncompleteStmt);
          end
          else if (TokenPtr(CurrentToken)^.OperatorType = otMulti) then
          begin
            if (TokenPtr(CurrentToken)^.OperatorType = otMulti) then
              TokenPtr(CurrentToken)^.FTokenType := ttIdent;
            TokenPtr(CurrentToken)^.FOperatorType := otUnknown;
            TokenPtr(CurrentToken)^.FUsageType := utDbIdent;
            Nodes.Ident := ApplyCurrentToken();
          end
          else if ((TokenPtr(CurrentToken)^.TokenType <> ttIdent)
            and (not AnsiQuotes or (TokenPtr(CurrentToken)^.TokenType <> ttDQIdent))
            and (AnsiQuotes or (TokenPtr(CurrentToken)^.TokenType <> ttMySQLIdent))) then
            SetError(PE_UnexpectedToken)
          else
            Nodes.Ident := ApplyCurrentToken();

          if (not Error
            and not EndOfStmt(CurrentToken) and (TokenPtr(CurrentToken)^.OperatorType = otDot)) then
          begin
            Nodes.DatabaseIdent := Nodes.TableIdent;
            Nodes.DatabaseDot := Nodes.TableDot;
            Nodes.TableIdent := Nodes.Ident;
            Nodes.TableDot := ApplyCurrentToken();

            if (EndOfStmt(CurrentToken)) then
            begin
              CompletionList.AddList(ADbIdentType, SQLUnescape(TokenPtr(Nodes.DatabaseIdent)^.Text), SQLUnescape(TokenPtr(Nodes.TableIdent)^.Text));
              SetError(PE_IncompleteStmt);
            end
            else if (TokenPtr(CurrentToken)^.OperatorType = otMulti) then
            begin
              if (TokenPtr(CurrentToken)^.OperatorType = otMulti) then
                TokenPtr(CurrentToken)^.FTokenType := ttIdent;
              TokenPtr(CurrentToken)^.FOperatorType := otUnknown;
              TokenPtr(CurrentToken)^.FUsageType := utDbIdent;
              Nodes.Ident := ApplyCurrentToken();
            end
            else if ((TokenPtr(CurrentToken)^.TokenType <> ttIdent)
              and (not AnsiQuotes or (TokenPtr(CurrentToken)^.TokenType <> ttDQIdent))
              and (AnsiQuotes or (TokenPtr(CurrentToken)^.TokenType <> ttMySQLIdent))) then
              SetError(PE_UnexpectedToken)
            else
              Nodes.Ident := ApplyCurrentToken();
          end;
        end;
    end;

  Result := TDbIdent.Create(Self, ADbIdentType, Nodes);
end;

function TSQLParser.ParseDefinerValue(): TOffset;
var
  Nodes: TValue.TNodes;
begin
  FillChar(Nodes, SizeOf(Nodes), 0);

  Nodes.IdentTag := ParseTag(kiDEFINER);

  if (not Error) then
    if (EndOfStmt(CurrentToken)) then
      SetError(PE_IncompleteStmt)
    else if (TokenPtr(CurrentToken)^.OperatorType <> otEqual) then
      SetError(PE_UnexpectedToken)
    else
    begin
      TokenPtr(CurrentToken)^.FOperatorType := otAssign;
      Nodes.AssignToken := ApplyCurrentToken();

      if (not Error) then
        Nodes.Expr := ParseUserIdent();
    end;

  Result := TValue.Create(Self, Nodes);
end;

function TSQLParser.ParseDeallocatePrepareStmt(): TOffset;
var
  Nodes: TDeallocatePrepareStmt.TNodes;
begin
  FillChar(Nodes, SizeOf(Nodes), 0);

  if (IsTag(kiDEALLOCATE, kiPREPARE)) then
    Nodes.StmtTag := ParseTag(kiDEALLOCATE, kiPREPARE)
  else
    Nodes.StmtTag := ParseTag(kiDROP, kiPREPARE);

  if (not Error) then
    Nodes.Ident := ParseVariableIdent();

  Result := TDeallocatePrepareStmt.Create(Self, Nodes);
end;

function TSQLParser.ParseDeclareStmt(): TOffset;
var
  Nodes: TDeclareStmt.TNodes;
begin
  FillChar(Nodes, SizeOf(Nodes), 0);

  Nodes.StmtTag := ParseTag(kiDECLARE);

  if (not Error) then
    Nodes.IdentList := ParseList(False, ParseVariableIdent);

  if (not Error) then
    Nodes.TypeNode := ParseDatatype();

  if (not Error) then
    if (IsTag(kiDEFAULT)) then
      Nodes.DefaultValue := ParseValue(kiDEFAULT, vaNo, ParseExpr);

  Result := TDeclareStmt.Create(Self, Nodes);
end;

function TSQLParser.ParseDeclareConditionStmt(): TOffset;
var
  Nodes: TDeclareConditionStmt.TNodes;
begin
  FillChar(Nodes, SizeOf(Nodes), 0);

  Nodes.StmtTag := ParseTag(kiDECLARE);

  if (not Error) then
    Nodes.Ident := ParseIdent();

  if (not Error) then
    Nodes.ConditionTag := ParseTag(kiCONDITION, kiFOR);

  if (not Error) then
    if (not IsTag(kiSQLSTATE)) then
      Nodes.ErrorCode := ParseInteger()
    else
    begin
      if (IsTag(kiSQLSTATE, kiVALUE)) then
        Nodes.SQLStateTag := ParseTag(kiSQLSTATE, kiVALUE)
      else
        Nodes.SQLStateTag := ParseTag(kiSQLSTATE);

      if (not Error) then
        Nodes.ErrorString := ParseString();
    end
    else if (EndOfStmt(CurrentToken)) then
      SetError(PE_IncompleteStmt)
    else
      SetError(PE_UnexpectedToken);

  Result := TDeclareConditionStmt.Create(Self, Nodes);
end;

function TSQLParser.ParseDeclareCursorStmt(): TOffset;
var
  Nodes: TDeclareCursorStmt.TNodes;
begin
  FillChar(Nodes, SizeOf(Nodes), 0);

  Nodes.StmtTag := ParseTag(kiDECLARE);

  if (not Error) then
    Nodes.Ident := ParseDbIdent(ditCursor);

  if (not Error) then
    Nodes.CursorTag := ParseTag(kiCURSOR, kiFOR);

  if (not Error) then
    Nodes.SelectStmt := ParseSelectStmt(True);

  Result := TDeclareCursorStmt.Create(Self, Nodes);
end;

function TSQLParser.ParseDeclareHandlerStmt(): TOffset;
var
  Nodes: TDeclareHandlerStmt.TNodes;
begin
  FillChar(Nodes, SizeOf(Nodes), 0);

  Nodes.StmtTag := ParseTag(kiDECLARE);

  if (not Error) then
    if (IsTag(kiCONTINUE)) then
      Nodes.ActionTag := ParseTag(kiCONTINUE)
    else if (IsTag(kiEXIT)) then
      Nodes.ActionTag := ParseTag(kiEXIT)
    else if (IsTag(kiUNDO)) then
      Nodes.ActionTag := ParseTag(kiUNDO)
    else if (EndOfStmt(CurrentToken)) then
      SetError(PE_IncompleteStmt)
    else
      SetError(PE_UnexpectedToken);

  if (not Error) then
    Nodes.HandlerTag := ParseTag(kiHANDLER);

  if (not Error) then
    Nodes.ForTag := ParseTag(kiFOR);

  if (not Error) then
    Nodes.ConditionsList := ParseList(False, ParseDeclareHandlerStmtCondition);

  if (not Error) then
    Nodes.Stmt := ParsePL_SQLStmt();

  Result := TDeclareHandlerStmt.Create(Self, Nodes);
end;

function TSQLParser.ParseDeclareHandlerStmtCondition(): TOffset;
begin
  Result := 0;
  if (TokenPtr(CurrentToken)^.TokenType = ttInteger) then
    Result := ParseInteger()
  else if (IsTag(kiSQLSTATE, kiVALUE)) then
    Result := ParseValue(WordIndices(kiSQLSTATE, kiVALUE), vaNo, ParseExpr)
  else if (IsTag(kiSQLSTATE)) then
    Result := ParseValue(kiSQLSTATE, vaNo, ParseExpr)
  else if (TokenPtr(CurrentToken)^.KeywordIndex = kiSQLWARNINGS) then
    Result := ParseTag(kiSQLWARNINGS)
  else if (IsTag(kiNOT, kiFOUND)) then
    Result := ParseTag(kiNOT, kiFOUND)
  else if (IsTag(kiSQLEXCEPTION)) then
    Result := ParseTag(kiSQLEXCEPTION)
  else if (TokenPtr(CurrentToken)^.TokenType in ttIdents) then // Must be in the end, because keywords are in ttIdents
    Result := ParseVariableIdent()
  else if (EndOfStmt(CurrentToken)) then
    SetError(PE_IncompleteStmt)
  else
    SetError(PE_UnexpectedToken);
end;

function TSQLParser.ParseDeleteStmt(): TOffset;
var
  Nodes: TDeleteStmt.TNodes;
  TableCount: Integer;
begin
  FillChar(Nodes, SizeOf(Nodes), 0);
  TableCount := 1;

  Nodes.DeleteTag := ParseTag(kiDELETE);

  if (not Error) then
    if (IsTag(kiLOW_PRIORITY)) then
      Nodes.LowPriorityTag := ParseTag(kiLOW_PRIORITY);

  if (not Error) then
    if (IsTag(kiQUICK)) then
      Nodes.QuickTag := ParseTag(kiQUICK);

  if (not Error) then
    if (IsTag(kiIGNORE)) then
      Nodes.IgnoreTag := ParseTag(kiIGNORE);

  if (not Error) then
    if (IsTag(kiFROM)) then
    begin
      Nodes.From.Tag := ParseTag(kiFROM);

      if (not Error) then
      begin
        Nodes.From.List := ParseList(False, ParseSelectStmtTableFactor);

        if (not Error) then
        begin
          Assert(NodePtr(Nodes.From.List)^.NodeType = ntList);

          TableCount := PList(NodePtr(Nodes.From.List))^.ElementCount;
        end;
      end;

      if (not Error) then
        if ((TableCount = 1) and IsTag(kiPARTITION)) then
        begin
          Nodes.Partition.Tag := ParseTag(kiPARTITION);

          if (not Error) then
            Nodes.Partition.List := ParseList(True, ParsePartitionIdent);
        end
        else if ((TableCount > 1) or IsTag(kiUSING)) then
        begin
          Nodes.Using.Tag := ParseTag(kiUSING);

          if (not Error) then
            Nodes.Using.List := ParseList(False, ParseSelectStmtTableEscapedReference);
        end;
    end
    else
    begin
      Nodes.From.List := ParseList(False, ParseTableIdent);

      if (not Error) then
        Nodes.From.Tag := ParseTag(kiFROM);

      if (not Error) then
        Nodes.TableReferenceList := ParseList(False, ParseSelectStmtTableEscapedReference);
    end;

  if (not Error) then
    if (IsTag(kiWHERE)) then
      Nodes.WhereValue := ParseValue(kiWHERE, vaNo, ParseExpr);

  if (not Error and (TableCount = 1)) then
    if (IsTag(kiORDER)) then
    begin
      Nodes.OrderBy.Tag := ParseTag(kiORDER, kiBY);

      if (not Error) then
        Nodes.OrderBy.Expr := ParseList(False, ParseSelectStmtOrderBy);
    end;

  if (not Error and (TableCount = 1)) then
    if (IsTag(kiLIMIT)) then
    begin
      Nodes.Limit.Tag := ParseTag(kiLIMIT);

      if (not Error) then
        Nodes.Limit.Expr := ParseExpr();
    end;

  Result := TDeleteStmt.Create(Self, Nodes);
end;

function TSQLParser.ParseDoStmt(): TOffset;
var
  Nodes: TDoStmt.TNodes;
begin
  FillChar(Nodes, SizeOf(Nodes), 0);

  Nodes.DoTag := ParseTag(kiDO);

  if (not Error) then
    Nodes.ExprList := ParseList(False, ParseExpr);

  Result := TDoStmt.Create(Self, Nodes);
end;

function TSQLParser.ParseDropDatabaseStmt(): TOffset;
var
  Nodes: TDropDatabaseStmt.TNodes;
begin
  FillChar(Nodes, SizeOf(Nodes), 0);

  if (IsTag(kiDROP, kiDATABASE)) then
    Nodes.StmtTag := ParseTag(kiDROP, kiDATABASE)
  else
    Nodes.StmtTag := ParseTag(kiDROP, kiSCHEMA);

  if (not Error) then
    if (IsTag(kiIF, kiEXISTS)) then
      Nodes.IfExistsTag := ParseTag(kiIF, kiEXISTS);

  if (not Error) then
    Nodes.DatabaseIdent := ParseDatabaseIdent();

  Result := TDropDatabaseStmt.Create(Self, Nodes);
end;

function TSQLParser.ParseDropEventStmt(): TOffset;
var
  Nodes: TDropEventStmt.TNodes;
begin
  FillChar(Nodes, SizeOf(Nodes), 0);

  Nodes.StmtTag := ParseTag(kiDROP, kiEVENT);

  if (not Error and not EndOfStmt(CurrentToken) and (TokenPtr(CurrentToken)^.KeywordIndex = kiIF)) then
    Nodes.IfExistsTag := ParseTag(kiIF, kiEXISTS);

  if (not Error) then
    Nodes.EventIdent := ParseDbIdent(ditEvent);

  Result := TDropEventStmt.Create(Self, Nodes);
end;

function TSQLParser.ParseDropIndexStmt(): TOffset;
var
  Found: Boolean;
  Nodes: TDropIndexStmt.TNodes;
begin
  FillChar(Nodes, SizeOf(Nodes), 0);

  Nodes.StmtTag := ParseTag(kiDROP, kiINDEX);

  if (not Error) then
    Nodes.IndexIdent := ParseKeyIdent();

  if (not Error) then
    Nodes.OnTag := ParseTag(kiON);

  if (not Error) then
    Nodes.TableIdent := ParseTableIdent();

  Found := True;
  while (not Error and Found) do
    if ((Nodes.AlgorithmValue = 0) and IsTag(kiALGORITHM)) then
      Nodes.AlgorithmValue := ParseValue(kiALGORITHM, vaAuto, WordIndices(kiDEFAULT, kiINPLACE, kiCOPY))
    else if ((Nodes.LockValue = 0) and IsTag(kiLOCK)) then
      Nodes.LockValue := ParseValue(kiLOCK, vaAuto, WordIndices(kiDEFAULT, kiNONE, kiSHARED, kiEXCLUSIVE))
    else
      Found := False;

  Result := TDropIndexStmt.Create(Self, Nodes);
end;

function TSQLParser.ParseDropRoutineStmt(const ARoutineType: TRoutineType): TOffset;
var
  Nodes: TDropRoutineStmt.TNodes;
begin
  FillChar(Nodes, SizeOf(Nodes), 0);

  if (ARoutineType = rtFunction) then
    Nodes.StmtTag := ParseTag(kiDROP, kiFUNCTION)
  else
    Nodes.StmtTag := ParseTag(kiDROP, kiPROCEDURE);

  if (not Error) then
    if (IsTag(kiIF, kiEXISTS)) then
      Nodes.IfExistsTag := ParseTag(kiIF, kiEXISTS);

  if (not Error) then
    if (ARoutineType = rtFunction) then
      Nodes.RoutineIdent := ParseFunctionIdent()
    else
      Nodes.RoutineIdent := ParseProcedureIdent();

  Result := TDropRoutineStmt.Create(Self, ARoutineType, Nodes);
end;

function TSQLParser.ParseDropServerStmt(): TOffset;
var
  Nodes: TDropServerStmt.TNodes;
begin
  FillChar(Nodes, SizeOf(Nodes), 0);

  Nodes.StmtTag := ParseTag(kiDROP, kiSERVER);

  if (not Error) then
    if (IsTag(kiIF, kiEXISTS)) then
      Nodes.IfExistsTag := ParseTag(kiIF, kiEXISTS);

  if (not Error) then
    Nodes.ServerIdent := ParseDbIdent(ditServer);

  Result := TDropServerStmt.Create(Self, Nodes);
end;

function TSQLParser.ParseDropTablespaceStmt(): TOffset;
var
  Nodes: TDropTablespaceStmt.TNodes;
begin
  FillChar(Nodes, SizeOf(Nodes), 0);

  Nodes.StmtTag := ParseTag(kiDROP, kiTABLESPACE);

  if (not Error) then
    Nodes.Ident := ParseIdent();

  if (not Error) then
    if (IsTag(kiENGINE)) then
      Nodes.EngineValue := ParseValue(kiENGINE, vaAuto, ParseEngineIdent);

  Result := TDropTablespaceStmt.Create(Self, Nodes);
end;

function TSQLParser.ParseDropTableStmt(): TOffset;
var
  Nodes: TDropTableStmt.TNodes;
begin
  FillChar(Nodes, SizeOf(Nodes), 0);

  if (IsTag(kiDROP, kiTABLE)) then
    Nodes.StmtTag := ParseTag(kiDROP, kiTABLE)
  else
    Nodes.StmtTag := ParseTag(kiDROP, kiTEMPORARY, kiTABLE);

  if (not Error) then
    if (IsTag(kiIF, kiEXISTS)) then
      Nodes.IfExistsTag := ParseTag(kiIF, kiEXISTS);

  if (not Error) then
    Nodes.TableIdentList := ParseList(False, ParseTableIdent);

  if (not Error) then
    if (IsTag(kiRESTRICT)) then
      Nodes.RestrictCascadeTag := ParseTag(kiRESTRICT)
    else if (IsTag(kiCASCADE)) then
      Nodes.RestrictCascadeTag := ParseTag(kiCASCADE);

  Result := TDropTableStmt.Create(Self, Nodes);
end;

function TSQLParser.ParseDropTriggerStmt(): TOffset;
var
  Nodes: TDropTriggerStmt.TNodes;
begin
  FillChar(Nodes, SizeOf(Nodes), 0);

  Nodes.StmtTag := ParseTag(kiDROP, kiTrigger);

  if (not Error) then
    if (IsTag(kiIF, kiEXISTS)) then
      Nodes.IfExistsTag := ParseTag(kiIF, kiEXISTS);

  if (not Error) then
    Nodes.TriggerIdent := ParseTriggerIdent();

  Result := TDropTriggerStmt.Create(Self, Nodes);
end;

function TSQLParser.ParseDropUserStmt(): TOffset;
var
  Nodes: TDropUserStmt.TNodes;
begin
  FillChar(Nodes, SizeOf(Nodes), 0);

  Nodes.StmtTag := ParseTag(kiDROP, kiUSER);

  if (not Error) then
    if (IsTag(kiIF, kiExists)) then
      Nodes.IfExistsTag := ParseTag(kiIF, kiEXISTS);

  if (not Error) then
    Nodes.UserList := ParseList(False, ParseUserIdent);

  Result := TDropUserStmt.Create(Self, Nodes);
end;

function TSQLParser.ParseDropViewStmt(): TOffset;
var
  Nodes: TDropViewStmt.TNodes;
begin
  FillChar(Nodes, SizeOf(Nodes), 0);

  Nodes.StmtTag := ParseTag(kiDROP, kiVIEW);

  if (not Error) then
    if (IsTag(kiIF, kiEXISTS)) then
      Nodes.IfExistsTag := ParseTag(kiIF, kiEXISTS);

  if (not Error) then
    Nodes.ViewIdentList := ParseList(False, ParseTableIdent);

  if (not Error) then
    if (IsTag(kiRESTRICT)) then
      Nodes.RestrictCascadeTag := ParseTag(kiRESTRICT)
    else if (IsTag(kiCASCADE)) then
      Nodes.RestrictCascadeTag := ParseTag(kiCASCADE);

  Result := TDropViewStmt.Create(Self, Nodes);
end;

function TSQLParser.ParseEventIdent(): TOffset;
begin
  Result := ParseDbIdent(ditEvent);
end;

function TSQLParser.ParseEndLabel(): TOffset;
var
  Nodes: TEndLabel.TNodes;
begin
  FillChar(Nodes, SizeOf(Nodes), 0);

  Nodes.LabelToken := ApplyCurrentToken(utLabel);

  Result := TEndLabel.Create(Self, Nodes);
end;

function TSQLParser.ParseEngineIdent(): TOffset;
begin
  Result := ParseDbIdent(ditEngine);
end;

function TSQLParser.ParseExecuteStmt(): TOffset;
var
  Nodes: TExecuteStmt.TNodes;
begin
  FillChar(Nodes, SizeOf(Nodes), 0);

  Nodes.StmtTag := ParseTag(kiEXECUTE);

  if (not Error) then
    Nodes.StmtVariable := ParseVariableIdent();

  if (not Error) then
    if (IsTag(kiUSING)) then
    begin
      Nodes.UsingTag := ParseTag(kiUSING);

      if (not Error) then
        Nodes.VariableIdents := ParseList(False, ParseVariableIdent);
    end;

  Result := TExecuteStmt.Create(Self, Nodes);
end;

function TSQLParser.ParseExistsFunc(): TOffset;
var
  Nodes: TExistsFunc.TNodes;
begin
  FillChar(Nodes, SizeOf(Nodes), 0);

  Nodes.IdentToken := ApplyCurrentToken(utFunction);

  if (not Error) then
    Nodes.OpenBracket := ParseSymbol(ttOpenBracket);

  if (not Error) then
    Nodes.SubQuery := ParseSelectStmt(True);

  if (not Error and (Nodes.OpenBracket > 0)) then
    Nodes.CloseBracket := ParseSymbol(ttCloseBracket);

  Result := TExistsFunc.Create(Self, Nodes);
end;

function TSQLParser.ParseExplainStmt(): TOffset;
var
  Nodes: TExplainStmt.TNodes;
begin
  FillChar(Nodes, SizeOf(Nodes), 0);

  Nodes.StmtTag := ParseTag(TokenPtr(CurrentToken)^.KeywordIndex); // EXPLAIN or DESCRIBE or DESC

  if (not Error) then
    if (not IsTag(kiEXTENDED)
      and not IsTag(kiPARTITIONS)
      and not IsTag(kiFORMAT)
      and not IsTag(kiSELECT)
      and not IsTag(kiDELETE)
      and not IsTag(kiINSERT)
      and not IsTag(kiREPLACE)
      and not IsTag(kiUPDATE)
      and not IsTag(kiFOR)) then
    begin
      Nodes.TableIdent := ParseTableIdent();

      if (not Error) then
        if (not EndOfStmt(CurrentToken)) then
          Nodes.FieldIdent := ParseFieldIdent();
    end
    else
    begin
      if (IsTag(kiEXTENDED)) then
        Nodes.ExplainType := ParseTag(kiEXTENDED)
      else if (IsTag(kiPARTITIONS)) then
        Nodes.ExplainType := ParseTag(kiPARTITIONS)
      else if (IsTag(kiFORMAT)) then
        Nodes.ExplainType := ParseValue(kiFORMAT, vaYes, WordIndices(kiTRADITIONAL, kiJSON));

      if (not Error) then
        if (IsTag(kiSELECT)) then
          Nodes.ExplainStmt := ParseSelectStmt(True)
        else if (IsTag(kiDELETE)) then
          Nodes.ExplainStmt := ParseDeleteStmt()
        else if (IsTag(kiINSERT)) then
          Nodes.ExplainStmt := ParseInsertStmt(False)
        else if (IsTag(kiREPLACE)) then
          Nodes.ExplainStmt := ParseInsertStmt(True)
        else if (IsTag(kiUPDATE)) then
          Nodes.ExplainStmt := ParseUpdateStmt()
        else if (IsTag(kiFOR, kiCONNECTION)) then
          Nodes.ForConnectionValue := ParseValue(WordIndices(kiFOR, kiCONNECTION), vaNo, ParseInteger)
        else if (EndOfStmt(CurrentToken)) then
          SetError(PE_IncompleteStmt)
        else
          SetError(PE_UnexpectedToken);
    end;

  Result := TExplainStmt.Create(Self, Nodes);
end;

function TSQLParser.ParseExpr(): TOffset;
begin
  Result := ParseExpr(True);
end;

function TSQLParser.ParseExpr(const InAllowed: Boolean): TOffset;
const
  MaxNodeCount = 1000;
var
  ArgumentsList: TOffset;
  CurrentOperatorType: TOperatorType;
  I: Integer;
  DbIdent: TOffset;
  InNodes: TInOp.TNodes;
  Length: Integer;
  LikeNodes: TLikeOp.TNodes;
  Node: TOffset;
  NodeCount: Integer;
  Nodes: array [0 .. MaxNodeCount - 1] of TOffset;
  OperatorPrecedence: Integer;
  OperatorType: TOperatorType;
  PreviousOperatorType: TOperatorType;
  RegExpNodes: TRegExpOp.TNodes;
  RemoveNodes: Integer;
  Text: PChar;
begin
  NodeCount := 0; PreviousOperatorType := otUnknown;

  repeat
    Node := CurrentToken;

    if (NodeCount = MaxNodeCount) then
      raise Exception.CreateFmt(STooManyTokensInExpr, [NodeCount])
    else if (EndOfStmt(Node)) then
      SetError(PE_IncompleteStmt)
    else if (TokenPtr(Node)^.TokenType in [ttColon, ttComma, ttCloseBracket]) then
      SetError(PE_UnexpectedToken)
    else if (TokenPtr(Node)^.KeywordIndex = kiSELECT) then
      Node := ParseSelectStmt(True)
    else if ((TokenPtr(Node)^.KeywordIndex = kiINTERVAL)
      and not EndOfStmt(NextToken[1]) and (TokenPtr(NextToken[1])^.TokenType <> ttOpenBracket)) then
      Node := ParseValue(kiINTERVAL, vaNo, ParseIntervalOp)
    else if ((TokenPtr(Node)^.OperatorType = otMinus) and ((NodeCount = 0) or IsToken(Nodes[NodeCount - 1]) and (TokenPtr(Nodes[NodeCount - 1])^.OperatorType <> otUnknown))) then
      TokenPtr(Node)^.FOperatorType := otUnaryMinus
    else if ((TokenPtr(Node)^.OperatorType = otPlus) and ((NodeCount = 0) or IsToken(Nodes[NodeCount - 1]) and (TokenPtr(Nodes[NodeCount - 1])^.OperatorType <> otUnknown))) then
      TokenPtr(Node)^.FOperatorType := otUnaryPlus
    else if ((TokenPtr(Node)^.OperatorType = otNot) and ((NodeCount = 0) or IsToken(Nodes[NodeCount - 1]) and (TokenPtr(Nodes[NodeCount - 1])^.OperatorType <> otUnknown))) then
    begin
      TokenPtr(Node)^.FOperatorType := otUnaryNot;
      TokenPtr(Node)^.FUsageType := utOperator;
    end
    else if ((NodeCount > 0)
      and (PreviousOperatorType in [otEqual, otGreater, otLess, otGreaterEqual, otLessEqual, otNotEqual])
      and ((TokenPtr(Node)^.KeywordIndex = kiANY) or (TokenPtr(Node)^.KeywordIndex = kiSOME) or (TokenPtr(Node)^.KeywordIndex = kiALL))) then
      Node := ParseSubquery()
    else if (TokenPtr(Node)^.TokenType = ttOpenBracket) then
      if (EndOfStmt(NextToken[1])) then
        SetError(PE_IncompleteStmt)
      else if (TokenPtr(NextToken[1])^.KeywordIndex = kiSELECT) then
        Node := ParseSelectStmt(True)
      else
        Node := ParseList(True, ParseExpr)
    else if ((TokenPtr(Node)^.TokenType in ttIdents)
      and (TokenPtr(Node)^.OperatorType <> otIn)
      and not EndOfStmt(NextToken[1]) and (TokenPtr(NextToken[1])^.TokenType = ttOpenBracket)
      and ((TokenPtr(Node)^.KeywordIndex < 0) or (TokenPtr(Node)^.OperatorType = otUnknown) or (FunctionList.IndexOf(TokenPtr(Node)^.Text) >= 0))) then
      if ((NodeCount >= 2)
        and IsToken(Nodes[NodeCount - 1]) and (TokenPtr(Nodes[NodeCount - 1])^.OperatorType = otDot)
        and IsToken(Nodes[NodeCount - 2]) and (TokenPtr(Nodes[NodeCount - 2])^.TokenType in ttIdents)) then
      begin // Db.Func()
        TokenPtr(Nodes[NodeCount - 2])^.FUsageType := utDbIdent;
        TokenPtr(Node)^.FUsageType := utDbIdent;
        DbIdent := TDbIdent.Create(Self, ditFunction, ApplyCurrentToken(), Nodes[NodeCount - 1], Nodes[NodeCount - 2], 0, 0);
        ArgumentsList := ParseList(True, ParseExpr);
        Node := TFunctionCall.Create(Self, DbIdent, ArgumentsList);
        Dec(NodeCount, 2);
      end
      else
      begin
        TokenPtr(Node)^.GetText(Text, Length);
        if ((Length = 7) and (StrLIComp(Text, 'ADDDATE', Length) = 0)) then
          Node := ParseDateAddFunc()
        else if ((Length = 4) and (StrLIComp(Text, 'CAST', Length) = 0)) then
          Node := ParseCastFunc()
        else if ((Length = 4) and (StrLIComp(Text, 'CHAR', Length) = 0)) then
          Node := ParseCharFunc()
        else if ((Length = 7) and (StrLIComp(Text, 'CONVERT', Length) = 0)) then
          Node := ParseConvertFunc()
        else if ((Length = 8) and (StrLIComp(Text, 'DATE_ADD', Length) = 0)) then
          Node := ParseDateAddFunc()
        else if ((Length = 8) and (StrLIComp(Text, 'DATE_SUB', Length) = 0)) then
          Node := ParseDateAddFunc()
        else if ((Length = 6) and (StrLIComp(Text, 'EXISTS', Length) = 0)) then
          Node := ParseExistsFunc()
        else if ((Length = 7) and (StrLIComp(Text, 'EXTRACT', Length) = 0)) then
          Node := ParseExtractFunc()
        else if ((Length = 12) and (StrLIComp(Text, 'GROUP_CONCAT', Length) = 0)) then
          Node := ParseGroupConcatFunc()
        else if ((Length = 5) and (StrLIComp(Text, 'MATCH', Length) = 0)) then
          Node := ParseMatchFunc()
        else if ((Length = 8) and (StrLIComp(Text, 'POSITION', Length) = 0)) then
          Node := ParsePositionFunc()
        else if ((Length = 7) and (StrLIComp(Text, 'SUBDATE', Length) = 0)) then
          Node := ParseDateAddFunc()
        else if ((Length = 6) and (StrLIComp(Text, 'SUBSTR', Length) = 0)) then
          Node := ParseSubstringFunc()
        else if ((Length = 9) and (StrLIComp(Text, 'SUBSTRING', Length) = 0)) then
          Node := ParseSubstringFunc()
        else if ((Length = 4) and (StrLIComp(Text, 'TRIM', Length) = 0)) then
          Node := ParseTrimFunc()
        else if ((Length = 13) and (StrLIComp(Text, 'WEIGHT_STRING', Length) = 0)) then
          Node := ParseWeightStringFunc()
        else // Func()
          Node := ParseFunctionCall();
      end
    else if (TokenPtr(Node)^.TokenType = ttAt) then
      Node := ParseVariableIdent()
    else if ((TokenPtr(Node)^.OperatorType = otMulti)
      and ((NodeCount = 0) or IsToken(Nodes[NodeCount - 1]) and (TokenPtr(Nodes[NodeCount - 1])^.OperatorType = otDot))) then
    begin
      TokenPtr(CurrentToken)^.FTokenType := ttIdent;
      TokenPtr(CurrentToken)^.FOperatorType := otUnknown;
      TokenPtr(CurrentToken)^.FUsageType := utDbIdent;
    end
    else if (TokenPtr(Node)^.KeywordIndex = kiCASE) then
      Node := ParseCaseOp()
    else if (TokenPtr(Node)^.KeywordIndex = kiDEFAULT) then
      TokenPtr(Node)^.FUsageType := utConst
    else if (TokenPtr(Node)^.KeywordIndex = kiNULL) then
      TokenPtr(Node)^.FUsageType := utConst
    else if (TokenPtr(Node)^.KeywordIndex = kiTRUE) then
      TokenPtr(Node)^.FUsageType := utConst
    else if (TokenPtr(Node)^.KeywordIndex = kiFALSE) then
      TokenPtr(Node)^.FUsageType := utConst
    else if (TokenPtr(Node)^.KeywordIndex = kiUNKNOWN) then
      TokenPtr(Node)^.FUsageType := utConst
    else if (TokenPtr(Node)^.KeywordIndex >= 0) then
    begin
      OperatorType := OperatorTypeByKeywordIndex[TokenPtr(Node)^.KeywordIndex];
      if (OperatorType <> otUnknown) then
      begin
        TokenPtr(Node)^.FTokenType := ttOperator;
        TokenPtr(Node)^.FOperatorType := OperatorType;
        TokenPtr(Node)^.FUsageType := utOperator;
      end;
    end;

    if ((NodeCount = 0) and IsToken(Node) and (TokenPtr(Node)^.OperatorType <> otUnknown) and not (TokenPtr(Node)^.OperatorType in UnaryOperators)) then
      SetError(PE_UnexpectedToken);

    if (Node = CurrentToken) then
      Nodes[NodeCount] := ApplyCurrentToken()
    else
      Nodes[NodeCount] := Node;
    Inc(NodeCount);

    if (not IsToken(Nodes[NodeCount - 1])) then
      PreviousOperatorType := otUnknown
    else
      PreviousOperatorType := TokenPtr(Nodes[NodeCount - 1])^.OperatorType;
    if (Error or EndOfStmt(CurrentToken)) then
      CurrentOperatorType := otUnknown
    else
      CurrentOperatorType := TokenPtr(CurrentToken)^.OperatorType;
  until (Error
    or EndOfStmt(CurrentToken)
    or (PreviousOperatorType = otUnknown) and ((CurrentOperatorType = otUnknown) or (CurrentOperatorType in UnaryOperators)))
    or (CurrentOperatorType = otIn) and not InAllowed;

  if (not Error and (NodeCount > 1)) then
    for OperatorPrecedence := 1 to MaxOperatorPrecedence do
    begin
      I := 0;
      while (not Error and (I < NodeCount)) do
        if (not IsToken(Nodes[I]) or (OperatorPrecedenceByOperatorType[TokenPtr(Nodes[I])^.OperatorType] <> OperatorPrecedence)) then
          Inc(I)
        else
          case (TokenPtr(Nodes[I])^.OperatorType) of
            otBinary,
            otInterval,
            otInvertBits,
            otDistinct,
            otUnaryMinus,
            otUnaryNot,
            otUnaryPlus:
              if (I >= NodeCount - 1) then
                SetError(PE_IncompleteStmt)
              else
              begin
                Nodes[I] := TUnaryOp.Create(Self, Nodes[I], Nodes[I + 1]);
                Dec(NodeCount);
                Move(Nodes[I + 2], Nodes[I + 1], (NodeCount - I - 1) * SizeOf(Nodes[0]));
              end;
            otAnd,
            otAssign,
            otAssign2,
            otBitAND,
            otBitOR,
            otBitXOR,
            otCollate,
            otDiv,
            otDivision,
            otEqual,
            otGreater,
            otGreaterEqual,
            otHat,
            otIs,
            otLess,
            otLessEqual,
            otMinus,
            otMod,
            otMulti,
            otNotEqual,
            otNullSaveEqual,
            otOr,
            otPipes,
            otPlus,
            otShiftLeft,
            otShiftRight,
            otXOr:
              if (I = 0) then
                SetError(PE_UnexpectedToken, Nodes[I])
              else if (I >= NodeCount - 1) then
                SetError(PE_IncompleteStmt)
              else if (IsToken(Nodes[I - 1]) and (TokenPtr(Nodes[I - 1])^.TokenType = ttOperator)) then
                SetError(PE_UnexpectedToken, Nodes[I])
              else if (IsToken(Nodes[I + 1]) and (TokenPtr(Nodes[I + 1])^.TokenType = ttOperator)) then
                SetError(PE_UnexpectedToken, Nodes[I + 1])
              else
              begin
                Nodes[I - 1] := TBinaryOp.Create(Self, Nodes[I], Nodes[I - 1], Nodes[I + 1]);
                Dec(NodeCount, 2);
                Move(Nodes[I + 2], Nodes[I], (NodeCount - I) * SizeOf(Nodes[0]));
                Dec(I);
              end;
            otBetween:
              if (I + 3 >= NodeCount) then
                SetError(PE_IncompleteStmt, Nodes[I])
              else if ((NodePtr(Nodes[I + 2])^.NodeType <> ntToken) or (TokenPtr(Nodes[I + 2])^.OperatorType <> otAnd)) then
                SetError(PE_UnexpectedToken, Nodes[I + 2])
              else if (IsToken(Nodes[I - 1]) and (TokenPtr(Nodes[I - 1])^.KeywordIndex = kiNOT)) then
              begin // NOT BETWEEN
                Nodes[I - 2] := TBetweenOp.Create(Self, Nodes[I - 2], Nodes[I - 1], Nodes[I], Nodes[I + 1], Nodes[I + 2], Nodes[I + 3]);
                Dec(NodeCount, 5);
                Move(Nodes[I + 4], Nodes[I - 1], (NodeCount - (I - 1)) * SizeOf(Nodes[0]));
                Dec(I, 2);
              end
              else
              begin // BETWEEN
                Nodes[I - 1] := TBetweenOp.Create(Self, Nodes[I - 1], 0, Nodes[I], Nodes[I + 1], Nodes[I + 2], Nodes[I + 3]);
                Dec(NodeCount, 4);
                Move(Nodes[I + 4], Nodes[I], (NodeCount - I) * SizeOf(Nodes[0]));
                Dec(I);
              end;
            otDot:
              if (I = 0) then
                SetError(PE_UnexpectedToken, Nodes[I])
              else if (I >= NodeCount - 1) then
                SetError(PE_IncompleteStmt)
              else if ((NodeCount <= I + 2) or not IsToken(Nodes[I + 2]) or (TokenPtr(Nodes[I + 2])^.OperatorType <> otDot)) then
              begin // Db.Tbl or Tbl.Clmn
                TokenPtr(Nodes[I + 1])^.FUsageType := utDbIdent;
                Nodes[I - 1] := TDbIdent.Create(Self, ditField, Nodes[I + 1], 0, 0, Nodes[I], Nodes[I - 1]);
                Dec(NodeCount, 2);
                Move(Nodes[I + 2], Nodes[I], (NodeCount - I) * SizeOf(Nodes[0]));
                Dec(I);
              end
              else
              begin // Db.Tbl.Clmn
                TokenPtr(Nodes[I - 1])^.FUsageType := utDbIdent;
                TokenPtr(Nodes[I + 1])^.FUsageType := utDbIdent;
                TokenPtr(Nodes[I + 3])^.FUsageType := utDbIdent;
                Nodes[I - 1] := TDbIdent.Create(Self, ditField, Nodes[I + 3], Nodes[I], Nodes[I - 1], Nodes[I + 2], Nodes[I + 1]);
                Dec(NodeCount, 4);
                Move(Nodes[I + 4], Nodes[I], (NodeCount - I) * SizeOf(Nodes[0]));
                Dec(I);
              end;
            otEscape:
              SetError(PE_UnexpectedToken, Nodes[I]);
            otIn:
              if (NodeCount = I + 1) then
                SetError(PE_IncompleteStmt)
              else if (I = 0) then
                SetError(PE_UnexpectedToken, Nodes[0])
              else
              begin
                FillChar(InNodes, SizeOf(InNodes), 0);
                if (IsToken(Nodes[I - 1]) and (TokenPtr(Nodes[I - 1])^.OperatorType = otNot)) then
                begin
                  InNodes.NotToken := Nodes[I - 1];
                  if (I = 1) then
                    SetError(PE_UnexpectedToken, Nodes[0])
                  else
                    InNodes.Operand := Nodes[I - 2];
                end
                else
                  InNodes.Operand := Nodes[I - 1];
                InNodes.InToken := Nodes[I];
                InNodes.List := Nodes[I + 1];

                RemoveNodes := 2;
                if (InNodes.NotToken = 0) then
                  Dec(I)
                else
                begin
                  Dec(I, 2);
                  Inc(RemoveNodes);
                end;

                Nodes[I] := TInOp.Create(Self, InNodes);
                Dec(NodeCount, RemoveNodes);
                Move(Nodes[I + RemoveNodes + 1], Nodes[I + 1], (NodeCount - 1) * SizeOf(Nodes[0]));
              end;
            otLike:
              if (NodeCount = I + 1) then
                SetError(PE_IncompleteStmt)
              else if (I = 0) then
                SetError(PE_UnexpectedToken, Nodes[0])
              else
              begin
                FillChar(LikeNodes, SizeOf(LikeNodes), 0);
                if (IsToken(Nodes[I - 1]) and (TokenPtr(Nodes[I - 1])^.OperatorType = otNot)) then
                begin
                  LikeNodes.NotToken := Nodes[I - 1];
                  if (I = 1) then
                    SetError(PE_UnexpectedToken, Nodes[0])
                  else
                    LikeNodes.Operand1 := Nodes[I - 2];
                end
                else
                  LikeNodes.Operand1 := Nodes[I - 1];
                LikeNodes.LikeToken := Nodes[I];
                LikeNodes.Operand2 := Nodes[I + 1];
                if ((NodeCount >= I + 3) and IsToken(Nodes[I + 2]) and (TokenPtr(Nodes[I + 2])^.OperatorType = otEscape)) then
                begin
                  if (NodeCount = I + 3) then
                    SetError(PE_IncompleteStmt);
                  LikeNodes.EscapeToken := Nodes[I + 1];
                  LikeNodes.EscapeCharToken := Nodes[I + 2];
                end;

                RemoveNodes := 2;
                if (LikeNodes.NotToken = 0) then
                  Dec(I)
                else
                begin
                  Dec(I, 2);
                  Inc(RemoveNodes);
                end;
                if (LikeNodes.EscapeCharToken > 0) then Inc(RemoveNodes, 2);

                Nodes[I] := TLikeOp.Create(Self, LikeNodes);
                Dec(NodeCount, RemoveNodes);
                Move(Nodes[I + RemoveNodes + 1], Nodes[I + 1], (NodeCount - 1) * SizeOf(Nodes[0]));
              end;
            otRegExp:
              if (NodeCount = I + 1) then
                SetError(PE_IncompleteStmt)
              else if (I = 0) then
                SetError(PE_UnexpectedToken, Nodes[0])
              else
              begin
                FillChar(RegExpNodes, SizeOf(RegExpNodes), 0);
                if (IsToken(Nodes[I - 1]) and (TokenPtr(Nodes[I - 1])^.OperatorType = otNot)) then
                begin
                  RegExpNodes.NotToken := Nodes[I - 1];
                  if (I = 1) then
                    SetError(PE_UnexpectedToken, Nodes[0])
                  else
                    RegExpNodes.Operand1 := Nodes[I - 2];
                end
                else
                  RegExpNodes.Operand1 := Nodes[I - 1];
                RegExpNodes.RegExpToken := Nodes[I];
                RegExpNodes.Operand2 := Nodes[I + 1];

                RemoveNodes := 2;
                if (RegExpNodes.NotToken = 0) then
                  Dec(I)
                else
                begin
                  Dec(I, 2);
                  Inc(RemoveNodes);
                end;

                Nodes[I] := TRegExpOp.Create(Self, RegExpNodes);
                Dec(NodeCount, RemoveNodes);
                Move(Nodes[I + RemoveNodes + 1], Nodes[I + 1], (NodeCount - 1) * SizeOf(Nodes[0]));
              end;
            otSounds:
              if (NodeCount < I + 3) then
                SetError(PE_IncompleteStmt, Nodes[I])
              else if ((NodePtr(Nodes[I + 1])^.NodeType <> ntToken) or (TokenPtr(Nodes[I + 1])^.OperatorType <> otLike)) then
                SetError(PE_UnexpectedToken, Nodes[I + 1])
              else
              begin
                Nodes[I - 1] := TSoundsLikeOp.Create(Self, Nodes[I - 1], Nodes[I], Nodes[I + 1], Nodes[I + 2]);
                Dec(NodeCount, 3);
                Move(Nodes[I + 3], Nodes[I], (NodeCount - I) * SizeOf(Nodes[0]));
                Dec(I);
              end;
            else
              case (NodePtr(Nodes[I])^.FNodeType) of
                ntToken: SetError(PE_UnexpectedToken, Nodes[I]);
                else raise ERangeError.Create(SArgumentOutOfRange);
              end;
        end;
    end;

  if (not Error and (NodeCount <> 1)) then
    SetError(PE_Unknown);

  if (NodeCount <> 1) then
    Result := 0
  else
    Result := Nodes[0];
end;

function TSQLParser.ParseExtractFunc(): TOffset;
var
  Nodes: TExtractFunc.TNodes;
begin
  FillChar(Nodes, SizeOf(Nodes), 0);

  Nodes.IdentToken := ApplyCurrentToken(utFunction);

  if (not Error) then
    Nodes.OpenBracket := ParseSymbol(ttOpenBracket);

  if (not Error) then
    Nodes.UnitTag := ParseDateUnit();

  if (not Error) then
    Nodes.FromTag := ParseTag(kiFROM);

  if (not Error) then
    Nodes.DateExpr := ParseExpr();

  if (not Error) then
    Nodes.CloseBracket := ParseSymbol(ttCloseBracket);

  Result := TExtractFunc.Create(Self, Nodes);
end;

function TSQLParser.ParseFetchStmt(): TOffset;
var
  Nodes: TFetchStmt.TNodes;
begin
  FillChar(Nodes, SizeOf(Nodes), 0);

  Nodes.StmtTag := ParseTag(kiFETCH);

  if (not Error) then
    if (IsTag(kiNEXT, kiFROM)) then
      Nodes.FromTag := ParseTag(kiNEXT, kiFROM)
    else if (IsTag(kiFROM)) then
      Nodes.FromTag := ParseTag(kiFROM);

  if (not Error) then
    Nodes.Ident := ParseDbIdent(ditCursor);

  if (not Error) then
    Nodes.IntoTag := ParseTag(kiINTO);

  if (not Error) then
    Nodes.VariableList := ParseList(False, ParseVariableIdent);

  Result := TFetchStmt.Create(Self, Nodes);
end;

function TSQLParser.ParseFieldIdent(): TOffset;
begin
  Result := ParseDbIdent(ditField, False);
end;

function TSQLParser.ParseFieldIdentFullQualified(): TOffset;
begin
  Result := ParseDbIdent(ditField, True);
end;

function TSQLParser.ParseFieldOrVariableIdent(): TOffset;
begin
  if (not IsSymbol(ttAT)) then
    Result := ParseFieldIdent()
  else
    Result := ParseVariableIdent();
end;

function TSQLParser.ParseFlushStmt(): TOffset;
var
  Nodes: TFlushStmt.TNodes;
begin
  FillChar(Nodes, SizeOf(Nodes), 0);

  Nodes.StmtTag := ParseTag(kiFLUSH);

  if (not Error and not EndOfStmt(CurrentToken)) then
    if (TokenPtr(CurrentToken)^.KeywordIndex = kiNO_WRITE_TO_BINLOG) then
      Nodes.NoWriteToBinLogTag := ParseTag(kiNO_WRITE_TO_BINLOG)
    else if (TokenPtr(CurrentToken)^.KeywordIndex = kiLOCAL) then
      Nodes.NoWriteToBinLogTag := ParseTag(kiLOCAL);

  if (not Error) then
    Nodes.OptionList := ParseList(False, ParseFlushStmtOption);

  Result := TFlushStmt.Create(Self, Nodes);
end;

function TSQLParser.ParseFlushStmtOption(): TOffset;
var
  Nodes: TFlushStmt.TOption.TNodes;
begin
  FillChar(Nodes, SizeOf(Nodes), 0);

  if (IsTag(kiHOSTS)) then
    Nodes.OptionTag := ParseTag(kiHOSTS)
  else if (IsTag(kiPRIVILEGES)) then
    Nodes.OptionTag := ParseTag(kiPRIVILEGES)
  else if (IsTag(kiSTATUS)) then
    Nodes.OptionTag := ParseTag(kiSTATUS)
  else if (IsTag(kiTABLE)
    or IsTag(kiTABLES)) then
  begin
    Nodes.OptionTag := ParseTag(TokenPtr(CurrentToken)^.KeywordIndex);

    if (not Error and not EndOfStmt(CurrentToken)) then
      Nodes.TablesList := ParseList(False, ParseTableIdent);
  end
  else if (EndOfStmt(CurrentToken)) then
    SetError(PE_IncompleteStmt)
  else
    SetError(PE_UnexpectedToken);

  Result := TFlushStmt.TOption.Create(Self, Nodes);
end;

function TSQLParser.ParseForeignKeyIdent(): TOffset;
begin
  Result := ParseDbIdent(ditForeignKey);
end;

function TSQLParser.ParseFunctionCall(): TOffset;
var
  Length: Integer;
  Nodes: TFunctionCall.TNodes;
  Text: PChar;
begin
  FillChar(Nodes, SizeOf(Nodes), 0);

  TokenPtr(CurrentToken)^.GetText(Text, Length);
  if ((FunctionList.Count = 0) or (FunctionList.IndexOf(Text, Length) >= 0)) then
    Nodes.Ident := ApplyCurrentToken(utFunction)
  else
    Nodes.Ident := ParseFunctionIdent();

  if (not Error) then
    Nodes.ArgumentsList := ParseList(True, ParseExpr);

  Result := TFunctionCall.Create(Self, Nodes);
end;

function TSQLParser.ParseFunctionIdent(): TOffset;
begin
  Result := ParseDbIdent(ditFunction);
end;

function TSQLParser.ParseFunctionParam(): TOffset;
var
  Nodes: TRoutineParam.TNodes;
begin
  FillChar(Nodes, SizeOf(Nodes), 0);

  Nodes.IdentToken := ParseDbIdent(ditParameter);

  if (not Error) then
    Nodes.DatatypeNode := ParseDatatype();

  Result := TRoutineParam.Create(Self, Nodes);
end;

function TSQLParser.ParseFunctionReturns(): TOffset;
var
  Nodes: TFunctionReturns.TNodes;
begin
  FillChar(Nodes, SizeOf(Nodes), 0);

  Nodes.ReturnsTag := ParseTag(kiRETURNS);

  if (not Error) then
    Nodes.DatatypeNode := ParseDatatype();

  Result := TFunctionReturns.Create(Self, Nodes);
end;

function TSQLParser.ParseGetDiagnosticsStmt(): TOffset;
var
  Nodes: TGetDiagnosticsStmt.TNodes;
begin
  FillChar(Nodes, SizeOf(Nodes), 0);

  Nodes.StmtTag := ParseTag(kiGET);

  if (not Error) then
    if (IsTag(kiCURRENT)) then
      Nodes.ScopeTag := ParseTag(kiCURRENT)
    else if (IsTag(kiSTACKED)) then
      Nodes.ScopeTag := ParseTag(kiSTACKED);

  if (not Error) then
    Nodes.DiagnosticsTag := ParseTag(kiDIAGNOSTICS);

  if (not Error) then
    if (not IsTag(kiCONDITION)) then
      Nodes.InfoList := ParseList(False, ParseGetDiagnosticsStmtStmtInfo)
    else
    begin
      Nodes.ConditionTag := ParseTag(kiCONDITION);

      if (not Error) then
        Nodes.ConditionNumber := ParseExpr();

      if (not Error) then
        Nodes.InfoList := ParseList(False, ParseGetDiagnosticsStmtConditionInfo)
    end;

  Result := TGetDiagnosticsStmt.Create(Self, Nodes);
end;

function TSQLParser.ParseGetDiagnosticsStmtStmtInfo(): TOffset;
var
  Nodes: TGetDiagnosticsStmt.TStmtInfo.TNodes;
begin
  FillChar(Nodes, SizeOf(Nodes), 0);

  Nodes.Target := ParseVariableIdent();

  if (not Error) then
    if (EndOfStmt(CurrentToken)) then
      SetError(PE_IncompleteStmt)
    else if (TokenPtr(CurrentToken)^.OperatorType <> otEqual) then
      SetError(PE_UnexpectedToken)
    else
      Nodes.EqualOp := ApplyCurrentToken();

  if (not Error) then
    if (IsTag(kiNUMBER)) then
      Nodes.ItemTag := ParseTag(kiNUMBER)
    else
      Nodes.ItemTag := ParseTag(kiROW_COUNT);

  Result := TGetDiagnosticsStmt.TStmtInfo.Create(Self, Nodes);
end;

function TSQLParser.ParseGetDiagnosticsStmtConditionInfo(): TOffset;
var
  Nodes: TGetDiagnosticsStmt.TCondInfo.TNodes;
begin
  FillChar(Nodes, SizeOf(Nodes), 0);

  Nodes.Target := ParseVariableIdent();

  if (not Error) then
    if (EndOfStmt(CurrentToken)) then
      SetError(PE_IncompleteStmt)
    else if (TokenPtr(CurrentToken)^.OperatorType <> otEqual) then
      SetError(PE_UnexpectedToken)
    else
      Nodes.EqualOp := ApplyCurrentToken();

  if (not Error) then
    if (IsTag(kiCLASS_ORIGIN)
      or IsTag(kiSUBCLASS_ORIGIN)
      or IsTag(kiRETURNED_SQLSTATE)
      or IsTag(kiMESSAGE_TEXT)
      or IsTag(kiMYSQL_ERRNO)
      or IsTag(kiCONSTRAINT_CATALOG)
      or IsTag(kiCONSTRAINT_SCHEMA)
      or IsTag(kiCONSTRAINT_NAME)
      or IsTag(kiCATALOG_NAME)
      or IsTag(kiSCHEMA_NAME)
      or IsTag(kiTABLE_NAME)
      or IsTag(kiCOLUMN_NAME)
      or IsTag(kiCURSOR_NAME)) then
      Nodes.ItemTag := ParseTag(TokenPtr(CurrentToken)^.KeywordIndex)
    else if (EndOfStmt(CurrentToken)) then
      SetError(PE_IncompleteStmt)
    else
      SetError(PE_UnexpectedToken);

  Result := TGetDiagnosticsStmt.TCondInfo.Create(Self, Nodes);
end;

function TSQLParser.ParseGrantStmt(): TOffset;
var
  Found: Boolean;
  Nodes: TGrantStmt.TNodes;
begin
  FillChar(Nodes, SizeOf(Nodes), 0);

  Nodes.StmtTag := ParseTag(kiGRANT);

  if (not Error) then
    if (not IsTag(kiPROXY)) then
    begin
      Nodes.PrivilegesList := ParseList(False, ParseGrantStmtPrivileg);

      if (not Error) then
        Nodes.OnTag := ParseTag(kiON);

      if (not Error) then
        if (IsTag(kiTABLE)) then
          Nodes.ObjectValue := ParseValue(kiTABLE, vaNo, ParseTableIdent)
        else if (IsTag(kiFUNCTION)) then
          Nodes.ObjectValue := ParseValue(kiFUNCTION, vaNo, ParseFunctionIdent)
        else if (IsTag(kiPROCEDURE)) then
          Nodes.ObjectValue := ParseValue(kiPROCEDURE, vaNo, ParseProcedureIdent)
        else
          Nodes.ObjectValue := ParseTableIdent();

      if (not Error) then
        Nodes.ToTag := ParseTag(kiTO);

      if (not Error) then
        Nodes.UserSpecifications := ParseList(False, ParseGrantStmtUserSpecification);

      if (not Error) then
        if (IsTag(kiWITH)) then
        begin
          Nodes.WithTag := ParseTag(kiWITH);

          Found := True;
          while (not Error and Found) do
            if ((Nodes.GrantOptionTag = 0) and IsTag(kiGRANT, kiOPTION)) then
              Nodes.GrantOptionTag := ParseTag(kiGRANT, kiOPTION)
            else if ((Nodes.MaxQueriesPerHourTag = 0) and IsTag(kiMAX_QUERIES_PER_HOUR)) then
              Nodes.MaxQueriesPerHourTag := ParseValue(kiMAX_QUERIES_PER_HOUR, vaNo, ParseInteger)
            else if ((Nodes.MaxUpdatesPerHourTag = 0) and IsTag(kiMAX_UPDATES_PER_HOUR)) then
              Nodes.MaxUpdatesPerHourTag := ParseValue(kiMAX_UPDATES_PER_HOUR, vaNo, ParseInteger)
            else if ((Nodes.MaxConnectionsPerHourTag = 0) and IsTag(kiMAX_CONNECTIONS_PER_HOUR)) then
              Nodes.MaxConnectionsPerHourTag := ParseValue(kiMAX_CONNECTIONS_PER_HOUR, vaNo, ParseInteger)
            else if ((Nodes.MaxUserConnectionsTag = 0) and IsTag(kiMAX_USER_CONNECTIONS)) then
              Nodes.MaxUserConnectionsTag := ParseValue(kiMAX_USER_CONNECTIONS, vaNo, ParseInteger)
            else
              Found := False;
        end;
    end
    else
    begin
      Nodes.PrivilegesList := ParseTag(kiPROXY);

      if (not Error) then
        Nodes.OnTag := ParseTag(kiON);

      if (not Error) then
        Nodes.OnUser := ParseUserIdent();

      if (not Error) then
        Nodes.ToTag := ParseTag(kiTO);

      if (not Error) then
        Nodes.UserSpecifications := ParseList(False, ParseGrantStmtUserSpecification);

      if (not Error) then
        if (IsTag(kiWITH, kiGRANT, kiOPTION)) then
          Nodes.WithTag := ParseTag(kiWITH, kiGRANT, kiOPTION);
    end;

  Result := TGrantStmt.Create(Self, Nodes);
end;

function TSQLParser.ParseGrantStmtPrivileg(): TOffset;
var
  Nodes: TGrantStmt.TPrivileg.TNodes;
begin
  FillChar(Nodes, SizeOf(Nodes), 0);

  if (IsTag(kiALL, kiPRIVILEGES)) then
    Nodes.PrivilegTag := ParseTag(kiALL, kiPRIVILEGES)
  else if (IsTag(kiALL)) then
    Nodes.PrivilegTag := ParseTag(kiALL)
  else if (IsTag(kiALTER, kiROUTINE)) then
    Nodes.PrivilegTag := ParseTag(kiALTER, kiROUTINE)
  else if (IsTag(kiALTER)) then
    Nodes.PrivilegTag := ParseTag(kiALTER)
  else if (IsTag(kiCREATE, kiROUTINE)) then
    Nodes.PrivilegTag := ParseTag(kiCREATE, kiROUTINE)
  else if (IsTag(kiCREATE, kiTABLESPACE)) then
    Nodes.PrivilegTag := ParseTag(kiCREATE, kiTABLESPACE)
  else if (IsTag(kiCREATE, kiTEMPORARY, kiTABLES)) then
    Nodes.PrivilegTag := ParseTag(kiCREATE, kiTEMPORARY, kiTABLES)
  else if (IsTag(kiCREATE, kiUSER)) then
    Nodes.PrivilegTag := ParseTag(kiCREATE, kiUSER)
  else if (IsTag(kiCREATE, kiVIEW)) then
    Nodes.PrivilegTag := ParseTag(kiCREATE, kiVIEW)
  else if (IsTag(kiCREATE)) then
    Nodes.PrivilegTag := ParseTag(kiCREATE)
  else if (IsTag(kiDELETE)) then
    Nodes.PrivilegTag := ParseTag(kiDELETE)
  else if (IsTag(kiDROP)) then
    Nodes.PrivilegTag := ParseTag(kiDROP)
  else if (IsTag(kiEVENT)) then
    Nodes.PrivilegTag := ParseTag(kiEVENT)
  else if (IsTag(kiEXECUTE)) then
    Nodes.PrivilegTag := ParseTag(kiEXECUTE)
  else if (IsTag(kiFILE)) then
    Nodes.PrivilegTag := ParseTag(kiFILE)
  else if (IsTag(kiGRANT, kiOPTION)) then
    Nodes.PrivilegTag := ParseTag(kiGRANT, kiOPTION)
  else if (IsTag(kiINDEX)) then
    Nodes.PrivilegTag := ParseTag(kiINDEX)
  else if (IsTag(kiINSERT)) then
    Nodes.PrivilegTag := ParseTag(kiINSERT)
  else if (IsTag(kiLOCK, kiTABLES)) then
    Nodes.PrivilegTag := ParseTag(kiLOCK, kiTABLES)
  else if (IsTag(kiPROCESS)) then
    Nodes.PrivilegTag := ParseTag(kiPROCESS)
  else if (IsTag(kiPROXY)) then
    Nodes.PrivilegTag := ParseTag(kiPROXY)
  else if (IsTag(kiREFERENCES)) then
    Nodes.PrivilegTag := ParseTag(kiREFERENCES)
  else if (IsTag(kiRELOAD)) then
    Nodes.PrivilegTag := ParseTag(kiRELOAD)
  else if (IsTag(kiREPLICATION, kiCLIENT)) then
    Nodes.PrivilegTag := ParseTag(kiREPLICATION, kiCLIENT)
  else if (IsTag(kiREPLICATION, kiSLAVE)) then
    Nodes.PrivilegTag := ParseTag(kiREPLICATION, kiSLAVE)
  else if (IsTag(kiSELECT)) then
    Nodes.PrivilegTag := ParseTag(kiSELECT)
  else if (IsTag(kiSHOW, kiDATABASES)) then
    Nodes.PrivilegTag := ParseTag(kiSHOW, kiDATABASES)
  else if (IsTag(kiSHOW, kiVIEW)) then
    Nodes.PrivilegTag := ParseTag(kiSHOW, kiVIEW)
  else if (IsTag(kiSHUTDOWN)) then
    Nodes.PrivilegTag := ParseTag(kiSHUTDOWN)
  else if (IsTag(kiSUPER)) then
    Nodes.PrivilegTag := ParseTag(kiSUPER)
  else if (IsTag(kiTRIGGER)) then
    Nodes.PrivilegTag := ParseTag(kiTRIGGER)
  else if (IsTag(kiUPDATE)) then
    Nodes.PrivilegTag := ParseTag(kiUPDATE)
  else if (IsTag(kiUSAGE)) then
    Nodes.PrivilegTag := ParseTag(kiUSAGE)
  else if (EndOfStmt(CurrentToken)) then
    SetError(PE_IncompleteStmt)
  else
    SetError(PE_UnexpectedToken);

  if (not Error) then
    if (IsSymbol(ttOpenBracket)) then
      Nodes.PrivilegTag := ParseList(True, ParseFieldIdent);

  Result := TGrantStmt.TPrivileg.Create(Self, Nodes);
end;

function TSQLParser.ParseGrantStmtUserSpecification(): TOffset;
var
  Nodes: TGrantStmt.TUserSpecification.TNodes;
begin
  FillChar(Nodes, SizeOf(Nodes), 0);

  Nodes.UserIdent := ParseUserIdent();

  if (not Error) then
    if (IsTag(kiIDENTIFIED, kiBY, kiPASSWORD)) then
    begin
      Nodes.IdentifiedToken := ParseTag(kiIDENTIFIED, kiBY, kiPASSWORD);

      if (not Error and not EndOfStmt(CurrentToken)) then
        if (TokenPtr(CurrentToken)^.TokenType in ttStrings - [ttIdent]) then
          Nodes.AuthString := ParseString()
        else if ((TokenPtr(CurrentToken)^.OperatorType = otLess)
          and not EndOfStmt(NextToken[2])
          and (TokenPtr(NextToken[1])^.TokenType = ttIdent)
          and (TokenPtr(NextToken[2])^.OperatorType = otGreater)) then
          Nodes.AuthString := ParseSecretIdent();
    end
    else if (IsTag(kiIDENTIFIED, kiBY)) then
    begin
      Nodes.IdentifiedToken := ParseTag(kiIDENTIFIED, kiBY);

      if (not Error) then
        Nodes.AuthString := ParseString();
    end
    else if (IsTag(kiIDENTIFIED, kiWITH)) then
    begin
      Nodes.IdentifiedToken := ParseTag(kiIDENTIFIED, kiWITH);

      if (not Error) then
        if (EndOfStmt(CurrentToken)) then
          SetError(PE_IncompleteStmt)
        else if (TokenPtr(CurrentToken)^.TokenType = ttIdent) then
          Nodes.PluginIdent := ParseIdent()
        else if (TokenPtr(CurrentToken)^.TokenType = ttString) then
          Nodes.PluginIdent := ParseString()
        else
          SetError(PE_UnexpectedToken);

      if (not Error) then
        if (IsTag(kiAS)) then
        begin
          Nodes.AsTag := ParseTag(kiAS);

          if (not Error) then
            Nodes.AuthString := ParseString();
        end
        else if (IsTag(kiBY)) then
        begin
          Nodes.AsTag := ParseTag(kiBY);

          if (not Error) then
            Nodes.AuthString := ParseString();
        end;
  end;

  Result := TGrantStmt.TUserSpecification.Create(Self, Nodes);
end;

function TSQLParser.ParseGroupConcatFunc(): TOffset;
var
  Nodes: TGroupConcatFunc.TNodes;
begin
  FillChar(Nodes, SizeOf(Nodes), 0);

  Nodes.IdentToken := ApplyCurrentToken(utFunction);

  if (not Error) then
    Nodes.OpenBracket := ParseSymbol(ttOpenBracket);

  if (not Error) then
    if (IsTag(kiDISTINCT)) then
      Nodes.DistinctTag := ParseTag(kiDISTINCT);

  if (not Error) then
    Nodes.ExprList := ParseList(False, ParseExpr);

  if (not Error) then
    if (IsTag(kiORDER, kiBY)) then
    begin
      Nodes.OrderByTag := ParseTag(kiORDER, kiBY);

      if (not Error) then
        Nodes.OrderByExprList := ParseList(False, ParseGroupConcatFuncExpr);
    end;

  if (not Error) then
    if (IsTag(kiSEPARATOR)) then
      Nodes.SeparatorValue := ParseValue(kiSEPARATOR, vaNo, ParseString);

  if (not Error) then
    Nodes.CloseBracket := ParseSymbol(ttCloseBracket);

  Result := TGroupConcatFunc.Create(Self, Nodes);
end;

function TSQLParser.ParseGroupConcatFuncExpr(): TOffset;
var
  Nodes: TGroupConcatFunc.TExpr.TNodes;
begin
  FillChar(Nodes, SizeOf(Nodes), 0);

  Nodes.Expr := ParseExpr();

  if (not Error) then
    if (IsTag(kiASC)) then
      Nodes.Direction := ParseTag(kiASC)
    else if (IsTag(kiDESC)) then
      Nodes.Direction := ParseTag(kiDESC);

  Result := TGroupConcatFunc.TExpr.Create(Self, Nodes);
end;

function TSQLParser.ParseHelpStmt(): TOffset;
var
  Nodes: THelpStmt.TNodes;
begin
  FillChar(Nodes, SizeOf(Nodes), 0);

  Nodes.StmtTag := ParseTag(kiHELP);

  if (not Error) then
    Nodes.HelpString := ParseString();

  Result := THelpStmt.Create(Self, Nodes);
end;

function TSQLParser.ParseIdent(): TOffset;
begin
  Result := 0;
  if (EndOfStmt(CurrentToken)) then
    SetError(PE_IncompleteStmt)
  else if (not (TokenPtr(CurrentToken)^.TokenType in ttIdents)) then
    SetError(PE_UnexpectedToken)
  else
    Result := ApplyCurrentToken();
end;

function TSQLParser.ParseIfStmt(): TOffset;

  function ParseBranch(): TOffset;
  var
    Nodes: TIfStmt.TBranch.TNodes;
  begin
    FillChar(Nodes, SizeOf(Nodes), 0);

    if (IsTag(kiIF)) then
    begin
      Nodes.Tag := ParseTag(kiIF);

      if (not Error) then
        Nodes.ConditionExpr := ParseExpr();

      if (not Error) then
        Nodes.ThenTag := ParseTag(kiTHEN);
    end
    else if (IsTag(kiELSEIF)) then
    begin
      Nodes.Tag := ParseTag(kiELSEIF);

      if (not Error) then
        Nodes.ConditionExpr := ParseExpr();

      if (not Error) then
        Nodes.ThenTag := ParseTag(kiTHEN);
    end
    else if (IsTag(kiELSE)) then
      Nodes.Tag := ParseTag(kiELSE)
    else
      SetError(PE_Unknown);

    if (not Error) then
      if (not IsTag(kiELSEIF)
        and not IsTag(kiELSE)
        and not IsTag(kiEND, kiIF)) then
        Nodes.StmtList := ParseList(False, ParsePL_SQLStmt, ttSemicolon);

    Result := TIfStmt.TBranch.Create(Self, Nodes);
  end;

var
  Found: Boolean;
  Branches: array of TOffset;
  ListNodes: TList.TNodes;
  Nodes: TIfStmt.TNodes;
begin
  FillChar(Nodes, SizeOf(Nodes), 0);

  SetLength(Branches, 1);
  Branches[0] := ParseBranch();

  Found := True;
  while (not Error and Found) do
    if (IsTag(kiELSEIF)) then
    begin
      SetLength(Branches, Length(Branches) + 1);
      Branches[Length(Branches) - 1] := ParseBranch();
    end
    else
      Found := False;

  if (not Error) then
    if (IsTag(kiELSE)) then
    begin
      SetLength(Branches, Length(Branches) + 1);
      Branches[Length(Branches) - 1] := ParseBranch();
    end;

  if (not Error) then
    Nodes.EndTag := ParseTag(kiEND, kiIF);

  FillChar(ListNodes, SizeOf(ListNodes), 0);
  Nodes.BranchList := TList.Create(Self, ListNodes, ttUnknown, Length(Branches), Branches);
  SetLength(Branches, 0);

  Result := TIfStmt.Create(Self, Nodes);
end;

function TSQLParser.ParseInsertStmt(const Replace: Boolean = False): TOffset;
var
  Nodes: TInsertStmt.TNodes;
begin
  FillChar(Nodes, SizeOf(Nodes), 0);

  if (IsTag(kiINSERT)) then
    Nodes.InsertTag := ParseTag(kiINSERT)
  else
    Nodes.InsertTag := ParseTag(kiREPLACE);

  if (not Error) then
    if (IsTag(kiLOW_PRIORITY)) then
      Nodes.PriorityTag := ParseTag(kiLOW_PRIORITY)
    else if (IsTag(kiDELAYED)) then
      Nodes.PriorityTag := ParseTag(kiDELAYED)
    else if (IsTag(kiHIGH_PRIORITY)) then
      Nodes.PriorityTag := ParseTag(kiHIGH_PRIORITY);

  if (not Error) then
    if (IsTag(kiIGNORE)) then
      Nodes.IgnoreTag := ParseTag(kiIGNORE);

  if (not Error) then
    if (IsTag(kiINTO)) then
      Nodes.IntoTag := ParseTag(kiINTO);

  if (not Error) then
    Nodes.TableIdent := ParseTableIdent();

  if (not Error) then
    if (IsTag(kiPARTITION)) then
    begin
      Nodes.Partition.Tag := ParseTag(kiPARTITION);

      if (not Error) then
        Nodes.Partition.List := ParseList(True, ParsePartitionIdent);
    end;

  if (not Error) then
    if (IsTag(kiSET)) then
    begin
      Nodes.Set_.Tag := ParseTag(kiSET);

      if (not Error) then
        Nodes.Set_.List := ParseList(False, ParseInsertStmtSetItemsList);
    end
    else if (IsTag(kiSELECT)) then
      Nodes.SelectStmt := ParseSelectStmt(True)
    else if (IsSymbol(ttOpenBracket)
      and not EndOfStmt(NextToken[1]) and (TokenPtr(NextToken[1])^.KeywordIndex = kiSELECT)) then
      Nodes.SelectStmt := ParseSelectStmt(True)
    else if (not EndOfStmt(CurrentToken)) then
    begin
      if (IsSymbol(ttOpenBracket)) then
        Nodes.ColumnList := ParseList(True, ParseFieldIdentFullQualified);

      if (not Error) then
        if (IsTag(kiVALUES)
          or IsTag(kiVALUE)) then
        begin
          Nodes.Values.Tag := ParseTag(TokenPtr(CurrentToken)^.KeywordIndex);

          if (not Error) then
            Nodes.Values.List := ParseList(False, ParseInsertStmtValuesList);
        end
        else if ((Nodes.SelectStmt = 0) and IsTag(kiSELECT)) then
          Nodes.SelectStmt := ParseSelectStmt(True)
        else if ((Nodes.SelectStmt = 0) and IsSymbol(ttOpenBracket)
          and not EndOfStmt(NextToken[1]) and (TokenPtr(NextToken[1])^.KeywordIndex = kiSELECT)) then
          Nodes.SelectStmt := ParseSelectStmt(True)
        else
          SetError(PE_UnexpectedToken);
    end
    else
      SetError(PE_IncompleteStmt);

  if (not Error) then
    if (IsTag(kiON, kiDUPLICATE, kiKEY, kiUPDATE)) then
    begin
      Nodes.OnDuplicateKeyUpdateTag := ParseTag(kiON, kiDUPLICATE, kiKEY, kiUPDATE);

      if (not Error) then
        Nodes.UpdateList := ParseList(False, ParseUpdateStmtValue);
    end;

  Result := TInsertStmt.Create(Self, Nodes);
end;

function TSQLParser.ParseInsertStmtSetItemsList(): TOffset;
var
  Nodes: TInsertStmt.TSetItem.TNodes;
begin
  FillChar(Nodes, SizeOf(Nodes), 0);

  Nodes.FieldToken := ParseFieldIdent();

  if (not Error) then
    if (EndOfStmt(CurrentToken)) then
      SetError(PE_IncompleteStmt)
    else if (TokenPtr(CurrentToken)^.OperatorType <> otEqual) then
      SetError(PE_UnexpectedToken)
    else
    begin
      TokenPtr(CurrentToken)^.FOperatorType := otAssign;
      Nodes.AssignToken := ApplyCurrentToken();
    end;

  if (not Error) then
    Nodes.ValueNode := ParseExpr();

  Result := TInsertStmt.TSetItem.Create(Self, Nodes);
end;

function TSQLParser.ParseInsertStmtValuesList(): TOffset;
begin
  Result := ParseList(True, ParseExpr);
end;

function TSQLParser.ParseInteger(): TOffset;
begin
  Result := 0;
  if (EndOfStmt(CurrentToken)) then
    SetError(PE_IncompleteStmt)
  else if (TokenPtr(CurrentToken)^.TokenType <> ttInteger) then
    SetError(PE_UnexpectedToken)
  else
    Result := ApplyCurrentToken(utConst);
end;

function TSQLParser.ParseIntervalOp(): TOffset;
var
  Nodes: TIntervalOp.TNodes;
begin
  FillChar(Nodes, SizeOf(Nodes), 0);

  Nodes.QuantityExp := ParseExpr();

  if (not Error) then
    Nodes.UnitTag := ParseDateUnit();

  Result := TIntervalOp.Create(Self, Nodes);
end;

function TSQLParser.ParseIterateStmt(): TOffset;
var
  Nodes: TIterateStmt.TNodes;
begin
  FillChar(Nodes, SizeOf(Nodes), 0);

  Nodes.IterateToken := ParseTag(kiITERATE);

  if (not Error) then
    if (EndOfStmt(CurrentToken)) then
      SetError(PE_IncompleteStmt)
    else if (TokenPtr(CurrentToken)^.TokenType <> ttIdent) then
      SetError(PE_UnexpectedToken)
    else
      Nodes.LabelToken := ApplyCurrentToken(utLabel);

  Result := TIterateStmt.Create(Self, Nodes);
end;

function TSQLParser.ParseKeyIdent(): TOffset;
begin
  Result := ParseDbIdent(ditKey);
end;

function TSQLParser.ParseKillStmt(): TOffset;
var
  Nodes: TKillStmt.TNodes;
begin
  FillChar(Nodes, SizeOf(Nodes), 0);

  if (IsTag(kiKILL, kiCONNECTION)) then
    Nodes.StmtTag := ParseTag(kiKILL, kiCONNECTION)
  else if (IsTag(kiKILL, kiQUERY)) then
    Nodes.StmtTag := ParseTag(kiKILL, kiQUERY)
  else
    Nodes.StmtTag := ParseTag(kiKILL);

  if (not Error) then
    Nodes.ProcessIdToken := ParseExpr();

  Result := TKillStmt.Create(Self, Nodes);
end;

function TSQLParser.ParseLeaveStmt(): TOffset;
var
  Nodes: TLeaveStmt.TNodes;
begin
  FillChar(Nodes, SizeOf(Nodes), 0);

  Nodes.StmtTag := ParseTag(kiLEAVE);

  if (not Error) then
    if (EndOfStmt(CurrentToken)) then
      SetError(PE_IncompleteStmt)
    else if (TokenPtr(CurrentToken)^.TokenType <> ttIdent) then
      SetError(PE_UnexpectedToken)
    else
      Nodes.LabelToken := ApplyCurrentToken(utLabel);

  Result := TLeaveStmt.Create(Self, Nodes);
end;

function TSQLParser.ParseList(const Brackets: Boolean; const ParseElement: TParseFunction; const DelimiterType: TTokenType = ttComma): TOffset;
var
  ChildrenArray: array [0 .. 100 - 1] of TOffset;
  ChildrenList: Classes.TList;
  DelimiterFound: Boolean;
  I: Integer;
  Index: Integer;
  Nodes: TList.TNodes;
begin
  FillChar(Nodes, SizeOf(Nodes), 0);

  ChildrenList := nil;

  if (Brackets) then
    Nodes.OpenBracket := ParseSymbol(ttOpenBracket);

  Index := 0;
  if (not Error and (not Brackets or not IsSymbol(ttCloseBracket))) then
  begin
    repeat
      if (Index < Length(ChildrenArray)) then
        ChildrenArray[Index] := ParseElement()
      else
      begin
        if (Index = Length(ChildrenArray)) then
        begin
          ChildrenList := Classes.TList.Create();
          for I := 0 to Length(ChildrenArray) - 1 do
            ChildrenList.Add(Pointer(ChildrenArray[I]));
        end;
        ChildrenList.Add(Pointer(ParseElement()));
      end;
      Inc(Index);

      DelimiterFound := IsSymbol(DelimiterType);
      if (not Error and DelimiterFound) then
      begin
        if (Index < Length(ChildrenArray)) then
          ChildrenArray[Index] := ParseSymbol(DelimiterType)
        else
        begin
          if (Index = Length(ChildrenArray)) then
          begin
            ChildrenList := Classes.TList.Create();
            for I := 0 to Length(ChildrenArray) - 1 do
              ChildrenList.Add(Pointer(ChildrenArray[I]));
          end;
          ChildrenList.Add(Pointer(ParseSymbol(DelimiterType)));
        end;
        Inc(Index);
      end;
    until (Error or not DelimiterFound
      or ((DelimiterType = ttSemicolon) and
        (IsTag(kiELSE)
          or IsTag(kiELSEIF)
          or IsTag(kiUNTIL)
          or IsTag(kiWHEN)
          or IsTag(kiEND))));

    if (not Error and DelimiterFound and EndOfStmt(CurrentToken)) then
      SetError(PE_IncompleteStmt);
  end;

  if (not Error and Brackets) then
    Nodes.CloseBracket := ParseSymbol(ttCloseBracket);

  if (not Assigned(ChildrenList)) then
    Result := TList.Create(Self, Nodes, DelimiterType, Index, ChildrenArray)
  else
  begin
    Result := TList.Create(Self, Nodes, DelimiterType, ChildrenList.Count, TIntegerArray(ChildrenList.List));
    ChildrenList.Free();
  end;
end;

function TSQLParser.ParseLoadDataStmt(): TOffset;
var
  Nodes: TLoadDataStmt.TNodes;
begin
  FillChar(Nodes, SizeOf(Nodes), 0);

  Nodes.StmtTag := ParseTag(kiLOAD, kiDATA);

  if (not Error) then
    if (IsTag(kiLOW_PRIORITY)) then
      Nodes.PriorityTag := ParseTag(kiLOW_PRIORITY)
    else if (IsTag(kiCONCURRENT)) then
      Nodes.PriorityTag := ParseTag(kiCONCURRENT);

  if (not Error) then
    if (IsTag(kiLOCAL, kiINFILE)) then
      Nodes.InfileTag := ParseTag(kiLOCAL, kiINFILE)
    else
      Nodes.InfileTag := ParseTag(kiINFILE);

  if (not Error) then
    Nodes.FilenameString := ParseString();

  if (not Error) then
    if (IsTag(kiREPLACE)) then
      Nodes.ReplaceIgnoreTag := ParseTag(kiREPLACE)
    else if (IsTag(kiIGNORE)) then
      Nodes.ReplaceIgnoreTag := ParseTag(kiIGNORE);

  if (not Error) then
    Nodes.IntoTableTag := ParseTag(kiINTO, kiTABLE);

  if (not Error) then
    Nodes.Ident := ParseTableIdent();

  if (not Error) then
    if (IsTag(kiPARTITION)) then
    begin
      Nodes.PartitionTag := ParseTag(kiPARTITION);

      if (not Error) then
        Nodes.PartitionList := ParseList(True, ParsePartitionIdent);
    end;

  if (not Error) then
    if (IsTag(kiCHARACTER, kiSET)) then
      Nodes.CharacterSetValue := ParseValue(WordIndices(kiCHARACTER, kiSET), vaNo, ParseCharsetIdent)
    else if (IsTag(kiCHARSET)) then
      Nodes.CharacterSetValue := ParseValue(kiCHARSET, vaNo, ParseCharsetIdent);

  if (not Error) then
    if (IsTag(kiFIELDS)
      or IsTag(kiCOLUMNS)) then
    begin
      Nodes.ColumnsTag := ParseTag(TokenPtr(CurrentToken)^.KeywordIndex);

      if (not Error) then
        if (IsTag(kiTERMINATED, kiBY)) then
          Nodes.ColumnsTerminatedByValue := ParseValue(WordIndices(kiTERMINATED, kiBY), vaNo, ParseString);

      if (not Error) then
        if (IsTag(kiOPTIONALLY, kiENCLOSED, kiBY)) then
          Nodes.EnclosedByValue := ParseValue(WordIndices(kiOPTIONALLY, kiENCLOSED, kiBY), vaNo, ParseString)
        else if (IsTag(kiENCLOSED, kiBY)) then
          Nodes.EnclosedByValue := ParseValue(WordIndices(kiENCLOSED, kiBY), vaNo, ParseString);

      if (not Error) then
        if (IsTag(kiESCAPED, kiBY)) then
          Nodes.EscapedByValue := ParseValue(WordIndices(kiESCAPED, kiBY), vaNo, ParseString);
    end;

  if (not Error) then
    if (IsTag(kiLINES)) then
    begin
      Nodes.LinesTag := ParseTag(kiLINES);

      if (not Error) then
        if (IsTag(kiSTARTING, kiBY)) then
          Nodes.StartingByValue := ParseValue(WordIndices(kiSTARTING, kiBY), vaNo, ParseString);

      if (not Error) then
        if (IsTag(kiTERMINATED, kiBY)) then
          Nodes.LinesTerminatedByValue := ParseValue(WordIndices(kiTERMINATED, kiBY), vaNo, ParseString);
    end;

  if (IsTag(kiIGNORE)) then
  begin
    Nodes.Ignore.Tag := ParseTag(kiIGNORE);

    if (not Error) then
      Nodes.Ignore.NumberToken := ParseInteger();

    if (not Error) then
      if (IsTag(kiLINES)) then
        Nodes.Ignore.LinesTag := ParseTag(kiLINES)
      else
        Nodes.Ignore.LinesTag := ParseTag(kiROWS);
  end;

  if (IsSymbol(ttOpenBracket)) then
    Nodes.ColumnList := ParseList(True, ParseFieldOrVariableIdent);

  if (not Error) then
    if (IsTag(kiSET)) then
      Nodes.SetList := ParseValue(kiSET, vaNo, False, ParseUpdateStmtValue);

  Result := TLoadDataStmt.Create(Self, Nodes);
end;

function TSQLParser.ParseLoadStmt(): TOffset;
begin
  if (IsTag(kiLOAD, kiDATA)) then
    Result := ParseLoadDataStmt()
  else
    Result := ParseLoadXMLStmt();
end;

function TSQLParser.ParseLoadXMLStmt(): TOffset;
var
  Nodes: TLoadXMLStmt.TNodes;
begin
  FillChar(Nodes, SizeOf(Nodes), 0);

  Nodes.StmtTag := ParseTag(kiLOAD, kiXML);

  if (IsTag(kiLOW_PRIORITY)) then
      Nodes.PriorityTag := ParseTag(kiLOW_PRIORITY)
    else if (IsTag(kiCONCURRENT)) then
      Nodes.PriorityTag := ParseTag(kiCONCURRENT);

  if (not Error) then
    if (IsTag(kiLOCAL, kiINFILE)) then
      Nodes.InfileTag := ParseTag(kiLOCAL, kiINFILE)
    else
      Nodes.InfileTag := ParseTag(kiINFILE);

  if (not Error and not EndOfStmt(CurrentToken)) then
    if (TokenPtr(CurrentToken)^.KeywordIndex = kiREPLACE) then
      Nodes.ReplaceIgnoreTag := ParseTag(kiREPLACE)
    else if (TokenPtr(CurrentToken)^.KeywordIndex = kiIGNORE) then
      Nodes.ReplaceIgnoreTag := ParseTag(kiIGNORE);

  if (not Error) then
    Nodes.IntoTableValue := ParseValue(WordIndices(kiINTO, kiTABLE), vaNo, ParseTableIdent);

  if (not Error) then
    if (IsTag(kiCHARACTER, kiSET)) then
      Nodes.CharacterSetValue := ParseValue(WordIndices(kiCHARACTER, kiSET), vaNo, ParseCharsetIdent)
    else if (IsTag(kiCHARSET)) then
      Nodes.CharacterSetValue := ParseValue(kiCHARSET, vaNo, ParseCharsetIdent);

  if (not Error) then
    if (IsTag(kiROWS, kiIDENTIFIED, kiBY)) then
      Nodes.RowsIdentifiedByValue := ParseValue(WordIndices(kiROWS, kiIDENTIFIED, kiBY), vaNo, ParseString);

  if (not Error) then
    if (IsTag(kiIGNORE)) then
    begin
      Nodes.Ignore.Tag := ParseTag(kiIGNORE);

      if (not Error) then
        Nodes.Ignore.NumberToken := ParseInteger();

      if (not Error) then
        if (IsTag(kiLINES)) then
          Nodes.Ignore.LinesTag := ParseTag(kiLINES)
        else
          Nodes.Ignore.LinesTag := ParseTag(kiROWS);
    end;

  if (not Error) then
    if (IsSymbol(ttOpenBracket)) then
      Nodes.FieldList := ParseList(True, ParseFieldIdent);

  if (not Error) then
    if (IsTag(kiSET)) then
      Nodes.SetList := ParseValue(kiSET, vaNo, False, ParseUpdateStmtValue);

  Result := TLoadXMLStmt.Create(Self, Nodes);
end;

function TSQLParser.ParseLockTableStmt(): TOffset;
var
  Nodes: TLockTableStmt.TNodes;
begin
  FillChar(Nodes, SizeOf(Nodes), 0);

  Nodes.LockTablesTag := ParseTag(kiLOCK, kiTABLES);

  if (not Error) then
    Nodes.ItemList := ParseList(False, ParseLockStmtItem);

  Result := TLockTableStmt.Create(Self, Nodes);
end;

function TSQLParser.ParseLockStmtItem(): TOffset;
var
  Nodes: TLockTableStmt.TItem.TNodes;
begin
  FillChar(Nodes, SizeOf(Nodes), 0);

  Nodes.TableIdent := ParseTableIdent();

  if (not Error) then
    if (IsTag(kiAS)) then
    begin
      Nodes.AsTag := ParseTag(kiAS);

      if (not Error) then
        Nodes.AliasIdent := ParseAliasIdent();
    end;

  if (not Error and (Nodes.AliasIdent = 0)
    and not EndOfStmt(CurrentToken)
    and (TokenPtr(CurrentToken)^.TokenType in ttIdents + ttStrings) and (TokenPtr(CurrentToken)^.KeywordIndex < 0)) then
    Nodes.AliasIdent := ParseAliasIdent();

  if (not Error) then
    if (IsTag(kiREAD, kiLOCAL)) then
      Nodes.TypeTag := ParseTag(kiREAD, kiLOCAL)
    else if (IsTag(kiREAD)) then
      Nodes.TypeTag := ParseTag(kiREAD)
    else if (IsTag(kiLOW_PRIORITY, kiWRITE)) then
      Nodes.TypeTag := ParseTag(kiLOW_PRIORITY, kiWRITE)
    else
      Nodes.TypeTag := ParseTag(kiWRITE);

  Result := TLockTableStmt.TItem.Create(Self, Nodes);
end;

function TSQLParser.ParseLoopStmt(): TOffset;
var
  Nodes: TLoopStmt.TNodes;
begin
  FillChar(Nodes, SizeOf(Nodes), 0);

  if (not EndOfStmt(CurrentToken) and (TokenPtr(CurrentToken)^.TokenType = ttIdent)
    and not EndOfStmt(NextToken[1]) and (TokenPtr(NextToken[1])^.TokenType = ttColon)) then
    Nodes.BeginLabel := ParseBeginLabel();

  Nodes.BeginTag := ParseTag(kiLOOP);

  if (not Error and not IsTag(kiEND)) then
    Nodes.StmtList := ParseList(False, ParsePL_SQLStmt, ttSemicolon);

  if (not Error) then
    Nodes.EndTag := ParseTag(kiEND, kiLOOP);

  if (not Error
    and not EndOfStmt(CurrentToken) and (TokenPtr(CurrentToken)^.TokenType = ttIdent)
    and (Nodes.BeginLabel > 0) and (NodePtr(Nodes.BeginLabel)^.NodeType = ntBeginLabel)) then
    if ((Nodes.BeginLabel = 0) or (lstrcmpi(PChar(TokenPtr(CurrentToken)^.AsString), PChar(PBeginLabel(NodePtr(Nodes.BeginLabel))^.LabelName)) <> 0)) then
      SetError(PE_UnexpectedToken)
    else
      Nodes.EndLabel := ParseEndLabel();

  Result := TLoopStmt.Create(Self, Nodes);
end;

function TSQLParser.ParseNumeric(): TOffset;
begin
  Result := 0;
  if (EndOfStmt(CurrentToken)) then
    SetError(PE_IncompleteStmt)
  else if (TokenPtr(CurrentToken)^.TokenType <> ttNumeric) then
    SetError(PE_UnexpectedToken)
  else
    Result := ApplyCurrentToken(utConst);
end;

function TSQLParser.ParseMatchFunc(): TOffset;
var
  Nodes: TMatchFunc.TNodes;
begin
  FillChar(Nodes, SizeOf(Nodes), 0);

  Nodes.IdentToken := ApplyCurrentToken(utFunction);

  if (not Error) then
    Nodes.MatchList := ParseList(True, ParseExpr);

  if (not Error) then
    Nodes.AgainstTag := ParseTag(kiAGAINST);

  if (not Error) then
    Nodes.OpenBracket := ParseSymbol(ttOpenBracket);

  if (not Error) then
    Nodes.Expr := ParseExpr(False);

  if (not Error and not EndOfStmt(CurrentToken)) then
    if (IsTag(kiIN, kiNATURAL, kiLANGUAGE, kiMODE, kiWITH, kiQUERY, kiEXPANSION)) then
      Nodes.SearchModifierTag := ParseTag(kiIN, kiNATURAL, kiLANGUAGE, kiMODE, kiWITH, kiQUERY, kiEXPANSION)
    else if (IsTag(kiIN, kiNATURAL, kiLANGUAGE, kiMODE)) then
      Nodes.SearchModifierTag := ParseTag(kiIN, kiNATURAL, kiLANGUAGE, kiMODE)
    else if (IsTag(kiIN, kiBOOLEAN, kiMODE)) then
      Nodes.SearchModifierTag := ParseTag(kiIN, kiBOOLEAN, kiMODE)
    else if (IsTag(kiWITH, kiQUERY, kiEXPANSION)) then
      Nodes.SearchModifierTag := ParseTag(kiWITH, kiQUERY, kiEXPANSION);

  if (not Error) then
    Nodes.CloseBracket := ParseSymbol(ttCloseBracket);

  Result := TMatchFunc.Create(Self, Nodes);
end;

function TSQLParser.ParseProcedureIdent(): TOffset;
begin
  Result := ParseDbIdent(ditProcedure);
end;

function TSQLParser.ParseOpenStmt(): TOffset;
var
  Nodes: TOpenStmt.TNodes;
begin
  FillChar(Nodes, SizeOf(Nodes), 0);

  Nodes.StmtTag := ParseTag(kiOPEN);

  if (not Error) then
    Nodes.Ident := ParseDbIdent(ditCursor);

  Result := TOpenStmt.Create(Self, Nodes);
end;

function TSQLParser.ParseOptimizeTableStmt(): TOffset;
var
  Nodes: TOptimizeTableStmt.TNodes;
begin
  FillChar(Nodes, SizeOf(Nodes), 0);

  Nodes.StmtTag := ParseTag(kiOPTIMIZE);

  if (not Error) then
    if (IsTag(kiNO_WRITE_TO_BINLOG)) then
      Nodes.OptionTag := ParseTag(kiNO_WRITE_TO_BINLOG)
    else if (IsTag(kiLOCAL)) then
      Nodes.OptionTag := ParseTag(kiLOCAL);

  if (not Error) then
    Nodes.TableTag := ParseTag(kiTABLE);

  if (not Error) then
    Nodes.TablesList := ParseList(False, ParseTableIdent);

  Result := TOptimizeTableStmt.Create(Self, Nodes);
end;

function TSQLParser.ParsePartitionIdent(): TOffset;
begin
  Result := ParseDbIdent(ditPartition);
end;

function TSQLParser.ParsePositionFunc(): TOffset;
var
  Nodes: TPositionFunc.TNodes;
begin
  FillChar(Nodes, SizeOf(Nodes), 0);

  Nodes.IdentToken := ApplyCurrentToken(utFunction);

  if (not Error) then
    Nodes.OpenBracket := ParseSymbol(ttOpenBracket);

  if (not Error) then
    Nodes.SubStr := ParseExpr(False);

  if (not Error) then
    Nodes.InTag := ParseTag(kiIN);

  if (not Error) then
    Nodes.Str := ParseExpr();

  if (not Error) then
    Nodes.CloseBracket := ParseSymbol(ttCloseBracket);

  Result := TPositionFunc.Create(Self, Nodes);
end;

function TSQLParser.ParsePL_SQLStmt(): TOffset;
begin
  BeginPL_SQL();
  Result := ParseStmt();
  EndPL_SQL();
end;

function TSQLParser.ParsePrepareStmt(): TOffset;
var
  Nodes: TPrepareStmt.TNodes;
begin
  FillChar(Nodes, SizeOf(Nodes), 0);

  Nodes.StmtTag := ParseTag(kiPREPARE);

  if (not Error) then
    Nodes.StmtIdent := ParseIdent();

  if (not Error) then
    Nodes.FromTag := ParseTag(kiFROM);

  if (not Error) then
    Nodes.StmtVariable := ParseVariableIdent();

  Result := TPrepareStmt.Create(Self, Nodes);
end;

function TSQLParser.ParsePurgeStmt(): TOffset;
var
  Nodes: TPurgeStmt.TNodes;
begin
  FillChar(Nodes, SizeOf(Nodes), 0);

  Nodes.StmtTag := ParseTag(kiPURGE);

  if (not Error) then
    if (IsTag(kiBINARY)) then
      Nodes.TypeTag := ParseTag(kiBINARY)
    else if (IsTag(kiMASTER)) then
      Nodes.TypeTag := ParseTag(kiMASTER)
    else if (EndOfStmt(CurrentToken)) then
      SetError(PE_IncompleteStmt)
    else
      SetError(PE_UnexpectedToken);

  if (not Error) then
    Nodes.LogsTag := ParseTag(kiLOGS);

  if (not Error) then
    if (IsTag(kiTO)) then
      Nodes.Value := ParseValue(kiTO, vaNo, ParseString)
    else if (IsTag(kiBEFORE)) then
      Nodes.Value := ParseValue(kiBEFORE, vaNo, ParseExpr)
    else if (EndOfStmt(CurrentToken)) then
      SetError(PE_IncompleteStmt)
    else
      SetError(PE_UnexpectedToken);

  Result := TPurgeStmt.Create(Self, Nodes);
end;

function TSQLParser.ParseProcedureParam(): TOffset;
var
  Nodes: TRoutineParam.TNodes;
begin
  FillChar(Nodes, SizeOf(Nodes), 0);

  if (IsTag(kiIN)) then
    Nodes.DirektionTag := ParseTag(kiIN)
  else if (IsTag(kiOUT)) then
    Nodes.DirektionTag := ParseTag(kiOUT)
  else if (IsTag(kiINOUT)) then
    Nodes.DirektionTag := ParseTag(kiINOUT);

  if (not Error) then
    Nodes.IdentToken := ParseDbIdent(ditParameter);

  if (not Error) then
    Nodes.DatatypeNode := ParseDatatype();

  Result := TRoutineParam.Create(Self, Nodes);
end;

function TSQLParser.ParseReleaseStmt(): TOffset;
var
  Nodes: TReleaseStmt.TNodes;
begin
  FillChar(Nodes, SizeOf(Nodes), 0);

  Nodes.ReleaseTag := ParseTag(kiRELEASE, kiSAVEPOINT);

  if (not Error) then
    Nodes.Ident := ParseSavepointIdent();

  Result := TReleaseStmt.Create(Self, Nodes);
end;

function TSQLParser.ParseRenameStmt(): TOffset;
var
  Nodes: TRenameStmt.TNodes;
begin
  FillChar(Nodes, SizeOf(Nodes), 0);

  if (IsTag(kiRENAME, kiTABLE)) then
  begin
    Nodes.StmtTag := ParseTag(kiRENAME, kiTABLE);

    if (not Error) then
      Nodes.RenameList := ParseList(False, ParseRenameStmtTablePair);
  end
  else if (IsTag(kiRENAME, kiUSER)) then
  begin
    Nodes.StmtTag := ParseTag(kiRENAME, kiUSER);

    if (not Error) then
      Nodes.RenameList := ParseList(False, ParseRenameStmtUserPair);
  end
  else
    SetError(PE_UnexpectedToken, NextToken[1]);

  Result := TRenameStmt.Create(Self, Nodes)
end;

function TSQLParser.ParseRenameStmtTablePair(): TOffset;
var
  Nodes: TRenameStmt.TPair.TNodes;
begin
  FillChar(Nodes, SizeOf(Nodes), 0);

  Nodes.OrgNode := ParseTableIdent();

  if (not Error) then
    Nodes.ToTag := ParseTag(kiTO);

  if (not Error) then
    Nodes.NewNode := ParseTableIdent();

  Result := TRenameStmt.TPair.Create(Self, Nodes);
end;

function TSQLParser.ParseRenameStmtUserPair(): TOffset;
var
  Nodes: TRenameStmt.TPair.TNodes;
begin
  FillChar(Nodes, SizeOf(Nodes), 0);

  Nodes.OrgNode := ParseUserIdent();

  if (not Error) then
    Nodes.ToTag := ParseTag(kiTO);

  if (not Error) then
    Nodes.NewNode := ParseUserIdent();

  Result := TRenameStmt.TPair.Create(Self, Nodes);
end;

function TSQLParser.ParseRepairTableStmt(): TOffset;
var
  Nodes: TRepairTableStmt.TNodes;
begin
  FillChar(Nodes, SizeOf(Nodes), 0);

  Nodes.StmtTag := ParseTag(kiREPAIR);

  if (not Error) then
    if (IsTag(kiNO_WRITE_TO_BINLOG)) then
      Nodes.OptionTag := ParseTag(kiNO_WRITE_TO_BINLOG)
    else if (IsTag(kiLOCAL)) then
      Nodes.OptionTag := ParseTag(kiLOCAL);

  if (not Error) then
    Nodes.TableTag := ParseTag(kiTABLE);

  if (not Error) then
    Nodes.TablesList := ParseList(False, ParseTableIdent);

  if (not Error) then
    if (IsTag(kiQUICK)) then
      Nodes.QuickTag := ParseTag(kiQUICK);

  if (not Error) then
    if (IsTag(kiEXTENDED)) then
      Nodes.ExtendedTag := ParseTag(kiEXTENDED);

  if (not Error) then
    if (IsTag(kiUSE_FRM)) then
      Nodes.UseFrmTag := ParseTag(kiUSE_FRM);

  Result := TRepairTableStmt.Create(Self, Nodes);
end;

function TSQLParser.ParseRepeatStmt(): TOffset;
var
  Nodes: TRepeatStmt.TNodes;
begin
  FillChar(Nodes, SizeOf(Nodes), 0);

  if (not EndOfStmt(CurrentToken) and (TokenPtr(CurrentToken)^.TokenType = ttIdent)
    and not EndOfStmt(NextToken[1]) and (TokenPtr(NextToken[1])^.TokenType = ttColon)) then
    Nodes.BeginLabel := ParseBeginLabel();

  Nodes.RepeatTag := ParseTag(kiREPEAT);

  if (not Error and not IsTag(kiUNTIL)) then
    Nodes.StmtList := ParseList(False, ParsePL_SQLStmt, ttSemicolon);

  if (not Error) then
    Nodes.UntilTag := ParseTag(kiUNTIL);

  if (not Error) then
    Nodes.SearchConditionExpr := ParseExpr();

  if (not Error) then
    Nodes.EndTag := ParseTag(kiEND, kiREPEAT);

  if (not Error
    and not EndOfStmt(CurrentToken) and (TokenPtr(CurrentToken)^.TokenType = ttIdent)
    and (Nodes.BeginLabel > 0) and (NodePtr(Nodes.BeginLabel)^.NodeType = ntBeginLabel)) then
    if ((Nodes.BeginLabel = 0) or (lstrcmpi(PChar(TokenPtr(CurrentToken)^.AsString), PChar(PBeginLabel(NodePtr(Nodes.BeginLabel))^.LabelName)) <> 0)) then
      SetError(PE_UnexpectedToken)
    else
      Nodes.EndLabel := ParseEndLabel();

  Result := TRepeatStmt.Create(Self, Nodes);
end;

function TSQLParser.ParseRevokeStmt(): TOffset;
var
  Nodes: TRevokeStmt.TNodes;
begin
  FillChar(Nodes, SizeOf(Nodes), 0);

  Nodes.StmtTag := ParseTag(kiREVOKE);

  if (not Error) then
    if (not IsTag(kiALL)
      and not IsTag(kiPROXY)) then
    begin
      Nodes.PrivilegesList := ParseList(False, ParseGrantStmtPrivileg);

      if (not Error) then
        Nodes.OnTag := ParseTag(kiON);

      if (not Error) then
        if (IsTag(kiTABLE)) then
          Nodes.ObjectValue := ParseValue(kiTABLE, vaNo, ParseTableIdent)
        else if (IsTag(kiFUNCTION)) then
          Nodes.ObjectValue := ParseValue(kiFUNCTION, vaNo, ParseFunctionIdent)
        else if (IsTag(kiPROCEDURE)) then
          Nodes.ObjectValue := ParseValue(kiFUNCTION, vaNo, ParseProcedureIdent)
        else
          Nodes.ObjectValue := ParseTableIdent();

      if (not Error) then
        Nodes.FromTag := ParseTag(kiFROM);

      if (not Error) then
        Nodes.UserIdentList := ParseList(False, ParseUserIdent);
    end
    else if (IsTag(kiALL, kiPRIVILEGES)) then
    begin
      Nodes.PrivilegesList := ParseTag(kiALL, kiPRIVILEGES);

      if (not Error) then
        Nodes.CommaToken := ParseSymbol(ttComma);

      if (not Error) then
        Nodes.GrantOptionTag := ParseTag(kiGRANT, kiOPTION);

      if (not Error) then
        Nodes.UserIdentList := ParseList(False, ParseUserIdent);
    end
    else if (IsTag(kiPROXY, kiON)) then
    begin
      Nodes.PrivilegesList := ParseTag(kiPROXY);

      if (not Error) then
        Nodes.OnTag := ParseTag(kiON);

      if (not Error) then
        Nodes.OnUser := ParseUserIdent();

      if (not Error) then
        Nodes.FromTag := ParseTag(kiFROM);

      if (not Error) then
        Nodes.UserIdentList := ParseList(False, ParseUserIdent);
    end
    else if (EndOfStmt(CurrentToken)) then
      SetError(PE_IncompleteStmt)
    else
      SetError(PE_UnexpectedToken);

  Result := TRevokeStmt.Create(Self, Nodes);
end;

function TSQLParser.ParseResetStmt(): TOffset;
var
  Nodes: TResetStmt.TNodes;
begin
  FillChar(Nodes, SizeOf(Nodes), 0);

  Nodes.StmtTag := ParseTag(kiRESET);

  if (not Error) then
    Nodes.OptionList := ParseList(False, ParseResetStmtOption);

  Result := TResetStmt.Create(Self, Nodes);
end;

function TSQLParser.ParseResetStmtOption(): TOffset;
var
  Nodes: TResetStmt.TOption.TNodes;
begin
  FillChar(Nodes, SizeOf(Nodes), 0);

  if (IsTag(kiMASTER)) then
    Nodes.OptionTag := ParseTag(kiMASTER)
  else if (IsTag(kiQUERY, kiCACHE)) then
    Nodes.OptionTag := ParseTag(kiQUERY, kiCACHE)
  else if (IsTag(kiSLAVE, kiALL)) then
  begin
    Nodes.OptionTag := ParseTag(kiSLAVE, kiALL);

    if (IsTag(kiFOR, kiCHANNEL)) then
      Nodes.ChannelValue := ParseValue(WordIndices(kiFOR, kiCHANNEL), vaNo, ParseIdent);
  end
  else if (IsTag(kiSLAVE)) then
  begin
    Nodes.OptionTag := ParseTag(kiSLAVE);

    if (IsTag(kiFOR, kiCHANNEL)) then
      Nodes.ChannelValue := ParseValue(WordIndices(kiFOR, kiCHANNEL), vaNo, ParseIdent);
  end
  else if (EndOfStmt(CurrentToken)) then
    SetError(PE_IncompleteStmt)
  else
    SetError(PE_UnexpectedToken);

  Result := TResetStmt.TOption.Create(Self, Nodes);
end;

function TSQLParser.ParseResignalStmt(): TOffset;
var
  Nodes: TResignalStmt.TNodes;
begin
  FillChar(Nodes, SizeOf(Nodes), 0);

  Nodes.StmtTag := ParseTag(kiRESIGNAL);

  Result := TResignalStmt.Create(Self, Nodes);
end;

function TSQLParser.ParseReturnStmt(): TOffset;
var
  Nodes: TReturnStmt.TNodes;
begin
  Assert(InCreateFunctionStmt);

  FillChar(Nodes, SizeOf(Nodes), 0);

  Nodes.StmtTag := ParseTag(kiRETURN);

  if (not Error) then
    Nodes.Expr := ParseExpr();

  Result := TReturnStmt.Create(Self, Nodes);
end;

function TSQLParser.ParseRollbackStmt(): TOffset;
var
  Nodes: TRollbackStmt.TNodes;
begin
  FillChar(Nodes, SizeOf(Nodes), 0);

  if (IsTag(kiROLLBACK, kiWORK)) then
    Nodes.RollbackTag := ParseTag(kiROLLBACK, kiWORK)
  else
    Nodes.RollbackTag := ParseTag(kiROLLBACK);

  if (not Error) then
    if (IsTag(kiTO, kiSAVEPOINT)) then
      Nodes.ToValue := ParseValue(WordIndices(kiTO, kiSAVEPOINT), vaNo, ParseSavepointIdent)
    else if (IsTag(kiTO)) then
      Nodes.ToValue := ParseValue(kiTO, vaNo, ParseSavepointIdent)
    else
    begin
      if (IsTag(kiAND, kiNO, kiCHAIN)) then
        Nodes.ChainTag := ParseTag(kiAND, kiNO, kiCHAIN)
      else if (IsTag(kiAND, kiCHAIN)) then
        Nodes.ChainTag := ParseTag(kiAND, kiCHAIN);

      if (not Error) then
        if (IsTag(kiNO, kiRELEASE)) then
          Nodes.ReleaseTag := ParseTag(kiNO, kiRELEASE)
        else if (IsTag(kiRELEASE)) then
          Nodes.ReleaseTag := ParseTag(kiRELEASE);
    end;

  Result := TRollbackStmt.Create(Self, Nodes);
end;

function TSQLParser.ParseRoot(): TOffset;
var
  Children: Classes.TList;
  ErrorCode: Byte;
  ErrorMessage: TOffset;
  FirstTokenAll: TOffset;
  OldCurrentToken: TOffset;
  Stmt: TOffset;
begin
  if (AnsiQuotes) then
  begin
    ttIdents := [ttIdent, ttDQIdent];
    UsageTypeByTokenType[ttDQIdent] := utDbIdent;
  end
  else
    ttIdents := [ttIdent, ttMySQLIdent];
  if (AnsiQuotes) then
    ttStrings := [ttIdent, ttString]
  else
  begin
    ttStrings := [ttIdent, ttString, ttDQIdent];
    UsageTypeByTokenType[ttDQIdent] := utConst;
  end;

  if (Parse.Length = 0) then
    FirstTokenAll := 0
  else
    FirstTokenAll := Nodes.UsedSize;
  FPreviousToken := 0;
  FCurrentToken := GetParsedToken(0); // Cache for speeding


  Children := Classes.TList.Create();

  ErrorCode := PE_Success; ErrorMessage := 0;
  repeat
    OldCurrentToken := CurrentToken;

    if ((CurrentToken > 0) and (TokenPtr(CurrentToken)^.TokenType = ttSemicolon)) then
      Children.Add(Pointer(ParseSymbol(ttSemicolon)))
    else
    begin
      CompletionList.Clear();
      FErrorCode := PE_Success;
      FErrorToken := 0;

      Stmt := ParseStmt();
      Children.Add(Pointer(Stmt));

      if ((ErrorCode = PE_Success) and (Stmt > 0)) then
      begin
        ErrorCode := StmtPtr(Stmt)^.FErrorCode;
        ErrorMessage := StmtPtr(Stmt).FErrorMessage;
      end;
    end;
  until ((CurrentToken = 0) or (CurrentToken = OldCurrentToken));

  Result := TRoot.Create(Self, ErrorCode, ErrorMessage, FirstTokenAll, LastTokenAll, Children.Count, TIntegerArray(Children.List));

  Children.Free();
end;

function TSQLParser.ParseSavepointIdent(): TOffset;
begin
  Result := 0;
  if (EndOfStmt(CurrentToken)) then
    SetError(PE_IncompleteStmt)
  else if (not (TokenPtr(CurrentToken)^.TokenType in ttIdents)) then
    SetError(PE_UnexpectedToken)
  else
    Result := ApplyCurrentToken();
end;

function TSQLParser.ParseSavepointStmt(): TOffset;
var
  Nodes: TSavepointStmt.TNodes;
begin
  FillChar(Nodes, SizeOf(Nodes), 0);

  Nodes.SavepointTag := ParseTag(kiSAVEPOINT);

  if (not Error) then
    Nodes.Ident := ParseSavepointIdent();

  Result := TSavepointStmt.Create(Self, Nodes);
end;

function TSQLParser.ParseSchedule(): TOffset;
var
  Nodes: TSchedule.TNodes;
begin
  FillChar(Nodes, SizeOf(Nodes), 0);

  if (IsTag(kiAT)) then
    Nodes.AtValue := ParseValue(kiAT, vaNo, ParseExpr)
  else if (IsTag(kiEVERY)) then
  begin
    Nodes.EveryValue := ParseValue(kiEVERY, vaNo, ParseIntervalOp);

    if (not Error) then
      if (IsTag(kiSTARTS)) then
        Nodes.StartsValue := ParseValue(kiSTARTS, vaNo, ParseExpr);

    if (not Error) then
      if (IsTag(kiENDS)) then
        Nodes.EndsValue := ParseValue(kiENDS, vaNo, ParseExpr);
  end
  else if (EndOfStmt(CurrentToken)) then
    SetError(PE_IncompleteStmt)
  else
    SetError(PE_UnexpectedToken);

  Result := TSchedule.Create(Self, Nodes);
end;

function TSQLParser.ParseSecretIdent(): TOffset;
var
  Nodes: TSecretIdent.TNodes;
begin
  FillChar(Nodes, SizeOf(Nodes), 0);

  if (EndOfStmt(CurrentToken)) then
    SetError(PE_IncompleteStmt)
  else if (TokenPtr(CurrentToken)^.OperatorType <> otLess) then
    SetError(PE_UnexpectedToken)
  else
    Nodes.OpenAngleBracket := ApplyCurrentToken();

  if (not Error) then
    Nodes.ItemToken := ParseIdent();

  if (not Error) then
    if (EndOfStmt(CurrentToken)) then
      SetError(PE_IncompleteStmt)
    else if (TokenPtr(CurrentToken)^.OperatorType <> otGreater) then
      SetError(PE_UnexpectedToken)
    else
      Nodes.CloseAngleBracket := ApplyCurrentToken();

  Result := TSecretIdent.Create(Self, Nodes);
end;

function TSQLParser.ParseSelectStmt(const SubSelect: Boolean): TOffset;

  function ParseInto(): TSelectStmt.TIntoNodes;
  begin
    FillChar(Result, SizeOf(Result), 0);

    if (not EndOfStmt(NextToken[1]) and (TokenPtr(NextToken[1])^.KeywordIndex = kiOUTFILE)) then
    begin
      Result.Tag := ParseTag(kiINTO, kiOUTFILE);

      if (not Error) then
        Result.Filename := ParseString();

      if (not Error) then
        if (IsTag(kiCHARACTER, kiSET)) then
          Result.CharacterSetValue := ParseValue(WordIndices(kiCHARACTER, kiSET), vaNo, ParseCharsetIdent);

      if (not Error) then
        if (IsTag(kiFIELDS, kiTERMINATED, kiBY)) then
          Result.FieldsTerminatedByValue := ParseValue(WordIndices(kiFIELDS, kiTERMINATED, kiBY), vaNo, ParseString);

      if (not Error) then
        if (IsTag(kiOPTIONALLY, kiENCLOSED, kiBY)) then
          Result.EnclosedByValue := ParseValue(WordIndices(kiOPTIONALLY, kiENCLOSED, kiBY), vaNo, ParseString)
        else if (IsTag(kiENCLOSED, kiBY)) then
          Result.EnclosedByValue := ParseValue(WordIndices(kiENCLOSED, kiBY), vaNo, ParseString);

      if (not Error) then
        if (IsTag(kiLINES, kiTERMINATED, kiBY)) then
          Result.LinesTerminatedByValue := ParseValue(WordIndices(kiLINES, kiTERMINATED, kiBY), vaNo, ParseString);
    end
    else if (not EndOfStmt(NextToken[1]) and (TokenPtr(NextToken[1])^.KeywordIndex = kiDUMPFILE)) then
    begin
      Result.Tag := ParseTag(kiINTO, kiDUMPFILE);

      if (not Error) then
        Result.Filename := ParseString();
    end
    else
    begin
      Result.Tag := ParseTag(kiINTO);

      if (not Error) then
        Result.VariableList := ParseList(False, ParseVariableIdent);
    end;
  end;

var
  Found: Boolean;
  Nodes: TSelectStmt.TNodes;
  SubAreaNodes: TSubArea.TNodes;
  SubAreaSelectNodes: TSubAreaSelectStmt.TNodes;
begin
  if (SubSelect and IsSymbol(ttOpenBracket)) then
  begin
    FillChar(SubAreaNodes, SizeOf(SubAreaNodes), 0);

    SubAreaNodes.OpenBracket := ParseSymbol(ttOpenBracket);

    if (not Error) then
      SubAreaNodes.AreaNode := ParseSelectStmt(SubSelect);

    if (not Error) then
      SubAreaNodes.CloseBracket := ParseSymbol(ttCloseBracket);

    Result := TSubArea.Create(Self, SubAreaNodes);


    if (IsTag(kiUNION)) then
    begin
      FillChar(SubAreaSelectNodes, SizeOf(SubAreaSelectNodes), 0);

      SubAreaSelectNodes.SelectStmt1 := Result;

      SubAreaSelectNodes.UnionTag := ParseTag(kiUNION);

      if (not Error) then
        if (IsTag(kiALL)) then
          SubAreaSelectNodes.HowTag := ParseTag(kiALL)
        else if (IsTag(kiDISTINCT)) then
          SubAreaSelectNodes.HowTag := ParseTag(kiDISTINCT);

      if (not Error) then
        SubAreaSelectNodes.SelectStmt2 := ParseSelectStmt(True);

      Result := TSubAreaSelectStmt.Create(Self, SubAreaSelectNodes);
    end;
  end
  else
  begin
    FillChar(Nodes, SizeOf(Nodes), 0);

    Nodes.SelectTag := ParseTag(kiSELECT);

    Found := True;
    while (not Error and Found) do
      if ((Nodes.DistinctTag = 0) and IsTag(kiALL)) then
        Nodes.DistinctTag := ParseTag(kiALL)
      else if ((Nodes.DistinctTag = 0) and IsTag(kiDISTINCT)) then
        Nodes.DistinctTag := ParseTag(kiDISTINCT)
      else if ((Nodes.DistinctTag = 0) and IsTag(kiDISTINCTROW)) then
        Nodes.DistinctTag := ParseTag(kiDISTINCTROW)
      else if ((Nodes.HighPriorityTag = 0) and IsTag(kiHIGH_PRIORITY)) then
        Nodes.HighPriorityTag := ParseTag(kiHIGH_PRIORITY)
      else if ((Nodes.MaxStatementTime = 0) and IsTag(kiMAX_STATEMENT_TIME)) then
        Nodes.MaxStatementTime := ParseValue(kiMAX_STATEMENT_TIME, vaYes, ParseInteger)
      else if ((Nodes.StraightJoinTag = 0) and IsTag(kiSTRAIGHT_JOIN)) then
        Nodes.StraightJoinTag := ParseTag(kiSTRAIGHT_JOIN)
      else if ((Nodes.SQLSmallResultTag = 0) and IsTag(kiSQL_SMALL_RESULT)) then
        Nodes.SQLSmallResultTag := ParseTag(kiSQL_SMALL_RESULT)
      else if ((Nodes.SQLBigResultTag = 0) and IsTag(kiSQL_BIG_RESULT)) then
        Nodes.SQLBigResultTag := ParseTag(kiSQL_BIG_RESULT)
      else if ((Nodes.SQLBufferResultTag = 0) and IsTag(kiSQL_BUFFER_RESULT)) then
        Nodes.SQLBufferResultTag := ParseTag(kiSQL_BUFFER_RESULT)
      else if ((Nodes.SQLNoCacheTag = 0) and IsTag(kiSQL_CACHE)) then
        Nodes.SQLNoCacheTag := ParseTag(kiSQL_CACHE)
      else if ((Nodes.SQLNoCacheTag = 0) and IsTag(kiSQL_NO_CACHE)) then
        Nodes.SQLNoCacheTag := ParseTag(kiSQL_NO_CACHE)
      else if ((Nodes.SQLCalcFoundRowsTag = 0) and IsTag(kiSQL_CALC_FOUND_ROWS)) then
        Nodes.SQLCalcFoundRowsTag := ParseTag(kiSQL_CALC_FOUND_ROWS)
      else
        Found := False;

    if (not Error) then
      Nodes.ColumnsList := ParseList(False, ParseSelectStmtColumn);

    if (not Error) then
      if (IsTag(kiINTO)) then
        Nodes.Into1 := ParseInto();

    if (not Error) then
      if (IsTag(kiFROM)) then
      begin
        Nodes.From.Tag := ParseTag(kiFROM);
        if (not Error) then
          Nodes.From.TableReferenceList := ParseList(False, ParseSelectStmtTableEscapedReference);

        if (not Error) then
          if (IsTag(kiPARTITION)) then
          begin
            Nodes.Partition.Tag := ParseTag(kiPARTITION);

            if (not Error) then
              Nodes.Partition.Ident := ParsePartitionIdent();
          end;

        if (not Error) then
          if (IsTag(kiWHERE)) then
          begin
            Nodes.Where.Tag := ParseTag(kiWHERE);

            if (not Error) then
              Nodes.Where.Expr := ParseExpr();
          end;

        if (not Error) then
          if (IsTag(kiGROUP, kiBY)) then
          begin
            Nodes.GroupBy.Tag := ParseTag(kiGROUP, kiBY);

            if (not Error) then
              Nodes.GroupBy.List := ParseList(False, ParseSelectStmtGroup);

            if (not Error and not EndOfStmt(CurrentToken) and (TokenPtr(CurrentToken)^.KeywordIndex = kiWITH)) then
              Nodes.GroupBy.WithRollupTag := ParseTag(kiWITH, kiROLLUP);
          end;

        if (not Error) then
          if (IsTag(kiHAVING)) then
          begin
            Nodes.Having.Tag := ParseTag(kiHAVING);

            if (not Error) then
              Nodes.Having.Expr := ParseExpr();
          end;

        if (not Error) then
          if (IsTag(kiORDER, kiBY)) then
          begin
            Nodes.OrderBy.Tag := ParseTag(kiORDER, kiBY);

            if (not Error) then
              Nodes.OrderBy.Expr := ParseList(False, ParseSelectStmtOrderBy);
          end;

        if (not Error) then
          if (IsTag(kiLIMIT)) then
          begin
            Nodes.Limit.Tag := ParseTag(kiLIMIT);

            if (not Error) then
              Nodes.Limit.RowCountToken := ParseExpr();

            if (not Error) then
              if (IsSymbol(ttComma)) then
              begin
                Nodes.Limit.CommaToken := ParseSymbol(ttComma);

                if (not Error) then
                begin
                  Nodes.Limit.OffsetToken := Nodes.Limit.RowCountToken;
                  Nodes.Limit.RowCountToken := ParseExpr();
                end;
              end
              else if (IsTag(kiOFFSET)) then
              begin
                Nodes.Limit.OffsetTag := ParseTag(kiOFFSET);

                if (not Error) then
                  Nodes.Limit.OffsetToken := ParseExpr();
              end;
          end;

        if (not Error) then
          if (IsTag(kiPROCEDURE)) then
          begin
            Nodes.Proc.Tag := ParseTag(kiPROCEDURE);

            if (not Error) then
              Nodes.Proc.Ident := ParseProcedureIdent();

            if (not Error) then
              Nodes.Proc.ParamList := ParseList(True, ParseExpr);
          end;

        if (not Error) then
          if (IsTag(kiINTO)) then
            if (Nodes.Into1.Tag > 0) then
              SetError(PE_UnexpectedToken)
            else
              Nodes.Into2 := ParseInto();

        if (not Error and (Nodes.From.Tag > 0)) then
          if (IsTag(kiFOR, kiUPDATE)) then
            Nodes.ForUpdatesTag := ParseTag(kiFOR, kiUPDATE)
          else if (IsTag(kiLOCK, kiIN, kiSHARE, kiMODE)) then
            Nodes.LockInShareMode := ParseTag(kiLOCK, kiIN, kiSHARE, kiMODE);
      end;

    if (not Error) then
      if (IsTag(kiUNION)) then
      begin
        if (IsTag(kiUNION, kiALL)) then
          Nodes.Union.Tag := ParseTag(kiUNION, kiALL)
        else if (IsTag(kiUNION, kiDISTINCT)) then
          Nodes.Union.Tag := ParseTag(kiUNION, kiDISTINCT)
        else
          Nodes.Union.Tag := ParseTag(kiUNION);

        if (not Error) then
          Nodes.Union.SelectStmt := ParseSelectStmt(True);
      end;

    Result := TSelectStmt.Create(Self, Nodes);
  end;
end;

function TSQLParser.ParseSelectStmtColumn(): TOffset;
var
  Nodes: TSelectStmt.TColumn.TNodes;
begin
  FillChar(Nodes, SizeOf(Nodes), 0);

  Nodes.Expr := ParseExpr();

  if (not Error) then
    if (IsTag(kiAS)) then
    begin
      Nodes.AsTag := ParseTag(kiAS);

      if (not Error) then
        Nodes.AliasIdent := ParseAliasIdent();
    end;

  if (not Error and (Nodes.AliasIdent = 0)
    and not EndOfStmt(CurrentToken)
    and (TokenPtr(CurrentToken)^.TokenType in ttIdents + ttStrings) and (TokenPtr(CurrentToken)^.KeywordIndex < 0)) then
    Nodes.AliasIdent := ParseAliasIdent();

  Result := TSelectStmt.TColumn.Create(Self, Nodes);
end;

function TSQLParser.ParseSelectStmtGroup(): TOffset;
var
  Nodes: TSelectStmt.TGroup.TNodes;
begin
  FillChar(Nodes, SizeOf(Nodes), 0);

  Nodes.Expr := ParseExpr();

  if (not Error) then
    if (IsTag(kiASC)) then
      Nodes.DirectionTag := ParseTag(kiASC)
    else if (IsTag(kiDESC)) then
      Nodes.DirectionTag := ParseTag(kiDESC);

  Result := TSelectStmt.TGroup.Create(Self, Nodes);
end;

function TSQLParser.ParseSelectStmtOrderBy(): TOffset;
var
  Nodes: TSelectStmt.TOrder.TNodes;
begin
  FillChar(Nodes, SizeOf(Nodes), 0);

  Nodes.Expr := ParseExpr();

  if (not Error) then
    if (IsTag(kiASC)) then
      Nodes.DirectionTag := ParseTag(kiASC)
    else if (IsTag(kiDESC)) then
      Nodes.DirectionTag := ParseTag(kiDESC);

  Result := TSelectStmt.TOrder.Create(Self, Nodes);
end;

function TSQLParser.ParseSelectStmtTableFactor(): TOffset;
var
  Found: Boolean;
  IndexHintList: Classes.TList;
  ListNodes: TList.TNodes;
  Nodes: TSelectStmt.TTableFactor.TNodes;
begin
  FillChar(Nodes, SizeOf(Nodes), 0);

  Nodes.TableIdent := ParseTableIdent();

  if (not Error) then
    if (IsTag(kiPARTITION)) then
    begin
      Nodes.PartitionTag := ParseTag(kiPARTITION);

      if (not Error) then
        Nodes.Partitions := ParseList(True, ParsePartitionIdent);
    end;

  if (not Error) then
    if (IsTag(kiAS)) then
    begin
      Nodes.AsTag := ParseTag(kiAS);

      if (not Error) then
        Nodes.AliasIdent := ParseAliasIdent();
    end;

  if (not Error and (Nodes.AliasIdent = 0)
    and not EndOfStmt(CurrentToken)
    and (TokenPtr(CurrentToken)^.TokenType in ttIdents + ttStrings) and (TokenPtr(CurrentToken)^.KeywordIndex < 0)) then
    Nodes.AliasIdent := ParseAliasIdent();

  if (not Error) then
    if (IsTag(kiUSE)
      or IsTag(kiIGNORE)
      or IsTag(kiFORCE)) then
    begin
      IndexHintList := Classes.TList.Create();

      Found := True;
      while (not Error and Found) do
      begin
        Found := IsTag(kiUSE)
          or IsTag(kiIGNORE)
          or IsTag(kiFORCE);
        if (Found) then
        begin
          IndexHintList.Add(Pointer(ParseSelectStmtTableFactorIndexHint));
          if (not Error) then
          begin
            Found := IsSymbol(ttComma)
              and not EndOfStmt(NextToken[1])
              and ((TokenPtr(NextToken[1])^.KeywordIndex = kiUSE)
                or (TokenPtr(NextToken[1])^.KeywordIndex = kiIGNORE)
                or (TokenPtr(NextToken[1])^.KeywordIndex = kiFORCE));
            if (Found) then
              IndexHintList.Add(Pointer(ParseSymbol(ttComma)));
          end;
        end;
      end;

      FillChar(ListNodes, SizeOf(ListNodes), 0);
      Nodes.IndexHintList := TList.Create(Self, ListNodes, ttComma, IndexHintList.Count, TIntegerArray(IndexHintList.List));
      IndexHintList.Free();
    end;

  Result := TSelectStmt.TTableFactor.Create(Self, Nodes);
end;

function TSQLParser.ParseSelectStmtTableEscapedReference(): TOffset;
begin
  if (EndOfStmt(CurrentToken) or (TokenPtr(CurrentToken)^.TokenType <> ttOpenCurlyBracket)) then
    Result := ParseSelectStmtTableReference()
  else
    Result := ParseSelectStmtTableFactorOj();
end;

function TSQLParser.ParseSelectStmtTableFactorSelect(): TOffset;
var
  Nodes: TSelectStmt.TTableFactorSelect.TNodes;
begin
  FillChar(Nodes, SizeOf(Nodes), 0);

  Nodes.SelectStmt := ParseSelectStmt(True);

  if (not Error) then
    if (IsTag(kiAS)) then
    begin
      Nodes.AsTag := ParseTag(kiAS);

      if (not Error) then
        Nodes.AliasIdent := ParseAliasIdent();
    end;

  if (not Error and (Nodes.AliasIdent = 0)
    and not EndOfStmt(CurrentToken)
    and (TokenPtr(CurrentToken)^.TokenType in ttIdents + ttStrings) and (TokenPtr(CurrentToken)^.KeywordIndex < 0)) then
    Nodes.AliasIdent := ParseAliasIdent();

  Result := TSelectStmt.TTableFactorSelect.Create(Self, Nodes);
end;

function TSQLParser.ParseSelectStmtTableFactorIndexHint(): TOffset;
var
  Nodes: TSelectStmt.TTableFactor.TIndexHint.TNodes;
begin
  FillChar(Nodes, SizeOf(Nodes), 0);

  if (IsTag(kiUSE, kiINDEX)) then
    Nodes.HintTag := ParseTag(kiUSE, kiINDEX)
  else if (IsTag(kiUSE, kiKEY)) then
    Nodes.HintTag := ParseTag(kiUSE, kiKEY)
  else if (IsTag(kiIGNORE, kiINDEX)) then
    Nodes.HintTag := ParseTag(kiIGNORE, kiINDEX)
  else if (IsTag(kiIGNORE, kiKEY)) then
    Nodes.HintTag := ParseTag(kiIGNORE, kiKEY)
  else if (IsTag(kiFORCE, kiINDEX)) then
    Nodes.HintTag := ParseTag(kiFORCE, kiINDEX)
  else if (IsTag(kiFORCE, kiKEY)) then
    Nodes.HintTag := ParseTag(kiFORCE, kiKEY)
  else if (EndOfStmt(CurrentToken)) then
    SetError(PE_IncompleteStmt)
  else
    SetError(PE_UnexpectedToken);

  if (not Error) then
    if (IsTag(kiFOR, kiJOIN)) then
      Nodes.ForTag := ParseTag(kiFOR, kiJOIN)
    else if (IsTag(kiFOR, kiORDER, kiBY)) then
      Nodes.ForTag := ParseTag(kiFOR, kiORDER, kiBY)
    else if (IsTag(kiFOR, kiGROUP, kiBY)) then
      Nodes.ForTag := ParseTag(kiFOR, kiGROUP, kiBY);

  if (not Error) then
    Nodes.IndexList := ParseList(True, ParseKeyIdent);

  Result := TSelectStmt.TTableFactor.TIndexHint.Create(Self, Nodes);
end;

function TSQLParser.ParseSelectStmtTableFactorOj(): TOffset;
var
  Nodes: TSelectStmt.TTableFactorOj.TNodes;
begin
  FillChar(Nodes, SizeOf(Nodes), 0);

  Nodes.OpenBracket := ParseSymbol(ttOpenCurlyBracket);

  if (not Error) then
    Nodes.OjTag := ParseTag(kiOJ);

  if (not Error) then
    Nodes.TableReference := ParseSelectStmtTableReference();

  if (not Error) then
    Nodes.CloseBracket := ParseSymbol(ttCloseCurlyBracket);

  Result := TSelectStmt.TTableFactorOj.Create(Self, Nodes);
end;

function TSQLParser.ParseSelectStmtTableReference(): TOffset;

  function ParseTableFactor(): TOffset;
  begin
    if (EndOfStmt(CurrentToken) or (TokenPtr(CurrentToken)^.TokenType in ttIdents)) then
      Result := ParseSelectStmtTableFactor()
    else if ((TokenPtr(CurrentToken)^.TokenType = ttOpenBracket)
      and not EndOfStmt(NextToken[1]) and (TokenPtr(NextToken[1])^.KeywordIndex = kiSELECT)) then
      Result := ParseSelectStmtTableFactorSelect()
    else if ((TokenPtr(CurrentToken)^.TokenType = ttOpenBracket)
      and not EndOfStmt(NextToken[1]) and (TokenPtr(NextToken[1])^.TokenType = ttOpenBracket)
      and not EndOfStmt(NextToken[2]) and (TokenPtr(NextToken[2])^.KeywordIndex = kiSELECT)) then
      Result := ParseSelectStmtTableFactorSelect()
    else if (TokenPtr(CurrentToken)^.TokenType = ttOpenBracket) then
      Result := ParseList(True, ParseSelectStmtTableEscapedReference)
    else
    begin
      SetError(PE_UnexpectedToken);
      Result := 0;
    end;
  end;

var
  ChildrenCount: Integer;
  Children: array[0 .. 99] of TOffset;
  JoinNodes: TSelectStmt.TTableJoin.TNodes;
  JoinType: TJoinType;
  Nodes: TList.TNodes;
begin
  FillChar(Nodes, SizeOf(Nodes), 0);

  ChildrenCount := 1;
  Children[0] := ParseTableFactor();

  while (not Error and not EndOfStmt(CurrentToken)
    and (IsTag(kiINNER)
      or IsTag(kiCROSS)
      or IsTag(kiJOIN)
      or IsTag(kiSTRAIGHT_JOIN)
      or IsTag(kiLEFT)
      or IsTag(kiRIGHT)
      or IsTag(kiNATURAL))) do
  begin
    if (ChildrenCount = Length(Children)) then
      raise Exception.Create(SArgumentOutOfRange);

    FillChar(JoinNodes, SizeOf(JoinNodes), 0);

    JoinType := jtUnknown;
    if (IsTag(kiINNER, kiJOIN)) then
    begin
      JoinNodes.JoinTag := ParseTag(kiINNER, kiJOIN);
      JoinType := jtInner
    end
    else if (IsTag(kiCROSS, kiJOIN)) then
    begin
      JoinNodes.JoinTag := ParseTag(kiCROSS, kiJOIN);
      JoinType := jtCross;
    end
    else if (IsTag(kiJOIN)) then
    begin
      JoinNodes.JoinTag := ParseTag(kiJOIN);
      JoinType := jtInner;
    end
    else if (IsTag(kiSTRAIGHT_JOIN)) then
    begin
      JoinNodes.JoinTag := ParseTag(kiSTRAIGHT_JOIN);
      JoinType := jtStraight;
    end
    else if (IsTag(kiLEFT, kiJOIN)) then
    begin
      JoinNodes.JoinTag := ParseTag(kiLEFT, kiJOIN);
      JoinType := jtLeft;
    end
    else if (IsTag(kiLEFT, kiOUTER, kiJOIN)) then
    begin
      JoinNodes.JoinTag := ParseTag(kiLEFT, kiOUTER, kiJOIN);
      JoinType := jtLeft;
    end
    else if (IsTag(kiRIGHT, kiJOIN)) then
    begin
      JoinNodes.JoinTag := ParseTag(kiRIGHT, kiJOIN);
      JoinType := jtRight;
    end
    else if (IsTag(kiRIGHT, kiOUTER, kiJOIN)) then
    begin
      JoinNodes.JoinTag := ParseTag(kiRIGHT, kiOUTER, kiJOIN);
      JoinType := jtRight;
    end
    else if (IsTag(kiNATURAL, kiJOIN)) then
    begin
      JoinNodes.JoinTag := ParseTag(kiNATURAL, kiJOIN);
      JoinType := jtEqui;
    end
    else if (IsTag(kiNATURAL, kiLEFT, kiJOIN)) then
    begin
      JoinNodes.JoinTag := ParseTag(kiNATURAL, kiLEFT, kiJOIN);
      JoinType := jtNaturalLeft;
    end
    else if (IsTag(kiNATURAL, kiLEFT, kiOUTER, kiJOIN)) then
    begin
      JoinNodes.JoinTag := ParseTag(kiNATURAL, kiLEFT, kiOUTER, kiJOIN);
      JoinType := jtNaturalLeft;
    end
    else if (IsTag(kiNATURAL, kiRIGHT, kiJOIN)) then
    begin
      JoinNodes.JoinTag := ParseTag(kiNATURAL, kiRIGHT, kiJOIN);
      JoinType := jtNaturalRight;
    end
    else if (IsTag(kiNATURAL, kiRIGHT, kiOUTER, kiJOIN)) then
    begin
      JoinNodes.JoinTag := ParseTag(kiNATURAL, kiRIGHT, kiOUTER, kiJOIN);
      JoinType := jtNaturalRight;
    end
    else if (EndOfStmt(CurrentToken)) then
      SetError(PE_IncompleteStmt)
    else
      SetError(PE_UnexpectedToken);

    if (not Error) then
      if (not (JoinType in [jtLeft, jtRight])) then
      begin
        JoinNodes.RightTable := ParseTableFactor();

        if (not Error) then
          if ((JoinType in [jtStraight]) and IsTag(kiON)) then
          begin
            JoinNodes.OnTag := ParseTag(kiON);
            if (not Error) then
              JoinNodes.Condition := ParseExpr();
          end
          else if (JoinType in [jtInner, jtCross]) then
            if (IsTag(kiON)) then
            begin
              JoinNodes.OnTag := ParseTag(kiON);
              if (not Error) then
                JoinNodes.Condition := ParseExpr();
            end
            else if (IsTag(kiUSING)) then
            begin
              JoinNodes.OnTag := ParseTag(kiUSING);
              if (not Error) then
                JoinNodes.Condition := ParseList(True, ParseFieldIdent);
            end;
      end
      else
      begin
        JoinNodes.RightTable := ParseSelectStmtTableReference();

        if (not Error and not (JoinType in [jtNaturalLeft, jtNaturalRight])) then
          if (IsTag(kiON)) then
          begin
            JoinNodes.OnTag := ParseTag(kiON);
            if (not Error) then
              if (EndOfStmt(CurrentToken)) then
                SetError(PE_IncompleteStmt)
              else
                JoinNodes.Condition := ParseExpr();
          end
          else if (IsTag(kiUSING)) then
          begin
            JoinNodes.OnTag := ParseTag(kiUSING);

            if (not Error) then
              JoinNodes.Condition := ParseList(True, ParseFieldIdent);
          end;
      end;

    Children[ChildrenCount] := TSelectStmt.TTableJoin.Create(Self, JoinType, JoinNodes);
    Inc(ChildrenCount);
  end;

  if (Error) then
    Result := 0
  else
    Result := TList.Create(Self, Nodes, ttUnknown, ChildrenCount, Children);
end;

function TSQLParser.ParseSetNamesStmt(): TOffset;
var
  Nodes: TSetNamesStmt.TNodes;
begin
  FillChar(Nodes, SizeOf(Nodes), 0);

  if (IsTag(kiSET, kiNAMES)) then
    Nodes.StmtTag := ParseTag(kiSET, kiNAMES)
  else if (IsTag(kiSET, kiCHARACTER, kiSET)) then
    Nodes.StmtTag := ParseTag(kiSET, kiCHARACTER, kiSET)
  else if (IsTag(kiSET, kiCHARSET)) then
    Nodes.StmtTag := ParseTag(kiSET, kiCHARSET)
  else
    SetError(PE_Unknown);

  if (not Error) then
    Nodes.CharsetValue := ParseCharsetIdent();

  Result := TSetNamesStmt.Create(Self, Nodes);
end;

function TSQLParser.ParseSetPasswordStmt(): TOffset;
var
  Nodes: TSetPasswordStmt.TNodes;
begin
  FillChar(Nodes, SizeOf(Nodes), 0);

  Nodes.StmtTag := ParseTag(kiSET, kiPASSWORD);

  if (not Error) then
    if (IsTag(kiFOR)) then
      Nodes.ForValue := ParseValue(kiFOR, vaNo, ParseUserIdent);

  if (not Error) then
    if (EndOfStmt(CurrentToken)) then
      SetError(PE_IncompleteStmt)
    else if (TokenPtr(CurrentToken)^.OperatorType <> otEqual) then
      SetError(PE_UnexpectedToken)
    else
    begin
      TokenPtr(CurrentToken)^.FOperatorType := otAssign;
      Nodes.AssignToken := ApplyCurrentToken();
    end;

  if (not Error) then
    Nodes.PasswordExpr := ParseExpr();

  Result := TSetPasswordStmt.Create(Self, Nodes);
end;

function TSQLParser.ParseSetStmt(): TOffset;
var
  Nodes: TSetStmt.TNodes;
begin
  FillChar(Nodes, SizeOf(Nodes), 0);

  Nodes.SetTag := ParseTag(kiSET);

  if (not Error) then
    if (IsTag(kiGLOBAL)) then
      Nodes.ScopeTag := ParseTag(kiGLOBAL)
    else if (IsTag(kiSESSION)) then
      Nodes.ScopeTag := ParseTag(kiSESSION);

  if (not Error) then
    Nodes.AssignmentList := ParseList(False, ParseSetStmtAssignment);

  Result := TSetStmt.Create(Self, Nodes);
end;

function TSQLParser.ParseSetStmtAssignment(): TOffset;
var
  Nodes: TSetStmt.TAssignment.TNodes;
begin
  FillChar(Nodes, SizeOf(Nodes), 0);

  if (not Error) then
    if (IsTag(kiGLOBAL)) then
      Nodes.ScopeTag := ParseTag(kiGLOBAL)
    else if (IsTag(kiSESSION)) then
      Nodes.ScopeTag := ParseTag(kiSESSION);

  if (not Error) then
    Nodes.Variable := ParseVariableIdent();

  if (not Error) then
    if (EndOfStmt(CurrentToken)) then
      SetError(PE_IncompleteStmt)
    else if (TokenPtr(CurrentToken)^.OperatorType = otEqual) then
    begin
      TokenPtr(CurrentToken)^.FOperatorType := otAssign;
      Nodes.AssignToken := ApplyCurrentToken();
    end
    else if (TokenPtr(CurrentToken)^.OperatorType = otAssign2) then
      Nodes.AssignToken := ApplyCurrentToken()
    else
      SetError(PE_UnexpectedToken);

  if (not Error) then
    Nodes.ValueExpr := ParseExpr();

  Result := TSetStmt.TAssignment.Create(Self, Nodes);
end;

function TSQLParser.ParseSetTransactionStmt(): TOffset;
var
  Nodes: TSetTransactionStmt.TNodes;
begin
  FillChar(Nodes, SizeOf(Nodes), 0);

  Nodes.SetTag := ParseTag(kiSET);

  if (not Error) then
    if (IsTag(kiGLOBAL)) then
      Nodes.ScopeTag := ParseTag(kiGLOBAL)
    else if (IsTag(kiSESSION)) then
      Nodes.ScopeTag := ParseTag(kiSESSION);

  if (not Error) then
    Nodes.TransactionTag := ParseTag(kiTRANSACTION);

  if (not Error) then
    Nodes.CharacteristicList := ParseList(False, ParseSetTransactionStmtCharacterisic);

  Result := TSetTransactionStmt.Create(Self, Nodes);
end;

function TSQLParser.ParseSetTransactionStmtCharacterisic(): TOffset;
var
  Nodes: TSetTransactionStmt.TCharacteristic.TNodes;
begin
  FillChar(Nodes, SizeOf(Nodes), 0);

  if (IsTag(kiISOLATION, kiLEVEL)) then
  begin
    Nodes.KindTag := ParseTag(kiISOLATION, kiLEVEL);

    if (not Error) then
      if (IsTag(kiREPEATABLE, kiREAD)) then
        Nodes.LevelTag := ParseTag(kiREPEATABLE, kiREAD)
      else if (IsTag(kiREAD, kiCOMMITTED)) then
        Nodes.LevelTag := ParseTag(kiREAD, kiCOMMITTED)
      else if (IsTag(kiREAD, kiUNCOMMITTED)) then
        Nodes.LevelTag := ParseTag(kiREAD, kiUNCOMMITTED)
      else if (IsTag(kiSERIALIZABLE)) then
        Nodes.LevelTag := ParseTag(kiSERIALIZABLE)
      else if (EndOfStmt(CurrentToken)) then
        SetError(PE_IncompleteStmt)
      else
        SetError(PE_UnexpectedToken);
  end
  else if (IsTag(kiREAD, kiWRITE)) then
    Nodes.KindTag := ParseTag(kiREAD, kiWRITE)
  else if (IsTag(kiREAD, kiWRITE)) then
    Nodes.KindTag := ParseTag(kiREAD, kiONLY)
  else if (EndOfStmt(CurrentToken)) then
    SetError(PE_IncompleteStmt)
  else
    SetError(PE_UnexpectedToken);

  Result := TSetTransactionStmt.TCharacteristic.Create(Self, Nodes);
end;

function TSQLParser.ParseShowAuthorsStmt(): TOffset;
var
  Nodes: TShowAuthorsStmt.TNodes;
begin
  FillChar(Nodes, SizeOf(Nodes), 0);

  Nodes.StmtTag := ParseTag(kiSHOW, kiAUTHORS);

  Result := TShowAuthorsStmt.Create(Self, Nodes);
end;

function TSQLParser.ParseShowBinaryLogsStmt(): TOffset;
var
  Nodes: TShowBinaryLogsStmt.TNodes;
begin
  FillChar(Nodes, SizeOf(Nodes), 0);

  Nodes.StmtTag := ParseTag(kiSHOW, kiBINARY, kiLOGS);

  Result := TShowBinaryLogsStmt.Create(Self, Nodes);
end;

function TSQLParser.ParseShowBinlogEventsStmt(): TOffset;
var
  Nodes: TShowBinlogEventsStmt.TNodes;
begin
  FillChar(Nodes, SizeOf(Nodes), 0);

  Nodes.StmtTag := ParseTag(kiSHOW, kiBINLOG, kiEVENTS);

  if (not Error) then
    if (IsTag(kiIN)) then
      Nodes.InValue := ParseValue(kiIN, vaNo, ParseString);

  if (not Error) then
    if (IsTag(kiFROM)) then
      Nodes.FromValue := ParseValue(kiFROM, vaNo, ParseInteger);

  if (not Error) then
    if (IsTag(kiLIMIT)) then
    begin
      Nodes.Limit.Tag := ParseTag(kiLIMIT);

      if (not Error) then
      begin
        Nodes.Limit.RowCountToken := ParseInteger();

        if (not Error) then
          if (IsSymbol(ttComma)) then
          begin
            Nodes.Limit.CommaToken := ParseSymbol(ttComma);

            if (not Error) then
            begin
              Nodes.Limit.OffsetToken := Nodes.Limit.RowCountToken;
              Nodes.Limit.RowCountToken := ParseInteger();
            end;
          end;
      end;
    end;

  Result := TShowBinlogEventsStmt.Create(Self, Nodes);
end;

function TSQLParser.ParseShowCharacterSetStmt(): TOffset;
var
  Nodes: TShowCharacterSetStmt.TNodes;
begin
  FillChar(Nodes, SizeOf(Nodes), 0);

  Nodes.StmtTag := ParseTag(kiSHOW, kiCHARACTER, kiSET);

  if (not Error) then
    if (IsTag(kiLIKE)) then
      Nodes.LikeValue := ParseValue(kiLIKE, vaNo, ParseString)
    else if (IsTag(kiWHERE)) then
      Nodes.WhereValue := ParseValue(kiWHERE, vaNo, ParseExpr);

  Result := TShowCharacterSetStmt.Create(Self, Nodes);
end;

function TSQLParser.ParseShowCollationStmt(): TOffset;
var
  Nodes: TShowCollationStmt.TNodes;
begin
  FillChar(Nodes, SizeOf(Nodes), 0);

  Nodes.StmtTag := ParseTag(kiSHOW, kiCOLLATION);

  if (not Error) then
    if (IsTag(kiLIKE)) then
      Nodes.LikeValue := ParseValue(kiLIKE, vaNo, ParseString)
    else if (IsTag(kiWHERE)) then
      Nodes.WhereValue := ParseValue(kiWHERE, vaNo, ParseExpr);

  Result := TShowCollationStmt.Create(Self, Nodes);
end;

function TSQLParser.ParseShowColumnsStmt(): TOffset;
var
  Nodes: TShowColumnsStmt.TNodes;
begin
  FillChar(Nodes, SizeOf(Nodes), 0);

  Nodes.StmtTag := ParseTag(kiSHOW);

  if (not Error) then
    if (IsTag(kiFULL)) then
      Nodes.FullTag := ParseTag(kiFULL);

  if (not Error) then
    Nodes.ColumnsTag := ParseTag(kiCOLUMNS);

  if (not Error) then
    if (IsTag(kiFROM)) then
      Nodes.FromTableTag := ParseTag(kiFROM)
    else
      Nodes.FromTableTag := ParseTag(kiIN);

  if (not Error) then
    Nodes.TableIdent := ParseTableIdent();

  if (not Error) then
    if (IsTag(kiFROM)) then
      Nodes.FromDatabaseValue := ParseValue(kiFROM, vaNo, ParseDatabaseIdent)
    else if (IsTag(kiIN)) then
      Nodes.FromDatabaseValue := ParseValue(kiFROM, vaNo, ParseDatabaseIdent);

  if (not Error) then
    if (IsTag(kiLIKE)) then
      Nodes.LikeValue := ParseValue(kiLIKE, vaNo, ParseString)
    else if (IsTag(kiWHERE)) then
      Nodes.WhereValue := ParseValue(kiWHERE, vaNo, ParseExpr);

  Result := TShowColumnsStmt.Create(Self, Nodes);
end;

function TSQLParser.ParseShowContributorsStmt(): TOffset;
var
  Nodes: TShowContributorsStmt.TNodes;
begin
  FillChar(Nodes, SizeOf(Nodes), 0);

  Nodes.StmtTag := ParseTag(kiSHOW, kiCONTRIBUTORS);

  Result := TShowContributorsStmt.Create(Self, Nodes);
end;

function TSQLParser.ParseShowCountErrorsStmt(): TOffset;
var
  Nodes: TShowCountErrorsStmt.TNodes;
begin
  FillChar(Nodes, SizeOf(Nodes), 0);

  Nodes.StmtTag := ParseTag(kiSHOW);

  if (not Error) then
    if (EndOfStmt(NextToken[3])) then
      SetError(PE_IncompleteStmt)
    else if (TokenPtr(CurrentToken)^.TokenType <> ttIdent) then
      SetError(PE_UnexpectedToken)
    else if (TokenPtr(NextToken[1])^.TokenType <> ttOpenBracket) then
      SetError(PE_UnexpectedToken, NextToken[1])
    else if ((TokenPtr(NextToken[2])^.TokenType <> ttOperator) or (TokenPtr(NextToken[2])^.OperatorType <> otMulti)) then
      SetError(PE_UnexpectedToken, NextToken[2])
    else if (TokenPtr(NextToken[3])^.TokenType <> ttCloseBracket) then
      SetError(PE_UnexpectedToken, NextToken[3])
    else
      Nodes.CountFunctionCall := ParseFunctionCall();

  Result := TShowCountErrorsStmt.Create(Self, Nodes);
end;

function TSQLParser.ParseShowCountWarningsStmt(): TOffset;
var
  Nodes: TShowCountWarningsStmt.TNodes;
begin
  FillChar(Nodes, SizeOf(Nodes), 0);

  Nodes.StmtTag := ParseTag(kiSHOW);

  if (not Error) then
    if (EndOfStmt(NextToken[3])) then
      SetError(PE_IncompleteStmt)
    else if (TokenPtr(CurrentToken)^.TokenType <> ttIdent) then
      SetError(PE_UnexpectedToken)
    else if (TokenPtr(NextToken[1])^.TokenType <> ttOpenBracket) then
      SetError(PE_UnexpectedToken, NextToken[1])
    else if ((TokenPtr(NextToken[2])^.TokenType <> ttOperator) or (TokenPtr(NextToken[2])^.OperatorType <> otMulti)) then
      SetError(PE_UnexpectedToken, NextToken[2])
    else if (TokenPtr(NextToken[3])^.TokenType <> ttCloseBracket) then
      SetError(PE_UnexpectedToken, NextToken[3])
    else
      Nodes.CountFunctionCall := ParseFunctionCall();

  Result := TShowCountWarningsStmt.Create(Self, Nodes);
end;

function TSQLParser.ParseShowCreateDatabaseStmt(): TOffset;
var
  Nodes: TShowCreateDatabaseStmt.TNodes;
begin
  FillChar(Nodes, SizeOf(Nodes), 0);

  if (IsTag(kiSCHEMA)) then
    Nodes.StmtTag := ParseTag(kiSHOW, kiCREATE, kiSCHEMA)
  else
    Nodes.StmtTag := ParseTag(kiSHOW, kiCREATE, kiDATABASE);

  if (not Error) then
    if (IsTag(kiIF, kiNOT, kiEXISTS)) then
      Nodes.IfNotExistsTag := ParseTag(kiIF, kiNOT, kiEXISTS);

  if (not Error) then
    Nodes.Ident := ParseDatabaseIdent();

  Result := TShowCreateDatabaseStmt.Create(Self, Nodes);
end;

function TSQLParser.ParseShowCreateEventStmt(): TOffset;
var
  Nodes: TShowCreateEventStmt.TNodes;
begin
  FillChar(Nodes, SizeOf(Nodes), 0);

  Nodes.StmtTag := ParseTag(kiSHOW, kiCREATE, kiEVENT);

  if (not Error) then
    Nodes.Ident := ParseDbIdent(ditEvent);

  Result := TShowCreateEventStmt.Create(Self, Nodes);
end;

function TSQLParser.ParseShowCreateFunctionStmt(): TOffset;
var
  Nodes: TShowCreateFunctionStmt.TNodes;
begin
  FillChar(Nodes, SizeOf(Nodes), 0);

  Nodes.StmtTag := ParseTag(kiSHOW, kiCREATE, kiFUNCTION);

  if (not Error) then
    Nodes.Ident := ParseFunctionIdent();

  Result := TShowCreateFunctionStmt.Create(Self, Nodes);
end;

function TSQLParser.ParseShowCreateProcedureStmt(): TOffset;
var
  Nodes: TShowCreateProcedureStmt.TNodes;
begin
  FillChar(Nodes, SizeOf(Nodes), 0);

  Nodes.StmtTag := ParseTag(kiSHOW, kiCREATE, kiPROCEDURE);

  if (not Error) then
    Nodes.Ident := ParseProcedureIdent();

  Result := TShowCreateProcedureStmt.Create(Self, Nodes);
end;

function TSQLParser.ParseShowCreateTableStmt(): TOffset;
var
  Nodes: TShowCreateTableStmt.TNodes;
begin
  FillChar(Nodes, SizeOf(Nodes), 0);

  Nodes.StmtTag := ParseTag(kiSHOW, kiCREATE, kiTABLE);

  if (not Error) then
    Nodes.Ident := ParseTableIdent();

  Result := TShowCreateTableStmt.Create(Self, Nodes);
end;

function TSQLParser.ParseShowCreateTriggerStmt(): TOffset;
var
  Nodes: TShowCreateTriggerStmt.TNodes;
begin
  FillChar(Nodes, SizeOf(Nodes), 0);

  Nodes.StmtTag := ParseTag(kiSHOW, kiCREATE, kiTRIGGER);

  if (not Error) then
    Nodes.Ident := ParseTriggerIdent();

  Result := TShowCreateTriggerStmt.Create(Self, Nodes);
end;

function TSQLParser.ParseShowCreateUserStmt(): TOffset;
var
  Nodes: TShowCreateUserStmt.TNodes;
begin
  FillChar(Nodes, SizeOf(Nodes), 0);

  Nodes.StmtTag := ParseTag(kiSHOW, kiCREATE, kiUSER);

  if (not Error) then
    Nodes.User := ParseUserIdent();

  Result := TShowCreateUserStmt.Create(Self, Nodes);
end;

function TSQLParser.ParseShowCreateViewStmt(): TOffset;
var
  Nodes: TShowCreateViewStmt.TNodes;
begin
  FillChar(Nodes, SizeOf(Nodes), 0);

  Nodes.StmtTag := ParseTag(kiSHOW, kiCREATE, kiVIEW);

  if (not Error) then
    Nodes.Ident := ParseTableIdent();

  Result := TShowCreateViewStmt.Create(Self, Nodes);
end;

function TSQLParser.ParseShowDatabasesStmt(): TOffset;
var
  Nodes: TShowDatabasesStmt.TNodes;
begin
  FillChar(Nodes, SizeOf(Nodes), 0);

  Nodes.StmtTag := ParseTag(kiSHOW, kiDATABASES);

  if (not Error) then
    if (IsTag(kiLIKE)) then
      Nodes.LikeValue := ParseValue(kiLIKE, vaNo, ParseString)
    else if (IsTag(kiWHERE)) then
      Nodes.WhereValue := ParseValue(kiWHERE, vaNo, ParseExpr);

  Result := TShowDatabasesStmt.Create(Self, Nodes);
end;

function TSQLParser.ParseShowEngineStmt(): TOffset;
var
  Nodes: TShowEngineStmt.TNodes;
begin
  FillChar(Nodes, SizeOf(Nodes), 0);

  Nodes.StmtTag := ParseTag(kiSHOW, kiENGINE);

  if (not Error) then
    Nodes.Ident := ParseIdent();

  if (not Error) then
    if (IsTag(kiSTATUS)) then
      Nodes.KindTag := ParseTag(kiSTATUS)
    else if (IsTag(kiMUTEX)) then
      Nodes.KindTag := ParseTag(kiMUTEX)
    else if (EndOfStmt(CurrentToken)) then
      SetError(PE_IncompleteStmt)
    else
      SetError(PE_UnexpectedToken);

  Result := TShowEngineStmt.Create(Self, Nodes);
end;

function TSQLParser.ParseShowEnginesStmt(): TOffset;
var
  Nodes: TShowEnginesStmt.TNodes;
begin
  FillChar(Nodes, SizeOf(Nodes), 0);

  if (TokenPtr(NextToken[1])^.KeywordIndex = kiSTORAGE) then
    Nodes.StmtTag := ParseTag(kiSHOW, kiSTORAGE, kiENGINES)
  else
    Nodes.StmtTag := ParseTag(kiSHOW, kiENGINES);

  Result := TShowEnginesStmt.Create(Self, Nodes);
end;

function TSQLParser.ParseShowErrorsStmt(): TOffset;
var
  Nodes: TShowErrorsStmt.TNodes;
begin
  FillChar(Nodes, SizeOf(Nodes), 0);

  Nodes.StmtTag := ParseTag(kiSHOW, kiERRORS);

  if (not Error) then
    if (IsTag(kiLIMIT)) then
    begin
      Nodes.Limit.Tag := ParseTag(kiLIMIT);

      if (not Error) then
      begin
        Nodes.Limit.RowCountToken := ParseInteger();

        if (not Error) then
          if (IsSymbol(ttComma)) then
          begin
            Nodes.Limit.CommaToken := ParseSymbol(ttComma);

            if (not Error) then
            begin
              Nodes.Limit.OffsetToken := Nodes.Limit.RowCountToken;
              Nodes.Limit.RowCountToken := ParseInteger();
            end;
          end;
      end;
    end;

  Result := TShowErrorsStmt.Create(Self, Nodes);
end;

function TSQLParser.ParseShowEventsStmt(): TOffset;
var
  Nodes: TShowEventsStmt.TNodes;
begin
  FillChar(Nodes, SizeOf(Nodes), 0);

  Nodes.StmtTag := ParseTag(kiSHOW, kiERRORS);

  if (not Error) then
    if (IsTag(kiFROM)) then
      Nodes.FromValue := ParseValue(kiFROM, vaNo, ParseDatabaseIdent)
    else if (IsTag(kiIN)) then
      Nodes.FromValue := ParseValue(kiIN, vaNo, ParseDatabaseIdent);

  if (not Error) then
    if (IsTag(kiLIKE)) then
      Nodes.LikeValue := ParseValue(kiLIKE, vaNo, ParseString)
    else if (IsTag(kiWHERE)) then
      Nodes.WhereValue := ParseValue(kiWHERE, vaNo, ParseExpr);

  Result := TShowEventsStmt.Create(Self, Nodes);
end;

function TSQLParser.ParseShowFunctionCodeStmt(): TOffset;
var
  Nodes: TShowFunctionCodeStmt.TNodes;
begin
  FillChar(Nodes, SizeOf(Nodes), 0);

  Nodes.StmtTag := ParseTag(kiSHOW, kiFUNCTION, kiCODE);

  Result := TShowFunctionCodeStmt.Create(Self, Nodes);
end;

function TSQLParser.ParseShowFunctionStatusStmt(): TOffset;
var
  Nodes: TShowFunctionStatusStmt.TNodes;
begin
  FillChar(Nodes, SizeOf(Nodes), 0);

  Nodes.StmtTag := ParseTag(kiSHOW, kiFUNCTION, kiSTATUS);

  Result := TShowFunctionStatusStmt.Create(Self, Nodes);
end;

function TSQLParser.ParseShowGrantsStmt(): TOffset;
var
  Nodes: TShowGrantsStmt.TNodes;
begin
  FillChar(Nodes, SizeOf(Nodes), 0);

  Nodes.StmtTag := ParseTag(kiSHOW, kiGRANTS);

  if (not Error) then
    if (IsTag(kiFOR)) then
      Nodes.ForValue := ParseValue(kiFOR, vaNo, ParseUserIdent);

  Result := TShowGrantsStmt.Create(Self, Nodes);
end;

function TSQLParser.ParseShowIndexStmt(): TOffset;
var
  Nodes: TShowIndexStmt.TNodes;
begin
  FillChar(Nodes, SizeOf(Nodes), 0);

  if (IsTag(kiSHOW, kiINDEX)) then
    Nodes.StmtTag := ParseTag(kiSHOW, kiINDEX)
  else if (IsTag(kiSHOW, kiINDEXES)) then
    Nodes.StmtTag := ParseTag(kiSHOW, kiINDEXES)
  else
    Nodes.StmtTag := ParseTag(kiSHOW, kiKEYS);

  if (not Error) then
    if (IsTag(kiFROM)) then
      Nodes.FromTableValue := ParseValue(kiFROM, vaNo, ParseTableIdent)
    else
      Nodes.FromTableValue := ParseValue(kiIN, vaNo, ParseTableIdent);

  if (not Error) then
    if (IsTag(kiFROM)) then
      Nodes.FromDatabaseValue := ParseValue(kiFROM, vaNo, ParseDatabaseIdent)
    else if (IsTag(kiIN)) then
      Nodes.FromDatabaseValue := ParseValue(kiIN, vaNo, ParseDatabaseIdent);

  if (not Error) then
    if (IsTag(kiWHERE)) then
      Nodes.WhereValue := ParseValue(kiWHERE, vaNo, ParseExpr);

  Result := TShowIndexStmt.Create(Self, Nodes);
end;

function TSQLParser.ParseShowMasterStatusStmt(): TOffset;
var
  Nodes: TShowMasterStatusStmt.TNodes;
begin
  FillChar(Nodes, SizeOf(Nodes), 0);

  Nodes.StmtTag := ParseTag(kiSHOW, kiMASTER, kiSTATUS);

  Result := TShowMasterStatusStmt.Create(Self, Nodes);
end;

function TSQLParser.ParseShowOpenTablesStmt(): TOffset;
var
  Nodes: TShowOpenTablesStmt.TNodes;
begin
  FillChar(Nodes, SizeOf(Nodes), 0);

  Nodes.StmtTag := ParseTag(kiSHOW, kiOPEN, kiTABLES);

  if (not Error) then
    if (IsTag(kiFROM)) then
      Nodes.FromDatabaseValue := ParseValue(kiFROM, vaNo, ParseDatabaseIdent)
    else if (IsTag(kiIN)) then
      Nodes.FromDatabaseValue := ParseValue(kiIN, vaNo, ParseDatabaseIdent);

  if (not Error) then
    if (IsTag(kiLIKE)) then
      Nodes.LikeValue := ParseValue(kiLIKE, vaNo, ParseString)
    else if (IsTag(kiWHERE)) then
      Nodes.WhereValue := ParseValue(kiWHERE, vaNo, ParseExpr);

  Result := TShowOpenTablesStmt.Create(Self, Nodes);
end;

function TSQLParser.ParseShowPluginsStmt(): TOffset;
var
  Nodes: TShowPluginsStmt.TNodes;
begin
  FillChar(Nodes, SizeOf(Nodes), 0);

  Nodes.StmtTag := ParseTag(kiSHOW, kiPLUGINS);

  Result := TShowPluginsStmt.Create(Self, Nodes);
end;

function TSQLParser.ParseShowPrivilegesStmt(): TOffset;
var
  Nodes: TShowPrivilegesStmt.TNodes;
begin
  FillChar(Nodes, SizeOf(Nodes), 0);

  Nodes.StmtTag := ParseTag(kiSHOW, kiPRIVILEGES);

  Result := TShowPrivilegesStmt.Create(Self, Nodes);
end;

function TSQLParser.ParseShowProcedureCodeStmt(): TOffset;
var
  Nodes: TShowProcedureCodeStmt.TNodes;
begin
  FillChar(Nodes, SizeOf(Nodes), 0);

  Nodes.StmtTag := ParseTag(kiSHOW, kiPROCEDURE, kiCODE);

  Result := TShowProcedureCodeStmt.Create(Self, Nodes);
end;

function TSQLParser.ParseShowProcedureStatusStmt(): TOffset;
var
  Nodes: TShowProcedureStatusStmt.TNodes;
begin
  FillChar(Nodes, SizeOf(Nodes), 0);

  Nodes.StmtTag := ParseTag(kiSHOW, kiPROCEDURE, kiSTATUS);

  if (not Error) then
    if (IsTag(kiLIKE)) then
      Nodes.LikeValue := ParseValue(kiLIKE, vaNo, ParseString)
    else if (IsTag(kiWHERE)) then
      Nodes.WhereValue := ParseValue(kiWHERE, vaNo, ParseExpr);

  Result := TShowProcedureStatusStmt.Create(Self, Nodes);
end;

function TSQLParser.ParseShowProcessListStmt(): TOffset;
var
  Nodes: TShowProcessListStmt.TNodes;
begin
  FillChar(Nodes, SizeOf(Nodes), 0);

  if (TokenPtr(NextToken[1])^.KeywordIndex = kiFULL) then
    Nodes.StmtTag := ParseTag(kiSHOW, kiFULL, kiPROCESSLIST)
  else
    Nodes.StmtTag := ParseTag(kiSHOW, kiPROCESSLIST);

  Result := TShowProcessListStmt.Create(Self, Nodes);
end;

function TSQLParser.ParseShowProfileStmt(): TOffset;
var
  Nodes: TShowProfileStmt.TNodes;
begin
  FillChar(Nodes, SizeOf(Nodes), 0);

  Nodes.StmtTag := ParseTag(kiSHOW, kiPROFILE);

  if (not Error) then
    Nodes.TypeList := ParseList(False, ParseShowProfileStmtType);

  if (not Error) then
    if (IsTag(kiFOR, kiQUERY)) then
      Nodes.ForQueryValue := ParseValue(WordIndices(kiFOR, kiQUERY), vaNo, ParseInteger);

  if (not Error) then
    if (IsTag(kiLIMIT)) then
    begin
      Nodes.LimitValue := ParseValue(kiLIMIT, vaNo, ParseInteger);

      if (not Error) then
        if (IsTag(kiOFFSET)) then
          Nodes.LimitValue := ParseValue(kiOFFSET, vaNo, ParseInteger);
    end;

  Result := TShowProfileStmt.Create(Self, Nodes);
end;

function TSQLParser.ParseShowProfilesStmt(): TOffset;
var
  Nodes: TShowProfilesStmt.TNodes;
begin
  FillChar(Nodes, SizeOf(Nodes), 0);

  Nodes.StmtTag := ParseTag(kiSHOW, kiPROFILES);

  Result := TShowProfilesStmt.Create(Self, Nodes);
end;

function TSQLParser.ParseShowProfileStmtType(): TOffset;
begin
  Result := 0;

  if (IsTag(kiALL)) then
    Result := ParseTag(kiALL)
  else if (IsTag(kiBLOCK, kiIO)) then
    Result := ParseTag(kiBLOCK, kiIO)
  else if (IsTag(kiCONTEXT, kiSWITCHES)) then
    Result := ParseTag(kiCONTEXT, kiSWITCHES)
  else if (IsTag(kiCPU)) then
    Result := ParseTag(kiCPU)
  else if (IsTag(kiIPC)) then
    Result := ParseTag(kiIPC)
  else if (IsTag(kiMEMORY)) then
    Result := ParseTag(kiMEMORY)
  else if (IsTag(kiPAGE, kiFAULTS)) then
    Result := ParseTag(kiPAGE, kiFAULTS)
  else if (IsTag(kiSOURCE)) then
    Result := ParseTag(kiSOURCE)
  else if (IsTag(kiSWAPS)) then
    Result := ParseTag(kiSWAPS)
  else if (EndOfStmt(CurrentToken)) then
    SetError(PE_IncompleteStmt)
  else
    SetError(PE_UnexpectedToken);
end;

function TSQLParser.ParseShowRelaylogEventsStmt(): TOffset;
var
  Nodes: TShowRelaylogEventsStmt.TNodes;
begin
  FillChar(Nodes, SizeOf(Nodes), 0);

  Nodes.StmtTag := ParseTag(kiSHOW, kiRELAYLOG, kiEVENTS);

  if (not Error) then
    if (IsTag(kiIN)) then
      Nodes.InValue := ParseValue(kiIN, vaNo, ParseString);

  if (not Error) then
    if (IsTag(kiFROM)) then
      Nodes.InValue := ParseValue(kiFROM, vaNo, ParseInteger);

  if (not Error) then
    if (IsTag(kiLIMIT)) then
    begin
      Nodes.Limit.Tag := ParseTag(kiLIMIT);

      if (not Error) then
      begin
        Nodes.Limit.RowCountToken := ParseInteger();

        if (not Error) then
          if (IsSymbol(ttComma)) then
          begin
            Nodes.Limit.CommaToken := ParseSymbol(ttComma);

            if (not Error) then
            begin
              Nodes.Limit.OffsetToken := Nodes.Limit.RowCountToken;
              Nodes.Limit.RowCountToken := ParseExpr();
            end;
          end;
      end;
    end;

  Result := TShowRelaylogEventsStmt.Create(Self, Nodes);
end;

function TSQLParser.ParseShowSlaveHostsStmt(): TOffset;
var
  Nodes: TShowSlaveHostsStmt.TNodes;
begin
  FillChar(Nodes, SizeOf(Nodes), 0);

  Nodes.StmtTag := ParseTag(kiSHOW, kiSLAVE, kiHOSTS);

  Result := TShowSlaveHostsStmt.Create(Self, Nodes);
end;

function TSQLParser.ParseShowSlaveStatusStmt(): TOffset;
var
  Nodes: TShowSlaveStatusStmt.TNodes;
begin
  FillChar(Nodes, SizeOf(Nodes), 0);

  Nodes.StmtTag := ParseTag(kiSHOW, kiSLAVE, kiSTATUS);

  Result := TShowSlaveStatusStmt.Create(Self, Nodes);
end;

function TSQLParser.ParseShowStatusStmt(): TOffset;
var
  Nodes: TShowStatusStmt.TNodes;
begin
  FillChar(Nodes, SizeOf(Nodes), 0);

  Nodes.ShowTag := ParseTag(kiSHOW);

  if (not Error) then
    if (IsTag(kiGLOBAL)) then
      Nodes.ScopeTag := ParseTag(kiGLOBAL)
    else if (IsTag(kiSESSION)) then
      Nodes.ScopeTag := ParseTag(kiSESSION);

  if (not Error) then
    Nodes.StatusTag := ParseTag(kiSTATUS);

  if (not Error) then
    if (IsTag(kiLIKE)) then
      Nodes.LikeValue := ParseValue(kiLIKE, vaNo, ParseString)
    else if (IsTag(kiWHERE)) then
      Nodes.WhereValue := ParseValue(kiWHERE, vaNo, ParseExpr);

  Result := TShowStatusStmt.Create(Self, Nodes);
end;

function TSQLParser.ParseShowTableStatusStmt(): TOffset;
var
  Nodes: TShowTableStatusStmt.TNodes;
begin
  FillChar(Nodes, SizeOf(Nodes), 0);

  Nodes.ShowTag := ParseTag(kiSHOW, kiTABLE, kiSTATUS);

  if (not Error) then
    if (IsTag(kiFROM)) then
      Nodes.FromDatabaseValue := ParseValue(kiFROM, vaNo, ParseDatabaseIdent)
    else if (IsTag(kiIN)) then
      Nodes.FromDatabaseValue := ParseValue(kiIN, vaNo, ParseDatabaseIdent);

  if (not Error) then
    if (IsTag(kiLIKE)) then
      Nodes.LikeValue := ParseValue(kiLIKE, vaNo, ParseString)
    else if (IsTag(kiWHERE)) then
      Nodes.WhereValue := ParseValue(kiWHERE, vaNo, ParseExpr);

  Result := TShowTableStatusStmt.Create(Self, Nodes);
end;

function TSQLParser.ParseShowTablesStmt(): TOffset;
var
  Nodes: TShowTablesStmt.TNodes;
begin
  FillChar(Nodes, SizeOf(Nodes), 0);

  Nodes.ShowTag := ParseTag(kiSHOW);

  if (not Error) then
    if (IsTag(kiFULL)) then
      Nodes.FullTag := ParseTag(kiFULL);

  if (not Error) then
    Nodes.TablesTag := ParseTag(kiTABLES);

  if (not Error) then
    if (IsTag(kiFROM)) then
      Nodes.FromDatabaseValue := ParseValue(kiFROM, vaNo, ParseDatabaseIdent)
    else if (IsTag(kiIN)) then
      Nodes.FromDatabaseValue := ParseValue(kiIN, vaNo, ParseDatabaseIdent);

  if (not Error) then
    if (IsTag(kiLIKE)) then
      Nodes.LikeValue := ParseValue(kiLIKE, vaNo, ParseString)
    else if (IsTag(kiWHERE)) then
      Nodes.WhereValue := ParseValue(kiWHERE, vaNo, ParseExpr);

  Result := TShowTablesStmt.Create(Self, Nodes);
end;

function TSQLParser.ParseShowTriggersStmt(): TOffset;
var
  Nodes: TShowTriggersStmt.TNodes;
begin
  FillChar(Nodes, SizeOf(Nodes), 0);

  Nodes.StmtTag := ParseTag(kiSHOW, kiTRIGGERS);

  if (not Error) then
    if (IsTag(kiFROM)) then
      Nodes.FromDatabaseValue := ParseValue(kiFROM, vaNo, ParseDatabaseIdent)
    else if (IsTag(kiIN)) then
      Nodes.FromDatabaseValue := ParseValue(kiIN, vaNo, ParseDatabaseIdent);

  if (not Error) then
    if (IsTag(kiLIKE)) then
      Nodes.LikeValue := ParseValue(kiLIKE, vaNo, ParseString)
    else if (IsTag(kiWHERE)) then
      Nodes.WhereValue := ParseValue(kiWHERE, vaNo, ParseExpr);

  Result := TShowTriggersStmt.Create(Self, Nodes);
end;

function TSQLParser.ParseShowVariablesStmt(): TOffset;
var
  Nodes: TShowVariablesStmt.TNodes;
begin
  FillChar(Nodes, SizeOf(Nodes), 0);

  Nodes.ShowTag := ParseTag(kiSHOW);

  if (not Error) then
    if (IsTag(kiGLOBAL)) then
      Nodes.ScopeTag := ParseTag(kiGLOBAL)
    else if (IsTag(kiSESSION)) then
      Nodes.ScopeTag := ParseTag(kiSESSION);

  if (not Error) then
    Nodes.VariablesTag := ParseTag(kiVARIABLES);

  if (not Error) then
    if (IsTag(kiLIKE)) then
      Nodes.LikeValue := ParseValue(kiLIKE, vaNo, ParseString)
    else if (IsTag(kiWHERE)) then
      Nodes.WhereValue := ParseValue(kiWHERE, vaNo, ParseExpr);

  Result := TShowVariablesStmt.Create(Self, Nodes);
end;

function TSQLParser.ParseShowWarningsStmt(): TOffset;
var
  Nodes: TShowWarningsStmt.TNodes;
begin
  FillChar(Nodes, SizeOf(Nodes), 0);

  Nodes.StmtTag := ParseTag(kiSHOW, kiWARNINGS);

  if (not Error) then
    if (IsTag(kiLIMIT)) then
    begin
      Nodes.Limit.Tag := ParseTag(kiLIMIT);

      if (not Error) then
      begin
        Nodes.Limit.RowCountToken := ParseInteger();

        if (not Error) then
          if (IsSymbol(ttComma)) then
          begin
            Nodes.Limit.CommaToken := ParseSymbol(ttComma);

            if (not Error) then
            begin
              Nodes.Limit.OffsetToken := Nodes.Limit.RowCountToken;
              Nodes.Limit.RowCountToken := ParseInteger();
            end;
          end;
      end;
    end;

  Result := TShowWarningsStmt.Create(Self, Nodes);
end;

function TSQLParser.ParseShutdownStmt(): TOffset;
var
  Nodes: TShutdownStmt.TNodes;
begin
  FillChar(Nodes, SizeOf(Nodes), 0);

  Nodes.StmtTag := ParseTag(kiSHUTDOWN);

  Result := TShutdownStmt.Create(Self, Nodes);
end;

function TSQLParser.ParseSignalStmt(): TOffset;
var
  Nodes: TSignalStmt.TNodes;
begin
  FillChar(Nodes, SizeOf(Nodes), 0);

  if (IsTag(kiSIGNAL)) then
  begin
    Nodes.StmtTag := ParseTag(kiSIGNAL);

    if (not Error) then
      if (IsTag(kiSQLSTATE, kiVALUE)) then
        Nodes.Condition := ParseValue(WordIndices(kiSQLSTATE, kiVALUE), vaNo, ParseExpr)
      else if (IsTag(kiSQLSTATE)) then
        Nodes.Condition := ParseValue(kiSQLSTATE, vaNo, ParseString)
      else
        Nodes.Condition := ParseIdent();
  end
  else if (IsTag(kiRESIGNAL)) then
  begin
    Nodes.StmtTag := ParseTag(kiRESIGNAL);

    if (not Error) then
      if (IsTag(kiSQLSTATE, kiVALUE)) then
        Nodes.Condition := ParseValue(WordIndices(kiSQLSTATE, kiVALUE), vaYes, ParseExpr)
      else if (IsTag(kiSQLSTATE)) then
        Nodes.Condition := ParseValue(kiSQLSTATE, vaNo, ParseString)
      else if (not EndOfStmt(CurrentToken)) then
        Nodes.Condition := ParseIdent();
  end
  else
    SetError(PE_Unknown);

  if (not Error) then
    if (IsTag(kiSET)) then
    begin
      Nodes.SetTag := ParseTag(kiSET);

      if (not Error and not EndOfStmt(CurrentToken)) then
        Nodes.InformationList := ParseList(False, ParseSignalStmtInformation);
    end;

  Result := TSignalStmt.Create(Self, Nodes);
end;

function TSQLParser.ParseSignalStmtInformation(): TOffset;
var
  Nodes: TSignalStmt.TInformation.TNodes;
begin
  FillChar(Nodes, SizeOf(Nodes), 0);

  if (IsTag(kiCLASS_ORIGIN)
    or IsTag(kiSUBCLASS_ORIGIN)
    or IsTag(kiMESSAGE_TEXT)
    or IsTag(kiMYSQL_ERRNO)
    or IsTag(kiCONSTRAINT_CATALOG)
    or IsTag(kiCONSTRAINT_SCHEMA)
    or IsTag(kiCONSTRAINT_NAME)
    or IsTag(kiCATALOG_NAME)
    or IsTag(kiSCHEMA_NAME)
    or IsTag(kiTABLE_NAME)
    or IsTag(kiCOLUMN_NAME)
    or IsTag(kiCURSOR_NAME)) then
    Nodes.Value := ParseValue(TokenPtr(CurrentToken)^.KeywordIndex, vaYes, ParseExpr)
  else if (EndOfStmt(CurrentToken)) then
    SetError(PE_IncompleteStmt)
  else
    SetError(PE_UnexpectedToken);

  Result := TSignalStmt.TInformation.Create(Self, Nodes);
end;

function TSQLParser.ParseSQL(const Text: PChar; const Length: Integer; const AUseCompletionList: Boolean = False): Boolean;
begin
  Clear();

  SetString(ParsedText, Text, Length);
  Parse.Pos := PChar(ParsedText);
  Parse.Length := Length;
  FUseCompletionList := AUseCompletionList;

  FRoot := ParseRoot();

  Result := Root^.ErrorCode = PE_Success;
end;

function TSQLParser.ParseSQL(const Text: string; const AUseCompletionList: Boolean = False): Boolean;
begin
  Result := ParseSQL(PChar(Text), Length(Text), AUseCompletionList);
end;

function TSQLParser.ParseStartSlaveStmt(): TOffset;
var
  Nodes: TStartSlaveStmt.TNodes;
begin
  FillChar(Nodes, SizeOf(Nodes), 0);

  Nodes.StmtTag := ParseTag(kiSTART, kiSLAVE);

  Result := TStartSlaveStmt.Create(Self, Nodes);
end;

function TSQLParser.ParseStartTransactionStmt(): TOffset;
var
  Found: Boolean;
  Nodes: TStartTransactionStmt.TNodes;
begin
  FillChar(Nodes, SizeOf(Nodes), 0);

  Nodes.StartTransactionTag := ParseTag(kiSTART, kiTRANSACTION);

  Found := True;
  while (not Error and Found) do
    if ((Nodes.WithConsistentSnapshotTag = 0) and IsTag(kiWRITE, kiCONSISTENT, kiSNAPSHOT)) then
      Nodes.WithConsistentSnapshotTag := ParseTag(kiWITH, kiCONSISTENT, kiSNAPSHOT)
    else if ((Nodes.ReadWriteTag = 0) and IsTag(kiREAD, kiWRITE)) then
      Nodes.RealOnlyTag := ParseTag(kiREAD, kiWRITE)
    else if ((Nodes.RealOnlyTag = 0) and IsTag(kiREAD, kiONLY)) then
      Nodes.RealOnlyTag := ParseTag(kiREAD, kiONLY)
    else
      Found := False;

  Result := TStartTransactionStmt.Create(Self, Nodes);
end;

function TSQLParser.ParseStopSlaveStmt(): TOffset;
var
  Nodes: TStopSlaveStmt.TNodes;
begin
  FillChar(Nodes, SizeOf(Nodes), 0);

  Nodes.StmtTag := ParseTag(kiSTOP, kiSLAVE);

  Result := TStopSlaveStmt.Create(Self, Nodes);
end;

function TSQLParser.ParseStmt(): TOffset;
var
  Continue: Boolean;
  Index: Integer;
  FFirstTokenAll: TOffset;
  FLastTokenAll: TOffset;
  KeywordIndex: TWordList.TIndex; // Cache for speeding
  KeywordIndex1: TWordList.TIndex; // Cache for speeding
  KeywordIndex2: TWordList.TIndex; // Cache for speeding
  KeywordToken: TOffset;
  Length: Integer;
  Msg: string;
  T: PToken;
  Text: PChar;
  Token: TOffset;
begin
  Result := 0;
  {$IFDEF Debug}
  Continue := False;
  {$ENDIF}

  if (PreviousToken = 0) then
    FFirstTokenAll := 1
  else
  begin
    T := TokenPtr(PreviousToken);
    if (T^.TokenType = ttSemicolon) then
    begin
      repeat
        FFirstTokenAll := T^.Offset;
        T := T^.NextTokenAll;
      until (not Assigned(T) or (T^.TokenType in [ttSpace, ttReturn]));
      if (Assigned(T)) then
        FFirstTokenAll := T^.Offset;
    end
    else
    begin
      repeat
        T := T^.NextTokenAll;
      until (not Assigned(T) or (T^.TokenType in [ttSpace, ttReturn]));
      if (Assigned(T)) then
        T := T^.NextTokenAll;
      if (not Assigned(T) or not Assigned(T^.NextTokenAll)) then
        FFirstTokenAll := 0
      else
        FFirstTokenAll := T^.NextTokenAll^.Offset;
    end;
  end;

  KeywordToken := CurrentToken;
  if (not EndOfStmt(CurrentToken) and (TokenPtr(CurrentToken)^.TokenType = ttIdent)
    and not EndOfStmt(NextToken[1]) and (TokenPtr(NextToken[1])^.TokenType = ttColon)) then
    KeywordToken := NextToken[2];

  if (EndOfStmt(KeywordToken)) then
  begin
    CompletionList.AddKeyword(kiALTER);
    CompletionList.AddKeyword(kiANALYZE);
    CompletionList.AddKeyword(kiBEGIN);
    CompletionList.AddKeyword(kiCALL);
    CompletionList.AddKeyword(kiCASE);
    CompletionList.AddKeyword(kiCHANGE);
    CompletionList.AddKeyword(kiCHECK);
    CompletionList.AddKeyword(kiCHECKSUM);
    if (InPL_SQL) then
      CompletionList.AddKeyword(kiCLOSE);
    CompletionList.AddKeyword(kiCOMMIT);
    CompletionList.AddKeyword(kiCREATE);
    CompletionList.AddKeyword(kiDEALLOCATE);
    if (InPL_SQL) then
      CompletionList.AddKeyword(kiDECLARE, kiCONDITION);
    if (InPL_SQL) then
      CompletionList.AddKeyword(kiDECLARE, kiCURSOR);
    if (InPL_SQL) then
      CompletionList.AddKeyword(kiDECLARE, kiHANDLER);
    CompletionList.AddKeyword(kiDELETE);
    CompletionList.AddKeyword(kiDESC);
    CompletionList.AddKeyword(kiDESCRIBE);
    CompletionList.AddKeyword(kiDO);
    CompletionList.AddKeyword(kiDROP, kiDATABASE);
    CompletionList.AddKeyword(kiDROP, kiEVENT);
    CompletionList.AddKeyword(kiDROP, kiFUNCTION);
    CompletionList.AddKeyword(kiDROP, kiINDEX);
    CompletionList.AddKeyword(kiDROP, kiPREPARE);
    CompletionList.AddKeyword(kiDROP, kiPROCEDURE);
    CompletionList.AddKeyword(kiDROP, kiSCHEMA);
    CompletionList.AddKeyword(kiDROP, kiSERVER);
    CompletionList.AddKeyword(kiDROP, kiTEMPORARY, kiTABLE);
    CompletionList.AddKeyword(kiDROP, kiTABLE);
    CompletionList.AddKeyword(kiDROP, kiTRIGGER);
    CompletionList.AddKeyword(kiDROP, kiUSER);
    CompletionList.AddKeyword(kiDROP, kiVIEW);
    CompletionList.AddKeyword(kiEXECUTE);
    CompletionList.AddKeyword(kiEXPLAIN);
    if (InPL_SQL) then
      CompletionList.AddKeyword(kiFETCH);
    CompletionList.AddKeyword(kiFLUSH);
    CompletionList.AddKeyword(kiGET, kiCURRENT);
    CompletionList.AddKeyword(kiGET, kiSTACKED);
    CompletionList.AddKeyword(kiGET, kiDIAGNOSTICS);
    CompletionList.AddKeyword(kiGRANT);
    CompletionList.AddKeyword(kiHELP);
    if (InPL_SQL) then
      CompletionList.AddKeyword(kiIF);
    CompletionList.AddKeyword(kiINSERT);
    if (InPL_SQL) then
      CompletionList.AddKeyword(kiITERATE);
    CompletionList.AddKeyword(kiKILL);
    CompletionList.AddKeyword(kiLEAVE);
    CompletionList.AddKeyword(kiLOAD);
    CompletionList.AddKeyword(kiLOCK);
    if (InPL_SQL) then
      CompletionList.AddKeyword(kiLOOP);
    CompletionList.AddKeyword(kiPREPARE);
    CompletionList.AddKeyword(kiPURGE);
    if (InPL_SQL) then
      CompletionList.AddKeyword(kiOPEN);
    CompletionList.AddKeyword(kiRENAME);
    CompletionList.AddKeyword(kiREPAIR);
    if (InPL_SQL) then
      CompletionList.AddKeyword(kiREPEAT);
    CompletionList.AddKeyword(kiRELEASE);
    CompletionList.AddKeyword(kiREPLACE);
    CompletionList.AddKeyword(kiRESET);
    CompletionList.AddKeyword(kiRESIGNAL);
    if (InPL_SQL and InCreateFunctionStmt) then
      CompletionList.AddKeyword(kiRETURN);
    CompletionList.AddKeyword(kiREVOKE);
    CompletionList.AddKeyword(kiROLLBACK);
    CompletionList.AddKeyword(kiSAVEPOINT);
    CompletionList.AddKeyword(kiSELECT);
    CompletionList.AddKeyword(kiSET);
    CompletionList.AddKeyword(kiSET, kiCHARACTER);
    CompletionList.AddKeyword(kiSET, kiGLOBAL, kiTRANSACTION);
    CompletionList.AddKeyword(kiSET, kiNAMES);
    CompletionList.AddKeyword(kiSET, kiPASSWORD);
    CompletionList.AddKeyword(kiSET, kiSESSION, kiTRANSACTION);
    CompletionList.AddKeyword(kiSET, kiTRANSACTION);
    CompletionList.AddKeyword(kiSHOW, kiAUTHORS);
    CompletionList.AddKeyword(kiSHOW, kiBINARY, kiLOGS);
    CompletionList.AddKeyword(kiSHOW, kiMASTER, kiLOGS);
    CompletionList.AddKeyword(kiSHOW, kiBINLOG, kiEVENTS);
    CompletionList.AddKeyword(kiSHOW, kiCHARACTER, kiSET);
    CompletionList.AddKeyword(kiSHOW, kiCOLLATION);
    CompletionList.AddKeyword(kiSHOW, kiCOLUMNS);
    CompletionList.AddKeyword(kiSHOW, kiFULL, kiCOLUMNS);
    CompletionList.AddKeyword(kiSHOW, kiCONTRIBUTORS);
    CompletionList.AddKeyword(kiSHOW, kiCREATE, kiDATABASE);
    CompletionList.AddKeyword(kiSHOW, kiCREATE, kiEVENT);
    CompletionList.AddKeyword(kiSHOW, kiCREATE, kiFUNCTION);
    CompletionList.AddKeyword(kiSHOW, kiCREATE, kiPROCEDURE);
    CompletionList.AddKeyword(kiSHOW, kiCREATE, kiSCHEMA);
    CompletionList.AddKeyword(kiSHOW, kiCREATE, kiTABLE);
    CompletionList.AddKeyword(kiSHOW, kiCREATE, kiTRIGGER);
    CompletionList.AddKeyword(kiSHOW, kiCREATE, kiUSER);
    CompletionList.AddKeyword(kiSHOW, kiCREATE, kiVIEW);
    CompletionList.AddKeyword(kiSHOW, kiDATABASES);
    CompletionList.AddKeyword(kiSHOW, kiENGINE);
    CompletionList.AddKeyword(kiSHOW, kiENGINES);
    CompletionList.AddKeyword(kiSHOW, kiERRORS);
    CompletionList.AddKeyword(kiSHOW, kiEVENTS);
    CompletionList.AddKeyword(kiSHOW, kiFULL, kiTABLES);
    CompletionList.AddKeyword(kiSHOW, kiFULL, kiPROCESSLIST);
    CompletionList.AddKeyword(kiSHOW, kiFUNCTION, kiCODE);
    CompletionList.AddKeyword(kiSHOW, kiFUNCTION, kiSTATUS);
    CompletionList.AddKeyword(kiSHOW, kiGRANTS);
    CompletionList.AddKeyword(kiSHOW, kiINDEX);
    CompletionList.AddKeyword(kiSHOW, kiINDEXES);
    CompletionList.AddKeyword(kiSHOW, kiKEYS);
    CompletionList.AddKeyword(kiSHOW, kiMASTER, kiSTATUS);
    CompletionList.AddKeyword(kiSHOW, kiOPEN, kiTABLES);
    CompletionList.AddKeyword(kiSHOW, kiPLUGINS);
    CompletionList.AddKeyword(kiSHOW, kiPRIVILEGES  );
    CompletionList.AddKeyword(kiSHOW, kiPROCEDURE, kiCODE);
    CompletionList.AddKeyword(kiSHOW, kiPROCEDURE, kiSTATUS);
    CompletionList.AddKeyword(kiSHOW, kiPROCESSLIST);
    CompletionList.AddKeyword(kiSHOW, kiPROFILE);
    CompletionList.AddKeyword(kiSHOW, kiPROFILES);
    CompletionList.AddKeyword(kiSHOW, kiRELAYLOG, kiEVENTS);
    CompletionList.AddKeyword(kiSHOW, kiSLAVE, kiHOSTS);
    CompletionList.AddKeyword(kiSHOW, kiSLAVE, kiSTATUS);
    CompletionList.AddKeyword(kiSHOW, kiSTATUS);
    CompletionList.AddKeyword(kiSHOW, kiGLOBAL, kiSTATUS);
    CompletionList.AddKeyword(kiSHOW, kiSESSION, kiSTATUS);
    CompletionList.AddKeyword(kiSHOW, kiTABLE, kiSTATUS);
    CompletionList.AddKeyword(kiSHOW, kiTABLES);
    CompletionList.AddKeyword(kiSHOW, kiTRIGGERS);
    CompletionList.AddKeyword(kiSHOW, kiVARIABLES);
    CompletionList.AddKeyword(kiSHOW, kiGLOBAL, kiVARIABLES);
    CompletionList.AddKeyword(kiSHOW, kiSESSION, kiVARIABLES);
    CompletionList.AddKeyword(kiSHOW, kiWARNINGS);
    CompletionList.AddKeyword(kiSHOW, kiSTORAGE, kiENGINES);
    CompletionList.AddKeyword(kiSHUTDOWN);
    CompletionList.AddKeyword(kiSIGNAL);
    CompletionList.AddKeyword(kiSTART, kiSLAVE);
    CompletionList.AddKeyword(kiSTART, kiTRANSACTION);
    CompletionList.AddKeyword(kiSTOP);
    CompletionList.AddKeyword(kiTRUNCATE);
    CompletionList.AddKeyword(kiUNLOCK);
    CompletionList.AddKeyword(kiUPDATE);
    CompletionList.AddKeyword(kiUSE);
    if (InPL_SQL and InCreateFunctionStmt) then
      CompletionList.AddKeyword(kiWHILE);
    CompletionList.AddKeyword(kiXA);
    SetError(PE_IncompleteStmt);
  end
  else
  begin
    KeywordIndex := TokenPtr(KeywordToken)^.KeywordIndex;

    if (KeywordIndex = kiALTER) then
      Result := ParseAlterStmt()
    else if (KeywordIndex = kiANALYZE) then
      Result := ParseAnalyzeTableStmt()
    else if (KeywordIndex = kiBEGIN) then
      if (not InPL_SQL) then
        Result := ParseBeginStmt()
      else
        Result := ParseCompoundStmt()
    else if (KeywordIndex = kiCALL) then
      Result := ParseCallStmt()
    else if (InPL_SQL and (KeywordIndex = kiCASE)) then
      Result := ParseCaseStmt()
    else if (KeywordIndex = kiCHANGE) then
      Result := ParseChangeMasterStmt()
    else if (KeywordIndex = kiCHECK) then
      Result := ParseCheckTableStmt()
    else if (KeywordIndex = kiCHECKSUM) then
      Result := ParseChecksumTableStmt()
    else if (InPL_SQL and (KeywordIndex = kiCLOSE)) then
      Result := ParseCloseStmt()
    else if (KeywordIndex = kiCOMMIT) then
      Result := ParseCommitStmt()
    else if (KeywordIndex = kiCREATE) then
      Result := ParseCreateStmt()
    else if (KeywordIndex = kiDEALLOCATE) then
      Result := ParseDeallocatePrepareStmt()
    else if (InPL_SQL and (KeywordIndex = kiDECLARE)) then
      if (not EndOfStmt(NextToken[2]) and (TokenPtr(NextToken[2])^.KeywordIndex = kiCONDITION)) then
        Result := ParseDeclareConditionStmt()
      else if (not EndOfStmt(NextToken[2]) and (TokenPtr(NextToken[2])^.KeywordIndex = kiCURSOR)) then
        Result := ParseDeclareCursorStmt()
      else if (not EndOfStmt(NextToken[2]) and (TokenPtr(NextToken[2])^.KeywordIndex = kiHANDLER)) then
        Result := ParseDeclareHandlerStmt()
      else
        Result := ParseDeclareStmt()
    else if (KeywordIndex = kiDELETE) then
      Result := ParseDeleteStmt()
    else if (KeywordIndex = kiDESC) then
      Result := ParseExplainStmt()
    else if (KeywordIndex = kiDESCRIBE) then
      Result := ParseExplainStmt()
    else if (KeywordIndex = kiDO) then
      Result := ParseDoStmt()
    else if (KeywordIndex = kiDROP) then
    begin
      if (EndOfStmt(NextToken[1])) then
        SetError(PE_IncompleteStmt)
      else
      begin
        KeywordIndex1 := TokenPtr(NextToken[1])^.KeywordIndex;
        if (KeywordIndex1 = kiDATABASE) then
          Result := ParseDropDatabaseStmt()
        else if (KeywordIndex1 = kiEVENT) then
          Result := ParseDropEventStmt()
        else if (KeywordIndex1 = kiFUNCTION) then
          Result := ParseDropRoutineStmt(rtFunction)
        else if (KeywordIndex1 = kiINDEX) then
          Result := ParseDropIndexStmt()
        else if (KeywordIndex1 = kiPREPARE) then
          Result := ParseDeallocatePrepareStmt()
        else if (KeywordIndex1 = kiPROCEDURE) then
          Result := ParseDropRoutineStmt(rtProcedure)
        else if (KeywordIndex1 = kiSCHEMA) then
          Result := ParseDropDatabaseStmt()
        else if (KeywordIndex1 = kiSERVER) then
          Result := ParseDropServerStmt()
        else if (KeywordIndex1 = kiTEMPORARY) then
        begin
          if (EndOfStmt(NextToken[2])) then
            SetError(PE_IncompleteStmt)
          else if (TokenPtr(NextToken[2])^.KeywordIndex <> kiTABLE) then
            SetError(PE_UnexpectedToken, NextToken[2])
          else
            Result := ParseDropTableStmt();
        end
        else if (KeywordIndex1 = kiTABLE) then
          Result := ParseDropTableStmt()
        else if (KeywordIndex1 = kiTRIGGER) then
          Result := ParseDropTriggerStmt()
        else if (KeywordIndex1 = kiUSER) then
          Result := ParseDropUserStmt()
        else if (KeywordIndex1 = kiVIEW) then
          Result := ParseDropViewStmt()
        else
        begin
          SetError(PE_UnknownStmt, NextToken[1]);
          Result := ParseUnknownStmt();
        end;
      end;
    end
    else if (KeywordIndex = kiEXECUTE) then
      Result := ParseExecuteStmt()
    else if (KeywordIndex = kiEXPLAIN) then
      Result := ParseExplainStmt()
    else if (InPL_SQL and (KeywordIndex = kiFETCH)) then
      Result := ParseFetchStmt()
    else if (KeywordIndex = kiFLUSH) then
      Result := ParseFlushStmt()
    else if ((KeywordIndex = kiGET)
      and not EndOfStmt(NextToken[2])
      and ((((TokenPtr(NextToken[1])^.KeywordIndex = kiCURRENT) or (TokenPtr(NextToken[1])^.KeywordIndex = kiSTACKED)) and (TokenPtr(NextToken[2])^.KeywordIndex = kiDIAGNOSTICS)) or (TokenPtr(NextToken[1])^.KeywordIndex = kiDIAGNOSTICS))) then
      Result := ParseGetDiagnosticsStmt()
    else if (KeywordIndex = kiGRANT) then
      Result := ParseGrantStmt()
    else if (KeywordIndex = kiHELP) then
      Result := ParseHelpStmt()
    else if (InPL_SQL and (KeywordIndex = kiIF)) then
      Result := ParseIfStmt()
    else if (KeywordIndex = kiINSERT) then
      Result := ParseInsertStmt()
    else if (InPL_SQL and (KeywordIndex = kiITERATE)) then
      Result := ParseIterateStmt()
    else if (KeywordIndex = kiKILL) then
      Result := ParseKillStmt()
    else if (InPL_SQL and (KeywordIndex = kiLEAVE)) then
      Result := ParseLeaveStmt()
    else if ((KeywordIndex = kiLOAD)) then
      Result := ParseLoadStmt()
    else if ((KeywordIndex = kiLOCK)) then
      Result := ParseLockTableStmt()
    else if (InPL_SQL and (KeywordIndex = kiLOOP)) then
      Result := ParseLoopStmt()
    else if (KeywordIndex = kiPREPARE) then
      Result := ParsePrepareStmt()
    else if (KeywordIndex = kiPURGE) then
      Result := ParsePurgeStmt()
    else if (InPL_SQL and (KeywordIndex = kiOPEN)) then
      Result := ParseOpenStmt()
    else if (KeywordIndex = kiRENAME) then
      Result := ParseRenameStmt()
    else if (KeywordIndex = kiREPAIR) then
      Result := ParseRepairTableStmt()
    else if (InPL_SQL and (KeywordIndex = kiREPEAT)) then
      Result := ParseRepeatStmt()
    else if (KeywordIndex = kiRELEASE) then
      Result := ParseReleaseStmt()
    else if (KeywordIndex = kiREPLACE) then
      Result := ParseInsertStmt()
    else if (KeywordIndex = kiRESET) then
      Result := ParseResetStmt()
    else if (KeywordIndex = kiRESIGNAL) then
      Result := ParseResignalStmt()
    else if (InPL_SQL and (KeywordIndex = kiRETURN) and InCreateFunctionStmt) then
      Result := ParseReturnStmt()
    else if (KeywordIndex = kiREVOKE) then
      Result := ParseRevokeStmt()
    else if (KeywordIndex = kiROLLBACK) then
      Result := ParseRollbackStmt()
    else if (KeywordIndex = kiSAVEPOINT) then
      Result := ParseSavepointStmt()
    else if (KeywordIndex = kiSELECT) then
      Result := ParseSelectStmt(False)
    else if (KeywordIndex = kiSET) then
      if (not EndOfStmt(NextToken[1]) and (TokenPtr(NextToken[1])^.KeywordIndex = kiNAMES)) then
        Result := ParseSetNamesStmt()
      else if (not EndOfStmt(NextToken[1]) and (TokenPtr(NextToken[1])^.KeywordIndex = kiCHARACTER)) then
        Result := ParseSetNamesStmt()
      else if (not EndOfStmt(NextToken[1]) and (TokenPtr(NextToken[1])^.KeywordIndex = kiCHARSET)) then
        Result := ParseSetNamesStmt()
      else if (not EndOfStmt(NextToken[1]) and (TokenPtr(NextToken[1])^.KeywordIndex = kiPASSWORD)) then
        Result := ParseSetPasswordStmt()
      else
      begin
        if (EndOfStmt(NextToken[1]) or (TokenPtr(NextToken[1])^.KeywordIndex <> kiGLOBAL) and (TokenPtr(NextToken[1])^.KeywordIndex <> kiSESSION)) then
          Index := 1
        else
          Index := 2;
        if (not EndOfStmt(NextToken[Index]) and (TokenPtr(NextToken[Index])^.KeywordIndex = kiTRANSACTION)) then
          Result := ParseSetTransactionStmt()
        else
          Result := ParseSetStmt()
      end
    else
    {$IFDEF Debug}
      Continue := True; // This "Hack" is needed to use <Ctrl+LeftClick>
    if (Continue) then  // the Delphi XE2 IDE. But why???
    {$ENDIF}
    if (KeywordIndex = kiSHOW) then
    begin
      KeywordIndex1 := 0; KeywordIndex2 := 0;
      if (not EndOfStmt(NextToken[1])) then
      begin
        KeywordIndex1 := TokenPtr(NextToken[1])^.KeywordIndex;
        if (not EndOfStmt(NextToken[2])) then
          KeywordIndex2 := TokenPtr(NextToken[2])^.KeywordIndex;
      end;
      if ((KeywordIndex1 = kiAUTHORS)) then
        Result := ParseShowAuthorsStmt()
      else if ((KeywordIndex1 = kiBINARY) and (KeywordIndex2 = kiLOGS)) then
        Result := ParseShowBinaryLogsStmt()
      else if ((KeywordIndex1 = kiMASTER) and (KeywordIndex2 = kiLOGS)) then
        Result := ParseShowBinaryLogsStmt()
      else if ((KeywordIndex1 = kiBINLOG) and (KeywordIndex2 = kiEVENTS)) then
        Result := ParseShowBinlogEventsStmt()
      else if ((KeywordIndex1 = kiCHARACTER) and (KeywordIndex2 = kiSET)) then
        Result := ParseShowCharacterSetStmt()
      else if (KeywordIndex1 = kiCOLLATION) then
        Result := ParseShowCollationStmt()
      else if (KeywordIndex1 = kiCOLUMNS) then
        Result := ParseShowColumnsStmt()
      else if ((KeywordIndex1 = kiFULL) and (KeywordIndex2 = kiCOLUMNS)) then
        Result := ParseShowColumnsStmt()
      else if (KeywordIndex1 = kiCONTRIBUTORS) then
        Result := ParseShowContributorsStmt()
      else if (not EndOfStmt(NextToken[5]) and (TokenPtr(NextToken[5])^.KeywordIndex = kiERRORS)) then
        Result := ParseShowCountErrorsStmt()
      else if (not EndOfStmt(NextToken[5]) and (TokenPtr(NextToken[5])^.KeywordIndex = kiWARNINGS)) then
        Result := ParseShowCountWarningsStmt()
      else if ((KeywordIndex1 = kiCREATE) and (KeywordIndex2 = kiDATABASE)) then
        Result := ParseShowCreateDatabaseStmt()
      else if ((KeywordIndex1 = kiCREATE) and (KeywordIndex2 = kiEVENT)) then
        Result := ParseShowCreateEventStmt()
      else if ((KeywordIndex1 = kiCREATE) and (KeywordIndex2 = kiFUNCTION)) then
        Result := ParseShowCreateFunctionStmt()
      else if ((KeywordIndex1 = kiCREATE) and (KeywordIndex2 = kiPROCEDURE)) then
        Result := ParseShowCreateProcedureStmt()
      else if ((KeywordIndex1 = kiCREATE) and (KeywordIndex2 = kiSCHEMA)) then
        Result := ParseShowCreateDatabaseStmt()
      else if ((KeywordIndex1 = kiCREATE) and (KeywordIndex2 = kiPROCEDURE)) then
        Result := ParseShowCreateProcedureStmt()
      else if ((KeywordIndex1 = kiCREATE) and (KeywordIndex2 = kiTABLE)) then
        Result := ParseShowCreateTableStmt()
      else if ((KeywordIndex1 = kiCREATE) and (KeywordIndex2 = kiTRIGGER)) then
        Result := ParseShowCreateTriggerStmt()
      else if ((KeywordIndex1 = kiCREATE) and (KeywordIndex2 = kiUSER)) then
        Result := ParseShowCreateUserStmt()
      else if ((KeywordIndex1 = kiCREATE) and (KeywordIndex2 = kiVIEW)) then
        Result := ParseShowCreateViewStmt()
      else if (KeywordIndex1 = kiDATABASES) then
        Result := ParseShowDatabasesStmt()
      else if (KeywordIndex1 = kiENGINE) then
        Result := ParseShowEngineStmt()
      else if (KeywordIndex1 = kiENGINES) then
        Result := ParseShowEnginesStmt()
      else if (KeywordIndex1 = kiERRORS) then
        Result := ParseShowErrorsStmt()
      else if (KeywordIndex1 = kiEVENTS) then
        Result := ParseShowEventsStmt()
      else if ((KeywordIndex1 = kiFULL) and (KeywordIndex2 = kiTABLES)) then
        Result := ParseShowTablesStmt()
      else if ((KeywordIndex1 = kiFULL) and (KeywordIndex2 = kiPROCESSLIST)) then
        Result := ParseShowProcessListStmt()
      else if ((KeywordIndex1 = kiFUNCTION) and (KeywordIndex2 = kiCODE)) then
        Result := ParseShowFunctionCodeStmt()
      else if ((KeywordIndex1 = kiFUNCTION) and (KeywordIndex2 = kiSTATUS)) then
        Result := ParseShowFunctionStatusStmt()
      else if (KeywordIndex1 = kiGRANTS) then
        Result := ParseShowGrantsStmt()
      else if (KeywordIndex1 = kiINDEX) then
        Result := ParseShowIndexStmt()
      else if (KeywordIndex1 = kiINDEXES) then
        Result := ParseShowIndexStmt()
      else if (KeywordIndex1 = kiKEYS) then
        Result := ParseShowIndexStmt()
      else if ((KeywordIndex1 = kiMASTER) and (KeywordIndex2 = kiSTATUS)) then
        Result := ParseShowMasterStatusStmt()
      else if ((KeywordIndex1 = kiOPEN) and (KeywordIndex2 = kiTABLES)) then
        Result := ParseShowOpenTablesStmt()
      else if (KeywordIndex1 = kiPLUGINS) then
        Result := ParseShowPluginsStmt()
      else if (KeywordIndex1 = kiPRIVILEGES) then
        Result := ParseShowPrivilegesStmt()
      else if ((KeywordIndex1 = kiPROCEDURE) and (KeywordIndex2 = kiCODE)) then
        Result := ParseShowProcedureCodeStmt()
      else if ((KeywordIndex1 = kiPROCEDURE) and (KeywordIndex2 = kiSTATUS)) then
        Result := ParseShowProcedureStatusStmt()
      else if (KeywordIndex1 = kiPROCESSLIST) then
        Result := ParseShowProcessListStmt()
      else if (KeywordIndex1 = kiPROFILE) then
        Result := ParseShowProfileStmt()
      else if (KeywordIndex1 = kiPROFILES) then
        Result := ParseShowProfilesStmt()
      else if ((KeywordIndex1 = kiRELAYLOG) and (KeywordIndex2 = kiEVENTS)) then
        Result := ParseShowRelaylogEventsStmt()
      else if ((KeywordIndex1 = kiSLAVE) and (KeywordIndex2 = kiHOSTS)) then
        Result := ParseShowSlaveHostsStmt()
      else if ((KeywordIndex1 = kiSLAVE) and (KeywordIndex2 = kiSTATUS)) then
        Result := ParseShowSlaveStatusStmt()
      else if ((KeywordIndex1 = kiSTATUS)
        or (KeywordIndex1 = kiGLOBAL) and (KeywordIndex2 = kiSTATUS)
        or (KeywordIndex1 = kiSESSION) and (KeywordIndex2 = kiSTATUS)) then
        Result := ParseShowStatusStmt()
      else if ((KeywordIndex1 = kiTABLE) and (KeywordIndex2 = kiSTATUS)) then
        Result := ParseShowTableStatusStmt()
      else if (KeywordIndex1 = kiTABLES) then
        Result := ParseShowTablesStmt()
      else if (KeywordIndex1 = kiTRIGGERS) then
        Result := ParseShowTriggersStmt()
      else if ((KeywordIndex1 = kiVARIABLES)
        or (KeywordIndex1 = kiGLOBAL) and (KeywordIndex2 = kiVARIABLES)
        or (KeywordIndex1 = kiSESSION) and (KeywordIndex2 = kiVARIABLES)) then
        Result := ParseShowVariablesStmt()
      else if (KeywordIndex1 = kiWARNINGS) then
        Result := ParseShowWarningsStmt()
      else if ((KeywordIndex1 = kiSTORAGE) and (KeywordIndex2 = kiENGINES)) then
        Result := ParseShowEnginesStmt()
      else
      begin
        SetError(PE_UnknownStmt, CurrentToken);
        Result := ParseUnknownStmt();
      end;
    end
    else if (KeywordIndex = kiSHUTDOWN) then
      Result := ParseShutdownStmt()
    else if (KeywordIndex = kiSIGNAL) then
      Result := ParseSignalStmt()
    else if ((KeywordIndex = kiSTART)
      and not EndOfStmt(NextToken[1]) and (TokenPtr(NextToken[1])^.KeywordIndex = kiSLAVE)) then
      Result := ParseStartSlaveStmt()
    else if (KeywordIndex = kiSTART) then
      Result := ParseStartTransactionStmt()
    else if (KeywordIndex = kiSTOP) then
      Result := ParseStopSlaveStmt()
    else if (KeywordIndex = kiTRUNCATE) then
      Result := ParseTruncateTableStmt()
    else if (KeywordIndex = kiUNLOCK) then
      Result := ParseUnlockTablesStmt()
    else if (KeywordIndex = kiUPDATE) then
      Result := ParseUpdateStmt()
    else if (KeywordIndex = kiUSE) then
      Result := ParseUseStmt()
    else if (InPL_SQL and (KeywordIndex = kiWHILE)) then
      Result := ParseWhileStmt()
    else if (KeywordIndex = kiXA) then
      Result := ParseXAStmt()
    else if ((TokenPtr(CurrentToken)^.TokenType = ttOpenBracket)
      and not EndOfStmt(NextToken[1]) and (TokenPtr(NextToken[1])^.KeywordIndex = kiSELECT)) then
      Result := ParseSelectStmt(True)
    else
    begin
      SetError(PE_UnknownStmt);
      Result := ParseUnknownStmt();
    end;

    if (IsStmt(Result)) then
    begin
      if (not Error
        and not EndOfStmt(CurrentToken)
        and (TokenPtr(CurrentToken)^.KeywordIndex <> kiEND)) then
        SetError(PE_ExtraToken);

      // Add unparsed Tokens to the Stmt
      while (not EndOfStmt(CurrentToken) and (TokenPtr(CurrentToken)^.KeywordIndex <> kiEND)) do
      begin
        Token := ApplyCurrentToken();
        StmtPtr(Result)^.Heritage.AddChildren(@Token, 1);
      end;

      Token := StmtPtr(Result)^.FLastToken;
      FLastTokenAll := Token;
      while ((Token > 0) and (TokenPtr(Token)^.TokenType <> ttSemicolon)) do
      begin
        T := TokenPtr(Token)^.NextTokenAll;
        if (not Assigned(T)) then
          Token := 0
        else
          Token := T^.Offset;
        if ((Token > 0) and (TokenPtr(Token)^.TokenType <> ttSemicolon)) then
          FLastTokenAll := Token;
      end;

      if (FErrorCode > PE_Success) then
      begin
        case (FErrorCode) of
          PE_Success:
            Msg := '';
          PE_Unknown:
            Msg := 'Unknown error';
          PE_IncompleteToken:
            Msg := 'Incomplete token in line ' + IntToStr(FErrorLine);
          PE_UnexpectedChar:
            Msg := 'Unexpected character near ''' + LeftStr(StrPas(TokenPtr(FErrorToken)^.ErrorPos), 8) + ''''
              + ' in line ' + IntToStr(FErrorLine);
          PE_IncompleteStmt:
            Msg := 'Incompleted statement';
          PE_UnexpectedToken:
            begin
              TokenPtr(FErrorToken)^.GetText(Text, Length);
              Msg := 'Unexpected character near ''' + LeftStr(StrPas(Text), 8) + ''''
                + ' in line ' + IntToStr(FErrorLine);
            end;
          PE_ExtraToken:
            begin
              TokenPtr(FErrorToken)^.GetText(Text, Length);
              Msg := 'Unexpected character near ''' + LeftStr(StrPas(Text), 8) + ''''
                + ' in line ' + IntToStr(FErrorLine);
            end;
          PE_NestedCondCode:
            Msg := 'Nested conditional MySQL options';
          PE_UnknownStmt:
            begin
              TokenPtr(FErrorToken)^.GetText(Text, Length);
              Msg := 'Unknown statement ''' + LeftStr(StrPas(Text), 8) + ''''
                + ' in line ' + IntToStr(FErrorLine);
            end;
          else
            raise Exception.Create(SArgumentOutOfRange);
        end;
        StmtPtr(Result)^.FErrorCode := FErrorCode;
        StmtPtr(Result)^.FErrorMessage := NewText(PChar(Msg), System.Length(Msg));
        StmtPtr(Result)^.FErrorToken := FErrorToken;
      end;
      StmtPtr(Result)^.FFirstTokenAll := FFirstTokenAll;
      StmtPtr(Result)^.FLastTokenAll := FLastTokenAll;
    end;
  end;
end;

function TSQLParser.ParseString(): TOffset;
begin
  Result := 0;
  if (EndOfStmt(CurrentToken)) then
    SetError(PE_IncompleteStmt)
  else if (not (TokenPtr(CurrentToken)^.TokenType in ttStrings) and not ((TokenPtr(CurrentToken)^.TokenType = ttIdent) and (TokenPtr(CurrentToken)^.KeywordIndex < 0))) then
    SetError(PE_UnexpectedToken)
  else
    Result := ApplyCurrentToken(utConst);
end;

function TSQLParser.ParseSubArea(const ParseArea: TParseFunction): TOffset;
var
  Nodes: TSubArea.TNodes;
begin
  FillChar(Nodes, SizeOf(Nodes), 0);

  Nodes.OpenBracket := ParseSymbol(ttOpenBracket);

  if (not Error) then
    Nodes.AreaNode := ParseArea();

  if (not Error) then
    Nodes.CloseBracket := ParseSymbol(ttCloseBracket);

  Result := TSubArea.Create(Self, Nodes);
end;

function TSQLParser.ParseSubPartition(): TOffset;
var
  Found: Boolean;
  Nodes: TSubPartition.TNodes;
begin
  if (not Error) then
    Nodes.SubPartitionTag := ParseTag(kiPARTITION);

  if (not Error) then
    Nodes.NameIdent := ParsePartitionIdent();

  Found := True;
  while (not Error and Found) do
    if ((Nodes.CommentValue = 0) and IsTag(kiCOMMENT)) then
      Nodes.CommentValue := ParseValue(kiCOMMENT, vaAuto, ParseString)
    else if ((Nodes.DataDirectoryValue = 0) and IsTag(kiDATA, kiDIRECTORY)) then
      Nodes.DataDirectoryValue := ParseValue(WordIndices(kiDATA, kiDIRECTORY), vaAuto, ParseString)
    else if ((Nodes.EngineValue = 0) and IsTag(kiSTORAGE, kiENGINE)) then
      Nodes.EngineValue := ParseValue(WordIndices(kiSTORAGE, kiENGINE), vaAuto, ParseIdent)
    else if ((Nodes.EngineValue = 0) and IsTag(kiENGINE)) then
      Nodes.EngineValue := ParseValue(kiENGINE, vaAuto, ParseEngineIdent)
    else if ((Nodes.IndexDirectoryValue = 0) and IsTag(kiINDEX, kiDIRECTORY)) then
      Nodes.IndexDirectoryValue := ParseValue(WordIndices(kiINDEX, kiDIRECTORY), vaAuto, ParseString)
    else if ((Nodes.MaxRowsValue = 0) and IsTag(kiMAX_ROWS)) then
      Nodes.MaxRowsValue := ParseValue(kiMAX_ROWS, vaAuto, ParseInteger)
    else if ((Nodes.MinRowsValue = 0) and IsTag(kiMIN_ROWS)) then
      Nodes.MinRowsValue := ParseValue(kiMIN_ROWS, vaAuto, ParseInteger)
    else if ((Nodes.TablespaceValue = 0) and IsTag(kiTABLESPACE)) then
      Nodes.TablespaceValue := ParseValue(kiTABLESPACE, vaAuto, ParseIdent)
    else
      Found := False;

  Result := TSubPartition.Create(Self, Nodes);
end;

function TSQLParser.ParseSubquery(): TOffset;
var
  Nodes: TSubquery.TNodes;
begin
  FillChar(Nodes, SizeOf(Nodes), 0);

  Nodes.IdentTag := ParseTag(TokenPtr(CurrentToken)^.KeywordIndex);

  if (not Error) then
    Nodes.OpenToken := ParseSymbol(ttOpenBracket);

  if (not Error) then
    Nodes.Subquery := ParseSelectStmt(True);

  if (not Error) then
    Nodes.CloseToken := ParseSymbol(ttCloseBracket);

  Result := TSubquery.Create(Self, Nodes);
end;

function TSQLParser.ParseSubstringFunc(): TOffset;
var
  Nodes: TSubstringFunc.TNodes;
  Symbol: Boolean;
begin
  FillChar(Nodes, SizeOf(Nodes), 0);
  Symbol := False;

  Nodes.IdentToken := ApplyCurrentToken(utFunction);

  if (not Error) then
    Nodes.OpenBracket := ParseSymbol(ttOpenBracket);

  if (not Error) then
    Nodes.Str := ParseExpr();

  if (not Error) then
    if (IsTag(kiFROM)) then
    begin
      Nodes.FromTag := ParseTag(kiFROM);
      Symbol := False;
    end
    else if (IsSymbol(ttComma)) then
    begin
      Nodes.FromTag := ParseSymbol(ttComma);
      Symbol := True;
    end
    else if (EndOfStmt(CurrentToken)) then
      SetError(PE_IncompleteStmt)
    else
      SetError(PE_UnexpectedToken);

  if (not Error) then
    Nodes.Pos := ParseExpr();

  if (not Error) then
    if (not Symbol and IsTag(kiFOR)) then
      Nodes.ForTag := ParseTag(kiFOR)
    else if (Symbol and IsSymbol(ttComma)) then
      Nodes.ForTag := ParseSymbol(ttComma);

  if (not Error and (Nodes.ForTag > 0)) then
    Nodes.Len := ParseExpr();

  if (not Error) then
    Nodes.CloseBracket := ParseSymbol(ttCloseBracket);

  Result := TSubstringFunc.Create(Self, Nodes);
end;

function TSQLParser.ParseTableIdent(): TOffset;
begin
  Result := ParseDbIdent(ditTable);
end;

function TSQLParser.ParseSymbol(const TokenType: TTokenType): TOffset;
begin
  Result := 0;

  if ((CurrentToken = 0) or (TokenType <> ttSemicolon) and (TokenPtr(CurrentToken)^.TokenType = ttSemicolon)) then
    SetError(PE_IncompleteStmt)
  else if (TokenPtr(CurrentToken)^.TokenType <> TokenType) then
    SetError(PE_UnexpectedToken)
  else
    Result := ApplyCurrentToken(utSymbol);
end;

function TSQLParser.ParseTag(const KeywordIndex1: TWordList.TIndex;
  const KeywordIndex2: TWordList.TIndex = -1; const KeywordIndex3: TWordList.TIndex = -1;
  const KeywordIndex4: TWordList.TIndex = -1; const KeywordIndex5: TWordList.TIndex = -1;
  const KeywordIndex6: TWordList.TIndex = -1; const KeywordIndex7: TWordList.TIndex = -1): TOffset;
var
  Nodes: TTag.TNodes;
begin
  FillChar(Nodes, SizeOf(Nodes), 0);

  if (EndOfStmt(CurrentToken)) then
  begin
    CompletionList.AddKeyword(KeywordIndex1, KeywordIndex2, KeywordIndex3, KeywordIndex4, KeywordIndex5, KeywordIndex6, KeywordIndex7);
    SetError(PE_IncompleteStmt);
  end
  else if (TokenPtr(CurrentToken)^.KeywordIndex <> KeywordIndex1) then
    SetError(PE_UnexpectedToken)
  else
  begin
    TokenPtr(CurrentToken)^.FOperatorType := otUnknown;
    Nodes.KeywordToken1 := ApplyCurrentToken();

    if (KeywordIndex2 >= 0) then
    begin
      if (EndOfStmt(CurrentToken)) then
      begin
        CompletionList.AddKeyword(KeywordIndex2, KeywordIndex3, KeywordIndex4, KeywordIndex5, KeywordIndex6, KeywordIndex7);
        SetError(PE_IncompleteStmt);
      end
      else if (TokenPtr(CurrentToken)^.KeywordIndex <> KeywordIndex2) then
        SetError(PE_UnexpectedToken)
      else
      begin
        TokenPtr(CurrentToken)^.FOperatorType := otUnknown;
        Nodes.KeywordToken2 := ApplyCurrentToken();

        if (KeywordIndex3 >= 0) then
        begin
          if (EndOfStmt(CurrentToken)) then
          begin
            CompletionList.AddKeyword(KeywordIndex3, KeywordIndex4, KeywordIndex5, KeywordIndex6, KeywordIndex7);
            SetError(PE_IncompleteStmt);
          end
          else if (TokenPtr(CurrentToken)^.KeywordIndex <> KeywordIndex3) then
            SetError(PE_UnexpectedToken)
          else
          begin
            TokenPtr(CurrentToken)^.FOperatorType := otUnknown;
            Nodes.KeywordToken3 := ApplyCurrentToken();

            if (KeywordIndex4 >= 0) then
            begin
              if (EndOfStmt(CurrentToken)) then
              begin
                CompletionList.AddKeyword(KeywordIndex4, KeywordIndex5, KeywordIndex6, KeywordIndex7);
                SetError(PE_IncompleteStmt);
              end
              else if (TokenPtr(CurrentToken)^.KeywordIndex <> KeywordIndex4) then
                SetError(PE_UnexpectedToken)
              else
              begin
                TokenPtr(CurrentToken)^.FOperatorType := otUnknown;
                Nodes.KeywordToken4 := ApplyCurrentToken();

                if (KeywordIndex5 >= 0) then
                begin
                  if (EndOfStmt(CurrentToken)) then
                  begin
                    CompletionList.AddKeyword(KeywordIndex5, KeywordIndex6, KeywordIndex7);
                    SetError(PE_IncompleteStmt);
                  end
                  else if (TokenPtr(CurrentToken)^.KeywordIndex <> KeywordIndex5) then
                    SetError(PE_UnexpectedToken)
                  else
                  begin
                    TokenPtr(CurrentToken)^.FOperatorType := otUnknown;
                    Nodes.KeywordToken5 := ApplyCurrentToken();

                    if (KeywordIndex6 >= 0) then
                    begin
                      if (EndOfStmt(CurrentToken)) then
                      begin
                         CompletionList.AddKeyword(KeywordIndex6, KeywordIndex7);
                        SetError(PE_IncompleteStmt);
                      end
                      else if (TokenPtr(CurrentToken)^.KeywordIndex <> KeywordIndex6) then
                        SetError(PE_UnexpectedToken)
                      else
                      begin
                        TokenPtr(CurrentToken)^.FOperatorType := otUnknown;
                        Nodes.KeywordToken6 := ApplyCurrentToken();

                        if (KeywordIndex7 >= 0) then
                        begin
                          if (EndOfStmt(CurrentToken)) then
                          begin
                            CompletionList.AddKeyword(KeywordIndex7);
                            SetError(PE_IncompleteStmt);
                          end
                          else if (TokenPtr(CurrentToken)^.KeywordIndex <> KeywordIndex7) then
                            SetError(PE_UnexpectedToken)
                          else
                          begin
                            TokenPtr(CurrentToken)^.FOperatorType := otUnknown;
                            Nodes.KeywordToken7 := ApplyCurrentToken();
                          end;
                        end;
                      end;
                    end;
                  end;
                end;
              end;
            end;
          end;
        end;
      end;
    end;
  end;

  Result := TTag.Create(Self, Nodes);
end;

function TSQLParser.ParseToken(): TOffset;
label
  TwoChars,
  Selection, SelSpace, SelQuotedIdent, SelNotLess, SelNotEqual1, SelNotGreater, SelNot1, SelDoubleQuote, SelComment, SelModulo, SelDolor, SelAmpersand2, SelBitAND, SelSingleQuote, SelOpenBracket, SelCloseBracket, SelMySQLCodeEnd, SelMulti, SelComma, SelDoubleDot, SelDot, SelDotNumber, SelMySQLCode, SelDiv, SelInteger, SelSLComment, SelArrow, SelMinus, SelPlus, SelAssign, SelColon, SelDelimiter, SelNULLSaveEqual, SelLessEqual, SelShiftLeft, SelNotEqual2, SelLess, SelEqual, SelGreaterEqual, SelShiftRight, SelGreater, SelParameter, SelAt, SelHex, SelHex2, SelUnquotedIdent, SelOpenSquareBracket, SelCloseSquareBracket, SelHat, SelIdent, SelMySQLIdent, SelBitValueHigh, SelBitValueLow, SelHexValueHigh, SelHexValueLow, SelUnquotedIdentLower, SelOpenCurlyBracket, SelOpenCurlyBracket2, SelPipe, SelBitOR, SelCloseCurlyBracket, SelTilde, SelE,
  SLComment, SLCommentL,
  MLComment, MLCommentL, MLCommentL2, MLCommentL3,
  Ident, IdentL, IdentL2, IdentL3, IdentLE, IdentE, IdentE2, IdentE3,
  Quoted, QuotedL, QuotedL2, QuotedLE, QuotedE, QuotedE2, QuotedE3,
    QuotedSecondQuoter, QuotedSecondQuoterL, QuotedSecondQuoterLE,
  MySQLCondCode, MySQLCondCodeL, MySQLCondCodeE,
  Numeric, NumericL, NumericExp, NumericE, NumericDot, NumericLE,
  Hex, HexL, HexL2, HexLE,
  IPAddress, IPAddressL, IPAddressLE,
  Return, Return2, ReturnE,
  Separator,
  WhiteSpace, WhiteSpaceL, WhiteSpaceLE,
  IncompleteToken, UnexpectedChar, UnexpectedCharL,
  TrippelChar,
  DoubleChar,
  SingleChar,
  Finish;
const
  Terminators: PChar = #9#10#13#32'"#%&''()*+,-./:;<=>@`{|}'; // Characters, terminating a token
  TerminatorsL = 27; // Count of Terminators
var
  AnsiQuotes: Boolean;
  DotFound: Boolean;
  EFound: Boolean;
  ErrorCode: Byte;
  ErrorPos: PChar;
  KeywordIndex: TWordList.TIndex;
  Length: Integer;
  Line: Integer;
  OperatorType: TOperatorType;
  PrefixPresent: Boolean;
  Text: PChar;
  TokenLength: Integer;
  TokenType: TTokenType;
  UsageType: TUsageType;
begin
  if (Parse.Length = 0) then
    Result := 0
  else
  begin
    AnsiQuotes := Self.AnsiQuotes;
    TokenType := ttUnknown;
    OperatorType := otUnknown;
    PrefixPresent := False;
    ErrorCode := PE_Success;
    Line := 0;
    ErrorPos := nil;
    Text := Parse.Pos;
    Length := Parse.Length;

    asm
        PUSH ES
        PUSH ESI
        PUSH EDI
        PUSH EBX

        PUSH DS                          // string operations uses ES
        POP ES
        CLD                              // string operations uses forward direction

        MOV ESI,Text
        MOV ECX,Length

      // ------------------------------

        CMP ECX,1                        // One character in SQL?
        JB IncompleteToken               // Less!
        JA TwoChars                      // More!
        MOV EAX,0                        // Hi Char in EAX
        MOV AX,[ESI]                     // One character from SQL to AX
        JMP Selection
      TwoChars:
        MOV EAX,[ESI]                    // Two characters from SQL to AX

      Selection:
        CMP AX,9                         // <Tabublator> ?
        JE WhiteSpace                    // Yes!
        CMP AX,10                        // <NewLine> ?
        JE Return                        // Yes!
        CMP AX,13                        // <CarriadgeReturn> ?
        JE Return                        // Yes!
        CMP AX,31                        // Invalid char ?
        JBE UnexpectedChar               // Yes!
      SelSpace:
        CMP AX,' '                       // <Space> ?
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
        MOV OperatorType,otUnaryNot
        JMP SingleChar
      SelDoubleQuote:
        CMP AX,'"'                       // Double Quote  ?
        JNE SelComment                   // No!
        MOV TokenType,ttDQIdent
        JMP Quoted
      SelComment:
        CMP AX,'#'                       // "#" ?
        JE SLComment                     // Yes!
      SelDolor:
        CMP AX,'$'                       // "$" ?
        JE Ident                         // Yes!
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
        JMP Quoted
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
        JNE SelMulti                     // No!
        MOV TokenType,ttMySQLCodeEnd
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
        JNE SelArrow                     // No!
        CMP ECX,3                        // Three characters in SQL?
        JB DoubleChar                    // No!
        CMP WORD PTR [ESI + 4],9         // "--<Tab>" ?
        JE SLComment                     // Yes!
        CMP WORD PTR [ESI + 4],10        // "--<LF>" ?
        JE SLComment                     // Yes!
        CMP WORD PTR [ESI + 4],13        // "--<CR>" ?
        JE SLComment                     // Yes!
        CMP WORD PTR [ESI + 4],' '       // "-- " ?
        JE SLComment                     // Yes!
        ADD ESI,4                        // Step over "--"
        SUB ECX,2                        // Two characters handled
        JMP UnexpectedChar
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
        JNE SelDotNumber                 // No!
        MOV OperatorType,otDoubleDot
        JMP DoubleChar
      SelDotNumber:
        CMP EAX,$0030002E                // ".0" ?
        JB SelDot                        // Less!
        CMP EAX,$0039002E                // ".9" ?
        JA SelDot                        // Above!
        JMP Numeric
      SelDot:
        MOV TokenType,ttDot
        MOV OperatorType,otDot
        JMP SingleChar
      SelMySQLCode:
        CMP AX,'/'                       // "/" ?
        JNE SelInteger                   // No!
        CMP EAX,$002A002F                // "/*" ?
        JNE SelDiv                       // No!
        CMP ECX,3                        // Three characters in SQL?
        JB MLComment                     // No!
        CMP WORD PTR [ESI + 4],'!'       // "/*!" ?
        JNE MLComment                    // No!
        JMP MySQLCondCode                // MySQL Code!
      SelDiv:
        MOV OperatorType,otDivision
        JMP SingleChar
      SelInteger:
        CMP AX,'9'                       // Digit?
        JBE Numeric                      // Yes!
      SelAssign:
        CMP EAX,$003D003A                // ":=" ?
        JNE SelColon                     // No!
        MOV OperatorType,otAssign2
        JMP DoubleChar
      SelColon:
        CMP AX,':'                       // ":" ?
        JNE SelDelimiter                 // No!
        MOV TokenType,ttColon
        JMP SingleChar
      SelDelimiter:
        CMP AX,';'                       // ";" ?
        JNE SelNULLSaveEqual             // No!
        MOV TokenType,ttSemicolon
        JMP SingleChar
      SelNULLSaveEqual:
        CMP AX,'<'                       // "<" ?
        JNE SelEqual                     // No!
        CMP EAX,$003D003C                // "<=" ?
        JNE SelShiftLeft                 // No!
        CMP ECX,3                        // Three characters in SQL?
        JB SelLessEqual                  // No!
        CMP WORD PTR [ESI + 4],'>'       // "<=>" ?
        JNE SelLessEqual                 // No!
        MOV OperatorType,otNULLSaveEqual
        JMP TrippelChar
      SelLessEqual:
        MOV OperatorType,otLessEqual     // "<="!
        JMP DoubleChar
      SelShiftLeft:
        CMP EAX,$003C003C                // "<<" ?
        JNE SelNotEqual2                 // No!
        MOV OperatorType,otShiftLeft
        JMP DoubleChar
      SelNotEqual2:
        CMP EAX,$003E003C                // "<>" ?
        JNE SelLess                      // No!
        MOV OperatorType,otNotEqual
        JMP DoubleChar
      SelLess:
        MOV OperatorType,otLess
        JMP SingleChar
      SelEqual:
        CMP AX,'='                       // "=" ?
        JNE SelGreaterEqual              // No!
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
        JNE SelHex                       // No!
        MOV TokenType,ttAt
        JMP SingleChar
      SelHex:
        CMP EAX,$00270078                // "x'"?
        JNE SelHex2                      // No!
        MOV DX,''''                      // Hex ends with "'"
        JMP Hex
      SelHex2:
        CMP EAX,$00270058                // "X'"?
        JNE SelUnquotedIdent             // No!
        MOV DX,''''                      // Hex ends with "'"
        JMP Hex
      SelUnquotedIdent:
        CMP AX,'Z'                       // Up case character?
        JA SelOpenSquareBracket          // No!
        MOV TokenType,ttIdent
        JMP Ident                        // Yes!
      SelOpenSquareBracket:
        CMP AX,'['                       // "[" ?
        JE UnexpectedChar                // Yes!
      SelCloseSquareBracket:
        CMP AX,']'                       // "]" ?
        JE UnexpectedChar                // Yes!
      SelHat:
        CMP AX,'^'                       // "^" ?
        JNE SelIdent                     // No!
        MOV OperatorType,otHat
        JMP SingleChar
      SelIdent:
        CMP AX,'_'                       // "_" ?
        JE Ident                         // Yes!
      SelMySQLIdent:
        CMP AX,'`'                       // "`" ?
        JNE SelBitValueHigh              // No!
        MOV TokenType,ttMySQLIdent
        JMP Quoted
      SelBitValueHigh:
        CMP EAX,$00270042                // "B'" ?
        JNE SelBitValueLow               // No!
        ADD ESI,2                        // Step over "B"
        DEC ECX                          // One character handled
        MOV TokenType,ttString
        JMP Quoted
      SelBitValueLow:
        CMP EAX,$00270062                // "b'" ?
        JNE SelHexValueHigh              // No!
        ADD ESI,2                        // Step over "b"
        DEC ECX                          // One character handled
        MOV TokenType,ttString
        JMP Quoted
      SelHexValueHigh:
        CMP EAX,$00270048                // "H'" ?
        JNE SelHexValueLow               // No!
        ADD ESI,2                        // Step over "H"
        DEC ECX                          // One character handled
        MOV TokenType,ttString
        JMP Quoted
      SelHexValueLow:
        CMP EAX,$00270068                // "h'" ?
        JNE SelUnquotedIdentLower        // No!
        ADD ESI,2                        // Step over "h"
        DEC ECX                          // One character handled
        MOV TokenType,ttString
        JMP Quoted
      SelUnquotedIdentLower:
        CMP AX,'z'                       // Low case character?
        JA SelOpenCurlyBracket           // No!
        MOV TokenType,ttIdent
        JMP Ident                        // Yes!
      SelOpenCurlyBracket:
        CMP AX,'{'                       // "{" ?
        JNE SelPipe                      // No!
        MOV TokenType,ttOpenCurlyBracket
        CMP DWORD PTR [ESI + 2],$004A004F// "{OJ" ?
        JE SelOpenCurlyBracket2          // Yes!
        CMP DWORD PTR [ESI + 2],$006A004F// "{Oj" ?
        JE SelOpenCurlyBracket2          // Yes!
        CMP DWORD PTR [ESI + 2],$004A006F// "{oJ" ?
        JE SelOpenCurlyBracket2          // Yes!
        CMP DWORD PTR [ESI + 2],$006A006F// "{oj" ?
        JE SelOpenCurlyBracket2          // Yes!
        JMP SingleChar
      SelOpenCurlyBracket2:
        CMP ECX,4                        // Four characters in SQL?
        JB SelPipe                       // No!
        PUSH EAX
        MOV AX,WORD PTR [ESI + 6]        // "{OJ " ?
        CALL Separator
        POP EAX
        JZ SingleChar                    // Yes!
      SelPipe:
        CMP AX,'|'                       // "|" ?
        JNE SelCloseCurlyBracket         // No!
        CMP EAX,$007C007C                // "||" ?
        JNE SelBitOR                     // No!
        MOV OperatorType,otPipes
        JMP DoubleChar
      SelBitOR:
        MOV OperatorType,otBitOr
        JMP SingleChar
      SelCloseCurlyBracket:
        CMP AX,'}'                       // "}" ?
        JNE SelTilde                     // No!
        MOV TokenType,ttCloseCurlyBracket
        JMP SingleChar
      SelTilde:
        CMP AX,'~'                       // "~" ?
        JNE SelE                         // No!
        MOV OperatorType,otInvertBits
        JMP SingleChar
      SelE:
        CMP AX,127                       // #127 ?
        JE UnexpectedChar                // No!
        JMP Ident

      // ------------------------------

      SLComment:
        MOV TokenType,ttSLComment
      SLCommentL:
        CMP AX,10                        // <NewLine> ?
        JE Finish                        // Yes!
        CMP AX,13                        // <CarriageReturn> ?
        JE Finish                        // Yes!
        ADD ESI,2                        // Next character in SQL
        MOV AX,[ESI]                     // One Character from SQL to AX
        LOOP SLCommentL
        JMP Finish

      // ------------------------------

      MLComment:
        MOV TokenType,ttMLComment
        ADD ESI,4                        // Step over "/*" in SQL
        SUB ECX,2                        // Two characters handled
      MLCommentL:
        CMP ECX,2                        // Two characters left in SQL?
        JAE MLCommentL2                  // Yes!
        JMP IncompleteToken
      MLCommentL2:
        MOV EAX,[ESI]                    // Load two character from SQL
        CMP AX,10                        // <NewLine>?
        JNE MLCommentL3                  // No!
        INC Line
      MLCommentL3:
        CMP EAX,$002F002A
        JE DoubleChar
        ADD ESI,2                        // Next character in SQL
        DEC ECX                          // One character handled
        JMP MLCommentL

      // ------------------------------

      Ident:
        MOV TokenType,ttIdent
        MOV PrefixPresent,True
        ADD ESI,2                        // Next character in SQL
        DEC ECX                          // One character handled
        MOV EDX,ESI
      IdentL:
        MOV AX,[ESI]                     // One Character from SQL to AX
        CMP AX,' '                       // <Space>?
        JE IdentE                        // Yes!
        CMP AX,''''                      // "'"?
        JE IdentE3                       // Yes!
        CMP AnsiQuotes,True              // AnsiQuotes?
        JE IdentL2                       // Yes!
        CMP AX,'"'                       // '"'?
        JE IdentE3                       // Yes!
      IdentL2:
        CALL Separator                   // SQL separator?
        JE Finish                        // No!
      IdentL3:
        CMP AX,'_'                       // "_"?
        JE IdentLE                       // Yes!
        CMP AX,'0'                       // Digit?
        JB IdentE                        // No!
        CMP AX,'9'
        JBE IdentLE                      // Yes!
        CMP AX,'A'                       // String character?
        JB IdentE                        // No!
        CMP AX,'Z'
        JBE IdentLE                      // Yes!
        CMP AX,'a'                       // String character?
        JB IdentE                        // No!
        CMP AX,'z'
        JBE IdentLE                      // Yes!
      IdentLE:
        ADD ESI,2                        // Next character in SQL
        LOOP IdentL
        JMP Finish
      IdentE:
        CMP ESI,EDX                      // Empty ident?
        JE IncompleteToken               // Yes!
        CMP AX,''''                      // "'"?
        JE IdentE3                       // Yes!
        CMP AnsiQuotes,True              // AnsiQuotes?
        JE IdentE2                       // Yes!
        CMP AX,'"'                       // '"'?
        JE IdentE3                       // Yes!
      IdentE2:
        JMP Finish
      IdentE3:
        MOV TokenType,ttString
        JMP Quoted

      // ------------------------------

      Quoted:
        MOV DX,[ESI]                     // End Quoter
        ADD ESI,2                        // Step over Start Quoter in SQL
        DEC ECX                          // One character handled
        JZ IncompleteToken               // End of SQL!
      QuotedL:
        MOV AX,[ESI]                     // One Character from SQL to AX
        CMP AX,10                        // <NewLine> ?
        JNE QuotedL2                     // No!
        INC Line
      QuotedL2:
        CMP AX,'\'                       // Escaper?
        JNE QuotedLE                     // No!
        CMP ECX,0                        // End of SQL?
        JE IncompleteToken               // Yes!
        ADD ESI,2                        // Next character in SQL
        DEC ECX                          // One character handled
        MOV AX,[ESI]                     // One Character from SQL to AX
        CMP AX,DX                        // Escaped End Quoter?
        JE Quoted                        // Yes!
      QuotedLE:
        CMP AX,DX                        // End Quoter (unescaped)?
        JE QuotedE                       // Yes!
        ADD ESI,2                        // One character handled
        LOOP QuotedL
        JMP IncompleteToken
      QuotedE:
        ADD ESI,2                        // Step over End Quoter in SQL
        DEC ECX                          // One character handled
        JZ Finish                        // All characters handled!
        CMP PrefixPresent,True
        JE Finish
        MOV AX,[ESI]                     // One Character from SQL to AX
        CMP DX,''''                      // Quoter = "'"?
        JE QuotedE2                      // Yes!
        CMP AnsiQuotes,True              // AnsiQuotes?
        JE QuotedE3                      // Yes!
        CMP DX,'"'                       // Quoter = '"'?
        JNE QuotedE3                     // No!
      QuotedE2:
        CMP AX,9                         // <Tabulator> ?
        JE QuotedSecondQuoter            // Yes!
        CMP AX,10                        // <NewLine> ?
        JE QuotedSecondQuoter            // Yes!
        CMP AX,13                        // <CarriadgeReturn> ?
        JE QuotedSecondQuoter            // Yes!
        CMP AX,' '                       // <Space> ?
        JE QuotedSecondQuoter            // Yes!
      QuotedE3:
        CMP AX,DX                        // A seconed End Quoter (unescaped)?
        JNE Finish                       // No!
        ADD ESI,2                        // Step over second End Quoter in SQL
        DEC ECX
        JNC QuotedL                     // Handle further characters
        JMP Finish

      QuotedSecondQuoter:
        PUSH ESI
        PUSH ECX
      QuotedSecondQuoterL:
        ADD ESI,2                        // One character handled
        DEC ECX
        JZ QuotedSecondQuoterLE          // Yes!
        MOV AX,[ESI]                     // One Character from SQL to AX
        CMP AX,9                         // <Tabulator> ?
        JE QuotedSecondQuoterL           // Yes!
        CMP AX,10                        // <NewLine> ?
        JE QuotedSecondQuoterL           // Yes!
        CMP AX,13                        // <CarriadgeReturn> ?
        JE QuotedSecondQuoterL           // Yes!
        CMP AX,' '                       // <Space> ?
        JE QuotedSecondQuoterL           // Yes!
        CMP AX,DX                        // New Quoter?
        JNE QuotedSecondQuoterLE         // No!
        POP EAX                          // Restore Stack
        POP EAX                          // Restore Stack
        CMP ECX,0                        // End of SQL?
        JE Finish                        // Yes!
        JMP Quoted
      QuotedSecondQuoterLE:
        POP ECX
        POP ESI
        JMP Finish

      // ------------------------------

      MySQLCondCode:
        MOV TokenType,ttMySQLCodeStart
        ADD ESI,6                        // Step over "/*!" in SQL
        SUB ECX,3                        // Two characters handled
        MOV EAX,0
        MOV EDX,0
      MySQLCondCodeL:
        CMP ECX,0                        // End of SQL?
        JE MySQLCondCodeE                // Yes!
        MOV AX,[ESI]                     // One Character from SQL to AX
        CMP AX,'0'                       // Digit?
        JB MySQLCondCodeE                // No!
        CMP AX,'9'                       // Digit?
        JA MySQLCondCodeE                // No!
        SUB AX,'0'                       // Str to Int
        PUSH EAX                         // EDX := EDX * 10
        MOV EAX,EDX
        MOV EDX,10
        MUL EDX
        MOV EDX,EAX
        POP EAX
        ADD EDX,EAX                      // EDX := EDX + Digit
        ADD ESI,2                        // Next character in SQL
        LOOP MySQLCondCodeL
      MySQLCondCodeE:
        JMP Finish

      // ------------------------------

      Numeric:
        MOV DotFound,False               // One dot in a numeric value allowed only
        MOV EFound,False                 // One "E" in a numeric value allowed only
        MOV TokenType,ttInteger
        MOV DX,0                         // No end-quoter in Hex
      NumericL:
        CMP AX,'.'                       // Dot?
        JE NumericDot                    // Yes!
        CALL Separator                   // SQL separator?
        JZ NumericE                      // Yes!
        CMP AX,'E'                       // "E"?
        JE NumericExp                    // Yes!
        CMP AX,'e'                       // "e"?
        JE NumericExp                    // Yes!
        CMP AX,'X'                       // "X"?
        JE Hex                           // Yes!
        CMP AX,'x'                       // "x"?
        JE Hex                           // Yes!
        CMP AX,'0'                       // Digit?
        JB NumericE                      // No!
        CMP AX,'9'
        JBE NumericLE                    // Yes!
        JMP Ident
      NumericDot:
        MOV TokenType,ttNumeric          // A dot means it's an Numeric token
        CMP EFound,False                 // A 'e' before?
        JNE UnexpectedChar               // Yes!
        CMP DotFound,False               // A dot before?
        JNE IPAddress                    // Yes!
        MOV DotFound,True
        JMP NumericLE
      NumericExp:
        MOV TokenType,ttNumeric          // A 'E' means it's an Numeric token
        CMP EFound,False                 // A 'e' before?
        JNE Ident                        // Yes!
        MOV EFound,True
      NumericLE:
        ADD ESI,2                        // Next character in SQL
        DEC ECX                          // One character handled
        JZ Finish                        // End of SQL!
        MOV AX,[ESI]                     // One Character from SQL to AX
        JMP NumericL
      NumericE:
        JMP Finish

      // ------------------------------

      Hex:
        MOV TokenType,ttString
        ADD ESI,2                        // Step over "x"
        DEC ECX                          // One character handled
        JZ IncompleteToken               // All characters handled!
        MOV AX,[ESI]                     // One Character from SQL to AX
        CMP AX,''''                      // String?
        JNE HexL
        ADD ESI,2                        // Step over "'"
        DEC ECX                          // One character handled
        JZ IncompleteToken               // All characters handled!
      HexL:
        MOV AX,[ESI]                     // One Character from SQL to AX
        CMP AX,10                        // <NewLine> ?
        JE Finish                        // Yes!
        CMP AX,13                        // <CarriageReturn> ?
        JE Finish                        // Yes!
        CMP AX,DX                        // End quoter?
        JNE HexL2
        ADD ESI,2                        // Step over "'"
        DEC ECX                          // One character handled
        JMP Finish
      HexL2:
        CALL Separator                   // SQL separator?
        JE Finish                        // Yes!
        CMP AX,'0'                       // Digit?
        JB UnexpectedChar                // No!
        CMP AX,'9'
        JBE HexLE                        // No!
        CMP AX,'A'                       // Hex Char?
        JB UnexpectedChar                // No!
        CMP AX,'F'
        JBE HexLE                        // No!
        CMP AX,'a'                       // Lower hex char?
        JB UnexpectedChar                // No!
        CMP AX,'f'
        JBE HexLE                        // No!
        JMP UnexpectedChar               // Invalid Character
      HexLE:
        ADD ESI,2                        // Next character in SQL
        LOOP HexL
        JMP Finish

      // ------------------------------

      IPAddress:
        MOV TokenType,ttIPAddress        // A dot means it's an Numeric token
      IPAddressL:
        MOV AX,[ESI]                     // One Character from SQL to AX
        CMP AX,'.'                       // Dot?
        JE IPAddressLE                   // Yes!
        CMP AX,'0'                       // Digit?
        JB UnexpectedChar                // No!
        CMP AX,'9'
        JA UnexpectedChar                // No!
      IPAddressLE:
        ADD ESI,2                        // Next character in SQL
        LOOP IPAddressL
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
        CMP AX,10                        // <NewLine> ?
        JNE Return2                      // No!
        INC Line
        JMP ReturnE
      Return2:
        CMP AX,13                        // <CarriadgeReturn> ?
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

      WhiteSpace:
        MOV TokenType,ttSpace
      WhiteSpaceL:
        CMP AX,9                         // Tabulator?
        JE WhiteSpaceLE                  // Yes!
        CMP AX,' '                       // Space?
        JNE Finish                       // No!
      WhiteSpaceLE:
        ADD ESI,2                        // Next character in SQL
        DEC ECX                          // One character handled
        CMP ECX,0                        // End of SQL?
        JE Finish                        // Yes!
        MOV AX,[ESI]                     // One Character from SQL to AX
        JMP WhiteSpaceL

      // ------------------------------

      IncompleteToken:
        MOV TokenType,ttError
        MOV ErrorCode,PE_IncompleteToken
        JMP Finish

      UnexpectedChar:
        MOV TokenType,ttError
        MOV ErrorCode,PE_UnexpectedChar
        MOV ErrorPos,ESI
        ADD ESI,2                        // Step over unexpected character
        DEC ECX                          // One character handled
        JZ Finish                        // End of SQL!
      UnexpectedCharL:
        MOV AX,[ESI]                     // One character from SQL to AX
        CALL Separator                   // Separator in SQL?
        JE Finish                        // Yes!
        ADD ESI,2                        // Next character in SQL
        LOOP UnexpectedCharL
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
        MOV EAX,Length                   // Calculate TokenLength
        SUB EAX,ECX
        MOV TokenLength,EAX

        POP EBX
        POP EDI
        POP ESI
        POP ES
    end;

    Assert((TokenLength > 0) or (ErrorCode <> PE_Success));

    if (TokenLength = 0) then
      raise Exception.Create(SUnknownError);

    if (TokenType <> ttIdent) then
      KeywordIndex := -1
    else
    begin
      KeywordIndex := KeywordList.IndexOf(Text, TokenLength);
      if (KeywordIndex >= 0) then
        OperatorType := OperatorTypeByKeywordIndex[KeywordIndex];
    end;

    if (KeywordIndex >= 0) then
      UsageType := utKeyword
    else if (OperatorType = otUnknown) then
      UsageType := UsageTypeByTokenType[TokenType]
    else
      UsageType := utOperator;

    if ((TokenType = ttUnknown) and (OperatorType <> otUnknown)) then
      TokenType := ttOperator;

    Result := TToken.Create(Self, Text, TokenLength, ErrorCode, ErrorPos, TokenType, OperatorType, KeywordIndex, UsageType);

    Parse.Pos := @Text[TokenLength];
    Dec(Parse.Length, TokenLength);
    if (not Error) then
    begin
      Inc(FErrorLine, Line);
      if (ErrorCode <> PE_Success) then
        SetError(ErrorCode, Result);
    end;
  end;
end;

function TSQLParser.ParseTriggerIdent(): TOffset;
begin
  Result := ParseDbIdent(ditTrigger);
end;

function TSQLParser.ParseTrimFunc(): TOffset;
var
  Nodes: TTrimFunc.TNodes;
begin
  FillChar(Nodes, SizeOf(Nodes), 0);

  Nodes.IdentToken := ApplyCurrentToken(utFunction);

  if (not Error) then
    Nodes.OpenBracket := ParseSymbol(ttOpenBracket);

  if (not Error and not EndOfStmt(CurrentToken)) then
    if (IsTag(kiBOTH)) then
      Nodes.DirectionTag := ParseTag(kiBOTH)
    else if (IsTag(kiLEADING)) then
      Nodes.DirectionTag := ParseTag(kiLEADING)
    else if (IsTag(kiTRAILING)) then
      Nodes.DirectionTag := ParseTag(kiTRAILING);

  if (not Error and (Nodes.DirectionTag > 0) and not IsTag(kiFROM)) then
    if (not EndOfStmt(CurrentToken) and (TokenPtr(CurrentToken)^.TokenType in ttStrings)) then
      Nodes.RemoveStr := ParseExpr();

  if (not Error and ((Nodes.DirectionTag > 0) or (Nodes.RemoveStr > 0))) then
    if (IsTag(kiFROM)) then
      Nodes.FromTag := ParseTag(kiFROM)
    else if (EndOfStmt(CurrentToken)) then
      SetError(PE_IncompleteStmt)
    else
      SetError(PE_UnexpectedToken);

  if (not Error) then
    Nodes.Str := ParseExpr();

  if (not Error and (Nodes.OpenBracket > 0)) then
    Nodes.CloseBracket := ParseSymbol(ttCloseBracket);

  Result := TTrimFunc.Create(Self, Nodes);
end;

function TSQLParser.ParseTruncateTableStmt(): TOffset;
var
  Nodes: TTruncateStmt.TNodes;
begin
  FillChar(Nodes, SizeOf(Nodes), 0);

  Nodes.StmtTag := ParseTag(kiTRUNCATE);

  if (not Error) then
    if (IsTag(kiTABLE)) then
      Nodes.TableTag := ParseTag(kiTABLE);

  if (not Error) then
    Nodes.TableIdent := ParseTableIdent();

  Result := TTruncateStmt.Create(Self, Nodes);
end;

function TSQLParser.ParseUnknownStmt(): TOffset;
var
  Tokens: Classes.TList;
begin
  Tokens := Classes.TList.Create();

  while (not EndOfStmt(CurrentToken)) do
    Tokens.Add(Pointer(ApplyCurrentToken()));

  Result := TUnknownStmt.Create(Self, Tokens.Count, TIntegerArray(Tokens.List));

  Tokens.Free();
end;

function TSQLParser.ParseUnlockTablesStmt(): TOffset;
var
  Nodes: TUnlockTablesStmt.TNodes;
begin
  FillChar(Nodes, SizeOf(Nodes), 0);

  Nodes.UnlockTablesTag := ParseTag(kiUNLOCK, kiTABLES);

  Result := TUnlockTablesStmt.Create(Self, Nodes);
end;

function TSQLParser.ParseUpdateStmt(): TOffset;
var
  Nodes: TUpdateStmt.TNodes;
  TableCount: Integer;
begin
  FillChar(Nodes, SizeOf(Nodes), 0);
  TableCount := 0;

  Nodes.UpdateTag := ParseTag(kiUPDATE);

  if (not Error) then
    if (IsTag(kiLOW_PRIORITY)) then
      Nodes.PriorityTag := ParseTag(kiLOW_PRIORITY)
    else if (IsTag(kiCONCURRENT)) then
      Nodes.PriorityTag := ParseTag(kiCONCURRENT);

  if (not Error) then
  begin
    Nodes.TableReferenceList := ParseList(False, ParseSelectStmtTableEscapedReference);
    TableCount := PList(NodePtr(Nodes.TableReferenceList))^.ElementCount;
  end;

  if (not Error) then
    Nodes.Set_.Tag := ParseTag(kiSET);

  if (not Error) then
    Nodes.Set_.Pairs := ParseList(False, ParseUpdateStmtValue);

  if (not Error) then
    if (IsTag(kiWHERE)) then
    begin
      Nodes.Where.Tag := ParseTag(kiWHERE);

      if (not Error) then
        Nodes.Where.Expr := ParseExpr();
    end;

  if (not Error and (TableCount = 1)) then
  begin
    if (IsTag(kiORDER, kiBY)) then
    begin
      Nodes.OrderBy.Tag := ParseTag(kiORDER, kiBY);

      if (not Error) then
        Nodes.OrderBy.Expr := ParseList(False, ParseSelectStmtOrderBy);
    end;

    if (not Error) then
      if (IsTag(kiLIMIT)) then
      begin
        Nodes.Limit.Tag := ParseTag(kiLIMIT);

        if (not Error) then
          Nodes.Limit.Expr := ParseExpr();
      end;
  end;

  Result := TUpdateStmt.Create(Self, Nodes);
end;

function TSQLParser.ParseUpdateStmtValue(): TOffset;
var
  Nodes: TValue.TNodes;
begin
  FillChar(Nodes, SizeOf(Nodes), 0);

  Nodes.IdentTag := ParseFieldIdentFullQualified();

  if (not Error) then
    if (EndOfStmt(CurrentToken)) then
      SetError(PE_IncompleteStmt)
    else if (not (TokenPtr(CurrentToken)^.OperatorType = otEqual)) then
      SetError(PE_UnexpectedToken)
    else
    begin
      TokenPtr(CurrentToken)^.FOperatorType := otAssign;
      Nodes.AssignToken := ApplyCurrentToken();
    end;

  if (not Error) then
    if (IsTag(kiDEFAULT)) then
      Nodes.Expr := ApplyCurrentToken() // DEFAULT
    else
      Nodes.Expr := ParseExpr();

  Result := TValue.Create(Self, Nodes);
end;

function TSQLParser.ParseUserIdent(): TOffset;
var
  Nodes: TUser.TNodes;
begin
  Result := 0;

  if (IsTag(kiCURRENT_USER)) then
    if ((EndOfStmt(NextToken[1]) or (TokenPtr(NextToken[1])^.TokenType <> ttOpenBracket))
      and ((EndOfStmt(NextToken[2]) or (TokenPtr(NextToken[2])^.TokenType <> ttCloseBracket)))) then
      Result := ApplyCurrentToken() // CURRENT_USER
    else
      Result := ParseFunctionCall() // CURRENT_USER()
  else if ((TokenPtr(CurrentToken)^.TokenType in ttIdents) or (TokenPtr(CurrentToken)^.TokenType in ttStrings)) then
  begin
    FillChar(Nodes, SizeOf(Nodes), 0);

    Nodes.NameToken := ApplyCurrentToken(utDbIdent);

    if (not Error and not EndOfStmt(CurrentToken) and (TokenPtr(CurrentToken)^.TokenType = ttAt)) then
    begin
      Nodes.AtToken := ParseSymbol(ttAt);

      if (not Error) then
        if (EndOfStmt(CurrentToken)) then
          SetError(PE_IncompleteStmt)
        else if ((TokenPtr(CurrentToken)^.TokenType = ttIPAddress)
          or (TokenPtr(CurrentToken)^.TokenType in ttIdents + ttStrings)) then
          Nodes.HostToken := ApplyCurrentToken(utDbIdent)
        else
          SetError(PE_UnexpectedToken);
    end;

    Result := TUser.Create(Self, Nodes);
  end
  else if (EndOfStmt(CurrentToken)) then
  begin
    CompletionList.AddList(ditUser);
    SetError(PE_IncompleteStmt);
  end
  else
    SetError(PE_UnexpectedToken);
end;

function TSQLParser.ParseUseStmt(): TOffset;
var
  Nodes: TUseStmt.TNodes;
begin
  FillChar(Nodes, SizeOf(Nodes), 0);

  Nodes.StmtToken := ParseTag(kiUSE);

  if (not Error) then
    Nodes.Ident := ParseDatabaseIdent();

  Result := TUseStmt.Create(Self, Nodes);
end;

function TSQLParser.ParseValue(const KeywordIndex: TWordList.TIndex;
  const Assign: TValueAssign; const Brackets: Boolean;
  const ParseItem: TParseFunction): TOffset;
var
  Nodes: TValue.TNodes;
begin
  FillChar(Nodes, SizeOf(Nodes), 0);

  Nodes.IdentTag := ParseTag(KeywordIndex);

  if (not Error and (Assign in [vaYes, vaAuto])) then
    if (EndOfStmt(CurrentToken)) then
      SetError(PE_IncompleteStmt)
    else if (TokenPtr(CurrentToken)^.OperatorType = otEqual) then
    begin
      TokenPtr(CurrentToken)^.FOperatorType := otAssign;
      Nodes.AssignToken := ApplyCurrentToken();
    end
    else if (Assign = vaYes) then
      SetError(PE_UnexpectedToken);

  if (not Error) then
    Nodes.Expr := ParseList(Brackets, ParseItem);

  Result := TValue.Create(Self, Nodes);
end;

function TSQLParser.ParseValue(const KeywordIndex: TWordList.TIndex;
  const Assign: TValueAssign; const OptionIndices: TWordList.TIndices): TOffset;
var
  I: Integer;
  Nodes: TValue.TNodes;
begin
  FillChar(Nodes, SizeOf(Nodes), 0);

  Nodes.IdentTag := ParseTag(KeywordIndex);

  if (not Error and (Assign in [vaYes, vaAuto])) then
    if (EndOfStmt(CurrentToken)) then
      SetError(PE_IncompleteStmt)
    else if (TokenPtr(CurrentToken)^.OperatorType = otEqual) then
    begin
      TokenPtr(CurrentToken)^.FOperatorType := otAssign;
      Nodes.AssignToken := ApplyCurrentToken();
    end
    else if (Assign = vaYes) then
      SetError(PE_UnexpectedToken);

  if (not Error) then
  begin
    for I := 0 to Length(OptionIndices) - 1 do
      if ((OptionIndices[I] < 0)) then
        break
      else if (OptionIndices[I] = TokenPtr(CurrentToken)^.KeywordIndex) then
      begin
        Nodes.Expr := ParseTag(TokenPtr(CurrentToken)^.KeywordIndex);
        break;
      end;
    if (Nodes.Expr = 0) then
      SetError(PE_UnexpectedToken);
  end;

  Result := TValue.Create(Self, Nodes);
end;

function TSQLParser.ParseValue(const KeywordIndex: TWordList.TIndex;
  const Assign: TValueAssign; const ParseValueNode: TParseFunction): TOffset;
var
  Nodes: TValue.TNodes;
begin
  FillChar(Nodes, SizeOf(Nodes), 0);

  Nodes.IdentTag := ParseTag(KeywordIndex);

  if (not Error and (Assign in [vaYes, vaAuto])) then
    if (EndOfStmt(CurrentToken)) then
      SetError(PE_IncompleteStmt)
    else if (TokenPtr(CurrentToken)^.OperatorType = otEqual) then
    begin
      TokenPtr(CurrentToken)^.FOperatorType := otAssign;
      Nodes.AssignToken := ApplyCurrentToken();
    end
    else if (Assign = vaYes) then
      SetError(PE_UnexpectedToken);

  if (not Error) then
    Nodes.Expr := ParseValueNode();

  Result := TValue.Create(Self, Nodes);
end;

function TSQLParser.ParseValue(const KeywordIndices: TWordList.TIndices;
  const Assign: TValueAssign; const Brackets: Boolean;
  const ParseItem: TParseFunction): TOffset;
var
  Nodes: TValue.TNodes;
begin
  Assert(KeywordIndices[4] = -1);

  FillChar(Nodes, SizeOf(Nodes), 0);

  Nodes.IdentTag := ParseTag(KeywordIndices[0], KeywordIndices[1], KeywordIndices[2], KeywordIndices[3], KeywordIndices[4], KeywordIndices[5], KeywordIndices[6]);

  if (not Error and (Assign in [vaYes, vaAuto])) then
    if (EndOfStmt(CurrentToken)) then
      SetError(PE_IncompleteStmt)
    else if (TokenPtr(CurrentToken)^.OperatorType = otEqual) then
    begin
      TokenPtr(CurrentToken)^.FOperatorType := otAssign;
      Nodes.AssignToken := ApplyCurrentToken();
    end
    else if (Assign = vaYes) then
      SetError(PE_UnexpectedToken);

  if (not Error) then
    Nodes.Expr := ParseList(Brackets, ParseItem);

  Result := TValue.Create(Self, Nodes);
end;

function TSQLParser.ParseValue(const KeywordIndices: TWordList.TIndices;
  const Assign: TValueAssign; const OptionIndices: TWordList.TIndices): TOffset;
var
  I: Integer;
  Nodes: TValue.TNodes;
begin
  FillChar(Nodes, SizeOf(Nodes), 0);

  Nodes.IdentTag := ParseTag(KeywordIndices[0], KeywordIndices[1], KeywordIndices[2], KeywordIndices[3], KeywordIndices[4], KeywordIndices[5], KeywordIndices[6]);

  if (not Error and (Assign in [vaYes, vaAuto])) then
    if (EndOfStmt(CurrentToken)) then
      SetError(PE_IncompleteStmt)
    else if (TokenPtr(CurrentToken)^.OperatorType = otEqual) then
    begin
      TokenPtr(CurrentToken)^.FOperatorType := otAssign;
      Nodes.AssignToken := ApplyCurrentToken();
    end
    else if (Assign = vaYes) then
      SetError(PE_UnexpectedToken);

  if (not Error) then
  begin
    for I := 0 to Length(OptionIndices) - 1 do
      if ((OptionIndices[I] < 0)) then
        break
      else if (OptionIndices[I] = TokenPtr(CurrentToken)^.KeywordIndex) then
      begin
        Nodes.Expr := ParseTag(TokenPtr(CurrentToken)^.KeywordIndex);
        break;
      end;
    if (Nodes.Expr = 0) then
      SetError(PE_UnexpectedToken);
  end;

  Result := TValue.Create(Self, Nodes);
end;

function TSQLParser.ParseValue(const KeywordIndices: TWordList.TIndices; const Assign: TValueAssign; const ParseValueNode: TParseFunction): TOffset;
var
  Nodes: TValue.TNodes;
begin
  Assert(KeywordIndices[4] = -1);

  FillChar(Nodes, SizeOf(Nodes), 0);

  Nodes.IdentTag := ParseTag(KeywordIndices[0], KeywordIndices[1], KeywordIndices[2], KeywordIndices[3], KeywordIndices[4], KeywordIndices[5], KeywordIndices[6]);

  if (not Error and (Assign in [vaYes, vaAuto])) then
    if (EndOfStmt(CurrentToken)) then
      SetError(PE_IncompleteStmt)
    else if (TokenPtr(CurrentToken)^.OperatorType = otEqual) then
    begin
      TokenPtr(CurrentToken)^.FOperatorType := otAssign;
      Nodes.AssignToken := ApplyCurrentToken();
    end
    else if (Assign = vaYes) then
      SetError(PE_UnexpectedToken);

  if (not Error) then
    Nodes.Expr := ParseValueNode();

  Result := TValue.Create(Self, Nodes);
end;

function TSQLParser.ParseValue(const KeywordIndices: TWordList.TIndices; const Assign: TValueAssign; const ValueKeywordIndex1: TWordList.TIndex; const ValueKeywordIndex2: TWordList.TIndex = -1): TOffset;
var
  Nodes: TValue.TNodes;
begin
  Assert(KeywordIndices[4] = -1);

  FillChar(Nodes, SizeOf(Nodes), 0);

  Nodes.IdentTag := ParseTag(KeywordIndices[0], KeywordIndices[1], KeywordIndices[2], KeywordIndices[3], KeywordIndices[4], KeywordIndices[5], KeywordIndices[6]);

  if (not Error and (Assign in [vaYes, vaAuto])) then
    if (EndOfStmt(CurrentToken)) then
      SetError(PE_IncompleteStmt)
    else if (TokenPtr(CurrentToken)^.OperatorType = otEqual) then
    begin
      TokenPtr(CurrentToken)^.FOperatorType := otAssign;
      Nodes.AssignToken := ApplyCurrentToken();
    end
    else if (Assign = vaYes) then
      SetError(PE_UnexpectedToken);

  if (not Error) then
    Nodes.Expr := ParseTag(ValueKeywordIndex1, ValueKeywordIndex2);

  Result := TValue.Create(Self, Nodes);
end;

function TSQLParser.ParseVariableIdent(): TOffset;
var
  Nodes: TVariable.TNodes;
begin
  FillChar(Nodes, SizeOf(Nodes), 0);

  if (IsSymbol(ttAt)) then
  begin
    Nodes.At1Token := ParseSymbol(ttAt);

    if (not Error and IsSymbol(ttAt)) then
    begin
      Nodes.At2Token := Nodes.At1Token;
      Nodes.At1Token := ParseSymbol(ttAt);
    end;
  end;

  if (not Error and (Nodes.At1Token > 0)) then
    if (IsTag(kiGLOBAL) or IsTag(kiSESSION) or IsTag(kiLOCAL)) then
    begin
      Nodes.ScopeTag := ParseTag(TokenPtr(CurrentToken)^.KeywordIndex);

      if (not Error) then
        if (EndOfStmt(CurrentToken)) then
          SetError(PE_IncompleteStmt)
        else if (TokenPtr(CurrentToken)^.OperatorType <> otDot) then
          SetError(PE_UnexpectedToken)
        else
          Nodes.ScopeDotToken := ApplyCurrentToken();
    end;

  if (not Error) then
    if (EndOfStmt(CurrentToken)) then
      SetError(PE_IncompleteStmt)
    else if (not EndOfStmt(NextToken[1]) and (TokenPtr(NextToken[1])^.TokenType = ttDot)) then
      Nodes.Ident := ParseList(False, ApplyCurrentToken, ttDot)
    else
      Nodes.Ident := ApplyCurrentToken();

  Result := TVariable.Create(Self, Nodes);
end;

function TSQLParser.ParseWeightStringFunc(): TOffset;
var
  Nodes: TWeightStringFunc.TNodes;
begin
  FillChar(Nodes, SizeOf(Nodes), 0);

  Nodes.IdentToken := ApplyCurrentToken(utFunction);

  if (not Error) then
    Nodes.OpenBracket := ParseSymbol(ttOpenBracket);

  if (not Error) then
    Nodes.Str := ParseString();

  if (not Error) then
    if (IsTag(kiAS)) then
    begin
      Nodes.AsTag := ParseTag(kiAS);

      if (not Error) then
        Nodes.Datatype := ParseDatatype();
    end;

  if (not Error) then
    if (IsTag(kiLEVEL)) then
    begin
      Nodes.AsTag := ParseTag(kiLEVEL);

      if (not Error) then
        Nodes.LevelList := ParseList(False, ParseWeightStringFuncLevel);
    end;

  if (not Error) then
    Nodes.CloseBracket := ParseSymbol(ttCloseBracket);

  Result := TWeightStringFunc.Create(Self, Nodes);
end;

function TSQLParser.ParseWeightStringFuncLevel(): TOffset;
var
  Nodes: TWeightStringFunc.TLevel.TNodes;
begin
  FillChar(Nodes, SizeOf(Nodes), 0);

  Nodes.CountInt := ParseInteger();

  if (not Error) then
    if (IsTag(kiASC)) then
      Nodes.DirectionTag := ParseTag(kiASC)
    else if (IsTag(kiDESC)) then
      Nodes.DirectionTag := ParseTag(kiDESC);

  if (not Error) then
    if (IsTag(kiREVERSE)) then
      Nodes.DirectionTag := ParseTag(kiREVERSE);

  Result := TWeightStringFunc.TLevel.Create(Self, Nodes);
end;

function TSQLParser.ParseWhileStmt(): TOffset;
var
  Nodes: TWhileStmt.TNodes;
begin
  FillChar(Nodes, SizeOf(Nodes), 0);

  if (not EndOfStmt(CurrentToken) and (TokenPtr(CurrentToken)^.TokenType = ttIdent)
    and not EndOfStmt(NextToken[1]) and (TokenPtr(NextToken[1])^.TokenType = ttColon)) then
    Nodes.BeginLabel := ParseBeginLabel();

  if (not Error) then
    Nodes.WhileTag := ParseTag(kiWHILE);

  if (not Error) then
    Nodes.SearchConditionExpr := ParseExpr();

  if (not Error) then
    Nodes.DoTag := ParseTag(kiDO);

  if (not Error and not IsTag(kiEND)) then
    Nodes.StmtList := ParseList(False, ParsePL_SQLStmt, ttSemicolon);

  if (not Error) then
    Nodes.EndTag := ParseTag(kiEND, kiWHILE);

  if (not Error
    and not EndOfStmt(CurrentToken) and (TokenPtr(CurrentToken)^.TokenType = ttIdent)
    and (Nodes.BeginLabel > 0) and (NodePtr(Nodes.BeginLabel)^.NodeType = ntBeginLabel)) then
    if ((Nodes.BeginLabel = 0) or (lstrcmpi(PChar(TokenPtr(CurrentToken)^.AsString), PChar(PBeginLabel(NodePtr(Nodes.BeginLabel))^.LabelName)) <> 0)) then
      SetError(PE_UnexpectedToken)
    else
      Nodes.EndLabel := ParseEndLabel();

  Result := TWhileStmt.Create(Self, Nodes);
end;

function TSQLParser.ParseXAStmt(): TOffset;

  function ParseXID(): TOffset;
  var
    Nodes: TXAStmt.TID.TNodes;
  begin
    FillChar(Nodes, SizeOf(Nodes), 0);

    Nodes.GTrId := ParseString();

    if (not Error) then
      if (IsSymbol(ttComma)) then
      begin
        Nodes.Comma1 := ParseSymbol(ttComma);

        if (not Error) then
          Nodes.BQual := ParseString();

        if (not Error) then
          if (IsSymbol(ttComma)) then
          begin
            Nodes.Comma1 := ParseSymbol(ttComma);

            if (not Error) then
              Nodes.FormatId := ParseInteger();
          end;
      end;

    Result := TXAStmt.TID.Create(Self, Nodes);
  end;

var
  Nodes: TXAStmt.TNodes;
begin
  FillChar(Nodes, SizeOf(Nodes), 0);

  Nodes.XATag := ParseTag(kiXA);

  if (not Error) then
    if (IsTag(kiBEGIN)
      or IsTag(kiSTART)) then
    begin
      Nodes.ActionTag := ParseTag(TokenPtr(CurrentToken)^.KeywordIndex);

      if (not Error) then
        Nodes.Ident := ParseXID();

      if (not Error) then
        if (IsTag(kiJOIN)) then
          Nodes.RestTag := ParseTag(kiJOIN)
        else if (IsTag(kiRESUME)) then
          Nodes.RestTag := ParseTag(kiRESUME);
    end
    else if (IsTag(kiCOMMIT)) then
    begin
      Nodes.ActionTag := ParseTag(kiCOMMIT);

      if (not Error) then
        Nodes.Ident := ParseXID();

      if (not Error) then
        if (IsTag(kiONE, kiPHASE)) then
          Nodes.RestTag := ParseTag(kiONE, kiPHASE);
    end
    else if (IsTag(kiEND)) then
    begin
      Nodes.ActionTag := ParseTag(kiEND);

      if (not Error) then
        Nodes.Ident := ParseXID();

      if (not Error) then
        if (IsTag(kiSUSPEND, kiFOR, kiMIGRATE)) then
          Nodes.RestTag := ParseTag(kiSUSPEND, kiFOR, kiMIGRATE)
        else if (IsTag(kiSUSPEND)) then
          Nodes.RestTag := ParseTag(kiSUSPEND);
    end
    else if (IsTag(kiPREPARE)) then
    begin
      Nodes.ActionTag := ParseTag(kiPREPARE);

      if (not Error) then
        Nodes.Ident := ParseXID();
    end
    else if (IsTag(kiRECOVER)) then
    begin
      Nodes.ActionTag := ParseTag(kiRECOVER);

      if (not Error) then
        if (IsTag(kiCONVERT, kiXID)) then
          Nodes.RestTag := ParseTag(kiCONVERT, kiXID);
    end
    else if (TokenPtr(CurrentToken)^.KeywordIndex = kiROLLBACK) then
    begin
      Nodes.ActionTag := ParseTag(kiROLLBACK);

      if (not Error) then
        Nodes.Ident := ParseXID();
    end
    else if (EndOfStmt(CurrentToken)) then
      SetError(PE_IncompleteStmt)
    else
      SetError(PE_UnexpectedToken);

  Result := TXAStmt.Create(Self, Nodes);
end;

procedure TSQLParser.SaveToFile(const Filename: string; const FileType: TFileType = ftSQL);
begin
  if (not Assigned(Root)) then
    raise Exception.Create('Empty Buffer');
  case (FileType) of
    ftSQL:
      SaveToSQLFile(Filename);
    ftFormatedSQL:
      SaveToFormatedSQLFile(Filename);
    ftDebugHTML:
      SaveToDebugHTMLFile(Filename);
  end;
end;

procedure TSQLParser.SaveToDebugHTMLFile(const Filename: string);
var
  G: Integer;
  Generation: Integer;
  GenerationCount: Integer;
  Handle: THandle;
  HTML: string;
  LastTokenIndex: Integer;
  Length: Integer;
  Node: PNode;
  ParentNodes: Classes.TList;
  S: string;
  Size: Cardinal;
  SQL: string;
  Stmt: PStmt;
  Text: PChar;
  Token: PToken;
begin
  Handle := CreateFile(PChar(Filename),
                       GENERIC_WRITE,
                       FILE_SHARE_READ,
                       nil,
                       CREATE_ALWAYS, FILE_ATTRIBUTE_NORMAL, 0);

  if (Handle = INVALID_HANDLE_VALUE) then
    RaiseLastOSError()
  else
  begin
    if (not WriteFile(Handle, PChar(BOM_UNICODE_LE)^, StrLen(BOM_UNICODE_LE), Size, nil)) then
      RaiseLastOSError();

    HTML :=
      '<!DOCTYPE HTML PUBLIC "-//W3C//DTD HTML 4.01 Transitional//EN">' + #13#10 +
      '<html>' + #13#10 +
      '  <head>' + #13#10 +
      '  <meta http-equiv="content-type" content="text/html">' + #13#10 +
      '  <title>Debug - SQL Parser</title>' + #13#10 +
      '  <style type="text/css">' + #13#10 +
      '    body {' + #13#10 +
      '      font: 12px Verdana,Arial,Sans-Serif;' + #13#10 +
      '      color: #000;' + #13#10 +
      '    }' + #13#10 +
      '    td {' + #13#10 +
      '      font: 12px Verdana,Arial,Sans-Serif;' + #13#10 +
      '    }' + #13#10 +
      '    a {' + #13#10 +
      '      text-decoration: none;' + #13#10 +
      '    }' + #13#10 +
      '    a:link span { display: none; }' + #13#10 +
      '    a:visited span { display: none; }' + #13#10 +
      '    a:hover span {' + #13#10 +
      '      display: block;' + #13#10 +
      '      position: absolute;' + #13#10 +
      '      margin: 18px 0px 0px 0px;' + #13#10 +
      '      background-color: #FFD;' + #13#10 +
      '      padding: 0px 2px 2px 1px;' + #13#10 +
      '      border: 1px solid #000;' + #13#10 +
      '      color: #000;' + #13#10 +
      '    }' + #13#10 +
      '    .Node {' + #13#10 +
      '      font-size: 11px;' + #13#10 +
      '      text-align: center;' + #13#10 +
      '      background-color: #F0F0F0;' + #13#10 +
      '    }' + #13#10 +
      '    .SQL {' + #13#10 +
      '      font-size: 12px;' + #13#10 +
      '      background-color: #F0F0F0;' + #13#10 +
      '      text-align: center;' + #13#10 +
      '    }' + #13#10 +
      '    .TokenError {' + #13#10 +
      '      font-size: 12px;' + #13#10 +
      '      background-color: #FFC0C0;' + #13#10 +
      '      text-align: center;' + #13#10 +
      '    }' + #13#10 +
      '    .StmtOk {' + #13#10 +
      '      font-size: 12px;' + #13#10 +
      '      background-color: #D0FFD0;' + #13#10 +
      '      text-align: center;' + #13#10 +
      '    }' + #13#10 +
      '    .StmtError {' + #13#10 +
      '      font-size: 12px;' + #13#10 +
      '      background-color: #FFC0C0;' + #13#10 +
      '      text-align: center;' + #13#10 +
      '    }' + #13#10 +
      '    .ErrorMessage {' + #13#10 +
      '      font-size: 12px;' + #13#10 +
      '      color: #E82020;' + #13#10 +
      '      font-weight:bold;' + #13#10 +
      '    }' + #13#10 +
      '  </style>' + #13#10 +
      '  </head>' + #13#10 +
      '  <body>' + #13#10;

    if (not WriteFile(Handle, PChar(HTML)^, System.Length(HTML) * SizeOf(HTML[1]), Size, nil)) then
      RaiseLastOSError()
    else
      HTML := '';

    Stmt := Root^.FirstStmt;
    while (Assigned(Stmt)) do
    begin
      Token := Stmt^.FirstToken; GenerationCount := 0;
      while (Assigned(Token)) do
      begin
        GenerationCount := Max(GenerationCount, Token^.Generation);
        if (Token = Stmt^.LastToken) then
          Token := nil
        else
          Token := Token^.NextToken;
      end;

      ParentNodes := Classes.TList.Create();
      ParentNodes.Add(Root);

      HTML := HTML
        + '<table cellspacing="2" cellpadding="0" border="0">' + #13#10;

      for Generation := 0 to GenerationCount - 1 do
      begin
        HTML := HTML
          + '<tr>' + #13#10;
        Token := Stmt^.FirstToken;
        LastTokenIndex := Token^.Index - 1;
        while (Assigned(Token)) do
        begin
          Node := Token^.ParentNode; G := Token^.Generation;
          while (IsChild(Node) and (G > Generation)) do
          begin
            Dec(G);
            if (G > Generation) then
              Node := PChild(Node)^.ParentNode;
          end;

          if (IsChild(Node) and (G = Generation) and (ParentNodes.IndexOf(Node) < 1)) then
          begin
            if (PChild(Node)^.FirstToken^.Index - LastTokenIndex - 1 > 0) then
              HTML := HTML
                + '<td colspan="' + IntToStr(PChild(Node)^.FirstToken^.Index - LastTokenIndex - 1) + '"></td>';
            HTML := HTML
              + '<td colspan="' + IntToStr(PChild(Node)^.LastToken^.Index - PChild(Node)^.FirstToken^.Index + 1) + '" class="Node">';
            HTML := HTML
              + '<a href="">'
              + HTMLEscape(NodeTypeToString[Node^.NodeType]);
            HTML := HTML
              + '<span><table cellspacing="2" cellpadding="0">';
            if (Assigned(PChild(Node)^.ParentNode)) then
              HTML := HTML
                + '<tr><td>ParentNode Offset:</td><td>&nbsp;</td><td>' + IntToStr(PChild(Node)^.ParentNode^.Offset) + '</td></tr>';
            HTML := HTML
              + '<tr><td>Offset:</td><td>&nbsp;</td><td>' + IntToStr(Node^.Offset) + '</td></tr>';
            if (IsStmt(Node)) then
              HTML := HTML + '<tr><td>StmtType:</td><td>&nbsp;</td><td>' + StmtTypeToString[PStmt(Node)^.StmtType] + '</td></tr>'
            else
              case (Node^.NodeType) of
                ntAlterRoutineStmt:
                  HTML := HTML
                    + '<tr><td>AlterRoutineType:</td><td>&nbsp;</td><td>' + RoutineTypeToString[PAlterRoutineStmt(Node)^.RoutineType] + '</td></tr>';
                ntBinaryOp:
                  if (IsToken(PNode(PBinaryOp(Node)^.Operator))) then
                    HTML := HTML
                      + '<tr><td>OperatorType:</td><td>&nbsp;</td><td>' + OperatorTypeToString[PToken(PBinaryOp(Node)^.Operator)^.OperatorType] + '</td></tr>';
                ntCreateRoutineStmt:
                  HTML := HTML
                    + '<tr><td>RoutineType:</td><td>&nbsp;</td><td>' + RoutineTypeToString[PCreateRoutineStmt(Node)^.RoutineType] + '</td></tr>';
                ntDropRoutineStmt:
                  HTML := HTML
                    + '<tr><td>RoutineType:</td><td>&nbsp;</td><td>' + RoutineTypeToString[PDropRoutineStmt(Node)^.RoutineType] + '</td></tr>';
                ntDbIdent:
                  HTML := HTML
                    + '<tr><td>DbIdentType:</td><td>&nbsp;</td><td>' + DbIdentTypeToString[PDbIdent(Node)^.DbIdentType] + '</td></tr>';
                ntList:
                  if (Assigned(PList(Node)^.FirstChild)) then
                    HTML := HTML
                      + '<tr><td>FirstChild Offset:</td><td>&nbsp;</td><td>' + IntToStr(PNode(PList(Node)^.FirstChild)^.Offset) + '</td></tr>'
                      + '<tr><td>ElementCount:</td><td>&nbsp;</td><td>' + IntToStr(PList(Node)^.ElementCount) + '</td></tr>';
                ntSelectStmtTableJoin:
                  HTML := HTML
                    + '<tr><td>JoinType:</td><td>&nbsp;</td><td>' + JoinTypeToString[TSelectStmt.PTableJoin(Node)^.JoinType] + '</td></tr>';
              end;
            HTML := HTML
              + '</table></span>';
            HTML := HTML
              + '</a></td>' + #13#10;

            LastTokenIndex := PChild(Node)^.LastToken^.Index;

            ParentNodes.Add(Node);
            Token := PChild(Node)^.LastToken;
          end;

          if (Token <> Stmt^.LastToken) then
            Token := Token^.NextToken
          else
          begin
            if (Token^.Index - LastTokenIndex > 0) then
              HTML := HTML
                + '<td colspan="' + IntToStr(Token^.Index - LastTokenIndex) + '"></td>';
            Token := nil;
          end;
        end;
        HTML := HTML
          + '</tr>' + #13#10;
      end;

      ParentNodes.Free();


      if (Stmt^.ErrorCode <> PE_Success) then
        HTML := HTML
          + '<tr class="SQL">' + #13#10
      else
        HTML := HTML
          + '<tr class="StmtOk">' + #13#10;

      Token := Stmt^.FirstToken;
      while (Assigned(Token)) do
      begin
        Token^.GetText(Text, Length);
        SetString(S, Text, Length);
        if (Token^.ErrorCode = PE_Success) then
          HTML := HTML
            + '<td>'
        else
          HTML := HTML
            + '<td class="TokenError">';
        HTML := HTML
          + '<a href="">';
        HTML := HTML
          + '<code>' + HTMLEscape(ReplaceStr(S, ' ', '&nbsp;')) + '</code>';
        HTML := HTML
          + '<span><table cellspacing="2" cellpadding="0">';
        if (Assigned(PChild(Token)^.ParentNode)) then
          HTML := HTML + '<tr><td>ParentNode Offset:</td><td>&nbsp;</td><td>' + IntToStr(PChild(Token)^.ParentNode^.Offset) + '</td></tr>';
        HTML := HTML + '<tr><td>Offset:</td><td>&nbsp;</td><td>' + IntToStr(PNode(Token)^.Offset) + '</td></tr>';
        HTML := HTML + '<tr><td>TokenType:</td><td>&nbsp;</td><td>' + HTMLEscape(TokenTypeToString[Token^.TokenType]) + '</td></tr>';
        if (Token^.KeywordIndex >= 0) then
          HTML := HTML + '<tr><td>KeywordIndex:</td><td>&nbsp;</td><td>ki' + HTMLEscape(KeywordList[Token^.KeywordIndex]) + '</td></tr>';
        if (Token^.OperatorType <> otUnknown) then
          HTML := HTML + '<tr><td>OperatorType:</td><td>&nbsp;</td><td>' + HTMLEscape(OperatorTypeToString[Token^.OperatorType]) + '</td></tr>';
        if (Token^.DbIdentType <> ditUnknown) then
          HTML := HTML + '<tr><td>DbIdentType:</td><td>&nbsp;</td><td>' + HTMLEscape(DbIdentTypeToString[Token^.DbIdentType]) + '</td></tr>';
        if ((Trim(Token^.AsString) <> '') and (Token^.KeywordIndex < 0)) then
          HTML := HTML + '<tr><td>AsString:</td><td>&nbsp;</td><td>' + HTMLEscape(Token^.AsString) + '</td></tr>';
        if (Token^.ErrorCode <> PE_Success) then
          HTML := HTML + '<tr><td>ErrorCode:</td><td>&nbsp;</td><td>' + IntToStr(Token^.ErrorCode) + '</td></tr>';
        if (Token^.UsageType <> utUnknown) then
          HTML := HTML + '<tr><td>UsageType:</td><td>&nbsp;</td><td>' + HTMLEscape(UsageTypeToString[Token^.UsageType]) + '</td></tr>';
        HTML := HTML
          + '</table></span>';
        HTML := HTML
          + '</a></td>' + #13#10;

        if (Token = Stmt^.LastToken) then
          Token := nil
        else
          Token := Token.NextToken;
      end;
      HTML := HTML
        + '</tr>' + #13#10;

      if ((Stmt^.ErrorCode <> PE_Success) and (Stmt^.ErrorCode <> PE_IncompleteStmt) and Assigned(Stmt^.ErrorToken)) then
      begin
        HTML := HTML
          + '<tr>';
        if (Stmt^.ErrorToken^.Index - Stmt^.FirstToken^.Index > 0) then
          HTML := HTML
            + '<td colspan="' + IntToStr(Stmt^.ErrorToken^.Index - Stmt^.FirstToken^.Index) + '"></td>';
        HTML := HTML
          + '<td class="StmtError"><a href="">&uarr;'
          + '<span><table cellspacing="2" cellpadding="0">'
          + '<tr><td>ErrorCode:</td><td>' + IntToStr(Stmt^.ErrorCode) + '</td></tr>'
          + '<tr><td>ErrorMessage:</td><td>' + HTMLEscape(Stmt^.ErrorMessage) + '</td></tr>'
          + '</table></span>'
          + '</a></td>';
        if (Stmt^.ErrorToken^.Index - Stmt^.LastToken^.Index > 0) then
          HTML := HTML
            + '<td colspan="' + IntToStr(Stmt^.LastToken^.Index - Stmt^.ErrorToken^.Index) + '"></td>';
        HTML := HTML
          + '</tr>' + #13#10;
      end;

      HTML := HTML
        + '</table>' + #13#10;

      if (Stmt^.ErrorCode <> PE_Success) then
        HTML := HTML
          + '<div class="ErrorMessage">' + HTMLEscape('Error: ' + ReplaceStr(ReplaceStr(Stmt^.ErrorMessage, #10, ''), #13, '')) + '</div>';

      Stmt := Stmt^.NextStmt;

      if (Assigned(Stmt)) then
        HTML := HTML
          + '<br><br>' + #13#10;

      if (not WriteFile(Handle, PChar(HTML)^, System.Length(HTML) * SizeOf(HTML[1]), Size, nil)) then
        RaiseLastOSError()
      else
        HTML := '';
    end;

    FormatSQL(SQL);
    HTML := HTML
      + '<br><br>'
      + '<code>' + HTMLEscape(ReplaceStr(SQL, ' ', '&nbsp;')) + '</code>';

    HTML := HTML
      + '    <br>' + #13#10
      + '    <br>' + #13#10
      + '  </body>' + #13#10
      + '</html>';

    if (not WriteFile(Handle, PChar(HTML)^, System.Length(HTML) * SizeOf(HTML[1]), Size, nil)) then
      RaiseLastOSError();

    if (not CloseHandle(Handle)) then
      RaiseLastOSError();
  end;
end;

procedure TSQLParser.SaveToFormatedSQLFile(const Filename: string);
var
  Handle: THandle;
  Size: Cardinal;
  SQL: string;
begin
  Handle := CreateFile(PChar(Filename),
                       GENERIC_WRITE,
                       FILE_SHARE_READ,
                       nil,
                       CREATE_ALWAYS, FILE_ATTRIBUTE_NORMAL, 0);

  if (Handle = INVALID_HANDLE_VALUE) then
    RaiseLastOSError();

  FormatSQL(SQL);

  if (not WriteFile(Handle, PChar(BOM_UNICODE_LE)^, StrLen(BOM_UNICODE_LE), Size, nil)
    or not WriteFile(Handle, PChar(SQL)^, Length(SQL) * SizeOf(SQL[1]), Size, nil)
    or not CloseHandle(Handle)) then
    RaiseLastOSError();
end;

procedure TSQLParser.SaveToSQLFile(const Filename: string);
var
  Handle: THandle;
  Length: Integer;
  Size: Cardinal;
  StringBuffer: TStringBuffer;
  Text: PChar;
  Token: PToken;
begin
  StringBuffer := TStringBuffer.Create(1024);

  Token := TokenPtr(1);
  while (Assigned(Token)) do
  begin
    Token^.GetText(Text, Length);
    StringBuffer.Write(Text, Length);
    Token := Token^.NextTokenAll;
  end;

  Handle := CreateFile(PChar(Filename),
                       GENERIC_WRITE,
                       FILE_SHARE_READ,
                       nil,
                       CREATE_ALWAYS, FILE_ATTRIBUTE_NORMAL, 0);

  if (Handle = INVALID_HANDLE_VALUE) then
    RaiseLastOSError();

  if (not WriteFile(Handle, PChar(BOM_UNICODE_LE)^, StrLen(BOM_UNICODE_LE), Size, nil)
    or not WriteFile(Handle, StringBuffer.Data^, StringBuffer.Size, Size, nil)
    or not CloseHandle(Handle)) then
    RaiseLastOSError();

  StringBuffer.Free();
end;

procedure TSQLParser.SetDatatypes(ADatatypes: string);

  function IndexOf(const Word: PChar): Integer;
  begin
    Result := DatatypeList.IndexOf(Word, StrLen(Word));

    if (Result < 0) then
      raise ERangeError.CreateFmt(SDatatypeNotFound, [Word]);
  end;

begin
  DatatypeList.Text := ADatatypes;

  if (ADatatypes <> '') then
  begin
    diBIGINT               := IndexOf('BIGINT');
    diBINARY               := IndexOf('BINARY');
    diBIT                  := IndexOf('BIT');
    diBLOB                 := IndexOf('BLOB');
    diBOOL                 := IndexOf('BOOL');
    diBOOLEAN              := IndexOf('BOOLEAN');
    diCHAR                 := IndexOf('CHAR');
    diCHARACTER            := IndexOf('CHARACTER');
    diDEC                  := IndexOf('DEC');
    diDECIMAL              := IndexOf('DECIMAL');
    diDATE                 := IndexOf('DATE');
    diDATETIME             := IndexOf('DATETIME');
    diDOUBLE               := IndexOf('DOUBLE');
    diENUM                 := IndexOf('ENUM');
    diFLOAT                := IndexOf('FLOAT');
    diGEOMETRY             := IndexOf('GEOMETRY');
    diGEOMETRYCOLLECTION   := IndexOf('GEOMETRYCOLLECTION');
    diINT                  := IndexOf('INT');
    diINT4                 := IndexOf('INT4');
    diINTEGER              := IndexOf('INTEGER');
    diJSON                 := IndexOf('JSON');
    diLARGEINT             := IndexOf('LARGEINT');
    diLINESTRING           := IndexOf('LINESTRING');
    diLONG                 := IndexOf('LONG');
    diLONGBLOB             := IndexOf('LONGBLOB');
    diLONGTEXT             := IndexOf('LONGTEXT');
    diMEDIUMBLOB           := IndexOf('MEDIUMBLOB');
    diMEDIUMINT            := IndexOf('MEDIUMINT');
    diMEDIUMTEXT           := IndexOf('MEDIUMTEXT');
    diMULTILINESTRING      := IndexOf('MULTILINESTRING');
    diMULTIPOINT           := IndexOf('MULTIPOINT');
    diMULTIPOLYGON         := IndexOf('MULTIPOLYGON');
    diNUMBER               := IndexOf('NUMBER');
    diNUMERIC              := IndexOf('NUMERIC');
    diNCHAR                := IndexOf('NCHAR');
    diNVARCHAR             := IndexOf('NVARCHAR');
    diPOINT                := IndexOf('POINT');
    diPOLYGON              := IndexOf('POLYGON');
    diREAL                 := IndexOf('REAL');
    diSERIAL               := IndexOf('SERIAL');
    diSET                  := IndexOf('SET');
    diSIGNED               := IndexOf('SIGNED');
    diSMALLINT             := IndexOf('SMALLINT');
    diTEXT                 := IndexOf('TEXT');
    diTIME                 := IndexOf('TIME');
    diTIMESTAMP            := IndexOf('TIMESTAMP');
    diTINYBLOB             := IndexOf('TINYBLOB');
    diTINYINT              := IndexOf('TINYINT');
    diTINYTEXT             := IndexOf('TINYTEXT');
    diUNSIGNED             := IndexOf('UNSIGNED');
    diVARBINARY            := IndexOf('VARBINARY');
    diVARCHAR              := IndexOf('VARCHAR');
    diYEAR                 := IndexOf('YEAR');
  end;
end;

procedure TSQLParser.SetError(const AErrorCode: Byte; const AErrorToken: TOffset = 0);
begin
  Assert(not Error and ((AErrorCode <> PE_IncompleteStmt) or (AErrorToken = 0) or IsToken(AErrorToken)));

  FErrorCode := AErrorCode;

  if (not IsChild(AErrorToken)) then
    FErrorToken := CurrentToken
  else
    FErrorToken := ChildPtr(AErrorToken)^.FFirstToken;
end;

procedure TSQLParser.SetFunctions(AFunctions: string);
begin
  FunctionList.Text := AFunctions;
end;

procedure TSQLParser.SetKeywords(AKeywords: string);

  function IndexOf(const Word: PChar): Integer;
  begin
    Result := KeywordList.IndexOf(Word, StrLen(Word));

    if (Result < 0) then
      raise ERangeError.CreateFmt(SKeywordNotFound, [Word]);
  end;

var
  Index: Integer;
begin
  KeywordList.Text := AKeywords;

  if (AKeywords <> '') then
  begin
    kiACCOUNT                  := IndexOf('ACCOUNT');
    kiACTION                   := IndexOf('ACTION');
    kiADD                      := IndexOf('ADD');
    kiAFTER                    := IndexOf('AFTER');
    kiAGAINST                  := IndexOf('AGAINST');
    kiALGORITHM                := IndexOf('ALGORITHM');
    kiALL                      := IndexOf('ALL');
    kiALTER                    := IndexOf('ALTER');
    kiALWAYS                   := IndexOf('ALWAYS');
    kiANALYZE                  := IndexOf('ANALYZE');
    kiAND                      := IndexOf('AND');
    kiANY                      := IndexOf('ANY');
    kiAS                       := IndexOf('AS');
    kiASC                      := IndexOf('ASC');
    kiASCII                    := IndexOf('ASCII');
    kiAT                       := IndexOf('AT');
    kiAUTO_INCREMENT           := IndexOf('AUTO_INCREMENT');
    kiAUTHORS                  := IndexOf('AUTHORS');
    kiAVG_ROW_LENGTH           := IndexOf('AVG_ROW_LENGTH');
    kiBEFORE                   := IndexOf('BEFORE');
    kiBEGIN                    := IndexOf('BEGIN');
    kiBETWEEN                  := IndexOf('BETWEEN');
    kiBINARY                   := IndexOf('BINARY');
    kiBINLOG                   := IndexOf('BINLOG');
    kiBLOCK                    := IndexOf('BLOCK');
    kiBOOLEAN                  := IndexOf('BOOLEAN');
    kiBOTH                     := IndexOf('BOTH');
    kiBTREE                    := IndexOf('BTREE');
    kiBY                       := IndexOf('BY');
    kiCACHE                    := IndexOf('CACHE');
    kiCALL                     := IndexOf('CALL');
    kiCASCADE                  := IndexOf('CASCADE');
    kiCASCADED                 := IndexOf('CASCADED');
    kiCASE                     := IndexOf('CASE');
    kiCATALOG_NAME             := IndexOf('CATALOG_NAME');
    kiCHANGE                   := IndexOf('CHANGE');
    kiCHANGED                  := IndexOf('CHANGED');
    kiCHANNEL                  := IndexOf('CHANNEL');
    kiCHAIN                    := IndexOf('CHAIN');
    kiCHARACTER                := IndexOf('CHARACTER');
    kiCHARSET                  := IndexOf('CHARSET');
    kiCHECK                    := IndexOf('CHECK');
    kiCHECKSUM                 := IndexOf('CHECKSUM');
    kiCLASS_ORIGIN             := IndexOf('CLASS_ORIGIN');
    kiCLIENT                   := IndexOf('CLIENT');
    kiCLOSE                    := IndexOf('CLOSE');
    kiCOALESCE                 := IndexOf('COALESCE');
    kiCODE                     := IndexOf('CODE');
    kiCOLLATE                  := IndexOf('COLLATE');
    kiCOLLATION                := IndexOf('COLLATION');
    kiCOLUMN                   := IndexOf('COLUMN');
    kiCOLUMN_NAME              := IndexOf('COLUMN_NAME');
    kiCOLUMN_FORMAT            := IndexOf('COLUMN_FORMAT');
    kiCOLUMNS                  := IndexOf('COLUMNS');
    kiCOMMENT                  := IndexOf('COMMENT');
    kiCOMMIT                   := IndexOf('COMMIT');
    kiCOMMITTED                := IndexOf('COMMITTED');
    kiCOMPACT                  := IndexOf('COMPACT');
    kiCOMPLETION               := IndexOf('COMPLETION');
    kiCOMPRESS                 := IndexOf('COMPRESS');
    kiCOMPRESSED               := IndexOf('COMPRESSED');
    kiCONCURRENT               := IndexOf('CONCURRENT');
    kiCONNECTION               := IndexOf('CONNECTION');
    kiCONDITION                := IndexOf('CONDITION');
    kiCONSISTENT               := IndexOf('CONSISTENT');
    kiCONSTRAINT               := IndexOf('CONSTRAINT');
    kiCONSTRAINT_CATALOG       := IndexOf('CONSTRAINT_CATALOG');
    kiCONSTRAINT_NAME          := IndexOf('CONSTRAINT_NAME');
    kiCONSTRAINT_SCHEMA        := IndexOf('CONSTRAINT_SCHEMA');
    kiCONTAINS                 := IndexOf('CONTAINS');
    kiCONTEXT                  := IndexOf('CONTEXT');
    kiCONTINUE                 := IndexOf('CONTINUE');
    kiCONTRIBUTORS             := IndexOf('CONTRIBUTORS');
    kiCONVERT                  := IndexOf('CONVERT');
    kiCOPY                     := IndexOf('COPY');
    kiCPU                      := IndexOf('CPU');
    kiCREATE                   := IndexOf('CREATE');
    kiCROSS                    := IndexOf('CROSS');
    kiCURRENT                  := IndexOf('CURRENT');
    kiCURRENT_DATE             := IndexOf('CURRENT_DATE');
    kiCURRENT_TIME             := IndexOf('CURRENT_TIME');
    kiCURRENT_TIMESTAMP        := IndexOf('CURRENT_TIMESTAMP');
    kiCURRENT_USER             := IndexOf('CURRENT_USER');
    kiCURSOR                   := IndexOf('CURSOR');
    kiCURSOR_NAME              := IndexOf('CURSOR_NAME');
    kiDATA                     := IndexOf('DATA');
    kiDATABASE                 := IndexOf('DATABASE');
    kiDATABASES                := IndexOf('DATABASES');
    kiDATAFILE                 := IndexOf('DATAFILE');
    kiDAY                      := IndexOf('DAY');
    kiDAY_HOUR                 := IndexOf('DAY_HOUR');
    kiDAY_MICROSECOND          := IndexOf('DAY_MICROSECOND');
    kiDAY_MINUTE               := IndexOf('DAY_MINUTE');
    kiDAY_SECOND               := IndexOf('DAY_SECOND');
    kiDEALLOCATE               := IndexOf('DEALLOCATE');
    kiDECLARE                  := IndexOf('DECLARE');
    kiDEFAULT                  := IndexOf('DEFAULT');
    kiDEFINER                  := IndexOf('DEFINER');
    kiDELAY_KEY_WRITE          := IndexOf('DELAY_KEY_WRITE');
    kiDELAYED                  := IndexOf('DELAYED');
    kiDELETE                   := IndexOf('DELETE');
    kiDESC                     := IndexOf('DESC');
    kiDESCRIBE                 := IndexOf('DESCRIBE');
    kiDETERMINISTIC            := IndexOf('DETERMINISTIC');
    kiDIAGNOSTICS              := IndexOf('DIAGNOSTICS');
    kiDIRECTORY                := IndexOf('DIRECTORY');
    kiDISABLE                  := IndexOf('DISABLE');
    kiDISCARD                  := IndexOf('DISCARD');
    kiDISK                     := IndexOf('DISK');
    kiDISTINCT                 := IndexOf('DISTINCT');
    kiDISTINCTROW              := IndexOf('DISTINCTROW');
    kiDIV                      := IndexOf('DIV');
    kiDO                       := IndexOf('DO');
    kiDROP                     := IndexOf('DROP');
    kiDUMPFILE                 := IndexOf('DUMPFILE');
    kiDUPLICATE                := IndexOf('DUPLICATE');
    kiDYNAMIC                  := IndexOf('DYNAMIC');
    kiEACH                     := IndexOf('EACH');
    kiELSE                     := IndexOf('ELSE');
    kiELSEIF                   := IndexOf('ELSEIF');
    kiENABLE                   := IndexOf('ENABLE');
    kiENABLE                   := IndexOf('ENABLE');
    kiENCLOSED                 := IndexOf('ENCLOSED');
    kiEND                      := IndexOf('END');
    kiENDS                     := IndexOf('ENDS');
    kiENGINE                   := IndexOf('ENGINE');
    kiENGINES                  := IndexOf('ENGINES');
    kiEVENT                    := IndexOf('EVENT');
    kiEVENTS                   := IndexOf('EVENTS');
    kiERRORS                   := IndexOf('ERRORS');
    kiESCAPE                   := IndexOf('ESCAPE');
    kiESCAPED                  := IndexOf('ESCAPED');
    kiEVERY                    := IndexOf('EVERY');
    kiEXCHANGE                 := IndexOf('EXCHANGE');
    kiEXCLUSIVE                := IndexOf('EXCLUSIVE');
    kiEXECUTE                  := IndexOf('EXECUTE');
    kiEXISTS                   := IndexOf('EXISTS');
    kiEXPANSION                := IndexOf('EXPANSION');
    kiEXPIRE                   := IndexOf('EXPIRE');
    kiEXPLAIN                  := IndexOf('EXPLAIN');
    kiEXIT                     := IndexOf('EXIT');
    kiEXTENDED                 := IndexOf('EXTENDED');
    kiFALSE                    := IndexOf('FALSE');
    kiFAST                     := IndexOf('FAST');
    kiFAULTS                   := IndexOf('FAULTS');
    kiFETCH                    := IndexOf('FETCH');
    kiFILE_BLOCK_SIZE          := IndexOf('FILE_BLOCK_SIZE');
    kiFLUSH                    := IndexOf('FLUSH');
    kiFIELDS                   := IndexOf('FIELDS');
    kiFILE                     := IndexOf('FILE');
    kiFIRST                    := IndexOf('FIRST');
    kiFIXED                    := IndexOf('FIXED');
    kiFOLLOWS                  := IndexOf('FOLLOWS');
    kiFOR                      := IndexOf('FOR');
    kiFORCE                    := IndexOf('FORCE');
    kiFOREIGN                  := IndexOf('FOREIGN');
    kiFORMAT                   := IndexOf('FORMAT');
    kiFOUND                    := IndexOf('FOUND');
    kiFROM                     := IndexOf('FROM');
    kiFULL                     := IndexOf('FULL');
    kiFULLTEXT                 := IndexOf('FULLTEXT');
    kiFUNCTION                 := IndexOf('FUNCTION');
    kiGENERATED                := IndexOf('GENERATED');
    kiGET                      := IndexOf('GET');
    kiGLOBAL                   := IndexOf('GLOBAL');
    kiGRANT                    := IndexOf('GRANT');
    kiGRANTS                   := IndexOf('GRANTS');
    kiGROUP                    := IndexOf('GROUP');
    kiHANDLER                  := IndexOf('HANDLER');
    kiHASH                     := IndexOf('HASH');
    kiHAVING                   := IndexOf('HAVING');
    kiHELP                     := IndexOf('HELP');
    kiHIGH_PRIORITY            := IndexOf('HIGH_PRIORITY');
    kiHOST                     := IndexOf('HOST');
    kiHOSTS                    := IndexOf('HOSTS');
    kiHOUR                     := IndexOf('HOUR');
    kiHOUR_MICROSECOND         := IndexOf('HOUR_MICROSECOND');
    kiHOUR_MINUTE              := IndexOf('HOUR_MINUTE');
    kiHOUR_SECOND              := IndexOf('HOUR_SECOND');
    kiIDENTIFIED               := IndexOf('IDENTIFIED');
    kiIF                       := IndexOf('IF');
    kiIGNORE                   := IndexOf('IGNORE');
    kiIGNORE_SERVER_IDS        := IndexOf('IGNORE_SERVER_IDS');
    kiIMPORT                   := IndexOf('IMPORT');
    kiIN                       := IndexOf('IN');
    kiINDEX                    := IndexOf('INDEX');
    kiINDEXES                  := IndexOf('INDEXES');
    kiINITIAL_SIZE             := IndexOf('INITIAL_SIZE');
    kiINNER                    := IndexOf('INNER');
    kiINFILE                   := IndexOf('INFILE');
    kiINNODB                   := IndexOf('INNODB');
    kiINOUT                    := IndexOf('INOUT');
    kiINPLACE                  := IndexOf('INPLACE');
    kiINSTANCE                 := IndexOf('INSTANCE');
    kiINSERT                   := IndexOf('INSERT');
    kiINSERT_METHOD            := IndexOf('INSERT_METHOD');
    kiINTERVAL                 := IndexOf('INTERVAL');
    kiINTO                     := IndexOf('INTO');
    kiINVOKER                  := IndexOf('INVOKER');
    kiIO                       := IndexOf('IO');
    kiIPC                      := IndexOf('IPC');
    kiIS                       := IndexOf('IS');
    kiISOLATION                := IndexOf('ISOLATION');
    kiITERATE                  := IndexOf('ITERATE');
    kiJOIN                     := IndexOf('JOIN');
    kiJSON                     := IndexOf('JSON');
    kiKEY                      := IndexOf('KEY');
    kiKEY_BLOCK_SIZE           := IndexOf('KEY_BLOCK_SIZE');
    kiKEYS                     := IndexOf('KEYS');
    kiKILL                     := IndexOf('KILL');
    kiLANGUAGE                 := IndexOf('LANGUAGE');
    kiLAST                     := IndexOf('LAST');
    kiLEADING                  := IndexOf('LEADING');
    kiLEAVE                    := IndexOf('LEAVE');
    kiLEFT                     := IndexOf('LEFT');
    kiLESS                     := IndexOf('LESS');
    kiLEVEL                    := IndexOf('LEVEL');
    kiLIKE                     := IndexOf('LIKE');
    kiLIMIT                    := IndexOf('LIMIT');
    kiLINEAR                   := IndexOf('LINEAR');
    kiLINES                    := IndexOf('LINES');
    kiLIST                     := IndexOf('LIST');
    kiLOGS                     := IndexOf('LOGS');
    kiLOAD                     := IndexOf('LOAD');
    kiLOCAL                    := IndexOf('LOCAL');
    kiLOCALTIME                := IndexOf('LOCALTIME');
    kiLOCALTIMESTAMP           := IndexOf('LOCALTIMESTAMP');
    kiLOCK                     := IndexOf('LOCK');
    kiLOOP                     := IndexOf('LOOP');
    kiLOW_PRIORITY             := IndexOf('LOW_PRIORITY');
    kiMASTER                   := IndexOf('MASTER');
    kiMASTER_AUTO_POSITION     := IndexOf('MASTER_AUTO_POSITION');
    kiMASTER_BIND              := IndexOf('MASTER_BIND');
    kiMASTER_CONNECT_RETRY     := IndexOf('MASTER_CONNECT_RETRY');
    kiMASTER_DELAY             := IndexOf('MASTER_DELAY');
    kiMASTER_HEARTBEAT_PERIOD  := IndexOf('MASTER_HEARTBEAT_PERIOD');
    kiMASTER_HOST              := IndexOf('MASTER_HOST');
    kiMASTER_LOG_FILE          := IndexOf('MASTER_LOG_FILE');
    kiMASTER_LOG_POS           := IndexOf('MASTER_LOG_POS');
    kiMASTER_PASSWORD          := IndexOf('MASTER_PASSWORD');
    kiMASTER_PORT              := IndexOf('MASTER_PORT');
    kiMASTER_RETRY_COUNT       := IndexOf('MASTER_RETRY_COUNT');
    kiMASTER_SSL               := IndexOf('MASTER_SSL');
    kiMASTER_SSL_CA            := IndexOf('MASTER_SSL_CA');
    kiMASTER_SSL_CAPATH        := IndexOf('MASTER_SSL_CAPATH');
    kiMASTER_SSL_CERT          := IndexOf('MASTER_SSL_CERT');
    kiMASTER_SSL_CIPHER        := IndexOf('MASTER_SSL_CIPHER');
    kiMASTER_SSL_CRL           := IndexOf('MASTER_SSL_CRL');
    kiMASTER_SSL_CRLPATH       := IndexOf('MASTER_SSL_CRLPATH');
    kiMASTER_SSL_KEY           := IndexOf('MASTER_SSL_KEY');
    kiMASTER_SSL_VERIFY_SERVER_CERT := IndexOf('MASTER_SSL_VERIFY_SERVER_CERT');
    kiMASTER_TLS_VERSION       := IndexOf('MASTER_TLS_VERSION');
    kiMASTER_USER              := IndexOf('MASTER_USER');
    kiMATCH                    := IndexOf('MATCH');
    kiMAX_CONNECTIONS_PER_HOUR := IndexOf('MAX_CONNECTIONS_PER_HOUR');
    kiMAX_QUERIES_PER_HOUR     := IndexOf('MAX_QUERIES_PER_HOUR');
    kiMAX_ROWS                 := IndexOf('MAX_ROWS');
    kiMAX_STATEMENT_TIME       := IndexOf('MAX_STATEMENT_TIME');
    kiMAX_UPDATES_PER_HOUR     := IndexOf('MAX_UPDATES_PER_HOUR');
    kiMAX_USER_CONNECTIONS     := IndexOf('MAX_USER_CONNECTIONS');
    kiMAXVALUE                 := IndexOf('MAXVALUE');
    kiMEDIUM                   := IndexOf('MEDIUM');
    kiMEMORY                   := IndexOf('MEMORY');
    kiMERGE                    := IndexOf('MERGE');
    kiMESSAGE_TEXT             := IndexOf('MESSAGE_TEXT');
    kiMICROSECOND              := IndexOf('MICROSECOND');
    kiMIGRATE                  := IndexOf('MIGRATE');
    kiMIN_ROWS                 := IndexOf('MIN_ROWS');
    kiMINUTE                   := IndexOf('MINUTE');
    kiMINUTE_SECOND            := IndexOf('MINUTE_SECOND');
    kiMINUTE_MICROSECOND       := IndexOf('MINUTE_MICROSECOND');
    kiMOD                      := IndexOf('MOD');
    kiMODE                     := IndexOf('MODE');
    kiMODIFIES                 := IndexOf('MODIFIES');
    kiMODIFY                   := IndexOf('MODIFY');
    kiMONTH                    := IndexOf('MONTH');
    kiMUTEX                    := IndexOf('MUTEX');
    kiMYSQL_ERRNO              := IndexOf('MYSQL_ERRNO');
    kiNAME                     := IndexOf('NAME');
    kiNAMES                    := IndexOf('NAMES');
    kiNATIONAL                 := IndexOf('NATIONAL');
    kiNATURAL                  := IndexOf('NATURAL');
    kiNEVER                    := IndexOf('NEVER');
    kiNEXT                     := IndexOf('NEXT');
    kiNO                       := IndexOf('NO');
    kiNONE                     := IndexOf('NONE');
    kiNOT                      := IndexOf('NOT');
    kiNO_WRITE_TO_BINLOG       := IndexOf('NO_WRITE_TO_BINLOG');
    kiNULL                     := IndexOf('NULL');
    kiNUMBER                   := IndexOf('NUMBER');
    kiOFFSET                   := IndexOf('OFFSET');
    kiOJ                       := IndexOf('OJ');
    kiON                       := IndexOf('ON');
    kiONE                      := IndexOf('ONE');
    kiONLY                     := IndexOf('ONLY');
    kiOPEN                     := IndexOf('OPEN');
    kiOPTIMIZE                 := IndexOf('OPTIMIZE');
    kiOPTION                   := IndexOf('OPTION');
    kiOPTIONALLY               := IndexOf('OPTIONALLY');
    kiOPTIONS                  := IndexOf('OPTIONS');
    kiOR                       := IndexOf('OR');
    kiORDER                    := IndexOf('ORDER');
    kiOUT                      := IndexOf('OUT');
    kiOUTER                    := IndexOf('OUTER');
    kiOUTFILE                  := IndexOf('OUTFILE');
    kiOWNER                    := IndexOf('OWNER');
    kiPACK_KEYS                := IndexOf('PACK_KEYS');
    kiPAGE                     := IndexOf('PAGE');
    kiPAGE_CHECKSUM            := IndexOf('PAGE_CHECKSUM');
    kiPARSER                   := IndexOf('PARSER');
    kiPARTIAL                  := IndexOf('PARTIAL');
    kiPARTITION                := IndexOf('PARTITION');
    kiPARTITIONING             := IndexOf('PARTITIONING');
    kiPARTITIONS               := IndexOf('PARTITIONS');
    kiPASSWORD                 := IndexOf('PASSWORD');
    kiPERSISTENT               := IndexOf('PERSISTENT');
    kiPHASE                    := IndexOf('PHASE');
    kiQUERY                    := IndexOf('QUERY');
    kiRECOVER                  := IndexOf('RECOVER');
    kiREDUNDANT                := IndexOf('REDUNDANT');
    kiPLUGINS                  := IndexOf('PLUGINS');
    kiPORT                     := IndexOf('PORT');
    kiPRECEDES                 := IndexOf('PRECEDES');
    kiPREPARE                  := IndexOf('PREPARE');
    kiPRESERVE                 := IndexOf('PRESERVE');
    kiPRIMARY                  := IndexOf('PRIMARY');
    kiPRIVILEGES               := IndexOf('PRIVILEGES');
    kiPROCEDURE                := IndexOf('PROCEDURE');
    kiPROCESS                  := IndexOf('PROCESS');
    kiPROCESSLIST              := IndexOf('PROCESSLIST');
    kiPROFILE                  := IndexOf('PROFILE');
    kiPROFILES                 := IndexOf('PROFILES');
    kiPROXY                    := IndexOf('PROXY');
    kiPURGE                    := IndexOf('PURGE');
    kiQUARTER                  := IndexOf('QUARTER');
    kiQUICK                    := IndexOf('QUICK');
    kiRANGE                    := IndexOf('RANGE');
    kiREAD                     := IndexOf('READ');
    kiREADS                    := IndexOf('READS');
    kiREBUILD                  := IndexOf('REBUILD');
    kiREFERENCES               := IndexOf('REFERENCES');
    kiREGEXP                   := IndexOf('REGEXP');
    kiRELAY_LOG_FILE           := IndexOf('RELAY_LOG_FILE');
    kiRELAY_LOG_POS            := IndexOf('RELAY_LOG_POS');
    kiRELAYLOG                 := IndexOf('RELAYLOG');
    kiRELEASE                  := IndexOf('RELEASE');
    kiRELOAD                   := IndexOf('RELOAD');
    kiREMOVE                   := IndexOf('REMOVE');
    kiRENAME                   := IndexOf('RENAME');
    kiREORGANIZE               := IndexOf('REORGANIZE');
    kiREPEAT                   := IndexOf('REPEAT');
    kiREPLICATION              := IndexOf('REPLICATION');
    kiREPAIR                   := IndexOf('REPAIR');
    kiREPEATABLE               := IndexOf('REPEATABLE');
    kiREPLACE                  := IndexOf('REPLACE');
    kiREQUIRE                  := IndexOf('REQUIRE');
    kiRESET                    := IndexOf('RESET');
    kiRESIGNAL                 := IndexOf('RESIGNAL');
    kiRESTRICT                 := IndexOf('RESTRICT');
    kiRESUME                   := IndexOf('RESUME');
    kiRETURN                   := IndexOf('RETURN');
    kiRETURNED_SQLSTATE        := IndexOf('RETURNED_SQLSTATE');
    kiRETURNS                  := IndexOf('RETURNS');
    kiREVERSE                  := IndexOf('REVERSE');
    kiREVOKE                   := IndexOf('REVOKE');
    kiRIGHT                    := IndexOf('RIGHT');
    kiRLIKE                    := IndexOf('RLIKE');
    kiROLLBACK                 := IndexOf('ROLLBACK');
    kiROLLUP                   := IndexOf('ROLLUP');
    kiROTATE                   := IndexOf('ROTATE');
    kiROUTINE                  := IndexOf('ROUTINE');
    kiROW                      := IndexOf('ROW');
    kiROW_COUNT                := IndexOf('ROW_COUNT');
    kiROW_FORMAT               := IndexOf('ROW_FORMAT');
    kiROWS                     := IndexOf('ROWS');
    kiSAVEPOINT                := IndexOf('SAVEPOINT');
    kiSCHEDULE                 := IndexOf('SCHEDULE');
    kiSCHEMA                   := IndexOf('SCHEMA');
    kiSCHEMA_NAME              := IndexOf('SCHEMA_NAME');
    kiSECOND                   := IndexOf('SECOND');
    kiSECOND_MICROSECOND       := IndexOf('SECOND_MICROSECOND');
    kiSECURITY                 := IndexOf('SECURITY');
    kiSELECT                   := IndexOf('SELECT');
    kiSEPARATOR                := IndexOf('SEPARATOR');
    kiSERIALIZABLE             := IndexOf('SERIALIZABLE');
    kiSERVER                   := IndexOf('SERVER');
    kiSESSION                  := IndexOf('SESSION');
    kiSET                      := IndexOf('SET');
    kiSHARE                    := IndexOf('SHARE');
    kiSHARED                   := IndexOf('SHARED');
    kiSHOW                     := IndexOf('SHOW');
    kiSHUTDOWN                 := IndexOf('SHUTDOWN');
    kiSIGNAL                   := IndexOf('SIGNAL');
    kiSIGNED                   := IndexOf('SIGNED');
    kiSIMPLE                   := IndexOf('SIMPLE');
    kiSLAVE                    := IndexOf('SLAVE');
    kiSNAPSHOT                 := IndexOf('SNAPSHOT');
    kiSOCKET                   := IndexOf('SOCKET');
    kiSOME                     := IndexOf('SOME');
    kiSONAME                   := IndexOf('SONAME');
    kiSOUNDS                   := IndexOf('SOUNDS');
    kiSOURCE                   := IndexOf('SOURCE');
    kiSPATIAL                  := IndexOf('SPATIAL');
    kiSQL                      := IndexOf('SQL');
    kiSQL_BIG_RESULT           := IndexOf('SQL_BIG_RESULT');
    kiSQL_BUFFER_RESULT        := IndexOf('SQL_BUFFER_RESULT');
    kiSQL_CACHE                := IndexOf('SQL_CACHE');
    kiSQL_CALC_FOUND_ROWS      := IndexOf('SQL_CALC_FOUND_ROWS');
    kiSQL_NO_CACHE             := IndexOf('SQL_NO_CACHE');
    kiSQL_SMALL_RESULT         := IndexOf('SQL_SMALL_RESULT');
    kiSQLEXCEPTION             := IndexOf('SQLEXCEPTION');
    kiSQLSTATE                 := IndexOf('SQLSTATE');
    kiSQLWARNINGS              := IndexOf('SQLWARNINGS');
    kiSTACKED                  := IndexOf('STACKED');
    kiSTARTING                 := IndexOf('STARTING');
    kiSTART                    := IndexOf('START');
    kiSTARTS                   := IndexOf('STARTS');
    kiSTATS_AUTO_RECALC        := IndexOf('STATS_AUTO_RECALC');
    kiSTATS_PERSISTENT         := IndexOf('STATS_PERSISTENT');
    kiSTATUS                   := IndexOf('STATUS');
    kiSTOP                     := IndexOf('STOP');
    kiSTORAGE                  := IndexOf('STORAGE');
    kiSTORED                   := IndexOf('STORED');
    kiSTRAIGHT_JOIN            := IndexOf('STRAIGHT_JOIN');
    kiSUBCLASS_ORIGIN          := IndexOf('SUBCLASS_ORIGIN');
    kiSUBPARTITION             := IndexOf('SUBPARTITION');
    kiSUBPARTITIONS            := IndexOf('SUBPARTITIONS');
    kiSUPER                    := IndexOf('SUPER');
    kiSUSPEND                  := IndexOf('SUSPEND');
    kiSWAPS                    := IndexOf('SWAPS');
    kiSWITCHES                 := IndexOf('SWITCHES');
    kiTABLE                    := IndexOf('TABLE');
    kiTABLE_NAME               := IndexOf('TABLE_NAME');
    kiTABLES                   := IndexOf('TABLES');
    kiTABLESPACE               := IndexOf('TABLESPACE');
    kiTEMPORARY                := IndexOf('TEMPORARY');
    kiTEMPTABLE                := IndexOf('TEMPTABLE');
    kiTERMINATED               := IndexOf('TERMINATED');
    kiTHAN                     := IndexOf('THAN');
    kiTHEN                     := IndexOf('THEN');
    kiTO                       := IndexOf('TO');
    kiTRAILING                 := IndexOf('TRAILING');
    kiTRADITIONAL              := IndexOf('TRADITIONAL');
    kiTRANSACTION              := IndexOf('TRANSACTION');
    kiTRANSACTIONAL            := IndexOf('TRANSACTIONAL');
    kiTRIGGER                  := IndexOf('TRIGGER');
    kiTRIGGERS                 := IndexOf('TRIGGERS');
    kiTRUNCATE                 := IndexOf('TRUNCATE');
    kiTRUE                     := IndexOf('TRUE');
    kiTYPE                     := IndexOf('TYPE');
    kiUNCOMMITTED              := IndexOf('UNCOMMITTED');
    kiUNDEFINED                := IndexOf('UNDEFINED');
    kiUNDO                     := IndexOf('UNDO');
    kiUNICODE                  := IndexOf('UNICODE');
    kiUNION                    := IndexOf('UNION');
    kiUNIQUE                   := IndexOf('UNIQUE');
    kiUNKNOWN                  := IndexOf('UNKNOWN');
    kiUNLOCK                   := IndexOf('UNLOCK');
    kiUNSIGNED                 := IndexOf('UNSIGNED');
    kiUNTIL                    := IndexOf('UNTIL');
    kiUPDATE                   := IndexOf('UPDATE');
    kiUPGRADE                  := IndexOf('UPGRADE');
    kiUSAGE                    := IndexOf('USAGE');
    kiUSE                      := IndexOf('USE');
    kiUSE_FRM                  := IndexOf('USE_FRM');
    kiUSER                     := IndexOf('USER');
    kiUSING                    := IndexOf('USING');
    kiVALIDATION               := IndexOf('VALIDATION');
    kiVALUE                    := IndexOf('VALUE');
    kiVALUES                   := IndexOf('VALUES');
    kiVARIABLES                := IndexOf('VARIABLES');
    kiVIEW                     := IndexOf('VIEW');
    kiVIRTUAL                  := IndexOf('VIRTUAL');
    kiWAIT                     := IndexOf('WAIT');
    kiWARNINGS                 := IndexOf('WARNINGS');
    kiWEEK                     := IndexOf('WEEK');
    kiWHEN                     := IndexOf('WHEN');
    kiWHERE                    := IndexOf('WHERE');
    kiWHILE                    := IndexOf('WHILE');
    kiWRAPPER                  := IndexOf('WRAPPER');
    kiWITH                     := IndexOf('WITH');
    kiWITHOUT                  := IndexOf('WITHOUT');
    kiWORK                     := IndexOf('WORK');
    kiWRITE                    := IndexOf('WRITE');
    kiXA                       := IndexOf('XA');
    kiXID                      := IndexOf('XID');
    kiXML                      := IndexOf('XML');
    kiXOR                      := IndexOf('XOR');
    kiYEAR                     := IndexOf('YEAR');
    kiYEAR_MONTH               := IndexOf('YEAR_MONTH');
    kiZEROFILL                 := IndexOf('ZEROFILL');

    SetLength(OperatorTypeByKeywordIndex, KeywordList.Count);
    for Index := 0 to KeywordList.Count - 1 do
      OperatorTypeByKeywordIndex[Index] := otUnknown;
    OperatorTypeByKeywordIndex[kiAND]      := otAnd;
    OperatorTypeByKeywordIndex[kiCASE]     := otCase;
    OperatorTypeByKeywordIndex[kiBETWEEN]  := otBetween;
    OperatorTypeByKeywordIndex[kiBINARY]   := otBinary;
    OperatorTypeByKeywordIndex[kiCOLLATE]  := otCollate;
    OperatorTypeByKeywordIndex[kiDISTINCT] := otDistinct;
    OperatorTypeByKeywordIndex[kiDIV]      := otDiv;
    OperatorTypeByKeywordIndex[kiESCAPE]   := otEscape;
    OperatorTypeByKeywordIndex[kiIS]       := otIs;
    OperatorTypeByKeywordIndex[kiIN]       := otIn;
    OperatorTypeByKeywordIndex[kiLIKE]     := otLike;
    OperatorTypeByKeywordIndex[kiMOD]      := otMOD;
    OperatorTypeByKeywordIndex[kiNOT]      := otNot;
    OperatorTypeByKeywordIndex[kiOR]       := otOr;
    OperatorTypeByKeywordIndex[kiREGEXP]   := otRegExp;
    OperatorTypeByKeywordIndex[kiRLIKE]    := otRegExp;
    OperatorTypeByKeywordIndex[kiSOUNDS]   := otSounds;
    OperatorTypeByKeywordIndex[kiXOR]      := otXOR;
  end;
end;

function TSQLParser.StmtPtr(const Node: TOffset): PStmt;
begin
  Assert((Node = 0) or IsStmt(Node));

  if (not IsStmt(Node)) then
    Result := nil
  else
    Result := @Nodes.Mem[Node];
end;

function TSQLParser.TokenPtr(const Token: TOffset): PToken;
begin
  Assert((Token = 0) or IsToken(Token));

  if (Token = 0) then
    Result := nil
  else
    Result := PToken(@Nodes.Mem[Token]);
end;

{$IFDEF Debug}
var
  Max: Integer;
  OperatorType: TSQLParser.TOperatorType;
{$ENDIF}
initialization
  {$IFDEF Debug}
    Max := 0;
    for OperatorType := Low(TSQLParser.TOperatorType) to High(TSQLParser.TOperatorType) do
      if (TSQLParser.OperatorPrecedenceByOperatorType[OperatorType] > Max) then
        Max := TSQLParser.OperatorPrecedenceByOperatorType[OperatorType];
    Assert(Max = TSQLParser.MaxOperatorPrecedence);
  {$ENDIF}
end.
// SQLUnescape
// utDatatype in AutoComplete aufnehmen
// GetMem in CompletionList.AddKeyword entfernen
// GetAsString with 'Test'  'Text'

