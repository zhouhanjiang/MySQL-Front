unit SQLParser;

interface {********************************************************************}

uses
  Classes;

type
  TMySQLParser = class
  public
    type
      TFileType = (ftSQL, ftFormatedSQL, ftDebugHTML);

      TNodeType = (
        ntRoot,            // Root token, one usage by the parser to handle stmt list
        ntToken,           // Token node (smalles logical part of the SQL text)

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
        ntCreateTableStmtFieldDefaultFunc,
        ntCreateTableStmtForeignKey,
        ntCreateTableStmtKey,
        ntCreateTableStmtKeyColumn,
        ntCreateTableStmtPartition,
        ntCreateTableStmtPartitionValues,
        ntCreateTableStmtReference,
        ntCreateTriggerStmt,
        ntCreateUserStmt,
        ntCreateViewStmt,
        ntCurrentTimestamp,
        ntDatatype,
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
        rtFunction,
        rtProcedure
      );

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
      PE_NestedCondCode = 7; // Nested conditional MySQL option

      // Bugs while parsing Root
      PE_UnknownStmt = 8; // Unknown statement

      MySQLDatatypes = 
        'BIGINT,BINARY,BIT,BLOB,BOOL,BOOLEAN,CHAR,DEC,DECIMAL,DATE,DATETIME,' +
        'DOUBLE,ENUM,FLOAT,GEOMETRY,GEOMETRYCOLLECTION,INT,INT4,INTEGER,' +
        'LARGEINT,LINESTRING,JSON,LONG,LONGBLOB,LONGTEXT,MEDIUMBLOB,' +
        'MEDIUMINT,MEDIUMTEXT,MULTILINESTRING,MULTIPOINT,MULTIPOLYGON,' +
        'NUMERIC,NCHAR,NVARCHAR,POINT,POLYGON,REAL,SERIAL,SET,SIGNED,' +
        'SMALLINT,TEXT,TIME,TIMESTAMP,TINYBLOB,TINYINT,TINYTEXT,UNSIGNED,' +
        'VARBINARY,VARCHAR,YEAR';
      
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
        'INNODB,INSTANCE,ROTATE,MICROSECOND,FOLLOWS,PRECEDES,SIGNED,' +
        'MAX_STATEMENT_TIME,SOME,ALWAYS,GENERATED,STORED,VIRTUAL,PERSISTENT,DISK,' +
        'COMPRESS,BOOLEAN,TRANSACTIONAL,' +

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
        'ntRoot',
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
        'ntCreateTableStmtField',
        'ntCreateTableStmtFieldDefaultFunc',
        'ntCreateTableStmtForeignKey',
        'ntCreateTableStmtKey',
        'ntCreateTableStmtKeyColumn',
        'ntCreateTableStmtPartition',
        'ntCreateTableStmtPartitionValues',
        'ntCreateTableStmtReference',
        'ntCreateTriggerStmt',
        'ntCreateUserStmt',
        'ntCreateViewStmt',
        'ntCurrentTimestamp',
        'ntDatatype',
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
        'ntIgnoreLines',
        'ntInOp',
        'ntInsertStmt',
        'ntInsertStmtSetItem',
        'ntIntervalOp',
        'ntIntervalOpListItem',
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
        'ntMatchFunc',
        'ntPositionFunc',
        'ntPrepareStmt',
        'ntPurgeStmt',
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
        'ntSubstringFunc',
        'ntTag',
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
        'ntXAStmtID'
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
        'stUnlock',
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
        'ditTable',
        'ditKey',
        'ditField',
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
        'rtFunction',
        'rtProcedure'
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
        ntUnlockStmt,
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
        PageChecksum: TOffset;
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
        TIndices = array [0..5] of TIndex;
      private
        FCount: TIndex;
        FIndex: array of PChar;
        FFirst: array of Integer;
        FParser: TMySQLParser;
        FText: string;
        function GetText(): string;
        function GetWord(Index: TIndex): string;
        procedure SetText(AText: string);
      protected
        procedure Clear();
        property Parser: TMySQLParser read FParser;
      public
        constructor Create(const ASQLParser: TMySQLParser; const AText: string = '');
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
      PNode = ^TNode;
      PToken = ^TToken;
      PStmt = ^TStmt;

      { Base nodes ------------------------------------------------------------}

      TNode = packed record
      private
        FNodeType: TNodeType;
        FParser: TMySQLParser;
      private
        class function Create(const AParser: TMySQLParser; const ANodeType: TNodeType): TOffset; static; {$IFNDEF Debug} inline; {$ENDIF}
        function GetOffset(): TOffset; {$IFNDEF Debug} inline; {$ENDIF}
        property Offset: TOffset read GetOffset;
      public
        property NodeType: TNodeType read FNodeType;
        property Parser: TMySQLParser read FParser;
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
        class function Create(const AParser: TMySQLParser;
          const AErrorCode: Byte; const AErrorMessage: TOffset;
          const AFirstTokenAll, ALastTokenAll: TOffset;
          const ChildCount: Integer; const Children: array of TOffset): TOffset; static;
        function GetErrorMessage(): string;
        function GetFirstStmt(): PStmt; {$IFNDEF Debug} inline; {$ENDIF}
        function GetFirstTokenAll(): PToken; {$IFNDEF Debug} inline; {$ENDIF}
        function GetLastStmt(): PStmt; {$IFNDEF Debug} inline; {$ENDIF}
        function GetLastTokenAll(): PToken; {$IFNDEF Debug} inline; {$ENDIF}
        property Parser: TMySQLParser read Heritage.FParser;
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
        class function Create(const AParser: TMySQLParser; const ANodeType: TNodeType): TOffset; static; {$IFNDEF Debug} inline; {$ENDIF}
        function GetDelimiter(): PToken; {$IFNDEF Debug} inline; {$ENDIF}
        function GetFFirstToken(): TOffset; {$IFNDEF Debug} inline; {$ENDIF}
        function GetFirstToken(): PToken; {$IFNDEF Debug} inline; {$ENDIF}
        function GetFLastToken(): TOffset; {$IFNDEF Debug} inline; {$ENDIF}
        function GetLastToken(): PToken; {$IFNDEF Debug} inline; {$ENDIF}
        function GetNextSibling(): PChild; {$IFNDEF Debug} inline; {$ENDIF}
        function GetParentNode(): PNode; {$IFNDEF Debug} inline; {$ENDIF}
        property FFirstToken: TOffset read GetFFirstToken;
        property FLastToken: TOffset read GetFLastToken;
        property Parser: TMySQLParser read Heritage.FParser;
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
        class function Create(const AParser: TMySQLParser;
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
        property Parser: TMySQLParser read Heritage.Heritage.FParser;
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
        class function Create(const AParser: TMySQLParser; const ANodeType: TNodeType): TOffset; static; {$IFNDEF Debug} inline; {$ENDIF}
        function GetFirstToken(): PToken; {$IFNDEF Debug} inline; {$ENDIF}
        function GetLastToken(): PToken; {$IFNDEF Debug} inline; {$ENDIF}
        function GetOffset(): TOffset; {$IFNDEF Debug} inline; {$ENDIF}
        function GetParentNode(): PNode; {$IFNDEF Debug} inline; {$ENDIF}
        procedure AddChildren(const Children: POffsetArray; const Count: Integer);
        property Offset: TOffset read GetOffset;
        property Parser: TMySQLParser read Heritage.Heritage.FParser;
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
        class function Create(const AParser: TMySQLParser; const AStmtType: TStmtType): TOffset; static;
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
        property Parser: TMySQLParser read Heritage.Heritage.Heritage.FParser;
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
      PAnalyzeStmt = ^TAnalyzeStmt;
      TAnalyzeStmt = packed record
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
        class function Create(const AParser: TMySQLParser; const ANodes: TNodes): TOffset; static;
      public
        property Parser: TMySQLParser read Heritage.Heritage.Heritage.Heritage.FParser;
      end;

      PAlterDatabaseStmt = ^TAlterDatabaseStmt;
      TAlterDatabaseStmt = packed record
      private type
        TNodes = packed record
          StmtTag: TOffset;
          IdentTag: TOffset;
          CharacterSetValue: TOffset;
          CollateValue: TOffset;
          UpgradeDataDirectoryNameTag: TOffset;
        end;
      private
        Heritage: TStmt;
      private
        Nodes: TNodes;
        class function Create(const AParser: TMySQLParser; const ANodes: TNodes): TOffset; static;
      public
        property Parser: TMySQLParser read Heritage.Heritage.Heritage.Heritage.FParser;
      end;

      PAlterEventStmt = ^TAlterEventStmt;
      TAlterEventStmt = packed record
      private type
        TNodes = packed record
          AlterTag: TOffset;
          DefinerNode: TOffset;
          EventTag: TOffset;
          EventIdent: TOffset;
          OnSchedule: packed record
            Tag: TOffset;
            Value: TOffset;
          end;
          OnCompletitionTag: TOffset;
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
        class function Create(const AParser: TMySQLParser; const ANodes: TNodes): TOffset; static;
      public
        property Parser: TMySQLParser read Heritage.Heritage.Heritage.Heritage.FParser;
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
        class function Create(const AParser: TMySQLParser; const ANodes: TNodes): TOffset; static;
      public
        property Parser: TMySQLParser read Heritage.Heritage.Heritage.Heritage.FParser;
      end;

      PAlterRoutineStmt = ^TAlterRoutineStmt;
      TAlterRoutineStmt = packed record
      private type
        TNodes = packed record
          AlterTag: TOffset;
          Ident: TOffset;
          CharacteristicList: TOffset;
        end;
      private
        Heritage: TStmt;
      private
        Nodes: TNodes;
        FRoutineType: TRoutineType;
        class function Create(const AParser: TMySQLParser; const ARoutineType: TRoutineType; const ANodes: TNodes): TOffset; static;
      public
        property Parser: TMySQLParser read Heritage.Heritage.Heritage.Heritage.FParser;
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
        class function Create(const AParser: TMySQLParser; const ANodes: TNodes): TOffset; static;
      public
        property Parser: TMySQLParser read Heritage.Heritage.Heritage.Heritage.FParser;
      end;

      PAlterTableStmt = ^TAlterTableStmt;
      TAlterTableStmt = packed record
      private type

        PAlterColumn = ^TAlterColumn;
        TAlterColumn = packed record
        private type
          TNodes = packed record
            AlterTag: TOffset;
            ColumnIdent: TOffset;
            SetDefaultValue: TOffset;
            DropDefaultTag: TOffset;
          end;
        private
          Heritage: TRange;
        private
          Nodes: TNodes;
          class function Create(const AParser: TMySQLParser; const ANodes: TNodes): TOffset; static;
        public
          property Parser: TMySQLParser read Heritage.Heritage.Heritage.FParser;
        end;

        PConvertTo = ^TConvertTo;
        TConvertTo = packed record
        private type
          TNodes = packed record
            ConvertToTag: TOffset;
            CharacterSetValue: TOffset;
            CollateValue: TOffset;
          end;
        private
          Heritage: TRange;
        private
          Nodes: TNodes;
          class function Create(const AParser: TMySQLParser; const ANodes: TNodes): TOffset; static;
        public
          property Parser: TMySQLParser read Heritage.Heritage.Heritage.FParser;
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
          class function Create(const AParser: TMySQLParser; const ANodes: TNodes): TOffset; static;
        public
          property Parser: TMySQLParser read Heritage.Heritage.Heritage.FParser;
        end;

        PExchangePartition = ^TExchangePartition;
        TExchangePartition = packed record
        private type
          TNodes = packed record
            ExchangePartitionTag: TOffset;
            PartitionIdent: TOffset;
            WithTableTag: TOffset;
            TableIdent: TOffset;
          end;
        private
          Heritage: TRange;
        private
          Nodes: TNodes;
          class function Create(const AParser: TMySQLParser; const ANodes: TNodes): TOffset; static;
        public
          property Parser: TMySQLParser read Heritage.Heritage.Heritage.FParser;
        end;

        PReorganizePartition = ^TReorganizePartition;
        TReorganizePartition = packed record
        private type
          TNodes = packed record
            ReorganizePartitionTag: TOffset;
            PartitionIdentList: TOffset;
            IntoTag: TOffset;
            PartitionList: TOffset;
          end;
        private
          Heritage: TRange;
        private
          Nodes: TNodes;
          class function Create(const AParser: TMySQLParser; const ANodes: TNodes): TOffset; static;
        public
          property Parser: TMySQLParser read Heritage.Heritage.Heritage.FParser;
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
        class function Create(const AParser: TMySQLParser; const ANodes: TNodes): TOffset; static;
      public
        property Parser: TMySQLParser read Heritage.Heritage.Heritage.Heritage.FParser;
      end;

      PAlterViewStmt = ^TAlterViewStmt;
      TAlterViewStmt = packed record
      private type
        TNodes = packed record
          AlterTag: TOffset;
          AlgorithmValue: TOffset;
          DefinerNode: TOffset;
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
        class function Create(const AParser: TMySQLParser; const ANodes: TNodes): TOffset; static;
      public
        property Parser: TMySQLParser read Heritage.Heritage.Heritage.Heritage.FParser;
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
        class function Create(const AParser: TMySQLParser; const ANodes: TNodes): TOffset; static;
      public
        property Parser: TMySQLParser read Heritage.Heritage.Heritage.FParser;
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
        class function Create(const AParser: TMySQLParser; const ANodes: TNodes): TOffset; static;
      public
        property Parser: TMySQLParser read Heritage.Heritage.Heritage.Heritage.FParser;
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
        class function Create(const AParser: TMySQLParser; const AExpr, ANotToken, ABetweenToken, AMin, AAndToken, AMax: TOffset): TOffset; static;
      public
        property Parser: TMySQLParser read Heritage.Heritage.Heritage.FParser;
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
        class function Create(const AParser: TMySQLParser; const AOperator, AOperand1, AOperand2: TOffset): TOffset; static;
        function GetOperand1(): PChild; {$IFNDEF Debug} inline; {$ENDIF}
        function GetOperand2(): PChild; {$IFNDEF Debug} inline; {$ENDIF}
        function GetOperator(): PChild; {$IFNDEF Debug} inline; {$ENDIF}
      public
        property Operand1: PChild read GetOperand1;
        property Operand2: PChild read GetOperand2;
        property Operator: PChild read GetOperator;
        property Parser: TMySQLParser read Heritage.Heritage.Heritage.FParser;
      end;

      PCallStmt = ^TCallStmt;
      TCallStmt = packed record
      private type
        TNodes = packed record
          CallTag: TOffset;
          ProcedureIdent: TOffset;
          ParamList: TOffset;
        end;
      private
        Heritage: TStmt;
      private
        Nodes: TNodes;
        class function Create(const AParser: TMySQLParser; const ANodes: TNodes): TOffset; static;
      public
        property Parser: TMySQLParser read Heritage.Heritage.Heritage.Heritage.FParser;
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
          class function Create(const AParser: TMySQLParser; const ANodes: TNodes): TOffset; static;
        public
          property Parser: TMySQLParser read Heritage.Heritage.Heritage.FParser;
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
        class function Create(const AParser: TMySQLParser; const ANodes: TNodes): TOffset; static;
      public
        property Parser: TMySQLParser read Heritage.Heritage.Heritage.FParser;
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
          class function Create(const AParser: TMySQLParser; const ANodes: TNodes): TOffset; static;
        public
          property Parser: TMySQLParser read Heritage.Heritage.Heritage.FParser;
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
        class function Create(const AParser: TMySQLParser; const ANodes: TNodes): TOffset; static;
      public
        property Parser: TMySQLParser read Heritage.Heritage.Heritage.Heritage.FParser;
      end;

      PCastFunc = ^TCastFunc;
      TCastFunc = packed record
      private type
        TNodes = packed record
          FuncToken: TOffset;
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
        class function Create(const AParser: TMySQLParser; const ANodes: TNodes): TOffset; static;
      public
        property Parser: TMySQLParser read Heritage.Heritage.Heritage.FParser;
      end;

      PCharFunc = ^TCharFunc;
      TCharFunc = packed record
      private type
        TNodes = packed record
          FuncToken: TOffset;
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
        class function Create(const AParser: TMySQLParser; const ANodes: TNodes): TOffset; static;
      public
        property Parser: TMySQLParser read Heritage.Heritage.Heritage.FParser;
      end;

      PChecksumStmt = ^TChecksumStmt;
      TChecksumStmt = packed record
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
        class function Create(const AParser: TMySQLParser; const ANodes: TNodes): TOffset; static;
      public
        property Parser: TMySQLParser read Heritage.Heritage.Heritage.Heritage.FParser;
      end;

      PCheckStmt = ^TCheckStmt;
      TCheckStmt = packed record
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
          class function Create(const AParser: TMySQLParser; const ANodes: TNodes): TOffset; overload; static;
        public
          property Parser: TMySQLParser read Heritage.Heritage.Heritage.FParser;
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
        class function Create(const AParser: TMySQLParser; const ANodes: TNodes): TOffset; static;
      public
        property Parser: TMySQLParser read Heritage.Heritage.Heritage.Heritage.FParser;
      end;

      PCloseStmt = ^TCloseStmt;
      TCloseStmt = packed record
      private type
        TNodes = packed record
          CloseTag: TOffset;
          CursorIdent: TOffset;
        end;
      private
        Heritage: TStmt;
      private
        Nodes: TNodes;
        class function Create(const AParser: TMySQLParser; const ANodes: TNodes): TOffset; static;
      public
        property Parser: TMySQLParser read Heritage.Heritage.Heritage.Heritage.FParser;
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
        class function Create(const AParser: TMySQLParser; const ANodes: TNodes): TOffset; static;
      public
        property Parser: TMySQLParser read Heritage.Heritage.Heritage.Heritage.FParser;
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
        class function Create(const AParser: TMySQLParser; const ANodes: TNodes): TOffset; static;
      public
        property Parser: TMySQLParser read Heritage.Heritage.Heritage.Heritage.FParser;
      end;

      PConvertFunc = ^TConvertFunc;
      TConvertFunc = packed record
      private type
        TNodes = packed record
          FuncToken: TOffset;
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
        class function Create(const AParser: TMySQLParser; const ANodes: TNodes): TOffset; static;
      public
        property Parser: TMySQLParser read Heritage.Heritage.Heritage.FParser;
      end;

      PCreateDatabaseStmt = ^TCreateDatabaseStmt;
      TCreateDatabaseStmt = packed record
      private type
        TNodes = packed record
          CreateTag: TOffset;
          DatabaseTag: TOffset;
          IfNotExistsTag: TOffset;
          DatabaseIdent: TOffset;
          CharacterSetValue: TOffset;
          CollateValue: TOffset;
        end;
      private
        Heritage: TStmt;
      private
        Nodes: TNodes;
        class function Create(const AParser: TMySQLParser; const ANodes: TNodes): TOffset; static;
      public
        property Parser: TMySQLParser read Heritage.Heritage.Heritage.Heritage.FParser;
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
        class function Create(const AParser: TMySQLParser; const ANodes: TNodes): TOffset; static;
      public
        property Parser: TMySQLParser read Heritage.Heritage.Heritage.Heritage.FParser;
      end;

      PCreateIndexStmt = ^TCreateIndexStmt;
      TCreateIndexStmt = packed record
      private type
        TNodes = packed record
          CreateTag: TOffset;
          IndexTag: TOffset;
          IndexIdent: TOffset;
          OnTag: TOffset;
          TableIdent: TOffset;
          IndexTypeValue: TOffset;
          KeyColumnList: TOffset;
          AlgorithmValue: TOffset;
          CommentValue: TOffset;
          KeyBlockSizeValue: TOffset;
          LockValue: TOffset;
          ParserValue: TOffset;
        end;
      private
        Heritage: TStmt;
      private
        Nodes: TNodes;
        class function Create(const AParser: TMySQLParser; const ANodes: TNodes): TOffset; static;
      public
        property Parser: TMySQLParser read Heritage.Heritage.Heritage.Heritage.FParser;
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
          CharacteristicList: TOffset;
          Body: TOffset;
        end;
      private
        Heritage: TStmt;
      private
        Nodes: TNodes;
        FRoutineType: TRoutineType;
        class function Create(const AParser: TMySQLParser; const ARoutineType: TRoutineType; const ANodes: TNodes): TOffset; static;
      public
        property Parser: TMySQLParser read Heritage.Heritage.Heritage.Heritage.FParser;
        property RoutineType: TRoutineType read FRoutineType;
      end;

      PCreateServerStmt = ^TCreateServerStmt;
      TCreateServerStmt = packed record
      private type
        TNodes = packed record
          CreateTag: TOffset;
          ServerTag: TOffset;
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
        class function Create(const AParser: TMySQLParser; const ANodes: TNodes): TOffset; static;
      public
        property Parser: TMySQLParser read Heritage.Heritage.Heritage.Heritage.FParser;
      end;

      PCreateTableStmt = ^TCreateTableStmt;
      TCreateTableStmt = packed record
      private type

        TFieldAdd = (caAdd, caChange, caModify, caNone);

        PField = ^TField;
        TField = packed record
        private type

          PDefaultFunc = ^TDefaultFunc;
          TDefaultFunc = packed record
          private type
            TNodes = packed record
              FuncToken: TOffset;
              OpenBracket: TOffset;
              CloseBracket: TOffset;
            end;
          private
            Heritage: TRange;
          private
            Nodes: TNodes;
            class function Create(const AParser: TMySQLParser; const ANodes: TNodes): TOffset; static;
          public
            property Parser: TMySQLParser read Heritage.Heritage.Heritage.FParser;
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
          class function Create(const AParser: TMySQLParser; const ANodes: TNodes): TOffset; static;
        public
          property Parser: TMySQLParser read Heritage.Heritage.Heritage.FParser;
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
          class function Create(const AParser: TMySQLParser; const ANodes: TNodes): TOffset; static;
        public
          property Parser: TMySQLParser read Heritage.Heritage.Heritage.FParser;
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
            ColumnIdentList: TOffset;
            KeyBlockSizeValue: TOffset;
            ParserValue: TOffset;
            CommentValue: TOffset;
          end;
        private
          Heritage: TRange;
        private
          Nodes: TNodes;
          class function Create(const AParser: TMySQLParser; const ANodes: TNodes): TOffset; static;
        public
          property Parser: TMySQLParser read Heritage.Heritage.Heritage.FParser;
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
          class function Create(const AParser: TMySQLParser; const ANodes: TNodes): TOffset; static;
        public
          property Parser: TMySQLParser read Heritage.Heritage.Heritage.FParser;
        end;

        PPartition = ^TPartition;
        TPartition = packed record
        private type
          TNodes = packed record
            AddTag: TOffset;
            PartitionTag: TOffset;
            NameIdent: TOffset;
            ValuesNode: TOffset;
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
          class function Create(const AParser: TMySQLParser; const ANodes: TNodes): TOffset; static;
        public
          property Parser: TMySQLParser read Heritage.Heritage.Heritage.FParser;
        end;

        PPartitionValues = ^TPartitionValues;
        TPartitionValues = packed record
        private type
          TNodes = packed record
            ValuesTag: TOffset;
            Value: TOffset;
          end;
        private
          Heritage: TRange;
        private
          Nodes: TNodes;
          class function Create(const AParser: TMySQLParser; const ANodes: TNodes): TOffset; static;
        public
          property Parser: TMySQLParser read Heritage.Heritage.Heritage.FParser;
        end;

        PReference = ^TReference;
        TReference = packed record
        private type
          TNodes = packed record
            Tag: TOffset;
            ParentTableIdent: TOffset;
            IndicesList: TOffset;
            MatchValue: TOffset;
            OnDeleteValue: TOffset;
            OnUpdateValue: TOffset;
          end;
        private
          Heritage: TRange;
        private
          Nodes: TNodes;
          class function Create(const AParser: TMySQLParser; const ANodes: TNodes): TOffset; static;
        public
          property Parser: TMySQLParser read Heritage.Heritage.Heritage.FParser;
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
        class function Create(const AParser: TMySQLParser; const ANodes: TNodes): TOffset; static;
      public
        property Parser: TMySQLParser read Heritage.Heritage.Heritage.Heritage.FParser;
      end;

      PCreateTriggerStmt = ^TCreateTriggerStmt;
      TCreateTriggerStmt = packed record
      private type
        TNodes = packed record
          CreateTag: TOffset;
          DefinerNode: TOffset;
          TriggerTag: TOffset;
          TriggerIdent: TOffset;
          ActionValue: TOffset;
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
        class function Create(const AParser: TMySQLParser; const ANodes: TNodes): TOffset; static;
      public
        property Parser: TMySQLParser read Heritage.Heritage.Heritage.Heritage.FParser;
      end;

      PCreateUserStmt = ^TCreateUserStmt;
      TCreateUserStmt = packed record
      private type
        TNodes = packed record
          CreateTag: TOffset;
          IfTag: TOffset;
          UserSpecifications: TOffset;
          WithTag: TOffset;
          ResourcesList: TOffset;
          PasswordOption: TOffset;
          PasswordDays: TOffset;
          DayTag: TOffset;
          AccountTag: TOffset;
        end;
      private
        Heritage: TStmt;
      private
        Nodes: TNodes;
        class function Create(const AParser: TMySQLParser; const ANodes: TNodes): TOffset; static;
      public
        property Parser: TMySQLParser read Heritage.Heritage.Heritage.Heritage.FParser;
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
          ColumnList: TOffset;
          AsTag: TOffset;
          SelectStmt: TOffset;
          OptionTag: TOffset;
        end;
      private
        Heritage: TStmt;
      private
        Nodes: TNodes;
        class function Create(const AParser: TMySQLParser; const ANodes: TNodes): TOffset; static;
      public
        property Parser: TMySQLParser read Heritage.Heritage.Heritage.Heritage.FParser;
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
        class function Create(const AParser: TMySQLParser; const ANodes: TNodes): TOffset; static;
      public
        property Parser: TMySQLParser read Heritage.Heritage.Heritage.FParser;
      end;

      PDatatype = ^TDatatype;
      TDatatype = packed record
      private type
        TNodes = packed record
          NationalToken: TOffset;
          SignedToken: TOffset;
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
        class function Create(const AParser: TMySQLParser; const ANodes: TNodes): TOffset; static;
      public
        property Parser: TMySQLParser read Heritage.Heritage.Heritage.FParser;
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
        class function Create(const AParser: TMySQLParser; const ADbIdentType: TDbIdentType; const ANodes: TNodes): TOffset; overload; static;
        class function Create(const AParser: TMySQLParser; const ADbIdentType: TDbIdentType;
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
        property Parser: TMySQLParser read Heritage.Heritage.Heritage.FParser;
        property TableIdent: PToken read GetTableIdent;
      end;

      PDeallocatePrepareStmt = ^TDeallocatePrepareStmt;
      TDeallocatePrepareStmt = packed record
      private type
        TNodes = packed record
          StmtTag: TOffset;
          StmtIdent: TOffset;
        end;
      private
        Heritage: TStmt;
      private
        Nodes: TNodes;
        class function Create(const AParser: TMySQLParser; const ANodes: TNodes): TOffset; static;
      public
        property Parser: TMySQLParser read Heritage.Heritage.Heritage.Heritage.FParser;
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
        class function Create(const AParser: TMySQLParser; const ANodes: TNodes): TOffset; static;
      public
        property Parser: TMySQLParser read Heritage.Heritage.Heritage.Heritage.FParser;
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
        class function Create(const AParser: TMySQLParser; const ANodes: TNodes): TOffset; static;
      public
        property Parser: TMySQLParser read Heritage.Heritage.Heritage.Heritage.FParser;
      end;

      PDeclareCursorStmt = ^TDeclareCursorStmt;
      TDeclareCursorStmt = packed record
      private type
        TNodes = packed record
          StmtTag: TOffset;
          Ident: TOffset;
          CursorTag: TOffset;
          ForTag: TOffset;
          SelectStmt: TOffset;
        end;
      private
        Heritage: TStmt;
      private
        Nodes: TNodes;
        class function Create(const AParser: TMySQLParser; const ANodes: TNodes): TOffset; static;
      public
        property Parser: TMySQLParser read Heritage.Heritage.Heritage.Heritage.FParser;
      end;

      PDeclareHandlerStmt = ^TDeclareHandlerStmt;
      TDeclareHandlerStmt = packed record
      private type

        PCondition = ^TCondition;
        TCondition = packed record
        private type
          TNodes = packed record
            ErrorCode: TOffset;
            SQLStateTag: TOffset;
            ConditionIdent: TOffset;
            SQLWarningsTag: TOffset;
            NotFoundTag: TOffset;
            SQLExceptionTag: TOffset;
          end;
        private
          Heritage: TRange;
        private
          Nodes: TNodes;
          class function Create(const AParser: TMySQLParser; const ANodes: TNodes): TOffset; overload; static;
        public
          property Parser: TMySQLParser read Heritage.Heritage.Heritage.FParser;
        end;

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
        class function Create(const AParser: TMySQLParser; const ANodes: TNodes): TOffset; static;
      public
        property Parser: TMySQLParser read Heritage.Heritage.Heritage.Heritage.FParser;
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
        class function Create(const AParser: TMySQLParser; const ANodes: TNodes): TOffset; static;
      public
        property Parser: TMySQLParser read Heritage.Heritage.Heritage.Heritage.FParser;
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
        class function Create(const AParser: TMySQLParser; const ANodes: TNodes): TOffset; static;
      public
        property Parser: TMySQLParser read Heritage.Heritage.Heritage.Heritage.FParser;
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
        class function Create(const AParser: TMySQLParser; const ANodes: TNodes): TOffset; static;
      public
        property Parser: TMySQLParser read Heritage.Heritage.Heritage.Heritage.FParser;
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
        class function Create(const AParser: TMySQLParser; const ANodes: TNodes): TOffset; static;
      public
        property Parser: TMySQLParser read Heritage.Heritage.Heritage.Heritage.FParser;
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
        class function Create(const AParser: TMySQLParser; const ANodes: TNodes): TOffset; static;
      public
        property Parser: TMySQLParser read Heritage.Heritage.Heritage.Heritage.FParser;
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
        class function Create(const AParser: TMySQLParser; const ARoutineType: TRoutineType; const ANodes: TNodes): TOffset; static;
      public
        property Parser: TMySQLParser read Heritage.Heritage.Heritage.Heritage.FParser;
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
        class function Create(const AParser: TMySQLParser; const ANodes: TNodes): TOffset; static;
      public
        property Parser: TMySQLParser read Heritage.Heritage.Heritage.Heritage.FParser;
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
        class function Create(const AParser: TMySQLParser; const ANodes: TNodes): TOffset; static;
      public
        property Parser: TMySQLParser read Heritage.Heritage.Heritage.Heritage.FParser;
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
        class function Create(const AParser: TMySQLParser; const ANodes: TNodes): TOffset; static;
      public
        property Parser: TMySQLParser read Heritage.Heritage.Heritage.Heritage.FParser;
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
        class function Create(const AParser: TMySQLParser; const ANodes: TNodes): TOffset; static;
      public
        property Parser: TMySQLParser read Heritage.Heritage.Heritage.Heritage.FParser;
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
        class function Create(const AParser: TMySQLParser; const ANodes: TNodes): TOffset; static;
      public
        property Parser: TMySQLParser read Heritage.Heritage.Heritage.Heritage.FParser;
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
        class function Create(const AParser: TMySQLParser; const ANodes: TNodes): TOffset; static;
      public
        property Parser: TMySQLParser read Heritage.Heritage.Heritage.FParser;
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
        class function Create(const AParser: TMySQLParser; const ANodes: TNodes): TOffset; static;
      public
        property Parser: TMySQLParser read Heritage.Heritage.Heritage.Heritage.FParser;
      end;

      PExistsFunc = ^TExistsFunc;
      TExistsFunc = packed record
      private type
        TNodes = packed record
          FuncToken: TOffset;
          OpenBracket: TOffset;
          SubQuery: TOffset;
          CloseBracket: TOffset;
        end;
      private
        Heritage: TRange;
      private
        Nodes: TNodes;
        class function Create(const AParser: TMySQLParser; const ANodes: TNodes): TOffset; static;
      public
        property Parser: TMySQLParser read Heritage.Heritage.Heritage.FParser;
      end;

      PExplainStmt = ^TExplainStmt;
      TExplainStmt = packed record
      private type
        TNodes = packed record
          StmtTag: TOffset;
          TableIdent: TOffset;
          ColumnIdent: TOffset;
          ExplainType: TOffset;
          AssignToken: TOffset;
          FormatKeyword: TOffset;
          ExplainStmt: TOffset;
        end;
      private
        Heritage: TStmt;
      private
        Nodes: TNodes;
        class function Create(const AParser: TMySQLParser; const ANodes: TNodes): TOffset; static;
      public
        property Parser: TMySQLParser read Heritage.Heritage.Heritage.Heritage.FParser;
      end;

      PExtractFunc = ^TExtractFunc;
      TExtractFunc = packed record
      private type
        TNodes = packed record
          FuncToken: TOffset;
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
        class function Create(const AParser: TMySQLParser; const ANodes: TNodes): TOffset; static;
      public
        property Parser: TMySQLParser read Heritage.Heritage.Heritage.FParser;
      end;

      PFetchStmt = ^TFetchStmt;
      TFetchStmt = packed record
      private type
        TNodes = packed record
          StmtTag: TOffset;
          FromTag: TOffset;
          CursorIdent: TOffset;
          IntoTag: TOffset;
          VariableList: TOffset;
        end;
      private
        Heritage: TStmt;
      private
        Nodes: TNodes;
        class function Create(const AParser: TMySQLParser; const ANodes: TNodes): TOffset; static;
      public
        property Parser: TMySQLParser read Heritage.Heritage.Heritage.Heritage.FParser;
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
            class function Create(const AParser: TMySQLParser; const ANodes: TNodes): TOffset; static;
          public
            property Parser: TMySQLParser read Heritage.Heritage.Heritage.FParser;
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
        class function Create(const AParser: TMySQLParser; const ANodes: TNodes): TOffset; static;
      public
        property Parser: TMySQLParser read Heritage.Heritage.Heritage.Heritage.FParser;
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
        class function Create(const AParser: TMySQLParser; const AIdent, AArgumentsList: TOffset): TOffset; overload; static;
        class function Create(const AParser: TMySQLParser; const ANodes: TNodes): TOffset; overload; static;
        function GetArguments(): PChild; {$IFNDEF Debug} inline; {$ENDIF}
        function GetIdent(): PChild; {$IFNDEF Debug} inline; {$ENDIF}
      public
        property Arguments: PChild read GetArguments;
        property Ident: PChild read GetIdent;
        property Parser: TMySQLParser read Heritage.Heritage.Heritage.FParser;
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
        class function Create(const AParser: TMySQLParser; const ANodes: TNodes): TOffset; static;
      public
        property Parser: TMySQLParser read Heritage.Heritage.Heritage.FParser;
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
          class function Create(const AParser: TMySQLParser; const ANodes: TNodes): TOffset; static;
        public
          property Parser: TMySQLParser read Heritage.Heritage.Heritage.FParser;
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
          class function Create(const AParser: TMySQLParser; const ANodes: TNodes): TOffset; static;
        public
          property Parser: TMySQLParser read Heritage.Heritage.Heritage.FParser;
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
        class function Create(const AParser: TMySQLParser; const ANodes: TNodes): TOffset; static;
      public
        property Parser: TMySQLParser read Heritage.Heritage.Heritage.Heritage.FParser;
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
          class function Create(const AParser: TMySQLParser; const ANodes: TNodes): TOffset; static;
        public
          property Parser: TMySQLParser read Heritage.Heritage.Heritage.FParser;
        end;

        PUserSpecification = ^TUserSpecification;
        TUserSpecification = packed record
        private type
          TNodes = packed record
            UserIdent: TOffset;
            IdentifiedToken: TOffset;
            PluginIdent: TOffset;
            AsToken: TOffset;
            AuthString: TOffset;
          end;
        private
          Heritage: TRange;
        private
          Nodes: TNodes;
          class function Create(const AParser: TMySQLParser; const ANodes: TNodes): TOffset; static;
        public
          property Parser: TMySQLParser read Heritage.Heritage.Heritage.FParser;
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
          ResourcesList: TOffset;
        end;
      private
        Heritage: TStmt;
      private
        Nodes: TNodes;
        class function Create(const AParser: TMySQLParser; const ANodes: TNodes): TOffset; static;
      public
        property Parser: TMySQLParser read Heritage.Heritage.Heritage.Heritage.FParser;
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
          class function Create(const AParser: TMySQLParser; const ANodes: TNodes): TOffset; static;
        public
          property Parser: TMySQLParser read Heritage.Heritage.Heritage.FParser;
        end;

        TNodes = packed record
          FuncToken: TOffset;
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
        class function Create(const AParser: TMySQLParser; const ANodes: TNodes): TOffset; static;
      public
        property Parser: TMySQLParser read Heritage.Heritage.Heritage.FParser;
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
        class function Create(const AParser: TMySQLParser; const ANodes: TNodes): TOffset; static;
      public
        property Parser: TMySQLParser read Heritage.Heritage.Heritage.Heritage.FParser;
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
          class function Create(const AParser: TMySQLParser; const ANodes: TNodes): TOffset; static;
        public
          property Parser: TMySQLParser read Heritage.Heritage.Heritage.FParser;
        end;

        TNodes = packed record
          BranchList: TOffset;
          EndTag: TOffset;
        end;
      private
        Heritage: TStmt;
      private
        Nodes: TNodes;
        class function Create(const AParser: TMySQLParser; const ANodes: TNodes): TOffset; static;
      public
        property Parser: TMySQLParser read Heritage.Heritage.Heritage.Heritage.FParser;
      end;

      PIgnoreLines = ^TIgnoreLines;
      TIgnoreLines = packed record
      private type
        TNodes = packed record
          IgnoreTag: TOffset;
          NumberToken: TOffset;
          LinesTag: TOffset;
        end;
      private
        Heritage: TRange;
      private
        Nodes: TNodes;
        class function Create(const AParser: TMySQLParser; const ANodes: TNodes): TOffset; static;
      public
        property Parser: TMySQLParser read Heritage.Heritage.Heritage.FParser;
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
        class function Create(const AParser: TMySQLParser; const ANodes: TNodes): TOffset; static;
      public
        property Parser: TMySQLParser read Heritage.Heritage.Heritage.FParser;
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
          class function Create(const AParser: TMySQLParser; const ANodes: TNodes): TOffset; static;
        public
          property Parser: TMySQLParser read Heritage.Heritage.Heritage.FParser;
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
        class function Create(const AParser: TMySQLParser; const ANodes: TNodes): TOffset; static;
      public
        property Parser: TMySQLParser read Heritage.Heritage.Heritage.Heritage.FParser;
      end;

      PIntervalOp = ^TIntervalOp;
      TIntervalOp = packed record
      private type

        PListItem = ^TListItem;
        TListItem = packed record
        private type
          TNodes = packed record
            PlusToken: TOffset;
            IntervalTag: TOffset;
            Interval: TOffset;
          end;
        private
          Heritage: TRange;
        private
          Nodes: TNodes;
          class function Create(const AParser: TMySQLParser; const ANodes: TNodes): TOffset; static;
        end;

        TNodes = packed record
          QuantityExp: TOffset;
          UnitTag: TOffset;
        end;
      private
        Heritage: TRange;
      private
        Nodes: TNodes;
        class function Create(const AParser: TMySQLParser; const ANodes: TNodes): TOffset; static;
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
        class function Create(const AParser: TMySQLParser; const ANodes: TNodes): TOffset; static;
      public
        property Parser: TMySQLParser read Heritage.Heritage.Heritage.Heritage.FParser;
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
        class function Create(const AParser: TMySQLParser; const ANodes: TNodes): TOffset; static;
      public
        property Parser: TMySQLParser read Heritage.Heritage.Heritage.Heritage.FParser;
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
        class function Create(const AParser: TMySQLParser; const ANodes: TNodes): TOffset; static;
      public
        property Parser: TMySQLParser read Heritage.Heritage.Heritage.Heritage.FParser;
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
        class function Create(const AParser: TMySQLParser; const ANodes: TNodes): TOffset; static;
      public
        property Parser: TMySQLParser read Heritage.Heritage.Heritage.FParser;
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
        class function Create(const AParser: TMySQLParser;
          const ANodes: TNodes; const ADelimiterType: TTokenType;
          const AChildrenCount: Integer; const AChildren: array of TOffset): TOffset; static;
        function GetFirstChild(): PChild; {$IFNDEF Debug} inline; {$ENDIF}
      public
        property DelimiterType: TTokenType read FDelimiterType;
        property ElementCount: Word read FElementCount;
        property FirstChild: PChild read GetFirstChild;
        property Parser: TMySQLParser read Heritage.Heritage.Heritage.FParser;
      end;
      PList = ^TList;

      PLoadDataStmt = ^TLoadDataStmt;
      TLoadDataStmt = packed record
      private type
        TNodes = packed record
          LoadDataTag: TOffset;
          PriorityTag: TOffset;
          InfileTag: TOffset;
          FilenameString: TOffset;
          ReplaceIgnoreTag: TOffset;
          IntoTableValue: TOffset;
          PartitionValue: TOffset;
          CharacterSetValue: TOffset;
          ColumnsTag: TOffset;
          ColumnsTerminatedByValue: TOffset;
          EnclosedByValue: TOffset;
          EscapedByValue: TOffset;
          LinesTag: TOffset;
          StartingByValue: TOffset;
          LinesTerminatedByValue: TOffset;
          IgnoreLines: TOffset;
          ColumnList: TOffset;
          SetList: TOffset;
        end;
      private
        Heritage: TStmt;
      private
        Nodes: TNodes;
        class function Create(const AParser: TMySQLParser; const ANodes: TNodes): TOffset; static;
      public
        property Parser: TMySQLParser read Heritage.Heritage.Heritage.Heritage.FParser;
      end;

      PLoadXMLStmt = ^TLoadXMLStmt;
      TLoadXMLStmt = packed record
      private type
        TNodes = packed record
          LoadXMLTag: TOffset;
          PriorityTag: TOffset;
          LocalTag: TOffset;
          InfileTag: TOffset;
          FilenameString: TOffset;
          ReplaceIgnoreTag: TOffset;
          IntoTableValue: TOffset;
          CharacterSetValue: TOffset;
          RowsIdentifiedByValue: TOffset;
          IgnoreLines: TOffset;
          ColumnList: TOffset;
          SetList: TOffset;
        end;
      private
        Heritage: TStmt;
      private
        Nodes: TNodes;
        class function Create(const AParser: TMySQLParser; const ANodes: TNodes): TOffset; static;
      public
        property Parser: TMySQLParser read Heritage.Heritage.Heritage.Heritage.FParser;
      end;

      PLockStmt = ^TLockStmt;
      TLockStmt = packed record
      private type

        PItem = ^TItem;
        TItem = packed record
        private type
          TNodes = packed record
            TableIdent: TOffset;
            AsToken: TOffset;
            AliasIdent: TOffset;
            TypeTag: TOffset;
          end;
        private
          Heritage: TRange;
        private
          Nodes: TNodes;
          class function Create(const AParser: TMySQLParser; const ANodes: TNodes): TOffset; static;
        public
          property Parser: TMySQLParser read Heritage.Heritage.Heritage.FParser;
        end;

        TNodes = packed record
          LockTablesTag: TOffset;
          ItemList: TOffset;
        end;
      private
        Heritage: TStmt;
      private
        Nodes: TNodes;
        class function Create(const AParser: TMySQLParser; const ANodes: TNodes): TOffset; static;
      public
        property Parser: TMySQLParser read Heritage.Heritage.Heritage.Heritage.FParser;
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
        class function Create(const AParser: TMySQLParser; const ANodes: TNodes): TOffset; static;
      public
        property Parser: TMySQLParser read Heritage.Heritage.Heritage.Heritage.FParser;
      end;

      PMatchFunc = ^TMatchFunc;
      TMatchFunc = packed record
      private type
        TNodes = packed record
          FuncToken: TOffset;
          MatchList: TOffset;
          AgainstToken: TOffset;
          OpenBracket: TOffset;
          Expr: TOffset;
          SearchModifierTag: TOffset;
          CloseBracket: TOffset;
        end;
      private
        Heritage: TRange;
      private
        Nodes: TNodes;
        class function Create(const AParser: TMySQLParser; const ANodes: TNodes): TOffset; static;
      public
        property Parser: TMySQLParser read Heritage.Heritage.Heritage.FParser;
      end;

      PPositionFunc = ^TPositionFunc;
      TPositionFunc = packed record
      private type
        TNodes = packed record
          FuncToken: TOffset;
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
        class function Create(const AParser: TMySQLParser; const ANodes: TNodes): TOffset; static;
      public
        property Parser: TMySQLParser read Heritage.Heritage.Heritage.FParser;
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
        class function Create(const AParser: TMySQLParser; const ANodes: TNodes): TOffset; static;
      public
        property Parser: TMySQLParser read Heritage.Heritage.Heritage.Heritage.FParser;
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
        class function Create(const AParser: TMySQLParser; const ANodes: TNodes): TOffset; static;
      public
        property Parser: TMySQLParser read Heritage.Heritage.Heritage.Heritage.FParser;
      end;

      POpenStmt = ^TOpenStmt;
      TOpenStmt = packed record
      private type
        TNodes = packed record
          StmtTag: TOffset;
          CursorIdent: TOffset;
        end;
      private
        Heritage: TStmt;
      private
        Nodes: TNodes;
        class function Create(const AParser: TMySQLParser; const ANodes: TNodes): TOffset; static;
      public
        property Parser: TMySQLParser read Heritage.Heritage.Heritage.Heritage.FParser;
      end;

      POptimizeStmt = ^TOptimizeStmt;
      TOptimizeStmt = packed record
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
        class function Create(const AParser: TMySQLParser; const ANodes: TNodes): TOffset; static;
      public
        property Parser: TMySQLParser read Heritage.Heritage.Heritage.Heritage.FParser;
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
        class function Create(const AParser: TMySQLParser; const ANodes: TNodes): TOffset; static;
      public
        property Parser: TMySQLParser read Heritage.Heritage.Heritage.FParser;
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
        class function Create(const AParser: TMySQLParser; const ANodes: TNodes): TOffset; static;
      public
        property Parser: TMySQLParser read Heritage.Heritage.Heritage.Heritage.FParser;
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
          class function Create(const AParser: TMySQLParser; const ANodes: TNodes): TOffset; static;
        public
          property Parser: TMySQLParser read Heritage.Heritage.Heritage.FParser;
        end;

        TNodes = packed record
          RenameTag: TOffset;
          RenameList: TOffset;
        end;
      private
        Heritage: TStmt;
      private
        Nodes: TNodes;
        class function Create(const AParser: TMySQLParser; const ANodes: TNodes): TOffset; static;
      public
        property Parser: TMySQLParser read Heritage.Heritage.Heritage.Heritage.FParser;
      end;

      PRepairStmt = ^TRepairStmt;
      TRepairStmt = packed record
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
        class function Create(const AParser: TMySQLParser; const ANodes: TNodes): TOffset; static;
      public
        property Parser: TMySQLParser read Heritage.Heritage.Heritage.Heritage.FParser;
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
        class function Create(const AParser: TMySQLParser; const ANodes: TNodes): TOffset; static;
      public
        property Parser: TMySQLParser read Heritage.Heritage.Heritage.Heritage.FParser;
      end;

      PResetStmt = ^TResetStmt;
      TResetStmt = packed record
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
          class function Create(const AParser: TMySQLParser; const ANodes: TNodes): TOffset; static;
        public
          property Parser: TMySQLParser read Heritage.Heritage.Heritage.FParser;
        end;

        TNodes = packed record
          StmtTag: TOffset;
          OptionList: TOffset;
        end;
      private
        Heritage: TStmt;
      private
        Nodes: TNodes;
        class function Create(const AParser: TMySQLParser; const ANodes: TNodes): TOffset; static;
      public
        property Parser: TMySQLParser read Heritage.Heritage.Heritage.Heritage.FParser;
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
        class function Create(const AParser: TMySQLParser; const ANodes: TNodes): TOffset; static;
      public
        property Parser: TMySQLParser read Heritage.Heritage.Heritage.Heritage.FParser;
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
        class function Create(const AParser: TMySQLParser; const ANodes: TNodes): TOffset; static;
      public
        property Parser: TMySQLParser read Heritage.Heritage.Heritage.Heritage.FParser;
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
        class function Create(const AParser: TMySQLParser; const ANodes: TNodes): TOffset; static;
      public
        property Parser: TMySQLParser read Heritage.Heritage.Heritage.Heritage.FParser;
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
        class function Create(const AParser: TMySQLParser; const ANodes: TNodes): TOffset; static;
      public
        property Parser: TMySQLParser read Heritage.Heritage.Heritage.FParser;
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
        class function Create(const AParser: TMySQLParser; const ANodes: TNodes): TOffset; static;
      public
        property Parser: TMySQLParser read Heritage.Heritage.Heritage.Heritage.FParser;
      end;

      PSchedule = ^TSchedule;
      TSchedule = packed record
      private type
        TNodes = packed record
          At: packed record
            Tag: TOffset;
            Timestamp: TOffset;
            IntervalList: TIntervalList;
          end;
          Every: packed record
            Tag: TOffset;
            Interval: TOffset;
          end;
          Starts: packed record
            Tag: TOffset;
            Timestamp: TOffset;
            IntervalList: TIntervalList;
          end;
          Ends: packed record
            Tag: TOffset;
            Timestamp: TOffset;
            IntervalList: TIntervalList;
          end;
        end;
      private
        Heritage: TRange;
      private
        Nodes: TNodes;
        class function Create(const AParser: TMySQLParser; const ANodes: TNodes): TOffset; static;
      public
        property Parser: TMySQLParser read Heritage.Heritage.Heritage.FParser;
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
        class function Create(const AParser: TMySQLParser; const ANodes: TNodes): TOffset; static;
      public
        property Parser: TMySQLParser read Heritage.Heritage.Heritage.FParser;
      end;

      PSelectStmt = ^TSelectStmt;
      TSelectStmt = packed record
      private type

        PColumn = ^TColumn;
        TColumn = packed record
        private type
          TNodes = packed record
            Expr: TOffset;
            AsToken: TOffset;
            AliasIdent: TOffset;
          end;
        private
          Heritage: TRange;
        private
          Nodes: TNodes;
          class function Create(const AParser: TMySQLParser; const ANodes: TNodes): TOffset; static;
        public
          property Parser: TMySQLParser read Heritage.Heritage.Heritage.FParser;
        end;

        PTableFactor = ^TTableFactor;
        TTableFactor = packed record
        private type

          PIndexHint = ^TIndexHint;
          TIndexHint = packed record
          public type
            TNodes = record
              KindTag: TOffset;
              ForValue: TOffset;
              IndexList: TOffset;
            end;
          private
            Heritage: TRange;
          private
            Nodes: TNodes;
            class function Create(const AParser: TMySQLParser; const ANodes: TNodes): TOffset; static;
          public
            property Parser: TMySQLParser read Heritage.Heritage.Heritage.FParser;
          end;

          TNodes = packed record
            TableIdent: TOffset;
            PartitionTag: TOffset;
            Partitions: TOffset;
            AsToken: TOffset;
            AliasIdent: TOffset;
            IndexHintList: TOffset;
            SelectStmt: TOffset;
          end;

        private
          Heritage: TRange;
        private
          Nodes: TNodes;
          class function Create(const AParser: TMySQLParser; const ANodes: TNodes): TOffset; static;
        public
          property Parser: TMySQLParser read Heritage.Heritage.Heritage.FParser;
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
          class function Create(const AParser: TMySQLParser; const AJoinType: TJoinType; const ANodes: TNodes): TOffset; static;
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
          class function Create(const AParser: TMySQLParser; const ANodes: TNodes): TOffset; static;
        public
          property Parser: TMySQLParser read Heritage.Heritage.Heritage.FParser;
        end;

        PTableFactorSelect = ^TTableFactorSelect;
        TTableFactorSelect = packed record
        private type
          TNodes = packed record
            SelectStmt: TOffset;
            AsToken: TOffset;
            AliasIdent: TOffset;
          end;
        private
          Heritage: TRange;
        private
          Nodes: TNodes;
          class function Create(const AParser: TMySQLParser; const ANodes: TNodes): TOffset; static;
        public
          property Parser: TMySQLParser read Heritage.Heritage.Heritage.FParser;
        end;

        PGroup = ^TGroup;
        TGroup = packed record
        private type
          TNodes = packed record
            Expr: TOffset;
            Direction: TOffset;
          end;
        private
          Heritage: TRange;
        private
          Nodes: TNodes;
          class function Create(const AParser: TMySQLParser; const ANodes: TNodes): TOffset; static;
        public
          property Parser: TMySQLParser read Heritage.Heritage.Heritage.FParser;
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
          class function Create(const AParser: TMySQLParser; const ANodes: TNodes): TOffset; static;
        public
          property Parser: TMySQLParser read Heritage.Heritage.Heritage.FParser;
        end;

        TIntoNodes = packed record
          Tag: TOffset;
          Filename: TOffset;
          CharacterSetValue: TOffset;
          VariableList: TOffset;
          FieldsTerminatedByValue: TOffset;
          OptionallyEnclosedByValue: TOffset;
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
        class function Create(const AParser: TMySQLParser; const ANodes: TNodes): TOffset; static;
      public
        property Parser: TMySQLParser read Heritage.Heritage.Heritage.Heritage.FParser;
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
          class function Create(const AParser: TMySQLParser; const ANodes: TNodes): TOffset; static;
        public
          property Parser: TMySQLParser read Heritage.Heritage.Heritage.FParser;
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
        class function Create(const AParser: TMySQLParser; const ANodes: TNodes): TOffset; static;
      public
        property Parser: TMySQLParser read Heritage.Heritage.Heritage.Heritage.FParser;
      end;

      PSetNamesStmt = ^TSetNamesStmt;
      TSetNamesStmt = packed record
      private type
        TNodes = packed record
          StmtTag: TOffset;
          ConstValue: TOffset;
        end;
      private
        Heritage: TStmt;
      private
        Nodes: TNodes;
        class function Create(const AParser: TMySQLParser; const ANodes: TNodes): TOffset; static;
      public
        property Parser: TMySQLParser read Heritage.Heritage.Heritage.Heritage.FParser;
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
        class function Create(const AParser: TMySQLParser; const ANodes: TNodes): TOffset; static;
      public
        property Parser: TMySQLParser read Heritage.Heritage.Heritage.Heritage.FParser;
      end;

      PSetTransactionStmt = ^TSetTransactionStmt;
      TSetTransactionStmt = packed record
      private type

        PCharacteristic = ^TCharacteristic;
        TCharacteristic = packed record
        private type
          TNodes = packed record
            KindTag: TOffset;
            Value: TOffset;
          end;
        private
          Heritage: TRange;
        private
          Nodes: TNodes;
          class function Create(const AParser: TMySQLParser; const ANodes: TNodes): TOffset; static;
        public
          property Parser: TMySQLParser read Heritage.Heritage.Heritage.FParser;
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
        class function Create(const AParser: TMySQLParser; const ANodes: TNodes): TOffset; static;
      public
        property Parser: TMySQLParser read Heritage.Heritage.Heritage.Heritage.FParser;
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
        class function Create(const AParser: TMySQLParser; const ANodes: TNodes): TOffset; static;
      public
        property Parser: TMySQLParser read Heritage.Heritage.Heritage.Heritage.FParser;
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
        class function Create(const AParser: TMySQLParser; const ANodes: TNodes): TOffset; static;
      public
        property Parser: TMySQLParser read Heritage.Heritage.Heritage.Heritage.FParser;
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
        class function Create(const AParser: TMySQLParser; const ANodes: TNodes): TOffset; static;
      public
        property Parser: TMySQLParser read Heritage.Heritage.Heritage.Heritage.FParser;
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
        class function Create(const AParser: TMySQLParser; const ANodes: TNodes): TOffset; static;
      public
        property Parser: TMySQLParser read Heritage.Heritage.Heritage.Heritage.FParser;
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
        class function Create(const AParser: TMySQLParser; const ANodes: TNodes): TOffset; static;
      public
        property Parser: TMySQLParser read Heritage.Heritage.Heritage.Heritage.FParser;
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
        class function Create(const AParser: TMySQLParser; const ANodes: TNodes): TOffset; static;
      public
        property Parser: TMySQLParser read Heritage.Heritage.Heritage.Heritage.FParser;
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
        class function Create(const AParser: TMySQLParser; const ANodes: TNodes): TOffset; static;
      public
        property Parser: TMySQLParser read Heritage.Heritage.Heritage.Heritage.FParser;
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
        class function Create(const AParser: TMySQLParser; const ANodes: TNodes): TOffset; static;
      public
        property Parser: TMySQLParser read Heritage.Heritage.Heritage.Heritage.FParser;
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
        class function Create(const AParser: TMySQLParser; const ANodes: TNodes): TOffset; static;
      public
        property Parser: TMySQLParser read Heritage.Heritage.Heritage.Heritage.FParser;
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
        class function Create(const AParser: TMySQLParser; const ANodes: TNodes): TOffset; static;
      public
        property Parser: TMySQLParser read Heritage.Heritage.Heritage.Heritage.FParser;
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
        class function Create(const AParser: TMySQLParser; const ANodes: TNodes): TOffset; static;
      public
        property Parser: TMySQLParser read Heritage.Heritage.Heritage.Heritage.FParser;
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
        class function Create(const AParser: TMySQLParser; const ANodes: TNodes): TOffset; static;
      public
        property Parser: TMySQLParser read Heritage.Heritage.Heritage.Heritage.FParser;
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
        class function Create(const AParser: TMySQLParser; const ANodes: TNodes): TOffset; static;
      public
        property Parser: TMySQLParser read Heritage.Heritage.Heritage.Heritage.FParser;
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
        class function Create(const AParser: TMySQLParser; const ANodes: TNodes): TOffset; static;
      public
        property Parser: TMySQLParser read Heritage.Heritage.Heritage.Heritage.FParser;
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
        class function Create(const AParser: TMySQLParser; const ANodes: TNodes): TOffset; static;
      public
        property Parser: TMySQLParser read Heritage.Heritage.Heritage.Heritage.FParser;
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
        class function Create(const AParser: TMySQLParser; const ANodes: TNodes): TOffset; static;
      public
        property Parser: TMySQLParser read Heritage.Heritage.Heritage.Heritage.FParser;
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
        class function Create(const AParser: TMySQLParser; const ANodes: TNodes): TOffset; static;
      public
        property Parser: TMySQLParser read Heritage.Heritage.Heritage.Heritage.FParser;
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
        class function Create(const AParser: TMySQLParser; const ANodes: TNodes): TOffset; static;
      public
        property Parser: TMySQLParser read Heritage.Heritage.Heritage.Heritage.FParser;
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
        class function Create(const AParser: TMySQLParser; const ANodes: TNodes): TOffset; static;
      public
        property Parser: TMySQLParser read Heritage.Heritage.Heritage.Heritage.FParser;
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
        class function Create(const AParser: TMySQLParser; const ANodes: TNodes): TOffset; static;
      public
        property Parser: TMySQLParser read Heritage.Heritage.Heritage.Heritage.FParser;
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
        class function Create(const AParser: TMySQLParser; const ANodes: TNodes): TOffset; static;
      public
        property Parser: TMySQLParser read Heritage.Heritage.Heritage.Heritage.FParser;
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
        class function Create(const AParser: TMySQLParser; const ANodes: TNodes): TOffset; static;
      public
        property Parser: TMySQLParser read Heritage.Heritage.Heritage.Heritage.FParser;
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
        class function Create(const AParser: TMySQLParser; const ANodes: TNodes): TOffset; static;
      public
        property Parser: TMySQLParser read Heritage.Heritage.Heritage.Heritage.FParser;
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
        class function Create(const AParser: TMySQLParser; const ANodes: TNodes): TOffset; static;
      public
        property Parser: TMySQLParser read Heritage.Heritage.Heritage.Heritage.FParser;
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
        class function Create(const AParser: TMySQLParser; const ANodes: TNodes): TOffset; static;
      public
        property Parser: TMySQLParser read Heritage.Heritage.Heritage.Heritage.FParser;
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
        class function Create(const AParser: TMySQLParser; const ANodes: TNodes): TOffset; static;
      public
        property Parser: TMySQLParser read Heritage.Heritage.Heritage.Heritage.FParser;
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
        class function Create(const AParser: TMySQLParser; const ANodes: TNodes): TOffset; static;
      public
        property Parser: TMySQLParser read Heritage.Heritage.Heritage.Heritage.FParser;
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
        class function Create(const AParser: TMySQLParser; const ANodes: TNodes): TOffset; static;
      public
        property Parser: TMySQLParser read Heritage.Heritage.Heritage.Heritage.FParser;
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
        class function Create(const AParser: TMySQLParser; const ANodes: TNodes): TOffset; static;
      public
        property Parser: TMySQLParser read Heritage.Heritage.Heritage.Heritage.FParser;
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
        class function Create(const AParser: TMySQLParser; const ANodes: TNodes): TOffset; static;
      public
        property Parser: TMySQLParser read Heritage.Heritage.Heritage.Heritage.FParser;
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
        class function Create(const AParser: TMySQLParser; const ANodes: TNodes): TOffset; static;
      public
        property Parser: TMySQLParser read Heritage.Heritage.Heritage.Heritage.FParser;
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
        class function Create(const AParser: TMySQLParser; const ANodes: TNodes): TOffset; static;
      public
        property Parser: TMySQLParser read Heritage.Heritage.Heritage.Heritage.FParser;
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
        class function Create(const AParser: TMySQLParser; const ANodes: TNodes): TOffset; static;
      public
        property Parser: TMySQLParser read Heritage.Heritage.Heritage.Heritage.FParser;
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
        class function Create(const AParser: TMySQLParser; const ANodes: TNodes): TOffset; static;
      public
        property Parser: TMySQLParser read Heritage.Heritage.Heritage.Heritage.FParser;
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
        class function Create(const AParser: TMySQLParser; const ANodes: TNodes): TOffset; static;
      public
        property Parser: TMySQLParser read Heritage.Heritage.Heritage.Heritage.FParser;
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
        class function Create(const AParser: TMySQLParser; const ANodes: TNodes): TOffset; static;
      public
        property Parser: TMySQLParser read Heritage.Heritage.Heritage.Heritage.FParser;
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
        class function Create(const AParser: TMySQLParser; const ANodes: TNodes): TOffset; static;
      public
        property Parser: TMySQLParser read Heritage.Heritage.Heritage.Heritage.FParser;
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
        class function Create(const AParser: TMySQLParser; const ANodes: TNodes): TOffset; static;
      public
        property Parser: TMySQLParser read Heritage.Heritage.Heritage.Heritage.FParser;
      end;

      PShowTableStatusStmt = ^TShowTableStatusStmt;
      TShowTableStatusStmt = packed record
      private type
        TNodes = packed record
          ShowTag: TOffset;
          FromValue: TOffset;
          LikeValue: TOffset;
          WhereValue: TOffset;
        end;
      private
        Heritage: TStmt;
      private
        Nodes: TNodes;
        class function Create(const AParser: TMySQLParser; const ANodes: TNodes): TOffset; static;
      public
        property Parser: TMySQLParser read Heritage.Heritage.Heritage.Heritage.FParser;
      end;

      PShowTablesStmt = ^TShowTablesStmt;
      TShowTablesStmt = packed record
      private type
        TNodes = packed record
          ShowTag: TOffset;
          FullTag: TOffset;
          TablesTag: TOffset;
          FromValue: TOffset;
          LikeValue: TOffset;
          WhereValue: TOffset;
        end;
      private
        Heritage: TStmt;
      private
        Nodes: TNodes;
        class function Create(const AParser: TMySQLParser; const ANodes: TNodes): TOffset; static;
      public
        property Parser: TMySQLParser read Heritage.Heritage.Heritage.Heritage.FParser;
      end;

      PShowTriggersStmt = ^TShowTriggersStmt;
      TShowTriggersStmt = packed record
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
        class function Create(const AParser: TMySQLParser; const ANodes: TNodes): TOffset; static;
      public
        property Parser: TMySQLParser read Heritage.Heritage.Heritage.Heritage.FParser;
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
        class function Create(const AParser: TMySQLParser; const ANodes: TNodes): TOffset; static;
      public
        property Parser: TMySQLParser read Heritage.Heritage.Heritage.Heritage.FParser;
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
        class function Create(const AParser: TMySQLParser; const ANodes: TNodes): TOffset; static;
      public
        property Parser: TMySQLParser read Heritage.Heritage.Heritage.Heritage.FParser;
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
        class function Create(const AParser: TMySQLParser; const ANodes: TNodes): TOffset; static;
      public
        property Parser: TMySQLParser read Heritage.Heritage.Heritage.Heritage.FParser;
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
          class function Create(const AParser: TMySQLParser; const ANodes: TNodes): TOffset; static;
        public
          property Parser: TMySQLParser read Heritage.Heritage.Heritage.FParser;
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
        class function Create(const AParser: TMySQLParser; const ANodes: TNodes): TOffset; static;
      public
        property Parser: TMySQLParser read Heritage.Heritage.Heritage.Heritage.FParser;
      end;

      PSubstringFunc = ^TSubstringFunc;
      TSubstringFunc = packed record
      private type
        TNodes = packed record
          FuncToken: TOffset;
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
        class function Create(const AParser: TMySQLParser; const ANodes: TNodes): TOffset; static;
      public
        property Parser: TMySQLParser read Heritage.Heritage.Heritage.FParser;
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
        class function Create(const AParser: TMySQLParser; const AOperand1, ASounds, ALike, AOperand2: TOffset): TOffset; static;
      public
        property Parser: TMySQLParser read Heritage.Heritage.Heritage.FParser;
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
        class function Create(const AParser: TMySQLParser; const ANodes: TNodes): TOffset; static;
      public
        property Parser: TMySQLParser read Heritage.Heritage.Heritage.Heritage.FParser;
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
        class function Create(const AParser: TMySQLParser; const ANodes: TNodes): TOffset; static;
      public
        property Parser: TMySQLParser read Heritage.Heritage.Heritage.Heritage.FParser;
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
        class function Create(const AParser: TMySQLParser; const ANodes: TNodes): TOffset; static;
      public
        property Parser: TMySQLParser read Heritage.Heritage.Heritage.Heritage.FParser;
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
        class function Create(const AParser: TMySQLParser; const ANodes: TNodes): TOffset; static;
      public
        property Parser: TMySQLParser read Heritage.Heritage.Heritage.FParser;
      end;

      PSubAreaSelectStmt = ^TSubAreaSelectStmt;
      TSubAreaSelectStmt = packed record
      private type
        TNodes = packed record
          SelectStmt1: TOffset;
          UnionTag: TOffset;
          SelectStmt2: TOffset;
        end;
      private
        Heritage: TStmt;
      private
        Nodes: TNodes;
        class function Create(const AParser: TMySQLParser; const ANodes: TNodes): TOffset; static;
      public
        property Parser: TMySQLParser read Heritage.Heritage.Heritage.Heritage.FParser;
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
        class function Create(const AParser: TMySQLParser; const ANodes: TNodes): TOffset; static;
      public
        property Parser: TMySQLParser read Heritage.Heritage.Heritage.FParser;
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
        end;
      private
        Heritage: TRange;
      private
        Nodes: TNodes;
        class function Create(const AParser: TMySQLParser; const ANodes: TNodes): TOffset; static;
      public
        property Parser: TMySQLParser read Heritage.Heritage.Heritage.FParser;
      end;

      PTruncateStmt = ^TTruncateStmt;
      TTruncateStmt = packed record
      private type
        TNodes = packed record
          TruncateTag: TOffset;
          TableTag: TOffset;
          TableIdent: TOffset;
        end;
      private
        Heritage: TStmt;
      private
        Nodes: TNodes;
        class function Create(const AParser: TMySQLParser; const ANodes: TNodes): TOffset; static;
      public
        property Parser: TMySQLParser read Heritage.Heritage.Heritage.Heritage.FParser;
      end;

      PTrimFunc = ^TTrimFunc;
      TTrimFunc = packed record
      private type
        TNodes = packed record
          FuncToken: TOffset;
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
        class function Create(const AParser: TMySQLParser; const ANodes: TNodes): TOffset; static;
      public
        property Parser: TMySQLParser read Heritage.Heritage.Heritage.FParser;
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
        class function Create(const AParser: TMySQLParser; const AOperator, AOperand: TOffset): TOffset; static;
      public
        property Parser: TMySQLParser read Heritage.Heritage.Heritage.FParser;
      end;

      PUnknownStmt = ^TUnknownStmt;
      TUnknownStmt = packed record
      private
        Heritage: TStmt;
      private
        class function Create(const AParser: TMySQLParser; const ATokenCount: Integer; const ATokens: array of TOffset): TOffset; static;
      public
        property Parser: TMySQLParser read Heritage.Heritage.Heritage.Heritage.FParser;
      end;

      PUnlockStmt = ^TUnlockStmt;
      TUnlockStmt = packed record
      private type
        TNodes = packed record
          UnlockTablesTag: TOffset;
        end;
      private
        Heritage: TStmt;
      private
        Nodes: TNodes;
        class function Create(const AParser: TMySQLParser; const ANodes: TNodes): TOffset; static;
      public
        property Parser: TMySQLParser read Heritage.Heritage.Heritage.Heritage.FParser;
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
        class function Create(const AParser: TMySQLParser; const ANodes: TNodes): TOffset; static;
      public
        property Parser: TMySQLParser read Heritage.Heritage.Heritage.Heritage.FParser;
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
        class function Create(const AParser: TMySQLParser; const ANodes: TNodes): TOffset; static;
      public
        property Parser: TMySQLParser read Heritage.Heritage.Heritage.FParser;
      end;

      PUseStmt = ^TUseStmt;
      TUseStmt = packed record
      private type
        TNodes = packed record
          StmtToken: TOffset;
          DbNameNode: TOffset;
        end;
      private
        Heritage: TStmt;
      private
        Nodes: TNodes;
        class function Create(const AParser: TMySQLParser; const ANodes: TNodes): TOffset; static;
      public
        property Parser: TMySQLParser read Heritage.Heritage.Heritage.Heritage.FParser;
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
        class function Create(const AParser: TMySQLParser; const ANodes: TNodes): TOffset; static;
      public
        property Parser: TMySQLParser read Heritage.Heritage.Heritage.FParser;
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
        class function Create(const AParser: TMySQLParser; const ANodes: TNodes): TOffset; static;
      public
        property Parser: TMySQLParser read Heritage.Heritage.Heritage.FParser;
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
          end;
        private
          Heritage: TRange;
        private
          Nodes: TNodes;
          class function Create(const AParser: TMySQLParser; const ANodes: TNodes): TOffset; static;
        public
          property Parser: TMySQLParser read Heritage.Heritage.Heritage.FParser;
        end;

        TNodes = packed record
          FuncToken: TOffset;
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
        class function Create(const AParser: TMySQLParser; const ANodes: TNodes): TOffset; static;
      public
        property Parser: TMySQLParser read Heritage.Heritage.Heritage.FParser;
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
        class function Create(const AParser: TMySQLParser; const ANodes: TNodes): TOffset; static;
      public
        property Parser: TMySQLParser read Heritage.Heritage.Heritage.Heritage.FParser;
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
          class function Create(const AParser: TMySQLParser; const ANodes: TNodes): TOffset; static;
        public
          property Parser: TMySQLParser read Heritage.Heritage.Heritage.FParser;
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
        class function Create(const AParser: TMySQLParser; const ANodes: TNodes): TOffset; static;
      public
        property Parser: TMySQLParser read Heritage.Heritage.Heritage.Heritage.FParser;
      end;

  protected
    diBIGINT,
    diBINARY,
    diBIT,
    diBLOB,
    diBOOL,
    diBOOLEAN,
    diCHAR,
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
    diINTEGER,
    diLARGEINT,
    diLINESTRING,
    diJSON,
    diLONG,
    diLONGBLOB,
    diLONGTEXT,
    diMEDIUMBLOB,
    diMEDIUMINT,
    diMEDIUMTEXT,
    diMULTILINESTRING,
    diMULTIPOINT,
    diMULTIPOLYGON,
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
    kiDAY,
    kiDAY_HOUR,
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
    kiHOUR_MINUTE,
    kiHOUR_SECOND,
    kiIDENTIFIED,
    kiIF,
    kiIGNORE,
    kiIMPORT,
    kiIN,
    kiINDEX,
    kiINDEXES,
    kiINFILE,
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
    kiVALUE,
    kiVALUES,
    kiVARIABLES,
    kiVIEW,
    kiVIRTUAL,
    kiWARNINGS,
    kiWEEK,
    kiWHEN,
    kiWHERE,
    kiWHILE,
    kiWRAPPER,
    kiWRITE,
    kiWITH,
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
    FCurrentToken: TOffset; // Cache for speeding
    FErrorCode: Byte;
    FErrorLine: Integer;
    FErrorToken: TOffset;
    FPreviousToken: TOffset;
    FInPL_SQL: Integer;
    OperatorTypeByKeywordIndex: array of TOperatorType;
    function GetDatatypes(): string;
    function GetError(): Boolean; {$IFNDEF Debug} inline; {$ENDIF}
    function GetErrorCode(): Byte;
    function GetErrorMessage(): string; overload; {$IFNDEF Debug} inline; {$ENDIF}
    function GetErrorMessage(const AErrorCode: Byte): string; overload;
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
    TokenIndex: Integer;
    KeywordList: TWordList;
    FMySQLVersion: Integer;
    ParsedNodes: record
      Mem: PAnsiChar;
      UsedSize: Integer;
      MemSize: Integer;
    end;
    ParseText: string;
    ParsePosition: record
      Text: PChar;
      Length: Integer;
    end;
    FRoot: TOffset;
    Texts: record
      Mem: PAnsiChar;
      UsedSize: Integer;
      MemSize: Integer;
    end;
    InCreateFunctionStmt: Boolean;
    InCreateProcedureStmt: Boolean;
    TokenBuffer: record
      Count: Integer;
      Tokens: array [0 .. 50 - 1] of TOffset;
    end;

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
    procedure FormatAlterTableStmt(const Nodes: TAlterTableStmt.TNodes);
    procedure FormatAlterViewStmt(const Nodes: TAlterViewStmt.TNodes);
    procedure FormatBeginLabel(const Nodes: TBeginLabel.TNodes);
    procedure FormatCallStmt(const Nodes: TCallStmt.TNodes);
    procedure FormatCaseOp(const Nodes: TCaseOp.TNodes);
    procedure FormatCaseStmt(const Nodes: TCaseStmt.TNodes);
    procedure FormatCaseStmtBranch(const Nodes: TCaseStmt.TBranch.TNodes);
    procedure FormatCastFunc(const Nodes: TCastFunc.TNodes);
    procedure FormatCharFunc(const Nodes: TCharFunc.TNodes);
    procedure FormatCompoundStmt(const Nodes: TCompoundStmt.TNodes);
    procedure FormatConvertFunc(const Nodes: TConvertFunc.TNodes);
    procedure FormatCreateEventStmt(const Nodes: TCreateEventStmt.TNodes);
    procedure FormatCreateIndexStmt(const Nodes: TCreateIndexStmt.TNodes);
    procedure FormatCreateRoutineStmt(const Nodes: TCreateRoutineStmt.TNodes);
    procedure FormatCreateServerStmt(const Nodes: TCreateServerStmt.TNodes);
    procedure FormatCreateTableStmt(const Nodes: TCreateTableStmt.TNodes);
    procedure FormatCreateTableStmtField(const Nodes: TCreateTableStmt.TField.TNodes);
    procedure FormatCreateTableStmtFieldDefaultFunc(const Nodes: TCreateTableStmt.TField.TDefaultFunc.TNodes);
    procedure FormatCreateTableStmtKey(const Nodes: TCreateTableStmt.TKey.TNodes);
    procedure FormatCreateTableStmtKeyColumn(const Nodes: TCreateTableStmt.TKeyColumn.TNodes);
    procedure FormatCreateTableStmtPartition(const Nodes: TCreateTableStmt.TPartition.TNodes);
    procedure FormatCreateTableStmtPartitionValues(const Nodes: TCreateTableStmt.TPartitionValues.TNodes);
    procedure FormatCreateTableStmtReference(const Nodes: TCreateTableStmt.TReference.TNodes);
    procedure FormatCreateTriggerStmt(const Nodes: TCreateTriggerStmt.TNodes);
    procedure FormatCreateUserStmt(const Nodes: TCreateUserStmt.TNodes);
    procedure FormatCreateViewStmt(const Nodes: TCreateViewStmt.TNodes);
    procedure FormatCurrentTimestamp(const Nodes: TCurrentTimestamp.TNodes);
    procedure FormatComments(const Token: PToken; Start: Boolean = False);
    procedure FormatDatatype(const Nodes: TDatatype.TNodes);
    procedure FormatDbIdent(const Nodes: TDbIdent.TNodes);
    procedure FormatDeclareCursorStmt(const Nodes: TDeclareCursorStmt.TNodes);
    procedure FormatDeclareHandlerStmt(const Nodes: TDeclareHandlerStmt.TNodes);
    procedure FormatDeleteStmt(const Nodes: TDeleteStmt.TNodes);
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
    procedure FormatSubstringFunc(const Nodes: TSubstringFunc.TNodes);
    procedure FormatToken(const Token: PToken);
    procedure FormatTrimFunc(const Nodes: TTrimFunc.TNodes);
    procedure FormatUnaryOp(const Nodes: TUnaryOp.TNodes);
    procedure FormatUnknownStmt(const Node: PUnknownStmt);
    procedure FormatUser(const Nodes: TUser.TNodes);
    procedure FormatValue(const Nodes: TValue.TNodes);
    procedure FormatVariable(const Nodes: TVariable.TNodes);
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
    function IsToken(const ANode: PNode): Boolean; overload; {$IFNDEF Debug} inline; {$ENDIF}
    function IsToken(const ANode: TOffset): Boolean; overload; {$IFNDEF Debug} inline; {$ENDIF}
    function NewNode(const ANodeType: TNodeType): TOffset;
    function NewText(const Text: PChar; const Length: Integer): TOffset;
    function NodePtr(const ANode: TOffset): PNode; {$IFNDEF Debug} inline; {$ENDIF}
    function NodeSize(const NodeType: TNodeType): Integer;
    function ParseRoot(): TOffset; overload;
    function ParseAnalyzeStmt(): TOffset;
    function ParseAliasIdent(): TOffset;
    function ParseAlterDatabaseStmt(): TOffset;
    function ParseAlterEventStmt(): TOffset;
    function ParseAlterInstanceStmt(): TOffset;
    function ParseAlterRoutineStmt(const ARoutineType: TRoutineType): TOffset;
    function ParseAlterServerStmt(): TOffset;
    function ParseAlterTableStmt(): TOffset;
    function ParseAlterTableStmtAlterColumn(): TOffset;
    function ParseAlterTableStmtConvertTo(): TOffset;
    function ParseAlterTableStmtDropItem(): TOffset;
    function ParseAlterTableStmtExchangePartition(): TOffset;
    function ParseAlterTableStmtReorganizePartition(): TOffset;
    function ParseAlterStmt(): TOffset;
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
    function ParseCheckStmt(): TOffset;
    function ParseCheckStmtOption(): TOffset;
    function ParseChecksumStmt(): TOffset;
    function ParseCloseStmt(): TOffset;
    function ParseCommitStmt(): TOffset;
    function ParseCompoundStmt(): TOffset;
    function ParseConvertFunc(): TOffset;
    function ParseFieldIdent(): TOffset;
    function ParseCreateDatabaseStmt(): TOffset;
    function ParseCreateEventStmt(): TOffset;
    function ParseCreateIndexStmt(): TOffset;
    function ParseCreateRoutineStmt(const ARoutineType: TRoutineType): TOffset;
    function ParseCreateRoutineStmtCharacteristList(): TOffset;
    function ParseCreateServerStmt(): TOffset;
    function ParseCreateServerStmtOptionList(): TOffset;
    function ParseCreateStmt(): TOffset; {$IFNDEF Debug} inline; {$ENDIF}
    function ParseCreateTableStmt(): TOffset;
    function ParseCreateTableStmtField(const Add: TCreateTableStmt.TFieldAdd = caNone): TOffset;
    function ParseCreateTableStmtFieldDefaultFunc(): TOffset;
    function ParseCreateTableStmtDefinition(): TOffset; overload;
    function ParseCreateTableStmtDefinition(const AlterTableStmt: Boolean): TOffset; overload;
    function ParseCreateTableStmtDefinitionPartitionNames(): TOffset;
    function ParseCreateTableStmtForeignKey(const Add: Boolean = False): TOffset;
    function ParseCreateTableStmtKey(const AlterTableStmt: Boolean): TOffset;
    function ParseCreateTableStmtKeyColumn(): TOffset;
    function ParseCreateTableStmtPartition(): TOffset; overload;
    function ParseCreateTableStmtPartition(const Add: Boolean): TOffset; overload;
    function ParseCreateTableStmtPartitionValues(): TOffset;
    function ParseCreateTableStmtReference(): TOffset;
    function ParseCreateTableStmtUnion(): TOffset;
    function ParseCreateTriggerStmt(): TOffset;
    function ParseCreateUserStmt(const Alter: Boolean): TOffset;
    function ParseCreateViewStmt(): TOffset;
    function ParseCurrentTimestamp(): TOffset;
    function ParseDatabaseIdent(): TOffset; {$IFNDEF Debug} inline; {$ENDIF}
    function ParseDatatype(): TOffset;
    function ParseDbIdent(const ADbIdentType: TDbIdentType): TOffset;
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
    function ParseDropTableStmt(): TOffset;
    function ParseDropTriggerStmt(): TOffset;
    function ParseDropUserStmt(): TOffset;
    function ParseDropViewStmt(): TOffset;
    function ParseEventIdent(): TOffset;
    function ParseEndLabel(): TOffset;
    function ParseExecuteStmt(): TOffset;
    function ParseExistsFunc(): TOffset;
    function ParseExplainStmt(): TOffset;
    function ParseExpr(): TOffset; overload; {$IFNDEF Debug} inline; {$ENDIF}
    function ParseExpr(const InAllowed: Boolean): TOffset; overload;
    function ParseExtractFunc(): TOffset;
    function ParseFetchStmt(): TOffset;
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
    function ParseIfStmtBranch(): TOffset;
    function ParseIgnoreLines(): TOffset;
    function ParseInsertStmt(const Replace: Boolean = False): TOffset;
    function ParseInsertStmtSetItemsList(): TOffset;
    function ParseInsertStmtValuesList(): TOffset;
    function ParseInteger(): TOffset;
    function ParseIntervalOp(): TOffset;
    function ParseIntervalOpList(): TIntervalList;
    function ParseIntervalOpListItem(): TOffset;
    function ParseIterateStmt(): TOffset;
    function ParseKeyIdent(): TOffset; {$IFNDEF Debug} inline; {$ENDIF}
    function ParseKeyword(): TOffset;
    function ParseKillStmt(): TOffset;
    function ParseLeaveStmt(): TOffset;
    function ParseList(const Brackets: Boolean; const ParseElement: TParseFunction; const DelimterType: TTokenType = ttComma): TOffset; overload;
    function ParseLoadDataStmt(): TOffset;
    function ParseLoadStmt(): TOffset;
    function ParseLoadXMLStmt(): TOffset;
    function ParseLockStmt(): TOffset;
    function ParseLockStmtItem(): TOffset;
    function ParseLoopStmt(): TOffset;
    function ParseNumeric(): TOffset;
    function ParseMatchFunc(): TOffset;
    function ParseOpenStmt(): TOffset;
    function ParseOptimizeStmt(): TOffset;
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
    function ParseRepairStmt(): TOffset;
    function ParseRepeatStmt(): TOffset;
    function ParseResetStmt(): TOffset;
    function ParseReturnStmt(): TOffset;
    function ParseResetStmtOption(): TOffset;
    function ParseRevokeStmt(): TOffset;
    function ParseRollbackStmt(): TOffset;
    function ParseSavepointIdent(): TOffset;
    function ParseSavepointStmt(): TOffset;
    function ParseSchedule(): TOffset;
    function ParseSecretIdent(): TOffset;
    function ParseSelectStmt(): TOffset;
    function ParseSelectStmtColumn(): TOffset;
    function ParseSelectStmtGroup(): TOffset;
    function ParseSelectStmtOrder(): TOffset;
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
    function ParseShowAuthorsStmt(): TOffset;
    function ParseShowBinaryLogsStmt(): TOffset;
    function ParseShowBinlogEventsStmt(): TOffset;
    function ParseShowCharacterSetStmt(): TOffset;
    function ParseShowCollationStmt(): TOffset;
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
    function ParseSubAreaSelectStmt(): TOffset;
    function ParseSubPartition(): TOffset;
    function ParseSubstringFunc(): TOffset;
    function ParseStmt(): TOffset;
    function ParseTableIdent(): TOffset; {$IFNDEF Debug} inline; {$ENDIF}
    function ParseTag(const KeywordIndex1: TWordList.TIndex;
      const KeywordIndex2: TWordList.TIndex = -1; const KeywordIndex3: TWordList.TIndex = -1;
      const KeywordIndex4: TWordList.TIndex = -1; const KeywordIndex5: TWordList.TIndex = -1;
      const KeywordIndex6: TWordList.TIndex = -1): TOffset;
    function ParseToken(): TOffset;
    function ParseTriggerIdent(): TOffset;
    function ParseSetTransactionStmtCharacterisic(): TOffset;
    function ParseTrimFunc(): TOffset;
    function ParseTruncateTableStmt(): TOffset;
    function ParseUnknownStmt(): TOffset;
    function ParseUnlockStmt(): TOffset;
    function ParseUpdateStmt(): TOffset;
    function ParseUpdateStmtValue(): TOffset;
    function ParseUserIdent(): TOffset;
    function ParseUseStmt(): TOffset;
    function ParseValue(const KeywordIndex: TWordList.TIndex; const Assign: TValueAssign; const Brackets: Boolean; const ParseItem: TParseFunction): TOffset; overload;
    function ParseValue(const KeywordIndex: TWordList.TIndex; const Assign: TValueAssign; const OptionIndices: TWordList.TIndices): TOffset; overload;
    function ParseValue(const KeywordIndex: TWordList.TIndex; const Assign: TValueAssign; const ParseValueNode: TParseFunction): TOffset; overload;
    function ParseValue(const KeywordIndices: TWordList.TIndices; const Assign: TValueAssign; const Brackets: Boolean; const ParseItem: TParseFunction): TOffset; overload;
    function ParseValue(const KeywordIndices: TWordList.TIndices; const Assign: TValueAssign; const ParseValueNode: TParseFunction): TOffset; overload;
    function ParseValue(const KeywordIndices: TWordList.TIndices; const Assign: TValueAssign; const ValueKeywordIndex1: TWordList.TIndex; const ValueKeywordIndex2: TWordList.TIndex = -1): TOffset; overload;
    function ParseVariable(): TOffset;
    function ParseWeightStringFunc(): TOffset;
    function ParseWeightStringFuncLevel(): TOffset;
    function ParseWhileStmt(): TOffset;
    function ParseXAStmt(): TOffset;
    function RangePtr(const ANode: TOffset): PRange; {$IFNDEF Debug} inline; {$ENDIF}
    procedure SetError(const AErrorCode: Byte; const AErrorToken: TOffset = 0);
    function StmtPtr(const Node: TOffset): PStmt; {$IFNDEF Debug} inline; {$ENDIF}
    function TokenPtr(const Token: TOffset): PToken; {$IFNDEF Debug} inline; {$ENDIF}

  public
    ttIdents: set of TTokenType;
    ttStrings: set of TTokenType;

    procedure Clear();
    constructor Create(const AMySQLVersion: Integer = 0);
    destructor Destroy(); override;
    procedure FormatSQL(out SQL: string);
    function LoadFromFile(const Filename: string): Boolean;
    function ParseSQL(const Text: PChar; const Length: Integer): Boolean; overload;
    function ParseSQL(const Text: string): Boolean; overload; {$IFNDEF Debug} inline; {$ENDIF}
    procedure SaveToFile(const Filename: string; const FileType: TFileType = ftSQL);
    property AnsiQuotes: Boolean read FAnsiQuotes write FAnsiQuotes;
    property Datatypes: string read GetDatatypes write SetDatatypes;
    property ErrorCode: Byte read GetErrorCode;
    property ErrorMessage: string read GetErrorMessage;
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
  STooManyTokensInExpr = 'Too many tokens (%d) in Expr';
  SUnknownNodeType = 'Unknown node type';
  SOutOfMemory = 'Out of memory (%d)';

const
  CP_UNICODE = 1200;
  BOM_UTF8: PAnsiChar = Chr($EF) + Chr($BB) + Chr($BF);
  BOM_UNICODE_LE: PAnsiChar = Chr($FF) + Chr($FE);

  DefaultParsedNodesMemSize = 100 * 1024;
  DefaultTextsMemSize = 10 * 1024;

var
  UsageTypeByTokenType: array[TMySQLParser.TTokenType] of TMySQLParser.TUsageType = (
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
    utConst,
    utDbIdent,
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

function WordIndices(const Index0: TMySQLParser.TWordList.TIndex;
  const Index1: TMySQLParser.TWordList.TIndex = -1;
  const Index2: TMySQLParser.TWordList.TIndex = -1;
  const Index3: TMySQLParser.TWordList.TIndex = -1;
  const Index4: TMySQLParser.TWordList.TIndex = -1;
  const Index5: TMySQLParser.TWordList.TIndex = -1): TMySQLParser.TWordList.TIndices;
begin
  Result[0] := Index0;
  Result[1] := Index1;
  Result[2] := Index2;
  Result[3] := Index3;
  Result[4] := Index4;
  Result[5] := Index5;
end;

{ TMySQLParser.TStringBuffer **************************************************}

procedure TMySQLParser.TStringBuffer.Clear();
begin
  Buffer.Write := Buffer.Mem;
end;

constructor TMySQLParser.TStringBuffer.Create(const InitialLength: Integer);
begin
  Buffer.Mem := nil;
  Buffer.MemSize := 0;
  Buffer.Write := nil;

  Reallocate(InitialLength);
end;

procedure TMySQLParser.TStringBuffer.Delete(const Start: Integer; const Length: Integer);
begin
  Move(Buffer.Mem[Start + Length], Buffer.Mem[Start], Size - Length);
  Buffer.Write := Pointer(Integer(Buffer.Write) - Length);
end;

destructor TMySQLParser.TStringBuffer.Destroy();
begin
  FreeMem(Buffer.Mem);

  inherited;
end;

function TMySQLParser.TStringBuffer.GetData(): Pointer;
begin
  Result := Pointer(Buffer.Mem);
end;

function TMySQLParser.TStringBuffer.GetLength(): Integer;
begin
  Result := (Integer(Buffer.Write) - Integer(Buffer.Mem)) div SizeOf(Buffer.Mem[0]);
end;

function TMySQLParser.TStringBuffer.GetSize(): Integer;
begin
  Result := Integer(Buffer.Write) - Integer(Buffer.Mem);
end;

function TMySQLParser.TStringBuffer.GetText(): PChar;
begin
  Result := Buffer.Mem;
end;

function TMySQLParser.TStringBuffer.Read(): string;
begin
  SetString(Result, PChar(Buffer.Mem), Size div SizeOf(Result[1]));
end;

procedure TMySQLParser.TStringBuffer.Reallocate(const NeededLength: Integer);
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

procedure TMySQLParser.TStringBuffer.Write(const Text: PChar; const Length: Integer);
begin
  if (Length > 0) then
  begin
    Reallocate(Length);

    Move(Text^, Buffer.Write^, Length * SizeOf(Buffer.Mem[0]));
    Buffer.Write := @Buffer.Write[Length];
  end;
end;

procedure TMySQLParser.TStringBuffer.Write(const Text: string);
begin
  Write(PChar(Text), System.Length(Text));
end;

procedure TMySQLParser.TStringBuffer.Write(const Char: Char);
begin
  Write(@Char, 1);
end;

{ TMySQLParser.TWordList ******************************************************}

procedure TMySQLParser.TWordList.Clear();
begin
  FText := '';

  FCount := 0;
  SetLength(FIndex, 0);

  SetLength(Parser.OperatorTypeByKeywordIndex, 0);
end;

constructor TMySQLParser.TWordList.Create(const ASQLParser: TMySQLParser; const AText: string = '');
begin
  FParser := ASQLParser;

  FCount := 0;
  SetLength(FIndex, 0);

  Text := AText;
end;

destructor TMySQLParser.TWordList.Destroy();
begin
  Clear();

  inherited;
end;

function TMySQLParser.TWordList.GetText(): string;
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

function TMySQLParser.TWordList.GetWord(Index: TWordList.TIndex): string;
begin
  Result := StrPas(FIndex[Index]);
end;

function TMySQLParser.TWordList.IndexOf(const Word: PChar; const Length: Integer): Integer;
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

function TMySQLParser.TWordList.IndexOf(const Word: string): Integer;
begin
  Result := IndexOf(PChar(Word), Length(Word));
end;

procedure TMySQLParser.TWordList.SetText(AText: string);
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

{ TMySQLParser.TFormatHandle **************************************************}

constructor TMySQLParser.TFormatBuffer.Create();
var
  S: string;
begin
  inherited Create(1024);

  FNewLine := False;
  Indent := 0;
  S := StringOfChar(' ', System.Length(IndentSpaces));
  Move(S[1], IndentSpaces, SizeOf(IndentSpaces));
end;

procedure TMySQLParser.TFormatBuffer.DecreaseIndent();
begin
  Assert(Indent >= IndentSize);

  if (Indent >= IndentSize) then
    Dec(Indent, IndentSize);
end;

destructor TMySQLParser.TFormatBuffer.Destroy();
begin
  inherited;
end;

procedure TMySQLParser.TFormatBuffer.IncreaseIndent();
begin
  Inc(Indent, IndentSize);
end;

procedure TMySQLParser.TFormatBuffer.Write(const Text: PChar; const Length: Integer);
begin
  inherited;

  FNewLine := False;
end;

procedure TMySQLParser.TFormatBuffer.WriteReturn();
begin
  Write(#13#10);
  if (Indent > 0) then
    Write(@IndentSpaces[0], Indent);
  FNewLine := True;
end;

procedure TMySQLParser.TFormatBuffer.WriteSpace();
begin
  Write(' ');
end;

{ TMySQLParser.TNode **********************************************************}

class function TMySQLParser.TNode.Create(const AParser: TMySQLParser; const ANodeType: TNodeType): TOffset;
begin
  Result := AParser.NewNode(ANodeType);

  with PNode(AParser.NodePtr(Result))^ do
  begin
    FNodeType := ANodeType;
    FParser := AParser;
  end;
end;

function TMySQLParser.TNode.GetOffset(): TOffset;
begin
  Result := @Self - Parser.ParsedNodes.Mem;
end;

{ TMySQLParser.TChild *********************************************************}

class function TMySQLParser.TChild.Create(const AParser: TMySQLParser; const ANodeType: TNodeType): TOffset;
begin
  Result := TNode.Create(AParser, ANodeType);

  with PChild(AParser.NodePtr(Result))^ do
  begin
    FParentNode := 0;
  end;
end;

function TMySQLParser.TChild.GetDelimiter(): PToken;
var
  Token: PToken;
begin
  Result := nil;
  if ((ParentNode^.NodeType = ntList)
    and (PList(ParentNode)^.DelimiterType <> ttUnknown)) then
  begin
    Token := LastToken^.NextToken;

    if (Assigned(Token) and (Token^.TokenType = PList(ParentNode)^.DelimiterType)) then
      Result := Token;
  end;
end;

function TMySQLParser.TChild.GetFFirstToken(): TOffset;
begin
  if (NodeType = ntToken) then
    Result := @Self - Parser.ParsedNodes.Mem
  else
    Result := TMySQLParser.PRange(@Self)^.FFirstToken;
end;

function TMySQLParser.TChild.GetFirstToken(): PToken;
begin
  if (NodeType = ntToken) then
    Result := @Self
  else
    Result := PRange(@Self)^.FirstToken;
end;

function TMySQLParser.TChild.GetFLastToken(): TOffset;
begin
  if (NodeType = ntToken) then
    Result := PNode(@Self)^.Offset
  else
    Result := PRange(@Self)^.FLastToken;
end;

function TMySQLParser.TChild.GetLastToken(): PToken;
begin
  if (NodeType = ntToken) then
    Result := @Self
  else
    Result := PRange(@Self)^.LastToken;
end;

function TMySQLParser.TChild.GetNextSibling(): PChild;
var
  Child: PChild;
  Token: PToken;
begin
  Result := nil;
  if (ParentNode^.NodeType = ntList) then
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

function TMySQLParser.TChild.GetParentNode(): PNode;
begin
  Assert(FParentNode < Parser.ParsedNodes.UsedSize);

  Result := Parser.NodePtr(FParentNode);
end;

{ TMySQLParser.TToken *********************************************************}

class function TMySQLParser.TToken.Create(const AParser: TMySQLParser;
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

function TMySQLParser.TToken.GetAsString(): string;
var
  Length: Integer;
  I: Integer;
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
    ttDBIdent:
      if ((Copy(S, 1, 1) = '[') and (Copy(S, Length, 1) = ']')) then
        Result := Trim(Copy(S, 1, Length - 2))
      else
        Result := S;
    ttMySQLIdent:
      Result := SQLUnescape(S);
    ttMySQLCodeStart:
      Result := Copy(S, 1, Length - 3);
    ttCSString:
      begin
        I := Pos('''', S);
        if (I = 0) then
          Result := S
        else
          Result := SQLUnescape(Copy(S, I, Length - I));
      end;
    else
      Result := S;
  end;
end;

function TMySQLParser.TToken.GetDbIdentType(): TDbIdentType;
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

function TMySQLParser.TToken.GetGeneration(): Integer;
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
function TMySQLParser.TToken.GetIndex(): Integer;
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

function TMySQLParser.TToken.GetIsUsed(): Boolean;
begin
  Result := not (TokenType in [ttSpace, ttReturn, ttSLComment, ttMLComment, ttMySQLCodeStart, ttMySQLCodeEnd])
    and ((Parser.AllowedMySQLVersion = 0) or (Parser.MySQLVersion >= Parser.AllowedMySQLVersion));
end;

function TMySQLParser.TToken.GetNextToken(): PToken;
var
  Offset: TOffset;
begin
  Offset := PNode(@Self)^.Offset;
  repeat
    repeat
      Inc(Offset, Parser.NodeSize(Parser.NodePtr(Offset)^.NodeType));
    until ((Offset = Parser.ParsedNodes.UsedSize) or (Parser.NodePtr(Offset)^.NodeType = ntToken));
    if (Offset = Parser.ParsedNodes.UsedSize) then
      Result := nil
    else
      Result := PToken(Parser.NodePtr(Offset));
  until (not Assigned(Result) or Result^.IsUsed);
end;

function TMySQLParser.TToken.GetNextTokenAll(): PToken;
var
  Offset: TOffset;
begin
  Offset := PNode(@Self)^.Offset;
  repeat
    repeat
      Inc(Offset, Parser.NodeSize(Parser.NodePtr(Offset)^.NodeType));
    until ((Offset = Parser.ParsedNodes.UsedSize) or (Parser.NodePtr(Offset)^.NodeType = ntToken));
    if (Offset = Parser.ParsedNodes.UsedSize) then
      Result := nil
    else
      Result := PToken(Parser.NodePtr(Offset));
  until (not Assigned(Result) or Parser.IsToken(Offset));
end;

function TMySQLParser.TToken.GetOffset(): TOffset;
begin
  Result := Heritage.Heritage.GetOffset();
end;

function TMySQLParser.TToken.GetParentNode(): PNode;
begin
  Result := Heritage.GetParentNode();
end;

function TMySQLParser.TToken.GetText(): string;
var
  Text: PChar;
  Length: Integer;
begin
  GetText(Text, Length);
  SetString(Result, Text, Length);
end;

procedure TMySQLParser.TToken.GetText(out Text: PChar; out Length: Integer);
begin
  if (NewText = 0) then
  begin
    Text := FText;
    Length := FLength;
  end
  else
    Parser.GetText(NewText, Text, Length);
end;

procedure TMySQLParser.TToken.SetText(const Text: PChar; const Length: Integer);
begin
  NewText := Parser.NewText(Text, Length);
end;

procedure TMySQLParser.TToken.SetText(Text: string);
begin
  SetText(PChar(Text), System.Length(Text));
end;

function TMySQLParser.TToken.GetTokenType(): TTokenType;
begin
  if (NewTokenType = ttUnknown) then
    Result := FTokenType
  else
    Result := NewTokenType;
end;

procedure TMySQLParser.TToken.SetTokenType(ATokenType: TTokenType);
begin
  if (ATokenType <> FTokenType) then
    NewTokenType := ATokenType;
end;

{ TMySQLParser.TRange *********************************************************}

class function TMySQLParser.TRange.Create(const AParser: TMySQLParser; const ANodeType: TNodeType): TOffset;
begin
  Result := TChild.Create(AParser, ANodeType);

  with PRange(AParser.NodePtr(Result))^ do
  begin
    FFirstToken := 0;
    FLastToken := 0;
  end;
end;

function TMySQLParser.TRange.GetFirstToken(): PToken;
begin
  Assert(FFirstToken > 0);

  Result := PToken(Parser.NodePtr(FFirstToken));
end;

function TMySQLParser.TRange.GetLastToken(): PToken;
begin
  Assert(FLastToken > 0);

  Result := PToken(Parser.NodePtr(FLastToken));
end;

function TMySQLParser.TRange.GetOffset(): TOffset;
begin
  Result := Heritage.Heritage.Offset;
end;

function TMySQLParser.TRange.GetParentNode(): PNode;
begin
  Result := PNode(Parser.NodePtr(FParentNode));
end;

procedure TMySQLParser.TRange.AddChildren(const Children: POffsetArray; const Count: Integer);
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

{ TMySQLParser.TRoot **********************************************************}

class function TMySQLParser.TRoot.Create(const AParser: TMySQLParser;
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

function TMySQLParser.TRoot.GetErrorMessage(): string;
var
  Length: Integer;
  Text: PChar;
begin
  Parser.GetText(FErrorMessage, Text, Length);
  SetString(Result, Text, Length);
end;

function TMySQLParser.TRoot.GetFirstStmt(): PStmt;
begin
  Result := PStmt(Parser.NodePtr(FFirstStmt));
end;

function TMySQLParser.TRoot.GetFirstTokenAll(): PToken;
begin
  Result := PToken(Parser.NodePtr(FFirstTokenAll));
end;

function TMySQLParser.TRoot.GetLastStmt(): PStmt;
begin
  Result := PStmt(Parser.NodePtr(FLastStmt));
end;

function TMySQLParser.TRoot.GetLastTokenAll(): PToken;
begin
  Result := PToken(Parser.NodePtr(FLastTokenAll));
end;

{ TMySQLParser.TStmt **********************************************************}

class function TMySQLParser.TStmt.Create(const AParser: TMySQLParser; const AStmtType: TStmtType): TOffset;
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

function TMySQLParser.TStmt.GetDelimiter(): PToken;
var
  Token: PToken;
begin
  Result := nil;
  if (ParentNode^.NodeType = ntRoot) then
  begin
    Token := LastToken^.NextToken;

    if (Assigned(Token) and (Token^.TokenType = ttDelimiter)) then
      Result := Token;
  end;
end;

function TMySQLParser.TStmt.GetErrorMessage(): string;
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

function TMySQLParser.TStmt.GetErrorToken(): PToken;
begin
  Result := PToken(Parser.NodePtr(FErrorToken));
end;

function TMySQLParser.TStmt.GetFirstToken(): PToken;
begin
  Result := PToken(Parser.NodePtr(FFirstToken));
end;

function TMySQLParser.TStmt.GetFirstTokenAll(): PToken;
begin
  Assert(FFirstTokenAll > 0);

  Result := PToken(Parser.NodePtr(FFirstTokenAll));
end;

function TMySQLParser.TStmt.GetLastToken(): PToken;
begin
  Result := PRange(@Self)^.LastToken;
end;

function TMySQLParser.TStmt.GetLastTokenAll(): PToken;
begin
  Assert(FLastTokenAll > 0);

  Result := PToken(Parser.NodePtr(FLastTokenAll));
end;

function TMySQLParser.TStmt.GetNextStmt(): PStmt;
var
  Token: PToken;
  Child: PChild;
begin
  if (Heritage.FParentNode <> Parser.FRoot) then
    Result := nil
  else
  begin
    Token := LastTokenAll;
    while (Assigned(Token) and (Token^.TokenType <> ttDelimiter)) do
      Token := Token^.NextToken;
    while (Assigned(Token) and (Token^.TokenType = ttDelimiter)) do
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

function TMySQLParser.TStmt.GetText(): string;
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

function TMySQLParser.TStmt.GetParentNode(): PNode;
begin
  Result := PNode(Heritage.ParentNode);
end;

{ TMySQLParser.TAnalyzeStmt ***************************************************}

class function TMySQLParser.TAnalyzeStmt.Create(const AParser: TMySQLParser; const ANodes: TNodes): TOffset;
begin
  Result := TStmt.Create(AParser, stAnalyze);

  with PAnalyzeStmt(AParser.NodePtr(Result))^ do
  begin
    Nodes := ANodes;

    Heritage.Heritage.AddChildren(@Nodes, SizeOf(Nodes) div SizeOf(TOffset));
  end;
end;

{ TMySQLParser.TAlterDatabase *************************************************}

class function TMySQLParser.TAlterDatabaseStmt.Create(const AParser: TMySQLParser; const ANodes: TNodes): TOffset;
begin
  Result := TStmt.Create(AParser, stAlterDatabase);

  with PAlterDatabaseStmt(AParser.NodePtr(Result))^ do
  begin
    Nodes := ANodes;

    Heritage.Heritage.AddChildren(@Nodes, SizeOf(Nodes) div SizeOf(TOffset));
  end;
end;

{ TMySQLParser.TAlterEvent ****************************************************}

class function TMySQLParser.TAlterEventStmt.Create(const AParser: TMySQLParser; const ANodes: TNodes): TOffset;
begin
  Result := TStmt.Create(AParser, stAlterEvent);

  with PAlterEventStmt(AParser.NodePtr(Result))^ do
  begin
    Nodes := ANodes;

    Heritage.Heritage.AddChildren(@Nodes, SizeOf(Nodes) div SizeOf(TOffset));
  end;
end;

{ TMySQLParser.TAlterRoutine **************************************************}

class function TMySQLParser.TAlterInstanceStmt.Create(const AParser: TMySQLParser; const ANodes: TNodes): TOffset;
begin
  Result := TStmt.Create(AParser, stAlterInstance);

  with PAlterInstanceStmt(AParser.NodePtr(Result))^ do
  begin
    Nodes := ANodes;

    Heritage.Heritage.AddChildren(@Nodes, SizeOf(Nodes) div SizeOf(TOffset));
  end;
end;

{ TMySQLParser.TAlterRoutine **************************************************}

class function TMySQLParser.TAlterRoutineStmt.Create(const AParser: TMySQLParser; const ARoutineType: TRoutineType; const ANodes: TNodes): TOffset;
begin
  Result := TStmt.Create(AParser, stAlterRoutine);

  with PAlterRoutineStmt(AParser.NodePtr(Result))^ do
  begin
    Nodes := ANodes;
    FRoutineType := ARoutineType;

    Heritage.Heritage.AddChildren(@Nodes, SizeOf(Nodes) div SizeOf(TOffset));
  end;
end;

{ TMySQLParser.TAlterServerStmt ***********************************************}

class function TMySQLParser.TAlterServerStmt.Create(const AParser: TMySQLParser; const ANodes: TNodes): TOffset;
begin
  Result := TStmt.Create(AParser, stAlterServer);

  with PAlterServerStmt(AParser.NodePtr(Result))^ do
  begin
    Nodes := ANodes;

    Heritage.Heritage.AddChildren(@Nodes, SizeOf(Nodes) div SizeOf(TOffset));
  end;
end;

{ TMySQLParser.TAlterTableStmt.TAlterColumn ***********************************}

class function TMySQLParser.TAlterTableStmt.TAlterColumn.Create(const AParser: TMySQLParser; const ANodes: TAlterColumn.TNodes): TOffset;
begin
  Result := TRange.Create(AParser, ntAlterTableStmtAlterColumn);

  with PAlterColumn(AParser.NodePtr(Result))^ do
  begin
    Nodes := ANodes;

    Heritage.AddChildren(@Nodes, SizeOf(Nodes) div SizeOf(TOffset));
  end;
end;

{ TMySQLParser.TAlterTableStmt.TConvertTo *************************************}

class function TMySQLParser.TAlterTableStmt.TConvertTo.Create(const AParser: TMySQLParser; const ANodes: TConvertTo.TNodes): TOffset;
begin
  Result := TRange.Create(AParser, ntAlterTableStmtConvertTo);

  with PConvertTo(AParser.NodePtr(Result))^ do
  begin
    Nodes := ANodes;

    Heritage.AddChildren(@Nodes, SizeOf(Nodes) div SizeOf(TOffset));
  end;
end;

{ TMySQLParser.TAlterTableStmt.TDropObject ************************************}

class function TMySQLParser.TAlterTableStmt.TDropObject.Create(const AParser: TMySQLParser; const ANodes: TDropObject.TNodes): TOffset;
begin
  Result := TRange.Create(AParser, ntAlterTableStmtDropObject);

  with PDropObject(AParser.NodePtr(Result))^ do
  begin
    Nodes := ANodes;

    Heritage.AddChildren(@Nodes, SizeOf(Nodes) div SizeOf(TOffset));
  end;
end;

{ TMySQLParser.TAlterTableStmt.TExchangePartition *****************************}

class function TMySQLParser.TAlterTableStmt.TExchangePartition.Create(const AParser: TMySQLParser; const ANodes: TExchangePartition.TNodes): TOffset;
begin
  Result := TRange.Create(AParser, ntAlterTableStmtExchangePartition);

  with PExchangePartition(AParser.NodePtr(Result))^ do
  begin
    Nodes := ANodes;

    Heritage.AddChildren(@Nodes, SizeOf(Nodes) div SizeOf(TOffset));
  end;
end;

{ TMySQLParser.TAlterTableStmt.TReorganizePartition ***************************}

class function TMySQLParser.TAlterTableStmt.TReorganizePartition.Create(const AParser: TMySQLParser; const ANodes: TReorganizePartition.TNodes): TOffset;
begin
  Result := TRange.Create(AParser, ntAlterTableStmtReorganizePartition);

  with PReorganizePartition(AParser.NodePtr(Result))^ do
  begin
    Nodes := ANodes;

    Heritage.AddChildren(@Nodes, SizeOf(Nodes) div SizeOf(TOffset));
  end;
end;

{ TMySQLParser.TAlterTableStmt ************************************************}

class function TMySQLParser.TAlterTableStmt.Create(const AParser: TMySQLParser; const ANodes: TNodes): TOffset;
begin
  Result := TStmt.Create(AParser, stAlterTable);

  with PAlterTableStmt(AParser.NodePtr(Result))^ do
  begin
    Nodes := ANodes;

    Heritage.Heritage.AddChildren(@Nodes, SizeOf(Nodes) div SizeOf(TOffset));
  end;
end;

{ TMySQLParser.TAlterViewStmt *************************************************}

class function TMySQLParser.TAlterViewStmt.Create(const AParser: TMySQLParser; const ANodes: TNodes): TOffset;
begin
  Result := TStmt.Create(AParser, stAlterView);

  with PAlterViewStmt(AParser.NodePtr(Result))^ do
  begin
    Nodes := ANodes;

    Heritage.Heritage.AddChildren(@Nodes, SizeOf(Nodes) div SizeOf(TOffset));
  end;
end;

{ TMySQLParser.TBeginLabel ****************************************************}

class function TMySQLParser.TBeginLabel.Create(const AParser: TMySQLParser; const ANodes: TNodes): TOffset;
begin
  Result := TRange.Create(AParser, ntBeginLabel);

  with PBeginLabel(AParser.NodePtr(Result))^ do
  begin
    Nodes := ANodes;

    Heritage.AddChildren(@Nodes, SizeOf(Nodes) div SizeOf(TOffset));
  end;
end;

{ TMySQLParser.TBeginStmt *****************************************************}

class function TMySQLParser.TBeginStmt.Create(const AParser: TMySQLParser; const ANodes: TNodes): TOffset;
begin
  Result := TStmt.Create(AParser, stBegin);

  with PBeginStmt(AParser.NodePtr(Result))^ do
  begin
    Nodes := ANodes;

    Heritage.Heritage.AddChildren(@Nodes, SizeOf(Nodes) div SizeOf(TOffset));
  end;
end;

{ TMySQLParser.TBinaryOp ******************************************************}

class function TMySQLParser.TBinaryOp.Create(const AParser: TMySQLParser; const AOperator, AOperand1, AOperand2: TOffset): TOffset;
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

function TMySQLParser.TBinaryOp.GetOperand1(): PChild;
begin
  Result := Parser.ChildPtr(Nodes.Operand1);
end;

function TMySQLParser.TBinaryOp.GetOperand2(): PChild;
begin
  Result := Parser.ChildPtr(Nodes.Operand2);
end;

function TMySQLParser.TBinaryOp.GetOperator(): PChild;
begin
  Result := Parser.ChildPtr(Nodes.OperatorToken);
end;

{ TMySQLParser.TBetweenOp *****************************************************}

class function TMySQLParser.TBetweenOp.Create(const AParser: TMySQLParser; const AExpr, ANotToken, ABetweenToken, AMin, AAndToken, AMax: TOffset): TOffset;
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

{ TMySQLParser.TCallStmt ******************************************************}

class function TMySQLParser.TCallStmt.Create(const AParser: TMySQLParser; const ANodes: TNodes): TOffset;
begin
  Result := TStmt.Create(AParser, stCall);

  with PCallStmt(AParser.NodePtr(Result))^ do
  begin
    Nodes := ANodes;

    Heritage.Heritage.AddChildren(@Nodes, SizeOf(Nodes) div SizeOf(TOffset));
  end;
end;

{ TMySQLParser.TCaseOp.TBranch ************************************************}

class function TMySQLParser.TCaseOp.TBranch.Create(const AParser: TMySQLParser; const ANodes: TBranch.TNodes): TOffset;
begin
  Result := TRange.Create(AParser, ntCaseOpBranch);

  with PBranch(AParser.NodePtr(Result))^ do
  begin
    Nodes := ANodes;

    Heritage.AddChildren(@Nodes, SizeOf(Nodes) div SizeOf(TOffset));
  end;
end;

{ TMySQLParser.TCaseOp ********************************************************}

class function TMySQLParser.TCaseOp.Create(const AParser: TMySQLParser; const ANodes: TNodes): TOffset;
begin
  Result := TRange.Create(AParser, ntCaseOp);

  with PCaseOp(AParser.NodePtr(Result))^ do
  begin
    Nodes := ANodes;

    Heritage.AddChildren(@Nodes, SizeOf(Nodes) div SizeOf(TOffset));
  end;
end;

{ TMySQLParser.TCaseStmt ******************************************************}

class function TMySQLParser.TCaseStmt.Create(const AParser: TMySQLParser; const ANodes: TNodes): TOffset;
begin
  Result := TStmt.Create(AParser, stCase);

  with PCaseStmt(AParser.NodePtr(Result))^ do
  begin
    Nodes := ANodes;

    Heritage.Heritage.AddChildren(@Nodes, SizeOf(Nodes) div SizeOf(TOffset));
  end;
end;

{ TMySQLParser.TCaseStmt.TBranch **********************************************}

class function TMySQLParser.TCaseStmt.TBranch.Create(const AParser: TMySQLParser; const ANodes: TBranch.TNodes): TOffset;
begin
  Result := TRange.Create(AParser, ntCaseStmtBranch);

  with PBranch(AParser.NodePtr(Result))^ do
  begin
    Nodes := ANodes;

    Heritage.AddChildren(@Nodes, SizeOf(Nodes) div SizeOf(TOffset));
  end;
end;

{ TMySQLParser.TCastFunc ******************************************************}

class function TMySQLParser.TCastFunc.Create(const AParser: TMySQLParser; const ANodes: TNodes): TOffset;
begin
  Result := TRange.Create(AParser, ntCastFunc);

  with PCastFunc(AParser.NodePtr(Result))^ do
  begin
    Nodes := ANodes;

    Heritage.AddChildren(@Nodes, SizeOf(Nodes) div SizeOf(TOffset));
  end;
end;

{ TMySQLParser.TCharFunc ******************************************************}

class function TMySQLParser.TCharFunc.Create(const AParser: TMySQLParser; const ANodes: TNodes): TOffset;
begin
  Result := TRange.Create(AParser, ntCharFunc);

  with PCharFunc(AParser.NodePtr(Result))^ do
  begin
    Nodes := ANodes;

    Heritage.AddChildren(@Nodes, SizeOf(Nodes) div SizeOf(TOffset));
  end;
end;

{ TMySQLParser.TCheckStmt *****************************************************}

class function TMySQLParser.TCheckStmt.Create(const AParser: TMySQLParser; const ANodes: TNodes): TOffset;
begin
  Result := TStmt.Create(AParser, stCheck);

  with PCheckStmt(AParser.NodePtr(Result))^ do
  begin
    Nodes := ANodes;

    Heritage.Heritage.AddChildren(@Nodes, SizeOf(Nodes) div SizeOf(TOffset));
  end;
end;

{ TMySQLParser.TCheckStmt.TOption *********************************************}

class function TMySQLParser.TCheckStmt.TOption.Create(const AParser: TMySQLParser; const ANodes: TOption.TNodes): TOffset;
begin
  Result := TRange.Create(AParser, ntCheckStmtOption);

  with POption(AParser.NodePtr(Result))^ do
  begin
    Nodes := ANodes;

    Heritage.AddChildren(@Nodes, SizeOf(Nodes) div SizeOf(TOffset));
  end;
end;

{ TMySQLParser.TChecksumStmt **************************************************}

class function TMySQLParser.TChecksumStmt.Create(const AParser: TMySQLParser; const ANodes: TNodes): TOffset;
begin
  Result := TStmt.Create(AParser, stChecksum);

  with PChecksumStmt(AParser.NodePtr(Result))^ do
  begin
    Nodes := ANodes;

    Heritage.Heritage.AddChildren(@Nodes, SizeOf(Nodes) div SizeOf(TOffset));
  end;
end;

{ TMySQLParser.TCloseStmt *****************************************************}

class function TMySQLParser.TCloseStmt.Create(const AParser: TMySQLParser; const ANodes: TNodes): TOffset;
begin
  Result := TStmt.Create(AParser, stClose);

  with PCloseStmt(AParser.NodePtr(Result))^ do
  begin
    Nodes := ANodes;

    Heritage.Heritage.AddChildren(@Nodes, SizeOf(Nodes) div SizeOf(TOffset));
  end;
end;

{ TMySQLParser.TCommitStmt ****************************************************}

class function TMySQLParser.TCommitStmt.Create(const AParser: TMySQLParser; const ANodes: TNodes): TOffset;
begin
  Result := TStmt.Create(AParser, stCommit);

  with PCommitStmt(AParser.NodePtr(Result))^ do
  begin
    Nodes := ANodes;

    Heritage.Heritage.AddChildren(@Nodes, SizeOf(Nodes) div SizeOf(TOffset));
  end;
end;

{ TMySQLParser.TCompoundStmt **************************************************}

class function TMySQLParser.TCompoundStmt.Create(const AParser: TMySQLParser; const ANodes: TNodes): TOffset;
begin
  Result := TStmt.Create(AParser, stCompound);

  with PCompoundStmt(AParser.NodePtr(Result))^ do
  begin
    Nodes := ANodes;

    Heritage.Heritage.AddChildren(@Nodes, SizeOf(Nodes) div SizeOf(TOffset));
  end;
end;

{ TMySQLParser.TConvertFunc ***************************************************}

class function TMySQLParser.TConvertFunc.Create(const AParser: TMySQLParser; const ANodes: TNodes): TOffset;
begin
  Result := TRange.Create(AParser, ntConvertFunc);

  with PConvertFunc(AParser.NodePtr(Result))^ do
  begin
    Nodes := ANodes;

    Heritage.AddChildren(@Nodes, SizeOf(Nodes) div SizeOf(TOffset));
  end;
end;

{ TMySQLParser.TCreateDatabaseStmt ********************************************}

class function TMySQLParser.TCreateDatabaseStmt.Create(const AParser: TMySQLParser; const ANodes: TNodes): TOffset;
begin
  Result := TStmt.Create(AParser, stCreateDatabase);

  with PCreateDatabaseStmt(AParser.NodePtr(Result))^ do
  begin
    Nodes := ANodes;

    Heritage.Heritage.AddChildren(@Nodes, SizeOf(Nodes) div SizeOf(TOffset));
  end;
end;

{ TMySQLParser.TCreateEventStmt ***********************************************}

class function TMySQLParser.TCreateEventStmt.Create(const AParser: TMySQLParser; const ANodes: TNodes): TOffset;
begin
  Result := TStmt.Create(AParser, stCreateEvent);

  with PCreateEventStmt(AParser.NodePtr(Result))^ do
  begin
    Nodes := ANodes;

    Heritage.Heritage.AddChildren(@Nodes, SizeOf(Nodes) div SizeOf(TOffset));
  end;
end;

{ TMySQLParser.TCreateIndexStmt ***********************************************}

class function TMySQLParser.TCreateIndexStmt.Create(const AParser: TMySQLParser; const ANodes: TNodes): TOffset;
begin
  Result := TStmt.Create(AParser, stCreateIndex);

  with PCreateIndexStmt(AParser.NodePtr(Result))^ do
  begin
    Nodes := ANodes;

    Heritage.Heritage.AddChildren(@Nodes, SizeOf(Nodes) div SizeOf(TOffset));
  end;
end;

{ TMySQLParser.TCreateRoutineStmt *********************************************}

class function TMySQLParser.TCreateRoutineStmt.Create(const AParser: TMySQLParser; const ARoutineType: TRoutineType; const ANodes: TNodes): TOffset;
begin
  Result := TStmt.Create(AParser, stCreateRoutine);

  with PCreateRoutineStmt(AParser.NodePtr(Result))^ do
  begin
    Nodes := ANodes;
    FRoutineType := ARoutineType;

    Heritage.Heritage.AddChildren(@Nodes, SizeOf(Nodes) div SizeOf(TOffset));
  end;
end;

{ TMySQLParser.TCreateServerStmt **********************************************}

class function TMySQLParser.TCreateServerStmt.Create(const AParser: TMySQLParser; const ANodes: TNodes): TOffset;
begin
  Result := TStmt.Create(AParser, stCreateServer);

  with PCreateServerStmt(AParser.NodePtr(Result))^ do
  begin
    Nodes := ANodes;

    Heritage.Heritage.AddChildren(@Nodes, SizeOf(Nodes) div SizeOf(TOffset));
  end;
end;

{ TMySQLParser.TCreateTableStmt ***********************************************}

class function TMySQLParser.TCreateTableStmt.Create(const AParser: TMySQLParser; const ANodes: TNodes): TOffset;
begin
  Result := TStmt.Create(AParser, stCreateTable);

  with PCreateTableStmt(AParser.NodePtr(Result))^ do
  begin
    Nodes := ANodes;

    Heritage.Heritage.AddChildren(@Nodes, SizeOf(Nodes) div SizeOf(TOffset));
  end;
end;

{ TMySQLParser.TCreateTableStmt.TField.TDefaultFunc ***************************}

class function TMySQLParser.TCreateTableStmt.TField.TDefaultFunc.Create(const AParser: TMySQLParser; const ANodes: TDefaultFunc.TNodes): TOffset;
begin
  Result := TRange.Create(AParser, ntCreateTableStmtFieldDefaultFunc);

  with PDefaultFunc(AParser.NodePtr(Result))^ do
  begin
    Nodes := ANodes;

    Heritage.AddChildren(@Nodes, SizeOf(Nodes) div SizeOf(TOffset));
  end;
end;

{ TMySQLParser.TCreateTableStmt.TField ****************************************}

class function TMySQLParser.TCreateTableStmt.TField.Create(const AParser: TMySQLParser; const ANodes: TField.TNodes): TOffset;
begin
  Result := TRange.Create(AParser, ntCreateTableStmtField);

  with PField(AParser.NodePtr(Result))^ do
  begin
    Nodes := ANodes;

    Heritage.AddChildren(@Nodes, SizeOf(Nodes) div SizeOf(TOffset));
  end;
end;

{ TMySQLParser.TCreateTableStmt.TForeignKey ***********************************}

class function TMySQLParser.TCreateTableStmt.TForeignKey.Create(const AParser: TMySQLParser; const ANodes: TForeignKey.TNodes): TOffset;
begin
  Result := TRange.Create(AParser, ntCreateTableStmtForeignKey);

  with PForeignKey(AParser.NodePtr(Result))^ do
  begin
    Nodes := ANodes;

    Heritage.AddChildren(@Nodes, SizeOf(Nodes) div SizeOf(TOffset));
  end;
end;

{ TMySQLParser.TCreateTableStmt.TKey ******************************************}

class function TMySQLParser.TCreateTableStmt.TKey.Create(const AParser: TMySQLParser; const ANodes: TKey.TNodes): TOffset;
begin
  Result := TRange.Create(AParser, ntCreateTableStmtKey);

  with PKey(AParser.NodePtr(Result))^ do
  begin
    Nodes := ANodes;

    Heritage.AddChildren(@Nodes, SizeOf(Nodes) div SizeOf(TOffset));
  end;
end;

{ TMySQLParser.TCreateTableStmt.TKeyColumn ************************************}

class function TMySQLParser.TCreateTableStmt.TKeyColumn.Create(const AParser: TMySQLParser; const ANodes: TKeyColumn.TNodes): TOffset;
begin
  Result := TRange.Create(AParser, ntCreateTableStmtKeyColumn);

  with PKeyColumn(AParser.NodePtr(Result))^ do
  begin
    Nodes := ANodes;

    Heritage.AddChildren(@Nodes, SizeOf(Nodes) div SizeOf(TOffset));
  end;
end;

{ TMySQLParser.TCreateTableStmt.TPartition ************************************}

class function TMySQLParser.TCreateTableStmt.TPartition.Create(const AParser: TMySQLParser; const ANodes: TPartition.TNodes): TOffset;
begin
  Result := TRange.Create(AParser, ntCreateTableStmtPartition);

  with PPartition(AParser.NodePtr(Result))^ do
  begin
    Nodes := ANodes;

    Heritage.AddChildren(@Nodes, SizeOf(Nodes) div SizeOf(TOffset));
  end;
end;

{ TMySQLParser.TCreateTableStmt.TPartitionValues ******************************}

class function TMySQLParser.TCreateTableStmt.TPartitionValues.Create(const AParser: TMySQLParser; const ANodes: TPartitionValues.TNodes): TOffset;
begin
  Result := TRange.Create(AParser, ntCreateTableStmtPartitionValues);

  with PPartitionValues(AParser.NodePtr(Result))^ do
  begin
    Nodes := ANodes;

    Heritage.AddChildren(@Nodes, SizeOf(Nodes) div SizeOf(TOffset));
  end;
end;

{ TMySQLParser.TCreateTableStmt.TReferences ******************************}

class function TMySQLParser.TCreateTableStmt.TReference.Create(const AParser: TMySQLParser; const ANodes: TReference.TNodes): TOffset;
begin
  Result := TRange.Create(AParser, ntCreateTableStmtReference);

  with PReference(AParser.NodePtr(Result))^ do
  begin
    Nodes := ANodes;

    Heritage.AddChildren(@Nodes, SizeOf(Nodes) div SizeOf(TOffset));
  end;
end;

{ TMySQLParser.TCreateTriggerStmt *********************************************}

class function TMySQLParser.TCreateTriggerStmt.Create(const AParser: TMySQLParser; const ANodes: TNodes): TOffset;
begin
  Result := TStmt.Create(AParser, stCreateTrigger);

  with PCreateTriggerStmt(AParser.NodePtr(Result))^ do
  begin
    Nodes := ANodes;

    Heritage.Heritage.AddChildren(@Nodes, SizeOf(Nodes) div SizeOf(TOffset));
  end;
end;

{ TMySQLParser.TCreateUserStmt ************************************************}

class function TMySQLParser.TCreateUserStmt.Create(const AParser: TMySQLParser; const ANodes: TNodes): TOffset;
begin
  Result := TStmt.Create(AParser, stCreateUser);

  with PCreateUserStmt(AParser.NodePtr(Result))^ do
  begin
    Nodes := ANodes;

    Heritage.Heritage.AddChildren(@Nodes, SizeOf(Nodes) div SizeOf(TOffset));
  end;
end;

{ TMySQLParser.TCreateViewStmt ************************************************}

class function TMySQLParser.TCreateViewStmt.Create(const AParser: TMySQLParser; const ANodes: TNodes): TOffset;
begin
  Result := TStmt.Create(AParser, stCreateView);

  with PCreateViewStmt(AParser.NodePtr(Result))^ do
  begin
    Nodes := ANodes;

    Heritage.Heritage.AddChildren(@Nodes, SizeOf(Nodes) div SizeOf(TOffset));
  end;
end;

{ TMySQLParser.TCurrentTimestamp **********************************************}

class function TMySQLParser.TCurrentTimestamp.Create(const AParser: TMySQLParser; const ANodes: TNodes): TOffset;
begin
  Result := TRange.Create(AParser, ntCurrentTimestamp);

  with PCurrentTimestamp(AParser.NodePtr(Result))^ do
  begin
    Nodes := ANodes;

    Heritage.AddChildren(@Nodes, SizeOf(Nodes) div SizeOf(TOffset));
  end;
end;

{ TMySQLParser.TDatatype ******************************************************}

class function TMySQLParser.TDatatype.Create(const AParser: TMySQLParser; const ANodes: TNodes): TOffset;
begin
  Result := TRange.Create(AParser, ntDatatype);

  with PDatatype(AParser.NodePtr(Result))^ do
  begin
    Nodes := ANodes;

    Heritage.AddChildren(@Nodes, SizeOf(Nodes) div SizeOf(TOffset));
  end;
end;

{ TMySQLParser.TDbIdent *******************************************************}

class function TMySQLParser.TDbIdent.Create(const AParser: TMySQLParser;
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

class function TMySQLParser.TDbIdent.Create(const AParser: TMySQLParser; const ADbIdentType: TDbIdentType;
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

function TMySQLParser.TDbIdent.GetDatabaseIdent(): PToken;
begin
  Result := Parser.TokenPtr(Nodes.DatabaseIdent);
end;

function TMySQLParser.TDbIdent.GetIdent(): PToken;
begin
  Result := Parser.TokenPtr(Nodes.Ident);
end;

function TMySQLParser.TDbIdent.GetParentNode(): PNode;
begin
  Result := Heritage.GetParentNode();
end;

function TMySQLParser.TDbIdent.GetTableIdent(): PToken;
begin
  Result := Parser.TokenPtr(Nodes.TableIdent);
end;

{ TMySQLParser.TDeallocatePrepareStmt *****************************************}

class function TMySQLParser.TDeallocatePrepareStmt.Create(const AParser: TMySQLParser; const ANodes: TNodes): TOffset;
begin
  Result := TStmt.Create(AParser, stDeallocatePrepare);

  with PDeallocatePrepareStmt(AParser.NodePtr(Result))^ do
  begin
    Nodes := ANodes;

    Heritage.Heritage.AddChildren(@Nodes, SizeOf(Nodes) div SizeOf(TOffset));
  end;
end;

{ TMySQLParser.TDeclareStmt ***************************************************}

class function TMySQLParser.TDeclareStmt.Create(const AParser: TMySQLParser; const ANodes: TNodes): TOffset;
begin
  Result := TStmt.Create(AParser, stDeclare);

  with PDeclareStmt(AParser.NodePtr(Result))^ do
  begin
    Nodes := ANodes;

    Heritage.Heritage.AddChildren(@Nodes, SizeOf(Nodes) div SizeOf(TOffset));
  end;
end;

{ TMySQLParser.TDeclareConditionStmt ******************************************}

class function TMySQLParser.TDeclareConditionStmt.Create(const AParser: TMySQLParser; const ANodes: TNodes): TOffset;
begin
  Result := TStmt.Create(AParser, stDeclareCondition);

  with PDeclareConditionStmt(AParser.NodePtr(Result))^ do
  begin
    Nodes := ANodes;

    Heritage.Heritage.AddChildren(@Nodes, SizeOf(Nodes) div SizeOf(TOffset));
  end;
end;

{ TMySQLParser.TDeclareCursorStmt *********************************************}

class function TMySQLParser.TDeclareCursorStmt.Create(const AParser: TMySQLParser; const ANodes: TNodes): TOffset;
begin
  Result := TStmt.Create(AParser, stDeclareCursor);

  with PDeclareCursorStmt(AParser.NodePtr(Result))^ do
  begin
    Nodes := ANodes;

    Heritage.Heritage.AddChildren(@Nodes, SizeOf(Nodes) div SizeOf(TOffset));
  end;
end;

{ TMySQLParser.TDeclareHandlerStmt ********************************************}

class function TMySQLParser.TDeclareHandlerStmt.Create(const AParser: TMySQLParser; const ANodes: TNodes): TOffset;
begin
  Result := TStmt.Create(AParser, stDeclareHandler);

  with PDeclareHandlerStmt(AParser.NodePtr(Result))^ do
  begin
    Nodes := ANodes;

    Heritage.Heritage.AddChildren(@Nodes, SizeOf(Nodes) div SizeOf(TOffset));
  end;
end;

{ TMySQLParser.TDeclareHandlerStmtCondition ***********************************}

class function TMySQLParser.TDeclareHandlerStmt.TCondition.Create(const AParser: TMySQLParser; const ANodes: TCondition.TNodes): TOffset;
begin
  Result := TRange.Create(AParser, ntDeclareHandlerStmtCondition);

  with PCondition(AParser.NodePtr(Result))^ do
  begin
    Nodes := ANodes;

    Heritage.AddChildren(@Nodes, SizeOf(Nodes) div SizeOf(TOffset));
  end;
end;

{ TMySQLParser.TDeleteStmt ****************************************************}

class function TMySQLParser.TDeleteStmt.Create(const AParser: TMySQLParser; const ANodes: TNodes): TOffset;
begin
  Result := TStmt.Create(AParser, stDelete);

  with PDeleteStmt(AParser.NodePtr(Result))^ do
  begin
    Nodes := ANodes;

    Heritage.Heritage.AddChildren(@Nodes, SizeOf(Nodes) div SizeOf(TOffset));
  end;
end;

{ TMySQLParser.TDoStmt ********************************************************}

class function TMySQLParser.TDoStmt.Create(const AParser: TMySQLParser; const ANodes: TNodes): TOffset;
begin
  Result := TStmt.Create(AParser, stDo);

  with PDoStmt(AParser.NodePtr(Result))^ do
  begin
    Nodes := ANodes;

    Heritage.Heritage.AddChildren(@Nodes, SizeOf(Nodes) div SizeOf(TOffset));
  end;
end;

{ TMySQLParser.TDropDatabaseStmt **********************************************}

class function TMySQLParser.TDropDatabaseStmt.Create(const AParser: TMySQLParser; const ANodes: TNodes): TOffset;
begin
  Result := TStmt.Create(AParser, stDropDatabase);

  with PDropDatabaseStmt(AParser.NodePtr(Result))^ do
  begin
    Nodes := ANodes;

    Heritage.Heritage.AddChildren(@Nodes, SizeOf(Nodes) div SizeOf(TOffset));
  end;
end;

{ TMySQLParser.TDropEventStmt *************************************************}

class function TMySQLParser.TDropEventStmt.Create(const AParser: TMySQLParser; const ANodes: TNodes): TOffset;
begin
  Result := TStmt.Create(AParser, stDropEvent);

  with PDropEventStmt(AParser.NodePtr(Result))^ do
  begin
    Nodes := ANodes;

    Heritage.Heritage.AddChildren(@Nodes, SizeOf(Nodes) div SizeOf(TOffset));
  end;
end;

{ TMySQLParser.TDropIndexStmt *************************************************}

class function TMySQLParser.TDropIndexStmt.Create(const AParser: TMySQLParser; const ANodes: TNodes): TOffset;
begin
  Result := TStmt.Create(AParser, stDropIndex);

  with PDropIndexStmt(AParser.NodePtr(Result))^ do
  begin
    Nodes := ANodes;

    Heritage.Heritage.AddChildren(@Nodes, SizeOf(Nodes) div SizeOf(TOffset));
  end;
end;

{ TMySQLParser.TDropRoutineStmt ***********************************************}

class function TMySQLParser.TDropRoutineStmt.Create(const AParser: TMySQLParser; const ARoutineType: TRoutineType; const ANodes: TNodes): TOffset;
begin
  Result := TStmt.Create(AParser, stDropRoutine);

  with PDropRoutineStmt(AParser.NodePtr(Result))^ do
  begin
    Nodes := ANodes;
    FRoutineType := ARoutineType;

    Heritage.Heritage.AddChildren(@Nodes, SizeOf(Nodes) div SizeOf(TOffset));
  end;
end;

{ TMySQLParser.TDropServerStmt ************************************************}

class function TMySQLParser.TDropServerStmt.Create(const AParser: TMySQLParser; const ANodes: TNodes): TOffset;
begin
  Result := TStmt.Create(AParser, stDropServer);

  with PDropServerStmt(AParser.NodePtr(Result))^ do
  begin
    Nodes := ANodes;

    Heritage.Heritage.AddChildren(@Nodes, SizeOf(Nodes) div SizeOf(TOffset));
  end;
end;

{ TMySQLParser.TDropTableStmt *************************************************}

class function TMySQLParser.TDropTableStmt.Create(const AParser: TMySQLParser; const ANodes: TNodes): TOffset;
begin
  Result := TStmt.Create(AParser, stDropTable);

  with PDropTableStmt(AParser.NodePtr(Result))^ do
  begin
    Nodes := ANodes;

    Heritage.Heritage.AddChildren(@Nodes, SizeOf(Nodes) div SizeOf(TOffset));
  end;
end;

{ TMySQLParser.TDropTriggerStmt ***********************************************}

class function TMySQLParser.TDropTriggerStmt.Create(const AParser: TMySQLParser; const ANodes: TNodes): TOffset;
begin
  Result := TStmt.Create(AParser, stDropTrigger);

  with PDropTriggerStmt(AParser.NodePtr(Result))^ do
  begin
    Nodes := ANodes;

    Heritage.Heritage.AddChildren(@Nodes, SizeOf(Nodes) div SizeOf(TOffset));
  end;
end;

{ TMySQLParser.TDropUserStmt **************************************************}

class function TMySQLParser.TDropUserStmt.Create(const AParser: TMySQLParser; const ANodes: TNodes): TOffset;
begin
  Result := TStmt.Create(AParser, stDropUser);

  with PDropUserStmt(AParser.NodePtr(Result))^ do
  begin
    Nodes := ANodes;

    Heritage.Heritage.AddChildren(@Nodes, SizeOf(Nodes) div SizeOf(TOffset));
  end;
end;

{ TMySQLParser.TDropViewStmt **************************************************}

class function TMySQLParser.TDropViewStmt.Create(const AParser: TMySQLParser; const ANodes: TNodes): TOffset;
begin
  Result := TStmt.Create(AParser, stDropView);

  with PDropViewStmt(AParser.NodePtr(Result))^ do
  begin
    Nodes := ANodes;

    Heritage.Heritage.AddChildren(@Nodes, SizeOf(Nodes) div SizeOf(TOffset));
  end;
end;

{ TMySQLParser.TEndLabel ******************************************************}

class function TMySQLParser.TEndLabel.Create(const AParser: TMySQLParser; const ANodes: TNodes): TOffset;
begin
  Result := TRange.Create(AParser, ntEndLabel);

  with PEndLabel(AParser.NodePtr(Result))^ do
  begin
    Nodes := ANodes;

    Heritage.AddChildren(@Nodes, SizeOf(Nodes) div SizeOf(TOffset));
  end;
end;

{ TMySQLParser.TExecuteStmt ***************************************************}

class function TMySQLParser.TExecuteStmt.Create(const AParser: TMySQLParser; const ANodes: TNodes): TOffset;
begin
  Result := TStmt.Create(AParser, stExecute);

  with PExecuteStmt(AParser.NodePtr(Result))^ do
  begin
    Nodes := ANodes;

    Heritage.Heritage.AddChildren(@Nodes, SizeOf(Nodes) div SizeOf(TOffset));
  end;
end;

{ TMySQLParser.TExistsFunc ****************************************************}

class function TMySQLParser.TExistsFunc.Create(const AParser: TMySQLParser; const ANodes: TNodes): TOffset;
begin
  Result := TRange.Create(AParser, ntExistsFunc);

  with PExistsFunc(AParser.NodePtr(Result))^ do
  begin
    Nodes := ANodes;

    Heritage.AddChildren(@Nodes, SizeOf(Nodes) div SizeOf(TOffset));
  end;
end;

{ TMySQLParser.TExplainStmt ***************************************************}

class function TMySQLParser.TExplainStmt.Create(const AParser: TMySQLParser; const ANodes: TNodes): TOffset;
begin
  Result := TStmt.Create(AParser, stExplain);

  with PExplainStmt(AParser.NodePtr(Result))^ do
  begin
    Nodes := ANodes;

    Heritage.Heritage.AddChildren(@Nodes, SizeOf(Nodes) div SizeOf(TOffset));
  end;
end;

{ TMySQLParser.TExtractFunc ***************************************************}

class function TMySQLParser.TExtractFunc.Create(const AParser: TMySQLParser; const ANodes: TNodes): TOffset;
begin
  Result := TRange.Create(AParser, ntExtractFunc);

  with PExtractFunc(AParser.NodePtr(Result))^ do
  begin
    Nodes := ANodes;

    Heritage.AddChildren(@Nodes, SizeOf(Nodes) div SizeOf(TOffset));
  end;
end;

{ TMySQLParser.TFetchStmt *****************************************************}

class function TMySQLParser.TFetchStmt.Create(const AParser: TMySQLParser; const ANodes: TNodes): TOffset;
begin
  Result := TStmt.Create(AParser, stFetch);

  with PFetchStmt(AParser.NodePtr(Result))^ do
  begin
    Nodes := ANodes;

    Heritage.Heritage.AddChildren(@Nodes, SizeOf(Nodes) div SizeOf(TOffset));
  end;
end;

{ TMySQLParser.TFlushStmt *****************************************************}

class function TMySQLParser.TFlushStmt.Create(const AParser: TMySQLParser; const ANodes: TNodes): TOffset;
begin
  Result := TStmt.Create(AParser, stFlush);

  with PFlushStmt(AParser.NodePtr(Result))^ do
  begin
    Nodes := ANodes;

    Heritage.Heritage.AddChildren(@Nodes, SizeOf(Nodes) div SizeOf(TOffset));
  end;
end;

{ TMySQLParser.TFlushStmtOption ***********************************************}

class function TMySQLParser.TFlushStmt.TOption.Create(const AParser: TMySQLParser; const ANodes: TOption.TNodes): TOffset;
begin
  Result := TRange.Create(AParser, ntFlushStmtOption);

  with POption(AParser.NodePtr(Result))^ do
  begin
    Nodes := ANodes;

    Heritage.AddChildren(@Nodes, SizeOf(Nodes) div SizeOf(TOffset));
  end;
end;

{ TMySQLParser.TFunctionCall **************************************************}

class function TMySQLParser.TFunctionCall.Create(const AParser: TMySQLParser; const AIdent, AArgumentsList: TOffset): TOffset;
var
  Nodes: TNodes;
begin
  Nodes.Ident := AIdent;
  Nodes.ArgumentsList := AArgumentsList;

  Result := Create(AParser, Nodes);
end;

class function TMySQLParser.TFunctionCall.Create(const AParser: TMySQLParser; const ANodes: TNodes): TOffset;
begin
  Result := TRange.Create(AParser, ntFunctionCall);

  with PFunctionCall(AParser.NodePtr(Result))^ do
  begin
    Nodes := ANodes;

    Heritage.AddChildren(@Nodes, SizeOf(Nodes) div SizeOf(TOffset));
  end;
end;

function TMySQLParser.TFunctionCall.GetArguments(): PChild;
begin
  Result := Parser.ChildPtr(Nodes.ArgumentsList);
end;

function TMySQLParser.TFunctionCall.GetIdent(): PChild;
begin
  Result := Parser.ChildPtr(Nodes.Ident);
end;

{ TMySQLParser.TFunctionReturns ***********************************************}

class function TMySQLParser.TFunctionReturns.Create(const AParser: TMySQLParser; const ANodes: TFunctionReturns.TNodes): TOffset;
begin
  Result := TRange.Create(AParser, ntFunctionReturns);

  with PFunctionReturns(AParser.NodePtr(Result))^ do
  begin
    Nodes := ANodes;

    Heritage.AddChildren(@Nodes, SizeOf(Nodes) div SizeOf(TOffset));
  end;
end;

{ TMySQLParser.TGetDiagnosticsStmt ********************************************}

class function TMySQLParser.TGetDiagnosticsStmt.Create(const AParser: TMySQLParser; const ANodes: TNodes): TOffset;
begin
  Result := TStmt.Create(AParser, stGetDiagnostics);

  with PGetDiagnosticsStmt(AParser.NodePtr(Result))^ do
  begin
    Nodes := ANodes;

    Heritage.Heritage.AddChildren(@Nodes, SizeOf(Nodes) div SizeOf(TOffset));
  end;
end;

{ TMySQLParser.TGetDiagnosticsStmt.TStmtInfo **********************************}

class function TMySQLParser.TGetDiagnosticsStmt.TStmtInfo.Create(const AParser: TMySQLParser; const ANodes: TStmtInfo.TNodes): TOffset;
begin
  Result := TRange.Create(AParser, ntGetDiagnosticsStmtStmtInfo);

  with PStmtInfo(AParser.NodePtr(Result))^ do
  begin
    Nodes := ANodes;

    Heritage.AddChildren(@Nodes, SizeOf(Nodes) div SizeOf(TOffset));
  end;
end;

{ TMySQLParser.TGetDiagnosticsStmt.TConditionalInfo ***************************}

class function TMySQLParser.TGetDiagnosticsStmt.TCondInfo.Create(const AParser: TMySQLParser; const ANodes: TCondInfo.TNodes): TOffset;
begin
  Result := TRange.Create(AParser, ntGetDiagnosticsStmtStmtInfo);

  with PCondInfo(AParser.NodePtr(Result))^ do
  begin
    Nodes := ANodes;

    Heritage.AddChildren(@Nodes, SizeOf(Nodes) div SizeOf(TOffset));
  end;
end;

{ TMySQLParser.TGrantStmt *****************************************************}

class function TMySQLParser.TGrantStmt.Create(const AParser: TMySQLParser; const ANodes: TNodes): TOffset;
begin
  Result := TStmt.Create(AParser, stGrant);

  with PGrantStmt(AParser.NodePtr(Result))^ do
  begin
    Nodes := ANodes;

    Heritage.Heritage.AddChildren(@Nodes, SizeOf(Nodes) div SizeOf(TOffset));
  end;
end;

{ TMySQLParser.TGrantStmt.TPrivileg *******************************************}

class function TMySQLParser.TGrantStmt.TPrivileg.Create(const AParser: TMySQLParser; const ANodes: TPrivileg.TNodes): TOffset;
begin
  Result := TRange.Create(AParser, ntGrantStmtPrivileg);

  with PPrivileg(AParser.NodePtr(Result))^ do
  begin
    Nodes := ANodes;

    Heritage.AddChildren(@Nodes, SizeOf(Nodes) div SizeOf(TOffset));
  end;
end;

{ TMySQLParser.TGrantStmt.TUserSpecification **********************************}

class function TMySQLParser.TGrantStmt.TUserSpecification.Create(const AParser: TMySQLParser; const ANodes: TUserSpecification.TNodes): TOffset;
begin
  Result := TRange.Create(AParser, ntGrantStmtUserSpecification);

  with PUserSpecification(AParser.NodePtr(Result))^ do
  begin
    Nodes := ANodes;

    Heritage.AddChildren(@Nodes, SizeOf(Nodes) div SizeOf(TOffset));
  end;
end;

{ TMySQLParser.TGroupConcatFunc ***********************************************}

class function TMySQLParser.TGroupConcatFunc.Create(const AParser: TMySQLParser; const ANodes: TNodes): TOffset;
begin
  Result := TRange.Create(AParser, ntGroupConcatFunc);

  with PGroupConcatFunc(AParser.NodePtr(Result))^ do
  begin
    Nodes := ANodes;

    Heritage.AddChildren(@Nodes, SizeOf(Nodes) div SizeOf(TOffset));
  end;
end;

{ TMySQLParser.TGroupConcatFunc.TExpr *****************************************}

class function TMySQLParser.TGroupConcatFunc.TExpr.Create(const AParser: TMySQLParser; const ANodes: TExpr.TNodes): TOffset;
begin
  Result := TRange.Create(AParser, ntGroupConcatFuncExpr);

  with PExpr(AParser.NodePtr(Result))^ do
  begin
    Nodes := ANodes;

    Heritage.AddChildren(@Nodes, SizeOf(Nodes) div SizeOf(TOffset));
  end;
end;

{ TMySQLParser.THelpStmt ******************************************************}

class function TMySQLParser.THelpStmt.Create(const AParser: TMySQLParser; const ANodes: THelpStmt.TNodes): TOffset;
begin
  Result := TStmt.Create(AParser, stHelp);

  with PHelpStmt(AParser.NodePtr(Result))^ do
  begin
    Nodes := ANodes;

    Heritage.Heritage.AddChildren(@Nodes, SizeOf(Nodes) div SizeOf(TOffset));
  end;
end;

{ TMySQLParser.TIfStmt.TBranch ************************************************}

class function TMySQLParser.TIfStmt.TBranch.Create(const AParser: TMySQLParser; const ANodes: TBranch.TNodes): TOffset;
begin
  Result := TRange.Create(AParser, ntIfStmtBranch);

  with PBranch(AParser.NodePtr(Result))^ do
  begin
    Nodes := ANodes;

    Heritage.AddChildren(@Nodes, SizeOf(Nodes) div SizeOf(TOffset));
  end;
end;

{ TMySQLParser.TIfStmt ********************************************************}

class function TMySQLParser.TIfStmt.Create(const AParser: TMySQLParser; const ANodes: TNodes): TOffset;
begin
  Result := TStmt.Create(AParser, stIf);

  with PIfStmt(AParser.NodePtr(Result))^ do
  begin
    Nodes := ANodes;

    Heritage.Heritage.AddChildren(@Nodes, SizeOf(Nodes) div SizeOf(TOffset));
  end;
end;

{ TMySQLParser.TIgnoreLines ***************************************************}

class function TMySQLParser.TIgnoreLines.Create(const AParser: TMySQLParser; const ANodes: TIgnoreLines.TNodes): TOffset;
begin
  Result := TRange.Create(AParser, ntIgnoreLines);

  with PIgnoreLines(AParser.NodePtr(Result))^ do
  begin
    Nodes := ANodes;

    Heritage.AddChildren(@Nodes, SizeOf(Nodes) div SizeOf(TOffset));
  end;
end;

{ TMySQLParser.TInOp **********************************************************}

class function TMySQLParser.TInOp.Create(const AParser: TMySQLParser; const ANodes: TNodes): TOffset;
begin
  Result := TRange.Create(AParser, ntInOp);

  with PInOp(AParser.NodePtr(Result))^ do
  begin
    Nodes := ANodes;

    Heritage.AddChildren(@Nodes, SizeOf(Nodes) div SizeOf(TOffset));
  end;
end;

{ TMySQLParser.TInsertStmt ****************************************************}

class function TMySQLParser.TInsertStmt.Create(const AParser: TMySQLParser; const ANodes: TNodes): TOffset;
begin
  Result := TStmt.Create(AParser, stInsert);

  with PInsertStmt(AParser.NodePtr(Result))^ do
  begin
    Nodes := ANodes;

    Heritage.Heritage.AddChildren(@Nodes, SizeOf(Nodes) div SizeOf(TOffset));
  end;
end;

{ TMySQLParser.TInsertStmt ****************************************************}

class function TMySQLParser.TInsertStmt.TSetItem.Create(const AParser: TMySQLParser; const ANodes: TSetItem.TNodes): TOffset;
begin
  Result := TRange.Create(AParser, ntInsertStmtSetItem);

  with PSetItem(AParser.NodePtr(Result))^ do
  begin
    Nodes := ANodes;

    Heritage.AddChildren(@Nodes, SizeOf(Nodes) div SizeOf(TOffset));
  end;
end;

{ TMySQLParser.TInterval ******************************************************}

class function TMySQLParser.TIntervalOp.Create(const AParser: TMySQLParser; const ANodes: TIntervalOp.TNodes): TOffset;
begin
  Result := TRange.Create(AParser, ntIntervalOp);

  with PIntervalOp(AParser.NodePtr(Result))^ do
  begin
    Nodes := ANodes;

    Heritage.AddChildren(@Nodes, SizeOf(Nodes) div SizeOf(TOffset));
  end;
end;

{ TMySQLParser.TIntervalListItem **********************************************}

class function TMySQLParser.TIntervalOp.TListItem.Create(const AParser: TMySQLParser; const ANodes: TListItem.TNodes): TOffset;
begin
  Result := TRange.Create(AParser, ntIntervalOpListItem);

  with PListItem(AParser.NodePtr(Result))^ do
  begin
    Nodes := ANodes;

    Heritage.AddChildren(@Nodes, SizeOf(Nodes) div SizeOf(TOffset));
  end;
end;

{ TMySQLParser.TIterateStmt ***************************************************}

class function TMySQLParser.TIterateStmt.Create(const AParser: TMySQLParser; const ANodes: TNodes): TOffset;
begin
  Result := TStmt.Create(AParser, stIterate);

  with PIterateStmt(AParser.NodePtr(Result))^ do
  begin
    Nodes := ANodes;

    Heritage.Heritage.AddChildren(@Nodes, SizeOf(Nodes) div SizeOf(TOffset));
  end;
end;

{ TMySQLParser.TKillStmt ******************************************************}

class function TMySQLParser.TKillStmt.Create(const AParser: TMySQLParser; const ANodes: TNodes): TOffset;
begin
  Result := TStmt.Create(AParser, stKill);

  with PKillStmt(AParser.NodePtr(Result))^ do
  begin
    Nodes := ANodes;

    Heritage.Heritage.AddChildren(@Nodes, SizeOf(Nodes) div SizeOf(TOffset));
  end;
end;

{ TMySQLParser.TLeaveStmt *****************************************************}

class function TMySQLParser.TLeaveStmt.Create(const AParser: TMySQLParser; const ANodes: TNodes): TOffset;
begin
  Result := TStmt.Create(AParser, stLeave);

  with PLeaveStmt(AParser.NodePtr(Result))^ do
  begin
    Nodes := ANodes;

    Heritage.Heritage.AddChildren(@Nodes, SizeOf(Nodes) div SizeOf(TOffset));
  end;
end;

{ TMySQLParser.TLikeOp ********************************************************}

class function TMySQLParser.TLikeOp.Create(const AParser: TMySQLParser; const ANodes: TNodes): TOffset;
begin
  Result := TRange.Create(AParser, ntLikeOp);

  with PLikeOp(AParser.NodePtr(Result))^ do
  begin
    Nodes := ANodes;

    Heritage.AddChildren(@Nodes, SizeOf(Nodes) div SizeOf(TOffset));
  end;
end;

{ TMySQLParser.TList **********************************************************}

class function TMySQLParser.TList.Create(const AParser: TMySQLParser;
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

function TMySQLParser.TList.GetFirstChild(): PChild;
begin
  Result := PChild(Parser.NodePtr(Nodes.FirstChild));
end;

{ TMySQLParser.TLoadDataStmt **************************************************}

class function TMySQLParser.TLoadDataStmt.Create(const AParser: TMySQLParser; const ANodes: TNodes): TOffset;
begin
  Result := TStmt.Create(AParser, stLoadData);

  with PLoadDataStmt(AParser.NodePtr(Result))^ do
  begin
    Nodes := ANodes;

    Heritage.Heritage.AddChildren(@Nodes, SizeOf(Nodes) div SizeOf(TOffset));
  end;
end;

{ TMySQLParser.TLoadXMLStmt ***************************************************}

class function TMySQLParser.TLoadXMLStmt.Create(const AParser: TMySQLParser; const ANodes: TNodes): TOffset;
begin
  Result := TStmt.Create(AParser, stLoadXML);

  with PLoadXMLStmt(AParser.NodePtr(Result))^ do
  begin
    Nodes := ANodes;

  end;
end;

{ TMySQLParser.TLockStmt.TItem ************************************************}

class function TMySQLParser.TLockStmt.TItem.Create(const AParser: TMySQLParser; const ANodes: TItem.TNodes): TOffset;
begin
  Result := TRange.Create(AParser, ntLockStmtItem);

  with PItem(AParser.NodePtr(Result))^ do
  begin
    Nodes := ANodes;

    Heritage.AddChildren(@Nodes, SizeOf(Nodes) div SizeOf(TOffset));
  end;
end;

{ TMySQLParser.TLockStmt ******************************************************}

class function TMySQLParser.TLockStmt.Create(const AParser: TMySQLParser; const ANodes: TNodes): TOffset;
begin
  Result := TStmt.Create(AParser, stLock);

  with PLockStmt(AParser.NodePtr(Result))^ do
  begin
    Nodes := ANodes;

    Heritage.Heritage.AddChildren(@Nodes, SizeOf(Nodes) div SizeOf(TOffset));
  end;
end;

{ TMySQLParser.TLoopStmt ******************************************************}

class function TMySQLParser.TLoopStmt.Create(const AParser: TMySQLParser; const ANodes: TNodes): TOffset;
begin
  Result := TStmt.Create(AParser, stLoop);

  with PLoopStmt(AParser.NodePtr(Result))^ do
  begin
    Nodes := ANodes;

    Heritage.Heritage.AddChildren(@Nodes, SizeOf(Nodes) div SizeOf(TOffset));
  end;
end;

{ TMySQLParser.TMatchFunc *****************************************************}

class function TMySQLParser.TMatchFunc.Create(const AParser: TMySQLParser; const ANodes: TNodes): TOffset;
begin
  Result := TRange.Create(AParser, ntMatchFunc);

  with PMatchFunc(AParser.NodePtr(Result))^ do
  begin
    Nodes := ANodes;

    Heritage.AddChildren(@Nodes, SizeOf(Nodes) div SizeOf(TOffset));
  end;
end;

{ TMySQLParser.TPositionFunc **************************************************}

class function TMySQLParser.TPositionFunc.Create(const AParser: TMySQLParser; const ANodes: TNodes): TOffset;
begin
  Result := TRange.Create(AParser, ntPositionFunc);

  with PPositionFunc(AParser.NodePtr(Result))^ do
  begin
    Nodes := ANodes;

    Heritage.AddChildren(@Nodes, SizeOf(Nodes) div SizeOf(TOffset));
  end;
end;

{ TMySQLParser.TPrepareStmt ***************************************************}

class function TMySQLParser.TPrepareStmt.Create(const AParser: TMySQLParser; const ANodes: TNodes): TOffset;
begin
  Result := TStmt.Create(AParser, stPrepare);

  with PPrepareStmt(AParser.NodePtr(Result))^ do
  begin
    Nodes := ANodes;

    Heritage.Heritage.AddChildren(@Nodes, SizeOf(Nodes) div SizeOf(TOffset));
  end;
end;

{ TMySQLParser.TPurgeStmt *****************************************************}

class function TMySQLParser.TPurgeStmt.Create(const AParser: TMySQLParser; const ANodes: TNodes): TOffset;
begin
  Result := TStmt.Create(AParser, stPurge);

  with PPurgeStmt(AParser.NodePtr(Result))^ do
  begin
    Nodes := ANodes;

    Heritage.Heritage.AddChildren(@Nodes, SizeOf(Nodes) div SizeOf(TOffset));
  end;
end;

{ TMySQLParser.TOpenStmt ******************************************************}

class function TMySQLParser.TOpenStmt.Create(const AParser: TMySQLParser; const ANodes: TNodes): TOffset;
begin
  Result := TStmt.Create(AParser, stOpen);

  with POpenStmt(AParser.NodePtr(Result))^ do
  begin
    Nodes := ANodes;

    Heritage.Heritage.AddChildren(@Nodes, SizeOf(Nodes) div SizeOf(TOffset));
  end;
end;

{ TMySQLParser.TOptimizeStmt **************************************************}

class function TMySQLParser.TOptimizeStmt.Create(const AParser: TMySQLParser; const ANodes: TNodes): TOffset;
begin
  Result := TStmt.Create(AParser, stOptimize);

  with POptimizeStmt(AParser.NodePtr(Result))^ do
  begin
    Nodes := ANodes;

    Heritage.Heritage.AddChildren(@Nodes, SizeOf(Nodes) div SizeOf(TOffset));
  end;
end;

{ TMySQLParser.TRenameTableStmt ***********************************************}

class function TMySQLParser.TRenameStmt.Create(const AParser: TMySQLParser; const ANodes: TNodes): TOffset;
begin
  Result := TStmt.Create(AParser, stRename);

  with PRenameStmt(AParser.NodePtr(Result))^ do
  begin
    Nodes := ANodes;

    Heritage.Heritage.AddChildren(@Nodes, SizeOf(Nodes) div SizeOf(TOffset));
  end;
end;

{ TMySQLParser.TRenameStmt.TPair **********************************************}

class function TMySQLParser.TRenameStmt.TPair.Create(const AParser: TMySQLParser; const ANodes: TPair.TNodes): TOffset;
begin
  Result := TRange.Create(AParser, ntRenameStmtPair);

  with PPair(AParser.NodePtr(Result))^ do
  begin
    Nodes := ANodes;

    Heritage.AddChildren(@Nodes, SizeOf(Nodes) div SizeOf(TOffset));
  end;
end;

{ TMySQLParser.TRegExpOp ******************************************************}

class function TMySQLParser.TRegExpOp.Create(const AParser: TMySQLParser; const ANodes: TNodes): TOffset;
begin
  Result := TRange.Create(AParser, ntRegExpOp);

  with PRegExpOp(AParser.NodePtr(Result))^ do
  begin
    Nodes := ANodes;

    Heritage.AddChildren(@Nodes, SizeOf(Nodes) div SizeOf(TOffset));
  end;
end;

{ TMySQLParser.TReleaseStmt ***************************************************}

class function TMySQLParser.TReleaseStmt.Create(const AParser: TMySQLParser; const ANodes: TNodes): TOffset;
begin
  Result := TStmt.Create(AParser, stRelease);

  with PReleaseStmt(AParser.NodePtr(Result))^ do
  begin
    Nodes := ANodes;

    Heritage.Heritage.AddChildren(@Nodes, SizeOf(Nodes) div SizeOf(TOffset));
  end;
end;

{ TMySQLParser.TRepairStmt ****************************************************}

class function TMySQLParser.TRepairStmt.Create(const AParser: TMySQLParser; const ANodes: TNodes): TOffset;
begin
  Result := TStmt.Create(AParser, stRepair);

  with PRepairStmt(AParser.NodePtr(Result))^ do
  begin
    Nodes := ANodes;

    Heritage.Heritage.AddChildren(@Nodes, SizeOf(Nodes) div SizeOf(TOffset));
  end;
end;

{ TMySQLParser.TRepeatStmt ****************************************************}

class function TMySQLParser.TRepeatStmt.Create(const AParser: TMySQLParser; const ANodes: TNodes): TOffset;
begin
  Result := TStmt.Create(AParser, stRepeat);

  with PRepeatStmt(AParser.NodePtr(Result))^ do
  begin
    Nodes := ANodes;

    Heritage.Heritage.AddChildren(@Nodes, SizeOf(Nodes) div SizeOf(TOffset));
  end;
end;

{ TMySQLParser.TRevokeStmt ****************************************************}

class function TMySQLParser.TRevokeStmt.Create(const AParser: TMySQLParser; const ANodes: TNodes): TOffset;
begin
  Result := TStmt.Create(AParser, stRevoke);

  with PRevokeStmt(AParser.NodePtr(Result))^ do
  begin
    Nodes := ANodes;

    Heritage.Heritage.AddChildren(@Nodes, SizeOf(Nodes) div SizeOf(TOffset));
  end;
end;

{ TMySQLParser.TResetStmt *****************************************************}

class function TMySQLParser.TResetStmt.Create(const AParser: TMySQLParser; const ANodes: TNodes): TOffset;
begin
  Result := TStmt.Create(AParser, stReset);

  with PResetStmt(AParser.NodePtr(Result))^ do
  begin
    Nodes := ANodes;

    Heritage.Heritage.AddChildren(@Nodes, SizeOf(Nodes) div SizeOf(TOffset));
  end;
end;

{ TMySQLParser.TResetStmt.TOption *********************************************}

class function TMySQLParser.TResetStmt.TOption.Create(const AParser: TMySQLParser; const ANodes: TOption.TNodes): TOffset;
begin
  Result := TStmt.Create(AParser, stReset);

  with POption(AParser.NodePtr(Result))^ do
  begin
    Nodes := ANodes;

    Heritage.AddChildren(@Nodes, SizeOf(Nodes) div SizeOf(TOffset));
  end;
end;

{ TMySQLParser.TReturnStmt ****************************************************}

class function TMySQLParser.TReturnStmt.Create(const AParser: TMySQLParser; const ANodes: TNodes): TOffset;
begin
  Result := TStmt.Create(AParser, stReturn);

  with PReturnStmt(AParser.NodePtr(Result))^ do
  begin
    Nodes := ANodes;

    Heritage.Heritage.AddChildren(@Nodes, SizeOf(Nodes) div SizeOf(TOffset));
  end;
end;

{ TMySQLParser.TRollbackStmt **************************************************}

class function TMySQLParser.TRollbackStmt.Create(const AParser: TMySQLParser; const ANodes: TNodes): TOffset;
begin
  Result := TStmt.Create(AParser, stRollback);

  with PRollbackStmt(AParser.NodePtr(Result))^ do
  begin
    Nodes := ANodes;

    Heritage.Heritage.AddChildren(@Nodes, SizeOf(Nodes) div SizeOf(TOffset));
  end;
end;

{ TMySQLParser.TRoutineParam **************************************************}

class function TMySQLParser.TRoutineParam.Create(const AParser: TMySQLParser; const ANodes: TNodes): TOffset;
begin
  Result := TRange.Create(AParser, ntRoutineParam);

  with PRoutineParam(AParser.NodePtr(Result))^ do
  begin
    Nodes := ANodes;

    Heritage.AddChildren(@Nodes, SizeOf(Nodes) div SizeOf(TOffset));
  end;
end;

{ TMySQLParser.TSavepointStmt *************************************************}

class function TMySQLParser.TSavepointStmt.Create(const AParser: TMySQLParser; const ANodes: TNodes): TOffset;
begin
  Result := TStmt.Create(AParser, stSavepoint);

  with PSavepointStmt(AParser.NodePtr(Result))^ do
  begin
    Nodes := ANodes;

    Heritage.Heritage.AddChildren(@Nodes, SizeOf(Nodes) div SizeOf(TOffset));
  end;
end;

{ TMySQLParser.TSecretIdent ***************************************************}

class function TMySQLParser.TSecretIdent.Create(const AParser: TMySQLParser; const ANodes: TNodes): TOffset;
begin
  Result := TRange.Create(AParser, ntSecretIdent);

  with PSecretIdent(AParser.NodePtr(Result))^ do
  begin
    Nodes := ANodes;

    Heritage.AddChildren(@Nodes, SizeOf(Nodes) div SizeOf(TOffset));
  end;
end;

{ TMySQLParser.TSelectStmt.TColumn ********************************************}

class function TMySQLParser.TSelectStmt.TColumn.Create(const AParser: TMySQLParser; const ANodes: TColumn.TNodes): TOffset;
begin
  Result := TRange.Create(AParser, ntSelectStmtColumn);

  with PColumn(AParser.NodePtr(Result))^ do
  begin
    Nodes := ANodes;

    Heritage.AddChildren(@Nodes, SizeOf(Nodes) div SizeOf(TOffset));
  end;
end;

{ TMySQLParser.TSelectStmt.TTableFactor.TIndexHint ****************************}

class function TMySQLParser.TSelectStmt.TTableFactor.TIndexHint.Create(const AParser: TMySQLParser; const ANodes: TIndexHint.TNodes): TOffset;
begin
  Result := TRange.Create(AParser, ntSelectStmtTableFactorIndexHint);

  with PIndexHint(AParser.NodePtr(Result))^ do
  begin
    Nodes := ANodes;

    Heritage.AddChildren(@Nodes, SizeOf(Nodes) div SizeOf(TOffset));
  end;
end;

{ TMySQLParser.TSelectStmt.TTableFactor ***************************************}

class function TMySQLParser.TSelectStmt.TTableFactor.Create(const AParser: TMySQLParser; const ANodes: TTableFactor.TNodes): TOffset;
begin
  Result := TRange.Create(AParser, ntSelectStmtTableFactor);

  with PTableFactor(AParser.NodePtr(Result))^ do
  begin
    Nodes := ANodes;

    Heritage.AddChildren(@Nodes, SizeOf(Nodes) div SizeOf(TOffset));
  end;
end;

{ TMySQLParser.TSelectStmt.TTableFactorOj *************************************}

class function TMySQLParser.TSelectStmt.TTableFactorOj.Create(const AParser: TMySQLParser; const ANodes: TTableFactorOj.TNodes): TOffset;
begin
  Result := TRange.Create(AParser, ntSelectStmtTableFactorOj);

  with PTableFactorOj(AParser.NodePtr(Result))^ do
  begin
    Nodes := ANodes;

    Heritage.AddChildren(@Nodes, SizeOf(Nodes) div SizeOf(TOffset));
  end;
end;

{ TMySQLParser.TSelectStmt.TTableFactorSelect *********************************}

class function TMySQLParser.TSelectStmt.TTableFactorSelect.Create(const AParser: TMySQLParser; const ANodes: TTableFactorSelect.TNodes): TOffset;
begin
  Result := TRange.Create(AParser, ntSelectStmtTableFactorSelect);

  with PTableFactorSelect(AParser.NodePtr(Result))^ do
  begin
    Nodes := ANodes;

    Heritage.AddChildren(@Nodes, SizeOf(Nodes) div SizeOf(TOffset));
  end;
end;

{ TMySQLParser.TSelectStmt.TTableJoin *****************************************}

class function TMySQLParser.TSelectStmt.TTableJoin.Create(const AParser: TMySQLParser; const AJoinType: TJoinType; const ANodes: TTableJoin.TNodes): TOffset;
begin
  Result := TRange.Create(AParser, ntSelectStmtTableJoin);

  with PTableJoin(AParser.NodePtr(Result))^ do
  begin
    FJoinType := AJoinType;

    Nodes := ANodes;

    Heritage.AddChildren(@Nodes, SizeOf(Nodes) div SizeOf(TOffset));
  end;
end;

{ TMySQLParser.TSelectStmt.TGroup *********************************************}

class function TMySQLParser.TSelectStmt.TGroup.Create(const AParser: TMySQLParser; const ANodes: TGroup.TNodes): TOffset;
begin
  Result := TRange.Create(AParser, ntSelectStmtGroup);

  with PGroup(AParser.NodePtr(Result))^ do
  begin
    Nodes := ANodes;

    Heritage.AddChildren(@Nodes, SizeOf(Nodes) div SizeOf(TOffset));
  end;
end;

{ TMySQLParser.TSelectStmt.TOrder *********************************************}

class function TMySQLParser.TSelectStmt.TOrder.Create(const AParser: TMySQLParser; const ANodes: TOrder.TNodes): TOffset;
begin
  Result := TRange.Create(AParser, ntSelectStmtOrder);

  with POrder(AParser.NodePtr(Result))^ do
  begin
    Nodes := ANodes;

    Heritage.AddChildren(@Nodes, SizeOf(Nodes) div SizeOf(TOffset));
  end;
end;

{ TMySQLParser.TSelectStmt ****************************************************}

class function TMySQLParser.TSelectStmt.Create(const AParser: TMySQLParser; const ANodes: TNodes): TOffset;
begin
  Result := TStmt.Create(AParser, stSelect);

  with PSelectStmt(AParser.NodePtr(Result))^ do
  begin
    Nodes := ANodes;

    Heritage.Heritage.AddChildren(@Nodes, SizeOf(Nodes) div SizeOf(TOffset));
  end;
end;

{ TMySQLParser.TSchedule ******************************************************}

class function TMySQLParser.TSchedule.Create(const AParser: TMySQLParser; const ANodes: TNodes): TOffset;
begin
  Result := TRange.Create(AParser, ntSchedule);

  with PSchedule(AParser.NodePtr(Result))^ do
  begin
    Nodes := ANodes;

    Heritage.AddChildren(@Nodes, SizeOf(Nodes) div SizeOf(TOffset));
  end;
end;

{ TMySQLParser.TSetStmt.TAssignment *******************************************}

class function TMySQLParser.TSetStmt.TAssignment.Create(const AParser: TMySQLParser; const ANodes: TAssignment.TNodes): TOffset;
begin
  Result := TRange.Create(AParser, ntSetStmtAssignment);

  with PAssignment(AParser.NodePtr(Result))^ do
  begin
    Nodes := ANodes;

    Heritage.AddChildren(@Nodes, SizeOf(Nodes) div SizeOf(TOffset));
  end;
end;

{ TMySQLParser.TSetStmt *******************************************************}

class function TMySQLParser.TSetStmt.Create(const AParser: TMySQLParser; const ANodes: TNodes): TOffset;
begin
  Result := TStmt.Create(AParser, stSet);

  with PSetStmt(AParser.NodePtr(Result))^ do
  begin
    Nodes := ANodes;

    Heritage.Heritage.AddChildren(@Nodes, SizeOf(Nodes) div SizeOf(TOffset));
  end;
end;

{ TMySQLParser.TSetNamesStmt **************************************************}

class function TMySQLParser.TSetNamesStmt.Create(const AParser: TMySQLParser; const ANodes: TNodes): TOffset;
begin
  Result := TStmt.Create(AParser, stSetNames);

  with PSetNamesStmt(AParser.NodePtr(Result))^ do
  begin
    Nodes := ANodes;

    Heritage.Heritage.AddChildren(@Nodes, SizeOf(Nodes) div SizeOf(TOffset));
  end;
end;

{ TMySQLParser.TSetPasswordStmt ***********************************************}

class function TMySQLParser.TSetPasswordStmt.Create(const AParser: TMySQLParser; const ANodes: TNodes): TOffset;
begin
  Result := TStmt.Create(AParser, stSetPassword);

  with PSetPasswordStmt(AParser.NodePtr(Result))^ do
  begin
    Nodes := ANodes;

    Heritage.Heritage.AddChildren(@Nodes, SizeOf(Nodes) div SizeOf(TOffset));
  end;
end;

{ TMySQLParser.TSetTransactionStmt.TCharacteristic ****************************}

class function TMySQLParser.TSetTransactionStmt.TCharacteristic.Create(const AParser: TMySQLParser; const ANodes: TCharacteristic.TNodes): TOffset;
begin
  Result := TRange.Create(AParser, ntSetTransactionStmtCharacteristic);

  with PCharacteristic(AParser.NodePtr(Result))^ do
  begin
    Nodes := ANodes;

    Heritage.AddChildren(@Nodes, SizeOf(Nodes) div SizeOf(TOffset));
  end;
end;

{ TMySQLParser.TSetTransactionStmt ********************************************}

class function TMySQLParser.TSetTransactionStmt.Create(const AParser: TMySQLParser; const ANodes: TNodes): TOffset;
begin
  Result := TStmt.Create(AParser, stSetTransaction);

  with PSetTransactionStmt(AParser.NodePtr(Result))^ do
  begin
    Nodes := ANodes;

    Heritage.Heritage.AddChildren(@Nodes, SizeOf(Nodes) div SizeOf(TOffset));
  end;
end;

{ TMySQLParser.TShowAuthorsStmt ***********************************************}

class function TMySQLParser.TShowAuthorsStmt.Create(const AParser: TMySQLParser; const ANodes: TNodes): TOffset;
begin
  Result := TStmt.Create(AParser, stShowAuthors);

  with PShowAuthorsStmt(AParser.NodePtr(Result))^ do
  begin
    Nodes := ANodes;

    Heritage.Heritage.AddChildren(@Nodes, SizeOf(Nodes) div SizeOf(TOffset));
  end;
end;

{ TMySQLParser.TShowBinaryLogsStmt ********************************************}

class function TMySQLParser.TShowBinaryLogsStmt.Create(const AParser: TMySQLParser; const ANodes: TNodes): TOffset;
begin
  Result := TStmt.Create(AParser, stShowBinaryLogs);

  with PShowBinaryLogsStmt(AParser.NodePtr(Result))^ do
  begin
    Nodes := ANodes;

    Heritage.Heritage.AddChildren(@Nodes, SizeOf(Nodes) div SizeOf(TOffset));
  end;
end;

{ TMySQLParser.TShowBinlogEventsStmt ******************************************}

class function TMySQLParser.TShowBinlogEventsStmt.Create(const AParser: TMySQLParser; const ANodes: TNodes): TOffset;
begin
  Result := TStmt.Create(AParser, stShowBinlogEvents);

  with PShowBinlogEventsStmt(AParser.NodePtr(Result))^ do
  begin
    Nodes := ANodes;

    Heritage.Heritage.AddChildren(@Nodes, SizeOf(Nodes) div SizeOf(TOffset));
  end;
end;

{ TMySQLParser.TShowCharacterSetStmt ******************************************}

class function TMySQLParser.TShowCharacterSetStmt.Create(const AParser: TMySQLParser; const ANodes: TNodes): TOffset;
begin
  Result := TStmt.Create(AParser, stShowCharacterSet);

  with PShowCharacterSetStmt(AParser.NodePtr(Result))^ do
  begin
    Nodes := ANodes;

    Heritage.Heritage.AddChildren(@Nodes, SizeOf(Nodes) div SizeOf(TOffset));
  end;
end;

{ TMySQLParser.TShowCollationStmt *********************************************}

class function TMySQLParser.TShowCollationStmt.Create(const AParser: TMySQLParser; const ANodes: TNodes): TOffset;
begin
  Result := TStmt.Create(AParser, stShowCollation);

  with PShowCollationStmt(AParser.NodePtr(Result))^ do
  begin
    Nodes := ANodes;

    Heritage.Heritage.AddChildren(@Nodes, SizeOf(Nodes) div SizeOf(TOffset));
  end;
end;

{ TMySQLParser.TShowContributorsStmt ******************************************}

class function TMySQLParser.TShowContributorsStmt.Create(const AParser: TMySQLParser; const ANodes: TNodes): TOffset;
begin
  Result := TStmt.Create(AParser, stShowContributors);

  with PShowContributorsStmt(AParser.NodePtr(Result))^ do
  begin
    Nodes := ANodes;

    Heritage.Heritage.AddChildren(@Nodes, SizeOf(Nodes) div SizeOf(TOffset));
  end;
end;

{ TMySQLParser.TShowCountErrorsStmt *******************************************}

class function TMySQLParser.TShowCountErrorsStmt.Create(const AParser: TMySQLParser; const ANodes: TNodes): TOffset;
begin
  Result := TStmt.Create(AParser, stShowCountErrors);

  with PShowCountErrorsStmt(AParser.NodePtr(Result))^ do
  begin
    Nodes := ANodes;

    Heritage.Heritage.AddChildren(@Nodes, SizeOf(Nodes) div SizeOf(TOffset));
  end;
end;

{ TMySQLParser.TShowCountWarningsStmt *****************************************}

class function TMySQLParser.TShowCountWarningsStmt.Create(const AParser: TMySQLParser; const ANodes: TNodes): TOffset;
begin
  Result := TStmt.Create(AParser, stShowCountWarnings);

  with PShowCountWarningsStmt(AParser.NodePtr(Result))^ do
  begin
    Nodes := ANodes;

    Heritage.Heritage.AddChildren(@Nodes, SizeOf(Nodes) div SizeOf(TOffset));
  end;
end;

{ TMySQLParser.TShowCreateDatabaseStmt ****************************************}

class function TMySQLParser.TShowCreateDatabaseStmt.Create(const AParser: TMySQLParser; const ANodes: TNodes): TOffset;
begin
  Result := TStmt.Create(AParser, stShowCreateDatabase);

  with PShowCreateDatabaseStmt(AParser.NodePtr(Result))^ do
  begin
    Nodes := ANodes;

    Heritage.Heritage.AddChildren(@Nodes, SizeOf(Nodes) div SizeOf(TOffset));
  end;
end;

{ TMySQLParser.TShowCreateEventStmt *******************************************}

class function TMySQLParser.TShowCreateEventStmt.Create(const AParser: TMySQLParser; const ANodes: TNodes): TOffset;
begin
  Result := TStmt.Create(AParser, stShowCreateEvent);

  with PShowCreateEventStmt(AParser.NodePtr(Result))^ do
  begin
    Nodes := ANodes;

    Heritage.Heritage.AddChildren(@Nodes, SizeOf(Nodes) div SizeOf(TOffset));
  end;
end;

{ TMySQLParser.TShowCreateFunctionStmt ****************************************}

class function TMySQLParser.TShowCreateFunctionStmt.Create(const AParser: TMySQLParser; const ANodes: TNodes): TOffset;
begin
  Result := TStmt.Create(AParser, stShowCreateFunction);

  with PShowCreateFunctionStmt(AParser.NodePtr(Result))^ do
  begin
    Nodes := ANodes;

    Heritage.Heritage.AddChildren(@Nodes, SizeOf(Nodes) div SizeOf(TOffset));
  end;
end;

{ TMySQLParser.TShowCreateProcedureStmt ***************************************}

class function TMySQLParser.TShowCreateProcedureStmt.Create(const AParser: TMySQLParser; const ANodes: TNodes): TOffset;
begin
  Result := TStmt.Create(AParser, stShowCreateProcedure);

  with PShowCreateProcedureStmt(AParser.NodePtr(Result))^ do
  begin
    Nodes := ANodes;

    Heritage.Heritage.AddChildren(@Nodes, SizeOf(Nodes) div SizeOf(TOffset));
  end;
end;

{ TMySQLParser.TShowCreateTableStmt *******************************************}

class function TMySQLParser.TShowCreateTableStmt.Create(const AParser: TMySQLParser; const ANodes: TNodes): TOffset;
begin
  Result := TStmt.Create(AParser, stShowCreateTable);

  with PShowCreateTableStmt(AParser.NodePtr(Result))^ do
  begin
    Nodes := ANodes;

    Heritage.Heritage.AddChildren(@Nodes, SizeOf(Nodes) div SizeOf(TOffset));
  end;
end;

{ TMySQLParser.TShowCreateTriggerStmt *****************************************}

class function TMySQLParser.TShowCreateTriggerStmt.Create(const AParser: TMySQLParser; const ANodes: TNodes): TOffset;
begin
  Result := TStmt.Create(AParser, stShowCreateTrigger);

  with PShowCreateTriggerStmt(AParser.NodePtr(Result))^ do
  begin
    Nodes := ANodes;

    Heritage.Heritage.AddChildren(@Nodes, SizeOf(Nodes) div SizeOf(TOffset));
  end;
end;

{ TMySQLParser.TShowCreateUserStmt ********************************************}

class function TMySQLParser.TShowCreateUserStmt.Create(const AParser: TMySQLParser; const ANodes: TNodes): TOffset;
begin
  Result := TStmt.Create(AParser, stShowCreateUser);

  with PShowCreateUserStmt(AParser.NodePtr(Result))^ do
  begin
    Nodes := ANodes;

    Heritage.Heritage.AddChildren(@Nodes, SizeOf(Nodes) div SizeOf(TOffset));
  end;
end;

{ TMySQLParser.TShowCreateViewStmt ********************************************}

class function TMySQLParser.TShowCreateViewStmt.Create(const AParser: TMySQLParser; const ANodes: TNodes): TOffset;
begin
  Result := TStmt.Create(AParser, stShowCreateView);

  with PShowCreateViewStmt(AParser.NodePtr(Result))^ do
  begin
    Nodes := ANodes;

    Heritage.Heritage.AddChildren(@Nodes, SizeOf(Nodes) div SizeOf(TOffset));
  end;
end;

{ TMySQLParser.TShowDatabasesStmt *********************************************}

class function TMySQLParser.TShowDatabasesStmt.Create(const AParser: TMySQLParser; const ANodes: TNodes): TOffset;
begin
  Result := TStmt.Create(AParser, stShowDatabases);

  with PShowDatabasesStmt(AParser.NodePtr(Result))^ do
  begin
    Nodes := ANodes;

    Heritage.Heritage.AddChildren(@Nodes, SizeOf(Nodes) div SizeOf(TOffset));
  end;
end;

{ TMySQLParser.TShowEngineStmt ************************************************}

class function TMySQLParser.TShowEngineStmt.Create(const AParser: TMySQLParser; const ANodes: TNodes): TOffset;
begin
  Result := TStmt.Create(AParser, stShowEngine);

  with PShowEngineStmt(AParser.NodePtr(Result))^ do
  begin
    Nodes := ANodes;

    Heritage.Heritage.AddChildren(@Nodes, SizeOf(Nodes) div SizeOf(TOffset));
  end;
end;

{ TMySQLParser.TShowEnginesStmt ***********************************************}

class function TMySQLParser.TShowEnginesStmt.Create(const AParser: TMySQLParser; const ANodes: TNodes): TOffset;
begin
  Result := TStmt.Create(AParser, stShowEngines);

  with PShowEnginesStmt(AParser.NodePtr(Result))^ do
  begin
    Nodes := ANodes;

    Heritage.Heritage.AddChildren(@Nodes, SizeOf(Nodes) div SizeOf(TOffset));
  end;
end;

{ TMySQLParser.TShowErrorsStmt ************************************************}

class function TMySQLParser.TShowErrorsStmt.Create(const AParser: TMySQLParser; const ANodes: TNodes): TOffset;
begin
  Result := TStmt.Create(AParser, stShowErrors);

  with PShowErrorsStmt(AParser.NodePtr(Result))^ do
  begin
    Nodes := ANodes;

    Heritage.Heritage.AddChildren(@Nodes, SizeOf(Nodes) div SizeOf(TOffset));
  end;
end;

{ TMySQLParser.TShowEventsStmt ************************************************}

class function TMySQLParser.TShowEventsStmt.Create(const AParser: TMySQLParser; const ANodes: TNodes): TOffset;
begin
  Result := TStmt.Create(AParser, stShowEvents);

  with PShowEventsStmt(AParser.NodePtr(Result))^ do
  begin
    Nodes := ANodes;

    Heritage.Heritage.AddChildren(@Nodes, SizeOf(Nodes) div SizeOf(TOffset));
  end;
end;

{ TMySQLParser.TShowFunctionCodeStmt ******************************************}

class function TMySQLParser.TShowFunctionCodeStmt.Create(const AParser: TMySQLParser; const ANodes: TNodes): TOffset;
begin
  Result := TStmt.Create(AParser, stShowFunctionCode);

  with PShowFunctionCodeStmt(AParser.NodePtr(Result))^ do
  begin
    Nodes := ANodes;

    Heritage.Heritage.AddChildren(@Nodes, SizeOf(Nodes) div SizeOf(TOffset));
  end;
end;

{ TMySQLParser.TShowFunctionStatusStmt ****************************************}

class function TMySQLParser.TShowFunctionStatusStmt.Create(const AParser: TMySQLParser; const ANodes: TNodes): TOffset;
begin
  Result := TStmt.Create(AParser, stShowFunctionStatus);

  with PShowFunctionStatusStmt(AParser.NodePtr(Result))^ do
  begin
    Nodes := ANodes;

    Heritage.Heritage.AddChildren(@Nodes, SizeOf(Nodes) div SizeOf(TOffset));
  end;
end;

{ TMySQLParser.TShowGrantsStmt ************************************************}

class function TMySQLParser.TShowGrantsStmt.Create(const AParser: TMySQLParser; const ANodes: TNodes): TOffset;
begin
  Result := TStmt.Create(AParser, stShowGrants);

  with PShowGrantsStmt(AParser.NodePtr(Result))^ do
  begin
    Nodes := ANodes;

    Heritage.Heritage.AddChildren(@Nodes, SizeOf(Nodes) div SizeOf(TOffset));
  end;
end;

{ TMySQLParser.TShowIndexStmt *************************************************}

class function TMySQLParser.TShowIndexStmt.Create(const AParser: TMySQLParser; const ANodes: TNodes): TOffset;
begin
  Result := TStmt.Create(AParser, stShowIndex);

  with PShowIndexStmt(AParser.NodePtr(Result))^ do
  begin
    Nodes := ANodes;

    Heritage.Heritage.AddChildren(@Nodes, SizeOf(Nodes) div SizeOf(TOffset));
  end;
end;

{ TMySQLParser.TShowMasterStatusStmt ******************************************}

class function TMySQLParser.TShowMasterStatusStmt.Create(const AParser: TMySQLParser; const ANodes: TNodes): TOffset;
begin
  Result := TStmt.Create(AParser, stShowMasterStatus);

  with PShowMasterStatusStmt(AParser.NodePtr(Result))^ do
  begin
    Nodes := ANodes;

    Heritage.Heritage.AddChildren(@Nodes, SizeOf(Nodes) div SizeOf(TOffset));
  end;
end;

{ TMySQLParser.TShowOpenTablesStmt ********************************************}

class function TMySQLParser.TShowOpenTablesStmt.Create(const AParser: TMySQLParser; const ANodes: TNodes): TOffset;
begin
  Result := TStmt.Create(AParser, stShowOpenTables);

  with PShowOpenTablesStmt(AParser.NodePtr(Result))^ do
  begin
    Nodes := ANodes;

    Heritage.Heritage.AddChildren(@Nodes, SizeOf(Nodes) div SizeOf(TOffset));
  end;
end;

{ TMySQLParser.TShowPluginsStmt ***********************************************}

class function TMySQLParser.TShowPluginsStmt.Create(const AParser: TMySQLParser; const ANodes: TNodes): TOffset;
begin
  Result := TStmt.Create(AParser, stShowPlugins);

  with PShowPluginsStmt(AParser.NodePtr(Result))^ do
  begin
    Nodes := ANodes;

    Heritage.Heritage.AddChildren(@Nodes, SizeOf(Nodes) div SizeOf(TOffset));
  end;
end;

{ TMySQLParser.TShowPrivilegesStmt ********************************************}

class function TMySQLParser.TShowPrivilegesStmt.Create(const AParser: TMySQLParser; const ANodes: TNodes): TOffset;
begin
  Result := TStmt.Create(AParser, stShowPrivileges);

  with PShowPrivilegesStmt(AParser.NodePtr(Result))^ do
  begin
    Nodes := ANodes;

    Heritage.Heritage.AddChildren(@Nodes, SizeOf(Nodes) div SizeOf(TOffset));
  end;
end;

{ TMySQLParser.TShowProcedureCodeStmt *****************************************}

class function TMySQLParser.TShowProcedureCodeStmt.Create(const AParser: TMySQLParser; const ANodes: TNodes): TOffset;
begin
  Result := TStmt.Create(AParser, stShowProcedureCode);

  with PShowProcedureCodeStmt(AParser.NodePtr(Result))^ do
  begin
    Nodes := ANodes;

    Heritage.Heritage.AddChildren(@Nodes, SizeOf(Nodes) div SizeOf(TOffset));
  end;
end;

{ TMySQLParser.TShowProcedureStatusStmt ***************************************}

class function TMySQLParser.TShowProcedureStatusStmt.Create(const AParser: TMySQLParser; const ANodes: TNodes): TOffset;
begin
  Result := TStmt.Create(AParser, stShowProcedureStatus);

  with PShowProcedureStatusStmt(AParser.NodePtr(Result))^ do
  begin
    Nodes := ANodes;

    Heritage.Heritage.AddChildren(@Nodes, SizeOf(Nodes) div SizeOf(TOffset));
  end;
end;

{ TMySQLParser.TShowProcessListStmt *******************************************}

class function TMySQLParser.TShowProcessListStmt.Create(const AParser: TMySQLParser; const ANodes: TNodes): TOffset;
begin
  Result := TStmt.Create(AParser, stShowProcessList);

  with PShowProcessListStmt(AParser.NodePtr(Result))^ do
  begin
    Nodes := ANodes;

    Heritage.Heritage.AddChildren(@Nodes, SizeOf(Nodes) div SizeOf(TOffset));
  end;
end;

{ TMySQLParser.TShowProfileStmt ***********************************************}

class function TMySQLParser.TShowProfileStmt.Create(const AParser: TMySQLParser; const ANodes: TNodes): TOffset;
begin
  Result := TStmt.Create(AParser, stShowProfile);

  with PShowProfileStmt(AParser.NodePtr(Result))^ do
  begin
    Nodes := ANodes;

    Heritage.Heritage.AddChildren(@Nodes, SizeOf(Nodes) div SizeOf(TOffset));
  end;
end;

{ TMySQLParser.TShowProfilesStmt **********************************************}

class function TMySQLParser.TShowProfilesStmt.Create(const AParser: TMySQLParser; const ANodes: TNodes): TOffset;
begin
  Result := TStmt.Create(AParser, stShowProfiles);

  with PShowProfilesStmt(AParser.NodePtr(Result))^ do
  begin
    Nodes := ANodes;

    Heritage.Heritage.AddChildren(@Nodes, SizeOf(Nodes) div SizeOf(TOffset));
  end;
end;

{ TMySQLParser.TShowRelaylogEventsStmt ****************************************}

class function TMySQLParser.TShowRelaylogEventsStmt.Create(const AParser: TMySQLParser; const ANodes: TNodes): TOffset;
begin
  Result := TStmt.Create(AParser, stShowRelaylogEvents);

  with PShowRelaylogEventsStmt(AParser.NodePtr(Result))^ do
  begin
    Nodes := ANodes;

    Heritage.Heritage.AddChildren(@Nodes, SizeOf(Nodes) div SizeOf(TOffset));
  end;
end;

{ TMySQLParser.TShowSlaveHostsStmt ********************************************}

class function TMySQLParser.TShowSlaveHostsStmt.Create(const AParser: TMySQLParser; const ANodes: TNodes): TOffset;
begin
  Result := TStmt.Create(AParser, stShowSlaveHosts);

  with PShowSlaveHostsStmt(AParser.NodePtr(Result))^ do
  begin
    Nodes := ANodes;

    Heritage.Heritage.AddChildren(@Nodes, SizeOf(Nodes) div SizeOf(TOffset));
  end;
end;

{ TMySQLParser.TShowSlaveStatusStmt *******************************************}

class function TMySQLParser.TShowSlaveStatusStmt.Create(const AParser: TMySQLParser; const ANodes: TNodes): TOffset;
begin
  Result := TStmt.Create(AParser, stShowSlaveStatus);

  with PShowSlaveStatusStmt(AParser.NodePtr(Result))^ do
  begin
    Nodes := ANodes;

    Heritage.Heritage.AddChildren(@Nodes, SizeOf(Nodes) div SizeOf(TOffset));
  end;
end;

{ TMySQLParser.TShowStatusStmt ************************************************}

class function TMySQLParser.TShowStatusStmt.Create(const AParser: TMySQLParser; const ANodes: TNodes): TOffset;
begin
  Result := TStmt.Create(AParser, stShowStatus);

  with PShowStatusStmt(AParser.NodePtr(Result))^ do
  begin
    Nodes := ANodes;

    Heritage.Heritage.AddChildren(@Nodes, SizeOf(Nodes) div SizeOf(TOffset));
  end;
end;

{ TMySQLParser.TShowTableStatusStmt *******************************************}

class function TMySQLParser.TShowTableStatusStmt.Create(const AParser: TMySQLParser; const ANodes: TNodes): TOffset;
begin
  Result := TStmt.Create(AParser, stShowTableStatus);

  with PShowTableStatusStmt(AParser.NodePtr(Result))^ do
  begin
    Nodes := ANodes;

    Heritage.Heritage.AddChildren(@Nodes, SizeOf(Nodes) div SizeOf(TOffset));
  end;
end;

{ TMySQLParser.TShowTablesStmt ************************************************}

class function TMySQLParser.TShowTablesStmt.Create(const AParser: TMySQLParser; const ANodes: TNodes): TOffset;
begin
  Result := TStmt.Create(AParser, stShowTables);

  with PShowTablesStmt(AParser.NodePtr(Result))^ do
  begin
    Nodes := ANodes;

    Heritage.Heritage.AddChildren(@Nodes, SizeOf(Nodes) div SizeOf(TOffset));
  end;
end;

{ TMySQLParser.TShowTriggersStmt **********************************************}

class function TMySQLParser.TShowTriggersStmt.Create(const AParser: TMySQLParser; const ANodes: TNodes): TOffset;
begin
  Result := TStmt.Create(AParser, stShowTriggers);

  with PShowTriggersStmt(AParser.NodePtr(Result))^ do
  begin
    Nodes := ANodes;

    Heritage.Heritage.AddChildren(@Nodes, SizeOf(Nodes) div SizeOf(TOffset));
  end;
end;

{ TMySQLParser.TShowVariablesStmt *********************************************}

class function TMySQLParser.TShowVariablesStmt.Create(const AParser: TMySQLParser; const ANodes: TNodes): TOffset;
begin
  Result := TStmt.Create(AParser, stShowVariables);

  with PShowVariablesStmt(AParser.NodePtr(Result))^ do
  begin
    Nodes := ANodes;

    Heritage.Heritage.AddChildren(@Nodes, SizeOf(Nodes) div SizeOf(TOffset));
  end;
end;

{ TMySQLParser.TShowWarningsStmt **********************************************}

class function TMySQLParser.TShowWarningsStmt.Create(const AParser: TMySQLParser; const ANodes: TNodes): TOffset;
begin
  Result := TStmt.Create(AParser, stShowWarnings);

  with PShowWarningsStmt(AParser.NodePtr(Result))^ do
  begin
    Nodes := ANodes;

    Heritage.Heritage.AddChildren(@Nodes, SizeOf(Nodes) div SizeOf(TOffset));
  end;
end;

{ TMySQLParser.TShutdownStmt **************************************************}

class function TMySQLParser.TShutdownStmt.Create(const AParser: TMySQLParser; const ANodes: TNodes): TOffset;
begin
  Result := TStmt.Create(AParser, stShutdown);

  with PShutdownStmt(AParser.NodePtr(Result))^ do
  begin
    Nodes := ANodes;

    Heritage.Heritage.AddChildren(@Nodes, SizeOf(Nodes) div SizeOf(TOffset));
  end;
end;

{ TMySQLParser.TSignalStmt ****************************************************}

class function TMySQLParser.TSignalStmt.Create(const AParser: TMySQLParser; const ANodes: TNodes): TOffset;
begin
  Result := TStmt.Create(AParser, stSignal);

  with PSignalStmt(AParser.NodePtr(Result))^ do
  begin
    Nodes := ANodes;

    Heritage.Heritage.AddChildren(@Nodes, SizeOf(Nodes) div SizeOf(TOffset));
  end;
end;

{ TMySQLParser.TSignalStmt.TInformation ***************************************}

class function TMySQLParser.TSignalStmt.TInformation.Create(const AParser: TMySQLParser; const ANodes: TInformation.TNodes): TOffset;
begin
  Result := TRange.Create(AParser, ntSignalStmtInformation);

  with PInformation(AParser.NodePtr(Result))^ do
  begin
    Nodes := ANodes;

    Heritage.AddChildren(@Nodes, SizeOf(Nodes) div SizeOf(TOffset));
  end;
end;

{ TMySQLParser.TSubstringFunc *************************************************}

class function TMySQLParser.TSubstringFunc.Create(const AParser: TMySQLParser; const ANodes: TNodes): TOffset;
begin
  Result := TRange.Create(AParser, ntSubstringFunc);

  with PSubstringFunc(AParser.NodePtr(Result))^ do
  begin
    Nodes := ANodes;

    Heritage.AddChildren(@Nodes, SizeOf(Nodes) div SizeOf(TOffset));
  end;
end;

{ TMySQLParser.TSoundsLikeOp **************************************************}

class function TMySQLParser.TSoundsLikeOp.Create(const AParser: TMySQLParser; const AOperand1, ASounds, ALike, AOperand2: TOffset): TOffset;
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

{ TMySQLParser.TStartSlaveStmt ************************************************}

class function TMySQLParser.TStartSlaveStmt.Create(const AParser: TMySQLParser; const ANodes: TNodes): TOffset;
begin
  Result := TStmt.Create(AParser, stStartSlave);

  with PStartSlaveStmt(AParser.NodePtr(Result))^ do
  begin
    Nodes := ANodes;

    Heritage.Heritage.AddChildren(@Nodes, SizeOf(Nodes) div SizeOf(TOffset));
  end;
end;

{ TMySQLParser.TStartTransactionStmt ******************************************}

class function TMySQLParser.TStartTransactionStmt.Create(const AParser: TMySQLParser; const ANodes: TNodes): TOffset;
begin
  Result := TStmt.Create(AParser, stStartTransaction);

  with PStartTransactionStmt(AParser.NodePtr(Result))^ do
  begin
    Nodes := ANodes;

    Heritage.Heritage.AddChildren(@Nodes, SizeOf(Nodes) div SizeOf(TOffset));
  end;
end;

{ TMySQLParser.TStopSlaveStmt *************************************************}

class function TMySQLParser.TStopSlaveStmt.Create(const AParser: TMySQLParser; const ANodes: TNodes): TOffset;
begin
  Result := TStmt.Create(AParser, stStopSlave);

  with PStopSlaveStmt(AParser.NodePtr(Result))^ do
  begin
    Nodes := ANodes;

    Heritage.Heritage.AddChildren(@Nodes, SizeOf(Nodes) div SizeOf(TOffset));
  end;
end;

{ TMySQLParser.TSubArea *******************************************************}

class function TMySQLParser.TSubArea.Create(const AParser: TMySQLParser; const ANodes: TNodes): TOffset;
begin
  Result := TRange.Create(AParser, ntSubArea);

  with PSubArea(AParser.NodePtr(Result))^ do
  begin
    Nodes := ANodes;

    Heritage.AddChildren(@Nodes, SizeOf(Nodes) div SizeOf(TOffset));
  end;
end;

{ TMySQLParser.TSubAreaSelectStmt *********************************************}

class function TMySQLParser.TSubAreaSelectStmt.Create(const AParser: TMySQLParser; const ANodes: TNodes): TOffset;
begin
  Result := TStmt.Create(AParser, stSubAreaSelect);

  with PSubAreaSelectStmt(AParser.NodePtr(Result))^ do
  begin
    Nodes := ANodes;

    Heritage.Heritage.AddChildren(@Nodes, SizeOf(Nodes) div SizeOf(TOffset));
  end;
end;

{ TMySQLParser.TSubPartition **************************************************}

class function TMySQLParser.TSubPartition.Create(const AParser: TMySQLParser; const ANodes: TNodes): TOffset;
begin
  Result := TRange.Create(AParser, ntSubPartition);

  with PSubPartition(AParser.NodePtr(Result))^ do
  begin
    Nodes := ANodes;

    Heritage.AddChildren(@Nodes, SizeOf(Nodes) div SizeOf(TOffset));
  end;
end;

{ TMySQLParser.TTag ***********************************************************}

class function TMySQLParser.TTag.Create(const AParser: TMySQLParser; const ANodes: TNodes): TOffset;
begin
  Result := TRange.Create(AParser, ntTag);

  with PTag(AParser.NodePtr(Result))^ do
  begin
    Nodes := ANodes;

    Heritage.AddChildren(@Nodes, SizeOf(Nodes) div SizeOf(TOffset));
  end;
end;

{ TMySQLParser.TTruncateStmt **************************************************}

class function TMySQLParser.TTruncateStmt.Create(const AParser: TMySQLParser; const ANodes: TNodes): TOffset;
begin
  Result := TStmt.Create(AParser, stTruncate);

  with PTruncateStmt(AParser.NodePtr(Result))^ do
  begin
    Nodes := ANodes;

    Heritage.Heritage.AddChildren(@Nodes, SizeOf(Nodes) div SizeOf(TOffset));
  end;
end;

{ TMySQLParser.TTrimFunc ******************************************************}

class function TMySQLParser.TTrimFunc.Create(const AParser: TMySQLParser; const ANodes: TNodes): TOffset;
begin
  Result := TRange.Create(AParser, ntTrimFunc);

  with PTrimFunc(AParser.NodePtr(Result))^ do
  begin
    Nodes := ANodes;

    Heritage.AddChildren(@Nodes, SizeOf(Nodes) div SizeOf(TOffset));
  end;
end;

{ TMySQLParser.TUnaryOp *******************************************************}

class function TMySQLParser.TUnaryOp.Create(const AParser: TMySQLParser; const AOperator, AOperand: TOffset): TOffset;
begin
  Result := TRange.Create(AParser, ntUnaryOp);

  with PUnaryOp(AParser.NodePtr(Result))^ do
  begin
    Nodes.Operator := AOperator;
    Nodes.Operand := AOperand;

    Heritage.AddChildren(@Nodes, SizeOf(Nodes) div SizeOf(TOffset));
  end;
end;

{ TMySQLParser.TUnknownStmt ***************************************************}

class function TMySQLParser.TUnknownStmt.Create(const AParser: TMySQLParser; const ATokenCount: Integer; const ATokens: array of TOffset): TOffset;
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

{ TMySQLParser.TUnlockStmt ****************************************************}

class function TMySQLParser.TUnlockStmt.Create(const AParser: TMySQLParser; const ANodes: TNodes): TOffset;
begin
  Result := TStmt.Create(AParser, stUnlock);

  with PUnlockStmt(AParser.NodePtr(Result))^ do
  begin
    Nodes := ANodes;

    Heritage.Heritage.AddChildren(@Nodes, SizeOf(Nodes) div SizeOf(TOffset));
  end;
end;

{ TMySQLParser.TUpdateStmt ****************************************************}

class function TMySQLParser.TUpdateStmt.Create(const AParser: TMySQLParser; const ANodes: TNodes): TOffset;
begin
  Result := TStmt.Create(AParser, stUpdate);

  with PUpdateStmt(AParser.NodePtr(Result))^ do
  begin
    Nodes := ANodes;

    Heritage.Heritage.AddChildren(@Nodes, SizeOf(Nodes) div SizeOf(TOffset));
  end;
end;

{ TMySQLParser.TUser **********************************************************}

class function TMySQLParser.TUser.Create(const AParser: TMySQLParser; const ANodes: TNodes): TOffset;
begin
  Result := TRange.Create(AParser, ntUser);

  with PUser(AParser.NodePtr(Result))^ do
  begin
    Nodes := ANodes;

    Heritage.AddChildren(@Nodes, SizeOf(Nodes) div SizeOf(TOffset));
  end;
end;

{ TMySQLParser.TUseStmt *******************************************************}

class function TMySQLParser.TUseStmt.Create(const AParser: TMySQLParser; const ANodes: TNodes): TOffset;
begin
  Result := TStmt.Create(AParser, stUse);

  with PUseStmt(AParser.NodePtr(Result))^ do
  begin
    Nodes := ANodes;

    Heritage.Heritage.AddChildren(@Nodes, SizeOf(Nodes) div SizeOf(TOffset));
  end;
end;

{ TMySQLParser.TValue *********************************************************}

class function TMySQLParser.TValue.Create(const AParser: TMySQLParser; const ANodes: TNodes): TOffset;
begin
  Result := TRange.Create(AParser, ntValue);

  with PValue(AParser.NodePtr(Result))^ do
  begin
    Nodes := ANodes;

    Heritage.AddChildren(@Nodes, SizeOf(Nodes) div SizeOf(TOffset));
  end;
end;

{ TMySQLParser.TVariable ******************************************************}

class function TMySQLParser.TVariable.Create(const AParser: TMySQLParser; const ANodes: TNodes): TOffset;
begin
  Result := TRange.Create(AParser, ntVariable);

  with PVariable(AParser.NodePtr(Result))^ do
  begin
    Nodes := ANodes;

    Heritage.AddChildren(@Nodes, SizeOf(Nodes) div SizeOf(TOffset));
  end;
end;

{ TMySQLParser.TWeightStringFunc **********************************************}

class function TMySQLParser.TWeightStringFunc.Create(const AParser: TMySQLParser; const ANodes: TNodes): TOffset;
begin
  Result := TRange.Create(AParser, ntWeightStringFunc);

  with PWeightStringFunc(AParser.NodePtr(Result))^ do
  begin
    Nodes := ANodes;

    Heritage.AddChildren(@Nodes, SizeOf(Nodes) div SizeOf(TOffset));
  end;
end;

{ TMySQLParser.TWeightStringFunc.TLevel ***************************************}

class function TMySQLParser.TWeightStringFunc.TLevel.Create(const AParser: TMySQLParser; const ANodes: TLevel.TNodes): TOffset;
begin
  Result := TRange.Create(AParser, ntWeightStringFuncLevel);

  with TWeightStringFunc.PLevel(AParser.NodePtr(Result))^ do
  begin
    Nodes := ANodes;

    Heritage.AddChildren(@Nodes, SizeOf(Nodes) div SizeOf(TOffset));
  end;
end;

{ TMySQLParser.TWhileStmt *****************************************************}

class function TMySQLParser.TWhileStmt.Create(const AParser: TMySQLParser; const ANodes: TNodes): TOffset;
begin
  Result := TStmt.Create(AParser, stWhile);

  with PWhileStmt(AParser.NodePtr(Result))^ do
  begin
    Nodes := ANodes;

    Heritage.Heritage.AddChildren(@Nodes, SizeOf(Nodes) div SizeOf(TOffset));
  end;
end;

{ TMySQLParser.TXAStmt ********************************************************}

class function TMySQLParser.TXAStmt.Create(const AParser: TMySQLParser; const ANodes: TNodes): TOffset;
begin
  Result := TStmt.Create(AParser, stXA);

  with PXAStmt(AParser.NodePtr(Result))^ do
  begin
    Nodes := ANodes;

    Heritage.Heritage.AddChildren(@Nodes, SizeOf(Nodes) div SizeOf(TOffset));
  end;
end;

{ TMySQLParser.TXAStmt.TID ****************************************************}

class function TMySQLParser.TXAStmt.TID.Create(const AParser: TMySQLParser; const ANodes: TID.TNodes): TOffset;
begin
  Result := TRange.Create(AParser, ntXAStmtID);

  with PID(AParser.NodePtr(Result))^ do
  begin
    Nodes := ANodes;

    Heritage.AddChildren(@Nodes, SizeOf(Nodes) div SizeOf(TOffset));
  end;
end;

{ TMySQLParser ****************************************************************}

function TMySQLParser.ApplyCurrentToken(): TOffset;
begin
  Result := ApplyCurrentToken(utUnknown);
end;

function TMySQLParser.ApplyCurrentToken(const AUsageType: TUsageType): TOffset;
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
  end;
end;

procedure TMySQLParser.BeginPL_SQL();
begin
  Inc(FInPL_SQL);
end;

function TMySQLParser.ChildPtr(const ANode: TOffset): PChild;
begin
  if (not IsChild(NodePtr(ANode))) then
    Result := nil
  else
    Result := @ParsedNodes.Mem[ANode];
end;

procedure TMySQLParser.Clear();
begin
  FErrorCode := PE_Success;
  FErrorLine := 1;
  FErrorToken := 0;
  {$IFDEF Debug} TokenIndex := 0; {$ENDIF}
  FInPL_SQL := 0;
  if (ParsedNodes.MemSize <> DefaultParsedNodesMemSize) then
  begin
    ParsedNodes.MemSize := DefaultParsedNodesMemSize;
    ReallocMem(ParsedNodes.Mem, ParsedNodes.MemSize);
  end;
  ParsedNodes.UsedSize := 1; // "0" means "not assigned", so we start with "1"
  if (Texts.MemSize <> DefaultTextsMemSize) then
  begin
    Texts.MemSize := DefaultTextsMemSize;
    ReallocMem(Texts.Mem, Texts.MemSize);
  end;
  Texts.UsedSize := 1; // "0" means "not assigned", so we start with "1"
  ParseText := '';
  FRoot := 0;
  InCreateFunctionStmt := False;
  InCreateProcedureStmt := False;
  AllowedMySQLVersion := 0;
  TokenBuffer.Count := 0;
end;

constructor TMySQLParser.Create(const AMySQLVersion: Integer = 0);
begin
  inherited Create();

  AllowedMySQLVersion := 0;
  Commands := nil;
  DatatypeList := TWordList.Create(Self);
  FAnsiQuotes := False;
  FunctionList := TWordList.Create(Self);
  TokenIndex := 0;
  KeywordList := TWordList.Create(Self);
  FMySQLVersion := AMySQLVersion;
  ParsedNodes.Mem := nil;
  ParsedNodes.UsedSize := 0;
  ParsedNodes.MemSize := 0;
  Texts.Mem := nil;
  Texts.UsedSize := 0;
  Texts.MemSize := 0;
  TokenBuffer.Count := 0;

  Datatypes := MySQLDatatypes;
  Functions := MySQLFunctions;
  Keywords := MySQLKeywords;

  Clear();
end;

destructor TMySQLParser.Destroy();
begin
  Clear();

  if (ParsedNodes.MemSize <> 0) then
    FreeMem(ParsedNodes.Mem);
  if (Texts.MemSize <> 0) then
    FreeMem(Texts.Mem, 0);

  DatatypeList.Free();
  FunctionList.Free();
  KeywordList.Free();

  inherited;
end;

function TMySQLParser.EndOfStmt(const Token: PToken): Boolean;
begin
  Result := Token^.TokenType = ttDelimiter;
end;

function TMySQLParser.EndOfStmt(const Token: TOffset): Boolean;
begin
  Result := (Token = 0) or (TokenPtr(Token)^.TokenType = ttDelimiter);
end;

procedure TMySQLParser.EndPL_SQL();
begin
  Assert(FInPL_SQL > 0);

  if (FInPL_SQL > 0) then
    Dec(FInPL_SQL);
end;

procedure TMySQLParser.FormatAlterDatabaseStmt(const Nodes: TAlterDatabaseStmt.TNodes);
begin
  FormatNode(Nodes.StmtTag);
  FormatNode(Nodes.IdentTag, stSpaceBefore);

  if (Nodes.UpgradeDataDirectoryNameTag = 0) then
  begin
    FormatNode(Nodes.CharacterSetValue, stSpaceBefore);
    FormatNode(Nodes.CollateValue, stSpaceBefore);
  end
  else
    FormatNode(Nodes.UpgradeDataDirectoryNameTag, stSpaceBefore);
end;

procedure TMySQLParser.FormatAlterEventStmt(const Nodes: TAlterEventStmt.TNodes);
begin
  FormatNode(Nodes.AlterTag);
  FormatNode(Nodes.DefinerNode, stSpaceBefore);
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

  if (Nodes.OnCompletitionTag > 0) then
  begin
    Commands.IncreaseIndent();
    FormatNode(Nodes.OnCompletitionTag, stReturnBefore);
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

procedure TMySQLParser.FormatAlterRoutineStmt(const Nodes: TAlterRoutineStmt.TNodes);
var
  Child: PChild;
begin
  Assert((Nodes.CharacteristicList = 0) or (NodePtr(Nodes.CharacteristicList)^.NodeType = ntList));

  FormatNode(Nodes.AlterTag);
  FormatNode(Nodes.Ident, stSpaceBefore);

  if (Nodes.CharacteristicList > 0) then
  begin
    Commands.IncreaseIndent();

    Child := PList(NodePtr(Nodes.CharacteristicList))^.FirstChild;
    while (Assigned(Child)) do
    begin
      FormatNode(PNode(Child), stReturnBefore);

      Child := Child^.NextSibling;
    end;

    Commands.DecreaseIndent();
  end;
end;

procedure TMySQLParser.FormatAlterTableStmt(const Nodes: TAlterTableStmt.TNodes);
begin
  FormatNode(Nodes.AlterTag);
  FormatNode(Nodes.IgnoreTag, stSpaceBefore);
  FormatNode(Nodes.TableTag, stSpaceBefore);
  FormatNode(Nodes.Ident, stSpaceBefore);

  Commands.IncreaseIndent();

  if (Nodes.SpecificationList > 0) then
  begin
    Commands.WriteReturn();
    FormatList(Nodes.SpecificationList, sReturn);
  end;

  FormatNode(Nodes.AlgorithmValue, stReturnBefore);
  FormatNode(Nodes.ConvertToCharacterSetNode, stReturnBefore);
  FormatNode(Nodes.DiscardTablespaceTag, stReturnBefore);
  FormatNode(Nodes.EnableKeys, stReturnBefore);
  FormatNode(Nodes.ForceTag, stReturnBefore);
  FormatNode(Nodes.ImportTablespaceTag, stReturnBefore);
  FormatNode(Nodes.LockValue, stReturnBefore);
  FormatNode(Nodes.OrderByValue, stReturnBefore);
  FormatNode(Nodes.RenameNode, stReturnBefore);

  Commands.DecreaseIndent();
end;

procedure TMySQLParser.FormatAlterViewStmt(const Nodes: TAlterViewStmt.TNodes);
begin
  FormatNode(Nodes.AlterTag);
  Commands.IncreaseIndent();
  FormatNode(Nodes.AlgorithmValue, stReturnBefore);
  FormatNode(Nodes.DefinerNode, stReturnBefore);
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

procedure TMySQLParser.FormatBeginLabel(const Nodes: TBeginLabel.TNodes);
begin
  FormatNode(Nodes.BeginToken);
  FormatNode(Nodes.ColonToken);
end;

procedure TMySQLParser.FormatCallStmt(const Nodes: TCallStmt.TNodes);
begin
  FormatNode(Nodes.CallTag);
  FormatNode(Nodes.ProcedureIdent, stSpaceBefore);
  FormatNode(Nodes.ParamList);
end;

procedure TMySQLParser.FormatCaseOp(const Nodes: TCaseOp.TNodes);
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

procedure TMySQLParser.FormatCaseStmt(const Nodes: TCaseStmt.TNodes);
begin
  FormatNode(Nodes.CaseTag);
  FormatNode(Nodes.CompareExpr, stSpaceBefore);
  Commands.IncreaseIndent();
  Commands.WriteReturn();
  FormatList(Nodes.BranchList, sReturn);
  Commands.DecreaseIndent();
  FormatNode(Nodes.EndTag, stReturnBefore);
end;

procedure TMySQLParser.FormatCaseStmtBranch(const Nodes: TCaseStmt.TBranch.TNodes);
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

procedure TMySQLParser.FormatCastFunc(const Nodes: TCastFunc.TNodes);
begin
  FormatNode(Nodes.FuncToken);
  FormatNode(Nodes.OpenBracket);
  FormatNode(Nodes.Expr);
  FormatNode(Nodes.AsTag, stSpaceBefore);
  FormatNode(Nodes.Datatype, stSpaceBefore);
  FormatNode(Nodes.CloseBracket);
end;

procedure TMySQLParser.FormatCharFunc(const Nodes: TCharFunc.TNodes);
begin
  FormatNode(Nodes.FuncToken);
  FormatNode(Nodes.OpenBracket, stSpaceBefore);
  FormatNode(Nodes.ValueExpr, stSpaceBefore);
  if (Nodes.UsingTag > 0) then
  begin
    FormatNode(Nodes.UsingTag, stSpaceBefore);
    FormatNode(Nodes.CharsetIdent, stSpaceBefore);
  end;
  FormatNode(Nodes.CloseBracket, stSpaceBefore);
end;

procedure TMySQLParser.FormatCompoundStmt(const Nodes: TCompoundStmt.TNodes);
begin
  FormatNode(Nodes.BeginLabel, stSpaceAfter);
  Commands.IncreaseIndent();
  FormatNode(Nodes.BeginTag);
  if (Nodes.StmtList > 0) then
  begin
    Commands.WriteReturn();
    FormatList(Nodes.StmtList, sReturn);
  end;
  Commands.DecreaseIndent();
  FormatNode(Nodes.EndTag, stReturnBefore);
  FormatNode(Nodes.EndLabel, stSpaceBefore);
end;

procedure TMySQLParser.FormatConvertFunc(const Nodes: TConvertFunc.TNodes);
begin
  FormatNode(Nodes.FuncToken);
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

procedure TMySQLParser.FormatCreateEventStmt(const Nodes: TCreateEventStmt.TNodes);
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
  Commands.IncreaseIndent();
  FormatNode(Nodes.Body, stReturnBefore);
  Commands.DecreaseIndent();
  Commands.DecreaseIndent();
end;

procedure TMySQLParser.FormatCreateIndexStmt(const Nodes: TCreateIndexStmt.TNodes);
begin
  FormatNode(Nodes.CreateTag);
  FormatNode(Nodes.IndexTag, stSpaceBefore);
  FormatNode(Nodes.IndexIdent, stSpaceBefore);
  Commands.IncreaseIndent();
  FormatNode(Nodes.IndexTypeValue, stReturnBefore);
  FormatNode(Nodes.OnTag, stReturnBefore);
  FormatNode(Nodes.TableIdent, stSpaceBefore);
  FormatNode(Nodes.KeyColumnList, stSpaceBefore);
  FormatNode(Nodes.AlgorithmValue, stReturnBefore);
  FormatNode(Nodes.CommentValue, stReturnBefore);
  FormatNode(Nodes.KeyBlockSizeValue, stReturnBefore);
  FormatNode(Nodes.LockValue, stReturnBefore);
  FormatNode(Nodes.ParserValue, stReturnBefore);
  Commands.DecreaseIndent();
end;

procedure TMySQLParser.FormatCreateRoutineStmt(const Nodes: TCreateRoutineStmt.TNodes);
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
  if (Nodes.CharacteristicList > 0) then
  begin
    Commands.IncreaseIndent();
    Commands.WriteReturn();
    FormatList(Nodes.CharacteristicList, sReturn);
    Commands.DecreaseIndent();
  end;
  FormatNode(Nodes.Body, stReturnBefore);
end;

procedure TMySQLParser.FormatCreateServerStmt(const Nodes: TCreateServerStmt.TNodes);
begin
  FormatNode(Nodes.CreateTag);
  FormatNode(Nodes.ServerTag, stSpaceBefore);
  FormatNode(Nodes.Ident, stSpaceBefore);
  Commands.IncreaseIndent();
  FormatNode(Nodes.ForeignDataWrapperValue, stReturnBefore);
  if ((Nodes.Options.List > 0) and (PList(NodePtr(Nodes.Options.List))^.ElementCount > 0)) then
  begin
    FormatNode(Nodes.Options.Tag, stReturnBefore);
    FormatNode(Nodes.Options.List, stSpaceBefore);
  end;
  Commands.DecreaseIndent();
end;

procedure TMySQLParser.FormatCreateTableStmt(const Nodes: TCreateTableStmt.TNodes);
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
  FormatNode(Nodes.TableOptionsNodes.PageChecksum, stSpaceBefore);
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

procedure TMySQLParser.FormatCreateTableStmtField(const Nodes: TCreateTableStmt.TField.TNodes);
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

procedure TMySQLParser.FormatCreateTableStmtFieldDefaultFunc(const Nodes: TCreateTableStmt.TField.TDefaultFunc.TNodes);
begin
  FormatNode(Nodes.FuncToken);
  FormatNode(Nodes.OpenBracket);
  FormatNode(Nodes.CloseBracket);
end;

procedure TMySQLParser.FormatCreateTableStmtKey(const Nodes: TCreateTableStmt.TKey.TNodes);
begin
  FormatNode(Nodes.AddTag, stSpaceAfter);
  FormatNode(Nodes.ConstraintTag, stSpaceAfter);
  FormatNode(Nodes.SymbolIdent, stSpaceAfter);
  FormatNode(Nodes.KeyTag);
  FormatNode(Nodes.KeyIdent, stSpaceBefore);
  FormatNode(Nodes.IndexTypeTag, stSpaceBefore);
  Commands.WriteSpace();
  FormatList(Nodes.ColumnIdentList, sNone);
  FormatNode(Nodes.KeyBlockSizeValue, stSpaceBefore);
  FormatNode(Nodes.ParserValue, stSpaceBefore);
  FormatNode(Nodes.CommentValue, stSpaceBefore);
end;

procedure TMySQLParser.FormatCreateTableStmtKeyColumn(const Nodes: TCreateTableStmt.TKeyColumn.TNodes);
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

procedure TMySQLParser.FormatCreateTableStmtPartition(const Nodes: TCreateTableStmt.TPartition.TNodes);
begin
  FormatNode(Nodes.AddTag, stSpaceAfter);
  FormatNode(Nodes.PartitionTag);
  FormatNode(Nodes.NameIdent, stSpaceBefore);
  Commands.IncreaseIndent();
  FormatNode(Nodes.ValuesNode, stSpaceBefore);
  FormatNode(Nodes.EngineValue, stSpaceBefore);
  FormatNode(Nodes.CommentValue, stSpaceBefore);
  FormatNode(Nodes.DataDirectoryValue, stSpaceBefore);
  FormatNode(Nodes.IndexDirectoryValue, stSpaceBefore);
  FormatNode(Nodes.MaxRowsValue, stSpaceBefore);
  FormatNode(Nodes.MinRowsValue, stSpaceBefore);
  FormatNode(Nodes.SubPartitionList, stSpaceBefore);
  Commands.DecreaseIndent();
end;

procedure TMySQLParser.FormatCreateTableStmtPartitionValues(const Nodes: TCreateTableStmt.TPartitionValues.TNodes);
begin
  FormatNode(Nodes.ValuesTag);
  FormatNode(Nodes.Value, stSpaceBefore);
end;

procedure TMySQLParser.FormatCreateTableStmtReference(const Nodes: TCreateTableStmt.TReference.TNodes);
begin
  FormatNode(Nodes.Tag);
  FormatNode(Nodes.ParentTableIdent, stSpaceBefore);
  FormatNode(Nodes.IndicesList, stSpaceBefore);
  FormatNode(Nodes.MatchValue, stSpaceBefore);
  FormatNode(Nodes.OnDeleteValue, stSpaceBefore);
  FormatNode(Nodes.OnUpdateValue, stSpaceBefore);
end;

procedure TMySQLParser.FormatCreateTriggerStmt(const Nodes: TCreateTriggerStmt.TNodes);
begin
  FormatNode(Nodes.CreateTag);
  Commands.IncreaseIndent();
  FormatNode(Nodes.DefinerNode, stReturnBefore);
  FormatNode(Nodes.TriggerTag, stReturnBefore);
  FormatNode(Nodes.TriggerIdent, stSpaceBefore);
  FormatNode(Nodes.ActionValue, stReturnBefore);
  FormatNode(Nodes.OnTag, stReturnBefore);
  FormatNode(Nodes.TableIdentNode, stSpaceBefore);
  FormatNode(Nodes.ForEachRowTag, stSpaceBefore);
  FormatNode(Nodes.OrderValue, stReturnBefore);
  Commands.DecreaseIndent();
  FormatNode(Nodes.Body, stReturnBefore);
end;

procedure TMySQLParser.FormatCreateUserStmt(const Nodes: TCreateUserStmt.TNodes);
begin
  FormatNode(Nodes.CreateTag);
  FormatNode(Nodes.IfTag, stSpaceBefore);
  Commands.IncreaseIndent();
  FormatNode(Nodes.UserSpecifications, stReturnBefore);

  if (Nodes.WithTag > 0) then
  begin
    FormatNode(Nodes.WithTag, stReturnBefore);
    Commands.WriteSpace();
    FormatList(Nodes.ResourcesList, sSpace);
  end;

  if (Nodes.PasswordOption > 0) then
  begin
    FormatNode(Nodes.PasswordOption, stReturnBefore);
    FormatNode(Nodes.PasswordDays, stSpaceBefore);
    FormatNode(Nodes.DayTag, stSpaceBefore);
  end
  else if (Nodes.AccountTag > 0) then
    FormatNode(Nodes.AccountTag, stReturnBefore);

  Commands.DecreaseIndent();
end;

procedure TMySQLParser.FormatCreateViewStmt(const Nodes: TCreateViewStmt.TNodes);
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
    FormatNode(Nodes.ColumnList, stSpaceBefore);
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
    FormatNode(Nodes.ColumnList, stSpaceBefore);
    Commands.IncreaseIndent();
    FormatNode(Nodes.AsTag, stReturnBefore);
    Commands.IncreaseIndent();
    FormatNode(Nodes.SelectStmt, stReturnBefore);
    Commands.DecreaseIndent();
    FormatNode(Nodes.OptionTag, stReturnBefore);
    Commands.DecreaseIndent();
    Commands.DecreaseIndent();
  end;
end;

procedure TMySQLParser.FormatCurrentTimestamp(const Nodes: TCurrentTimestamp.TNodes);
begin
  FormatNode(Nodes.CurrentTimestampTag);
  FormatNode(Nodes.OpenBracket);
  FormatNode(Nodes.LengthInteger);
  FormatNode(Nodes.CloseBracket);
end;

procedure TMySQLParser.FormatComments(const Token: PToken; Start: Boolean = False);
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
          if (Start) then
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

procedure TMySQLParser.FormatDatatype(const Nodes: TDatatype.TNodes);
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
  FormatNode(Nodes.CharacterSetValue, stSpaceBefore);
  FormatNode(Nodes.BinaryToken, stSpaceBefore);
  FormatNode(Nodes.ASCIIToken, stSpaceBefore);
  FormatNode(Nodes.UnicodeToken, stSpaceBefore);
end;

procedure TMySQLParser.FormatDbIdent(const Nodes: TDbIdent.TNodes);
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

procedure TMySQLParser.FormatDeclareCursorStmt(const Nodes: TDeclareCursorStmt.TNodes);
begin
  FormatNode(Nodes.StmtTag);
  FormatNode(Nodes.Ident, stSpaceBefore);
  FormatNode(Nodes.CursorTag, stSpaceBefore);
  Commands.IncreaseIndent();
  FormatNode(Nodes.ForTag, stSpaceBefore);
  FormatNode(Nodes.SelectStmt, stReturnBefore);
  Commands.DecreaseIndent();
end;

procedure TMySQLParser.FormatDeclareHandlerStmt(const Nodes: TDeclareHandlerStmt.TNodes);
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

procedure TMySQLParser.FormatDeleteStmt(const Nodes: TDeleteStmt.TNodes);
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

procedure TMySQLParser.FormatExistsFunc(const Nodes: TExistsFunc.TNodes);
begin
  FormatNode(Nodes.FuncToken);
  FormatNode(Nodes.OpenBracket);
  FormatNode(Nodes.SubQuery);
  FormatNode(Nodes.CloseBracket);
end;

procedure TMySQLParser.FormatExtractFunc(const Nodes: TExtractFunc.TNodes);
begin
  FormatNode(Nodes.FuncToken);
  FormatNode(Nodes.OpenBracket);
  FormatNode(Nodes.UnitTag);
  FormatNode(Nodes.FromTag, stSpaceBefore);
  FormatNode(Nodes.DateExpr, stSpaceBefore);
  FormatNode(Nodes.CloseBracket);
end;

procedure TMySQLParser.FormatFunctionCall(const Nodes: TFunctionCall.TNodes);
begin
  FormatNode(Nodes.Ident);
  FormatNode(Nodes.ArgumentsList);
end;

procedure TMySQLParser.FormatGroupConcatFunc(const Nodes: TGroupConcatFunc.TNodes);
begin
  FormatNode(Nodes.FuncToken);
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

procedure TMySQLParser.FormatIfStmt(const Nodes: TIfStmt.TNodes);
begin
  FormatNode(Nodes.BranchList);
  FormatNode(Nodes.EndTag, stReturnBefore);
end;

procedure TMySQLParser.FormatIfStmtBranch(const Nodes: TIfStmt.TBranch.TNodes);
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

procedure TMySQLParser.FormatInsertStmt(const Nodes: TInsertStmt.TNodes);
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
    FormatNode(Nodes.ColumnList, stSpaceBefore);
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

procedure TMySQLParser.FormatList(const Nodes: TList.TNodes; const DelimiterType: TTokenType);
begin
  case (DelimiterType) of
    ttComma: FormatList(Nodes, sSpace);
    ttDot: FormatList(Nodes, sNone);
    ttDelimiter: FormatList(Nodes, sReturn);
    else FormatList(Nodes, sSpace);
  end;
end;

procedure TMySQLParser.FormatList(const Nodes: TList.TNodes; const Spacer: TSpacer);
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

procedure TMySQLParser.FormatList(const Node: TOffset; const Spacer: TSpacer);
begin
  if (Node > 0) then
  begin
    Assert(NodePtr(Node)^.NodeType = ntList);

    FormatList(PList(NodePtr(Node))^.Nodes, Spacer);
  end;
end;

procedure TMySQLParser.FormatLoadDataStmt(const Nodes: TLoadDataStmt.TNodes);
begin
  FormatNode(Nodes.LoadDataTag);
  FormatNode(Nodes.PriorityTag, stSpaceBefore);
  FormatNode(Nodes.InfileTag, stSpaceBefore);
  FormatNode(Nodes.FilenameString, stSpaceBefore);
  Commands.IncreaseIndent();
  FormatNode(Nodes.ReplaceIgnoreTag, stReturnBefore);
  FormatNode(Nodes.IntoTableValue, stReturnBefore);
  FormatNode(Nodes.PartitionValue, stReturnBefore);
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
  FormatNode(Nodes.IgnoreLines, stReturnBefore);
  FormatNode(Nodes.ColumnList, stReturnBefore);
  FormatNode(Nodes.SetList, stReturnBefore);
  Commands.DecreaseIndent();
end;

procedure TMySQLParser.FormatLoadXMLStmt(const Nodes: TLoadXMLStmt.TNodes);
begin
  FormatNode(Nodes.LoadXMLTag);
  FormatNode(Nodes.PriorityTag, stSpaceBefore);
  FormatNode(Nodes.InfileTag, stSpaceBefore);
  FormatNode(Nodes.FilenameString, stSpaceBefore);
  Commands.IncreaseIndent();
  FormatNode(Nodes.ReplaceIgnoreTag, stReturnBefore);
  FormatNode(Nodes.IntoTableValue, stReturnBefore);
  FormatNode(Nodes.CharacterSetValue, stReturnBefore);
  FormatNode(Nodes.RowsIdentifiedByValue, stReturnBefore);
  FormatNode(Nodes.IgnoreLines, stReturnBefore);
  FormatNode(Nodes.ColumnList, stReturnBefore);
  FormatNode(Nodes.SetList, stReturnBefore);
  Commands.DecreaseIndent();
end;

procedure TMySQLParser.FormatLoopStmt(const Nodes: TLoopStmt.TNodes);
begin
  FormatNode(Nodes.BeginLabel, stSpaceAfter);
  Commands.IncreaseIndent();
  FormatNode(Nodes.BeginTag);
  FormatNode(Nodes.StmtList, stReturnBefore);
  Commands.DecreaseIndent();
  FormatNode(Nodes.EndTag, stReturnBefore);
  FormatNode(Nodes.EndLabel, stSpaceBefore);
end;

procedure TMySQLParser.FormatMatchFunc(const Nodes: TMatchFunc.TNodes);
begin
  FormatNode(Nodes.FuncToken);
  FormatNode(Nodes.MatchList, stSpaceBefore);
  FormatNode(Nodes.AgainstToken, stSpaceBefore);
  FormatNode(Nodes.OpenBracket, stSpaceBefore);
  FormatNode(Nodes.Expr);
  FormatNode(Nodes.SearchModifierTag, stSpaceBefore);
  FormatNode(Nodes.CloseBracket);
end;

procedure TMySQLParser.FormatPositionFunc(const Nodes: TPositionFunc.TNodes);
begin
  FormatNode(Nodes.FuncToken);
  FormatNode(Nodes.OpenBracket);
  FormatNode(Nodes.SubStr);
  FormatNode(Nodes.InTag, stSpaceBefore);
  FormatNode(Nodes.Str, stSpaceBefore);
  FormatNode(Nodes.CloseBracket);
end;

procedure TMySQLParser.FormatNode(const Node: PNode; const Separator: TSeparatorType = stNone);

  procedure FormatDefaultNode(const Nodes: POffsetArray; const Size: Integer);
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

      ntAnalyzeStmt: FormatDefaultNode(@PAnalyzeStmt(Node)^.Nodes, SizeOf(TAnalyzeStmt.TNodes));
      ntAlterDatabaseStmt: FormatAlterDatabaseStmt(PAlterDatabaseStmt(Node)^.Nodes);
      ntAlterEventStmt: FormatAlterEventStmt(PAlterEventStmt(Node)^.Nodes);
      ntAlterInstanceStmt: FormatDefaultNode(@PAlterInstanceStmt(Node)^.Nodes, SizeOf(TAlterInstanceStmt.TNodes));
      ntAlterRoutineStmt: FormatAlterRoutineStmt(PAlterRoutineStmt(Node)^.Nodes);
      ntAlterServerStmt: FormatDefaultNode(@PAlterServerStmt(Node)^.Nodes, SizeOf(TAlterServerStmt.TNodes));
      ntAlterTableStmt: FormatAlterTableStmt(PAlterTableStmt(Node)^.Nodes);
      ntAlterTableStmtAlterColumn: FormatDefaultNode(@TAlterTableStmt.PAlterColumn(Node)^.Nodes, SizeOf(TAlterTableStmt.TAlterColumn.TNodes));
      ntAlterTableStmtConvertTo: FormatDefaultNode(@TAlterTableStmt.PConvertTo(Node)^.Nodes, SizeOf(TAlterTableStmt.TConvertTo.TNodes));
      ntAlterTableStmtDropObject: FormatDefaultNode(@TAlterTableStmt.PDropObject(Node)^.Nodes, SizeOf(TAlterTableStmt.TDropObject.TNodes));
      ntAlterTableStmtExchangePartition: FormatDefaultNode(@TAlterTableStmt.PExchangePartition(Node)^.Nodes, SizeOf(TAlterTableStmt.TExchangePartition.TNodes));
      ntAlterTableStmtReorganizePartition: FormatDefaultNode(@TAlterTableStmt.PReorganizePartition(Node)^.Nodes, SizeOf(TAlterTableStmt.TReorganizePartition.TNodes));
      ntAlterViewStmt: FormatAlterViewStmt(PAlterViewStmt(Node)^.Nodes);
      ntBeginLabel: FormatBeginLabel(PBeginLabel(Node)^.Nodes);
      ntBeginStmt: FormatDefaultNode(@PBeginStmt(Node)^.Nodes, SizeOf(TBeginStmt.TNodes));
      ntBetweenOp: FormatDefaultNode(@PBetweenOp(Node)^.Nodes, SizeOf(TBetweenOp.TNodes));
      ntBinaryOp: FormatDefaultNode(@PBinaryOp(Node)^.Nodes, SizeOf(TBinaryOp.TNodes));
      ntCallStmt: FormatCallStmt(PCallStmt(Node)^.Nodes);
      ntCaseOp: FormatCaseOp(PCaseOp(Node)^.Nodes);
      ntCaseOpBranch: FormatDefaultNode(@TCaseOp.PBranch(Node)^.Nodes, SizeOf(TCaseOp.TBranch.TNodes));
      ntCaseStmt: FormatCaseStmt(PCaseStmt(Node)^.Nodes);
      ntCaseStmtBranch: FormatCaseStmtBranch(TCaseStmt.PBranch(Node)^.Nodes);
      ntCastFunc: FormatCastFunc(PCastFunc(Node)^.Nodes);
      ntCharFunc: FormatCharFunc(PCharFunc(Node)^.Nodes);
      ntCheckStmt: FormatDefaultNode(@PCheckStmt(Node)^.Nodes, SizeOf(TCheckStmt.TNodes));
      ntCheckStmtOption: FormatDefaultNode(@TCheckStmt.POption(Node)^.Nodes, SizeOf(TCheckStmt.TOption.TNodes));
      ntChecksumStmt: FormatDefaultNode(@PChecksumStmt(Node)^.Nodes, SizeOf(TChecksumStmt.TNodes));
      ntCloseStmt: FormatDefaultNode(@PCloseStmt(Node)^.Nodes, SizeOf(TCloseStmt.TNodes));
      ntCommitStmt: FormatDefaultNode(@PCommitStmt(Node)^.Nodes, SizeOf(TCommitStmt.TNodes));
      ntCompoundStmt: FormatCompoundStmt(PCompoundStmt(Node)^.Nodes);
      ntConvertFunc: FormatConvertFunc(PConvertFunc(Node)^.Nodes);
      ntCreateDatabaseStmt: FormatDefaultNode(@PCreateDatabaseStmt(Node)^.Nodes, SizeOf(TCreateDatabaseStmt.TNodes));
      ntCreateEventStmt: FormatCreateEventStmt(PCreateEventStmt(Node)^.Nodes);
      ntCreateIndexStmt: FormatCreateIndexStmt(PCreateIndexStmt(Node)^.Nodes);
      ntCreateRoutineStmt: FormatCreateRoutineStmt(PCreateRoutineStmt(Node)^.Nodes);
      ntCreateServerStmt: FormatCreateServerStmt(PCreateServerStmt(Node)^.Nodes);
      ntCreateTableStmt: FormatCreateTableStmt(PCreateTableStmt(Node)^.Nodes);
      ntCreateTableStmtField: FormatCreateTableStmtField(TCreateTableStmt.PField(Node)^.Nodes);
      ntCreateTableStmtFieldDefaultFunc: FormatCreateTableStmtFieldDefaultFunc(TCreateTableStmt.TField.PDefaultFunc(Node)^.Nodes);
      ntCreateTableStmtForeignKey: FormatDefaultNode(@TCreateTableStmt.PForeignKey(Node)^.Nodes, SizeOf(TCreateTableStmt.TForeignKey.TNodes));
      ntCreateTableStmtKey: FormatCreateTableStmtKey(TCreateTableStmt.PKey(Node)^.Nodes);
      ntCreateTableStmtKeyColumn: FormatCreateTableStmtKeyColumn(TCreateTableStmt.PKeyColumn(Node)^.Nodes);
      ntCreateTableStmtPartition: FormatCreateTableStmtPartition(TCreateTableStmt.PPartition(Node)^.Nodes);
      ntCreateTableStmtPartitionValues: FormatCreateTableStmtPartitionValues(TCreateTableStmt.PPartitionValues(Node)^.Nodes);
      ntCreateTableStmtReference: FormatCreateTableStmtReference(TCreateTableStmt.PReference(Node)^.Nodes);
      ntCreateTriggerStmt: FormatCreateTriggerStmt(PCreateTriggerStmt(Node)^.Nodes);
      ntCreateUserStmt: FormatCreateUserStmt(PCreateUserStmt(Node)^.Nodes);
      ntCreateViewStmt: FormatCreateViewStmt(PCreateViewStmt(Node)^.Nodes);
      ntCurrentTimestamp: FormatCurrentTimestamp(PCurrentTimestamp(Node)^.Nodes);
      ntDatatype: FormatDatatype(PDatatype(Node)^.Nodes);
      ntDbIdent: FormatDbIdent(PDbIdent(Node)^.Nodes);
      ntDeallocatePrepareStmt: FormatDefaultNode(@PDeallocatePrepareStmt(Node)^.Nodes, SizeOf(TDeallocatePrepareStmt.TNodes));
      ntDeclareStmt: FormatDefaultNode(@PDeclareStmt(Node)^.Nodes, SizeOf(TDeclareStmt.TNodes));
      ntDeclareConditionStmt: FormatDefaultNode(@PDeclareConditionStmt(Node)^.Nodes, SizeOf(TDeclareConditionStmt.TNodes));
      ntDeclareCursorStmt: FormatDeclareCursorStmt(PDeclareCursorStmt(Node)^.Nodes);
      ntDeclareHandlerStmt: FormatDeclareHandlerStmt(PDeclareHandlerStmt(Node)^.Nodes);
      ntDeclareHandlerStmtCondition: FormatDefaultNode(@TDeclareHandlerStmt.PCondition(Node)^.Nodes, SizeOf(TDeclareHandlerStmt.TCondition.TNodes));
      ntDeleteStmt: FormatDeleteStmt(PDeleteStmt(Node)^.Nodes);
      ntDoStmt: FormatDefaultNode(@PDoStmt(Node)^.Nodes, SizeOf(TDoStmt.TNodes));
      ntDropDatabaseStmt: FormatDefaultNode(@PDropDatabaseStmt(Node)^.Nodes, SizeOf(TDropDatabaseStmt.TNodes));
      ntDropEventStmt: FormatDefaultNode(@PDropEventStmt(Node)^.Nodes, SizeOf(TDropEventStmt.TNodes));
      ntDropIndexStmt: FormatDefaultNode(@PDropIndexStmt(Node)^.Nodes, SizeOf(TDropIndexStmt.TNodes));
      ntDropRoutineStmt: FormatDefaultNode(@PDropRoutineStmt(Node)^.Nodes, SizeOf(TDropRoutineStmt.TNodes));
      ntDropServerStmt: FormatDefaultNode(@PDropServerStmt(Node)^.Nodes, SizeOf(TDropServerStmt.TNodes));
      ntDropTableStmt: FormatDefaultNode(@PDropTableStmt(Node)^.Nodes, SizeOf(TDropTableStmt.TNodes));
      ntDropTriggerStmt: FormatDefaultNode(@PDropTriggerStmt(Node)^.Nodes, SizeOf(TDropTriggerStmt.TNodes));
      ntDropUserStmt: FormatDefaultNode(@PDropUserStmt(Node)^.Nodes, SizeOf(TDropUserStmt.TNodes));
      ntDropViewStmt: FormatDefaultNode(@PDropViewStmt(Node)^.Nodes, SizeOf(TDropViewStmt.TNodes));
      ntEndLabel: FormatDefaultNode(@PEndLabel(Node)^.Nodes, SizeOf(TEndLabel.TNodes));
      ntExecuteStmt: FormatDefaultNode(@PExecuteStmt(Node)^.Nodes, SizeOf(TExecuteStmt.TNodes));
      ntExistsFunc: FormatExistsFunc(PExistsFunc(Node)^.Nodes);
      ntExplainStmt: FormatDefaultNode(@PExplainStmt(Node)^.Nodes, SizeOf(TExplainStmt.TNodes));
      ntExtractFunc: FormatExtractFunc(PExtractFunc(Node)^.Nodes);
      ntFetchStmt: FormatDefaultNode(@PFetchStmt(Node)^.Nodes, SizeOf(TFetchStmt.TNodes));
      ntFlushStmt: FormatDefaultNode(@PFlushStmt(Node)^.Nodes, SizeOf(TFlushStmt.TNodes));
      ntFlushStmtOption: FormatDefaultNode(@TFlushStmt.POption(Node)^.Nodes, SizeOf(TFlushStmt.TOption.TNodes));
      ntFunctionCall: FormatFunctionCall(PFunctionCall(Node)^.Nodes);
      ntFunctionReturns: FormatDefaultNode(@PFunctionReturns(Node)^.Nodes, SizeOf(TFunctionReturns.TNodes));
      ntGetDiagnosticsStmt: FormatDefaultNode(@PGetDiagnosticsStmt(Node)^.Nodes, SizeOf(TGetDiagnosticsStmt.TNodes));
      ntGetDiagnosticsStmtStmtInfo: FormatDefaultNode(@TGetDiagnosticsStmt.PStmtInfo(Node)^.Nodes, SizeOf(TGetDiagnosticsStmt.TStmtInfo.TNodes));
      ntGetDiagnosticsStmtCondInfo: FormatDefaultNode(@TGetDiagnosticsStmt.PCondInfo(Node)^.Nodes, SizeOf(TGetDiagnosticsStmt.TCondInfo.TNodes));
      ntGrantStmt: FormatDefaultNode(@PGrantStmt(Node)^.Nodes, SizeOf(TGrantStmt.TNodes));
      ntGrantStmtPrivileg: FormatDefaultNode(@TGrantStmt.PPrivileg(Node)^.Nodes, SizeOf(TGrantStmt.TPrivileg.TNodes));
      ntGrantStmtUserSpecification: FormatDefaultNode(@TGrantStmt.PUserSpecification(Node)^.Nodes, SizeOf(TGrantStmt.TUserSpecification.TNodes));
      ntGroupConcatFunc: FormatGroupConcatFunc(PGroupConcatFunc(Node)^.Nodes);
      ntGroupConcatFuncExpr: FormatDefaultNode(@TGroupConcatFunc.PExpr(Node)^.Nodes, SizeOf(TGroupConcatFunc.TExpr.TNodes));
      ntHelpStmt: FormatDefaultNode(@PHelpStmt(Node)^.Nodes, SizeOf(THelpStmt.TNodes));
      ntIfStmt: FormatIfStmt(PIfStmt(Node)^.Nodes);
      ntIfStmtBranch: FormatIfStmtBranch(TIfStmt.PBranch(Node)^.Nodes);
      ntIgnoreLines: FormatDefaultNode(@PIgnoreLines(Node)^.Nodes, SizeOf(TIgnoreLines.TNodes));
      ntInOp: FormatDefaultNode(@PInOp(Node)^.Nodes, SizeOf(TInOp.TNodes));
      ntInsertStmt: FormatInsertStmt(PInsertStmt(Node)^.Nodes);
      ntInsertStmtSetItem: FormatDefaultNode(@TInsertStmt.PSetItem(Node)^.Nodes, SizeOf(TInsertStmt.TSetItem.TNodes));
      ntIntervalOp: FormatDefaultNode(@PIntervalOp(Node)^.Nodes, SizeOf(TIntervalOp.TNodes));
      ntIntervalOpListItem: FormatDefaultNode(@TIntervalOp.PListItem(Node)^.Nodes, SizeOf(TIntervalOp.TListItem.TNodes));
      ntIterateStmt: FormatDefaultNode(@PIterateStmt(Node)^.Nodes, SizeOf(TIterateStmt.TNodes));
      ntKillStmt: FormatDefaultNode(@PKillStmt(Node)^.Nodes, SizeOf(TKillStmt.TNodes));
      ntLeaveStmt: FormatDefaultNode(@PLeaveStmt(Node)^.Nodes, SizeOf(TLeaveStmt.TNodes));
      ntLikeOp: FormatDefaultNode(@PLikeOp(Node)^.Nodes, SizeOf(TLikeOp.TNodes));
      ntList: FormatList(PList(Node)^.Nodes, PList(Node)^.DelimiterType);
      ntLoadDataStmt: FormatLoadDataStmt(PLoadDataStmt(Node)^.Nodes);
      ntLoadXMLStmt: FormatLoadXMLStmt(PLoadXMLStmt(Node)^.Nodes);
      ntLockStmt: FormatDefaultNode(@PLockStmt(Node)^.Nodes, SizeOf(TLockStmt.TNodes));
      ntLockStmtItem: FormatDefaultNode(@TLockStmt.PItem(Node)^.Nodes, SizeOf(TLockStmt.TItem.TNodes));
      ntLoopStmt: FormatLoopStmt(PLoopStmt(Node)^.Nodes);
      ntMatchFunc: FormatMatchFunc(PMatchFunc(Node)^.Nodes);
      ntPositionFunc: FormatPositionFunc(PPositionFunc(Node)^.Nodes);
      ntPrepareStmt: FormatDefaultNode(@PPrepareStmt(Node)^.Nodes, SizeOf(TPrepareStmt.TNodes));
      ntPurgeStmt: FormatDefaultNode(@PPurgeStmt(Node)^.Nodes, SizeOf(TPurgeStmt.TNodes));
      ntOpenStmt: FormatDefaultNode(@POpenStmt(Node)^.Nodes, SizeOf(TOpenStmt.TNodes));
      ntOptimizeStmt: FormatDefaultNode(@POptimizeStmt(Node)^.Nodes, SizeOf(TOptimizeStmt.TNodes));
      ntRegExpOp: FormatDefaultNode(@PRegExpOp(Node)^.Nodes, SizeOf(TRegExpOp.TNodes));
      ntRenameStmt: FormatDefaultNode(@PRenameStmt(Node)^.Nodes, SizeOf(TRenameStmt.TNodes));
      ntRenameStmtPair: FormatDefaultNode(@TRenameStmt.PPair(Node)^.Nodes, SizeOf(TRenameStmt.TPair.TNodes));
      ntReleaseStmt: FormatDefaultNode(@PReleaseStmt(Node)^.Nodes, SizeOf(TReleaseStmt.TNodes));
      ntRepairStmt: FormatDefaultNode(@PRepairStmt(Node)^.Nodes, SizeOf(TRepairStmt.TNodes));
      ntRepeatStmt: FormatRepeatStmt(PRepeatStmt(Node)^.Nodes);
      ntResetStmt: FormatDefaultNode(@PResetStmt(Node)^.Nodes, SizeOf(TResetStmt.TNodes));
      ntReturnStmt: FormatDefaultNode(@PReturnStmt(Node)^.Nodes, SizeOf(TReturnStmt.TNodes));
      ntRevokeStmt: FormatDefaultNode(@PRevokeStmt(Node)^.Nodes, SizeOf(TRevokeStmt.TNodes));
      ntRollbackStmt: FormatDefaultNode(@PRollbackStmt(Node)^.Nodes, SizeOf(TRollbackStmt.TNodes));
      ntRoutineParam: FormatDefaultNode(@PRoutineParam(Node)^.Nodes, SizeOf(TRoutineParam.TNodes));
      ntSavepointStmt: FormatDefaultNode(@PSavepointStmt(Node)^.Nodes, SizeOf(TSavepointStmt.TNodes));
      ntSchedule: FormatSchedule(PSchedule(Node)^.Nodes);
      ntSecretIdent: FormatSecretIdent(PSecretIdent(Node)^.Nodes);
      ntSelectStmt: FormatSelectStmt(PSelectStmt(Node)^.Nodes);
      ntSelectStmtColumn: FormatSelectStmtColumn(TSelectStmt.PColumn(Node)^.Nodes);
      ntSelectStmtGroup: FormatDefaultNode(@TSelectStmt.PGroup(Node)^.Nodes, SizeOf(TSelectStmt.TGroup.TNodes));
      ntSelectStmtOrder: FormatDefaultNode(@TSelectStmt.POrder(Node)^.Nodes, SizeOf(TSelectStmt.TOrder.TNodes));
      ntSelectStmtTableFactor: FormatSelectStmtTableFactor(TSelectStmt.PTableFactor(Node)^.Nodes);
      ntSelectStmtTableFactorIndexHint: FormatDefaultNode(@TSelectStmt.TTableFactor.PIndexHint(Node)^.Nodes, SizeOf(TSelectStmt.TTableFactor.TIndexHint.TNodes));
      ntSelectStmtTableFactorOj: FormatSelectStmtTableFactorOj(TSelectStmt.PTableFactorOj(Node)^.Nodes);
      ntSelectStmtTableFactorSelect: FormatDefaultNode(@TSelectStmt.PTableFactorSelect(Node)^.Nodes, SizeOf(TSelectStmt.TTableFactorSelect.TNodes));
      ntSelectStmtTableJoin: FormatSelectStmtTableJoin(TSelectStmt.PTableJoin(Node)^.Nodes);
      ntSetNamesStmt: FormatDefaultNode(@PSetNamesStmt(Node)^.Nodes, SizeOf(TSetNamesStmt.TNodes));
      ntSetPasswordStmt: FormatDefaultNode(@PSetPasswordStmt(Node)^.Nodes, SizeOf(TSetPasswordStmt.TNodes));
      ntSetStmt: FormatSetStmt(PSetStmt(Node)^.Nodes);
      ntSetStmtAssignment: FormatDefaultNode(@TSetStmt.PAssignment(Node)^.Nodes, SizeOf(TSetStmt.TAssignment.TNodes));
      ntSetTransactionStmt: FormatDefaultNode(@PSetTransactionStmt(Node)^.Nodes, SizeOf(TSetTransactionStmt.TNodes));
      ntSetTransactionStmtCharacteristic: FormatDefaultNode(@TSetTransactionStmt.PCharacteristic(Node)^.Nodes, SizeOf(TSetTransactionStmt.TCharacteristic.TNodes));
      ntShowAuthorsStmt: FormatDefaultNode(@PShowAuthorsStmt(Node)^.Nodes, SizeOf(TShowAuthorsStmt.TNodes));
      ntShowBinaryLogsStmt: FormatDefaultNode(@PShowBinaryLogsStmt(Node)^.Nodes, SizeOf(TShowBinaryLogsStmt.TNodes));
      ntShowBinlogEventsStmt: FormatShowBinlogEventsStmt(PShowBinlogEventsStmt(Node)^.Nodes);
      ntShowCharacterSetStmt: FormatDefaultNode(@PShowCharacterSetStmt(Node)^.Nodes, SizeOf(TShowCharacterSetStmt.TNodes));
      ntShowCollationStmt: FormatDefaultNode(@PShowCollationStmt(Node)^.Nodes, SizeOf(TShowCollationStmt.TNodes));
      ntShowContributorsStmt: FormatDefaultNode(@PShowContributorsStmt(Node)^.Nodes, SizeOf(TShowContributorsStmt.TNodes));
      ntShowCountErrorsStmt: FormatDefaultNode(@PShowCountErrorsStmt(Node)^.Nodes, SizeOf(TShowCountErrorsStmt.TNodes));
      ntShowCountWarningsStmt: FormatDefaultNode(@PShowCountWarningsStmt(Node)^.Nodes, SizeOf(TShowCountWarningsStmt.TNodes));
      ntShowCreateDatabaseStmt: FormatDefaultNode(@PShowCreateDatabaseStmt(Node)^.Nodes, SizeOf(TShowCreateDatabaseStmt.TNodes));
      ntShowCreateEventStmt: FormatDefaultNode(@PShowCreateEventStmt(Node)^.Nodes, SizeOf(TShowCreateEventStmt.TNodes));
      ntShowCreateFunctionStmt: FormatDefaultNode(@PShowCreateFunctionStmt(Node)^.Nodes, SizeOf(TShowCreateFunctionStmt.TNodes));
      ntShowCreateProcedureStmt: FormatDefaultNode(@PShowCreateProcedureStmt(Node)^.Nodes, SizeOf(TShowCreateProcedureStmt.TNodes));
      ntShowCreateTableStmt: FormatDefaultNode(@PShowCreateTableStmt(Node)^.Nodes, SizeOf(TShowCreateTableStmt.TNodes));
      ntShowCreateTriggerStmt: FormatDefaultNode(@PShowCreateTriggerStmt(Node)^.Nodes, SizeOf(TShowCreateTriggerStmt.TNodes));
      ntShowCreateUserStmt: FormatDefaultNode(@PShowCreateUserStmt(Node)^.Nodes, SizeOf(TShowCreateUserStmt.TNodes));
      ntShowCreateViewStmt: FormatDefaultNode(@PShowCreateViewStmt(Node)^.Nodes, SizeOf(TShowCreateViewStmt.TNodes));
      ntShowDatabasesStmt: FormatDefaultNode(@PShowDatabasesStmt(Node)^.Nodes, SizeOf(TShowDatabasesStmt.TNodes));
      ntShowEngineStmt: FormatDefaultNode(@PShowEngineStmt(Node)^.Nodes, SizeOf(TShowEngineStmt.TNodes));
      ntShowEnginesStmt: FormatDefaultNode(@PShowEnginesStmt(Node)^.Nodes, SizeOf(TShowEnginesStmt.TNodes));
      ntShowErrorsStmt: FormatShowErrorsStmt(PShowErrorsStmt(Node)^.Nodes);
      ntShowEventsStmt: FormatDefaultNode(@PShowEventsStmt(Node)^.Nodes, SizeOf(TShowEventsStmt.TNodes));
      ntShowFunctionCodeStmt: FormatDefaultNode(@PShowFunctionCodeStmt(Node)^.Nodes, SizeOf(TShowFunctionCodeStmt.TNodes));
      ntShowFunctionStatusStmt: FormatDefaultNode(@PShowFunctionStatusStmt(Node)^.Nodes, SizeOf(TShowFunctionStatusStmt.TNodes));
      ntShowGrantsStmt: FormatDefaultNode(@PShowGrantsStmt(Node)^.Nodes, SizeOf(TShowGrantsStmt.TNodes));
      ntShowIndexStmt: FormatDefaultNode(@PShowIndexStmt(Node)^.Nodes, SizeOf(TShowIndexStmt.TNodes));
      ntShowMasterStatusStmt: FormatDefaultNode(@PShowMasterStatusStmt(Node)^.Nodes, SizeOf(TShowMasterStatusStmt.TNodes));
      ntShowOpenTablesStmt: FormatDefaultNode(@PShowOpenTablesStmt(Node)^.Nodes, SizeOf(TShowOpenTablesStmt.TNodes));
      ntShowPluginsStmt: FormatDefaultNode(@PShowPluginsStmt(Node)^.Nodes, SizeOf(TShowPluginsStmt.TNodes));
      ntShowPrivilegesStmt: FormatDefaultNode(@PShowPrivilegesStmt(Node)^.Nodes, SizeOf(TShowPrivilegesStmt.TNodes));
      ntShowProcedureCodeStmt: FormatDefaultNode(@PShowProcedureCodeStmt(Node)^.Nodes, SizeOf(TShowProcedureCodeStmt.TNodes));
      ntShowProcedureStatusStmt: FormatDefaultNode(@PShowProcedureStatusStmt(Node)^.Nodes, SizeOf(TShowProcedureStatusStmt.TNodes));
      ntShowProcessListStmt: FormatDefaultNode(@PShowProcessListStmt(Node)^.Nodes, SizeOf(TShowProcessListStmt.TNodes));
      ntShowProfileStmt: FormatDefaultNode(@PShowProfileStmt(Node)^.Nodes, SizeOf(TShowProfileStmt.TNodes));
      ntShowProfilesStmt: FormatDefaultNode(@PShowProfilesStmt(Node)^.Nodes, SizeOf(TShowProfilesStmt.TNodes));
      ntShowRelaylogEventsStmt: FormatShowRelaylogEventsStmt(PShowBinlogEventsStmt(Node)^.Nodes);
      ntShowSlaveHostsStmt: FormatDefaultNode(@PShowSlaveHostsStmt(Node)^.Nodes, SizeOf(TShowSlaveHostsStmt.TNodes));
      ntShowSlaveStatusStmt: FormatDefaultNode(@PShowSlaveStatusStmt(Node)^.Nodes, SizeOf(TShowSlaveStatusStmt.TNodes));
      ntShowStatusStmt: FormatDefaultNode(@PShowStatusStmt(Node)^.Nodes, SizeOf(TShowStatusStmt.TNodes));
      ntShowTableStatusStmt: FormatDefaultNode(@PShowTableStatusStmt(Node)^.Nodes, SizeOf(TShowTableStatusStmt.TNodes));
      ntShowTablesStmt: FormatDefaultNode(@PShowTablesStmt(Node)^.Nodes, SizeOf(TShowTablesStmt.TNodes));
      ntShowTriggersStmt: FormatDefaultNode(@PShowTriggersStmt(Node)^.Nodes, SizeOf(TShowTriggersStmt.TNodes));
      ntShowVariablesStmt: FormatDefaultNode(@PShowVariablesStmt(Node)^.Nodes, SizeOf(TShowVariablesStmt.TNodes));
      ntShowWarningsStmt: FormatShowWarningsStmt(PShowWarningsStmt(Node)^.Nodes);
      ntShutdownStmt: FormatDefaultNode(@PShutdownStmt(Node)^.Nodes, SizeOf(TShutdownStmt.TNodes));
      ntSignalStmt: FormatDefaultNode(@PSignalStmt(Node)^.Nodes, SizeOf(TSignalStmt.TNodes));
      ntSignalStmtInformation: FormatDefaultNode(@TSignalStmt.PInformation(Node)^.Nodes, SizeOf(TSignalStmt.TInformation.TNodes));
      ntSoundsLikeOp: FormatDefaultNode(@PSoundsLikeOp(Node)^.Nodes, SizeOf(TSoundsLikeOp.TNodes));
      ntStartSlaveStmt: FormatDefaultNode(@PStartSlaveStmt(Node)^.Nodes, SizeOf(TSignalStmt.TNodes));
      ntStartTransactionStmt: FormatDefaultNode(@PStartTransactionStmt(Node)^.Nodes, SizeOf(TStartTransactionStmt.TNodes));
      ntStopSlaveStmt: FormatDefaultNode(@PStopSlaveStmt(Node)^.Nodes, SizeOf(TSignalStmt.TNodes));
      ntSubArea: FormatSubArea(PSubArea(Node)^.Nodes);
      ntSubAreaSelectStmt: FormatSubAreaSelectStmt(PSubAreaSelectStmt(Node)^.Nodes);
      ntSubPartition: FormatSubPartition(PSubPartition(Node)^.Nodes);
      ntSubstringFunc: FormatSubstringFunc(PSubstringFunc(Node)^.Nodes);
      ntTag: FormatDefaultNode(@PTag(Node)^.Nodes, SizeOf(TTag.TNodes));
      ntTrimFunc: FormatTrimFunc(PTrimFunc(Node)^.Nodes);
      ntTruncateStmt: FormatDefaultNode(@PTruncateStmt(Node)^.Nodes, SizeOf(TTruncateStmt.TNodes));
      ntUnaryOp: FormatUnaryOp(PUnaryOp(Node)^.Nodes);
      ntUnlockStmt: FormatDefaultNode(@PUnlockStmt(Node)^.Nodes, SizeOf(TUnlockStmt.TNodes));
      ntUpdateStmt: FormatDefaultNode(@PUpdateStmt(Node)^.Nodes, SizeOf(TUpdateStmt.TNodes));
      ntUser: FormatUser(PUser(Node)^.Nodes);
      ntUseStmt: FormatDefaultNode(@PUseStmt(Node)^.Nodes, SizeOf(TUseStmt.TNodes));
      ntValue: FormatValue(PValue(Node)^.Nodes);
      ntVariable: FormatVariable(PVariable(Node)^.Nodes);
      ntWeightStringFunc: FormatWeightStringFunc(PWeightStringFunc(Node)^.Nodes);
      ntWeightStringFuncLevel: FormatDefaultNode(@TWeightStringFunc.PLevel(Node)^.Nodes, SizeOf(TWeightStringFunc.TLevel.TNodes));
      ntWhileStmt: FormatWhileStmt(PWhileStmt(Node)^.Nodes);
      ntXAStmt: FormatDefaultNode(@PXAStmt(Node)^.Nodes, SizeOf(TXAStmt.TNodes));
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

procedure TMySQLParser.FormatNode(const Node: TOffset; const Separator: TSeparatorType = stNone);
begin
  FormatNode(NodePtr(Node), Separator);
end;

procedure TMySQLParser.FormatRepeatStmt(const Nodes: TRepeatStmt.TNodes);
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

procedure TMySQLParser.FormatRoot(const Node: PNode);
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

procedure TMySQLParser.FormatSchedule(const Nodes: TSchedule.TNodes);
var
  I: Integer;
begin
  if (Nodes.At.Tag > 0) then
  begin
    FormatNode(Nodes.At.Tag);
    FormatNode(Nodes.At.Timestamp, stSpaceBefore);

    for I := 0 to Length(Nodes.Starts.IntervalList) - 1 do
      FormatNode(Nodes.Starts.IntervalList[I], stSpaceBefore);
  end
  else if (Nodes.Every.Tag > 0) then
  begin
    FormatNode(Nodes.Every.Tag);
    FormatNode(Nodes.Every.Interval, stSpaceBefore);

    if (Nodes.Starts.Tag > 0) then
    begin
      Commands.IncreaseIndent();
      FormatNode(Nodes.Starts.Tag, stReturnBefore);
      FormatNode(Nodes.Starts.Timestamp, stSpaceBefore);

      for I := 0 to Length(Nodes.Starts.IntervalList) - 1 do
        FormatNode(Nodes.Starts.IntervalList[I], stSpaceBefore);
      Commands.DecreaseIndent();
    end;

    if (Nodes.Ends.Tag > 0) then
    begin
      Commands.IncreaseIndent();
      FormatNode(Nodes.Ends.Tag, stReturnBefore);
      FormatNode(Nodes.Ends.Timestamp, stSpaceBefore);

      for I := 0 to Length(Nodes.Ends.IntervalList) - 1 do
        FormatNode(Nodes.Ends.IntervalList[I], stSpaceBefore);
      Commands.DecreaseIndent();
    end;
  end
  else
    raise Exception.Create(SArgumentOutOfRange);
end;

procedure TMySQLParser.FormatSecretIdent(const Nodes: TSecretIdent.TNodes);
begin
  FormatNode(Nodes.OpenAngleBracket);
  FormatNode(Nodes.ItemToken);
  FormatNode(Nodes.CloseAngleBracket);
end;

procedure TMySQLParser.FormatSelectStmt(const Nodes: TSelectStmt.TNodes);
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
        FormatNode(Nodes.OptionallyEnclosedByValue, stSpaceBefore);
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

procedure TMySQLParser.FormatSelectStmtColumn(const Nodes: TSelectStmt.TColumn.TNodes);
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
    if (Nodes.AsToken > 0) then
      FormatNode(Nodes.AsToken, stSpaceBefore)
    else
      Commands.Write(' AS');
    FormatNode(Nodes.AliasIdent, stSpaceBefore);
  end;
end;

procedure TMySQLParser.FormatSelectStmtTableFactor(const Nodes: TSelectStmt.TTableFactor.TNodes);
begin
  FormatNode(Nodes.TableIdent);
  FormatNode(Nodes.PartitionTag, stSpaceBefore);
  FormatNode(Nodes.Partitions, stSpaceBefore);
  if (Nodes.AliasIdent > 0) then
  begin
    if (Nodes.AsToken > 0) then
      FormatNode(Nodes.AsToken, stSpaceBefore)
    else
      Commands.Write(' AS');
    FormatNode(Nodes.AliasIdent, stSpaceBefore);
  end;
  FormatNode(Nodes.IndexHintList, stSpaceBefore);
  FormatNode(Nodes.SelectStmt, stSpaceBefore);
end;

procedure TMySQLParser.FormatSelectStmtTableFactorOj(const Nodes: TSelectStmt.TTableFactorOj.TNodes);
begin
  FormatNode(Nodes.OpenBracket);
  FormatNode(Nodes.OjTag);
  FormatNode(Nodes.TableReference, stSpaceBefore);
  FormatNode(Nodes.CloseBracket);
end;

procedure TMySQLParser.FormatSelectStmtTableJoin(const Nodes: TSelectStmt.TTableJoin.TNodes);
begin
  Commands.IncreaseIndent();
  FormatNode(Nodes.JoinTag, stReturnBefore);
  FormatNode(Nodes.RightTable, stSpaceBefore);
  FormatNode(Nodes.OnTag, stSpaceBefore);
  FormatNode(Nodes.Condition, stSpaceBefore);
  Commands.DecreaseIndent();
end;

procedure TMySQLParser.FormatSetStmt(const Nodes: TSetStmt.TNodes);
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

procedure TMySQLParser.FormatShowBinlogEventsStmt(const Nodes: TShowBinlogEventsStmt.TNodes);
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

procedure TMySQLParser.FormatShowErrorsStmt(const Nodes: TShowErrorsStmt.TNodes);
begin
  FormatNode(Nodes.StmtTag);
  FormatNode(Nodes.Limit.Tag, stSpaceBefore);
  Commands.WriteSpace();
  FormatNode(Nodes.Limit.OffsetToken);
  FormatNode(Nodes.Limit.CommaToken);
  FormatNode(Nodes.Limit.RowCountToken);
end;

procedure TMySQLParser.FormatShowRelaylogEventsStmt(const Nodes: TShowBinlogEventsStmt.TNodes);
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

procedure TMySQLParser.FormatShowWarningsStmt(const Nodes: TShowWarningsStmt.TNodes);
begin
  FormatNode(Nodes.StmtTag);
  FormatNode(Nodes.Limit.Tag, stSpaceBefore);
  Commands.WriteSpace();
  FormatNode(Nodes.Limit.OffsetToken);
  FormatNode(Nodes.Limit.CommaToken);
  FormatNode(Nodes.Limit.RowCountToken);
end;

procedure TMySQLParser.FormatSQL(out SQL: string);
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

procedure TMySQLParser.FormatSubArea(const Nodes: TSubArea.TNodes);
begin
  FormatNode(Nodes.OpenBracket);
  FormatNode(Nodes.AreaNode);
  FormatNode(Nodes.CloseBracket);
end;

procedure TMySQLParser.FormatSubAreaSelectStmt(const Nodes: TSubAreaSelectStmt.TNodes);
begin
  FormatNode(Nodes.SelectStmt1);
  FormatNode(Nodes.UnionTag, stReturnBefore);
  FormatNode(Nodes.SelectStmt2, stReturnBefore);
end;

procedure TMySQLParser.FormatSubPartition(const Nodes: TSubPartition.TNodes);
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

procedure TMySQLParser.FormatSubstringFunc(const Nodes: TSubstringFunc.TNodes);
begin
  FormatNode(Nodes.FuncToken);
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

procedure TMySQLParser.FormatToken(const Token: PToken);
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
      ttCSString: Commands.Write(SQLEscape(SQLUnescape(Token^.AsString), ''''));
      else Commands.Write(Token^.AsString);
    end;
  end
  else if (Token^.TokenType = ttCSString) then
    Commands.Write(SQLEscape(SQLUnescape(Token^.AsString), ''''))
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

procedure TMySQLParser.FormatTrimFunc(const Nodes: TTrimFunc.TNodes);
begin
  FormatNode(Nodes.FuncToken);
  FormatNode(Nodes.OpenBracket);
  FormatNode(Nodes.DirectionTag, stSpaceAfter);
  FormatNode(Nodes.RemoveStr, stSpaceAfter);
  FormatNode(Nodes.FromTag, stSpaceAfter);
  FormatNode(Nodes.Str);
  FormatNode(Nodes.CloseBracket);
end;

procedure TMySQLParser.FormatUnaryOp(const Nodes: TUnaryOp.TNodes);
begin
  FormatNode(Nodes.Operator);
  if (IsToken(Nodes.Operator) and (TokenPtr(Nodes.Operator)^.KeywordIndex >= 0)) then
    Commands.WriteSpace();
  FormatNode(Nodes.Operand);
end;

procedure TMySQLParser.FormatUnknownStmt(const Node: PUnknownStmt);
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

procedure TMySQLParser.FormatUser(const Nodes: TUser.TNodes);
begin
  FormatNode(Nodes.NameToken);
  FormatNode(Nodes.AtToken);
  FormatNode(Nodes.HostToken);
end;

procedure TMySQLParser.FormatValue(const Nodes: TValue.TNodes);
begin
  FormatNode(Nodes.IdentTag);
  if (Nodes.AssignToken = 0) then
    Commands.WriteSpace()
  else
    FormatNode(Nodes.AssignToken);
  FormatNode(Nodes.Expr);
end;

procedure TMySQLParser.FormatVariable(const Nodes: TVariable.TNodes);
begin
  FormatNode(Nodes.At1Token);
  FormatNode(Nodes.At2Token);
  FormatNode(Nodes.ScopeTag);
  FormatNode(Nodes.ScopeDotToken);
  FormatNode(Nodes.Ident);
end;

procedure TMySQLParser.FormatWeightStringFunc(const Nodes: TWeightStringFunc.TNodes);
begin
  FormatNode(Nodes.FuncToken);
  FormatNode(Nodes.OpenBracket);
  FormatNode(Nodes.Str);
  FormatNode(Nodes.AsTag, stSpaceBefore);
  FormatNode(Nodes.Datatype, stSpaceBefore);
  FormatNode(Nodes.LevelTag, stSpaceBefore);
  FormatNode(Nodes.LevelList, stSpaceBefore);
  FormatNode(Nodes.CloseBracket);
end;

procedure TMySQLParser.FormatWhileStmt(const Nodes: TWhileStmt.TNodes);
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

procedure TMySQLParser.FormatXID(const Nodes: TXAStmt.TID.TNodes);
begin
  FormatNode(Nodes.GTrId);
  FormatNode(Nodes.Comma1, stSpaceAfter);
  FormatNode(Nodes.BQual);
  FormatNode(Nodes.Comma2, stSpaceAfter);
  FormatNode(Nodes.FormatId);
end;

function TMySQLParser.GetDatatypes(): string;
begin
  Result := DatatypeList.Text;
end;

function TMySQLParser.GetError(): Boolean;
begin
  Result := FErrorCode <> PE_Success;
end;

function TMySQLParser.GetErrorCode(): Byte;
begin
  if (FRoot = 0) then
    Result := PE_Success
  else
    Result := Root^.ErrorCode;
end;

function TMySQLParser.GetErrorMessage(): string;
begin
  if (FRoot = 0) then
    Result := ''
  else
    Result := Root^.ErrorMessage;
end;

function TMySQLParser.GetErrorMessage(const AErrorCode: Byte): string;
begin
  case (AErrorCode) of
    PE_Success: Result := '';
    PE_Unknown: Result := 'Unknown error';
    PE_IncompleteToken: Result := 'Incompleted token';
    PE_UnexpectedChar: Result := 'Unexpected character';
    PE_IncompleteStmt: Result := 'Incompleted statement';
    PE_UnexpectedToken: Result := 'Unexpected token';
    PE_ExtraToken: Result := 'Unexpected token after statement';
    PE_UnknownStmt: Result := 'Unknown statement';
    PE_NestedCondCode: Result := 'Nested conditional MySQL option';
    else Result := '[Unknown error]';
  end;
end;

function TMySQLParser.GetFunctions(): string;
begin
  Result := FunctionList.Text;
end;

function TMySQLParser.GetInPL_SQL(): Boolean;
begin
  Result := FInPL_SQL > 0;
end;

function TMySQLParser.GetKeywords(): string;
begin
  Result := KeywordList.Text;
end;

function TMySQLParser.GetNextToken(Index: Integer): TOffset;
begin
  Assert(Index >= 0);

  Result := GetParsedToken(Index);
end;

function TMySQLParser.GetParsedToken(const Index: Integer): TOffset;
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
      end;
    until ((Token = 0) or (TokenBuffer.Count - 1 = Index));

  if (Index >= TokenBuffer.Count) then
    Result := 0
  else
    Result := TokenBuffer.Tokens[Index];
end;

function TMySQLParser.GetRoot(): PRoot;
begin
  if (FRoot = 0) then
    Result := nil
  else
    Result := PRoot(NodePtr(FRoot));
end;

procedure TMySQLParser.GetText(const Offset: TOffset; out Text: PChar; out Length: Integer);
begin
  Assert((0 < Offset) and (Offset < Texts.UsedSize));

  Text := @Texts.Mem[Offset + SizeOf(Integer)];
  Length := Integer(Texts.Mem[Offset]);
end;

function TMySQLParser.IsChild(const ANode: PNode): Boolean;
begin
  Result := Assigned(ANode) and (ANode^.NodeType <> ntRoot);
end;

function TMySQLParser.IsChild(const ANode: TOffset): Boolean;
begin
  Result := IsChild(NodePtr(ANode));
end;

function TMySQLParser.IsRange(const ANode: PNode): Boolean;
begin
  Result := Assigned(ANode) and not (ANode^.NodeType in [ntRoot, ntToken]);
end;

function TMySQLParser.IsRange(const ANode: TOffset): Boolean;
begin
  Result := IsRange(NodePtr(ANode));
end;

function TMySQLParser.IsRoot(const ANode: PNode): Boolean;
begin
  Result := Assigned(ANode) and (ANode^.NodeType = ntRoot);
end;

function TMySQLParser.IsStmt(const ANode: PNode): Boolean;
begin
  Result := Assigned(ANode) and (ANode^.NodeType in StmtNodeTypes);
end;

function TMySQLParser.IsStmt(const ANode: TOffset): Boolean;
begin
  Result := IsStmt(NodePtr(ANode));
end;

function TMySQLParser.IsToken(const ANode: PNode): Boolean;
begin
  Result := Assigned(ANode) and (ANode^.NodeType = ntToken);
end;

function TMySQLParser.IsToken(const ANode: TOffset): Boolean;
begin
  Result := IsToken(NodePtr(ANode));
end;

function TMySQLParser.LoadFromFile(const Filename: string): Boolean;
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
      RaiseLastOSError()
    else
    begin
      MemSize := ((FileSize div BytesPerSector) + 1) * BytesPerSector;

      GetMem(Mem, MemSize);
      if (not Assigned(Mem)) then
        raise Exception.CreateFmt(SOutOfMemory, [MemSize])
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
          SetLength(ParseText, Len);
          MultiByteToWideChar(CP_UTF8, MB_ERR_INVALID_CHARS, @Mem[Length(BOM_UTF8)], BytesRead - DWord(Length(BOM_UTF8)), @ParseText[1], Len);
        end
        else if ((BytesRead >= DWord(Length(BOM_UNICODE_LE))) and (CompareMem(Mem, BOM_UNICODE_LE, StrLen(BOM_UNICODE_LE)))) then
        begin
          Len := (BytesRead - DWord(Length(BOM_UNICODE_LE))) div SizeOf(WideChar);
          SetLength(ParseText, Len);
          MoveMemory(@ParseText[1], @Mem[Length(BOM_UNICODE_LE)], Len * SizeOf(WideChar));
        end
        else
        begin
          Len := MultiByteToWideChar(CP_ACP, MB_ERR_INVALID_CHARS, Mem, BytesRead, nil, 0);
          SetLength(ParseText, Len);
          MultiByteToWideChar(CP_ACP, MB_ERR_INVALID_CHARS, Mem, BytesRead, @ParseText[1], Len);
        end;

        FreeMem(Mem);

        ParsePosition.Text := PChar(ParseText);
        ParsePosition.Length := Len;
        FRoot := ParseRoot();
      end;
    end;

    CloseHandle(Handle);
  end;

  Result := not Error;
end;

function TMySQLParser.NewNode(const ANodeType: TNodeType): TOffset;
var
  Size: Integer;
begin
  Size := NodeSize(ANodeType);

  if (ParsedNodes.UsedSize + Size > ParsedNodes.MemSize) then
  begin
    Inc(ParsedNodes.MemSize, ParsedNodes.MemSize);
    ReallocMem(ParsedNodes.Mem, ParsedNodes.MemSize);
  end;

  Result := ParsedNodes.UsedSize;
  Inc(ParsedNodes.UsedSize, Size);
end;

function TMySQLParser.NewText(const Text: PChar; const Length: Integer): TOffset;
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

function TMySQLParser.NodePtr(const ANode: TOffset): PNode;
begin
  Assert(ANode < ParsedNodes.UsedSize);

  if (ANode = 0) then
    Result := nil
  else
    Result := @ParsedNodes.Mem[ANode];
end;

function TMySQLParser.NodeSize(const NodeType: TNodeType): Integer;
begin
  case (NodeType) of
    ntRoot: Result := SizeOf(TRoot);
    ntToken: Result := SizeOf(TToken);

    ntAnalyzeStmt: Result := SizeOf(TAnalyzeStmt);
    ntAlterDatabaseStmt: Result := SizeOf(TAlterDatabaseStmt);
    ntAlterEventStmt: Result := SizeOf(TAlterEventStmt);
    ntAlterInstanceStmt: Result := SizeOf(TAlterInstanceStmt);
    ntAlterRoutineStmt: Result := SizeOf(TAlterRoutineStmt);
    ntAlterServerStmt: Result := SizeOf(TAlterServerStmt);
    ntAlterTableStmt: Result := SizeOf(TAlterTableStmt);
    ntAlterTableStmtAlterColumn: Result := SizeOf(TAlterTableStmt.TAlterColumn);
    ntAlterTableStmtConvertTo: Result := SizeOf(TAlterTableStmt.TConvertTo);
    ntAlterTableStmtDropObject: Result := SizeOf(TAlterTableStmt.TDropObject);
    ntAlterTableStmtExchangePartition: Result := SizeOf(TAlterTableStmt.TExchangePartition);
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
    ntCharFunc: Result := SizeOf(TCharFunc);
    ntCheckStmt: Result := SizeOf(TCheckStmt);
    ntCheckStmtOption: Result := SizeOf(TCheckStmt.TOption);
    ntChecksumStmt: Result := SizeOf(TChecksumStmt);
    ntCloseStmt: Result := SizeOf(TCloseStmt);
    ntCommitStmt: Result := SizeOf(TCommitStmt);
    ntCompoundStmt: Result := SizeOf(TCompoundStmt);
    ntConvertFunc: Result := SizeOf(TConvertFunc);
    ntCreateDatabaseStmt: Result := SizeOf(TCreateDatabaseStmt);
    ntCreateEventStmt: Result := SizeOf(TCreateEventStmt);
    ntCreateIndexStmt: Result := SizeOf(TCreateIndexStmt);
    ntCreateRoutineStmt: Result := SizeOf(TCreateRoutineStmt);
    ntCreateServerStmt: Result := SizeOf(TCreateServerStmt);
    ntCreateTableStmt: Result := SizeOf(TCreateTableStmt);
    ntCreateTableStmtField: Result := SizeOf(TCreateTableStmt.TField);
    ntCreateTableStmtFieldDefaultFunc: Result := SizeOf(TCreateTableStmt.TField.TDefaultFunc);
    ntCreateTableStmtForeignKey: Result := SizeOf(TCreateTableStmt.TForeignKey);
    ntCreateTableStmtKey: Result := SizeOf(TCreateTableStmt.TKey);
    ntCreateTableStmtKeyColumn: Result := SizeOf(TCreateTableStmt.TKeyColumn);
    ntCreateTableStmtPartition: Result := SizeOf(TCreateTableStmt.TPartition);
    ntCreateTableStmtPartitionValues: Result := SizeOf(TCreateTableStmt.TPartitionValues);
    ntCreateTableStmtReference: Result := SizeOf(TCreateTableStmt.TReference);
    ntCreateTriggerStmt: Result := SizeOf(TCreateTriggerStmt);
    ntCreateUserStmt: Result := SizeOf(TCreateUserStmt);
    ntCreateViewStmt: Result := SizeOf(TCreateViewStmt);
    ntCurrentTimestamp: Result := SizeOf(TCurrentTimestamp);
    ntDatatype: Result := SizeOf(TDatatype);
    ntDbIdent: Result := SizeOf(TDbIdent);
    ntDeallocatePrepareStmt: Result := SizeOf(TDeallocatePrepareStmt);
    ntDeclareStmt: Result := SizeOf(TDeclareStmt);
    ntDeclareConditionStmt: Result := SizeOf(TDeclareConditionStmt);
    ntDeclareCursorStmt: Result := SizeOf(TDeclareCursorStmt);
    ntDeclareHandlerStmt: Result := SizeOf(TDeclareHandlerStmt);
    ntDeclareHandlerStmtCondition: Result := SizeOf(TDeclareHandlerStmt.TCondition);
    ntDeleteStmt: Result := SizeOf(TDeleteStmt);
    ntDoStmt: Result := SizeOf(TDoStmt);
    ntDropDatabaseStmt: Result := SizeOf(TDropDatabaseStmt);
    ntDropEventStmt: Result := SizeOf(TDropEventStmt);
    ntDropIndexStmt: Result := SizeOf(TDropIndexStmt);
    ntDropRoutineStmt: Result := SizeOf(TDropRoutineStmt);
    ntDropServerStmt: Result := SizeOf(TDropServerStmt);
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
    ntIgnoreLines: Result := SizeOf(TIgnoreLines);
    ntInOp: Result := SizeOf(TInOp);
    ntInsertStmt: Result := SizeOf(TInsertStmt);
    ntIntervalOp: Result := SizeOf(TIntervalOp);
    ntIntervalOpListItem: Result := SizeOf(TIntervalOp.TListItem);
    ntIterateStmt: Result := SizeOf(TIterateStmt);
    ntKillStmt: Result := SizeOf(TKillStmt);
    ntLeaveStmt: Result := SizeOf(TLeaveStmt);
    ntLikeOp: Result := SizeOf(TLikeOp);
    ntList: Result := SizeOf(TList);
    ntLoadDataStmt: Result := SizeOf(TLoadDataStmt);
    ntLoadXMLStmt: Result := SizeOf(TLoadXMLStmt);
    ntLockStmt: Result := SizeOf(TLockStmt);
    ntLockStmtItem: Result := SizeOf(TLockStmt.TItem);
    ntLoopStmt: Result := SizeOf(TLoopStmt);
    ntMatchFunc: Result := SizeOf(TMatchFunc);
    ntOpenStmt: Result := SizeOf(TOpenStmt);
    ntOptimizeStmt: Result := SizeOf(TOptimizeStmt);
    ntPositionFunc: Result := SizeOf(TPositionFunc);
    ntPrepareStmt: Result := SizeOf(TPrepareStmt);
    ntPurgeStmt: Result := SizeOf(TPurgeStmt);
    ntRegExpOp: Result := SizeOf(TRegExpOp);
    ntRenameStmt: Result := SizeOf(TRenameStmt);
    ntRenameStmtPair: Result := SizeOf(TRenameStmt.TPair);
    ntReleaseStmt: Result := SizeOf(TReleaseStmt);
    ntRepairStmt: Result := SizeOf(TRepairStmt);
    ntRepeatStmt: Result := SizeOf(TRepeatStmt);
    ntResetStmt: Result := SizeOf(TResetStmt);
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
    ntSubstringFunc: Result := SizeOf(TSubstringFunc);
    ntTag: Result := SizeOf(TTag);
    ntTruncateStmt: Result := SizeOf(TTruncateStmt);
    ntUnaryOp: Result := SizeOf(TUnaryOp);
    ntUnknownStmt: Result := SizeOf(TUnknownStmt);
    ntUnlockStmt: Result := SizeOf(TUnlockStmt);
    ntUpdateStmt: Result := SizeOf(TUpdateStmt);
    ntUser: Result := SizeOf(TUser);
    ntUseStmt: Result := SizeOf(TUseStmt);
    ntValue: Result := SizeOf(TValue);
    ntVariable: Result := SizeOf(TVariable);
    ntWeightStringFunc: Result := SizeOf(TWeightStringFunc);
    ntWeightStringFuncLevel: Result := SizeOf(TWeightStringFunc.TLevel);
    ntWhileStmt: Result := SizeOf(TWhileStmt);
    ntXAStmt: Result := SizeOf(TXAStmt);
    ntXAStmtID: Result := SizeOf(TXAStmt.TID);
    else raise Exception.Create(SArgumentOutOfRange);
  end;
end;

function TMySQLParser.ParseRoot(): TOffset;
var
  ErrorCode: Byte;
  ErrorMessage: TOffset;
  FirstTokenAll: TOffset;
  Stmt: TOffset;
  Children: Classes.TList;
begin
  if (AnsiQuotes) then
  begin
    ttIdents := [ttIdent, ttDQIdent];
    UsageTypeByTokenType[ttDQIdent] := utDbIdent;
  end
  else
    ttIdents := [ttIdent, ttMySQLIdent];
  if (AnsiQuotes) then
    ttStrings := [ttIdent, ttString, ttCSString]
  else
  begin
    ttStrings := [ttIdent, ttString, ttDQIdent, ttCSString];
    UsageTypeByTokenType[ttDQIdent] := utConst;
  end;

  if (ParsePosition.Length = 0) then
    FirstTokenAll := 0
  else
    FirstTokenAll := ParsedNodes.UsedSize;
  FPreviousToken := 0;
  FCurrentToken := GetParsedToken(0); // Cache for speeding


  Children := Classes.TList.Create();

  ErrorCode := PE_Success; ErrorMessage := 0;
  while (CurrentToken > 0) do
    if (not TokenPtr(CurrentToken)^.IsUsed or (TokenPtr(CurrentToken)^.TokenType = ttDelimiter)) then
      Children.Add(Pointer(ApplyCurrentToken()))
    else
    begin
      FErrorCode := PE_Success;
      FErrorToken := 0;

      Stmt := ParseStmt();
      Children.Add(Pointer(Stmt));

      if (ErrorCode = PE_Success) then
      begin
        ErrorCode := StmtPtr(Stmt)^.ErrorCode;
        ErrorMessage := StmtPtr(Stmt).FErrorMessage;
      end;
    end;

  Result := TRoot.Create(Self, ErrorCode, ErrorMessage, FirstTokenAll, FPreviousToken, Children.Count, TIntegerArray(Children.List));

  Children.Free();
end;

function TMySQLParser.ParseAnalyzeStmt(): TOffset;
var
  Nodes: TAnalyzeStmt.TNodes;
begin
  FillChar(Nodes, SizeOf(Nodes), 0);

  Nodes.StmtTag := ParseTag(kiANALYZE);

  if (not Error and not EndOfStmt(CurrentToken)) then
    if (TokenPtr(CurrentToken)^.KeywordIndex = kiNO_WRITE_TO_BINLOG) then
      Nodes.NoWriteToBinlogTag := ParseTag(kiNO_WRITE_TO_BINLOG)
    else if (TokenPtr(CurrentToken)^.KeywordIndex = kiLOCAL) then
      Nodes.NoWriteToBinlogTag := ParseTag(kiLOCAL);

  if (not Error) then
    Nodes.TableTag := ParseTag(kiTABLE);

  if (not Error) then
    if (EndOfStmt(CurrentToken)) then
      SetError(PE_IncompleteStmt)
    else
      Nodes.TablesList := ParseList(False, ParseTableIdent);
  Result := TAnalyzeStmt.Create(Self, Nodes);
end;

function TMySQLParser.ParseAliasIdent(): TOffset;
begin
  Result := 0;
  if (not (TokenPtr(CurrentToken)^.TokenType in ttIdents + ttStrings)) then
    SetError(PE_UnexpectedToken)
  else
    Result := ApplyCurrentToken(utConst);
end;

function TMySQLParser.ParseAlterDatabaseStmt(): TOffset;
var
  Found: Boolean;
  Nodes: TAlterDatabaseStmt.TNodes;
begin
  FillChar(Nodes, SizeOf(Nodes), 0);

  if (EndOfStmt(NextToken[1])) then
    SetError(PE_IncompleteStmt)
  else if (TokenPtr(NextToken[1])^.KeywordIndex = kiDATABASE) then
    Nodes.StmtTag := ParseTag(kiALTER, kiDATABASE)
  else if (TokenPtr(NextToken[1])^.KeywordIndex = kiSCHEMA) then
    Nodes.StmtTag := ParseTag(kiALTER, kiSCHEMA)
  else
    SetError(PE_UnexpectedToken);

  if (not Error) then
    if (EndOfStmt(CurrentToken)) then
      SetError(PE_IncompleteStmt)
    else if (TokenPtr(CurrentToken)^.KeywordIndex <> kiUPGRADE) then
    begin
      if (not Error and not EndOfStmt(CurrentToken)
        and (TokenPtr(CurrentToken)^.TokenType in ttIdents)
        and (TokenPtr(CurrentToken)^.KeywordIndex <> kiDEFAULT)
        and (TokenPtr(CurrentToken)^.KeywordIndex <> kiCHARACTER)
        and (TokenPtr(CurrentToken)^.KeywordIndex <> kiCOLLATE)) then
        Nodes.IdentTag := ParseDatabaseIdent();

      Found := True;                                                      
      while (not Error and Found and not EndOfStmt(CurrentToken)) do
        if ((Nodes.CharacterSetValue = 0) and (TokenPtr(CurrentToken)^.KeywordIndex = kiCHARACTER)) then
          Nodes.CharacterSetValue := ParseValue(WordIndices(kiCHARACTER, kiSET), vaAuto, ParseIdent)
        else if ((Nodes.CharacterSetValue = 0) and (TokenPtr(CurrentToken)^.KeywordIndex = kiCHARSET)) then
          Nodes.CharacterSetValue := ParseValue(kiCHARSET, vaAuto, ParseIdent)
        else if ((Nodes.CollateValue = 0) and (TokenPtr(CurrentToken)^.KeywordIndex = kiCOLLATE)) then
          Nodes.CollateValue := ParseValue(kiCOLLATE, vaAuto, ParseIdent)
        else if ((Nodes.CharacterSetValue = 0) and not EndOfStmt(NextToken[1]) and (TokenPtr(CurrentToken)^.KeywordIndex = kiDEFAULT) and (TokenPtr(NextToken[1])^.KeywordIndex = kiCHARACTER)) then
          Nodes.CharacterSetValue := ParseValue(WordIndices(kiDEFAULT, kiCHARACTER, kiSET), vaAuto, ParseIdent)
        else if ((Nodes.CollateValue = 0) and not EndOfStmt(NextToken[1]) and (TokenPtr(CurrentToken)^.KeywordIndex = kiDEFAULT) and (TokenPtr(NextToken[1])^.KeywordIndex = kiCOLLATE)) then
          Nodes.CollateValue := ParseValue(WordIndices(kiDEFAULT, kiCOLLATE), vaAuto, ParseIdent)
        else
          Found := False;
    end
    else
    begin
      if (EndOfStmt(CurrentToken)) then
        SetError(PE_IncompleteStmt)
      else if (not (TokenPtr(CurrentToken)^.TokenType in ttIdents)) then
        SetError(PE_UnexpectedToken)
      else
        Nodes.IdentTag := ParseDatabaseIdent();

      if (not Error) then
        Nodes.UpgradeDataDirectoryNameTag := ParseTag(kiUPGRADE, kiDATA, kiDIRECTORY, kiNAME);
    end;

  Result := TAlterDatabaseStmt.Create(Self, Nodes);
end;

function TMySQLParser.ParseAlterEventStmt(): TOffset;
var
  Nodes: TAlterEventStmt.TNodes;
begin
  FillChar(Nodes, SizeOf(Nodes), 0);

  Nodes.AlterTag := ParseTag(kiALTER);

  if (not Error and not EndOfStmt(CurrentToken) and (TokenPtr(CurrentToken)^.KeywordIndex = kiDEFINER)) then
    Nodes.DefinerNode := ParseDefinerValue();

  if (not Error) then
    Nodes.EventTag := ParseTag(kiEVENT);

  if (not Error) then
    if (EndOfStmt(CurrentToken)) then
      SetError(PE_IncompleteStmt)
    else if (not (TokenPtr(CurrentToken)^.TokenType in ttIdents)) then
      SetError(PE_UnexpectedToken)
    else
      Nodes.EventIdent := ParseEventIdent();

  if (not Error and not EndOfStmt(CurrentToken)
    and (TokenPtr(CurrentToken)^.KeywordIndex = kiON)
    and not EndOfStmt(NextToken[1]) and (TokenPtr(NextToken[1])^.KeywordIndex = kiSCHEDULE)) then
  begin
    Nodes.OnSchedule.Tag := ParseTag(kiON, kiSCHEDULE);
    if (not Error) then
      if (EndOfStmt(CurrentToken)) then
        SetError(PE_IncompleteStmt)
      else
        Nodes.OnSchedule.Value := ParseSchedule();
  end;

  if (not Error and not EndOfStmt(CurrentToken)
    and (TokenPtr(CurrentToken)^.KeywordIndex = kiON)
    and not EndOfStmt(NextToken[1]) and (TokenPtr(NextToken[1])^.KeywordIndex = kiCOMPLETION)) then
    if (not EndOfStmt(NextToken[2]) and (TokenPtr(NextToken[2])^.KeywordIndex = kiNOT)) then
      Nodes.OnCompletitionTag := ParseTag(kiON, kiCOMPLETION, kiNOT, kiPRESERVE)
    else
      Nodes.OnCompletitionTag := ParseTag(kiON, kiCOMPLETION, kiPRESERVE);

  if (not Error and not EndOfStmt(CurrentToken) and (TokenPtr(CurrentToken)^.KeywordIndex = kiRENAME)) then
    Nodes.RenameValue := ParseValue(WordIndices(kiRENAME, kiTO), vaNo, ParseEventIdent);

  if (not Error and not EndOfStmt(CurrentToken)) then
    if (TokenPtr(CurrentToken)^.KeywordIndex = kiENABLE) then
      Nodes.EnableTag := ParseTag(kiENABLE)
    else if ((TokenPtr(CurrentToken)^.KeywordIndex = kiDISABLE)
      and (TokenPtr(NextToken[1])^.KeywordIndex = kiON)) then
      Nodes.EnableTag := ParseTag(kiDISABLE, kiON, kiSLAVE)
    else if (TokenPtr(CurrentToken)^.KeywordIndex = kiDISABLE) then
      Nodes.EnableTag := ParseTag(kiDISABLE);

  if (not Error and not EndOfStmt(CurrentToken) and (TokenPtr(CurrentToken)^.KeywordIndex = kiCOMMENT)) then
    Nodes.CommentValue := ParseValue(kiCOMMENT, vaNo, ParseString);

  if (not Error and not EndOfStmt(CurrentToken) and (TokenPtr(CurrentToken)^.KeywordIndex = kiDO)) then
  begin
    Nodes.DoTag := ParseTag(kiDO);

    if (not Error) then
      Nodes.Body := ParsePL_SQLStmt();
  end;

  Result := TAlterEventStmt.Create(Self, Nodes);
end;

function TMySQLParser.ParseAlterInstanceStmt(): TOffset;
var
  Nodes: TAlterInstanceStmt.TNodes;
begin
  FillChar(Nodes, SizeOf(Nodes), 0);

  Nodes.StmtTag := ParseTag(kiALTER, kiINSTANCE);

  if (not Error) then
    Nodes.RotateTag := ParseTag(kiROTATE, kiINNODB, kiMASTER, kiKEY);

  Result := TAlterInstanceStmt.Create(Self, Nodes);
end;

function TMySQLParser.ParseAlterRoutineStmt(const ARoutineType: TRoutineType): TOffset;
var
  Nodes: TAlterRoutineStmt.TNodes;
begin
  FillChar(Nodes, SizeOf(Nodes), 0);

  if (EndOfStmt(NextToken[1])) then
    SetError(PE_IncompleteStmt, NextToken[1])
  else if (TokenPtr(NextToken[1])^.KeywordIndex = kiPROCEDURE) then
    Nodes.AlterTag := ParseTag(kiALTER, kiPROCEDURE)
  else if (TokenPtr(NextToken[1])^.KeywordIndex = kiFUNCTION) then
    Nodes.AlterTag := ParseTag(kiALTER, kiFUNCTION)
  else
    SetError(PE_UnexpectedToken, NextToken[1]);

  if (not Error) then
    if (ARoutineType = rtFunction) then
      Nodes.Ident := ParseDbIdent(ditFunction)
    else
      Nodes.Ident := ParseDbIdent(ditProcedure);

  if (not Error) then
    Nodes.CharacteristicList := ParseCreateRoutineStmtCharacteristList();

  Result := TAlterRoutineStmt.Create(Self, ARoutineType, Nodes);
end;

function TMySQLParser.ParseAlterServerStmt(): TOffset;
var
  Nodes: TAlterServerStmt.TNodes;
begin
  FillChar(Nodes, SizeOf(Nodes), 0);

  Nodes.StmtTag := ParseTag(kiALTER, kiSERVER);

  if (not Error) then
    if (EndOfStmt(CurrentToken)) then
      SetError(PE_IncompleteStmt)
    else if (not (TokenPtr(CurrentToken)^.TokenType in ttIdents)) then
      SetError(PE_UnexpectedToken)
    else
      Nodes.Ident := ParseDbIdent(ditServer);

  if (not Error) then
    Nodes.Options.Tag := ParseTag(kiOPTIONS);

  if (not Error) then
    if (EndOfStmt(CurrentToken)) then
      SetError(PE_IncompleteStmt)
    else
      Nodes.Options.List := ParseCreateServerStmtOptionList();

  Result := TAlterServerStmt.Create(Self, Nodes);
end;

function TMySQLParser.ParseAlterTableStmt(): TOffset;
var
  DelimiterExpected: Boolean;
  DelimiterFound: Boolean;
  ListNodes: TList.TNodes;
  Nodes: TAlterTableStmt.TNodes;
  Specifications: Classes.TList;
  TableOptionNodes: TTableOptionNodes;
begin
  FillChar(Nodes, SizeOf(Nodes), 0);
  FillChar(TableOptionNodes, SizeOf(TableOptionNodes), 0);

  Specifications := Classes.TList.Create();

  Nodes.AlterTag := ParseTag(kiALTER);

  if (not Error and not EndOfStmt(CurrentToken) and (TokenPtr(CurrentToken)^.KeywordIndex = kiIGNORE)) then
    Nodes.IgnoreTag := ParseTag(kiIGNORE);

  if (not Error) then
    Nodes.TableTag := ParseTag(kiTABLE);

  if (not Error) then
    Nodes.Ident := ParseTableIdent();


  DelimiterFound := False; DelimiterExpected := False;
  while (not Error and (DelimiterFound or not DelimiterExpected) and not EndOfStmt(CurrentToken)) do
  begin
    DelimiterExpected := True;

    if (TokenPtr(CurrentToken)^.KeywordIndex = kiADD) then
      Specifications.Add(Pointer(ParseCreateTableStmtDefinition(True)))
    else if (TokenPtr(CurrentToken)^.KeywordIndex = kiALTER) then
      Specifications.Add(Pointer(ParseAlterTableStmtAlterColumn()))
    else if (TokenPtr(CurrentToken)^.KeywordIndex = kiCHANGE) then
      Specifications.Add(Pointer(ParseCreateTableStmtField(caChange)))
    else if (TokenPtr(CurrentToken)^.KeywordIndex = kiDROP) then
      Specifications.Add(Pointer(ParseAlterTableStmtDropItem()))
    else if (TokenPtr(CurrentToken)^.KeywordIndex = kiMODIFY) then
      Specifications.Add(Pointer(ParseCreateTableStmtField(caModify)))


    else if ((Nodes.AlgorithmValue = 0) and (TokenPtr(CurrentToken)^.KeywordIndex = kiALGORITHM)) then
    begin
      Nodes.AlgorithmValue := ParseValue(kiALGORITHM, vaAuto, WordIndices(kiDEFAULT, kiINPLACE, kiCOPY));
      Specifications.Add(Pointer(Nodes.AlgorithmValue));
    end
    else if ((Nodes.ConvertToCharacterSetNode = 0) and (TokenPtr(CurrentToken)^.KeywordIndex = kiCONVERT)) then
    begin
      Nodes.ConvertToCharacterSetNode := ParseAlterTableStmtConvertTo();
      Specifications.Add(Pointer(Nodes.ConvertToCharacterSetNode));
    end
    else if ((Nodes.EnableKeys = 0) and (TokenPtr(CurrentToken)^.KeywordIndex = kiDISABLE)) then
    begin
      Nodes.EnableKeys := ParseTag(kiDISABLE, kiKEYS);
      Specifications.Add(Pointer(Nodes.EnableKeys));
    end
    else if ((Nodes.DiscardTablespaceTag = 0) and (TokenPtr(CurrentToken)^.KeywordIndex = kiDISCARD)) then
    begin
      Nodes.DiscardTablespaceTag := ParseTag(kiDISCARD, kiTABLESPACE);
      Specifications.Add(Pointer(Nodes.DiscardTablespaceTag));
    end
    else if ((Nodes.EnableKeys = 0) and (TokenPtr(CurrentToken)^.KeywordIndex = kiENABLE)) then
    begin
      Nodes.EnableKeys := ParseTag(kiENABLE, kiKEYS);
      Specifications.Add(Pointer(Nodes.EnableKeys));
    end
    else if ((Nodes.ForceTag = 0) and (TokenPtr(CurrentToken)^.KeywordIndex = kiFORCE)) then
    begin
      Nodes.ForceTag := ParseTag(kiFORCE);
      Specifications.Add(Pointer(Nodes.ForceTag));
    end
    else if ((Nodes.ImportTablespaceTag = 0) and (TokenPtr(CurrentToken)^.KeywordIndex = kiIMPORT)) then
    begin
      Nodes.ImportTablespaceTag := ParseTag(kiDISCARD, kiTABLESPACE);
      Specifications.Add(Pointer(Nodes.ImportTablespaceTag));
    end
    else if ((Nodes.LockValue = 0) and (TokenPtr(CurrentToken)^.KeywordIndex = kiLOCK)) then
    begin
      Nodes.LockValue := ParseValue(kiLOCK, vaAuto, WordIndices(kiDEFAULT, kiNONE, kiSHARED, kiEXCLUSIVE));
      Specifications.Add(Pointer(Nodes.LockValue));
    end
    else if ((Nodes.OrderByValue = 0) and (TokenPtr(CurrentToken)^.KeywordIndex = kiORDER)) then
    begin
      Nodes.OrderByValue := ParseValue(WordIndices(kiORDER, kiBY), vaNo, ParseCreateTableStmtKeyColumn);
      Specifications.Add(Pointer(Nodes.OrderByValue));
    end
    else if ((Nodes.RenameNode = 0) and (TokenPtr(CurrentToken)^.KeywordIndex = kiRENAME)) then
    begin
      if (not EndOfStmt(NextToken[1]) and (TokenPtr(NextToken[1])^.KeywordIndex = kiTO)) then
        Nodes.RenameNode := ParseValue(WordIndices(kiRENAME, kiTO), vaNo, ParseTableIdent)
      else if (not EndOfStmt(NextToken[1]) and (TokenPtr(NextToken[1])^.KeywordIndex = kiAS)) then
        Nodes.RenameNode := ParseValue(WordIndices(kiRENAME, kiAS), vaNo, ParseTableIdent)
      else
        Nodes.RenameNode := ParseValue(kiRENAME, vaNo, ParseTableIdent);
      Specifications.Add(Pointer(Nodes.RenameNode));
    end


    else if ((TableOptionNodes.AutoIncrementValue = 0) and (TokenPtr(CurrentToken)^.KeywordIndex = kiAUTO_INCREMENT)) then
    begin
      TableOptionNodes.AutoIncrementValue := ParseValue(kiAUTO_INCREMENT, vaAuto, ParseInteger);
      Specifications.Add(Pointer(TableOptionNodes.AutoIncrementValue));
      DelimiterExpected := False;
    end
    else if ((TableOptionNodes.AvgRowLengthValue = 0) and (TokenPtr(CurrentToken)^.KeywordIndex = kiAVG_ROW_LENGTH)) then
    begin
      TableOptionNodes.AvgRowLengthValue := ParseValue(kiAVG_ROW_LENGTH, vaAuto, ParseInteger);
      Specifications.Add(Pointer(TableOptionNodes.AvgRowLengthValue));
      DelimiterExpected := False;
    end
    else if ((TableOptionNodes.CharacterSetValue = 0) and not EndOfStmt(NextToken[2])
      and ((TokenPtr(CurrentToken)^.KeywordIndex = kiDEFAULT)
        or (TokenPtr(CurrentToken)^.KeywordIndex = kiCHARACTER)
        or (TokenPtr(CurrentToken)^.KeywordIndex = kiCHARSET))) then
    begin
      if ((TokenPtr(CurrentToken)^.KeywordIndex = kiDEFAULT) and (TokenPtr(NextToken[1])^.KeywordIndex = kiCHARACTER)) then
        TableOptionNodes.CharacterSetValue := ParseValue(WordIndices(kiDEFAULT, kiCHARACTER, kiSET), vaAuto, ParseIdent)
      else if ((TokenPtr(CurrentToken)^.KeywordIndex = kiDEFAULT) and (TokenPtr(NextToken[1])^.KeywordIndex = kiCHARSET)) then
        TableOptionNodes.CharacterSetValue := ParseValue(WordIndices(kiDEFAULT, kiCHARSET), vaAuto, ParseIdent)
      else if ((TokenPtr(CurrentToken)^.KeywordIndex = kiCHARACTER)) then
        TableOptionNodes.CharacterSetValue := ParseValue(WordIndices(kiCHARACTER, kiSET), vaAuto, ParseIdent)
      else if ((TokenPtr(CurrentToken)^.KeywordIndex = kiCHARSET)) then
        TableOptionNodes.CharacterSetValue := ParseValue(WordIndices(kiCHARSET), vaAuto, ParseIdent);
      Specifications.Add(Pointer(TableOptionNodes.CharacterSetValue));
      DelimiterExpected := False;
    end
    else if ((TableOptionNodes.ChecksumValue = 0) and (TokenPtr(CurrentToken)^.KeywordIndex = kiCHECKSUM)) then
    begin
      TableOptionNodes.AutoIncrementValue := ParseValue(kiCHECKSUM, vaAuto, ParseInteger);
      Specifications.Add(Pointer(TableOptionNodes.AutoIncrementValue));
      DelimiterExpected := False;
    end
    else if ((TableOptionNodes.CollateValue = 0) and (TokenPtr(CurrentToken)^.KeywordIndex = kiCOLLATE)) then
    begin
      TableOptionNodes.CollateValue := ParseValue(WordIndices(kiCOLLATE), vaAuto, ParseIdent);
      Specifications.Add(Pointer(TableOptionNodes.CollateValue));
      DelimiterExpected := False;
    end
    else if ((TableOptionNodes.CharacterSetValue = 0) and (TokenPtr(CurrentToken)^.KeywordIndex = kiDEFAULT) and not EndOfStmt(NextToken[1]) and (TokenPtr(NextToken[1])^.KeywordIndex = kiCHARACTER)) then
    begin
      TableOptionNodes.CharacterSetValue := ParseValue(WordIndices(kiDEFAULT, kiCHARACTER, kiSET), vaAuto, ParseIdent);
      Specifications.Add(Pointer(TableOptionNodes.CharacterSetValue));
      DelimiterExpected := False;
    end
    else if ((TableOptionNodes.CollateValue = 0) and (TokenPtr(CurrentToken)^.KeywordIndex = kiDEFAULT) and not EndOfStmt(NextToken[1]) and (TokenPtr(NextToken[1])^.KeywordIndex = kiCOLLATE)) then
    begin
      TableOptionNodes.CollateValue := ParseValue(WordIndices(kiDEFAULT, kiCOLLATE), vaAuto, ParseIdent);
      Specifications.Add(Pointer(TableOptionNodes.CollateValue));
      DelimiterExpected := False;
    end
    else if ((TableOptionNodes.CommentValue = 0) and (TokenPtr(CurrentToken)^.KeywordIndex = kiCOMMENT)) then
    begin
      TableOptionNodes.CommentValue := ParseValue(kiCOMMENT, vaAuto, ParseString);
      Specifications.Add(Pointer(TableOptionNodes.CommentValue));
      DelimiterExpected := False;
    end
    else if ((TableOptionNodes.CompressValue = 0) and (TokenPtr(CurrentToken)^.KeywordIndex = kiCOMPRESS)) then
    begin
      TableOptionNodes.CompressValue := ParseValue(kiCOMPRESS, vaAuto, ParseString);
      Specifications.Add(Pointer(TableOptionNodes.CompressValue));
      DelimiterExpected := False;
    end
    else if ((TableOptionNodes.ConnectionValue = 0) and (TokenPtr(CurrentToken)^.KeywordIndex = kiCONNECTION)) then
    begin
      TableOptionNodes.ConnectionValue := ParseValue(kiCONNECTION, vaAuto, ParseString);
      Specifications.Add(Pointer(TableOptionNodes.ConnectionValue));
      DelimiterExpected := False;
    end
    else if ((TableOptionNodes.DataDirectoryValue = 0) and (TokenPtr(CurrentToken)^.KeywordIndex = kiDATA)) then
    begin
      TableOptionNodes.DataDirectoryValue := ParseValue(WordIndices(kiDATA, kiDIRECTORY), vaAuto, ParseString);
      Specifications.Add(Pointer(TableOptionNodes.DataDirectoryValue));
      DelimiterExpected := False;
    end
    else if ((TableOptionNodes.DelayKeyWriteValue = 0) and (TokenPtr(CurrentToken)^.KeywordIndex = kiDELAY_KEY_WRITE)) then
    begin
      TableOptionNodes.DelayKeyWriteValue := ParseValue(kiDELAY_KEY_WRITE, vaAuto, ParseInteger);
      Specifications.Add(Pointer(TableOptionNodes.DelayKeyWriteValue));
      DelimiterExpected := False;
    end
    else if ((TableOptionNodes.EngineValue = 0) and (TokenPtr(CurrentToken)^.KeywordIndex = kiENGINE)) then
    begin
      TableOptionNodes.EngineValue := ParseValue(kiENGINE, vaAuto, ParseIdent);
      Specifications.Add(Pointer(TableOptionNodes.EngineValue));
      DelimiterExpected := False;
    end
    else if ((TableOptionNodes.IndexDirectoryValue = 0) and (TokenPtr(CurrentToken)^.KeywordIndex = kiINDEX)) then
    begin
      TableOptionNodes.IndexDirectoryValue := ParseValue(WordIndices(kiINDEX, kiDIRECTORY), vaAuto, ParseString);
      Specifications.Add(Pointer(TableOptionNodes.IndexDirectoryValue));
      DelimiterExpected := False;
    end
    else if ((TableOptionNodes.InsertMethodValue = 0) and (TokenPtr(CurrentToken)^.KeywordIndex = kiINSERT_METHOD)) then
    begin
      TableOptionNodes.InsertMethodValue := ParseValue(kiINSERT_METHOD, vaAuto, WordIndices(kiNO, kiFIRST, kiLAST));
      Specifications.Add(Pointer(TableOptionNodes.InsertMethodValue));
      DelimiterExpected := False;
    end
    else if ((TableOptionNodes.KeyBlockSizeValue = 0) and (TokenPtr(CurrentToken)^.KeywordIndex = kiKEY_BLOCK_SIZE)) then
    begin
      TableOptionNodes.KeyBlockSizeValue := ParseValue(kiKEY_BLOCK_SIZE, vaAuto, ParseInteger);
      Specifications.Add(Pointer(TableOptionNodes.KeyBlockSizeValue));
      DelimiterExpected := False;
    end
    else if ((TableOptionNodes.MaxRowsValue = 0) and (TokenPtr(CurrentToken)^.KeywordIndex = kiMAX_ROWS)) then
    begin
      TableOptionNodes.MaxRowsValue := ParseValue(kiMAX_ROWS, vaAuto, ParseInteger);
      Specifications.Add(Pointer(TableOptionNodes.MaxRowsValue));
      DelimiterExpected := False;
    end
    else if ((TableOptionNodes.MinRowsValue = 0) and (TokenPtr(CurrentToken)^.KeywordIndex = kiMIN_ROWS)) then
    begin
      TableOptionNodes.MinRowsValue := ParseValue(kiMIN_ROWS, vaAuto, ParseInteger);
      Specifications.Add(Pointer(TableOptionNodes.MinRowsValue));
      DelimiterExpected := False;
    end
    else if ((TableOptionNodes.PackKeysValue = 0) and (TokenPtr(CurrentToken)^.KeywordIndex = kiPACK_KEYS)) then
    begin
      TableOptionNodes.PackKeysValue := ParseValue(kiPACK_KEYS, vaAuto, ParseExpr);
      Specifications.Add(Pointer(TableOptionNodes.PackKeysValue));
      DelimiterExpected := False;
    end
    else if ((TableOptionNodes.PageChecksum = 0) and (TokenPtr(CurrentToken)^.KeywordIndex = kiPAGE_CHECKSUM)) then
    begin
      TableOptionNodes.PageChecksum := ParseValue(kiPAGE_CHECKSUM, vaAuto, ParseInteger);
      Specifications.Add(Pointer(TableOptionNodes.PageChecksum));
      DelimiterExpected := False;
    end
    else if ((TableOptionNodes.PasswordValue = 0) and (TokenPtr(CurrentToken)^.KeywordIndex = kiPASSWORD)) then
    begin
      TableOptionNodes.PasswordValue := ParseValue(kiPASSWORD, vaAuto, ParseString);
      Specifications.Add(Pointer(TableOptionNodes.PasswordValue));
      DelimiterExpected := False;
    end
    else if ((TableOptionNodes.RowFormatValue = 0) and (TokenPtr(CurrentToken)^.KeywordIndex = kiROW_FORMAT)) then
    begin
      TableOptionNodes.RowFormatValue := ParseValue(kiROW_FORMAT, vaAuto, ParseIdent);
      Specifications.Add(Pointer(TableOptionNodes.RowFormatValue));
      DelimiterExpected := False;
    end
    else if ((TableOptionNodes.StatsAutoRecalcValue = 0) and (TokenPtr(CurrentToken)^.KeywordIndex = kiSTATS_AUTO_RECALC)) then
    begin
      if (EndOfStmt(NextToken[1])) then
        SetError(PE_IncompleteStmt)
      else if (TokenPtr(NextToken[1])^.TokenType = ttInteger) then
        TableOptionNodes.StatsAutoRecalcValue := ParseValue(kiSTATS_AUTO_RECALC, vaAuto, ParseInteger)
      else
        TableOptionNodes.StatsAutoRecalcValue := ParseValue(kiSTATS_AUTO_RECALC, vaAuto, WordIndices(kiDEFAULT));
      Specifications.Add(Pointer(TableOptionNodes.StatsAutoRecalcValue));
      DelimiterExpected := False;
    end
    else if ((TableOptionNodes.StatsPersistentValue = 0) and (TokenPtr(CurrentToken)^.KeywordIndex = kiSTATS_PERSISTENT)) then
    begin
      if (EndOfStmt(NextToken[1])) then
        SetError(PE_IncompleteStmt)
      else if (TokenPtr(NextToken[1])^.TokenType = ttInteger) then
        TableOptionNodes.StatsPersistentValue := ParseValue(kiSTATS_PERSISTENT, vaAuto, ParseInteger)
      else
        TableOptionNodes.StatsPersistentValue := ParseValue(kiSTATS_PERSISTENT, vaAuto, WordIndices(kiDEFAULT));
      Specifications.Add(Pointer(TableOptionNodes.StatsPersistentValue));
      DelimiterExpected := False;
    end
    else if ((TableOptionNodes.TransactionalValue = 0) and (TokenPtr(CurrentToken)^.KeywordIndex = kiTRANSACTIONAL)) then
    begin
      TableOptionNodes.TransactionalValue := ParseValue(kiTRANSACTIONAL, vaAuto, ParseInteger);
      Specifications.Add(Pointer(TableOptionNodes.TransactionalValue));
      DelimiterExpected := False;
    end
    else if ((TableOptionNodes.EngineValue = 0) and (TokenPtr(CurrentToken)^.KeywordIndex = kiTYPE)) then
    begin
      TableOptionNodes.EngineValue := ParseValue(kiTYPE, vaAuto, ParseIdent);
      Specifications.Add(Pointer(TableOptionNodes.EngineValue));
      DelimiterExpected := False;
    end
    else if ((TableOptionNodes.UnionList = 0) and (TokenPtr(CurrentToken)^.KeywordIndex = kiUNION)) then
    begin
      TableOptionNodes.UnionList := ParseCreateTableStmtUnion();
      Specifications.Add(Pointer(TableOptionNodes.UnionList));
      DelimiterExpected := False;
    end


    else if ((TokenPtr(CurrentToken)^.KeywordIndex = kiANALYZE)
      or (TokenPtr(CurrentToken)^.KeywordIndex = kiCHECK)
      or (TokenPtr(CurrentToken)^.KeywordIndex = kiOPTIMIZE)
      or (TokenPtr(CurrentToken)^.KeywordIndex = kiREBUILD)
      or (TokenPtr(CurrentToken)^.KeywordIndex = kiREPAIR)
      or (TokenPtr(CurrentToken)^.KeywordIndex = kiTRUNCATE)) then
    begin
      Specifications.Add(Pointer(ParseValue(TokenPtr(CurrentToken)^.KeywordIndex, vaNo, ParseCreateTableStmtDefinitionPartitionNames)));
      break;
    end
    else if (TokenPtr(CurrentToken)^.KeywordIndex = kiCOALESCE) then
    begin
      Specifications.Add(Pointer(ParseValue(kiCOALESCE, vaNo, ParseInteger)));
      break;
    end
    else if (TokenPtr(CurrentToken)^.KeywordIndex = kiEXCHANGE) then
    begin
      Specifications.Add(Pointer(ParseAlterTableStmtExchangePartition()));
      break;
    end
    else if (TokenPtr(CurrentToken)^.KeywordIndex = kiREMOVE) then
    begin
      Specifications.Add(Pointer(ParseTag(kiREMOVE, kiPARTITIONING)));
      break;
    end
    else if (TokenPtr(CurrentToken)^.KeywordIndex = kiREORGANIZE) then
    begin
      Specifications.Add(Pointer(ParseAlterTableStmtReorganizePartition()));
      break;
    end;

    if (not Error) then
      if (DelimiterExpected and not EndOfStmt(CurrentToken) and not (TokenPtr(CurrentToken)^.TokenType in [ttComma, ttDelimiter])) then
        SetError(PE_UnexpectedToken)
      else
      begin
        DelimiterFound := not EndOfStmt(CurrentToken) and (TokenPtr(CurrentToken)^.TokenType = ttComma);
        if (DelimiterFound) then
          Specifications.Add(Pointer(ApplyCurrentToken()));
      end;
  end;

  FillChar(ListNodes, SizeOf(ListNodes), 0);
  Nodes.SpecificationList := TList.Create(Self, ListNodes, ttComma, Specifications.Count, TIntegerArray(Specifications.List));
  Result := TAlterTableStmt.Create(Self, Nodes);

  Specifications.Free();
end;

function TMySQLParser.ParseAlterTableStmtAlterColumn(): TOffset;
var
  Nodes: TAlterTableStmt.TAlterColumn.TNodes;
begin
  FillChar(Nodes, SizeOf(Nodes), 0);

  if (not EndOfStmt(NextToken[1]) and (TokenPtr(NextToken[1])^.KeywordIndex = kiCOLUMN)) then
    Nodes.AlterTag := ParseTag(kiALTER, kiCOLUMN)
  else
    Nodes.AlterTag := ParseTag(kiALTER);

  if (not Error) then
    if (EndOfStmt(CurrentToken)) then
      SetError(PE_IncompleteStmt)
    else if (not (TokenPtr(CurrentToken)^.TokenType in ttIdents)) then
      SetError(PE_UnexpectedToken)
    else
      Nodes.ColumnIdent := ParseFieldIdent();

  if (not Error and not EndOfStmt(CurrentToken)) then
    if (TokenPtr(CurrentToken)^.KeywordIndex = kiSET) then
      Nodes.SetDefaultValue := ParseValue(WordIndices(kiSET, kiDEFAULT), vaNo, ParseString)
    else
      Nodes.DropDefaultTag := ParseTag(kiDROP);

  Result := TAlterTableStmt.TAlterColumn.Create(Self, Nodes);
end;

function TMySQLParser.ParseAlterTableStmtConvertTo(): TOffset;
var
  Nodes: TAlterTableStmt.TConvertTo.TNodes;
begin
  FillChar(Nodes, SizeOf(Nodes), 0);

  Nodes.ConvertToTag := ParseTag(kiCONVERT, kiTO);

  if (not Error) then
    if (EndOfStmt(CurrentToken)) then
      SetError(PE_IncompleteStmt)
    else if (TokenPtr(CurrentToken)^.KeywordIndex = kiCHARACTER) then
      Nodes.CharacterSetValue := ParseValue(WordIndices(kiCHARACTER, kiSET), vaNo, ParseIdent)
    else if (TokenPtr(CurrentToken)^.KeywordIndex = kiCHARSET) then
      Nodes.CharacterSetValue := ParseValue(kiCHARSET, vaNo, ParseIdent)
    else
      SetError(PE_UnexpectedToken);

  if (not Error and not EndOfStmt(CurrentToken) and (TokenPtr(CurrentToken)^.KeywordIndex = kiCOLLATE)) then
    Nodes.CollateValue := ParseValue(kiCOLLATE, vaNo, ParseIdent);

  Result := TAlterTableStmt.TConvertTo.Create(Self, Nodes);
end;

function TMySQLParser.ParseAlterTableStmtDropItem(): TOffset;
var
  Nodes: TAlterTableStmt.TDropObject.TNodes;
begin
  FillChar(Nodes, SizeOf(Nodes), 0);

  Nodes.DropTag := ParseTag(kiDROP);

  if (not Error) then
  begin
    if (EndOfStmt(CurrentToken)) then
      SetError(PE_IncompleteStmt)
    else if (TokenPtr(CurrentToken)^.KeywordIndex = kiPARTITION) then
    begin
      Nodes.ItemTypeTag := ParseTag(kiPARTITION);

      if (not Error) then
        Nodes.Ident := ParseList(False, ParsePartitionIdent);
    end
    else
    begin
      if (TokenPtr(CurrentToken)^.KeywordIndex = kiPRIMARY) then
        Nodes.ItemTypeTag := ParseTag(kiPRIMARY, kiKEY)
      else if ((TokenPtr(CurrentToken)^.KeywordIndex = kiINDEX)
        or (TokenPtr(CurrentToken)^.KeywordIndex = kiKEY)) then
      begin
        Nodes.ItemTypeTag := ParseTag(TokenPtr(CurrentToken)^.KeywordIndex);
        if (not Error) then
          if (EndOfStmt(CurrentToken)) then
            SetError(PE_IncompleteStmt)
          else
            Nodes.Ident := ParseKeyIdent();
      end
      else if (TokenPtr(CurrentToken)^.KeywordIndex = kiFOREIGN) then
      begin
        Nodes.ItemTypeTag := ParseTag(kiFOREIGN, kiKEY);

        if (not Error) then
          if (EndOfStmt(CurrentToken)) then
            SetError(PE_IncompleteStmt)
          else
            Nodes.Ident := ParseForeignKeyIdent();
      end
      else
      begin
        if (not EndOfStmt(CurrentToken) and (TokenPtr(CurrentToken)^.KeywordIndex = kiCOLUMN)) then
          Nodes.ItemTypeTag := ParseTag(kiCOLUMN);

        if (not Error) then
          if (EndOfStmt(CurrentToken)) then
            SetError(PE_IncompleteStmt)
          else if (not (TokenPtr(CurrentToken)^.TokenType in ttIdents)) then
            SetError(PE_UnexpectedToken)
          else
            Nodes.Ident := ParseFieldIdent();
      end;
    end;
  end;

  Result := TAlterTableStmt.TDropObject.Create(Self, Nodes);
end;

function TMySQLParser.ParseAlterTableStmtExchangePartition(): TOffset;
var
  Nodes: TAlterTableStmt.TExchangePartition.TNodes;
begin
  FillChar(Nodes, SizeOf(Nodes), 0);

  Nodes.ExchangePartitionTag := ParseTag(kiEXCHANGE, kiPARTITION);

  if (not Error) then
    if (EndOfStmt(CurrentToken)) then
      SetError(PE_IncompleteStmt)
    else
      Nodes.PartitionIdent := ParsePartitionIdent();

  if (not Error) then
    Nodes.WithTableTag := ParseTag(kiWITH, kiTABLE);

  if (not Error) then
    if (EndOfStmt(CurrentToken)) then
      SetError(PE_IncompleteStmt)
    else if (not (TokenPtr(CurrentToken)^.TokenType in ttIdents)) then
      SetError(PE_UnexpectedToken)
    else
      Nodes.TableIdent := ParseTableIdent();

  Result := TAlterTableStmt.TExchangePartition.Create(Self, Nodes);
end;

function TMySQLParser.ParseAlterTableStmtReorganizePartition(): TOffset;
var
  Nodes: TAlterTableStmt.TReorganizePartition.TNodes;
begin
  FillChar(Nodes, SizeOf(Nodes), 0);

  Nodes.ReorganizePartitionTag := ParseTag(kiREORGANIZE, kiPARTITION);

  if (not Error) then
    Nodes.PartitionIdentList := ParseList(False, ParsePartitionIdent);

  if (not Error) then
    Nodes.IntoTag := ParseTag(kiINTO);

  if (not Error) then
    Nodes.PartitionList := ParseList(True, ParseCreateTableStmtPartition);

  Result := TAlterTableStmt.TReorganizePartition.Create(Self, Nodes);
end;

function TMySQLParser.ParseAlterStmt(): TOffset;
var
  Index: Integer;
begin
  Assert(TokenPtr(CurrentToken)^.KeywordIndex = kiALTER);

  Index := 1;

  if (not Error and not EndOfStmt(NextToken[Index]) and (TokenPtr(NextToken[Index])^.KeywordIndex = kiIGNORE)) then
    Inc(Index);

  if (not Error and not EndOfStmt(NextToken[Index]) and (TokenPtr(NextToken[Index])^.KeywordIndex = kiALGORITHM)) then
  begin
    Inc(Index);
    if (EndOfStmt(NextToken[Index])) then
      SetError(PE_IncompleteStmt)
    else if (TokenPtr(NextToken[Index])^.OperatorType <> otEqual) then
      SetError(PE_UnexpectedToken, NextToken[Index])
    else
    begin
      Inc(Index);

      if (EndOfStmt(NextToken[Index])) then
        SetError(PE_IncompleteStmt)
      else if ((TokenPtr(NextToken[Index])^.KeywordIndex <> kiUNDEFINED)
        and (TokenPtr(NextToken[Index])^.KeywordIndex <> kiMERGE)
        and (TokenPtr(NextToken[Index])^.KeywordIndex <> kiTEMPTABLE)) then
        SetError(PE_UnexpectedToken, NextToken[Index])
      else
        Inc(Index);
    end;
  end;

  if (not Error and not EndOfStmt(NextToken[Index]) and (TokenPtr(NextToken[Index])^.KeywordIndex = kiDEFINER)) then
  begin
    Inc(Index);
    if (EndOfStmt(NextToken[Index])) then
      SetError(PE_IncompleteStmt)
    else if (TokenPtr(NextToken[Index])^.OperatorType <> otEqual) then
      SetError(PE_UnexpectedToken, NextToken[Index])
    else
    begin
      Inc(Index);

      if (EndOfStmt(NextToken[Index])) then
        SetError(PE_IncompleteStmt)
      else if (TokenPtr(NextToken[Index])^.KeywordIndex = kiCURRENT_USER) then
        if (((NextToken[Index + 1] = 0) or (TokenPtr(NextToken[Index + 1])^.TokenType <> ttOpenBracket))
          and ((NextToken[Index + 2] = 0) or (TokenPtr(NextToken[Index + 2])^.TokenType <> ttCloseBracket))) then
          Inc(Index)
        else
          Inc(Index, 3)
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

  if (not Error and not EndOfStmt(NextToken[Index]) and (TokenPtr(NextToken[Index])^.KeywordIndex = kiSQL)) then
  begin
    Inc(Index);
    if (EndOfStmt(NextToken[Index])) then
      SetError(PE_IncompleteStmt)
    else if (TokenPtr(NextToken[Index])^.KeywordIndex <> kiSECURITY) then
      SetError(PE_UnexpectedToken, NextToken[Index])
    else
    begin
      Inc(Index);
      if (EndOfStmt(NextToken[Index])) then
        SetError(PE_IncompleteStmt)
      else if ((TokenPtr(NextToken[Index])^.KeywordIndex <> kiDEFINER)
        and (TokenPtr(NextToken[Index])^.KeywordIndex <> kiINVOKER)) then
        SetError(PE_UnexpectedToken, NextToken[Index])
      else
        Inc(Index);
    end;
  end;

  Result := 0;
  if (not Error) then
    if (EndOfStmt(NextToken[Index])) then
      SetError(PE_IncompleteStmt)
    else if (TokenPtr(NextToken[Index])^.KeywordIndex = kiDATABASE) then
      Result := ParseAlterDatabaseStmt()
    else if (TokenPtr(NextToken[Index])^.KeywordIndex = kiEVENT) then
      Result := ParseAlterEventStmt()
    else if (TokenPtr(NextToken[Index])^.KeywordIndex = kiFUNCTION) then
      Result := ParseAlterRoutineStmt(rtFunction)
    else if (TokenPtr(NextToken[Index])^.KeywordIndex = kiPROCEDURE) then
      Result := ParseAlterRoutineStmt(rtProcedure)
    else if (TokenPtr(NextToken[Index])^.KeywordIndex = kiSERVER) then
      Result := ParseAlterServerStmt()
    else if (TokenPtr(NextToken[Index])^.KeywordIndex = kiTABLE) then
      Result := ParseAlterTableStmt()
    else if (TokenPtr(NextToken[Index])^.KeywordIndex = kiUSER) then
      Result := ParseCreateUserStmt(True)
    else if (TokenPtr(NextToken[Index])^.KeywordIndex = kiVIEW) then
      Result := ParseAlterViewStmt()
    else
      SetError(PE_UnexpectedToken, NextToken[Index]);
end;

function TMySQLParser.ParseAlterViewStmt(): TOffset;
var
  Nodes: TAlterViewStmt.TNodes;
begin
  FillChar(Nodes, SizeOf(Nodes), 0);

  Nodes.AlterTag := ParseTag(kiALTER);

  if (not Error and (TokenPtr(CurrentToken)^.KeywordIndex = kiALGORITHM)) then
    Nodes.AlgorithmValue := ParseValue(kiALGORITHM, vaYes, ParseKeyword);

  if (not Error and not EndOfStmt(CurrentToken) and (TokenPtr(CurrentToken)^.KeywordIndex = kiDEFINER)) then
    Nodes.DefinerNode := ParseDefinerValue();

  if (not Error and (TokenPtr(CurrentToken)^.KeywordIndex = kiSQL)) then
    if (EndOfStmt(NextToken[2])) then
      SetError(PE_IncompleteStmt)
    else if (TokenPtr(NextToken[2])^.KeywordIndex = kiDEFINER) then
      Nodes.SQLSecurityTag := ParseTag(kiSQL, kiSECURITY, kiDEFINER)
    else
      Nodes.SQLSecurityTag := ParseTag(kiSQL, kiSECURITY, kiINVOKER);

  if (not Error) then
    Nodes.ViewTag := ParseTag(kiVIEW);

  if (not Error) then
    Nodes.Ident := ParseTableIdent();

  if (not Error and not EndOfStmt(CurrentToken) and (TokenPtr(CurrentToken)^.TokenType = ttOpenBracket)) then
    Nodes.Columns := ParseList(True, ParseFieldIdent);

  if (not Error) then
    Nodes.AsTag := ParseTag(kiAS);

  if (not Error) then
    if (EndOfStmt(CurrentToken)) then
      SetError(PE_IncompleteStmt)
    else if ((TokenPtr(CurrentToken)^.KeywordIndex <> kiSELECT)
      and EndOfStmt(NextToken[1]) and (TokenPtr(NextToken[1])^.KeywordIndex <> kiSELECT)) then
      SetError(PE_UnexpectedToken)
    else
    begin
      Nodes.SelectStmt := ParseSubAreaSelectStmt();

      if (not Error and (TokenPtr(CurrentToken)^.KeywordIndex = kiWITH)) then
        if (EndOfStmt(NextToken[1])) then
          SetError(PE_IncompleteStmt)
        else if (TokenPtr(NextToken[1])^.KeywordIndex = kiCASCADED) then
          Nodes.OptionTag := ParseTag(kiWITH, kiCASCADED, kiCHECK, kiOPTION)
        else if (TokenPtr(NextToken[1])^.KeywordIndex = kiLOCAL) then
          Nodes.OptionTag := ParseTag(kiWITH, kiLOCAL, kiCHECK, kiOPTION)
        else
          Nodes.OptionTag := ParseTag(kiWITH, kiCHECK, kiOPTION);
    end;

  Result := TAlterViewStmt.Create(Self, Nodes);
end;

function TMySQLParser.ParseBeginLabel(): TOffset;
var
  Nodes: TBeginLabel.TNodes;
begin
  FillChar(Nodes, SizeOf(Nodes), 0);

  Nodes.BeginToken := ApplyCurrentToken(utLabel);
  Nodes.ColonToken := ApplyCurrentToken(utLabel);

  Result := TBeginLabel.Create(Self, Nodes);
end;

function TMySQLParser.ParseBeginStmt(): TOffset;
var
  Nodes: TBeginStmt.TNodes;
begin
  FillChar(Nodes, SizeOf(Nodes), 0);

  if (not EndOfStmt(NextToken[1]) and (TokenPtr(NextToken[1])^.KeywordIndex = kiWORK)) then
    Nodes.BeginTag := ParseTag(kiBEGIN, kiWORK)
  else
    Nodes.BeginTag := ParseTag(kiBEGIN);

  Result := TBeginStmt.Create(Self, Nodes);
end;

function TMySQLParser.ParseCallStmt(): TOffset;
var
  Nodes: TCallStmt.TNodes;
begin
  FillChar(Nodes, SizeOf(Nodes), 0);

  Nodes.CallTag := ParseTag(kiCALL);

  if (not Error) then
    Nodes.ProcedureIdent := ParseDbIdent(ditProcedure);

  if (not Error and not EndOfStmt(CurrentToken) and (TokenPtr(CurrentToken)^.TokenType = ttOpenBracket)) then
    Nodes.ParamList := ParseList(True, ParseExpr);

  Result := TCallStmt.Create(Self, Nodes);
end;

function TMySQLParser.ParseCaseOp(): TOffset;
var
  Branches: array of TOffset;
  ListNodes: TList.TNodes;
  Nodes: TCaseOp.TNodes;
begin
  FillChar(Nodes, SizeOf(Nodes), 0);

  SetLength(Branches, 0);

  Nodes.CaseTag := ParseTag(kiCASE);

  if (not Error) then
    if (EndOfStmt(CurrentToken)) then
      SetError(PE_IncompleteStmt)
    else if (TokenPtr(CurrentToken)^.KeywordIndex <> kiWHEN) then
    begin
      Nodes.CompareExpr := ParseExpr();

      if (not Error) then
        if (EndOfStmt(CurrentToken)) then
          SetError(PE_IncompleteStmt)
        else if (TokenPtr(CurrentToken)^.KeywordIndex <> kiWHEN) then
          SetError(PE_UnexpectedToken)
        else
          repeat
            SetLength(Branches, Length(Branches) + 1);
            Branches[Length(Branches) - 1] := ParseCaseOpBranch();
          until (Error or EndOfStmt(CurrentToken) or (TokenPtr(CurrentToken)^.KeywordIndex <> kiWHEN));
    end
    else
    begin
      repeat
        SetLength(Branches, Length(Branches) + 1);
        Branches[Length(Branches) - 1] := ParseCaseOpBranch();
      until (Error or EndOfStmt(CurrentToken) or (TokenPtr(CurrentToken)^.KeywordIndex <> kiWHEN));
    end;

  FillChar(ListNodes, SizeOf(ListNodes), 0);
  Nodes.BranchList := TList.Create(Self, ListNodes, ttUnknown, Length(Branches), Branches);
  SetLength(Branches, 0);

  if (not Error and not EndOfStmt(CurrentToken) and (TokenPtr(CurrentToken)^.KeywordIndex = kiELSE)) then
  begin
    Nodes.ElseTag := ParseTag(kiELSE);

    if (not Error) then
      Nodes.ElseExpr := ParseExpr();
  end;

  if (not Error) then
    Nodes.EndTag := ParseTag(kiEND);

  Result := TCaseOp.Create(Self, Nodes);
end;

function TMySQLParser.ParseCaseOpBranch(): TOffset;
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

function TMySQLParser.ParseCaseStmt(): TOffset;
var
  Branches: array of TOffset;
  ListNodes: TList.TNodes;
  Nodes: TCaseStmt.TNodes;
begin
  FillChar(Nodes, SizeOf(Nodes), 0);

  SetLength(Branches, 0);

  Nodes.CaseTag := ParseTag(kiCASE);

  if (not Error and not EndOfStmt(CurrentToken) and (TokenPtr(CurrentToken)^.KeywordIndex <> kiWHEN)) then
    Nodes.CompareExpr := ParseExpr();

  while (not Error and not EndOfStmt(CurrentToken) and (TokenPtr(CurrentToken)^.KeywordIndex = kiWHEN)) do
  begin
    SetLength(Branches, Length(Branches) + 1);
    Branches[Length(Branches) - 1] := ParseCaseStmtBranch();
  end;

  if (not Error and not EndOfStmt(CurrentToken) and (TokenPtr(CurrentToken)^.KeywordIndex = kiELSE)) then
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

function TMySQLParser.ParseCaseStmtBranch(): TOffset;
var
  Nodes: TCaseStmt.TBranch.TNodes;
begin
  FillChar(Nodes, SizeOf(Nodes), 0);

  if (not EndOfStmt(CurrentToken)) then
    if (TokenPtr(CurrentToken)^.KeywordIndex = kiWHEN) then
    begin
      Nodes.Tag := ParseTag(kiWHEN);

      if (not Error) then
        Nodes.ConditionExpr := ParseExpr();

      if (not Error) then
        Nodes.ThenTag := ParseTag(kiTHEN);
    end
    else if (TokenPtr(CurrentToken)^.KeywordIndex = kiELSE) then
      Nodes.Tag := ParseTag(kiELSE);

  if (not Error and not EndOfStmt(CurrentToken)
    and (TokenPtr(CurrentToken)^.KeywordIndex <> kiWHEN)
    and (TokenPtr(CurrentToken)^.KeywordIndex <> kiELSE)
    and (TokenPtr(CurrentToken)^.KeywordIndex <> kiEND)) then
    Nodes.StmtList := ParseList(False, ParsePL_SQLStmt, ttDelimiter);

  Result := TCaseStmt.TBranch.Create(Self, Nodes);
end;

function TMySQLParser.ParseCastFunc(): TOffset;
var
  Nodes: TCastFunc.TNodes;
begin
  FillChar(Nodes, SizeOf(Nodes), 0);

  Nodes.FuncToken := ApplyCurrentToken(utFunction);

  if (not Error) then
    if (EndOfStmt(CurrentToken)) then
      SetError(PE_IncompleteStmt)
    else if (TokenPtr(CurrentToken)^.TokenType <> ttOpenBracket) then
      SetError(PE_UnexpectedToken)
    else
      Nodes.OpenBracket := ApplyCurrentToken();

  if (not Error) then
    Nodes.Expr := ParseExpr();

  if (not Error) then
    Nodes.AsTag := ParseTag(kiAS);

  if (not Error) then
    if (EndOfStmt(CurrentToken)) then
      SetError(PE_IncompleteStmt)
    else if (TokenPtr(CurrentToken)^.TokenType <> ttIdent) then
      SetError(PE_UnexpectedToken)
    else
      Nodes.Datatype := ParseDatatype();

  if (not Error) then
    if (EndOfStmt(CurrentToken)) then
      SetError(PE_IncompleteStmt)
    else if (TokenPtr(CurrentToken)^.TokenType <> ttCloseBracket) then
      SetError(PE_UnexpectedToken)
    else
      Nodes.CloseBracket := ApplyCurrentToken();

  Result := TCastFunc.Create(Self, Nodes);
end;

function TMySQLParser.ParseCharFunc(): TOffset;
var
  Nodes: TCharFunc.TNodes;
begin
  FillChar(Nodes, SizeOf(Nodes), 0);

  Nodes.FuncToken := ApplyCurrentToken(utFunction);

  if (not Error) then
    if (EndOfStmt(CurrentToken)) then
      SetError(PE_IncompleteStmt)
    else if (TokenPtr(CurrentToken)^.TokenType <> ttOpenBracket) then
      SetError(PE_UnexpectedToken)
    else
      Nodes.OpenBracket := ApplyCurrentToken();

  if (not Error) then
    Nodes.ValueExpr := ParseList(False, ParseExpr);

  if (not Error and not EndOfStmt(CurrentToken) and (TokenPtr(CurrentToken)^.KeywordIndex = kiUSING)) then
  begin
    Nodes.UsingTag := ParseTag(kiUSING);

    if (not Error) then
      if (EndOfStmt(CurrentToken)) then
        SetError(PE_IncompleteStmt)
      else if (TokenPtr(CurrentToken)^.TokenType <> ttIdent) then
        SetError(PE_UnexpectedToken)
      else
        Nodes.CharsetIdent := ParseIdent();
  end;

  if (not Error) then
    if (EndOfStmt(CurrentToken)) then
      SetError(PE_IncompleteStmt)
    else if (TokenPtr(CurrentToken)^.TokenType <> ttCloseBracket) then
      SetError(PE_UnexpectedToken)
    else
      Nodes.CloseBracket := ApplyCurrentToken();

  Result := TCharFunc.Create(Self, Nodes);
end;

function TMySQLParser.ParseCheckStmt(): TOffset;
var
  Nodes: TCheckStmt.TNodes;
begin
  FillChar(Nodes, SizeOf(Nodes), 0);

  Nodes.StmtTag := ParseTag(kiCHECK, kiTABLE);

  if (not Error) then
    Nodes.TablesList := ParseList(False, ParseTableIdent);

  if (not Error and not EndOfStmt(CurrentToken)) then
    Nodes.OptionList := ParseList(False, ParseCheckStmtOption);

  Result := TCheckStmt.Create(Self, Nodes);
end;

function TMySQLParser.ParseCheckStmtOption(): TOffset;
var
  Nodes: TCheckStmt.TOption.TNodes;
begin
  FillChar(Nodes, SizeOf(Nodes), 0);

  if (EndOfStmt(CurrentToken)) then
    SetError(PE_IncompleteStmt)
  else if (TokenPtr(CurrentToken)^.KeywordIndex = kiFOR) then
    Nodes.OptionTag := ParseTag(kiFOR, kiUPGRADE)
  else if (TokenPtr(CurrentToken)^.KeywordIndex = kiQUICK) then
    Nodes.OptionTag := ParseTag(kiQUICK)
  else if (TokenPtr(CurrentToken)^.KeywordIndex = kiFAST) then
    Nodes.OptionTag := ParseTag(kiFAST)
  else if (TokenPtr(CurrentToken)^.KeywordIndex = kiMEDIUM) then
    Nodes.OptionTag := ParseTag(kiMEDIUM)
  else if (TokenPtr(CurrentToken)^.KeywordIndex = kiEXTENDED) then
    Nodes.OptionTag := ParseTag(kiEXTENDED)
  else if (TokenPtr(CurrentToken)^.KeywordIndex = kiCHANGED) then
    Nodes.OptionTag := ParseTag(kiCHANGED)
  else
    SetError(PE_UnexpectedToken);

  Result := TCheckStmt.TOption.Create(Self, Nodes);
end;

function TMySQLParser.ParseChecksumStmt(): TOffset;
var
  Nodes: TChecksumStmt.TNodes;
begin
  FillChar(Nodes, SizeOf(Nodes), 0);

  Nodes.StmtTag := ParseTag(kiCHECKSUM, kiTABLE);

  if (not Error) then
    Nodes.TablesList := ParseList(False, ParseTableIdent);

  if (not Error and not EndOfStmt(CurrentToken)) then
    if (TokenPtr(CurrentToken)^.KeywordIndex = kiQUICK) then
      Nodes.OptionTag := ParseTag(kiQUICK)
    else if (TokenPtr(CurrentToken)^.KeywordIndex = kiEXTENDED) then
      Nodes.OptionTag := ParseTag(kiEXTENDED)
    else
      SetError(PE_UnexpectedToken);

  Result := TChecksumStmt.Create(Self, Nodes);
end;

function TMySQLParser.ParseCloseStmt(): TOffset;
var
  Nodes: TCloseStmt.TNodes;
begin
  FillChar(Nodes, SizeOf(Nodes), 0);

  Nodes.CloseTag := ParseTag(kiCLOSE);

  if (not Error) then
    Nodes.CursorIdent := ParseDbIdent(ditCursor);

  Result := TCloseStmt.Create(Self, Nodes);
end;

function TMySQLParser.ParseCommitStmt(): TOffset;
var
  Nodes: TCommitStmt.TNodes;
begin
  FillChar(Nodes, SizeOf(Nodes), 0);

  if (not EndOfStmt(NextToken[1]) and (TokenPtr(NextToken[1])^.KeywordIndex = kiWORK)) then
    Nodes.StmtTag := ParseTag(kiCOMMIT, kiWORK)
  else
    Nodes.StmtTag := ParseTag(kiCOMMIT);

  if (not Error and not EndOfStmt(CurrentToken) and (TokenPtr(CurrentToken)^.KeywordIndex = kiAND)) then
    if (not EndOfStmt(NextToken[1]) and (TokenPtr(NextToken[1])^.KeywordIndex = kiNO)) then
      Nodes.ChainTag := ParseTag(kiAND, kiNO, kiCHAIN)
    else
      Nodes.ChainTag := ParseTag(kiAND, kiCHAIN);

  if (not Error and not EndOfStmt(CurrentToken)) then
    if (TokenPtr(NextToken[1])^.KeywordIndex = kiNO) then
      Nodes.ReleaseTag := ParseTag(kiNO, kiRELEASE)
    else if (TokenPtr(NextToken[1])^.KeywordIndex = kiRELEASE) then
      Nodes.ReleaseTag := ParseTag(kiRELEASE);

  Result := TCommitStmt.Create(Self, Nodes);
end;

function TMySQLParser.ParseCompoundStmt(): TOffset;
var
  BeginLabelToken: TOffset;
  Nodes: TCompoundStmt.TNodes;
begin
  FillChar(Nodes, SizeOf(Nodes), 0);
  BeginLabelToken := 0;

  if (not EndOfStmt(CurrentToken) and (TokenPtr(CurrentToken)^.TokenType = ttIdent)
    and not EndOfStmt(NextToken[1]) and (TokenPtr(NextToken[1])^.TokenType = ttColon)) then
  begin
    BeginLabelToken := CurrentToken;
    Nodes.BeginLabel := ParseBeginLabel();
  end;

  if (not Error) then
    Nodes.BeginTag := ParseTag(kiBEGIN);

  if (not Error and not EndOfStmt(CurrentToken) and (TokenPtr(CurrentToken)^.KeywordIndex <> kiEND)) then
    Nodes.StmtList := ParseList(False, ParsePL_SQLStmt, ttDelimiter);

  if (not Error) then
    Nodes.EndTag := ParseTag(kiEND);

  if (not Error and not EndOfStmt(CurrentToken) and (TokenPtr(CurrentToken)^.TokenType = ttIdent)) then
    if ((Nodes.BeginLabel = 0) or (StrIComp(PChar(TokenPtr(CurrentToken)^.AsString), PChar(TokenPtr(BeginLabelToken)^.AsString)) <> 0)) then
      SetError(PE_UnexpectedToken)
    else
      Nodes.EndLabel := ParseEndLabel();

  Result := TCompoundStmt.Create(Self, Nodes);
end;

function TMySQLParser.ParseConvertFunc(): TOffset;
var
  Nodes: TConvertFunc.TNodes;
begin
  FillChar(Nodes, SizeOf(Nodes), 0);

  Nodes.FuncToken := ApplyCurrentToken(utFunction);

  if (not Error) then
    if (EndOfStmt(CurrentToken)) then
      SetError(PE_IncompleteStmt)
    else if (TokenPtr(CurrentToken)^.TokenType <> ttOpenBracket) then
      SetError(PE_UnexpectedToken)
    else
      Nodes.OpenBracket := ApplyCurrentToken();

  if (not Error) then
    Nodes.Expr := ParseExpr();

  if (not Error) then
    if (EndOfStmt(CurrentToken)) then
      SetError(PE_IncompleteStmt)
    else if (TokenPtr(CurrentToken)^.TokenType = ttComma) then
    begin
      Nodes.Comma := ApplyCurrentToken();
      if (not Error) then
        if (EndOfStmt(CurrentToken)) then
          SetError(PE_IncompleteStmt)
        else if (TokenPtr(CurrentToken)^.TokenType <> ttIdent) then
          SetError(PE_UnexpectedToken)
        else
          Nodes.Datatype := ParseDatatype();
    end
    else if (TokenPtr(CurrentToken)^.KeywordIndex = kiUSING) then
    begin
      Nodes.UsingTag := ParseTag(kiUSING);
      if (not Error) then
        if (EndOfStmt(CurrentToken)) then
          SetError(PE_IncompleteStmt)
        else if (TokenPtr(CurrentToken)^.TokenType <> ttIdent) then
          SetError(PE_UnexpectedToken)
        else
          Nodes.CharsetIdent := ApplyCurrentToken();
    end
    else
      SetError(PE_UnexpectedToken);

  if (not Error) then
    if (EndOfStmt(CurrentToken)) then
      SetError(PE_IncompleteStmt)
    else if (TokenPtr(CurrentToken)^.TokenType <> ttCloseBracket) then
      SetError(PE_UnexpectedToken)
    else
      Nodes.CloseBracket := ApplyCurrentToken();

  Result := TConvertFunc.Create(Self, Nodes);
end;

function TMySQLParser.ParseFieldIdent(): TOffset;
begin
  Result := ParseDbIdent(ditField);
end;

function TMySQLParser.ParseCreateDatabaseStmt(): TOffset;
var
  Nodes: TCreateDatabaseStmt.TNodes;
begin
  FillChar(Nodes, SizeOf(Nodes), 0);

  Nodes.CreateTag := ParseTag(kiCREATE);

  if (not Error) then
    if (EndOfStmt(CurrentToken)) then
      SetError(PE_IncompleteStmt)
    else if (TokenPtr(CurrentToken)^.KeywordIndex = kiSCHEMA) then
      Nodes.DatabaseTag := ParseTag(kiSCHEMA)
    else
      Nodes.DatabaseTag := ParseTag(kiDATABASE);

  if (not Error and not EndOfStmt(CurrentToken) and (TokenPtr(CurrentToken)^.KeywordIndex = kiIF)) then
    Nodes.IfNotExistsTag := ParseTag(kiIF, kiNOT, kiEXISTS);

  if (not Error) then
    if (EndOfStmt(CurrentToken)) then
      SetError(PE_IncompleteStmt)
    else if (not (TokenPtr(CurrentToken)^.TokenType in ttIdents)) then
      SetError(PE_UnexpectedToken)
    else
      Nodes.DatabaseIdent := ParseDatabaseIdent();

  while (not Error and not EndOfStmt(CurrentToken)) do
    if ((Nodes.CharacterSetValue = 0) and (TokenPtr(CurrentToken)^.KeywordIndex = kiCHARACTER)) then
      Nodes.CharacterSetValue := ParseValue(WordIndices(kiCHARACTER, kiSET), vaNo, ParseIdent)
    else if ((Nodes.CollateValue = 0) and (TokenPtr(CurrentToken)^.KeywordIndex = kiCOLLATE)) then
      Nodes.CollateValue := ParseValue(kiCOLLATE, vaNo, ParseIdent)
    else if ((Nodes.CollateValue = 0)
      and not EndOfStmt(NextToken[1]) and (TokenPtr(CurrentToken)^.KeywordIndex = kiDEFAULT) and (TokenPtr(NextToken[1])^.KeywordIndex = kiCHARACTER)) then
      Nodes.CharacterSetValue := ParseValue(WordIndices(kiDEFAULT, kiCHARACTER, kiSET), vaNo, ParseIdent)
    else if ((Nodes.CollateValue = 0)
      and not EndOfStmt(NextToken[1]) and (TokenPtr(CurrentToken)^.KeywordIndex = kiDEFAULT) and (TokenPtr(NextToken[1])^.KeywordIndex = kiCHARSET)) then
      Nodes.CharacterSetValue := ParseValue(WordIndices(kiDEFAULT, kiCHARSET), vaNo, ParseIdent)
    else if ((Nodes.CollateValue = 0)
      and not EndOfStmt(NextToken[1]) and (TokenPtr(CurrentToken)^.KeywordIndex = kiDEFAULT) and (TokenPtr(NextToken[1])^.KeywordIndex = kiCOLLATE)) then
      Nodes.CollateValue := ParseValue(WordIndices(kiDEFAULT, kiCOLLATE), vaNo, ParseIdent)
    else
      SetError(PE_UnexpectedToken);

  Result := TCreateDatabaseStmt.Create(Self, Nodes);
end;

function TMySQLParser.ParseCreateEventStmt(): TOffset;
var
  Nodes: TCreateEventStmt.TNodes;
begin
  FillChar(Nodes, SizeOf(Nodes), 0);

  Nodes.CreateTag := ParseTag(kiCREATE);

  if (not Error and not EndOfStmt(CurrentToken) and (TokenPtr(CurrentToken)^.KeywordIndex = kiDEFINER)) then
    Nodes.DefinerNode := ParseDefinerValue();

  if (not Error) then
    Nodes.EventTag := ParseTag(kiEVENT);

  if (not Error and not EndOfStmt(CurrentToken) and (TokenPtr(CurrentToken)^.KeywordIndex = kiIF)) then
    Nodes.IfNotExistsTag := ParseTag(kiIF, kiNOT, kiEXISTS);

  if (not Error) then
    Nodes.EventIdent := ParseEventIdent();

  if (not Error) then
    Nodes.OnScheduleValue := ParseValue(WordIndices(kiON, kiSCHEDULE), vaNo, ParseSchedule);

  if (not Error and not EndOfStmt(CurrentToken) and (TokenPtr(CurrentToken)^.KeywordIndex = kiON) and not EndOfStmt(NextToken[1]) and (TokenPtr(NextToken[1])^.KeywordIndex = kiCOMPLETION)) then
    if (not EndOfStmt(NextToken[2]) and (TokenPtr(NextToken[2])^.KeywordIndex = kiNOT)) then
      Nodes.OnCompletitionTag := ParseTag(kiON, kiCOMPLETION, kiNOT, kiPRESERVE)
    else
      Nodes.OnCompletitionTag := ParseTag(kiON, kiCOMPLETION, kiPRESERVE);

  if (not Error and not EndOfStmt(CurrentToken)) then
    if (TokenPtr(CurrentToken)^.KeywordIndex = kiENABLE) then
      Nodes.EnableTag := ParseTag(kiENABLE)
    else if ((TokenPtr(CurrentToken)^.KeywordIndex = kiDISABLE)
      and not EndOfStmt(NextToken[1]) and (TokenPtr(NextToken[1])^.KeywordIndex = kiON)) then
      Nodes.EnableTag := ParseTag(kiDISABLE, kiON, kiSLAVE)
    else if (TokenPtr(CurrentToken)^.KeywordIndex = kiDISABLE) then
      Nodes.EnableTag := ParseTag(kiDISABLE);

  if (not Error and not EndOfStmt(CurrentToken) and (TokenPtr(CurrentToken)^.KeywordIndex = kiCOMMENT)) then
    Nodes.CommentValue := ParseValue(kiCOMMENT, vaNo, ParseString);

  if (not Error) then
    Nodes.DoTag := ParseTag(kiDO);

  if (not Error) then
    if (EndOfStmt(CurrentToken)) then
      SetError(PE_IncompleteStmt)
    else
      Nodes.Body := ParsePL_SQLStmt();

  Result := TCreateEventStmt.Create(Self, Nodes);
end;

function TMySQLParser.ParseCreateIndexStmt(): TOffset;
var
  Found: Boolean;
  Nodes: TCreateIndexStmt.TNodes;
begin
  FillChar(Nodes, SizeOf(Nodes), 0);

  Nodes.CreateTag := ParseTag(kiCREATE);

  if (not Error and not EndOfStmt(CurrentToken)) then
    if (TokenPtr(CurrentToken)^.KeywordIndex = kiUNIQUE) then
      Nodes.IndexTag := ParseTag(kiUNIQUE, kiINDEX)
    else if (TokenPtr(CurrentToken)^.KeywordIndex = kiFULLTEXT) then
      Nodes.IndexTag := ParseTag(kiFULLTEXT, kiINDEX)
    else if (TokenPtr(CurrentToken)^.KeywordIndex = kiSPATIAL) then
      Nodes.IndexTag := ParseTag(kiSPATIAL, kiINDEX)
    else if (TokenPtr(CurrentToken)^.KeywordIndex = kiINDEX) then
      Nodes.IndexTag := ParseTag(kiINDEX)
    else
      SetError(PE_UnexpectedToken);

  if (not Error) then
    Nodes.IndexIdent := ParseKeyIdent();

  if (not Error and not EndOfStmt(CurrentToken) and (TokenPtr(CurrentToken)^.KeywordIndex = kiUSING)) then
    Nodes.IndexTypeValue := ParseValue(kiUSING, vaNo, WordIndices(kiBTREE, kiHASH));

  if (not Error) then
  begin
    Nodes.OnTag := ParseTag(kiON);

    if (not Error) then
      Nodes.TableIdent := ParseTableIdent();
  end;

  if (not Error) then
    Nodes.KeyColumnList := ParseList(True, ParseCreateTableStmtKeyColumn);

  Found := True;
  while (not Error and Found and not EndOfStmt(CurrentToken)) do
    if ((Nodes.AlgorithmValue = 0) and (TokenPtr(CurrentToken)^.KeywordIndex = kiALGORITHM)) then
      Nodes.AlgorithmValue := ParseValue(kiALGORITHM, vaAuto, WordIndices(kiDEFAULT, kiINPLACE, kiCOPY))
    else if ((Nodes.CommentValue = 0) and (TokenPtr(CurrentToken)^.KeywordIndex = kiCOMMENT)) then
      Nodes.CommentValue := ParseValue(kiCOMMENT, vaAuto, ParseString)
    else if ((Nodes.KeyBlockSizeValue = 0) and (TokenPtr(CurrentToken)^.KeywordIndex = kiKEY_BLOCK_SIZE)) then
      Nodes.KeyBlockSizeValue := ParseValue(kiKEY_BLOCK_SIZE, vaAuto, ParseInteger)
    else if ((Nodes.LockValue = 0) and (TokenPtr(CurrentToken)^.KeywordIndex = kiLOCK)) then
      Nodes.LockValue := ParseValue(kiLOCK, vaAuto, WordIndices(kiDEFAULT, kiNONE, kiSHARED, kiEXCLUSIVE))
    else if ((Nodes.IndexTypeValue = 0) and (TokenPtr(CurrentToken)^.KeywordIndex = kiUSING)) then
      Nodes.IndexTypeValue := ParseValue(kiUSING, vaNo, WordIndices(kiBTREE, kiHASH))
    else if ((Nodes.ParserValue = 0) and (TokenPtr(CurrentToken)^.KeywordIndex = kiWITH)) then
      Nodes.ParserValue := ParseValue(WordIndices(kiWITH, kiPARSER), vaNo, ParseString)
    else
      Found := False;

  Result := TCreateIndexStmt.Create(Self, Nodes);
end;

function TMySQLParser.ParseCreateRoutineStmt(const ARoutineType: TRoutineType): TOffset;
var
  Nodes: TCreateRoutineStmt.TNodes;
begin
  FillChar(Nodes, SizeOf(Nodes), 0);

  Nodes.CreateTag := ParseTag(kiCREATE);

  if (not Error and not EndOfStmt(CurrentToken) and (TokenPtr(CurrentToken)^.KeywordIndex = kiDEFINER)) then
    Nodes.DefinerNode := ParseDefinerValue();

  if (not Error) then
    if (ARoutineType = rtFunction) then
      Nodes.RoutineTag := ParseTag(kiFUNCTION)
    else
      Nodes.RoutineTag := ParseTag(kiPROCEDURE);

  if (not Error) then
    if (EndOfStmt(CurrentToken)) then
      SetError(PE_IncompleteStmt)
    else if (not (TokenPtr(CurrentToken)^.TokenType in ttIdents)) then
      SetError(PE_UnexpectedToken)
    else if (ARoutineType = rtFunction) then
      Nodes.Ident := ParseDbIdent(ditFunction)
    else
      Nodes.Ident := ParseDbIdent(ditProcedure);

  if (not Error) then
    if (EndOfStmt(CurrentToken)) then
      SetError(PE_IncompleteStmt)
    else if (TokenPtr(CurrentToken)^.TokenType <> ttOpenBracket) then
      SetError(PE_UnexpectedToken)
    else
      Nodes.OpenBracket := ApplyCurrentToken();

  if (not Error) then
    if (EndOfStmt(CurrentToken)) then
      SetError(PE_IncompleteStmt)
    else if (TokenPtr(CurrentToken)^.TokenType <> ttCloseBracket) then
      if (ARoutineType = rtFunction) then
        Nodes.ParameterList := ParseList(False, ParseFunctionParam)
      else
        Nodes.ParameterList := ParseList(False, ParseProcedureParam);

  if (not Error) then
    if (EndOfStmt(CurrentToken)) then
      SetError(PE_IncompleteStmt)
    else if (TokenPtr(CurrentToken)^.TokenType <> ttCloseBracket) then
      SetError(PE_UnexpectedToken)
    else
      Nodes.CloseBracket := ApplyCurrentToken();

  if (not Error and (ARoutineType = rtFunction)) then
    if (EndOfStmt(CurrentToken)) then
      SetError(PE_IncompleteStmt)
    else if (TokenPtr(CurrentToken)^.KeywordIndex <> kiRETURNS) then
      SetError(PE_UnexpectedToken)
    else
      Nodes.Returns := ParseFunctionReturns();

  if (not Error and not EndOfStmt(CurrentToken)) then
    Nodes.CharacteristicList := ParseCreateRoutineStmtCharacteristList();

  if (not Error) then
    if (EndOfStmt(CurrentToken)) then
      SetError(PE_IncompleteStmt)
    else
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

function TMySQLParser.ParseCreateRoutineStmtCharacteristList(): TOffset;
var
  Characteristics: array of TOffset;
  CommentFound: Boolean;
  DeterministicFound: Boolean;
  Found: Boolean;
  LanguageFound: Boolean;
  ListNodes: TList.TNodes;
  SQLSecurityFound: Boolean;
  Characteristic: TOffset;
begin
  SetLength(Characteristics, 0);

  if (not EndOfStmt(CurrentToken)) then
  begin
    CommentFound := False;
    DeterministicFound := False;
    Found := False;
    LanguageFound := False;
    SQLSecurityFound := False;

    repeat
      Characteristic := 0;
      if (TokenPtr(CurrentToken)^.KeywordIndex = kiCOMMENT) then
      begin
        if (CommentFound) then
          SetError(PE_UnexpectedToken)
        else
          Characteristic := ParseValue(kiCOMMENT, vaNo, ParseString);
        CommentFound := True;
      end
      else if (TokenPtr(CurrentToken)^.KeywordIndex = kiLANGUAGE) then
      begin
        if (LanguageFound) then
          SetError(PE_UnexpectedToken)
        else
          Characteristic := ParseTag(kiLANGUAGE, kiSQL);
        LanguageFound := True;
      end
      else if ((TokenPtr(CurrentToken)^.KeywordIndex = kiNOT)
        and not EndOfStmt(NextToken[1]) and (TokenPtr(NextToken[1])^.KeywordIndex = kiDETERMINISTIC)) then
      begin
        if (DeterministicFound) then
          SetError(PE_UnexpectedToken)
        else
          Characteristic := ParseTag(kiNOT, kiDETERMINISTIC);
        DeterministicFound := True;
      end
      else if (TokenPtr(CurrentToken)^.KeywordIndex = kiDETERMINISTIC) then
      begin
        if (DeterministicFound) then
          SetError(PE_UnexpectedToken)
        else
          Characteristic := ParseTag(kiDETERMINISTIC);
        DeterministicFound := True;
      end
      else if ((TokenPtr(CurrentToken)^.KeywordIndex = kiCONTAINS)
        and not EndOfStmt(NextToken[1]) and (TokenPtr(NextToken[1])^.KeywordIndex = kiSQL)) then
      begin
        if (Found) then
          SetError(PE_UnexpectedToken)
        else
          Characteristic := ParseTag(kiCONTAINS, kiSQL);
        Found := True;
      end
      else if ((TokenPtr(CurrentToken)^.KeywordIndex = kiNO)
        and not EndOfStmt(NextToken[1]) and (TokenPtr(NextToken[1])^.KeywordIndex = kiSQL)) then
      begin
        if (Found) then
          SetError(PE_UnexpectedToken)
        else
          Characteristic := ParseTag(kiNO, kiSQL);
        Found := True;
      end
      else if ((TokenPtr(CurrentToken)^.KeywordIndex = kiREADS)
        and not EndOfStmt(NextToken[1]) and (TokenPtr(NextToken[1])^.KeywordIndex = kiSQL)
        and not EndOfStmt(NextToken[2]) and (TokenPtr(NextToken[2])^.KeywordIndex = kiDATA)) then
      begin
        if (Found) then
          SetError(PE_UnexpectedToken)
        else
          Characteristic := ParseTag(kiREADS, kiSQL, kiDATA);
        Found := True;
      end
      else if ((TokenPtr(CurrentToken)^.KeywordIndex = kiMODIFIES)
        and not EndOfStmt(NextToken[1]) and (TokenPtr(NextToken[1])^.KeywordIndex = kiSQL)
        and not EndOfStmt(NextToken[2]) and (TokenPtr(NextToken[2])^.KeywordIndex = kiDATA)) then
      begin
        if (Found) then
          SetError(PE_UnexpectedToken)
        else
          Characteristic := ParseTag(kiMODIFIES, kiSQL, kiDATA);
        Found := True;
      end
      else if ((TokenPtr(CurrentToken)^.KeywordIndex = kiSQL)
        and not EndOfStmt(NextToken[1]) and (TokenPtr(NextToken[1])^.KeywordIndex = kiSECURITY)
        and not EndOfStmt(NextToken[2]) and (TokenPtr(NextToken[2])^.KeywordIndex = kiDEFINER)) then
      begin
        if (SQLSecurityFound) then
          SetError(PE_UnexpectedToken)
        else
        begin
          Characteristic := ParseTag(kiSQL, kiSECURITY, kiDEFINER);
          SQLSecurityFound := True;
        end;
      end
      else if ((TokenPtr(CurrentToken)^.KeywordIndex = kiSQL)
        and not EndOfStmt(NextToken[1]) and (TokenPtr(NextToken[1])^.KeywordIndex = kiSECURITY)
        and not EndOfStmt(NextToken[2]) and (TokenPtr(NextToken[2])^.KeywordIndex = kiINVOKER)) then
      begin
        if (SQLSecurityFound) then
          SetError(PE_UnexpectedToken)
        else
        begin
          Characteristic := ParseTag(kiSQL, kiSECURITY, kiINVOKER);
          SQLSecurityFound := True;
        end;
      end;

      if (Characteristic > 0) then
      begin
        SetLength(Characteristics, Length(Characteristics) + 1);
        Characteristics[Length(Characteristics) - 1] := Characteristic;
      end;
    until (Error or EndOfStmt(CurrentToken) or (Characteristic = 0));
  end;

  if (Length(Characteristics) = 0) then
    Result := 0
  else
  begin
    FillChar(ListNodes, SizeOf(ListNodes), 0);
    Result := TList.Create(Self, ListNodes, ttUnknown, Length(Characteristics), Characteristics);
    SetLength(Characteristics, 0);
  end;
end;

function TMySQLParser.ParseCreateServerStmt(): TOffset;
var
  Nodes: TCreateServerStmt.TNodes;
begin
  FillChar(Nodes, SizeOf(Nodes), 0);

  Nodes.CreateTag := ParseTag(kiCREATE);

  if (not Error) then
    Nodes.ServerTag := ParseTag(kiSERVER);

  if (not Error) then
    Nodes.Ident := ParseDbIdent(ditServer);

  if (not Error) then
    if (EndOfStmt(CurrentToken)) then
      SetError(PE_IncompleteStmt)
    else if (TokenPtr(CurrentToken)^.KeywordIndex <> kiFOREIGN) then
      SetError(PE_UnexpectedToken)
    else
      Nodes.ForeignDataWrapperValue := ParseValue(WordIndices(kiFOREIGN, kiDATA, kiWRAPPER), vaNo, ParseString);

  if (not Error) then
    Nodes.Options.Tag := ParseTag(kiOPTIONS);

  if (not Error) then
    if (EndOfStmt(CurrentToken)) then
      SetError(PE_IncompleteStmt)
    else
      Nodes.Options.List := ParseCreateServerStmtOptionList();

  Result := TCreateServerStmt.Create(Self, Nodes);
end;

function TMySQLParser.ParseCreateServerStmtOptionList(): TOffset;
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

  if (EndOfStmt(CurrentToken)) then
    SetError(PE_IncompleteStmt)
  else if (TokenPtr(CurrentToken)^.TokenType <> ttOpenBracket) then
    SetError(PE_UnexpectedToken)
  else
    ListNodes.OpenBracket := ApplyCurrentToken();

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
      if (EndOfStmt(CurrentToken)) then
        SetError(PE_IncompleteStmt)
      else if (not HostFound and (TokenPtr(CurrentToken).KeywordIndex = kiHOST)) then
      begin
        Children[ChildrenIndex] := ParseValue(kiHOST, vaNo, ParseString);
        HostFound := True;
      end
      else if (not DatabaseFound and (TokenPtr(CurrentToken).KeywordIndex = kiDATABASE)) then
      begin
        Children[ChildrenIndex] := ParseValue(kiDATABASE, vaNo, ParseString);
        DatabaseFound := True;
      end
      else if (not UserFound and (TokenPtr(CurrentToken).KeywordIndex = kiUSER)) then
      begin
        Children[ChildrenIndex] := ParseValue(kiUSER, vaNo, ParseString);
        UserFound := True;
      end
      else if (not PasswordFound and (TokenPtr(CurrentToken).KeywordIndex = kiPASSWORD)) then
      begin
        Children[ChildrenIndex] := ParseValue(kiPASSWORD, vaNo, ParseString);
        PasswordFound := True;
      end
      else if (not SocketFound and (TokenPtr(CurrentToken).KeywordIndex = kiSOCKET)) then
      begin
        Children[ChildrenIndex] := ParseValue(kiSOCKET, vaNo, ParseString);
        SocketFound := True;
      end
      else if (not OwnerFound and (TokenPtr(CurrentToken).KeywordIndex = kiOWNER)) then
      begin
        Children[ChildrenIndex] := ParseValue(kiOWNER, vaNo, ParseString);
        OwnerFound := True;
      end
      else if (not PortFound and (TokenPtr(CurrentToken).KeywordIndex = kiPORT)) then
      begin
        Children[ChildrenIndex] := ParseValue(kiPort, vaNo, ParseInteger);
        PortFound := True;
      end
      else
        SetError(PE_UnexpectedToken);

      Inc(ChildrenIndex);

      DelimiterFound := not Error and not EndOfStmt(CurrentToken) and (TokenPtr(CurrentToken)^.TokenType = ttComma);
      if (DelimiterFound) then
      begin
        Children[ChildrenIndex] := ApplyCurrentToken(); // Delimiter
        Inc(ChildrenIndex);
      end;
    until (Error or not DelimiterFound);
  end;

  if (not Error) then
    if (EndOfStmt(CurrentToken)) then
      SetError(PE_IncompleteStmt)
    else if (TokenPtr(CurrentToken)^.TokenType <> ttCloseBracket) then
      SetError(PE_UnexpectedToken)
    else
      ListNodes.CloseBracket := ApplyCurrentToken();

  Result := TList.Create(Self, ListNodes, ttComma, ChildrenIndex, Children);
end;

function TMySQLParser.ParseCreateStmt(): TOffset;
var
  Index: Integer;
begin
  Result := 0;

  Index := 1;
  if (not EndOfStmt(NextToken[Index]) and (TokenPtr(NextToken[Index])^.KeywordIndex = kiOR)) then
  begin
    Inc(Index);
    if (EndOfStmt(NextToken[Index])) then
      SetError(PE_IncompleteStmt)
    else if (TokenPtr(NextToken[Index])^.KeywordIndex <> kiREPLACE) then
      SetError(PE_UnexpectedToken, NextToken[Index])
    else
      Inc(Index);
  end;

  if (not Error and not EndOfStmt(NextToken[Index]) and (TokenPtr(NextToken[Index])^.KeywordIndex = kiALGORITHM)) then
  begin
    Inc(Index);
    if (EndOfStmt(NextToken[Index])) then
      SetError(PE_IncompleteStmt)
    else if (TokenPtr(NextToken[Index])^.OperatorType <> otEqual) then
      SetError(PE_UnexpectedToken, NextToken[Index])
    else
    begin
      Inc(Index);

      if (EndOfStmt(NextToken[Index])) then
        SetError(PE_IncompleteStmt)
      else if ((TokenPtr(NextToken[Index])^.KeywordIndex <> kiUNDEFINED)
        and (TokenPtr(NextToken[Index])^.KeywordIndex <> kiMERGE)
        and (TokenPtr(NextToken[Index])^.KeywordIndex <> kiTEMPTABLE)) then
        SetError(PE_UnexpectedToken, NextToken[Index])
      else
        Inc(Index);
    end;
  end;

  if (not Error and not EndOfStmt(NextToken[Index]) and (TokenPtr(NextToken[Index])^.KeywordIndex = kiDEFINER)) then
  begin
    Inc(Index);
    if (EndOfStmt(NextToken[Index])) then
      SetError(PE_IncompleteStmt)
    else if (TokenPtr(NextToken[Index])^.OperatorType <> otEqual) then
      SetError(PE_UnexpectedToken, NextToken[Index])
    else
    begin
      Inc(Index);

      if (EndOfStmt(NextToken[Index])) then
        SetError(PE_IncompleteStmt)
      else if (TokenPtr(NextToken[Index])^.KeywordIndex = kiCURRENT_USER) then
        if (((NextToken[Index + 1] = 0) or (TokenPtr(NextToken[Index + 1])^.TokenType <> ttOpenBracket))
          and ((NextToken[Index + 2] = 0) or (TokenPtr(NextToken[Index + 2])^.TokenType <> ttCloseBracket))) then
          Inc(Index)
        else
          Inc(Index, 3)
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
    Inc(Index);

  if (not Error and not EndOfStmt(NextToken[Index]) and (TokenPtr(NextToken[Index])^.KeywordIndex = kiSQL)) then
  begin
    Inc(Index);
    if (EndOfStmt(NextToken[Index])) then
      SetError(PE_IncompleteStmt)
    else if (TokenPtr(NextToken[Index])^.KeywordIndex <> kiSECURITY) then
      SetError(PE_UnexpectedToken, NextToken[Index])
    else
    begin
      Inc(Index);
      if (EndOfStmt(NextToken[Index])) then
        SetError(PE_IncompleteStmt)
      else if ((TokenPtr(NextToken[Index])^.KeywordIndex <> kiDEFINER)
        and (TokenPtr(NextToken[Index])^.KeywordIndex <> kiINVOKER)) then
        SetError(PE_UnexpectedToken, NextToken[Index])
      else
        Inc(Index);
    end;
  end;

  if (not Error and not EndOfStmt(NextToken[Index]) and (TokenPtr(NextToken[Index])^.KeywordIndex = kiTEMPORARY)) then
    Inc(Index);

  if (not Error) then
    if (EndOfStmt(NextToken[Index])) then
      SetError(PE_IncompleteStmt)
    else if (TokenPtr(NextToken[Index])^.KeywordIndex = kiDATABASE) then
      Result := ParseCreateDatabaseStmt()
    else if (TokenPtr(NextToken[Index])^.KeywordIndex = kiEVENT) then
      Result := ParseCreateEventStmt()
    else if (TokenPtr(NextToken[Index])^.KeywordIndex = kiFUNCTION) then
      Result := ParseCreateRoutineStmt(rtFunction)
    else if (TokenPtr(NextToken[Index])^.KeywordIndex = kiINDEX) then
      Result := ParseCreateIndexStmt()
    else if (TokenPtr(NextToken[Index])^.KeywordIndex = kiPROCEDURE) then
      Result := ParseCreateRoutineStmt(rtProcedure)
    else if (TokenPtr(NextToken[Index])^.KeywordIndex = kiSCHEMA) then
      Result := ParseCreateDatabaseStmt()
    else if (TokenPtr(NextToken[Index])^.KeywordIndex = kiSERVER) then
      Result := ParseCreateServerStmt()
    else if (TokenPtr(NextToken[Index])^.KeywordIndex = kiTABLE) then
      Result := ParseCreateTableStmt()
    else if (TokenPtr(NextToken[Index])^.KeywordIndex = kiTRIGGER) then
      Result := ParseCreateTriggerStmt()
    else if (TokenPtr(NextToken[Index])^.KeywordIndex = kiUSER) then
      Result := ParseCreateUserStmt(False)
    else if (TokenPtr(NextToken[Index])^.KeywordIndex = kiVIEW) then
      Result := ParseCreateViewStmt()
    else
    begin
      SetError(PE_UnexpectedToken, NextToken[Index]);
      Result := ParseUnknownStmt();
    end;
end;

function TMySQLParser.ParseCreateTableStmt(): TOffset;
var
  Found: Boolean;
  Index: Integer;
  Nodes: TCreateTableStmt.TNodes;
  PartitionType: (ptUnknown, ptHash, ptKey, ptRANGE, ptList);
begin
  FillChar(Nodes, SizeOf(Nodes), 0);

  Nodes.CreateTag := ParseTag(kiCREATE);

  if (not Error and not EndOfStmt(CurrentToken) and (TokenPtr(CurrentToken)^.KeywordIndex = kiTEMPORARY)) then
    Nodes.TemporaryTag := ParseTag(kiTEMPORARY);

  if (not Error) then
    Nodes.TableTag := ParseTag(kiTABLE);

  if (not Error and not EndOfStmt(CurrentToken) and (TokenPtr(CurrentToken)^.KeywordIndex = kiIF)) then
    Nodes.IfNotExistsTag := ParseTag(kiIF, kiNOT, kiEXISTS);

  if (not Error) then
    Nodes.TableIdent := ParseTableIdent();

  if (not Error) then
    if (EndOfStmt(CurrentToken)) then
      SetError(PE_IncompleteStmt)
    else if (TokenPtr(CurrentToken)^.KeywordIndex = kiLIKE) then
    begin
      Nodes.LikeTag := ParseTag(kiLIKE);

      if (not Error) then
        Nodes.LikeTableIdent := ParseTableIdent();
    end
    else if ((TokenPtr(CurrentToken)^.TokenType = ttOpenBracket)
      and not EndOfStmt(NextToken[1]) and (TokenPtr(NextToken[1])^.KeywordIndex = kiLIKE)) then
    begin
      Nodes.OpenBracket := ApplyCurrentToken();

      Nodes.LikeTag := ParseTag(kiLIKE);

      if (not Error) then
        Nodes.LikeTableIdent := ParseTableIdent();

      if (not Error) then
        if (EndOfStmt(CurrentToken)) then
          SetError(PE_IncompleteStmt)
        else if (TokenPtr(CurrentToken)^.TokenType <> ttCloseBracket) then
          SetError(PE_UnexpectedToken)
        else
          Nodes.CloseBracket := ApplyCurrentToken();
    end
    else if ((TokenPtr(CurrentToken)^.TokenType = ttOpenBracket)
      and (EndOfStmt(NextToken[1]) or (TokenPtr(NextToken[1])^.KeywordIndex <> kiSELECT))) then
    begin
      Nodes.OpenBracket := ApplyCurrentToken();

      if (EndOfStmt(CurrentToken)) then
        SetError(PE_IncompleteStmt)
      else
        Nodes.DefinitionList := ParseList(False, ParseCreateTableStmtDefinition);

      if (not Error) then
        if (EndOfStmt(CurrentToken)) then
          SetError(PE_IncompleteStmt)
        else if (TokenPtr(CurrentToken)^.TokenType <> ttCloseBracket) then
          SetError(PE_UnexpectedToken)
        else
          Nodes.CloseBracket := ApplyCurrentToken();
    end;

  if (not Error and not EndOfStmt(CurrentToken) and (Nodes.LikeTag = 0)) then
  begin
    Found := True;
    while (not Error and Found and not EndOfStmt(CurrentToken)) do
      if ((Nodes.TableOptionsNodes.AutoIncrementValue = 0) and (TokenPtr(CurrentToken)^.KeywordIndex = kiAUTO_INCREMENT)) then
        Nodes.TableOptionsNodes.AutoIncrementValue := ParseValue(kiAUTO_INCREMENT, vaAuto, ParseInteger)
      else if ((Nodes.TableOptionsNodes.AvgRowLengthValue = 0) and (TokenPtr(CurrentToken)^.KeywordIndex = kiAVG_ROW_LENGTH)) then
        Nodes.TableOptionsNodes.AvgRowLengthValue := ParseValue(kiAVG_ROW_LENGTH, vaAuto, ParseInteger)
      else if ((Nodes.TableOptionsNodes.CharacterSetValue = 0) and (TokenPtr(CurrentToken)^.KeywordIndex = kiCHARACTER)) then
        Nodes.TableOptionsNodes.CharacterSetValue := ParseValue(WordIndices(kiCHARACTER, kiSET), vaAuto, ParseIdent)
      else if ((Nodes.TableOptionsNodes.CharacterSetValue = 0) and (TokenPtr(CurrentToken)^.KeywordIndex = kiCHARSET)) then
        Nodes.TableOptionsNodes.CharacterSetValue := ParseValue(kiCHARSET, vaAuto, ParseIdent)
      else if ((Nodes.TableOptionsNodes.ChecksumValue = 0) and (TokenPtr(CurrentToken)^.KeywordIndex = kiCHECKSUM)) then
        Nodes.TableOptionsNodes.ChecksumValue := ParseValue(kiCHECKSUM, vaAuto, ParseInteger)
      else if ((Nodes.TableOptionsNodes.CollateValue = 0) and (TokenPtr(CurrentToken)^.KeywordIndex = kiCOLLATE)) then
        Nodes.TableOptionsNodes.CollateValue := ParseValue(WordIndices(kiCOLLATE), vaAuto, ParseIdent)
      else if ((Nodes.TableOptionsNodes.CharacterSetValue = 0) and (TokenPtr(CurrentToken)^.KeywordIndex = kiDEFAULT) and not EndOfStmt(NextToken[1]) and (TokenPtr(NextToken[1])^.KeywordIndex = kiCHARSET)) then
        Nodes.TableOptionsNodes.CharacterSetValue := ParseValue(WordIndices(kiDEFAULT, kiCHARSET), vaAuto, ParseIdent)
      else if ((Nodes.TableOptionsNodes.CharacterSetValue = 0) and (TokenPtr(CurrentToken)^.KeywordIndex = kiDEFAULT) and not EndOfStmt(NextToken[2]) and (TokenPtr(NextToken[1])^.KeywordIndex = kiCHARACTER) and (TokenPtr(NextToken[2])^.KeywordIndex = kiSET)) then
        Nodes.TableOptionsNodes.CharacterSetValue := ParseValue(WordIndices(kiDEFAULT, kiCHARACTER, kiSET), vaAuto, ParseIdent)
      else if ((Nodes.TableOptionsNodes.CollateValue = 0) and (TokenPtr(CurrentToken)^.KeywordIndex = kiDEFAULT) and not EndOfStmt(NextToken[1]) and (TokenPtr(NextToken[1])^.KeywordIndex = kiCOLLATE)) then
        Nodes.TableOptionsNodes.CollateValue := ParseValue(WordIndices(kiDEFAULT, kiCOLLATE), vaAuto, ParseIdent)
      else if ((Nodes.TableOptionsNodes.CommentValue = 0) and (TokenPtr(CurrentToken)^.KeywordIndex = kiCOMMENT)) then
        Nodes.TableOptionsNodes.CommentValue := ParseValue(kiCOMMENT, vaAuto, ParseString)
      else if ((Nodes.TableOptionsNodes.CompressValue = 0) and (TokenPtr(CurrentToken)^.KeywordIndex = kiCOMPRESS)) then
        Nodes.TableOptionsNodes.CompressValue := ParseValue(kiCOMPRESS, vaAuto, ParseString)
      else if ((Nodes.TableOptionsNodes.ConnectionValue = 0) and (TokenPtr(CurrentToken)^.KeywordIndex = kiCONNECTION)) then
        Nodes.TableOptionsNodes.ConnectionValue := ParseValue(kiCONNECTION, vaAuto, ParseString)
      else if ((Nodes.TableOptionsNodes.DataDirectoryValue = 0) and (TokenPtr(CurrentToken)^.KeywordIndex = kiDATA)) then
        Nodes.TableOptionsNodes.DataDirectoryValue := ParseValue(WordIndices(kiDATA, kiDIRECTORY), vaAuto, ParseString)
      else if ((Nodes.TableOptionsNodes.DelayKeyWriteValue = 0) and (TokenPtr(CurrentToken)^.KeywordIndex = kiDELAY_KEY_WRITE)) then
        Nodes.TableOptionsNodes.DelayKeyWriteValue := ParseValue(kiDELAY_KEY_WRITE, vaAuto, ParseInteger)
      else if ((Nodes.TableOptionsNodes.EngineValue = 0) and (TokenPtr(CurrentToken)^.KeywordIndex = kiENGINE)) then
        Nodes.TableOptionsNodes.EngineValue := ParseValue(kiENGINE, vaAuto, ParseIdent)
      else if ((Nodes.TableOptionsNodes.IndexDirectoryValue = 0) and (TokenPtr(CurrentToken)^.KeywordIndex = kiINDEX)) then
        Nodes.TableOptionsNodes.IndexDirectoryValue := ParseValue(WordIndices(kiINDEX, kiDIRECTORY), vaAuto, ParseString)
      else if ((Nodes.TableOptionsNodes.InsertMethodValue = 0) and (TokenPtr(CurrentToken)^.KeywordIndex = kiINSERT_METHOD)) then
        Nodes.TableOptionsNodes.InsertMethodValue := ParseValue(kiINSERT_METHOD, vaAuto, WordIndices(kiNO, kiFIRST, kiLAST))
      else if ((Nodes.TableOptionsNodes.KeyBlockSizeValue = 0) and (TokenPtr(CurrentToken)^.KeywordIndex = kiKEY_BLOCK_SIZE)) then
        Nodes.TableOptionsNodes.KeyBlockSizeValue := ParseValue(kiKEY_BLOCK_SIZE, vaAuto, ParseInteger)
      else if ((Nodes.TableOptionsNodes.MaxRowsValue = 0) and (TokenPtr(CurrentToken)^.KeywordIndex = kiMAX_ROWS)) then
        Nodes.TableOptionsNodes.MaxRowsValue := ParseValue(kiMAX_ROWS, vaAuto, ParseInteger)
      else if ((Nodes.TableOptionsNodes.MinRowsValue = 0) and (TokenPtr(CurrentToken)^.KeywordIndex = kiMIN_ROWS)) then
        Nodes.TableOptionsNodes.MinRowsValue := ParseValue(kiMIN_ROWS, vaAuto, ParseInteger)
      else if ((Nodes.TableOptionsNodes.PackKeysValue = 0) and (TokenPtr(CurrentToken)^.KeywordIndex = kiPACK_KEYS)) then
        Nodes.TableOptionsNodes.PackKeysValue := ParseValue(kiPACK_KEYS, vaAuto, ParseExpr)
      else if ((Nodes.TableOptionsNodes.PageChecksum = 0) and (TokenPtr(CurrentToken)^.KeywordIndex = kiPAGE_CHECKSUM)) then
        Nodes.TableOptionsNodes.PageChecksum := ParseValue(kiPAGE_CHECKSUM, vaAuto, ParseInteger)
      else if ((Nodes.TableOptionsNodes.PasswordValue = 0) and (TokenPtr(CurrentToken)^.KeywordIndex = kiPASSWORD)) then
        Nodes.TableOptionsNodes.PasswordValue := ParseValue(kiPASSWORD, vaAuto, ParseString)
      else if ((Nodes.TableOptionsNodes.RowFormatValue = 0) and (TokenPtr(CurrentToken)^.KeywordIndex = kiROW_FORMAT)) then
        Nodes.TableOptionsNodes.RowFormatValue := ParseValue(kiROW_FORMAT, vaAuto, ParseIdent)
      else if ((Nodes.TableOptionsNodes.StatsAutoRecalcValue = 0) and (TokenPtr(CurrentToken)^.KeywordIndex = kiSTATS_AUTO_RECALC)) then
      begin
        if (EndOfStmt(NextToken[2]) or (TokenPtr(NextToken[1])^.OperatorType <> otEqual)) then
          Index := 1
        else
          Index := 2;
        if (EndOfStmt(NextToken[Index])) then
          SetError(PE_IncompleteStmt)
        else if (TokenPtr(NextToken[Index])^.TokenType = ttInteger) then
          Nodes.TableOptionsNodes.StatsAutoRecalcValue := ParseValue(kiSTATS_AUTO_RECALC, vaAuto, ParseInteger)
        else
          Nodes.TableOptionsNodes.StatsAutoRecalcValue := ParseValue(kiSTATS_AUTO_RECALC, vaAuto, WordIndices(kiDEFAULT));
      end
      else if ((Nodes.TableOptionsNodes.StatsPersistentValue = 0) and (TokenPtr(CurrentToken)^.KeywordIndex = kiSTATS_PERSISTENT)) then
      begin
        if (EndOfStmt(NextToken[2]) or (TokenPtr(NextToken[1])^.OperatorType <> otEqual)) then
          Index := 1
        else
          Index := 2;
        if (EndOfStmt(NextToken[Index])) then
          SetError(PE_IncompleteStmt)
        else if (TokenPtr(NextToken[Index])^.TokenType = ttInteger) then
          Nodes.TableOptionsNodes.StatsPersistentValue := ParseValue(kiSTATS_PERSISTENT, vaAuto, ParseInteger)
        else
          Nodes.TableOptionsNodes.StatsPersistentValue := ParseValue(kiSTATS_PERSISTENT, vaAuto, WordIndices(kiDEFAULT));
      end
      else if ((Nodes.TableOptionsNodes.TransactionalValue = 0) and (TokenPtr(CurrentToken)^.KeywordIndex = kiTRANSACTIONAL)) then
        Nodes.TableOptionsNodes.TransactionalValue := ParseValue(kiTRANSACTIONAL, vaAuto, ParseInteger)
      else if ((Nodes.TableOptionsNodes.EngineValue = 0) and (TokenPtr(CurrentToken)^.KeywordIndex = kiTYPE)) then
        Nodes.TableOptionsNodes.EngineValue := ParseValue(kiTYPE, vaAuto, ParseIdent)
      else if ((Nodes.TableOptionsNodes.UnionList = 0) and (TokenPtr(CurrentToken)^.KeywordIndex = kiUNION)) then
        Nodes.TableOptionsNodes.UnionList := ParseCreateTableStmtUnion()
      else
        Found := False;
  end;

  if (not Error and not EndOfStmt(CurrentToken) and (TokenPtr(CurrentToken)^.KeywordIndex = kiPARTITION)) then
    if (Nodes.LikeTag > 0) then
      SetError(PE_UnexpectedToken)
    else
    begin
      Nodes.PartitionOption.Tag := ParseTag(kiPARTITION, kiBY);

      PartitionType := ptUnknown;
      if (not Error) then
        if (EndOfStmt(CurrentToken)) then
          SetError(PE_IncompleteStmt)
        else if (TokenPtr(CurrentToken)^.KeywordIndex = kiHASH) then
        begin
          Nodes.PartitionOption.KindTag := ParseTag(kiHASH);
          PartitionType := ptHash;
        end
        else if ((TokenPtr(CurrentToken)^.KeywordIndex = kiLINEAR) and not EndOfStmt(NextToken[1]) and (TokenPtr(NextToken[1])^.KeywordIndex = kiHASH)) then
        begin
          Nodes.PartitionOption.KindTag := ParseTag(kiLINEAR, kiHASH);
          PartitionType := ptHash;
        end
        else if ((TokenPtr(CurrentToken)^.KeywordIndex = kiLINEAR) and not EndOfStmt(NextToken[1]) and (TokenPtr(NextToken[1])^.KeywordIndex = kiKEY)) then
        begin
          Nodes.PartitionOption.KindTag := ParseTag(kiLINEAR, kiKEY);
          PartitionType := ptKey;
        end
        else if (TokenPtr(CurrentToken)^.KeywordIndex = kiKEY) then
        begin
          Nodes.PartitionOption.KindTag := ParseTag(kiKEY);
          PartitionType := ptKEY;
        end
        else if (TokenPtr(CurrentToken)^.KeywordIndex = kiRANGE) then
        begin
          Nodes.PartitionOption.KindTag := ParseTag(kiRANGE);
          PartitionType := ptRange;
        end
        else if (TokenPtr(CurrentToken)^.KeywordIndex = kiLIST) then
        begin
          Nodes.PartitionOption.KindTag := ParseTag(kiLIST);
          PartitionType := ptList;
        end
        else
          SetError(PE_UnexpectedToken);

      if (not Error) then
        if (PartitionType = ptHash) then
        begin
          Nodes.PartitionOption.Expr := ParseSubArea(ParseExpr);
        end
        else if (PartitionType = ptKey) then
        begin
          if (not EndOfStmt(CurrentToken) and (TokenPtr(CurrentToken)^.KeywordIndex = kiALGORITHM)) then
            Nodes.PartitionOption.AlgorithmValue := ParseValue(kiALGORITHM, vaAuto, ParseInteger);

          if (not Error and not EndOfStmt(CurrentToken) and (TokenPtr(CurrentToken)^.KeywordIndex = kiCOLUMNS)) then
            Nodes.PartitionOption.Columns.Tag := ParseTag(kiCOLUMNS);

          if (not Error) then
            Nodes.PartitionOption.Columns.List := ParseList(True, ParseFieldIdent);
        end
        else if (PartitionType in [ptRange, ptList]) then
        begin
          if (EndOfStmt(CurrentToken)) then
            SetError(PE_IncompleteStmt)
          else if (TokenPtr(CurrentToken)^.KeywordIndex <> kiCOLUMNS) then
            Nodes.PartitionOption.Expr := ParseSubArea(ParseExpr)
          else
          begin
            Nodes.PartitionOption.Columns.Tag := ParseTag(kiCOLUMNS);

            if (not Error) then
              Nodes.PartitionOption.Columns.List := ParseList(True, ParseFieldIdent);
          end;
        end;

      if (not Error and not EndOfStmt(CurrentToken) and (TokenPtr(CurrentToken)^.KeywordIndex = kiPARTITIONS)) then
        Nodes.PartitionOption.Value := ParseValue(kiPARTITIONS, vaNo, ParseInteger);

      if (not Error and not EndOfStmt(CurrentToken) and (TokenPtr(CurrentToken)^.KeywordIndex = kiSUBPARTITION)) then
      begin
        Nodes.PartitionOption.SubPartition.Tag := ParseTag(kiSUBPARTITION, kiBY);

        if (not Error) then
          if (EndOfStmt(CurrentToken)) then
            SetError(PE_IncompleteStmt)
          else if (TokenPtr(CurrentToken)^.KeywordIndex = kiHASH) then
            Nodes.PartitionOption.SubPartition.KindTag := ParseTag(kiHASH)
          else if ((TokenPtr(CurrentToken)^.KeywordIndex = kiLINEAR) and not EndOfStmt(NextToken[1]) and (TokenPtr(NextToken[1])^.KeywordIndex = kiHASH)) then
            Nodes.PartitionOption.SubPartition.KindTag := ParseTag(kiLINEAR, kiHASH)
          else if ((TokenPtr(CurrentToken)^.KeywordIndex = kiLINEAR) and not EndOfStmt(NextToken[1]) and (TokenPtr(NextToken[1])^.KeywordIndex = kiKEY)) then
            Nodes.PartitionOption.SubPartition.KindTag := ParseTag(kiLINEAR, kiKEY)
          else if (TokenPtr(CurrentToken)^.KeywordIndex = kiKEY) then
            Nodes.PartitionOption.SubPartition.KindTag := ParseTag(kiKEY)
          else
            SetError(PE_UnexpectedToken);

        if (not Error) then
          Nodes.PartitionOption.SubPartition.Expr := ParseExpr();

        if (not Error and not EndOfStmt(CurrentToken) and (TokenPtr(CurrentToken)^.KeywordIndex = kiALGORITHM)) then
          Nodes.PartitionOption.SubPartition.AlgorithmValue := ParseValue(kiALGORITHM, vaAuto, ParseInteger);

        if (not Error and not EndOfStmt(CurrentToken) and (TokenPtr(CurrentToken)^.TokenType = ttOpenBracket)) then
          Nodes.PartitionOption.SubPartition.ColumnList := ParseList(True, ParseIdent);

        if (not Error and not EndOfStmt(CurrentToken) and (TokenPtr(CurrentToken)^.KeywordIndex = kiSUBPARTITIONS)) then
          Nodes.PartitionOption.SubPartition.Value := ParseValue(kiSUBPARTITIONS, vaNo, ParseInteger);
      end;
    end;

  if (not Error and (Nodes.LikeTag = 0)
    and not EndOfStmt(CurrentToken) and (TokenPtr(CurrentToken)^.TokenType = ttOpenBracket)
    and (EndOfStmt(NextToken[1]) or (TokenPtr(NextToken[1])^.KeywordIndex <> kiSELECT))) then
    Nodes.PartitionDefinitionList := ParseList(True, ParseCreateTableStmtPartition);

  if (not Error and not EndOfStmt(CurrentToken) and (Nodes.LikeTag = 0)) then
  begin
    if (TokenPtr(CurrentToken)^.KeywordIndex = kiIGNORE) then
      Nodes.IgnoreReplaceTag := ParseTag(kiIGNORE)
    else if (TokenPtr(CurrentToken)^.KeywordIndex = kiREPLACE) then
      Nodes.IgnoreReplaceTag := ParseTag(kiREPLACE);

    if (not Error and not EndOfStmt(CurrentToken) and (TokenPtr(CurrentToken)^.KeywordIndex = kiAS)) then
      Nodes.AsTag := ParseTag(kiAS);

    if (not Error) then
      if (EndOfStmt(CurrentToken) and (Nodes.AsTag > 0)) then
        SetError(PE_IncompleteStmt)
      else if (not EndOfStmt(CurrentToken) and (TokenPtr(CurrentToken)^.KeywordIndex = kiSELECT)) then
        if (Nodes.SelectStmt2 > 0) then
          SetError(PE_UnexpectedToken)
        else
          Nodes.SelectStmt2 := ParseSelectStmt()
      else if (not EndOfStmt(NextToken[1])
        and (TokenPtr(CurrentToken)^.TokenType = ttOpenBracket)
        and (TokenPtr(NextToken[1])^.KeywordIndex = kiSELECT)) then
        if (Nodes.SelectStmt2 > 0) then
          SetError(PE_UnexpectedToken)
        else
          Nodes.SelectStmt2 := ParseSubAreaSelectStmt();
  end;

  Result := TCreateTableStmt.Create(Self, Nodes);
end;

function TMySQLParser.ParseCreateTableStmtField(const Add: TCreateTableStmt.TFieldAdd = caNone): TOffset;
var
  Found: Boolean;
  Length: Integer;
  Nodes: TCreateTableStmt.TField.TNodes;
  Text: PChar;
begin
  FillChar(Nodes, SizeOf(Nodes), 0);

  if (not Error) then
    if (EndOfStmt(CurrentToken)) then
      SetError(PE_IncompleteStmt)
    else if (Add = caAdd) then
      Nodes.AddTag := ParseTag(kiADD)
    else if (Add = caChange) then
      Nodes.AddTag := ParseTag(kiCHANGE)
    else if (Add = caModify) then
      Nodes.AddTag := ParseTag(kiMODIFY);

  if (not Error and (Add <> caNone) and not EndOfStmt(CurrentToken) and (TokenPtr(CurrentToken)^.KeywordIndex = kiCOLUMN)) then
    Nodes.ColumnTag := ParseTag(kiCOLUMN);

  if (not Error and (Add in [caChange, caModify])) then
    Nodes.OldNameIdent := ParseFieldIdent();

  if (not Error and (Add in [caNone, caAdd, caChange])) then
    Nodes.NameIdent := ParseFieldIdent();

  if (not Error) then
    if (EndOfStmt(CurrentToken)) then
      SetError(PE_IncompleteStmt)
    else if (TokenPtr(CurrentToken)^.TokenType <> ttIdent) then
      SetError(PE_UnexpectedToken)
    else
      Nodes.Datatype := ParseDatatype();


  if (not Error and not EndOfStmt(CurrentToken)) then
    if ((TokenPtr(CurrentToken)^.KeywordIndex <> kiGENERATED)
      and (TokenPtr(CurrentToken)^.KeywordIndex <> kiAS)) then
    begin
      Found := True;
      while (not Error and not EndOfStmt(CurrentToken)
        and Found and not (TokenPtr(CurrentToken)^.TokenType in [ttComma, ttCloseBracket])) do
      begin
        Found := False;

        if ((Nodes.NullTag = 0) and (TokenPtr(CurrentToken)^.KeywordIndex = kiNOT)) then
        begin
          Nodes.NullTag := ParseTag(kiNOT, kiNULL);
          Found := True;
        end
        else if (Nodes.NullTag = 0) and ((TokenPtr(CurrentToken)^.KeywordIndex = kiNULL)) then
        begin
          Nodes.NullTag := ParseTag(kiNULL);
          Found := True;
        end
        else if ((Nodes.Real.DefaultValue = 0) and (TokenPtr(CurrentToken)^.KeywordIndex = kiDEFAULT)) then
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
          Found := True;
        end
        else if ((Nodes.Real.OnUpdateTag = 0)
          and (TokenPtr(CurrentToken)^.KeywordIndex = kiON) and (TokenPtr(NextToken[1])^.KeywordIndex = kiUPDATE)
          and (not EndOfStmt(NextToken[2]) and ((TokenPtr(NextToken[2])^.KeywordIndex = kiCURRENT_TIMESTAMP) or (TokenPtr(NextToken[2])^.KeywordIndex = kiCURRENT_TIME) or (TokenPtr(NextToken[2])^.KeywordIndex = kiCURRENT_DATE)))) then
        begin
          if (EndOfStmt(NextToken[3]) or (TokenPtr(NextToken[3])^.TokenType <> ttOpenBracket)) then
            Nodes.Real.OnUpdateTag := ParseTag(kiON, kiUPDATE, TokenPtr(NextToken[2])^.KeywordIndex)
          else
            Nodes.Real.OnUpdateTag := ParseValue(WordIndices(kiON, kiUPDATE), vaNo, ParseFunctionCall);
          Found := True;
        end
        else if ((Nodes.Real.AutoIncrementTag = 0) and (TokenPtr(CurrentToken)^.KeywordIndex = kiAUTO_INCREMENT)) then
        begin
          Nodes.Real.AutoIncrementTag := ParseTag(kiAUTO_INCREMENT);
          Found := True;
        end
        else if ((Nodes.KeyTag = 0)
          and (TokenPtr(CurrentToken)^.KeywordIndex = kiUNIQUE) and not EndOfStmt(NextToken[1]) and (TokenPtr(NextToken[1])^.KeywordIndex = kiKEY)) then
        begin
          Nodes.KeyTag := ParseTag(kiUNIQUE, kiKEY);
          Found := True;
        end
        else if ((Nodes.KeyTag = 0)
          and (TokenPtr(CurrentToken)^.KeywordIndex = kiUNIQUE))  then
        begin
          Nodes.KeyTag := ParseTag(kiUNIQUE);
          Found := True;
        end
        else if ((Nodes.KeyTag = 0)
          and (TokenPtr(CurrentToken)^.KeywordIndex = kiPRIMARY) and not EndOfStmt(NextToken[1]) and (TokenPtr(NextToken[1])^.KeywordIndex = kiKEY)) then
        begin
          Nodes.KeyTag := ParseTag(kiPRIMARY, kiKEY);
          Found := True;
        end
        else if ((Nodes.KeyTag = 0)
          and (TokenPtr(CurrentToken)^.KeywordIndex = kiKEY))  then
        begin
          Nodes.KeyTag := ParseTag(kiKEY);
          Found := True;
        end
        else if ((Nodes.CommentValue = 0) and (TokenPtr(CurrentToken)^.KeywordIndex = kiCOMMENT)) then
        begin
          Nodes.CommentValue := ParseValue(kiCOMMENT, vaNo, ParseString);
          Found := True;
        end
        else if ((Nodes.Real.ColumnFormat = 0) and (TokenPtr(CurrentToken)^.KeywordIndex = kiCOLUMN_FORMAT)) then
        begin
          Nodes.Real.ColumnFormat := ParseValue(kiCOLUMN_FORMAT, vaNo, WordIndices(kiFIXED, kiDYNAMIC, kiDEFAULT));
          Found := True;
        end
        else if ((Nodes.Real.StorageTag = 0) and (TokenPtr(CurrentToken)^.KeywordIndex = kiSTORAGE)) then
        begin
          if (EndOfStmt(NextToken[1])) then
            SetError(PE_IncompleteStmt)
          else if (TokenPtr(NextToken[1])^.KeywordIndex = kiDISK) then
            Nodes.Real.StorageTag := ParseTag(kiSTORAGE, kiDISK)
          else if (TokenPtr(NextToken[1])^.KeywordIndex = kiMEMORY) then
            Nodes.Real.StorageTag := ParseTag(kiSTORAGE, kiMEMORY)
          else if (TokenPtr(NextToken[1])^.KeywordIndex = kiDEFAULT) then
            Nodes.Real.StorageTag := ParseTag(kiSTORAGE, kiDEFAULT)
          else
            SetError(PE_UnexpectedToken, NextToken[1]);
          Found := True;
        end
        else if ((Nodes.Real.Reference = 0) and (TokenPtr(CurrentToken)^.KeywordIndex = kiREFERENCES)) then
          Nodes.Real.Reference := ParseCreateTableStmtReference();
      end;
    end
    else
    begin
      if (TokenPtr(CurrentToken)^.KeywordIndex = kiGENERATED) then
        Nodes.Virtual.GernatedAlwaysTag := ParseTag(kiGENERATED, kiALWAYS);

      if (not Error) then
        Nodes.Virtual.AsTag := ParseTag(kiAS);

      if (not Error) then
        Nodes.Virtual.Expr := ParseSubArea(ParseExpr);

      if (not Error) then
        if (EndOfStmt(CurrentToken)) then
          SetError(PE_IncompleteStmt)
        else if (TokenPtr(CurrentToken)^.KeywordIndex = kiVIRTUAL) then
          Nodes.Virtual.Virtual := ParseTag(kiVIRTUAL)
        else if (TokenPtr(CurrentToken)^.KeywordIndex = kiSTORED) then
          Nodes.Virtual.Virtual := ParseTag(kiSTORED)
        else if (TokenPtr(CurrentToken)^.KeywordIndex = kiPERSISTENT) then
          Nodes.Virtual.Virtual := ParseTag(kiPERSISTENT)
        else
          SetError(PE_UnexpectedToken);

      Found := True;
      while (not Error and not EndOfStmt(CurrentToken)
        and Found and not (TokenPtr(CurrentToken)^.TokenType in [ttComma, ttCloseBracket])) do
      begin
        Found := False;

        if ((Nodes.NullTag = 0) and (TokenPtr(CurrentToken)^.KeywordIndex = kiNOT)) then
        begin
          Nodes.NullTag := ParseTag(kiNOT, kiNULL);
          Found := True;
        end
        else if ((Nodes.NullTag = 0) and (TokenPtr(CurrentToken)^.KeywordIndex = kiNULL)) then
        begin
          Nodes.NullTag := ParseTag(kiNULL);
          Found := True;
        end
        else if ((Nodes.KeyTag = 0)
          and (TokenPtr(CurrentToken)^.KeywordIndex = kiUNIQUE) and not EndOfStmt(NextToken[1]) and (TokenPtr(NextToken[1])^.KeywordIndex = kiKEY)) then
        begin
          Nodes.KeyTag := ParseTag(kiUNIQUE, kiKEY);
          Found := True;
        end
        else if ((Nodes.KeyTag = 0) and (TokenPtr(CurrentToken)^.KeywordIndex = kiUNIQUE)) then
        begin
          Nodes.KeyTag := ParseTag(kiUNIQUE);
          Found := True;
        end
        else if ((Nodes.KeyTag = 0)
          and (TokenPtr(CurrentToken)^.KeywordIndex = kiPRIMARY) and not EndOfStmt(NextToken[1]) and (TokenPtr(NextToken[1])^.KeywordIndex = kiKEY)) then
        begin
          Nodes.KeyTag := ParseTag(kiPRIMARY, kiKEY);
          Found := True;
        end
        else if ((Nodes.KeyTag = 0) and (TokenPtr(CurrentToken)^.KeywordIndex = kiKEY)) then
        begin
          Nodes.KeyTag := ParseTag(kiKEY);
          Found := True;
        end
        else if ((Nodes.CommentValue = 0) and (TokenPtr(CurrentToken)^.KeywordIndex = kiCOMMENT)) then
        begin
          Nodes.CommentValue := ParseValue(kiCOMMENT, vaNo, ParseString);
          Found := True;
        end;
      end;
  end;

  if (not Error and (Add <> caNone) and not EndOfStmt(CurrentToken)) then
    if (TokenPtr(CurrentToken)^.KeywordIndex = kiFIRST) then
      Nodes.Position := ParseTag(kiFIRST)
    else if (TokenPtr(CurrentToken)^.KeywordIndex = kiAFTER) then
      Nodes.Position := ParseValue(kiAFTER, vaNo, ParseFieldIdent);

  Result := TCreateTableStmt.TField.Create(Self, Nodes);
end;

function TMySQLParser.ParseCreateTableStmtFieldDefaultFunc(): TOffset;
var
  Nodes: TMySQLParser.TCreateTableStmt.TField.TDefaultFunc.TNodes;
begin
  FillChar(Nodes, SizeOf(Nodes), 0);

  Nodes.FuncToken := ParseIdent();

  if (not Error) then
    if (EndOfStmt(CurrentToken)) then
      SetError(PE_IncompleteStmt)
    else if (TokenPtr(CurrentToken)^.TokenType <> ttOpenBracket) then
      SetError(PE_UnexpectedToken)
    else
      Nodes.OpenBracket := ApplyCurrentToken();

  if (not Error) then
    if (EndOfStmt(CurrentToken)) then
      SetError(PE_IncompleteStmt)
    else if (TokenPtr(CurrentToken)^.TokenType <> ttCloseBracket) then
      SetError(PE_UnexpectedToken)
    else
      Nodes.CloseBracket := ApplyCurrentToken();

  Result := TMySQLParser.TCreateTableStmt.TField.TDefaultFunc.Create(Self, Nodes);
end;

function TMySQLParser.ParseCreateTableStmtDefinition(): TOffset;
begin
  Result := ParseCreateTableStmtDefinition(False);
end;

function TMySQLParser.ParseCreateTableStmtDefinition(const AlterTableStmt: Boolean): TOffset;
var
  Index: Integer;
  SpecificationType: (stUnknown, stField, stKey, stForeignKey, stPartition);
begin
  if (not AlterTableStmt) then
    Index := 0
  else
    Index := 1; // "ADD"

  Result := 0;
  SpecificationType := stUnknown;

  if (EndOfStmt(NextToken[Index])) then
    SetError(PE_IncompleteStmt)
  else if ((TokenPtr(NextToken[Index])^.KeywordIndex = kiKEY)
    or (TokenPtr(NextToken[Index])^.KeywordIndex = kiFULLTEXT)
    or (TokenPtr(NextToken[Index])^.KeywordIndex = kiINDEX)
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
  else if (TokenPtr(NextToken[Index])^.KeywordIndex = kiPARTITION) then
    SpecificationType := stPartition
  else
    SpecificationType := stField;

  if (not Error) then
    case (SpecificationType) of
      stField:
        if (not AlterTableStmt) then
          Result := ParseCreateTableStmtField(caNone)
        else
          Result := ParseCreateTableStmtField(caAdd);
      stKey: Result := ParseCreateTableStmtKey(AlterTableStmt);
      stForeignKey: Result := ParseCreateTableStmtForeignKey(AlterTableStmt);
      stPartition: Result := ParseCreateTableStmtPartition(AlterTableStmt);
    end;
end;

function TMySQLParser.ParseCreateTableStmtDefinitionPartitionNames(): TOffset;
begin
  if (EndOfStmt(CurrentToken)) then
  begin
    SetError(PE_IncompleteStmt);
    Result := 0;
  end
  else if (TokenPtr(CurrentToken)^.KeywordIndex = kiALL) then
    Result := ParseTag(kiALL)
  else
    Result := ParseList(False, ParsePartitionIdent);
end;

function TMySQLParser.ParseCreateTableStmtForeignKey(const Add: Boolean = False): TOffset;
var
  Nodes: TCreateTableStmt.TForeignKey.TNodes;
begin
  FillChar(Nodes, SizeOf(Nodes), 0);

  if (Add) then
    Nodes.AddTag := ParseTag(kiADD);

  if (not Error and not EndOfStmt(CurrentToken) and (TokenPtr(CurrentToken)^.KeywordIndex = kiCONSTRAINT)) then
  begin
    Nodes.ConstraintTag := ParseTag(kiCONSTRAINT);

    if (not Error and not EndOfStmt(CurrentToken) and (TokenPtr(CurrentToken)^.KeywordIndex <> kiPRIMARY) and (TokenPtr(CurrentToken)^.KeywordIndex <> kiUNIQUE)) then
      Nodes.SymbolIdent := ParseForeignKeyIdent();
  end;

  if (not Error) then
    Nodes.ForeignKeyTag := ParseTag(kiFOREIGN, kiKEY);

  if (not Error and not EndOfStmt(CurrentToken) and (TokenPtr(CurrentToken)^.TokenType <> ttOpenBracket)) then
    Nodes.NameIdent := ParseDbIdent(ditForeignKey);

  if (not Error) then
    Nodes.ColumnNameList := ParseList(True, ParseFieldIdent);

  if (not Error) then
    Nodes.Reference := ParseCreateTableStmtReference();

  Result := TCreateTableStmt.TForeignKey.Create(Self, Nodes);
end;

function TMySQLParser.ParseCreateTableStmtKey(const AlterTableStmt: Boolean): TOffset;
var
  Found: Boolean;
  KeyName: Boolean;
  KeyType: Boolean;
  Nodes: TCreateTableStmt.TKey.TNodes;
begin
  FillChar(Nodes, SizeOf(Nodes), 0);

  if (AlterTableStmt) then
    Nodes.AddTag := ParseTag(kiADD);

  if (not Error and not EndOfStmt(CurrentToken) and (TokenPtr(CurrentToken)^.KeywordIndex = kiCONSTRAINT)) then
  begin
    Nodes.ConstraintTag := ParseTag(kiCONSTRAINT);

    if (not Error and not EndOfStmt(CurrentToken) and (TokenPtr(CurrentToken)^.KeywordIndex <> kiPRIMARY) and (TokenPtr(CurrentToken)^.KeywordIndex <> kiUNIQUE)) then
      if (TokenPtr(CurrentToken)^.TokenType in ttStrings) then
        Nodes.SymbolIdent := ParseString()
      else
        Nodes.SymbolIdent := ParseIdent();
  end;

  KeyName := True; KeyType := False;
  if (not Error) then
    if (EndOfStmt(NextToken[1])) then
      SetError(PE_IncompleteStmt)
    else if ((TokenPtr(CurrentToken)^.KeywordIndex = kiPRIMARY) and (TokenPtr(NextToken[1])^.KeywordIndex = kiKEY)) then
      begin Nodes.KeyTag := ParseTag(kiPRIMARY, kiKEY); KeyName := False; KeyType := True; end
    else if ((TokenPtr(CurrentToken)^.KeywordIndex = kiINDEX)) then
      begin Nodes.KeyTag := ParseTag(kiINDEX); KeyType := True; end
    else if ((TokenPtr(CurrentToken)^.KeywordIndex = kiKEY)) then
      begin Nodes.KeyTag := ParseTag(kiKEY); KeyType := True; end
    else if ((TokenPtr(CurrentToken)^.KeywordIndex = kiUNIQUE) and (TokenPtr(NextToken[1])^.KeywordIndex = kiINDEX)) then
      begin Nodes.KeyTag := ParseTag(kiUNIQUE, kiINDEX); KeyType := True; end
    else if ((TokenPtr(CurrentToken)^.KeywordIndex = kiUNIQUE) and (TokenPtr(NextToken[1])^.KeywordIndex = kiKEY)) then
      begin Nodes.KeyTag := ParseTag(kiUNIQUE, kiKEY); KeyType := True; end
    else if (TokenPtr(CurrentToken)^.KeywordIndex = kiUNIQUE) then
      begin Nodes.KeyTag := ParseTag(kiUNIQUE); KeyType := True; end
    else if ((TokenPtr(CurrentToken)^.KeywordIndex = kiFULLTEXT) and (TokenPtr(NextToken[1])^.KeywordIndex = kiINDEX)) then
      Nodes.KeyTag := ParseTag(kiFULLTEXT, kiINDEX)
    else if ((TokenPtr(CurrentToken)^.KeywordIndex = kiFULLTEXT) and (TokenPtr(NextToken[1])^.KeywordIndex = kiKEY)) then
      Nodes.KeyTag := ParseTag(kiFULLTEXT, kiKEY)
    else if ((TokenPtr(CurrentToken)^.KeywordIndex = kiSPATIAL) and (TokenPtr(NextToken[1])^.KeywordIndex = kiINDEX)) then
      Nodes.KeyTag := ParseTag(kiSPATIAL, kiINDEX)
    else if ((TokenPtr(CurrentToken)^.KeywordIndex = kiSPATIAL) and (TokenPtr(NextToken[1])^.KeywordIndex = kiKEY)) then
      Nodes.KeyTag := ParseTag(kiSPATIAL, kiKEY)
    else
      SetError(PE_UnexpectedToken);

  if (not Error and KeyName) then
    if (EndOfStmt(CurrentToken)) then
      SetError(PE_IncompleteStmt)
    else if ((TokenPtr(CurrentToken)^.KeywordIndex <> kiUSING) and (TokenPtr(CurrentToken)^.TokenType <> ttOpenBracket)) then
      Nodes.KeyIdent := ParseKeyIdent();

  if (not Error and KeyType and not EndOfStmt(CurrentToken) and (TokenPtr(CurrentToken)^.KeywordIndex = kiUSING)) then
    if (EndOfStmt(NextToken[1])) then
      SetError(PE_IncompleteStmt)
    else if (TokenPtr(NextToken[1])^.KeywordIndex = kiBTREE) then
      Nodes.IndexTypeTag := ParseValue(kiUSING, vaNo, WordIndices(kiBTREE, kiBTREE))
    else if (TokenPtr(NextToken[1])^.KeywordIndex = kiHASH) then
      Nodes.IndexTypeTag := ParseValue(kiUSING, vaNo, WordIndices(kiBTREE, kiHASH))
    else
      SetError(PE_UnexpectedToken);

  if (not Error) then
    Nodes.ColumnIdentList := ParseList(True, ParseCreateTableStmtKeyColumn);

  Found := True;
  while (not Error and Found and not EndOfStmt(CurrentToken)) do
    if ((Nodes.CommentValue = 0) and (TokenPtr(CurrentToken)^.KeywordIndex = kiCOMMENT)) then
      Nodes.CommentValue := ParseValue(kiCOMMENT, vaNo, ParseString)
    else if ((Nodes.KeyBlockSizeValue = 0) and (TokenPtr(CurrentToken)^.KeywordIndex = kiKEY_BLOCK_SIZE)) then
      Nodes.KeyBlockSizeValue := ParseValue(kiKEY_BLOCK_SIZE, vaAuto, ParseInteger)
    else if ((Nodes.IndexTypeTag = 0) and (TokenPtr(CurrentToken)^.KeywordIndex = kiUSING)) then
      Nodes.IndexTypeTag := ParseValue(kiUSING, vaNo, WordIndices(kiBTREE, kiHASH))
    else if ((Nodes.ParserValue = 0) and (TokenPtr(CurrentToken)^.KeywordIndex = kiWITH)) then
      Nodes.ParserValue := ParseValue(WordIndices(kiWITH, kiPARSER), vaNo, ParseIdent)
    else
      Found := False;

  Result := TCreateTableStmt.TKey.Create(Self, Nodes);
end;

function TMySQLParser.ParseCreateTableStmtKeyColumn(): TOffset;
var
  Nodes: TCreateTableStmt.TKeyColumn.TNodes;
begin
  FillChar(Nodes, SizeOf(Nodes), 0);

  Nodes.IdentTag := ParseDbIdent(ditField);

  if (not Error and not EndOfStmt(CurrentToken) and (TokenPtr(CurrentToken)^.TokenType = ttOpenBracket)) then
  begin
    Nodes.OpenBracket := ApplyCurrentToken();

    if (not Error) then
      if (EndOfStmt(CurrentToken)) then
        SetError(PE_IncompleteStmt)
      else if (TokenPtr(CurrentToken)^.TokenType <> ttInteger) then
        SetError(PE_UnexpectedToken)
      else
        Nodes.LengthToken := ParseInteger();

    if (not Error) then
      if (EndOfStmt(CurrentToken)) then
        SetError(PE_IncompleteStmt)
      else if (TokenPtr(CurrentToken)^.TokenType <> ttCloseBracket) then
        SetError(PE_UnexpectedToken)
      else
        Nodes.CloseBracket := ApplyCurrentToken();
  end;

  if (not Error and not EndOfStmt(CurrentToken)) then
    if (TokenPtr(CurrentToken)^.KeywordIndex = kiASC) then
      Nodes.SortTag := ParseTag(kiASC)
    else if (TokenPtr(CurrentToken)^.KeywordIndex = kiDESC) then
      Nodes.SortTag := ParseTag(kiDESC);

  Result := TCreateTableStmt.TKeyColumn.Create(Self, Nodes);
end;

function TMySQLParser.ParseCreateTableStmtPartition(): TOffset;
begin
  Result := ParseCreateTableStmtPartition(False);
end;

function TMySQLParser.ParseCreateTableStmtPartition(const Add: Boolean): TOffset;
var
  Found: Boolean;
  Nodes: TCreateTableStmt.TPartition.TNodes;
begin
  FillChar(Nodes, SizeOf(Nodes), 0);

  if (Add) then
    Nodes.AddTag := ParseTag(kiADD);

  if (not Error) then
    Nodes.PartitionTag := ParseTag(kiPARTITION);

  if (not Error) then
    Nodes.NameIdent := ParsePartitionIdent();

  if (not Error and not EndOfStmt(CurrentToken) and (TokenPtr(CurrentToken)^.KeywordIndex = kiVALUES)) then
    Nodes.ValuesNode := ParseCreateTableStmtPartitionValues();

  Found := True;
  while (not Error and Found and not EndOfStmt(CurrentToken)) do
    if ((Nodes.CommentValue = 0) and (TokenPtr(CurrentToken)^.KeywordIndex = kiCOMMENT)) then
      Nodes.CommentValue := ParseValue(kiCOMMENT, vaAuto, ParseString)
    else if ((Nodes.DataDirectoryValue = 0) and (TokenPtr(CurrentToken)^.KeywordIndex = kiDATA)) then
      Nodes.DataDirectoryValue := ParseValue(WordIndices(kiDATA, kiDIRECTORY), vaAuto, ParseString)
    else if ((Nodes.EngineValue = 0) and (TokenPtr(CurrentToken)^.KeywordIndex = kiSTORAGE)) then
      Nodes.EngineValue := ParseValue(WordIndices(kiSTORAGE, kiENGINE), vaAuto, ParseIdent)
    else if ((Nodes.EngineValue = 0) and (TokenPtr(CurrentToken)^.KeywordIndex = kiENGINE)) then
      Nodes.EngineValue := ParseValue(kiENGINE, vaAuto, ParseIdent)
    else if ((Nodes.IndexDirectoryValue = 0) and (TokenPtr(CurrentToken)^.KeywordIndex = kiINDEX)) then
      Nodes.IndexDirectoryValue := ParseValue(WordIndices(kiINDEX, kiDIRECTORY), vaAuto, ParseString)
    else if ((Nodes.MaxRowsValue = 0) and (TokenPtr(CurrentToken)^.KeywordIndex = kiMAX_ROWS)) then
      Nodes.MaxRowsValue := ParseValue(kiMAX_ROWS, vaAuto, ParseInteger)
    else if ((Nodes.MinRowsValue = 0) and (TokenPtr(CurrentToken)^.KeywordIndex = kiMIN_ROWS)) then
      Nodes.MinRowsValue := ParseValue(kiMIN_ROWS, vaAuto, ParseInteger)
    else if ((Nodes.EngineValue = 0) and (TokenPtr(CurrentToken)^.KeywordIndex = kiSTORAGE)) then
      Nodes.EngineValue := ParseValue(WordIndices(kiSTORAGE, kiENGINE), vaAuto, ParseIdent)
    else if ((Nodes.SubPartitionList = 0) and (TokenPtr(CurrentToken)^.TokenType = ttOpenBracket)) then
      Nodes.SubPartitionList := ParseList(True, ParseSubPartition)
    else
      Found := False;

  Result := TCreateTableStmt.TPartition.Create(Self, Nodes);
end;

function TMySQLParser.ParseCreateTableStmtPartitionValues(): TOffset;
var
  Nodes: TCreateTableStmt.TPartitionValues.TNodes;
  ValueNodes: TValue.TNodes;
begin
  FillChar(Nodes, SizeOf(Nodes), 0);

  Nodes.ValuesTag := ParseTag(kiVALUES);

  if (EndOfStmt(CurrentToken)) then
    SetError(PE_IncompleteStmt)
  else if (TokenPtr(CurrentToken)^.KeywordIndex = kiLESS) then
    if (EndOfStmt(NextToken[1])) then
      SetError(PE_IncompleteStmt)
    else if (TokenPtr(NextToken[1])^.KeywordIndex <> kiTHAN) then
      SetError(PE_UnexpectedToken, NextToken[1])
    else if (EndOfStmt(NextToken[2])) then
      SetError(PE_IncompleteStmt)
    else if (TokenPtr(NextToken[2])^.TokenType = ttOpenBracket) then
    begin
      FillChar(ValueNodes, SizeOf(ValueNodes), 0);
      ValueNodes.IdentTag := ParseTag(kiLESS, kiTHAN);
      ValueNodes.Expr := ParseList(True, ParseExpr);
      Nodes.Value := TValue.Create(Self, ValueNodes);
    end
    else
      Nodes.Value := ParseValue(WordIndices(kiLESS, kiTHAN), vaNo, kiMAXVALUE)

  else if (TokenPtr(CurrentToken)^.KeywordIndex = kiIN) then
  begin
    FillChar(ValueNodes, SizeOf(ValueNodes), 0);
    ValueNodes.IdentTag := ParseTag(kiIN);
    if (not Error) then
      ValueNodes.Expr := ParseList(True, ParseExpr);
    Nodes.Value := TValue.Create(Self, ValueNodes);
  end
  else
    SetError(PE_UnexpectedToken);

  Result := TCreateTableStmt.TPartitionValues.Create(Self, Nodes);
end;

function TMySQLParser.ParseCreateTableStmtReference(): TOffset;
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
    Nodes.IndicesList := ParseList(True, ParseFieldIdent);

  if (not Error and not EndOfStmt(CurrentToken) and (TokenPtr(CurrentToken)^.KeywordIndex = kiMATCH)) then
    if (not EndOfStmt(NextToken[1]) and ((TokenPtr(NextToken[1])^.KeywordIndex = kiFULL) or (TokenPtr(NextToken[1])^.KeywordIndex = kiPARTIAL) or (TokenPtr(NextToken[1])^.KeywordIndex = kiSIMPLE))) then
      Nodes.MatchValue := ParseValue(kiMATCH, vaNo, WordIndices(kiFULL, kiPARTIAL, kiSIMPLE));


  Found := True;
  while (not Error and Found
    and not EndOfStmt(CurrentToken) and (TokenPtr(CurrentToken)^.KeywordIndex = kiON)) do
  begin
    Found := False;

    if (not EndOfStmt(NextToken[1]) and (TokenPtr(NextToken[1])^.KeywordIndex = kiDELETE)) then
    begin
      if (EndOfStmt(NextToken[2])) then
        SetError(PE_IncompleteStmt)
      else if (TokenPtr(NextToken[2])^.KeywordIndex = kiRESTRICT) then
        Nodes.OnDeleteValue := ParseValue(WordIndices(kiON, kiDELETE), vaNo, kiRESTRICT)
      else if (TokenPtr(NextToken[2])^.KeywordIndex = kiCASCADE) then
        Nodes.OnDeleteValue := ParseValue(WordIndices(kiON, kiDELETE), vaNo, kiCASCADE)
      else if ((TokenPtr(NextToken[2])^.KeywordIndex = kiSET)
        and not EndOfStmt(NextToken[3]) and (TokenPtr(NextToken[3])^.KeywordIndex = kiDEFAULT)) then
        Nodes.OnDeleteValue := ParseValue(WordIndices(kiON, kiDELETE), vaNo, kiSET, kiDEFAULT)
      else if ((TokenPtr(NextToken[2])^.KeywordIndex = kiSET)
        and not EndOfStmt(NextToken[3]) and (TokenPtr(NextToken[3])^.KeywordIndex = kiNULL)) then
        Nodes.OnDeleteValue := ParseValue(WordIndices(kiON, kiDELETE), vaNo, kiSET, kiNULL)
      else if (TokenPtr(NextToken[2])^.KeywordIndex = kiNO) then
        Nodes.OnDeleteValue := ParseValue(WordIndices(kiON, kiDELETE), vaNo, kiNO, kiACTION)
      else
        SetError(PE_UnexpectedToken);
      Found := True;
    end;

    if (not EndOfStmt(NextToken[1]) and (TokenPtr(NextToken[1])^.KeywordIndex = kiUPDATE)) then
    begin
      if (EndOfStmt(NextToken[2])) then
        SetError(PE_IncompleteStmt)
      else if (TokenPtr(NextToken[2])^.KeywordIndex = kiRESTRICT) then
        Nodes.OnUpdateValue := ParseValue(WordIndices(kiON, kiUPDATE), vaNo, kiRESTRICT)
      else if (TokenPtr(NextToken[2])^.KeywordIndex = kiCASCADE) then
        Nodes.OnUpdateValue := ParseValue(WordIndices(kiON, kiUPDATE), vaNo, kiCASCADE)
      else if ((TokenPtr(NextToken[2])^.KeywordIndex = kiSET)
        and not EndOfStmt(NextToken[3]) and (TokenPtr(NextToken[3])^.KeywordIndex = kiDEFAULT)) then
        Nodes.OnUpdateValue := ParseValue(WordIndices(kiON, kiUPDATE), vaNo, kiSET, kiDEFAULT)
      else if ((TokenPtr(NextToken[2])^.KeywordIndex = kiSET)
        and not EndOfStmt(NextToken[3]) and (TokenPtr(NextToken[3])^.KeywordIndex = kiNULL)) then
        Nodes.OnUpdateValue := ParseValue(WordIndices(kiON, kiUPDATE), vaNo, kiSET, kiNULL)
      else if (TokenPtr(NextToken[2])^.KeywordIndex = kiNO) then
        Nodes.OnUpdateValue := ParseValue(WordIndices(kiON, kiUPDATE), vaNo, kiNO, kiACTION)
      else
        SetError(PE_UnexpectedToken);
      Found := True;
    end;
  end;

  Result := TCreateTableStmt.TReference.Create(Self, Nodes);
end;

function TMySQLParser.ParseCreateTableStmtUnion(): TOffset;
var
  Nodes: TValue.TNodes;
begin
  FillChar(Nodes, SizeOf(Nodes), 0);

  Nodes.IdentTag := ParseTag(kiUNION);

  if (not Error and not EndOfStmt(CurrentToken) and (TokenPtr(CurrentToken)^.OperatorType = otEqual)) then
  begin
    TokenPtr(CurrentToken)^.FOperatorType := otAssign;
    Nodes.AssignToken := ApplyCurrentToken();
  end;

  if (not Error) then
    Nodes.Expr := ParseList(True, ParseTableIdent);

  Result := TValue.Create(Self, Nodes);
end;

function TMySQLParser.ParseCreateTriggerStmt(): TOffset;
var
  Nodes: TCreateTriggerStmt.TNodes;
begin
  FillChar(Nodes, SizeOf(Nodes), 0);

  Nodes.CreateTag := ParseTag(kiCREATE);

  if (not Error and not EndOfStmt(CurrentToken) and (TokenPtr(CurrentToken)^.KeywordIndex = kiDEFINER)) then
    Nodes.DefinerNode := ParseDefinerValue();

    Nodes.TriggerTag := ParseTag(kiTRIGGER);

  if (not Error) then
    if (EndOfStmt(CurrentToken)) then
      SetError(PE_IncompleteStmt)
    else if (not (TokenPtr(CurrentToken)^.TokenType in ttIdents)) then
      SetError(PE_UnexpectedToken)
    else
      Nodes.TriggerIdent := ParseTriggerIdent();

  if (not Error) then
    if (EndOfStmt(CurrentToken) or EndOfStmt(NextToken[1])) then
      SetError(PE_IncompleteStmt)
    else if ((TokenPtr(NextToken[1])^.KeywordIndex <> kiINSERT)
      and (TokenPtr(NextToken[1])^.KeywordIndex <> kiUPDATE)
      and (TokenPtr(NextToken[1])^.KeywordIndex <> kiDELETE)) then
      SetError(PE_UnexpectedToken, NextToken[1])
    else if (TokenPtr(CurrentToken)^.KeywordIndex = kiBEFORE) then
      Nodes.ActionValue := ParseValue(kiBEFORE, vaNo, ParseKeyword)
    else if (TokenPtr(CurrentToken)^.KeywordIndex = kiAFTER) then
      Nodes.ActionValue := ParseValue(kiAFTER, vaNo, ParseKeyword)
    else
      SetError(PE_UnexpectedToken);

  if (not Error) then
    Nodes.OnTag := ParseTag(kiON);

  if (not Error) then
    if (EndOfStmt(CurrentToken)) then
      SetError(PE_IncompleteStmt)
    else if (not (TokenPtr(CurrentToken)^.TokenType in ttIdents)) then
      SetError(PE_UnexpectedToken)
    else
      Nodes.TableIdentNode := ParseTableIdent();

  if (not Error) then
    Nodes.ForEachRowTag := ParseTag(kiFOR, kiEACH, kiROW);

  if (not Error and not EndOfStmt(CurrentToken)) then
    if (TokenPtr(CurrentToken)^.KeywordIndex = kiFOLLOWS) then
      Nodes.OrderValue := ParseValue(kiFOLLOWS, vaNo, ParseTriggerIdent)
    else if (TokenPtr(CurrentToken)^.KeywordIndex = kiPRECEDES) then
      Nodes.OrderValue := ParseValue(kiPRECEDES, vaNo, ParseTriggerIdent);

  if (not Error) then
    if (EndOfStmt(CurrentToken)) then
      SetError(PE_IncompleteStmt)
    else
      Nodes.Body := ParsePL_SQLStmt();

  Result := TCreateTriggerStmt.Create(Self, Nodes);
end;

function TMySQLParser.ParseCreateUserStmt(const Alter: Boolean): TOffset;
var
  ListNodes: TList.TNodes;
  Nodes: TCreateUserStmt.TNodes;
  Resources: array of TOffset;
begin
  FillChar(Nodes, SizeOf(Nodes), 0);

  Nodes.CreateTag := ParseTag(kiCREATE, kiUSER);

  if (not Error and not EndOfStmt(CurrentToken) and (TokenPtr(CurrentToken)^.KeywordIndex = kiIF)) then
    if (not Alter) then
      Nodes.IfTag := ParseTag(kiIF, kiNOT, kiEXISTS)
    else
      Nodes.IfTag := ParseTag(kiIF, kiEXISTS);

  if (not Error) then
    Nodes.UserSpecifications := ParseList(False, ParseGrantStmtUserSpecification);

  if (not Error and not EndOfStmt(CurrentToken) and (TokenPtr(CurrentToken)^.KeywordIndex = kiWITH)) then
  begin
    SetLength(Resources, 0);

    Nodes.WithTag := ParseTag(kiWITH);

    repeat
      if (EndOfStmt(CurrentToken)) then
        SetError(PE_IncompleteStmt)
      else if (TokenPtr(CurrentToken)^.KeywordIndex = kiMAX_QUERIES_PER_HOUR) then
      begin
        SetLength(Resources, Length(Resources) + 1);
        Resources[Length(Resources) - 1] := ParseValue(kiMAX_QUERIES_PER_HOUR, vaNo, ParseInteger);
      end
      else if (TokenPtr(CurrentToken)^.KeywordIndex = kiMAX_UPDATES_PER_HOUR) then
      begin
        SetLength(Resources, Length(Resources) + 1);
        Resources[Length(Resources) - 1] := ParseValue(kiMAX_UPDATES_PER_HOUR, vaNo, ParseInteger);
      end
      else if (TokenPtr(CurrentToken)^.KeywordIndex = kiMAX_CONNECTIONS_PER_HOUR) then
      begin
        SetLength(Resources, Length(Resources) + 1);
        Resources[Length(Resources) - 1] := ParseValue(kiMAX_CONNECTIONS_PER_HOUR, vaNo, ParseInteger);
      end
      else if (TokenPtr(CurrentToken)^.KeywordIndex = kiMAX_USER_CONNECTIONS) then
      begin
        SetLength(Resources, Length(Resources) + 1);
        Resources[Length(Resources) - 1] := ParseValue(kiMAX_USER_CONNECTIONS, vaNo, ParseInteger);
      end
      else
        SetError(PE_UnexpectedToken);
    until (Error or EndOfStmt(CurrentToken));

    FillChar(ListNodes, SizeOf(ListNodes), 0);
    Nodes.ResourcesList := TList.Create(Self, ListNodes, ttUnknown, Length(Resources), Resources);
  end;

  if (not Error) then
    if (not EndOfStmt(CurrentToken) and (TokenPtr(CurrentToken)^.KeywordIndex = kiPASSWORD)
      and not EndOfStmt(NextToken[1]) and (TokenPtr(NextToken[1])^.KeywordIndex = kiEXPIRE)) then
    begin
      if (EndOfStmt(NextToken[2]) and (TokenPtr(NextToken[2])^.KeywordIndex = kiDEFAULT)) then
        Nodes.PasswordOption := ParseTag(kiPASSWORD, kiEXPIRE, kiDEFAULT)
      else if (EndOfStmt(NextToken[2]) and (TokenPtr(NextToken[2])^.KeywordIndex = kiNEVER)) then
        Nodes.PasswordOption := ParseTag(kiPASSWORD, kiEXPIRE, kiNEVER)
      else if (EndOfStmt(NextToken[2]) and (TokenPtr(NextToken[2])^.KeywordIndex = kiINTERVAL)) then
      begin
        Nodes.PasswordOption := ParseTag(kiPASSWORD, kiEXPIRE, kiINTERVAL);
        if (not Error) then
          if (EndOfStmt(CurrentToken)) then
            SetError(PE_IncompleteStmt)
          else if (TokenPtr(CurrentToken)^.TokenType <> ttInteger) then
            SetError(PE_UnexpectedToken)
          else
            Nodes.PasswordDays := ParseInteger();
        if (not Error) then
          if (EndOfStmt(CurrentToken)) then
            SetError(PE_IncompleteStmt)
          else if (TokenPtr(CurrentToken)^.KeywordIndex <> kiDAY) then
            SetError(PE_UnexpectedToken)
          else
            Nodes.DayTag := ParseTag(kiDAY);
      end
      else
        Nodes.PasswordOption := ParseTag(kiPASSWORD, kiEXPIRE);
    end
    else if (not EndOfStmt(CurrentToken) and (TokenPtr(CurrentToken)^.KeywordIndex = kiACCOUNT)) then
    begin
      if (EndOfStmt(NextToken[1])) then
        SetError(PE_IncompleteStmt)
      else if (TokenPtr(CurrentToken)^.KeywordIndex = kiLOCK) then
        Nodes.AccountTag := ParseTag(kiACCOUNT, kiLOCK)
      else if (TokenPtr(CurrentToken)^.KeywordIndex = kiUNLOCK) then
        Nodes.AccountTag := ParseTag(kiACCOUNT, kiUNLOCK)
      else
        SetError(PE_UnexpectedToken);
    end;

  Result := TCreateUserStmt.Create(Self, Nodes);
end;

function TMySQLParser.ParseCreateViewStmt(): TOffset;
var
  Nodes: TCreateViewStmt.TNodes;
begin
  FillChar(Nodes, SizeOf(Nodes), 0);

  Nodes.CreateTag := ParseTag(kiCREATE);

  if (not Error and not EndOfStmt(CurrentToken) and (TokenPtr(CurrentToken)^.KeywordIndex = kiOR)) then
    Nodes.OrReplaceTag := ParseTag(kiOR, kiREPLACE);

  if (not Error and not EndOfStmt(CurrentToken) and (TokenPtr(CurrentToken)^.KeywordIndex = kiALGORITHM)) then
    Nodes.AlgorithmValue := ParseValue(kiALGORITHM, vaYes, ParseKeyword);

  if (not Error and not EndOfStmt(CurrentToken) and (TokenPtr(CurrentToken)^.KeywordIndex = kiDEFINER)) then
    Nodes.DefinerNode := ParseDefinerValue();

  if (not Error and not EndOfStmt(CurrentToken) and (TokenPtr(CurrentToken)^.KeywordIndex = kiSQL)) then
    Nodes.SQLSecurityTag := ParseValue(WordIndices(kiSQL, kiSECURITY), vaNo, ParseIdent);

  if (not Error) then
    Nodes.ViewTag := ParseTag(kiVIEW);

  if (not Error) then
    Nodes.Ident := ParseTableIdent();

  if (not Error and not EndOfStmt(CurrentToken) and (TokenPtr(CurrentToken)^.TokenType = ttOpenBracket)) then
    Nodes.ColumnList:= ParseList(True, ParseFieldIdent);

  if (not Error) then
    Nodes.AsTag := ParseTag(kiAS);

  if (not Error) then
    if (EndOfStmt(CurrentToken)) then
      SetError(PE_IncompleteStmt)
    else
      Nodes.SelectStmt := ParseSubAreaSelectStmt();

  if (not Error and not EndOfStmt(CurrentToken) and (TokenPtr(CurrentToken)^.KeywordIndex = kiWITH)) then
    if (EndOfStmt(NextToken[1])) then
      SetError(PE_IncompleteStmt, NextToken[1])
    else if (TokenPtr(NextToken[1])^.KeywordIndex = kiCASCADED) then
      Nodes.OptionTag := ParseTag(kiWITH, kiCASCADED, kiCHECK, kiOPTION)
    else if (TokenPtr(NextToken[1])^.KeywordIndex = kiLOCAL) then
      Nodes.OptionTag := ParseTag(kiWITH, kiLOCAL, kiCHECK, kiOPTION)
    else
      Nodes.OptionTag := ParseTag(kiWITH, kiCHECK, kiOPTION);

  Result := TCreateViewStmt.Create(Self, Nodes);
end;

function TMySQLParser.ParseCurrentTimestamp(): TOffset;
var
  Nodes: TCurrentTimestamp.TNodes;
begin
  FillChar(Nodes, SizeOf(Nodes), 0);

  Nodes.CurrentTimestampTag := ParseTag(kiCURRENT_TIMESTAMP);

  if (not Error and not EndOfStmt(CurrentToken) and (TokenPtr(CurrentToken)^.TokenType = ttOpenBracket)) then
  begin
    Nodes.OpenBracket := ApplyCurrentToken();

    if (not Error) then
      if (EndOfStmt(CurrentToken)) then
        SetError(PE_IncompleteStmt)
      else if (TokenPtr(CurrentToken)^.TokenType <> ttInteger) then
        SetError(PE_UnexpectedToken)
      else
        Nodes.LengthInteger := ParseInteger();
    
    if (not Error) then
      if (EndOfStmt(CurrentToken)) then
        SetError(PE_IncompleteStmt)
      else if (TokenPtr(CurrentToken)^.TokenType <> ttCloseBracket) then
        SetError(PE_UnexpectedToken)
      else
        Nodes.CloseBracket := ApplyCurrentToken();
  end;
  
  Result := TCurrentTimestamp.Create(Self, Nodes);
end;

function TMySQLParser.ParseDatabaseIdent(): TOffset;
begin
  Result := ParseDbIdent(ditDatabase);
end;

function TMySQLParser.ParseDatatype(): TOffset;
var
  DatatypeIndex: Integer;
  DatatypeIndex2: Integer;
  Length: Integer;
  Nodes: TDatatype.TNodes;
  Text: PChar;
begin
  FillChar(Nodes, SizeOf(Nodes), 0);

  if (not EndOfStmt(CurrentToken) and (TokenPtr(CurrentToken)^.KeywordIndex = kiNATIONAL)) then
    Nodes.NationalToken := ApplyCurrentToken(utDatatype);

  if (not Error) then
    if (EndOfStmt(CurrentToken)) then
      SetError(PE_IncompleteStmt)
    else if (TokenPtr(CurrentToken)^.TokenType <> ttIdent) then
      SetError(PE_UnexpectedToken)
    else
      Nodes.DatatypeToken := ApplyCurrentToken(utDatatype);

  TokenPtr(Nodes.DatatypeToken)^.GetText(Text, Length);
  DatatypeIndex := DatatypeList.IndexOf(Text, Length);
  if (not Error and (DatatypeIndex < 0)) then
    SetError(PE_UnexpectedToken);
  
  if (not Error and not EndOfStmt(CurrentToken) and not (TokenPtr(CurrentToken)^.TokenType in [ttComma, ttCloseBracket])
    and (DatatypeIndex = diSIGNED) or (DatatypeIndex = diUNSIGNED)) then
  begin
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

  if (not Error and not EndOfStmt(CurrentToken) and (TokenPtr(CurrentToken)^.TokenType = ttOpenBracket)
    and (DatatypeIndex = diBIGINT)
      or (DatatypeIndex = diBINARY)
      or (DatatypeIndex = diBIT)
      or (DatatypeIndex = diCHAR)
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
      or (DatatypeIndex = diYEAR)) then
  begin
    Nodes.OpenBracket := ApplyCurrentToken();

    if (not Error) then
      if (EndOfStmt(CurrentToken)) then
        SetError(PE_IncompleteStmt)
      else
        Nodes.LengthToken := ParseInteger();

    if (not Error and not EndOfStmt(CurrentToken) and (TokenPtr(CurrentToken)^.TokenType = ttComma)
      and ((DatatypeIndex = diDECIMAL)
        or (DatatypeIndex = diDOUBLE)
        or (DatatypeIndex = diFLOAT)
        or (DatatypeIndex = diNUMERIC)
        or (DatatypeIndex = diREAL))) then
    begin
      Nodes.CommaToken := ApplyCurrentToken();

      if (not Error) then
        if (EndOfStmt(CurrentToken)) then
          SetError(PE_IncompleteStmt)
        else
          Nodes.DecimalsToken := ParseInteger();
    end;

    if (not Error) then
      if (EndOfStmt(CurrentToken)) then
        SetError(PE_IncompleteStmt)
      else if (TokenPtr(CurrentToken)^.TokenType <> ttCloseBracket) then
        SetError(PE_UnexpectedToken)
      else
        Nodes.CloseBracket := ApplyCurrentToken();
  end;

  if (not Error) then
  begin
    if ((Nodes.LengthToken = 0)
      and ((DatatypeIndex = diVARCHAR)
        or (DatatypeIndex = diVARBINARY))) then
      if (EndOfStmt(CurrentToken)) then
        SetError(PE_IncompleteStmt)
      else
        SetError(PE_UnexpectedToken);
  end;

  if (not Error
    and (DatatypeIndex = diENUM)
      or (DatatypeIndex = diSET)) then
    Nodes.ItemsList := ParseList(True, ParseString);

  if (not Error and not EndOfStmt(CurrentToken)) then
    if (TokenPtr(CurrentToken)^.KeywordIndex = kiSIGNED) then
      Nodes.SignedToken := ApplyCurrentToken(utDatatype)
    else if (TokenPtr(CurrentToken)^.KeywordIndex = kiUNSIGNED) then
      Nodes.SignedToken := ApplyCurrentToken(utDatatype);

  if ((DatatypeIndex = diBIGINT)
    or (DatatypeIndex = diDECIMAL)
    or (DatatypeIndex = diDOUBLE)
    or (DatatypeIndex = diFLOAT)
    or (DatatypeIndex = diINT)
    or (DatatypeIndex = diINTEGER)
    or (DatatypeIndex = diMEDIUMINT)
    or (DatatypeIndex = diNUMERIC)
    or (DatatypeIndex = diREAL)
    or (DatatypeIndex = diSMALLINT)
    or (DatatypeIndex = diTINYINT)) then
  begin
    if (not Error and not EndOfStmt(CurrentToken) and (TokenPtr(CurrentToken)^.KeywordIndex = kiZEROFILL)) then
      Nodes.ZerofillTag := ParseTag(kiZEROFILL);
  end;

  if ((DatatypeIndex = diCHAR)
    or (DatatypeIndex = diTEXT)
    or (DatatypeIndex = diLONGTEXT)
    or (DatatypeIndex = diMEDIUMTEXT)
    or (DatatypeIndex = diTINYTEXT)
    or (DatatypeIndex = diVARCHAR)) then
  begin
    if (not Error and not EndOfStmt(CurrentToken) and (TokenPtr(CurrentToken)^.KeywordIndex = kiBINARY)) then
      Nodes.BinaryToken := ApplyCurrentToken(utDatatype);

    if (not Error and not EndOfStmt(CurrentToken) and (TokenPtr(CurrentToken)^.KeywordIndex = kiASCII)) then
      Nodes.ASCIIToken := ApplyCurrentToken(utDatatype);

    if (not Error and not EndOfStmt(CurrentToken) and (TokenPtr(CurrentToken)^.KeywordIndex = kiUNICODE)) then
      Nodes.UnicodeToken := ApplyCurrentToken(utDatatype);
  end;

  if ((DatatypeIndex = diCHAR)
    or (DatatypeIndex = diENUM)
    or (DatatypeIndex = diLONGTEXT)
    or (DatatypeIndex = diMEDIUMTEXT)
    or (DatatypeIndex = diSET)
    or (DatatypeIndex = diTEXT)
    or (DatatypeIndex = diTINYTEXT)
    or (DatatypeIndex = diVARCHAR)) then
  begin
    if (not EndOfStmt(CurrentToken) and (TokenPtr(CurrentToken)^.KeywordIndex = kiCHARACTER)) then
      Nodes.CharacterSetValue := ParseValue(WordIndices(kiCHARACTER, kiSET), vaNo, ParseIdent)
    else if (not EndOfStmt(CurrentToken) and (TokenPtr(CurrentToken)^.KeywordIndex = kiCHARSET)) then
      Nodes.CharacterSetValue := ParseValue(kiCHARSET, vaNo, ParseIdent);

    if (not Error and not EndOfStmt(CurrentToken) and (TokenPtr(CurrentToken)^.KeywordIndex = kiCOLLATE)) then
      Nodes.CollateValue := ParseValue(kiCOLLATE, vaNo, ParseIdent);
  end;

  Result := TDatatype.Create(Self, Nodes);
end;

function TMySQLParser.ParseDbIdent(const ADbIdentType: TDbIdentType): TOffset;
var
  Nodes: TDbIdent.TNodes;
begin
  FillChar(Nodes, SizeOf(Nodes), 0);

  if (EndOfStmt(CurrentToken)) then
    SetError(PE_IncompleteStmt)
  else if (not (TokenPtr(CurrentToken)^.TokenType in ttIdents + ttStrings) and (TokenPtr(CurrentToken)^.OperatorType <> otMulti)) then
    SetError(PE_UnexpectedToken);

  if (not Error) then
  begin
    if (TokenPtr(CurrentToken)^.OperatorType = otMulti) then
      TokenPtr(CurrentToken)^.FTokenType := ttIdent;
    TokenPtr(CurrentToken)^.FOperatorType := otUnknown;
    TokenPtr(CurrentToken)^.FUsageType := utDbIdent;
    Nodes.Ident := ApplyCurrentToken();
  end;

  if (not Error) then
    if (not EndOfStmt(CurrentToken) and (TokenPtr(CurrentToken)^.OperatorType = otDot)) then
    case (ADbIdentType) of
      ditKey,
      ditField:
        begin
          Nodes.TableIdent := Nodes.Ident;

          Nodes.TableDot := ApplyCurrentToken();

          if (EndOfStmt(CurrentToken)) then
            SetError(PE_IncompleteStmt)
          else if (TokenPtr(CurrentToken)^.OperatorType = otMulti) then
          begin
            if (TokenPtr(CurrentToken)^.OperatorType = otMulti) then
              TokenPtr(CurrentToken)^.FTokenType := ttIdent;
            TokenPtr(CurrentToken)^.FOperatorType := otUnknown;
            TokenPtr(CurrentToken)^.FUsageType := utDbIdent;
            Nodes.Ident := ApplyCurrentToken();
          end
          else if (((TokenPtr(CurrentToken)^.TokenType = ttIdent) or AnsiQuotes and (TokenPtr(CurrentToken)^.TokenType = ttDQIdent) or not AnsiQuotes and (TokenPtr(CurrentToken)^.TokenType = ttMySQLIdent))) then
            Nodes.Ident := ApplyCurrentToken()
          else
            SetError(PE_UnexpectedToken);

          if (not Error and not EndOfStmt(CurrentToken)
            and (TokenPtr(CurrentToken)^.OperatorType = otDot)) then
          begin
            Nodes.DatabaseIdent := Nodes.TableIdent;
            Nodes.DatabaseDot := Nodes.TableDot;
            Nodes.TableIdent := Nodes.Ident;

            Nodes.TableDot := ApplyCurrentToken();

            if (EndOfStmt(CurrentToken)) then
              SetError(PE_IncompleteStmt)
            else if (TokenPtr(CurrentToken)^.OperatorType = otMulti) then
            begin
              if (TokenPtr(CurrentToken)^.OperatorType = otMulti) then
                TokenPtr(CurrentToken)^.FTokenType := ttIdent;
              TokenPtr(CurrentToken)^.FOperatorType := otUnknown;
              TokenPtr(CurrentToken)^.FUsageType := utDbIdent;
              Nodes.Ident := ApplyCurrentToken();
            end
            else if (((TokenPtr(CurrentToken)^.TokenType = ttIdent) or AnsiQuotes and (TokenPtr(CurrentToken)^.TokenType = ttDQIdent) or not AnsiQuotes and (TokenPtr(CurrentToken)^.TokenType = ttMySQLIdent))) then
              Nodes.Ident := ApplyCurrentToken()
            else
              SetError(PE_UnexpectedToken);
          end;
        end;
      ditTable,
      ditFunction,
      ditProcedure,
      ditTrigger,
      ditEvent:
        begin
          Nodes.DatabaseIdent := Nodes.Ident;

          Nodes.DatabaseDot := ApplyCurrentToken();
          if (EndOfStmt(CurrentToken)) then
            SetError(PE_IncompleteStmt)
          else
          begin
            if (TokenPtr(CurrentToken)^.OperatorType = otMulti) then
              TokenPtr(CurrentToken)^.FTokenType := ttIdent;
            TokenPtr(CurrentToken)^.FOperatorType := otUnknown;
            TokenPtr(CurrentToken)^.FUsageType := utDbIdent;
            Nodes.Ident := ApplyCurrentToken();
          end;
        end;
    end;

  Result := TDbIdent.Create(Self, ADbIdentType, Nodes);
end;

function TMySQLParser.ParseDefinerValue(): TOffset;
var
  Nodes: TValue.TNodes;
begin
  FillChar(Nodes, SizeOf(Nodes), 0);

  Nodes.IdentTag := ParseTag(kiDEFINER);

  if (not Error) then
    if (EndOfStmt(CurrentToken)) then
      SetError(PE_IncompleteStmt)
    else if (not (TokenPtr(CurrentToken)^.OperatorType = otEqual)) then
      SetError(PE_UnexpectedToken)
    else
    begin
      TokenPtr(CurrentToken)^.FOperatorType := otAssign;
      Nodes.AssignToken := ApplyCurrentToken();

      if (not Error) then
        if (EndOfStmt(CurrentToken)) then
          SetError(PE_IncompleteStmt)
        else
          Nodes.Expr := ParseUserIdent();
    end;

  Result := TValue.Create(Self, Nodes);
end;

function TMySQLParser.ParseDeallocatePrepareStmt(): TOffset;
var
  Nodes: TDeallocatePrepareStmt.TNodes;
begin
  FillChar(Nodes, SizeOf(Nodes), 0);

  if ((TokenPtr(CurrentToken)^.KeywordIndex <> kiDEALLOCATE)
    and (TokenPtr(CurrentToken)^.KeywordIndex <> kiDROP)) then
    SetError(PE_UnexpectedToken)
  else if (EndOfStmt(NextToken[1])) then
    SetError(PE_IncompleteStmt)
  else if (TokenPtr(NextToken[1])^.KeywordIndex <> kiPREPARE) then
    SetError(PE_UnexpectedToken, NextToken[1])
  else
    Nodes.StmtTag := ParseTag(TokenPtr(CurrentToken)^.KeywordIndex, kiPREPARE);

  if (not Error) then
    if (EndOfStmt(CurrentToken)) then
      SetError(PE_IncompleteStmt)
    else
      Nodes.StmtIdent := ParseVariable();

  Result := TDeallocatePrepareStmt.Create(Self, Nodes);
end;

function TMySQLParser.ParseDeclareStmt(): TOffset;
var
  Nodes: TDeclareStmt.TNodes;
begin
  FillChar(Nodes, SizeOf(Nodes), 0);

  Nodes.StmtTag := ParseTag(kiDECLARE);

  if (not Error) then
    if (not EndOfStmt(CurrentToken) and (TokenPtr(CurrentToken)^.KeywordIndex = kiCURSOR)) then
    begin
      Nodes.CursorForTag := ParseTag(kiCURSOR, kiFOR);

      if (not Error) then
        Nodes.SelectStmt := ParseSubAreaSelectStmt();
    end
    else
    begin
      Nodes.IdentList := ParseList(False, ApplyCurrentToken);

      if (not Error) then
        if (EndOfStmt(CurrentToken)) then
          SetError(PE_IncompleteStmt)
        else if (TokenPtr(CurrentToken)^.TokenType <> ttIdent) then
          SetError(PE_UnexpectedToken)
        else
          Nodes.TypeNode := ParseDatatype();

      if (not Error and not EndOfStmt(CurrentToken) and (TokenPtr(CurrentToken)^.KeywordIndex = kiDEFAULT)) then
        Nodes.DefaultValue := ParseValue(kiDEFAULT, vaNo, ParseExpr);
    end;

  Result := TDeclareStmt.Create(Self, Nodes);
end;

function TMySQLParser.ParseDeclareConditionStmt(): TOffset;
var
  Nodes: TDeclareConditionStmt.TNodes;
begin
  FillChar(Nodes, SizeOf(Nodes), 0);

  Nodes.StmtTag := ParseTag(kiDECLARE);

  if (not Error) then
    if (EndOfStmt(CurrentToken)) then
      SetError(PE_IncompleteStmt)
    else
      Nodes.Ident := ApplyCurrentToken();

  if (not Error) then
    if (EndOfStmt(CurrentToken)) then
      SetError(PE_IncompleteStmt)
    else
      Nodes.ConditionTag := ParseTag(kiCONDITION, kiFOR);

  if (not Error) then
    if (EndOfStmt(CurrentToken)) then
      SetError(PE_IncompleteStmt)
    else if (TokenPtr(CurrentToken)^.KeywordIndex <> kiSQLSTATE) then
      Nodes.ErrorCode := ParseInteger()
    else
    begin
      if (EndOfStmt(NextToken[1]) or (TokenPtr(NextToken[1])^.KeywordIndex <> kiVALUE)) then
        Nodes.SQLStateTag := ParseTag(kiSQLSTATE)
      else
        Nodes.SQLStateTag := ParseTag(kiSQLSTATE, kiVALUE);

      if (not Error) then
        if (EndOfStmt(CurrentToken)) then
          SetError(PE_IncompleteStmt)
        else
          Nodes.ErrorString := ParseString();
    end;


  Result := TDeclareConditionStmt.Create(Self, Nodes);
end;

function TMySQLParser.ParseDeclareCursorStmt(): TOffset;
var
  Nodes: TDeclareCursorStmt.TNodes;
begin
  FillChar(Nodes, SizeOf(Nodes), 0);

  Nodes.StmtTag := ParseTag(kiDECLARE);

  if (not Error) then
    if (EndOfStmt(CurrentToken)) then
      SetError(PE_IncompleteStmt)
    else
      Nodes.Ident := ParseDbIdent(ditCursor);

  if (not Error) then
    if (EndOfStmt(CurrentToken)) then
      SetError(PE_IncompleteStmt)
    else if (TokenPtr(CurrentToken)^.KeywordIndex <> kiCURSOR) then
      SetError(PE_UnexpectedToken)
    else
      Nodes.CursorTag := ParseTag(kiCURSOR);

  if (not Error) then
    if (EndOfStmt(CurrentToken)) then
      SetError(PE_IncompleteStmt)
    else if (TokenPtr(CurrentToken)^.KeywordIndex <> kiFOR) then
      SetError(PE_UnexpectedToken)
    else
      Nodes.ForTag := ParseTag(kiFOR);

  if (not Error) then
    if (EndOfStmt(CurrentToken)) then
      SetError(PE_IncompleteStmt)
    else
      Nodes.SelectStmt := ParseStmt();

  Result := TDeclareCursorStmt.Create(Self, Nodes);
end;

function TMySQLParser.ParseDeclareHandlerStmt(): TOffset;
var
  Nodes: TDeclareHandlerStmt.TNodes;
begin
  FillChar(Nodes, SizeOf(Nodes), 0);

  Nodes.StmtTag := ParseTag(kiDECLARE);

  if (not Error) then
    if (EndOfStmt(CurrentToken)) then
      SetError(PE_IncompleteStmt)
    else if (TokenPtr(CurrentToken)^.KeywordIndex = kiCONTINUE) then
      Nodes.ActionTag := ParseTag(kiCONTINUE)
    else if (TokenPtr(CurrentToken)^.KeywordIndex = kiEXIT) then
      Nodes.ActionTag := ParseTag(kiEXIT)
    else if (TokenPtr(CurrentToken)^.KeywordIndex = kiUNDO) then
      Nodes.ActionTag := ParseTag(kiUNDO)
    else
      SetError(PE_UnexpectedToken);

  if (not Error) then
    Nodes.HandlerTag := ParseTag(kiHANDLER);

  if (not Error) then
    Nodes.ForTag := ParseTag(kiFOR);

  if (not Error) then
    Nodes.ConditionsList := ParseList(False, ParseDeclareHandlerStmtCondition);

  if (not Error) then
    if (EndOfStmt(CurrentToken)) then
      SetError(PE_IncompleteStmt)
    else
      Nodes.Stmt := ParsePL_SQLStmt();

  Result := TDeclareHandlerStmt.Create(Self, Nodes);
end;

function TMySQLParser.ParseDeclareHandlerStmtCondition(): TOffset;
var
  Nodes: TDeclareHandlerStmt.TCondition.TNodes;
begin
  FillChar(Nodes, SizeOf(Nodes), 0);

  if (EndOfStmt(CurrentToken)) then
    SetError(PE_IncompleteStmt)
  else if (TokenPtr(CurrentToken)^.TokenType = ttInteger) then
    Nodes.ErrorCode := ParseInteger()
  else if ((TokenPtr(CurrentToken)^.KeywordIndex = kiSQLSTATE)
    and not EndOfStmt(NextToken[1]) and (TokenPtr(NextToken[1])^.KeywordIndex = kiVALUE)) then
    Nodes.SQLStateTag := ParseValue(WordIndices(kiSQLSTATE, kiVALUE), vaNo, ParseExpr)
  else if (TokenPtr(CurrentToken)^.KeywordIndex = kiSQLSTATE) then
    Nodes.SQLStateTag := ParseValue(kiSQLSTATE, vaNo, ParseExpr)
  else if ((TokenPtr(CurrentToken)^.KeywordIndex = kiNOT)
    and not EndOfStmt(NextToken[1]) and (TokenPtr(NextToken[1])^.KeywordIndex = kiFOUND)) then
    Nodes.SQLStateTag := ParseTag(kiNOT, kiFOUND)
  else if (TokenPtr(CurrentToken)^.KeywordIndex = kiSQLWARNINGS) then
    Nodes.SQLWarningsTag := ParseTag(kiSQLWARNINGS)
  else if (TokenPtr(CurrentToken)^.KeywordIndex = kiSQLEXCEPTION) then
    Nodes.SQLWarningsTag := ParseTag(kiSQLEXCEPTION)
  else if (TokenPtr(CurrentToken)^.TokenType in ttIdents) then // Must be in the end, because keywords are in ttIdents
    Nodes.ConditionIdent := ParseVariable()
  else
    SetError(PE_UnexpectedToken);

  Result := TDeclareHandlerStmt.TCondition.Create(Self, Nodes);
end;

function TMySQLParser.ParseDeleteStmt(): TOffset;
var
  Nodes: TDeleteStmt.TNodes;
  TableCount: Integer;
begin
  FillChar(Nodes, SizeOf(Nodes), 0);
  TableCount := 1;

  Nodes.DeleteTag := ParseTag(kiDELETE);

  if (not Error and not EndOfStmt(CurrentToken) and (TokenPtr(CurrentToken)^.KeywordIndex = kiLOW_PRIORITY)) then
    Nodes.LowPriorityTag := ParseTag(kiLOW_PRIORITY);

  if (not Error and not EndOfStmt(CurrentToken) and (TokenPtr(CurrentToken)^.KeywordIndex = kiQUICK)) then
    Nodes.QuickTag := ParseTag(kiQUICK);

  if (not Error and not EndOfStmt(CurrentToken) and (TokenPtr(CurrentToken)^.KeywordIndex = kiIGNORE)) then
    Nodes.IgnoreTag := ParseTag(kiIGNORE);

  if (not Error) then
    if (EndOfStmt(CurrentToken)) then
      SetError(PE_IncompleteStmt)
    else if (TokenPtr(CurrentToken)^.KeywordIndex = kiFROM) then
    begin
      Nodes.From.Tag := ParseTag(kiFROM);

      if (not Error) then
      begin
        Nodes.From.List := ParseList(False, ParseSelectStmtTableFactor);

        if (not Error) then
          TableCount := PList(NodePtr(Nodes.From.List))^.ElementCount;
      end;

      if (not Error and not EndOfStmt(CurrentToken)) then
        if ((TableCount = 1) and (TokenPtr(CurrentToken)^.KeywordIndex = kiPARTITION)) then
        begin
          Nodes.Partition.Tag := ParseTag(kiPARTITION);

          if (not Error) then
            Nodes.Partition.List := ParseList(True, ParsePartitionIdent);
        end
        else if (TokenPtr(CurrentToken)^.KeywordIndex = kiUSING) then
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
        if (EndOfStmt(CurrentToken)) then
          SetError(PE_IncompleteStmt)
        else if (TokenPtr(CurrentToken)^.KeywordIndex <> kiFROM) then
          SetError(PE_UnexpectedToken)
        else
          Nodes.From.Tag := ParseTag(kiFROM);

      if (not Error) then
        Nodes.TableReferenceList := ParseList(False, ParseSelectStmtTableEscapedReference);
    end;

  if (not Error and not EndOfStmt(CurrentToken) and (TokenPtr(CurrentToken)^.KeywordIndex = kiWHERE)) then
    Nodes.WhereValue := ParseValue(kiWHERE, vaNo, ParseExpr);

  if (TableCount = 1) then
  begin
    if (not Error and not EndOfStmt(CurrentToken) and (TokenPtr(CurrentToken)^.KeywordIndex = kiORDER)) then
    begin
      Nodes.OrderBy.Tag := ParseTag(kiORDER, kiBY);

      if (not Error) then
        Nodes.OrderBy.Expr := ParseList(False, ParseSelectStmtOrder);
    end;

    if (not Error and not EndOfStmt(CurrentToken) and (TokenPtr(CurrentToken)^.KeywordIndex = kiLIMIT)) then
    begin
      Nodes.Limit.Tag := ParseTag(kiLIMIT);

      if (not Error) then
        Nodes.Limit.Expr := ParseExpr();
    end;
  end;

  Result := TDeleteStmt.Create(Self, Nodes);
end;

function TMySQLParser.ParseDoStmt(): TOffset;
var
  Nodes: TDoStmt.TNodes;
begin
  FillChar(Nodes, SizeOf(Nodes), 0);

  Nodes.DoTag := ParseTag(kiDO);

  if (not Error) then
    Nodes.ExprList := ParseList(False, ParseExpr);

  Result := TDoStmt.Create(Self, Nodes);
end;

function TMySQLParser.ParseDropDatabaseStmt(): TOffset;
var
  Nodes: TDropDatabaseStmt.TNodes;
begin
  FillChar(Nodes, SizeOf(Nodes), 0);

  if (TokenPtr(NextToken[1])^.KeywordIndex = kiDATABASE) then
    Nodes.StmtTag := ParseTag(kiDROP, kiDATABASE)
  else
    Nodes.StmtTag := ParseTag(kiDROP, kiSCHEMA);

  if (not Error and not EndOfStmt(CurrentToken) and (TokenPtr(CurrentToken)^.KeywordIndex = kiIF)) then
    Nodes.IfExistsTag := ParseTag(kiIF, kiEXISTS);

  if (not Error) then
    Nodes.DatabaseIdent := ParseDatabaseIdent();

  Result := TDropDatabaseStmt.Create(Self, Nodes);
end;

function TMySQLParser.ParseDropEventStmt(): TOffset;
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

function TMySQLParser.ParseDropIndexStmt(): TOffset;
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
  while (not Error and Found and not EndOfStmt(CurrentToken)) do
    if ((Nodes.AlgorithmValue = 0) and (TokenPtr(CurrentToken)^.KeywordIndex = kiALGORITHM)) then
      Nodes.AlgorithmValue := ParseValue(kiALGORITHM, vaAuto, WordIndices(kiDEFAULT, kiINPLACE, kiCOPY))
    else if ((Nodes.LockValue = 0) and (TokenPtr(CurrentToken)^.KeywordIndex = kiLOCK)) then
      Nodes.LockValue := ParseValue(kiLOCK, vaAuto, WordIndices(kiDEFAULT, kiNONE, kiSHARED, kiEXCLUSIVE))
    else
      Found := False;

  Result := TDropIndexStmt.Create(Self, Nodes);
end;

function TMySQLParser.ParseDropRoutineStmt(const ARoutineType: TRoutineType): TOffset;
var
  Nodes: TDropRoutineStmt.TNodes;
begin
  FillChar(Nodes, SizeOf(Nodes), 0);

  if (ARoutineType = rtFunction) then
    Nodes.StmtTag := ParseTag(kiDROP, kiFUNCTION)
  else
    Nodes.StmtTag := ParseTag(kiDROP, kiPROCEDURE);

  if (not Error and not EndOfStmt(CurrentToken) and (TokenPtr(CurrentToken)^.KeywordIndex = kiIF)) then
    Nodes.IfExistsTag := ParseTag(kiIF, kiEXISTS);

  if (not Error) then
    if (ARoutineType = rtFunction) then
      Nodes.RoutineIdent := ParseDbIdent(ditFunction)
    else
      Nodes.RoutineIdent := ParseDbIdent(ditProcedure);

  Result := TDropRoutineStmt.Create(Self, ARoutineType, Nodes);
end;

function TMySQLParser.ParseDropServerStmt(): TOffset;
var
  Nodes: TDropServerStmt.TNodes;
begin
  FillChar(Nodes, SizeOf(Nodes), 0);

  Nodes.StmtTag := ParseTag(kiDROP, kiSERVER);

  if (not Error and not EndOfStmt(CurrentToken) and (TokenPtr(CurrentToken)^.KeywordIndex = kiIF)) then
    Nodes.IfExistsTag := ParseTag(kiIF, kiEXISTS);

  if (not Error) then
    Nodes.ServerIdent := ParseDbIdent(ditServer);

  Result := TDropServerStmt.Create(Self, Nodes);
end;

function TMySQLParser.ParseDropTableStmt(): TOffset;
var
  Nodes: TDropTableStmt.TNodes;
begin
  FillChar(Nodes, SizeOf(Nodes), 0);

  if (TokenPtr(NextToken[1])^.KeywordIndex <> kiTEMPORARY) then
    Nodes.StmtTag := ParseTag(kiDROP, kiTABLE)
  else
    Nodes.StmtTag := ParseTag(kiDROP, kiTEMPORARY, kiTABLE);

  if (not Error and not EndOfStmt(CurrentToken) and (TokenPtr(CurrentToken)^.KeywordIndex = kiIF)) then
    Nodes.IfExistsTag := ParseTag(kiIF, kiEXISTS);

  if (not Error) then
    Nodes.TableIdentList := ParseList(False, ParseTableIdent);

  if (not Error and not EndOfStmt(CurrentToken)) then
    if (TokenPtr(CurrentToken)^.KeywordIndex = kiRESTRICT) then
      Nodes.RestrictCascadeTag := ParseTag(kiRESTRICT)
    else if (TokenPtr(CurrentToken)^.KeywordIndex = kiCASCADE) then
      Nodes.RestrictCascadeTag := ParseTag(kiCASCADE);

  Result := TDropTableStmt.Create(Self, Nodes);
end;

function TMySQLParser.ParseDropTriggerStmt(): TOffset;
var
  Nodes: TDropTriggerStmt.TNodes;
begin
  FillChar(Nodes, SizeOf(Nodes), 0);

  Nodes.StmtTag := ParseTag(kiDROP, kiTrigger);

  if (not Error and not EndOfStmt(CurrentToken) and (TokenPtr(CurrentToken)^.KeywordIndex = kiIF)) then
    Nodes.IfExistsTag := ParseTag(kiIF, kiEXISTS);

  if (not Error) then
    Nodes.TriggerIdent := ParseDbIdent(ditTrigger);

  Result := TDropTriggerStmt.Create(Self, Nodes);
end;

function TMySQLParser.ParseDropUserStmt(): TOffset;
var
  Nodes: TDropUserStmt.TNodes;
begin
  FillChar(Nodes, SizeOf(Nodes), 0);

  Nodes.StmtTag := ParseTag(kiDROP, kiUSER);

  if (not Error and not EndOfStmt(CurrentToken) and (TokenPtr(CurrentToken)^.KeywordIndex = kiIF)) then
    Nodes.IfExistsTag := ParseTag(kiIF, kiEXISTS);

  if (not Error) then
    Nodes.UserList := ParseList(False, ParseUserIdent);

  Result := TDropUserStmt.Create(Self, Nodes);
end;

function TMySQLParser.ParseDropViewStmt(): TOffset;
var
  Nodes: TDropViewStmt.TNodes;
begin
  FillChar(Nodes, SizeOf(Nodes), 0);

  Nodes.StmtTag := ParseTag(kiDROP, kiVIEW);

  if (not Error and not EndOfStmt(CurrentToken) and (TokenPtr(CurrentToken)^.KeywordIndex = kiIF)) then
    Nodes.IfExistsTag := ParseTag(kiIF, kiEXISTS);

  if (not Error) then
    Nodes.ViewIdentList := ParseList(False, ParseTableIdent);

  if (not Error and not EndOfStmt(CurrentToken)) then
    if (TokenPtr(CurrentToken)^.KeywordIndex = kiRESTRICT) then
      Nodes.RestrictCascadeTag := ParseTag(kiRESTRICT)
    else if (TokenPtr(CurrentToken)^.KeywordIndex = kiCASCADE) then
      Nodes.RestrictCascadeTag := ParseTag(kiCASCADE);

  Result := TDropViewStmt.Create(Self, Nodes);
end;

function TMySQLParser.ParseEventIdent(): TOffset;
begin
  Result := ParseDbIdent(ditEvent);
end;

function TMySQLParser.ParseEndLabel(): TOffset;
var
  Nodes: TEndLabel.TNodes;
begin
  FillChar(Nodes, SizeOf(Nodes), 0);

  Nodes.LabelToken := ApplyCurrentToken(utLabel);

  Result := TEndLabel.Create(Self, Nodes);
end;

function TMySQLParser.ParseExecuteStmt(): TOffset;
var
  Nodes: TExecuteStmt.TNodes;
begin
  FillChar(Nodes, SizeOf(Nodes), 0);

  Nodes.StmtTag := ParseTag(kiEXECUTE);

  if (not Error) then
    if (EndOfStmt(CurrentToken)) then
      SetError(PE_IncompleteStmt)
    else
      Nodes.StmtVariable := ParseVariable();

  if (not Error and not EndOfStmt(CurrentToken) and (TokenPtr(CurrentToken)^.KeywordIndex = kiUSING)) then
  begin
    Nodes.UsingTag := ParseTag(kiUSING);

    if (not Error) then
      Nodes.VariableIdents := ParseList(False, ParseVariable);
  end;

  Result := TExecuteStmt.Create(Self, Nodes);
end;

function TMySQLParser.ParseExistsFunc(): TOffset;
var
  Nodes: TExistsFunc.TNodes;
begin
  FillChar(Nodes, SizeOf(Nodes), 0);

  Nodes.FuncToken := ApplyCurrentToken(utFunction);

  if (not Error) then
    if (EndOfStmt(CurrentToken)) then
      SetError(PE_IncompleteStmt)
    else if (TokenPtr(CurrentToken)^.TokenType <> ttOpenBracket) then
      SetError(PE_UnexpectedToken)
    else
      Nodes.OpenBracket := ApplyCurrentToken();

  if (not Error) then
    if (EndOfStmt(CurrentToken)) then
      SetError(PE_IncompleteStmt)
    else if (TokenPtr(CurrentToken)^.KeywordIndex <> kiSELECT) then
      SetError(PE_UnexpectedToken)
    else
      Nodes.SubQuery := ParseSubAreaSelectStmt();

  if (not Error and (Nodes.OpenBracket > 0)) then
    if (EndOfStmt(CurrentToken)) then
      SetError(PE_IncompleteStmt)
    else if (TokenPtr(CurrentToken)^.TokenType <> ttCloseBracket) then
      SetError(PE_UnexpectedToken)
    else
      Nodes.CloseBracket := ApplyCurrentToken();

  Result := TExistsFunc.Create(Self, Nodes);
end;

function TMySQLParser.ParseExplainStmt(): TOffset;
var
  Nodes: TExplainStmt.TNodes;
begin
  FillChar(Nodes, SizeOf(Nodes), 0);

  Nodes.StmtTag := ParseTag(TokenPtr(CurrentToken)^.KeywordIndex);

  if (not Error) then
    if (EndOfStmt(CurrentToken)) then
      SetError(PE_IncompleteStmt)
    else if ((TokenPtr(CurrentToken)^.KeywordIndex <> kiEXTENDED)
      and (TokenPtr(CurrentToken)^.KeywordIndex <> kiPARTITIONS)
      and (TokenPtr(CurrentToken)^.KeywordIndex <> kiFORMAT)) then
    begin
      Nodes.TableIdent := ParseTableIdent();

      if (not Error and not EndOfStmt(CurrentToken)) then
        Nodes.ColumnIdent := ParseFieldIdent();
    end
    else
    begin
      if (TokenPtr(CurrentToken)^.KeywordIndex = kiEXTENDED) then
        Nodes.ExplainType := ParseTag(kiEXTENDED)
      else if (TokenPtr(CurrentToken)^.KeywordIndex = kiPARTITIONS) then
        Nodes.ExplainType := ParseTag(kiPARTITIONS)
      else if (TokenPtr(CurrentToken)^.KeywordIndex = kiFORMAT) then
      begin
        Nodes.ExplainType := ParseTag(kiFORMAT);

        if (not Error) then
          if (EndOfStmt(CurrentToken)) then
            SetError(PE_IncompleteStmt)
          else if (TokenPtr(CurrentToken)^.OperatorType = otEqual) then
          begin
            TokenPtr(CurrentToken)^.FOperatorType := otAssign;

            Nodes.AssignToken := ApplyCurrentToken();

            if (not Error) then
              if (EndOfStmt(CurrentToken)) then
                SetError(PE_IncompleteStmt)
              else if (TokenPtr(CurrentToken)^.KeywordIndex = kiTRADITIONAL) then
                Nodes.FormatKeyword := ApplyCurrentToken()
              else if (TokenPtr(CurrentToken)^.KeywordIndex = kiJSON) then
                Nodes.FormatKeyword := ApplyCurrentToken()
              else
                SetError(PE_UnexpectedToken);
          end;
      end;
    end;

  if (not Error) then
    if (EndOfStmt(CurrentToken)) then
      SetError(PE_IncompleteStmt)
    else if (TokenPtr(CurrentToken)^.KeywordIndex = kiSELECT) then
      Nodes.ExplainStmt := ParseSubAreaSelectStmt()
    else if (TokenPtr(CurrentToken)^.KeywordIndex = kiDELETE) then
      Nodes.ExplainStmt := ParseDeleteStmt()
    else if (TokenPtr(CurrentToken)^.KeywordIndex = kiINSERT) then
      Nodes.ExplainStmt := ParseInsertStmt(False)
    else if (TokenPtr(CurrentToken)^.KeywordIndex = kiREPLACE) then
      Nodes.ExplainStmt := ParseInsertStmt(True)
    else if (TokenPtr(CurrentToken)^.KeywordIndex = kiUPDATE) then
      Nodes.ExplainStmt := ParseUpdateStmt()
    else
      SetError(PE_UnexpectedToken);

  Result := TExplainStmt.Create(Self, Nodes);
end;

function TMySQLParser.ParseExpr(): TOffset;
begin
  Result := ParseExpr(True);
end;

function TMySQLParser.ParseExpr(const InAllowed: Boolean): TOffset;
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
  NodeCount := 0;

  repeat
    Node := CurrentToken;

    if (NodeCount = MaxNodeCount) then
      raise Exception.CreateFmt(STooManyTokensInExpr, [NodeCount])
    else if (EndOfStmt(Node)) then
      SetError(PE_IncompleteStmt)
    else if (TokenPtr(Node)^.TokenType in [ttColon, ttComma, ttCloseBracket]) then
      SetError(PE_UnexpectedToken)
    else if (TokenPtr(Node)^.KeywordIndex = kiSELECT) then
      Node := ParseSelectStmt()
    else if (TokenPtr(Node)^.KeywordIndex = kiINTERVAL) then
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
    else if (TokenPtr(Node)^.TokenType = ttOpenBracket) then
      if (EndOfStmt(NextToken[1])) then
        SetError(PE_IncompleteStmt)
      else if (TokenPtr(NextToken[1])^.KeywordIndex = kiSELECT) then
        Node := ParseSubAreaSelectStmt()
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
        if ((Length = 4) and (StrLIComp(Text, 'CAST', Length) = 0)) then
          Node := ParseCastFunc()
        else if ((Length = 4) and (StrLIComp(Text, 'CHAR', Length) = 0)) then
          Node := ParseCharFunc()
        else if ((Length = 7) and (StrLIComp(Text, 'CONVERT', Length) = 0)) then
          Node := ParseConvertFunc()
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
        else if ((Length = 6) and (StrLIComp(Text, 'SUBSTR', Length) = 0)) then
          Node := ParseSubstringFunc()
        else if ((Length = 9) and (StrLIComp(Text, 'SUBSTRING', Length) = 0)) then
          Node := ParseSubstringFunc()
        else if ((Length = 4) and (StrLIComp(Text, 'TRIM', Length) = 0)) then
          Node := ParseTrimFunc()
        else if ((Length = 13) and (StrLIComp(Text, 'WEIGHT_STRING', Length) = 0)) then
          Node := ParseWeightStringFunc()
        else // Func()
          Node := ParseFunctionCall()
      end
    else if (TokenPtr(Node)^.TokenType = ttAt) then
      Node := ParseVariable()
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

function TMySQLParser.ParseExtractFunc(): TOffset;
var
  Nodes: TExtractFunc.TNodes;
begin
  FillChar(Nodes, SizeOf(Nodes), 0);

  Nodes.FuncToken := ApplyCurrentToken(utFunction);

  if (not Error) then
    if (EndOfStmt(CurrentToken)) then
      SetError(PE_IncompleteStmt)
    else if (TokenPtr(CurrentToken)^.TokenType <> ttOpenBracket) then
      SetError(PE_UnexpectedToken)
    else
      Nodes.OpenBracket := ApplyCurrentToken();

  if (not Error) then
    if (EndOfStmt(CurrentToken)) then
      SetError(PE_IncompleteStmt)
    else if (TokenPtr(CurrentToken)^.TokenType <> ttIdent) then
      SetError(PE_UnexpectedToken)
    else
      Nodes.UnitTag := ParseIdent();

  if (not Error) then
    Nodes.FromTag := ParseTag(kiFROM);

  if (not Error) then
    Nodes.DateExpr := ParseExpr();

  if (not Error) then
    if (EndOfStmt(CurrentToken)) then
      SetError(PE_IncompleteStmt)
    else if (TokenPtr(CurrentToken)^.TokenType <> ttCloseBracket) then
      SetError(PE_UnexpectedToken)
    else
      Nodes.CloseBracket := ApplyCurrentToken();

  Result := TExtractFunc.Create(Self, Nodes);
end;

function TMySQLParser.ParseFetchStmt(): TOffset;
var
  Nodes: TFetchStmt.TNodes;
begin
  FillChar(Nodes, SizeOf(Nodes), 0);

  Nodes.StmtTag := ParseTag(kiFETCH);

  if (not Error) then
    if (not EndOfStmt(CurrentToken) and (TokenPtr(CurrentToken)^.KeywordIndex = kiNEXT)
      and not EndOfStmt(NextToken[1]) and (TokenPtr(NextToken[1])^.KeywordIndex = kiFROM)) then
      Nodes.FromTag := ParseTag(kiNEXT, kiFROM)
    else if (not EndOfStmt(CurrentToken) and (TokenPtr(CurrentToken)^.KeywordIndex = kiFROM)) then
      Nodes.FromTag := ParseTag(kiFROM);

  if (not Error) then
    if (EndOfStmt(CurrentToken)) then
      SetError(PE_IncompleteStmt)
    else if (not (TokenPtr(CurrentToken)^.TokenType in ttIdents)) then
      SetError(PE_UnexpectedToken)
    else
      Nodes.CursorIdent := ParseDbIdent(ditCursor);

  if (not Error) then
    if (EndOfStmt(CurrentToken)) then
      SetError(PE_IncompleteStmt)
    else if (TokenPtr(CurrentToken)^.KeywordIndex <> kiINTO) then
      SetError(PE_UnexpectedToken)
    else
      Nodes.IntoTag := ParseTag(kiINTO);

  if (not Error) then
    Nodes.VariableList := ParseList(False, ParseVariable);

  Result := TFetchStmt.Create(Self, Nodes);
end;

function TMySQLParser.ParseFlushStmt(): TOffset;
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

function TMySQLParser.ParseFlushStmtOption(): TOffset;
var
  Nodes: TFlushStmt.TOption.TNodes;
begin
  FillChar(Nodes, SizeOf(Nodes), 0);

  if (EndOfStmt(CurrentToken)) then
    SetError(PE_IncompleteStmt)
  else if (TokenPtr(CurrentToken)^.KeywordIndex = kiHOSTS) then
    Nodes.OptionTag := ParseTag(kiHOSTS)
  else if (TokenPtr(CurrentToken)^.KeywordIndex = kiPRIVILEGES) then
    Nodes.OptionTag := ParseTag(kiPRIVILEGES)
  else if (TokenPtr(CurrentToken)^.KeywordIndex = kiSTATUS) then
    Nodes.OptionTag := ParseTag(kiSTATUS)
  else if ((TokenPtr(CurrentToken)^.KeywordIndex = kiTABLE)
    or (TokenPtr(CurrentToken)^.KeywordIndex = kiTABLES)) then
  begin
    Nodes.OptionTag := ParseTag(TokenPtr(CurrentToken)^.KeywordIndex);

    if (not Error and not EndOfStmt(CurrentToken)) then
      Nodes.TablesList := ParseList(False, ParseTableIdent);
  end
  else
    SetError(PE_UnexpectedToken);

  Result := TFlushStmt.TOption.Create(Self, Nodes);
end;

function TMySQLParser.ParseForeignKeyIdent(): TOffset;
begin
  Result := ParseDbIdent(ditForeignKey);
end;

function TMySQLParser.ParseFunctionCall(): TOffset;
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

function TMySQLParser.ParseFunctionIdent(): TOffset;
begin
  Result := ParseDbIdent(ditFunction);
end;

function TMySQLParser.ParseFunctionParam(): TOffset;
var
  Nodes: TRoutineParam.TNodes;
begin
  FillChar(Nodes, SizeOf(Nodes), 0);

  if (EndOfStmt(CurrentToken)) then
    SetError(PE_IncompleteStmt)
  else if (not (TokenPtr(CurrentToken)^.TokenType in ttIdents)) then
    SetError(PE_UnexpectedToken)
  else
    Nodes.IdentToken := ParseDbIdent(ditParameter);

  if (not Error) then
    if (EndOfStmt(CurrentToken)) then
      SetError(PE_IncompleteStmt)
    else if (TokenPtr(CurrentToken)^.TokenType <> ttIdent) then
      SetError(PE_UnexpectedToken)
    else
      Nodes.DatatypeNode := ParseDatatype();

  Result := TRoutineParam.Create(Self, Nodes);
end;

function TMySQLParser.ParseFunctionReturns(): TOffset;
var
  Nodes: TFunctionReturns.TNodes;
begin
  FillChar(Nodes, SizeOf(Nodes), 0);

  Nodes.ReturnsTag := ParseTag(kiRETURNS);

  if (not Error) then
    if (EndOfStmt(CurrentToken)) then
      SetError(PE_IncompleteStmt)
    else if (TokenPtr(CurrentToken)^.TokenType <> ttIdent) then
      SetError(PE_UnexpectedToken)
    else
      Nodes.DatatypeNode := ParseDatatype();

  if (not Error and not EndOfStmt(CurrentToken) and (TokenPtr(CurrentToken)^.KeywordIndex = kiCHARSET)) then
    Nodes.CharsetValue := ParseValue(kiCHARSET, vaNO, ParseIdent);

  Result := TFunctionReturns.Create(Self, Nodes);
end;

function TMySQLParser.ParseGetDiagnosticsStmt(): TOffset;
var
  Nodes: TGetDiagnosticsStmt.TNodes;
begin
  FillChar(Nodes, SizeOf(Nodes), 0);

  Nodes.StmtTag := ParseTag(kiGET);

  if (not Error and not EndOfStmt(CurrentToken)) then
    if ((TokenPtr(CurrentToken)^.KeywordIndex = kiCURRENT) or (TokenPtr(CurrentToken)^.KeywordIndex = kiSTACKED)) then
      Nodes.ScopeTag := ParseTag(TokenPtr(CurrentToken)^.KeywordIndex);

  if (not Error) then
    if (EndOfStmt(CurrentToken)) then
      SetError(PE_IncompleteStmt)
    else if (TokenPtr(CurrentToken)^.KeywordIndex <> kiDIAGNOSTICS) then
      SetError(PE_UnexpectedToken)
    else
      Nodes.DiagnosticsTag := ParseTag(kiDIAGNOSTICS);

  if (not Error) then
    if (EndOfStmt(CurrentToken) or (TokenPtr(CurrentToken)^.KeywordIndex <> kiCONDITION)) then
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

function TMySQLParser.ParseGetDiagnosticsStmtStmtInfo(): TOffset;
var
  Nodes: TGetDiagnosticsStmt.TStmtInfo.TNodes;
begin
  FillChar(Nodes, SizeOf(Nodes), 0);

  Nodes.Target := ParseVariable();

  if (not Error) then
    if (EndOfStmt(CurrentToken)) then
      SetError(PE_IncompleteStmt)
    else if (TokenPtr(CurrentToken)^.OperatorType <> otEqual) then
      SetError(PE_UnexpectedToken)
    else
      Nodes.EqualOp := ApplyCurrentToken();

  if (not Error) then
    if (EndOfStmt(CurrentToken)) then
      SetError(PE_IncompleteStmt)
    else if ((TokenPtr(CurrentToken)^.KeywordIndex = kiNUMBER)
      or (TokenPtr(CurrentToken)^.KeywordIndex = kiROW_COUNT)) then
      Nodes.ItemTag := ParseTag(TokenPtr(CurrentToken)^.KeywordIndex)
    else
      SetError(PE_UnexpectedToken);

  Result := TGetDiagnosticsStmt.TStmtInfo.Create(Self, Nodes);
end;

function TMySQLParser.ParseGetDiagnosticsStmtConditionInfo(): TOffset;
var
  Nodes: TGetDiagnosticsStmt.TCondInfo.TNodes;
begin
  FillChar(Nodes, SizeOf(Nodes), 0);

  Nodes.Target := ParseVariable();

  if (not Error) then
    if (EndOfStmt(CurrentToken)) then
      SetError(PE_IncompleteStmt)
    else if (TokenPtr(CurrentToken)^.OperatorType <> otEqual) then
      SetError(PE_UnexpectedToken)
    else
      Nodes.EqualOp := ApplyCurrentToken();

  if (not Error) then
    if (EndOfStmt(CurrentToken)) then
      SetError(PE_IncompleteStmt)
    else if ((TokenPtr(CurrentToken)^.KeywordIndex = kiCLASS_ORIGIN)
      or (TokenPtr(CurrentToken)^.KeywordIndex = kiSUBCLASS_ORIGIN)
      or (TokenPtr(CurrentToken)^.KeywordIndex = kiRETURNED_SQLSTATE)
      or (TokenPtr(CurrentToken)^.KeywordIndex = kiMESSAGE_TEXT)
      or (TokenPtr(CurrentToken)^.KeywordIndex = kiMYSQL_ERRNO)
      or (TokenPtr(CurrentToken)^.KeywordIndex = kiCONSTRAINT_CATALOG)
      or (TokenPtr(CurrentToken)^.KeywordIndex = kiCONSTRAINT_SCHEMA)
      or (TokenPtr(CurrentToken)^.KeywordIndex = kiCONSTRAINT_NAME)
      or (TokenPtr(CurrentToken)^.KeywordIndex = kiCATALOG_NAME)
      or (TokenPtr(CurrentToken)^.KeywordIndex = kiSCHEMA_NAME)
      or (TokenPtr(CurrentToken)^.KeywordIndex = kiTABLE_NAME)
      or (TokenPtr(CurrentToken)^.KeywordIndex = kiCOLUMN_NAME)
      or (TokenPtr(CurrentToken)^.KeywordIndex = kiCURSOR_NAME)) then
      Nodes.ItemTag := ParseTag(TokenPtr(CurrentToken)^.KeywordIndex)
    else
      SetError(PE_UnexpectedToken);

  Result := TGetDiagnosticsStmt.TCondInfo.Create(Self, Nodes);
end;

function TMySQLParser.ParseGrantStmt(): TOffset;
var
  GrantOptionHandled: Boolean;
  ListNodes: TList.TNodes;
  MaxQueriesPerHourHandled: Boolean;
  MaxUpdatesPerHourHandled: Boolean;
  MaxConnectionsPerHourHandled: Boolean;
  MaxUserConnectionsHandled: Boolean;
  Nodes: TGrantStmt.TNodes;
  Resources: array of TOffset;
begin
  FillChar(Nodes, SizeOf(Nodes), 0);

  Nodes.StmtTag := ParseTag(kiGRANT);

  if (not Error) then
    if (EndOfStmt(CurrentToken)) then
      SetError(PE_UnexpectedToken)
    else if (TokenPtr(CurrentToken)^.KeywordIndex <> kiPROXY) then
    begin
      Nodes.PrivilegesList := ParseList(False, ParseGrantStmtPrivileg);

      if (not Error) then
        Nodes.OnTag := ParseTag(kiON);

      if (not Error) then
        if (EndOfStmt(CurrentToken)) then
          SetError(PE_IncompleteStmt)
        else if (TokenPtr(CurrentToken)^.KeywordIndex = kiTABLE) then
          Nodes.ObjectValue := ParseValue(kiTABLE, vaNo, ParseTableIdent)
        else if (TokenPtr(CurrentToken)^.KeywordIndex = kiFUNCTION) then
          Nodes.ObjectValue := ParseValue(kiFUNCTION, vaNo, ParseFunctionIdent)
        else if (TokenPtr(CurrentToken)^.KeywordIndex = kiPROCEDURE) then
          Nodes.ObjectValue := ParseValue(kiPROCEDURE, vaNo, ParseProcedureIdent)
        else
          Nodes.ObjectValue := ParseTableIdent();

      if (not Error) then
        Nodes.ToTag := ParseTag(kiTO);

      if (not Error) then
        Nodes.UserSpecifications := ParseList(False, ParseGrantStmtUserSpecification);

      if (not Error and not EndOfStmt(CurrentToken) and (TokenPtr(CurrentToken)^.KeywordIndex = kiWITH)) then
      begin
        GrantOptionHandled := False;
        MaxQueriesPerHourHandled := False;
        MaxUpdatesPerHourHandled := False;
        MaxConnectionsPerHourHandled := False;
        MaxUserConnectionsHandled := False;

        SetLength(Resources, 0);

        Nodes.WithTag := ParseTag(kiWITH);

        repeat
          if (EndOfStmt(CurrentToken)) then
            SetError(PE_IncompleteStmt)
          else if ((TokenPtr(CurrentToken)^.KeywordIndex = kiGRANT) and not GrantOptionHandled) then
          begin
            SetLength(Resources, Length(Resources) + 1);
            Resources[Length(Resources) - 1] := ParseTag(kiGRANT, kiOPTION);
            GrantOptionHandled := True;
          end
          else if ((TokenPtr(CurrentToken)^.KeywordIndex = kiMAX_QUERIES_PER_HOUR) and not MaxQueriesPerHourHandled) then
          begin
            SetLength(Resources, Length(Resources) + 1);
            Resources[Length(Resources) - 1] := ParseValue(kiMAX_QUERIES_PER_HOUR, vaNo, ParseInteger);
            MaxQueriesPerHourHandled := False;
          end
          else if ((TokenPtr(CurrentToken)^.KeywordIndex = kiMAX_UPDATES_PER_HOUR) and not MaxUpdatesPerHourHandled) then
          begin
            SetLength(Resources, Length(Resources) + 1);
            Resources[Length(Resources) - 1] := ParseValue(kiMAX_UPDATES_PER_HOUR, vaNo, ParseInteger);
            MaxUpdatesPerHourHandled := True;
          end
          else if ((TokenPtr(CurrentToken)^.KeywordIndex = kiMAX_CONNECTIONS_PER_HOUR) and not MaxConnectionsPerHourHandled) then
          begin
            SetLength(Resources, Length(Resources) + 1);
            Resources[Length(Resources) - 1] := ParseValue(kiMAX_CONNECTIONS_PER_HOUR, vaNo, ParseInteger);
            MaxConnectionsPerHourHandled := True;
          end
          else if ((TokenPtr(CurrentToken)^.KeywordIndex = kiMAX_USER_CONNECTIONS) and not MaxUserConnectionsHandled) then
          begin
            SetLength(Resources, Length(Resources) + 1);
            Resources[Length(Resources) - 1] := ParseValue(kiMAX_USER_CONNECTIONS, vaNo, ParseInteger);
            MaxUserConnectionsHandled := True;
          end
          else
            SetError(PE_UnexpectedToken);
        until (Error or EndOfStmt(CurrentToken));

        FillChar(ListNodes, SizeOf(ListNodes), 0);
        Nodes.ResourcesList := TList.Create(Self, ListNodes, ttUnknown, Length(Resources), Resources);
      end;
    end
    else
    begin
      Nodes.PrivilegesList := ParseTag(kiPROXY);

      if (not Error) then
        Nodes.OnTag := ParseTag(kiON);

      if (not Error) then
        if (EndOfStmt(CurrentToken)) then
          SetError(PE_IncompleteStmt)
        else
          Nodes.OnUser := ParseUserIdent();

      if (not Error) then
        Nodes.ToTag := ParseTag(kiTO);

      if (not Error) then
        Nodes.UserSpecifications := ParseList(False, ParseGrantStmtUserSpecification);

      if (not Error and not EndOfStmt(CurrentToken) and (TokenPtr(CurrentToken)^.KeywordIndex = kiWITH)) then
        Nodes.WithTag := ParseTag(kiWITH, kiGRANT, kiOPTION);
    end;

  Result := TGrantStmt.Create(Self, Nodes);
end;

function TMySQLParser.ParseGrantStmtPrivileg(): TOffset;
var
  Nodes: TGrantStmt.TPrivileg.TNodes;
begin
  FillChar(Nodes, SizeOf(Nodes), 0);

  if (EndOfStmt(CurrentToken)) then
    SetError(PE_IncompleteStmt)
  else if ((TokenPtr(CurrentToken)^.KeywordIndex = kiALL)
    and not EndOfStmt(NextToken[1]) and (TokenPtr(NextToken[1])^.KeywordIndex = kiPRIVILEGES)) then
    Nodes.PrivilegTag := ParseTag(kiALL, kiPRIVILEGES)
  else if (TokenPtr(CurrentToken)^.KeywordIndex = kiALL) then
    Nodes.PrivilegTag := ParseTag(kiALL)
  else if ((TokenPtr(CurrentToken)^.KeywordIndex = kiALTER)
    and not EndOfStmt(NextToken[1]) and (TokenPtr(NextToken[1])^.KeywordIndex = kiROUTINE)) then
    Nodes.PrivilegTag := ParseTag(kiALTER, kiROUTINE)
  else if (TokenPtr(CurrentToken)^.KeywordIndex = kiALTER) then
    Nodes.PrivilegTag := ParseTag(kiALTER)
  else if ((TokenPtr(CurrentToken)^.KeywordIndex = kiCREATE)
    and not EndOfStmt(NextToken[1]) and (TokenPtr(NextToken[1])^.KeywordIndex = kiROUTINE)) then
    Nodes.PrivilegTag := ParseTag(kiCREATE, kiROUTINE)
  else if ((TokenPtr(CurrentToken)^.KeywordIndex = kiCREATE)
    and not EndOfStmt(NextToken[1]) and (TokenPtr(NextToken[1])^.KeywordIndex = kiTABLESPACE)) then
    Nodes.PrivilegTag := ParseTag(kiCREATE, kiTABLESPACE)
  else if ((TokenPtr(CurrentToken)^.KeywordIndex = kiCREATE)
    and not EndOfStmt(NextToken[1]) and (TokenPtr(NextToken[1])^.KeywordIndex = kiTEMPORARY)
    and not EndOfStmt(NextToken[2]) and (TokenPtr(NextToken[2])^.KeywordIndex = kiTABLES)) then
    Nodes.PrivilegTag := ParseTag(kiCREATE, kiTEMPORARY, kiTABLES)
  else if ((TokenPtr(CurrentToken)^.KeywordIndex = kiCREATE)
    and not EndOfStmt(NextToken[1]) and (TokenPtr(NextToken[1])^.KeywordIndex = kiUSER)) then
    Nodes.PrivilegTag := ParseTag(kiCREATE, kiUSER)
  else if ((TokenPtr(CurrentToken)^.KeywordIndex = kiCREATE)
    and not EndOfStmt(NextToken[1]) and (TokenPtr(NextToken[1])^.KeywordIndex = kiVIEW)) then
    Nodes.PrivilegTag := ParseTag(kiCREATE, kiVIEW)
  else if (TokenPtr(CurrentToken)^.KeywordIndex = kiCREATE) then
    Nodes.PrivilegTag := ParseTag(kiCREATE)
  else if (TokenPtr(CurrentToken)^.KeywordIndex = kiDELETE) then
    Nodes.PrivilegTag := ParseTag(kiDELETE)
  else if (TokenPtr(CurrentToken)^.KeywordIndex = kiDROP) then
    Nodes.PrivilegTag := ParseTag(kiDROP)
  else if (TokenPtr(CurrentToken)^.KeywordIndex = kiEVENT) then
    Nodes.PrivilegTag := ParseTag(kiEVENT)
  else if (TokenPtr(CurrentToken)^.KeywordIndex = kiEXECUTE) then
    Nodes.PrivilegTag := ParseTag(kiEXECUTE)
  else if (TokenPtr(CurrentToken)^.KeywordIndex = kiFILE) then
    Nodes.PrivilegTag := ParseTag(kiFILE)
  else if ((TokenPtr(CurrentToken)^.KeywordIndex = kiGRANT)
    and not EndOfStmt(NextToken[1]) and (TokenPtr(NextToken[1])^.KeywordIndex = kiOPTION)) then
    Nodes.PrivilegTag := ParseTag(kiGRANT, kiOPTION)
  else if (TokenPtr(CurrentToken)^.KeywordIndex = kiINDEX) then
    Nodes.PrivilegTag := ParseTag(kiINDEX)
  else if (TokenPtr(CurrentToken)^.KeywordIndex = kiINSERT) then
    Nodes.PrivilegTag := ParseTag(kiINSERT)
  else if ((TokenPtr(CurrentToken)^.KeywordIndex = kiLOCK)
    and not EndOfStmt(NextToken[1]) and (TokenPtr(NextToken[1])^.KeywordIndex = kiTABLES)) then
    Nodes.PrivilegTag := ParseTag(kiLOCK, kiTABLES)
  else if (TokenPtr(CurrentToken)^.KeywordIndex = kiPROCESS) then
    Nodes.PrivilegTag := ParseTag(kiPROCESS)
  else if (TokenPtr(CurrentToken)^.KeywordIndex = kiPROXY) then
    Nodes.PrivilegTag := ParseTag(kiPROXY)
  else if (TokenPtr(CurrentToken)^.KeywordIndex = kiREFERENCES) then
    Nodes.PrivilegTag := ParseTag(kiREFERENCES)
  else if (TokenPtr(CurrentToken)^.KeywordIndex = kiRELOAD) then
    Nodes.PrivilegTag := ParseTag(kiRELOAD)
  else if ((TokenPtr(CurrentToken)^.KeywordIndex = kiREPLICATION)
    and not EndOfStmt(NextToken[1]) and (TokenPtr(NextToken[1])^.KeywordIndex = kiCLIENT)) then
    Nodes.PrivilegTag := ParseTag(kiREPLICATION, kiCLIENT)
  else if ((TokenPtr(CurrentToken)^.KeywordIndex = kiREPLICATION)
    and not EndOfStmt(NextToken[1]) and (TokenPtr(NextToken[1])^.KeywordIndex = kiSLAVE)) then
    Nodes.PrivilegTag := ParseTag(kiREPLICATION, kiSLAVE)
  else if (TokenPtr(CurrentToken)^.KeywordIndex = kiSELECT) then
    Nodes.PrivilegTag := ParseTag(kiSELECT)
  else if ((TokenPtr(CurrentToken)^.KeywordIndex = kiSHOW)
    and not EndOfStmt(NextToken[1]) and (TokenPtr(NextToken[1])^.KeywordIndex = kiDATABASES)) then
    Nodes.PrivilegTag := ParseTag(kiSHOW, kiDATABASES)
  else if ((TokenPtr(CurrentToken)^.KeywordIndex = kiSHOW)
    and not EndOfStmt(NextToken[1]) and (TokenPtr(NextToken[1])^.KeywordIndex = kiVIEW)) then
    Nodes.PrivilegTag := ParseTag(kiSHOW, kiVIEW)
  else if (TokenPtr(CurrentToken)^.KeywordIndex = kiSHUTDOWN) then
    Nodes.PrivilegTag := ParseTag(kiSHUTDOWN)
  else if (TokenPtr(CurrentToken)^.KeywordIndex = kiSUPER) then
    Nodes.PrivilegTag := ParseTag(kiSUPER)
  else if (TokenPtr(CurrentToken)^.KeywordIndex = kiTRIGGER) then
    Nodes.PrivilegTag := ParseTag(kiTRIGGER)
  else if (TokenPtr(CurrentToken)^.KeywordIndex = kiUPDATE) then
    Nodes.PrivilegTag := ParseTag(kiUPDATE)
  else if (TokenPtr(CurrentToken)^.KeywordIndex = kiUSAGE) then
    Nodes.PrivilegTag := ParseTag(kiUSAGE)
  else
    SetError(PE_UnexpectedToken);

  if (not Error and not EndOfStmt(CurrentToken) and (TokenPtr(CurrentToken)^.TokenType = ttOpenBracket)) then
    Nodes.PrivilegTag := ParseList(True, ParseFieldIdent);

  Result := TGrantStmt.TPrivileg.Create(Self, Nodes);
end;

function TMySQLParser.ParseGrantStmtUserSpecification(): TOffset;
var
  Nodes: TGrantStmt.TUserSpecification.TNodes;
begin
  FillChar(Nodes, SizeOf(Nodes), 0);

  Nodes.UserIdent := ParseUserIdent();

  if (not Error and not EndOfStmt(CurrentToken) and (TokenPtr(CurrentToken)^.KeywordIndex = kiIDENTIFIED)) then
  begin
    if (not EndOfStmt(NextToken[1]) and (TokenPtr(NextToken[1])^.KeywordIndex = kiBY)) then
    begin
      if (EndOfStmt(NextToken[2]) or (TokenPtr(NextToken[2])^.KeywordIndex <> kiPASSWORD)) then
        Nodes.IdentifiedToken := ParseTag(kiIDENTIFIED, kiBY)
      else
        Nodes.IdentifiedToken := ParseTag(kiIDENTIFIED, kiBY, kiPASSWORD);

      if (not Error) then
        if (EndOfStmt(CurrentToken)) then
          SetError(PE_IncompleteStmt)
        else if (TokenPtr(CurrentToken)^.TokenType in ttStrings - [ttIdent]) then
          Nodes.AuthString := ParseString()
        else if ((TokenPtr(CurrentToken)^.OperatorType = otLess)
          and not EndOfStmt(NextToken[2])
          and (TokenPtr(NextToken[1])^.TokenType = ttIdent)
          and (TokenPtr(NextToken[2])^.OperatorType = otGreater)) then
          Nodes.AuthString := ParseSecretIdent()
        else
          SetError(PE_UnexpectedToken);
    end
    else if (not EndOfStmt(NextToken[1]) and (TokenPtr(NextToken[1])^.KeywordIndex = kiWITH)) then
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

      if (not Error and not EndOfStmt(CurrentToken)) then
        if (TokenPtr(CurrentToken)^.KeywordIndex = kiAS) then
        begin
          Nodes.AsToken := ParseTag(kiAS);

          if (not Error) then
            if (EndOfStmt(CurrentToken)) then
              SetError(PE_IncompleteStmt)
            else if (TokenPtr(CurrentToken)^.TokenType <> ttString) then
              SetError(PE_UnexpectedToken)
            else
              Nodes.AuthString := ParseString();
        end
        else if (TokenPtr(CurrentToken)^.KeywordIndex = kiAS) then
        begin
          Nodes.AsToken := ParseTag(kiAS);

          if (not Error) then
            if (EndOfStmt(CurrentToken)) then
              SetError(PE_IncompleteStmt)
            else if (TokenPtr(CurrentToken)^.TokenType <> ttString) then
              SetError(PE_UnexpectedToken)
            else
              Nodes.AuthString := ParseString();
        end;
    end;
  end;

  Result := TGrantStmt.TUserSpecification.Create(Self, Nodes);
end;

function TMySQLParser.ParseGroupConcatFunc(): TOffset;
var
  Nodes: TGroupConcatFunc.TNodes;
begin
  FillChar(Nodes, SizeOf(Nodes), 0);

  Nodes.FuncToken := ApplyCurrentToken(utFunction);

  if (not Error) then
    if (EndOfStmt(CurrentToken)) then
      SetError(PE_IncompleteStmt)
    else if (TokenPtr(CurrentToken)^.TokenType <> ttOpenBracket) then
      SetError(PE_UnexpectedToken)
    else
      Nodes.OpenBracket := ApplyCurrentToken();

  if (not Error and not EndOfStmt(CurrentToken) and (TokenPtr(CurrentToken)^.KeywordIndex = kiDISTINCT)) then
    Nodes.DistinctTag := ParseTag(kiDISTINCT);

  if (not Error) then
    Nodes.ExprList := ParseList(False, ParseExpr);

  if (not Error and not EndOfStmt(CurrentToken) and (TokenPtr(CurrentToken)^.KeywordIndex = kiORDER)) then
  begin
    Nodes.OrderByTag := ParseTag(kiORDER, kiBY);

    if (not Error) then
      Nodes.OrderByExprList := ParseList(False, ParseGroupConcatFuncExpr);
  end;

  if (not Error and not EndOfStmt(CurrentToken) and (TokenPtr(CurrentToken)^.KeywordIndex = kiSEPARATOR)) then
    Nodes.SeparatorValue := ParseValue(kiSEPARATOR, vaNo, ParseString);

  if (not Error) then
    if (EndOfStmt(CurrentToken)) then
      SetError(PE_IncompleteStmt)
    else if (TokenPtr(CurrentToken)^.TokenType <> ttCloseBracket) then
      SetError(PE_UnexpectedToken)
    else
      Nodes.CloseBracket := ApplyCurrentToken();

  Result := TGroupConcatFunc.Create(Self, Nodes);
end;

function TMySQLParser.ParseGroupConcatFuncExpr(): TOffset;
var
  Nodes: TGroupConcatFunc.TExpr.TNodes;
begin
  FillChar(Nodes, SizeOf(Nodes), 0);

  Nodes.Expr := ParseExpr();

  if (not Error and not EndOfStmt(CurrentToken) and ((TokenPtr(CurrentToken)^.KeywordIndex = kiASC) or (TokenPtr(CurrentToken)^.KeywordIndex = kiDESC))) then
    Nodes.Direction := ParseTag(TokenPtr(CurrentToken)^.KeywordIndex);

  Result := TGroupConcatFunc.TExpr.Create(Self, Nodes);
end;

function TMySQLParser.ParseHelpStmt(): TOffset;
var
  Nodes: THelpStmt.TNodes;
begin
  FillChar(Nodes, SizeOf(Nodes), 0);

  Nodes.StmtTag := ParseTag(kiHELP);

  if (not Error) then
    if (EndOfStmt(CurrentToken)) then
      SetError(PE_IncompleteStmt)
    else if (TokenPtr(CurrentToken)^.TokenType <> ttString) then
      SetError(PE_UnexpectedToken)
    else
      Nodes.HelpString := ParseString();

  Result := THelpStmt.Create(Self, Nodes);
end;

function TMySQLParser.ParseIdent(): TOffset;
begin
  Result := 0;
  if (EndOfStmt(CurrentToken)) then
    SetError(PE_IncompleteStmt)
  else if (not (TokenPtr(CurrentToken)^.TokenType in ttIdents)) then
    SetError(PE_UnexpectedToken)
  else
    Result := ApplyCurrentToken();
end;

function TMySQLParser.ParseIfStmt(): TOffset;
var
  Branches: array of TOffset;
  ListNodes: TList.TNodes;
  Nodes: TIfStmt.TNodes;
begin
  FillChar(Nodes, SizeOf(Nodes), 0);

  SetLength(Branches, 1);
  Branches[0] := ParseIfStmtBranch();

  while (not Error) do
    if (EndOfStmt(CurrentToken)) then
      SetError(PE_IncompleteStmt)
    else if (TokenPtr(CurrentToken)^.KeywordIndex = kiELSEIF) then
    begin
      SetLength(Branches, Length(Branches) + 1);
      Branches[Length(Branches) - 1] := ParseIfStmtBranch();
    end
    else if (TokenPtr(CurrentToken)^.KeywordIndex = kiELSE) then
    begin
      SetLength(Branches, Length(Branches) + 1);
      Branches[Length(Branches) - 1] := ParseIfStmtBranch();
      break;
    end
    else if (TokenPtr(CurrentToken)^.KeywordIndex = kiEND) then
      break
    else
      SetError(PE_UnexpectedToken);

  if (not Error) then
    Nodes.EndTag := ParseTag(kiEND, kiIF);

  FillChar(ListNodes, SizeOf(ListNodes), 0);
  Nodes.BranchList := TList.Create(Self, ListNodes, ttUnknown, Length(Branches), Branches);
  SetLength(Branches, 0);

  Result := TIfStmt.Create(Self, Nodes);
end;

function TMySQLParser.ParseIfStmtBranch(): TOffset;
var
  Nodes: TIfStmt.TBranch.TNodes;
begin
  FillChar(Nodes, SizeOf(Nodes), 0);

  if (TokenPtr(CurrentToken)^.KeywordIndex = kiIF) then
  begin
    Nodes.Tag := ParseTag(kiIF);

    if (not Error) then
      Nodes.ConditionExpr := ParseExpr();

    if (not Error) then
      Nodes.ThenTag := ParseTag(kiTHEN);
  end
  else if (TokenPtr(CurrentToken)^.KeywordIndex = kiELSEIF) then
  begin
    Nodes.Tag := ParseTag(kiELSEIF);

    if (not Error) then
      Nodes.ConditionExpr := ParseExpr();

    if (not Error) then
      Nodes.ThenTag := ParseTag(kiTHEN);
  end
  else if (TokenPtr(CurrentToken)^.KeywordIndex = kiELSE) then
    Nodes.Tag := ParseTag(kiELSE)
  else
    SetError(PE_UnexpectedToken);

  if (not Error and not EndOfStmt(CurrentToken)
    and (TokenPtr(CurrentToken)^.KeywordIndex <> kiELSEIF)
    and (TokenPtr(CurrentToken)^.KeywordIndex <> kiELSE)
    and (TokenPtr(CurrentToken)^.KeywordIndex <> kiEND)) then
    Nodes.StmtList := ParseList(False, ParsePL_SQLStmt, ttDelimiter);

  Result := TIfStmt.TBranch.Create(Self, Nodes);
end;

function TMySQLParser.ParseIgnoreLines(): TOffset;
var
  Nodes: TIgnoreLines.TNodes;
begin
  FillChar(Nodes, SizeOf(Nodes), 0);

  Nodes.IgnoreTag := ParseTag(kiIGNORE);

  if (not Error) then
    Nodes.NumberToken := ParseInteger();

  if (not Error) then
    if (EndOfStmt(CurrentToken)) then
      SetError(PE_IncompleteStmt)
    else if (TokenPtr(CurrentToken)^.KeywordIndex = kiLINES) then
      Nodes.LinesTag := ParseTag(kiLINES)
    else if (TokenPtr(CurrentToken)^.KeywordIndex = kiROWS) then
      Nodes.LinesTag := ParseTag(kiROWS)
    else
      SetError(PE_UnexpectedToken);

  Result := TIgnoreLines.Create(Self, Nodes);
end;

function TMySQLParser.ParseInsertStmt(const Replace: Boolean = False): TOffset;
var
  Nodes: TInsertStmt.TNodes;
begin
  FillChar(Nodes, SizeOf(Nodes), 0);

  Nodes.InsertTag := ParseTag(TokenPtr(CurrentToken)^.KeywordIndex);

  if (not Error and not EndOfStmt(CurrentToken)) then
    if (TokenPtr(CurrentToken)^.KeywordIndex = kiLOW_PRIORITY) then
      Nodes.PriorityTag := ParseTag(kiLOW_PRIORITY)
    else if (TokenPtr(CurrentToken)^.KeywordIndex = kiDELAYED) then
      Nodes.PriorityTag := ParseTag(kiDELAYED)
    else if (TokenPtr(CurrentToken)^.KeywordIndex = kiHIGH_PRIORITY) then
      Nodes.PriorityTag := ParseTag(kiHIGH_PRIORITY);

  if (not Error and not EndOfStmt(CurrentToken) and (TokenPtr(CurrentToken)^.KeywordIndex = kiIGNORE)) then
    Nodes.IgnoreTag := ParseTag(kiIGNORE);

  if (not Error and not EndOfStmt(CurrentToken) and (TokenPtr(CurrentToken)^.KeywordIndex = kiINTO)) then
    Nodes.IntoTag := ParseTag(kiINTO);

  if (not Error) then
    if (EndOfStmt(CurrentToken)) then
      SetError(PE_IncompleteStmt)
    else if (not (TokenPtr(CurrentToken)^.TokenType in ttIdents)) then
      SetError(PE_UnexpectedToken)
    else
      Nodes.TableIdent := ParseTableIdent();

  if (not Error and not EndOfStmt(CurrentToken) and (TokenPtr(CurrentToken)^.KeywordIndex = kiPARTITION)) then
  begin
    Nodes.Partition.Tag := ParseTag(kiPARTITION);

    if (not Error) then
      Nodes.Partition.List := ParseList(True, ParsePartitionIdent);
  end;

  if (not Error) then
    if (EndOfStmt(CurrentToken)) then
      SetError(PE_IncompleteStmt)
    else if (TokenPtr(CurrentToken)^.KeywordIndex = kiSET) then
    begin
      Nodes.Set_.Tag := ParseTag(kiSET);

      if (not Error) then
        Nodes.Set_.List := ParseList(False, ParseInsertStmtSetItemsList);
    end
    else
    begin
      if (TokenPtr(CurrentToken)^.KeywordIndex = kiSELECT) then
        Nodes.SelectStmt := ParseSelectStmt()
      else if ((TokenPtr(CurrentToken)^.TokenType = ttOpenBracket)
        and not EndOfStmt(NextToken[1]) and (TokenPtr(NextToken[1])^.KeywordIndex = kiSELECT)) then
        Nodes.SelectStmt := ParseSubAreaSelectStmt()
      else
      begin
        if (TokenPtr(CurrentToken)^.TokenType = ttOpenBracket) then
          Nodes.ColumnList := ParseList(True, ParseFieldIdent);

        if (not Error) then
          if (EndOfStmt(CurrentToken)) then
            SetError(PE_IncompleteStmt)
          else if ((TokenPtr(CurrentToken)^.KeywordIndex = kiVALUES) or (TokenPtr(CurrentToken)^.KeywordIndex = kiVALUE)) then
          begin
            Nodes.Values.Tag := ParseTag(TokenPtr(CurrentToken)^.KeywordIndex);

            if (not Error) then
              Nodes.Values.List := ParseList(False, ParseInsertStmtValuesList);
          end
          else if ((Nodes.SelectStmt = 0) and (TokenPtr(CurrentToken)^.KeywordIndex = kiSELECT)) then
            Nodes.SelectStmt := ParseSelectStmt()
          else if ((Nodes.SelectStmt = 0) and (TokenPtr(CurrentToken)^.TokenType = ttOpenBracket)
            and not EndOfStmt(NextToken[1]) and (TokenPtr(NextToken[1])^.KeywordIndex = kiSELECT)) then
            Nodes.SelectStmt := ParseSubAreaSelectStmt()
          else
            SetError(PE_UnexpectedToken);
      end;
    end;

  if (not Error and not EndOfStmt(CurrentToken) and (TokenPtr(CurrentToken)^.KeywordIndex = kiON)) then
  begin
    Nodes.OnDuplicateKeyUpdateTag := ParseTag(kiON, kiDUPLICATE, kiKEY, kiUPDATE);

    if (not Error) then
      Nodes.UpdateList := ParseList(False, ParseUpdateStmtValue);
  end;

  Result := TInsertStmt.Create(Self, Nodes);
end;

function TMySQLParser.ParseInsertStmtSetItemsList(): TOffset;
var
  Nodes: TInsertStmt.TSetItem.TNodes;
begin
  FillChar(Nodes, SizeOf(Nodes), 0);

  if (EndOfStmt(CurrentToken)) then
    SetError(PE_IncompleteStmt)
  else
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

function TMySQLParser.ParseInsertStmtValuesList(): TOffset;
begin
  Result := ParseList(True, ParseExpr);
end;

function TMySQLParser.ParseInteger(): TOffset;
begin
  Result := 0;
  if (EndOfStmt(CurrentToken)) then
    SetError(PE_IncompleteStmt)
  else if (TokenPtr(CurrentToken)^.TokenType <> ttInteger) then
    SetError(PE_UnexpectedToken)
  else
    Result := ApplyCurrentToken();
end;

function TMySQLParser.ParseIntervalOp(): TOffset;
var
  Nodes: TIntervalOp.TNodes;
begin
  FillChar(Nodes, SizeOf(Nodes), 0);

  if (not Error) then
    Nodes.QuantityExp := ParseExpr();

  if (not Error) then
    if (EndOfStmt(CurrentToken)) then
      SetError(PE_IncompleteStmt)
    else if ((TokenPtr(CurrentToken)^.KeywordIndex <> kiDAY)
      and (TokenPtr(CurrentToken)^.KeywordIndex <> kiDAY_HOUR)
      and (TokenPtr(CurrentToken)^.KeywordIndex <> kiDAY_MINUTE)
      and (TokenPtr(CurrentToken)^.KeywordIndex <> kiDAY_SECOND)
      and (TokenPtr(CurrentToken)^.KeywordIndex <> kiHOUR)
      and (TokenPtr(CurrentToken)^.KeywordIndex <> kiHOUR_MINUTE)
      and (TokenPtr(CurrentToken)^.KeywordIndex <> kiHOUR_SECOND)
      and (TokenPtr(CurrentToken)^.KeywordIndex <> kiMICROSECOND)
      and (TokenPtr(CurrentToken)^.KeywordIndex <> kiMINUTE)
      and (TokenPtr(CurrentToken)^.KeywordIndex <> kiMINUTE_SECOND)
      and (TokenPtr(CurrentToken)^.KeywordIndex <> kiMONTH)
      and (TokenPtr(CurrentToken)^.KeywordIndex <> kiQUARTER)
      and (TokenPtr(CurrentToken)^.KeywordIndex <> kiSECOND)
      and (TokenPtr(CurrentToken)^.KeywordIndex <> kiWEEK)
      and (TokenPtr(CurrentToken)^.KeywordIndex <> kiYEAR)
      and (TokenPtr(CurrentToken)^.KeywordIndex <> kiYEAR_MONTH)) then
      SetError(PE_UnexpectedToken)
    else
      Nodes.UnitTag := ParseTag(TokenPtr(CurrentToken)^.KeywordIndex);

  Result := TIntervalOp.Create(Self, Nodes);
end;

function TMySQLParser.ParseIntervalOpList(): TIntervalList;
var
  Day: Boolean;
  DayHour: Boolean;
  DayMinute: Boolean;
  DaySecond: Boolean;
  Found: Boolean;
  Hour: Boolean;
  HourMinute: Boolean;
  HourSecond: Boolean;
  I: Integer;
  Index: Integer;
  Minute: Boolean;
  MinuteSecond: Boolean;
  Month: Boolean;
  Quarter: Boolean;
  Second: Boolean;
  Week: Boolean;
  Year: Boolean;
  YearMonth: Boolean;
begin
  for I := 0 to Length(Result) - 1 do
    Result[I] := 0;

  Day := False;
  DayHour := False;
  DayMinute := False;
  DaySecond := False;
  Hour := False;
  HourMinute := False;
  HourSecond := False;
  Minute := False;
  MinuteSecond := False;
  Month := False;
  Quarter := False;
  Second := False;
  Week := False;
  Year := False;
  YearMonth := False;
  Found := True;
  Index := 0;
  while (not Error and Found and not EndOfStmt(CurrentToken) and (TokenPtr(CurrentToken)^.OperatorType = otPlus)) do
    if (Index = Length(Result)) then
      raise Exception.Create(SArgumentOutOfRange)
    else if (EndOfStmt(NextToken[1])) then
      SetError(PE_IncompleteStmt)
    else if (TokenPtr(NextToken[1])^.KeywordIndex <> kiINTERVAL) then
      SetError(PE_UnexpectedToken, NextToken[1])
    else if (EndOfStmt(NextToken[3])) then
      SetError(PE_IncompleteStmt)
    else if (not Year and (TokenPtr(NextToken[3])^.KeywordIndex = kiYEAR)) then
    begin
      Result[Index] := ParseIntervalOpListItem();
      Inc(Index);
      Year := True;
      Found := True;
    end
    else if (not Quarter and (TokenPtr(NextToken[3])^.KeywordIndex = kiQUARTER)) then
    begin
      Result[Index] := ParseIntervalOpListItem();
      Inc(Index);
      Quarter := True;
      Found := True;
    end
    else if (not MONTH and (TokenPtr(NextToken[3])^.KeywordIndex = kiMONTH)) then
    begin
      Result[Index] := ParseIntervalOpListItem();
      Inc(Index);
      Month := True;
      Found := True;
    end
    else if (not Day and (TokenPtr(NextToken[3])^.KeywordIndex = kiDAY)) then
    begin
      Result[Index] := ParseIntervalOpListItem();
      Inc(Index);
      Day := True;
      Found := True;
    end
    else if (not Hour and (TokenPtr(NextToken[3])^.KeywordIndex = kiHOUR)) then
    begin
      Result[Index] := ParseIntervalOpListItem();
      Inc(Index);
      Hour := True;
      Found := True;
    end
    else if (not Minute and (TokenPtr(NextToken[3])^.KeywordIndex = kiMINUTE)) then
    begin
      Result[Index] := ParseIntervalOpListItem();
      Inc(Index);
      Minute := True;
      Found := True;
    end
    else if (not Week and (TokenPtr(NextToken[3])^.KeywordIndex = kiWEEK)) then
    begin
      Result[Index] := ParseIntervalOpListItem();
      Inc(Index);
      Week := True;
      Found := True;
    end
    else if (not Second and (TokenPtr(NextToken[3])^.KeywordIndex = kiSECOND)) then
    begin
      Result[Index] := ParseIntervalOpListItem();
      Inc(Index);
      Second := True;
      Found := True;
    end
    else if (not YearMonth and (TokenPtr(NextToken[3])^.KeywordIndex = kiYEAR_MONTH)) then
    begin
      Result[Index] := ParseIntervalOpListItem();
      Inc(Index);
      YearMonth := True;
      Found := True;
    end
    else if (not DayHour and (TokenPtr(NextToken[3])^.KeywordIndex = kiDAY_HOUR)) then
    begin
      Result[Index] := ParseIntervalOpListItem();
      Inc(Index);
      DayHour := True;
      Found := True;
    end
    else if (not DayMinute and (TokenPtr(NextToken[3])^.KeywordIndex = kiDAY_MINUTE)) then
    begin
      Result[Index] := ParseIntervalOpListItem();
      Inc(Index);
      DayMinute := True;
      Found := True;
    end
    else if (not DaySecond and (TokenPtr(NextToken[3])^.KeywordIndex = kiDAY_SECOND)) then
    begin
      Result[Index] := ParseIntervalOpListItem();
      Inc(Index);
      DaySecond := True;
      Found := True;
    end
    else if (not HourMinute and (TokenPtr(NextToken[3])^.KeywordIndex = kiHOUR_MINUTE)) then
    begin
      Result[Index] := ParseIntervalOpListItem();
      Inc(Index);
      HourMinute := True;
      Found := True;
    end
    else if (not HourSecond and (TokenPtr(NextToken[3])^.KeywordIndex = kiHOUR_SECOND)) then
    begin
      Result[Index] := ParseIntervalOpListItem();
      Inc(Index);
      HourSecond := True;
      Found := True;
    end
    else if (not MinuteSecond and (TokenPtr(NextToken[3])^.KeywordIndex = kiMINUTE_SECOND)) then
    begin
      Result[Index] := ParseIntervalOpListItem();
      Inc(Index);
      MinuteSecond := True;
      Found := True;
    end
    else
      Found := False;
end;

function TMySQLParser.ParseIntervalOpListItem(): TOffset;
var
  Nodes: TIntervalOp.TListItem.TNodes;
begin
  FillChar(Nodes, SizeOf(Nodes), 0);

  if (EndOfStmt(CurrentToken)) then
    SetError(PE_IncompleteStmt)
  else if (TokenPtr(CurrentToken)^.OperatorType <> otPlus) then
    SetError(PE_UnexpectedToken)
  else
    Nodes.PlusToken := ApplyCurrentToken();

  if (not Error) then
    if (EndOfStmt(CurrentToken)) then
      SetError(PE_IncompleteStmt)
    else if (TokenPtr(CurrentToken)^.KeywordIndex <> kiINTERVAL) then
      SetError(PE_UnexpectedToken)
    else
      Nodes.IntervalTag := ParseTag(kiINTERVAL);

  if (not Error) then
    if (EndOfStmt(CurrentToken)) then
      SetError(PE_IncompleteStmt)
    else
      Nodes.Interval := ParseIntervalOp();

  Result := TIntervalOp.TListItem.Create(Self, Nodes);
end;

function TMySQLParser.ParseIterateStmt(): TOffset;
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

function TMySQLParser.ParseKeyIdent(): TOffset;
begin
  Result := ParseDbIdent(ditKey);
end;

function TMySQLParser.ParseKeyword(): TOffset;
begin
  Result := 0;
  if (EndOfStmt(CurrentToken)) then
    SetError(PE_IncompleteStmt)
  else if (TokenPtr(CurrentToken)^.KeywordIndex < 0) then
    SetError(PE_UnexpectedToken)
  else
    Result := ApplyCurrentToken();
end;

function TMySQLParser.ParseKillStmt(): TOffset;
var
  Nodes: TKillStmt.TNodes;
begin
  FillChar(Nodes, SizeOf(Nodes), 0);

  if (EndOfStmt(NextToken[1])) then
    Nodes.StmtTag := ParseTag(kiKILL)
  else if (TokenPtr(NextToken[1])^.KeywordIndex = kiCONNECTION) then
    Nodes.StmtTag := ParseTag(kiKILL, kiCONNECTION)
  else if (TokenPtr(NextToken[1])^.KeywordIndex = kiQUERY) then
    Nodes.StmtTag := ParseTag(kiKILL, kiQUERY)
  else
    Nodes.StmtTag := ParseTag(kiKILL);

  if (not Error) then
    Nodes.ProcessIdToken := ParseExpr();

  Result := TKillStmt.Create(Self, Nodes);
end;

function TMySQLParser.ParseLeaveStmt(): TOffset;
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

function TMySQLParser.ParseList(const Brackets: Boolean; const ParseElement: TParseFunction; const DelimterType: TTokenType = ttComma): TOffset;
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
    if (EndOfStmt(CurrentToken)) then
      SetError(PE_IncompleteStmt)
    else if (TokenPtr(CurrentToken)^.TokenType <> ttOpenBracket) then
      SetError(PE_UnexpectedToken)
    else
      Nodes.OpenBracket := ApplyCurrentToken();

  Index := 0;
  if (not Error and (not Brackets or not EndOfStmt(CurrentToken) and (TokenPtr(CurrentToken)^.TokenType <> ttCloseBracket))) then
  begin
    repeat
      if (EndOfStmt(CurrentToken)) then
        SetError(PE_IncompleteStmt)
      else if (Index < Length(ChildrenArray)) then
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

      DelimiterFound := (CurrentToken > 0) and (TokenPtr(CurrentToken)^.TokenType = DelimterType);
      if (not Error and DelimiterFound) then
      begin
        if (Index < Length(ChildrenArray)) then
          ChildrenArray[Index] := ApplyCurrentToken() // Delimiter
        else
        begin
          if (Index = Length(ChildrenArray)) then
          begin
            ChildrenList := Classes.TList.Create();
            for I := 0 to Length(ChildrenArray) - 1 do
              ChildrenList.Add(Pointer(ChildrenArray[I]));
          end;
          ChildrenList.Add(Pointer(ApplyCurrentToken()));  // Delimiter
        end;
        Inc(Index);
      end;
    until (Error or EndOfStmt(CurrentToken) or not DelimiterFound
      or ((DelimterType = ttDelimiter) and
        ((TokenPtr(CurrentToken)^.KeywordIndex = kiELSE)
          or (TokenPtr(CurrentToken)^.KeywordIndex = kiELSEIF)
          or (TokenPtr(CurrentToken)^.KeywordIndex = kiUNTIL)
          or (TokenPtr(CurrentToken)^.KeywordIndex = kiWHEN)
          or (TokenPtr(CurrentToken)^.KeywordIndex = kiEND))));

    if (not Error and DelimiterFound and EndOfStmt(CurrentToken)) then
      SetError(PE_IncompleteStmt);
  end;

  if (not Error and Brackets) then
    if (EndOfStmt(CurrentToken)) then
      SetError(PE_IncompleteStmt)
    else if (TokenPtr(CurrentToken)^.TokenType <> ttCloseBracket) then
      SetError(PE_UnexpectedToken)
    else
      Nodes.CloseBracket := ApplyCurrentToken();

  if (not Assigned(ChildrenList)) then
    Result := TList.Create(Self, Nodes, DelimterType, Index, ChildrenArray)
  else
  begin
    Result := TList.Create(Self, Nodes, DelimterType, ChildrenList.Count, TIntegerArray(ChildrenList.List));
    ChildrenList.Free();
  end;
end;

function TMySQLParser.ParseLoadDataStmt(): TOffset;
var
  Nodes: TLoadDataStmt.TNodes;
begin
  FillChar(Nodes, SizeOf(Nodes), 0);

  Nodes.LoadDataTag := ParseTag(kiLOAD, kiDATA);

  if (not Error and not EndOfStmt(CurrentToken)) then
    if (TokenPtr(CurrentToken)^.KeywordIndex = kiLOW_PRIORITY) then
      Nodes.PriorityTag := ParseTag(kiLOW_PRIORITY)
    else if (TokenPtr(CurrentToken)^.KeywordIndex = kiCONCURRENT) then
      Nodes.PriorityTag := ParseTag(kiCONCURRENT);

  if (not Error) then
    if (EndOfStmt(CurrentToken)) then
      SetError(PE_IncompleteStmt)
    else if (TokenPtr(CurrentToken)^.KeywordIndex = kiLOCAL) then
      Nodes.InfileTag := ParseTag(kiLOCAL, kiINFILE)
    else if (TokenPtr(CurrentToken)^.KeywordIndex = kiINFILE) then
      Nodes.InfileTag := ParseTag(kiINFILE)
    else
      SetError(PE_UnexpectedToken);

  if (not Error) then
    if (EndOfStmt(CurrentToken)) then
      SetError(PE_IncompleteStmt)
    else if (not (TokenPtr(CurrentToken)^.TokenType in ttStrings)) then
      SetError(PE_UnexpectedToken)
    else
      Nodes.FilenameString := ParseString();

  if (not Error and not EndOfStmt(CurrentToken)) then
    if (TokenPtr(CurrentToken)^.KeywordIndex = kiREPLACE) then
      Nodes.ReplaceIgnoreTag := ParseTag(kiREPLACE)
    else if (TokenPtr(CurrentToken)^.KeywordIndex = kiIGNORE) then
      Nodes.ReplaceIgnoreTag := ParseTag(kiIGNORE);

  if (not Error) then
    if (EndOfStmt(CurrentToken)) then
      SetError(PE_IncompleteStmt)
    else
      Nodes.IntoTableValue := ParseValue(WordIndices(kiINTO, kiTABLE), vaNo, ParseTableIdent);

  if (not Error and not EndOfStmt(CurrentToken) and (TokenPtr(CurrentToken)^.KeywordIndex = kiPARTITION)) then
    Nodes.PartitionValue := ParseValue(kiPARTITION, vaNo, True, ParsePartitionIdent);

  if (not Error and not EndOfStmt(CurrentToken) and (TokenPtr(CurrentToken)^.KeywordIndex = kiCHARACTER)) then
    Nodes.CharacterSetValue := ParseValue(WordIndices(kiCHARACTER, kiSET), vaNo, ParseIdent);

  if (not Error and not EndOfStmt(CurrentToken) and ((TokenPtr(CurrentToken)^.KeywordIndex = kiFIELDS) or (TokenPtr(CurrentToken)^.KeywordIndex = kiCOLUMNS)))then
  begin
    Nodes.ColumnsTag := ParseTag(TokenPtr(CurrentToken)^.KeywordIndex);

    if (not Error and not EndOfStmt(CurrentToken) and (TokenPtr(CurrentToken)^.KeywordIndex = kiTERMINATED)) then
      Nodes.ColumnsTerminatedByValue := ParseValue(WordIndices(kiTERMINATED, kiBY), vaNo, ParseString);

    if (not Error and not EndOfStmt(CurrentToken)) then
      if (TokenPtr(CurrentToken)^.KeywordIndex = kiOPTIONALLY) then
        Nodes.EnclosedByValue := ParseValue(WordIndices(kiOPTIONALLY, kiENCLOSED, kiBY), vaNo, ParseString)
      else if (TokenPtr(CurrentToken)^.KeywordIndex = kiENCLOSED) then
        Nodes.EnclosedByValue := ParseValue(WordIndices(kiENCLOSED, kiBY), vaNo, ParseString);

    if (not Error and not EndOfStmt(CurrentToken) and (TokenPtr(CurrentToken)^.KeywordIndex = kiESCAPED)) then
      Nodes.EscapedByValue := ParseValue(WordIndices(kiESCAPED, kiBY), vaNo, ParseString);
  end;

  if (not Error and not EndOfStmt(CurrentToken) and (TokenPtr(CurrentToken)^.KeywordIndex = kiLINES)) then
  begin
    Nodes.LinesTag := ParseTag(kiLINES);

    if (not Error and not EndOfStmt(CurrentToken) and (TokenPtr(CurrentToken)^.KeywordIndex = kiSTARTING)) then
      Nodes.StartingByValue := ParseValue(WordIndices(kiSTARTING, kiBY), vaNo, ParseString);

    if (not Error and not EndOfStmt(CurrentToken) and (TokenPtr(CurrentToken)^.KeywordIndex = kiTERMINATED)) then
      Nodes.LinesTerminatedByValue := ParseValue(WordIndices(kiTERMINATED, kiBY), vaNo, ParseString);
  end;

  if (not Error and not EndOfStmt(CurrentToken) and (TokenPtr(CurrentToken)^.KeywordIndex = kiIGNORE)) then
    Nodes.IgnoreLines := ParseIgnoreLines();

  if (not Error and not EndOfStmt(CurrentToken) and (TokenPtr(CurrentToken)^.TokenType = ttOpenBracket)) then
    Nodes.ColumnList := ParseList(True, ParseFieldIdent);

  if (not Error and not EndOfStmt(CurrentToken) and (TokenPtr(CurrentToken)^.KeywordIndex = kiSET)) then
    Nodes.SetList := ParseValue(kiSET, vaNo, False, ParseUpdateStmtValue);

  Result := TLoadDataStmt.Create(Self, Nodes);
end;

function TMySQLParser.ParseLoadStmt(): TOffset;
begin
  Result := 0;
  if (EndOfStmt(NextToken[1])) then
    SetError(PE_IncompleteStmt)
  else if (TokenPtr(NextToken[1])^.KeywordIndex = kiDATA) then
    Result := ParseLoadDataStmt()
  else if (TokenPtr(NextToken[1])^.KeywordIndex = kiXML) then
    Result := ParseLoadXMLStmt()
  else
    SetError(PE_UnexpectedToken);
end;

function TMySQLParser.ParseLoadXMLStmt(): TOffset;
var
  Nodes: TLoadXMLStmt.TNodes;
  IgnoreLinesNodes: TIgnoreLines.TNodes;
begin
  FillChar(Nodes, SizeOf(Nodes), 0);

  Nodes.LoadXMLTag := ParseTag(kiLOAD, kiXML);

  if (not Error and not EndOfStmt(CurrentToken)) then
    if (TokenPtr(CurrentToken)^.KeywordIndex = kiLOW_PRIORITY) then
      Nodes.PriorityTag := ParseTag(kiLOW_PRIORITY)
    else if (TokenPtr(CurrentToken)^.KeywordIndex = kiCONCURRENT) then
      Nodes.PriorityTag := ParseTag(kiCONCURRENT);

  if (not Error and not EndOfStmt(CurrentToken) and (TokenPtr(CurrentToken)^.KeywordIndex = kiLOCAL)) then
    Nodes.LocalTag := ParseTag(kiLOCAL);

  if (not Error) then
    Nodes.InfileTag := ParseTag(kiINFILE);

  if (not Error and not EndOfStmt(CurrentToken)) then
    if (TokenPtr(CurrentToken)^.KeywordIndex = kiREPLACE) then
      Nodes.ReplaceIgnoreTag := ParseTag(kiREPLACE)
    else if (TokenPtr(CurrentToken)^.KeywordIndex = kiIGNORE) then
      Nodes.ReplaceIgnoreTag := ParseTag(kiIGNORE);

  if (not Error) then
    Nodes.IntoTableValue := ParseValue(WordIndices(kiINTO, kiTABLE), vaNo, ParseTableIdent);

  if (not Error and not EndOfStmt(CurrentToken) and (TokenPtr(CurrentToken)^.KeywordIndex = kiCHARACTER)) then
    Nodes.CharacterSetValue := ParseValue(WordIndices(kiCHARACTER, kiSET), vaNo, ParseIdent);

  if (not Error and not EndOfStmt(CurrentToken) and (TokenPtr(CurrentToken)^.KeywordIndex = kiROWS)) then
    Nodes.RowsIdentifiedByValue := ParseValue(WordIndices(kiROWS, kiIDENTIFIED, kiBY), vaNo, ParseString);

  if (not Error and not EndOfStmt(CurrentToken) and (TokenPtr(CurrentToken)^.KeywordIndex = kiIGNORE)) then
  begin
    FillChar(IgnoreLinesNodes, SizeOf(IgnoreLinesNodes), 0);

    IgnoreLinesNodes.IgnoreTag := ParseTag(kiIGNORE);

    if (not Error) then
      IgnoreLinesNodes.NumberToken := ParseInteger;

    if (not Error) then
      if (EndOfStmt(CurrentToken)) then
        SetError(PE_IncompleteStmt)
      else if (TokenPtr(CurrentToken)^.KeywordIndex = kiLINES) then
        IgnoreLinesNodes.LinesTag := ParseTag(kiLINES)
      else if (TokenPtr(CurrentToken)^.KeywordIndex = kiROWS) then
        IgnoreLinesNodes.LinesTag := ParseTag(kiROWS)
      else
        SetError(PE_UnexpectedToken);

    Nodes.IgnoreLines := TIgnoreLines.Create(Self, IgnoreLinesNodes);
  end;

  if (not Error and not EndOfStmt(CurrentToken) and (TokenPtr(CurrentToken)^.TokenType = ttOpenBracket)) then
    Nodes.ColumnList := ParseList(True, ParseFieldIdent);

  if (not Error and not EndOfStmt(CurrentToken) and (TokenPtr(CurrentToken)^.KeywordIndex = kiSET)) then
    Nodes.SetList := ParseValue(kiSET, vaNo, False, ParseUpdateStmtValue);

  Result := TLoadXMLStmt.Create(Self, Nodes);
end;

function TMySQLParser.ParseLockStmt(): TOffset;
var
  Nodes: TLockStmt.TNodes;
begin
  FillChar(Nodes, SizeOf(Nodes), 0);

  Nodes.LockTablesTag := ParseTag(kiLOCK, kiTABLES);

  if (not Error) then
    Nodes.ItemList := ParseList(False, ParseLockStmtItem);

  Result := TLockStmt.Create(Self, Nodes);
end;

function TMySQLParser.ParseLockStmtItem(): TOffset;
var
  Nodes: TLockStmt.TItem.TNodes;
begin
  FillChar(Nodes, SizeOf(Nodes), 0);

  Nodes.TableIdent := ParseTableIdent();

  if (not Error and not EndOfStmt(CurrentToken) and (TokenPtr(CurrentToken)^.KeywordIndex = kiAS)) then
  begin
    Nodes.AsToken := ParseTag(kiAS);

    if (not Error) then
      if (EndOfStmt(CurrentToken)) then
        SetError(PE_IncompleteStmt)
      else if (not (TokenPtr(CurrentToken)^.TokenType in ttIdents + ttStrings) or (TokenPtr(CurrentToken)^.KeywordIndex >= 0)) then
        SetError(PE_UnexpectedToken)
      else
        Nodes.AliasIdent := ParseAliasIdent();
  end;

  if (not Error and not EndOfStmt(CurrentToken)
    and (Nodes.AliasIdent = 0)
    and (TokenPtr(CurrentToken)^.TokenType in ttIdents + ttStrings) and (TokenPtr(CurrentToken)^.KeywordIndex < 0)) then
    Nodes.AliasIdent := ParseAliasIdent();

  if (not Error) then
    if (EndOfStmt(CurrentToken)) then
      SetError(PE_IncompleteStmt)
    else if (TokenPtr(CurrentToken)^.KeywordIndex = kiREAD) then
      if (not EndOfStmt(NextToken[1]) and (TokenPtr(NextToken[1])^.KeywordIndex = kiLOCAL)) then
        Nodes.TypeTag := ParseTag(kiREAD, kiLOCAL)
      else
        Nodes.TypeTag := ParseTag(kiREAD)
    else if (TokenPtr(CurrentToken)^.KeywordIndex = kiLOW_PRIORITY) then
      Nodes.TypeTag := ParseTag(kiLOW_PRIORITY, kiWRITE)
    else if (TokenPtr(CurrentToken)^.KeywordIndex = kiWRITE) then
      Nodes.TypeTag := ParseTag(kiWRITE)
    else
      SetError(PE_UnexpectedToken);

  Result := TLockStmt.TItem.Create(Self, Nodes);
end;

function TMySQLParser.ParseLoopStmt(): TOffset;
var
  BeginLabelToken: TOffset;
  Nodes: TLoopStmt.TNodes;
begin
  FillChar(Nodes, SizeOf(Nodes), 0);
  BeginLabelToken := 0;

  if (not EndOfStmt(CurrentToken) and (TokenPtr(CurrentToken)^.TokenType = ttIdent)
    and not EndOfStmt(NextToken[1]) and (TokenPtr(NextToken[1])^.TokenType = ttColon)) then
  begin
    BeginLabelToken := CurrentToken;
    Nodes.BeginLabel := ParseBeginLabel();
  end;

  Nodes.BeginTag := ParseTag(kiLOOP);

  if (not Error and not EndOfStmt(CurrentToken) and (TokenPtr(CurrentToken)^.KeywordIndex <> kiEND)) then
    Nodes.StmtList := ParseList(False, ParsePL_SQLStmt, ttDelimiter);

  if (not Error) then
    Nodes.EndTag := ParseTag(kiEND, kiLOOP);

  if (not Error and not EndOfStmt(CurrentToken) and (TokenPtr(CurrentToken)^.TokenType = ttIdent)) then
    if ((Nodes.BeginLabel = 0) or (StrIComp(PChar(TokenPtr(CurrentToken)^.AsString), PChar(TokenPtr(BeginLabelToken)^.AsString)) <> 0)) then
      SetError(PE_UnexpectedToken)
    else
      Nodes.EndLabel := ParseEndLabel();

  Result := TLoopStmt.Create(Self, Nodes);
end;

function TMySQLParser.ParseNumeric(): TOffset;
begin
  Result := 0;
  if (EndOfStmt(CurrentToken)) then
    SetError(PE_IncompleteStmt)
  else if (TokenPtr(CurrentToken)^.TokenType <> ttNumeric) then
    SetError(PE_UnexpectedToken)
  else
    Result := ApplyCurrentToken();
end;

function TMySQLParser.ParseMatchFunc(): TOffset;
var
  Nodes: TMatchFunc.TNodes;
begin
  FillChar(Nodes, SizeOf(Nodes), 0);

  Nodes.FuncToken := ApplyCurrentToken(utFunction);

  if (not Error) then
    Nodes.MatchList := ParseList(True, ParseFieldIdent);

  if (not Error) then
    if (EndOfStmt(CurrentToken)) then
      SetError(PE_IncompleteStmt)
    else if (TokenPtr(CurrentToken)^.KeywordIndex <> kiAGAINST) then
      SetError(PE_UnexpectedToken)
    else
      Nodes.AgainstToken := ApplyCurrentToken();

  if (not Error) then
    if (EndOfStmt(CurrentToken)) then
      SetError(PE_IncompleteStmt)
    else if (TokenPtr(CurrentToken)^.TokenType <> ttOpenBracket) then
      SetError(PE_UnexpectedToken)
    else
      Nodes.OpenBracket := ApplyCurrentToken();

  if (not Error) then
    if (EndOfStmt(CurrentToken)) then
      SetError(PE_IncompleteStmt)
    else
      Nodes.Expr := ParseExpr(False);

  if (not Error and not EndOfStmt(CurrentToken)) then
    if ((TokenPtr(CurrentToken)^.KeywordIndex = kiIN)
      and not EndOfStmt(NextToken[1]) and (TokenPtr(NextToken[1])^.KeywordIndex = kiNATURAL)
      and not EndOfStmt(NextToken[2]) and (TokenPtr(NextToken[2])^.KeywordIndex = kiLANGUAGE)
      and not EndOfStmt(NextToken[3]) and (TokenPtr(NextToken[3])^.KeywordIndex = kiMODE)
      and not EndOfStmt(NextToken[4]) and (TokenPtr(NextToken[4])^.KeywordIndex = kiWITH)) then
      Nodes.SearchModifierTag := ParseTag(kiIN, kiNATURAL, kiMODE, kiWITH, kiQUERY, kiEXPANSION)
    else if ((TokenPtr(CurrentToken)^.KeywordIndex = kiIN)
      and not EndOfStmt(NextToken[1]) and (TokenPtr(NextToken[1])^.KeywordIndex = kiNATURAL)
      and not EndOfStmt(NextToken[2]) and (TokenPtr(NextToken[2])^.KeywordIndex = kiLANGUAGE)
      and not EndOfStmt(NextToken[3]) and (TokenPtr(NextToken[3])^.KeywordIndex = kiMODE)) then
      Nodes.SearchModifierTag := ParseTag(kiIN, kiNATURAL, kiMODE)
    else if ((TokenPtr(CurrentToken)^.KeywordIndex = kiIN)
      and not EndOfStmt(NextToken[1]) and (TokenPtr(NextToken[1])^.KeywordIndex = kiBOOLEAN)
      and not EndOfStmt(NextToken[2]) and (TokenPtr(NextToken[2])^.KeywordIndex = kiMODE)) then
      Nodes.SearchModifierTag := ParseTag(kiIN, kiBOOLEAN, kiMODE)
    else if ((TokenPtr(CurrentToken)^.KeywordIndex = kiWITH)
      and not EndOfStmt(NextToken[1]) and (TokenPtr(NextToken[1])^.KeywordIndex = kiQUERY)
      and not EndOfStmt(NextToken[2]) and (TokenPtr(NextToken[2])^.KeywordIndex = kiEXPANSION)) then
      Nodes.SearchModifierTag := ParseTag(kiWITH, kiQUERY, kiEXPANSION);

  if (not Error) then
    if (EndOfStmt(CurrentToken)) then
      SetError(PE_IncompleteStmt)
    else if (TokenPtr(CurrentToken)^.TokenType <> ttCloseBracket) then
      SetError(PE_UnexpectedToken)
    else
      Nodes.CloseBracket := ApplyCurrentToken();

  Result := TMatchFunc.Create(Self, Nodes);
end;

function TMySQLParser.ParseProcedureIdent(): TOffset;
begin
  Result := ParseDbIdent(ditProcedure);
end;

function TMySQLParser.ParseOpenStmt(): TOffset;
var
  Nodes: TOpenStmt.TNodes;
begin
  FillChar(Nodes, SizeOf(Nodes), 0);

  Nodes.StmtTag := ParseTag(kiOPEN);

  if (not Error) then
    if (EndOfStmt(CurrentToken)) then
      SetError(PE_IncompleteStmt)
    else if (not (TokenPtr(CurrentToken)^.TokenType in ttIdents)) then
      SetError(PE_UnexpectedToken)
    else
      Nodes.CursorIdent := ParseDbIdent(ditCursor);

  Result := TOpenStmt.Create(Self, Nodes);
end;

function TMySQLParser.ParseOptimizeStmt(): TOffset;
var
  Nodes: TOptimizeStmt.TNodes;
begin
  FillChar(Nodes, SizeOf(Nodes), 0);

  Nodes.StmtTag := ParseTag(kiOPTIMIZE);

  if (not Error) then
    if (EndOfStmt(CurrentToken)) then
      SetError(PE_IncompleteStmt)
    else if (TokenPtr(CurrentToken)^.KeywordIndex = kiNO_WRITE_TO_BINLOG) then
      Nodes.OptionTag := ParseTag(kiNO_WRITE_TO_BINLOG)
    else if (TokenPtr(CurrentToken)^.KeywordIndex = kiLOCAL) then
      Nodes.OptionTag := ParseTag(kiLOCAL)
    else
      SetError(PE_UnexpectedToken);

  if (not Error) then
    if (EndOfStmt(CurrentToken)) then
      SetError(PE_IncompleteStmt)
    else if (TokenPtr(CurrentToken)^.KeywordIndex <> kiTABLE) then
      SetError(PE_UnexpectedToken)
    else
      Nodes.TableTag := ParseTag(kiTABLE);

  if (not Error) then
    Nodes.TablesList := ParseList(False, ParseTableIdent);

  Result := TOptimizeStmt.Create(Self, Nodes);
end;

function TMySQLParser.ParsePartitionIdent(): TOffset;
begin
  Result := ParseDbIdent(ditPartition);
end;

function TMySQLParser.ParsePositionFunc(): TOffset;
var
  Nodes: TPositionFunc.TNodes;
begin
  FillChar(Nodes, SizeOf(Nodes), 0);

  Nodes.FuncToken := ApplyCurrentToken(utFunction);

  if (not Error) then
    if (EndOfStmt(CurrentToken)) then
      SetError(PE_IncompleteStmt)
    else if (TokenPtr(CurrentToken)^.TokenType <> ttOpenBracket) then
      SetError(PE_UnexpectedToken)
    else
      Nodes.OpenBracket := ApplyCurrentToken();

  if (not Error) then
    if (EndOfStmt(CurrentToken)) then
      SetError(PE_IncompleteStmt)
    else
      Nodes.SubStr := ParseExpr(False);

  if (not Error) then
    if (EndOfStmt(CurrentToken)) then
      SetError(PE_IncompleteStmt)
    else if (TokenPtr(CurrentToken)^.KeywordIndex <> kiIN) then
      SetError(PE_UnexpectedToken)
    else
      Nodes.InTag := ParseTag(kiIN);

  if (not Error) then
    Nodes.Str := ParseExpr();

  if (not Error) then
    if (EndOfStmt(CurrentToken)) then
      SetError(PE_IncompleteStmt)
    else if (TokenPtr(CurrentToken)^.TokenType <> ttCloseBracket) then
      SetError(PE_UnexpectedToken)
    else
      Nodes.CloseBracket := ApplyCurrentToken();

  Result := TPositionFunc.Create(Self, Nodes);
end;

function TMySQLParser.ParsePL_SQLStmt(): TOffset;
begin
  BeginPL_SQL();
  Result := ParseStmt();
  EndPL_SQL();
end;

function TMySQLParser.ParsePrepareStmt(): TOffset;
var
  Nodes: TPrepareStmt.TNodes;
begin
  FillChar(Nodes, SizeOf(Nodes), 0);

  Nodes.StmtTag := ParseTag(kiPREPARE);

  if (not Error) then
    if (EndOfStmt(CurrentToken)) then
      SetError(PE_IncompleteStmt)
    else
      Nodes.StmtIdent := ParseIdent();

  if (not Error) then
    if (EndOfStmt(CurrentToken)) then
      SetError(PE_IncompleteStmt)
    else if (TokenPtr(CurrentToken)^.KeywordIndex <> kiFROM) then
      SetError(PE_UnexpectedToken)
    else
      Nodes.FromTag := ParseTag(kiFROM);

  if (not Error) then
    if (EndOfStmt(CurrentToken)) then
      SetError(PE_IncompleteStmt)
    else
      Nodes.StmtVariable := ParseVariable();

  Result := TPrepareStmt.Create(Self, Nodes);
end;

function TMySQLParser.ParsePurgeStmt(): TOffset;
var
  Nodes: TPurgeStmt.TNodes;
begin
  FillChar(Nodes, SizeOf(Nodes), 0);

  Nodes.StmtTag := ParseTag(kiPURGE);

  if (not Error) then
    if (EndOfStmt(CurrentToken)) then
      SetError(PE_IncompleteStmt)
    else if (TokenPtr(CurrentToken)^.KeywordIndex = kiBINARY) then
      Nodes.TypeTag := ParseTag(kiBINARY)
    else if (TokenPtr(CurrentToken)^.KeywordIndex = kiMASTER) then
      Nodes.TypeTag := ParseTag(kiMASTER)
    else
      SetError(PE_UnexpectedToken);

  if (not Error) then
    if (EndOfStmt(CurrentToken)) then
      SetError(PE_IncompleteStmt)
    else if (TokenPtr(CurrentToken)^.KeywordIndex <> kiLOGS) then
      SetError(PE_UnexpectedToken)
    else
      Nodes.LogsTag := ParseTag(kiLOGS);

  if (not Error) then
    if (EndOfStmt(CurrentToken)) then
      SetError(PE_IncompleteStmt)
    else if (TokenPtr(CurrentToken)^.KeywordIndex = kiTO) then
      Nodes.Value := ParseValue(kiTO, vaNo, ParseString)
    else if (TokenPtr(CurrentToken)^.KeywordIndex = kiBEFORE) then
      Nodes.Value := ParseValue(kiBEFORE, vaNo, ParseExpr)
    else
      SetError(PE_UnexpectedToken);

  Result := TPurgeStmt.Create(Self, Nodes);
end;

function TMySQLParser.ParseProcedureParam(): TOffset;
var
  Nodes: TRoutineParam.TNodes;
begin
  FillChar(Nodes, SizeOf(Nodes), 0);

  if (EndOfStmt(CurrentToken)) then
    SetError(PE_IncompleteStmt)
  else if (TokenPtr(CurrentToken)^.KeywordIndex = kiIN) then
    Nodes.DirektionTag := ParseTag(kiIN)
  else if (TokenPtr(CurrentToken)^.KeywordIndex = kiOUT) then
    Nodes.DirektionTag := ParseTag(kiOUT)
  else if (TokenPtr(CurrentToken)^.KeywordIndex = kiINOUT) then
    Nodes.DirektionTag := ParseTag(kiINOUT);

  if (not Error) then
    if (EndOfStmt(CurrentToken)) then
      SetError(PE_IncompleteStmt)
    else if (not (TokenPtr(CurrentToken)^.TokenType in ttIdents)) then
      SetError(PE_UnexpectedToken)
    else
      Nodes.IdentToken := ParseDbIdent(ditParameter);

  if (not Error) then
    if (EndOfStmt(CurrentToken)) then
      SetError(PE_IncompleteStmt)
    else if (TokenPtr(CurrentToken)^.TokenType <> ttIdent) then
      SetError(PE_UnexpectedToken)
    else
      Nodes.DatatypeNode := ParseDatatype();

  Result := TRoutineParam.Create(Self, Nodes);
end;

function TMySQLParser.ParseReleaseStmt(): TOffset;
var
  Nodes: TReleaseStmt.TNodes;
begin
  FillChar(Nodes, SizeOf(Nodes), 0);

  Nodes.ReleaseTag := ParseTag(kiRELEASE, kiSAVEPOINT);

  if (not Error) then
    Nodes.Ident := ParseSavepointIdent();

  Result := TReleaseStmt.Create(Self, Nodes);
end;

function TMySQLParser.ParseRenameStmt(): TOffset;
var
  Nodes: TRenameStmt.TNodes;
begin
  FillChar(Nodes, SizeOf(Nodes), 0);

  if (EndOfStmt(NextToken[1])) then
    SetError(PE_IncompleteStmt, NextToken[1])
  else if (TokenPtr(NextToken[1])^.KeywordIndex = kiTABLE) then
  begin
    Nodes.RenameTag := ParseTag(kiRENAME, kiTABLE);

    if (not Error) then
      Nodes.RenameList := ParseList(False, ParseRenameStmtTablePair);
  end
  else if (TokenPtr(NextToken[1])^.KeywordIndex = kiUSER) then
  begin
    Nodes.RenameTag := ParseTag(kiRENAME, kiUSER);

    if (not Error) then
      Nodes.RenameList := ParseList(False, ParseRenameStmtUserPair);
  end
  else
    SetError(PE_UnexpectedToken, NextToken[1]);

  Result := TRenameStmt.Create(Self, Nodes)
end;

function TMySQLParser.ParseRenameStmtTablePair(): TOffset;
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

function TMySQLParser.ParseRenameStmtUserPair(): TOffset;
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

function TMySQLParser.ParseRepairStmt(): TOffset;
var
  Nodes: TRepairStmt.TNodes;
begin
  FillChar(Nodes, SizeOf(Nodes), 0);

  Nodes.StmtTag := ParseTag(kiREPAIR);

  if (not Error) then
    if (EndOfStmt(CurrentToken)) then
      SetError(PE_IncompleteStmt)
    else if (TokenPtr(CurrentToken)^.KeywordIndex = kiNO_WRITE_TO_BINLOG) then
      Nodes.OptionTag := ParseTag(kiNO_WRITE_TO_BINLOG)
    else if (TokenPtr(CurrentToken)^.KeywordIndex = kiLOCAL) then
      Nodes.OptionTag := ParseTag(kiLOCAL)
    else
      SetError(PE_UnexpectedToken);

  if (not Error) then
    if (EndOfStmt(CurrentToken)) then
      SetError(PE_IncompleteStmt)
    else if (TokenPtr(CurrentToken)^.KeywordIndex <> kiTABLE) then
      SetError(PE_UnexpectedToken)
    else
      Nodes.TableTag := ParseTag(kiTABLE);

  if (not Error) then
    Nodes.TablesList := ParseList(False, ParseTableIdent);

  if (not Error and not EndOfStmt(CurrentToken)) then
    if (TokenPtr(CurrentToken)^.KeywordIndex = kiQUICK) then
      Nodes.OptionTag := ParseTag(kiQUICK)
    else if (TokenPtr(CurrentToken)^.KeywordIndex = kiEXTENDED) then
      Nodes.OptionTag := ParseTag(kiEXTENDED)
    else if (TokenPtr(CurrentToken)^.KeywordIndex = kiUSE_FRM) then
      Nodes.OptionTag := ParseTag(kiUSE_FRM)
    else
      SetError(PE_UnexpectedToken);

  Result := TRepairStmt.Create(Self, Nodes);
end;

function TMySQLParser.ParseRepeatStmt(): TOffset;
var
  BeginLabelToken: TOffset;
  Nodes: TRepeatStmt.TNodes;
begin
  FillChar(Nodes, SizeOf(Nodes), 0);
  BeginLabelToken := 0;

  if (not EndOfStmt(CurrentToken) and (TokenPtr(CurrentToken)^.TokenType = ttIdent)
    and not EndOfStmt(NextToken[1]) and (TokenPtr(NextToken[1])^.TokenType = ttColon)) then
  begin
    BeginLabelToken := CurrentToken;
    Nodes.BeginLabel := ParseBeginLabel();
  end;

  Nodes.RepeatTag := ParseTag(kiREPEAT);

  if (not Error and not EndOfStmt(CurrentToken) and (TokenPtr(CurrentToken)^.KeywordIndex <> kiUNTIL)) then
    Nodes.StmtList := ParseList(False, ParsePL_SQLStmt, ttDelimiter);

  if (not Error) then
    Nodes.UntilTag := ParseTag(kiUNTIL);

  if (not Error) then
    Nodes.SearchConditionExpr := ParseExpr();

  if (not Error) then
    Nodes.EndTag := ParseTag(kiEND, kiREPEAT);

  if (not Error and not EndOfStmt(CurrentToken) and (TokenPtr(CurrentToken)^.TokenType = ttIdent)) then
    if ((Nodes.BeginLabel = 0) or (StrIComp(PChar(TokenPtr(CurrentToken)^.AsString), PChar(TokenPtr(BeginLabelToken)^.AsString)) <> 0)) then
      SetError(PE_UnexpectedToken)
    else
      Nodes.EndLabel := ParseEndLabel();

  Result := TRepeatStmt.Create(Self, Nodes);
end;

function TMySQLParser.ParseRevokeStmt(): TOffset;
var
  Nodes: TRevokeStmt.TNodes;
begin
  FillChar(Nodes, SizeOf(Nodes), 0);

  Nodes.StmtTag := ParseTag(kiREVOKE);

  if (not Error) then
    if (EndOfStmt(CurrentToken)) then
      SetError(PE_UnexpectedToken)
    else if ((TokenPtr(CurrentToken)^.KeywordIndex <> kiALL)
      and (TokenPtr(CurrentToken)^.KeywordIndex <> kiPROXY)) then
    begin
      Nodes.PrivilegesList := ParseList(False, ParseGrantStmtPrivileg);

      if (not Error) then
        if (EndOfStmt(CurrentToken)) then
          SetError(PE_IncompleteStmt)
        else if (TokenPtr(CurrentToken)^.KeywordIndex <> kiON) then
          SetError(PE_UnexpectedToken)
        else
          Nodes.OnTag := ParseTag(kiON);

      if (not Error) then
        if (EndOfStmt(CurrentToken)) then
          SetError(PE_IncompleteStmt)
        else if (TokenPtr(CurrentToken)^.KeywordIndex = kiTABLE) then
          Nodes.ObjectValue := ParseValue(kiTABLE, vaNo, ParseTableIdent)
        else if (TokenPtr(CurrentToken)^.KeywordIndex = kiFUNCTION) then
          Nodes.ObjectValue := ParseValue(kiFUNCTION, vaNo, ParseFunctionIdent)
        else if (TokenPtr(CurrentToken)^.KeywordIndex = kiPROCEDURE) then
          Nodes.ObjectValue := ParseValue(kiFUNCTION, vaNo, ParseProcedureIdent)
        else
          Nodes.ObjectValue := ParseTableIdent();

      if (not Error) then
        if (EndOfStmt(CurrentToken)) then
          SetError(PE_IncompleteStmt)
        else if (TokenPtr(CurrentToken)^.KeywordIndex <> kiFROM) then
          SetError(PE_UnexpectedToken)
        else
          Nodes.FromTag := ParseTag(kiFROM);

      if (not Error) then
        Nodes.UserIdentList := ParseList(False, ParseUserIdent);
    end
    else if (TokenPtr(CurrentToken)^.KeywordIndex = kiALL) then
    begin
      Nodes.PrivilegesList := ParseTag(kiALL, kiPRIVILEGES);

      if (not Error) then
        if (EndOfStmt(CurrentToken)) then
          SetError(PE_IncompleteStmt)
        else if (TokenPtr(CurrentToken)^.TokenType <> ttComma) then
          SetError(PE_UnexpectedToken)
        else
          Nodes.CommaToken := ApplyCurrentToken();

      if (not Error) then
        if (EndOfStmt(CurrentToken)) then
          SetError(PE_IncompleteStmt)
        else
          Nodes.GrantOptionTag := ParseTag(kiGRANT, kiOPTION);

      if (not Error) then
        Nodes.UserIdentList := ParseList(False, ParseUserIdent);
    end
    else
    begin
      Nodes.PrivilegesList := ParseTag(kiPROXY);

      if (not Error) then
        Nodes.OnTag := ParseTag(kiON);

      if (not Error) then
        if (EndOfStmt(CurrentToken)) then
          SetError(PE_IncompleteStmt)
        else
          Nodes.OnUser := ParseUserIdent();

      if (not Error) then
        Nodes.FromTag := ParseTag(kiFROM);

      if (not Error) then
        Nodes.UserIdentList := ParseList(False, ParseUserIdent);
    end;

  Result := TRevokeStmt.Create(Self, Nodes);
end;

function TMySQLParser.ParseResetStmt(): TOffset;
var
  Nodes: TResetStmt.TNodes;
begin
  FillChar(Nodes, SizeOf(Nodes), 0);

  Nodes.StmtTag := ParseTag(kiRESET);

  if (not Error) then
    Nodes.OptionList := ParseList(False, ParseResetStmtOption);

  Result := TResetStmt.Create(Self, Nodes);
end;

function TMySQLParser.ParseResetStmtOption(): TOffset;
var
  Nodes: TResetStmt.TOption.TNodes;
begin
  FillChar(Nodes, SizeOf(Nodes), 0);

  if (TokenPtr(CurrentToken)^.KeywordIndex = kiMASTER) then
    Nodes.OptionTag := ParseTag(kiMASTER)
  else if (TokenPtr(CurrentToken)^.KeywordIndex = kiQUERY) then
    Nodes.OptionTag := ParseTag(kiQUERY, kiCACHE)
  else if (TokenPtr(CurrentToken)^.KeywordIndex = kiSLAVE) then
    Nodes.OptionTag := ParseTag(kiSLAVE)
  else
    SetError(PE_UnexpectedToken);

  Result := TResetStmt.TOption.Create(Self, Nodes);
end;

function TMySQLParser.ParseReturnStmt(): TOffset;
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

function TMySQLParser.ParseRollbackStmt(): TOffset;
var
  Nodes: TRollbackStmt.TNodes;
begin
  FillChar(Nodes, SizeOf(Nodes), 0);

  if (not EndOfStmt(NextToken[1]) and (TokenPtr(NextToken[1])^.KeywordIndex = kiWORK)) then
    Nodes.RollbackTag := ParseTag(kiROLLBACK, kiWORK)
  else
    Nodes.RollbackTag := ParseTag(kiROLLBACK);

  if (not Error and not EndOfStmt(CurrentToken)) then
    if (TokenPtr(CurrentToken)^.KeywordIndex = kiTO) then
      if (not EndOfStmt(NextToken[1]) and (TokenPtr(NextToken[1])^.KeywordIndex = kiSAVEPOINT)) then
        Nodes.ToValue := ParseValue(WordIndices(kiTO, kiSAVEPOINT), vaNo, ParseSavepointIdent)
      else
        Nodes.ToValue := ParseValue(kiTO, vaNo, ParseSavepointIdent)
    else
    begin
      if (TokenPtr(CurrentToken)^.KeywordIndex = kiAND) then
        if (not EndOfStmt(NextToken[1]) and (TokenPtr(NextToken[1])^.KeywordIndex = kiNO)) then
          Nodes.ChainTag := ParseTag(kiAND, kiNO, kiCHAIN)
        else
          Nodes.ChainTag := ParseTag(kiAND, kiCHAIN);

      if (not Error and not EndOfStmt(CurrentToken)) then
        if (TokenPtr(NextToken[1])^.KeywordIndex = kiNO) then
          Nodes.ReleaseTag := ParseTag(kiNO, kiRELEASE)
        else if (TokenPtr(NextToken[1])^.KeywordIndex = kiRELEASE) then
          Nodes.ReleaseTag := ParseTag(kiRELEASE);
    end;

  Result := TRollbackStmt.Create(Self, Nodes);
end;

function TMySQLParser.ParseSavepointIdent(): TOffset;
begin
  Result := 0;
  if (EndOfStmt(CurrentToken)) then
    SetError(PE_IncompleteStmt)
  else if (not (TokenPtr(CurrentToken)^.TokenType in ttIdents)) then
    SetError(PE_UnexpectedToken)
  else
    Result := ApplyCurrentToken();
end;

function TMySQLParser.ParseSavepointStmt(): TOffset;
var
  Nodes: TSavepointStmt.TNodes;
begin
  FillChar(Nodes, SizeOf(Nodes), 0);

  Nodes.SavepointTag := ParseTag(kiSAVEPOINT);

  if (not Error) then
    Nodes.Ident := ParseSavepointIdent();

  Result := TSavepointStmt.Create(Self, Nodes);
end;

function TMySQLParser.ParseSchedule(): TOffset;
var
  Nodes: TSchedule.TNodes;
begin
  FillChar(Nodes, SizeOf(Nodes), 0);

  if (EndOfStmt(CurrentToken)) then
    SetError(PE_IncompleteStmt)
  else if (TokenPtr(CurrentToken)^.KeywordIndex = kiAT) then
  begin
    Nodes.At.Tag := ParseTag(kiAT);

    if (not Error) then
      Nodes.At.Timestamp := ParseExpr();

    if (not Error and not EndOfStmt(CurrentToken) and (TokenPtr(CurrentToken)^.OperatorType = otPlus)) then
      Nodes.At.IntervalList := ParseIntervalOpList();
  end
  else if (TokenPtr(CurrentToken)^.KeywordIndex = kiEVERY) then
  begin
    Nodes.Every.Tag := ParseTag(kiEVERY);

    if (not Error) then
      if (EndOfStmt(CurrentToken)) then
        SetError(PE_IncompleteStmt)
      else
        Nodes.Every.Interval := ParseIntervalOp();

    if (not Error and not EndOfStmt(CurrentToken) and (TokenPtr(CurrentToken)^.KeywordIndex = kiSTARTS)) then
    begin
      Nodes.Starts.Tag := ParseTag(kiSTARTS);

      if (not Error) then
        Nodes.Starts.Timestamp := ParseExpr();

      if (not Error and not EndOfStmt(CurrentToken) and (TokenPtr(CurrentToken)^.OperatorType = otPlus)) then
        Nodes.Starts.IntervalList := ParseIntervalOpList();
    end;

    if (not Error and not EndOfStmt(CurrentToken) and (TokenPtr(CurrentToken)^.KeywordIndex = kiENDS)) then
    begin
      Nodes.Ends.Tag := ParseTag(kiENDS);

      if (not Error) then
        Nodes.Ends.Timestamp := ParseExpr();

      if (not Error and not EndOfStmt(CurrentToken) and (TokenPtr(CurrentToken)^.OperatorType = otPlus)) then
        Nodes.Ends.IntervalList := ParseIntervalOpList();
    end;
  end;

  Result := TSchedule.Create(Self, Nodes);
end;

function TMySQLParser.ParseSecretIdent(): TOffset;
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

function TMySQLParser.ParseSelectStmt(): TOffset;

  function ParseInto(): TSelectStmt.TIntoNodes;
  begin
    FillChar(Result, SizeOf(Result), 0);

    if (not EndOfStmt(NextToken[1]) and (TokenPtr(NextToken[1])^.KeywordIndex = kiOUTFILE)) then
    begin
      Result.Tag := ParseTag(kiINTO, kiOUTFILE);

      if (not Error) then
        Result.Filename := ParseString();

      if (not Error and not EndOfStmt(CurrentToken) and (TokenPtr(CurrentToken)^.KeywordIndex = kiCHARACTER)) then
        Result.CharacterSetValue := ParseValue(WordIndices(kiCHARACTER, kiSET), vaNo, ParseIdent);

      if (not Error and not EndOfStmt(CurrentToken) and (TokenPtr(CurrentToken)^.KeywordIndex = kiFIELDS)) then
        Result.FieldsTerminatedByValue := ParseValue(WordIndices(kiFIELDS, kiTERMINATED, kiBY), vaNo, ParseString);

      if (not Error and not EndOfStmt(CurrentToken) and (TokenPtr(CurrentToken)^.KeywordIndex = kiOPTIONALLY)) then
        Result.OptionallyEnclosedByValue := ParseValue(WordIndices(kiOPTIONALLY, kiENCLOSED, kiBY), vaNo, ParseString);

      if (not Error and not EndOfStmt(CurrentToken) and (TokenPtr(CurrentToken)^.KeywordIndex = kiLINES)) then
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
        Result.VariableList := ParseList(False, ParseVariable);
    end;
  end;

var
  Found: Boolean;
  Nodes: TSelectStmt.TNodes;
begin
  FillChar(Nodes, SizeOf(Nodes), 0);

  Nodes.SelectTag := ParseTag(kiSELECT);

  Found := True;
  while (not Error and Found and not EndOfStmt(CurrentToken)) do
    if ((Nodes.DistinctTag = 0) and (TokenPtr(CurrentToken)^.KeywordIndex = kiALL)) then
      Nodes.DistinctTag := ParseTag(kiALL)
    else if ((Nodes.DistinctTag = 0) and (TokenPtr(CurrentToken)^.KeywordIndex = kiDISTINCT)) then
      Nodes.DistinctTag := ParseTag(kiDISTINCT)
    else if ((Nodes.DistinctTag = 0) and (TokenPtr(CurrentToken)^.KeywordIndex = kiDISTINCTROW)) then
      Nodes.DistinctTag := ParseTag(kiDISTINCTROW)
    else if ((Nodes.HighPriorityTag = 0) and (TokenPtr(CurrentToken)^.KeywordIndex = kiHIGH_PRIORITY)) then
      Nodes.HighPriorityTag := ParseTag(kiHIGH_PRIORITY)
    else if ((Nodes.MaxStatementTime = 0) and (TokenPtr(CurrentToken)^.KeywordIndex = kiMAX_STATEMENT_TIME)) then
      Nodes.MaxStatementTime := ParseValue(kiMAX_STATEMENT_TIME, vaYes, ParseInteger)
    else if ((Nodes.StraightJoinTag = 0) and (TokenPtr(CurrentToken)^.KeywordIndex = kiSTRAIGHT_JOIN)) then
      Nodes.StraightJoinTag := ParseTag(kiSTRAIGHT_JOIN)
    else if ((Nodes.SQLSmallResultTag = 0) and (TokenPtr(CurrentToken)^.KeywordIndex = kiSQL_SMALL_RESULT)) then
      Nodes.SQLSmallResultTag := ParseTag(kiSQL_SMALL_RESULT)
    else if ((Nodes.SQLBigResultTag = 0) and (TokenPtr(CurrentToken)^.KeywordIndex = kiSQL_BIG_RESULT)) then
      Nodes.SQLBigResultTag := ParseTag(kiSQL_BIG_RESULT)
    else if ((Nodes.SQLBufferResultTag = 0) and (TokenPtr(CurrentToken)^.KeywordIndex = kiSQL_BUFFER_RESULT)) then
      Nodes.SQLBufferResultTag := ParseTag(kiSQL_BUFFER_RESULT)
    else if ((Nodes.SQLNoCacheTag = 0) and (TokenPtr(CurrentToken)^.KeywordIndex = kiSQL_CACHE)) then
      Nodes.SQLNoCacheTag := ParseTag(kiSQL_CACHE)
    else if ((Nodes.SQLNoCacheTag = 0) and (TokenPtr(CurrentToken)^.KeywordIndex = kiSQL_NO_CACHE)) then
      Nodes.SQLNoCacheTag := ParseTag(kiSQL_NO_CACHE)
    else if ((Nodes.SQLCalcFoundRowsTag = 0) and (TokenPtr(CurrentToken)^.KeywordIndex = kiSQL_CALC_FOUND_ROWS)) then
      Nodes.SQLCalcFoundRowsTag := ParseTag(kiSQL_CALC_FOUND_ROWS)
    else
      Found := False;

  if (not Error) then
    Nodes.ColumnsList := ParseList(False, ParseSelectStmtColumn);

  if (not Error and not EndOfStmt(CurrentToken) and (TokenPtr(CurrentToken)^.KeywordIndex = kiINTO)) then
    Nodes.Into1 := ParseInto();

  if (not Error and not EndOfStmt(CurrentToken) and (TokenPtr(CurrentToken)^.KeywordIndex = kiFROM)) then
  begin
    Nodes.From.Tag := ParseTag(kiFROM);
    if (not Error) then
      Nodes.From.TableReferenceList := ParseList(False, ParseSelectStmtTableEscapedReference);

    if (not Error and not EndOfStmt(CurrentToken) and (TokenPtr(CurrentToken)^.KeywordIndex = kiPARTITION)) then
    begin
      Nodes.Partition.Tag := ParseTag(kiPARTITION);

      if (not Error) then
        Nodes.Partition.Ident := ParsePartitionIdent();
    end;

    if (not Error and not EndOfStmt(CurrentToken) and (TokenPtr(CurrentToken)^.KeywordIndex = kiWHERE)) then
    begin
      Nodes.Where.Tag := ParseTag(kiWHERE);

      if (not Error) then
        Nodes.Where.Expr := ParseExpr();
    end;

    if (not Error and not EndOfStmt(CurrentToken) and (TokenPtr(CurrentToken)^.KeywordIndex = kiGROUP)) then
    begin
      Nodes.GroupBy.Tag := ParseTag(kiGROUP, kiBY);

      if (not Error) then
        Nodes.GroupBy.List := ParseList(False, ParseSelectStmtGroup);

      if (not Error and not EndOfStmt(CurrentToken) and (TokenPtr(CurrentToken)^.KeywordIndex = kiWITH)) then
        Nodes.GroupBy.WithRollupTag := ParseTag(kiWITH, kiROLLUP);
    end;

    if (not Error and not EndOfStmt(CurrentToken) and (TokenPtr(CurrentToken)^.KeywordIndex = kiHAVING)) then
    begin
      Nodes.Having.Tag := ParseTag(kiHAVING);

      if (not Error) then
        Nodes.Having.Expr := ParseExpr();
    end;

    if (not Error and not EndOfStmt(CurrentToken) and (TokenPtr(CurrentToken)^.KeywordIndex = kiORDER)) then
    begin
      Nodes.OrderBy.Tag := ParseTag(kiORDER, kiBY);

      if (not Error) then
        Nodes.OrderBy.Expr := ParseList(False, ParseSelectStmtOrder);
    end;

    if (not Error and not EndOfStmt(CurrentToken) and (TokenPtr(CurrentToken)^.KeywordIndex = kiLIMIT)) then
    begin
      Nodes.Limit.Tag := ParseTag(kiLIMIT);

      if (not Error) then
        Nodes.Limit.RowCountToken := ParseExpr();

      if (not Error and not EndOfStmt(CurrentToken) and (TokenPtr(CurrentToken)^.TokenType = ttComma)) then
      begin
        Nodes.Limit.CommaToken := ApplyCurrentToken();

        if (EndOfStmt(CurrentToken)) then
          SetError(PE_IncompleteStmt)
        else
        begin
          Nodes.Limit.OffsetToken := Nodes.Limit.RowCountToken;
          Nodes.Limit.RowCountToken := ParseExpr();
        end;
      end
      else if (not EndOfStmt(CurrentToken) and (TokenPtr(CurrentToken)^.KeywordIndex = kiOFFSET)) then
      begin
        Nodes.Limit.OffsetTag := ParseTag(kiOFFSET);

        if (EndOfStmt(CurrentToken)) then
          SetError(PE_IncompleteStmt)
        else
          Nodes.Limit.OffsetToken := ParseInteger();
      end;
    end;

    if (not Error and not EndOfStmt(CurrentToken) and (TokenPtr(CurrentToken)^.KeywordIndex = kiPROCEDURE)) then
    begin
      Nodes.Proc.Tag := ParseTag(kiPROCEDURE);

      if (not Error) then
        Nodes.Proc.Ident := ParseProcedureIdent();

      if (not Error) then
        Nodes.Proc.ParamList := ParseList(True, ParseExpr);
    end;

    if (not Error and not EndOfStmt(CurrentToken) and (TokenPtr(CurrentToken)^.KeywordIndex = kiINTO)) then
      if (Nodes.Into1.Tag > 0) then
        SetError(PE_UnexpectedToken)
      else
        Nodes.Into2 := ParseInto();

    if (not Error and not EndOfStmt(CurrentToken) and (Nodes.From.Tag > 0)) then
      if (TokenPtr(CurrentToken)^.KeywordIndex = kiFOR) then
        Nodes.ForUpdatesTag := ParseTag(kiFOR, kiUPDATE)
      else if (TokenPtr(CurrentToken)^.KeywordIndex = kiLOCK) then
        Nodes.LockInShareMode := ParseTag(kiLOCK, kiIN, kiSHARE, kiMODE);
  end;

  if (not Error and not EndOfStmt(CurrentToken) and (TokenPtr(CurrentToken)^.KeywordIndex = kiUNION)) then
  begin
    if (not Error and not EndOfStmt(NextToken[1]) and ((TokenPtr(NextToken[1])^.KeywordIndex = kiALL) or (TokenPtr(NextToken[1])^.KeywordIndex = kiDISTINCT))) then
      Nodes.Union.Tag := ParseTag(kiUNION, TokenPtr(NextToken[1])^.KeywordIndex)
    else
      Nodes.Union.Tag := ParseTag(kiUNION);

    if (not Error) then
      Nodes.Union.SelectStmt := ParseSubAreaSelectStmt();
  end;

  Result := TSelectStmt.Create(Self, Nodes);
end;

function TMySQLParser.ParseSelectStmtColumn(): TOffset;
var
  Nodes: TSelectStmt.TColumn.TNodes;
begin
  FillChar(Nodes, SizeOf(Nodes), 0);

  if (EndOfStmt(CurrentToken)) then
    SetError(PE_IncompleteStmt)
  else
    Nodes.Expr := ParseExpr();

  if (not Error and not EndOfStmt(CurrentToken) and (TokenPtr(CurrentToken)^.KeywordIndex = kiAS)) then
  begin
    Nodes.AsToken := ParseTag(kiAS);

    if (not Error) then
      if (EndOfStmt(CurrentToken)) then
        SetError(PE_IncompleteStmt)
      else if (not (TokenPtr(CurrentToken)^.TokenType in ttIdents + ttStrings) or (TokenPtr(CurrentToken)^.KeywordIndex >= 0)) then
        SetError(PE_UnexpectedToken)
      else
        Nodes.AliasIdent := ParseAliasIdent();
  end;

  if (not Error and not EndOfStmt(CurrentToken)
    and (Nodes.AliasIdent = 0)
    and (TokenPtr(CurrentToken)^.TokenType in ttIdents + ttStrings)
    and (TokenPtr(CurrentToken)^.KeywordIndex <> kiFROM)
    and (TokenPtr(CurrentToken)^.KeywordIndex <> kiINTO)
    and (TokenPtr(CurrentToken)^.KeywordIndex <> kiELSE)
    and (TokenPtr(CurrentToken)^.KeywordIndex <> kiELSEIF)
    and (TokenPtr(CurrentToken)^.KeywordIndex <> kiUNTIL)
    and (TokenPtr(CurrentToken)^.KeywordIndex <> kiWHEN)
    and (TokenPtr(CurrentToken)^.KeywordIndex <> kiEND)) then
    Nodes.AliasIdent := ParseAliasIdent();

  Result := TSelectStmt.TColumn.Create(Self, Nodes);
end;

function TMySQLParser.ParseSelectStmtGroup(): TOffset;
var
  Nodes: TSelectStmt.TGroup.TNodes;
begin
  FillChar(Nodes, SizeOf(Nodes), 0);

  Nodes.Expr := ParseExpr();

  if (not Error and not EndOfStmt(CurrentToken) and ((TokenPtr(CurrentToken)^.KeywordIndex = kiASC) or (TokenPtr(CurrentToken)^.KeywordIndex = kiDESC))) then
    Nodes.Direction := ParseTag(TokenPtr(CurrentToken)^.KeywordIndex);

  Result := TSelectStmt.TGroup.Create(Self, Nodes);
end;

function TMySQLParser.ParseSelectStmtOrder(): TOffset;
var
  Nodes: TSelectStmt.TOrder.TNodes;
begin
  FillChar(Nodes, SizeOf(Nodes), 0);

  Nodes.Expr := ParseExpr();

  if (not Error and not EndOfStmt(CurrentToken) and ((TokenPtr(CurrentToken)^.KeywordIndex = kiASC) or (TokenPtr(CurrentToken)^.KeywordIndex = kiDESC))) then
    Nodes.DirectionTag := ParseTag(TokenPtr(CurrentToken)^.KeywordIndex);

  Result := TSelectStmt.TOrder.Create(Self, Nodes);
end;

function TMySQLParser.ParseSelectStmtTableFactor(): TOffset;
var
  Nodes: TSelectStmt.TTableFactor.TNodes;
begin
  FillChar(Nodes, SizeOf(Nodes), 0);

  Nodes.TableIdent := ParseTableIdent();

  if (not Error and not EndOfStmt(CurrentToken) and (TokenPtr(CurrentToken)^.KeywordIndex = kiPARTITION)) then
  begin
    Nodes.PartitionTag := ParseTag(kiPARTITION);

    if (not Error) then
      Nodes.Partitions := ParseList(True, ParsePartitionIdent);
  end;

  if (not Error and not EndOfStmt(CurrentToken) and (TokenPtr(CurrentToken)^.KeywordIndex = kiAS)) then
  begin
    Nodes.AsToken := ParseTag(kiAS);

    if (not Error) then
      if (EndOfStmt(CurrentToken)) then
        SetError(PE_IncompleteStmt)
      else if (not (TokenPtr(CurrentToken)^.TokenType in ttIdents + ttStrings) or (TokenPtr(CurrentToken)^.KeywordIndex >= 0)) then
        SetError(PE_UnexpectedToken)
      else
        Nodes.AliasIdent := ParseAliasIdent();
  end;

  if (not Error and not EndOfStmt(CurrentToken)
    and (Nodes.AliasIdent = 0)
    and (TokenPtr(CurrentToken)^.TokenType in ttIdents + ttStrings) and (TokenPtr(CurrentToken)^.KeywordIndex < 0)) then
    Nodes.AliasIdent := ParseAliasIdent();

  if (not Error and not EndOfStmt(CurrentToken) and ((TokenPtr(CurrentToken)^.KeywordIndex = kiUSE) or (TokenPtr(CurrentToken)^.KeywordIndex = kiIGNORE) or (TokenPtr(CurrentToken)^.KeywordIndex = kiFORCE))) then
    Nodes.IndexHintList := ParseList(False, ParseSelectStmtTableFactorIndexHint);

  Result := TSelectStmt.TTableFactor.Create(Self, Nodes);
end;

function TMySQLParser.ParseSelectStmtTableEscapedReference(): TOffset;
begin
  if (TokenPtr(CurrentToken)^.TokenType <> ttOpenCurlyBracket) then
    Result := ParseSelectStmtTableReference()
  else
    Result := ParseSelectStmtTableFactorOj();
end;

function TMySQLParser.ParseSelectStmtTableFactorSelect(): TOffset;
var
  Nodes: TSelectStmt.TTableFactorSelect.TNodes;
begin
  FillChar(Nodes, SizeOf(Nodes), 0);

  if (not EndOfStmt(NextToken[1]) and (TokenPtr(NextToken[1])^.TokenType = ttOpenBracket)) then
    Nodes.SelectStmt := ParseSubArea(ParseSubAreaSelectStmt)
  else
    Nodes.SelectStmt := ParseSubAreaSelectStmt();

  if (not Error and not EndOfStmt(CurrentToken) and (TokenPtr(CurrentToken)^.KeywordIndex = kiAS)) then
  begin
    Nodes.AsToken := ParseTag(kiAS);

    if (not Error) then
      if (EndOfStmt(CurrentToken)) then
        SetError(PE_IncompleteStmt)
      else if (not (TokenPtr(CurrentToken)^.TokenType in ttIdents + ttStrings) or (TokenPtr(CurrentToken)^.KeywordIndex >= 0)) then
        SetError(PE_UnexpectedToken)
      else
        Nodes.AliasIdent := ParseAliasIdent();
  end;

  if (not Error and not EndOfStmt(CurrentToken)
    and (Nodes.AliasIdent = 0)
    and (TokenPtr(CurrentToken)^.TokenType in ttIdents + ttStrings) and (TokenPtr(CurrentToken)^.KeywordIndex < 0)) then
    Nodes.AliasIdent := ParseAliasIdent();

  Result := TSelectStmt.TTableFactorSelect.Create(Self, Nodes);
end;

function TMySQLParser.ParseSelectStmtTableFactorIndexHint(): TOffset;
var
  Nodes: TSelectStmt.TTableFactor.TIndexHint.TNodes;
  ValueNodes: TValue.TNodes;
begin
  FillChar(Nodes, SizeOf(Nodes), 0);

  if (EndOfStmt(NextToken[1])) then
    SetError(PE_IncompleteStmt)
  else if ((TokenPtr(CurrentToken)^.KeywordIndex <> kiUSE) and (TokenPtr(CurrentToken)^.KeywordIndex <> kiIGNORE) and (TokenPtr(CurrentToken)^.KeywordIndex <> kiFORCE)) then
    SetError(PE_UnexpectedToken)
  else if ((TokenPtr(NextToken[1])^.KeywordIndex <> kiINDEX) and (TokenPtr(NextToken[1])^.KeywordIndex <> kiKEY)) then
    SetError(PE_UnexpectedToken, NextToken[1])
  else
    Nodes.KindTag := ParseTag(TokenPtr(CurrentToken)^.KeywordIndex, TokenPtr(NextToken[1])^.KeywordIndex);

  if (not Error and not EndOfStmt(CurrentToken) and (TokenPtr(CurrentToken)^.KeywordIndex = kiFOR)) then
  begin
    FillChar(ValueNodes, SizeOf(ValueNodes), 0);

    ValueNodes.IdentTag := ParseTag(kiFOR);

    if (EndOfStmt(CurrentToken)) then
      SetError(PE_IncompleteStmt)
    else if (TokenPtr(CurrentToken)^.KeywordIndex = kiJOIN) then
      ValueNodes.Expr := ParseTag(kiJOIN)
    else if (TokenPtr(CurrentToken)^.KeywordIndex = kiORDER) then
      ValueNodes.Expr := ParseTag(kiORDER, kiBY)
    else if (TokenPtr(CurrentToken)^.KeywordIndex = kiGROUP) then
      ValueNodes.Expr := ParseTag(kiGROUP, kiBY)
    else
      SetError(PE_UnexpectedToken);

    Nodes.ForValue := TValue.Create(Self, ValueNodes);
  end;

  if (not Error) then
    if (EndOfStmt(CurrentToken)) then
      SetError(PE_IncompleteStmt)
    else
      Nodes.IndexList := ParseList(True, ParseKeyIdent);

  Result := TSelectStmt.TTableFactor.TIndexHint.Create(Self, Nodes);
end;

function TMySQLParser.ParseSelectStmtTableFactorOj(): TOffset;
var
  Nodes: TSelectStmt.TTableFactorOj.TNodes;
begin
  FillChar(Nodes, SizeOf(Nodes), 0);

  Nodes.OpenBracket := ApplyCurrentToken();

  if (not Error) then
    Nodes.OjTag := ParseTag(kiOJ);

  if (not Error) then
    if (EndOfStmt(CurrentToken)) then
      SetError(PE_IncompleteStmt)
    else
      Nodes.TableReference := ParseSelectStmtTableReference();

  if (not Error) then
    if (EndOfStmt(CurrentToken)) then
      SetError(PE_IncompleteStmt)
    else if (TokenPtr(CurrentToken)^.TokenType <> ttCloseBracket) then
      SetError(PE_UnexpectedToken)
    else
      Nodes.CloseBracket := ApplyCurrentToken();

  Result := TSelectStmt.TTableFactorOj.Create(Self, Nodes);
end;

function TMySQLParser.ParseSelectStmtTableReference(): TOffset;

  function ParseTableFactor(): TOffset;
  begin
    if (TokenPtr(CurrentToken)^.TokenType in ttIdents) then
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
  Children: array[0 .. 19] of TOffset;
  JoinNodes: TSelectStmt.TTableJoin.TNodes;
  JoinType: TJoinType;
  Nodes: TList.TNodes;
begin
  FillChar(Nodes, SizeOf(Nodes), 0);

  ChildrenCount := 1;
  Children[0] := ParseTableFactor();

  while (not Error and not EndOfStmt(CurrentToken)
    and ((TokenPtr(CurrentToken)^.KeywordIndex = kiINNER)
      or (TokenPtr(CurrentToken)^.KeywordIndex = kiCROSS)
      or (TokenPtr(CurrentToken)^.KeywordIndex = kiJOIN)
      or (TokenPtr(CurrentToken)^.KeywordIndex = kiSTRAIGHT_JOIN)
      or (TokenPtr(CurrentToken)^.KeywordIndex = kiLEFT)
      or (TokenPtr(CurrentToken)^.KeywordIndex = kiRIGHT)
      or (TokenPtr(CurrentToken)^.KeywordIndex = kiNATURAL))) do
  begin
    if (ChildrenCount = Length(Children)) then
      raise Exception.Create(SArgumentOutOfRange);

    FillChar(JoinNodes, SizeOf(JoinNodes), 0);

    JoinType := jtUnknown;
    if (not Error) then
      if (EndOfStmt(CurrentToken)) then
        SetError(PE_IncompleteStmt)
      else if ((TokenPtr(CurrentToken)^.KeywordIndex = kiINNER)
        and not EndOfStmt(NextToken[1]) and (TokenPtr(NextToken[1])^.KeywordIndex = kiJOIN)) then
      begin
        JoinNodes.JoinTag := ParseTag(kiINNER, kiJOIN);
        JoinType := jtInner
      end
      else if ((TokenPtr(CurrentToken)^.KeywordIndex = kiCROSS)
        and not EndOfStmt(NextToken[1]) and (TokenPtr(NextToken[1])^.KeywordIndex = kiJOIN)) then
      begin
        JoinNodes.JoinTag := ParseTag(kiCROSS, kiJOIN);
        JoinType := jtCross;
      end
      else if (TokenPtr(CurrentToken)^.KeywordIndex = kiJOIN) then
      begin
        JoinNodes.JoinTag := ParseTag(kiJOIN);
        JoinType := jtInner;
      end
      else if (TokenPtr(CurrentToken)^.KeywordIndex = kiSTRAIGHT_JOIN) then
      begin
        JoinNodes.JoinTag := ParseTag(kiSTRAIGHT_JOIN);
        JoinType := jtCross;
      end
      else if ((TokenPtr(CurrentToken)^.KeywordIndex = kiLEFT)
        and not EndOfStmt(NextToken[1]) and (TokenPtr(NextToken[1])^.KeywordIndex = kiJOIN)) then
      begin
        JoinNodes.JoinTag := ParseTag(kiLEFT, kiJOIN);
        JoinType := jtLeft;
      end
      else if ((TokenPtr(CurrentToken)^.KeywordIndex = kiLEFT)
        and not EndOfStmt(NextToken[1]) and (TokenPtr(NextToken[1])^.KeywordIndex = kiOUTER)
        and not EndOfStmt(NextToken[2]) and (TokenPtr(NextToken[2])^.KeywordIndex = kiJOIN)) then
      begin
        JoinNodes.JoinTag := ParseTag(kiLEFT, kiOUTER, kiJOIN);
        JoinType := jtLeft;
      end
      else if ((TokenPtr(CurrentToken)^.KeywordIndex = kiRIGHT)
        and not EndOfStmt(NextToken[1]) and (TokenPtr(NextToken[1])^.KeywordIndex = kiJOIN)) then
      begin
        JoinNodes.JoinTag := ParseTag(kiRIGHT, kiJOIN);
        JoinType := jtRight;
      end
      else if ((TokenPtr(CurrentToken)^.KeywordIndex = kiRIGHT)
        and not EndOfStmt(NextToken[1]) and (TokenPtr(NextToken[1])^.KeywordIndex = kiOUTER)
        and not EndOfStmt(NextToken[2]) and (TokenPtr(NextToken[2])^.KeywordIndex = kiJOIN)) then
      begin
        JoinNodes.JoinTag := ParseTag(kiRIGHT, kiOUTER, kiJOIN);
        JoinType := jtRight;
      end
      else if ((TokenPtr(CurrentToken)^.KeywordIndex = kiNATURAL)
        and not EndOfStmt(NextToken[1]) and (TokenPtr(NextToken[1])^.KeywordIndex = kiJOIN)) then
      begin
        JoinNodes.JoinTag := ParseTag(kiNATURAL, kiJOIN);
        JoinType := jtEqui;
      end
      else if ((TokenPtr(CurrentToken)^.KeywordIndex = kiNATURAL)
        and not EndOfStmt(NextToken[1]) and (TokenPtr(NextToken[1])^.KeywordIndex = kiLEFT)
        and not EndOfStmt(NextToken[2]) and (TokenPtr(NextToken[2])^.KeywordIndex = kiJOIN)) then
      begin
        JoinNodes.JoinTag := ParseTag(kiNATURAL, kiLEFT, kiJOIN);
        JoinType := jtNaturalLeft;
      end
      else if ((TokenPtr(CurrentToken)^.KeywordIndex = kiNATURAL)
        and not EndOfStmt(NextToken[1]) and (TokenPtr(NextToken[1])^.KeywordIndex = kiLEFT)
        and not EndOfStmt(NextToken[2]) and (TokenPtr(NextToken[2])^.KeywordIndex = kiOUTER)
        and not EndOfStmt(NextToken[3]) and (TokenPtr(NextToken[3])^.KeywordIndex = kiJOIN)) then
      begin
        JoinNodes.JoinTag := ParseTag(kiNATURAL, kiLEFT, kiOUTER, kiJOIN);
        JoinType := jtNaturalLeft;
      end
      else if ((TokenPtr(CurrentToken)^.KeywordIndex = kiNATURAL)
        and not EndOfStmt(NextToken[1]) and (TokenPtr(NextToken[1])^.KeywordIndex = kiRIGHT)
        and not EndOfStmt(NextToken[2]) and (TokenPtr(NextToken[2])^.KeywordIndex = kiJOIN)) then
      begin
        JoinNodes.JoinTag := ParseTag(kiNATURAL, kiRIGHT, kiJOIN);
        JoinType := jtNaturalRight;
      end
      else if ((TokenPtr(CurrentToken)^.KeywordIndex = kiNATURAL)
        and not EndOfStmt(NextToken[1]) and (TokenPtr(NextToken[1])^.KeywordIndex = kiRIGHT)
        and not EndOfStmt(NextToken[2]) and (TokenPtr(NextToken[2])^.KeywordIndex = kiOUTER)
        and not EndOfStmt(NextToken[3]) and (TokenPtr(NextToken[3])^.KeywordIndex = kiJOIN)) then
      begin
        JoinNodes.JoinTag := ParseTag(kiNATURAL, kiRIGHT, kiOUTER, kiJOIN);
        JoinType := jtNaturalRight;
      end
      else
        raise Exception.Create(SUnknownError);

    if (not Error) then
      if (EndOfStmt(CurrentToken)) then
        SetError(PE_IncompleteStmt)
      else
      begin
        if (not (JoinType in [jtLeft, jtRight])) then
        begin
          JoinNodes.RightTable := ParseTableFactor();

          if (not Error and not EndOfStmt(CurrentToken)) then
            if ((JoinType in [jtStraight]) and (TokenPtr(CurrentToken)^.KeywordIndex = kiON)) then
            begin
              JoinNodes.OnTag := ParseTag(kiON);
              if (not Error) then
                JoinNodes.Condition := ParseExpr();
            end
            else if (JoinType in [jtInner, jtCross]) then
              if (TokenPtr(CurrentToken)^.KeywordIndex = kiON) then
              begin
                JoinNodes.OnTag := ParseTag(kiON);
                if (not Error) then
                  JoinNodes.Condition := ParseExpr();
              end
              else if (TokenPtr(CurrentToken)^.KeywordIndex = kiUSING) then
              begin
                JoinNodes.OnTag := ParseTag(kiUSING);
                if (not Error) then
                  JoinNodes.Condition := ParseList(True, ParseFieldIdent);
              end;
        end
        else
        begin
          JoinNodes.RightTable := ParseSelectStmtTableReference();

          if (not Error and not EndOfStmt(CurrentToken) and not (JoinType in [jtNaturalLeft, jtNaturalRight])) then
            if (TokenPtr(CurrentToken)^.KeywordIndex = kiON) then
            begin
              JoinNodes.OnTag := ParseTag(kiON);
              if (not Error) then
                if (EndOfStmt(CurrentToken)) then
                  SetError(PE_IncompleteStmt)
                else
                  JoinNodes.Condition := ParseExpr();
            end
            else if (TokenPtr(CurrentToken)^.KeywordIndex = kiUSING) then
            begin
              JoinNodes.OnTag := ParseTag(kiUSING);

              if (not Error) then
                JoinNodes.Condition := ParseList(True, ParseFieldIdent);
            end;
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

function TMySQLParser.ParseSetNamesStmt(): TOffset;
var
  Nodes: TSetNamesStmt.TNodes;
begin
  FillChar(Nodes, SizeOf(Nodes), 0);

  if (TokenPtr(NextToken[1])^.KeywordIndex = kiNAMES) then
    Nodes.StmtTag := ParseTag(kiSET, kiNAMES)
  else if (not EndOfStmt(NextToken[1]) and (TokenPtr(NextToken[1])^.KeywordIndex = kiCHARACTER)) then
    Nodes.StmtTag := ParseTag(kiSET, kiCHARACTER, kiSET)
  else
    raise Exception.Create(SUnknownError);

  if (not Error) then
    if (EndOfStmt(CurrentToken)) then
      SetError(PE_IncompleteStmt)
    else if (TokenPtr(CurrentToken)^.TokenType <> ttIdent) then
      SetError(PE_UnexpectedToken)
    else
      Nodes.ConstValue := ApplyCurrentToken();

  Result := TSetNamesStmt.Create(Self, Nodes);
end;

function TMySQLParser.ParseSetPasswordStmt(): TOffset;
var
  Nodes: TSetPasswordStmt.TNodes;
begin
  FillChar(Nodes, SizeOf(Nodes), 0);

  Nodes.StmtTag := ParseTag(kiSET, kiPASSWORD);

  if (not Error and not EndOfStmt(CurrentToken) and (TokenPtr(CurrentToken)^.KeywordIndex = kiFOR)) then
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

function TMySQLParser.ParseSetStmt(): TOffset;
var
  Nodes: TSetStmt.TNodes;
begin
  FillChar(Nodes, SizeOf(Nodes), 0);

  Nodes.SetTag := ParseTag(kiSET);

  if (not Error and not EndOfStmt(CurrentToken)
    and ((TokenPtr(CurrentToken)^.KeywordIndex = kiGLOBAL) or (TokenPtr(CurrentToken)^.KeywordIndex = kiSESSION))) then
    Nodes.ScopeTag := ParseTag(TokenPtr(CurrentToken)^.KeywordIndex);

  if (not Error) then
    Nodes.AssignmentList := ParseList(False, ParseSetStmtAssignment);

  Result := TSetStmt.Create(Self, Nodes);
end;

function TMySQLParser.ParseSetStmtAssignment(): TOffset;
var
  Nodes: TSetStmt.TAssignment.TNodes;
begin
  FillChar(Nodes, SizeOf(Nodes), 0);

  if (not Error and not EndOfStmt(CurrentToken)) then
    if (TokenPtr(CurrentToken)^.KeywordIndex = kiGLOBAL) then
      Nodes.ScopeTag := ParseTag(kiGLOBAL)
    else if (TokenPtr(CurrentToken)^.KeywordIndex = kiSESSION) then
      Nodes.ScopeTag := ParseTag(kiSESSION);

  if (not Error) then
    if (EndOfStmt(CurrentToken)) then
      SetError(PE_IncompleteStmt)
    else
      Nodes.Variable := ParseVariable();

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

function TMySQLParser.ParseSetTransactionStmt(): TOffset;
var
  Nodes: TSetTransactionStmt.TNodes;
begin
  FillChar(Nodes, SizeOf(Nodes), 0);

  Nodes.SetTag := ParseTag(kiSET);

  if (not Error and not EndOfStmt(CurrentToken)) then
    if (TokenPtr(CurrentToken)^.KeywordIndex = kiGLOBAL) then
      Nodes.ScopeTag := ParseTag(kiGLOBAL)
    else if (TokenPtr(CurrentToken)^.KeywordIndex = kiSESSION) then
      Nodes.ScopeTag := ParseTag(kiSESSION);

  if (not Error) then
    Nodes.TransactionTag := ParseTag(kiTRANSACTION);

  if (not Error) then
    Nodes.CharacteristicList := ParseList(False, ParseSetTransactionStmtCharacterisic);

  Result := TSetTransactionStmt.Create(Self, Nodes);
end;

function TMySQLParser.ParseTriggerIdent(): TOffset;
begin
  Result := ParseDbIdent(ditTrigger);
end;

function TMySQLParser.ParseSetTransactionStmtCharacterisic(): TOffset;
var
  Nodes: TSetTransactionStmt.TCharacteristic.TNodes;
begin
  FillChar(Nodes, SizeOf(Nodes), 0);

  if (EndOfStmt(CurrentToken)) then
    SetError(PE_IncompleteStmt)
  else if (TokenPtr(CurrentToken)^.KeywordIndex = kiISOLATION) then
  begin
    Nodes.KindTag := ParseTag(kiISOLATION, kiLEVEL);

    if (not Error) then
      if (EndOfStmt(CurrentToken)) then
        SetError(PE_IncompleteStmt)
      else if (TokenPtr(CurrentToken)^.KeywordIndex = kiREPEATABLE) then
        Nodes.Value := ParseTag(kiREPEATABLE, kiREAD)
      else if (TokenPtr(CurrentToken)^.KeywordIndex = kiREAD) then
        if (EndOfStmt(NextToken[1])) then
          SetError(PE_IncompleteStmt)
        else if (TokenPtr(NextToken[1])^.KeywordIndex = kiCOMMITTED) then
          Nodes.Value := ParseTag(kiREAD, kiCOMMITTED)
        else if (TokenPtr(NextToken[1])^.KeywordIndex = kiUNCOMMITTED) then
          Nodes.Value := ParseTag(kiREAD, kiUNCOMMITTED)
        else
          SetError(PE_UnexpectedToken)
      else if (TokenPtr(CurrentToken)^.KeywordIndex = kiSERIALIZABLE) then
        Nodes.Value := ParseTag(kiSERIALIZABLE)
      else
        SetError(PE_UnexpectedToken);
  end
  else if (TokenPtr(CurrentToken)^.KeywordIndex = kiREAD) then
    if (EndOfStmt(CurrentToken)) then
      SetError(PE_IncompleteStmt)
    else if (TokenPtr(NextToken[1])^.KeywordIndex = kiWRITE) then
      Nodes.KindTag := ParseTag(kiREAD, kiWRITE)
    else if (TokenPtr(NextToken[1])^.KeywordIndex = kiONLY) then
      Nodes.KindTag := ParseTag(kiREAD, kiONLY)
    else
      SetError(PE_UnexpectedToken)
  else
   SetError(PE_UnexpectedToken);

  Result := TSetTransactionStmt.TCharacteristic.Create(Self, Nodes);
end;

function TMySQLParser.ParseShowAuthorsStmt(): TOffset;
var
  Nodes: TShowAuthorsStmt.TNodes;
begin
  FillChar(Nodes, SizeOf(Nodes), 0);

  Nodes.StmtTag := ParseTag(kiSHOW, kiAUTHORS);

  Result := TShowAuthorsStmt.Create(Self, Nodes);
end;

function TMySQLParser.ParseShowBinaryLogsStmt(): TOffset;
var
  Nodes: TShowBinaryLogsStmt.TNodes;
begin
  FillChar(Nodes, SizeOf(Nodes), 0);

  Nodes.StmtTag := ParseTag(kiSHOW, kiBINARY, kiLOGS);

  Result := TShowBinaryLogsStmt.Create(Self, Nodes);
end;

function TMySQLParser.ParseShowBinlogEventsStmt(): TOffset;
var
  Nodes: TShowBinlogEventsStmt.TNodes;
begin
  FillChar(Nodes, SizeOf(Nodes), 0);

  Nodes.StmtTag := ParseTag(kiSHOW, kiBINLOG, kiEVENTS);

  if (not Error and not EndOfStmt(CurrentToken) and (TokenPtr(CurrentToken)^.KeywordIndex = kiIN)) then
    Nodes.InValue := ParseValue(kiIN, vaNo, ParseString);

  if (not Error and not EndOfStmt(CurrentToken) and (TokenPtr(CurrentToken)^.KeywordIndex = kiFROM)) then
    Nodes.FromValue := ParseValue(kiFROM, vaNo, ParseInteger);

  if (not Error and not EndOfStmt(CurrentToken) and (TokenPtr(CurrentToken)^.KeywordIndex = kiLIMIT)) then
  begin
    Nodes.Limit.Tag := ParseTag(kiLIMIT);

    if (EndOfStmt(CurrentToken)) then
      SetError(PE_IncompleteStmt)
    else
    begin
      Nodes.Limit.RowCountToken := ParseInteger();

      if (not Error and not EndOfStmt(CurrentToken) and (TokenPtr(CurrentToken)^.TokenType = ttComma)) then
      begin
        Nodes.Limit.CommaToken := ApplyCurrentToken();

        if (EndOfStmt(CurrentToken)) then
          SetError(PE_IncompleteStmt)
        else
        begin
          Nodes.Limit.OffsetToken := Nodes.Limit.RowCountToken;
          Nodes.Limit.RowCountToken := ParseInteger();
        end;
      end;
    end;
  end;

  Result := TShowBinlogEventsStmt.Create(Self, Nodes);
end;

function TMySQLParser.ParseShowCharacterSetStmt(): TOffset;
var
  Nodes: TShowCharacterSetStmt.TNodes;
begin
  FillChar(Nodes, SizeOf(Nodes), 0);

  Nodes.StmtTag := ParseTag(kiSHOW, kiCHARACTER, kiSET);

  if (not Error and not EndOfStmt(CurrentToken)) then
    if (TokenPtr(CurrentToken)^.KeywordIndex = kiLIKE) then
      Nodes.LikeValue := ParseValue(kiLIKE, vaNo, ParseString)
    else if (TokenPtr(CurrentToken)^.KeywordIndex = kiWHERE) then
      Nodes.WhereValue := ParseValue(kiWHERE, vaNo, ParseExpr);

  Result := TShowCharacterSetStmt.Create(Self, Nodes);
end;

function TMySQLParser.ParseShowCollationStmt(): TOffset;
var
  Nodes: TShowCollationStmt.TNodes;
begin
  FillChar(Nodes, SizeOf(Nodes), 0);

  Nodes.StmtTag := ParseTag(kiSHOW, kiCOLLATION);

  if (not Error and not EndOfStmt(CurrentToken)) then
    if (TokenPtr(CurrentToken)^.KeywordIndex = kiLIKE) then
      Nodes.LikeValue := ParseValue(kiLIKE, vaNo, ParseString)
    else if (TokenPtr(CurrentToken)^.KeywordIndex = kiWHERE) then
      Nodes.WhereValue := ParseValue(kiWHERE, vaNo, ParseExpr);

  Result := TShowCollationStmt.Create(Self, Nodes);
end;

function TMySQLParser.ParseShowContributorsStmt(): TOffset;
var
  Nodes: TShowContributorsStmt.TNodes;
begin
  FillChar(Nodes, SizeOf(Nodes), 0);

  Nodes.StmtTag := ParseTag(kiSHOW, kiCONTRIBUTORS);

  Result := TShowContributorsStmt.Create(Self, Nodes);
end;

function TMySQLParser.ParseShowCountErrorsStmt(): TOffset;
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

function TMySQLParser.ParseShowCountWarningsStmt(): TOffset;
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

function TMySQLParser.ParseShowCreateDatabaseStmt(): TOffset;
var
  Nodes: TShowCreateDatabaseStmt.TNodes;
begin
  FillChar(Nodes, SizeOf(Nodes), 0);

  if (TokenPtr(NextToken[2])^.KeywordIndex = kiSCHEMA) then
    Nodes.StmtTag := ParseTag(kiSHOW, kiCREATE, kiSCHEMA)
  else
    Nodes.StmtTag := ParseTag(kiSHOW, kiCREATE, kiDATABASE);

  if (not Error and not EndOfStmt(NextToken[2])
    and (TokenPtr(CurrentToken)^.KeywordIndex = kiIF)
    and (TokenPtr(NextToken[1])^.KeywordIndex = kiNOT)
    and (TokenPtr(NextToken[2])^.KeywordIndex = kiEXISTS)) then
    Nodes.IfNotExistsTag := ParseTag(kiIF, kiNOT, kiEXISTS);

  if (not Error) then
    Nodes.Ident := ParseDatabaseIdent();

  Result := TShowCreateDatabaseStmt.Create(Self, Nodes);
end;

function TMySQLParser.ParseShowCreateEventStmt(): TOffset;
var
  Nodes: TShowCreateEventStmt.TNodes;
begin
  FillChar(Nodes, SizeOf(Nodes), 0);

  Nodes.StmtTag := ParseTag(kiSHOW, kiCREATE, kiEVENT);

  if (not Error) then
    Nodes.Ident := ParseDbIdent(ditEvent);

  Result := TShowCreateEventStmt.Create(Self, Nodes);
end;

function TMySQLParser.ParseShowCreateFunctionStmt(): TOffset;
var
  Nodes: TShowCreateFunctionStmt.TNodes;
begin
  FillChar(Nodes, SizeOf(Nodes), 0);

  Nodes.StmtTag := ParseTag(kiSHOW, kiCREATE, kiFUNCTION);

  if (not Error) then
    Nodes.Ident := ParseDbIdent(ditFunction);

  Result := TShowCreateFunctionStmt.Create(Self, Nodes);
end;

function TMySQLParser.ParseShowCreateProcedureStmt(): TOffset;
var
  Nodes: TShowCreateProcedureStmt.TNodes;
begin
  FillChar(Nodes, SizeOf(Nodes), 0);

  Nodes.StmtTag := ParseTag(kiSHOW, kiCREATE, kiPROCEDURE);

  if (not Error) then
    Nodes.Ident := ParseDbIdent(ditProcedure);

  Result := TShowCreateProcedureStmt.Create(Self, Nodes);
end;

function TMySQLParser.ParseShowCreateTableStmt(): TOffset;
var
  Nodes: TShowCreateTableStmt.TNodes;
begin
  FillChar(Nodes, SizeOf(Nodes), 0);

  Nodes.StmtTag := ParseTag(kiSHOW, kiCREATE, kiTABLE);

  if (not Error) then
    Nodes.Ident := ParseTableIdent();

  Result := TShowCreateTableStmt.Create(Self, Nodes);
end;

function TMySQLParser.ParseShowCreateTriggerStmt(): TOffset;
var
  Nodes: TShowCreateTriggerStmt.TNodes;
begin
  FillChar(Nodes, SizeOf(Nodes), 0);

  Nodes.StmtTag := ParseTag(kiSHOW, kiCREATE, kiTRIGGER);

  if (not Error) then
    Nodes.Ident := ParseDbIdent(ditTrigger);

  Result := TShowCreateTriggerStmt.Create(Self, Nodes);
end;

function TMySQLParser.ParseShowCreateUserStmt(): TOffset;
var
  Nodes: TShowCreateUserStmt.TNodes;
begin
  FillChar(Nodes, SizeOf(Nodes), 0);

  Nodes.StmtTag := ParseTag(kiSHOW, kiCREATE, kiUSER);

  if (not Error) then
    Nodes.User := ParseUserIdent();

  Result := TShowCreateUserStmt.Create(Self, Nodes);
end;

function TMySQLParser.ParseShowCreateViewStmt(): TOffset;
var
  Nodes: TShowCreateViewStmt.TNodes;
begin
  FillChar(Nodes, SizeOf(Nodes), 0);

  Nodes.StmtTag := ParseTag(kiSHOW, kiCREATE, kiVIEW);

  if (not Error) then
    Nodes.Ident := ParseTableIdent();

  Result := TShowCreateViewStmt.Create(Self, Nodes);
end;

function TMySQLParser.ParseShowDatabasesStmt(): TOffset;
var
  Nodes: TShowDatabasesStmt.TNodes;
begin
  FillChar(Nodes, SizeOf(Nodes), 0);

  Nodes.StmtTag := ParseTag(kiSHOW, kiDATABASES);

  if (not Error and not EndOfStmt(CurrentToken)) then
    if (TokenPtr(CurrentToken)^.KeywordIndex = kiLIKE) then
      Nodes.LikeValue := ParseValue(kiLIKE, vaNo, ParseString)
    else if (TokenPtr(CurrentToken)^.KeywordIndex = kiWHERE) then
      Nodes.WhereValue := ParseValue(kiWHERE, vaNo, ParseExpr);

  Result := TShowDatabasesStmt.Create(Self, Nodes);
end;

function TMySQLParser.ParseShowEngineStmt(): TOffset;
var
  Nodes: TShowEngineStmt.TNodes;
begin
  FillChar(Nodes, SizeOf(Nodes), 0);

  Nodes.StmtTag := ParseTag(kiSHOW, kiENGINE);

  if (not Error) then
    Nodes.Ident := ParseIdent();

  if (not Error) then
    if (EndOfStmt(CurrentToken)) then
      SetError(PE_IncompleteStmt)
    else if (TokenPtr(CurrentToken)^.KeywordIndex = kiSTATUS) then
      Nodes.KindTag := ParseTag(kiSTATUS)
    else if (TokenPtr(CurrentToken)^.KeywordIndex = kiMUTEX) then
      Nodes.KindTag := ParseTag(kiMUTEX)
    else
      SetError(PE_UnexpectedToken);

  Result := TShowEngineStmt.Create(Self, Nodes);
end;

function TMySQLParser.ParseShowEnginesStmt(): TOffset;
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

function TMySQLParser.ParseShowErrorsStmt(): TOffset;
var
  Nodes: TShowErrorsStmt.TNodes;
begin
  FillChar(Nodes, SizeOf(Nodes), 0);

  Nodes.StmtTag := ParseTag(kiSHOW, kiERRORS);

  if (not Error and not EndOfStmt(CurrentToken) and (TokenPtr(CurrentToken)^.KeywordIndex = kiLIMIT)) then
  begin
    Nodes.Limit.Tag := ParseTag(kiLIMIT);

    if (EndOfStmt(CurrentToken)) then
      SetError(PE_IncompleteStmt)
    else
    begin
      Nodes.Limit.RowCountToken := ParseInteger();

      if (not Error and not EndOfStmt(CurrentToken) and (TokenPtr(CurrentToken)^.TokenType = ttComma)) then
      begin
        Nodes.Limit.CommaToken := ApplyCurrentToken();

        if (EndOfStmt(CurrentToken)) then
          SetError(PE_IncompleteStmt)
        else
        begin
          Nodes.Limit.OffsetToken := Nodes.Limit.RowCountToken;
          Nodes.Limit.RowCountToken := ParseInteger();
        end;
      end;
    end;
  end;

  Result := TShowErrorsStmt.Create(Self, Nodes);
end;

function TMySQLParser.ParseShowEventsStmt(): TOffset;
var
  Nodes: TShowEventsStmt.TNodes;
begin
  FillChar(Nodes, SizeOf(Nodes), 0);

  Nodes.StmtTag := ParseTag(kiSHOW, kiERRORS);

  if (not Error and not EndOfStmt(CurrentToken)) then
    if (TokenPtr(CurrentToken)^.KeywordIndex = kiFROM) then
      Nodes.FromValue := ParseValue(kiFROM, vaNo, ParseDatabaseIdent)
    else if (TokenPtr(CurrentToken)^.KeywordIndex = kiIN) then
      Nodes.FromValue := ParseValue(kiIN, vaNo, ParseDatabaseIdent);

  if (not Error and not EndOfStmt(CurrentToken)) then
    if (TokenPtr(CurrentToken)^.KeywordIndex = kiLIKE) then
      Nodes.LikeValue := ParseValue(kiLIKE, vaNo, ParseString)
    else if (TokenPtr(CurrentToken)^.KeywordIndex = kiWHERE) then
      Nodes.WhereValue := ParseValue(kiWHERE, vaNo, ParseExpr);

  Result := TShowEventsStmt.Create(Self, Nodes);
end;

function TMySQLParser.ParseShowFunctionCodeStmt(): TOffset;
var
  Nodes: TShowFunctionCodeStmt.TNodes;
begin
  FillChar(Nodes, SizeOf(Nodes), 0);

  Nodes.StmtTag := ParseTag(kiSHOW, kiFUNCTION, kiCODE);

  Result := TShowFunctionCodeStmt.Create(Self, Nodes);
end;

function TMySQLParser.ParseShowFunctionStatusStmt(): TOffset;
var
  Nodes: TShowFunctionStatusStmt.TNodes;
begin
  FillChar(Nodes, SizeOf(Nodes), 0);

  Nodes.StmtTag := ParseTag(kiSHOW, kiFUNCTION, kiSTATUS);

  Result := TShowFunctionStatusStmt.Create(Self, Nodes);
end;

function TMySQLParser.ParseShowGrantsStmt(): TOffset;
var
  Nodes: TShowGrantsStmt.TNodes;
begin
  FillChar(Nodes, SizeOf(Nodes), 0);

  Nodes.StmtTag := ParseTag(kiSHOW, kiGRANTS);

  if (not Error and not EndOfStmt(CurrentToken) and (TokenPtr(CurrentToken)^.KeywordIndex = kiFOR)) then
    Nodes.ForValue := ParseValue(kiFOR, vaNo, ParseUserIdent);

  Result := TShowGrantsStmt.Create(Self, Nodes);
end;

function TMySQLParser.ParseShowIndexStmt(): TOffset;
var
  Nodes: TShowIndexStmt.TNodes;
begin
  FillChar(Nodes, SizeOf(Nodes), 0);

  if (TokenPtr(NextToken[1])^.KeywordIndex = kiINDEX) then
    Nodes.StmtTag := ParseTag(kiSHOW, kiINDEX)
  else if (TokenPtr(NextToken[1])^.KeywordIndex = kiINDEXES) then
    Nodes.StmtTag := ParseTag(kiSHOW, kiINDEXES)
  else if (TokenPtr(NextToken[1])^.KeywordIndex = kiKEYS) then
    Nodes.StmtTag := ParseTag(kiSHOW, kiKEYS)
  else
    SetError(PE_UnexpectedToken);

  if (not Error and EndOfStmt(CurrentToken)) then
    SetError(PE_IncompleteStmt)
  else if (TokenPtr(CurrentToken)^.KeywordIndex = kiFROM) then
    Nodes.FromTableValue := ParseValue(kiFROM, vaNo, ParseTableIdent)
  else if (TokenPtr(CurrentToken)^.KeywordIndex = kiIN) then
    Nodes.FromTableValue := ParseValue(kiIN, vaNo, ParseTableIdent)
  else
    SetError(PE_UnexpectedToken);

  if (not Error and not EndOfStmt(CurrentToken)) then
    if (TokenPtr(CurrentToken)^.KeywordIndex = kiFROM) then
      Nodes.FromDatabaseValue := ParseValue(kiFROM, vaNo, ParseDatabaseIdent)
    else if (TokenPtr(CurrentToken)^.KeywordIndex = kiIN) then
      Nodes.FromDatabaseValue := ParseValue(kiIN, vaNo, ParseDatabaseIdent);

  if (not Error and not EndOfStmt(CurrentToken) and (TokenPtr(CurrentToken)^.KeywordIndex = kiWHERE)) then
    Nodes.WhereValue := ParseValue(kiWHERE, vaNo, ParseExpr);

  Result := TShowIndexStmt.Create(Self, Nodes);
end;

function TMySQLParser.ParseShowMasterStatusStmt(): TOffset;
var
  Nodes: TShowMasterStatusStmt.TNodes;
begin
  FillChar(Nodes, SizeOf(Nodes), 0);

  Nodes.StmtTag := ParseTag(kiSHOW, kiMASTER, kiSTATUS);

  Result := TShowMasterStatusStmt.Create(Self, Nodes);
end;

function TMySQLParser.ParseShowOpenTablesStmt(): TOffset;
var
  Nodes: TShowOpenTablesStmt.TNodes;
begin
  FillChar(Nodes, SizeOf(Nodes), 0);

  Nodes.StmtTag := ParseTag(kiSHOW, kiOPEN, kiTABLES);

  if (not Error and not EndOfStmt(CurrentToken)) then
    if (TokenPtr(CurrentToken)^.KeywordIndex = kiFROM) then
      Nodes.FromDatabaseValue := ParseValue(kiFROM, vaNo, ParseDatabaseIdent)
    else if (TokenPtr(CurrentToken)^.KeywordIndex = kiIN) then
      Nodes.FromDatabaseValue := ParseValue(kiIN, vaNo, ParseDatabaseIdent);

  if (not Error and not EndOfStmt(CurrentToken)) then
    if (TokenPtr(CurrentToken)^.KeywordIndex = kiLIKE) then
      Nodes.LikeValue := ParseValue(kiLIKE, vaNo, ParseString)
    else if (TokenPtr(CurrentToken)^.KeywordIndex = kiWHERE) then
      Nodes.WhereValue := ParseValue(kiWHERE, vaNo, ParseExpr);

  Result := TShowOpenTablesStmt.Create(Self, Nodes);
end;

function TMySQLParser.ParseShowPluginsStmt(): TOffset;
var
  Nodes: TShowPluginsStmt.TNodes;
begin
  FillChar(Nodes, SizeOf(Nodes), 0);

  Nodes.StmtTag := ParseTag(kiSHOW, kiPLUGINS);

  Result := TShowPluginsStmt.Create(Self, Nodes);
end;

function TMySQLParser.ParseShowPrivilegesStmt(): TOffset;
var
  Nodes: TShowPrivilegesStmt.TNodes;
begin
  FillChar(Nodes, SizeOf(Nodes), 0);

  Nodes.StmtTag := ParseTag(kiSHOW, kiPRIVILEGES);

  Result := TShowPrivilegesStmt.Create(Self, Nodes);
end;

function TMySQLParser.ParseShowProcedureCodeStmt(): TOffset;
var
  Nodes: TShowProcedureCodeStmt.TNodes;
begin
  FillChar(Nodes, SizeOf(Nodes), 0);

  Nodes.StmtTag := ParseTag(kiSHOW, kiPROCEDURE, kiCODE);

  Result := TShowProcedureCodeStmt.Create(Self, Nodes);
end;

function TMySQLParser.ParseShowProcedureStatusStmt(): TOffset;
var
  Nodes: TShowProcedureStatusStmt.TNodes;
begin
  FillChar(Nodes, SizeOf(Nodes), 0);

  Nodes.StmtTag := ParseTag(kiSHOW, kiPROCEDURE, kiSTATUS);

  if (not Error and not EndOfStmt(CurrentToken)) then
    if (TokenPtr(CurrentToken)^.KeywordIndex = kiLIKE) then
      Nodes.LikeValue := ParseValue(kiLIKE, vaNo, ParseString)
    else if (TokenPtr(CurrentToken)^.KeywordIndex = kiWHERE) then
      Nodes.WhereValue := ParseValue(kiWHERE, vaNo, ParseExpr);

  Result := TShowProcedureStatusStmt.Create(Self, Nodes);
end;

function TMySQLParser.ParseShowProcessListStmt(): TOffset;
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

function TMySQLParser.ParseShowProfileStmt(): TOffset;
var
  Nodes: TShowProfileStmt.TNodes;
begin
  FillChar(Nodes, SizeOf(Nodes), 0);

  Nodes.StmtTag := ParseTag(kiSHOW, kiPROFILE);

  if (not Error) then
    Nodes.TypeList := ParseList(False, ParseShowProfileStmtType);

  if (not Error and not EndOfStmt(CurrentToken) and (TokenPtr(CurrentToken)^.KeywordIndex = kiFOR)) then
    Nodes.ForQueryValue := ParseValue(WordIndices(kiFOR, kiQUERY), vaNo, ParseInteger);

  if (not Error and not EndOfStmt(CurrentToken) and (TokenPtr(CurrentToken)^.KeywordIndex = kiLIMIT)) then
  begin
    Nodes.LimitValue := ParseValue(kiLIMIT, vaNo, ParseInteger);

    if (not Error and not EndOfStmt(CurrentToken) and (TokenPtr(CurrentToken)^.KeywordIndex = kiOFFSET)) then
      Nodes.LimitValue := ParseValue(kiOFFSET, vaNo, ParseInteger);
  end;

  Result := TShowProfileStmt.Create(Self, Nodes);
end;

function TMySQLParser.ParseShowProfilesStmt(): TOffset;
var
  Nodes: TShowProfilesStmt.TNodes;
begin
  FillChar(Nodes, SizeOf(Nodes), 0);

  Nodes.StmtTag := ParseTag(kiSHOW, kiPROFILES);

  Result := TShowProfilesStmt.Create(Self, Nodes);
end;

function TMySQLParser.ParseShowProfileStmtType(): TOffset;
begin
  Result := 0;

  if (EndOfStmt(CurrentToken)) then
    SetError(PE_IncompleteStmt)
  else if (TokenPtr(CurrentToken)^.KeywordIndex = kiALL) then
    Result := ParseTag(kiALL)
  else if ((TokenPtr(CurrentToken)^.KeywordIndex = kiBLOCK)
    and not EndOfStmt(NextToken[1]) and (TokenPtr(NextToken[1])^.KeywordIndex = kiIO)) then
    Result := ParseTag(kiBLOCK, kiIO)
  else if ((TokenPtr(CurrentToken)^.KeywordIndex = kiCONTEXT)
    and not EndOfStmt(NextToken[1]) and (TokenPtr(NextToken[1])^.KeywordIndex = kiSWITCHES)) then
    Result := ParseTag(kiCONTEXT, kiSWITCHES)
  else if (TokenPtr(CurrentToken)^.KeywordIndex = kiCPU) then
    Result := ParseTag(kiCPU)
  else if (TokenPtr(CurrentToken)^.KeywordIndex = kiIPC) then
    Result := ParseTag(kiIPC)
  else if (TokenPtr(CurrentToken)^.KeywordIndex = kiMEMORY) then
    Result := ParseTag(kiMEMORY)
  else if ((TokenPtr(CurrentToken)^.KeywordIndex = kiPAGE)
    and not EndOfStmt(NextToken[1]) and (TokenPtr(NextToken[1])^.KeywordIndex = kiFAULTS)) then
    Result := ParseTag(kiPAGE, kiFAULTS)
  else if (TokenPtr(CurrentToken)^.KeywordIndex = kiSOURCE) then
    Result := ParseTag(kiSOURCE)
  else
    SetError(PE_UnexpectedToken);
end;

function TMySQLParser.ParseShowRelaylogEventsStmt(): TOffset;
var
  Nodes: TShowRelaylogEventsStmt.TNodes;
begin
  FillChar(Nodes, SizeOf(Nodes), 0);

  Nodes.StmtTag := ParseTag(kiSHOW, kiRELAYLOG, kiEVENTS);

  if (not Error and not EndOfStmt(CurrentToken) and (TokenPtr(CurrentToken)^.KeywordIndex = kiIN)) then
    Nodes.InValue := ParseValue(kiIN, vaNo, ParseString);

  if (not Error and not EndOfStmt(CurrentToken) and (TokenPtr(CurrentToken)^.KeywordIndex = kiFROM)) then
    Nodes.InValue := ParseValue(kiFROM, vaNo, ParseInteger);

  if (not Error and not EndOfStmt(CurrentToken) and (TokenPtr(CurrentToken)^.KeywordIndex = kiLIMIT)) then
  begin
    Nodes.Limit.Tag := ParseTag(kiLIMIT);

    if (EndOfStmt(CurrentToken)) then
      SetError(PE_IncompleteStmt)
    else
    begin
      Nodes.Limit.RowCountToken := ParseInteger();

      if (not Error and not EndOfStmt(CurrentToken) and (TokenPtr(CurrentToken)^.TokenType = ttComma)) then
      begin
        Nodes.Limit.CommaToken := ApplyCurrentToken();

        if (EndOfStmt(CurrentToken)) then
          SetError(PE_IncompleteStmt)
        else
        begin
          Nodes.Limit.OffsetToken := Nodes.Limit.RowCountToken;
          Nodes.Limit.RowCountToken := ParseExpr();
        end;
      end;
    end;
  end;

  Result := TShowRelaylogEventsStmt.Create(Self, Nodes);
end;

function TMySQLParser.ParseShowSlaveHostsStmt(): TOffset;
var
  Nodes: TShowSlaveHostsStmt.TNodes;
begin
  FillChar(Nodes, SizeOf(Nodes), 0);

  Nodes.StmtTag := ParseTag(kiSHOW, kiSLAVE, kiHOSTS);

  Result := TShowSlaveHostsStmt.Create(Self, Nodes);
end;

function TMySQLParser.ParseShowSlaveStatusStmt(): TOffset;
var
  Nodes: TShowSlaveStatusStmt.TNodes;
begin
  FillChar(Nodes, SizeOf(Nodes), 0);

  Nodes.StmtTag := ParseTag(kiSHOW, kiSLAVE, kiSTATUS);

  Result := TShowSlaveStatusStmt.Create(Self, Nodes);
end;

function TMySQLParser.ParseShowStatusStmt(): TOffset;
var
  Nodes: TShowStatusStmt.TNodes;
begin
  FillChar(Nodes, SizeOf(Nodes), 0);

  Nodes.ShowTag := ParseTag(kiSHOW);

  if (not Error and not EndOfStmt(CurrentToken) and ((TokenPtr(CurrentToken)^.KeywordIndex = kiGLOBAL) or (TokenPtr(CurrentToken)^.KeywordIndex = kiSESSION))) then
    Nodes.ScopeTag := ParseTag(TokenPtr(CurrentToken)^.KeywordIndex);

  if (not Error) then
    if (EndOfStmt(CurrentToken)) then
      SetError(PE_IncompleteStmt)
    else if (TokenPtr(CurrentToken)^.KeywordIndex <> kiSTATUS) then
      SetError(PE_UnexpectedToken)
    else
      Nodes.StatusTag := ParseTag(kiSTATUS);

  if (not Error and not EndOfStmt(CurrentToken)) then
    if (TokenPtr(CurrentToken)^.KeywordIndex = kiLIKE) then
      Nodes.LikeValue := ParseValue(kiLIKE, vaNo, ParseString)
    else if (TokenPtr(CurrentToken)^.KeywordIndex = kiWHERE) then
      Nodes.WhereValue := ParseValue(kiWHERE, vaNo, ParseExpr);

  Result := TShowStatusStmt.Create(Self, Nodes);
end;

function TMySQLParser.ParseShowTableStatusStmt(): TOffset;
var
  Nodes: TShowTableStatusStmt.TNodes;
begin
  FillChar(Nodes, SizeOf(Nodes), 0);

  Nodes.ShowTag := ParseTag(kiSHOW, kiTABLE, kiSTATUS);

  if (not Error and not EndOfStmt(CurrentToken)) then
    if (TokenPtr(CurrentToken)^.KeywordIndex = kiFROM) then
      Nodes.FromValue := ParseValue(kiFROM, vaNo, ParseDatabaseIdent)
    else if (TokenPtr(CurrentToken)^.KeywordIndex = kiIN) then
      Nodes.FromValue := ParseValue(kiIN, vaNo, ParseDatabaseIdent);

  if (not Error and not EndOfStmt(CurrentToken)) then
    if (TokenPtr(CurrentToken)^.KeywordIndex = kiLIKE) then
      Nodes.LikeValue := ParseValue(kiLIKE, vaNo, ParseString)
    else if (TokenPtr(CurrentToken)^.KeywordIndex = kiWHERE) then
      Nodes.WhereValue := ParseValue(kiWHERE, vaNo, ParseExpr);

  Result := TShowTableStatusStmt.Create(Self, Nodes);
end;

function TMySQLParser.ParseShowTablesStmt(): TOffset;
var
  Nodes: TShowTablesStmt.TNodes;
begin
  FillChar(Nodes, SizeOf(Nodes), 0);

  Nodes.ShowTag := ParseTag(kiSHOW);

  if (not Error and not EndOfStmt(CurrentToken) and (TokenPtr(CurrentToken)^.KeywordIndex = kiFULL)) then
    Nodes.FullTag := ParseTag(kiFULL);

  if (not Error) then
    Nodes.TablesTag := ParseTag(kiTABLES);

  if (not Error and not EndOfStmt(CurrentToken)) then
    if (TokenPtr(CurrentToken)^.KeywordIndex = kiFROM) then
      Nodes.FromValue := ParseValue(kiFROM, vaNo, ParseDatabaseIdent)
    else if (TokenPtr(CurrentToken)^.KeywordIndex = kiIN) then
      Nodes.FromValue := ParseValue(kiIN, vaNo, ParseDatabaseIdent);

  if (not Error and not EndOfStmt(CurrentToken)) then
    if (TokenPtr(CurrentToken)^.KeywordIndex = kiLIKE) then
      Nodes.LikeValue := ParseValue(kiLIKE, vaNo, ParseString)
    else if (TokenPtr(CurrentToken)^.KeywordIndex = kiWHERE) then
      Nodes.WhereValue := ParseValue(kiWHERE, vaNo, ParseExpr);

  Result := TShowTablesStmt.Create(Self, Nodes);
end;

function TMySQLParser.ParseShowTriggersStmt(): TOffset;
var
  Nodes: TShowTriggersStmt.TNodes;
begin
  FillChar(Nodes, SizeOf(Nodes), 0);

  Nodes.StmtTag := ParseTag(kiSHOW, kiTRIGGERS);

  if (not Error and not EndOfStmt(CurrentToken)) then
    if (TokenPtr(CurrentToken)^.KeywordIndex = kiFROM) then
      Nodes.FromValue := ParseValue(kiFROM, vaNo, ParseDatabaseIdent)
    else if (TokenPtr(CurrentToken)^.KeywordIndex = kiIN) then
      Nodes.FromValue := ParseValue(kiIN, vaNo, ParseDatabaseIdent);

  if (not Error and not EndOfStmt(CurrentToken)) then
    if (TokenPtr(CurrentToken)^.KeywordIndex = kiLIKE) then
      Nodes.LikeValue := ParseValue(kiLIKE, vaNo, ParseString)
    else if (TokenPtr(CurrentToken)^.KeywordIndex = kiWHERE) then
      Nodes.WhereValue := ParseValue(kiWHERE, vaNo, ParseExpr);

  Result := TShowTriggersStmt.Create(Self, Nodes);
end;

function TMySQLParser.ParseShowVariablesStmt(): TOffset;
var
  Nodes: TShowVariablesStmt.TNodes;
begin
  FillChar(Nodes, SizeOf(Nodes), 0);

  Nodes.ShowTag := ParseTag(kiSHOW);

  if (not Error and not EndOfStmt(CurrentToken) and ((TokenPtr(CurrentToken)^.KeywordIndex = kiGLOBAL) or (TokenPtr(CurrentToken)^.KeywordIndex = kiSESSION))) then
    Nodes.ScopeTag := ParseTag(TokenPtr(CurrentToken)^.KeywordIndex);

  if (not Error) then
    if (EndOfStmt(CurrentToken)) then
      SetError(PE_IncompleteStmt)
    else if (TokenPtr(CurrentToken)^.KeywordIndex <> kiVARIABLES) then
      SetError(PE_UnexpectedToken)
    else
      Nodes.VariablesTag := ParseTag(kiVARIABLES);

  if (not Error and not EndOfStmt(CurrentToken)) then
    if (TokenPtr(CurrentToken)^.KeywordIndex = kiLIKE) then
      Nodes.LikeValue := ParseValue(kiLIKE, vaNo, ParseString)
    else if (TokenPtr(CurrentToken)^.KeywordIndex = kiWHERE) then
      Nodes.WhereValue := ParseValue(kiWHERE, vaNo, ParseExpr);

  Result := TShowVariablesStmt.Create(Self, Nodes);
end;

function TMySQLParser.ParseShowWarningsStmt(): TOffset;
var
  Nodes: TShowWarningsStmt.TNodes;
begin
  FillChar(Nodes, SizeOf(Nodes), 0);

  Nodes.StmtTag := ParseTag(kiSHOW, kiWARNINGS);

  if (not Error and not EndOfStmt(CurrentToken) and (TokenPtr(CurrentToken)^.KeywordIndex = kiLIMIT)) then
  begin
    Nodes.Limit.Tag := ParseTag(kiLIMIT);

    if (EndOfStmt(CurrentToken)) then
      SetError(PE_IncompleteStmt)
    else
    begin
      Nodes.Limit.RowCountToken := ParseInteger();

      if (not Error and not EndOfStmt(CurrentToken) and (TokenPtr(CurrentToken)^.TokenType = ttComma)) then
      begin
        Nodes.Limit.CommaToken := ApplyCurrentToken();

        if (EndOfStmt(CurrentToken)) then
          SetError(PE_IncompleteStmt)
        else
        begin
          Nodes.Limit.OffsetToken := Nodes.Limit.RowCountToken;
          Nodes.Limit.RowCountToken := ParseInteger();
        end;
      end;
    end;
  end;

  Result := TShowWarningsStmt.Create(Self, Nodes);
end;

function TMySQLParser.ParseShutdownStmt(): TOffset;
var
  Nodes: TShutdownStmt.TNodes;
begin
  FillChar(Nodes, SizeOf(Nodes), 0);

  Nodes.StmtTag := ParseTag(kiSHUTDOWN);

  Result := TShutdownStmt.Create(Self, Nodes);
end;

function TMySQLParser.ParseSignalStmt(): TOffset;
var
  Nodes: TSignalStmt.TNodes;
begin
  FillChar(Nodes, SizeOf(Nodes), 0);

  if (TokenPtr(CurrentToken)^.KeywordIndex = kiSIGNAL) then
  begin
    Nodes.StmtTag := ParseTag(kiSIGNAL);

    if (not Error) then
      if (EndOfStmt(CurrentToken)) then
        SetError(PE_IncompleteStmt)
      else if ((TokenPtr(CurrentToken)^.KeywordIndex = kiSQLSTATE)
        and not EndOfStmt(NextToken[1]) and (TokenPtr(NextToken[1])^.KeywordIndex = kiVALUE)) then
        Nodes.Condition := ParseValue(WordIndices(kiSQLSTATE, kiVALUE), vaNo, ParseExpr)
      else if (TokenPtr(CurrentToken)^.KeywordIndex = kiSQLSTATE) then
        Nodes.Condition := ParseValue(kiSQLSTATE, vaNo, ParseString)
      else
        Nodes.Condition := ParseIdent();
  end
  else if (TokenPtr(CurrentToken)^.KeywordIndex = kiRESIGNAL) then
  begin
    Nodes.StmtTag := ParseTag(kiRESIGNAL);

    if (not Error and not EndOfStmt(CurrentToken)) then
      if ((TokenPtr(CurrentToken)^.KeywordIndex = kiSQLSTATE)
        and not EndOfStmt(NextToken[1]) and (TokenPtr(NextToken[1])^.KeywordIndex = kiVALUE)) then
        Nodes.Condition := ParseValue(WordIndices(kiSQLSTATE, kiVALUE), vaYes, ParseExpr)
      else if (TokenPtr(CurrentToken)^.KeywordIndex = kiSQLSTATE) then
        Nodes.Condition := ParseValue(kiSQLSTATE, vaNo, ParseString)
      else
        Nodes.Condition := ParseIdent();
  end
  else
    SetError(PE_Unknown);

  if (not Error and not EndOfStmt(CurrentToken) and (TokenPtr(CurrentToken)^.KeywordIndex = kiSET)) then
  begin
    Nodes.SetTag := ParseTag(kiSET);

    if (not Error and not EndOfStmt(CurrentToken)) then
      Nodes.InformationList := ParseList(False, ParseSignalStmtInformation);
  end;

  Result := TSignalStmt.Create(Self, Nodes);
end;

function TMySQLParser.ParseSignalStmtInformation(): TOffset;
var
  Nodes: TSignalStmt.TInformation.TNodes;
begin
  FillChar(Nodes, SizeOf(Nodes), 0);

  if (EndOfStmt(CurrentToken)) then
    SetError(PE_IncompleteStmt)
  else if ((TokenPtr(CurrentToken)^.KeywordIndex = kiCLASS_ORIGIN)
    or (TokenPtr(CurrentToken)^.KeywordIndex = kiSUBCLASS_ORIGIN)
    or (TokenPtr(CurrentToken)^.KeywordIndex = kiMESSAGE_TEXT)
    or (TokenPtr(CurrentToken)^.KeywordIndex = kiMYSQL_ERRNO)
    or (TokenPtr(CurrentToken)^.KeywordIndex = kiCONSTRAINT_CATALOG)
    or (TokenPtr(CurrentToken)^.KeywordIndex = kiCONSTRAINT_SCHEMA)
    or (TokenPtr(CurrentToken)^.KeywordIndex = kiCONSTRAINT_NAME)
    or (TokenPtr(CurrentToken)^.KeywordIndex = kiCATALOG_NAME)
    or (TokenPtr(CurrentToken)^.KeywordIndex = kiSCHEMA_NAME)
    or (TokenPtr(CurrentToken)^.KeywordIndex = kiTABLE_NAME)
    or (TokenPtr(CurrentToken)^.KeywordIndex = kiCOLUMN_NAME)
    or (TokenPtr(CurrentToken)^.KeywordIndex = kiCURSOR_NAME)) then
    Nodes.Value := ParseValue(TokenPtr(CurrentToken)^.KeywordIndex, vaYes, ParseExpr)
  else
    SetError(PE_UnexpectedToken);

  Result := TSignalStmt.TInformation.Create(Self, Nodes);
end;

function TMySQLParser.ParseSQL(const Text: PChar; const Length: Integer): Boolean;
begin
  Clear();

  SetString(ParseText, Text, Length);
  ParsePosition.Text := PChar(ParseText);
  ParsePosition.Length := Length;

  FRoot := ParseRoot();

  Result := not Error;
end;

function TMySQLParser.ParseSQL(const Text: string): Boolean;
begin
  Result := ParseSQL(PChar(Text), Length(Text));
end;

function TMySQLParser.ParseStartSlaveStmt(): TOffset;
var
  Nodes: TStartSlaveStmt.TNodes;
begin
  FillChar(Nodes, SizeOf(Nodes), 0);

  Nodes.StmtTag := ParseTag(kiSTART, kiSLAVE);

  Result := TStartSlaveStmt.Create(Self, Nodes);
end;

function TMySQLParser.ParseStartTransactionStmt(): TOffset;
var
  Found: Boolean;
  Nodes: TStartTransactionStmt.TNodes;
begin
  FillChar(Nodes, SizeOf(Nodes), 0);

  Nodes.StartTransactionTag := ParseTag(kiSTART, kiTRANSACTION);

  Found := True;
  while (not Error and Found and not EndOfStmt(CurrentToken)) do
    if (TokenPtr(CurrentToken)^.KeywordIndex = kiREAD) then
      if (EndOfStmt(NextToken[1])) then
        SetError(PE_IncompleteStmt)
      else if (TokenPtr(CurrentToken)^.KeywordIndex = kiONLY) then
        Nodes.RealOnlyTag := ParseTag(kiREAD, kiONLY)
      else if (TokenPtr(CurrentToken)^.KeywordIndex = kiWRITE) then
        Nodes.RealOnlyTag := ParseTag(kiREAD, kiWRITE)
      else
        SetError(PE_UnexpectedToken)
    else if (TokenPtr(CurrentToken)^.KeywordIndex = kiWITH) then
      Nodes.WithConsistentSnapshotTag := ParseTag(kiWITH, kiCONSISTENT, kiSNAPSHOT)
    else
      Found := False;

  Result := TStartTransactionStmt.Create(Self, Nodes);
end;

function TMySQLParser.ParseStopSlaveStmt(): TOffset;
var
  Nodes: TStopSlaveStmt.TNodes;
begin
  FillChar(Nodes, SizeOf(Nodes), 0);

  Nodes.StmtTag := ParseTag(kiSTOP, kiSLAVE);

  Result := TStopSlaveStmt.Create(Self, Nodes);
end;

function TMySQLParser.ParseStmt(): TOffset;
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
  S: string;
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
    if (T^.TokenType = ttDelimiter) then
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
    SetError(PE_IncompleteStmt)
  else
  begin
    KeywordIndex := TokenPtr(KeywordToken)^.KeywordIndex;

    if (KeywordIndex = kiANALYZE) then
      Result := ParseAnalyzeStmt()
    else if (KeywordIndex = kiALTER) then
      Result := ParseAlterStmt()
    else if (KeywordIndex = kiBEGIN) then
      if (not InPL_SQL) then
        Result := ParseBeginStmt()
      else
        Result := ParseCompoundStmt()
    else if (KeywordIndex = kiCALL) then
      Result := ParseCallStmt()
    else if (InPL_SQL and (KeywordIndex = kiCASE)) then
      Result := ParseCaseStmt()
    else if (KeywordIndex = kiCHECK) then
      Result := ParseCheckStmt()
    else if (KeywordIndex = kiCHECKSUM) then
      Result := ParseChecksumStmt()
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
      Result := ParseLockStmt()
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
      Result := ParseRepairStmt()
    else if (InPL_SQL and (KeywordIndex = kiREPEAT)) then
      Result := ParseRepeatStmt()
    else if (KeywordIndex = kiRELEASE) then
      Result := ParseReleaseStmt()
    else if (KeywordIndex = kiREPLACE) then
      Result := ParseInsertStmt()
    else if (KeywordIndex = kiRESET) then
      Result := ParseResetStmt()
    else if (InPL_SQL and (KeywordIndex = kiRETURN) and InCreateFunctionStmt) then
      Result := ParseReturnStmt()
    else if (KeywordIndex = kiREVOKE) then
      Result := ParseRevokeStmt()
    else if (KeywordIndex = kiROLLBACK) then
      Result := ParseRollbackStmt()
    else if (KeywordIndex = kiSAVEPOINT) then
      Result := ParseSavepointStmt()
    else if (KeywordIndex = kiSELECT) then
      Result := ParseSelectStmt()
    else if (KeywordIndex = kiSET) then
      if (not EndOfStmt(NextToken[1]) and (TokenPtr(NextToken[1])^.KeywordIndex = kiNAMES)) then
        Result := ParseSetNamesStmt()
      else if (not EndOfStmt(NextToken[1]) and (TokenPtr(NextToken[1])^.KeywordIndex = kiCHARACTER)) then
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
      Result := ParseUnlockStmt()
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
      Result := ParseSubAreaSelectStmt()
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
      while ((Token > 0) and (TokenPtr(Token)^.TokenType <> ttDelimiter)) do
      begin
        T := TokenPtr(Token)^.NextTokenAll;
        if (not Assigned(T)) then
          Token := 0
        else
          Token := T^.Offset;
        if ((Token > 0) and (TokenPtr(Token)^.TokenType <> ttDelimiter)) then
          FLastTokenAll := Token;
      end;

      StmtPtr(Result)^.FErrorCode := FErrorCode;
      if (FErrorCode > PE_Success) then
      begin
        case (FErrorCode) of
          PE_IncompleteToken:
            S := 'Incomplete token in line ' + IntToStr(FErrorLine);
          PE_UnexpectedChar:
            S := 'Unexpected character near ''' + LeftStr(StrPas(TokenPtr(FErrorToken)^.ErrorPos), 8) + ''''
              + ' in line ' + IntToStr(FErrorLine);
          PE_UnexpectedToken:
            begin
              TokenPtr(FErrorToken)^.GetText(Text, Length);
              S := 'Unexpected character near ''' + LeftStr(StrPas(Text), 8) + ''''
                + ' in line ' + IntToStr(FErrorLine);
            end;
          PE_ExtraToken:
            begin
              TokenPtr(FErrorToken)^.GetText(Text, Length);
              S := 'Unexpected character near ''' + LeftStr(StrPas(Text), 8) + ''''
                + ' in line ' + IntToStr(FErrorLine);
            end;
          PE_UnknownStmt:
            begin
              TokenPtr(FErrorToken)^.GetText(Text, Length);
              S := 'Unknown statement ''' + LeftStr(StrPas(Text), 8) + ''''
                + ' in line ' + IntToStr(FErrorLine);
            end;
          else S := GetErrorMessage(FErrorCode);
        end;
        StmtPtr(Result)^.FErrorMessage := NewText(PChar(S), System.Length(S));
      end;
      StmtPtr(Result)^.FErrorToken := FErrorToken;
      StmtPtr(Result)^.FFirstTokenAll := FFirstTokenAll;
      StmtPtr(Result)^.FLastTokenAll := FLastTokenAll;
    end;
  end;
end;

function TMySQLParser.ParseString(): TOffset;
begin
  Result := 0;
  if (EndOfStmt(CurrentToken)) then
    SetError(PE_IncompleteStmt)
  else if (not (TokenPtr(CurrentToken)^.TokenType in ttStrings) and not ((TokenPtr(CurrentToken)^.TokenType = ttIdent) and (TokenPtr(CurrentToken)^.KeywordIndex < 0))) then
    SetError(PE_UnexpectedToken)
  else
    Result := ApplyCurrentToken();
end;

function TMySQLParser.ParseSubArea(const ParseArea: TParseFunction): TOffset;
var
  Nodes: TSubArea.TNodes;
begin
  FillChar(Nodes, SizeOf(Nodes), 0);

  if (EndOfStmt(CurrentToken)) then
    SetError(PE_IncompleteStmt)
  else if (TokenPtr(CurrentToken)^.TokenType <> ttOpenBracket) then
    SetError(PE_UnexpectedToken)
  else
    Nodes.OpenBracket := ApplyCurrentToken();

  if (not Error) then
    Nodes.AreaNode := ParseArea();

  if (not Error) then
    if (EndOfStmt(CurrentToken)) then
      SetError(PE_IncompleteStmt)
    else if (TokenPtr(CurrentToken)^.TokenType <> ttCloseBracket) then
      SetError(PE_UnexpectedToken)
    else
      Nodes.CloseBracket := ApplyCurrentToken();

  Result := TSubArea.Create(Self, Nodes);
end;

function TMySQLParser.ParseSubAreaSelectStmt(): TOffset;
var
  Nodes: TSubAreaSelectStmt.TNodes;
begin
  Result := 0;

  if (EndOfStmt(CurrentToken)) then
    SetError(PE_IncompleteStmt)
  else if (TokenPtr(CurrentToken)^.TokenType <> ttOpenBracket) then
    Result := ParseSelectStmt()
  else
  begin
    Result := ParseSubArea(ParseSelectStmt);

    if (not Error and not EndOfStmt(CurrentToken) and (TokenPtr(CurrentToken)^.KeywordIndex = kiUNION)) then
    begin
      Nodes.SelectStmt1 := Result;

      if (not Error and not EndOfStmt(NextToken[1]) and ((TokenPtr(NextToken[1])^.KeywordIndex = kiALL) or (TokenPtr(NextToken[1])^.KeywordIndex = kiDISTINCT))) then
        Nodes.UnionTag := ParseTag(kiUNION, TokenPtr(NextToken[1])^.KeywordIndex)
      else
        Nodes.UnionTag := ParseTag(kiUNION);

      if (not Error) then
        if (EndOfStmt(CurrentToken)) then
          SetError(PE_IncompleteStmt)
        else
          Nodes.SelectStmt2 := ParseSubAreaSelectStmt();

      Result := TSubAreaSelectStmt.Create(Self, Nodes);
    end;
  end;
end;

function TMySQLParser.ParseSubPartition(): TOffset;
var
  Found: Boolean;
  Nodes: TSubPartition.TNodes;
begin
  if (not Error) then
    Nodes.SubPartitionTag := ParseTag(kiPARTITION);

  if (not Error) then
    Nodes.NameIdent := ParsePartitionIdent();

  Found := True;
  while (not Error and Found and not EndOfStmt(CurrentToken)) do
    if ((Nodes.CommentValue = 0) and (TokenPtr(CurrentToken)^.KeywordIndex = kiCOMMENT)) then
      Nodes.CommentValue := ParseValue(kiCOMMENT, vaAuto, ParseString)
    else if ((Nodes.DataDirectoryValue = 0) and (TokenPtr(CurrentToken)^.KeywordIndex = kiDATA)) then
      Nodes.DataDirectoryValue := ParseValue(WordIndices(kiDATA, kiDIRECTORY), vaAuto, ParseString)
    else if ((Nodes.EngineValue = 0) and (TokenPtr(CurrentToken)^.KeywordIndex = kiSTORAGE)) then
      Nodes.EngineValue := ParseValue(WordIndices(kiSTORAGE, kiENGINE), vaAuto, ParseIdent)
    else if ((Nodes.EngineValue = 0) and (TokenPtr(CurrentToken)^.KeywordIndex = kiENGINE)) then
      Nodes.EngineValue := ParseValue(kiENGINE, vaAuto, ParseIdent)
    else if ((Nodes.IndexDirectoryValue = 0) and (TokenPtr(CurrentToken)^.KeywordIndex = kiINDEX)) then
      Nodes.IndexDirectoryValue := ParseValue(WordIndices(kiINDEX, kiDIRECTORY), vaAuto, ParseString)
    else if ((Nodes.MaxRowsValue = 0) and (TokenPtr(CurrentToken)^.KeywordIndex = kiMAX_ROWS)) then
      Nodes.MaxRowsValue := ParseValue(kiMAX_ROWS, vaAuto, ParseInteger)
    else if ((Nodes.MinRowsValue = 0) and (TokenPtr(CurrentToken)^.KeywordIndex = kiMIN_ROWS)) then
      Nodes.MinRowsValue := ParseValue(kiMIN_ROWS, vaAuto, ParseInteger)
    else if ((Nodes.TablespaceValue = 0) and (TokenPtr(CurrentToken)^.KeywordIndex = kiTABLESPACE)) then
      Nodes.TablespaceValue := ParseValue(kiTABLESPACE, vaAuto, ParseIdent)
    else
      Found := False;

  Result := TSubPartition.Create(Self, Nodes);
end;

function TMySQLParser.ParseSubstringFunc(): TOffset;
var
  Nodes: TSubstringFunc.TNodes;
begin
  FillChar(Nodes, SizeOf(Nodes), 0);

  Nodes.FuncToken := ApplyCurrentToken(utFunction);

  if (not Error) then
    if (EndOfStmt(CurrentToken)) then
      SetError(PE_IncompleteStmt)
    else if (TokenPtr(CurrentToken)^.TokenType <> ttOpenBracket) then
      SetError(PE_UnexpectedToken)
    else
      Nodes.OpenBracket := ApplyCurrentToken();

  if (not Error) then
    Nodes.Str := ParseExpr();

  if (not Error) then
    if (EndOfStmt(CurrentToken)) then
      SetError(PE_IncompleteStmt)
    else if (TokenPtr(CurrentToken)^.KeywordIndex = kiFROM) then
      Nodes.FromTag := ParseTag(kiFROM)
    else if (TokenPtr(CurrentToken)^.TokenType = ttComma) then
      Nodes.FromTag := ApplyCurrentToken()
    else
      SetError(PE_UnexpectedToken);

  if (not Error) then
    Nodes.Pos := ParseExpr();

  if (not Error and not EndOfStmt(CurrentToken)) then
    if (TokenPtr(CurrentToken)^.KeywordIndex = kiFOR) then
      Nodes.ForTag := ParseTag(kiFOR)
    else if (TokenPtr(CurrentToken)^.TokenType = ttComma) then
      Nodes.ForTag := ApplyCurrentToken();

  if (not Error and (Nodes.ForTag > 0)) then
    Nodes.Len := ParseExpr();

  if (not Error) then
    if (EndOfStmt(CurrentToken)) then
      SetError(PE_IncompleteStmt)
    else if (TokenPtr(CurrentToken)^.TokenType <> ttCloseBracket) then
      SetError(PE_UnexpectedToken)
    else
      Nodes.CloseBracket := ApplyCurrentToken();

  Result := TSubstringFunc.Create(Self, Nodes);
end;

function TMySQLParser.ParseTableIdent(): TOffset;
begin
  Result := ParseDbIdent(ditTable);
end;

function TMySQLParser.ParseTag(const KeywordIndex1: TWordList.TIndex;
  const KeywordIndex2: TWordList.TIndex = -1; const KeywordIndex3: TWordList.TIndex = -1;
  const KeywordIndex4: TWordList.TIndex = -1; const KeywordIndex5: TWordList.TIndex = -1;
  const KeywordIndex6: TWordList.TIndex = -1): TOffset;
var
  Nodes: TTag.TNodes;
begin
  FillChar(Nodes, SizeOf(Nodes), 0);

  if (EndOfStmt(CurrentToken)) then
    SetError(PE_IncompleteStmt)
  else if (TokenPtr(CurrentToken)^.KeywordIndex <> KeywordIndex1) then
    SetError(PE_UnexpectedToken)
  else
  begin
    TokenPtr(CurrentToken)^.FOperatorType := otUnknown;
    Nodes.KeywordToken1 := ApplyCurrentToken();

    if (KeywordIndex2 >= 0) then
    begin
      if (EndOfStmt(CurrentToken)) then
        SetError(PE_IncompleteStmt)
      else if (TokenPtr(CurrentToken)^.KeywordIndex <> KeywordIndex2) then
        SetError(PE_UnexpectedToken)
      else
      begin
        TokenPtr(CurrentToken)^.FOperatorType := otUnknown;
        Nodes.KeywordToken2 := ApplyCurrentToken();

        if (KeywordIndex3 >= 0) then
        begin
          if (EndOfStmt(CurrentToken)) then
            SetError(PE_IncompleteStmt)
          else if (TokenPtr(CurrentToken)^.KeywordIndex <> KeywordIndex3) then
            SetError(PE_UnexpectedToken)
          else
          begin
            TokenPtr(CurrentToken)^.FOperatorType := otUnknown;
            Nodes.KeywordToken3 := ApplyCurrentToken();

            if (KeywordIndex4 >= 0) then
            begin
              if (EndOfStmt(CurrentToken)) then
                SetError(PE_IncompleteStmt)
              else if (TokenPtr(CurrentToken)^.KeywordIndex <> KeywordIndex4) then
                SetError(PE_UnexpectedToken)
              else
              begin
                TokenPtr(CurrentToken)^.FOperatorType := otUnknown;
                Nodes.KeywordToken4 := ApplyCurrentToken();
              end;

              if (KeywordIndex5 >= 0) then
              begin
                if (EndOfStmt(CurrentToken)) then
                  SetError(PE_IncompleteStmt)
                else if (TokenPtr(CurrentToken)^.KeywordIndex <> KeywordIndex4) then
                  SetError(PE_UnexpectedToken)
                else
                begin
                  TokenPtr(CurrentToken)^.FOperatorType := otUnknown;
                  Nodes.KeywordToken5 := ApplyCurrentToken();

                  if (KeywordIndex6 >= 0) then
                  begin
                    if (EndOfStmt(CurrentToken)) then
                      SetError(PE_IncompleteStmt)
                    else if (TokenPtr(CurrentToken)^.KeywordIndex <> KeywordIndex4) then
                      SetError(PE_UnexpectedToken)
                    else
                    begin
                      TokenPtr(CurrentToken)^.FOperatorType := otUnknown;
                      Nodes.KeywordToken6 := ApplyCurrentToken();
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

function TMySQLParser.ParseToken(): TOffset;
label
  TwoChars,
  Selection, SelSpace, SelQuotedIdent, SelNotLess, SelNotEqual1, SelNotGreater, SelNot1, SelDoubleQuote, SelComment, SelModulo, SelDolor, SelAmpersand2, SelBitAND, SelSingleQuote, SelOpenBracket, SelCloseBracket, SelMySQLCodeEnd, SelMulti, SelComma, SelDoubleDot, SelDot, SelDotNumber, SelMySQLCode, SelDiv, SelInteger, SelSLComment, SelArrow, SelMinus, SelPlus, SelAssign, SelColon, SelDelimiter, SelNULLSaveEqual, SelLessEqual, SelShiftLeft, SelNotEqual2, SelLess, SelEqual, SelGreaterEqual, SelShiftRight, SelGreater, SelParameter, SelAt, SelHex, SelHex2, SelUnquotedIdent, SelDBIdent, SelCloseSquareBracket, SelHat, SelMySQLCharacterSet, SelMySQLIdent, SelBitValueHigh, SelBitValueLow, SelHexValueHigh, SelHexValueLow, SelUnquotedIdentLower, SelOpenCurlyBracket, SelOpenCurlyBracket2, SelPipe, SelBitOR, SelCloseCurlyBracket, SelTilde, SelE,
  SLComment, SLCommentL,
  MLComment, MLCommentL, MLCommentL2, MLCommentL3,
  MySQLCharacterSet, MySQLCharacterSetL, MySQLCharacterSetL2, MySQLCharacterSetLE, MySQLCharacterSetE, MySQLCharacterSetE2, MySQLCharacterSetE3,
  MySQLCondCode, MySQLCondCodeL, MySQLCondCodeE,
  Numeric, NumericL, NumericExp, NumericE, NumericDot, NumericLE,
  Hex, HexL, HexL2, HexLE,
  IPAddress, IPAddressL, IPAddressLE,
  QuotedIdent, QuotedIdentL, QuotedIdentL2, QuotedIdentLE, QuotedIdentE,
  Return, Return2, ReturnE,
  Separator,
  UnquotedIdent, UnquotedIdentL, UnquotedIdentLE,
  WhiteSpace, WhiteSpaceL, WhiteSpaceLE,
  IncompleteToken, UnexpectedChar, UnexpectedCharL,
  TrippelChar,
  DoubleChar,
  SingleChar,
  Finish;
const
  Terminators: PChar = #9#10#13#32'#%&''()*+,-./:;<=>@`{|}'; // Characters, terminating a token
  TerminatorsL = 26; // Count of Terminators
var
  DotFound: Boolean;
  EFound: Boolean;
  ErrorCode: Byte;
  ErrorPos: PChar;
  KeywordIndex: TWordList.TIndex;
  Length: Integer;
  Line: Integer;
  OperatorType: TOperatorType;
  Text: PChar;
  TokenLength: Integer;
  TokenType: TTokenType;
  UsageType: TUsageType;
begin
  if (ParsePosition.Length = 0) then
    Result := 0
  else
  begin
    TokenType := ttUnknown;
    OperatorType := otUnknown;
    ErrorCode := PE_Success;
    Line := 0;
    ErrorPos := nil;
    Text := ParsePosition.Text;
    Length := ParsePosition.Length;

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
        CMP AX,9                         // Tab ?
        JE WhiteSpace                    // Yes!
        CMP AX,10                        // <NewLine> ?
        JE Return                        // Yes!
        CMP AX,13                        // <CarriadgeReturn> ?
        JE Return                        // Yes!
        CMP AX,31                        // Invalid char ?
        JBE UnexpectedChar               // Yes!
      SelSpace:
        CMP AX,' '                       // Space ?
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
        MOV DX,'"'                       // End Quoter
        JMP QuotedIdent
      SelComment:
        CMP AX,'#'                       // "#" ?
        JE SLComment                     // Yes!
      SelDolor:
        CMP AX,'$'                       // "$" ?
        JE UnquotedIdent                 // Yes!
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
        MOV DX,''''                      // End Quoter
        JMP QuotedIdent
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
        MOV TokenType,ttDelimiter
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
        JA SelDBIdent                    // No!
        MOV TokenType,ttIdent
        JMP UnquotedIdent                // Yes!
      SelDBIdent:
        CMP AX,'['                       // "[" ?
        JNE SelCloseSquareBracket        // No!
        MOV TokenType,ttDBIdent
        MOV DX,']'                       // End Quoter
        JMP QuotedIdent

      SelCloseSquareBracket:
        CMP AX,']'                       // "]" ?
        JE UnexpectedChar                // Yes!
      SelHat:
        CMP AX,'^'                       // "^" ?
        JNE SelMySQLCharacterSet         // No!
        MOV OperatorType,otHat
        JMP SingleChar
      SelMySQLCharacterSet:
        CMP AX,'_'                       // "_" ?
        JE MySQLCharacterSet             // Yes!
      SelMySQLIdent:
        CMP AX,'`'                       // "`" ?
        JNE SelBitValueHigh              // No!
        MOV TokenType,ttMySQLIdent
        MOV DX,'`'                       // End Quoter
        JMP QuotedIdent
      SelBitValueHigh:
        CMP EAX,$00270042                // "B'" ?
        JNE SelBitValueLow               // No!
        ADD ESI,2                        // Step over "B"
        DEC ECX                          // One character handled
        MOV TokenType,ttString
        MOV DX,''''                      // End Quoter
        JMP QuotedIdent
      SelBitValueLow:
        CMP EAX,$00270062                // "b'" ?
        JNE SelHexValueHigh              // No!
        ADD ESI,2                        // Step over "b"
        DEC ECX                          // One character handled
        MOV TokenType,ttString
        MOV DX,''''                      // End Quoter
        JMP QuotedIdent
      SelHexValueHigh:
        CMP EAX,$00270048                // "H'" ?
        JNE SelHexValueLow               // No!
        ADD ESI,2                        // Step over "H"
        DEC ECX                          // One character handled
        MOV TokenType,ttString
        MOV DX,''''                      // End Quoter
        JMP QuotedIdent
      SelHexValueLow:
        CMP EAX,$00270068                // "h'" ?
        JNE SelUnquotedIdentLower        // No!
        ADD ESI,2                        // Step over "h"
        DEC ECX                          // One character handled
        MOV TokenType,ttString
        MOV DX,''''                      // End Quoter
        JMP QuotedIdent
      SelUnquotedIdentLower:
        CMP AX,'z'                       // Low case character?
        JA SelOpenCurlyBracket           // No!
        MOV TokenType,ttIdent
        JMP UnquotedIdent                // Yes!
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
        JMP UnquotedIdent

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

      MySQLCharacterSet:
        MOV TokenType,ttCSString
        ADD ESI,2                        // Next character in SQL
        DEC ECX                          // One character handled
        MOV EDX,ESI
      MySQLCharacterSetL:
        MOV AX,[ESI]                     // One Character from SQL to AX
        CMP AX,' '                       // <Space>?
        JE MySQLCharacterSetE2           // Yes!
        CMP AX,''''                      // "'"?
        JE MySQLCharacterSetE3           // Yes!
        CALL Separator                   // SQL separator?
        JNE MySQLCharacterSetL2          // No!
        MOV TokenType,ttIdent
        JMP Finish
      MySQLCharacterSetL2:
        CMP AX,'0'                       // Digit?
        JB MySQLCharacterSetE            // No!
        CMP AX,'9'
        JBE MySQLCharacterSetLE          // Yes!
        CMP AX,'A'                       // String character?
        JB MySQLCharacterSetE            // No!
        CMP AX,'Z'
        JBE MySQLCharacterSetLE          // Yes!
        CMP AX,'a'                       // String character?
        JB MySQLCharacterSetE            // No!
        CMP AX,'z'
        JBE MySQLCharacterSetLE          // Yes!
      MySQLCharacterSetLE:
        ADD ESI,2                        // Next character in SQL
        LOOP MySQLCharacterSetL
        MOV TokenType,ttIdent
        JMP Finish
      MySQLCharacterSetE:
        CMP AX,'_'                       // Secord "_"?
        JE UnquotedIdent                 // Yes!
        CMP ESI,EDX                      // Empty ident?
        JE IncompleteToken               // Yes!
        MOV AX,[ESI]                     // One Character from SQL to AX
        CMP AX,''''                      // "'"?
        JE MySQLCharacterSetE2
        MOV TokenType,ttIdent
        JMP UnquotedIdent
      MySQLCharacterSetE2:
        CMP ECX,0                        // End of SQL?
        JE IncompleteToken               // Yes!
        ADD ESI,2                        // Step over <Space>
        DEC ECX                          // One character handled
        JZ IncompleteToken               // End of SQL!
        MOV AX,[ESI]
        CMP AX,'0'                       // "0"?
        JNE UnexpectedChar               // No!
        ADD ESI,2                        // Step over "0"
        DEC ECX                          // One character handled
        JMP Hex
      MySQLCharacterSetE3:
        MOV DX,''''                      // End Quoter
        JMP QuotedIdent

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
        JMP UnquotedIdent
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
        JNE UnquotedIdent                // Yes!
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

      QuotedIdent:
        // DX: End Quoter
        ADD ESI,2                        // Step over Start Quoter in SQL
        DEC ECX                          // One character handled
        JZ IncompleteToken               // End of SQL!
      QuotedIdentL:
        MOV AX,[ESI]                     // One Character from SQL to AX
        CMP AX,10                        // <NewLine> ?
        JNE QuotedIdentL2                // No!
        INC Line
      QuotedIdentL2:
        CMP AX,'\'                       // Escaper?
        JNE QuotedIdentLE                // No!
        CMP ECX,0                        // End of SQL?
        JE IncompleteToken               // Yes!
        ADD ESI,2                        // Next character in SQL
        DEC ECX                          // One character handled
        MOV AX,[ESI]                     // One Character from SQL to AX
        CMP AX,DX                        // Escaped End Quoter?
        JE QuotedIdent                   // Yes!
      QuotedIdentLE:
        CMP AX,DX                        // End Quoter (unescaped)?
        JE QuotedIdentE                  // Yes!
        ADD ESI,2                        // One character handled
        LOOP QuotedIdentL
        JMP IncompleteToken
      QuotedIdentE:
        ADD ESI,2                        // Step over End Quoter in SQL
        DEC ECX                          // One character handled
        JZ Finish                        // All characters handled!
        MOV AX,[ESI]                     // One Character from SQL to AX
        CMP AX,DX                        // A seconed End Quoter (unescaped)?
        JNZ Finish                       // No!
        ADD ESI,2                        // Step over second End Quoter in SQL
        LOOP QuotedIdentL                // Handle further characters
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

      UnquotedIdent:
        MOV TokenType,ttIdent
      UnquotedIdentL:
        CALL Separator                   // SQL separator?
        JE Finish                        // Yes!
        CMP AX,32                        // Special character?
        JB UnexpectedChar                // Yes!
        ADD ESI,2                        // Next character in SQL
        MOV AX,[ESI]                     // One Character from SQL to AX
        LOOP UnquotedIdentL
        JMP Finish

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
      UnexpectedCharL:
        MOV AX,[ESI]                     // One Character from SQL to AX
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

    Assert((ErrorCode = PE_Success) or (TokenLength > 0));

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

    ParsePosition.Text := @Text[TokenLength];
    Dec(ParsePosition.Length, TokenLength);
    if (not Error) then
    begin
      Inc(FErrorLine, Line);
      if (ErrorCode <> PE_Success) then
        SetError(ErrorCode, Result);
    end;
  end;
end;

function TMySQLParser.ParseTrimFunc(): TOffset;
var
  Nodes: TTrimFunc.TNodes;
begin
  FillChar(Nodes, SizeOf(Nodes), 0);

  Nodes.FuncToken := ApplyCurrentToken(utFunction);

  if (not Error) then
    if (EndOfStmt(CurrentToken)) then
      SetError(PE_IncompleteStmt)
    else if (TokenPtr(CurrentToken)^.TokenType <> ttOpenBracket) then
      SetError(PE_UnexpectedToken)
    else
      Nodes.OpenBracket := ApplyCurrentToken();

  if (not Error and not EndOfStmt(CurrentToken)) then
    if (TokenPtr(CurrentToken)^.KeywordIndex = kiBOTH) then
      Nodes.DirectionTag := ParseTag(kiBOTH)
    else if (TokenPtr(CurrentToken)^.KeywordIndex = kiLEADING) then
      Nodes.DirectionTag := ParseTag(kiLEADING)
    else if (TokenPtr(CurrentToken)^.KeywordIndex = kiTRAILING) then
      Nodes.DirectionTag := ParseTag(kiTRAILING);

  if (not Error and (Nodes.DirectionTag > 0) and not EndOfStmt(CurrentToken) and (TokenPtr(CurrentToken)^.TokenType in ttStrings)) then
    Nodes.RemoveStr := ParseExpr();

  if (not Error and ((Nodes.DirectionTag > 0) or (Nodes.RemoveStr > 0))) then
    if (EndOfStmt(CurrentToken)) then
      SetError(PE_IncompleteStmt)
    else if (TokenPtr(CurrentToken)^.KeywordIndex <> kiFROM) then
      SetError(PE_UnexpectedToken)
    else
      Nodes.FromTag := ParseTag(kiFROM);

  if (not Error) then
    Nodes.Str := ParseExpr();

  if (not Error and (Nodes.OpenBracket > 0)) then
    if (EndOfStmt(CurrentToken)) then
      SetError(PE_IncompleteStmt)
    else if (TokenPtr(CurrentToken)^.TokenType <> ttCloseBracket) then
      SetError(PE_UnexpectedToken)
    else
      Nodes.CloseBracket := ApplyCurrentToken();

  Result := TTrimFunc.Create(Self, Nodes);
end;

function TMySQLParser.ParseTruncateTableStmt(): TOffset;
var
  Nodes: TTruncateStmt.TNodes;
begin
  FillChar(Nodes, SizeOf(Nodes), 0);

  Nodes.TruncateTag := ParseTag(kiTRUNCATE);

  if (not Error and not EndOfStmt(CurrentToken) and (TokenPtr(CurrentToken)^.KeywordIndex = kiTABLE)) then
    Nodes.TableTag := ParseTag(kiTABLE);

  if (not Error) then
    Nodes.TableIdent := ParseTableIdent();

  Result := TTruncateStmt.Create(Self, Nodes);
end;

function TMySQLParser.ParseUnknownStmt(): TOffset;
var
  Tokens: Classes.TList;
begin
  Tokens := Classes.TList.Create();

  while (not EndOfStmt(CurrentToken)) do
    Tokens.Add(Pointer(ApplyCurrentToken()));

  Result := TUnknownStmt.Create(Self, Tokens.Count, TIntegerArray(Tokens.List));

  Tokens.Free();
end;

function TMySQLParser.ParseUnlockStmt(): TOffset;
var
  Nodes: TUnlockStmt.TNodes;
begin
  FillChar(Nodes, SizeOf(Nodes), 0);

  Nodes.UnlockTablesTag := ParseTag(kiUNLOCK, kiTABLES);

  Result := TUnlockStmt.Create(Self, Nodes);
end;

function TMySQLParser.ParseUpdateStmt(): TOffset;
var
  Nodes: TUpdateStmt.TNodes;
  TableCount: Integer;
begin
  FillChar(Nodes, SizeOf(Nodes), 0);
  TableCount := 0;

  Nodes.UpdateTag := ParseTag(kiUPDATE);

  if (not Error and not EndOfStmt(CurrentToken)) then
    if (TokenPtr(CurrentToken)^.KeywordIndex = kiLOW_PRIORITY) then
      Nodes.PriorityTag := ParseTag(kiLOW_PRIORITY)
    else if (TokenPtr(CurrentToken)^.KeywordIndex = kiCONCURRENT) then
      Nodes.PriorityTag := ParseTag(kiCONCURRENT);

  if (not Error) then
  begin
    Nodes.TableReferenceList := ParseList(False, ParseSelectStmtTableEscapedReference);
    TableCount := PList(NodePtr(Nodes.TableReferenceList))^.ElementCount;
  end;

  if (not Error) then
    if (EndOfStmt(CurrentToken)) then
      SetError(PE_IncompleteStmt)
    else if (TokenPtr(CurrentToken)^.KeywordIndex <> kiSET) then
      SetError(PE_UnexpectedToken)
    else
      Nodes.Set_.Tag := ParseTag(kiSET);

  if (not Error) then
    Nodes.Set_.Pairs := ParseList(False, ParseUpdateStmtValue);

  if (not Error and not EndOfStmt(CurrentToken) and (TokenPtr(CurrentToken)^.KeywordIndex = kiWHERE)) then
  begin
    Nodes.Where.Tag := ParseTag(kiWHERE);

    if (not Error) then
      if (EndOfStmt(CurrentToken)) then
        SetError(PE_IncompleteStmt)
      else
        Nodes.Where.Expr := ParseExpr();
  end;

  if (not Error and (TableCount = 1)) then
  begin
    if (not Error and not EndOfStmt(CurrentToken) and (TokenPtr(CurrentToken)^.KeywordIndex = kiORDER)) then
    begin
      Nodes.OrderBy.Tag := ParseTag(kiORDER, kiBY);

      if (not Error) then
        if (EndOfStmt(CurrentToken)) then
          SetError(PE_IncompleteStmt)
        else
          Nodes.OrderBy.Expr := ParseList(False, ParseSelectStmtOrder);
    end;

    if (not Error and not EndOfStmt(CurrentToken) and (TokenPtr(CurrentToken)^.KeywordIndex = kiLIMIT)) then
    begin
      Nodes.Limit.Tag := ParseTag(kiLIMIT);

      if (not Error) then
        if (EndOfStmt(CurrentToken)) then
          SetError(PE_IncompleteStmt)
        else
          Nodes.Limit.Expr := ParseExpr();
    end;
  end;

  Result := TUpdateStmt.Create(Self, Nodes);
end;

function TMySQLParser.ParseUpdateStmtValue(): TOffset;
var
  Nodes: TValue.TNodes;
begin
  FillChar(Nodes, SizeOf(Nodes), 0);

  Nodes.IdentTag := ParseFieldIdent();

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
    if (not EndOfStmt(CurrentToken) and (TokenPtr(CurrentToken)^.KeywordIndex = kiDEFAULT)) then
      Nodes.Expr := ApplyCurrentToken()
    else
      Nodes.Expr := ParseExpr();

  Result := TValue.Create(Self, Nodes);
end;

function TMySQLParser.ParseUserIdent(): TOffset;
var
  Nodes: TUser.TNodes;
begin
  Result := 0;

  if (EndOfStmt(CurrentToken)) then
    SetError(PE_IncompleteStmt)
  else if (TokenPtr(CurrentToken)^.KeywordIndex = kiCURRENT_USER) then
    if (((NextToken[1] = 0) or (TokenPtr(NextToken[1])^.TokenType <> ttOpenBracket))
      and ((NextToken[2] = 0) or (TokenPtr(NextToken[2])^.TokenType <> ttCloseBracket))) then
      Result := ApplyCurrentToken()
    else
      Result := ParseFunctionCall()
  else if (not (TokenPtr(CurrentToken)^.TokenType in ttIdents) and not (TokenPtr(CurrentToken)^.TokenType in ttStrings)) then
    SetError(PE_UnexpectedToken)
  else
  begin
    FillChar(Nodes, SizeOf(Nodes), 0);

    Nodes.NameToken := ApplyCurrentToken(utDbIdent);

    if (not Error and not EndOfStmt(CurrentToken) and (TokenPtr(CurrentToken)^.TokenType = ttAt)) then
    begin
      Nodes.AtToken := ApplyCurrentToken();

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
  end;
end;

function TMySQLParser.ParseUseStmt(): TOffset;
var
  Nodes: TUseStmt.TNodes;
begin
  FillChar(Nodes, SizeOf(Nodes), 0);

  Nodes.StmtToken := ParseTag(kiUSE);

  if (not Error) then
    if (EndOfStmt(CurrentToken)) then
      SetError(PE_IncompleteStmt)
    else if (not (TokenPtr(CurrentToken)^.TokenType in ttIdents)) then
      SetError(PE_UnexpectedToken)
    else
      Nodes.DbNameNode := ParseDatabaseIdent();

  Result := TUseStmt.Create(Self, Nodes);
end;

function TMySQLParser.ParseValue(const KeywordIndex: TWordList.TIndex;
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

function TMySQLParser.ParseValue(const KeywordIndex: TWordList.TIndex;
  const Assign: TValueAssign; const OptionIndices: TWordList.TIndices): TOffset;
var
  CurrentKeywordIndex: Integer;
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
    CurrentKeywordIndex := TokenPtr(CurrentToken)^.KeywordIndex;
    for I := 0 to Length(OptionIndices) - 1 do
      if ((OptionIndices[I] < 0)) then
        break
      else if (OptionIndices[I] = CurrentKeywordIndex) then
      begin
        Nodes.Expr := ParseTag(CurrentKeywordIndex);
        break;
      end;
    if (Nodes.Expr = 0) then
      SetError(PE_UnexpectedToken);
  end;

  Result := TValue.Create(Self, Nodes);
end;

function TMySQLParser.ParseValue(const KeywordIndex: TWordList.TIndex;
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

function TMySQLParser.ParseValue(const KeywordIndices: TWordList.TIndices;
  const Assign: TValueAssign; const Brackets: Boolean;
  const ParseItem: TParseFunction): TOffset;
var
  Nodes: TValue.TNodes;
begin
  Assert(KeywordIndices[4] = -1);

  FillChar(Nodes, SizeOf(Nodes), 0);

  Nodes.IdentTag := ParseTag(KeywordIndices[0], KeywordIndices[1], KeywordIndices[2], KeywordIndices[3]);

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

function TMySQLParser.ParseValue(const KeywordIndices: TWordList.TIndices; const Assign: TValueAssign; const ParseValueNode: TParseFunction): TOffset;
var
  Nodes: TValue.TNodes;
begin
  Assert(KeywordIndices[4] = -1);

  FillChar(Nodes, SizeOf(Nodes), 0);

  Nodes.IdentTag := ParseTag(KeywordIndices[0], KeywordIndices[1], KeywordIndices[2], KeywordIndices[3]);

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
    if (EndOfStmt(CurrentToken)) then
      SetError(PE_IncompleteStmt)
    else
      Nodes.Expr := ParseValueNode();

  Result := TValue.Create(Self, Nodes);
end;

function TMySQLParser.ParseValue(const KeywordIndices: TWordList.TIndices; const Assign: TValueAssign; const ValueKeywordIndex1: TWordList.TIndex; const ValueKeywordIndex2: TWordList.TIndex = -1): TOffset;
var
  Nodes: TValue.TNodes;
begin
  Assert(KeywordIndices[4] = -1);

  FillChar(Nodes, SizeOf(Nodes), 0);

  Nodes.IdentTag := ParseTag(KeywordIndices[0], KeywordIndices[1], KeywordIndices[2], KeywordIndices[3]);

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

function TMySQLParser.ParseVariable(): TOffset;
var
  Nodes: TVariable.TNodes;
begin
  FillChar(Nodes, SizeOf(Nodes), 0);

  if (not EndOfStmt(CurrentToken) and (TokenPtr(CurrentToken)^.TokenType = ttAt)) then
    Nodes.At1Token := ApplyCurrentToken();

  if (not Error and (Nodes.At1Token > 0)
    and not EndOfStmt(CurrentToken) and (TokenPtr(CurrentToken)^.TokenType = ttAt)) then
  begin
    Nodes.At2Token := Nodes.At1Token;
    Nodes.At1Token := ApplyCurrentToken();
  end;

  if (not Error and not EndOfStmt(CurrentToken)
    and (Nodes.At1Token > 0)
    and ((TokenPtr(CurrentToken)^.KeywordIndex = kiGLOBAL) or (TokenPtr(CurrentToken)^.KeywordIndex = kiSESSION) or (TokenPtr(CurrentToken)^.KeywordIndex = kiLOCAL))) then
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

function TMySQLParser.ParseWeightStringFunc(): TOffset;
var
  Nodes: TWeightStringFunc.TNodes;
begin
  FillChar(Nodes, SizeOf(Nodes), 0);

  Nodes.FuncToken := ApplyCurrentToken(utFunction);

  if (not Error) then
    if (EndOfStmt(CurrentToken)) then
      SetError(PE_IncompleteStmt)
    else if (TokenPtr(CurrentToken)^.TokenType <> ttOpenBracket) then
      SetError(PE_UnexpectedToken)
    else
      Nodes.OpenBracket := ApplyCurrentToken();

  if (not Error) then
    if (EndOfStmt(CurrentToken)) then
      SetError(PE_IncompleteStmt)
    else if (TokenPtr(CurrentToken)^.TokenType <> ttString) then
      SetError(PE_UnexpectedToken)
    else
      Nodes.Str := ParseString();

  if (not Error and not EndOfStmt(CurrentToken) and (TokenPtr(CurrentToken)^.KeywordIndex <> kiAS)) then
  begin
    Nodes.AsTag := ParseTag(kiAS);

    if (not Error) then
      if (EndOfStmt(CurrentToken)) then
        SetError(PE_IncompleteStmt)
      else
        Nodes.Datatype := ParseDatatype();
  end;

  if (not Error and not EndOfStmt(CurrentToken) and (TokenPtr(CurrentToken)^.KeywordIndex <> kiLEVEL)) then
  begin
    Nodes.AsTag := ParseTag(kiLEVEL);

    if (not Error) then
      Nodes.LevelList := ParseList(False, ParseWeightStringFuncLevel);
  end;

  if (not Error) then
    if (EndOfStmt(CurrentToken)) then
      SetError(PE_IncompleteStmt)
    else if (TokenPtr(CurrentToken)^.TokenType <> ttCloseBracket) then
      SetError(PE_UnexpectedToken)
    else
      Nodes.CloseBracket := ApplyCurrentToken();

  Result := TWeightStringFunc.Create(Self, Nodes);
end;

function TMySQLParser.ParseWeightStringFuncLevel(): TOffset;
var
  Nodes: TWeightStringFunc.TLevel.TNodes;
begin
  FillChar(Nodes, SizeOf(Nodes), 0);

  Nodes.CountInt := ParseInteger();

  if (not Error and not EndOfStmt(CurrentToken)) then
    if (TokenPtr(CurrentToken)^.KeywordIndex = kiASC) then
      Nodes.DirectionTag := ParseTag(kiASC)
    else if (TokenPtr(CurrentToken)^.KeywordIndex = kiDESC) then
      Nodes.DirectionTag := ParseTag(kiDESC)
    else if (TokenPtr(CurrentToken)^.KeywordIndex = kiREVERSE) then
      Nodes.DirectionTag := ParseTag(kiREVERSE);

  Result := TWeightStringFunc.TLevel.Create(Self, Nodes);
end;

function TMySQLParser.ParseWhileStmt(): TOffset;
var
  BeginLabelToken: TOffset;
  Nodes: TWhileStmt.TNodes;
begin
  FillChar(Nodes, SizeOf(Nodes), 0);
  BeginLabelToken := 0;

  if (not EndOfStmt(CurrentToken) and (TokenPtr(CurrentToken)^.TokenType = ttIdent)
    and not EndOfStmt(NextToken[1]) and (TokenPtr(NextToken[1])^.TokenType = ttColon)) then
  begin
    BeginLabelToken := CurrentToken;
    Nodes.BeginLabel := ParseBeginLabel();
  end;

  if (not Error) then
    Nodes.WhileTag := ParseTag(kiWHILE);

  if (not Error) then
    Nodes.SearchConditionExpr := ParseExpr();

  if (not Error) then
    Nodes.DoTag := ParseTag(kiDO);

  if (not Error and not EndOfStmt(CurrentToken) and (TokenPtr(CurrentToken)^.KeywordIndex <> kiEND)) then
    Nodes.StmtList := ParseList(False, ParsePL_SQLStmt, ttDelimiter);

  if (not Error) then
    Nodes.EndTag := ParseTag(kiEND, kiWHILE);

  if (not Error and not EndOfStmt(CurrentToken) and (TokenPtr(CurrentToken)^.TokenType = ttIdent)) then
    if ((Nodes.BeginLabel = 0) or (StrIComp(PChar(TokenPtr(CurrentToken)^.AsString), PChar(TokenPtr(BeginLabelToken)^.AsString)) <> 0)) then
      SetError(PE_UnexpectedToken)
    else
      Nodes.EndLabel := ParseEndLabel();

  Result := TWhileStmt.Create(Self, Nodes);
end;

function TMySQLParser.ParseXAStmt(): TOffset;

  function ParseXID(): TOffset;
  var
    Nodes: TXAStmt.TID.TNodes;
  begin
    FillChar(Nodes, SizeOf(Nodes), 0);

    Nodes.GTrId := ParseString();

    if (not Error and not EndOfStmt(CurrentToken) and (TokenPtr(CurrentToken)^.TokenType = ttComma)) then
    begin
      Nodes.Comma1 := ApplyCurrentToken();

      if (not Error) then
        if (EndOfStmt(CurrentToken)) then
          SetError(PE_IncompleteStmt)
          else if (TokenPtr(CurrentToken)^.TokenType <> ttString) then
          SetError(PE_UnexpectedToken)
        else
          Nodes.BQual := ParseString();

      if (not Error and not EndOfStmt(CurrentToken) and (TokenPtr(CurrentToken)^.TokenType = ttComma)) then
      begin
        Nodes.Comma1 := ApplyCurrentToken();

        if (not Error) then
          if (EndOfStmt(CurrentToken)) then
            SetError(PE_IncompleteStmt)
          else if (TokenPtr(CurrentToken)^.TokenType <> ttInteger) then
            SetError(PE_UnexpectedToken)
          else
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
    if (EndOfStmt(CurrentToken)) then
      SetError(PE_IncompleteStmt)
    else if ((TokenPtr(CurrentToken)^.KeywordIndex = kiBEGIN) or (TokenPtr(CurrentToken)^.KeywordIndex = kiSTART)) then
    begin
      Nodes.ActionTag := ParseTag(TokenPtr(CurrentToken)^.KeywordIndex);

      if (not Error) then
        if (EndOfStmt(CurrentToken)) then
          SetError(PE_IncompleteStmt)
        else if (TokenPtr(CurrentToken)^.TokenType <> ttString) then
          SetError(PE_UnexpectedToken)
        else
          Nodes.Ident := ParseXID();

      if (not Error and not EndOfStmt(CurrentToken) and ((TokenPtr(CurrentToken)^.KeywordIndex = kiJOIN) or (TokenPtr(CurrentToken)^.KeywordIndex = kiRESUME))) then
        Nodes.RestTag := ParseTag(TokenPtr(CurrentToken)^.KeywordIndex);
    end
    else if (TokenPtr(CurrentToken)^.KeywordIndex = kiCOMMIT) then
    begin
      Nodes.ActionTag := ParseTag(kiCOMMIT);

      if (not Error) then
        if (EndOfStmt(CurrentToken)) then
          SetError(PE_IncompleteStmt)
        else if (TokenPtr(CurrentToken)^.TokenType <> ttString) then
          SetError(PE_UnexpectedToken)
        else
          Nodes.Ident := ParseXID();

      if (not Error and not EndOfStmt(CurrentToken) and (TokenPtr(CurrentToken)^.KeywordIndex = kiONE)) then
        Nodes.RestTag := ParseTag(kiONE, kiPHASE);
    end
    else if (TokenPtr(CurrentToken)^.KeywordIndex = kiEND) then
    begin
      Nodes.ActionTag := ParseTag(kiEND);

      if (not Error) then
        if (EndOfStmt(CurrentToken)) then
          SetError(PE_IncompleteStmt)
        else if (TokenPtr(CurrentToken)^.TokenType <> ttString) then
          SetError(PE_UnexpectedToken)
        else
          Nodes.Ident := ParseXID();

      if (not Error and not EndOfStmt(CurrentToken) and (TokenPtr(CurrentToken)^.KeywordIndex = kiSUSPEND)) then
        if (not EndOfStmt(NextToken[1]) and (TokenPtr(NextToken[1])^.KeywordIndex = kiFOR)) then
          Nodes.RestTag := ParseTag(kiSUSPEND, kiFOR, kiMIGRATE)
        else
          Nodes.RestTag := ParseTag(kiSUSPEND);
    end
    else if (TokenPtr(CurrentToken)^.KeywordIndex = kiPREPARE) then
    begin
      Nodes.ActionTag := ParseTag(kiPREPARE);

      if (not Error) then
        if (EndOfStmt(CurrentToken)) then
          SetError(PE_IncompleteStmt)
        else if (TokenPtr(CurrentToken)^.TokenType <> ttString) then
          SetError(PE_UnexpectedToken)
        else
          Nodes.Ident := ParseXID();
    end
    else if (TokenPtr(CurrentToken)^.KeywordIndex = kiRECOVER) then
    begin
      Nodes.ActionTag := ParseTag(kiRECOVER);

      if (not Error and not EndOfStmt(CurrentToken) and (TokenPtr(CurrentToken)^.KeywordIndex = kiCONVERT)) then
        Nodes.RestTag := ParseTag(kiCONVERT, kiXID);
    end
    else if (TokenPtr(CurrentToken)^.KeywordIndex = kiROLLBACK) then
    begin
      Nodes.ActionTag := ParseTag(kiROLLBACK);

      if (not Error) then
        if (EndOfStmt(CurrentToken)) then
          SetError(PE_IncompleteStmt)
        else if (TokenPtr(CurrentToken)^.TokenType <> ttString) then
          SetError(PE_UnexpectedToken)
        else
          Nodes.Ident := ParseXID();
    end
    else
      SetError(PE_UnexpectedToken);

  Result := TXAStmt.Create(Self, Nodes);
end;

function TMySQLParser.RangePtr(const ANode: TOffset): PRange;
begin
  Assert(IsRange(NodePtr(ANode)));

  Result := PRange(NodePtr(ANode));
end;

procedure TMySQLParser.SaveToFile(const Filename: string; const FileType: TFileType = ftSQL);
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

procedure TMySQLParser.SaveToDebugHTMLFile(const Filename: string);
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
          + '<div class="ErrorMessage">' + HTMLEscape('Error: ' + Stmt^.ErrorMessage) + '</div>';

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

procedure TMySQLParser.SaveToFormatedSQLFile(const Filename: string);
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

procedure TMySQLParser.SaveToSQLFile(const Filename: string);
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

procedure TMySQLParser.SetDatatypes(ADatatypes: string);

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
    diLARGEINT             := IndexOf('LARGEINT');
    diLINESTRING           := IndexOf('LINESTRING');
    diJSON                 := IndexOf('JSON');
    diLONG                 := IndexOf('LONG');
    diLONGBLOB             := IndexOf('LONGBLOB');
    diLONGTEXT             := IndexOf('LONGTEXT');
    diMEDIUMBLOB           := IndexOf('MEDIUMBLOB');
    diMEDIUMINT            := IndexOf('MEDIUMINT');
    diMEDIUMTEXT           := IndexOf('MEDIUMTEXT');
    diMULTILINESTRING      := IndexOf('MULTILINESTRING');
    diMULTIPOINT           := IndexOf('MULTIPOINT');
    diMULTIPOLYGON         := IndexOf('MULTIPOLYGON');
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

procedure TMySQLParser.SetError(const AErrorCode: Byte; const AErrorToken: TOffset = 0);
begin
  Assert(not Error and ((AErrorCode <> PE_IncompleteStmt) or (AErrorToken = 0) or IsToken(AErrorToken)));

  FErrorCode := AErrorCode;

  if (not IsChild(AErrorToken)) then
    FErrorToken := CurrentToken
  else
    FErrorToken := ChildPtr(AErrorToken)^.FFirstToken;
end;

procedure TMySQLParser.SetFunctions(AFunctions: string);
begin
  FunctionList.Text := AFunctions;
end;

procedure TMySQLParser.SetKeywords(AKeywords: string);

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
    kiDAY                      := IndexOf('DAY');
    kiDAY_HOUR                 := IndexOf('DAY_HOUR');
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
    kiHOUR_MINUTE              := IndexOf('HOUR_MINUTE');
    kiHOUR_SECOND              := IndexOf('HOUR_SECOND');
    kiIDENTIFIED               := IndexOf('IDENTIFIED');
    kiIF                       := IndexOf('IF');
    kiIGNORE                   := IndexOf('IGNORE');
    kiIMPORT                   := IndexOf('IMPORT');
    kiIN                       := IndexOf('IN');
    kiINDEX                    := IndexOf('INDEX');
    kiINDEXES                  := IndexOf('INDEXES');
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
    kiVALUE                    := IndexOf('VALUE');
    kiVALUES                   := IndexOf('VALUES');
    kiVARIABLES                := IndexOf('VARIABLES');
    kiVIEW                     := IndexOf('VIEW');
    kiVIRTUAL                  := IndexOf('VIRTUAL');
    kiWARNINGS                 := IndexOf('WARNINGS');
    kiWEEK                     := IndexOf('WEEK');
    kiWHEN                     := IndexOf('WHEN');
    kiWHERE                    := IndexOf('WHERE');
    kiWHILE                    := IndexOf('WHILE');
    kiWRAPPER                  := IndexOf('WRAPPER');
    kiWITH                     := IndexOf('WITH');
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

function TMySQLParser.StmtPtr(const Node: TOffset): PStmt;
begin
  Assert((Node = 0) or IsStmt(Node));

  if (not IsStmt(Node)) then
    Result := nil
  else
    Result := @ParsedNodes.Mem[Node];
end;

function TMySQLParser.TokenPtr(const Token: TOffset): PToken;
begin
  Assert((Token = 0) or IsToken(Token));

  if (Token = 0) then
    Result := nil
  else
    Result := PToken(@ParsedNodes.Mem[Token]);
end;

{$IFDEF Debug}
var
  Max: Integer;
  OperatorType: TMySQLParser.TOperatorType;
{$ENDIF}
initialization
  {$IFDEF Debug}
    Max := 0;
    for OperatorType := Low(TMySQLParser.TOperatorType) to High(TMySQLParser.TOperatorType) do
      if (TMySQLParser.OperatorPrecedenceByOperatorType[OperatorType] > Max) then
        Max := TMySQLParser.OperatorPrecedenceByOperatorType[OperatorType];
    Assert(Max = TMySQLParser.MaxOperatorPrecedence);
  {$ENDIF}
end.

