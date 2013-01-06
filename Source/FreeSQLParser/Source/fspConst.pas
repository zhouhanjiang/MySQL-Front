unit fspConst;

interface {********************************************************************}

uses
  fspTypes;

const
  PE_Success = 0;

  // Bugs while parsing Tokens:
  PE_Unknown = 1; // Unknown error
  PE_EmptyText = 2; // Text is empty

  // Bugs while parsing Tokens:
  PE_Syntax = 3; // Invalid or unexpected character
  PE_IncompleteToken = 4; // Uncompleted Token

  // Bugs while parsing Stmts:
  PE_UnexpectedToken = 5; // Token unexpected or not understood
  PE_UnkownStmt = 6; // First Token is not a known keyword
  PE_IncompleteStmt = 7; // Uncompleted Token
  PE_InvalidEndLabel = 8; // Begin and End Token are different

  MySQLFunctions =
    'BINARY,INTERVAL,' +
    'ABS,ACOS,ADD,ADDDATE,ADDTIME,AES_DECRYPT,AES_ENCRYPT,ANALYSE,ASCII,ASIN,' +
    'ATAN,ATAN2,AVG,BIN,BIT_AND,BIT_COUNT,BIT_LENGTH,BIT_OR,BIT_XOR,' +
    'CAST,CEIL,CEILING,CHAR_LENGTH,CHARACTER_LENGTH,COALESCE,COERCIBILITY,' +
    'COMPRESS,CONCAT,CONCAT_WS,CONNECTION_ID,CONV,CONVERT_TZ,COS,COT,COUNT,' +
    'CRC32,CURDATE,CURRENT_DATE,CURRENT_TIME,CURRENT_TIMESTAMP,CURRENT_USER,' +
    'CURTIME,DATE_ADD,DATE_FORMAT,DATE_SUB,DATEDIFF,DAY,DAY_HOUR,DAY_MINUTE,' +
    'DAY_SECOND,DAYNAME,DAYOFMONTH,DAYOFWEEK,DAYOFYEAR,DECODE,DEGREES,ELT,' +
    'ENCODE,ENCRYPT,EXP,EXPORT_SET,EXTRACT,EXTRACTVALUE,FIELD,FIND_IN_SET,' +
    'FLOOR,FORMAT,FOUND_ROWS,FROM_DAYS,FROM_UNIXTIME,GET_FORMAT,GET_LOCK,' +
    'GREATEST,GROUP_CONCAT,HEX,HOUR,HOUR_MINUTE,HOUR_SECOND,IFNULL,IN,' +
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
    'WEEKDAY,WEEKOFYEAR,YEAR_MONTH,YEARWEEK,';

  MySQLKeywords =
    'BINARY,IN,INTERVAL,' +
    'ACTION,AFTER,AGAINST,AGGREGATE,ALGORITHM,ALL,ALTER,ANALYZE,AND,ANY,AS,' +
    'ASC,AT,AUTHORS,AUTO_INCREMENT,AUTOEXTEND_SIZE,AVG_ROW_LENGTH,BACKUP,' +
    'BEFORE,BEGIN,BENCHMARK,BETWEEN,BINLOG,BIT,BOTH,BY,CACHE,CALL,CASCADE,' +
    'CASCADED,CASE,CATALOG_NAME,CHAIN,CHANGE,CHANGED,CHARACTER,CHARSET,CHECK,' +
    'CHECKSUM,CLASS_ORIGIN,CLIENT,CLOSE,CODE,COLLATE,COLLATION,COLUMN,' +
    'COLUMN_FORMAT,COLUMN_NAME,COLUMNS,COMMENT,COMMIT,COMMITTED,COMPLETION,' +
    'CONCURRENT,CONDITION,CONNECTION,CONSISTENT,CONSTRAINT,REPEAT,' +
    'CONSTRAINT_CATALOG,CONSTRAINT_NAME,CONSTRAINT_SCHEMA,CONTAINS,CONTENTS,' +
    'CONTINUE,CONTRIBUTORS,CONVERT,CREATE,CROSS,CURSOR,CURSOR_NAME,DATA,' +
    'DATABASE,DATABASES,DATAFILE,DEALLOCATE,DEC,DECLARE,DEFAULT,DEFINER,' +
    'DELAY_KEY_WRITE,DELAYED,DELETE,DESC,DESCRIBE,DETERMINISTIC,DIRECTORY,' +
    'DISABLE,DISCARD,DISTINCT,DISTINCTROW,DIV,DO,DROP,DUAL,DUMPFILE,' +
    'DUPLICATE,EACH,ELSE,ELSEIF,ENABLE,ENCLOSED,END,ENDIF,ENDS,ENGINE,' +
    'ENGINES,ERRORS,ESCAPE,ESCAPED,EVENT,EVENTS,EVERY,EXECUTE,EXISTS,EXIT,' +
    'EXPANSION,EXPLAIN,EXTENDED,EXTENT_SIZE,FALSE,FAST,FETCH,FIELDS,FILE,' +
    'FIRST,FLUSH,FOR,FORCE,FOREIGN,FOUND,FROM,FULL,FULLTEXT,FUNCTION,' +
    'FUNCTIONS,GLOBAL,GOTO,GRANT,GRANTS,GROUP,HANDLER,HAVING,HELP,' +
    'HIGH_PRIORITY,HOST,HOSTS,IDENTIFIED,IF,IGNORE,IGNORE_SERVER_IDS,IMPORT,' +
    'INDEX,INFILE,INITIAL_SIZE,INNER,INSERT,INSERT_METHOD,INSTALL,INT1,INT2,' +
    'INT3,INT4,INT8,INTO,INVOKER,IO_THREAD,IS,ISOLATION,ISSUER,ITERATE,JOIN,' +
    'KEY,KEY_BLOCK_SIZE,KEYS,KILL,LANGUAGE,LAST,LEADING,LEAVE,LEAVES,LESS,' +
    'LEVEL,LIKE,LIMIT,LINEAR,LINES,LIST,LOAD,LOCAL,LOCK,LOGFILE,LOGS,LONG,' +
    'LOOP,LOW_PRIORITY,MASTER,MASTER_BIND,MASTER_CONNECT_RETRY,' +
    'MASTER_HEARTBEAT_PERIOD,MASTER_HOST,MASTER_LOG_FILE,MASTER_LOG_POS,' +
    'MASTER_PASSWORD,MASTER_PORT,MASTER_SSL,MASTER_SSL_CA,MASTER_SSL_CAPATH,' +
    'MASTER_SSL_CERT,MASTER_SSL_CIPHER,MASTER_SSL_KEY,' +
    'MASTER_SSL_VERIFY_SERVER_CERT,MASTER_USER,MATCH,' +
    'MAX_CONNECTIONS_PER_HOUR,MAX_QUERIES_PER_HOUR,MAX_ROWS,MAX_SIZE,' +
    'MAX_UPDATES_PER_HOUR,MAX_USER_CONNECTIONS,MAXVALUE,MEDIUM,MESSAGE_TEXT,' +
    'MIDDLEINT,MIN_ROWS,MOD,MODE,MODIFIES,MODIFY,MUTEX,MYSQL_ERRNO,NAME,' +
    'NAMES,NATURAL,NEW,NEXT,NO,NO_WRITE_TO_BINLOG,NODEGROUP,NONE,NOT,NULL,' +
    'OFFLINE,OFFSET,OJ,OLD,ON,ONLINE,OPEN,OPTIMIZE,OPTION,OPTIONALLY,' +
    'OPTIONS,OR,ORDER,OUTER,OUTFILE,OWNER,PACK_KEYS,PARSER,PARTIAL,' +
    'PARTITION,PARTITIONING,PARTITIONS,PLUGIN,PLUGINS,PORT,PREPARE,PRESERVE,' +
    'PREV,PRIMARY,PRIVILEGES,PROCEDURE,PROCESS,PROCESSLIST,PROFILE,PROFILES,' +
    'PROXY,PURGE,QUERY,QUICK,RAID_CHUNKS,RAID_CHUNKSIZE,RAID_TYPE,RANGE,' +
    'READ,REBUILD,REDO_BUFFER_SIZE,REFERENCES,REGEXP,RELAY_LOG_FILE,' +
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
    'ntRangeNode',
    'ntColumns',
    'ntColumn',
    'ntDbIdentifier',
    'ntUnaryOperation',
    'ntBinaryOperation',
    'ntBetweenOperation',
    'ntSoundsLikeOperation',
    'ntStmt',
    'ntSelectStmt'
  );

  StmtTypeToString: array[TStmtType] of PChar = (
    'stUnknown',
    'stSELECT',
    'stCompound'
  );

  TokenTypeToString: array[TTokenType] of PChar = (
    'ttUnknown',
    'ttSpace',
    'ttReturn',
    'ttComment',
    'ttComma',
    'ttOpenBracket',
    'ttCloseBracket',
    'ttDelimiter',
    'ttInteger',
    'ttNumeric',
    'ttString',
    'ttIdentifier',
    'ttBeginLabel',
    'ttEndLabel',
    'ttVariable',
    'ttBindVariable',
    'ttDQIdentifier',
    'ttDBIdentifier',
    'ttBRIdentifier',
    'ttMySQLIdentifier',
    'ttMySQLCodeStart',
    'ttMySQLCodeEnd',
    'ttMySQLCharacterSet',
    'ttOperator',
    'ttAt',
    'ttBackslash',
    'ttKeyword'
  );

  UsageTypeToString: array[TUsageType] of PChar = (
    'utUnknown',
    'utWhiteSpace',
    'utComment',
    'utSymbol',
    'utKeyword',
    'utLabel',
    'utOperator',
    'utSign',
    'utConst',
    'utVariable',
    'utFunction',
    'utDbIdentifier',
    'utAlias'
  );

  OperatorTypeToString: array[TOperatorType] of PChar = (
    'otUnknown',

    'otFunction',
    'otInterval',
    'otBinary',
    'otCollate',

    'otUnaryMinus',
    'otUnaryPlus',
    'otNot1',
    'otDot',

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
    'otTHEN',
    'otELSE',

    'otNot2',

    'otAnd',

    'otXOr',

    'otPipes',
    'otOr',


    'otAssignment',
    'otAssign',
    'otLeftJoin',
    'otRightJoin',
    'otHat',
    'otDoubleDot',
    'otArrow',
    'otParameter'
  );

  DbIdentifierTypeToString: array[TDbIdentifierType] of PChar = (
    'ditUnknown',
    'ditTable',
    'ditField',
    'ditFunction',
    'ditProcedure',
    'ditTrigger',
    'ditView',
    'ditIndex',
    'ditDatabase',
    'ditParameter',
    'ditLocalVariable',
    'ditEvent'
  );

  OperatorPrecedenceByOperatorType: array[TOperatorType] of Integer = (
    0,   // otUnknown

    1,   // otFunction
    1,   // otInterval
    1,   // otBinary
    1,   // otCollate

    2,   // otNot1
         // otNot2, if Parser.HighNotPrecedence

    3,   // otUnaryMinus
    3,   // otUnaryPlus
    3,   // otInvertBits
    3,   // otDot

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
    12,  // otTHEN
    12,  // otELSE

    13,  // otNot2, if not Parser.HighNotPrecedence

    14,  // otAnd

    15,  // otXOr

    16,  // otPipes, if not Parser.PipesAsConcat
    16,  // otOr


    0,  // otAssignment
    0,  // otAssign
    0,  // otLeftJoin
    0,  // otRightJoin
    0,  // otHat
    0,  // otDoubleDot
    0,  // otArrow
    0   // otParameter
  );
  MaxOperatorPrecedence = 16;

implementation {***************************************************************}

end.
