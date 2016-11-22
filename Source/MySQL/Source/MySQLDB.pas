unit MySQLDB;

interface {********************************************************************}

uses
  Classes, SysUtils, Windows, SyncObjs,
  DB, DBCommon, SqlTimSt,
  MySQLConsts;

const
  DS_ASYNCHRON     = -1;
  DS_MIN_ERROR     = 2300;
  DS_SET_NAMES     = 2300;
  DS_SERVER_OLD    = 2301;
  DS_OUT_OF_MEMORY = 2302;

type
  TMySQLMonitor = class;
  TMySQLConnection = class;                      
  TMySQLQuery = class;
  TMySQLDataSet = class;

  TMySQLLibrary = class
  type
    TLibraryType = (ltBuiltIn, ltDLL, ltHTTP);
  private
    FHandle: HModule;
    FLibraryType: TLibraryType;
    FFilename: TFileName;
    FVersion: Integer;
    function GetVersionStr(): string;
  public
    my_init: Tmy_init;
    mysql_affected_rows: Tmysql_affected_rows;
    mysql_character_set_name: Tmysql_character_set_name;
    mysql_close: Tmysql_close;
    mysql_errno: Tmysql_errno;
    mysql_error: Tmysql_error;
    mysql_fetch_field: Tmysql_fetch_field;
    mysql_fetch_field_direct: Tmysql_fetch_field_direct;
    mysql_fetch_fields: Tmysql_fetch_fields;
    mysql_fetch_lengths: Tmysql_fetch_lengths;
    mysql_fetch_row: Tmysql_fetch_row;
    mysql_field_count: Tmysql_field_count;
    mysql_free_result: Tmysql_free_result;
    mysql_get_client_info: Tmysql_get_client_info;
    mysql_get_client_version: Tmysql_get_client_version;
    mysql_get_host_info: Tmysql_get_host_info;
    mysql_get_server_info: Tmysql_get_server_info;
    mysql_get_server_version: Tmysql_get_server_version;
    mysql_info: Tmysql_info;
    mysql_init: Tmysql_init;
    mysql_insert_id: Tmysql_insert_id;
    mysql_library_end: Tmysql_library_end;
    mysql_library_init: Tmysql_library_init;
    mysql_more_results: Tmysql_more_results;
    mysql_next_result: Tmysql_next_result;
    mysql_num_fields: Tmysql_num_fields;
    mysql_num_rows: Tmysql_num_rows;
    mysql_options: Tmysql_options;
    mysql_ping: Tmysql_ping;
    mysql_real_connect: Tmysql_real_connect;
    mysql_real_escape_string: Tmysql_real_escape_string;
    mysql_real_query: Tmysql_real_query;
    mysql_session_track_get_first: Tmysql_session_track_get_first;
    mysql_session_track_get_next: Tmysql_session_track_get_next;
    mysql_set_character_set: Tmysql_set_character_set;
    mysql_set_local_infile_default: Tmysql_set_local_infile_default;
    mysql_set_local_infile_handler: Tmysql_set_local_infile_handler;
    mysql_set_server_option: Tmysql_set_server_option;
    mysql_shutdown: Tmysql_shutdown;
    mysql_store_result: Tmysql_store_result;
    mysql_thread_end: Tmysql_thread_end;
    mysql_thread_id: Tmysql_thread_id;
    mysql_thread_init: Tmysql_thread_init;
    mysql_thread_save: Tmysql_thread_save;
    mysql_use_result: Tmysql_use_result;
    mysql_warning_count: Tmysql_warning_count;
    constructor Create(const ALibraryType: TLibraryType; const AFilename: TFileName); virtual;
    destructor Destroy(); override;
    procedure SetField(const RawField: MYSQL_FIELD; out Field: TMYSQL_FIELD); inline;
    property Filename: TFileName read FFilename;
    property Handle: HModule read FHandle;
    property LibraryType: TLibraryType read FLibraryType;
    property Version: Integer read FVersion;
    property VersionStr: string read GetVersionStr;
  end;

  EDatabasePostError = class(EDatabaseError);

  EMySQLError = class(EDatabaseError)
  protected
    FConnection: TMySQLConnection;
    FErrorCode: Integer;
  public
    constructor Create(const Msg: string; const AErrorCode: Integer; const AConnection: TMySQLConnection); virtual;
    property Connection: TMySQLConnection read FConnection;
    property ErrorCode: Integer read FErrorCode;
  end;

  TMySQLMonitor = class(TComponent)
  type
    TCache = record
      First: Integer;
      Items: TList;
      MaxSize: Integer;
      Mem: PChar;
      MemSize: Integer;
      UsedSize: Integer;
    end;
    TTraceType = (ttTime, ttRequest, ttResult, ttInfo, ttDebug);
    TTraceTypes = set of TTraceType;
    TMySQLOnMonitor = procedure (const Connection: TMySQLConnection; const Text: PChar; const Length: Integer; const ATraceType: TTraceType) of object;
  private
    Cache: TCache;
    FCacheSize: Integer;
    FConnection: TMySQLConnection;
    FEnabled: Boolean;
    FOnMonitor: TMySQLOnMonitor;
    FTraceTypes: TTraceTypes;
    function GetCacheText(): string;
    procedure SetConnection(const AConnection: TMySQLConnection);
    procedure SetCacheSize(const ACacheSize: Integer);
    procedure SetOnMonitor(const AOnMonitor: TMySQLOnMonitor);
  public
    property CacheText: string read GetCacheText;
    procedure Clear(); virtual;
    constructor Create(AOwner: TComponent); override;
    destructor Destroy(); override;
    procedure DoMonitor(const Connection: TMySQLConnection; const Text: PChar; const Length: Integer; const ATraceType: TTraceType); virtual;
  published
    property Connection: TMySQLConnection read FConnection write SetConnection;
    property Enabled: Boolean read FEnabled write FEnabled default False;
    property OnMonitor: TMySQLOnMonitor read FOnMonitor write SetOnMonitor default nil;
    property CacheSize: Integer read Cache.MaxSize write SetCacheSize;
    property TraceTypes: TTraceTypes read FTraceTypes write FTraceTypes default [ttRequest];
  end;

  TMySQLConnection = class(TCustomConnection)
  type
    TSyncThread = class;
    TDataResult = TSyncThread;

    TConvertErrorNotifyEvent = procedure (Sender: TObject; Text: string) of object;
    TDatabaseChangeEvent = procedure (const Connection: TMySQLConnection; const NewName: string) of object;
    TErrorEvent = procedure (const Connection: TMySQLConnection; const ErrorCode: Integer; const ErrorMessage: string) of object;
    TVariableChangeEvent = procedure (const Connection: TMySQLConnection; const Name, NewValue: string) of object;
    TOnUpdateIndexDefsEvent = procedure (const DataSet: TMySQLQuery; const IndexDefs: TIndexDefs) of object;
    TResultEvent = function (const ErrorCode: Integer; const ErrorMessage: string; const WarningCount: Integer;
      const CommandText: string; const DataHandle: TDataResult; const Data: Boolean): Boolean of object;
    TSynchronizeEvent = procedure (const Data: Pointer) of object;

    TLibraryDataType = (ldtConnecting, ldtExecutingSQL, ldtDisconnecting);
    Plocal_infile = ^Tlocal_infile;
    Tlocal_infile = record
      Buffer: Pointer;
      BufferSize: DWord;
      Connection: TMySQLConnection;
      ErrorCode: Integer;
      Filename: array [0 .. MAX_PATH] of Char;
      Handle: THandle;
      LastError: DWord;
      Position: DWord;
    end;

    TSyncThread = class(TThread)
    type
      TMode = (smSQL, smDataHandle, smDataSet);
      TState = (ssClose, ssConnecting, ssReady, ssExecutingFirst, ssExecutingNext, ssResult, ssReceivingResult, ssDisconnecting);
    private
      Done: TEvent;
      ExecutionTime: TDateTime;
      FConnection: TMySQLConnection;
      RunExecute: TEvent;
      function GetCommandText(): string;
      function GetIsRunning(): Boolean;
      function GetNextCommandText(): string;
    protected
      CLStmts: array of Boolean;
      DataSet: TMySQLQuery;
      ErrorCode: Integer;
      ErrorMessage: string;
      LibHandle: MySQLConsts.MYSQL;
      LibThreadId: my_uint;
      Mode: TMode;
      OnResult: TResultEvent;
      ResHandle: MYSQL_RES;
      SQL: string;
      SQLIndex: Integer;
      StmtIndex: Integer;
      StmtLengths: TList;
      State: TState;
      WarningCount: Integer;
      procedure Execute(); override;
      procedure Synchronize(); virtual;
      property CommandText: string read GetCommandText;
      property IsRunning: Boolean read GetIsRunning;
      property NextCommandText: string read GetNextCommandText;
    public
      constructor Create(const AConnection: TMySQLConnection); overload; virtual;
      destructor Destroy(); override;
      procedure Terminate(); reintroduce;
      property Connection: TMySQLConnection read FConnection;
    end;

    TTerminatedThreads = class(TList)
    private
      CriticalSection: TCriticalSection;
      FConnection: TMySQLConnection;
    protected
      property Connection: TMySQLConnection read FConnection;
    public
      constructor Create(const AConnection: TMySQLConnection); reintroduce; virtual;
      destructor Destroy(); override;
      function Add(const Item: Pointer): Integer; reintroduce;
      procedure Delete(const Item: Pointer); overload;
    end;

  private
    ExecuteSQLDone: TEvent;
    FAfterExecuteSQL: TNotifyEvent;
    FAnsiQuotes: Boolean;
    FAsynchron: Boolean;
    FBeforeExecuteSQL: TNotifyEvent;
    FBugMonitor: TMySQLMonitor;
    FCharset: string;
    FCharsetNr: Byte;
    FCodePage: Cardinal;
    FConnected: Boolean;
    FErrorCode: Integer;
    FErrorMessage: string;
    FErrorCommandText: string;
    FHost: string;
    FHostInfo: string;
    FHTTPAgent: string;
    FIdentifierQuoted: Boolean;
    FIdentifierQuoter: Char;
    FLatestConnect: TDateTime;
    FLibraryName: string;
    FLibraryType: TMySQLLibrary.TLibraryType;
    FMariaDBVersion: Integer;
    FMySQLVersion: Integer;
    FOnConvertError: TConvertErrorNotifyEvent;
    FOnDatabaseChange: TDatabaseChangeEvent;
    FOnSQLError: TErrorEvent;
    FOnUpdateIndexDefs: TOnUpdateIndexDefsEvent;
    FOnVariableChange: TVariableChangeEvent;
    FPassword: string;
    FPort: Word;
    FServerTimeout: Word;
    FServerVersionStr: string;
    FSQLMonitors: array of TMySQLMonitor;
    FTerminateCS: TCriticalSection;
    FTerminatedThreads: TTerminatedThreads;
    FThreadDeep: Integer;
    FThreadId: my_uint;
    FUsername: string;
    InMonitor: Boolean;
    InOnResult: Boolean;
    KillThreadId: my_uint;
    SilentCount: Integer;
    SynchronCount: Integer;
    function GetNextCommandText(): string;
    function GetServerDateTime(): TDateTime;
    function GetHandle(): MySQLConsts.MYSQL;
    function GetInfo(): string;
    procedure SetDatabaseName(const ADatabaseName: string);
    procedure SetHost(const AHost: string);
    procedure SetLibraryName(const ALibraryName: string);
    procedure SetLibraryType(const ALibraryType: TMySQLLibrary.TLibraryType);
    procedure SetPassword(const APassword: string);
    procedure SetPort(const APort: Word);
    procedure SetUsername(const AUsername: string);
    function UseCompression(): Boolean; inline;
    function UseSyncThread(): Boolean;
  protected
    FDatabaseName: string;
    FExecutedStmts: Integer;
    FExecutionTime: TDateTime;
    FFormatSettings: TFormatSettings;
    FLib: TMySQLLibrary;
    FMultiStatements: Boolean;
    FRowsAffected: Int64;
    FSuccessfullExecutedSQLLength: Integer;
    FSyncThread: TSyncThread;
    FWarningCount: Integer;
    TimeDiff: TDateTime;
    procedure DoAfterExecuteSQL(); virtual;
    procedure DoBeforeExecuteSQL(); virtual;
    procedure DoConnect(); override;
    procedure DoConvertError(const Sender: TObject; const Text: string; const Error: EConvertError); virtual;
    procedure DoDatabaseChange(const NewName: string); virtual;
    procedure DoDisconnect(); override;
    procedure DoError(const AErrorCode: Integer; const AErrorMessage: string); virtual;
    procedure DoVariableChange(const Name, NewValue: string); virtual;
    function GetErrorMessage(const AHandle: MySQLConsts.MYSQL): string; virtual;
    function GetConnected(): Boolean; override;
    function GetInsertId(): my_ulonglong; virtual;
    function GetDataFileAllowed(): Boolean; virtual;
    function GetMaxAllowedServerPacket(): Integer; virtual;
    function InternExecuteSQL(const Mode: TSyncThread.TMode; const Synchron: Boolean; const SQL: string;
      const OnResult: TResultEvent = nil; const Done: TEvent = nil): Boolean; overload; virtual;
    function LibDecode(const Text: my_char; const Length: my_int = -1): string; virtual;
    function LibEncode(const Value: string): RawByteString; virtual;
    function LibPack(const Value: string): RawByteString; virtual;
    function LibUnpack(const Data: my_char; const Length: my_int = -1): string; virtual;
    procedure local_infile_end(const local_infile: Plocal_infile); virtual;
    function local_infile_error(const local_infile: Plocal_infile; const error_msg: my_char; const error_msg_len: my_uint): my_int; virtual;
    function local_infile_init(out local_infile: Plocal_infile; const filename: my_char): my_int; virtual;
    function local_infile_read(const local_infile: Plocal_infile; buf: my_char; const buf_len: my_uint): my_int; virtual;
    procedure RegisterSQLMonitor(const AMySQLMonitor: TMySQLMonitor); virtual;
    procedure SetAnsiQuotes(const AAnsiQuotes: Boolean); virtual;
    procedure SetCharset(const ACharset: string); virtual;
    procedure SetConnected(Value: Boolean); override;
    function SQLUse(const DatabaseName: string): string; virtual;
    procedure SyncAfterExecuteSQL(const SyncThread: TSyncThread);
    procedure SyncBindDataSet(const DataSet: TMySQLQuery);
    procedure SyncConnecting(const SyncThread: TSyncThread);
    procedure SyncConnected(const SyncThread: TSyncThread);
    procedure SyncDisconnecting(const SyncThread: TSyncThread);
    procedure SyncDisconnected(const SyncThread: TSyncThread);
    procedure SyncExecute(const SyncThread: TSyncThread);
    procedure SyncExecuted(const SyncThread: TSyncThread);
    procedure SyncExecutingFirst(const SyncThread: TSyncThread);
    procedure SyncExecutingNext(const SyncThread: TSyncThread);
    procedure SyncHandledResult(const SyncThread: TSyncThread);
    procedure SyncPing(const SyncThread: TSyncThread);
    procedure SyncReceivingResult(const SyncThread: TSyncThread);
    procedure SyncReleaseDataSet(const DataSet: TMySQLQuery);
    procedure UnRegisterSQLMonitor(const AMySQLMonitor: TMySQLMonitor); virtual;
    procedure WriteMonitor(const AText: PChar; const Length: Integer; const ATraceType: TMySQLMonitor.TTraceType); virtual;
    property CharsetNr: Byte read FCharsetNr;
    property Handle: MySQLConsts.MYSQL read GetHandle;
    property IdentifierQuoter: Char read FIdentifierQuoter;
    property IdentifierQuoted: Boolean read FIdentifierQuoted write FIdentifierQuoted;
    property SyncThread: TSyncThread read FSyncThread;
    property TerminateCS: TCriticalSection read FTerminateCS;
    property TerminatedThreads: TTerminatedThreads read FTerminatedThreads;
  public
    procedure BeginSilent(); virtual;
    procedure BeginSynchron(); virtual;
    function CharsetToCharsetNr(const Charset: string): Byte; virtual;
    function CharsetToCodePage(const Charset: string): Cardinal; overload; virtual;
    procedure CloseResult(const DataHandle: TDataResult); virtual;
    function CodePageToCharset(const CodePage: Cardinal): string; virtual;
    constructor Create(AOwner: TComponent); override;
    destructor Destroy(); override;
    procedure EndSilent(); virtual;
    procedure EndSynchron(); virtual;
    function EscapeIdentifier(const Identifier: string): string; virtual;
    function ExecuteSQL(const SQL: string; const OnResult: TResultEvent = nil): Boolean; overload; virtual;
    function FirstResult(out DataHandle: TDataResult; const SQL: string): Boolean; virtual;
    function InUse(): Boolean; virtual;
    function NextResult(const DataHandle: TDataResult): Boolean; virtual;
    function SendSQL(const SQL: string; const Done: TEvent): Boolean; overload; virtual;
    function SendSQL(const SQL: string; const OnResult: TResultEvent = nil; const Done: TEvent = nil): Boolean; overload; virtual;
    procedure Terminate(); virtual;
    property AnsiQuotes: Boolean read FAnsiQuotes write SetAnsiQuotes;
    property BugMonitor: TMySQLMonitor read FBugMonitor;
    property CodePage: Cardinal read FCodePage;
    property DataFileAllowed: Boolean read GetDataFileAllowed;
    property HostInfo: string read FHostInfo;
    property HTTPAgent: string read FHTTPAgent write FHTTPAgent;
    property ErrorCode: Integer read FErrorCode;
    property ErrorMessage: string read FErrorMessage;
    property ErrorCommandText: string read FErrorCommandText;
    property ExecutedStmts: Integer read FExecutedStmts;
    property ExecutionTime: TDateTime read FExecutionTime;
    property FormatSettings: TFormatSettings read FFormatSettings;
    property Info: string read GetInfo;
    property LatestConnect: TDateTime read FLatestConnect;
    property Lib: TMySQLLibrary read FLib;
    property MariaDBVersion: Integer read FMariaDBVersion;
    property MySQLVersion: Integer read FMySQLVersion;
    property MaxAllowedServerPacket: Integer read GetMaxAllowedServerPacket;
    property MultiStatements: Boolean read FMultiStatements;
    property NextCommandText: string read GetNextCommandText;
    property RowsAffected: Int64 read FRowsAffected;
    property ServerDateTime: TDateTime read GetServerDateTime;
    property ServerVersionStr: string read FServerVersionStr;
    property SuccessfullExecutedSQLLength: Integer read FSuccessfullExecutedSQLLength;
    property ThreadId: my_uint read FThreadId;
    property WarningCount: Integer read FWarningCount;
  published
    property Asynchron: Boolean read FAsynchron write FAsynchron default False;
    property AfterExecuteSQL: TNotifyEvent read FAfterExecuteSQL write FAfterExecuteSQL;
    property BeforeExecuteSQL: TNotifyEvent read FBeforeExecuteSQL write FBeforeExecuteSQL;
    property Charset: string read FCharset write SetCharset;
    property DatabaseName: string read FDatabaseName write SetDatabaseName;
    property Host: string read FHost write SetHost;
    property InsertId: my_ulonglong read GetInsertId;
    property LibraryName: string read FLibraryName write SetLibraryName;
    property LibraryType: TMySQLLibrary.TLibraryType read FLibraryType write SetLibraryType default ltBuiltIn;
    property OnConvertError: TConvertErrorNotifyEvent read FOnConvertError write FOnConvertError;
    property OnDatabaseChange: TDatabaseChangeEvent read FOnDatabaseChange write FOnDatabaseChange;
    property OnSQLError: TErrorEvent read FOnSQLError write FOnSQLError;
    property OnUpdateIndexDefs: TOnUpdateIndexDefsEvent read FOnUpdateIndexDefs write FOnUpdateIndexDefs;
    property OnVariableChange: TVariableChangeEvent read FOnVariableChange write FOnVariableChange;
    property Password: string read FPassword write SetPassword;
    property Port: Word read FPort write SetPort default MYSQL_PORT;
    property ServerTimeout: Word read FServerTimeout write FServerTimeout default 0;
    property Username: string read FUsername write SetUsername;
    property Connected;
    property AfterConnect;
    property BeforeConnect;
    property AfterDisconnect;
    property BeforeDisconnect;
  end;

  TMySQLQuery = class(TDataSet)
  type
    TCommandType = (ctQuery, ctTable);
    PRecordBufferData = ^TRecordBufferData;
    TRecordBufferData = packed record
      LibLengths: MYSQL_LENGTHS;
      LibRow: MYSQL_ROW;
    end;
  private
    FConnection: TMySQLConnection;
    FIndexDefs: TIndexDefs;
    FInformConvertError: Boolean;
    FRecNo: Integer;
    FRowsAffected: Integer;
    function GetHandle(): MySQLConsts.MYSQL_RES;
  protected
    FCommandText: string;
    FCommandType: TCommandType;
    FDatabaseName: string;
    SyncThread: TMySQLConnection.TSyncThread;
    function AllocRecordBuffer(): TRecordBuffer; override;
    procedure DataConvert(Field: TField; Source, Dest: Pointer; ToNative: Boolean); override;
    function FindRecord(Restart, GoForward: Boolean): Boolean; override;
    procedure FreeRecordBuffer(var Buffer: TRecordBuffer); override;
    function GetCanModify(): Boolean; override;
    function GetFieldData(const Field: TField; const Buffer: Pointer; const Data: PRecordBufferData): Boolean; overload; virtual;
    function GetLibLengths(): MYSQL_LENGTHS; virtual;
    function GetLibRow(): MYSQL_ROW; virtual;
    function GetIsIndexField(Field: TField): Boolean; override;
    function GetRecNo(): Integer; override;
    function GetRecord(Buffer: TRecordBuffer; GetMode: TGetMode; DoCheck: Boolean): TGetResult; override;
    function GetRecordCount(): Integer; override;
    function GetUniDirectional(): Boolean; virtual;
    procedure InternalClose(); override;
    procedure InternalHandleException(); override;
    procedure InternalInitFieldDefs(); override;
    procedure InternalOpen(); override;
    function IsCursorOpen(): Boolean; override;
    procedure SetActive(Value: Boolean); override;
    function SetActiveEvent(const ErrorCode: Integer; const ErrorMessage: string; const WarningCount: Integer;
      const CommandText: string; const DataHandle: TMySQLConnection.TDataResult; const Data: Boolean): Boolean; virtual;
    procedure SetCommandText(const ACommandText: string); virtual;
    procedure SetConnection(const AConnection: TMySQLConnection); virtual;
    function SQLSelect(): string; overload; virtual;
    procedure UpdateIndexDefs(); override;
    property Handle: MySQLConsts.MYSQL_RES read GetHandle;
    property IndexDefs: TIndexDefs read FIndexDefs;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy(); override;
    function CreateBlobStream(Field: TField; Mode: TBlobStreamMode): TStream; override;
    function GetFieldData(Field: TField; Buffer: Pointer): Boolean; override;
    procedure Open(const DataHandle: TMySQLConnection.TDataResult); overload; virtual;
    function SQLFieldValue(const Field: TField; Data: PRecordBufferData = nil): string; overload; virtual;
    property DatabaseName: string read FDatabaseName;
    property LibLengths: MYSQL_LENGTHS read GetLibLengths;
    property LibRow: MYSQL_ROW read GetLibRow;
    property RowsAffected: Integer read FRowsAffected;
    property UniDirectional: Boolean read GetUniDirectional;
  published
    property CommandText: string read FCommandText write SetCommandText;
    property CommandType: TCommandType read FCommandType;
    property Connection: TMySQLConnection read FConnection write SetConnection;
    property Active;
    property AfterClose;
    property AfterOpen;
    property AfterRefresh;
    property AfterScroll;
    property BeforeClose;
    property BeforeEdit;
    property BeforeInsert;
    property BeforeOpen;
    property BeforeRefresh;
    property BeforeScroll;
    property OnCalcFields;
    property OnFilterRecord;
    property OnNewRecord;
  end;

  TMySQLDataSet = class(TMySQLQuery)
  type
    TTextWidth = function (const Text: string): Integer of object;
    PInternRecordBuffer = ^TInternRecordBuffer;
    TInternRecordBuffer = record
      NewData: TMySQLQuery.PRecordBufferData;
      OldData: TMySQLQuery.PRecordBufferData;
      VisibleInFilter: Boolean;
    end;
    PExternRecordBuffer = ^TExternRecordBuffer;
    TExternRecordBuffer = record
      InternRecordBuffer: PInternRecordBuffer;
      Index: Integer;
      BookmarkFlag: TBookmarkFlag;
    end;
    TInternRecordBuffers = class(TList)
    private
      FDataSet: TMySQLDataSet;
      FRecordReceived: TEvent;
      function Get(Index: Integer): PInternRecordBuffer; inline;
      procedure Put(Index: Integer; Buffer: PInternRecordBuffer); inline;
    public
      CriticalSection: TCriticalSection;
      FilteredRecordCount: Integer;
      Index: Integer;
      procedure Clear(); override;
      constructor Create(const ADataSet: TMySQLDataSet);
      destructor Destroy(); override;
      property Buffers[Index: Integer]: PInternRecordBuffer read Get write Put; default;
      property DataSet: TMySQLDataSet read FDataSet;
      property Received: TEvent read FRecordReceived;
    end;
  private
    DeleteBookmarks: array of TBookmark;
    FCachedUpdates: Boolean;
    FCanModify: Boolean;
    FCursorOpen: Boolean;
    FDataSize: Int64;
    FilterParser: TExprParser;
    FInternRecordBuffers: TInternRecordBuffers;
    FLocateNext: Boolean;
    FRecordsReceived: TEvent;
    FSortDef: TIndexDef;
    TableName: string;
    function AllocInternRecordBuffer(): PInternRecordBuffer;
    function BookmarkToInternBufferIndex(const Bookmark: TBookmark): Integer;
    procedure FreeInternRecordBuffer(const InternRecordBuffer: PInternRecordBuffer);
    procedure InternActivateFilter();
    function VisibleInFilter(const InternRecordBuffer: PInternRecordBuffer): Boolean;
  protected
    procedure ActivateFilter(); virtual;
    function AllocRecordBuffer(): TRecordBuffer; override;
    procedure ClearBuffers(); override;
    procedure DeactivateFilter(); virtual;
    function FindRecord(Restart, GoForward: Boolean): Boolean; override;
    procedure FreeRecordBuffer(var Buffer: TRecordBuffer); override;
    procedure GetBookmarkData(Buffer: TRecordBuffer; Data: Pointer); override;
    function GetBookmarkFlag(Buffer: TRecordBuffer): TBookmarkFlag; override;
    function GetCanModify(): Boolean; override;
    function GetLibLengths(): MYSQL_LENGTHS; override;
    function GetLibRow(): MYSQL_ROW; override;
    function GetRecNo(): Integer; override;
    function GetRecord(Buffer: TRecordBuffer; GetMode: TGetMode; DoCheck: Boolean): TGetResult; override;
    function GetRecordCount(): Integer; override;
    function GetUniDirectional(): Boolean; override;
    function InternAddRecord(const LibRow: MYSQL_ROW; const LibLengths: MYSQL_LENGTHS; const Index: Integer = -1): Boolean; virtual;
    procedure InternalAddRecord(Buffer: Pointer; Append: Boolean); override;
    procedure InternalCancel(); override;
    procedure InternalClose(); override;
    procedure InternalDelete(); override;
    procedure InternalFirst(); override;
    procedure InternalGotoBookmark(Bookmark: Pointer); override;
    procedure InternalInitFieldDefs(); override;
    procedure InternalInitRecord(Buffer: TRecordBuffer); override;
    procedure InternalInsert(); override;
    procedure InternalLast(); override;
    procedure InternalOpen(); override;
    procedure InternalPost(); override;
    procedure InternalRefresh(); override;
    procedure InternalSetToRecord(Buffer: TRecordBuffer); override;
    function IsCursorOpen(): Boolean; override;
    function MoveRecordBufferData(var DestData: TMySQLQuery.PRecordBufferData; const SourceData: TMySQLQuery.PRecordBufferData): Boolean;
    procedure SetBookmarkData(Buffer: TRecordBuffer; Data: Pointer); override;
    procedure SetBookmarkFlag(Buffer: TRecordBuffer; Value: TBookmarkFlag); override;
    procedure SetFieldData(Field: TField; Buffer: Pointer); override;
    procedure SetFieldData(const Field: TField; const Buffer: Pointer; const Size: Integer); overload; virtual;
    procedure SetFieldsSortTag(); virtual;
    procedure SetFiltered(Value: Boolean); override;
    procedure SetFilterText(const Value: string); override;
    procedure SetRecNo(Value: Integer); override;
    function SQLFieldValue(const Field: TField; Buffer: TRecordBuffer = nil): string; overload; virtual;
    function SQLTableClause(): string; virtual;
    function SQLUpdate(Buffer: TRecordBuffer = nil): string; virtual;
    procedure UpdateIndexDefs(); override;
    property InternRecordBuffers: TInternRecordBuffers read FInternRecordBuffers;
    property RecordsReceived: TEvent read FRecordsReceived;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy(); override;
    function BookmarkValid(Bookmark: TBookmark): Boolean; override;
    function CompareBookmarks(Bookmark1, Bookmark2: TBookmark): Integer; override;
    function CreateBlobStream(Field: TField; Mode: TBlobStreamMode): TStream; override;
    procedure Delete(const Bookmarks: array of TBookmark); overload; virtual;
    function GetFieldData(Field: TField; Buffer: Pointer): Boolean; override;
    function GetMaxTextWidth(const Field: TField; const TextWidth: TTextWidth): Integer; virtual;
    function Locate(const KeyFields: string; const KeyValues: Variant;
      Options: TLocateOptions): Boolean; override;
    procedure Resync(Mode: TResyncMode); override;
    procedure Sort(const ASortDef: TIndexDef); virtual;
    function SQLDelete(): string; virtual;
    function SQLInsert(): string; virtual;
    property DataSize: Int64 read FDataSize;
    property LocateNext: Boolean read FLocateNext write FLocateNext;
    property SortDef: TIndexDef read FSortDef;
  published
    property CachedUpdates: Boolean read FCachedUpdates write FCachedUpdates default False;
    property AfterCancel;
    property AfterDelete;
    property AfterEdit;
    property AfterInsert;
    property AfterPost;
    property BeforeCancel;
    property BeforeDelete;
    property BeforeEdit;
    property BeforeInsert;
    property BeforePost;
    property Filter;
    property Filtered;
    property FilterOptions;
    property OnDeleteError;
    property OnEditError;
    property OnPostError;
  end;

  TMySQLTable = class(TMySQLDataSet)
  private
    FAutomaticLoadNextRecords: Boolean;
    FLimit: Integer;
    FLimitedDataReceived: Boolean;
    FOffset: Integer;
  protected
    RequestedRecordCount: Integer;
    function GetCanModify(): Boolean; override;
    procedure InternalClose(); override;
    procedure InternalLast(); override;
    procedure InternalOpen(); override;
    function SQLSelect(): string; overload; override;
    function SQLSelect(const IgnoreLimit: Boolean): string; overload; virtual;
  public
    constructor Create(AOwner: TComponent); override;
    function LoadNextRecords(const AllRecords: Boolean = False): Boolean; virtual;
    procedure Sort(const ASortDef: TIndexDef); override;
    property CommandText: string read FCommandText write SetCommandText;
    property LimitedDataReceived: Boolean read FLimitedDataReceived;
  published
    property AutomaticLoadNextRecords: Boolean read FAutomaticLoadNextRecords write FAutomaticLoadNextRecords default False;
    property Limit: Integer read FLimit write FLimit default 0;
    property Offset: Integer read FOffset write FOffset default 0;
  end;

  TMySQLBitField = class(TLargeintField)
  protected
    function GetAsString(): string; override;
    function GetIsNull(): Boolean; override;
    procedure GetText(var Text: string; DisplayText: Boolean); override;
    procedure SetAsString(const Value: string); override;
  end;

  TMySQLBlobField = class(TBlobField)
  protected
    function GetAsAnsiString(): AnsiString; override;
    function GetAsString(): string; override;
    function GetAsVariant(): Variant; override;
    function GetIsNull(): Boolean; override;
    procedure GetText(var Text: string; DisplayText: Boolean); override;
    procedure SetAsString(const Value: string); override;
  end;

  TMySQLByteField = class(TByteField)
  protected
    function GetAsString(): string; override;
    function GetIsNull(): Boolean; override;
    procedure GetText(var Text: string; DisplayText: Boolean); override;
  end;

  TMySQLDateField = class(TDateField)
  private
    ZeroDateString: string;
  protected
    function GetAsString(): string; override;
    function GetIsNull(): Boolean; override;
    procedure GetText(var Text: string; DisplayText: Boolean); override;
    procedure SetAsString(const Value: string); override;
    procedure SetDataSet(ADataSet: TDataSet); override;
  end;

  TMySQLDateTimeField = class(TDateTimeField)
  private
    ZeroDateString: string;
  protected
    function GetAsString(): string; override;
    function GetIsNull(): Boolean; override;
    procedure GetText(var Text: string; DisplayText: Boolean); override;
    procedure SetAsString(const Value: string); override;
    procedure SetDataSet(ADataSet: TDataSet); override;
  end;

  TMySQLExtendedField = class(TExtendedField)
  protected
    function GetAsString(): string; override;
    function GetIsNull(): Boolean; override;
    procedure GetText(var Text: string; DisplayText: Boolean); override;
  end;

  TMySQLFloatField = class(TFloatField)
  protected
    function GetAsString(): string; override;
    function GetIsNull(): Boolean; override;
    procedure GetText(var Text: string; DisplayText: Boolean); override;
  end;

  TMySQLIntegerField = class(TIntegerField)
  protected
    function GetAsString(): string; override;
    function GetIsNull(): Boolean; override;
    procedure GetText(var Text: string; DisplayText: Boolean); override;
  end;

  TMySQLLargeWordField = class(TLargeintField)
  private
    FMaxValue: UInt64;
    FMinValue: UInt64;
    procedure CheckRange(Value, Min, Max: UInt64);
  protected
    function GetAsLargeInt(): Largeint; override;
    function GetAsString(): string; override;
    function GetIsNull(): Boolean; override;
    procedure GetText(var Text: string; DisplayText: Boolean); override;
    procedure SetAsLargeInt(Value: Largeint); override;
    procedure SetAsString(const Value: string); override;
  published
    property MinValue: UInt64 read FMinValue write FMinValue default 0;
    property MaxValue: UInt64 read FMaxValue write FMaxValue default 0;
  end;

  TMySQLLongWordField = class(TLongWordField)
  protected
    function GetAsString(): string; override;
    function GetIsNull(): Boolean; override;
    procedure GetText(var Text: string; DisplayText: Boolean); override;
  end;

  TMySQLShortIntField = class(TShortIntField)
  protected
    function GetAsString(): string; override;
    function GetIsNull(): Boolean; override;
    procedure GetText(var Text: string; DisplayText: Boolean); override;
  end;

  TMySQLSingleField = class(TSingleField)
  protected
    function GetAsString(): string; override;
    function GetIsNull(): Boolean; override;
    procedure GetText(var Text: string; DisplayText: Boolean); override;
  end;

  TMySQLSmallIntField = class(TSmallIntField)
  protected
    function GetAsString(): string; override;
    function GetIsNull(): Boolean; override;
    procedure GetText(var Text: string; DisplayText: Boolean); override;
  end;

  TMySQLStringField = class(TStringField)
  protected
    function GetAsAnsiString(): AnsiString; override;
    function GetAsString(): string; override;
    function GetIsNull(): Boolean; override;
    procedure GetText(var Text: string; DisplayText: Boolean); override;
    procedure SetAsString(const Value: string); override;
  end;

  TMySQLTimeField = class(TIntegerField)
  protected
    SQLFormat: string;
    function GetAsDateTime(): TDateTime; override;
    function GetAsString(): string; override;
    function GetAsVariant(): Variant; override;
    function GetDataSize(): Integer; override;
    function GetIsNull(): Boolean; override;
    procedure GetText(var Text: string; DisplayText: Boolean); override;
    procedure SetAsString(const Value: string); override;
    procedure SetAsVariant(const Value: Variant); override;
    procedure SetDataSet(ADataSet: TDataSet); override;
  public
    constructor Create(AOwner: TComponent); override;
    property Value: Variant read GetAsVariant write SetAsVariant;
  end;

  TMySQLTimeStampField = class(TSQLTimeStampField)
  protected
    SQLFormat: string;
    function GetAsSQLTimeStamp(): TSQLTimeStamp; override;
    function GetAsString(): string; override;
    function GetAsVariant(): Variant; override;
    function GetDataSize(): Integer; override;
    function GetIsNull(): Boolean; override;
    function GetOldValue(): Variant;
    procedure GetText(var Text: string; DisplayText: Boolean); override;
    procedure SetAsSQLTimeStamp(const Value: TSQLTimeStamp); override;
    procedure SetAsString(const Value: string); override;
    procedure SetAsVariant(const Value: Variant); override;
  public
    procedure SetDataSet(ADataSet: TDataSet); override;
    property Value: Variant read GetAsVariant write SetAsVariant;
  end;

  TMySQLWideMemoField = class(TWideMemoField)
  public
    function GetAsString(): string; override;
    function GetAsVariant: Variant; override;
    function GetIsNull(): Boolean; override;
    procedure GetText(var Text: string; DisplayText: Boolean); override;
    procedure SetAsString(const Value: string); override;
  end;

  TMySQLWideStringField = class(TWideStringField)
  protected
    function GetAsDateTime(): TDateTime; override;
    function GetAsString(): string; override;
    function GetIsNull(): Boolean; override;
    procedure GetText(var Text: string; DisplayText: Boolean); override;
    procedure SetAsDateTime(Value: TDateTime); override;
  end;

  TMySQLWordField = class(TWordField)
  protected
    function GetAsString(): string; override;
    function GetIsNull(): Boolean; override;
    procedure GetText(var Text: string; DisplayText: Boolean); override;
  end;

const
  MySQLZeroDate = -693593;

  ftAscSortedField  = $10000;
  ftDescSortedField = $20000;
  ftSortedField     = $30000;
  ftBitField        = $40000;
  ftGeometryField   = $80000;

function BitField(const Field: TField): Boolean;
function DateTimeToStr(const DateTime: TDateTime; const FormatSettings: TFormatSettings): string; overload;
function DateToStr(const Date: TDateTime; const FormatSettings: TFormatSettings): string; overload;
function ExecutionTimeToStr(const Time: TDateTime; const Digits: Byte = 2): string;
function GetZeroDateString(const FormatSettings: TFormatSettings): string;
function GeometryField(const Field: TField): Boolean;
procedure MySQLConnectionSynchronize(const Data: Pointer);
function StrToDate(const S: string; const FormatSettings: TFormatSettings): TDateTime; overload;
function StrToDateTime(const S: string; const FormatSettings: TFormatSettings): TDateTime; overload;
function SQLFormatToDisplayFormat(const SQLFormat: string): string;
function AnsiCharToWideChar(const CodePage: UINT; const lpMultiByteStr: LPCSTR; const cchMultiByte: Integer; const lpWideCharStr: LPWSTR; const cchWideChar: Integer): Integer;
function WideCharToAnsiChar(const CodePage: UINT; const lpWideCharStr: LPWSTR; const cchWideChar: Integer; const lpMultiByteStr: LPSTR; const cchMultiByte: Integer): Integer;


const
  NotQuotedDataTypes = [ftShortInt, ftByte, ftSmallInt, ftWord, ftInteger, ftLongWord, ftLargeint, ftSingle, ftFloat, ftExtended];
  BinaryDataTypes = [ftString, ftBlob];
  TextDataTypes = [ftWideString, ftWideMemo];
  RightAlignedDataTypes = [ftShortInt, ftByte, ftSmallInt, ftWord, ftInteger, ftLongWord, ftLargeint, ftSingle, ftFloat, ftExtended];

var
  LocaleFormatSettings: TFormatSettings;
  MySQLConnectionOnSynchronize: TMySQLConnection.TSynchronizeEvent;

implementation {***************************************************************}

uses
  DBConsts, Forms, Variants,
  RTLConsts, Consts, SysConst, Math, StrUtils,
  {$IFDEF EurekaLog}
  ExceptionLog,
  {$ENDIF}
  MySQLClient,
  SQLUtils, CSVUtils, HTTPTunnel;

resourcestring
  SNoConnection = 'Database not connected';
  SInUse = 'Connection in use';
  SInvalidBuffer = 'Invalid buffer';
  SLibraryNotAvailable = 'Library can not be loaded ("%s")';
  SOutOfSync = 'Thread synchronization error';
  SWrongDataSet = 'Field doesn''t attached to a "%s" DataSet';

const
  DATASET_ERRORS: array [0..2] of PChar = (
    'SET NAMES / SET CHARACTER SET / SET CHARSET statements are not supported', {0}
    'Require MySQL Server 3.23.20 or higher',                                   {1}
    'DataSet run out of memory');                                               {2}

const
  // Field mappings needed for filtering. (What field type should be compared with what internal type?)
  FldTypeMap: TFieldMap = (
    { ftUnknown          }  Ord(ftUnknown),
    { ftString           }  Ord(ftString),
    { ftSmallInt         }  Ord(ftSmallInt),
    { ftInteger          }  Ord(ftInteger),
    { ftWord             }  Ord(ftWord),
    { ftBoolean          }  Ord(ftBoolean),
    { ftFloat            }  Ord(ftFloat),
    { ftCurrency         }  Ord(ftFloat),
    { ftBCD              }  Ord(ftBCD),
    { ftDate             }  Ord(ftDate),
    { ftTime             }  Ord(ftTime),
    { ftDateTime         }  Ord(ftDateTime),
    { ftBytes            }  Ord(ftBytes),
    { ftVarBytes         }  Ord(ftVarBytes),
    { ftAutoInc          }  Ord(ftInteger),
    { fBlob              }  Ord(ftBlob),
    { ftMemo             }  Ord(ftBlob),
    { ftGraphic          }  Ord(ftBlob),
    { ftFmtMemo          }  Ord(ftBlob),
    { ftParadoxOle       }  Ord(ftBlob),
    { ftDBaseOle         }  Ord(ftBlob),
    { ftTypedBinary      }  Ord(ftBlob),
    { ftCursor           }  Ord(ftUnknown),
    { ftFixedChar        }  Ord(ftString),
    { ftWideString       }  Ord(ftWideString),
    { ftLargeInt         }  Ord(ftLargeInt),
    { ftADT              }  Ord(ftADT),
    { ftArray            }  Ord(ftArray),
    { ftReference        }  Ord(ftUnknown),
    { ftDataset          }  Ord(ftUnknown),
    { ftOraBlob          }  Ord(ftBlob),
    { ftOraClob          }  Ord(ftBlob),
    { ftVariant          }  Ord(ftUnknown),
    { ftInterface        }  Ord(ftUnknown),
    { ftIDispatch        }  Ord(ftUnknown),
    { ftGUID             }  Ord(ftGUID),
    { ftTimeStamp        }  Ord(ftTimeStamp),
    { ftFmtBCD           }  Ord(ftFmtBCD),
    { ftFixedWideChar    }  Ord(ftFixedWideChar),
    { ftWideMemo         }  Ord(ftWideMemo),
    { ftOraTimeStamp     }  Ord(ftOraTimeStamp),
    { ftOraInterval      }  Ord(ftOraInterval),
    { ftLongWord         }  Ord(ftLongWord),
    { ftShortint         }  Ord(ftShortint),
    { ftByte             }  Ord(ftByte),
    { ftExtended         }  Ord(ftExtended),
    { ftConnection       }  Ord(ftConnection),
    { ftParams           }  Ord(ftParams),
    { ftStream           }  Ord(ftStream),
    { ftTimeStampOffset  }  Ord(ftTimeStampOffset),
    { ftObject           }  Ord(ftObject),
    { ftSingle           }  Ord(ftSingle)
  );
                                                                            
type
  TMySQLQueryBlobStream = class(TMemoryStream)
  public
    constructor Create(const AField: TBlobField);
    function Write(const Buffer; Len: Integer): Longint; override;
    procedure WriteBuffer(const Buffer; Count: Longint);
  end;

  TMySQLQueryMemoStream = TMySQLQueryBlobStream;

  TMySQLDataSetBlobStream = class(TMemoryStream)
  private
    Empty: Boolean;
    Field: TBlobField;
    Mode: TBlobStreamMode;
  public
    constructor Create(const AField: TBlobField; AMode: TBlobStreamMode);
    destructor Destroy; override;
    function Write(const Buffer; Len: Integer): Longint; override;
  end;

var
  DataSetNumber: Integer;
  MySQLLibraries: array of TMySQLLibrary;

{******************************************************************************}

var
  SynchronizingThreadsCS: TCriticalSection;
  SynchronizingThreads: TList;

function BitField(const Field: TField): Boolean;
begin
  Result := Assigned(Field) and (Field.Tag and ftBitField <> 0);
end;

function DateTimeToStr(const DateTime: TDateTime;
  const FormatSettings: TFormatSettings): string; overload;
begin
  if (Trunc(DateTime) <= MySQLZeroDate) then
    Result := GetZeroDateString(FormatSettings) + ' ' + TimeToStr(DateTime, FormatSettings)
  else
    Result := SysUtils.DateTimeToStr(DateTime, FormatSettings);
end;

function DateToStr(const Date: TDateTime;
  const FormatSettings: TFormatSettings): string; overload;
begin
  if (Trunc(Date) <= MySQLZeroDate) then
    Result := GetZeroDateString(FormatSettings)
  else
    Result := SysUtils.DateToStr(Date, FormatSettings);
end;

function DisplayFormatToSQLFormat(const DateTimeFormat: string): string;
var
  Index: Integer;
begin
  Result := DateTimeFormat;

  while (Pos('mm', Result) > 0) do
  begin
    Index := Pos('mm', Result);
    Result[Index] := '%';
    if ((Index > 1) and (Result[Index - 1] = 'h')) then
      Result[Index + 1] := 'i'
    else
      Result[Index + 1] := 'm';
  end;

  Result := ReplaceStr(Result, 'yyyy', '%Y');
  Result := ReplaceStr(Result, 'yy', '%y');
  Result := ReplaceStr(Result, 'MM', '%m');
  Result := ReplaceStr(Result, 'dd', '%d');
  Result := ReplaceStr(Result, 'hh', '%H');
  Result := ReplaceStr(Result, 'ss', '%s');
end;

function ExecutionTimeToStr(const Time: TDateTime; const Digits: Byte = 2): string;
var
  Hour: Word;
  Minute: Word;
  MSec: Word;
  Second: Word;
begin
  if (Time >= 1) then
    Result := IntToStr(Trunc(Time)) + ' days'
  else
  begin
    DecodeTime(Time, Hour, Minute, Second, MSec);
    if (Time < 0) then
      Result := '???'
    else if ((Hour > 0) or (Minute > 0)) then
      Result := TimeToStr(Time, LocaleFormatSettings)
    else
      Result := Format('%2.' + IntToStr(Digits) + 'f', [Second + MSec / 1000]);
  end;
end;

procedure FreeMySQLLibraries();
var
  I: Integer;
begin
  for I := 0 to Length(MySQLLibraries) - 1 do
    MySQLLibraries[I].Free();
  SetLength(MySQLLibraries, 0);
end;

function GeometryField(const Field: TField): Boolean;
begin
  Result := Assigned(Field) and (Field.Tag and ftGeometryField <> 0);
end;

function GetZeroDateString(const FormatSettings: TFormatSettings): string;
begin
  Result := FormatSettings.ShortDateFormat;
  Result := ReplaceStr(Result, 'Y', '0');
  Result := ReplaceStr(Result, 'y', '0');
  Result := ReplaceStr(Result, 'M', '0');
  Result := ReplaceStr(Result, 'm', '0');
  Result := ReplaceStr(Result, 'D', '0');
  Result := ReplaceStr(Result, 'd', '0');
  Result := ReplaceStr(Result, 'e', '0');
  Result := ReplaceStr(Result, '/', FormatSettings.DateSeparator);
end;

function LoadMySQLLibrary(const ALibraryType: TMySQLLibrary.TLibraryType; const AFileName: TFileName): TMySQLLibrary;
var
  I: Integer;
begin
  Result := nil;
  for I := 0 to Length(MySQLLibraries) - 1 do
    if ((MySQLLibraries[I].LibraryType = ALibraryType) and (MySQLLibraries[I].FileName = AFileName)) then
      Result := MySQLLibraries[I];

  if (not Assigned(Result)) then
  begin
    Result := TMySQLLibrary.Create(ALibraryType, AFileName);
    if (Result.Version = 0) then
      FreeAndNil(Result);

    if (Assigned(Result)) then
    begin
      SetLength(MySQLLibraries, Length(MySQLLibraries) + 1);
      MySQLLibraries[Length(MySQLLibraries) - 1] := Result;
    end;
  end;
end;

procedure MySQLConnectionSynchronize(const Data: Pointer);
var
  Index: Integer;
  SyncThread: TMySQLConnection.TSyncThread;
begin
  SyncThread := TMySQLConnection.TSyncThread(Data);

  SynchronizingThreadsCS.Enter();
  Index := SynchronizingThreads.IndexOf(SyncThread);
  if (Index < 0) then
    SyncThread := nil
  else
    SynchronizingThreads.Delete(Index);
  SynchronizingThreadsCS.Leave();

  if (Assigned(SyncThread)) then
    SyncThread.Synchronize();
end;

procedure MySQLConnectionSynchronizeRequest(const SyncThread: TMySQLConnection.TSyncThread);
begin
  if (not Assigned(MySQLConnectionOnSynchronize)) then
    SyncThread.Terminate()
  else
  begin
    SynchronizingThreadsCS.Enter();
    SynchronizingThreads.Add(SyncThread);

    MySQLConnectionOnSynchronize(SyncThread);
    SynchronizingThreadsCS.Leave();
  end;
end;

function MySQLTimeStampToStr(const SQLTimeStamp: TSQLTimeStamp; const DisplayFormat: string): string;
var
  I: Integer;
begin
  Result := LowerCase(DisplayFormat);

  for I := Length(DisplayFormat) - 1 downto 0 do
  begin
    if (DisplayFormat[I] = 'c') then
    begin
      Delete(Result, I, 1);
      Insert(FormatSettings.ShortDateFormat, Result, I);
    end;
    if (Copy(DisplayFormat, I, 2) = 'tt') then
    begin
      Delete(Result, I, 1);
      Insert(FormatSettings.LongTimeFormat, Result, I);
    end;
    if (DisplayFormat[I] = 't') then
    begin
      Delete(Result, I, 1);
      Insert(FormatSettings.ShortTimeFormat, Result, I);
    end;
    if (DisplayFormat[I] = '/') then
    begin
      Delete(Result, I, 1);
      Insert(FormatSettings.DateSeparator, Result, I);
    end;
    if (DisplayFormat[I] = ':') then
    begin
      Delete(Result, I, 1);
      Insert(FormatSettings.TimeSeparator, Result, I);
    end;
  end;

  for I := Length(Result) downto 1 do
    if (Result[I] = 'm') then
      if ((I > 1) and (Result[I - 1] = 'm')) then
      else if (((I > 1) and (Result[I - 1] = 'h')) or ((I > 2) and (Result[I - 1] = FormatSettings.TimeSeparator) and (Result[I - 2] = 'h'))) then
        if ((I < Length(Result)) and (Result[I + 1] = 'm')) then
          begin Delete(Result, I, 2); Insert(ReplaceStr(Format('%2d', [SQLTimeStamp.Minute]), ' ', '0'), Result, I); end
        else
          begin Delete(Result, I, 1); Insert(IntToStr(SQLTimeStamp.Minute), Result, I); end;

  while (Pos('yyyy', Result) > 0) do
    Result := ReplaceStr(Result, 'yyyy', ReplaceStr(Format('%4d', [SQLTimeStamp.Year]), ' ', '0'));
  while (Pos('yy', Result) > 0) do
    Result := ReplaceStr(Result, 'yy', ReplaceStr(Format('%2d', [SQLTimeStamp.Year mod 100]), ' ', '0'));
  while (Pos('mm', Result) > 0) do
    Result := ReplaceStr(Result, 'mm', ReplaceStr(Format('%2d', [SQLTimeStamp.Month]), ' ', '0'));
  while (Pos('dd', Result) > 0) do
    Result := ReplaceStr(Result, 'dd', ReplaceStr(Format('%2d', [SQLTimeStamp.Day]), ' ', '0'));
  while (Pos('d', Result) > 0) do
    Result := ReplaceStr(Result, 'd', IntToStr(SQLTimeStamp.Day));
  while (Pos('hh', Result) > 0) do
    Result := ReplaceStr(Result, 'hh', ReplaceStr(Format('%2d', [SQLTimeStamp.Hour]), ' ', '0'));
  while (Pos('h', Result) > 0) do
    Result := ReplaceStr(Result, 'h', IntToStr(SQLTimeStamp.Hour));
  while (Pos('ss', Result) > 0) do
    Result := ReplaceStr(Result, 'ss', ReplaceStr(Format('%2d', [SQLTimeStamp.Second]), ' ', '0'));
  while (Pos('s', Result) > 0) do
    Result := ReplaceStr(Result, 's', IntToStr(SQLTimeStamp.Second));
end;

function StrToDate(const S: string;
  const FormatSettings: TFormatSettings): TDateTime; overload;
begin
  if (S = GetZeroDateString(FormatSettings)) then
    Result := MySQLZeroDate
  else
    Result := SysUtils.StrToDate(S, FormatSettings);
end;

function StrToDateTime(const S: string;
  const FormatSettings: TFormatSettings): TDateTime;
var
  ZeroDateString: string;
begin
  ZeroDateString := GetZeroDateString(FormatSettings);

  if (Copy(S, 1, Length(ZeroDateString)) = ZeroDateString) then
    Result := MySQLZeroDate - StrToTime(Copy(S, Length(ZeroDateString) + 1, Length(S) - Length(ZeroDateString)), FormatSettings)
  else
    Result := SysUtils.StrToDateTime(S, FormatSettings);
end;

function StrToMySQLTimeStamp(const Str: string; const SQLFormat: string): TSQLTimeStamp;
var
  I: Integer;
  Pos: Integer;
  SQLTimeStamp: TSQLTimeStamp;
begin
  SQLTimeStamp := NullSQLTimeStamp;

  I := 0; Pos := 1;
  while ((Pos <= Length(Str)) and (I < Length(SQLFormat) - 1)) do
  begin
    if (SQLFormat[I + 1] <> '%') then
      Inc(Pos, 1)
    else
    begin
      case (SQLFormat[I + 2]) of
        'Y': begin SQLTimeStamp.Year := StrToInt(Copy(Str, Pos, 4)); Inc(Pos, 4); end;
        'y': begin
               SQLTimeStamp.Year := StrToInt(Copy(Str, Pos, 2)); Inc(Pos, 2);
               if (SQLTimeStamp.Year <= 69) then
                 Inc(SQLTimeStamp.Year, 2000)
               else
                 Inc(SQLTimeStamp.Year, 1900);
             end;
        'm': begin SQLTimeStamp.Month := StrToInt(Copy(Str, Pos, 2)); Inc(Pos, 2); end;
        'd': begin SQLTimeStamp.Day := StrToInt(Copy(Str, Pos, 2)); Inc(Pos, 2); end;
        'H': begin SQLTimeStamp.Hour := StrToInt(Copy(Str, Pos, 2)); Inc(Pos, 2); end;
        'i': begin SQLTimeStamp.Minute := StrToInt(Copy(Str, Pos, 2)); Inc(Pos, 2); end;
        's': begin SQLTimeStamp.Second := StrToInt(Copy(Str, Pos, 2)); Inc(Pos, 2); end;
        else raise EConvertError.CreateFMT(SConvUnknownType, [SQLFormat[I + 2]]);
      end;
      Inc(I);
    end;
    Inc(I);
  end;

  Result := SQLTimeStamp;
end;

function StrToTime(const S, SQLFormat: string): Integer; overload;
var
  B: Byte;
  DisplayFormat: string;
  L: Byte;
  Str: string;
begin
  Result := 0;
  Str := S;
  if (S[1] = '-') then Delete(Str, 1, 1);

  DisplayFormat := SQLFormatToDisplayFormat(SQLFormat);
  DisplayFormat := ReplaceStr(DisplayFormat, 'hh', StringOfChar('h', Length(Str) - Length(DisplayFormat) + 2));

  if (Pos('ss', DisplayFormat) > 0) then
  begin
    B := StrToInt(Copy(Str, Pos('ss', DisplayFormat), 2));
    if (B >= 60) then
      raise EConvertError.CreateFmt(SInvalidTime, [S]);
    Result := Result + B;
  end;
  if (Pos('mm', DisplayFormat) > 0) then
  begin
    B := StrToInt(Copy(Str, Pos('mm', DisplayFormat), 2));
    if (B >= 60) then
      raise EConvertError.CreateFmt(SInvalidTime, [S]);
    Result := Result + B * 60;
  end;
  if (Pos('hh', DisplayFormat) > 0) then
  begin
    L := 2;
    while (DisplayFormat[Pos('hh', DisplayFormat) + L] = 'h') do Inc(L);
    Result := Result + StrToInt(Copy(Str, Pos('hh', DisplayFormat), L)) * 3600;
  end;
  if (S[1] = '-') then
    Result := - Result;

  if ((Result <= -3020400) or (3020400 <= Result))  then
    raise EConvertError.CreateFmt(SInvalidTime, [S]);
end;

function SQLFormatToDisplayFormat(const SQLFormat: string): string;
begin
  Result := SQLFormat;
  Result := ReplaceStr(Result, '%Y', 'yyyy');
  Result := ReplaceStr(Result, '%y', 'yy');
  Result := ReplaceStr(Result, '%M', 'mmmm');
  Result := ReplaceStr(Result, '%m', 'mm');
  Result := ReplaceStr(Result, '%c', 'm');
  Result := ReplaceStr(Result, '%D', 'dddd');
  Result := ReplaceStr(Result, '%d', 'dd');
  Result := ReplaceStr(Result, '%e', 'd');
  Result := ReplaceStr(Result, '%h', 'h');
  Result := ReplaceStr(Result, '%H', 'hh');
  Result := ReplaceStr(Result, '%p', 'x');
  Result := ReplaceStr(Result, '%i', 'mm');
  Result := ReplaceStr(Result, '%k', 'hh');
  Result := ReplaceStr(Result, '%l', 'n');
  Result := ReplaceStr(Result, '%S', 'ss');
  Result := ReplaceStr(Result, '%s', 'ss');
end;

function TimeToStr(const Time: Integer; const SQLFormat: string): string; overload;
var
  S: string;
  T: Integer;
begin
  T := Time;

  Result := SQLFormatToDisplayFormat(SQLFormat);

  if (T < 0) then
  begin
    T := -T;
    Result := ReplaceStr(Result, 'hh', '-hh');
  end;

  S := IntToStr(T mod 60); if (Length(S) = 1) then S := '0' + S; Result := ReplaceStr(Result, 'ss', S); T := T div 60;
  S := IntToStr(T mod 60); if (Length(S) = 1) then S := '0' + S; Result := ReplaceStr(Result, 'mm', S); T := T div 60;
  S := IntToStr(T       ); if (Length(S) = 1) then S := '0' + S; Result := ReplaceStr(Result, 'hh', S);
end;

function AnsiCharToWideChar(const CodePage: UINT; const lpMultiByteStr: LPCSTR; const cchMultiByte: Integer; const lpWideCharStr: LPWSTR; const cchWideChar: Integer): Integer;
var
  Hex: string;
  Length: Integer;
begin
  if (not Assigned(lpMultiByteStr) or (cchMultiByte = 0)) then
    Result := 0
  else
  begin
    Result := MultiByteToWideChar(CodePage, MB_ERR_INVALID_CHARS, lpMultiByteStr, cchMultiByte, lpWideCharStr, cchWideChar);
    if (Result = 0) then
    begin
      Length := cchMultiByte - 1;
      while ((Length > 0) and (MultiByteToWideChar(CodePage, MB_ERR_INVALID_CHARS, lpMultiByteStr, Length, nil, 0) = 0)) do
        Dec(Length);
      Hex := SQLEscapeBin(lpMultiByteStr, cchMultiByte, True);
      raise EOSError.CreateFmt(SOSError + ' (CodePage: %d, Hex: %s, Index: %d)', [GetLastError(), SysErrorMessage(GetLastError()), CodePage, Hex, Length]);
    end;
  end;
end;

function WideCharToAnsiChar(const CodePage: UINT; const lpWideCharStr: LPWSTR; const cchWideChar: Integer; const lpMultiByteStr: LPSTR; const cchMultiByte: Integer): Integer;
var
  Flags: DWord;
  Index: Integer;
begin
  if (not Assigned(lpWideCharStr) or (cchWideChar = 0)) then
    Result := 0
  else
  begin
    if ((CodePage <> CP_UTF8) or not CheckWin32Version(6)) then Flags := 0 else Flags := WC_ERR_INVALID_CHARS;
    Result := WideCharToMultiByte(CodePage, Flags, lpWideCharStr, cchWideChar, lpMultiByteStr, cchMultiByte, nil, nil);
    if (Result = 0) then
    begin
      Index := cchMultiByte - 1;
      while ((Index > 0) and (WideCharToMultiByte(CodePage, Flags, lpWideCharStr, Index, nil, 0, nil, nil) = 0)) do
        Dec(Index);
      raise EOSError.CreateFmt(SOSError + ' near "%s" (CodePage: %d)', [GetLastError(), SysErrorMessage(GetLastError()), Copy(StrPas(lpWideCharStr), 1 + Index, 20), CodePage]);
    end;
  end;
end;

function SwapUInt64(I: UInt64): UInt64; register; // swap byte order
asm
  MOV   EAX,DWORD [I]
  BSWAP EAX
  MOV   DWORD [Result + 4],EAX
  MOV   EAX,DWORD [I + 4]
  BSWAP EAX
  MOV   DWORD [Result],EAX
end;

{ Callback functions **********************************************************}

procedure local_infile_end(local_infile: TMySQLConnection.Plocal_infile); cdecl;
begin
  local_infile^.Connection.local_infile_end(local_infile);
end;

function local_infile_error(local_infile: TMySQLConnection.Plocal_infile; error_msg: my_char; error_msg_len: my_uint): my_int; cdecl;
begin
  Result := local_infile^.Connection.local_infile_error(local_infile, error_msg, error_msg_len);
end;

function local_infile_init(out local_infile: TMySQLConnection.Plocal_infile; filename: my_char; userdata: Pointer): my_int; cdecl;
begin
  Result := TMySQLConnection(userdata).local_infile_init(local_infile, filename);
end;

function local_infile_read(local_infile: TMySQLConnection.Plocal_infile; buf: my_char; buf_len: my_uint): my_int; cdecl;
begin
  Result := local_infile^.Connection.local_infile_read(local_infile, buf, buf_len);
end;

function TMySQLLibrary.GetVersionStr(): string;
begin
  Result := string(mysql_get_client_info());
end;

{ TMySQLLibrary ***************************************************************}

constructor TMySQLLibrary.Create(const ALibraryType: TLibraryType; const AFilename: TFileName);
var
  Code: Integer;
  S: string;
begin
  inherited Create();

  FHandle := 0;
  FLibraryType := ALibraryType;
  FFilename := AFilename;

  if (LibraryType = ltDLL) then
  begin
    Assert(FFilename <> '');

    FHandle := LoadLibrary(PChar(FFilename));

    if (Handle > 0) then
    begin
      my_init := GetProcAddress(Handle, 'my_init');
      mysql_affected_rows := GetProcAddress(Handle, 'mysql_affected_rows');
      mysql_character_set_name := GetProcAddress(Handle, 'mysql_character_set_name');
      mysql_close := GetProcAddress(Handle, 'mysql_close');
      mysql_errno := GetProcAddress(Handle, 'mysql_errno');
      mysql_error := GetProcAddress(Handle, 'mysql_error');
      mysql_fetch_field := GetProcAddress(Handle, 'mysql_fetch_field');
      mysql_fetch_fields := GetProcAddress(Handle, 'mysql_fetch_fields');
      mysql_fetch_field_direct := GetProcAddress(Handle, 'mysql_fetch_field_direct');
      mysql_fetch_lengths := GetProcAddress(Handle, 'mysql_fetch_lengths');
      mysql_fetch_row := GetProcAddress(Handle, 'mysql_fetch_row');
      mysql_field_count := GetProcAddress(Handle, 'mysql_field_count');
      mysql_free_result := GetProcAddress(Handle, 'mysql_free_result');
      mysql_get_client_info := GetProcAddress(Handle, 'mysql_get_client_info');
      mysql_get_client_version := GetProcAddress(Handle, 'mysql_get_client_version');
      mysql_get_host_info := GetProcAddress(Handle, 'mysql_get_host_info');
      mysql_get_server_info := GetProcAddress(Handle, 'mysql_get_server_info');
      mysql_get_server_version := GetProcAddress(Handle, 'mysql_get_server_version');
      mysql_info := GetProcAddress(Handle, 'mysql_info');
      mysql_init := GetProcAddress(Handle, 'mysql_init');
      mysql_insert_id := GetProcAddress(Handle, 'mysql_insert_id');
      mysql_more_results := GetProcAddress(Handle, 'mysql_more_results');
      mysql_next_result := GetProcAddress(Handle, 'mysql_next_result');
      mysql_library_end := GetProcAddress(Handle, 'mysql_library_end');
      mysql_library_init := GetProcAddress(Handle, 'mysql_library_init');
      mysql_num_fields := GetProcAddress(Handle, 'mysql_num_fields');
      mysql_num_rows := GetProcAddress(Handle, 'mysql_num_rows');
      mysql_options := GetProcAddress(Handle, 'mysql_options');
      mysql_ping := GetProcAddress(Handle, 'mysql_ping');
      mysql_real_connect := GetProcAddress(Handle, 'mysql_real_connect');
      mysql_real_escape_string := GetProcAddress(Handle, 'mysql_real_escape_string');
      mysql_real_query := GetProcAddress(Handle, 'mysql_real_query');
      mysql_session_track_get_first := GetProcAddress(Handle, 'mysql_session_track_get_first');
      mysql_session_track_get_next := GetProcAddress(Handle, 'mysql_session_track_get_next');
      mysql_set_character_set := GetProcAddress(Handle, 'mysql_set_character_set');
      mysql_set_local_infile_default := GetProcAddress(Handle, 'mysql_set_local_infile_default');
      mysql_set_local_infile_handler := GetProcAddress(Handle, 'mysql_set_local_infile_handler');
      mysql_set_server_option := GetProcAddress(Handle, 'mysql_set_server_option');
      mysql_shutdown := GetProcAddress(Handle, 'mysql_shutdown');
      mysql_store_result := GetProcAddress(Handle, 'mysql_store_result');
      mysql_thread_end := GetProcAddress(Handle, 'mysql_thread_end');
      mysql_thread_id := GetProcAddress(Handle, 'mysql_thread_id');
      mysql_thread_init := GetProcAddress(Handle, 'mysql_thread_init');
      mysql_thread_save := GetProcAddress(Handle, 'mysql_thread_save');
      mysql_use_result := GetProcAddress(Handle, 'mysql_use_result');
      mysql_warning_count := GetProcAddress(Handle, 'mysql_warning_count');
    end;
  end
  else
  begin
    my_init := nil;
    mysql_affected_rows := @MySQLClient.mysql_affected_rows;
    mysql_character_set_name := @MySQLClient.mysql_character_set_name;
    mysql_close := @MySQLClient.mysql_close;
    mysql_errno := @MySQLClient.mysql_errno;
    mysql_error := @MySQLClient.mysql_error;
    mysql_fetch_field := @MySQLClient.mysql_fetch_field;
    mysql_fetch_fields := @MySQLClient.mysql_fetch_fields;
    mysql_fetch_field_direct := @MySQLClient.mysql_fetch_field_direct;
    mysql_fetch_lengths := @MySQLClient.mysql_fetch_lengths;
    mysql_fetch_row := @MySQLClient.mysql_fetch_row;
    mysql_field_count := @MySQLClient.mysql_field_count;
    mysql_free_result := @MySQLClient.mysql_free_result;
    mysql_get_client_info := @MySQLClient.mysql_get_client_info;
    mysql_get_client_version := @MySQLClient.mysql_get_client_version;
    mysql_get_host_info := @MySQLClient.mysql_get_host_info;
    mysql_get_server_info := @MySQLClient.mysql_get_server_info;
    mysql_get_server_version := @MySQLClient.mysql_get_server_version;
    mysql_info := @MySQLClient.mysql_info;
    if (LibraryType <> ltHTTP) then
      mysql_init := @MySQLClient.mysql_init
    else
      mysql_init := @HTTPTunnel.mysql_init;
    mysql_insert_id := @MySQLClient.mysql_insert_id;
    mysql_more_results := @MySQLClient.mysql_more_results;
    mysql_next_result := @MySQLClient.mysql_next_result;
    mysql_library_end := nil;
    mysql_library_init := nil;
    mysql_num_fields := @MySQLClient.mysql_num_fields;
    mysql_num_rows := @MySQLClient.mysql_num_rows;
    mysql_options := @MySQLClient.mysql_options;
    mysql_ping := @MySQLClient.mysql_ping;
    mysql_real_connect := @MySQLClient.mysql_real_connect;
    mysql_real_escape_string := @MySQLClient.mysql_real_escape_string;
    mysql_real_query := @MySQLClient.mysql_real_query;
    mysql_session_track_get_first := @MySQLClient.mysql_session_track_get_first;
    mysql_session_track_get_next := @MySQLClient.mysql_session_track_get_next;
    mysql_set_character_set := @MySQLClient.mysql_set_character_set;
    mysql_set_local_infile_default := @MySQLClient.mysql_set_local_infile_default;
    mysql_set_local_infile_handler := @MySQLClient.mysql_set_local_infile_handler;
    mysql_set_server_option := @MySQLClient.mysql_set_server_option;
    if (LibraryType <> ltHTTP) then
      mysql_shutdown := @MySQLClient.mysql_shutdown
    else
      mysql_shutdown := nil;
    mysql_store_result := @MySQLClient.mysql_store_result;
    mysql_thread_end := nil;
    mysql_thread_id := @MySQLClient.mysql_thread_id;
    mysql_thread_init := nil;
    mysql_thread_save := nil;
    mysql_use_result := @MySQLClient.mysql_use_result;
    mysql_warning_count := @MySQLClient.mysql_warning_count;
  end;

  if (Assigned(mysql_library_init)) then
    mysql_library_init(0, nil, nil);

  if (Assigned(mysql_get_client_version)) then
    FVersion := mysql_get_client_version()
  else if (Assigned(mysql_get_client_info)) then
  begin
    S := string(mysql_get_client_info());
    if (Pos('-', S) > 0) then
      S := Copy(S, 1, Pos('-', S) - 1);
    if (S[2] = '.') and (S[4] = '.') then
      Insert('0', S, 3);
    if (S[2] = '.') and (Length(S) = 6) then
      Insert('0', S, 6);
    Val(StringReplace(S, '.', '', [rfReplaceAll	]), FVersion, Code);
  end;
end;

destructor TMySQLLibrary.Destroy();
begin
  if (Assigned(mysql_library_end)) then
    mysql_library_end();
  if (Handle > 0) then
    FreeLibrary(Handle);

  inherited;
end;

procedure TMySQLLibrary.SetField(const RawField: MYSQL_FIELD; out Field: TMYSQL_FIELD);
begin
  ZeroMemory(@Field, SizeOf(Field));

  if (Version >= 40101) then
  begin
    Field.name := MYSQL_FIELD_40101(RawField)^.name;
    Field.org_name := MYSQL_FIELD_40101(RawField)^.org_name;
    Field.table := MYSQL_FIELD_40101(RawField)^.table;
    Field.org_table := MYSQL_FIELD_40101(RawField)^.org_table;
    Field.db := MYSQL_FIELD_40101(RawField)^.db;
    Field.catalog := MYSQL_FIELD_40101(RawField)^.catalog;
    Field.def := MYSQL_FIELD_40101(RawField)^.def;
    Field.length := MYSQL_FIELD_40101(RawField)^.length;
    Field.max_length := MYSQL_FIELD_40101(RawField)^.max_length;
    Field.name_length := MYSQL_FIELD_40101(RawField)^.name_length;
    Field.org_name_length := MYSQL_FIELD_40101(RawField)^.org_name_length;
    Field.table_length := MYSQL_FIELD_40101(RawField)^.table_length;
    Field.org_table_length := MYSQL_FIELD_40101(RawField)^.org_table_length;
    Field.db_length := MYSQL_FIELD_40101(RawField)^.db_length;
    Field.catalog_length := MYSQL_FIELD_40101(RawField)^.catalog_length;
    Field.def_length := MYSQL_FIELD_40101(RawField)^.def_length;
    Field.flags := MYSQL_FIELD_40101(RawField)^.flags;
    Field.decimals := MYSQL_FIELD_40101(RawField)^.decimals;
    Field.charsetnr := MYSQL_FIELD_40101(RawField)^.charsetnr;
    Field.field_type := MYSQL_FIELD_40101(RawField)^.field_type;
  end
  else if (Version >= 40100) then
  begin
    Field.name := MYSQL_FIELD_40100(RawField)^.name;
    Field.org_name := MYSQL_FIELD_40100(RawField)^.org_name;
    Field.table := MYSQL_FIELD_40100(RawField)^.table;
    Field.org_table := MYSQL_FIELD_40100(RawField)^.org_table;
    Field.db := MYSQL_FIELD_40100(RawField)^.db;
    Field.catalog := nil;
    Field.def := MYSQL_FIELD_40100(RawField)^.def;
    Field.length := MYSQL_FIELD_40100(RawField)^.length;
    Field.max_length := MYSQL_FIELD_40100(RawField)^.max_length;
    Field.name_length := MYSQL_FIELD_40100(RawField)^.name_length;
    Field.org_name_length := MYSQL_FIELD_40100(RawField)^.org_name_length;
    Field.table_length := MYSQL_FIELD_40100(RawField)^.table_length;
    Field.org_table_length := MYSQL_FIELD_40100(RawField)^.org_table_length;
    Field.db_length := MYSQL_FIELD_40100(RawField)^.db_length;
    Field.catalog_length := 0;
    Field.def_length := MYSQL_FIELD_40100(RawField)^.def_length;
    Field.flags := MYSQL_FIELD_40100(RawField)^.flags;
    Field.decimals := MYSQL_FIELD_40100(RawField)^.decimals;
    Field.charsetnr := MYSQL_FIELD_40100(RawField)^.charsetnr;
    Field.field_type := MYSQL_FIELD_40100(RawField)^.field_type;
  end
  else if (Version >= 40000) then
  begin
    Field.name := MYSQL_FIELD_40000(RawField)^.name;
    Field.org_name := nil;
    Field.table := MYSQL_FIELD_40000(RawField)^.table;
    Field.org_table := MYSQL_FIELD_40000(RawField)^.org_table;
    if (Version = 40017) then
      Field.db := nil // libMySQL.dll 4.0.17 gives back an invalid pointer 
    else
      Field.db := MYSQL_FIELD_40000(RawField)^.db;
    Field.catalog := nil;
    Field.def := MYSQL_FIELD_40000(RawField)^.def;
    Field.length := MYSQL_FIELD_40000(RawField)^.length;
    Field.max_length := MYSQL_FIELD_40000(RawField)^.max_length;
    Field.name_length := 0;
    Field.org_name_length := 0;
    Field.table_length := 0;
    Field.org_table_length := 0;
    Field.db_length := 0;
    Field.catalog_length := 0;
    Field.def_length := 0;
    Field.flags := MYSQL_FIELD_40000(RawField)^.flags;
    Field.decimals := MYSQL_FIELD_40000(RawField)^.decimals;
    Field.charsetnr := 0;
    Field.field_type := MYSQL_FIELD_40000(RawField)^.field_type;
  end
  else
  begin
    Field.name := MYSQL_FIELD_32300(RawField)^.name;
    Field.org_name := nil;
    Field.table := MYSQL_FIELD_32300(RawField)^.table;
    Field.org_table := nil;
    Field.db := nil;
    Field.catalog := nil;
    Field.def := MYSQL_FIELD_32300(RawField)^.def;
    Field.length := MYSQL_FIELD_32300(RawField)^.length;
    Field.max_length := MYSQL_FIELD_32300(RawField)^.max_length;
    Field.name_length := 0;
    Field.org_name_length := 0;
    Field.table_length := 0;
    Field.org_table_length := 0;
    Field.db_length := 0;
    Field.catalog_length := 0;
    Field.def_length := 0;
    Field.flags := MYSQL_FIELD_32300(RawField)^.flags;
    Field.decimals := MYSQL_FIELD_32300(RawField)^.decimals;
    Field.charsetnr := 0;
    Field.field_type := MYSQL_FIELD_32300(RawField)^.field_type;
  end
end;

{ EMySQLError *****************************************************************}

constructor EMySQLError.Create(const Msg: string; const AErrorCode: Integer; const AConnection: TMySQLConnection);
begin
  inherited Create(Msg);

  FConnection := AConnection;
  FErrorCode := AErrorCode;
end;

function TMySQLMonitor.GetCacheText(): string;
begin
  SetLength(Result, Cache.UsedSize);
  if (Cache.UsedSize > 0) then
  begin
    if (Cache.First + Cache.UsedSize <= Cache.MemSize) then
      MoveMemory(@Result[1], @Cache.Mem[Cache.First], Cache.UsedSize * SizeOf(Cache.Mem[0]))
    else
    begin
      MoveMemory(@Result[1], @Cache.Mem[Cache.First], (Cache.MemSize - Cache.First) * SizeOf(Cache.Mem[0]));
      MoveMemory(@Result[1 + Cache.MemSize - Cache.First], Cache.Mem, (Cache.UsedSize - (Cache.MemSize - Cache.First)) * SizeOf(Cache.Mem[0]));
    end;
    while ((Length(Result) > 0) and CharInSet(Result[Length(Result)], [#10,#13])) do
      SetLength(Result, Length(Result) - 1);
  end;
end;

procedure TMySQLMonitor.SetConnection(const AConnection: TMySQLConnection);
begin
  if (Assigned(Connection)) then
    Connection.UnRegisterSQLMonitor(Self);

  FConnection := AConnection;

  if (Assigned(Connection)) then
    Connection.RegisterSQLMonitor(Self);
end;

procedure TMySQLMonitor.SetCacheSize(const ACacheSize: Integer);
begin
  while (Cache.UsedSize > ACacheSize) do
  begin
    Cache.First := (Cache.First + Integer(Cache.Items[0])) mod Cache.MemSize;
    Dec(Cache.UsedSize, Integer(Cache.Items[0]));
    Cache.Items.Delete(0);
  end;

  MoveMemory(Cache.Mem, @Cache.Mem[Cache.First], Cache.UsedSize * SizeOf(Cache.Mem[0]));
  Cache.First := 0;

  Cache.MaxSize := ACacheSize;

  Cache.MemSize := Cache.MaxSize;
  ReallocMem(Cache.Mem, Cache.MemSize * SizeOf(Cache.Mem[0]));
end;

procedure TMySQLMonitor.SetOnMonitor(const AOnMonitor: TMySQLOnMonitor);
begin
  FOnMonitor := AOnMonitor;
end;

procedure TMySQLMonitor.DoMonitor(const Connection: TMySQLConnection; const Text: PChar; const Length: Integer; const ATraceType: TTraceType);
var
  CacheLength: Integer;
  CacheText: PChar;
  Pos: Integer;
begin
  if ((Cache.MaxSize > 0) and (Length > 0)) then
  begin
    CacheText := Text; CacheLength := Length;
    while ((CacheLength > 0) and CharInSet(CacheText[0], [#9,#10,#13,' '])) do
    begin
      CacheText := @CacheText[1];
      Dec(CacheLength);
    end;

    while ((CacheLength > 0) and CharInSet(CacheText[CacheLength - 1], [#9,#10,#13,' ',';'])) do
      Dec(CacheLength);

    if (ATraceType in [ttRequest, ttResult]) then
      Inc(CacheLength, 3) // ';' + New Line
    else
      Inc(CacheLength, 2); // New Line

    if ((CacheLength > Cache.MemSize) or (Cache.MemSize > Cache.MaxSize)) then
    begin
      Clear();

      Cache.MemSize := Max(Cache.MaxSize, CacheLength);
      ReallocMem(Cache.Mem, Cache.MemSize * SizeOf(Cache.Mem[0]));
      MoveMemory(Cache.Mem, CacheText, CacheLength * SizeOf(Cache.Mem[0]));
    end
    else
    begin
      while ((Cache.UsedSize + CacheLength > Cache.MemSize) and (Cache.Items.Count > 0)) do
      begin
        Inc(Cache.First, Integer(Cache.Items[0])); if (Cache.First >= Cache.MemSize) then Dec(Cache.First, Cache.MemSize);
        Dec(Cache.UsedSize, Integer(Cache.Items[0]));
        Cache.Items.Delete(0);
      end;

      Pos := (Cache.First + Cache.UsedSize) mod Cache.MemSize;
      if (Pos + CacheLength <= Cache.MemSize) then
        MoveMemory(@Cache.Mem[Pos], CacheText, CacheLength * SizeOf(Cache.Mem[0]))
      else
      begin
        MoveMemory(@Cache.Mem[Pos], CacheText, (Cache.MemSize - Pos) * SizeOf(Cache.Mem[0]));
        MoveMemory(Cache.Mem, @CacheText[Cache.MemSize - Pos], (CacheLength - (Cache.MemSize - Pos)) * SizeOf(Cache.Mem[0]));
      end;
    end;

    Inc(Cache.UsedSize, CacheLength);
    Cache.Items.Add(Pointer(CacheLength));

    Pos := (Cache.First + Cache.UsedSize) mod Cache.MemSize;
    case (Pos) of
      0:
        begin
          if (ATraceType in [ttRequest, ttResult]) then
            Cache.Mem[Cache.MemSize - 3] := ';';
          Cache.Mem[Cache.MemSize - 2] := #13;
          Cache.Mem[Cache.MemSize - 1] := #10;
        end;
      1:
        begin
          if (ATraceType in [ttRequest, ttResult]) then
            Cache.Mem[Cache.MemSize - 2] := ';';
          Cache.Mem[Cache.MemSize - 1] := #13;
          Cache.Mem[0] := #10;
        end;
      2:
        begin
          if (ATraceType in [ttRequest, ttResult]) then
            Cache.Mem[Cache.MemSize - 1] := ';';
          Cache.Mem[1] := #13;
          Cache.Mem[0] := #10;
        end;
      else
        begin
          if (ATraceType in [ttRequest, ttResult]) then
            Cache.Mem[Pos - 3] := ';';
          Cache.Mem[Pos - 2] := #13;
          Cache.Mem[Pos - 1] := #10;
        end;
    end;
  end;

  if (Enabled and Assigned(OnMonitor)) then
    OnMonitor(Connection, Text, Length, ATraceType);
end;

{ TMySQLMonitor ***************************************************************}

procedure TMySQLMonitor.Clear();
begin
  Cache.First := 0;
  Cache.UsedSize := 0;
  Cache.Items.Clear();
end;

constructor TMySQLMonitor.Create(AOwner: TComponent);
begin
  inherited;

  FConnection := nil;
  FEnabled := False;
  FOnMonitor := nil;
  FCacheSize := 0;
  FTraceTypes := [ttRequest, ttDebug];

  Cache.First := 0;
  Cache.Items := TList.Create();
  Cache.MemSize := FCacheSize;
  GetMem(Cache.Mem, Cache.MemSize * SizeOf(Cache.Mem[0]));
  Cache.UsedSize := 0;
end;

destructor TMySQLMonitor.Destroy();
begin
  if (Assigned(Connection)) then
    Connection.UnRegisterSQLMonitor(Self);

  FreeMem(Cache.Mem);
  Cache.Items.Free();

  inherited;
end;

{ TMySQLConnection.TTerminatedThreads *****************************************}

function TMySQLConnection.TTerminatedThreads.Add(const Item: Pointer): Integer;
begin
  CriticalSection.Enter();

  Result := inherited Add(Item);

  CriticalSection.Leave();
end;

constructor TMySQLConnection.TTerminatedThreads.Create(const AConnection: TMySQLConnection);
begin
  inherited Create();

  FConnection := AConnection;

  CriticalSection := TCriticalSection.Create();
end;

procedure TMySQLConnection.TTerminatedThreads.Delete(const Item: Pointer);
var
  Index: Integer;
begin
  CriticalSection.Enter();

  Index := IndexOf(Item);
  if (Index >= 0) then
    Delete(Index);

  CriticalSection.Leave();
end;

destructor TMySQLConnection.TTerminatedThreads.Destroy();
begin
  CriticalSection.Enter();

  while (Count > 0) do
  begin
    TerminateThread(TThread(Items[0]).Handle, 1);
    inherited Delete(0);
  end;

  CriticalSection.Leave();

  CriticalSection.Free();

  inherited;
end;

{ TMySQLConnection.TSyncThread *********************************************}

constructor TMySQLConnection.TSyncThread.Create(const AConnection: TMySQLConnection);
begin
  inherited Create(False);

  FConnection := AConnection;

  RunExecute := TEvent.Create(nil, True, False, '');
  SetLength(CLStmts, 0);
  StmtLengths := TList.Create();
  State := ssClose;
end;

destructor TMySQLConnection.TSyncThread.Destroy();
begin
  RunExecute.Free(); RunExecute := nil;
  SetLength(CLStmts, 0);
  StmtLengths.Free();

  inherited;
end;

procedure TMySQLConnection.TSyncThread.Execute();
var
  Timeout: LongWord;
  WaitResult: TWaitResult;
begin
  {$IFDEF EurekaLog}
  try
  {$ENDIF}

  while (not Terminated) do
  begin
    if ((Connection.ServerTimeout = 0) or (Connection.LibraryType = ltHTTP)) then
      Timeout := INFINITE
    else
      Timeout := Connection.ServerTimeout * 1000;
    WaitResult := RunExecute.WaitFor(Timeout);

    if (not Terminated) then
      if (WaitResult = wrTimeout) then
      begin
        if (State = ssReady) then
          Connection.SyncPing(Self);
      end
      else
      begin
        case (State) of
          ssConnecting:
            Connection.SyncConnecting(Self);
          ssExecutingFirst:
            Connection.SyncExecutingFirst(Self);
          ssExecutingNext:
            Connection.SyncExecutingNext(Self);
          ssReceivingResult:
            Connection.SyncReceivingResult(Self);
          ssDisconnecting:
            Connection.SyncDisconnecting(Self);
          else
            raise Exception.CreateFMT(SOutOfSync + ' (State: %d)', [Ord(State)]);
        end;

        Connection.TerminateCS.Enter();
        RunExecute.ResetEvent();
        if (not Terminated) then
          MySQLConnectionSynchronizeRequest(Self);
        Connection.TerminateCS.Leave();
      end;
  end;

  Connection.TerminateCS.Enter();
  Connection.TerminatedThreads.Delete(Self);
  Connection.TerminateCS.Leave();

  {$IFDEF EurekaLog}
  except
    StandardEurekaNotify(GetLastExceptionObject(), GetLastExceptionAddress());
  end;
  {$ENDIF}
end;

function TMySQLConnection.TSyncThread.GetCommandText(): string;
var
  EndingCommentLength: Integer;
  StartingCommentLength: Integer;
  StmtLength: Integer;
begin
  if (StmtIndex = StmtLengths.Count) then
    Result := ''
  else
  begin
    StmtLength := SQLTrimStmt(PChar(@SQL[SQLIndex]), Integer(StmtLengths[StmtIndex]), Connection.MySQLVersion, StartingCommentLength, EndingCommentLength);
    if (SQL[SQLIndex + StartingCommentLength + StmtLength - 1] = ';') then
      Dec(StmtLength);
    if (StmtLength = 0) then
      Result := ''
    else
      SetString(Result, PChar(@SQL[SQLIndex + StartingCommentLength]), StmtLength);
  end;
end;

function TMySQLConnection.TSyncThread.GetIsRunning(): Boolean;
begin
  Result := not Terminated
    and ((RunExecute.WaitFor(IGNORE) = wrSignaled) or not (State in [ssClose, ssReady]));
end;

function TMySQLConnection.TSyncThread.GetNextCommandText(): string;
var
  EndingCommentLength: Integer;
  Len: Integer;
  StartingCommentLength: Integer;
  StmtLength: Integer;
begin
  if (StmtIndex + 1 = StmtLengths.Count) then
    Result := ''
  else
  begin
    StmtLength := Integer(StmtLengths[StmtIndex + 1]);
    Len := SQLTrimStmt(SQL, SQLIndex, StmtLength, Connection.MySQLVersion, StartingCommentLength, EndingCommentLength);
    Result := Copy(SQL, SQLIndex + Integer(StmtLengths[StmtIndex]) + StartingCommentLength, Len);
  end;
end;

procedure TMySQLConnection.TSyncThread.Synchronize();
begin
  if (not Terminated) then
    case (State) of
      ssConnecting:
        Connection.SyncConnected(Self);
      ssExecutingFirst,
      ssExecutingNext:
        begin
          Connection.SyncExecuted(Self);
          if (Mode in [smSQL, smDataSet]) then
            if (State in [ssExecutingNext, ssExecutingFirst]) then
            begin
              Connection.SyncExecute(Self);
              RunExecute.SetEvent();
            end;
        end;
	    ssReceivingResult:
        begin
          if (DataSet.SyncThread.ErrorCode <> 0) then
            Connection.DoError(DataSet.SyncThread.ErrorCode, DataSet.SyncThread.ErrorMessage);
          Connection.SyncReleaseDataSet(DataSet);
          if (Mode in [smSQL, smDataSet]) then
            if (State in [ssExecutingNext, ssExecutingFirst]) then
            begin
              Connection.SyncExecute(Self);
              RunExecute.SetEvent();
            end
            else if (State = ssReady) then
              Connection.SyncAfterExecuteSQL(Self);
        end;
      ssDisconnecting:
        Connection.SyncDisconnected(Self);
      else
        raise Exception.CreateFMT(SOutOfSync + ' (State: %d)', [Ord(State)]);
    end;
end;

procedure TMySQLConnection.TSyncThread.Terminate();
var
  Index: Integer;
begin
  SynchronizingThreadsCS.Enter();
  Index := SynchronizingThreads.IndexOf(Self);
  if (Index >= 0) then
    SynchronizingThreads.Delete(Index);
  SynchronizingThreadsCS.Leave();

  inherited;

  if (RunExecute.WaitFor(IGNORE) <> wrSignaled) then
    RunExecute.SetEvent();

  Connection.TerminatedThreads.Add(Self);
  if (Connection.SyncThread = Self) then
    Connection.FSyncThread := nil;
end;

{ TMySQLConnection ************************************************************}

procedure TMySQLConnection.BeginSilent();
begin
  Inc(SilentCount);
end;

procedure TMySQLConnection.BeginSynchron();
begin
  Inc(SynchronCount);
end;

function TMySQLConnection.CharsetToCharsetNr(const Charset: string): Byte;
var
  I: Integer;
begin
  Result := 0;

  if (MySQLVersion < 40101) then
  begin
    for I := 0 to Length(MySQL_Character_Sets) - 1 do
      if (lstrcmpiA(PAnsiChar(AnsiString(Charset)), MySQL_Character_Sets[I].CharsetName) = 0) then
        Result := I;
  end
  else
  begin
    for I := 0 to Length(MySQL_Collations) - 1 do
      if (MySQL_Collations[I].Default and (lstrcmpiA(PAnsiChar(AnsiString(Charset)), MySQL_Collations[I].CharsetName) = 0)) then
        Result := MySQL_Collations[I].CharsetNr;
  end;
end;

function TMySQLConnection.CharsetToCodePage(const Charset: string): Cardinal;
var
  I: Integer;
begin
  Result := CP_ACP;

  if (MySQLVersion < 40101) then
  begin
    for I := 0 to Length(MySQL_Character_Sets) - 1 do
      if (lstrcmpiA(PAnsiChar(AnsiString(Charset)), MySQL_Character_Sets[I].CharsetName) = 0) then
        Result := MySQL_Character_Sets[I].CodePage;
  end
  else
  begin
    for I := 0 to Length(MySQL_Collations) - 1 do
      if (MySQL_Collations[I].Default and (lstrcmpiA(PAnsiChar(AnsiString(Charset)), MySQL_Collations[I].CharsetName) = 0)) then
        Result := MySQL_Collations[I].CodePage;
  end;
end;

procedure TMySQLConnection.CloseResult(const DataHandle: TDataResult);
begin
  TerminateCS.Enter();
  if (Asynchron and (DataHandle.State <> ssReady)) then
    DataHandle.Terminate();
  TerminateCS.Leave();
end;

function TMySQLConnection.CodePageToCharset(const CodePage: Cardinal): string;
var
  I: Integer;
begin
  Result := '';

  if (MySQLVersion < 40101) then
  begin
    for I := 0 to Length(MySQL_Character_Sets) - 1 do
      if ((Result = '') and (MySQL_Character_Sets[I].CodePage = CodePage)) then
        Result := string(StrPas(MySQL_Character_Sets[I].CharsetName));
  end
  else
  begin
    for I := 0 to Length(MySQL_Collations) - 1 do
      if ((Result = '') and (MySQL_Collations[I].CodePage = CodePage)) then
        Result := string(StrPas(MySQL_Collations[I].CharsetName));
  end;
end;

constructor TMySQLConnection.Create(AOwner: TComponent);
begin
  inherited;

  FFormatSettings := TFormatSettings.Create(GetSystemDefaultLCID());
  FFormatSettings.ThousandSeparator := #0;
  FFormatSettings.DecimalSeparator := '.';
  FFormatSettings.ShortDateFormat := 'yyyy/MM/dd';
  FFormatSettings.LongDateFormat := FFormatSettings.ShortDateFormat;
  FFormatSettings.LongTimeFormat := 'hh:mm:ss';
  FFormatSettings.DateSeparator := '-';
  FFormatSettings.TimeSeparator := ':';

  ExecuteSQLDone := TEvent.Create(nil, False, False, 'ExecuteSQLDone');
  FAfterExecuteSQL := nil;
  FAnsiQuotes := False;
  FAsynchron := False;
  FBeforeExecuteSQL := nil;
  FCharset := 'utf8';
  FCharsetNr := 33;
  FCodePage := CP_UTF8;
  FConnected := False;
  FDatabaseName := '';
  FExecutionTime := 0;
  FHost := '';
  FHTTPAgent := '';
  FIdentifierQuoted := True;
  FIdentifierQuoter := '`';
  FLatestConnect := 0;
  FLib := nil;
  FSyncThread := nil;
  FLibraryType := ltBuiltIn;
  FMariaDBVersion := 0;
  FMultiStatements := True;
  FOnConvertError := nil;
  FOnSQLError := nil;
  FOnUpdateIndexDefs := nil;
  FPassword := '';
  FPort := MYSQL_PORT;
  FServerTimeout := 0;
  FMySQLVersion := 0;
  FServerVersionStr := '';
  FTerminateCS := TCriticalSection.Create();
  FTerminatedThreads := TTerminatedThreads.Create(Self);
  FThreadDeep := 0;
  FThreadId := 0;
  FUserName := '';
  InMonitor := False;
  InOnResult := False;
  TimeDiff := 0;
  SilentCount := 0;

  FBugMonitor := TMySQLMonitor.Create(nil);
  FBugMonitor.Connection := Self;
  FBugMonitor.CacheSize := 1000;
  FBugMonitor.Enabled := True;
  FBugMonitor.TraceTypes := [ttTime, ttRequest, ttInfo, ttDebug];
end;

destructor TMySQLConnection.Destroy();
var
  I: Integer;
  TempSyncThread: TSyncThread;
begin
  Asynchron := False;
  Close();

  while (DataSetCount > 0) do
    DataSets[0].Free();

  TempSyncThread := SyncThread;
  TerminateCS.Enter();
  if (Assigned(SyncThread)) then
    SyncThread.Terminate();
  TerminateCS.Leave();
  if (Assigned(TempSyncThread)) then
  begin
    TempSyncThread.WaitFor();
    TempSyncThread.Free();
  end;
  TerminateCS.Enter();
  for I := 0 to TerminatedThreads.Count - 1 do
    TerminateThread(TThread(TerminatedThreads[I]).Handle, 0);
  TerminateCS.Leave();
  TerminatedThreads.Free();

  ExecuteSQLDone.Free();
  TerminateCS.Free();
  FBugMonitor.Free();

  inherited;
end;

procedure TMySQLConnection.DoAfterExecuteSQL();
begin
  if (Assigned(AfterExecuteSQL)) then AfterExecuteSQL(Self);
end;

procedure TMySQLConnection.DoBeforeExecuteSQL();
begin
  if (Assigned(BeforeExecuteSQL)) then BeforeExecuteSQL(Self);
end;

procedure TMySQLConnection.DoConnect();
begin
  Assert(not Connected);

  FExecutionTime := 0;
  FErrorCode := DS_ASYNCHRON;
  FErrorMessage := '';
  FErrorCommandText := '';
  FWarningCount := 0;

  if (Assigned(SyncThread) and (SyncThread.State <> ssClose)) then
    Terminate();
  if (not Assigned(FSyncThread)) then
    FSyncThread := TSyncThread.Create(Self);

  SyncThread.State := ssConnecting;
  if (not UseSyncThread()) then
  begin
    SyncConnecting(SyncThread);
    SyncConnected(SyncThread);
  end
  else
    SyncThread.RunExecute.SetEvent();
end;

procedure TMySQLConnection.DoConvertError(const Sender: TObject; const Text: string; const Error: EConvertError);
begin
  if (Assigned(FOnConvertError)) then
    FOnConvertError(Sender, Text)
  else
    raise Error;
end;

procedure TMySQLConnection.DoDatabaseChange(const NewName: string);
begin
  FDatabaseName := NewName;

  if (Assigned(FOnDatabaseChange)) then
    FOnDatabaseChange(Self, NewName);
end;

procedure TMySQLConnection.DoDisconnect();
begin
  Assert(Connected);

  SendConnectEvent(False);

  if (Assigned(SyncThread) and (SyncThread.State <> ssReady)) then
    Terminate();

  FErrorCode := DS_ASYNCHRON;
  FErrorMessage := '';
  FErrorCommandText := '';
  FWarningCount := 0;

  if (not Assigned(SyncThread) or not Assigned(SyncThread.LibHandle)) then
    SyncDisconnected(nil)
  else
  begin
    SyncThread.State := ssDisconnecting;
    if (not UseSyncThread()) then
    begin
      SyncDisconnecting(SyncThread);
      SyncDisconnected(SyncThread);
    end
    else
      SyncThread.RunExecute.SetEvent();
  end;
end;

procedure TMySQLConnection.DoError(const AErrorCode: Integer; const AErrorMessage: string);
begin
  if ((ErrorCode = CR_UNKNOWN_HOST)
    or (ErrorCode = DS_SERVER_OLD)) then
    Close()
  else if ((ErrorCode = CR_UNKNOWN_ERROR)
    or (ErrorCode = CR_IPSOCK_ERROR)
    or (ErrorCode = CR_SERVER_GONE_ERROR)
    or (ErrorCode = CR_SERVER_HANDSHAKE_ERR)
    or (ErrorCode = CR_SERVER_LOST)
    or (ErrorCode = CR_COMMANDS_OUT_OF_SYNC)) then
    SyncDisconnected(SyncThread);

  FErrorCode := AErrorCode;
  FErrorMessage := AErrorMessage;
  if (not Assigned(SyncThread)) then
    FErrorCommandText := ''
  else
    FErrorCommandText := SyncThread.CommandText + ';' + #13#10;

  if (SilentCount = 0) then
    if (not Assigned(FOnSQLError)) then
      raise EMySQLError.Create(ErrorMessage, ErrorCode, Self)
    else
      FOnSQLError(Self, ErrorCode, ErrorMessage);
end;

procedure TMySQLConnection.DoVariableChange(const Name, NewValue: string);
begin
  if (Assigned(FOnVariableChange)) then
    FOnVariableChange(Self, Name, NewValue);
end;

procedure TMySQLConnection.EndSilent();
begin
  if (SilentCount > 0) then
    Dec(SilentCount);
end;

procedure TMySQLConnection.EndSynchron();
begin
  if (SynchronCount > 0) then
    Dec(SynchronCount);
end;

function TMySQLConnection.EscapeIdentifier(const Identifier: string): string;
begin
  Result := IdentifierQuoter + Identifier + IdentifierQuoter;
end;

function TMySQLConnection.ExecuteSQL(const SQL: string; const OnResult: TResultEvent = nil): Boolean;
begin
  Result := InternExecuteSQL(smSQL, True, SQL, OnResult);
end;

function TMySQLConnection.FirstResult(out DataHandle: TMySQLConnection.TDataResult; const SQL: string): Boolean;
begin
  Result := InternExecuteSQL(smDataHandle, True, SQL);

  DataHandle := SyncThread;
end;

function TMySQLConnection.GetConnected(): Boolean;
begin
  Result := FConnected;
end;

function TMySQLConnection.GetDataFileAllowed(): Boolean;
begin
  Result := Assigned(Lib) and not (Lib.FLibraryType in [ltHTTP]);
end;

function TMySQLConnection.GetErrorMessage(const AHandle: MySQLConsts.MYSQL): string;
var
  RBS: RawByteString;
begin
  RBS := Lib.mysql_error(AHandle);
  try
    Result := LibDecode(my_char(RBS));
  except
    Result := string(RBS);
  end;
end;

function TMySQLConnection.GetHandle(): MySQLConsts.MYSQL;
begin
  if (not Assigned(SyncThread)) then
    Result := nil
  else
    Result := SyncThread.LibHandle;
end;

function TMySQLConnection.GetInfo(): string;
var
  Info: my_char;
begin
  if (not Assigned(Handle)) then
    Result := ''
  else
  begin
    Info := Lib.mysql_info(Handle);
    try
      Result := '--> ' + LibDecode(Info);
    except
      Result := '--> ' + string(Info);
    end;
  end;
end;

function TMySQLConnection.GetInsertId(): my_ulonglong;
begin
  if (not Assigned(Handle)) then
    Result := 0
  else
    Result := Lib.mysql_insert_id(Handle);
end;

function TMySQLConnection.GetMaxAllowedServerPacket(): Integer;
begin
  // MAX_ALLOWED_PACKET Constante of the Server - SizeOf(COM_QUERY)
  Result := 1 * 1024 * 1024 - 1;
end;

function TMySQLConnection.GetNextCommandText(): string;
begin
  if (not Assigned(SyncThread) or not SyncThread.IsRunning) then
    Result := ''
  else
    Result := SyncThread.NextCommandText;
end;

function TMySQLConnection.GetServerDateTime(): TDateTime;
begin
  Result := Now() + TimeDiff;
end;

function TMySQLConnection.InternExecuteSQL(const Mode: TSyncThread.TMode; const Synchron: Boolean;
  const SQL: string; const OnResult: TResultEvent = nil; const Done: TEvent = nil): Boolean;
var
  CLStmt: TSQLCLStmt;
  SetNames: Boolean;
  SQLIndex: Integer;
  StmtIndex: Integer;
  StmtLength: Integer;
begin
  Assert(SQL <> '');
  Assert(not Assigned(Done) or (Mode = smSQL));
  Assert(not Assigned(Done) or (Done.WaitFor(IGNORE) <> wrSignaled));

  if (InOnResult) then
    raise Exception.Create(SOutOfSync + ' (in OnResult): ' + SyncThread.CommandText);
  if (InMonitor) then
    raise Exception.Create(SOutOfSync + ' (in Monitor): ' + SyncThread.CommandText);

  if (Assigned(SyncThread) and not (SyncThread.State in [ssClose, ssReady])) then
    Terminate();

  if (not Assigned(SyncThread)) then
    FSyncThread := TSyncThread.Create(Self);

  SetLength(SyncThread.CLStmts, 0);
  SyncThread.DataSet := nil;
  SyncThread.ExecutionTime := 0;
  SyncThread.Mode := Mode;
  SyncThread.OnResult := OnResult;
  SyncThread.Done := Done;
  SyncThread.SQLIndex := 1;
  SyncThread.StmtIndex := 0;
  SyncThread.StmtLengths.Clear();

  if (KillThreadId = 0) then
    SyncThread.SQL := SQL
  else if (MySQLVersion < 50000) then
    SyncThread.SQL := 'KILL ' + IntToStr(ThreadId) + ';' + #13#10 + SQL
  else
    SyncThread.SQL := 'KILL CONNECTION ' + IntToStr(ThreadId) + ';' + #13#10 + SQL;


  FErrorCode := DS_ASYNCHRON;
  FErrorMessage := '';
  FErrorCommandText := '';
  FWarningCount := 0;
  FSuccessfullExecutedSQLLength := 0; FExecutedStmts := 0;
  FRowsAffected := -1; FExecutionTime := 0;

  SQLIndex := 1;
  while (SQLIndex < Length(SyncThread.SQL)) do
  begin
    StmtLength := SQLStmtLength(PChar(@SyncThread.SQL[SQLIndex]), Length(SyncThread.SQL) - (SQLIndex - 1));
    SyncThread.StmtLengths.Add(Pointer(StmtLength));
    Inc(SQLIndex, StmtLength);
  end;

  SetLength(SyncThread.CLStmts, SyncThread.StmtLengths.Count);
  SQLIndex := 1; SetNames := False;
  for StmtIndex := 0 to SyncThread.StmtLengths.Count - 1 do
    if (not SetNames) then
    begin
      StmtLength := Integer(SyncThread.StmtLengths[StmtIndex]);
      SyncThread.CLStmts[StmtIndex] := SQLParseCLStmt(CLStmt, @SyncThread.SQL[SQLIndex], StmtLength, MySQLVersion);
      SetNames := SyncThread.CLStmts[StmtIndex] and (CLStmt.CommandType in [ctSetNames, ctSetCharacterSet, ctSetCharset]);
      Inc(SQLIndex, StmtLength);
    end;

  if (SyncThread.SQL = '') then
    raise EDatabaseError.Create('Empty query')
  else if (SyncThread.StmtLengths.Count = 0) then
    Result := False
  else if (SetNames) then
  begin
    DoError(DS_SET_NAMES, StrPas(DATASET_ERRORS[DS_SET_NAMES - DS_MIN_ERROR]));
    Result := False;
  end
  else
  begin
    DoBeforeExecuteSQL();

    SyncThread.State := ssExecutingFirst;

    if (Mode = smDataHandle) then
    begin
      SyncExecute(SyncThread);
      SyncExecutingFirst(SyncThread);
      SyncExecuted(SyncThread);
      Result := SyncThread.ErrorCode = 0;
    end
    else if (Synchron or not UseSyncThread()) then
    begin
      repeat
        SyncExecute(SyncThread);
        SyncExecutingFirst(SyncThread);
        SyncExecuted(SyncThread);
        while (SyncThread.State = ssExecutingNext) do
        begin
          SyncExecute(SyncThread);
          SyncExecutingNext(SyncThread);
          SyncExecuted(SyncThread);
        end;
      until (SyncThread.State <> ssExecutingFirst);
      Result := SyncThread.ErrorCode = 0;
    end
    else
    begin
      SyncExecute(SyncThread);
      SyncThread.RunExecute.SetEvent();
      Result := False;
    end;
  end;
end;

function TMySQLConnection.InUse(): Boolean;
begin
  TerminateCS.Enter();
  Result := Assigned(SyncThread) and SyncThread.IsRunning;
  TerminateCS.Leave();
end;

function TMySQLConnection.LibDecode(const Text: my_char; const Length: my_int = -1): string;
label
  StringL;
var
  Len: Integer;
begin
  if (Length >= 0) then
    Len := Length
  else if (Assigned(Text)) then
    Len := lstrlenA(Text)
  else
    Len := 0;
  SetLength(Result, AnsiCharToWideChar(CodePage, Text, Len, nil, 0));
  if (Len > 0) then
    SetLength(Result, AnsiCharToWideChar(CodePage, Text, Len, PChar(Result), System.Length(Result)));
end;

function TMySQLConnection.LibEncode(const Value: string): RawByteString;
var
  Len: Integer;
begin
  Len := WideCharToAnsiChar(CodePage, PChar(Value), Length(Value), nil, 0);
  SetLength(Result, Len);
  WideCharToAnsiChar(CodePage, PChar(Value), Length(Value), PAnsiChar(Result), Len);
end;

function TMySQLConnection.LibPack(const Value: string): RawByteString;
label
  StringL;
var
  Len: Integer;
begin
  if (Value = '') then
    Result := ''
  else
  begin
    Len := Length(Value);
    SetLength(Result, Len);
    asm
        PUSH ES
        PUSH ESI
        PUSH EDI

        PUSH DS                          // string operations uses ES
        POP ES
        CLD                              // string operations uses forward direction

        MOV ESI,PChar(Value)             // Copy characters from Value
        MOV EAX,Result                   //   to Result
        MOV EDI,[EAX]

        MOV ECX,Len
      StringL:
        LODSW                            // Load WideChar from Value
        STOSB                            // Store AnsiChar into Result
        LOOP StringL                     // Repeat for all characters

        POP EDI
        POP ESI
        POP ES
    end;
  end;
end;

function TMySQLConnection.LibUnpack(const Data: my_char; const Length: my_int = -1): string;
label
  StringL;
var
  Len: Integer;
begin
  if (Length = -1) then
    Len := lstrlenA(Data)
  else
    Len := Length;
  if (Len = 0) then
    Result := ''
  else
  begin
    SetLength(Result, Len);
    asm
        PUSH ES
        PUSH ESI
        PUSH EDI

        PUSH DS                          // string operations uses ES
        POP ES
        CLD                              // string operations uses forward direction

        MOV ESI,Data                     // Copy characters from Data
        MOV EAX,Result                   //   to Result
        MOV EDI,[EAX]

        MOV AH,0                         // Clear AH, since AL will be load but AX stored
        MOV ECX,Len
      StringL:
        LODSB                            // Load AnsiChar from Data
        STOSW                            // Store WideChar into S
        LOOP StringL                     // Repeat for all characters

        POP EDI
        POP ESI
        POP ES
    end;
  end;
end;

procedure TMySQLConnection.local_infile_end(const local_infile: Plocal_infile);
begin
  if (local_infile^.Handle <> INVALID_HANDLE_VALUE) then
    CloseHandle(local_infile^.Handle);
  if (Assigned(local_infile^.Buffer)) then
    VirtualFree(local_infile^.Buffer, local_infile^.BufferSize, MEM_RELEASE);

  FreeMem(local_infile);
end;

function TMySQLConnection.local_infile_error(const local_infile: Plocal_infile; const error_msg: my_char; const error_msg_len: my_uint): my_int;
var
  Buffer: PChar;
  Len: Integer;
begin
  Buffer := nil;
  Len := FormatMessage(FORMAT_MESSAGE_ALLOCATE_BUFFER or FORMAT_MESSAGE_FROM_SYSTEM or FORMAT_MESSAGE_IGNORE_INSERTS, nil, local_infile^.LastError, 0, @Buffer, 0, nil);
  if (Len > 0) then
  begin
    Len := WideCharToAnsiChar(CodePage, Buffer, Len, error_msg, error_msg_len);
    error_msg[Len] := #0;
    LocalFree(HLOCAL(Buffer));
  end
  else if (GetLastError() = 0) then
    RaiseLastOSError();
  Result := local_infile^.ErrorCode;
end;

function TMySQLConnection.local_infile_init(out local_infile: Plocal_infile; const filename: my_char): my_int;
begin
  GetMem(local_infile, SizeOf(local_infile^));
  ZeroMemory(local_infile, SizeOf(local_infile^));
  local_infile^.Buffer := nil;
  local_infile^.Connection := Self;
  local_infile^.Position := 0;

  if ((AnsiCharToWideChar(CodePage, filename, lstrlenA(filename), @local_infile^.Filename, Length(local_infile^.Filename)) = 0) and (GetLastError() <> 0)) then
  begin
    local_infile^.ErrorCode := EE_FILENOTFOUND;
    local_infile^.LastError := GetLastError();
  end
  else
  begin
    local_infile^.Handle := CreateFile(@local_infile^.Filename, GENERIC_READ, FILE_SHARE_READ, nil, OPEN_EXISTING, FILE_ATTRIBUTE_NORMAL, 0);
    if (local_infile^.Handle = INVALID_HANDLE_VALUE) then
    begin
      local_infile^.ErrorCode := EE_FILENOTFOUND;
      local_infile^.LastError := GetLastError();
    end
    else
    begin
      local_infile^.ErrorCode := 0;
      local_infile^.LastError := 0;
    end;
  end;

  Result := local_infile^.ErrorCode;
end;

function TMySQLConnection.local_infile_read(const local_infile: Plocal_infile; buf: my_char; const buf_len: my_uint): my_int;
var
  BytesPerSector: DWord;
  NumberofFreeClusters: DWord;
  SectorsPerCluser: DWord;
  Size: DWord;
  TotalNumberOfClusters: DWord;
begin
  if (not Assigned(local_infile^.Buffer)) then
  begin
    local_infile^.BufferSize := buf_len;
    if (GetFileSize(local_infile^.Handle, nil) > 0) then
      local_infile^.BufferSize := Min(GetFileSize(local_infile^.Handle, nil), local_infile^.BufferSize);

    if (GetDiskFreeSpace(PChar(ExtractFileDrive(local_infile^.Filename)), SectorsPerCluser, BytesPerSector, NumberofFreeClusters, TotalNumberOfClusters)
      and (local_infile^.BufferSize mod BytesPerSector > 0)) then
      Inc(local_infile^.BufferSize, BytesPerSector - local_infile^.BufferSize mod BytesPerSector);
    local_infile^.Buffer := VirtualAlloc(nil, local_infile^.BufferSize, MEM_COMMIT, PAGE_READWRITE);

    if (not Assigned(local_infile^.Buffer)) then
      local_infile^.ErrorCode := EE_OUTOFMEMORY;
  end;

  if (not ReadFile(local_infile^.Handle, local_infile^.Buffer^, Min(buf_len, local_infile^.BufferSize), Size, nil)) then
  begin
    local_infile^.LastError := GetLastError();
    local_infile^.ErrorCode := EE_READ;
    Result := -1;
  end
  else
  begin
    MoveMemory(buf, local_infile^.Buffer, Size);
    Inc(local_infile^.Position, Size);
    Result := Size;
  end;
end;

function TMySQLConnection.NextResult(const DataHandle: TMySQLConnection.TDataResult): Boolean;
begin
  // Debug 2016-11-11
  if (not Assigned(DataHandle)) then
    raise ERangeError.CreateFmt(SPropertyOutOfRange, ['DataHandle']);

  SyncExecute(DataHandle);
  SyncExecutingNext(DataHandle);
  SyncExecuted(DataHandle);

  Result := DataHandle.ErrorCode = 0;
end;

procedure TMySQLConnection.RegisterSQLMonitor(const AMySQLMonitor: TMySQLMonitor);
begin
  SetLength(FSQLMonitors, Length(FSQLMonitors) + 1);

  FSQLMonitors[Length(FSQLMonitors) - 1] := AMySQLMonitor;
end;

function TMySQLConnection.SendSQL(const SQL: string; const Done: TEvent): Boolean;
begin
  Result := InternExecuteSQL(smSQL, False, SQL, TResultEvent(nil), Done) and not UseSyncThread();
end;

function TMySQLConnection.SendSQL(const SQL: string; const OnResult: TResultEvent = nil; const Done: TEvent = nil): Boolean;
begin
  Result := InternExecuteSQL(smSQL, False, SQL, OnResult, Done) and not UseSyncThread();
end;

procedure TMySQLConnection.SetAnsiQuotes(const AAnsiQuotes: Boolean);
begin
  FAnsiQuotes := AAnsiQuotes;

  if (AnsiQuotes) then
    FIdentifierQuoter := '"'
  else
    FIdentifierQuoter := '`';
end;

procedure TMySQLConnection.SetCharset(const ACharset: string);
begin
  if ((ACharset <> '') and (ACharset <> Charset)) then
  begin
    FCharset := LowerCase(ACharset);
    FCharsetNr := CharsetToCharsetNr(FCharset);
    FCodePage := CharsetToCodePage(FCharset);

    if (Connected and Assigned(Lib.mysql_options) and Assigned(SyncThread)) then
      Lib.mysql_options(Handle, MYSQL_SET_CHARSET_NAME, my_char(RawByteString(FCharset)));
  end;
end;

procedure TMySQLConnection.SetConnected(Value: Boolean);
begin
  if ((csReading in ComponentState) and Value) then
    inherited
  else if (Value and not GetConnected()) then
  begin
    if (Assigned(BeforeConnect)) then BeforeConnect(Self);
    if (not Assigned(FLib)) then
      FLib := LoadMySQLLibrary(FLibraryType, LibraryName);
    DoConnect();
    // Maybe we're using Asynchron. So the Events should be called after
    // thread execution in SyncConnected.
  end
  else if (not Value and GetConnected()) then
  begin
    if Assigned(BeforeDisconnect) then BeforeDisconnect(Self);
    DoDisconnect();
    // Maybe we're using Asynchron. So the Events should be called after
    // thread execution in SyncDisconncted.
  end;
end;

procedure TMySQLConnection.SetDatabaseName(const ADatabaseName: string);
begin
  Assert(not Connected);

  FDatabaseName := ADatabaseName;
end;

procedure TMySQLConnection.SetHost(const AHost: string);
begin
  Assert(not Connected);

  FHost := AHost;
end;

procedure TMySQLConnection.SetLibraryName(const ALibraryName: string);
begin
  Assert(not Connected);

  FLibraryName := ALibraryName;
end;

procedure TMySQLConnection.SetLibraryType(const ALibraryType: TMySQLLibrary.TLibraryType);
begin
  Assert(not Connected);

  FLibraryType := ALibraryType;
end;

procedure TMySQLConnection.SetPassword(const APassword: string);
begin
  Assert(not Connected);

  FPassword := APassword;
end;

procedure TMySQLConnection.SetPort(const APort: Word);
begin
  Assert(not Connected);

  FPort := APort;
end;

procedure TMySQLConnection.SetUsername(const AUsername: string);
begin
  Assert(not Connected);

  FUserName := AUserName;
end;

function TMySQLConnection.SQLUse(const DatabaseName: string): string;
begin
  Result := 'USE ' + EscapeIdentifier(DatabaseName) + ';' + #13#10;
end;

procedure TMySQLConnection.SyncAfterExecuteSQL(const SyncThread: TSyncThread);
begin
  FExecutionTime := SyncThread.ExecutionTime;

  if (FErrorCode = 0) then
    SyncThread.SQL := '';

  DoAfterExecuteSQL();

  if (Assigned(SyncThread.Done)) then
    SyncThread.Done.SetEvent();
end;

procedure TMySQLConnection.SyncBindDataSet(const DataSet: TMySQLQuery);
begin
  Assert(Assigned(SyncThread));
  Assert(SyncThread.State = ssResult);
  Assert(not Assigned(SyncThread.DataSet));

  if (not Assigned(DataSet)) then
    raise ERangeError.CreateFmt(SPropertyOutOfRange, ['DataSet']);

  DataSet.SyncThread := SyncThread;
  SyncThread.DataSet := DataSet;

  SyncThread.State := ssReceivingResult;

  if (DataSet is TMySQLDataSet) then
    SyncThread.RunExecute.SetEvent();
end;

procedure TMySQLConnection.SyncConnecting(const SyncThread: TSyncThread);
var
  ClientFlag: my_uint;
begin
  if (not Assigned(Lib)) then
  begin
    SyncThread.ErrorCode := ER_CANT_OPEN_LIBRARY;
    SyncThread.ErrorMessage := Format(SLibraryNotAvailable, [LibraryName]);
  end
  else
  begin
    if (not Assigned(SyncThread.LibHandle)) then
    begin
      if (Assigned(Lib.my_init)) then
        Lib.my_init();
      SyncThread.LibHandle := Lib.mysql_init(nil);
      if (Assigned(Lib.mysql_set_local_infile_handler)) then
        Lib.mysql_set_local_infile_handler(SyncThread.LibHandle, @MySQLDB.local_infile_init, @MySQLDB.local_infile_read, @MySQLDB.local_infile_end, @MySQLDB.local_infile_error, Self);
    end;

    ClientFlag := CLIENT_INTERACTIVE or CLIENT_LOCAL_FILES or CLIENT_CAN_HANDLE_EXPIRED_PASSWORDS;
    if (DatabaseName <> '') then
      ClientFlag := ClientFlag or CLIENT_CONNECT_WITH_DB;
    if (Assigned(Lib.mysql_more_results) and Assigned(Lib.mysql_next_result)) then
      ClientFlag := ClientFlag or CLIENT_MULTI_STATEMENTS or CLIENT_MULTI_RESULTS;
    if (UseCompression()) then
      ClientFlag := ClientFlag or CLIENT_COMPRESS;

    Lib.mysql_options(SyncThread.LibHandle, MYSQL_OPT_READ_TIMEOUT, my_char(RawByteString(IntToStr(NET_WAIT_TIMEOUT))));
    Lib.mysql_options(SyncThread.LibHandle, MYSQL_OPT_WRITE_TIMEOUT, my_char(RawByteString(IntToStr(NET_WAIT_TIMEOUT))));
    Lib.mysql_options(SyncThread.LibHandle, MYSQL_SET_CHARSET_NAME, my_char(RawByteString(FCharset)));
    if (UseCompression()) then
      Lib.mysql_options(SyncThread.LibHandle, MYSQL_OPT_COMPRESS, nil);
    if (LibraryType = ltHTTP) then
    begin
      Lib.mysql_options(SyncThread.LibHandle, enum_mysql_option(MYSQL_OPT_HTTPTUNNEL_URL), my_char(LibEncode(LibraryName)));
      if (HTTPAgent <> '') then
        Lib.mysql_options(SyncThread.LibHandle, enum_mysql_option(MYSQL_OPT_HTTPTUNNEL_AGENT), my_char(LibEncode(HTTPAgent)));
    end;

    if (Host <> LOCAL_HOST_NAMEDPIPE) then
      Lib.mysql_real_connect(SyncThread.LibHandle,
        my_char(LibEncode(Host)),
        my_char(LibEncode(Username)), my_char(LibEncode(Password)),
        my_char(LibEncode(DatabaseName)), Port, '', ClientFlag)
    else
    begin
      Lib.mysql_options(SyncThread.LibHandle, enum_mysql_option(MYSQL_OPT_NAMED_PIPE), nil);
      Lib.mysql_real_connect(SyncThread.LibHandle,
        my_char(LOCAL_HOST),
        my_char(LibEncode(Username)), my_char(LibEncode(Password)),
        my_char(LibEncode(DatabaseName)), Port, MYSQL_NAMEDPIPE, ClientFlag);
    end;
    if ((Lib.mysql_errno(SyncThread.LibHandle) <> 0) or (Lib.LibraryType = ltHTTP)) then
      SyncThread.LibThreadId := 0
    else
      SyncThread.LibThreadId := Lib.mysql_thread_id(SyncThread.LibHandle);

    SyncThread.ErrorCode := Lib.mysql_errno(SyncThread.LibHandle);
    SyncThread.ErrorMessage := GetErrorMessage(SyncThread.LibHandle);

    if ((SyncThread.ErrorCode = 0)
      and Assigned(Lib.mysql_set_character_set)
      and (Lib.mysql_get_server_version(SyncThread.LibHandle) >= 50503)
      and (StrIComp(Lib.mysql_character_set_name(SyncThread.LibHandle), 'utf8') = 0)) then
      Lib.mysql_set_character_set(SyncThread.LibHandle, 'utf8mb4');

    if (SyncThread.ErrorCode > 0) then
      SyncDisconnecting(SyncThread);
  end;
end;

procedure TMySQLConnection.SyncConnected(const SyncThread: TSyncThread);
var
  I: Integer;
  S: string;
begin
  FConnected := SyncThread.ErrorCode = 0;
  FErrorCode := SyncThread.ErrorCode;
  FErrorMessage := SyncThread.ErrorMessage;
  FThreadId := SyncThread.LibThreadId;

  if (FErrorCode > 0) then
    DoError(FErrorCode, FErrorMessage)
  else
  begin
    FLatestConnect := Now();

    if ((ServerVersionStr = '') and (Lib.mysql_get_server_info(SyncThread.LibHandle) <> '')) then
    begin
      FServerVersionStr := string(Lib.mysql_get_server_info(SyncThread.LibHandle));
      if (Assigned(Lib.mysql_get_server_version)) then
        FMySQLVersion := Lib.mysql_get_server_version(SyncThread.LibHandle)
      else
      begin
        S := FServerVersionStr;
        if (Pos('-', S) > 0) then
          S := LeftStr(S, Pos('-', S) - 1);
        if ((Pos('.', S) = 0) or not TryStrToInt(LeftStr(S, Pos('.', S) - 1), I)) then
          FMySQLVersion := 0
        else
        begin
          FMySQLVersion := I * 10000;
          Delete(S, 1, Pos('.', S));
          if ((Pos('.', S) = 0) or not TryStrToInt(LeftStr(S, Pos('.', S) - 1), I)) then
            FMySQLVersion := 0
          else
          begin
            FMySQLVersion := FMySQLVersion + I * 100;
            Delete(S, 1, Pos('.', S));
            if (not TryStrToInt(S, I)) then
              FMySQLVersion := 0
            else
            begin
              FMySQLVersion := FMySQLVersion + I;
              Delete(S, 1, Pos('.', S));
            end;
          end;
        end;
      end;

      S := FServerVersionStr;
      if (Pos('-MariaDB', S) > 0) then
      begin
        S := LeftStr(S, Pos('-MariaDB', S) - 1);
        if (Pos('-', S) > 0) then
          Delete(S, 1, Pos('-', S));
        if ((Pos('.', S) = 0) or not TryStrToInt(LeftStr(S, Pos('.', S) - 1), I)) then
          FMariaDBVersion := 0
        else
        begin
          FMariaDBVersion := I * 10000;
          Delete(S, 1, Pos('.', S));
          if ((Pos('.', S) = 0) or not TryStrToInt(LeftStr(S, Pos('.', S) - 1), I)) then
            FMariaDBVersion := 0
          else
          begin
            FMariaDBVersion := FMariaDBVersion + I * 100;
            Delete(S, 1, Pos('.', S));
            TryStrToInt(S, I);
            FMariaDBVersion := FMariaDBVersion + I;
            Delete(S, 1, Pos('.', S));
          end;
        end;
      end;
    end;

    if ((0 < MySQLVersion) and (MySQLVersion < 32320)) then
      DoError(DS_SERVER_OLD, StrPas(DATASET_ERRORS[DS_SERVER_OLD - DS_MIN_ERROR]))
    else
    begin
      if ((MySQLVersion < 40101) or not Assigned(Lib.mysql_character_set_name)) then
      begin
        FCharset := 'latin1';
        FCharsetNr := 0;
      end
      else
      begin
        FCharset := string(Lib.mysql_character_set_name(SyncThread.LibHandle));
        FCharsetNr := CharsetToCharsetNr(FCharset);
      end;
      FCodePage := CharsetToCodePage(FCharset);

      FHostInfo := LibDecode(Lib.mysql_get_host_info(SyncThread.LibHandle));
      FMultiStatements := FMultiStatements and Assigned(Lib.mysql_more_results) and Assigned(Lib.mysql_next_result) and ((MySQLVersion > 40100) or (Lib.FLibraryType = ltHTTP)) and not ((50000 <= MySQLVersion) and (MySQLVersion < 50007));
    end;
  end;

  if (not Connected) then
    SyncThread.State := ssClose
  else
    SyncThread.State := ssReady;

  if (Connected) then
    SendConnectEvent(True);
  if (Assigned(AfterConnect)) then
    AfterConnect(Self);
end;

procedure TMySQLConnection.SyncDisconnecting(const SyncThread: TSyncThread);
begin
  Assert(Assigned(SyncThread.LibHandle));

  if (Assigned(SyncThread.LibHandle)) then
  begin
    Lib.mysql_close(SyncThread.LibHandle);
    SyncThread.LibHandle := nil;
  end;
end;

procedure TMySQLConnection.SyncDisconnected(const SyncThread: TSyncThread);
begin
  FThreadId := 0;
  FConnected := False;

  if (Assigned(SyncThread)) then
    SyncThread.State := ssClose;

  if (Assigned(AfterDisconnect)) then AfterDisconnect(Self);
end;

procedure TMySQLConnection.SyncExecute(const SyncThread: TSyncThread);
var
  S: string;
  StmtLength: Integer;
begin
  Assert(SyncThread.State in [ssExecutingFirst, ssExecutingNext]);

  if (SyncThread.State = ssExecutingFirst) then
  begin
    S := '# ' + SysUtils.DateTimeToStr(Now() + TimeDiff, FormatSettings);
    WriteMonitor(PChar(S), Length(S), ttTime);
  end;

  if (SyncThread.StmtIndex < SyncThread.StmtLengths.Count) then
  begin
    StmtLength := Integer(SyncThread.StmtLengths[SyncThread.StmtIndex]);
    WriteMonitor(@SyncThread.SQL[SyncThread.SQLIndex], StmtLength, ttRequest);
  end;
end;

procedure TMySQLConnection.SyncExecuted(const SyncThread: TSyncThread);
var
  CLStmt: TSQLCLStmt;
  Data: my_char;
  Info: my_char;
  Len: Integer;
  Name: string;
  Size: size_t;
  S: string;
  Value: string;
begin
  Assert(SyncThread.State in [ssExecutingFirst, ssExecutingNext]);

  FErrorCode := SyncThread.ErrorCode;
  FErrorMessage := SyncThread.ErrorMessage;
  FWarningCount := SyncThread.WarningCount;
  FThreadId := SyncThread.LibThreadId;

  if (SyncThread.StmtIndex < SyncThread.StmtLengths.Count) then
    WriteMonitor(@SyncThread.SQL[SyncThread.SQLIndex], Integer(SyncThread.StmtLengths[SyncThread.StmtIndex]), ttResult);

  if (SyncThread.ErrorCode > 0) then
  begin
    S := '--> Error #' + IntToStr(SyncThread.ErrorCode) + ': ' + SyncThread.ErrorMessage;
    WriteMonitor(PChar(S), Length(S), ttInfo);
  end
  else
  begin
    Inc(FExecutedStmts);

    if (SyncThread.WarningCount > 0) then
    begin
      S := '--> ' + IntToStr(SyncThread.WarningCount) + ' Warning(s) available';
      WriteMonitor(PChar(S), Length(S), ttInfo);
    end;

    if (Assigned(Lib.mysql_session_track_get_first) and Assigned(Lib.mysql_session_track_get_next)) then
      if (Lib.mysql_session_track_get_first(SyncThread.LibHandle, SESSION_TRACK_SYSTEM_VARIABLES, Data, Size) = 0) then
        repeat
          Len := AnsiCharToWideChar(CodePage, Data, Size, nil, 0);
          SetLength(Name, Len);
          AnsiCharToWideChar(CodePage, Data, Size, PChar(Name), Len);
          if (Lib.mysql_session_track_get_next(SyncThread.LibHandle, SESSION_TRACK_SYSTEM_VARIABLES, Data, Size) = 0) then
          begin
            Len := AnsiCharToWideChar(CodePage, Data, Size, nil, 0);
            SetLength(Value, Len);
            AnsiCharToWideChar(CodePage, Data, Size, PChar(Value), Len);
            DoVariableChange(Name, Value);
          end;
        until (Lib.mysql_session_track_get_next(SyncThread.LibHandle, SESSION_TRACK_SYSTEM_VARIABLES, Data, Size) <> 0);

    if (SyncThread.CLStmts[SyncThread.StmtIndex]) then
    begin
      if ((SyncThread.StmtIndex < SyncThread.StmtLengths.Count)
        and (SQLParseCLStmt(CLStmt, @SyncThread.SQL[SyncThread.SQLIndex], Integer(SyncThread.StmtLengths[SyncThread.StmtIndex]), MySQLVersion))) then
        if ((CLStmt.CommandType = ctDropDatabase) and (CLStmt.ObjectName = FDatabaseName)) then
        begin
          S := '--> Database unselected';
          WriteMonitor(PChar(S), Length(S), ttInfo);
          DoDatabaseChange('');
        end
        else if ((CLStmt.CommandType = ctUse) and (CLStmt.ObjectName <> FDatabaseName)) then
        begin
          S := '--> Database selected: ' + CLStmt.ObjectName;
          WriteMonitor(PChar(S), Length(S), ttInfo);
          DoDatabaseChange(CLStmt.ObjectName);
        end
        else if (CLStmt.CommandType = ctShutdown) then
        begin
          S := '--> Server is going down';
          WriteMonitor(PChar(S), Length(S), ttInfo);
        end;
    end
    else if (not Assigned(SyncThread.ResHandle)) then
    begin
      if (Lib.mysql_affected_rows(SyncThread.LibHandle) >= 0) then
      begin
        if (FRowsAffected < 0) then FRowsAffected := 0;
        Inc(FRowsAffected, Lib.mysql_affected_rows(SyncThread.LibHandle));
      end;

      if (Assigned(Lib.mysql_info) and Assigned(Lib.mysql_info(SyncThread.LibHandle))) then
      begin
        Info := Lib.mysql_info(SyncThread.LibHandle);
        try
          S := '--> ' + LibDecode(Info);
        except
          S := '--> ' + string(StrPas(Info));
        end;
        WriteMonitor(PChar(S), Length(S), ttInfo);
      end
      else if (Lib.mysql_affected_rows(SyncThread.LibHandle) > 0) then
      begin
        S := '--> ' + IntToStr(Lib.mysql_affected_rows(SyncThread.LibHandle)) + ' Record(s) affected';
        WriteMonitor(PChar(S), Length(S), ttInfo);
      end
      else
      begin
        S := '--> Ok';
        WriteMonitor(PChar(S), Length(S), ttInfo);
      end;
    end;
  end;


  SyncThread.State := ssResult;

  if (not Assigned(SyncThread.OnResult) or (KillThreadId > 0)) then
  begin
    KillThreadId := 0;

    if (SyncThread.ErrorCode > 0) then
    begin
      DoError(SyncThread.ErrorCode, SyncThread.ErrorMessage);
      SyncThread.State := ssReady;
      SyncHandledResult(SyncThread);
    end;
  end
  else
  begin
    InOnResult := True;
    try
      if (not SyncThread.OnResult(SyncThread.ErrorCode, SyncThread.ErrorMessage, SyncThread.WarningCount,
        SyncThread.CommandText, SyncThread, Assigned(SyncThread.ResHandle))
        and (SyncThread.ErrorCode > 0)) then
      begin
        DoError(SyncThread.ErrorCode, SyncThread.ErrorMessage);
        SyncThread.State := ssReady;
        SyncHandledResult(SyncThread);
      end
      else if ((SyncThread.State = ssResult) and Assigned(SyncThread.ResHandle)) then
        raise Exception.Create('Query has not been handled: ' + SyncThread.CommandText);
    finally
      InOnResult := False;
    end;
  end;

  if ((SyncThread.Mode = smSQL) and (SyncThread.State = ssResult)) then
    SyncHandledResult(SyncThread);

  if (SyncThread.State = ssReady) then
    SyncAfterExecuteSQL(SyncThread);
end;

procedure TMySQLConnection.SyncExecutingFirst(const SyncThread: TSyncThread);
var
  AlterTableAfterCreateTable: Boolean;
  CreateTableInPacket: Boolean;
  DDLStmt: TSQLDDLStmt;
  LibLength: Integer;
  LibSQL: RawByteString;
  NeedReconnect: Boolean;
  PacketComplete: (pcNo, pcExclusiveStmt, pcInclusiveStmt);
  PacketLength: Integer;
  ProcedureName: string;
  ResetPassword: Boolean;
  Retry: Integer;
  SQL: PChar;
  SQLIndex: Integer;
  StartTime: TDateTime;
  Stmt: RawByteString;
  StmtIndex: Integer;
  StmtLength: Integer;
  Success: Boolean;
begin
  Assert(SyncThread.State = ssExecutingFirst);

  CreateTableInPacket := False; AlterTableAfterCreateTable := False;
  PacketLength := 0; PacketComplete := pcNo;
  StmtIndex := SyncThread.StmtIndex; SQLIndex := SyncThread.SQLIndex;
  while ((StmtIndex < SyncThread.StmtLengths.Count) and (PacketComplete = pcNo)) do
  begin
    SQL := @SyncThread.SQL[SQLIndex];
    StmtLength := Integer(SyncThread.StmtLengths[StmtIndex]);

    if (MySQLVersion <= 50100) then
      if (not CreateTableInPacket) then
        CreateTableInPacket := SQLParseDDLStmt(DDLStmt, SQL, StmtLength, MySQLVersion) and (DDLStmt.DefinitionType = dtCreate) and (DDLStmt.ObjectType = otTable)
      else
        AlterTableAfterCreateTable := SQLParseDDLStmt(DDLStmt, SQL, StmtLength, MySQLVersion) and (DDLStmt.DefinitionType = dtAlter) and (DDLStmt.ObjectType = otTable);

    if (AlterTableAfterCreateTable) then
      PacketComplete := pcExclusiveStmt
    else if ((StmtLength = 8) and (StrLIComp(SQL, 'SHUTDOWN', 8) = 0) and (MySQLVersion < 50709)) then
      PacketComplete := pcExclusiveStmt
    else if ((SizeOf(COM_QUERY) + SQLIndex - 1 + StmtLength > MaxAllowedServerPacket)
      and (SizeOf(COM_QUERY) + WideCharToAnsiChar(CodePage, PChar(@SyncThread.SQL[SQLIndex]), StmtLength, nil, 0) > MaxAllowedServerPacket)) then
      PacketComplete := pcExclusiveStmt
    else if (not MultiStatements or (SQLIndex - 1 + StmtLength = Length(SyncThread.SQL))) then
      PacketComplete := pcInclusiveStmt
    else if (SQLParseCallStmt(SQL, StmtLength, ProcedureName, MySQLVersion) and (ProcedureName <> '')) then
      PacketComplete := pcInclusiveStmt
    else
      PacketComplete := pcNo;

    if ((PacketLength = 0) or (PacketComplete in [pcNo, pcInclusiveStmt])) then
      Inc(PacketLength, StmtLength);

    Inc(StmtIndex);
    Inc(SQLIndex, StmtLength);
  end;

  LibLength := WideCharToAnsiChar(CodePage, PChar(@SyncThread.SQL[SyncThread.SQLIndex]), PacketLength, nil, 0);
  SetLength(LibSQL, LibLength);
  WideCharToAnsiChar(CodePage, PChar(@SyncThread.SQL[SyncThread.SQLIndex]), PacketLength, PAnsiChar(LibSQL), LibLength);

  if (not MultiStatements) then
    while ((LibLength > 0) and (LibSQL[LibLength] in [#9, #10, #13, ' ', ';'])) do
      Dec(LibLength);

  if (LibLength = 0) then
    raise ERangeError.CreateFmt(SPropertyOutOfRange, ['LibLength']);

  Retry := 0; NeedReconnect := not Assigned(SyncThread.LibHandle);
  repeat
    if (not NeedReconnect) then
      Success := True
    else
    begin
      SyncConnecting(SyncThread);
      Success := Assigned(SyncThread.LibHandle) and (SyncThread.ErrorCode = 0);
      NeedReconnect := not Assigned(SyncThread.LibHandle) or (SyncThread.ErrorCode = CR_SERVER_LOST);
    end;
    ResetPassword := False;

    if (not SyncThread.Terminated and Success) then
    begin
      if ((LibLength = 8) and (StrLIComp(my_char(LibSQL), 'SHUTDOWN', 8) = 0) and (MySQLVersion < 50709) and Assigned(Lib.mysql_shutdown)) then
        Lib.mysql_shutdown(SyncThread.LibHandle, SHUTDOWN_DEFAULT)
      else
      begin
        StartTime := Now();
        Lib.mysql_real_query(SyncThread.LibHandle, my_char(LibSQL), LibLength);
        SyncThread.ExecutionTime := SyncThread.ExecutionTime + Now() - StartTime;
      end;

      if (Lib.mysql_errno(SyncThread.LibHandle) = ER_MUST_CHANGE_PASSWORD) then
      begin
        Stmt := LibEncode('SET PASSWORD=Password(' + SQLEscape(Password) + ')');
        ResetPassword := Lib.mysql_real_query(SyncThread.LibHandle, my_char(Stmt), Length(Stmt)) = 0;
      end
      else
      begin
        NeedReconnect := not Assigned(SyncThread.LibHandle)
          or (Lib.mysql_errno(SyncThread.LibHandle) = CR_SERVER_GONE_ERROR)
          or (Lib.mysql_errno(SyncThread.LibHandle) = CR_SERVER_HANDSHAKE_ERR)
          or (Lib.mysql_errno(SyncThread.LibHandle) = CR_SERVER_LOST)
          or (Lib.mysql_errno(SyncThread.LibHandle) = CR_COMMANDS_OUT_OF_SYNC);
      end;
    end;

    Inc(Retry);
  until ((not ResetPassword and not NeedReconnect) or (Retry > RETRY_COUNT));

  if (Assigned(SyncThread.LibHandle)) then
  begin
    if ((Lib.mysql_errno(SyncThread.LibHandle) = 0) and not SyncThread.Terminated) then
      SyncThread.ResHandle := Lib.mysql_use_result(SyncThread.LibHandle);

    SyncThread.ErrorCode := Lib.mysql_errno(SyncThread.LibHandle);
    SyncThread.ErrorMessage := GetErrorMessage(SyncThread.LibHandle);
    if ((MySQLVersion < 40100) or not Assigned(Lib.mysql_warning_count)) then
      SyncThread.WarningCount := 0
    else
      SyncThread.WarningCount := Lib.mysql_warning_count(SyncThread.LibHandle);
  end;
end;

procedure TMySQLConnection.SyncExecutingNext(const SyncThread: TSyncThread);
var
  StartTime: TDateTime;
begin
  Assert(SyncThread.State = ssExecutingNext);

  StartTime := Now();
  Lib.mysql_next_result(SyncThread.LibHandle);
  SyncThread.ExecutionTime := SyncThread.ExecutionTime + Now() - StartTime;

  if (SyncThread.ErrorCode = 0) then
    SyncThread.ResHandle := Lib.mysql_use_result(SyncThread.LibHandle);

  SyncThread.ErrorCode := Lib.mysql_errno(SyncThread.LibHandle);
  SyncThread.ErrorMessage := GetErrorMessage(SyncThread.LibHandle);
  if ((MySQLVersion < 40100) or not Assigned(Lib.mysql_warning_count)) then
    SyncThread.WarningCount := 0
  else
    SyncThread.WarningCount := Lib.mysql_warning_count(SyncThread.LibHandle);
end;

procedure TMySQLConnection.SyncHandledResult(const SyncThread: TSyncThread);
var
  S: String;
begin
  Assert((SyncThread.State in [ssReceivingResult, ssReady]) or (SyncThread.State = ssResult) and not Assigned(SyncThread.ResHandle));

  if (SyncThread.State = ssReceivingResult) then
  begin
    S := '--> ' + IntToStr(Lib.mysql_num_rows(SyncThread.ResHandle)) + ' Record(s) received';
    WriteMonitor(PChar(S), Length(S), ttInfo);

    Lib.mysql_free_result(SyncThread.ResHandle);
    SyncThread.ResHandle := nil;

    if (SyncThread.ErrorCode > 0) then
    begin
      S := '--> Error #' + IntToStr(FErrorCode) + ': ' + FErrorMessage + ' while receiving Record(s)';
      WriteMonitor(PChar(S), Length(S), ttInfo);
    end;
  end;


  if (SyncThread.StmtIndex < SyncThread.StmtLengths.Count) then
  begin
    Inc(SyncThread.SQLIndex, Integer(SyncThread.StmtLengths[SyncThread.StmtIndex]));
    Inc(SyncThread.StmtIndex);
    if (SyncThread.ErrorCode = 0) then
      FSuccessfullExecutedSQLLength := SyncThread.SQLIndex;
  end;


  if (SyncThread.State = ssReady) then
    // An error occurred and it was NOT handled in OnResult
  else if (SyncThread.ErrorCode = CR_SERVER_GONE_ERROR) then
    SyncThread.State := ssReady
  else if (MultiStatements and Assigned(SyncThread.LibHandle) and (Lib.mysql_more_results(SyncThread.LibHandle) = 1)) then
    SyncThread.State := ssExecutingNext
  else if (SyncThread.StmtIndex < SyncThread.StmtLengths.Count) then
    SyncThread.State := ssExecutingFirst
  else
    SyncThread.State := ssReady;
end;

procedure TMySQLConnection.SyncPing(const SyncThread: TSyncThread);
begin
  if ((Lib.LibraryType <> ltHTTP) and Assigned(SyncThread.LibHandle)) then
    Lib.mysql_ping(SyncThread.LibHandle);
end;

procedure TMySQLConnection.SyncReceivingResult(const SyncThread: TSyncThread);
var
  DataSet: TMySQLDataSet;
  LibRow: MYSQL_ROW;
begin
  Assert(SyncThread.State = ssReceivingResult);
  Assert(SyncThread.DataSet is TMySQLDataSet);
  DataSet := TMySQLDataSet(SyncThread.DataSet);

  // Debug 2016-11-18
  if (not Assigned(SyncThread.ResHandle)) then
    raise ERangeError.Create(SRangeError);

  repeat
    if (SyncThread.Terminated) then
      LibRow := nil
    else
    begin
      LibRow := Lib.mysql_fetch_row(SyncThread.ResHandle);
      if (Lib.mysql_errno(SyncThread.LibHandle) <> 0) then
      begin
        SyncThread.ErrorCode := Lib.mysql_errno(SyncThread.LibHandle);
        SyncThread.ErrorMessage := GetErrorMessage(SyncThread.LibHandle);
      end
      else if (not DataSet.InternAddRecord(LibRow, Lib.mysql_fetch_lengths(SyncThread.ResHandle))) then
      begin
        SyncThread.ErrorCode := DS_OUT_OF_MEMORY;
        SyncThread.ErrorMessage := StrPas(DATASET_ERRORS[DS_OUT_OF_MEMORY - DS_MIN_ERROR]);
      end;
    end;
  until (not Assigned(LibRow) or (SyncThread.ErrorCode <> 0));

  DataSet.InternAddRecord(nil, nil);
end;

procedure TMySQLConnection.Terminate();
var
  S: string;
begin
  TerminateCS.Enter();

  if (Assigned(SyncThread) and SyncThread.IsRunning) then
  begin
    KillThreadId := SyncThread.ThreadId;

    SyncThread.Terminate();

    {$IFDEF Debug}
      MessageBox(0, 'Terminate!', 'Warning', MB_OK + MB_ICONWARNING);
    {$ENDIF}

    S := '--> Connection terminated';
    WriteMonitor(PChar(S), Length(S), ttInfo);
  end;

  TerminateCS.Leave();
end;

procedure TMySQLConnection.SyncReleaseDataSet(const DataSet: TMySQLQuery);
begin
  Assert(Assigned(DataSet));

  if (DataSet.SyncThread = SyncThread) then
  begin
    SyncThread.DataSet := nil;

    if (not SyncThread.Terminated and (SyncThread.State = ssReceivingResult)) then
      SyncHandledResult(SyncThread);
  end;

  DataSet.SyncThread := nil;
end;

procedure TMySQLConnection.UnRegisterSQLMonitor(const AMySQLMonitor: TMySQLMonitor);
var
  I: Integer;
  Index: Integer;
begin
  Index := -1;
  for I := 0 to Length(FSQLMonitors) - 1 do
    if (AMySQLMonitor = FSQLMonitors[I]) then
      Index := I;

  if (Index >= 0) then
  begin
    for I := Length(FSQLMonitors) - 2 downto Index do
      FSQLMonitors[I] := FSQLMonitors[I + 1];

    SetLength(FSQLMonitors, Length(FSQLMonitors) - 1);
  end;
end;

function TMySQLConnection.UseCompression(): Boolean;
begin
  Result := (Host <> LOCAL_HOST_NAMEDPIPE) or (LibraryType = ltHTTP);
end;

function TMySQLConnection.UseSyncThread(): Boolean;
begin
  Result := Asynchron and Assigned(MySQLConnectionOnSynchronize) and (SynchronCount = 0);
end;

procedure TMySQLConnection.WriteMonitor(const AText: PChar; const Length: Integer; const ATraceType: TMySQLMonitor.TTraceType);
var
  I: Integer;
begin
  InMonitor := True;
  try
    for I := 0 to System.Length(FSQLMonitors) - 1 do
      if (ATraceType in FSQLMonitors[I].TraceTypes) then
        FSQLMonitors[I].DoMonitor(Self, AText, Length, ATraceType);
  finally
    InMonitor := False;
  end;
end;

{ TMySQLBitField **************************************************************}

function TMySQLBitField.GetAsString(): string;
begin
  GetText(Result, False);
end;

function TMySQLBitField.GetIsNull(): Boolean;
begin
  if (not Assigned(DataSet)) then
    // Debug 2016-11-10
    raise ERangeError.CreateFMT(SPropertyOutOfRange, ['DataSet'])
  else if (not (DataSet is TMySQLQuery)) then
    // Debug 2016-11-10
    raise ERangeError.CreateFMT(SPropertyOutOfRange, ['DataSet'])
  else if (FieldNo - 1 >= TMySQLQuery(DataSet).FieldCount) then
    // Debug 2016-11-21
    raise ERangeError.Create(SRangeError);

  Result := not Assigned(TMySQLQuery(DataSet).LibRow) or not Assigned(TMySQLQuery(DataSet).LibRow^[FieldNo - 1]);
end;

procedure TMySQLBitField.GetText(var Text: string; DisplayText: Boolean);
var
  FmtStr: string;
  L: Largeint;
begin
  if (not GetValue(L)) then
    Text := ''
  else
  begin
    if (DisplayText or (EditFormat = '')) then
      FmtStr := DisplayFormat
    else
      FmtStr := EditFormat;
    while ((Length(FmtStr) > 0) and (FmtStr[1] = '#')) do Delete(FmtStr, 1, 1);
    Text := IntToBitString(L, Length(FmtStr));
  end;
end;

procedure TMySQLBitField.SetAsString(const Value: string);
var
  Error: Boolean;
  L: LargeInt;
begin
  L := BitStringToInt(PChar(Value), Length(Value), @Error);
  if (Error) then
    raise EConvertError.CreateFmt(SInvalidBinary, [Value])
  else
    SetAsLargeInt(L);
end;

{ TMySQLBlobField *************************************************************}

function TMySQLBlobField.GetAsAnsiString(): AnsiString;
begin
  SetLength(Result, TMySQLDataSet(DataSet).LibLengths^[FieldNo - 1]);
  MoveMemory(@Result[1], TMySQLDataSet(DataSet).LibRow^[FieldNo - 1], TMySQLDataSet(DataSet).LibLengths^[FieldNo - 1]);
end;

function TMySQLBlobField.GetAsString(): string;
begin
  GetText(Result, False);
end;

function TMySQLBlobField.GetAsVariant(): Variant;
begin
  if (IsNull) then
    Result := Null
  else
    Result := GetAsString();
end;

function TMySQLBlobField.GetIsNull(): Boolean;
begin
  if (not Assigned(DataSet)) then
    // Debug 2016-11-10
    raise ERangeError.CreateFMT(SPropertyOutOfRange, ['DataSet'])
  else if (not (DataSet is TMySQLQuery)) then
    // Debug 2016-11-10
    raise ERangeError.CreateFMT(SPropertyOutOfRange, ['DataSet'])
  else if (FieldNo - 1 >= TMySQLQuery(DataSet).FieldCount) then
    // Debug 2016-11-21
    raise ERangeError.Create(SRangeError);

  Result := not Assigned(TMySQLQuery(DataSet).LibRow) or not Assigned(TMySQLQuery(DataSet).LibRow^[FieldNo - 1]);
end;

procedure TMySQLBlobField.GetText(var Text: string; DisplayText: Boolean);
begin
  if (IsNull) then
    Text := ''
  else
    Text := string(GetAsAnsiString);
end;

procedure TMySQLBlobField.SetAsString(const Value: string);
begin
  if (Length(Value) > 0) then
    inherited SetAsString(Value)
  else
    with DataSet.CreateBlobStream(Self, bmWrite) do
      try
        Write(Value, 0);
      finally
        Free();
      end;
end;

{ TMySQLByteField *************************************************************}

function TMySQLByteField.GetAsString(): string;
begin
  GetText(Result, False);
end;

function TMySQLByteField.GetIsNull(): Boolean;
begin
  if (not Assigned(DataSet)) then
    // Debug 2016-11-10
    raise ERangeError.CreateFMT(SPropertyOutOfRange, ['DataSet'])
  else if (not (DataSet is TMySQLQuery)) then
    // Debug 2016-11-10
    raise ERangeError.CreateFMT(SPropertyOutOfRange, ['DataSet'])
  else if (FieldNo - 1 >= TMySQLQuery(DataSet).FieldCount) then
    // Debug 2016-11-21
    raise ERangeError.Create(SRangeError);

  Result := not Assigned(TMySQLQuery(DataSet).LibRow) or not Assigned(TMySQLQuery(DataSet).LibRow^[FieldNo - 1]);
end;

procedure TMySQLByteField.GetText(var Text: string; DisplayText: Boolean);
begin
  if (IsNull) then
    Text := ''
  else
    Text := TMySQLQuery(DataSet).Connection.LibUnpack(TMySQLQuery(DataSet).LibRow^[FieldNo - 1], TMySQLQuery(DataSet).LibLengths^[FieldNo - 1]);
end;

{ TMySQLDateField *************************************************************}

function TMySQLDateField.GetAsString(): string;
begin
  GetText(Result, False);
end;

function TMySQLDateField.GetIsNull(): Boolean;
begin
  if (not Assigned(DataSet)) then
    // Debug 2016-11-10
    raise ERangeError.CreateFMT(SPropertyOutOfRange, ['DataSet'])
  else if (not (DataSet is TMySQLQuery)) then
    // Debug 2016-11-10
    raise ERangeError.CreateFMT(SPropertyOutOfRange, ['DataSet'])
  else if (FieldNo - 1 >= TMySQLQuery(DataSet).FieldCount) then
    // Debug 2016-11-21
    raise ERangeError.Create(SRangeError);

  Result := not Assigned(TMySQLQuery(DataSet).LibRow) or not Assigned(TMySQLQuery(DataSet).LibRow^[FieldNo - 1]);
end;

procedure TMySQLDateField.GetText(var Text: string; DisplayText: Boolean);
begin
  if (IsNull) then
    Text := ''
  else
    Text := TMySQLQuery(DataSet).Connection.LibUnpack(TMySQLQuery(DataSet).LibRow^[FieldNo - 1], TMySQLQuery(DataSet).LibLengths^[FieldNo - 1]);
end;

procedure TMySQLDateField.SetAsString(const Value: string);
begin
  try
    AsDateTime := MySQLDB.StrToDate(Value, TMySQLQuery(DataSet).Connection.FormatSettings);
  except
    on E: EConvertError do
      TMySQLQuery(DataSet).Connection.DoConvertError(Self, Value, E);
  end;
end;

procedure TMySQLDateField.SetDataSet(ADataSet: TDataSet);
begin
  inherited;

  ZeroDateString := GetZeroDateString(TMySQLQuery(DataSet).Connection.FormatSettings);

  if (DataSet is TMySQLQuery) then
    ValidChars := ['0'..'9', TMySQLConnection(TMySQLQuery(DataSet).Connection).FormatSettings.DateSeparator]
  else
    ValidChars := ['0'..'9', '-'];
end;

{ TMySQLDateTimeField *********************************************************}

function TMySQLDateTimeField.GetAsString(): string;
begin
  GetText(Result, False);
end;

function TMySQLDateTimeField.GetIsNull(): Boolean;
begin
  if (not Assigned(DataSet)) then
    // Debug 2016-11-10
    raise ERangeError.CreateFMT(SPropertyOutOfRange, ['DataSet'])
  else if (not (DataSet is TMySQLQuery)) then
    // Debug 2016-11-10
    raise ERangeError.CreateFMT(SPropertyOutOfRange, ['DataSet'])
  else if (FieldNo - 1 >= TMySQLQuery(DataSet).FieldCount) then
    // Debug 2016-11-21
    raise ERangeError.Create(SRangeError);

  Result := not Assigned(TMySQLQuery(DataSet).LibRow) or not Assigned(TMySQLQuery(DataSet).LibRow^[FieldNo - 1]);
end;

procedure TMySQLDateTimeField.GetText(var Text: string; DisplayText: Boolean);
begin
  if (IsNull) then
    Text := ''
  else
    Text := TMySQLQuery(DataSet).Connection.LibUnpack(TMySQLQuery(DataSet).LibRow^[FieldNo - 1], TMySQLQuery(DataSet).LibLengths^[FieldNo - 1]);
end;

procedure TMySQLDateTimeField.SetAsString(const Value: string);
begin
   if (Value = '') then
    AsDateTime := -1
  else
    AsDateTime := MySQLDB.StrToDateTime(Value, TMySQLQuery(DataSet).Connection.FormatSettings);
end;

procedure TMySQLDateTimeField.SetDataSet(ADataSet: TDataSet);
begin
  inherited;

  if (DataSet is TMySQLQuery) then
    ValidChars := ['0'..'9', ' ', TMySQLConnection(TMySQLQuery(DataSet).Connection).FormatSettings.DateSeparator, TMySQLConnection(TMySQLQuery(DataSet).Connection).FormatSettings.TimeSeparator]
  else
    ValidChars := ['0'..'9', ' ', '-', ':'];

  ZeroDateString := GetZeroDateString(TMySQLQuery(DataSet).Connection.FormatSettings);
end;

{ TMySQLExtendedField *********************************************************}

function TMySQLExtendedField.GetAsString(): string;
begin
  GetText(Result, False);
end;

function TMySQLExtendedField.GetIsNull(): Boolean;
begin
  if (not Assigned(DataSet)) then
    // Debug 2016-11-10
    raise ERangeError.CreateFMT(SPropertyOutOfRange, ['DataSet'])
  else if (not (DataSet is TMySQLQuery)) then
    // Debug 2016-11-10
    raise ERangeError.CreateFMT(SPropertyOutOfRange, ['DataSet'])
  else if (FieldNo - 1 >= TMySQLQuery(DataSet).FieldCount) then
    // Debug 2016-11-21
    raise ERangeError.Create(SRangeError);

  Result := not Assigned(TMySQLQuery(DataSet).LibRow) or not Assigned(TMySQLQuery(DataSet).LibRow^[FieldNo - 1]);
end;

procedure TMySQLExtendedField.GetText(var Text: string; DisplayText: Boolean);
begin
  if (IsNull) then
    Text := ''
  else
    Text := TMySQLQuery(DataSet).Connection.LibUnpack(TMySQLQuery(DataSet).LibRow^[FieldNo - 1], TMySQLQuery(DataSet).LibLengths^[FieldNo - 1]);
end;

{ TMySQLFloatField ************************************************************}

function TMySQLFloatField.GetAsString(): string;
begin
  GetText(Result, False);
end;

function TMySQLFloatField.GetIsNull(): Boolean;
begin
  if (not Assigned(DataSet)) then
    // Debug 2016-11-10
    raise ERangeError.CreateFMT(SPropertyOutOfRange, ['DataSet'])
  else if (not (DataSet is TMySQLQuery)) then
    // Debug 2016-11-10
    raise ERangeError.CreateFMT(SPropertyOutOfRange, ['DataSet'])
  else if (FieldNo - 1 >= TMySQLQuery(DataSet).FieldCount) then
    // Debug 2016-11-21
    raise ERangeError.Create(SRangeError);

  Result := not Assigned(TMySQLQuery(DataSet).LibRow) or not Assigned(TMySQLQuery(DataSet).LibRow^[FieldNo - 1]);
end;

procedure TMySQLFloatField.GetText(var Text: string; DisplayText: Boolean);
begin
  if (IsNull) then
    Text := ''
  else
    Text := TMySQLQuery(DataSet).Connection.LibUnpack(TMySQLQuery(DataSet).LibRow^[FieldNo - 1], TMySQLQuery(DataSet).LibLengths^[FieldNo - 1]);
end;

{ TMySQLIntegerField **********************************************************}

function TMySQLIntegerField.GetAsString(): string;
begin
  GetText(Result, False);
end;

function TMySQLIntegerField.GetIsNull(): Boolean;
begin
  if (not Assigned(DataSet)) then
    // Debug 2016-11-10
    raise ERangeError.CreateFMT(SPropertyOutOfRange, ['DataSet'])
  else if (not (DataSet is TMySQLQuery)) then
    // Debug 2016-11-10
    raise ERangeError.CreateFMT(SPropertyOutOfRange, ['DataSet'])
  else if (FieldNo - 1 >= TMySQLQuery(DataSet).FieldCount) then
    // Debug 2016-11-21
    raise ERangeError.Create(SRangeError);

    Result := not Assigned(TMySQLQuery(DataSet).LibRow) or not Assigned(TMySQLQuery(DataSet).LibRow^[FieldNo - 1]);
end;

procedure TMySQLIntegerField.GetText(var Text: string; DisplayText: Boolean);
begin
  if (IsNull) then
    Text := ''
  else
    Text := TMySQLQuery(DataSet).Connection.LibUnpack(TMySQLQuery(DataSet).LibRow^[FieldNo - 1], TMySQLQuery(DataSet).LibLengths^[FieldNo - 1]);
end;

{ TMySQLLargeWordField ********************************************************}

procedure TMySQLLargeWordField.CheckRange(Value, Min, Max: UInt64);
begin
  if ((Value < Min) or (Value > Max)) then RangeError(Value, Min, Max);
end;

function TMySQLLargeWordField.GetAsLargeInt(): Largeint;
begin
  Result := StrToUInt64(AsString);
end;

function TMySQLLargeWordField.GetAsString(): string;
begin
  GetText(Result, False);
end;

function TMySQLLargeWordField.GetIsNull(): Boolean;
begin
  if (not Assigned(DataSet)) then
    // Debug 2016-11-10
    raise ERangeError.CreateFMT(SPropertyOutOfRange, ['DataSet'])
  else if (not (DataSet is TMySQLQuery)) then
    // Debug 2016-11-10
    raise ERangeError.CreateFMT(SPropertyOutOfRange, ['DataSet'])
  else if (FieldNo - 1 >= TMySQLQuery(DataSet).FieldCount) then
    // Debug 2016-11-21
    raise ERangeError.Create(SRangeError);

  Result := not Assigned(TMySQLQuery(DataSet).LibRow) or not Assigned(TMySQLQuery(DataSet).LibRow^[FieldNo - 1]);
end;

procedure TMySQLLargeWordField.GetText(var Text: string; DisplayText: Boolean);
begin
  if (IsNull) then
    Text := ''
  else
    Text := TMySQLQuery(DataSet).Connection.LibUnpack(TMySQLQuery(DataSet).LibRow^[FieldNo - 1], TMySQLQuery(DataSet).LibLengths^[FieldNo - 1]);
end;

procedure TMySQLLargeWordField.SetAsLargeInt(Value: Largeint);
begin
  if (FMinValue <> 0) or (FMaxValue <> 0) then
    CheckRange(UInt64(Value), UInt64(FMinValue), UInt64(FMaxValue));
  SetData(@Value);
end;

procedure TMySQLLargeWordField.SetAsString(const Value: string);
begin
  if (Value = '') then
    Clear()
  else
    SetAsLargeint(StrToUInt64(Value));
end;

{ TMySQLLongWordField *********************************************************}

function TMySQLLongWordField.GetAsString(): string;
begin
  GetText(Result, False);
end;

function TMySQLLongWordField.GetIsNull(): Boolean;
begin
  if (not Assigned(DataSet)) then
    // Debug 2016-11-10
    raise ERangeError.CreateFMT(SPropertyOutOfRange, ['DataSet'])
  else if (not (DataSet is TMySQLQuery)) then
    // Debug 2016-11-10
    raise ERangeError.CreateFMT(SPropertyOutOfRange, ['DataSet'])
  else if (FieldNo - 1 >= TMySQLQuery(DataSet).FieldCount) then
    // Debug 2016-11-21
    raise ERangeError.Create(SRangeError);

  Result := not Assigned(TMySQLQuery(DataSet).LibRow) or not Assigned(TMySQLQuery(DataSet).LibRow^[FieldNo - 1]);
end;

procedure TMySQLLongWordField.GetText(var Text: string; DisplayText: Boolean);
begin
  if (IsNull) then
    Text := ''
  else
    Text := TMySQLQuery(DataSet).Connection.LibUnpack(TMySQLQuery(DataSet).LibRow^[FieldNo - 1], TMySQLQuery(DataSet).LibLengths^[FieldNo - 1]);
end;

{ TMySQLShortIntField *********************************************************}

function TMySQLShortIntField.GetAsString(): string;
begin
  GetText(Result, False);
end;

function TMySQLShortIntField.GetIsNull(): Boolean;
begin
  if (not Assigned(DataSet)) then
    // Debug 2016-11-10
    raise ERangeError.CreateFMT(SPropertyOutOfRange, ['DataSet'])
  else if (not (DataSet is TMySQLQuery)) then
    // Debug 2016-11-10
    raise ERangeError.CreateFMT(SPropertyOutOfRange, ['DataSet'])
  else if (FieldNo - 1 >= TMySQLQuery(DataSet).FieldCount) then
    // Debug 2016-11-21
    raise ERangeError.Create(SRangeError);

  Result := not Assigned(TMySQLQuery(DataSet).LibRow) or not Assigned(TMySQLQuery(DataSet).LibRow^[FieldNo - 1]);
end;

procedure TMySQLShortIntField.GetText(var Text: string; DisplayText: Boolean);
begin
  if (IsNull) then
    Text := ''
  else
    Text := TMySQLQuery(DataSet).Connection.LibUnpack(TMySQLQuery(DataSet).LibRow^[FieldNo - 1], TMySQLQuery(DataSet).LibLengths^[FieldNo - 1]);
end;

{ TMySQLSingleField ***********************************************************}

function TMySQLSingleField.GetAsString(): string;
begin
  GetText(Result, False);
end;

function TMySQLSingleField.GetIsNull(): Boolean;
begin
  if (not Assigned(DataSet)) then
    // Debug 2016-11-10
    raise ERangeError.CreateFMT(SPropertyOutOfRange, ['DataSet'])
  else if (not (DataSet is TMySQLQuery)) then
    // Debug 2016-11-10
    raise ERangeError.CreateFMT(SPropertyOutOfRange, ['DataSet'])
  else if (FieldNo - 1 >= TMySQLQuery(DataSet).FieldCount) then
    // Debug 2016-11-21
    raise ERangeError.Create(SRangeError);

  Result := not Assigned(TMySQLQuery(DataSet).LibRow) or not Assigned(TMySQLQuery(DataSet).LibRow^[FieldNo - 1]);
end;

procedure TMySQLSingleField.GetText(var Text: string; DisplayText: Boolean);
begin
  if (IsNull) then
    Text := ''
  else
    Text := TMySQLQuery(DataSet).Connection.LibUnpack(TMySQLQuery(DataSet).LibRow^[FieldNo - 1], TMySQLQuery(DataSet).LibLengths^[FieldNo - 1]);
end;

{ TMySQLSmallIntField *********************************************************}

function TMySQLSmallIntField.GetAsString(): string;
begin
  GetText(Result, False);
end;

function TMySQLSmallIntField.GetIsNull(): Boolean;
begin
  if (not Assigned(DataSet)) then
    // Debug 2016-11-10
    raise ERangeError.CreateFMT(SPropertyOutOfRange, ['DataSet'])
  else if (not (DataSet is TMySQLQuery)) then
    // Debug 2016-11-10
    raise ERangeError.CreateFMT(SPropertyOutOfRange, ['DataSet'])
  else if (FieldNo - 1 >= TMySQLQuery(DataSet).FieldCount) then
    // Debug 2016-11-21
    raise ERangeError.Create(SRangeError);

  Result := not Assigned(TMySQLQuery(DataSet).LibRow) or not Assigned(TMySQLQuery(DataSet).LibRow^[FieldNo - 1]);
end;

procedure TMySQLSmallIntField.GetText(var Text: string; DisplayText: Boolean);
begin
  if (IsNull) then
    Text := ''
  else
    Text := TMySQLQuery(DataSet).Connection.LibUnpack(TMySQLQuery(DataSet).LibRow^[FieldNo - 1], TMySQLQuery(DataSet).LibLengths^[FieldNo - 1]);
end;

{ TMySQLStringField ***********************************************************}

function TMySQLStringField.GetAsAnsiString(): AnsiString;
begin
  if (IsNull) then
    Result := ''
  else
    SetString(Result, TMySQLQuery(DataSet).LibRow^[FieldNo - 1], TMySQLQuery(DataSet).LibLengths^[FieldNo - 1]);
end;

function TMySQLStringField.GetAsString(): string;
begin
  if (IsNull) then
    Result := ''
  else
    Result := TMySQLQuery(DataSet).Connection.LibUnpack(TMySQLQuery(DataSet).LibRow^[FieldNo - 1], TMySQLQuery(DataSet).LibLengths^[FieldNo - 1]);
end;

function TMySQLStringField.GetIsNull(): Boolean;
begin
  if (not Assigned(DataSet)) then
    // Debug 2016-11-10
    raise ERangeError.CreateFMT(SPropertyOutOfRange, ['DataSet'])
  else if (not (DataSet is TMySQLQuery)) then
    // Debug 2016-11-10
    raise ERangeError.CreateFMT(SPropertyOutOfRange, ['DataSet'])
  else if (FieldNo - 1 >= TMySQLQuery(DataSet).FieldCount) then
    // Debug 2016-11-21
    raise ERangeError.Create(SRangeError);

  Result := not Assigned(TMySQLQuery(DataSet).LibRow) or not Assigned(TMySQLQuery(DataSet).LibRow^[FieldNo - 1]);
end;

procedure TMySQLStringField.GetText(var Text: string; DisplayText: Boolean);
begin
  Text := string(GetAsAnsiString());
end;

procedure TMySQLStringField.SetAsString(const Value: string);
begin
  if (Value <> AsString) then
  begin
    TMySQLDataSet(DataSet).SetFieldData(Self, PAnsiChar(RawByteString(Value)), Length(Value));
    TMySQLDataSet(DataSet).DataEvent(deFieldChange, Longint(Self));
  end;
end;

{ TMySQLTimeField *************************************************************}

constructor TMySQLTimeField.Create(AOwner: TComponent);
begin
  inherited;

  SetDataType(ftTime);
end;

function TMySQLTimeField.GetAsDateTime(): TDateTime;
var
  Hour: Word;
  Min: Word;
  Sec: Word;
  Time: Integer;
begin
  if (not GetData(@Time)) then
    Result := Null
  else
  begin
    Sec := Time mod 60; Time := Time div 60;
    Min := Time mod 60; Time := Time div 60;
    Hour := Time mod 60;
    Result := EncodeTime(Hour, Min, Sec, 0);
  end;
end;

function TMySQLTimeField.GetAsString(): string;
begin
  GetText(Result, False);
end;

function TMySQLTimeField.GetAsVariant(): Variant;
var
  Time: Integer;
begin
  if (not GetData(@Time)) then
    Result := Null
  else
    Result := TimeToStr(Time, SQLFormat);
end;

function TMySQLTimeField.GetDataSize(): Integer;
begin
  Result := SizeOf(Integer);
end;

function TMySQLTimeField.GetIsNull(): Boolean;
begin
  if (not Assigned(DataSet)) then
    // Debug 2016-11-10
    raise ERangeError.CreateFMT(SPropertyOutOfRange, ['DataSet'])
  else if (not (DataSet is TMySQLQuery)) then
    // Debug 2016-11-10
    raise ERangeError.CreateFMT(SPropertyOutOfRange, ['DataSet'])
  else if (FieldNo - 1 >= TMySQLQuery(DataSet).FieldCount) then
    // Debug 2016-11-21
    raise ERangeError.Create(SRangeError);

  Result := not Assigned(TMySQLQuery(DataSet).LibRow) or not Assigned(TMySQLQuery(DataSet).LibRow^[FieldNo - 1]);
end;

procedure TMySQLTimeField.GetText(var Text: string; DisplayText: Boolean);
begin
  if (IsNull) then
    Text := ''
  else
    Text := TMySQLQuery(DataSet).Connection.LibUnpack(TMySQLQuery(DataSet).LibRow^[FieldNo - 1], TMySQLQuery(DataSet).LibLengths^[FieldNo - 1]);
end;

procedure TMySQLTimeField.SetAsString(const Value: string);
begin
  try
    AsInteger := StrToTime(Value, SQLFormat);
  except
    on E: EConvertError do
      if (DataSet is TMySQLQuery) then
        TMySQLQuery(DataSet).Connection.DoConvertError(Self, Value, E)
      else
        raise EConvertError.CreateFmt(SInvalidTime, [Value]);
  end;
end;

procedure TMySQLTimeField.SetAsVariant(const Value: Variant);
begin
  AsString := Value;
end;

procedure TMySQLTimeField.SetDataSet(ADataSet: TDataSet);
begin
  inherited;

  if (DataSet is TMySQLQuery) then
    ValidChars := ['-', '0'..'9', TMySQLConnection(TMySQLQuery(DataSet).Connection).FormatSettings.TimeSeparator]
  else
    ValidChars := ['-', '0'..'9', ':'];
end;

{ TMySQLTimeStampField ********************************************************}

function TMySQLTimeStampField.GetAsSQLTimeStamp(): TSQLTimeStamp;
begin
  if (not GetData(@Result)) then
    Result := NULLSQLTimeStamp;
end;

function TMySQLTimeStampField.GetAsString(): string;
begin
  GetText(Result, False);
end;

function TMySQLTimeStampField.GetAsVariant: Variant;
var
  SQLTimeStamp: TSQLTimeStamp;
begin
  if (not GetData(@SQLTimeStamp)) then
    Result := ''
  else
    Result := MySQLTimeStampToStr(SQLTimeStamp, SQLFormatToDisplayFormat(SQLFormat));
end;

function TMySQLTimeStampField.GetDataSize: Integer;
begin
  Result := SizeOf(TSQLTimeStamp);
end;

function TMySQLTimeStampField.GetIsNull(): Boolean;
begin
  if (not Assigned(DataSet)) then
    // Debug 2016-11-10
    raise ERangeError.CreateFMT(SPropertyOutOfRange, ['DataSet'])
  else if (not (DataSet is TMySQLQuery)) then
    // Debug 2016-11-10
    raise ERangeError.CreateFMT(SPropertyOutOfRange, ['DataSet'])
  else if (FieldNo - 1 >= TMySQLQuery(DataSet).FieldCount) then
    // Debug 2016-11-21
    raise ERangeError.Create(SRangeError);

  Result := not Assigned(TMySQLQuery(DataSet).LibRow) or not Assigned(TMySQLQuery(DataSet).LibRow^[FieldNo - 1]);
end;

function TMySQLTimeStampField.GetOldValue: Variant;
var
  SQLTimeStamp: TSQLTimeStamp;
begin
  if (not GetData(@SQLTimeStamp)) then
    Result := ''
  else
    Result := MySQLTimeStampToStr(SQLTimeStamp, SQLFormatToDisplayFormat(SQLFormat));
end;

procedure TMySQLTimeStampField.GetText(var Text: string; DisplayText: Boolean);
begin
  if (IsNull) then
    Text := ''
  else
    Text := TMySQLQuery(DataSet).Connection.LibUnpack(TMySQLQuery(DataSet).LibRow^[FieldNo - 1], TMySQLQuery(DataSet).LibLengths^[FieldNo - 1]);
end;

procedure TMySQLTimeStampField.SetAsSQLTimeStamp(const Value: TSQLTimeStamp);
begin
  SetData(@Value);
end;

procedure TMySQLTimeStampField.SetAsString(const Value: string);
var
  SQLTimeStamp: TSQLTimeStamp;
begin
  if (Value = '') then
  begin
    SQLTimeStamp.Year := Word(-32767);
    SQLTimeStamp.Month := 0;
    SQLTimeStamp.Day := 0;
    SQLTimeStamp.Hour := 0;
    SQLTimeStamp.Minute := 0;
    SQLTimeStamp.Second := 0;
    SQLTimeStamp.Fractions := 0;
    AsSQLTimeStamp := SQLTimeStamp;
  end
  else if (DisplayFormat <> '') then
    AsSQLTimeStamp := StrToMySQLTimeStamp(Value, DisplayFormatToSQLFormat(DisplayFormat))
  else
    AsSQLTimeStamp := StrToMySQLTimeStamp(Value, SQLFormat);
end;

procedure TMySQLTimeStampField.SetAsVariant(const Value: Variant);
begin
  SetAsSQLTimeStamp(StrToMySQLTimeStamp(Value, SQLFormat));
end;

procedure TMySQLTimeStampField.SetDataSet(ADataSet: TDataSet);
begin
  inherited;

  if (DataSet is TMySQLQuery) then
    if (TMySQLConnection(TMySQLQuery(DataSet).Connection).MySQLVersion >= 40100) then
      ValidChars := ['0'..'9', TMySQLConnection(TMySQLQuery(DataSet).Connection).FormatSettings.DateSeparator, TMySQLConnection(TMySQLQuery(DataSet).Connection).FormatSettings.TimeSeparator, ' ']
    else
      ValidChars := ['0'..'9']
  else
    ValidChars := ['0'..'9', '-', ':', ' '];
end;

{ TMySQLWideMemoField *************************************************************}

function TMySQLWideMemoField.GetAsString(): string;
begin
  GetText(Result, False);
end;

function TMySQLWideMemoField.GetAsVariant(): Variant;
begin
  if (IsNull) then
    Result := Null
  else
    Result := GetAsString();
end;

function TMySQLWideMemoField.GetIsNull(): Boolean;
begin
  if (not Assigned(DataSet)) then
    // Debug 2016-11-10
    raise ERangeError.CreateFMT(SPropertyOutOfRange, ['DataSet'])
  else if (not (DataSet is TMySQLQuery)) then
    // Debug 2016-11-10
    raise ERangeError.CreateFMT(SPropertyOutOfRange, ['DataSet'])
  else if (FieldNo - 1 >= TMySQLQuery(DataSet).FieldCount) then
    // Debug 2016-11-21
    raise ERangeError.Create(SRangeError);

  Result := not Assigned(TMySQLQuery(DataSet).LibRow) or not Assigned(TMySQLQuery(DataSet).LibRow^[FieldNo - 1]);
end;

procedure TMySQLWideMemoField.GetText(var Text: string; DisplayText: Boolean);
begin
  if (IsNull) then
    Text := ''
  else
    Text := TMySQLQuery(DataSet).Connection.LibDecode(TMySQLQuery(DataSet).LibRow^[FieldNo - 1], TMySQLQuery(DataSet).LibLengths^[FieldNo - 1]);
end;

procedure TMySQLWideMemoField.SetAsString(const Value: string);
begin
  if (Length(Value) > 0) then
    inherited SetAsString(Value)
  else
    with DataSet.CreateBlobStream(Self, bmWrite) do
      try
        Write(Value, 0);
      finally
        Free();
      end;
end;

{ TMySQLWideStringField *******************************************************}

function TMySQLWideStringField.GetAsDateTime(): TDateTime;
begin
  Result := MySQLDB.StrToDateTime(GetAsString(), TMySQLQuery(DataSet).Connection.FormatSettings);
end;

function TMySQLWideStringField.GetAsString(): string;
begin
  GetText(Result, False);
end;

function TMySQLWideStringField.GetIsNull(): Boolean;
begin
  if (not Assigned(DataSet)) then
    // Debug 2016-11-10
    raise ERangeError.CreateFMT(SPropertyOutOfRange, ['DataSet'])
  else if (not (DataSet is TMySQLQuery)) then
    // Debug 2016-11-10
    raise ERangeError.CreateFMT(SPropertyOutOfRange, ['DataSet'])
  else if (FieldNo - 1 >= TMySQLQuery(DataSet).FieldCount) then
    // Debug 2016-11-21
    raise ERangeError.Create(SRangeError);

  Result := not Assigned(TMySQLQuery(DataSet).LibRow) or not Assigned(TMySQLQuery(DataSet).LibRow^[FieldNo - 1]);
end;

procedure TMySQLWideStringField.GetText(var Text: string; DisplayText: Boolean);
var
  Data: string;
  I: Integer;
  LibLengths: MYSQL_LENGTHS;
  LibRow: MYSQL_ROW;
  Msg: string;
  SQL: string;
begin
  if (IsNull) then
    Text := ''
  else
  begin
    LibRow := TMySQLQuery(DataSet).LibRow;
    LibLengths := TMySQLQuery(DataSet).LibLengths;

    try
      Text := TMySQLQuery(DataSet).Connection.LibDecode(LibRow^[FieldNo - 1], LibLengths^[FieldNo - 1]);
    except
      SetString(Data, LibRow^[FieldNo - 1], LibLengths^[FieldNo - 1]);
      if (DataSet is TMySQLTable) then
        SQL := TMySQLTable(DataSet).SQLSelect()
      else
        SQL := TMySQLQuery(DataSet).CommandText;
      Msg := 'Error while decoding data from the database server.' + #10#10
        + 'SQL query: ' + SQL + #10
        + 'Field: ' + FieldName + #10
        + 'Raw data: ' + Data + #10
        + 'Hex data: ' + SQLEscapeBin(LibRow^[FieldNo - 1], LibLengths^[FieldNo - 1], True) + #10
        + 'Character set: ' + TMySQLQuery(DataSet).Connection.Charset + #10
        + 'Windows codepage: '  + IntToStr(TMySQLQuery(DataSet).Connection.CodePage) + #10
        + 'Connection type: ' + IntToStr(Ord(TMySQLQuery(DataSet).Connection.LibraryType));

      for I := 0 to DataSet.FieldCount - 1 do
        if (DataSet.Fields[I].IsIndexField) then
          try
            Msg := Msg
              + DataSet.Fields[I].FieldName + ': ' + DataSet.Fields[I].AsString + #10;
          except
          end;

      raise ERangeError.Create(Msg);
    end;
  end;
end;

procedure TMySQLWideStringField.SetAsDateTime(Value: TDateTime);
begin
  SetAsString(MySQLDB.DateTimeToStr(Value, TMySQLQuery(DataSet).Connection.FormatSettings));
end;

{ TMySQLWordField *************************************************************}

function TMySQLWordField.GetAsString(): string;
begin
  GetText(Result, False);
end;

function TMySQLWordField.GetIsNull(): Boolean;
begin
  if (not Assigned(DataSet)) then
    // Debug 2016-11-10
    raise ERangeError.CreateFMT(SPropertyOutOfRange, ['DataSet'])
  else if (not (DataSet is TMySQLQuery)) then
    // Debug 2016-11-10
    raise ERangeError.CreateFMT(SPropertyOutOfRange, ['DataSet'])
  else if (FieldNo - 1 >= TMySQLQuery(DataSet).FieldCount) then
    // Debug 2016-11-21
    raise ERangeError.Create(SRangeError);

  Result := not Assigned(TMySQLQuery(DataSet).LibRow) or not Assigned(TMySQLQuery(DataSet).LibRow^[FieldNo - 1]);
end;

procedure TMySQLWordField.GetText(var Text: string; DisplayText: Boolean);
begin
  if (IsNull) then
    Text := ''
  else
    Text := TMySQLQuery(DataSet).Connection.LibUnpack(TMySQLQuery(DataSet).LibRow^[FieldNo - 1], TMySQLQuery(DataSet).LibLengths^[FieldNo - 1]);
end;

{ TMySQLQueryBlobStream *******************************************************}

constructor TMySQLQueryBlobStream.Create(const AField: TBlobField);
begin
  Assert(AField.DataSet is TMySQLQuery);


  inherited Create();

  case (AField.DataType) of
    ftBlob:
      begin
        SetSize(TMySQLQuery.PRecordBufferData(AField.DataSet.ActiveBuffer())^.LibLengths^[AField.FieldNo - 1]);
        Move(TMySQLQuery.PRecordBufferData(AField.DataSet.ActiveBuffer())^.LibRow^[AField.FieldNo - 1]^, Memory^, TMySQLQuery.PRecordBufferData(AField.DataSet.ActiveBuffer())^.LibLengths^[AField.FieldNo - 1]);
      end;
    ftWideMemo: TMySQLQuery(AField.DataSet).DataConvert(AField, AField.DataSet.ActiveBuffer(), Self, False);
  end;
end;

function TMySQLQueryBlobStream.Write(const Buffer; Len: Integer): Integer;
begin
  Result := inherited Write(Buffer, Len);
end;

procedure TMySQLQueryBlobStream.WriteBuffer(const Buffer; Count: Longint);
begin
  inherited WriteBuffer(Buffer, Count);
end;

{ TMySQLQuery *****************************************************************}

function TMySQLQuery.AllocRecordBuffer(): TRecordBuffer;
begin
  try
    New(PRecordBufferData(Result));
  except
    Result := nil;
  end;

  if (Assigned(Result)) then
  begin
    PRecordBufferData(Result)^.LibLengths := nil;
    PRecordBufferData(Result)^.LibRow := nil;

    InitRecord(Result);
  end;
end;

constructor TMySQLQuery.Create(AOwner: TComponent);
begin
  inherited;

  Name := 'TMySQLQuery' + IntToStr(DataSetNumber);
  Inc(DataSetNumber);

  FCommandText := '';
  FCommandType := ctQuery;
  FConnection := nil;
  FDatabaseName := '';
  SyncThread := nil;

  FIndexDefs := TIndexDefs.Create(Self);
  FRecNo := -1;

  SetUniDirectional(True);
end;

function TMySQLQuery.CreateBlobStream(Field: TField; Mode: TBlobStreamMode): TStream;
begin
  case (Field.DataType) of
    ftBlob: Result := TMySQLQueryBlobStream.Create(TMySQLBlobField(Field));
    ftWideMemo: Result := TMySQLQueryMemoStream.Create(TMySQLWideMemoField(Field));
    else Result := inherited CreateBlobStream(Field, Mode);
  end;
end;

procedure TMySQLQuery.DataConvert(Field: TField; Source, Dest: Pointer; ToNative: Boolean);
var
  Len: Integer;
  S: string;
begin
  case (Field.DataType) of
    ftWideMemo:
      if (ToNative) then
      begin
        Len := WideCharToAnsiChar(Connection.CodePage, TMemoryStream(Source).Memory, TMemoryStream(Source).Size div SizeOf(WideChar), nil, 0);
        SetLength(RawByteString(Dest^), Len);
        WideCharToAnsiChar(Connection.CodePage, TMemoryStream(Source).Memory, TMemoryStream(Source).Size div SizeOf(WideChar), PAnsiChar(RawByteString(Dest^)), Len);
      end
      else
      begin
        Len := AnsiCharToWideChar(Connection.CodePage, PRecordBufferData(Source)^.LibRow^[Field.FieldNo - 1], PRecordBufferData(Source)^.LibLengths^[Field.FieldNo - 1], nil, 0);
        TMemoryStream(Dest).SetSize(Len * SizeOf(WideChar));
        AnsiCharToWideChar(Connection.CodePage, PRecordBufferData(Source)^.LibRow^[Field.FieldNo - 1], PRecordBufferData(Source)^.LibLengths^[Field.FieldNo - 1], TMemoryStream(Dest).Memory, Len);
      end;
    ftWideString:
      if (ToNative) then
        WideCharToAnsiChar(Connection.CodePage, PChar(Source), -1, PAnsiChar(Dest), Field.DataSize)
      else
      begin
        try
          Len := AnsiCharToWideChar(Connection.CodePage, PRecordBufferData(Source^)^.LibRow^[Field.FieldNo - 1], PRecordBufferData(Source^)^.LibLengths^[Field.FieldNo - 1], nil, 0);
        except
          on E: Exception do
            begin
              S := SQLEscapeBin(PRecordBufferData(Source^)^.LibRow^[Field.FieldNo - 1], PRecordBufferData(Source^)^.LibLengths^[Field.FieldNo - 1], True);
              raise Exception.CreateFmt(E.Message + '  (DatabaseName: %s, CommandText: %s, DisplayName: %s, LibRow: %s)', [DatabaseName, CommandText, Field.DisplayName, S])
            end;
        end;
        AnsiCharToWideChar(Connection.CodePage, PRecordBufferData(Source^)^.LibRow^[Field.FieldNo - 1], PRecordBufferData(Source^)^.LibLengths^[Field.FieldNo - 1], PChar(Dest), Field.DataSize);
        PChar(Dest)[Len] := #0;
      end;
    else
      inherited;
  end;
end;

destructor TMySQLQuery.Destroy();
begin
  Close();
  Connection := nil; // UnRegister Connection

  FIndexDefs.Free();

  inherited;
end;

function TMySQLQuery.FindRecord(Restart, GoForward: Boolean): Boolean;
begin
  Result := not Restart and GoForward and (MoveBy(1) <> 0);

  SetFound(Result);
end;

procedure TMySQLQuery.FreeRecordBuffer(var Buffer: TRecordBuffer);
begin
  Dispose(Buffer); Buffer := nil;
end;

function TMySQLQuery.GetCanModify(): Boolean;
begin
  Result := False;
end;

function TMySQLQuery.GetFieldData(Field: TField; Buffer: Pointer): Boolean;
var
  Data: PRecordBufferData;
begin
  Data := PRecordBufferData(ActiveBuffer());
  Result := GetFieldData(Field, Buffer, Data);
end;

function TMySQLQuery.GetFieldData(const Field: TField; const Buffer: Pointer; const Data: PRecordBufferData): Boolean;
var
  DT: TDateTime;
  S: string;
begin
  Result := Assigned(Field) and (Field.FieldNo > 0) and Assigned(Data) and Assigned(Data^.LibRow) and Assigned(Data^.LibRow^[Field.FieldNo - 1]);
  if (Result and Assigned(Buffer)) then
    try
      if (BitField(Field)) then
        begin
          ZeroMemory(Buffer, Field.DataSize);
          MoveMemory(@PAnsiChar(Buffer)[Field.DataSize - Data^.LibLengths^[Field.FieldNo - 1]], Data^.LibRow^[Field.FieldNo - 1], Data^.LibLengths^[Field.FieldNo - 1]);
          UInt64(Buffer^) := SwapUInt64(UInt64(Buffer^));
        end
      else
        case (Field.DataType) of
          ftString: begin Move(Data^.LibRow^[Field.FieldNo - 1]^, Buffer^, Data^.LibLengths^[Field.FieldNo - 1]); PAnsiChar(Buffer)[Data^.LibLengths^[Field.FieldNo - 1]] := #0; end;
          ftShortInt: begin SetString(S, Data^.LibRow^[Field.FieldNo - 1], Data^.LibLengths^[Field.FieldNo - 1]); ShortInt(Buffer^) := StrToInt(S); end;
          ftByte: begin SetString(S, Data^.LibRow^[Field.FieldNo - 1], Data^.LibLengths^[Field.FieldNo - 1]); Byte(Buffer^) := StrToInt(S); end;
          ftSmallInt: begin SetString(S, Data^.LibRow^[Field.FieldNo - 1], Data^.LibLengths^[Field.FieldNo - 1]); Smallint(Buffer^) := StrToInt(S); end;
          ftWord: begin SetString(S, Data^.LibRow^[Field.FieldNo - 1], Data^.LibLengths^[Field.FieldNo - 1]); Word(Buffer^) := StrToInt(S); end;
          ftInteger: begin SetString(S, Data^.LibRow^[Field.FieldNo - 1], Data^.LibLengths^[Field.FieldNo - 1]); Longint(Buffer^) := StrToInt(S); end;
          ftLongWord: begin SetString(S, Data^.LibRow^[Field.FieldNo - 1], Data^.LibLengths^[Field.FieldNo - 1]); LongWord(Buffer^) := StrToInt64(S); end;
          ftLargeint:
            if (not (Field is TMySQLLargeWordField)) then
              begin SetString(S, Data^.LibRow^[Field.FieldNo - 1], Data^.LibLengths^[Field.FieldNo - 1]); Largeint(Buffer^) := StrToInt64(S); end
            else
              begin SetString(S, Data^.LibRow^[Field.FieldNo - 1], Data^.LibLengths^[Field.FieldNo - 1]); UInt64(Buffer^) := StrToUInt64(S); end;
          ftSingle: begin SetString(S, Data^.LibRow^[Field.FieldNo - 1], Data^.LibLengths^[Field.FieldNo - 1]); Single(Buffer^) := StrToFloat(S, Connection.FormatSettings); end;
          ftFloat: begin SetString(S, Data^.LibRow^[Field.FieldNo - 1], Data^.LibLengths^[Field.FieldNo - 1]); Double(Buffer^) := StrToFloat(S, Connection.FormatSettings); end;
          ftExtended: begin SetString(S, Data^.LibRow^[Field.FieldNo - 1], Data^.LibLengths^[Field.FieldNo - 1]); Extended(Buffer^) := StrToFloat(S, Connection.FormatSettings); end;
          ftDate: begin SetString(S, Data^.LibRow^[Field.FieldNo - 1], Data^.LibLengths^[Field.FieldNo - 1]); DT := MySQLDB.StrToDate(S, Connection.FormatSettings); DataConvert(Field, @DT, Buffer, True); end;
          ftDateTime: begin SetString(S, Data^.LibRow^[Field.FieldNo - 1], Data^.LibLengths^[Field.FieldNo - 1]); DT := MySQLDB.StrToDateTime(S, Connection.FormatSettings); DataConvert(Field, @DT, Buffer, True); end;
          ftTime: begin SetString(S, Data^.LibRow^[Field.FieldNo - 1], Data^.LibLengths^[Field.FieldNo - 1]); Longint(Buffer^) := StrToTime(S, TMySQLTimeField(Field).SQLFormat); end;
          ftTimeStamp: begin SetString(S, Data^.LibRow^[Field.FieldNo - 1], Data^.LibLengths^[Field.FieldNo - 1]); PSQLTimeStamp(Buffer)^ := StrToMySQLTimeStamp(S, TMySQLTimeStampField(Field).SQLFormat); end;
          ftBlob: begin TMemoryStream(Buffer).SetSize(Data^.LibLengths^[Field.FieldNo - 1]); Move(Data^.LibRow^[Field.FieldNo - 1]^, TMemoryStream(Buffer).Memory^, Data^.LibLengths^[Field.FieldNo - 1]); end;
          ftWideString: Move(Data, Buffer^, SizeOf(Data));
          ftWideMemo: DataConvert(Field, Data, Buffer, False);
          else raise EDatabaseError.CreateFMT(SUnknownFieldType + '(%d)', [Field.Name, Integer(Field.DataType)]);
        end;
    except
      on E: EConvertError do
      begin
        if (FInformConvertError) then
        begin
          FInformConvertError := False;
          SetString(S, Data^.LibRow^[Field.FieldNo - 1], Data^.LibLengths^[Field.FieldNo - 1]);
          Connection.DoConvertError(Field, S, E);
        end;
        Result := False;
      end;
    end;
end;

function TMySQLQuery.GetHandle(): MySQLConsts.MYSQL_RES;
begin
  if (not Assigned(Connection)) then
    Result := nil
  else
  begin
    Connection.TerminateCS.Enter();
    if (not Assigned(SyncThread)) then
      Result := nil
    else
      Result := SyncThread.ResHandle;
    Connection.TerminateCS.Leave();
  end;
end;

function TMySQLQuery.GetLibLengths(): MYSQL_LENGTHS;
begin
  Assert(Active);
  Assert(Assigned(ActiveBuffer()));

  Result := PRecordBufferData(ActiveBuffer())^.LibLengths;
end;

function TMySQLQuery.GetLibRow(): MYSQL_ROW;
begin
  Assert(Active);
  Assert(Assigned(ActiveBuffer()));

  Result := PRecordBufferData(ActiveBuffer())^.LibRow;
end;

function TMySQLQuery.GetIsIndexField(Field: TField): Boolean;
begin
  Result := pfInKey in Field.ProviderFlags;
end;

function TMySQLQuery.GetRecNo(): Integer;
begin
  Result := FRecNo;
end;

function TMySQLQuery.GetRecord(Buffer: TRecordBuffer; GetMode: TGetMode; DoCheck: Boolean): TGetResult;
begin
  if (GetMode <> gmNext) then
    Result := grError
  else if (not Assigned(SyncThread.ResHandle)) then
    Result := grEOF
  else
  begin
    PRecordBufferData(ActiveBuffer())^.LibRow := Connection.Lib.mysql_fetch_row(SyncThread.ResHandle);
    if (Assigned(PRecordBufferData(ActiveBuffer())^.LibRow)) then
    begin
      PRecordBufferData(ActiveBuffer())^.LibLengths := Connection.Lib.mysql_fetch_lengths(SyncThread.ResHandle);

      Inc(FRecNo);
      Result := grOk;
    end
    else if (Connection.Lib.mysql_errno(Connection.SyncThread.LibHandle) <> 0) then
    begin
      SyncThread.ErrorCode := Connection.Lib.mysql_errno(Connection.SyncThread.LibHandle);
      SyncThread.ErrorMessage := Connection.GetErrorMessage(Connection.SyncThread.LibHandle);
      Connection.DoError(SyncThread.ErrorCode, SyncThread.ErrorMessage);
      Result := grError;
    end
    else
      Result := grEOF;
  end;
end;

function TMySQLQuery.GetRecordCount(): Integer;
begin
  Result := FRecNo + 1;
end;

function TMySQLQuery.GetUniDirectional(): Boolean;
begin
  Result := True;
end;

procedure TMySQLQuery.InternalClose();
begin
  if (Assigned(SyncThread)) then
    Connection.SyncReleaseDataSet(Self);

  FIndexDefs.Clear();

  FieldDefs.Clear();
  Fields.Clear();
end;

procedure TMySQLQuery.InternalHandleException();
begin
  Application.HandleException(Self);
end;

procedure TMySQLQuery.InternalInitFieldDefs();
var
  Binary: Boolean;
  CreateField: Boolean;
  Decimals: Word;
  DName: string;
  Field: TField;
  I: Integer;
  Len: Longword;
  LibField: TMYSQL_FIELD;
  RawField: MYSQL_FIELD;
  S: string;
  UniqueDatabaseName: Boolean;
begin
  if (FieldDefs.Count = 0) then
  begin
    if (not Assigned(Handle)) then
      for I := 0 to FieldCount - 1 do
      begin
        if (Fields[I].FieldName = '') then Fields[I].FieldName := Fields[I].Name;
        FieldDefs.Add(Fields[I].FieldName, Fields[I].DataType, Fields[I].Size, Fields[I].Required);
      end
    else
    begin
      FieldDefs.Clear();
      UniqueDatabaseName := True;

      repeat
        RawField := MYSQL_FIELD(Connection.Lib.mysql_fetch_field(Handle));

        if (Assigned(RawField)) then
        begin
          Connection.Lib.SetField(RawField, LibField);

          if ((Connection.MySQLVersion < 40101) or (Connection.Lib.Version < 40101)) then
          begin
            Binary := LibField.flags and BINARY_FLAG <> 0;
            if (not (LibField.field_type in [MYSQL_TYPE_ENUM, MYSQL_TYPE_SET, MYSQL_TYPE_TINY_BLOB, MYSQL_TYPE_MEDIUM_BLOB, MYSQL_TYPE_LONG_BLOB, MYSQL_TYPE_BLOB, MYSQL_TYPE_VAR_STRING, MYSQL_TYPE_STRING]) or (LibField.flags and BINARY_FLAG <> 0)) then
              Len := LibField.length
            else if (Connection.MySQLVersion <= 40109) then // In 40109 this is needed. In 40122 and higher the problem is fixed. What is the exact ServerVersion?
              Len := LibField.length
            else
            begin
              if (MySQL_Character_Sets[Connection.CharsetNr].MaxLen = 0) then
                raise ERangeError.CreateFmt(SPropertyOutOfRange + ' - Charset: %s', ['MaxLen', MySQL_Character_Sets[Connection.CharsetNr].CharsetName])
              else
                Len := LibField.length div MySQL_Character_Sets[Connection.CharsetNr].MaxLen;
            end;
          end
          else
          begin
            Binary := LibField.charsetnr = 63;
            Len := LibField.length;
            if (not Binary and (Connection.MySQLVersion > 40109)) then // In 40109 this is needed. In 40122 and higher the problem is fixed. What is the exact ServerVersion?
              for I := 0 to Length(MySQL_Collations) - 1 do
                if (MySQL_Collations[I].CharsetNr = LibField.charsetnr) then
                  if (MySQL_Collations[I].MaxLen = 0) then
                    raise ERangeError.CreateFmt(SPropertyOutOfRange + ' - CharsetNr: %d', ['MaxLen', MySQL_Collations[I].CharsetNr])
                  else
                    Len := LibField.length div MySQL_Collations[I].MaxLen;
          end;
          Binary := Binary or (LibField.field_type = MYSQL_TYPE_IPV6);
          Len := Len and $7FFFFFFF;

          case (LibField.field_type) of
            MYSQL_TYPE_NULL:
              Field := TField.Create(Self);
            MYSQL_TYPE_BIT:
              begin Field := TMySQLBitField.Create(Self); Field.Tag := ftBitField; end;
            MYSQL_TYPE_TINY:
              if (LibField.flags and UNSIGNED_FLAG = 0) then
                Field := TMySQLShortIntField.Create(Self)
              else
                Field := TMySQLByteField.Create(Self);
            MYSQL_TYPE_SHORT:
              if (LibField.flags and UNSIGNED_FLAG = 0) then
                Field := TMySQLSmallIntField.Create(Self)
              else
                Field := TMySQLWordField.Create(Self);
            MYSQL_TYPE_INT24,
            MYSQL_TYPE_LONG:
              if (LibField.flags and UNSIGNED_FLAG = 0) then
                Field := TMySQLIntegerField.Create(Self)
              else
                Field := TMySQLLongWordField.Create(Self);
            MYSQL_TYPE_LONGLONG:
              if (LibField.flags and UNSIGNED_FLAG = 0) then
                Field := TLargeintField.Create(Self)
              else
                Field := TMySQLLargeWordField.Create(Self);
            MYSQL_TYPE_FLOAT:
              Field := TMySQLSingleField.Create(Self);
            MYSQL_TYPE_DOUBLE:
              Field := TMySQLFloatField.Create(Self);
            MYSQL_TYPE_DECIMAL,
            MYSQL_TYPE_NEWDECIMAL:
              Field := TMySQLExtendedField.Create(Self);
            MYSQL_TYPE_TIMESTAMP:
              if (Len in [2, 4, 6, 8, 10, 12, 14]) then
                Field := TMySQLTimeStampField.Create(Self)
              else if ((Integer(Len) <= Length(Connection.FormatSettings.LongDateFormat + ' ' + Connection.FormatSettings.LongTimeFormat))) then
                Field := TMySQLDateTimeField.Create(Self)
              else // Fractal seconds
                begin Field := TMySQLWideStringField.Create(Self); Field.Size := Len; end;
            MYSQL_TYPE_DATE:
              Field := TMySQLDateField.Create(Self);
            MYSQL_TYPE_TIME:
              if (Integer(Len - 2) <= Length(Connection.FormatSettings.LongTimeFormat)) then
                Field := TMySQLTimeField.Create(Self)
              else
                begin Field := TMySQLWideStringField.Create(Self); Field.Size := Len; end;
            MYSQL_TYPE_DATETIME,
            MYSQL_TYPE_NEWDATE:
              if ((Integer(Len) <= Length(Connection.FormatSettings.LongDateFormat + ' ' + Connection.FormatSettings.LongTimeFormat))) then
                Field := TMySQLDateTimeField.Create(Self)
              else
                begin Field := TMySQLWideStringField.Create(Self); Field.Size := Len; end;
            MYSQL_TYPE_YEAR:
              if (Len = 2) then
                Field := TMySQLByteField.Create(Self)
              else
                Field := TMySQLWordField.Create(Self);
            MYSQL_TYPE_ENUM,
            MYSQL_TYPE_SET:
              if (Binary) then
                begin Field := TMySQLStringField.Create(Self); if (Connection.MySQLVersion < 40100) then Field.Size := Len + 1 else Field.Size := Len; end
              else
                begin Field := TMySQLWideStringField.Create(Self); Field.Size := Len; end;
            MYSQL_TYPE_TINY_BLOB,
            MYSQL_TYPE_MEDIUM_BLOB,
            MYSQL_TYPE_LONG_BLOB,
            MYSQL_TYPE_BLOB:
              if (Binary) then
                begin Field := TMySQLBlobField.Create(Self); Field.Size := Len; end
              else
                begin Field := TMySQLWideMemoField.Create(Self); Field.Size := Len; end;
            MYSQL_TYPE_IPV6,
            MYSQL_TYPE_VAR_STRING,
            MYSQL_TYPE_STRING:
              if (Binary) then
                begin Field := TMySQLStringField.Create(Self); if (Connection.MySQLVersion < 40100) then Field.Size := Len + 1 else Field.Size := Len; end
              else if ((Len <= $FF)  and (Connection.MySQLVersion < 50000)) { ENum&Set are not marked as MYSQL_TYPE_ENUM & MYSQL_TYPE_SET in older MySQL versions} then
                begin Field := TMySQLWideStringField.Create(Self); Field.Size := $FF; end
              else if ((Len <= $5555) and (Connection.MySQLVersion >= 50000)) then
                begin Field := TMySQLWideStringField.Create(Self); Field.Size := 65535; end
              else
                begin Field := TMySQLWideMemoField.Create(Self); Field.Size := Len; end;
            MYSQL_TYPE_GEOMETRY:
              begin Field := TMySQLBlobField.Create(Self); Field.Size := Len; Field.Tag := ftGeometryField; end;
            else
              raise EDatabaseError.CreateFMT(SBadFieldType + ' (%d)', [LibField.name, Byte(LibField.field_type)]);
          end;

          case (LibField.field_type) of
            MYSQL_TYPE_TINY:  // 8 bit
              if (LibField.flags and UNSIGNED_FLAG = 0) then
                begin TShortIntField(Field).MinValue := -$80; TShortIntField(Field).MaxValue := $7F; end
              else
                begin TByteField(Field).MinValue := 0; TByteField(Field).MaxValue := $FF; end;
            MYSQL_TYPE_SHORT: // 16 bit
              if (LibField.flags and UNSIGNED_FLAG = 0) then
                begin TSmallIntField(Field).MinValue := -$8000; TSmallIntField(Field).MaxValue := $7FFF; end
              else
                begin TWordField(Field).MinValue := 0; TWordField(Field).MaxValue := $FFFF; end;
            MYSQL_TYPE_INT24: // 24 bit
              if (LibField.flags and UNSIGNED_FLAG = 0) then
                begin TIntegerField(Field).MinValue := -$800000; TIntegerField(Field).MaxValue := $7FFFFF; end
              else
                begin TLongWordField(Field).MinValue := 0; TLongWordField(Field).MaxValue := $FFFFFF; end;
            MYSQL_TYPE_LONG: // 32 bit
              if (LibField.flags and UNSIGNED_FLAG = 0) then
                begin TIntegerField(Field).MinValue := -$80000000; TIntegerField(Field).MaxValue := $7FFFFFFF; end
              else
                begin TLongWordField(Field).MinValue := 0; TLongWordField(Field).MaxValue := $FFFFFFFF; end;
            MYSQL_TYPE_LONGLONG: // 64 bit
              if (LibField.flags and UNSIGNED_FLAG = 0) then
                begin TLargeintField(Field).MinValue := -$8000000000000000; TLargeintField(Field).MaxValue := $7FFFFFFFFFFFFFFF; end
              else
                begin TMySQLLargeWordField(Field).MinValue := 0; TMySQLLargeWordField(Field).MaxValue := $FFFFFFFFFFFFFFFF; end;
            MYSQL_TYPE_YEAR:
              if (Len = 2) then
                begin TByteField(Field).MinValue := 0; TByteField(Field).MaxValue := 99; end
              else
                begin TWordField(Field).MinValue := 1901; TWordField(Field).MaxValue := 2155; end
          end;

          Field.FieldName := Connection.LibDecode(LibField.name);
          if (Assigned(FindField(Field.FieldName))) then
          begin
            I := 2;
            while (Assigned(FindField(Field.FieldName + '_' + IntToStr(I)))) do Inc(I);
            Field.FieldName := Field.FieldName + '_' + IntToStr(I);
          end;

          Field.Required := LibField.flags and NOT_NULL_FLAG <> 0;

          CreateField := Pos('.', Field.Origin) = 0;
          if (CreateField) then
          begin
            if ((Connection.Lib.Version >= 40100) and (LibField.org_name_length > 0)) then
              Field.Origin := '"' + Connection.LibDecode(LibField.org_name) + '"'
            else if (LibField.name_length > 0) then
              Field.Origin := '"' + Connection.LibDecode(LibField.name) + '"'
            else
              Field.Origin := '';
            if (Field.Origin <> '') then
              if ((Connection.Lib.Version >= 40000) and (LibField.org_table_length > 0)) then
              begin
                Field.Origin := '"' + Connection.LibDecode(LibField.org_table) + '".' + Field.Origin;
                if ((Connection.Lib.Version >= 40101) and (LibField.db_length > 0)) then
                  Field.Origin := '"' + Connection.LibDecode(LibField.db) + '".' + Field.Origin;
              end
              else if (LibField.table_length > 0) then
                Field.Origin := '"' + Connection.LibDecode(LibField.table) + '".' + Field.Origin;
            Field.ReadOnly := LibField.table = '';
            if ((Connection.Lib.Version >= 40101) and (LibField.db_length > 0)) then
              if (DName = '') then
                DName := Connection.LibDecode(LibField.db)
              else
                UniqueDatabaseName := UniqueDatabaseName and (Connection.LibDecode(LibField.db) = DName);

            if (LibField.flags and (AUTO_INCREMENT_FLAG) <> 0) then
              Field.AutoGenerateValue := arAutoInc
            else
              Field.AutoGenerateValue := arNone;
            if (Field.DataType = ftTime) then
              TMySQLTimeField(Field).SQLFormat := DisplayFormatToSQLFormat(Connection.FormatSettings.LongTimeFormat)
            else if (Field.DataType = ftTimeStamp) then
              case (Len) of
                2: TMySQLTimeStampField(Field).SQLFormat := '%y';
                4: TMySQLTimeStampField(Field).SQLFormat := '%y%m';
                6: TMySQLTimeStampField(Field).SQLFormat := '%y%m%d';
                8: TMySQLTimeStampField(Field).SQLFormat := '%Y%m%d';
                10: TMySQLTimeStampField(Field).SQLFormat := '%y%m%d%H%i';
                12: TMySQLTimeStampField(Field).SQLFormat := '%y%m%d%H%i%s';
                14: TMySQLTimeStampField(Field).SQLFormat := '%Y%m%d%H%i%s';
              end;

            case (Field.DataType) of
              ftShortInt,
              ftByte,
              ftSmallInt,
              ftWord,
              ftInteger,
              ftLongWord,
              ftSingle,
              ftFloat,
              ftExtended:
                begin
                  if (Len = 0) then
                    S := '0'
                  else if (BitField(Field) or (LibField.flags and ZEROFILL_FLAG <> 0)) then
                    S := StringOfChar('0', Len)
                  else
                    S := StringOfChar('#', Len - 1) + '0';
                  Decimals := LibField.decimals;
                  if (Decimals > Len) then Decimals := Len;
                  if ((Field.DataType in [ftSingle, ftFloat, ftExtended]) and (Decimals > 0)) then
                  begin
                    System.Delete(S, 1, Decimals + 1);
                    S := S + '.' + StringOfChar('0', Decimals);
                  end;
                  if (Length(S) > 256 - 32) then // Limit given because of the usage of SysUtils.FormatFloat
                    System.Delete(S, 1, Length(S) - 32);
                  TNumericField(Field).DisplayFormat := S;
                end;
              ftDate: TMySQLDateField(Field).DisplayFormat := Connection.FormatSettings.ShortDateFormat;
              ftTime: TMySQLTimeField(Field).DisplayFormat := Connection.FormatSettings.LongTimeFormat;
              ftDateTime: TMySQLDateTimeField(Field).DisplayFormat := Connection.FormatSettings.ShortDateFormat + ' ' + Connection.FormatSettings.LongTimeFormat;
              ftTimeStamp: TMySQLTimeStampField(Field).DisplayFormat := SQLFormatToDisplayFormat(TMySQLTimeStampField(Field).SQLFormat);
            end;

            if (Assigned(Handle)) then
            begin
              Field.DisplayLabel := Connection.LibDecode(LibField.name);
              case (Field.DataType) of
                ftBlob: Field.DisplayWidth := 7;
                ftWideMemo: Field.DisplayWidth := 8;
                else
                  case (Field.Tag) of
                    ftBitField: Field.DisplayWidth := Len;
                    ftGeometryField: Field.DisplayWidth := 7;
                    else Field.DisplayWidth := Len;
                  end;
              end;
            end;

            if ((Field.Name = '') and IsValidIdent(ReplaceStr(ReplaceStr(Field.FieldName, ' ', '_'), '.', '_'))) then
              Field.Name := ReplaceStr(ReplaceStr(Field.FieldName, ' ', '_'), '.', '_');
            if (Field.Name = '') then
              Field.Name := 'Field' + '_' + IntToStr(FieldDefs.Count);
            if (Field.FieldName = '') then
              Field.FieldName := Field.Name;


            if (LibField.flags and PRI_KEY_FLAG = 0) then
              Field.ProviderFlags := Field.ProviderFlags - [pfInKey]
            else
              Field.ProviderFlags := Field.ProviderFlags + [pfInKey];

            if (Assigned(Handle)) then
              Field.DataSet := Self;

            FieldDefs.Add(Field.FieldName, Field.DataType, Field.Size, Field.Required);

            if (not Assigned(Handle)) then
              FreeAndNil(Field)
          end;
        end;
      until (not Assigned(RawField));

      if (UniqueDatabaseName and (DName <> '')) then
        FDatabaseName := DName;
    end;
  end;
end;

procedure TMySQLQuery.InternalOpen();
var
  OpenFromSyncThread: Boolean;
begin
  Assert(Assigned(Connection) and Assigned(Connection.Lib) and not Assigned(SyncThread));

  FInformConvertError := True;
  FRowsAffected := -1;
  FRecNo := -1;

  OpenFromSyncThread := Assigned(Connection.SyncThread) and (Connection.SyncThread.State = ssResult);

  if (OpenFromSyncThread) then
    SyncThread := Connection.SyncThread;

  InitFieldDefs();
  BindFields(True);

  if (OpenFromSyncThread) then
    Connection.SyncBindDataSet(Self);
end;

function TMySQLQuery.IsCursorOpen(): Boolean;
begin
  Result := Assigned(SyncThread);
end;

procedure TMySQLQuery.Open(const DataHandle: TMySQLConnection.TDataResult);
begin
  Connection := DataHandle.Connection;

  if (CommandType = ctQuery) then
  begin
    FCommandText := DataHandle.CommandText;
    FDatabaseName := Connection.DatabaseName;
  end;

  SetActiveEvent(DataHandle.ErrorCode, DataHandle.ErrorMessage, DataHandle.WarningCount,
    FCommandText, DataHandle, Assigned(DataHandle.ResHandle));
end;

procedure TMySQLQuery.SetActive(Value: Boolean);
var
  SQL: string;
begin
  if (Value <> Active) then
    if (not Value) then
      inherited
    else if (FieldCount > 0) then
      inherited
    else
    begin
      if (CommandType <> ctTable) then
        SQL := CommandText
      else
        SQL := SQLSelect();

      Connection.InternExecuteSQL(smDataSet, True, SQL, SetActiveEvent);
    end;
end;

function TMySQLQuery.SetActiveEvent(const ErrorCode: Integer; const ErrorMessage: string; const WarningCount: Integer;
  const CommandText: string; const DataHandle: TMySQLConnection.TDataResult; const Data: Boolean): Boolean;
begin
  Assert(not Assigned(SyncThread));
  Assert(DataHandle = Connection.SyncThread);

  if (not Data or (DataHandle.ErrorCode <> 0)) then
    SetState(dsInactive)
  else
  begin
    DoBeforeOpen();
    SetState(dsOpening);
    OpenCursorComplete();
  end;

  Result := False;
end;

procedure TMySQLQuery.SetCommandText(const ACommandText: string);
begin
  Assert(not Active);

  FCommandText := ACommandText;
end;

procedure TMySQLQuery.SetConnection(const AConnection: TMySQLConnection);
begin
  Assert(not IsCursorOpen());

  if (not Assigned(FConnection) and Assigned(AConnection)) then
    AConnection.RegisterClient(Self);
  if (Assigned(FConnection) and not Assigned(AConnection)) then
    FConnection.UnRegisterClient(Self);

  FConnection := AConnection;
end;

function TMySQLQuery.SQLFieldValue(const Field: TField; Data: PRecordBufferData = nil): string;
begin
  if (not Assigned(Data)) then
    if (not (Self is TMySQLDataSet)) then
      Data := PRecordBufferData(ActiveBuffer())
    else
      Data := TMySQLDataSet.PExternRecordBuffer(ActiveBuffer())^.InternRecordBuffer^.NewData;

  if (not Assigned(Data) or not Assigned(Data^.LibRow^[Field.FieldNo - 1])) then
    if (not Field.Required) then
      Result := 'NULL'
    else
      Result := 'DEFAULT'
  else if (BitField(Field)) then
    Result := 'b''' + Field.AsString + ''''
  else
    case (Field.DataType) of
      ftString: Result := SQLEscapeBin(Data^.LibRow^[Field.FieldNo - 1], Data^.LibLengths^[Field.FieldNo - 1], Connection.MySQLVersion <= 40000);
      ftShortInt,
      ftByte,
      ftSmallInt,
      ftWord,
      ftInteger,
      ftLongWord,
      ftLargeint,
      ftSingle,
      ftFloat,
      ftExtended: Result := Connection.LibUnpack(Data^.LibRow^[Field.FieldNo - 1], Data^.LibLengths^[Field.FieldNo - 1]);
      ftDate,
      ftDateTime,
      ftTime: Result := '''' + Connection.LibUnpack(Data^.LibRow^[Field.FieldNo - 1], Data^.LibLengths^[Field.FieldNo - 1]) + '''';
      ftTimeStamp: Connection.LibUnpack(Data^.LibRow^[Field.FieldNo - 1], Data^.LibLengths^[Field.FieldNo - 1]);
      ftBlob: Result := SQLEscapeBin(Data^.LibRow^[Field.FieldNo - 1], Data^.LibLengths^[Field.FieldNo - 1], Connection.MySQLVersion <= 40000);
      ftWideMemo,
      ftWideString: Result := SQLEscape(Connection.LibDecode(Data^.LibRow^[Field.FieldNo - 1], Data^.LibLengths^[Field.FieldNo - 1]));
      else raise EDatabaseError.CreateFMT(SUnknownFieldType + '(%d)', [Field.Name, Integer(Field.DataType)]);
    end;
end;

function TMySQLQuery.SQLSelect(): string;
begin
  raise EAbstractError.Create(SAbstractError);
end;

procedure TMySQLQuery.UpdateIndexDefs();
begin
  if (not Assigned(Handle) and not FIndexDefs.Updated) then
  begin
    if (Assigned(Connection.OnUpdateIndexDefs)) then
      Connection.OnUpdateIndexDefs(Self, FIndexDefs);
    FIndexDefs.Updated := True;
  end;
end;

{ TMySQLDataSetBlobStream *****************************************************}

constructor TMySQLDataSetBlobStream.Create(const AField: TBlobField; AMode: TBlobStreamMode);
begin
  inherited Create();

  Empty := True;
  Field := AField;
  Mode := AMode;

  if (Mode in [bmRead, bmReadWrite]) then
  begin
    Empty := not Assigned(TMySQLDataSet.PExternRecordBuffer(Field.DataSet.ActiveBuffer()))
      or not Assigned(TMySQLDataSet.PExternRecordBuffer(Field.DataSet.ActiveBuffer())^.InternRecordBuffer)
      or not Assigned(TMySQLDataSet.PExternRecordBuffer(Field.DataSet.ActiveBuffer())^.InternRecordBuffer^.NewData)
      or not Assigned(TMySQLDataSet.PExternRecordBuffer(Field.DataSet.ActiveBuffer())^.InternRecordBuffer^.NewData^.LibRow^[Field.FieldNo - 1]);
    if (Empty or (TMySQLDataSet.PExternRecordBuffer(Field.DataSet.ActiveBuffer())^.InternRecordBuffer^.NewData^.LibLengths^[Field.FieldNo - 1] = 0)) then
      SetSize(0)
    else
      TMySQLDataSet(Field.DataSet).GetFieldData(Field, Self, TMySQLDataSet.PExternRecordBuffer(Field.DataSet.ActiveBuffer())^.InternRecordBuffer^.NewData);
  end;
end;

destructor TMySQLDataSetBlobStream.Destroy();
begin
  if (Mode in [bmWrite, bmReadWrite]) then
  begin
    if (Empty) then
      TMySQLDataSet(Field.DataSet).SetFieldData(Field, nil)
    else
      TMySQLDataSet(Field.DataSet).SetFieldData(Field, Self);

    TMySQLDataSet(Field.DataSet).DataEvent(deFieldChange, Longint(Field));
  end;

  inherited;
end;

function TMySQLDataSetBlobStream.Write(const Buffer; Len: Integer): Longint;
begin
  Empty := False;
  Result := inherited Write(Buffer, Len);
end;

{ TMySQLDataSet.TInternRecordBuffers ******************************************}

procedure TMySQLDataSet.TInternRecordBuffers.Clear();
var
  I: Integer;
begin
  CriticalSection.Enter();
  if ((DataSet.State = dsBrowse) and Assigned(DataSet.ActiveBuffer()) and Assigned(PExternRecordBuffer(DataSet.ActiveBuffer())^.InternRecordBuffer)) then
    PExternRecordBuffer(DataSet.ActiveBuffer())^.InternRecordBuffer := nil;
  for I := 0 to Count - 1 do
    DataSet.FreeInternRecordBuffer(Items[I]);
  inherited Clear();
  FilteredRecordCount := 0;
  Index := -1;
  Received.ResetEvent();
  CriticalSection.Leave();
end;

constructor TMySQLDataSet.TInternRecordBuffers.Create(const ADataSet: TMySQLDataSet);
begin
  inherited Create();

  FDataSet := ADataSet;

  CriticalSection := TCriticalSection.Create();
  Index := -1;
  FRecordReceived := TEvent.Create(nil, True, False, '');
end;

destructor TMySQLDataSet.TInternRecordBuffers.Destroy();
begin
  inherited;

  CriticalSection.Free();
  FRecordReceived.Free();
end;

function TMySQLDataSet.TInternRecordBuffers.Get(Index: Integer): PInternRecordBuffer;
begin
  Result := PInternRecordBuffer(Items[Index]);
end;

procedure TMySQLDataSet.TInternRecordBuffers.Put(Index: Integer; Buffer: PInternRecordBuffer);
begin
  Items[Index] := Buffer;
end;

{ TMySQLDataSet ***************************************************************}

procedure TMySQLDataSet.ActivateFilter();
var
  OldBookmark: TBookmark;
begin
  CheckBrowseMode();

  DisableControls();
  DoBeforeScroll();

  OldBookmark := Bookmark;

  InternActivateFilter();

  if (not BookmarkValid(OldBookmark)) then
    First()
  else
    Bookmark := OldBookmark;

  DoAfterScroll();
  EnableControls();
end;

function TMySQLDataSet.AllocInternRecordBuffer(): PInternRecordBuffer;
begin
  try
    GetMem(Result, SizeOf(Result^));
  except
    Result := nil;
  end;

  if (Assigned(Result)) then
  begin
    Result^.NewData := nil;
    Result^.OldData := nil;
    Result^.VisibleInFilter := True;
  end;
end;

function TMySQLDataSet.AllocRecordBuffer(): TRecordBuffer;
begin
  try
    New(PExternRecordBuffer(Result));

    PExternRecordBuffer(Result)^.InternRecordBuffer := nil;
    PExternRecordBuffer(Result)^.Index := -1;
    PExternRecordBuffer(Result)^.BookmarkFlag := bfInserted;
  except
    Result := nil;
  end;
end;

function TMySQLDataSet.BookmarkToInternBufferIndex(const Bookmark: TBookmark): Integer;
var
  I: Integer;
begin
  Result := -1;

  if (Length(Bookmark) = BookmarkSize) then
  begin
    I := 0;
    while ((Result < 0) and (I < InternRecordBuffers.Count)) do
    begin
      if (PPointer(@Bookmark[0])^ = InternRecordBuffers[I]) then
        Result := I;
      Inc(I);
    end;
  end;
end;

function TMySQLDataSet.BookmarkValid(Bookmark: TBookmark): Boolean;
var
  Index: Integer;
begin
  Result := (Length(Bookmark) = BookmarkSize);
  if (Result) then
  begin
    Index := BookmarkToInternBufferIndex(Bookmark);
    Result := (Index >= 0) and (not Filtered or InternRecordBuffers[Index]^.VisibleInFilter);
  end;
end;

procedure TMySQLDataSet.ClearBuffers();
var
  I: Integer;
begin
  inherited;

  for I := 0 to BufferCount - 1 do
    PExternRecordBuffer(Buffers[I])^.InternRecordBuffer := nil;
end;

function TMySQLDataSet.CompareBookmarks(Bookmark1, Bookmark2: TBookmark): Integer;
begin
  Result := Sign(BookmarkToInternBufferIndex(Bookmark1) - BookmarkToInternBufferIndex(Bookmark2));
end;

constructor TMySQLDataSet.Create(AOwner: TComponent);
begin
  inherited;

  FCanModify := False;
  FCommandType := ctQuery;
  FCursorOpen := False;
  FDataSize := 0;
  FilterParser := nil;
  FInternRecordBuffers := TInternRecordBuffers.Create(Self);
  FLocateNext := False;
  FRecordsReceived := TEvent.Create(nil, True, False, '');
  FSortDef := TIndexDef.Create(nil, '', '', []);
  TableName := '';

  BookmarkSize := SizeOf(InternRecordBuffers[0]);

  SetUniDirectional(False);
  FilterOptions := [foNoPartialCompare];
end;

function TMySQLDataSet.CreateBlobStream(Field: TField; Mode: TBlobStreamMode): TStream;
begin
  case (Field.DataType) of
    ftBlob: Result := TMySQLDataSetBlobStream.Create(TMySQLBlobField(Field), Mode);
    ftWideMemo: Result := TMySQLDataSetBlobStream.Create(TMySQLWideMemoField(Field), Mode);
    else Result := inherited CreateBlobStream(Field, Mode);
  end;
end;

procedure TMySQLDataSet.DeactivateFilter();
var
  OldBookmark: TBookmark;
begin
  CheckBrowseMode();
  DisableControls();
  DoBeforeScroll();

  OldBookmark := Bookmark;

  if (Assigned(FilterParser)) then
    FreeAndNil(FilterParser);

  if (not BookmarkValid(OldBookmark)) then
    First()
  else
    Bookmark := OldBookmark;

  DoAfterScroll();
  EnableControls();
end;

procedure TMySQLDataSet.Delete(const Bookmarks: array of TBookmark);
var
  I: Integer;
begin
  SetLength(DeleteBookmarks, Length(Bookmarks));

  for I := 0 to Length(DeleteBookmarks) - 1 do
    DeleteBookmarks[I] := Bookmarks[I];

  Delete();

  SetLength(DeleteBookmarks, 0);
end;

destructor TMySQLDataSet.Destroy();
begin
  inherited;

  FSortDef.Free();
  FRecordsReceived.Free();
  if (Assigned(FilterParser)) then
    FreeAndNil(FilterParser);
  FInternRecordBuffers.Free();
end;

function TMySQLDataSet.FindRecord(Restart, GoForward: Boolean): Boolean;
var
  Distance: Integer;
begin
  if (Restart) then
  begin
    Result := RecordCount > 0;
    if (Result) then
      if (GoForward) then
        First()
      else
        Last();
  end
  else
  begin
    if (GoForward) then
      Distance := +1
    else
      Distance := -1;
    Result := MoveBy(Distance) <> 0;
  end;

  SetFound(Result);
end;

procedure TMySQLDataSet.FreeInternRecordBuffer(const InternRecordBuffer: PInternRecordBuffer);
begin
  if (Assigned(InternRecordBuffer^.NewData) and (InternRecordBuffer^.NewData <> InternRecordBuffer^.OldData)) then
    FreeMem(InternRecordBuffer^.NewData);
  if (Assigned(InternRecordBuffer^.OldData)) then
    FreeMem(InternRecordBuffer^.OldData);

  FreeMem(InternRecordBuffer);
end;

procedure TMySQLDataSet.FreeRecordBuffer(var Buffer: TRecordBuffer);
begin
  Dispose(Buffer); Buffer := nil;
end;

procedure TMySQLDataSet.GetBookmarkData(Buffer: TRecordBuffer; Data: Pointer);
begin
  PPointer(Data)^ := PExternRecordBuffer(Buffer)^.InternRecordBuffer;
end;

function TMySQLDataSet.GetBookmarkFlag(Buffer: TRecordBuffer): TBookmarkFlag;
begin
  Result := PExternRecordBuffer(Buffer)^.BookmarkFlag;
end;

function TMySQLDataSet.GetCanModify(): Boolean;
begin
  if (not IndexDefs.Updated) then
    UpdateIndexDefs();

  Result := CachedUpdates or FCanModify;
end;

function TMySQLDataSet.GetFieldData(Field: TField; Buffer: Pointer): Boolean;
begin
  Result := Assigned(ActiveBuffer())
    and Assigned(PExternRecordBuffer(ActiveBuffer())^.InternRecordBuffer)
    and GetFieldData(Field, Buffer, PExternRecordBuffer(ActiveBuffer())^.InternRecordBuffer^.NewData);
end;

function TMySQLDataSet.GetLibLengths(): MYSQL_LENGTHS;
begin
  if (not Active
    or not Assigned(ActiveBuffer())
    or not Assigned(PExternRecordBuffer(ActiveBuffer())^.InternRecordBuffer)
    or not Assigned(PExternRecordBuffer(ActiveBuffer())^.InternRecordBuffer^.NewData)) then
    Result := nil
  else
    Result := PExternRecordBuffer(ActiveBuffer())^.InternRecordBuffer^.NewData^.LibLengths;
end;

function TMySQLDataSet.GetLibRow(): MYSQL_ROW;
begin
  if (not Active) then
  else if (not Assigned(ActiveBuffer())) then
  else if (not Assigned(PExternRecordBuffer(ActiveBuffer())^.InternRecordBuffer)) then
  else if (not Assigned(PExternRecordBuffer(ActiveBuffer())^.InternRecordBuffer^.NewData)) then ;

  if (not Active
    or not Assigned(ActiveBuffer())
    or not Assigned(PExternRecordBuffer(ActiveBuffer())^.InternRecordBuffer)
    or not Assigned(PExternRecordBuffer(ActiveBuffer())^.InternRecordBuffer^.NewData)) then
    Result := nil
  else
    Result := PExternRecordBuffer(ActiveBuffer())^.InternRecordBuffer^.NewData^.LibRow;
end;

function TMySQLDataSet.GetMaxTextWidth(const Field: TField; const TextWidth: TTextWidth): Integer;
var
  I: Integer;
  Index: Integer;
begin
  if (InternRecordBuffers.Count = 0) then
    Result := TextWidth(StringOfChar('e', Field.DisplayWidth))
  else
    Result := 10;

  InternRecordBuffers.CriticalSection.Enter();
  Index := Field.FieldNo - 1;
  if ((not (Field.DataType in [ftWideString, ftWideMemo]))) then
    for I := 0 to InternRecordBuffers.Count - 1 do
      Result := Max(Result, TextWidth(Connection.LibUnpack(InternRecordBuffers[I]^.NewData^.LibRow^[Index], InternRecordBuffers[I]^.NewData^.LibLengths^[Index])))
  else
    for I := 0 to InternRecordBuffers.Count - 1 do
      Result := Max(Result, TextWidth(Connection.LibDecode(InternRecordBuffers[I]^.NewData^.LibRow^[Index], InternRecordBuffers[I]^.NewData^.LibLengths^[Index])));
  InternRecordBuffers.CriticalSection.Leave();
end;

function TMySQLDataSet.GetRecNo(): Integer;
begin
  if (PExternRecordBuffer(ActiveBuffer())^.BookmarkFlag <> bfCurrent) then
    Result := -1
  else
    Result := PExternRecordBuffer(ActiveBuffer())^.Index;
end;

function TMySQLDataSet.GetRecord(Buffer: TRecordBuffer; GetMode: TGetMode; DoCheck: Boolean): TGetResult;
var
  NewIndex: Integer;
begin
  NewIndex := InternRecordBuffers.Index;
  case (GetMode) of
    gmPrior:
      begin
        Result := grError;
        while (Result = grError) do
          if (NewIndex < 0) then
            Result := grBOF
          else
          begin
            Dec(NewIndex);
            if ((NewIndex >= 0) and (not Filtered or InternRecordBuffers[NewIndex]^.VisibleInFilter)) then
              Result := grOk;
          end;
      end;
    gmNext:
      begin
        Result := grError;
        while (Result = grError) do
        begin
          if ((NewIndex + 1 = InternRecordBuffers.Count) and not Filtered
            and ((RecordsReceived.WaitFor(IGNORE) <> wrSignaled) or (Self is TMySQLTable) and TMySQLTable(Self).LimitedDataReceived and TMySQLTable(Self).AutomaticLoadNextRecords and TMySQLTable(Self).LoadNextRecords())
            and Assigned(SyncThread)) then
            InternRecordBuffers.Received.WaitFor(NET_WAIT_TIMEOUT * 1000);

          if (NewIndex >= InternRecordBuffers.Count - 1) then
            Result := grEOF
          else
          begin
            Inc(NewIndex);
            if (not Filtered or InternRecordBuffers[NewIndex]^.VisibleInFilter) then
              Result := grOk;
          end;
        end;

        if (Result <> grEOF) then
          InternRecordBuffers.Received.ResetEvent();
      end;
    else // gmCurrent
      if (Filtered) then
      begin
        if (NewIndex < 0) then
          Result := grBOF
        else if (NewIndex < InternRecordBuffers.Count) then
          repeat
            if (not Filtered or InternRecordBuffers[NewIndex]^.VisibleInFilter) then
              Result := grOk
            else if (NewIndex + 1 = InternRecordBuffers.Count) then
              Result := grEOF
            else
            begin
              Result := grError;
              Inc(NewIndex);
            end;
          until (Result <> grError)
        else
        begin
          Result := grEOF;
          NewIndex := InternRecordBuffers.Count - 1;
        end;
        while ((Result = grEOF) and (NewIndex > 0)) do
          if (not Filtered or InternRecordBuffers[NewIndex]^.VisibleInFilter) then
            Result := grOk
          else
            Dec(NewIndex);
      end
      else if ((0 <= InternRecordBuffers.Index) and (InternRecordBuffers.Index < InternRecordBuffers.Count)) then
        Result := grOk
      else
        Result := grEOF;
  end;

  if (Result = grOk) then
  begin
    InternRecordBuffers.CriticalSection.Enter();
    InternRecordBuffers.Index := NewIndex;

    PExternRecordBuffer(Buffer)^.InternRecordBuffer := InternRecordBuffers[InternRecordBuffers.Index];
    PExternRecordBuffer(Buffer)^.Index := InternRecordBuffers.Index;
    PExternRecordBuffer(Buffer)^.BookmarkFlag := bfCurrent;

    InternRecordBuffers.CriticalSection.Leave();
  end;
end;

function TMySQLDataSet.GetRecordCount(): Integer;
begin
  if (Filtered) then
    Result := InternRecordBuffers.FilteredRecordCount
  else
    Result := InternRecordBuffers.Count;
end;

function TMySQLDataSet.GetUniDirectional(): Boolean;
begin
  Result := False;
end;

procedure TMySQLDataSet.InternActivateFilter();
var
  I: Integer;
begin
  if (Assigned(FilterParser)) then
    FilterParser.Free();
  FilterParser := TExprParser.Create(Self, Filter, FilterOptions, [poExtSyntax], '', nil, FldTypeMap);

  InternRecordBuffers.CriticalSection.Enter();

  InternRecordBuffers.FilteredRecordCount := 0;
  for I := 0 to InternRecordBuffers.Count - 1 do
  begin
    InternRecordBuffers[I]^.VisibleInFilter := VisibleInFilter(InternRecordBuffers[I]);
    if (InternRecordBuffers[I]^.VisibleInFilter) then
      Inc(InternRecordBuffers.FilteredRecordCount);
  end;

  InternRecordBuffers.CriticalSection.Leave();
end;

function TMySQLDataSet.InternAddRecord(const LibRow: MYSQL_ROW; const LibLengths: MYSQL_LENGTHS; const Index: Integer = -1): Boolean;
var
  Data: TMySQLQuery.TRecordBufferData;
  I: Integer;
  InternRecordBuffer: PInternRecordBuffer;
begin
  if (not Assigned(LibRow)) then
    Result := True
  else
  begin
    InternRecordBuffer := AllocInternRecordBuffer();
    Result := Assigned(InternRecordBuffer);

    if (Result) then
    begin
      Data.LibLengths := LibLengths;
      Data.LibRow := LibRow;

      Result := MoveRecordBufferData(InternRecordBuffer^.OldData, @Data);
      if (not Result) then
        FreeInternRecordBuffer(InternRecordBuffer)
      else
      begin
        InternRecordBuffer^.NewData := InternRecordBuffer^.OldData;
        InternRecordBuffer^.VisibleInFilter := not Filtered or VisibleInFilter(InternRecordBuffer);

        for I := 0 to FieldCount - 1 do
          Inc(FDataSize, Data.LibLengths^[I]);

        if (Filtered and InternRecordBuffer^.VisibleInFilter) then
          Inc(InternRecordBuffers.FilteredRecordCount);

        InternRecordBuffers.CriticalSection.Enter();
        if (Index >= 0) then
          InternRecordBuffers.Insert(Index, InternRecordBuffer)
        else
          InternRecordBuffers.Add(InternRecordBuffer);
        InternRecordBuffers.CriticalSection.Leave();
      end;
    end;
  end;

  if (not Assigned(LibRow) or not Result) then
  begin
    if ((Self is TMySQLTable) and Assigned(SyncThread)) then
      TMySQLTable(Self).FLimitedDataReceived :=
        Result and (Connection.Lib.mysql_num_rows(SyncThread.ResHandle) = TMySQLTable(Self).RequestedRecordCount);

    RecordsReceived.SetEvent();
  end;

  InternRecordBuffers.Received.SetEvent();
end;

procedure TMySQLDataSet.InternalAddRecord(Buffer: Pointer; Append: Boolean);
var
  Success: Boolean;
begin
  if (not Append and (InternRecordBuffers.Count > 0)) then
    Success := InternAddRecord(PExternRecordBuffer(Buffer)^.InternRecordBuffer^.NewData^.LibRow, PExternRecordBuffer(Buffer)^.InternRecordBuffer^.NewData^.LibLengths, InternRecordBuffers.Index)
  else
    Success := InternAddRecord(PExternRecordBuffer(Buffer)^.InternRecordBuffer^.NewData^.LibRow, PExternRecordBuffer(Buffer)^.InternRecordBuffer^.NewData^.LibLengths);

  if (not Success) then
    Connection.DoError(DS_OUT_OF_MEMORY, StrPas(CLIENT_ERRORS[DS_OUT_OF_MEMORY - DS_MIN_ERROR]));
end;

procedure TMySQLDataSet.InternalCancel();
begin
  if (PExternRecordBuffer(ActiveBuffer())^.BookmarkFlag <> bfCurrent) then
  begin
    FreeInternRecordBuffer(PExternRecordBuffer(ActiveBuffer())^.InternRecordBuffer);
    PExternRecordBuffer(ActiveBuffer())^.InternRecordBuffer := nil;
  end
  else if (PExternRecordBuffer(ActiveBuffer())^.InternRecordBuffer^.NewData <> PExternRecordBuffer(ActiveBuffer())^.InternRecordBuffer^.OldData) then
  begin
    FreeMem(PExternRecordBuffer(ActiveBuffer())^.InternRecordBuffer^.NewData);
    PExternRecordBuffer(ActiveBuffer())^.InternRecordBuffer^.NewData := PExternRecordBuffer(ActiveBuffer())^.InternRecordBuffer^.OldData;
  end;
end;

procedure TMySQLDataSet.InternalClose();
begin
  Connection.TerminateCS.Enter();
  if (Assigned(SyncThread) and (SyncThread = Connection.SyncThread) and Connection.SyncThread.IsRunning) then
    Connection.Terminate();
  Connection.TerminateCS.Leave();

  FSortDef.Fields := '';

  FCursorOpen := False;

  inherited;

  InternRecordBuffers.Clear();
  InternRecordBuffers.FilteredRecordCount := 0;
  FDataSize := 0;
end;

procedure TMySQLDataSet.InternalDelete();
var
  I: Integer;
  Index: Integer;
  J: Integer;
  SQL: string;
  Success: Boolean;
begin
  if (not CachedUpdates) then
  begin
    SQL := SQLDelete();
    if (Connection.DatabaseName <> DatabaseName) then
      SQL := Connection.SQLUse(DatabaseName) + SQL;
    Success := Connection.ExecuteSQL(SQL);
    if (Success and (Connection.RowsAffected = 0)) then
      raise EDatabasePostError.Create(SRecordChanged);

    InternRecordBuffers.CriticalSection.Enter();
    if (Length(DeleteBookmarks) = 0) then
    begin
      InternalSetToRecord(ActiveBuffer());
      FreeInternRecordBuffer(InternRecordBuffers[InternRecordBuffers.Index]);
      InternRecordBuffers.Delete(InternRecordBuffers.Index);
        for J := ActiveRecord + 1 to BufferCount - 1 do
          Dec(PExternRecordBuffer(Buffers[J])^.Index);
      if (Filtered) then
        Dec(InternRecordBuffers.FilteredRecordCount);
    end
    else
    begin
      for I := 0 to Max(1, Length(DeleteBookmarks)) - 1 do
      begin
        Index := BookmarkToInternBufferIndex(DeleteBookmarks[I]);
        if (Index < InternRecordBuffers.Index) then
          Dec(InternRecordBuffers.Index);
        for J := 0 to BufferCount - 1 do
          if (Assigned(PExternRecordBuffer(Buffers[J])) and (PExternRecordBuffer(Buffers[J])^.InternRecordBuffer = InternRecordBuffers[Index])) then
            PExternRecordBuffer(Buffers[J])^.InternRecordBuffer := nil;
        FreeInternRecordBuffer(InternRecordBuffers[Index]);
        InternRecordBuffers.Delete(Index);
        for J := ActiveRecord + 1 to BufferCount - 1 do
          Dec(PExternRecordBuffer(Buffers[J])^.Index);
        if (Filtered) then
          Dec(InternRecordBuffers.FilteredRecordCount);
      end;
    end;
    InternRecordBuffers.CriticalSection.Leave();

    PExternRecordBuffer(ActiveBuffer())^.InternRecordBuffer := nil;
  end;
end;

procedure TMySQLDataSet.InternalFirst();
begin
  InternRecordBuffers.Index := -1;
end;

procedure TMySQLDataSet.InternalGotoBookmark(Bookmark: Pointer);
var
  Index: Integer;
begin
  Index := InternRecordBuffers.IndexOf(PPointer(@TBookmark(Bookmark)[0])^);

  if (Index >= 0) then
    InternRecordBuffers.Index := Index;
end;

procedure TMySQLDataSet.InternalInitFieldDefs();
var
  FieldInfo: TFieldInfo;
  I: Integer;
  UniqueTableName: Boolean;
begin
  inherited;

  if (Self is TMySQLTable) then
    TableName := CommandText
  else
  begin
    UniqueTableName := True;
    for I := 0 to FieldCount - 1 do
      if (GetFieldInfo(Fields[I].Origin, FieldInfo)) then
      begin
        if (TableName = '') then
          TableName := FieldInfo.TableName;
        UniqueTableName := UniqueTableName and ((TableName = '') or (TableName = FieldInfo.TableName));
      end;

    if (not UniqueTableName) then
      TableName := '';
  end;
end;

procedure TMySQLDataSet.InternalInitRecord(Buffer: TRecordBuffer);
begin
  PExternRecordBuffer(Buffer)^.InternRecordBuffer := nil;
  PExternRecordBuffer(Buffer)^.Index := -1;
  PExternRecordBuffer(Buffer)^.BookmarkFlag := bfCurrent;
end;

procedure TMySQLDataSet.InternalInsert();
var
  I: Integer;
  RBS: RawByteString;
begin
  InternalSetToRecord(ActiveBuffer());

  PExternRecordBuffer(ActiveBuffer())^.InternRecordBuffer := AllocInternRecordBuffer();

  if (Filtered) then
    Inc(InternRecordBuffers.FilteredRecordCount);

  for I := 0 to FieldCount - 1 do
    if (Fields[I].DefaultExpression <> '') then
    begin
      RBS := Connection.LibEncode(SQLUnescape(Fields[I].DefaultExpression));
      SetFieldData(Fields[I], @RBS[1], Length(RBS));
    end
    else if (Fields[I].Required and (Fields[I].DataType in BinaryDataTypes * TextDataTypes)) then
      SetFieldData(Fields[I], Pointer(1), 0);
end;

procedure TMySQLDataSet.InternalLast();
begin
  InternRecordBuffers.Index := InternRecordBuffers.Count;
end;

procedure TMySQLDataSet.InternalOpen();
begin
  Assert(not IsCursorOpen());

  FCursorOpen := True;

  RecordsReceived.ResetEvent();

  inherited;

  SetFieldsSortTag();
end;

procedure TMySQLDataSet.InternalPost();
var
  I: Integer;
  SQL: string;
  Success: Boolean;
  Update: Boolean;
begin
  if (CachedUpdates) then
    Success := True
  else
  begin
    Update := PExternRecordBuffer(ActiveBuffer())^.BookmarkFlag = bfCurrent;

    if (Update) then
      SQL := SQLUpdate()
    else
      SQL := SQLInsert();

    if (SQL = '') then
      Success := False
    else
    begin
      if (Connection.DatabaseName <> DatabaseName) then
        SQL := Connection.SQLUse(DatabaseName) + SQL;
      Connection.BeginSilent();
      try
        Success := Connection.ExecuteSQL(SQL);
        if (not Success) then
          raise EMySQLError.Create(Connection.ErrorMessage, Connection.ErrorCode, Connection)
        else if (Update and (Connection.RowsAffected = 0)) then
          raise EDatabasePostError.Create(SRecordChanged);
      finally
        Connection.EndSilent();
      end;
    end;
  end;

  if (Success) then
  begin
    case (PExternRecordBuffer(ActiveBuffer())^.BookmarkFlag) of
      bfInserted:
        InternRecordBuffers.Insert(InternRecordBuffers.Index, PExternRecordBuffer(ActiveBuffer())^.InternRecordBuffer);
      bfBOF,
      bfEOF:
        InternRecordBuffers.Index := InternRecordBuffers.Add(PExternRecordBuffer(ActiveBuffer())^.InternRecordBuffer);
    end;

    if (Connection.Connected) then
      for I := 0 to Fields.Count - 1 do
      begin
        if ((Fields[I].AutoGenerateValue = arAutoInc) and (Fields[I].IsNull or (Fields[I].AsLargeInt = 0)) and (Connection.InsertId > 0)) then
          Fields[I].AsLargeInt := Connection.InsertId;
        if (Fields[I].Required and Fields[I].IsNull and (Fields[I].DefaultExpression <> '')) then
          Fields[I].AsString := SQLUnescape(Fields[I].DefaultExpression);
    end;

    if (PExternRecordBuffer(ActiveBuffer())^.InternRecordBuffer^.OldData <> PExternRecordBuffer(ActiveBuffer())^.InternRecordBuffer^.NewData) then
      FreeMem(PExternRecordBuffer(ActiveBuffer())^.InternRecordBuffer^.OldData);
    PExternRecordBuffer(ActiveBuffer())^.InternRecordBuffer^.OldData := PExternRecordBuffer(ActiveBuffer())^.InternRecordBuffer^.NewData;
  end;
end;

procedure TMySQLDataSet.InternalRefresh();
var
  SQL: string;
begin
  if (Assigned(SyncThread)) then
  begin
    Connection.Terminate();
    SyncThread := nil;
  end;

  InternRecordBuffers.Clear();
  ActivateBuffers();

  RecordsReceived.ResetEvent();

  if (CommandType = ctQuery) then
    SQL := CommandText
  else
    SQL := SQLSelect();
  if (Connection.InternExecuteSQL(smDataSet, True, SQL)) then
    Connection.SyncBindDataSet(Self);
end;

procedure TMySQLDataSet.InternalSetToRecord(Buffer: TRecordBuffer);
var
  MaxIndex: Integer;
  NewIndex: Integer;
  NewInternBuffersIndex: Integer;
begin
  NewInternBuffersIndex := InternRecordBuffers.Index;

  NewIndex := Max(InternRecordBuffers.Index - BufferCount, 0);
  MaxIndex := Min(InternRecordBuffers.Index + BufferCount, RecordCount);
  while ((NewInternBuffersIndex = InternRecordBuffers.Index) and (NewIndex < MaxIndex)) do
  begin
    if (PExternRecordBuffer(Buffer)^.InternRecordBuffer = InternRecordBuffers[NewIndex]) then
    begin
      NewInternBuffersIndex := NewIndex;
      break;
    end;
    Inc(NewIndex);
  end;

  InternRecordBuffers.Index := NewInternBuffersIndex;
end;

function TMySQLDataSet.IsCursorOpen(): Boolean;
begin
  Result := FCursorOpen;
end;

function TMySQLDataSet.Locate(const KeyFields: string; const KeyValues: Variant;
  Options: TLocateOptions): Boolean;
var
  Bookmark: TBookmark;
  FieldNames: TCSVStrings;
  Fields: array of TField;
  FmtStrs: array of string;
  I: Integer;
  Index: Integer;
  L: Largeint;
  S: string;
  Values: array of string;
begin
  CheckBrowseMode();

  SetLength(FieldNames, 0);
  CSVSplitValues(KeyFields, ';', #0, FieldNames);
  SetLength(Fields, 0);
  for I := 0 to Length(FieldNames) - 1 do
    if (Assigned(FindField(FieldNames[I]))) then
    begin
      SetLength(Fields, Length(Fields) + 1);
      Fields[Length(Fields) - 1] := FieldByName(FieldNames[I]);
    end;

  SetLength(FmtStrs, Length(Fields));
  for I := 0 to Length(Fields) - 1 do
    if (Fields[I] is TMySQLBitField) then
    begin
      FmtStrs[I] := TMySQLBitField(Fields[I]).DisplayFormat;
      while ((Length(FmtStrs[I]) > 0) and (FmtStrs[I][1] = '#')) do System.Delete(FmtStrs[I], 1, 1);
    end;

  if (not VarIsArray(KeyValues)) then
  begin
    SetLength(Values, 1);
    Values[0] := KeyValues;
  end
  else
  begin
    SetLength(Values, Length(Fields));
    try
      for I := 0 to Length(Fields) - 1 do
        Values[I] := KeyValues[I];
    except
      SetLength(Values, 0);
    end;
  end;
  if ((loCaseInsensitive in Options) and (loPartialKey in Options)) then
    for I := 0 to Length(Values) - 1 do
      Values[I] := LowerCase(Values[I]);

  Result := (Length(Fields) = Length(FieldNames)) and (Length(Fields) = Length(Values));

  if (Result) then
  begin
    if (not LocateNext) then
      Index := 0
    else
      Index := RecNo + 1;

    Result := False;
    while (not Result and (Index < InternRecordBuffers.Count)) do
    begin
      Result := True;
      for I := 0 to Length(Fields) - 1 do
        if (not Assigned(InternRecordBuffers[Index]^.NewData^.LibRow^[I])) then
          Result := False
        else if (Fields[I] is TMySQLBitField) then
        begin
          L := 0;
          MoveMemory(@L, InternRecordBuffers[Index]^.NewData^.LibRow^[Fields[I].FieldNo - 1], InternRecordBuffers[Index]^.NewData^.LibLengths^[Fields[I].FieldNo - 1]);
          S := IntToBitString(L, Length(FmtStrs[I]));
          if (loPartialKey in Options) then
            Result := Result and (Pos(Values[I], S) > 0)
          else
            Result := Result and (Values[I] = S)
        end
        else if (loCaseInsensitive in Options) then
          if (loPartialKey in Options) then
            Result := Result and (Pos(Values[I], LowerCase(Connection.LibDecode(InternRecordBuffers[Index]^.NewData^.LibRow^[Fields[I].FieldNo - 1], InternRecordBuffers[Index]^.NewData^.LibLengths^[Fields[I].FieldNo - 1]))) > 0)
          else
            Result := Result and (lstrcmpi(PChar(Values[I]), PChar(Connection.LibDecode(InternRecordBuffers[Index]^.NewData^.LibRow^[Fields[I].FieldNo - 1], InternRecordBuffers[Index]^.NewData^.LibLengths^[Fields[I].FieldNo - 1]))) = 0)
        else
          if (loPartialKey in Options) then
            Result := Result and (Pos(Values[I], Connection.LibDecode(InternRecordBuffers[Index]^.NewData^.LibRow^[I], InternRecordBuffers[Fields[I].FieldNo - 1]^.NewData^.LibLengths^[Fields[I].FieldNo - 1])) > 0)
          else
            Result := Result and (lstrcmp(PChar(Values[I]), PChar(Connection.LibDecode(InternRecordBuffers[Index]^.NewData^.LibRow^[Fields[I].FieldNo - 1], InternRecordBuffers[Index]^.NewData^.LibLengths^[Fields[I].FieldNo - 1]))) = 0);

      if (not Result) then
        Inc(Index);
    end;

    if (Result) then
    begin
      CheckBrowseMode();
      SetLength(Bookmark, BookmarkSize);
      PPointer(@Bookmark[0])^ := InternRecordBuffers[Index];
      GotoBookmark(Bookmark);
      SetLength(Bookmark, 0);
    end;
  end;

  SetLength(Values, 0);
  SetLength(FieldNames, 0);
end;

function TMySQLDataSet.MoveRecordBufferData(var DestData: TMySQLQuery.PRecordBufferData; const SourceData: TMySQLQuery.PRecordBufferData): Boolean;
var
  I: Integer;
  Index: Integer;
  MemSize: Integer;
begin
  Assert(Assigned(SourceData));
  Assert(Assigned(SourceData^.LibLengths));


  if (Assigned(DestData)) then
    FreeMem(DestData);

  MemSize := SizeOf(DestData^) + FieldCount * (SizeOf(DestData^.LibLengths^[0]) + SizeOf(DestData^.LibRow^[0]));
  for I := 0 to FieldCount - 1 do
    Inc(MemSize, SourceData^.LibLengths^[I]);
  try
    GetMem(DestData, MemSize);
  except
    DestData := nil;
  end;

  Result := Assigned(DestData);
  if (Result) then
  begin
    DestData^.LibLengths := Pointer(@PAnsiChar(DestData)[SizeOf(DestData^)]);
    DestData^.LibRow := Pointer(@PAnsiChar(DestData)[SizeOf(DestData^) + FieldCount * SizeOf(DestData^.LibLengths^[0])]);

    MoveMemory(DestData^.LibLengths, SourceData^.LibLengths, FieldCount * SizeOf(DestData^.LibLengths^[0]));
    Index := SizeOf(DestData^) + FieldCount * (SizeOf(DestData^.LibLengths^[0]) + SizeOf(DestData^.LibRow^[0]));
    for I := 0 to FieldCount - 1 do
      if (not Assigned(SourceData^.LibRow^[I])) then
        DestData^.LibRow^[I] := nil
      else
      begin
        DestData^.LibRow^[I] := @PAnsiChar(DestData)[Index];
        MoveMemory(DestData^.LibRow^[I], SourceData^.LibRow^[I], DestData^.LibLengths^[I]);
        Inc(Index, DestData^.LibLengths^[I]);
      end;
  end;
end;

procedure TMySQLDataSet.Resync(Mode: TResyncMode);
begin
  // Why is this needed in Delphi XE2? Without this, Buffers are not reinitialized well.
  if ((InternRecordBuffers.Index >= 0) and (PExternRecordBuffer(ActiveBuffer())^.Index >= 0)
    and Assigned(PExternRecordBuffer(ActiveBuffer())^.InternRecordBuffer)
    and (Mode = [])) then
    InternRecordBuffers.Index := PExternRecordBuffer(ActiveBuffer())^.Index;

  inherited;
end;

procedure TMySQLDataSet.SetBookmarkData(Buffer: TRecordBuffer; Data: Pointer);
begin
  PExternRecordBuffer(Buffer)^.InternRecordBuffer := PPointer(Data)^;
end;

procedure TMySQLDataSet.SetBookmarkFlag(Buffer: TRecordBuffer; Value: TBookmarkFlag);
begin
  PExternRecordBuffer(Buffer)^.BookmarkFlag := Value;
end;

procedure TMySQLDataSet.SetFieldData(Field: TField; Buffer: Pointer);
var
  DT: TDateTime;
  Len: Integer;
  RBS: RawByteString;
  U: UInt64;
begin
  if (not Assigned(Buffer)) then
    SetFieldData(Field, nil, 0)
  else if (BitField(Field)) then
  begin
    ZeroMemory(@U, SizeOf(U));
    MoveMemory(@U, Buffer, Field.DataSize);
    U := SwapUInt64(U);
    Len := SizeOf(U);
    while ((Len > 0) and (PAnsiChar(@U)[SizeOf(U) - Len] = #0)) do Dec(Len);
    SetFieldData(Field, @PAnsiChar(@U)[SizeOf(U) - Len], Len)
  end
  else
  begin
    case (Field.DataType) of
      ftString: SetString(RBS, PAnsiChar(Buffer), Field.DataSize);
      ftShortInt: RBS := Connection.LibPack(FormatFloat(TNumericField(Field).DisplayFormat, ShortInt(Buffer^), Connection.FormatSettings));
      ftByte: RBS := Connection.LibPack(FormatFloat(TNumericField(Field).DisplayFormat, Byte(Buffer^), Connection.FormatSettings));
      ftSmallInt: RBS := Connection.LibPack(FormatFloat(TNumericField(Field).DisplayFormat, SmallInt(Buffer^), Connection.FormatSettings));
      ftWord: RBS := Connection.LibPack(FormatFloat(TNumericField(Field).DisplayFormat, Word(Buffer^), Connection.FormatSettings));
      ftInteger: RBS := Connection.LibPack(FormatFloat(TNumericField(Field).DisplayFormat, Integer(Buffer^), Connection.FormatSettings));
      ftLongWord: RBS := Connection.LibPack(FormatFloat(TNumericField(Field).DisplayFormat, LongWord(Buffer^), Connection.FormatSettings));
      ftLargeint:
        if (not (Field is TMySQLLargeWordField) or (UInt64(Buffer^) and $80000000 = 0)) then
          RBS := Connection.LibPack(FormatFloat(TNumericField(Field).DisplayFormat, Largeint(Buffer^), Connection.FormatSettings))
        else
          RBS := Connection.LibPack(UInt64ToStr(UInt64(Buffer^)));
      ftSingle: RBS := Connection.LibPack(FormatFloat(TNumericField(Field).DisplayFormat, Single(Buffer^), Connection.FormatSettings));
      ftFloat: RBS := Connection.LibPack(FormatFloat(TNumericField(Field).DisplayFormat, Double(Buffer^), Connection.FormatSettings));
      ftExtended: RBS := Connection.LibPack(FormatFloat(TNumericField(Field).DisplayFormat, Extended(Buffer^), Connection.FormatSettings));
      ftDate: begin DataConvert(Field, Buffer, @DT, False); RBS := Connection.LibPack(MySQLDB.DateToStr(DT, Connection.FormatSettings)); end;
      ftTime: RBS := Connection.LibPack(TimeToStr(Integer(Buffer^), TMySQLTimeField(Field).SQLFormat));
      ftTimeStamp: RBS := Connection.LibPack(MySQLTimeStampToStr(PSQLTimeStamp(Buffer)^, TMySQLTimeStampField(Field).DisplayFormat));
      ftDateTime: begin DataConvert(Field, Buffer, @DT, False); RBS := Connection.LibPack(MySQLDB.DateTimeToStr(DT, Connection.FormatSettings)); end;
      ftBlob: begin SetLength(RBS, TMemoryStream(Buffer).Size); Move(TMemoryStream(Buffer).Memory^, PAnsiChar(RBS)^, TMemoryStream(Buffer).Size); end;
      ftWideMemo: DataConvert(Field, Buffer, @RBS, True);
      ftWideString: RBS := PAnsiChar(Buffer);
      else raise EDatabaseError.CreateFMT(SUnknownFieldType + '(%d)', [Field.Name, Integer(Field.DataType)]);
    end;
    if (RBS = '') then
      SetFieldData(Field, Pointer(-1), 0)
    else
      SetFieldData(Field, PAnsiChar(RBS), Length(RBS));
  end;

  DataEvent(deFieldChange, Longint(Field));
end;

procedure TMySQLDataSet.SetFieldData(const Field: TField; const Buffer: Pointer; const Size: Integer);
var
  I: Integer;
  Index: Integer;
  MemSize: Integer;
  NewData: TMySQLQuery.PRecordBufferData;
  OldData: TMySQLQuery.PRecordBufferData;
begin
  OldData := PExternRecordBuffer(ActiveBuffer())^.InternRecordBuffer^.NewData;

  MemSize := SizeOf(NewData^) + FieldCount * (SizeOf(NewData^.LibLengths^[0]) + SizeOf(NewData^.LibRow^[0]));
  for I := 0 to FieldCount - 1 do
    if (I = Field.FieldNo - 1) then
      if (not Assigned(Buffer)) then
        // no data
      else
        Inc(MemSize, Size)
    else if (Assigned(OldData)) then
      Inc(MemSize, OldData^.LibLengths^[I]);

  GetMem(NewData, MemSize);

  NewData^.LibLengths := Pointer(@PAnsiChar(NewData)[SizeOf(NewData^)]);
  NewData^.LibRow := Pointer(@PAnsiChar(NewData)[SizeOf(NewData^) + FieldCount * SizeOf(NewData^.LibLengths^[0])]);

  Index := SizeOf(NewData^) + FieldCount * (SizeOf(NewData^.LibLengths^[0]) + SizeOf(NewData^.LibRow^[0]));
  for I := 0 to FieldCount - 1 do
  begin
    if (I = Field.FieldNo - 1) then
      if (not Assigned(Buffer)) then
      begin
        NewData^.LibLengths^[I] := 0;
        NewData^.LibRow^[I] := nil;
      end
      else
      begin
        NewData^.LibLengths^[I] := Size;
        NewData^.LibRow^[I] := Pointer(@PAnsiChar(NewData)[Index]);
        MoveMemory(NewData^.LibRow^[I], Buffer, Size);
      end
    else
      if (not Assigned(OldData) or not Assigned(OldData^.LibRow^[I])) then
      begin
        NewData^.LibLengths^[I] := 0;
        NewData^.LibRow^[I] := nil;
      end
      else
      begin
        NewData^.LibLengths^[I] := OldData^.LibLengths^[I];
        NewData^.LibRow^[I] := Pointer(@PAnsiChar(NewData)[Index]);
        MoveMemory(NewData^.LibRow^[I], OldData^.LibRow^[I], OldData^.LibLengths^[I]);
      end;
    Inc(Index, NewData^.LibLengths^[I]);
  end;

  if (not Assigned(PExternRecordBuffer(ActiveBuffer())^.InternRecordBuffer)) then
    PExternRecordBuffer(ActiveBuffer())^.InternRecordBuffer := AllocInternRecordBuffer()
  else if (PExternRecordBuffer(ActiveBuffer())^.InternRecordBuffer^.NewData <> PExternRecordBuffer(ActiveBuffer())^.InternRecordBuffer^.OldData) then
    FreeMem(PExternRecordBuffer(ActiveBuffer())^.InternRecordBuffer^.NewData);
  PExternRecordBuffer(ActiveBuffer())^.InternRecordBuffer^.NewData := NewData;
end;

procedure TMySQLDataSet.SetFieldsSortTag();
var
  Field: TField;
  FieldName: string;
  Pos: Integer;
begin
  for Field in Fields do
    Field.Tag := Field.Tag and not ftSortedField;
  Pos := 1;
  repeat
    FieldName := ExtractFieldName(SortDef.Fields, Pos);
    if (FieldName <> '') then
    begin
      Field := FindField(FieldName);
      if (Assigned(Field)) then
        Field.Tag := Field.Tag or ftAscSortedField;
    end;
  until (FieldName = '');
  Pos := 1;
  repeat
    FieldName := ExtractFieldName(SortDef.DescFields, Pos);
    if (FieldName <> '') then
    begin
      Field := FindField(FieldName);
      if (Assigned(Field)) then
        Field.Tag := (Field.Tag and (not ftAscSortedField) or ftDescSortedField);
    end;
  until (FieldName = '');
end;

procedure TMySQLDataSet.SetFiltered(Value: Boolean);
begin
  if (Value <> Filtered) then
  begin
    inherited;

    if (Active) then
      if (Value) then
        ActivateFilter()
      else
        DeactivateFilter();
  end;
end;

procedure TMySQLDataSet.SetFilterText(const Value: string);
begin
  Filtered := Filtered and (Value <> '');

  if (Filtered) then
    DeactivateFilter();

  inherited;

  if (Filtered and (Filter <> '') and IsCursorOpen()) then
    ActivateFilter();
end;

procedure TMySQLDataSet.SetRecNo(Value: Integer);
var
  Bookmark: TBookmark;
  Index: Integer;
  VisibleRecords: Integer;
begin
  VisibleRecords := 0;

  Index := 0;
  while ((VisibleRecords < Value + 1) and (Index < InternRecordBuffers.Count)) do
  begin
    if (not Filtered or InternRecordBuffers[Index]^.VisibleInFilter) then
      Inc(VisibleRecords);
    Inc(Index);
  end;

  if ((VisibleRecords = Value + 1) and (Index < InternRecordBuffers.Count)) then
  begin
    SetLength(Bookmark, BookmarkSize);
    PPointer(@Bookmark[0])^ := InternRecordBuffers[Index - 1];
    GotoBookmark(Bookmark);
    SetLength(Bookmark, 0);
  end;
end;

procedure TMySQLDataSet.Sort(const ASortDef: TIndexDef);
var
  Ascending: array of Boolean;
  SortFields: array of TField;

  function Compare(const A, B: PInternRecordBuffer; const FieldIndex: Integer = 0): Integer;
  var
    Field: TField;
    ShortIntA, ShortIntB: ShortInt;
    ByteA, ByteB: Byte;
    SmallIntA, SmallIntB: SmallInt;
    WordA, WordB: Word;
    IntegerA, IntegerB: Integer;
    LongWordA, LongWordB: LongWord;
    LargeIntA, LargeIntB: LargeInt;
    UInt64A, UInt64B: UInt64;
    SingleA, SingleB: Single;
    DoubleA, DoubleB: Double;
    ExtendedA, ExtendedB: Extended;
    DateTimeA, DateTimeB: TDateTimeRec;
    StringA, StringB: string;
  begin
    Field := SortFields[FieldIndex];

    if (A = B) then
      Result := 0
    else if (not Assigned(A^.NewData^.LibRow[Field.FieldNo - 1]) and Assigned(B^.NewData^.LibRow[Field.FieldNo - 1])) then
      Result := -1
    else if (Assigned(A^.NewData^.LibRow[Field.FieldNo - 1]) and not Assigned(B^.NewData^.LibRow[Field.FieldNo - 1])) then
      Result := +1
    else if ((A^.NewData^.LibLengths[Field.FieldNo - 1] = 0) and (B^.NewData^.LibLengths[Field.FieldNo - 1] > 0)) then
      Result := -1
    else if ((A^.NewData^.LibLengths[Field.FieldNo - 1] > 0) and (B^.NewData^.LibLengths[Field.FieldNo - 1] = 0)) then
      Result := +1
    else if ((A^.NewData^.LibLengths[Field.FieldNo - 1] = 0) and (B^.NewData^.LibLengths[Field.FieldNo - 1] = 0)) then
      Result := 0
    else
    begin
      case (Field.DataType) of
        ftString: Result := lstrcmpA(A^.NewData^.LibRow^[Field.FieldNo - 1], B^.NewData^.LibRow^[Field.FieldNo - 1]);
        ftShortInt: begin GetFieldData(Field, @ShortIntA, A^.NewData); GetFieldData(Field, @ShortIntB, B^.NewData); Result := Sign(ShortIntA - ShortIntB); end;
        ftByte:
          begin
            GetFieldData(Field, @ByteA, A^.NewData);
            GetFieldData(Field, @ByteB, B^.NewData);
            if (ByteA < ByteB) then Result := -1 else if (ByteA > ByteB) then Result := +1 else Result := 0;
          end;
        ftSmallInt: begin GetFieldData(Field, @SmallIntA, A^.NewData); GetFieldData(Field, @SmallIntB, B^.NewData); Result := Sign(SmallIntA - SmallIntB); end;
        ftWord:
          begin
            GetFieldData(Field, @WordA, A^.NewData);
            GetFieldData(Field, @WordB, B^.NewData);
            if (WordA < WordB) then Result := -1 else if (WordA > WordB) then Result := +1 else Result := 0;
          end;
        ftInteger: begin GetFieldData(Field, @IntegerA, A^.NewData); GetFieldData(Field, @IntegerB, B^.NewData); Result := Sign(IntegerA - IntegerB); end;
        ftLongWord:
          begin
            GetFieldData(Field, @LongWordA, A^.NewData);
            GetFieldData(Field, @LongWordB, B^.NewData);
            if (LongWordA < LongWordB) then Result := -1 else if (LongWordA > LongWordB) then Result := +1 else Result := 0;
          end;
        ftLargeInt:
          if (not (Field is TMySQLLargeWordField)) then
            begin GetFieldData(Field, @LargeIntA, A^.NewData); GetFieldData(Field, @LargeIntB, B^.NewData); Result := Sign(LargeIntA - LargeIntB); end
          else
          begin
            GetFieldData(Field, @UInt64A, A^.NewData);
            GetFieldData(Field, @UInt64B, B^.NewData);
            if (UInt64A < UInt64B) then Result := -1 else if (UInt64A > UInt64B) then Result := +1 else Result := 0;
          end;
        ftSingle: begin GetFieldData(Field, @SingleA, A^.NewData); GetFieldData(Field, @SingleB, B^.NewData); Result := Sign(SingleA - SingleB); end;
        ftFloat: begin GetFieldData(Field, @DoubleA, A^.NewData); GetFieldData(Field, @DoubleB, B^.NewData); Result := Sign(DoubleA - DoubleB); end;
        ftExtended: begin GetFieldData(Field, @ExtendedA, A^.NewData); GetFieldData(Field, @ExtendedB, B^.NewData); Result := Sign(ExtendedA - ExtendedB); end;
        ftDate: begin GetFieldData(Field, @DateTimeA, A^.NewData); GetFieldData(Field, @DateTimeB, B^.NewData); Result := Sign(DateTimeA.Date - DateTimeB.Date); end;
        ftDateTime: begin GetFieldData(Field, @DateTimeA, A^.NewData); GetFieldData(Field, @DateTimeB, B^.NewData); Result := Sign(DateTimeA.DateTime - DateTimeB.DateTime); end;
        ftTime: begin GetFieldData(Field, @IntegerA, A^.NewData); GetFieldData(Field, @IntegerB, B^.NewData); Result := Sign(IntegerA - IntegerB); end;
        ftTimeStamp: Result := lstrcmpA(A^.NewData^.LibRow^[Field.FieldNo - 1], B^.NewData^.LibRow^[Field.FieldNo - 1]);
        ftWideString,
        ftWideMemo:
          begin
            StringA := Connection.LibDecode(A^.NewData^.LibRow^[Field.FieldNo - 1], A^.NewData^.LibLengths^[Field.FieldNo - 1]);
            StringB := Connection.LibDecode(B^.NewData^.LibRow^[Field.FieldNo - 1], B^.NewData^.LibLengths^[Field.FieldNo - 1]);
            Result := lstrcmpi(PChar(StringA), PChar(StringB));
          end;
        ftBlob: Result := lstrcmpA(A^.NewData^.LibRow^[Field.FieldNo - 1], B^.NewData^.LibRow^[Field.FieldNo - 1]);
        else
          raise EDatabaseError.CreateFMT(SUnknownFieldType + '(%d)', [Field.Name, Integer(Field.DataType)]);
      end;
    end;

    if (Result = 0) then
    begin
      if (FieldIndex + 1 < Length(SortFields)) then
        Result := Compare(A, B, FieldIndex + 1);
    end
    else if (not Ascending[FieldIndex]) then
      Result := -Result;
  end;

  procedure QuickSort(const Lo, Hi: Integer);
  var
    L, R: Integer;
    M: PInternRecordBuffer;
  begin
    L := Lo;
    R := Hi;
    M := InternRecordBuffers[(L + R + 1) div 2];

    while (L <= R) do
    begin
      while (Compare(InternRecordBuffers[L], M) < 0) do Inc(L);
      while (Compare(InternRecordBuffers[R], M) > 0) do Dec(R);
      if (L <= R) then
      begin
        InternRecordBuffers.Exchange(L, R);
        Inc(L); Dec(R);
      end;
    end;

    if (Lo < R) then QuickSort(Lo, R);
    if (L < Hi) then QuickSort(L, Hi);
  end;

var
  FieldName: string;
  I: Integer;
  OldBookmark: TBookmark;
  Pos: Integer;
begin
  if (Assigned(SyncThread)) then
    Connection.Terminate();

  CheckBrowseMode();
  DoBeforeScroll();

  SortDef.Assign(ASortDef);

  OldBookmark := Bookmark;

  if ((ASortDef.Fields <> '') and (InternRecordBuffers.Count > 0)) then
  begin
    SetLength(SortFields, 0);
    SetLength(Ascending, 0);
    Pos := 1;
    repeat
      FieldName := ExtractFieldName(SortDef.Fields, Pos);
      if (FieldName <> '') then
      begin
        SetLength(SortFields, Length(SortFields) + 1);
        SortFields[Length(SortFields) - 1] := FieldByName(FieldName);
        SetLength(Ascending, Length(Ascending) + 1);
        Ascending[Length(SortFields) - 1] := True;
      end;
    until (FieldName = '');
    Pos := 1;
    repeat
      FieldName := ExtractFieldName(SortDef.DescFields, Pos);
      if (FieldName <> '') then
        for I := 0 to Length(SortFields) - 1 do
          if (SortFields[I].FieldName = FieldName) then
            Ascending[I] := False;
    until (FieldName = '');

    QuickSort(0, InternRecordBuffers.Count - 1);
  end;

  SetFieldsSortTag();

  Bookmark := OldBookmark;

  DoAfterScroll();
end;

function TMySQLDataSet.SQLDelete(): string;
var
  I: Integer;
  InternRecordBuffer: PInternRecordBuffer;
  J: Integer;
  NullValue: Boolean;
  ValueHandled: Boolean;
  Values: string;
  WhereField: TField;
  WhereFieldCount: Integer;
begin
  Result := 'DELETE FROM ' + SQLTableClause() + ' WHERE ';

  if (Length(DeleteBookmarks) = 0) then
  begin
    InternRecordBuffer := PExternRecordBuffer(ActiveBuffer())^.InternRecordBuffer;

    ValueHandled := False;
    for I := 0 to Fields.Count - 1 do
      if (pfInWhere in Fields[I].ProviderFlags) then
      begin
        if (ValueHandled) then Result := Result + ' AND ';
        if (not Assigned(InternRecordBuffer^.OldData) or not Assigned(InternRecordBuffer^.OldData^.LibRow^[Fields[I].FieldNo - 1])) then
          Result := Result + Connection.EscapeIdentifier(Fields[I].FieldName) + ' IS NULL'
        else
          Result := Result + Connection.EscapeIdentifier(Fields[I].FieldName) + '=' + SQLFieldValue(Fields[I], InternRecordBuffer^.OldData);
        ValueHandled := True;
      end;
  end
  else
  begin
    WhereFieldCount := 0; WhereField := nil;
    for I := 0 to FieldCount - 1 do
      if (pfInWhere in Fields[I].ProviderFlags) then
      begin
        WhereField := Fields[I];
        Inc(WhereFieldCount);
      end;

    if (WhereFieldCount = 1) then
    begin
      Values := ''; NullValue := False;
      for I := 0 to Length(DeleteBookmarks) - 1 do
      begin
        InternRecordBuffer := InternRecordBuffers[BookmarkToInternBufferIndex(TBookmark(DeleteBookmarks[I]))];
        if (not Assigned(InternRecordBuffer^.OldData) or not Assigned(InternRecordBuffer^.OldData^.LibRow^[WhereField.FieldNo - 1])) then
          NullValue := True
        else
        begin
          if (Values <> '') then Values := Values + ',';
          Values := Values + SQLFieldValue(WhereField, InternRecordBuffer^.OldData);
        end;
      end;

      if (Values <> '') then
        Result := Result + Connection.EscapeIdentifier(WhereField.FieldName) + ' IN (' + Values + ')';
      if (NullValue) then
      begin
        if (Values <> '') then
          Result := Result + ' OR ';
        Result := Result + Connection.EscapeIdentifier(WhereField.FieldName) + ' IS NULL';
      end;
    end
    else
    begin
      for I := 0 to Length(DeleteBookmarks) - 1 do
      begin
        if (I > 0) then Result := Result + ' OR ';
        Result := Result + '(';
        ValueHandled := False;
        for J := 0 to FieldCount - 1 do
          if (pfInWhere in Fields[J].ProviderFlags) then
          begin
            InternRecordBuffer := InternRecordBuffers[BookmarkToInternBufferIndex(TBookmark(DeleteBookmarks[I]))];
            if (ValueHandled) then Result := Result + ' AND ';
            if (not Assigned(InternRecordBuffer^.OldData^.LibRow^[Fields[J].FieldNo - 1])) then
              Result := Result + Connection.EscapeIdentifier(Fields[J].FieldName) + ' IS NULL'
            else
              Result := Result + '(' + Connection.EscapeIdentifier(Fields[J].FieldName) + '=' + SQLFieldValue(Fields[J], InternRecordBuffer^.OldData) + ')';
            ValueHandled := True;
          end;
        Result := Result + ')';
      end;
    end;
  end;

  Result := Result + ';' + #13#10;
end;

function TMySQLDataSet.SQLFieldValue(const Field: TField; Buffer: TRecordBuffer = nil): string;
begin
  if (not Assigned(Buffer)) then
    Buffer := ActiveBuffer();

  Result := SQLFieldValue(Field, PExternRecordBuffer(Buffer)^.InternRecordBuffer^.NewData);
end;

function TMySQLDataSet.SQLInsert(): string;
var
  ExternRecordBuffer: PExternRecordBuffer;
  I: Integer;
  ValueHandled: Boolean;
begin
  ExternRecordBuffer := PExternRecordBuffer(ActiveBuffer());

  if (not Assigned(ExternRecordBuffer^.InternRecordBuffer^.NewData)) then
    Result := ''
  else
  begin
    Result := 'INSERT INTO ' + SQLTableClause() + ' SET ';
    ValueHandled := False;
    for I := 0 to FieldCount - 1 do
      if (Assigned(ExternRecordBuffer^.InternRecordBuffer^.NewData^.LibRow^[Fields[I].FieldNo - 1])) then
      begin
        if (ValueHandled) then Result := Result + ',';
        Result := Result + Connection.EscapeIdentifier(Fields[I].FieldName) + '=' + SQLFieldValue(Fields[I], TRecordBuffer(ExternRecordBuffer));
        ValueHandled := True;
      end;
    Result := Result + ';' + #13#10;
  end;
end;

function TMySQLDataSet.SQLTableClause(): string;
begin
  if (DatabaseName = '') then
    Result := ''
  else
    Result := Connection.EscapeIdentifier(DatabaseName) + '.';
  Result := Result + Connection.EscapeIdentifier(TableName);
end;

function TMySQLDataSet.SQLUpdate(Buffer: TRecordBuffer = nil): string;
var
  I: Integer;
  ValueHandled: Boolean;
begin
  if (not Assigned(Buffer)) then
    Buffer := ActiveBuffer();

  Result := '';
  ValueHandled := False;
  for I := 0 to FieldCount - 1 do
    if ((PExternRecordBuffer(Buffer)^.InternRecordBuffer^.NewData^.LibLengths^[Fields[I].FieldNo - 1] <> PExternRecordBuffer(Buffer)^.InternRecordBuffer^.OldData^.LibLengths^[Fields[I].FieldNo - 1])
      or (Assigned(PExternRecordBuffer(Buffer)^.InternRecordBuffer^.NewData^.LibRow^[Fields[I].FieldNo - 1]) xor Assigned(PExternRecordBuffer(Buffer)^.InternRecordBuffer^.OldData^.LibRow^[Fields[I].FieldNo - 1]))
      or (not CompareMem(PExternRecordBuffer(Buffer)^.InternRecordBuffer^.NewData^.LibRow^[Fields[I].FieldNo - 1], PExternRecordBuffer(Buffer)^.InternRecordBuffer^.OldData^.LibRow^[Fields[I].FieldNo - 1], PExternRecordBuffer(Buffer)^.InternRecordBuffer^.OldData^.LibLengths^[Fields[I].FieldNo - 1]))) then
    begin
      if (ValueHandled) then Result := Result + ',';
      Result := Result + Connection.EscapeIdentifier(Fields[I].FieldName) + '=' + SQLFieldValue(Fields[I], Buffer);
      ValueHandled := True;
    end;
  if (Result <> '') then
  begin
    Result := 'UPDATE ' + SQLTableClause() + ' SET ' + Result + ' WHERE ';
    ValueHandled := False;
    for I := 0 to FieldCount - 1 do
      if (pfInWhere in Fields[I].ProviderFlags) then
      begin
        if (ValueHandled) then Result := Result + ' AND ';
        if (not Assigned(PExternRecordBuffer(Buffer)^.InternRecordBuffer^.OldData^.LibRow^[Fields[I].FieldNo - 1])) then
          Result := Result + Connection.EscapeIdentifier(Fields[I].FieldName) + ' IS NULL'
        else
          Result := Result + Connection.EscapeIdentifier(Fields[I].FieldName) + '=' + SQLFieldValue(Fields[I], PExternRecordBuffer(Buffer)^.InternRecordBuffer^.OldData);
        ValueHandled := True;
      end;
    Result := Result + ';' + #13#10;
  end;
end;

procedure TMySQLDataSet.UpdateIndexDefs();
var
  DName: string;
  FieldName: string;
  Found: Boolean;
  I: Integer;
  Index: TIndexDef;
  Parse: TSQLParse;
  Pos: Integer;
  TName: string;
begin
  if (not Assigned(Handle)) then
  begin
    inherited;

    if (not (Self is TMySQLTable)) then
    begin
      Index := nil;
      for I := 0 to FIndexDefs.Count - 1 do
        if (not Assigned(Index) and (ixUnique in FIndexDefs[I].Options)) then
          Index := FIndexDefs[I];
      FCanModify := Assigned(Index);

      if (not FCanModify and SQLCreateParse(Parse, PChar(CommandText), Length(CommandText), Connection.MySQLVersion) and SQLParseKeyword(Parse, 'SELECT') and SQLParseChar(Parse, '*') and SQLParseKeyword(Parse, 'FROM')) then
      begin
        TName := SQLParseValue(Parse);
        if (not SQLParseChar(Parse, '.')) then
          DName := DatabaseName
        else
        begin
          DName := TName;
          TName := SQLParseValue(Parse);
        end;
        if (((TName = TableName) or SQLParseKeyword(Parse, 'AS') and (SQLParseValue(Parse) = TableName))
          and ((SQLParseKeyword(Parse, 'FROM') or SQLParseKeyword(Parse, 'WHERE') or SQLParseKeyword(Parse, 'GROUP BY') or SQLParseKeyword(Parse, 'HAVING') or SQLParseKeyword(Parse, 'ORDER BY') or SQLParseKeyword(Parse, 'LIMIT') or SQLParseEnd(Parse)))) then
        begin
          FDatabaseName := DName;
          TableName := TName;
          FCanModify := True;

          Found := False;
          for I := 0 to FieldCount - 1 do
            Found := Found or (pfInKey in Fields[I].ProviderFlags);

          if (Found) then
          begin
            Index := FIndexDefs.AddIndexDef();
            Index.Name := '';
            Index.Options := [ixPrimary, ixUnique, ixCaseInsensitive];
            for I := 0 to FieldCount - 1 do
              if (pfInKey in Fields[I].ProviderFlags) then
              begin
                if (Index.Fields <> '') then Index.Fields := Index.Fields + ';';
                Index.Fields := Index.Fields + Fields[I].FieldName;
              end;
          end;
        end;
      end;
    end;

    Index := nil;
    for I := 0 to FIndexDefs.Count - 1 do
      if (not Assigned(Index) and (ixUnique in FIndexDefs[I].Options)) then
        Index := FIndexDefs[I];

    if (Assigned(Index)) then
    begin
      FCanModify := True;

      for I := 0 to FieldCount - 1 do
        Fields[I].ProviderFlags := Fields[I].ProviderFlags - [pfInWhere];
      Pos := 1;
      repeat
        FieldName := ExtractFieldName(Index.Fields, Pos);
        for I := 0 to FieldCount - 1 do
          if (Fields[I].FieldName = FieldName) then
            Fields[I].ProviderFlags := Fields[I].ProviderFlags + [pfInWhere];
      until (FieldName = '');
    end
    else
    begin
      for I := 0 to FieldCount - 1 do
        Fields[I].ProviderFlags := Fields[I].ProviderFlags + [pfInWhere];
    end;
  end;
end;

function TMySQLDataSet.VisibleInFilter(const InternRecordBuffer: PInternRecordBuffer): Boolean;

  type
    PCANExpr = ^TCANExpr;
    TCANExpr = packed record
      iVer: Word;
      iTotalSize: Word;
      iNodes: Word;
      iNodeStart: Word;
      iLiteralStart: Word;
    end;

    PCANHdr = ^TCANHdr;
    TCANHdr = packed record
      nodeClass: DBCommon.NODEClass;
      Reserved1: Byte;
      Reserved2: Word;
      coOp: DBCommon.TCANOperator;
      Reserved3: Byte;
      Reserved4: Word;
      case DBCommon.NODEClass of
        nodeUNARY: ( Unary: record
          NodeOfs: Word;
        end; );
        nodeBINARY: ( Binary: record
          LeftPosOfs: Word;
          RightPosOfs: Word;
        end; );
        nodeCOMPARE: ( Compare: record
          CaseInsensitive: WordBool;
          PartLength: Word;
          NodeOfs: Word;
          DataOfs: Word;
        end; );
        nodeFIELD: ( Field2: record
          FieldNo: Word;
          FieldNameOfs: Word;
        end; );
        nodeCONST: ( Const2: record
          FieldType: Word;
          Size: Word;
          DataOfs: Word;
        end; );
        nodeLIST: ( List: record
        end; );
        nodeFUNC: ( Func: record
          FunctionNameOfs: Word;
          ArgOfs: Word;
        end; );
        nodeLISTELEM: ( ListItem: record
        end; );
    end;

  var
    Expr: PCANExpr;

  function VIsNull(AVariant: Variant): Boolean;
  begin
    Result:= VarIsNull(AVariant) or VarIsEmpty(AVariant);
  end;

  function ParseNode(const Node: PCANHdr): Variant;
  type
    PLargeint = ^Largeint;
  var
    I, Z: Integer;
    Year, Month, Day, Hour, Min, Sec, MSec:word;
    P: Pointer;
    Arg1, Arg2: Variant;
    S: string;
    TS: TTimeStamp;
    TempNode: PCANHdr;
  begin
    case (Node^.nodeClass) of
      nodeFIELD:
        case (Node^.coOp) of
          coFIELD2:
            if (not Assigned(InternRecordBuffer^.NewData^.LibRow^[Node^.Field2.FieldNo - 1])) then
              Result := Null
            else if (BitField(Fields[Node^.Field2.FieldNo - 1])) then
              Result := Fields[Node^.Field2.FieldNo - 1].AsLargeInt
            else if (Fields[Node^.Field2.FieldNo - 1].DataType in [ftWideString, ftWideMemo]) then
              Result := Connection.LibDecode(InternRecordBuffer^.NewData^.LibRow^[Node^.Field2.FieldNo - 1], InternRecordBuffer^.NewData^.LibLengths^[Node^.Field2.FieldNo - 1])
            else
              Result := Connection.LibUnpack(InternRecordBuffer^.NewData^.LibRow^[Node^.Field2.FieldNo - 1], InternRecordBuffer^.NewData^.LibLengths^[Node^.Field2.FieldNo - 1]);
          else raise EDatabaseError.CreateFmt('coOp not supported (%d)', [Ord(Node^.coOp)]);
        end;

      nodeCONST:
        case (Node^.coOp) of
          coCONST2:
            begin
              P := @FilterParser.FilterData[Expr^.iLiteralStart + Node^.Const2.DataOfs];

              if (Node^.Const2.FieldType = $1007) then
              begin
                SetString(S, PChar(@PAnsiChar(P)[2]), PWord(@PAnsiChar(P)[0])^ div SizeOf(Char));
                Result := S;
              end
              else
                case (TFieldType(Node^.Const2.FieldType)) of
                  ftShortInt: Result := PShortInt(P)^;
                  ftByte: Result := PByte(P)^;
                  ftSmallInt: Result := PSmallInt(P)^;
                  ftWord: Result := PWord(P)^;
                  ftInteger: Result := PInteger(P)^;
                  ftLongword: Result := PLongword(P)^;
                  ftLargeint: Result := PLargeint(P)^;
                  ftSingle: Result := PSingle(P)^;
                  ftFloat: Result := PDouble(P)^;
                  ftExtended: Result := PExtended(P)^;
                  ftWideString: Result := PString(P)^;
                  ftDate: begin TS.Date := PInteger(P)^; TS.Time := 0; Result := TimeStampToDateTime(TS); end;
                  ftDateTime: Result := TimeStampToDateTime(MSecsToTimeStamp(PDouble(P)^));
                  ftTime: begin TS.Date := 0; TS.Time := PInteger(P)^; Result := TimeStampToDateTime(TS); end;
                  ftTimeStamp: Result := VarSQLTimeStampCreate(PSQLTimeStamp(P)^);
                  ftString,
                  ftFixedChar: Result := string(PChar(P));
                  ftBoolean: Result := PWordBool(P)^;
                  else raise EDatabaseError.CreateFmt('FieldType not supported (%d)', [Ord(Node^.Const2.FieldType)]);
                end;
            end;
          else raise EDatabaseError.CreateFmt('coOp not supported (%d)', [Ord(Node^.coOp)]);
        end;

      nodeUNARY:
        begin
          Arg1 := ParseNode(@FilterParser.FilterData[CANEXPRSIZE + Node^.Unary.NodeOfs]);

          case (Node^.coOp) of
            coISBLANK:
              Result := VIsNull(Arg1);
            coNOTBLANK:
              Result := not VIsNull(Arg1);
            coNOT:
              if (VIsNull(Arg1)) then Result := Null else Result := not Arg1;
            coMINUS:
              if (VIsNull(Arg1)) then Result := Null else Result := -Arg1;
            coUPPER:
              if (VIsNull(Arg1)) then Result := Null else Result := UpperCase(Arg1);
            coLOWER:
              if (VIsNull(Arg1)) then Result := Null else Result := LowerCase(Arg1);
            else raise EDatabaseError.CreateFmt('coOp not supported (%d)', [Ord(Node^.coOp)]);
          end;
        end;

      nodeBINARY:
        begin
          Arg1 := ParseNode(@FilterParser.FilterData[CANEXPRSIZE + Node^.Binary.LeftPosOfs]);
          Arg2 := ParseNode(@FilterParser.FilterData[CANEXPRSIZE + Node^.Binary.RightPosOfs]);

          case (Node^.coOp) of
            coEQ:
              if (VIsNull(Arg1) or VIsNull(Arg2)) then Result := False else Result := (Arg1 = Arg2);
            coNE:
              if (VIsNull(Arg1) or VIsNull(Arg2)) then Result := False else Result := (Arg1 <> Arg2);
            coGT:
              if (VIsNull(Arg1) or VIsNull(Arg2)) then Result := False else Result := (Arg1 > Arg2);
            coGE:
              if (VIsNull(Arg1) or VIsNull(Arg2)) then Result := False else Result := (Arg1 >= Arg2);
            coLT:
              if (VIsNull(Arg1) or VIsNull(Arg2)) then Result := False else Result := (Arg1 < Arg2);
            coLE:
              if (VIsNull(Arg1) or VIsNull(Arg2)) then Result := False else Result := (Arg1 <= Arg2);
            coOR:
              if (VIsNull(Arg1) or VIsNull(Arg2)) then Result := False else Result := (Arg1 or Arg2);
            coAND:
              if (VIsNull(Arg1) or VIsNull(Arg2)) then Result := False else Result := (Arg1 and Arg2);
            coADD:
              if (VIsNull(Arg1) or VIsNull(Arg2)) then Result := Null else Result := (Arg1 + Arg2);
            coSUB:
              if (VIsNull(Arg1) or VIsNull(Arg2)) then Result := Null else Result := (Arg1 - Arg2);
            coMUL:
              if (VIsNull(Arg1) or VIsNull(Arg2)) then Result := Null else Result := (Arg1 * Arg2);
            coDIV:
              if (VIsNull(Arg1) or VIsNull(Arg2)) then Result := Null else Result := (Arg1 / Arg2);
            coMOD,
            coREM:
              if (VIsNull(Arg1) or VIsNull(Arg2)) then Result := Null else Result := (Arg1 mod Arg2);
            coIN:
              if (VIsNull(Arg1) or VIsNull(Arg2)) then
                Result := False
              else if (VarIsArray(Arg2)) then
              begin
                Result := False;
                for i := 0 to VarArrayHighBound(Arg2, 1) do
                begin
                  if (VarIsEmpty(Arg2[i])) then break;
                  Result := (Arg1 = Arg2[i]);
                  if (Result) then break;
                end;
              end
              else
                Result := (Arg1 = Arg2);
            coLike:
              if (VIsNull(Arg1) or VIsNull(Arg2)) then
                Result := False
              else if (Arg2 = '%') then
                Result := Arg1 <> ''
              else if ((LeftStr(Arg2, 1) = '%') and (RightStr(Arg2, 1) = '%')) then
                Result := ContainsText(Arg1, Copy(Arg2, 2, Length(Arg2) - 2))
              else if (LeftStr(Arg2, 1) = '%') then
                Result := EndsText(Copy(Arg2, 2, Length(Arg2) - 1), Arg1)
              else if (RightStr(Arg2, 1) = '%') then
                Result := StartsText(Copy(Arg2, 1, Length(Arg2) - 1), Arg1)
              else
                Result := UpperCase(Arg1) = UpperCase(Arg2);
            else raise EDatabaseError.CreateFmt('coOp not supported (%d)', [Ord(Node^.coOp)]);
          end;
        end;

      nodeCOMPARE:
        begin
          Arg1 := ParseNode(@FilterParser.FilterData[CANEXPRSIZE + Node^.Compare.NodeOfs]);
          Arg2 := ParseNode(@FilterParser.FilterData[CANEXPRSIZE + Node^.Compare.DataOfs]);

          case (Node^.coOp) of
            coEQ,
            coNE:
              begin
                if (VIsNull(Arg1) or VIsNull(Arg2)) then
                  Result := False
                else if (Node^.Compare.PartLength = 0) then
                  if (Node^.Compare.CaseInsensitive) then
                    Result := lstrcmpi(PChar(VarToStr(Arg1)), PChar(VarToStr(Arg2))) = 0
                  else
                    Result := lstrcmp(PChar(VarToStr(Arg1)), PChar(VarToStr(Arg2))) = 0
                else
                  if (Node^.Compare.CaseInsensitive) then
                    Result := lstrcmpi(PChar(LeftStr(Arg1, Node^.Compare.PartLength)), PChar(LeftStr(Arg2, Node^.Compare.PartLength))) = 0
                  else
                    Result := lstrcmp(PChar(LeftStr(Arg1, Node^.Compare.PartLength)), PChar(LeftStr(Arg2, Node^.Compare.PartLength))) = 0;
                if (Node^.coOp = coNE) then
                  Result := not Result;
              end;
            coLIKE:
              if (VIsNull(Arg1) or VIsNull(Arg2)) then
                Result := False
              else if (Arg2 = '%') then
                Result := Arg1 <> ''
              else if ((LeftStr(Arg2, 1) = '%') and (RightStr(Arg2, 1) = '%')) then
                Result := ContainsText(Arg1, Copy(Arg2, 2, Length(Arg2) - 2))
              else if (LeftStr(Arg2, 1) = '%') then
                Result := EndsText(Copy(Arg2, 2, Length(Arg2) - 1), Arg1)
              else if (RightStr(Arg2, 1) = '%') then
                Result := StartsText(Copy(Arg2, 1, Length(Arg2) - 1), Arg1)
              else
                Result := UpperCase(Arg1) = UpperCase(Arg2);
            else raise EDatabaseError.CreateFmt('coOp not supported (%d)', [Ord(Node^.coOp)]);
          end;
        end;

      nodeFUNC:
        case (Node^.coOp) of
          coFUNC2:
            begin
              P := PAnsiChar(@FilterParser.FilterData[Expr^.iLiteralStart + Node^.Func.FunctionNameOfs]);
              Arg1 := ParseNode(@FilterParser.FilterData[CANEXPRSIZE + Node^.Func.ArgOfs]);

              if (lstrcmpiA(P, 'UPPER') = 0) then
                if (VIsNull(Arg1)) then Result := Null else Result := UpperCase(VarToStr(Arg1))

              else if (lstrcmpiA(P, 'LOWER') = 0) then
                if (VIsNull(Arg1)) then Result := Null else Result := LowerCase(VarToStr(Arg1))

              else if (lstrcmpiA(P, 'SUBSTRING') = 0) then
                if (VIsNull(Arg1)) then
                  Result := Null
                else
                begin
                  Result := Arg1;
                  try
                    Arg1 := VarToStr(Result[0]);
                  except
                    on EVariantError do // no Params for "SubString"
                      raise EDatabaseError.CreateFmt('InvMissParam',[Arg1]);
                  end;

                  if (Result[2] <> 0) then
                    Result := Copy(Arg1, Integer(Result[1]), Integer(Result[2]))
                  else if (Pos(',', Result[1]) > 0) then  // "From" and "To" entered without space!
                    Result := Copy(Arg1, Integer(Result[1]), StrToInt(Copy(Result[1], Pos(',', Result[1]) + 1, Length(Result[1]))))
                  else // No "To" entered so use all
                    Result := VarToStr(Arg1);
                end

              else if (lstrcmpiA(P, 'TRIM') = 0) then
                if (VIsNull(Arg1)) then Result := Null else Result := Trim(VarToStr(Arg1))

              else if (lstrcmpiA(P, 'TRIMLEFT') = 0) then
                if (VIsNull(Arg1)) then Result := Null else Result := TrimLeft(VarToStr(Arg1))

              else if (lstrcmpiA(P, 'TRIMRIGHT') = 0) then
                if (VIsNull(Arg1)) then Result := Null else Result := TrimRight(VarToStr(Arg1))

              else if (lstrcmpiA(P, 'GETDATE') = 0) then
                Result := Now()

              else if (lstrcmpiA(P, 'YEAR') = 0) then
                if (VIsNull(Arg1)) then Result := Null else begin DecodeDate(VarToDateTime(Arg1), Year, Month, Day); Result := Year; end

              else if (lstrcmpiA(P, 'MONTH') = 0) then
                if (VIsNull(Arg1)) then Result := Null else begin DecodeDate(VarToDateTime(Arg1), Year, Month, Day); Result := Month; end

              else if (lstrcmpiA(P, 'DAY') = 0) then
                if (VIsNull(Arg1)) then Result := Null else begin DecodeDate(VarToDateTime(Arg1), Year, Month, Day); Result := Day; end

              else if (lstrcmpiA(P, 'HOUR') = 0) then
                if (VIsNull(Arg1)) then Result := Null else begin DecodeTime(VarToDateTime(Arg1), Hour, Min, Sec, MSec); Result := Hour; end

              else if (lstrcmpiA(P, 'MINUTE') = 0) then
                if (VIsNull(Arg1)) then Result := Null else begin DecodeTime(VarToDateTime(Arg1), Hour, Min, Sec, MSec); Result := Min; end

              else if (lstrcmpiA(P, 'SECOND') = 0) then
                if (VIsNull(Arg1)) then Result := Null else begin DecodeTime(VarToDateTime(Arg1), Hour, Min, Sec, MSec); Result := Sec; end

              else if (lstrcmpiA(P, 'DATE') = 0) then  // Format: DATE('datestring','formatstring')
              begin                                    //   or    DATE(datevalue)
                Result := Arg1;
                if VarIsArray(Result) then
                begin
                  try
                    Arg1 := VarToStr(Result[0]);
                    Arg1 := VarToStr(Result[1]);
                  except
                    on EVariantError do // no Params for DATE
                      raise EDatabaseError.CreateFmt('Missing parameter', [Arg1]);
                  end;

                  S := FormatSettings.ShortDateFormat;
                  try
                    FormatSettings.ShortDateFormat := Arg1;
                    Result := StrToDate(Arg1);
                  finally
                    FormatSettings.ShortDateFormat := S;
                  end;
                end
                else
                  Result := Longint(Trunc(VarToDateTime(Result)));
              end

              else if (lstrcmpiA(P, 'TIME') = 0) then  // Format TIME('timestring','formatstring')
              begin                                               // or     TIME(datetimevalue)
                Result := Arg1;
                if (VarIsArray(Result)) then
                begin
                  try
                    Arg1 := VarToStr(Result[0]);
                    Arg1 := VarToStr(Result[1]);
                  except
                    on EVariantError do // no Params for TIME
                      raise EDatabaseError.CreateFmt('Missing parameter', [Arg1]);
                  end;

                  S := FormatSettings.ShortTimeFormat;
                  try
                    FormatSettings.ShortTimeFormat := Arg1;
                    Result := StrToTime(Arg1);
                  finally
                    FormatSettings.ShortTimeFormat := S;
                  end;
               end
               else
                 Result := Frac(VarToDateTime(Result));
              end

              else raise EDatabaseError.CreateFmt('Function not supported (%s)', [P]);
            end;

          else raise EDatabaseError.CreateFmt('coOp not supported', [Ord(Node^.coOp)]);
        end;

      nodeLISTELEM:
        case (Node^.coOp) of
          coLISTELEM2:
            begin
              Result := VarArrayCreate([0, 50], VarVariant); // Create VarArray for ListElements Values

              I := 0;
              TempNode := PCANHdr(@FilterParser.FilterData[CANEXPRSIZE + PWord(@PChar(Node)[CANHDRSIZE + I * 2])^]);
              while (TempNode^.nodeClass = nodeLISTELEM) do
              begin
                Arg1 := ParseNode(TempNode);
                if (not VarIsArray(Arg1)) then
                  Result[I] := Arg1
                else
                begin
                  Z := 0;
                  while (not VarIsEmpty(Arg1[Z])) do
                  begin
                    Result[I + Z] := Arg1[Z];
                    Inc(Z);
                  end;
                end;

                Inc(I);
                TempNode := PCANHdr(@FilterParser.FilterData[CANEXPRSIZE + PWord(@PChar(Node)[CANHDRSIZE + I * 2])^]);
             end;

             // Only one or no Value, so don't return as VarArray
             if (I < 2) then
               if (VIsNull(Result[0])) then
                 Result := False
               else
                 Result := VarAsType(Result[0], varString);
            end;
          else raise EDatabaseError.CreateFmt('coOp not supported (%d)', [Ord(Node^.coOp)]);
        end;

      else
        raise EDatabaseError.CreateFmt('nodeClass not supported', [Ord(Node^.nodeClass)]);
    end;
  end;

begin
  Expr := PCANExpr(@FilterParser.FilterData[0]);
  Result := ParseNode(@FilterParser.FilterData[Expr^.iNodeStart]);
end;

{ TMySQLTable *****************************************************************}

function TMySQLTable.GetCanModify(): Boolean;
begin
  if (not IndexDefs.Updated) then
    UpdateIndexDefs();

  Result := True;
end;

constructor TMySQLTable.Create(AOwner: TComponent);
begin
  inherited;

  FAutomaticLoadNextRecords := False;
  SetLength(DeleteBookmarks, 0);
  FCommandType := ctTable;
  FLimitedDataReceived := False;
end;

procedure TMySQLTable.InternalClose();
begin
  inherited;

  FLimitedDataReceived := False;
end;

procedure TMySQLTable.InternalLast();
begin
  if (LimitedDataReceived and AutomaticLoadNextRecords and LoadNextRecords(True)) then
    RecordsReceived.WaitFor(INFINITE);

  inherited;
end;

procedure TMySQLTable.InternalOpen();
begin
  Assert(CommandText <> '');

  inherited;

  if (IsCursorOpen()) then
    if (Filtered) then
      InternActivateFilter();
end;

function TMySQLTable.LoadNextRecords(const AllRecords: Boolean = False): Boolean;
begin
  RecordsReceived.ResetEvent();

  Result := Connection.InternExecuteSQL(smDataSet, True, SQLSelect(AllRecords));
  if (Result) then
  begin
    Connection.SyncBindDataSet(Self);
    InternRecordBuffers.Received.WaitFor(INFINITE);
  end;
end;

procedure TMySQLTable.Sort(const ASortDef: TIndexDef);
var
  FieldName: string;
  Pos: Integer;
  StringFieldsEnclosed: Boolean;
begin
  StringFieldsEnclosed := False;
  Pos := 1;
  repeat
    FieldName := ExtractFieldName(ASortDef.Fields, Pos);
    if (Assigned(FindField(FieldName))) then
      StringFieldsEnclosed := StringFieldsEnclosed or (FieldByName(FieldName).DataType in [ftWideMemo, ftWideString]);
  until (FieldName = '');

  if (Active and ((ASortDef.Fields <> SortDef.Fields) or (ASortDef.DescFields <> SortDef.DescFields))) then
  begin
    if ((ASortDef.Fields <> '') and (RecordsReceived.WaitFor(IGNORE) = wrSignaled) and not LimitedDataReceived and (InternRecordBuffers.Count < 1000) and not StringFieldsEnclosed) then
      inherited
    else
    begin
      SortDef.Assign(ASortDef);
      FOffset := 0;

      Refresh();

      SetFieldsSortTag();
    end;
  end;
end;

function TMySQLTable.SQLSelect(): string;
begin
  Result := SQLSelect(False);
end;

function TMySQLTable.SQLSelect(const IgnoreLimit: Boolean): string;
var
  DescFieldName: string;
  DescPos: Integer;
  FieldName: string;
  FirstField: Boolean;
  Pos: Integer;
begin
  Result := inherited SQLSelect();
  if (SortDef.Fields <> '') then
  begin
    Result := Result + ' ORDER BY ';
    Pos := 1; FirstField := True;
    repeat
      FieldName := ExtractFieldName(SortDef.Fields, Pos);
      if (FieldName <> '') then
      begin
        if (not FirstField) then Result := Result + ',';
        Result := Result + Connection.EscapeIdentifier(FieldName);

        DescPos := 1;
        repeat
          DescFieldName := ExtractFieldName(SortDef.DescFields, DescPos);
          if (DescFieldName = FieldName) then
            Result := Result + ' DESC';
        until (DescFieldName = '');
      end;
      FirstField := False;
    until (FieldName = '');
  end;
  if (Limit = 0) then
    RequestedRecordCount := $7fffffff
  else
  begin
    Result := Result + ' LIMIT ';
    if (Offset + InternRecordBuffers.Count > 0) then
      Result := Result + IntToStr(Offset + InternRecordBuffers.Count) + ',';
    if (IgnoreLimit) then
      RequestedRecordCount := $7fffffff - (Offset + InternRecordBuffers.Count)
    else if (InternRecordBuffers.Count = 0) then
      RequestedRecordCount := Limit
    else
      RequestedRecordCount := InternRecordBuffers.Count;
    Result := Result + IntToStr(RequestedRecordCount);
  end;
end;

{******************************************************************************}

//function HexToStr(const Hex: string): RawByteString;
//const
//  Digits: string = '0123456789ABCDEF';
//var
//  Index: Integer;
//begin
//  Index := 0;
//  Result := '';
//  while (Index < Length(Hex) div 2) do
//  begin
//    Result := Result + AnsiChar(Chr((Pos(Hex[2 * Index + 1], Digits) - 1) * $10 + Pos(Hex[2 * Index + 2], Digits) - 1));
//    Inc(Index);
//  end;
//end;
//
//var
//  Len: Integer;
//  RBS: RawByteString;
//  S: string;
initialization
//  RBS := HexToStr('BFB9BEE0');
//  SetLength(S, Length(RBS));
//  Len := AnsiCharToWideChar(65001, PAnsiChar(RBS), Length(RBS), PChar(S), Length(S));
//  SetLength(S, Len);
//
  MySQLConnectionOnSynchronize := nil;
  SynchronizingThreads := TList.Create();
  SynchronizingThreadsCS := TCriticalSection.Create();

  LocaleFormatSettings := TFormatSettings.Create(LOCALE_USER_DEFAULT);
  SetLength(MySQLLibraries, 0);
finalization
  SynchronizingThreadsCS.Free();
  SynchronizingThreads.Free();

  FreeMySQLLibraries();
end.

