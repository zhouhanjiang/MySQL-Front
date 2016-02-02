unit fSession;

interface {********************************************************************}

uses
  SysUtils, Classes, Windows,
  Graphics,
  DB,
  acAST, acQBBase, acMYSQLSynProvider, acQBEventMetaProvider,
  SQLUtils, MySQLDB,
  fPreferences;

type
  TMySQLEventType = (etUnknown, etSingle, etMultiple);
  TMySQLIntervalType = (itUnknown, itYear, itQuarter, itMonth, itDay, itHour,
    itMinute, itWeek, itSecond, itMicrosecond, itYearMonth, itDayHour,
    itDayMinute, itDaySecond, itHourMinute, itHourSecond, itMinuteSecond,
    itDayMicrosecond, itHourMicrosecond, itMinuteMicrosecond, itSecondMicrosecond);
  TMySQLFieldKind = (mkUnknown, mkReal, mkVirtual);
  TMySQLFieldStored = (msUnknown, msVirtual, msStored);
  TMySQLFieldType = (mfUnknown,
    mfBit, mfTinyInt, mfSmallInt, mfMediumInt, mfInt, mfBigInt,
    mfFloat, mfDouble, mfDecimal, mfDate, mfDateTime, mfTimeStamp, mfTime, mfYear,
    mfChar, mfVarChar, mfTinyText, mfText, mfMediumText, mfLongText, mfEnum, mfSet,
    mfBinary, mfVarBinary, mfTinyBlob, mfBlob, mfMediumBlob, mfLongBlob,
    mfGeometry, mfPoint, mfLineString, mfPolygon, mfMultiPoint, mfMultiLineString, mfMultiPolygon, mfGeometryCollection,
    mfJSON);
  TMySQLPartitionType = (ptNone, ptHash, ptKey, ptRange, ptList);
  TMySQLRowType = (mrUnknown, mrFixed, mrDynamic, mrCompressed, mrRedundant, mrCompact);
  TMySQLForeignKeyDeleteType = (dtNoAction, dtCascade, dtSetNull, dtSetDefault, dtRestrict);
  TMySQLForeignKeyUpdateType = (utNoAction, utCascade, utSetNull, utSetDefault, utRestrict);
  TMySQLForeignKeyMatchType = (mtNo, mtFull, mtPartial);

const
  NotQuotedFieldTypes = [mfBit, mfTinyInt, mfSmallInt, mfMediumInt, mfInt, mfBigInt, mfFloat, mfDouble, mfDecimal, mfYear];
  BinaryFieldTypes = [mfBinary, mfVarBinary, mfTinyBlob, mfBlob, mfMediumBlob, mfLongBlob];
  TextFieldTypes = [mfChar, mfVarChar, mfTinyText, mfText, mfMediumText, mfLongText, mfEnum, mfSet, mfJSON];
  LOBFieldTypes = [mfTinyText, mfText, mfMediumText, mfLongText, mfTinyBlob, mfBlob, mfMediumBlob, mfLongBlob];

type
  TSItems = class;
  TSEntities = class;
  TSObjects = class;
  TSDBObject = class;
  TSDBObjects = class;
  TSDependencies = class;
  TSKeyColumns = class;
  TSKey = class;
  TSTableField = class;
  TSBaseTableField = class;
  TSKeys = class;
  TSTableFields = class;
  TSForeignKey = class;
  TSForeignKeys = class;
  TSTable = class;
  TSBaseTableFields = class;
  TSBaseTable = class;
  TSView = class;
  TSTables = class;
  TSRoutine = class;
  TSRoutines = class;
  TSTrigger = class;
  TSTriggers = class;
  TSEvent = class;
  TSEvents = class;
  TSDatabase = class;
  TSDatabases = class;
  TSVariable = class;
  TSVariables = class;
  TSUserRight = class;
  TSUser = class;
  TSUsers = class;
  TSPlugin = class;
  TSPlugins = class;
  TSEngine = class;
  TSEngines = class;
  TSFieldType = class;
  TSFieldTypes = class;
  TSCharsets = class;
  TSCollation = class;
  TSCollations = class;
  TSSession = class;
  TSSessions = class;

  TSSecurity = (seDefiner, seInvoker);

  TSItem = class(TObject)
  private
    FName: string;
  protected
    FCItems: TSItems;
    function GetCaption(): string; virtual;
    function GetIndex(): Integer; virtual;
    procedure SetName(const AName: string); virtual;
  public
    procedure Assign(const Source: TSItem); virtual;
    function Equal(const Second: TSItem): Boolean; virtual;
    constructor Create(const ACItems: TSItems; const AName: string = ''); virtual;
    property Caption: string read GetCaption;
    property CItems: TSItems read FCItems;
    property Index: Integer read GetIndex;
    property Name: string read FName write SetName;
  end;

  TSItems = class(TList)
  private
    FSession: TSSession;
    function GetItem(Index: Integer): TSItem; inline;
  protected
    function GetCount(): Integer; virtual;
    function InsertIndex(const Name: string; out Index: Integer): Boolean; virtual;
  public
    procedure Clear(); override;
    constructor Create(const ASession: TSSession);
    destructor Destroy(); override;
    function IndexByName(const Name: string): Integer; virtual;
    function NameCmp(const Name1, Name2: string): Integer; virtual;
    property Session: TSSession read FSession;
    property Count: Integer read GetCount;
    property Item[Index: Integer]: TSItem read GetItem; default;
  end;

  TSEntity = class(TSItem)
  private
    function GetEntities(): TSEntities; inline;
  public
    property Entities: TSEntities read GetEntities;
  end;

  TSEntities = class(TSItems)
  private
    FSession: TSSession;
  protected
    FValid: Boolean;
    function Add(const AEntity: TSEntity; const ExecuteEvent: Boolean = False): Integer; virtual;
    function Build(const DataSet: TMySQLQuery; const UseInformationSchema: Boolean; Filtered: Boolean = False; const SessionEvents: Boolean = True): Boolean; virtual;
    procedure Delete(const AEntity: TSEntity); overload; virtual;
    function GetValid(): Boolean; virtual;
    function SQLGetItems(const Name: string = ''): string; virtual; abstract;
  public
    constructor Create(const ASession: TSSession); reintroduce; virtual;
    procedure Invalidate(); virtual;
    procedure PushBuildEvent(const Sender: TObject); virtual;
    function Update(): Boolean; virtual;
    property Session: TSSession read FSession;
    property Valid: Boolean read GetValid;
  end;

  TSObject = class(TSEntity)
  type
    TDesktop = class
    private
      FCObject: TSObject;
    protected
      property CObject: TSObject read FCObject;
    public
      constructor Create(const ACObject: TSObject);
    end;
  private
    function GetObjects(): TSObjects; inline;
  protected
    FInvalid: Boolean;
    FDesktop: TDesktop;
    FSession: TSSession;
    FSource: string;
    FValidSource: Boolean;
    procedure FreeDesktop(); virtual;
    function GetDesktop(): TDesktop; virtual;
    function GetSource(): string; virtual;
    function GetValid(): Boolean; virtual;
    function GetValidSource(): Boolean; virtual;
    procedure SetName(const AName: string); override;
    procedure SetSource(const AField: TField); overload; virtual;
    procedure SetSource(const ADataSet: TMySQLQuery); overload; virtual; abstract;
    procedure SetSource(const ASource: string); overload; virtual;
    property ValidSource: Boolean read GetValidSource;
  public
    procedure Assign(const Source: TSObject); reintroduce; virtual;
    constructor Create(const ACItems: TSItems; const AName: string = ''); reintroduce; virtual;
    destructor Destroy(); override;
    procedure Invalidate(); virtual;
    function Update(): Boolean; virtual;
    property Desktop: TDesktop read GetDesktop;
    property Invalid: Boolean read FInvalid;
    property Objects: TSObjects read GetObjects;
    property Session: TSSession read FSession;
    property Source: string read GetSource;
    property Valid: Boolean read GetValid;
  end;

  TSObjects = class(TSEntities)
  protected
    function Add(const AEntity: TSEntity; const ExecuteEvent: Boolean = False): Integer; override;
    procedure Delete(const AEntity: TSEntity); override;
  public
    procedure Invalidate(); override;
  end;

  TSDependency = class
  private
    function GetDBObject(): TSDBObject;
  protected
    DatabaseName: string;
    ObjectClass: TClass;
    ObjectName: string;
    Session: TSSession;
  public
    property DBObject: TSDBObject read GetDBObject;
  end;

  TSDependencies = class(TList)
  private
    function GetDependency(Index: Integer): TSDependency;
  public
    function Add(Item: Pointer): Integer;
    destructor Destroy(); override;
    property Dependency[Index: Integer]: TSDependency read GetDependency; default;
  end;

  TSDBObject = class(TSObject)
  private
    FDatabase: TSDatabase;
    function GetDBObjects(): TSDBObjects; inline;
  protected
    FDependencies: TSDependencies;
    function GetDependencies(): TSDependencies; virtual;
    procedure SetDatabase(const ADatabase: TSDatabase); virtual;
    procedure SetSource(const ASource: string); override;
    function SQLGetSource(): string; virtual; abstract;
  public
    constructor Create(const ACDBObjects: TSDBObjects; const AName: string = ''); reintroduce; virtual;
    destructor Destroy(); override;
    function GetSourceEx(const DropBeforeCreate: Boolean = False; const EncloseDefiner: Boolean = True): string; virtual; abstract;
    procedure Invalidate(); override;
    procedure PushBuildEvent(const CItemsEvents: Boolean = True); virtual;
    function Update(): Boolean; override;
    property Database: TSDatabase read FDatabase;
    property DBObjects: TSDBObjects read GetDBObjects;
    property Dependencies: TSDependencies read GetDependencies;
  end;

  TSDBObjects = class(TSObjects)
  private
    FDatabase: TSDatabase;
  protected
    function Add(const AEntity: TSEntity; const ExecuteEvent: Boolean = False): Integer; override;
    procedure Delete(const AEntity: TSEntity); override;
  public
    constructor Create(const ADatabase: TSDatabase); reintroduce; virtual;
    property Database: TSDatabase read FDatabase;
  end;

  TSKeyColumn = class
  private
    FKeyColumns: TSKeyColumns;
  public
    Ascending: Boolean;
    Field: TSBaseTableField;
    Length: Integer;
    procedure Assign(const Source: TSKeyColumn); virtual;
    constructor Create(const AKeyColumns: TSKeyColumns); virtual;
    function Equal(const Second: TSKeyColumn): Boolean; virtual;
    property IndexColumns: TSKeyColumns read FKeyColumns;
  end;

  TSKeyColumns = class(TSItems)
  private
    FKey: TSKey;
    function GetColumn(Index: Integer): TSKeyColumn;
  public
    procedure AddColumn(const NewColumn: TSKeyColumn); virtual;
    constructor Create(const AKey: TSKey); virtual;
    procedure DeleteColumn(const AColumn: TSKeyColumn); virtual;
    function KeyByField(const AField: TSTableField): Integer; virtual;
    property Column[Index: Integer]: TSKeyColumn read GetColumn; default;
    property Count: Integer read GetCount;
    property Key: TSKey read FKey;
  end;

  TSKey = class(TSItem)
  private
    Created: Boolean;
    FColumns: TSKeyColumns;
    OriginalName: string;
    function GetKeys(): TSKeys; inline;
    function GetTable(): TSBaseTable;
  protected
    function GetCaption(): string; override;
    procedure SetName(const AName: string); override;
  public
    BlockSize: Integer;
    Comment: string;
    Fulltext: Boolean;
    IndexType: string;
    PrimaryKey: Boolean;
    Unique: Boolean;
    procedure Assign(const Source: TSKey); reintroduce; virtual;
    procedure Clear(); virtual;
    function ColumnByField(const AField: TSBaseTableField): TSKeyColumn; virtual;
    function ColumnByFieldName(const AFieldName: string): TSKeyColumn; virtual;
    constructor Create(const AKeys: TSKeys; const AName: string = ''); reintroduce; virtual;
    destructor Destroy(); override;
    function Equal(const Second: TSKey): Boolean; reintroduce; virtual;
    procedure GetSortDef(var SortDef: TIndexDef);
    property Columns: TSKeyColumns read FColumns;
    property Index: Integer read GetIndex;
    property Keys: TSKeys read GetKeys;
    property Table: TSBaseTable read GetTable;
  end;

  TSKeys = class(TSItems)
  private
    FTable: TSBaseTable;
    function GetKey(Index: Integer): TSKey; inline;
    function GetPrimaryKey(): TSKey;
  public
    procedure AddKey(const NewKey: TSKey); virtual;
    procedure Assign(const Source: TSKeys); virtual;
    constructor Create(const ATable: TSBaseTable); reintroduce; virtual;
    procedure DeleteKey(const AKey: TSKey); virtual;
    property Key[Index: Integer]: TSKey read GetKey; default;
    property PrimaryKey: TSKey read GetPrimaryKey;
    property Table: TSBaseTable read FTable;
  end;

  TSField = class(TSItem)
  private
    FFieldTypes: TSFieldTypes;
  protected
    procedure ParseFieldType(var Parse: TSQLParse); virtual;
  public
    Charset: string;
    Decimals: Integer;
    Expression: string;
    FieldKind: TMySQLFieldKind;
    FieldType: TMySQLFieldType;
    Items: array of string;
    National: Boolean;
    Size: Integer;
    Stored: TMySQLFieldStored;
    Unsigned: Boolean;
    procedure Assign(const Source: TSField); reintroduce; virtual;
    procedure Clear(); virtual;
    constructor Create(const AFieldTypes: TSFieldTypes; const AName: string = ''); reintroduce; virtual;
    function DBTypeStr(): string; virtual;
    function Equal(const Second: TSField): Boolean; reintroduce; virtual;
    function EscapeValue(const Value: string): string; virtual;
    property FieldTypes: TSFieldTypes read FFieldTypes;
  end;

  TSTableField = class(TSField)
  private
    FCollation: string;
    FFields: TSTableFields;
    FInPrimaryKey: Boolean;
    FInUniqueKey: Boolean;
    function GetTable(): TSTable; inline;
  protected
    procedure ParseFieldType(var Parse: TSQLParse); override;
  public
    Ascii: Boolean;
    AutoIncrement: Boolean;
    Binary: Boolean;
    Comment: string;
    Default: string;
    FieldBefore: TSTableField;
    Format: TMySQLRowType;
    NullAllowed: Boolean;
    Unicode: Boolean;
    Zerofill: Boolean;
    procedure Assign(const Source: TSField); override;
    procedure Clear(); override;
    constructor Create(const AFields: TSTableFields; const AName: string = ''); reintroduce; virtual;
    function DBTypeStr(): string; override;
    destructor Destroy(); override;
    function Equal(const Second: TSTableField): Boolean; reintroduce; virtual;
    function UnescapeValue(const Value: string): string; virtual;
    property Collation: string read FCollation write FCollation;
    property Fields: TSTableFields read FFields;
    property InPrimaryKey: Boolean read FInPrimaryKey;
    property InUniqueKey: Boolean read FInUniqueKey;
    property Index: Integer read GetIndex;
    property Table: TSTable read GetTable;
  end;

  TSBaseTableField = class(TSTableField)
  private
    function GetTable(): TSBaseTable;
  protected
    function GetIndex(): Integer; override;
    procedure SetName(const AName: string); override;
  public
    Moved: Boolean;
    OnUpdate: string;
    OriginalName: string;
    procedure Assign(const Source: TSField); override;
    procedure Clear(); override;
    constructor Create(const AFields: TSTableFields; const AName: string = ''); override;
    property Table: TSBaseTable read GetTable;
  end;

  TSViewField = class(TSTableField)
    function GetIndex(): Integer; override;
  end;

  TSTableFields = class(TSItems)
  private
    FTable: TSTable;
    function GetField(Index: Integer): TSTableField; inline;
  protected
    function FieldByName(const FieldName: string): TSTableField; virtual;
    function InsertIndex(const Name: string; out Index: Integer): Boolean; override;
  public
    procedure AddField(const NewField: TSTableField); virtual;
    procedure Assign(const Source: TSTableFields); virtual;
    constructor Create(const ATable: TSTable);
    procedure DeleteField(const AField: TSTableField); virtual;
    function IndexByName(const Name: string): Integer; override;
    function IndexOf(const AField: TSTableField): Integer; virtual;
    property Field[Index: Integer]: TSTableField read GetField; default;
    property Table: TSTable read FTable;
  end;

  TSBaseTableFields = class(TSTableFields)
  public
    procedure MoveField(const AField: TSTableField; const NewFieldBefore: TSTableField); virtual;
  end;

  TSViewFields = class(TSTableFields)
  protected
    FValid: Boolean;
  public
    constructor Create(const ATable: TSTable);
    procedure Invalidate(); virtual;
    property Valid: Boolean read FValid;
  end;

  TSForeignKey = class(TSItem)
  private
    function GetForeignKeys(): TSForeignKeys; inline;
    function GetTable(): TSBaseTable;
  protected
    Created: Boolean;
    OriginalName: string;
    procedure SetName(const AName: string); override;
  public
    Fields: array of TSTableField;
    Match: TMySQLForeignKeyMatchType;
    OnDelete: TMySQLForeignKeyDeleteType;
    OnUpdate: TMySQLForeignKeyUpdateType;
    Parent : record
      DatabaseName: string;
      TableName: string;
      FieldNames: array of string;
    end;
    procedure Assign(const Source: TSForeignKey); reintroduce; virtual;
    procedure Clear(); virtual;
    constructor Create(const AForeignKeys: TSForeignKeys; const AName: string = ''); reintroduce; virtual;
    function DBTypeStr(): string; virtual;
    function Equal(const Second: TSForeignKey): Boolean; reintroduce; virtual;
    destructor Destroy(); override;
    property ForeignKeys: TSForeignKeys read GetForeignKeys;
    property Index: Integer read GetIndex;
    property Table: TSBaseTable read GetTable;
  end;

  TSForeignKeys = class(TSItems)
  private
    FTable: TSBaseTable;
    function GetSession(): TSSession; inline;
    function GetForeignKey(Index: Integer): TSForeignKey; inline;
  protected
    FValid: Boolean;
  public
    procedure AddForeignKey(const NewForeignKey: TSForeignKey); virtual;
    procedure Assign(const Source: TSForeignKeys); virtual;
    procedure Clear(); override;
    constructor Create(const ATable: TSBaseTable); reintroduce; virtual;
    procedure DeleteForeignKey(const AForeignKey: TSForeignKey); virtual;
    procedure InsertForeignKey(const Index: Integer; const NewForeignKey: TSForeignKey); virtual;
    property Session: TSSession read GetSession;
    property ForeignKey[Index: Integer]: TSForeignKey read GetForeignKey; default;
    property Table: TSBaseTable read FTable;
    property Valid: Boolean read FValid;
  end;

  TSTableDataSet = class(TMySQLTable)
  private
    FFilterSQL: string;
    FQuickSearch: string;
    FTable: TSTable;
  protected
    function SQLSelect(const IgnoreLimit: Boolean = False): string; override;
  public
    constructor Create(const ATable: TSTable); reintroduce; virtual;
    property FilterSQL: string read FFilterSQL write FFilterSQL;
    property QuickSearch: string read FQuickSearch write FQuickSearch;
    property Table: TSTable read FTable;
  end;

  TSTable = class(TSDBObject)
  private
    FFields: TSTableFields;
    function GetDataSet(): TSTableDataSet;
    function GetTables(): TSTables; inline;
    function GetValidData(): Boolean;
    function OpenEvent(const DataHandle: TMySQLConnection.TDataResult; const Data: Boolean): Boolean;
  protected
    FDataSet: TSTableDataSet;
    FFilterSQL: string;
    FSourceParsed: Boolean;
    function GetFields(): TSTableFields; virtual;
    procedure SetName(const AName: string); override;
    property SourceParsed: Boolean read FSourceParsed;
  public
    procedure Assign(const Source: TSTable); reintroduce; virtual;
    function CountRecords(): Integer; virtual;
    constructor Create(const ACDBObjects: TSDBObjects; const AName: string = ''); reintroduce; virtual;
    function DeleteRecords(const Field: TSTableField; const Values: TStringList): Boolean; virtual;
    destructor Destroy(); override;
    function FieldByName(const FieldName: string): TSTableField; virtual;
    function GetSourceEx(const DropBeforeCreate: Boolean = False; const EncloseDefiner: Boolean = True): string; override;
    procedure Invalidate(); override;
    procedure InvalidateData(); virtual;
    procedure Open(const FilterSQL, QuickSearch: string; const ASortDef: TIndexDef; const Offset: Integer; const Limit: Integer); virtual;
    procedure PushBuildEvent(const CItemsEvents: Boolean = True); override;
    property DataSet: TSTableDataSet read GetDataSet;
    property Fields: TSTableFields read GetFields;
    property Index: Integer read GetIndex;
    property Tables: TSTables read GetTables;
    property ValidData: Boolean read GetValidData;
  end;

  TSPartition = class(TSItem)
  private
    FTable: TSBaseTable;
  protected
    OriginalName: string;
    function DBTypeStr(): string; virtual;
  public
    Comment: string;
    Engine: TSEngine;
    MaxRows: Integer;
    MinRows: Integer;
    ValuesExpr: string;
    procedure Assign(const Source: TSPartition); reintroduce; virtual;
    procedure Clear(); virtual;
    constructor Create(const ACItems: TSItems; const ATable: TSBaseTable); reintroduce; virtual;
    function Equal(const Second: TSPartition): Boolean; reintroduce; virtual;
    property Table: TSBaseTable read FTable;
  end;

  TSPartitions = class(TSItems)
  private
    FTable: TSBaseTable;
    function GetPartition(Index: Integer): TSPartition;
  public
    Expression: string;
    Linear: Boolean;
    PartitionsNumber: Integer;
    PartitionType: TMySQLPartitionType;
    procedure AddPartition(const NewPartition: TSPartition); virtual;
    procedure Assign(const Source: TSPartitions); virtual;
    procedure Clear(); override;
    constructor Create(const ATable: TSBaseTable); reintroduce; virtual;
    procedure DeletePartition(const APartition: TSPartition); virtual;
    function IndexOf(const APartition: TSPartition): Integer; virtual;
    procedure MovePartition(const APartition: TSPartition; const NewIndex: Integer); virtual;
    function UpdatePartition(const Partition, NewPartition: TSPartition): Boolean; virtual;
    property Table: TSBaseTable read FTable;
    property Partition[Index: Integer]: TSPartition read GetPartition; default;
  end;

  TSBaseTable = class(TSTable)
  type
    TPackKeys = (piUnpacked, piPacked, piDefault);
    TInsertMethod = (imNo, imFirst, imLast);
    TMergeSourceTable = record DatabaseName: string; TableName: string; end;
  private
    FAutoIncrement: LargeInt; // 0 -> unknown
    FAvgRowLength: LargeInt;
    FBlockSize: Integer;
    FChecked: TDateTime;
    FChecksum: Boolean;
    FCollation: string;
    FComment: string;
    FCreated: TDateTime;
    FDataSize: Int64;
    FDefaultCharset: string;
    FDefaultCodePage: Cardinal;
    FDelayKeyWrite: Boolean;
    FEngine: TSEngine;
    FForeignKeys: TSForeignKeys;
    FIndexSize: Int64;
    FInsertMethod: TInsertMethod;
    FKeys: TSKeys;
    FMaxDataSize: Int64;
    FMaxRows: Int64;
    FMergeSourceTables: array of TMergeSourceTable;
    FMinRows: Int64;
    FPackKeys: TPackKeys;
    FPartitions: TSPartitions;
    FRows: Int64;
    FRowType: TMySQLRowType;
    FTemporary: Boolean;
    FUnusedSize: Int64;
    FUpdated: TDateTime;
    function GetAutoIncrementField(): TSBaseTableField;
    function GetBaseTableFields(): TSBaseTableFields; inline;
    function GetPrimaryKey(): TSKey;
    function GetTriggers(Index: Integer): TSTrigger;
    function GetTriggerCount(): Integer;
    procedure SetDefaultCharset(const ADefaultCharset: string);
  protected
    FValidStatus: Boolean;
    procedure BuildStatus(const DataSet: TMySQLQuery; const UseInformationSchema: Boolean); virtual;
    function GetDependencies(): TSDependencies; override;
    function GetValid(): Boolean; override;
    procedure ParseCreateTable(const SQL: string); virtual;
    procedure SetSource(const ADataSet: TMySQLQuery); overload; override;
    function SQLGetSource(): string; override;
  public
    procedure Assign(const Source: TSTable); override;
    function FieldByName(const FieldName: string): TSBaseTableField; reintroduce; virtual;
    function ForeignKeyByName(const ForeignKeyName: string): TSForeignKey; virtual;
    function CountRecords(): Integer; override;
    constructor Create(const ACDBObjects: TSDBObjects; const AName: string = ''; const ASystemTable: Boolean = False); reintroduce; virtual;
    destructor Destroy(); override;
    function DBRowTypeStr(): string; virtual;
    procedure Empty(); virtual;
    function EmptyFields(const Fields: TList): Boolean; virtual;
    function GetSourceEx(const DropBeforeCreate: Boolean = False; const EncloseDefiner: Boolean = True): string; override;
    function KeyByCaption(const Caption: string): TSKey; virtual;
    function IndexByName(const Name: string): TSKey; virtual;
    function KeyByDataSet(const DataSet: TSTableDataSet): TSKey; virtual;
    procedure Invalidate(); override;
    procedure InvalidateStatus(); virtual;
    function PartitionByName(const PartitionName: string): TSPartition; virtual;
    procedure PushBuildEvent(const CItemsEvents: Boolean = True); override;
    function Repair(): Boolean; virtual;
    property AutoIncrement: LargeInt read FAutoIncrement write FAutoIncrement;
    property AutoIncrementField: TSBaseTableField read GetAutoIncrementField;
    property AvgRowLength: LargeInt read FAvgRowLength;
    property BlockSize: Integer read FBlockSize write FBlockSize;
    property Checked: TDateTime read FChecked write FChecked;
    property Checksum: Boolean read FChecksum write FChecksum;
    property Collation: string read FCollation write FCollation;
    property Comment: string read FComment write FComment;
    property Created: TDateTime read FCreated;
    property DataSize: Int64 read FDataSize;
    property DefaultCharset: string read FDefaultCharset write SetDefaultCharset;
    property DefaultCodePage: Cardinal read FDefaultCodePage;
    property DelayKeyWrite: Boolean read FDelayKeyWrite write FDelayKeyWrite;
    property Engine: TSEngine read FEngine write FEngine;
    property Fields: TSBaseTableFields read GetBaseTableFields;
    property IndexSize: Int64 read FIndexSize;
    property InsertMethod: TInsertMethod read FInsertMethod write FInsertMethod;
    property ForeignKeys: TSForeignKeys read FForeignKeys;
    property Keys: TSKeys read FKeys;
    property MaxDataSize: Int64 read FMaxDataSize;
    property MaxRows: Int64 read FMaxRows;
    property MinRows: Int64 read FMinRows;
    property PackKeys: TPackKeys read FPackKeys write FPackKeys;
    property Partitions: TSPartitions read FPartitions;
    property PrimaryKey: TSKey read GetPrimaryKey;
    property Rows: Int64 read FRows;
    property RowType: TMySQLRowType read FRowType write FRowType;
    property Temporary: Boolean read FTemporary write FTemporary;
    property TriggerCount: Integer read GetTriggerCount;
    property Triggers[Index: Integer]: TSTrigger read GetTriggers;
    property UnusedSize: Int64 read FUnusedSize write FUnusedSize;
    property Updated: TDateTime read FUpdated;
    property ValidStatus: Boolean read FValidStatus;
  end;

  TSSystemView = class(TSBaseTable)
  end;

  TSView = class(TSTable)
  type
    TAlgorithm = (vaUndefined, vaMerge, vaTemptable);
    TCheckOption = (voNone, voDefault, voCascaded, voLocal);
  private
    ActualDependencies: Boolean;
    FAlgorithm: TAlgorithm;
    FCheckOption: TCheckOption;
    FDefiner: string;
    FSecurity: TSSecurity;
    FStmt: string;
    function GetValidFields(): Boolean; inline;
    function GetViewFields(): TSViewFields; inline;
    function ParseCreateView(const SQL: string): string;
  protected
    FComment: string;
    function GetDependencies(): TSDependencies; override;
    function GetValid(): Boolean; override;
    procedure SetSource(const ADataSet: TMySQLQuery); overload; override;
    procedure SetSource(const ASource: string); override;
    function SQLGetSource(): string; override;
    property ValidFields: Boolean read GetValidFields;
  public
    procedure Assign(const Source: TSTable); override;
    constructor Create(const ACDBObjects: TSDBObjects; const AName: string = ''); override;
    function GetSourceEx(const DropBeforeCreate: Boolean = False; const EncloseDefiner: Boolean = True): string; overload; override;
    procedure Invalidate(); override;
    property Algorithm: TAlgorithm read FAlgorithm write FAlgorithm;
    property CheckOption: TCheckOption read FCheckOption write FCheckOption;
    property Definer: string read FDefiner;
    property Fields: TSViewFields read GetViewFields;
    property Security: TSSecurity read FSecurity write FSecurity;
    property Stmt: string read FStmt write FStmt;
  end;

  TSTables = class(TSDBObjects)
  private
    function GetTable(Index: Integer): TSTable; inline;
    function GetValidStatus(): Boolean;
  protected
    function Build(const DataSet: TMySQLQuery; const UseInformationSchema: Boolean; Filtered: Boolean = False; const SessionEvents: Boolean = True): Boolean; overload; override;
    procedure BuildViewFields(const DataSet: TMySQLQuery; const UseInformationSchema: Boolean);
    function SQLGetItems(const Name: string = ''): string; override;
    function SQLGetStatus(const Tables: TList = nil): string;
    function SQLGetViewFields(const Tables: TList = nil): string;
  public
    procedure AddTable(const NewTable: TSTable); virtual;
    function NameCmp(const Name1, Name2: string): Integer; override;
    property Table[Index: Integer]: TSTable read GetTable; default;
    property ValidStatus: Boolean read GetValidStatus;
  end;

  TSRoutineParameter = class(TSField)
  type
    TParameterType = (ptIn, ptOut, ptInOut);
  private
    FRoutine: TSRoutine;
    FParameterType: TParameterType;
  public
    procedure Assign(const Source: TSField); override;
    constructor Create(ARoutine: TSRoutine); reintroduce; virtual;
    property Routine: TSRoutine read FRoutine;
    property ParameterType: TParameterType read FParameterType;
  end;

  TSRoutine = class(TSDBObject)
  type
    TRoutineType = (rtUnknown, rtProcedure, rtFunction);
    TMySQLDataSets = array of TMySQLDataSet;
  private
    FStmt: string;
    FComment: string;
    FCreated: TDateTime;
    FDefiner: string;
    FFunctionResult: TSField;
    FInputDataSet: TMySQLDataSet;
    FModified: TDateTime;
    FParameters: array of TSRoutineParameter;
    FRoutineType: TRoutineType;
    FSecurity: TSSecurity;
    FSourceParsed: Boolean;
    function GetStmt(): string;
    function GetInputDataSet(): TMySQLDataSet;
    function GetParameter(Index: Integer): TSRoutineParameter;
    function GetParameterCount(): Integer;
    function GetRoutines(): TSRoutines; inline;
    procedure ParseCreateRoutine(const SQL: string);
  protected
    procedure SetSource(const ASource: string); override;
    property SourceParsed: Boolean read FSourceParsed;
    function SQLGetSource(): string; override;
  public
    procedure Assign(const Source: TSRoutine); reintroduce; virtual;
    constructor Create(const ACDBObjects: TSDBObjects; const AName: string = ''); override;
    destructor Destroy(); override;
    function GetSourceEx(const DropBeforeCreate: Boolean = False; const EncloseDefiner: Boolean = True): string; override;
    procedure Invalidate(); override;
    function SQLRun(): string; virtual;
    property Stmt: string read GetStmt;
    property Comment: string read FComment write FComment;
    property Created: TDateTime read FCreated;
    property Definer: string read FDefiner;
    property FunctionResult: TSField read FFunctionResult;
    property InputDataSet: TMySQLDataSet read GetInputDataSet;
    property Modified: TDateTime read FModified;
    property Security: TSSecurity read FSecurity write FSecurity;
    property Source: string read GetSource write SetSource;
    property Parameter[Index: Integer]: TSRoutineParameter read GetParameter;
    property ParameterCount: Integer read GetParameterCount;
    property Routines: TSRoutines read GetRoutines;
    property RoutineType: TRoutineType read FRoutineType;
  end;

  TSProcedure = class(TSRoutine)
  protected
    procedure SetSource(const ADataSet: TMySQLQuery); overload; override;
    function SQLGetSource(): string; override;
  public
    function SQLRun(): string; override;
  end;

  TSFunction = class(TSRoutine)
  protected
    procedure SetSource(const ADataSet: TMySQLQuery); overload; override;
    function SQLGetSource(): string; override;
  public
    function SQLRun(): string; override;
  end;

  TSRoutines = class(TSDBObjects)
  private
    function GetRoutine(Index: Integer): TSRoutine;
  protected
    function Build(const DataSet: TMySQLQuery; const UseInformationSchema: Boolean; Filtered: Boolean = False; const SessionEvents: Boolean = True): Boolean; override;
    function SQLGetItems(const Name: string = ''): string; override;
  public
    procedure AddRoutine(const NewRoutine: TSRoutine); virtual;
    property Routine[Index: Integer]: TSRoutine read GetRoutine; default;
  end;

  TSTrigger = class(TSDBObject)
  type
    TEvent = (teInsert, teUpdate, teDelete);
    TTiming = (ttBefore, ttAfter);
  private
    FInputDataSet: TMySQLDataSet;
    FSourceParsed: Boolean;
    function GetInputDataSet(): TMySQLDataSet;
    function GetStmt(): string;
    function GetTable(): TSBaseTable; inline;
    function GetTriggers(): TSTriggers; inline;
    procedure ParseCreateTrigger(const SQL: string);
  protected
    FCreated: TDateTime;
    FDefiner: string;
    FEvent: TEvent;
    FStmt: string;
    FTableName: string;
    FTiming: TTiming;
    FValid: Boolean;
    procedure SetSource(const ADataSet: TMySQLQuery); overload; override;
    function SQLGetSource(): string; override;
    property SourceParsed: Boolean read FSourceParsed;
    property Valid: Boolean read FValid;
  public
    procedure Assign(const Source: TSTrigger); reintroduce; virtual;
    constructor Create(const ACDBObjects: TSDBObjects; const AName: string = ''); override;
    destructor Destroy(); override;
    function GetSourceEx(const DropBeforeCreate: Boolean = False; const EncloseDefiner: Boolean = True): string; override;
    procedure Invalidate(); override;
    function SQLDelete(): string; virtual;
    function SQLInsert(): string; virtual;
    function SQLReplace(): string; virtual;
    function SQLUpdate(): string; virtual;
    property Created: TDateTime read FCreated;
    property Definer: string read FDefiner;
    property Event: TEvent read FEvent write FEvent;
    property InputDataSet: TMySQLDataSet read GetInputDataSet;
    property Source: string read GetSource;
    property Stmt: string read GetStmt write FStmt;
    property Table: TSBaseTable read GetTable;
    property TableName: string read FTableName write FTableName;
    property Timing: TTiming read FTiming write FTiming;
    property Triggers: TSTriggers read GetTriggers;
  end;

  TSTriggers = class(TSDBObjects)
  private
    function GetTrigger(Index: Integer): TSTrigger; inline;
  protected
    function Add(const AEntity: TSEntity; const ExecuteEvent: Boolean = False): Integer; override;
    function Build(const DataSet: TMySQLQuery; const UseInformationSchema: Boolean; Filtered: Boolean = False; const SessionEvents: Boolean = True): Boolean; override;
    procedure Delete(const AEntity: TSEntity); override;
    function SQLGetItems(const Name: string = ''): string; override;
  public
    procedure Invalidate(); override;
    property Trigger[Index: Integer]: TSTrigger read GetTrigger; default;
  end;

  TSEvent = class(TSDBObject)
  private
    FCreated: TDateTime;
    FComment: string;
    FDefiner: string;
    FEnabled: Boolean;
    FEndDateTime: TDateTime;
    FEventType: TMySQLEventType;
    FExecute: TDateTime;
    FIntervalType: TMySQLIntervalType;
    FIntervalValue: string;
    FPreserve: Boolean;
    FStartDateTime: TDateTime;
    FStmt: string;
    FUpdated: TDateTime;
    function GetEvents(): TSEvents; inline;
  protected
    procedure ParseCreateEvent(const SQL: string); virtual;
    procedure SetSource(const ADataSet: TMySQLQuery); overload; override;
    procedure SetSource(const ASource: string); override;
    function SQLGetSource(): string; override;
  public
    procedure Assign(const Source: TSEvent); reintroduce; virtual;
    constructor Create(const ACDBObjects: TSDBObjects; const AName: string = ''); override;
    function GetSourceEx(const DropBeforeCreate: Boolean = False; const EncloseDefiner: Boolean = True): string; override;
    function SQLRun(): string; virtual;
    property Created: TDateTime read FCreated write FCreated;
    property Comment: string read FComment write FComment;
    property Definer: string read FDefiner write FDefiner;
    property Enabled: Boolean read FEnabled write FEnabled;
    property EndDateTime: TDateTime read FEndDateTime write FEndDateTime;
    property Events: TSEvents read GetEvents;
    property EventType: TMySQLEventType read FEventType write FEventType;
    property Execute: TDateTime read FExecute write FExecute;
    property IntervalType: TMySQLIntervalType read FIntervalType write FIntervalType;
    property IntervalValue: string read FIntervalValue write FIntervalValue;
    property Preserve: Boolean read FPreserve write FPreserve;
    property Source: string read GetSource;
    property StartDateTime: TDateTime read FStartDateTime write FStartDateTime;
    property Stmt: string read FStmt write FStmt;
    property Updated: TDateTime read FUpdated;
  end;

  TSEvents = class(TSDBObjects)
  private
    function GetEvent(Index: Integer): TSEvent;
  protected
    function Build(const DataSet: TMySQLQuery; const UseInformationSchema: Boolean; Filtered: Boolean = False; const SessionEvents: Boolean = True): Boolean; override;
    function SQLGetItems(const Name: string = ''): string; override;
  public
    property Event[Index: Integer]: TSEvent read GetEvent; default;
  end;

  TSDatabase = class(TSObject)
  private
    FDefaultCharset: string;
    FDefaultCodePage: Cardinal;
    FCollation: string;
    FEvents: TSEvents;
    FRoutines: TSRoutines;
    FTables: TSTables;
    FTriggers: TSTriggers;
    RepairTableList: TList;
    function CheckTableEvent(const DataHandle: TMySQLConnection.TDataResult; const Data: Boolean): Boolean;
    function GetDefaultCharset(): string;
    function GetCollation(): string;
    function GetCount(): Integer;
    function GetDatabases(): TSDatabases; inline;
    function GetDataSize(): Int64;
    function GetIndexSize(): Int64;
    function GetMaxDataSize(): Int64;
    function GetUpdated(): TDateTime;
    function GetValidSources(): Boolean;
    procedure ParseCreateDatabase(const SQL: string);
    procedure SetDefaultCharset(const ADefaultCharset: string);
  protected
    function Build(const DataSet: TMySQLQuery): Boolean; virtual;
    procedure FreeDesktop(); override;
    function GetCreated(): TDateTime; virtual;
    function GetSource(): string; override;
    function GetValid(): Boolean; override;
    function GetValidSource(): Boolean; override;
    function ParseDependencies(const SQL: string; var Dependencies: TSDependencies): Boolean; virtual;
    procedure SetName(const AName: string); override;
    procedure SetSource(const ADataSet: TMySQLQuery); override;
    function SQLAlterTable(const Table, NewTable: TSBaseTable; const EncloseFields: Boolean = True): string; virtual;
    function SQLGetSource(): string; virtual;
    function SQLTruncateTable(const Table: TSBaseTable): string; virtual;
    property ValidSources: Boolean read GetValidSources;
  public
    function AddEvent(const NewEvent: TSEvent): Boolean; virtual;
    function AddRoutine(const SQLCreateRoutine: string): Boolean; virtual;
    function AddTable(const NewTable: TSBaseTable): Boolean; virtual;
    function AddTrigger(const NewTrigger: TSTrigger): Boolean; virtual;
    function AddView(const NewView: TSView): Boolean; virtual;
    procedure Assign(const Source: TSObject); reintroduce; virtual;
    function BaseTableByName(const TableName: string): TSBaseTable; overload; virtual;
    function CheckTables(const Tables: TList): Boolean; virtual;
    function CloneRoutine(const Routine: TSRoutine; const NewRoutineName: string): Boolean; overload; virtual;
    function CloneTable(const Table: TSBaseTable; const NewTableName: string; const Data: Boolean): Boolean; overload; virtual;
    function CloneView(const View: TSView; const NewViewName: string): Boolean; virtual;
    constructor Create(const ASession: TSSession = nil; const AName: string = ''); reintroduce; virtual;
    function DeleteObject(const DBObject: TSDBObject): Boolean; virtual;
    destructor Destroy(); override;
    function EmptyTables(const Tables: TList = nil): Boolean; virtual;
    function EventByName(const EventName: string): TSEvent; virtual;
    function FlushTables(const Tables: TList): Boolean; virtual;
    function FunctionByName(const FunctionName: string): TSFunction; virtual;
    function GetSourceEx(const DropBeforeCreate: Boolean = False): string; virtual;
    procedure Invalidate(); override;
    function OptimizeTables(const Tables: TList): Boolean; virtual;
    procedure PushBuildEvents(); virtual;
    function ProcedureByName(const ProcedureName: string): TSProcedure;
    function RenameTable(const Table: TSTable; const NewTableName: string): Boolean; virtual;
    function RepairTables(const Tables: TList): Boolean; virtual;
    function SQLUse(): string; virtual;
    function TableByName(const TableName: string): TSTable; overload; virtual;
    function TriggerByName(const TriggerName: string): TSTrigger; virtual;
    function Unlock(): Boolean; virtual;
    function Update(const Status: Boolean = False): Boolean; reintroduce; virtual;
    function UpdateEvent(const Event, NewEvent: TSEvent): Boolean; virtual;
    function UpdateRoutine(const Routine: TSRoutine; const NewRoutine: TSRoutine): Boolean; overload; virtual;
    function UpdateRoutine(const Routine: TSRoutine; const SQLCreateRoutine: string): Boolean; overload; virtual;
    function UpdateTable(const Table, NewTable: TSBaseTable): Boolean; virtual;
    function UpdateTables(const TableNames: TStringList; const ACharset, ACollation, AEngine: string; const ARowType: TMySQLRowType): Boolean; virtual;
    function UpdateTrigger(const Trigger, NewTrigger: TSTrigger): Boolean; virtual;
    function UpdateView(const View, NewView: TSView): Boolean; virtual;
    function ViewByName(const TableName: string): TSView; overload; virtual;
    property Collation: string read GetCollation write FCollation;
    property Count: Integer read GetCount;
    property Created: TDateTime read GetCreated;
    property DataSize: Int64 read GetDataSize;
    property DefaultCharset: string read GetDefaultCharset write SetDefaultCharset;
    property DefaultCodePage: Cardinal read FDefaultCodePage;
    property Databases: TSDatabases read GetDatabases;
    property Events: TSEvents read FEvents;
    property IndexSize: Int64 read GetIndexSize;
    property MaxDataSize: Int64 read GetMaxDataSize;
    property Routines: TSRoutines read FRoutines;
    property Tables: TSTables read FTables;
    property Triggers: TSTriggers read FTriggers;
    property Updated: TDateTime read GetUpdated;
  end;

  TSSystemDatabase = class(TSDatabase)
  protected
    function GetCreated(): TDateTime; override;
  end;

  TSDatabases = class(TSObjects)
  private
    function GetDatabase(Index: Integer): TSDatabase; inline;
  protected
    function Build(const DataSet: TMySQLQuery; const UseInformationSchema: Boolean; Filtered: Boolean = False; const SessionEvents: Boolean = True): Boolean; override;
    procedure Delete(const AEntity: TSEntity); override;
    function SQLGetItems(const Name: string = ''): string; override;
  public
    function NameCmp(const Name1, Name2: string): Integer; override;
    property Database[Index: Integer]: TSDatabase read GetDatabase; default;
  end;

  TSVariable = class(TSEntity)
  type
    TUpdateMode = (vuGlobal, vuSession);
    TUpdateModes = set of TUpdateMode;
  private
    FValue: string;
    function GetAsBoolean(): Boolean;
    function GetAsFloat(): Double;
    function GetAsInteger(): Integer;
    function GetAsString(): string;
    function GetVariables(): TSVariables; inline;
    procedure SetAsBoolean(const AAsBoolean: Boolean);
    procedure SetAsFloat(const AAsFloat: Double);
    procedure SetAsInteger(const AAsInteger: Integer);
    procedure SetAsString(const AAsString: string);
  public
    procedure Assign(const Source: TSVariable); reintroduce; virtual;
    property AsBoolean: Boolean read GetAsBoolean write SetAsBoolean;
    property AsFloat: Double read GetAsFloat write SetAsFloat;
    property AsInteger: Integer read GetAsInteger write SetAsInteger;
    property AsString: string read GetAsString write SetAsString;
    property Value: string read FValue write FValue;
    property Variables: TSVariables read GetVariables;
  end;

  TSVariables = class(TSEntities)
  private
    function GetVariable(Index: Integer): TSVariable; inline;
  protected
    function Build(const DataSet: TMySQLQuery; const UseInformationSchema: Boolean; Filtered: Boolean = False; const SessionEvents: Boolean = True): Boolean; override;
    function SQLGetItems(const Name: string = ''): string; override;
  public
    property Variable[Index: Integer]: TSVariable read GetVariable; default;
  end;

  TSStatus = class(TSEntity)
  public
    Value: string;
  end;

  TSStati = class(TSEntities)
  private
    function GetStatus(Index: Integer): TSStatus;
  protected
    function Build(const DataSet: TMySQLQuery; const UseInformationSchema: Boolean; Filtered: Boolean = False; const SessionEvents: Boolean = True): Boolean; override;
    function SQLGetItems(const Name: string = ''): string; override;
  public
    property Status[Index: Integer]: TSStatus read GetStatus; default;
  end;

  TSEngine = class(TSEntity)
  private
    FComment: string;
    FDefault: Boolean;
    function GetEngines(): TSEngines; inline;
    function GetForeignKeyAllowed(): Boolean; inline;
    function GetIsInnoDB(): Boolean; inline;
    function GetIsMerge(): Boolean; inline;
    function GetIsMyISAM(): Boolean; inline;
  protected
    property Engines: TSEngines read GetEngines;
  public
    function FieldAvailable(const MySQLFieldType: TMySQLFieldType): Boolean; virtual;
    function ApplyMySQLFieldType(const MySQLFieldType: TMySQLFieldType; const MySQLFieldSize: Integer): TMySQLFieldType; virtual;
    property Comment: string read FComment write FComment;
    property Default: Boolean read FDefault;
    property ForeignKeyAllowed: Boolean read GetForeignKeyAllowed;
    property IsInnoDB: Boolean read GetIsInnoDB;
    property IsMerge: Boolean read GetIsMerge;
    property IsMyISAM: Boolean read GetIsMyISAM;
  end;

  TSSystemEngine = class(TSEngine);

  TSEngines = class(TSEntities)
  private
    function GetDefaultEngine(): TSEngine;
    function GetEngine(Index: Integer): TSEngine; inline;
  protected
    function Build(const DataSet: TMySQLQuery; const UseInformationSchema: Boolean; Filtered: Boolean = False; const SessionEvents: Boolean = True): Boolean; override;
    function SQLGetItems(const Name: string = ''): string; override;
  public
    function Update(): Boolean; override;
    property DefaultEngine: TSEngine read GetDefaultEngine;
    property Engine[Index: Integer]: TSEngine read GetEngine; default;
  end;

  TSPlugin = class(TSEntity)
  private
    function GetPlugins(): TSPlugins; inline;
  protected
    FComment: string;
  public
    property Comment: string read FComment;
    property Plugins: TSPlugins read GetPlugins;
  end;

  TSPlugins = class(TSEntities)
  private
    function GetPlugin(Index: Integer): TSPlugin;
  protected
    function Build(const DataSet: TMySQLQuery; const UseInformationSchema: Boolean; Filtered: Boolean = False; const SessionEvents: Boolean = True): Boolean; override;
    function SQLGetItems(const Name: string = ''): string; override;
  public
    property Plugin[Index: Integer]: TSPlugin read GetPlugin; default;
  end;

  TSFieldType = class
  private
    FCaption: string;
    FHighlighted: Boolean;
    FieldTypes: TSFieldTypes;
    FMySQLFieldType: TMySQLFieldType;
  public
    function DBTypeStr(): string; virtual;
    constructor Create(const AFieldTypes: TSFieldTypes; const AMySQLFieldType: TMySQLFieldType; const ACaption: string; const AHighlighted: Boolean); virtual;
    property Caption: string read FCaption;
    property Highlighted: Boolean read FHighlighted;
    property MySQLFieldType: TMySQLFieldType read FMySQLFieldType;
  end;

  TSFieldTypes = class(TSItems)
  private
    FSession: TSSession;
    procedure Add(const AMySQLFieldType: TMySQLFieldType; const ACaption: string; const AHighlighted: Boolean);
    function GetFieldType(Index: Integer): TSFieldType;
  protected
    function FieldAvailable(const Engine: TSEngine; const MySQLFieldType: TMySQLFieldType): Boolean; virtual;
  public
    function ApplyMySQLFieldType(const Engine: TSEngine; const MySQLFieldType: TMySQLFieldType): TMySQLFieldType; virtual;
    constructor Create(const ASession: TSSession); reintroduce; virtual;
    property Session: TSSession read FSession;
    property FieldType[Index: Integer]: TSFieldType read GetFieldType; default;
  end;

  TSCharset = class(TSEntity)
  private
    FHint: string;
    FCollation: string;
    FMaxLength: Integer;
    function GetCharsets(): TSCharsets; inline;
    function GetDefaultCollation(): TSCollation;
  public
    property Charsets: TSCharsets read GetCharsets;
    property Collation: string read FCollation;
    property DefaultCollation: TSCollation read GetDefaultCollation;
    property Hint: string read FHint;
    property MaxLength: Integer read FMaxLength;
  end;

  TSCharsets = class(TSEntities)
  private
    function GetCharset(Index: Integer): TSCharset;
  protected
    function Build(const DataSet: TMySQLQuery; const UseInformationSchema: Boolean; Filtered: Boolean = False; const SessionEvents: Boolean = True): Boolean; override;
    function SQLGetItems(const Name: string = ''): string; override;
  public
    function Update(): Boolean; override;
    property Charset[Index: Integer]: TSCharset read GetCharset; default;
  end;

  TSCollation = class(TSEntity)
  private
    FCharset: TSCharset;
    FCompiled: Boolean;
    FDefault: Boolean;
    FHint: string;
    FId: Word;
    FSortLength: Byte;
    function GetCollations(): TSCollations; inline;
  public
    property Charset: TSCharset read FCharset;
    property Compiled: Boolean read FCompiled;
    property Default: Boolean read FDefault;
    property Hint: string read FHint;
    property Id: Word read FId;
    property SortLength: Byte read FSortLength;
    property Collations: TSCollations read GetCollations;
  end;

  TSCollations = class(TSEntities)
  private
    function GetCollation(Index: Integer): TSCollation;
  protected
    function Build(const DataSet: TMySQLQuery; const UseInformationSchema: Boolean; Filtered: Boolean = False; const SessionEvents: Boolean = True): Boolean; override;
    function SQLGetItems(const Name: string = ''): string; override;
  public
    property Collation[Index: Integer]: TSCollation read GetCollation; default;
  end;

  TSProcess = class(TSEntity)
  private
    FCommand: string;
    FDatabaseName: string;
    FHost: string;
    FUserName: string;
    FTime: TDateTime;
    FState: string;
    FSQL: string;
    function GetThreadId(): Longword;
    procedure SetThreadId(AThreadId: Longword);
  public
    property Command: string read FCommand;
    property DatabaseName: string read FDatabaseName;
    property Host: string read FHost;
    property UserName: string read FUserName;
    property Time: TDateTime read FTime;
    property State: string read FState;
    property SQL: string read FSQL;
    property ThreadId: Longword read GetThreadId write SetThreadId;
  end;

  TSProcesses = class(TSEntities)
  private
    function GetProcess(Index: Integer): TSProcess;
  protected
    function Build(const DataSet: TMySQLQuery; const UseInformationSchema: Boolean; Filtered: Boolean = False; const SessionEvents: Boolean = True): Boolean; override;
    function GetValid(): Boolean; override;
    function SQLGetItems(const Name: string = ''): string; override;
  public
    procedure Delete(const AEntity: TSEntity); override;
    function NameCmp(const Name1, Name2: string): Integer; override;
    property Process[Index: Integer]: TSProcess read GetProcess; default;
  end;

  TSUserRight = class
  private
    FRawPassword: string;
    function GetCaption(): string;
  public
    DatabaseName: string;
    FieldName: string;
    FunctionName: string;
    NewPassword: string;
    ProcedureName: string;
    TableName: string;
    RAlter: Boolean;
    RAlterRoutine: Boolean;
    RCreate: Boolean;
    RCreateTableSpace: Boolean;
    RCreateTempTable: Boolean;
    RCreateRoutine: Boolean;
    RCreateUser: Boolean;
    RCreateView: Boolean;
    RDelete: Boolean;
    RDrop: Boolean;
    REvent: Boolean;
    RExecute: Boolean;
    RFile: Boolean;
    RGrant: Boolean;
    RIndex: Boolean;
    RInsert: Boolean;
    RLockTables: Boolean;
    RProcess: Boolean;
    RProxy: Boolean;
    RReferences: Boolean;
    RReload: Boolean;
    RReplClient: Boolean;
    RReplSlave: Boolean;
    RSelect: Boolean;
    RShowDatabases, RShowView, RShutdown, RSuper, RTrigger, RUpdate: Boolean;
    constructor Create(); virtual;
    procedure Assign(const Source: TSUserRight);
    property Caption: string read GetCaption;
    property RawPassword: string read FRawPassword;
  end;

  TSUser = class(TSObject)
  private
    FConnectionsPerHour: Int64;
    FNewPassword: string;
    FQueriesPerHour: Int64;
    FRawPassword: string;
    FRights: TList;
    FUserConnections: Int64;
    FUpdatesPerHour: Int64;
    function GetHost(): string;
    function GetLogin(): string;
    function GetRight(Index: Integer): TSUserRight; inline;
    function GetRightCount(): Integer;
    function GetSlowSQLLog(): string; inline;
    function GetSQLLog(): string; inline;
    function GetUsers(): TSUsers; inline;
    procedure ParseGrant(const SQL: string);
  protected
    function GetCaption(): string; override;
    function GetValid(): Boolean; override;
    procedure SetName(const AName: string); override;
    procedure SetSource(const ADataSet: TMySQLQuery); override;
    procedure SetSource(const ASource: string); override;
    function SQLGetSource(): string;
  public
    function AddRight(const NewUserRight: TSUserRight): Boolean;
    procedure Assign(const Source: TSUser); reintroduce;
    constructor Create(const ACItems: TSItems; const AName: string = ''); reintroduce;
    procedure DeleteRight(const UserRight: TSUserRight);
    destructor Destroy(); override;
    function IndexOf(const UserRight: TSUserRight): Integer;
    function RightByCaption(const Caption: string): TSUserRight;
    function Update(): Boolean; override;
    function UpdateRight(const UserRight,  NewUserRight: TSUserRight): Boolean;
    property ConnectionsPerHour: Int64 read FConnectionsPerHour write FConnectionsPerHour;
    property Host: string read GetHost;
    property Login: string read GetLogin;
    property NewPassword: string read FNewPassword write FNewPassword;
    property QueriesPerHour: Int64 read FQueriesPerHour write FQueriesPerHour;
    property RawPassword: string read FRawPassword write FRawPassword;
    property Right[Index: Integer]: TSUserRight read GetRight;
    property RightCount: Integer read GetRightCount;
    property SlowSQLLog: string read GetSlowSQLLog;
    property SQLLog: string read GetSQLLog;
    property UpdatesPerHour: Int64 read FUpdatesPerHour write FUpdatesPerHour;
    property UserConnections: Int64 read FUserConnections write FUserConnections;
    property Users: TSUsers read GetUsers;
  end;

  TSUsers = class(TSObjects)
  private
    function GetUser(Index: Integer): TSUser; inline;
  protected
    function Build(const DataSet: TMySQLQuery; const UseInformationSchema: Boolean; Filtered: Boolean = False; const SessionEvents: Boolean = True): Boolean; override;
    function GetValid(): Boolean; override;
    function SQLGetItems(const Name: string = ''): string; override;
  public
    property User[Index: Integer]: TSUser read GetUser; default;
  end;

  TSSession = class(TMySQLConnection)
  type
    TEventType = (etItemsValid, etItemValid, etItemCreated, etItemDropped, etItemAltered, etBeforeExecuteSQL, etAfterExecuteSQL, etMonitor, etError);
    TUpdate = function (): Boolean of object;
    TEvent = class
    public
      Session: TSSession;
      EventType: TEventType;
      Sender: TObject;
      SItems: TSItems;
      SItem: TSItem;
      Update: TUpdate;
      constructor Create(const ASession: TSSession);
    end;
    TCreateDesktop = function (const CObject: TSObject): TSObject.TDesktop of Object;
    TEventProc = procedure (const AEvent: TEvent) of object;
  private
    AutoCommitBeforeTransaction: Boolean;
    EventProcs: TList;
    FAccount: TAAccount;
    FCharsets: TSCharsets;
    FSessions: TSSessions;
    FCollations: TSCollations;
    FCreateDesktop: TCreateDesktop;
    FCurrentUser: string;
    FDatabases: TSDatabases;
    FEngines: TSEngines;
    FFieldTypes: TSFieldTypes;
    FInformationSchema: TSDatabase;
    FInvalidObjects: TList;
    FMetadataProvider: TacEventMetadataProvider;
    FPerformanceSchema: TSDatabase;
    FPlugins: TSPlugins;
    FProcesses: TSProcesses;
    FStati: TSStati;
    FSQLMonitor: TMySQLMonitor;
    FStartTime: TDateTime;
    FSyntaxProvider: TacMYSQLSyntaxProvider;
    FUser: TSUser;
    FUsers: TSUsers;
    FVariables: TSVariables;
    StmtMonitor: TMySQLMonitor;
    function BuildEvents(const DataSet: TMySQLQuery): Boolean; {$IFNDEF Debug} inline; {$ENDIF}
    function BuildRoutines(const DataSet: TMySQLQuery): Boolean; {$IFNDEF Debug} inline; {$ENDIF}
    function BuildTables(const DataSet: TMySQLQuery): Boolean; {$IFNDEF Debug} inline; {$ENDIF}
    function BuildTriggers(const DataSet: TMySQLQuery): Boolean; {$IFNDEF Debug} inline; {$ENDIF}
    procedure ConnectChange(Sender: TObject; Connecting: Boolean);
    procedure DoExecuteEvent(const AEvent: TEvent);
    function GetCaption(): string;
    function GetCollation(): string;
    function GetDefaultCharset(): string;
    function GetLogActive(): Boolean;
    function GetSlowLogActive(): Boolean;
    function GetUserRights(): TSUserRight;
    function GetValid(): Boolean;
    procedure SetCreateDesktop(ACreateDesktop: TCreateDesktop);
  protected
    FLowerCaseTableNames: Byte;
    FMaxAllowedPacket: Integer;
    procedure BuildUser(const DataSet: TMySQLQuery);
    procedure DoAfterExecuteSQL(); override;
    procedure DoBeforeExecuteSQL(); override;
    procedure ExecuteEvent(const EventType: TEventType); overload;
    procedure ExecuteEvent(const EventType: TEventType; const Sender: TObject; const SItems: TSItems = nil; const SItem: TSItem = nil); overload;
    function GetAutoCommit(): Boolean; override;
    function GetDataFileAllowed(): Boolean; override;
    function GetLog(): string;
    function GetMaxAllowedPacket(): Integer; override;
    function GetSlowLog(): string;
    function GetSlowSQLLog(const User: TSUser = nil): string;
    function GetSQLLog(const User: TSUser = nil): string;
    procedure MonitorLog(const Sender: TObject; const Text: PChar; const Len: Integer; const ATraceType: TMySQLMonitor.TTraceType);
    procedure MonitorExecutedStmts(const Sender: TObject; const Text: PChar; const Len: Integer; const ATraceType: TMySQLMonitor.TTraceType);
    function SessionResult(const DataHandle: TMySQLConnection.TDataResult; const Data: Boolean): Boolean;
    procedure SetAutoCommit(const AAutoCommit: Boolean); override;
    procedure SetCharset(const ACharset: string); override;
    property Sessions: TSSessions read FSessions;
    property InvalidObjects: TList read FInvalidObjects;
  public
    function AddDatabase(const NewDatabase: TSDatabase): Boolean;
    function AddUser(const ANewUser: TSUser): Boolean;
    function ApplyIdentifierName(const AIdentifierName: string): string;
    procedure GridCanEditShow(Sender: TObject);
    function CharsetByName(const CharsetName: string): TSCharset;
    function CharsetByCollation(const Collation: string): TSCharset;
    function CollationByName(const CollationName: string): TSCollation;
    procedure CommitTransaction(); override;
    procedure FirstConnect(); overload;
    procedure FirstConnect(const AConnectionType: Integer; const ALibraryName: string; const AHost, AUser, APassword, ADatabase: string; const APort: Integer; const AAsynchron: Boolean); overload;
    constructor Create(const ASessions: TSSessions; const AAccount: TAAccount = nil); reintroduce;
    function DatabaseByName(const DatabaseName: string): TSDatabase;
    procedure DecodeInterval(const Value: string; const IntervalType: TMySQLIntervalType; var Year, Month, Day, Quarter, Week, Hour, Minute, Second, MSec: Word);
    function DeleteDatabase(const Database: TSDatabase): Boolean;
    function DeleteEntities(const List: TList): Boolean;
    function DeleteProcess(const Process: TSProcess): Boolean;
    function DeleteUser(const User: TSUser): Boolean;
    function DeleteUsers(const List: TList): Boolean;
    destructor Destroy(); override;
    procedure EmptyDatabases(const Databases: TList);
    function EncodeInterval(const Year, Month, Day, Quarter, Week, Hour, Minute, Second, MSec: Word; var Value: string; var IntervalType: TMySQLIntervalType): Boolean;
    function EngineByName(const EngineName: string): TSEngine;
    function EscapeRightIdentifier(const Identifier: string; const IdentifierQuoting: Boolean = False): string;
    function EscapeUser(const User: string; const IdentifierQuoting: Boolean = False): string;
    function FieldTypeByCaption(const Caption: string): TSFieldType;
    function FieldTypeByMySQLFieldType(const MySQLFieldType: TMySQLFieldType): TSFieldType;
    function FlushHosts(): Boolean;
    procedure UpdateIndexDefs(const DataSet: TMySQLQuery; const IndexDefs: TIndexDefs);
    procedure Invalidate();
    function PluginByName(const PluginName: string): TSPlugin;
    function ProcessByThreadId(const ThreadId: Longword): TSProcess;
    procedure RegisterEventProc(const AEventProc: TEventProc);
    procedure RollbackTransaction(); override;
    procedure StartTransaction(); override;
    function StatusByName(const StatusName: string): TSStatus;
    function TableName(const Name: string): string;
    function TableNameCmp(const Name1, Name2: string): Integer; inline;
    function UnescapeValue(const Value: string; const FieldType: TMySQLFieldType = mfVarChar): string; overload;
    function UnecapeRightIdentifier(const Identifier: string): string;
    function Update(): Boolean; overload;
    function Update(const Objects: TList; const Status: Boolean = False): Boolean; overload;
    function UpdateDatabase(const Database, NewDatabase: TSDatabase): Boolean;
    function UpdateUser(const User, NewUser: TSUser): Boolean;
    function UpdateVariable(const Variable, NewVariable: TSVariable; const UpdateModes: TSVariable.TUpdateModes): Boolean;
    procedure UnRegisterEventProc(const AEventProc: TEventProc);
    function UserByCaption(const Caption: string): TSUser;
    function UserByName(const UserName: string): TSUser;
    function VariableByName(const VariableName: string): TSVariable;
    property Account: TAAccount read FAccount;
    property Caption: string read GetCaption;
    property Charsets: TSCharsets read FCharsets;
    property Collation: string read GetCollation;
    property Collations: TSCollations read FCollations;
    property CreateDesktop: TCreateDesktop read FCreateDesktop write SetCreateDesktop;
    property CurrentUser: string read FCurrentUser;
    property Databases: TSDatabases read FDatabases;
    property DefaultCharset: string read GetDefaultCharset;
    property Engines: TSEngines read FEngines;
    property FieldTypes: TSFieldTypes read FFieldTypes;
    property InformationSchema: TSDatabase read FInformationSchema;
    property Log: string read GetLog;
    property LogActive: Boolean read GetLogActive;
    property LowerCaseTableNames: Byte read FLowerCaseTableNames;
    property MetadataProvider: TacEventMetadataProvider read FMetadataProvider;
    property PerformanceSchema: TSDatabase read FPerformanceSchema;
    property Plugins: TSPlugins read FPlugins;
    property Processes: TSProcesses read FProcesses;
    property SlowLog: string read GetSlowLog;
    property SlowLogActive: Boolean read GetSlowLogActive;
    property StartTime: TDateTime read FStartTime;
    property Stati: TSStati read FStati;
    property SQLMonitor: TMySQLMonitor read FSQLMonitor;
    property SyntaxProvider: TacMYSQLSyntaxProvider read FSyntaxProvider;
    property User: TSUser read FUser;
    property UserRights: TSUserRight read GetUserRights;
    property Users: TSUsers read FUsers;
    property Valid: Boolean read GetValid;
    property Variables: TSVariables read FVariables;
  end;

  TSSessions = class(TList)
  private
    FOnSQLError: TMySQLConnection.TErrorEvent;
    function GetSession(Index: Integer): TSSession; inline;
  public
    function Add(const Session: TSSession): Integer;
    function SessionByAccount(const Account: TAAccount; const DatabaseName: string): TSSession;
    property Session[Index: Integer]: TSSession read GetSession; default;
    property OnSQLError: TMySQLConnection.TErrorEvent read FOnSQLError write FOnSQLError;
  end;

const
  DefaultLimit = 100;
  DefaultLimitSize = 50 * 1024;

var
  Sessions: TSSessions;

implementation {***************************************************************}

uses
  Variants, SysConst, WinInet, DBConsts, RTLConsts, Math,
  Consts, DBCommon, StrUtils,
  Forms, DBGrids,
  MySQLConsts, CSVUtils, HTTPTunnel, MySQLDBGrid,
  fURI;

const
  INFORMATION_SCHEMA = 'INFORMATION_SCHEMA';
  PERFORMANCE_SCHEMA = 'PERFORMANCE_SCHEMA';
  OutParameterCaption = '<OUT>';
  InParameterCaption = '<IN>';

resourcestring
  SUnknownRoutineType = 'Unknown Routine type (%s)';
  SUnknownSQLStmt = 'Unknow SQL Stmt (%s)';
  SUnknownEngineType = 'Unknown Engine type (%s)';
  SSourceParseError = 'Source code of "%s" cannot be analyzed:' + #10#10 + '%s';

type
  TMCharsetTranslation = record
    OldCharset: PChar;
    NewCharset: PChar;
    NewCollation: PChar;
  end;
const
  CharsetTranslations: array[0..29] of TMCharsetTranslation = (
    (OldCharset: 'big5';       NewCharset: 'big5';   NewCollation: 'big5_chinese_ci'),
    (OldCharset: 'czech';      NewCharset: 'latin2'; NewCollation: 'latin2_czech_ci'),
    (OldCharset: 'dec8';       NewCharset: 'dec8';   NewCollation: 'dec8_swedish_ci'),
    (OldCharset: 'dos';        NewCharset: 'cp850';  NewCollation: 'cp850_general_ci'),
    (OldCharset: 'german1';    NewCharset: 'latin1'; NewCollation: 'latin1_german1_ci'),
    (OldCharset: 'hp8';        NewCharset: 'hp8';    NewCollation: 'hp8_english_ci'),
    (OldCharset: 'koi8_ru';    NewCharset: 'koi8r';  NewCollation: 'koi8r_general_ci'),
    (OldCharset: 'latin1';     NewCharset: 'latin1'; NewCollation: 'latin1_swedish_ci'),
    (OldCharset: 'latin2';     NewCharset: 'latin2'; NewCollation: 'latin2_general_ci'),
    (OldCharset: 'swe7';       NewCharset: 'swe7';   NewCollation: 'swe7_swedish_ci'),
    (OldCharset: 'usa7';       NewCharset: 'ascii';  NewCollation: 'ascii_general_ci'),
    (OldCharset: 'ujis';       NewCharset: 'ujis';   NewCollation: 'ujis_japanese_ci'),
    (OldCharset: 'sjis';       NewCharset: 'sjis';   NewCollation: 'sjis_japanese_ci'),
    (OldCharset: 'cp1251';     NewCharset: 'cp1251'; NewCollation: 'cp1251_bulgarian_ci'),
    (OldCharset: 'danish';     NewCharset: 'latin1'; NewCollation: 'latin1_danish_ci'),
    (OldCharset: 'hebrew';     NewCharset: 'hebrew'; NewCollation: 'hebrew_general_ci'),
    (OldCharset: 'cp1251';     NewCharset: 'cp1251'; NewCollation: 'cp1251_bulgarian_ci'),
    (OldCharset: 'euc_kr';     NewCharset: 'euckr';  NewCollation: 'euckr_korean_ci'),
    (OldCharset: 'estonia';    NewCharset: 'latin7'; NewCollation: 'latin7_estonian_ci'),
    (OldCharset: 'hungarian';  NewCharset: 'latin2'; NewCollation: 'latin2_hungarian_ci'),
    (OldCharset: 'koi8_ukr';   NewCharset: 'koi8u';  NewCollation: 'koi8u_ukrainian_ci'),
    (OldCharset: 'win1251ukr'; NewCharset: 'cp1251'; NewCollation: 'cp1251_ukrainian_ci'),
    (OldCharset: 'gb2312';     NewCharset: 'gb2312'; NewCollation: 'gb2312_chinese_ci'),
    (OldCharset: 'greek';      NewCharset: 'greek';  NewCollation: 'greek_general_ci'),
    (OldCharset: 'win1250';    NewCharset: 'cp1250'; NewCollation: 'cp1250_general_ci'),
    (OldCharset: 'croat';      NewCharset: 'latin2'; NewCollation: 'latin2_croatian_ci'),
    (OldCharset: 'gbk';        NewCharset: 'gbk';    NewCollation: 'gbk_chinese_ci'),
    (OldCharset: 'cp1257';     NewCharset: 'cp1257'; NewCollation: 'cp1257_lithuanian_ci'),
    (OldCharset: 'latin5';     NewCharset: 'latin5'; NewCollation: 'latin5_turkish_ci'),
    (OldCharset: 'latin1_de';  NewCharset: 'latin1'; NewCollation: 'latin1_german2_ci')
  );

function StrToMySQLRowType(const Str: string): TMySQLRowType;
begin
  if (UpperCase(Str) = 'FIXED') then Result := mrFixed
  else if (UpperCase(Str) = 'DYNAMIC') then Result := mrDynamic
  else if (UpperCase(Str) = 'COMPRESSED') then Result := mrCompressed
  else if (UpperCase(Str) = 'REDUNDANT') then Result := mrRedundant
  else if (UpperCase(Str) = 'COMPACT') then Result := mrCompact
  else Result := mrUnknown;
end;

function StrToPartitionType(const Str: string): TMySQLPartitionType;
begin
  if (UpperCase(Str) = 'HASH') then Result := ptHash
  else if (UpperCase(Str) = 'KEY') then Result := ptKey
  else if (UpperCase(Str) = 'RANGE') then Result := ptRange
  else if (UpperCase(Str) = 'LIST') then Result := ptList
  else Result := ptNone;
end;

function StrToEventType(const Str: string): TMySQLEventType;
begin
  if (UpperCase(Str) = 'ONE TIME') then Result := etSingle
  else if (UpperCase(Str) = 'RECURRING') then Result := etMultiple
  else Result := etUnknown;
end;

function StrToIntervalType(const Str: string): TMySQLIntervalType;
begin
  if (UpperCase(Str) = 'YEAR') then Result := itYear
  else if (UpperCase(Str) = 'QUARTER') then Result := itQuarter
  else if (UpperCase(Str) = 'MONTH') then Result := itMonth
  else if (UpperCase(Str) = 'DAY') then Result := itDay
  else if (UpperCase(Str) = 'HOUR') then Result := itHour
  else if (UpperCase(Str) = 'MINUTE') then Result := itMinute
  else if (UpperCase(Str) = 'WEEK') then Result := itWeek
  else if (UpperCase(Str) = 'SECOND') then Result := itSecond
  else if (UpperCase(Str) = 'MICROSECOND') then Result := itMicrosecond
  else if (UpperCase(Str) = 'YEAR_MONTH') then Result := itYearMonth
  else if (UpperCase(Str) = 'DAY_HOUR') then Result := itDayMinute
  else if (UpperCase(Str) = 'DAY_MINUTE') then Result := itDayMinute
  else if (UpperCase(Str) = 'DAY_SECOND') then Result := itDaySecond
  else if (UpperCase(Str) = 'HOUR_MINUTE') then Result := itHourMinute
  else if (UpperCase(Str) = 'HOUR_SECOND') then Result := itHourSecond
  else if (UpperCase(Str) = 'MINUTE_SECOND') then Result := itMinuteSecond
  else if (UpperCase(Str) = 'DAY_MICROSECOND') then Result := itDayMicrosecond
  else if (UpperCase(Str) = 'HOUR_MICROSECOND') then Result := itHourMicrosecond
  else if (UpperCase(Str) = 'MINUTE_MICROSECOND') then Result := itMinuteMicrosecond
  else if (UpperCase(Str) = 'SECOND_MICROSECOND') then Result := itSecondMicrosecond
  else Result := itUnknown;
end;

function IntervalToStr(const IntervalValue: string; const IntervalType: TMySQLIntervalType): string;
begin
  case (IntervalType) of
    itYear: Result := 'YEAR';
    itQuarter: Result := 'QUARTER';
    itMonth: Result := 'MONTH';
    itDay: Result := 'DAY';
    itHour: Result := 'HOUR';
    itMinute: Result := 'MINUTE';
    itWeek: Result := 'WEEK';
    itSecond: Result := 'SECOND';
    itMicrosecond: Result := 'MICROSECOND';
    itYearMonth: Result := 'YEAR_MONTH';
    itDayHour: Result := 'DAY_HOUR';
    itDayMinute: Result := 'DAY_MINUTE';
    itDaySecond: Result := 'DAY_SECOND';
    itHourMinute: Result := 'HOUR_MINUTE';
    itHourSecond: Result := 'HOUR_SECOND';
    itMinuteSecond: Result := 'MINUTE_SECOND';
    itDayMicrosecond: Result := 'DAY_MICROSECOND';
    itHourMicrosecond: Result := 'HOUR_MICROSECOND';
    itMinuteMicrosecond: Result := 'MINUTE_MICROSECOND';
    itSecondMicrosecond: Result := 'SECOND_MICROSECOND';
    else Result := '';
  end;
  if (Result <> '') then
    Result := '''' + IntervalValue + ''' ' + Result;
end;

{ TSResultSet *****************************************************************}

{ TSItem **********************************************************************}

procedure TSItem.Assign(const Source: TSItem);
begin
  Assert(Assigned(Source) and (Source.ClassType = ClassType));


  FName := Source.Name;
end;

constructor TSItem.Create(const ACItems: TSItems; const AName: string = '');
begin
  inherited Create();

  FCItems := ACItems;
  FName := AName;
end;

function TSItem.Equal(const Second: TSItem): Boolean;
begin
  Result := Assigned(Second) and (Second.ClassType = ClassType);
end;

function TSItem.GetCaption(): string;
begin
  Result := Name;
end;

function TSItem.GetIndex(): Integer;
begin
  Result := CItems.IndexOf(Self);
end;

procedure TSItem.SetName(const AName: string);
var
  NewIndex: Integer;
begin
  if (AName <> FName) then
  begin
    if (CItems.InsertIndex(AName, NewIndex) and (Index >= 0)) then
    begin
      if (NewIndex > Index) then
        Dec(NewIndex);
      CItems.Move(Index, NewIndex);
    end;
    FName := AName;
  end;
end;

{ TSDBItems *******************************************************************}

procedure TSItems.Clear();
var
  I: Integer;
begin
  for I := 0 to TList(Self).Count - 1 do
    TSItem(Items[I]).Free();

  inherited;
end;

constructor TSItems.Create(const ASession: TSSession);
begin
  inherited Create();

  FSession := ASession;
end;

destructor TSItems.Destroy();
begin
  Clear();

  inherited;
end;

function TSItems.GetItem(Index: Integer): TSItem;
begin
  Assert(TObject(Items[Index]) is TSItem);

  Result := TSItem(Items[Index]);
end;

function TSItems.GetCount(): Integer;
begin
  Result := TList(Self).Count;
end;

function TSItems.IndexByName(const Name: string): Integer;
type
  Tstrcmp = function (lpString1, lpString2: PWideChar): Integer; stdcall;
var
  Left: Integer;
  Mid: Integer;
  Right: Integer;
  strcmp: Tstrcmp;
begin
  Result := -1;

  if (((Self is TSTables) or (Self is TSDatabases)) and (Session.LowerCaseTableNames = 0)) then
    strcmp := lstrcmp
  else
    strcmp := lstrcmpi;

  Left := 0;
  Right := Count - 1;
  while (Left <= Right) do
  begin
    Mid := (Right - Left) div 2 + Left;
    case (strcmp(PChar(Item[Mid].Name), PChar(Name))) of
      -1: Left := Mid + 1;
      0: begin Result := Mid; break; end;
      1: Right := Mid - 1;
    end;
  end;
end;

function TSItems.InsertIndex(const Name: string; out Index: Integer): Boolean;
type
  Tstrcmp = function (lpString1, lpString2: PWideChar): Integer; stdcall;
var
  Left: Integer;
  Mid: Integer;
  Right: Integer;
  strcmp: Tstrcmp;
begin
  Result := True;

  if (((Self is TSTables) or (Self is TSDatabases)) and (Session.LowerCaseTableNames = 0)) then
    strcmp := lstrcmp
  else
    strcmp := lstrcmpi;

  if ((TList(Self).Count = 0) or (strcmp(PChar(Item[Count - 1].Name), PChar(Name)) < 0)) then
    Index := TList(Self).Count
  else
  begin
    Left := 0;
    Right := TList(Self).Count - 1;
    while (Left <= Right) do
    begin
      Mid := (Right - Left) div 2 + Left;
      case (strcmp(PChar(Item[Mid].Name), PChar(Name))) of
        -1: begin Left := Mid + 1;  Index := Mid + 1; end;
        0: begin Result := False; Index := Mid; break; end;
        1: begin Right := Mid - 1; Index := Mid; end;
      end;
    end;
  end;
end;

function TSItems.NameCmp(const Name1, Name2: string): Integer;
begin
  Result := lstrcmpi(PChar(Name1), PChar(Name2));
end;

{ TSEntity ********************************************************************}

function TSEntity.GetEntities(): TSEntities;
begin
  Assert(CItems is TSEntities);

  Result := TSEntities(CItems);
end;

{ TSEntities ******************************************************************}

function TSEntities.Add(const AEntity: TSEntity; const ExecuteEvent: Boolean): Integer;
begin
  if (InsertIndex(AEntity.Name, Result)) then
    if (Result < TList(Self).Count) then
      TList(Self).Insert(Result, AEntity)
    else
      TList(Self).Add(AEntity);
end;

function TSEntities.Build(const DataSet: TMySQLQuery; const UseInformationSchema: Boolean; Filtered: Boolean = False; const SessionEvents: Boolean = True): Boolean;
begin
  FValid := True;

  Result := False;
end;

constructor TSEntities.Create(const ASession: TSSession);
begin
  inherited Create(ASession);

  FSession := ASession;

  FValid := False;
end;

procedure TSEntities.Delete(const AEntity: TSEntity);
var
  Index: Integer;
begin
  Index := IndexOf(AEntity);

  if (Index >= 0) then
  begin
    TList(Self).Delete(Index);

    AEntity.Free();
  end;
end;

function TSEntities.GetValid(): Boolean;
begin
  Result := FValid;
end;

procedure TSEntities.Invalidate();
begin
  FValid := False;
end;

procedure TSEntities.PushBuildEvent(const Sender: TObject);
begin
  Session.ExecuteEvent(etItemsValid, Sender, Self);
end;

function TSEntities.Update(): Boolean;
var
  Objects: TList;
begin
  Objects := TList.Create();
  Objects.Add(Self);
  Result := Session.Update(Objects);
  Objects.Free();
end;

{ TSObject ********************************************************************}

constructor TSObject.TDesktop.Create(const ACObject: TSObject);
begin
  inherited Create();

  FCObject := ACObject;
end;

{ TSObject ********************************************************************}

procedure TSObject.Assign(const Source: TSObject);
begin
  Assert(Assigned(Source));

  inherited Assign(TSItem(Source));

  FInvalid := Source.Invalid;
  FSource := Source.Source;
  FValidSource := Source.ValidSource;
end;

constructor TSObject.Create(const ACItems: TSItems; const AName: string = '');
begin
  inherited Create(ACItems, AName);

  FInvalid := False;
  FSession := ACItems.Session;
  FSource := '';
  FValidSource := False;

  FDesktop := nil;
end;

destructor TSObject.Destroy();
begin
  if (Assigned(FDesktop)) then
    FDesktop.Free();

  if (Assigned(Session.InvalidObjects) and (Session.InvalidObjects.IndexOf(Self) > 0)) then
    Session.InvalidObjects.Delete(Session.InvalidObjects.IndexOf(Self));

  inherited;
end;

procedure TSObject.FreeDesktop();
begin
  if (Assigned(FDesktop)) then
    FreeAndNil(FDesktop);
end;

function TSObject.GetDesktop(): TDesktop;
begin
  if (not Assigned(FDesktop) and Assigned(Session.CreateDesktop)) then
    FDesktop := Session.CreateDesktop(Self);

  Result := FDesktop;
end;

function TSObject.GetObjects(): TSObjects;
begin
  Assert(CItems is TSObjects);

  Result := TSObjects(CItems);
end;

function TSObject.GetSource(): string;
begin
  Result := FSource;
end;

function TSObject.GetValid(): Boolean;
begin
  Result := ValidSource;
end;

function TSObject.GetValidSource(): Boolean;
begin
  Result := FValidSource;
end;

procedure TSObject.Invalidate();
begin
  if (Valid and Assigned(Session.InvalidObjects) and (Session.InvalidObjects.IndexOf(Self) < 0)) then
    Session.InvalidObjects.Add(Self);

  FInvalid := False;
  FSource := '';
  FValidSource := False;
end;

procedure TSObject.SetName(const AName: string);
begin
  inherited;

  FValidSource := False;
  FSource := '';
end;

procedure TSObject.SetSource(const AField: TField);
var
  SQL: string;
begin
  if (AField.DataType = ftBlob) then
    SQL := Trim(Session.LibDecode(my_char(AField.AsAnsiString)))
  else
    SQL := Trim(AField.AsString);
  if ((SQL <> '') and (Copy(SQL, Length(SQL), 1) <> ';')) then
    SQL := SQL + ';';
  SetSource(SQL);
end;

procedure TSObject.SetSource(const ASource: string);
begin
  FValidSource := True;
  FSource := ASource;
end;

function TSObject.Update(): Boolean;
begin
  if (not (Self is TSDatabase)) then
    raise EAbstractError.Create(SAbstractError)
  else
    Result := TSDatabase(Self).Update();
end;

{ TSObjects *******************************************************************}

function TSObjects.Add(const AEntity: TSEntity; const ExecuteEvent: Boolean): Integer;
begin
  Assert(AEntity is TSObject);

  if (InsertIndex(AEntity.Name, Result)) then
    if (Result < TList(Self).Count) then
      TList(Self).Insert(Result, AEntity)
    else
      TList(Self).Add(AEntity);

  if (ExecuteEvent) then
  begin
    TSObject(AEntity).Invalidate();
    Session.InvalidObjects.Add(AEntity);
    Session.ExecuteEvent(etItemCreated, Session, Self, AEntity);
  end;
end;

procedure TSObjects.Delete(const AEntity: TSEntity);
var
  Index: Integer;
begin
  Assert(AEntity is TSObject);

  Index := IndexOf(AEntity);

  if (Index >= 0) then
  begin
    Session.ExecuteEvent(etItemDropped, Session, Self, AEntity);

    TList(Self).Delete(Index);

    AEntity.Free();
  end;
end;

procedure TSObjects.Invalidate();
var
  I: Integer;
begin
  inherited;

  for I := 0 to Count - 1 do
    TSObject(Items[I]).Invalidate();
end;

{ TSDependency ****************************************************************}

function TSDependency.GetDBObject(): TSDBObject;
var
  Database: TSDatabase;
begin
  Database := Session.DatabaseByName(DatabaseName);

  Result := nil;
  if (Assigned(Database)) then
    if ((ObjectClass = TSBaseTable) or (ObjectClass = TSSystemView) or (ObjectClass = TSView) or (ObjectClass = TSTable)) then
      Result := Database.TableByName(ObjectName)
    else if (ObjectClass = TSRoutine) then
      Result := Database.ProcedureByName(ObjectName)
    else if (ObjectClass = TSFunction) then
      Result := Database.FunctionByName(ObjectName)
    else
      raise ERangeError.Create(SRangeError);
end;

{ TSDependencies **************************************************************}

function TSDependencies.Add(Item: Pointer): Integer;
var
  I: Integer;
begin
  Assert(TObject(Item) is TSDependency);

  for I := 0 to Count - 1 do
    if (Assigned(Item)
      and (TSDependency(Item).DatabaseName = TSDependency(Items[I]).DatabaseName)
      and (TSDependency(Item).ObjectClass = TSDependency(Items[I]).ObjectClass)
      and (TSDependency(Item).ObjectName = TSDependency(Items[I]).ObjectName)) then
      FreeAndNil(Item);

  if (not Assigned(Item)) then
    Result := -1
  else
    Result := TList(Self).Add(Item);
end;

destructor TSDependencies.Destroy();
var
  I: Integer;
begin
  for I := 0 to Count - 1 do
    TSDependency(Items[I]).Free();

  inherited;
end;

function TSDependencies.GetDependency(Index: Integer): TSDependency;
begin
  Result := TSDependency(Items[Index]);
end;

{ TSDBObject ******************************************************************}

constructor TSDBObject.Create(const ACDBObjects: TSDBObjects; const AName: string = '');
begin
  inherited Create(ACDBObjects, AName);

  FDatabase := ACDBObjects.Database;
  FDependencies := nil;
end;

destructor TSDBObject.Destroy();
begin
  if (Assigned(FDependencies)) then
    FDependencies.Free();

  inherited;
end;

function TSDBObject.GetDBObjects(): TSDBObjects;
begin
  Assert(CItems is TSDBObjects);

  Result := TSDBObjects(CItems);
end;

function TSDBObject.GetDependencies(): TSDependencies;
begin
  if (not Assigned(FDependencies)) then
  begin
    FDependencies := TSDependencies.Create();
    Database.ParseDependencies(GetSourceEx(False, False), FDependencies);
  end;

  Result := FDependencies;
end;

procedure TSDBObject.Invalidate();
begin
  if (Assigned(FDependencies)) then
    FreeAndNil(FDependencies);

  inherited;
end;

procedure TSDBObject.PushBuildEvent(const CItemsEvents: Boolean = True);
begin
  if (Valid) then
  begin
    if (CItemsEvents) then
      Session.ExecuteEvent(etItemsValid, Database, CItems);
    Session.ExecuteEvent(etItemValid, Database, CItems, Self);
  end;
end;

procedure TSDBObject.SetDatabase(const ADatabase: TSDatabase);
begin
  if (ADatabase <> FDatabase) then
  begin
    Entities.Delete(Self);
    if (Self is TSTable) then
      FCItems := ADatabase.Tables
    else if (Self is TSRoutine) then
      FCItems := ADatabase.Routines
    else if (Self is TSRoutine) then
      FCItems := ADatabase.Routines
    else if (Self is TSTrigger) then
      FCItems := ADatabase.Triggers
    else
      raise ERangeError.Create(SRangeError);
    Entities.Add(Self, True);
  end;
end;

procedure TSDBObject.SetSource(const ASource: string);
begin
  inherited;

  PushBuildEvent();
end;

function TSDBObject.Update(): Boolean;
var
  List: TList;
begin
  List := TList.Create();
  List.Add(Self);
  Result := Session.Update(List);
  List.Free();
end;

{ TSDBObjects *****************************************************************}

function TSDBObjects.Add(const AEntity: TSEntity; const ExecuteEvent: Boolean = False): Integer;
begin
  Assert(AEntity is TSObject);

  if (InsertIndex(AEntity.Name, Result)) then
    if (Result < TList(Self).Count) then
      TList(Self).Insert(Result, AEntity)
    else
      TList(Self).Add(AEntity);

  if (ExecuteEvent) then
  begin
    TSObject(AEntity).Invalidate();
    Session.InvalidObjects.Add(AEntity);
    Session.ExecuteEvent(etItemCreated, Database, Self, AEntity);
  end;
end;

constructor TSDBObjects.Create(const ADatabase: TSDatabase);
begin
  inherited Create(ADatabase.Session);

  FDatabase := ADatabase;
end;

procedure TSDBObjects.Delete(const AEntity: TSEntity);
var
  Index: Integer;
begin
  Assert(AEntity is TSObject);

  Index := IndexOf(AEntity);

  if (Index >= 0) then
  begin
    Session.ExecuteEvent(etItemDropped, Database, Self, AEntity);

    TList(Self).Delete(Index);

    AEntity.Free();
  end;
end;

{ TSIndexColumn ***************************************************************}

procedure TSKeyColumn.Assign(const Source: TSKeyColumn);
begin
  Field := IndexColumns.Key.Table.FieldByName(Source.Field.Name);
  Length := Source.Length;
end;

constructor TSKeyColumn.Create(const AKeyColumns: TSKeyColumns);
begin
  FKeyColumns := AKeyColumns;

  Ascending := True;
  Field := nil;
  Length := 0;
end;

function TSKeyColumn.Equal(const Second: TSKeyColumn): Boolean;
begin
  Result := Assigned(Second) and (ClassType = Second.ClassType);

  Result := Result and (lstrcmpi(PChar(Field.Name), PChar(Second.Field.Name)) = 0);
  Result := Result and (Length = Second.Length);
end;

{ TSIndexColumns **************************************************************}

procedure TSKeyColumns.AddColumn(const NewColumn: TSKeyColumn);
begin
  Add(TSKeyColumn.Create(Self));
  Column[TList(Self).Count - 1].Assign(NewColumn);
end;

constructor TSKeyColumns.Create(const AKey: TSKey);
begin
  FKey := AKey;
end;

procedure TSKeyColumns.DeleteColumn(const AColumn: TSKeyColumn);
begin
  Delete(IndexOf(AColumn));
end;

function TSKeyColumns.KeyByField(const AField: TSTableField): Integer;
var
  I: Integer;
begin
  Result := -1;

  for I := 0 to Count - 1 do
    if (AField = Column[I].Field) then
      Result := I;
end;

function TSKeyColumns.GetColumn(Index: Integer): TSKeyColumn;
begin
  Result := TSKeyColumn(Items[Index]);
end;

{ TSIndex *********************************************************************}

procedure TSKey.Assign(const Source: TSKey);
var
  I: Integer;
begin
  inherited Assign(Source);

  OriginalName := Source.OriginalName;

  BlockSize := Source.BlockSize;
  Created := Source.Created;
  Columns.Clear();
  for I := 0 to Source.Columns.Count - 1 do
    Columns.AddColumn(Source.Columns[I]);
  Comment := Source.Comment;
  Fulltext := Source.Fulltext;
  IndexType := Source.IndexType;
  PrimaryKey := Source.PrimaryKey;
  Unique := Source.Unique;
end;

function TSKey.ColumnByField(const AField: TSBaseTableField): TSKeyColumn;
var
  I: Integer;
begin
  Result := nil;

  for I := 0 to Columns.Count - 1 do
    if (Columns.Column[I].Field = AField) then
      Result := Columns.Column[I];
end;

function TSKey.ColumnByFieldName(const AFieldName: string): TSKeyColumn;
var
  I: Integer;
begin
  Result := nil;

  for I := 0 to Columns.Count - 1 do
    if (Assigned(Columns[I]) and (lstrcmpi(PChar(Columns[I].Field.Name), PChar(AFieldName)) = 0)) then
      Result := Columns.Column[I];
end;

constructor TSKey.Create(const AKeys: TSKeys; const AName: string = '');
begin
  inherited Create(AKeys, AName);

  FColumns := TSKeyColumns.Create(Self);
  Clear();

  OriginalName := Name;
end;

procedure TSKey.Clear();
begin
  Created := False;

  BlockSize := 0;
  if (Assigned(FColumns)) then
    FColumns.Clear();
  Comment := '';
  Fulltext := False;
  PrimaryKey := False;
  Unique := False;
end;

function TSKey.GetCaption(): string;
begin
  if (PrimaryKey) then
    Result := Preferences.LoadStr(154)
  else
    Result := Name;
end;

function TSKey.GetKeys(): TSKeys;
begin
  Assert(CItems is TSKeys);

  Result := TSKeys(CItems);
end;

procedure TSKey.GetSortDef(var SortDef: TIndexDef);
var
  I: Integer;
begin
  SortDef.Name := Name;
  SortDef.Fields := '';
  SortDef.DescFields := '';
  SortDef.Options := [];

  for I := 0 to Columns.Count - 1 do
  begin
    if (SortDef.Fields <> '') then SortDef.Fields := SortDef.Fields + ';';
    SortDef.Fields := SortDef.Fields + Columns[I].Field.Name;
    if (not Columns[I].Ascending) then
    begin
      if (SortDef.Fields <> '') then SortDef.DescFields := SortDef.DescFields + ';';
      SortDef.DescFields := SortDef.DescFields + Columns[I].Field.Name;
    end;
  end;
end;

function TSKey.GetTable(): TSBaseTable;
begin
  Result := Keys.Table;
end;

function TSKey.Equal(const Second: TSKey): Boolean;
var
  I: Integer;
begin
  Result := inherited Equal(Second);

  Result := Result and (lstrcmpi(PChar(Name), PChar(Second.Name)) = 0);
  Result := Result and (Columns.Count = Second.Columns.Count);
  for I := 0 to Columns.Count - 1 do
  begin
    Result := Result and (lstrcmpi(PChar(Columns[I].Field.OriginalName), PChar(Second.Columns[I].Field.OriginalName)) = 0);
    Result := Result and (Columns[I].Length = Second.Columns[I].Length);
  end;

  Result := Result and (BlockSize = Second.BlockSize);
  Result := Result and (Comment = Second.Comment);
  Result := Result and (Fulltext = Second.Fulltext);
  Result := Result and (PrimaryKey = Second.PrimaryKey);
  Result := Result and (Unique = Second.Unique);
end;

destructor TSKey.Destroy();
begin
  Clear();

  FColumns.Free();

  inherited;
end;

procedure TSKey.SetName(const AName: string);
begin
  FName := AName;
end;

{ TIndices ********************************************************************}

procedure TSKeys.Assign(const Source: TSKeys);
var
  I: Integer;
begin
  Clear();

  for I := 0 to Source.Count - 1 do
  begin
    AddKey(Source.Key[I]);
    Key[I].Created := False;
  end;
end;

procedure TSKeys.AddKey(const NewKey: TSKey);
var
  Index: Integer;
begin
  if (NewKey.PrimaryKey) then
    Index := 0
  else
    Index := Count;

  Insert(Index, TSKey.Create(Self));
  Self.Key[Index].Assign(NewKey);
  Self.Key[Index].Created := True;
end;

constructor TSKeys.Create(const ATable: TSBaseTable);
begin
  inherited Create(ATable.Session);

  FTable := ATable;
end;

procedure TSKeys.DeleteKey(const AKey: TSKey);
var
  I: Integer;
begin
  if (AKey.PrimaryKey) then
    for I := 0 to Table.Fields.Count - 1 do
      if (Table.Fields[I] is TSBaseTableField) then
        TSBaseTableField(Table.Fields[I]).AutoIncrement := False;

  Key[IndexOf(AKey)].Free();

  Delete(IndexOf(AKey));
end;

function TSKeys.GetKey(Index: Integer): TSKey;
begin
  Result := TSKey(Items[Index]);
end;

function TSKeys.GetPrimaryKey(): TSKey;
begin
  if ((Count >= 1) and (Key[0].Name = '')) then
    Result := Key[0]
  else
    Result := nil;
end;

{ TSField *********************************************************************}

procedure TSField.Assign(const Source: TSField);
begin
  Assert(Source is TSTableField);

  inherited Assign(Source);

  Charset := Source.Charset;
  Decimals := Source.Decimals;
  Expression := Source.Expression;
  FieldKind := Source.FieldKind;
  FieldType := Source.FieldType;
  Items := TSTableField(Source).Items;
  National := TSTableField(Source).National;
  Size := Source.Size;
  Stored := Source.Stored;
  Unsigned := TSTableField(Source).Unsigned;
end;

procedure TSField.Clear();
begin
  Charset := '';
  Decimals := 0;
  Expression := '';
  FieldKind := mkUnknown;
  FieldType := mfUnknown;
  SetLength(Items, 0);
  FName := '';
  National := False;
  Size := 0;
  Stored := msUnknown;
  Unsigned := False;
end;

constructor TSField.Create(const AFieldTypes: TSFieldTypes; const AName: string = '');
begin
  inherited Create(AFieldTypes, AName);

  FFieldTypes := AFieldTypes;
end;

function TSField.DBTypeStr(): string;
var
  I: Integer;
begin
  Result := '';

  if (National and (FieldTypes.Session.ServerVersion < 40101)) then
    Result := Result + 'national ';

  Result := Result + FFieldTypes.Session.FieldTypeByMySQLFieldType(FieldType).DBTypeStr();

  if (FieldType in [mfEnum, mfSet]) then
  begin
    Result := Result + '(';
    for I := 0 to Length(Items) - 1 do
    begin
      if (I > 0) then Result := Result + ',';
      Result := Result + SQLEscape(Items[I]);
    end;
    Result := Result + ')';
  end
  else if ((FieldType in [mfFloat, mfDouble, mfDecimal]) and (Size > 0)) then
    Result := Result + '(' + IntToStr(Size) + ',' + IntToStr(Decimals) + ')'
  else if (FieldType in [mfChar, mfVarChar, mfBinary, mfVarBinary]) then
    Result := Result + '(' + IntToStr(Size) + ')'
  else if ((FieldType in [mfTime, mfDateTime, mfTimeStamp]) and (Size > 0) and (FieldTypes.Session.ServerVersion >= 50604)) then
    Result := Result + '(' + IntToStr(Size) + ')'
  else if (FieldType in [mfTinyText, mfText, mfMediumText, mfLongText, mfTinyBlob, mfBlob, mfMediumBlob, mfLongBlob]) then
  else if (FieldType in [mfGeometry, mfPoint, mfLineString, mfPolygon, mfMultiPoint,  mfMultiLineString, mfMultiPolygon, mfGeometryCollection]) then
  else if (FieldType in [mfDate, mfDateTime, mfTime]) then
  else if (FieldType in [mfTimeStamp]) then
    if (FieldTypes.Session.ServerVersion < 40100) then
      Result := Result + '(' + IntToStr(Size) + ')'
    else
  else
    if (Size > 0) then Result := Result + '(' + IntToStr(Size) + ')';

  if (FieldType in [mfTinyInt, mfSmallInt, mfMediumInt, mfInt, mfBigInt, mfFloat, mfDouble, mfDecimal]) then
    if (Unsigned) then Result := Result + ' unsigned';
end;

function TSField.Equal(const Second: TSField): Boolean;
begin
  Result := inherited Equal(Second);

  Result := Result and (National = Second.National);
end;

function TSField.EscapeValue(const Value: string): string;
begin
  if (FieldType = mfBit) then
    if (Value = '') then Result := 'NULL' else Result := 'b''' + Value + ''''
  else if (FieldType in NotQuotedFieldTypes) then
    if (Value = '') then Result := 'NULL' else Result := Value
  else if (FieldType in BinaryFieldTypes) then
    Result := SQLEscapeBin(Value, FieldTypes.Session.ServerVersion <= 40000)
  else
    Result := SQLEscape(Value);
end;

procedure TSField.ParseFieldType(var Parse: TSQLParse);
var
  Identifier: string;
  S: string;
begin
  National := SQLParseKeyword(Parse, 'NATIONAL');

  Identifier := SQLParseValue(Parse);
  FieldType := FFieldTypes.Session.FieldTypeByCaption(Identifier).MySQLFieldType;

  Size := -1;
  Decimals := -1;
  if (SQLParseChar(Parse, '(')) then
  begin
    if (FieldType in [mfEnum, mfSet]) then
    begin
      SetLength(Items, 0);
      repeat
        SetLength(Items, Length(Items) + 1);
        Items[Length(Items) - 1] := SQLParseValue(Parse);
      until (not SQLParseChar(Parse, ','));
    end
    else if (FieldType in [mfFloat, mfDouble, mfDecimal]) then
    begin
      Size := StrToInt(SQLParseValue(Parse));

      if (not SQLParseChar(Parse, ',') and not SQLParseChar(Parse, '.')) then
        Decimals := 0
      else
        Decimals := StrToInt(SQLParseValue(Parse));
    end
    else if (FieldType in [mfTinyText, mfText, mfMediumText, mfLongText, mfTime, mfDateTime, mfTimeStamp]) then
    begin
      S := SQLParseValue(Parse);
      if (not TryStrToInt(S, Size)) then
        Size := -1;
    end
    else if (FieldType in [mfDateTime]) then
      Size := -1
    else if (FieldType in [mfGeometry, mfPoint, mfLineString, mfPolygon, mfMultiPoint, mfMultiLineString, mfMultiPolygon, mfGeometryCollection]) then
      Size := -1
    else
      Size := StrToInt(SQLParseValue(Parse));

    if (not SQLParseChar(Parse, ')')) then
      raise EConvertError.CreateFmt(SSourceParseError, [Name, string(Parse.Start)]);
  end
  else if ((FieldType = mfTinyInt) and ((UpperCase(Identifier) = 'BOOL') or (UpperCase(Identifier) = 'BOOLEAN'))) then
    Size := 1
  else if (FieldType in [mfTinyText, mfTinyBlob]) then
    Size := (1 shl 8) - 1
  else if (FieldType in [mfText, mfBlob]) then
    Size := (1 shl 16) - 1
  else if (FieldType in [mfMediumText, mfMediumBlob]) then
    Size := (1 shl 24) - 1
  else if (FieldType in [mfLongText, mfLongBlob]) then
    Size := (1 shl 32) - 1;

  if (SQLParseKeyword(Parse, 'CHARACTER') or SQLParseKeyword(Parse, 'CHAR')) then
  begin
    SQLParseKeyword(Parse, 'SET');
    Charset := SQLParseValue(Parse);
  end;

  Unsigned := SQLParseKeyword(Parse, 'UNSIGNED');
end;

{ TSTableField ****************************************************************}

procedure TSTableField.Assign(const Source: TSField);
begin
  inherited Assign(Source);

  Ascii := TSTableField(Source).Ascii;
  AutoIncrement := TSTableField(Source).AutoIncrement;
  Binary := TSTableField(Source).Binary;
  Comment := TSTableField(Source).Comment;
  FCollation := TSTableField(Source).FCollation;
  Default := TSTableField(Source).Default;
  FieldBefore := nil;
  FInPrimaryKey := TSTableField(Source).InPrimaryKey;
  FInUniqueKey := TSTableField(Source).InUniqueKey;
  if (Assigned(Fields) and Assigned(TSTableField(Source).Fields) and Assigned(TSTableField(Source).FieldBefore)) then
    FieldBefore := Fields.FieldByName(TSTableField(Source).FieldBefore.Name);
  NullAllowed := TSTableField(Source).NullAllowed or ((Default = 'NULL') and not TSTableField(Source).AutoIncrement and not (FieldType in [mfTinyText, mfText, mfMediumText, mfLongText, mfTinyBlob, mfBlob, mfMediumBlob, mfLongBlob]));
  Unicode := TSTableField(Source).Unicode;
  Zerofill := TSTableField(Source).Zerofill;
end;

procedure TSTableField.Clear();
begin
  Ascii := False;
  AutoIncrement := False;
  Binary := False;
  if (Table is TSBaseTable) then
    Collation := TSBaseTable(Table).FCollation;
  Comment := '';
  Default := '';
  FieldBefore := nil;
  NullAllowed := True;
  Unicode := False;
  Zerofill := False;

  inherited;
end;

constructor TSTableField.Create(const AFields: TSTableFields; const AName: string = '');
begin
  FCItems := AFields;
  FFields := AFields;

  Clear();

  inherited Create(AFields.Table.Database.Session.FieldTypes, AName);
end;

function TSTableField.DBTypeStr(): string;
begin
  Result := '';

  Result := Result + inherited DBTypeStr();

  if ((FieldType in [mfTinyInt, mfSmallInt, mfMediumInt, mfInt, mfBigInt, mfFloat, mfDouble, mfDecimal]) and Zerofill) then
    Result := Result + ' zerofill';
  if (Binary and (Fields.Table.Database.Session.ServerVersion < 40101)) then
    Result := Result + ' binary';
end;

destructor TSTableField.Destroy();
begin
  Clear();

  inherited;
end;

function TSTableField.Equal(const Second: TSTableField): Boolean;
var
  I: Integer;
begin
  Result := inherited Equal(Second);

  Result := Result and (Ascii = Second.Ascii);
  Result := Result and (AutoIncrement = Second.AutoIncrement);
  Result := Result and (Binary = Second.Binary);
  Result := Result and (Collation = Second.Collation);
  Result := Result and (Comment = Second.Comment);
  Result := Result and (Decimals = Second.Decimals);
  Result := Result and (Default = Second.Default);
  Result := Result and (Charset = Second.Charset);
  Result := Result and (FieldType = Second.FieldType);
  Result := Result and (Name = Second.Name);
  Result := Result and (NullAllowed = Second.NullAllowed);
  Result := Result and (Length(Items) = Length(Second.Items));
  if (Result) then
    for I := 0 to Length(Items) - 1 do
      Result := Result and (Items[I] = Second.Items[I]);
  Result := Result and (Size = Second.Size);
  Result := Result and (Unicode = Second.Unicode);
  Result := Result and (Unsigned = Second.Unsigned);
  Result := Result and (Zerofill = Second.Zerofill);
end;

function TSTableField.GetTable(): TSTable;
begin
  Assert(FFields is TSTableFields);

  Result := FFields.Table;
end;

procedure TSTableField.ParseFieldType(var Parse: TSQLParse);
begin
  inherited ParseFieldType(Parse);

  Zerofill := SQLParseKeyword(Parse, 'ZEROFILL');

  Binary := SQLParseKeyword(Parse, 'BINARY');

  Ascii := SQLParseKeyword(Parse, 'ASCII');

  Unicode := SQLParseKeyword(Parse, 'UNICODE');
end;

function TSTableField.UnescapeValue(const Value: string): string;
begin
  Result := SQLUnescape(Value);

  case (FieldType) of
    mfBit:
      if (Value = 'NULL') then
        Result := ''
      else if (Pos('b', Value) = 1) then
        Result := Fields.Table.Database.Session.UnescapeValue(Copy(Value, 2, Length(Value) - 1), FieldType)
      else
        Result := Fields.Table.Database.Session.UnescapeValue(Value, FieldType);
    mfTinyInt, mfSmallInt, mfMediumInt, mfInt, mfBigInt:
      if (Value = 'NULL') then
        Result := ''
      else
        Result := Fields.Table.Database.Session.UnescapeValue(Value, FieldType);
    mfFloat, mfDouble, mfDecimal:
      begin
        Result := ReplaceStr(Fields.Table.Database.Session.UnescapeValue(Value, FieldType), '.', FormatSettings.DecimalSeparator);
        if ((Result = '') and (FieldKind = mkReal)) then Result := 'NULL';
      end;
    mfEnum, mfSet:
      Result := ReplaceStr(Fields.Table.Database.Session.UnescapeValue(Value, FieldType), '''''', '''');
    else
      try
        Result := Fields.Session.UnescapeValue(Value, FieldType);
      except
        on E: EConvertError do
          Fields.Table.Database.Session.DoConvertError(Self, Value, E);
      end;
  end;
end;

{ TSBaseTableField ************************************************************}

procedure TSBaseTableField.Assign(const Source: TSField);
begin
  inherited Assign(Source);

  if (Assigned(TSBaseTable(TSTableField(Source).Fields.Table).FEngine)) then
    FieldType := TSBaseTable(TSTableField(Source).Fields.Table).Engine.ApplyMySQLFieldType(TSTableField(Source).FieldType, TSTableField(Source).Size);
  if (Fields.Table.Database.Session.ServerVersion < 40102) then
    OnUpdate := ''
  else
    OnUpdate := TSBaseTableField(TSTableField(Source)).OnUpdate;

  OriginalName := TSBaseTableField(TSTableField(Source)).OriginalName;
  Moved := TSBaseTableField(TSTableField(Source)).Moved;
end;

procedure TSBaseTableField.Clear();
begin
  Charset := '';
  FCollation := '';

  OnUpdate := '';
  Moved := False;

  inherited;
end;

constructor TSBaseTableField.Create(const AFields: TSTableFields; const AName: string = '');
begin
  inherited Create(AFields, AName);

  OriginalName := AName;
end;

function TSBaseTableField.GetIndex(): Integer;
begin
  Result := Table.Fields.IndexOf(Self);
end;

function TSBaseTableField.GetTable(): TSBaseTable;
begin
  if (not Assigned(Fields) or not (Fields.Table is TSBaseTable)) then
    Result := nil
  else
    Result := TSBaseTable(Fields.Table);
end;

procedure TSBaseTableField.SetName(const AName: string);
begin
  FName := AName;
end;

{ TSViewField *****************************************************************}

function TSViewField.GetIndex(): Integer;
begin
  Result := Table.Fields.IndexOf(Self);
end;

{ TSTableFields ***************************************************************}

procedure TSTableFields.AddField(const NewField: TSTableField);
var
  Index: Integer;
begin
  if (not Assigned(NewField.FieldBefore)) then
    Index := 0
  else
    Index := NewField.FieldBefore.Index + 1;

  if (NewField is TSBaseTableField) then
  begin
    Assert(Self is TSBaseTableFields);
    Insert(Index, TSBaseTableField.Create(TSBaseTableFields(Self)));
  end
  else if (NewField is TSViewField) then
    Insert(Index, TSViewField.Create(Self))
  else
    Exception.Create(sUnknownToType);
  Field[Index].Assign(NewField);
  if (Index > 0) then
    Field[Index].FieldBefore := Field[Index - 1];
  if (Index + 1 <= Count - 1) then
    Field[Index + 1].FieldBefore := Field[Index];
end;

procedure TSTableFields.Assign(const Source: TSTableFields);
var
  I: Integer;
begin
  Clear();

  for I := 0 to Source.Count - 1 do
    AddField(Source.Field[I]);
end;

constructor TSTableFields.Create(const ATable: TSTable);
begin
  inherited Create(ATable.Session);

  FTable := ATable;
end;

procedure TSTableFields.DeleteField(const AField: TSTableField);
var
  I: Integer;
  Index: Integer;
  J: Integer;
begin
  Index := IndexOf(AField);

  if (Assigned(Table) and (Table is TSBaseTable)) then
    for I := TSBaseTable(Table).Keys.Count - 1 downto 0 do
    begin
      for J := TSBaseTable(Table).Keys[I].Columns.Count - 1 downto 0 do
        if (TSBaseTable(Table).Keys[I].Columns[J].Field = AField) then
          TSBaseTable(Table).Keys[I].Columns.DeleteColumn(TSBaseTable(Table).Keys[I].Columns[J]);
      if (TSBaseTable(Table).Keys[I].Columns.Count = 0) then
        TSBaseTable(Table).Keys.DeleteKey(TSBaseTable(Table).Keys[I]);
    end;

  if (Index + 1 < Count) then
    if (Index <= 0) then
      Field[Index + 1].FieldBefore := nil
    else
      Field[Index + 1].FieldBefore := Field[Index - 1];

  Field[Index].Free();
  Delete(Index);
end;

function TSTableFields.FieldByName(const FieldName: string): TSTableField;
var
  Index: Integer;
begin
  Index := IndexByName(FieldName);
  if (Index < 0) then
    Result := nil
  else
    Result := Field[Index];
end;

function TSTableFields.GetField(Index: Integer): TSTableField;
begin
  Result := TSTableField(Items[Index]);
end;

function TSTableFields.IndexByName(const Name: string): Integer;
var
  I: Integer;
begin
  Result := -1;
  for I := 0 to Count - 1 do
    if (NameCmp(Item[I].Name, Name) = 0) then
    begin
      Result := I;
      break;
    end;
end;

function TSTableFields.InsertIndex(const Name: string; out Index: Integer): Boolean;
begin
  raise EAbstractError.Create(SAbstractError);
end;

function TSTableFields.IndexOf(const AField: TSTableField): Integer;
var
  I: Integer;
begin
  Result := -1;

  if (Assigned(AField)) then
    for I := 0 to Count - 1 do
      if (Field[I].Name = AField.Name) then
        Result := I;
end;

{ TSBaseTableFields ***********************************************************}

procedure TSBaseTableFields.MoveField(const AField: TSTableField; const NewFieldBefore: TSTableField);
var
  I: Integer;
  Index: Integer;
  NewIndex: Integer;
begin
  if (NewFieldBefore <> AField.FieldBefore) then
  begin
    Index := IndexOf(AField);

    if (not Assigned(NewFieldBefore)) then
      NewIndex := 0
    else
      NewIndex := IndexOf(NewFieldBefore) + 1;

    Move(Index, NewIndex);

    TSBaseTableField(Field[NewIndex]).Moved := True;

    Field[0].FieldBefore := nil;
    for I := 1 to Count - 1 do
      Field[I].FieldBefore := Field[I - 1];
  end;
end;

{ TSViewFields ****************************************************************}

constructor TSViewFields.Create(const ATable: TSTable);
begin
  inherited Create(ATable);

  FValid := False;
end;

procedure TSViewFields.Invalidate();
begin
  FValid := False;
end;

{ TSForeignKey ****************************************************************}

procedure TSForeignKey.Assign(const Source: TSForeignKey);
var
  I: Integer;
begin
  inherited Assign(Source);

  OriginalName := Source.OriginalName;

  Created := Source.Created;
  SetLength(Fields, Length(Source.Fields));
  for I := 0 to Length(Source.Fields) - 1 do
    Fields[I] := Source.Fields[I];
  Match := Source.Match;
  OnDelete := Source.OnDelete;
  OnUpdate := Source.OnUpdate;
  Parent.DatabaseName := Source.Parent.DatabaseName;
  Parent.TableName := Source.Parent.TableName;
  SetLength(Parent.FieldNames, Length(Source.Parent.FieldNames));
  for I := 0 to Length(Source.Parent.FieldNames) - 1 do
    Parent.FieldNames[I] := Source.Parent.FieldNames[I];
end;

procedure TSForeignKey.Clear();
begin
  Created := False;

  SetLength(Fields, 0);
  Match := mtNo;
  OnDelete := dtRestrict;
  OnUpdate := utRestrict;
  Parent.DatabaseName := '';
  Parent.TableName := '';
  SetLength(Parent.FieldNames, 0);
end;

constructor TSForeignKey.Create(const AForeignKeys: TSForeignKeys; const AName: string = '');
begin
  inherited Create(AForeignKeys, AName);

  Clear();

  OriginalName := AName;
end;

function TSForeignKey.DBTypeStr(): string;
var
  J: Integer;
begin
  Result := '';
  if (Length(Fields) > 1) then Result := Result + '(';
  for J := 0 to Length(Fields) - 1 do
  begin
    if (J > 0) then Result := Result + ', ';
    if (not Assigned(Fields[J])) then
      Result := '???'
    else
      Result := Result + Fields[J].Name;
  end;
  if (Length(Fields) > 1) then Result := Result + ')';

  Result := Result + ' -> ';

  if (Parent.DatabaseName <> ForeignKeys.Table.Database.Name) then
    Result := Result + ForeignKeys.Table.Database.Name + '.';
  Result := Result + Parent.TableName + '.';
  if (Length(Parent.FieldNames) = 1) then
    Result := Result + Parent.FieldNames[0]
  else
  begin
    Result := Result + '(';
    for J := 0 to Length(Parent.FieldNames) - 1 do
    begin
      if (J > 0) then Result := Result + ', ';
      Result := Result + Parent.FieldNames[J];
    end;
    Result := Result + ')';
  end;
end;

destructor TSForeignKey.Destroy();
begin
  SetLength(Fields, 0);
  SetLength(Parent.FieldNames, 0);

  inherited;
end;

function TSForeignKey.Equal(const Second: TSForeignKey): Boolean;
var
  I: Integer;
begin
  Result := inherited Equal(Second);

  Result := Result and (Length(Fields) = Length(Second.Fields));
  for I := 0 to Length(Fields) - 1 do
    Result := Result and (lstrcmpi(PChar(Fields[I].Name), PChar(Second.Fields[I].Name)) = 0);
  Result := Result and (Match = Second.Match);
  Result := Result and (lstrcmpi(PChar(Name), PChar(Second.Name)) = 0);
  Result := Result and (OnDelete = Second.OnDelete);
  Result := Result and (OnUpdate = Second.OnUpdate);
  Result := Result and (Table.Database.Session.TableNameCmp(Parent.DatabaseName, Second.Parent.DatabaseName) = 0);
  Result := Result and ((Table.Database.Session.TableNameCmp(Parent.TableName, Second.Parent.TableName) = 0)
                     or (Table.Database.Session.TableNameCmp(Parent.TableName, Second.Parent.TableName) = 0));
  Result := Result and (Length(Parent.FieldNames) = Length(Second.Parent.FieldNames));
  for I := 0 to Length(Parent.FieldNames) - 1 do
    Result := Result
      and (Table.Fields.NameCmp(Parent.FieldNames[I], Second.Parent.FieldNames[I]) = 0);
end;

function TSForeignKey.GetForeignKeys(): TSForeignKeys;
begin
  Assert(CItems is TSForeignKeys);

  Result := TSForeignKeys(CItems);
end;

function TSForeignKey.GetTable(): TSBaseTable;
begin
  Result := ForeignKeys.Table;
end;

procedure TSForeignKey.SetName(const AName: string);
begin
  FName := AName;
end;

{ TSForeignKeys ***************************************************************}

procedure TSForeignKeys.AddForeignKey(const NewForeignKey: TSForeignKey);
begin
  InsertForeignKey(TList(Self).Count, NewForeignKey);
end;

procedure TSForeignKeys.Assign(const Source: TSForeignKeys);
var
  I: Integer;
begin
  Clear();

  for I := 0 to Source.Count - 1 do
  begin
    AddForeignKey(Source.ForeignKey[I]);
    ForeignKey[I].Created := Source.ForeignKey[I].Created;
  end;
  FValid := Source.Valid;
end;

procedure TSForeignKeys.Clear();
begin
  inherited;

  FValid := False;
end;

constructor TSForeignKeys.Create(const ATable: TSBaseTable);
begin
  inherited Create(ATable.Session);

  FValid := False;

  FTable := ATable;
end;

procedure TSForeignKeys.DeleteForeignKey(const AForeignKey: TSForeignKey);
var
  Index: Integer;
begin
  Index := IndexOf(AForeignKey);

  ForeignKey[Index].Free();
  Delete(Index);
end;

function TSForeignKeys.GetSession(): TSSession;
begin
  Result := Table.Session;
end;

function TSForeignKeys.GetForeignKey(Index: Integer): TSForeignKey;
begin
  Result := TSForeignKey(Items[Index]);
end;

procedure TSForeignKeys.InsertForeignKey(const Index: Integer; const NewForeignKey: TSForeignKey);
begin
  Insert(Index, TSForeignKey.Create(Self));
  ForeignKey[Index].Assign(NewForeignKey);
  ForeignKey[Index].Created := True;
end;

{ TSTableDataSet **************************************************************}

constructor TSTableDataSet.Create(const ATable: TSTable);
begin
  inherited Create(nil);

  FTable := ATable;

  Connection := ATable.Database.Session;
  FFilterSQL := '';
end;

function TSTableDataSet.SQLSelect(const IgnoreLimit: Boolean = False): string;
var
  DescFieldName: string;
  DescPos: Integer;
  FieldName: string;
  FirstField: Boolean;
  I: Integer;
  Pos: Integer;
begin
  Result := 'SELECT * FROM ';
  if (DatabaseName <> '') then
    Result := Result + Connection.EscapeIdentifier(FDatabaseName) + '.';
  Result := Result + Connection.EscapeIdentifier(Table.Name);
  if ((FilterSQL <> '') or (QuickSearch <> '')) then
  begin
    Result := Result + ' WHERE ';
    if ((FilterSQL <> '') and (QuickSearch <> '')) then
      Result := Result + '(';
    if (FilterSQL <> '') then
      Result := Result + FilterSQL;
    if ((FilterSQL <> '') and (QuickSearch <> '')) then
      Result := Result + ') AND (';
    if (QuickSearch <> '') then
      for I := 0 to Table.Fields.Count - 1 do
      begin
        if (I > 0) then Result := Result + ' OR ';
        Result := Result + Connection.EscapeIdentifier(Table.Fields[I].Name) + ' LIKE ' + SQLEscape('%' + QuickSearch + '%');
      end;
    if ((FilterSQL <> '') and (QuickSearch <> '')) then
      Result := Result + ')';
  end;
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
    if (Offset + RecordCount > 0) then
      Result := Result + IntToStr(Offset + RecordCount) + ',';
    if (IgnoreLimit) then
      RequestedRecordCount := $7fffffff - (Offset + RecordCount)
    else if (RecordCount = 0) then
      RequestedRecordCount := Limit
    else
      RequestedRecordCount := RecordCount;
    Result := Result + IntToStr(RequestedRecordCount);
  end;
  Result := Result + ';' + #13#10;
end;

{ TSTable *********************************************************************}

procedure TSTable.Assign(const Source: TSTable);
begin
  Assert(Assigned(Source.Fields));


  inherited Assign(Source);

  if (not Assigned(FDatabase)) then FDatabase := Source.Database;

  FSourceParsed := Source.SourceParsed;

  if (Assigned(FDataSet)) then
    FreeAndNil(FDataSet);

  FFields.Assign(Source.Fields);
end;

function TSTable.CountRecords(): Integer;
var
  DataSet: TMySQLQuery;
begin
  Result := -1;

  DataSet := TMySQLQuery.Create(nil);
  DataSet.Connection := Session;
  DataSet.CommandText := 'SELECT COUNT(*) FROM '+ Session.EscapeIdentifier(Database.Name) +'.' + Session.EscapeIdentifier(Name);
  DataSet.Open();
  if (DataSet.Active and not DataSet.IsEmpty()) then
    Result := DataSet.Fields[0].AsInteger;
  DataSet.Free();
end;

constructor TSTable.Create(const ACDBObjects: TSDBObjects; const AName: string = '');
begin
  inherited Create(ACDBObjects, ACDBObjects.Session.TableName(AName));

  FDataSet := nil;
  if (Self is TSBaseTable) then
    FFields := TSBaseTableFields.Create(Self)
  else if (Self is TSView) then
    FFields := TSViewFields.Create(Self)
  else
    FFields := TSTableFields.Create(Self);
  FSourceParsed := False;
end;

function TSTable.DeleteRecords(const Field: TSTableField; const Values: TStringList): Boolean;
var
  I: Integer;
  SQL: string;
begin
  SQL := 'DELETE FROM ' + Session.EscapeIdentifier(Database.Name) + '.' + Session.EscapeIdentifier(Name) + #13#10;
  SQL := SQL + '  WHERE ' + Session.EscapeIdentifier(Field.Name) + ' IN (';
  for I := 0 to Values.Count - 1 do
  begin
    if (I > 0) then SQL := SQL + ',';
    SQL := SQL + Field.EscapeValue(Values.Strings[I]);
  end;
  SQL := SQL + ');';

  Result := Database.Session.ExecuteSQL(SQL);

  if (Result) then
    InvalidateData();
end;

destructor TSTable.Destroy();
begin
  inherited;

  if (Assigned(FDataSet)) then
    FDataSet.Free();
  if (Assigned(FFields)) then
    FFields.Free();
end;

function TSTable.FieldByName(const FieldName: string): TSTableField;
begin
  Result := FFields.FieldByName(FieldName);
end;

function TSTable.GetDataSet(): TSTableDataSet;
begin
  Assert(Assigned(Database));


  if (not Assigned(FDataSet)) then
  begin
    FDataSet := TSTableDataSet.Create(Self);
    FDataSet.AutomaticLoadNextRecords := True;
    FDataSet.FDatabaseName := Database.Name;
    FDataSet.CommandText := Name;
  end;

  Result := FDataSet;
end;

function TSTable.GetFields(): TSTableFields;
begin
  Result := FFields;
end;

function TSTable.GetTables(): TSTables;
begin
  Result := TSTables(CItems);
end;

function TSTable.GetValidData(): Boolean;
begin
  Result := Assigned(FDataSet) and FDataSet.Active;
end;

procedure TSTable.Invalidate();
begin
  inherited;

  FSourceParsed := False;

  InvalidateData();
end;

procedure TSTable.InvalidateData();
begin
  if (Assigned(FDataSet) and FDataSet.Active) then
  begin
    FDataSet.Close();
    if (Session.InvalidObjects.IndexOf(Self) < 0) then
      Session.InvalidObjects.Add(Self);
  end;
end;

function TSTable.GetSourceEx(const DropBeforeCreate: Boolean = False; const EncloseDefiner: Boolean = True): string;
begin
  raise EAbstractError.Create(SAbstractError);
end;

procedure TSTable.Open(const FilterSQL, QuickSearch: string; const ASortDef: TIndexDef; const Offset: Integer; const Limit: Integer);
begin
  FFilterSQL := FilterSQL;

  DataSet.Close();

  DataSet.Limit := Limit;
  DataSet.Offset := Offset;
  DataSet.FilterSQL := FilterSQL;
  DataSet.QuickSearch := QuickSearch;
  DataSet.SortDef.Assign(ASortDef);

  Session.SendSQL(DataSet.SQLSelect(), OpenEvent);
end;

function TSTable.OpenEvent(const DataHandle: TMySQLConnection.TDataResult; const Data: Boolean): Boolean;
begin
  DataSet.Open(DataHandle);

  Result := False;
end;

procedure TSTable.SetName(const AName: string);
begin
  if (Database.Session.LowerCaseTableNames = 1) then
    inherited SetName(LowerCase(AName))
  else
    inherited SetName(AName);
end;

procedure TSTable.PushBuildEvent(const CItemsEvents: Boolean = True);
begin
  if (Valid) then
  begin
    if (CItemsEvents) then
    begin
      Session.ExecuteEvent(etItemsValid, Database, CItems);
      Session.ExecuteEvent(etItemsValid, Self, Fields);
    end;
    Session.ExecuteEvent(etItemValid, Database, CItems, Self);
  end;
end;

{ TSPartition *****************************************************************}

procedure TSPartition.Assign(const Source: TSPartition);
begin
  inherited Assign(Source);

  if (Assigned(Source.Table)) then FTable := Source.Table;

  Comment := Source.Comment;
  Engine := Source.Engine;
  OriginalName := Source.OriginalName;
  MaxRows := Source.MaxRows;
  MinRows := Source.MinRows;
  ValuesExpr := Source.ValuesExpr;
end;

procedure TSPartition.Clear();
begin
  Comment := '';
  Engine := nil;
  MaxRows := -1;
  MinRows := -1;
  ValuesExpr := '';
end;

constructor TSPartition.Create(const ACItems: TSItems; const ATable: TSBaseTable);
begin
  inherited Create(ACItems);

  Clear();

  FTable := ATable;
end;

function TSPartition.DBTypeStr(): string;
begin
  if (not (Table.Partitions.PartitionType in [ptRange, ptList])) then
    Result := ''
  else
  begin
    Result := 'PARTITION ';
    if (Name <> '') then
      Result := Result + Table.Session.EscapeIdentifier(Name) + ' ';
    case (Table.Partitions.PartitionType) of
      ptRange: Result := Result + 'VALUES LESS THAN (' + ValuesExpr + ')';
      ptList: Result := Result + 'VALUES IN (' + ValuesExpr + ')';
    end;
    if (Comment <> '') then
      Result := Result + ' COMMENT=' + SQLEscape(Comment);
    if (MaxRows >= 0) then
      Result := Result + ' MAX_ROWS=' + IntToStr(MaxRows);
    if (MinRows >= 0) then
      Result := Result + ' MIN_ROWS=' + IntToStr(MinRows);
  end;
end;

function TSPartition.Equal(const Second: TSPartition): Boolean;
begin
  Result := inherited Equal(Second)
    and (Comment = Second.Comment)
    and (Engine = Second.Engine)
    and (Name = Second.Name)
    and (MaxRows = Second.MaxRows)
    and (MinRows = Second.MinRows)
    and (ValuesExpr = Second.ValuesExpr);
end;

{ TSPartitions ****************************************************************}

procedure TSPartitions.AddPartition(const NewPartition: TSPartition);
begin
  Add(TSPartition.Create(Self, TSBaseTable(Table)));
  Partition[Count - 1].Assign(NewPartition);
end;

procedure TSPartitions.Assign(const Source: TSPartitions);
var
  I: Integer;
begin
  Clear();

  if (Assigned(Source.FTable)) then FTable := Source.FTable;

  Expression := Source.Expression;
  Linear := Source.Linear;
  PartitionsNumber := Source.PartitionsNumber;
  PartitionType := Source.PartitionType;

  for I := 0 to TList(Source).Count - 1 do
    AddPartition(Source.Partition[I]);
end;

procedure TSPartitions.Clear();
begin
  Expression := '';
  Linear := False;
  PartitionsNumber := -1;
  PartitionType := ptNone;

  inherited;
end;

constructor TSPartitions.Create(const ATable: TSBaseTable);
begin
  inherited Create(ATable.Session);

  FTable := ATable;
end;

procedure TSPartitions.DeletePartition(const APartition: TSPartition);
var
  Index: Integer;
begin
  Index := IndexOf(APartition);

  Partition[Index].Free();
  Delete(Index);
end;

function TSPartitions.GetPartition(Index: Integer): TSPartition;
begin
  Result := TSPartition(Items[Index]);
end;

function TSPartitions.IndexOf(const APartition: TSPartition): Integer;
var
  I: Integer;
begin
  Result := -1;

  if (Assigned(APartition)) then
    for I := 0 to Count - 1 do
      if (Partition[I].Name = APartition.Name) then
        Result := I;
end;

procedure TSPartitions.MovePartition(const APartition: TSPartition; const NewIndex: Integer);
begin
  Move(IndexOf(APartition), NewIndex);
end;

function TSPartitions.UpdatePartition(const Partition, NewPartition: TSPartition): Boolean;
begin
  Result := Assigned(Partition) and not Assigned(Table.PartitionByName(NewPartition.Name)) or (Table.PartitionByName(NewPartition.Name) = Partition);

  if (Result) then
    Partition.Assign(NewPartition);
end;

{ TSBaseTable *****************************************************************}

procedure TSBaseTable.Assign(const Source: TSTable);
var
  I: Integer;
begin
  inherited;

  FAutoIncrement := TSBaseTable(Source).AutoIncrement;
  FAvgRowLength := TSBaseTable(Source).AvgRowLength;
  FBlockSize := TSBaseTable(Source).BlockSize;
  FChecked := TSBaseTable(Source).Checked;
  FChecksum := TSBaseTable(Source).Checksum;
  FCollation := TSBaseTable(Source).FCollation;
  FComment := TSBaseTable(Source).Comment;
  FCreated := TSBaseTable(Source).Created;
  FDataSize := TSBaseTable(Source).DataSize;
  FDefaultCharset := TSBaseTable(Source).FDefaultCharset;
  FDefaultCodePage := TSBaseTable(Source).DefaultCodePage;
  FDelayKeyWrite := TSBaseTable(Source).DelayKeyWrite;
  FEngine := TSBaseTable(Source).Engine;
  FIndexSize := TSBaseTable(Source).IndexSize;
  FInsertMethod := TSBaseTable(Source).InsertMethod;
  FMaxDataSize := TSBaseTable(Source).MaxDataSize;
  FMaxRows := TSBaseTable(Source).MaxRows;
  FMinRows := TSBaseTable(Source).MinRows;
  FPackKeys := TSBaseTable(Source).PackKeys;
  FRows := TSBaseTable(Source).Rows;
  FRowType := TSBaseTable(Source).RowType;
  FUnusedSize := TSBaseTable(Source).UnusedSize;
  FUpdated := TSBaseTable(Source).Updated;
  FValidStatus := TSBaseTable(Source).ValidStatus;

  if (SourceParsed) then
  begin
    FKeys.Assign(TSBaseTable(Source).Keys);

    FForeignKeys.FValid := True; // Do not allow GetSource!
    FForeignKeys.Assign(TSBaseTable(Source).ForeignKeys);

    if (Assigned(FPartitions) and (not Assigned(Database) or (Database.Session.ServerVersion < 50107))) then
      FreeAndNil(FPartitions)
    else if (not Assigned(FPartitions) and Assigned(Database) and (Database.Session.ServerVersion >= 50107)) then
      FPartitions := TSPartitions.Create(Self);
    if (Assigned(FPartitions) and Assigned(TSBaseTable(Source).Partitions)) then FPartitions.Assign(TSBaseTable(Source).Partitions);
  end;

  if (Assigned(Source.Database) and Assigned(Database)) then
    if ((Source.Database.Session.ServerVersion < 40101) and (Database.Session.ServerVersion < 40101) or (Source.Database.Session.ServerVersion >= 40101) and (Database.Session.ServerVersion >= 40101)) then
    begin
      DefaultCharset := TSBaseTable(Source).DefaultCharset;
      Collation := TSBaseTable(Source).Collation;
    end
    else if ((Source.Database.Session.ServerVersion < 40101) and (Database.Session.ServerVersion >= 40101)) then
    begin
      for I := 0 to Length(CharsetTranslations) - 1 do
        if (TSBaseTable(Source).DefaultCharset = CharsetTranslations[I].OldCharset) then
        begin
          DefaultCharset := string(CharsetTranslations[I].NewCharset);
          Collation := string(CharsetTranslations[I].NewCollation);
        end;
    end
    else if ((Source.Database.Session.ServerVersion > 40101) and (Database.Session.ServerVersion < 40101)) then
    begin
      for I := 0 to Length(CharsetTranslations) - 1 do
        if ((TSBaseTable(Source).DefaultCharset = CharsetTranslations[I].NewCharset) and (TSBaseTable(Source).Collation = string(CharsetTranslations[I].NewCollation))) then
          DefaultCharset := CharsetTranslations[I].OldCharset;
      if (DefaultCharset = '') then
        for I := 0 to Length(CharsetTranslations) - 1 do
          if (TSBaseTable(Source).DefaultCharset = CharsetTranslations[I].OldCharset) then
            DefaultCharset := CharsetTranslations[I].OldCharset;
      if (DefaultCharset = '') then
        for I := 0 to Length(CharsetTranslations) - 1 do
          if (TSBaseTable(Source).DefaultCharset = CharsetTranslations[I].NewCharset) then
            DefaultCharset := CharsetTranslations[I].NewCharset;
    end;
end;

procedure TSBaseTable.BuildStatus(const DataSet: TMySQLQuery; const UseInformationSchema: Boolean);
begin
  if (not UseInformationSchema) then
  begin
    if (Assigned(DataSet.FindField('Type'))) then // MySQL < 4.1.2 and 5.0.0???
      FEngine := Database.Session.EngineByName(DataSet.FieldByName('Type').AsString)
    else
      FEngine := Database.Session.EngineByName(DataSet.FieldByName('Engine').AsString);
    FRowType := StrToMySQLRowType(DataSet.FieldByName('Row_format').AsString);
    if (Self is TSSystemView) then
      FRows := -1
    else
      FRows := DataSet.FieldByName('Rows').AsLargeInt;
    FAvgRowLength := DataSet.FieldByName('Avg_row_length').AsLargeInt;
    FDataSize := DataSet.FieldByName('Data_length').AsLargeInt;
    FIndexSize := DataSet.FieldByName('Index_length').AsLargeInt;
    FMaxDataSize := DataSet.FieldByName('Max_data_length').AsLargeInt;
    FUnusedSize := DataSet.FieldByName('Data_free').AsLargeInt;
    FAutoIncrement := DataSet.FieldByName('Auto_increment').AsLargeInt;
    FCreated := DataSet.FieldByName('Create_time').AsDateTime;
    FUpdated := DataSet.FieldByName('Update_time').AsDateTime;
    FChecked := DataSet.FieldByName('Check_time').AsDateTime;
    FComment := DataSet.FieldByName('Comment').AsString;
  end
  else
  begin
    FEngine := Database.Session.EngineByName(DataSet.FieldByName('ENGINE').AsString);
    RowType := StrToMySQLRowType(DataSet.FieldByName('ROW_FORMAT').AsString);
    if (Self is TSSystemView) then
      FRows := -1
    else
      FRows := DataSet.FieldByName('TABLE_ROWS').AsLargeInt;
    FAvgRowLength := DataSet.FieldByName('AVG_ROW_LENGTH').AsInteger;
    FDataSize := DataSet.FieldByName('DATA_LENGTH').AsLargeInt;
    FMaxDataSize := DataSet.FieldByName('MAX_DATA_LENGTH').AsLargeInt;
    FIndexSize := DataSet.FieldByName('INDEX_LENGTH').AsLargeInt;
    FUnusedSize := DataSet.FieldByName('DATA_FREE').AsLargeInt;
    FAutoIncrement := DataSet.FieldByName('AUTO_INCREMENT').AsLargeInt;
    FCreated := DataSet.FieldByName('CREATE_TIME').AsDateTime;
    FUpdated := DataSet.FieldByName('UPDATE_TIME').AsDateTime;
    FChecked := DataSet.FieldByName('CHECK_TIME').AsDateTime;
    if (Assigned(DataSet.FindField('TABLE_COLLATION'))) then
      Collation := LowerCase(DataSet.FieldByName('TABLE_COLLATION').AsString);
    try
      FComment := DataSet.FieldByName('TABLE_COMMENT').AsString;
    except
      FComment := string(DataSet.FieldByName('TABLE_COMMENT').AsAnsiString);
    end;

    if (not Assigned(Database.Session.CharsetByCollation(FCollation))) then
      DefaultCharset := ''
    else
      DefaultCharset := Database.Session.CharsetByCollation(FCollation).Name;
  end;

  if (Pos('InnoDB free: ', FComment) > 0) then
  begin
    Delete(FComment, Pos('InnoDB free: ', FComment), Length(FComment) - Pos('InnoDB free: ', FComment) + 1);
    FComment := Trim(FComment);
    if (Copy(FComment, Length(FComment), 1) = ';') then Delete(FComment, Length(FComment), 1);
  end;

  FValidStatus := True;
end;

function TSBaseTable.CountRecords(): Integer;
begin
  if (Assigned(Engine) and Engine.IsInnoDB) then
    Result := Rows
  else
    Result := inherited CountRecords();
end;

constructor TSBaseTable.Create(const ACDBObjects: TSDBObjects; const AName: string = ''; const ASystemTable: Boolean = False);
begin
  FKeys := TSKeys.Create(Self);
  FForeignKeys := TSForeignKeys.Create(Self);
  if (ACDBObjects.Database.Session.ServerVersion < 50107) then
    FPartitions := nil
  else
    FPartitions := TSPartitions.Create(Self);

  inherited Create(ACDBObjects, AName);

  FAutoIncrement := -1;
  FAvgRowLength := -1;
  FChecked := -1;
  FChecksum := False;
  FCollation := '';
  FComment := '';
  FCreated := -1;
  FDataSize := -1;
  if (Assigned(Database)) then
    FDefaultCharset := Database.DefaultCharset
  else
    FDefaultCharset := '';
  FDefaultCodePage := CP_ACP;
  FDelayKeyWrite := False;
  FEngine := nil;
  FIndexSize := -1;
  FInsertMethod := imNo;
  FMaxDataSize := -1;
  FMaxRows := -1;
  FMinRows := -1;
  FPackKeys := piDefault;
  FRows := -1;
  FRowType := mrUnknown;
  FUnusedSize := -1;
  FUpdated := -1;
end;

function TSBaseTable.DBRowTypeStr(): string;
begin
  Result := '';
  case (RowType) of
    mrFixed: Result := 'FIXED';
    mrDynamic: Result := 'DYNAMIC';
    mrCompressed: Result := 'COMPRESSED';
    mrRedundant: Result := 'REDUNDANT';
    mrCompact: if (Database.Session.ServerVersion >= 50003) then Result := 'COMPACT';
  end;
end;

destructor TSBaseTable.Destroy();
var
  I: Integer;
begin
  inherited;

  FKeys.Free();
  FForeignKeys.Free();
  if (Assigned(FPartitions)) then
    FPartitions.Free();
  for I := 0 to Length(FMergeSourceTables) - 1 do
  begin
    FMergeSourceTables[I].DatabaseName := '';
    FMergeSourceTables[I].TableName := '';
  end;
  SetLength(FMergeSourceTables, 0);
end;

procedure TSBaseTable.Empty();
var
  List: TList;
begin
  List := TList.Create();
  List.Add(Self);
  Database.EmptyTables(List);
  List.Free();
end;

function TSBaseTable.EmptyFields(const Fields: TList): Boolean;
var
  I: Integer;
  SQL: string;
begin
  Result := True;

  SQL := '';
  for I := 0 to Fields.Count - 1 do
  begin
    if (SQL <> '') then SQL := SQL + ',';
    if (TSBaseTableField(Fields[I]).NullAllowed) then
      SQL := SQL + Session.EscapeIdentifier(TSBaseTableField(Fields[I]).Name) + '=NULL';
  end;
  if (Result and (SQL <> '')) then
  begin
    SQL := 'UPDATE ' + Session.EscapeIdentifier(Database.Name) + '.' + Session.EscapeIdentifier(Name) + ' SET ' + SQL + ';';

    Result := Database.Session.ExecuteSQL(SQL);

    if (Result) then
      InvalidateData();
  end;
end;

function TSBaseTable.FieldByName(const FieldName: string): TSBaseTableField;
var
  Field: TSField;
begin
  Field := inherited FieldByName(FieldName);
  if (not (Field is TSBaseTableField)) then
    Result := nil
  else
    Result := TSBaseTableField(Field);
end;

function TSBaseTable.ForeignKeyByName(const ForeignKeyName: string): TSForeignKey;
var
  Index: Integer;
begin
  Index := ForeignKeys.IndexByName(ForeignKeyName);
  if (Index < 0) then
    Result := nil
  else
    Result := ForeignKeys[Index];
end;

function TSBaseTable.GetAutoIncrementField(): TSBaseTableField;
var
  I: Integer;
begin
  Result := nil;

  for I := 0 to Fields.Count - 1 do
    if (Fields[I].AutoIncrement) then
      Result := TSBaseTableField(Fields[I]);
end;

function TSBaseTable.GetBaseTableFields(): TSBaseTableFields;
begin
  Result := TSBaseTableFields(GetFields());
end;

function TSBaseTable.GetDependencies(): TSDependencies;
var
  Dependency: TSDependency;
  I: Integer;
begin
  if (not Assigned(FDependencies) and ((Length(FMergeSourceTables) > 0) or Assigned(ForeignKeys) and (ForeignKeys.Count > 0))) then
  begin
    FDependencies := TSDependencies.Create();

    for I := 0 to Length(FMergeSourceTables) - 1 do
    begin
      Dependency := TSDependency.Create();
      Dependency.Session := Session;
      Dependency.DatabaseName := FMergeSourceTables[I].DatabaseName;
      Dependency.ObjectClass := TSBaseTable;
      Dependency.ObjectName := FMergeSourceTables[I].TableName;
      Dependencies.Add(Dependency);
    end;

    for I := 0 to ForeignKeys.Count - 1 do
    begin
      Dependency := TSDependency.Create();
      Dependency.Session := Session;
      Dependency.DatabaseName := ForeignKeys[I].Parent.DatabaseName;
      Dependency.ObjectClass := TSBaseTable;
      Dependency.ObjectName := ForeignKeys[I].Parent.TableName;
      Dependencies.Add(Dependency);
    end;
  end;

  Result := FDependencies;
end;

function TSBaseTable.GetPrimaryKey(): TSKey;
begin
  Result := IndexByName('');
end;

function TSBaseTable.GetSourceEx(const DropBeforeCreate: Boolean = False; const EncloseDefiner: Boolean = True): string;
var
  SQL: string;
begin
  SQL := Trim(FSource) + #13#10;

  if (DropBeforeCreate) then
    SQL := 'DROP TABLE IF EXISTS ' + Session.EscapeIdentifier(Name) + ';' + #13#10 + SQL;

  Result := SQL;
end;

function TSBaseTable.GetTriggers(Index: Integer): TSTrigger;
var
  I: Integer;
  Counter: Integer;
begin
  Result := nil;
  if (Assigned(Database.Triggers)) then
  begin
    Counter := 0;
    for I := 0 to Database.Triggers.Count - 1 do
      if (Database.Triggers[I].Table = Self) then
      begin
        if (Counter = Index) then
          Result := Database.Triggers[I];
        Inc(Counter);
      end;
  end;
end;

function TSBaseTable.GetTriggerCount(): Integer;
var
  I: Integer;
begin
  Result := 0;
  if (Assigned(Database.Triggers)) then
    for I := 0 to Database.Triggers.Count - 1 do
      if (Database.Triggers[I].Table = Self) then
        Inc(Result);
end;

function TSBaseTable.GetValid(): Boolean;
begin
  Result := inherited and (Fields.Count > 0) and ValidStatus;
end;

function TSBaseTable.KeyByCaption(const Caption: string): TSKey;
var
  IndexName: string;
begin
  IndexName := Caption;

  Delete(IndexName, Pos(' (' + Preferences.LoadStr(377), IndexName), Length(IndexName) - Pos(' (' + Preferences.LoadStr(377), IndexName) + 2);
  if (IndexName = Preferences.LoadStr(154)) then IndexName := '';

  Result := IndexByName(IndexName);
end;

function TSBaseTable.IndexByName(const Name: string): TSKey;
var
  Index: Integer;
begin
  if (Keys.Count = 0) then
    Result := nil
  else if (Name = 'PRIMARY') and (Keys[0].Name = '') then
    Result := Keys[0]
  else
  begin
    Index := Keys.IndexByName(Name);
    if (Index < 0) then
      Result := nil
    else
      Result := Keys[Index];
  end;
end;

function TSBaseTable.KeyByDataSet(const DataSet: TSTableDataSet): TSKey;
var
  DescPos: Integer;
  FieldName: string;
  Found: Boolean;
  I: Integer;
  J: Integer;
  Pos: Integer;
begin
  Result := nil;

  for I := 0 to Keys.Count - 1 do
    if (not Assigned(Result)) then
    begin
      Pos := 1; Found := True;
      for J := 0 to Keys[I].Columns.Count - 1 do
        if (Found) then
        begin
          FieldName := ExtractFieldName(DataSet.SortDef.Fields, Pos);
          Found := Found and (FieldName = Keys[I].Columns[J].Field.Name);
          if (Found and not Keys[I].Columns[J].Ascending) then
          begin
            DescPos := 1;
            repeat
              FieldName := ExtractFieldName(DataSet.SortDef.DescFields, DescPos);
              Found := FieldName = Keys[I].Columns[J].Field.Name;
            until (Found or (FieldName = ''));
          end;
        end;
      if (Found) then
        Result := Keys[I];
    end;
end;

procedure TSBaseTable.Invalidate();
begin
  inherited;

  InvalidateStatus();
end;

procedure TSBaseTable.InvalidateStatus();
begin
  if (ValidStatus and (Session.InvalidObjects.IndexOf(Self) < 0)) then
    Session.InvalidObjects.Add(Self);

  FValidStatus := False;
end;

function TSBaseTable.PartitionByName(const PartitionName: string): TSPartition;
var
  Index: Integer;
begin
  Index := Partitions.IndexByName(PartitionName);
  if (Index < 0) then
    Result := nil
  else
    Result := Partitions[Index];
end;

procedure TSBaseTable.ParseCreateTable(const SQL: string);
var
  DatabaseName: string;
  DeleteList: TList;
  FieldName: string;
  FirstParse: Boolean;
  Fulltext: Boolean;
  I: Integer;
  Index: Integer;
  J: Integer;
  K: Integer;
  L: Largeint;
  Moved: Boolean;
  Name: string;
  NewField: TSBaseTableField;
  NewForeignKey: TSForeignKey;
  NewKey: TSKey;
  NewKeyColumn: TSKeyColumn;
  NewPartition: TSPartition;
  Parse: TSQLParse;
  Primary: Boolean;
  S: string;
  Unique: Boolean;
begin
  if (SQLCreateParse(Parse, PChar(SQL), Length(SQL), Session.ServerVersion)) then
  begin
    for I := 0 to Length(FMergeSourceTables) - 1 do
    begin
      FMergeSourceTables[I].DatabaseName := '';
      FMergeSourceTables[I].TableName := '';
    end;
    SetLength(FMergeSourceTables, 0);


    FirstParse := FFields.Count = 0;

    if (not SQLParseKeyword(Parse, 'CREATE')) then
      raise EConvertError.CreateFmt(SSourceParseError, [Database.Name + '.' + Name, SQL]);

    Temporary := SQLParseKeyword(Parse, 'TEMPORARY');

    if (not SQLParseKeyword(Parse, 'TABLE')) then raise EConvertError.CreateFmt(SSourceParseError, [Database.Name + '.' + Name, SQL]);

    Name := SQLParseValue(Parse);
    if (SQLParseChar(Parse, '.')) then
    begin
      if (Database.Session.TableNameCmp(Database.Name, Name) <> 0) then
        raise EConvertError.CreateFmt(SSourceParseError, [Database.Name + '.' + Name, SQL]);
      Name := SQLParseValue(Parse);
    end;

    if (not SQLParseChar(Parse, '(')) then raise EConvertError.CreateFmt(SSourceParseError, [Database.Name + '.' + Name, SQL]);

    if (not Assigned(Database.Session.VariableByName('sql_quote_show_create'))) then
    begin
      Database.Session.IdentifierQuoted := SQLParseChar(Parse, '`', False) or SQLParseChar(Parse, '"', False);
      if (not Assigned(Database.Session.VariableByName('sql_mode')) and Database.Session.IdentifierQuoted) then
        Database.Session.AnsiQuotes := SQLParseChar(Parse, '"', False);
    end;

    Index := 0;
    while (Database.Session.IdentifierQuoted and SQLParseChar(Parse, Database.Session.IdentifierQuoter, False))
      or (not SQLParseChar(Parse, ')', False)
      and not SQLParseKeyword(Parse, 'PRIMARY', False)
      and not SQLParseKeyword(Parse, 'SPATIAL', False)
      and not SQLParseKeyword(Parse, 'KEY', False)
      and not SQLParseKeyword(Parse, 'INDEX', False)
      and not SQLParseKeyword(Parse, 'UNIQUE', False)
      and not SQLParseKeyword(Parse, 'FULLTEXT', False)
      and not SQLParseKeyword(Parse, 'CONSTRAINT', False)
      and not SQLParseKeyword(Parse, 'FOREIGN KEY', False)) do
    begin
      Assert(FFields is TSBaseTableFields);

      Name := SQLParseValue(Parse);

      Moved := False;
      if (Index = FFields.Count) then
        Index := FFields.Add(TSBaseTableField.Create(TSBaseTableFields(FFields), Name))
      else if (Index < FFields.IndexByName(Name)) then
      begin
        FFields.Move(FFields.IndexByName(Name), Index);
        TSBaseTableField(FFields[Index]).Clear();
        TSBaseTableField(FFields[Index]).FName := Name;
        Moved := True;
      end
      else if (Name <> FFields[Index].Name) then
        FFields.Insert(Index, TSBaseTableField.Create(TSBaseTableFields(FFields), Name))
      else
      begin
        TSBaseTableField(FFields[Index]).Clear();
        TSBaseTableField(FFields[Index]).FName := Name;
      end;

      NewField := TSBaseTableField(FFields[Index]);

      if (Index = 0) then
        NewField.FieldBefore := nil
      else
        NewField.FieldBefore := FFields.Field[Index - 1];

      NewField.ParseFieldType(Parse);

      SQLParseKeyword(Parse, 'GENERATED ALWAYS');

      while (not SQLParseChar(Parse, ',', False) and not SQLParseChar(Parse, ')', False)) do
        if (not SQLParseKeyword(Parse, 'AS', False)) then
        begin
          NewField.FieldKind := mkReal;

          if (SQLParseKeyword(Parse, 'CHARACTER SET')) then
            NewField.Charset := SQLParseValue(Parse)
          else if (SQLParseKeyword(Parse, 'COLLATE')) then
            NewField.Collation := LowerCase(SQLParseValue(Parse))
          else if (SQLParseKeyword(Parse, 'NOT NULL')) then
            NewField.NullAllowed := False
          else if (SQLParseKeyword(Parse, 'NULL')) then
            NewField.NullAllowed := True
          else if (SQLParseKeyword(Parse, 'DEFAULT')) then
          begin
            if (SQLParseKeyword(Parse, 'NULL')) then
              NewField.Default := 'NULL'
            else if (SQLParseKeyword(Parse, 'CURRENT_TIMESTAMP')) then
              NewField.Default := 'CURRENT_TIMESTAMP'
            else if (NewField.FieldType = mfBit) then
            begin
              S := SQLParseValue(Parse);
              if (LowerCase(Copy(S, 1, 1)) <> 'b') then
              begin
                MoveMemory(@L, PAnsiChar(RawByteString(S)), Length(S));
                NewField.Default := IntToBitString(L, NewField.Size);
              end
              else
              begin
                Delete(S, 1, 1);
                NewField.Default := SQLUnescape(S);
              end;
            end
            else
              NewField.Default := SQLEscape(SQLParseValue(Parse));
            if (SQLParseKeyword(Parse, 'ON UPDATE')) then
              NewField.OnUpdate := SQLParseValue(Parse);
          end
          else if (SQLParseKeyword(Parse, 'AUTO_INCREMENT')) then
            NewField.AutoIncrement := True
          else if (SQLParseKeyword(Parse, 'COMMENT')) then
            NewField.Comment := SQLParseValue(Parse)
          else if (SQLParseKeyword(Parse, 'COLUMN_FORMAT')) then
            NewField.Format := StrToMySQLRowType(SQLParseValue(Parse))
          else
            raise EConvertError.CreateFmt(SSourceParseError, [Database.Name + '.' + Name, SQL]);
        end
        else if (SQLParseKeyword(Parse, 'AS')) then
        begin
          NewField.FieldKind := mkVirtual;

          NewField.Expression := SQLParseBracketContent(Parse);

          if (SQLParseKeyword(Parse, 'VIRTUAL')) then
            NewField.Stored := msVirtual
          else if (SQLParseKeyword(Parse, 'STORED')) then
            NewField.Stored := msStored;

          if (SQLParseKeyword(Parse, 'COMMENT')) then
            NewField.Comment := SQLParseValue(Parse);

          if (SQLParseKeyword(Parse, 'NOT NULL')) then
            NewField.NullAllowed := False
          else if (SQLParseKeyword(Parse, 'NULL')) then
            NewField.NullAllowed := True;
        end
        else
          SQLParseValue(Parse);

      if (Moved and not FirstParse) then
      begin
        Session.ExecuteEvent(etItemDropped, Self, FFields, NewField);
        Session.ExecuteEvent(etItemCreated, Self, FFields, NewField);
      end;

      Inc(Index);
      SQLParseChar(Parse, ',');
    end;
    while (Index < FFields.Count) do
    begin
      FFields[Index].Free();
      FFields.Delete(Index);
    end;


    DeleteList := TList.Create();
    DeleteList.Assign(FKeys);
    while (SQLParseKeyword(Parse, 'PRIMARY', False)
      or SQLParseKeyword(Parse, 'SPATIAL', False)
      or SQLParseKeyword(Parse, 'KEY', False)
      or SQLParseKeyword(Parse, 'INDEX', False)
      or SQLParseKeyword(Parse, 'UNIQUE', False)
      or SQLParseKeyword(Parse, 'FULLTEXT', False)) do
    begin
      Primary := SQLParseKeyword(Parse, 'PRIMARY');

      Unique := SQLParseKeyword(Parse, 'UNIQUE');

      SQLParseKeyword(Parse, 'SPATIAL');

      Fulltext := SQLParseKeyword(Parse, 'FULLTEXT');

      SQLParseKeyword(Parse, 'KEY');
      SQLParseKeyword(Parse, 'INDEX');

      if (Primary or SQLParseKeyword(Parse, 'TYPE', False) or SQLParseKeyword(Parse, 'USING', False) or SQLParseChar(Parse, '(', False)) then
        Name := ''
      else
        Name := SQLParseValue(Parse);


      if (not FKeys.InsertIndex(Name, Index)) then
      begin
        DeleteList.Delete(DeleteList.IndexOf(FKeys.Items[Index]));
        FKeys[Index].Clear();
      end
      else if (Index < FKeys.Count) then
        FKeys.Insert(Index, TSKey.Create(FKeys, Name))
      else
        FKeys.Add(TSKey.Create(FKeys, Name));
      NewKey := FKeys[Index];

      NewKey.PrimaryKey := Primary;
      NewKey.Unique := Unique;
      NewKey.Fulltext := FullText;

      if (SQLParseKeyword(Parse, 'TYPE') or SQLParseKeyword(Parse, 'USING')) then
        NewKey.IndexType := SQLParseValue(Parse);

      if (SQLParseChar(Parse, '(')) then
        while (not SQLParseChar(Parse, ')')) do
        begin
          NewKeyColumn := TSKeyColumn.Create(NewKey.Columns);

          NewKeyColumn.Field := FieldByName(SQLParseValue(Parse));
          if (SQLParseChar(Parse, '(')) then
          begin
            NewKeyColumn.Length := StrToInt(SQLParseValue(Parse));
            SQLParseChar(Parse, ')')
          end;
          NewKey.Columns.AddColumn(NewKeyColumn);
          FreeAndNil(NewKeyColumn);

          SQLParseChar(Parse, ',');
        end;

      while (not SQLParseChar(Parse, ',', False) and not SQLParseChar(Parse, ')', False)) do
      begin
        if (SQLParseKeyword(Parse, 'COMMENT')) then
          NewKey.Comment := SQLParseValue(Parse)
        else if (SQLParseKeyword(Parse, 'KEY_BLOCK_SIZE') and SQLParseChar(Parse, '=')) then
          NewKey.BlockSize := StrToInt(SQLParseValue(Parse))
        else if (SQLParseKeyword(Parse, 'USING')) then
          NewKey.IndexType := SQLParseValue(Parse)
        else
          SQLParseValue(Parse);
      end;

      NewKey.Unique := NewKey.Unique or NewKey.PrimaryKey;


      SQLParseChar(Parse, ',');
    end;
    while (DeleteList.Count > 0) do
    begin
      Index := FKeys.IndexOf(DeleteList.Items[0]);
      FKeys[Index].Free();
      FKeys.Delete(Index);
      DeleteList.Delete(0);
    end;


    DeleteList.Assign(FForeignKeys);
    while (SQLParseKeyword(Parse, 'CONSTRAINT', False) or SQLParseKeyword(Parse, 'FOREIGN KEY', False)) do
    begin
      if (not SQLParseKeyword(Parse, 'CONSTRAINT')) then
        Name := ''
      else
        Name := SQLParseValue(Parse);// Symbol Name

      if (not SQLParseKeyword(Parse, 'FOREIGN KEY')) then raise EConvertError.CreateFmt(SSourceParseError, [Database.Name + '.' + Name, SQL]);

      if (not SQLParseChar(Parse, '(', False)) then
        Name := SQLParseValue(Parse); // Index Name


      if (not FForeignKeys.InsertIndex(Name, Index)) then
      begin
        DeleteList.Delete(DeleteList.IndexOf(FForeignKeys.Items[Index]));
        FForeignKeys[Index].Clear();
      end
      else if (Index < FForeignKeys.Count) then
        FForeignKeys.Insert(Index, TSForeignKey.Create(FForeignKeys, Name))
      else
        FForeignKeys.Add(TSForeignKey.Create(FForeignKeys, Name));
      NewForeignKey := FForeignKeys[Index];

      if (not SQLParseChar(Parse, '(')) then raise EConvertError.CreateFmt(SSourceParseError, [Database.Name + '.' + Name, SQL]);
      repeat
        SetLength(NewForeignKey.Fields, Length(NewForeignKey.Fields) + 1);
        NewForeignKey.Fields[Length(NewForeignKey.Fields) - 1] := FieldByName(SQLParseValue(Parse));

        SQLParseChar(Parse, ',');
      until (SQLParseChar(Parse, ')'));

      if (not SQLParseKeyword(Parse, 'REFERENCES')) then raise EConvertError.CreateFmt(SSourceParseError, [Database.Name + '.' + Name, SQL]);

      NewForeignKey.Parent.DatabaseName := Database.Name;
      if (not SQLParseObjectName(Parse, NewForeignKey.Parent.DatabaseName, NewForeignKey.Parent.TableName)) then raise EConvertError.CreateFmt(SSourceParseError, [Database.Name + '.' + Name, SQL]);

      NewForeignKey.Parent.TableName := Session.TableName(NewForeignKey.Parent.TableName); // Sometimes MySQL reports parent table name in wrong case sensitive

      if (not SQLParseChar(Parse, '(')) then raise EConvertError.CreateFmt(SSourceParseError, [Database.Name + '.' + Name, SQL]);
      repeat
        FieldName := SQLParseValue(Parse);
        SetLength(NewForeignKey.Parent.FieldNames, Length(NewForeignKey.Parent.FieldNames) + 1);
        NewForeignKey.Parent.FieldNames[Length(NewForeignKey.Parent.FieldNames) - 1] := FieldName;

        SQLParseChar(Parse, ',');
      until (SQLParseChar(Parse, ')'));

      if (not SQLParseKeyword(Parse, 'MATCH')) then
        NewForeignKey.Match := mtNo
      else if (not SQLParseKeyword(Parse, 'FULL')) then
        NewForeignKey.Match := mtFull
      else if (not SQLParseKeyword(Parse, 'PARTIAL')) then
        NewForeignKey.Match := mtPartial;

      if (not SQLParseKeyword(Parse, 'ON DELETE')) then
        NewForeignKey.OnDelete := dtRestrict
      else if (SQLParseKeyword(Parse, 'RESTRICT')) then
        NewForeignKey.OnDelete := dtRestrict
      else if (SQLParseKeyword(Parse, 'CASCADE')) then
        NewForeignKey.OnDelete := dtCascade
      else if (SQLParseKeyword(Parse, 'SET NULL')) then
        NewForeignKey.OnDelete := dtSetNull
      else if (SQLParseKeyword(Parse, 'SET DEFAULT')) then
        NewForeignKey.OnDelete := dtSetDefault
      else if (SQLParseKeyword(Parse, 'NO ACTION')) then
        NewForeignKey.OnDelete := dtNoAction;

      if (not SQLParseKeyword(Parse, 'ON UPDATE')) then
        NewForeignKey.OnUpdate := utRestrict
      else if (SQLParseKeyword(Parse, 'RESTRICT')) then
        NewForeignKey.OnUpdate := utRestrict
      else if (SQLParseKeyword(Parse, 'CASCADE')) then
        NewForeignKey.OnUpdate := utCascade
      else if (SQLParseKeyword(Parse, 'SET NULL')) then
        NewForeignKey.OnUpdate := utSetNull
      else if (SQLParseKeyword(Parse, 'SET DEFAULT')) then
        NewForeignKey.OnUpdate := utSetDefault
      else if (SQLParseKeyword(Parse, 'NO ACTION')) then
        NewForeignKey.OnUpdate := utNoAction;

      Inc(Index);
      SQLParseChar(Parse, ',');
    end;
    while (DeleteList.Count > 0) do
    begin
      Index := FForeignKeys.IndexOf(DeleteList.Items[0]);
      FForeignKeys[Index].Free();
      FForeignKeys.Delete(Index);
      DeleteList.Delete(0);
    end;
    DeleteList.Free();


    if (not SQLParseChar(Parse, ')')) then raise EConvertError.CreateFmt(SSourceParseError, [Database.Name + '.' + Name, SQL]);

    while (not SQLParseChar(Parse, ')') and not SQLParseEnd(Parse) and not SQLParseChar(Parse, ';')) do
    begin
      if (SQLParseKeyword(Parse, 'TYPE') or SQLParseKeyword(Parse, 'ENGINE')) then
      begin
        SQLParseChar(Parse, '=');
        FEngine := Database.Session.EngineByName(SQLParseValue(Parse));
      end
      else if (SQLParseKeyword(Parse, 'AUTO_INCREMENT')) then
      begin
        SQLParseChar(Parse, '=');
        FAutoIncrement := StrToUInt64(SQLParseValue(Parse));
      end
      else if (SQLParseKeyword(Parse, 'CHECKSUM')) then
      begin
        SQLParseChar(Parse, '=');
        FChecksum := SQLParseValue(Parse) = '1';
      end
      else if (SQLParseKeyword(Parse, 'COLLATE')) then
      begin
        SQLParseChar(Parse, '=');
        FCollation := LowerCase(SQLParseValue(Parse));
      end
      else if (SQLParseKeyword(Parse, 'COMMENT')) then
      begin
        SQLParseChar(Parse, '=');
        FComment := SQLParseValue(Parse);
      end
      else if (SQLParseKeyword(Parse, 'DEFAULT CHARSET') or SQLParseKeyword(Parse, 'CHARSET')) then
      begin
        SQLParseChar(Parse, '=');
        DefaultCharset := SQLParseValue(Parse);
      end
      else if (SQLParseKeyword(Parse, 'DELAY_KEY_WRITE')) then
      begin
        SQLParseChar(Parse, '=');
        FDelayKeyWrite := SQLParseValue(Parse) = '1';
      end
      else if (SQLParseKeyword(Parse, 'INSERT_METHOD')) then
      begin
        SQLParseChar(Parse, '=');
        S := Uppercase(SQLParseValue(Parse));
        if (S = 'FIRST') then
          FInsertMethod := imFirst
        else if (S = 'LAST') then
          FInsertMethod := imLast
        else
          FInsertMethod := imNo;
      end
      else if (SQLParseKeyword(Parse, 'KEY_BLOCK_SIZE')) then
      begin
        SQLParseChar(Parse, '=');
        FBlockSize := StrToInt(SQLParseValue(Parse));
      end
      else if (SQLParseKeyword(Parse, 'MAX_ROWS')) then
      begin
        SQLParseChar(Parse, '=');
        FMaxRows := StrToInt64(SQLParseValue(Parse));
      end
      else if (SQLParseKeyword(Parse, 'MIN_ROWS')) then
      begin
        SQLParseChar(Parse, '=');
        FMinRows := StrToInt64(SQLParseValue(Parse));
      end
      else if (SQLParseKeyword(Parse, 'PACK_KEYS')) then
      begin
        SQLParseChar(Parse, '=');
        S := SQLParseValue(Parse);
        if (S = '0') then
          FPackKeys := piUnpacked
        else if (S = '1') then
          FPackKeys := piPacked
        else
          FPackKeys := piDefault;
      end
      else if (SQLParseKeyword(Parse, 'ROW_FORMAT')) then
      begin
        SQLParseChar(Parse, '=');
        FRowType := StrToMySQLRowType(SQLParseValue(Parse));
      end
      else if (SQLParseKeyword(Parse, 'PARTITION')) then
      begin
        SQLParseKeyword(Parse, 'BY');

        if (Assigned(FPartitions)) then
        begin
          FPartitions.Linear := SQLParseKeyword(Parse, 'LINEAR');

          FPartitions.PartitionType := StrToPartitionType(SQLParseValue(Parse));
          if (SQLParseChar(Parse, '(')) then
          begin
            FPartitions.Expression := '(' + SQLParseBracketContent(Parse) + ')';
            if (not SQLParseChar(Parse, ')')) then raise EConvertError.CreateFmt(SSourceParseError, [Database.Name + '.' + Name, SQL]);
          end;

          if (SQLParseKeyword(Parse, 'PARTITIONS')) then
            FPartitions.PartitionsNumber := StrToInt(SQLParseValue(Parse));

          if (SQLParseChar(Parse, '(')) then
          begin
            while (not SQLParseEnd(Parse) and not SQLParseChar(Parse, ')')) do
            begin
              NewPartition := TSPartition.Create(FPartitions, Self);

              while (not SQLParseEnd(Parse) and not SQLParseChar(Parse, ',', False) and not SQLParseChar(Parse, ')', False)) do
              begin
                if (SQLParseKeyword(Parse, 'PARTITION')) then
                begin
                  NewPartition.FName := SQLParseValue(Parse);
                  NewPartition.OriginalName := NewPartition.Name;
                end
                else if (SQLParseKeyword(Parse, 'VALUES')) then
                begin
                  SQLParseKeyword(Parse, 'IN');
                  SQLParseKeyword(Parse, 'LESS THAN');

                  NewPartition.ValuesExpr := SQLParseValue(Parse);
                end
                else if (SQLParseKeyword(Parse, 'ENGINE')) then
                begin
                  SQLParseChar(Parse, '=');
                  NewPartition.Engine := Database.Session.EngineByName(SQLParseValue(Parse));
                end
                else if (SQLParseKeyword(Parse, 'COMMENT')) then
                begin
                  SQLParseChar(Parse, '=');
                  NewPartition.Comment := SQLParseValue(Parse);
                end
                else if (SQLParseKeyword(Parse, 'MAX_ROWS')) then
                begin
                  SQLParseChar(Parse, '=');
                  NewPartition.MaxRows := StrToInt(SQLParseValue(Parse));
                end
                else if (SQLParseKeyword(Parse, 'MIN_ROWS')) then
                begin
                  SQLParseChar(Parse, '=');
                  NewPartition.MinRows := StrToInt(SQLParseValue(Parse));
                end
                else
                begin
                  SQLParseValue(Parse);
                  SQLParseChar(Parse, '=');
                  SQLParseValue(Parse);
                end;
              end;

              FPartitions.AddPartition(NewPartition);
              FreeAndNil(NewPartition);

              SQLParseChar(Parse, ',');
            end;
          end;
        end;
      end
      else if (SQLParseKeyword(Parse, 'UNION')) then
      begin
        if (SQLParseChar(Parse, '(')) then
          repeat
            SetLength(FMergeSourceTables, Length(FMergeSourceTables) + 1);
            FMergeSourceTables[Length(FMergeSourceTables) + 1].DatabaseName := Database.Name;
            if (not SQLParseObjectName(Parse, FMergeSourceTables[Length(FMergeSourceTables) + 1].DatabaseName, FMergeSourceTables[Length(FMergeSourceTables) + 1].TableName)) then
              raise EConvertError.CreateFmt(SSourceParseError, [Database.Name + '.' + Name, SQL]);
            SQLParseChar(Parse, ',');
          until (SQLParseEnd(Parse) or SQLParseChar(Parse, ')'));
      end
      else
      begin
        SQLParseValue(Parse);
        if (SQLParseChar(Parse, '=')) then;
          SQLParseValue(Parse);
      end;

      // subpartitioning is not supported now
    end;

    for I := 0 to FFields.Count - 1 do
      if (FFields.Field[I] is TSBaseTableField) then
      begin
        TSBaseTableField(FFields.Field[I]).FInPrimaryKey := False;
        TSBaseTableField(FFields.Field[I]).FInUniqueKey := False;
        for J := 0 to FKeys.Count - 1 do
          if (J = 0) or (FKeys.Key[J].Unique) then
            for K := 0 to FKeys.Key[J].Columns.Count - 1 do
              if (TSBaseTableField(FFields.Field[I]) = FKeys.Key[J].Columns.Column[K].Field) then
                TSBaseTableField(FFields.Field[I]).FInUniqueKey := True;
      end;

    if ((FKeys.Count >= 1) and (FKeys[0].PrimaryKey)) then
      for J := 0 to FKeys.Key[0].Columns.Count - 1 do
        FKeys.Key[0].Columns.Column[J].Field.FInPrimaryKey := True;

    FSourceParsed := True;

    PushBuildEvent();
  end;
end;

function TSBaseTable.Repair(): Boolean;
begin
  Result := Database.Session.ExecuteSQL('REPAIR TABLE ' + Session.EscapeIdentifier(Database.Name) + '.' + Session.EscapeIdentifier(Name) + ';');
end;

procedure TSBaseTable.PushBuildEvent(const CItemsEvents: Boolean = True);
begin
  if (SourceParsed and CItemsEvents) then
    Session.ExecuteEvent(etItemsValid, Self);
  if (Valid) then
    Session.ExecuteEvent(etItemValid, Database, Tables, Self);
end;

procedure TSBaseTable.SetDefaultCharset(const ADefaultCharset: string);
begin
  FDefaultCharset := LowerCase(ADefaultCharset);

  if (lstrcmpi(PChar(ADefaultCharset), 'binary') = 0) then
    FDefaultCodePage := 0
  else if (not Assigned(Database) or not Assigned(Database.Session)) then
    FDefaultCodePage := CP_ACP
  else if (FDefaultCharset <> '') then
    FDefaultCodePage := Database.Session.CharsetToCodePage(FDefaultCharset)
  else if (Database.FDefaultCharset <> '') then
    FDefaultCodePage := Database.Session.CharsetToCodePage(Database.FDefaultCharset)
  else
    FDefaultCodePage := Database.Session.CharsetToCodePage(Database.Session.DefaultCharset)
end;

procedure TSBaseTable.SetSource(const ADataSet: TMySQLQuery);
var
  RBS: RawByteString;
  S: string;
begin
  try
    SetSource(ADataSet.FieldByName('Create Table'));
  except
    // Sometimes, the MySQL server sends invalid field comments
    // This code allow the user to handle this table - but the comments are wrong.
    SetLength(S, ADataSet.LibLengths[ADataSet.FieldByName('Create Table').FieldNo - 1]);
    try
      AnsiCharToWideChar(CP_ACP, ADataSet.LibRow[ADataSet.FieldByName('Create Table').FieldNo - 1], ADataSet.LibLengths[ADataSet.FieldByName('Create Table').FieldNo - 1], @S[1], Length(S));
      SetSource(S);
    except
      SetString(RBS, ADataSet.LibRow[ADataSet.FieldByName('Create Table').FieldNo - 1], ADataSet.LibLengths[ADataSet.FieldByName('Create Table').FieldNo - 1]);
      SetSource(string(RBS));
    end;
  end;

  if (Source <> '') then
    ParseCreateTable(Source);
end;

function TSBaseTable.SQLGetSource(): string;
begin
  Result := 'SHOW CREATE TABLE ' + Session.EscapeIdentifier(Database.Name) + '.' + Session.EscapeIdentifier(Name) + ';' + #13#10
end;

{ TSSystemView ****************************************************************}

{ TSViewField *****************************************************************}

{ TSView **********************************************************************}

procedure TSView.Assign(const Source: TSTable);
begin
  inherited;

  FAlgorithm := TSView(Source).Algorithm;
  FDefiner := TSView(Source).Definer;
  FCheckOption := TSView(Source).CheckOption;
  FSecurity := TSView(Source).Security;
  FStmt := TSView(Source).Stmt;
end;

constructor TSView.Create(const ACDBObjects: TSDBObjects; const AName: string = '');
begin
  inherited Create(ACDBObjects, AName);

  ActualDependencies := False;
  FAlgorithm := vaUndefined;
  FDefiner := '';
  FCheckOption := voNone;
  FSecurity := seDefiner;
  FStmt := '';
end;

function TSView.GetDependencies(): TSDependencies;
begin
  if (not Assigned(FDependencies)) then
  begin
    FDependencies := TSDependencies.Create();
    Database.ParseDependencies(Stmt, FDependencies);
  end;

  Result := FDependencies;
end;

function TSView.GetValid(): Boolean;
begin
  Result := inherited GetValid() and ValidFields;
end;

function TSView.GetValidFields(): Boolean;
begin
  Result := Fields.Valid;
end;

function TSView.GetViewFields(): TSViewFields;
begin
  Result := TSViewFields(GetFields());
end;

function TSView.GetSourceEx(const DropBeforeCreate: Boolean = False; const EncloseDefiner: Boolean = True): string;
var
  CheckOptionSQL: string;
  EndingCommentLen: Integer;
  I: Integer;
  Index: Integer;
  Parse: TSQLParse;
  RemovedLength: Integer;
  SQL: string;
  StartingCommentLen: Integer;
  StringList: TStringList;
begin
  SQL := FSource; RemovedLength := 0;

  if (SQLCreateParse(Parse, PChar(FSource), Length(FSource), Session.ServerVersion)) then
  begin
    if (not EncloseDefiner) then
    begin
      if (not SQLParseKeyword(Parse, 'CREATE')) then raise EConvertError.CreateFmt(SSourceParseError, [Database.Name + '.' + Name, SQL]);

      Index := SQLParseGetIndex(Parse);
      if (SQLParseKeyword(Parse, 'ALGORITHM')) then
      begin
        if (not SQLParseChar(Parse, '=')) then raise EConvertError.CreateFmt(SSourceParseError, [Database.Name + '.' + Name, SQL]);
        SQLParseValue(Parse);
        Delete(SQL, Index - RemovedLength, SQLParseGetIndex(Parse) - Index);
        Inc(RemovedLength, SQLParseGetIndex(Parse) - Index);
      end;

      Index := SQLParseGetIndex(Parse);
      if (SQLParseKeyword(Parse, 'DEFINER')) then
      begin
        if (not SQLParseChar(Parse, '=')) then raise EConvertError.CreateFmt(SSourceParseError, [Database.Name + '.' + Name, SQL]);
        SQLParseValue(Parse);
        Delete(SQL, Index - RemovedLength, SQLParseGetIndex(Parse) - Index);
        Inc(RemovedLength, SQLParseGetIndex(Parse) - Index);
      end;

      Index := SQLParseGetIndex(Parse);
      if (SQLParseKeyword(Parse, 'SQL SECURITY')) then
      begin
        SQLParseValue(Parse);
        Delete(SQL, Index - RemovedLength, SQLParseGetIndex(Parse) - Index);
        Inc(RemovedLength, SQLParseGetIndex(Parse) - Index);
      end;

      if (not SQLParseKeyword(Parse, 'VIEW')) then raise EConvertError.CreateFmt(SSourceParseError, [Database.Name + '.' + Name, SQL]);

      Index := SQLParseGetIndex(Parse);
      FName := SQLParseValue(Parse);
      if (SQLParseChar(Parse, '.')) then
      begin
        Delete(SQL, Index - RemovedLength, SQLParseGetIndex(Parse) - Index);
        Inc(RemovedLength, SQLParseGetIndex(Parse) - Index);
        FName := SQLParseValue(Parse);
      end;

      if (SQLParseChar(Parse, '(')) then
        while (not SQLParseEnd(Parse) and not SQLParseChar(Parse, ')')) do
        begin
          SQLParseValue(Parse);
          SQLParseChar(Parse, ',');
        end;

      if (not SQLParseKeyword(Parse, 'AS')) then raise EConvertError.CreateFmt(SSourceParseError, [Database.Name + '.' + Name, SQL]);

      SQLTrimStmt(FSource, SQLParseGetIndex(Parse), Length(FSource) - (SQLParseGetIndex(Parse) - 1), StartingCommentLen, EndingCommentLen);
      if (Copy(SQL, Length(SQL) - EndingCommentLen, 1) = ';') then
        Inc(EndingCommentLen);

      if (UpperCase(RightStr(SQL, 18)) = ' WITH CHECK OPTION') then
        CheckOptionSQL := RightStr(SQL, 17)
      else if (UpperCase(RightStr(SQL, 27)) = ' WITH CASCADED CHECK OPTION') then
        CheckOptionSQL := RightStr(SQL, 26)
      else if (UpperCase(RightStr(SQL, 24)) = ' WITH LOCAL CHECK OPTION') then
        CheckOptionSQL := RightStr(SQL, 23)
      else
        CheckOptionSQL := '';

      SQL := LeftStr(SQL, SQLParseGetIndex(Parse) - RemovedLength - 1);

      StringList := TStringList.Create();
      StringList.Text := LeftStr(Stmt, Length(Stmt) - 1);
      for I := 0 to StringList.Count - 1 do
        StringList[I] := '  ' + StringList[I];
      SQL := SQL + #13#10 + '  ' + Trim(StringList.Text);
      if (CheckOptionSQL <> '') then
        SQL := SQL + #13#10 + CheckOptionSQL;
      SQL := SQL + ';' + #13#10;
      StringList.Free();
    end;
  end;

  if (DropBeforeCreate) then
    SQL := 'DROP VIEW IF EXISTS ' + Session.EscapeIdentifier(Name) + ';' + #13#10 + SQL;

  Result := SQL;
end;

procedure TSView.Invalidate();
begin
  inherited;

  Fields.Invalidate();
end;

function TSView.ParseCreateView(const SQL: string): string;
var
  EndingCommentLen: Integer;
  Len: Integer;
  Parse: TSQLParse;
  StartingCommentLen: Integer;
begin
  if (not SQLCreateParse(Parse, PChar(SQL), Length(SQL), Database.Session.ServerVersion)) then
    Result := ''
  else
  begin
    Result := SQL;

    if (not SQLParseKeyword(Parse, 'CREATE')) then raise EConvertError.CreateFmt(SSourceParseError, [Database.Name + '.' + Name, SQL]);

    if (not SQLParseKeyword(Parse, 'ALGORITHM')) then
      Algorithm := vaUndefined
    else
    begin
      if (not SQLParseChar(Parse, '=')) then raise EConvertError.CreateFmt(SSourceParseError, [Database.Name + '.' + Name, SQL]);

      if (SQLParseKeyword(Parse, 'MERGE')) then
        FAlgorithm := vaMerge
      else if (SQLParseKeyword(Parse, 'TEMPTABLE')) then
        FAlgorithm := vaTemptable
      else if (SQLParseKeyword(Parse, 'UNDEFINED')) then
        FAlgorithm := vaUndefined
      else
        FAlgorithm := vaUndefined;
    end;

    if (SQLParseKeyword(Parse, 'DEFINER')) then
    begin
      if (not SQLParseChar(Parse, '=')) then raise EConvertError.CreateFmt(SSourceParseError, [Database.Name + '.' + Name, SQL]);
      FDefiner := SQLParseValue(Parse);
    end;

    if (SQLParseKeyword(Parse, 'SQL SECURITY DEFINER')) then
      FSecurity := seDefiner
    else if (SQLParseKeyword(Parse, 'SQL SECURITY INVOKER')) then
      FSecurity := seInvoker;

    if (not SQLParseKeyword(Parse, 'VIEW')) then raise EConvertError.CreateFmt(SSourceParseError, [Database.Name + '.' + Name, SQL]);

    FName := SQLParseValue(Parse);
    if (SQLParseChar(Parse, '.')) then
    begin
      if (Database.Session.TableNameCmp(Database.Name, FName) <> 0) then
        raise EConvertError.CreateFmt(SSourceParseError, [Database.Name + '.' + FName, SQL]);
      FName := SQLParseValue(Parse);
    end;

    if (not SQLParseKeyword(Parse, 'AS')) then raise EConvertError.CreateFmt(SSourceParseError, [Database.Name + '.' + Name, SQL]);

    Len := SQLTrimStmt(SQL, SQLParseGetIndex(Parse), Length(SQL) - (SQLParseGetIndex(Parse) - 1), StartingCommentLen, EndingCommentLen);
    if (Copy(SQL, Length(SQL) - EndingCommentLen, 1) = ';') then
    begin
      Dec(Len);
      Inc(EndingCommentLen);
    end;

    if (UpperCase(RightStr(SQL, 18)) = ' WITH CHECK OPTION') then
    begin
      FCheckOption := voDefault;
      Dec(Len, 18); Inc(EndingCommentLen, 18);
    end
    else if (UpperCase(RightStr(SQL, 27)) = ' WITH CASCADED CHECK OPTION') then
    begin
      FCheckOption := voCascaded;
      Dec(Len, 27); Inc(EndingCommentLen, 27);
    end
    else if (UpperCase(RightStr(SQL, 24)) = ' WITH LOCAL CHECK OPTION') then
    begin
      FCheckOption := voLocal;
      Dec(Len, 24); Inc(EndingCommentLen, 24);
    end
    else
      FCheckOption := voNone;

    FStmt := Copy(SQL, SQLParseGetIndex(Parse), Len) + ';';

    FSourceParsed := True;
  end;
end;

procedure TSView.SetSource(const ADataSet: TMySQLQuery);
begin
  SetSource(ADataSet.FieldByName('Create View'));

  if (Valid) then
    PushBuildEvent();
end;

procedure TSView.SetSource(const ASource: string);
begin
  inherited;

  ParseCreateView(FSource);
end;

function TSView.SQLGetSource(): string;
begin
  Result := 'SHOW CREATE VIEW ' + Session.EscapeIdentifier(Database.Name) + '.' + Session.EscapeIdentifier(Name) + ';' + #13#10
end;

{ TSTables ********************************************************************}

procedure TSTables.AddTable(const NewTable: TSTable);
var
  I: Integer;
  Index: Integer;
  TableName: string;
begin
  TableName := NewTable.Name;

  Index := TList(Self).Count - 1;
  for I := TList(Self).Count - 1 downto 0 do
    if (Database.Session.TableNameCmp(TableName, Table[I].Name) <= 0) then
      Index := I;

  if (NewTable is TSBaseTable) then
  begin
    Insert(Index, TSBaseTable.Create(Self));
    Table[Index].Assign(TSBaseTable(NewTable));
  end
  else
  begin
    Insert(Index, TSView.Create(Self));
    Table[Index].Assign(TSView(NewTable));
  end;
end;

function TSTables.Build(const DataSet: TMySQLQuery; const UseInformationSchema: Boolean; Filtered: Boolean = False; const SessionEvents: Boolean = True): Boolean;
var
  DeleteList: TList;
  Index: Integer;
  Name: string;
  NewTable: TSTable;
begin
  if (DataSet.FieldCount = 0) then
    Result := inherited
  else if (DataSet.FieldCount <= 2) then // SHOW [FULL] TABLES
  begin
    DeleteList := TList.Create();
    DeleteList.Assign(Self);

    if (not DataSet.IsEmpty()) then
      repeat
        Name := DataSet.Fields[0].AsString;

        if (InsertIndex(Name, Index)) then
        begin
          if (Database = Database.Session.PerformanceSchema) then
            NewTable := TSSystemView.Create(Self, Name, True)
          else if ((Database.Session.ServerVersion < 50002) or (UpperCase(DataSet.FieldByName('Table_Type').AsString) = 'BASE TABLE') or (UpperCase(DataSet.FieldByName('Table_Type').AsString) = 'ERROR')) then
            NewTable := TSBaseTable.Create(Self, Name)
          else if ((UpperCase(DataSet.FieldByName('Table_Type').AsString) = 'SYSTEM VIEW') or ((50000 <= Database.Session.ServerVersion) and (Database.Session.ServerVersion < 50012) and (Database = Database.Session.InformationSchema)) or (Database = Database.Session.PerformanceSchema)) then
            NewTable := TSSystemView.Create(Self, Name, True)
          else if (UpperCase(DataSet.FieldByName('Table_Type').AsString) = 'VIEW') then
            NewTable := TSView.Create(Self, Name)
          else
            raise EDatabaseError.CreateFmt('Unknown TABLE_TYPE "%s" for NewTable "%S". NewTable will be ignored.  (%s)', [DataSet.FieldByName('TABLE_TYPE').AsString, Name, DataSet.CommandText]);

          if (Index < Count) then
            Insert(Index, NewTable)
          else
            Add(NewTable);
        end
        else if (DeleteList.IndexOf(Items[Index]) >= 0) then
          DeleteList.Delete(DeleteList.IndexOf(Items[Index]));

        if (Filtered and SessionEvents) then
          Session.ExecuteEvent(etItemValid, Database, Self, Table[Index]);
      until (not DataSet.FindNext());
    FValid := True;

    if (not Filtered) then
      while (DeleteList.Count > 0) do
      begin
        Index := IndexOf(DeleteList.Items[0]);
        Item[Index].Free();
        Delete(Index);
        DeleteList.Delete(0);
      end;
    DeleteList.Free();

    Result := inherited;

    if (not Filtered and SessionEvents) then
    begin
      Session.ExecuteEvent(etItemsValid, Session, Session.Databases);
      Session.ExecuteEvent(etItemsValid, Database, Self);
    end;
  end
  else
  begin
    if (not DataSet.IsEmpty()) then
      repeat
        if (not UseInformationSchema) then
          Name := DataSet.FieldByName('Name').AsString
        else
          Name := DataSet.FieldByName('TABLE_NAME').AsString;

        if (InsertIndex(Name, Index)) then
        begin
          if (Database = Database.Session.PerformanceSchema) then
            NewTable := TSSystemView.Create(Self, Name, True)
          else if ((Database.Session.ServerVersion < 50002) or (UpperCase(DataSet.FieldByName('Table_Type').AsString) = 'BASE TABLE') or (UpperCase(DataSet.FieldByName('Table_Type').AsString) = 'ERROR')) then
            NewTable := TSBaseTable.Create(Self, Name)
          else if ((UpperCase(DataSet.FieldByName('Table_Type').AsString) = 'SYSTEM VIEW') or ((50000 <= Database.Session.ServerVersion) and (Database.Session.ServerVersion < 50012) and (Database = Database.Session.InformationSchema)) or (Database = Database.Session.PerformanceSchema)) then
            NewTable := TSSystemView.Create(Self, Name, True)
          else if (UpperCase(DataSet.FieldByName('Table_Type').AsString) = 'VIEW') then
            NewTable := TSView.Create(Self, Name)
          else
            raise EDatabaseError.CreateFmt('Unknown TABLE_TYPE "%s" for NewTable "%S". NewTable will be ignored.  (%s)', [DataSet.FieldByName('TABLE_TYPE').AsString, Name, DataSet.CommandText]);

          if (Index < Count) then
            Insert(Index, NewTable)
          else
            Add(NewTable);
        end;

        if (Table[Index] is TSBaseTable) then
          TSBaseTable(Table[Index]).BuildStatus(DataSet, UseInformationSchema);

        if (Filtered and SessionEvents) then
          Table[Index].PushBuildEvent(Filtered);
      until (not DataSet.FindNext() or (UseInformationSchema and (Session.Databases.NameCmp(DataSet.FieldByName('TABLE_SCHEMA').AsString, Database.Name) <> 0)));

    if (not Filtered and SessionEvents) then
    begin
      FValid := True;
      Session.ExecuteEvent(etItemsValid, Session, Session.Databases);
      Session.ExecuteEvent(etItemsValid, Database, Self);
    end;
    if (Database.Valid and SessionEvents) then
      Session.ExecuteEvent(etItemValid, Session, Session.Databases, Database);

    Result := False;
  end;
end;

procedure TSTables.BuildViewFields(const DataSet: TMySQLQuery; const UseInformationSchema: Boolean);
var
  I: Integer;
  Index: Integer;
  Name: string;
  NewField: TSViewField;
  Parse: TSQLParse;
  View: TSView;
begin
  View := nil; Index := 0;

  if (not DataSet.IsEmpty()) then
    repeat
      if ((Database.ViewByName(DataSet.FieldByName('TABLE_NAME').AsString) <> View) and Assigned(View)) then
      begin
        while (Index < View.Fields.Count) do
        begin
          View.Fields[Index].Free();
          View.Fields.Delete(Index);
        end;

        View.Fields.FValid := True;
        View.PushBuildEvent(False);

        Index := 0;
      end;

      View := Database.ViewByName(DataSet.FieldByName('TABLE_NAME').AsString);

      if (Assigned(View)) then
      begin
        Name := DataSet.FieldByName('COLUMN_NAME').AsString;

        if (Index = View.Fields.Count) then
          Index := View.Fields.Add(TSViewField.Create(View.Fields, Name))
        else if (Index < View.Fields.IndexByName(Name)) then
        begin
          I := View.Fields.IndexByName(Name);
          View.Fields[I].Free();
          View.Fields.Delete(I);
          View.Fields.Insert(Index, TSViewField.Create(View.Fields, Name));
        end
        else
        begin
          TSViewField(View.Fields[Index]).Clear();
          TSViewField(View.Fields[Index]).FName := Name;
        end;

        NewField := TSViewField(View.Fields[Index]);

        if (Index > 0) then
          NewField.FieldBefore := View.Fields[Index - 1];

        NewField.AutoIncrement := Pos('AUTO_INCREMENT', UpperCase(DataSet.FieldByName('EXTRA').AsString)) > 0;
        NewField.FCollation := LowerCase(DataSet.FieldByName('COLLATION_NAME').AsString);
        NewField.Comment := DataSet.FieldByName('COLUMN_COMMENT').AsString;
        NewField.Default := DataSet.FieldByName('COLUMN_DEFAULT').AsString;
        NewField.Charset := DataSet.FieldByName('CHARACTER_SET_NAME').AsString;
        if (DataSet.FieldByName('COLUMN_TYPE').IsNull or (DataSet.FieldByName('COLUMN_TYPE').AsString = 'null') or not SQLCreateParse(Parse, PChar(DataSet.FieldByName('COLUMN_TYPE').AsString), Length(DataSet.FieldByName('COLUMN_TYPE').AsString), Session.ServerVersion)) then
          NewField.FieldType := mfUnknown
        else
          NewField.ParseFieldType(Parse);
        NewField.FInPrimaryKey := UpperCase(DataSet.FieldByName('EXTRA').AsString) = 'PRI';
        NewField.FInUniqueKey := NewField.InPrimaryKey or (UpperCase(DataSet.FieldByName('EXTRA').AsString) = 'UNI');
        NewField.NullAllowed := DataSet.FieldByName('IS_NULLABLE').AsBoolean;

        Inc(Index);
      end;
    until (not DataSet.FindNext());

  if (Assigned(View)) then
  begin
    while (Index < View.Fields.Count) do
    begin
      View.Fields[Index].Free();
      View.Fields.Delete(Index);
    end;

    View.Fields.FValid := True;
    View.PushBuildEvent(False);
  end;

  Session.ExecuteEvent(etItemsValid, Session, Session.Databases);
  Session.ExecuteEvent(etItemsValid, Database, Self);
  if (Database.Valid) then
    Session.ExecuteEvent(etItemValid, Session, Session.Databases, Database);
end;

function TSTables.GetTable(Index: Integer): TSTable;
begin
  Result := TSTable(Items[Index]);
end;

function TSTables.GetValidStatus(): Boolean;
var
  I: Integer;
begin
  Result := True;

  for I := 0 to Count - 1 do
    Result := Result and (not (Table[I] is TSBaseTable) or TSBaseTable(Table[I]).ValidStatus);
end;

function TSTables.NameCmp(const Name1, Name2: string): Integer;
begin
  if (Session.LowerCaseTableNames = 0) then
    Result := lstrcmp(PChar(Name1), PChar(Name2))
  else
    Result := lstrcmpi(PChar(Name1), PChar(Name2));
end;

function TSTables.SQLGetItems(const Name: string = ''): string;
begin
  if (Database.Session.ServerVersion < 50002) then
    Result := 'SHOW TABLES FROM ' + Session.EscapeIdentifier(Database.Name) + ';' + #13#10
  else
    Result := 'SHOW FULL TABLES FROM ' + Session.EscapeIdentifier(Database.Name) + ';' + #13#10;
end;

function TSTables.SQLGetStatus(const Tables: TList = nil): string;
var
  I: Integer;
begin
  Result := '';

  if (Session.ServerVersion < 50003) then // 5.0.2 supports INFORMATION_SCHEMA, but WHERE clause is supported up from 5.0.3
  begin
    if (Tables.Count < Count) then
    begin
      for I  := 0 to Tables.Count - 1 do
        if (TSDBObject(Tables[I]) is TSBaseTable) then
          Result := Result + 'SHOW TABLE STATUS FROM ' + Session.EscapeIdentifier(Database.Name) + ' LIKE ' + SQLEscape(TSTable(Tables[I]).Name) + ';' + #13#10;
    end
    else if (not ValidStatus) then
      Result := 'SHOW TABLE STATUS FROM ' + Session.EscapeIdentifier(Database.Name) + ';' + #13#10
  end
  else
  begin
    if (Tables.Count < Count) then
    begin
      for I := 0 to Tables.Count - 1 do
        if (TSDBObject(Tables[I]) is TSBaseTable) then
        begin
          if (Result <> '') then Result := Result + ',';
          Result := Result + SQLEscape(TSBaseTable(Tables[I]).Name);
        end;
      if (Result <> '') then
        Result := 'SELECT * FROM ' + Session.EscapeIdentifier(INFORMATION_SCHEMA) + '.' + Session.EscapeIdentifier('TABLES')
          + ' WHERE ' + Session.EscapeIdentifier('TABLE_SCHEMA') + '=' + SQLEscape(Database.Name)
          + ' AND ' + Session.EscapeIdentifier('TABLE_NAME') + ' IN (' + Result + ');' + #13#10;
    end
    else if (not ValidStatus) then
      Result := 'SELECT * FROM ' + Session.EscapeIdentifier(INFORMATION_SCHEMA) + '.' + Session.EscapeIdentifier('TABLES')
        + ' WHERE ' + Session.EscapeIdentifier('TABLE_SCHEMA') + '=' + SQLEscape(Database.Name) + ';' + #13#10;
  end;
end;

function TSTables.SQLGetViewFields(const Tables: TList = nil): string;
var
  I: Integer;
  SQL: string;
begin
  if ((Tables.Count = 0) or (Session.ServerVersion < 50001)) then
    SQL := ''
  else if (Tables.Count < Count) then
  begin
    SQL := '';
    for I := 0 to Tables.Count - 1 do
      if (TSTable(Tables[I]) is TSView) then
      begin
        if (SQL <> '') then SQL := SQL + ',';
        SQL := SQL + SQLEscape(TSView(Tables[I]).Name);
      end;
    if (SQL <> '') then
      SQL := 'SELECT * FROM ' + Session.EscapeIdentifier(INFORMATION_SCHEMA) + '.' + Session.EscapeIdentifier('COLUMNS') + ' WHERE ' + Session.EscapeIdentifier('TABLE_SCHEMA') + '=' + SQLEscape(Database.Name) + ' AND ' + Session.EscapeIdentifier('TABLE_NAME') + ' IN (' + SQL + ') ORDER BY ' + Session.EscapeIdentifier('TABLE_NAME') + ',' + Session.EscapeIdentifier('ORDINAL_POSITION') + ';' + #13#10;
  end
  else
    SQL := SQL + 'SELECT * FROM ' + Session.EscapeIdentifier(INFORMATION_SCHEMA) + '.' + Session.EscapeIdentifier('COLUMNS') + ' WHERE ' + Session.EscapeIdentifier('TABLE_SCHEMA') + '=' + SQLEscape(Database.Name) + ' ORDER BY ' + Session.EscapeIdentifier('TABLE_NAME') + ',' + Session.EscapeIdentifier('ORDINAL_POSITION') + ';' + #13#10;

  Result := SQL;
end;

{ TSRoutineParameter **********************************************************}

procedure TSRoutineParameter.Assign(const Source: TSField);
begin
  Assert(Source is TSRoutineParameter);

  inherited;

  FParameterType := TSRoutineParameter(Source).FParameterType;
end;

constructor TSRoutineParameter.Create(ARoutine: TSRoutine);
begin
  inherited Create(ARoutine.Database.Session.FieldTypes);

  FRoutine := ARoutine;

  FParameterType := ptIn;
end;

{ TSRoutine *******************************************************************}

procedure TSRoutine.Assign(const Source: TSRoutine);
var
  I: Integer;
begin
  inherited Assign(Source);

  if (not Assigned(FDatabase)) then FDatabase := Source.FDatabase;

  Comment := Source.Comment;
  FCreated := Source.Created;
  FDatabase := Source.Database;
  FDefiner := Source.Definer;
  if (Assigned(Source.FFunctionResult)) then
  begin
    FFunctionResult := TSField.Create(Database.Session.FieldTypes);
    FFunctionResult.Assign(Source.FFunctionResult);
  end;
  FModified := Source.Modified;
  SetLength(FParameters, Length(Source.FParameters));
  for I := 0 to Length(FParameters) - 1 do
  begin
    FParameters[I] := TSRoutineParameter.Create(Self);
    FParameters[I].Assign(Source.FParameters[I]);
  end;
  FRoutineType := Source.RoutineType;
  FSecurity := Source.Security;
  FSourceParsed := Source.SourceParsed;
  FValidSource := Source.ValidSource;
end;

constructor TSRoutine.Create(const ACDBObjects: TSDBObjects; const AName: string = '');
begin
  inherited Create(ACDBObjects, AName);

  FStmt := '';
  FComment := '';
  FCreated := 0;
  FDefiner := '';
  FFunctionResult := nil;
  FModified := 0;
  FRoutineType := rtUnknown;
  FSecurity := seDefiner;
  SetLength(FParameters, 0);
  FValidSource := False;
end;

destructor TSRoutine.Destroy();
begin
  while (Length(FParameters) > 0) do
  begin
    FParameters[Length(FParameters) - 1].Free();
    SetLength(FParameters, Length(FParameters) - 1);
  end;

  if (Assigned(FInputDataSet)) then
  begin
    FInputDataSet.Cancel();
    FreeAndNil(FInputDataSet);
  end;
  if (Assigned(FFunctionResult)) then
    FFunctionResult.Free();

  inherited;
end;

function TSRoutine.GetStmt(): string;
begin
  if (not SourceParsed and (Source <> '')) then
    ParseCreateRoutine(Source);

  Result := FStmt;
end;

function TSRoutine.GetInputDataSet(): TMySQLDataSet;
var
  Field: TField;
  I: Integer;
begin
  if (Assigned(FInputDataSet) and (FInputDataSet.FieldCount <> ParameterCount)) then
    FreeAndNil(FInputDataSet);
  if (not Assigned(FInputDataSet) and (ParameterCount > 0)) then
  begin
    FInputDataSet := TMySQLDataSet.Create(nil);
    FInputDataSet.CachedUpdates := True;
    FInputDataSet.Connection := Database.Session;
    for I := 0 to ParameterCount - 1 do
    begin
      if (not (Parameter[I].ParameterType in [ptIn, ptInOut])) then
      begin
        Field := TStringField.Create(nil);
        Field.Size := Length(OutParameterCaption);
      end
      else
        case (Parameter[I].FieldType) of
          mfBit:
            begin
              Field := TMySQLBlobField.Create(nil);
              if ((50020 <= Database.Session.ServerVersion) and (Database.Session.ServerVersion < 50100) or (Database.Session.ServerVersion >= 50110)) then
                Field.Size := Parameter[I].Size div 8
              else
                Field.Size := Parameter[I].Size;
              Field.Tag := ftBitField;
            end;
          mfTinyInt,
          mfSmallInt: if (not Parameter[I].Unsigned) then Field := TSmallIntField.Create(nil) else Field := TWordField.Create(nil);
          mfMediumInt,
          mfInt: if (not Parameter[I].Unsigned) then Field := TIntegerField.Create(nil) else Field := TLargeintField.Create(nil);
          mfBigInt: Field := TLargeintField.Create(nil);
          mfFloat,
          mfDouble,
          mfDecimal: Field := TFloatField.Create(nil);
          mfDate: Field := TMySQLDateField.Create(nil);
          mfDateTime: Field := TMySQLDateTimeField.Create(nil);
          mfTimeStamp:
            if (Session.ServerVersion < 40100) then
              Field := TMySQLTimeStampField.Create(nil)
            else
              Field := TMySQLDateTimeField.Create(nil);
          mfTime: Field := TMySQLTimeField.Create(nil);
          mfYear: Field := TSmallIntField.Create(nil);
          mfChar,
          mfVarChar:
            begin
              if ((Parameter[I].Size < 256) and ((Parameter[I].Size < 65535) or (Database.Session.ServerVersion < 50000))) then
                Field := TMySQLWideStringField.Create(nil)
              else
                Field := TMySQLWideMemoField.Create(nil);
              if (Parameter[I].Size > 0) then Field.Size := Parameter[I].Size;
              Field.Tag := Database.DefaultCodePage;
            end;
          mfBinary,
          mfVarBinary:
            begin
              Field := TMySQLBlobField.Create(nil);
              if (Parameter[I].Size > 0) then Field.Size := Parameter[I].Size and $7FFFFFFF;
            end;
          mfTinyText,
          mfText,
          mfMediumText,
          mfLongText:
            begin
                Field := TMySQLWideMemoField.Create(nil);
              if (Parameter[I].Size > 0) then Field.Size := Parameter[I].Size;
              Field.Tag := Database.DefaultCodePage;
            end;
          mfTinyBlob,
          mfBlob,
          mfMediumBlob,
          mfLongBlob:
            begin
              Field := TMySQLBlobField.Create(nil);
              if (Parameter[I].Size > 0) then Field.Size := Parameter[I].Size and $7FFFFFFF;
            end;
          mfEnum,
          mfSet:
            begin
              Field := TMySQLWideStringField.Create(nil);
              if (Parameter[I].Size > 0) then Field.Size := Parameter[I].Size;
              Field.Tag := Database.DefaultCodePage;
            end;
          mfGeometry,
          mfPoint,
          mfLineString,
          mfPolygon,
          mfMultiPoint,
          mfMultiLineString,
          mfMultiPolygon,
          mfGeometryCollection:
            begin
              Field := TMySQLBlobField.Create(nil);
              Field.Size := Parameter[I].Size and $7FFFFFFF;
              Field.Tag := ftGeometryField;
            end;
          mfJSON:
            begin
              Field := TMySQLWideStringField.Create(nil);
              if (Parameter[I].Size > 0) then Field.Size := Parameter[I].Size;
              Field.Tag := Database.DefaultCodePage;
            end;
          else raise EDatabaseError.CreateFMT(SUnknownFieldType + '(%s)', [Parameter[I].Name, Database.Session.FieldTypeByMySQLFieldType(Parameter[I].FieldType).Caption]);
        end;
      Field.FieldName := Parameter[I].Name;
      Field.Name := ReplaceStr(ReplaceStr(Field.FieldName, ' ', '_'), '.', '_');
      Field.DataSet := FInputDataSet;
    end;

    FInputDataSet.Open();
    if (not FInputDataSet.Active) then
      FreeAndNil(FInputDataSet)
    else
    begin
      FInputDataSet.CachedUpdates := True;
      FInputDataSet.Append();
      for I := 0 to ParameterCount - 1 do
        if (not (Parameter[I].ParameterType in [ptIn, ptInOut])) then
        begin
          FInputDataSet.Fields[I].AsString := OutParameterCaption;
          FInputDataSet.Fields[I].ReadOnly := True;
        end;
      FInputDataSet.Post();
      FInputDataSet.Edit();
    end;
  end;

  Result := FInputDataSet;
end;

function TSRoutine.GetParameter(Index: Integer): TSRoutineParameter;
begin
  if (not SourceParsed and (Source <> '')) then
    ParseCreateRoutine(Source);

  Result := FParameters[Index];
end;

function TSRoutine.GetParameterCount(): Integer;
begin
  if (not SourceParsed and (Source <> '')) then
    ParseCreateRoutine(Source);

  Result := Length(FParameters);
end;

function TSRoutine.GetRoutines(): TSRoutines;
begin
  Assert(CItems is TSRoutines);

  Result := TSRoutines(CItems);
end;

function TSRoutine.GetSourceEx(const DropBeforeCreate: Boolean = False; const EncloseDefiner: Boolean = True): string;
var
  Index: Integer;
  Parse: TSQLParse;
  SQL: string;
begin
  SQL := FSource;

  if (SQLCreateParse(Parse, PChar(Source), Length(Source), Session.ServerVersion)) then
  begin
    if (not EncloseDefiner) then
    begin
      if (not SQLParseKeyword(Parse, 'CREATE')) then raise EConvertError.CreateFmt(SSourceParseError, [Database.Name + '.' + Name, SQL]);

      Index := SQLParseGetIndex(Parse);
      if (SQLParseKeyword(Parse, 'DEFINER')) then
      begin
        if (not SQLParseChar(Parse, '=')) then raise EConvertError.CreateFmt(SSourceParseError, [Database.Name + '.' + Name, SQL]);
        SQLParseValue(Parse);
        Delete(SQL, Index, SQLParseGetIndex(Parse) - Index);
      end;
    end;
  end;

  SQL := Trim(SQL) + #13#10;

  if (DropBeforeCreate) then
    if (RoutineType = rtProcedure) then
      SQL := 'DROP PROCEDURE IF EXISTS ' + Session.EscapeIdentifier(Name) + ';' + #13#10 + SQL
    else
      SQL := 'DROP FUNCTION IF EXISTS ' + Session.EscapeIdentifier(Name) + ';' + #13#10 + SQL;

  Result := SQL;
end;

procedure TSRoutine.Invalidate();
begin
  inherited;

  FSourceParsed := False;
end;

procedure TSRoutine.ParseCreateRoutine(const SQL: string);
var
  Index: Integer;
  Parameter: TSRoutineParameter;
  Parse: TSQLParse;
begin
  if (SQLCreateParse(Parse, PChar(SQL), Length(SQL), Database.Session.ServerVersion)) then
  begin
    while (Length(FParameters) > 0) do
    begin
      FParameters[Length(FParameters) - 1].Free();
      SetLength(FParameters, Length(FParameters) - 1);
    end;
    if (Assigned(FFunctionResult)) then
      FreeAndNil(FFunctionResult);

    if (not SQLParseKeyword(Parse, 'CREATE')) then
      raise EConvertError.CreateFmt(SSourceParseError, [Database.Name + '.' + Name, SQL]);

    if (SQLParseKeyword(Parse, 'DEFINER')) then
    begin
      if (not SQLParseChar(Parse, '=')) then raise EConvertError.CreateFmt(SSourceParseError, [Database.Name + '.' + Name, SQL]);
      FDefiner := SQLParseValue(Parse);
    end;

    if (SQLParseKeyword(Parse, 'PROCEDURE')) then
      FRoutineType := rtProcedure
    else if (SQLParseKeyword(Parse, 'FUNCTION')) then
      FRoutineType := rtFunction
    else
      raise EConvertError.CreateFmt(SSourceParseError, [Database.Name + '.' + Name, SQL]);

    FName := SQLParseValue(Parse);
    if (SQLParseChar(Parse, '.')) then
    begin
      if (Database.Session.TableNameCmp(Database.Name, FName) <> 0) then
        raise EConvertError.CreateFmt(SSourceParseError, [Database.Name + '.' + FName, SQL]);
      FName := SQLParseValue(Parse);
    end;

    if (SQLParseChar(Parse, '(')) then
      while (not SQLParseChar(Parse, ')')) do
      begin
        SetLength(FParameters, Length(FParameters) + 1);
        FParameters[Length(FParameters) - 1] := TSRoutineParameter.Create(Self);
        Parameter := FParameters[Length(FParameters) - 1];

        if (SQLParseKeyword(Parse, 'OUT')) then
          Parameter.FParameterType := ptOut
        else if (SQLParseKeyword(Parse, 'INOUT')) then
          Parameter.FParameterType := ptInOut
        else if (SQLParseKeyword(Parse, 'IN')) then
          Parameter.FParameterType := ptIn;

        Parameter.FName := SQLParseValue(Parse);

        Parameter.ParseFieldType(Parse);

        if (SQLParseKeyword(Parse, 'CHARSET')) then
          Parameter.Charset := SQLParseValue(Parse)
        else if (SQLParseKeyword(Parse, 'BINARY')) then
          Parameter.Charset := 'BINARY';

        SQLParseChar(Parse, ',');
      end;

    if ((Self is TSFunction) and SQLParseKeyword(Parse, 'RETURNS')) then
    begin
      FFunctionResult := TSField.Create(Database.Session.FieldTypes);
      FFunctionResult.ParseFieldType(Parse);

      if (SQLParseKeyword(Parse, 'CHARSET')) then
        FFunctionResult.Charset := SQLParseValue(Parse);
    end;

    repeat
      Index := SQLParseGetIndex(Parse);

      if (SQLParseKeyword(Parse, 'COMMENT')) then
        FComment := SQLParseValue(Parse)
      else if (SQLParseKeyword(Parse, 'LANGUAGE SQL')) then
      else if (SQLParseKeyword(Parse, 'NOT DETERMINISTIC') or SQLParseKeyword(Parse, 'DETERMINISTIC')) then
      else if (SQLParseKeyword(Parse, 'CONTAINS SQL') or SQLParseKeyword(Parse, 'NO SQL') or SQLParseKeyword(Parse, 'READS SQL DATA') or SQLParseKeyword(Parse, 'MODIFIES SQL DATA')) then
      else if (SQLParseKeyword(Parse, 'SQL SECURITY DEFINER')) then
        FSecurity := seDefiner
      else if (SQLParseKeyword(Parse, 'SQL SECURITY INVOKER')) then
        FSecurity := seInvoker;
    until (Index = SQLParseGetIndex(Parse));

    FStmt := SQLParseRest(Parse);

    FSourceParsed := True;
  end;
end;

procedure TSRoutine.SetSource(const ASource: string);
begin
  inherited;

  try
  ParseCreateRoutine(FSource);
  except
  ParseCreateRoutine(FSource);
  end;
end;

function TSRoutine.SQLGetSource(): string;
begin
  Result := '';
end;

function TSRoutine.SQLRun(): string;
begin
  Result := '';
end;

{ TSProcedure *****************************************************************}

procedure TSProcedure.SetSource(const ADataSet: TMySQLQuery);
begin
  SetSource(ADataSet.FieldByName('Create Procedure'));

  if (Valid) then
  begin
    Session.ExecuteEvent(etItemsValid, Session, Session.Databases);
    Session.ExecuteEvent(etItemValid, Database, Routines, Self);
  end;
end;

function TSProcedure.SQLGetSource(): string;
begin
  Result := 'SHOW CREATE PROCEDURE ' + Session.EscapeIdentifier(Database.Name) + '.' + Session.EscapeIdentifier(Name) + ';' + #13#10
end;

function TSProcedure.SQLRun(): string;
const
  ObjectIDEParameterName = 'MySQL_Front_Object_IDE_';
var
  I: Integer;
  InParameters: Boolean;
  OutParameters: Boolean;
begin
  InParameters := False; OutParameters := False;
  for I := 0 to ParameterCount - 1 do
  begin
    InParameters := InParameters or (Parameter[I].ParameterType in [ptIn, ptInOut]);
    OutParameters := OutParameters or (Parameter[I].ParameterType in [ptInOut, ptOut]);
  end;

  for I := 0 to ParameterCount - 1 do
    if (Parameter[I].ParameterType in [ptInOut]) then
    begin
      if (Result <> '') then Result := Result + ',';
      Result := '@' + ObjectIDEParameterName + Parameter[I].Name + '=' + InputDataSet.SQLFieldValue(InputDataSet.Fields[I]);
    end;
  if (Result <> '') then
    Result := 'SET ' + Result + ';' + #13#10;

  Result := Result + 'CALL ' + Session.EscapeIdentifier(Database.Name) + '.' + Session.EscapeIdentifier(Name) + '(';
  for I := 0 to ParameterCount - 1 do
  begin
    if (I > 0) then Result := Result + ',';
    if (Parameter[I].ParameterType <> ptIn) then
      Result := Result + '@' + ObjectIDEParameterName + Parameter[I].Name
    else
      Result := Result + InputDataSet.SQLFieldValue(InputDataSet.Fields[I]);
  end;
  Result := Result + ');' + #13#10;

  if (OutParameters) then
  begin
    Result := Result + 'SELECT ';
    for I := 0 to ParameterCount - 1 do
    begin
      if (I > 0) then Result := Result + ',';
      if (Parameter[I].ParameterType = ptIn) then
        Result := Result + SQLEscape(InParameterCaption)
      else
        Result := Result + '@' + ObjectIDEParameterName + Parameter[I].Name;
      Result := Result + ' AS ' + Session.EscapeIdentifier(Parameter[I].Name);
    end;
    Result := Result + ';' + #13#10;
  end;
end;

{ TSFunction ******************************************************************}

procedure TSFunction.SetSource(const ADataSet: TMySQLQuery);
begin
  SetSource(ADataSet.FieldByName('Create Function'));

  if (Valid) then
  begin
    Session.ExecuteEvent(etItemsValid, Session, Session.Databases);
    Session.ExecuteEvent(etItemValid, Database, Routines, Self);
  end;
end;

function TSFunction.SQLGetSource(): string;
begin
  Result := 'SHOW CREATE FUNCTION ' + Session.EscapeIdentifier(Database.Name) + '.' + Session.EscapeIdentifier(Name) + ';' + #13#10
end;

function TSFunction.SQLRun(): string;
var
  I: Integer;
begin
  Result := 'SELECT ' + Session.EscapeIdentifier(Database.Name) + '.' + Session.EscapeIdentifier(Name) + '(';
  for I := 0 to ParameterCount - 1 do
  begin
    if (I > 0) then Result := Result + ',';
    if (InputDataSet.Fields[I].Value = Null) then
      Result := Result + 'NULL'
    else
      Result := Result + InputDataSet.SQLFieldValue(InputDataSet.Fields[I]);
  end;
  Result := Result + ');';
end;

{ TSRoutines ******************************************************************}

procedure TSRoutines.AddRoutine(const NewRoutine: TSRoutine);
begin
  if (NewRoutine is TSProcedure) then
  begin
    Add(TSProcedure.Create(Self));
    TSProcedure(Items[TList(Self).Count - 1]).Assign(TSProcedure(NewRoutine));
  end
  else
  begin
    Add(TSFunction.Create(Self));
    TSFunction(Items[TList(Self).Count - 1]).Assign(TSFunction(NewRoutine));
  end;
end;

function TSRoutines.Build(const DataSet: TMySQLQuery; const UseInformationSchema: Boolean; Filtered: Boolean = False; const SessionEvents: Boolean = True): Boolean;
var
  DeleteList: TList;
  Index: Integer;
  Name: string;
  NewRoutine: TSRoutine;
  RoutineType: TSRoutine.TRoutineType;
begin
  DeleteList := TList.Create();
  DeleteList.Assign(Self);

  if (not DataSet.IsEmpty()) then
    repeat
      RoutineType := rtUnknown;
      if (not UseInformationSchema) then
      begin
        Name := DataSet.FieldByName('Name').AsString;
        if (UpperCase(DataSet.FieldByName('Type').AsString) = 'PROCEDURE') then
          RoutineType := rtProcedure
        else
          RoutineType := rtFunction;
      end
      else if (Session.Databases.NameCmp(DataSet.FieldByName('ROUTINE_SCHEMA').AsString, Database.Name) = 0) then
      begin
        Name := DataSet.FieldByName('ROUTINE_NAME').AsString;
        if (UpperCase(DataSet.FieldByName('ROUTINE_TYPE').AsString) = 'PROCEDURE') then
          RoutineType := rtProcedure
        else
          RoutineType := rtFunction;
      end;

      if (RoutineType <> rtUnknown) then
      begin
        if (InsertIndex(Name, Index)) then
        begin
          if (RoutineType = rtProcedure) then
            NewRoutine := TSProcedure.Create(Self, Name)
          else
            NewRoutine := TSFunction.Create(Self, Name);
          if (Index < Count) then
            Insert(Index, NewRoutine)
          else
            Index := Add(NewRoutine);
        end
        else if (DeleteList.IndexOf(Items[Index]) >= 0) then
          DeleteList.Delete(DeleteList.IndexOf(Items[Index]));

        if (not UseInformationSchema) then
        begin
          Routine[Index].FComment := DataSet.FieldByName('Comment').AsString;
          if (DataSet.FieldByName('Security_type').AsString = 'INVOKER') then
            Routine[Index].FSecurity := seInvoker
          else
            Routine[Index].FSecurity := seDefiner ;
          Routine[Index].FCreated := DataSet.FieldByName('Created').AsDateTime;
          Routine[Index].FDefiner := DataSet.FieldByName('Definer').AsString;
          Routine[Index].FModified := DataSet.FieldByName('Modified').AsDateTime;
          Routine[Index].FRoutineType := RoutineType;
        end
        else
        begin
          Routine[Index].FComment := DataSet.FieldByName('ROUTINE_COMMENT').AsString;
          if (DataSet.FieldByName('SECURITY_TYPE').AsString = 'INVOKER') then
            Routine[Index].FSecurity := seInvoker
          else
            Routine[Index].FSecurity := seDefiner;
          Routine[Index].FCreated := DataSet.FieldByName('CREATED').AsDateTime;
          Routine[Index].FDefiner := DataSet.FieldByName('DEFINER').AsString;
          Routine[Index].FModified := DataSet.FieldByName('LAST_ALTERED').AsDateTime;
          Routine[Index].FRoutineType := RoutineType;
        end;
      end;

      if (Filtered and SessionEvents) then
        Session.ExecuteEvent(etItemValid, Session, Self, Routine[Index]);
    until (not DataSet.FindNext() or (Session.Databases.NameCmp(DataSet.FieldByName('ROUTINE_SCHEMA').AsString, Database.Name) <> 0));

  Result := inherited or (Session.ErrorCode = ER_CANNOT_LOAD_FROM_TABLE);

  if (not Filtered) then
    while (DeleteList.Count > 0) do
    begin
      Index := IndexOf(DeleteList.Items[0]);
      Item[Index].Free();
      Delete(Index);
      DeleteList.Delete(0);
    end;
  DeleteList.Free();

  if (not Filtered and SessionEvents) then
  begin
    Session.ExecuteEvent(etItemsValid, Session, Session.Databases);
    Session.ExecuteEvent(etItemsValid, Database, Self);
  end;
  if (Database.Valid and SessionEvents) then
    Session.ExecuteEvent(etItemValid, Session, Session.Databases, Database);
end;

function TSRoutines.GetRoutine(Index: Integer): TSRoutine;
begin
  Result := TSRoutine(Items[Index]);
end;

function TSRoutines.SQLGetItems(const Name: string = ''): string;
begin
  Result := 'SELECT * FROM ' + Session.EscapeIdentifier(INFORMATION_SCHEMA) + '.' + Session.EscapeIdentifier('ROUTINES')
    + ' WHERE ' + Session.EscapeIdentifier('ROUTINE_SCHEMA') + '=' + SQLEscape(Database.Name) + ';' + #13#10;
end;

{ TSTrigger *******************************************************************}

procedure TSTrigger.Assign(const Source: TSTrigger);
begin
  inherited Assign(Source);

  if (Assigned(Source.FDatabase)) then FDatabase := Source.FDatabase;

  FEvent := Source.Event;
  FDatabase := Source.Database;
  FDefiner := Source.Definer;
  FSourceParsed := Source.SourceParsed;
  FStmt := Source.Stmt;
  FTableName := Source.FTableName;
  FTiming := Source.Timing;
end;

constructor TSTrigger.Create(const ACDBObjects: TSDBObjects; const AName: string = '');
begin
  inherited;

  FEvent := teInsert;
  FCreated := 0;
  FDefiner := '';
  FSourceParsed := False;
  FStmt := '';
  FTiming := ttAfter;
  FValid := False;
end;

destructor TSTrigger.Destroy();
begin
  if (Assigned(FInputDataSet)) then
  begin
    FInputDataSet.Cancel();
    FreeAndNil(FInputDataSet);
  end;

  inherited;
end;

function TSTrigger.GetInputDataSet(): TMySQLDataSet;
begin
  if (not Assigned(FInputDataSet)) then
  begin
    FInputDataSet := TMySQLDataSet.Create(nil);
    FInputDataSet.Connection := Database.Session;
    FInputDataSet.CommandText := 'SELECT * FROM ' + Session.EscapeIdentifier(Database.Name) + '.' + Session.EscapeIdentifier(FTableName) + ' LIMIT 0';
    FInputDataSet.Open();
    if (not FInputDataSet.Active) then
      FreeAndNil(FInputDataSet)
    else
    begin
      FInputDataSet.CachedUpdates := True;
      FInputDataSet.Append();
      FInputDataSet.Post();
      FInputDataSet.Edit();
    end;
  end;

  Result := FInputDataSet;
end;

function TSTrigger.GetSourceEx(const DropBeforeCreate: Boolean = False; const EncloseDefiner: Boolean = True): string;
var
  SQL: string;
begin
  SQL := 'CREATE';
  if ((Definer <> '') and not EncloseDefiner) then
    SQL := SQL + ' DEFINER=' + Database.Session.EscapeUser(Definer, True);
  SQL := SQL + ' TRIGGER ' + Session.EscapeIdentifier(Database.Name) + '.' + Session.EscapeIdentifier(Name) + ' ';
  case (Timing) of
    ttBefore: SQL := SQL + 'BEFORE';
    ttAfter: SQL := SQL + 'AFTER';
  end;
  SQL := SQL + ' ';
  case (Event) of
    teInsert: SQL := SQL + 'INSERT';
    teUpdate: SQL := SQL + 'UPDATE';
    teDelete: SQL := SQL + 'DELETE';
  end;
  SQL := SQL + ' ON ';
  SQL := SQL + Session.EscapeIdentifier(Database.Name) + '.' + Session.EscapeIdentifier(FTableName) + #13#10;
  SQL := SQL + '  FOR EACH ROW ' + Stmt;
  if (RightStr(SQL, 1) <> ';') then SQL := SQL + ';';
  SQL := Trim(SQL) + #13#10;

  if (DropBeforeCreate) then
    SQL := 'DROP TRIGGER IF EXISTS ' + Session.EscapeIdentifier(Name) + ';' + #13#10 + SQL;

  Result := SQL;
end;

function TSTrigger.GetStmt(): string;
begin
  if (not SourceParsed and (Source <> '')) then
    ParseCreateTrigger(Source);

  Result := FStmt;
end;

function TSTrigger.GetTable(): TSBaseTable;
begin
  Result := Database.BaseTableByName(FTableName);
end;

function TSTrigger.GetTriggers(): TSTriggers;
begin
  Assert(CItems is TSTriggers);

  Result := TSTriggers(CItems);
end;

procedure TSTrigger.Invalidate();
begin
  inherited;

  FValid := False;
  FSourceParsed := False;
end;

procedure TSTrigger.ParseCreateTrigger(const SQL: string);
var
  DatabaseName: string;
  Parse: TSQLParse;
begin
  if (SQLCreateParse(Parse, PChar(SQL), Length(SQL), Database.Session.ServerVersion)) then
  begin
    if (not SQLParseKeyword(Parse, 'CREATE')) then
      raise EConvertError.CreateFmt(SSourceParseError, [Database.Name + '.' + Name, SQL]);

    if (SQLParseKeyword(Parse, 'DEFINER') and SQLParseChar(Parse, '=')) then
      FDefiner := SQLParseValue(Parse);

    if (not SQLParseKeyword(Parse, 'TRIGGER')) then
      raise EConvertError.CreateFmt(SSourceParseError, [Database.Name + '.' + Name, SQL]);

    DatabaseName := Database.Name;
    if (not SQLParseObjectName(Parse, DatabaseName, FName)) then
      raise EConvertError.CreateFmt(SSourceParseError, [Database.Name + '.' + Name, SQL]);

    if (SQLParseKeyword(Parse, 'BEFORE')) then
      FTiming := ttBefore
    else if (SQLParseKeyword(Parse, 'AFTER')) then
      FTiming := ttAfter
    else
      raise EConvertError.CreateFmt(SSourceParseError, [Database.Name + '.' + Name, SQL]);

    if (SQLParseKeyword(Parse, 'INSERT')) then
      FEvent := teInsert
    else if (SQLParseKeyword(Parse, 'UPDATE')) then
      FEvent := teUpdate
    else if (SQLParseKeyword(Parse, 'DELETE')) then
      FEvent := teDelete
    else
      raise EConvertError.CreateFmt(SSourceParseError, [Database.Name + '.' + Name, SQL]);

    if (not SQLParseKeyword(Parse, 'ON')) then
      raise EConvertError.CreateFmt(SSourceParseError, [Database.Name + '.' + Name, SQL]);

    if (not SQLParseObjectName(Parse, DatabaseName, FTableName)) then
      raise EConvertError.CreateFmt(SSourceParseError, [Database.Name + '.' + Name, SQL]);

    if (not SQLParseKeyword(Parse, 'FOR EACH ROW')) then
      raise EConvertError.CreateFmt(SSourceParseError, [Database.Name + '.' + Name, SQL]);

    FStmt := SQLParseRest(Parse);

    FSourceParsed := True;
  end;
end;

procedure TSTrigger.SetSource(const ADataSet: TMySQLQuery);
begin
  SetSource(ADataSet.FieldByName('SQL Original Statement'));

  if (Valid) then
  begin
    Session.ExecuteEvent(etItemsValid, Session.Databases);
    Session.ExecuteEvent(etItemValid, Database, Triggers, Self);
  end;
end;

function TSTrigger.SQLDelete(): string;
begin
  Result := InputDataSet.SQLDelete();
end;

function TSTrigger.SQLGetSource(): string;
begin
  Result := 'SHOW CREATE TRIGGER ' + Session.EscapeIdentifier(Database.Name) + '.' + Session.EscapeIdentifier(Name) + ';' + #13#10
end;

function TSTrigger.SQLInsert(): string;
begin
  Result := InputDataSet.SQLInsert();
end;

function TSTrigger.SQLReplace(): string;
begin
  Result := SQLInsert();
  if (Result <> '') then
    Result := 'REPLACE' + RightStr(Result, Length(Result) - Length('INSERT'));
end;

function TSTrigger.SQLUpdate(): string;
var
  Valid: Boolean;
  I: Integer;
  SetClause: string;
  TableClause: string;
  WhereClause: string;
begin
  TableClause := Session.EscapeIdentifier(Database.Name) + '.' + Session.EscapeIdentifier(FTableName);
  SetClause := '';
  WhereClause := '';

  for I := 0 to InputDataSet.FieldCount - 1 do
    if (not (pfInWhere in InputDataSet.Fields[I].ProviderFlags) and (not InputDataSet.Fields[I].Required or not InputDataSet.Fields[I].IsNull)) then
    begin
      if (SetClause <> '') then SetClause := SetClause + ',';
      SetClause := SetClause + Session.EscapeIdentifier(InputDataSet.Fields[I].FieldName)
        + '=' + InputDataSet.SQLFieldValue(InputDataSet.Fields[I]);
    end;

  Valid := SetClause <> '';
  if (Valid) then
  begin
    for I := 0 to InputDataSet.FieldCount - 1 do
      if (pfInWhere in InputDataSet.Fields[I].ProviderFlags) then
        Valid := Valid and not InputDataSet.Fields[I].IsNull;

    if (Valid) then
      for I := 0 to InputDataSet.FieldCount - 1 do
        if ((pfInWhere in InputDataSet.Fields[I].ProviderFlags)) then
        begin
          if (WhereClause <> '') then WhereClause := WhereClause + ' AND ';
          WhereClause := WhereClause + Session.EscapeIdentifier(InputDataSet.Fields[I].FieldName)
            + '=' + InputDataSet.SQLFieldValue(InputDataSet.Fields[I]);
        end;
  end;

  if (not Valid) then
    Result := ''
  else
    Result := 'UPDATE ' + TableClause + ' SET ' + SetClause + ' WHERE ' + WhereClause + ';' + #13#10;
end;

{ TSTriggers ******************************************************************}

function TSTriggers.Add(const AEntity: TSEntity; const ExecuteEvent: Boolean = False): Integer;
begin
  Assert(AEntity is TSTrigger);

  if (InsertIndex(AEntity.Name, Result)) then
    if (Result < TList(Self).Count) then
      TList(Self).Insert(Result, AEntity)
    else
      TList(Self).Add(AEntity);

  if (ExecuteEvent) then
  begin
    TSTrigger(AEntity).Invalidate();
    Session.InvalidObjects.Add(AEntity);
    Session.ExecuteEvent(etItemCreated, Database, Self, AEntity);
  end;
end;

function TSTriggers.Build(const DataSet: TMySQLQuery; const UseInformationSchema: Boolean; Filtered: Boolean = False; const SessionEvents: Boolean = True): Boolean;
var
  DeleteList: TList;
  Index: Integer;
  Name: string;
begin
  DeleteList := TList.Create();
  DeleteList.Assign(Self);

  if (not DataSet.IsEmpty()) then
    repeat
      if (not UseInformationSchema) then
        Name := DataSet.FieldByName('Name').AsString
      else if (Session.Databases.NameCmp(DataSet.FieldByName('TRIGGER_SCHEMA').AsString, Database.Name) = 0) then
        Name := DataSet.FieldByName('TRIGGER_NAME').AsString
      else
        Name := '';

      if (Name <> '') then
      begin
        if (InsertIndex(Name, Index)) then
          if (Index < Count) then
            Insert(Index, TSTrigger.Create(Self, Name))
          else
            Add(TSTrigger.Create(Self, Name))
        else if (DeleteList.IndexOf(Items[Index]) >= 0) then
          DeleteList.Delete(DeleteList.IndexOf(Items[Index]));

        if (not UseInformationSchema) then
        begin
          if (UpperCase(DataSet.FieldByName('Event').AsString) = 'INSERT') then
            Trigger[Index].FEvent := teInsert
          else if (UpperCase(DataSet.FieldByName('Event').AsString) = 'UPDATE') then
            Trigger[Index].FEvent := teUpdate
          else if (UpperCase(DataSet.FieldByName('Event').AsString) = 'DELETE') then
            Trigger[Index].FEvent := teDelete;
          if (not Assigned(DataSet.FindField('Definer'))) then
            Trigger[Index].FDefiner := ''
          else
            Trigger[Index].FDefiner := DataSet.FieldByName('Definer').AsString;
          Trigger[Index].FStmt := DataSet.FieldByName('Statement').AsString + ';';
          Trigger[Index].FTableName := DataSet.FieldByName('Table').AsString;
          if (UpperCase(DataSet.FieldByName('Timing').AsString) = 'BEFORE') then
            Trigger[Index].FTiming := ttBefore
          else if (UpperCase(DataSet.FieldByName('Timing').AsString) = 'AFTER') then
            Trigger[Index].FTiming := ttAfter;
        end
        else
        begin
          if (UpperCase(DataSet.FieldByName('EVENT_MANIPULATION').AsString) = 'INSERT') then
            Trigger[Index].FEvent := teInsert
          else if (UpperCase(DataSet.FieldByName('EVENT_MANIPULATION').AsString) = 'UPDATE') then
            Trigger[Index].FEvent := teUpdate
          else if (UpperCase(DataSet.FieldByName('EVENT_MANIPULATION').AsString) = 'DELETE') then
            Trigger[Index].FEvent := teDelete;
          Trigger[Index].FName := DataSet.FieldByName('TRIGGER_NAME').AsString;
          if (not Assigned(DataSet.FindField('DEFINER'))) then
            Trigger[Index].FDefiner := ''
          else
            Trigger[Index].FDefiner := DataSet.FieldByName('DEFINER').AsString;
          Trigger[Index].FStmt := DataSet.FieldByName('ACTION_STATEMENT').AsString + ';';
          Trigger[Index].FTableName := DataSet.FieldByName('EVENT_OBJECT_TABLE').AsString;
          if (UpperCase(DataSet.FieldByName('ACTION_TIMING').AsString) = 'BEFORE') then
            Trigger[Index].FTiming := ttBefore
          else if (UpperCase(DataSet.FieldByName('ACTION_TIMING').AsString) = 'AFTER') then
            Trigger[Index].FTiming := ttAfter;
        end;
        Trigger[Index].FValid := True;

        if (Database.Session.ServerVersion < 50121) then
          Trigger[Index].SetSource(Trigger[Index].GetSourceEx());

        if (Filtered and SessionEvents) then
          Session.ExecuteEvent(etItemValid, Session, Self, Trigger[Index]);
      end;
    until (not DataSet.FindNext() or (Session.Databases.NameCmp(DataSet.FieldByName('TRIGGER_SCHEMA').AsString, Database.Name) <> 0));

  Result := inherited;

  if (not Filtered) then
    while (DeleteList.Count > 0) do
    begin
      Index := IndexOf(DeleteList.Items[0]);
      Item[Index].Free();
      Delete(Index);
      DeleteList.Delete(0);
    end;
  DeleteList.Free();

  if (not Filtered and SessionEvents) then
  begin
    Session.ExecuteEvent(etItemsValid, Session, Session.Databases);
    Session.ExecuteEvent(etItemsValid, Database, Self);
  end;
  if (Database.Valid and SessionEvents) then
    Session.ExecuteEvent(etItemValid, Session, Session.Databases, Database);
end;

procedure TSTriggers.Delete(const AEntity: TSEntity);
var
  Index: Integer;
begin
  Assert(AEntity is TSTrigger);


  Index := IndexOf(AEntity);

  if (Index >= 0) then
  begin
    TList(Self).Delete(Index);

    Session.ExecuteEvent(etItemDropped, Database, Self, AEntity);

    AEntity.Free();
  end;
end;

function TSTriggers.GetTrigger(Index: Integer): TSTrigger;
begin
  Result := TSTrigger(Items[Index]);
end;

procedure TSTriggers.Invalidate();
var
  I: Integer;
begin
  inherited;

  for I := 0 to Count - 1 do
    Trigger[I].Invalidate();
end;

function TSTriggers.SQLGetItems(const Name: string = ''): string;
begin
  Result := 'SELECT * FROM ' + Session.EscapeIdentifier(INFORMATION_SCHEMA) + '.' + Session.EscapeIdentifier('TRIGGERS')
    + ' WHERE ' + Session.EscapeIdentifier('EVENT_OBJECT_SCHEMA') + '=' + SQLEscape(Database.Name);
  if (Name <> '') then
    Result := Result + ' AND ' + Session.EscapeIdentifier('TRIGGER_NAME') + '=' + SQLEscape(Name);
  Result := Result + ';' + #13#10;
end;

{ TSEvent *********************************************************************}

procedure TSEvent.Assign(const Source: TSEvent);
begin
  inherited Assign(Source);

  if (Assigned(Source.FDatabase)) then FDatabase := Source.FDatabase;

  FCreated := Source.Created;
  FComment := Source.Comment;
  FDefiner := Source.Definer;
  FEnabled := Source.Enabled;
  FEndDateTime := Source.EndDateTime;
  FEventType := Source.EventType;
  FExecute := Source.Execute;
  FIntervalType := Source.IntervalType;
  FIntervalValue := Source.IntervalValue;
  FPreserve := Source.Preserve;
  FStartDateTime := Source.StartDateTime;
  FStmt := Source.Stmt;
  FUpdated := Source.Updated;
end;

constructor TSEvent.Create(const ACDBObjects: TSDBObjects; const AName: string = '');
begin
  inherited;

  FCreated := 0;
  FComment := '';
  FDefiner := '';
  FEnabled := True;
  FEndDateTime := 0;
  FEventType := etUnknown;
  FExecute := 0;
  FIntervalType := itUnknown;
  FIntervalValue := '';
  FPreserve := False;
  FStartDateTime := 0;
  FStmt := '';
  FUpdated := 0;
end;

function TSEvent.GetEvents(): TSEvents;
begin
  Assert(CItems is TSEvents);

  Result := TSEvents(CItems);
end;

function TSEvent.GetSourceEx(const DropBeforeCreate: Boolean = False; const EncloseDefiner: Boolean = True): string;
var
  Index: Integer;
  Parse: TSQLParse;
  SQL: string;
begin
  SQL := FSource;

  if (SQLCreateParse(Parse, PChar(Source), Length(Source), Session.ServerVersion)) then
  begin
    if (not EncloseDefiner) then
    begin
      if (not SQLParseKeyword(Parse, 'CREATE')) then raise EConvertError.CreateFmt(SSourceParseError, [Database.Name + '.' + Name, SQL]);

      Index := SQLParseGetIndex(Parse);
      if (SQLParseKeyword(Parse, 'DEFINER')) then
      begin
        if (not SQLParseChar(Parse, '=')) then raise EConvertError.CreateFmt(SSourceParseError, [Database.Name + '.' + Name, SQL]);
        SQLParseValue(Parse);
        Delete(SQL, Index, SQLParseGetIndex(Parse) - Index);
      end;
    end;
  end;

  SQL := Trim(SQL) + #13#10;

  if (DropBeforeCreate) then
    SQL := 'DROP EVENT IF EXISTS ' + Session.EscapeIdentifier(Name) + ';' + #13#10 + SQL;

  Result := SQL;
end;

procedure TSEvent.ParseCreateEvent(const SQL: string);
var
  Parse: TSQLParse;
begin
  if (not SQLCreateParse(Parse, PChar(SQL), Length(SQL), Database.Session.ServerVersion)) then
    raise EConvertError.CreateFmt(SSourceParseError, [Database.Name + '.' + Name, SQL])
  else
  begin
    if (not SQLParseKeyword(Parse, 'CREATE')) then raise EConvertError.CreateFmt(SSourceParseError, [Database.Name + '.' + Name, SQL]);

    if (SQLParseKeyword(Parse, 'DEFINER')) then
    begin
      if (not SQLParseChar(Parse, '=')) then raise EConvertError.CreateFmt(SSourceParseError, [Database.Name + '.' + Name, SQL]);
      FDefiner := SQLParseValue(Parse);
    end;

    if (not SQLParseKeyword(Parse, 'EVENT')) then raise EConvertError.CreateFmt(SSourceParseError, [Database.Name + '.' + Name, SQL]);

    FName := SQLParseValue(Parse);
    if (SQLParseChar(Parse, '.')) then
    begin
      if (Database.Session.TableNameCmp(Database.Name, FName) = 0) then
        raise EConvertError.CreateFmt(SSourceParseError, [Database.Name + '.' + FName, SQL]);
      FName := SQLParseValue(Parse);
    end;

    if (not SQLParseKeyword(Parse, 'ON SCHEDULE')) then raise EConvertError.CreateFmt(SSourceParseError, [Database.Name + '.' + Name, SQL]);

    if (SQLParseKeyword(Parse, 'AT')) then
    begin
      FExecute := MySQLDB.StrToDateTime(SQLParseValue(Parse), Database.Session.FormatSettings);
      if (SQLParseChar(Parse, '+') and SQLParseKeyword(Parse, 'INTERVAL')) then
      begin
        FIntervalValue := SQLParseValue(Parse);
        FIntervalType := StrToIntervalType(SQLParseValue(Parse));
      end;
    end
    else if (SQLParseKeyword(Parse, 'EVERY')) then
    begin
      FIntervalValue := SQLParseValue(Parse);
      FIntervalType := StrToIntervalType(SQLParseValue(Parse));
      if (SQLParseKeyword(Parse, 'STARTS')) then
        FStartDateTime := MySQLDB.StrToDateTime(SQLParseValue(Parse), Database.Session.FormatSettings);
      if (SQLParseKeyword(Parse, 'ENDS')) then
        FEndDateTime := MySQLDB.StrToDateTime(SQLParseValue(Parse), Database.Session.FormatSettings);
    end
    else
      raise EConvertError.CreateFmt(SSourceParseError, [Database.Name + '.' + Name, SQL]);

    FPreserve := SQLParseKeyword(Parse, 'ON COMPLETION PRESERVE') or not SQLParseKeyword(Parse, 'ON COMPLETION NOT PRESERVE');

    FEnabled := SQLParseKeyword(Parse, 'ENABLE');
    if (not FEnabled and SQLParseKeyword(Parse, 'DISABLE')) then
      SQLParseKeyword(Parse, 'ON SLAVE');

    if (SQLParseKeyword(Parse, 'COMMENT')) then
      FComment := SQLParseValue(Parse);

    if (not SQLParseKeyword(Parse, 'DO')) then raise EConvertError.CreateFmt(SSourceParseError, [Database.Name + '.' + Name, SQL]);

    FStmt := SQLParseRest(Parse);
  end;
end;

procedure TSEvent.SetSource(const ADataSet: TMySQLQuery);
begin
  SetSource(ADataSet.FieldByName('Create Event'));

  if (Valid) then
  begin
    Session.ExecuteEvent(etItemsValid, Session.Databases);
    Session.ExecuteEvent(etItemValid, Database, Events, Self);
  end;
end;

procedure TSEvent.SetSource(const ASource: string);
begin
  inherited;

  ParseCreateEvent(FSource);
end;

function TSEvent.SQLGetSource(): string;
begin
  Result := 'SHOW CREATE EVENT ' + Session.EscapeIdentifier(Database.Name) + '.' + Session.EscapeIdentifier(Name) + ';' + #13#10
end;

function TSEvent.SQLRun(): string;
const
  ObjectIDEEventProcedureName = 'MySQL_Front_Object_IDE_Event';
begin
  Result := 'DROP PROCEDURE IF EXISTS ' + Session.EscapeIdentifier(Database.Name) + '.' + Session.EscapeIdentifier(ObjectIDEEventProcedureName) + ';' + #13#10;
  Result := Result + 'CREATE PROCEDURE ' + Session.EscapeIdentifier(Database.Name) + '.' + Session.EscapeIdentifier(ObjectIDEEventProcedureName) + '()' + #13#10;
  Result := Result + Stmt + #13#10;
  Result := Result + 'CALL ' + Session.EscapeIdentifier(Database.Name) + '.' + Session.EscapeIdentifier(ObjectIDEEventProcedureName) + '();' + #13#10;
  Result := Result + 'DROP PROCEDURE ' + Session.EscapeIdentifier(Database.Name) + '.' + Session.EscapeIdentifier(ObjectIDEEventProcedureName) + ';' + #13#10;
end;

{ TSEvents ********************************************************************}

function TSEvents.Build(const DataSet: TMySQLQuery; const UseInformationSchema: Boolean; Filtered: Boolean = False; const SessionEvents: Boolean = True): Boolean;
var
  DeleteList: TList;
  Index: Integer;
  Name: string;
begin
  DeleteList := TList.Create();
  DeleteList.Assign(Self);

  if (not DataSet.IsEmpty()) then
    repeat
      if ((not UseInformationSchema and (DataSet.FieldByName('Db').AsString = Database.Name))
        or (UseInformationSchema and (Session.Databases.NameCmp(DataSet.FieldByName('EVENT_SCHEMA').AsString, Database.Name) = 0))) then
      begin
        if (not UseInformationSchema) then
          Name := DataSet.FieldByName('Name').AsString
        else
          Name := DataSet.FieldByName('EVENT_NAME').AsString;

        if (InsertIndex(Name, Index)) then
          if (Index < Count) then
            Insert(Index, TSEvent.Create(Self, Name))
          else
            Add(TSEvent.Create(Self, Name))
        else if (DeleteList.IndexOf(Items[Index]) >= 0) then
          DeleteList.Delete(DeleteList.IndexOf(Items[Index]));

        if (not UseInformationSchema) then
        begin
//          Event[Index].FCreated := DataSet.FieldByName('CREATED').AsDateTime;
//          Event[Index].FComment := DataSet.FieldByName('EVENT_COMMENT').AsString;
          Event[Index].FDefiner := DataSet.FieldByName('Definer').AsString;
          Event[Index].FEnabled := DataSet.FieldByName('Status').AsString = 'ENABLED';
          Event[Index].FEndDateTime := DataSet.FieldByName('Ends').AsDateTime;
          Event[Index].FEventType := StrToEventType(DataSet.FieldByName('Type').AsString);
          Event[Index].FExecute := DataSet.FieldByName('Execute at').AsDateTime;
          Event[Index].FIntervalType := StrToIntervalType(DataSet.FieldByName('Interval field').AsString);
          Event[Index].FIntervalValue := DataSet.FieldByName('Interval value').AsString;
//          Event[Index].FPreserve := DataSet.FieldByName('ON_COMPLETION').AsString = 'PRESERVE';
          Event[Index].FStartDateTime := DataSet.FieldByName('Starts').AsDateTime;
//          Event[Index].FStmt := Trim(SQLTrim(DataSet.FieldByName('EVENT_DEFINITION').AsString));
//          Event[Index].FUpdated := DataSet.FieldByName('LAST_ALTERED').AsDateTime;
        end
        else
        begin
          Event[Index].FCreated := DataSet.FieldByName('CREATED').AsDateTime;
          Event[Index].FComment := DataSet.FieldByName('EVENT_COMMENT').AsString;
          Event[Index].FDefiner := DataSet.FieldByName('DEFINER').AsString;
          Event[Index].FEnabled := DataSet.FieldByName('STATUS').AsString = 'ENABLED';
          Event[Index].FEndDateTime := DataSet.FieldByName('ENDS').AsDateTime;
          Event[Index].FEventType := StrToEventType(DataSet.FieldByName('EVENT_TYPE').AsString);
          Event[Index].FExecute := DataSet.FieldByName('EXECUTE_AT').AsDateTime;
          Event[Index].FIntervalType := StrToIntervalType(DataSet.FieldByName('INTERVAL_FIELD').AsString);
          Event[Index].FIntervalValue := DataSet.FieldByName('INTERVAL_VALUE').AsString;
          Event[Index].FPreserve := DataSet.FieldByName('ON_COMPLETION').AsString = 'PRESERVE';
          Event[Index].FStartDateTime := DataSet.FieldByName('STARTS').AsDateTime;
          Event[Index].FStmt := SQLTrimStmt(DataSet.FieldByName('EVENT_DEFINITION').AsString);
          Event[Index].FUpdated := DataSet.FieldByName('LAST_ALTERED').AsDateTime;
        end;

        if (Copy(Event[Index].Stmt, Length(Event[Index].Stmt), 1) <> ';') then Event[Index].Stmt := Event[Index].Stmt + ';';

        if (Filtered and SessionEvents) then
          Session.ExecuteEvent(etItemsValid, Database, Self, Event[Index]);
      end;
    until (not DataSet.FindNext() or (Session.Databases.NameCmp(DataSet.FieldByName('EVENT_SCHEMA').AsString, Database.Name) <> 0));

  Result := inherited;

  if (not Filtered) then
    while (DeleteList.Count > 0) do
    begin
      Index := IndexOf(DeleteList.Items[0]);
      Item[Index].Free();
      Delete(Index);
      DeleteList.Delete(0);
    end;
  DeleteList.Free();

  if (not Filtered and SessionEvents) then
  begin
    Session.ExecuteEvent(etItemsValid, Session, Session.Databases);
    Session.ExecuteEvent(etItemsValid, Database, Self);
  end;
  if (Database.Valid and SessionEvents) then
    Session.ExecuteEvent(etItemValid, Session, Session.Databases, Database);
end;

function TSEvents.GetEvent(Index: Integer): TSEvent;
begin
  Result := TSEvent(Items[Index]);
end;

function TSEvents.SQLGetItems(const Name: string = ''): string;
begin
  Result := 'SELECT * FROM ' + Session.EscapeIdentifier(INFORMATION_SCHEMA) + '.' + Session.EscapeIdentifier('EVENTS')
    + ' WHERE ' + Session.EscapeIdentifier('EVENT_SCHEMA') + '=' + SQLEscape(Database.Name) + ';' + #13#10;
end;

{ TSDatabase ******************************************************************}

function TSDatabase.AddEvent(const NewEvent: TSEvent): Boolean;
begin
  Result := UpdateEvent(nil, NewEvent);
end;

function TSDatabase.AddRoutine(const SQLCreateRoutine: string): Boolean;
begin
  Result := UpdateRoutine(nil, SQLCreateRoutine);
end;

function TSDatabase.AddTable(const NewTable: TSBaseTable): Boolean;
begin
  NewTable.FForeignKeys.FValid := True;
  NewTable.FSourceParsed := True;
  Result := UpdateTable(nil, NewTable);
end;

function TSDatabase.AddTrigger(const NewTrigger: TSTrigger): Boolean;
begin
  NewTrigger.FDatabase := Self;
  Result := UpdateTrigger(nil, NewTrigger);
end;

function TSDatabase.AddView(const NewView: TSView): Boolean;
begin
  Result := UpdateView(nil, NewView);
end;

procedure TSDatabase.Assign(const Source: TSObject);
begin
  inherited Assign(Source);

  FDefaultCharset := TSDatabase(Source).DefaultCharset;
  FDefaultCodePage := TSDatabase(Source).DefaultCodePage;
  FCollation := TSDatabase(Source).Collation;
end;

function TSDatabase.BaseTableByName(const TableName: string): TSBaseTable;
var
  Index: Integer;
begin
  Index := Tables.IndexByName(TableName);
  if ((Index < 0) or not (Tables[Index] is TSBaseTable)) then
    Result := nil
  else
    Result := TSBaseTable(Tables[Index]);
end;

function TSDatabase.Build(const DataSet: TMySQLQuery): Boolean;
var
  Field: TField;
begin
  if (not DataSet.IsEmpty()) then
  begin
    Field := DataSet.FieldByName('Create Database');
    if (Field.DataType <> ftBlob) then
      FSource := Field.AsString
    else
      FSource := Session.LibDecode(my_char(Field.AsAnsiString));
    FSource := Trim(ReplaceStr(ReplaceStr(FSource, #10, #13#10), #13#13#10, #13#10));
    if (FSource <> '') then
      FSource := FSource + ';';
  end;

  Result := False;
end;

function TSDatabase.CheckTables(const Tables: TList): Boolean;
var
  I: Integer;
  SQL: string;
begin
  SQL := '';
  for I := 0 to Tables.Count - 1 do
    if (TObject(Tables[I]) is TSBaseTable) then
    begin
      if (SQL <> '') then SQL := SQL + ',';
      SQL := SQL + Session.EscapeIdentifier(Name) + '.' + Session.EscapeIdentifier(TSBaseTable(Tables[I]).Name);
    end;

  Result := SQL <> '';
  if (Result) then
  begin
    SQL := 'CHECK TABLE ' + SQL + ';' + #13#10;

    if (Session.DatabaseName <> Name) then
      SQL := SQLUse() + SQL;

    RepairTableList := TList.Create();
    Result := Session.ExecuteSQL(SQL, CheckTableEvent);
    if (Result and (RepairTableList.Count > 0)) then
      Result := RepairTables(RepairTableList);
    RepairTableList.Free();


    SQL := SQL + Self.Tables.SQLGetStatus(Tables);

    Session.SendSQL(SQL, Session.SessionResult);
  end;
end;

function TSDatabase.CheckTableEvent(const DataHandle: TMySQLConnection.TDataResult; const Data: Boolean): Boolean;
var
  DatabaseName: string;
  DataSet: TMySQLQuery;
  Parse: TSQLParse;
  TableName: string;
begin
  if (Data) then
  begin
    DataSet := TMySQLQuery.Create(nil);
    DataSet.Open(DataHandle);
    if (not DataSet.IsEmpty) then
      repeat
        if ((lstrcmpi(PChar(DataSet.FieldByName('Msg_text').AsString), 'OK') <> 0)
          and SQLCreateParse(Parse, PChar(DataSet.FieldByName('Table').AsString), Length(DataSet.FieldByName('Table').AsString), Session.ServerVersion)) then
        begin
          DatabaseName := Name;
          if (SQLParseObjectName(Parse, DatabaseName, TableName)) then
            RepairTableList.Add(Session.DatabaseByName(DatabaseName).BaseTableByName(TableName));
        end;
      until (not DataSet.FindNext());
    DataSet.Free();
  end;

  Result := False;
end;

function TSDatabase.CloneRoutine(const Routine: TSRoutine; const NewRoutineName: string): Boolean;
var
  DatabaseName: string;
  Parse: TSQLParse;
  RoutineName: string;
  SQL: string;
begin
  if (not SQLCreateParse(Parse, PChar(Routine.Source), Length(Routine.Source), Session.ServerVersion)) then
    Result := False
  else
  begin
    if (not SQLParseKeyword(Parse, 'CREATE')) then raise EConvertError.CreateFmt(SSourceParseError, [Name + '.' + Routine.Name, SQL]);

    if (SQLParseKeyword(Parse, 'DEFINER')) then
    begin
      if (not SQLParseChar(Parse, '=')) then raise EConvertError.CreateFmt(SSourceParseError, [Name, SQL]);
      SQLParseValue(Parse);
    end;

    if (not SQLParseKeyword(Parse, 'PROCEDURE') and not SQLParseKeyword(Parse, 'FUNCTION')) then
      raise EConvertError.CreateFmt(SSourceParseError, [Name + '.' + Routine.Name, SQL]);

    SQL := LeftStr(Routine.Source, SQLParseGetIndex(Parse) - 1);

    DatabaseName := Name;
    if (not SQLParseObjectName(Parse, DatabaseName, RoutineName)) then
      raise EConvertError.CreateFmt(SSourceParseError, [Name + '.' + Routine.Name, SQL]);

    SQL := SQL + Session.EscapeIdentifier(Name) + '.' + Session.EscapeIdentifier(NewRoutineName);
    SQL := SQL + RightStr(Routine.Source, Length(Routine.Source) - (SQLParseGetIndex(Parse) - 1));
    SQL := SQL + #13#10;

    if ((Routine.RoutineType = rtProcedure) and Assigned(ProcedureByName(NewRoutineName))) then
      SQL := 'DROP PROCEDURE ' + Session.EscapeIdentifier(Name) + '.' + Session.EscapeIdentifier(Routine.Name) + ';' + #13#10
    else if ((Routine.RoutineType = rtFunction) and Assigned(FunctionByName(NewRoutineName))) then
      SQL := 'DROP FUNCTION ' + Session.EscapeIdentifier(Name) + '.' + Session.EscapeIdentifier(Routine.Name) + ';' + #13#10;

    Result := Session.ExecuteSQL(SQL);
  end;
end;

function TSDatabase.CloneTable(const Table: TSBaseTable; const NewTableName: string; const Data: Boolean): Boolean;
var
  NewTable: TSBaseTable;
  SQL: string;
begin
  Session.BeginSynchron();

  Result := Assigned(Table);

  if (Result) then
  begin
    Table.Update();

    NewTable := TSBaseTable.Create(Tables);
    NewTable.Assign(Table);
    NewTable.Name := NewTableName;
    if (not Data) then
      NewTable.AutoIncrement := 0;
    SQL := Trim(SQLAlterTable(nil, NewTable, not Data or (Session.ServerVersion >= 40100)));
    NewTable.Free();

    if (Data) then
      Insert(' SELECT * FROM ' + Session.EscapeIdentifier(Table.Database.Name) + '.' + Session.EscapeIdentifier(Table.Name), SQL, Length(SQL));

    if (Assigned(TableByName(NewTableName))) then
      if (TableByName(NewTableName) is TSBaseTable) then
        SQL := 'DROP TABLE ' + Session.EscapeIdentifier(NewTableName) + ';' + #13#10 + SQL
      else
        SQL := 'DROP VIEW ' + Session.EscapeIdentifier(NewTableName) + ';' + #13#10 + SQL;

    if (Session.DatabaseName <> Name) then
      SQL := SQLUse() + SQL;

    Result := Session.ExecuteSQL(SQL);
  end;

  if (Result) then
    if ((Session.ServerVersion < 40100) and Assigned(Table.AutoIncrementField)) then
    begin
      BaseTableByName(NewTableName).Update();

      NewTable := TSBaseTable.Create(Tables);
      NewTable.Assign(BaseTableByName(NewTableName));
      NewTable.FieldByName(Table.AutoIncrementField.Name).AutoIncrement := True;
      Result := UpdateTable(BaseTableByName(NewTableName), NewTable);
      NewTable.Free();
    end;
  Session.EndSynchron();
end;

function TSDatabase.CloneView(const View: TSView; const NewViewName: string): Boolean;
var
  SQL: string;
begin
  SQL := ReplaceStr(View.GetSourceEx(), 'VIEW ' + Session.EscapeIdentifier(View.Name), 'VIEW ' + Session.EscapeIdentifier(Name) + '.' + Session.EscapeIdentifier(NewViewName));

  if (Assigned(TableByName(NewViewName))) then
    SQL := 'DROP VIEW ' + Session.EscapeIdentifier(NewViewName) + ';' + #13#10 + SQL;

  if (Session.DatabaseName <> Name) then
    SQL := SQLUse() + SQL;

  Result := Session.ExecuteSQL(SQL);
end;

constructor TSDatabase.Create(const ASession: TSSession = nil; const AName: string = '');
begin
  inherited Create(ASession.Databases, ASession.TableName(AName));

  FCollation := '';
  FDefaultCharset := '';
  FDefaultCodePage := CP_ACP;

  if ((Session.ServerVersion < 50004) or (Self is TSSystemDatabase)) then FRoutines := nil else FRoutines := TSRoutines.Create(Self);
  FTables := TSTables.Create(Self);
  if ((Session.ServerVersion < 50010) or (Self is TSSystemDatabase)) then FTriggers := nil else FTriggers := TSTriggers.Create(Self);
  if ((Session.ServerVersion < 50106) or (Self is TSSystemDatabase)) then FEvents := nil else FEvents := TSEvents.Create(Self);
end;

function TSDatabase.DeleteObject(const DBObject: TSDBObject): Boolean;
var
  List: TList;
begin
  List := TList.Create();
  List.Add(DBObject);
  Result := Session.DeleteEntities(List);
  List.Free();
end;

destructor TSDatabase.Destroy();
begin
  FreeDesktop();

  FTables.Free();
  if (Assigned(FRoutines)) then FRoutines.Free();
  if (Assigned(FTriggers)) then FTriggers.Free();
  if (Assigned(FEvents)) then FEvents.Free();

  inherited;
end;

function TSDatabase.EventByName(const EventName: string): TSEvent;
var
  Index: Integer;
begin
  Index := Events.IndexByName(EventName);
  if (Index < 0) then
    Result := nil
  else
    Result := Events[Index];
end;

function TSDatabase.EmptyTables(const Tables: TList = nil): Boolean;
var
  I: Integer;
  SQL: string;
  WorkingList: TList;
begin
  SQL := '';

  WorkingList := TList.Create();
  if (not Assigned(Tables)) then
    WorkingList.Assign(Self.Tables)
  else
    WorkingList.Assign(Tables);

  for I := 0 to WorkingList.Count - 1 do
    if (TObject(WorkingList[I]) is TSBaseTable) then
    begin
      TSBaseTable(WorkingList[I]).InvalidateStatus();
      SQL := SQL + SQLTruncateTable(TSBaseTable(WorkingList[I]));
    end;

  WorkingList.Free();

  Result := (SQL = '') or Session.SendSQL(SQL);
end;

function TSDatabase.FlushTables(const Tables: TList): Boolean;
var
  I: Integer;
  SQL: string;
begin
  SQL := '';
  for I := 0 to Tables.Count - 1 do
    if (TObject(Tables[I]) is TSBaseTable) then
    begin
      if (SQL <> '') then SQL := SQL + ',';
      SQL := SQL + Session.EscapeIdentifier(Name) + '.' + Session.EscapeIdentifier(TSBaseTable(Tables[I]).Name);

      TSBaseTable(Tables[I]).InvalidateStatus();
    end;
  SQL := 'FLUSH TABLE ' + SQL + ';' + #13#10;

  if (Session.DatabaseName <> Name) then
    SQL := SQLUse() + SQL;

  Result := Session.SendSQL(SQL, Session.SessionResult);
end;

procedure TSDatabase.FreeDesktop();
var
  I: Integer;
begin
  for I := 0 to Tables.Count - 1 do
    Tables[I].FreeDesktop();
  if (Assigned(Routines)) then
    for I := 0 to Routines.Count - 1 do
      Routines[I].FreeDesktop();
  if (Assigned(Triggers)) then
    for I := 0 to Triggers.Count - 1 do
      Triggers[I].FreeDesktop();
  if (Assigned(Events)) then
    for I := 0 to Events.Count - 1 do
      Events[I].FreeDesktop();

  inherited;
end;

function TSDatabase.FunctionByName(const FunctionName: string): TSFunction;
var
  I: Integer;
begin
  Result := nil;

  for I := 0 to Routines.Count - 1 do
    if ((lstrcmpi(PChar(Routines[I].Name), PChar(FunctionName)) = 0) and (Routines[I] is TSFunction)) then
      Result := TSFunction(Routines[I]);
end;

function TSDatabase.GetCollation(): string;
begin
  if ((FCollation = '') and (Source <> '')) then
    ParseCreateDatabase(Source);

  Result := FCollation;
end;

function TSDatabase.GetCount(): Integer;
begin
  if (Tables.Valid or Assigned(Routines) and Routines.Valid or Assigned(Events) and Events.Valid) then
  begin
    Result := Tables.Count;
    if (Assigned(Routines)) then Inc(Result, Routines.Count);
    if (Assigned(Events)) then Inc(Result, Events.Count);
  end
  else
    Result := -1;
end;

function TSDatabase.GetCreated(): TDateTime;
var
  I: Integer;
begin
  Result := 0;

  for I := 0 to Tables.Count - 1 do
    if ((Tables[I] is TSBaseTable) and (TSBaseTable(Tables[I]).Created >= 0) and ((Result = 0) or (Result > TSBaseTable(Tables[I]).Created))) then
      Result := TSBaseTable(Tables[I]).Created;
end;

function TSDatabase.GetDatabases(): TSDatabases;
begin
  Assert(CItems is TSDatabases);

  Result := TSDatabases(CItems);
end;

function TSDatabase.GetDefaultCharset(): string;
begin
  if ((FDefaultCharset = '') and (Source <> '')) then
    ParseCreateDatabase(Source);

  Result := FDefaultCharset;
end;

function TSDatabase.GetDataSize(): Int64;
// Result in Byte
var
  I: Integer;
begin
  if (not Tables.Valid) then
    Result := -1
  else
  begin
    Result := 0;
    for I := 0 to Tables.Count - 1 do
      if ((Tables[I] is TSBaseTable) and TSBaseTable(Tables[I]).ValidStatus) then
        Inc(Result, TSBaseTable(Tables[I]).DataSize);
  end;
end;

function TSDatabase.GetIndexSize(): Int64;
// Result in Byte
var
  I: Integer;
begin
  if (not Tables.Valid) then
    Result := -1
  else
  begin
    Result := 0;
    for I := 0 to Tables.Count - 1 do
      if ((Tables[I] is TSBaseTable) and TSBaseTable(Tables[I]).ValidStatus) then
        Inc(Result, TSBaseTable(Tables[I]).IndexSize);
  end;
end;

function TSDatabase.GetMaxDataSize(): Int64;
// Result in Byte
var
  I: Integer;
begin
  if (not Tables.Valid) then
    Result := -1
  else
  begin
    Result := 0;
    for I := 0 to Tables.Count - 1 do
      if ((Tables[I] is TSBaseTable) and TSBaseTable(Tables[I]).ValidStatus) then
        Inc(Result, TSBaseTable(Tables[I]).MaxDataSize);
  end;
end;

function TSDatabase.GetSource(): string;
begin
  if (FSource = '') then
    FSource := GetSourceEx();

  Result := FSource;
end;

function TSDatabase.GetSourceEx(const DropBeforeCreate: Boolean = False): string;
var
  SQL: string;
begin
  if (FSource = '') then
    SQL := ''
  else
  begin
    SQL := Trim(FSource) + #13#10;

    if ((SQL <> '') and DropBeforeCreate) then
      SQL := 'DROP DATABASE IF EXISTS ' + Session.EscapeIdentifier(Name) + ';' + #13#10 + SQL;
  end;

  Result := SQL;
end;

function TSDatabase.GetUpdated(): TDateTime;
var
  I: Integer;
begin
  Result := 0;
  for I := 0 to Tables.Count - 1 do
    if ((Tables[I] is TSBaseTable) and ((Result = 0) or (Result < TSBaseTable(Tables[I]).Updated))) then
      Result := TSBaseTable(Tables[I]).Updated;
end;

function TSDatabase.GetValid(): Boolean;
begin
  Result := Assigned(Tables) and Tables.Valid
    and (not Assigned(Routines) or Routines.Valid)
    and (not Assigned(Triggers) or Triggers.Valid)
    and (not Assigned(Events) or Events.Valid);
end;

function TSDatabase.GetValidSource(): Boolean;
begin
  if (Self is TSSystemDatabase) then
    Result := True
  else if (Session.ServerVersion < 40101) then
  begin
    if (FSource = '') then
      FSource := 'CREATE DATABASE ' + Session.EscapeIdentifier(Name) + ';' + #13#10;
    Result := True;
  end
  else
    Result := inherited;
end;

function TSDatabase.GetValidSources(): Boolean;
var
  I: Integer;
begin
  Result := ValidSource;
  for I := 0 to Tables.Count - 1 do
    Result := Result and Tables[I].ValidSource;
  if (Assigned(Routines)) then
  begin
    Result := Result and Routines.Valid;
    for I := 0 to Routines.Count - 1 do
      Result := Result and Routines[I].ValidSource;
  end;
  if (Assigned(Triggers)) then
  begin
    Result := Result and Triggers.Valid;
    for I := 0 to Triggers.Count - 1 do
      Result := Result and Triggers[I].ValidSource;
  end;
  if (Assigned(Events)) then
  begin
    Result := Result and Routines.Valid;
    for I := 0 to Events.Count - 1 do
      Result := Result and Events[I].ValidSource;
  end;
end;

procedure TSDatabase.Invalidate();
begin
  inherited;

  Tables.Invalidate();
  if (Assigned(Routines)) then Routines.Invalidate();
  if (Assigned(Triggers)) then Triggers.Invalidate();
  if (Assigned(Events)) then Events.Invalidate();
end;

function TSDatabase.OptimizeTables(const Tables: TList): Boolean;
var
  I: Integer;
  SQL: string;
begin
  SQL := '';
  for I := 0 to Tables.Count - 1 do
    if (TObject(Tables[I]) is TSBaseTable) then
    begin
      if (SQL <> '') then SQL := SQL + ',';
      SQL := SQL + Session.EscapeIdentifier(Name) + '.' + Session.EscapeIdentifier(TSBaseTable(Tables[I]).Name);
    end;

  Result := SQL <> '';
  if (Result) then
  begin
    SQL := 'OPTIMIZE TABLE ' + SQL + ';' + #13#10;

    SQL := SQL + Self.Tables.SQLGetStatus(Tables);

    if (Session.DatabaseName <> Name) then
      SQL := SQLUse() + SQL;

    Result := Session.SendSQL(SQL, Session.SessionResult);
  end;
end;

procedure TSDatabase.ParseCreateDatabase(const SQL: string);
var
  Parse: TSQLParse;
begin
  if (SQLCreateParse(Parse, PChar(SQL), Length(SQL), Session.ServerVersion)) then
  begin
    if (not SQLParseKeyword(Parse, 'CREATE DATABASE')) then raise EConvertError.CreateFmt(SSourceParseError, [Name, SQL]);

    FName := SQLParseValue(Parse);

    if (SQLParseKeyword(Parse, 'DEFAULT CHARACTER SET') or SQLParseKeyword(Parse, 'CHARACTER SET')) then
      FDefaultCharset := LowerCase(SQLParseValue(Parse))
    else
      FDefaultCharset := '';

    if (SQLParseKeyword(Parse, 'DEFAULT COLLATE') or SQLParseKeyword(Parse, 'COLLATE')) then
      FCollation := LowerCase(SQLParseValue(Parse))
    else if (FDefaultCharset <> '') then
      FCollation := Session.CharsetByName(FDefaultCharset).DefaultCollation.Caption
    else
      FCollation := '';
  end;
end;

function TSDatabase.ParseDependencies(const SQL: string; var Dependencies: TSDependencies): Boolean;
begin
  Result := False;
end;

function TSDatabase.ProcedureByName(const ProcedureName: string): TSProcedure;
var
  I: Integer;
begin
  Result := nil;

  for I := 0 to Routines.Count - 1 do
    if ((lstrcmpi(PChar(Routines[I].Name), PChar(ProcedureName)) = 0) and (Routines[I] is TSProcedure)) then
      Result := TSProcedure(Routines[I]);
end;

procedure TSDatabase.PushBuildEvents();
begin
  Session.ExecuteEvent(etItemsValid, Self, Tables);
  if (Assigned(Routines)) then
    Session.ExecuteEvent(etItemsValid, Self, Routines);
  if (Assigned(Events)) then
    Session.ExecuteEvent(etItemsValid, Self, Events);
end;

function TSDatabase.RenameTable(const Table: TSTable; const NewTableName: string): Boolean;
var
  NewView: TSView;
begin
  if (NewTableName = Table.Name) then
    Result := True
  else
  begin
    if (Assigned(Table.FDataSet)) then
      Table.FDataSet.CommandText := NewTableName;

    if ((Table is TSView) and (Session.ServerVersion < 50014)) then
    begin
      NewView := TSView.Create(Tables);
      NewView.Assign(TSView(Table));
      NewView.FName := NewTableName;
      Result := UpdateView(TSView(Table), NewView);
      NewView.Free();
    end
    else
      Result := Session.SendSQL('RENAME TABLE ' + Session.EscapeIdentifier(Table.Database.Name) + '.' + Session.EscapeIdentifier(Table.Name) + ' TO ' + Session.EscapeIdentifier(Name) + '.' + Session.EscapeIdentifier(NewTableName) + ';');
  end;
end;

function TSDatabase.RepairTables(const Tables: TList): Boolean;
var
  I: Integer;
  SQL: string;
begin
  SQL := '';
  for I := 0 to Tables.Count - 1 do
    if (TObject(Tables[I]) is TSBaseTable) then
    begin
      if (SQL <> '') then SQL := SQL + ',';
      SQL := SQL + Session.EscapeIdentifier(Name) + '.' + Session.EscapeIdentifier(TSBaseTable(Tables[I]).Name);
    end;
  SQL := 'REPAIR TABLE ' + SQL + ';' + #13#10;

  SQL := SQL + Self.Tables.SQLGetStatus(Tables);

  if (Session.DatabaseName <> Name) then
    SQL := SQLUse() + SQL;

  Result := Session.ExecuteSQL(SQL);
end;

procedure TSDatabase.SetDefaultCharset(const ADefaultCharset: string);
begin
  FDefaultCharset := LowerCase(ADefaultCharset);

  if (not Assigned(Session)) then
    FDefaultCodePage := CP_ACP
  else
    FDefaultCodePage := Session.CharsetToCodePage(FDefaultCharset);
end;

procedure TSDatabase.SetName(const AName: string);
begin
  if (Session.LowerCaseTableNames = 1) then
    inherited SetName(LowerCase(AName))
  else
    inherited SetName(AName);
end;

procedure TSDatabase.SetSource(const ADataSet: TMySQLQuery);
begin
  SetSource(ADataSet.FieldByName('Create Database'));

  ParseCreateDatabase(Source);

  if (Valid) then
    Session.ExecuteEvent(etItemValid, Session, Databases, Self);
end;

function TSDatabase.SQLAlterTable(const Table, NewTable: TSBaseTable; const EncloseFields: Boolean): string;
var
  AutoIncrementField: TSBaseTableField;
  FieldNames: string;
  ForeignKeyAdded: Boolean;
  ForeignKeyDropClausel: string;
  Found: Boolean;
  I: Integer;
  J: Integer;
  Modified: Boolean;
  NewField: TSBaseTableField;
  NewForeignKey: TSForeignKey;
  NewKey: TSKey;
  NewKeyColumn: TSKeyColumn;
  NewPartition: TSPartition;
  OldField: TSBaseTableField;
  OldForeignKey: TSForeignKey;
  OldKey: TSKey;
  OldPartition: TSPartition;
  SQL: string;
  SQLPart: string;
begin
  AutoIncrementField := nil;
  for I := 0 to NewTable.Fields.Count - 1 do
    if (NewTable.Fields[I].AutoIncrement) then
      AutoIncrementField := TSBaseTableField(NewTable.Fields[I]);
  Found := False;
  for I := 0 to NewTable.Keys.Count - 1 do
    if (NewTable.Keys[I].PrimaryKey) then
      for J := 0 to NewTable.Keys[I].Columns.Count - 1 do
        if (NewTable.Keys[I].Columns[J].Field = AutoIncrementField) then
          Found := True;
  if (not Found and Assigned(AutoIncrementField)) then
    if (not Assigned(NewTable.PrimaryKey)) then
    begin
      NewKey := TSKey.Create(NewTable.Keys);
      NewKey.PrimaryKey := True;
      NewKeyColumn := TSKeyColumn.Create(NewKey.Columns);
      NewKeyColumn.Field := AutoIncrementField;
      NewKey.Columns.AddColumn(NewKeyColumn);
      NewTable.Keys.AddKey(NewKey);
      FreeAndNil(NewKeyColumn);
      FreeAndNil(NewKey);
    end
    else
      AutoIncrementField.AutoIncrement := False;

  if (EncloseFields) then
    for I := 0 to NewTable.Fields.Count - 1 do
    begin
      NewField := TSBaseTableField(NewTable.Fields.Field[I]);
      if (not Assigned(Table) or (NewField.OriginalName = '')) then
        OldField := nil
      else
        OldField := Table.FieldByName(NewField.OriginalName);
      if (not Assigned(OldField) or (not NewField.Equal(OldField) or NewField.Moved)) then
      begin
        SQLPart := '';
        if (SQL <> '') then SQLPart := SQLPart + ',' + #13#10; SQLPart := SQLPart + '  ';
        if (not Assigned(Table)) then
          SQLPart := SQLPart + Session.EscapeIdentifier(NewField.Name)
        else if (not Assigned(OldField)) then
          SQLPart := SQLPart + 'ADD COLUMN ' + Session.EscapeIdentifier(NewField.Name)
        else
          SQLPart := SQLPart + 'CHANGE COLUMN ' + Session.EscapeIdentifier(OldField.Name) + ' ' + Session.EscapeIdentifier(NewField.Name);
        SQLPart := SQLPart + ' ' + NewField.DBTypeStr();

        if (NewField.FieldKind = mkReal) then
        begin
          if ((NewField.FieldType in TextFieldTypes) and (Session.ServerVersion >= 40101)) then
          begin
            if ((NewField.Charset <> '')
              and ((NewField.Charset <> '') and (NewField.Charset <> NewTable.DefaultCharset))) then
              SQLPart := SQLPart + ' CHARACTER SET ' + NewField.Charset;
            if ((NewField.Collation <> '') and (NewField.Collation <> NewTable.FCollation)) then
              SQLPart := SQLPart + ' COLLATE ' + NewField.Collation;
          end;
          if (not NewField.NullAllowed) then SQLPart := SQLPart + ' NOT'; SQLPart := SQLPart + ' NULL';
          if (NewField.AutoIncrement) then
            SQLPart := SQLPart + ' AUTO_INCREMENT'
          else if ((NewField.Default <> '') and not (NewField.FieldType in [mfTinyText, mfText, mfMediumText, mfLongText, mfTinyBlob, mfBlob, mfMediumBlob, mfLongBlob])) then
            SQLPart := SQLPart + ' DEFAULT ' + NewField.Default;
          if ((NewField.OnUpdate <> '') and (NewField.FieldType = mfTimeStamp)) then
            SQLPart := SQLPart + ' ON UPDATE ' + NewField.OnUpdate;
          if ((Session.ServerVersion >= 40100) and (NewField.Comment <> '')) then
            SQLPart := SQLPart + ' COMMENT ' + SQLEscape(NewField.Comment);
        end
        else if (NewField.FieldKind = mkVirtual) then
        begin
          SQLPart := SQLPart + ' AS (' + NewField.Expression + ')';
          if (NewField.Stored = msVirtual) then
            SQLPart := SQLPart + ' VIRTUAL'
          else if (NewField.Stored = msStored) then
            SQLPart := SQLPart + ' STORED';
          if (NewField.Comment <> '') then
            SQLPart := SQLPart + ' COMMENT ' + SQLEscape(NewField.Comment);
          if (not NewField.NullAllowed) then
            SQLPart := SQLPart + ' NOT NULL';
        end
        else
          raise ERangeError.Create(SRangeError);
        if ((Session.ServerVersion >= 40100) and (NewField.Comment <> '')) then
          SQLPart := SQLPart + ' COMMENT ' + SQLEscape(NewField.Comment);
        if (Assigned(Table) and (not Assigned(OldField) or (Session.ServerVersion >= 40001))) then
          if (not Assigned(NewField.FieldBefore) and (not Assigned(OldField) or Assigned(OldField.FieldBefore))) then
            SQLPart := SQLPart + ' FIRST'
          else if (Assigned(NewField.FieldBefore) and ((not Assigned(OldField) and Assigned(NewField.FieldBefore) and (NewField.FieldBefore.Index <> NewTable.Fields.Count - 2)) or (Assigned(OldField) and (not Assigned(OldField.FieldBefore) or (lstrcmpi(PChar(OldField.FieldBefore.Name), PChar(NewField.FieldBefore.Name)) <> 0) or (TSBaseTableField(NewField.FieldBefore).Moved))))) then
            SQLPart := SQLPart + ' AFTER ' + Session.EscapeIdentifier(NewField.FieldBefore.Name);

        SQL := SQL + SQLPart;
      end;
    end;

  for I := 0 to NewTable.Keys.Count - 1 do
  begin
    Modified := False;
    if (Assigned(Table)) then
      for J := 0 to Table.Keys.Count - 1 do
        if (not NewTable.Keys[I].Created and (NewTable.Keys.NameCmp(NewTable.Keys[I].OriginalName, Table.Keys[J].OriginalName) = 0)) then
          Modified := not NewTable.Keys[I].Equal(Table.Keys[J]);

    if (not Assigned(Table) or Modified or NewTable.Keys[I].Created) then
    begin
      NewKey := NewTable.Keys[I];

      SQLPart := '';
      SQLPart := SQLPart + '  ';
      if (Assigned(Table)) then SQLPart := SQLPart + 'ADD ';
      if (NewKey.PrimaryKey) then
        SQLPart := SQLPart + 'PRIMARY KEY'
      else if (NewKey.Unique) then
        SQLPart := SQLPart + 'UNIQUE INDEX'
      else if (NewKey.Fulltext) then
        SQLPart := SQLPart + 'FULLTEXT INDEX'
      else
        SQLPart := SQLPart + 'INDEX';
      if (NewKey.Name <> '') then SQLPart := SQLPart + ' ' + Session.EscapeIdentifier(NewKey.Name);

      if ((((40100 <= Session.ServerVersion) and (Session.ServerVersion < 50060)) or ((50100 <= Session.ServerVersion) and (Session.ServerVersion < 50110))) and (NewKey.IndexType <> '')) then
        SQLPart := SQLPart + ' USING=' + NewKey.IndexType;

      FieldNames := '';
      for J := 0 to NewKey.Columns.Count - 1 do
      begin
        if (FieldNames <> '') then FieldNames := FieldNames + ',';
        FieldNames := FieldNames + Session.EscapeIdentifier(NewKey.Columns.Column[J].Field.Name);
        if ((NewKey.Columns.Column[J].Field.FieldType in [mfChar, mfVarChar, mfTinyText, mfText, mfMediumText, mfLongText]) and (NewKey.Columns.Column[J].Length > 0)) then
          FieldNames := FieldNames + '(' + IntToStr(NewKey.Columns.Column[J].Length) + ')';
      end;
      SQLPart := SQLPart + ' (' + FieldNames + ')';

      if ((Session.ServerVersion >= 50110) and (NewKey.BlockSize > 0)) then
        SQLPart := SQLPart + ' KEY_BLOCK_SIZE ' + IntToStr(NewKey.BlockSize);
      if ((((50060 <= Session.ServerVersion) and (Session.ServerVersion < 50100)) or (50110 <= Session.ServerVersion)) and (NewKey.IndexType <> '')) then
        SQLPart := SQLPart + ' USING ' + NewKey.IndexType;
      if ((Session.ServerVersion >= 50503) and (NewKey.Comment <> '')) then
        SQLPart := SQLPart + ' COMMENT ' + SQLEscape(NewKey.Comment);

      if (SQL <> '') then SQL := SQL + ',' + #13#10;
      SQL := SQL + SQLPart;
    end;
  end;

  ForeignKeyAdded := False;
  for I := 0 to NewTable.ForeignKeys.Count - 1 do
  begin
    Modified := False;
    if (Assigned(Table)) then
      for J := 0 to Table.ForeignKeys.Count - 1 do
        if (not NewTable.ForeignKeys[I].Created and (lstrcmpi(PChar(NewTable.ForeignKeys[I].OriginalName), PChar(Table.ForeignKeys[J].OriginalName)) = 0)) then
          Modified := not NewTable.ForeignKeys[I].Equal(Table.ForeignKeys[J]);

    if (not Assigned(Table) or Modified or NewTable.ForeignKeys[I].Created and (NewTable.ForeignKeys[I].Parent.TableName <> '')) then
    begin
      NewForeignKey := NewTable.ForeignKeys[I];

      SQLPart := '';
      if (SQL <> '') then SQLPart := SQLPart + ',' + #13#10; SQLPart := SQLPart + '  ';
      if (Assigned(Table)) then
      begin
        SQLPart := SQLPart + 'ADD ';
        ForeignKeyAdded := True;
      end;
      if (NewForeignKey.Name <> '') then
        SQLPart := SQLPart + 'CONSTRAINT ' + Session.EscapeIdentifier(NewForeignKey.Name) + ' ';
      SQLPart := SQLPart + 'FOREIGN KEY (';
      for J := 0 to Length(NewForeignKey.Fields) - 1 do
      if (Assigned(NewForeignKey.Fields[J])) then
        begin
          if (J > 0) then SQLPart := SQLPart + ',';
          SQLPart := SQLPart + Session.EscapeIdentifier(NewForeignKey.Fields[J].Name);
        end;
      SQLPart := SQLPart + ') REFERENCES ' + Session.EscapeIdentifier(NewForeignKey.Parent.DatabaseName) + '.' + Session.EscapeIdentifier(NewForeignKey.Parent.TableName) + ' (';
      for J := 0 to Length(NewForeignKey.Parent.FieldNames) - 1 do
      begin
        if (J > 0) then SQLPart := SQLPart + ',';
        SQLPart := SQLPart + Session.EscapeIdentifier(NewForeignKey.Parent.FieldNames[J]);
      end;
      SQLPart := SQLPart + ')';

      if (NewForeignKey.Match = mtFull) then
        SQLPart := SQLPart + ' MATCH FULL'
      else if (NewForeignKey.Match = mtPartial) then
        SQLPart := SQLPart + ' MATCH PARTIAL';

      if (NewForeignKey.OnDelete = dtNoAction) then SQLPart := SQLPart + ' ON DELETE NO ACTION';
      if (NewForeignKey.OnDelete = dtRestrict) then SQLPart := SQLPart + ' ON DELETE RESTRICT';
      if (NewForeignKey.OnDelete = dtCascade) then SQLPart := SQLPart + ' ON DELETE CASCADE';
      if (NewForeignKey.OnDelete = dtSetNull) then SQLPart := SQLPart + ' ON DELETE SET NULL';
      if (NewForeignKey.OnDelete = dtSetDefault) then SQLPart := SQLPart + ' ON DELETE SET DEFAULT';
      if (NewForeignKey.OnUpdate = utNoAction) then SQLPart := SQLPart + ' ON UPDATE NO ACTION';
      if (NewForeignKey.OnUpdate = utRestrict) then SQLPart := SQLPart + ' ON UPDATE RESTRICT';
      if (NewForeignKey.OnUpdate = utCascade) then SQLPart := SQLPart + ' ON UPDATE CASCADE';
      if (NewForeignKey.OnUpdate = utSetNull) then SQLPart := SQLPart + ' ON UPDATE SET NULL';
      if (NewForeignKey.OnUpdate = utSetDefault) then SQLPart := SQLPart + ' ON UPDATE SET DEFAULT';

      SQL := SQL + SQLPart;
    end;
  end;

  if (Assigned(Table)) then
    for I := 0 to Table.Fields.Count - 1 do
    begin
      OldField := TSBaseTableField(Table.Fields[I]);
      Found := False;
      for J := 0 to NewTable.Fields.Count - 1 do
      begin
        NewField := TSBaseTableField(NewTable.Fields[J]);
        if (lstrcmpi(PChar(NewField.OriginalName), PChar(OldField.Name)) = 0) then
          Found := True;
      end;
      if (not Found) then
      begin
        if (SQL <> '') then SQL := SQL + ',' + #13#10;
        SQL := SQL + '  DROP COLUMN ' + Session.EscapeIdentifier(OldField.Name);
      end;
    end;

  if (Assigned(Table)) then
    for I := 0 to Table.Keys.Count - 1 do
    begin
      OldKey := Table.Keys[I];
      Modified := False;
      for J := 0 to NewTable.Keys.Count - 1 do
        if (not NewTable.Keys[J].Created and (lstrcmpi(PChar(NewTable.Keys[J].OriginalName), PChar(OldKey.Name)) = 0)) then
          Modified := NewTable.Keys[J].Equal(OldKey);
      if (not Modified) then
      begin
        if (SQL <> '') then SQL := SQL + ',' + #13#10;
        if (OldKey.Name = '') then
          SQL := SQL + '  DROP PRIMARY KEY'
        else
          SQL := SQL + '  DROP INDEX ' + Session.EscapeIdentifier(OldKey.Name);
      end;
    end;

  ForeignKeyDropClausel := '';
  if (Assigned(Table)) then
  begin
    for I := 0 to Table.ForeignKeys.Count - 1 do
    begin
      OldForeignKey := Table.ForeignKeys[I];
      Found := False;
      for J := 0 to NewTable.ForeignKeys.Count - 1 do
        if (lstrcmpi(PChar(NewTable.ForeignKeys[J].OriginalName), PChar(OldForeignKey.Name)) = 0) then
          Found := NewTable.ForeignKeys[J].Equal(OldForeignKey);
      if (not Found) then
      begin
        if (ForeignKeyDropClausel <> '') then ForeignKeyDropClausel := ForeignKeyDropClausel + ',' + #13#10;
        ForeignKeyDropClausel := ForeignKeyDropClausel + '  DROP FOREIGN KEY ' + Session.EscapeIdentifier(OldForeignKey.Name);
      end;
    end;
    if (not ForeignKeyAdded and (ForeignKeyDropClausel <> '')) then
    begin
      if (SQL <> '') then SQL := SQL + ',' + #13#10;
      SQL := SQL + ForeignKeyDropClausel;
    end;
  end;

  if (not Assigned(Table)) then
    SQL := SQL + #13#10 + ')'
  else if (NewTable.FName <> Table.Name) then
    begin
      if (Assigned(Table)) and (SQL <> '') then SQL := SQL + ',' + #13#10;
      SQL := SQL + '  RENAME TO ' + Session.EscapeIdentifier(NewTable.Database.Name) + '.' + Session.EscapeIdentifier(NewTable.FName);
    end;

  if (Assigned(NewTable.FEngine) and (not Assigned(Table) and (NewTable.FEngine <> Session.Engines.DefaultEngine) or Assigned(Table) and (NewTable.FEngine <> Table.Engine))) then
  begin
    if (Assigned(Table) and (SQL <> '')) then SQL := SQL + ',' + #13#10;
    if ((Session.ServerVersion < 40102) and Assigned(NewTable.FEngine)) then
      SQL := SQL + ' TYPE=' + NewTable.FEngine.Name
    else
      SQL := SQL + ' ENGINE=' + NewTable.FEngine.Name;
  end;
  if ((not Assigned(Table) or Assigned(Table) and (NewTable.FAutoIncrement <> Table.AutoIncrement)) and (NewTable.FAutoIncrement > 0)) then
  begin
    if (Assigned(Table) and (SQL <> '')) then SQL := SQL + ',' + #13#10;
    SQL := SQL + ' AUTO_INCREMENT=' + IntToStr(NewTable.FAutoIncrement);
  end;
  if ((not Assigned(Table) and NewTable.Checksum or Assigned(Table) and (NewTable.Checksum <> Table.Checksum))) then
  begin
    if (Assigned(Table) and (SQL <> '')) then SQL := SQL + ',' + #13#10;
    if (not NewTable.Checksum) then
      SQL := SQL + ' CHECKSUM=0'
    else
      SQL := SQL + ' CHECKSUM=1';
  end;
  if (not Assigned(Table) and (NewTable.FComment <> '') or Assigned(Table) and (NewTable.FComment <> Table.Comment) and (Session.ServerVersion >= 40100)) then
  begin
    if (Assigned(Table) and (SQL <> '')) then SQL := SQL + ',' + #13#10;
    SQL := SQL + ' COMMENT=' + SQLEscape(NewTable.FComment);
  end;
  if (Session.ServerVersion >= 40100) then
  begin
    if ((NewTable.FDefaultCharset <> '') and (not Assigned(Table) or (NewTable.FDefaultCharset <> Table.DefaultCharset))) then
    begin
      if (Assigned(Table) and (SQL <> '')) then SQL := SQL + ',' + #13#10;
      SQL := SQL + ' DEFAULT CHARSET=' + NewTable.FDefaultCharset;
    end;
    if ((NewTable.FCollation <> '') and (not Assigned(Table) or (NewTable.FCollation <> Table.Collation))) then
    begin
      if (Assigned(Table) and (SQL <> '')) then SQL := SQL + ',' + #13#10;
      SQL := SQL + ' COLLATE=' + NewTable.FCollation;
    end;
  end;
  if ((not Assigned(Table) and NewTable.DelayKeyWrite or Assigned(Table) and (NewTable.DelayKeyWrite <> Table.DelayKeyWrite))) then
  begin
    if (Assigned(Table) and (SQL <> '')) then SQL := SQL + ',' + #13#10;
    if (not NewTable.DelayKeyWrite) then
      SQL := SQL + ' DELAY_KEY_WRITE=0'
    else
      SQL := SQL + ' DELAY_KEY_WRITE=1';
  end;
  if (((not Assigned(Table) and (NewTable.InsertMethod <> imNo) or Assigned(Table) and (NewTable.Checksum <> Table.Checksum))) and (Session.ServerVersion >= 40000)) then
  begin
    if (Assigned(Table) and (SQL <> '')) then SQL := SQL + ',' + #13#10;
    case (NewTable.InsertMethod) of
      imFirst: SQL := SQL + ' INSERT_METHOD=FIRST';
      imLast: SQL := SQL + ' INSERT_METHOD=LAST';
      else {imNo} SQL := SQL + ' INSERT_METHOD=NO';
    end;
  end;
  if (not Assigned(Table) and (NewTable.BlockSize > 0) or Assigned(Table) and (NewTable.BlockSize <> Table.BlockSize)) then
  begin
    if (Assigned(Table) and (SQL <> '')) then SQL := SQL + ',' + #13#10;
    SQL := SQL + ' KEY_BLOCK_SIZE=' + IntToStr(NewTable.BlockSize);
  end;
  if (not Assigned(Table) and (NewTable.MaxRows > 0) or Assigned(Table) and (NewTable.MaxRows <> Table.MaxRows)) then
  begin
    if (Assigned(Table) and (SQL <> '')) then SQL := SQL + ',' + #13#10;
    SQL := SQL + ' MAX_ROWS=' + SQLEscape(IntToStr(NewTable.MaxRows));
  end;
  if (not Assigned(Table) and (NewTable.MinRows > 0) or Assigned(Table) and (NewTable.MinRows <> Table.MinRows)) then
  begin
    if (Assigned(Table) and (SQL <> '')) then SQL := SQL + ',' + #13#10;
    SQL := SQL + ' MIN_ROWS=' + SQLEscape(IntToStr(NewTable.MinRows));
  end;
  if (not Assigned(Table) and (NewTable.FPackKeys <> piDefault) or Assigned(Table) and (NewTable.FPackKeys <> Table.PackKeys)) then
  begin
    if (Assigned(Table) and (SQL <> '')) then SQL := SQL + ',' + #13#10;
    case (NewTable.FPackKeys) of
      piUnpacked: SQL := SQL + ' PACK_KEYS=0';
      piPacked: SQL := SQL + ' PACK_KEYS=1';
      else {piDefault} SQL := SQL + ' PACK_KEYS=DEFAULT';
    end;
  end;
  if ((not Assigned(Table) and (NewTable.FRowType <> mrUnknown) or Assigned(Table) and (NewTable.FRowType <> Table.RowType)) and (NewTable.DBRowTypeStr() <> '')) then
  begin
    if (Assigned(Table) and (SQL <> '')) then SQL := SQL + ',' + #13#10;
    SQL := SQL + ' ROW_FORMAT=' + NewTable.DBRowTypeStr();
  end;

  if (Assigned(Table) and Assigned(Table.Partitions) and (Table.Partitions.PartitionType <> ptNone) and (NewTable.Partitions.PartitionType = ptNone)) then
    SQL := SQL + ' REMOVE PARTITIONING'
  else if (Assigned(NewTable.Partitions) and (NewTable.Partitions.PartitionType <> ptNone)) then
  begin
    if (not Assigned(Table) or (NewTable.Partitions.PartitionType <> Table.Partitions.PartitionType) or (NewTable.Partitions.Expression <> Table.Partitions.Expression) or (NewTable.Partitions.PartitionsNumber <> Table.Partitions.PartitionsNumber)) then
    begin
      if (Assigned(Table) and (SQL <> '')) then SQL := SQL + #13#10;
      SQL := SQL + ' PARTITION BY ';
      if (NewTable.Partitions.Linear) then
        SQL := SQL + 'LINEAR ';
      case (NewTable.Partitions.PartitionType) of
        ptHash: SQL := SQL + 'HASH(' + NewTable.Partitions.Expression + ')';
        ptKey: SQL := SQL + 'KEY';
        ptRange: SQL := SQL + 'RANGE(' + NewTable.Partitions.Expression + ')';
        ptList: SQL := SQL + 'LIST(' + NewTable.Partitions.Expression + ')';
      end;
      if (NewTable.Partitions.PartitionsNumber > 0) then
        SQL := SQL + ' PARTITIONS ' + IntToStr(NewTable.Partitions.PartitionsNumber);
    end;

    if (NewTable.Partitions.PartitionType in [ptHash, ptKey]) then
    begin
      if (not Assigned(Table) or (NewTable.Partitions.Count <> Table.Partitions.Count) and (NewTable.Partitions.Count > 0)) then
      begin
//        if (Assigned(Table) and (SQL <> '')) then SQL := SQL + #13#10;
//        if (Assigned(Table)) then
//          SQL := SQL + ' PARTITIONS ' + IntToStr(NewTable.Partitions.Count)
//        else
//          SQL := SQL + ' COALESCE PARTITION ' + IntToStr(NewTable.Partitions.Count);
      end;
    end
    else if (NewTable.Partitions.PartitionType in [ptRange, ptList]) then
    begin
      if (not Assigned(Table) or (Table.Partitions.Count = 0)) then
      begin
        SQL := SQL + ' (' + #13#10;
        for I := 0 to NewTable.Partitions.Count - 1 do
        begin
          if (I > 0) then SQL := SQL + ',' + #13#10;
          SQL := SQL + '  ' + NewTable.Partitions[I].DBTypeStr();
        end;
        SQL := SQL + #13#10 + ') ' + #13#10;
      end
      else
      begin
        Found := False;
        for I := 0 to NewTable.Partitions.Count - 1 do
        begin
          NewPartition := NewTable.Partitions.Partition[I];
          if (not Assigned(Table) or (NewPartition.OriginalName = '')) then
            OldPartition := nil
          else
            OldPartition := Table.PartitionByName(NewPartition.OriginalName);
          Found := Found or not Assigned(OldPartition);
          if (Assigned(OldPartition) and not NewPartition.Equal(OldPartition)) then
            SQL := SQL + ' REORGANIZE PARTITION ' + Session.EscapeIdentifier(OldPartition.Name) + ' INTO ' + NewPartition.DBTypeStr() + #13#10;
        end;

        if (Found) then
        begin
          Found := False;
          SQL := SQL + ' ADD PARTITION (';
          for I := 0 to NewTable.Partitions.Count - 1 do
          begin
            NewPartition := NewTable.Partitions.Partition[I];
            if (not Assigned(Table) or (NewPartition.OriginalName = '')) then
              OldPartition := nil
            else
              OldPartition := Table.PartitionByName(NewPartition.OriginalName);
            if (not Assigned(OldPartition)) then
            begin
              if (Found) then SQL := SQL + ', ';
              SQL := SQL + NewPartition.DBTypeStr();
              Found := True;
            end;
          end;
          SQL := SQL + ')' + #13#10;
        end;
      end;

      if (Assigned(Table)) then
      begin
        SQLPart := '';
        for I := 0 to Table.Partitions.Count - 1 do
        begin
          OldPartition := Table.Partitions.Partition[I];
          Found := False;
          for J := 0 to NewTable.Partitions.Count - 1 do
          begin
            NewPartition := NewTable.Partitions.Partition[J];
            if (lstrcmpi(PChar(NewPartition.OriginalName), PChar(OldPartition.Name)) = 0) then
              Found := True;
          end;
          if (not Found) then
          begin
            if (SQLPart <> '') then SQLPart := SQLPart + ', ';
            SQLPart := SQLPart + Session.EscapeIdentifier(OldPartition.Name);
          end;
        end;
        if (SQLPart <> '') then
          SQL := ' DROP PARTITION ' + SQLPart;
      end;
    end;
  end;

  if (Trim(SQL) = '') then
    Result := ''
  else if (not Assigned(Table)) then
    Result := 'CREATE TABLE ' + Session.EscapeIdentifier(Name) + '.' + Session.EscapeIdentifier(NewTable.Name) + ' (' + #13#10 + TrimRight(SQL) + ';' + #13#10
  else
  begin
    Result := '';

    if (ForeignKeyAdded and (ForeignKeyDropClausel <> '')) then
      Result := Result + 'ALTER TABLE ' + Session.EscapeIdentifier(Name) + '.' + Session.EscapeIdentifier(Table.Name) + #13#10 + TrimRight(ForeignKeyDropClausel) + ';' + #13#10;

    Result := Result + 'ALTER TABLE ' + Session.EscapeIdentifier(Name) + '.' + Session.EscapeIdentifier(Table.Name) + #13#10 + TrimRight(SQL) + ';' + #13#10;
  end;

  if ((Result <> '') and (Session.DatabaseName <> Name)) then
    Result := SQLUse() + Result;
end;

function TSDatabase.SQLGetSource(): string;
begin
  if ((Session.ServerVersion < 40101) or (Self is TSSystemDatabase)) then
    Result := ''
  else
    Result := 'SHOW CREATE DATABASE ' + Session.EscapeIdentifier(Name) + ';' + #13#10;
end;

function TSDatabase.SQLTruncateTable(const Table: TSBaseTable): string;
begin
  if ((Session.ServerVersion < 32328) or Assigned(Table.Engine) and Table.Engine.IsInnoDB) then
  begin
    Result := 'DELETE FROM ' + Session.EscapeIdentifier(Name) + '.' + Session.EscapeIdentifier(Table.Name) + ';' + #13#10;
    if (Assigned(Table.Engine) and Table.Engine.IsInnoDB) then
      Result := Result + 'ALTER TABLE ' + Session.EscapeIdentifier(Name) + '.' + Session.EscapeIdentifier(Table.Name) + ' AUTO_INCREMENT=1;' + #13#10;
  end
  else
    Result := 'TRUNCATE TABLE ' + Session.EscapeIdentifier(Name) + '.' + Session.EscapeIdentifier(Table.Name) + ';' + #13#10;

  if (Session.DatabaseName <> Name) then
    Result := SQLUse() + Result;
end;

function TSDatabase.SQLUse(): string;
begin
  Result := Session.SQLUse(Name);
end;

function TSDatabase.TableByName(const TableName: string): TSTable;
var
  Index: Integer;
begin
  Index := Tables.IndexByName(TableName);
  if (Index < 0) then
    Result := nil
  else
    Result := Tables[Index];
end;

function TSDatabase.TriggerByName(const TriggerName: string): TSTrigger;
var
  Index: Integer;
begin
  Index := Triggers.IndexByName(TriggerName);
  if (Index < 0) then
    Result := nil
  else
    Result := Triggers[Index];
end;

function TSDatabase.Unlock(): Boolean;
var
  SQL: string;
begin
  SQL := 'UNLOCK TABLES;' + #13#10;

  if (Session.DatabaseName <> Name) then
    SQL := SQLUse() + SQL;

  Result := Session.ExecuteSQL(SQL);
end;

function TSDatabase.Update(const Status: Boolean = False): Boolean;
var
  List: TList;
begin
  List := TList.Create();
  List.Add(Self);
  Result := Session.Update(List, Status);
  List.Free();
end;

function TSDatabase.UpdateEvent(const Event, NewEvent: TSEvent): Boolean;
var
  SQL: string;
begin
  SQL := '';

  if (not Assigned(Event) or (NewEvent.EventType <> Event.EventType) or (NewEvent.Execute <> Event.Execute) or (NewEvent.IntervalValue <> Event.IntervalValue) or (NewEvent.IntervalType <> Event.IntervalType)) then
  begin
    SQL := SQL + '  ON SCHEDULE ';
    case (NewEvent.EventType) of
      etSingle:
        SQL := SQL + 'AT ' + SQLEscape(MySQLDB.DateTimeToStr(NewEvent.Execute, Session.FormatSettings));
      etMultiple:
        begin
          SQL := SQL + 'EVERY ' + IntervalToStr(NewEvent.IntervalValue, NewEvent.IntervalType);
          if (NewEvent.StartDateTime > 0) then
            SQL := SQL + ' STARTS ' + SQLEscape(MySQLDB.DateTimeToStr(NewEvent.StartDateTime, Session.FormatSettings));
          if (NewEvent.EndDateTime > 0) then
            SQL := SQL + ' ENDS ' + SQLEscape(MySQLDB.DateTimeToStr(NewEvent.EndDateTime, Session.FormatSettings));
        end;
    end;
    SQL := SQL + #13#10;
  end;

  if (Assigned(Event) and (NewEvent.Name <> Event.Name))then
    SQL := SQL + '  RENAME TO ' + Session.EscapeIdentifier(Name) + '.' + Session.EscapeIdentifier(NewEvent.Name);

  if (not Assigned(Event) or (NewEvent.Preserve <> Event.Preserve)) then
    if (not NewEvent.Preserve) then
      SQL := SQL + '  ON COMPLETION NOT PRESERVE' + #13#10
    else
      SQL := SQL + '  ON COMPLETION PRESERVE' + #13#10;

  if ((not Assigned(Event) or (NewEvent.Enabled <> Event.Enabled)) and not NewEvent.Enabled) then
    SQL := SQL + '  DISABLE' + #13#10
  else if (Assigned(Event) and (NewEvent.Enabled <> Event.Enabled)) then
    SQL := SQL + '  ENABLE' + #13#10;

  if (not Assigned(Event) and (NewEvent.Comment <> '') or Assigned(Event) and (NewEvent.Comment <> Event.Comment))then
    SQL := SQL + '  COMMENT ' + SQLEscape(NewEvent.Comment) + #13#10;

  if (not Assigned(Event) and (SQLTrimStmt(NewEvent.Stmt) <> '') or Assigned(Event) and (SQLTrimStmt(NewEvent.Stmt) <> SQLTrimStmt(Event.Stmt)))then
  begin
    SQL := SQL + '  DO ' + SQLTrimStmt(NewEvent.Stmt);
    if (SQL[Length(SQL)] = ';') then Delete(SQL, Length(SQL), 1);
  end;

  if (SQL = '') then
    Result := True
  else
  begin
    if (not Assigned(Event)) then
      SQL := 'CREATE EVENT ' + Session.EscapeIdentifier(Name) + '.' + Session.EscapeIdentifier(NewEvent.Name) + #13#10 + TrimRight(SQL) + ';' + #13#10
    else
      SQL := 'ALTER EVENT ' + Session.EscapeIdentifier(Name) + '.' + Session.EscapeIdentifier(Event.Name) + #13#10 + TrimRight(SQL) + ';' + #13#10;

    Result := Session.SendSQL(SQL);
  end;
end;

function TSDatabase.UpdateRoutine(const Routine: TSRoutine; const NewRoutine: TSRoutine): Boolean;
var
  SQL: string;
begin
  SQL := '';
  if (NewRoutine.Security <> Routine.Security) then
    case (NewRoutine.Security) of
      seDefiner: SQL := SQL + ' SQL SECURITY DEFINER';
      seInvoker: SQL := SQL + ' SQL SECURITY INVOKER';
    end;
  if (Routine.Comment <> NewRoutine.Comment) then
    SQL := SQL + ' COMMENT ' + SQLEscape(NewRoutine.Comment);

  if (SQL <> '') then
    if (Routine.RoutineType = rtProcedure) then
      SQL := 'ALTER PROCEDURE ' + Session.EscapeIdentifier(Name) + '.' + Session.EscapeIdentifier(NewRoutine.Name) + SQL +  ';' + #13#10
    else
      SQL := 'ALTER FUNCTION ' + Session.EscapeIdentifier(Name) + '.' + Session.EscapeIdentifier(NewRoutine.Name) + SQL +  ';' + #13#10;

  if (SQL = '') then
    Result := True
  else
  begin
    if (Session.DatabaseName <> Name) then
      SQL := SQLUse() + SQL;

    Result := Session.ExecuteSQL(SQL);

    // Warum verliert die MySQL Datenbank den Multi Stmt Status ??? (Fixed in 5.0.67 - auch schon vorher?)
    if (not Result and Session.MultiStatements and Assigned(Session.Lib.mysql_set_server_option)) then
      Session.Lib.mysql_set_server_option(Session.Handle, MYSQL_OPTION_MULTI_STATEMENTS_ON);

    if (not Result and Assigned(Routine)) then
      Session.ExecuteSQL(Routine.Source);
    if (Result) then
    begin
      SQL := Routines.SQLGetItems();
      Session.SendSQL(SQL, Session.SessionResult);
    end;
  end;
end;

function TSDatabase.UpdateRoutine(const Routine: TSRoutine; const SQLCreateRoutine: string): Boolean;
var
  SQL: string;
begin
  SQL := '';

  if (Assigned(Routine)) then
    if (Routine.RoutineType = rtProcedure) then
      SQL := 'DROP PROCEDURE IF EXISTS ' + Session.EscapeIdentifier(Name) + '.' + Session.EscapeIdentifier(Routine.Name) + ';' + #13#10
    else
      SQL := 'DROP FUNCTION IF EXISTS ' + Session.EscapeIdentifier(Name) + '.' + Session.EscapeIdentifier(Routine.Name) + ';' + #13#10;

  SQL := SQL + SQLCreateRoutine + #13#10;

  if (Session.DatabaseName <> Name) then
    SQL := SQLUse() + SQL;

  Result := Session.ExecuteSQL(SQL);

  // Warum verliert die MySQL Datenbank den Multi Stmt Status ??? (Fixed in 5.0.67 - auch schon vorher?)
  if (not Result and Session.MultiStatements and Assigned(Session.Lib.mysql_set_server_option)) then
    Session.Lib.mysql_set_server_option(Session.Handle, MYSQL_OPTION_MULTI_STATEMENTS_ON);

  if (not Result and Assigned(Routine)) then
    Session.ExecuteSQL(Routine.Source);
end;

function TSDatabase.UpdateTable(const Table, NewTable: TSBaseTable): Boolean;
var
  I: Integer;
  List: TList;
  SQL: string;
begin
  if (Assigned(Table)) then
    for I := 0 to NewTable.Fields.Count - 1 do
      if (NewTable.Fields[I].FieldType in TextFieldTypes) then
      begin
        if ((NewTable.Fields[I].Charset = Table.DefaultCharset) or (NewTable.Fields[I].Charset = '') and (NewTable.DefaultCharset <> Table.DefaultCharset)) then
          NewTable.Fields[I].Charset := NewTable.DefaultCharset;
        if ((NewTable.Fields[I].Collation = Table.Collation) or (NewTable.Fields[I].Collation = '') and (NewTable.Collation <> Table.Collation)) then
          NewTable.Fields[I].Collation := NewTable.Collation;
      end;

  List := TList.Create();
  List.Add(Table);

  SQL := SQLAlterTable(Table, NewTable)
    + NewTable.SQLGetSource()
    + Tables.SQLGetStatus(List);

  List.Free();

  Result := (SQL = '') or Session.SendSQL(SQL, Session.SessionResult);
end;

function TSDatabase.UpdateTables(const TableNames: TStringList; const ACharset, ACollation, AEngine: string; const ARowType: TMySQLRowType): Boolean;
var
  I: Integer;
  J: Integer;
  NewTable: TSBaseTable;
  SQL: string;
  Table: TSBaseTable;
begin
  Result := True; SQL := '';

  NewTable := TSBaseTable.Create(Tables);

  for I := 0 to TableNames.Count - 1 do
  begin
    Result := Result and (TableByName(TableNames.Strings[I]) is TSBaseTable);
    if (Result) then
    begin
      Table := BaseTableByName(TableNames.Strings[I]);

      if (Assigned(Table) and ((lstrcmpi(PChar(ACharset), PChar(Table.DefaultCharset)) = 0) or (lstrcmpi(PChar(ACollation), PChar(Table.Collation)) <> 0) or (Session.EngineByName(AEngine) <> Table.Engine) or (ARowType <> Table.RowType))) then
      begin
        NewTable.Assign(Table);
        if (ACharset <> '') then
        begin
          NewTable.DefaultCharset := ACharset;

          for J := 0 to NewTable.Fields.Count - 1 do
            if (NewTable.Fields[J].FieldType in TextFieldTypes) then
            begin
              if ((NewTable.DefaultCharset <> Table.DefaultCharset) and ((NewTable.Fields[J].Charset = Table.DefaultCharset) or (NewTable.Fields[J].Charset = ''))) then
                NewTable.Fields[J].Charset := NewTable.DefaultCharset;
              if ((NewTable.Collation <> Table.Collation) and ((NewTable.Fields[J].Collation = Table.Collation) or (NewTable.Fields[J].Collation = ''))) then
                NewTable.Fields[J].Collation := NewTable.Collation;
            end;
        end;
        if (ACollation <> '') then NewTable.Collation := LowerCase(ACollation);
        if (AEngine <> '') then NewTable.Engine := Session.EngineByName(AEngine);
        if (ARowType <> mrUnknown) then NewTable.RowType := ARowType;

        SQL := SQL + SQLAlterTable(Table, NewTable);
      end;
    end;
  end;

  NewTable.Free();

  Result := Result and ((SQL = '') or Session.ExecuteSQL(SQL));
end;

function TSDatabase.UpdateTrigger(const Trigger, NewTrigger: TSTrigger): Boolean;
var
  SQL: string;
begin
  SQL := '';

  if (Session.DatabaseName <> Name) then
    SQL := SQL + SQLUse();

  if (Assigned(Trigger)) then
    SQL := SQL + 'DROP TRIGGER IF EXISTS ' + Session.EscapeIdentifier(Name) + '.' + Session.EscapeIdentifier(Trigger.Name) + ';' + #13#10;

  SQL := SQL + NewTrigger.GetSourceEx();

  Result := Session.ExecuteSQL(SQL);

  // Warum verliert die MySQL Datenbank den Multi Stmt Status ??? (Fixed in 5.0.67 - auch schon vorher?)
  if (Session.Connected and Session.MultiStatements and Assigned(Session.Lib.mysql_set_server_option)) then
    Session.Lib.mysql_set_server_option(Session.Handle, MYSQL_OPTION_MULTI_STATEMENTS_ON);

  if (not Result and Assigned(Trigger)) then
    Session.ExecuteSQL(Trigger.Source);
end;

function TSDatabase.UpdateView(const View, NewView: TSView): Boolean;
var
  SQL: string;
begin
  SQL := '';

  case (NewView.Algorithm) of
    vaUndefined: SQL := SQL + 'ALGORITHM=UNDEFINED ';
    vaMerge: SQL := SQL + 'ALGORITHM=MERGE ';
    vaTemptable: SQL := SQL + 'ALGORITHM=TEMPTABLE ';
  end;
  if (Session.ServerVersion >= 50016) then
  begin
    if (not Assigned(View) and (NewView.Definer <> '') or Assigned(View) and (View.Definer <> NewView.Definer)) then
      SQL := SQL + 'DEFINER=' + Session.EscapeUser(NewView.Definer, True) + ' ';
    case (NewView.Security) of
      seDefiner: SQL := SQL + 'SQL SECURITY DEFINER ';
      seInvoker: SQL := SQL + 'SQL SECURITY INVOKER ';
    end;
  end;
  SQL := SQL + 'VIEW ' + Session.EscapeIdentifier(NewView.Database.Name) + '.' + Session.EscapeIdentifier(NewView.Name);
  SQL := SQL + ' AS ' + SQLTrimStmt(NewView.Stmt);
  if (SQL[Length(SQL)] = ';') then
    Delete(SQL, Length(SQL), 1);
  case (NewView.CheckOption) of
    voDefault: SQL := SQL + ' WITH CHECK OPTION';
    voCascaded: SQL := SQL + ' WITH CASCADED CHECK OPTION';
    voLocal: SQL := SQL + ' WITH LOCAL CHECK OPTION';
  end;

  if (not Assigned(View)) then
    SQL := 'CREATE ' + SQL
  else
    SQL := 'ALTER ' + SQL;
  SQL := Trim(SQL);
  if (SQL[Length(SQL)] <> ';') then
    SQL := SQL + ';';
  SQL := SQL + #13#10;

  SQL := SQL + NewView.SQLGetSource();

  if (Assigned(View) and (View.Name <> NewView.Name)) then
    SQL := 'RENAME TABLE '
      + Session.EscapeIdentifier(View.Database.Name) + '.' + Session.EscapeIdentifier(View.Name) + ' TO '
      + Session.EscapeIdentifier(NewView.Database.Name) + '.' + Session.EscapeIdentifier(NewView.Name) + ';' + #13#10
      + SQL;

  if (Session.DatabaseName <> Name) then
    SQL := SQLUse() + SQL;

  Result := (SQL = '') or Session.SendSQL(SQL, Session.SessionResult);
end;

function TSDatabase.ViewByName(const TableName: string): TSView;
var
  Index: Integer;
begin
  Index := Tables.IndexByName(TableName);
  if ((Index < 0) or not (Tables[Index] is TSView)) then
    Result := nil
  else
    Result := TSView(Tables[Index]);
end;

function TSSystemDatabase.GetCreated(): TDateTime;
begin
  if (Session.StartTime = 0) then
    Result := inherited
  else
    Result := Session.StartTime;
end;

{ TSDatabases *****************************************************************}

function TSDatabases.Build(const DataSet: TMySQLQuery; const UseInformationSchema: Boolean; Filtered: Boolean = False; const SessionEvents: Boolean = True): Boolean;
var
  DatabaseNames: TCSVStrings;
  DeleteList: TList;
  Found: Boolean;
  I: Integer;
  Index: Integer;
  Name: string;
  NewDatabase: TSDatabase;
begin
  DeleteList := TList.Create();
  DeleteList.Assign(Self);

  if (not DataSet.IsEmpty()) then
  begin
    SetLength(DatabaseNames, 0);
    if (Assigned(Session.Account)) then
      CSVSplitValues(Session.Account.Connection.Database, ',', '"', DatabaseNames);

    repeat
      if (not UseInformationSchema) then
        Name := DataSet.Fields[0].AsString
      else
        Name := DataSet.FieldByName('SCHEMA_NAME').AsString;

      Found := ((Session.TableNameCmp(Name, INFORMATION_SCHEMA) = 0)
        or (NameCmp(Name, PERFORMANCE_SCHEMA) = 0));
      for I := 0 to Length(DatabaseNames) - 1 do
        if (NameCmp(Name, DatabaseNames[I]) = 0) then
          Found := True;

      if (Found or (Length(DatabaseNames) = 0)) then
      begin
        if (InsertIndex(Name, Index)) then
        begin
          if (Session.TableNameCmp(Name, INFORMATION_SCHEMA) = 0) then
          begin
            NewDatabase := TSSystemDatabase.Create(Session, Name);
            Session.FInformationSchema := NewDatabase;
          end
          else if (NameCmp(Name, PERFORMANCE_SCHEMA) = 0) then
          begin
            NewDatabase := TSSystemDatabase.Create(Session, Name);
            Session.FPerformanceSchema := NewDatabase;
          end
          else
            NewDatabase := TSDatabase.Create(Session, Name);

          if (Index < Count) then
            Insert(Index, NewDatabase)
          else
            Index := Add(NewDatabase);
        end
        else if (DeleteList.IndexOf(Items[Index]) >= 0) then
          DeleteList.Delete(DeleteList.IndexOf(Items[Index]));

        if (UseInformationSchema) then
        begin
          Database[Index].DefaultCharset := DataSet.FieldByName('DEFAULT_CHARACTER_SET_NAME').AsString;
          Database[Index].Collation := LowerCase(DataSet.FieldByName('DEFAULT_COLLATION_NAME').AsString);
        end;

        if (Filtered and SessionEvents) then
          Session.ExecuteEvent(etItemValid, Session, Self, Database[Index]);
      end;
    until (not DataSet.FindNext());
  end
  else if (Assigned(Session.Account) and (Session.Account.Connection.Database <> '')) then
  begin
    CSVSplitValues(Session.Account.Connection.Database, ',', '"', DatabaseNames);
    for I := 0 to Length(DatabaseNames) - 1 do
    begin
      Name := DatabaseNames[I];

      if (Session.TableNameCmp(Name, INFORMATION_SCHEMA) = 0) then
      begin
        Index := Add(TSSystemDatabase.Create(Session, Name));
        Session.FInformationSchema := Database[Index];
      end
      else if (Session.TableNameCmp(Name, PERFORMANCE_SCHEMA) = 0) then
      begin
        Index := Add(TSSystemDatabase.Create(Session, Name));
        Session.FInformationSchema := Database[Index];
      end
      else
        Add(TSDatabase.Create(Session, Name));

      if (Filtered and SessionEvents) then
        Session.ExecuteEvent(etItemValid, Session, Self, Database[Index]);
    end;
  end;

  Result := inherited;

  if (not Filtered) then
    while (DeleteList.Count > 0) do
    begin
      Index := IndexOf(DeleteList.Items[0]);
      Item[Index].Free();
      Delete(Index);
      DeleteList.Delete(0);
    end;
  DeleteList.Free();

  if (not Filtered and SessionEvents) then
    Session.ExecuteEvent(etItemsValid, Session, Self);
end;

procedure TSDatabases.Delete(const AEntity: TSEntity);
var
  I: Integer;
  Index: Integer;
  J: Integer;
  Names: TCSVStrings;
begin
  Assert(AEntity is TSDatabase);


  if (Session.Account.Connection.Database <> '') then
  begin
    SetLength(Names, 0);
    CSVSplitValues(Session.Account.Connection.Database, ',', '"', Names);
    for I := Length(Names) - 1 downto 0 do
      if (Names[I] = AEntity.Name) then
      begin
        for J := I to Length(Names) - 2 do
          Names[I] := Names[I + 1];
        SetLength(Names, Length(Names) - 1);
      end;
    Session.Account.Connection.Database := '';
    for I := 0 to Length(Names) - 1 do
    begin
      if (I > 0) then Session.Account.Connection.Database := Session.Account.Connection.Database + ',';
      Session.Account.Connection.Database := Session.Account.Connection.Database + CSVEscape(Names[I]);
    end;
  end;

  Index := IndexOf(AEntity);
  if (Index >= 0) then
    Delete(Index);

  Session.ExecuteEvent(etItemDropped, Session, Self, AEntity);

  AEntity.Free();
end;

function TSDatabases.GetDatabase(Index: Integer): TSDatabase;
begin
  Result := TSDatabase(Items[Index]);
end;

function TSDatabases.NameCmp(const Name1, Name2: string): Integer;
begin
  if (Session.LowerCaseTableNames = 0) then
    Result := lstrcmp(PChar(Name1), PChar(Name2))
  else
    Result := lstrcmpi(PChar(Name1), PChar(Name2));
end;

function TSDatabases.SQLGetItems(const Name: string = ''): string;
var
  DatabaseNames: TCSVStrings;
  I: Integer;
begin
  if (Session.ServerVersion < 50006) then
    Result := 'SHOW DATABASES;' + #13#10
  else
  begin
    Result := 'SELECT * FROM ' + Session.EscapeIdentifier(INFORMATION_SCHEMA) + '.' + Session.EscapeIdentifier('SCHEMATA');
    if (Assigned(Session.Account) and (Session.Account.Connection.Database <> '')) then
    begin
      Result := Result + ' WHERE ' + Session.EscapeIdentifier('SCHEMA_NAME') + ' IN (';

      SetLength(DatabaseNames, 0);
      CSVSplitValues(Session.Account.Connection.Database, ',', '"', DatabaseNames);
      for I := 0 to Length(DatabaseNames) - 1 do
      begin
        if (I > 0) then Result := Result + ',';
        Result := Result + SQLEscape(DatabaseNames[I]);
      end;
      Result := Result + ',' + SQLEscape(INFORMATION_SCHEMA);
      if (Session.ServerVersion >= 50503) then
        Result := Result + ',' + SQLEscape(PERFORMANCE_SCHEMA);
      Result := Result + ')';

      SetLength(DatabaseNames, 0);
    end;
    Result := Result + ';' + #13#10;
  end;
end;

{ TSVariable ******************************************************************}

procedure TSVariable.Assign(const Source: TSVariable);
begin
  inherited Assign(Source);

  Value := Source.Value;
end;

function TSVariable.GetAsBoolean(): Boolean;
begin
  Result := (Value = '1') or (UpperCase(Value) = 'TRUE') or (UpperCase(Value) = 'YES') or (UpperCase(Value) = 'ON');
end;

function TSVariable.GetAsFloat(): Double;
begin
  if (not Assigned(Variables)) then
    Result := StrToFloat(ReplaceStr(Value, ',', ''))
  else
    Result := StrToFloat(Value, Variables.Session.FormatSettings);
end;

function TSVariable.GetAsInteger(): Integer;
begin
  if (not TryStrToInt(ReplaceStr(Value, '.', ''), Result)) then
    if (UpperCase(Value) = 'OFF') then
      Result := 0
    else if (UpperCase(Value) = 'ON') then
      Result := 1
    else
      EConvertError.CreateFmt(SConvStrParseError + '(' + Value + ')', ['"' + Name + '"']);
end;

function TSVariable.GetAsString(): string;
begin
  Result := Value;
end;

function TSVariable.GetVariables(): TSVariables;
begin
  Assert(CItems is TSVariables);

  Result := TSVariables(CItems);
end;

procedure TSVariable.SetAsBoolean(const AAsBoolean: Boolean);
begin
  if (AAsBoolean <> AsBoolean) then
    if ((UpperCase(Value) = 'YES') or (UpperCase(Value) = 'NO')) then
      if (AAsBoolean) then Value := 'YES' else Value := 'NO'
    else if ((UpperCase(Value) = '1') or (UpperCase(Value) = '0')) then
      if (AAsBoolean) then Value := '1' else Value := '0'
    else if ((UpperCase(Value) = 'ON') or (UpperCase(Value) = 'OFF')) then
      if (AAsBoolean) then Value := 'ON' else Value := 'OFF'
    else
      if (AAsBoolean) then Value := 'TRUE' else Value := 'FALSE'
end;

procedure TSVariable.SetAsFloat(const AAsFloat: Double);
begin
  if (Assigned(Variables)) then
    Value := FloatToStr(AAsFloat)
  else
    Value := FloatToStr(AAsFloat, Variables.Session.FormatSettings);
end;

procedure TSVariable.SetAsInteger(const AAsInteger: Integer);
begin
  Value := IntToStr(AAsInteger);
end;

procedure TSVariable.SetAsString(const AAsString: string);
begin
  Value := AAsString;
end;

{ TSVariables *****************************************************************}

function TSVariables.Build(const DataSet: TMySQLQuery; const UseInformationSchema: Boolean; Filtered: Boolean = False; const SessionEvents: Boolean = True): Boolean;
var
  DeleteList: TList;
  Index: Integer;
  Name: string;
begin
  DeleteList := TList.Create();
  DeleteList.Assign(Self);

  if (not DataSet.IsEmpty()) then
    repeat
      if (not UseInformationSchema) then
        Name := DataSet.FieldByName('Variable_name').AsString
      else
        Name := DataSet.FieldByName('VARIABLE_NAME').AsString;

      if (InsertIndex(Name, Index)) then
        if (Index < Count) then
          Insert(Index, TSVariable.Create(Self, Name))
        else
          Index := Add(TSVariable.Create(Self, Name))
      else if (DeleteList.IndexOf(Items[Index]) >= 0) then
        DeleteList.Delete(DeleteList.IndexOf(Items[Index]));

      if (not UseInformationSchema) then
        Variable[Index].Value := DataSet.FieldByName('Value').AsString
      else
        Variable[Index].Value := DataSet.FieldByName('VARIABLE_VALUE').AsString;

      if (Filtered and SessionEvents) then
        Session.ExecuteEvent(etItemValid, Session, Self, Variable[Index]);
    until (not DataSet.FindNext());

  Result := inherited;

  if (Count > 0) then
  begin
    if (Assigned(Session.VariableByName('character_set'))) then
    begin
      Session.Charsets.Build(nil, False);
      Session.Charset := Session.VariableByName('character_set').Value;
    end
    else if (Assigned(Session.VariableByName('character_set_client'))) then
    begin
      if (UpperCase(Session.VariableByName('character_set_client').Value) <> UpperCase(Session.VariableByName('character_set_results').Value)) then
        raise ERangeError.CreateFmt(SPropertyOutOfRange + ': character_set_client (%s) <> character_set_results (%s)', ['Charset', Session.VariableByName('character_set_client').Value, Session.VariableByName('character_set_results').Value])
      else
        Session.Charset := Session.VariableByName('character_set_client').Value;
    end;

    if (Session.ServerVersion < 40102) then
      Session.Engines.Build(nil, False);

    if (Assigned(Session.VariableByName('max_allowed_packet'))) then
      Session.FMaxAllowedPacket := Session.VariableByName('max_allowed_packet').AsInteger - 1; // 1 Byte for COM_QUERY

    if (Assigned(Session.VariableByName('lower_case_table_names'))) then
      Session.FLowerCaseTableNames := Session.VariableByName('lower_case_table_names').AsInteger;

    if (Assigned(Session.VariableByName('sql_mode')) and (POS('ANSI_QUOTES', Session.VariableByName('sql_mode').Value) > 0)) then
      Session.AnsiQuotes := True;
    if (Assigned(Session.VariableByName('sql_quote_show_create'))) then
      Session.IdentifierQuoted := Session.VariableByName('sql_quote_show_create').AsBoolean;

    if (Assigned(Session.VariableByName('wait_timeout'))) then
      if (Session.VariableByName('wait_timeout').AsInteger >= 4) then
        Session.ServerTimeout := Session.VariableByName('wait_timeout').AsInteger - 3
      else if (Session.VariableByName('wait_timeout').AsInteger >= 60) then
        Session.ServerTimeout := Session.VariableByName('wait_timeout').AsInteger - 1;
  end;

  if (not Filtered) then
    while (DeleteList.Count > 0) do
    begin
      Index := IndexOf(DeleteList.Items[0]);
      Item[Index].Free();
      Delete(Index);
      DeleteList.Delete(0);
    end;
  DeleteList.Free();

  if (not Filtered and SessionEvents) then
    Session.ExecuteEvent(etItemsValid, Session, Self);
end;

function TSVariables.GetVariable(Index: Integer): TSVariable;
begin
  Result := TSVariable(Items[Index]);
end;

function TSVariables.SQLGetItems(const Name: string = ''): string;
begin
  // In 5.7.8 INFORMATION_SCHEMA.SESSION_VARIABLES is empty. Instead of it,
  // PERFORMANCE_SCHEMA.SESSION_VARIABLES shows the variables. But only,
  // if SHOW_COMPATIBILITY_56 = OFF.

  if (Session.ServerVersion < 40003) then
    Result := 'SHOW VARIABLES;' + #13#10
  else
  begin
    Result := 'SHOW SESSION VARIABLES';
    if (Name <> '') then
      Result := Result + ' LIKE ' + SQLEscape(Name);
    Result := Result + ';' + #13#10;
  end;
end;

{ TSStati *********************************************************************}

function TSStati.Build(const DataSet: TMySQLQuery; const UseInformationSchema: Boolean; Filtered: Boolean = False; const SessionEvents: Boolean = True): Boolean;
var
  DeleteList: TList;
  Index: Integer;
  Name: string;
  Seconds: UInt64;
begin
  DeleteList := TList.Create();
  DeleteList.Assign(Self);

  if (not DataSet.IsEmpty()) then
    repeat
      if (not UseInformationSchema) then
        Name := DataSet.FieldByName('Variable_name').AsString
      else
        Name := DataSet.FieldByName('VARIABLE_NAME').AsString;

      if (InsertIndex(Name, Index)) then
        if (Index < Count) then
          Insert(Index, TSStatus.Create(Self, Name))
        else
          Add(TSStatus.Create(Self, Name))
      else if (DeleteList.IndexOf(Items[Index]) >= 0) then
        DeleteList.Delete(DeleteList.IndexOf(Items[Index]));

      if (not UseInformationSchema) then
        Status[Index].Value := DataSet.FieldByName('Value').AsString
      else
        Status[Index].Value := DataSet.FieldByName('VARIABLE_VALUE').AsString;

      if (Filtered and SessionEvents) then
        Session.ExecuteEvent(etItemValid, Session, Self, Status[Index]);
    until (not DataSet.FindNext());

  if (Assigned(Session.StatusByName('Uptime'))) then
  begin
    Seconds := StrToUInt64(Session.StatusByName('Uptime').Value);

    Session.FStartTime := Now();
    Session.FStartTime := Session.FStartTime - EncodeTime(0, 0, Seconds mod 60, 0); Seconds := Seconds div 60;
    Session.FStartTime := Session.FStartTime - EncodeTime(0, Seconds mod 60, 0, 0); Seconds := Seconds div 60;
    Session.FStartTime := Session.FStartTime - EncodeTime(Seconds mod 24, 0, 0, 0); Seconds := Seconds div 24;
    Session.FStartTime := Session.FStartTime - Seconds;
  end;

  Result := inherited;

  if (not Filtered) then
    while (DeleteList.Count > 0) do
    begin
      Index := IndexOf(DeleteList.Items[0]);
      Item[Index].Free();
      Delete(Index);
      DeleteList.Delete(0);
    end;
  DeleteList.Free();

  if (not Filtered and SessionEvents) then
    Session.ExecuteEvent(etItemsValid, Session, Self);
end;

function TSStati.GetStatus(Index: Integer): TSStatus;
begin
  Result := TSStatus(Items[Index]);
end;

function TSStati.SQLGetItems(const Name: string = ''): string;
begin
  // See comment in TSVariables.SQLGetItems.

  if (Session.ServerVersion < 50002) then
    Result := 'SHOW STATUS;' + #13#10
  else
    Result := 'SHOW SESSION STATUS;' + #13#10
end;

{ TSEngine ********************************************************************}

function TSEngine.FieldAvailable(const MySQLFieldType: TMySQLFieldType): Boolean;
begin
  Result := Engines.Session.FieldTypes.FieldAvailable(Self, MySQLFieldType);
end;

function TSEngine.GetEngines(): TSEngines;
begin
  Assert(CItems is TSEngines);

  Result := TSEngines(CItems);
end;

function TSEngine.GetIsInnoDB(): Boolean;
begin
  Result := Engines.NameCmp(Name, 'InnoDB') = 0;
end;

function TSEngine.GetForeignKeyAllowed(): Boolean;
begin
  Result := IsInnoDB or IsMyISAM and (Engines.Session.ServerVersion >= 50200);
end;

function TSEngine.GetIsMerge(): Boolean;
begin
  Result := (Engines.NameCmp(Name, 'MERGE') = 0)
    or (Engines.NameCmp(Name, 'MRG_ISAM') = 0)
    or (Engines.NameCmp(Name, 'MRG_MYISAM') = 0);
end;

function TSEngine.GetIsMyISAM(): Boolean;
begin
  Result := (Engines.NameCmp(Name, 'MyISAM') = 0);
end;

function TSEngine.ApplyMySQLFieldType(const MySQLFieldType: TMySQLFieldType; const MySQLFieldSize: Integer): TMySQLFieldType;
begin
  Result := Engines.Session.FieldTypes.ApplyMySQLFieldType(Self, MySQLFieldType);

  if (((Result in [mfChar, mfVarChar]) and (Engines.Session.ServerVersion < 50003) or (Result in [mfTinyText])) and (MySQLFieldSize >= 1 shl 8)) then
    Result := mfText;
  if (((Result in [mfChar, mfVarChar]) and (Engines.Session.ServerVersion >= 50003) or (Result in [mfText])) and (MySQLFieldSize >= 1 shl 16)) then
    Result := mfMediumText;
  if ((Result in [mfMediumText]) and (MySQLFieldSize >= 1 shl 24)) then
    Result := mfLongText;

  if (((Result in [mfBinary, mfVarBinary]) and (Engines.Session.ServerVersion < 50003) or (Result in [mfTinyBlob])) and (MySQLFieldSize >= 1 shl 8)) then
    Result := mfBlob;
  if (((Result in [mfBinary, mfVarBinary]) and (Engines.Session.ServerVersion >= 50003) or (Result in [mfBlob])) and (MySQLFieldSize >= 1 shl 16)) then
    Result := mfMediumBlob;
  if ((Result in [mfMediumBlob]) and (MySQLFieldSize >= 1 shl 24)) then
    Result := mfLongBlob;
end;

{ TSEngines *******************************************************************}

function TSEngines.Build(const DataSet: TMySQLQuery; const UseInformationSchema: Boolean; Filtered: Boolean = False; const SessionEvents: Boolean = True): Boolean;
var
  DeleteList: TList;
  I: Integer;
  Index: Integer;
  Name: string;
  NewEngine: TSEngine;
begin
  DeleteList := TList.Create();
  DeleteList.Assign(Self);

  if (not Assigned(DataSet) and Session.Variables.Valid) then
  begin
    if ((Session.ServerVersion >= 32334) and Assigned(Session.VariableByName('have_bdb')) and Session.VariableByName('have_bdb').AsBoolean) then
      Add(TSEngine.Create(Self, 'BDB'));

    Add(TSEngine.Create(Self, 'HEAP'));

    if (Assigned(Session.VariableByName('have_innodb')) and Session.VariableByName('have_innodb').AsBoolean) then
      Add(TSEngine.Create(Self, 'InnoDB'));

    if (Assigned(Session.VariableByName('have_isam')) and Session.VariableByName('have_isam').AsBoolean) then
      Add(TSEngine.Create(Self, 'ISAM'));

    if (Session.ServerVersion >= 32325) then
      Add(TSEngine.Create(Self, 'MERGE'));

    Add(TSEngine.Create(Self, 'MyISAM'));
    Engine[Count - 1].FDefault := not Assigned(Session.VariableByName('table_type'));

    Add(TSEngine.Create(Self, 'MRG_MyISAM'));

    if (Assigned(Session.VariableByName('table_type'))) then
      for I := 0 to TList(Self).Count - 1 do
        Engine[I].FDefault := UpperCase(Engine[I].Name) = UpperCase(Session.VariableByName('table_type').Value);
    if (Assigned(Session.VariableByName('storage_engine'))) then
      for I := 0 to TList(Self).Count - 1 do
        Engine[I].FDefault := UpperCase(Engine[I].Name) = UpperCase(Session.VariableByName('storage_engine').Value);
  end
  else if (not DataSet.IsEmpty()) then
    repeat
      if ((not UseInformationSchema and (UpperCase(DataSet.FieldByName('Support').AsString) <> 'NO') and (UpperCase(DataSet.FieldByName('Support').AsString) <> 'DISABLED'))
        or (UseInformationSchema and (UpperCase(DataSet.FieldByName('SUPPORT').AsString) <> 'NO') and (UpperCase(DataSet.FieldByName('SUPPORT').AsString) <> 'DISABLED'))) then
      begin
        if (not UseInformationSchema) then
          Name := DataSet.FieldByName('Engine').AsString
        else
          Name := DataSet.FieldByName('ENGINE').AsString;

        if (InsertIndex(Name, Index)) then
        begin
          if (UpperCase(Name) = 'PERFORMANCE_SCHEMA') then
            NewEngine := TSSystemEngine.Create(Self, Name)
          else
            NewEngine := TSEngine.Create(Self, Name);

          if (Index < Count) then
            Insert(Index, NewEngine)
          else
            Add(NewEngine);
        end
        else if (DeleteList.IndexOf(Items[Index]) >= 0) then
          DeleteList.Delete(DeleteList.IndexOf(Items[Index]));

        if (not UseInformationSchema) then
        begin
          Engine[Index].FComment := DataSet.FieldByName('Comment').AsString;
          Engine[Index].FDefault := UpperCase(DataSet.FieldByName('Support').AsString) = 'DEFAULT';
        end
        else
        begin
          Engine[Index].FComment := DataSet.FieldByName('COMMENT').AsString;
          Engine[Index].FDefault := UpperCase(DataSet.FieldByName('SUPPORT').AsString) = 'DEFAULT';
        end;
      end;
    until (not DataSet.FindNext());

  Result := inherited;

  if (not Filtered) then
    while (DeleteList.Count > 0) do
    begin
      Index := IndexOf(DeleteList.Items[0]);
      Item[Index].Free();
      Delete(Index);
      DeleteList.Delete(0);
    end;
  DeleteList.Free();
end;

function TSEngines.GetDefaultEngine(): TSEngine;
var
  I: Integer;
begin
  Result := nil;

  for I := 0 to Count - 1 do
    if (Engine[I].Default) then
      Result := Engine[I];
end;

function TSEngines.GetEngine(Index: Integer): TSEngine;
begin
  Result := TSEngine(Items[Index]);
end;

function TSEngines.SQLGetItems(const Name: string = ''): string;
begin
  if (Session.ServerVersion < 40102) then
    Result := ''
  else if (Session.ServerVersion < 50105) then
    Result := 'SHOW ENGINES;' + #13#10
  else
    Result := 'SELECT * FROM ' + Session.EscapeIdentifier(INFORMATION_SCHEMA) + '.' + Session.EscapeIdentifier('ENGINES') + ';' + #13#10;
end;

function TSEngines.Update(): Boolean;
begin
  if (Session.ServerVersion < 40102) then
    Result := Session.Variables.Update()
  else
    Result := inherited;
end;

{ TSPlugin ********************************************************************}

function TSPlugin.GetPlugins(): TSPlugins;
begin
  Assert(CItems is TSPlugins);

  Result := TSPlugins(CItems);
end;

{ TSPlugins *******************************************************************}

function TSPlugins.Build(const DataSet: TMySQLQuery; const UseInformationSchema: Boolean; Filtered: Boolean = False; const SessionEvents: Boolean = True): Boolean;
var
  DeleteList: TList;
  Index: Integer;
  Name: string;
begin
  DeleteList := TList.Create();
  DeleteList.Assign(Self);

  if (not DataSet.IsEmpty()) then
    repeat
      if (not UseInformationSchema) then
        Name := DataSet.FieldByName('Name').AsString
      else
        Name := DataSet.FieldByName('PLUGIN_NAME').AsString;

      if (InsertIndex(Name, Index)) then
        if (Index < Count) then
          Insert(Index, TSPlugin.Create(Self, Name))
        else
          Add(TSPlugin.Create(Self, Name))
      else if (DeleteList.IndexOf(Items[Index]) >= 0) then
        DeleteList.Delete(DeleteList.IndexOf(Items[Index]));

      if (UseInformationSchema) then
        Plugin[Index].FComment := DataSet.FieldByName('PLUGIN_DESCRIPTION').AsString;

      if (Filtered and SessionEvents) then
        Session.ExecuteEvent(etItemValid, Session, Self, Plugin[Index]);
    until (not DataSet.FindNext());

  Result := inherited;

  if (not Filtered) then
    while (DeleteList.Count > 0) do
    begin
      Index := IndexOf(DeleteList.Items[0]);
      Item[Index].Free();
      Delete(Index);
      DeleteList.Delete(0);
    end;
  DeleteList.Free();

  if (not Filtered and SessionEvents) then
    Session.ExecuteEvent(etItemsValid, Session, Self);
end;

function TSPlugins.GetPlugin(Index: Integer): TSPlugin;
begin
  Result := TSPlugin(Items[Index]);
end;

function TSPlugins.SQLGetItems(const Name: string = ''): string;
begin
  if (Session.ServerVersion < 50109) then
    Result := 'SHOW PLUGIN;' + #13#10
  else if (Session.ServerVersion < 50105) then
    Result := 'SHOW PLUGINS;' + #13#10
  else
    Result := 'SELECT * FROM ' + Session.EscapeIdentifier(INFORMATION_SCHEMA) + '.' + Session.EscapeIdentifier('PLUGINS') + ';' + #13#10;
end;

{ TSFieldType *****************************************************************}

constructor TSFieldType.Create(const AFieldTypes: TSFieldTypes; const AMySQLFieldType: TMySQLFieldType; const ACaption: string; const AHighlighted: Boolean);
begin
  FieldTypes := AFieldTypes;

  FCaption := ACaption;
  FHighlighted := AHighlighted;
  FMySQLFieldType := AMySQLFieldType;
end;

function TSFieldType.DBTypeStr(): string;
begin
  Result := LowerCase(Caption);
end;

{ TSFieldTypes ****************************************************************}

procedure TSFieldTypes.Add(const AMySQLFieldType: TMySQLFieldType; const ACaption: string; const AHighlighted: Boolean);
begin
  inherited Add(TSFieldType.Create(Self, AMySQLFieldType, ACaption, AHighlighted));
end;

function TSFieldTypes.ApplyMySQLFieldType(const Engine: TSEngine; const MySQLFieldType: TMySQLFieldType): TMySQLFieldType;
begin
  if (FieldAvailable(Engine, MySQLFieldType)) then
    Result := MySQLFieldType
  else
    case (MySQLFieldType) of
      mfBit: Result := mfBigInt;
      mfGeometry,
      mfPoint,
      mfLineString,
      mfPolygon,
      mfMultiPoint,
      mfMultiLineString,
      mfMultiPolygon,
      mfGeometryCollection: Result := mfBlob;
      mfJSON: Result := mfVarChar;
      else Result := MySQLFieldType;
    end;
end;

constructor TSFieldTypes.Create(const ASession: TSSession);
begin
  inherited Create(ASession);

  FSession := ASession;

  Add(mfBit, 'Bit', False);
  Add(mfTinyInt, 'TinyInt', False);
  Add(mfSmallInt, 'SmallInt', False);
  Add(mfMediumInt, 'MediumInt', False);
  Add(mfInt, 'Int', True);
  Add(mfBigInt, 'BigInt', False);
  Add(mfFloat, 'Real', False);
  Add(mfDouble, 'Double', False);
  Add(mfFloat, 'Float', False);
  Add(mfDecimal, 'Decimal', False);
  Add(mfDecimal, 'Numeric', False);
  Add(mfDate, 'Date', False);
  Add(mfTime, 'Time', False);
  Add(mfTimeStamp, 'TimeStamp', False);
  Add(mfDateTime, 'DateTime', True);
  Add(mfYear, 'Year', False);
  Add(mfChar, 'Char', False);
  Add(mfVarChar, 'VarChar', True);
  Add(mfBinary, 'Binary', False);
  Add(mfVarBinary, 'VarBinary', False);
  Add(mfTinyBlob, 'TinyBlob', False);
  Add(mfBlob, 'Blob', True);
  Add(mfMediumBlob, 'MediumBlob', False);
  Add(mfLongBlob, 'LongBlob', False);
  Add(mfTinyText, 'TinyText', False);
  Add(mfText, 'Text', True);
  Add(mfMediumText, 'MediumText', False);
  Add(mfLongText, 'LongText', False);
  Add(mfEnum, 'Enum', False);
  Add(mfSet, 'Set', False);
  Add(mfGeometry, 'Geometry', False);
  Add(mfPoint, 'Point', False);
  Add(mfLineString, 'LineString', False);
  Add(mfPolygon, 'Polygon', False);
  Add(mfMultiPoint, 'MultiPoint', False);
  Add(mfMultiLineString, 'MultiLineString', False);
  Add(mfMultiPolygon, 'MultiPolygon', False);
  Add(mfGeometryCollection, 'GeometryCollection', False);
  Add(mfJSON, 'JSON', False);
end;

function TSFieldTypes.FieldAvailable(const Engine: TSEngine; const MySQLFieldType: TMySQLFieldType): Boolean;
begin
  case (MySQLFieldType) of
    mfUnknown: Result := False;
    mfBit: Result := Assigned(Engine) and ((Session.ServerVersion >= 50003) and (Engine.Name = 'MyISAM') or (Session.ServerVersion >= 50005) and ((Engine.Name = 'MEMORY') or Engine.IsInnoDB or (Engine.Name = 'BDB')));
    mfBinary,
    mfVarBinary: Result := Session.ServerVersion >= 40102;
    mfGeometry,
    mfPoint,
    mfLineString,
    mfPolygon,
    mfMultiPoint,
    mfMultiLineString,
    mfMultiPolygon,
    mfGeometryCollection: Result := Assigned(Engine) and (Assigned(Session.VariableByName('have_geometry')) and Session.VariableByName('have_geometry').AsBoolean and ((Engine.Name = 'MyISAM') or (Session.ServerVersion >= 50016) and (Engine.IsInnoDB or (Engine.Name = 'NDB') or (Engine.Name = 'BDB') or (Engine.Name = 'ARCHIVE'))));
    mfJSON: Result := Pos('JSON', UpperCase(Session.ServerVersionStr)) > 0;
    else Result := True;
  end;
end;

function TSFieldTypes.GetFieldType(Index: Integer): TSFieldType;
begin
  Result := TSFieldType(Items[Index]);
end;

{ TSCharset *******************************************************************}

function TSCharset.GetCharsets(): TSCharsets;
begin
  Assert(CItems is TSCharsets);

  Result := TSCharsets(CItems);
end;

function TSCharset.GetDefaultCollation(): TSCollation;
var
  I: Integer;
begin
  Result := nil;

  if (Assigned(Charsets.Session.Collations)) then
    for I := 0 to Charsets.Session.Collations.Count - 1 do
      if ((Charsets.Session.Collations[I].Charset = Self) and Charsets.Session.Collations[I].Default) then
        Result := Charsets.Session.Collations[I];
end;

{ TSCharsets ******************************************************************}

function TSCharsets.Build(const DataSet: TMySQLQuery; const UseInformationSchema: Boolean; Filtered: Boolean = False; const SessionEvents: Boolean = True): Boolean;
var
  DeleteList: TList;
  Index: Integer;
  Name: string;
  Names: string;
begin
  DeleteList := TList.Create();
  DeleteList.Assign(Self);

  if (not Assigned(DataSet) and Session.Variables.Valid) then
  begin
    if (Assigned(Session.VariableByName('character_sets'))) then
    begin
      Names := Session.VariableByName('character_sets').Value;

      while (Names <> '') do
      begin
        if (Pos(' ', Names) > 0) then
        begin
          Name := Copy(Names, 1, Pos(' ', Names) - 1);
          System.Delete(Names, 1, Pos(' ', Names));
        end
        else
        begin
          Name := Names;
          Names := '';
        end;

        if (InsertIndex(Name, Index)) then
          if (Index < Count) then
            Insert(Index, TSCharset.Create(Self, Name))
          else
            Add(TSCharset.Create(Self, Name))
        else if (DeleteList.IndexOf(Items[Index]) >= 0) then
          DeleteList.Delete(DeleteList.IndexOf(Items[Index]));
      end;
    end;
  end
  else if (not DataSet.IsEmpty()) then
    repeat
      if (not UseInformationSchema) then
        Name := DataSet.FieldByName('Charset').AsString
      else
        Name := DataSet.FieldByName('CHARACTER_SET_NAME').AsString;

      if (InsertIndex(Name, Index)) then
        if (Index < Count) then
          Insert(Index, TSCharset.Create(Self, Name))
        else
          Add(TSCharset.Create(Self, Name))
      else if (DeleteList.IndexOf(Items[Index]) >= 0) then
        DeleteList.Delete(DeleteList.IndexOf(Items[Index]));

      if (not UseInformationSchema) then
      begin
        Charset[Index].FHint := DataSet.FieldByName('Description').AsString;
        Charset[Index].FCollation := LowerCase(DataSet.FieldByName('Default collation').AsString);
        Charset[Index].FMaxLength := DataSet.FieldByName('Maxlen').AsInteger;
      end
      else
      begin
        Charset[Index].FHint := DataSet.FieldByName('DESCRIPTION').AsString;
        Charset[Index].FCollation := LowerCase(DataSet.FieldByName('DEFAULT_COLLATE_NAME').AsString);
        Charset[Index].FMaxLength := DataSet.FieldByName('MAXLEN').AsInteger;
      end;
    until (not DataSet.FindNext());

  Result := inherited;

  if (not Filtered) then
    while (DeleteList.Count > 0) do
    begin
      Index := IndexOf(DeleteList.Items[0]);
      Item[Index].Free();
      Delete(Index);
      DeleteList.Delete(0);
    end;
  DeleteList.Free();
end;

function TSCharsets.GetCharset(Index: Integer): TSCharset;
begin
  Result := TSCharset(Items[Index]);
end;

function TSCharsets.SQLGetItems(const Name: string = ''): string;
begin
  if (Session.ServerVersion < 40100) then
    Result := ''
  else if (Session.ServerVersion < 50006) then
    Result := 'SHOW CHARACTER SET;' + #13#10
  else
    Result := 'SELECT * FROM ' + Session.EscapeIdentifier(INFORMATION_SCHEMA) + '.' + Session.EscapeIdentifier('CHARACTER_SETS') + ';' + #13#10;
end;

function TSCharsets.Update(): Boolean;
begin
  if (Session.ServerVersion < 40100) then
    Result := Session.Variables.Update()
  else
    Result := inherited;
end;

{ TSCollation *****************************************************************}

function TSCollation.GetCollations(): TSCollations;
begin
  Assert(CItems is TSCollations);

  Result := TSCollations(CItems);
end;

{ TSCollations ****************************************************************}

function TSCollations.Build(const DataSet: TMySQLQuery; const UseInformationSchema: Boolean; Filtered: Boolean = False; const SessionEvents: Boolean = True): Boolean;
var
  DeleteList: TList;
  Index: Integer;
  Name: string;
begin
  DeleteList := TList.Create();
  DeleteList.Assign(Self);

  if (not DataSet.IsEmpty()) then
    repeat
      if (not UseInformationSchema) then
        Name := DataSet.FieldByName('Collation').AsString
      else
        Name := DataSet.FieldByName('COLLATION_NAME').AsString;

      if (InsertIndex(Name, Index)) then
        if (Index < Count) then
          Insert(Index, TSCollation.Create(Self, Name))
        else
          Add(TSCollation.Create(Self, Name))
      else if (DeleteList.IndexOf(Items[Index]) >= 0) then
        DeleteList.Delete(DeleteList.IndexOf(Items[Index]));

      if (not UseInformationSchema) then
      begin
        Collation[Index].FCharset := Session.CharsetByName(DataSet.FieldByName('Charset').AsString);
        Collation[Index].FId := DataSet.FieldByName('Id').AsInteger;
        Collation[Index].FDefault := UpperCase(DataSet.FieldByName('Default').AsString) = 'YES';
        Collation[Index].FCompiled := UpperCase(DataSet.FieldByName('Compiled').AsString) = 'YES';
        Collation[Index].FSortLength := DataSet.FieldByName('Sortlen').AsInteger;
      end
      else
      begin
        Collation[Index].FCharset := Session.CharsetByName(DataSet.FieldByName('CHARACTER_SET_NAME').AsString);
        Collation[Index].FId := DataSet.FieldByName('ID').AsInteger;
        Collation[Index].FDefault := UpperCase(DataSet.FieldByName('IS_DEFAULT').AsString) = 'YES';
        Collation[Index].FCompiled := UpperCase(DataSet.FieldByName('IS_COMPILED').AsString) = 'YES';
        Collation[Index].FSortLength := DataSet.FieldByName('SORTLEN').AsInteger;
      end;
    until (not DataSet.FindNext());

  Result := inherited;

  if (not Filtered) then
    while (DeleteList.Count > 0) do
    begin
      Index := IndexOf(DeleteList.Items[0]);
      Item[Index].Free();
      Delete(Index);
      DeleteList.Delete(0);
    end;
  DeleteList.Free();
end;

function TSCollations.GetCollation(Index: Integer): TSCollation;
begin
  Result := TSCollation(Items[Index]);
end;

function TSCollations.SQLGetItems(const Name: string = ''): string;
begin
  if (Session.ServerVersion < 50006) then
    Result := 'SHOW COLLATION;' + #13#10
  else
    Result := 'SELECT * FROM ' + Session.EscapeIdentifier(INFORMATION_SCHEMA) + '.' + Session.EscapeIdentifier('COLLATIONS') + ';' + #13#10;
end;

{ TSProcesse ******************************************************************}

function TSProcess.GetThreadId(): Longword;
begin
  Result := StrToInt(Name);
end;

procedure TSProcess.SetThreadId(AThreadId: Longword);
begin
  Name := IntToStr(AThreadId);
end;

{ TSProcesses *****************************************************************}

function TSProcesses.Build(const DataSet: TMySQLQuery; const UseInformationSchema: Boolean; Filtered: Boolean = False; const SessionEvents: Boolean = True): Boolean;
var
  Days: Integer;
  DeleteList: TList;
  Hours: Integer;
  Index: Integer;
  Minutes: Integer;
  Name: string;
  Seconds: Integer;
begin
  DeleteList := TList.Create();
  DeleteList.Assign(Self);

  if (not DataSet.IsEmpty()) then
    repeat
      if (not UseInformationSchema) then
        Name := DataSet.FieldByName('Id').AsString
      else
        Name := DataSet.FieldByName('ID').AsString;
      Name := SQLUnescape(Name);

      if (InsertIndex(Name, Index)) then
        if (Index < Count) then
          Insert(Index, TSProcess.Create(Self, Name))
        else
          Add(TSProcess.Create(Self, Name))
      else if (DeleteList.IndexOf(Items[Index]) >= 0) then
        DeleteList.Delete(DeleteList.IndexOf(Items[Index]));

      if (not UseInformationSchema) then
      begin
        Process[Index].FUserName := DataSet.FieldByName('User').AsString;
        Process[Index].FHost := DataSet.FieldByName('Host').AsString;
        Process[Index].FDatabaseName := DataSet.FieldByName('db').AsString;
        Process[Index].FCommand := DataSet.FieldByName('Command').AsString;
        if (not TryStrToInt(DataSet.FieldByName('Time').AsString, Seconds) or (Seconds < 0)) then
          Process[Index].FTime := 0
        else
        begin
          Minutes := Seconds div SecsPerMin; Seconds := Seconds mod SecsPerMin;
          Hours := Minutes div MinsPerHour; Minutes := Minutes mod MinsPerHour;
          Days := Hours div HoursPerDay; Hours := Hours mod HoursPerDay;
          Process[Index].FTime := Days + EncodeTime(Hours, Minutes, Seconds, 0);
        end;
        Process[Index].FState := DataSet.FieldByName('State').AsString;
        Process[Index].FSQL := DataSet.FieldByName('Info').AsString;
      end
      else
      begin
        Process[Index].FUserName := DataSet.FieldByName('USER').AsString;
        Process[Index].FHost := DataSet.FieldByName('HOST').AsString;
        Process[Index].FDatabaseName := DataSet.FieldByName('DB').AsString;
        Process[Index].FCommand := DataSet.FieldByName('COMMAND').AsString;
        if (not TryStrToInt(DataSet.FieldByName('TIME').AsString, Seconds) or (Seconds < 0)) then
          Process[Index].FTime := 0
        else
        begin
          Minutes := Seconds div SecsPerMin; Seconds := Seconds mod SecsPerMin;
          Hours := Minutes div MinsPerHour; Minutes := Minutes mod MinsPerHour;
          Days := Hours div HoursPerDay; Hours := Hours mod HoursPerDay;
          Process[Index].FTime := Days + EncodeTime(Hours, Minutes, Seconds, 0);
        end;
        Process[Index].FState := DataSet.FieldByName('STATE').AsString;
        Process[Index].FSQL := DataSet.FieldByName('INFO').AsString;
      end;

      if (Filtered and SessionEvents) then
        Session.ExecuteEvent(etItemValid, Session, Self, Process[Index]);
    until (not DataSet.FindNext());

  Result := inherited;

  if (not Filtered) then
    while (DeleteList.Count > 0) do
    begin
      Index := IndexOf(DeleteList.Items[0]);
      Item[Index].Free();
      Delete(Index);
      DeleteList.Delete(0);
    end;
  DeleteList.Free();

  if (not Filtered) then
    Session.ExecuteEvent(etItemsValid, Session, Self);
end;

procedure TSProcesses.Delete(const AEntity: TSEntity);
begin
  inherited;

  Session.ExecuteEvent(etItemDropped, Session, Self, AEntity);
end;

function TSProcesses.GetProcess(Index: Integer): TSProcess;
begin
  Result := TSProcess(Items[Index]);
end;

function TSProcesses.GetValid(): Boolean;
begin
  if ((Session.ServerVersion >= 50000) and (not Assigned(Session.UserRights) or not Session.UserRights.RProcess)) then
    Result := False
  else
    Result := inherited;
end;

function TSProcesses.NameCmp(const Name1, Name2: string): Integer;
begin
  Result := Sign(StrToInt(Name1) - StrToInt(Name2));
end;

function TSProcesses.SQLGetItems(const Name: string = ''): string;
begin
  if (Session.ServerVersion < 50107) then
    Result := 'SHOW FULL PROCESSLIST;' + #13#10
  else
    Result := 'SELECT * FROM ' + Session.EscapeIdentifier(INFORMATION_SCHEMA) + '.' + Session.EscapeIdentifier('PROCESSLIST') + ';' + #13#10;
end;

{ TSUserRight *****************************************************************}

procedure TSUserRight.Assign(const Source: TSUserRight);
begin
  DatabaseName := Source.DatabaseName;
  TableName := Source.TableName;
  ProcedureName := Source.ProcedureName;
  FunctionName := Source.FunctionName;
  FieldName := Source.FieldName;

  RAlter := Source.RAlter;
  RAlterRoutine := Source.RAlterRoutine;
  RCreate := Source.RCreate;
  RCreateTableSpace := Source.RCreateTableSpace;
  RCreateTempTable := Source.RCreateTempTable;
  RCreateRoutine := Source.RCreateRoutine;
  RCreateUser := Source.RCreateUser;
  RCreateView := Source.RCreateView;
  RDelete := Source.RDelete;
  RDrop := Source.RDrop;
  REvent := Source.REvent;
  RExecute := Source.RExecute;
  RFile := Source.RFile;
  RGrant := Source.RGrant;
  RIndex := Source.RIndex;
  RInsert := Source.RInsert;
  RLockTables := Source.RLockTables;
  RProcess := Source.RProcess;
  RProxy := Source.RProxy;
  RReferences := Source.RReferences;
  RReload := Source.RReload;
  RReplClient := Source.RReplClient;
  RReplSlave := Source.RReplSlave;
  RSelect := Source.RSelect;
  RShowDatabases := Source.RShowDatabases;
  RShowView := Source.RShowView;
  RShutdown := Source.RShutdown;
  RSuper := Source.RSuper;
  RTrigger := Source.RTrigger;
  RUpdate := Source.RUpdate;
end;

constructor TSUserRight.Create();
begin
  inherited Create();

  DatabaseName := '';
  TableName := '';
  ProcedureName := '';
  FunctionName := '';
  FieldName := '';

  RAlter := False;
  RAlterRoutine := False;
  RCreate := False;
  RCreateTableSpace := False;
  RCreateTempTable := False;
  RCreateRoutine := False;
  RCreateUser := False;
  RCreateView := False;
  RDelete := False;
  RDrop := False;
  REvent := False;
  RExecute := False;
  RFile := False;
  RGrant := False;
  RIndex := False;
  RInsert := False;
  RLockTables := False;
  RProcess := False;
  RProxy := False;
  RReferences := False;
  RReload := False;
  RReplClient := False;
  RReplSlave := False;
  RSelect := False;
  RShowDatabases := False;
  RShowView := False;
  RShutdown := False;
  RTrigger := False;
  RSuper := False;
  RUpdate := False;
end;

function TSUserRight.GetCaption(): string;
begin
  Result := '';
  if (DatabaseName <> '') then
    Result := Result + DatabaseName;
  if (TableName <> '') then
  begin
    Result := Result + '.' + TableName;
    if (FieldName <> '') then
      Result := Result + '.' + FieldName;
  end
  else if (ProcedureName <> '') then
    Result := Result + '.' + ProcedureName
  else if (FunctionName <> '') then
    Result := Result + '.' + FunctionName;
  if (Result = '') then
    Result := '<' + Preferences.LoadStr(214) + '>';
end;

{ TSUser **********************************************************************}

function TSUser.AddRight(const NewUserRight: TSUserRight): Boolean;
var
  I: Integer;
  Index: Integer;
begin
  Index := FRights.Count;
  for I := FRights.Count - 1 downto 0 do
    if (lstrcmpi(PChar(NewUserRight.DatabaseName), PChar(Right[I].DatabaseName)) < 0) then
      Index := I
    else if (lstrcmpi(PChar(NewUserRight.DatabaseName), PChar(Right[I].DatabaseName)) = 0) then
      if ((lstrcmpi(PChar(NewUserRight.TableName), PChar(Right[I].TableName)) < 0) and (NewUserRight.ProcedureName = '') and (NewUserRight.FunctionName = '')) then
        Index := I
      else if ((lstrcmpi(PChar(NewUserRight.TableName), PChar(Right[I].TableName)) = 0) and (NewUserRight.ProcedureName = '') and (NewUserRight.FunctionName = '')) then
        if ((lstrcmpi(PChar(NewUserRight.FieldName), PChar(Right[I].FieldName)) < 0) and (NewUserRight.ProcedureName = '') and (NewUserRight.FunctionName = '')) then
          Index := I
        else if (lstrcmpi(PChar(NewUserRight.ProcedureName), PChar(Right[I].ProcedureName)) < 0) then
          Index := I
        else if (lstrcmpi(PChar(NewUserRight.FunctionName), PChar(Right[I].FunctionName)) < 0) then
          Index := I;

  FRights.Insert(Index, TSUserRight.Create());
  TSUserRight(FRights[Index]).Assign(NewUserRight);

  Result := True;
end;

procedure TSUser.Assign(const Source: TSUser);
var
  I: Integer;
begin
  inherited Assign(Source);

  if (not Assigned(FCItems)) then FCItems := Source.Users;

  FConnectionsPerHour := Source.ConnectionsPerHour;
  FRawPassword := Source.RawPassword;
  FNewPassword := Source.NewPassword;
  FQueriesPerHour := Source.QueriesPerHour;
  for I := 0 to Source.FRights.Count - 1 do
  begin
    FRights.Add(TSUserRight.Create());
    TSUserRight(FRights[I]).Assign(Source.FRights[I]);
  end;
  FUpdatesPerHour := Source.UpdatesPerHour;
  FUserConnections := Source.UserConnections;
end;

constructor TSUser.Create(const ACItems: TSItems; const AName: string = '');
begin
  inherited;

  ConnectionsPerHour := 0;
  NewPassword := '';
  QueriesPerHour := 0;
  RawPassword := '';
  FRights := TList.Create();
  UpdatesPerHour := 0;
  UserConnections := 0;
end;

procedure TSUser.DeleteRight(const UserRight: TSUserRight);
var
  Index: Integer;
begin
  if (UserRight <> nil) then
  begin
    Index := IndexOf(UserRight);

    Right[Index].Free();
    FRights.Delete(Index);
  end;
end;

destructor TSUser.Destroy();
begin
  while (FRights.Count > 0) do
  begin
    TSUserRight(FRights[0]).Free();
    FRights.Delete(0);
  end;
  FRights.Free();

  inherited;
end;

function TSUser.GetCaption(): string;
begin
  if (Name <> '') then
    Result := Name
  else
    Result := '<' + Preferences.LoadStr(287) + '>';
end;

function TSUser.GetHost(): string;
begin
  if (Pos('@', Name) = 0) then
    Result := ''
  else
  begin
    Result := Name;
    Delete(Result, 1, Pos('@', Name));
  end;
end;

function TSUser.GetLogin(): string;
begin
  if (Pos('@', Name) = 0) then
    Result := Name
  else
    Result := Copy(Name, 1, Pos('@', Name) - 1);
end;

function TSUser.GetRight(Index: Integer): TSUserRight;
begin
  Result := FRights[Index];
end;

function TSUser.GetRightCount(): Integer;
begin
  Result := FRights.Count;
end;

function TSUser.GetSlowSQLLog(): string;
begin
  Result := Session.GetSlowSQLLog(Self);
end;

function TSUser.GetSQLLog(): string;
begin
  Result := Session.GetSQLLog(Self);
end;

function TSUser.GetUsers(): TSUsers;
begin
  Assert(CItems is TSUsers);

  Result := TSUsers(CItems);
end;

function TSUser.GetValid(): Boolean;
begin
  Result := ValidSource and (RightCount > 0);
end;

function TSUser.IndexOf(const UserRight: TSUserRight): Integer;
var
  I: Integer;
begin
  Result := - 1;

  for I := 0 to FRights.Count - 1 do
    if (Right[I] = UserRight) then
      Result := I;
end;

procedure TSUser.ParseGrant(const SQL: string);

  procedure AddPrivileg(const Right: TSUserRight; const Privileg: string);
  begin
    with Right do
      begin
        RAlter           := (RAlter           or (Privileg = 'ALTER')                   or (Privileg = 'ALL PRIVILEGES'));
        RAlterRoutine    := (RAlterRoutine    or (Privileg = 'ALTER ROUTINE')           or (Privileg = 'ALL PRIVILEGES')) and (Users.Session.ServerVersion >= 50003);
        RCreate          := (RCreate          or (Privileg = 'CREATE')                  or (Privileg = 'ALL PRIVILEGES'));
        RCreateRoutine   := (RCreateRoutine   or (Privileg = 'CREATE ROUTINE')          or (Privileg = 'ALL PRIVILEGES')) and (Users.Session.ServerVersion >= 50003);
        RCreateTableSpace:= (RCreateTableSpace or(Privileg = 'CREATE TABLESPACE')       or (Privileg = 'ALL PRIVILEGES')) and (Users.Session.ServerVersion >= 50500);
        RCreateTempTable := (RCreateTempTable or (Privileg = 'CREATE TEMPORARY TABLES') or (Privileg = 'ALL PRIVILEGES')) and (Users.Session.ServerVersion >= 40002);
        RCreateUser      := (RCreateUser      or (Privileg = 'CREATE USER')             or (Privileg = 'ALL PRIVILEGES')) and (Users.Session.ServerVersion >= 50003);
        RCreateView      := (RCreateView      or (Privileg = 'CREATE VIEW')             or (Privileg = 'ALL PRIVILEGES')) and (Users.Session.ServerVersion >= 50001);
        RDelete          := (RDelete          or (Privileg = 'DELETE')                  or (Privileg = 'ALL PRIVILEGES'));
        RDrop            := (RDrop            or (Privileg = 'DROP')                    or (Privileg = 'ALL PRIVILEGES'));
        REvent           := (REvent           or (Privileg = 'EVENT')                   or (Privileg = 'ALL PRIVILEGES')) and (Users.Session.ServerVersion >= 50106);
        RExecute         := (RExecute         or (Privileg = 'EXECUTE')                 or (Privileg = 'ALL PRIVILEGES')) and (Users.Session.ServerVersion >= 50003);
        RFile            := (RFile            or (Privileg = 'FILE')                    or (Privileg = 'ALL PRIVILEGES'));
        RGrant           := (RGrant           or (Privileg = 'GRANT OPTION')            or (Privileg = 'ALL PRIVILEGES'));
        RIndex           := (RIndex           or (Privileg = 'INDEX')                   or (Privileg = 'ALL PRIVILEGES'));
        RInsert          := (RInsert          or (Privileg = 'INSERT')                  or (Privileg = 'ALL PRIVILEGES'));
        RLockTables      := (RLockTables      or (Privileg = 'LOCK TABLES')             or (Privileg = 'ALL PRIVILEGES')) and (Users.Session.ServerVersion >= 40002);
        RProcess         := (RProcess         or (Privileg = 'PROCESS')                 or (Privileg = 'ALL PRIVILEGES'));
        RProxy           := (RProxy           or (Privileg = 'PROXY')                   or (Privileg = 'ALL PRIVILEGES')) and (Users.Session.ServerVersion >= 50507);
        RReferences      := (RReferences      or (Privileg = 'REFERENCES')              or (Privileg = 'ALL PRIVILEGES'));
        RReload          := (RReload          or (Privileg = 'RELOAD')                  or (Privileg = 'ALL PRIVILEGES'));
        RReplClient      := (RReplClient      or (Privileg = 'REPLICATION CLIENT')      or (Privileg = 'ALL PRIVILEGES')) and (Users.Session.ServerVersion >= 40002);
        RReplSlave       := (RReplSlave       or (Privileg = 'REPLICATION SLAVE')       or (Privileg = 'ALL PRIVILEGES')) and (Users.Session.ServerVersion >= 40002);
        RSelect          := (RSelect          or (Privileg = 'SELECT')                  or (Privileg = 'ALL PRIVILEGES'));
        RShowDatabases   := (RShowDatabases   or (Privileg = 'SHOW DATABASES')          or (Privileg = 'ALL PRIVILEGES')) and (Users.Session.ServerVersion >= 40002);
        RShowView        := (RShowView        or (Privileg = 'SHOW VIEW')               or (Privileg = 'ALL PRIVILEGES')) and (Users.Session.ServerVersion >= 50001);
        RShutdown        := (RShutdown        or (Privileg = 'SHUTDOWN')                or (Privileg = 'ALL PRIVILEGES'));
        RSuper           := (RSuper           or (Privileg = 'SUPER')                   or (Privileg = 'ALL PRIVILEGES')) and (Users.Session.ServerVersion >= 40002);
        RTrigger         := (RTrigger         or (Privileg = 'TRIGGER')                 or (Privileg = 'ALL PRIVILEGES')) and (Users.Session.ServerVersion >= 50106);
        RUpdate          := (RUpdate          or (Privileg = 'UPDATE')                  or (Privileg = 'ALL PRIVILEGES'));
      end;
  end;

var
  DatabaseName: string;
  FieldName: string;
  FunctionName: string;
  Grant: Boolean;
  I: Integer;
  Index: Integer;
  NewRights: array of TSUserRight;
  Parse: TSQLParse;
  Privileg: string;
  ProcedureName: string;
  ProxyName: string;
  RawPassword: string;
  TableName: string;
begin
  FRights.Clear();

  if (SQLCreateParse(Parse, PChar(SQL), Length(SQL), Users.Session.ServerVersion)) then
  begin
    while (not SQLParseEnd(Parse)) do
    begin
      if (not SQLParseKeyword(Parse, 'GRANT')) then raise EConvertError.CreateFmt(SSourceParseError, [Name, SQL]);

      repeat
        Privileg := '';
        while (not SQLParseChar(Parse, '(', False) and not SQLParseChar(Parse, ',') and not SQLParseKeyword(Parse, 'ON', False)) do
        begin
          if (Privileg <> '') then Privileg := Privileg + ' ';
          Privileg := Privileg + SQLParseValue(Parse);
        end;

        if (Privileg = 'PROXY') then
        else if (not SQLParseChar(Parse, '(')) then
        begin
          Index := -1;
          for I := 0 to Length(NewRights) - 1 do
            if (NewRights[I].FieldName = '') then
              Index := I;

          if (Index < 0) then
          begin
            SetLength(NewRights, Length(NewRights) + 1);
            Index := Length(NewRights) - 1;
            NewRights[Index] := TSUserRight.Create();
          end;

          AddPrivileg(NewRights[Index], Privileg);
        end
        else
        begin
          repeat
            FieldName := SQLParseValue(Parse);

            Index := -1;
            for I := 0 to Length(NewRights) - 1 do
              if (lstrcmpi(PChar(NewRights[I].FieldName), PChar(FieldName)) = 0) then
                Index := I;

            if (Index < 0) then
            begin
              SetLength(NewRights, Length(NewRights) + 1);
              Index := Length(NewRights) - 1;
              NewRights[Index] := TSUserRight.Create();
              NewRights[Index].FieldName := FieldName;
            end;

            AddPrivileg(NewRights[Index], Privileg);
          until (not SQLParseChar(Parse, ','));
          if (not SQLParseChar(Parse, ')')) then raise EConvertError.CreateFmt(SSourceParseError, [Name, SQL]);
        end;
      until (SQLParseKeyword(Parse, 'ON'));

      DatabaseName := ''; TableName := ''; ProcedureName := ''; FunctionName := ''; RawPassword := '';

      if (Privileg = 'PROXY') then
        ProxyName := SQLParseValue(Parse)
      else if (SQLParseKeyword(Parse, 'PROCEDURE')) then
      begin
        ProcedureName := SQLParseValue(Parse);
        if (SQLParseChar(Parse, '.')) then
        begin
          DatabaseName := ProcedureName;
          ProcedureName := SQLParseValue(Parse);
        end;
      end
      else if (SQLParseKeyword(Parse, 'FUNCTION')) then
      begin
        FunctionName := SQLParseValue(Parse);
        if (SQLParseChar(Parse, '.')) then
        begin
          DatabaseName := FunctionName;
          FunctionName := SQLParseValue(Parse);
        end;
      end
      else
      begin
        SQLParseKeyword(Parse, 'TABLE');

        TableName := SQLParseValue(Parse);
        if (SQLParseChar(Parse, '.')) then
        begin
          DatabaseName := TableName;
          TableName := SQLParseValue(Parse);
        end;
      end;

      if (DatabaseName = '*') then DatabaseName := '';
      if (TableName = '*') then TableName := '';

      if (not SQLParseKeyword(Parse, 'TO')) then raise EConvertError.CreateFmt(SSourceParseError, [Name, SQL]);

      FName := SQLParseValue(Parse);

      if (SQLParseKeyword(Parse, 'IDENTIFIED BY')) then
      begin
        SQLParseKeyword(Parse, 'PASSWORD');
        if (SQLParseChar(Parse, '''', False)) then
          RawPassword := SQLParseValue(Parse)
        else if (SQLParseChar(Parse, '<', False)) then
          SQLParseValue(Parse);
      end;

      if (SQLParseKeyword(Parse, 'REQUIRE')) then
        repeat SQLParseValue(Parse); until (SQLParseChar(Parse, ';') or SQLParseKeyword(Parse, 'WITH', False));

      Grant := False;
      if (SQLParseKeyword(Parse, 'WITH')) then
        repeat
          if (SQLParseKeyword(Parse, 'GRANT OPTION')) then
            Grant := True
          else if (SQLParseKeyword(Parse, 'MAX_QUERIES_PER_HOUR')) then
            QueriesPerHour := StrToInt64(SQLParseValue(Parse))
          else if (SQLParseKeyword(Parse, 'MAX_UPDATES_PER_HOUR')) then
            UpdatesPerHour := StrToInt64(SQLParseValue(Parse))
          else if (SQLParseKeyword(Parse, 'MAX_CONNECTIONS_PER_HOUR')) then
            ConnectionsPerHour := StrToInt64(SQLParseValue(Parse))
          else if (SQLParseKeyword(Parse, 'MAX_USER_CONNECTIONS')) then
            UserConnections := StrToInt64(SQLParseValue(Parse))
          else
            SQLParseValue(Parse);
        until (SQLParseChar(Parse, ';', False) or SQLParseEnd(Parse));

      if (not SQLParseChar(Parse, ';')) then raise EConvertError.CreateFmt(SSourceParseError, [Name, SQL]);

      for I := 0 to Length(NewRights) - 1 do
        if (Privileg <> 'PROXY') then
        begin
          NewRights[I].DatabaseName := DatabaseName;
          NewRights[I].TableName := TableName;
          NewRights[I].ProcedureName := ProcedureName;
          NewRights[I].FunctionName := FunctionName;
          NewRights[I].FRawPassword := RawPassword;
          NewRights[I].RGrant := NewRights[I].RGrant or Grant;
          AddRight(NewRights[I]);
        end;

      for I := 0 to Length(NewRights) - 1 do
        NewRights[I].Free();
      SetLength(NewRights, 0);
    end;
  end;
end;

function TSUser.RightByCaption(const Caption: string): TSUserRight;
var
  I: Integer;
begin
  Result := nil;

  for I := 0 to FRights.Count - 1 do
    if (Right[I].Caption = Caption) then
      Result := Right[I];
end;

procedure TSUser.SetName(const AName: string);
begin
  inherited;

  if ((Pos('@', FName) = 0) and (FName <> '')) then
    Name := FName + '@%';
end;

procedure TSUser.SetSource(const ADataSet: TMySQLQuery);
var
  Source: string;
begin
  Source := '';
  if (not ADataSet.IsEmpty) then
    repeat
      Source := Source + ADataSet.Fields[0].AsString + ';' + #13#10;
    until (not ADataSet.FindNext());

  if (Source <> '') then
  begin
    SetSource(Source);

    if (Valid) then
    begin
      Session.ExecuteEvent(etItemsValid, Session, Session.Users);
      Session.ExecuteEvent(etItemValid, Session, Users, Self);
    end;
  end;
end;

procedure TSUser.SetSource(const ASource: string);
begin
  inherited;

  if (Source <> '') then
    ParseGrant(Source);
end;

function TSUser.SQLGetSource(): string;
begin
  Result := 'SHOW GRANTS FOR ' + Session.EscapeUser(Name) + ';' + #13#10
end;

function TSUser.Update(): Boolean;
var
  List: TList;
begin
  List := TList.Create();
  List.Add(Self);
  Result := Session.Update(List);
  List.Free();
end;

function TSUser.UpdateRight(const UserRight,  NewUserRight: TSUserRight): Boolean;
var
  Index: Integer;
begin
  Index := IndexOf(UserRight);

  Result := Index >= 0;
  if (Result) then
    Right[Index].Assign(NewUserRight);
end;

{ TSUsers *********************************************************************}

function TSUsers.Build(const DataSet: TMySQLQuery; const UseInformationSchema: Boolean; Filtered: Boolean = False; const SessionEvents: Boolean = True): Boolean;
var
  DeleteList: TList;
  Index: Integer;
  Name: string;
  Parse: TSQLParse;
begin
  DeleteList := TList.Create();
  DeleteList.Assign(Self);

  if (not DataSet.IsEmpty()) then
    repeat
      if (not UseInformationSchema) then
        Name := DataSet.FieldByName('User').AsString + '@' + DataSet.FieldByName('Host').AsString
      else if (SQLCreateParse(Parse, PChar(DataSet.FieldByName('GRANTEE').AsString), Length(DataSet.FieldByName('GRANTEE').AsString), Session.ServerVersion)) then
        Name := SQLParseValue(Parse)
      else
        raise ERangeError.CreateFmt(SPropertyOutOfRange, ['Name']);

      if (InsertIndex(Name, Index)) then
        if (Index < Count) then
          Insert(Index, TSUser.Create(Self, Name))
        else
          Add(TSUser.Create(Self, Name))
      else if (DeleteList.IndexOf(Items[Index]) >= 0) then
        DeleteList.Delete(DeleteList.IndexOf(Items[Index]));

      if (Filtered and SessionEvents) then
        Session.ExecuteEvent(etItemValid, Session, Self, User[Index]);
    until (not DataSet.FindNext());

  Result := inherited or (Session.ErrorCode = ER_DBACCESS_DENIED_ERROR) or (Session.ErrorCode = ER_TABLEACCESS_DENIED_ERROR);

  if (Result and not Filtered) then
    while (DeleteList.Count > 0) do
    begin
      if (Items[0] = Session.User) then
        Session.FUser := nil;
      Index := IndexOf(DeleteList.Items[0]);
      Item[Index].Free();
      Delete(Index);
      DeleteList.Delete(0);
    end;
  DeleteList.Free();

  if (not Filtered and SessionEvents) then
    Session.ExecuteEvent(etItemsValid, Session, Self);
end;

function TSUsers.GetUser(Index: Integer): TSUser;
begin
  Result := TSUser(Items[Index]);
end;

function TSUsers.GetValid(): Boolean;
begin
  Result := (Assigned(Session.UserRights) and not Session.UserRights.RGrant) or inherited;
end;

function TSUsers.SQLGetItems(const Name: string = ''): string;
begin
  if (Session.ServerVersion < 50002) then
    Result := 'SELECT * FROM ' + Session.EscapeIdentifier('mysql') + '.' + Session.EscapeIdentifier('user') + ';' + #13#10
  else
  begin
    // If "only_full_group_by" is set in the sql_mode, the column of the GROUP BY clause is required in the column clause
    // Since only GRANTEE is used in the Build methode the other columns are not needed
    Result := 'SELECT ' + Session.EscapeIdentifier('GRANTEE') + ' FROM ' + Session.EscapeIdentifier(INFORMATION_SCHEMA) + '.' + Session.EscapeIdentifier('USER_PRIVILEGES') + ' GROUP BY ' + Session.EscapeIdentifier('GRANTEE') + ';' + #13#10;
  end;
end;

{ TSSession.TEvent ************************************************************}

constructor TSSession.TEvent.Create(const ASession: TSSession);
begin
  inherited Create();

  Session := ASession;
  Sender := nil;
  SItems := nil;
  Update := nil;
end;

{ TSSession *******************************************************************}

function Compare(Item1, Item2: Pointer): Integer;

  function ClassOrder(const Item: TObject): Integer;
  begin
    if (Item is TSVariables) then Result := 0
    else if (Item is TSStati) then Result := 1
    else if (Item is TSEngines) then Result := 2
    else if (Item is TSCharsets) then Result := 3
    else if (Item is TSCollations) then Result := 4
    else if (Item is TSPlugins) then Result := 5
    else if (Item is TSDatabases) then Result := 6
    else if (Item is TSUsers) then Result := 7
    else if (Item is TSTable) then Result := 8
    else if (Item is TSProcedure) then Result := 9
    else if (Item is TSFunction) then Result := 10
    else if (Item is TSTrigger) then Result := 11
    else if (Item is TSEvent) then Result := 12
    else if (Item is TSDatabase) then Result := 13
    else if (Item is TSVariable) then Result := 14
    else if (Item is TSStatus) then Result := 15
    else if (Item is TSEngine) then Result := 16
    else if (Item is TSCharset) then Result := 17
    else if (Item is TSCollation) then Result := 18
    else if (Item is TSPlugin) then Result := 19
    else if (Item is TSProcess) then Result := 20
    else if (Item is TSUser) then Result := 21
    else Result := -1;
  end;

begin
  Result := Sign(ClassOrder(TObject(Item1)) - ClassOrder(TObject(Item2)));
  if ((Result = 0) and (TObject(Item1) is TSDBObject)) then
    Result := Sign(TSDBObject(Item1).Database.Index - TSDBObject(Item2).Database.Index);
  if (Result = 0) then
    Result := Sign(TSObject(Item1).Index - TSObject(Item2).Index);
end;

function TSSession.AddDatabase(const NewDatabase: TSDatabase): Boolean;
begin
  Result := UpdateDatabase(nil, NewDatabase);

  if (Result and (Account.Connection.Database <> '')) then
    Account.Connection.Database := Account.Connection.Database + ',' + CSVEscape(NewDatabase.Name);
end;

function TSSession.AddUser(const ANewUser: TSUser): Boolean;
begin
  Result := UpdateUser(nil, ANewUser);
end;

function TSSession.ApplyIdentifierName(const AIdentifierName: string): string;
begin
  Result := AIdentifierName;

  Result := ReplaceStr(Result, #0, '_');
  if (ServerVersion < 50106) then
  begin
    Result := ReplaceStr(Result, ' ', '_');
    Result := ReplaceStr(Result, '/', '_');
    Result := ReplaceStr(Result, '\', '_');
    Result := ReplaceStr(Result, '.', '_');
  end;
end;

function TSSession.BuildEvents(const DataSet: TMySQLQuery): Boolean;
var
  Database: TSDatabase;
  I: Integer;
begin
  if (not DataSet.IsEmpty()) then
    repeat
      Database := DatabaseByName(DataSet.FindField('EVENT_SCHEMA').AsString);
      if (Assigned(Database)) then
        Database.Events.Build(DataSet, True, False, False)
      else
        DataSet.FindNext();
    until (DataSet.Eof);

  for I := 0 to Databases.Count - 1 do
    if (Assigned(Databases[I].Events)) then
      Databases[I].Events.FValid := True;

  ExecuteEvent(etItemsValid, Self, Databases);

  Result := False;
end;

function TSSession.BuildRoutines(const DataSet: TMySQLQuery): Boolean;
var
  Database: TSDatabase;
  I: Integer;
begin
  if (not DataSet.IsEmpty()) then
    repeat
      Database := DatabaseByName(DataSet.FindField('ROUTINE_SCHEMA').AsString);
      if (Assigned(Database) and Assigned(Database.Routines)) then
        Database.Routines.Build(DataSet, True, False, False)
      else
        DataSet.FindNext();
    until (DataSet.Eof);

  for I := 0 to Databases.Count - 1 do
    if (Assigned(Databases[I].Routines)) then
      Databases[I].Routines.FValid := True;

  ExecuteEvent(etItemsValid, Self, Databases);

  Result := False;
end;

function TSSession.BuildTables(const DataSet: TMySQLQuery): Boolean;
var
  Database: TSDatabase;
  I: Integer;
begin
  if (not DataSet.IsEmpty()) then
    repeat
      Database := DatabaseByName(DataSet.FindField('TABLE_SCHEMA').AsString);
      if (Assigned(Database)) then
        Database.Tables.Build(DataSet, True, False, False)
      else
        DataSet.FindNext();
    until (DataSet.Eof);

  for I := 0 to Databases.Count - 1 do
    Databases[I].Tables.FValid := True;

  ExecuteEvent(etItemsValid, Self, Databases);

  Result := False;
end;

function TSSession.BuildTriggers(const DataSet: TMySQLQuery): Boolean;
var
  Database: TSDatabase;
  I: Integer;
begin
  if (not DataSet.IsEmpty()) then
    repeat
      Database := DatabaseByName(DataSet.FindField('TRIGGER_SCHEMA').AsString);
      if (Assigned(Database)) then
        Database.Triggers.Build(DataSet, True, False, False)
      else
        DataSet.FindNext();
    until (DataSet.Eof);

  for I := 0 to Databases.Count - 1 do
    if (Assigned(Databases[I].Triggers)) then
      Databases[I].Triggers.FValid := True;

  ExecuteEvent(etItemsValid, Self, Databases);

  Result := False;
end;

procedure TSSession.BuildUser(const DataSet: TMySQLQuery);
var
  Index: Integer;
  Name: string;
  Source: string;
begin
  Source :='';
  if (not DataSet.IsEmpty()) then
    repeat
      Source := Source + DataSet.Fields[0].AsString + ';' + #13#10;
    until (not DataSet.FindNext());

  FUser := TSUser.Create(Users);
  if (Source <> '') then
    FUser.SetSource(Source);

  if (FUser.Name <> '') then
    Name := FUser.Name
  else if (FCurrentUser <> '') then
    Name := FCurrentUser
  else
    Name := Username;

  if (not Users.InsertIndex(Name, Index)) then
  begin
    Users[Index].Free();
    Users.Items[Index] := FUser;
  end
  else
    Users.Add(FUser);

  ExecuteEvent(etItemValid, Self, Users, User);
end;

procedure TSSession.ConnectChange(Sender: TObject; Connecting: Boolean);
const
  BufferSize = 10240;
var
  DataSet: TMySQLQuery;
  Equal: Integer;
  I: Integer;
  URL1: string;
  URL2: string;
begin
  if (not Assigned(FEngines) and Connecting) then
  begin
    if (not Assigned(FCollations) and (ServerVersion >= 40100)) then FCollations := TSCollations.Create(Self);
    if (not Assigned(FFieldTypes)) then FFieldTypes := TSFieldTypes.Create(Self);
    if (not Assigned(FEngines)) then FEngines := TSEngines.Create(Self);
    if (not Assigned(FPlugins) and (ServerVersion >= 50105)) then FPlugins := TSPlugins.Create(Self);
    if (not Assigned(FProcesses)) then FProcesses := TSProcesses.Create(Self);
    if (not Assigned(FStati)) then FStati := TSStati.Create(Self);
    if (not Assigned(FUsers)) then FUsers := TSUsers.Create(Self);

    if (Assigned(Account)) then
    begin
      Account.LastLogin := Now();

      if ((ServerVersion > 40100) and not Account.ManualURLFetched) then
      begin
        BeginSilent();

        try
          DataSet := TMySQLQuery.Create(nil);
          DataSet.Connection := Self;
          DataSet.CommandText := 'HELP ' + SQLEscape('SELECT');
          DataSet.Open();
          if (not DataSet.Active or not Assigned(DataSet.FindField('name')) or DataSet.IsEmpty()) then
            URL1 := ''
          else
          begin
            URL1 := DataSet.FieldByName('description').AsString;
            while (Pos('URL:', URL1) > 1) do Delete(URL1, 1, Pos('URL:', URL1) - 1);
            if (Pos('URL:', URL1) = 1) then Delete(URL1, 1, Length('URL:'));
            URL1 := Trim(URL1);
          end;
          DataSet.Free();

          DataSet := TMySQLQuery.Create(nil);
          DataSet.Connection := Self;
          DataSet.CommandText := 'HELP ' + SQLEscape('VERSION');
          DataSet.Open();
          if (not DataSet.Active or not Assigned(DataSet.FindField('name')) or DataSet.IsEmpty()) then
            URL2 := ''
          else
          begin
            URL2 := DataSet.FieldByName('description').AsString;
            while (Pos('URL:', URL2) > 1) do Delete(URL2, 1, Pos('URL:', URL2) - 1);
            if (Pos('URL:', URL2) = 1) then Delete(URL2, 1, Length('URL:'));
            URL2 := Trim(URL2);
          end;
          DataSet.Free();

          EndSilent();

          Equal := 0;
          for I := 1 to Min(Length(URL1), Length(URL2)) - 1 do
            if ((URL1[I] = URL2[I]) and (Equal = I - 1)) then
              Equal := I + 1;

          if (Copy(URL1, 1, 7) = 'http://') then
            Account.ManualURL := Copy(URL1, 1, Equal - 1);
        except
        end;
        Account.ManualURLFetched := True;
      end;
    end;
  end;
end;

function TSSession.CharsetByName(const CharsetName: string): TSCharset;
var
  Index: Integer;
begin
  Index := Charsets.IndexByName(CharsetName);
  if (Index < 0) then
    Result := nil
  else
    Result := Charsets[Index];
end;

function TSSession.CharsetByCollation(const Collation: string): TSCharset;
var
  I: Integer;
begin
  Result := nil;

  if (Assigned(Collations)) then
    for I := 0 to Collations.Count - 1 do
      if (Collations[I].Name = Collation) then
        Result := Collations[I].Charset;
end;

function TSSession.CollationByName(const CollationName: string): TSCollation;
var
  Index: Integer;
begin
  if (not Assigned(Collations)) then
    Index := -1
  else
    Index := Collations.IndexByName(CollationName);
  if (Index < 0) then
    Result := nil
  else
    Result := Collations[Index];
end;

procedure TSSession.CommitTransaction();
begin
  inherited;
  AutoCommit := AutoCommitBeforeTransaction;
end;

constructor TSSession.Create(const ASessions: TSSessions; const AAccount: TAAccount = nil);
begin
  inherited Create(nil);

  FSessions := ASessions;
  Sessions.Add(Self);
  FAccount := AAccount;

  EventProcs := TList.Create();
  FCurrentUser := '';
  FInformationSchema := nil;
  FLowerCaseTableNames := 0;
  FMaxAllowedPacket := 0;
  FMetadataProvider := TacEventMetadataProvider.Create(nil);
  FPerformanceSchema := nil;
  FSyntaxProvider := TacMYSQLSyntaxProvider.Create(nil);
  FSyntaxProvider.ServerVersionInt := ServerVersion;
  FUser := nil;

  if (not Assigned(AAccount)) then
  begin
    FSQLMonitor := nil;
    StmtMonitor := nil;

    FCharsets := TSCharsets.Create(Self);
    FCollations := nil;
    FDatabases :=  TSDatabases.Create(Self);
    FFieldTypes := nil;
    FEngines := nil;
    FInvalidObjects := nil;
    FPlugins := nil;
    FProcesses := nil;
    FStati := nil;
    FUsers := nil;
    FVariables := TSVariables.Create(Self);
  end
  else
  begin
    FSQLMonitor := TMySQLMonitor.Create(nil);
    FSQLMonitor.Connection := Self;
    FSQLMonitor.CacheSize := Preferences.LogSize;
    FSQLMonitor.Enabled := True;
    FSQLMonitor.TraceTypes := [ttRequest];
    if (Preferences.LogResult) then FSQLMonitor.TraceTypes := FSQLMonitor.TraceTypes + [ttInfo];
    if (Preferences.LogTime) then FSQLMonitor.TraceTypes := FSQLMonitor.TraceTypes + [ttTime];
    FSQLMonitor.OnMonitor := MonitorLog;
    StmtMonitor := TMySQLMonitor.Create(nil);
    StmtMonitor.Connection := Self;
    StmtMonitor.Enabled := True;
    StmtMonitor.TraceTypes := [ttResult];
    StmtMonitor.OnMonitor := MonitorExecutedStmts;

    FCharsets := TSCharsets.Create(Self);
    FCollations := nil;
    FDatabases :=  TSDatabases.Create(Self);
    FFieldTypes := nil;
    FEngines := nil;
    FInvalidObjects := TList.Create();
    FPlugins := nil;
    FProcesses := nil;
    FStati := nil;
    FUsers := nil;
    FVariables := TSVariables.Create(Self);
  end;

  RegisterClient(Self, ConnectChange);
end;

function TSSession.DatabaseByName(const DatabaseName: string): TSDatabase;
var
  Index: Integer;
begin
  Index := Databases.IndexByName(DatabaseName);
  if (Index < 0) then
    Result := nil
  else
    Result := Databases[Index];
end;

procedure TSSession.DecodeInterval(const Value: string; const IntervalType: TMySQLIntervalType; var Year, Month, Day, Quarter, Week, Hour, Minute, Second, MSec: Word);
var
  S: string;
begin
  S := UnescapeValue(Value);

  Year := 0;
  Month := 0;
  Day := 0;
  Quarter := 0;
  Week := 0;
  Hour := 0;
  Minute := 0;
  Second := 0;
  MSec := 0;

  case (IntervalType) of
    itYear: Year := StrToInt(S);
    itQuarter: Quarter := StrToInt(S);
    itMonth: Month := StrToInt(S);
    itDay: Day := StrToInt(S);
    itHour: Hour := StrToInt(S);
    itMinute: Minute := StrToInt(S);
    itWeek: Week := StrToInt(S);
    itSecond: Second := StrToInt(S);
    itMicrosecond: MSec := StrToInt(S);
    itYearMonth: begin Year := StrToInt(Copy(S, 1, Pos('-', S) - 1)); Month := StrToInt(Copy(S, Pos('-', S) - 1, Length(S) - Pos('-', S))); end;
    itDayHour: begin Day := StrToInt(Copy(S, 1, Pos(' ', S) - 1)); Hour := StrToInt(Copy(S, Pos(' ', S) - 1, Length(S) - Pos(' ', S))); end;
    itDayMinute: begin Day := StrToInt(Copy(S, 1, Pos(' ', S) - 1)); DecodeTime(StrToTime(Copy(S, Pos(' ', S) - 1, Length(S) - Pos(' ', S)) + ':00'), Hour, Minute, Second, MSec); end;
    itDaySecond: begin Day := StrToInt(Copy(S, 1, Pos(' ', S) - 1)); DecodeTime(StrToTime(Copy(S, Pos(' ', S) - 1, Length(S) - Pos(' ', S))), Hour, Minute, Second, MSec); end;
    itHourMinute: begin Hour := StrToInt(Copy(S, 1, Pos(':', S) - 1)); Minute := StrToInt(Copy(S, Pos(':', S) - 1, Length(S) - Pos(':', S))); end;
    itHourSecond: DecodeTime(StrToTime(S), Hour, Minute, Second, MSec);
    itMinuteSecond: begin Minute := StrToInt(Copy(S, 1, Pos(':', S) - 1)); Second := StrToInt(Copy(S, Pos(':', S) - 1, Length(S) - Pos(':', S))); end;
    itDayMicrosecond: begin Day := StrToInt(Copy(S, 1, Pos(' ', S) - 1)); DecodeTime(StrToTime(Copy(S, Pos(' ', S) - 1, Length(S) - Pos(' ', S))), Hour, Minute, Second, MSec); end;
    itHourMicrosecond: DecodeTime(StrToTime(S), Hour, Minute, Second, MSec);
    itMinuteMicrosecond: DecodeTime(StrToTime('00:' + S), Hour, Minute, Second, MSec);
    itSecondMicrosecond: begin Second := StrToInt(Copy(S, 1, Pos('.', S) - 1)); MSec := StrToInt(Copy(S, Pos('.', S) - 1, Length(S) - Pos('.', S))); end;
  end;
end;

function TSSession.DeleteDatabase(const Database: TSDatabase): Boolean;
var
  List: TList;
begin
  List := TList.Create();
  List.Add(Database);
  Result := DeleteEntities(List);
  List.Free();
end;

function TSSession.DeleteEntities(const List: TList): Boolean;
var
  Database: TSDatabase;
  FlushPrivileges: Boolean;
  I: Integer;
  Identifiers: string;
  SQL: string;
begin
  List.Sort(Compare);

  Database := nil; FlushPrivileges := False;
  SQL := '';

  Identifiers := '';
  for I := 0 to List.Count - 1 do
    if (TSObject(List[I]) is TSBaseTable) then
    begin
      if (not Assigned(Database) or (TSBaseTable(List[I]).Database <> Database) or not Assigned(Database) and (Databases.NameCmp(TSBaseTable(List[I]).Database.Name, DatabaseName) <> 0)) then
      begin
        if (Assigned(Database) or (TSBaseTable(List[I]).Database.Name <> DatabaseName)) then
          SQL := SQL + TSBaseTable(List[I]).Database.SQLUse() + SQL;
        Database := TSBaseTable(List[I]).Database;
        if (Identifiers <> '') then
        begin
          SQL := SQL + 'DROP TABLE ' + Identifiers + ';' + #13#10;
          Identifiers := '';
        end;
      end;
      if (Identifiers <> '') then Identifiers := Identifiers + ',';
      Identifiers := Identifiers + EscapeIdentifier(Database.Name) + '.' + EscapeIdentifier(TSBaseTable(List[I]).Name);
    end;
  if (Identifiers <> '') then
    SQL := SQL + 'DROP TABLE ' + Identifiers + ';' + #13#10;

  Identifiers := '';
  for I := 0 to List.Count - 1 do
    if (TSObject(List[I]) is TSView) then
    begin
      if (not Assigned(Database) or (TSView(List[I]).Database <> Database) or not Assigned(Database) and (Databases.NameCmp(TSView(List[I]).Database.Name, DatabaseName) <> 0)) then
      begin
        if (Assigned(Database) or (TSBaseTable(List[I]).Database.Name <> DatabaseName)) then
          SQL := SQL + TSBaseTable(List[I]).Database.SQLUse() + SQL;
        Database := TSView(List[I]).Database;
        if (Identifiers <> '') then
        begin
          SQL := SQL + 'DROP VIEW ' + Identifiers + ';' + #13#10;
          Identifiers := '';
        end;
      end;
      if (Identifiers <> '') then Identifiers := Identifiers + ',';
      Identifiers := Identifiers + EscapeIdentifier(Database.Name) + '.' + EscapeIdentifier(TSView(List[I]).Name);
    end;
  if (Identifiers <> '') then
    SQL := SQL + 'DROP VIEW ' + Identifiers + ';' + #13#10;

  for I := 0 to List.Count - 1 do
    if ((TObject(List[I]) is TSRoutine) or (TObject(List[I]) is TSTrigger) or (TObject(List[I]) is TSEvent)) then
    begin
      if (not Assigned(Database) or (TSDBObject(List[I]).Database <> Database) or not Assigned(Database) and (Databases.NameCmp(TSDBObject(List[I]).Database.Name, DatabaseName) <> 0)) then
      begin
        if (Assigned(Database) or (TSBaseTable(List[I]).Database.Name <> DatabaseName)) then
          SQL := SQL + TSBaseTable(List[I]).Database.SQLUse() + SQL;
        Database := TSDBObject(List[I]).Database;
      end;
      if (TSObject(List[I]) is TSProcedure) then
        SQL := SQL + 'DROP PROCEDURE ' + EscapeIdentifier(Database.Name) + '.' + EscapeIdentifier(TSObject(List[I]).Name) + ';' + #13#10
      else if (TSObject(List[I]) is TSFunction) then
        SQL := SQL + 'DROP FUNCTION ' + EscapeIdentifier(Database.Name) + '.' + EscapeIdentifier(TSObject(List[I]).Name) + ';' + #13#10
      else if (TSObject(List[I]) is TSTrigger) then
        SQL := SQL + 'DROP TRIGGER ' + EscapeIdentifier(Database.Name) + '.' + EscapeIdentifier(TSObject(List[I]).Name) + ';' + #13#10
      else if (TSObject(List[I]) is TSEvent) then
        SQL := SQL + 'DROP EVENT ' + EscapeIdentifier(Database.Name) + '.' + EscapeIdentifier(TSObject(List[I]).Name) + ';' + #13#10;
    end;

  for I := 0 to List.Count - 1 do
    if (TObject(List[I]) is TSDatabase) then
      SQL := SQL + 'DROP DATABASE ' + EscapeIdentifier(TSDatabase(List[I]).Name) + ';' + #13#10;

  for I := 0 to List.Count - 1 do
    if (TObject(List[I]) is TSProcess) then
      if (ServerVersion < 50000) then
        SQL := SQL + 'KILL ' + IntToStr(TSProcess(List[I]).ThreadId) + ';' + #13#10
      else
        SQL := SQL + 'KILL CONNECTION ' + IntToStr(TSProcess(List[I]).ThreadId) + ';' + #13#10;

  if (FlushPrivileges) then
    SQL := SQL + 'FLUSH PRIVILEGES;' + #13#10;

  Result := ExecuteSQL(SQL);
end;

function TSSession.DeleteProcess(const Process: TSProcess): Boolean;
var
  List: TList;
begin
  List := TList.Create();
  List.Add(Process);
  Result := DeleteEntities(List);
  List.Free();
end;

function TSSession.DeleteUser(const User: TSUser): Boolean;
var
  List: TList;
begin
  List := TList.Create();
  List.Add(User);
  Result := DeleteUsers(List);
  List.Free();
end;

function TSSession.DeleteUsers(const List: TList): Boolean;
var
  I: Integer;
  SQL: string;
  User: TSUser;
begin
  for I := 0 to List.Count - 1 do
  begin
    User := TSUser(List[I]);

    if (ServerVersion < 40101) then
    begin
      SQL := SQL + 'DELETE FROM ' + EscapeIdentifier('mysql') + '.' + EscapeIdentifier('user')         + ' WHERE ' + EscapeIdentifier('User') + '=' + SQLEscape(User.Name) + ';' + #13#10;
      SQL := SQL + 'DELETE FROM ' + EscapeIdentifier('mysql') + '.' + EscapeIdentifier('db')           + ' WHERE ' + EscapeIdentifier('User') + '=' + SQLEscape(User.Name) + ';' + #13#10;
      SQL := SQL + 'DELETE FROM ' + EscapeIdentifier('mysql') + '.' + EscapeIdentifier('tables_priv')  + ' WHERE ' + EscapeIdentifier('User') + '=' + SQLEscape(User.Name) + ';' + #13#10;
      SQL := SQL + 'DELETE FROM ' + EscapeIdentifier('mysql') + '.' + EscapeIdentifier('columns_priv') + ' WHERE ' + EscapeIdentifier('User') + '=' + SQLEscape(User.Name) + ';' + #13#10;
    end
    else
      SQL := SQL + 'DROP USER ' + EscapeUser(User.Name) + ';' + #13#10;
  end;

  if (SQL = '') then
    Result := True
  else
  begin
    SQL := SQL + 'FLUSH PRIVILEGES;' + #13#10;
    Result := (SQL = '') or SendSQL(SQL);
  end;
end;

destructor TSSession.Destroy();
begin
  UnRegisterClient(Self);

  if (Assigned(EventProcs)) then EventProcs.Free();

  if (Assigned(FCharsets)) then FCharsets.Free();
  if (Assigned(FCollations)) then FCollations.Free();
  if (Assigned(FDatabases)) then FDatabases.Free();
  if (Assigned(FEngines)) then FEngines.Free();
  if (Assigned(FFieldTypes)) then FFieldTypes.Free();
  if (Assigned(FInvalidObjects)) then FInvalidObjects.Free();
  if (Assigned(FPlugins)) then FPlugins.Free();
  if (Assigned(FProcesses)) then FProcesses.Free();
  if (Assigned(FStati)) then FStati.Free();
  if (Assigned(FUsers)) then FUsers.Free();
  if (Assigned(FVariables)) then FVariables.Free();

  if (Assigned(FSQLMonitor)) then
    FSQLMonitor.Free();
  if (Assigned(StmtMonitor)) then
    StmtMonitor.Free();

  Sessions.Delete(Sessions.IndexOf(Self));

  FMetadataProvider.Free();
  FSyntaxProvider.Free();

  inherited;
end;

procedure TSSession.DoAfterExecuteSQL();
begin
  inherited;

  ExecuteEvent(etAfterExecuteSQL);
end;

procedure TSSession.DoBeforeExecuteSQL();
begin
  ExecuteEvent(etBeforeExecuteSQL);

  inherited;
end;

procedure TSSession.DoExecuteEvent(const AEvent: TEvent);
var
  I: Integer;
  EventProc: TEventProc;
begin
  for I := 0 to EventProcs.Count - 1 do
  begin
    MoveMemory(@TMethod(EventProc), EventProcs[I], SizeOf(TMethod));
    EventProc(AEvent);
  end;
end;

procedure TSSession.EmptyDatabases(const Databases: TList);
var
  I: Integer;
begin
  for I := 0 to Databases.Count - 1 do
    TSDatabase(Databases[I]).EmptyTables();
end;

function TSSession.EncodeInterval(const Year, Month, Day, Quarter, Week, Hour, Minute, Second, MSec: Word; var Value: string; var IntervalType: TMySQLIntervalType): Boolean;
begin
  if ((Year <> 0) and (Month = 0) and (Day = 0) and (Quarter = 0) and (Week = 0) and (Hour = 0) and (Minute = 0) and (Second = 0) and (MSec = 0)) then
    begin Value := IntToStr(Year); IntervalType := itYear; end
  else if ((Year = 0) and (Month = 0) and (Day = 0) and (Quarter <> 0) and (Week = 0) and (Hour = 0) and (Minute = 0) and (Second = 0) and (MSec = 0)) then
    begin Value := IntToStr(Quarter); IntervalType := itQuarter; end
  else if ((Year = 0) and (Month <> 0) and (Day = 0) and (Quarter = 0) and (Week = 0) and (Hour = 0) and (Minute = 0) and (Second = 0) and (MSec = 0)) then
    begin Value := IntToStr(Month); IntervalType := itMonth; end
  else if ((Year = 0) and (Month = 0) and (Day <> 0) and (Quarter = 0) and (Week = 0) and (Hour = 0) and (Minute = 0) and (Second = 0) and (MSec = 0)) then
    begin Value := IntToStr(Day); IntervalType := itDay; end
  else if ((Year = 0) and (Month = 0) and (Day = 0) and (Quarter = 0) and (Week = 0) and (Hour <> 0) and (Minute = 0) and (Second = 0) and (MSec = 0)) then
    begin Value := IntToStr(Hour); IntervalType := itHour; end
  else if ((Year = 0) and (Month = 0) and (Day = 0) and (Quarter = 0) and (Week = 0) and (Hour = 0) and (Minute <> 0) and (Second = 0) and (MSec = 0)) then
    begin Value := IntToStr(Minute); IntervalType := itMinute; end
  else if ((Year = 0) and (Month = 0) and (Day = 0) and (Quarter = 0) and (Week <> 0) and (Hour = 0) and (Minute = 0) and (Second = 0) and (MSec = 0)) then
    begin Value := IntToStr(Week); IntervalType := itWeek; end
  else if ((Year = 0) and (Month = 0) and (Day = 0) and (Quarter = 0) and (Week = 0) and (Hour = 0) and (Minute = 0) and (Second <> 0) and (MSec = 0)) then
    begin Value := IntToStr(Second); IntervalType := itSecond; end
  else if ((Year = 0) and (Month = 0) and (Day = 0) and (Quarter = 0) and (Week = 0) and (Hour = 0) and (Minute = 0) and (Second = 0) and (MSec <> 0)) then
    begin Value := IntToStr(MSec); IntervalType := itMicrosecond; end
  else if ((Year <> 0) and (Month <> 0) and (Day = 0) and (Quarter = 0) and (Week = 0) and (Hour = 0) and (Minute = 0) and (Second = 0) and (MSec = 0)) then
    begin Value := SQLEscape(IntToStr(Year) + '-' + IntToStr(Month)); IntervalType := itYearMonth; end
  else if ((Year = 0) and (Month = 0) and (Day <> 0) and (Quarter = 0) and (Week = 0) and (Hour <> 0) and (Minute = 0) and (Second = 0) and (MSec = 0)) then
    begin Value := SQLEscape(IntToStr(Day) + ' ' + IntToStr(Hour)); IntervalType := itDayHour; end
  else if ((Year = 0) and (Month = 0) and (Day <> 0) and (Quarter = 0) and (Week = 0)                 and (Minute <> 0) and (Second = 0) and (MSec = 0)) then
    begin Value := SQLEscape(IntToStr(Day) + ' ' + IntToStr(Hour) + ':' + IntToStr(Minute)); IntervalType := itDayMinute; end
  else if ((Year = 0) and (Month = 0) and (Day <> 0) and (Quarter = 0) and (Week = 0)                                 and (Second <> 0) and (MSec = 0)) then
    begin Value := SQLEscape(IntToStr(Day) + ' ' + IntToStr(Hour) + ':' + IntToStr(Minute) + ':' + IntToStr(Second)); IntervalType := itDaySecond; end
  else if ((Year = 0) and (Month = 0) and (Day = 0) and (Quarter = 0) and (Week = 0) and (Hour <> 0) and (Minute <> 0) and (Second = 0) and (MSec = 0)) then
    begin Value := SQLEscape(IntToStr(Hour) + ':' + IntToStr(Minute)); IntervalType := itHourMinute; end
  else if ((Year = 0) and (Month = 0) and (Day = 0) and (Quarter = 0) and (Week = 0) and (Hour <> 0)                  and (Second <> 0) and (MSec = 0)) then
    begin Value := SQLEscape(IntToStr(Hour) + ':' + IntToStr(Minute) + ':' + IntToStr(Second)); IntervalType := itHourSecond; end
  else if ((Year = 0) and (Month = 0) and (Day = 0) and (Quarter = 0) and (Week = 0) and (Hour = 0) and (Minute <> 0) and (Second <> 0) and (MSec = 0)) then
    begin Value := SQLEscape(IntToStr(Minute) + ':' + IntToStr(Second)); IntervalType := itMinuteSecond; end
  else if ((Year = 0) and (Month = 0) and (Day <> 0)                                                                                   and (MSec <> 0)) then
    begin Value := SQLEscape(IntToStr(Day) + ' ' + IntToStr(Hour) + ':' + IntToStr(Minute) + IntToStr(Second) + '.' + IntToStr(MSec)); IntervalType := itDayMicrosecond; end
  else if ((Year = 0) and (Month = 0) and (Day = 0) and (Quarter = 0) and (Week = 0) and (Hour <> 0)                                   and (MSec <> 0)) then
    begin Value := SQLEscape(IntToStr(Hour) + ':' + IntToStr(Minute) + IntToStr(Second) + '.' + IntToStr(MSec)); IntervalType := itHourMicrosecond; end
  else if ((Year = 0) and (Month = 0) and (Day = 0) and (Quarter = 0) and (Week = 0) and (Hour = 0) and (Minute <> 0)                  and (MSec <> 0)) then
    begin Value := SQLEscape(IntToStr(Minute) + IntToStr(Second) + '.' + IntToStr(MSec)); IntervalType := itMinuteMicrosecond; end
  else if ((Year = 0) and (Month = 0) and (Day = 0) and (Quarter = 0) and (Week = 0) and (Hour = 0) and (Minute = 0) and (Second <> 0) and (MSec <> 0)) then
    begin Value := SQLEscape(IntToStr(Hour) + ':' + IntToStr(Minute)); IntervalType := itSecondMicrosecond; end
  else
    begin Value := ''; IntervalType := itUnknown; end;

  Result := IntervalType <> itUnknown;
end;

function TSSession.EngineByName(const EngineName: string): TSEngine;
var
  Index: Integer;
begin
  Index := Engines.IndexByName(EngineName);
  if (Index < 0) then
    Result := nil
  else
    Result := Engines[Index];
end;

function TSSession.EscapeRightIdentifier(const Identifier: string; const IdentifierQuoting: Boolean = False): string;
begin
  if (IdentifierQuoting) then
    Result := SQLEscape(Identifier)
  else
    Result := EscapeIdentifier(Identifier);
end;

function TSSession.EscapeUser(const User: string; const IdentifierQuoting: Boolean = False): string;
var
  Count: Integer;
  Host: string;
  I: Integer;
  Username: string;
begin
  Count := Length(User);
  for I := 1 to Length(User) do
    if (User[I] = '@') then
      Count := I - 1;
  Username := Copy(User, 1, Count);
  if (Count < Length(User)) then
    Host := Copy(User, Count + 2, Length(User) - Count)
  else
    Host := '';

  if (IdentifierQuoting) then
    if (Host = '') then
      Result := SQLEscape(Username)
    else
      Result := SQLEscape(Username) + '@' + SQLEscape(Host)
  else
    if (Host = '') then
      Result := EscapeIdentifier(Username)
    else
      Result := EscapeIdentifier(Username) + '@' + EscapeIdentifier(Host);
end;

procedure TSSession.ExecuteEvent(const EventType: TEventType);
var
  Event: TEvent;
begin
  Event := TEvent.Create(Self);
  Event.EventType := EventType;
  DoExecuteEvent(Event);
  Event.Free();
end;

procedure TSSession.ExecuteEvent(const EventType: TEventType; const Sender: TObject; const SItems: TSItems = nil; const SItem: TSItem = nil);
var
  Event: TEvent;
begin
  Event := TEvent.Create(Self);
  Event.EventType := EventType;
  Event.Sender := Sender;
  Event.SItems := SItems;
  Event.SItem := SItem;
  DoExecuteEvent(Event);
  Event.Free();
end;

function TSSession.FieldTypeByCaption(const Caption: string): TSFieldType;
var
  I: Integer;
begin
  Result := nil;

  for I := 0 to FieldTypes.Count - 1 do
    if (lstrcmpi(PChar(FieldTypes[I].Caption), PChar(Caption)) = 0) then
      Result := FieldTypes[I];

  if (not Assigned(Result)) then
    if ((UpperCase(Caption) = 'BOOL') or (UpperCase(Caption) = 'BOOLEAN')) then
      Result := FieldTypeByMySQLFieldType(mfTinyInt)
    else if (UpperCase(Caption) = 'INT4') then
      Result := FieldTypeByMySQLFieldType(mfMediumInt)
    else if (UpperCase(Caption) = 'INTEGER') then
      Result := FieldTypeByMySQLFieldType(mfInt)
    else if (UpperCase(Caption) = 'LONG') then
      Result := FieldTypeByMySQLFieldType(mfInt)
    else if (UpperCase(Caption) = 'DEC') then
      Result := FieldTypeByMySQLFieldType(mfDecimal)
    else if (UpperCase(Caption) = 'NVARCHAR') then
      Result := FieldTypeByMySQLFieldType(mfVarChar);

  if (not Assigned(Result)) then
    raise Exception.CreateFMT(SUnknownFieldType, [Caption]);
end;

function TSSession.FieldTypeByMySQLFieldType(const MySQLFieldType: TMySQLFieldType): TSFieldType;
var
  I: Integer;
begin
  Result := nil;

  for I := 0 to FieldTypes.Count - 1 do
    if (FieldTypes[I].MySQLFieldType = MySQLFieldType) then
      Result := FieldTypes[I];
end;

procedure TSSession.FirstConnect();
begin
  Connected := False;

  Asynchron := True;
  FDatabaseName := Account.GetDefaultDatabase();
  case (Account.Connection.LibraryType) of
    ltBuiltIn: LibraryName := '';
    ltDLL: LibraryName := Account.Connection.LibraryFilename;
    ltHTTP: LibraryName := Account.Connection.HTTPTunnelURI;
    ltNamedPipe: LibraryName := Account.Connection.PipeName;
  end;
  Host := Account.Connection.Host;
  HTTPAgent := Preferences.InternetAgent;
  LibraryType := Account.Connection.LibraryType;
  LoginPrompt := False;
  OnUpdateIndexDefs := UpdateIndexDefs;
  Password := Account.Connection.Password;
  Port := Account.Connection.Port;
  Username := Account.Connection.Username;

  try
    Open();
  except
    on E: EMySQLError do
      DoError(E.ErrorCode, E.Message);
  end;
end;

procedure TSSession.FirstConnect(const AConnectionType: Integer; const ALibraryName: string; const AHost, AUser, APassword, ADatabase: string; const APort: Integer; const AAsynchron: Boolean);
begin
  Close();

  FDatabaseName := ADatabase;
  Host := AHost;
  HTTPAgent := Preferences.InternetAgent;
  LibraryName := ALibraryName;
  case (AConnectionType) of
    0: LibraryType := ltBuiltIn;
    1: LibraryType := ltDLL;
    2: LibraryType := ltHTTP;
  end;
  LoginPrompt := False;
  FMultiStatements := True;
  Password := APassword;
  Port := APort;
  Username := AUser;

  Open();
  Asynchron := AAsynchron;
end;

function TSSession.FlushHosts(): Boolean;
begin
  Result := ExecuteSQL('FLUSH HOSTS;');
end;

function TSSession.GetAutoCommit(): Boolean;
begin
  if (Assigned(Lib.mysql_get_server_status) or not Variables.Valid or not Assigned(VariableByName('autocommit'))) then
    Result := inherited GetAutoCommit()
  else
    Result := VariableByName('autocommit').AsBoolean;
end;

function TSSession.GetCaption(): string;
begin
  Result := Host;
  if (Port <> MYSQL_PORT) then
    Result := Result + ':' + IntToStr(Port);
end;

function TSSession.GetCollation(): string;
begin
  if (Assigned(VariableByName('collation_server'))) then
    Result := VariableByName('collation_server').Value
  else
    Result := '';
end;

function TSSession.GetDefaultCharset(): string;
begin
  if (Assigned(VariableByName('character_set'))) then
    Result := VariableByName('character_set').Value
  else if (Assigned(VariableByName('character_set_server'))) then
    Result := VariableByName('character_set_server').Value
  else
    Result := 'latin1';
end;

function TSSession.GetDataFileAllowed(): Boolean;
begin
  Result := inherited GetDataFileAllowed() and ((ServerVersion < 40003) or VariableByName('local_infile').AsBoolean);
end;

function TSSession.GetLogActive(): Boolean;
begin
  Result := (ServerVersion >= 50111) and Assigned(VariableByName('log')) and VariableByName('log').AsBoolean;
end;

function TSSession.GetSlowLogActive(): Boolean;
begin
  Result := (ServerVersion >= 50111) and Assigned(VariableByName('log_slow_queries')) and VariableByName('log_slow_queries').AsBoolean;
end;

function TSSession.GetSlowLog(): string;
begin
  Result := GetSlowSQLLog();
end;

function TSSession.GetSlowSQLLog(const User: TSUser = nil): string;
var
  DataSet: TMySQLQuery;
  Hour: Word;
  Min: Word;
  MSec: Word;
  Sec: Word;
  Seconds: Integer;
  SQL: string;
begin
  Result := '';

  SQL := 'SELECT ' + EscapeIdentifier('start_time') + ',' + EscapeIdentifier('query_time') + ',';
  if (not Assigned(User)) then
    SQL := SQL + EscapeIdentifier('user_host') + ',';
  SQL := SQL + EscapeIdentifier('sql_text');
  SQL := SQL + ' FROM ' + EscapeIdentifier('mysql') + '.' + EscapeIdentifier('slow_log');
  if (Assigned(User)) then
    SQL := SQL + ' WHERE ' + EscapeIdentifier('user_host') + ' LIKE ' + EscapeRightIdentifier(User.Name + '@%');
  SQL := SQL + ' ORDER BY ' + EscapeIdentifier('start_time') + ' DESC';
  SQL := SQL + ' LIMIT 100';

  DataSet := TMySQLQuery.Create(nil);
  DataSet.Connection := Self;
  DataSet.CommandText := SQL;
  DataSet.Open();
  if (DataSet.Active and not DataSet.IsEmpty()) then
    repeat
      DecodeTime(StrToTime(DataSet.FieldByName('query_time').AsString), Hour, Min, Sec, MSec);
      Seconds := SecsPerMin * MinsPerHour * Hour + SecsPerMin * Min + Sec;

      SQL := DataSet.FieldByName('sql_text').AsString;
      if (Copy(SQL, 1, Length(SQL))) <> ';' then
        Result := SQL + ';' + #13#10 + Result
      else
        Result := SQL + #13#10 + Result;
      if (not Assigned(User)) then
        Result := '# ' + MySQLDB.DateTimeToStr(DataSet.FieldByName('start_time').AsDateTime, FormatSettings) + ', ' + Preferences.LoadStr(829) + ': ' + IntToStr(Seconds) + ', ' + Preferences.LoadStr(561) + ': ' + Copy(DataSet.FieldByName('user_host').AsString, 1, Pos('[', DataSet.FieldByName('user_host').AsString) - 1) + #13#10 + Result
      else
        Result := '# ' + MySQLDB.DateTimeToStr(DataSet.FieldByName('start_time').AsDateTime, FormatSettings) + ', ' + Preferences.LoadStr(829) + ': ' + IntToStr(Seconds) + #13#10 + Result;
    until (not DataSet.FindNext());
  DataSet.Free();
end;

function TSSession.GetLog(): string;
begin
  Result := GetSQLLog();
end;

function TSSession.GetMaxAllowedPacket(): Integer;
begin
  if (FMaxAllowedPacket = 0) then
    Result := inherited
  else
    Result := FMaxAllowedPacket;
end;

function TSSession.GetSQLLog(const User: TSUser = nil): string;
var
  DataSet: TMySQLQuery;
  SQL: string;
begin
  Result := '';

  SQL := 'SELECT ' + EscapeIdentifier('event_time') + ',';
  if (not Assigned(User)) then
    SQL := SQL + EscapeIdentifier('user_host') + ',';
  SQL := SQL + EscapeIdentifier('argument');
  SQL := SQL + ' FROM ' + EscapeIdentifier('mysql') + '.' + EscapeIdentifier('general_log');
  SQL := SQL + ' WHERE UPPER(' + EscapeIdentifier('command_type') + ')=''SQL''';
  if (Assigned(User)) then
    SQL := SQL + ' AND' + EscapeIdentifier('user_host') + ' LIKE ' + EscapeRightIdentifier(User.Name + '@%');
  SQL := SQL + ' ORDER BY ' + EscapeIdentifier('event_time') + ' DESC';
  SQL := SQL + ' LIMIT 100';

  DataSet := TMySQLQuery.Create(nil);
  DataSet.Connection := Self;
  DataSet.CommandText := SQL;
  if (DataSet.Active and not DataSet.IsEmpty()) then
    repeat
      SQL := DataSet.FieldByName('argument').AsString;
      if (Copy(SQL, 1, Length(SQL))) <> ';' then
        Result := SQL + ';' + #13#10 + Result
      else
        Result := SQL + #13#10 + Result;
      if (not Assigned(User)) then
        Result := '# ' + MySQLDB.DateTimeToStr(DataSet.FieldByName('event_time').AsDateTime, FormatSettings) + ', ' + Preferences.LoadStr(561) + ': ' + Copy(DataSet.FieldByName('user_host').AsString, 1, Pos('[', DataSet.FieldByName('user_host').AsString) - 1) + #13#10 + Result
      else if (Assigned(SQLMonitor) and (ttTime in SQLMonitor.TraceTypes)) then
        Result := '# ' + MySQLDB.DateTimeToStr(DataSet.FieldByName('event_time').AsDateTime, FormatSettings) + #13#10 + Result;
    until (not DataSet.FindNext());
  DataSet.Free();
end;

function TSSession.GetUserRights(): TSUserRight;
begin
  if (not Assigned(User) or (User.RightCount = 0)) then
    Result := nil
  else
    Result := User.Right[0];
end;

function TSSession.GetValid(): Boolean;
var
  I: Integer;
begin
  Result := (FCurrentUser <> '') and Assigned(FUser)
    and Variables.Valid and Stati.Valid and Engines.Valid and Charsets.Valid and (not Assigned(Collations) or Collations.Valid) and Users.Valid
    and Databases.Valid;
  for I := 0 to Databases.Count - 1 do
    Result := Result and (Databases[I].Valid);
end;

procedure TSSession.GridCanEditShow(Sender: TObject);
var
  Database: TSDatabase;
  Field: TSTableField;
  FieldInfo: TFieldInfo;
  Grid: TMySQLDBGrid;
  I: Integer;
  J: Integer;
  Table: TSTable;
begin
  if ((Sender is TMySQLDBGrid) and (TMySQLDBGrid(Sender).DataSource.DataSet is TMySQLDataSet)) then
  begin
    Grid := TMySQLDBGrid(Sender);

    for I := 0 to Grid.Columns.Count - 1 do
    begin
      if (GetFieldInfo(Grid.Columns[I].Field.Origin, FieldInfo)) then
      begin
        Database := DatabaseByName(FieldInfo.DatabaseName);
        if (Assigned(Database)) then
        begin
          Table := Database.TableByName(FieldInfo.TableName);
          if (Assigned(Table)) then
          begin
            Field := Table.FieldByName(FieldInfo.OriginalFieldName);
            if (Assigned(Field)) then
            begin
              Grid.Columns[I].PickList.Clear();
              if (Field.FieldType = mfEnum) then
                for J := 0 to Length(Field.Items) - 1 do
                  Grid.Columns[I].PickList.Add(Field.Items[J]);

              Grid.Columns[I].ButtonStyle := cbsAuto;
            end;
          end;
        end;
      end;
    end;
  end;
end;

procedure TSSession.Invalidate();
begin
  if (Assigned(Variables)) then Variables.Invalidate();
  if (Assigned(Stati)) then Stati.Invalidate();
  if (Assigned(Engines)) then Engines.Invalidate();
  if (Assigned(Charsets)) then Charsets.Invalidate();
  if (Assigned(Collations)) then Collations.Invalidate();
  if (Assigned(Databases)) then Databases.Invalidate();
  if (Assigned(Plugins)) then Plugins.Invalidate();
  if (Assigned(Users)) then Users.Invalidate();
end;

procedure TSSession.MonitorLog(const Sender: TObject; const Text: PChar; const Len: Integer; const ATraceType: TMySQLMonitor.TTraceType);
begin
  ExecuteEvent(etMonitor);
end;

procedure TSSession.MonitorExecutedStmts(const Sender: TObject; const Text: PChar; const Len: Integer; const ATraceType: TMySQLMonitor.TTraceType);
var
  Database: TSDatabase;
  DatabaseName: string;
  DDLStmt: TSQLDDLStmt;
  DMLStmt: TSQLDMLStmt;
  Event: TSEvent;
  NextDDLStmt: TSQLDDLStmt;
  NextSQL: string;
  ObjectName: string;
  OldObjectName: string;
  Parse: TSQLParse;
  Process: TSProcess;
  Routine: TSRoutine;
  Table: TSTable;
  Trigger: TSTrigger;
  User: TSUser;
  Variable: TSVariable;
begin
  if (SQLCreateParse(Parse, Text, Len, ServerVersion)) then
    if (SQLParseKeyword(Parse, 'SELECT') or SQLParseKeyword(Parse, 'SHOW')) then
      // Do nothing - but do not parse the Text further more
    else if (SQLParseDDLStmt(DDLStmt, Text, Len, ServerVersion)) then
    begin
      DDLStmt.DatabaseName := TableName(DDLStmt.DatabaseName);
      if (DDLStmt.ObjectType = otTable) then
        DDLStmt.ObjectName := TableName(DDLStmt.ObjectName);

      if (DDLStmt.ObjectType = otDatabase) then
        case (DDLStmt.DefinitionType) of
          dtCreate:
            if (Assigned(DatabaseByName(DDLStmt.ObjectName))) then
              ExecuteEvent(etItemAltered, Self, Databases, DatabaseByName(DDLStmt.ObjectName))
            else
              Databases.Add(TSDatabase.Create(Self, DDLStmt.ObjectName), True);
          dtRename,
          dtAlter:
            begin
              Database := DatabaseByName(DDLStmt.ObjectName);
              if (Assigned(Database) and Database.Valid or Database.ValidSource or Database.ValidSources) then
              begin
                Database.Invalidate();
                ExecuteEvent(etItemAltered, Self, Databases, Database);
              end;
            end;
          dtDrop:
            begin
              Database := DatabaseByName(DDLStmt.ObjectName);
              if (Assigned(Database)) then
                Databases.Delete(Database);
            end;
        end
      else
      begin
        if (DDLStmt.DatabaseName = '') then
          DatabaseName := Self.DatabaseName
        else
          DatabaseName := DDLStmt.DatabaseName;
        Database := DatabaseByName(DatabaseName);
        if (Assigned(Database)) then
          case (DDLStmt.ObjectType) of
            otTable,
            otView:
              case (DDLStmt.DefinitionType) of
                dtCreate:
                  if (Assigned(Database.TableByName(DDLStmt.ObjectName))) then
                    ExecuteEvent(etItemAltered, Database, Database.Tables, Database.TableByName(DDLStmt.ObjectName))
                  else if (DDLStmt.ObjectType = otTable) then
                    Database.Tables.Add(TSBaseTable.Create(Database.Tables, DDLStmt.ObjectName), True)
                  else
                    Database.Tables.Add(TSView.Create(Database.Tables, DDLStmt.ObjectName), True);
                dtRename:
                  if (SQLParseKeyword(Parse, 'RENAME')
                    and (SQLParseKeyword(Parse, 'TABLE') or SQLParseKeyword(Parse, 'VIEW'))) then
                  repeat
                    DatabaseName := Self.DatabaseName;
                    if (SQLParseObjectName(Parse, DatabaseName, ObjectName)) then
                    begin
                      Database := DatabaseByName(DatabaseName);
                      DDLStmt.NewDatabaseName := DatabaseName;
                      if (SQLParseKeyword(Parse, 'TO') and SQLParseObjectName(Parse, DDLStmt.NewDatabaseName, DDLStmt.NewObjectName)) then
                      begin
                        Table := Database.TableByName(ObjectName);
                        if (Assigned(Table)) then
                        begin
                          if (DDLStmt.NewDatabaseName <> DatabaseName) then
                            ExecuteEvent(etItemDropped, Table.Database, Table.Tables, Table);
                          Table.SetDatabase(DatabaseByName(DDLStmt.NewDatabaseName));
                          Table.Name := DDLStmt.NewObjectName;
                          if (DDLStmt.NewDatabaseName <> DatabaseName) then
                            ExecuteEvent(etItemDropped, Table.Database, Table.Tables, Table)
                          else
                            ExecuteEvent(etItemAltered, Database, Database.Tables, Table);
                        end;
                      end;
                    end;
                  until (not SQLParseChar(Parse, ','));
                dtAlter,
                dtAlterRename:
                  begin
                    Table := Database.TableByName(DDLStmt.ObjectName);
                    if (Assigned(Table)) then
                    begin
                      if (DDLStmt.DefinitionType = dtAlterRename) then
                      begin
                        if (Databases.NameCmp(DDLStmt.NewDatabaseName, Database.Name) <> 0) then
                          ExecuteEvent(etItemDropped, Table.Database, Table.Tables, Table);
                        Table.SetDatabase(Database);
                        Table.Name := DDLStmt.NewObjectName;
                        if (Databases.NameCmp(DDLStmt.NewDatabaseName, Database.Name) <> 0) then
                          ExecuteEvent(etItemDropped, Table.Database, Table.Tables, Table)
                        else
                          ExecuteEvent(etItemAltered, Database, Database.Tables, Table);
                      end;

                      if ((Table.Database <> Database) and Table.Database.Valid) then
                      begin
                        Table.Database.Invalidate();
                        ExecuteEvent(etItemAltered, Database, Database.Tables, Table);
                      end
                      else if (Table.ValidSource
                        or (Table is TSBaseTable) and TSBaseTable(Table).ValidStatus
                        or (Table is TSView) and TSView(Table).Valid) then
                      begin
                        Table.Invalidate();
                        ExecuteEvent(etItemAltered, Database, Database.Tables, Table);
                      end;
                    end;
                  end;
                dtDrop:
                  if (SQLParseKeyword(Parse, 'DROP')
                    and (SQLParseKeyword(Parse, 'TABLE') or SQLParseKeyword(Parse, 'VIEW'))) then
                  begin
                    SQLParseKeyword(Parse, 'IF EXISTS');
                    repeat
                      DatabaseName := Self.DatabaseName;
                      if (SQLParseObjectName(Parse, DatabaseName, ObjectName)) then
                      begin
                        Database := DatabaseByName(DatabaseName);
                        Table := Database.TableByName(ObjectName);
                        if (Assigned(Table)) then
                          Database.Tables.Delete(Table);
                      end;
                    until (not SQLParseChar(Parse, ','));
                  end;
              end;
            otFunction,
            otProcedure:
              case (DDLStmt.DefinitionType) of
                dtCreate:
                  begin
                    if (DDLStmt.ObjectType = otProcedure) then
                      Routine := Database.ProcedureByName(DDLStmt.ObjectName)
                    else
                      Routine := Database.FunctionByName(DDLStmt.ObjectName);
                    if (Assigned(Routine)) then
                      Routine.SetSource(Text)
                    else
                      if (DDLStmt.ObjectType = otProcedure) then
                        Database.Routines.Add(TSProcedure.Create(Database.Routines, DDLStmt.ObjectName), True)
                      else
                        Database.Routines.Add(TSFunction.Create(Database.Routines, DDLStmt.ObjectName), True);
                  end;
                dtAlter,
                dtAlterRename:
                  begin
                    if (DDLStmt.ObjectType = otProcedure) then
                      Routine := Database.ProcedureByName(DDLStmt.ObjectName)
                    else
                      Routine := Database.FunctionByName(DDLStmt.ObjectName);
                    if (Routine.ValidSource) then
                    begin
                      Routine.Invalidate();
                      ExecuteEvent(etItemAltered, Database, Database.Routines, Routine);
                    end;
                  end;
                dtDrop:
                  begin
                    NextSQL := NextCommandText();
                    if (SQLParseDDLStmt(NextDDLStmt, PChar(NextSQL), Length(NextSQL), ServerVersion)
                      and (NextDDLStmt.ObjectType = DDLStmt.ObjectType)
                      and ((NextDDLStmt.DatabaseName = DDLStmt.DatabaseName) or (NextDDLStmt.DatabaseName = ''))
                      and (NextDDLStmt.ObjectName = DDLStmt.ObjectName)) then
                      // will be handled as etItemAltered within the next Stmt
                    else
                    begin
                      if (DDLStmt.ObjectType = otProcedure) then
                        Routine := Database.ProcedureByName(DDLStmt.ObjectName)
                      else
                        Routine := Database.FunctionByName(DDLStmt.ObjectName);
                      if (Assigned(Routine)) then
                        Database.Routines.Delete(Routine);
                    end;
                  end;
              end;
            otTrigger:
              case (DDLStmt.DefinitionType) of
                dtCreate:
                  if (Assigned(Database.TriggerByName(DDLStmt.ObjectName))) then
                    Database.TriggerByName(DDLStmt.ObjectName).SetSource(Text)
                  else
                  begin
                    Trigger := TSTrigger.Create(Database.Triggers, DDLStmt.ObjectName);
                    if (SQLParseKeyword(Parse, 'CREATE')) then
                    begin
                      if (SQLParseKeyword(Parse, 'DEFINER') and SQLParseChar(Parse, '=')) then
                        Trigger.FDefiner := SQLParseValue(Parse);
                      if (SQLParseKeyword(Parse, 'TRIGGER')) then
                        SQLParseObjectName(Parse, DatabaseName, ObjectName);
                      if (SQLParseKeyword(Parse, 'BEFORE')) then
                        Trigger.FTiming := ttBefore
                      else if (SQLParseKeyword(Parse, 'AFTER')) then
                        Trigger.FTiming := ttAfter;
                      if (SQLParseKeyword(Parse, 'INSERT')) then
                        Trigger.FEvent := teInsert
                      else if (SQLParseKeyword(Parse, 'UPDATE')) then
                        Trigger.FEvent := teUpdate
                      else if (SQLParseKeyword(Parse, 'DELETE')) then
                        Trigger.FEvent := teDelete;
                      if (SQLParseKeyword(Parse, 'ON')) then
                        SQLParseObjectName(Parse, DatabaseName, Trigger.FTableName);
                      if (SQLParseKeyword(Parse, 'FOR EACH ROW')) then
                        Trigger.FStmt := SQLParseRest(Parse);
                    end;
                    Database.Triggers.Add(Trigger, True);
                  end;
                dtDrop:
                  begin
                    NextSQL := NextCommandText();
                    if (SQLParseDDLStmt(NextDDLStmt, PChar(NextSQL), Length(NextSQL), ServerVersion)
                      and (NextDDLStmt.ObjectType = DDLStmt.ObjectType)
                      and ((NextDDLStmt.DatabaseName = DDLStmt.DatabaseName) or (NextDDLStmt.DatabaseName = ''))
                      and (NextDDLStmt.ObjectName = DDLStmt.ObjectName)) then
                      // will be handled as etItemAltered within the next Stmt
                    else
                      Database.Triggers.Delete(Database.TriggerByName(DDLStmt.ObjectName));
                  end;
              end;
            otEvent:
              case (DDLStmt.DefinitionType) of
                dtCreate:
                  if (Assigned(Database.EventByName(DDLStmt.ObjectName))) then
                    Database.EventByName(DDLStmt.ObjectName).SetSource(Text)
                  else
                    Database.Events.Add(TSEvent.Create(Database.Events, DDLStmt.ObjectName), True);
                dtAlter,
                dtAlterRename:
                  begin
                    Event := Database.EventByName(DDLStmt.ObjectName);
                    if (DDLStmt.DefinitionType = dtAlterRename) then
                    begin
                      if (Databases.NameCmp(DDLStmt.NewDatabaseName, Database.Name) <> 0) then
                        ExecuteEvent(etItemDropped, Event.Database, Event.Events, Event);
                      Event.SetDatabase(DatabaseByName(DDLStmt.NewDatabaseName));
                      Event.Name := DDLStmt.NewObjectName;
                      if (Databases.NameCmp(DDLStmt.NewDatabaseName, Database.Name) <> 0) then
                        ExecuteEvent(etItemDropped, Event.Database, Event.Events, Event)
                      else
                        ExecuteEvent(etItemAltered, Database, Database.Events, Event);
                    end;

                    if ((Event.Database <> Database) and Event.Database.Valid) then
                    begin
                      Event.Database.Invalidate();
                      ExecuteEvent(etItemAltered, Database, Database.Events, Event);
                    end
                    else if (Event.ValidSource) then
                    begin
                      Event.Invalidate();
                      ExecuteEvent(etItemAltered, Database, Database.Events, Event);
                    end;
                  end;
                dtDrop:
                  Database.Events.Delete(Database.EventByName(DDLStmt.ObjectName));
              end;
        end;
      end;
    end
    else if (SQLParseDMLStmt(DMLStmt, Text, Len, ServerVersion)) then
    begin
      if ((Length(DMLStmt.DatabaseNames) = 1) and (Length(DMLStmt.TableNames) = 1)
        and (TableNameCmp(DMLStmt.DatabaseNames[0], 'mysql') = 0) and (TableNameCmp(DMLStmt.TableNames[0], 'user') = 0)) then
        Users.Invalidate();
      if ((DMLStmt.ManipulationType = mtDelete) and SQLParseKeyword(Parse, 'DELETE FROM')) then
      begin
        DatabaseName := Self.DatabaseName;
        if (SQLParseObjectName(Parse, DatabaseName, ObjectName)) then
        begin
          Database := DatabaseByName(DatabaseName);
          if (Assigned(Database)) then
          begin
            Table := Database.BaseTableByName(ObjectName);
            if (Assigned(Table) and (SQLParseEnd(Parse) or SQLParseChar(Parse, ';'))) then
            begin
              TSBaseTable(Table).InvalidateStatus();
              TSBaseTable(Table).InvalidateData();
              ExecuteEvent(etItemValid, Database, Database.Tables, Table);
            end;
          end;
        end;
      end;
    end
    else if (SQLParseKeyword(Parse, 'SET')) then
    begin
      repeat
        if ((SQLParseKeyword(Parse, 'GLOBAL') or SQLParseKeyword(Parse, 'SESSION')) and not SQLParseChar(Parse, '@', False)) then
        begin
          Variable := VariableByName(SQLParseValue(Parse));
          if (Assigned(Variable) and SQLParseChar(Parse, '=')) then
          begin
            Variable.FValue := SQLParseValue(Parse);
            ExecuteEvent(etItemAltered, Self, Variables, Variable);
          end;
        end
        else
        begin
          ObjectName := SQLParseValue(Parse);
          if (Copy(ObjectName, 1, 2) = '@@') then
          begin
            Delete(ObjectName, 1, 2);
            if (LowerCase(Copy(ObjectName, 1, 7)) = 'global.') then
              Delete(ObjectName, 1, 7)
            else if (LowerCase(Copy(ObjectName, 1, 8)) = 'session.') then
              Delete(ObjectName, 1, 8);
            Variable := VariableByName(ObjectName);
            if (Assigned(Variable) and SQLParseChar(Parse, '=')) then
            begin
              Variable.FValue := SQLParseValue(Parse);
              ExecuteEvent(etItemAltered, Self, Variables, Variable);
            end;
          end;
        end;
      until (not SQLParseChar(Parse, ','));
    end
    else if (SQLParseKeyword(Parse, 'OPTIMIZE')) then
    begin
      SQLParseKeyword(Parse, 'NO_WRITE_TO_BINLOG');
      SQLParseKeyword(Parse, 'LOCAL');
      if (SQLParseKeyword(Parse, 'TABLE')) then
        repeat
          DatabaseName := Self.DatabaseName;
          if (SQLParseObjectName(Parse, DatabaseName, ObjectName)) then
          begin
            Database := DatabaseByName(DatabaseName);
            if (Assigned(Database)) then
            begin
              Table := Database.BaseTableByName(ObjectName);
              if (Assigned(Table)) then
              begin
                TSBaseTable(Table).InvalidateStatus();
                ExecuteEvent(etItemValid, Database, Database.Tables, Table);
              end;
            end;
          end;
        until (not SQLParseChar(Parse, ','));
    end
    else if (SQLParseKeyword(Parse, 'TRUNCATE')) then
    begin
      SQLParseKeyword(Parse, 'TABLE');
      DatabaseName := Self.DatabaseName;
      if (SQLParseObjectName(Parse, DatabaseName, ObjectName)) then
      begin
        Database := DatabaseByName(DatabaseName);
        if (Assigned(Database)) then
        begin
          Table := Database.BaseTableByName(ObjectName);
          if (Assigned(Table)) then
          begin
            TSBaseTable(Table).InvalidateStatus();
            TSBaseTable(Table).InvalidateData();
            ExecuteEvent(etItemValid, Database, Database.Tables, Table);
          end;
        end;
      end;
    end
    else if (SQLParseKeyword(Parse, 'CREATE USER')) then
      repeat
        ObjectName := SQLParseValue(Parse);
        Users.Add(TSUser.Create(Users, ObjectName), True);
        while (not SQLParseEnd(Parse) and not SQLParseChar(Parse, ',') and not SQLParseChar(Parse, ';')) do
          SQLParseValue(Parse);
      until (not SQLParseChar(Parse, ','))
    else if (SQLParseKeyword(Parse, 'RENAME USER')) then
      repeat
        OldObjectName := SQLParseValue(Parse);
        if (SQLParseKeyword(Parse, 'TO')) then
        begin
          ObjectName := SQLParseValue(Parse);
          User := UserByName(OldObjectName);
          if (Assigned(User)) then
          begin
            User.Name := ObjectName;
            ExecuteEvent(etItemAltered, Self, Users, User);
          end;
        end;
      until (not SQLParseChar(Parse, ','))
    else if (SQLParseKeyword(Parse, 'GRANT')) then
      begin
        while (not SQLParseChar(Parse, ';') and not SQLParseEnd(Parse) and not SQLParseKeyword(Parse, 'TO', False)) do
          SQLParseValue(Parse);
        if (SQLParseKeyword(Parse, 'TO')) then
          repeat
            ObjectName := SQLParseValue(Parse);
            User := UserByName(ObjectName);
            if (Assigned(User)) then
            begin
              User.Invalidate();
              ExecuteEvent(etItemAltered, Self, Users, User);
            end;
            while (not SQLParseChar(Parse, ';') and not SQLParseEnd(Parse) and not SQLParseKeyword(Parse, 'REQUIRE', False) and not SQLParseKeyword(Parse, 'WITH', False) and not SQLParseChar(Parse, ',', False)) do
              SQLParseValue(Parse);
          until (not SQLParseChar(Parse, ','));
      end
    else if (SQLParseKeyword(Parse, 'REVOKE')) then
      begin
        while (not SQLParseEnd(Parse) and not SQLParseKeyword(Parse, 'FROM', False)) do
          SQLParseValue(Parse);
        if (SQLParseKeyword(Parse, 'FROM')) then
          repeat
            ObjectName := SQLParseValue(Parse);
            User := UserByName(ObjectName);
            if (Assigned(User)) then
            begin
              User.Invalidate();
              ExecuteEvent(etItemAltered, Self, Users, User);
            end;
          until (not SQLParseChar(Parse, ','));
      end
    else if (SQLParseKeyword(Parse, 'DROP USER')) then
      repeat
        ObjectName := SQLParseValue(Parse);
        User := UserByName(ObjectName);
        if (Assigned(User)) then
          Users.Delete(User);
      until (not SQLParseChar(Parse, ','))
    else if (SQLParseKeyword(Parse, 'KILL') and (SQLParseKeyword(Parse, 'CONNECTION') or not SQLParseKeyword(Parse, 'QUERY'))) then
    begin
      ObjectName := SQLParseValue(Parse);
      Process := ProcessByThreadId(StrToInt(ObjectName));
      if (Assigned(Process)) then
        Processes.Delete(Process);
    end;
end;

function TSSession.PluginByName(const PluginName: string): TSPlugin;
var
  Index: Integer;
begin
  Index := Plugins.IndexByName(PluginName);
  if (Index < 0) then
    Result := nil
  else
    Result := Plugins[Index];
end;

function TSSession.ProcessByThreadId(const ThreadId: Longword): TSProcess;
var
  I: Integer;
begin
  Result := nil;

  if (Assigned(Processes)) then
    for I := 0 to Processes.Count - 1 do
      if (Processes[I].ThreadId = ThreadId) then
        Result := Processes[I];
end;

procedure TSSession.RegisterEventProc(const AEventProc: TEventProc);
var
  I: Integer;
  Index: Integer;
  ListEntry: Pointer;
begin
  Index := -1;
  for I := 0 to EventProcs.Count - 1 do
    if (CompareMem(EventProcs[I], @TMethod(AEventProc), SizeOf(TMethod))) then
      Index := I;

  if (Index < 0) then
  begin
    GetMem(ListEntry, SizeOf(TMethod));
    MoveMemory(ListEntry, @TMethod(AEventProc), SizeOf(TMethod));
    EventProcs.Add(ListEntry);
  end;
end;

procedure TSSession.RollbackTransaction();
begin
  inherited;
  AutoCommit := AutoCommitBeforeTransaction;

  Invalidate();
end;

function TSSession.SessionResult(const DataHandle: TMySQLConnection.TDataResult; const Data: Boolean): Boolean;
var
  Database: TSDatabase;
  DatabaseName: string;
  DataSet: TMySQLQuery;
  Field: Integer;
  FunctionName: string;
  ObjectName: string;
  Parse: TSQLParse;
  SObject: TSObject;
  SQL: string;
  Table: TSTable;
begin
  Result := False;

  DataSet := TMySQLQuery.Create(nil);

  SQL := CommandText;
  if (SQLCreateParse(Parse, PChar(SQL), Length(SQL), ServerVersion)) then
    if (SQLParseKeyword(Parse, 'SELECT')) then
    begin
      DatabaseName := Self.DatabaseName;
      if ((SQLParseChar(Parse, '*') or (SQLParseValue(Parse) = 'GRANTEE')) and SQLParseKeyword(Parse, 'FROM') and SQLParseObjectName(Parse, DatabaseName, ObjectName)) then
      begin
        if (TableNameCmp(DatabaseName, INFORMATION_SCHEMA) = 0) then
        begin
          DataSet.Open(DataHandle);
          if (TableNameCmp(ObjectName, 'CHARACTER_SETS') = 0) then
            Result := Charsets.Build(DataSet, True, not SQLParseEnd(Parse) and not SQLParseChar(Parse, ';'))
          else if (TableNameCmp(ObjectName, 'COLLATIONS') = 0) then
            Result := Collations.Build(DataSet, True, not SQLParseEnd(Parse) and not SQLParseChar(Parse, ';'))
          else if ((TableNameCmp(ObjectName, 'COLUMNS') = 0) and SQLParseKeyword(Parse, 'WHERE') and (UpperCase(SQLParseValue(Parse)) = 'TABLE_SCHEMA') and SQLParseChar(Parse, '=')) then
            DatabaseByName(SQLParseValue(Parse)).Tables.BuildViewFields(DataSet, True)
          else if (TableNameCmp(ObjectName, 'ENGINES') = 0) then
            Result := Engines.Build(DataSet, True, not SQLParseEnd(Parse) and not SQLParseChar(Parse, ';'))
          else if ((TableNameCmp(ObjectName, 'EVENTS') = 0) and (SQLParseEnd(Parse) or SQLParseChar(Parse, ';'))) then
            Result := BuildEvents(DataSet)
          else if ((TableNameCmp(ObjectName, 'EVENTS') = 0) and SQLParseKeyword(Parse, 'WHERE') and (UpperCase(SQLParseValue(Parse)) = 'EVENT_SCHEMA') and SQLParseChar(Parse, '=')) then
          begin
            Database := DatabaseByName(SQLParseValue(Parse));
            Result := Database.Events.Build(DataSet, True, not SQLParseEnd(Parse) and not SQLParseChar(Parse, ';'));
          end
          else if (TableNameCmp(ObjectName, 'PLUGINS') = 0) then
            Result := Plugins.Build(DataSet, True, not SQLParseEnd(Parse) and not SQLParseChar(Parse, ';'))
          else if (TableNameCmp(ObjectName, 'PROCESSLIST') = 0) then
            Result := Processes.Build(DataSet, True, not SQLParseEnd(Parse) and not SQLParseChar(Parse, ';'))
          else if ((TableNameCmp(ObjectName, 'ROUTINES') = 0) and (SQLParseEnd(Parse) or SQLParseChar(Parse, ';'))) then
            Result := BuildRoutines(DataSet)
          else if ((TableNameCmp(ObjectName, 'ROUTINES') = 0) and SQLParseKeyword(Parse, 'WHERE') and (UpperCase(SQLParseValue(Parse)) = 'ROUTINE_SCHEMA') and SQLParseChar(Parse, '=')) then
          begin
            Database := DatabaseByName(SQLParseValue(Parse));
            Result := Database.Routines.Build(DataSet, True, not SQLParseEnd(Parse) and not SQLParseChar(Parse, ';'));
          end
          else if (TableNameCmp(ObjectName, 'SESSION_STATUS') = 0) then
            Result := Stati.Build(DataSet, True, not SQLParseEnd(Parse) and not SQLParseChar(Parse, ';'))
          else if (TableNameCmp(ObjectName, 'SESSION_VARIABLES') = 0) then
            Result := Variables.Build(DataSet, True, not SQLParseEnd(Parse) and not SQLParseChar(Parse, ';'))
          else if ((TableNameCmp(ObjectName, 'SCHEMATA') = 0) and (SQLParseEnd(Parse) or SQLParseChar(Parse, ';'))) then
            Result := Databases.Build(DataSet, True, False)
          else if ((TableNameCmp(ObjectName, 'SCHEMATA') = 0) and SQLParseKeyword(Parse, 'WHERE') and (UpperCase(SQLParseValue(Parse)) = 'SCHEMA_NAME')) then
            Result := Databases.Build(DataSet, True, False)
          else if ((TableNameCmp(ObjectName, 'TABLES') = 0) and (SQLParseEnd(Parse) or SQLParseChar(Parse, ';'))) then
            Result := BuildTables(DataSet)
          else if ((TableNameCmp(ObjectName, 'TABLES') = 0) and SQLParseKeyword(Parse, 'WHERE') and (UpperCase(SQLParseValue(Parse)) = 'TABLE_SCHEMA') and SQLParseChar(Parse, '=')) then
          begin
            Database := DatabaseByName(SQLParseValue(Parse));
            Result := Database.Tables.Build(DataSet, True, not SQLParseEnd(Parse) and not SQLParseChar(Parse, ';'));
          end
          else if ((TableNameCmp(ObjectName, 'TRIGGERS') = 0) and (SQLParseEnd(Parse) or SQLParseChar(Parse, ';'))) then
            Result := BuildTriggers(DataSet)
          else if ((TableNameCmp(ObjectName, 'TRIGGERS') = 0) and SQLParseKeyword(Parse, 'WHERE') and (UpperCase(SQLParseValue(Parse)) = 'EVENT_OBJECT_SCHEMA') and SQLParseChar(Parse, '=')) then
          begin
            Database := DatabaseByName(SQLParseValue(Parse));
            Result := Database.Triggers.Build(DataSet, True, not SQLParseEnd(Parse) and not SQLParseChar(Parse, ';'));
          end
          else if ((TableNameCmp(ObjectName, 'USER_PRIVILEGES') = 0)) then
            Result := Users.Build(DataSet, True, not SQLParseKeyword(Parse, 'GROUP BY') and not SQLParseEnd(Parse) and not SQLParseChar(Parse, ';'))
          else
            raise EConvertError.CreateFmt(SUnknownSQLStmt, [CommandText]);
        end
        else if (Databases.NameCmp(DatabaseName, PERFORMANCE_SCHEMA) = 0) then
        begin
          DataSet.Open(DataHandle);
          if (TableNameCmp(ObjectName, 'SESSION_STATUS') = 0) then
            Result := Stati.Build(DataSet, True, not SQLParseEnd(Parse) and not SQLParseChar(Parse, ';'))
          else if (TableNameCmp(ObjectName, 'SESSION_VARIABLES') = 0) then
            Result := Variables.Build(DataSet, True, not SQLParseEnd(Parse) and not SQLParseChar(Parse, ';'));
        end
        else if (Databases.NameCmp(DatabaseName, 'mysql') = 0) then
        begin
          DataSet.Open(DataHandle);
          if (TableNameCmp(ObjectName, 'user') = 0) then
            Result := Users.Build(DataSet, False, not SQLParseEnd(Parse) and not SQLParseChar(Parse, ';'));
        end
        else if (DataHandle.Connection.ErrorCode = 0) then
        begin
          Database := DatabaseByName(DatabaseName);
          if (Assigned(Database)) then
          begin
            Table := Database.TableByName(ObjectName);
            if (Assigned(Table.FDataSet) and not Table.FDataSet.Active) then
              Table.FDataSet.Open(DataHandle)
            else
              DataSet.Open(DataHandle);
          end;
        end;
      end
      else if ((FCurrentUser = '') and (DataHandle.Connection.ErrorCode = 0)) then
      begin
        DataSet.Open(DataHandle);
        Field := 0;
        if (not DataSet.IsEmpty) then
          repeat
            FunctionName := SQLParseValue(Parse);
            if (SQLParseChar(Parse, '(', False)) then
              FunctionName := FunctionName + SQLParseValue(Parse);
            if (lstrcmpi(PChar(FunctionName), 'CURRENT_USER()') = 0) then
              FCurrentUser := DataSet.Fields[Field].AsString
            else if (lstrcmpi(PChar(FunctionName), 'SYSDATE()') = 0) then
            begin
              if (TryStrToDateTime(DataSet.Fields[0].AsString, TimeDiff, FormatSettings)) then
                TimeDiff := TimeDiff - Now();
            end
            else if (lstrcmpi(PChar(FunctionName), 'USER()') = 0) then
              FCurrentUser := DataSet.Fields[Field].AsString;
            Inc(Field);
          until (not SQLParseChar(Parse, ','));
      end;
    end
    else if (SQLParseKeyword(Parse, 'SHOW')) then
    begin
      DataSet.Open(DataHandle);
      DatabaseName := DataSet.DatabaseName;
      if (SQLParseKeyword(Parse, 'CHARACTER SET')) then
        Result := Charsets.Build(DataSet, False, not SQLParseEnd(Parse))
      else if (SQLParseKeyword(Parse, 'COLLATION')) then
        Result := Collations.Build(DataSet, False, not SQLParseEnd(Parse))
      else if (SQLParseKeyword(Parse, 'CREATE')) then
      begin
        if (not DataSet.Active) then
        begin
          SObject := nil;
          if (SQLParseKeyword(Parse, 'DATABASE')) then
            SObject := DatabaseByName(SQLParseValue(Parse))
          else if (not Assigned(DatabaseByName(DatabaseName))) then
            SObject := nil
          else if (SQLParseKeyword(Parse, 'EVENT')) then
            begin if (SQLParseObjectName(Parse, DatabaseName, ObjectName)) then SObject := DatabaseByName(DatabaseName).EventByName(ObjectName); end
          else if (SQLParseKeyword(Parse, 'FUNCTION')) then
            begin if (SQLParseObjectName(Parse, DatabaseName, ObjectName)) then SObject := DatabaseByName(DatabaseName).FunctionByName(ObjectName); end
          else if (SQLParseKeyword(Parse, 'PROCEDURE')) then
            begin if (SQLParseObjectName(Parse, DatabaseName, ObjectName)) then SObject := DatabaseByName(DatabaseName).ProcedureByName(ObjectName); end
          else if (SQLParseKeyword(Parse, 'TABLE')) then
            begin if (SQLParseObjectName(Parse, DatabaseName, ObjectName)) then SObject := DatabaseByName(DatabaseName).TableByName(ObjectName); end
          else if (SQLParseKeyword(Parse, 'TRIGGER')) then
            begin if (SQLParseObjectName(Parse, DatabaseName, ObjectName)) then SObject := DatabaseByName(DatabaseName).TriggerByName(ObjectName); end
          else if (SQLParseKeyword(Parse, 'VIEW')) then
            begin if (SQLParseObjectName(Parse, DatabaseName, ObjectName)) then SObject := DatabaseByName(DatabaseName).TableByName(ObjectName); end;
          if (Assigned(SObject)) then
            SObject.FInvalid := True;
        end
        else
        begin
          if (SQLParseKeyword(Parse, 'DATABASE')) then
            DatabaseByName(SQLParseValue(Parse)).SetSource(DataSet)
          else if (SQLParseKeyword(Parse, 'EVENT')) then
            begin if (SQLParseObjectName(Parse, DatabaseName, ObjectName)) then DatabaseByName(DatabaseName).EventByName(ObjectName).SetSource(DataSet); end
          else if (SQLParseKeyword(Parse, 'FUNCTION')) then
            begin if (SQLParseObjectName(Parse, DatabaseName, ObjectName)) then DatabaseByName(DatabaseName).FunctionByName(ObjectName).SetSource(DataSet); end
          else if (SQLParseKeyword(Parse, 'PROCEDURE')) then
            begin if (SQLParseObjectName(Parse, DatabaseName, ObjectName)) then DatabaseByName(DatabaseName).ProcedureByName(ObjectName).SetSource(DataSet); end
          else if (SQLParseKeyword(Parse, 'TABLE')) then
            begin if (SQLParseObjectName(Parse, DatabaseName, ObjectName)) then DatabaseByName(DatabaseName).TableByName(ObjectName).SetSource(DataSet); end
          else if (SQLParseKeyword(Parse, 'TRIGGER')) then
            begin if (SQLParseObjectName(Parse, DatabaseName, ObjectName)) then DatabaseByName(DatabaseName).TriggerByName(ObjectName).SetSource(DataSet); end
          else if (SQLParseKeyword(Parse, 'VIEW')) then
            begin if (SQLParseObjectName(Parse, DatabaseName, ObjectName)) then DatabaseByName(DatabaseName).TableByName(ObjectName).SetSource(DataSet); end;
        end;
      end
      else if (SQLParseKeyword(Parse, 'DATABASES')) then
        Result := Databases.Build(DataSet, False, not SQLParseEnd(Parse) and not SQLParseChar(Parse, ';'))
      else if (SQLParseKeyword(Parse, 'ENGINES')) then
        Result := Engines.Build(DataSet, False, not SQLParseEnd(Parse))
      else if (SQLParseKeyword(Parse, 'EVENTS')) then
      begin
        if (not SQLParseKeyword(Parse, 'FROM') or SQLParseKeyword(Parse, 'IN')) then
          DatabaseName := Self.DatabaseName
        else
          DatabaseName := SQLParseValue(Parse);
        Result := DatabaseByName(DatabaseName).Events.Build(DataSet, False, not SQLParseEnd(Parse));
      end
      else if (SQLParseKeyword(Parse, 'GRANTS FOR')) then
        if (SQLParseKeyword(Parse, 'CURRENT_USER')) then
          BuildUser(DataSet)
        else
        begin
          ObjectName := SQLParseValue(Parse);
          if (Users.NameCmp(ObjectName, FCurrentUser) = 0) then
            BuildUser(DataSet)
          else if (Assigned(UserByName(ObjectName))) then
            UserByName(ObjectName).SetSource(DataSet)
          else
            Users.Invalidate();
        end
      else if (SQLParseKeyword(Parse, 'PLUGINS')) then
        Result := Plugins.Build(DataSet, False, not SQLParseEnd(Parse))
      else if (SQLParseKeyword(Parse, 'PROCEDURE STATUS')
        or SQLParseKeyword(Parse, 'FUNCTION STATUS')) then
      begin
        if (not SQLParseKeyword(Parse, 'WHERE') or not SQLParseKeyword(Parse, 'DB') or not SQLParseChar(Parse, '=')) then
          DatabaseName := Self.DatabaseName
        else
          DatabaseName := SQLParseValue(Parse);
        Result := DatabaseByName(DatabaseName).Routines.Build(DataSet, False, not SQLParseEnd(Parse));
      end
      else if (SQLParseKeyword(Parse, 'FULL PROCESSLIST')
        or SQLParseKeyword(Parse, 'PROCESSLIST')) then
        Result := Processes.Build(DataSet, False, not SQLParseEnd(Parse) and not SQLParseChar(Parse, ';'))
      else if (SQLParseKeyword(Parse, 'STATUS')
        or SQLParseKeyword(Parse, 'SESSION STATUS')) then
        Result := Stati.Build(DataSet, False, not SQLParseEnd(Parse) and not SQLParseChar(Parse, ';'))
      else if (SQLParseKeyword(Parse, 'VARIABLES')
        or SQLParseKeyword(Parse, 'SESSION VARIABLES')) then
        Result := Variables.Build(DataSet, False, not SQLParseEnd(Parse) and not SQLParseChar(Parse, ';'))
      else if (SQLParseKeyword(Parse, 'FULL TABLES')
        or SQLParseKeyword(Parse, 'OPEN TABLES')
        or SQLParseKeyword(Parse, 'TABLES')) then
      begin
        if (not SQLParseKeyword(Parse, 'FROM') or SQLParseKeyword(Parse, 'IN')) then
          DatabaseName := Self.DatabaseName
        else
          DatabaseName := SQLParseValue(Parse);
        Result := Assigned(DatabaseByName(DatabaseName)) and DatabaseByName(DatabaseName).Tables.Build(DataSet, False, not SQLParseChar(Parse, ';') and not SQLParseEnd(Parse));
      end
      else if (SQLParseKeyword(Parse, 'TABLE STATUS')) then
      begin
        if (not SQLParseKeyword(Parse, 'FROM') or SQLParseKeyword(Parse, 'IN')) then
          DatabaseName := Self.DatabaseName
        else
          DatabaseName := SQLParseValue(Parse);
        Result := DatabaseByName(DatabaseName).Tables.Build(DataSet, False,not SQLParseChar(Parse, ';') and not SQLParseEnd(Parse));
      end
      else if (SQLParseKeyword(Parse, 'TRIGGERS')) then
      begin
        if (not SQLParseKeyword(Parse, 'FROM') or SQLParseKeyword(Parse, 'IN')) then
          DatabaseName := Self.DatabaseName
        else
          DatabaseName := SQLParseValue(Parse);
        Result := DatabaseByName(DatabaseName).Triggers.Build(DataSet, False, not SQLParseChar(Parse, ';') and not SQLParseEnd(Parse));
      end
      else
        raise EConvertError.CreateFmt(SUnknownSQLStmt, [CommandText]);
    end;

  DataSet.Free();
end;

procedure TSSession.SetAutoCommit(const AAutoCommit: Boolean);
var
  DataSet: TMySQLQuery;
  Index: Integer;
  SQL: string;
begin
  if (AAutoCommit <> AutoCommit) then
  begin
    Index := Variables.IndexByName('autocommit');
    if (Index >= 0) then
    begin
      if (AAutoCommit) then
        SQL := 'SET AUTOCOMMIT=1'
      else
        SQL := 'SET AUTOCOMMIT=0';

      BeginSilent();
      if (ExecuteSQL(SQL)) then
      begin
        DataSet := TMySQLQuery.Create(nil);
        DataSet.Connection := Self;
        DataSet.CommandText := 'SELECT @@AUTOCOMMIT';
        DataSet.Open();
        if (DataSet.Active and (DataSet.FieldCount = 1)) then
        begin
          Variables[Index].Value := DataSet.Fields[0].AsString;
          inherited SetAutoCommit(Variables[Index].AsBoolean);
        end;
        DataSet.Free();
      end;
      EndSilent();
    end;
  end;
end;

procedure TSSession.SetCharset(const ACharset: string);
begin
  inherited;

  if (Assigned(Variables) and Variables.Valid) then
    if (ServerVersion < 40101) then
      VariableByName('character_set').Value := Charset
    else
    begin
      VariableByName('character_set_client').Value := Charset;
      VariableByName('character_set_connection').Value := Charset;
      VariableByName('character_set_results').Value := Charset;
    end;
end;

procedure TSSession.SetCreateDesktop(ACreateDesktop: TCreateDesktop);
var
  I: Integer;
begin
  if (Assigned(FCreateDesktop) and not Assigned(ACreateDesktop)) then
    for I := 0 to Databases.Count - 1 do
      Databases[I].FreeDesktop();

  FCreateDesktop := ACreateDesktop;
end;

procedure TSSession.StartTransaction();
begin
  AutoCommitBeforeTransaction := AutoCommit;

  AutoCommit := False;

  inherited;
end;

function TSSession.StatusByName(const StatusName: string): TSStatus;
var
  Index: Integer;
begin
  Index := Stati.IndexByName(StatusName);
  if (Index < 0) then
    Result := nil
  else
    Result := Stati[Index];
end;

function TSSession.TableName(const Name: string): string;
begin
  if (LowerCaseTableNames in [0]) then
    Result := Name
  else
    Result := LowerCase(Name);
end;

function TSSession.TableNameCmp(const Name1, Name2: string): Integer;
begin
  if (LowerCaseTableNames in [0]) then
    Result := lstrcmp(PChar(Name1), PChar(Name2))
  else
    Result := lstrcmpi(PChar(Name1), PChar(Name2));
end;

function TSSession.UnescapeValue(const Value: string; const FieldType: TMySQLFieldType = mfVarChar): string;
begin
  Result := SQLUnescape(Value);

  if (FieldType in [mfFloat, mfDouble, mfDecimal]) then
    Result := ReplaceStr(Result, '.', FormatSettings.DecimalSeparator)
  else
    Result := SQLUnescape(Value);
end;

function TSSession.UnecapeRightIdentifier(const Identifier: string): string;
var
  DBIdentifier: string;
begin
  DBIdentifier := UnescapeValue(Identifier);

  if (DBIdentifier = '%') then Result := '' else Result := DBIdentifier;
end;

procedure TSSession.UnRegisterEventProc(const AEventProc: TEventProc);
var
  I: Integer;
  Index: Integer;
begin
  Index := -1;
  for I := 0 to EventProcs.Count - 1 do
    if (CompareMem(EventProcs[I], @TMethod(AEventProc), SizeOf(TMethod))) then
      Index := I;

  if (Index >= 0) then
  begin
    FreeMem(EventProcs[Index]);
    EventProcs.Delete(Index);
  end;
end;

function TSSession.Update(): Boolean;
var
  List: TList;
begin
  List := TList.Create();
  Result := Update(List);
  List.Free();
end;

function TSSession.Update(const Objects: TList; const Status: Boolean = False): Boolean;
var
  Database: TSDatabase;
  I: Integer;
  List: TList;
  SQL: string;
  Tables: TList;
  ViewInTables: Boolean;
begin
  SQL := '';

  if (FCurrentUser = '') then
    if (ServerVersion < 40006) then
      SQL := SQL + 'SELECT SYSDATE(),USER();' + #13#10
    else
      SQL := SQL + 'SELECT SYSDATE(),CURRENT_USER();' + #13#10;

  if (not Assigned(FUser) and ((ServerVersion >= 40102) or (FCurrentUser <> ''))) then
    if (ServerVersion < 40102) then
      SQL := SQL + 'SHOW GRANTS FOR ' + EscapeUser(FCurrentUser) + ';' + #13#10
    else
      SQL := SQL + 'SHOW GRANTS FOR CURRENT_USER();' + #13#10;


  List := TList.Create();

  if (Assigned(Variables) and not Variables.Valid) then List.Add(Variables);
  if (Assigned(Stati) and not Stati.Valid) then List.Add(Stati);
  if (Assigned(Engines) and not Engines.Valid) then List.Add(Engines);
  if (Assigned(Charsets) and not Charsets.Valid) then List.Add(Charsets);
  if (Assigned(Collations) and not Collations.Valid) then List.Add(Collations);
  if (Assigned(Users) and not Users.Valid) then List.Add(Users);
  if (Assigned(Databases) and not Databases.Valid) then List.Add(Databases);

  if (Assigned(Objects)) then
    List.Assign(Objects, laOr);
  if (Assigned(InvalidObjects) and (InvalidObjects.Count < 10)) then
    List.Assign(InvalidObjects, laOr);
  List.Sort(Compare);

  Tables := TList.Create();
  ViewInTables := False;

  Database := nil;
  for I := 0 to List.Count - 1 do
    if ((TObject(List[I]) is TSEntities) and not TSEntities(List[I]).Valid) then
      SQL := SQL + TSEntities(List[I]).SQLGetItems()
    else if (TObject(List[I]) is TSDatabase) then
    begin
      if (not TSDatabase(List[I]).ValidSource) then
        SQL := SQL + TSDatabase(List[I]).SQLGetSource();
      if (not TSDatabase(List[I]).Tables.Valid) then
        SQL := SQL + TSDatabase(List[I]).Tables.SQLGetItems();
      if (Assigned(TSDatabase(List[I]).Routines) and not TSDatabase(List[I]).Routines.Valid) then
        SQL := SQL + TSDatabase(List[I]).Routines.SQLGetItems();
      if (Assigned(TSDatabase(List[I]).Triggers) and not TSDatabase(List[I]).Triggers.Valid) then
        SQL := SQL + TSDatabase(List[I]).Triggers.SQLGetItems();
      if (Assigned(TSDatabase(List[I]).Events) and not TSDatabase(List[I]).Events.Valid) then
        SQL := SQL + TSDatabase(List[I]).Events.SQLGetItems();
    end
    else if (TObject(List[I]) is TSDBObject) then
    begin
      if (Assigned(Database) and (TSDBObject(List[I]).Database <> Database)) then
      begin
        if (Tables.Count > 0) then
        begin
          SQL := SQL + Database.Tables.SQLGetStatus(Tables);
          if (ViewInTables) then
            SQL := SQL + Database.Tables.SQLGetViewFields(Tables);
          Tables.Clear();
          ViewInTables := False;
        end;
      end;
      Database := TSDBObject(List[I]).Database;

      if (not TSDBObject(List[I]).ValidSource) then
        SQL := SQL + TSDBObject(List[I]).SQLGetSource();
      if ((TSDBObject(List[I]) is TSBaseTable) and not TSBaseTable(List[I]).ValidStatus) then
        Tables.Add(List[I])
      else if ((TSObject(List[I]) is TSView) and not TSView(List[I]).ValidFields) then
        Tables.Add(List[I]);
      ViewInTables := ViewInTables or (TSObject(List[I]) is TSView);
    end
    else if ((TObject(List[I]) is TSUser) and not TSUser(List[I]).Valid) then
      SQL := SQL + TSUser(List[I]).SQLGetSource();
  if (Tables.Count > 0) then
  begin
    SQL := SQL + Database.Tables.SQLGetStatus(Tables);
    if (ViewInTables) then
      SQL := SQL + Database.Tables.SQLGetViewFields(Tables);
    Tables.Clear();
  end;

  for I := 0 to List.Count - 1 do
    if ((TObject(List[I]) is TSBaseTable) and Assigned(TSBaseTable(List[I]).FDataSet) and not TSBaseTable(List[I]).FDataSet.Active
      and (TSBaseTable(List[I]).Database <> InformationSchema) and (Databases.NameCmp(TSBaseTable(List[I]).Database.Name, 'mysql') <> 0)) then
      SQL := SQL + TSBaseTable(List[I]).FDataSet.SQLSelect();

  for I := 0 to List.Count - 1 do
    if (TObject(List[I]) is TSDatabase) then
    begin
      if (Status and not TSDatabase(List[I]).Tables.ValidStatus and not (TSDatabase(List[I]) is TSSystemDatabase)) then
        SQL := SQL + TSDatabase(List[I]).Tables.SQLGetStatus(TSDatabase(List[I]).Tables);
    end;

  if (not Assigned(Objects) and Status and (ServerVersion >= 50002) and not Valid) then
  begin
    SQL := SQL + 'SELECT * FROM ' + EscapeIdentifier(INFORMATION_SCHEMA) + '.' + EscapeIdentifier('TABLES') + ';' + #13#10;
    if (ServerVersion >= 50010) then SQL := SQL + 'SELECT * FROM ' + EscapeIdentifier(INFORMATION_SCHEMA) + '.' + EscapeIdentifier('TRIGGERS') + ';' + #13#10;
    if (ServerVersion >= 50004) then SQL := SQL + 'SELECT * FROM ' + EscapeIdentifier(INFORMATION_SCHEMA) + '.' + EscapeIdentifier('ROUTINES') + ';' + #13#10;
    if (ServerVersion >= 50106) then SQL := SQL + 'SELECT * FROM ' + EscapeIdentifier(INFORMATION_SCHEMA) + '.' + EscapeIdentifier('EVENTS') + ';' + #13#10;
  end;


  Tables.Free();
  if (Assigned(InvalidObjects)) then
    InvalidObjects.Clear();
  List.Free();

  Result := (SQL = '') or SendSQL(SQL, SessionResult);
end;

function TSSession.UpdateDatabase(const Database, NewDatabase: TSDatabase): Boolean;
var
  SQL: string;
begin
  SQL := '';
  if (ServerVersion >= 40101) then
  begin
    if (NewDatabase.FDefaultCharset <> '') then
      SQL := SQL + ' DEFAULT CHARACTER SET ' + NewDatabase.FDefaultCharset;
    if (NewDatabase.FCollation <> '') then
      SQL := SQL + ' COLLATE ' + NewDatabase.FCollation;
  end;

  if (not Assigned(Database)) then
    SQL := 'CREATE DATABASE ' + EscapeIdentifier(NewDatabase.Name) + SQL + ';' + #13#10
      + NewDatabase.SQLGetSource()
  else if (SQL <> '') then
    if (ServerVersion < 40108) then
    begin
      SQL := 'ALTER DATABASE ' + SQL + ';' + #13#10;
      if (DatabaseName <> Database.Name) then
        SQL := Database.SQLUse() + SQL;
    end
    else
      SQL := 'ALTER DATABASE ' + EscapeIdentifier(Database.Name) + SQL + ';' + #13#10;

  Result := (SQL = '') or SendSQL(SQL, SessionResult);
end;

procedure TSSession.UpdateIndexDefs(const DataSet: TMySQLQuery; const IndexDefs: TIndexDefs);
var
  Database: TSDatabase;
  Field: TSBaseTableField;
  FieldInfo: TFieldInfo;
  Found: Boolean;
  I: Integer;
  IndexDef: TIndexDef;
  J: Integer;
  K: Integer;
  OriginalDatabaseName: string;
  OriginalTableName: string;
  Table: TSBaseTable;
  UniqueTable: Boolean;
begin
  if (DataSet is TSTableDataSet) then
  begin
    OriginalDatabaseName := DataSet.DatabaseName;
    OriginalTableName := DataSet.CommandText;
  end
  else if (Assigned(DataSet)) then
  begin
    OriginalTableName := ''; UniqueTable := True;
    for I := 0 to DataSet.FieldCount - 1 do
      if (GetFieldInfo(DataSet.Fields[I].Origin, FieldInfo) and (FieldInfo.DatabaseName <> '') and (FieldInfo.TableName <> '')) then
      begin
        if (OriginalDatabaseName = '') then
          OriginalDatabaseName := FieldInfo.DatabaseName
        else
          UniqueTable := UniqueTable and (TableNameCmp(FieldInfo.DatabaseName, OriginalDatabaseName) = 0);
        if (OriginalTableName = '') then
          OriginalTableName := FieldInfo.TableName
        else
          UniqueTable := UniqueTable and (TableNameCmp(FieldInfo.TableName, OriginalTableName) = 0);
      end;
    if (not UniqueTable) then
    begin
      OriginalDatabaseName := '';
      OriginalTableName := '';
    end;
  end;

  if (Assigned(DataSet) and Assigned(DatabaseByName(OriginalDatabaseName))) then
  begin
    IndexDefs.Clear();
    Table := DatabaseByName(OriginalDatabaseName).BaseTableByName(OriginalTableName);
    if (Assigned(Table) and (Table.Keys.Count > 0)) then
      for I := 0 to Table.Keys.Count - 1 do
      begin
        Found := True;
        for J := 0 to Table.Keys[I].Columns.Count - 1 do
          if (Found) then
          begin
            Found := False;
            for K := 0 to DataSet.FieldCount - 1 do
              if (GetFieldInfo(DataSet.Fields[K].Origin, FieldInfo)) then
                Found := Found or (FieldInfo.OriginalFieldName = Table.Keys[I].Columns[J].Field.Name);
          end;
        if (Found) then
        begin
          IndexDef := IndexDefs.AddIndexDef();
          IndexDef.Name := Table.Keys[I].Name;
          if (Table.Keys[I].PrimaryKey) then
            IndexDef.Options := [ixPrimary, ixUnique]
          else if (Table.Keys[I].Unique) then
            IndexDef.Options := [ixUnique];
          for J := 0 to Table.Keys[I].Columns.Count - 1 do
          begin
            if (IndexDef.Fields <> '') then IndexDef.Fields := IndexDef.Fields + ';';
            for K := 0 to DataSet.FieldCount - 1 do
              if (GetFieldInfo(DataSet.Fields[K].Origin, FieldInfo) and (FieldInfo.OriginalFieldName = Table.Keys[I].Columns[J].Field.Name)) then
              begin
                IndexDef.Fields := IndexDef.Fields + DataSet.Fields[K].FieldName;
                if (not Table.Keys[I].Columns[J].Ascending) then
                begin
                  if (IndexDef.DescFields <> '') then IndexDef.DescFields := IndexDef.DescFields + ';';
                  IndexDef.Fields := IndexDef.Fields + DataSet.Fields[K].FieldName;
                end;
              end;
          end;
        end;
      end;
  end;

  for I := 0 to DataSet.FieldCount - 1 do
    if (GetFieldInfo(DataSet.Fields[I].Origin, FieldInfo)) then
    begin
      if (FieldInfo.DatabaseName = '') then
        Database := DatabaseByName(DataSet.DatabaseName)
      else
        Database := DatabaseByName(FieldInfo.DatabaseName);
      if (Assigned(Database)) then
      begin
        Table := Database.BaseTableByName(FieldInfo.TableName);
        if (Assigned(Table)) then
        begin
          Table.Session.BeginSynchron();
          Table.Update();
          Table.Session.EndSynchron();
          Field := Table.FieldByName(FieldInfo.OriginalFieldName);
          if (Assigned(Field) and not Field.AutoIncrement and (Field.Default <> 'NULL') and (Copy(Field.Default, 1, 17) <> 'CURRENT_TIMESTAMP')) then
            DataSet.Fields[I].DefaultExpression := Field.UnescapeValue(Field.Default);
          DataSet.Fields[I].ReadOnly := DataSet.Fields[I].ReadOnly or (Field.FieldKind = mkVirtual);
        end;
      end;
    end;
end;

function TSSession.UpdateUser(const User, NewUser: TSUser): Boolean;
type
  TRightType = (rtAll, rtDatabase, rtTable, rtRoutine, rtField);

  function GetRightType(const Right: TSUserRight): TRightType;
  begin
    if (Right.FieldName <> '') then
      Result := rtField
    else if ((Right.FunctionName <> '') or (Right.ProcedureName <> '')) then
      Result := rtRoutine
    else if (Right.TableName <> '') then
      Result := rtTable
    else if (Right.DatabaseName <> '') then
      Result := rtDatabase
    else
      Result := rtAll;
  end;

  function GetPrivileges(const Grant: Boolean; const OldRight, NewRight: TSUserRight; const RightType: TRightType): string;
  begin
    Result := '';

    if ((not Grant xor NewRight.RAlter          ) and (Grant xor (Assigned(OldRight) and OldRight.RAlter          )) and (RightType in [rtAll, rtDatabase])                                        ) then       Result := Result + ',ALTER';
    if ((not Grant xor NewRight.RAlterRoutine   ) and (Grant xor (Assigned(OldRight) and OldRight.RAlterRoutine   )) and (RightType in [rtAll, rtDatabase, rtRoutine]) and (ServerVersion >= 50003)) then       Result := Result + ',ALTER ROUTINE';
    if ((not Grant xor NewRight.RCreate         ) and (Grant xor (Assigned(OldRight) and OldRight.RCreate         )) and (RightType in [rtAll, rtDatabase])                                        ) then       Result := Result + ',CREATE';
    if ((not Grant xor NewRight.RCreateRoutine  ) and (Grant xor (Assigned(OldRight) and OldRight.RCreateRoutine  )) and (RightType in [rtAll, rtDatabase])            and (ServerVersion >= 50003)) then       Result := Result + ',CREATE ROUTINE';
    if ((not Grant xor NewRight.RCreateTableSpace)and (Grant xor (Assigned(OldRight) and OldRight.RCreateTableSpace))and (RightType in [rtAll])                        and (ServerVersion >= 50500)) then       Result := Result + ',CREATE TABLESPACE';
    if ((not Grant xor NewRight.RCreateTempTable) and (Grant xor (Assigned(OldRight) and OldRight.RCreateTempTable)) and (RightType in [rtAll, rtDatabase])            and (ServerVersion >= 40002)) then       Result := Result + ',CREATE TEMPORARY TABLES';
    if ((not Grant xor NewRight.RCreateUser     ) and (Grant xor (Assigned(OldRight) and OldRight.RCreateUser     )) and (RightType in [rtAll, rtDatabase])            and (ServerVersion >= 50003)) then       Result := Result + ',CREATE USER';
    if ((not Grant xor NewRight.RCreateView     ) and (Grant xor (Assigned(OldRight) and OldRight.RCreateView     )) and (RightType in [rtAll, rtDatabase])            and (ServerVersion >= 50001)) then       Result := Result + ',CREATE VIEW';
    if ((not Grant xor NewRight.RDelete         ) and (Grant xor (Assigned(OldRight) and OldRight.RDelete         )) and (RightType in [rtAll, rtDatabase, rtTable])                               ) then       Result := Result + ',DELETE';
    if ((not Grant xor NewRight.RDrop           ) and (Grant xor (Assigned(OldRight) and OldRight.RDrop           )) and (RightType in [rtAll, rtDatabase])                                        ) then       Result := Result + ',DROP';
    if ((not Grant xor NewRight.REvent          ) and (Grant xor (Assigned(OldRight) and OldRight.REvent          )) and (RightType in [rtAll, rtDatabase])            and (ServerVersion >= 50106)) then       Result := Result + ',EVENT';
    if ((not Grant xor NewRight.RExecute        ) and (Grant xor (Assigned(OldRight) and OldRight.RExecute        )) and (RightType in [rtAll, rtDatabase, rtRoutine]) and (ServerVersion >= 50003)) then       Result := Result + ',EXECUTE';
    if ((not Grant xor NewRight.RFile           ) and (Grant xor (Assigned(OldRight) and OldRight.RFile           )) and (RightType in [rtAll])                                                    ) then       Result := Result + ',FILE';
    if ((not Grant xor NewRight.RGrant          ) and (Grant xor (Assigned(OldRight) and OldRight.RGrant          )) and (RightType in [rtAll, rtDatabase, rtRoutine])                             ) then       Result := Result + ',GRANT OPTION';
    if ((not Grant xor NewRight.RIndex          ) and (Grant xor (Assigned(OldRight) and OldRight.RIndex          )) and (RightType in [rtAll, rtDatabase])                                        ) then       Result := Result + ',INDEX';
    if ((not Grant xor NewRight.RInsert         ) and (Grant xor (Assigned(OldRight) and OldRight.RInsert         ))                                                                               ) then begin Result := Result + ',INSERT';                  if (not Grant and (OldRight.FieldName <> '')) then Result := Result + '(' + EscapeIdentifier(OldRight.FieldName) + ')' else if (Grant and (NewRight.FieldName <> '')) then Result := Result + '(' + EscapeIdentifier(NewRight.FieldName) + ')'; end;
    if ((not Grant xor NewRight.RLockTables     ) and (Grant xor (Assigned(OldRight) and OldRight.RLockTables     )) and (RightType in [rtAll, rtDatabase])            and (ServerVersion >= 40002)) then       Result := Result + ',LOCK TABLES';
    if ((not Grant xor NewRight.RProcess        ) and (Grant xor (Assigned(OldRight) and OldRight.RProcess        )) and (RightType in [rtAll])                                                    ) then       Result := Result + ',PROCESS';
    if ((not Grant xor NewRight.RProxy          ) and (Grant xor (Assigned(OldRight) and OldRight.RProxy          )) and (RightType in [rtAll])                        and (ServerVersion >= 50507)) then       Result := Result + ',PROXY';
    if ((not Grant xor NewRight.RReferences     ) and (Grant xor (Assigned(OldRight) and OldRight.RReferences     ))                                                                               ) then begin Result := Result + ',REFERENCES';              if (not Grant and (OldRight.FieldName <> '')) then Result := Result + '(' + EscapeIdentifier(OldRight.FieldName) + ')' else if (Grant and (NewRight.FieldName <> '')) then Result := Result + '(' + EscapeIdentifier(NewRight.FieldName) + ')'; end;
    if ((not Grant xor NewRight.RReload         ) and (Grant xor (Assigned(OldRight) and OldRight.RReload         )) and (RightType in [rtAll])                                                    ) then       Result := Result + ',RELOAD';
    if ((not Grant xor NewRight.RReplClient     ) and (Grant xor (Assigned(OldRight) and OldRight.RReplClient     )) and (RightType in [rtAll]) and (ServerVersion >= 40002)                       ) then       Result := Result + ',REPLICATION CLIENT';
    if ((not Grant xor NewRight.RReplSlave      ) and (Grant xor (Assigned(OldRight) and OldRight.RReplSlave      )) and (RightType in [rtAll]) and (ServerVersion >= 40002)                       ) then       Result := Result + ',REPLICATION SLAVE';
    if ((not Grant xor NewRight.RSelect         ) and (Grant xor (Assigned(OldRight) and OldRight.RSelect         ))                                                                               ) then begin Result := Result + ',SELECT';                  if (not Grant and (OldRight.FieldName <> '')) then Result := Result + '(' + EscapeIdentifier(OldRight.FieldName) + ')' else if (Grant and (NewRight.FieldName <> '')) then Result := Result + '(' + EscapeIdentifier(NewRight.FieldName) + ')'; end;
    if ((not Grant xor NewRight.RShowDatabases  ) and (Grant xor (Assigned(OldRight) and OldRight.RShowDatabases  )) and (RightType in [rtAll]) and (ServerVersion >= 40002)                       ) then       Result := Result + ',SHOW DATABASES';
    if ((not Grant xor NewRight.RShowView       ) and (Grant xor (Assigned(OldRight) and OldRight.RShowView       )) and (RightType in [rtAll, rtDatabase]) and (ServerVersion >= 50001)           ) then       Result := Result + ',SHOW VIEW';
    if ((not Grant xor NewRight.RShutdown       ) and (Grant xor (Assigned(OldRight) and OldRight.RShutdown       )) and (RightType in [rtAll])                                                    ) then       Result := Result + ',SHUTDOWN';
    if ((not Grant xor NewRight.RSuper          ) and (Grant xor (Assigned(OldRight) and OldRight.RSuper          )) and (RightType in [rtAll]) and (ServerVersion >= 40002)                       ) then       Result := Result + ',SUPER';
    if ((not Grant xor NewRight.RTrigger        ) and (Grant xor (Assigned(OldRight) and OldRight.RTrigger        )) and (RightType in [rtAll, rtDatabase]) and (ServerVersion >= 50106)           ) then       Result := Result + ',TRIGGER';
    if ((not Grant xor NewRight.RUpdate         ) and (Grant xor (Assigned(OldRight) and OldRight.RUpdate         ))                                                                               ) then begin Result := Result + ',UPDATE';                  if (not Grant and (OldRight.FieldName <> '')) then Result := Result + '(' + EscapeIdentifier(OldRight.FieldName) + ')' else if (Grant and (NewRight.FieldName <> '')) then Result := Result + '(' + EscapeIdentifier(NewRight.FieldName) + ')'; end;

    Delete(Result, 1, 1);
  end;

var
  EmptyRight: TSUserRight;
  I: Integer;
  J: Integer;
  NewRight: TSUserRight;
  OldRight: TSUserRight;
  Options: string;
  Privileges: string;
  RemovedUserRights: array of Boolean;
  SingleSQL: string;
  SQL: string;
begin
  SQL := '';

  if (Assigned(User) and (NewUser.Name <> User.Name)) then
    if (ServerVersion < 50002) then
    begin
      SQL := SQL + 'UPDATE ' + EscapeIdentifier('mysql') + '.' + EscapeIdentifier('user'        ) + ' SET ' + EscapeIdentifier('User') + '=' + SQLEscape(NewUser.Login) + ',' + EscapeIdentifier('Host') + '=' + SQLEscape(NewUser.Host) + ' WHERE ' + EscapeIdentifier('User') + '=' + SQLEscape(User.Login) + ' AND ' + EscapeIdentifier('Host') + '=' + SQLEscape(User.Host) + ';' + #13#10;
      SQL := SQL + 'UPDATE ' + EscapeIdentifier('mysql') + '.' + EscapeIdentifier('db'          ) + ' SET ' + EscapeIdentifier('User') + '=' + SQLEscape(NewUser.Login) + ',' + EscapeIdentifier('Host') + '=' + SQLEscape(NewUser.Host) + ' WHERE ' + EscapeIdentifier('User') + '=' + SQLEscape(User.Login) + ' AND ' + EscapeIdentifier('Host') + '=' + SQLEscape(User.Host) + ';' + #13#10;
      SQL := SQL + 'UPDATE ' + EscapeIdentifier('mysql') + '.' + EscapeIdentifier('tables_priv' ) + ' SET ' + EscapeIdentifier('User') + '=' + SQLEscape(NewUser.Login) + ',' + EscapeIdentifier('Host') + '=' + SQLEscape(NewUser.Host) + ' WHERE ' + EscapeIdentifier('User') + '=' + SQLEscape(User.Login) + ' AND ' + EscapeIdentifier('Host') + '=' + SQLEscape(User.Host) + ';' + #13#10;
      SQL := SQL + 'UPDATE ' + EscapeIdentifier('mysql') + '.' + EscapeIdentifier('columns_priv') + ' SET ' + EscapeIdentifier('User') + '=' + SQLEscape(NewUser.Login) + ',' + EscapeIdentifier('Host') + '=' + SQLEscape(NewUser.Host) + ' WHERE ' + EscapeIdentifier('User') + '=' + SQLEscape(User.Login) + ' AND ' + EscapeIdentifier('Host') + '=' + SQLEscape(User.Host) + ';' + #13#10;
    end
    else
      SQL := SQL + 'RENAME USER ' + EscapeUser(User.Name) + ' TO ' + EscapeUser(NewUser.Name) + ';' + #13#10;

  if (not Assigned(User) and (ServerVersion > 50002)) then
    SQL := SQL + 'CREATE USER ' + EscapeUser(NewUser.Name) + ';' + #13#10;

  if (not Assigned(User)) then
    SetLength(RemovedUserRights, 0)
  else
  begin
    SetLength(RemovedUserRights, User.RightCount);
    for I := 0 to Length(RemovedUserRights) - 1 do
      RemovedUserRights[I] := False;
  end;

  for I := 0 to NewUser.RightCount - 1 do
  begin
    NewRight := NewUser.Right[I];

    OldRight := nil;
    if (Assigned(User)) then
      for J := 0 to User.RightCount - 1 do
        if   ((TableNameCmp(User.Right[J].DatabaseName , NewRight.DatabaseName ) = 0)
          and (TableNameCmp(User.Right[J].TableName    , NewRight.TableName    ) = 0)
          and (lstrcmpi(PChar(User.Right[J].ProcedureName), PChar(NewRight.ProcedureName)) = 0)
          and (lstrcmpi(PChar(User.Right[J].FunctionName ), PChar(NewRight.FunctionName )) = 0)
          and (lstrcmpi(PChar(User.Right[J].FieldName    ), PChar(NewRight.FieldName    )) = 0)) then
          OldRight := User.Right[J];


    if (Assigned(OldRight)) then
    begin
      Privileges := GetPrivileges(False, OldRight, NewRight, GetRightType(OldRight));

      if (Privileges <> '') then
      begin
        SingleSQL := 'REVOKE ' + Privileges + ' ON ';
        if (NewRight.TableName <> '') then
          SingleSQL := SingleSQL + EscapeIdentifier(NewRight.DatabaseName) + '.' + EscapeIdentifier(NewRight.TableName)
        else if (NewRight.ProcedureName <> '') then
          SingleSQL := SingleSQL + 'PROCEDURE ' + EscapeIdentifier(NewRight.DatabaseName) + '.' + EscapeIdentifier(NewRight.ProcedureName)
        else if (NewRight.FunctionName <> '') then
          SingleSQL := SingleSQL + 'FUNCTION ' + EscapeIdentifier(NewRight.DatabaseName) + '.' + EscapeIdentifier(NewRight.FunctionName)
        else if (NewRight.DatabaseName <> '') then
          SingleSQL := SingleSQL + EscapeIdentifier(NewRight.DatabaseName) + '.*'
        else
          SingleSQL := SingleSQL + '*.*';
        SingleSQL := SingleSQL + ' FROM ' + EscapeUser(NewUser.Name);
      SQL := SQL + SingleSQL + ';' + #13#10;
      end;
    end;

    Privileges := GetPrivileges(True, OldRight, NewRight, GetRightType(NewRight));

    Options := '';
    if (NewRight.RGrant and not (Assigned(OldRight) and OldRight.RGrant)) then Options := Options + ' GRANT OPTION';
    if (NewRight.DatabaseName = '') then
    begin
      if (not Assigned(User) and (NewUser.ConnectionsPerHour > 0) or Assigned(User) and (User.ConnectionsPerHour <> NewUser.ConnectionsPerHour)) then Options := Options + ' MAX_CONNECTIONS_PER_HOUR ' + IntToStr(NewUser.ConnectionsPerHour);
      if (not Assigned(User) and (NewUser.QueriesPerHour     > 0) or Assigned(User) and (User.QueriesPerHour     <> NewUser.QueriesPerHour    )) then Options := Options + ' MAX_QUERIES_PER_HOUR '     + IntToStr(NewUser.QueriesPerHour);
      if (not Assigned(User) and (NewUser.UpdatesPerHour     > 0) or Assigned(User) and (User.UpdatesPerHour     <> NewUser.UpdatesPerHour    )) then Options := Options + ' MAX_UPDATES_PER_HOUR '     + IntToStr(NewUser.UpdatesPerHour);
      if (ServerVersion >= 50003) then
        if (not Assigned(User) and (NewUser.UserConnections    > 0) or Assigned(User) and (User.UserConnections    <> NewUser.UserConnections   )) then Options := Options + ' MAX_USER_CONNECTIONS '     + IntToStr(NewUser.UserConnections);
    end;
    Options := Trim(Options);

    if ((Privileges = '') and ((Options <> '') or (not Assigned(User) and (ServerVersion <= 50002)))) then
      Privileges := 'USAGE';

    if (Privileges <> '') then
    begin
      SingleSQL := 'GRANT ' + Privileges + ' ON ';

      if (NewRight.TableName <> '') then
        if (ServerVersion < 50006) then
          SingleSQL := SingleSQL + EscapeIdentifier(NewRight.DatabaseName) + '.' + EscapeIdentifier(NewRight.TableName)
        else
          SingleSQL := SingleSQL + 'TABLE ' + EscapeIdentifier(NewRight.DatabaseName) + '.' + EscapeIdentifier(NewRight.TableName)
      else if (NewRight.ProcedureName <> '') then
        SingleSQL := SingleSQL + 'PROCEDURE ' + EscapeIdentifier(NewRight.DatabaseName) + '.' + EscapeIdentifier(NewRight.ProcedureName)
      else if (NewRight.FunctionName <> '') then
        SingleSQL := SingleSQL + 'FUNCTION ' + EscapeIdentifier(NewRight.DatabaseName) + '.' + EscapeIdentifier(NewRight.FunctionName)
      else if (NewRight.DatabaseName <> '') then
        SingleSQL := SingleSQL + EscapeIdentifier(NewRight.DatabaseName) + '.*'
      else
        SingleSQL := SingleSQL + '*.*';

      SingleSQL := SingleSQL + ' TO ' + EscapeUser(NewUser.Name);
      if (Options <> '') then
        SingleSQL := SingleSQL + ' WITH ' + Options;
      SQL := SQL + SingleSQL + ';' + #13#10;
    end;
  end;

  if (Assigned(User)) then
  begin
    for I := 0 to User.RightCount - 1 do
    begin
      OldRight := User.Right[I];
      NewRight := nil;
      if (Assigned(NewUser)) then
        for J := 0 to NewUser.RightCount - 1 do
          if   ((TableNameCmp(NewUser.Right[J].DatabaseName , OldRight.DatabaseName ) = 0)
            and (TableNameCmp(NewUser.Right[J].TableName    , OldRight.TableName    ) = 0)
            and (TableNameCmp(NewUser.Right[J].ProcedureName, OldRight.ProcedureName) = 0)
            and (TableNameCmp(NewUser.Right[J].FunctionName , OldRight.FunctionName ) = 0)
            and (lstrcmpi(PChar(NewUser.Right[J].FieldName), PChar(OldRight.FieldName)) = 0)) then
            NewRight := NewUser.Right[J];


      if (not Assigned(NewRight)) then
      begin
        RemovedUserRights[User.IndexOf(OldRight)] := True;

        EmptyRight := TSUserRight.Create();
        Privileges := GetPrivileges(False, OldRight, EmptyRight, GetRightType(OldRight));
        EmptyRight.Free();

        if (Privileges <> '') then
        begin
          SingleSQL := 'REVOKE ' + Privileges + ' ON ';
          if (OldRight.TableName <> '') then
            SingleSQL := SingleSQL + EscapeIdentifier(OldRight.DatabaseName) + '.' + EscapeIdentifier(OldRight.TableName)
          else if (OldRight.ProcedureName <> '') then
            SingleSQL := SingleSQL + 'PROCEDURE ' + EscapeIdentifier(OldRight.DatabaseName) + '.' + EscapeIdentifier(OldRight.ProcedureName)
          else if (OldRight.FunctionName <> '') then
            SingleSQL := SingleSQL + 'FUNCTION ' + EscapeIdentifier(OldRight.DatabaseName) + '.' + EscapeIdentifier(OldRight.FunctionName)
          else if (OldRight.DatabaseName <> '') then
            SingleSQL := SingleSQL + EscapeIdentifier(OldRight.DatabaseName) + '.*'
          else
            SingleSQL := SingleSQL + '*.*';
          SingleSQL := SingleSQL + ' FROM ' + EscapeUser(NewUser.Name);
          SQL := SQL + SingleSQL + ';' + #13#10;
        end;
      end;
    end;
  end;

  if (SQL <> '') then
    SQL := SQL + 'FLUSH PRIVILEGES;' + #13#10;

  if (not Assigned(User) and (NewUser.NewPassword <> '') or Assigned(User) and (NewUser.NewPassword <> User.RawPassword) and (NewUser.RightCount > 0)) then
    SQL := SQL + 'SET PASSWORD FOR ' + EscapeUser(NewUser.Name) + '=PASSWORD(' + SQLEscape(NewUser.NewPassword) + ');' + #13#10;

  Result := (SQL = '') or ExecuteSQL(SQL);
end;

function TSSession.UpdateVariable(const Variable, NewVariable: TSVariable; const UpdateModes: TSVariable.TUpdateModes): Boolean;
var
  I: Integer;
  SQL: string;
begin
  SQL := 'SET ';
  if (vuGlobal in UpdateModes) then
    SQL := SQL + 'GLOBAL '
  else if (vuSession in UpdateModes) then
    SQL := SQL + 'SESSION ';
  if (TryStrToInt(NewVariable.Value, I)) then
    SQL := SQL + Variable.Name + '=' + NewVariable.Value + ';' + #13#10
  else
    SQL := SQL + Variable.Name + '=' + SQLEscape(NewVariable.Value) + ';' + #13#10;

  SQL := SQL + Variables.SQLGetItems(Variable.Name);

  Result := (SQL = '') or SendSQL(SQL, SessionResult);
end;

function TSSession.UserByCaption(const Caption: string): TSUser;
begin
  if (Caption = '<' + Preferences.LoadStr(287) + '>') then
    Result := UserByName('')
  else
    Result := UserByName(Caption);
end;

function TSSession.UserByName(const UserName: string): TSUser;
var
  Index: Integer;
begin
  Index := Users.IndexByName(UserName);
  if (Index < 0) then
    Result := nil
  else
    Result := Users[Index];
end;

function TSSession.VariableByName(const VariableName: string): TSVariable;
var
  Index: Integer;
begin
  Index := Variables.IndexByName(VariableName);
  if (Index < 0) then
    Result := nil
  else
    Result := Variables[Index];
end;

{ TSSessions ******************************************************************}

function TSSessions.Add(const Session: TSSession): Integer;
begin
  Result := inherited Add(Session);

  Session.OnSQLError := OnSQLError;
end;

function TSSessions.SessionByAccount(const Account: TAAccount; const DatabaseName: string): TSSession;
var
  I: Integer;
begin
  Result := nil;

  for I := 0 to Count - 1 do
    if (Sessions[I].Account = Account) and (Sessions[I].Databases.NameCmp(Session[I].DatabaseName, DatabaseName) = 0) then
      Result := Sessions[I];

  if (not Assigned(Result)) then
    for I := 0 to Count - 1 do
      if (Sessions[I].Account = Account) then
        Result := Sessions[I];
end;

function TSSessions.GetSession(Index: Integer): TSSession;
begin
  Result := TSSession(Items[Index]);
end;

{ *****************************************************************************}

initialization
  Sessions := TSSessions.Create();
finalization
  Sessions.Free();
end.

