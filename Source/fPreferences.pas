unit fPreferences;

interface {********************************************************************}

uses
  Controls, Forms, Graphics, Windows, XMLDoc, XMLIntf, Classes, SysUtils,
  Registry, IniFiles, ComCtrls,
  TaskSchd,
  SynEditHighlighter, SynHighlighterSQL,
  MySQLDB;

type
  TPAccounts = class;

  TPPreferences = class(TRegistry)
  type
    TItems = class;

    TDelimiterType = (dtTab, dtChar);
    TNodeType = (ntDisabled, ntName, ntCustom);
    TStmtType = (stInsert, stReplace, stUpdate, stInsertOrUpdate);
    TQuotingType = (qtNone, qtStrings, qtAll);

    TItem = class
    private
      FName: string;
    protected
      FAItems: TItems;
      function GetIndex(): Integer; inline;
      procedure LoadFromXML(const XML: IXMLNode); virtual; abstract;
      procedure SaveToXML(const XML: IXMLNode); virtual; abstract;
    public
      procedure Assign(const Source: TItem); virtual;
      constructor Create(const AAItems: TItems; const AName: string = '');
      property AItems: TItems read FAItems;
      property Index: Integer read GetIndex;
      property Name: string read FName write FName;
    end;

    TItems = class(TList)
    private
      function GetItem(Index: Integer): TItem; inline;
    protected
      function InsertIndex(const Name: string; out Index: Integer): Boolean; virtual;
    public
      function Add(const AItem: TItem): Integer; overload; virtual;
      procedure Clear(); override;
      function IndexByName(const Name: string): Integer; virtual;
      property Item[Index: Integer]: TItem read GetItem; default;
    end;

    TMRUList = class
    private
      FMaxCount: Integer;
      FValues: array of string;
      function GetCount(): Integer;
      function GetValue(Index: Integer): string;
    public
      property Count: Integer read GetCount;
      property MaxCount: Integer read FMaxCount;
      property Values[Index: Integer]: string read GetValue; default;
      procedure Add(const Value: string); virtual;
      procedure Assign(const Source: TMRUList); virtual;
      procedure Clear(); virtual;
      constructor Create(const AMaxCount: Integer); virtual;
      procedure Delete(const Index: Integer);
      destructor Destroy(); override;
      function IndexOf(const Value: string): Integer; virtual;
      procedure LoadFromXML(const XML: IXMLNode; const NodeName: string); virtual;
      procedure SaveToXML(const XML: IXMLNode; const NodeName: string); virtual;
    end;

    TWindow = class(TItem)
    protected
      procedure LoadFromXML(const XML: IXMLNode); override;
      procedure SaveToXML(const XML: IXMLNode); override;
    public
      Height: Integer;
      Width: Integer;
      constructor Create(); virtual;
    end;

    TDatabase = class(TWindow);

    TDatabases = class(TWindow)
      Left: Integer;
      Top: Integer;
    end;

    TEditor = class
    protected
      procedure LoadFromXML(const XML: IXMLNode); virtual;
      procedure SaveToXML(const XML: IXMLNode); virtual;
    public
      CodeCompletion: Boolean;
      CodeCompletionTime: Integer;
      ConditionalCommentForeground, ConditionalCommentBackground: TColor;
      ConditionalCommentStyle: TFontStyles;
      CommentForeground, CommentBackground: TColor;
      CommentStyle: TFontStyles;
      CurrRowBGColorEnabled: Boolean;
      CurrRowBGColor: TColor;
      DataTypeForeground, DataTypeBackground: TColor;
      DataTypeStyle: TFontStyles;
      FunctionForeground, FunctionBackground: TColor;
      FunctionStyle: TFontStyles;
      IdentifierForeground, IdentifierBackground: TColor;
      IdentifierStyle: TFontStyles;
      KeywordForeground, KeywordBackground: TColor;
      KeywordStyle: TFontStyles;
      NumberForeground, NumberBackground: TColor;
      NumberStyle: TFontStyles;
      LineNumbersForeground, LineNumbersBackground: TColor;
      LineNumbersStyle: TFontStyles;
      StringForeground, StringBackground: TColor;
      StringStyle: TFontStyles;
      SymbolForeground, SymbolBackground: TColor;
      SymbolStyle: TFontStyles;
      VariableForeground, VariableBackground: TColor;
      VariableStyle: TFontStyles;
      constructor Create(); virtual;
    end;

    TEvent = class(TWindow);

    TExport = class(TWindow)
    protected
      procedure LoadFromXML(const XML: IXMLNode); override;
      procedure SaveToXML(const XML: IXMLNode); override;
    public
      CSV: record
        Headline: Boolean;
        QuoteValues: TQuotingType;
        Quoter: Char;
        Delimiter: string;
        DelimiterType: TDelimiterType;
      end;
      Excel: record
        Excel2003: Boolean;
      end;
      Access: record
        Access2003: Boolean;
      end;
      HTML: record
        Data: Boolean;
        NULLText: Boolean;
        MemoContent: Boolean;
        RowBGColor: Boolean;
        Structure: Boolean;
      end;
      ODBC: record
        DataSource: string;
      end;
      SQL: record
        Data: Boolean;
        DropStmts: Boolean;
        ReplaceData: Boolean;
        Structure: Boolean;
      end;
      XML: record
        Database: record
          NodeType: TNodeType;
          NodeText: string;
          NodeAttribute: string;
        end;
        Field: record
          NodeType: TNodeType;
          NodeText: string;
          NodeAttribute: string;
        end;
        Row: record
          NodeText: string;
        end;
        Root: record
          NodeText: string;
        end;
        Table: record
          NodeType: TNodeType;
          NodeText: string;
          NodeAttribute: string;
        end;
      end;
      procedure Assign(const Source: TItem); override;
      constructor Create(const AAItems: TItems = nil; const AName: string = ''); reintroduce; virtual;
    end;

    TField = class(TWindow);

    TFind = class(TWindow)
    type
      TOption = (foMatchCase, foWholeValue, foRegExpr);
      TOptions = set of TOption;
    protected
      procedure LoadFromXML(const XML: IXMLNode); override;
      procedure SaveToXML(const XML: IXMLNode); override;
    public
      FindTextMRU: TMRUList;
      Left: Integer;
      Options: TOptions;
      Top: Integer;
      constructor Create(); override;
      destructor Destroy(); override;
    end;

    THost = class(TWindow);

    TForeignKey = class(TWindow);

    TImport = class(TWindow)
    protected
      procedure LoadFromXML(const XML: IXMLNode); override;
      procedure SaveToXML(const XML: IXMLNode); override;
    public
      CSV: record
        Headline: Boolean;
        Quote: TQuotingType;
        QuoteChar: string;
        Delimiter: string;
        DelimiterType: TDelimiterType;
      end;
      Charset: string;
      Collation: string;
      Data: Boolean;
      Engine: string;
      RowType: Integer;
      StmtType: TStmtType;
      Structure: Boolean;
      procedure Assign(const Source: TItem); override;
      constructor Create(const AAItems: TItems = nil; const AName: string = ''); reintroduce; virtual;
    end;

    TKey = class(TWindow);

    TODBC = class(TWindow)
    public
      Left: Integer;
      Top: Integer;
      constructor Create(); override;
    end;

    TPaste = class
    private
      FPreferences: TPPreferences;
    protected
      property Preferences: TPPreferences read FPreferences;
      procedure LoadFromXML(const XML: IXMLNode); virtual;
      procedure SaveToXML(const XML: IXMLNode); virtual;
    public
      Data: Boolean;
      constructor Create(); virtual;
    end;

    TReplace = class(TWindow)
    type
      TOption = (roMatchCase, roWholeValue, roRegExpr);
      TOptions = set of TOption;
    protected
      procedure LoadFromXML(const XML: IXMLNode); override;
      procedure SaveToXML(const XML: IXMLNode); override;
    public
      FindTextMRU: TMRUList;
      ReplaceTextMRU: TMRUList;
      Left: Integer;
      Options: TOptions;
      Top: Integer;
      constructor Create(); override;
      destructor Destroy(); override;
    end;

    TRoutine = class(TWindow);

    TServer = class(TWindow);

    TSQLHelp = class(TWindow)
    protected
      procedure LoadFromXML(const XML: IXMLNode); override;
      procedure SaveToXML(const XML: IXMLNode); override;
    public
      Left: Integer;
      Top: Integer;
      constructor Create(); override;
    end;

    TAccount = class(TWindow);

    TAccounts = class(TWindow)
    protected
      procedure LoadFromXML(const XML: IXMLNode); override;
      procedure SaveToXML(const XML: IXMLNode); override;
    public
      SelectOrder: Integer;
      constructor Create(); override;
    end;

    TStatement = class(TWindow);

    TTable = class(TWindow);

    TTableService = class(TWindow)
    protected
      procedure LoadFromXML(const XML: IXMLNode); override;
      procedure SaveToXML(const XML: IXMLNode); override;
    public
      Analyze: Boolean;
      Check: Boolean;
      Flush: Boolean;
      Optimize: Boolean;
      Repair: Boolean;
      constructor Create(); override;
    end;

    TTransfer = class(TWindow)
    protected
      procedure LoadFromXML(const XML: IXMLNode); override;
      procedure SaveToXML(const XML: IXMLNode); override;
    public
      Data: Boolean;
      Structure: Boolean;
      constructor Create(); override;
    end;

    TTrigger = class(TWindow);

    TUser = class(TWindow);

    TPView = class(TWindow);

    TLanguage = class(TMemIniFile)
    private
      FActiveQueryBuilderLanguageName: string;
      FLanguageId: Integer;
      FStrs: array of string;
      function GetStr(Index: Integer): string;
    protected
      property Strs[Index: Integer]: string read GetStr;
    public
      property ActiveQueryBuilderLanguageName: string read FActiveQueryBuilderLanguageName;
      property LanguageId: Integer read FLanguageId;
      constructor Create(const FileName: string); reintroduce;
      destructor Destroy(); override;
    end;

    TToolbarTab = (ttObjects, ttBrowser, ttIDE, ttBuilder, ttDiagram, ttEditor, ttEditor2, ttEditor3);
    TToolbarTabs = set of TToolbarTab;
    TUpdateCheckType = (utNever, utDaily);
  private
    FInternetAgent: string;
    FLanguage: TLanguage;
    FLargeImages: TImageList;
    FSmallImages: TImageList;
    FTaskService: ITaskService;
    FVerMajor, FVerMinor, FVerPatch, FVerBuild: Integer;
    FXMLDocument: IXMLDocument;
    OldAssociateSQL: Boolean;
    procedure LoadFromRegistry();
    function GetFilename(): TFileName;
    function GetLanguage(): TLanguage;
    function GetLanguagePath(): TFileName;
    function GetTaskService(): ITaskService;
    function GetVersion(var VerMajor, VerMinor, VerPatch, VerBuild: Integer): Boolean;
    function GetVersionInfo(): Integer;
    function GetVersionStr(): string;
    function GetXMLDocument(): IXMLDocument;
  protected
    KeyBase: string;
    procedure LoadFromXML(const XML: IXMLNode); virtual;
    procedure SaveToRegistry(); virtual;
    procedure SaveToXML(const XML: IXMLNode); virtual;
    property Filename: TFileName read GetFilename;
    property TaskService: ITaskService read GetTaskService;
    property XMLDocument: IXMLDocument read GetXMLDocument;
  public
    Account: TAccount;
    Accounts: TAccounts;
    AssociateSQL: Boolean;
    Database: TDatabase;
    Databases: TDatabases;
    Editor: TEditor;
    Event: TEvent;
    Export: TExport;
    Field: TField;
    Find: TFind;
    ForeignKey: TForeignKey;
    GridCurrRowBGColor: TColor;
    GridCurrRowBGColorEnabled: Boolean;
    GridDefaultSorting: Boolean;
    GridFontColor: TColor;
    GridFontName: TFontName;
    GridFontSize, GridFontCharset: Integer;
    GridFontStyle: TFontStyles;
    GridMaxColumnWidth: Integer;
    GridMemoContent: Boolean;
    GridNullBGColor: TColor;
    GridNullBGColorEnabled: Boolean;
    GridNullText: Boolean;
    GridRowBGColor: Boolean;
    Height: Integer;
    Host: THost;
    Import: TImport;
    Key: TKey;
    LanguageFilename: TFileName;
    Left: Integer;
    LogFontColor: TColor;
    LogFontName: TFontName;
    LogFontSize, LogFontCharset: Integer;
    LogFontStyle: TFontStyles;
    LogHighlighting: Boolean;
    LogResult: Boolean;
    LogSize: Integer;
    LogTime: Boolean;
    ODBC: TODBC;
    Paste: TPaste;
    Path: TFileName;
    Replace: TReplace;
    Routine: TRoutine;
    Server: TServer;
    SetupProgram: TFileName;
    SetupProgramExecute: Boolean;
    SetupProgramInstalled: Boolean;
    SoundFileNavigating: string;
    SQLFontCharset: Integer;
    SQLFontColor: TColor;
    SQLFontName: TFontName;
    SQLFontSize: Integer;
    SQLFontStyle: TFontStyles;
    SQLHelp: TSQLHelp;
    Statement: TStatement;
    Table: TTable;
    TableService: TTableService;
    TabsVisible: Boolean;
    ToolbarTabs: TToolbarTabs;
    Top: Integer;
    Transfer: TTransfer;
    Trigger: TTrigger;
    User: TUser;
    View: TPView;
    Width: Integer;
    WindowState: TWindowState;
    UpdateCheck: TUpdateCheckType;
    UpdateChecked: TDateTime;
    UserPath: TFileName;
    constructor Create(); virtual;
    destructor Destroy(); override;
    function LoadStr(const Index: Integer; const Param1: string = ''; const Param2: string = ''; const Param3: string = ''): string; overload; virtual;
    procedure Save(); virtual;
    property InternetAgent: string read FInternetAgent;
    property Language: TLanguage read GetLanguage;
    property LanguagePath: TFileName read GetLanguagePath;
    property LargeImages: TImageList read FLargeImages;
    property SmallImages: TImageList read FSmallImages;
    property VerMajor: Integer read FVerMajor;
    property VerMinor: Integer read FVerMinor;
    property VerPatch: Integer read FVerPatch;
    property VerBuild: Integer read FVerBuild;
    property Version: Integer read GetVersionInfo;
    property VersionStr: string read GetVersionStr;
  end;

  TPAccount = class
  type
    TJobs = class;
    TFiles = class;
    TDesktop = class;

    TEventProc = procedure (const ClassType: TClass) of object;

    TJob = class
    type
      TObjectType = (jotServer, jotDatabase, jotTable, jotProcedure, jotFunction, jotTrigger, jotEvent);
      TJobObject = record
        DatabaseName: string;
        Name: string;
        ObjectType: TObjectType;
      end;
      TSourceObject = record
        Name: string;
      end;
      TFieldMapping = record
        DestinationFieldName: string;
        SourceFieldName: string;
      end;
      TTriggerType = (ttSingle, ttDaily, ttWeekly, ttMonthly);
    private
      FJobs: TJobs;
      FName: string;
      function GetLogFilename(): TFileName;
    protected
      procedure LoadFromXML(const XML: IXMLNode); virtual; abstract;
      procedure SaveToXML(const XML: IXMLNode); virtual; abstract;
      function Save(const Update: Boolean): Boolean; virtual;
      property Jobs: TJobs read FJobs;
    public
      Enabled: Boolean;
      Start: TDateTime;
      TriggerType: TTriggerType;
      procedure Assign(const Source: TJob); virtual;
      constructor Create(const AJobs: TJobs; const AName: string = ''); reintroduce;
      property LogFilename: TFileName read GetLogFilename;
      property Name: string read FName;
    end;

    TJobs = class(TList)
    private
      FAccount: TPAccount;
      function GetJob(Index: Integer): TJob; inline;
      function GetTaskFolder(const AutoCreate: Boolean = False): ITaskFolder;
      function GetTaskService(): ITaskService; inline;
    protected
      function IndexByName(const Name: string): Integer; virtual;
      property Account: TPAccount read FAccount;
      property TaskService: ITaskService read GetTaskService;
    public
      function AddJob(const NewJob: TJob): Boolean; virtual;
      procedure Clear(); override;
      constructor Create(const AAccount: TPAccount);
      procedure DeleteJob(const Job: TJob); overload; virtual;
      destructor Destroy(); override;
      procedure Load(); virtual;
      function UpdateJob(const Job, NewJob: TJob): Boolean; virtual;
      property Job[Index: Integer]: TJob read GetJob; default;
    end;

    TFile = class
    private
      FFiles: TFiles;
    protected
      FCodePage: Cardinal;
      FFilename: TFileName;
      procedure LoadFromXML(const XML: IXMLNode); virtual;
      procedure SaveToXML(const XML: IXMLNode); virtual;
      property Files: TFiles read FFiles;
    public
      constructor Create(const AFiles: TFiles);
      property CodePage: Cardinal read FCodePage write FCodePage;
      property Filename: TFileName read FFilename write FFilename;
    end;

    TFiles = class(TList)
    private
      FDesktop: TDesktop;
      FMaxCount: Integer;
      function GetFile(Index: Integer): TFile; inline;
    protected
      procedure LoadFromXML(const XML: IXMLNode); virtual;
      procedure SaveToXML(const XML: IXMLNode); virtual;
      property Desktop: TDesktop read FDesktop;
      property MaxCount: Integer read FMaxCount;
    public
      procedure Add(const AFilename: TFileName; const ACodePage: Cardinal); reintroduce; virtual;
      procedure Clear(); override;
      constructor Create(const ADesktop: TDesktop; const AMaxCount: Integer);
      property Files[Index: Integer]: TFile read GetFile; default;
    end;

    TJobImport = class(TJob)
    type
      TImportType = (itUnknown, itSQLFile, itTextFile, itAccessFile, itExcelFile, itODBC);
    protected
      procedure LoadFromXML(const XML: IXMLNode); override;
      procedure SaveToXML(const XML: IXMLNode); override;
    public
      CSV: record
        Headline: Boolean;
        Quote: TPPreferences.TQuotingType;
        QuoteChar: string;
        Delimiter: string;
        DelimiterType: TPPreferences.TDelimiterType;
      end;
      Charset: string;
      CodePage: Integer;
      Collation: string;
      Data: Boolean;
      Engine: string;
      ImportType: TImportType;
      JobObject: TJob.TJobObject;
      FieldMappings: array of TJob.TFieldMapping;
      Filename: TFileName;
      ODBC: record
        DataSource: string;
        Username: string;
        Password: string;
      end;
      RowType: Integer;
      SourceObjects: array of TJob.TSourceObject;
      StmtType: TPPreferences.TStmtType;
      Structure: Boolean;
      procedure Assign(const Source: TJob); override;
      constructor Create(const AJobs: TJobs = nil; const AName: string = '');
      destructor Destroy(); override;
    end;

    TJobExport = class(TJob)
    type
      TExportType = (etUnknown, etSQLFile, etTextFile, etExcelFile, etAccessFile, etODBC, etHTMLFile, etXMLFile, etPDFFile, etPrinter);
    protected
      procedure LoadFromXML(const XML: IXMLNode); override;
      procedure SaveToXML(const XML: IXMLNode); override;
    public
      CodePage: Integer;
      CSV: record
        Headline: Boolean;
        QuoteValues: TPPreferences.TQuotingType;
        Quoter: Char;
        Delimiter: string;
        DelimiterType: TPPreferences.TDelimiterType;
      end;
      Excel: record
        Excel2007: Boolean;
      end;
      Access: record
        Access2003: Boolean;
      end;
      ExportType: TExportType;
      HTML: record
        Data: Boolean;
        NULLText: Boolean;
        MemoContent: Boolean;
        RowBGColor: Boolean;
        Structure: Boolean;
      end;
      Filename: TFileName;
      JobObjects: array of TJob.TJobObject;
      ODBC: record
        DataSource: string;
        Password: string;
        Username: string;
      end;
      SQL: record
        Data: Boolean;
        DropStmts: Boolean;
        ReplaceData: Boolean;
        Structure: Boolean;
      end;
      XML: record
        Database: record
          NodeType: TPPreferences.TNodeType;
          NodeText: string;
          NodeAttribute: string;
        end;
        Field: record
          NodeType: TPPreferences.TNodeType;
          NodeText: string;
          NodeAttribute: string;
        end;
        Row: record
          NodeText: string;
        end;
        Root: record
          NodeText: string;
        end;
        Table: record
          NodeType: TPPreferences.TNodeType;
          NodeText: string;
          NodeAttribute: string;
        end;
      end;
      procedure Assign(const Source: TJob); override;
      procedure ClearObjects(); virtual;
      constructor Create(const AJobs: TJobs = nil; const AName: string = '');
      destructor Destroy(); override;
    end;

    TDesktop = class
    type
      TListViewKind = (lkServer, lkDatabase, lkTable, lkProcesses, lkStati, lkUsers, lkVariables);
    private
      FAccount: TPAccount;
      FAddressMRU: TPPreferences.TMRUList;
      FFiles: TFiles;
      FPath: string;
      function GetAddress(): string;
      procedure SetAddress(AAddress: string); inline;
    protected
      procedure Assign(const Source: TDesktop); virtual;
      procedure LoadFromXML(const XML: IXMLNode); virtual;
      procedure SaveToXML(const XML: IXMLNode); virtual;
      property Account: TPAccount read FAccount;
    public
      BlobHeight: Integer;
      ColumnWidths: array [lkServer .. lkVariables] of array [0..7] of Integer;
      DataHeight: Integer;
      EditorContent: array [ttEditor .. ttEditor3] of string;
      ExplorerVisible: Boolean;
      FilesFilter: string;
      FoldersHeight: Integer;
      JobsVisible: Boolean;
      LogHeight: Integer;
      LogVisible: Boolean;
      NavigatorVisible: Boolean;
      SidebarWitdth: Integer;
      SQLHistoryVisible: Boolean;
      constructor Create(const AAccount: TPAccount); overload; virtual;
      destructor Destroy(); override;
      property Address: string read GetAddress write SetAddress;
      property AddressMRU: TPPreferences.TMRUList read FAddressMRU;
      property Files: TFiles read FFiles;
    end;

    TConnection = class
    protected
      Section: string;
      procedure LoadFromXML(const XML: IXMLNode); virtual;
      procedure SaveToXML(const XML: IXMLNode); virtual;
    public
      Database: string;
      Host: string;
      HTTPTunnelURI: string;
      LibraryFilename: TFileName;
      LibraryType: TMySQLLibrary.TLibraryType;
      Password: string;
      Port: Integer;
      Username: string;
      procedure Assign(const Source: TConnection); virtual;
      constructor Create(); virtual;
    end;

  private
    DesktopXMLDocument: IXMLDocument;
    FAccounts: TPAccounts;
    FDesktop: TDesktop;
    FDesktops: array of record Control: Pointer; AccountEventProc: TEventProc; end;
    FHistoryXMLDocument: IXMLDocument;
    FJobs: TJobs;
    FLastLogin: TDateTime;
    FName: string;
    FXML: IXMLNode;
    Modified: Boolean;
    function GetDataPath(): TFileName;
    function GetDesktopCount(): Integer;
    function GetDesktopFilename(): TFileName;
    function GetDesktopXML(): IXMLNode;
    function GetHistoryFilename(): TFileName;
    function GetHistoryXML(): IXMLNode;
    function GetJobs(): TJobs;
    function GetName(): string;
    procedure SetLastLogin(const ALastLogin: TDateTime);
  protected
    Section: string;
    function GetIndex(): Integer;
    procedure Load(); virtual;
    procedure Save(); virtual;
    procedure AccountEvent(const ClassType: TClass); virtual;
    function ValidDatabaseName(const ADatabaseName: string): Boolean;
    property DesktopFilename: TFileName read GetDesktopFilename;
    property HistoryFilename: TFileName read GetHistoryFilename;
    property HistoryXMLDocument: IXMLDocument read FHistoryXMLDocument;
    property XML: IXMLNode read FXML;
  public
    Connection: TConnection;
    ManualURL: string;
    ManualURLFetched: Boolean;
    procedure Assign(const Source: TPAccount); virtual;
    constructor Create(const AAccounts: TPAccounts; const AXML: IXMLNode = nil); virtual;
    destructor Destroy(); override;
    function ExtractPath(const AAddress: string): string; virtual;
    function ExpandAddress(const APath: string): string; virtual;
    function Frame(): Pointer; virtual;
    function GetDefaultDatabase(): string; virtual;
    function JobByName(const Name: string): TJob; virtual;
    procedure RegisterDesktop(const AControl: Pointer; const AEventProc: TEventProc); virtual;
    procedure UnRegisterDesktop(const AControl: Pointer); virtual;
    property Accounts: TPAccounts read FAccounts;
    property DataPath: TFileName read GetDataPath;
    property Desktop: TDesktop read FDesktop;
    property DesktopCount: Integer read GetDesktopCount;
    property DesktopXML: IXMLNode read GetDesktopXML;
    property HistoryXML: IXMLNode read GetHistoryXML;
    property Index: Integer read GetIndex;
    property Jobs: TJobs read GetJobs;
    property LastLogin: TDateTime read FLastLogin write SetLastLogin;
    property Name: string read GetName write FName;
  end;

  TPAccounts = class(TList)
  type
    TDBLogin = function(const Account: Pointer): Boolean of object;
  private
    DefaultAccountName: string;
    FDBLogin: TDBLogin;
    FXMLDocument: IXMLDocument;
    function GetDataPath(): TFileName;
    function GetDefault(): TPAccount; inline;
    function GetFilename(): TFileName;
    function GetXMLDocument(): IXMLDocument;
    function GetFAccounts(Index: Integer): TPAccount; inline;
    procedure SetDefault(const AAccount: TPAccount);
  protected
    Section: string;
    procedure Load(); virtual;
    property DataPath: TFileName read GetDataPath;
    property Filename: TFileName read GetFilename;
    property XMLDocument: IXMLDocument read GetXMLDocument;
  public
    function AccountByName(const AccountName: string): TPAccount; virtual;
    function AccountByURI(const AURI: string; const DefaultAccount: TPAccount = nil): TPAccount; virtual;
    procedure AddAccount(const NewAccount: TPAccount); virtual;
    procedure Clear(); override;
    constructor Create(const ADBLogin: TDBLogin);
    function DeleteAccount(const AAccount: TPAccount): Boolean; virtual;
    destructor Destroy(); override;
    procedure Save(); virtual;
    procedure UpdateAccount(const Account, NewAccount: TPAccount); virtual;
    property Account[Index: Integer]: TPAccount read GetFAccounts; default;
    property Default: TPAccount read GetDefault write SetDefault;
    property DBLogin: TDBLogin read FDBLogin;
  end;

function EncodeVersion(const AMajor, AMinor, APatch, ABuild: Integer): Integer;
function XMLNode(const XML: IXMLNode; const Path: string; const NodeAutoCreate: Boolean = False): IXMLNode; overload;

function IsConnectedToInternet(): Boolean;
function VersionStrToVersion(VersionStr: string): Integer;
function CopyDir(const fromDir, toDir: string): Boolean;
function HostIsLocalhost(const Host: string): Boolean;
function ValidJobName(const Name: string): Boolean;
function TrySplitParam(const Param: string; out Name, Value: string): Boolean;

var
  Accounts: TPAccounts;
  FileFormatSettings: TFormatSettings;
  MainHighlighter: TSynSQLSyn;
  Preferences: TPPreferences;

implementation {***************************************************************}

uses
  Consts, CommCtrl, SHFolder, WinInet, ShellAPI, ImgList, ShlObj, StrUtils,
  Variants, Math, SysConst, ActiveX, RTLConsts,
  MySQLConsts,
  CSVUtils,
  fURI;

const
  INTERNET_CONNECTION_CONFIGURED = $40;

function VersionStrToVersion(VersionStr: string): Integer;
begin
  if (Pos('.', VersionStr) = 0) then
    Result := StrToInt(VersionStr) * 10000
  else
    Result := StrToInt(copy(VersionStr, 1, Pos('.', VersionStr) - 1)) * 10000
      + VersionStrToVersion(Copy(VersionStr, Pos('.', VersionStr) + 1, Length(VersionStr) - Pos('.', VersionStr))) div 100;
end;

function IsConnectedToInternet(): Boolean;
var
  ConnectionTypes: DWord;
begin
  ConnectionTypes := INTERNET_CONNECTION_MODEM or INTERNET_CONNECTION_LAN or INTERNET_CONNECTION_PROXY or INTERNET_CONNECTION_CONFIGURED;
  Result := InternetGetConnectedState(@ConnectionTypes, 0);
end;

function CopyDir(const fromDir, toDir: string): Boolean;
var
  fos: TSHFileOpStruct;
begin
  ZeroMemory(@fos, SizeOf(fos));
  with fos do
  begin
    wFunc  := FO_COPY;
    fFlags := FOF_FILESONLY;
    pFrom  := PChar(fromDir + #0);
    pTo    := PChar(toDir)
  end;
  Result := (0 = ShFileOperation(fos));
end;

function TryStrToWindowState(const Str: string; var WindowState: TWindowState): Boolean;
begin
  Result := True;
  if (UpperCase(Str) = 'NORMAL') then WindowState := wsNormal
  else if (UpperCase(Str) = 'MINIMIZED') then WindowState := wsMinimized
  else if (UpperCase(Str) = 'MAXIMIZED') then WindowState := wsMaximized
  else Result := False;
end;

function WindowStateToStr(const WindowState: TWindowState): string;
begin
  case (WindowState) of
    wsMaximized: Result := 'Maximized';
    else Result := 'Normal';
  end;
end;

function TryStrToQuote(const Str: string; var Quote: TPPreferences.TQuotingType): Boolean;
begin
  Result := True;
  if (UpperCase(Str) = 'NOTHING') then Quote := qtNone
  else if (UpperCase(Str) = 'STRINGS') then Quote := qtStrings
  else if (UpperCase(Str) = 'ALL') then Quote := qtAll
  else Result := False;
end;

function QuoteToStr(const Quote: TPPreferences.TQuotingType): string;
begin
  case Quote of
    qtStrings: Result := 'Stings';
    qtAll: Result := 'All';
    else Result := 'Nothing';
  end;
end;

function TryStrToSeparatorType(const Str: string; var SeparatorType: TPPreferences.TDelimiterType): Boolean;
begin
  Result := True;
  if (UpperCase(Str) = 'STANDARD') then SeparatorType := dtChar
  else if (UpperCase(Str) = 'TAB') then SeparatorType := dtTab
  else Result := False;
end;

function SeparatorTypeToStr(const SeparatorType: TPPreferences.TDelimiterType): string;
begin
  case (SeparatorType) of
    dtTab: Result := 'Tab';
    else Result := 'Standard';
  end;
end;

function TryStrToNodeType(const Str: string; var NodeType: TPPreferences.TNodeType): Boolean;
begin
  Result := True;
  if (UpperCase(Str) = 'DISABLED') then NodeType := ntDisabled
  else if (UpperCase(Str) = 'NAME') then NodeType := ntName
  else if (UpperCase(Str) = 'CUSTOM') then NodeType := ntCustom
  else Result := False;
end;

function NodeTypeToStr(const NodeType: TPPreferences.TNodeType): string;
begin
  case (NodeType) of
    ntDisabled: Result := 'Disabled';
    ntCustom: Result := 'Custom';
    else Result := 'Name';
  end;
end;

function TryStrToStmtType(const Str: string; var StmtType: TPPreferences.TStmtType): Boolean;
begin
  Result := True;
  if (UpperCase(Str) = 'INSERT') then StmtType := stInsert
  else if (UpperCase(Str) = 'REPLACE') then StmtType := stReplace
  else if (UpperCase(Str) = 'UPDATE') then StmtType := stUpdate
  else if (UpperCase(Str) = 'INSERTUPDATE') then StmtType := stInsertOrUpdate
  else Result := False;
end;

function StmtTypeToStr(const StmtType: TPPreferences.TStmtType): string;
begin
  case (StmtType) of
    stReplace: Result := 'Replace';
    stUpdate: Result := 'Update';
    stInsertOrUpdate: Result := 'InsertUpdate';
    else Result := 'Insert';
  end;
end;

function StrToStyle(const Str: string): TFontStyles;
begin
  Result := [];
  if (Pos('BOLD', UpperCase(Str)) > 0) then Result := Result + [fsBold];
  if (Pos('ITALIC', UpperCase(Str)) > 0) then Result := Result + [fsItalic];
  if (Pos('UNDERLINE', UpperCase(Str)) > 0) then Result := Result + [fsUnderline];
  if (Pos('STRIKEOUT', UpperCase(Str)) > 0) then Result := Result + [fsStrikeOut];
end;

function StyleToStr(const Style: TFontStyles): string;
begin
  Result := '';

  if (fsBold in Style) then begin if (Result <> '') then Result := Result + ','; Result := Result + 'Bold'; end;
  if (fsItalic in Style) then begin if (Result <> '') then Result := Result + ','; Result := Result + 'Italic'; end;
  if (fsUnderline in Style) then begin if (Result <> '') then Result := Result + ','; Result := Result + 'Underline'; end;
  if (fsStrikeOut in Style) then begin if (Result <> '') then Result := Result + ','; Result := Result + 'StrikeOut'; end;
end;

function StrToFindOptions(const Str: string): TPPreferences.TFind.TOptions;
begin
  Result := [];
  if (Pos('MATCHCASE', UpperCase(Str)) > 0) then Result := Result + [foMatchCase];
  if (Pos('WHOLEWORD', UpperCase(Str)) > 0) then Result := Result + [foWholeValue];
  if (Pos('REGEXPR', UpperCase(Str)) > 0) then Result := Result + [foRegExpr];
end;

function FindOptionsToStr(const FindOptions: TPPreferences.TFind.TOptions): string;
begin
  Result := '';

  if (foMatchCase in FindOptions) then begin if (Result <> '') then Result := Result + ','; Result := Result + 'MatchCase'; end;
  if (foWholeValue in FindOptions) then begin if (Result <> '') then Result := Result + ','; Result := Result + 'WholeValue'; end;
  if (foRegExpr in FindOptions) then begin if (Result <> '') then Result := Result + ','; Result := Result + 'RegExpr'; end;
end;

function TryStrToUpdateCheck(const Str: string; var UpdateCheckType: TPPreferences.TUpdateCheckType): Boolean;
begin
  Result := True;
  if (UpperCase(Str) = 'NEVER') then UpdateCheckType := utNever
  else if (UpperCase(Str) = 'DAILY') then UpdateCheckType := utDaily
  else Result := False;
end;

function UpdateCheckToStr(const UpdateCheck: TPPreferences.TUpdateCheckType): string;
begin
  case (UpdateCheck) of
    utDaily: Result := 'Daily';
    else Result := 'Never';
  end;
end;

function TryStrToRowType(const Str: string; var RowType: Integer): Boolean;
begin
  Result := True;
  if (Str = '') then RowType := 0
  else if (UpperCase(Str) = 'FIXED') then RowType := 1
  else if (UpperCase(Str) = 'DYNAMIC') then RowType := 2
  else if (UpperCase(Str) = 'COMPRESSED') then RowType := 3
  else if (UpperCase(Str) = 'REDUNDANT') then RowType := 4
  else if (UpperCase(Str) = 'COMPACT') then RowType := 5
  else Result := False;
end;

function RowTypeToStr(const RowType: Integer): string;
begin
  case RowType of
    1: Result := 'Fixed';
    2: Result := 'Dynamic';
    3: Result := 'Compressed';
    4: Result := 'Redundant';
    5: Result := 'Compact';
    else Result := '';
  end;
end;

function EncodeVersion(const AMajor, AMinor, APatch, ABuild: Integer): Integer;
begin
  Result := AMajor * 100000000 + AMinor * 1000000 + APatch * 10000 + ABuild;
end;

function StrToReplaceOptions(const Str: string): TPPreferences.TReplace.TOptions;
begin
  Result := [];
  if (Pos('MATCHCASE', UpperCase(Str)) > 0) then Result := Result + [roMatchCase];
  if (Pos('WHOLEWORD', UpperCase(Str)) > 0) then Result := Result + [roWholeValue];
  if (Pos('REGEXPR', UpperCase(Str)) > 0) then Result := Result + [roRegExpr];
end;

function ReplaceOptionsToStr(const Options: TPPreferences.TReplace.TOptions): string;
begin
  Result := '';

  if (roMatchCase in Options) then begin if (Result <> '') then Result := Result + ','; Result := Result + 'MatchCase'; end;
  if (roWholeValue in Options) then begin if (Result <> '') then Result := Result + ','; Result := Result + 'WholeValue'; end;
  if (roRegExpr in Options) then begin if (Result <> '') then Result := Result + ','; Result := Result + 'RegExpr'; end;
end;

function TryStrToObjectType(const Str: string; var ObjectType: TPAccount.TJob.TObjectType): Boolean;
begin
  Result := True;
  if (UpperCase(Str) = 'SERVER') then ObjectType := jotDatabase
  else if (UpperCase(Str) = 'DATABASE') then ObjectType := jotDatabase
  else if (UpperCase(Str) = 'TABLE') then ObjectType := jotTable
  else if (UpperCase(Str) = 'PROCEDURE') then ObjectType := jotProcedure
  else if (UpperCase(Str) = 'FUNCTION') then ObjectType := jotFunction
  else if (UpperCase(Str) = 'TRIGGER') then ObjectType := jotTrigger
  else if (UpperCase(Str) = 'EVENT') then ObjectType := jotEvent
  else Result := False;
end;

function ObjectTypeToStr(const ObjectType: TPAccount.TJob.TObjectType): string;
begin
  case (ObjectType) of
    jotServer: Result := 'Server';
    jotDatabase: Result := 'Database';
    jotTable: Result := 'Table';
    jotProcedure: Result := 'Procedure';
    jotFunction: Result := 'Function';
    jotTrigger: Result := 'Trigger';
    jotEvent: Result := 'Event';
    else raise ERangeError.CreateFmt(SPropertyOutOfRange, ['ObjectType']);
  end;
end;

function TryStrToImportType(const Str: string; var ImportType: TPAccount.TJobImport.TImportType): Boolean;
begin
  Result := True;
  if (UpperCase(Str) = 'SQLFILE') then ImportType := itSQLFile
  else if (UpperCase(Str) = 'TEXTFILE') then ImportType := itTextFile
  else if (UpperCase(Str) = 'EXCELFILE') then ImportType := itExcelFile
  else if (UpperCase(Str) = 'ACCESSFILE') then ImportType := itAccessFile
  else if (UpperCase(Str) = 'ODBC') then ImportType := itODBC
  else Result := False;
end;

function ImportTypeToStr(const ImportType: TPAccount.TJobImport.TImportType): string;
begin
  case (ImportType) of
    itSQLFile: Result := 'SQLFile';
    itTextFile: Result := 'TextFile';
    itExcelFile: Result := 'ExcelFile';
    itAccessFile: Result := 'AccessFile';
    itODBC: Result := 'ODBC';
    else raise ERangeError.CreateFmt(SPropertyOutOfRange, ['ImportType']);
  end;
end;

function TryStrToExportType(const Str: string; var ExportType: TPAccount.TJobExport.TExportType): Boolean;
begin
  Result := True;
  if (UpperCase(Str) = 'SQLFILE') then ExportType := etSQLFile
  else if (UpperCase(Str) = 'TEXTFILE') then ExportType := etTextFile
  else if (UpperCase(Str) = 'EXCELFILE') then ExportType := etExcelFile
  else if (UpperCase(Str) = 'ACCESSFILE') then ExportType := etAccessFile
  else if (UpperCase(Str) = 'ODBC') then ExportType := etODBC
  else if (UpperCase(Str) = 'HTMLFILE') then ExportType := etHTMLFile
  else if (UpperCase(Str) = 'XMLFILE') then ExportType := etXMLFile
  else if (UpperCase(Str) = 'PDFFILE') then ExportType := etPDFFile
  else if (UpperCase(Str) = 'PRINTER') then ExportType := etPrinter
  else Result := False;
end;

function ExportTypeToStr(const ExportType: TPAccount.TJobExport.TExportType): string;
begin
  case (ExportType) of
    etSQLFile: Result := 'SQLFile';
    etTextFile: Result := 'TextFile';
    etExcelFile: Result := 'ExcelFile';
    etAccessFile: Result := 'AccessFile';
    etODBC: Result := 'ODBC';
    etHTMLFile: Result := 'HTMLFile';
    etXMLFile: Result := 'XMLFile';
    etPDFFile: Result := 'PDFFile';
    etPrinter: Result := 'Printer';
    else raise ERangeError.CreateFmt(SPropertyOutOfRange, ['ExportType']);
  end;
end;

function ReplaceEnviromentVariables(const AStr: string): string;
var
  Len: Integer;
begin
  Len := ExpandEnvironmentStrings(PChar(AStr), nil, 0);
  SetLength(Result, Len);
  ExpandEnvironmentStrings(PChar(AStr), PChar(Result), Len);
  if (RightStr(Result, 1) = #0) then
    Delete(Result, Length(Result), 1);
end;

function XMLNode(const XML: IXMLNode; const Path: string; const NodeAutoCreate: Boolean = False): IXMLNode;
var
  ChildKey: string;
  CurrentKey: string;
begin
  if (not Assigned(XML)) then
    Result := nil
  else if (Path = '') then
    Result := XML
  else
  begin
    if (Pos('/', Path) = 0) then
    begin
      CurrentKey := Path;
      ChildKey := '';
    end
    else
    begin
      CurrentKey := Copy(Path, 1, Pos('/', Path) - 1);
      ChildKey := Path; Delete(ChildKey, 1, Length(CurrentKey) + 1);
    end;

    if (Assigned(XML.ChildNodes.FindNode(CurrentKey))) then
      Result := XMLNode(XML.ChildNodes.FindNode(CurrentKey), ChildKey, NodeAutoCreate)
    else if (NodeAutoCreate or (doNodeAutoCreate in XML.OwnerDocument.Options)) then
      Result := XMLNode(XML.AddChild(CurrentKey), ChildKey, NodeAutoCreate)
    else
      Result := nil;
  end;
end;

function GetFileIcon(const CSIDL: Integer): HIcon; overload;
var
  FileInfo: TSHFileInfo;
  PIDL: PItemIDList;
begin
  PIDL := nil;
  SHGetFolderLocation(Application.Handle, CSIDL, 0, 0, PIDL);
  ZeroMemory(@FileInfo, SizeOf(FileInfo));
  SHGetFileInfo(PChar(PIDL), 0, FileInfo, SizeOf(FileInfo), SHGFI_PIDL or SHGFI_ICON or SHGFI_SMALLICON);
  Result := FileInfo.hIcon;
end;

function GetFileIcon(const Path: TFileName): HIcon; overload;
var
  FileInfo: TSHFileInfo;
begin
  ZeroMemory(@FileInfo, SizeOf(FileInfo));
  SHGetFileInfo(PChar(Path), 0, FileInfo, SizeOf(FileInfo), SHGFI_ICON or SHGFI_SMALLICON);
  Result := FileInfo.hIcon;
end;

function ViewStyleToInteger(const ViewStyle: TViewStyle): Integer;
begin
  Result := 2;
  case (ViewStyle) of
    vsIcon: Result := 0;
    vsSmallIcon: Result := 1;
    vsList: Result := 2;
    vsReport: Result := 3;
  end;
end;

function TryStrToViewStyle(const Str: string; var ViewStyle: TViewStyle): Boolean;
begin
  Result := True;
  if (UpperCase(Str) = 'ICON') then ViewStyle := vsIcon
  else if (UpperCase(Str) = 'SMALLICON') then ViewStyle := vsSmallIcon
  else if (UpperCase(Str) = 'LIST') then ViewStyle := vsList
  else if (UpperCase(Str) = 'REPORT') then ViewStyle := vsReport
  else Result := False;
end;

function ViewStyleToStr(const ViewStyle: TViewStyle): string;
begin
  case (ViewStyle) of
    vsIcon: Result := 'Icon';
    vsList: Result := 'List';
    vsReport: Result := 'Report';
    else Result := 'SmallIcon';
  end;
end;

function IntegerToViewStyle(const Int: Integer): TViewStyle;
begin
  Result := vsList;
  case (Int) of
    0: Result := vsIcon;
    1: Result := vsSmallIcon;
    2: Result := vsList;
    3: Result := vsReport;
  end;
end;

function HostIsLocalhost(const Host: string): Boolean;
begin
  Result := (lstrcmpi(PChar(Host), PChar(LOCAL_HOST)) = 0) or (Host = '127.0.0.1') or (Host = '::1');
end;

function ValidJobName(const Name: string): Boolean;
begin
  Result := (Name <> '') and (Pos('.', Name) = 0) and (Pos('\', Name) = 0);
end;

function TrySplitParam(const Param: string; out Name, Value: string): Boolean;
begin
  Result := (Copy(Param, 1, 1) = '/') and (Pos('=', Param) > 2);
  if (True) then
  begin
    Name := Trim(Copy(Param, 2, Pos('=', Param) - 2));
    Value := Trim(Copy(Param, Pos('=', Param) + 1, Length(Param) - Pos('=', Param)));
  end;
end;

{ TPPreferences.TItem *********************************************************}

procedure TPPreferences.TItem.Assign(const Source: TItem);
begin
  Assert(Assigned(Source) and (Source.ClassType = ClassType));


  FName := Source.Name;
end;

constructor TPPreferences.TItem.Create(const AAItems: TItems; const AName: string = '');
begin
  inherited Create();

  FAItems := AAItems;
  FName := AName;
end;

function TPPreferences.TItem.GetIndex(): Integer;
begin
  Result := AItems.IndexOf(Self);
end;

{ TPPreferences.TItems ********************************************************}

function TPPreferences.TItems.Add(const AItem: TItem): Integer;
begin
  if (InsertIndex(AItem.Name, Result)) then
    if (Result < TList(Self).Count) then
      TList(Self).Insert(Result, AItem)
    else
      TList(Self).Add(AItem);
end;

procedure TPPreferences.TItems.Clear();
var
  I: Integer;
begin
  for I := 0 to Count - 1 do
    Item[I].Free();

  inherited;
end;

function TPPreferences.TItems.GetItem(Index: Integer): TItem;
begin
  Result := TItem(Items[Index]);
end;

function TPPreferences.TItems.IndexByName(const Name: string): Integer;
var
  Left: Integer;
  Mid: Integer;
  Right: Integer;
begin
  Result := -1;

  Left := 0;
  Right := Count - 1;
  while (Left <= Right) do
  begin
    Mid := (Right - Left) div 2 + Left;
    case (lstrcmpi(PChar(Item[Mid].Name), PChar(Name))) of
      -1: Left := Mid + 1;
      0: begin Result := Mid; break; end;
      1: Right := Mid - 1;
    end;
  end;
end;

function TPPreferences.TItems.InsertIndex(const Name: string; out Index: Integer): Boolean;
var
  Left: Integer;
  Mid: Integer;
  Right: Integer;
begin
  Result := True;

  if ((Count = 0) or (lstrcmpi(PChar(Item[Count - 1].Name), PChar(Name)) < 0)) then
    Index := Count
  else
  begin
    Left := 0;
    Right := Count - 1;
    while (Left <= Right) do
    begin
      Mid := (Right - Left) div 2 + Left;
      case (lstrcmpi(PChar(Item[Mid].Name), PChar(Name))) of
        -1: begin Left := Mid + 1;  Index := Mid + 1; end;
        0: begin Result := False; Index := Mid; break; end;
        1: begin Right := Mid - 1; Index := Mid; end;
      end;
    end;

    if (Index < 0) then
      raise ERangeError.CreateFmt(SPropertyOutOfRange, ['Index']);
  end;
end;

{ TPPreferences.TMRUList ******************************************************}

procedure TPPreferences.TMRUList.Add(const Value: string);
var
  I: Integer;
  Index: Integer;
begin
  if (Value <> '') then
  begin
    Index := IndexOf(Value);

    if (Index >= 0) then
      Delete(Index);

    while (Length(FValues) >= FMaxCount) do
      Delete(FMaxCount - 1);

    SetLength(FValues, Length(FValues) + 1);
    for I := Length(FValues) - 2 downto 0 do
      FValues[I + 1] := FValues[I];
    FValues[0] := Value;
  end;
end;

procedure TPPreferences.TMRUList.Assign(const Source: TMRUList);
var
  I: Integer;
begin
  Clear();

  if (Assigned(Source)) then
  begin
    FMaxCount := Source.MaxCount;
    for I := Source.Count - 1 downto 0 do
      Add(Source.Values[I]);
  end;
end;

procedure TPPreferences.TMRUList.Clear();
begin
  SetLength(FValues, 0);
end;

constructor TPPreferences.TMRUList.Create(const AMaxCount: Integer);
begin
  FMaxCount := AMaxCount;

  Clear();
end;

procedure TPPreferences.TMRUList.Delete(const Index: Integer);
var
  I: Integer;
begin
  if ((Index < 0) or (Length(FValues) <= Index)) then
    raise ERangeError.CreateRes(@SInvalidCurrentItem);

  for I := Index to Length(FValues) - 2 do
    FValues[I] := FValues[I + 1];
  SetLength(FValues, Length(FValues) - 1);
end;

destructor TPPreferences.TMRUList.Destroy();
begin
  Clear();

  inherited;
end;

function TPPreferences.TMRUList.GetCount(): Integer;
begin
  Result := Length(FValues);
end;

function TPPreferences.TMRUList.GetValue(Index: Integer): string;
begin
  Result := FValues[Index];
end;

function TPPreferences.TMRUList.IndexOf(const Value: string): Integer;
var
  I: Integer;
begin
  Result := -1;

  for I := 0 to Length(FValues) - 1 do
    if (lstrcmpi(PChar(Value), PChar(FValues[I])) = 0) then
      Result := I;
end;

procedure TPPreferences.TMRUList.LoadFromXML(const XML: IXMLNode; const NodeName: string);
var
  I: Integer;
begin
  Clear();

  for I := 0 to XML.ChildNodes.Count - 1 do
    if ((XML.ChildNodes[I].NodeName = NodeName) and (Length(FValues) < MaxCount)) then
    begin
      SetLength(FValues, Length(FValues) + 1);
      FValues[Length(FValues) - 1] := XML.ChildNodes[I].Text;
    end;
end;

procedure TPPreferences.TMRUList.SaveToXML(const XML: IXMLNode; const NodeName: string);
var
  I: Integer;
begin
  for I := XML.ChildNodes.Count - 1 downto 0 do
    if (XML.ChildNodes[I].NodeName = NodeName) then
      XML.ChildNodes.Delete(I);
  for I := 0 to Count - 1 do
    XML.AddChild(NodeName).Text := Values[I];
end;

{ TPPreferences.TWindow *******************************************************}

constructor TPPreferences.TWindow.Create();
begin
  inherited Create(nil);

  Height := -1;
  Width := -1;
end;

procedure TPPreferences.TWindow.LoadFromXML(const XML: IXMLNode);
var
  PixelsPerInch: Integer;
begin
  inherited;

  if (not TryStrToInt(XML.OwnerDocument.DocumentElement.Attributes['pixelsperinch'], PixelsPerInch)) then PixelsPerInch := Screen.PixelsPerInch;

  if (Assigned(XMLNode(XML, 'height')) and TryStrToInt(XMLNode(XML, 'height').Text, Height)) then Height := Round(Height * Screen.PixelsPerInch / PixelsPerInch);
  if (Assigned(XMLNode(XML, 'width')) and TryStrToInt(XMLNode(XML, 'width').Text, Width)) then Width := Round(Width * Screen.PixelsPerInch / PixelsPerInch);
end;

procedure TPPreferences.TWindow.SaveToXML(const XML: IXMLNode);
begin
  inherited;

  XMLNode(XML, 'height').Text := IntToStr(Height);
  XMLNode(XML, 'width').Text := IntToStr(Width);
end;

{ TPPreferences.TDatabases ****************************************************}

{ TPPreferences.TEditor *******************************************************}

constructor TPPreferences.TEditor.Create();
begin
  inherited;

  CodeCompletion := False;
  CodeCompletionTime := 1000;
  ConditionalCommentForeground := clTeal; ConditionalCommentBackground := clNone; ConditionalCommentStyle := [];
  CommentForeground := clGreen; CommentBackground := clNone; CommentStyle := [fsItalic];
  CurrRowBGColorEnabled := True; CurrRowBGColor := $C0FFFF;
  DataTypeForeground := clMaroon; DataTypeBackground := clNone; DataTypeStyle := [fsBold];
  FunctionForeground := clNavy; FunctionBackground := clNone; FunctionStyle := [fsBold];
  IdentifierForeground := clNone; IdentifierBackground := clNone; IdentifierStyle := [];
  KeywordForeground := clNavy; KeywordBackground := clNone; KeywordStyle := [fsBold];
  LineNumbersForeground := $9999CC; LineNumbersBackground := $F4F4F4; LineNumbersStyle := [];
  NumberForeground := clBlue; NumberBackground := clNone; NumberStyle := [];
  StringForeground := clBlue; StringBackground := clNone; StringStyle := [];
  SymbolForeground := clNone; SymbolBackground := clNone; SymbolStyle := [];
  VariableForeground := clGreen; VariableBackground := clNone; VariableStyle := [];
end;

procedure TPPreferences.TEditor.LoadFromXML(const XML: IXMLNode);
begin
  if (Assigned(XMLNode(XML, 'autocompletion'))) then TryStrToBool(XMLNode(XML, 'autocompletion').Attributes['enabled'], CodeCompletion);
  if (Assigned(XMLNode(XML, 'autocompletion/time'))) then TryStrToInt(XMLNode(XML, 'autocompletion/time').Text, CodeCompletionTime);
  if (Assigned(XMLNode(XML, 'currentrow/background'))) then TryStrToBool(XMLNode(XML, 'currentrow/background').Attributes['visible'], CurrRowBGColorEnabled);
  if (Assigned(XMLNode(XML, 'currentrow/background/color'))) then CurrRowBGColor := StringToColor(XMLNode(XML, 'currentrow/background/color').Text);
end;

procedure TPPreferences.TEditor.SaveToXML(const XML: IXMLNode);
begin
  XMLNode(XML, 'autocompletion').Attributes['enabled'] := CodeCompletion;
  XMLNode(XML, 'autocompletion/time').Text := IntToStr(CodeCompletionTime);
  XMLNode(XML, 'currentrow/background').Attributes['visible'] := CurrRowBGColorEnabled;
  XMLNode(XML, 'currentrow/background/color').Text := ColorToString(CurrRowBGColor);
end;

{ TPPreferences.TExport *******************************************************}

procedure TPPreferences.TExport.Assign(const Source: TItem);
begin
  Assert(Source is TExport);

  inherited Assign(Source);

  CSV := TExport(Source).CSV;
  Excel := TExport(Source).Excel;
  HTML := TExport(Source).HTML;
  SQL := TExport(Source).SQL;
  XML := TExport(Source).XML;
end;

constructor TPPreferences.TExport.Create(const AAItems: TItems = nil; const AName: string = '');
begin
  inherited Create();

  CSV.Headline := True;
  CSV.QuoteValues := qtStrings;
  CSV.Quoter := '"';
  CSV.Delimiter := ',';
  CSV.DelimiterType := dtChar;
  Excel.Excel2003 := False;
  Access.Access2003 := False;
  HTML.Data := True;
  HTML.NULLText := False;
  HTML.MemoContent := False;
  HTML.RowBGColor := False;
  HTML.Structure := False;
  SQL.Data := True;
  SQL.DropStmts := True;
  SQL.Structure := True;
  SQL.ReplaceData := False;
  XML.Database.NodeType := ntDisabled;
  XML.Database.NodeText := 'database';
  XML.Database.NodeAttribute := 'name';
  XML.Field.NodeType := ntName;
  XML.Field.NodeText := 'field';
  XML.Field.NodeAttribute := 'name';
  XML.Root.NodeText := 'mysql';
  XML.Row.NodeText := 'row';
  XML.Table.NodeType := ntDisabled;
  XML.Table.NodeText := 'table';
  XML.Table.NodeAttribute := 'name';
end;

procedure TPPreferences.TExport.LoadFromXML(const XML: IXMLNode);
begin
  inherited;

  if (Assigned(XMLNode(XML, 'csv/headline'))) then TryStrToBool(XMLNode(XML, 'csv/headline').Attributes['enabled'], CSV.Headline);
  if (Assigned(XMLNode(XML, 'csv/quote/string')) and (XMLNode(XML, 'csv/quote/string').Text <> '')) then CSV.Quoter := XMLNode(XML, 'csv/quote/string').Text[1];
  if (Assigned(XMLNode(XML, 'csv/quote/type'))) then TryStrToQuote(XMLNode(XML, 'csv/quote/type').Text, CSV.QuoteValues);
  if (Assigned(XMLNode(XML, 'csv/separator/character/string'))) then CSV.Delimiter := XMLNode(XML, 'csv/separator/character/string').Text;
  if (Assigned(XMLNode(XML, 'csv/separator/character/type'))) then TryStrToSeparatorType(XMLNode(XML, 'csv/separator/character/type').Text, CSV.DelimiterType);
  if (Assigned(XMLNode(XML, 'excel/format')) and (XMLNode(XML, 'excel/format').Text = '2007')) then Excel.Excel2003 := True else Excel.Excel2003 := False;
  if (Assigned(XMLNode(XML, 'access/format')) and (XMLNode(XML, 'access/format').Text = '2003')) then Access.Access2003 := True else Access.Access2003 := False;
  if (Assigned(XMLNode(XML, 'html/data'))) then TryStrToBool(XMLNode(XML, 'html/data').Attributes['enabled'], HTML.Data);
  if (Assigned(XMLNode(XML, 'html/memo')) and (XMLNode(XML, 'html/memo').Attributes['visible'] <> Null)) then TryStrToBool(XMLNode(XML, 'html/memo').Attributes['visible'], HTML.MemoContent);
  if (Assigned(XMLNode(XML, 'html/null')) and (XMLNode(XML, 'html/null').Attributes['visible'] <> Null)) then TryStrToBool(XMLNode(XML, 'html/null').Attributes['visible'], HTML.NULLText);
  if (Assigned(XMLNode(XML, 'html/row/background')) and (XMLNode(XML, 'html/row/background').Attributes['visible'] <> Null)) then TryStrToBool(XMLNode(XML, 'html/row/background').Attributes['visible'], HTML.RowBGColor);
  if (Assigned(XMLNode(XML, 'html/structure'))) then TryStrToBool(XMLNode(XML, 'html/structure').Attributes['enabled'], HTML.Structure);
  if (Assigned(XMLNode(XML, 'odbc/datasource'))) then ODBC.DataSource := XMLNode(XML, 'odbc/datasource').Text;
  if (Assigned(XMLNode(XML, 'sql/data'))) then TryStrToBool(XMLNode(XML, 'sql/data').Attributes['enabled'], SQL.Data);
  if (Assigned(XMLNode(XML, 'sql/data'))) then TryStrToBool(XMLNode(XML, 'sql/data').Attributes['replace'], SQL.ReplaceData);
  if (Assigned(XMLNode(XML, 'sql/structure'))) then TryStrToBool(XMLNode(XML, 'sql/structure').Attributes['enabled'], SQL.Structure);
  if (Assigned(XMLNode(XML, 'sql/structure'))) then TryStrToBool(XMLNode(XML, 'sql/structure').Attributes['drop'], SQL.DropStmts);
  if (Assigned(XMLNode(XML, 'xml/database')) and (XMLNode(XML, 'xml/database').Attributes['type'] <> Null)) then TryStrToNodeType(XMLNode(XML, 'xml/database').Attributes['type'], Self.XML.Database.NodeType);
  if (Assigned(XMLNode(XML, 'xml/database')) and (XMLNode(XML, 'xml/database').Text <> '')) then Self.XML.Database.NodeText := XMLNode(XML, 'xml/database').Text;
  if (Assigned(XMLNode(XML, 'xml/database')) and (XMLNode(XML, 'xml/database').Attributes['attribute'] <> Null)) then Self.XML.Database.NodeAttribute := XMLNode(XML, 'xml/database').Attributes['attribute'];
  if (Assigned(XMLNode(XML, 'xml/field')) and (XMLNode(XML, 'xml/field').Attributes['type'] <> Null)) then TryStrToNodeType(XMLNode(XML, 'xml/field').Attributes['type'], Self.XML.Field.NodeType);
  if (Assigned(XMLNode(XML, 'xml/field')) and (XMLNode(XML, 'xml/field').Text <> '')) then Self.XML.Field.NodeText := XMLNode(XML, 'xml/field').Text;
  if (Assigned(XMLNode(XML, 'xml/field')) and (XMLNode(XML, 'xml/field').Attributes['attribute'] <> Null)) then Self.XML.Field.NodeAttribute := XMLNode(XML, 'xml/field').Attributes['attribute'];
  if (Assigned(XMLNode(XML, 'xml/record')) and (XMLNode(XML, 'xml/record').Text <> '')) then Self.XML.Root.NodeText := XMLNode(XML, 'xml/record').Text;
  if (Assigned(XMLNode(XML, 'xml/root')) and (XMLNode(XML, 'xml/root').Text <> '')) then Self.XML.Root.NodeText := XMLNode(XML, 'xml/root').Text;
  if (Assigned(XMLNode(XML, 'xml/table')) and (XMLNode(XML, 'xml/table').Attributes['type'] <> Null)) then TryStrToNodeType(XMLNode(XML, 'xml/table').Attributes['type'], Self.XML.Table.NodeType);
  if (Assigned(XMLNode(XML, 'xml/table')) and (XMLNode(XML, 'xml/table').Text <> '')) then Self.XML.Table.NodeText := XMLNode(XML, 'xml/table').Text;
  if (Assigned(XMLNode(XML, 'xml/table')) and (XMLNode(XML, 'xml/table').Attributes['attribute'] <> Null)) then Self.XML.Table.NodeAttribute := XMLNode(XML, 'xml/table').Attributes['attribute'];
end;

procedure TPPreferences.TExport.SaveToXML(const XML: IXMLNode);
begin
  inherited;

  XMLNode(XML, 'csv/headline').Attributes['enabled'] := CSV.Headline;
  XMLNode(XML, 'csv/quote/string').Text := CSV.Quoter;
  XMLNode(XML, 'csv/quote/type').Text := QuoteToStr(CSV.QuoteValues);
  XMLNode(XML, 'csv/separator/character/string').Text := CSV.Delimiter;
  XMLNode(XML, 'csv/separator/character/type').Text := SeparatorTypeToStr(CSV.DelimiterType);
  if (Excel.Excel2003) then XMLNode(XML, 'excel/format').Text := '2007' else XMLNode(XML, 'excel/format').Text := '';
  if (Access.Access2003) then XMLNode(XML, 'access/format').Text := '2003' else XMLNode(XML, 'access/format').Text := '';
  XMLNode(XML, 'html/data').Attributes['enabled'] := HTML.Data;
  XMLNode(XML, 'html/memo').Attributes['visible'] := HTML.MemoContent;
  XMLNode(XML, 'html/null').Attributes['visible'] := HTML.NullText;
  XMLNode(XML, 'grid/row/background').Attributes['visible'] := HTML.RowBGColor;
  XMLNode(XML, 'html/structure').Attributes['enabled'] := HTML.Structure;
  XMLNode(XML, 'odbc/datasource').Text := ODBC.DataSource;
  XMLNode(XML, 'sql/data').Attributes['enabled'] := SQL.Data;
  XMLNode(XML, 'sql/data').Attributes['replace'] := SQL.ReplaceData;
  XMLNode(XML, 'sql/structure').Attributes['enabled'] := SQL.Structure;
  XMLNode(XML, 'sql/structure').Attributes['drop'] := SQL.DropStmts;
  XMLNode(XML, 'xml/database').Text := Self.XML.Database.NodeText;
  XMLNode(XML, 'xml/database').Attributes['type'] := NodeTypeToStr(Self.XML.Database.NodeType);
  XMLNode(XML, 'xml/database').Attributes['attribute'] := Self.XML.Database.NodeAttribute;
  XMLNode(XML, 'xml/field').Text := Self.XML.Field.NodeText;
  XMLNode(XML, 'xml/field').Attributes['type'] := NodeTypeToStr(Self.XML.Field.NodeType);
  XMLNode(XML, 'xml/field').Attributes['attribute'] := Self.XML.Field.NodeAttribute;
  XMLNode(XML, 'xml/record').Text := Self.XML.Row.NodeText;
  XMLNode(XML, 'xml/root').Text := Self.XML.Root.NodeText;
  XMLNode(XML, 'xml/table').Text := Self.XML.Table.NodeText;
  XMLNode(XML, 'xml/table').Attributes['type'] := NodeTypeToStr(Self.XML.Table.NodeType);
  XMLNode(XML, 'xml/table').Attributes['attribute'] := Self.XML.Table.NodeAttribute;
end;

{ TPPreferences.TFind *********************************************************}

constructor TPPreferences.TFind.Create();
begin
  inherited;

  FindTextMRU := TPPreferences.TMRUList.Create(10);
  Left := -1;
  Options := [foMatchCase];
  Top := -1;
end;

destructor TPPreferences.TFind.Destroy();
begin
  FindTextMRU.Free();

  inherited;
end;

procedure TPPreferences.TFind.LoadFromXML(const XML: IXMLNode);
var
  I: Integer;
begin
  inherited;

  FindTextMRU.Clear();
  if (Assigned(XMLNode(XML, 'findtext/mru'))) then
    for I := XMLNode(XML, 'findtext/mru').ChildNodes.Count - 1 downto 0 do
      if (XMLNode(XML, 'findtext/mru').ChildNodes[I].NodeName = 'text') then
        FindTextMRU.Add(XMLNode(XML, 'findtext/mru').ChildNodes[I].Text);
  if (Assigned(XMLNode(XML, 'options'))) then Options := StrToFindOptions(XMLNode(XML, 'options').Text);
end;

procedure TPPreferences.TFind.SaveToXML(const XML: IXMLNode);
var
  I: Integer;
begin
  inherited;

  XMLNode(XML, 'findtext/mru').ChildNodes.Clear();
  for I := 0 to FindTextMRU.Count - 1 do
    XMLNode(XML, 'findtext/mru').AddChild('text').Text := FindTextMRU.Values[I];
  XMLNode(XML, 'options').Text := FindOptionsToStr(Options);
end;

{ TPPreferences.TImport *******************************************************}

procedure TPPreferences.TImport.Assign(const Source: TItem);
begin
  Assert(Assigned(Source) and (Source.ClassType = ClassType));


  inherited;

  Charset := TImport(Source).Charset;
  CSV.Headline := TImport(Source).CSV.Headline;
  CSV.Quote := TImport(Source).CSV.Quote;
  CSV.QuoteChar := TImport(Source).CSV.QuoteChar;
  CSV.Delimiter := TImport(Source).CSV.Delimiter;
  CSV.DelimiterType := TImport(Source).CSV.DelimiterType;
  Collation := TImport(Source).Collation;
  Data := TImport(Source).Data;
  Engine := TImport(Source).Engine;
  RowType := TImport(Source).RowType;
  StmtType := TImport(Source).StmtType;
  Structure := TImport(Source).Structure;
end;

constructor TPPreferences.TImport.Create(const AAItems: TItems = nil; const AName: string = '');
begin
  inherited Create();

  Charset := '';
  CSV.Headline := True;
  CSV.Quote := qtStrings;
  CSV.QuoteChar := '"';
  CSV.Delimiter := ',';
  CSV.DelimiterType := dtChar;
  Collation := '';
  Data := True;
  Engine := '';
  RowType := 0;
  StmtType := stInsert;
  Structure := True;
end;

procedure TPPreferences.TImport.LoadFromXML(const XML: IXMLNode);
begin
  inherited;

  if (Assigned(XMLNode(XML, 'csv/headline'))) then TryStrToBool(XMLNode(XML, 'csv/headline').Attributes['enabled'], CSV.Headline);
  if (Assigned(XMLNode(XML, 'csv/quote/string'))) then CSV.QuoteChar := XMLNode(XML, 'csv/quote/string').Text;
  if (Assigned(XMLNode(XML, 'csv/quote/type'))) then TryStrToQuote(XMLNode(XML, 'csv/quote/type').Text, CSV.Quote);
  if (Assigned(XMLNode(XML, 'csv/separator/character/string'))) then CSV.Delimiter := XMLNode(XML, 'csv/separator/character/string').Text;
  if (Assigned(XMLNode(XML, 'csv/separator/character/type'))) then TryStrToSeparatorType(XMLNode(XML, 'csv/separator/character/type').Text, CSV.DelimiterType);
  if (Assigned(XMLNode(XML, 'data')) and (XMLNode(XML, 'data').Attributes['enabled'] <> Null)) then TryStrToBool(XMLNode(XML, 'data').Attributes['enabled'], Data);
  if (Assigned(XMLNode(XML, 'data/importtype'))) then TryStrToStmtType(XMLNode(XML, 'data/importtype').Text, StmtType);
  if (Assigned(XMLNode(XML, 'rowtype'))) then TryStrToRowType(XMLNode(XML, 'rowtype').Text, RowType);
  if (Assigned(XMLNode(XML, 'structure')) and (XMLNode(XML, 'structure').Attributes['charset'] <> Null)) then Charset := XMLNode(XML, 'structure').Attributes['charset'];
  if (Assigned(XMLNode(XML, 'structure')) and (XMLNode(XML, 'structure').Attributes['collation'] <> Null)) then Collation := XMLNode(XML, 'structure').Attributes['collation'];
  if (Assigned(XMLNode(XML, 'structure')) and (XMLNode(XML, 'structure').Attributes['enabled'] <> Null)) then TryStrToBool(XMLNode(XML, 'structure').Attributes['enabled'], Structure);
  if (Assigned(XMLNode(XML, 'structure')) and (XMLNode(XML, 'structure').Attributes['engine'] <> Null)) then Engine := XMLNode(XML, 'structure').Attributes['engine'];
  if (Assigned(XMLNode(XML, 'structure')) and (XMLNode(XML, 'structure').Attributes['rowtype'] <> Null)) then TryStrToRowType(XMLNode(XML, 'structure').Attributes['rowtype'], RowType);
end;

procedure TPPreferences.TImport.SaveToXML(const XML: IXMLNode);
begin
  inherited;

  XMLNode(XML, 'csv/headline').Attributes['enabled'] := BoolToStr(CSV.Headline, True);
  XMLNode(XML, 'csv/quote/string').Text := CSV.QuoteChar;
  XMLNode(XML, 'csv/quote/type').Text := QuoteToStr(CSV.Quote);
  XMLNode(XML, 'csv/separator/character/string').Text := CSV.Delimiter;
  XMLNode(XML, 'csv/separator/character/type').Text := SeparatorTypeToStr(CSV.DelimiterType);
  XMLNode(XML, 'data').Attributes['enabled'] := BoolToStr(Data);
  XMLNode(XML, 'data/importtype').Text := StmtTypeToStr(StmtType);
  XMLNode(XML, 'rowtype').Text := RowTypeToStr(RowType);
  XMLNode(XML, 'structure').Attributes['charset'] := Charset;
  XMLNode(XML, 'structure').Attributes['collation'] := Collation;
  XMLNode(XML, 'structure').Attributes['enabled'] := Structure;
  XMLNode(XML, 'structure').Attributes['engine'] := Engine;
  XMLNode(XML, 'structure').Attributes['rowtype'] := RowTypeToStr(RowType);
end;

{ TPPreferences.TODBC *********************************************************}

constructor TPPreferences.TODBC.Create();
begin
  inherited;

  Left := -1;
  Top := -1;
end;

{ TPPreferences.TPaste ********************************************************}

constructor TPPreferences.TPaste.Create();
begin
  inherited;

  Data := True;
end;

procedure TPPreferences.TPaste.LoadFromXML(const XML: IXMLNode);
begin
  if (Assigned(XMLNode(XML, 'data'))) then TryStrToBool(XMLNode(XML, 'data').Text, Data);
end;

procedure TPPreferences.TPaste.SaveToXML(const XML: IXMLNode);
begin
  XMLNode(XML, 'data').Text := BoolToStr(Data, True);
end;

{ TPPreferences.TReplace ******************************************************}

constructor TPPreferences.TReplace.Create();
begin
  inherited;

  FindTextMRU := TPPreferences.TMRUList.Create(10);
  ReplaceTextMRU := TPPreferences.TMRUList.Create(10);
  Left := -1;
  Options := [roMatchCase];
  Top := -1;
end;

destructor TPPreferences.TReplace.Destroy();
begin
  FindTextMRU.Free();
  ReplaceTextMRU.Free();

  inherited;
end;

procedure TPPreferences.TReplace.LoadFromXML(const XML: IXMLNode);
var
  I: Integer;
begin
  inherited;

  FindTextMRU.Clear();
  if (Assigned(XMLNode(XML, 'findtext/mru'))) then
    for I := XMLNode(XML, 'findtext/mru').ChildNodes.Count - 1 downto 0 do
      if (XMLNode(XML, 'findtext/mru').ChildNodes[I].NodeName = 'text') then
        FindTextMRU.Add(XMLNode(XML, 'findtext/mru').ChildNodes[I].Text);
  if (Assigned(XMLNode(XML, 'options'))) then Options := StrToReplaceOptions(XMLNode(XML, 'options').Text);
  ReplaceTextMRU.Clear();
  if (Assigned(XMLNode(XML, 'replacetext/mru'))) then
    for I := XMLNode(XML, 'replacetext/mru').ChildNodes.Count - 1 downto 0 do
      if (XMLNode(XML, 'replacetext/mru').ChildNodes[I].NodeName = 'text') then
        ReplaceTextMRU.Add(XMLNode(XML, 'replacetext/mru').ChildNodes[I].Text);
end;

procedure TPPreferences.TReplace.SaveToXML(const XML: IXMLNode);
var
  I: Integer;
begin
  inherited;

  XMLNode(XML, 'findtext/mru').ChildNodes.Clear();
  for I := 0 to FindTextMRU.Count - 1 do
    XMLNode(XML, 'findtext/mru').AddChild('text').Text := FindTextMRU.Values[I];
  XMLNode(XML, 'options').Text := ReplaceOptionsToStr(Options);
  XMLNode(XML, 'replacetext/mru').ChildNodes.Clear();
  for I := 0 to ReplaceTextMRU.Count - 1 do
    XMLNode(XML, 'replacetext/mru').AddChild('text').Text := ReplaceTextMRU.Values[I];
end;

{ TPPreferences.TServer *******************************************************}

{ TPPreferences.TSQLHelp ******************************************************}

constructor TPPreferences.TSQLHelp.Create();
begin
  inherited;

  Left := -1;
  Top := -1;
end;

procedure TPPreferences.TSQLHelp.LoadFromXML(const XML: IXMLNode);
var
  PixelsPerInch: Integer;
begin
  inherited;

  if (not TryStrToInt(XML.OwnerDocument.DocumentElement.Attributes['pixelsperinch'], PixelsPerInch)) then PixelsPerInch := Screen.PixelsPerInch;

  if (Assigned(XMLNode(XML, 'left')) and TryStrToInt(XMLNode(XML, 'left').Text, Left)) then Left := Round(Left * Screen.PixelsPerInch / PixelsPerInch);
  if (Assigned(XMLNode(XML, 'top')) and TryStrToInt(XMLNode(XML, 'top').Text, Top)) then Top := Round(Top * Screen.PixelsPerInch / PixelsPerInch);
end;

procedure TPPreferences.TSQLHelp.SaveToXML(const XML: IXMLNode);
begin
  inherited;

  XMLNode(XML, 'left').Text := IntToStr(Left);
  XMLNode(XML, 'top').Text := IntToStr(Top);
end;

{ TPPreferences.TAccounts *****************************************************}

constructor TPPreferences.TAccounts.Create();
begin
  inherited;

  SelectOrder := 0;
end;

procedure TPPreferences.TAccounts.LoadFromXML(const XML: IXMLNode);
begin
  inherited;

  if (Assigned(XMLNode(XML, 'selectorder'))) then TryStrToInt(XMLNode(XML, 'selectorder').Text, SelectOrder);
end;

procedure TPPreferences.TAccounts.SaveToXML(const XML: IXMLNode);
begin
  inherited;

  XMLNode(XML, 'selectorder').Text := IntToStr(SelectOrder);
end;

{ TPPreferences.TTableService *************************************************}

constructor TPPreferences.TTableService.Create();
begin
  inherited;

  Analyze := False;
  Check := False;
  Flush := False;
  Optimize := False;
  Repair := False;
end;

procedure TPPreferences.TTableService.LoadFromXML(const XML: IXMLNode);
begin
  inherited;

  if (Assigned(XMLNode(XML, 'analyze'))) then TryStrToBool(XMLNode(XML, 'analyze').Attributes['enabled'], Analyze);
  if (Assigned(XMLNode(XML, 'check'))) then TryStrToBool(XMLNode(XML, 'check').Attributes['enabled'], Check);
  if (Assigned(XMLNode(XML, 'flush'))) then TryStrToBool(XMLNode(XML, 'flush').Attributes['enabled'], Flush);
  if (Assigned(XMLNode(XML, 'optimize'))) then TryStrToBool(XMLNode(XML, 'optimize').Attributes['enabled'], Optimize);
  if (Assigned(XMLNode(XML, 'repair'))) then TryStrToBool(XMLNode(XML, 'repair').Attributes['enabled'], Repair);
end;

procedure TPPreferences.TTableService.SaveToXML(const XML: IXMLNode);
begin
  inherited;

  XMLNode(XML, 'analyze').Attributes['enabled'] := Analyze;
  XMLNode(XML, 'check').Attributes['enabled'] := Check;
  XMLNode(XML, 'flush').Attributes['enabled'] := Flush;
  XMLNode(XML, 'optimize').Attributes['enabled'] := Optimize;
  XMLNode(XML, 'repair').Attributes['enabled'] := Repair;
end;

{ TPPreferences.TTransfer *****************************************************}

constructor TPPreferences.TTransfer.Create();
begin
  inherited;

  Data := True;
  Structure := False;
end;

procedure TPPreferences.TTransfer.LoadFromXML(const XML: IXMLNode);
begin
  inherited;

  if (Assigned(XMLNode(XML, 'data'))) then TryStrToBool(XMLNode(XML, 'data').Attributes['enabled'], Data);
  if (Assigned(XMLNode(XML, 'structure'))) then TryStrToBool(XMLNode(XML, 'structure').Attributes['enabled'], Structure);
end;

procedure TPPreferences.TTransfer.SaveToXML(const XML: IXMLNode);
begin
  inherited;

  XMLNode(XML, 'data').Attributes['enabled'] := Data;
  XMLNode(XML, 'structure').Attributes['enabled'] := Structure;
end;

{ TPPreferences.TLanguage *****************************************************}

constructor TPPreferences.TLanguage.Create(const FileName: string);
var
  I: Integer;
  Item: Integer;
  MaxItem: Integer;
  Strings: TStringList;
begin
  inherited;

  FActiveQueryBuilderLanguageName := 'English';
  if (not FileExists(Filename)) then
    FLanguageId := LANG_NEUTRAL
  else
  begin
    FLanguageId := ReadInteger('Global', 'LanguageId', LANG_NEUTRAL);
    FActiveQueryBuilderLanguageName := ReadString('Global', 'ActiveQueryBuilderLanguage', 'English');

    Strings := TStringList.Create();

    ReadSectionValues('Strings', Strings);

    MaxItem := 0;
    for I := 0 to Strings.Count - 1 do
      if (TryStrToInt(Strings.Names[I], Item) and (Item > MaxItem)) then
        MaxItem := Item;

    SetLength(FStrs, MaxItem + 1);

    for I := 0 to Strings.Count - 1 do
      if (TryStrToInt(Strings.Names[I], Item)) then
        FStrs[Item] := Strings.ValueFromIndex[I];

    Strings.Free();
  end;
end;

destructor TPPreferences.TLanguage.Destroy();
begin
  SetLength(FStrs, 0);

  inherited;
end;

function TPPreferences.TLanguage.GetStr(Index: Integer): string;
begin
  if ((Index >= Length(FStrs)) or (FStrs[Index] = '')) then
    Result := SysUtils.LoadStr(10000 + Index)
  else
    Result := FStrs[Index];

  Result := ReplaceStr(ReplaceStr(Trim(Result), '\n', #10), '\r', #13);
end;

{ TPPreferences ***************************************************************}

constructor TPPreferences.Create();
var
  Foldername: array [0..MAX_PATH] of Char;
  Font: TFont;
  FontSize: Integer;
  I: Integer;
  MaxIconIndex: Integer;
  NonClientMetrics: TNonClientMetrics;
  Path: string;
  StringList: TStringList;
begin
  inherited Create(KEY_ALL_ACCESS);

  FXMLDocument := nil;
  FTaskService := nil;

  NonClientMetrics.cbSize := SizeOf(NonClientMetrics);
  if (not SystemParametersInfo(SPI_GETNONCLIENTMETRICS, SizeOf(NonClientMetrics), @NonClientMetrics, 0)) then
    FontSize := 9
  else
  begin
    Font := TFont.Create();
    Font.Handle := CreateFontIndirect(NonClientMetrics.lfMessageFont);
    FontSize := Font.Size;
    Font.Free();
  end;

  WindowState := wsNormal;
  Top := 0;
  Left := 0;
  Height := 0;
  Width := 0;
  GridFontName := 'Microsoft Sans Serif';
  GridFontColor := clWindowText;
  GridFontStyle := [];
  GridFontSize := FontSize;
  GridFontCharset := DEFAULT_CHARSET;
  GridMaxColumnWidth := Round(100 * Screen.PixelsPerInch / USER_DEFAULT_SCREEN_DPI);
  GridRowBGColor := True;
  GridCurrRowBGColor := $C0FFFF;
  GridCurrRowBGColorEnabled := True;
  GridNullBGColorEnabled := False;
  GridNullBGColor := $E0F0E0;
  GridNullText := True;
  GridMemoContent := False;
  GridDefaultSorting := True;
  SetupProgramExecute := False;
  SetupProgramInstalled := False;
  SQLFontName := 'Courier New';
  SQLFontColor := clWindowText;
  SQLFontStyle := [];
  SQLFontSize := FontSize;
  SQLFontCharset := DEFAULT_CHARSET;
  LogFontName := 'Courier New';
  LogFontColor := clWindowText;
  LogFontStyle := [];
  LogFontSize := 8;
  LogFontCharset := DEFAULT_CHARSET;
  LogHighlighting := False;
  LogTime := False;
  LogResult := False;
  LogSize := 100 * 1024;
  LanguageFilename := 'English.ini';
  TabsVisible := False;
  ToolbarTabs := [ttObjects, ttBrowser, ttEditor];
  UpdateCheck := utNever;
  UpdateChecked := 0;


  KeyBase := SysUtils.LoadStr(1003);
  GetVersion(FVerMajor, FVerMinor, FVerPatch, FVerBuild);
  {$IFDEF Debug}
  if (SysUtils.LoadStr(1000) = '') then
    FInternetAgent := 'MySQL-Front'
  else
  {$ENDIF}
  FInternetAgent := SysUtils.LoadStr(1000) + '/' + IntToStr(VerMajor) + '.' + IntToStr(VerMinor);
  SHGetFolderPath(Application.Handle, CSIDL_PERSONAL, 0, 0, @Foldername);
  Path := IncludeTrailingPathDelimiter(PChar(@Foldername));
  if ((FileExists(ExtractFilePath(Application.ExeName) + '\Desktop.xml')) or (SHGetFolderPath(Application.Handle, CSIDL_APPDATA, 0, 0, @Foldername) <> S_OK)) then
    UserPath := IncludeTrailingPathDelimiter(ExtractFilePath(Application.ExeName))
  {$IFDEF Debug}
  else if (SysUtils.LoadStr(1002) = '') then
    UserPath := IncludeTrailingPathDelimiter(IncludeTrailingPathDelimiter(PChar(@Foldername)) + 'MySQL-Front')
  {$ENDIF}
  else
    UserPath := IncludeTrailingPathDelimiter(IncludeTrailingPathDelimiter(PChar(@Foldername)) + SysUtils.LoadStr(1002));

  SoundFileNavigating := '';
  if (OpenKeyReadOnly('\AppEvents\Schemes\Apps\Explorer\Navigating\.Current')) then
  begin
    if (ValueExists('')) then
      SoundFileNavigating := ReplaceEnviromentVariables(ReadString(''));
    if (not FileExists(SoundFileNavigating)) then
      SoundFileNavigating := '';

    CloseKey();
  end;


  if (DirectoryExists(PChar(@Foldername) + PathDelim + 'SQL-Front' + PathDelim)
    and not DirectoryExists(UserPath)) then
  begin
    Path := PChar(@Foldername) + PathDelim + 'SQL-Front' + PathDelim;
    CopyDir(Path, UserPath);
    StringList := TStringList.Create();
    StringList.LoadFromFile(Filename);
    StringList.Text := ReplaceStr(StringList.Text, '<session', '<account');
    StringList.Text := ReplaceStr(StringList.Text, '</session', '</account');
    StringList.SaveToFile(Filename);
    StringList.Free();
  end;


  MaxIconIndex := 0;
  for I := 1 to 200 do
    if (FindResource(HInstance, MAKEINTRESOURCE(10000 + I), RT_GROUP_ICON) > 0) then
      MaxIconIndex := I;

  FSmallImages := TImageList.Create(nil);
  FSmallImages.ColorDepth := cd32Bit;
  FSmallImages.Height := GetSystemMetrics(SM_CYSMICON);
  FSmallImages.Width := GetSystemMetrics(SM_CXSMICON);

  for I := 0 to MaxIconIndex do
    if (I = 16) then
    begin // ODBC icon
      SHGetFolderPath(Application.Handle, CSIDL_SYSTEM, 0, 0, @Foldername);
      ImageList_AddIcon(FSmallImages.Handle, GetFileIcon(StrPas(PChar(@Foldername)) + '\odbcad32.exe'))
    end
    else if (FindResource(HInstance, MAKEINTRESOURCE(10000 + I), RT_GROUP_ICON) > 0) then
      ImageList_AddIcon(FSmallImages.Handle, LoadImage(hInstance, MAKEINTRESOURCE(10000 + I), IMAGE_ICON, GetSystemMetrics(SM_CYSMICON), GetSystemMetrics(SM_CXSMICON), LR_DEFAULTCOLOR))
    else if (I > 0) then
      ImageList_AddIcon(FSmallImages.Handle, ImageList_GetIcon(FSmallImages.Handle, 0, 0));

  FLargeImages := TImageList.Create(nil);
  FLargeImages.ColorDepth := cd32Bit;
  FLargeImages.Height := (GetSystemMetrics(SM_CYICON) + GetSystemMetrics(SM_CYSMICON)) div 2;
  FLargeImages.Width := (GetSystemMetrics(SM_CXICON) + GetSystemMetrics(SM_CXSMICON)) div 2;

  for I := 0 to MaxIconIndex do
    if (FindResource(HInstance, MAKEINTRESOURCE(10000 + I), RT_GROUP_ICON) > 0) then
      ImageList_AddIcon(FLargeImages.Handle, LoadImage(hInstance, MAKEINTRESOURCE(10000 + I), IMAGE_ICON, FLargeImages.Height, FLargeImages.Width, LR_DEFAULTCOLOR))
    else
      ImageList_AddIcon(FLargeImages.Handle, ImageList_GetIcon(FSmallImages.Handle, I, 0));


  Database := TDatabase.Create();
  Databases := TDatabases.Create();
  Editor := TEditor.Create();
  Event := TEvent.Create();
  Export := TExport.Create();
  Field := TField.Create();
  Find := TFind.Create();
  ForeignKey := TForeignKey.Create();
  Host := THost.Create();
  Import := TImport.Create();
  Key := TKey.Create();
  ODBC := TODBC.Create();
  Paste := TPaste.Create();
  Replace := TReplace.Create();
  Routine := TRoutine.Create();
  Server := TServer.Create();
  Account := TAccount.Create();
  Accounts := TAccounts.Create();
  SQLHelp := TSQLHelp.Create();
  Statement := TStatement.Create();
  Table := TTable.Create();
  TableService := TTableService.Create();
  Transfer := TTransfer.Create();
  Trigger := TTrigger.Create();
  User := TUser.Create();
  View := TPView.Create();

  LoadFromRegistry();
  if Assigned(XMLDocument.DocumentElement) then LoadFromXML(XMLDocument.DocumentElement);
end;

destructor TPPreferences.Destroy();
begin
  SaveToRegistry();

  Database.Free();
  Databases.Free();
  Editor.Free();
  Event.Free();
  Export.Free();
  Field.Free();
  Find.Free();
  ForeignKey.Free();
  Host.Free();
  Import.Free();
  Key.Free();
  ODBC.Free();
  Paste.Free();
  Replace.Free();
  Routine.Free();
  Server.Free();
  Account.Free();
  Accounts.Free();
  SQLHelp.Free();
  Statement.Free();
  Table.Free();
  TableService.Free();
  Transfer.Free();
  Trigger.Free();
  User.Free();
  View.Free();

  FLargeImages.Free();
  FSmallImages.Free();

  if (Assigned(FLanguage)) then
    FLanguage.Free();

  inherited;
end;

function TPPreferences.GetFilename(): TFileName;
begin
  Result := UserPath + 'Desktop.xml';
end;

function TPPreferences.GetLanguage(): TLanguage;
begin
  if (not Assigned(FLanguage)) then
    FLanguage := TPPreferences.TLanguage.Create(LanguagePath + LanguageFilename);

  Result := FLanguage;
end;

function TPPreferences.GetLanguagePath(): TFileName;
begin
  Result := IncludeTrailingPathDelimiter(ExtractFilePath(Application.ExeName) + 'Languages');

  {$IFDEF Debug}
    if (not FileExists(Result)) then
      Result := IncludeTrailingPathDelimiter('..\Languages\');
  {$ENDIF}
end;

function TPPreferences.GetTaskService(): ITaskService;
begin
  if (not Assigned(FTaskService)) then
    if (Failed(CoCreateInstance(CLSID_TaskScheduler, nil, CLSCTX_INPROC_SERVER, IID_ITaskService, FTaskService))
      or (Failed(FTaskService.Connect(Null, Null, Null, Null)))) then
      FTaskService := nil;

  Result := FTaskService;
end;

function TPPreferences.GetVersion(var VerMajor, VerMinor, VerPatch, VerBuild: Integer): Boolean;
var
  Buffer: PChar;
  BufferSize: Cardinal;
  FileInfo: ^VS_FIXEDFILEINFO;
  FileInfoSize: UINT;
  FileVersionAvailable: Boolean;
  Handle: Cardinal;
begin
  Result := False;
  VerMajor := -1; VerMinor := -1; VerPatch := -1; VerBuild := -1;

  BufferSize := GetFileVersionInfoSize(PChar(Application.ExeName), Handle);

  if (BufferSize > 0) then
  begin
    GetMem(Buffer, BufferSize);
    FileVersionAvailable := GetFileVersionInfo(PChar(Application.ExeName), Application.Handle, BufferSize, Buffer);
    if (FileVersionAvailable) then
      if (VerQueryValue(Buffer, '\', Pointer(FileInfo), FileInfoSize)) then
        if (FileInfoSize >= SizeOf(FileInfo^)) then
        begin
          VerMajor := FileInfo.dwFileVersionMS shr 16;
          VerMinor := FileInfo.dwFileVersionMS and $FFFF;
          VerPatch := FileInfo.dwFileVersionLS shr 16;
          VerBuild := FileInfo.dwFileVersionLS and $FFFF;
          Result := (VerMajor > 0) and (VerMinor >= 0) and (VerPatch >= 0) and (VerBuild >= 0);
        end;
    FreeMem(Buffer);
  end;
end;

function TPPreferences.GetVersionInfo(): Integer;
var
  VerBuild: Integer;
  VerMajor: Integer;
  VerMinor: Integer;
  VerPatch: Integer;
begin
  if (not GetVersion(VerMajor, VerMinor, VerPatch, VerBuild)) then
    Result := -1
  else
    Result := EncodeVersion(VerMajor, VerMinor, VerPatch, VerBuild);
end;

function TPPreferences.GetVersionStr(): string;
var
  VerBuild: Integer;
  VerMajor: Integer;
  VerMinor: Integer;
  VerPatch: Integer;
begin
  if (not GetVersion(VerMajor, VerMinor, VerPatch, VerBuild)) then
    Result := '???'
  else
    Result := IntToStr(VerMajor) + '.' + IntToStr(VerMinor) + '  (Build ' + IntToStr(VerPatch) + '.' + IntToStr(VerBuild) + ')';
end;

function TPPreferences.GetXMLDocument(): IXMLDocument;
begin
  if (not Assigned(FXMLDocument)) then
  begin
    if (FileExists(Filename)) then
      try
        FXMLDocument := LoadXMLDocument(Filename);

        if (VersionStrToVersion(FXMLDocument.DocumentElement.Attributes['version']) < 10002)  then
        begin
          XMLNode(FXMLDocument.DocumentElement, 'grid/maxcolumnwidth').Text := IntToStr(100);

          FXMLDocument.DocumentElement.Attributes['version'] := '1.0.2';
        end;

        if (VersionStrToVersion(FXMLDocument.DocumentElement.Attributes['version']) < 10100)  then
        begin
          FXMLDocument.DocumentElement.ChildNodes.Delete('session');
          FXMLDocument.DocumentElement.ChildNodes.Delete('sessions');

          FXMLDocument.DocumentElement.Attributes['version'] := '1.1.0';
        end;

        if (VersionStrToVersion(FXMLDocument.DocumentElement.Attributes['version']) < 10101)  then
        begin
          if (Assigned(XMLNode(FXMLDocument.DocumentElement, 'updates/check'))
            and (UpperCase(XMLNode(FXMLDocument.DocumentElement, 'updates/check').Text) = 'STARTUP')) then
            XMLNode(FXMLDocument.DocumentElement, 'updates/check').Text := 'Daily';

          FXMLDocument.DocumentElement.Attributes['version'] := '1.1.1';
        end;

        if (VersionStrToVersion(FXMLDocument.DocumentElement.Attributes['version']) < 10200)  then
        begin
          FXMLDocument.DocumentElement.Attributes['pixelsperinch'] := IntToStr(Screen.PixelsPerInch);

          FXMLDocument.DocumentElement.Attributes['version'] := '1.2.0';
        end;
      except
        FXMLDocument := nil;
      end;

    if (not Assigned(FXMLDocument) or not Assigned(FXMLDocument.DocumentElement)) then
    begin
      FXMLDocument := NewXMLDocument();
      FXMLDocument.Encoding := 'utf-8';
      FXMLDocument.Node.AddChild('desktop').Attributes['version'] := '1.1.1';
    end;

    FXMLDocument.Options := FXMLDocument.Options - [doAttrNull, doNodeAutoCreate];
  end;

  Result := FXMLDocument;
end;

procedure TPPreferences.LoadFromRegistry();
var
  KeyName: string;
begin
  RootKey := HKEY_CLASSES_ROOT;

  AssociateSQL := False;
  if (OpenKeyReadOnly('.sql')) then
  begin
    if (ValueExists('')) then KeyName := ReadString('');
    CloseKey();

    if (OpenKeyReadOnly(KeyName + '\shell\open\command')) then
    begin
      AssociateSQL := Pos(UpperCase(Application.ExeName), UpperCase(ReadString(''))) > 0;
      CloseKey();
    end;
  end;
  OldAssociateSQL := AssociateSQL;

  RootKey := HKEY_CURRENT_USER;

  if (OpenKeyReadOnly(KeyBase)) then
  begin
    if (ValueExists('LanguageFile')) then LanguageFilename := ExtractFileName(ReadString('LanguageFile'));
    if (ValueExists('Path') and DirectoryExists(ReadString('Path'))) then Path := IncludeTrailingPathDelimiter(ReadString('Path'));
    if (ValueExists('SetupProgram') and FileExists(ReadString('SetupProgram'))) then SetupProgram := ReadString('SetupProgram');
    if (ValueExists('SetupProgramInstalled')) then SetupProgramInstalled := ReadBool('SetupProgramInstalled');

    CloseKey();
  end;
end;

procedure TPPreferences.LoadFromXML(const XML: IXMLNode);
var
  Visible: Boolean;
var
  PixelsPerInch: Integer;
begin
  inherited;

  XML.OwnerDocument.Options := XML.OwnerDocument.Options - [doNodeAutoCreate];

  if (not TryStrToInt(XML.OwnerDocument.DocumentElement.Attributes['pixelsperinch'], PixelsPerInch)) then PixelsPerInch := Screen.PixelsPerInch;

  if (Assigned(XMLNode(XML, 'grid/currentrow/background'))) then TryStrToBool(XMLNode(XML, 'grid/currentrow/background').Attributes['visible'], GridCurrRowBGColorEnabled);
  if (Assigned(XMLNode(XML, 'grid/currentrow/background/color'))) then GridCurrRowBGColor := StringToColor(XMLNode(XML, 'grid/currentrow/background/color').Text);
  if (Assigned(XMLNode(XML, 'grid/font/charset'))) then TryStrToInt(XMLNode(XML, 'grid/font/charset').Text, GridFontCharset);
  if (Assigned(XMLNode(XML, 'grid/font/color'))) then GridFontColor := StringToColor(XMLNode(XML, 'grid/font/color').Text);
  if (Assigned(XMLNode(XML, 'grid/font/name'))) then GridFontName := XMLNode(XML, 'grid/font/name').Text;
  if (Assigned(XMLNode(XML, 'grid/font/size'))) then TryStrToInt(XMLNode(XML, 'grid/font/size').Text, GridFontSize);
  if (Assigned(XMLNode(XML, 'grid/font/style'))) then GridFontStyle := StrToStyle(XMLNode(XML, 'grid/font/style').Text);
  if (Assigned(XMLNode(XML, 'grid/memo'))) then TryStrToBool(XMLNode(XML, 'grid/memo').Attributes['visible'], GridMemoContent);
  if (Assigned(XMLNode(XML, 'grid/null'))) then TryStrToBool(XMLNode(XML, 'grid/null').Attributes['visible'], GridNullText);
  if (Assigned(XMLNode(XML, 'grid/null/background'))) then TryStrToBool(XMLNode(XML, 'grid/null/background').Attributes['visible'], GridNullBGColorEnabled);
  if (Assigned(XMLNode(XML, 'grid/null/background/color'))) then GridNullBGColor := StringToColor(XMLNode(XML, 'grid/null/background/color').Text);
  if (Assigned(XMLNode(XML, 'grid/maxcolumnwidth')) and TryStrToInt(XMLNode(XML, 'grid/maxcolumnwidth').Text, GridMaxColumnWidth)) then GridMaxColumnWidth := Round(GridMaxColumnWidth * Screen.PixelsPerInch / PixelsPerInch);
  if (Assigned(XMLNode(XML, 'grid/row/background'))) then TryStrToBool(XMLNode(XML, 'grid/row/background').Attributes['visible'], GridRowBGColor);
  if (Assigned(XMLNode(XML, 'height')) and TryStrToInt(XMLNode(XML, 'height').Text, Height)) then Height := Round(Height * Screen.PixelsPerInch / PixelsPerInch);
  if (Assigned(XMLNode(XML, 'language/file'))) then LanguageFilename := ExtractFileName(XMLNode(XML, 'language/file').Text);
  if (Assigned(XMLNode(XML, 'left'))) then TryStrToInt(XMLNode(XML, 'left').Text, Left);
  if (Assigned(XMLNode(XML, 'log/font/charset'))) then TryStrToInt(XMLNode(XML, 'log/font/charset').Text, LogFontCharset);
  if (Assigned(XMLNode(XML, 'log/font/color'))) then LogFontColor := StringToColor(XMLNode(XML, 'log/font/color').Text);
  if (Assigned(XMLNode(XML, 'log/font/name'))) then LogFontName := XMLNode(XML, 'log/font/name').Text;
  if (Assigned(XMLNode(XML, 'log/font/size'))) then TryStrToInt(XMLNode(XML, 'log/font/size').Text, LogFontSize);
  if (Assigned(XMLNode(XML, 'log/font/style'))) then LogFontStyle := StrToStyle(XMLNode(XML, 'log/font/style').Text);
  if (Assigned(XMLNode(XML, 'log/highlighting'))) then TryStrToBool(XMLNode(XML, 'log/highlighting').Attributes['visible'], LogHighlighting);
  if (Assigned(XMLNode(XML, 'log/size')) and TryStrToInt(XMLNode(XML, 'log/size').Text, LogSize)) then LogSize := Round(LogSize * Screen.PixelsPerInch / PixelsPerInch);
  if (Assigned(XMLNode(XML, 'log/dbresult'))) then TryStrToBool(XMLNode(XML, 'log/dbresult').Attributes['visible'], LogResult);
  if (Assigned(XMLNode(XML, 'log/time'))) then TryStrToBool(XMLNode(XML, 'log/time').Attributes['visible'], LogTime);
  if (Assigned(XMLNode(XML, 'toolbar/objects')) and TryStrToBool(XMLNode(XML, 'toolbar/objects').Attributes['visible'], Visible)) then
    if (Visible) then ToolbarTabs := ToolbarTabs + [ttObjects] else ToolbarTabs := ToolbarTabs - [ttObjects];
  if (Assigned(XMLNode(XML, 'toolbar/browser')) and TryStrToBool(XMLNode(XML, 'toolbar/browser').Attributes['visible'], Visible)) then
    if (Visible) then ToolbarTabs := ToolbarTabs + [ttBrowser] else ToolbarTabs := ToolbarTabs - [ttBrowser];
  if (Assigned(XMLNode(XML, 'toolbar/ide')) and TryStrToBool(XMLNode(XML, 'toolbar/ide').Attributes['visible'], Visible)) then
    if (Visible) then ToolbarTabs := ToolbarTabs + [ttIDE] else ToolbarTabs := ToolbarTabs - [ttIDE];
  if (Assigned(XMLNode(XML, 'toolbar/builder')) and TryStrToBool(XMLNode(XML, 'toolbar/builder').Attributes['visible'], Visible)) then
    if (Visible) then ToolbarTabs := ToolbarTabs + [ttBuilder] else ToolbarTabs := ToolbarTabs - [ttBuilder];
  if (Assigned(XMLNode(XML, 'toolbar/diagram')) and TryStrToBool(XMLNode(XML, 'toolbar/diagram').Attributes['visible'], Visible)) then
    if (Visible) then ToolbarTabs := ToolbarTabs + [ttDiagram] else ToolbarTabs := ToolbarTabs - [ttDiagram];
  if (Assigned(XMLNode(XML, 'toolbar/editor')) and TryStrToBool(XMLNode(XML, 'toolbar/editor').Attributes['visible'], Visible)) then
    if (Visible) then ToolbarTabs := ToolbarTabs + [ttEditor] else ToolbarTabs := ToolbarTabs - [ttEditor];
  if (Assigned(XMLNode(XML, 'toolbar/editor2')) and TryStrToBool(XMLNode(XML, 'toolbar/editor2').Attributes['visible'], Visible)) then
    if (Visible) then ToolbarTabs := ToolbarTabs + [ttEditor2] else ToolbarTabs := ToolbarTabs - [ttEditor2];
  if (Assigned(XMLNode(XML, 'toolbar/editor3')) and TryStrToBool(XMLNode(XML, 'toolbar/editor3').Attributes['visible'], Visible)) then
    if (Visible) then ToolbarTabs := ToolbarTabs + [ttEditor3] else ToolbarTabs := ToolbarTabs - [ttEditor3];
  if (Assigned(XMLNode(XML, 'sql/font/charset'))) then TryStrToInt(XMLNode(XML, 'sql/font/charset').Text, SQLFontCharset);
  if (Assigned(XMLNode(XML, 'sql/font/color'))) then SQLFontColor := StringToColor(XMLNode(XML, 'sql/font/color').Text);
  if (Assigned(XMLNode(XML, 'sql/font/name'))) then SQLFontName := XMLNode(XML, 'sql/font/name').Text;
  if (Assigned(XMLNode(XML, 'sql/font/size'))) then TryStrToInt(XMLNode(XML, 'sql/font/size').Text, SQLFontSize);
  if (Assigned(XMLNode(XML, 'sql/font/style'))) then SQLFontStyle := StrToStyle(XMLNode(XML, 'sql/font/style').Text);
  if (Assigned(XMLNode(XML, 'sql/highlighting/comment/color'))) then Editor.CommentForeground := StringToColor(XMLNode(XML, 'sql/highlighting/comment/color').Text);
  if (Assigned(XMLNode(XML, 'sql/highlighting/comment/background/color'))) then Editor.CommentBackground := StringToColor(XMLNode(XML, 'sql/highlighting/comment/background/color').Text);
  if (Assigned(XMLNode(XML, 'sql/highlighting/comment/style'))) then Editor.CommentStyle := StrToStyle(XMLNode(XML, 'sql/highlighting/comment/style').Text);
  if (Assigned(XMLNode(XML, 'sql/highlighting/conditional/color'))) then Editor.ConditionalCommentForeground := StringToColor(XMLNode(XML, 'sql/highlighting/conditional/color').Text);
  if (Assigned(XMLNode(XML, 'sql/highlighting/conditional/background/color'))) then Editor.ConditionalCommentBackground := StringToColor(XMLNode(XML, 'sql/highlighting/conditional/background/color').Text);
  if (Assigned(XMLNode(XML, 'sql/highlighting/conditional/style'))) then Editor.ConditionalCommentStyle := StrToStyle(XMLNode(XML, 'sql/highlighting/conditional/style').Text);
  if (Assigned(XMLNode(XML, 'sql/highlighting/datatype/color'))) then Editor.DataTypeForeground := StringToColor(XMLNode(XML, 'sql/highlighting/datatype/color').Text);
  if (Assigned(XMLNode(XML, 'sql/highlighting/datatype/background/color'))) then Editor.DataTypeBackground := StringToColor(XMLNode(XML, 'sql/highlighting/datatype/background/color').Text);
  if (Assigned(XMLNode(XML, 'sql/highlighting/datatype/style'))) then Editor.DataTypeStyle := StrToStyle(XMLNode(XML, 'sql/highlighting/datatype/style').Text);
  if (Assigned(XMLNode(XML, 'sql/highlighting/function/color'))) then Editor.FunctionForeground := StringToColor(XMLNode(XML, 'sql/highlighting/function/color').Text);
  if (Assigned(XMLNode(XML, 'sql/highlighting/function/background/color'))) then Editor.FunctionBackground := StringToColor(XMLNode(XML, 'sql/highlighting/function/background/color').Text);
  if (Assigned(XMLNode(XML, 'sql/highlighting/function/style'))) then Editor.FunctionStyle := StrToStyle(XMLNode(XML, 'sql/highlighting/function/style').Text);
  if (Assigned(XMLNode(XML, 'sql/highlighting/identifier/color'))) then Editor.IdentifierForeground := StringToColor(XMLNode(XML, 'sql/highlighting/identifier/color').Text);
  if (Assigned(XMLNode(XML, 'sql/highlighting/identifier/background/color'))) then Editor.IdentifierBackground := StringToColor(XMLNode(XML, 'sql/highlighting/identifier/background/color').Text);
  if (Assigned(XMLNode(XML, 'sql/highlighting/identifier/style'))) then Editor.IdentifierStyle := StrToStyle(XMLNode(XML, 'sql/highlighting/identifier/style').Text);
  if (Assigned(XMLNode(XML, 'sql/highlighting/keyword/color'))) then Editor.KeywordForeground := StringToColor(XMLNode(XML, 'sql/highlighting/keyword/color').Text);
  if (Assigned(XMLNode(XML, 'sql/highlighting/keyword/background/color'))) then Editor.KeywordBackground := StringToColor(XMLNode(XML, 'sql/highlighting/keyword/background/color').Text);
  if (Assigned(XMLNode(XML, 'sql/highlighting/keyword/style'))) then Editor.KeywordStyle := StrToStyle(XMLNode(XML, 'sql/highlighting/keyword/style').Text);
  if (Assigned(XMLNode(XML, 'sql/highlighting/linenumbers/color'))) then Editor.LineNumbersForeground := StringToColor(XMLNode(XML, 'sql/highlighting/linenumbers/color').Text);
  if (Assigned(XMLNode(XML, 'sql/highlighting/linenumbers/background/color'))) then Editor.LineNumbersBackground := StringToColor(XMLNode(XML, 'sql/highlighting/linenumbers/background/color').Text);
  if (Assigned(XMLNode(XML, 'sql/highlighting/linenumbers/style'))) then Editor.LineNumbersStyle := StrToStyle(XMLNode(XML, 'sql/highlighting/linenumbers/style').Text);
  if (Assigned(XMLNode(XML, 'sql/highlighting/number/color'))) then Editor.NumberForeground := StringToColor(XMLNode(XML, 'sql/highlighting/number/color').Text);
  if (Assigned(XMLNode(XML, 'sql/highlighting/number/background/color'))) then Editor.NumberBackground := StringToColor(XMLNode(XML, 'sql/highlighting/number/background/color').Text);
  if (Assigned(XMLNode(XML, 'sql/highlighting/number/style'))) then Editor.NumberStyle := StrToStyle(XMLNode(XML, 'sql/highlighting/number/style').Text);
  if (Assigned(XMLNode(XML, 'sql/highlighting/string/color'))) then Editor.StringForeground := StringToColor(XMLNode(XML, 'sql/highlighting/string/color').Text);
  if (Assigned(XMLNode(XML, 'sql/highlighting/string/background/color'))) then Editor.StringBackground := StringToColor(XMLNode(XML, 'sql/highlighting/string/background/color').Text);
  if (Assigned(XMLNode(XML, 'sql/highlighting/string/style'))) then Editor.StringStyle := StrToStyle(XMLNode(XML, 'sql/highlighting/string/style').Text);
  if (Assigned(XMLNode(XML, 'sql/highlighting/symbol/color'))) then Editor.SymbolForeground := StringToColor(XMLNode(XML, 'sql/highlighting/symbol/color').Text);
  if (Assigned(XMLNode(XML, 'sql/highlighting/symbol/background/color'))) then Editor.SymbolBackground := StringToColor(XMLNode(XML, 'sql/highlighting/symbol/background/color').Text);
  if (Assigned(XMLNode(XML, 'sql/highlighting/symbol/style'))) then Editor.SymbolStyle := StrToStyle(XMLNode(XML, 'sql/highlighting/symbol/style').Text);
  if (Assigned(XMLNode(XML, 'sql/highlighting/variable/color'))) then Editor.VariableForeground := StringToColor(XMLNode(XML, 'sql/highlighting/variable/color').Text);
  if (Assigned(XMLNode(XML, 'sql/highlighting/variable/background/color'))) then Editor.VariableBackground := StringToColor(XMLNode(XML, 'sql/highlighting/variable/background/color').Text);
  if (Assigned(XMLNode(XML, 'sql/highlighting/variable/style'))) then Editor.VariableStyle := StrToStyle(XMLNode(XML, 'sql/highlighting/variable/style').Text);
  if (Assigned(XMLNode(XML, 'tabs'))) then TryStrToBool(XMLNode(XML, 'tabs').Attributes['visible'], TabsVisible);
  if (Assigned(XMLNode(XML, 'top')) and TryStrToInt(XMLNode(XML, 'top').Text, Top)) then Top := Round(Top * Screen.PixelsPerInch / PixelsPerInch);
  if (Assigned(XMLNode(XML, 'updates/check'))) then TryStrToUpdateCheck(XMLNode(XML, 'updates/check').Text, UpdateCheck);
  if (Assigned(XMLNode(XML, 'updates/lastcheck'))) then TryStrToDate(XMLNode(XML, 'updates/lastcheck').Text, UpdateChecked, FileFormatSettings);
  if (Assigned(XMLNode(XML, 'width')) and TryStrToInt(XMLNode(XML, 'width').Text, Width)) then Width := Round(Width * Screen.PixelsPerInch / PixelsPerInch);
  if (Assigned(XMLNode(XML, 'windowstate'))) then TryStrToWindowState(XMLNode(XML, 'windowstate').Text, WindowState);

  if (Assigned(XMLNode(XML, 'database'))) then Database.LoadFromXML(XMLNode(XML, 'database'));
  if (Assigned(XMLNode(XML, 'editor'))) then Editor.LoadFromXML(XMLNode(XML, 'editor'));
  if (Assigned(XMLNode(XML, 'event'))) then Event.LoadFromXML(XMLNode(XML, 'event'));
  if (Assigned(XMLNode(XML, 'export'))) then Export.LoadFromXML(XMLNode(XML, 'export'));
  if (Assigned(XMLNode(XML, 'field'))) then Field.LoadFromXML(XMLNode(XML, 'field'));
  if (Assigned(XMLNode(XML, 'find'))) then Find.LoadFromXML(XMLNode(XML, 'find'));
  if (Assigned(XMLNode(XML, 'foreignkey'))) then ForeignKey.LoadFromXML(XMLNode(XML, 'foreignkey'));
  if (Assigned(XMLNode(XML, 'host'))) then Host.LoadFromXML(XMLNode(XML, 'host'));
  if (Assigned(XMLNode(XML, 'import'))) then Import.LoadFromXML(XMLNode(XML, 'import'));
  if (Assigned(XMLNode(XML, 'index'))) then Key.LoadFromXML(XMLNode(XML, 'index'));
  if (Assigned(XMLNode(XML, 'odbc'))) then ODBC.LoadFromXML(XMLNode(XML, 'odbc'));
  if (Assigned(XMLNode(XML, 'paste'))) then Paste.LoadFromXML(XMLNode(XML, 'paste'));
  if (Assigned(XMLNode(XML, 'replace'))) then Replace.LoadFromXML(XMLNode(XML, 'replace'));
  if (Assigned(XMLNode(XML, 'routine'))) then Routine.LoadFromXML(XMLNode(XML, 'routine'));
  if (Assigned(XMLNode(XML, 'server'))) then Server.LoadFromXML(XMLNode(XML, 'server'));
  if (Assigned(XMLNode(XML, 'account'))) then Account.LoadFromXML(XMLNode(XML, 'account'));
  if (Assigned(XMLNode(XML, 'accounts'))) then Accounts.LoadFromXML(XMLNode(XML, 'accounts'));
  if (Assigned(XMLNode(XML, 'sqlhelp'))) then SQLHelp.LoadFromXML(XMLNode(XML, 'sqlhelp'));
  if (Assigned(XMLNode(XML, 'statement'))) then Statement.LoadFromXML(XMLNode(XML, 'statement'));
  if (Assigned(XMLNode(XML, 'table'))) then Table.LoadFromXML(XMLNode(XML, 'table'));
  if (Assigned(XMLNode(XML, 'tableservice'))) then TableService.LoadFromXML(XMLNode(XML, 'tableservice'));
  if (Assigned(XMLNode(XML, 'transfer'))) then Transfer.LoadFromXML(XMLNode(XML, 'transfer'));
  if (Assigned(XMLNode(XML, 'trigger'))) then Trigger.LoadFromXML(XMLNode(XML, 'trigger'));
  if (Assigned(XMLNode(XML, 'user'))) then User.LoadFromXML(XMLNode(XML, 'user'));
  if (Assigned(XMLNode(XML, 'view'))) then View.LoadFromXML(XMLNode(XML, 'view'));

  FreeAndNil(FLanguage);
end;

function TPPreferences.LoadStr(const Index: Integer; const Param1: string = ''; const Param2: string = ''; const Param3: string = ''): string;
begin
  SetLength(Result, 100);
  if (Assigned(Language) and (Language.Strs[Index] <> '')) then
    Result := Language.Strs[Index]
  else
    Result := SysUtils.LoadStr(10000 + Index);
  if (Result = '') then
    Result := '<' + IntToStr(Index) + '>';

  Result := ReplaceStr(Result, '%1', Param1);
  Result := ReplaceStr(Result, '%2', Param2);
  Result := ReplaceStr(Result, '%3', Param3);
end;

procedure TPPreferences.Save();
begin
  SaveToXML(XMLDocument.DocumentElement);
  FreeAndNil(FLanguage);
end;

procedure TPPreferences.SaveToRegistry();
var
  KeyName: string;
begin
  Access := KEY_ALL_ACCESS;

  if (AssociateSQL <> OldAssociateSQL) then
  begin
    RootKey := HKEY_CLASSES_ROOT;

    if (OpenKey('.sql', True)) then
    begin
      if (not ValueExists('')) then WriteString('', 'SQLFile');
      KeyName := ReadString('');
      CloseKey();
    end;

    if (not AssociateSQL) then
    begin
      if (OpenKey(KeyName + '\DefaultIcon', False)) then
      begin
        if (ValueExists('')) then
          DeleteValue('');
        CloseKey();
      end;
      if (OpenKey(KeyName + '\shell\open\command', False)) then
      begin
        if (ValueExists('')) then
          DeleteValue('');
        CloseKey();
      end;
    end
    else
    begin
      if (OpenKey(KeyName + '\DefaultIcon', True)) then
        begin WriteString('', Application.ExeName + ',0'); CloseKey(); end;
      if (OpenKey(KeyName + '\shell\open\command', True)) then
        begin WriteString('', '"' + Application.ExeName + '" "%0"'); CloseKey(); end;
    end;

    RootKey := HKEY_CURRENT_USER;
  end;

  RootKey := HKEY_CURRENT_USER;

  if (OpenKey(KeyBase, False) or (UserPath <> ExtractFilePath(Application.ExeName)) and OpenKey(KeyBase, True)) then
  begin
    if (SetupProgram <> '') then
      WriteString('SetupProgram', SetupProgram)
    else if (ValueExists('SetupProgram')) then
      DeleteValue('SetupProgram');
    if (SetupProgramInstalled) then
      WriteBool('SetupProgramInstalled', SetupProgramInstalled)
    else if (ValueExists('SetupProgramInstalled')) then
      DeleteValue('SetupProgramInstalled');
    WriteString('Path', Path);

    CloseKey();
  end;

  Access := KEY_READ;

  SaveToXML(XMLDocument.DocumentElement);
end;

procedure TPPreferences.SaveToXML(const XML: IXMLNode);
begin
  XML.OwnerDocument.Options := XML.OwnerDocument.Options + [doNodeAutoCreate];

  XML.OwnerDocument.DocumentElement.Attributes['pixelsperinch'] := IntToStr(Screen.PixelsPerInch);

  XMLNode(XML, 'grid/currentrow/background').Attributes['visible'] := GridCurrRowBGColorEnabled;
  XMLNode(XML, 'grid/currentrow/background/color').Text := ColorToString(GridCurrRowBGColor);
  XMLNode(XML, 'grid/font/charset').Text := IntToStr(GridFontCharset);
  XMLNode(XML, 'grid/font/color').Text := ColorToString(GridFontColor);
  XMLNode(XML, 'grid/font/name').Text := GridFontName;
  XMLNode(XML, 'grid/font/size').Text := IntToStr(GridFontSize);
  XMLNode(XML, 'grid/font/style').Text := StyleToStr(GridFontStyle);
  XMLNode(XML, 'grid/memo').Attributes['visible'] := GridMemoContent;
  XMLNode(XML, 'grid/null').Attributes['visible'] := GridNullText;
  XMLNode(XML, 'grid/null/background').Attributes['visible'] := GridNullBGColorEnabled;
  XMLNode(XML, 'grid/null/background/color').Text := ColorToString(GridNullBGColor);
  XMLNode(XML, 'grid/maxcolumnwidth').Text := IntToStr(GridMaxColumnWidth);
  XMLNode(XML, 'grid/row/background').Attributes['visible'] := GridRowBGColor;
  XMLNode(XML, 'height').Text := IntToStr(Height);
  XMLNode(XML, 'language/file').Text := ExtractFileName(LanguageFilename);
  XMLNode(XML, 'left').Text := IntToStr(Left);
  XMLNode(XML, 'log/font/charset').Text := IntToStr(LogFontCharset);
  XMLNode(XML, 'log/font/color').Text := ColorToString(LogFontColor);
  XMLNode(XML, 'log/font/name').Text := LogFontName;
  XMLNode(XML, 'log/font/size').Text := IntToStr(LogFontSize);
  XMLNode(XML, 'log/font/style').Text := StyleToStr(LogFontStyle);
  XMLNode(XML, 'log/highlighting').Attributes['visible'] := LogHighlighting;
  XMLNode(XML, 'log/size').Text := IntToStr(LogSize);
  XMLNode(XML, 'log/dbresult').Attributes['visible'] := LogResult;
  XMLNode(XML, 'log/time').Attributes['visible'] := LogTime;
  if (WindowState in [wsNormal, wsMaximized	]) then
    XMLNode(XML, 'windowstate').Text := WindowStateToStr(WindowState);
  XMLNode(XML, 'toolbar/objects').Attributes['visible'] := ttObjects in ToolbarTabs;
  XMLNode(XML, 'toolbar/browser').Attributes['visible'] := ttBrowser in ToolbarTabs;
  XMLNode(XML, 'toolbar/ide').Attributes['visible'] := ttIDE in ToolbarTabs;
  XMLNode(XML, 'toolbar/builder').Attributes['visible'] := ttBuilder in ToolbarTabs;
  XMLNode(XML, 'toolbar/diagram').Attributes['visible'] := ttDiagram in ToolbarTabs;
  XMLNode(XML, 'toolbar/editor').Attributes['visible'] := ttEditor in ToolbarTabs;
  XMLNode(XML, 'toolbar/editor2').Attributes['visible'] := ttEditor2 in ToolbarTabs;
  XMLNode(XML, 'toolbar/editor3').Attributes['visible'] := ttEditor3 in ToolbarTabs;
  XMLNode(XML, 'sql/font/charset').Text := IntToStr(SQLFontCharset);
  XMLNode(XML, 'sql/font/color').Text := ColorToString(SQLFontColor);
  XMLNode(XML, 'sql/font/name').Text := SQLFontName;
  XMLNode(XML, 'sql/font/size').Text := IntToStr(SQLFontSize);
  XMLNode(XML, 'sql/font/style').Text := StyleToStr(SQLFontStyle);
  XMLNode(XML, 'sql/highlighting/comment/color').Text := ColorToString(Editor.CommentForeground);
  XMLNode(XML, 'sql/highlighting/comment/background/color').Text := ColorToString(Editor.CommentBackground);
  XMLNode(XML, 'sql/highlighting/comment/style').Text := StyleToStr(Editor.CommentStyle);
  XMLNode(XML, 'sql/highlighting/conditional/color').Text := ColorToString(Editor.ConditionalCommentForeground);
  XMLNode(XML, 'sql/highlighting/conditional/background/color').Text := ColorToString(Editor.ConditionalCommentBackground);
  XMLNode(XML, 'sql/highlighting/conditional/style').Text := StyleToStr(Editor.ConditionalCommentStyle);
  XMLNode(XML, 'sql/highlighting/datatype/color').Text := ColorToString(Editor.DataTypeForeground);
  XMLNode(XML, 'sql/highlighting/datatype/background/color').Text := ColorToString(Editor.DataTypeBackground);
  XMLNode(XML, 'sql/highlighting/datatype/style').Text := StyleToStr(Editor.DataTypeStyle);
  XMLNode(XML, 'sql/highlighting/function/color').Text := ColorToString(Editor.FunctionForeground);
  XMLNode(XML, 'sql/highlighting/function/background/color').Text := ColorToString(Editor.FunctionBackground);
  XMLNode(XML, 'sql/highlighting/function/style').Text := StyleToStr(Editor.FunctionStyle);
  XMLNode(XML, 'sql/highlighting/identifier/color').Text := ColorToString(Editor.IdentifierForeground);
  XMLNode(XML, 'sql/highlighting/identifier/background/color').Text := ColorToString(Editor.IdentifierBackground);
  XMLNode(XML, 'sql/highlighting/identifier/style').Text := StyleToStr(Editor.IdentifierStyle);
  XMLNode(XML, 'sql/highlighting/keyword/color').Text := ColorToString(Editor.KeywordForeground);
  XMLNode(XML, 'sql/highlighting/keyword/background/color').Text := ColorToString(Editor.KeywordBackground);
  XMLNode(XML, 'sql/highlighting/keyword/style').Text := StyleToStr(Editor.KeywordStyle);
  XMLNode(XML, 'sql/highlighting/linenumbers/color').Text := ColorToString(Editor.LineNumbersForeground);
  XMLNode(XML, 'sql/highlighting/linenumbers/background/color').Text := ColorToString(Editor.LineNumbersBackground);
  XMLNode(XML, 'sql/highlighting/linenumbers/style').Text := StyleToStr(Editor.LineNumbersStyle);
  XMLNode(XML, 'sql/highlighting/number/color').Text := ColorToString(Editor.NumberForeground);
  XMLNode(XML, 'sql/highlighting/number/background/color').Text := ColorToString(Editor.NumberBackground);
  XMLNode(XML, 'sql/highlighting/number/style').Text := StyleToStr(Editor.NumberStyle);
  XMLNode(XML, 'sql/highlighting/string/color').Text := ColorToString(Editor.StringForeground);
  XMLNode(XML, 'sql/highlighting/string/background/color').Text := ColorToString(Editor.StringBackground);
  XMLNode(XML, 'sql/highlighting/string/style').Text := StyleToStr(Editor.StringStyle);
  XMLNode(XML, 'sql/highlighting/symbol/color').Text := ColorToString(Editor.SymbolForeground);
  XMLNode(XML, 'sql/highlighting/symbol/background/color').Text := ColorToString(Editor.SymbolBackground);
  XMLNode(XML, 'sql/highlighting/symbol/style').Text := StyleToStr(Editor.SymbolStyle);
  XMLNode(XML, 'sql/highlighting/variable/color').Text := ColorToString(Editor.VariableForeground);
  XMLNode(XML, 'sql/highlighting/variable/background/color').Text := ColorToString(Editor.VariableBackground);
  XMLNode(XML, 'sql/highlighting/variable/style').Text := StyleToStr(Editor.VariableStyle);
  XMLNode(XML, 'tabs').Attributes['visible'] := TabsVisible;
  XMLNode(XML, 'top').Text := IntToStr(Top);
  XMLNode(XML, 'updates/check').Text := UpdateCheckToStr(UpdateCheck);
  XMLNode(XML, 'updates/lastcheck').Text := SysUtils.DateToStr(UpdateChecked, FileFormatSettings);
  XMLNode(XML, 'width').Text := IntToStr(Width);

  Database.SaveToXML(XMLNode(XML, 'database', True));
  Editor.SaveToXML(XMLNode(XML, 'editor', True));
  Event.SaveToXML(XMLNode(XML, 'event', True));
  Export.SaveToXML(XMLNode(XML, 'export', True));
  Field.SaveToXML(XMLNode(XML, 'field', True));
  Find.SaveToXML(XMLNode(XML, 'find', True));
  ForeignKey.SaveToXML(XMLNode(XML, 'foreignkey', True));
  Host.SaveToXML(XMLNode(XML, 'host', True));
  Import.SaveToXML(XMLNode(XML, 'import', True));
  Key.SaveToXML(XMLNode(XML, 'index', True));
  ODBC.SaveToXML(XMLNode(XML, 'odbc', True));
  Paste.SaveToXML(XMLNode(XML, 'paste', True));
  Replace.SaveToXML(XMLNode(XML, 'replace', True));
  Routine.SaveToXML(XMLNode(XML, 'routine', True));
  Server.SaveToXML(XMLNode(XML, 'server', True));
  Account.SaveToXML(XMLNode(XML, 'account', True));
  Accounts.SaveToXML(XMLNode(XML, 'accounts', True));
  SQLHelp.SaveToXML(XMLNode(XML, 'sqlhelp', True));
  Statement.SaveToXML(XMLNode(XML, 'statement', True));
  Table.SaveToXML(XMLNode(XML, 'table', True));
  TableService.SaveToXML(XMLNode(XML, 'tableservice', True));
  Transfer.SaveToXML(XMLNode(XML, 'transfer', True));
  Trigger.SaveToXML(XMLNode(XML, 'trigger', True));
  User.SaveToXML(XMLNode(XML, 'user', True));
  View.SaveToXML(XMLNode(XML, 'view', True));

  if (XML.OwnerDocument.Modified and ForceDirectories(ExtractFilePath(Filename))) then
    try XML.OwnerDocument.SaveToFile(Filename); except end; // We do not know about problems.
end;

{ TPAccount.TFile *************************************************************}

constructor TPAccount.TFile.Create(const AFiles: TPAccount.TFiles);
begin
  inherited Create();

  FFiles := AFiles;
end;

procedure TPAccount.TFile.LoadFromXML(const XML: IXMLNode);
var
  CodePage: Integer;
begin
  FFilename := XML.Text;
  if ((XML.Attributes['codepage'] <> Null) and TryStrToInt(XML.Attributes['codepage'], CodePage)) then FCodePage := CodePage;
end;

procedure TPAccount.TFile.SaveToXML(const XML: IXMLNode);
begin
  XML.OwnerDocument.Options := XML.OwnerDocument.Options + [doNodeAutoCreate];

  XML.Text := Filename;
  if (CodePage <> CP_ACP) then
    XML.Attributes['codepage'] := IntToStr(CodePage)
  else if (XML.Attributes['codepage'] <> Null) then
    XML.Attributes['codepage'] := Null;

  XML.OwnerDocument.Options := XML.OwnerDocument.Options - [doNodeAutoCreate];
end;

{ TPAccount.TFiles ************************************************************}

procedure TPAccount.TFiles.Add(const AFilename: TFileName; const ACodePage: Cardinal);
var
  I: Integer;
  Index: Integer;
begin
  Index := -1;

  I := 0;
  while ((I < Count) and (Index < 0)) do
    if (lstrcmpi(PChar(AFilename), PChar(Files[I].Filename)) = 0) then
      Index := I
    else
      Inc(I);

  if (Index < 0) then
  begin
    Index := 0;
    Insert(Index, TPAccount.TFile.Create(Self));

    while (Count > MaxCount) do
    begin
      Files[Count - 1].Free();
      Delete(Count - 1);
    end;
  end
  else if (Index <> 0) then
    Move(Index, 0);

  Files[0].FFilename := AFilename;
  Files[0].FCodePage := ACodePage
end;

procedure TPAccount.TFiles.Clear();
begin
  while (Count > 0) do
  begin
    Files[0].Free();
    Delete(0);
  end;

  inherited;
end;

constructor TPAccount.TFiles.Create(const ADesktop: TDesktop; const AMaxCount: Integer);
begin
  inherited Create();

  FDesktop := ADesktop;

  FMaxCount := AMaxCount;
end;

function TPAccount.TFiles.GetFile(Index: Integer): TPAccount.TFile;
begin
  Result := TPAccount.TFile(Items[Index]);
end;

procedure TPAccount.TFiles.LoadFromXML(const XML: IXMLNode);
var
  I: Integer;
begin
  Clear();

  for I := 0 to XML.ChildNodes.Count - 1 do
    if (XML.ChildNodes[I].NodeName = 'file') then
    begin
      inherited Add(TPAccount.TFile.Create(Self));
      Files[Count - 1].LoadFromXML(XML.ChildNodes[I]);
    end;
end;

procedure TPAccount.TFiles.SaveToXML(const XML: IXMLNode);
var
  I: Integer;
  Node: IXMLNode;
begin
  while (XML.ChildNodes.Count > 0) do
    XML.ChildNodes.Delete(0);

  for I := 0 to Count - 1 do
  begin
    Node := XML.AddChild('file');
    Files[I].SaveToXML(Node);
  end;
end;

{ TPPreferences.TJob **********************************************************}

procedure TPAccount.TJob.Assign(const Source: TJob);
begin
  Assert(Assigned(Source) and (Source.ClassType = ClassType));


  inherited;

  Enabled := TJob(Source).Enabled;
  Start := TJob(Source).Start;
  TriggerType := TJob(Source).TriggerType;
end;

constructor TPAccount.TJob.Create(const AJobs: TJobs; const AName: string = '');
begin
  inherited Create();

  FJobs := AJobs;
  FName := AName;

  Enabled := True;
  Start := Date() + 1;
  TriggerType := ttSingle;
end;

function TPAccount.TJob.GetLogFilename(): TFileName;
begin
  Result := Jobs.Account.DataPath + 'Jobs' + PathDelim + Name + '.err';
end;

function TPAccount.TJob.Save(const Update: Boolean): Boolean;
var
  Action: IAction;
  DailyTrigger: IDailyTrigger;
  ExecAction: IExecAction;
  Hour, Minute, Sec, MSec: Word;
  MonthlyTrigger: IMonthlyTrigger;
  RegisteredTask: IRegisteredTask;
  TaskDefinition: ITaskDefinition;
  TaskFolder: ITaskFolder;
  Trigger: ITrigger;
  XMLDocument: IXMLDocument;
  WeeklyTrigger: IWeeklyTrigger;
  Year, Month, Day: Word;
begin
  Result := False;

  TaskFolder := Jobs.GetTaskFolder(True);
  if (Assigned(TaskFolder)) then
  begin
    if (Succeeded(Jobs.TaskService.NewTask(0, TaskDefinition))
      and Succeeded(TaskDefinition.Actions.Create(TASK_ACTION_EXEC, Action))
      and Succeeded(Action.QueryInterface(IID_IExecAction, ExecAction))) then
    begin
      TaskDefinition.Settings.DisallowStartIfOnBatteries := FALSE;
      TaskDefinition.Settings.ExecutionTimeLimit := 'PT12H';
      TaskDefinition.Settings.MultipleInstances := TASK_INSTANCES_PARALLEL;
      TaskDefinition.Settings.RunOnlyIfNetworkAvailable := not HostIsLocalhost(Jobs.Account.Connection.Host);
      TaskDefinition.Settings.StartWhenAvailable := TRUE;
      TaskDefinition.Settings.StopIfGoingOnBatteries := FALSE;
      TaskDefinition.Settings.IdleSettings.IdleDuration := nil;
      TaskDefinition.Settings.IdleSettings.StopOnIdleEnd := FALSE;
      TaskDefinition.Settings.IdleSettings.WaitTimeout := nil;

      ExecAction.Path := TBStr(ParamStr(0));
      ExecAction.Arguments := TBStr('/Account="' + Jobs.Account.Name + '" /Job="' + Name + '"');

      XMLDocument := NewXMLDocument();
      XMLDocument.Encoding := 'utf-8';
      XMLDocument.Node.AddChild('job').Attributes['version'] := '1.0.0';
      SaveToXML(XMLDocument.DocumentElement);
      TaskDefinition.Data := TBStr(XMLDocument.XML.Text);

      DecodeDate(Start, Year, Month, Day);
      DecodeTime(Start, Hour, Minute, Sec, MSec);
      case (TriggerType) of
        ttSingle: TaskDefinition.Triggers.Create(TASK_TRIGGER_TIME, Trigger);
        ttDaily:
          if (Succeeded(TaskDefinition.Triggers.Create(TASK_TRIGGER_DAILY, Trigger))
            and Succeeded(Trigger.QueryInterface(IID_IDailyTrigger, DailyTrigger))) then
            DailyTrigger.DaysInterval := 1;
        ttWeekly:
          if (Succeeded(TaskDefinition.Triggers.Create(TASK_TRIGGER_WEEKLY, Trigger))
            and Succeeded(Trigger.QueryInterface(IID_IWeeklyTrigger, WeeklyTrigger))) then
          begin
            WeeklyTrigger.DaysOfWeek := 1 shl (DayOfWeek(Start) - 1);
            WeeklyTrigger.WeeksInterval := 1;
          end;
        ttMonthly:
          if (Succeeded(TaskDefinition.Triggers.Create(TASK_TRIGGER_MONTHLY, Trigger))
            and Succeeded(Trigger.QueryInterface(IID_IMonthlyTrigger, MonthlyTrigger))) then
          begin
            MonthlyTrigger.DaysOfMonth := 1 shl (Day - 1);
            MonthlyTrigger.MonthsOfYear := $FFF; // Every month
          end;
        else raise ERangeError.CreateFmt(SPropertyOutOfRange, ['TriggerType']);
      end;
      Trigger.StartBoundary := TBStr(FormatDateTime('yyyy-mm-dd"T"hh:nn:ss', Start));
      Trigger.Enabled := Enabled;

      Result := Succeeded(TaskFolder.RegisterTaskDefinition(TBStr(Name), TaskDefinition, LONG(TASK_CREATE_OR_UPDATE), Null, Null, TASK_LOGON_S4U, Null, RegisteredTask));
    end;
  end;
end;

{ TPAccount.TJobImport ********************************************************}

procedure TPAccount.TJobImport.Assign(const Source: TJob);
var
  I: Integer;
begin
  Assert(Source is TPAccount.TJobImport);

  inherited;

  CodePage := TPAccount.TJobImport(Source).CodePage;
  SetLength(FieldMappings, Length(TPAccount.TJobImport(Source).FieldMappings));
  for I := 0 to Length(FieldMappings) - 1 do
    FieldMappings[I] := TPAccount.TJobImport(Source).FieldMappings[I];
  Filename := TPAccount.TJobImport(Source).Filename;
  ImportType := TPAccount.TJobImport(Source).ImportType;
  JobObject := TPAccount.TJobImport(Source).JobObject;
  ODBC.DataSource := TPAccount.TJobImport(Source).ODBC.DataSource;
  ODBC.Username := TPAccount.TJobImport(Source).ODBC.Username;
  ODBC.Password := TPAccount.TJobImport(Source).ODBC.Password;
  SourceObjects := TPAccount.TJobImport(Source).SourceObjects;
end;

constructor TPAccount.TJobImport.Create(const AJobs: TJobs = nil; const AName: string = '');
begin
  inherited Create(AJobs, AName);

  CodePage := CP_ACP;
  SetLength(FieldMappings, 0);
  Filename := '';
  JobObject.DatabaseName := '';
  JobObject.Name := '';
  SetLength(SourceObjects, 0);
end;

destructor TPAccount.TJobImport.Destroy();
var
  I: Integer;
begin
  for I := 0 to Length(FieldMappings) - 1 do
  begin
    FieldMappings[I].DestinationFieldName := '';
    FieldMappings[I].SourceFieldName := '';
  end;
  SetLength(FieldMappings, 0);
  for I := 0 to Length(SourceObjects) - 1 do
    SourceObjects[I].Name := '';
  SetLength(SourceObjects, 0);

  inherited;
end;

procedure TPAccount.TJobImport.LoadFromXML(const XML: IXMLNode);
var
  Child: IXMLNode;
  ObjectType: TPAccount.TJob.TObjectType;
begin
  inherited;

  if (Assigned(XMLNode(XML, 'csv/headline'))) then TryStrToBool(XMLNode(XML, 'csv/headline').Attributes['enabled'], CSV.Headline);
  if (Assigned(XMLNode(XML, 'csv/quote/string'))) then CSV.QuoteChar := XMLNode(XML, 'csv/quote/string').Text;
  if (Assigned(XMLNode(XML, 'csv/quote/type'))) then TryStrToQuote(XMLNode(XML, 'csv/quote/type').Text, CSV.Quote);
  if (Assigned(XMLNode(XML, 'csv/separator/character/string'))) then CSV.Delimiter := XMLNode(XML, 'csv/separator/character/string').Text;
  if (Assigned(XMLNode(XML, 'csv/separator/character/type'))) then TryStrToSeparatorType(XMLNode(XML, 'csv/separator/character/type').Text, CSV.DelimiterType);
  if (Assigned(XMLNode(XML, 'data')) and (XMLNode(XML, 'data').Attributes['enabled'] <> Null)) then TryStrToBool(XMLNode(XML, 'data').Attributes['enabled'], Data);
  if (Assigned(XMLNode(XML, 'data/importtype'))) then TryStrToStmtType(XMLNode(XML, 'data/importtype').Text, StmtType);
  if (Assigned(XMLNode(XML, 'filename'))) then Filename := XMLNode(XML, 'filename').Text;
  if (Assigned(XMLNode(XML, 'filename')) and (XMLNode(XML, 'filename').Attributes['codepage'] <> Null)) then TryStrToInt(XMLNode(XML, 'filename').Attributes['codepage'], CodePage);
  if (Assigned(XMLNode(XML, 'odbc/datasource'))) then ODBC.DataSource := XMLNode(XML, 'odbc/datasource').Text;
  if (Assigned(XMLNode(XML, 'odbc/password'))) then ODBC.Password := XMLNode(XML, 'odbc/password').Text;
  if (Assigned(XMLNode(XML, 'odbc/username'))) then ODBC.Username := XMLNode(XML, 'odbc/username').Text;
  if (Assigned(XMLNode(XML, 'rowtype'))) then TryStrToRowType(XMLNode(XML, 'rowtype').Text, RowType);
  if (Assigned(XMLNode(XML, 'structure')) and (XMLNode(XML, 'structure').Attributes['charset'] <> Null)) then Charset := XMLNode(XML, 'structure').Attributes['charset'];
  if (Assigned(XMLNode(XML, 'structure')) and (XMLNode(XML, 'structure').Attributes['collation'] <> Null)) then Collation := XMLNode(XML, 'structure').Attributes['collation'];
  if (Assigned(XMLNode(XML, 'structure')) and (XMLNode(XML, 'structure').Attributes['enabled'] <> Null)) then TryStrToBool(XMLNode(XML, 'structure').Attributes['enabled'], Structure);
  if (Assigned(XMLNode(XML, 'structure')) and (XMLNode(XML, 'structure').Attributes['engine'] <> Null)) then Engine := XMLNode(XML, 'structure').Attributes['engine'];
  if (Assigned(XMLNode(XML, 'structure')) and (XMLNode(XML, 'structure').Attributes['rowtype'] <> Null)) then TryStrToRowType(XMLNode(XML, 'structure').Attributes['rowtype'], RowType);
  if (Assigned(XMLNode(XML, 'type'))) then TryStrToImportType(XMLNode(XML, 'type').Text, ImportType);

  if (Assigned(XMLNode(XML, 'object')) and TryStrToObjectType(XMLNode(XML, 'object').Attributes['type'], ObjectType)) then
  begin
    if (XMLNode(XML, 'object').Attributes['database'] = Null) then
      JobObject.DatabaseName := ''
    else
      JobObject.DatabaseName := XMLNode(XML, 'object').Attributes['database'];
    JobObject.Name := XMLNode(XML, 'object').Attributes['name'];
    JobObject.ObjectType := ObjectType;
  end;

  if (Assigned(XMLNode(XML, 'sources'))) then
  begin
    Child := XMLNode(XML, 'sources').ChildNodes.First();
    while (Assigned(Child)) do
    begin
      if (Child.NodeName = 'source') then
      begin
        SetLength(SourceObjects, Length(SourceObjects) + 1);
        SourceObjects[Length(SourceObjects) - 1].Name := Child.Attributes['name'];
      end;
      Child := Child.NextSibling();
    end;
  end;

  if (Assigned(XMLNode(XML, 'fields'))) then
  begin
    Child := XMLNode(XML, 'fields').ChildNodes.First();
    while (Assigned(Child)) do
    begin
      if (Child.NodeName = 'field') then
      begin
        SetLength(FieldMappings, Length(FieldMappings) + 1);
        if (Assigned(XMLNode(Child, 'source'))) then FieldMappings[Length(FieldMappings) - 1].SourceFieldName := XMLNode(Child, 'source').Text;
        if (Assigned(XMLNode(Child, 'destination'))) then FieldMappings[Length(FieldMappings) - 1].DestinationFieldName := XMLNode(Child, 'destination').Text;
      end;

      Child := Child.NextSibling();
    end;
  end;
end;

procedure TPAccount.TJobImport.SaveToXML(const XML: IXMLNode);
var
  Child: IXMLNode;
  RemoveChild: IXMLNode;
  I: Integer;
begin
  inherited;

  XML.Attributes['type'] := 'import';

  XMLNode(XML, 'csv/headline').Attributes['enabled'] := BoolToStr(CSV.Headline, True);
  XMLNode(XML, 'csv/quote/string').Text := CSV.QuoteChar;
  XMLNode(XML, 'csv/quote/type').Text := QuoteToStr(CSV.Quote);
  XMLNode(XML, 'csv/separator/character/string').Text := CSV.Delimiter;
  XMLNode(XML, 'csv/separator/character/type').Text := SeparatorTypeToStr(CSV.DelimiterType);
  XMLNode(XML, 'data').Attributes['enabled'] := BoolToStr(Data);
  XMLNode(XML, 'data/importtype').Text := StmtTypeToStr(StmtType);
  XMLNode(XML, 'filename').Text := Filename;
  if (CodePage = CP_ACP) then XMLNode(XML, 'filename').Attributes['codepage'] := Null else XMLNode(XML, 'filename').Attributes['codepage'] := IntToStr(CodePage);
  XMLNode(XML, 'odbc/datasource').Text := ODBC.DataSource;
  XMLNode(XML, 'odbc/password').Text := ODBC.Password;
  XMLNode(XML, 'odbc/username').Text := ODBC.Username;
  XMLNode(XML, 'rowtype').Text := RowTypeToStr(RowType);
  XMLNode(XML, 'structure').Attributes['charset'] := Charset;
  XMLNode(XML, 'structure').Attributes['collation'] := Collation;
  XMLNode(XML, 'structure').Attributes['enabled'] := Structure;
  XMLNode(XML, 'structure').Attributes['engine'] := Engine;
  XMLNode(XML, 'structure').Attributes['rowtype'] := RowTypeToStr(RowType);
  XMLNode(XML, 'type').Text := ImportTypeToStr(ImportType);

  XMLNode(XML, 'object').Attributes['name'] := JobObject.Name;
  if (JobObject.ObjectType in [jotTable, jotProcedure, jotFunction, jotTrigger, jotEvent]) then
    XMLNode(XML, 'object').Attributes['database'] := JobObject.DatabaseName;
  XMLNode(XML, 'object').Attributes['type'] := ObjectTypeToStr(JobObject.ObjectType);

  if (Assigned(XMLNode(XML, 'sources'))) then
  begin
    Child := XMLNode(XML, 'sources').ChildNodes.First();
    while (Assigned(Child)) do
    begin
      if (Child.NodeName <> 'source') then
        RemoveChild := nil
      else
        RemoveChild := Child;
      Child := Child.NextSibling();
      if (Assigned(RemoveChild)) then
        XMLNode(XML, 'sources').ChildNodes.Remove(RemoveChild);
    end;

    for I := 0 to Length(SourceObjects) - 1 do
    begin
      Child := XMLNode(XML, 'sources').AddChild('source');
      Child.Attributes['name'] := SourceObjects[I].Name;
      Child.Attributes['type'] := 'Table';
    end;

    for I := 0 to Length(FieldMappings) - 1 do
    begin
      Child := XMLNode(XML, 'fields').AddChild('field');
      Child.AddChild('source').Text := FieldMappings[I].SourceFieldName;
      Child.AddChild('destination').Text := FieldMappings[I].DestinationFieldName;
    end;
  end;
end;

{ TPAccount.TJobExport ********************************************************}

procedure TPAccount.TJobExport.Assign(const Source: TJob);
var
  I: Integer;
begin
  Assert(Source is TPAccount.TJobExport);

  inherited Assign(Source);

  CodePage := TPAccount.TJobExport(Source).CodePage;
  CSV.Headline := TPAccount.TJobExport(Source).CSV.Headline;
  CSV.QuoteValues := TPAccount.TJobExport(Source).CSV.QuoteValues;
  CSV.Quoter := TPAccount.TJobExport(Source).CSV.Quoter;
  CSV.Delimiter := TPAccount.TJobExport(Source).CSV.Delimiter;
  CSV.DelimiterType := TPAccount.TJobExport(Source).CSV.DelimiterType;
  Excel.Excel2007 := TPAccount.TJobExport(Source).Excel.Excel2007;
  Access.Access2003 := TPAccount.TJobExport(Source).Access.Access2003;
  ExportType := TPAccount.TJobExport(Source).ExportType;
  HTML.Data := TPAccount.TJobExport(Source).HTML.Data;
  HTML.NULLText := TPAccount.TJobExport(Source).HTML.NULLText;
  HTML.MemoContent := TPAccount.TJobExport(Source).HTML.MemoContent;
  HTML.RowBGColor := TPAccount.TJobExport(Source).HTML.RowBGColor;
  HTML.Structure := TPAccount.TJobExport(Source).HTML.Structure;
  Filename := TPAccount.TJobExport(Source).Filename;
  ClearObjects();
  SetLength(JobObjects, Length(TPAccount.TJobExport(Source).JobObjects));
  for I := 0 to Length(JobObjects) - 1 do
  begin
    JobObjects[I].ObjectType := TPAccount.TJobExport(Source).JobObjects[I].ObjectType;
    JobObjects[I].Name := TPAccount.TJobExport(Source).JobObjects[I].Name;
    JobObjects[I].DatabaseName := TPAccount.TJobExport(Source).JobObjects[I].DatabaseName;
  end;
  ODBC.DataSource := TPAccount.TJobExport(Source).ODBC.DataSource;
  ODBC.Password := TPAccount.TJobExport(Source).ODBC.Password;
  ODBC.Username := TPAccount.TJobExport(Source).ODBC.Username;
  SQL.Data := TPAccount.TJobExport(Source).SQL.Data;
  SQL.DropStmts := TPAccount.TJobExport(Source).SQL.DropStmts;
  SQL.ReplaceData := TPAccount.TJobExport(Source).SQL.ReplaceData;
  SQL.Structure := TPAccount.TJobExport(Source).SQL.Structure;
  XML.Database.NodeType := TPAccount.TJobExport(Source).XML.Database.NodeType;
  XML.Database.NodeText := TPAccount.TJobExport(Source).XML.Database.NodeText;
  XML.Database.NodeAttribute := TPAccount.TJobExport(Source).XML.Database.NodeAttribute;
  XML.Field.NodeType := TPAccount.TJobExport(Source).XML.Field.NodeType;
  XML.Field.NodeText := TPAccount.TJobExport(Source).XML.Field.NodeText;
  XML.Field.NodeAttribute := TPAccount.TJobExport(Source).XML.Field.NodeAttribute;
  XML.Row.NodeText := TPAccount.TJobExport(Source).XML.Row.NodeText;
  XML.Root.NodeText := TPAccount.TJobExport(Source).XML.Root.NodeText;
  XML.Table.NodeType := TPAccount.TJobExport(Source).XML.Table.NodeType;
  XML.Table.NodeText := TPAccount.TJobExport(Source).XML.Table.NodeText;
  XML.Table.NodeAttribute := TPAccount.TJobExport(Source).XML.Table.NodeAttribute;
end;

procedure TPAccount.TJobExport.ClearObjects();
var
  I: Integer;
begin
  for I := 0 to Length(JobObjects) - 1 do
  begin
    JobObjects[I].DatabaseName := '';
    JobObjects[I].Name := '';
  end;
  SetLength(JobObjects, 0);
end;

constructor TPAccount.TJobExport.Create(const AJobs: TJobs = nil; const AName: string = '');
begin
  inherited Create(AJobs, AName);

  CodePage := CP_ACP;
  Filename := '';
  SetLength(JobObjects, 0);
end;

destructor TPAccount.TJobExport.Destroy();
begin
  ClearObjects();

  inherited;
end;

procedure TPAccount.TJobExport.LoadFromXML(const XML: IXMLNode);
var
  Child: IXMLNode;
  ObjectType: TPAccount.TJob.TObjectType;
begin
  inherited;

  if (Assigned(XMLNode(XML, 'csv/headline'))) then TryStrToBool(XMLNode(XML, 'csv/headline').Attributes['enabled'], CSV.Headline);
  if (Assigned(XMLNode(XML, 'csv/quote/string')) and (XMLNode(XML, 'csv/quote/string').Text <> '')) then CSV.Quoter := XMLNode(XML, 'csv/quote/string').Text[1];
  if (Assigned(XMLNode(XML, 'csv/quote/type'))) then TryStrToQuote(XMLNode(XML, 'csv/quote/type').Text, CSV.QuoteValues);
  if (Assigned(XMLNode(XML, 'csv/separator/character/string'))) then CSV.Delimiter := XMLNode(XML, 'csv/separator/character/string').Text;
  if (Assigned(XMLNode(XML, 'csv/separator/character/type'))) then TryStrToSeparatorType(XMLNode(XML, 'csv/separator/character/type').Text, CSV.DelimiterType);
  if (Assigned(XMLNode(XML, 'excel/format')) and (XMLNode(XML, 'excel/format').Text = '2007')) then Excel.Excel2007 := True else Excel.Excel2007 := False;
  if (Assigned(XMLNode(XML, 'access/format')) and (XMLNode(XML, 'access/format').Text = '2003')) then Access.Access2003 := True else Access.Access2003 := False;
  if (Assigned(XMLNode(XML, 'filename'))) then Filename := XMLNode(XML, 'filename').Text;
  if (Assigned(XMLNode(XML, 'filename')) and (XMLNode(XML, 'filename').Attributes['codepage'] <> Null)) then TryStrToInt(XMLNode(XML, 'filename').Attributes['codepage'], CodePage);
  if (Assigned(XMLNode(XML, 'html/data'))) then TryStrToBool(XMLNode(XML, 'html/data').Attributes['enabled'], HTML.Data);
  if (Assigned(XMLNode(XML, 'html/memo')) and (XMLNode(XML, 'html/memo').Attributes['visible'] <> Null)) then TryStrToBool(XMLNode(XML, 'html/memo').Attributes['visible'], HTML.MemoContent);
  if (Assigned(XMLNode(XML, 'html/null')) and (XMLNode(XML, 'html/null').Attributes['visible'] <> Null)) then TryStrToBool(XMLNode(XML, 'html/null').Attributes['visible'], HTML.NULLText);
  if (Assigned(XMLNode(XML, 'html/row/background')) and (XMLNode(XML, 'html/row/background').Attributes['visible'] <> Null)) then TryStrToBool(XMLNode(XML, 'html/row/background').Attributes['visible'], HTML.RowBGColor);
  if (Assigned(XMLNode(XML, 'html/structure'))) then TryStrToBool(XMLNode(XML, 'html/structure').Attributes['enabled'], HTML.Structure);
  if (Assigned(XMLNode(XML, 'odbc/datasource'))) then ODBC.DataSource := XMLNode(XML, 'odbc/datasource').Text;
  if (Assigned(XMLNode(XML, 'odbc/password'))) then ODBC.Password := XMLNode(XML, 'odbc/password').Text;
  if (Assigned(XMLNode(XML, 'odbc/username'))) then ODBC.Username := XMLNode(XML, 'odbc/username').Text;
  if (Assigned(XMLNode(XML, 'sql/data'))) then TryStrToBool(XMLNode(XML, 'sql/data').Attributes['enabled'], SQL.Data);
  if (Assigned(XMLNode(XML, 'sql/data'))) then TryStrToBool(XMLNode(XML, 'sql/data').Attributes['replace'], SQL.ReplaceData);
  if (Assigned(XMLNode(XML, 'sql/structure'))) then TryStrToBool(XMLNode(XML, 'sql/structure').Attributes['enabled'], SQL.Structure);
  if (Assigned(XMLNode(XML, 'sql/structure'))) then TryStrToBool(XMLNode(XML, 'sql/structure').Attributes['drop'], SQL.DropStmts);
  if (Assigned(XMLNode(XML, 'type'))) then TryStrToExportType(XMLNode(XML, 'type').Text, ExportType);
  if (Assigned(XMLNode(XML, 'xml/database')) and (XMLNode(XML, 'xml/database').Attributes['type'] <> Null)) then TryStrToNodeType(XMLNode(XML, 'xml/database').Attributes['type'], Self.XML.Database.NodeType);
  if (Assigned(XMLNode(XML, 'xml/database')) and (XMLNode(XML, 'xml/database').Text <> '')) then Self.XML.Database.NodeText := XMLNode(XML, 'xml/database').Text;
  if (Assigned(XMLNode(XML, 'xml/database')) and (XMLNode(XML, 'xml/database').Attributes['attribute'] <> Null)) then Self.XML.Database.NodeAttribute := XMLNode(XML, 'xml/database').Attributes['attribute'];
  if (Assigned(XMLNode(XML, 'xml/field')) and (XMLNode(XML, 'xml/field').Attributes['type'] <> Null)) then TryStrToNodeType(XMLNode(XML, 'xml/field').Attributes['type'], Self.XML.Field.NodeType);
  if (Assigned(XMLNode(XML, 'xml/field')) and (XMLNode(XML, 'xml/field').Text <> '')) then Self.XML.Field.NodeText := XMLNode(XML, 'xml/field').Text;
  if (Assigned(XMLNode(XML, 'xml/field')) and (XMLNode(XML, 'xml/field').Attributes['attribute'] <> Null)) then Self.XML.Field.NodeAttribute := XMLNode(XML, 'xml/field').Attributes['attribute'];
  if (Assigned(XMLNode(XML, 'xml/record')) and (XMLNode(XML, 'xml/record').Text <> '')) then Self.XML.Root.NodeText := XMLNode(XML, 'xml/record').Text;
  if (Assigned(XMLNode(XML, 'xml/root')) and (XMLNode(XML, 'xml/root').Text <> '')) then Self.XML.Root.NodeText := XMLNode(XML, 'xml/root').Text;
  if (Assigned(XMLNode(XML, 'xml/table')) and (XMLNode(XML, 'xml/table').Attributes['type'] <> Null)) then TryStrToNodeType(XMLNode(XML, 'xml/table').Attributes['type'], Self.XML.Table.NodeType);
  if (Assigned(XMLNode(XML, 'xml/table')) and (XMLNode(XML, 'xml/table').Text <> '')) then Self.XML.Table.NodeText := XMLNode(XML, 'xml/table').Text;
  if (Assigned(XMLNode(XML, 'xml/table')) and (XMLNode(XML, 'xml/table').Attributes['attribute'] <> Null)) then Self.XML.Table.NodeAttribute := XMLNode(XML, 'xml/table').Attributes['attribute'];

  if (Assigned(XMLNode(XML, 'objects'))) then
  begin
    Child := XMLNode(XML, 'objects').ChildNodes.First();
    while (Assigned(Child)) do
    begin
      if ((Child.NodeName = 'object') and TryStrToObjectType(Child.Attributes['type'], ObjectType)) then
      begin
        SetLength(JobObjects, Length(JobObjects) + 1);
        if (Child.Attributes['database'] = Null) then
          JobObjects[Length(JobObjects) - 1].DatabaseName := ''
        else
          JobObjects[Length(JobObjects) - 1].DatabaseName := Child.Attributes['database'];
        JobObjects[Length(JobObjects) - 1].Name := Child.Attributes['name'];
        JobObjects[Length(JobObjects) - 1].ObjectType := ObjectType;
      end;
      Child := Child.NextSibling();
    end;
  end;
end;

procedure TPAccount.TJobExport.SaveToXML(const XML: IXMLNode);
var
  Child: IXMLNode;
  RemoveChild: IXMLNode;
  I: Integer;
begin
  inherited;

  XML.Attributes['type'] := 'export';

  XMLNode(XML, 'csv/headline').Attributes['enabled'] := CSV.Headline;
  XMLNode(XML, 'csv/quote/string').Text := CSV.Quoter;
  XMLNode(XML, 'csv/quote/type').Text := QuoteToStr(CSV.QuoteValues);
  XMLNode(XML, 'csv/separator/character/string').Text := CSV.Delimiter;
  XMLNode(XML, 'csv/separator/character/type').Text := SeparatorTypeToStr(CSV.DelimiterType);
  if (Excel.Excel2007) then XMLNode(XML, 'excel/format').Text := '2007' else XMLNode(XML, 'excel/format').Text := '';
  if (Access.Access2003) then XMLNode(XML, 'access/format').Text := '2003' else XMLNode(XML, 'access/format').Text := '';
  XMLNode(XML, 'filename').Text := Filename;
  if (CodePage = CP_ACP) then XMLNode(XML, 'filename').Attributes['codepage'] := Null else XMLNode(XML, 'filename').Attributes['codepage'] := IntToStr(CodePage);
  XMLNode(XML, 'grid/row/background').Attributes['visible'] := HTML.RowBGColor;
  XMLNode(XML, 'html/data').Attributes['enabled'] := HTML.Data;
  XMLNode(XML, 'html/memo').Attributes['visible'] := HTML.MemoContent;
  XMLNode(XML, 'html/null').Attributes['visible'] := HTML.NullText;
  XMLNode(XML, 'html/structure').Attributes['enabled'] := HTML.Structure;
  XMLNode(XML, 'type').Text := ExportTypeToStr(ExportType);
  XMLNode(XML, 'odbc/datasource').Text := ODBC.DataSource;
  XMLNode(XML, 'odbc/password').Text := ODBC.Password;
  XMLNode(XML, 'odbc/username').Text := ODBC.Username;
  XMLNode(XML, 'sql/data').Attributes['enabled'] := SQL.Data;
  XMLNode(XML, 'sql/data').Attributes['replace'] := SQL.ReplaceData;
  XMLNode(XML, 'sql/structure').Attributes['enabled'] := SQL.Structure;
  XMLNode(XML, 'sql/structure').Attributes['drop'] := SQL.DropStmts;
  XMLNode(XML, 'xml/database').Text := Self.XML.Database.NodeText;
  XMLNode(XML, 'xml/database').Attributes['type'] := NodeTypeToStr(Self.XML.Database.NodeType);
  XMLNode(XML, 'xml/database').Attributes['attribute'] := Self.XML.Database.NodeAttribute;
  XMLNode(XML, 'xml/field').Text := Self.XML.Field.NodeText;
  XMLNode(XML, 'xml/field').Attributes['type'] := NodeTypeToStr(Self.XML.Field.NodeType);
  XMLNode(XML, 'xml/field').Attributes['attribute'] := Self.XML.Field.NodeAttribute;
  XMLNode(XML, 'xml/record').Text := Self.XML.Row.NodeText;
  XMLNode(XML, 'xml/root').Text := Self.XML.Root.NodeText;
  XMLNode(XML, 'xml/table').Text := Self.XML.Table.NodeText;
  XMLNode(XML, 'xml/table').Attributes['type'] := NodeTypeToStr(Self.XML.Table.NodeType);
  XMLNode(XML, 'xml/table').Attributes['attribute'] := Self.XML.Table.NodeAttribute;

  if (Assigned(XMLNode(XML, 'objects'))) then
  begin
    Child := XMLNode(XML, 'objects').ChildNodes.First();
    while (Assigned(Child)) do
    begin
      if (Child.NodeName <> 'object') then
        RemoveChild := nil
      else
        RemoveChild := Child;
      Child := Child.NextSibling();
      if (Assigned(RemoveChild)) then
        XMLNode(XML, 'objects').ChildNodes.Remove(RemoveChild);
    end;

    for I := 0 to Length(JobObjects) - 1 do
    begin
      Child := XMLNode(XML, 'objects').AddChild('object');
      Child.Attributes['name'] := JobObjects[I].Name;
      if (JobObjects[I].ObjectType in [jotTable, jotProcedure, jotFunction, jotTrigger, jotEvent]) then
        Child.Attributes['database'] := JobObjects[I].DatabaseName;
      Child.Attributes['type'] := ObjectTypeToStr(JobObjects[I].ObjectType);
    end;
  end;
end;

{ TPAccount.TJobs *************************************************************}

function TPAccount.TJobs.AddJob(const NewJob: TPAccount.TJob): Boolean;
var
  Job: TPAccount.TJob;
begin
  Result := IndexByName(NewJob.Name) < 0;
  if (Result) then
  begin
    if (NewJob is TPAccount.TJobImport) then
      Job := TPAccount.TJobImport.Create(Self, NewJob.Name)
    else if (NewJob is TPAccount.TJobExport) then
      Job := TPAccount.TJobExport.Create(Self, NewJob.Name)
    else
      raise ERangeError.Create(SRangeError);
    Job.Assign(NewJob);

    Result := Job.Save(False);

    if (Result) then
    begin
      Add(Job);
      Account.AccountEvent(ClassType);
    end;
  end;
end;

procedure TPAccount.TJobs.Clear();
var
  I: Integer;
begin
  for I := 0 to Count - 1 do
    Job[I].Free();

  inherited;
end;

constructor TPAccount.TJobs.Create(const AAccount: TPAccount);
begin
  inherited Create();

  FAccount := AAccount;
end;

procedure TPAccount.TJobs.DeleteJob(const Job: TPAccount.TJob);
var
  Index: Integer;
  TaskFolder: ITaskFolder;
begin
  TaskFolder := GetTaskFolder(True);

  if (Assigned(TaskFolder)) then
    TaskFolder.DeleteTask(TBStr(Job.Name), 0);

  if (FileExists(Job.LogFilename)) then
    DeleteFile(Job.LogFilename);

  Index := IndexOf(Job);
  if (Index >= 0) then
    Delete(Index);
  Job.Free();

  Account.AccountEvent(ClassType);
end;

destructor TPAccount.TJobs.Destroy();
begin
  Clear();

  inherited;
end;

function TPAccount.TJobs.GetJob(Index: Integer): TPAccount.TJob;
begin
  Result := Items[Index];
end;

function TPAccount.TJobs.GetTaskFolder(const AutoCreate: Boolean = False): ITaskFolder;
var
  AppFolder: ITaskFolder;
  RootFolder: ITaskFolder;
begin
  if (not Assigned(TaskService) or not Assigned(Account)) then
    Result := nil
  else if (Failed(TaskService.GetFolder(TBStr('\' + SysUtils.LoadStr(1006) + '\Accounts\' + Account.Name), Result))) then
    if (not AutoCreate
      or Failed(TaskService.GetFolder('\', RootFolder)))
      or Failed(RootFolder.GetFolder(TBStr(SysUtils.LoadStr(1006) + '\Accounts'), AppFolder)) and Failed(RootFolder.CreateFolder(TBStr(SysUtils.LoadStr(1006) + '\Accounts'), Null, AppFolder))
      or Failed(AppFolder.GetFolder(TBStr(Account.Name), Result)) and Failed(AppFolder.CreateFolder(TBStr(Account.Name), Null, Result)) then
      Result := nil;
end;

function TPAccount.TJobs.GetTaskService(): ITaskService;
begin
  Result := Preferences.TaskService;
end;

function TPAccount.TJobs.IndexByName(const Name: string): Integer;
var
  Left: Integer;
  Mid: Integer;
  Right: Integer;
begin
  Result := -1;

  Left := 0;
  Right := Count - 1;
  while (Left <= Right) do
  begin
    Mid := (Right - Left) div 2 + Left;
    case (lstrcmpi(PChar(Job[Mid].Name), PChar(Name))) of
      -1: Left := Mid + 1;
      0: begin Result := Mid; break; end;
      1: Right := Mid - 1;
    end;
  end;
end;

procedure TPAccount.TJobs.Load();
var
  I: Integer;
  Job: TPAccount.TJob;
  S: string;
  RegisteredTask: IRegisteredTask;
  TaskFolder: ITaskFolder;
  Tasks: IRegisteredTaskCollection;
  XMLDocument: IXMLDocument;
begin
  Clear();

  TaskFolder := GetTaskFolder(False);
  if (Assigned(TaskFolder) and Succeeded(TaskFolder.GetTasks(0, Tasks))) then
  begin
    XMLDocument := NewXMLDocument();
    for I := 0 to Tasks.Count - 1 do
    begin
      RegisteredTask := Tasks.Item[1 + I];
      XMLDocument.LoadFromXML(StrPas(RegisteredTask.Definition.Data));
      if (XMLDocument.DocumentElement.Attributes['type'] = 'import') then
        Job := TPAccount.TJobImport.Create(Self, StrPas(RegisteredTask.Name))
      else if (XMLDocument.DocumentElement.Attributes['type'] = 'export') then
        Job := TPAccount.TJobExport.Create(Self, StrPas(RegisteredTask.Name))
      else
        Job := nil;
      if (Assigned(Job)) then
      begin
        if (Assigned(XMLDocument.DocumentElement)) then Job.LoadFromXML(XMLDocument.DocumentElement);
        if (RegisteredTask.Definition.Triggers.Count >= 1) then
        begin
          Job.Enabled := RegisteredTask.Definition.Triggers.Item[1].Enabled;
          S := StrPas(RegisteredTask.Definition.Triggers[1].StartBoundary);
          if ((Length(S) >= 19) and (UpCase(S[11]) = 'T')) then
          begin
            Job.Start := EncodeDate(StrToInt(Copy(S, 1, 4)), StrToInt(Copy(S, 6, 2)), StrToInt(Copy(S, 9, 2)))
              + EncodeTime(StrToInt(Copy(S, 12, 2)), StrToInt(Copy(S, 15, 2)), StrToInt(Copy(S, 18, 2)), 0);
          end;
          case (RegisteredTask.Definition.Triggers.Item[1].TriggerType) of
            TASK_TRIGGER_TIME: Job.TriggerType := ttSingle;
            TASK_TRIGGER_DAILY: Job.TriggerType := ttDaily;
            TASK_TRIGGER_WEEKLY: Job.TriggerType := ttWeekly;
            TASK_TRIGGER_MONTHLY: Job.TriggerType := ttMonthly;
            else Job.TriggerType := ttSingle;
          end;
        end;

        Add(Job);
      end;
    end;
  end;
end;

function TPAccount.TJobs.UpdateJob(const Job, NewJob: TPAccount.TJob): Boolean;
begin
  Result := (IndexOf(Job) >= 0) and ((IndexByName(NewJob.Name) = IndexOf(Job)) or (IndexByName(NewJob.Name) < 0));
  if (Result) then
  begin
    if ((NewJob.LogFilename <> Job.LogFilename) and FileExists(Job.LogFilename)) then
      RenameFile(Job.LogFilename, NewJob.LogFilename);

    Job.Assign(NewJob);

    Result := Job.Save(True);

    if (Result) then
      Account.AccountEvent(ClassType);
  end;
end;

{ TPAccount.TDesktop **********************************************************}

procedure TPAccount.TDesktop.Assign(const Source: TDesktop);
var
  I: Integer;
  Kind: TListViewKind;
begin
  Address := Account.ExpandAddress(Source.Account.ExtractPath(Source.Address));
  BlobHeight := Source.BlobHeight;
  for Kind := lkServer to lkVariables do
    for I := 0 to Length(ColumnWidths[Kind]) - 1 do
      ColumnWidths[Kind, I] := Source.ColumnWidths[Kind, I];
  DataHeight := Source.DataHeight;
  EditorContent := Source.EditorContent;
  ExplorerVisible := Source.ExplorerVisible;
  FilesFilter := Source.FilesFilter;
  FoldersHeight := Source.FoldersHeight;
  JobsVisible := Source.JobsVisible;
  LogHeight := Source.LogHeight;
  LogVisible := Source.LogVisible;
  NavigatorVisible := Source.NavigatorVisible;
  SidebarWitdth := Source.SidebarWitdth;
  SQLHistoryVisible := Source.SQLHistoryVisible;
end;

constructor TPAccount.TDesktop.Create(const AAccount: TPAccount);
var
  I: Integer;
  Kind: TListViewKind;
begin
  inherited Create();

  FAccount := AAccount;

  FAddressMRU := TPPreferences.TMRUList.Create(10);
  BlobHeight := Round(100 * Screen.PixelsPerInch / USER_DEFAULT_SCREEN_DPI);
  for Kind := lkServer to lkVariables do
    for I := 0 to Length(ColumnWidths[Kind]) - 1 do
      ColumnWidths[Kind][I] := ColumnTextWidth;
  DataHeight := Round(150 * Screen.PixelsPerInch / USER_DEFAULT_SCREEN_DPI);
  EditorContent[ttEditor] := '';
  EditorContent[ttEditor2] := '';
  EditorContent[ttEditor3] := '';
  ExplorerVisible := False;
  FilesFilter := '*.sql';
  FoldersHeight := Round(100 * Screen.PixelsPerInch / USER_DEFAULT_SCREEN_DPI);
  JobsVisible := False;
  NavigatorVisible := True;
  LogHeight := Round(80 * Screen.PixelsPerInch / USER_DEFAULT_SCREEN_DPI);
  LogVisible := False;
  FPath := '/';
  SidebarWitdth := Round(150 * Screen.PixelsPerInch / USER_DEFAULT_SCREEN_DPI);
  SQLHistoryVisible := False;

  FFiles := TFiles.Create(Self, 10);
end;

destructor TPAccount.TDesktop.Destroy();
begin
  FAddressMRU.Free();
  FFiles.Free();

  inherited;
end;

function TPAccount.TDesktop.GetAddress(): string;
begin
  Result := Account.ExpandAddress(FPath);
end;

procedure TPAccount.TDesktop.LoadFromXML(const XML: IXMLNode);
var
  PixelsPerInch: Integer;
begin
  inherited;

  if (not TryStrToInt(XML.OwnerDocument.DocumentElement.Attributes['pixelsperinch'], PixelsPerInch)) then PixelsPerInch := Screen.PixelsPerInch;

  if (Assigned(XMLNode(XML, 'datagrid/height')) and TryStrToInt(XMLNode(XML, 'datagrid/height').Text, DataHeight)) then DataHeight := Round(DataHeight * Screen.PixelsPerInch / PixelsPerInch);
  if (Assigned(XMLNode(XML, 'datagrid/blob/height')) and TryStrToInt(XMLNode(XML, 'datagrid/blob/height').Text, BlobHeight)) then BlobHeight := Round(BlobHeight * Screen.PixelsPerInch / PixelsPerInch);
  if (Assigned(XMLNode(XML, 'editor/content'))) then EditorContent[ttEditor] := XMLNode(XML, 'editor/content').Text;
  if (Assigned(XMLNode(XML, 'editor2/content'))) then EditorContent[ttEditor2] := XMLNode(XML, 'editor2/content').Text;
  if (Assigned(XMLNode(XML, 'editor3/content'))) then EditorContent[ttEditor3] := XMLNode(XML, 'editor3/content').Text;
  if (Assigned(XMLNode(XML, 'log/height')) and TryStrToInt(XMLNode(XML, 'log/height').Text, LogHeight)) then LogHeight := Round(LogHeight * Screen.PixelsPerInch / PixelsPerInch);
  if (Assigned(XMLNode(XML, 'log/visible'))) then TryStrToBool(XMLNode(XML, 'log/visible').Text, LogVisible);
  if (Assigned(XMLNode(XML, 'objects/server/widths/name')) and TryStrToInt(XMLNode(XML, 'objects/server/widths/name').Text, ColumnWidths[lkServer][0])) then ColumnWidths[lkServer][0] := Round(ColumnWidths[lkServer][0] * Screen.PixelsPerInch / PixelsPerInch);
  if (Assigned(XMLNode(XML, 'objects/server/widths/size')) and TryStrToInt(XMLNode(XML, 'objects/server/widths/size').Text, ColumnWidths[lkServer][1])) then ColumnWidths[lkServer][1] := Round(ColumnWidths[lkServer][1] * Screen.PixelsPerInch / PixelsPerInch);
  if (Assigned(XMLNode(XML, 'objects/server/widths/count')) and TryStrToInt(XMLNode(XML, 'objects/server/widths/count').Text, ColumnWidths[lkServer][2])) then ColumnWidths[lkServer][2] := Round(ColumnWidths[lkServer][2] * Screen.PixelsPerInch / PixelsPerInch);
  if (Assigned(XMLNode(XML, 'objects/server/widths/created')) and TryStrToInt(XMLNode(XML, 'objects/server/widths/created').Text, ColumnWidths[lkServer][3])) then ColumnWidths[lkServer][3] := Round(ColumnWidths[lkServer][3] * Screen.PixelsPerInch / PixelsPerInch);
  if (Assigned(XMLNode(XML, 'objects/server/widths/extras')) and TryStrToInt(XMLNode(XML, 'objects/server/widths/extras').Text, ColumnWidths[lkServer][4])) then ColumnWidths[lkServer][4] := Round(ColumnWidths[lkServer][4] * Screen.PixelsPerInch / PixelsPerInch);
  if (Assigned(XMLNode(XML, 'objects/database/widths/name')) and TryStrToInt(XMLNode(XML, 'objects/database/widths/name').Text, ColumnWidths[lkDatabase][0])) then ColumnWidths[lkDatabase][0] := Round(ColumnWidths[lkDatabase][0] * Screen.PixelsPerInch / PixelsPerInch);
  if (Assigned(XMLNode(XML, 'objects/database/widths/type')) and TryStrToInt(XMLNode(XML, 'objects/database/widths/type').Text, ColumnWidths[lkDatabase][1])) then ColumnWidths[lkDatabase][1] := Round(ColumnWidths[lkDatabase][1] * Screen.PixelsPerInch / PixelsPerInch);
  if (Assigned(XMLNode(XML, 'objects/database/widths/recordcount')) and TryStrToInt(XMLNode(XML, 'objects/database/widths/recordcount').Text, ColumnWidths[lkDatabase][2])) then ColumnWidths[lkDatabase][2] := Round(ColumnWidths[lkDatabase][2] * Screen.PixelsPerInch / PixelsPerInch);
  if (Assigned(XMLNode(XML, 'objects/database/widths/size')) and TryStrToInt(XMLNode(XML, 'objects/database/widths/size').Text, ColumnWidths[lkDatabase][3])) then ColumnWidths[lkDatabase][3] := Round(ColumnWidths[lkDatabase][3] * Screen.PixelsPerInch / PixelsPerInch);
  if (Assigned(XMLNode(XML, 'objects/database/widths/updated')) and TryStrToInt(XMLNode(XML, 'objects/database/widths/updated').Text, ColumnWidths[lkDatabase][4])) then ColumnWidths[lkDatabase][4] := Round(ColumnWidths[lkDatabase][4] * Screen.PixelsPerInch / PixelsPerInch);
  if (Assigned(XMLNode(XML, 'objects/database/widths/extras')) and TryStrToInt(XMLNode(XML, 'objects/database/widths/extras').Text, ColumnWidths[lkDatabase][5])) then ColumnWidths[lkDatabase][5] := Round(ColumnWidths[lkDatabase][5] * Screen.PixelsPerInch / PixelsPerInch);
  if (Assigned(XMLNode(XML, 'objects/database/widths/comment')) and TryStrToInt(XMLNode(XML, 'objects/database/widths/comment').Text, ColumnWidths[lkDatabase][6])) then ColumnWidths[lkDatabase][6] := Round(ColumnWidths[lkDatabase][6] * Screen.PixelsPerInch / PixelsPerInch);
  if (Assigned(XMLNode(XML, 'objects/table/widths/name')) and TryStrToInt(XMLNode(XML, 'objects/table/widths/name').Text, ColumnWidths[lkTable][0])) then ColumnWidths[lkTable][0] := Round(ColumnWidths[lkTable][0] * Screen.PixelsPerInch / PixelsPerInch);
  if (Assigned(XMLNode(XML, 'objects/table/widths/type')) and TryStrToInt(XMLNode(XML, 'objects/table/widths/type').Text, ColumnWidths[lkTable][1])) then ColumnWidths[lkTable][1] := Round(ColumnWidths[lkTable][1] * Screen.PixelsPerInch / PixelsPerInch);
  if (Assigned(XMLNode(XML, 'objects/table/widths/null')) and TryStrToInt(XMLNode(XML, 'objects/table/widths/null').Text, ColumnWidths[lkTable][2])) then ColumnWidths[lkTable][2] := Round(ColumnWidths[lkTable][2] * Screen.PixelsPerInch / PixelsPerInch);
  if (Assigned(XMLNode(XML, 'objects/table/widths/default')) and TryStrToInt(XMLNode(XML, 'objects/table/widths/default').Text, ColumnWidths[lkTable][3])) then ColumnWidths[lkTable][3] := Round(ColumnWidths[lkTable][3] * Screen.PixelsPerInch / PixelsPerInch);
  if (Assigned(XMLNode(XML, 'objects/table/widths/extras')) and TryStrToInt(XMLNode(XML, 'objects/table/widths/extras').Text, ColumnWidths[lkTable][4])) then ColumnWidths[lkTable][4] := Round(ColumnWidths[lkTable][4] * Screen.PixelsPerInch / PixelsPerInch);
  if (Assigned(XMLNode(XML, 'objects/table/widths/comment')) and TryStrToInt(XMLNode(XML, 'objects/table/widths/comment').Text, ColumnWidths[lkTable][5])) then ColumnWidths[lkTable][5] := Round(ColumnWidths[lkTable][5] * Screen.PixelsPerInch / PixelsPerInch);
  if (Assigned(XMLNode(XML, 'objects/processes/widths/id')) and TryStrToInt(XMLNode(XML, 'objects/processes/widths/id').Text, ColumnWidths[lkProcesses][0])) then ColumnWidths[lkProcesses][0] := Round(ColumnWidths[lkProcesses][0] * Screen.PixelsPerInch / PixelsPerInch);
  if (Assigned(XMLNode(XML, 'objects/processes/widths/user')) and TryStrToInt(XMLNode(XML, 'objects/processes/widths/user').Text, ColumnWidths[lkProcesses][1])) then ColumnWidths[lkProcesses][1] := Round(ColumnWidths[lkProcesses][1] * Screen.PixelsPerInch / PixelsPerInch);
  if (Assigned(XMLNode(XML, 'objects/processes/widths/host')) and TryStrToInt(XMLNode(XML, 'objects/processes/widths/host').Text, ColumnWidths[lkProcesses][2])) then ColumnWidths[lkProcesses][2] := Round(ColumnWidths[lkProcesses][2] * Screen.PixelsPerInch / PixelsPerInch);
  if (Assigned(XMLNode(XML, 'objects/processes/widths/database')) and TryStrToInt(XMLNode(XML, 'objects/processes/widths/database').Text, ColumnWidths[lkProcesses][3])) then ColumnWidths[lkProcesses][3] := Round(ColumnWidths[lkProcesses][3] * Screen.PixelsPerInch / PixelsPerInch);
  if (Assigned(XMLNode(XML, 'objects/processes/widths/command')) and TryStrToInt(XMLNode(XML, 'objects/processes/widths/command').Text, ColumnWidths[lkProcesses][4])) then ColumnWidths[lkProcesses][4] := Round(ColumnWidths[lkProcesses][4] * Screen.PixelsPerInch / PixelsPerInch);
  if (Assigned(XMLNode(XML, 'objects/processes/widths/statement')) and TryStrToInt(XMLNode(XML, 'objects/processes/widths/statement').Text, ColumnWidths[lkProcesses][5])) then ColumnWidths[lkProcesses][5] := Round(ColumnWidths[lkProcesses][5] * Screen.PixelsPerInch / PixelsPerInch);
  if (Assigned(XMLNode(XML, 'objects/processes/widths/time')) and TryStrToInt(XMLNode(XML, 'objects/processes/widths/time').Text, ColumnWidths[lkProcesses][6])) then ColumnWidths[lkProcesses][6] := Round(ColumnWidths[lkProcesses][6] * Screen.PixelsPerInch / PixelsPerInch);
  if (Assigned(XMLNode(XML, 'objects/processes/widths/state')) and TryStrToInt(XMLNode(XML, 'objects/processes/widths/state').Text, ColumnWidths[lkProcesses][7])) then ColumnWidths[lkProcesses][7] := Round(ColumnWidths[lkProcesses][7] * Screen.PixelsPerInch / PixelsPerInch);
  if (Assigned(XMLNode(XML, 'objects/stati/widths/name')) and TryStrToInt(XMLNode(XML, 'objects/stati/widths/name').Text, ColumnWidths[lkStati][0])) then ColumnWidths[lkStati][0] := Round(ColumnWidths[lkStati][0] * Screen.PixelsPerInch / PixelsPerInch);
  if (Assigned(XMLNode(XML, 'objects/stati/widths/value')) and TryStrToInt(XMLNode(XML, 'objects/stati/widths/value').Text, ColumnWidths[lkStati][1])) then ColumnWidths[lkStati][1] := Round(ColumnWidths[lkStati][1] * Screen.PixelsPerInch / PixelsPerInch);
  if (Assigned(XMLNode(XML, 'objects/users/widths/name')) and TryStrToInt(XMLNode(XML, 'objects/users/widths/name').Text, ColumnWidths[lkUsers][0])) then ColumnWidths[lkUsers][0] := Round(ColumnWidths[lkUsers][0] * Screen.PixelsPerInch / PixelsPerInch);
  if (Assigned(XMLNode(XML, 'objects/users/widths/fullname')) and TryStrToInt(XMLNode(XML, 'objects/users/widths/fullname').Text, ColumnWidths[lkUsers][1])) then ColumnWidths[lkUsers][1] := Round(ColumnWidths[lkUsers][1] * Screen.PixelsPerInch / PixelsPerInch);
  if (Assigned(XMLNode(XML, 'objects/users/widths/comment')) and TryStrToInt(XMLNode(XML, 'objects/users/widths/comment').Text, ColumnWidths[lkUsers][2])) then ColumnWidths[lkUsers][2] := Round(ColumnWidths[lkUsers][2] * Screen.PixelsPerInch / PixelsPerInch);
  if (Assigned(XMLNode(XML, 'objects/variables/widths/name')) and TryStrToInt(XMLNode(XML, 'objects/variables/widths/name').Text, ColumnWidths[lkVariables][0])) then ColumnWidths[lkVariables][0] := Round(ColumnWidths[lkVariables][0] * Screen.PixelsPerInch / PixelsPerInch);
  if (Assigned(XMLNode(XML, 'objects/variables/widths/value')) and TryStrToInt(XMLNode(XML, 'objects/variables/widths/value').Text, ColumnWidths[lkVariables][1])) then ColumnWidths[lkVariables][1] := Round(ColumnWidths[lkVariables][1] * Screen.PixelsPerInch / PixelsPerInch);
  if (Assigned(XMLNode(XML, 'path'))) then FPath := XMLNode(XML, 'path').Text;
  if (Assigned(XMLNode(XML, 'sidebar/explorer/folders/height')) and TryStrToInt(XMLNode(XML, 'sidebar/explorer/folders/height').Text, FoldersHeight)) then FoldersHeight := Round(FoldersHeight * Screen.PixelsPerInch / PixelsPerInch);
  if (Assigned(XMLNode(XML, 'sidebar/explorer/files/filter'))) then FilesFilter := XMLNode(XML, 'sidebar/explorer/files/filter').Text;
  if (Assigned(XMLNode(XML, 'sidebar/width')) and TryStrToInt(XMLNode(XML, 'sidebar/width').Text, SidebarWitdth)) then SidebarWitdth := Round(SidebarWitdth * Screen.PixelsPerInch / PixelsPerInch);
  if (Assigned(XMLNode(XML, 'sidebar/visible'))) then
  begin
    NavigatorVisible := UpperCase(XMLNode(XML, 'sidebar/visible').Text) = 'NAVIGATOR';
    ExplorerVisible := not NavigatorVisible and (UpperCase(XMLNode(XML, 'sidebar/visible').Text) = 'EXPLORER');
    JobsVisible := not NavigatorVisible and not ExplorerVisible and (UpperCase(XMLNode(XML, 'sidebar/visible').Text) = 'JOBS');
    SQLHistoryVisible := not NavigatorVisible and not ExplorerVisible and not JobsVisible and (UpperCase(XMLNode(XML, 'sidebar/visible').Text) = 'SQL HISTORY');
  end;

  if Assigned(XMLNode(XML, 'editor/files')) then Files.LoadFromXML(XMLNode(XML, 'editor/files'));
end;

procedure TPAccount.TDesktop.SaveToXML(const XML: IXMLNode);
begin
  XML.OwnerDocument.Options := XML.OwnerDocument.Options + [doNodeAutoCreate];

  XML.OwnerDocument.DocumentElement.Attributes['pixelsperinch'] := IntToStr(Screen.PixelsPerInch);

  XMLNode(XML, 'datagrid/height').Text := IntToStr(DataHeight);
  XMLNode(XML, 'datagrid/blob/height').Text := IntToStr(BlobHeight);
  try XMLNode(XML, 'editor/content').Text := EditorContent[ttEditor]; except end;
  try XMLNode(XML, 'editor2/content').Text := EditorContent[ttEditor2]; except end;
  try XMLNode(XML, 'editor3/content').Text := EditorContent[ttEditor3]; except end;
  XMLNode(XML, 'log/height').Text := IntToStr(LogHeight);
  XMLNode(XML, 'log/visible').Text := BoolToStr(LogVisible, True);
  XMLNode(XML, 'objects/server/widths/name').Text := IntToStr(ColumnWidths[lkServer][0]);
  XMLNode(XML, 'objects/server/widths/count').Text := IntToStr(ColumnWidths[lkServer][1]);
  XMLNode(XML, 'objects/server/widths/size').Text := IntToStr(ColumnWidths[lkServer][2]);
  XMLNode(XML, 'objects/server/widths/created').Text := IntToStr(ColumnWidths[lkServer][3]);
  XMLNode(XML, 'objects/server/widths/extras').Text := IntToStr(ColumnWidths[lkServer][4]);
  XMLNode(XML, 'objects/database/widths/name').Text := IntToStr(ColumnWidths[lkDatabase][0]);
  XMLNode(XML, 'objects/database/widths/type').Text := IntToStr(ColumnWidths[lkDatabase][1]);
  XMLNode(XML, 'objects/database/widths/recordcount').Text := IntToStr(ColumnWidths[lkDatabase][2]);
  XMLNode(XML, 'objects/database/widths/size').Text := IntToStr(ColumnWidths[lkDatabase][3]);
  XMLNode(XML, 'objects/database/widths/updated').Text := IntToStr(ColumnWidths[lkDatabase][4]);
  XMLNode(XML, 'objects/database/widths/extras').Text := IntToStr(ColumnWidths[lkDatabase][5]);
  XMLNode(XML, 'objects/database/widths/comment').Text := IntToStr(ColumnWidths[lkDatabase][6]);
  XMLNode(XML, 'objects/table/widths/name').Text := IntToStr(ColumnWidths[lkTable][0]);
  XMLNode(XML, 'objects/table/widths/type').Text := IntToStr(ColumnWidths[lkTable][1]);
  XMLNode(XML, 'objects/table/widths/null').Text := IntToStr(ColumnWidths[lkTable][2]);
  XMLNode(XML, 'objects/table/widths/default').Text := IntToStr(ColumnWidths[lkTable][3]);
  XMLNode(XML, 'objects/table/widths/extras').Text := IntToStr(ColumnWidths[lkTable][4]);
  XMLNode(XML, 'objects/table/widths/comment').Text := IntToStr(ColumnWidths[lkTable][5]);
  XMLNode(XML, 'objects/processes/widths/id').Text := IntToStr(ColumnWidths[lkProcesses][0]);
  XMLNode(XML, 'objects/processes/widths/user').Text := IntToStr(ColumnWidths[lkProcesses][1]);
  XMLNode(XML, 'objects/processes/widths/host').Text := IntToStr(ColumnWidths[lkProcesses][2]);
  XMLNode(XML, 'objects/processes/widths/database').Text := IntToStr(ColumnWidths[lkProcesses][3]);
  XMLNode(XML, 'objects/processes/widths/command').Text := IntToStr(ColumnWidths[lkProcesses][4]);
  XMLNode(XML, 'objects/processes/widths/statement').Text := IntToStr(ColumnWidths[lkProcesses][5]);
  XMLNode(XML, 'objects/processes/widths/time').Text := IntToStr(ColumnWidths[lkProcesses][6]);
  XMLNode(XML, 'objects/processes/widths/state').Text := IntToStr(ColumnWidths[lkProcesses][7]);
  XMLNode(XML, 'objects/stati/widths/name').Text := IntToStr(ColumnWidths[lkStati][0]);
  XMLNode(XML, 'objects/stati/widths/value').Text := IntToStr(ColumnWidths[lkStati][1]);
  XMLNode(XML, 'objects/users/widths/name').Text := IntToStr(ColumnWidths[lkUsers][0]);
  XMLNode(XML, 'objects/users/widths/fullname').Text := IntToStr(ColumnWidths[lkUsers][1]);
  XMLNode(XML, 'objects/users/widths/comment').Text := IntToStr(ColumnWidths[lkUsers][2]);
  XMLNode(XML, 'objects/variables/widths/name').Text := IntToStr(ColumnWidths[lkVariables][0]);
  XMLNode(XML, 'objects/variables/widths/value').Text := IntToStr(ColumnWidths[lkVariables][1]);
  XMLNode(XML, 'path').Text := FPath;
  XMLNode(XML, 'sidebar/explorer/folders/height').Text := IntToStr(FoldersHeight);
  XMLNode(XML, 'sidebar/explorer/files/filter').Text := FilesFilter;
  XMLNode(XML, 'sidebar/width').Text := IntToStr(SidebarWitdth);
  if (NavigatorVisible) then
    XMLNode(XML, 'sidebar/visible').Text := 'Navigator'
  else if (ExplorerVisible) then
    XMLNode(XML, 'sidebar/visible').Text := 'Explorer'
  else if (JobsVisible) then
    XMLNode(XML, 'sidebar/visible').Text := 'Jobs'
  else if (SQLHistoryVisible) then
    XMLNode(XML, 'sidebar/visible').Text := 'SQL History'
  else
    XMLNode(XML, 'sidebar/visible').Text := BoolToStr(False, True);

  Files.SaveToXML(XMLNode(XML, 'editor/files', True));

  XML.OwnerDocument.Options := XML.OwnerDocument.Options - [doNodeAutoCreate];
end;

procedure TPAccount.TDesktop.SetAddress(AAddress: string);
begin
  FPath := Account.ExtractPath(AAddress);
end;

{ TPAccount.TConnection *******************************************************}

procedure TPAccount.TConnection.Assign(const Source: TConnection);
begin
  Database := Source.Database;
  Host := Source.Host;
  HTTPTunnelURI := Source.HTTPTunnelURI;
  LibraryFilename := Source.LibraryFilename;
  LibraryType := Source.LibraryType;
  Password := Source.Password;
  Port := Source.Port;
  Username := Source.Username;
end;

constructor TPAccount.TConnection.Create();
begin
  inherited Create();

  Database := '';
  Host := '';
  HTTPTunnelURI := '';
  LibraryFilename := 'libMySQL.dll';
  LibraryType := ltBuiltIn;
  Password := '';
  Port := MYSQL_PORT;
  Username := '';
end;

procedure TPAccount.TConnection.LoadFromXML(const XML: IXMLNode);
begin
  if (Assigned(XMLNode(XML, 'database'))) then Database := XMLNode(XML, 'database').Text;
  if (Assigned(XMLNode(XML, 'host'))) then Host := XMLNode(XML, 'host').Text;
  if (Assigned(XMLNode(XML, 'library/type'))) then
    if (UpperCase(XMLNode(XML, 'library/type').Text) = 'FILE') then LibraryType := ltDLL
    else if (UpperCase(XMLNode(XML, 'library/type').Text) = 'TUNNEL') then LibraryType := ltHTTP
    else if (UpperCase(XMLNode(XML, 'library/type').Text) = 'HTTPTUNNEL') then LibraryType := ltHTTP
    else LibraryType := ltBuiltIn;
  if (Assigned(XMLNode(XML, 'library/filename'))) then LibraryFilename := XMLNode(XML, 'library/filename').Text;
  if (Assigned(XMLNode(XML, 'library/tunnel_url'))) then HTTPTunnelURI := XMLNode(XML, 'library/tunnel_url').Text;
  if (Assigned(XMLNode(XML, 'password')) and (XMLNode(XML, 'password').Attributes['encode'] = 'none')) then Password := XMLNode(XML, 'password').Text;
  if (Assigned(XMLNode(XML, 'port'))) then TryStrToInt(XMLNode(XML, 'port').Text, Port);
  if (Assigned(XMLNode(XML, 'user'))) then Username := XMLNode(XML, 'user').Text;
end;

procedure TPAccount.TConnection.SaveToXML(const XML: IXMLNode);
begin
  XMLNode(XML, 'database').Text := Database;
  XMLNode(XML, 'host').Text := Host;
  case (LibraryType) of
    ltDLL: XMLNode(XML, 'library/type').Text := 'File';
    ltHTTP: XMLNode(XML, 'library/type').Text := 'HTTPTunnel';
    else XMLNode(XML, 'library').ChildNodes.Delete('type');
  end;
  XMLNode(XML, 'library/filename').Text := LibraryFilename;
  XMLNode(XML, 'library/tunnel_url').Text := HTTPTunnelURI;
  XMLNode(XML, 'password').Attributes['encode'] := 'none';
  XMLNode(XML, 'password').Text := Password;
  XMLNode(XML, 'port').Text := IntToStr(Port);
  XMLNode(XML, 'user').Text := Username;
end;

{ TPAccount *******************************************************************}

procedure TPAccount.AccountEvent(const ClassType: TClass);
var
  I: Integer;
begin
  for I := 0 to Length(FDesktops) - 1 do
    if (Assigned(FDesktops[I].AccountEventProc)) then
      FDesktops[I].AccountEventProc(ClassType);
end;

procedure TPAccount.Assign(const Source: TPAccount);
begin
  if (not Assigned(Accounts)) then FAccounts := Source.Accounts;

  FLastLogin := Source.LastLogin;
  ManualURL := Source.ManualURL;
  ManualURLFetched := Source.ManualURLFetched;
  Name := Source.Name;

  Modified := True;

  Connection.Assign(Source.Connection);
  if (Assigned(FDesktop) and Assigned(Source.FDesktop)) then
    FDesktop.Assign(Source.FDesktop);
end;

constructor TPAccount.Create(const AAccounts: TPAccounts; const AXML: IXMLNode = nil);
var
  Node: IXMLNode;
begin
  FAccounts := AAccounts;
  FXML := AXML;

  FHistoryXMLDocument := nil;
  FLastLogin := 0;
  ManualURL := '';
  ManualURLFetched := False;
  Modified := False;

  Connection := TConnection.Create();
  FJobs := nil;

  FDesktop := TDesktop.Create(Self);

  if (FileExists(DesktopFilename)) then
    try
      DesktopXMLDocument := LoadXMLDocument(DesktopFilename);
    except
      DesktopXMLDocument := nil;
    end;

  if (not Assigned(DesktopXMLDocument) or not Assigned(DesktopXMLDocument.DocumentElement)) then
  begin
    DesktopXMLDocument := NewXMLDocument();
    DesktopXMLDocument.Encoding := 'utf-8';
    DesktopXMLDocument.Node.AddChild('desktop').Attributes['version'] := '1.3.1';
  end;

  if (DesktopXMLDocument.DocumentElement.Attributes['version'] <> Null) then
  begin
    if (VersionStrToVersion(DesktopXMLDocument.DocumentElement.Attributes['version']) < 10300)  then
    begin
      Node := DesktopXMLDocument.DocumentElement;
      if (Assigned(Node) and Assigned(XMLNode(Node, 'address'))) then
        Node.ChildNodes.Remove(XMLNode(Node, 'address'));

      DesktopXMLDocument.DocumentElement.Attributes['version'] := '1.3';
    end;

    if (VersionStrToVersion(DesktopXMLDocument.DocumentElement.Attributes['version']) < 10301)  then
    begin
      Node := DesktopXMLDocument.DocumentElement;
      if (Assigned(XMLNode(Node, 'browser'))) then
        Node.ChildNodes.Remove(XMLNode(Node, 'browser'));
      Node := XMLNode(Node, 'editor');
      if (Assigned(XMLNode(Node, 'filename'))) then
        Node.ChildNodes.Remove(XMLNode(Node, 'filename'));

      DesktopXMLDocument.DocumentElement.Attributes['version'] := '1.3.1';
    end;

    if (VersionStrToVersion(DesktopXMLDocument.DocumentElement.Attributes['version']) < 10400)  then
    begin
      DesktopXMLDocument.DocumentElement.Attributes['pixelsperinch'] := IntToStr(Screen.PixelsPerInch);

      DesktopXMLDocument.DocumentElement.Attributes['version'] := '1.4.0';
    end;
  end;

  DesktopXMLDocument.Options := DesktopXMLDocument.Options - [doAttrNull, doNodeAutoCreate];
end;

destructor TPAccount.Destroy();
begin
  if (Assigned(FDesktop)) then FDesktop.Free();
  if (Assigned(FJobs)) then FJobs.Free();
  Connection.Free();

  inherited;
end;

function TPAccount.ExtractPath(const AAddress: string): string;
var
  URI: TUURI;
begin
  URI := TUURI.Create(AAddress);
  Result := URI.Path + URI.ExtraInfos;
  URI.Free();
end;

function TPAccount.ExpandAddress(const APath: string): string;
var
  Len: Cardinal;
  URL: array[0 .. INTERNET_MAX_URL_LENGTH] of Char;
  URLComponents: URL_COMPONENTS;
begin
  ZeroMemory(@URLComponents, SizeOf(URLComponents));
  URLComponents.dwStructSize := SizeOf(URLComponents);

  URLComponents.lpszScheme := PChar('mysql');
  if (Connection.Host = LOCAL_HOST_NAMEDPIPE) then
    URLComponents.lpszHostName := LOCAL_HOST
  else
    URLComponents.lpszHostName := PChar(Connection.Host);
  if (Connection.Port <> MYSQL_PORT) then
    URLComponents.nPort := Connection.Port;
  URLComponents.lpszUrlPath := PChar(APath);

  Len := Length(URL);
  if (not InternetCreateUrl(URLComponents, ICU_ESCAPE, @URL, Len)) then
    RaiseLastOSError()
  else
    SetString(Result, PChar(@URL), Len);
end;

function TPAccount.Frame(): Pointer;
begin
  if (Length(FDesktops) = 0) then
    Result := nil
  else
    Result := FDesktops[0].Control;
end;

function TPAccount.GetDataPath(): TFileName;
begin
  Result := Accounts.DataPath + ReplaceStr(Name, '/', '_') + PathDelim;
end;

function TPAccount.GetDefaultDatabase(): string;
var
  DatabaseNames: TCSVStrings;
  Found: Boolean;
  I: Integer;
  URI: TUURI;
begin
  Result := '';

  if (Assigned(FDesktop)) then
  begin
    URI := TUURI.Create(FDesktop.Address);

    if (ValidDatabaseName(URI.Database)) then
    begin
      Result := URI.Database;

      if (Connection.Database <> '') then
      begin
        SetLength(DatabaseNames, 0);
        CSVSplitValues(Connection.Database, ',', '"', DatabaseNames);

        Found := False;
        for I := 0 to Length(DatabaseNames) - 1 do
          if (lstrcmpi(PChar(DatabaseNames[I]), PChar(Result)) = 0) then
            Found := True;
        if (not Found) then
          Result := '';

        SetLength(DatabaseNames, 0);
      end;
    end;

    URI.Free();

    if (Result = '') then
    begin
      SetLength(DatabaseNames, 0);
      CSVSplitValues(Connection.Database, ',', '"', DatabaseNames);

      if (Length(DatabaseNames) = 0) then
        Result := ''
      else
        Result := DatabaseNames[0];

      SetLength(DatabaseNames, 0);
    end;
  end;
end;

function TPAccount.GetDesktopCount(): Integer;
begin
  Result := Length(FDesktops);
end;

function TPAccount.GetDesktopXML(): IXMLNode;
begin
  if (not Assigned(DesktopXMLDocument)) then
    Result := nil
  else
    Result := DesktopXMLDocument.DocumentElement;
end;

function TPAccount.GetDesktopFilename(): TFileName;
begin
  if (not DirectoryExists(DataPath)) then
    Result := ''
  else
    Result := DataPath + 'Desktop.xml';
end;

function TPAccount.GetHistoryFilename(): TFileName;
begin
  if (not DirectoryExists(DataPath)) then
    Result := ''
  else
    Result := DataPath + 'History.xml';
end;

function TPAccount.GetHistoryXML(): IXMLNode;
begin
  if (not Assigned(FHistoryXMLDocument)) then
  begin
    if (FileExists(HistoryFilename)) then
      try
        FHistoryXMLDocument := LoadXMLDocument(HistoryFilename);
      except
        // If there are errors inside the History.xml the user should not bothered with it...
        FHistoryXMLDocument := nil;
      end;

    if (not Assigned(FHistoryXMLDocument) or not Assigned(FHistoryXMLDocument.DocumentElement)) then
    begin
      FHistoryXMLDocument := NewXMLDocument();
      FHistoryXMLDocument.Encoding := 'utf-8';
      FHistoryXMLDocument.Node.AddChild('history').Attributes['version'] := '1.0';
    end;

    FHistoryXMLDocument.Options := FHistoryXMLDocument.Options - [doAttrNull, doNodeAutoCreate];
  end;

  Result := FHistoryXMLDocument.DocumentElement;
end;

function TPAccount.GetIndex(): Integer;
begin
  Result := Accounts.IndexOf(Self);
end;

function TPAccount.GetJobs(): TPAccount.TJobs;
begin
  if (not Assigned(FJobs) and CheckWin32Version(6)) then
  begin
    FJobs := TPAccount.TJobs.Create(Self);
    FJobs.Load();
  end;

  Result := FJobs;
end;

function TPAccount.GetName(): string;
begin
  if ((FName = '') and Assigned(XML)) then
    FName := XML.Attributes['name'];

  Result := FName;
end;

function TPAccount.JobByName(const Name: string): TPAccount.TJob;
var
  Index: Integer;
begin
  Index := Jobs.IndexByName(Name);
  if (Index < 0) then
    Result := nil
  else
    Result := Jobs[Index];
end;

procedure TPAccount.Load();
begin
  if (Assigned(XML)) then
  begin
    if (Assigned(XMLNode(XML, 'lastlogin'))) then
      TryStrToFloat(ReplaceStr(XMLNode(XML, 'lastlogin').Text, '.', FormatSettings.DecimalSeparator), Double(FLastLogin));
    if (Assigned(XMLNode(XML, 'manualurl'))) then ManualURL := XMLNode(XML, 'manualurl').Text;
    if (Assigned(XMLNode(XML, 'manualurlfetched'))) then TryStrToBool(XMLNode(XML, 'manualurlfetched').Text, ManualURLFetched);

    Modified := False;

    if (Assigned(XMLNode(XML, 'connection'))) then Connection.LoadFromXML(XMLNode(XML, 'connection'));
    if (Assigned(FDesktop) and Assigned(DesktopXMLDocument.DocumentElement)) then
      FDesktop.LoadFromXML(DesktopXMLDocument.DocumentElement); // Session must be loaded to use FullAddress correctly
  end;
end;

procedure TPAccount.RegisterDesktop(const AControl: Pointer; const AEventProc: TEventProc);
begin
  SetLength(FDesktops, Length(FDesktops) + 1);
  FDesktops[Length(FDesktops) - 1].Control := AControl;
  FDesktops[Length(FDesktops) - 1].AccountEventProc := AEventProc;
end;

procedure TPAccount.Save();
begin
  if (Assigned(XML)) then
  begin
    XML.Attributes['name'] := Name;

    XMLNode(XML, 'lastlogin').Text := FloatToStr(LastLogin);
    XMLNode(XML, 'manualurl').Text := ManualURL;
    XMLNode(XML, 'manualurlfetched').Text := BoolToStr(ManualURLFetched, True);

    Connection.SaveToXML(XMLNode(XML, 'connection', True));

    if (Assigned(FDesktop)) then
      FDesktop.SaveToXML(DesktopXMLDocument.DocumentElement);

    if (ForceDirectories(DataPath)) then
    begin
      try
        if (DesktopXMLDocument.Modified) then
          if (ForceDirectories(ExtractFilePath(DesktopFilename))) then
            DesktopXMLDocument.SaveToFile(DesktopFilename);
        if (Assigned(HistoryXMLDocument) and HistoryXMLDocument.Modified) then
          if (ForceDirectories(ExtractFilePath(HistoryFilename))) then
            HistoryXMLDocument.SaveToFile(HistoryFilename);
      except
        // Do not inform user about problems while saving file
      end;
    end;

    Modified := False;
  end;
end;

procedure TPAccount.SetLastLogin(const ALastLogin: TDateTime);
begin
  FLastLogin := ALastLogin;

  Modified := True;
end;

procedure TPAccount.UnRegisterDesktop(const AControl: Pointer);
var
  I: Integer;
  J: Integer;
begin
  I := 0;
  while (I < Length(FDesktops)) do
    if (FDesktops[I].Control <> AControl) then
      Inc(I)
    else
    begin
      for J := I to Length(FDesktops) - 2 do
        FDesktops[J] := FDesktops[J + 1];
      SetLength(FDesktops, Length(FDesktops) - 1);
    end;
end;

function TPAccount.ValidDatabaseName(const ADatabaseName: string): Boolean;
var
  S: string;
  TempDatabaseName: string;
begin
  Result := False;
  S := Connection.Database;

  while (S <> '') do
    if (Pos(',', S) = 0) then
      begin
        if (S = ADatabaseName) then
          Result := True;
        S := '';
      end
    else
      begin
        TempDatabaseName := Copy(S, 1, Pos(',', S) - 1);
        Delete(S, 1, Pos(',', S));
        if (TempDatabaseName = ADatabaseName) then
          Result := True;
      end;
end;

{ TAAccounts ******************************************************************}

procedure TPAccounts.AddAccount(const NewAccount: TPAccount);
var
  XML: IXMLNode;
begin
  if (NewAccount.Name = '') then
    raise ERangeError.CreateFmt(SPropertyOutOfRange, ['Name'])
  else if (Assigned(AccountByName(NewAccount.Name))) then
    raise ERangeError.CreateFmt(SPropertyOutOfRange, ['Name'])
  else
  begin
    XML := Accounts.XMLDocument.DocumentElement.AddChild('account');
    XML.Attributes['name'] := NewAccount.Name;
    Add(TPAccount.Create(Self, XML));
    Account[Count - 1].Assign(NewAccount);
  end;
end;

procedure TPAccounts.Clear();
begin
  while (Count > 0) do
  begin
    Account[0].Free();
    Delete(0);
  end;

  inherited;
end;

constructor TPAccounts.Create(const ADBLogin: TDBLogin);
var
  Msg: string;
  StringList: TStringList;
begin
  inherited Create();

  FDBLogin := ADBLogin;

  DefaultAccountName := '';
  FXMLDocument := nil;

  Section := 'Accounts';

  // "Sessions" used up to version 5.1 // May 2012
  if (DirectoryExists(Preferences.UserPath + 'Sessions' + PathDelim)
    and not DirectoryExists(DataPath)
    and not CopyDir(PChar(Preferences.UserPath + 'Sessions' + PathDelim), PChar(DataPath))) then
  begin
    Msg := 'Cannot copy folder'  + #13#10
      + Preferences.UserPath + 'Sessions' + PathDelim + #13#10
      + 'to' + #13#10
      + DataPath + #13#10
      + '(Error #' + IntToStr(GetLastError()) + ': ' + SysErrorMessage(GetLastError()) + ')';
    MessageBox(0, PChar(Msg), 'ERROR', MB_OK + MB_ICONERROR);
  end;
  if (not FileExists(Filename)
    and FileExists(DataPath + 'Sessions.xml')
    and CopyFile(PChar(DataPath + 'Sessions.xml'), PChar(Filename), False)) then
    begin
      StringList := TStringList.Create();
      StringList.LoadFromFile(Filename);
      StringList.Text := ReplaceStr(StringList.Text, '<session', '<account');
      StringList.Text := ReplaceStr(StringList.Text, '</session', '</account');
      StringList.SaveToFile(Filename);
      StringList.Free();
    end;

  Load();
end;

function TPAccounts.DeleteAccount(const AAccount: TPAccount): Boolean;
var
  I: Integer;
  Index: Integer;
  TaskFolder: ITaskFolder;
  Tasks: IRegisteredTaskCollection;
begin
  if (FileExists(AAccount.DesktopFilename)) then
    DeleteFile(PChar(AAccount.DesktopFilename));
  if (FileExists(AAccount.HistoryFilename)) then
    DeleteFile(PChar(AAccount.HistoryFilename));
  if (DirectoryExists(AAccount.DataPath)) then
    RemoveDirectory(PChar(AAccount.DataPath));

  if (Assigned(Preferences.TaskService)
    and Succeeded(Preferences.TaskService.GetFolder(TBStr('\' + SysUtils.LoadStr(1006) + '\Accounts\' + AAccount.Name), TaskFolder))
    and Succeeded(TaskFolder.GetTasks(0, Tasks))) then
  begin
    for I := 0 to Tasks.Count - 1 do
      TaskFolder.DeleteTask(TBStr(Tasks.Item[1 + I].Name), 0);
    if (Succeeded(Preferences.TaskService.GetFolder(TBStr('\' + SysUtils.LoadStr(1006) + '\Accounts'), TaskFolder))) then
      TaskFolder.DeleteFolder(TBStr(AAccount.Name), 0);
  end;

  for I := XMLDocument.DocumentElement.ChildNodes.Count - 1 downto 0 do
    if ((XMLDocument.DocumentElement.ChildNodes[I].NodeName = 'account') and (lstrcmpi(PChar(string(XMLDocument.DocumentElement.ChildNodes[I].Attributes['name'])), PChar(AAccount.Name)) = 0)) then
      XMLDocument.DocumentElement.ChildNodes.Remove(XMLDocument.DocumentElement.ChildNodes[I]);

  Index := IndexOf(AAccount);

  Account[Index].Free();
  Delete(Index);

  Save();

  Result := True;
end;

destructor TPAccounts.Destroy();
begin
  Save();

  Clear();

  inherited;
end;

function TPAccounts.GetDataPath(): TFileName;
begin
  Result := Preferences.UserPath + 'Accounts' + PathDelim;
end;

function TPAccounts.GetDefault(): TPAccount;
begin
  Result := AccountByName(DefaultAccountName);
end;

function TPAccounts.GetFilename(): TFileName;
begin
  Result := DataPath + 'Accounts.xml';
end;

function TPAccounts.GetFAccounts(Index: Integer): TPAccount;
begin
  Result := TPAccount(Items[Index]);
end;

function TPAccounts.GetXMLDocument(): IXMLDocument;
begin
  if (not Assigned(FXMLDocument)) then
  begin
    if (FileExists(Filename)) then
    begin
      FXMLDocument := LoadXMLDocument(Filename);
    end
    else
    begin
      FXMLDocument := NewXMLDocument();
      FXMLDocument.Encoding := 'utf-8';
      FXMLDocument.Node.AddChild('accounts').Attributes['version'] := '1.1.0';
    end;

    FXMLDocument.Options := FXMLDocument.Options - [doAttrNull, doNodeAutoCreate];
  end;

  Result := FXMLDocument;
end;

procedure TPAccounts.Load();
var
  I: Integer;
  Index: Integer;
  J: Integer;
begin
  Clear();

  XMLDocument.Options := XMLDocument.Options - [doNodeAutoCreate];

  for I := 0 to XMLDocument.DocumentElement.ChildNodes.Count - 1 do
  begin
    if ((XMLDocument.DocumentElement.ChildNodes[I].NodeName = 'account') and (XMLDocument.DocumentElement.ChildNodes[I].Attributes['name'] <> '')) then
    begin
      Index := TList(Self).Count;
      for J := TList(Self).Count - 1 downto 0 do
        if (lstrcmpi(PChar(string(XMLDocument.DocumentElement.ChildNodes[I].Attributes['name'])), PChar(Account[J].Name)) <= 0) then
          Index := J;

      Insert(Index, TPAccount.Create(Self, XMLDocument.DocumentElement.ChildNodes[I]));
      Account[Index].Load();
    end;
  end;

  if (Assigned(XMLNode(XMLDocument.DocumentElement, 'default'))) then
    DefaultAccountName := XMLNode(XMLDocument.DocumentElement, 'default').Text;

  XMLDocument.Options := XMLDocument.Options + [doNodeAutoCreate];
end;

procedure TPAccounts.Save();
var
  I: Integer;
begin
  XMLDocument.Options := XMLDocument.Options + [doNodeAutoCreate];

  for I := 0 to Count - 1 do
    if (Account[I].Modified) then
      Account[I].Save();

  XMLNode(XMLDocument.DocumentElement, 'default').Text := DefaultAccountName;

  XMLDocument.Options := XMLDocument.Options - [doNodeAutoCreate];


  if (XMLDocument.Modified) then
    if (ForceDirectories(ExtractFilePath(Filename))) then
      XMLDocument.SaveToFile(Filename);
end;

function TPAccounts.AccountByName(const AccountName: string): TPAccount;
var
  I: Integer;
begin
  Result := nil;

  for I:=0 to Count - 1 do
    if (Account[I].Name = AccountName) then
      Result := Account[I];
end;

function TPAccounts.AccountByURI(const AURI: string; const DefaultAccount: TPAccount = nil): TPAccount;
var
  Found: Integer;
  Host: string;
  I: Integer;
  Name: string;
  NewAccount: TPAccount;
  NewAccountName: string;
  URI: TUURI;
  URLComponents: TURLComponents;
begin
  Result := nil;

  URI := nil;
  if (LowerCase(Copy(AURI, 1, 8)) = 'mysql://') then
    try
      URI := TUURI.Create(AURI);
    except
      URI := nil;
    end;

  if (Assigned(URI)) then
  begin
    Found := 0;
    for I := 0 to Count - 1 do
    begin
      ZeroMemory(@URLComponents, SizeOf(URLComponents));
      URLComponents.dwStructSize := SizeOf(URLComponents);
      if ((Account[I].Connection.LibraryType <> ltHTTP) or (lstrcmpi(PChar(Account[I].Connection.Host), LOCAL_HOST) <> 0)) then
        Host := LowerCase(Account[I].Connection.Host)
      else if (InternetCrackUrl(PChar(Account[I].Connection.HTTPTunnelURI), Length(Account[I].Connection.HTTPTunnelURI), 0, URLComponents)) then
      begin
        Inc(URLComponents.dwHostNameLength);
        GetMem(URLComponents.lpszHostName, URLComponents.dwHostNameLength * SizeOf(URLComponents.lpszHostName[0]));
        InternetCrackUrl(PChar(Account[I].Connection.HTTPTunnelURI), Length(Account[I].Connection.HTTPTunnelURI), 0, URLComponents);
        SetString(Host, URLComponents.lpszHostName, URLComponents.dwHostNameLength);
        FreeMem(URLComponents.lpszHostName);
      end
      else
        Host := LOCAL_HOST;
      if ((lstrcmpi(PChar(Host), PChar(URI.Host)) = 0) and (URI.Port = Account[I].Connection.Port)
        and ((URI.Username = '') or (lstrcmpi(PChar(URI.Username), PChar(Account[I].Connection.Username)) = 0))) then
      begin
        Result := Account[I];
        Inc(Found);
        if ((Result = DefaultAccount) or (Result.DesktopCount > 0)) then
          break;
      end;
    end;

    if (Found = 0) then
    begin
      NewAccountName := URI.Host;
      if (URI.Username <> '') then
        NewAccountName := NewAccountName + ' (' + URI.Username + ')';
      Name := NewAccountName;
      I := 1;
      while (Assigned(AccountByName(Name))) do
      begin
        Inc(I);
        Name := NewAccountName + ' (' + IntToStr(I) + ')';
      end;

      NewAccount := TPAccount.Create(Self);
      NewAccount.Name := Name;
      NewAccount.Connection.Host := URI.Host;
      NewAccount.Connection.Port := URI.Port;
      NewAccount.Connection.Username := URI.Username;
      NewAccount.Connection.Password := URI.Password;
      NewAccount.Connection.Database := URI.Database;
      AddAccount(NewAccount);
      NewAccount.Free();

      Result := AccountByName(NewAccountName);

      Save();
    end;

    URI.Free();
  end;
end;

procedure TPAccounts.SetDefault(const AAccount: TPAccount);
begin
  if (not Assigned(AAccount)) then
    DefaultAccountName := ''
  else
    DefaultAccountName := AAccount.Name;
end;

procedure TPAccounts.UpdateAccount(const Account, NewAccount: TPAccount);
var
  I: Integer;
  NewJob: TPAccount.TJob;
  TaskFolder: ITaskFolder;
begin
  if (Assigned(Account) and Assigned(NewAccount) and (not Assigned(AccountByName(NewAccount.Name)) or (NewAccount.Name = Account.Name))) then
  begin
    if (NewAccount.Name <> Account.Name) then
    begin
      if (DirectoryExists(Account.DataPath)) then
        RenameFile(Account.DataPath, NewAccount.DataPath);

      if (Assigned(Account.Jobs)) then
        for I := Account.Jobs.Count - 1 downto 0 do
        begin
          if (Account.Jobs[I] is TPAccount.TJobExport) then
            NewJob := TPAccount.TJobExport.Create(NewAccount.Jobs, Account.Jobs[I].Name)
          else if (Account.Jobs[I] is TPAccount.TJobImport) then
            NewJob := TPAccount.TJobImport.Create(NewAccount.Jobs, Account.Jobs[I].Name)
          else
            NewJob := nil;
          NewJob.Assign(Account.Jobs[I]);
          NewAccount.Jobs.AddJob(NewJob);
          NewJob.Free();
          Account.Jobs.DeleteJob(Account.Jobs[I]);
        end;
      if (Assigned(Preferences.TaskService)
        and Succeeded(Preferences.TaskService.GetFolder(TBStr('\' + SysUtils.LoadStr(1006) + '\Accounts'), TaskFolder))) then
        TaskFolder.DeleteFolder(TBStr(Account.Name), 0);
    end;

    Account.Assign(NewAccount);

    Save();
  end;
end;

{******************************************************************************}

initialization
  Preferences := nil;

  FileFormatSettings := TFormatSettings.Create(LOCALE_SYSTEM_DEFAULT);
  FileFormatSettings.ThousandSeparator := ',';
  FileFormatSettings.DecimalSeparator := '.';
  FileFormatSettings.DateSeparator := '/';
  FileFormatSettings.TimeSeparator := ':';
  FileFormatSettings.ListSeparator := ',';
  FileFormatSettings.ShortDateFormat := 'dd/mm/yy';
  FileFormatSettings.LongDateFormat := 'dd/mm/yyyy';
  FileFormatSettings.TimeAMString := '';
  FileFormatSettings.TimePMString := '';
  FileFormatSettings.ShortTimeFormat := 'hh:mm';
  FileFormatSettings.LongTimeFormat := 'hh:mm:ss';

  CoInitialize(nil);
finalization
  CoUninitialize();
end.

