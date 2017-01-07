unit uFSession;

interface {********************************************************************}

uses
  Forms, Windows, Messages, SysUtils, Classes, Graphics, Controls,
  Dialogs, ActnList, ComCtrls, ExtCtrls, Menus, StdCtrls, DB, DBGrids, Grids,
  DBCtrls, DBActns, StdActns, ImgList, XMLIntf, Actions,
  ShDocVw, CommCtrl, PNGImage, GIFImg, Jpeg, ToolWin,
  MPHexEditor, MPHexEditorEx,
  SynEdit, SynEditHighlighter, SynHighlighterSQL, SynMemo, SynEditMiscClasses,
  SynEditSearch, SynCompletionProposal,
  acQBBase, acAST, acQBEventMetaProvider, acMYSQLSynProvider, acSQLBuilderPlainText,
  ShellControls, JAMControls, ShellLink,
  ComCtrls_Ext, StdCtrls_Ext, Dialogs_Ext, Forms_Ext, ExtCtrls_Ext,
  MySQLDB, MySQLDBGrid, SQLParser,
  uSession, uPreferences, uTools,
  uBase, uDExport, uDImport, uCWorkbench, uFObjectSearch;

const
  UM_ACTIVATE_DBGRID = WM_USER + 500;
  UM_ACTIVATEFRAME = WM_USER + 501;
  UM_ACTIVATEFTEXT = WM_USER + 502;
  UM_CLOSE_FRAME = WM_USER + 503;
  UM_POST_BUILDER_QUERY_CHANGE = WM_USER + 505;
  UM_SYNCOMPLETION_TIMER = WM_USER + 506;
  UM_WANTED_SYNCHRONIZE = WM_USER + 507;
  UM_STATUS_BAR_REFRESH = WM_USER + 508;

const
  sbMessage = 0;
  sbNavigation = 1;
  sbSummarize = 2;

type
  TSynMemoBeforeDrag = record SelStart: Integer; SelLength: Integer; end;
  TListViewSortRec = record Kind: TPAccount.TDesktop.TListViewKind; Index: Integer; Order: Integer; end;
  TListViewSortData = array [lkServer .. lkVariables] of TListViewSortRec;

type
  TFSession = class (TFrame)
    ActionList: TActionList;
    aDCreate: TAction;
    aDDelete: TAction;
    aDDeleteRecord: TDataSetDelete;
    aDInsertRecord: TDataSetInsert;
    aDNext: TAction;
    aDPrev: TAction;
    aEClearAll: TAction;
    aPCollapse: TAction;
    aPExpand: TAction;
    aPObjectBrowserTable: TAction;
    aPResult: TAction;
    aSynCompletionExecute: TAction;
    aTBFilter: TAction;
    aTBLimit: TAction;
    aTBOffset: TAction;
    aTBQuickSearch: TAction;
    aVBlobHexEditor: TAction;
    aVBlobHTML: TAction;
    aVBlobImage: TAction;
    aVBlobRTF: TAction;
    aVBlobText: TAction;
    BDELETE: TButton;
    BINSERT: TButton;
    BREPLACE: TButton;
    BUPDATE: TButton;
    DataSetCancel: TDataSetCancel;
    DataSetDelete: TDataSetDelete;
    DataSetFirst: TDataSetFirst;
    DataSetLast: TDataSetLast;
    DataSetPost: TDataSetPost;
    FFilter: TComboBox_Ext;
    FFilterEnabled: TToolButton;
    FGridDataSource: TDataSource;
    FHexEditor: TMPHexEditorEx;
    FImage: TImage;
    FLimit: TEdit;
    FLimitEnabled: TToolButton;
    FLog: TRichEdit;
    FNavigator: TTreeView_Ext;
    FObjectIDEGrid: TMySQLDBGrid;
    FObjectSearch: TEdit;
    FObjectSearchStart: TToolButton;
    FOffset: TEdit;
    FQueryBuilder: TacQueryBuilder;
    FQueryBuilderSynMemo: TSynMemo;
    FQuickSearch: TEdit;
    FQuickSearchEnabled: TToolButton;
    FRTF: TRichEdit;
    FServerListView: TListView_Ext;
    FSQLEditorSearch: TSynEditSearch;
    FSQLEditorSynMemo: TSynMemo;
    FSQLHistory: TTreeView_Ext;
    FText: TRichEdit;
    FUDLimit: TUpDown;
    FUDOffset: TUpDown;
    ghmCopy: TMenuItem;
    ghmGoto: TMenuItem;
    gmDDeleteRecord: TMenuItem;
    gmDEditRecord: TMenuItem;
    gmDInsertRecord: TMenuItem;
    gmECopy: TMenuItem;
    gmECopyToFile: TMenuItem;
    gmECut: TMenuItem;
    gmEDelete: TMenuItem;
    gmEmpty: TMenuItem;
    gmEPaste: TMenuItem;
    gmEPasteFromFile: TMenuItem;
    gmFExport: TMenuItem;
    gmFExportExcel: TMenuItem;
    gmFExportHTML: TMenuItem;
    gmFExportPDF: TMenuItem;
    gmFExportSQL: TMenuItem;
    gmFExportText: TMenuItem;
    gmFExportXML: TMenuItem;
    gmFilter: TMenuItem;
    mfDelete: TMenuItem;
    mfFilter: TMenuItem;
    mfFilterAccess: TMenuItem;
    mfFilterClear: TMenuItem;
    mfFilterExcel: TMenuItem;
    mfFilterHTML: TMenuItem;
    mfFilterSQL: TMenuItem;
    mfFilterText: TMenuItem;
    mfFilterXML: TMenuItem;
    MFiles: TPopupMenu;
    mfOpen: TMenuItem;
    mfProperties: TMenuItem;
    mfRename: TMenuItem;
    MGrid: TPopupMenu;
    MGridHeader: TPopupMenu;
    miHCollapse: TMenuItem;
    miHCopy: TMenuItem;
    miHExpand: TMenuItem;
    miHOpen: TMenuItem;
    miHProperties: TMenuItem;
    miHRun: TMenuItem;
    miHSaveAs: TMenuItem;
    miHStatementIntoSQLEditor: TMenuItem;
    miNCollapse: TMenuItem;
    miNCopy: TMenuItem;
    miNCreate: TMenuItem;
    miNCreateDatabase: TMenuItem;
    miNCreateEvent: TMenuItem;
    miNCreateField: TMenuItem;
    miNCreateForeignKey: TMenuItem;
    miNCreateFunction: TMenuItem;
    miNCreateIndex: TMenuItem;
    miNCreateProcedure: TMenuItem;
    miNCreateTable: TMenuItem;
    miNCreateTrigger: TMenuItem;
    miNCreateUser: TMenuItem;
    miNCreateView: TMenuItem;
    miNDelete: TMenuItem;
    miNEmpty: TMenuItem;
    miNExpand: TMenuItem;
    miNExport: TMenuItem;
    miNExportAccess: TMenuItem;
    miNExportExcel: TMenuItem;
    miNExportHTML: TMenuItem;
    miNExportODBC: TMenuItem;
    miNExportPDF: TMenuItem;
    miNExportSQL: TMenuItem;
    miNExportText: TMenuItem;
    miNExportXML: TMenuItem;
    miNImport: TMenuItem;
    miNImportAccess: TMenuItem;
    miNImportExcel: TMenuItem;
    miNImportODBC: TMenuItem;
    miNImportSQL: TMenuItem;
    miNImportText: TMenuItem;
    miNPaste: TMenuItem;
    miNProperties: TMenuItem;
    miNRename: TMenuItem;
    miSNavigator: TMenuItem;
    miSSQLHistory: TMenuItem;
    mlDCreate: TMenuItem;
    mlDCreateDatabase: TMenuItem;
    mlDCreateEvent: TMenuItem;
    mlDCreateField: TMenuItem;
    mlDCreateForeignKey: TMenuItem;
    mlDCreateFunction: TMenuItem;
    mlDCreateIndex: TMenuItem;
    mlDCreateProcedure: TMenuItem;
    mlDCreateTable: TMenuItem;
    mlDCreateTrigger: TMenuItem;
    mlDCreateUser: TMenuItem;
    mlDCreateView: TMenuItem;
    mlDDelete: TMenuItem;
    mlDEmpty: TMenuItem;
    mlECopy: TMenuItem;
    mlEPaste: TMenuItem;
    mlEProperties: TMenuItem;
    mlERename: TMenuItem;
    mlFExport: TMenuItem;
    mlFExportAccess: TMenuItem;
    mlFExportExcel: TMenuItem;
    mlFExportHTML: TMenuItem;
    mlFExportODBC: TMenuItem;
    mlFExportPDF: TMenuItem;
    mlFExportSQL: TMenuItem;
    mlFExportText: TMenuItem;
    mlFExportXML: TMenuItem;
    mlFImport: TMenuItem;
    mlFImportAccess: TMenuItem;
    mlFImportExcel: TMenuItem;
    mlFImportODBC: TMenuItem;
    mlFImportSQL: TMenuItem;
    mlFImportText: TMenuItem;
    MList: TPopupMenu;
    MLog: TPopupMenu;
    mlOpen: TMenuItem;
    MNavigator: TPopupMenu;
    mpDRun: TMenuItem;
    mpDRunSelection: TMenuItem;
    mpECopy: TMenuItem;
    mpECopyToFile: TMenuItem;
    mpECut: TMenuItem;
    mpEDelete: TMenuItem;
    mpEPaste: TMenuItem;
    mpEPasteFromFile: TMenuItem;
    mpESelectAll: TMenuItem;
    MSideBar: TPopupMenu;
    MSQLEditor: TPopupMenu;
    MSQLHistory: TPopupMenu;
    mtBrowser: TMenuItem;
    mtBuilder: TMenuItem;
    mtDiagram: TMenuItem;
    mtEditor: TMenuItem;
    mtEditor2: TMenuItem;
    mtEditor3: TMenuItem;
    MText: TPopupMenu;
    mtIDE: TMenuItem;
    mtObjects: TMenuItem;
    MToolBar: TPopupMenu;
    mwAddTable: TMenuItem;
    mwCreateLink: TMenuItem;
    mwCreateSection: TMenuItem;
    mwDCreate: TMenuItem;
    mwDCreateField: TMenuItem;
    mwDCreateForeignKey: TMenuItem;
    mwDCreateTable: TMenuItem;
    mwDDelete: TMenuItem;
    mwDEmpty: TMenuItem;
    mwDProperties: TMenuItem;
    mwECopy: TMenuItem;
    mwEDelete: TMenuItem;
    mwEPaste: TMenuItem;
    mwFExport: TMenuItem;
    mwFExportAccess: TMenuItem;
    mwFExportBitmap: TMenuItem;
    mwFExportExcel: TMenuItem;
    mwFExportHTML: TMenuItem;
    mwFExportODBC: TMenuItem;
    mwFExportPDF: TMenuItem;
    mwFExportSQL: TMenuItem;
    mwFExportText: TMenuItem;
    mwFExportXML: TMenuItem;
    mwFImport: TMenuItem;
    mwFImportAccess: TMenuItem;
    mwFImportExcel: TMenuItem;
    mwFImportODBC: TMenuItem;
    mwFImportSQL: TMenuItem;
    mwFImportText: TMenuItem;
    MWorkbench: TPopupMenu;
    N1: TMenuItem;
    N02: TMenuItem;
    N03: TMenuItem;
    N04: TMenuItem;
    N05: TMenuItem;
    N07: TMenuItem;
    N08: TMenuItem;
    N09: TMenuItem;
    N10: TMenuItem;
    N12: TMenuItem;
    N13: TMenuItem;
    N14: TMenuItem;
    N15: TMenuItem;
    N16: TMenuItem;
    N17: TMenuItem;
    N18: TMenuItem;
    N19: TMenuItem;
    N2: TMenuItem;
    N21: TMenuItem;
    N22: TMenuItem;
    N23: TMenuItem;
    N24: TMenuItem;
    N25: TMenuItem;
    N27: TMenuItem;
    N28: TMenuItem;
    N29: TMenuItem;
    N30: TMenuItem;
    N31: TMenuItem;
    N32: TMenuItem;
    N33: TMenuItem;
    N34: TMenuItem;
    OpenDialog: TOpenDialog_Ext;
    PBlob: TPanel_Ext;
    PQueryBuilder: TPanel_Ext;
    PQueryBuilderSynMemo: TPanel_Ext;
    PContent: TPanel_Ext;
    PDataBrowser: TPanel_Ext;
    PDataBrowserSpacer: TPanel_Ext;
    PSQLEditorDBGrid: TPanel_Ext;
    PExplorer: TPanel_Ext;
    PFiles: TPanel_Ext;
    PFolders: TPanel_Ext;
    PListView: TPanel_Ext;
    PLog: TPanel_Ext;
    PLogHeader: TPanel_Ext;
    PNavigator: TPanel_Ext;
    PObjectIDE: TPanel_Ext;
    PObjectIDEGridDataSource: TDataSource;
    PObjectIDESpacer: TPanel_Ext;
    PObjectIDETrigger: TPanel_Ext;
    PResult: TPanel_Ext;
    PResultHeader: TPanel_Ext;
    PSideBar: TPanel_Ext;
    PSQLHistory: TPanel_Ext;
    PSynMemo: TPanel_Ext;
    PToolBarBlob: TPanel_Ext;
    PHeader: TPanel_Ext;
    PWorkbench: TPanel_Ext;
    SaveDialog: TSaveDialog_Ext;
    SQueryBuilderSynMemo: TSplitter_Ext;
    SExplorer: TSplitter_Ext;
    SLog: TSplitter_Ext;
    smECopy: TMenuItem;
    smEEmpty: TMenuItem;
    smESelectAll: TMenuItem;
    SQLBuilder: TacSQLBuilderPlainText;
    SResult: TSplitter_Ext;
    SSideBar: TSplitter_Ext;
    SynCompletion: TSynCompletionProposal;
    TBBlob: TToolBar;
    tbBlobHexEditor: TToolButton;
    tbBlobHTML: TToolButton;
    tbBlobImage: TToolButton;
    tbBlobRTF: TToolButton;
    tbBlobText: TToolButton;
    tbBrowser: TToolButton;
    tbBuilder: TToolButton;
    tbDiagram: TToolButton;
    tbEditor: TToolButton;
    tbEditor2: TToolButton;
    tbEditor3: TToolButton;
    tbExplorer: TToolButton;
    TBFilterEnabled: TToolBar;
    tbIDE: TToolButton;
    TBLimitEnabled: TToolBar;
    tbNavigator: TToolButton;
    tbObjects: TToolButton;
    TBQuickSearchEnabled: TToolBar;
    TBObjectSearch: TToolBar;
    TBSideBar: TToolBar;
    tbSQLHistory: TToolButton;
    tmECopy: TMenuItem;
    tmECut: TMenuItem;
    tmEDelete: TMenuItem;
    tmEPaste: TMenuItem;
    tmESelectAll: TMenuItem;
    ToolBar: TToolBar;
    SBlob: TSplitter_Ext; // 2017-01-07 For debugging moved out of alphabetical order
    procedure aDCreateDatabaseExecute(Sender: TObject);
    procedure aDCreateEventExecute(Sender: TObject);
    procedure aDCreateExecute(Sender: TObject);
    procedure aDCreateFieldExecute(Sender: TObject);
    procedure aDCreateForeignKeyExecute(Sender: TObject);
    procedure aDCreateKeyExecute(Sender: TObject);
    procedure aDCreateRoutineExecute(Sender: TObject);
    procedure aDCreateTableExecute(Sender: TObject);
    procedure aDCreateTriggerExecute(Sender: TObject);
    procedure aDCreateUserExecute(Sender: TObject);
    procedure aDCreateViewExecute(Sender: TObject);
    procedure aDDeleteExecute(Sender: TObject);
    procedure aDDeleteRecordExecute(Sender: TObject);
    procedure aDInsertRecordExecute(Sender: TObject);
    procedure aDNextExecute(Sender: TObject);
    procedure aDPrevExecute(Sender: TObject);
    procedure aDPropertiesExecute(Sender: TObject);
    procedure aEClearAllExecute(Sender: TObject);
    procedure aEFormatSQLExecute(Sender: TObject);
    procedure aEPasteFromFileExecute(Sender: TObject);
    procedure aFExportAccessExecute(Sender: TObject);
    procedure aFExportBitmapExecute(Sender: TObject);
    procedure aFExportExcelExecute(Sender: TObject);
    procedure aFExportHTMLExecute(Sender: TObject);
    procedure aFExportODBCExecute(Sender: TObject);
    procedure aFExportPDFExecute(Sender: TObject);
    procedure aFExportSQLExecute(Sender: TObject);
    procedure aFExportTextExecute(Sender: TObject);
    procedure aFExportXMLExecute(Sender: TObject);
    procedure aFImportAccessExecute(Sender: TObject);
    procedure aFImportExcelExecute(Sender: TObject);
    procedure aFImportODBCExecute(Sender: TObject);
    procedure aFImportSQLExecute(Sender: TObject);
    procedure aFImportTextExecute(Sender: TObject);
    procedure aHRunClick(Sender: TObject);
    procedure aHRunExecute(Sender: TObject);
    procedure aPCollapseExecute(Sender: TObject);
    procedure aPExpandExecute(Sender: TObject);
    procedure aPObjectBrowserTableExecute(Sender: TObject);
    procedure aPResultExecute(Sender: TObject);
    procedure aSSearchFindNotFound(Sender: TObject);
    procedure aSynCompletionExecuteExecute(Sender: TObject);
    procedure aTBFilterExecute(Sender: TObject);
    procedure aTBLimitExecute(Sender: TObject);
    procedure aTBOffsetExecute(Sender: TObject);
    procedure aTBQuickSearchExecute(Sender: TObject);
    procedure aVBlobExecute(Sender: TObject);
    procedure aVSortAscExecute(Sender: TObject);
    procedure aVSortDescExecute(Sender: TObject);
    procedure BObjectIDEClick(Sender: TObject);
    procedure DataSetAfterCancel(DataSet: TDataSet);
    procedure DataSetAfterClose(DataSet: TDataSet);
    procedure DataSetAfterOpen(const DBGrid: TMySQLDBGrid; const DataSet: TDataSet);
    procedure DataSetAfterPost(DataSet: TDataSet);
    procedure DataSetAfterScroll(DataSet: TDataSet);
    procedure DataSetBeforeCancel(DataSet: TDataSet);
    procedure DataSetBeforePost(DataSet: TDataSet);
    procedure DBGridCellEnter(Column: TColumn);
    procedure DBGridColEnter(Sender: TObject);
    procedure DBGridColExit(Sender: TObject);
    procedure DBGridCopyToExecute(Sender: TObject);
    procedure DBGridDataSourceDataChange(Sender: TObject; Field: TField);
    procedure DBGridDblClick(Sender: TObject);
    procedure DBGridDrawColumnCell(Sender: TObject; const Rect: TRect;
      DataCol: Integer; Column: TColumn; State: TGridDrawState);
    procedure DBGridEditExecute(Sender: TObject);
    procedure DBGridEmptyExecute(Sender: TObject);
    procedure DBGridEnter(Sender: TObject);
    procedure DBGridExit(Sender: TObject);
    procedure DBGridKeyDown(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure DBGridKeyUp(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure DBGridMouseMove(Sender: TObject; Shift: TShiftState; X,
      Y: Integer);
    procedure DBGridTitleClick(Column: TColumn);
    procedure FBlobResize(Sender: TObject);
    procedure PDataBrowserResize(Sender: TObject);
    procedure FFilesEnter(Sender: TObject);
    procedure FFilterChange(Sender: TObject);
    procedure FFilterDropDown(Sender: TObject);
    procedure FFilterEnabledClick(Sender: TObject);
    procedure FFilterEnter(Sender: TObject);
    procedure FFilterKeyPress(Sender: TObject; var Key: Char);
    procedure FFoldersChange(Sender: TObject; Node: TTreeNode);
    procedure FHexEditorChange(Sender: TObject);
    procedure FHexEditorEnter(Sender: TObject);
    procedure FHexEditorKeyPress(Sender: TObject; var Key: Char);
    procedure FLimitChange(Sender: TObject);
    procedure FLimitEnabledClick(Sender: TObject);
    procedure FLogEnter(Sender: TObject);
    procedure FLogExit(Sender: TObject);
    procedure FNavigatorChange(Sender: TObject; Node: TTreeNode);
    procedure FNavigatorChange2(Sender: TObject; Node: TTreeNode);
    procedure FNavigatorChanging(Sender: TObject; Node: TTreeNode;
      var AllowChange: Boolean);
    procedure FNavigatorDragDrop(Sender, Source: TObject; X,
      Y: Integer);
    procedure FNavigatorDragOver(Sender, Source: TObject; X,
      Y: Integer; State: TDragState; var Accept: Boolean);
    procedure FNavigatorEdited(Sender: TObject; Node: TTreeNode; var S: string);
    procedure FNavigatorEditing(Sender: TObject; Node: TTreeNode;
      var AllowEdit: Boolean);
    procedure FNavigatorEnter(Sender: TObject);
    procedure FNavigatorExit(Sender: TObject);
    procedure FNavigatorExpanding(Sender: TObject; Node: TTreeNode;
      var AllowExpansion: Boolean);
    procedure FNavigatorKeyDown(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure FNavigatorKeyPress(Sender: TObject; var Key: Char);
    procedure FObjectSearchChange(Sender: TObject);
    procedure FObjectSearchEnter(Sender: TObject);
    procedure FObjectSearchKeyPress(Sender: TObject; var Key: Char);
    procedure FObjectSearchStartClick(Sender: TObject);
    procedure FOffsetChange(Sender: TObject);
    procedure FOffsetKeyPress(Sender: TObject; var Key: Char);
    procedure FormResize(Sender: TObject);
    procedure FQueryBuilderDragDrop(Sender, Source: TObject; X,
      Y: Integer);
    procedure FQueryBuilderDragOver(Sender, Source: TObject; X, Y: Integer;
      State: TDragState; var Accept: Boolean);
    procedure FQueryBuilderEnter(Sender: TObject);
    procedure FQueryBuilderExit(Sender: TObject);
    procedure FQueryBuilderResize(Sender: TObject);
    procedure FQueryBuilderSQLUpdated(Sender: TObject);
    procedure FQueryBuilderSynMemoChange(Sender: TObject);
    procedure FQueryBuilderSynMemoEnter(Sender: TObject);
    procedure FQueryBuilderSynMemoExit(Sender: TObject);
    procedure FQueryBuilderValidatePopupMenu(Sender: TacQueryBuilder;
      AControlOwner: TacQueryBuilderControlOwner; AForControl: TControl;
      APopupMenu: TPopupMenu);
    procedure FQuickSearchChange(Sender: TObject);
    procedure FQuickSearchEnabledClick(Sender: TObject);
    procedure FQuickSearchKeyPress(Sender: TObject; var Key: Char);
    procedure FRTFChange(Sender: TObject);
    procedure FRTFEnter(Sender: TObject);
    procedure FRTFExit(Sender: TObject);
    procedure FSQLEditorSynMemoKeyPress(Sender: TObject; var Key: Char);
    procedure FSQLHistoryChange(Sender: TObject; Node: TTreeNode);
    procedure FSQLHistoryChanging(Sender: TObject; Node: TTreeNode;
      var AllowChange: Boolean);
    procedure FSQLHistoryDblClick(Sender: TObject);
    procedure FSQLHistoryEnter(Sender: TObject);
    procedure FSQLHistoryExit(Sender: TObject);
    procedure FSQLHistoryHint(Sender: TObject; const Node: TTreeNode;
      var Hint: string);
    procedure FSQLHistoryKeyDown(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure FSQLHistoryKeyPress(Sender: TObject; var Key: Char);
    procedure FSQLHistoryMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure FTextChange(Sender: TObject);
    procedure FTextEnter(Sender: TObject);
    procedure FTextExit(Sender: TObject);
    procedure FTextKeyPress(Sender: TObject; var Key: Char);
    procedure FTextKeyUp(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure FTextMouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure ghmCopyClick(Sender: TObject);
    procedure ghmGotoClick(Sender: TObject);
    procedure ListViewAdvancedCustomDrawItem(Sender: TCustomListView;
      Item: TListItem; State: TCustomDrawState; Stage: TCustomDrawStage;
      var DefaultDraw: Boolean);
    procedure ListViewAdvancedCustomDrawSubItem(Sender: TCustomListView;
      Item: TListItem; SubItem: Integer; State: TCustomDrawState;
      Stage: TCustomDrawStage; var DefaultDraw: Boolean);
    procedure ListViewColumnClick(Sender: TObject; Column: TListColumn);
    procedure ListViewCompare(Sender: TObject; Item1: TListItem;
      Item2: TListItem; Data: Integer; var Compare: Integer);
    procedure ListViewDblClick(Sender: TObject);
    procedure ListViewDragOver(Sender, Source: TObject; X, Y: Integer;
      State: TDragState; var Accept: Boolean);
    procedure ListViewEdited(Sender: TObject; Item: TListItem;
      var S: string);
    procedure ListViewEditing(Sender: TObject; Item: TListItem;
      var AllowEdit: Boolean);
    procedure ListViewEnter(Sender: TObject);
    procedure ListViewExit(Sender: TObject);
    procedure ListViewKeyDown(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure ListViewSelectItem(Sender: TObject; Item: TListItem;
      Selected: Boolean);
    procedure MetadataProviderGetSQLFieldNames(Sender: TacBaseMetadataProvider;
      const SQL: WideString; Fields: TacFieldsList);
    procedure mfOpenClick(Sender: TObject);
    procedure mfFilterClearClick(Sender: TObject);
    procedure mfFilterSQLClick(Sender: TObject);
    procedure mfFilterTextClick(Sender: TObject);
    procedure mfFilterHTMLClick(Sender: TObject);
    procedure mfFilterXMLClick(Sender: TObject);
    procedure mfFilterAccessClick(Sender: TObject);
    procedure mfFilterExcelClick(Sender: TObject);
    procedure mfDeleteClick(Sender: TObject);
    procedure mfRenameClick(Sender: TObject);
    procedure mfPropertiesClick(Sender: TObject);
    procedure MFilesPopup(Sender: TObject);
    procedure MGridHeaderPopup(Sender: TObject);
    procedure MGridPopup(Sender: TObject);
    procedure miHOpenClick(Sender: TObject);
    procedure miHPropertiesClick(Sender: TObject);
    procedure miHSaveAsClick(Sender: TObject);
    procedure miHStatementIntoSQLEditorClick(Sender: TObject);
    procedure MListPopup(Sender: TObject);
    procedure mlOpenClick(Sender: TObject);
    procedure MNavigatorPopup(Sender: TObject);
    procedure MSQLEditorPopup(Sender: TObject);
    procedure MSQLHistoryPopup(Sender: TObject);
    procedure MTextPopup(Sender: TObject);
    procedure MToolBarPopup(Sender: TObject);
    procedure mwCreateLinkExecute(Sender: TObject);
    procedure mwCreateSectionClick(Sender: TObject);
    procedure mwDCreateForeignKeyClick(Sender: TObject);
    procedure mwDCreateTableClick(Sender: TObject);
    procedure mwEPasteClick(Sender: TObject);
    procedure mwEDeleteClick(Sender: TObject);
    procedure MWorkbenchPopup(Sender: TObject);
    procedure PanelMouseDown(Sender: TObject;
      Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure PanelMouseMove(Sender: TObject; Shift: TShiftState; X,
      Y: Integer);
    procedure PanelMouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure PanelPaint(Sender: TObject);
    procedure PanelResize(Sender: TObject);
    procedure PContentResize(Sender: TObject);
    procedure PGridResize(Sender: TObject);
    procedure PHeaderCheckElements(Sender: TObject);
    procedure PHeaderMouseMove(Sender: TObject; Shift: TShiftState; X,
      Y: Integer);
    procedure PHeaderPaint(Sender: TObject);
    procedure PHeaderResize(Sender: TObject);
    procedure PLogResize(Sender: TObject);
    procedure PObjectIDEResize(Sender: TObject);
    procedure PToolBarBlobResize(Sender: TObject);
    procedure SearchNotFound(Sender: TObject; FindText: string);
    procedure SLogCanResize(Sender: TObject; var NewSize: Integer;
      var Accept: Boolean);
    procedure SLogMoved(Sender: TObject);
    procedure smEEmptyClick(Sender: TObject);
    procedure SplitterCanResize(Sender: TObject; var NewSize: Integer;
      var Accept: Boolean);
    procedure SSideBarCanResize(Sender: TObject; var NewSize: Integer;
      var Accept: Boolean);
    procedure SSideBarMoved(Sender: TObject);
    procedure SynCompletionChange(Sender: TObject; AIndex: Integer);
    procedure SynCompletionClose(Sender: TObject);
    procedure SynCompletionExecute(Kind: SynCompletionType; Sender: TObject;
      var CurrentInput: string; var x, y: Integer; var CanExecute: Boolean);
    procedure SynCompletionCancelled(Sender: TObject);
    procedure SynCompletionAfterCodeCompletion(Sender: TObject;
      const Value: string; Shift: TShiftState; Index: Integer; EndToken: Char);
    procedure SynMemoDragDrop(Sender, Source: TObject; X, Y: Integer);
    procedure SynMemoDragOver(Sender, Source: TObject; X, Y: Integer;
      State: TDragState; var Accept: Boolean);
    procedure SynMemoEnter(Sender: TObject);
    procedure SynMemoExit(Sender: TObject);
    procedure SynMemoStatusChange(Sender: TObject; Changes: TSynStatusChanges);
    procedure ToolBarResize(Sender: TObject);
    procedure ToolBarTabsClick(Sender: TObject);
    procedure ToolButtonStyleClick(Sender: TObject);
    procedure TreeViewCollapsed(Sender: TObject; Node: TTreeNode);
    procedure TreeViewCollapsing(Sender: TObject;
      Node: TTreeNode; var AllowCollapse: Boolean);
    procedure TreeViewEndDrag(Sender, Target: TObject; X, Y: Integer);
    procedure TreeViewExpanded(Sender: TObject; Node: TTreeNode);
    procedure TreeViewGetSelectedIndex(Sender: TObject; Node: TTreeNode);
    procedure TreeViewMouseDown(Sender: TObject;
      Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure TreeViewMouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure FObjectSearchExit(Sender: TObject);
  type
    TNewLineFormat = (nlWindows, nlUnix, nlMacintosh);
    TTabState = set of (tsLoading, tsActive);
    TView = (vObjects, vBrowser, vIDE, vBuilder, vDiagram, vEditor, vEditor2, vEditor3, vObjectSearch);
    TToolBarData = record
      Caption: string;
      tbPropertiesAction: TBasicAction;
      View: TView;
    end;

  type
    TSQLEditor = class(TObject)
    type
      TResult = record
        DataSet: TMySQLDataSet;
        DataSource: TDataSource;
        DBGrid: TMySQLDBGrid;
      end;
    private
      FSession: TFSession;
      FSynMemo: TSynMemo;
      PDBGrid: TPanel_Ext;
      Results: TList;
      TCResult: TTabControl;
      function GetActiveDBGrid(): TMySQLDBGrid;
      procedure TCResultChange(Sender: TObject);
    protected
      procedure DataSetAfterOpen(DataSet: TDataSet);
    public
      Filename: TFileName;
      FileCodePage: Cardinal;
      procedure CloseResult();
      constructor Create(const AFSession: TFSession; const ASynMemo: TSynMemo; const APDBGrid: TPanel_Ext);
      destructor Destroy(); override;
      function ResultEvent(const ErrorCode: Integer; const ErrorMessage: string; const WarningCount: Integer;
        const CommandText: string; const DataHandle: TMySQLConnection.TDataHandle; const Data: Boolean): Boolean;
      property ActiveDBGrid: TMySQLDBGrid read GetActiveDBGrid;
      property SynMemo: TSynMemo read FSynMemo;
    end;

    TSObjectDesktop = class(TSObject.TDesktop)
    private
      FFSession: TFSession;
    protected
      property FSession: TFSession read FFSession;
    public
      constructor Create(const AFSession: TFSession; const ASObject: TSObject);
    end;

    TDatabaseDesktop = class(TSObjectDesktop)
    private
      DataSet: TMySQLDataSet;
      DataSource: TDataSource;
      FBuilderDBGrid: TMySQLDBGrid;
      FListView: TListView;
      PDBGrid: TPanel_Ext;
      FXML: IXMLNode;
      function GetDatabase(): TSDatabase; inline;
      function GetXML(): IXMLNode;
    protected
      FWorkbench: TWWorkbench;
      procedure BuilderDataSetAfterOpen(DataSet: TDataSet);
    public
      function BuilderResultEvent(const ErrorCode: Integer; const ErrorMessage: string; const WarningCount: Integer;
        const CommandText: string; const DataHandle: TMySQLConnection.TDataHandle; const Data: Boolean): Boolean;
      procedure CloseBuilderResult();
      procedure CloseQuery(Sender: TObject; var CanClose: Boolean);
      constructor Create(const AFSession: TFSession; const ADatabase: TSDatabase);
      function CreateListView(): TListView; virtual;
      function CreateWorkbench(): TWWorkbench; virtual;
      destructor Destroy(); override;
      property BuilderDBGrid: TMySQLDBGrid read FBuilderDBGrid;
      property Database: TSDatabase read GetDatabase;
      property ListView: TListView read FListView;
      property Workbench: TWWorkbench read FWorkbench;
      property XML: IXMLNode read GetXML;
    end;

    TTableDesktop = class(TSObjectDesktop)
    private
      DataSource: TDataSource;
      FListView: TListView;
      FDBGrid: TMySQLDBGrid;
      PDBGrid: TPanel_Ext;
      FXML: IXMLNode;
      function GetFilter(Index: Integer): string;
      function GetFilterCount(): Integer;
      function GetLimit(): Integer;
      function GetLimited(): Boolean;
      function GetTable(): TSTable; inline;
      function GetXML(): IXMLNode;
      procedure SetLimit(const Limit: Integer);
      procedure SetLimited(const ALimited: Boolean);
    public
      procedure AddFilter(const AFilter: string);
      constructor Create(const AFSession: TFSession; const ATable: TSTable);
      function CreateDBGrid(): TMySQLDBGrid;
      function CreateListView(): TListView;
      procedure DataSetAfterOpen(DataSet: TDataSet);
      procedure DataSetAfterRefresh(DataSet: TDataSet);
      destructor Destroy(); override;
      property DBGrid: TMySQLDBGrid read FDBGrid;
      property Filters[Index: Integer]: string read GetFilter;
      property FilterCount: Integer read GetFilterCount;
      property Limit: Integer read GetLimit write SetLimit;
      property Limited: Boolean read GetLimited write SetLimited;
      property ListView: TListView read FListView;
      property Table: TSTable read GetTable;
      property XML: IXMLNode read GetXML;
    end;

    TViewDesktop = class(TTableDesktop)
    private
      FSynMemo: TSynMemo;
    public
      constructor Create(const AFSession: TFSession; const AView: TSView);
      function CreateSynMemo(): TSynMemo;
      destructor Destroy(); override;
      procedure DataSetBeforeOpen(DataSet: TDataSet);
      property SynMemo: TSynMemo read FSynMemo;
    end;

    TRoutineDesktop = class(TSObjectDesktop)
    type
      TResult = record
        DataSet: TMySQLDataSet;
        DataSource: TDataSource;
        DBGrid: TMySQLDBGrid;
      end;
    private
      FSynMemo: TSynMemo;
      PDBGrid: TPanel_Ext;
      Results: TList;
      TCResult: TTabControl;
      function GetActiveDBGrid(): TMySQLDBGrid;
      procedure TCResultChange(Sender: TObject);
    protected
      procedure DataSetAfterOpen(DataSet: TDataSet);
    public
      procedure CloseIDEResult();
      constructor Create(const AFSession: TFSession; const ARoutine: TSRoutine);
      function CreateSynMemo(): TSynMemo; virtual;
      destructor Destroy(); override;
      function IDEResultEvent(const ErrorCode: Integer; const ErrorMessage: string; const WarningCount: Integer;
        const CommandText: string; const DataHandle: TMySQLConnection.TDataHandle; const Data: Boolean): Boolean;
      property ActiveDBGrid: TMySQLDBGrid read GetActiveDBGrid;
      property SynMemo: TSynMemo read FSynMemo;
    end;

    TTriggerDesktop = class(TSObjectDesktop)
    private
      FSynMemo: TSynMemo;
    public
      constructor Create(const AFSession: TFSession; const ATrigger: TSTrigger);
      function CreateSynMemo(): TSynMemo;
      destructor Destroy(); override;
      property SynMemo: TSynMemo read FSynMemo;
    end;

    TEventDesktop = class(TSObjectDesktop)
    private
      FSynMemo: TSynMemo;
    public
      constructor Create(const AFSession: TFSession; const AEvent: TSEvent);
      function CreateSynMemo(): TSynMemo;
      destructor Destroy(); override;
      property SynMemo: TSynMemo read FSynMemo;
    end;

    TWanted = class
    private
      FAction: TAction;
      FAddress: string;
      FSession: TFSession;
      FUpdate: TSSession.TUpdate;
      procedure Clear();
      function GetNothing(): Boolean;
      procedure SetAction(const AAction: TAction);
      procedure SetAddress(const AAddress: string);
      procedure SetUpdate(const AUpdate: TSSession.TUpdate);
    protected
      procedure Synchronize();
    public
      constructor Create(const AFSession: TFSession);
      procedure Execute();
      property Action: TAction read FAction write SetAction;
      property Address: string read FAddress write SetAddress;
      property Update: TSSession.TUpdate read FUpdate write SetUpdate;
      property Nothing: Boolean read GetNothing;
    end;

  private
    ActiveControlOnDeactivate: TWinControl;
    ActiveDBGrid: TMySQLDBGrid;
    ActiveIDEInputDataSet: TDataSet;
    ActiveListView: TListView;
    ActiveSynMemo: TSynMemo;
    ActiveWorkbench: TWWorkbench;
    aDRunExecuteSelStart: Integer;
    BMPImage: TBitmap;
    CloseButtonHot: TPicture;
    CloseButtonNormal: TPicture;
    CloseButtonPushed: TPicture;
    FAddress: string;
    FFiles: TJamShellList;
    FFolders: TJamShellTree;
    FHTML: TWebBrowser;
    FilterMRU: TPPreferences.TMRUList;
    FNavigatorMenuNode: TTreeNode;
    FNavigatorNodeAfterActivate: TTreeNode;
    FNavigatorNodeToExpand: TTreeNode;
    FrameState: TTabState;
    FSQLEditorSynMemo2: TSynMemo;
    FSQLEditorSynMemo3: TSynMemo;
    FSQLHistoryMenuNode: TTreeNode;
    FView: TView;
    GIFImage: TGIFImage;
    JPEGImage: TJPEGImage;
    LastFNavigatorSelected: TTreeNode;
    LastObjectIDEAddress: string;
    LastSelectedDatabase: string;
    LastSelectedTable: string;
    LastTableView: TView;
    LeftMousePressed: Boolean;
    ListViewSortData: TListViewSortData;
    MGridHeaderColumn: TColumn;
    MouseDownNode: TTreeNode;
    MovingToAddress: Boolean;
    NewLineFormat: TNewLineFormat;
    NMListView: PNMListView;
    ObjectSearch: TSObjectSearch;
    ObjectSearchListView: TListView;
    OldAddress: string;
    OldFListOrderIndex: Integer;
    PanelMouseDownPoint: TPoint;
    Param: string;
    PasteMode: Boolean;
    PNGImage: TPNGImage;
    PObjectSearch: TPObjectSearch;
    PResultHeight: Integer;
    ProcessesListView: TListView;
    ShellLink: TJamShellLink;
    SplitColor: TColor;
    SQLEditor: TSQLEditor;
    SQLEditor2: TSQLEditor;
    SQLEditor3: TSQLEditor;
    SQueryBuilderSynMemoMoved: Boolean;
    SynMemoBeforeDrag: TSynMemoBeforeDrag;
    SynCompletionPending: record
      Active: Boolean;
      CurrentInput: string;
      X: Integer;
      Y: Integer;
    end;
    NavigatorElapse: Integer;
    UsersListView: TListView;
    VariablesListView: TListView;
    Wanted: TWanted;
    procedure aDCancelExecute(Sender: TObject);
    procedure AddressChanged(Sender: TObject);
    procedure AddressChanging(const Sender: TObject; const NewAddress: String; var AllowChange: Boolean);
    procedure aDPostObjectExecute(Sender: TObject);
    procedure aDRunExecute(Sender: TObject);
    procedure aDRunSelectionExecute(Sender: TObject);
    procedure aECopyExecute(Sender: TObject);
    procedure aEPasteExecute(Sender: TObject);
    procedure aEPasteFromExecute(Sender: TObject);
    procedure aERedoExecute(Sender: TObject);
    procedure aERenameExecute(Sender: TObject);
    procedure aESelectAllExecute(Sender: TObject);
    procedure aFExportExecute(const Sender: TObject; const ExportType: TExportType);
    procedure aFImportExecute(const Sender: TObject; const ImportType: TImportType);
    procedure aFOpenExecute(Sender: TObject);
    procedure aFSaveAsExecute(Sender: TObject);
    procedure aFSaveExecute(Sender: TObject);
    procedure AfterExecuteSQL(Sender: TObject);
    procedure aHManualExecute(Sender: TObject);
    procedure aHSQLExecute(Sender: TObject);
    procedure aViewExecute(Sender: TObject);
    procedure aVRefreshAllExecute(Sender: TObject);
    procedure aVRefreshExecute(Sender: TObject);
    procedure aVSideBarExecute(Sender: TObject);
    procedure aVSQLLogExecute(Sender: TObject);
    procedure BeforeExecuteSQL(Sender: TObject);
    procedure BeginEditLabel(Sender: TObject);
    procedure SessionUpdate(const Event: TSSession.TEvent);
    function ColumnWidthKindByListView(const ListView: TListView): TPAccount.TDesktop.TListViewKind;
    procedure CMSysFontChanged(var Message: TMessage); message CM_SYSFONTCHANGED;
    function CreateDesktop(const CObject: TSObject): TSObject.TDesktop;
    procedure CreateExplorer();
    function CreateDBGrid(const PDBGrid: TPanel_Ext; const DataSource: TDataSource): TMySQLDBGrid;
    function CreateListView(const Data: TCustomData): TListView;
    function CreatePDBGrid(): TPanel_Ext;
    function CreateSynMemo(SObject: TSObject): TSynMemo;
    function CreateTCResult(const PDBGrid: TPanel_Ext): TTabControl;
    function CreateWorkbench(const ADatabase: TSDatabase): TWWorkbench;
    procedure DBGridInitialize(const DBGrid: TMySQLDBGrid);
    function Desktop(const Database: TSDatabase): TDatabaseDesktop; overload; inline;
    function Desktop(const Event: TSEvent): TEventDesktop; overload; inline;
    function Desktop(const Routine: TSRoutine): TRoutineDesktop; overload; inline;
    function Desktop(const Table: TSTable): TTableDesktop; overload; inline;
    function Desktop(const Trigger: TSTrigger): TTriggerDesktop; overload; inline;
    function Desktop(const View: TSView): TViewDesktop; overload; inline;
    function Dragging(const Sender: TObject): Boolean;
    procedure EndEditLabel(Sender: TObject);
    procedure FHexEditorShow(Sender: TObject);
    procedure FHTMLHide(Sender: TObject);
    procedure FHTMLShow(Sender: TObject);
    procedure FieldSetText(Sender: TField; const Text: string);
    procedure FImageShow(Sender: TObject);
    procedure FLogUpdate();
    procedure FNavigatorEmptyExecute(Sender: TObject);
    procedure FNavigatorInitialize(Sender: TObject);
    function FNavigatorNodeByAddress(const Address: string): TTreeNode;
    procedure FNavigatorChanged(Sender: TObject; const Node: TTreeNode);
    procedure FNavigatorUpdate(const Event: TSSession.TEvent);
    procedure FormSessionEvent(const Event: TSSession.TEvent);
    function FQueryBuilderActiveSelectList(): TacQueryBuilderSelectListControl;
    function FQueryBuilderActiveWorkArea(): TacQueryBuilderWorkArea;
    procedure FQueryBuilderAddTable(Sender: TObject);
    function FQueryBuilderEditorPageControl(): TacPageControl;
    procedure FQueryBuilderEditorPageControlChange(Sender: TObject);
    procedure FQueryBuilderEditorPageControlCheckStyle();
    procedure FQueryBuilderEditorTabSheetEnter(Sender: TObject);
    procedure FreeDBGrid(const DBGrid: TMySQLDBGrid);
    procedure FreeListView(const ListView: TListView);
    procedure FRTFShow(Sender: TObject);
    procedure FSQLHistoryRefresh(Sender: TObject);
    procedure FTextShow(Sender: TObject);
    function GetActiveDBGrid(): TMySQLDBGrid;
    function GetActiveIDEInputDataSet(): TDataSet;
    function GetActiveListView(): TListView;
    function GetActiveSynMemo(): TSynMemo;
    function GetActiveWorkbench(): TWWorkbench;
    function GetEditorField(): TField;
    function GetFocusedSItem(): TSItem;
    function GetPath(): TFileName; inline;
    function GetMenuDatabase(): TSDatabase;
    function GetSelectedDatabase(): string;
    function GetSelectedImageIndex(): Integer;
    function GetSQLEditors(View: TView): TSQLEditor;
    function GetWindow(): TForm_Ext;
    procedure gmFilterClearClick(Sender: TObject);
    procedure gmFilterIntoFilterClick(Sender: TObject);
    function ImageIndexByData(const Data: TObject): Integer;
    function ImportError(const Details: TTool.TErrorDetails): TDataAction;
    procedure ListViewEmpty(Sender: TObject);
    procedure ListViewInitialize(const ListView: TListView);
    procedure ListViewUpdate(const Event: TSSession.TEvent; const ListView: TListView; const Data: TCustomData = nil);
    function NavigatorNodeToAddress(const Node: TTreeNode): string;
    function ObjectSearchStep2(): Boolean;
    procedure OnConvertError(Sender: TObject; Text: string);
    function ParamToView(const AParam: Variant): TView;
    procedure PasteExecute(const Node: TTreeNode; const Objects: string);
    procedure PContentChange(Sender: TObject);
    function PostObject(Sender: TObject): Boolean;
    procedure PropertiesServerExecute(Sender: TObject);
    function RenameSItem(const SItem: TSItem; const NewName: string): Boolean;
    procedure SaveDiagram(Sender: TObject);
    procedure SaveSQLFile(Sender: TObject);
    procedure SendQuery(Sender: TObject; const SQL: string);
    procedure SetView(const AView: TView);
    procedure SetAddress(const AAddress: string);
    procedure SetListViewGroupHeader(const ListView: TListView; const GroupID: Integer; const NewHeader: string);
    procedure SetPath(const APath: TFileName);
    procedure SQLError(DataSet: TDataSet; E: EDatabaseError; var Action: TDataAction);
    procedure SynMemoApplyPreferences(const SynMemo: TSynMemo);
    procedure TableOpen(Sender: TObject);
    procedure TCResultMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
    procedure UMActivateDBGrid(var Message: TMessage); message UM_ACTIVATE_DBGRID;
    procedure UMActivateFText(var Message: TMessage); message UM_ACTIVATEFTEXT;
    procedure UMChangePreferences(var Message: TMessage); message UM_CHANGEPREFERENCES;
    procedure UMCloseFrame(var Message: TMessage); message UM_CLOSE_FRAME;
    procedure UMCloseTabQuery(var Message: TMessage); message UM_CLOSE_TAB_QUERY;
    procedure UMFrameActivate(var Message: TMessage); message UM_ACTIVATEFRAME;
    procedure UMFrameDeactivate(var Message: TMessage); message UM_DEACTIVATEFRAME;
    procedure UMPostBuilderQueryChange(var Message: TMessage); message UM_POST_BUILDER_QUERY_CHANGE;
    procedure UMPostShow(var Message: TMessage); message UM_POST_SHOW;
    procedure UMStausBarRefresh(var Message: TMessage); message UM_STATUS_BAR_REFRESH;
    procedure UMSynCompletionTime(var Message: TMessage); message UM_SYNCOMPLETION_TIMER;
    procedure UMWantedSynchronize(var Message: TMessage); message UM_WANTED_SYNCHRONIZE;
    function UpdateAfterAddressChanged(): Boolean;
    procedure ViewChanged(Sender: TObject);
    function ViewToParam(const AView: TView): Variant;
    procedure WorkbenchAddTable(Sender: TObject);
    procedure WorkbenchChange(Sender: TObject; Control: TWControl);
    procedure WorkbenchCursorMove(Sender: TObject; X, Y: Integer);
    procedure WorkbenchDragDrop(Sender, Source: TObject; X, Y: Integer);
    procedure WorkbenchDragOver(Sender, Source: TObject; X, Y: Integer;
      State: TDragState; var Accept: Boolean);
    procedure WorkbenchEmptyExecute(Sender: TObject);
    procedure WorkbenchEnter(Sender: TObject);
    procedure WorkbenchExit(Sender: TObject);
    procedure WorkbenchPasteExecute(Sender: TObject);
    function WorkbenchValidateControl(Sender: TObject; Control: TWControl): Boolean;
    procedure WMActivate(var Message: TWMActivate); message WM_ACTIVATE;
    procedure WMNotify(var Message: TWMNotify); message WM_NOTIFY;
    procedure WMParentNotify(var Message: TWMParentNotify); message WM_PARENTNOTIFY;
    procedure WMTimer(var Message: TWMTimer); message WM_TIMER;
    property EditorField: TField read GetEditorField;
    property FocusedSItem: TSItem read GetFocusedSItem;
    property MenuDatabase: TSDatabase read GetMenuDatabase;
    property SelectedImageIndex: Integer read GetSelectedImageIndex;
    property SQLEditors[View: TView]: TSQLEditor read GetSQLEditors;
  protected
    procedure CreateParams(var Params: TCreateParams); override;
  public
    Session: TSSession;
    StatusBar: TStatusBar;
    ToolBarData: TToolBarData;
    constructor Create(const AOwner: TComponent; const AParent: TWinControl; const ASession: TSSession; const AParam: string); reintroduce;
    destructor Destroy(); override;
    function AddressToCaption(const AAddress: string): string;
    procedure aEFindExecute(Sender: TObject);
    procedure aEReplaceExecute(Sender: TObject);
    procedure aETransferExecute(Sender: TObject);
    procedure CrashRescue();
    procedure OpenDiagram();
    procedure OpenSQLFile(const AFilename: TFileName; const CodePage: Cardinal = 0; const Insert: Boolean = False);
    procedure StatusBarRefresh(const Immediately: Boolean = False);
    property Address: string read FAddress write SetAddress;
    property Path: TFileName read GetPath write SetPath;
    property SelectedDatabase: string read GetSelectedDatabase;
    property View: TView read FView write SetView;
    property Window: TForm_Ext read GetWindow;
  end;

implementation {***************************************************************}

{$R *.dfm}

uses
  MMSystem, Math, DBConsts, Clipbrd, DBCommon, ShellAPI, Variants, Types,
  XMLDoc, Themes, StrUtils, UxTheme, FileCtrl, SysConst, RichEdit, UITypes,
  ShLwApi, Consts, ComObj,
  acQBLocalizer, acQBStrings,
  CommCtrl_Ext, StdActns_Ext,
  MySQLConsts, SQLUtils,
  uDeveloper,
  uDField, uDKey, uDTable, uDTables, uDVariable, uDDatabase, uDForeignKey,
  uDUser, uDQuickFilter, uDSQLHelp, uDTransfer, uDSearch, uDServer,
  uURI, uDView, uDRoutine, uDTrigger, uDStatement, uDEvent, uDPaste, uDSegment,
  uDConnecting, uPDataBrowserDummy, uDExecutingSQL;

const
  nlHost = 0;
  nlDatabase = 1;
  nlTable = 2;

const
  tiNavigator = 1;
  tiStatusBar = 2;
  tiShowSynCompletion = 3;
  tiHideSynCompletion = 4;

const
  giDatabases = 1;
  giSystemTools = 2;
  giTables = 3;
  giRoutines = 4;
  giEvents = 5;
  giKeys = 6;
  giFields = 7;
  giForeignKeys = 8;
  giTriggers = 9;
  giProcesses = 10;
  giUsers = 12;
  giVariables = 13;

const
  Filters: array[0 .. 12 - 1] of
    record Text: PChar; ValueType: Integer end = (
      (Text: '%s IS NULL'; ValueType: 0),
      (Text: '%s IS NOT NULL'; ValueType: 0),
      (Text: '%s = %s'; ValueType: 1),
      (Text: '%s <> %s'; ValueType: 1),
      (Text: '%s < %s'; ValueType: 1),
      (Text: '%s > %s'; ValueType: 1),
      (Text: '%s LIKE %s'; ValueType: 2),
      (Text: '%s = %s'; ValueType: 3),
      (Text: '%s <> %s'; ValueType: 3),
      (Text: '%s < %s'; ValueType: 3),
      (Text: '%s > %s'; ValueType: 3),
      (Text: '%s LIKE %s'; ValueType: 4)
    );

  ToolbarTabByView: array[vObjects .. vEditor3] of TPPreferences.TToolbarTab =
    (ttObjects, ttBrowser, ttIDE, ttBuilder, ttDiagram, ttEditor, ttEditor2, ttEditor3);

var
  SBlobDebug: Pointer; // Debug 2016-12-28

// Debug 2016-12-07
function GetControlByHandle(const Control: TWinControl; const Wnd: HWND): TWinControl;
var
  I: Integer;
begin
  Result := nil;

  for I := 0 to Control.ControlCount - 1 do
    if (Control.Controls[I] is TWinControl) then
    begin
      Result := GetControlByHandle(TWinControl(Control.Controls[I]), Wnd);
      if (Assigned(Result)) then
        break;
    end;

  if (Control.Handle = Wnd) then
    Result := Control
  else if (Assigned(Result)) then
    raise ERangeError.Create(SRangeError);
end;

function IsRTF(const Value: string): Boolean;
var
  S: string;
begin
  S := Value;
  while (Copy(S, 2, 1) = '{') do Delete(S, 2, 1);
  Result := Copy(S, 1, 5) = '{\rtf';
end;

function IsHTML(const Value: string): Boolean;
begin
  Result := Copy(Trim(Value), 1, 2) = '<!';
end;

function IsPDF(const Value: string): Boolean;
begin
  Result := Copy(Trim(Value), 1, 4) = '%PDF';
end;

function FindChildByClassType(const Control: TWinControl; ClassType: TClass): TWinControl;
var
  I: Integer;
begin
  Result := nil;

  if (Assigned(Control)) then
    for I := 0 to Control.ControlCount - 1 do
      if (not Assigned(Result) and (Control.Controls[I] is TWinControl)) then
        if (Control.Controls[I].ClassType = ClassType) then
          Result := TWinControl(Control.Controls[I])
        else
          Result := FindChildByClassType(TWinControl(Control.Controls[I]), ClassType);
end;

function CopyName(const Name: string; const Items: TSItems): string;
var
  I: Integer;
begin
  Result := Name;
  I := 1;
  while (Items.IndexByName(Result) >= 0) do
  begin
    if (I = 1) then
      Result := Preferences.LoadStr(680, Name)
    else
      Result := Preferences.LoadStr(681, Name, IntToStr(I));
    Result := ReplaceStr(Result, ' ', '_');
    Inc(I);
  end;
end;

{ TFSession.TSQLEditorDesktop *************************************************}

procedure TFSession.TSQLEditor.CloseResult();
var
  I: Integer;
begin
  if (Assigned(Results)) then
  begin
    if (Assigned(TCResult)) then
      FreeAndNil(TCResult);

    for I := 0 to Results.Count - 1 do
    begin
      FSession.FreeDBGrid(TResult(Results[I]^).DBGrid);
      TResult(Results[I]^).DataSource.Free();
      TResult(Results[I]^).DataSet.Free();
      FreeMem(Results[I]);
    end;
    Results.Clear();
  end;
end;

constructor TFSession.TSQLEditor.Create(const AFSession: TFSession; const ASynMemo: TSynMemo; const APDBGrid: TPanel_Ext);
begin
  inherited Create();

  FSession := AFSession;

  Filename := '';
  FileCodePage := GetACP();
  FSynMemo := ASynMemo;
  PDBGrid := APDBGrid;
  Results := nil;
  TCResult := nil;
end;

procedure TFSession.TSQLEditor.DataSetAfterOpen(DataSet: TDataSet);
begin
  FSession.DataSetAfterOpen(ActiveDBGrid, DataSet);
end;

destructor TFSession.TSQLEditor.Destroy();
begin
  CloseResult();
  if (Assigned(Results)) then
    Results.Free();
  if (PDBGrid <> FSession.PSQLEditorDBGrid) then
    PDBGrid.Free();

  inherited;
end;

function TFSession.TSQLEditor.GetActiveDBGrid(): TMySQLDBGrid;
begin
  if (not Assigned(Results) or (Results.Count = 0)) then
    Result := nil
  else if (not Assigned(TCResult)) then
    Result := TResult(Results[0]^).DBGrid
  else
    Result := TResult(Results[TCResult.TabIndex]^).DBGrid;
end;

function TFSession.TSQLEditor.ResultEvent(const ErrorCode: Integer; const ErrorMessage: string; const WarningCount: Integer;
  const CommandText: string; const DataHandle: TMySQLConnection.TDataHandle; const Data: Boolean): Boolean;
var
  EndingCommentLength: Integer;
  Item: ^TResult;
  Len: Integer;
  Msg: string;
  Parse: TSQLParse;
  SQL: string;
  StartingCommentLength: Integer;
  URI: TUURI;
  XML: IXMLNode;
begin
  if (not Assigned(Results)) then
    Results := TList.Create();

  if ((ErrorCode = 0)
    and (Results.Count < 5)
    and Assigned(FSession.Session.Account.HistoryXML)
    and ValidXMLText(CommandText)
    and SQLCreateParse(Parse, PChar(CommandText), Length(CommandText), FSession.Session.Connection.MySQLVersion)
    and not SQLParseKeyword(Parse, 'USE')) then
  begin
    while (FSession.Session.Account.HistoryXML.ChildNodes.Count > 100) do
      FSession.Session.Account.HistoryXML.ChildNodes.Delete(0);

    XML := FSession.Session.Account.HistoryXML.AddChild('sql');
    if (not Data) then
      XML.Attributes['type'] := 'statement'
    else
      XML.Attributes['type'] := 'query';
    XML.AddChild('database').Text := FSession.Session.Connection.DatabaseName;
    XML.AddChild('datetime').Text := FloatToStr(FSession.Session.Connection.ServerDateTime, FileFormatSettings);
    if (not Data and (FSession.Session.Connection.RowsAffected >= 0)) then
      XML.AddChild('rows_affected').Text := IntToStr(FSession.Session.Connection.RowsAffected);

    XML.AddChild('sql').Text := CommandText;
    if (FSession.Session.Connection.Info <> '') then
      XML.AddChild('info').Text := FSession.Session.Connection.Info;
    XML.AddChild('execution_time').Text := FloatToStr(FSession.Session.Connection.ExecutionTime, FileFormatSettings);
    if (FSession.Session.Connection.Connected and (FSession.Session.Connection.InsertId > 0)) then
      XML.AddChild('insert_id').Text := IntToStr(FSession.Session.Connection.InsertId);

    FSession.FSQLHistoryRefresh(nil);
  end;

  if (ErrorCode > 0) then
  begin
    if ((CommandText <> '') and (Length(FSynMemo.Text) > Length(CommandText) + 5)) then
    begin
      SQL := CommandText;
      Len := SQLStmtLength(PChar(SQL), Length(SQL));
      SQLTrimStmt(SQL, 1, Len, StartingCommentLength, EndingCommentLength);
      FSynMemo.SelStart := FSession.aDRunExecuteSelStart + FSession.Session.Connection.SuccessfullExecutedSQLLength + StartingCommentLength;
      FSynMemo.SelLength := Len - StartingCommentLength - EndingCommentLength;
    end
  end
  else
  begin
    if (WarningCount > 0) then
    begin
      Msg := Preferences.LoadStr(922, IntToStr(WarningCount) + ' Warning(s)');
      Msg := Msg
        + #10#10
        + 'Statement:' + #10
        + CommandText;

      MsgBoxCheck(Msg, Preferences.LoadStr(47), MB_OK + MB_ICONWARNING,
        ID_OK, '{46aa8b98-74ae-4c10-9b64-ceded860b3d4}');
    end;

    if (not Data) then
    begin
      if (FSession.Session.Databases.NameCmp(FSession.Session.Connection.DatabaseName, FSession.SelectedDatabase) <> 0) then
      begin
        URI := TUURI.Create(FSession.Address);
        URI.Database := FSession.Session.Connection.DatabaseName;
        FSession.Wanted.Address := URI.Address;
        URI.Free();
      end;
    end
    else
    begin
      if (Results.Count = 1) then
      begin
        TCResult := FSession.CreateTCResult(PDBGrid);
        TCResult.Tabs.Add(Preferences.LoadStr(861, IntToStr(Results.Count)));
        TCResult.OnChange := TCResultChange;
        TCResult.OnMouseMove := FSession.TCResultMouseMove;
        TCResult.Tag := NativeInt(Self);
      end;

      GetMem(Item, SizeOf(TResult));
      TResult(Item^).DataSet := TMySQLDataSet.Create(FSession.Owner);
      TResult(Item^).DataSet.AfterOpen := DataSetAfterOpen;
      TResult(Item^).DataSource := TDataSource.Create(FSession.Owner);
      TResult(Item^).DataSource.Enabled := False;
      TResult(Item^).DBGrid := FSession.CreateDBGrid(PDBGrid, TResult(Item^).DataSource);
      TResult(Item^).DBGrid.Tag := Results.Count;
      TResult(Item^).DataSource.DataSet := TResult(Item^).DataSet;
      Results.Add(Item);

      if (Results.Count > 1) then
      begin
        TCResult.Tabs.Add(Preferences.LoadStr(861, IntToStr(Results.Count)));
        TCResult.TabIndex := Results.Count - 1;
        TCResultChange(nil);
      end;

      FSession.ActiveDBGrid := TResult(Item^).DBGrid;
      TResult(Item^).DataSet.Open(DataHandle);
    end;
  end;

  Result := False;
end;

procedure TFSession.TSQLEditor.TCResultChange(Sender: TObject);
var
  I: Integer;
begin
  for I := 0 to PDBGrid.ControlCount - 1 do
    if (PDBGrid.Controls[I] is TMySQLDBGrid) then
    begin
      PDBGrid.Controls[I].Visible := TMySQLDBGrid(PDBGrid.Controls[I]).Tag = TCResult.TabIndex;
      if (PDBGrid.Controls[I].Visible) then
        FSession.ActiveDBGrid := GetActiveDBGrid();
    end;
end;

{ TFSession.TCObjectDesktop ***************************************************}

constructor TFSession.TSObjectDesktop.Create(const AFSession: TFSession; const ASObject: TSObject);
begin
  FFSession := AFSession;

  inherited Create(ASObject);
end;

{ TFSession.TDatabaseDesktop **************************************************}

procedure TFSession.TDatabaseDesktop.BuilderDataSetAfterOpen(DataSet: TDataSet);
begin
  FSession.DataSetAfterOpen(BuilderDBGrid, DataSet);
end;

function TFSession.TDatabaseDesktop.BuilderResultEvent(const ErrorCode: Integer; const ErrorMessage: string; const WarningCount: Integer;
  const CommandText: string; const DataHandle: TMySQLConnection.TDataHandle; const Data: Boolean): Boolean;
begin
  if (Data) then
  begin
    DataSet := TMySQLDataSet.Create(FSession.Owner);
    DataSet.AfterOpen := BuilderDataSetAfterOpen;

    if (not Assigned(PDBGrid)) then
      PDBGrid := FSession.CreatePDBGrid();
    if (not Assigned(DataSource)) then
    begin
      DataSource := TDataSource.Create(FSession.Owner);
      DataSource.Enabled := False;
    end;
    if (not Assigned(FBuilderDBGrid)) then
      FBuilderDBGrid := FSession.CreateDBGrid(PDBGrid, DataSource);
    DataSource.DataSet := DataSet;

    FSession.ActiveDBGrid := FBuilderDBGrid;
    DataSet.Open(DataHandle);
  end;

  Result := False;
end;

procedure TFSession.TDatabaseDesktop.CloseBuilderResult();
begin
  if (Assigned(FBuilderDBGrid)) then
    FreeAndNil(FBuilderDBGrid);
  if (Assigned(DataSet)) then
    FreeAndNil(DataSet);
end;

procedure TFSession.TDatabaseDesktop.CloseQuery(Sender: TObject; var CanClose: Boolean);
begin
  if (Assigned(Workbench) and Workbench.Modified) then
    if (Workbench.ObjectCount > 0) then
    begin
      if (not SysUtils.ForceDirectories(ExtractFilePath(FSession.Session.Account.DataPath + Database.Name + PathDelim))) then
        RaiseLastOSError();
      FWorkbench.SaveToFile(FSession.Session.Account.DataPath + Database.Name + PathDelim + 'Diagram.xml');
    end
    else if (FileExists(FSession.Session.Account.DataPath + Database.Name + PathDelim + 'Diagram.xml')) then
      DeleteFile(FSession.Session.Account.DataPath + Database.Name + PathDelim + 'Diagram.xml');
end;

constructor TFSession.TDatabaseDesktop.Create(const AFSession: TFSession; const ADatabase: TSDatabase);
begin
  inherited Create(AFSession, ADatabase);

  DataSet := nil;
  DataSource := nil;
  FBuilderDBGrid := nil;
  PDBGrid := nil;
  FWorkbench := nil;
  FXML := nil;
end;

function TFSession.TDatabaseDesktop.CreateListView(): TListView;
begin
  if (not Assigned(FListView)) then
  begin
    FListView := FSession.CreateListView(Database);
    Database.PushBuildEvents();
  end;

  Result := FListView;
end;

function TFSession.TDatabaseDesktop.CreateWorkbench(): TWWorkbench;
begin
  if (not Assigned(FWorkbench)) then
    FWorkbench := FSession.CreateWorkbench(Database);

  Result := FWorkbench;
end;

destructor TFSession.TDatabaseDesktop.Destroy();
var
  I: Integer;
  TablesXML: IXMLNode;
begin
  TablesXML := XMLNode(XML, 'tables');
  if (Assigned(TablesXML)) then
    for I := TablesXML.ChildNodes.Count - 1 downto 0 do
      if ((TablesXML.ChildNodes[I].NodeName = 'table') and not Assigned(Database.TableByName(TablesXML.ChildNodes[I].Attributes['name']))) then
        TablesXML.ChildNodes.Delete(I);

  CloseBuilderResult();
  if (Assigned(PDBGrid)) then
    PDBGrid.Free();
  if (Assigned(DataSource)) then
    DataSource.Free();
  if (Assigned(ListView)) then
    FSession.FreeListView(ListView);
  if (Assigned(FWorkbench)) then
    FWorkbench.Free();

  inherited;
end;

function TFSession.TDatabaseDesktop.GetDatabase(): TSDatabase;
begin
  Result := TSDatabase(SObject);
end;

function TFSession.TDatabaseDesktop.GetXML(): IXMLNode;
var
  I: Integer;
  Node: IXMLNode;
begin
  if (not Assigned(FXML) and Assigned(FSession.Session.Account.DesktopXML)) then
  begin
    Node := XMLNode(FSession.Session.Account.DesktopXML, 'browser/databases', True);

    FXML := nil;
    for I := 0 to Node.ChildNodes.Count - 1 do
      if ((Node.ChildNodes[I].NodeName = 'database') and (FSession.Session.Databases.NameCmp(Node.ChildNodes[I].Attributes['name'], Database.Name) = 0)) then
        FXML := Node.ChildNodes[I];

    if (not Assigned(FXML)) then
    begin
      FXML := Node.AddChild('database');
      FXML.Attributes['name'] := Database.Name;
    end;
  end;

  Result := FXML;
end;

{ TFSession.TTableDesktop *****************************************************}

procedure TFSession.TTableDesktop.AddFilter(const AFilter: string);
var
  FiltersXML: IXMLNode;
  I: Integer;
begin
  if (AFilter <> '') then
  begin
    FiltersXML := XMLNode(XML, 'filters', True);
    for I := FiltersXML.ChildNodes.Count - 1 downto 0 do
      if (FiltersXML.ChildNodes[I].Text = AFilter) then
        FiltersXML.ChildNodes.Delete(I);
    try
      FiltersXML.AddChild('filter').NodeValue := AFilter;
    except
      // Some characters are not storable inside XML - so just ignore them
    end;
  end;

  while (FiltersXML.ChildNodes.Count > 10) do
    FiltersXML.ChildNodes.Delete(0);
end;

constructor TFSession.TTableDesktop.Create(const AFSession: TFSession; const ATable: TSTable);
begin
  inherited Create(AFSession, ATable);

  PDBGrid := nil;
  FXML := nil;
end;

function TFSession.TTableDesktop.CreateDBGrid(): TMySQLDBGrid;
begin
  if (not Assigned(FDBGrid)) then
  begin
    if (not Assigned(PDBGrid)) then
      PDBGrid := FSession.CreatePDBGrid();
    if (not Assigned(DataSource)) then
    begin
      DataSource := TDataSource.Create(FSession.Owner);
      DataSource.Enabled := False;
    end;
    FDBGrid := FSession.CreateDBGrid(PDBGrid, DataSource);
    DataSource.DataSet := Table.DataSet;
  end;

  Result := FDBGrid;
end;

function TFSession.TTableDesktop.CreateListView(): TListView;
begin
  if (not Assigned(FListView)) then
  begin
    FListView := FSession.CreateListView(Table);
    Table.PushBuildEvent(False);
  end;

  Result := FListView;
end;

procedure TFSession.TTableDesktop.DataSetAfterOpen(DataSet: TDataSet);
begin
  if (not Assigned(DBGrid)) then
    CreateDBGrid();

  FSession.DataSetAfterOpen(DBGrid, DataSet);

  DBGrid.ReadOnly := Table is TSSystemView;
end;

procedure TFSession.TTableDesktop.DataSetAfterRefresh(DataSet: TDataSet);
var
  I: Integer;
begin
  if (Table.DataSet.FilterSQL <> '') then
    AddFilter(Table.DataSet.FilterSQL);
  if (((Table.DataSet.Limit > 0) <> Limited) or (Limit <> Table.DataSet.Limit)) then
  begin
    Limited := Table.DataSet.Limit > 0;
    Limit := Table.DataSet.Limit;
  end;

  FSession.FUDOffset.Position := Table.DataSet.Offset;
  FSession.FLimitEnabled.Down := Table.DataSet.Limit > 0;
  if (FSession.FLimitEnabled.Down) then
    FSession.FUDLimit.Position := Table.DataSet.Limit;

  FSession.FFilterEnabled.Down := Table.DataSet.FilterSQL <> '';
  FSession.FFilterEnabled.Enabled := FSession.FFilter.Text <> '';
  FSession.FilterMRU.Clear();
  if (FSession.FFilterEnabled.Down) then
    FSession.FFilter.Text := Table.DataSet.FilterSQL;
  for I := 0 to FilterCount - 1 do
    FSession.FilterMRU.Add(Filters[I]);
  FSession.gmFilter.Clear();

  FSession.FQuickSearchEnabled.Down := Table.DataSet.QuickSearch <> '';
end;

destructor TFSession.TTableDesktop.Destroy();
begin
  // The import runs in a separated thread. Because if this, a DROP inside
  // the .sql import executes this call from a different thread - that
  // forces a Access Denied (5).
  // This work-a-round hides error notification
  if (GetCurrentThreadId() = MainThreadID) then
  begin
    if (Assigned(ListView)) then
      FSession.FreeListView(ListView);
    if (Assigned(DBGrid)) then
      FSession.FreeDBGrid(DBGrid);
    if (Assigned(DataSource)) then
      DataSource.Free();
    if (Assigned(PDBGrid)) then
      PDBGrid.Free();

    inherited;
  end;
end;

function TFSession.TTableDesktop.GetFilter(Index: Integer): string;
var
  FiltersXML: IXMLNode;
begin
  Result := '';

  if (Assigned(XML)) then
  begin
    FiltersXML := XMLNode(XML, 'filters', True);
    if (Assigned(FiltersXML) and (Index < FiltersXML.ChildNodes.Count)) then
      Result := XMLNode(XML, 'filters').ChildNodes[FiltersXML.ChildNodes.Count - Index - 1].Text;
  end;
end;

function TFSession.TTableDesktop.GetFilterCount(): Integer;
begin
  Result := 0;

  if (Assigned(XML)) then
    if (Assigned(XMLNode(XML, 'filters'))) then
      Result := XMLNode(XML, 'filters').ChildNodes.Count;
end;

function TFSession.TTableDesktop.GetLimit(): Integer;
begin
  if ((Table is TSBaseTable) and TSBaseTable(Table).ValidStatus and (TSBaseTable(Table).AvgRowLength > 0)) then
  begin
    Result := DefaultLimitSize div TSBaseTable(Table).AvgRowLength;
    if (Result < DefaultLimit) then
      Result := DefaultLimit
    else
      Result := Result div DefaultLimit * DefaultLimit;
  end
  else if (Assigned(XML) and Assigned(XMLNode(XML, 'limit')) and
    TryStrToInt(XMLNode(XML, 'limit').Text, Result)) then
  else
    Result := DefaultLimit;

  if (Result < 1) then
    Result := DefaultLimit;
end;

function TFSession.TTableDesktop.GetLimited(): Boolean;
begin
  Result := True;
  if ((Table is TSBaseTable) and TSBaseTable(Table).ValidStatus) then
    Result := TSBaseTable(Table).RecordCount >= Limit
  else if (Assigned(XML) and Assigned(XMLNode(XML, 'limit'))) then
    TryStrToBool(XMLNode(XML, 'limit').Attributes['used'], Result);
end;

function TFSession.TTableDesktop.GetXML(): IXMLNode;
var
  I: Integer;
  Node: IXMLNode;
begin
  if (not Assigned(FXML) and Assigned(FSession.Desktop(Table.Database).XML)) then
  begin
    Node := XMLNode(FSession.Desktop(Table.Database).XML, 'tables', True);

    for I := 0 to Node.ChildNodes.Count - 1 do
      if ((Node.ChildNodes[I].NodeName = 'table') and (lstrcmpi(PChar(string(Node.ChildNodes[I].Attributes['name'])), PChar(Table.Name)) = 0)) then
        FXML := Node.ChildNodes[I];

    if (not Assigned(FXML)) then
    begin
      FXML := Node.AddChild('table');
      FXML.Attributes['name'] := Table.Name;
    end;
  end;

  Result := FXML;
end;

function TFSession.TTableDesktop.GetTable(): TSTable;
begin
  Result := TSTable(SObject);
end;

procedure TFSession.TTableDesktop.SetLimit(const Limit: Integer);
begin
  if (Limit > 0) then
    XMLNode(XML, 'limit', True).Text := IntToStr(Limit);
end;

procedure TFSession.TTableDesktop.SetLimited(const ALimited: Boolean);
begin
  XMLNode(XML, 'limit', True).Attributes['used'] := BoolToStr(ALimited, True);
end;

{ TFSession.TViewDesktop ******************************************************}

constructor TFSession.TViewDesktop.Create(const AFSession: TFSession; const AView: TSView);
begin
  inherited Create(AFSession, AView);

  FSynMemo := nil;
end;

function TFSession.TViewDesktop.CreateSynMemo(): TSynMemo;
begin
  if (not Assigned(FSynMemo) and TSView(SObject).Valid) then
  begin
    FSynMemo := FSession.CreateSynMemo(SObject);
    FSynMemo.Text := TSView(SObject).Stmt + #13#10;
  end;

  Result := FSynMemo;
end;

procedure TFSession.TViewDesktop.DataSetBeforeOpen(DataSet: TDataSet);
var
  DescFieldNames: string;
  FieldNames: string;
  NewSortDef: TIndexDef;
begin
  if ((DataSet is TMySQLDataSet)
    and Table.Session.SQLParser.ParseSQL(TSView(Table).Stmt)
    and GetOrderFromSelectStmt(Table.Session.SQLParser.FirstStmt, FieldNames, DescFieldNames)) then
  begin
    NewSortDef := TIndexDef.Create(nil, '', FieldNames, []);
    NewSortDef.DescFields := DescFieldNames;
    TMySQLDataSet(DataSet).SortDef.Assign(NewSortDef);
    NewSortDef.Free();
  end;
  Table.Session.SQLParser.Clear();
end;

destructor TFSession.TViewDesktop.Destroy();
begin
  if (Assigned(SynMemo)) then
    SynMemo.Free;

  inherited;
end;

{ TFSession.TRoutineDesktop ***************************************************}

procedure TFSession.TRoutineDesktop.CloseIDEResult();
var
  I: Integer;
begin
  if (Assigned(Results)) then
  begin
    if (Assigned(TCResult)) then
      FreeAndNil(TCResult);

    for I := 0 to Results.Count - 1 do
    begin
      FSession.FreeDBGrid(TResult(Results[I]^).DBGrid);
      TResult(Results[I]^).DataSource.Free();
      TResult(Results[I]^).DataSet.Free();
      FreeMem(Results[I]);
    end;
    Results.Clear();
  end;
end;

constructor TFSession.TRoutineDesktop.Create(const AFSession: TFSession; const ARoutine: TSRoutine);
begin
  inherited Create(AFSession, ARoutine);

  FSynMemo := nil;
  Results := nil;
end;

function TFSession.TRoutineDesktop.CreateSynMemo(): TSynMemo;
begin
  if (not Assigned(FSynMemo) and TSRoutine(SObject).Valid) then
  begin
    FSynMemo := FSession.CreateSynMemo(SObject);
    FSynMemo.Text := TSRoutine(SObject).Source;
  end;

  Result := FSynMemo;
end;

procedure TFSession.TRoutineDesktop.DataSetAfterOpen(DataSet: TDataSet);
begin
  FSession.DataSetAfterOpen(ActiveDBGrid, DataSet);
end;

destructor TFSession.TRoutineDesktop.Destroy();
begin
  CloseIDEResult();
  if (Assigned(PDBGrid)) then
    PDBGrid.Free();
  if (Assigned(SynMemo)) then
    SynMemo.Free();
  if (Assigned(Results)) then
    Results.Free();

  inherited;
end;

function TFSession.TRoutineDesktop.GetActiveDBGrid(): TMySQLDBGrid;
begin
  if (not Assigned(Results) or (Results.Count = 0)) then
    Result := nil
  else if (not Assigned(TCResult)) then
    Result := TResult(Results[0]^).DBGrid
  else if (not Assigned(Results[TCResult.TabIndex])) then
    // Debug 2016-11-23
    raise ERangeError.Create(SRangeError)
  else
    Result := TResult(Results[TCResult.TabIndex]^).DBGrid;
end;

function TFSession.TRoutineDesktop.IDEResultEvent(const ErrorCode: Integer; const ErrorMessage: string; const WarningCount: Integer;
  const CommandText: string; const DataHandle: TMySQLConnection.TDataHandle; const Data: Boolean): Boolean;
var
  Item: ^TResult;
begin
  if (Data) then
  begin
    if (not Assigned(PDBGrid)) then
      PDBGrid := FSession.CreatePDBGrid();
    if (not Assigned(Results)) then
      Results := TList.Create();

    if (Results.Count = 1) then
    begin
      TCResult := FSession.CreateTCResult(PDBGrid);
      TCResult.Tabs.Add(Preferences.LoadStr(861, IntToStr(Results.Count)));
      TCResult.OnChange := TCResultChange;
    end;

    GetMem(Item, SizeOf(TResult));
    TResult(Item^).DataSet := TMySQLDataSet.Create(FSession.Owner);
    TResult(Item^).DataSet.AfterOpen := DataSetAfterOpen;
    TResult(Item^).DataSource := TDataSource.Create(FSession.Owner);
    TResult(Item^).DataSource.Enabled := False;
    TResult(Item^).DBGrid := FSession.CreateDBGrid(PDBGrid, TResult(Item^).DataSource);
    TResult(Item^).DBGrid.Tag := Results.Count;
    TResult(Item^).DataSource.DataSet := TResult(Item^).DataSet;
    Results.Add(Item);

    if (Results.Count > 1) then
    begin
      TCResult.Tabs.Add(Preferences.LoadStr(861, IntToStr(Results.Count)));
      TCResult.TabIndex := Results.Count - 1;
      TCResultChange(nil);
    end;

    FSession.ActiveDBGrid := TResult(Item^).DBGrid;
    TResult(Item^).DataSet.Open(DataHandle);
  end;

  Result := False;
end;

procedure TFSession.TRoutineDesktop.TCResultChange(Sender: TObject);
var
  I: Integer;
begin
  for I := 0 to PDBGrid.ControlCount - 1 do
    if (PDBGrid.Controls[I] is TMySQLDBGrid) then
      PDBGrid.Controls[I].Visible := TMySQLDBGrid(PDBGrid.Controls[I]).Tag = TCResult.TabIndex;
end;

{ TFSession.TTriggerDesktop ***************************************************}

constructor TFSession.TTriggerDesktop.Create(const AFSession: TFSession; const ATrigger: TSTrigger);
begin
  inherited Create(AFSession, ATrigger);

  FSynMemo := nil;
end;

function TFSession.TTriggerDesktop.CreateSynMemo(): TSynMemo;
begin
  if (not Assigned(FSynMemo) and TSTrigger(SObject).Valid) then
  begin
    FSynMemo := FSession.CreateSynMemo(SObject);
    FSynMemo.Text := TSTrigger(SObject).Stmt;
  end;

  Result := FSynMemo;
end;

destructor TFSession.TTriggerDesktop.Destroy();
begin
  if (Assigned(SynMemo)) then
    SynMemo.Free();

  inherited;
end;

{ TFSession.TEventDesktop *****************************************************}

constructor TFSession.TEventDesktop.Create(const AFSession: TFSession; const AEvent: TSEvent);
begin
  inherited Create(AFSession, AEvent);

  FSynMemo := nil;
end;

function TFSession.TEventDesktop.CreateSynMemo(): TSynMemo;
begin
  if (not Assigned(FSynMemo) and TSEvent(SObject).Valid) then
  begin
    FSynMemo := FSession.CreateSynMemo(SObject);
    FSynMemo.Text := TSEvent(SObject).Stmt;
  end;

  Result := FSynMemo;
end;

destructor TFSession.TEventDesktop.Destroy();
begin
  if (Assigned(SynMemo)) then
    SynMemo.Free();

  inherited;
end;

{ TFSession.TWanted ***********************************************************}

procedure TFSession.TWanted.Clear();
begin
  FAction := nil;
  FAddress := '';
  FUpdate := nil;
end;

constructor TFSession.TWanted.Create(const AFSession: TFSession);
begin
  FSession := AFSession;

  Clear();
end;

procedure TFSession.TWanted.Execute();
begin
  PostMessage(FSession.Handle, UM_WANTED_SYNCHRONIZE, 0, 0);
end;

function TFSession.TWanted.GetNothing(): Boolean;
begin
  Result := not Assigned(Action) and (Address = '') and not Assigned(Update);
end;

procedure TFSession.TWanted.SetAction(const AAction: TAction);
begin
  if (AAction <> FAction) then
  begin
    Clear();
    FAction := AAction;
  end;
end;

procedure TFSession.TWanted.SetAddress(const AAddress: string);
var
  URI: TUURI;
begin
  if (AAddress <> FAddress) then
  begin
    // Debug 2016-12-16
    URI := TUURI.Create(Address);
    if ((URI.Param['view'] = 'browser') and (URI.Table = '')) then
      raise ERangeError.Create('Address: ' + Address);
    if ((URI.Param['view'] = 'ide') and (URI.Database = '')) then
      raise ERangeError.Create('Address: ' + Address);
    if ((URI.Param['view'] = 'ide') and (URI.Param['object'] = Null)) then
      raise ERangeError.Create('Address: ' + Address);
    URI.Free();

    Clear();
    FAddress := AAddress;
  end;
end;

procedure TFSession.TWanted.SetUpdate(const AUpdate: TSSession.TUpdate);
begin
  Clear();
  if (not FSession.Session.Connection.InUse()) then
    AUpdate()
  else
    FUpdate := AUpdate;
end;

procedure TFSession.TWanted.Synchronize();
var
  TempAction: TAction;
  TempAddress: string;
  TempUpdate: TSSession.TUpdate;
  URI: TUURI;
begin
  if (Assigned(Action)) then
  begin
    TempAction := Action;
    Clear();
    TempAction.Execute();
  end
  else if (Address <> '') then
  begin
    TempAddress := Address;
    Clear();

    // Debug 2016-12-16
    URI := TUURI.Create(TempAddress);
    if ((URI.Param['view'] = 'browser') and (URI.Table = '')) then
      raise ERangeError.Create('Address: ' + TempAddress);
    if ((URI.Param['view'] = 'ide') and (URI.Database = '')) then
      raise ERangeError.Create('Address: ' + TempAddress);
    URI.Free();

    FSession.Address := TempAddress;
    if (Nothing and (FSession.Address <> TempAddress)) then
    begin
      URI := TUURI.Create();
      URI.Address := TempAddress;
      URI.Database := '';
      URI.Table := '';
      URI.Param['view'] := Null;
      URI.Param['objecttype'] := Null;
      URI.Param['object'] := Null;
      FSession.Address := URI.Address;
      URI.Free();
    end;
  end
  else if (Assigned(Update)) then
  begin
    TempUpdate := Update;
    Clear();
    TempUpdate();
  end;
end;

{ TFSession *******************************************************************}

procedure TFSession.aDCancelExecute(Sender: TObject);
begin
  Wanted.Clear();

  Session.Connection.Terminate();

  MainAction('aDCancel').Enabled := False;

  StatusBar.Panels[sbMessage].Text := '';
  StatusBarRefresh();
end;

procedure TFSession.aDCreateDatabaseExecute(Sender: TObject);
begin
  Wanted.Clear();

  DDatabase.Session := Session;
  DDatabase.Database := nil;
  DDatabase.Execute();
end;

procedure TFSession.aDCreateEventExecute(Sender: TObject);
begin
  Wanted.Clear();

  if (FocusedSItem is TSDatabase) then
  begin
    DEvent.Database := TSDatabase(FocusedSItem);
    DEvent.Event := nil;
    DEvent.Execute();
  end;
end;

procedure TFSession.aDCreateExecute(Sender: TObject);
begin
  Wanted.Clear();

  if ((Window.ActiveControl = FNavigator) or (Window.ActiveControl = ActiveListView)) then
  begin
    if (MainAction('aDCreateDatabase').Enabled) then MainAction('aDCreateDatabase').Execute()
    else if (MainAction('aDCreateDatabase').Enabled) then MainAction('aDCreateDatabase').Execute()
    else if (MainAction('aDCreateTable').Enabled) then MainAction('aDCreateTable').Execute()
    else if (MainAction('aDCreateField').Enabled) then MainAction('aDCreateField').Execute()
    else if (MainAction('aDCreateUser').Enabled) then MainAction('aDCreateUser').Execute();
  end
  else if (Window.ActiveControl = ActiveSynMemo) then
    ActiveSynMemo.InsertMode := not ActiveSynMemo.InsertMode
  else if (Window.ActiveControl = ActiveDBGrid) and (not ActiveDBGrid.EditorMode) then
    MainAction('aDInsertRecord').Execute();
end;

procedure TFSession.aDCreateFieldExecute(Sender: TObject);
var
  Table: TSBaseTable;
begin
  Wanted.Clear();

  if (FocusedSItem is TSBaseTable) then
  begin
    Table := TSBaseTable(FocusedSItem);

    DField.Table := Table;
    DField.Field := nil;
    DField.ModifyTableOnly := False;
    DField.Execute();
  end;
end;

procedure TFSession.aDCreateForeignKeyExecute(Sender: TObject);
var
  Table: TSBaseTable;
begin
  Wanted.Clear();

  if (FocusedSItem is TSBaseTable) then
  begin
    Table := TSBaseTable(FocusedSItem);

    DForeignKey.Table := Table;
    DForeignKey.ParentTable := nil;
    DForeignKey.ForeignKey := nil;
    DForeignKey.ModifyTableOnly := False;
    DForeignKey.Execute();
  end;
end;

procedure TFSession.aDCreateKeyExecute(Sender: TObject);
var
  Table: TSBaseTable;
begin
  Wanted.Clear();

  if (FocusedSItem is TSBaseTable) then
  begin
    Table := TSBaseTable(FocusedSItem);

    DKey.Table := Table;
    DKey.Key := nil;
    DKey.Execute();
  end;
end;

procedure TFSession.aDCreateRoutineExecute(Sender: TObject);
begin
  Wanted.Clear();

  if (FocusedSItem is TSDatabase) then
  begin
    DRoutine.Database := TSDatabase(FocusedSItem);
    if (Sender = MainAction('aDCreateProcedure')) then
      DRoutine.RoutineType := TSRoutine.TRoutineType.rtProcedure
    else if (Sender = MainAction('aDCreateFunction')) then
      DRoutine.RoutineType := TSRoutine.TRoutineType.rtFunction
    else
      DRoutine.RoutineType := TSRoutine.TRoutineType.rtUnknown;
    DRoutine.Routine := nil;
    DRoutine.Execute();
  end;
end;

procedure TFSession.aDCreateTableExecute(Sender: TObject);
begin
  Wanted.Clear();

  if ((Window.ActiveControl = ActiveWorkbench) and Assigned(ActiveWorkbench) and (FNavigator.Selected.ImageIndex = iiDatabase) and (View = vDiagram)) then
    ActiveWorkbench.CreateNewTable(0, 0)
  else if (FocusedSItem is TSDatabase) then
  begin
    DTable.Database := TSDatabase(FocusedSItem);
    DTable.Table := nil;
    DTable.Execute();
  end;
end;

procedure TFSession.aDCreateTriggerExecute(Sender: TObject);
begin
  Wanted.Clear();

  if (FocusedSItem is TSBaseTable) then
  begin
    DTrigger.Table := TSBaseTable(FocusedSItem);
    DTrigger.Trigger := nil;
    DTrigger.Execute();
  end;
end;

procedure TFSession.aDCreateUserExecute(Sender: TObject);
begin
  Wanted.Clear();

  DUser.Session := Session;
  DUser.User := nil;
  DUser.Execute();
end;

procedure TFSession.aDCreateViewExecute(Sender: TObject);
begin
  Wanted.Clear();

  if (FocusedSItem is TSDatabase) then
  begin
    DView.Database := TSDatabase(FocusedSItem);
    DView.View := nil;
    DView.Execute();
  end;
end;

procedure TFSession.aDDeleteExecute(Sender: TObject);
var
  I: Integer;
  Items: TList;
  J: Integer;
  List: TList;
  Msg: string;
  NewTable: TSBaseTable;
  Success: Boolean;
  Table: TSBaseTable;
begin
  Wanted.Clear();

  Items := TList.Create();

  if ((Window.ActiveControl = ActiveListView) and (ActiveListView.SelCount > 1)) then
  begin
    for I := 0 to ActiveListView.Items.Count - 1 do
      if (ActiveListView.Items[I].Selected) then
        Items.Add(ActiveListView.Items[I].Data);
  end
  else if ((Window.ActiveControl = ActiveWorkbench) and (ActiveWorkbench.SelCount > 1)) then
  begin
    for I := 0 to ActiveWorkbench.ControlCount - 1 do
      if ((ActiveWorkbench.Controls[I] is TWTable) and (TWTable(ActiveWorkbench.Controls[I]).Selected)) then
        Items.Add(TWTable(ActiveWorkbench.Controls[I]).BaseTable)
      else if ((ActiveWorkbench.Controls[I] is TWForeignKey) and (TWForeignKey(ActiveWorkbench.Controls[I]).Selected)) then
        Items.Add(TWForeignKey(ActiveWorkbench.Controls[I]).BaseForeignKey)
  end
  else if (Assigned(FocusedSItem)) then
    Items.Add(FocusedSItem);

  if (Items.Count > 1) then
    Msg := Preferences.LoadStr(413)
  else if (Items.Count = 1) then
  begin
    if (TSItem(Items[0]) is TSDatabase) then Msg := Preferences.LoadStr(146, TSItem(Items[0]).Caption)
    else if (TSItem(Items[0]) is TSBaseTable) then Msg := Preferences.LoadStr(113, TSItem(Items[0]).Caption)
    else if (TSItem(Items[0]) is TSView) then Msg := Preferences.LoadStr(748, TSItem(Items[0]).Caption)
    else if (TSItem(Items[0]) is TSProcedure) then Msg := Preferences.LoadStr(772, TSItem(Items[0]).Caption)
    else if (TSItem(Items[0]) is TSFunction) then Msg := Preferences.LoadStr(773, TSItem(Items[0]).Caption)
    else if (TSItem(Items[0]) is TSEvent) then Msg := Preferences.LoadStr(813, TSItem(Items[0]).Caption)
    else if (TSItem(Items[0]) is TSTrigger) then Msg := Preferences.LoadStr(787, TSItem(Items[0]).Caption)
    else if (TSItem(Items[0]) is TSKey) then Msg := Preferences.LoadStr(162, TSItem(Items[0]).Caption)
    else if (TSItem(Items[0]) is TSField) then Msg := Preferences.LoadStr(100, TSItem(Items[0]).Caption)
    else if (TSItem(Items[0]) is TSForeignKey) then Msg := Preferences.LoadStr(692, TSItem(Items[0]).Caption)
    else if (TSItem(Items[0]) is TSUser) then Msg := Preferences.LoadStr(428, TSItem(Items[0]).Caption)
    else if (TSItem(Items[0]) is TSProcess) then Msg := Preferences.LoadStr(534, TSItem(Items[0]).Caption);
  end
  else
    Msg := '';

  if ((Msg <> '') and (MsgBox(Msg, Preferences.LoadStr(101), MB_YESNOCANCEL + MB_ICONQUESTION) = IDYES)) then
  begin
    List := TList.Create();

    Success := True;

    for I := 0 to Items.Count - 1 do
      if ((TSItem(Items[I]) is TSDatabase) or (TSItem(Items[I]) is TSDBObject) or (TSItem(Items[I]) is TSProcess)) then
      begin
        List.Add(TSEntity(Items[I]));
        if (TSItem(Items[I]) is TSBaseTable) then
          for J := 0 to TSBaseTable(Items[I]).TriggerCount - 1 do
            List.Add(TSBaseTable(Items[I]).Triggers[J]);
        Items[I] := nil;
      end;
    if (Success and (List.Count > 0)) then
      Success := Session.DeleteEntities(List);

    Table := nil;
    for I := 0 to Items.Count - 1 do
      if (TSItem(Items[I]) is TSKey) then
        Table := TSKey(Items[I]).Table
      else if (TSItem(Items[I]) is TSBaseTableField) then
        Table := TSBaseTableField(Items[I]).Table
      else if (TSItem(Items[I]) is TSForeignKey) then
        Table := TSForeignKey(Items[I]).Table;
    if (Success and Assigned(Table)) then
    begin
      NewTable := TSBaseTable.Create(Table.Database.Tables);
      NewTable.Assign(Table);

      for I := Items.Count - 1 downto 0 do
        if ((TSItem(Items[I]) is TSKey) and (TSKey(Items[I]).Table = Table)) then
        begin
          // Debug 2016-12-19
          if (not Assigned(NewTable.KeyByName(TSKey(Items[I]).Name))) then
            raise ERangeError.Create('Key "' + TSKey(Items[I]).Name + '" not found!' + #13#10
              + 'Table SQL:' + #13#10
              + Table.Source);

          NewTable.Keys.Delete(NewTable.KeyByName(TSKey(Items[I]).Name));
          Items[I] := nil;
        end
        else if ((TSItem(Items[I]) is TSTableField) and (TSTableField(Items[I]).Table = Table)) then
        begin
          NewTable.Fields.Delete(NewTable.FieldByName(TSTableField(Items[I]).Name));
          Items[I] := nil;
        end
        else if ((TSItem(Items[I]) is TSForeignKey) and (TSForeignKey(Items[I]).Table = Table)) then
        begin
          NewTable.ForeignKeys.Delete(NewTable.ForeignKeyByName(TSForeignKey(Items[I]).Name));
          Items[I] := nil;
        end;

      if (NewTable.Fields.Count = 0) then
        Success := Table.Database.DeleteObject(NewTable)
      else
        Success := Table.Database.UpdateBaseTable(Table, NewTable);

      NewTable.Free();
    end;

    for I := 0 to Items.Count - 1 do
      if (TSItem(Items[I]) is TSUser) then
      begin
        List.Add(TSUser(Items[I]));
        Items[I] := nil;
      end;
    if (Success and (List.Count > 0)) then
      Session.DeleteEntities(List);

    List.Free();
  end;

  Items.Free();
end;

procedure TFSession.aDDeleteRecordExecute(Sender: TObject);
var
  Bookmarks: array of TBookmark;
  I: Integer;
begin
  Wanted.Clear();

  if (MsgBox(Preferences.LoadStr(176), Preferences.LoadStr(101), MB_YESNOCANCEL + MB_ICONQUESTION) = ID_YES) then
  begin
    if (ActiveDBGrid.SelectedRows.Count = 0) then
      aDDeleteRecord.Execute()
    else if (ActiveDBGrid.DataSource.DataSet is TMySQLDataSet) then
    begin
      SetLength(Bookmarks, ActiveDBGrid.SelectedRows.Count);
      for I := 0 to Length(Bookmarks) - 1 do
        Bookmarks[I] := ActiveDBGrid.SelectedRows.Items[I];
      ActiveDBGrid.SelectedRows.Clear();
      TMySQLDataSet(ActiveDBGrid.DataSource.DataSet).Delete(Bookmarks);
      SetLength(Bookmarks, 0);
    end;
  end;
end;

procedure TFSession.AddressChanged(Sender: TObject);
var
  Control: TWinControl;
  DDLStmt: TSQLDDLStmt;
  Empty: Boolean;
  I: Integer;
  NewActiveControl: TWinControl;
  NewAddressURI: TUURI;
  OldControl: TWinControl;
  OldAddressURI: TUURI;
  Parse: TSQLParse;
  Sibling: TTreeNode;
  SQL: string;
  Table: TSTable;
begin
  if (not (csDestroying in ComponentState)) then
  begin
    OldAddressURI := TUURI.Create(OldAddress);
    NewAddressURI := TUURI.Create(Address);

    if (Session.Databases.NameCmp(NewAddressURI.Database, OldAddressURI.Database) <> 0) then
      LastSelectedTable := '';

    if (NewAddressURI.Param['view'] <> OldAddressURI.Param['view']) then
      ViewChanged(nil);

    NewAddressURI.Free();
    OldAddressURI.Free();


    LeftMousePressed := False;

    OldControl := Window.ActiveControl;

    PHeaderCheckElements(Sender);
    PContentChange(Sender);


    while (Assigned(OldControl) and OldControl.Visible and OldControl.Enabled and Assigned(OldControl.Parent)) do
      OldControl := OldControl.Parent;

    case (View) of
      vObjects: NewActiveControl := ActiveListView;
      vBrowser: NewActiveControl := ActiveDBGrid;
      vIDE: NewActiveControl := ActiveSynMemo;
      vBuilder: NewActiveControl := FQueryBuilderActiveWorkArea();
      vDiagram: NewActiveControl := ActiveWorkbench;
      vEditor,
      vEditor2,
      vEditor3: NewActiveControl := ActiveSynMemo;
      vObjectSearch: NewActiveControl := ObjectSearchListView;
      else NewActiveControl := nil;
    end;

    Control := NewActiveControl;
    while (Assigned(Control) and Control.Visible and Control.Enabled and Assigned(Control.Parent)) do
      Control := Control.Parent;
    if ((not Assigned(OldControl) or not OldControl.Visible or not OldControl.Enabled)
      and Assigned(Control) and Control.Visible and Control.Enabled) then
      Window.ActiveControl := NewActiveControl;


    case (View) of
      vObjects: if (not (ttObjects in Preferences.ToolbarTabs)) then begin Include(Preferences.ToolbarTabs, ttObjects); PostMessage(Window.Handle, UM_CHANGEPREFERENCES, 0, 0); end;
      vBrowser: if (not (ttBrowser in Preferences.ToolbarTabs)) then begin Include(Preferences.ToolbarTabs, ttBrowser); PostMessage(Window.Handle, UM_CHANGEPREFERENCES, 0, 0); end;
      vIDE: if (not (ttIDE in Preferences.ToolbarTabs)) then begin Include(Preferences.ToolbarTabs, ttIDE); PostMessage(Window.Handle, UM_CHANGEPREFERENCES, 0, 0); end;
      vBuilder: if (not (ttBuilder in Preferences.ToolbarTabs)) then begin Include(Preferences.ToolbarTabs, ttBuilder); PostMessage(Window.Handle, UM_CHANGEPREFERENCES, 0, 0); end;
      vDiagram: if (not (ttDiagram in Preferences.ToolbarTabs)) then begin Include(Preferences.ToolbarTabs, ttDiagram); PostMessage(Window.Handle, UM_CHANGEPREFERENCES, 0, 0); end;
      vEditor: if (not (ttEditor in Preferences.ToolbarTabs)) then begin Include(Preferences.ToolbarTabs, ttEditor); PostMessage(Window.Handle, UM_CHANGEPREFERENCES, 0, 0); end;
      vEditor2: if (not (ttEditor2 in Preferences.ToolbarTabs)) then begin Include(Preferences.ToolbarTabs, ttEditor2); PostMessage(Window.Handle, UM_CHANGEPREFERENCES, 0, 0); end;
      vEditor3: if (not (ttEditor3 in Preferences.ToolbarTabs)) then begin Include(Preferences.ToolbarTabs, ttEditor3); PostMessage(Window.Handle, UM_CHANGEPREFERENCES, 0, 0); end;
    end;

    ToolBarData.Caption := AddressToCaption(Address);
    ToolBarData.View := View;
    Window.Perform(UM_UPDATETOOLBAR, 0, LPARAM(Self));

    if (Assigned(FNavigator.Selected) and (FNavigator.Selected <> LastFNavigatorSelected)) then
    begin
      if (FNavigator.AutoExpand and Assigned(FNavigator.Selected)) then
      begin
        if ((FNavigator.Selected.ImageIndex in [iiBaseTable, iiView, iiSystemView]) and not Dragging(FNavigator)) then
          FNavigator.Selected.Expand(False);

        if (Assigned(FNavigator.Selected.Parent)) then
        begin
          Sibling := FNavigator.Selected.Parent.getFirstChild();
          while (Assigned(Sibling)) do
          begin
            if (Sibling <> FNavigator.Selected) then
              Sibling.Collapse(False);
            Sibling := FNavigator.Selected.Parent.getNextChild(Sibling);
          end;
        end;
      end;

      if ((tsActive in FrameState) and not (tsLoading in FrameState) and Wanted.Nothing) then
        PlaySound(PChar(Preferences.SoundFileNavigating), Handle, SND_FILENAME or SND_ASYNC);

      if ((View = vBrowser) and (FNavigator.Selected.ImageIndex in [iiBaseTable, iiView, iiSystemView])) then
      begin
        Table := TSTable(FNavigator.Selected.Data);

        if (not (Table.DataSet is TSTable.TDataSet) or not Assigned(Table.DataSet) or not Table.DataSet.Active or (TSTable.TDataSet(Table.DataSet).Limit < 1)) then
        begin
          FUDOffset.Position := 0;
          FUDLimit.Position := Desktop(Table).Limit;
          FLimitEnabled.Down := Desktop(Table).Limited;
        end
        else
        begin
          FUDOffset.Position := TSTable.TDataSet(Table.DataSet).Offset;
          FUDLimit.Position := TSTable.TDataSet(Table.DataSet).Limit;
          FLimitEnabled.Down := TSTable.TDataSet(Table.DataSet).Limit > 1;
        end;
        FFilterEnabled.Down := Assigned(Table.DataSet) and Table.DataSet.Active and (Table.DataSet.FilterSQL <> '');
        if (not FFilterEnabled.Down) then
          FFilter.Text := ''
        else
          FFilter.Text := Table.DataSet.FilterSQL;
        FilterMRU.Clear();
        for I := 0 to Desktop(Table).FilterCount - 1 do
          FilterMRU.Add(Desktop(Table).Filters[I]);
        FFilterEnabled.Enabled := FFilter.Text <> '';
      end;

      if (SelectedDatabase <> '') then
        FQueryBuilder.MetadataContainer.DefaultDatabaseNameStr := SelectedDatabase;

      if (Window.ActiveControl = FNavigator) then
        FNavigatorChanged(FNavigator, FNavigator.Selected);

      FNavigator.AutoExpand := not (FNavigator.Selected.ImageIndex in [iiBaseTable, iiView, iiSystemView]) and not CheckWin32Version(6);

      TreeViewExpanded(FNavigator, FNavigator.Selected);
    end;

    LastFNavigatorSelected := FNavigator.Selected;
    if (SelectedDatabase <> '') then
      LastSelectedDatabase := SelectedDatabase;
    if (SelectedImageIndex in [iiBaseTable, iiView, iiSystemView]) then
    begin
      LastSelectedTable := FNavigator.Selected.Text;
      LastTableView := View;
    end
    else if (SelectedImageIndex = iiTrigger) then
      LastSelectedTable := FNavigator.Selected.Parent.Text;
    if (View = vIDE) then
      LastObjectIDEAddress := Address;


    Empty := not Assigned(ActiveSynMemo) or (ActiveSynMemo.Lines.Count <= 1) and (ActiveSynMemo.Text = ''); // Takes a lot of time
    if (not Empty and (View = vIDE)) then SQL := ActiveSynMemo.Text else SQL := '';

    MainAction('aFOpen').Enabled := (View = vDiagram) or (View in [vEditor, vEditor2, vEditor3]);
    MainAction('aFSave').Enabled := (View = vDiagram) or (View in [vEditor, vEditor2, vEditor3]) and not Empty and ((SQLEditors[View].Filename = '') or ActiveSynMemo.Modified);
    MainAction('aFSaveAs').Enabled := (View = vDiagram) or (View in [vEditor, vEditor2, vEditor3]) and not Empty;
    MainAction('aVObjects').Enabled := True;
    MainAction('aVBrowser').Enabled := (SelectedImageIndex in [iiBaseTable, iiView, iiSystemView, iiTrigger]) or ((LastSelectedDatabase <> '') and (LastSelectedDatabase = SelectedDatabase) and (LastSelectedTable <> ''));
    MainAction('aVIDE').Enabled := (SelectedImageIndex in [iiView, iiProcedure, iiFunction, iiEvent, iiTrigger]) or (LastObjectIDEAddress <> '');
    MainAction('aVBuilder').Enabled := (LastSelectedDatabase <> '');
    MainAction('aVSQLEditor').Enabled := True;
    MainAction('aVSQLEditor2').Enabled := True;
    MainAction('aVSQLEditor3').Enabled := True;
    MainAction('aVDiagram').Enabled := (LastSelectedDatabase <> '');
    MainAction('aDRun').Enabled :=
      ((View in [vEditor, vEditor2, vEditor3])
      or ((View = vBuilder) and FQueryBuilder.Visible)
      or ((View = vIDE) and SQLSingleStmt(SQL) and (SelectedImageIndex in [iiView, iiProcedure, iiFunction, iiEvent]))) and not Empty;
    MainAction('aDRunSelection').Enabled := (((View in [vEditor, vEditor2, vEditor3]) and not Empty) or Assigned(ActiveSynMemo) and (Trim(ActiveSynMemo.SelText) <> ''));
    MainAction('aDPostObject').Enabled := (View = vIDE) and Assigned(ActiveSynMemo) and ActiveSynMemo.Modified and SQLSingleStmt(SQL)
      and ((SelectedImageIndex in [iiView]) and SQLCreateParse(Parse, PChar(SQL), Length(SQL),Session.Connection.MySQLVersion) and (SQLParseKeyword(Parse, 'SELECT'))
        or (SelectedImageIndex in [iiProcedure, iiFunction]) and SQLParseDDLStmt(DDLStmt, PChar(SQL), Length(SQL), Session.Connection.MySQLVersion) and (DDLStmt.DefinitionType = dtCreate) and (DDLStmt.ObjectType in [otProcedure, otFunction])
        or (SelectedImageIndex in [iiEvent, iiTrigger]));
    MainAction('aEFormatSQL').Enabled := not Empty;

    StatusBarRefresh();


    FNavigatorMenuNode := FNavigator.Selected;

    if (View <> vObjectSearch) then
      Wanted.Update := UpdateAfterAddressChanged;

    if (tsLoading in FrameState) then
    begin
      if (PSideBar.Visible) then
      begin
        if (PListView.Visible) then Window.ActiveControl := ActiveListView
        else if (PQueryBuilder.Visible) then Window.ActiveControl := FQueryBuilder
        else if (PSynMemo.Visible) then Window.ActiveControl := ActiveSynMemo
        else if (PResult.Visible) then Window.ActiveControl := ActiveDBGrid
        else if (PNavigator.Visible) then Window.ActiveControl := FNavigator;
      end
      else
        case (View) of
          vObjects: if (PListView.Visible) then Window.ActiveControl := ActiveListView;
          vBrowser: if (PResult.Visible) then Window.ActiveControl := ActiveDBGrid;
          vIDE: if (PSynMemo.Visible and Assigned(ActiveSynMemo)) then Window.ActiveControl := ActiveSynMemo;
          vBuilder: if (PQueryBuilder.Visible and Assigned(FQueryBuilderActiveWorkArea())) then Window.ActiveControl := FQueryBuilderActiveWorkArea();
          vDiagram: if (PWorkbench.Visible) then Window.ActiveControl := ActiveWorkbench;
          vEditor,
          vEditor2,
          vEditor3: if (PSynMemo.Visible) then Window.ActiveControl := ActiveSynMemo;
          vObjectSearch: if (PListView.Visible) then Window.ActiveControl := ObjectSearchListView;
        end;
      Exclude(FrameState, tsLoading);
    end;

    OldAddress := Address;

    KillTimer(Handle, tiShowSynCompletion);
  end;
end;

procedure TFSession.AddressChanging(const Sender: TObject; const NewAddress: String; var AllowChange: Boolean);
var
  Database: TSDatabase;
  DBObject: TSDBObject;
  Host: string; // Debug 2016-12-05
  Host2: string; // Debug 2016-15-05
  NotFound: Boolean;
  S: string;
  URI: TUURI;
begin
  URI := TUURI.Create(NewAddress); NotFound := False;

  // Debug 2016-12-01
  if (not Assigned(Session)) then
    raise ERangeError.Create(SRangeError);
  if (not Assigned(Session.Account)) then
    raise ERangeError.Create(SRangeError);
  if (not Assigned(Session.Account.Connection)) then
    raise ERangeError.Create(SRangeError); // Occurred on 2016-12-22
  // Debug 2016-12-08
  if (not (Session.Account.Connection is TPAccount.TConnection)) then
    raise ERangeError.Create(SRangeError);
  Host := URI.Host;
  if (Length(Host) > 256) then
    raise ERangeError.Create(SRangeError);
  Host2 := Session.Account.Connection.Host;
  if (Length(Host2) > 256) then
    raise ERangeError.Create(SRangeError);
  try
    lstrcmpi(PChar(Host), PChar(Host2));
  except
    raise ERangeError.Create(SRangeError + 'Host1: ' + Host + ', Host2: ' + Host2);
  end;
  lstrcmpi(PChar(Host), LOCAL_HOST);

  if (URI.Scheme <> 'mysql') then
    AllowChange := False
  else if ((lstrcmpi(PChar(URI.Host), PChar(Session.Account.Connection.Host)) <> 0) and ((lstrcmpi(PChar(URI.Host), LOCAL_HOST) <> 0))) then
    AllowChange := False
  else if (URI.Port <> Session.Account.Connection.Port) then
    AllowChange := False
  else if ((URI.Username <> '') and (lstrcmpi(PChar(URI.Username), PChar(Session.Account.Connection.Username)) <> 0)) then
    AllowChange := False
  else if ((URI.Password <> '') and (URI.Password <> Session.Account.Connection.Password)) then
    AllowChange := False
  else
  begin
    S := URI.Path;
    if (URI.Database <> '') then
      Delete(S, 1, 1 + Length(EscapeURL(URI.Database)));
    if (URI.Table <> '') then
      Delete(S, 1, 1 + Length(EscapeURL(URI.Table)));
    if ((S <> '') and (S <> '/')) then
      AllowChange := False;
  end;

  if (AllowChange) then
  begin
    if (URI.Param['system'] = 'processes') then
    begin
      Session.Processes.Invalidate();
      Session.Processes.Update();
    end
    else if (URI.Param['system'] = 'users') then
      Session.Users.Update()
    else if (URI.Param['system'] = 'variables') then
      Session.Variables.Update()
    else if (URI.Database = '') then
      Session.Update(nil, URI.Param['view'] = Null)
    else if (not Session.Databases.Update()) then
      AllowChange := False
    else
    begin
      Database := Session.DatabaseByName(URI.Database);
      if (not Assigned(Database)) then
        NotFound := True
      else if (not (ParamToView(URI.Param['view']) in [vEditor, vEditor2, vEditor3]) and not Database.Update((URI.Param['view'] = Null) and (URI.Table = '')) and ((URI.Table <> '') or (URI.Param['object'] <> Null))) then
        AllowChange := False
      else if ((URI.Table <> '') or (URI.Param['object'] <> Null)) then
      begin
        if (URI.Table <> '') then
          DBObject := Database.TableByName(URI.Table)
        else if ((URI.Param['objecttype'] = 'procedure') and (URI.Param['object'] <> Null)) then
          DBObject := Database.ProcedureByName(URI.Param['object'])
        else if ((URI.Param['objecttype'] = 'function') and (URI.Param['object'] <> Null)) then
          DBObject := Database.FunctionByName(URI.Param['object'])
        else if ((URI.Param['objecttype'] = 'event') and (URI.Param['object'] <> Null)) then
          DBObject := Database.EventByName(URI.Param['object'])
        else
          DBObject := nil;

        if (not Assigned(DBObject)) then
          NotFound := True
        else if ((DBObject is TSBaseTable) and (URI.Param['objecttype'] = 'trigger') and (not TSBaseTable(DBObject).Update() or not Database.Triggers.Update())) then
          AllowChange := False;
      end;
    end;

    if (NotFound) then
    begin
      AllowChange := False;
      Wanted.Clear();
    end
    else if (not AllowChange) then
      Wanted.Address := NewAddress;
  end;

  URI.Free();

  if (AllowChange and Assigned(ActiveDBGrid) and Assigned(ActiveDBGrid.DataSource.DataSet) and ActiveDBGrid.DataSource.DataSet.Active) then
    try
      if ((Window.ActiveControl = FText) or (Window.ActiveControl = FRTF) or (Window.ActiveControl = FHexEditor)) then
        Window.ActiveControl := ActiveDBGrid;
      ActiveDBGrid.DataSource.DataSet.CheckBrowseMode();
    except
      AllowChange := False;
    end;

  if (AllowChange) then
    if (Assigned(Window.ActiveControl) and IsChild(PContent.Handle, Window.ActiveControl.Handle)) then
      Window.ActiveControl := nil;
end;

function TFSession.AddressToCaption(const AAddress: string): string;
var
  URI: TUURI;
begin
  URI := TUURI.Create(AAddress); if (URI.Scheme <> 'mysql') then FreeAndNil(URI);

  if (not Assigned(URI)) then
    Result := ''
  else if (URI.Database <> '') then
  begin
    Result := URI.Database;
    if (URI.Table <> '') then
      Result := Result + '.' + URI.Table;
    if (((URI.Table = '') or (URI.Param['objecttype'] = 'trigger')) and (URI.Param['object'] <> Null)) then
      Result := Result + '.' + URI.Param['object'];
  end
  else if ((URI.Database = '') and (URI.Param['system'] = 'processes')) then
    Result := Preferences.LoadStr(24)
  else if ((URI.Database = '') and (URI.Param['system'] = 'users')) then
    Result := Preferences.LoadStr(561)
  else if ((URI.Database = '') and (URI.Param['system'] = 'variables')) then
    Result := Preferences.LoadStr(22);

  if ((ParamToView(URI.Param['view']) in [vEditor, vEditor2, vEditor3]) and (URI.Param['file'] <> Null)) then
    Result := Result + ' - ' + EscapeURL(URI.Param['file']);

  Result := UnescapeURL(Result);

  URI.Free();
end;

procedure TFSession.aDInsertRecordExecute(Sender: TObject);
begin
  Wanted.Clear();

  aDInsertRecord.Execute();
end;

procedure TFSession.aDNextExecute(Sender: TObject);
begin
  Wanted.Clear();

  ActiveDBGrid.DataSource.DataSet.MoveBy(ActiveDBGrid.RowCount - 1);
end;

procedure TFSession.aDPostObjectExecute(Sender: TObject);
begin
  Wanted.Clear();

  PostObject(Sender)
end;

procedure TFSession.aDPrevExecute(Sender: TObject);
begin
  Wanted.Clear();

  ActiveDBGrid.DataSource.DataSet.MoveBy(- (ActiveDBGrid.RowCount - 1));
end;

procedure TFSession.aDPropertiesExecute(Sender: TObject);
type
  TExecute = function (): Boolean of object;
var
  SItem: TSItem;
  I: Integer;
  Process: TSProcess;
begin
  Wanted.Clear();

  if ((Window.ActiveControl = ActiveListView) and (SelectedImageIndex in [iiDatabase, iiSystemDatabase]) and (ActiveListView.SelCount > 1)) then
  begin
    DTables.Tables := TList.Create();
    for I := 0 to ActiveListView.Items.Count - 1 do
      if (ActiveListView.Items[I].Selected and (ActiveListView.Items[I].ImageIndex = iiBaseTable)) then
        DTables.Tables.Add(ActiveListView.Items[I].Data);
    if (DTables.Execute()) then
      Wanted.Update := Session.Update;
    FreeAndNil(DTables.Tables);
  end
  else if ((Window.ActiveControl = ActiveWorkbench) and (ActiveWorkbench.Selected is TWSection)) then
  begin
    DSegment.Section := TWSection(ActiveWorkbench.Selected);
    DSegment.Execute();
    ActiveWorkbench.Selected := nil;
  end
  else
  begin
    SItem := FocusedSItem;

    if (SItem is TSDatabase) then
    begin
      DDatabase.Session := Session;
      DDatabase.Database := TSDatabase(SItem);
      DDatabase.Execute();
    end
    else if (SItem is TSBaseTable) then
    begin
      DTable.Database := TSBaseTable(SItem).Database;
      DTable.Table := TSBaseTable(SItem);
      DTable.Execute();
    end
    else if (SItem is TSView) then
    begin
      DView.Database := TSView(SItem).Database;
      DView.View := TSView(SItem);
      if (DView.Execute()) then
        Wanted.Update := UpdateAfterAddressChanged;
    end
    else if (SItem is TSProcedure) then
    begin
      DRoutine.Database := TSRoutine(SItem).Database;
      DRoutine.Routine := TSRoutine(SItem);
      DRoutine.RoutineType := TSRoutine.TRoutineType.rtProcedure;
      DRoutine.Execute();
    end
    else if (SItem is TSFunction) then
    begin
      DRoutine.Database := TSRoutine(SItem).Database;
      DRoutine.Routine := TSRoutine(SItem);
      DRoutine.RoutineType := TSRoutine.TRoutineType.rtFunction;
      DRoutine.Execute();
    end
    else if (SItem is TSTrigger) then
    begin
      DTrigger.Table := TSTrigger(SItem).Table;
      DTrigger.Trigger := TSTrigger(SItem);
      DTrigger.Execute();
    end
    else if (SItem is TSEvent) then
    begin
      DEvent.Database := TSEvent(SItem).Database;
      DEvent.Event := TSEvent(SItem);
      DEvent.Execute();
    end
    else if (SItem is TSKey) then
    begin
      DKey.Table := TSKey(SItem).Table;
      DKey.Key := TSKey(SItem);
      DKey.Execute();
    end
    else if (SItem is TSBaseTableField) then
    begin
      DField.Table := TSBaseTable(TSBaseTableField(SItem).Table);
      DField.Field := TSBaseTableField(SItem);
      DField.ModifyTableOnly := False;
      DField.Execute();
    end
    else if (SItem is TSForeignKey) then
    begin
      DForeignKey.Table := TSForeignKey(SItem).Table;
      DForeignKey.ParentTable := nil;
      DForeignKey.ForeignKey := TSForeignKey(SItem);
      DForeignKey.ModifyTableOnly := False;
      DForeignKey.Execute();
    end
    else if (SItem is TSProcess) then
    begin
      Process := Session.ProcessByThreadId(SysUtils.StrToInt(ActiveListView.Selected.Caption));

      DStatement.DatabaseName := Process.DatabaseName;
      DStatement.DateTime := Session.Connection.ServerDateTime - Process.Time;
      DStatement.Host := Process.Host;
      DStatement.Id := Process.ThreadId;
      DStatement.StatementTime := Process.Time;
      if (UpperCase(Process.Command) <> 'QUERY') then
        DStatement.SQL := ''
      else
        DStatement.SQL := Process.SQL + ';';
      DStatement.UserName := Process.UserName;
      DStatement.ViewType := vtProcess;

      DStatement.Execute();
    end
    else if (SItem is TSUser) then
    begin
      DUser.Session := Session;
      DUser.User := TSUser(SItem);
      DUser.Execute();
    end
    else if (SItem is TSVariable) then
    begin
      DVariable.Session := Session;
      DVariable.Variable := TSVariable(SItem);
      DVariable.Execute();
    end;
  end;
end;

procedure TFSession.aDRunExecute(Sender: TObject);
var
  SQL: string;
begin
  Wanted.Clear();

  if (Window.ActiveControl is TMySQLDBGrid) then
    TMySQLDBGrid(Window.ActiveControl).EditorMode := False;

  if (Assigned(FQueryBuilderActiveSelectList())) then
    FQueryBuilderActiveSelectList().EditorMode := False;

  aDRunExecuteSelStart := 0;
  SQL := '';
  if (View in [vBuilder, vEditor, vEditor2, vEditor3]) then
    SQL := Trim(ActiveSynMemo.Text)
  else if ((View = vIDE) and (SelectedImageIndex = iiView)) then
  begin
    if (not ActiveSynMemo.Modified or PostObject(Sender)) then
      View := vBrowser;
  end
  else if ((View = vIDE) and (SelectedImageIndex = iiProcedure)) then
  begin
    if (Assigned(FObjectIDEGrid.DataSource.DataSet)) then
      FObjectIDEGrid.DataSource.DataSet.CheckBrowseMode();
    if (not ActiveSynMemo.Modified or PostObject(Sender)) then
      SQL := TSProcedure(FNavigator.Selected.Data).SQLRun();
  end
  else if ((View = vIDE) and (SelectedImageIndex = iiFunction)) then
  begin
    if (Assigned(FObjectIDEGrid.DataSource.DataSet)) then
      FObjectIDEGrid.DataSource.DataSet.CheckBrowseMode();
    if (not ActiveSynMemo.Modified or PostObject(Sender)) then
      SQL := TSFunction(FNavigator.Selected.Data).SQLRun();
  end
  else if ((View = vIDE) and (SelectedImageIndex = iiEvent)) then
  begin
    if (not ActiveSynMemo.Modified or PostObject(Sender)) then
      SQL := TSEvent(FNavigator.Selected.Data).SQLRun();
  end;

  if (SQL <> '') then
  begin
    if ((SelectedDatabase <> '') and (SelectedDatabase <> Session.Connection.DatabaseName)) then
    begin
      Dec(aDRunExecuteSelStart, Length(Session.Connection.SQLUse(SelectedDatabase)) + 1);
      SQL := Session.Connection.SQLUse(SelectedDatabase) + SQL;
    end;

    SendQuery(Sender, SQL);
  end;
end;

procedure TFSession.aDRunSelectionExecute(Sender: TObject);
var
  I: Integer;
  Index: Integer;
  Len: Integer;
  SQL: string;
begin
  Wanted.Clear();

  aDRunExecuteSelStart := ActiveSynMemo.SelStart;
  if (ActiveSynMemo.SelText = '') then
  begin
    SQL := ActiveSynMemo.Text;
    Index := 1; I := Index; Len := 1;
    while ((I <= aDRunExecuteSelStart + 1) and (Index < Length(SQL)) and (Len > 0)) do
    begin
      Len := SQLStmtLength(PChar(@SQL[Index]), Length(SQL) - (Index - 1));
      Inc(Index, Len);
      I := Index; while ((I > 1) and (CharInSet(SQL[I - 1], [#10,#13]))) do Dec(I);
    end;
    Dec(Index, Len);
    SQL := Copy(SQL, Index, Len);
    aDRunExecuteSelStart := Index - 1;
  end
  else
    SQL := Trim(ActiveSynMemo.SelText);

  if (SQL <> '') then
  begin
    if ((SelectedDatabase <> '') and (SelectedDatabase <> Session.Connection.DatabaseName)) then
    begin
      Dec(aDRunExecuteSelStart, Length(Session.Connection.SQLUse(SelectedDatabase)) + 1);
      SQL := Session.Connection.SQLUse(SelectedDatabase) + SQL;
    end;

    SendQuery(Sender, SQL);
  end;
end;

procedure TFSession.aEClearAllExecute(Sender: TObject);
begin
  Wanted.Clear();

  if (Window.ActiveControl = FText) then
    FText.Text := ''
  else if (Window.ActiveControl = ActiveSynMemo) then
  begin
    // ClearAll kann nicht mit Undo Rückgängig gemacht werden.
    ActiveSynMemo.BeginUpdate();
    ActiveSynMemo.SelectAll();
    MainAction('aEDelete').Execute();
    ActiveSynMemo.EndUpdate();
  end;
end;

procedure TFSession.aECopyExecute(Sender: TObject);
var
  ClipboardData: HGLOBAL;
  Data: string;
  Filename: array [0 .. MAX_PATH] of Char;
  I: Integer;
  ImageIndex: Integer;
  S: string;
  StringList: TStringList;
begin
  // Debug 2017-01-02
  if (OpenClipboard(Handle)) then
    CloseClipboard()
  else if (GetLastError() = ERROR_ACCESS_DENIED) then
  begin
    CloseClipboard();
    if (OpenClipboard(Handle)) then
    begin
      CloseClipboard();
      SendToDeveloper('Clipboard now opened (1)' + #13#10
        + TOSVersion.ToString);
    end
    else if (GetLastError() = ERROR_ACCESS_DENIED) then
    begin
      Sleep(500);
      CloseClipboard();
      Sleep(500);
      if (OpenClipboard(Handle)) then
      begin
        SendToDeveloper('Clipboard openable after Sleep (1)' + #13#10
          + TOSVersion.ToString);
        CloseClipboard();
        Sleep(500);
      end
      else
      begin
        SetString(S, PChar(@Filename[0]), GetWindowModuleFileName(GetClipboardOwner(), PChar(@Filename[0]), Length(Filename)));
        SendToDeveloper('Clipboard still not openable (1): ' + SysErrorMessage(GetLastError()) + #13#10
          + S + #13#10
          + TOSVersion.ToString);
      end;
    end;
  end;


  Data := '';

  if (not Assigned(Window.ActiveControl)) then
  begin
    MessageBeep(MB_ICONERROR);
    Exit;
  end
  else if (Window.ActiveControl = FNavigator) then
  begin
    if (not Assigned(FNavigatorMenuNode.Parent)) then
      ImageIndex := -1
    else
    begin
      ImageIndex := FNavigatorMenuNode.Parent.ImageIndex;
      case (FNavigatorMenuNode.ImageIndex) of
        iiDatabase:   Data := Data + 'Database='    + FNavigatorMenuNode.Text + #13#10;
        iiBaseTable:  Data := Data + 'Table='       + FNavigatorMenuNode.Text + #13#10;
        iiView:       Data := Data + 'View='        + FNavigatorMenuNode.Text + #13#10;
        iiProcedure:  Data := Data + 'Procedure='   + FNavigatorMenuNode.Text + #13#10;
        iiFunction:   Data := Data + 'Function='    + FNavigatorMenuNode.Text + #13#10;
        iiEvent:      Data := Data + 'Event='       + FNavigatorMenuNode.Text + #13#10;
        iiKey:        Data := Data + 'Key='         + TSKey(FNavigatorMenuNode.Data).Name + #13#10;
        iiSystemViewField,
        iiField,
        iiVirtualField,
        iiViewField:  Data := Data + 'Field='       + FNavigatorMenuNode.Text + #13#10;
        iiForeignKey: Data := Data + 'ForeignKey='  + FNavigatorMenuNode.Text + #13#10;
        iiTrigger:    Data := Data + 'Trigger='     + FNavigatorMenuNode.Text + #13#10;
        iiUser:       Data := Data + 'User='        + FNavigatorMenuNode.Text + #13#10;
      end;
      if (Data <> '') then
        Data := 'Address=' + NavigatorNodeToAddress(FNavigatorMenuNode.Parent) + #13#10 + Data;
    end;
  end
  else if (Window.ActiveControl = ActiveListView) then
  begin
    ImageIndex := SelectedImageIndex;
    for I := 0 to ActiveListView.Items.Count - 1 do
      if (ActiveListView.Items[I].Selected) then
        case (ActiveListView.Items[I].ImageIndex) of
          iiDatabase:   Data := Data + 'Database='   + ActiveListView.Items[I].Caption + #13#10;
          iiBaseTable:  Data := Data + 'Table='      + ActiveListView.Items[I].Caption + #13#10;
          iiView:       Data := Data + 'View='       + ActiveListView.Items[I].Caption + #13#10;
          iiProcedure:  Data := Data + 'Procedure='  + ActiveListView.Items[I].Caption + #13#10;
          iiFunction:   Data := Data + 'Function='   + ActiveListView.Items[I].Caption + #13#10;
          iiEvent:      Data := Data + 'Event='      + ActiveListView.Items[I].Caption + #13#10;
          iiKey:
            begin
              // Debug 2016-11-11
              if (not (TObject(ActiveListView.Items[I].Data) is TSKey)) then
                raise ERangeError.Create(SPropertyOutOfRange + ' (' + TObject(ActiveListView.Items[I].Data).ClassName + ')');
              Data := Data + 'Key='        + TSKey(ActiveListView.Items[I].Data).Name + #13#10;
            end;
          iiField,
          iiVirtualField,
          iiViewField:  Data := Data + 'Field='      + ActiveListView.Items[I].Caption + #13#10;
          iiForeignKey: Data := Data + 'ForeignKey=' + ActiveListView.Items[I].Caption + #13#10;
          iiTrigger:    Data := Data + 'Trigger='    + ActiveListView.Items[I].Caption + #13#10;
          iiUser:       Data := Data + 'User='       + ActiveListView.Items[I].Caption + #13#10;
        end;
    if (Data <> '') then
      Data := 'Address=' + NavigatorNodeToAddress(FNavigator.Selected) + #13#10 + Data;
  end
  else if (Window.ActiveControl = ActiveDBGrid) then
  begin
    ImageIndex := 8;
    if (not Assigned(EditorField)) then
      ActiveDBGrid.CopyToClipboard()
    else if (FText.Visible) then
      FText.CopyToClipboard()
    else if (FRTF.Visible) then
      FRTF.CopyToClipboard()
    else
      MessageBeep(MB_ICONERROR);
  end
  else if (Window.ActiveControl = ActiveWorkbench) then
  begin
    if ((ActiveWorkbench.Selected is TWSection) and OpenClipboard(Handle)) then
    begin
      try
        EmptyClipboard();

        S := TWSection(ActiveWorkbench.Selected).Caption;
        ClipboardData := GlobalAlloc(GMEM_MOVEABLE + GMEM_DDESHARE, SizeOf(S[1]) * (Length(S) + 1));
        StrPCopy(GlobalLock(ClipboardData), S);
        SetClipboardData(CF_UNICODETEXT, ClipboardData);
        GlobalUnlock(ClipboardData);
      finally
        CloseClipboard();
      end;

      exit;
    end
    else
    begin
      ImageIndex := SelectedImageIndex;
      if (Assigned(ActiveWorkbench)) then
      begin
        Data := 'Address=' + NavigatorNodeToAddress(FNavigator.Selected);
        if (not Assigned(ActiveWorkbench.Selected)) then
          Data := Data + 'Database='   + ActiveWorkbench.Database.Name + #13#10
        else if (ActiveWorkbench.Selected is TWTable) then
          Data := Data + 'Table='      + TWTable(ActiveWorkbench.Selected).BaseTable.Name + #13#10
        else if (ActiveWorkbench.Selected is TWForeignKey) then
          Data := Data + 'ForeignKey=' + TWForeignKey(ActiveWorkbench.Selected).BaseForeignKey.Name + #13#10;
        if (Data <> '') then
          Data := 'Address=' + NavigatorNodeToAddress(FNavigator.Selected) + #13#10 + Data;
      end;
    end;
  end
  else if (Window.ActiveControl = FSQLHistory) then
  begin
    if (Assigned(FSQLHistory.Selected) and OpenClipboard(Handle)) then
    begin
      try
        EmptyClipboard();

        S := XMLNode(IXMLNode(FSQLHistoryMenuNode.Data), 'sql').Text;
        ClipboardData := GlobalAlloc(GMEM_MOVEABLE + GMEM_DDESHARE, SizeOf(S[1]) * (Length(S) + 1));
        StrPCopy(GlobalLock(ClipboardData), S);
        SetClipboardData(CF_UNICODETEXT, ClipboardData);
        GlobalUnlock(ClipboardData);
      finally
        CloseClipboard();
      end;
    end;
    exit;
  end
  else if (Window.ActiveControl = FHexEditor) then
  begin
    FHexEditor.ExecuteAction(MainAction('aECopy'));
    exit;
  end
  else if (Window.ActiveControl = ActiveSynMemo) then
  begin
    ActiveSynMemo.CopyToClipboard();
    exit;
  end
  else
  begin
    SendMessage(Window.ActiveControl.Handle, WM_COPY, 0, 0);
    exit;
  end;

  // Debug 2017-01-02
  if (OpenClipboard(Handle)) then
    CloseClipboard()
  else if (GetLastError() = ERROR_ACCESS_DENIED) then
  begin
    CloseClipboard();
    if (OpenClipboard(Handle)) then
    begin
      CloseClipboard();
      SendToDeveloper('Clipboard now opened (2)' + ' - ' + Window.ActiveControl.ClassName + #13#10
        + TOSVersion.ToString);
    end
    else if (GetLastError() = ERROR_ACCESS_DENIED) then
    begin
      Sleep(500);
      CloseClipboard();
      Sleep(500);
      if (OpenClipboard(Handle)) then
      begin
        SendToDeveloper('Clipboard openable after Sleep (2)' + #13#10
          + TOSVersion.ToString);
        CloseClipboard();
        Sleep(500);
      end
      else
      begin
        SetString(S, PChar(@Filename[0]), GetWindowModuleFileName(GetClipboardOwner(), PChar(@Filename[0]), Length(Filename)));
        SendToDeveloper('Clipboard still not openable (2): ' + SysErrorMessage(GetLastError()) + #13#10
          + S + #13#10
          + TOSVersion.ToString);
      end;
    end;
  end;


  if ((Data <> '') and OpenClipboard(Handle)) then
  begin
    try
      EmptyClipboard();

      ClipboardData := GlobalAlloc(GMEM_MOVEABLE + GMEM_DDESHARE, SizeOf(Char) * (Length(Data) + 1));
      StrPCopy(GlobalLock(ClipboardData), Data);
      case (ImageIndex) of
        iiServer: SetClipboardData(CF_MYSQLSERVER, ClipboardData);
        iiDatabase,
        iiSystemDatabase: SetClipboardData(CF_MYSQLDATABASE, ClipboardData);
        iiTable,
        iiBaseTable,
        iiSystemView: SetClipboardData(CF_MYSQLTABLE, ClipboardData);
        iiView: SetClipboardData(CF_MYSQLVIEW, ClipboardData);
        iiUsers: SetClipboardData(CF_MYSQLUSERS, ClipboardData);
      end;
      GlobalUnlock(ClipboardData);

      StringList := TStringList.Create();
      StringList.Text := Trim(Data);
      for I := 1 to StringList.Count - 1 do
        if (StringList.ValueFromIndex[I] <> '') then
        begin
          if (S <> '') then S := S + ',';
          S := S + StringList.ValueFromIndex[I];
        end;
      StringList.Free();

      ClipboardData := GlobalAlloc(GMEM_MOVEABLE + GMEM_DDESHARE, SizeOf(S[1]) * (Length(S) + 1));
      StrPCopy(GlobalLock(ClipboardData), S);
      SetClipboardData(CF_UNICODETEXT, ClipboardData);
      GlobalUnlock(ClipboardData);
    finally
      CloseClipboard();
    end;
  end;
end;

procedure TFSession.aEFindExecute(Sender: TObject);
begin
  Wanted.Clear();

  DSearch.Session := Session;
  DSearch.Database := Session.DatabaseByName(SelectedDatabase);
  DSearch.SearchOnly := True;
  DSearch.Tab := Self;
  DSearch.Execute();
end;

procedure TFSession.aEPasteExecute(Sender: TObject);
var
  B: Boolean;
  ClipboardData: HGLOBAL;
  Control: TWinControl;
  FileName: array [0..MAX_PATH] of Char; // Debug 2012-12
  I: Integer;
  Msg: string; // Debug 2016-12-07
  Node: TTreeNode;
  S: string;
  Text: array [0..128] of Char; // Debug 2016-12-07
begin
  // Debug 2016-12-14
  if (not OpenClipboard(Handle)) then
    raise ERangeError.Create('LastError: ' + SysErrorMessage(GetLastError()) + #13#10
      + 'ActiveControl: ' + Window.ActiveControl.ClassName)
  else
    CloseClipboard();

  if (Session.Connection.InUse()) then
    MessageBeep(MB_ICONERROR)
  else if (Assigned(ActiveDBGrid) and (Window.ActiveControl = ActiveDBGrid)) then
  begin
    if (not Assigned(EditorField)) then
      ActiveDBGrid.PasteFromClipboard()
    else if (FText.Visible) then
      FText.PasteFromClipboard()
    else if (FRTF.Visible) then
      FRTF.PasteFromClipboard();
  end
  else if (Assigned(ActiveDBGrid) and (Window.ActiveControl = ActiveDBGrid.InplaceEditor)) then
  begin
    if (not ActiveDBGrid.DataSource.DataSet.CanModify or ActiveDBGrid.SelectedField.ReadOnly) then
      MessageBeep(MB_ICONERROR)
    else
    begin
      ActiveDBGrid.DataSource.DataSet.Edit();
      ActiveDBGrid.InplaceEditor.PasteFromClipboard();
    end;
  end
  else if ((Window.ActiveControl = FNavigator) or Assigned(ActiveListView) and (Window.ActiveControl = ActiveListView)) then
  begin
    if (Window.ActiveControl = FNavigator) then
      Node := FNavigatorMenuNode
    else if ((Window.ActiveControl = ActiveListView) and Assigned(ActiveListView.Selected)) then
    begin
      Node := nil;
      FNavigatorExpanding(Sender, FNavigator.Selected, B);
      for I := 0 to FNavigator.Selected.Count - 1 do
        if ((FNavigator.Selected[I].ImageIndex = ActiveListView.Selected.ImageIndex)
          and (FNavigator.Selected[I].Text = ActiveListView.Selected.Caption)) then
          Node := FNavigator.Selected[I];
    end
    else
      Node := FNavigator.Selected;

    if (not Assigned(Node)) then
      MessageBeep(MB_ICONERROR)
    else if ((Node.ImageIndex > 0) and OpenClipboard(Handle)) then
    begin
      try
        case (Node.ImageIndex) of
          iiServer: ClipboardData := GetClipboardData(CF_MYSQLSERVER);
          iiDatabase: ClipboardData := GetClipboardData(CF_MYSQLDATABASE);
          iiBaseTable: ClipboardData := GetClipboardData(CF_MYSQLTABLE);
          iiView: ClipboardData := GetClipboardData(CF_MYSQLVIEW);
          iiUsers: ClipboardData := GetClipboardData(CF_MYSQLUSERS);
          else ClipboardData := 0;
        end;

        if (ClipboardData <> 0) then
        begin
          SetString(S, PChar(GlobalLock(ClipboardData)), GlobalSize(ClipboardData) div SizeOf(S[1]));
          GlobalUnlock(ClipboardData);
        end;
      finally
        CloseClipboard();
      end;

      if (ClipboardData = 0) then
        MessageBeep(MB_ICONERROR)
      else
        PasteExecute(Node, S);
    end;
  end
  else if (Window.ActiveControl = ActiveSynMemo) then
    try
      ActiveSynMemo.PasteFromClipboard();
    except
      on E: EClipboardException do
        begin
          Msg := E.Message;
          SetString(S, PChar(@FileName[0]), GetWindowModuleFileName(GetClipboardOwner(), PChar(@FileName[0]), Length(FileName)));
          Msg := Msg + #10 + 'Filename: ' + S;
          SetString(S, PChar(@Text[0]), GetWindowText(GetClipboardOwner(), PChar(@Text[0]), Length(Text)));
          Msg := Msg + #10 + 'WindowText: ' + S;
          try
            Control := GetControlByHandle(Window, GetClipboardOwner());
            if (Assigned(Control)) then
              Msg := Msg + #10 + 'ClassType: ' + Control.ClassName
                 + #10 + 'Name: ' + Control.Name;
          except
          end;
          raise Exception.Create(E.Message + #10 + 'Clipboard Owner: ' + Msg);
        end;
    end
  else if (Assigned(ActiveWorkbench) and (Window.ActiveControl = ActiveWorkbench)) then
    WorkbenchPasteExecute(Sender)
  else if (Window.ActiveControl = FFilter) then
    FFilter.PasteFromClipboard()
  else if (Window.ActiveControl = FQuickSearch) then
    FQuickSearch.PasteFromClipboard()
  else if (Window.ActiveControl is TCustomEdit) then
    TCustomEdit(Window.ActiveControl).PasteFromClipboard()
  else
    MessageBeep(MB_ICONERROR);

  // Debug 2016-12-14
  if (not OpenClipboard(Handle)) then
    raise ERangeError.Create('ActiveControl: ' + Window.ActiveControl.ClassName)
  else
    CloseClipboard();
end;

procedure TFSession.aEPasteFromExecute(Sender: TObject);
begin
  Wanted.Clear();

  OpenSQLFile('', 0, True);
end;

procedure TFSession.aEPasteFromFileExecute(Sender: TObject);
var
  CodePage: Cardinal;
  Content: Pointer;
  Handle: THandle;
  FileSize: DWord;
  FileSizeHigh: DWord;
  Mem: Pointer;
  Mem2: PChar;
  RBS: AnsiString;
  Size: DWord;
begin
  Wanted.Clear();

  OpenDialog.Title := Preferences.LoadStr(581);
  OpenDialog.InitialDir := Path;
  OpenDialog.FileName := '';
  if (ActiveDBGrid.SelectedField.DataType = ftWideMemo) then
  begin
    OpenDialog.DefaultExt := 'txt';
    OpenDialog.Filter := FilterDescription('txt') + ' (*.txt)|*.txt|' + FilterDescription('*') + ' (*.*)|*.*';
    OpenDialog.Encodings.Text := EncodingCaptions();
    OpenDialog.EncodingIndex := OpenDialog.Encodings.IndexOf(CodePageToEncoding(Session.Connection.CodePage));
  end
  else if (ActiveDBGrid.SelectedField.DataType = ftBlob) then
  begin
    OpenDialog.DefaultExt := '';
    OpenDialog.Filter := FilterDescription('*') + ' (*.*)|*.*';
    OpenDialog.Encodings.Clear();
    OpenDialog.EncodingIndex := -1;
  end;

  if (OpenDialog.Execute()) then
  begin
    Path := ExtractFilePath(OpenDialog.FileName);

    Handle := CreateFile(PChar(OpenDialog.FileName),
                         GENERIC_READ,
                         FILE_SHARE_READ,
                         nil,
                         OPEN_EXISTING, 0, 0);

    if (Handle = INVALID_HANDLE_VALUE) then
      RaiseLastOSError()
    else
    begin
      FileSize := GetFileSize(Handle, @FileSizeHigh);

      if (FileSizeHigh > 0) then
        raise ERangeError.Create(SRangeError)
      else
      begin
        GetMem(Mem, FileSize);
        ActiveDBGrid.DataSource.DataSet.Edit();

        if (not ReadFile(Handle, Mem^, FileSize, Size, nil) or (Size < FileSize)) then
          RaiseLastOSError()
        else if (ActiveDBGrid.SelectedField.DataType = ftBlob) then
        begin
          SetString(RBS, PAnsiChar(Mem), Size);
          ActiveDBGrid.SelectedField.AsAnsiString := RBS;
        end
        else if (ActiveDBGrid.SelectedField.DataType = ftWideMemo) then
        begin
          if (CompareMem(Mem, BOM_UNICODE_LE, Length(BOM_UNICODE_LE))) then
          begin
            CodePage := CP_UNICODE;
            Content := @PAnsiChar(Mem)[Length(BOM_UNICODE_LE)];
            FileSize := FileSize - Cardinal(Length(BOM_UNICODE_LE));
          end
          else if (CompareMem(Mem, BOM_UTF8, Length(BOM_UTF8))) then
          begin
            CodePage := CP_UTF8;
            Content := @PAnsiChar(Mem)[Length(BOM_UTF8)];
            FileSize := FileSize - Cardinal(Length(BOM_UTF8));
          end
          else
          begin
            CodePage := EncodingToCodePage(OpenDialog.Encodings[OpenDialog.EncodingIndex]);
            Content := Mem;
          end;

          case (CodePage) of
            CP_UNICODE: ActiveDBGrid.SelectedField.AsString := StrPas(PWideChar(Content));
            else
            begin
              Size := AnsiCharToWideChar(CodePage, PAnsiChar(Content), FileSize, nil, 0);
              GetMem(Mem2, Size * SizeOf(Char));
              AnsiCharToWideChar(CodePage, PAnsiChar(Content), FileSize, Mem2, Size);
              ActiveDBGrid.SelectedField.AsString := StrPas(Mem2);
              FreeMem(Mem2);
            end;
          end;
        end
        else
          raise ERangeError.Create(SRangeError);
        FreeMem(Mem);
      end;
      CloseHandle(Handle);
    end;
  end;
end;

procedure TFSession.aERedoExecute(Sender: TObject);
begin
  ActiveSynMemo.Redo();
  SynMemoStatusChange(Sender, [scAll]);
end;

procedure TFSession.aERenameExecute(Sender: TObject);
begin
  if (Window.ActiveControl = FNavigator) then
    FNavigatorMenuNode.EditText()
  else if (Window.ActiveControl = ActiveListView) then
    ActiveListView.Selected.EditCaption();
end;

procedure TFSession.aEReplaceExecute(Sender: TObject);
begin
  Wanted.Clear();

  DSearch.Session := Session;
  DSearch.Database := Session.DatabaseByName(SelectedDatabase);
  DSearch.SearchOnly := False;
  DSearch.Tab := Self;
  DSearch.Execute();
  Wanted.Update := Session.Update;
end;

procedure TFSession.aESelectAllExecute(Sender: TObject);
begin
  if (not Assigned(ActiveListView)) then
    raise ERangeError.Create('Address: ' + Address + #13#10
      + 'Assigned: ' + BoolToStr(Assigned(GetActiveListView())));

  ActiveListView.SelectAll();
end;

procedure TFSession.aETransferExecute(Sender: TObject);
begin
  Wanted.Clear();

  DTransfer.SourceSession := Session;
  DTransfer.SourceDatabase := Session.DatabaseByName(SelectedDatabase);
  DTransfer.Execute();
end;

procedure TFSession.aFExportAccessExecute(Sender: TObject);
begin
  Wanted.Clear();

  aFExportExecute(Sender, etAccessFile);
end;

procedure TFSession.aFExportBitmapExecute(Sender: TObject);
begin
  Wanted.Clear();

  SaveDialog.Title := Preferences.LoadStr(582);
  SaveDialog.InitialDir := Path;
  SaveDialog.FileName := SelectedDatabase + '.bmp';
  SaveDialog.DefaultExt := 'bmp';
  SaveDialog.Filter := FilterDescription('bmp') + ' (*.bmp)|*.bmp';
  SaveDialog.Encodings.Clear();
  SaveDialog.EncodingIndex := -1;
  if (SaveDialog.Execute()) then
  begin
    ActiveWorkbench.SaveToBMP(SaveDialog.FileName);

    Path := ExtractFilePath(SaveDialog.FileName);
  end;
end;

procedure TFSession.aFExportExcelExecute(Sender: TObject);
begin
  Wanted.Clear();

  aFExportExecute(Sender, etExcelFile);
end;

procedure TFSession.aFExportExecute(const Sender: TObject; const ExportType: TExportType);
var
  Database: TSDatabase;
  I: Integer;
begin
  DExport.Session := Session;
  DExport.DBGrid := nil;
  DExport.ExportType := ExportType;
  DExport.DBObjects.Clear();
  DExport.Window := Window;

  if (not Assigned(Window.ActiveControl)) then
    MessageBeep(MB_ICONERROR)
  else if (Window.ActiveControl = ActiveDBGrid) then
    DExport.DBGrid := ActiveDBGrid
  else if (Window.ActiveControl = ActiveWorkbench) then
  begin
    Database := TSDatabase(FNavigator.Selected.Data);
    if (((Session.Databases.NameCmp(Database.Name, 'mysql') <> 0) and (Session.Databases.NameCmp(Database.Name, 'sys') <> 0) and (Session.Connection.MySQLVersion >= 50707)) and not (Database is TSSystemDatabase)) then
      for I := 0 to ActiveWorkbench.Tables.Count - 1 do
        if (not Assigned(ActiveWorkbench.Selected) or ActiveWorkbench.Tables[I].Selected) then
          DExport.DBObjects.Add(ActiveWorkbench.Tables[I].BaseTable);
  end
  else if ((Window.ActiveControl = ActiveListView) and (ActiveListView.SelCount > 0)) then
  begin
    case (SelectedImageIndex) of
      iiServer:
        for I := 0 to ActiveListView.Items.Count - 1 do
          if (ActiveListView.Items[I].Selected) then
          begin
            Database := TSDatabase(ActiveListView.Items[I].Data);
            if (((Session.Databases.NameCmp(Database.Name, 'mysql') <> 0) and (Session.Databases.NameCmp(Database.Name, 'sys') <> 0) and (Session.Connection.MySQLVersion >= 50707)) and not (Database is TSSystemDatabase)) then
              DExport.DBObjects.Add(Database);
          end;

      iiDatabase:
        begin
          Database := TSDatabase(FNavigator.Selected.Data);
          if (not (Database is TSSystemDatabase)) then
            for I := 0 to ActiveListView.Items.Count - 1 do
              if (ActiveListView.Items[I].Selected) then
                DExport.DBObjects.Add(TSDBObject(ActiveListView.Items[I].Data));
        end;
      iiBaseTable:
        begin
          Database := TSDatabase(FNavigator.Selected.Parent.Data);
          if (not (Database is TSSystemDatabase)) then
            for I := 0 to ActiveListView.Items.Count - 1 do
              if (ActiveListView.Items[I].Selected and (ActiveListView.Items[I].ImageIndex = iiTrigger)) then
                DExport.DBObjects.Add(TSDBObject(ActiveListView.Items[I].Data));
        end;
    end
  end
  else if (FocusedSItem is TSDatabase) then
  begin
    Database := TSDatabase(FocusedSItem);
    if (not (Database is TSSystemDatabase)) then
      DExport.DBObjects.Add(Database);
  end
  else if (FocusedSItem is TSDBObject) then
  begin
    Database := TSDBObject(FocusedSItem).Database;
    if (not (Database is TSSystemDatabase)) then
      DExport.DBObjects.Add(FocusedSItem);
  end
  else
  begin
    for I := 0 to Session.Databases.Count - 1 do
      if (((Session.Databases.NameCmp(Session.Databases[I].Name, 'mysql') <> 0)
        and ((Session.Databases.NameCmp(Session.Databases[I].Name, 'sys') <> 0) or (Session.Connection.MySQLVersion < 50707)))
        and not (Session.Databases[I] is TSSystemDatabase)) then
        DExport.DBObjects.Add(Session.Databases[I]);
  end;

  DExport.Execute();
end;

procedure TFSession.aFExportHTMLExecute(Sender: TObject);
begin
  Wanted.Clear();

  aFExportExecute(Sender, etHTMLFile);
end;

procedure TFSession.aFExportODBCExecute(Sender: TObject);
begin
  Wanted.Clear();

  aFExportExecute(Sender, etODBC);
end;

procedure TFSession.aFExportPDFExecute(Sender: TObject);
begin
  Wanted.Clear();

  aFExportExecute(Sender, etPDFFile);
end;

procedure TFSession.aFExportSQLExecute(Sender: TObject);
begin
  Wanted.Clear();

  aFExportExecute(Sender, etSQLFile);
end;

procedure TFSession.aFExportTextExecute(Sender: TObject);
begin
  Wanted.Clear();

  aFExportExecute(Sender, etTextFile);
end;

procedure TFSession.aFExportXMLExecute(Sender: TObject);
begin
  Wanted.Clear();

  aFExportExecute(Sender, etXMLFile);
end;

procedure TFSession.aFImportAccessExecute(Sender: TObject);
begin
  Wanted.Clear();

  aFImportExecute(Sender, itAccessFile);
end;

procedure TFSession.aFImportExcelExecute(Sender: TObject);
begin
  Wanted.Clear();

  aFImportExecute(Sender, itExcelFile);
end;

procedure TFSession.aFImportExecute(const Sender: TObject; const ImportType: TImportType);
var
  SItem: TSItem;
begin
  SItem := FocusedSItem;

  if ((Sender is TAction)
    and ((SItem is TSDatabase) and not TSDatabase(SItem).Update()) or ((SItem is TSTable) and not TSTable(SItem).Update())) then
    Wanted.Action := TAction(Sender)
  else
  begin
    DImport.Session := Session;
    if (SItem is TSObject) then
      DImport.SObject := TSObject(SItem)
    else
      DImport.SObject := nil;
    DImport.Filename := '';
    DImport.CodePage := CP_ACP;
    DImport.Window := Window;
    DImport.ImportType := ImportType;

    // Debug 2016-12-12
    if (not Assigned(FNavigator)) then
      raise ERangeError.Create(SRangeError);
    // Debug 2016-12-22
    DImport.FNavigator := @FNavigator;

    DImport.Progress := '2';
    DImport.Execute();
    DImport.Progress := DImport.Progress + '_';

    // Debug 2017-01-06
    if (not Assigned(FNavigator)) then
      raise ERangeError.Create(SRangeError);

    UpdateAfterAddressChanged();
  end;
end;

procedure TFSession.aFImportODBCExecute(Sender: TObject);
begin
  Wanted.Clear();

  aFImportExecute(Sender, itODBC);
end;

procedure TFSession.aFImportSQLExecute(Sender: TObject);
begin
  Wanted.Clear();

  aFImportExecute(Sender, itSQLFile);
end;

procedure TFSession.aFImportTextExecute(Sender: TObject);
begin
  Wanted.Clear();

  aFImportExecute(Sender, itTextFile);
end;

procedure TFSession.aFOpenExecute(Sender: TObject);
begin
  Wanted.Clear();

  if (View = vDiagram) then
    OpenDiagram()
  else if (Boolean(Perform(UM_CLOSE_TAB_QUERY, 0, 0))) then
    OpenSQLFile('');
end;

procedure TFSession.aFSaveAsExecute(Sender: TObject);
begin
  Wanted.Clear();

  if (View = vDiagram) then
    SaveDiagram(Sender)
  else
    SaveSQLFile(Sender);
end;

procedure TFSession.aFSaveExecute(Sender: TObject);
begin
  Wanted.Clear();

  if (View = vDiagram) then
    SaveDiagram(Sender)
  else
    SaveSQLFile(Sender);
end;

procedure TFSession.AfterExecuteSQL(Sender: TObject);
var
  Hour: Word;
  Minute: Word;
  MSec: Word;
  Msg: string;
  Second: Word;
begin
  MainAction('aDCancel').Enabled := False;

  if (Session.Connection.RowsAffected < 0) then
    Msg := Preferences.LoadStr(382)
  else
    Msg := Preferences.LoadStr(658, IntToStr(Session.Connection.RowsAffected));

  DecodeTime(Session.Connection.ExecutionTime, Hour, Minute, Second, MSec);
  if (Hour > 0) or (Minute > 0) then
    Msg := Msg + '  (' + Preferences.LoadStr(520) + ': ' + FormatDateTime(FormatSettings.LongTimeFormat, Session.Connection.ExecutionTime) + ')'
  else if (Session.Connection.ExecutionTime >= 0) then
    Msg := Msg + '  (' + Preferences.LoadStr(520) + ': ' + Format('%2.2f', [Second + MSec / 1000]) + ')';

  StatusBarRefresh();
  StatusBar.Panels[sbMessage].Text := Msg;
  SetTimer(Handle, tiStatusBar, 5000, nil);

  if (not Wanted.Nothing) then
    Wanted.Execute();
end;

procedure TFSession.aHManualExecute(Sender: TObject);
begin
  ShellExecute(Window.Handle, 'open', PChar(Session.Account.ManualURL), '', '', SW_SHOW);
end;

procedure TFSession.aEFormatSQLExecute(Sender: TObject);
const
  EventPrefix = 'CREATE EVENT `Parser` ON SCHEDULE AT ''2016-01-01 00:00:00'' DO ';
  TriggerPrefix = 'CREATE TRIGGER `Parser` BEFORE INSERT ON `Table` FOR EACH ROW ';
var
  SQL: string;
begin
  Window.ActiveControl := ActiveSynMemo;

  // Debug 2016-11-12
  if (not Assigned(ActiveSynMemo)) then
    raise ERangeError.Create(SRangeError);

  if (ActiveSynMemo.SelAvail) then
    SQL := ActiveSynMemo.SelText
  else
  begin
    SQL := ActiveSynMemo.Text;
    case (SelectedImageIndex) of
      iiTrigger:
        SQL := TriggerPrefix + SQL;
      iiEvent:
        SQL := EventPrefix + SQL;
    end;
  end;

  if (not Session.SQLParser.ParseSQL(SQL)) then
    MsgBox(Session.SQLParser.ErrorMessage, Preferences.LoadStr(45), MB_OK + MB_ICONERROR)
  else
  begin
    SQL := Session.SQLParser.FormatSQL();

    if (ActiveSynMemo.SelAvail) then
      ActiveSynMemo.SelText := SQL
    else
    begin
      case (SelectedImageIndex) of
        iiTrigger:
          begin
            Delete(SQL, 1, Pos('FOR EACH ROW', SQL) + Length('FOR EACH ROW'));
            SQL := Trim(SQL);
          end;
        iiEvent:
          begin
            Delete(SQL, 1, Pos('DO', SQL) + Length('DO'));
            SQL := Trim(SQL);
          end;
      end;
      // Using SelectAll / SelText to have the option using the Undo feature of SynMemo
      ActiveSynMemo.SelectAll();
      ActiveSynMemo.SelText := SQL;
    end;
  end;

  Session.SQLParser.Clear();
end;

procedure TFSession.aHRunClick(Sender: TObject);
var
  SQL: string;
begin
  if (Assigned(FSQLHistoryMenuNode) and (FSQLHistoryMenuNode.ImageIndex in [iiStatement, iiQuery, iiClock])) then
  begin
    Wanted.Clear();

    if (not (View in [vEditor, vEditor2, vEditor3])) then
      View := vEditor;
    if (View in [vEditor, vEditor2, vEditor3]) then
    begin
      SQL := '';

      if ((XMLNode(IXMLNode(FSQLHistoryMenuNode.Data), 'database').Text <> Session.Connection.DatabaseName) and (XMLNode(IXMLNode(FSQLHistoryMenuNode.Data), 'database').Text <> '')) then
        SQL := SQL + Session.Connection.SQLUse(XMLNode(IXMLNode(FSQLHistoryMenuNode.Data), 'database').Text);

      SQL := SQL + XMLNode(IXMLNode(FSQLHistoryMenuNode.Data), 'sql').Text;

      SendQuery(Sender, SQL);
    end;
  end;
end;

procedure TFSession.aHRunExecute(Sender: TObject);
var
  SQL: string;
begin
  Wanted.Clear();

  if (Assigned(FSQLHistoryMenuNode) and (FSQLHistoryMenuNode.ImageIndex in [iiStatement, iiQuery, iiClock])) then
  begin
    if (not (View in [vEditor, vEditor2, vEditor3])) then
      View := vEditor;
    if (View in [vEditor, vEditor2, vEditor3]) then
    begin
      SQL := '';

      if ((XMLNode(IXMLNode(FSQLHistoryMenuNode.Data), 'database').Text <> Session.Connection.DatabaseName) and (XMLNode(IXMLNode(FSQLHistoryMenuNode.Data), 'database').Text <> '')) then
        SQL := SQL + Session.Connection.SQLUse(XMLNode(IXMLNode(FSQLHistoryMenuNode.Data), 'database').Text);

      SQL := SQL + XMLNode(IXMLNode(FSQLHistoryMenuNode.Data), 'sql').Text;

      SendQuery(Sender, SQL);
    end;
  end;
end;

procedure TFSession.aHSQLExecute(Sender: TObject);
begin
  if (Assigned(ActiveSynMemo) and (Window.ActiveControl = ActiveSynMemo)) then
    if (ActiveSynMemo.SelText <> '') then
      DSQLHelp.Keyword := ActiveSynMemo.SelText
    else if (ActiveSynMemo.WordAtCursor <> '') then
      DSQLHelp.Keyword := ActiveSynMemo.WordAtCursor
    else
      DSQLHelp.Keyword := ''
  else
    DSQLHelp.Keyword := '';

  DSQLHelp.Session := Session;
  DSQLHelp.Execute();
end;

procedure TFSession.aPCollapseExecute(Sender: TObject);
begin
  if (Window.ActiveControl = FNavigator) then
    FNavigatorMenuNode.Collapse(False)
  else if (Window.ActiveControl = FSQLHistory) then
    FSQLHistoryMenuNode.Collapse(False);
end;

procedure TFSession.aPExpandExecute(Sender: TObject);
begin
  if (Window.ActiveControl = FNavigator) then
    FNavigatorMenuNode.Expand(False)
  else if (Window.ActiveControl = FSQLHistory) then
    FSQLHistoryMenuNode.Expand(False);
end;

procedure TFSession.aPObjectBrowserTableExecute(Sender: TObject);
var
  URI: TUURI;
begin
  Wanted.Clear();

  URI := TUURI.Create(Address);
  URI.Param['view'] := Null;
  if (URI.Database <> '') then
    URI.Table := LastSelectedTable;
  Address := URI.Address;
  URI.Free();
end;

procedure TFSession.aPResultExecute(Sender: TObject);
begin
  Wanted.Clear();

  if (PResult.Visible and Assigned(ActiveDBGrid)) then
    Window.ActiveControl := ActiveDBGrid;
end;

procedure TFSession.aSSearchFindNotFound(Sender: TObject);
begin
  MsgBox(Preferences.LoadStr(533, TSearchFind(Sender).Dialog.FindText), Preferences.LoadStr(43), MB_OK + MB_ICONINFORMATION);
end;

procedure TFSession.aSynCompletionExecuteExecute(Sender: TObject);
begin
  with SynCompletionPending do
    SynCompletion.Execute(CurrentInput, X, Y);
  SynCompletionPending.Active := False;
end;

procedure TFSession.aTBFilterExecute(Sender: TObject);
begin
  if (View = vBrowser) then
    Window.ActiveControl := FFilter;
end;

procedure TFSession.aTBLimitExecute(Sender: TObject);
begin
  if (View = vBrowser) then
    Window.ActiveControl := FLimit;
end;

procedure TFSession.aTBOffsetExecute(Sender: TObject);
begin
  if (View = vBrowser) then
    Window.ActiveControl := FOffset;
end;

procedure TFSession.aTBQuickSearchExecute(Sender: TObject);
begin
  if (View = vBrowser) then
    Window.ActiveControl := FQuickSearch;
end;

procedure TFSession.aVBlobExecute(Sender: TObject);
begin
  Wanted.Clear();

  FHTMLHide(Sender);

  FText.Visible := (Sender = aVBlobText) or not Assigned(Sender) and aVBlobText.Checked;
  FRTF.Visible := (Sender = aVBlobRTF) or not Assigned(Sender) and aVBlobRTF.Checked;
  if (Assigned(FHTML)) then
    FHTML.Visible := (Sender = aVBlobHTML) or not Assigned(Sender) and aVBlobHTML.Checked;
  FImage.Visible := (Sender = aVBlobImage) or not Assigned(Sender) and aVBlobImage.Checked;
  FHexEditor.Visible := (Sender = aVBlobHexEditor) or not Assigned(Sender) and aVBlobHexEditor.Checked;
  PToolBarBlobResize(Sender);

  if (FText.Visible) then
    FTextShow(Sender)
  else if (FRTF.Visible) then
    FRTFShow(Sender)
  else if ((Sender = aVBlobHTML) or not Assigned(Sender) and aVBlobHTML.Checked) then
    FHTMLShow(Sender)
  else if (FImage.Visible) then
    FImageShow(Sender)
  else if (FHexEditor.Visible) then
    FHexEditorShow(Sender);
end;

procedure TFSession.aViewExecute(Sender: TObject);
var
  AllowChange: Boolean;
  Control: TWinControl; // Debug 2016-12-12
  I: Integer;
  NewView: TView;
begin
  if (Sender = MainAction('aVObjects')) then
    NewView := vObjects
  else if (Sender = MainAction('aVBrowser')) then
    NewView := vBrowser
  else if (Sender = MainAction('aVIDE')) then
    NewView := vIDE
  else if (Sender = MainAction('aVBuilder')) then
    NewView := vBuilder
  else if (Sender = MainAction('aVDiagram')) then
    NewView := vDiagram
  else if (Sender = MainAction('aVSQLEditor')) then
    NewView := vEditor
  else if (Sender = MainAction('aVSQLEditor2')) then
    NewView := vEditor2
  else if (Sender = MainAction('aVSQLEditor3')) then
    NewView := vEditor3
  else
    raise ERangeError.Create(SRangeError);

  AllowChange := True;
  if (AllowChange and Assigned(ActiveDBGrid) and Assigned(ActiveDBGrid.DataSource.DataSet) and ActiveDBGrid.DataSource.DataSet.Active) then
    try
      if ((Window.ActiveControl = FText) or (Window.ActiveControl = FRTF) or (Window.ActiveControl = FHexEditor)) then
        Window.ActiveControl := ActiveDBGrid;
      ActiveDBGrid.DataSource.DataSet.CheckBrowseMode();
    except
      AllowChange := False;
    end;

  if (AllowChange) then
  begin
    // Debug 2016-11-14
    if (not Assigned(Window)) then
      raise ERangeError.Create(SRangeError);

    View := NewView;

    case (View) of
      vObjects: if (PListView.Visible) then Window.ActiveControl := ActiveListView;
      vBrowser: if (PResult.Visible and Assigned(ActiveDBGrid)) then Window.ActiveControl := ActiveDBGrid;
      vIDE: if (PSynMemo.Visible and Assigned(ActiveSynMemo)) then Window.ActiveControl := ActiveSynMemo;
      vBuilder: if (PQueryBuilder.Visible) then
        if (FQueryBuilder.Visible and Assigned(FQueryBuilderActiveWorkArea())) then
        begin
          // Debug 2016-12-05
          if (not FQueryBuilderActiveWorkArea().Visible) then
            raise ERangeError.Create(SRangeError);
          // Debug 2016-12-08
          if (not FQueryBuilderActiveWorkArea().Enabled) then
            raise ERangeError.Create(SRangeError);

          // Debug 2016-12-12
          Control := FQueryBuilderActiveWorkArea();
          repeat
            if (not Control.Enabled) then
              raise ERangeError.Create('Control is not enabled: ' + Control.Name + ' / ' + Control.ClassName);
            if (not Control.Visible) then
              raise ERangeError.Create('Control is not visible: ' + Control.Name + ' / ' + Control.ClassName);
            if (not Assigned(Control.Parent)) then
              raise ERangeError.Create('Control has no parent: ' + Control.Name + ' / ' + Control.ClassName);
            Control := Control.Parent;
          until (not Assigned(Control) or (Control = Window));

          Window.ActiveControl := FQueryBuilderActiveWorkArea()
        end
        else
          Window.ActiveControl := FQueryBuilderSynMemo;
      vDiagram: if (PWorkbench.Visible) then Window.ActiveControl := ActiveWorkbench;
      vEditor,
      vEditor2,
      vEditor3: if (PSynMemo.Visible) then
        begin
          // Debug 2016-11-30
          if (not ActiveSynMemo.Visible) then
            raise ERangeError.Create(SRangeError);
          for I := 0 to PSynMemo.ControlCount - 1 do
            if ((PSynMemo.Controls[I] <> ActiveSynMemo)
              and PSynMemo.Controls[I].Visible) then
              raise ERangeError.Create(SRangeError + '(' + PSynMemo.Controls[I].ClassName + ')');
          Window.ActiveControl := ActiveSynMemo;
        end;
      vObjectSearch: if (PListView.Visible) then Window.ActiveControl := ObjectSearchListView;
    end;
  end;
end;

procedure TFSession.aVRefreshAllExecute(Sender: TObject);
var
  ChangeEvent: TTVChangedEvent;
  ChangingEvent: TTVChangingEvent;
  TempAddress: string;
begin
  KillTimer(Handle, tiStatusBar);
  KillTimer(Handle, tiNavigator);

  Session.Invalidate();

  TempAddress := Address;

  ChangingEvent := FNavigator.OnChanging; FNavigator.OnChanging := nil;
  ChangeEvent := FNavigator.OnChange; FNavigator.OnChange := nil;
  FNavigator.Selected := nil;
  FNavigator.OnChanging := ChangingEvent;
  FNavigator.OnChange := ChangeEvent;

  Address := TempAddress;
end;

procedure TFSession.aVRefreshExecute(Sender: TObject);
var
  AllowRefresh: Boolean;
  List: TList;
begin
  if (GetKeyState(VK_SHIFT) < 0) then
    aVRefreshAllExecute(Sender)
  else
  begin
    KillTimer(Handle, tiNavigator);

    case (View) of
      vObjects,
      vIDE:
        begin
          case (SelectedImageIndex) of
            iiServer: Session.Invalidate();
            iiDatabase,
            iiSystemDatabase: TSDatabase(FNavigator.Selected.Data).Invalidate();
            iiBaseTable,
            iiView,
            iiSystemView:
              begin
                // Debug 2016-11-21
                if (not Assigned(FNavigator.Selected.Data)) then
                  raise ERangeError.Create(SRangeError);
                if (not (TObject(FNavigator.Selected.Data) is TSTable)) then
                  raise ERangeError.Create(SRangeError + ' (' + IntToStr(FNavigator.Selected.ImageIndex) + ')');

                TSTable(FNavigator.Selected.Data).Invalidate();

                if ((TSTable(FNavigator.Selected.Data) is TSBaseTable) and
                  Assigned(TSBaseTable(FNavigator.Selected.Data).Database.Triggers)) then
                  TSBaseTable(FNavigator.Selected.Data).Database.Triggers.Invalidate();
              end;
            iiProcedure: TSProcedure(FNavigator.Selected.Data).Invalidate();
            iiFunction: TSFunction(FNavigator.Selected.Data).Invalidate();
            iiEvent: TSEvent(FNavigator.Selected.Data).Invalidate();
            iiTrigger: TSTrigger(FNavigator.Selected.Data).Invalidate();
            iiProcesses: Session.Processes.Invalidate();
            iiUsers: Session.Users.Invalidate();
            iiVariables: Session.Variables.Invalidate();
          end;
          Address := Address;
        end;
      vBrowser,
      vBuilder,
      vEditor,
      vEditor2,
      vEditor3:
        begin
          if ((View = vBrowser) or PResult.Visible) then
          begin
            if (not Assigned(ActiveDBGrid)) then
              // Why is this needed??? 2017-01-06
              ActiveDBGrid := GetActiveDBGrid();
            if (ActiveDBGrid.DataSource.DataSet is TMySQLDataSet) then
            begin
              AllowRefresh := ActiveDBGrid.DataSource.DataSet.Active;

              if (AllowRefresh) then
              begin
                ActiveDBGrid.EditorMode := False;
                try
                  ActiveDBGrid.DataSource.DataSet.CheckBrowseMode();
                except
                  AllowRefresh := False;
                end;
              end;

              if (AllowRefresh) then
                ActiveDBGrid.DataSource.DataSet.Refresh()
              else
                ActiveDBGrid.DataSource.DataSet.Open();
            end;
          end;
        end;
      vDiagram:
        begin
          TSDatabase(FNavigator.Selected.Data).Tables.Invalidate();
          List := TList.Create();
          List.Add(TSDatabase(FNavigator.Selected.Data).Tables);
          Session.Update(List);
          List.Free();
        end;
      vObjectSearch:
        begin
          ObjectSearchListView.Free();
          ObjectSearch.Step1();
          Wanted.Update := ObjectSearchStep2;
        end;
    end;
  end;
end;

procedure TFSession.aVSideBarExecute(Sender: TObject);
begin
  PSideBar.DisableAlign();

  MainAction('aVNavigator').Checked := (Sender = MainAction('aVNavigator')) and MainAction('aVNavigator').Checked;
  MainAction('aVExplorer').Checked := (Sender = MainAction('aVExplorer')) and MainAction('aVExplorer').Checked;
  MainAction('aVSQLHistory').Checked := (Sender = MainAction('aVSQLHistory')) and MainAction('aVSQLHistory').Checked;

  PNavigator.Visible := MainAction('aVNavigator').Checked;
  PExplorer.Visible := MainAction('aVExplorer').Checked;
  PSQLHistory.Visible := MainAction('aVSQLHistory').Checked;

  if (PExplorer.Visible and not Assigned(FFolders)) then
    CreateExplorer()
  else if (PSQLHistory.Visible) then
    FSQLHistoryRefresh(Sender);

  SSideBar.Visible := PNavigator.Visible or PExplorer.Visible or PSQLHistory.Visible;
  if (not SSideBar.Visible) then
    PSideBar.Visible := False
  else
  begin
    SSideBar.Left := PNavigator.Width;
    SSideBar.Align := alLeft;
    PSideBar.Visible := SSideBar.Visible;
  end;
  TBSideBar.Visible := PSideBar.Visible;


  if (PSideBar.Visible) then
  begin
    FormResize(Sender);
    PSideBar.EnableAlign();

    if (MainAction('aVNavigator').Checked) then
      Window.ActiveControl := FNavigator
    else if (MainAction('aVExplorer').Checked) then
      Window.ActiveControl := FFolders
    else if (MainAction('aVSQLHistory').Checked) then
      Window.ActiveControl := FSQLHistory;
  end;
end;

procedure TFSession.aVSortAscExecute(Sender: TObject);
var
  SortDef: TIndexDef;
begin
  Wanted.Clear();

  SortDef := TIndexDef.Create(nil, '', ActiveDBGrid.SelectedField.FieldName, []);

  TMySQLDataSet(ActiveDBGrid.DataSource.DataSet).Sort(SortDef);
  ActiveDBGrid.UpdateHeader();

  SortDef.Free();
end;

procedure TFSession.aVSortDescExecute(Sender: TObject);
var
  SortDef: TIndexDef;
begin
  Wanted.Clear();

  SortDef := TIndexDef.Create(nil, '', ActiveDBGrid.SelectedField.FieldName, []);
  SortDef.DescFields := ActiveDBGrid.SelectedField.FieldName;

  TMySQLDataSet(ActiveDBGrid.DataSource.DataSet).Sort(SortDef);
  ActiveDBGrid.UpdateHeader();

  SortDef.Free();
end;

procedure TFSession.aVSQLLogExecute(Sender: TObject);
begin
  PLog.Visible := MainAction('aVSQLLog').Checked;
  SLog.Visible := PLog.Visible;

  if (PLog.Visible) then
  begin
    // make sure, SLog is above PLog
    SLog.Align := alNone;
    SLog.Top := 0;
    SLog.Align := alBottom;

    FLogUpdate();
  end
  else
    FLog.Lines.Clear();

  FormResize(Sender);
end;

procedure TFSession.BeforeExecuteSQL(Sender: TObject);
begin
  StatusBar.Panels[sbMessage].Text := Preferences.LoadStr(196) + '...';
  KillTimer(Handle, tiStatusBar);

  MainAction('aDCancel').Enabled := True;
end;

procedure TFSession.BeginEditLabel(Sender: TObject);
begin
  KillTimer(Handle, tiNavigator);

  aDCreate.ShortCut := 0;
  aDDelete.ShortCut := 0;
end;

procedure TFSession.BObjectIDEClick(Sender: TObject);
var
  SQL: string;
  Trigger: TSTrigger;
begin
  Wanted.Clear();

  Trigger := nil;
  if (TObject(FNavigator.Selected.Data) is TSTrigger) then
    Trigger := TSTrigger(FNavigator.Selected.Data);

  if (Assigned(Trigger) and (not ActiveSynMemo.Modified or PostObject(Sender))) then
  begin
    FObjectIDEGrid.EditorMode := False;
    if (Sender = BINSERT) then
      SQL := Trigger.SQLInsert()
    else if (Sender = BREPLACE) then
      SQL := Trigger.SQLReplace()
    else if (Sender = BUPDATE) then
      SQL := Trigger.SQLUpdate()
    else if (Sender = BDELETE) then
      SQL := Trigger.SQLDelete();

    if (SelectedDatabase <> Session.Connection.DatabaseName) then
      SQL := Session.Connection.SQLUse(SelectedDatabase) + SQL;

    Session.Connection.SendSQL(SQL);
  end;
end;

function TFSession.ColumnWidthKindByListView(const ListView: TListView): TPAccount.TDesktop.TListViewKind;
begin
  if (ListView.Tag = 0) then
    Result := lkServer
  else if (TObject(ListView.Tag) is TSDatabase) then
    Result := lkDatabase
  else if (TObject(ListView.Tag) is TSTable) then
    Result := lkTable
  else if (TObject(ListView.Tag) is TSProcesses) then
    Result := lkProcesses
  else if (TObject(ListView.Tag) is TSUsers) then
    Result := lkUsers
  else if (TObject(ListView.Tag) is TSVariables) then
    Result := lkVariables
  else if (TOBJECT(ListView.Tag) is TSObjectSearch) then
    Result := lkObjectSearch
  else
    raise ERangeError.Create(SRangeError);
end;

procedure TFSession.CrashRescue();
var
  View: TView;
begin
  for View in [vEditor, vEditor2, vEditor3] do
    if (not Assigned(SQLEditors[View]) or (SQLEditors[View].Filename <> '') and not SQLEditors[View].SynMemo.Modified) then
      Session.Account.Desktop.EditorContent[ToolbarTabByView[View]] := ''
      else
      Session.Account.Desktop.EditorContent[ToolbarTabByView[View]] := SQLEditors[View].SynMemo.Text;

  try
    if (Assigned(ActiveWorkbench)) then
      ActiveWorkbench.SaveToFile(Session.Account.DataPath + ActiveWorkbench.Name + PathDelim + 'Diagram.xml');
  except
  end;
end;

constructor TFSession.Create(const AOwner: TComponent; const AParent: TWinControl; const ASession: TSSession; const AParam: string);
var
  Kind: TPAccount.TDesktop.TListViewKind;
  NonClientMetrics: TNonClientMetrics;
begin
  inherited Create(AOwner);


  // Debug 2016-12-15
  if (not (ASession.Account is TPAccount)) then
    raise ERangeError.Create(SRangeError);
  // Debug 2016-12-20
  if (not Assigned(ASession.Account.Desktop)) then
    raise ERangeError.Create(SRangeError);


  Parent := TWinControl(AParent);

  Width := Window.ClientWidth;
  Height := Window.ClientHeight;

  FrameState := [tsLoading];

  NMListView := nil;
  Session := ASession;
  SQLEditor := TSQLEditor.Create(Self, FSQLEditorSynMemo, PSQLEditorDBGrid);
  Param := AParam;
  ActiveControlOnDeactivate := nil;
  ActiveDBGrid := nil;
  ActiveIDEInputDataSet := nil;
  ActiveListView := FServerListView;
  ActiveWorkbench := nil;
  CloseButtonNormal := nil;
  CloseButtonPushed := nil;
  CloseButtonHot := nil;
  ObjectSearch := nil;
  ObjectSearchListView := nil;
  ProcessesListView := nil;
  UsersListView := nil;
  VariablesListView := nil;
  for Kind := lkServer to lkVariables do
  begin
    ListViewSortData[Kind].Kind := Kind;
    ListViewSortData[Kind].Index := 0;
    if (Kind = lkTable) then
      ListViewSortData[Kind].Order := 0
    else
      ListViewSortData[Kind].Order := 1;
  end;
  FNavigatorNodeAfterActivate := nil;
  FNavigatorNodeToExpand := nil;
  PanelMouseDownPoint := Point(-1, -1);
  SQueryBuilderSynMemoMoved := False;
  SynCompletionPending.Active := False;


  TBSideBar.Images := Preferences.Images;
  ToolBar.Images := Preferences.Images;
  TBObjectSearch.Images := Preferences.Images;
  FNavigator.Images := Preferences.Images;
  FSQLHistory.Images := Preferences.Images;
  TBLimitEnabled.Images := Preferences.Images;
  TBQuickSearchEnabled.Images := Preferences.Images;
  TBFilterEnabled.Images := Preferences.Images;
  FServerListView.SmallImages := Preferences.Images;

  FUDOffset.HandleNeeded();
  FOffset.HandleNeeded();
  FUDLimit.HandleNeeded();
  FLimit.HandleNeeded();

  tbNavigator.Action := MainAction('aVNavigator');
  tbExplorer.Action := MainAction('aVExplorer');
  tbSQLHistory.Action := MainAction('aVSQLHistory');

  tbObjects.Action := MainAction('aVObjects'); tbObjects.Caption := tbObjects.Caption;
  tbBrowser.Action := MainAction('aVBrowser'); tbBrowser.Caption := tbBrowser.Caption;
  tbIDE.Action := MainAction('aVIDE'); tbIDE.Caption := tbIDE.Caption;
  tbBuilder.Action := MainAction('aVBuilder'); tbBuilder.Caption := tbBuilder.Caption;
  tbDiagram.Action := MainAction('aVDiagram'); tbDiagram.Caption := tbDiagram.Caption;
  tbEditor.Action := MainAction('aVSQLEditor'); tbEditor.Caption := tbEditor.Caption;
  tbEditor2.Action := MainAction('aVSQLEditor2'); tbEditor2.Caption := tbEditor2.Caption;
  tbEditor3.Action := MainAction('aVSQLEditor3'); tbEditor3.Caption := tbEditor3.Caption;

  miSNavigator.Action := MainAction('aVNavigator');
  miSSQLHistory.Action := MainAction('aVSQLHistory');

  miNImportSQL.Action := MainAction('aFImportSQL');
  miNImportText.Action := MainAction('aFImportText');
  miNImportExcel.Action := MainAction('aFImportExcel');
  miNImportAccess.Action := MainAction('aFImportAccess');
  miNImportODBC.Action := MainAction('aFImportODBC');
  miNExportSQL.Action := MainAction('aFExportSQL');
  miNExportText.Action := MainAction('aFExportText');
  miNExportExcel.Action := MainAction('aFExportExcel');
  miNExportAccess.Action := MainAction('aFExportAccess');
  miNExportODBC.Action := MainAction('aFExportODBC');
  miNExportXML.Action := MainAction('aFExportXML');
  miNExportHTML.Action := MainAction('aFExportHTML');
  miNExportPDF.Action := MainAction('aFExportPDF');
  miNCopy.Action := MainAction('aECopy');
  miNPaste.Action := MainAction('aEPaste');
  miNRename.Action := MainAction('aERename');
  miNCreateDatabase.Action := MainAction('aDCreateDatabase');
  miNCreateTable.Action := MainAction('aDCreateTable');
  miNCreateView.Action := MainAction('aDCreateView');
  miNCreateProcedure.Action := MainAction('aDCreateProcedure');
  miNCreateFunction.Action := MainAction('aDCreateFunction');
  miNCreateIndex.Action := MainAction('aDCreateKey');
  miNCreateField.Action := MainAction('aDCreateField');
  miNCreateForeignKey.Action := MainAction('aDCreateForeignKey');
  miNCreateTrigger.Action := MainAction('aDCreateTrigger');
  miNCreateEvent.Action := MainAction('aDCreateEvent');
  miNCreateUser.Action := MainAction('aDCreateUser');
  miNEmpty.Action := MainAction('aDEmpty');

  miHCopy.Action := MainAction('aECopy');

  mlFImportSQL.Action := MainAction('aFImportSQL');
  mlFImportText.Action := MainAction('aFImportText');
  mlFImportExcel.Action := MainAction('aFImportExcel');
  mlFImportAccess.Action := MainAction('aFImportAccess');
  mlFImportODBC.Action := MainAction('aFImportODBC');
  mlFExportSQL.Action := MainAction('aFExportSQL');
  mlFExportText.Action := MainAction('aFExportText');
  mlFExportExcel.Action := MainAction('aFExportExcel');
  mlFExportAccess.Action := MainAction('aFExportAccess');
  mlFExportODBC.Action := MainAction('aFExportODBC');
  mlFExportXML.Action := MainAction('aFExportXML');
  mlFExportHTML.Action := MainAction('aFExportHTML');
  mlFExportPDF.Action := MainAction('aFExportPDF');
  mlECopy.Action := MainAction('aECopy');
  mlEPaste.Action := MainAction('aEPaste');
  mlERename.Action := MainAction('aERename');
  mlDCreateDatabase.Action := MainAction('aDCreateDatabase');
  mlDCreateTable.Action := MainAction('aDCreateTable');
  mlDCreateView.Action := MainAction('aDCreateView');
  mlDCreateProcedure.Action := MainAction('aDCreateProcedure');
  mlDCreateFunction.Action := MainAction('aDCreateFunction');
  mlDCreateIndex.Action := MainAction('aDCreateKey');
  mlDCreateField.Action := MainAction('aDCreateField');
  mlDCreateForeignKey.Action := MainAction('aDCreateForeignKey');
  mlDCreateTrigger.Action := MainAction('aDCreateTrigger');
  mlDCreateEvent.Action := MainAction('aDCreateEvent');
  mlDCreateUser.Action := MainAction('aDCreateUser');
  mlDEmpty.Action := MainAction('aDEmpty');

  mpDRun.Action := MainAction('aDRun');
  mpDRunSelection.Action := MainAction('aDRunSelection');
  mpECut.Action := MainAction('aECut');
  mpECopy.Action := MainAction('aECopy');
  mpEPaste.Action := MainAction('aEPaste');
  mpEDelete.Action := MainAction('aEDelete');
  mpECopyToFile.Action := MainAction('aECopyToFile');
  mpEPasteFromFile.Action := MainAction('aEPasteFromFile');
  mpESelectAll.Action := MainAction('aESelectAll');

  gmECut.Action := MainAction('aECut');
  gmECopy.Action := MainAction('aECopy');
  gmEPaste.Action := MainAction('aEPaste');
  gmEDelete.Action := MainAction('aEDelete');
  gmEmpty.Action := MainAction('aDEmpty');
  gmECopyToFile.Action := MainAction('aECopyToFile');
  gmEPasteFromFile.Action := MainAction('aEPasteFromFile');
  gmFExportSQL.Action := MainAction('aFExportSQL');
  gmFExportText.Action := MainAction('aFExportText');
  gmFExportExcel.Action := MainAction('aFExportExcel');
  gmFExportXML.Action := MainAction('aFExportXML');
  gmFExportHTML.Action := MainAction('aFExportHTML');
  gmFExportPDF.Action := MainAction('aFExportPDF');
  gmDInsertRecord.Action := MainAction('aDInsertRecord');
  gmDDeleteRecord.Action := MainAction('aDDeleteRecord');
  gmDEditRecord.Action := MainAction('aDEditRecord');

  mwFImportSQL.Action := MainAction('aFImportSQL');
  mwFImportText.Action := MainAction('aFImportText');
  mwFImportExcel.Action := MainAction('aFImportExcel');
  mwFImportAccess.Action := MainAction('aFImportAccess');
  mwFImportODBC.Action := MainAction('aFImportODBC');
  mwFExportSQL.Action := MainAction('aFExportSQL');
  mwFExportText.Action := MainAction('aFExportText');
  mwFExportExcel.Action := MainAction('aFExportExcel');
  mwFExportAccess.Action := MainAction('aFExportAccess');
  mwFExportODBC.Action := MainAction('aFExportODBC');
  mwFExportXML.Action := MainAction('aFExportXML');
  mwFExportHTML.Action := MainAction('aFExportHTML');
  mwFExportPDF.Action := MainAction('aFExportPDF');
  mwFExportBitmap.Action := MainAction('aFExportBitmap');
  mwECopy.Action := MainAction('aECopy');
  mwDCreateField.Action := MainAction('aDCreateField');
  mwDCreateForeignKey.Action := MainAction('aDCreateForeignKey');
  mwDEmpty.Action := MainAction('aDEmpty');

  tmECut.Action := MainAction('aECut');
  tmECopy.Action := MainAction('aECopy');
  tmEPaste.Action := MainAction('aEPaste');
  tmEDelete.Action := MainAction('aEDelete');
  tmESelectAll.Action := MainAction('aESelectAll');

  smECopy.Action := MainAction('aECopy');
  smESelectAll.Action := MainAction('aESelectAll');


  if (not StyleServices.Enabled and not CheckWin32Version(6)) then
  begin
    PNavigator.BevelInner := bvRaised; PNavigator.BevelOuter := bvLowered;
    PExplorer.BevelInner := bvRaised; PExplorer.BevelOuter := bvLowered;
    PSQLHistory.BevelInner := bvRaised; PSQLHistory.BevelOuter := bvLowered;
    PFolders.BevelInner := bvRaised; PFolders.BevelOuter := bvLowered;
    PFiles.BevelInner := bvRaised; PFiles.BevelOuter := bvLowered;
    PListView.BevelInner := bvRaised; PListView.BevelOuter := bvLowered;
    PQueryBuilderSynMemo.BevelInner := bvRaised; PQueryBuilderSynMemo.BevelOuter := bvLowered;
    PSynMemo.BevelInner := bvRaised; PSynMemo.BevelOuter := bvLowered;
    PWorkbench.BevelInner := bvRaised; PWorkbench.BevelOuter := bvLowered;
    PSQLEditorDBGrid.BevelInner := bvRaised; PSQLEditorDBGrid.BevelOuter := bvLowered;
    PBlob.BevelInner := bvRaised; PBlob.BevelOuter := bvLowered;
    PLog.BevelInner := bvRaised; PLog.BevelOuter := bvLowered;
  end
  else
  begin
    PNavigator.BevelInner := bvNone; PNavigator.BevelOuter := bvNone;
    PExplorer.BevelInner := bvNone; PExplorer.BevelOuter := bvNone;
    PFolders.BevelInner := bvNone; PNavigator.BevelOuter := bvNone;
    PFiles.BevelInner := bvNone; PFiles.BevelOuter := bvNone;
    PSQLHistory.BevelInner := bvNone; PSQLHistory.BevelOuter := bvNone;
    PListView.BevelInner := bvNone; PListView.BevelOuter := bvNone;
    PQueryBuilderSynMemo.BevelInner := bvNone; PQueryBuilderSynMemo.BevelOuter := bvNone;
    PSynMemo.BevelInner := bvNone; PSynMemo.BevelOuter := bvNone;
    PWorkbench.BevelInner := bvNone; PWorkbench.BevelOuter := bvNone;
    PSQLEditorDBGrid.BevelInner := bvNone; PSQLEditorDBGrid.BevelOuter := bvNone;
    PBlob.BevelInner := bvNone; PBlob.BevelOuter := bvNone;
    PLog.BevelInner := bvNone; PLog.BevelOuter := bvNone;
  end;

  FNavigator.RowSelect := CheckWin32Version(6, 1);
  FSQLHistory.RowSelect := CheckWin32Version(6, 1);

  PListView.Align := alClient;
  PSynMemo.Align := alClient;
  PQueryBuilder.Align := alClient;

  FServerListView.RowSelect := CheckWin32Version(6);
  SetWindowLong(ListView_GetHeader(FServerListView.Handle), GWL_STYLE, GetWindowLong(ListView_GetHeader(FServerListView.Handle), GWL_STYLE) or HDS_DRAGDROP);

  FSQLEditorSynMemo.Highlighter := MainHighlighter;
  FQueryBuilderSynMemo.Highlighter := MainHighlighter;


  Session.SyntaxProvider.AnsiQuotes := Session.Connection.AnsiQuotes;
  if (Session.LowerCaseTableNames <> 0) then
    Session.SyntaxProvider.IdentCaseSens := icsSensitiveLowerCase
  else
    Session.SyntaxProvider.IdentCaseSens := icsNonSensitive;
  Session.SyntaxProvider.ServerVersionInt := Session.Connection.MySQLVersion;
  Session.MetadataProvider.OnGetSQLFieldNames := MetadataProviderGetSQLFieldNames;

  FQueryBuilder.MetadataProvider := Session.MetadataProvider;
  FQueryBuilder.SyntaxProvider := Session.SyntaxProvider;


  FText.Modified := False;
  BMPImage := TBitmap.Create();
  GIFImage := TGIFImage.Create();
  PNGImage := TPNGImage.Create();
  JPEGImage := TJPEGImage.Create();

  if (not StyleServices.Enabled) then
    PObjectIDESpacer.Color := clBtnFace
  else
    PObjectIDESpacer.Color := clActiveBorder;

  NavigatorElapse := 0;
  MovingToAddress := False;

  LastFNavigatorSelected := nil;
  LastSelectedDatabase := '';
  LastSelectedTable := '';
  LastTableView := vObjects;
  LastObjectIDEAddress := '';


  OldFListOrderIndex := -1;

  Session.CreateDesktop := CreateDesktop;
  Session.RegisterEventProc(FormSessionEvent);

  Wanted := TWanted.Create(Self);

  FilterMRU := TPPreferences.TMRUList.Create(100);

  FOffset.Constraints.MaxWidth := FOffset.Width;

  Perform(UM_CHANGEPREFERENCES, 0, 0);
  Window.Perform(CM_SYSFONTCHANGED, 0, 0);

  NonClientMetrics.cbSize := SizeOf(NonClientMetrics);
  if (SystemParametersInfo(SPI_GETNONCLIENTMETRICS, SizeOf(NonClientMetrics), @NonClientMetrics, 0)) then
    Window.ApplyWinAPIUpdates(Self, NonClientMetrics.lfStatusFont);


  // Debug 2016-12-28
  SBlobDebug := SBlob;


  PostMessage(Handle, UM_POST_SHOW, 0, 0);
end;

procedure TFSession.CreateParams(var Params: TCreateParams);
begin
  inherited;

  Params.Style := Params.Style
    and not (WS_THICKFRAME or WS_SYSMENU or WS_DLGFRAME or WS_BORDER);
end;

procedure TFSession.CMSysFontChanged(var Message: TMessage);
var
  Color: TColor;
  LogFont: TLogFont;
  NonClientMetrics: TNonClientMetrics;
  R: TRect;
begin
  inherited;

  Font := Window.Font;

  ToolBar.AutoSize := False;
  ToolBar.ButtonHeight := Max(ToolBar.Images.Height + 2 * GetSystemMetrics(SM_CYFIXEDFRAME), ToolBar.Canvas.TextHeight('I') + 10);
  ToolBar.ButtonWidth := ToolBar.Images.Width + 2 * GetSystemMetrics(SM_CXFIXEDFRAME) + 1;
  ToolBar.AutoSize := True;

  TBSideBar.Left := 0;
  TBSideBar.Top := 0;
  TBSideBar.ButtonHeight := ToolBar.ButtonHeight;

  TBObjectSearch.ButtonHeight := Toolbar.ButtonHeight;
  TBObjectSearch.ButtonWidth := TBObjectSearch.Images.Width + 2 * GetSystemMetrics(SM_CXFIXEDFRAME) + 1;
  TBObjectSearch.Width := TBObjectSearch.ButtonCount * TBObjectSearch.ButtonWidth + 2 * 2 * TBObjectSearch.BorderWidth;

  if (Assigned(CloseButtonNormal)) then CloseButtonNormal.Free();
  if (Assigned(CloseButtonHot)) then CloseButtonHot.Free();
  if (Assigned(CloseButtonPushed)) then CloseButtonPushed.Free();

  R.Left := 0;
  R.Top := 0;
  R.Width := GetSystemMetrics(SM_CXSMICON) * 3 div 4;
  R.Height := R.Width;
  CloseButtonNormal := TPicture.Create();
  CloseButtonNormal.Bitmap.Width := R.Width;
  CloseButtonNormal.Bitmap.Height := R.Height;
  CloseButtonHot := TPicture.Create();
  CloseButtonHot.Bitmap.Width := R.Width;
  CloseButtonHot.Bitmap.Height := R.Height;
  CloseButtonPushed := TPicture.Create();
  CloseButtonPushed.Bitmap.Width := R.Width;
  CloseButtonPushed.Bitmap.Height := R.Height;

  if (StyleServices.Enabled) then
  begin
    StyleServices.DrawElement(CloseButtonNormal.Bitmap.Canvas.Handle, StyleServices.GetElementDetails(tbPushButtonNormal), R);
    StyleServices.DrawElement(CloseButtonHot.Bitmap.Canvas.Handle, StyleServices.GetElementDetails(tbPushButtonHot), R);
    StyleServices.DrawElement(CloseButtonPushed.Bitmap.Canvas.Handle, StyleServices.GetElementDetails(tbPushButtonPressed), R);

    R.Inflate(- 2 * GetSystemMetrics(SM_CXEDGE), - 2 * GetSystemMetrics(SM_CYEDGE));
    DrawCloseBitmap(CloseButtonNormal.Bitmap, R);
    DrawCloseBitmap(CloseButtonHot.Bitmap, R);
    R.Offset(GetSystemMetrics(SM_CXEDGE) div 2, GetSystemMetrics(SM_CYEDGE) div 2);
    DrawCloseBitmap(CloseButtonPushed.Bitmap, R);
  end
  else
  begin
    CloseButtonNormal.Bitmap.Canvas.Brush.Color := clBtnFace;
    CloseButtonNormal.Bitmap.Canvas.FillRect(R);
    CloseButtonHot.Bitmap.Canvas.Brush.Color := clBtnFace;
    CloseButtonHot.Bitmap.Canvas.FillRect(R);
    CloseButtonPushed.Bitmap.Canvas.Brush.Color := clBtnFace;
    CloseButtonPushed.Bitmap.Canvas.FillRect(R);

    DrawEdge(CloseButtonHot.Bitmap.Canvas.Handle, R, BDR_RAISEDINNER, BF_RECT);
    DrawEdge(CloseButtonPushed.Bitmap.Canvas.Handle, R, BDR_SUNKENOUTER, BF_RECT);
    R.Inflate(- GetSystemMetrics(SM_CXEDGE), - GetSystemMetrics(SM_CYEDGE));
    DrawCloseBitmap(CloseButtonNormal.Bitmap, R);
    DrawCloseBitmap(CloseButtonHot.Bitmap, R);
    R.Offset(GetSystemMetrics(SM_CXEDGE) div 2, GetSystemMetrics(SM_CYEDGE) div 2);
    DrawCloseBitmap(CloseButtonPushed.Bitmap, R);
  end;

  PHeader.ClientHeight := ToolBar.Height + PHeader.Canvas.Pen.Width;
  if (not StyleServices.Enabled or not StyleServices.GetElementColor(StyleServices.GetElementDetails(ttTopTabItemSelected), ecGlowColor, Color)) then
    PHeader.Color := clBtnFace
  else
    PHeader.Color := Color;

  if (not StyleServices.Enabled or not StyleServices.GetElementColor(StyleServices.GetElementDetails(tebNormalGroupHead),
    ecGradientColor2, Color)) then
  begin
    SplitColor := clBtnFace;
    SSideBar.Color := clBtnFace;
  end
  else
  begin
    SplitColor := Color;
    SSideBar.Color := clWindow;
    SSideBar.ActiveBorderColor := Color;
    SLog.ActiveBorderColor := Color;
    SExplorer.ActiveBorderColor := Color;
    SResult.ActiveBorderColor := Color;
    SBlob.ActiveBorderColor := Color;
    SQueryBuilderSynMemo.ActiveBorderColor := Color;
  end;

  SSideBar.Width := GetSystemMetrics(SM_CXFIXEDFRAME);
  PDataBrowserSpacer.Height := GetSystemMetrics(SM_CYFIXEDFRAME);
  PObjectIDESpacer.Height := GetSystemMetrics(SM_CYFIXEDFRAME);
  SQueryBuilderSynMemo.Height := GetSystemMetrics(SM_CYFIXEDFRAME);
  SResult.Height := GetSystemMetrics(SM_CYFIXEDFRAME);
  SBlob.Height := GetSystemMetrics(SM_CYFIXEDFRAME);
  PResultHeader.Width := CloseButtonNormal.Width + 2 * GetSystemMetrics(SM_CXEDGE);
  SLog.Height := GetSystemMetrics(SM_CYFIXEDFRAME);
  PLogHeader.Width := CloseButtonNormal.Width + 2 * GetSystemMetrics(SM_CXEDGE);

  FormResize(nil);

  PDataBrowserSpacer.Top := FFilter.Height;
  PDataBrowser.ClientHeight := FFilter.Height + PDataBrowserSpacer.Height;

  FSQLEditorSynMemo.Font.Name := Preferences.SQLFontName;
  FSQLEditorSynMemo.Font.Style := Preferences.SQLFontStyle;
  FSQLEditorSynMemo.Font.Color := Preferences.SQLFontColor;
  FSQLEditorSynMemo.Font.Size := Preferences.SQLFontSize;
  FSQLEditorSynMemo.Font.Charset := Preferences.SQLFontCharset;
  FSQLEditorSynMemo.Gutter.Font := FSQLEditorSynMemo.Font;
  if (Preferences.Editor.LineNumbersForeground = clNone) then
    FSQLEditorSynMemo.Gutter.Font.Color := clWindowText
  else
    FSQLEditorSynMemo.Gutter.Font.Color := Preferences.Editor.LineNumbersForeground;

  SynCompletion.Font := FSQLEditorSynMemo.Font;
  SynCompletion.Width := Round(260 * Screen.PixelsPerInch / USER_DEFAULT_SCREEN_DPI);

  FText.Font.Name := Preferences.GridFontName;
  FText.Font.Style := Preferences.GridFontStyle;
  FText.Font.Color := Preferences.GridFontColor;
  FText.Font.Size := Preferences.GridFontSize;
  FText.Font.Charset := Preferences.GridFontCharset;
  NonClientMetrics.cbSize := SizeOf(NonClientMetrics);
  if (SystemParametersInfo(SPI_GETNONCLIENTMETRICS, SizeOf(NonClientMetrics), @NonClientMetrics, 0)
    and (GetObject(FText.Font.Handle, SizeOf(LogFont), @LogFont) <> 0)) then
  begin
    LogFont.lfQuality  := NonClientMetrics.lfMessageFont.lfQuality;
    FText.Font.Handle := CreateFontIndirect(LogFont);
  end;

  FRTF.Font := FText.Font;

  FHexEditor.Font := FSQLEditorSynMemo.Font;
  if (Preferences.Editor.LineNumbersForeground = clNone) then
    FHexEditor.Colors.Offset := clWindowText
  else
    FHexEditor.Colors.Offset := Preferences.Editor.LineNumbersForeground;
  if (Preferences.Editor.LineNumbersBackground = clNone) then
    FHexEditor.Colors.OffsetBackground := clBtnFace
  else
    FHexEditor.Colors.OffsetBackground := Preferences.Editor.LineNumbersBackground;

  PQueryBuilderSynMemo.Constraints.MinHeight :=
    (FQueryBuilderSynMemo.Canvas.TextHeight('SELECT') + 1) + 2 * FQueryBuilderSynMemo.Top + 2 * BevelWidth
    + GetSystemMetrics(SM_CYHSCROLL);

  SendMessage(FLog.Handle, EM_SETTEXTMODE, TM_PLAINTEXT, 0);
  SendMessage(FLog.Handle, EM_SETWORDBREAKPROC, 0, LPARAM(@EditWordBreakProc));
end;

function TFSession.CreateDesktop(const CObject: TSObject): TSObject.TDesktop;
begin
  if (CObject is TSDatabase) then
    Result := TDatabaseDesktop.Create(Self, TSDatabase(CObject))
  else if (CObject is TSBaseTable) then
    Result := TTableDesktop.Create(Self, TSBaseTable(CObject))
  else if (CObject is TSView) then
    Result := TViewDesktop.Create(Self, TSView(CObject))
  else if (CObject is TSSystemView) then
    Result := TTableDesktop.Create(Self, TSSystemView(CObject))
  else if (CObject is TSRoutine) then
    Result := TRoutineDesktop.Create(Self, TSRoutine(CObject))
  else if (CObject is TSEvent) then
    Result := TEventDesktop.Create(Self, TSEvent(CObject))
  else if (CObject is TSTrigger) then
    Result := TTriggerDesktop.Create(Self, TSTrigger(CObject))
  else
    Result := nil;
end;

procedure TFSession.CreateExplorer();
begin
  if (not Assigned(ShellLink)) then
    ShellLink := TJamShellLink.Create(Owner);

  if (not Assigned(FFolders)) then
  begin
    FFolders := TJamShellTree.Create(Owner);
    FFolders.Parent := PFolders;
    FFolders.Visible := False;
    FFolders.Left := 0;
    FFolders.Top := 0;
    FFolders.Width := PFolders.ClientWidth;
    FFolders.Height := PFolders.ClientHeight;
    FFolders.Align := alClient;
    FFolders.HelpType := htContext;
    FFolders.HelpContext := 1108;
    FFolders.HideSelection := False;
    FFolders.HotTrack := True;
    FFolders.ShellLink := ShellLink;
    FFolders.BorderStyle := bsNone;
    FFolders.ParentFont := True;
    FFolders.ShowLines := False;
    FFolders.ShowNetHood := False;
    FFolders.ShowRecycleBin := False;
    FFolders.Visible := True;
    FFolders.OnChange := FFoldersChange;

    if ((ComCtl32MajorVersion > 4) or (ComCtl32MinorVersion >= 71)) then
      SendMessage(FFolders.Handle, TVM_SETITEMHEIGHT, GetSystemMetrics(SM_CYSMICON) + 2 * GetSystemMetrics(SM_CYEDGE), 0);
    if (CheckWin32Version(6)) then
    begin
      FFolders.Indent := GetSystemMetrics(SM_CXSMICON) div 2 + GetSystemMetrics(SM_CXEDGE);
      SetWindowLong(FFolders.Handle, GWL_STYLE, GetWindowLong(FFolders.Handle, GWL_STYLE) or TVS_NOHSCROLL);
      SendMessage(FFolders.Handle, TVM_SETEXTENDEDSTYLE, TVS_EX_AUTOHSCROLL or TVS_EX_FADEINOUTEXPANDOS or TVS_EX_DOUBLEBUFFER, TVS_EX_AUTOHSCROLL or TVS_EX_FADEINOUTEXPANDOS or TVS_EX_DOUBLEBUFFER);
    end;
    if (CheckWin32Version(6, 1)) then
      SetWindowLong(FFolders.Handle, GWL_STYLE, GetWindowLong(FFolders.Handle, GWL_STYLE) or TVS_FULLROWSELECT);
  end;

  if (not Assigned(FFiles)) then
  begin
    FFiles := TJamShellList.Create(Owner);
    FFiles.Parent := PFiles;
    FFiles.Left := 0;
    FFiles.Top := 0;
    FFiles.Width := PFiles.Width;
    FFiles.Height := PFiles.Height;
    FFiles.Align := alClient;
    FFiles.BorderStyle := bsNone;
    FFiles.Filter := Session.Account.Desktop.FilesFilter;
    FFiles.HelpType := htContext;
    FFiles.HelpContext := 1108;
    FFiles.HideSelection := False;
    FFiles.IconOptions.AutoArrange := True;
    FFiles.PopupMenu := MFiles;
    FFiles.BackgroundPopupMenu := MFiles;
    FFiles.ParentFont := True;
    FFiles.RowSelect := True;
    FFiles.ShellContextMenu := False;
    FFiles.ShowFolders := False;
    FFiles.ShowRecycleBin := False;
    FFiles.ShellLink := ShellLink;
    FFiles.OnDblClick := ListViewDblClick;
    FFiles.OnKeyDown := ListViewKeyDown;
    FFiles.OnEnter := FFilesEnter;

    if (CheckWin32Version(6,1)) then
      SendMessage(FFiles.Handle, LVM_SETEXTENDEDLISTVIEWSTYLE, LVS_EX_JUSTIFYCOLUMNS, 0);
    SendMessage(FFiles.Handle, LVM_SETEXTENDEDLISTVIEWSTYLE, LVS_EX_DOUBLEBUFFER, LVS_EX_DOUBLEBUFFER);
    SendMessage(FFiles.Handle, LVM_SETEXTENDEDLISTVIEWSTYLE, LVS_EX_COLUMNSNAPPOINTS, LVS_EX_COLUMNSNAPPOINTS);
  end;

  FFolders.SelectedFolder := Path;
end;

function TFSession.CreateDBGrid(const PDBGrid: TPanel_Ext; const DataSource: TDataSource): TMySQLDBGrid;
var
  I: Integer;
  LogFont: TLogFont;
  NonClientMetrics: TNonClientMetrics;
begin
  Result := TMySQLDBGrid.Create(Owner);
  Result.BorderStyle := bsNone;

  Result.Left := 0;
  Result.Top := 0;
  Result.Width := PDBGrid.ClientWidth;
  Result.Height := PDBGrid.ClientHeight;
  Result.Align := alClient;
  Result.Constraints.MinHeight := 30;
  Result.DataSource := FGridDataSource;
  Result.DefaultDrawing := False;
  Result.HelpType := htContext;
  Result.HelpContext := 1036;
  Result.Options := [dgTitles, dgColumnResize, dgColLines, dgRowLines, dgTabs, dgAlwaysShowSelection, dgConfirmDelete, dgMultiSelect, dgTitleClick, dgTitleHotTrack];
  Result.ParentFont := False;
  Result.Font.Name := Preferences.GridFontName;
  Result.Font.Style := Preferences.GridFontStyle;
  Result.Font.Color := Preferences.GridFontColor;
  Result.Font.Size := Preferences.GridFontSize;
  Result.Font.Charset := Preferences.GridFontCharset;
  NonClientMetrics.cbSize := SizeOf(NonClientMetrics);
  if (SystemParametersInfo(SPI_GETNONCLIENTMETRICS, SizeOf(NonClientMetrics), @NonClientMetrics, 0)
    and (GetObject(Result.Font.Handle, SizeOf(LogFont), @LogFont) <> 0)) then
  begin
    LogFont.lfQuality  := NonClientMetrics.lfMessageFont.lfQuality;
    Result.Font.Handle := CreateFontIndirect(LogFont);
  end;
  Result.TitleFont.Name :=  Result.Font.Name;
  Result.TitleFont.Style := Result.Font.Style;
  Result.TitleFont.Color := Result.Font.Color;
  Result.TitleFont.Size := Result.Font.Size;
  Result.TitleFont.Charset := Result.Font.Charset;
  Result.Canvas.Font := Result.Font;
  for I := 0 to Result.Columns.Count - 1 do
    Result.Columns[I].Font := Result.Font;
  Result.PopupMenu := MGrid;
  Result.TabOrder := 0;
  Result.OnCanEditShow := Session.GridCanEditShow;
  Result.OnCellClick := DBGridCellEnter;
  Result.OnColEnter := DBGridColEnter;
  Result.OnColExit := DBGridColExit;
  Result.OnDrawColumnCell := DBGridDrawColumnCell;
  Result.OnDblClick := DBGridDblClick;
  Result.OnEnter := DBGridEnter;
  Result.OnExit := DBGridExit;
  Result.OnKeyDown := DBGridKeyDown;
  Result.OnMouseMove := DBGridMouseMove;
  Result.OnTitleClick := DBGridTitleClick;

  Result.DataSource := DataSource;

try
  Result.Parent := PDBGrid;
except
  on E: Exception do
    raise Exception.Create(E.Message + ' (Result.Parent := PDBGrid)');
end;

  Result.Perform(CM_PARENTCOLORCHANGED, 0, 0);
  Result.Perform(CM_PARENTFONTCHANGED, 0, 0);
  Result.Perform(CM_PARENTSHOWHINTCHANGED, 0, 0);
  Result.Perform(CM_PARENTBIDIMODECHANGED, 0, 0);
  Result.Perform(CM_PARENTDOUBLEBUFFEREDCHANGED, 0, 0);
  Result.Perform(CM_PARENTTABLETOPTIONSCHANGED, 0, 0);

  NonClientMetrics.cbSize := SizeOf(NonClientMetrics);
  if (SystemParametersInfo(SPI_GETNONCLIENTMETRICS, SizeOf(NonClientMetrics), @NonClientMetrics, 0)) then
    Window.ApplyWinAPIUpdates(Result, NonClientMetrics.lfStatusFont);
end;

function TFSession.CreateListView(const Data: TCustomData): TListView;
var
  NonClientMetrics: TNonClientMetrics;
begin
  Result := TListView.Create(FServerListView.Owner);

  Result.Left := 0;
  Result.Top := 0;
  Result.Width := PListView.ClientWidth;
  Result.Height := PListView.ClientHeight;
  Result.Align := alClient;
  Result.BorderStyle := FServerListView.BorderStyle;
  Result.DragMode := FServerListView.DragMode;
  Result.HelpType := htContext;
  Result.HelpContext := FServerListView.HelpContext;
  Result.HideSelection := FServerListView.HideSelection;
  Result.MultiSelect := FServerListView.MultiSelect;
  Result.GroupView := FServerListView.GroupView;
  Result.PopupMenu := FServerListView.PopupMenu;
  Result.RowSelect := FServerListView.RowSelect;
  Result.SmallImages := Preferences.Images;
  Result.ViewStyle := FServerListView.ViewStyle;
  Result.Visible := False;
  if (TObject(Data) is TSTable) then
  begin
    Result.OnAdvancedCustomDrawItem := ListViewAdvancedCustomDrawItem;
    Result.OnAdvancedCustomDrawSubItem := ListViewAdvancedCustomDrawSubItem;
  end;

  Result.Parent := FServerListView.Parent;

  Result.OnChange := FServerListView.OnChange;
  Result.OnChanging := FServerListView.OnChanging;
  Result.OnColumnClick := FServerListView.OnColumnClick;
  Result.OnCompare := FServerListView.OnCompare;
  Result.OnDblClick := FServerListView.OnDblClick;
  Result.OnEdited := FServerListView.OnEdited;
  Result.OnEditing := FServerListView.OnEditing;
  Result.OnEnter := FServerListView.OnEnter;
  Result.OnExit := FServerListView.OnExit;
  Result.OnDragDrop := FServerListView.OnDragDrop;
  Result.OnDragOver := FServerListView.OnDragOver;
  Result.OnKeyDown := FServerListView.OnKeyDown;
  Result.OnSelectItem := FServerListView.OnSelectItem;

  Result.Tag := NativeInt(Data);

  SetWindowLong(ListView_GetHeader(Result.Handle), GWL_STYLE, GetWindowLong(ListView_GetHeader(FServerListView.Handle), GWL_STYLE) or HDS_DRAGDROP);

  Result.Parent := PListView;

  Result.Perform(CM_PARENTCOLORCHANGED, 0, 0);
  Result.Perform(CM_PARENTFONTCHANGED, 0, 0);
  Result.Perform(CM_PARENTSHOWHINTCHANGED, 0, 0);
  Result.Perform(CM_PARENTBIDIMODECHANGED, 0, 0);
  Result.Perform(CM_PARENTDOUBLEBUFFEREDCHANGED, 0, 0);
  Result.Perform(CM_PARENTTABLETOPTIONSCHANGED, 0, 0);

  NonClientMetrics.cbSize := SizeOf(NonClientMetrics);
  if (SystemParametersInfo(SPI_GETNONCLIENTMETRICS, SizeOf(NonClientMetrics), @NonClientMetrics, 0)) then
    Window.ApplyWinAPIUpdates(Result, NonClientMetrics.lfStatusFont);

  ListViewInitialize(Result);
end;

function TFSession.CreatePDBGrid(): TPanel_Ext;
var
  NonClientMetrics: TNonClientMetrics;
begin
  Result := TPanel_Ext.Create(Owner);

  Result.Left := PSQLEditorDBGrid.Left;
  Result.Top := PSQLEditorDBGrid.Top;
  Result.Width := PSQLEditorDBGrid.Width;
  Result.Height := PSQLEditorDBGrid.Height;
  Result.Align := PSQLEditorDBGrid.Align;
  Result.BevelInner := PSQLEditorDBGrid.BevelInner;
  Result.BevelOuter := PSQLEditorDBGrid.BevelOuter;
  Result.Color := PSQLEditorDBGrid.Color;
  Result.Constraints.MinHeight := PSQLEditorDBGrid.Constraints.MinHeight;
  Result.ParentBackground := PSQLEditorDBGrid.ParentBackground;
  Result.OnResize := PSQLEditorDBGrid.OnResize;

  Result.Parent := PSQLEditorDBGrid.Parent;

  Result.Perform(CM_PARENTCOLORCHANGED, 0, 0);
  Result.Perform(CM_PARENTFONTCHANGED, 0, 0);
  Result.Perform(CM_PARENTSHOWHINTCHANGED, 0, 0);
  Result.Perform(CM_PARENTBIDIMODECHANGED, 0, 0);
  Result.Perform(CM_PARENTDOUBLEBUFFEREDCHANGED, 0, 0);
  Result.Perform(CM_PARENTTABLETOPTIONSCHANGED, 0, 0);

  NonClientMetrics.cbSize := SizeOf(NonClientMetrics);
  if (SystemParametersInfo(SPI_GETNONCLIENTMETRICS, SizeOf(NonClientMetrics), @NonClientMetrics, 0)) then
    Window.ApplyWinAPIUpdates(Result, NonClientMetrics.lfStatusFont);
end;

function TFSession.CreateSynMemo(SObject: TSObject): TSynMemo;
var
  NonClientMetrics: TNonClientMetrics;
begin
  Result := TSynMemo.Create(FSQLEditorSynMemo.Owner);

  Result.Left := 0;
  Result.Top := 0;
  Result.Width := PSynMemo.ClientWidth;
  Result.Height := PSynMemo.ClientHeight;
  Result.Align := alClient;
  Result.BorderStyle := FSQLEditorSynMemo.BorderStyle;
  Result.HelpType := htContext;
  Result.HelpContext := FSQLEditorSynMemo.HelpContext;
  Result.Highlighter := FSQLEditorSynMemo.Highlighter;
  Result.Gutter.AutoSize := FSQLEditorSynMemo.Gutter.AutoSize;
  Result.Gutter.Color := FSQLEditorSynMemo.Gutter.Color;
  Result.Gutter.DigitCount := FSQLEditorSynMemo.Gutter.DigitCount;
  Result.Gutter.LeftOffset := FSQLEditorSynMemo.Gutter.LeftOffset;
  Result.Gutter.ShowLineNumbers := FSQLEditorSynMemo.Gutter.ShowLineNumbers;
  Result.Keystrokes := FSQLEditorSynMemo.Keystrokes;
  Result.MaxScrollWidth := FSQLEditorSynMemo.MaxScrollWidth;
  Result.OnChange := FSQLEditorSynMemo.OnChange;
  Result.OnDragDrop := FSQLEditorSynMemo.OnDragDrop;
  Result.OnDragOver := FSQLEditorSynMemo.OnDragOver;
  Result.OnEnter := FSQLEditorSynMemo.OnEnter;
  Result.OnExit := FSQLEditorSynMemo.OnExit;
  Result.OnSearchNotFound := FSQLEditorSynMemo.OnSearchNotFound;
  Result.OnStatusChange := FSQLEditorSynMemo.OnStatusChange;
  Result.PopupMenu := FSQLEditorSynMemo.PopupMenu;
  Result.RightEdge := FSQLEditorSynMemo.RightEdge;
  Result.ScrollHintFormat := FSQLEditorSynMemo.ScrollHintFormat;
  Result.SearchEngine := FSQLEditorSynMemo.SearchEngine;
  Result.Tag := NativeInt(SObject);

  Result.Parent := PSynMemo;

  SynMemoApplyPreferences(Result);
  SynCompletion.AddEditor(Result);

  Result.Perform(CM_PARENTCOLORCHANGED, 0, 0);
  Result.Perform(CM_PARENTFONTCHANGED, 0, 0);
  Result.Perform(CM_PARENTSHOWHINTCHANGED, 0, 0);
  Result.Perform(CM_PARENTBIDIMODECHANGED, 0, 0);
  Result.Perform(CM_PARENTDOUBLEBUFFEREDCHANGED, 0, 0);
  Result.Perform(CM_PARENTTABLETOPTIONSCHANGED, 0, 0);

  NonClientMetrics.cbSize := SizeOf(NonClientMetrics);
  if (SystemParametersInfo(SPI_GETNONCLIENTMETRICS, SizeOf(NonClientMetrics), @NonClientMetrics, 0)) then
    Window.ApplyWinAPIUpdates(Result, NonClientMetrics.lfStatusFont);
end;

function TFSession.CreateTCResult(const PDBGrid: TPanel_Ext): TTabControl;
var
  NonClientMetrics: TNonClientMetrics;
begin
  Result := TTabControl.Create(Owner);

  Result.Left := 0;
  Result.Top := 0;
  Result.Width := PDBGrid.ClientWidth;
  Result.Align := alTop;

  Result.Parent := PDBGrid;

  Result.Perform(CM_PARENTCOLORCHANGED, 0, 0);
  Result.Perform(CM_PARENTFONTCHANGED, 0, 0);
  Result.ParentShowHint := False;
  Result.Perform(CM_PARENTBIDIMODECHANGED, 0, 0);
  Result.Perform(CM_PARENTDOUBLEBUFFEREDCHANGED, 0, 0);
  Result.Perform(CM_PARENTTABLETOPTIONSCHANGED, 0, 0);

  NonClientMetrics.cbSize := SizeOf(NonClientMetrics);
  if (SystemParametersInfo(SPI_GETNONCLIENTMETRICS, SizeOf(NonClientMetrics), @NonClientMetrics, 0)) then
    Window.ApplyWinAPIUpdates(Result, NonClientMetrics.lfStatusFont);
end;

function TFSession.CreateWorkbench(const ADatabase: TSDatabase): TWWorkbench;
begin
  Result := TWWorkbench.Create(Owner, ADatabase);

  Result.Left := 0;
  Result.Top := 0;
  Result.Width := PWorkbench.ClientWidth;
  Result.Height := PWorkbench.ClientHeight;
  Result.Align := alClient;
  Result.BorderStyle := bsNone;
  Result.HelpType := htContext;
  Result.HelpContext := 1125;
  Result.HideSelection := True;
  Result.HorzScrollBar.Tracking := True;
  Result.MultiSelect := True;
  Result.OnChange := WorkbenchChange;
  Result.OnDblClick := ListViewDblClick;
  Result.OnDragOver := WorkbenchDragOver;
  Result.OnDragDrop := WorkbenchDragDrop;
  Result.OnEnter := WorkbenchEnter;
  Result.OnExit := WorkbenchExit;
  Result.OnCursorMove := WorkbenchCursorMove;
  Result.OnValidateControl := WorkbenchValidateControl;
  Result.ParentFont := True;
  Result.PopupMenu := MWorkbench;
  Result.VertScrollBar.Tracking := True;

  Result.Parent := PWorkbench;

  Result.Perform(CM_PARENTCOLORCHANGED, 0, 0);
  Result.Perform(CM_PARENTFONTCHANGED, 0, 0);
  Result.Perform(CM_PARENTSHOWHINTCHANGED, 0, 0);
  Result.Perform(CM_PARENTBIDIMODECHANGED, 0, 0);
  Result.Perform(CM_PARENTDOUBLEBUFFEREDCHANGED, 0, 0);
  Result.Perform(CM_PARENTTABLETOPTIONSCHANGED, 0, 0);
end;

function TFSession.Desktop(const Database: TSDatabase): TDatabaseDesktop;
begin
  Result := TDatabaseDesktop(Database.Desktop);
end;

function TFSession.Desktop(const Event: TSEvent): TEventDesktop;
begin
  Result := TEventDesktop(Event.Desktop);
end;

function TFSession.Desktop(const Routine: TSRoutine): TRoutineDesktop;
begin
  Result := TRoutineDesktop(Routine.Desktop);
end;

function TFSession.Desktop(const Table: TSTable): TTableDesktop;
begin
  Result := TTableDesktop(Table.Desktop);
end;

function TFSession.Desktop(const Trigger: TSTrigger): TTriggerDesktop;
begin
  Result := TTriggerDesktop(Trigger.Desktop);
end;

function TFSession.Desktop(const View: TSView): TViewDesktop;
begin
  Result := TViewDesktop(View.Desktop);
end;

procedure TFSession.DataSetAfterCancel(DataSet: TDataSet);
begin
  DBGridColEnter(ActiveDBGrid);
end;

procedure TFSession.DataSetAfterClose(DataSet: TDataSet);
begin
  if (not (csDestroying in ComponentState)) then
  begin
    PBlob.Visible := False;
    SBlob.Visible := PBlob.Visible;
    if (PResult.Align = alClient) then
    begin
      PResult.Align := alBottom;
      PResult.Height := PResultHeight;
      if (PQueryBuilder.Visible) then PQueryBuilder.Align := alClient;
      if (PSynMemo.Visible) then PSynMemo.Align := alClient;
    end;

    PResult.Visible := False; SResult.Visible := False;
    PQueryBuilder.Update(); // TSynMemo does not update immediately after a change of TSynMemo.Align
    PSynMemo.Update(); // TSynMemo does not update immediately after a change of TSynMemo.Align
  end;

  aDPrev.Enabled := False;
  aDNext.Enabled := False;

  MainAction('aFExportSQL').Enabled := False;
  MainAction('aFExportText').Enabled := False;
  MainAction('aFExportExcel').Enabled := False;
  MainAction('aFExportXML').Enabled := False;
  MainAction('aFExportHTML').Enabled := False;
  MainAction('aFExportPDF').Enabled := False;
end;

procedure TFSession.DataSetAfterOpen(const DBGrid: TMySQLDBGrid; const DataSet: TDataSet);
begin
  PContentChange(nil);

  DBGrid.DataSource.Enabled := False;
  DBGrid.DataSource.DataSet := DataSet;
  DBGridInitialize(DBGrid);
end;

procedure TFSession.DataSetAfterPost(DataSet: TDataSet);
begin
  DataSetAfterScroll(DataSet);

  StatusBarRefresh();
end;

procedure TFSession.DataSetAfterScroll(DataSet: TDataSet);
var
  InputDataSet: Boolean;
  DBGrid: TMySQLDBGrid;
begin
  if (not DataSet.ControlsDisabled) then
  begin
    InputDataSet := (DataSet is TMySQLDataSet) and TMySQLDataSet(DataSet).CachedUpdates;

    if (((Window.ActiveControl = ActiveDBGrid) or (Window.ActiveControl = FText) or (Window.ActiveControl = FRTF) or (Window.ActiveControl = FHexEditor)) and (TMySQLQuery(DataSet).FieldCount > 0)) then
      DBGrid := ActiveDBGrid
    else if (FObjectIDEGrid.DataSource.DataSet = DataSet) then
      DBGrid := FObjectIDEGrid
    else
      DBGrid := nil;

    if (Assigned(DBGrid)) then
      DBGridColEnter(DBGrid);

    aDPrev.Enabled := not DataSet.Bof and not InputDataSet;
    aDNext.Enabled := not DataSet.Eof and not InputDataSet;
    MainAction('aDInsertRecord').Enabled := (Window.ActiveControl = ActiveDBGrid) and aDInsertRecord.Enabled and (DataSet.State in [dsBrowse, dsEdit]) and (DataSet.FieldCount > 0) and Assigned(ActiveDBGrid) and (ActiveDBGrid.SelectedRows.Count < 1) and not InputDataSet;
    MainAction('aDDeleteRecord').Enabled := (Window.ActiveControl = ActiveDBGrid) and aDDeleteRecord.Enabled and (DataSet.State in [dsBrowse, dsEdit]) and not DataSet.IsEmpty() and not InputDataSet;

    // <Ctrl+Down> marks the new row as selected, but the OnAfterScroll event
    // will be executed BEFORE mark the row as selected.
    PostMessage(Handle, UM_STATUS_BAR_REFRESH, 0, 0);
  end;
end;

procedure TFSession.DataSetBeforeCancel(DataSet: TDataSet);
begin
  if (PBlob.Visible) then
  begin
    DBGridColEnter(ActiveDBGrid);
    if (PResult.Visible and Assigned(ActiveDBGrid)) then
      Window.ActiveControl := ActiveDBGrid;
  end;
end;

procedure TFSession.DataSetBeforePost(DataSet: TDataSet);
begin
  if (PBlob.Visible and aVBlobText.Checked) then
    FTextExit(DataSetPost);
end;

procedure TFSession.DBGridCellEnter(Column: TColumn);
begin
  StatusBarRefresh();
end;

procedure TFSession.DBGridColEnter(Sender: TObject);
var
  DBGrid: TMySQLDBGrid;
begin
  if (Sender is TMySQLDBGrid) then
  begin
    DBGrid := TMySQLDBGrid(Sender);
    if (not Assigned(DBGrid.SelectedField)) then
      DBGrid.SelectedField := DBGrid.Fields[0];

    if ((((Window.ActiveControl = DBGrid) or (Window.ActiveControl = FText) or (Window.ActiveControl = FRTF) or (Window.ActiveControl = FHexEditor)) and Assigned(DBGrid.SelectedField)) or (Sender = DataSetCancel)) then
    begin
      FText.OnChange := nil;

      if (Assigned(EditorField) xor PBlob.Visible) then
      begin
        PostMessage(DBGrid.Handle, WM_SIZE, SIZE_MAXIMIZED, DBGrid.Height shl 16 + DBGrid.Width);
        PContentChange(DBGrid);
      end;

      if (Assigned(EditorField)) then
      begin
        if (EditorField.DataType = ftBlob) then
          FImageShow(Sender);

        aVBlobText.Visible := not GeometryField(EditorField) and not IsPDF(EditorField.AsString) and ((EditorField.DataType = ftWideMemo) or not Assigned(FImage.Picture.Graphic));
        aVBlobRTF.Visible := aVBlobText.Visible and (EditorField.DataType = ftWideMemo) and not EditorField.IsNull and IsRTF(EditorField.AsString);
        aVBlobHTML.Visible := aVBlobText.Visible and (EditorField.DataType = ftWideMemo) and not EditorField.IsNull and IsHTML(EditorField.AsString) or IsPDF(EditorField.AsString);
        aVBlobImage.Visible := (EditorField.DataType = ftBlob) and Assigned(FImage.Picture.Graphic);
        PToolBarBlobResize(Sender);

        if (aVBlobHTML.Visible) then
          if (not IsPDF(EditorField.AsString)) then
            aVBlobHTML.Caption := 'HTML'
          else
            aVBlobHTML.Caption := 'PDF';

        if (aVBlobText.Visible and aVBlobText.Checked) then
          aVBlobExecute(nil)
        else if (aVBlobRTF.Visible and aVBlobRTF.Checked) then
          aVBlobExecute(nil)
        else if (aVBlobHTML.Visible and aVBlobHTML.Checked) then
          aVBlobExecute(nil)
        else if (aVBlobImage.Visible and aVBlobImage.Checked) then
          aVBlobExecute(nil)
        else if (aVBlobHexEditor.Visible and aVBlobHexEditor.Checked) then
          aVBlobExecute(nil)
        else if (aVBlobText.Visible) then
          aVBlobText.Execute()
        else if (aVBlobRTF.Visible) then
          aVBlobRTF.Execute()
        else if (aVBlobHTML.Visible) then
          aVBlobHTML.Execute()
        else if (aVBlobImage.Visible) then
          aVBlobImage.Execute()
        else
          aVBlobHexEditor.Execute();
      end;

      if ((DBGrid.SelectedField.DataType in [ftWideMemo, ftBlob]) or (DBGrid.SelectedField.DataType in [ftUnknown])) then
        DBGrid.Options := DBGrid.Options - [dgEditing]
      else
        DBGrid.Options := DBGrid.Options + [dgEditing];

      FText.OnChange := FTextChange;
    end;

    DBGrid.UpdateAction(MainAction('aEPaste'));
    MainAction('aECopyToFile').Enabled := Assigned(DBGrid.SelectedField) and (DBGrid.SelectedField.DataType in [ftWideMemo, ftBlob]) and (not DBGrid.SelectedField.IsNull) and (DBGrid.SelectedRows.Count <= 1);
    MainAction('aEPasteFromFile').Enabled := Assigned(DBGrid.SelectedField) and (DBGrid.SelectedField.DataType in [ftWideMemo, ftBlob]) and not DBGrid.SelectedField.ReadOnly and (DBGrid.SelectedRows.Count <= 1);
    MainAction('aDCreateField').Enabled := Assigned(DBGrid.SelectedField) and (View = vBrowser);
    MainAction('aDEditRecord').Enabled := Assigned(DBGrid.SelectedField) and (View <> vIDE);
    MainAction('aDEmpty').Enabled := (DBGrid.DataSource.DataSet.CanModify and Assigned(DBGrid.SelectedField) and not DBGrid.SelectedField.IsNull and not DBGrid.SelectedField.Required and (DBGrid.SelectedRows.Count <= 1));
  end;

  StatusBarRefresh();
end;

procedure TFSession.DBGridColExit(Sender: TObject);
var
  Trigger: TSTrigger;
begin
  MainAction('aECopyToFile').Enabled := False;
  MainAction('aEPasteFromFile').Enabled := False;
  MainAction('aDCreateField').Enabled := False;
  MainAction('aDEditRecord').Enabled := False;
  MainAction('aDEmpty').Enabled := False;

  if ((Sender = FObjectIDEGrid) and (SelectedImageIndex = iiTrigger) and (TMySQLDBGrid(Sender).DataSource.DataSet is TMySQLDataSet)) then
  begin
    Trigger := TSTrigger(FNavigator.Selected.Data);

    BINSERT.Enabled := Trigger.SQLInsert() <> '';
    BREPLACE.Enabled := Trigger.SQLReplace() <> '';
    BUPDATE.Enabled := Trigger.SQLUpdate() <> '';
    BDELETE.Enabled := Trigger.SQLDelete() <> '';
  end;

  gmFilter.Clear();
end;

procedure TFSession.DBGridDrawColumnCell(Sender: TObject; const Rect: TRect;
  DataCol: Integer; Column: TColumn; State: TGridDrawState);
var
  BGRect: TRect;
  DBGrid: TMySQLDBGrid;
  Text: string;
  TextRect: TRect;
begin
  if (Sender is TMySQLDBGrid) then
  begin
    DBGrid := TMySQLDBGrid(Sender);

    if (not Assigned(Column.Field)) then
      Text := ''
    else if (Column.Field.IsNull) then
      if (not Column.Field.Required and Preferences.GridNullText) then
        Text := '<NULL>'
      else
        Text := ''
    else if (GeometryField(Column.Field)) then
      Text := '<GEO>'
    else if (Column.Field.DataType = ftBlob) then
      Text := '<BLOB>'
    else if (Column.Field.DataType = ftWideMemo) then
      if (not Preferences.GridMemoContent) then
        Text := '<MEMO>'
      else
        Text := Copy(Column.Field.AsString, 1, 1000)
    else
      Text := Column.Field.AsString;

    TextRect := Rect;
    InflateRect(TextRect, -2, -2);
    if (Column.Alignment = taRightJustify) then
      TextRect.Left := Max(TextRect.Left, TextRect.Right - DBGrid.Canvas.Textwidth(Text));

    if (DBGrid.Focused and DBGrid.SelectedRows.CurrentRowSelected) then
    begin // Row is selected, Grid is focused
      DBGrid.Canvas.Font.Color := clHighlightText;
      DBGrid.Canvas.Brush.Color := clHighlight;
    end
    else if (not DBGrid.Focused and DBGrid.SelectedRows.CurrentRowSelected) then
    begin // Row is selected, Grid is NOT focused
      DBGrid.Canvas.Font.Color := clBtnText;
      DBGrid.Canvas.Brush.Color := clBtnFace;
    end
    else if (gdFocused in State) then
    begin // Cell is focused, Grid is focused
      DBGrid.Canvas.Font.Color := clHighlightText;
      DBGrid.Canvas.Brush.Color := clHighlight;

      DBGrid.Canvas.Pen.Style := psDot;
      DBGrid.Canvas.Pen.Mode := pmNotCopy;
      DBGrid.Canvas.Rectangle(Rect);

      BGRect := Rect; InflateRect(BGRect, -1, -1);
      DBGrid.Canvas.Pen.Style := psSolid;
      DBGrid.Canvas.Pen.Mode := pmCopy;
    end
    else if (not DBGrid.Focused and (Column.Field = DBGrid.SelectedField) and DBGrid.CurrentRow and not DBGrid.Focused and (dgAlwaysShowSelection in DBGrid.Options)) then
    begin // Cell is focused, Grid is NOT focused
      DBGrid.Canvas.Font.Color := clBtnText;
      DBGrid.Canvas.Brush.Color := clBtnFace;
    end
    else if ((DBGrid.Parent <> PObjectIDE) and Preferences.GridCurrRowBGColorEnabled and DBGrid.CurrentRow) then
    begin // Row is focused
      DBGrid.Canvas.Font.Color := clWindowText;
      DBGrid.Canvas.Brush.Color := ColorToRGB(Preferences.GridCurrRowBGColor)
    end
    else if ((DBGrid.Parent <> PObjectIDE) and Preferences.GridNullBGColorEnabled and Assigned(Column.Field) and Column.Field.IsNull and not Column.Field.Required) then
    begin // Cell is NULL
      DBGrid.Canvas.Font.Color := clGrayText;
      DBGrid.Canvas.Brush.Color := Preferences.GridNullBGColor;
    end
    else
    begin
      DBGrid.Canvas.Font.Color := clWindowText;
      DBGrid.Canvas.Brush.Color := clWindow;
    end;

    if (Assigned(Column.Field) and Column.Field.IsNull) then
      DBGrid.Canvas.Font.Color := clGrayText;
    DBGrid.Canvas.FillRect(Rect);
    DBGrid.Canvas.TextRect(TextRect, TextRect.Left, TextRect.Top, Text);
  end;
end;

procedure TFSession.DBGridCopyToExecute(Sender: TObject);
const
  ChunkSize = 32768;
var
  BytesRead: DWord;
  BytesToWrite: DWord;
  BytesWritten: DWord;
  CodePage: Cardinal;
  FileBuffer: array[0 .. (ChunkSize - 1) * 3] of AnsiChar;
  Handle: THandle;
  Stream: TStream;
  StreamBuffer: array[0 .. ChunkSize - 1] of Byte;
  Success: Boolean;
begin
  SaveDialog.Title := Preferences.LoadStr(582);
  SaveDialog.InitialDir := Path;
  if (ActiveDBGrid.SelectedField.DataType = ftWideMemo) then
  begin
    SaveDialog.FileName := ActiveDBGrid.SelectedField.DisplayName + '.txt';
    SaveDialog.DefaultExt := 'txt';
    SaveDialog.Filter := FilterDescription('txt') + ' (*.txt)|*.txt' + '|' + FilterDescription('*') + ' (*.*)|*.*';
    SaveDialog.Encodings.Text := EncodingCaptions();
    SaveDialog.EncodingIndex := SaveDialog.Encodings.IndexOf(CodePageToEncoding(Session.Connection.CodePage));
  end
  else if (ActiveDBGrid.SelectedField.DataType = ftBlob) then
  begin
    SaveDialog.FileName := ActiveDBGrid.SelectedField.DisplayName;
    SaveDialog.DefaultExt := '';
    SaveDialog.Filter := FilterDescription('*') + ' (*.*)|*.*';
    SaveDialog.Encodings.Clear();
    SaveDialog.EncodingIndex := -1;
  end
  else
    Exit;

  if (SaveDialog.Execute()) then
  begin
    Path := ExtractFilePath(SaveDialog.FileName);

    Handle := CreateFile(PChar(SaveDialog.FileName),
                         GENERIC_WRITE,
                         FILE_SHARE_READ,
                         nil,
                         CREATE_ALWAYS, 0, 0);
    if (Handle = INVALID_HANDLE_VALUE) then
      MsgBox(SysErrorMessage(GetLastError()), Preferences.LoadStr(45), MB_OK + MB_ICONERROR)
    else
    begin
      if (ActiveDBGrid.SelectedField.DataType in BinaryDataTypes) then
        CodePage := 0
      else
        CodePage := EncodingToCodePage(SaveDialog.Encodings[SaveDialog.EncodingIndex]);

      Stream := ActiveDBGrid.SelectedField.DataSet.CreateBlobStream(ActiveDBGrid.SelectedField, bmRead);

      if (ActiveDBGrid.SelectedField.DataType = ftWideMemo) then
        case (CodePage) of
          CP_UNICODE: WriteFile(Handle, BOM_UNICODE_LE^, Length(BOM_UNICODE_LE), BytesWritten, nil);
          CP_UTF8: WriteFile(Handle, BOM_UTF8^, Length(BOM_UTF8), BytesWritten, nil);
        end;

      repeat
        BytesRead := Stream.Read(StreamBuffer, SizeOf(StreamBuffer));
        if (BytesRead = 0) then
        begin
          BytesToWrite := 0;
          BytesWritten := 0;
          Success := True;
        end
        else if ((ActiveDBGrid.SelectedField.DataType = ftBlob) or (CodePage = CP_UNICODE)) then
        begin
          BytesToWrite := BytesRead;
          Success := WriteFile(Handle, StreamBuffer, BytesToWrite, BytesWritten, nil);
        end
        else
        begin
          BytesToWrite := WideCharToAnsiChar(CodePage, PWideChar(@StreamBuffer), BytesRead div SizeOf(WideChar), nil, 0);
          if (BytesToWrite < 1) or (SizeOf(FileBuffer) < BytesToWrite) then
            raise ERangeError.Create(SRangeError);
          WideCharToAnsiChar(CodePage, PWideChar(@StreamBuffer), BytesRead div SizeOf(WideChar), @FileBuffer, SizeOf(FileBuffer));
          Success := WriteFile(Handle, FileBuffer, BytesToWrite, BytesWritten, nil);
        end;
      until ((BytesToWrite = 0) or (BytesWritten <> BytesToWrite));

      Stream.Free();

      if (not Success) then
        MsgBox(SysErrorMessage(GetLastError), Preferences.LoadStr(45), MB_OK + MB_ICONERROR);

      CloseHandle(Handle);
    end;
  end;
end;

procedure TFSession.DBGridDataSourceDataChange(Sender: TObject; Field: TField);
begin
  if (Assigned(Window.ActiveControl) and (Window.ActiveControl = ActiveDBGrid) and Assigned(ActiveDBGrid.SelectedField) and (Field = ActiveDBGrid.SelectedField) and (ActiveDBGrid.SelectedField.DataType in [ftWideMemo, ftBlob])) then
    aVBlobExecute(nil);
end;

procedure TFSession.DBGridDblClick(Sender: TObject);
var
  DBGrid: TMySQLDBGrid;
begin
  if (not (Sender is TMySQLDBGrid)) then
    raise ERangeError.Create(SRangeError)
  else
    DBGrid := TMySQLDBGrid(Sender);

  Wanted.Clear();

  // Debug 2017-01-05
  if (not DBGrid.DataSource.Enabled) then
    raise ERangeERror.Create('ClassType: ' + DBGrid.ClassName + #13#10
      + 'Name: ' + DBGrid.Name);

  if (DBGrid.DataSource.DataSet.CanModify) then
    if (not Assigned(DBGrid.SelectedField) or not (DBGrid.SelectedField.DataType in [ftWideMemo, ftBlob])) then
      DBGrid.EditorMode := True
    else if (aVBlobText.Visible) then
    begin
      aVBlobText.Checked := True;
      aVBlobExecute(nil);
      FText.SelStart := FText.SelStart + FText.SelLength;
      SendMessage(FText.Handle, WM_VSCROLL, SB_BOTTOM, 0);
      Window.ActiveControl := FText;
    end
    else
    begin
      aVBlobHexEditor.Checked := True;
      aVBlobExecute(nil);
      FHexEditor.SelStart := FHexEditor.DataSize - 1;
      SendMessage(FHexEditor.Handle, WM_VSCROLL, SB_BOTTOM, 0);
      Window.ActiveControl := FHexEditor;
    end;
end;

procedure TFSession.DBGridEditExecute(Sender: TObject);
begin
  Wanted.Clear();

  // Debug 2016-12-15
  if (not Assigned(ActiveDBGrid)) then
    if (not Assigned(Sender)) then
      raise ERangeError.Create(SRangeError + #13#10 + 'Address: ' + Address)
    else if (Sender is TAction) then
      raise ERangeError.Create('Action Name: ' + TAction(Sender).Name + #13#10
        + 'Address: ' + Address + #13#10
        + 'Assigned: ' + BoolToStr(Assigned(GetActiveDBGrid()), True  ))
    else
      raise ERangeError.Create('Sender ClassType: ' + Sender.ClassName + #13#10
        + 'Address: ' + Address);

  DBGridDblClick(ActiveDBGrid);
end;

procedure TFSession.DBGridEmptyExecute(Sender: TObject);
begin
  Wanted.Clear();

  FText.Lines.Clear();
  FHexEditor.DataSize := 0;
  if (Assigned(FImage.Picture.Graphic)) then
    FImage.Picture.Graphic := nil;

  if (not ActiveDBGrid.SelectedField.IsNull) then
  begin
    ActiveDBGrid.DataSource.DataSet.Edit();
    if (ActiveDBGrid.EditorMode) then
      ActiveDBGrid.InplaceEditor.Text := '';
    ActiveDBGrid.SelectedField.Clear();
  end;
  DBGridColEnter(ActiveDBGrid);
end;

procedure TFSession.DBGridEnter(Sender: TObject);
var
  DBGrid: TMySQLDBGrid;
  SQL: string;
begin
  if ((Sender is TMySQLDBGrid) and Assigned(TMySQLDBGrid(Sender).DataSource.DataSet)) then
  begin
    if (View <> vIDE) then
      SQL := ''
    else
    begin
      // Debug 2016-11-23
      if (not Assigned(ActiveSynMemo)) then
        raise ERangeError.Create(SRangeError);

      SQL := SQLTrimStmt(ActiveSynMemo.Text);
    end;

    DBGrid := TMySQLDBGrid(Sender);

    MainAction('aFExportSQL').Enabled := DBGrid.DataSource.DataSet.CanModify;
    MainAction('aFExportText').Enabled := True;
    MainAction('aFExportExcel').Enabled := True;
    MainAction('aFExportXML').Enabled := True;
    MainAction('aFExportHTML').Enabled := True;
    MainAction('aFExportPDF').Enabled := True;
    MainAction('aECopyToFile').OnExecute := DBGridCopyToExecute;
    MainAction('aEPasteFromFile').OnExecute := aEPasteFromFileExecute;
    MainAction('aDEditRecord').OnExecute := DBGridEditExecute;
    MainAction('aDEmpty').OnExecute := DBGridEmptyExecute;

    MainAction('aERename').ShortCut := 0;
    MainAction('aEDelete').ShortCut := 0;

    MainAction('aDInsertRecord').ShortCut := VK_INSERT;
    MainAction('aDDeleteRecord').ShortCut := ShortCut(VK_DELETE, [ssCtrl]);
    MainAction('aDEditRecord').ShortCut := VK_F2;

    DataSetAfterScroll(DBGrid.DataSource.DataSet);
  end;
end;

procedure TFSession.DBGridExit(Sender: TObject);
var
  Cancel: Boolean;
  DBGrid: TMySQLDBGrid;
begin
  if (Sender is TMySQLDBGrid) then
  begin
    DBGrid := TMySQLDBGrid(Sender);

    try
      if (Assigned(Window.ActiveControl) and (Window.ActiveControl.Parent <> PBlob)) then
        DBGrid.DataSource.DataSet.CheckBrowseMode();
      Cancel := False;
    except
      Cancel := True;
    end;

    if (not Cancel) then
    begin
      DBGridColExit(Sender);
      DBGrid.Repaint();

      MainAction('aFExportSQL').Enabled := False;
      MainAction('aFExportText').Enabled := False;
      MainAction('aFExportExcel').Enabled := False;
      MainAction('aFExportXML').Enabled := False;
      MainAction('aFExportHTML').Enabled := False;
      MainAction('aFExportPDF').Enabled := False;
      MainAction('aFImportText').Enabled := False;
      MainAction('aFImportExcel').Enabled := False;
      MainAction('aFImportAccess').Enabled := False;
      MainAction('aFImportODBC').Enabled := False;
      MainAction('aECopyToFile').Enabled := False;
      MainAction('aEPasteFromFile').Enabled := False;
      MainAction('aDInsertRecord').Enabled := False;
      MainAction('aDDeleteRecord').Enabled := False;
      MainAction('aDEditRecord').Enabled := False;
      MainAction('aDPostObject').Enabled := False;
      MainAction('aDEmpty').Enabled := False;

      MainAction('aDInsertRecord').ShortCut := 0;
      MainAction('aDDeleteRecord').ShortCut := 0;
      MainAction('aDEditRecord').ShortCut := 0;

      MainAction('aERename').ShortCut := VK_F2;
      MainAction('aEDelete').ShortCut := ShortCut(VK_DELETE, [ssCtrl]);
    end;
  end;
end;

procedure TFSession.DBGridKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
var
  DBGrid: TMySQLDBGrid;
begin
  if (not (Sender is TMySQLDBGrid)) then
    raise ERangeError.Create(SRangeError)
  else
    DBGrid := TMySQLDBGrid(Sender);

  if (Key = VK_APPS) then
    DBGrid.PopupMenu := MGrid
  else if ((Key = VK_INSERT) and (Shift = []) and not DBGrid.EditorMode) then
    MainAction('aDInsertRecord').Execute()
  else if ((Key = VK_DELETE) and (Shift = [ssCtrl]) and not DBGrid.EditorMode) then
  begin
    MainAction('aDDeleteRecord').Execute();
    Key := 0;
  end
  else if (Assigned(DBGrid.SelectedField) and (DBGrid.SelectedField.DataType in [ftWideMemo, ftBlob])) then
    if ((Key = VK_RETURN) and not aVBlobText.Visible and not aVBlobImage.Visible) then
    begin
      aVBlobHexEditor.Checked := True;
      SendMessage(FText.Handle, WM_VSCROLL, SB_BOTTOM, 0);
    end
    else if (aVBlobText.Visible and not (Key in [VK_F2, VK_TAB, VK_DOWN, VK_UP, VK_LEFT, VK_RIGHT, VK_HOME, VK_END, VK_PRIOR, VK_NEXT, VK_APPS, VK_SHIFT, VK_CONTROL, VK_MENU])) then
    begin
      aVBlobText.Checked := True;

      // Debug 2016-12-06
      if (not PBlob.Visible) then
        raise ERangeError.Create(SRangeError);

      Window.ActiveControl := FText;
      if (Key = VK_RETURN) then
      begin
        SendMessage(FText.Handle, WM_VSCROLL, SB_BOTTOM, 0);
        PostMessage(Handle, UM_ACTIVATEFTEXT, 0, 0);
      end
      else if (Key = VK_DELETE) then
        SendMessage(FText.Handle, WM_CLEAR, 0, 0)
      else
        PostMessage(Handle, UM_ACTIVATEFTEXT, Key, 0);

      Key := 0;
    end;
end;

procedure TFSession.DBGridKeyUp(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  StatusBarRefresh();
end;

procedure TFSession.DBGridMouseMove(Sender: TObject; Shift: TShiftState; X,
  Y: Integer);
var
  ColumnX: Integer;
  DBGrid: TMySQLDBGrid;
  GridCoord: TGridCoord;
  I: Integer;
begin
  inherited;

  if (Sender is TMySQLDBGrid) then
  begin
    DBGrid := TMySQLDBGrid(Sender);

    if (not (ssLeft in Shift) and DBGrid.Dragging()) then
      DBGrid.EndDrag(False);

    GridCoord := DBGrid.MouseCoord(X, Y);

    if ((GridCoord.X >= 0) and (GridCoord.Y = 0)) then
    begin
      MGridHeaderColumn := nil;
      ColumnX := 0;
      for I := DBGrid.LeftCol to DBGrid.Columns.Count - 1 do
      begin
        if ((ColumnX <= X) and (X <= ColumnX + DBGrid.Columns[I].Width)) then
          MGridHeaderColumn := DBGrid.Columns[I];

        Inc(ColumnX, DBGrid.Columns[I].Width);
        if (dgRowLines in DBGrid.Options) then
          Inc(ColumnX, DBGrid.GridLineWidth);
      end;


      DBGrid.PopupMenu := MGridHeader;
    end
    else
      DBGrid.PopupMenu := MGrid;
  end;
end;

procedure TFSession.DBGridTitleClick(Column: TColumn);
var
  FieldName: string;
  OldDescending: Boolean;
  Pos: Integer;
  SortDef: TIndexDef;
begin
  // Debug 2016-11-21
  if (not Column.Grid.DataSource.Enabled) then
    raise ERangeError.Create(SRangeError);

  // Debug 2017-01-02
  if (not Assigned(Column)) then
    raise ERangeError.Create(SRangeError);
  if (not Assigned(Column.Field)) then
    raise ERangeError.Create(SRangeError);

  if (not (Column.Field.DataType in [ftUnknown, ftWideMemo, ftBlob])) then
  begin
    SortDef := TIndexDef.Create(nil, '', '', []);

    OldDescending := True;

    Pos := 1;
    repeat
      FieldName := ExtractFieldName(TMySQLDataSet(ActiveDBGrid.DataSource.DataSet).SortDef.Fields, Pos);
      if (FieldName <> '') then
        if (FieldName <> ActiveDBGrid.Fields[Column.Index].FieldName) then
        begin
          if (SortDef.Fields <> '') then SortDef.Fields := SortDef.Fields + ';';
          SortDef.Fields := SortDef.Fields + FieldName;
        end
        else
          OldDescending := False;
    until (FieldName = '');

    Pos := 1;
    repeat
      FieldName := ExtractFieldName(TMySQLDataSet(ActiveDBGrid.DataSource.DataSet).SortDef.DescFields, Pos);
      if (FieldName <> '') then
        if (FieldName <> ActiveDBGrid.Fields[Column.Index].FieldName) then
        begin
          if (SortDef.DescFields <> '') then SortDef.DescFields := SortDef.DescFields + ';';
          SortDef.DescFields := SortDef.DescFields + FieldName;
        end
        else
          OldDescending := True;
    until (FieldName = '');

    if (ssShift in ActiveDBGrid.MouseDownShiftState) then
    begin
      if (SortDef.Fields <> '') then SortDef.Fields := ';' + SortDef.Fields;
      SortDef.Fields := ActiveDBGrid.Fields[Column.Index].FieldName + SortDef.Fields;
      if (not OldDescending) then
      begin
        if (SortDef.DescFields <> '') then SortDef.DescFields := ';' + SortDef.DescFields;
        SortDef.DescFields := ActiveDBGrid.Fields[Column.Index].FieldName + SortDef.DescFields;
      end;
    end
    else if (ssCtrl in ActiveDBGrid.MouseDownShiftState) then
    begin
      if (SortDef.Fields <> '') then SortDef.Fields := SortDef.Fields + ';';
      SortDef.Fields := SortDef.Fields + ActiveDBGrid.Fields[Column.Index].FieldName;
      if (not OldDescending) then
      begin
        if (SortDef.DescFields <> '') then SortDef.DescFields := SortDef.DescFields + ';';
        SortDef.DescFields := SortDef.DescFields + ActiveDBGrid.Fields[Column.Index].FieldName;
      end;
    end
    else
    begin
      SortDef.Fields := '';
      SortDef.DescFields := '';
      SortDef.Fields := ActiveDBGrid.Fields[Column.Index].FieldName;
      if (not OldDescending) then
        SortDef.DescFields := ActiveDBGrid.Fields[Column.Index].FieldName;
    end;

    ActiveDBGrid.SelectedRows.Clear();
    TMySQLDataSet(ActiveDBGrid.DataSource.DataSet).Sort(SortDef);
    ActiveDBGrid.UpdateHeader();

    SortDef.Free();
  end;
end;

procedure TFSession.DBGridInitialize(const DBGrid: TMySQLDBGrid);
var
  I: Integer;
begin
  DBGrid.DataSource.DataSet.AfterClose := DataSetAfterClose;
  DBGrid.DataSource.DataSet.AfterScroll := DataSetAfterScroll;
  DBGrid.DataSource.DataSet.BeforePost := DataSetBeforePost;
  DBGrid.DataSource.DataSet.AfterPost := DataSetAfterPost;
  DBGrid.DataSource.DataSet.BeforeCancel := DataSetBeforeCancel;
  DBGrid.DataSource.DataSet.AfterCancel := DataSetAfterCancel;
  DBGrid.DataSource.DataSet.OnDeleteError := SQLError;
  DBGrid.DataSource.DataSet.OnEditError := SQLError;
  DBGrid.DataSource.DataSet.OnPostError := SQLError;

  DBGrid.DataSource.Enabled := True;

  DBGrid.Columns.BeginUpdate();
  for I := 0 to DBGrid.Columns.Count - 1 do
    if (Assigned(DBGrid.Columns[I].Field)) then
    begin
      if (DBGrid.Columns[I].Field.IsIndexField) then
        DBGrid.Canvas.Font.Style := DBGrid.Font.Style + [fsBold];

      if (DBGrid.Columns[I].Width < DBGrid.Canvas.TextWidth('ee' + DBGrid.Columns[I].Title.Caption)) then
        DBGrid.Columns[I].Width := DBGrid.Canvas.TextWidth('ee' + DBGrid.Columns[I].Title.Caption);
      if ((DBGrid.Columns[I].Width > Preferences.GridMaxColumnWidth)) then
        DBGrid.Columns[I].Width := Preferences.GridMaxColumnWidth;

      if (DBGrid.Columns[I].Field.IsIndexField) then
        DBGrid.Canvas.Font.Style := DBGrid.Font.Style - [fsBold];

      DBGrid.Columns[I].Field.OnSetText := FieldSetText;
    end;
  DBGrid.Columns.EndUpdate();

  DBGridColEnter(DBGrid);

  SResult.Visible := PResult.Visible and (PQueryBuilder.Visible or PSynMemo.Visible);
end;

destructor TFSession.Destroy();
var
  DatabasesXML: IXMLNode;
  I: Integer;
  TempB: Boolean;
  ToolbarTab: TPPreferences.TToolbarTab; // Debug 2016-12-07
  URI: TUURI;
  View: TView;
begin
  // Debug 2016-11-16
  if (not Assigned(Session)) then
    raise ERangeError.Create(SRangeError)
  else if (Sessions.IndexOf(Session) < 0) then
    raise ERangeError.Create(SRangeError);
  // Debug 2016-12-23
  if (not Assigned(Session.Account)) then
    raise ERangeError.Create(SRangeError);
  if (not (TObject(Session.Account) is TPAccount)) then
    try
      raise ERangeError.Create(SRangeError + ' ClassType: ' + TObject(Session.Account).ClassName)
    except
      raise ERangeError.Create(SRangeError); // Occured on 2016-12-25
    end;

  Session.UnRegisterEventProc(FormSessionEvent);
  Session.CreateDesktop := nil;

  FNavigatorChanging(nil, nil, TempB);

  if (Assigned(PObjectSearch)) then
    PObjectSearch.Free();

  Window.ActiveControl := nil;
  OnResize := nil;

  FNavigator.Items.BeginUpdate();
  FNavigator.Items.Clear();
  FNavigator.Items.EndUpdate();

  for View in [vEditor, vEditor2, vEditor3] do
    if (Assigned(SQLEditors[View])) then
    begin
      // Debug 2016-12-07
      if (not Assigned(Session)) then
        raise ERangeError.Create(SRangeError);
      // Debug 2016-12-10
      if (not (TObject(Session) is TSSession)) then
        try
          raise ERangeError.Create(SRangeError + ' ClassType: ' + TObject(Session).ClassName)
        except
          raise ERangeError.Create(SRangeError);
        end;
      if (not Assigned(Session.Account)) then
        raise ERangeError.Create(SRangeError);
      if (not (TObject(Session.Account) is TPAccount)) then
        try
          raise ERangeError.Create(SRangeError + ' ClassType: ' + TObject(Session.Account).ClassName)
        except
          raise ERangeError.Create(SRangeError);
        end;
      if (not Assigned(Session.Account.Desktop)) then
        raise ERangeError.Create(SRangeError);
      ToolbarTab := ToolbarTabByView[View];
      if (not (ToolbarTab in [ttEditor .. ttEditor3])) then
        raise ERangeError.Create(SRangeError + ' (' + IntToStr(Ord(ToolbarTab)) + ')');
      if (not Assigned(SQLEditors[View].SynMemo)) then
        raise ERangeError.Create(SRangeError);

      if ((SQLEditors[View].Filename <> '') and not SQLEditors[View].SynMemo.Modified) then
        Session.Account.Desktop.EditorContent[ToolbarTab] := ''
      else
        Session.Account.Desktop.EditorContent[ToolbarTab] := SQLEditors[View].SynMemo.Text;
    end;
  Session.Account.Desktop.FoldersHeight := PFolders.Height;

  if (Assigned(FFiles)) then
    Session.Account.Desktop.FilesFilter := FFiles.Filter;
  Session.Account.Desktop.NavigatorVisible := PNavigator.Visible;
  Session.Account.Desktop.ExplorerVisible := PExplorer.Visible;
  Session.Account.Desktop.SQLHistoryVisible := PSQLHistory.Visible;
  Session.Account.Desktop.SidebarWitdth := PSideBar.Width;
  Session.Account.Desktop.LogVisible := PLog.Visible;
  Session.Account.Desktop.LogHeight := PLog.Height;
  URI := TUURI.Create(Address);
  if (URI.Param['view'] = 'objectsearch') then
    URI.Param['view'] := Null;
  URI.Param['file'] := Null;
  URI.Param['cp'] := Null;
  Session.Account.Desktop.Address := URI.Address;
  FreeAndNil(URI);

  if (PResult.Align <> alBottom) then
    Session.Account.Desktop.DataHeight := PResultHeight
  else
    Session.Account.Desktop.DataHeight := PResult.Height;
  Session.Account.Desktop.BlobHeight := PBlob.Height;

  if (Assigned(Session.Account.DesktopXML)) then
  begin
    DatabasesXML := XMLNode(Session.Account.DesktopXML, 'browser/databases');
    if (Assigned(DatabasesXML)) then
      for I := DatabasesXML.ChildNodes.Count - 1 downto 0 do
        if ((DatabasesXML.ChildNodes[I].NodeName = 'database') and not Assigned(Session.DatabaseByName(DatabasesXML.ChildNodes[I].Attributes['name']))) then
          DatabasesXML.ChildNodes.Delete(I);
  end;

  Session.Account.UnRegisterTab(Self);

  if (Assigned(ObjectSearch)) then
    ObjectSearch.Free();

  FServerListView.OnChanging := nil;
  FServerListView.Items.BeginUpdate();
  FServerListView.Items.Clear();
  FServerListView.Items.EndUpdate();
  if (Assigned(ProcessesListView)) then FreeListView(ProcessesListView);
  if (Assigned(UsersListView)) then FreeListView(UsersListView);
  if (Assigned(VariablesListView)) then FreeListView(VariablesListView);
  if (Assigned(SQLEditor)) then SQLEditor.Free();
  if (Assigned(SQLEditor2)) then SQLEditor2.Free();
  if (Assigned(SQLEditor3)) then SQLEditor3.Free();
  if (Assigned(FSQLEditorSynMemo2)) then FSQLEditorSynMemo2.Free();
  if (Assigned(FSQLEditorSynMemo3)) then FSQLEditorSynMemo3.Free();

  try
    FreeAndNil(JPEGImage);
    FreeAndNil(PNGImage);
    FreeAndNil(GIFImage);
    FreeAndNil(BMPImage);
  except
  end;

  FLog.Lines.Clear();

  FreeAndNil(CloseButtonNormal);
  FreeAndNil(CloseButtonHot);
  FreeAndNil(CloseButtonPushed);

  FreeAndNil(FilterMRU);
  FreeAndNil(Wanted);

  inherited;
end;

function TFSession.Dragging(const Sender: TObject): Boolean;
begin
  Result := LeftMousePressed and (Window.ActiveControl = FNavigator) and ((Window.ActiveControl as TTreeView_Ext).Selected <> MouseDownNode);
end;

procedure TFSession.EndEditLabel(Sender: TObject);
begin
  aDCreate.ShortCut := VK_INSERT;
  aDDelete.ShortCut := VK_DELETE;
end;

procedure TFSession.FBlobResize(Sender: TObject);
begin
  FText.Repaint();
end;

procedure TFSession.FFilesEnter(Sender: TObject);
begin
  miHOpen.ShortCut := VK_RETURN;
end;

procedure TFSession.FFilterChange(Sender: TObject);
begin
  FFilterEnabled.Enabled := SQLSingleStmt(FFilter.Text);
  FFilterEnabled.Down := FFilterEnabled.Enabled and Assigned(ActiveDBGrid.DataSource.DataSet) and (FFilter.Text = TSTable.TDataSet(ActiveDBGrid.DataSource.DataSet).FilterSQL);
end;

procedure TFSession.FFilterDropDown(Sender: TObject);
var
  I: Integer;
begin
  FFilter.Items.Clear();
  for I := FilterMRU.Count - 1 downto 0 do
    FFilter.Items.Add(FilterMRU.Values[I]);
end;

procedure TFSession.FFilterEnabledClick(Sender: TObject);
begin
  FQuickSearchEnabled.Down := False;
  TableOpen(Sender);
  Window.ActiveControl := FFilter;
end;

procedure TFSession.FFilterEnter(Sender: TObject);
begin
  if (FFilter.Items.Count = 0) then
    FFilterDropDown(Sender);
end;

procedure TFSession.FFilterKeyPress(Sender: TObject; var Key: Char);
begin
  if ((Key = Chr(VK_ESCAPE)) and (TSTable.TDataSet(ActiveDBGrid.DataSource.DataSet).FilterSQL <> '')) then
  begin
    FFilter.Text := TSTable.TDataSet(ActiveDBGrid.DataSource.DataSet).FilterSQL;
    FFilterChange(Sender);

    FFilter.SelStart := 0;
    FFilter.SelLength := Length(FFilter.Text);
    Key := #0;
  end
  else if ((Key = Chr(VK_RETURN)) and not FFilterEnabled.Down) then
  begin
    FFilterEnabled.Down := True;
    FFilterEnabledClick(Sender);

    FFilter.SelStart := 0;
    FFilter.SelLength := Length(FFilter.Text);
    Key := #0;
  end;
end;

procedure TFSession.FFoldersChange(Sender: TObject; Node: TTreeNode);
begin
  if (not (tsLoading in FrameState) and PExplorer.Visible and Visible) then
    if ((Sender is TJamShellTree) and not TJamShellTree(Sender).Visible) then
    begin
      TJamShellTree(Sender).Visible := True;
      Window.ActiveControl := TJamShellTree(Sender);
    end;

  Path := FFolders.SelectedFolder;
end;

procedure TFSession.FHexEditorChange(Sender: TObject);
var
  Stream: TStream;
begin
  if (FHexEditor.Modified) then
  begin
    if (EditorField.DataSet.State <> dsEdit) then
      EditorField.DataSet.Edit();

    if (EditorField.DataType = ftWideMemo) then
      Stream := TStringStream.Create('')
    else if (EditorField.DataType = ftBlob) then
      Stream := EditorField.DataSet.CreateBlobStream(EditorField, bmWrite)
    else
      Stream := nil;

    if (Assigned(Stream)) then
    begin
      FHexEditor.SaveToStream(Stream);

      Stream.Free();
    end;
  end;

  aVBlobRTF.Visible := Assigned(EditorField) and (EditorField.DataType = ftWideMemo) and not EditorField.IsNull and IsRTF(EditorField.AsString);
end;

procedure TFSession.FHexEditorEnter(Sender: TObject);
begin
  StatusBarRefresh();
end;

procedure TFSession.FHexEditorKeyPress(Sender: TObject; var Key: Char);
begin
  if (Key = #27) then
  begin
    Window.ActiveControl := ActiveDBGrid;
    ActiveDBGrid.DataSource.DataSet.Cancel();
  end;
end;

procedure TFSession.FHexEditorShow(Sender: TObject);
var
  Stream: TStream;
begin
  if (not EditorField.IsNull and (EditorField.DataType in [ftString, ftBlob])) then
  begin
    if (EditorField.DataType = ftString) then
    begin
      Stream := TMemoryStream.Create();
      TMemoryStream(Stream).Write(EditorField.AsAnsiString[1], Length(EditorField.AsAnsiString));
    end
    else
      Stream := EditorField.DataSet.CreateBlobStream(EditorField, bmRead);
    FHexEditor.UnicodeChars := False;
    FHexEditor.BytesPerColumn := 1;
  end
  else if (not EditorField.IsNull and (EditorField.DataType = ftWideMemo)) then
  begin
    Stream := EditorField.DataSet.CreateBlobStream(EditorField, bmRead);
    FHexEditor.BytesPerColumn := 2;
  end
  else
    Stream := nil;

  if (not Assigned(Stream)) then
  begin
    FHexEditor.DataSize := 0;
    FHexEditor.UnicodeChars := False;
  end
  else
  begin
    FHexEditor.LoadFromStream(Stream);
    FHexEditor.UnicodeChars := EditorField.DataType = ftWideMemo;
    FHexEditor.AllowInsertMode := True;
    FHexEditor.InsertMode := False;
    FHexEditor.ReadOnlyView := EditorField.ReadOnly or not EditorField.DataSet.CanModify;
    FHexEditor.SelectAll();
    Stream.Free();
  end;
end;

procedure TFSession.FHTMLHide(Sender: TObject);
begin
  if (Assigned(FHTML)) then
    FreeAndNil(FHTML);
end;

procedure TFSession.FHTMLShow(Sender: TObject);
var
  FilenameP: array [0 .. MAX_PATH] of Char;
  FileStream: TFileStream;
  Stream: TStream;
begin
  if (not Assigned(FHTML)) then
  begin
    FHTML := TWebBrowser.Create(Self);
    FHTML.Align := alClient;

    PBlob.InsertControl(FHTML);
  end;

  if (Assigned(FHTML)) then
  begin
    if (not EditorField.IsNull and (EditorField.DataType = ftWideMemo)) then
      Stream := TStringStream.Create(EditorField.AsString)
    else if (not EditorField.IsNull and (EditorField.DataType = ftBlob)) then
      Stream := EditorField.DataSet.CreateBlobStream(EditorField, bmRead)
    else
      Stream := nil;

    if (Assigned(Stream) and (GetTempPath(MAX_PATH, FilenameP) > 0) and (GetTempFileName(@FilenameP, '~MF', 0, @FilenameP) > 0)) then
    begin
      FileStream := TFileStream.Create(FilenameP, fmCreate or fmShareDenyWrite);
      if (Assigned(FileStream)) then
      begin
        FileStream.CopyFrom(Stream, 0);
        FileStream.Free();

        FHTML.Navigate(FilenameP);

        DeleteFile(string(FilenameP));
      end;

      Stream.Free();
    end;
  end;
end;

procedure TFSession.FieldSetText(Sender: TField; const Text: string);
begin
  try
    Sender.AsString := Text;
  except
    OnConvertError(Sender, Text);
    Abort();
  end;
end;

procedure TFSession.FImageShow(Sender: TObject);
var
  Buffer: array [0..9] of AnsiChar;
  Size: Integer;
  Stream: TStream;
begin
  if (EditorField.IsNull or not (EditorField.DataType in [ftWideMemo, ftBlob])) then
    Stream := nil
  else
    Stream := EditorField.DataSet.CreateBlobStream(EditorField, bmRead);

  if (not Assigned(Stream) or (Stream.Size = 0)) then
    Size := 0
  else
    begin Size := Stream.Read(Buffer, Length(Buffer)); Stream.Seek(0, soFromBeginning); end;

  try
    if ((Size > 2) and (Buffer[0] = 'B') and (Buffer[1] = 'M')) then
      try BMPImage.LoadFromStream(Stream); FImage.Picture.Graphic := BMPImage; except FImage.Picture.Graphic := nil; end
    else if ((Size > 3) and (Buffer[0] = 'G') and (Buffer[1] = 'I') and (Buffer[2] = 'F')) then
      try GIFImage.LoadFromStream(Stream); FImage.Picture.Graphic := GIFImage; except FImage.Picture.Graphic := nil; end
    else if ((Size >= 10) and (Buffer[6] = 'J') and (Buffer[7] = 'F') and (Buffer[8] = 'I') and (Buffer[9] = 'F')) then
      try JPEGImage.LoadFromStream(Stream); FImage.Picture.Graphic := JPEGImage; except FImage.Picture.Graphic := nil; end
    else if ((Size >= 4) and (Buffer[1] = 'P') and (Buffer[2] = 'N') and (Buffer[3] = 'G')) then
      try PNGImage.LoadFromStream(Stream); FImage.Picture.Graphic := PNGImage; except FImage.Picture.Graphic := nil; end
    else
      FImage.Picture.Graphic := nil;
  except
  end;
  if (Assigned(Stream)) then
    Stream.Free();
end;

procedure TFSession.FLimitChange(Sender: TObject);
begin
  FUDLimit.Position := FUDLimit.Position;

  FUDOffset.Increment := FUDLimit.Position;

  FOffsetChange(Sender);
end;

procedure TFSession.FLimitEnabledClick(Sender: TObject);
begin
  FQuickSearchEnabled.Down := False;

  TableOpen(Sender);

  Window.ActiveControl := FOffset;
end;

procedure TFSession.FLogEnter(Sender: TObject);
begin
  MainAction('aSSearchReplace').Enabled := False;

  MainAction('aHIndex').ShortCut := 0;
  MainAction('aHSQL').ShortCut := ShortCut(VK_F1, []);
end;

procedure TFSession.FLogExit(Sender: TObject);
begin
  MainAction('aHIndex').ShortCut := ShortCut(VK_F1, []);
  MainAction('aHSQL').ShortCut := 0;
end;

procedure TFSession.FLogUpdate();
begin
  if (MainAction('aVSQLLog').Checked) then
  begin
    FLog.Text := Session.SQLMonitor.CacheText;

    PLogResize(nil);
  end;
end;

procedure TFSession.FNavigatorChange(Sender: TObject; Node: TTreeNode);
begin
  FNavigatorMenuNode := Node;

  if (not (tsLoading in FrameState) and Assigned(Node)) then
  begin
    KillTimer(Handle, tiNavigator);
    if (NavigatorElapse = 0) then
      FNavigatorChange2(Sender, Node)
    else
    begin
      SetTimer(Self.Handle, tiNavigator, NavigatorElapse, nil);
      NavigatorElapse := 0;
    end;
  end;
end;

procedure TFSession.FNavigatorChange2(Sender: TObject; Node: TTreeNode);
begin
  KillTimer(Handle, tiNavigator);
  FNavigatorNodeAfterActivate := nil;

  Address := NavigatorNodeToAddress(Node);
end;

procedure TFSession.FNavigatorChanging(Sender: TObject; Node: TTreeNode;
  var AllowChange: Boolean);
begin
  AllowChange := AllowChange
    and not Dragging(Sender)
    and not (Assigned(Node) and (Node.ImageIndex in [iiKey, iiField, iiVirtualField, iiSystemViewField, iiViewField, iiForeignKey]));

  if (AllowChange and Assigned(ActiveDBGrid) and Assigned(ActiveDBGrid.DataSource.DataSet) and ActiveDBGrid.DataSource.DataSet.Active) then
    try
      if ((Window.ActiveControl = FText) or (Window.ActiveControl = FRTF) or (Window.ActiveControl = FHexEditor)) then
        Window.ActiveControl := ActiveDBGrid;
      ActiveDBGrid.DataSource.DataSet.CheckBrowseMode();
    except
      AllowChange := False;
    end;
end;

procedure TFSession.FNavigatorDragDrop(Sender, Source: TObject; X, Y: Integer);
var
  I: Integer;
  List: TList;
  Objects: string;
  SourceNode: TTreeNode;
  TargetNode: TTreeNode;
begin
  if (Sender = FNavigator) then
    TargetNode := FNavigator.GetNodeAt(X, Y)
  else if (Sender = ActiveListView) then
    TargetNode := FNavigator.Selected
  else
    TargetNode := nil;

  List := TList.Create();

  if ((Source is TTreeView_Ext) and (TTreeView_Ext(Source).Name = FNavigator.Name)) then
  begin
    SourceNode := TFSession(TTreeView_Ext(Source).Owner).MouseDownNode;

    case (SourceNode.ImageIndex) of
      iiDatabase:   Objects := Objects + 'Database='    + SourceNode.Text + #13#10;
      iiBaseTable:  Objects := Objects + 'Table='       + SourceNode.Text + #13#10;
      iiView:       Objects := Objects + 'View='        + SourceNode.Text + #13#10;
      iiProcedure:  Objects := Objects + 'Procedure='   + SourceNode.Text + #13#10;
      iiFunction:   Objects := Objects + 'Function='    + SourceNode.Text + #13#10;
      iiEvent:      Objects := Objects + 'Event='       + SourceNode.Text + #13#10;
      iiKey:        Objects := Objects + 'Index='       + SourceNode.Text + #13#10;
      iiField,
      iiVirtualField,
      iiViewField:  Objects := Objects + 'Field='       + SourceNode.Text + #13#10;
      iiForeignKey: Objects := Objects + 'ForeignKey='  + SourceNode.Text + #13#10;
      iiTrigger:    Objects := Objects + 'Trigger='     + SourceNode.Text + #13#10;
      iiUser:       Objects := Objects + 'User='        + SourceNode.Text + #13#10;
    end;
    if (Objects <> '') then
      Objects := 'Address=' + NavigatorNodeToAddress(SourceNode) + #13#10 + Objects;
  end
  else if ((Source is TListView) and (TListView(Source).Parent.Name = 'PListView')) then
  begin
    SourceNode := TFSession(TComponent(TListView(Source).Owner)).FNavigator.Selected;

    for I := 0 to TListView(Source).Items.Count - 1 do
      if (TListView(Source).Items[I].Selected) then
        case (TListView(Source).Items[I].ImageIndex) of
          iiDatabase:   Objects := Objects + 'Database='   + TListView(Source).Items[I].Caption + #13#10;
          iiBaseTable:  Objects := Objects + 'Table='      + TListView(Source).Items[I].Caption + #13#10;
          iiView:       Objects := Objects + 'View='       + TListView(Source).Items[I].Caption + #13#10;
          iiProcedure:  Objects := Objects + 'Procedure='  + TListView(Source).Items[I].Caption + #13#10;
          iiFunction:   Objects := Objects + 'Function='   + TListView(Source).Items[I].Caption + #13#10;
          iiEvent:      Objects := Objects + 'Event='      + TListView(Source).Items[I].Caption + #13#10;
          iiKey:        Objects := Objects + 'Key='        + TSKey(TListView(Source).Items[I].Data).Name + #13#10;
          iiField,
          iiVirtualField,
          iiViewField:  Objects := Objects + 'Field='      + TListView(Source).Items[I].Caption + #13#10;
          iiForeignKey: Objects := Objects + 'ForeignKey=' + TListView(Source).Items[I].Caption + #13#10;
          iiTrigger:    Objects := Objects + 'Trigger='    + TListView(Source).Items[I].Caption + #13#10;
          iiUser:       Objects := Objects + 'User='       + TListView(Source).Items[I].Caption + #13#10;
        end;
    if (Objects <> '') then
      Objects := 'Address=' + NavigatorNodeToAddress(SourceNode) + #13#10 + Objects;
  end;

  List.Free();

  if (Objects <> '') then
    PasteExecute(TargetNode, Objects);
end;

procedure TFSession.FNavigatorDragOver(Sender, Source: TObject; X, Y: Integer;
  State: TDragState; var Accept: Boolean);
var
  SourceItem: TListItem;
  SourceNode: TTreeNode;
  TargetNode: TTreeNode;
begin
  Accept := False;

  TargetNode := TTreeView(Sender).GetNodeAt(X, Y);

  if (Source is TTreeView and (TTreeView(Source).Name = FNavigator.Name)) then
  begin
    SourceNode := TFSession(TTreeView(Source).Owner).MouseDownNode;
    if (Assigned(TargetNode) and (TargetNode <> SourceNode)) then
      case (SourceNode.ImageIndex) of
        iiDatabase: Accept := (TargetNode = TTreeView(Sender).Items.getFirstNode()) and (TargetNode <> SourceNode.Parent);
        iiBaseTable: Accept := (TargetNode.ImageIndex = iiDatabase) and (TargetNode <> SourceNode.Parent);
        iiProcedure,
        iiFunction: Accept := (TargetNode.ImageIndex = iiDatabase) and (TargetNode <> SourceNode.Parent);
        iiField,
        iiVirtualField: Accept := (TargetNode.ImageIndex = iiBaseTable) and (TargetNode <> SourceNode.Parent);
      end;
  end
  else if ((Source is TListView) and (TListView(Source).Parent.Name = PListView.Name) and Assigned(TargetNode)) then
  begin
    SourceItem := TListView(Source).Selected;
    case (SourceItem.ImageIndex) of
      iiDatabase: Accept := (TargetNode = TTreeView(Sender).Items.getFirstNode()) and (TargetNode <> TFSession(TListView(Source).Owner).FNavigator.Selected);
      iiBaseTable: Accept := (TargetNode.ImageIndex = iiDatabase) and (TargetNode <> TFSession(TListView(Source).Owner).FNavigator.Selected);
      iiProcedure,
      iiFunction: Accept := (TargetNode.ImageIndex = iiDatabase) and (TargetNode <> TFSession(TListView(Source).Owner).FNavigator.Selected);
      iiField,
      iiVirtualField: Accept := (TargetNode.ImageIndex = iiBaseTable) and (TargetNode <> TFSession(TListView(Source).Owner).FNavigator.Selected);
    end;
  end;
end;

procedure TFSession.FNavigatorEdited(Sender: TObject; Node: TTreeNode; var S: string);
begin
  if (not RenameSItem(Node.Data, S)) then
    S := Node.Text;
end;

procedure TFSession.FNavigatorEditing(Sender: TObject; Node: TTreeNode;
  var AllowEdit: Boolean);
begin
  AllowEdit := (Node.ImageIndex = iiDatabase) and (Session.Connection.MySQLVersion >= 50107) or (Node.ImageIndex = iiForeignKey) and (Session.Connection.MySQLVersion >= 40013) or (Node.ImageIndex in [iiBaseTable, iiView, iiEvent, iiTrigger, iiField, iiVirtualField]);
end;

procedure TFSession.FNavigatorEmptyExecute(Sender: TObject);
var
  Database: TSDatabase;
  Field: TSBaseTableField;
  List: TList;
  Table: TSBaseTable;
begin
  Wanted.Clear();

  if ((FocusedSItem is TSDatabase) and (Sender is TAction)) then
  begin
    Database := TSDatabase(FocusedSItem);
    if (not Database.Update()) then
      Wanted.Action := TAction(Sender)
    else if (MsgBox(Preferences.LoadStr(374, Database.Name), Preferences.LoadStr(101), MB_YESNOCANCEL + MB_ICONQUESTION) = IDYES) then
      Database.EmptyTables();
  end
  else if (FocusedSItem is TSBaseTable) then
  begin
    Table := TSBaseTable(FocusedSItem);
    if (MsgBox(Preferences.LoadStr(375, Table.Name), Preferences.LoadStr(101), MB_YESNOCANCEL + MB_ICONQUESTION) = IDYES) then
    begin
      Table.Empty();

      if ((View = vBrowser) and (FNavigator.Selected.Data = Table)) then
        Wanted.Update := UpdateAfterAddressChanged;
    end;
  end
  else if (FocusedSItem is TSBaseTableField) then
  begin
    Field := TSBaseTableField(FocusedSItem);
    Table := Field.Table;
    if (Assigned(Field) and (MsgBox(Preferences.LoadStr(376, Field.Name), Preferences.LoadStr(101), MB_YESNOCANCEL + MB_ICONQUESTION) = IDYES)) then
    begin
      List := TList.Create();
      List.Add(Field);
      Table.EmptyFields(List);
      Table.Update();
      List.Free();

      if ((View = vBrowser) and (FNavigator.Selected.Data = Table)) then
        Wanted.Update := UpdateAfterAddressChanged;
    end;
  end;
end;

procedure TFSession.FNavigatorEnter(Sender: TObject);
begin
  MainAction('aDEmpty').OnExecute := FNavigatorEmptyExecute;

  aDDelete.ShortCut := VK_DELETE;

  FNavigatorChanged(Sender, FNavigator.Selected);
  StatusBarRefresh();
end;

procedure TFSession.FNavigatorExit(Sender: TObject);
begin
  MainAction('aFImportSQL').Enabled := False;
  MainAction('aFImportText').Enabled := False;
  MainAction('aFImportExcel').Enabled := False;
  MainAction('aFImportAccess').Enabled := False;
  MainAction('aFImportODBC').Enabled := False;
  MainAction('aFExportSQL').Enabled := False;
  MainAction('aFExportText').Enabled := False;
  MainAction('aFExportExcel').Enabled := False;
  MainAction('aFExportAccess').Enabled := False;
  MainAction('aFExportODBC').Enabled := False;
  MainAction('aFExportXML').Enabled := False;
  MainAction('aFExportHTML').Enabled := False;
  MainAction('aFExportPDF').Enabled := False;
  MainAction('aECopy').Enabled := False;
  MainAction('aEPaste').Enabled := False;
  MainAction('aERename').Enabled := False;
  MainAction('aDCreateDatabase').Enabled := False;
  MainAction('aDCreateTable').Enabled := False;
  MainAction('aDCreateView').Enabled := False;
  MainAction('aDCreateProcedure').Enabled := False;
  MainAction('aDCreateFunction').Enabled := False;
  MainAction('aDCreateEvent').Enabled := False;
  MainAction('aDCreateTrigger').Enabled := False;
  MainAction('aDCreateKey').Enabled := False;
  MainAction('aDCreateField').Enabled := False;
  MainAction('aDCreateForeignKey').Enabled := False;
  MainAction('aDCreateUser').Enabled := False;
  MainAction('aDDeleteDatabase').Enabled := False;
  MainAction('aDDeleteTable').Enabled := False;
  MainAction('aDDeleteView').Enabled := False;
  MainAction('aDDeleteRoutine').Enabled := False;
  MainAction('aDDeleteEvent').Enabled := False;
  MainAction('aDDeleteKey').Enabled := False;
  MainAction('aDDeleteField').Enabled := False;
  MainAction('aDDeleteForeignKey').Enabled := False;
  MainAction('aDDeleteTrigger').Enabled := False;
  MainAction('aDEditServer').Enabled := False;
  MainAction('aDEditDatabase').Enabled := False;
  MainAction('aDEditTable').Enabled := False;
  MainAction('aDEditView').Enabled := False;
  MainAction('aDEditRoutine').Enabled := False;
  MainAction('aDEditEvent').Enabled := False;
  MainAction('aDEditTrigger').Enabled := False;
  MainAction('aDEditKey').Enabled := False;
  MainAction('aDEditField').Enabled := False;
  MainAction('aDEditForeignKey').Enabled := False;
  MainAction('aDEditProcess').Enabled := False;
  MainAction('aDEditUser').Enabled := False;
  MainAction('aDEditVariable').Enabled := False;
  MainAction('aDEmpty').Enabled := False;

  aDDelete.ShortCut := 0;
end;

procedure TFSession.FNavigatorExpanding(Sender: TObject; Node: TTreeNode;
  var AllowExpansion: Boolean);
var
  Database: TSDatabase;
  Table: TSTable;
begin
  // Debug 2016-11-17
  if (not Assigned(Node.Data)) then
    raise ERangeError(SRangeError);

  if (Node.HasChildren) then
  begin
    Database := nil;
    Table := nil;
    case (Node.ImageIndex) of
      iiDatabase,
      iiSystemDatabase:
        begin
          Database := TSDatabase(Node.Data);
          // Debug 2016-11-26
          if (not Assigned(Database)) then
            raise ERangeError.Create(SRangeError);
          AllowExpansion := AllowExpansion and Database.Update();
        end;
      iiBaseTable,
      iiView,
      iiSystemView:
        begin
          Table := TSTable(Node.Data);
          // Debug 2016-11-26
          if (not Assigned(Table)) then
            raise ERangeError.Create(SRangeError);
          AllowExpansion := AllowExpansion and Table.Update();
        end;
    end;

    if (not AllowExpansion and (Sender = FNavigator)) then
      FNavigatorNodeToExpand := Node;
    if (Node.HasChildren and not Assigned(Node.getFirstChild())) then
    begin
      FNavigatorNodeToExpand := Node;
      if (Assigned(Table)) then
        Table.PushBuildEvent()
      else if (Assigned(Database)) then
        Database.PushBuildEvents();
    end;
  end;
end;

procedure TFSession.FNavigatorInitialize(Sender: TObject);
var
  Node: TTreeNode;
begin
  Node := FNavigator.Items.getFirstNode().getFirstChild();
  while (Assigned(Node)) do
  begin
    case (Node.ImageIndex) of
      iiProcesses: Node.Text := Preferences.LoadStr(24);
      iiUsers: Node.Text := Preferences.LoadStr(561);
      iiVariables: Node.Text := Preferences.LoadStr(22);
    end;
    Node := Node.getNextSibling();
  end;
end;

procedure TFSession.FNavigatorKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  if (not TTreeView(Sender).IsEditing()) then
    if ((Key = Ord('C')) and (Shift = [ssCtrl]) or (Key = VK_INSERT) and (Shift = [ssCtrl])) then
      begin aECopyExecute(Sender); Key := 0; end
    else if ((Key = Ord('V')) and (Shift = [ssCtrl]) or (Key = VK_INSERT) and (Shift = [ssShift])) then
      begin aEPasteExecute(Sender); Key := 0; end
    else if (not (Key in [VK_SHIFT, VK_CONTROL])) then
      NavigatorElapse := 500;
end;

procedure TFSession.FNavigatorKeyPress(Sender: TObject; var Key: Char);
begin
  if (Key = #3) then Key := #0; // Why is threre a Beep on <Ctrl+C> without this?
end;

function TFSession.FNavigatorNodeByAddress(const Address: string): TTreeNode;
var
  AllowExpansion: Boolean;
  Child: TTreeNode;
  DatabaseNode: TTreeNode;
  TableName: string;
  TableNode: TTreeNode;
  URI: TUURI;
begin
  URI := TUURI.Create(Address);

  Result := nil;

  if (URI.Param['system'] <> Null) then
  begin
    Child := FNavigator.Items.getFirstNode().getFirstChild();
    while (Assigned(Child) and not Assigned(Result)) do
      if ((URI.Param['system'] = 'processes') and (Child.ImageIndex = iiProcesses)
        or (URI.Param['system'] = 'users') and (Child.ImageIndex = iiUsers)
        or (URI.Param['system'] = 'variables') and (Child.ImageIndex = iiVariables)) then
        Result := Child
      else
        Child := Child.getNextSibling();
  end
  else if (URI.Database <> '') then
  begin
    Child := FNavigator.Items.getFirstNode().getFirstChild(); DatabaseNode := nil;
    while (Assigned(Child) and not Assigned(DatabaseNode)) do
      if ((Child.ImageIndex in [iiDatabase, iiSystemDatabase]) and (Session.Databases.NameCmp(URI.Database, Child.Text) = 0)) then
        DatabaseNode := Child
      else
        Child := Child.getNextSibling();

    if (not Assigned(DatabaseNode)) then
      Result := nil
    else if ((URI.Table = '') and ((URI.Param['objecttype'] = Null) or (URI.Param['object'] = Null))) then
      Result := DatabaseNode
    else
    begin
      if (DatabaseNode.HasChildren and not Assigned(DatabaseNode.getFirstChild())) then
      begin
        AllowExpansion := True;
        FNavigatorExpanding(nil, DatabaseNode, AllowExpansion);
      end;

      Child := DatabaseNode.getFirstChild();
      if ((URI.Table <> '') or (URI.Param['objecttype'] = 'trigger') and (URI.Param['object'] <> Null)) then
      begin
        if (URI.Table <> '') then
          TableName := URI.Table
        else
          TableName := TSDatabase(DatabaseNode.Data).TriggerByName(URI.Param['object']).TableName;
        TableNode := nil;
        while (Assigned(Child) and not Assigned(TableNode)) do
          if ((Child.ImageIndex in [iiBaseTable, iiView, iiSystemView]) and (TSDatabase(DatabaseNode.Data).Tables.NameCmp(TableName, Child.Text) = 0)) then
            TableNode := Child
          else
            Child := Child.getNextSibling();
        if ((URI.Param['view'] <> 'ide') or (URI.Param['objecttype'] <> 'trigger') or (URI.Param['object'] = Null)) then
          Result := TableNode
        else
        begin
          if (TableNode.HasChildren and not Assigned(TableNode.getFirstChild())) then
          begin
            AllowExpansion := True;
            FNavigatorExpanding(nil, TableNode, AllowExpansion);
          end;
          Child := TableNode.getFirstChild();
          while (Assigned(Child) and not Assigned(Result)) do
            if ((Child.ImageIndex in [iiTrigger]) and (TSDatabase(DatabaseNode.Data).Triggers.NameCmp(URI.Param['object'], Child.Text) = 0)) then
              Result := Child
            else
              Child := Child.getNextSibling();
        end;
      end
      else if (URI.Param['objecttype'] = 'procedure') then
        while (Assigned(Child) and not Assigned(Result)) do
          if ((Child.ImageIndex = iiProcedure) and (TSDatabase(DatabaseNode.Data).Routines.NameCmp(Child.Text, URI.Param['object']) = 0)) then
            Result := Child
          else
            Child := Child.getNextSibling()
      else if (URI.Param['objecttype'] = 'function') then
        while (Assigned(Child) and not Assigned(Result)) do
          if ((Child.ImageIndex = iiFunction) and (TSDatabase(DatabaseNode.Data).Routines.NameCmp(Child.Text, URI.Param['object']) = 0)) then
            Result := Child
          else
            Child := Child.getNextSibling()
      else if (URI.Param['objecttype'] = 'event') then
        while (Assigned(Child) and not Assigned(Result)) do
          if ((Child.ImageIndex = iiEvent) and (TSDatabase(DatabaseNode.Data).Events.NameCmp(Child.Text, URI.Param['object']) = 0)) then
            Result := Child
          else
            Child := Child.getNextSibling()
    end;
  end
  else
    Result := FNavigator.Items.getFirstNode();

  // Debug 2016-12-27
  if (Assigned(Result)
    and not (Result.ImageIndex in [iiBaseTable, iiView, iiSystemView])
    and (URI.Param['view'] = 'browser')) then
    raise ERangeError.Create('Address: ' + Address + #13#10
      + 'ImageIndex: ' + IntToStr(Result.ImageIndex) + #13#10
      + 'Text: ' + Result.Text);

  URI.Free();
end;

procedure TFSession.FNavigatorUpdate(const Event: TSSession.TEvent);

  procedure SetNodeBoldState(Node: TTreeNode; Value: Boolean);
  var
    TVItem: TTVItem;
  begin
    if not Assigned(Node) then Exit;
    with TVItem do
    begin
      mask := TVIF_STATE or TVIF_HANDLE;
      hItem := Node.ItemId;
      stateMask := TVIS_BOLD;
      if Value then state := TVIS_BOLD
      else
        state := 0;
      TreeView_SetItem(Node.Handle, TVItem);
    end;
  end;

  function GroupIDByImageIndex(const ImageIndex: Integer): Integer;
  begin
    case (ImageIndex) of
      iiDatabase,
      iiSystemDatabase:
        Result := giDatabases;
      iiTable,
      iiBaseTable,
      iiView,
      iiSystemView:
        Result := giTables;
      iiProcedure,
      iiFunction:
        Result := giRoutines;
      iiEvent:
        Result := giEvents;
      iiKey:
        Result := giKeys;
      iiField,
      iiVirtualField,
      iiSystemViewField,
      iiViewField:
        Result := giFields;
      iiForeignKey:
        Result := giForeignKeys;
      iiTrigger:
        Result := giTriggers;
      iiProcesses,
      iiUsers,
      iiVariables:
        Result := giSystemTools;
      iiProcess:
        Result := giProcesses;
      iiUser:
        Result := giUsers;
      iiVariable:
        Result := giVariables;
      else
        Result := 0;
    end;
  end;

  function Compare(const Item1, Item2: TTreeNode): Integer;
  const
    ImageIndexSort = Chr(iiProcesses) + Chr(iiUsers) + Chr(iiVariables);
  begin
    // Debug 2016-11-17
    if (Item1.Data = Item2.Data) then
      raise ERangeError.Create(SRangeError);
    // Debug 2016-11-21
    if (TObject(Item1.Data) is TSItem) then ; // On 2016-12-09 here occured an AV. Why???
    if (TObject(Item2.Data) is TSItem) then ;
    // Debug 2016-11-23
    if (not Assigned(Item1.Data)) then
      raise ERangeError.Create(SRangeError);
    if (not Assigned(Item2.Data)) then
      raise ERangeError.Create(SRangeError);

    if (GroupIDByImageIndex(Item1.ImageIndex) <> GroupIDByImageIndex(Item2.ImageIndex)) then
      Result := Sign(GroupIDByImageIndex(Item1.ImageIndex) - GroupIDByImageIndex(Item2.ImageIndex))
    else if (GroupIDByImageIndex(Item1.ImageIndex) = giSystemTools) then
      Result := Sign(Pos(Chr(Item1.ImageIndex), ImageIndexSort) - Pos(Chr(Item2.ImageIndex), ImageIndexSort))
    else if ((TObject(Item1.Data) is TSItem) and (TObject(Item2.Data) is TSItem)) then
      Result := Sign(TSItem(Item1.Data).Index - TSItem(Item2.Data).Index)
    else
      raise ERangeError.Create(SRangeError);
  end;

  procedure InsertOrUpdateChild(const Node: TTreeNode; const Data: TObject);
  var
    Added: Boolean;
    C: Integer;
    Child: TTreeNode;
    I: Integer;
    Index: Integer;
    Left: Integer;
    Mid: Integer;
    MidChild: TTreeNode;
    NodeCount: Integer;
    OldMid: Integer;
    Right: Integer;
    Text: string;
    VirtualChild: TTreeNode;
  begin
    Index := 0;
    Child := Node.getFirstChild();
    while (Assigned(Child) and (Child.Data <> Data)) do
    begin
      Child := Child.getNextSibling();
      Inc(Index);
    end;

    NodeCount := Node.Count;
    if ((Index = NodeCount) and (NodeCount > 0)) then
    begin
      VirtualChild := TTreeNode.Create(Node.Owner);
      VirtualChild.Data := Data;
      VirtualChild.ImageIndex := ImageIndexByData(Data);
      VirtualChild.Text := TSItem(Data).Caption;

      Left := 0; C := 0; OldMid := 0; MidChild := Node.getFirstChild();
      Right := NodeCount - 1;
      while (Left <= Right) do
      begin
        Mid := (Right - Left) div 2 + Left;
        if (Mid < OldMid) then
          for I := OldMid downto Mid + 1 do
            MidChild := MidChild.getPrevSibling()
        else if (Mid > OldMid) then
          for I := OldMid to Mid - 1 do
            MidChild := MidChild.getNextSibling();
        OldMid := Mid;
        C := Compare(MidChild, VirtualChild);
        case (C) of
          -1: begin Left := Mid + 1; Index := Mid + 1; end;
          0: raise ERangeError.Create(SRangeError);
          1: begin Right := Mid - 1; Index := Mid; end;
        end;
      end;
      if (C = -1) then
        Child := MidChild.getNextSibling()
      else
        Child := MidChild;

      VirtualChild.Free();
    end;

    if (TObject(Data) is TSItem) then
      Text := TSItem(Data).Caption
    else if (TObject(Data) is TSProcesses) then
      Text := Preferences.LoadStr(24)
    else if (TObject(Data) is TSUsers) then
      Text := Preferences.LoadStr(561)
    else if (TObject(Data) is TSVariables) then
      Text := Preferences.LoadStr(22)
    else
      raise ERangeError.Create(SRangeError);

    if (Index = NodeCount) then
    begin
      Child := FNavigator.Items.AddChild(Node, Text);
      Added := True;
    end
    else if (Child.Data <> Data) then
    begin
      Child := FNavigator.Items.Insert(Child, Text);
      Added := True;
    end
    else
      Added := False;
    Child.Data := Data;
    Child.ImageIndex := ImageIndexByData(Data);
    Child.Text := Text;
    if (Added and (Child.ImageIndex in [iiDatabase, iiSystemDatabase, iiBaseTable, iiView, iiSystemView])) then
      Child.HasChildren := True;
    if (Assigned(Child)) then
      SetNodeBoldState(Child, (Child.ImageIndex = iiKey) and TSKey(Child.Data).PrimaryKey or (Child.ImageIndex in [iiField, iiVirtualField]) and TSTableField(Child.Data).InPrimaryKey);
  end;

  procedure AddChild(const Node: TTreeNode; const Data: TObject);
  var
    Child: TTreeNode;
    Text: string;
  begin
    if (TObject(Data) is TSItem) then
      Text := TSItem(Data).Caption
    else if (TObject(Data) is TSProcesses) then
      Text := Preferences.LoadStr(24)
    else if (TObject(Data) is TSUsers) then
      Text := Preferences.LoadStr(561)
    else if (TObject(Data) is TSVariables) then
      Text := Preferences.LoadStr(22)
    else
      raise ERangeError.Create(SRangeError);

    Child := FNavigator.Items.AddChild(Node, Text);
    Child.Data := Data;
    Child.ImageIndex := ImageIndexByData(Data);
    Child.Text := Text;
    if (Child.ImageIndex in [iiDatabase, iiSystemDatabase, iiBaseTable, iiView, iiSystemView]) then
      Child.HasChildren := True;
    if (Assigned(Child)) then
      SetNodeBoldState(Child, (Child.ImageIndex = iiKey) and TSKey(Child.Data).PrimaryKey or (Child.ImageIndex in [iiField, iiVirtualField]) and TSTableField(Child.Data).InPrimaryKey);

    // Debug 2016-12-08
    if (not Assigned(Child.Data)) then
      raise ERangeError.Create(SRangeError);
  end;

  procedure DeleteNode(const Node: TTreeNode);
  var
    N: TTreeNode;
  begin
    N := FNavigatorNodeToExpand;
    while (Assigned(N)) do
    begin
      if (Node = N) then
        FNavigatorNodeToExpand := nil;
      N := N.Parent;
    end;
    Node.Data := nil;
    Node.Delete();
  end;

  procedure UpdateGroup(const Node: TTreeNode; const GroupID: Integer; const SItems: TSItems);
  var
    Add: Boolean;
    Child: TTreeNode;
    DeleteChild: TTreeNode;
    Destination: TTreeNode;
    I: Integer;
  begin
    case (Event.EventType) of
      etItemsValid,
      etItemValid:
        begin
          Child := Node.getFirstChild();
          while (Assigned(Child)) do
            if ((GroupIDByImageIndex(Child.ImageIndex) <> GroupID) or (SItems.IndexOf(Child.Data) >= 0)) then
              Child := Child.getNextSibling()
            else
            begin
              if ((Child = FNavigatorNodeToExpand) or (Child.Parent = FNavigatorNodeToExpand)) then
                FNavigatorNodeToExpand := nil;
              DeleteChild := Child;
              Child := Child.getNextSibling();
              DeleteNode(DeleteChild);
            end;

          Add := not Assigned(Node.getFirstChild());
          for I := 0 to SItems.Count - 1 do
            if (not (SItems is TSTriggers) or (TSTriggers(SItems)[I].Table = Node.Data)) then
              if (not Add) then
                InsertOrUpdateChild(Node, SItems[I])
              else
                AddChild(Node, SItems[I]);
        end;
      etItemCreated:
        if (not (Event.Item is TSTrigger) or (Node.Count > 0)) then
          InsertOrUpdateChild(Node, Event.Item);
      etItemAltered:
        begin
          Child := Node.getFirstChild();
          while (Assigned(Child) and (Child.Data <> Event.Item)) do
            Child := Child.getNextSibling();
          if (Assigned(Child) and (Child.Text <> Event.Item.Caption)) then
          begin
            Child.Text := Event.Item.Caption;
            Destination := Node.getFirstChild();
            while (Assigned(Destination) and ((Destination = Child) or (Compare(Destination, Child) < 0))) do
              Destination := Destination.getNextSibling();
            if (Assigned(Destination)) then
              Child.MoveTo(Destination, naInsert)
            else
              Child.MoveTo(Node, naAddChild);
          end;
          if (Assigned(Child)) then
            SetNodeBoldState(Child, (Child.ImageIndex = iiKey) and TSKey(Child.Data).PrimaryKey or (Child.ImageIndex in [iiField, iiVirtualField]) and TSTableField(Child.Data).InPrimaryKey);
        end;
      etItemDropped:
        begin
          Child := Node.getFirstChild();
          while (Assigned(Child)) do
            if (Child.Data <> Event.Item) then
              Child := Child.getNextSibling()
            else
            begin
              if ((Child = FNavigatorNodeToExpand) or (Child.Parent = FNavigatorNodeToExpand)) then
                FNavigatorNodeToExpand := nil;
              DeleteChild := FNavigator.Selected;
              while (Assigned(DeleteChild)) do
              begin
                if (Child = DeleteChild) then
                  NavigatorElapse := 1; // We're inside a Monitor callback - but the related Address change has to be outside the callback
                DeleteChild := DeleteChild.Parent;
              end;
              DeleteChild := Child;
              Child := Child.getNextSibling();
              DeleteNode(DeleteChild);
              if (not Assigned(FNavigator.Selected)) then
                FNavigator.Selected := Node;
            end;
        end;
    end;
  end;

var
  ChangeEvent: TTVChangedEvent;
  ChangingEvent: TTVChangingEvent;
  Database: TSDatabase;
  Expanded: Boolean;
  ExpandingEvent: TTVExpandingEvent;
  Node: TTreeNode;
  OldSelected: TTreeNode;
  Table: TSTable;
begin
  OldSelected := FNavigator.Selected;

  ChangingEvent := FNavigator.OnChanging; FNavigator.OnChanging := nil;
  ChangeEvent := FNavigator.OnChange; FNavigator.OnChange := nil;

  if (Event.Sender is TSSession) then
  begin
    Node := FNavigator.Items.getFirstNode();

    FNavigator.Items.BeginUpdate();

    if (not Assigned(Node.getFirstChild())) then
      Node.Expand(False);

    if (Event.Items is TSDatabases) then
      UpdateGroup(Node, giDatabases, Event.Items);

    FNavigator.Items.EndUpdate();
  end
  else if ((Event.Sender is TSDatabase) and not (Event.Items is TSTriggers)) then
  begin
    Database := TSDatabase(Event.Sender);

    Node := FNavigator.Items.getFirstNode().getFirstChild();
    while (Assigned(Node) and (Node.Data <> Database)) do
      Node := Node.getNextSibling();

    if (Assigned(Node) and (not Node.HasChildren or Assigned(Node.getFirstChild()) or (Node = FNavigatorNodeToExpand))) then
    begin
      FNavigator.Items.BeginUpdate();

      Expanded := Node.Expanded;

      if (Expanded or (Node = FNavigatorNodeToExpand)) then
        if (Event.Items is TSTables) then
          UpdateGroup(Node, giTables, Event.Items)
        else if (Event.Items is TSRoutines) then
          UpdateGroup(Node, giRoutines, Event.Items)
        else if (Event.Items is TSEvents) then
          UpdateGroup(Node, giEvents, Event.Items);

      Node.HasChildren := Assigned(Node.getFirstChild());
      Node.Expanded := Expanded;

      FNavigator.Items.EndUpdate();
    end;
  end
  else if ((Event.Sender is TSTable) or (Event.Item is TSTrigger) and Assigned(TSTrigger(Event.Item).Table)) then
  begin
    if (Event.Item is TSTrigger) then
      Table := TSTrigger(Event.Item).Table
    else
      Table := TSTable(Event.Sender);

    Node := FNavigator.Items.getFirstNode().getFirstChild();
    while (Assigned(Node) and (Node.Data <> Table.Database)) do
      Node := Node.getNextSibling();

    if (Assigned(Node)) then
    begin
      Node := Node.getFirstChild();
      while (Assigned(Node) and (Node.Data <> Table)) do
        Node := Node.getNextSibling();

      if (Assigned(Node) and (not Node.HasChildren or Assigned(Node.getFirstChild()) or (Node = FNavigatorNodeToExpand))) then
      begin
        FNavigator.Items.BeginUpdate();

        Expanded := Node.Expanded;

        if (Expanded or (Node = FNavigatorNodeToExpand)) then
          if (Table is TSBaseTable) then
            UpdateGroup(Node, giKeys, TSBaseTable(Table).Keys);
          UpdateGroup(Node, giFields, Table.Fields);
          if ((Table is TSBaseTable) and Assigned(TSBaseTable(Table).ForeignKeys)) then
            UpdateGroup(Node, giForeignKeys, TSBaseTable(Table).ForeignKeys);
          if ((Table is TSBaseTable) and Assigned(Table.Database.Triggers)) then
            UpdateGroup(Node, giTriggers, Table.Database.Triggers);

        Node.HasChildren := Assigned(Node.getFirstChild());
        Node.Expanded := Expanded;

        FNavigator.Items.EndUpdate();
      end;
    end;
  end;

  FNavigator.OnChanging := ChangingEvent;
  FNavigator.OnChange := ChangeEvent;

  if (FNavigator.Selected <> OldSelected) then
    SetTimer(Self.Handle, tiNavigator, 1, nil);

  if (Assigned(FNavigatorNodeToExpand) and (FNavigatorNodeToExpand.Count > 0)) then
  begin
    ExpandingEvent := FNavigator.OnExpanding;
    FNavigator.OnExpanding := nil;
    FNavigatorNodeToExpand.Expand(False);
    FNavigatorNodeToExpand := nil;
    FNavigator.OnExpanding := ExpandingEvent;
  end;
end;

procedure TFSession.FNavigatorChanged(Sender: TObject; const Node: TTreeNode);
begin
  aPExpand.Enabled := Assigned(Node) and (not Assigned(Node) or not Node.Expanded) and (Assigned(Node) and Node.HasChildren);
  aPCollapse.Enabled := Assigned(Node) and (not Assigned(Node) or Node.Expanded) and (Node.ImageIndex <> iiServer);

  MainAction('aFImportSQL').Enabled := Assigned(Node) and (((Node.ImageIndex = iiServer) and (not Assigned(Session.UserRights) or Session.UserRights.RInsert)) or (Node.ImageIndex = iiDatabase));
  MainAction('aFImportText').Enabled := Assigned(Node) and (Node.ImageIndex in [iiDatabase, iiBaseTable]);
  MainAction('aFImportExcel').Enabled := Assigned(Node) and (Node.ImageIndex in [iiDatabase, iiBaseTable]);
  MainAction('aFImportAccess').Enabled := Assigned(Node) and (Node.ImageIndex in [iiDatabase, iiBaseTable]);
  MainAction('aFImportODBC').Enabled := Assigned(Node) and (Node.ImageIndex in [iiDatabase, iiBaseTable]);
  MainAction('aFExportSQL').Enabled := Assigned(Node) and (Node.ImageIndex in [iiServer, iiDatabase, iiBaseTable, iiView, iiProcedure, iiFunction, iiEvent, iiTrigger]);
  MainAction('aFExportText').Enabled := Assigned(Node) and (Node.ImageIndex in [iiBaseTable, iiView]);
  MainAction('aFExportExcel').Enabled := Assigned(Node) and (Node.ImageIndex in [iiDatabase, iiBaseTable, iiView]);
  MainAction('aFExportAccess').Enabled := Assigned(Node) and (Node.ImageIndex in [iiDatabase, iiBaseTable]);
  MainAction('aFExportODBC').Enabled := Assigned(Node) and (Node.ImageIndex in [iiDatabase, iiBaseTable]);
  MainAction('aFExportXML').Enabled := Assigned(Node) and (Node.ImageIndex in [iiServer, iiDatabase, iiBaseTable, iiView]);
  MainAction('aFExportHTML').Enabled := Assigned(Node) and (Node.ImageIndex in [iiServer, iiDatabase, iiBaseTable, iiView, iiProcedure, iiFunction, iiEvent, iiTrigger]);
  MainAction('aFExportPDF').Enabled := Assigned(Node) and (Node.ImageIndex in [iiServer, iiDatabase, iiBaseTable, iiView, iiProcedure, iiFunction, iiEvent, iiTrigger]);
  MainAction('aECopy').Enabled := Assigned(Node) and (Node.ImageIndex in [iiDatabase, iiBaseTable, iiView, iiProcedure, iiFunction, iiEvent, iiTrigger, iiField, iiVirtualField, iiViewField, iiSystemViewField]);
  MainAction('aEPaste').Enabled := Assigned(Node) and ((Node.ImageIndex = iiServer) and Clipboard.HasFormat(CF_MYSQLSERVER) or (Node.ImageIndex = iiDatabase) and Clipboard.HasFormat(CF_MYSQLDATABASE) or (Node.ImageIndex = iiBaseTable) and Clipboard.HasFormat(CF_MYSQLTABLE) or (Node.ImageIndex = iiUsers) and Clipboard.HasFormat(CF_MYSQLUSERS));
  MainAction('aERename').Enabled := Assigned(Node) and ((Node.ImageIndex = iiForeignKey) and (Session.Connection.MySQLVersion >= 40013) or (Node.ImageIndex in [iiBaseTable, iiView, iiEvent, iiTrigger, iiField, iiVirtualField]));
  MainAction('aDCreateDatabase').Enabled := Assigned(Node) and (Node.ImageIndex in [iiServer]) and (not Assigned(Session.UserRights) or Session.UserRights.RCreate);
  MainAction('aDCreateTable').Enabled := Assigned(Node) and (Node.ImageIndex = iiDatabase);
  MainAction('aDCreateView').Enabled := Assigned(Node) and (Node.ImageIndex = iiDatabase) and (Session.Connection.MySQLVersion >= 50001);
  MainAction('aDCreateProcedure').Enabled := Assigned(Node) and (Node.ImageIndex = iiDatabase) and Assigned(TSDatabase(Node.Data).Routines);
  MainAction('aDCreateFunction').Enabled := MainAction('aDCreateProcedure').Enabled;
  MainAction('aDCreateEvent').Enabled := Assigned(Node) and (Node.ImageIndex = iiDatabase) and Assigned(TSDatabase(Node.Data).Events);
  MainAction('aDCreateTrigger').Enabled := Assigned(Node) and (Node.ImageIndex = iiBaseTable) and Assigned(TSDatabase(Node.Parent.Data).Triggers);
  MainAction('aDCreateKey').Enabled := Assigned(Node) and (Node.ImageIndex = iiBaseTable);
  MainAction('aDCreateField').Enabled := Assigned(Node) and (Node.ImageIndex = iiBaseTable);
  MainAction('aDCreateForeignKey').Enabled := Assigned(Node) and (Node.ImageIndex in [iiBaseTable]);
  MainAction('aDCreateUser').Enabled := Assigned(Node) and (Node.ImageIndex = iiUsers);
  MainAction('aDDeleteDatabase').Enabled := Assigned(Node) and (Node.ImageIndex = iiDatabase);
  MainAction('aDDeleteTable').Enabled := Assigned(Node) and (Node.ImageIndex = iiBaseTable);
  MainAction('aDDeleteView').Enabled := Assigned(Node) and (Node.ImageIndex = iiView);
  MainAction('aDDeleteRoutine').Enabled := Assigned(Node) and (Node.ImageIndex in [iiProcedure, iiFunction]);
  MainAction('aDDeleteEvent').Enabled := Assigned(Node) and (Node.ImageIndex = iiEvent);
  MainAction('aDDeleteKey').Enabled := Assigned(Node) and (Node.ImageIndex = iiKey);
  MainAction('aDDeleteField').Enabled := Assigned(Node) and (Node.ImageIndex in [iiField, iiVirtualField]) and (TObject(Node.Data) is TSTableField) and (TSTableField(Node.Data).Fields.Count > 1);
  MainAction('aDDeleteForeignKey').Enabled := Assigned(Node) and (Node.ImageIndex = iiForeignKey) and (Session.Connection.MySQLVersion >= 40013);
  MainAction('aDDeleteTrigger').Enabled := Assigned(Node) and (Node.ImageIndex = iiTrigger);
  MainAction('aDDeleteProcess').Enabled := False;
  MainAction('aDEditServer').Enabled := Assigned(Node) and (Node.ImageIndex = iiServer);
  MainAction('aDEditDatabase').Enabled := Assigned(Node) and (Node.ImageIndex = iiDatabase);
  MainAction('aDEditTable').Enabled := Assigned(Node) and (Node.ImageIndex = iiBaseTable);
  MainAction('aDEditView').Enabled := Assigned(Node) and (Node.ImageIndex = iiView);
  MainAction('aDEditRoutine').Enabled := Assigned(Node) and (Node.ImageIndex in [iiProcedure, iiFunction]);
  MainAction('aDEditEvent').Enabled := Assigned(Node) and (Node.ImageIndex = iiEvent);
  MainAction('aDEditKey').Enabled := Assigned(Node) and (Node.ImageIndex = iiKey);
  MainAction('aDEditField').Enabled := Assigned(Node) and (Node.ImageIndex in [iiField, iiVirtualField]);
  MainAction('aDEditForeignKey').Enabled := Assigned(Node) and (Node.ImageIndex = iiForeignKey);
  MainAction('aDEditTrigger').Enabled := Assigned(Node) and (Node.ImageIndex = iiTrigger);
  MainAction('aDEmpty').Enabled := Assigned(Node) and ((Node.ImageIndex = iiDatabase) or (Node.ImageIndex = iiBaseTable) or ((Node.ImageIndex in [iiField]) and TSTableField(Node.Data).NullAllowed));

  miNExpand.Default := aPExpand.Enabled;
  miNCollapse.Default := aPCollapse.Enabled;
  aDDelete.Enabled := MainAction('aDDeleteDatabase').Enabled
    or MainAction('aDDeleteTable').Enabled
    or MainAction('aDDeleteView').Enabled
    or MainAction('aDDeleteRoutine').Enabled
    or MainAction('aDDeleteEvent').Enabled
    or MainAction('aDDeleteKey').Enabled
    or MainAction('aDDeleteField').Enabled
    or MainAction('aDDeleteForeignKey').Enabled
    or MainAction('aDDeleteTrigger').Enabled;
  if (not Assigned(Node)) then
    miNProperties.Action := nil
  else
    case (Node.ImageIndex) of
      iiServer: miNProperties.Action := MainAction('aDEditServer');
      iiDatabase: miNProperties.Action := MainAction('aDEditDatabase');
      iiBaseTable: miNProperties.Action := MainAction('aDEditTable');
      iiView: miNProperties.Action := MainAction('aDEditView');
      iiProcedure,
      iiFunction: miNProperties.Action := MainAction('aDEditRoutine');
      iiEvent: miNProperties.Action := MainAction('aDEditEvent');
      iiTrigger: miNProperties.Action := MainAction('aDEditTrigger');
      iiKey: miNProperties.Action := MainAction('aDEditKey');
      iiField,
      iiVirtualField: miNProperties.Action := MainAction('aDEditField');
      iiForeignKey: miNProperties.Action := MainAction('aDEditForeignKey');
      iiProcess: miNProperties.Action := MainAction('aDEditProcess');
      iiVariable: miNProperties.Action := MainAction('aDEditVariable');
      else miNProperties.Action := nil;
    end;
  miNProperties.Enabled := Assigned(miNProperties.Action) and (miNProperties.Action is TAction) and TAction(miNProperties.Action).Enabled;
  miNProperties.Caption := Preferences.LoadStr(97) + '...';
  miNProperties.ShortCut := ShortCut(VK_RETURN, [ssAlt]);

  ToolBarData.tbPropertiesAction := miNProperties.Action;
  Window.Perform(UM_UPDATETOOLBAR, 0, LPARAM(Self));

  ShowEnabledItems(MNavigator.Items);

  miNExpand.Default := miNExpand.Visible;
  miNCollapse.Default := miNCollapse.Visible;

  FNavigator.ReadOnly := not MainAction('aERename').Enabled;
end;

procedure TFSession.FObjectSearchChange(Sender: TObject);
begin
  FObjectSearchStart.Enabled := Trim(FObjectSearch.Text) <> '';
end;

procedure TFSession.FObjectSearchEnter(Sender: TObject);
begin
  if (not Assigned(PObjectSearch)) then
  begin
    PObjectSearch := TPObjectSearch.Create(nil);
    PHeaderResize(nil);
    PObjectSearch.Color := FObjectSearch.Color;
    PObjectSearch.PopupParent := Window;
    PObjectSearch.Perform(CM_SYSFONTCHANGED, 0, 0);
    PObjectSearch.Perform(UM_CHANGEPREFERENCES, 0, 0);

    PObjectSearch.Session := Session;
  end;

  PObjectSearch.Location := TObject(FNavigator.Selected.Data);
  PObjectSearch.Visible := True;
//  SetActiveWindow(PObjectSearch.Handle);
end;

procedure TFSession.FObjectSearchExit(Sender: TObject);
begin
  if (Assigned(PObjectSearch)) then
    PObjectSearch.Hide();
end;

procedure TFSession.FObjectSearchKeyPress(Sender: TObject; var Key: Char);
begin
  if (Key = #13) then
  begin
    FObjectSearchStart.Click();
    Key := #0;
  end
  else
    inherited;
end;

procedure TFSession.FObjectSearchStartClick(Sender: TObject);
begin
  if (Assigned(ObjectSearchListView)) then
  begin
    ObjectSearchListView.Free();
    ObjectSearchListView := nil;
  end;
  if (Assigned(ObjectSearch)) then
    ObjectSearch.Free();

  PObjectSearch.Hide();

  ObjectSearch := TSObjectSearch.Create(Session);
  ObjectSearch.Comment := PObjectSearch.Comment;
  ObjectSearch.Databases := PObjectSearch.Databases;
  ObjectSearch.Events := PObjectSearch.Events;
  ObjectSearch.Fields := PObjectSearch.Fields;
  ObjectSearch.Location := PObjectSearch.Location;
  ObjectSearch.MatchCase := PObjectSearch.MatchCase;
  ObjectSearch.Name := PObjectSearch.Name;
  ObjectSearch.RegExpr := PObjectSearch.RegExpr;
  ObjectSearch.Routines := PObjectSearch.Routines;
  ObjectSearch.Tables := PObjectSearch.Tables;
  ObjectSearch.Triggers := PObjectSearch.Triggers;
  ObjectSearch.WholeValue := PObjectSearch.WholeValue;
  ObjectSearch.Value := Trim(FObjectSearch.Text);
  ObjectSearch.Step1();
  Wanted.Update := ObjectSearchStep2;

  View := vObjectSearch;
  FNavigator.Selected := nil;
  Window.ActiveControl := ObjectSearchListView;
end;

procedure TFSession.FOffsetChange(Sender: TObject);
begin
  if (Assigned(ActiveDBGrid) and Assigned(ActiveDBGrid.DataSource.DataSet)) then
  begin
    FUDOffset.Position := FUDOffset.Position;

    FLimitEnabled.Enabled := FUDLimit.Position > 0;
    FLimitEnabled.Down := (FUDOffset.Position = TSTable.TDataSet(ActiveDBGrid.DataSource.DataSet).Offset) and (FUDLimit.Position = TSTable.TDataSet(ActiveDBGrid.DataSource.DataSet).Limit);
  end;
end;

procedure TFSession.FOffsetKeyPress(Sender: TObject; var Key: Char);
begin
  if ((Key = Chr(VK_ESCAPE)) and (TSTable.TDataSet(ActiveDBGrid.DataSource.DataSet).Limit > 0)) then
  begin
    FUDOffset.Position := FUDOffset.Position;
    FUDLimit.Position := FUDLimit.Position;
    FLimitChange(Sender);

    if (Sender is TEdit) then
    begin
      TEdit(Sender).SelStart := 0;
      TEdit(Sender).SelLength := Length(TEdit(Sender).Text);
    end;
    Key := #0;
  end
  else if ((Key = Chr(VK_RETURN)) and not FLimitEnabled.Down) then
  begin
    FLimitEnabled.Down := True;
    FLimitEnabledClick(Sender);

    if (Sender is TEdit) then
    begin
      TEdit(Sender).SelStart := 0;
      TEdit(Sender).SelLength := Length(TEdit(Sender).Text);
    end;
    Key := #0;
  end;
end;

procedure TFSession.FormSessionEvent(const Event: TSSession.TEvent);
begin
  // Debug 2017-01-01
  if (not Assigned(FNavigator)) then
    raise ERangeError.Create(SRangeError);

  if (not (csDestroying in ComponentState)) then
    case (Event.EventType) of
      etItemsValid,
      etItemValid,
      etItemCreated,
      etItemAltered,
      etItemDropped:
        SessionUpdate(Event);
      etMonitor:
        FLogUpdate();
      etBeforeExecuteSQL:
        BeforeExecuteSQL(Event);
      etAfterExecuteSQL:
        AfterExecuteSQL(Event);
      etError:
        Wanted.Clear();
    end;

  // Debug 2017-01-01
  if (not Assigned(FNavigator)) then
    raise ERangeError.Create(SRangeError);
end;

procedure TFSession.FormResize(Sender: TObject);
var
  MaxHeight: Integer;
begin
  PHeader.ClientHeight := ToolBar.Height + PHeader.Canvas.Pen.Width;
  PHeaderResize(Sender);

  if (PSideBar.Visible) then
  begin
    PSideBar.Constraints.MaxWidth := ClientWidth - PContent.Constraints.MinWidth - SSideBar.Width;

    MaxHeight := ClientHeight;
    if (PLog.Visible) then Dec(MaxHeight, PLog.Height);
    if (SLog.Visible) then Dec(MaxHeight, SLog.Height);
    PSideBar.Constraints.MaxHeight := MaxHeight;
    PanelResize(PSideBar);
  end;

  if (PLog.Visible) then
  begin
    PLog.Constraints.MaxHeight := ClientHeight - PHeader.Height - PContent.Constraints.MinHeight - SLog.Height;
    PLogResize(Sender);
  end;
end;

function TFSession.FQueryBuilderActiveSelectList(): TacQueryBuilderSelectListControl;
begin
  if (not Assigned(FQueryBuilderEditorPageControl())) then
    Result := nil
  else
  begin
    Result := TacQueryBuilderSelectListControl(FindChildByClassType(FQueryBuilderEditorPageControl().ActivePage, TacQueryBuilderSelectListControl));
    if (not Assigned(Result) and (FQueryBuilderEditorPageControl().PageCount = 1)) then
      Result := TacQueryBuilderSelectListControl(FindChildByClassType(FQueryBuilderEditorPageControl().Pages[0], TacQueryBuilderSelectListControl));
  end;
end;

function TFSession.FQueryBuilderActiveWorkArea(): TacQueryBuilderWorkArea;
var
  Control: TWinControl;
  PageControl: TPageControl;
begin
  if (not Assigned(FQueryBuilderEditorPageControl())) then
    Result := nil
  else
  begin
    PageControl := FQueryBuilderEditorPageControl();
    if (Assigned(PageControl.ActivePage)) then
      Result := TacQueryBuilderWorkArea(FindChildByClassType(PageControl.ActivePage, TacQueryBuilderWorkArea))
    else if (PageControl.PageCount = 1) then
      Result := TacQueryBuilderWorkArea(FindChildByClassType(PageControl.Pages[0], TacQueryBuilderWorkArea))
    else
      Result := nil;

    if (Result is TWinControl) then
    begin
      Control := TWinControl(Result);
      while (Assigned(Control) and (Control <> PageControl)) do
        if (not Control.Visible) then
          Control := nil
        else
          Control := Control.Parent;
      if (not Assigned(Control)) then
        Result := nil;
    end;

    if (not Assigned(PageControl.OnChange)) then
    begin
      PageControl.OnChange := FQueryBuilderEditorPageControlChange;
      FQueryBuilderEditorPageControlChange(nil);
    end;
    FQueryBuilderEditorPageControlCheckStyle();
  end;
end;

procedure TFSession.FQueryBuilderAddTable(Sender: TObject);
var
  MenuItem: TMenuItem;
  SQLQualifiedName: TSQLQualifiedName;
  Table: TSTable;
begin
  if (Sender is TMenuItem) then
  begin
    MenuItem := TMenuItem(Sender);

    if ((MenuItem.GetParentMenu() is TPopupMenu) and (TObject(MenuItem.Tag) is TSTable)) then
    begin
      Table := TSTable(TMenuItem(Sender).Tag);

      SQLQualifiedName := TSQLQualifiedName.Create(FQueryBuilder.MetadataContainer.SQLContext);

      SQLQualifiedName.AddName(Table.Name);
      FQueryBuilder.ActiveSubQuery.ActiveUnionSubquery.AddObjectAt(SQLQualifiedName, FQueryBuilderActiveWorkArea().ScreenToClient(TPopupMenu(MenuItem.GetParentMenu()).PopupPoint));

      SQLQualifiedName.Free();
    end;
  end;
end;

procedure TFSession.FQueryBuilderDragDrop(Sender, Source: TObject; X, Y: Integer);
var
  SQLQualifiedName: TSQLQualifiedName;
begin
  if ((Source = FNavigator) and (MouseDownNode.ImageIndex in [iiBaseTable, iiView, iiSystemView])) then
  begin
    SQLQualifiedName := TSQLQualifiedName.Create(FQueryBuilder.MetadataContainer.SQLContext);
    SQLQualifiedName.AddName(TSTable(MouseDownNode.Data).Name);
    SQLQualifiedName.AddPrefix(TSDatabase(MouseDownNode.Parent.Data).Name);
    FQueryBuilder.ActiveSubQuery.ActiveUnionSubquery.AddObjectAt(SQLQualifiedName, Point(X, Y));
    SQLQualifiedName.Free();
  end;
end;

procedure TFSession.FQueryBuilderDragOver(Sender, Source: TObject; X,
  Y: Integer; State: TDragState; var Accept: Boolean);
var
  SourceNode: TTreeNode;
begin
  Accept := False;

  if (Source = FNavigator) then
  begin
    SourceNode := MouseDownNode;

    Accept := SourceNode.ImageIndex in [iiBaseTable, iiView, iiSystemView];
  end
end;

function TFSession.FQueryBuilderEditorPageControl(): TacPageControl;
begin
  Result := TacQueryBuilderPageControl(FindChildByClassType(FQueryBuilder, TacQueryBuilderPageControl));
end;

procedure TFSession.FQueryBuilderEditorPageControlChange(Sender: TObject);
begin
  if (not Assigned(FQueryBuilderEditorPageControl().ActivePage.OnEnter)) then
    FQueryBuilderEditorPageControl().ActivePage.OnEnter := FQueryBuilderEditorTabSheetEnter;
end;

procedure TFSession.FQueryBuilderEditorPageControlCheckStyle();
var
  PageControl: TPageControl;
begin
  PageControl := FQueryBuilderEditorPageControl();
  if (PQueryBuilder.Visible and Assigned(PageControl)) then
    if ((FQueryBuilder.SubQueries.Count = 1) and PageControl.Pages[0].TabVisible) then
    begin
      PageControl.Style := tsFlatButtons;
      PageControl.Pages[0].TabVisible := False;
      PageControl.Pages[0].Visible := True;
    end
    else if ((FQueryBuilder.SubQueries.Count > 1) and not PageControl.Pages[0].TabVisible) then
    begin
      PageControl.Style := tsTabs;
      PageControl.Pages[0].TabVisible := True;
    end;
end;

procedure TFSession.FQueryBuilderEditorTabSheetEnter(Sender: TObject);
begin
  StatusBarRefresh();
end;

procedure TFSession.FQueryBuilderEnter(Sender: TObject);
begin
  FQueryBuilderSynMemo.OnChange := nil;

  StatusBarRefresh();
end;

procedure TFSession.FQueryBuilderExit(Sender: TObject);
begin
  FQueryBuilderSynMemo.OnChange := FQueryBuilderSynMemoChange;
end;

procedure TFSession.FQueryBuilderResize(Sender: TObject);
var
  FBuilderEditorSelectList: TacQueryBuilderSelectListControl;
  I: Integer;
  MaxHeight: Integer;
  NewHeight: Integer;
  PSQLEditorBuilderSelectList: TacPanel;
  ScrollBarInfo: TScrollBarInfo;
begin
  if (Assigned(FQueryBuilderEditorPageControl())) then
    for I := 0 to FQueryBuilderEditorPageControl().PageCount - 1 do
    begin
      FBuilderEditorSelectList := TacQueryBuilderSelectListControl(FindChildByClassType(FQueryBuilderEditorPageControl().Pages[I], TacQueryBuilderSelectListControl));
      if (Assigned(FBuilderEditorSelectList) and (FBuilderEditorSelectList.Parent is TacPanel)) then
        PSQLEditorBuilderSelectList := TacPanel(FBuilderEditorSelectList.Parent)
      else
        PSQLEditorBuilderSelectList := nil;
      if (Assigned(PSQLEditorBuilderSelectList)) then
      begin
        ZeroMemory(@ScrollBarInfo, SizeOf(ScrollBarInfo));
        ScrollBarInfo.cbSize := SizeOf(ScrollBarInfo);
        GetScrollBarInfo(FBuilderEditorSelectList.Handle, Integer(OBJID_HSCROLL), ScrollBarInfo);

        MaxHeight := (FBuilderEditorSelectList.DefaultRowHeight + 2) + 5 * (FBuilderEditorSelectList.DefaultRowHeight + 1) + 1;
        if (ScrollBarInfo.rgstate[0] <> STATE_SYSTEM_INVISIBLE) then
          Inc(MaxHeight, GetSystemMetrics(SM_CYHSCROLL));
        NewHeight := (FBuilderEditorSelectList.DefaultRowHeight + 2) + FBuilderEditorSelectList.SelectList.Count * (FBuilderEditorSelectList.DefaultRowHeight + 1) + 1;
        if (ScrollBarInfo.rgstate[0] <> STATE_SYSTEM_INVISIBLE) then
          Inc(NewHeight, GetSystemMetrics(SM_CYHSCROLL));

        if (PSQLEditorBuilderSelectList.Height < MaxHeight) then
          PSQLEditorBuilderSelectList.Height := NewHeight;
      end;
    end;
end;

procedure TFSession.FQueryBuilderSQLUpdated(Sender: TObject);
begin
  FQueryBuilderEditorPageControlCheckStyle();

  if ((SQLBuilder.SQL = '')
    or not Assigned(Session)
    or Session.SQLParser.ParseSQL(SQLBuilder.SQL)) then
    FQueryBuilderSynMemo.Lines.Text := SQLBuilder.SQL
  else
  begin
    FQueryBuilderSynMemo.Lines.Text := Session.SQLParser.FormatSQL();

    Session.SQLParser.Clear();
  end;
end;

procedure TFSession.FQueryBuilderSynMemoChange(Sender: TObject);
begin
  try
    FQueryBuilder.SQL := FQueryBuilderSynMemo.Lines.Text;
    PostMessage(Handle, UM_POST_BUILDER_QUERY_CHANGE, 0, 0);
  except
  end;
end;

procedure TFSession.FQueryBuilderSynMemoEnter(Sender: TObject);
begin
  SQLBuilder.OnSQLUpdated := nil;

  SynMemoEnter(Sender);
end;

procedure TFSession.FQueryBuilderSynMemoExit(Sender: TObject);
begin
  SynMemoExit(Sender);

  SQLBuilder.OnSQLUpdated := FQueryBuilderSQLUpdated;
end;

procedure TFSession.FQueryBuilderValidatePopupMenu(Sender: TacQueryBuilder;
  AControlOwner: TacQueryBuilderControlOwner; AForControl: TControl;
  APopupMenu: TPopupMenu);
var
  I: Integer;
  J: Integer;
  MenuItem: TMenuItem;
begin
  if (AForControl.ClassType = TacQueryBuilderWorkArea) then
  begin
    for I := 0 to APopupMenu.Items.Count - 1 do
      if (APopupMenu.Items[I].Caption = QueryBuilderLocalizer.ReadWideString('acAddObject', acAddObject)) then
      begin
        APopupMenu.Items[I].Caption := Preferences.LoadStr(383);
        APopupMenu.Items[I].OnClick := nil;

        for J := 0 to TSDatabase(FNavigator.Selected.Data).Tables.Count - 1 do
        begin
          MenuItem := TMenuItem.Create(Self);
          MenuItem.Caption := TSDatabase(FNavigator.Selected.Data).Tables[J].Name;
          MenuItem.OnClick := FQueryBuilderAddTable;
          MenuItem.Tag := Integer(TSDatabase(FNavigator.Selected.Data).Tables[J]);
          APopupMenu.Items[I].Add(MenuItem);
        end;
      end;
  end;
end;

procedure TFSession.FQuickSearchChange(Sender: TObject);
begin
  FQuickSearchEnabled.Enabled := Assigned(ActiveDBGrid) and (FQuickSearch.Text <> '');
  FQuickSearchEnabled.Down := FQuickSearchEnabled.Enabled and (FQuickSearch.Text <> '') and (ActiveDBGrid.DataSource.DataSet is TSTable.TDataSet) and (FQuickSearch.Text = TSTable.TDataSet(ActiveDBGrid.DataSource.DataSet).QuickSearch);
end;

procedure TFSession.FQuickSearchEnabledClick(Sender: TObject);
begin
  Wanted.Clear();

  TableOpen(Sender);
  Window.ActiveControl := FQuickSearch;
end;

procedure TFSession.FQuickSearchKeyPress(Sender: TObject; var Key: Char);
begin
  if (Key = Chr(VK_ESCAPE)) then
  begin
    FQuickSearch.Text := '';
    if ((ActiveDBGrid.DataSource.DataSet is TSTable.TDataSet) and (TSTable.TDataSet(ActiveDBGrid.DataSource.DataSet).QuickSearch <> '')) then
      FQuickSearchEnabled.Click();

    Key := #0;
  end
  else if ((Key = Chr(VK_RETURN)) and not FQuickSearchEnabled.Down) then
  begin
    FQuickSearchEnabled.Down := True;
    FQuickSearchEnabled.Click();

    FQuickSearch.SelStart := 0;
    FQuickSearch.SelLength := Length(FQuickSearch.Text);
    Key := #0;
  end;
end;

procedure TFSession.FreeDBGrid(const DBGrid: TMySQLDBGrid);
begin
  FText.OnChange := nil;
  PBlob.Parent := PContent;
  PBlob.Visible := False;
  FText.OnChange := FTextChange;

  if (DBGrid = ActiveDBGrid) then
    ActiveDBGrid := nil;

  DBGrid.Free();
end;

procedure TFSession.FreeListView(const ListView: TListView);
begin
  if (ListView = ActiveListView) then
    ActiveListView := nil;

  // Debug 2016-11-23
  if (not Assigned(ListView)) then
    raise ERangeError.Create(SRangeError);
  if (not (TObject(ListView) is TListView)) then
    raise ERangeError.Create(SRangeError);

  ListView.OnChanging := nil;
  ListView.Items.BeginUpdate();
  ListView.Items.Clear();
  ListView.Items.EndUpdate();
  try
    ListView.Free();
  except
    // Without this, sometimes there is a #5 Access Denied error...
  end;
end;

procedure TFSession.FRTFChange(Sender: TObject);
begin
  FRTF.ReadOnly := True;
{
  FRTF.Text ist nicht der RTF SourceCode!
  FRTF.ReadOnly := not Assigned(ActiveDBGrid.DataSource.DataSet) or not ActiveDBGrid.DataSource.DataSet.CanModify;
  if (not FRTF.ReadOnly and (FRTF.Text <> EditorField.OldValue)) then
    ActiveDBGrid.DataSource.DataSet.Edit();
}
end;

procedure TFSession.FRTFEnter(Sender: TObject);
begin
  FRTFChange(nil);

  StatusBarRefresh();
end;

procedure TFSession.FRTFExit(Sender: TObject);
begin
{
  FRTF.Text ist nicht der RTF SourceCode!
  if (FRTF.Modified) then
    EditorField.AsString := FRTF.Text;
}

  SendMessage(FRTF.Handle, WM_VSCROLL, SB_TOP, 0);
end;

procedure TFSession.FRTFShow(Sender: TObject);
var
  TempFRTFOnChange: TNotifyEvent;
begin
  TempFRTFOnChange := FRTF.OnChange; FRTF.OnChange := nil;

  if (not Assigned(EditorField) or EditorField.IsNull) then
  begin
    FRTF.Text := '';
    FRTF.ReadOnly := False;
  end
  else
  begin
    FRTF.Text := EditorField.AsString;
    FRTF.ReadOnly := EditorField.ReadOnly or not EditorField.DataSet.CanModify;
  end;
  FRTF.SelectAll();

  FRTF.OnChange := TempFRTFOnChange;
end;

procedure TFSession.FSQLEditorSynMemoKeyPress(Sender: TObject; var Key: Char);
var
  AnsiKey: AnsiChar;
begin
  if (Preferences.Editor.CodeCompletion
    and not SynCompletion.Form.Active) then
    if (Key = '.') then
      PostMessage(Handle, UM_SYNCOMPLETION_TIMER, tiShowSynCompletion, 10)
    else
    begin
      Byte(AnsiKey) := Ord(Key) and $FF;
      if ((AnsiKey in ['0' .. '9'])
        or (AnsiKey in ['A' .. 'Z'])
        or (AnsiKey in ['a' .. 'z'])
        or (AnsiKey in ['$', '_'])
        or (Key > Chr(127))) then
        PostMessage(Handle, UM_SYNCOMPLETION_TIMER, tiShowSynCompletion, Preferences.Editor.CodeCompletionTime)
    end;
end;

procedure TFSession.FSQLHistoryChange(Sender: TObject; Node: TTreeNode);
begin
  FSQLHistoryMenuNode := Node;
end;

procedure TFSession.FSQLHistoryChanging(Sender: TObject; Node: TTreeNode;
  var AllowChange: Boolean);
begin
  AllowChange := False;
end;

procedure TFSession.FSQLHistoryDblClick(Sender: TObject);
var
  I: Integer;
  MenuItem: TMenuItem;
begin
  Wanted.Clear();

  MSQLHistoryPopup(FSQLHistory);

  MenuItem := nil;
  if (Assigned(MSQLHistory)) then
    for I := 0 to MSQLHistory.Items.Count - 1 do
      if (MSQLHistory.Items[I].Default and MSQLHistory.Items[I].Visible and MSQLHistory.Items[I].Enabled) then
        MenuItem := MSQLHistory.Items[I];

  if (Assigned(MenuItem)) then MenuItem.Click();
end;

procedure TFSession.FSQLHistoryEnter(Sender: TObject);
begin
  miHProperties.ShortCut := ShortCut(VK_RETURN, [ssAlt]);
end;

procedure TFSession.FSQLHistoryExit(Sender: TObject);
begin
  MainAction('aECopy').Enabled := False;

  miHProperties.ShortCut := 0;
end;

procedure TFSession.FSQLHistoryHint(Sender: TObject; const Node: TTreeNode;
  var Hint: string);
begin
  if (not Assigned(Node.Data) or not (Node.ImageIndex in [iiQuery, iiStatement])) then
    Hint := ''
  else
    Hint := XMLNode(IXMLNode(Node.Data), 'sql').Text;
end;

procedure TFSession.FSQLHistoryKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  if (not TTreeView(Sender).IsEditing()) then
    if ((Key = Ord('C')) and (Shift = [ssCtrl]) or (Key = VK_INSERT) and (Shift = [ssCtrl])) then
      begin aECopyExecute(Sender); Key := 0; end
    else if ((Key = Ord('V')) and (Shift = [ssCtrl]) or (Key = VK_INSERT) and (Shift = [ssShift])) then
      begin aEPasteExecute(Sender); Key := 0; end;
end;

procedure TFSession.FSQLHistoryKeyPress(Sender: TObject; var Key: Char);
var
  I: Integer;
  MenuItem: TMenuItem;
  TreeView: TTreeView_Ext;
begin
  TreeView := TTreeView_Ext(Sender);

  if (not TreeView.IsEditing()) then
    if ((Sender = ActiveListView) and (Ord(Key) = VK_BACK)) then
      FNavigator.Selected := FNavigator.Selected.Parent
    else if (Key = #3) then
      Key := #0
    else if (Ord(Key) = VK_RETURN) then
      if (Assigned(TreeView.Selected)) then
      begin
        MenuItem := nil;
        for I := 0 to TreeView.PopupMenu.Items.Count - 1 do
          if (TreeView.PopupMenu.Items[I].Default) then
            MenuItem := TreeView.PopupMenu.Items[I];
        if Assigned(MenuItem) then
          begin MenuItem.Click(); Key := #0; end;
      end;
end;

procedure TFSession.FSQLHistoryMouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
var
  Point: TPoint;
begin
  TreeViewMouseDown(Sender, Button, Shift, X, Y);

  Point := GetClientOrigin();
  FSQLHistoryMenuNode := FSQLHistory.GetNodeAt(X, Y);
end;

procedure TFSession.FSQLHistoryRefresh(Sender: TObject);
var
  Date: TDateTime;
  DateNode: TTreeNode;
  Node: TTreeNode;
  OldNode: TTreeNode;
  XML: IXMLNode;
begin
  if (PSQLHistory.Visible) then
  begin
    DateNode := nil;

    FSQLHistory.Items.BeginUpdate();

    Node := FSQLHistory.Items.GetFirstNode();
    OldNode := Node;
    while (Assigned(Node)) do
    begin
      OldNode := Node;
      Node := Node.getNextSibling();
    end;
    if (Assigned(OldNode)) then
    begin
      Node := OldNode.getFirstChild();
      while (Assigned(Node)) do
      begin
        OldNode := Node;
        Node := Node.getNextSibling();
      end;
    end;

    if (Assigned(OldNode) and (OldNode.ImageIndex in [iiStatement, iiQuery])) then
      XML := IXMLNode(OldNode.Data).NextSibling
    else if (Session.Account.HistoryXML.ChildNodes.Count > 0) then
      XML := Session.Account.HistoryXML.ChildNodes.First()
    else
      XML := nil;

    while (Assigned(XML)) do
    begin
      if (XML.NodeName = 'sql') then
      begin
        Date := SysUtils.StrToFloat(XMLNode(XML, 'datetime').Text, FileFormatSettings);

        DateNode := nil;
        Node := FSQLHistory.Items.GetFirstNode();
        while (Assigned(Node) and not Assigned(DateNode)) do
        begin
          if (Node.Text = SysUtils.DateToStr(Date, LocaleFormatSettings)) then
            DateNode := Node;
          Node := Node.getNextSibling();
        end;
        if (not Assigned(DateNode)) then
        begin
          DateNode := FSQLHistory.Items.Add(nil, SysUtils.DateToStr(Date, LocaleFormatSettings));
          DateNode.HasChildren := True;
          DateNode.ImageIndex := iiCalendar;
        end;

        Node := FSQLHistory.Items.AddChild(DateNode, SQLStmtToCaption(XMLNode(XML, 'sql').Text));
        if (XML.Attributes['type'] <> 'query') then
          Node.ImageIndex := iiStatement
        else
          Node.ImageIndex := iiQuery;
        Node.Data := Pointer(XML);
      end;

      XML := XML.NextSibling();
    end;

    if (Assigned(DateNode)) then
    begin
      DateNode.Expand(False);

      PostMessage(FSQLHistory.Handle, WM_VSCROLL, SB_BOTTOM, 0);
    end;

    FSQLHistory.Items.EndUpdate();

    FSQLHistoryMenuNode := DateNode;
  end;
end;

procedure TFSession.FTextChange(Sender: TObject);
begin
  if (Assigned(EditorField) and EditorField.CanModify and FText.Modified) then
  begin
    if (EditorField.DataSet.State = dsBrowse) then
      EditorField.DataSet.Edit();
    case (NewLineFormat) of
      nlUnix: EditorField.AsString := ReplaceStr(FText.Text, #13#10, #10);
      nlMacintosh: EditorField.AsString := ReplaceStr(FText.Text, #13#10, #13);
      else EditorField.AsString := FText.Text;
    end;
  end;

  aVBlobRTF.Visible := Assigned(EditorField) and (EditorField.DataType = ftWideMemo) and not EditorField.IsNull and IsRTF(EditorField.AsString);

  StatusBarRefresh();
end;

procedure TFSession.FTextEnter(Sender: TObject);
begin
  StatusBarRefresh();
end;

procedure TFSession.FTextExit(Sender: TObject);
begin
  SendMessage(FText.Handle, WM_VSCROLL, SB_TOP, 0);
end;

procedure TFSession.FTextKeyPress(Sender: TObject; var Key: Char);
begin
  if ((Window.ActiveControl = FText) and (Key = #27)) then
  begin
    Window.ActiveControl := ActiveDBGrid;
    ActiveDBGrid.DataSource.DataSet.Cancel();
  end
end;

procedure TFSession.FTextKeyUp(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  StatusBarRefresh();
end;

procedure TFSession.FTextMouseUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  StatusBarRefresh();
end;

procedure TFSession.FTextShow(Sender: TObject);
var
  S: string;
begin
  FText.OnChange := nil;

  if (not Assigned(EditorField) or EditorField.IsNull) then
    FText.Text := ''
  else
  begin
    S := EditorField.AsString;
    if ((Pos(#10, S) > 0) and (Pos(#13, S) > 0)) then
    begin
      if (Pos(#10, S) < Pos(#13, S)) then
        NewLineFormat := nlUnix
      else if ((Length(S) > Pos(#13, S)) and (S[Pos(#13, S) + 1] = #10)) then
        NewLineFormat := nlWindows
      else if ((Length(S) > Pos(#10, S)) and (S[Pos(#10, S) + 1] = #13)) then
        NewLineFormat := nlMacintosh;
    end
    else if (Pos(#13, S) > 0) then
      NewLineFormat := nlMacintosh
    else
      NewLineFormat := nlUnix;

    case (NewLineFormat) of
      nlUnix: S := ReplaceStr(ReplaceStr(EditorField.AsString, #10, #13#10), #13#10#10, #13#10);
      nlMacintosh: S := ReplaceStr(ReplaceStr(EditorField.AsString, #13, #13#10), #13#13#10, #13#10);
    end;
    FText.Text := S;
  end;
  FText.Modified := False;
  if (FText.Text <> '') then
    FText.SelectAll();

  FText.ReadOnly := not Assigned(EditorField) or EditorField.ReadOnly or not EditorField.DataSet.CanModify;

  FText.OnChange := FTextChange;


  aVBlobRTF.Visible := (EditorField.DataType = ftWideMemo) and not EditorField.IsNull and IsRTF(EditorField.AsString);
end;

function TFSession.GetActiveDBGrid(): TMySQLDBGrid;
var
  I: Integer;
begin
  case (View) of
    vBrowser:
      begin
        // Debug 2016-12-23
        if (not Assigned(FNavigator)) then
          raise ERangeError.Create(SRangeError);
        if (not Assigned(FNavigator.Selected)) then
          raise ERangeError.Create(SRangeError);
        if (not (FNavigator.Selected.ImageIndex in [iiBaseTable, iiView, iiSystemView])) then
          raise ERangeError.Create('ImageIndex: ' + IntToStr(FNavigator.Selected.ImageIndex) + #13#10
            + 'Address: ' + Address);
        Result := Desktop(TSTable(FNavigator.Selected.Data)).CreateDBGrid();
      end;
    vIDE:
      case (FNavigator.Selected.ImageIndex) of
        iiProcedure,
        iiFunction: Result := Desktop(TSRoutine(FNavigator.Selected.Data)).ActiveDBGrid;
        else Result := nil;
      end;
    vBuilder:
      begin
        // Debug 2016-11-25
        if (not (TObject(FNavigator.Selected.Data) is TSDatabase)) then
          try
            raise ERangeError.Create(SRangeError + #13#10 + 'ClassType: ' + TObject(FNavigator.Selected.Data).ClassName + #13#10 + 'Address: ' + Address);
          except
            raise ERangeError.Create(SRangeError + #13#10 + 'Address: ' + Address);
          end;
        Result := Desktop(TSDatabase(FNavigator.Selected.Data)).BuilderDBGrid;
      end;
    vEditor,
    vEditor2,
    vEditor3:
      if (not Assigned(SQLEditors[View])) then
        Result := nil
      else
        Result := SQLEditors[View].ActiveDBGrid;
    else
      Result := nil;
  end;

  if (Assigned(Result)) then
  begin
    // Debug 2016-11-23
    if (not Assigned(aDDeleteRecord)) then
      raise ERangeError.Create(SRangeError);

    aDDeleteRecord.DataSource := Result.DataSource;
    aDInsertRecord.DataSource := Result.DataSource;
    Result.DataSource.OnDataChange := DBGridDataSourceDataChange;

    for I := 0 to PResult.ControlCount - 1 do
      if (PResult.Controls[I] <> PResultHeader) then
        PResult.Controls[I].Visible := PResult.Controls[I] = Result.Parent;
  end;
end;

function TFSession.GetActiveIDEInputDataSet(): TDataSet;
var
  I: Integer;
  J: Integer;
  Routine: TSRoutine;
begin
  case (SelectedImageIndex) of
    iiProcedure,
    iiFunction:
      begin
        Routine := TSRoutine(FNavigator.Selected.Data);

        FObjectIDEGrid.DataSource.DataSet := Routine.InputDataSet;
        FObjectIDEGrid.DataSource.Enabled := Assigned(FObjectIDEGrid.DataSource.DataSet);

        for I := 0 to Routine.ParameterCount - 1 do
        begin
          FObjectIDEGrid.Columns[I].PickList.Clear();
          if (Routine.Parameter[I].FieldType = mfEnum) then
            for J := 0 to Length(Routine.Parameter[I].Items) - 1 do
              FObjectIDEGrid.Columns[I].PickList.Add(Routine.Parameter[I].Items[J]);
        end;
      end;
    iiTrigger:
      if (not Session.Connection.InUse()) then
        FObjectIDEGrid.DataSource.DataSet := TSTrigger(FNavigator.Selected.Data).InputDataSet;
    else
      FObjectIDEGrid.DataSource.DataSet := nil;
  end;

  if (Assigned(FObjectIDEGrid.DataSource.DataSet) and not FObjectIDEGrid.DataSource.Enabled) then
    FObjectIDEGrid.DataSource.Enabled := True;

  if (Assigned(FObjectIDEGrid.DataSource.DataSet)) then
  begin
    FObjectIDEGrid.Columns.BeginUpdate();
    for I := 0 to FObjectIDEGrid.Columns.Count - 1 do
      if ((FObjectIDEGrid.Columns[I].Width > Preferences.GridMaxColumnWidth)) then
        FObjectIDEGrid.Columns[I].Width := Preferences.GridMaxColumnWidth;
    FObjectIDEGrid.Columns.EndUpdate();
  end;

  if (Assigned(FObjectIDEGrid.DataSource.DataSet) and not FObjectIDEGrid.DataSource.Enabled) then
    DBGridColEnter(FObjectIDEGrid);

  PObjectIDETrigger.Visible := (SelectedImageIndex = iiTrigger);
  if (PObjectIDETrigger.Visible) then
    DBGridColExit(FObjectIDEGrid);

  PObjectIDEResize(nil);

  Result := FObjectIDEGrid.DataSource.DataSet;
end;

function TFSession.GetActiveListView(): TListView;
var
  I: Integer;
begin
  if (not Assigned(FNavigator.Selected)) then
    Result := FServerListView
  else if (View = vObjectSearch) then
  begin
    if (not Assigned(ObjectSearchListView)) then
      ObjectSearchListView := CreateListView(ObjectSearch);
    Result := ObjectSearchListView;
  end
  else
    case (FNavigator.Selected.ImageIndex) of
      iiServer: Result := FServerListView;
      iiDatabase,
      iiSystemDatabase:
        Result := Desktop(TSDatabase(FNavigator.Selected.Data)).CreateListView();
      iiBaseTable,
      iiView,
      iiSystemView:
        Result := Desktop(TSTable(FNavigator.Selected.Data)).CreateListView();
      iiProcesses:
        begin
          if (not Assigned(ProcessesListView)) then
          begin
            ProcessesListView := CreateListView(Session.Processes);
            Session.Processes.PushBuildEvent(Session.Processes);
          end;
          Result := ProcessesListView;
        end;
      iiUsers:
        begin
          if (not Assigned(UsersListView)) then
          begin
            UsersListView := CreateListView(Session.Users);
            Session.Users.PushBuildEvent(Session.Users);
          end;
          Result := UsersListView;
        end;
      iiVariables:
        begin
          if (not Assigned(VariablesListView)) then
          begin
            VariablesListView := CreateListView(Session.Variables);
            Session.Variables.PushBuildEvent(Session.Variables);
          end;
          Result := VariablesListView;
        end;
      else
        Result := nil;
    end;

  for I := 0 to PListView.ControlCount - 1 do
    PListView.Controls[I].Visible := PListView.Controls[I] = Result;
end;

function TFSession.GetActiveSynMemo(): TSynMemo;
var
  I: Integer;
begin
  case (View) of
    vIDE:
      begin
        case (SelectedImageIndex) of
          iiView: Result := Desktop(TSView(FNavigator.Selected.Data)).CreateSynMemo();
          iiProcedure,
          iiFunction: Result := Desktop(TSRoutine(FNavigator.Selected.Data)).CreateSynMemo();
          iiTrigger: Result := Desktop(TSTrigger(FNavigator.Selected.Data)).CreateSynMemo();
          iiEvent: Result := Desktop(TSEvent(FNavigator.Selected.Data)).CreateSynMemo();
          else Result := nil;
        end;
      end;
    vBuilder:
      Result := FQueryBuilderSynMemo;
    vEditor:
      Result := FSQLEditorSynMemo;
    vEditor2:
      begin
        if (not Assigned(FSQLEditorSynMemo2)) then
        begin
          FSQLEditorSynMemo2 := CreateSynMemo(nil);
          FSQLEditorSynMemo2.Options := FSQLEditorSynMemo2.Options + [eoScrollPastEol];  // Speed up the performance
          FSQLEditorSynMemo2.Text := Session.Account.Desktop.EditorContent[ttEditor2];
          if (Length(FSQLEditorSynMemo2.Lines.Text) < LargeSQLScriptSize) then
            FSQLEditorSynMemo2.Options := FSQLEditorSynMemo2.Options - [eoScrollPastEol];  // Slow down the performance on large content
          SQLEditor2 := TSQLEditor.Create(Self, FSQLEditorSynMemo2, CreatePDBGrid());
        end;
        Result := FSQLEditorSynMemo2;
      end;
    vEditor3:
      begin
        if (not Assigned(FSQLEditorSynMemo3)) then
        begin
          FSQLEditorSynMemo3 := CreateSynMemo(nil);
          FSQLEditorSynMemo3.Options := FSQLEditorSynMemo3.Options + [eoScrollPastEol];  // Speed up the performance
          FSQLEditorSynMemo3.Text := Session.Account.Desktop.EditorContent[ttEditor3];
          if (Length(FSQLEditorSynMemo3.Lines.Text) < LargeSQLScriptSize) then
            FSQLEditorSynMemo3.Options := FSQLEditorSynMemo3.Options - [eoScrollPastEol];  // Slow down the performance on large content
          SQLEditor3 := TSQLEditor.Create(Self, FSQLEditorSynMemo3, CreatePDBGrid());
        end;
        Result := FSQLEditorSynMemo3;
      end;
    else
      Result := nil;
  end;

  // Debug 2016-12-17
  if (not Assigned(PSynMemo)) then
    if (csDestroying in ComponentState) then
      raise ERangeError.Create('csDestroying')
    else
      raise ERangeError.Create('Error Message')
  else if (not (PSynMemo is TWinControl)) then
    try
      raise ERangeError.Create('ClassType: ' + PSynMemo.ClassName);
    except
      raise ERangeError.Create(SRangeError);
    end;

  for I := 0 to PSynMemo.ControlCount - 1 do
    PSynMemo.Controls[I].Visible := PSynMemo.Controls[I] = Result;
end;

function TFSession.GetActiveWorkbench(): TWWorkbench;
var
  I: Integer;
begin
  case (View) of
    vDiagram:
      Result := Desktop(TSDatabase(FNavigator.Selected.Data)).Workbench;
    else
      Result := nil;
  end;

  if (Assigned(Result)) then
    for I := 0 to PWorkbench.ControlCount - 1 do
      PWorkbench.Controls[I].Visible := PWorkbench.Controls[I] = Result;
end;

function TFSession.GetEditorField(): TField;
begin
  // Debug 2016-12-24
  if (Assigned(ActiveDBGrid)) then
  begin
    if (not (TObject(ActiveDBGrid) is TDBGrid)) then
      try
        raise ERangeERror.Create('ClassType: ' + TObject(ActiveDBGrid).ClassName);
      except
        raise ERangeError.Create(SRangeError);
      end;
    if (Assigned(ActiveDBGrid.SelectedField)) then
      if (not (ActiveDBGrid.SelectedField is TField)) then
        try
          raise ERangeError.Create(SRangeError + ' ClassType: ' + ActiveDBGrid.SelectedField.ClassName);
        finally
          raise ERangeError.Create(SRangeError);
        end;
  end;

  if (not Assigned(ActiveDBGrid)
    or not Assigned(ActiveDBGrid.SelectedField)
    or not (ActiveDBGrid.SelectedField.DataType in [ftString, ftWideMemo, ftBlob])) then
    Result := nil
  else
    Result := ActiveDBGrid.SelectedField;
end;

function TFSession.GetFocusedSItem(): TSItem;
begin
  if (not Assigned(Window.ActiveControl)) then
    Result := nil
  else if ((Window.ActiveControl = ActiveListView) and Assigned(ActiveListView.Selected)) then
    if ((ActiveListView.SelCount > 1) or not (TSObject(ActiveListView.Selected.Data) is TSItem)) then
      Result := nil
    else
      Result := TSItem(ActiveListView.Selected.Data)
  else if ((Window.ActiveControl = ActiveWorkbench) and Assigned(ActiveWorkbench) and Assigned(ActiveWorkbench.Selected)) then
    if (ActiveWorkbench.Selected is TWTable) then
      Result := TWTable(ActiveWorkbench.Selected).BaseTable
    else if (ActiveWorkbench.Selected is TWForeignKey) then
      Result := TWForeignKey(ActiveWorkbench.Selected).BaseForeignKey
    else
      Result := TSDatabase(FNavigator.Selected.Data)
  else if ((Window.ActiveControl = ActiveListView) and not Assigned(ActiveListView.Selected) or (Window.ActiveControl = ActiveWorkbench) and (not Assigned(ActiveWorkbench) or not Assigned(ActiveWorkbench.Selected))) then
    Result := TSItem(FNavigator.Selected.Data)
  else if (Assigned(FNavigatorMenuNode)) then
    Result := TSItem(FNavigatorMenuNode.Data)
  else if ((Window.ActiveControl = FNavigator) and Assigned(FNavigator.Selected)) then
    Result := TSItem(FNavigator.Selected.Data)
  else
    Result := nil;

  // Debug 2016-12-19
  if (Assigned(Result)) then
    try
      if (Result is TObject) then
        Write;
    except
      raise ERangeError.Create('ActiveControl: ' + Window.ActiveControl.ClassName);
    end;
end;

function TFSession.GetMenuDatabase(): TSDatabase;
var
  Node: TTreeNode;
begin
  if (Window.ActiveControl = FNavigator) then
    Node := FNavigatorMenuNode
  else
    Node := FNavigator.Selected;

  while (Assigned(Node) and Assigned(Node.Parent) and Assigned(Node.Parent.Parent)) do
    Node := Node.Parent;

  if (Assigned(Node) and (Node.ImageIndex in [iiDatabase, iiSystemDatabase])) then
    Result := TSDatabase(Node.Data)
  else
    Result := nil;
end;

function TFSession.GetPath(): TFileName;
begin
  Result := ExcludeTrailingPathDelimiter(Preferences.Path);
end;

function TFSession.GetSelectedDatabase(): string;
var
  URI: TUURI;
begin
  URI := TUURI.Create(Address);
  Result := URI.Database;
  URI.Free();
end;

function TFSession.GetSelectedImageIndex(): Integer;
begin
  // Debug 2016-12-09
  if (not Assigned(Self)) then
    raise ERangeError.Create(SRangeError);
  if (not Assigned(FNavigator)) then
    raise ERangeError.Create(SRangeError);

  if (not Assigned(FNavigator.Selected)) then
    Result := -1
  else
    Result := FNavigator.Selected.ImageIndex;
end;

function TFSession.GetSQLEditors(View: TView): TSQLEditor;
begin
  case (View) of
    vEditor: Result := SQLEditor;
    vEditor2: Result := SQLEditor2;
    vEditor3: Result := SQLEditor3;
    else raise ERangeError.Create(SRangeError + ' (' + IntToStr(Ord(View)) +  ')');
  end;
end;

function TFSession.GetWindow(): TForm_Ext;
begin
  if (Owner is TForm_Ext) then
    Result := TForm_Ext(Owner)
  else
    try
      raise ERangeError.Create(TObject(Owner).ClassName);
    except
      raise ERangeError.Create('Owner not set')
    end;
end;

procedure TFSession.ghmCopyClick(Sender: TObject);
var
  ClipboardData: HGLOBAL;
  Content: string;
  FieldInfo: TFieldInfo;
  Len: Integer;
begin
  if (GetFieldInfo(MGridHeaderColumn.Field.Origin, FieldInfo) and OpenClipboard(Handle)) then
  begin
    try
      EmptyClipboard();

      Content := Session.Connection.EscapeIdentifier(FieldInfo.OriginalFieldName);

      Len := Length(Content);
      ClipboardData := GlobalAlloc(GMEM_MOVEABLE + GMEM_DDESHARE, (Len + 1) * SizeOf(Content[1]));
      Move(PChar(Content)^, GlobalLock(ClipboardData)^, (Len + 1) * SizeOf(Content[1]));
      SetClipboardData(CF_UNICODETEXT, ClipboardData);
      GlobalUnlock(ClipboardData);
    finally
      CloseClipboard();
    end;
  end;
end;

procedure TFSession.ghmGotoClick(Sender: TObject);
var
  Item: TMenuItem;
begin
  if (Sender is TMenuItem) then
  begin
    Item := TMenuItem(Sender);

    ActiveDBGrid.SelectedIndex := Item.Parent.IndexOf(Item);
  end;
end;

procedure TFSession.gmFilterClearClick(Sender: TObject);
begin
  Wanted.Clear();

  if (ActiveDBGrid.DataSource.DataSet.Filtered) then
  begin
    ActiveDBGrid.DataSource.DataSet.Filtered := False;
    ActiveDBGrid.DataSource.DataSet.Filter := '';
    StatusBarRefresh();
  end
  else if (ActiveDBGrid.DataSource.DataSet is TSTable.TDataSet) then
  begin
    FFilterEnabled.Down := False;
    FFilterEnabledClick(Sender);
  end;
end;

procedure TFSession.gmFilterIntoFilterClick(Sender: TObject);
var
  FilterIndex: Integer;
  MenuItem: TMenuItem;
  Success: Boolean;
  Value: string;
begin
  Wanted.Clear();

  MenuItem := TMenuItem(Sender); FilterIndex := MenuItem.Tag;

  Success := True;
  case (Filters[FilterIndex].ValueType) of
    0: Value := '';
    1: if (ActiveDBGrid.SelectedField.DataType in NotQuotedDataTypes) then
         Value := ActiveDBGrid.SelectedField.DisplayText
       else
         Value := SQLEscape(ActiveDBGrid.SelectedField.DisplayText);
    2: Value := SQLEscape('%' + ActiveDBGrid.SelectedField.DisplayText + '%');
    else
      begin
        if (Filters[FilterIndex].ValueType = 3) then
          DQuickFilter.Data := ActiveDBGrid.SelectedField.DisplayText
        else
          DQuickFilter.Data := '%' + ActiveDBGrid.SelectedField.DisplayText + '%';
        Success := DQuickFilter.Execute();
        if (Success) then
          if (ActiveDBGrid.SelectedField.DataType in NotQuotedDataTypes) then
            Value := DQuickFilter.Data
          else
            Value := SQLEscape(DQuickFilter.Data);
      end;
  end;

  if (Success) then
    if (View = vBrowser) then
    begin
      FFilter.Text := Format(Filters[FilterIndex].Text, [Session.Connection.EscapeIdentifier(ActiveDBGrid.SelectedField.FieldName), Value]);
      FFilterEnabled.Enabled := True;
      FFilterEnabled.Down := True;
      FFilterEnabledClick(Sender);
      if (not (TObject(FNavigator.Selected.Data) is TSTable)) then
        raise ERangeError.Create(SRangeError);
      Desktop(TSTable(FNavigator.Selected.Data)).AddFilter(Format(Filters[FilterIndex].Text, [Session.Connection.EscapeIdentifier(ActiveDBGrid.SelectedField.FieldName), Value]));
    end
    else
    begin
      ActiveDBGrid.DataSource.DataSet.FilterOptions := [foCaseInsensitive];
      ActiveDBGrid.DataSource.DataSet.Filter := Format(Filters[FilterIndex].Text, ['[' + ActiveDBGrid.SelectedField.FieldName + ']', Value]);
      ActiveDBGrid.DataSource.DataSet.Filtered := True;
      StatusBarRefresh();
    end;
end;

function TFSession.ImageIndexByData(const Data: TObject): Integer;
begin
  if (not Assigned(Data)) then
    Result := iiServer
  else if (TObject(Data) is TSSystemDatabase) then
    Result := iiSystemDatabase
  else if (TObject(Data) is TSDatabase) then
    Result := iiDatabase
  else if (TObject(Data) is TSBaseTable) then
    Result := iiBaseTable
  else if (TObject(Data) is TSView) then
    Result := iiView
  else if (TObject(Data) is TSSystemView) then
    Result := iiSystemView
  else if (TObject(Data) is TSProcedure) then
    Result := iiProcedure
  else if (TObject(Data) is TSFunction) then
    Result := iiFunction
  else if (TObject(Data) is TSEvent) then
    Result := iiEvent
  else if (TObject(Data) is TSKey) then
    Result := iiKey
  else if (TObject(Data) is TSTableField) then
    if (TSTableField(Data).Table is TSSystemView) then
      Result := iiSystemViewField
    else if (TSTableField(Data).Table is TSView) then
      Result := iiViewField
    else if (TSTableField(Data).FieldKind = mkVirtual) then
      Result := iiVirtualField
    else
      Result := iiField
  else if (TObject(Data) is TSForeignKey) then
    Result := iiForeignKey
  else if (TObject(Data) is TSTrigger) then
    Result := iiTrigger
  else if (TObject(Data) is TSUser) then
    Result := iiUser
  else if (TObject(Data) is TSProcesses) then
    Result := iiProcesses
  else if (TObject(Data) is TSUsers) then
    Result := iiUsers
  else if (TObject(Data) is TSVariables) then
    Result := iiVariables
  else
    raise ERangeError.Create(SRangeError);
end;

function TFSession.ImportError(const Details: TTool.TErrorDetails): TDataAction;
begin
  MsgBox(Details.Error.ErrorMessage, Preferences.LoadStr(45), MB_OK + MB_ICONERROR);

  Result := daAbort;
end;

procedure TFSession.ListViewAdvancedCustomDrawItem(
  Sender: TCustomListView; Item: TListItem; State: TCustomDrawState;
  Stage: TCustomDrawStage; var DefaultDraw: Boolean);
begin
  if ((Stage = cdPrePaint) and Assigned(Item)
    and ((Item.ImageIndex = iiKey) and TSKey(Item.Data).PrimaryKey or (Item.ImageIndex in [iiField, iiVirtualField]) and TSTableField(Item.Data).InPrimaryKey)) then
    Sender.Canvas.Font.Style := [fsBold]
  else
    Sender.Canvas.Font.Style := [];
end;

procedure TFSession.ListViewAdvancedCustomDrawSubItem(
  Sender: TCustomListView; Item: TListItem; SubItem: Integer;
  State: TCustomDrawState; Stage: TCustomDrawStage; var DefaultDraw: Boolean);
begin
  Sender.Canvas.Font.Style := [fsBold]; // Without this the first sub item is Bold (Delphi XE4)
  Sender.Canvas.Font.Style := [];
end;

procedure TFSession.ListViewColumnClick(Sender: TObject; Column: TListColumn);
var
  HDItem: THDItem;
  I: Integer;
  ListView: TListView;
  Kind: TPAccount.TDesktop.TListViewKind;
begin
  if (not (Sender is TListView)) then
    raise ERangeError.Create(SRangeError)
  else
    ListView := TListView(Sender);

  Kind := ColumnWidthKindByListView(ListView);

  with (ListViewSortData[Kind]) do
  begin
    ListViewSortData[Kind].Index := Column.Index;
    if ((OldFListOrderIndex <> Index) or ((Order = 0) or (Order < 0) and (not (SelectedImageIndex in [iiBaseTable, iiView, iiSystemView]) or (Index > 0)))) then
      ListViewSortData[Kind].Order := 1
    else if (Order = 1) then
      ListViewSortData[Kind].Order := -1
    else
      ListViewSortData[Kind].Order := 0;

    ListView.CustomSort(nil, LPARAM(@ListViewSortData[Kind]));

    HDItem.Mask := HDI_FORMAT;
    for I := 0 to ListView.Columns.Count - 1 do
      if (BOOL(SendMessage(ListView_GetHeader(ListView.Handle), HDM_GETITEM, I, LParam(@HDItem)))) then
      begin
        if ((Order = 0) or (I <> Index)) then
          HDItem.fmt := HDItem.fmt and not HDF_SORTUP and not HDF_SORTDOWN
        else if (Order > 0) then
          HDItem.fmt := HDItem.fmt and not HDF_SORTDOWN or HDF_SORTUP
        else
          HDItem.fmt := HDItem.fmt and not HDF_SORTUP or HDF_SORTDOWN;
        SendMessage(ListView_GetHeader(ListView.Handle), HDM_SETITEM, I, LParam(@HDItem));
      end;

    if ((ComCtl32MajorVersion >= 6) and not CheckWin32Version(6, 1)) then
      if (Order = 0) then
        SendMessage(ListView.Handle, LVM_SETSELECTEDCOLUMN, -1, 0)
      else
        SendMessage(ListView.Handle, LVM_SETSELECTEDCOLUMN, Index, 0);

    OldFListOrderIndex := Index;
  end;
end;

procedure TFSession.ListViewCompare(Sender: TObject; Item1: TListItem;
  Item2: TListItem; Data: Integer; var Compare: Integer);
const
  ImageIndexSort = Chr(iiProcesses) + Chr(iiUsers) + Chr(iiVariables);
var
  String1: string;
  String2: string;
  SortRec: ^TListViewSortRec;
begin
  // Debug 2016-11-21
  if (not Assigned(Item1.Data)) then
    raise ERangeError.Create(SRangeError);
  if (not Assigned(Item2.Data)) then
    raise ERangeError.Create(SRangeError);

  SortRec := @TListViewSortRec(Pointer(Data)^);

  Compare := 0;
  if (Item1.GroupID <> Item2.GroupID) then
    Compare := Sign(Item1.GroupID - Item2.GroupID)
  else if (Item1.GroupID = giSystemTools) then
    Compare := Sign(Pos(Chr(Item1.ImageIndex), ImageIndexSort) - Pos(Chr(Item2.ImageIndex), ImageIndexSort))
  else if (SortRec^.Index = 0) then
    Compare := Sign(TSItem(Item1.Data).Index - TSItem(Item2.Data).Index)
  else
  begin
    if ((SortRec^.Index > Item1.SubItems.Count) or (SortRec^.Index > Item2.SubItems.Count)) then
    begin
      String1 := Item1.Caption;
      String2 := Item2.Caption;
    end
    else
    begin
      String1 := Item1.SubItems[SortRec^.Index - 1];
      String2 := Item2.SubItems[SortRec^.Index - 1];

      Compare := 0;
      case (SelectedImageIndex) of
        iiServer:
          case (SortRec^.Index) of
            1:
              begin
                if (String1 = '') then String1 := '0';              if (String2 = '') then String2 := '0';
                Compare := Sign(SysUtils.StrToInt64(String1) - SysUtils.StrToInt64(String2));
              end;
            2:
              begin
                if (String1 = '') then String1 := '0';              if (String2 = '') then String2 := '0';

                String1 := ReplaceStr(String1, '???', '0');         String2 := ReplaceStr(String2, '???', '0');

                String1 := ReplaceStr(String1, ' B', '');           String2 := ReplaceStr(String2, ' B', '');
                String1 := ReplaceStr(String1, ' KB', '000');       String2 := ReplaceStr(String2, ' KB', '000');
                String1 := ReplaceStr(String1, ' MB', '000000');    String2 := ReplaceStr(String2, ' MB', '000000');
                String1 := ReplaceStr(String1, ' GB', '000000000'); String2 := ReplaceStr(String2, ' GB', '000000000');

                String1 := ReplaceStr(String1, LocaleFormatSettings.ThousandSeparator, '');
                String2 := ReplaceStr(String2, LocaleFormatSettings.ThousandSeparator, '');
                Compare := Sign(SysUtils.StrToFloat(String1, LocaleFormatSettings) - SysUtils.StrToFloat(String2, LocaleFormatSettings));
              end;
            3:
              begin
                if (String1 = '') then String1 := SysUtils.DateToStr(1, LocaleFormatSettings);
                if (String2 = '') then String2 := SysUtils.DateToStr(1, LocaleFormatSettings);

                String1 := ReplaceStr(String1, '???', SysUtils.DateToStr(1, LocaleFormatSettings));
                String2 := ReplaceStr(String2, '???', SysUtils.DateToStr(1, LocaleFormatSettings));

                Compare := Sign(SysUtils.StrToDateTime(String1, LocaleFormatSettings) - SysUtils.StrToDateTime(String2, LocaleFormatSettings));
              end;
          end;
        iiDatabase:
          case (SortRec^.Index) of
            2:
              begin
                if (String1 = '') then String1 := '0';
                if (String2 = '') then String2 := '0';

                String1 := ReplaceStr(String1, '???', '0');         String2 := ReplaceStr(String2, '???', '0');

                if ((Length(String1) >= 1) and (String1[1] = '~')) then Delete(String1, 1, 1);
                if ((Length(String2) >= 1) and (String2[1] = '~')) then Delete(String2, 1, 1);

                String1 := ReplaceStr(String1, LocaleFormatSettings.ThousandSeparator, '');
                String2 := ReplaceStr(String2, LocaleFormatSettings.ThousandSeparator, '');
                Compare := Sign(SysUtils.StrToFloat(String1, LocaleFormatSettings) - SysUtils.StrToFloat(String2, LocaleFormatSettings));
              end;
            3:
              begin
                if (String1 = '') then String1 := '0';               if (String2 = '') then String2 := '0';

                String1 := ReplaceStr(String1, '???', '0');         String2 := ReplaceStr(String2, '???', '0');
                String1 := ReplaceStr(String1, '???', '0');         String2 := ReplaceStr(String2, '???', '0');
                String1 := ReplaceStr(String1, ' B', '');           String2 := ReplaceStr(String2, ' B', '');
                String1 := ReplaceStr(String1, ' KB', '000');       String2 := ReplaceStr(String2, ' KB', '000');
                String1 := ReplaceStr(String1, ' MB', '000000');    String2 := ReplaceStr(String2, ' MB', '000000');
                String1 := ReplaceStr(String1, ' GB', '000000000'); String2 := ReplaceStr(String2, ' GB', '000000000');

                String1 := ReplaceStr(String1, LocaleFormatSettings.ThousandSeparator, '');
                String2 := ReplaceStr(String2, LocaleFormatSettings.ThousandSeparator, '');
                Compare := Sign(SysUtils.StrToFloat(String1, LocaleFormatSettings) - SysUtils.StrToFloat(String2, LocaleFormatSettings));
              end;
            4:
              begin
                if (String1 = '') then String1 := SysUtils.DateToStr(1, LocaleFormatSettings);
                if (String2 = '') then String2 := SysUtils.DateToStr(1, LocaleFormatSettings);

                String1 := ReplaceStr(String1, '???', SysUtils.DateToStr(1, LocaleFormatSettings));
                String2 := ReplaceStr(String2, '???', SysUtils.DateToStr(1, LocaleFormatSettings));

                Compare := Sign(SysUtils.StrToDateTime(String1, LocaleFormatSettings) - SysUtils.StrToDateTime(String2, LocaleFormatSettings));
              end;
          end;
        iiProcesses:
          case (SortRec^.Index) of
            6:
              Compare := Sign(Double(TSProcess(Item1.Data).Time) - Double(TSProcess(Item2.Data).Time))
          end;
      end;
    end;

    if (Compare = 0) then
      Compare := Sign(lstrcmpi(PChar(String1), PChar(String2)));
    if (Compare = 0) then
      Compare := Sign(lstrcmp(PChar(String1), PChar(String2)));
    if (Compare = 0) then
      Compare := Sign(lstrcmpi(PChar(Item1.Caption), PChar(Item2.Caption)));
    if (Compare = 0) then
      Compare := Sign(lstrcmp(PChar(Item1.Caption), PChar(Item2.Caption)));
  end;

  if (ListViewSortData[SortRec^.Kind].Order <> 0) then
    Compare := ListViewSortData[SortRec^.Kind].Order * Compare;
end;

procedure TFSession.ListViewDblClick(Sender: TObject);
var
  I: Integer;
  MenuItem: TMenuItem;
  PopupMenu: TPopupMenu;
begin
  Wanted.Clear();

  MenuItem := nil;

  if ((Sender is TListView) and Assigned(TListView(Sender).OnSelectItem)) then
    TListView(Sender).OnSelectItem(Sender, TListView(Sender).Selected, Assigned(TListView(Sender).Selected));

  if (Sender is TListView) then
    PopupMenu := TListView(Sender).PopupMenu
  else if (Sender is TJamShellList) then
    PopupMenu := TJamShellList(Sender).PopupMenu
  else if (Sender is TWWorkbench) then
    PopupMenu := TWWorkbench(Sender).PopupMenu
  else
    PopupMenu := nil;

  if (Assigned(PopupMenu)) then
    for I := 0 to PopupMenu.Items.Count - 1 do
      if (PopupMenu.Items[I].Default and PopupMenu.Items[I].Visible and PopupMenu.Items[I].Enabled) then
        MenuItem := PopupMenu.Items[I];

  if (Assigned(MenuItem)) then MenuItem.Click();
end;

procedure TFSession.ListViewDragOver(Sender, Source: TObject; X, Y: Integer;
  State: TDragState; var Accept: Boolean);
var
  SourceSession: TSSession;
  SourceDatabase: TSDatabase;
  SourceItem: TListItem;
  SourceNode: TTreeNode;
  TargetItem: TListItem;
begin
  Accept := False;

  TargetItem := TListView(Sender).GetItemAt(X, Y);

  if ((Source is TTreeView) and (TTreeView(Source).Name = FNavigator.Name)) then
  begin
    SourceNode := TFSession(TTreeView(Source).Owner).MouseDownNode;

    if (not Assigned(TargetItem)) then
      case (SourceNode.ImageIndex) of
        iiBaseTable,
        iiProcedure,
        iiFunction,
        iiField,
        iiVirtualField: Accept := (SourceNode.Parent.ImageIndex = SelectedImageIndex) and (SourceNode.Parent <> FNavigator.Selected);
      end
    else if (((TargetItem.Caption <> SourceNode.Text) or (SourceNode.Parent <> FNavigator.Selected)) and (SourceNode.Parent.Text <> TargetItem.Caption)) then
      case (TargetItem.ImageIndex) of
        iiDatabase: Accept := (SourceNode.ImageIndex in [iiDatabase, iiBaseTable, iiProcedure, iiFunction]);
        iiBaseTable: Accept := SourceNode.ImageIndex in [iiField, iiVirtualField];
      end;
  end
  else if ((Source is TListView) and (TListView(Source).SelCount = 1) and (TListView(Source).Parent.Name = PListView.Name)) then
  begin
    SourceItem := TListView(Source).Selected;
    SourceSession := TFSession(TTreeView_Ext(Source).Owner).Session;
    SourceDatabase := TFSession(TTreeView_Ext(Source).Owner).MenuDatabase;

    if (not Assigned(TargetItem)) then
      case (SourceItem.ImageIndex) of
        iiBaseTable,
        iiProcedure,
        iiFunction: Accept := (SelectedImageIndex = iiDatabase) and (not Assigned(TargetItem) and (Session.DatabaseByName(SelectedDatabase) <> SourceDatabase));
      end
    else if ((TargetItem <> SourceItem) and (TargetItem.ImageIndex = SourceItem.ImageIndex)) then
      case (SourceItem.ImageIndex) of
        iiBaseTable: Accept := (SelectedImageIndex = iiDatabase);
      end
    else if ((TargetItem <> SourceItem) and (SourceSession <> Session)) then
      case (SourceItem.ImageIndex) of
        iiBaseTable: Accept := (SelectedImageIndex = iiServer);
      end;
  end;
end;

procedure TFSession.ListViewEdited(Sender: TObject; Item: TListItem;
  var S: string);
begin
  // Debug 2016-11-27
  if (not Assigned(Item)) then
    raise ERangeError.Create(SRangeError);

  if (not RenameSItem(Item.Data, S)) then
    S := Item.Caption;
end;

procedure TFSession.ListViewEditing(Sender: TObject; Item: TListItem;
  var AllowEdit: Boolean);
begin
  AllowEdit := (Item.ImageIndex = iiDatabase)
    and (Session.Connection.MySQLVersion >= 50107) or (Item.ImageIndex = iiForeignKey) and (Session.Connection.MySQLVersion >= 40013) or (Item.ImageIndex in [iiBaseTable, iiView, iiEvent, iiField, iiVirtualField, iiTrigger, iiUser]);
end;

procedure TFSession.ListViewInitialize(const ListView: TListView);

  procedure SetColumnWidths(const ListView: TListView; const Kind: TPAccount.TDesktop.TListViewKind);
  var
    HDLayout: THDLayout;
    HDRect: TRect;
    HDWindowPos: TWindowPos;
    I: Integer;
    MinWidth: Integer;
  begin
    MinWidth := -1;
    HDLayout.Rect := @HDRect;
    HDLayout.WindowPos := @HDWindowPos;
    if (Header_Layout(ListView_GetHeader(ListView.Handle), @HDLayout)) then
      MinWidth := HDWindowPos.cy;

    for I := 0 to ListView.Columns.Count - 1 do
    begin
      ListView.Column[I].Width := Session.Account.Desktop.ColumnWidths[Kind, I];
      if (MinWidth > 0) then
        ListView.Columns[I].MinWidth := MinWidth;
    end;
  end;

var
  Count: Integer;
  I: Integer;
  Update: Boolean;
begin
  Update := ListView.Columns.Count > 0;

  ListView.Groups.BeginUpdate();

  if (not Update) then
  begin
    ListView.Columns.BeginUpdate();
    if (ListView = FServerListView) then
    begin
      ListView.Columns.Add();
      ListView.Columns.Add();
      ListView.Columns.Add();
      ListView.Columns.Add();
      ListView.Columns.Add();
      ListView.Columns.EndUpdate();
      SetColumnWidths(ListView, lkServer);
      ListView.Columns[1].Alignment := taRightJustify;
      ListView.Columns[2].Alignment := taRightJustify;

      ListView.Groups.Add().GroupID := giDatabases;
      ListView.Groups.Add().GroupID := giSystemTools;
    end
    else if (TObject(ListView.Tag) is TSDatabase) then
    begin
      ListView.Columns.Add();
      if (not (TObject(ListView.Tag) is TSSystemDatabase)) then
      begin
        ListView.Columns.Add();
        ListView.Columns.Add();
        ListView.Columns.Add();
        ListView.Columns.Add();
        ListView.Columns.Add();
        ListView.Columns.Add();
      end;
      ListView.Columns.EndUpdate();
      SetColumnWidths(ListView, lkDatabase);
      if (not (TObject(ListView.Tag) is TSSystemDatabase)) then
      begin
        if (ListView.Column[1].Width > 2 * Preferences.GridMaxColumnWidth) then
          ListView.Column[1].Width := 2 * Preferences.GridMaxColumnWidth;
        ListView.Columns[2].Alignment := taRightJustify;
        ListView.Columns[3].Alignment := taRightJustify;
      end;

      ListView.Groups.Add().GroupID := giTables;
      ListView.Groups.Add().GroupID := giRoutines;
      ListView.Groups.Add().GroupID := giEvents;
    end
    else if (TObject(ListView.Tag) is TSBaseTable) then
    begin
      ListView.Columns.Add();
      ListView.Columns.Add();
      ListView.Columns.Add();
      ListView.Columns.Add();
      ListView.Columns.Add();
      if (Session.Connection.MySQLVersion >= 40100) then
        ListView.Columns.Add();
      ListView.Columns.EndUpdate();
      SetColumnWidths(ListView, lkTable);
      if (ListView.Column[1].Width > 2 * Preferences.GridMaxColumnWidth) then
        ListView.Column[1].Width := 2 * Preferences.GridMaxColumnWidth;

      ListView.Groups.Add().GroupID := giKeys;
      ListView.Groups.Add().GroupID := giFields;
      ListView.Groups.Add().GroupID := giForeignKeys;
      ListView.Groups.Add().GroupID := giTriggers;
    end
    else if ((TObject(ListView.Tag) is TSView)
      or (TObject(ListView.Tag) is TSSystemView)) then
    begin
      ListView.Columns.Add();
      ListView.Columns.Add();
      ListView.Columns.Add();
      ListView.Columns.Add();
      ListView.Columns.Add();
      ListView.Columns.EndUpdate();
      SetColumnWidths(ListView, lkTable);
      if (ListView.Column[1].Width > 2 * Preferences.GridMaxColumnWidth) then
        ListView.Column[1].Width := 2 * Preferences.GridMaxColumnWidth;

      ListView.Groups.Add().GroupID := giFields;
    end
    else if (TObject(ListView.Tag) is TSProcesses) then
    begin
      ListView.Columns.Add();
      ListView.Columns.Add();
      ListView.Columns.Add();
      ListView.Columns.Add();
      ListView.Columns.Add();
      ListView.Columns.Add();
      ListView.Columns.Add();
      ListView.Columns.Add();
      ListView.Columns.EndUpdate();
      SetColumnWidths(ListView, lkProcesses);

      ListView.Groups.Add().GroupID := giProcesses;
    end
    else if (TObject(ListView.Tag) is TSUsers) then
    begin
      ListView.Columns.Add();
      ListView.Columns.EndUpdate();
      SetColumnWidths(ListView, lkUsers);

      ListView.Groups.Add().GroupID := giUsers;
    end
    else if (TObject(ListView.Tag) is TSVariables) then
    begin
      ListView.Columns.Add();
      ListView.Columns.Add();
      ListView.Columns.EndUpdate();
      SetColumnWidths(ListView, lkVariables);

      ListView.Groups.Add().GroupID := giVariables;
    end
    else if (TObject(ListView.Tag) is TSObjectSearch) then
    begin
      ListView.Columns.Add();
      ListView.Columns.Add();
      ListView.Columns.Add();
      ListView.Columns.Add();
      ListView.Columns.Add();
      ListView.Columns.EndUpdate();
      SetColumnWidths(ListView, lkObjectSearch);

      ListView.Groups.Add().GroupID := giDatabases;
      ListView.Groups.Add().GroupID := giTables;
      ListView.Groups.Add().GroupID := giRoutines;
      ListView.Groups.Add().GroupID := giEvents;
      ListView.Groups.Add().GroupID := giFields;
      ListView.Groups.Add().GroupID := giTriggers;
      ListView.Groups.Add().GroupID := giUsers;
    end;
  end;

  ListView.Groups.EndUpdate();

  if (ListView = FServerListView) then
  begin
    ListView.Columns[0].Caption := Preferences.LoadStr(35);
    ListView.Columns[1].Caption := Preferences.LoadStr(76);
    ListView.Columns[2].Caption := Preferences.LoadStr(67);
    ListView.Columns[3].Caption := Preferences.LoadStr(77);
    ListView.Columns[4].Caption := Preferences.LoadStr(73);

    Count := ListView.Items.Count;
    for I := 0 to ListView.Items.Count - 1 do
      case (ListView.Items[I].ImageIndex) of
        iiProcesses: ListView.Items[I].Caption := Preferences.LoadStr(24);
        iiUsers: ListView.Items[I].Caption := Preferences.LoadStr(561);
        iiVariables: ListView.Items[I].Caption := Preferences.LoadStr(22);
        else Dec(Count);
      end;
    SetListViewGroupHeader(ListView, giSystemTools, Preferences.LoadStr(12) + ' (' + IntToStr(Count) + ')');
  end
  else if (TObject(ListView.Tag) is TSDatabase) then
  begin
    ListView.Columns[0].Caption := Preferences.LoadStr(35);
    if (not (TObject(ListView.Tag) is TSSystemDatabase)) then
    begin
      ListView.Columns[1].Caption := Preferences.LoadStr(69);
      ListView.Columns[2].Caption := Preferences.LoadStr(66);
      ListView.Columns[3].Caption := Preferences.LoadStr(67);
      ListView.Columns[4].Caption := Preferences.LoadStr(68);
      ListView.Columns[5].Caption := Preferences.LoadStr(73);
      ListView.Columns[6].Caption := Preferences.LoadStr(111);
    end;
  end
  else if (TObject(ListView.Tag) is TSBaseTable) then
  begin
    ListView.Columns[0].Caption := Preferences.LoadStr(35);
    ListView.Columns[1].Caption := Preferences.LoadStr(69);
    ListView.Columns[2].Caption := Preferences.LoadStr(71);
    ListView.Columns[3].Caption := Preferences.LoadStr(72);
    ListView.Columns[4].Caption := Preferences.LoadStr(73);
    if (Session.Connection.MySQLVersion >= 40100) then
      ListView.Columns[5].Caption := Preferences.LoadStr(111);
  end
  else if ((TObject(ListView.Tag) is TSView)
    or (TObject(ListView.Tag) is TSSystemView)) then
  begin
    ListView.Columns[0].Caption := Preferences.LoadStr(35);
    ListView.Columns[1].Caption := Preferences.LoadStr(69);
    ListView.Columns[2].Caption := Preferences.LoadStr(71);
    ListView.Columns[3].Caption := Preferences.LoadStr(72);
    ListView.Columns[4].Caption := Preferences.LoadStr(73);
  end
  else if (TObject(ListView.Tag) is TSProcesses) then
  begin
    ListView.Columns[0].Caption := Preferences.LoadStr(269);
    ListView.Columns[1].Caption := Preferences.LoadStr(561);
    ListView.Columns[2].Caption := Preferences.LoadStr(271);
    ListView.Columns[3].Caption := Preferences.LoadStr(38);
    ListView.Columns[4].Caption := Preferences.LoadStr(273);
    ListView.Columns[5].Caption := Preferences.LoadStr(274);
    ListView.Columns[6].Caption := Preferences.LoadStr(661);
    ListView.Columns[7].Caption := Preferences.LoadStr(276);
  end
  else if (TObject(ListView.Tag) is TSUsers) then
  begin
    ListView.Columns[0].Caption := Preferences.LoadStr(561);
  end
  else if (TObject(ListView.Tag) is TSVariables) then
  begin
    ListView.Columns[0].Caption := Preferences.LoadStr(267);
    ListView.Columns[1].Caption := Preferences.LoadStr(268);
  end
  else if (TObject(ListView.Tag) is TSObjectSearch) then
  begin
    ListView.Columns[0].Caption := Preferences.LoadStr(35);
    ListView.Columns[1].Caption := Preferences.LoadStr(69);
    ListView.Columns[2].Caption := Preferences.LoadStr(935);
    ListView.Columns[3].Caption := Preferences.LoadStr(119);
    ListView.Columns[4].Caption := Preferences.LoadStr(111);
  end;
end;

procedure TFSession.ListViewKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
var
  I: Integer;
  MenuItem: TMenuItem;
begin
  if (not TListView(Sender).IsEditing()) then
    if ((Sender = ActiveListView) and (Key = VK_BACK) and Assigned(FNavigator.Selected.Parent)) then
      FNavigator.Selected := FNavigator.Selected.Parent
    else if ((Key = Ord('A')) and (Shift = [ssCtrl])) then
      MainAction('aESelectAll').Execute()
    else if ((Key = Ord('C')) and (Shift = [ssCtrl]) or (Key = VK_INSERT) and (Shift = [ssCtrl])) then
      begin aECopyExecute(Sender); Key := 0; end
    else if ((Key = Ord('V')) and (Shift = [ssCtrl]) or (Key = VK_INSERT) and (Shift = [ssShift])) then
      begin aEPasteExecute(Sender); Key := 0; end
    else if (Key = VK_RETURN) then
      if (Assigned(TListView(Sender).Selected) and Assigned(TListView(Sender).PopupMenu)) then
      begin
        MenuItem := nil;
        for I := 0 to TListView(Sender).PopupMenu.Items.Count - 1 do
          if (TListView(Sender).PopupMenu.Items[I].Default) then
            MenuItem := TListView(Sender).PopupMenu.Items[I];
        if (Assigned(MenuItem)) then
          begin MenuItem.Click(); Key := 0; end;
      end;
end;

procedure TFSession.ListViewEmpty(Sender: TObject);
var
  I: Integer;
  List: TList;
begin
  Wanted.Clear();

  if (ActiveListView.SelCount <= 1) then
    FNavigatorEmptyExecute(Sender)
  else if (Sender is TAction) then
  begin
    List := TList.Create();
    case (SelectedImageIndex) of
      iiServer:
        begin
          for I := 0 to ActiveListView.Items.Count - 1 do
            if (ActiveListView.Items[I].Selected and (ActiveListView.Items[I].ImageIndex in [iiDatabase, iiSystemDatabase])) then
              List.Add(ActiveListView.Items[I].Data);
          if (not Session.Update(List)) then
            Wanted.Action := TAction(Sender)
          else if (MsgBox(Preferences.LoadStr(405), Preferences.LoadStr(101), MB_YESNOCANCEL + MB_ICONQUESTION) = IDYES) then
            Session.EmptyDatabases(List);
        end;
      iiDatabase:
        begin
          for I := 0 to ActiveListView.Items.Count - 1 do
            if (ActiveListView.Items[I].Selected and (ActiveListView.Items[I].ImageIndex in [iiBaseTable])) then
              List.Add(ActiveListView.Items[I].Data);
          if (not Session.Update(List)) then
            Wanted.Action := TAction(Sender)
          else if (MsgBox(Preferences.LoadStr(406), Preferences.LoadStr(101), MB_YESNOCANCEL + MB_ICONQUESTION) = IDYES) then
            Session.DatabaseByName(SelectedDatabase).EmptyTables(List);
        end;
      iiBaseTable:
        if (MsgBox(Preferences.LoadStr(407), Preferences.LoadStr(101), MB_YESNOCANCEL + MB_ICONQUESTION) = IDYES) then
        begin
          for I := 0 to ActiveListView.Items.Count - 1 do
            if (ActiveListView.Items[I].Selected and (ActiveListView.Items[I].ImageIndex in [iiField, iiVirtualField])) then
              List.Add(ActiveListView.Items[I].Data);
          TSBaseTable(FNavigator.Selected.Data).EmptyFields(List);
        end;
    end;
    List.Free();
  end;
end;

procedure TFSession.ListViewEnter(Sender: TObject);
begin
  MainAction('aESelectAll').OnExecute := aESelectAllExecute;
  MainAction('aDEmpty').OnExecute := ListViewEmpty;

  aDCreate.ShortCut := VK_INSERT;
  aDDelete.ShortCut := VK_DELETE;

  if (Assigned(ActiveListView)) then
  begin
    ListViewSelectItem(ActiveListView, ActiveListView.Selected, Assigned(ActiveListView.Selected));
    if (not Assigned(ActiveListView.Selected) and (ActiveListView.Items.Count > 0)) then
      ActiveListView.Items[0].Focused := True;
  end;

  StatusBarRefresh();
end;

procedure TFSession.ListViewExit(Sender: TObject);
var
  Column: TListColumn;
  I: Integer;
  ListViewKind: TPAccount.TDesktop.TListViewKind; // Debug 2016-12-01
  Width: Integer; // Debug 2016-12-01
begin
  if (Sender is TListView) then
  begin
    // Debug 2016-12-20
    // Somewhere, Session demagged, but why and where???
    if (not Assigned(Session.Account)) then // Why looses Session the Account value???
      raise ERangeError.Create(SRangeError);

    for I := 0 to TListView(Sender).Columns.Count - 1 do
    begin
      // Debug 2016-12-02
      if (not Assigned(Session)) then
        raise ERangeError.Create(SRangeError);
      if (not (TObject(Session) is TSSession)) then
        try
          raise ERangeError.Create(SRangeError + ' ClassType: ' + TObject(Session).ClassName);
        except
          raise ERangeError.Create(SRangeError);
        end;
      if (not Assigned(Session.Account)) then
        raise ERangeError.Create(SRangeError);
      if (not (TObject(Session.Account) is TPAccount)) then
        try
          raise ERangeError.Create(SRangeError + ' ClassType: ' + TObject(Session.Account).ClassName);
        except
          raise ERangeError.Create(SRangeError);
        end;
      if (not Assigned(Session.Account.Desktop)) then
        raise ERangeError.Create(SRangeError);
      Column := TListView(Sender).Columns[I];
      Width := Column.Width;
      ListViewKind := ColumnWidthKindByListView(TListView(Sender));
      if (not (ListViewKind in [lkServer .. lkObjectSearch])) then
        raise ERangeError.Create(SRangeError + '(' + IntToStr(Ord(ListViewKind)) + ')');
      if (Length(Session.Account.Desktop.ColumnWidths) < 1 + Ord(ListViewKind)) then
        raise ERangeError.Create(SRangeError + ' ' + IntToStr(1 + Ord(ListViewKind)));
      if (I < 0) then
        raise ERangeError.Create(SRangeError);
      if (I >= Length(Session.Account.Desktop.ColumnWidths[lkServer])) then
        raise ERangeError.Create(SRangeError);
      Session.Account.Desktop.ColumnWidths[ListViewKind, I] := Width;
    end;
  end;

  MainAction('aESelectAll').OnExecute := nil;
  MainAction('aDEmpty').OnExecute := nil;

  MainAction('aFImportSQL').Enabled := False;
  MainAction('aFImportText').Enabled := False;
  MainAction('aFImportExcel').Enabled := False;
  MainAction('aFImportAccess').Enabled := False;
  MainAction('aFImportODBC').Enabled := False;
  MainAction('aFExportSQL').Enabled := False;
  MainAction('aFExportText').Enabled := False;
  MainAction('aFExportExcel').Enabled := False;
  MainAction('aFExportAccess').Enabled := False;
  MainAction('aFExportODBC').Enabled := False;
  MainAction('aFExportXML').Enabled := False;
  MainAction('aFExportHTML').Enabled := False;
  MainAction('aFExportPDF').Enabled := False;
  MainAction('aERename').Enabled := False;
  MainAction('aDCreateDatabase').Enabled := False;
  MainAction('aDCreateTable').Enabled := False;
  MainAction('aDCreateView').Enabled := False;
  MainAction('aDCreateProcedure').Enabled := False;
  MainAction('aDCreateFunction').Enabled := False;
  MainAction('aDCreateEvent').Enabled := False;
  MainAction('aDCreateTrigger').Enabled := False;
  MainAction('aDCreateKey').Enabled := False;
  MainAction('aDCreateField').Enabled := False;
  MainAction('aDCreateForeignKey').Enabled := False;
  MainAction('aDCreateUser').Enabled := False;
  MainAction('aDDeleteDatabase').Enabled := False;
  MainAction('aDDeleteTable').Enabled := False;
  MainAction('aDDeleteView').Enabled := False;
  MainAction('aDDeleteRoutine').Enabled := False;
  MainAction('aDDeleteEvent').Enabled := False;
  MainAction('aDDeleteKey').Enabled := False;
  MainAction('aDDeleteField').Enabled := False;
  MainAction('aDDeleteForeignKey').Enabled := False;
  MainAction('aDDeleteTrigger').Enabled := False;
  MainAction('aDDeleteUser').Enabled := False;
  MainAction('aDEditDatabase').Enabled := False;
  MainAction('aDEditTable').Enabled := False;
  MainAction('aDEditView').Enabled := False;
  MainAction('aDEditRoutine').Enabled := False;
  MainAction('aDEditEvent').Enabled := False;
  MainAction('aDEditKey').Enabled := False;
  MainAction('aDEditField').Enabled := False;
  MainAction('aDEditForeignKey').Enabled := False;
  MainAction('aDEditTrigger').Enabled := False;
  MainAction('aDEditUser').Enabled := False;
  MainAction('aDEditVariable').Enabled := False;
  MainAction('aDEmpty').Enabled := False;

  aDCreate.ShortCut := 0;
  aDDelete.ShortCut := 0;
  mlEProperties.ShortCut := 0;
end;

procedure TFSession.ListViewUpdate(const Event: TSSession.TEvent; const ListView: TListView; const Data: TCustomData = nil);

  function Compare(const Kind: TPAccount.TDesktop.TListViewKind; const Item1, Item2: TListItem): Integer;
  begin
    ListViewCompare(nil, Item1, Item2, LPARAM(@ListViewSortData[Kind]), Result);
  end;

  procedure UpdateItem(const Item: TListItem; const Data: TObject);
  var
    I: Integer;
    S: string;
    S2: string;
  begin
    // Debug 2016-11-17
    if (not Assigned(Data)) then
      raise ERangeError.Create(SRangeError);
    if (not Assigned(Item.Data)) then
      raise ERangeError.Create(SRangeError);
    // Debug 2016-11-23
    if (Item.Data <> Data) then
      raise ERangeError.Create(SRangeError);

    Assert(Item.Data = Data);

    Item.SubItems.BeginUpdate();
    Item.SubItems.Clear();

    if (Data is TSDatabase) then
    begin
      Item.GroupID := giDatabases;
      if (Data is TSSystemDatabase) then
        Item.ImageIndex := iiSystemDatabase
      else if (Data is TSDatabase) then
        Item.ImageIndex := iiDatabase
      else // Debug 2016-11-23
        raise ERangeError.Create(SRangeError);
      Item.Caption := TSDatabase(Data).Caption;

      Item.SubItems.Clear();
      if (TSDatabase(Data).Count < 0) then
        Item.SubItems.Add('')
      else
        Item.SubItems.Add(FormatFloat('#,##0', TSDatabase(Data).Count, LocaleFormatSettings));
      if ((TSDatabase(Data) is TSSystemDatabase) or (TSDatabase(Data).DataSize <= 0)) then
        Item.SubItems.Add('')
      else
        Item.SubItems.Add(SizeToStr(TSDatabase(Data).DataSize));
      if (TSDatabase(Data).Created <= 0) then
        Item.SubItems.Add('')
      else
        Item.SubItems.Add(SysUtils.DateToStr(TSDatabase(Data).Created, LocaleFormatSettings));
      if ((TSDatabase(Data).Charset <> '') and (TSDatabase(Data).Charset <> Session.Charset)) then
        S := TSDatabase(Data).Charset
      else
        S := '';
      if ((TSDatabase(Data).Collation <> '') and (TSDatabase(Data).Collation <> Session.Collation)) then
      begin
        if (S <> '') then S := S + ', ';
        S := S + TSDatabase(Data).Collation;
      end;
      if (TSDatabase(Data) is TSSystemDatabase) then
        Item.SubItems.Add('')
      else
        Item.SubItems.Add(S);
    end
    else if (Data is TSTable) then
    begin
      Item.GroupID := giTables;
      if (Data is TSBaseTable) then
        Item.ImageIndex := iiBaseTable
      else if (Data is TSView) then
        Item.ImageIndex := iiView
      else if (Data is TSSystemView) then
        Item.ImageIndex := iiSystemView;
      Item.Caption := TSTable(Data).Caption;
      if ((TSTable(Data) is TSBaseTable) and Assigned(TSBaseTable(Data).Engine)) then
        Item.SubItems.Add(TSBaseTable(Data).Engine.Name)
      else if ((TSTable(Data) is TSView)) then
        Item.SubItems.Add(Preferences.LoadStr(738))
      else
        Item.SubItems.Add('');
      if (TSTable(Data) is TSBaseTable) then
        if (not TSBaseTable(Data).ValidStatus or (TSBaseTable(Data).RecordCount < 0) or not Assigned(TSBaseTable(Data).Engine)) then
          Item.SubItems.Add('')
        else if (TSBaseTable(Data).Engine.IsInnoDB) then
          Item.SubItems.Add(FormatFloat('~#,##0', TSBaseTable(Data).RecordCount, LocaleFormatSettings))
        else
          Item.SubItems.Add(FormatFloat('#,##0', TSBaseTable(Data).RecordCount, LocaleFormatSettings))
      else
        Item.SubItems.Add('');
      if ((TSTable(Data) is TSView) and (TSView(Data).Stmt <> '')) then
        Item.SubItems.Add(SizeToStr(Length(TSView(Data).Stmt)))
      else if ((TSTable(Data) is TSBaseTable) and TSBaseTable(Data).ValidStatus) then
        Item.SubItems.Add(SizeToStr(TSBaseTable(Data).DataSize))
      else
        Item.SubItems.Add('');
      if (not (TSTable(Data) is TSBaseTable) or not TSBaseTable(Data).ValidStatus or (TSBaseTable(Data).Updated <= 0)) then
        Item.SubItems.Add('')
      else
        Item.SubItems.Add(SysUtils.DateTimeToStr(TSBaseTable(Data).Updated, LocaleFormatSettings));
      S := '';
      if ((TSTable(Data) is TSBaseTable) and (TSBaseTable(Data).Charset <> '') and (TSBaseTable(Data).Charset <> TSBaseTable(Data).Database.Charset)) then
        S := S + TSBaseTable(Data).Charset;
      if ((TSTable(Data) is TSBaseTable) and (TSBaseTable(Data).Collation <> '') and (TSBaseTable(Data).Collation <> TSBaseTable(Data).Database.Collation)) then
      begin
        if (S <> '') then S := S + ', ';
        S := S + TSBaseTable(Data).Collation;
      end;
      Item.SubItems.Add(S);
      if (Data is TSBaseTable) then
        Item.SubItems.Add(TSBaseTable(Data).Comment);
    end
    else if (Data is TSRoutine) then
    begin
      Item.GroupID := giRoutines;
      if (Data is TSProcedure) then
        Item.ImageIndex := iiProcedure
      else
        Item.ImageIndex := iiFunction;
      Item.Caption := TSRoutine(Data).Caption;
      case (TSRoutine(Data).RoutineType) of
        TSRoutine.TRoutineType.rtProcedure: Item.SubItems.Add(Preferences.LoadStr(768));
        TSRoutine.TRoutineType.rtFunction: Item.SubItems.Add(Preferences.LoadStr(769));
      end;
      Item.SubItems.Add('');
      if (TSRoutine(Data).Stmt = '') then
        Item.SubItems.Add('')
      else
        Item.SubItems.Add(SizeToStr(Length(TSRoutine(Data).Stmt)));
      if (TSRoutine(Data).Modified = 0) then
        Item.SubItems.Add('')
      else
        Item.SubItems.Add(SysUtils.DateTimeToStr(TSRoutine(Data).Modified, LocaleFormatSettings));
      if (not Assigned(TSRoutine(Data).FunctionResult) or (TSRoutine(Data).FunctionResult.Charset = '') or (TSRoutine(Data).FunctionResult.Charset = TSRoutine(Data).Database.Charset)) then
        Item.SubItems.Add('')
      else
        Item.SubItems.Add(TSRoutine(Data).FunctionResult.Charset);
      Item.SubItems.Add(TSRoutine(Data).Comment);
    end
    else if (Data is TSEvent) then
    begin
      Item.GroupID := giEvents;
      Item.ImageIndex := iiEvent;
      Item.Caption := TSEvent(Data).Caption;
      Item.SubItems.Add(Preferences.LoadStr(812));
      Item.SubItems.Add('');
      if (TSEvent(Data).Stmt = '') then
        Item.SubItems.Add('')
      else
        Item.SubItems.Add(SizeToStr(Length(TSEvent(Data).Stmt)));
      if (TSEvent(Data).Updated = 0) then
        Item.SubItems.Add('')
      else
        Item.SubItems.Add(SysUtils.DateTimeToStr(TSEvent(Data).Updated, LocaleFormatSettings));
      Item.SubItems.Add('');
      Item.SubItems.Add(TSEvent(Data).Comment);
    end
    else if (Data is TSKey) then
    begin
      Item.GroupID := giKeys;
      Item.ImageIndex := iiKey;
      Item.Caption := TSKey(Data).Caption;
      S := '';
      for I := 0 to TSKey(Data).Columns.Count - 1 do
      begin
        if (S <> '') then S := S + ',';
        S := S + TSKey(Data).Columns[I].Field.Name;
        if (TSKey(Data).Columns.Column[I].Length > 0) then
          S := S + '(' + IntToStr(TSKey(Data).Columns[I].Length) + ')';
      end;
      Item.SubItems.Add(S);
      Item.SubItems.Add('');
      Item.SubItems.Add('');
      if (TSKey(Data).Unique) then
        Item.SubItems.Add('unique')
      else if (TSKey(Data).Fulltext) then
        Item.SubItems.Add('fulltext')
      else
        Item.SubItems.Add('');
      if (Session.Connection.MySQLVersion >= 50503) then
        Item.SubItems.Add(TSKey(Data).Comment);
    end
    else if (Data is TSBaseTableField) then
    begin
      Item.GroupID := giFields;
      Item.Caption := TSBaseTableField(Data).Caption;
      Item.SubItems.Add(SQLUnescape(TSBaseTableField(Data).DBTypeStr()));
      if (TSBaseTableField(Data).NullAllowed) then
        Item.SubItems.Add(Preferences.LoadStr(74))
      else
        Item.SubItems.Add(Preferences.LoadStr(75));
      if (TSBaseTableField(Data).FieldKind = mkReal) then
      begin
        if (TSBaseTableField(Data).AutoIncrement) then
          Item.SubItems.Add('<auto_increment>')
        else if (TSBaseTableField(Data).Default = 'NULL') then
          Item.SubItems.Add('<' + Preferences.LoadStr(71) + '>')
        else if (TSBaseTableField(Data).Default = 'CURRENT_TIMESTAMP') then
          Item.SubItems.Add('<INSERT-TimeStamp>')
        else
          Item.SubItems.Add(TSBaseTableField(Data).UnescapeValue(TSBaseTableField(Data).Default));
        S := '';
        if (TSBaseTableField(Data).FieldType in TextFieldTypes) then
        begin
          if ((TSBaseTableField(Data).Charset <> '') and (TSBaseTableField(Data).Charset <> TSBaseTableField(Data).Table.Charset)) then
            S := S + TSBaseTableField(Data).Charset;
          if ((TSBaseTableField(Data).Collation <> '') and (TSBaseTableField(Data).Collation <> TSBaseTableField(Data).Table.Collation)) then
          begin
            if (S <> '') then S := S + ', ';
            S := S + TSBaseTableField(Data).Collation;
          end;
        end;
        Item.SubItems.Add(S);
        if (Session.Connection.MySQLVersion >= 40100) then
          Item.SubItems.Add(TSBaseTableField(Data).Comment);
        Item.ImageIndex := iiField;
      end
      else if (TSBaseTableField(Data).FieldKind = mkVirtual) then
      begin
        Item.SubItems.Add(TSBaseTableField(Data).Expression);
        Item.SubItems.Add('');
        Item.SubItems.Add(TSBaseTableField(Data).Comment);
        Item.ImageIndex := iiVirtualField;
      end;
    end
    else if (Data is TSForeignKey) then
    begin
      Item.GroupID := giForeignKeys;
      Item.ImageIndex := iiForeignKey;
      Item.Caption := TSForeignKey(Data).Caption;
      Item.SubItems.Add(TSForeignKey(Data).DBTypeStr());
      Item.SubItems.Add('');
      Item.SubItems.Add('');
      S := '';
      if (TSForeignKey(Data).OnDelete = dtCascade) then S := 'cascade on delete';
      if (TSForeignKey(Data).OnDelete = dtSetNull) then S := 'set NULL on delete';
      if (TSForeignKey(Data).OnDelete = dtSetDefault) then S := 'set default on delete';
      if (TSForeignKey(Data).OnDelete = dtNoAction) then S := 'no action on delete';
      S2 := '';
      if (TSForeignKey(Data).OnUpdate = utCascade) then S2 := 'cascade on update';
      if (TSForeignKey(Data).OnUpdate = utSetNull) then S2 := 'set NULL on update';
      if (TSForeignKey(Data).OnUpdate = utSetDefault) then S2 := 'set default on update';
      if (TSForeignKey(Data).OnUpdate = utNoAction) then S2 := 'no action on update';
      if (S <> '') and (S2 <> '') then S := S + ', ';
      S := S + S2;
      Item.SubItems.Add(S);
    end
    else if (Data is TSTrigger) then
    begin
      Item.GroupID := giTriggers;
      Item.ImageIndex := iiTrigger;
      Item.Caption := TSTrigger(Data).Caption;
      S := '';
      case (TSTrigger(Data).Timing) of
        ttBefore: S := S + 'before ';
        ttAfter: S := S + 'after ';
      end;
      case (TSTrigger(Data).Event) of
        teInsert: S := S + 'insert';
        teUpdate: S := S + 'update';
        teDelete: S := S + 'delete';
      end;
      Item.SubItems.Add(S);
    end
    else if (Data is TSTableField) then
    begin
      Item.GroupID := giFields;
      if (TSTableField(Data).Table is TSSystemView) then
        Item.ImageIndex := iiSystemViewField
      else if (Data is TSViewField) then
        Item.ImageIndex := iiViewField
      else if (TSTableField(Data).FieldKind = mkVirtual) then
        Item.ImageIndex := iiVirtualField
      else
        Item.ImageIndex := iiField;
      Item.Caption := TSTableField(Data).Caption;
      if (TSTableField(Data).FieldType <> mfUnknown) then
      begin
        Item.SubItems.Add(TSTableField(Data).DBTypeStr());
        if (TSTableField(Data).NullAllowed) then
          Item.SubItems.Add(Preferences.LoadStr(74))
        else
          Item.SubItems.Add(Preferences.LoadStr(75));
        if (TSTableField(Data).AutoIncrement) then
          Item.SubItems.Add('<auto_increment>')
        else
          Item.SubItems.Add(TSTableField(Data).Default);
        if (TSTableField(Data).Charset <> TSTableField(Data).Table.Database.Charset) then
          Item.SubItems.Add(TSTableField(Data).Charset);
      end;
    end
    else if (Data is TSProcesses) then
    begin
      Item.GroupID := giSystemTools;
      Item.ImageIndex := iiProcesses;
      if (TSProcesses(Data).Count > 0) then
        Item.SubItems.Add(FormatFloat('#,##0', TSProcesses(Data).Count, LocaleFormatSettings));
    end
    else if (Data is TSProcess) then
    begin
      Item.GroupID := giProcesses;
      Item.ImageIndex := iiProcess;
      Item.Caption := IntToStr(TSProcess(Data).ThreadId);
      Item.SubItems.Add(TSProcess(Data).UserName);
      Item.SubItems.Add(TSProcess(Data).Host);
      Item.SubItems.Add(TSProcess(Data).DatabaseName);
      Item.SubItems.Add(TSProcess(Data).Command);
      Item.SubItems.Add(SQLStmtToCaption(TSProcess(Data).SQL, 30));
      if (TSProcess(Data).Time = 0) then
        Item.SubItems.Add('')
      else
        Item.SubItems.Add(ExecutionTimeToStr(TSProcess(Data).Time));
      Item.SubItems.Add(TSProcess(Data).State);
    end
    else if (Data is TSUsers) then
    begin
      Item.GroupID := giSystemTools;
      Item.ImageIndex := iiUsers;
      if (Session.Users.Count >= 0) then
        Item.SubItems.Add(FormatFloat('#,##0', Session.Users.Count, LocaleFormatSettings))
      else
        Item.SubItems.Add('???');
    end
    else if (Data is TSUser) then
    begin
      Item.GroupID := giUsers;
      Item.ImageIndex := iiUser;
      Item.Caption := TSUser(Data).Caption;
    end
    else if (Data is TSVariables) then
    begin
      Item.GroupID := giSystemTools;
      Item.ImageIndex := iiVariables;
      Item.SubItems.Add(FormatFloat('#,##0', Session.Variables.Count, LocaleFormatSettings));
    end
    else if (Data is TSVariable) then
    begin
      Item.GroupID := giVariables;
      Item.ImageIndex := iiVariable;
      Item.Caption := TSVariable(Data).Caption;
      Item.SubItems.Add(TSVariable(Data).Value);
    end;

    Item.SubItems.EndUpdate();
  end;

  function InsertOrUpdateItem(const Kind: TPAccount.TDesktop.TListViewKind; const Data: TObject): TListItem;
  var
    {$IFNDEF Debug}
    GroupID: Integer;
    I: Integer;
    {$ENDIF}
    Item: TListItem;
    Index: Integer;
    Left: Integer;
    Mid: Integer;
    ReorderGroup: Boolean;
    Right: Integer;
  begin
    Index := 0;
    while ((Index < ListView.Items.Count) and (ListView.Items[Index].Data <> Data)) do
      Inc(Index);

    if ((0 < ListView.Items.Count) and (ListView.Items.Count = Index)) then
    begin
      Item := TListItem.Create(ListView.Items);
      Item.Data := Data;
      UpdateItem(Item, Data);

      Left := 0;
      Right := ListView.Items.Count - 1;
      while (Left <= Right) do
      begin
        Mid := (Right - Left) div 2 + Left;
        case (Compare(Kind, ListView.Items[Mid], Item)) of
          -1: begin Left := Mid + 1; Index := Mid + 1; end;
          0: raise ERangeError.CreateFmt(SRangeError + ': %s / %s', [TSItem(Data).Name, TSItem(Data).ClassName]);
          1: begin Right := Mid - 1; Index := Mid; end;
        end;
      end;

      Item.Free();
    end;

    if (Index = ListView.Items.Count) then
    begin
      Result := ListView.Items.Add();
      Result.Data := Data;
      ReorderGroup := False;
    end
    else if (ListView.Items[Index].Data <> Data) then
    begin
      Result := ListView.Items.Insert(Index);
      Result.Data := Data;
      ReorderGroup := True;
    end
    else
    begin
      Result := ListView.Items[Index];
      ReorderGroup := not (Data is TSItem) or (TSItem(Data).Caption <> Result.Caption);
    end;
    UpdateItem(Result, Data);

    if (ReorderGroup and ListView.GroupView) then
    begin
      {$IFDEF Debug}
      // Is this code needed in Delphi XE4?
      {$ELSE}
      GroupID := Result.GroupID;
      for I := Result.Index + 1 to ListView.Items.Count - 1 do
        if (ListView.Items[I].GroupID = GroupID) then
        begin
          ListView.Items[I].GroupID := -1;
          ListView.Items[I].GroupID := GroupID; // Why is this needed (in Delphi XE2)???
        end;
      {$ENDIF}
    end;
  end;

  function AddItem(const Kind: TPAccount.TDesktop.TListViewKind; const Data: TSItem): TListItem;
  begin
    Result := ListView.Items.Add();
    Result.Data := Data;
    UpdateItem(Result, Data);
  end;

  procedure UpdateGroup(const Kind: TPAccount.TDesktop.TListViewKind; const GroupID: Integer; const SItems: TSItems);
  var
    Add: Boolean;
    ColumnWidths: array [0..7] of Integer;
    Count: Integer;
    Data: Pointer; // Debug 2016-12-26
    Header: string;
    I: Integer;
    Index: Integer;
    Item: TListItem;
    ItemFocused: Boolean;
    ItemSelected: Boolean;
    J: Integer;
    NewIndex: Integer;
    S: string;
    Table: TSBaseTable; // Debug 2016-12-26
  begin
    case (Event.EventType) of
      etItemsValid:
        begin
          ListView.Columns.BeginUpdate();
          ListView.Items.BeginUpdate();
          ListView.DisableAlign();

          for I := 0 to ListView.Columns.Count - 1 do
          begin
            ColumnWidths[I] := ListView.Columns[I].Width;
            ListView.Columns[I].Width := 50; // Make soure no auto column width will be calculated for each item
          end;

          for I := ListView.Items.Count - 1 downto 0 do
            if ((ListView.Items[I].GroupID = GroupID) and (SItems.IndexOf(ListView.Items[I].Data) < 0)) then
              ListView.Items.Delete(I);

          Add := (ListView.Items.Count = 0) and (ListViewSortData[Kind].Index = 0) and (ListViewSortData[Kind].Order = 1);
          for I := 0 to SItems.Count - 1 do
            if (not (SItems is TSTriggers) or (TSTriggers(SItems)[I].Table = TObject(ListView.Tag))) then
              if (not Add) then
                InsertOrUpdateItem(Kind, SItems[I])
              else
                AddItem(Kind, SItems[I]);

          for I := 0 to ListView.Columns.Count - 1 do
            if ((Kind = lkProcesses) and (I = 5)) then
              ListView.Columns[I].Width := Preferences.GridMaxColumnWidth
            else
              ListView.Columns[I].Width := ColumnWidths[I];

          ListView.EnableAlign();
          ListView.Items.EndUpdate();
          ListView.Columns.EndUpdate();
        end;
      etItemValid:
        begin
          if (Event.Item is TSTrigger) then
          begin
            Add := True;
            for I := 0 to ListView.Items.Count - 1 do
              if (ListView.Items[I].Data = Event.Item) then
              begin
                UpdateItem(ListView.Items[I], Event.Item);
                Add := False;
              end;
            if (Add) then
              InsertOrUpdateItem(Kind, Event.Item);
          end
          else
            for I := 0 to ListView.Items.Count - 1 do
              if (ListView.Items[I].Data = Event.Item) then
                UpdateItem(ListView.Items[I], Event.Item);
        end;
      etItemCreated:
        begin
          Item := InsertOrUpdateItem(Kind, Event.Item);
          if (not Assigned(ListView.Selected)) then
          begin
            Item.Selected := True;
            Item.Focused := True;
          end;
        end;
      etItemAltered:
        begin
          Index := 0;
          while ((Index < ListView.Items.Count) and (ListView.Items[Index].Data <> Event.Item)) do
            Inc(Index);
          if (Index = ListView.Items.Count) then
            InsertOrUpdateItem(Kind, Event.Item)
          else if (ListView.Items[Index].Caption = Event.Item.Caption) then
            UpdateItem(ListView.Items[Index], Event.Item)
          else
          begin
            ItemSelected := ListView.Items[Index].Selected;
            ItemFocused := ListView.Items[Index].Focused;
            ListView.Items.Delete(Index);
            Item := InsertOrUpdateItem(Kind, Event.Item);
            Item.Selected := ItemSelected;
            Item.Focused := ItemFocused;
          end;
        end;
      etItemDropped:
        begin
          for I := ListView.Items.Count - 1 downto 0 do
            if (ListView.Items[I].Data = Event.Item) then
              ListView.Items.Delete(I);
          if ((Kind = lkTable) and (Event.Items is TSBaseTableFields)) then
          begin
            for I := ListView.Items.Count - 1 downto 0 do
              if ((TObject(ListView.Items[I].Data) is TSKey) and (TSBaseTable(TSBaseTableFields(Event.Items).Table).Keys.IndexOf(ListView.Items[I].Data) < 0)) then
                ListView.Items.Delete(I);
            for I := ListView.Items.Count - 1 downto 0 do
              if ((TObject(ListView.Items[I].Data) is TSField) and (TSBaseTable(TSBaseTableFields(Event.Items).Table).Fields.IndexOf(ListView.Items[I].Data) < 0)) then
                ListView.Items.Delete(I);
            for I := ListView.Items.Count - 1 downto 0 do
              if ((TObject(ListView.Items[I].Data) is TSForeignKey) and (TSBaseTable(TSBaseTableFields(Event.Items).Table).ForeignKeys.IndexOf(ListView.Items[I].Data) < 0)) then
                ListView.Items.Delete(I);
          end;
        end;
    end;

    if (Event.EventType in [etItemsValid, etItemCreated, etItemDropped]) then
      case (GroupID) of
        giDatabases:
          SetListViewGroupHeader(ListView, GroupID, Preferences.LoadStr(265) + ' (' + IntToStr(Event.Items.Count) + ')');
        giTables:
          begin
            Header := Preferences.LoadStr(234);
            if (Session.Connection.MySQLVersion >= 50001) then
              Header := Header + ' + ' + Preferences.LoadStr(873);
            Header := Header + ' (' + IntToStr(Event.Items.Count) + ')';
            SetListViewGroupHeader(ListView, GroupID, Header);
          end;
        giRoutines:
          SetListViewGroupHeader(ListView, GroupID, Preferences.LoadStr(874) + ' + ' + Preferences.LoadStr(875) + ' (' + IntToStr(Event.Items.Count) + ')');
        giEvents:
          SetListViewGroupHeader(ListView, GroupID, Preferences.LoadStr(876) + ' (' + IntToStr(Event.Items.Count) + ')');
        giKeys:
          SetListViewGroupHeader(ListView, GroupID, Preferences.LoadStr(458) + ' (' + IntToStr(TSBaseTable(ListView.Tag).Keys.Count) + ')');
        giFields:
          SetListViewGroupHeader(ListView, GroupID, Preferences.LoadStr(253) + ' (' + IntToStr(TSTable(ListView.Tag).Fields.Count) + ')');
        giForeignKeys:
          SetListViewGroupHeader(ListView, GroupID, Preferences.LoadStr(459) + ' (' + IntToStr(TSBaseTable(ListView.Tag).ForeignKeys.Count) + ')');
        giTriggers:
          begin
            Count := 0;
            for I := 0 to TSTriggers(SItems).Count - 1 do
              if (TSTriggers(SItems)[I].Table = TObject(ListView.Tag)) then
              begin
                InsertOrUpdateItem(Kind, TSTriggers(SItems)[I]);
                Inc(Count);
              end;
            SetListViewGroupHeader(ListView, GroupID, Preferences.LoadStr(797) + ' (' + IntToStr(Count) + ')');
          end;
        giProcesses:
          SetListViewGroupHeader(ListView, GroupID, Preferences.LoadStr(24) + ' (' + IntToStr(Session.Processes.Count) + ')');
        giUsers:
          SetListViewGroupHeader(ListView, GroupID, Preferences.LoadStr(561) + ' (' + IntToStr(Session.Users.Count) + ')');
        giVariables:
          SetListViewGroupHeader(ListView, GroupID, Preferences.LoadStr(22) + ' (' + IntToStr(Session.Variables.Count) + ')');
      end;
  end;

  procedure UpdateObjectSearch();
  var
    DatabaseCount: Integer;
    EventCount: Integer;
    FieldCount: Integer;
    Header: string;
    I: Integer;
    Item: TListItem;
    S: string;
    TableCount: Integer;
    TriggerCount: Integer;
    RoutineCount: Integer;
    UserCount: Integer;
  begin
    for I := ListView.Items.Count to Event.Items.Count - 1 do
    begin
      Item := nil;
      if (Event.Items[I] is TSDatabase) then
      begin
        Item := ListView.Items.Add();
        Item.SubItems.Add(Preferences.LoadStr(38));
        Item.SubItems.Add('');
        if (TSDatabase(Event.Items[I]).Updated = 0) then
          Item.SubItems.Add('')
        else
          Item.SubItems.Add(SysUtils.DateTimeToStr(TSDatabase(Event.Items[I]).Updated, LocaleFormatSettings));
        Item.SubItems.Add('');
        Item.GroupID := giDatabases;
      end
      else if (Event.Items[I] is TSBaseTable) then
      begin
        Item := ListView.Items.Add();
        if (not Assigned(TSBaseTable(Event.Items[I]).Engine)) then
          Item.SubItems.Add('')
        else
          Item.SubItems.Add(TSBaseTable(Event.Items[I]).Engine.Name);
        Item.SubItems.Add(TSBaseTable(Event.Items[I]).Database.Name);
        if (TSBaseTable(Event.Items[I]).Updated = 0) then
          Item.SubItems.Add('')
        else
          Item.SubItems.Add(SysUtils.DateTimeToStr(TSBaseTable(Event.Items[I]).Updated, LocaleFormatSettings));
        Item.SubItems.Add(TSBaseTable(Event.Items[I]).Comment);
        Item.GroupID := giTables;
      end
      else if (Event.Items[I] is TSView) then
      begin
        Item := ListView.Items.Add();
        Item.SubItems.Add(Preferences.LoadStr(738));
        Item.SubItems.Add(TSView(Event.Items[I]).Database.Name);
        Item.SubItems.Add('');
        Item.SubItems.Add('');
        Item.GroupID := giTables;
      end
      else if (Event.Items[I] is TSRoutine) then
      begin
        Item := ListView.Items.Add();
        if (TSRoutine(Event.Items[I]).RoutineType = rtProcedure) then
          Item.SubItems.Add(Preferences.LoadStr(768))
        else
          Item.SubItems.Add(Preferences.LoadStr(769));
        Item.SubItems.Add(TSRoutine(Event.Items[I]).Database.Name);
        Item.SubItems.Add(SysUtils.DateTimeToStr(TSRoutine(Event.Items[I]).Created, LocaleFormatSettings));
        Item.SubItems.Add(TSRoutine(Event.Items[I]).Comment);
        Item.GroupID := giRoutines;
      end
      else if (Event.Items[I] is TSEvent) then
      begin
        Item := ListView.Items.Add();
        Item.SubItems.Add(Preferences.LoadStr(793));
        Item.SubItems.Add(TSEvent(Event.Items[I]).Database.Name);
        Item.SubItems.Add(SysUtils.DateTimeToStr(TSEvent(Event.Items[I]).Updated, LocaleFormatSettings));
        Item.SubItems.Add(TSEvent(Event.Items[I]).Comment);
        Item.GroupID := giEvents;
      end
      else if (Event.Items[I] is TSTableField) then
      begin
        Item := ListView.Items.Add();
        if (Event.Items[I] is TSViewField) then
          Item.SubItems.Add(TSViewField(Event.Items[I]).DBTypeStr())
        else if (TSBaseTableField(Event.Items[I]).FieldKind = mkReal) then
          Item.SubItems.Add(SQLUnescape(TSBaseTableField(Event.Items[I]).DBTypeStr()))
        else // mkVirtual
          Item.SubItems.Add(TSBaseTableField(Data).Expression);
        Item.SubItems.Add(TSTableField(Event.Items[I]).Table.Database.Name + '.' + TSTableField(Event.Items[I]).Table.Name);
        Item.SubItems.Add('');
        if (not (Event.Items[I] is TSBaseTableField)) then
          Item.SubItems.Add('')
        else
          Item.SubItems.Add(TSBaseTableField(Event.Items[I]).Comment);
        Item.GroupID := giFields;
      end
      else if (Event.Items[I] is TSTrigger) then
      begin
        Item := ListView.Items.Add();
        S := '';
        case (TSTrigger(Event.Items[I]).Timing) of
          ttBefore: S := S + 'before ';
          ttAfter: S := S + 'after ';
        end;
        case (TSTrigger(Event.Items[I]).Event) of
          teInsert: S := S + 'insert';
          teUpdate: S := S + 'update';
          teDelete: S := S + 'delete';
        end;
        Item.SubItems.Add(S);
        Item.SubItems.Add(TSTrigger(Event.Items[I]).Table.Database.Name + '.' + TSTrigger(Event.Items[I]).Table.Name);
        if (TSTrigger(Event.Items[I]).Created = 0) then
          Item.SubItems.Add('')
        else
          Item.SubItems.Add(SysUtils.DateTimeToStr(TSTrigger(Event.Items[I]).Created, LocaleFormatSettings));
        Item.SubItems.Add('');
        Item.GroupID := giTriggers;
      end
      else if (Event.Items[I] is TSUser) then
      begin
        Item := ListView.Items.Add();
        Item.SubItems.Add(Preferences.LoadStr(561));
        Item.GroupID := giUsers;
      end;
      if (Assigned(Item)) then
      begin
        Item.Caption := Event.Items[I].Name;
        Item.ImageIndex := ImageIndexByData(Event.Items[I]);
        Item.Data := Event.Items[I];
      end;
    end;

    DatabaseCount := 0;
    TableCount := 0;
    RoutineCount := 0;
    EventCount := 0;
    FieldCount := 0;
    TriggerCount := 0;
    UserCount := 0;
    for I := 0 to ListView.Items.Count - 1 do
      case (ListView.Items[I].GroupID) of
        giDatabases: Inc(DatabaseCount);
        giTables: Inc(TableCount);
        giRoutines: Inc(RoutineCount);
        giEvents: Inc(EventCount);
        giFields: Inc(FieldCount);
        giTriggers: Inc(TriggerCount);
        giUsers: Inc(UserCount);
      end;

    if (DatabaseCount > 0) then
      SetListViewGroupHeader(ListView, giDatabases, Preferences.LoadStr(265) + ' (' + IntToStr(DatabaseCount) + ')');
    if (TableCount > 0) then
      SetListViewGroupHeader(ListView, giTables, Preferences.LoadStr(234) + ' + ' + Preferences.LoadStr(873) + ' (' + IntToStr(TableCount) + ')');
    if (RoutineCount > 0) then
      SetListViewGroupHeader(ListView, giRoutines, Preferences.LoadStr(874) + ' + ' + Preferences.LoadStr(875) + ' (' + IntToStr(RoutineCount) + ')');
    if (EventCount > 0) then
      SetListViewGroupHeader(ListView, giEvents, Preferences.LoadStr(876) + ' (' + IntToStr(EventCount) + ')');
    if (FieldCount > 0) then
      SetListViewGroupHeader(ListView, giFields, Preferences.LoadStr(253) + ' (' + IntToStr(FieldCount) + ')');
    if (TriggerCount > 0) then
      SetListViewGroupHeader(ListView, giTriggers, Preferences.LoadStr(797) + ' (' + IntToStr(TriggerCount) + ')');
    if (UserCount > 0) then
      SetListViewGroupHeader(ListView, giUsers, Preferences.LoadStr(561) + ' (' + IntToStr(UserCount) + ')');
  end;

var
  ChangingEvent: TLVChangingEvent;
  I: Integer;
  Kind: TPAccount.TDesktop.TListViewKind;
  Table: TSTable;
begin
  if (Assigned(ListView) and (Assigned(Event.Items)
    or (Event.Sender is TSTable)
    or (Event.Sender is TSObjectSearch))) then
  begin
    ChangingEvent := ListView.OnChanging;
    ListView.OnChanging := nil;

    if (Event.Sender is TSObjectSearch) then
      UpdateObjectSearch()
    else
    begin
      Kind := ColumnWidthKindByListView(ListView);

      case (Kind) of
        lkServer:
          begin
            if (ListView.Items.Count = 0) then
            begin
              if (Assigned(Session.Processes)) then
                InsertOrUpdateItem(Kind, Session.Processes);
              if (Assigned(Session.Users)) then
                InsertOrUpdateItem(Kind, Session.Users);
              if (Assigned(Session.Variables)) then
                InsertOrUpdateItem(Kind, Session.Variables);
              ListViewInitialize(ListView);
            end;

            if (Event.Items is TSDatabases) then
              UpdateGroup(Kind, giDatabases, Event.Items)
            else if ((Event.Items is TSProcesses) or (Event.Items is TSUsers) or (Event.Items is TSVariables)) then
            begin
              for I := 0 to ListView.Items.Count - 1 do
                if (ListView.Items[I].Data = Event.Items) then
                  UpdateItem(ListView.Items[I], Event.Items);
            end;
          end;
        lkDatabase:
          begin
            if (Event.Items is TSTables) then
              UpdateGroup(Kind, giTables, Event.Items)
            else if (Event.Items is TSRoutines) then
              UpdateGroup(Kind, giRoutines, Event.Items)
            else if (Event.Items is TSEvents) then
              UpdateGroup(Kind, giEvents, Event.Items);
          end;
        lkTable:
          begin
            if ((Event.Sender is TSTable) or (Event.Item is TSTrigger) and (TSTrigger(Event.Item).Table = Pointer(ListView.Tag))) then
            begin
              if (Event.Item is TSTrigger) then
                Table := TSTrigger(Event.Item).Table
              else
                Table := TSTable(Event.Sender);
              ListView.Items.BeginUpdate();
              ListView.DisableAlign();
              if (Table is TSBaseTable) then
                UpdateGroup(Kind, giKeys, TSBaseTable(Table).Keys);
              UpdateGroup(Kind, giFields, Table.Fields);
              if ((Table is TSBaseTable) and Assigned(TSBaseTable(Table).ForeignKeys)) then
                UpdateGroup(Kind, giForeignKeys, TSBaseTable(Table).ForeignKeys);
              if ((Table is TSBaseTable) and Assigned(TSBaseTable(Table).Database.Triggers)) then
                UpdateGroup(Kind, giTriggers, TSBaseTable(Table).Database.Triggers);
              ListView.EnableAlign();
              ListView.Items.EndUpdate();
            end
            else if ((Event.Sender is TSDatabase) and (Event.Items is TSTriggers)) then
              UpdateGroup(Kind, giTriggers, Event.Items);
          end;
        lkProcesses:
          UpdateGroup(Kind, giProcesses, Event.Items);
        lkUsers:
          UpdateGroup(Kind, giUsers, Event.Items);
        lkVariables:
          UpdateGroup(Kind, giVariables, Event.Items);
      end;
    end;

    if ((Window.ActiveControl = ListView) and Assigned(ListView.OnSelectItem)) then
      ListView.OnSelectItem(nil, ListView.Selected, Assigned(ListView.Selected));

    ListView.OnChanging := ChangingEvent;
  end;
end;

procedure TFSession.ListViewSelectItem(Sender: TObject; Item: TListItem;
  Selected: Boolean);
var
  I: Integer;
  ListView: TListView;
  Msg: TMsg;
begin
  if (not (Sender is TListView)) then
    ListView := nil
  else
    ListView := TListView(Sender);

  if (Assigned(ListView) and (not (PeekMessage(Msg, 0, 0, 0, PM_NOREMOVE) and (Msg.Message = WM_MOUSEMOVE) and (Msg.wParam = MK_LBUTTON)) or (ListView.SelCount <= 1))) then
  begin
    MainAction('aFImportSQL').Enabled := False;
    MainAction('aFImportText').Enabled := False;
    MainAction('aFImportExcel').Enabled := False;
    MainAction('aFImportAccess').Enabled := False;
    MainAction('aFImportODBC').Enabled := False;
    MainAction('aFExportSQL').Enabled := False;
    MainAction('aFExportText').Enabled := False;
    MainAction('aFExportExcel').Enabled := False;
    MainAction('aFExportAccess').Enabled := False;
    MainAction('aFExportODBC').Enabled := False;
    MainAction('aFExportXML').Enabled := False;
    MainAction('aFExportHTML').Enabled := False;
    MainAction('aFExportPDF').Enabled := False;
    MainAction('aECopy').Enabled := False;
    MainAction('aEPaste').Enabled := False;
    MainAction('aERename').Enabled := False;
    MainAction('aESelectAll').Enabled := ListView.Items.Count > 1;
    MainAction('aDCreateDatabase').Enabled := False;
    MainAction('aDCreateTable').Enabled := False;
    MainAction('aDCreateView').Enabled := False;
    MainAction('aDCreateProcedure').Enabled := False;
    MainAction('aDCreateFunction').Enabled := False;
    MainAction('aDCreateTrigger').Enabled := False;
    MainAction('aDCreateEvent').Enabled := False;
    MainAction('aDCreateKey').Enabled := False;
    MainAction('aDCreateField').Enabled := False;
    MainAction('aDCreateForeignKey').Enabled := False;
    MainAction('aDCreateUser').Enabled := False;
    MainAction('aDDeleteDatabase').Enabled := False;
    MainAction('aDDeleteTable').Enabled := False;
    MainAction('aDDeleteView').Enabled := False;
    MainAction('aDDeleteRoutine').Enabled := False;
    MainAction('aDDeleteKey').Enabled := False;
    MainAction('aDDeleteField').Enabled := False;
    MainAction('aDDeleteForeignKey').Enabled := False;
    MainAction('aDDeleteTrigger').Enabled := False;
    MainAction('aDDeleteEvent').Enabled := False;
    MainAction('aDDeleteUser').Enabled := False;
    MainAction('aDEditServer').Enabled := False;
    MainAction('aDEditTable').Enabled := False;
    MainAction('aDEditView').Enabled := False;
    MainAction('aDEditRoutine').Enabled := False;
    MainAction('aDEditEvent').Enabled := False;
    MainAction('aDEditKey').Enabled := False;
    MainAction('aDEditField').Enabled := False;
    MainAction('aDEditForeignKey').Enabled := False;
    MainAction('aDEditTrigger').Enabled := False;
    MainAction('aDEmpty').Enabled := False;

    mlOpen.Enabled := False;
    aDDelete.Enabled := False;
    mlEProperties.Action := nil;

    if (Assigned(Item)) then
    begin
      MainAction('aFImportSQL').Enabled := (ListView.SelCount = 1) and (Item.ImageIndex in [iiDatabase]);
      MainAction('aFImportText').Enabled := (ListView.SelCount = 1) and (Item.ImageIndex in [iiDatabase, iiBaseTable]);
      MainAction('aFImportExcel').Enabled := (ListView.SelCount = 1) and (Item.ImageIndex in [iiDatabase, iiBaseTable]);
      MainAction('aFImportAccess').Enabled := (ListView.SelCount = 1) and (Item.ImageIndex in [iiDatabase, iiBaseTable]);
      MainAction('aFImportODBC').Enabled := (ListView.SelCount = 1) and (Item.ImageIndex in [iiDatabase, iiBaseTable]);
      MainAction('aFExportSQL').Enabled := (Item.ImageIndex in [iiDatabase, iiBaseTable, iiView, iiProcedure, iiFunction, iiEvent, iiTrigger]);
      MainAction('aFExportText').Enabled := (ListView.SelCount = 1) and (Item.ImageIndex in [iiBaseTable, iiView]);
      MainAction('aFExportExcel').Enabled := (ListView.SelCount = 1) and (Item.ImageIndex in [iiDatabase, iiBaseTable, iiView]);
      MainAction('aFExportAccess').Enabled := (ListView.SelCount = 1) and (Item.ImageIndex in [iiDatabase, iiBaseTable]);
      MainAction('aFExportODBC').Enabled := (ListView.SelCount = 1) and (Item.ImageIndex in [iiDatabase, iiBaseTable]);
      MainAction('aFExportXML').Enabled := (Item.ImageIndex in [iiDatabase, iiBaseTable, iiView]);
      MainAction('aFExportHTML').Enabled := (Item.ImageIndex in [iiDatabase, iiBaseTable, iiView, iiProcedure, iiFunction, iiEvent, iiTrigger]);
      MainAction('aFExportPDF').Enabled := (Item.ImageIndex in [iiDatabase, iiBaseTable, iiView, iiProcedure, iiFunction, iiEvent, iiTrigger]);
      MainAction('aECopy').Enabled := ListView.SelCount = 1;
      MainAction('aEPaste').Enabled := (ListView.SelCount = 1) and ((Item.ImageIndex = iiDatabase) and Clipboard.HasFormat(CF_MYSQLDATABASE) or (Item.ImageIndex = iiBaseTable) and Clipboard.HasFormat(CF_MYSQLTABLE) or (Item.ImageIndex = iiView) and Clipboard.HasFormat(CF_MYSQLVIEW));
      MainAction('aERename').Enabled := (ListView.SelCount = 1) and ((Item.ImageIndex in [iiBaseTable, iiView, iiEvent, iiField, iiVirtualField, iiTrigger]) or (Item.ImageIndex = iiForeignKey) and (Session.Connection.MySQLVersion >= 40013));
      MainAction('aDCreateTable').Enabled := (ListView.SelCount = 1) and (Item.ImageIndex in [iiDatabase]);
      MainAction('aDCreateView').Enabled := (ListView.SelCount = 1) and (Item.ImageIndex in [iiDatabase]) and (Session.Connection.MySQLVersion >= 50001);
      MainAction('aDCreateProcedure').Enabled := (ListView.SelCount = 1) and (Item.ImageIndex in [iiDatabase]) and Assigned(TSDatabase(Item.Data).Routines);
      MainAction('aDCreateFunction').Enabled := MainAction('aDCreateProcedure').Enabled;
      MainAction('aDCreateEvent').Enabled := (ListView.SelCount = 1) and (Item.ImageIndex in [iiDatabase]) and Assigned(TSDatabase(Item.Data).Events);
      MainAction('aDCreateTrigger').Enabled := (ListView.SelCount = 1) and (Item.ImageIndex = iiBaseTable) and Assigned(TSBaseTable(Item.Data).Database.Triggers);
      MainAction('aDCreateKey').Enabled := (ListView.SelCount = 1) and (Item.ImageIndex = iiBaseTable);
      MainAction('aDCreateField').Enabled := (ListView.SelCount = 1) and (Item.ImageIndex = iiBaseTable);
      MainAction('aDCreateForeignKey').Enabled := (ListView.SelCount = 1) and (Item.ImageIndex = iiBaseTable);
      MainAction('aDCreateUser').Enabled := (ListView.SelCount = 1) and (Item.ImageIndex in [iiUsers]);
      MainAction('aDDeleteDatabase').Enabled := (ListView.SelCount >= 1) and (Item.ImageIndex in [iiDatabase]);
      MainAction('aDDeleteTable').Enabled := (ListView.SelCount >= 1) and (Item.ImageIndex = iiBaseTable);
      MainAction('aDDeleteView').Enabled := (ListView.SelCount >= 1) and (Item.ImageIndex = iiView);
      MainAction('aDDeleteRoutine').Enabled := (ListView.SelCount >= 1) and (Item.ImageIndex in [iiProcedure, iiFunction]);
      MainAction('aDDeleteTrigger').Enabled := (ListView.SelCount >= 1) and Selected and (Item.ImageIndex = iiTrigger);
      MainAction('aDDeleteEvent').Enabled := (ListView.SelCount = 1) and (Item.ImageIndex = iiEvent);
      MainAction('aDDeleteKey').Enabled := (ListView.SelCount >= 1) and (Item.ImageIndex = iiKey);
      MainAction('aDDeleteField').Enabled := (ListView.SelCount >= 1) and (Item.ImageIndex in [iiField, iiVirtualField]) and (TSBaseTableField(Item.Data).Table.Fields.Count > ListView.SelCount);
      MainAction('aDDeleteForeignKey').Enabled := (ListView.SelCount >= 1) and (Item.ImageIndex = iiForeignKey) and (Session.Connection.MySQLVersion >= 40013);
      MainAction('aDDeleteProcess').Enabled := (ListView.SelCount >= 1) and (Item.ImageIndex in [iiProcess]) and (TSProcess(Item.Data).ThreadId <> Session.Connection.ThreadId);
      MainAction('aDDeleteUser').Enabled := (ListView.SelCount >= 1) and (Item.ImageIndex in [iiUser]);
      MainAction('aDEditDatabase').Enabled := (ListView.SelCount >= 1) and (Item.ImageIndex in [iiDatabase]);
      MainAction('aDEditTable').Enabled := (ListView.SelCount >= 1) and (Item.ImageIndex = iiBaseTable);
      MainAction('aDEditView').Enabled := (ListView.SelCount = 1) and (Item.ImageIndex = iiView);
      MainAction('aDEditRoutine').Enabled := (ListView.SelCount = 1) and (Item.ImageIndex in [iiProcedure, iiFunction]);
      MainAction('aDEditTrigger').Enabled := (ListView.SelCount = 1) and (Item.ImageIndex = iiTrigger);
      MainAction('aDEditEvent').Enabled := (ListView.SelCount = 1) and (Item.ImageIndex = iiEvent);
      MainAction('aDEditKey').Enabled := (ListView.SelCount = 1) and (Item.ImageIndex = iiKey);
      MainAction('aDEditField').Enabled := (ListView.SelCount = 1) and (Item.ImageIndex in [iiField, iiVirtualField]);
      MainAction('aDEditForeignKey').Enabled := (ListView.SelCount = 1) and (Item.ImageIndex = iiForeignKey);
      MainAction('aDEditProcess').Enabled := (ListView.SelCount = 1) and (Item.ImageIndex in [iiProcess]);
      MainAction('aDEditUser').Enabled := (ListView.SelCount = 1) and (Item.ImageIndex in [iiUser]);
      MainAction('aDEditVariable').Enabled := (ListView.SelCount = 1) and (Item.ImageIndex in [iiVariable]);
      MainAction('aDEmpty').Enabled := (ListView.SelCount >= 1) and ((Item.ImageIndex in [iiDatabase, iiBaseTable]) or (Item.ImageIndex in [iiField]) and TSBaseTableField(Item.Data).NullAllowed);

      mlOpen.Enabled := (ListView.SelCount = 1) and (Item.ImageIndex in [iiDatabase, iiSystemDatabase, iiBaseTable, iiView, iiSystemView, iiProcedure, iiFunction, iiEvent, iiTrigger]);
      aDDelete.Enabled := (ListView.SelCount >= 1);

      case (Item.ImageIndex) of
        iiDatabase: mlEProperties.Action := MainAction('aDEditDatabase');
        iiBaseTable: mlEProperties.Action := MainAction('aDEditTable');
        iiView: mlEProperties.Action := MainAction('aDEditView');
        iiProcedure,
        iiFunction: mlEProperties.Action := MainAction('aDEditRoutine');
        iiTrigger: mlEProperties.Action := MainAction('aDEditTrigger');
        iiEvent: mlEProperties.Action := MainAction('aDEditEvent');
        iiKey: mlEProperties.Action := MainAction('aDEditKey');
        iiField,
        iiVirtualField: mlEProperties.Action := MainAction('aDEditField');
        iiForeignKey: mlEProperties.Action := MainAction('aDEditForeignKey');
        iiProcess: mlEProperties.Action := MainAction('aDEditProcess');
        iiUser: mlEProperties.Action := MainAction('aDEditUser');
        iiVariable: mlEProperties.Action := MainAction('aDEditVariable');
      end;

      for I := 0 to ListView.Items.Count - 1 do
        if (ListView.Items[I].Selected) then
        begin
          MainAction('aFExportSQL').Enabled := MainAction('aFExportSQL').Enabled and (ListView.Items[I].ImageIndex in [iiDatabase, iiBaseTable, iiView, iiProcedure, iiFunction, iiEvent, iiTrigger]);
          MainAction('aFExportExcel').Enabled := MainAction('aFExportExcel').Enabled and (ListView.Items[I].ImageIndex in [iiDatabase, iiBaseTable, iiView]);
          MainAction('aFExportAccess').Enabled := MainAction('aFExportAccess').Enabled and (ListView.Items[I].ImageIndex in [iiDatabase, iiBaseTable]);
          MainAction('aFExportODBC').Enabled := MainAction('aFExportODBC').Enabled and (ListView.Items[I].ImageIndex in [iiDatabase, iiBaseTable]);
          MainAction('aFExportXML').Enabled := MainAction('aFExportXML').Enabled and (ListView.Items[I].ImageIndex in [iiDatabase, iiBaseTable, iiView]);
          MainAction('aFExportHTML').Enabled := MainAction('aFExportHTML').Enabled and (ListView.Items[I].ImageIndex in [iiDatabase, iiBaseTable, iiView, iiProcedure, iiFunction, iiEvent, iiTrigger]);
          MainAction('aFExportPDF').Enabled := MainAction('aFExportPDF').Enabled and (ListView.Items[I].ImageIndex in [iiDatabase, iiBaseTable, iiView, iiProcedure, iiFunction, iiEvent, iiTrigger]);
          MainAction('aECopy').Enabled := MainAction('aECopy').Enabled and (ListView.Items[I].ImageIndex in [iiDatabase, iiBaseTable, iiView, iiSystemView, iiProcedure, iiFunction, iiEvent, iiKey, iiField, iiVirtualField, iiViewField, iiSystemViewField, iiForeignKey]);
          MainAction('aDDeleteDatabase').Enabled := MainAction('aDDeleteDatabase').Enabled and (ListView.Items[I].ImageIndex in [iiDatabase]);
          MainAction('aDDeleteTable').Enabled := MainAction('aDDeleteTable').Enabled and (ListView.Items[I].ImageIndex in [iiBaseTable]);
          MainAction('aDDeleteView').Enabled := MainAction('aDDeleteView').Enabled and (ListView.Items[I].ImageIndex in [iiView]);
          MainAction('aDDeleteRoutine').Enabled := MainAction('aDDeleteRoutine').Enabled and (ListView.Items[I].ImageIndex in [iiProcedure, iiFunction]);
          MainAction('aDDeleteEvent').Enabled := MainAction('aDDeleteEvent').Enabled and (ListView.Items[I].ImageIndex in [iiEvent]);
          MainAction('aDDeleteTrigger').Enabled := MainAction('aDDeleteTrigger').Enabled and (ListView.Items[I].ImageIndex in [iiTrigger]);
          MainAction('aDDeleteKey').Enabled := MainAction('aDDeleteKey').Enabled and (ListView.Items[I].ImageIndex in [iiKey]);
          MainAction('aDDeleteField').Enabled := MainAction('aDDeleteField').Enabled and (ListView.Items[I].ImageIndex in [iiField, iiVirtualField]);
          MainAction('aDDeleteForeignKey').Enabled := MainAction('aDDeleteForeignKey').Enabled and (ListView.Items[I].ImageIndex in [iiForeignKey]);
          MainAction('aDEditDatabase').Enabled := MainAction('aDEditDatabase').Enabled and (ListView.Items[I].ImageIndex in [iiDatabase]);
          MainAction('aDEditTable').Enabled := MainAction('aDEditTable').Enabled and (ListView.Items[I].ImageIndex in [iiBaseTable]);
          MainAction('aDEditKey').Enabled := MainAction('aDEditKey').Enabled and (ListView.Items[I].ImageIndex in [iiKey]);
          MainAction('aDEditField').Enabled := MainAction('aDEditField').Enabled and (ListView.Items[I].ImageIndex in [iiField, iiVirtualField]);
          MainAction('aDEditForeignKey').Enabled := MainAction('aDEditForeignKey').Enabled and (ListView.Items[I].ImageIndex in [iiForeignKey]);
          MainAction('aDEditTrigger').Enabled := MainAction('aDEditTrigger').Enabled and (ListView.Items[I].ImageIndex in [iiTrigger]);
          MainAction('aDEmpty').Enabled := MainAction('aDEmpty').Enabled and (ListView.Items[I].ImageIndex in [iiDatabase, iiBaseTable, iiField]);
          aDDelete.Enabled := aDDelete.Enabled and (ListView.Items[I].ImageIndex in [iiDatabase, iiBaseTable, iiView, iiProcedure, iiFunction, iiTrigger, iiEvent, iiKey, iiField, iiVirtualField, iiForeignKey]);
        end;
    end
    else if ((View = vObjects) and Assigned(FNavigator.Selected)) then
    begin
      FNavigatorChanged(FNavigator, FNavigator.Selected);

      MainAction('aECopy').Enabled := False;
      MainAction('aERename').Enabled := False;
      MainAction('aDEmpty').Enabled := False;
      aDDelete.Enabled := False;

      case (FNavigator.Selected.ImageIndex) of
        iiDatabase: mlEProperties.Action := MainAction('aDEditDatabase');
        iiBaseTable: mlEProperties.Action := MainAction('aDEditTable');
        iiView: mlEProperties.Action := MainAction('aDEditView');
        iiProcedure,
        iiFunction: mlEProperties.Action := MainAction('aDEditRoutine');
        iiTrigger: mlEProperties.Action := MainAction('aDEditTrigger');
        iiEvent: mlEProperties.Action := MainAction('aDEditEvent');
        iiKey: mlEProperties.Action := MainAction('aDEditKey');
        iiField,
        iiVirtualField: mlEProperties.Action := MainAction('aDEditField');
        iiForeignKey: mlEProperties.Action := MainAction('aDEditForeignKey');
        iiProcess: mlEProperties.Action := MainAction('aDEditProcess');
        iiUser: mlEProperties.Action := MainAction('aDEditUser');
        iiVariable: mlEProperties.Action := MainAction('aDEditVariable');
        else mlEProperties.Action := nil;
      end;
    end;

    mlOpen.Default := mlOpen.Enabled;
    mlEProperties.Default := Assigned(Item) and not mlOpen.Default and mlEProperties.Enabled;
    mlEProperties.Caption := Preferences.LoadStr(97) + '...';
    mlEProperties.ShortCut := ShortCut(VK_RETURN, [ssAlt]);

    ToolBarData.tbPropertiesAction := mlEProperties.Action;
    Window.Perform(UM_UPDATETOOLBAR, 0, LPARAM(Self));

    ShowEnabledItems(MList.Items);

    if (Sender <> MList) then
      StatusBarRefresh();
  end;
end;

procedure TFSession.MetadataProviderGetSQLFieldNames(Sender: TacBaseMetadataProvider;
  const SQL: WideString; Fields: TacFieldsList);
var
  Database: TSDatabase;
  DatabaseName: string;
  I: Integer;
  Parse: TSQLParse;
  Table: TSTable;
  TableName: string;
begin
  if (not SQLCreateParse(Parse, PChar(SQL), Length(SQL), Session.Connection.MySQLVersion)
    or not SQLParseKeyword(Parse, 'SELECT')) then
    raise ERangeError.Create(SRangeError)
  else
  begin
    if (not SQLParseChar(Parse, '*')) then
      raise ERangeError.Create(SRangeError);
    if (not SQLParseKeyword(Parse, 'FROM')) then
      raise ERangeError.Create(SRangeError)
    else
    begin
      Session.Connection.BeginSynchron();
      DatabaseName := SelectedDatabase;
      if (SQLParseObjectName(Parse, DatabaseName, TableName)) then
      begin
        Database := Session.DatabaseByName(DatabaseName);
        if (Assigned(Database) and Database.Tables.Update()) then
        begin
          Table := Database.TableByName(TableName);
          if (Assigned(Table) and Table.Update()) then
            for I := 0 to Table.Fields.Count - 1 do
              Fields.AddField(Table.Fields[I].Name, Session.LowerCaseTableNames = 0);
        end;
      end;
      Session.Connection.EndSynchron();
    end;
  end;
end;

procedure TFSession.mfDeleteClick(Sender: TObject);
begin
  FFiles.InvokeCommandOnSelected('delete');
end;

procedure TFSession.mfFilterAccessClick(Sender: TObject);
begin
  FFiles.Filter := '*.mdb;*.accdb';
end;

procedure TFSession.mfFilterClearClick(Sender: TObject);
begin
  FFiles.Filter := '*';
  mfFilter.Checked := False;
  mfFilterText.Checked := False;
end;

procedure TFSession.mfFilterExcelClick(Sender: TObject);
begin
  FFiles.Filter := '*.xls;*.xlsb';
end;

procedure TFSession.mfFilterHTMLClick(Sender: TObject);
begin
  FFiles.Filter := '*.html;*.htm';
end;

procedure TFSession.mfFilterSQLClick(Sender: TObject);
begin
  FFiles.Filter := '*.sql';
end;

procedure TFSession.mfFilterTextClick(Sender: TObject);
begin
  FFiles.Filter := '*.txt;*.csv';
end;

procedure TFSession.mfFilterXMLClick(Sender: TObject);
begin
  FFiles.Filter := '*.xml';
end;

procedure TFSession.MFilesPopup(Sender: TObject);
begin
  mfFilterClear.Checked := FFiles.Filter = '*';
  mfFilterSQL.Checked := FFiles.Filter = '*.sql';
  mfFilterText.Checked := FFiles.Filter = '*.txt;*.csv';
  mfFilterAccess.Checked := FFiles.Filter = '*.mdb;*.accdb';
  mfFilterExcel.Checked := FFiles.Filter = '*.xls;*.xlsb';
  mfFilterHTML.Checked := FFiles.Filter = '*.html;*.htm';
  mfFilterXML.Checked := FFiles.Filter = '*.xml';

  mfOpen.Enabled := FFiles.SelCount = 1;
  mfFilter.Enabled := FFiles.SelCount = 0;
  mfDelete.Enabled := FFiles.SelCount = 1;
  mfRename.Enabled := FFiles.SelCount = 1;
  mfProperties.Enabled := FFiles.SelCount = 1;

  ShowEnabledItems(MFiles.Items);
end;

procedure TFSession.mfOpenClick(Sender: TObject);
begin
  if (Assigned(FFiles.Selected) and (LowerCase(ExtractFileExt(FFolders.SelectedFolder + PathDelim + FFiles.Selected.Caption)) = '.sql')) then
  begin
    if (not (View in [vEditor, vEditor2, vEditor3])) then
      View := vEditor;

    if (Boolean(Perform(UM_CLOSE_TAB_QUERY, 0, 0))) then
      OpenSQLFile(FFolders.SelectedFolder + PathDelim + FFiles.Selected.Caption);
  end
  else
    FFiles.InvokeCommandOnSelected('open');
end;

procedure TFSession.mfPropertiesClick(Sender: TObject);
begin
  FFiles.InvokeCommandOnSelected('properties');
end;

procedure TFSession.mfRenameClick(Sender: TObject);
begin
  FFiles.Selected.EditCaption();
end;

procedure TFSession.MGridHeaderPopup(Sender: TObject);
var
  I: Integer;
  Item: TMenuItem;
begin
  ghmGoto.Clear();
  for I := 0 to ActiveDBGrid.Columns.Count - 1 do
  begin
    Item := TMenuItem.Create(ghmGoto);
    Item.Caption := ActiveDBGrid.Columns[I].DisplayName;
    Item.Checked := I = ActiveDBGrid.SelectedIndex;
    Item.RadioItem := True;
    Item.OnClick := ghmGotoClick;
    ghmGoto.Add(Item);
  end;
end;

procedure TFSession.MGridPopup(Sender: TObject);

  procedure AddFilterMenuItem(const Field: TField; const Value: string; const FilterIndex: Integer);
  var
    NewMenuItem: TMenuItem;
  begin
    NewMenuItem := TMenuItem.Create(Self);
    NewMenuItem.AutoHotkeys := maManual;
    if (FilterIndex < 0) then
      NewMenuItem.Caption := '-'
    else
    begin
      NewMenuItem.Caption := Format(Filters[FilterIndex].Text, [Field.DisplayName, Value]);
      NewMenuItem.OnClick := gmFilterIntoFilterClick;
      NewMenuItem.Tag := FilterIndex;
    end;
    gmFilter.Add(NewMenuItem);
  end;

var
  ClientCoord: TPoint;
  GridCoord: TGridCoord;
  I: Integer;
  NewMenuItem: TMenuItem;
  Value: string;
begin
  ClientCoord := ActiveDBGrid.ScreenToClient(MGrid.PopupPoint);
  GridCoord := ActiveDBGrid.MouseCoord(ClientCoord.X, ClientCoord.Y);

  if (GridCoord.X < 0) then
  begin
    for I := 0 to MGrid.Items.Count - 1 do
      MGrid.Items[I].Visible := False;

    ShowEnabledItems(gmFExport);
    gmFExport.Visible := not ActiveDBGrid.EditorMode;
  end
  else
  begin
    gmFilter.Clear(); gmFilter.Enabled := False;

    if (Assigned(ActiveDBGrid.SelectedField)
      and not ActiveDBGrid.EditorMode
      and not (ActiveDBGrid.SelectedField.DataType in [ftWideMemo, ftBlob])) then
    begin
      gmFilter.Enabled := True;

      if (((ActiveDBGrid.DataSource.DataSet is TSTable.TDataSet) and (TSTable.TDataSet(ActiveDBGrid.DataSource.DataSet).FilterSQL <> ''))
        or not (ActiveDBGrid.DataSource.DataSet is TSTable.TDataSet) and ActiveDBGrid.DataSource.DataSet.Filtered) then
      begin
        NewMenuItem := TMenuItem.Create(Self);
        NewMenuItem.Caption := Preferences.LoadStr(28);
        NewMenuItem.OnClick := gmFilterClearClick;
        gmFilter.Add(NewMenuItem);

        NewMenuItem := TMenuItem.Create(Self);
        NewMenuItem.Caption := '-';
        gmFilter.Add(NewMenuItem);
      end;

      if (not ActiveDBGrid.SelectedField.Required) then
      begin
        AddFilterMenuItem(ActiveDBGrid.SelectedField, Value, 0);
        AddFilterMenuItem(ActiveDBGrid.SelectedField, Value, 1);
        AddFilterMenuItem(ActiveDBGrid.SelectedField, '', -1);
      end;

      if (not ActiveDBGrid.SelectedField.IsNull) then
      begin
        if (ActiveDBGrid.SelectedField.DataType in NotQuotedDataTypes) then
          Value := ActiveDBGrid.SelectedField.DisplayText
        else
          Value := SQLEscape(ActiveDBGrid.SelectedField.DisplayText);
        AddFilterMenuItem(ActiveDBGrid.SelectedField, Value, 2);
        AddFilterMenuItem(ActiveDBGrid.SelectedField, Value, 3);
        AddFilterMenuItem(ActiveDBGrid.SelectedField, Value, 4);
        AddFilterMenuItem(ActiveDBGrid.SelectedField, Value, 5);
        if (ActiveDBGrid.SelectedField.DataType in [ftString, ftWideString]) then
          AddFilterMenuItem(ActiveDBGrid.SelectedField, SQLEscape('%' + ActiveDBGrid.SelectedField.DisplayText + '%'), 6);
        AddFilterMenuItem(ActiveDBGrid.SelectedField, '', -1);
      end;

      if (ActiveDBGrid.SelectedField.DataType in NotQuotedDataTypes) then
        Value := '...'
      else
        Value := SQLEscape('...');
      AddFilterMenuItem(ActiveDBGrid.SelectedField, Value, 7);
      AddFilterMenuItem(ActiveDBGrid.SelectedField, Value, 8);
      AddFilterMenuItem(ActiveDBGrid.SelectedField, Value, 9);
      AddFilterMenuItem(ActiveDBGrid.SelectedField, Value, 10);
      if (ActiveDBGrid.SelectedField.DataType in [ftString, ftWideString]) then
        AddFilterMenuItem(ActiveDBGrid.SelectedField, Value, 11);
    end;

    ShowEnabledItems(MGrid.Items);

    gmDInsertRecord.Visible := gmDInsertRecord.Visible and not ActiveDBGrid.EditorMode;
    gmDDeleteRecord.Visible := gmDDeleteRecord.Visible and not ActiveDBGrid.EditorMode;
    gmFExport.Visible := gmFExport.Visible and not ActiveDBGrid.EditorMode;
    gmFilter.Visible := gmFilter.Visible and not ActiveDBGrid.EditorMode;
    gmDEditRecord.Visible := gmDEditRecord.Visible and not ActiveDBGrid.EditorMode;
  end;
end;

procedure TFSession.miHOpenClick(Sender: TObject);
begin
  Wanted.Clear();

  if (Assigned(FSQLHistoryMenuNode) and (FSQLHistoryMenuNode.ImageIndex in [iiStatement, iiQuery, iiClock])
    and Boolean(Perform(UM_CLOSE_TAB_QUERY, 0, 0))) then
  begin
    if (not (View in [vEditor, vEditor2, vEditor3])) then
      View := vEditor;
    if (View in [vEditor, vEditor2, vEditor3]) then
    begin
      ActiveSynMemo.Text := XMLNode(IXMLNode(FSQLHistoryMenuNode.Data), 'sql').Text;

      Window.ActiveControl := ActiveSynMemo;
    end;
  end;
end;

procedure TFSession.miHPropertiesClick(Sender: TObject);
var
  Node: IXMLNode;
begin
  Node := IXMLNode(FSQLHistoryMenuNode.Data);

  if (not Assigned(XMLNode(Node, 'database'))) then
    DStatement.DatabaseName := ''
  else
    DStatement.DatabaseName := XMLNode(Node, 'database').Text;
  DStatement.DateTime := StrToFloat(XMLNode(Node, 'datetime').Text, FileFormatSettings);
  if (not Assigned(XMLNode(Node, 'info'))) then
    DStatement.Info := ''
  else
    DStatement.Info := XMLNode(Node, 'info').Text;
  if (not Assigned(XMLNode(Node, 'insert_id'))) then
    DStatement.Id := 0
  else
    DStatement.Id := StrToInt64(XMLNode(Node, 'insert_id').Text);
  if (not Assigned(XMLNode(Node, 'rows_affected'))) then
    DStatement.RowsAffected := -1
  else
    DStatement.RowsAffected := SysUtils.StrToInt(XMLNode(Node, 'rows_affected').Text);
  if (not Assigned(XMLNode(Node, 'execution_time'))) then
    DStatement.StatementTime := MySQLZeroDate
  else
    DStatement.StatementTime := StrToFloat(XMLNode(Node, 'execution_time').Text, FileFormatSettings);
  DStatement.SQL := XMLNode(Node, 'sql').Text;
  if (Node.Attributes['type'] = 'query') then
    DStatement.ViewType := vtQuery
  else
    DStatement.ViewType := vtStatement;

  DStatement.Execute();
end;

procedure TFSession.miHSaveAsClick(Sender: TObject);
begin
  Wanted.Clear();

  SaveSQLFile(Sender);
end;

procedure TFSession.miHStatementIntoSQLEditorClick(Sender: TObject);
var
  SelLength: Integer;
  SelStart: Integer;
begin
  Wanted.Clear();

  if (Assigned(FSQLHistoryMenuNode) and (FSQLHistoryMenuNode.ImageIndex in [iiStatement, iiQuery, iiClock])) then
  begin
    if (not (View in [vEditor, vEditor2, vEditor3])) then
      View := vEditor;
    if (View in [vEditor, vEditor2, vEditor3]) then
    begin
      SelStart := ActiveSynMemo.SelStart;
      ActiveSynMemo.SelText := XMLNode(IXMLNode(FSQLHistoryMenuNode.Data), 'sql').Text;
      SelLength := ActiveSynMemo.SelStart - SelStart;

      ActiveSynMemo.SelStart := SelStart;
      ActiveSynMemo.SelLength := SelLength;

      Window.ActiveControl := ActiveSynMemo;
    end;
  end;
end;

procedure TFSession.MListPopup(Sender: TObject);
var
  I: Integer;
  Rect: TRect;
begin
  // Debug 2016-11-17
  if (not Assigned(ActiveListView)) then
    raise ERangeError.Create(SRangeError);
    // Occurred on 2017-01-05

  ListViewSelectItem(ActiveListView, ActiveListView.Selected, Assigned(ActiveListView.Selected));

  if (not BOOL(Header_GetItemRect(ListView_GetHeader(ActiveListView.Handle), 0, @Rect)) or (MList.PopupPoint.Y - ActiveListView.ClientOrigin.Y < Rect.Bottom)) then
    for I := 0 to MList.Items.Count - 1 do
      MList.Items[I].Visible := False;
end;

procedure TFSession.mlOpenClick(Sender: TObject);
var
  Child: TTreeNode;
  ForeignKey: TSForeignKey;
  NewNode: TTreeNode;
  URI: TUURI;
begin
  Wanted.Clear();

  case (ActiveListView.Selected.ImageIndex) of
    iiForeignKey:
      begin
        ForeignKey := TSForeignKey(ActiveListView.Selected.Data);
        URI := TUURI.Create(Address);
        URI.Database := ForeignKey.Parent.DatabaseName;
        URI.Table := ForeignKey.Parent.TableName;
        Address := URI.Address;
        URI.Free();
      end;
    else
      begin
        NewNode := nil;
        FNavigator.Selected.Expand(False);
        Child := FNavigator.Selected.getFirstChild();
        while (Assigned(Child)) do
        begin
          if (lstrcmpi(PChar(Child.Text), PChar(ActiveListView.Selected.Caption)) = 0) then
            NewNode := Child;
          Child := FNavigator.Selected.getNextChild(Child);
        end;
        if (Assigned(NewNode)) then
          FNavigator.Selected := NewNode;
      end;
  end;
end;

procedure TFSession.MNavigatorPopup(Sender: TObject);
var
  AllowChange: Boolean;
  P: TPoint;
begin
  KillTimer(Handle, tiStatusBar);
  KillTimer(Handle, tiNavigator);

  AllowChange := True;
  FNavigatorChanging(Sender, FNavigator.Selected, AllowChange);

  if (Sender = FNavigator.PopupMenu) then
  begin
    // Bei einem Click auf den WhiteSpace: FNavigator.Selected zeigt den zuletzt selektierten Node an :-(
    P := GetClientOrigin();
    FNavigatorMenuNode := FNavigator.GetNodeAt(MNavigator.PopupPoint.X - P.x - (PSideBar.Left + PNavigator.Left + FNavigator.Left), MNavigator.PopupPoint.y - P.y - (PSideBar.Top + PNavigator.Top + FNavigator.Top));
  end
  else
    FNavigatorMenuNode := FNavigator.Selected;

  FNavigatorChanged(Sender, FNavigatorMenuNode);
end;

procedure TFSession.MSQLEditorPopup(Sender: TObject);
var
  I: Integer;
begin
  // Debug 2017-01-05
  if (not Assigned(ActiveSynMemo)) then
    raise ERangeError.Create('Address: ' + Address + #13#10
      + 'Assigned: ' + BoolToStr(Assigned(GetActiveSynMemo()), True));

  SynMemoStatusChange(Sender, []);
  ShowEnabledItems(MSQLEditor.Items);

  if (ActiveSynMemo.Gutter.Visible) then
    for I := 0 to MSQLEditor.Items.Count - 1 do
      MSQLEditor.Items[I].Visible := MSQLEditor.Items[I].Visible and (MSQLEditor.PopupPoint.X - ActiveSynMemo.ClientOrigin.X > ActiveSynMemo.Gutter.Width);
end;

procedure TFSession.MSQLHistoryPopup(Sender: TObject);
var
  Point: TPoint;
begin
  if (Sender = FSQLHistory.PopupMenu) then
  begin
    // Bei einem Click auf den WhiteSpace: FNavigator.Selected zeigt den zuletzt selektierten Node an :-(
    Point := GetClientOrigin();
    FSQLHistoryMenuNode := FSQLHistory.GetNodeAt(MSQLHistory.PopupPoint.x - Point.x - (PSideBar.Left + PSQLHistory.Left + FSQLHistory.Left), MSQLHistory.PopupPoint.y - Point.y - (PSideBar.Top + PSQLHistory.Top + FSQLHistory.Top));
  end
  else if (Assigned(FSQLHistory.Selected)) then
    FSQLHistoryMenuNode := FSQLHistory.Selected;

  MainAction('aECopy').Enabled := Assigned(FSQLHistoryMenuNode) and (FSQLHistoryMenuNode.ImageIndex in [iiStatement, iiQuery]);

  miHStatementIntoSQLEditor.Enabled := Assigned(FSQLHistoryMenuNode) and (View in [vEditor, vEditor2, vEditor3]) and (FSQLHistoryMenuNode.ImageIndex in [iiStatement, iiQuery, iiClock]);
  aPExpand.Enabled := Assigned(FSQLHistoryMenuNode) and not FSQLHistoryMenuNode.Expanded and FSQLHistoryMenuNode.HasChildren;
  aPCollapse.Enabled := Assigned(FSQLHistoryMenuNode) and FSQLHistoryMenuNode.Expanded;
  miHOpen.Enabled := Assigned(FSQLHistoryMenuNode) and (View in [vEditor, vEditor2, vEditor3]) and (FSQLHistoryMenuNode.ImageIndex in [iiStatement, iiQuery, iiClock]);
  miHSaveAs.Enabled := Assigned(FSQLHistoryMenuNode) and (FSQLHistoryMenuNode.ImageIndex in [iiStatement, iiQuery, iiClock]);
  miHRun.Enabled := Assigned(FSQLHistoryMenuNode) and (FSQLHistoryMenuNode.ImageIndex in [iiStatement, iiQuery, iiClock]);
  miHProperties.Enabled := Assigned(FSQLHistoryMenuNode) and (FSQLHistoryMenuNode.ImageIndex in [iiStatement, iiQuery, iiClock]) and not FSQLHistoryMenuNode.HasChildren;

  miHExpand.Default := aPExpand.Enabled;
  miHCollapse.Default := aPCollapse.Enabled;
  miHStatementIntoSQLEditor.Default := not aPExpand.Enabled and not aPCollapse.Enabled and miHStatementIntoSQLEditor.Enabled;

  ShowEnabledItems(MSQLHistory.Items);
end;

procedure TFSession.MTextPopup(Sender: TObject);
begin
  ShowEnabledItems(MText.Items);

  tmECut.Visible := True;
  tmECopy.Visible := True;
  tmEPaste.Visible := True;
  tmEDelete.Visible := True;
  tmESelectAll.Visible := True;
end;

procedure TFSession.MToolBarPopup(Sender: TObject);
var
  Checked: Integer;
  I: Integer;
begin
  mtObjects.Checked := ttObjects in Preferences.ToolbarTabs;
  mtBrowser.Checked := ttBrowser in Preferences.ToolbarTabs;
  mtIDE.Checked := ttIDE in Preferences.ToolbarTabs;
  mtBuilder.Checked := ttBuilder in Preferences.ToolbarTabs;
  mtDiagram.Checked := ttDiagram in Preferences.ToolbarTabs;
  mtEditor.Checked := ttEditor in Preferences.ToolbarTabs;
  mtEditor2.Checked := ttEditor2 in Preferences.ToolbarTabs;
  mtEditor3.Checked := ttEditor3 in Preferences.ToolbarTabs;

  mtObjects.Enabled := View <> vObjects;
  mtBrowser.Enabled := View <> vBrowser;
  mtIDE.Enabled := View <> vIDE;
  mtBuilder.Enabled := View <> vBuilder;
  mtDiagram.Enabled := View <> vDiagram;
  mtEditor.Enabled := View <> vEditor;
  mtEditor2.Enabled := View <> vEditor2;
  mtEditor3.Enabled := View <> vEditor3;

  Checked := 0;
  for I := 0 to MToolBar.Items.Count - 1 do
    if (MToolBar.Items[I].Checked) then
      Inc(Checked);

  for I := 0 to MToolBar.Items.Count - 1 do
    MToolBar.Items[I].Enabled := (Checked > 1) or not MToolBar.Items[I].Checked;
end;

procedure TFSession.mwCreateLinkExecute(Sender: TObject);
var
  MenuItem: TMenuItem;
  P: TPoint;
begin
  Wanted.Clear();

  if (Sender is TMenuItem) then
  begin
    MenuItem := TMenuItem(Sender);

    P := ActiveWorkbench.ScreenToClient(TPopupMenu(MenuItem.GetParentMenu()).PopupPoint);
    ActiveWorkbench.CreateNewLink(P.X, P.Y);
  end;
end;

procedure TFSession.mwCreateSectionClick(Sender: TObject);
var
  MenuItem: TMenuItem;
  P: TPoint;
begin
  Wanted.Clear();

  if (Sender is TMenuItem) then
  begin
    MenuItem := TMenuItem(Sender);

    P := ActiveWorkbench.ScreenToClient(TPopupMenu(MenuItem.GetParentMenu()).PopupPoint);
    ActiveWorkbench.CreateNewSection(P.X, P.Y);
  end;
end;

procedure TFSession.mwDCreateForeignKeyClick(Sender: TObject);
var
  MenuItem: TMenuItem;
  P: TPoint;
begin
  Wanted.Clear();

  if (Sender is TMenuItem) then
  begin
    MenuItem := TMenuItem(Sender);

    P := ActiveWorkbench.ScreenToClient(TPopupMenu(MenuItem.GetParentMenu()).PopupPoint);
    ActiveWorkbench.CreateNewForeignKey(P.X, P.Y);
  end;
end;

procedure TFSession.mwDCreateTableClick(Sender: TObject);
var
  MenuItem: TMenuItem;
  P: TPoint;
begin
  Wanted.Clear();

  if (Sender is TMenuItem) then
  begin
    MenuItem := TMenuItem(Sender);

    P := ActiveWorkbench.ScreenToClient(TPopupMenu(MenuItem.GetParentMenu()).PopupPoint);
    ActiveWorkbench.CreateNewTable(P.X, P.Y);
  end;
end;

procedure TFSession.mwEDeleteClick(Sender: TObject);
begin
  MainAction('aEDelete').Execute();
end;

procedure TFSession.mwEPasteClick(Sender: TObject);
begin
  WorkbenchPasteExecute(Sender);
end;

procedure TFSession.MWorkbenchPopup(Sender: TObject);
var
  I: Integer;
  MenuItem: TMenuItem;
begin
  mwAddTable.Clear();
  mwEPaste.Enabled := MainAction('aEPaste').Enabled;

  ActiveWorkbench.UpdateAction(MainAction('aEDelete'));
  mwEDelete.Enabled := not (ActiveWorkbench.Selected is TWForeignKey);
  if ((ActiveWorkbench.Selected is TWTable)) then
    mwEDelete.Caption := Preferences.LoadStr(559)
  else
    mwEDelete.Caption := Preferences.LoadStr(28);


  if (not Assigned(ActiveWorkbench.Selected)) then
    for I := 0 to ActiveWorkbench.Database.Tables.Count - 1 do
      if ((ActiveWorkbench.Database.Tables[I] is TSBaseTable)
        and not Assigned(ActiveWorkbench.TableByBaseTable(TSBaseTable(ActiveWorkbench.Database.Tables[I])))) then
      begin
        MenuItem := TMenuItem.Create(Self);
        MenuItem.Caption := ActiveWorkbench.Database.Tables[I].Name;
        MenuItem.OnClick := WorkbenchAddTable;
        MenuItem.Tag := Integer(ActiveWorkbench.Database.Tables[I]);
        mwAddTable.Add(MenuItem);
      end;
  mwDProperties.Default := mwDProperties.Enabled;

  mwDCreateForeignKey.OnClick := mwDCreateForeignKeyClick;

  ShowEnabledItems(MWorkbench.Items);
end;

function TFSession.NavigatorNodeToAddress(const Node: TTreeNode): string;
var
  URI: TUURI;
begin
  URI := TUURI.Create('');

  URI.Scheme := 'mysql';
  URI.Host := Session.Connection.Host;
  if (Session.Connection.Port <> MYSQL_PORT) then
    URI.Port := Session.Connection.Port;

  if (Assigned(Node)) then
  begin
    if (not (Node.ImageIndex in [iiProcesses, iiUsers, iiVariables])) then
      URI.Param['view'] := ViewToParam(View);

    case (Node.ImageIndex) of
      iiServer:
        begin
          if ((URI.Param['view'] <> Null) and not (ParamToView(URI.Param['view']) in [vEditor, vEditor2, vEditor3])) then URI.Param['view'] := Null;
        end;
      iiDatabase,
      iiSystemDatabase:
        begin
          if ((URI.Param['view'] <> Null) and not (ParamToView(URI.Param['view']) in [vEditor, vEditor2, vEditor3]) and (URI.Param['view'] <> 'builder') and (URI.Param['view'] <> 'diagram')) then URI.Param['view'] := Null;
          URI.Database := TSDatabase(Node.Data).Name;
        end;
      iiBaseTable,
      iiSystemView:
        begin
          if ((URI.Param['view'] <> Null) and (URI.Param['view'] <> 'browser')) then URI.Param['view'] := Null;
          URI.Database := TSDatabase(Node.Parent.Data).Name;
          URI.Table := TSTable(Node.Data).Name;
        end;
      iiView:
        begin
          if ((URI.Param['view'] <> Null) and (URI.Param['view'] <> 'browser') and (URI.Param['view'] <> 'ide')) then URI.Param['view'] := ViewToParam(LastTableView);
          URI.Database := TSDatabase(Node.Parent.Data).Name;
          URI.Table := TSTable(Node.Data).Name;
        end;
      iiProcedure:
        begin
          URI.Param['view'] := 'ide';
          URI.Database := TSDatabase(Node.Parent.Data).Name;
          URI.Param['objecttype'] := 'procedure';
          URI.Param['object'] := Node.Text;
        end;
      iiFunction:
        begin
          URI.Param['view'] := 'ide';
          URI.Database := TSDatabase(Node.Parent.Data).Name;
          URI.Param['objecttype'] := 'function';
          URI.Param['object'] := Node.Text;
        end;
      iiEvent:
        begin
          URI.Param['view'] := 'ide';
          URI.Database := TSDatabase(Node.Parent.Data).Name;
          URI.Param['objecttype'] := 'event';
          URI.Param['object'] := Node.Text;
        end;
      iiField,
      iiViewField:
        begin
          URI.Param['view'] := Null;
          URI.Database := TSDatabase(Node.Parent.Parent.Data).Name;
          URI.Table := TSTable(Node.Parent.Data).Name;
        end;
      iiTrigger:
        begin
          URI.Param['view'] := 'ide';
          URI.Database := TSDatabase(Node.Parent.Parent.Data).Name;
          URI.Table := TSTrigger(Node.Parent.Data).Name;
          URI.Param['objecttype'] := 'trigger';
          URI.Param['object'] := Node.Text;
        end;
      iiProcesses:
        URI.Param['system'] := 'processes';
      iiUsers:
        URI.Param['system'] := 'users';
      iiVariables:
        URI.Param['system'] := 'variables';
    end;

    if (URI.Param['view'] <> 'browser') then
    begin
      URI.Param['offset'] := Null;
      URI.Param['filter'] := Null;
      URI.Param['search'] := Null;
    end;
    if (URI.Param['view'] <> 'ide') then
    begin
      URI.Param['objecttype'] := Null;
      URI.Param['object'] := Null;
    end;
    if (not (ParamToView(URI.Param['view']) in [vEditor, vEditor2, vEditor3])) then
    begin
      URI.Param['file'] := Null;
      URI.Param['cp'] := Null;
    end;

    if (Node = FNavigator.Selected) then
    begin
      if (URI.Param['view'] = 'browser') then
      begin
        // Debug 2016-11-23
        if (not (FNavigator.Selected.ImageIndex in [iiBaseTable, iiView, iiSystemView])) then
          raise ERangeError.Create(SRangeError);
        if (not Assigned(FNavigator.Selected.Data)) then
          raise ERangeError.Create(SRangeError);
        // Debug 2016-11-27
        if (not (TObject(FNavigator.Selected.Data) is TSTable)) then
          raise ERangeError.Create(SRangeError);

        if (TSTable(FNavigator.Selected.Data).ValidData) then
        begin
           if (Desktop(TSTable(FNavigator.Selected.Data)).Table.DataSet.Offset > 0) then
            URI.Param['offset'] := IntToStr(Desktop(TSTable(FNavigator.Selected.Data)).Table.DataSet.Offset);
          if (Desktop(TSTable(FNavigator.Selected.Data)).Table.DataSet.FilterSQL <> '') then
            URI.Param['filter'] := Desktop(TSTable(FNavigator.Selected.Data)).Table.DataSet.FilterSQL;
          if (Desktop(TSTable(FNavigator.Selected.Data)).Table.DataSet.QuickSearch <> '') then
            URI.Param['search'] := Desktop(TSTable(FNavigator.Selected.Data)).Table.DataSet.QuickSearch;
        end;
      end
      else if (ParamToView(URI.Param['view']) in [vEditor, vEditor2, vEditor3]) then
      begin
        if (SQLEditors[ParamToView(URI.Param['view'])].Filename = '') then
        begin
          URI.Param['file'] := Null;
          URI.Param['cp'] := Null;
        end
        else
        begin
          URI.Param['file'] := EscapeURL(SQLEditors[ParamToView(URI.Param['view'])].Filename);
          if (SQLEditors[ParamToView(URI.Param['view'])].FileCodePage = 0) then
            URI.Param['cp'] := Null
          else
            URI.Param['cp'] := IntToStr(SQLEditors[ParamToView(URI.Param['view'])].FileCodePage);
        end;
      end;
    end;
  end;

  // Debug 2017-01-02
  if ((URI.Param['view'] = 'ide') and (URI.Database = '')) then
    raise ERangeError.Create('Address: ' + URI.Address + #13#10
      + 'ImageIndex: ' + IntToStr(FNavigator.Selected.ImageIndex) + #13#10
      + 'Text: ' + FNavigator.Selected.Text);

  Result := URI.Address;

  URI.Free();
end;

function TFSession.ObjectSearchStep2(): Boolean;
begin
  if (Assigned(ObjectSearch)) then
    ObjectSearch.Step2();

  Result := True; // For compiler warning only...
end;

procedure TFSession.OnConvertError(Sender: TObject; Text: string);
begin
  uBase.ConvertError(Sender, Text);
end;

procedure TFSession.OpenDiagram();
begin
  OpenDialog.Title := Preferences.LoadStr(581);
  OpenDialog.InitialDir := Path;
  OpenDialog.FileName := '';
  OpenDialog.DefaultExt := 'xml';
  OpenDialog.Filter := FilterDescription('xml') + ' (*.xml)|*.xml|' + FilterDescription('*') + ' (*.*)|*.*';
  OpenDialog.Encodings.Text := '';
  OpenDialog.EncodingIndex := -1;

  if (OpenDialog.Execute()) then
    ActiveWorkbench.LoadFromFile(OpenDialog.FileName);
end;

procedure TFSession.OpenSQLFile(const AFilename: TFileName; const CodePage: Cardinal = 0; const Insert: Boolean = False);
var
  Answer: Integer;
  FileSize: TLargeInteger;
  Handle: THandle;
  Import: TTImportSQL;
  Text: string;
  URI: TUURI;
begin
  if (not (View in [vEditor, vEditor2, vEditor3])) then
    View := vEditor;

  OpenDialog.Title := Preferences.LoadStr(581);
  if (AFilename = '') then
    OpenDialog.InitialDir := Path
  else
    OpenDialog.InitialDir := ExtractFilePath(AFilename);
  OpenDialog.FileName := AFilename;
  OpenDialog.DefaultExt := 'sql';
  OpenDialog.Filter := FilterDescription('sql') + ' (*.sql)|*.sql|' + FilterDescription('*') + ' (*.*)|*.*';
  OpenDialog.Encodings.Text := EncodingCaptions();
  if (CodePage <> 0) then
    OpenDialog.EncodingIndex := OpenDialog.Encodings.IndexOf(CodePageToEncoding(CodePage))
  else
    OpenDialog.EncodingIndex := OpenDialog.Encodings.IndexOf(CodePageToEncoding(Session.Connection.CodePage));

  if ((OpenDialog.FileName <> '') or OpenDialog.Execute()) then
  begin
    Path := ExtractFilePath(OpenDialog.FileName);

    Answer := ID_CANCEL;

    Handle := CreateFile(PChar(OpenDialog.FileName),
                         GENERIC_READ,
                         FILE_SHARE_READ,
                         nil,
                         OPEN_EXISTING, 0, 0);

    if (Handle <> INVALID_HANDLE_VALUE) then
      LARGE_INTEGER(FileSize).LowPart := GetFileSize(Handle, @LARGE_INTEGER(FileSize).HighPart);

    if ((Handle = INVALID_HANDLE_VALUE) or (LARGE_INTEGER(FileSize).LowPart = INVALID_FILE_SIZE) and (GetLastError() <> 0)) then
      MsgBox(SysErrorMessage(GetLastError()), Preferences.LoadStr(45), MB_OK + MB_ICONERROR)
    else if ((ActiveSynMemo <> ActiveSynMemo) or (FileSize < TLargeInteger(LargeSQLScriptSize))) then
      Answer := ID_NO
    else
      Answer := MsgBox(Preferences.LoadStr(751), Preferences.LoadStr(101), MB_YESNOCANCEL + MB_ICONQUESTION);
    CloseHandle(Handle);
    if (Answer = ID_YES) then
    begin
      DImport.Session := Session;
      DImport.SObject := Session.DatabaseByName(SelectedDatabase);
      DImport.FileName := OpenDialog.FileName;
      DImport.CodePage := EncodingToCodePage(OpenDialog.Encodings[OpenDialog.EncodingIndex]);
      DImport.ImportType := itSQLFile;

      // Debug 2017-01-06
      if (not Assigned(FNavigator)) then
        raise ERangeError.Create(SRangeError);

      // Debug 2016-12-22
      DImport.FNavigator := @FNavigator;

      DImport.Progress := '1';
      DImport.Execute();
      DImport.Progress := DImport.Progress + '_';

      // Debug 2017-01-06
      if (not Assigned(FNavigator)) then
        raise ERangeError.Create(SRangeError);

      Wanted.Update := Session.Update;
    end
    else if (Answer = ID_NO) then
    begin
      Import := TTImportSQL.Create(OpenDialog.FileName, EncodingToCodePage(OpenDialog.Encodings[OpenDialog.EncodingIndex]), Session, nil);
      Import.OnError := ImportError;
      Import.Text := @Text;
      Import.Execute();

      if (Import.ErrorCount = 0) then
      begin
        ActiveSynMemo.Options := ActiveSynMemo.Options + [eoScrollPastEol];  // Speed up the performance
        if (Insert) then
          ActiveSynMemo.SelText := Text
        else
        begin
          ActiveSynMemo.Text := Text;
          SQLEditors[View].Filename := Import.Filename;
          SQLEditors[View].FileCodePage := Import.CodePage;
          URI := TUURI.Create(Address);
          URI.Param['file'] := EscapeURL(SQLEditors[View].Filename);
          if (SQLEditors[View].FileCodePage = 0) then
            URI.Param['cp'] := Null
          else
            URI.Param['cp'] := IntToStr(SQLEditors[View].FileCodePage);
          FAddress := URI.Address;
          AddressChanged(nil);
          URI.Free();
          Session.Account.Desktop.Files.Add(SQLEditors[View].Filename, SQLEditors[View].FileCodePage);
          Window.Perform(UM_UPDATETOOLBAR, 0, LPARAM(Self));
        end;
        if (Length(ActiveSynMemo.Lines.Text) < LargeSQLScriptSize) then
          ActiveSynMemo.Options := ActiveSynMemo.Options - [eoScrollPastEol];  // Slow down the performance on large content
        ActiveSynMemo.ClearUndo();
        ActiveSynMemo.Modified := Import.SetNamesApplied;

        Window.Perform(UM_UPDATETOOLBAR, 0, LPARAM(Self));
      end;

      Import.Free();
    end;
  end;
end;

procedure TFSession.PanelMouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  if ((Button = mbLeft) and (Sender is TPanel)) then
  begin
    PanelMouseDownPoint := Point(X, Y);
    TPanel(Sender).OnMouseMove(Sender, Shift, X, Y);
  end;
end;

procedure TFSession.PanelMouseMove(Sender: TObject; Shift: TShiftState; X,
  Y: Integer);
var
  Panel: TPanel_Ext;
  Rect: TRect;
begin
  if ((Sender is TPanel_Ext) and Assigned(CloseButtonNormal)) then
  begin
    Panel := TPanel_Ext(Sender);

    Rect.Left := GetSystemMetrics(SM_CXEDGE);
    Rect.Top := GetSystemMetrics(SM_CYEDGE);
    Rect.Width := CloseButtonNormal.Width;
    Rect.Height := CloseButtonNormal.Width;

    if (PtInRect(Rect, Point(X, Y))) then
    begin
      SetCapture(Panel.Handle);

      if (ssLeft in Shift) then
        TPanel_Ext(Sender).Canvas.Draw(Rect.Left, Rect.Top, CloseButtonPushed.Bitmap)
      else
        TPanel_Ext(Sender).Canvas.Draw(Rect.Left, Rect.Top, CloseButtonHot.Bitmap)
    end
    else if (ReleaseCapture()) then
      TPanel_Ext(Sender).Canvas.Draw(Rect.Left, Rect.Top, CloseButtonNormal.Bitmap);
  end;
end;

procedure TFSession.PanelMouseUp(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
var
  Panel: TPanel_Ext;
  Rect: TRect;
begin
  if ((Button = mbLeft) and (Sender is TPanel_Ext) and Assigned(CloseButtonNormal)) then
  begin
    Panel := TPanel_Ext(Sender);

    if (Sender = PHeader) then
    begin
      Rect.Left := Panel.ClientWidth - GetSystemMetrics(SM_CXEDGE) - (GetSystemMetrics(SM_CXSIZE) - 2 *GetSystemMetrics(SM_CXEDGE));
      Rect.Top := GetSystemMetrics(SM_CYEDGE);
      Rect.Width := GetSystemMetrics(SM_CXSIZE) - 2 * GetSystemMetrics(SM_CXEDGE);
      Rect.Height := GetSystemMetrics(SM_CYSIZE) - 2 * GetSystemMetrics(SM_CYEDGE);
    end
    else
    begin
      Rect.Left := GetSystemMetrics(SM_CXEDGE);
      Rect.Top := GetSystemMetrics(SM_CYEDGE);
      Rect.Width := CloseButtonNormal.Width;
      Rect.Height := CloseButtonNormal.Width;
    end;

    if (PtInRect(Rect, Point(X, Y)) and PtInRect(Rect, PanelMouseDownPoint)) then
      if (Sender = PHeader) then
        PostMessage(Handle, UM_CLOSE_FRAME, 0, 0)
      else if (Sender = PResultHeader) then
      begin
        SResult.Visible := False;
        PResult.Visible := False;
        case (View) of
          vIDE: if (FNavigator.Selected.ImageIndex in [iiProcedure, iiFunction]) then Desktop(TSRoutine(FNavigator.Selected.Data)).CloseIDEResult();
          vBuilder: Desktop(TSDatabase(FNavigator.Selected.Data)).CloseBuilderResult();
          vEditor,
          vEditor2,
          vEditor3: SQLEditors[View].CloseResult();
        end;
        ActiveDBGrid := nil;
      end
      else if (Sender = PLogHeader) then
        MainAction('aVSQLLog').Execute();
    PanelMouseDownPoint := Point(-1, -1);
  end;
end;

procedure TFSession.PanelPaint(Sender: TObject);
var
  Rect: TRect;
begin
  if (Assigned(CloseButtonNormal)) then
  begin
    Rect.Left := GetSystemMetrics(SM_CXEDGE);
    Rect.Top := GetSystemMetrics(SM_CYEDGE);
    Rect.Width := CloseButtonNormal.Width;
    Rect.Height := CloseButtonNormal.Width;

    TPanel_Ext(Sender).Canvas.Draw(Rect.Left, Rect.Top, CloseButtonNormal.Bitmap)
  end;
end;

procedure TFSession.PanelResize(Sender: TObject);
var
  ClientControl: TWinControl;
  Control: TWinControl;
  I: Integer;
  NewHeight: Integer;
  ReduceControl: TWinControl;
  ToReduceHeight: Integer;
begin
  if (Sender is TWinControl) then
  begin
    Control := TWinControl(Sender);

    ClientControl := nil;
    NewHeight := Control.ClientHeight;
    for I := 0 to Control.ControlCount - 1 do
      if (Control.Controls[I].Visible) then
        if ((Control.Controls[I].Align = alClient) and (Control.Controls[I] is TWinControl)) then
          ClientControl := TWinControl(Control.Controls[I])
        else if (Control.Controls[I].Align in [alTop, alBottom]) then
          Dec(NewHeight, Control.Controls[I].Height);

    if (Assigned(ClientControl) and (NewHeight < ClientControl.Constraints.MinHeight)) then
    begin
      ToReduceHeight := ClientControl.Constraints.MinHeight - NewHeight;

      ReduceControl := nil;
      for I := 0 to Control.ControlCount - 1 do
        if (Control.Controls[I].Visible and (Control.Controls[I].Align = alBottom) and (Control.Controls[I].Height > Control.Controls[I].Constraints.MinHeight)) then
          if (not Assigned(ReduceControl) or (Control.Controls[I].Top > ReduceControl.Top) and (Control.Controls[I] <> SResult)) then
            ReduceControl := TWinControl(Control.Controls[I]);
      if (Assigned(ReduceControl)) then
        ReduceControl.Height := ReduceControl.Height - ToReduceHeight;
    end;
  end;
end;

function TFSession.ParamToView(const AParam: Variant): TView;
begin
  if (AParam = 'browser') then
    Result := vBrowser
  else if (AParam = 'ide') then
    Result := vIDE
  else if (AParam = 'builder') then
    Result := vBuilder
  else if (AParam = 'diagram') then
    Result := vDiagram
  else if (AParam = 'editor') then
    Result := vEditor
  else if (AParam = 'editor2') then
    Result := vEditor2
  else if (AParam = 'editor3') then
    Result := vEditor3
  else if (AParam = 'objectsearch') then
    Result := vObjectSearch
  else
    Result := vObjects;
end;

procedure TFSession.PasteExecute(const Node: TTreeNode; const Objects: string);
var
  Database: TSDatabase;
  Found: Boolean;
  I: Integer;
  J: Integer;
  Name: string;
  NewField: TSTableField;
  NewForeignKey: TSForeignKey;
  NewKey: TSKey;
  NewTable: TSBaseTable;
  NewTrigger: TSTrigger;
  SourceDatabase: TSDatabase;
  SourceField: TSField;
  SourceForeignKey: TSForeignKey;
  SourceKey: TSKey;
  SourceRoutine: TSRoutine;
  SourceSession: TSSession;
  SourceTable: TSBaseTable;
  SourceTrigger: TSTrigger;
  SourceURI: TUURI;
  SourceUser: TSUser;
  SourceView: TSView;
  StringList: TStringList;
  Success: Boolean;
  Table: TSBaseTable;
begin
  StringList := TStringList.Create();
  StringList.Text := Objects;

  if (StringList.Count > 0) then
  begin
    SourceURI := TUURI.Create(StringList.Values['Address']);
    SourceSession := Sessions.SessionByAccount(Accounts.AccountByURI(SourceURI.Address, Session.Account));
    if (not Assigned(SourceSession) and Assigned(Accounts.AccountByURI(SourceURI.Address))) then
    begin
      SourceSession := TSSession.Create(Sessions, Accounts.AccountByURI(SourceURI.Address));
      DConnecting.Session := SourceSession;
      if (not DConnecting.Execute()) then
        FreeAndNil(SourceSession);
    end;

    if (Assigned(SourceSession)) then
    begin
      Success := True;

      case (Node.ImageIndex) of
        iiDatabase:
          begin
            DExecutingSQL.Session := SourceSession;
            DExecutingSQL.Update := SourceSession.Databases.Update;

            if (SourceSession.Databases.Valid or DExecutingSQL.Execute()) then
            begin
              SourceDatabase := SourceSession.DatabaseByName(SourceURI.Database);

              if (not Assigned(SourceDatabase)) then
                MessageBeep(MB_ICONERROR)
              else
              begin
                Database := TSDatabase(Node.Data);

                Found := False;
                for I := 1 to StringList.Count - 1 do
                  Found := Found or (StringList.Names[I] = 'Table');

                DExecutingSQL.Update := SourceDatabase.Tables.Update;
                if (not Assigned(Database) or not SourceDatabase.Tables.Valid and not DExecutingSQL.Execute()) then
                  MessageBeep(MB_ICONERROR)
                else if (not Found or DPaste.Execute()) then
                begin
                  if (Found and (SourceSession <> Session)) then
                    MessageBeep(MB_ICONERROR)
                  else
                    for I := 1 to StringList.Count - 1 do
                      if (Success) then
                        if (StringList.Names[I] = 'Table') then
                        begin
                          SourceTable := SourceDatabase.BaseTableByName(StringList.ValueFromIndex[I]);

                          if (not Assigned(SourceTable)) then
                            MessageBeep(MB_ICONERROR)
                          else
                          begin
                            Name := Session.TableName(CopyName(SourceTable.Name, Database.Tables));

                            Session.Connection.BeginSynchron();
                            Success := Database.CloneTable(SourceTable, Name, DPaste.Data);
                            Session.Connection.EndSynchron();
                          end;
                        end;
                  for I := 1 to StringList.Count - 1 do
                    if (Success) then
                      if (StringList.Names[I] = 'View') then
                      begin
                        SourceView := SourceDatabase.ViewByName(StringList.ValueFromIndex[I]);

                        if (not Assigned(SourceView)) then
                          MessageBeep(MB_ICONERROR)
                        else
                        begin
                          Name := CopyName(SourceView.Name, Database.Tables);
                          if (Session.LowerCaseTableNames = 1) then
                            Name := LowerCase(Name);

                          Session.Connection.BeginSynchron();
                          Success := Database.CloneTable(SourceView, Name, False);
                          Session.Connection.EndSynchron();
                        end;
                      end;
                  for I := 1 to StringList.Count - 1 do
                    if (Success) then
                      if (StringList.Names[I] = 'Procedure') then
                      begin
                        SourceRoutine := SourceDatabase.ProcedureByName(StringList.ValueFromIndex[I]);

                        if (not Assigned(SourceRoutine)) then
                          MessageBeep(MB_ICONERROR)
                        else
                        begin
                          Name := SourceRoutine.Name;
                          J := 1;
                          while (Assigned(Database.ProcedureByName(Name))) do
                          begin
                            if (J = 1) then
                              Name := Preferences.LoadStr(680, SourceRoutine.Name)
                            else
                              Name := Preferences.LoadStr(681, SourceRoutine.Name, IntToStr(J));
                            Name := ReplaceStr(Name, ' ', '_');
                            Inc(J);
                          end;

                          Session.Connection.BeginSynchron();
                          Success := Database.CloneRoutine(SourceRoutine, Name);
                          Session.Connection.EndSynchron();
                        end;
                      end
                      else if (StringList.Names[I] = 'Function') then
                      begin
                        SourceRoutine := SourceDatabase.FunctionByName(StringList.ValueFromIndex[I]);

                        if (not Assigned(SourceRoutine)) then
                          MessageBeep(MB_ICONERROR)
                        else
                        begin
                          Name := SourceRoutine.Name;
                          J := 1;
                          while (Assigned(Database.FunctionByName(Name))) do
                          begin
                            if (J = 1) then
                              Name := Preferences.LoadStr(680, SourceRoutine.Name)
                            else
                              Name := Preferences.LoadStr(681, SourceRoutine.Name, IntToStr(J));
                            Name := ReplaceStr(Name, ' ', '_');
                            Inc(J);
                          end;

                          Session.Connection.BeginSynchron();
                          Success := Database.CloneRoutine(SourceRoutine, Name);
                          Session.Connection.EndSynchron();
                        end;
                      end;
                end;
              end;
            end;
          end;
        iiBaseTable:
          begin
            SourceDatabase := SourceSession.DatabaseByName(SourceURI.Database);

            if (not Assigned(SourceDatabase)) then
              MessageBeep(MB_ICONERROR)
            else
            begin
              DExecutingSQL.Session := SourceSession;
              DExecutingSQL.Update := SourceDatabase.Tables.Update;
              if (not Assigned(SourceDatabase) or not SourceDatabase.Tables.Valid and not DExecutingSQL.Execute()) then
                SourceTable := nil
              else
                SourceTable := SourceDatabase.BaseTableByName(SourceURI.Table);

              DExecutingSQL.Update := SourceTable.Update;
              if (not Assigned(SourceTable) or not SourceTable.Valid and not DExecutingSQL.Execute()) then
                MessageBeep(MB_ICONERROR)
              else
              begin
                Database := TSDatabase(Node.Parent.Data);
                Table := TSBaseTable(Node.Data);

                DExecutingSQL.Update := Table.Update;
                if (Table.Valid or DExecutingSQL.Execute()) then
                begin
                  NewTable := TSBaseTable.Create(Database.Tables);
                  NewTable.Assign(Table);

                  for I := 1 to StringList.Count - 1 do
                    if (StringList.Names[I] = 'Field') then
                    begin
                      Name := CopyName(StringList.ValueFromIndex[I], NewTable.Fields);

                      SourceField := SourceTable.FieldByName(StringList.ValueFromIndex[I]);

                      if (not Assigned(SourceField)) then
                        MessageBeep(MB_ICONERROR)
                      else
                      begin
                        NewField := TSBaseTableField.Create(NewTable.Fields);
                        NewField.Assign(SourceField);
                        TSBaseTableField(NewField).OriginalName := '';
                        NewField.Name := Name;
                        NewField.FieldBefore := NewTable.Fields[NewTable.Fields.Count - 1];
                        NewTable.Fields.AddField(NewField);
                        NewField.Free();
                      end;
                    end;

                  for I := 1 to StringList.Count - 1 do
                    if (StringList.Names[I] = 'Key') then
                    begin
                      Name := CopyName(StringList.ValueFromIndex[I], NewTable.Keys);

                      SourceKey := SourceTable.KeyByName(StringList.ValueFromIndex[I]);

                      if (not Assigned(SourceKey)) then
                        MessageBeep(MB_ICONERROR)
                      else
                      begin
                        Found := True;
                        for J := 0 to SourceKey.Columns.Count - 1 do
                          if (not Assigned(NewTable.FieldByName(SourceKey.Columns[J].Field.Name))) then
                            Found := False;
                        if (not Found) then
                          MessageBeep(MB_ICONERROR)
                        else
                        begin
                          NewKey := TSKey.Create(NewTable.Keys);
                          NewKey.Assign(SourceKey);
                          NewKey.Name := Name;
                          NewTable.Keys.AddKey(NewKey);
                          NewKey.Free();
                        end;
                      end;
                    end
                    else if (StringList.Names[I] = 'ForeignKey') then
                    begin
                      Name := CopyName(StringList.ValueFromIndex[I], NewTable.ForeignKeys);

                      SourceForeignKey := SourceTable.ForeignKeyByName(StringList.ValueFromIndex[I]);

                      if (not Assigned(SourceForeignKey)) then
                        MessageBeep(MB_ICONERROR)
                      else
                      begin
                        NewForeignKey := TSForeignKey.Create(NewTable.ForeignKeys);
                        NewForeignKey.Assign(SourceForeignKey);
                        NewForeignKey.Name := Name;
                        NewTable.ForeignKeys.AddForeignKey(NewForeignKey);
                        NewForeignKey.Free();
                      end;
                    end;

                  Session.Connection.BeginSynchron();
                  Database.UpdateBaseTable(Table, NewTable);
                  Session.Connection.EndSynchron();

                  for I := 1 to StringList.Count - 1 do
                    if (StringList.Names[I] = 'Trigger') then
                    begin
                      DExecutingSQL.Session := SourceSession;
                      DExecutingSQL.Update := SourceDatabase.Triggers.Update;
                      if (not Assigned(SourceDatabase) or not SourceDatabase.Triggers.Valid and not DExecutingSQL.Execute()) then
                        SourceTrigger := nil
                      else
                        SourceTrigger := SourceDatabase.TriggerByName(StringList.ValueFromIndex[I]);

                      if (not Assigned(SourceTrigger)) then
                        MessageBeep(MB_ICONERROR)
                      else
                      begin
                        Name := CopyName(StringList.ValueFromIndex[I], Database.Triggers);

                        NewTrigger := TSTrigger.Create(Database.Triggers);
                        NewTrigger.Assign(SourceTrigger);
                        NewTrigger.Name := Name;
                        NewTrigger.TableName := NewTable.Name;
                        Session.Connection.BeginSynchron();
                        Database.AddTrigger(NewTrigger);
                        Session.Connection.EndSynchron();
                        NewTrigger.Free();
                      end;
                    end;

                  NewTable.Free();
                end;
              end;
            end;
          end;
      end;

      SourceURI.Free();
    end;
  end;
  StringList.Free();
end;

procedure TFSession.PContentChange(Sender: TObject);

  procedure DisableAligns(const Control: TWinControl);
  var
    I: Integer;
  begin
    // Debug 2016-12-28
    if (not Assigned(Control)) then
      raise ERangeError.Create(SRangeError);

    if (Control.Top < 0) then Control.Top := 0;
try
    SendMessage(Control.Handle, WM_MOVE, 0, MAKELPARAM(Control.Left, Control.Top));
except
  on E: Exception do
    raise ERangeError.Create('Left: ' + IntToStr(Control.Left) + #13#10
      + 'Top: ' + IntToStr(Control.Top) + #13#10
      + E.Message);
end;
    Control.DisableAlign();
    for I := 0 to Control.ControlCount - 1 do
      if (Control.Controls[I] is TWinControl) then
        DisableAligns(TWinControl(Control.Controls[I]));
  end;

  procedure EnableAligns(const Control: TWinControl);
  var
    I: Integer;
  begin
    for I := 0 to Control.ControlCount - 1 do
      if (Control.Controls[I] is TWinControl) then
        EnableAligns(TWinControl(Control.Controls[I]));
    if (Control.AlignDisabled) then
      Control.EnableAlign();
  end;

var
  I: Integer;
  NewTop: Integer;
  OldActiveControl: TWinControl;
  PResultVisible: Boolean;
begin
  for I := 0 to Session.Databases.Count - 1 do
    if (Assigned(Desktop(Session.Databases[I]).FWorkbench)) then
      Desktop(Session.Databases[I]).Workbench.Visible := Session.Databases[I].Name = SelectedDatabase;

  if (Sender <> Self) then
  begin
    if (PResult.Align = alBottom) then
      PResultHeight := PResult.Height;

    OldActiveControl := Window.ActiveControl;

    // Debug 2016-12-28
    if (not Assigned(PContent)) then
      raise ERangeError.Create(SRangeError);
    DisableAligns(PContent);

    if (PListView.Align = alClient) then PListView.Align := alNone;
    if (PQueryBuilder.Align = alClient) then PQueryBuilder.Align := alNone;
    if (PSynMemo.Align = alClient) then PSynMemo.Align := alNone;
    if (PResult.Align = alClient) then PResult.Align := alNone;
    PListView.Align := alNone;
    PDataBrowser.Align := alNone;
    PObjectIDE.Align := alNone;
    PQueryBuilder.Align := alNone;
    PSynMemo.Align := alNone;
    SResult.Align := alNone;
    PResult.Align := alNone;

    // Debug 2016-12-27
    if (not Assigned(SBlob)) then
      raise ERangeError.Create('Destroying: ' + BoolToStr(csDestroying in ComponentState, True) + #13#10
        + 'Assigned(SBlobDebug): ' + BoolToStr(Assigned(SBlobDebug), True) + #13#10
        + 'SBlob = SBlobDebug: ' + BoolToStr(SBlob = SBlobDebug, True) + #13#10
        + 'Assigned(PBlob): ' + BoolToStr(Assigned(PBlob), True));
      // Occurred on 2017-01-04: SBlob <> SBlobDebug ... DROP TABLE was the last stmt

    SBlob.Align := alNone;
    PBlob.Align := alNone;

    PBlob.Visible := False;


    EnableAligns(PContent);

    if (View in [vObjects, vObjectSearch]) then ActiveListView := GetActiveListView() else ActiveListView := nil;
    if (View in [vIDE]) then ActiveIDEInputDataSet := GetActiveIDEInputDataSet() else ActiveIDEInputDataSet := nil;
    if (View in [vBrowser, vIDE, vBuilder, vEditor, vEditor2, vEditor3]) then ActiveSynMemo := GetActiveSynMemo() else ActiveSynMemo := nil;
    if (View in [vBrowser, vIDE, vBuilder, vEditor, vEditor2, vEditor3]) then ActiveDBGrid := GetActiveDBGrid() else ActiveDBGrid := nil;
    if (View in [vDiagram]) then ActiveWorkbench := GetActiveWorkbench() else ActiveWorkbench := nil;

    if ((View = vBrowser) and Assigned(FNavigator.Selected) and (TObject(FNavigator.Selected.Data) is TSTable)) then
    begin
      FUDOffset.Position := 0;
      FUDLimit.Position := Desktop(TSTable(FNavigator.Selected.Data)).Limit;
      FLimitEnabled.Down := Desktop(TSTable(FNavigator.Selected.Data)).Limited;

      PDataBrowser.Top := 0;
      PDataBrowser.Align := alTop;
      PDataBrowser.Visible := True;
    end
    else
      PDataBrowser.Visible := False;

    if (Assigned(ActiveIDEInputDataSet)) then
    begin
      PObjectIDETrigger.Visible := (SelectedImageIndex = iiTrigger);
      PObjectIDEResize(Sender);

      PObjectIDE.Top := 0;
      PObjectIDE.Align := alTop;
      PObjectIDE.Visible := True;
    end
    else
      PObjectIDE.Visible := False;

    if (not Assigned(FNavigator.Selected)) then
      PResultVisible := True
    else
      case (View) of
        vBrowser:
          PResultVisible := True;
        vIDE:
          case (FNavigator.Selected.ImageIndex) of
            iiProcedure,
            iiFunction:
              PResultVisible := Assigned(Desktop(TSRoutine(FNavigator.Selected.Data)).ActiveDBGrid);
            else PResultVisible := False;
          end;
        vBuilder:
          PResultVisible := Assigned(Desktop(TSDatabase(FNavigator.Selected.Data)).BuilderDBGrid);
        vEditor,
        vEditor2,
        vEditor3:
          PResultVisible := Assigned(SQLEditors[View]) and Assigned(SQLEditors[View].ActiveDBGrid);
        else PResultVisible := False;
      end;

    FText.OnChange := nil;
    if (Assigned(OldActiveControl) and (PResultVisible or (OldActiveControl = FObjectIDEGrid)) and (OldActiveControl is TMySQLDBGrid) and (TMySQLDBGrid(OldActiveControl).SelectedField = EditorField)) then
    begin
      if ((OldActiveControl = FObjectIDEGrid) and (PBlob.Parent <> PContent)) then
        PBlob.Parent := PContent
      else if ((OldActiveControl <> FObjectIDEGrid) and (PBlob.Parent <> PResult)) then
        PBlob.Parent := ActiveDBGrid.Parent;
      NewTop := PBlob.Parent.ClientHeight - PBlob.Height;
      for I := 0 to PBlob.Parent.ControlCount - 1 do
        case (PBlob.Parent.Controls[I].Align) of
          alClient: NewTop := Max(NewTop, PBlob.Parent.Controls[I].Constraints.MinHeight);
          alBottom: Dec(NewTop, PBlob.Parent.Controls[I].Height);
        end;
      NewTop := Max(NewTop, PResult.Constraints.MinHeight);
      PBlob.Top := NewTop;
      PBlob.Height := PBlob.Parent.ClientHeight - PBlob.Top;
      PBlob.Align := alBottom;
      PBlob.Visible := True;
    end
    else
    begin
      PBlob.Visible := False;
      PBlob.Parent := PContent;
    end;
    FText.OnChange := FTextChange;

    if (PBlob.Visible) then
    begin
      SBlob.Parent := PBlob.Parent;
      SBlob.Top := PBlob.Top - SBlob.Height;
      SBlob.Align := alBottom;
      SBlob.Visible := True;
    end
    else
    begin
      SBlob.Visible := False;
      SBlob.Parent := nil;
    end;

    if (PResultVisible and Assigned(ActiveDBGrid) and Assigned(ActiveDBGrid.DataSource.DataSet) and ActiveDBGrid.DataSource.DataSet.Active) then
    begin
      NewTop := PContent.ClientHeight - PResult.Height;
      for I := 0 to PContent.ControlCount - 1 do
        if (PContent.Controls[I].Align = alBottom) then
          Dec(NewTop, PContent.Controls[I].Height);
      PResult.Top := NewTop;
      if (View = vBrowser) then
      begin
        PResult.Align := alClient;
        PResult.Left := 0;
        PResult.Height := PContent.ClientHeight - PResult.Top;
        PResult.Width := PContent.ClientWidth
      end
      else
      begin
        PResult.Align := alBottom;
        PResult.Height := PResultHeight;
      end;
      PResultHeader.Visible := View <> vBrowser;

      PResult.Visible := True;
    end
    else
      PResult.Visible := False;

    if (PResult.Visible and (PResult.Align = alBottom)) then
    begin
      NewTop := PContent.ClientHeight - SResult.Height;
      for I := 0 to PContent.ControlCount - 1 do
        if (PContent.Controls[I].Align = alBottom) then
          Dec(NewTop, PContent.Controls[I].Height);
      SResult.Top := NewTop;
      SResult.Align := alBottom;
      SResult.Visible := True;
    end
    else
      SResult.Visible := False;

    if (View = vDiagram) then
    begin
      PWorkbench.Align := alClient;
      PWorkbench.Visible := True;
      PWorkbench.BringToFront();
    end
    else
      PWorkbench.Visible := False;

    if ((View = vBuilder) and (SelectedImageIndex in [iiServer, iiDatabase, iiSystemDatabase])) then
    begin
      PQueryBuilder.Align := alClient;
      PQueryBuilder.Visible := True;
    end
    else
      PQueryBuilder.Visible := False;

    if ((View in [vEditor, vEditor2, vEditor3]) and (SelectedImageIndex in [iiServer, iiDatabase, iiSystemDatabase])
      or (View = vIDE) and (SelectedImageIndex in [iiView, iiFunction, iiProcedure, iiEvent, iiTrigger])) then
    begin
      if (Assigned(ActiveSynMemo)) then ActiveSynMemo.BringToFront();
      PSynMemo.Align := alClient;
      PSynMemo.Visible := True;
    end
    else
      PSynMemo.Visible := False;

    if ((View = vObjects) and not (SelectedImageIndex in [iiKey, iiField, iiVirtualField, iiForeignKey])
      or ((View = vBrowser) and (SelectedImageIndex = iiServer))
      or (View = vObjectSearch)) then
    begin
      PListView.Align := alClient;
      PListView.Visible := True;
    end
    else
      PListView.Visible := False;
  end;

  PObjectIDEResize(Sender);
  PanelResize(PContent);
  if (Assigned(PResult.OnResize)) then PResult.OnResize(PResult);
end;

procedure TFSession.PContentResize(Sender: TObject);
begin
  SetWindowLong(FNavigator.Handle, GWL_STYLE, GetWindowLong(FNavigator.Handle, GWL_STYLE) or TVS_NOHSCROLL);
  SetWindowLong(FSQLHistory.Handle, GWL_STYLE, GetWindowLong(FSQLHistory.Handle, GWL_STYLE) or TVS_NOHSCROLL);
  if (Assigned(FFolders)) then
    SetWindowLong(FFolders.Handle, GWL_STYLE, GetWindowLong(FFolders.Handle, GWL_STYLE) or TVS_NOHSCROLL);

  PanelResize(Sender);
  Toolbar.Left := ClientWidth - PContent.Width;
end;

procedure TFSession.PDataBrowserResize(Sender: TObject);
var
  I: Integer;
  Msg: TMsg;
begin
  // With higher DPI system, the width of the following components are not
  // applyed in a "frame" (Delphi XE4). So there is the PDataBrowserDummy as
  // a "form" to get the correct values...

  if (not (PeekMessage(Msg, 0, 0, 0, PM_NOREMOVE) and (Msg.Message = WM_MOUSEMOVE) and (Msg.wParam = MK_LBUTTON))) then
  begin
    for I := 0 to PDataBrowser.ControlCount - 1 do
      if (PDataBrowser.Controls[I] <> PDataBrowserSpacer) then
        PDataBrowser.Controls[I].Height := PDataBrowser.ClientHeight - PDataBrowserSpacer.Height;

    FOffset.Left := PDataBrowserDummy.FOffset.Left;
    FOffset.Width := PDataBrowserDummy.FOffset.Width;
    FUDOffset.Left := FOffset.Left + FOffset.Width;
    FUDOffset.Width := PDataBrowserDummy.FUDOffset.Width;
    FLimit.Left := FUDOffset.Left + FUDOffset.Width;
    FLimit.Width := PDataBrowserDummy.FLimit.Width;
    FUDLimit.Left := FLimit.Left + FLimit.Width;
    FUDLimit.Width := PDataBrowserDummy.FUDLimit.Width;
    TBLimitEnabled.Left := FUDLimit.Left + FUDLimit.Width;
    TBLimitEnabled.Width := TBLimitEnabled.Height;

    TBQuickSearchEnabled.Left := PDataBrowser.ClientWidth - PDataBrowserDummy.TBQuickSearchEnabled.Width - GetSystemMetrics(SM_CXVSCROLL);
    TBQuickSearchEnabled.Width := PDataBrowserDummy.TBQuickSearchEnabled.Width;
    TBFilterEnabled.Width := TBFilterEnabled.Height;
    FQuickSearch.Left := TBQuickSearchEnabled.Left - PDataBrowserDummy.FQuickSearch.Width;
    FQuickSearch.Width := PDataBrowserDummy.FQuickSearch.Width;

    TBFilterEnabled.Left := FQuickSearch.Left - PDataBrowserDummy.TBFilterEnabled.Width;
    TBFilterEnabled.Width := PDataBrowserDummy.TBFilterEnabled.Width;
    FFilter.Left := PDataBrowserDummy.FFilter.Left;
    FFilter.Width := TBFilterEnabled.Left - FFilter.Left;
  end;
end;

procedure TFSession.PGridResize(Sender: TObject);
begin
  if (Assigned(ActiveDBGrid)) then
    ActiveDBGrid.Invalidate();
end;

procedure TFSession.PHeaderCheckElements(Sender: TObject);
begin
  {$IFDEF Debug}
  FObjectSearch.Visible := (Session.Connection.MySQLVersion >= 50002)
    and (View in [vObjects, vObjectSearch])
    and (FObjectSearch.Left > Toolbar.Left + Toolbar.Width + GetSystemMetrics(SM_CXFIXEDFRAME))
    and Assigned(FNavigator.Selected) and (FNavigator.Selected.ImageIndex in [iiServer, iiDatabase, iiBaseTable, iiView]);
  TBObjectSearch.Visible := FObjectSearch.Visible;
  if (not FObjectSearch.Visible and Assigned(PObjectSearch)) then
    PObjectSearch.Hide();
  {$ENDIF}
end;

procedure TFSession.PHeaderMouseMove(Sender: TObject; Shift: TShiftState; X,
  Y: Integer);
var
  Panel: TPanel_Ext;
  Rect: TRect;
begin
  if (Sender is TPanel_Ext) then
  begin
    Panel := TPanel_Ext(Sender);

    Rect.Left := Panel.ClientWidth - GetSystemMetrics(SM_CXEDGE) - (GetSystemMetrics(SM_CXSIZE) - 2 *GetSystemMetrics(SM_CXEDGE));
    Rect.Top := GetSystemMetrics(SM_CYEDGE);
    Rect.Width := GetSystemMetrics(SM_CXSIZE) - 2 * GetSystemMetrics(SM_CXEDGE);
    Rect.Height := GetSystemMetrics(SM_CYSIZE) - 2 * GetSystemMetrics(SM_CXEDGE);

    if (PtInRect(Rect, Point(X, Y))) then
    begin
      SetCapture(Panel.Handle);

      if (ssLeft in Shift) then
        if (StyleServices.Enabled) then
          StyleServices.DrawElement(TPanel_Ext(Sender).Canvas.Handle, StyleServices.GetElementDetails(twSmallCloseButtonPushed), Rect)
        else
          DrawFrameControl(TPanel_Ext(Sender).Canvas.Handle, Rect, DFC_CAPTION, DFCS_CAPTIONCLOSE or DFCS_PUSHED)
      else
        if (StyleServices.Enabled) then
          StyleServices.DrawElement(TPanel_Ext(Sender).Canvas.Handle, StyleServices.GetElementDetails(twSmallCloseButtonHot), Rect)
        else
          DrawFrameControl(TPanel_Ext(Sender).Canvas.Handle, Rect, DFC_CAPTION, DFCS_CAPTIONCLOSE or DFCS_HOT)
    end
    else if (ReleaseCapture()) then
      if (StyleServices.Enabled) then
        StyleServices.DrawElement(TPanel_Ext(Sender).Canvas.Handle, StyleServices.GetElementDetails(twSmallCloseButtonNormal), Rect)
      else
        DrawFrameControl(TPanel_Ext(Sender).Canvas.Handle, Rect, DFC_CAPTION, DFCS_CAPTIONCLOSE);
  end;
end;

procedure TFSession.PHeaderPaint(Sender: TObject);
var
  Panel: TPanel_Ext;
  Rect: TRect;
begin
  Panel := TPanel_Ext(Sender);

  Rect.Left := Panel.ClientWidth - GetSystemMetrics(SM_CXEDGE) - (GetSystemMetrics(SM_CXSIZE) - 2 * GetSystemMetrics(SM_CXEDGE));
  Rect.Top := GetSystemMetrics(SM_CYEDGE);
  Rect.Width := GetSystemMetrics(SM_CXSIZE) - 2 * GetSystemMetrics(SM_CXEDGE);
  Rect.Height := GetSystemMetrics(SM_CYSIZE) - 2 * GetSystemMetrics(SM_CXEDGE);

  if (StyleServices.Enabled) then
    StyleServices.DrawElement(TPanel_Ext(Sender).Canvas.Handle, StyleServices.GetElementDetails(twSmallCloseButtonNormal), Rect)
  else if (Sender is TPanel_Ext) then
    DrawFrameControl(TPanel_Ext(Sender).Canvas.Handle, Rect, DFC_CAPTION, DFCS_CAPTIONCLOSE);

  if (StyleServices.Enabled) then
  begin
    PHeader.Canvas.Pen.Color := SplitColor;
    PHeader.Canvas.MoveTo(0, PHeader.ClientHeight - 1);
    PHeader.Canvas.LineTo(PHeader.ClientWidth, PHeader.ClientHeight - 1);
  end;
end;

procedure TFSession.PHeaderResize(Sender: TObject);
begin
  TBObjectSearch.Left := PHeader.ClientWidth - GetSystemMetrics(SM_CXEDGE) - (GetSystemMetrics(SM_CXSIZE) - 2 * GetSystemMetrics(SM_CXEDGE)) - GetSystemMetrics(SM_CXEDGE) - TBObjectSearch.Width;
  TBObjectSearch.Top := 0;
  TBObjectSearch.Height := Toolbar.Height;
  FObjectSearch.Left := TBObjectSearch.Left - FObjectSearch.Width;
  FObjectSearch.Top := 2 * Toolbar.BorderWidth;
  FObjectSearch.Height := Toolbar.Height - 2 * 2 * Toolbar.BorderWidth;
  if (Assigned(PObjectSearch)) then
  begin
    PObjectSearch.Left := ClientToScreen(Point(FObjectSearch.Left + FObjectSearch.Width - PObjectSearch.Width, 0)).X;
    PObjectSearch.Top := ClientToScreen(Point(0, FObjectSearch.Top + FObjectSearch.Height)).Y;
  end;

  PHeaderCheckElements(Sender);
end;

procedure TFSession.PLogResize(Sender: TObject);
begin
  if (PLog.Visible and (FLog.Lines.Count > 0)) then
    PostMessage(FLog.Handle, WM_VSCROLL, SB_BOTTOM, 0);
end;

procedure TFSession.PObjectIDEResize(Sender: TObject);
var
  I: Integer;
  NewHeight: Integer;
  ScrollBarInfo: TScrollBarInfo;
begin
  NewHeight := 0;

  Inc(NewHeight, FObjectIDEGrid.DefaultRowHeight);
  if (dgRowLines in FObjectIDEGrid.Options) then
    Inc(NewHeight, FObjectIDEGrid.GridLineWidth);
  Inc(NewHeight, FObjectIDEGrid.DefaultRowHeight);

  ZeroMemory(@ScrollBarInfo, SizeOf(ScrollBarInfo));
  ScrollBarInfo.cbSize := SizeOf(ScrollBarInfo);
  GetScrollBarInfo(FObjectIDEGrid.Handle, Integer(OBJID_HSCROLL), ScrollBarInfo);
  if (ScrollBarInfo.rgstate[0] <> STATE_SYSTEM_INVISIBLE) then
    Inc(NewHeight, GetSystemMetrics(SM_CYHSCROLL));

  for I := 0 to PObjectIDE.ControlCount - 1 do
    if (PObjectIDE.Controls[I].Visible and (PObjectIDE.Controls[I].Align in [alTop, alClient, alBottom]) and (PObjectIDE.Controls[I] <> FObjectIDEGrid)) then
      Inc(NewHeight, PObjectIDE.Controls[I].Height);
  PObjectIDE.Height := NewHeight;
end;

function TFSession.PostObject(Sender: TObject): Boolean;
var
  Database: TSDatabase;
  Event: TSEvent;
  NewEvent: TSEvent;
  NewRoutine: TSRoutine;
  NewTrigger: TSTrigger;
  NewView: TSView;
  Routine: TSRoutine;
  Trigger: TSTrigger;
  View: TSView;
begin
  Database := Session.DatabaseByName(SelectedDatabase);

  case (SelectedImageIndex) of
    iiView:
      begin
        View := TSView(FNavigator.Selected.Data);

        NewView := TSView.Create(Database.Tables);
        NewView.Assign(View);

        NewView.Stmt := Trim(ActiveSynMemo.Text);

        Result := Database.UpdateView(View, NewView);

        NewView.Free();
      end;
    iiProcedure,
    iiFunction:
      begin
        Routine := TSRoutine(FNavigator.Selected.Data);

        if (SelectedImageIndex = iiProcedure) then
          NewRoutine := TSProcedure.Create(Routine.Database.Routines)
        else
          NewRoutine := TSFunction.Create(Routine.Database.Routines);
        NewRoutine.Assign(Routine);
        NewRoutine.Source := Trim(ActiveSynMemo.Text);

        Result := Database.UpdateRoutine(Routine, NewRoutine);

        NewRoutine.Free();
      end;
    iiEvent:
      begin
        Event := TSEvent(FNavigator.Selected.Data);

        NewEvent := TSEvent.Create(Database.Events);
        NewEvent.Assign(Event);

        NewEvent.Stmt := Trim(ActiveSynMemo.Text);

        Result := Database.UpdateEvent(Event, NewEvent);

        NewEvent.Free();
      end;
    iiTrigger:
      begin
        Trigger := TSTrigger(FNavigator.Selected.Data);

        NewTrigger := TSTrigger.Create(Database.Triggers);
        NewTrigger.Assign(Trigger);

        NewTrigger.Stmt := Trim(ActiveSynMemo.Text);

        Result := Database.UpdateTrigger(Trigger, NewTrigger);

        NewTrigger.Free();
      end;
    else
      Result := False;
  end;
end;

procedure TFSession.PropertiesServerExecute(Sender: TObject);
begin
  Wanted.Clear();

  DServer.Session := Session;
  DServer.Tab := Self;
  DServer.Execute();
end;

procedure TFSession.PToolBarBlobResize(Sender: TObject);
begin
  if (Assigned(TBBlob.Images)) then
  begin
    TBBlob.ButtonHeight := Max(TBBlob.Images.Height + 2 * GetSystemMetrics(SM_CYFIXEDFRAME), ToolBar.Canvas.TextHeight('I') + 10);
    TBBlob.ButtonWidth := TBBlob.Images.Width + 2 * GetSystemMetrics(SM_CXFIXEDFRAME) + 1;
    TBBlob.Height := TBBlob.ButtonHeight;
    PToolBarBlob.ClientHeight := TBBlob.Height;
  end;
end;

function TFSession.RenameSItem(const SItem: TSItem; const NewName: string): Boolean;
var
  BaseTable: TSBaseTable;
  Event: TSEvent;
  NewBaseTable: TSBaseTable;
  NewEvent: TSEvent;
  NewTrigger: TSTrigger;
  NewUser: TSUser;
  Table: TSTable;
  Trigger: TSTrigger;
  User: TSUser;
begin
  if (SItem is TSTable) then
  begin
    Table := TSTable(SItem);

    Result := Table.Database.RenameTable(Table, NewName);
  end
  else if (SItem is TSTrigger) then
  begin
    Trigger := TSTrigger(SItem);

    NewTrigger := TSTrigger.Create(Trigger.Database.Triggers);
    NewTrigger.Assign(Trigger);
    NewTrigger.Name := NewName;
    Result := Trigger.Database.UpdateTrigger(Trigger, NewTrigger);
    NewTrigger.Free();
  end
  else if (SItem is TSEvent) then
  begin
    Event := TSEvent(SItem);

    NewEvent := TSEvent.Create(Event.Database.Events);
    NewEvent.Assign(Event);
    NewEvent.Name := NewName;
    Result := Event.Database.UpdateEvent(Event, NewEvent);
    NewEvent.Free();
  end
  else if (SItem is TSKey) then
  begin
    BaseTable := TSBaseTableField(SItem).Table;

    NewBaseTable := TSBaseTable.Create(BaseTable.Database.Tables);
    NewBaseTable.Assign(BaseTable);
    NewBaseTable.KeyByCaption(SItem.Caption).Name := NewName;
    Result := BaseTable.Database.UpdateBaseTable(BaseTable, NewBaseTable);
    NewBaseTable.Free();
  end
  else if (SItem is TSBaseTableField) then
  begin
    BaseTable := TSBaseTableField(SItem).Table;

    NewBaseTable := TSBaseTable.Create(BaseTable.Database.Tables);
    NewBaseTable.Assign(BaseTable);
    NewBaseTable.FieldByName(SItem.Name).Name := NewName;
    Result := BaseTable.Database.UpdateBaseTable(BaseTable, NewBaseTable);
    NewBaseTable.Free();
  end
  else if (SItem is TSForeignKey) then
  begin
    BaseTable := TSForeignKey(SItem).Table;

    NewBaseTable := TSBaseTable.Create(BaseTable.Database.Tables);
    NewBaseTable.Assign(BaseTable);
    NewBaseTable.ForeignKeyByName(SItem.Name).Name := NewName;
    Result := BaseTable.Database.UpdateBaseTable(BaseTable, NewBaseTable);
    NewBaseTable.Free();
  end
  else if (SItem is TSUser) then
  begin
    User := TSUser(SItem);

    NewUser := TSUser.Create(Session.Users);
    NewUser.Assign(User);
    if (NewName = '<' + Preferences.LoadStr(287) + '>') then
      NewUser.Name := ''
    else
      NewUser.Name := NewName;
    Result := Session.UpdateUser(User, NewUser);
    NewUser.Free();
  end
  else
    Result := False;

  if (Assigned(ActiveDBGrid) and Assigned(ActiveDBGrid.DataSource.DataSet) and Result) then
    ActiveDBGrid.DataSource.DataSet.Close();
end;

procedure TFSession.SaveDiagram(Sender: TObject);
begin
  SaveDialog.Title := Preferences.LoadStr(582);
  SaveDialog.InitialDir := Path;
  SaveDialog.Encodings.Text := '';
  SaveDialog.EncodingIndex := -1;
  if ((Sender = MainAction('aFSaveAs')) or (ActiveWorkbench.Filename = '')) then
    SaveDialog.FileName := SelectedDatabase + '.xml'
  else
    SaveDialog.FileName := ActiveWorkbench.Filename;
  SaveDialog.DefaultExt := 'xml';
  OpenDialog.Filter := FilterDescription('xml') + ' (*.xml)|*.xml|' + FilterDescription('*') + ' (*.*)|*.*';

  if ((Sender = MainAction('aFSave')) and (ActiveWorkbench.Filename <> '') or SaveDialog.Execute()) then
    ActiveWorkbench.SaveToFile(SaveDialog.FileName);
end;

procedure TFSession.SaveSQLFile(Sender: TObject);
var
  BytesWritten: DWord;
  FileBuffer: PAnsiChar;
  Handle: THandle;
  Len: Cardinal;
  Success: Boolean;
  Text: string;
  URI: TUURI;
begin
  SaveDialog.Title := Preferences.LoadStr(582);
  SaveDialog.InitialDir := Path;
  SaveDialog.Encodings.Text := EncodingCaptions();
  SaveDialog.EncodingIndex := SaveDialog.Encodings.IndexOf(CodePageToEncoding(Session.Connection.CodePage));
  if ((Sender = MainAction('aFSave')) or (Sender = MainAction('aFSaveAs'))) then
  begin
    if (SQLEditors[View].Filename = '') then
      SaveDialog.FileName := Preferences.LoadStr(6) + '.sql'
    else
    begin
      if (Sender = MainAction('aFSave')) then
        SaveDialog.FileName := SQLEditors[View].Filename
      else
        SaveDialog.FileName := ExtractFileName(SQLEditors[View].Filename);
      SaveDialog.EncodingIndex := SaveDialog.Encodings.IndexOf(CodePageToEncoding(SQLEditors[View].FileCodePage));
    end;
    Text := ActiveSynMemo.Text;
  end
  else if (Sender = MainAction('aECopyToFile')) then
  begin
    if (Window.ActiveControl = ActiveSynMemo) then
    begin
      SaveDialog.FileName := '';
      Text := ActiveSynMemo.SelText;
      if (Text = '') then Text := ActiveSynMemo.Text;
    end
    else if (Window.ActiveControl = FLog) then
    begin
      SaveDialog.FileName := Preferences.LoadStr(11) + '.sql';
      Text := FLog.SelText;
      if (Text = '') then Text := FLog.Text;
    end;
  end
  else if (Sender = miHSaveAs) then
  begin
    SaveDialog.FileName := '';
    Text := XMLNode(IXMLNode(FSQLHistoryMenuNode.Data), 'sql').Text;
  end
  else
    Exit;
  SaveDialog.DefaultExt := 'sql';
  SaveDialog.Filter := FilterDescription('sql') + ' (*.sql)|*.sql' + '|' + FilterDescription('*') + ' (*.*)|*.*';
  if (((Sender = MainAction('aFSave')) and (SQLEditors[View].Filename <> '')) or (Text <> '') and SaveDialog.Execute()) then
  begin
    Path := ExtractFilePath(SaveDialog.FileName);

    Handle := CreateFile(PChar(SaveDialog.FileName),
                         GENERIC_WRITE,
                         FILE_SHARE_READ,
                         nil,
                         CREATE_ALWAYS, 0, 0);
    if (Handle = INVALID_HANDLE_VALUE) then
      MsgBox(SysErrorMessage(GetLastError()), Preferences.LoadStr(45), MB_OK + MB_ICONERROR)
    else
    begin
      SQLEditors[View].Filename := SaveDialog.FileName;
      SQLEditors[View].FileCodePage := EncodingToCodePage(SaveDialog.Encodings[SaveDialog.EncodingIndex]);

      case (SQLEditors[View].FileCodePage) of
        CP_UNICODE: Success := WriteFile(Handle, BOM_UNICODE_LE^, Length(BOM_UNICODE_LE), BytesWritten, nil);
        CP_UTF8: Success := WriteFile(Handle, BOM_UTF8^, Length(BOM_UTF8), BytesWritten, nil);
        else Success := True;
      end;

      if (Success) then
        case (SQLEditors[View].FileCodePage) of
          CP_UNICODE: Success := WriteFile(Handle, Text[1], Length(Text), BytesWritten, nil);
          else
            begin
              Len := WideCharToAnsiChar(SQLEditors[View].FileCodePage, PChar(Text), Length(Text), nil, 0);
              if (Len > 0) then
              begin
                GetMem(FileBuffer, Len);
                WideCharToAnsiChar(SQLEditors[View].FileCodePage, PChar(Text), Length(Text), FileBuffer, Len);
                Success := WriteFile(Handle, FileBuffer^, Len, BytesWritten, nil);
                FreeMem(FileBuffer);
              end;
            end;
        end;

      if (not Success) then
        MsgBox(SysErrorMessage(GetLastError()), Preferences.LoadStr(45), MB_OK + MB_ICONERROR)
      else
      begin
        if ((Sender = MainAction('aFSave')) or (Sender = MainAction('aFSaveAs'))) then
          ActiveSynMemo.Modified := False;
        URI := TUURI.Create(Address);
        URI.Param['file'] := EscapeURL(SQLEditors[View].Filename);
        if (SQLEditors[View].FileCodePage = 0) then
          URI.Param['cp'] := Null
        else
          URI.Param['cp'] := IntToStr(SQLEditors[View].FileCodePage);
        FAddress := URI.Address;
        AddressChanged(nil);
        URI.Free();
        Session.Account.Desktop.Files.Add(SQLEditors[View].Filename, SQLEditors[View].FileCodePage);
        Window.Perform(UM_UPDATETOOLBAR, 0, LPARAM(Self));
      end;

      CloseHandle(Handle);
    end;
  end;
end;

procedure TFSession.SearchNotFound(Sender: TObject; FindText: string);
begin
  MsgBox(Preferences.LoadStr(533, FindText), Preferences.LoadStr(43), MB_OK + MB_ICONINFORMATION);
end;

procedure TFSession.SendQuery(Sender: TObject; const SQL: string);
begin
  case (View) of
    vIDE:
      case (FNavigator.Selected.ImageIndex) of
        iiProcedure,
        iiFunction:
          begin
            Desktop(TSRoutine(FNavigator.Selected.Data)).CloseIDEResult();
            PContentChange(Sender);
            Session.Connection.SendSQL(SQL, Desktop(TSRoutine(FNavigator.Selected.Data)).IDEResultEvent);
          end;
        iiEvent:
          Session.Connection.SendSQL(SQL);
      end;
    vBuilder:
      case (FNavigator.Selected.ImageIndex) of
        iiDatabase,
        iiSystemDatabase:
          begin
            Desktop(TSDatabase(FNavigator.Selected.Data)).CloseBuilderResult();
            PContentChange(Sender);
            Session.Connection.SendSQL(SQL, Desktop(TSDatabase(FNavigator.Selected.Data)).BuilderResultEvent);
          end;
      end;
    vEditor,
    vEditor2,
    vEditor3:
      begin
        SQLEditors[View].CloseResult();
        PContentChange(Sender);
        Session.Connection.SendSQL(SQL, SQLEditors[View].ResultEvent);
      end;
  end;
end;

procedure TFSession.SessionUpdate(const Event: TSSession.TEvent);
var
  Control: TWinControl;
  I: Integer;
  S1: string;
  S2: string;
  Table: TSTable;
  TempActiveControl: TWinControl;
begin
  LeftMousePressed := False;

  TempActiveControl := Window.ActiveControl;

  if (Assigned(Event)) then
    if (Event.Sender is TSObjectSearch) then
      ListViewUpdate(Event, ObjectSearchListView)
    else
    begin
      if (Event.EventType in [etItemsValid, etItemValid, etItemCreated, etItemAltered, etItemDropped]) then
        FNavigatorUpdate(Event);

      if (Event.EventType in [etItemsValid, etItemValid, etItemCreated, etItemAltered, etItemDropped]) then
      begin
        if (Event.Items is TSDatabases) then
        begin
          ListViewUpdate(Event, FServerListView);
          if (Event.Sender is TSDatabase) then
            ListViewUpdate(Event, Desktop(TSDatabase(Event.Sender)).ListView);
        end
        else if (Event.Items is TSProcesses) then
        begin
          ListViewUpdate(Event, FServerListView);
          ListViewUpdate(Event, ProcessesListView);
        end
        else if (Event.Items is TSUsers) then
        begin
          ListViewUpdate(Event, FServerListView);
          ListViewUpdate(Event, UsersListView);
        end
        else if (Event.Items is TSVariables) then
        begin
          ListViewUpdate(Event, FServerListView);
          ListViewUpdate(Event, VariablesListView);
        end
        else if ((Event.Sender is TSDatabase) and not (Event.Items is TSTriggers)) then
        begin
          ListViewUpdate(Event, FServerListView);
          if (not (Event.Items is TSTriggers)) then
            ListViewUpdate(Event, Desktop(TSDatabase(Event.Sender)).ListView)
          else if (Event.EventType = etItemDropped) then
            ListViewUpdate(Event, Desktop(TSTrigger(Event.Item).Table).ListView)
          else
            for I := 0 to TSTriggers(Event.Items).Count - 1 do
              if (Assigned(TSTriggers(Event.Items)[I].Table)) then
                ListViewUpdate(Event, Desktop(TSTriggers(Event.Items)[I].Table).ListView);
        end
        else if ((Event.Sender is TSTable) or (Event.Item is TSTrigger) and Assigned(TSTrigger(Event.Item).Table)) then
        begin
          if (Event.Item is TSTrigger) then
            Table := TSTrigger(Event.Item).Table
          else
            Table := TSTable(Event.Sender);
          ListViewUpdate(Event, Desktop(Table.Database).ListView);
          ListViewUpdate(Event, Desktop(Table).ListView);
        end;
      end;

      if ((Event.EventType = etItemValid)) then
        if ((Event.Item is TSView) and Assigned(Desktop(TSView(Event.Item)).SynMemo)) then
          Desktop(TSView(Event.Item)).SynMemo.Text := TSView(Event.Item).Stmt + #13#10
        else if ((Event.Item is TSRoutine) and Assigned(Desktop(TSRoutine(Event.Item)).CreateSynMemo())) then
        begin
          Desktop(TSRoutine(Event.Item)).SynMemo.Text := TSRoutine(Event.Item).Source;
          PContentChange(nil);
        end
        else if ((Event.Item is TSTrigger) and Assigned(Desktop(TSTrigger(Event.Item)).CreateSynMemo())) then
        begin
          Desktop(TSTrigger(Event.Item)).SynMemo.Text := TSTrigger(Event.Item).Stmt;
          PContentChange(nil);
        end
        else if ((Event.Item is TSEvent) and Assigned(Desktop(TSEvent(Event.Item)).CreateSynMemo())) then
        begin
          Desktop(TSEvent(Event.Item)).SynMemo.Text := TSEvent(Event.Item).Stmt;
          Desktop(TSEvent(Event.Item)).SynMemo.Modified := False;
          PContentChange(nil);
        end;

      if (Event.EventType = etItemValid) then
      begin
        if ((View = vBrowser)
          and Assigned(FNavigator.Selected) and (Event.Item = FNavigator.Selected.Data)) then
          Wanted.Update := UpdateAfterAddressChanged;
        if ((View = vIDE) and ((Event.Item is TSView) or (Event.Item is TSFunction))) then
          PContentChange(nil);
      end;
    end;

  if (PContent.Visible and Assigned(TempActiveControl) and TempActiveControl.Visible) then
  begin
    Control := TempActiveControl;
    while (Control.Visible and Control.Enabled and Assigned(Control.Parent)) do Control := Control.Parent;
    if (Control.Visible and Control.Enabled) then
      Window.ActiveControl := TempActiveControl;
  end;

  // StatusBar should be refreshed, after all events applied.
  PostMessage(Handle, UM_STATUS_BAR_REFRESH, 0, 0);

  if (Assigned(Event)
    and ((Event.EventType in [etItemCreated, etItemAltered])
      or (Event.EventType in [etItemValid]) and (Event.Item is TSObject) and not TSObject(Event.Item).Valid)
    and (Screen.ActiveForm = Window)
    and Wanted.Nothing) then
    Wanted.Update := Session.Update;
end;

procedure TFSession.SetView(const AView: TView);
var
  ScrollPos: record
    Horz: Integer;
    Vert: Integer;
  end;
  URI: TUURI;
begin
  URI := TUURI.Create(Address);

  if ((URI.Param['view'] = 'browser') and (URI.Table = '')) then
    raise ERangeError.Create('View: ' + IntToStr(Ord(AView)) + #13#10
      + 'LastSelectedTable: ' + LastSelectedTable + #13#10
      + 'Address: ' + Address + #13#10
      + 'ImageIndex: ' + IntToStr(FNavigator.Selected.ImageIndex) + #13#10
      + 'Text: ' + FNavigator.Selected.Text + #13#10
      + 'URI.Address: ' + URI.Address);

  case (AView) of
    vObjects: URI.Param['view'] := Null;
    vBrowser: URI.Param['view'] := 'browser';
    vIDE: URI.Param['view'] := 'ide';
    vBuilder: URI.Param['view'] := 'builder';
    vDiagram: URI.Param['view'] := 'diagram';
    vEditor,
    vEditor2,
    vEditor3:
      begin
        URI.Param['view'] := ViewToParam(AView);
        if (Assigned(SQLEditors[AView])) then
          if (SQLEditors[AView].Filename = '') then
          begin
            URI.Param['file'] := Null;
            URI.Param['cp'] := Null;
          end
          else
          begin
            URI.Param['file'] := EscapeURL(SQLEditors[AView].Filename);
            if (SQLEditors[AView].FileCodePage = 0) then
              URI.Param['cp'] := Null
            else
              URI.Param['cp'] := IntToStr(SQLEditors[AView].FileCodePage);
          end;
      end;
    vObjectSearch: URI.Param['view'] := 'objectsearch';
  end;


  if ((URI.Param['view'] = 'objects') and (SelectedImageIndex in [iiProcedure, iiFunction, iiTrigger, iiEvent])) then
  begin
    URI.Param['objecttype'] := Null;
    URI.Param['object'] := Null;
  end
  else if ((URI.Param['view'] = 'browser') and not (SelectedImageIndex in [iiBaseTable, iiView, iiSystemView])) then
  begin
    if (SelectedImageIndex = iiTrigger) then
      URI.Table := TSTrigger(FNavigator.Selected.Data).TableName
    else if (LastSelectedTable = '') then
      // Debug 2016-12-07
      raise ERangeError.Create(SRangeError)
    else
    begin
      URI.Database := LastSelectedDatabase;
      URI.Table := LastSelectedTable;
    end;

    // Debug 2017-01-05
    if (URI.Table = '') then
      raise ERangeError.Create(SRangeError);
  end
  else if ((URI.Param['view'] = 'ide') and not (SelectedImageIndex in [iiView, iiProcedure, iiFunction, iiEvent, iiTrigger])) then
    URI.Address := LastObjectIDEAddress
  else if ((URI.Param['view'] = 'builder') and not (SelectedImageIndex in [iiDatabase, iiSystemDatabase])
    or (ParamToView(URI.Param['view']) in [vEditor, vEditor2, vEditor3]) and not (SelectedImageIndex in [iiServer, iiDatabase, iiSystemDatabase])) then
  begin
    if (URI.Database = '') then
      URI.Database := LastSelectedDatabase;
    URI.Table := '';
    URI.Param['objecttype'] := Null;
    URI.Param['object'] := Null;
    URI.Param['system'] := Null;
    URI.Param['filter'] := Null;
    URI.Param['search'] := Null;
    URI.Param['offset'] := Null;
    URI.Param['file'] := Null;
    URI.Param['cp'] := Null;
  end
  else if ((ParamToView(URI.Param['view']) in [vEditor, vEditor2, vEditor3]) and (Session.Connection.DatabaseName <> '') and not (SelectedImageIndex in [iiDatabase, iiSystemDatabase])) then
    URI.Database := Session.Connection.DatabaseName
  else if ((URI.Param['view'] = 'diagram') and not (SelectedImageIndex in [iiDatabase, iiSystemDatabase])) then
  begin
    if (URI.Database = '') then
      URI.Database := LastSelectedDatabase;
    URI.Table := '';
    URI.Param['system'] := Null;
    URI.Param['filter'] := Null;
    URI.Param['search'] := Null;
    URI.Param['offset'] := Null;
    URI.Param['file'] := Null;
    URI.Param['cp'] := Null;
  end
  else if (URI.Param['view'] = 'objectsearch') then
  begin
    URI.Param['objecttype'] := Null;
    URI.Param['object'] := Null;
    URI.Param['system'] := Null;
    URI.Param['filter'] := Null;
    URI.Param['search'] := Null;
    URI.Param['offset'] := Null;
    URI.Param['file'] := Null;
    URI.Param['cp'] := Null;
  end;

  if ((URI.Param['view'] = 'browser') and (URI.Table = '')) then
    raise ERangeError.Create('View: ' + IntToStr(Ord(AView)) + #13#10
      + 'LastSelectedTable: ' + LastSelectedTable + #13#10
      + 'Address: ' + Address + #13#10
      + 'ImageIndex: ' + IntToStr(FNavigator.Selected.ImageIndex) + #13#10
      + 'Text: ' + FNavigator.Selected.Text + #13#10
      + 'URI.Address: ' + URI.Address);

  LockWindowUpdate(FNavigator.Handle);
  ScrollPos.Horz := GetScrollPos(FNavigator.Handle, SB_HORZ);
  ScrollPos.Vert := GetScrollPos(FNavigator.Handle, SB_VERT);
  Address := URI.Address;
  SetScrollPos(FNavigator.Handle, SB_HORZ, ScrollPos.Horz, TRUE);
  SetScrollPos(FNavigator.Handle, SB_VERT, ScrollPos.Vert, TRUE);
  LockWindowUpdate(0);

  URI.Free();
end;

procedure TFSession.SetAddress(const AAddress: string);
var
  AllowChange: Boolean;
  ChangeEvent: TTVChangedEvent;
  ChangingEvent: TTVChangingEvent;
  CodePage: Integer;
  FileName: string;
  NewView: TView;
  NewAddress: string;
  Node: TTreeNode;
  Position: Integer;
  Table: TSTable;
  URI: TUURI;
begin
  AllowChange := True; Node := nil;
  NewAddress := AAddress; // We need this, since in AddressChanging maybe Wanted.Address will be changed, but AAddress is Wanted.Address
  AddressChanging(nil, NewAddress, AllowChange);
  if (AllowChange) then
  begin
    Node := FNavigatorNodeByAddress(NewAddress);
    AllowChange := Assigned(Node);
    if (not AllowChange) then
      Wanted.Address := Session.Account.ExpandAddress('/');
  end;
  if (AllowChange) then
  begin
    FAddress := NewAddress;

    ChangingEvent := FNavigator.OnChanging; FNavigator.OnChanging := nil;
    ChangeEvent := FNavigator.OnChange; FNavigator.OnChange := nil;
    FNavigator.Selected := Node;
    FNavigator.OnChanging := ChangingEvent;
    FNavigator.OnChange := ChangeEvent;

    URI := TUURI.Create(Address);

    // Debug 2016-12-12
    if ((URI.Param['view'] = 'browser') and (URI.Table = '')) then
      raise ERangeError.Create('NewAddress: ' + NewAddress + #13#10
        + 'Address: ' + Address);

    if ((URI.Param['view'] = 'browser') and (Node.ImageIndex in [iiBaseTable, iiView, iiSystemView])) then
      NewView := vBrowser
    else if ((URI.Param['view'] = 'ide') and (Node.ImageIndex in [iiView, iiProcedure, iiFunction, iiTrigger, iiEvent])) then
      NewView := vIDE
    else if ((URI.Param['view'] = 'builder') and (Node.ImageIndex in [iiDatabase, iiSystemDatabase])) then
      NewView := vBuilder
    else if ((URI.Param['view'] = 'diagram') and (Node.ImageIndex in [iiDatabase, iiSystemDatabase])) then
      NewView := vDiagram
    else if (URI.Param['view'] = 'editor') then
      NewView := vEditor
    else if (URI.Param['view'] = 'editor2') then
      NewView := vEditor2
    else if (URI.Param['view'] = 'editor3') then
      NewView := vEditor3
    else if ((URI.Param['view'] = 'objectsearch') and Assigned(ObjectSearch)) then
      NewView := vObjectSearch
    else
      NewView := vObjects;

    MainAction('aVObjects').Checked := NewView = vObjects;
    MainAction('aVBrowser').Checked := NewView = vBrowser;
    MainAction('aVIDE').Checked := NewView = vIDE;
    MainAction('aVBuilder').Checked := NewView = vBuilder;
    MainAction('aVDiagram').Checked := NewView = vDiagram;
    MainAction('aVSQLEditor').Checked := NewView = vEditor;
    MainAction('aVSQLEditor2').Checked := NewView = vEditor2;
    MainAction('aVSQLEditor3').Checked := NewView = vEditor3;

    case (NewView) of
      vBrowser:
        begin
          Table := Session.DatabaseByName(URI.Database).TableByName(URI.Table);

          FUDOffset.Position := 0;
          FUDLimit.Position := Desktop(Table).Limit;
          FLimitEnabled.Down := Desktop(Table).Limited;

          if (URI.Param['offset'] <> Null) then
          begin
            if (TryStrToInt(URI.Param['offset'], Position)) then FUDOffset.Position := Position else FUDOffset.Position := 0;
            FLimitEnabled.Down := URI.Param['offset'] <> '';
          end;
          if (URI.Param['filter'] = Null) then
          begin
            FFilter.Text := '';
            FFilterEnabled.Down := False;
            FFilterEnabled.Enabled := FFilterEnabled.Down;
          end
          else
          begin
            FFilter.Text := URI.Param['filter'];
            FFilterEnabled.Down := URI.Param['filter'] <> '';
            FFilterEnabled.Enabled := FFilterEnabled.Down;
          end;
          if (URI.Param['search'] = Null) then
          begin
            FQuickSearch.Text := '';
            FQuickSearchEnabled.Down := False;
            FQuickSearchEnabled.Enabled := FQuickSearchEnabled.Down;
          end
          else
          begin
            FQuickSearch.Text := URI.Param['search'];
            FQuickSearchEnabled.Down := URI.Param['search'] <> '';
            FQuickSearchEnabled.Enabled := FQuickSearchEnabled.Down;
          end;
        end;
      vEditor,
      vEditor2,
      vEditor3:
        if (URI.Param['file'] <> Null) then
        begin
          FileName := UnescapeURL(URI.Param['file']);
          if (ExtractFilePath(FileName) = '') then
            FileName := ExpandFilename(FileName);
          if (Assigned(SQLEditors[NewView]) and (FileName <> SQLEditors[NewView].Filename) and FileExists(FileName)) then
            if ((URI.Param['cp'] = Null) or not TryStrToInt(URI.Param['cp'], CodePage)) then
              OpenSQLFile(FileName)
            else
              OpenSQLFile(FileName, CodePage);
        end;
    end;

    // Debug 2016-12-27
    if ((URI.Param['view'] = 'browser') and not (FNavigator.Selected.ImageIndex in [iiBaseTable, iiView, iiSystemView])) then
      raise ERangeError.Create('Address: ' + URI.Address + #13#10
        + 'ImageIndex: ' + IntToStr(FNavigator.Selected.ImageIndex) + #13#10
        + 'Text: ' + FNavigator.Selected.Text);

    URI.Free();

    if (AllowChange) then
      AddressChanged(nil);
  end;
end;

procedure TFSession.SetListViewGroupHeader(const ListView: TListView; const GroupID: Integer; const NewHeader: string);
  // In Delphi XE2 there is a flickering in the VScrollBar, while changing
  // TListGroup.Header - also if Groups.BeginUpdate is used
var
  LVGroup: TLVGroup;
begin
  ZeroMemory(@LVGroup, SizeOf(LVGroup));
  LVGroup.cbSize := SizeOf(LVGroup);
  LVGroup.mask := LVGF_HEADER;
  LVGroup.pszHeader := PChar(NewHeader);
  LVGroup.cchHeader := Length(NewHeader);
  ListView_SetGroupInfo(ListView.Handle, GroupID, LVGroup);
end;

procedure TFSession.SetPath(const APath: TFileName);
begin
  if (APath <> Preferences.Path) then
  begin
    Preferences.Path := APath;
    if (Assigned(FFolders) and (APath <> FFolders.SelectedFolder)) then
      FFolders.SelectedFolder := APath;
  end;
end;

procedure TFSession.SLogCanResize(Sender: TObject; var NewSize: Integer;
  var Accept: Boolean);
var
  MaxHeight: Integer;
begin
  MaxHeight := ClientHeight - PHeader.Height - SLog.Height - PContent.Constraints.MinHeight;

  if (NewSize > MaxHeight) then
    NewSize := MaxHeight;

  Accept := (PLog.Constraints.MinHeight <= NewSize) and (NewSize <= MaxHeight);
end;

procedure TFSession.SLogMoved(Sender: TObject);
begin
  FormResize(Sender);
end;

procedure TFSession.smEEmptyClick(Sender: TObject);
begin
  Wanted.Clear();

  Session.SQLMonitor.Clear();
  FLog.Lines.Clear();
  PLogResize(nil);
end;

procedure TFSession.SplitterCanResize(Sender: TObject; var NewSize: Integer;
  var Accept: Boolean);
var
  I: Integer;
  MaxHeight: Integer;
  MinHeight: Integer;
  Splitter: TSplitter;
begin
  if (Sender is TSplitter) then
  begin
    Splitter := TSplitter(Sender);

    MaxHeight := Splitter.Parent.ClientHeight;
    MinHeight := 0;
    for I := 0 to Splitter.Parent.ControlCount - 1 do
      if (Splitter.Parent.Controls[I].Visible) then
        if (Splitter.Parent.Controls[I].Top < Splitter.Top) then
          Dec(MaxHeight, Splitter.Parent.Controls[I].Constraints.MinHeight)
        else if (Splitter.Parent.Controls[I] = Splitter) then
          Dec(MaxHeight, Splitter.Height)
        else if (Splitter.Parent.Controls[I].Top > Splitter.Top) then
          Inc(MinHeight, Splitter.Parent.Controls[I].Constraints.MinHeight);
    for I := 0 to Splitter.Parent.ControlCount - 1 do
      if (Splitter.Parent.Controls[I].Visible) then
        if ((Splitter.Parent.Controls[I].Top > Splitter.Top)
          and (Splitter.Parent.Controls[I].Constraints.MaxHeight < MaxHeight)) then
          MaxHeight := Splitter.Parent.Controls[I].Constraints.MaxHeight;

    if (NewSize < MinHeight) then
      NewSize := MinHeight
    else if ((NewSize > MaxHeight) and (MaxHeight > 0)) then
      NewSize := MaxHeight;
  end;

  if (Sender = SQueryBuilderSynMemo) then
    SQueryBuilderSynMemoMoved := True;
end;

procedure TFSession.SQLError(DataSet: TDataSet; E: EDatabaseError; var Action: TDataAction);
var
  Flags: Integer;
  Msg: string;
begin
  if (E is EDatabasePostError) then
    Msg := Preferences.LoadStr(675)
  else if (E is EMySQLError) then
    case (EMySQLError(E).ErrorCode) of
      CR_CONN_HOST_ERROR: if (EMySQLError(E).Connection.Host <> '') then Msg := Preferences.LoadStr(495, EMySQLError(E).Connection.Host) else Msg := Preferences.LoadStr(495);
      CR_UNKNOWN_HOST: if (EMySQLError(E).Connection.Host <> '') then Msg := Preferences.LoadStr(706, EMySQLError(E).Connection.Host) else Msg := Preferences.LoadStr(706);
      else Msg := Preferences.LoadStr(165, IntToStr(EMySQLError(E).ErrorCode), E.Message);
    end
  else
    Msg := E.Message;

  Flags := MB_CANCELTRYCONTINUE + MB_ICONERROR;
  case (MsgBox(Msg, Preferences.LoadStr(45), Flags)) of
    IDCANCEL,
    IDABORT: begin Action := daAbort; PostMessage(Handle, UM_ACTIVATE_DBGRID, 0, LPARAM(ActiveDBGrid)); end;
    IDRETRY,
    IDTRYAGAIN: Action := daRetry;
    IDCONTINUE,
    IDIGNORE: begin Action := daAbort; DataSet.Cancel(); end;
  end;
end;

procedure TFSession.SSideBarCanResize(Sender: TObject; var NewSize: Integer;
  var Accept: Boolean);
begin
  Accept := NewSize <= ClientWidth - SSideBar.Width - PContent.Constraints.MinWidth;
end;

procedure TFSession.SSideBarMoved(Sender: TObject);
begin
  FormResize(Sender);
end;

procedure TFSession.StatusBarRefresh(const Immediately: Boolean = False);
var
  Count: Integer; // Debug 2016-12-21
  QueryBuilderWorkArea: TacQueryBuilderWorkArea;
  SelCount: Integer;
begin
  if (Assigned(StatusBar) and (Immediately or (tsActive in FrameState)) and not (csDestroying in ComponentState) and Assigned(Window)) then
  begin
    if (not Assigned(Window.ActiveControl)) then
      StatusBar.Panels[sbNavigation].Text := ''
    else if (Window.ActiveControl = ActiveSynMemo) then
      StatusBar.Panels[sbNavigation].Text := IntToStr(ActiveSynMemo.CaretXY.Line) + ':' + IntToStr(ActiveSynMemo.CaretXY.Char)
    else if (Window.ActiveControl = ActiveListView) then
    begin
      if (Assigned(ActiveListView.Selected) and (TObject(ActiveListView.Selected.Data) is TSKey)) then
        StatusBar.Panels[sbNavigation].Text := Preferences.LoadStr(377) + ': ' + IntToStr(TSKey(ActiveListView.Selected.Data).Index + 1)
      else if (Assigned(ActiveListView.Selected) and (TObject(ActiveListView.Selected.Data) is TSTableField)) then
        StatusBar.Panels[sbNavigation].Text := Preferences.LoadStr(164) + ': ' + IntToStr(TSTableField(ActiveListView.Selected.Data).Index)
      else
        StatusBar.Panels[sbNavigation].Text := '';
    end
    else if ((Window.ActiveControl = ActiveDBGrid) and Assigned(ActiveDBGrid.SelectedField) and (ActiveDBGrid.DataSource.DataSet.RecNo >= 0)) then
      StatusBar.Panels[sbNavigation].Text := IntToStr(ActiveDBGrid.DataSource.DataSet.RecNo + 1) + ':' + IntToStr(ActiveDBGrid.SelectedField.FieldNo)
    else if (Window.ActiveControl = FText) then
      StatusBar.Panels[sbNavigation].Text := IntToStr(FText.CaretPos.Y + 1) + ':' + IntToStr(FText.CaretPos.X + 1)
    else
      StatusBar.Panels[sbNavigation].Text := '';


    Count := -1;
    case (View) of
      vObjects,
      vObjectSearch:
        if (Assigned(ActiveListView)) then Count := ActiveListView.Items.Count;
      vBrowser:
        if (Assigned(ActiveDBGrid) and Assigned(ActiveDBGrid.DataSource.DataSet) and ActiveDBGrid.DataSource.DataSet.Active) then Count := ActiveDBGrid.DataSource.DataSet.RecordCount;
      vIDE,
      vBuilder:
        if (Assigned(FQueryBuilderEditorPageControl())) then
        begin
          QueryBuilderWorkArea := FQueryBuilderActiveWorkArea();
          if (Assigned(Window.ActiveControl) and Assigned(QueryBuilderWorkArea) and IsChild(QueryBuilderWorkArea.Handle, Window.ActiveControl.Handle)) then
            Count := FQueryBuilderActiveWorkArea().ControlCount
          else if (Window.ActiveControl = FQueryBuilderSynMemo) then
            Count := FQueryBuilderSynMemo.Lines.Count;
        end;
      vEditor,
      vEditor2,
      vEditor3:
        if (Assigned(Window.ActiveControl) and (Window.ActiveControl = ActiveSynMemo)) then
          Count := ActiveSynMemo.Lines.Count
        else if ((Window.ActiveControl = ActiveDBGrid) and Assigned(ActiveDBGrid) and Assigned(ActiveDBGrid.DataSource.DataSet)) then
          Count := ActiveDBGrid.DataSource.DataSet.RecordCount;
    end;

    SelCount := -1;
    if ((Window.ActiveControl = ActiveListView) and Assigned(ActiveListView) and (ActiveListView.SelCount > 1)) then
      SelCount := ActiveListView.SelCount
    else if (Assigned(ActiveSynMemo) and (Window.ActiveControl = ActiveSynMemo)) then
      SelCount := 0
    else if ((Window.ActiveControl = ActiveDBGrid) and Assigned(ActiveDBGrid) and (ActiveDBGrid.SelectedRows.Count > 0)) then
      SelCount := ActiveDBGrid.SelectedRows.Count;

    if (((View = vBrowser) or (Window.ActiveControl = ActiveDBGrid)) and Assigned(ActiveDBGrid)) then
    begin
      if (SelCount > 0) then
        StatusBar.Panels[sbSummarize].Text := Preferences.LoadStr(888, IntToStr(SelCount))
      else if ((View = vBrowser) and (SelectedImageIndex in [iiBaseTable]) and not Session.Connection.InUse() and TSBaseTable(FNavigator.Selected.Data).ValidData and TSBaseTable(FNavigator.Selected.Data).DataSet.LimitedDataReceived and (TSBaseTable(FNavigator.Selected.Data).RecordCount >= 0)) then
        StatusBar.Panels[sbSummarize].Text := Preferences.LoadStr(889, FormatFloat('#,##0', Count, LocaleFormatSettings), FormatFloat('#,##0', TSBaseTable(FNavigator.Selected.Data).RecordCount, LocaleFormatSettings))
      else if (Assigned(ActiveDBGrid) and Assigned(ActiveDBGrid.DataSource.DataSet)) then
        StatusBar.Panels[sbSummarize].Text := Preferences.LoadStr(887, FormatFloat('#,##0', ActiveDBGrid.DataSource.DataSet.RecordCount, LocaleFormatSettings));
    end
    else if (SelCount > 0) then
      StatusBar.Panels[sbSummarize].Text := Preferences.LoadStr(688, FormatFloat('#,##0', SelCount, LocaleFormatSettings))
    else if (Assigned(ActiveSynMemo) and (Window.ActiveControl = ActiveSynMemo) and (Count >= 0)) then
      StatusBar.Panels[sbSummarize].Text := FormatFloat('#,##0', Count, LocaleFormatSettings) + ' ' + Preferences.LoadStr(600)
    else if ((View = vBuilder) and (Count >= 0)) then
      if (Window.ActiveControl = FQueryBuilderSynMemo) then
        StatusBar.Panels[sbSummarize].Text := FormatFloat('#,##0', Count, LocaleFormatSettings) + ' ' + Preferences.LoadStr(600)
      else
        StatusBar.Panels[sbSummarize].Text := Preferences.LoadStr(687, FormatFloat('#,##0', Count, LocaleFormatSettings))
    else if (Count >= 0) then
      StatusBar.Panels[sbSummarize].Text := Preferences.LoadStr(687, FormatFloat('#,##0', Count, LocaleFormatSettings))
    else
      StatusBar.Panels[sbSummarize].Text := '';
  end;
end;

procedure TFSession.SynCompletionAfterCodeCompletion(Sender: TObject;
  const Value: string; Shift: TShiftState; Index: Integer; EndToken: Char);
begin
  SynCompletion.ItemList.Clear();
  SynCompletion.InsertList.Clear();
end;

procedure TFSession.SynCompletionCancelled(Sender: TObject);
begin
  SynCompletion.ItemList.Clear();
  SynCompletion.InsertList.Clear();
end;

procedure TFSession.SynCompletionChange(Sender: TObject; AIndex: Integer);
begin
  KillTimer(Handle, tiHideSynCompletion);
end;

procedure TFSession.SynCompletionClose(Sender: TObject);
begin
  KillTimer(Handle, tiHideSynCompletion);
end;

procedure TFSession.SynCompletionExecute(Kind: SynCompletionType;
  Sender: TObject; var CurrentInput: string; var x, y: Integer;
  var CanExecute: Boolean);

  procedure SynCompletionListAdd(const Item: string; const Insert: string);
  type
    Tstrcmp = function (lpString1, lpString2: PWideChar): Integer; stdcall;
  var
    Index: Integer;
    Left: Integer;
    Mid: Integer;
    Right: Integer;
    strcmp: Tstrcmp;
  begin
    if (Session.LowerCaseTableNames = 0) then
      strcmp := lstrcmp
    else
      strcmp := lstrcmpi;

    if ((SynCompletion.ItemList.Count = 0)
      or (strcmp(PChar(SynCompletion.ItemList[SynCompletion.ItemList.Count - 1]), PChar(Item)) < 0)) then
      Index := SynCompletion.ItemList.Count
    else
    begin
      Index := -1;
      Left := 0;
      Right := SynCompletion.ItemList.Count - 1;
      while (Left <= Right) do
      begin
        Mid := (Right - Left) div 2 + Left;
        case (strcmp(PChar(SynCompletion.ItemList[Mid]), PChar(Item))) of
          -1: begin Left := Mid + 1;  Index := Mid + 1; end;
          0: begin Index := -1; break; end;
          1: begin Right := Mid - 1; Index := Mid; end;
        end;
      end;
    end;

    if (Index < 0) then
      // Skip, since it's already in list
    else if (Index < SynCompletion.ItemList.Count) then
    begin
      SynCompletion.ItemList.Insert(Index, Item);
      SynCompletion.InsertList.Insert(Index, Insert);
    end
    else
    begin
      SynCompletion.ItemList.Add(Item);
      SynCompletion.InsertList.Add(Insert);
    end;
  end;

var
  ColumnName: string;
  Database: TSDatabase;
  DataSet: TMySQLQuery;
  I: Integer;
  Index: Integer;
  Item: TSQLParser.TCompletionList.PItem;
  J: Integer;
  Len: Integer;
  List: TList;
  SQL: string;
  Table: TSTable;
begin
  // Debug 2016-11-14
  if (not Assigned(ActiveSynMemo)) then
    raise ERangeError.Create(SRangeError);

  CanExecute := ActiveSynMemo.SelText = '';

  if (CanExecute) then
  begin
    SQL := ActiveSynMemo.Text;

    Index := 1;
    while (Index < ActiveSynMemo.SelStart + 1) do
    begin
      Len := SQLStmtLength(PChar(@SQL[Index]), Length(SQL) - (Index - 1));
      if ((Len = 0) or (Index + Len >= ActiveSynMemo.SelStart + 1)) then
        break
      else
        Inc(Index, Len);
    end;
    SQL := Copy(SQL, Index, ActiveSynMemo.SelStart + 1 - Index - Length(CurrentInput));

    Session.SQLParser.ParseSQL(SQL, True);

    CanExecute := Session.SQLParser.ErrorCode in [PE_Success, PE_IncompleteStmt];

    if (CanExecute) then
    begin
      List := TList.Create();

      SynCompletion.ItemList.Clear();
      SynCompletion.InsertList.Clear();

      for I := 0 to Session.SQLParser.CompletionList.Count - 1 do
      begin
        Item := Session.SQLParser.CompletionList[I];
        case (Item^.ItemType) of
          itList:
            begin
              if (PChar(@Item^.DatabaseName) = '') then
                Database := Session.DatabaseByName(SelectedDatabase)
              else
                Database := Session.DatabaseByName(StrPas(PChar(@Item^.DatabaseName)));
              if (not Assigned(Database) or (PChar(@Item^.TableName) = '')) then
                Table := nil
              else
                Table := Database.TableByName(StrPas(PChar(@Item^.TableName)));
              case (Item^.DbIdentType) of
                ditDatabase:
                  List.Add(Session.Databases);
                ditTable:
                  if (Assigned(Database)) then
                    List.Add(Database.Tables);
                ditProcedure,
                ditFunction:
                  if (Assigned(Database) and Assigned(Database.Routines)) then
                    List.Add(Database.Routines);
                ditTrigger:
                  if (Assigned(Database) and Assigned(Database.Triggers)) then
                    List.Add(Database.Triggers);
                ditEvent:
                  if (Assigned(Database) and Assigned(Database.Events)) then
                    List.Add(Database.Events);
                ditKey,
                ditField,
                ditForeignKey:
                  begin
                    if (Assigned(Table)) then
                      List.Add(Table)
                    else if (Assigned(Database)) then
                      List.Add(Database);
                    if (Assigned(Database) and Assigned(Database.Columns) and (PChar(@Item^.TableName) = '')) then
                      List.Add(Database.Columns);
                  end;
                ditUser:
                  List.Add(Session.Users);
                ditEngine:
                  List.Add(Session.Engines);
                ditCharset:
                  List.Add(Session.Charsets);
                ditCollation:
                  List.Add(Session.Collations);
              end;
            end;
        end;
      end;

      for I := 0 to List.Count - 1 do
        CanExecute := CanExecute
          and ((TObject(List[I]) is TSEntities) and TSEntities(List[I]).Valid
            or (TObject(List[I]) is TSObject) and TSObject(List[I]).Valid);

      if (not CanExecute) then
      begin
        CanExecute := Session.Update(List);
        if (not CanExecute) then
        begin
          SynCompletionPending.Active := True;
          SynCompletionPending.CurrentInput := CurrentInput;
          SynCompletionPending.X := X;
          SynCompletionPending.Y := Y;
          Wanted.Action := aSynCompletionExecute;
        end;
      end;

      if (CanExecute) then
      begin
        for I := 0 to Session.SQLParser.CompletionList.Count - 1 do
        begin
          Item := Session.SQLParser.CompletionList[I];
          case (Item^.ItemType) of
            itText:
              SynCompletionListAdd(
                StrPas(PChar(@Item^.Text)),
                StrPas(PChar(@Item^.Text)));
            itList:
              begin
                if (Item^.DatabaseName = '') then
                  Database := Session.DatabaseByName(SelectedDatabase)
                else
                  Database := Session.DatabaseByName(Item^.DatabaseName);
                if (not Assigned(Database) or (Item^.TableName = '')) then
                  Table := nil
                else
                  Table := Database.TableByName(Item^.TableName);
                case (Item^.DbIdentType) of
                  ditDatabase:
                    for J := 0 to Session.Databases.Count - 1 do
                      SynCompletionListAdd(
                        Session.Databases[J].Name,
                        Session.Connection.EscapeIdentifier(Session.Databases[J].Name));
                  ditTable:
                    if (Assigned(Database)) then
                      for J := 0 to Database.Tables.Count - 1 do
                        SynCompletionListAdd(
                          Database.Tables[J].Name,
                          Session.Connection.EscapeIdentifier(Database.Tables[J].Name));
                  ditProcedure:
                    if (Assigned(Database) and Assigned(Database.Routines)) then
                      for J := 0 to Database.Routines.Count - 1 do
                        if (Database.Routines[J] is TSProcedure) then
                          SynCompletionListAdd(
                            Database.Routines[J].Name,
                            Session.Connection.EscapeIdentifier(Database.Routines[J].Name));
                  ditFunction:
                    begin
                      if (Assigned(Database) and Assigned(Database.Routines)) then
                        for J := 0 to Database.Routines.Count - 1 do
                          if (Database.Routines[J] is TSFunction) then
                            SynCompletionListAdd(
                              Database.Routines[J].Name,
                              Session.Connection.EscapeIdentifier(Database.Routines[J].Name));
                    end;
                  ditTrigger:
                    if (Assigned(Database) and Assigned(Database.Triggers)) then
                      for J := 0 to Database.Triggers.Count - 1 do
                        SynCompletionListAdd(
                          Database.Triggers[J].Name,
                          Session.Connection.EscapeIdentifier(Database.Triggers[J].Name));
                  ditEvent:
                    if (Assigned(Database) and Assigned(Database.Events)) then
                      for J := 0 to Database.Events.Count - 1 do
                        SynCompletionListAdd(
                          Database.Events[J].Name,
                          Session.Connection.EscapeIdentifier(Database.Events[J].Name));
                  ditKey:
                    if (Table is TSBaseTable) then
                      for J := 0 to TSBaseTable(Table).Keys.Count - 1 do
                        SynCompletionListAdd(
                          TSBaseTable(Table).Keys[J].Name,
                          Session.Connection.EscapeIdentifier(TSBaseTable(Table).Keys[J].Name));
                  ditField:
                    if (Assigned(Table)) then
                      for J := 0 to Table.Fields.Count - 1 do
                        SynCompletionListAdd(
                          Table.Fields[J].Name,
                          Session.Connection.EscapeIdentifier(Table.Fields[J].Name))
                    else if (Assigned(Database) and Assigned(Database.Columns) and (PChar(@Item^.TableName) = '')) then
                      for J := 0 to Database.Columns.Count - 1 do
                      begin
                        ColumnName := Database.Columns[J]; // Buffer for speeding
                        SynCompletionListAdd(
                          ColumnName,
                          Session.Connection.EscapeIdentifier(ColumnName));
                      end;
                  ditForeignKey:
                    if (Table is TSBaseTable) then
                      for J := 0 to TSBaseTable(Table).ForeignKeys.Count - 1 do
                        SynCompletionListAdd(
                          TSBaseTable(Table).ForeignKeys[J].Name,
                          Session.Connection.EscapeIdentifier(TSBaseTable(Table).ForeignKeys[J].Name));
                  ditUser:
                    for J := 0 to Session.Users.Count - 1 do
                      SynCompletionListAdd(
                        Session.Users[J].Name,
                        Session.EscapeUser(Session.Users[J].Name));
                  ditEngine:
                    for J := 0 to Session.Engines.Count - 1 do
                      SynCompletionListAdd(
                        Session.Engines[J].Name,
                        Session.Engines[J].Name);
                  ditCharset:
                    for J := 0 to Session.Charsets.Count - 1 do
                      SynCompletionListAdd(
                        Session.Charsets[J].Name,
                        Session.Charsets[J].Name);
                  ditCollation:
                    for J := 0 to Session.Collations.Count - 1 do
                      SynCompletionListAdd(
                        Session.Collations[J].Name,
                        Session.Collations[J].Name);
                  ditDatatype:
                    for J := 0 to Session.FieldTypes.Count - 1 do
                      SynCompletionListAdd(
                        Session.FieldTypes[J].Name,
                        Session.FieldTypes[J].Name);
                  else
                    raise ERangeError.Create(SRangeError);
                end;
              end;
            else
              raise ERangeError.Create(SRangeError);
          end;
        end;

        CanExecute := SynCompletion.ItemList.Count > 0;

        if (CanExecute and (CurrentInput <> '')) then
        begin
          CanExecute := False; Len := Length(CurrentInput);
          for I := 0 to SynCompletion.ItemList.Count - 1 do
            if (AnsiStrLIComp(PChar(SynCompletion.ItemList[I]), PChar(CurrentInput), Len) = 0) then
            begin
              CanExecute := True;
              break;
            end;
        end;
      end;

      List.Free();
    end;

    Session.SQLParser.Clear();
  end;
end;

procedure TFSession.SynMemoApplyPreferences(const SynMemo: TSynMemo);
begin
  if (SynMemo <> FSQLEditorSynMemo) then
  begin
    SynMemo.ActiveLineColor := FSQLEditorSynMemo.ActiveLineColor;
    SynMemo.Font.Name := FSQLEditorSynMemo.Font.Name;
    SynMemo.Font.Style := FSQLEditorSynMemo.Font.Style;
    SynMemo.Font.Color := FSQLEditorSynMemo.Font.Color;
    SynMemo.Font.Size := FSQLEditorSynMemo.Font.Size;
    SynMemo.Font.Charset := FSQLEditorSynMemo.Font.Charset;
    SynMemo.Gutter.Font.Name := FSQLEditorSynMemo.Gutter.Font.Name;
    SynMemo.Gutter.Font.Style := FSQLEditorSynMemo.Gutter.Font.Style;
    SynMemo.Gutter.Font.Color := FSQLEditorSynMemo.Gutter.Font.Color;
    SynMemo.Gutter.Font.Size := FSQLEditorSynMemo.Gutter.Font.Size;
    SynMemo.Gutter.Font.Charset := FSQLEditorSynMemo.Gutter.Font.Charset;
    SynMemo.Gutter.Visible := FSQLEditorSynMemo.Gutter.Visible;
    SynMemo.Highlighter := FSQLEditorSynMemo.Highlighter;
    SynMemo.Options := FSQLEditorSynMemo.Options;
    SynMemo.RightEdge := FSQLEditorSynMemo.RightEdge;
    SynMemo.TabWidth := FSQLEditorSynMemo.TabWidth;
    SynMemo.WantTabs := FSQLEditorSynMemo.WantTabs;
    SynMemo.WordWrap := FSQLEditorSynMemo.WordWrap;
  end;
end;

procedure TFSession.SynMemoDragDrop(Sender, Source: TObject; X, Y: Integer);
var
  DatabaseName: string;
  S: string;
  SelStart: Integer;
begin
  if ((Source = FNavigator) and (Sender is TSynMemo)) then
  begin
    case (MouseDownNode.ImageIndex) of
      iiKey: S := TSKey(MouseDownNode.Data).Name;
      iiForeignKey: S := TSForeignKey(MouseDownNode.Data).Name;
      else S := MouseDownNode.Text;
    end;
    SelStart := TSynMemo(Sender).SelStart;
    TSynMemo(Sender).SelText := Session.Connection.EscapeIdentifier(S);
    TSynMemo(Sender).SelStart := SelStart;
    TSynMemo(Sender).SelLength := Length(Session.Connection.EscapeIdentifier(S));
    TSynMemo(Sender).AlwaysShowCaret := False;

    Window.ActiveControl := TSynMemo(Sender);
  end
  else if ((Source = FSQLHistory) and (Sender is TSynMemo)) then
  begin
    S := XMLNode(IXMLNode(MouseDownNode.Data), 'sql').Text;

    DatabaseName := XMLNode(IXMLNode(MouseDownNode.Data), 'database').Text;
    if (DatabaseName <> SelectedDatabase) then
      S := Session.Connection.SQLUse(DatabaseName) + S;
    S := ReplaceStr(ReplaceStr(S, #13#10, #10), #10, #13#10);

    SelStart := TSynMemo(Sender).SelStart;
    TSynMemo(Sender).SelText := S;
    TSynMemo(Sender).SelStart := SelStart;
    TSynMemo(Sender).SelLength := Length(S);
    TSynMemo(Sender).AlwaysShowCaret := False;

    Window.ActiveControl := TSynMemo(Sender);
  end
  else if ((Source = ActiveDBGrid) and (Sender = ActiveSynMemo)) then
  begin
    S := ActiveDBGrid.SelectedField.AsString;

    SelStart := TSynMemo(Sender).SelStart;
    TSynMemo(Sender).SelText := S;
    TSynMemo(Sender).SelStart := SelStart;
    TSynMemo(Sender).SelLength := Length(S);
    TSynMemo(Sender).AlwaysShowCaret := False;

    Window.ActiveControl := TSynMemo(Sender);
  end;
end;

procedure TFSession.SynMemoDragOver(Sender, Source: TObject; X, Y: Integer;
  State: TDragState; var Accept: Boolean);
begin
  if (Source = FNavigator) then
    Accept := MouseDownNode.ImageIndex in [iiDatabase, iiSystemDatabase, iiBaseTable, iiView, iiSystemView, iiProcedure, iiFunction, iiEvent, iiTrigger, iiKey, iiField, iiVirtualField, iiSystemViewField, iiViewField, iiForeignKey]
  else if (Source = FSQLHistory) then
    Accept := MouseDownNode.ImageIndex in [iiStatement, iiQuery, iiClock]
  else if (Source = ActiveDBGrid) then
    Accept := Assigned(ActiveDBGrid.SelectedField)
      and not ActiveDBGrid.SelectedField.IsNull
      and not (ActiveDBGrid.SelectedField.DataType in [ftWideMemo, ftBlob])
  else
    Accept := False;

  if (Accept) then
  begin
    if ((Sender = ActiveSynMemo) and not ActiveSynMemo.AlwaysShowCaret) then
    begin
      SynMemoBeforeDrag.SelStart := ActiveSynMemo.SelStart;
      SynMemoBeforeDrag.SelLength := ActiveSynMemo.SelLength;
      ActiveSynMemo.AlwaysShowCaret := True;
    end;

    if (not ActiveSynMemo.Gutter.Visible) then
      ActiveSynMemo.CaretX := (X) div ActiveSynMemo.CharWidth + 1
    else
      ActiveSynMemo.CaretX := (X - ActiveSynMemo.Gutter.RealGutterWidth(ActiveSynMemo.CharWidth)) div ActiveSynMemo.CharWidth + 1;
    ActiveSynMemo.CaretY := (Y div ActiveSynMemo.LineHeight) + 1;
  end;
end;

procedure TFSession.SynMemoEnter(Sender: TObject);
begin
  MainAction('aECopyToFile').OnExecute := SaveSQLFile;
  MainAction('aEPasteFromFile').OnExecute := aEPasteFromExecute;

  MainAction('aHIndex').ShortCut := 0;
  MainAction('aHSQL').ShortCut := ShortCut(VK_F1, []);

  SynMemoStatusChange(Sender, [scAll]);
  StatusBarRefresh();
end;

procedure TFSession.SynMemoExit(Sender: TObject);
begin
  MainAction('aFImportSQL').Enabled := False;
  MainAction('aFExportSQL').Enabled := False;
  MainAction('aERedo').Enabled := False;
  MainAction('aECopyToFile').Enabled := False;
  MainAction('aEPasteFromFile').Enabled := False;

  MainAction('aHIndex').ShortCut := ShortCut(VK_F1, []);
  MainAction('aHSQL').ShortCut := 0;

  SynCompletionPending.Active := False;
end;

procedure TFSession.SynMemoStatusChange(Sender: TObject; Changes: TSynStatusChanges);
var
  DDLStmt: TSQLDDLStmt;
  Empty: Boolean;
  Parse: TSQLParse;
  SelSQL: string;
  SQL: string;
begin
  if (not (csDestroying in ComponentState)) then
  begin
    KillTimer(Handle, tiShowSynCompletion);

    if (((scCaretX in Changes) or (scSelection in Changes) or (scModified in Changes) or (scAll in Changes)) and Assigned(ActiveSynMemo)) then
    begin
      SynCompletionPending.Active := False;

      SelSQL := ActiveSynMemo.SelText; // Cache, da Abfrage bei vielen Zeilen Zeit benötigt
      if (View = vIDE) then SQL := ActiveSynMemo.Text;

      Empty := ((ActiveSynMemo.Lines.Count <= 1) and (ActiveSynMemo.Text = '')); // Benötigt bei vielen Zeilen Zeit

      MainAction('aFSave').Enabled := not Empty and (View in [vEditor, vEditor2, vEditor3]) and (SQLEditors[View].Filename = '');
      MainAction('aFSaveAs').Enabled := not Empty and (View in [vEditor, vEditor2, vEditor3]);
      MainAction('aERedo').Enabled := ActiveSynMemo.CanRedo;
      MainAction('aECopyToFile').Enabled := (SelSQL <> '');
      MainAction('aEPasteFromFile').Enabled := (View in [vEditor, vEditor2, vEditor3]);
      MainAction('aDRun').Enabled :=
        ((View in [vEditor, vEditor2, vEditor3])
        or ((View = vBuilder) and FQueryBuilder.Visible)
        or ((View = vIDE) and SQLSingleStmt(SQL) and (SelectedImageIndex in [iiView, iiProcedure, iiFunction, iiEvent]))) and not Empty;
      MainAction('aDRunSelection').Enabled := (((View in [vEditor, vEditor2, vEditor3]) and not Empty) or Assigned(ActiveSynMemo) and (Trim(ActiveSynMemo.SelText) <> ''));
      MainAction('aDPostObject').Enabled := (View = vIDE) and ActiveSynMemo.Modified and SQLSingleStmt(SQL)
        and ((SelectedImageIndex in [iiView]) and SQLCreateParse(Parse, PChar(SQL), Length(SQL),Session.Connection.MySQLVersion) and (SQLParseKeyword(Parse, 'SELECT'))
          or (SelectedImageIndex in [iiProcedure, iiFunction]) and SQLParseDDLStmt(DDLStmt, PChar(SQL), Length(SQL), Session.Connection.MySQLVersion) and (DDLStmt.DefinitionType = dtCreate) and (DDLStmt.ObjectType in [otProcedure, otFunction])
          or (SelectedImageIndex in [iiEvent, iiTrigger]));
      MainAction('aEFormatSQL').Enabled := not Empty;
    end;

    StatusBarRefresh();
  end;
end;

procedure TFSession.TableOpen(Sender: TObject);
var
  FilterSQL: string;
  Limit: Integer;
  Offset: Integer;
  QuickSearch: string;
  SortDef: TIndexDef;
  Table: TSTable;
begin
  Table := TSTable(FNavigator.Selected.Data);

  if (not FLimitEnabled.Down) then
  begin
    Offset := 0;
    Limit := 0;
  end
  else
  begin
    Offset := FUDOffset.Position;
    Limit := FUDLimit.Position;
  end;

  if (not FFilterEnabled.Down) then
    FilterSQL := ''
  else
    FilterSQL := FFilter.Text;

  if (not FQuickSearchEnabled.Down) then
    QuickSearch := ''
  else
    QuickSearch := FQuickSearch.Text;

  SortDef := TIndexDef.Create(nil, '', '', []);

  if (not Table.DataSet.Active) then
  begin
    // Debug 2017-01-05
    if (not (TObject(Table) is TSTable)) then
      try
        raise ERangeError.Create('ClassType: ' + TObject(Table).ClassName);
      except
        raise ERangeError.Create(SRangeError);
      end;

    if ((Table is TSBaseTable) and Assigned(TSBaseTable(Table).PrimaryKey)) then
      TSBaseTable(Table).PrimaryKey.GetSortDef(SortDef);

    // Debug 2017-01-05
    if (not (TObject(Table) is TSTable)) then
      try
        raise ERangeError.Create('ClassType: ' + TObject(Table).ClassName);
      except
        raise ERangeError.Create(SRangeError);
      end;

    Table.DataSet.AfterOpen := Desktop(Table).DataSetAfterOpen;

    // Debug 2017-01-05
    if (not (TObject(Table) is TSTable)) then
      try
        raise ERangeError.Create('ClassType: ' + TObject(Table).ClassName);
      except
        raise ERangeError.Create(SRangeError);
      end;

    Table.DataSet.AfterRefresh := Desktop(Table).DataSetAfterRefresh;

    // Debug 2017-01-05
    if (not (TObject(Table) is TSTable)) then
      try
        raise ERangeError.Create('ClassType: ' + TObject(Table).ClassName);
      except
        raise ERangeError.Create(SRangeError);
      end;

    if (Table is TSView) then
      Table.DataSet.BeforeOpen := Desktop(TSView(Table)).DataSetBeforeOpen;

    // Debug 2017-01-01
    if (not (TObject(Table) is TSTable)) then
      try
        raise ERangeError.Create('ClassType: ' + TObject(Table).ClassName);
      except
        raise ERangeError.Create(SRangeError);
      end;

    Table.Open(FilterSQL, QuickSearch, SortDef, Offset, Limit);
  end
  else
  begin
    Table.DataSet.FilterSQL := FilterSQL;
    Table.DataSet.QuickSearch := QuickSearch;
    Table.DataSet.Offset := Offset;
    Table.DataSet.Limit := Limit;
    Table.DataSet.Refresh();
  end;

  SortDef.Free();
end;

procedure TFSession.TCResultMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
var
  Index: Integer;
  SQLEditor: TSQLEditor;
  TabControl: TTabControl;
begin
  if ((Sender is TTabControl) and (Shift = [])) then
  begin
    TabControl := TTabControl(Sender);

    if (TObject(TabControl.Tag) is TSQLEditor) then
    begin
      SQLEditor := TSQLEditor(TabControl.Tag);
      Index := TabControl.IndexOfTabAt(X, Y);

      if (Index >= 0) then
        TabControl.Hint := TSQLEditor.TResult(SQLEditor.Results[Index]^).DataSet.CommandText;
    end;
  end;
end;

procedure TFSession.ToolBarResize(Sender: TObject);
begin
  if ((Sender is TToolBar) and (TToolBar(Sender).Parent is TPanel)) then
    PHeader.ClientHeight := TToolBar(Sender).Height + PHeader.Canvas.Pen.Width;
end;

procedure TFSession.ToolBarTabsClick(Sender: TObject);
begin
  Wanted.Clear();

  if (Sender = mtObjects) then
    if (mtObjects.Checked) then
      Exclude(Preferences.ToolbarTabs, ttObjects)
    else
      Include(Preferences.ToolbarTabs, ttObjects);
  if (Sender = mtBrowser) then
    if (mtBrowser.Checked) then
      Exclude(Preferences.ToolbarTabs, ttBrowser)
    else
      Include(Preferences.ToolbarTabs, ttBrowser);
  if (Sender = mtIDE) then
    if (mtIDE.Checked) then
      Exclude(Preferences.ToolbarTabs, ttIDE)
    else
      Include(Preferences.ToolbarTabs, ttIDE);
  if (Sender = mtBuilder) then
    if (mtBuilder.Checked) then
      Exclude(Preferences.ToolbarTabs, ttBuilder)
    else
      Include(Preferences.ToolbarTabs, ttBuilder);
  if (Sender = mtDiagram) then
    if (mtDiagram.Checked) then
      Exclude(Preferences.ToolbarTabs, ttDiagram)
    else
      Include(Preferences.ToolbarTabs, ttDiagram);
  if (Sender = mtEditor) then
    if (mtEditor.Checked) then
      Exclude(Preferences.ToolbarTabs, ttEditor)
    else
      Include(Preferences.ToolbarTabs, ttEditor);
  if (Sender = mtEditor2) then
    if (mtEditor2.Checked) then
      Exclude(Preferences.ToolbarTabs, ttEditor2)
    else
      Include(Preferences.ToolbarTabs, ttEditor2);
  if (Sender = mtEditor3) then
    if (mtEditor3.Checked) then
      Exclude(Preferences.ToolbarTabs, ttEditor3)
    else
      Include(Preferences.ToolbarTabs, ttEditor3);

  PostMessage(Handle, UM_CHANGEPREFERENCES, 0, 0);
end;

procedure TFSession.ToolButtonStyleClick(Sender: TObject);
begin
  Wanted.Clear();

  TToolButton(Sender).CheckMenuDropdown();
end;

procedure TFSession.TreeViewCollapsed(Sender: TObject; Node: TTreeNode);
begin
  aPExpand.Enabled := not (Node.ImageIndex in [iiServer]) and Node.HasChildren;
  aPCollapse.Enabled := False;

  if ((Sender is TTreeView_Ext) and Assigned(TTreeView_Ext(Sender).PopupMenu)) then
    ShowEnabledItems(TTreeView_Ext(Sender).PopupMenu.Items);
end;

procedure TFSession.TreeViewCollapsing(Sender: TObject;
  Node: TTreeNode; var AllowCollapse: Boolean);
begin
  if ((Sender is TTreeView_Ext) and Assigned(TTreeView_Ext(Sender).Selected) and Assigned(TTreeView_Ext(Sender).OnChange)) then
  begin
    if ((View = vBrowser) and not (Node.ImageIndex in [iiBaseTable, iiView, iiSystemView]) and (Node = TTreeView_Ext(Sender).Selected.Parent)) then
      TTreeView_Ext(Sender).Selected := Node;

    AllowCollapse := Node <> TTreeView_Ext(Sender).Items.getFirstNode();
  end;
end;

procedure TFSession.TreeViewEndDrag(Sender, Target: TObject; X, Y: Integer);
begin
  if (Assigned(ActiveSynMemo) and (ActiveSynMemo.AlwaysShowCaret)) then
  begin
    ActiveSynMemo.SelStart := SynMemoBeforeDrag.SelStart;
    ActiveSynMemo.SelLength := SynMemoBeforeDrag.SelLength;
    ActiveSynMemo.AlwaysShowCaret := False;
  end;
end;

procedure TFSession.TreeViewExpanded(Sender: TObject; Node: TTreeNode);
begin
  aPExpand.Enabled := False;
  aPCollapse.Enabled := Node.ImageIndex <> iiServer;

  if ((Sender is TTreeView_Ext) and Assigned(TTreeView_Ext(Sender).PopupMenu)) then
    ShowEnabledItems(TTreeView_Ext(Sender).PopupMenu.Items);
end;

procedure TFSession.TreeViewGetSelectedIndex(Sender: TObject; Node: TTreeNode);
begin
  Node.SelectedIndex := Node.ImageIndex;
end;

procedure TFSession.TreeViewMouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  LeftMousePressed := (Sender is TTreeView_Ext) and (Button = mbLeft);
  if (LeftMousePressed) then
    MouseDownNode := TTreeView_Ext(Sender).GetNodeAt(X, Y);
  Exclude(FrameState, tsLoading);
end;

procedure TFSession.TreeViewMouseUp(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  LeftMousePressed := False;
end;

procedure TFSession.UMActivateDBGrid(var Message: TMessage);
begin
  Window.ActiveControl := TWinControl(Message.LParam);
  ActiveDBGrid.EditorMode := False;
end;

procedure TFSession.UMActivateFText(var Message: TMessage);
const
  KEYEVENTF_UNICODE = 4;
var
  Input: TInput;
begin
  if (Message.WParam <> 0) then
  begin
    ZeroMemory(@Input, SizeOf(Input));
    Input.Itype := INPUT_KEYBOARD;
    Input.ki.wVk := Message.WParam;
    Input.ki.dwFlags := KEYEVENTF_UNICODE;
    SendInput(1, Input, SizeOf(Input));
  end;
  FText.SelStart := Length(FText.Text);
end;

procedure TFSession.UMChangePreferences(var Message: TMessage);
var
  I: Integer;
begin
  if (not CheckWin32Version(6) or TStyleManager.Enabled and (TStyleManager.ActiveStyle <> TStyleManager.SystemStyle)) then
  begin
    TBSideBar.BorderWidth := 0;
    ToolBar.BorderWidth := 0;
  end
  else
  begin
    TBSideBar.BorderWidth := GetSystemMetrics(SM_CXEDGE);
    ToolBar.BorderWidth := GetSystemMetrics(SM_CXEDGE);
    TBObjectSearch.BorderWidth := GetSystemMetrics(SM_CXEDGE);
  end;

  Session.SQLMonitor.CacheSize := Preferences.LogSize;
  if (Preferences.LogResult) then
    Session.SQLMonitor.TraceTypes := Session.SQLMonitor.TraceTypes + [ttInfo]
  else
    Session.SQLMonitor.TraceTypes := Session.SQLMonitor.TraceTypes - [ttInfo];
  if (Preferences.LogTime) then
    Session.SQLMonitor.TraceTypes := Session.SQLMonitor.TraceTypes + [ttTime]
  else
    Session.SQLMonitor.TraceTypes := Session.SQLMonitor.TraceTypes - [ttTime];

  aPExpand.Caption := Preferences.LoadStr(150);
  aPCollapse.Caption := Preferences.LoadStr(151);
  aDDelete.Caption := Preferences.LoadStr(28);
  aDPrev.Caption := Preferences.LoadStr(512);
  aDNext.Caption := Preferences.LoadStr(513);
  DataSetFirst.Caption := Preferences.LoadStr(514);
  DataSetLast.Caption := Preferences.LoadStr(515);
  DataSetPost.Caption := Preferences.LoadStr(516);
  DataSetCancel.Caption := Preferences.LoadStr(517);
  aVBlobText.Caption := Preferences.LoadStr(379);
  aVBlobRTF.Caption := 'RTF';
  aVBlobImage.Caption := Preferences.LoadStr(380);
  aVBlobHexEditor.Caption := Preferences.LoadStr(381);
  mwDCreateTable.Caption := MainAction('aDCreateTable').Caption;
  mwCreateSection.Caption := Preferences.LoadStr(877) + ' ...';
  mwCreateLink.Caption := Preferences.LoadStr(251) + ' ...';

  for I := 0 to ActionList.ActionCount - 1 do
    if (ActionList.Actions[I] is TCustomAction) and (TCustomAction(ActionList.Actions[I]).Hint = '') then
      TCustomAction(ActionList.Actions[I]).Hint := TCustomAction(ActionList.Actions[I]).Caption;

  tbObjects.Caption := Preferences.LoadStr(4);
  tbBrowser.Caption := Preferences.LoadStr(5);
  tbIDE.Caption := Preferences.LoadStr(865);
  tbBuilder.Caption := tbBuilder.Caption;
  tbDiagram.Caption := Preferences.LoadStr(800);
  tbEditor.Caption := Preferences.LoadStr(6);
  tbEditor2.Caption := Preferences.LoadStr(6) + ' #2';
  tbEditor3.Caption := Preferences.LoadStr(6) + ' #3';

  mfOpen.Caption := Preferences.LoadStr(581);
  mfFilter.Caption := Preferences.LoadStr(209);
  mfFilterClear.Caption := FilterDescription('*') + ' (*.*)';
  mfFilterSQL.Caption := FilterDescription('sql') + ' (*.sql)';
  mfFilterText.Caption := FilterDescription('txt') + ' (*.txt,*.csv)';
  mfFilterHTML.Caption := FilterDescription('html') + ' (*.html,*.hmt)';
  mfFilterXML.Caption := FilterDescription('xml') + ' (*.xml)';
  mfFilterAccess.Caption := FilterDescription('mdb') + ' (*.mdb;*.accdb)';
  mfFilterExcel.Caption := FilterDescription('xls') + ' (*.xls;*.xlsb)';
  mfDelete.Caption := Preferences.LoadStr(28);
  mfRename.Caption := Preferences.LoadStr(98);
  mfProperties.Caption := Preferences.LoadStr(97) + '...';

  miNImport.Caption := Preferences.LoadStr(371);
  miNExport.Caption := Preferences.LoadStr(200);
  miNCreate.Caption := Preferences.LoadStr(26);
  miNDelete.Caption := Preferences.LoadStr(28);

  miHOpen.Caption := Preferences.LoadStr(581);
  miHSaveAs.Caption := MainAction('aFSaveAs').Caption;
  miHStatementIntoSQLEditor.Caption := Preferences.LoadStr(198) + ' -> ' + Preferences.LoadStr(20);
  miHRun.Caption := MainAction('aDRun').Caption;
  miHProperties.Caption := Preferences.LoadStr(684) + '...';

  mlOpen.Caption := Preferences.LoadStr(581);
  mlFImport.Caption := Preferences.LoadStr(371);
  mlFExport.Caption := Preferences.LoadStr(200);
  mlDCreate.Caption := Preferences.LoadStr(26);
  mlDDelete.Caption := Preferences.LoadStr(28);

  mwFImport.Caption := Preferences.LoadStr(371);
  mwFExport.Caption := Preferences.LoadStr(200);
  mwAddTable.Caption := Preferences.LoadStr(383);
  mwEPaste.Caption := MainAction('aEPaste').Caption;
  mwDCreate.Caption := Preferences.LoadStr(26);
  mwDProperties.Caption := Preferences.LoadStr(97) + '...';

  gmFExport.Caption := Preferences.LoadStr(200);
  gmFilter.Caption := Preferences.LoadStr(209);

  ghmCopy.Caption := Preferences.LoadStr(64);
  ghmGoto.Caption := Preferences.LoadStr(676);

  mtObjects.Caption := tbObjects.Caption;
  mtBrowser.Caption := tbBrowser.Caption;
  mtIDE.Caption := tbIDE.Caption;
  mtBuilder.Caption := tbBuilder.Caption;
  mtDiagram.Caption := tbDiagram.Caption;
  mtEditor.Caption := tbEditor.Caption;
  mtEditor2.Caption := tbEditor2.Caption;
  mtEditor3.Caption := tbEditor3.Caption;

  tbObjects.Visible := ttObjects in Preferences.ToolbarTabs;
  tbBrowser.Visible := ttBrowser in Preferences.ToolbarTabs;
  tbIDE.Visible := ttIDE in Preferences.ToolbarTabs;
  tbBuilder.Visible := ttBuilder in Preferences.ToolbarTabs;
  tbEditor.Visible := ttEditor in Preferences.ToolbarTabs;
  tbEditor2.Visible := ttEditor2 in Preferences.ToolbarTabs;
  tbEditor3.Visible := ttEditor3 in Preferences.ToolbarTabs;
  tbDiagram.Visible := ttDiagram in Preferences.ToolbarTabs;
  PHeaderResize(nil);


  if (not (tsLoading in FrameState)) then
  begin
    SessionUpdate(nil);

    for I := 0 to PListView.ControlCount - 1 do
      if (PListView.Controls[I] is TListView) then
      begin
        ListViewInitialize(TListView(PListView.Controls[I]));

        if (PListView.Controls[I].Tag = 0) then
          Session.Databases.PushBuildEvent(nil)
        else if (TObject(PListView.Controls[I].Tag) is TSProcesses) then
          Session.Processes.PushBuildEvent(nil)
        else if (TObject(PListView.Controls[I].Tag) is TSUsers) then
          Session.Users.PushBuildEvent(nil)
        else if (TObject(PListView.Controls[I].Tag) is TSVariables) then
          Session.Variables.PushBuildEvent(nil)
        else if (TObject(PListView.Controls[I].Tag) is TSDatabase) then
          TSDatabase(PListView.Controls[I].Tag).PushBuildEvents()
        else if (TObject(PListView.Controls[I].Tag) is TSTable) then
          TSTable(PListView.Controls[I].Tag).PushBuildEvent();
      end;
  end;

  FObjectSearch.Hint := Preferences.LoadStr(934);
  FObjectSearchStart.Hint := Preferences.LoadStr(424);
  FOffset.Hint := Preferences.LoadStr(846) + ' (' + ShortCutToText(aTBOffset.ShortCut) + ')';
  FUDOffset.Hint := Preferences.LoadStr(846);
  FLimit.Hint := Preferences.LoadStr(197) + ' (' + ShortCutToText(aTBLimit.ShortCut) + ')';
  FUDLimit.Hint := Preferences.LoadStr(846);
  FLimitEnabled.Hint := Preferences.LoadStr(197);
  FFilter.Hint := Preferences.LoadStr(209) + ' (' + ShortCutToText(aTBFilter.ShortCut) + ')';
  FFilterEnabled.Hint := Preferences.LoadStr(209);
  FQuickSearch.Hint := Preferences.LoadStr(424) + ' (' + ShortCutToText(aTBQuickSearch.ShortCut) + ')';
  FQuickSearchEnabled.Hint := Preferences.LoadStr(424);
  if (CheckWin32Version(6)) then
  begin
    SendMessage(FFilter.Handle, CB_SETCUEBANNER, 0, LParam(PChar(Preferences.LoadStr(209))));
    SendMessage(FQuickSearch.Handle, EM_SETCUEBANNER, 0, LParam(PChar(Preferences.LoadStr(424))));
    SendMessage(FObjectSearch.Handle, EM_SETCUEBANNER, 0, LParam(PChar(Preferences.LoadStr(934))));
  end;

  if (not Preferences.Editor.CurrRowBGColorEnabled) then
    FSQLEditorSynMemo.ActiveLineColor := clNone
  else
    FSQLEditorSynMemo.ActiveLineColor := Preferences.Editor.CurrRowBGColor;
  if (Preferences.Editor.LineNumbersBackground = clNone) then
    FSQLEditorSynMemo.Gutter.Color := clBtnFace
  else
    FSQLEditorSynMemo.Gutter.Color := Preferences.Editor.LineNumbersBackground;
  FSQLEditorSynMemo.Gutter.Font.Style := Preferences.Editor.LineNumbersStyle;
  FSQLEditorSynMemo.Gutter.Font.Size := FSQLEditorSynMemo.Font.Size;
  FSQLEditorSynMemo.Gutter.Font.Charset := FSQLEditorSynMemo.Font.Charset;
  FSQLEditorSynMemo.Options := FSQLEditorSynMemo.Options + [eoScrollHintFollows];  // Slow down the performance on large content

  for I := 0 to PSynMemo.ControlCount - 1 do
    if (PSynMemo.Controls[I] is TSynMemo) then
      SynMemoApplyPreferences(TSynMemo(PSynMemo.Controls[I]));

  SynMemoApplyPreferences(FQueryBuilderSynMemo);

  smEEmpty.Caption := Preferences.LoadStr(181);

  BINSERT.Font := FSQLEditorSynMemo.Font;
  BINSERT.Font.Style := [fsBold];
  BREPLACE.Font := BINSERT.Font;
  BUPDATE.Font := BINSERT.Font;
  BDELETE.Font := BINSERT.Font;

  OpenDialog.EncodingLabel := Preferences.LoadStr(682) + ':';
  SaveDialog.EncodingLabel := Preferences.LoadStr(682) + ':';

  Perform(CM_PARENTCOLORCHANGED, 0, 0);
  Perform(CM_PARENTFONTCHANGED, 0, 0);
  Perform(CM_PARENTSHOWHINTCHANGED, 0, 0);
  Perform(CM_PARENTBIDIMODECHANGED, 0, 0);
  Perform(CM_PARENTDOUBLEBUFFEREDCHANGED, 0, 0);
  Perform(CM_PARENTTABLETOPTIONSCHANGED, 0, 0);

  FQueryBuilderSQLUpdated(nil);

  FLog.Font.Name := Preferences.LogFontName;
  FLog.Font.Style := Preferences.LogFontStyle;
  FLog.Font.Color := Preferences.LogFontColor;
  FLog.Font.Size := Preferences.LogFontSize;
  FLog.Font.Charset := Preferences.LogFontCharset;

  PasteMode := False;
end;

procedure TFSession.UMCloseFrame(var Message: TMessage);
begin
  MainAction('aFClose').Execute();
end;

procedure TFSession.UMCloseTabQuery(var Message: TMessage);
var
  CanClose: Boolean;
  I: Integer;
  J: Integer;
  SObject: TSObject;
  SynMemo: TSynMemo;
  View: TView;
begin
  // Debug 2016-12-26
  if (not Assigned(Session.Account)) then
    raise ERangeError.Create(SRangeError);
  if (not (TObject(Session.Account) is TPAccount)) then
    try
      raise ERangeError.Create(SRangeError + ' ClassType: ' + TObject(Session.Account).ClassName)
    except
      raise ERangeError.Create(SRangeError); // Occured on 2016-12-25
    end;

  CanClose := True;

  if (CanClose and Assigned(ActiveDBGrid) and Assigned(ActiveDBGrid.DataSource.DataSet) and ActiveDBGrid.DataSource.DataSet.Active) then
    ActiveDBGrid.DataSource.DataSet.CheckBrowseMode();

  for I := 0 to PSynMemo.ControlCount - 1 do
    if (CanClose) then
      if (PSynMemo.Controls[I] is TSynMemo) then
      begin
        SynMemo := TSynMemo(PSynMemo.Controls[I]);
        if (SynMemo.Modified) then
        begin
          SObject := TSObject(SynMemo.Tag);
          if (Assigned(SObject)) then
            for J := 0 to FNavigator.Items.Count - 1 do
              if (FNavigator.Items[J].Data = SObject) then
              begin
                Address := NavigatorNodeToAddress(FNavigator.Items[J]);
                if (SynMemo = SQLEditors[vEditor].SynMemo) then Self.View := vEditor
                else if (Assigned(SQLEditor2) and (SynMemo = SQLEditor2.SynMemo)) then Self.View := vEditor2
                else if (Assigned(SQLEditor3) and (SynMemo = SQLEditor3.SynMemo)) then Self.View := vEditor3;
                Window.ActiveControl := ActiveSynMemo;
                case (MsgBox(Preferences.LoadStr(584, SObject.Name), Preferences.LoadStr(101), MB_YESNOCANCEL + MB_ICONQUESTION)) of
                  IDYES: MainAction('aDPostObject').Execute();
                  IDCANCEL: CanClose := False;
                end;
              end;
        end;
      end;

  if (CanClose) then
    for View in [vEditor, vEditor2, vEditor3] do
      if (Assigned(SQLEditors[View]) and SQLEditors[View].SynMemo.Modified and (SQLEditors[View].Filename <> '')) then
      begin
        // Debug 2016-12-06
        if (not (View in [vEditor, vEditor2, vEditor3])) then
          raise ERangeError.Create(SRangeError);

        Self.View := View;
        Window.ActiveControl := ActiveSynMemo;
        case (MsgBox(Preferences.LoadStr(584, ExtractFileName(SQLEditors[View].Filename)), Preferences.LoadStr(101), MB_YESNOCANCEL + MB_ICONQUESTION)) of
          IDYES: SaveSQLFile(MainAction('aFSave'));
          IDCANCEL: CanClose := False;
        end;
      end;

  for I := 0 to Session.Databases.Count - 1 do
    if (CanClose) then
      Desktop(Session.Databases[I]).CloseQuery(nil, CanClose);

  if (not CanClose) then
    Message.Result := 0
  else
    Message.Result := 1;

  // Debug 2016-12-26
  if (not Assigned(Session.Account)) then
    raise ERangeError.Create(SRangeError);
  if (not (TObject(Session.Account) is TPAccount)) then
    try
      raise ERangeError.Create(SRangeError + ' ClassType: ' + TObject(Session.Account).ClassName)
    except
      raise ERangeError.Create(SRangeError); // Occured on 2016-12-25
    end;
end;

procedure TFSession.UMFrameActivate(var Message: TMessage);
begin
  // Debug 2016-12-21
  // This is a helper for TWWindow.UMUpdateToolbar
  if (not Assigned(Session.Account.Desktop)) then
    raise ERangeError.Create(SRangeError);

  Include(FrameState, tsActive);

  FormatSettings.ThousandSeparator := Session.Connection.FormatSettings.ThousandSeparator;
  FormatSettings.DecimalSeparator := Session.Connection.FormatSettings.DecimalSeparator;
  FormatSettings.ShortDateFormat := Session.Connection.FormatSettings.ShortDateFormat;
  FormatSettings.LongTimeFormat := Session.Connection.FormatSettings.LongTimeFormat;
  FormatSettings.DateSeparator := Session.Connection.FormatSettings.DateSeparator;
  FormatSettings.TimeSeparator := Session.Connection.FormatSettings.TimeSeparator;

  Session.Connection.OnConvertError := OnConvertError;

  if (Window.ActiveControl is TWinControl) then
    Perform(CM_ENTER, 0, 0);

  if (Assigned(MainActionList)) then
  begin
    MainAction('aVNavigator').Checked := PNavigator.Visible;
    MainAction('aVExplorer').Checked := PExplorer.Visible;
    MainAction('aVSQLHistory').Checked := PSQLHistory.Visible;
    MainAction('aVSQLLog').Checked := PLog.Visible;

    MainAction('aFOpen').OnExecute := aFOpenExecute;
    MainAction('aFSave').OnExecute := aFSaveExecute;
    MainAction('aFSaveAs').OnExecute := aFSaveAsExecute;
    MainAction('aFImportSQL').OnExecute := aFImportSQLExecute;
    MainAction('aFImportText').OnExecute := aFImportTextExecute;
    MainAction('aFImportExcel').OnExecute := aFImportExcelExecute;
    MainAction('aFImportAccess').OnExecute := aFImportAccessExecute;
    MainAction('aFImportODBC').OnExecute := aFImportODBCExecute;
    MainAction('aFExportSQL').OnExecute := aFExportSQLExecute;
    MainAction('aFExportText').OnExecute := aFExportTextExecute;
    MainAction('aFExportExcel').OnExecute := aFExportExcelExecute;
    MainAction('aFExportAccess').OnExecute := aFExportAccessExecute;
    MainAction('aFExportODBC').OnExecute := aFExportODBCExecute;
    MainAction('aFExportXML').OnExecute := aFExportXMLExecute;
    MainAction('aFExportHTML').OnExecute := aFExportHTMLExecute;
    MainAction('aFExportPDF').OnExecute := aFExportPDFExecute;
    MainAction('aFExportBitmap').OnExecute := aFExportBitmapExecute;
    MainAction('aERedo').OnExecute := aERedoExecute;
    MainAction('aECopy').OnExecute := aECopyExecute;
    MainAction('aEPaste').OnExecute := aEPasteExecute;
    MainAction('aERename').OnExecute := aERenameExecute;
    MainAction('aVObjects').OnExecute := aViewExecute;
    MainAction('aVBrowser').OnExecute := aViewExecute;
    MainAction('aVIDE').OnExecute := aViewExecute;
    MainAction('aVBuilder').OnExecute := aViewExecute;
    MainAction('aVSQLEditor').OnExecute := aViewExecute;
    MainAction('aVSQLEditor2').OnExecute := aViewExecute;
    MainAction('aVSQLEditor3').OnExecute := aViewExecute;
    MainAction('aVDiagram').OnExecute := aViewExecute;
    MainAction('aVNavigator').OnExecute := aVSideBarExecute;
    MainAction('aVExplorer').OnExecute := aVSideBarExecute;
    MainAction('aVSQLHistory').OnExecute := aVSideBarExecute;
    MainAction('aVSQLLog').OnExecute := aVSQLLogExecute;
    MainAction('aVRefresh').OnExecute := aVRefreshExecute;
    MainAction('aVRefreshAll').OnExecute := aVRefreshAllExecute;
    MainAction('aDCancel').OnExecute := aDCancelExecute;
    MainAction('aDCreateDatabase').OnExecute := aDCreateDatabaseExecute;
    MainAction('aDCreateTable').OnExecute := aDCreateTableExecute;
    MainAction('aDCreateView').OnExecute := aDCreateViewExecute;
    MainAction('aDCreateProcedure').OnExecute := aDCreateRoutineExecute;
    MainAction('aDCreateFunction').OnExecute := aDCreateRoutineExecute;
    MainAction('aDCreateKey').OnExecute := aDCreateKeyExecute;
    MainAction('aDCreateField').OnExecute := aDCreateFieldExecute;
    MainAction('aDCreateForeignKey').OnExecute := aDCreateForeignKeyExecute;
    MainAction('aDCreateTrigger').OnExecute := aDCreateTriggerExecute;
    MainAction('aDCreateEvent').OnExecute := aDCreateEventExecute;
    MainAction('aDCreateUser').OnExecute := aDCreateUserExecute;
    MainAction('aDEditServer').OnExecute := PropertiesServerExecute;
    MainAction('aDEditDatabase').OnExecute := aDPropertiesExecute;
    MainAction('aDEditTable').OnExecute := aDPropertiesExecute;
    MainAction('aDEditView').OnExecute := aDPropertiesExecute;
    MainAction('aDEditRoutine').OnExecute := aDPropertiesExecute;
    MainAction('aDEditEvent').OnExecute := aDPropertiesExecute;
    MainAction('aDEditTrigger').OnExecute := aDPropertiesExecute;
    MainAction('aDEditKey').OnExecute := aDPropertiesExecute;
    MainAction('aDEditField').OnExecute := aDPropertiesExecute;
    MainAction('aDEditForeignKey').OnExecute := aDPropertiesExecute;
    MainAction('aDEditProcess').OnExecute := aDPropertiesExecute;
    MainAction('aDEditUser').OnExecute := aDPropertiesExecute;
    MainAction('aDEditVariable').OnExecute := aDPropertiesExecute;
    MainAction('aDDeleteDatabase').OnExecute := aDDeleteExecute;
    MainAction('aDDeleteTable').OnExecute := aDDeleteExecute;
    MainAction('aDDeleteView').OnExecute := aDDeleteExecute;
    MainAction('aDDeleteRoutine').OnExecute := aDDeleteExecute;
    MainAction('aDDeleteKey').OnExecute := aDDeleteExecute;
    MainAction('aDDeleteField').OnExecute := aDDeleteExecute;
    MainAction('aDDeleteForeignKey').OnExecute := aDDeleteExecute;
    MainAction('aDDeleteTrigger').OnExecute := aDDeleteExecute;
    MainAction('aDDeleteEvent').OnExecute := aDDeleteExecute;
    MainAction('aDDeleteUser').OnExecute := aDDeleteExecute;
    MainAction('aDDeleteProcess').OnExecute := aDDeleteExecute;
    MainAction('aDInsertRecord').OnExecute := aDInsertRecordExecute;
    MainAction('aDDeleteRecord').OnExecute := aDDeleteRecordExecute;
    MainAction('aDRun').OnExecute := aDRunExecute;
    MainAction('aDRunSelection').OnExecute := aDRunSelectionExecute;
    MainAction('aDPostObject').OnExecute := aDPostObjectExecute;
    MainAction('aEFormatSQL').OnExecute := aEFormatSQLExecute;
    MainAction('aHSQL').OnExecute := aHSQLExecute;
    MainAction('aHManual').OnExecute := aHManualExecute;


    MainAction('aVObjects').Enabled := True;
    MainAction('aVBrowser').Enabled := (SelectedImageIndex in [iiBaseTable, iiView, iiSystemView, iiTrigger]) or ((LastSelectedDatabase <> '') and (LastSelectedDatabase = SelectedDatabase) and (LastSelectedTable <> ''));
    MainAction('aVIDE').Enabled := (SelectedImageIndex in [iiView, iiProcedure, iiFunction, iiEvent, iiTrigger]) or (LastObjectIDEAddress <> '');
    MainAction('aVBuilder').Enabled := (LastSelectedDatabase <> '');
    MainAction('aVSQLEditor').Enabled := True;
    MainAction('aVSQLEditor2').Enabled := True;
    MainAction('aVSQLEditor3').Enabled := True;
    MainAction('aVDiagram').Enabled := (LastSelectedDatabase <> '');
    MainAction('aVNavigator').Enabled := True;
    MainAction('aVExplorer').Enabled := True;
    MainAction('aVSQLHistory').Enabled := True;
    MainAction('aVSQLLog').Enabled := True;
    MainAction('aVRefresh').Enabled := True;
    MainAction('aVRefreshAll').Enabled := True;
    MainAction('aDCancel').Enabled := Session.Connection.InUse();
    MainAction('aHSQL').Enabled := Session.Connection.MySQLVersion >= 40100;
    MainAction('aHManual').Enabled := Session.Account.ManualURL <> '';

    aPResult.ShortCut := ShortCut(VK_F8, [ssAlt]);

    if (Assigned(ActiveControlOnDeactivate) and ActiveControlOnDeactivate.Visible) then
      try Window.FocusControl(ActiveControlOnDeactivate); except end;

    if (Assigned(Window.ActiveControl)) then
      if (Window.ActiveControl = FNavigator) then FNavigatorEnter(FNavigator)
      else if (Window.ActiveControl = ActiveListView) then ListViewEnter(ActiveListView)
      else if (Window.ActiveControl = FLog) then FLogEnter(FLog)
      else if (Window.ActiveControl is TSynMemo) then SynMemoEnter(Window.ActiveControl)
      else if (Window.ActiveControl = ActiveDBGrid) then DBGridEnter(ActiveDBGrid);

    if (Assigned(FNavigatorNodeAfterActivate)) then
      FNavigatorChange2(FNavigator, FNavigatorNodeAfterActivate);

    if (Assigned(FFolders) and (Path <> FFolders.SelectedFolder)) then
      FFolders.SelectedFolder := Path;
  end;

  if (Assigned(StatusBar)) then
    StatusBarRefresh(True);
end;

procedure TFSession.UMFrameDeactivate(var Message: TMessage);
begin
  KillTimer(Handle, tiNavigator);
  KillTimer(Handle, tiStatusBar);

  ActiveControlOnDeactivate := Window.ActiveControl;

  MainAction('aVObjects').Enabled := False;
  MainAction('aVBrowser').Enabled := False;
  MainAction('aVIDE').Enabled := False;
  MainAction('aVBuilder').Enabled := False;
  MainAction('aVSQLEditor').Enabled := False;
  MainAction('aVSQLEditor2').Enabled := False;
  MainAction('aVSQLEditor3').Enabled := False;
  MainAction('aVDiagram').Enabled := False;
  MainAction('aVNavigator').Enabled := False;
  MainAction('aVExplorer').Enabled := False;
  MainAction('aVSQLHistory').Enabled := False;
  MainAction('aVSQLLog').Enabled := False;
  MainAction('aVRefresh').Enabled := False;
  MainAction('aVRefreshAll').Enabled := False;
  MainAction('aDCancel').Enabled := False;
  MainAction('aHSQL').Enabled := False;
  MainAction('aHManual').Enabled := False;

  MainAction('aECopy').OnExecute := nil;
  MainAction('aEPaste').OnExecute := nil;

  aPResult.ShortCut := 0;

  if (Window.ActiveControl = FNavigator) then FNavigatorExit(Window.ActiveControl)
  else if (Window.ActiveControl = ActiveListView) then ListViewExit(Window.ActiveControl)
  else if (Window.ActiveControl = FLog) then FLogExit(Window.ActiveControl)
  else if (Window.ActiveControl is TSynMemo) then SynMemoExit(Window.ActiveControl)
  else if (Window.ActiveControl = ActiveDBGrid) then DBGridExit(Window.ActiveControl);

  Include(FrameState, tsActive);
end;

procedure TFSession.UMPostBuilderQueryChange(var Message: TMessage);
begin
  FQueryBuilderEditorPageControlCheckStyle();
end;

procedure TFSession.UMPostShow(var Message: TMessage);
var
  Node: TTreeNode;
  URI: TUURI;
begin
  PNavigator.Visible := Session.Account.Desktop.NavigatorVisible;
  PExplorer.Visible := Session.Account.Desktop.ExplorerVisible;
  PSQLHistory.Visible := Session.Account.Desktop.SQLHistoryVisible;
  PSideBar.Visible := PNavigator.Visible or PExplorer.Visible or PSQLHistory.Visible; SSideBar.Visible := PSideBar.Visible;

  if (PExplorer.Visible) then
    CreateExplorer()
  else if (PSQLHistory.Visible) then
    FSQLHistoryRefresh(nil);

  PSideBar.Width := Session.Account.Desktop.SidebarWitdth;
  PFiles.Height := PSideBar.ClientHeight - Session.Account.Desktop.FoldersHeight - SExplorer.Height;

  FSQLEditorSynMemo.Options := FSQLEditorSynMemo.Options + [eoScrollPastEol];  // Speed up the performance
  FSQLEditorSynMemo.Text := Session.Account.Desktop.EditorContent[ttEditor];
  if (Length(FSQLEditorSynMemo.Lines.Text) < LargeSQLScriptSize) then
    FSQLEditorSynMemo.Options := FSQLEditorSynMemo.Options - [eoScrollPastEol];  // Slow down the performance on large content
  PResult.Height := Session.Account.Desktop.DataHeight;
  PResultHeight := PResult.Height;
  PBlob.Height := Session.Account.Desktop.BlobHeight;

  PLog.Height := Session.Account.Desktop.LogHeight;
  PLog.Visible := Session.Account.Desktop.LogVisible; SLog.Visible := PLog.Visible;

  aVBlobText.Checked := True;

  FQueryBuilderEditorPageControlCheckStyle();

  FormResize(nil);

  Perform(UM_ACTIVATEFRAME, 0, 0);


  Node := FNavigator.Items.Add(nil, Session.Caption);
  Node.Data := Session;
  Node.ImageIndex := iiServer;

  if (Assigned(Session.Processes)) then
  begin
    Node := FNavigator.Items.AddChild(FNavigator.Items.GetFirstNode(), Preferences.LoadStr(24));
    Node.Data := Session.Processes;
    Node.ImageIndex := iiProcesses;
  end;
  if (Assigned(Session.Users)) then
  begin
    Node := FNavigator.Items.AddChild(FNavigator.Items.GetFirstNode(), Preferences.LoadStr(561));
    Node.Data := Session.Users;
    Node.ImageIndex := iiUsers;
  end;
  if (Assigned(Session.Variables)) then
  begin
    Node := FNavigator.Items.AddChild(FNavigator.Items.GetFirstNode(), Preferences.LoadStr(22));
    Node.Data := Session.Variables;
    Node.ImageIndex := iiVariables;
  end;

  FNavigator.Items.GetFirstNode().Expand(False);

  FNavigatorInitialize(nil);


  if (Copy(Param, 1, 8) = 'mysql://') then
    try
      Address := Param;
    except
      Address := Session.Account.Desktop.Address;
    end
  else if (Param <> '') then
  begin
    URI := TUURI.Create(Session.Account.Desktop.Address);
    URI.Param['view'] := 'editor';
    URI.Table := '';
    URI.Param['system'] := Null;
    URI.Param['filter'] := Null;
    URI.Param['search'] := Null;
    URI.Param['offset'] := Null;
    URI.Param['objecttype'] := Null;
    URI.Param['object'] := Null;
    URI.Param['offset'] := Null;
    URI.Param['file'] := EscapeURL(Param);
    URI.Param['cp'] := Null;
    Address := URI.Address;
    URI.Free();
  end
  else
    Address := Session.Account.Desktop.Address;
  PHeaderCheckElements(nil);
end;

procedure TFSession.UMStausBarRefresh(var Message: TMessage);
begin
  StatusBarRefresh();
end;

procedure TFSession.UMSynCompletionTime(var Message: TMessage);
begin
  // SetTimer must be called after SynMemoStatusChange
  SetTimer(Handle, Message.WParam, Message.LParam, nil);
end;

procedure TFSession.UMWantedSynchronize(var Message: TMessage);
begin
  if (not (csDestroying in ComponentState)) then
    Wanted.Synchronize();
end;

function TFSession.UpdateAfterAddressChanged(): Boolean;
var
  Database: TSDatabase;
  I: Integer;
  List: TList;
begin
  Result := False;

  case (View) of
    vObjects:
      case (SelectedImageIndex) of
        iiServer:
          if (Session.Connection.MySQLVersion < 50002) then
          begin
            List := TList.Create();
            for I := 0 to Session.Databases.Count - 1 do
              List.Add(Session.Databases[I]);
            Result := not Session.Update(List, True);
            List.Free();
          end
          else
            Session.Update(nil, True);
        iiDatabase,
        iiSystemDatabase:
          begin
            Database := TSDatabase(FNavigator.Selected.Data);
            if (not Database.Tables.Update()) then
              Wanted.Update := UpdateAfterAddressChanged;
            List := TList.Create();
            if (Session.Connection.MySQLVersion < 50002) then
              List.Add(Database);
            Result := not Session.Update(List, True);
            List.Free();
          end;
        iiBaseTable,
        iiView,
        iiSystemView:
          TSTable(FNavigator.Selected.Data).Update();
      end;
    vBrowser:
      if ((TObject(FNavigator.Selected.Data) is TSView and not TSView(FNavigator.Selected.Data).Update())
        or (TObject(FNavigator.Selected.Data) is TSBaseTable and not TSBaseTable(FNavigator.Selected.Data).Update(True))) then
        Wanted.Update := UpdateAfterAddressChanged
      else if (not TSTable(FNavigator.Selected.Data).DataSet.Active) then
        TableOpen(nil);
    vIDE:
      begin
        // Debug 2016-12-26
        if (not (TObject(FNavigator.Selected.Data) is TSDBObject)) then
          try
            raise ERangeError.Create('ClassType: ' + TObject(FNavigator.Selected.Data).ClassName + #13#10
              + 'ImageIndex: ' + IntToStr(FNavigator.Selected.ImageIndex) + #13#10
              + 'Text: ' + FNavigator.Selected.Text + #13#10
              + 'Address: ' + Address);
          except
            raise ERangeError.Create('ImageIndex: ' + IntToStr(FNavigator.Selected.ImageIndex) + #13#10
              + 'Text: ' + FNavigator.Selected.Text + #13#10
              + 'Address: ' + Address);
          end;
        TSDBObject(FNavigator.Selected.Data).Update();
      end;
    vDiagram:
      if (not Assigned(ActiveWorkbench) and Assigned(FNavigator.Selected)) then
      begin
        Desktop(TSDatabase(FNavigator.Selected.Data)).CreateWorkbench();
        ActiveWorkbench := GetActiveWorkbench();
        if (FileExists(Session.Account.DataPath + ActiveWorkbench.Database.Name + PathDelim + 'Diagram.xml')) then
          ActiveWorkbench.LoadFromFile(Session.Account.DataPath + ActiveWorkbench.Database.Name + PathDelim + 'Diagram.xml');
      end;
  end;
end;

procedure TFSession.ViewChanged(Sender: TObject);
begin
  tbObjects.Down := MainAction('aVObjects').Checked;
  tbBrowser.Down := MainAction('aVBrowser').Checked;
  tbIDE.Down := MainAction('aVIDE').Checked;
  tbBuilder.Down := MainAction('aVBuilder').Checked;
  tbDiagram.Down := MainAction('aVDiagram').Checked;
  tbEditor.Down := MainAction('aVSQLEditor').Checked;
  tbEditor2.Down := MainAction('aVSQLEditor2').Checked;
  tbEditor3.Down := MainAction('aVSQLEditor3').Checked;

  if (tbObjects.Down) then
    FView := vObjects
  else if (tbBrowser.Down) then
    FView := vBrowser
  else if (tbIDE.Down) then
    FView := vIDE
  else if (tbBuilder.Down) then
    FView := vBuilder
  else if (tbDiagram.Down) then
    FView := vDiagram
  else if (tbEditor.Down) then
    FView := vEditor
  else if (tbEditor2.Down) then
    FView := vEditor2
  else if (tbEditor3.Down) then
    FView := vEditor3
  else if (Assigned(ObjectSearch)) then
    FView := vObjectSearch
  else
    FView := vObjects;

  if ((View <> vObjectSearch) and Assigned(ObjectSearch)) then
  begin
    if (Assigned(ObjectSearchListView)) then
    begin
      ObjectSearchListView.Free();
      ObjectSearchListView := nil;
    end;

    ObjectSearch.Free();
    ObjectSearch := nil;
    FObjectSearch.Text := '';
  end;
end;

function TFSession.ViewToParam(const AView: TView): Variant;
begin
  case (AView) of
    vBrowser: Result := 'browser';
    vIDE: Result := 'ide';
    vBuilder: Result := 'builder';
    vDiagram: Result := 'diagram';
    vEditor: Result := 'editor';
    vEditor2: Result := 'editor2';
    vEditor3: Result := 'editor3';
    else Result := Null;
  end;
end;

procedure TFSession.WMActivate(var Message: TWMActivate);
begin
  if (Message.Active = WA_INACTIVE) then
    if (Assigned(PObjectSearch)) then
    begin
      PObjectSearch.Hide();
      Window.ActiveControl := nil;
    end;
end;

procedure TFSession.WMNotify(var Message: TWMNotify);
begin
  case (Message.NMHdr^.code) of
    TVN_BEGINLABELEDIT,
    LVN_BEGINLABELEDIT: BeginEditLabel(Window.ActiveControl);
    TVN_ENDLABELEDIT,
    LVN_ENDLABELEDIT: EndEditLabel(Window.ActiveControl);
    LVN_ITEMCHANGING: NMListView := PNMListView(Message.NMHdr);
  end;

  inherited;

  NMListView := nil;
end;

procedure TFSession.WMParentNotify(var Message: TWMParentNotify);
var
  ClientPoint: TPoint;
  GridPoint: TPoint;
  GridCoord: TGridCoord;
  ScreenPoint: TPoint;
begin
  ClientPoint := Point(Message.XPos, Message.YPos);
  ScreenPoint := ClientToScreen(ClientPoint);

  if ((Message.Event = WM_RBUTTONDOWN)
    and (ControlAtPos(ClientPoint, False, True) = PContent)
    and (PContent.ControlAtPos(PContent.ScreenToClient(ScreenPoint), False, True) = PResult)
    and (PResult.ControlAtPos(PResult.ScreenToClient(ScreenPoint), False, True) = ActiveDBGrid.Parent)
    and (ActiveDBGrid.Parent.ControlAtPos(ActiveDBGrid.Parent.ScreenToClient(ScreenPoint), False, True) = ActiveDBGrid)) then
  begin
    Window.ActiveControl := ActiveDBGrid;
    GridPoint := ActiveDBGrid.Parent.ScreenToClient(ScreenPoint);
    GridCoord := ActiveDBGrid.MouseCoord(GridPoint.X, GridPoint.Y);
    if ((GridCoord.X >= 0) and (GridCoord.Y = 0)) then
      ActiveDBGrid.PopupMenu := MGridHeader
    else
      ActiveDBGrid.PopupMenu := MGrid;
  end;

  inherited;
end;

procedure TFSession.WMTimer(var Message: TWMTimer);
begin
  case (Message.TimerID) of
    tiNavigator:
      begin
        KillTimer(Handle, Message.TimerID);
        if (Window.Active) then
          FNavigatorChange2(FNavigator, FNavigator.Selected)
        else
          FNavigatorNodeAfterActivate := FNavigator.Selected;
      end;
    tiStatusBar:
      begin
        KillTimer(Handle, Message.TimerID);
        StatusBar.Panels[sbMessage].Text := '';
        StatusBarRefresh();
      end;
    tiShowSynCompletion:
      begin
        KillTimer(Handle, Message.TimerID);
        if (Window.Active and (View in [vEditor, vEditor2, vEditor3])) then
        begin
          SynCompletion.Form.CurrentEditor := ActiveSynMemo;
          SynCompletion.ActivateCompletion();
          PostMessage(Handle, UM_SYNCOMPLETION_TIMER, tiHideSynCompletion, 5000);
        end;
      end;
    tiHideSynCompletion:
      begin
        KillTimer(Handle, Message.TimerID);
        SynCompletion.CancelCompletion();
      end;
  end;
end;

procedure TFSession.WorkbenchAddTable(Sender: TObject);
var
  BaseTable: TSBaseTable;
  MenuItem: TMenuItem;
  Point: TPoint;
begin
  if (Sender is TMenuItem) then
  begin
    MenuItem := TMenuItem(Sender);

    if ((MenuItem.GetParentMenu() is TPopupMenu) and (TObject(MenuItem.Tag) is TSTable)) then
    begin
      Point := ActiveWorkbench.ScreenToClient(TPopupMenu(MenuItem.GetParentMenu()).PopupPoint);
      BaseTable := TSBaseTable(TMenuItem(Sender).Tag);

      ActiveWorkbench.AddExistingTable(Point.X, Point.Y, BaseTable);
    end;
  end;
end;

procedure TFSession.WorkbenchChange(Sender: TObject; Control: TWControl);
var
  aEPasteEnabled: Boolean;
  ClipboardData: HGLOBAL;
  Database: TSDatabase;
  DatabaseName: string;
  Index: Integer;
  S: string;
  AccountName: string;
  Table: TSBaseTable;
  TableName: string;
  Values: TStringList;
begin
  if (not Clipboard.HasFormat(CF_MYSQLTABLE)) then
    aEPasteEnabled := False
  else if (not OpenClipboard(Handle)) then
    aEPasteEnabled := False
  else
  begin
    try
      ClipboardData := GetClipboardData(CF_MYSQLTABLE);
      SetString(S, PChar(GlobalLock(ClipboardData)), GlobalSize(ClipboardData) div SizeOf(S[1]));
      GlobalUnlock(ClipboardData);
    finally
      CloseClipboard();
    end;

    Values := TStringList.Create();
    Values.Text := ReplaceStr(Trim(S), ',', #13#10);

    aEPasteEnabled := Values.Count = 1;

    if (aEPasteEnabled) then
    begin
      S := Values[0];

      if (Pos('.', S) = 0) then
        AccountName := ''
      else
      begin
        Index := Pos('.', S);
        while ((Index > 1) and (S[Index - 1] = '\')) do
          Inc(Index, Pos('.', Copy(S, Index + 1, Length(S) - Index)));
        AccountName := ReplaceStr(Copy(S, 1, Index - 1), '\.', '.');
        Delete(S, 1, Index);
      end;
      if (Pos('.', S) = 0) then
        DatabaseName := ''
      else
      begin
        DatabaseName := Copy(S, 1, Pos('.', S) - 1);
        Delete(S, 1, Length(DatabaseName) + 1);
      end;
      if (Pos('.', S) = 0) then
        TableName := S
      else
        TableName := '';

      Table := Session.DatabaseByName(SelectedDatabase).BaseTableByName(TableName);

      aEPasteEnabled := (AccountName = Session.Account.Name) and (DatabaseName = SelectedDatabase)
        and Assigned(Table) and not Assigned(ActiveWorkbench.TableByCaption(Table.Name));
    end;

    Values.Free();
  end;

  Database := Session.DatabaseByName(SelectedDatabase);

  MainAction('aFExportSQL').Enabled := not Assigned(Control) or (Control is TWTable);
  MainAction('aFExportText').Enabled := not Assigned(Control) or (Control is TWTable);
  MainAction('aFExportExcel').Enabled := not Assigned(Control) or (Control is TWTable);
  MainAction('aFExportAccess').Enabled := not Assigned(Control) or (Control is TWTable);
  MainAction('aFExportODBC').Enabled := not Assigned(Control) or (Control is TWTable);
  MainAction('aFExportXML').Enabled := not Assigned(Control) or (Control is TWTable);
  MainAction('aFExportHTML').Enabled := not Assigned(Control) or (Control is TWTable);
  MainAction('aFExportPDF').Enabled := not Assigned(Control) or (Control is TWTable);
  MainAction('aFExportBitmap').Enabled := not Assigned(Control) and Assigned(ActiveWorkbench) and (ActiveWorkbench.ControlCount > 0);
  MainAction('aECopy').Enabled := Assigned(Control) and (not (Control is TWLink) or (Control is TWForeignKey));
  MainAction('aEPaste').Enabled := aEPasteEnabled;
  MainAction('aDCreateTable').Enabled := not Assigned(Control) or (Control is TWSection);
  mwDCreateTable.Enabled := MainAction('aDCreateTable').Enabled;
  MainAction('aDCreateKey').Enabled := Control is TWTable;
  MainAction('aDCreateField').Enabled := Control is TWTable;
  MainAction('aDCreateForeignKey').Enabled := (Control is TWTable);
  mwCreateSection.Enabled := not Assigned(Control);
  mwCreateLink.Enabled := Control is TWTable;
  MainAction('aDCreateTrigger').Enabled := (Control is TWTable) and Assigned(TWTable(Control).BaseTable) and Assigned(Database.Triggers);
  MainAction('aDDeleteTable').Enabled := Control is TWTable;
  MainAction('aDDeleteForeignKey').Enabled := (Control is TWForeignKey);
  MainAction('aDEmpty').Enabled := Control is TWTable;
  mwDProperties.Enabled := Assigned(Control) and (not (Control is TWLink) or (Control is TWForeignKey));

  aDDelete.Enabled := MainAction('aDDeleteTable').Enabled or MainAction('aDDeleteForeignKey').Enabled;
end;

procedure TFSession.WorkbenchCursorMove(Sender: TObject; X, Y: Integer);
begin
  StatusBar.Panels[sbNavigation].Text := IntToStr(X) + ':' + IntToStr(Y);
end;

procedure TFSession.WorkbenchDragDrop(Sender, Source: TObject; X, Y: Integer);
begin
  if (Assigned(MouseDownNode) and (MouseDownNode.TreeView = Source) and (Source = FNavigator)
    and (FNavigator.Selected = MouseDownNode.Parent) and (MouseDownNode.ImageIndex = iiBaseTable)) then
  begin
    ActiveWorkbench.AddExistingTable(X, Y, TSBaseTable(MouseDownNode.Data));
    Window.ActiveControl := ActiveWorkbench;
  end;
end;

procedure TFSession.WorkbenchDragOver(Sender, Source: TObject; X, Y: Integer;
  State: TDragState; var Accept: Boolean);
var
  BaseTable: TSBaseTable;
begin
  Accept := False;

  if (Assigned(MouseDownNode) and (MouseDownNode.TreeView = Source) and (Source = FNavigator)
    and (FNavigator.Selected = MouseDownNode.Parent) and (MouseDownNode.ImageIndex = iiBaseTable)) then
  begin
    BaseTable := TSBaseTable(MouseDownNode.Data);
    Accept := Assigned(BaseTable) and not Assigned(ActiveWorkbench.TableByBaseTable(BaseTable));
  end;
end;

procedure TFSession.WorkbenchEmptyExecute(Sender: TObject);
var
  BaseTable: TSBaseTable;
begin
  if (ActiveWorkbench.Selected is TWTable) then
  begin
    BaseTable := TWTable(ActiveWorkbench.Selected).BaseTable;

    if (MsgBox(Preferences.LoadStr(375, BaseTable.Name), Preferences.LoadStr(101), MB_YESNOCANCEL + MB_ICONQUESTION) = IDYES) then
      BaseTable.Empty();
  end;
end;

procedure TFSession.WorkbenchEnter(Sender: TObject);
begin
  if (Sender is TWWorkbench) then
  begin
    MainAction('aDEmpty').OnExecute := WorkbenchEmptyExecute;

    WorkbenchChange(Sender, TWWorkbench(Sender).Selected);
  end;
end;

procedure TFSession.WorkbenchExit(Sender: TObject);
begin
  MainAction('aFExportSQL').Enabled := False;
  MainAction('aFExportText').Enabled := False;
  MainAction('aFExportExcel').Enabled := False;
  MainAction('aFExportAccess').Enabled := False;
  MainAction('aFExportODBC').Enabled := False;
  MainAction('aFExportXML').Enabled := False;
  MainAction('aFExportHTML').Enabled := False;
  MainAction('aFExportPDF').Enabled := False;
  MainAction('aFExportBitmap').Enabled := False;
  MainAction('aECopy').Enabled := False;
  MainAction('aDCreateTable').Enabled := False;
  MainAction('aDCreateView').Enabled := False;
  MainAction('aDCreateProcedure').Enabled := False;
  MainAction('aDCreateFunction').Enabled := False;
  MainAction('aDCreateEvent').Enabled := False;
  MainAction('aDCreateKey').Enabled := False;
  MainAction('aDCreateField').Enabled := False;
  MainAction('aDCreateForeignKey').Enabled := False;
  MainAction('aDCreateTrigger').Enabled := False;
  MainAction('aDDeleteTable').Enabled := False;
  MainAction('aDDeleteForeignKey').Enabled := False;
  mwCreateSection.Enabled := False;
  mwCreateLink.Enabled := False;
  MainAction('aDEmpty').Enabled := False;
end;

procedure TFSession.WorkbenchPasteExecute(Sender: TObject);
var
  aEPasteEnabled: Boolean;
  ClipboardData: HGLOBAL;
  DatabaseName: string;
  Index: Integer;
  MenuItem: TMenuItem;
  P: TPoint;
  S: string;
  AccountName: string;
  BaseTable: TSBaseTable;
  TableName: string;
  Values: TStringList;
begin
  Wanted.Clear();

  if (not Clipboard.HasFormat(CF_MYSQLTABLE)) then
    MessageBeep(MB_ICONERROR)
  else if (not OpenClipboard(Handle)) then
    MessageBeep(MB_ICONERROR)
  else
  begin
    try
      ClipboardData := GetClipboardData(CF_MYSQLTABLE);
      SetString(S, PChar(GlobalLock(ClipboardData)), GlobalSize(ClipboardData) div SizeOf(S[1]));
      GlobalUnlock(ClipboardData);
    finally
      CloseClipboard();
    end;

    Values := TStringList.Create();
    Values.Text := ReplaceStr(Trim(S), ',', #13#10);

    aEPasteEnabled := Values.Count = 1;

    if (aEPasteEnabled) then
    begin
      S := Values[0];

      if (Pos('.', S) = 0) then
        AccountName := ''
      else
      begin
        Index := Pos('.', S);
        while ((Index > 1) and (S[Index - 1] = '\')) do
          Inc(Index, Pos('.', Copy(S, Index + 1, Length(S) - Index)));
        AccountName := ReplaceStr(Copy(S, 1, Index - 1), '\.', '.');
        Delete(S, 1, Index);
      end;
      if (Pos('.', S) = 0) then
        DatabaseName := ''
      else
      begin
        DatabaseName := Copy(S, 1, Pos('.', S) - 1);
        Delete(S, 1, Length(DatabaseName) + 1);
      end;
      if (Pos('.', S) = 0) then
        TableName := S
      else
        TableName := '';

      P := Point(0, 0);
      if (Sender is TMenuItem) then
      begin
        MenuItem := TMenuItem(Sender);

        if ((MenuItem.GetParentMenu() is TPopupMenu)) then
          P := ActiveWorkbench.ScreenToClient(TPopupMenu(MenuItem.GetParentMenu()).PopupPoint);
      end;
      BaseTable := Session.DatabaseByName(SelectedDatabase).BaseTableByName(TableName);

      ActiveWorkbench.AddExistingTable(P.X, P.Y, BaseTable);
    end;

    Values.Free();
  end;
end;

function TFSession.WorkbenchValidateControl(Sender: TObject; Control: TWControl): Boolean;
var
  ChildTable: TSBaseTable;
  ParentTable: TSBaseTable;
begin
  if (Control is TWTable) then
  begin
    if (Assigned(TWTable(Control).BaseTable)) then
    begin
      Session.Connection.BeginSynchron();
      Result := TWTable(Control).BaseTable.Update();
      Session.Connection.EndSynchron();
    end
    else
    begin
      DTable.Database := Control.Workbench.Database;
      DTable.Table := nil;
      Result := DTable.Execute();
      if (Result) then
        Wanted.Update := Session.Update;
    end
  end
  else if (Control is TWForeignKey) then
  begin
    ChildTable := TWForeignKey(Control).ChildTable.BaseTable;
    ParentTable := TWForeignKey(Control).ParentTable.BaseTable;

    if (Assigned(ChildTable) and Assigned(ParentTable) and not Assigned(TWForeignKey(Control).BaseForeignKey)) then
    begin
      DForeignKey.Table := ChildTable;
      DForeignKey.ParentTable := ParentTable;
      DForeignKey.ForeignKey := nil;
      DForeignKey.ModifyTableOnly := False;
      Result := DForeignKey.Execute();
      if (Result) then
        Wanted.Update := Session.Update;
    end
    else
      Result := False;
  end
  else if (Control is TWLink) then
  begin
    ChildTable := TWForeignKey(Control).ChildTable.BaseTable;
    ParentTable := TWForeignKey(Control).ParentTable.BaseTable;

    Result := Assigned(ChildTable) and Assigned(ParentTable);
  end
  else
    Result := False;
end;

end.
