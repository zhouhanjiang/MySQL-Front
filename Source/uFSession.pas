﻿unit uFSession;

interface {********************************************************************}

uses
  Forms, Windows, Messages, SysUtils, Classes, Graphics, Controls, ActiveX,
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
  uBase, uDExport, uDImport, uCWorkbench, uPObjectSearch, uPDBGridFilter;

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
  TFSession = class (TFrame, IDropSource, IDropTarget)
    ActionList: TActionList;
    aDCreate: TAction;
    aDDelete: TAction;
    aDNext: TAction;
    aDPrev: TAction;
    aEClearAll: TAction;
    aPCollapse: TAction;
    aPExpand: TAction;
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
    FListView: TListView_Ext;
    FSQLEditorSearch: TSynEditSearch;
    FSQLEditorSynMemo: TSynMemo;
    FSQLHistory: TTreeView_Ext;
    FText: TRichEdit;
    FUDLimit: TUpDown;
    FUDOffset: TUpDown;
    ghmCopy: TMenuItem;
    ghmGoto: TMenuItem;
    ghmSelectAll: TMenuItem;
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
    miNFavoriteAdd: TMenuItem;
    miNFavoriteRemove: TMenuItem;
    miNFavoriteOpen: TMenuItem;
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
    mtObjectSearch: TMenuItem;
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
    N3: TMenuItem;
    N30: TMenuItem;
    N31: TMenuItem;
    N32: TMenuItem;
    N33: TMenuItem;
    N34: TMenuItem;
    N4: TMenuItem;
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
    SBlob: TSplitter_Ext;
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
    N5: TMenuItem;
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
    procedure DBGridHeaderMouseMove(Sender: TObject; Shift: TShiftState; X,
      Y: Integer);
    procedure DBGridHeaderSplitButton(DBGrid: TMySQLDBGrid; Column: TColumn; Shift: TShiftState);
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
    procedure FNavigatorKeyUp(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure FNavigatorMouseMove(Sender: TObject; Shift: TShiftState; X,
      Y: Integer);
    procedure FObjectSearchChange(Sender: TObject);
    procedure FObjectSearchExit(Sender: TObject);
    procedure FObjectSearchKeyPress(Sender: TObject; var Key: Char);
    procedure FObjectSearchMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
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
    procedure ghmSelectAllClick(Sender: TObject);
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
    procedure ListViewHeaderUpdate(Sender: TObject);
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
    procedure miHStatementIntoSQLEditorClick(Sender: TObject);
    procedure miNFavoriteAddClick(Sender: TObject);
    procedure miNFavoriteRemoveClick(Sender: TObject);
    procedure miNFavoriteOpenClick(Sender: TObject);
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
    procedure PDBGridResize(Sender: TObject);
    procedure PHeaderCheckElements(Sender: TObject);
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
  type
    TClassIndex = (ciUnknown, ciSession, ciDatabase, ciSystemDatabase, ciBaseTable, ciView, ciSystemView, ciProcedure, ciFunction, ciTrigger, ciEvent, ciKey, ciBaseField, ciViewField, ciForeignKey, ciProcesses, ciProcess, ciUsers, ciUser, ciVariables, ciVariable, ciObjectSearch, ciQuickAccess);
    TListViewSortRec = record Kind: TPAccount.TDesktop.TListViewKind; ColumnIndex: Integer; Order: Integer; end;
    TListViewSortData = array [Low(TPAccount.TDesktop.TListViewKind) .. High(TPAccount.TDesktop.TListViewKind)] of TListViewSortRec;
    TNewLineFormat = (nlWindows, nlUnix, nlMacintosh);
    TTabState = set of (fsLoading, fsActive);
    TView = (vObjects, vBrowser, vIDE, vBuilder, vDiagram, vEditor, vEditor2, vEditor3, vObjectSearch);
    TToolBarData = record
      Caption: string;
      tbPropertiesAction: TBasicAction;
      View: TView;
    end;

    TSQLEditor = class
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
      procedure etItemResort(const AFilter: string);
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

    TDBGridDropData = class(TInterfacedObject, IDataObject, IEnumFORMATETC)
    private
      FDBGrid: TMySQLDBGrid;
      EnumFormatEtcIndex: Integer;
    protected
      function Clone(out Enum: IEnumFormatEtc): HResult; stdcall;
      function DAdvise(const formatetc: TFormatEtc; advf: Longint;
        const advSink: IAdviseSink; out dwConnection: Longint): HResult; stdcall;
      function DUnadvise(dwConnection: Longint): HResult; stdcall;
      function EnumDAdvise(out enumAdvise: IEnumStatData): HResult;
        stdcall;
      function EnumFormatEtc(dwDirection: Longint; out enumFormatEtc:
        IEnumFormatEtc): HResult; stdcall;
      function GetCanonicalFormatEtc(const formatetc: TFormatEtc;
        out formatetcOut: TFormatEtc): HResult; stdcall;
      function GetData(const formatetcIn: TFormatEtc; out medium: TStgMedium):
        HResult; stdcall;
      function GetDataHere(const formatetc: TFormatEtc; out medium: TStgMedium):
        HResult; stdcall;
      function Next(celt: Longint; out elt; pceltFetched: PLongint): HResult; stdcall;
      function QueryGetData(const formatetc: TFormatEtc): HResult;
        stdcall;
      function Reset(): HResult; stdcall;
      function SetData(const formatetc: TFormatEtc; var medium: TStgMedium;
        fRelease: BOOL): HResult; stdcall;
      function Skip(celt: Longint): HResult; stdcall;
    public
      constructor Create(const ADBGrid: TMySQLDBGrid);
      property DBGrid: TMySQLDBGrid read FDBGrid;
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
    FCurrentAddress: string;
    FFiles: TJamShellList;
    FFolders: TJamShellTree;
    FHTML: TWebBrowser;
    FilterMRU: TPPreferences.TMRUList;
    FNavigatorDragDisabled: Boolean;
    FNavigatorHotTrackDisabled: Boolean;
    FNavigatorIgnoreChange: Boolean;
    FNavigatorKeyDownNode: TTreeNode;
    FNavigatorMenuNode: TTreeNode;
    FNavigatorNodeAfterActivate: TTreeNode;
    FNavigatorNodeToExpand: TTreeNode;
    FrameState: TTabState;
    FSQLEditorSynMemo2: TSynMemo;
    FSQLEditorSynMemo3: TSynMemo;
    FSQLHistoryMenuNode: TTreeNode;
    GIFImage: TGIFImage;
    JPEGImage: TJPEGImage;
    LastFNavigatorSelected: TTreeNode;
    LastSelectedDatabase: string;
    LastSelectedObjectIDE: string;
    LastSelectedTable: string;
    LastTableView: TView;
    LeftMousePressed: Boolean;
    ListViewSortData: TListViewSortData;
    MGridHeaderColumn: TColumn;
    MouseDownNode: TTreeNode;
    MovingToAddress: Boolean;
    NewLineFormat: TNewLineFormat;
    NMListView: PNMListView;
    ObjectSearch: TSItemSearch;
    ObjectSearchListView: TListView;
    OldAddress: string;
    PanelMouseDownPoint: TPoint;
    Param: string;
    PasteMode: Boolean;
    PDBGridFilter: TPDBGridFilter;
    PNGImage: TPNGImage;
    PObjectSearch: TPObjectSearch;
    PResultHeight: Integer;
    ProcessesListView: TListView;
    QuickAccessListView: TListView;
    SBlobDebug: TSplitter;
    ServerListView: TListView;
    ShellLink: TJamShellLink;
    SplitColor: TColor;
    SQLEditor: TSQLEditor;
    SQLEditor2: TSQLEditor;
    SQLEditor3: TSQLEditor;
    SynMemoBeforeDrag: record
      SelLength: Integer;
      SelStart: Integer;
    end;
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
    function AddressByData(const Data: TCustomData): string;
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
    function ClassIndexByAddress(const Address: string): TClassIndex;
    function ClassIndexByData(const Data: TCustomData): TClassIndex;
    function ChangeCurrentAddress(const AAddress: string): Boolean;
    function ColumnWidthKindByListView(const ListView: TListView): TPAccount.TDesktop.TListViewKind;
    procedure CMSysFontChanged(var Msg: TMessage); message CM_SYSFONTCHANGED;
    function CreateDesktop(const CObject: TSObject): TSObject.TDesktop;
    procedure CreateExplorer();
    function CreateDBGrid(const PDBGrid: TPanel_Ext; const DataSource: TDataSource): TMySQLDBGrid;
    function CreateListView(const Data: TCustomData): TListView;
    function CreatePDBGrid(): TPanel_Ext;
    function CreateSynMemo(SObject: TSObject): TSynMemo;
    function CreateTCResult(const PDBGrid: TPanel_Ext): TTabControl;
    function CreateWorkbench(const ADatabase: TSDatabase): TWWorkbench;
    function DataByAddress(const Address: string): TCustomData;
    procedure DataSetAfterCancel(DataSet: TDataSet);
    procedure DataSetAfterClose(DataSet: TDataSet);
    procedure DataSetAfterOpen(const DBGrid: TMySQLDBGrid; const DataSet: TDataSet);
    procedure DataSetAfterPost(DataSet: TDataSet);
    procedure DataSetAfterScroll(DataSet: TDataSet);
    procedure DataSetBeforeCancel(DataSet: TDataSet);
    procedure DataSetBeforePost(DataSet: TDataSet);
    procedure DBGridDragOver(Sender, Source: TObject; X, Y: Integer;
      State: TDragState; var Accept: Boolean);
    function DBGridDrop(const DBGrid: TMySQLDBGrid; const dataObj: IDataObject;
      grfKeyState: Longint; pt: TPoint; var dwEffect: Longint): HResult;
    procedure DBGridInitialize(const DBGrid: TMySQLDBGrid);
    function Desktop(const Database: TSDatabase): TDatabaseDesktop; overload; inline;
    function Desktop(const Event: TSEvent): TEventDesktop; overload; inline;
    function Desktop(const Routine: TSRoutine): TRoutineDesktop; overload; inline;
    function Desktop(const Table: TSTable): TTableDesktop; overload; inline;
    function Desktop(const Trigger: TSTrigger): TTriggerDesktop; overload; inline;
    function Desktop(const View: TSView): TViewDesktop; overload; inline;
    function Dragging(const Sender: TObject): Boolean;
    function EditDrop(const Edit: TEdit; const dataObj: IDataObject;
      grfKeyState: Longint; pt: TPoint; var dwEffect: Longint): HResult;
    procedure EndEditLabel(Sender: TObject);
    procedure FavoritesAdd(const Objects: TList);
    procedure FavoritesEvent(const Favorites: TPAccount.TFavorites);
    function FFilterDrop(const dataObj: IDataObject; grfKeyState: Longint;
      pt: TPoint; var dwEffect: Longint): HResult;
    procedure FHexEditorShow(Sender: TObject);
    procedure FHTMLHide(Sender: TObject);
    procedure FHTMLShow(Sender: TObject);
    procedure FieldSetText(Sender: TField; const Text: string);
    procedure FImageShow(Sender: TObject);
    procedure FLogUpdate();
    procedure FNavigatorChanged(Sender: TObject; const Node: TTreeNode);
    procedure FNavigatorEmptyExecute(Sender: TObject);
    procedure FNavigatorInitialize(Sender: TObject);
    function FNavigatorNodeByAddress(const Address: string): TTreeNode;
    procedure FNavigatorRemoveFavorites();
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
    function GetCurrentClassIndex(): TClassIndex; inline;
    function GetCurrentData(): TCustomData; inline;
    function GetEditorField(): TField;
    function GetFocusedSItem(): TSItem;
    function GetMenuDatabase(): TSDatabase;
    function GetPath(): TFileName; inline;
    function GetSelectedDatabase(): string;
    function GetSQLEditors(View: TView): TSQLEditor;
    function GetView(): TView;
    function GetWindow(): TForm_Ext;
    procedure gmFilterClearClick(Sender: TObject);
    procedure gmFilterIntoFilterClick(Sender: TObject);
    function GroupIDByImageIndex(const ImageIndex: Integer): Integer;
    function ImageIndexByData(const Data: TObject): Integer;
    function ImportError(const Details: TTool.TErrorDetails): TDataAction;
    procedure ListViewEmpty(Sender: TObject);
    procedure ListViewInitialize(const ListView: TListView);
    procedure ListViewUpdate(const Event: TSSession.TEvent; const ListView: TListView; const Data: TCustomData = nil);
    function ObjectSearchFinish(): Boolean;
    function ObjectSearchStep2(): Boolean;
    procedure OnConvertError(Sender: TObject; Text: string);
    function ParamToView(const AParam: Variant): TView;
    procedure PasteExecute(const Node: TTreeNode; const Objects: string);
    procedure PContentChange(Sender: TObject);
    function PostObject(Sender: TObject): Boolean;
    procedure PropertiesServerExecute(Sender: TObject);
    function QuickAccessStep1(): Boolean;
    function QuickAccessStep2(): Boolean;
    function RenameSItem(const SItem: TSItem; const NewName: string): Boolean;
    procedure SaveDiagram(Sender: TObject);
    procedure SaveSQLFile(Sender: TObject);
    procedure SendQuery(Sender: TObject; const SQL: string);
    procedure SetCurrentAddress(const AAddress: string); inline;
    procedure SetView(const AView: TView);
    procedure SetListViewGroupHeader(const ListView: TListView; const GroupID: Integer; const NewHeader: string);
    procedure SetPath(const APath: TFileName);
    procedure SQLError(DataSet: TDataSet; E: EDatabaseError; var Action: TDataAction);
    procedure SynMemoApplyPreferences(const SynMemo: TSynMemo);
    function SynMemoDrop(const SynMemo: TSynMemo; const dataObj: IDataObject;
      grfKeyState: Longint; pt: TPoint; var dwEffect: Longint): HResult;
    procedure TableOpen(Sender: TObject);
    procedure TCResultMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
    procedure UMActivateDBGrid(var Msg: TMessage); message UM_ACTIVATE_DBGRID;
    procedure UMActivateFText(var Msg: TMessage); message UM_ACTIVATEFTEXT;
    procedure UMChangePreferences(var Msg: TMessage); message UM_CHANGEPREFERENCES;
    procedure UMCloseFrame(var Msg: TMessage); message UM_CLOSE_FRAME;
    procedure UMCloseTabQuery(var Msg: TMessage); message UM_CLOSE_TAB_QUERY;
    procedure UMFrameActivate(var Msg: TMessage); message UM_ACTIVATEFRAME;
    procedure UMFrameDeactivate(var Msg: TMessage); message UM_DEACTIVATEFRAME;
    procedure UMPostBuilderQueryChange(var Msg: TMessage); message UM_POST_BUILDER_QUERY_CHANGE;
    procedure UMPostShow(var Msg: TMessage); message UM_POST_SHOW;
    procedure UMStausBarRefresh(var Msg: TMessage); message UM_STATUS_BAR_REFRESH;
    procedure UMSynCompletionTime(var Msg: TMessage); message UM_SYNCOMPLETION_TIMER;
    procedure UMWantedSynchronize(var Msg: TMessage); message UM_WANTED_SYNCHRONIZE;
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
    procedure WMNotify(var Msg: TWMNotify); message WM_NOTIFY;
    procedure WMParentNotify(var Msg: TWMParentNotify); message WM_PARENTNOTIFY;
    procedure WMTimer(var Msg: TWMTimer); message WM_TIMER;
    property EditorField: TField read GetEditorField;
    property FocusedSItem: TSItem read GetFocusedSItem;
    property MenuDatabase: TSDatabase read GetMenuDatabase;
    property SQLEditors[View: TView]: TSQLEditor read GetSQLEditors;
  protected
    procedure CreateParams(var Params: TCreateParams); override;
    function DragEnter(const dataObj: IDataObject; grfKeyState: Longint;
      pt: TPoint; var dwEffect: Longint): HResult; stdcall;
    function DragLeave(): HResult; stdcall;
    function DragOver(grfKeyState: Longint; pt: TPoint; var dwEffect: Longint): HResult; reintroduce; stdcall;
    function Drop(const dataObj: IDataObject; grfKeyState: Longint; pt: TPoint;
      var dwEffect: Longint): HResult; stdcall;
    function GiveFeedback(dwEffect: Longint): HResult; stdcall;
    function QueryContinueDrag(fEscapePressed: BOOL; grfKeyState: Longint): HResult; stdcall;
  public
    Session: TSSession;
    StatusBar: TStatusBar;
    ToolBarData: TToolBarData;
    TimeMonitor: TMySQLMonitor;
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
    property CurrentAddress: string read FCurrentAddress write SetCurrentAddress;
    property CurrentClassIndex: TClassIndex read GetCurrentClassIndex;
    property CurrentData: TCustomData read GetCurrentData;
    property Path: TFileName read GetPath write SetPath;
    property SelectedDatabase: string read GetSelectedDatabase;
    property View: TView read GetView write SetView;
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
uProfiling,
  uDeveloper,
  uDField, uDKey, uDTable, uDTables, uDVariable, uDDatabase, uDForeignKey,
  uDUser, uDQuickFilter, uWSQLHelp, uDTransfer, uDSearch, uDServer,
  uURI, uDView, uDRoutine, uDTrigger, uDStatement, uDEvent, uDPaste, uDSegment,
  uDConnecting, uDExecutingSQL;

const
  nlHost = 0;
  nlDatabase = 1;
  nlTable = 2;

const
  tiNavigator = 1;
  tiHideSynCompletion = 2;
  tiShowSynCompletion = 3;
  tiStatusBar = 4;

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
  giFrequentObjects = 14;
  giRecentObjects = 15;

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

const
  QuickAccessItemCount = 10;
  QuickAccessListEscaper = 'QuickAccessEscaper';

var
  ListViewUpdateCount: Integer; // Debug 2017-02-04

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

function QuickAccessFrequentObjectsCompare(List: TStringList; Index1, Index2: Integer): Integer;
var
  Int1: Integer;
  Int2: Integer;
  Name1: string;
  Name2: string;
  URI1: TUURI;
  URI2: TUURI;
begin
  Result := Sign(SysUtils.StrToInt(List.Values[List.Names[Index2]]) - SysUtils.StrToInt(List.Values[List.Names[Index1]]));

  if (Result = 0) then
  begin
    URI1 := TUURI.Create(ReplaceStr(List.Names[Index1], QuickAccessListEscaper, '='));
    URI2 := TUURI.Create(ReplaceStr(List.Names[Index2], QuickAccessListEscaper, '='));

    if (URI1.Param['object'] <> Null) then
      Name1 := URI1.Param['object']
    else if (URI1.Table <> '') then
      Name1 := URI1.Table
    else
      raise ERangeError.Create('Unknown name for: ' + URI1.Address);
    if (URI2.Param['object'] <> Null) then
      Name2 := URI2.Param['object']
    else if (URI2.Table <> '') then
      Name2 := URI2.Table
    else
      raise ERangeError.Create('Unknown name for: ' + URI2.Address);

    Result := lstrcmpi(PChar(Name1), PChar(Name2));

    if (Result = 0) then
    begin
      if (URI1.Param['objecttype'] = 'view') then
        Int1 := 0
      else if (URI1.Param['objecttype'] = 'procedure') then
        Int1 := 1
      else if (URI1.Param['objecttype'] = 'function') then
        Int1 := 1
      else if (URI1.Param['objecttype'] = 'event') then
        Int1 := 2
      else if (URI1.Param['objecttype'] = 'trigger') then
        Int1 := 2
      else if (URI1.Table <> '') then
        Int1 := 0
      else
        raise ERangeError.Create('Unknown Int for: ' + URI1.Address);
      if (URI2.Param['objecttype'] = 'view') then
        Int2 := 0
      else if (URI2.Param['objecttype'] = 'procedure') then
        Int2 := 1
      else if (URI2.Param['objecttype'] = 'function') then
        Int2 := 1
      else if (URI2.Param['objecttype'] = 'event') then
        Int2 := 2
      else if (URI2.Param['objecttype'] = 'trigger') then
        Int2 := 2
      else if (URI2.Table <> '') then
        Int2 := 0
      else
        raise ERangeError.Create('Unknown Int for: ' + URI2.Address);

      Result := Sign(Int1 - Int2);
    end;

    if (Result = 0) then
      Result := lstrcmpi(PChar(URI1.Database), PChar(URI2.Database));

    if (Result = 0) then
      Result := lstrcmpi(PChar(URI1.Address), PChar(URI2.Address));

    URI1.Free();
    URI2.Free();
  end;

  if (Result = 0) then
    Write;
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
      if ((Len > 0) and (SQL[Len] = ';')) then Dec(Len);
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
        URI := TUURI.Create(FSession.CurrentAddress);
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
  FListView := nil;
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

procedure TFSession.TTableDesktop.etItemResort(const AFilter: string);
var
  FiltersXML: IXMLNode;
  I: Integer;
begin
  if ((AFilter <> '') and ValidXMLText(AFilter)) then
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
    etItemResort(Table.DataSet.FilterSQL);
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

{ TFSession.TDBGridDropData.TIEnumFORMATETC ***********************************}

function TFSession.TDBGridDropData.Skip(celt: Longint): HResult;
begin
  Result := S_FALSE;
end;

{ TFSession.TDBGridDropData ***************************************************}

function TFSession.TDBGridDropData.Clone(out Enum: IEnumFormatEtc): HResult;
begin
  Enum := TDBGridDropData.Create(DBGrid);
  Result := S_OK;
end;

constructor TFSession.TDBGridDropData.Create(const ADBGrid: TMySQLDBGrid);
begin
  inherited Create();

  FDBGrid := ADBGrid;

  EnumFormatEtcIndex := 0;
end;

function TFSession.TDBGridDropData.DAdvise(const formatetc: TFormatEtc; advf: Longint;
  const advSink: IAdviseSink; out dwConnection: Longint): HResult;
begin
  Result := OLE_E_ADVISENOTSUPPORTED;
end;

function TFSession.TDBGridDropData.DUnadvise(dwConnection: Longint): HResult;
begin
  Result := OLE_E_ADVISENOTSUPPORTED;
end;

function TFSession.TDBGridDropData.EnumDAdvise(out enumAdvise: IEnumStatData): HResult;
begin
  Result := OLE_E_ADVISENOTSUPPORTED;
end;

function TFSession.TDBGridDropData.EnumFormatEtc(dwDirection: Longint;
  out enumFormatEtc: IEnumFormatEtc): HResult;
begin
  case (dwDirection) of
    DATADIR_GET:
      begin
        enumFormatEtc := Self;
        Result := S_OK;
      end;
    else
      raise ERangeError.Create(SRangeError);
  end;
end;

function TFSession.TDBGridDropData.GetCanonicalFormatEtc(const formatetc: TFormatEtc;
  out formatetcOut: TFormatEtc): HResult;
begin
  MoveMemory(@formatetcOut, @formatetc, SizeOf(formatetc));
  formatetcOut.ptd := nil;
  Result := DATA_S_SAMEFORMATETC;
end;

function TFSession.TDBGridDropData.GetData(const formatetcIn: TFormatEtc;
  out medium: TStgMedium): HResult;
var
  Text: string;
begin
  if (formatetcin.lindex <> -1) then
    Result := DV_E_LINDEX
  else if (formatetcin.tymed <> TYMED_HGLOBAL) then
    Result := DV_E_TYMED
  else
  begin
    Result := S_OK;
    case (formatetcin.cfFormat) of
      CF_UNICODETEXT: Text := DBGrid.SelText;
      CF_MYSQLSQLDATA: Text := DBGrid.SelSQLData;
      else Result := DV_E_FORMATETC;
    end;

    if (Result = S_OK) then
    begin
      FillChar(medium, SizeOf(medium), 0);
      medium.tymed := TYMED_HGLOBAL;
      medium.hGlobal := GlobalAlloc(GMEM_MOVEABLE + GMEM_DDESHARE, SizeOf(Text[1]) * Length(Text));
      MoveMemory(GlobalLock(medium.hGlobal), PChar(Text), Length(Text) * SizeOf(Text[1]));
    end;
  end;
end;

function TFSession.TDBGridDropData.GetDataHere(const formatetc: TFormatEtc;
  out medium: TStgMedium): HResult;
var
  Text: string;
begin
  Text := DBGrid.SelText;

  if (formatetc.lindex <> -1) then
    Result := DV_E_LINDEX
  else if (formatetc.tymed <> TYMED_HGLOBAL) then
    Result := DV_E_TYMED
  else if (GlobalSize(medium.hGlobal) < SIZE_T(Length(Text) * SizeOf(Text[1]))) then
    Result := STG_E_MEDIUMFULL
  else
  begin
    MoveMemory(GlobalLock(medium.hGlobal), PChar(Text), Length(Text) * SizeOf(Text[1]));
    Result := S_OK;
  end;
end;

function TFSession.TDBGridDropData.Next(celt: Longint; out elt;
  pceltFetched: PLongint): HResult;
type
  TFormatEtcArray2 = array [0 .. $FFFF] of FORMATETC;
var
  Formats: ^TFormatEtcArray2;
begin
  if ((celt = 0) or (celt > 1) and not Assigned(pceltFetched)
    or (EnumFormatEtcIndex = 2)) then
    Result := S_FALSE
  else
  begin
    Formats := @elt;

    case (EnumFormatEtcIndex) of
      0:
        begin
          Formats^[0].cfFormat := CF_UNICODETEXT;
          Formats^[0].ptd := nil;
          Formats^[0].dwAspect := DVASPECT_CONTENT;
          Formats^[0].lindex := -1;
          Formats^[0].tymed := TYMED_HGLOBAL;
        end;
      1:
        begin
          Formats^[0].cfFormat := CF_MYSQLSQLDATA;
          Formats^[0].ptd := nil;
          Formats^[0].dwAspect := DVASPECT_CONTENT;
          Formats^[0].lindex := -1;
          Formats^[0].tymed := TYMED_HGLOBAL;
        end;
      else
        raise ERangeError.Create('Index: ' + IntToStr(EnumFormatEtcIndex));
    end;
    Inc(EnumFormatEtcIndex);
    if (Assigned(pceltFetched)) then
      Inc(pceltFetched^);

    if (celt = 1) then
      Result := S_OK
    else
      Result := Next(celt - 1, Formats^[1], pceltFetched);
  end;
end;

function TFSession.TDBGridDropData.QueryGetData(const formatetc: TFormatEtc): HResult;
var
  Format: TFormatEtc;
begin
  if (formatetc.lindex <> -1) then
    Result := DV_E_LINDEX
  else if (formatetc.tymed <> TYMED_HGLOBAL) then
    Result := DV_E_TYMED
  else
  begin
    Reset();
    repeat
      Result := Next(1, Format, nil);
    until ((Result <> S_OK) or (Format.cfFormat = formatetc.cfFormat));

    if (Result = S_FALSE) then
      Result := DV_E_FORMATETC;
  end;
end;

function TFSession.TDBGridDropData.Reset(): HResult;
begin
  EnumFormatEtcIndex := 0;

  Result := S_OK;
end;

function TFSession.TDBGridDropData.SetData(const formatetc: TFormatEtc;
  var medium: TStgMedium; fRelease: BOOL): HResult;
begin
  Result := E_FAIL;
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
    Clear();

    // Debug 2017-02-06
    URI := TUURI.Create(AAddress);
    if ((URI.Param['view'] = 'browser') and (URI.Table = '')) then
      raise ERangeError.Create('AAddress: ' + AAddress + #13#10
        + 'URI.Address: ' + URI.Address);
    if ((URI.Param['view'] = 'objectsearch') and (URI.Param['text'] = Null)) then
      raise ERangeError.Create('AAddress: ' + AAddress + #13#10
        + 'URI.Address: ' + URI.Address);
    URI.Free();

    if (not FSession.Session.Connection.InUse()) then
      FSession.CurrentAddress := AAddress
    else
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
    if (not FSession.ChangeCurrentAddress(TempAddress) and not FSession.Session.Connection.InUse()) then
      FSession.ChangeCurrentAddress(FSession.Session.Account.ExpandAddress('/'));
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

  if ((Window.ActiveControl = ActiveWorkbench) and Assigned(ActiveWorkbench) and (CurrentClassIndex = ciDatabase) and (View = vDiagram)) then
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

  // Debug 2017-02-06
  Session.Connection.DebugMonitor.Append('aDDeleteExecute - start - SBlob: ' + BoolToStr(Assigned(SBlob), True), ttDebug);

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
      else if (TSItem(Items[I]) is TSBaseField) then
        Table := TSBaseField(Items[I]).Table
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
              + 'I: ' + IntToStr(I) + #13#10
              + 'Count: ' + IntToStr(Items.Count) + #13#10
              + Table.Source);
          // 2017-02-08 occurred with "" key name - but inside the CREATE TABLE, there was a primary key. :-/
          // 2017-02-08 occurred with "ProdiDiktiID" key name - but inside the CREATE TABLE, there was this key. :-/
          //   Count: 53 ... did the user try to all fields and keys?


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

  // Debug 2017-02-06
  Session.Connection.DebugMonitor.Append('aDDeleteExecute - end - SBlob: ' + BoolToStr(Assigned(SBlob), True), ttDebug);
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
      ActiveDBGrid.DataSource.DataSet.Delete()
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

function TFSession.AddressByData(const Data: TCustomData): string;
var
  URI: TUURI;
begin
  URI := TUURI.Create(Session.Account.ExpandAddress('/'));

  if (TObject(Data) is TSSession) then
    // nothing to do...
  else if (TObject(Data) is TSDatabase) then
    URI.Database := TSDatabase(Data).Name
  else if (TObject(Data) is TSTable) then
  begin
    URI.Database := TSTable(Data).Database.Name;
    URI.Table := TSTable(Data).Name;
    if (TSTable(Data) is TSView) then
      URI.Param['objecttype'] := 'view'
    else if (TSTable(Data) is TSSystemView) then
      URI.Param['objecttype'] := 'systemview';
  end
  else if (TObject(Data) is TSProcedure) then
  begin
    URI.Database := TSProcedure(Data).Database.Name;
    URI.Param['objecttype'] := 'procedure';
    URI.Param['object'] := TSProcedure(Data).Name;
  end
  else if (TObject(Data) is TSFunction) then
  begin
    URI.Database := TSFunction(Data).Database.Name;
    URI.Param['objecttype'] := 'function';
    URI.Param['object'] := TSFunction(Data).Name;
  end
  else if (TObject(Data) is TSEvent) then
  begin
    URI.Database := TSEvent(Data).Database.Name;
    URI.Param['objecttype'] := 'event';
    URI.Param['object'] := TSEvent(Data).Name;
  end
  else if (TObject(Data) is TSTrigger) then
  begin
    URI.Database := TSTrigger(Data).Database.Name;
    URI.Table := TSTrigger(Data).TableName;
    URI.Param['objecttype'] := 'trigger';
    URI.Param['object'] := TSTrigger(Data).Name;
  end
  else if (TObject(Data) is TSProcesses) then
    URI.Param['system'] := 'processes'
  else if (TObject(Data) is TSUsers) then
    URI.Param['system'] := 'users'
  else if (TObject(Data) is TSVariables) then
    URI.Param['system'] := 'variables'
  else
    FreeAndNil(URI);

  if (not Assigned(URI)) then
    Result := ''
  else
    Result := URI.Address;

  URI.Free();
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
  URI: TUURI;
begin
  if (not (csDestroying in ComponentState)) then
  begin
    OldAddressURI := TUURI.Create(OldAddress);
    NewAddressURI := TUURI.Create(CurrentAddress);

    if (NewAddressURI.Param['view'] <> OldAddressURI.Param['view']) then
      if (NewAddressURI.Param['view'] <> 'objectsearch') then
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
      vObjectSearch: if (not (ttObjectSearch in Preferences.ToolbarTabs)) then begin Include(Preferences.ToolbarTabs, ttObjectSearch); PostMessage(Window.Handle, UM_CHANGEPREFERENCES, 0, 0); end;
    end;

    ToolBarData.Caption := AddressToCaption(CurrentAddress);
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

      if ((fsActive in FrameState) and not (fsLoading in FrameState) and Wanted.Nothing) then
        PlaySound(PChar(Preferences.SoundFileNavigating), Handle, SND_FILENAME or SND_ASYNC);

      if ((View = vBrowser) and (CurrentClassIndex in [ciBaseTable, ciView, ciSystemView])) then
      begin
        Table := TSTable(CurrentData);

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

      FNavigator.AutoExpand := not (CurrentClassIndex in [ciBaseTable, ciView, ciSystemView]) and not CheckWin32Version(6);

      TreeViewExpanded(FNavigator, FNavigator.Selected);
    end;

    LastFNavigatorSelected := FNavigator.Selected;
    if ((CurrentClassIndex in [ciBaseTable, ciView, ciSystemView]) and (View in [vObjects, vBrowser])) then
      LastTableView := View;
    URI := TUURI.Create(CurrentAddress);
    if (URI.Database <> '') then
      LastSelectedDatabase := URI.Address;
    if (URI.Table <> '') then
      LastSelectedTable := URI.Address;
    if (URI.Param['view'] = 'ide') then
      LastSelectedObjectIDE := URI.Address;
    URI.Free();


    Empty := not Assigned(ActiveSynMemo) or (ActiveSynMemo.Lines.Count <= 1) and (ActiveSynMemo.Text = ''); // Takes a lot of time
    if (not Empty and (View = vIDE)) then SQL := ActiveSynMemo.Text else SQL := '';

    MainAction('aFOpen').Enabled := (View = vDiagram) or (View in [vEditor, vEditor2, vEditor3]);
    MainAction('aFSave').Enabled := (View = vDiagram) or (View in [vEditor, vEditor2, vEditor3]) and not Empty and ((SQLEditors[View].Filename = '') or ActiveSynMemo.Modified);
    MainAction('aFSaveAs').Enabled := (View = vDiagram) or (View in [vEditor, vEditor2, vEditor3]) and not Empty;
    MainAction('aVObjects').Enabled := True;
    MainAction('aVBrowser').Enabled := (CurrentClassIndex in [ciBaseTable, ciView, ciSystemView, ciTrigger]) or (LastSelectedTable <> '');
    MainAction('aVIDE').Enabled := (CurrentClassIndex in [ciView, ciProcedure, ciFunction, ciEvent, ciTrigger]) or (LastSelectedObjectIDE <> '');
    MainAction('aVBuilder').Enabled := LastSelectedDatabase <> '';
    MainAction('aVSQLEditor').Enabled := True;
    MainAction('aVSQLEditor2').Enabled := True;
    MainAction('aVSQLEditor3').Enabled := True;
    MainAction('aVDiagram').Enabled := LastSelectedDatabase <> '';
    MainAction('aDRun').Enabled :=
      ((View in [vEditor, vEditor2, vEditor3])
      or ((View = vBuilder) and FQueryBuilder.Visible)
      or ((View = vIDE) and SQLSingleStmt(SQL) and (CurrentClassIndex in [ciView, ciProcedure, ciFunction, ciEvent]))) and not Empty;
    MainAction('aDRunSelection').Enabled := (((View in [vEditor, vEditor2, vEditor3]) and not Empty) or Assigned(ActiveSynMemo) and (Trim(ActiveSynMemo.SelText) <> ''));
    MainAction('aDPostObject').Enabled := (View = vIDE) and Assigned(ActiveSynMemo) and ActiveSynMemo.Modified and SQLSingleStmt(SQL)
      and ((CurrentClassIndex in [ciView]) and SQLCreateParse(Parse, PChar(SQL), Length(SQL),Session.Connection.MySQLVersion) and (SQLParseKeyword(Parse, 'SELECT'))
        or (CurrentClassIndex in [ciProcedure, ciFunction]) and SQLParseDDLStmt(DDLStmt, PChar(SQL), Length(SQL), Session.Connection.MySQLVersion) and (DDLStmt.DefinitionType = dtCreate) and (DDLStmt.ObjectType in [otProcedure, otFunction])
        or (CurrentClassIndex in [ciEvent, ciTrigger]));
    MainAction('aEFormatSQL').Enabled := not Empty;

    if (View <> vObjectSearch) then
    begin
      FObjectSearch.Text := '';

      // Debug 2017-02-02
      if (FObjectSearchStart.Enabled) then
        raise ERangeError.Create(SRangeError);
    end;

    StatusBarRefresh();


    FNavigatorMenuNode := FNavigator.Selected;

    Wanted.Update := UpdateAfterAddressChanged;

    if (fsLoading in FrameState) then
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
      Exclude(FrameState, fsLoading);
    end;

    OldAddress := CurrentAddress;

    KillTimer(Handle, tiShowSynCompletion);
  end;
end;

procedure TFSession.AddressChanging(const Sender: TObject; const NewAddress: String; var AllowChange: Boolean);
var
  Database: TSDatabase;
  DBObject: TSDBObject;
  NotFound: Boolean;
  S: string;
  URI: TUURI;
begin
  Assert(NewAddress <> '');

  URI := TUURI.Create(NewAddress); NotFound := False;

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
      Session.Update();
    end
    else if (URI.Param['system'] = 'users') then
      Session.Update()
    else if (URI.Param['system'] = 'variables') then
      Session.Update()
    else if (URI.Param['system'] = 'quick') then
      Session.Update()
    else if (URI.Database = '') then
      Session.Update(nil, URI.Param['view'] = Null)
    else if (not Session.Databases.Update()) then
      AllowChange := False
    else
    begin
      Database := Session.DatabaseByName(URI.Database);
      if (not Assigned(Database)) then
        NotFound := True
      else if (not (ParamToView(URI.Param['view']) in [vEditor, vEditor2, vEditor3]) and not Database.Update(URI.Param['view'] = Null)) then
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

  ActiveDBGrid.DataSource.DataSet.Insert();
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

  if ((Window.ActiveControl = ActiveListView) and (CurrentClassIndex in [ciDatabase, ciSystemDatabase]) and (ActiveListView.SelCount > 1)) then
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
    else if (SItem is TSBaseField) then
    begin
      DField.Table := TSBaseTable(TSBaseField(SItem).Table);
      DField.Field := TSBaseField(SItem);
      DField.ModifyTableOnly := False;

      // Debug 2017-01-15
      if (DField.Field.FieldType = mfUnknown) then
        raise ERangeError.Create('Name: ' + DField.Field.Name + #13#10
          + 'Table.Valid: ' + BoolToStr(DField.Table.Valid) + #13#10
          + 'Table.ValidSource: ' + BoolToStr(DField.Table.ValidSource) + #13#10
          + 'Fields.Valid: ' + BoolToStr(DField.Table.Fields.Valid, True) + #13#10
          + DField.Table.Source);

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
      Process := Session.ProcessByThreadId(SysUtils.StrToInt64(ActiveListView.Selected.Caption));

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

    UpdateAfterAddressChanged();
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
  if (View in [vBuilder]) then
    SQL := Trim(ActiveSynMemo.Text)
  else if (View in [vEditor, vEditor2, vEditor3]) then
    SQL := Trim(ActiveSynMemo.Text)
  else if ((View = vIDE) and (CurrentClassIndex = ciView)) then
  begin
    if (not ActiveSynMemo.Modified or PostObject(Sender)) then
      View := vBrowser;
  end
  else if ((View = vIDE) and (CurrentClassIndex = ciProcedure)) then
  begin
    if (Assigned(FObjectIDEGrid.DataSource.DataSet)) then
      FObjectIDEGrid.DataSource.DataSet.CheckBrowseMode();
    if (not ActiveSynMemo.Modified or PostObject(Sender)) then
      SQL := TSProcedure(CurrentData).SQLRun();
  end
  else if ((View = vIDE) and (CurrentClassIndex = ciFunction)) then
  begin
    if (Assigned(FObjectIDEGrid.DataSource.DataSet)) then
      FObjectIDEGrid.DataSource.DataSet.CheckBrowseMode();
    if (not ActiveSynMemo.Modified or PostObject(Sender)) then
      SQL := TSFunction(CurrentData).SQLRun();
  end
  else if ((View = vIDE) and (CurrentClassIndex = ciEvent)) then
  begin
    if (not ActiveSynMemo.Modified or PostObject(Sender)) then
      SQL := TSEvent(CurrentData).SQLRun();
  end;

  if (SQL = '') then
    MessageBeep(MB_ICONERROR)
  else
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
  ClassIndex: TClassIndex;
  ClipboardData: HGLOBAL;
  Data: string;
  I: Integer;
  Opened: Boolean;
  Retry: Integer;
  S: string;
  StringList: TStringList;
begin
  Retry := 0;
  repeat
    Opened := OpenClipboard(Handle);
    if (Opened) then
      CloseClipboard()
    else
    begin
      Sleep(50);
      Inc(Retry);
    end;
  until (Opened or (Retry = 10));


  Data := '';

  if (not Opened
    or not Assigned(Window.ActiveControl)) then
  begin
    MessageBeep(MB_ICONERROR);
    Exit;
  end
  else if (Window.ActiveControl = FNavigator) then
  begin
    if (not (TObject(FNavigatorMenuNode.Data) is TSItem)) then
      ClassIndex := ciUnknown
    else
    begin
      ClassIndex := ClassIndexByData(FNavigatorMenuNode.Parent.Data);
      case (FNavigatorMenuNode.ImageIndex) of
        iiDatabase:        Data := Data + 'Database='    + FNavigatorMenuNode.Text + #13#10;
        iiBaseTable:       Data := Data + 'Table='       + FNavigatorMenuNode.Text + #13#10;
        iiView:            Data := Data + 'View='        + FNavigatorMenuNode.Text + #13#10;
        iiProcedure:       Data := Data + 'Procedure='   + FNavigatorMenuNode.Text + #13#10;
        iiFunction:        Data := Data + 'Function='    + FNavigatorMenuNode.Text + #13#10;
        iiEvent:           Data := Data + 'Event='       + FNavigatorMenuNode.Text + #13#10;
        iiKey:             Data := Data + 'Key='         + TSKey(FNavigatorMenuNode.Data).Name + #13#10;
        iiBaseField,
        iiVirtualField,
        iiViewField,
        iiSystemViewField: Data := Data + 'Field='       + FNavigatorMenuNode.Text + #13#10;
        iiForeignKey:      Data := Data + 'ForeignKey='  + FNavigatorMenuNode.Text + #13#10;
        iiTrigger:         Data := Data + 'Trigger='     + FNavigatorMenuNode.Text + #13#10;
        iiUser:            Data := Data + 'User='        + FNavigatorMenuNode.Text + #13#10;
      end;
      if (Data <> '') then
        Data := 'Address=' + AddressByData(FNavigatorMenuNode.Parent.Data) + #13#10 + Data;
    end;
  end
  else if (Window.ActiveControl = ActiveListView) then
  begin
    ClassIndex := CurrentClassIndex;
    for I := 0 to ActiveListView.Items.Count - 1 do
      if (ActiveListView.Items[I].Selected) then
        case (ActiveListView.Items[I].ImageIndex) of
          iiDatabase:        Data := Data + 'Database='   + ActiveListView.Items[I].Caption + #13#10;
          iiBaseTable:       Data := Data + 'Table='      + ActiveListView.Items[I].Caption + #13#10;
          iiView:            Data := Data + 'View='       + ActiveListView.Items[I].Caption + #13#10;
          iiProcedure:       Data := Data + 'Procedure='  + ActiveListView.Items[I].Caption + #13#10;
          iiFunction:        Data := Data + 'Function='   + ActiveListView.Items[I].Caption + #13#10;
          iiEvent:           Data := Data + 'Event='      + ActiveListView.Items[I].Caption + #13#10;
          iiKey:
            begin
              // Debug 2016-11-11
              if (not (TObject(ActiveListView.Items[I].Data) is TSKey)) then
                raise ERangeError.Create(SPropertyOutOfRange + ' (' + TObject(ActiveListView.Items[I].Data).ClassName + ')');
                   Data := Data + 'Key='        + TSKey(ActiveListView.Items[I].Data).Name + #13#10;
            end;
          iiBaseField,
          iiVirtualField,
          iiViewField,
          iiSystemViewField: Data := Data + 'Field='      + ActiveListView.Items[I].Caption + #13#10;
          iiForeignKey:      Data := Data + 'ForeignKey=' + ActiveListView.Items[I].Caption + #13#10;
          iiTrigger:         Data := Data + 'Trigger='    + ActiveListView.Items[I].Caption + #13#10;
          iiUser:            Data := Data + 'User='       + ActiveListView.Items[I].Caption + #13#10;
        end;
    if (Data <> '') then
      Data := 'Address=' + CurrentAddress + #13#10 + Data;
  end
  else if (Window.ActiveControl = ActiveDBGrid) then
  begin
    ClassIndex := ciUnknown;
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
      ClassIndex := CurrentClassIndex;
      if (Assigned(ActiveWorkbench)) then
      begin
        Data := '';
        if (not Assigned(ActiveWorkbench.Selected)) then
          Data := Data + 'Database='   + ActiveWorkbench.Database.Name + #13#10
        else if (ActiveWorkbench.Selected is TWTable) then
          Data := Data + 'Table='      + TWTable(ActiveWorkbench.Selected).BaseTable.Name + #13#10
        else if (ActiveWorkbench.Selected is TWForeignKey) then
          Data := Data + 'ForeignKey=' + TWForeignKey(ActiveWorkbench.Selected).BaseForeignKey.Name + #13#10;
        if (Data <> '') then
          Data := 'Address=' + CurrentAddress + #13#10 + Data;
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

  if (Data <> '') then
  begin
    Retry := 0;
    repeat
      Opened := OpenClipboard(Handle);
      if (Opened) then
        CloseClipboard()
      else
      begin
        Sleep(50);
        Inc(Retry);
      end;
    until (Opened or (Retry = 10));
  end;

  if ((Data <> '') and OpenClipboard(Handle)) then
  begin
    try
      EmptyClipboard();

      ClipboardData := GlobalAlloc(GMEM_MOVEABLE + GMEM_DDESHARE, SizeOf(Char) * (Length(Data) + 1));
      StrPCopy(GlobalLock(ClipboardData), Data);
      case (ClassIndex) of
        ciSession: SetClipboardData(CF_MYSQLSERVER, ClipboardData);
        ciDatabase: SetClipboardData(CF_MYSQLDATABASE, ClipboardData);
        ciBaseTable: SetClipboardData(CF_MYSQLTABLE, ClipboardData);
        ciView: SetClipboardData(CF_MYSQLVIEW, ClipboardData);
        ciUsers: SetClipboardData(CF_MYSQLUSERS, ClipboardData);
      end;
      GlobalUnlock(ClipboardData);

      StringList := TStringList.Create();
      StringList.Text := Trim(Data);
      for I := 1 to StringList.Count - 1 do
        if (StringList.ValueFromIndex[I] <> '') then
        begin
          if (S <> '') then S := S + #13#10;
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
  ClipboardData: HGLOBAL;
  Node: TTreeNode;
  Opened: Boolean;
  Retry: Integer;
  S: string;
begin
  Retry := 0;
  repeat
    Opened := OpenClipboard(Handle);
    if (Opened) then
      CloseClipboard()
    else
    begin
      Sleep(50);
      Inc(Retry);
    end;
  until (Opened or (Retry = 10));


  if (not Opened) then
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
    else if (Window.ActiveControl = ActiveListView) then
      if (Assigned(ActiveListView.Selected)) then
        Node := FNavigatorNodeByAddress(AddressByData(ActiveListView.Selected))
      else
        Node := FNavigatorNodeByAddress(AddressByData(TObject(ActiveListView.Tag)))
    else
      Node := FNavigatorNodeByAddress(CurrentAddress);

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
    if (IsClipboardFormatAvailable(CF_MYSQLSQLDATA)) then
    begin
      if (OpenClipboard(Handle)) then
        try
          ClipboardData := GetClipboardData(CF_MYSQLSQLDATA);
          if (ClipboardData <> 0) then
          begin
            SetString(S, PChar(GlobalLock(ClipboardData)), GlobalSize(ClipboardData) div SizeOf(S[1]));
            GlobalUnlock(ClipboardData);
            ActiveSynMemo.SelText := S;
          end;
        finally
          CloseClipboard();
        end;
    end
    else
      ActiveSynMemo.PasteFromClipboard()
  else if (Assigned(ActiveWorkbench) and (Window.ActiveControl = ActiveWorkbench)) then
    WorkbenchPasteExecute(Sender)
  else if ((Window.ActiveControl = FFilter) and (Clipboard.HasFormat(CF_UNICODETEXT) or Clipboard.HasFormat(CF_MYSQLSQLDATA))) then
  begin
    if (Clipboard.HasFormat(CF_MYSQLSQLDATA)) then
    begin
      if (OpenClipboard(Handle)) then
        try
          ClipboardData := GetClipboardData(CF_MYSQLSQLDATA);
          if (ClipboardData <> 0) then
          begin
            SetString(S, PChar(GlobalLock(ClipboardData)), GlobalSize(ClipboardData) div SizeOf(S[1]));
            GlobalUnlock(ClipboardData);
            FFilter.SelText := S;
          end;
        finally
          CloseClipboard();
        end;
    end
    else
    begin
      Text := Clipboard.AsText;
      if (Pos(#13#10, Text) > 0) then
        Text := '(' + ReplaceStr(Text, #13#10, '),(') + ')'
      else if (Pos(#10, Text) > 0) then
        Text := '(' + ReplaceStr(Text, #10, '),(') + ')';
      FFilter.SelText := ReplaceStr(Text, #9, ',');
    end;
  end
  else if (Window.ActiveControl = FQuickSearch) then
    FQuickSearch.PasteFromClipboard()
  else if (Window.ActiveControl is TCustomEdit) then
    TCustomEdit(Window.ActiveControl).PasteFromClipboard()
  else
    MessageBeep(MB_ICONERROR);
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
    raise ERangeError.Create('Address: ' + CurrentAddress + #13#10
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
    Database := TSDatabase(CurrentData);
    if (((Session.Databases.NameCmp(Database.Name, 'mysql') <> 0) and (Session.Databases.NameCmp(Database.Name, 'sys') <> 0) and (Session.Connection.MySQLVersion >= 50707)) and not (Database is TSSystemDatabase)) then
      for I := 0 to ActiveWorkbench.Tables.Count - 1 do
        if (not Assigned(ActiveWorkbench.Selected) or ActiveWorkbench.Tables[I].Selected) then
          DExport.DBObjects.Add(ActiveWorkbench.Tables[I].BaseTable);
  end
  else if ((Window.ActiveControl = ActiveListView) and (ActiveListView.SelCount > 0)) then
  begin
    case (CurrentClassIndex) of
      ciSession:
        for I := 0 to ActiveListView.Items.Count - 1 do
          if (ActiveListView.Items[I].Selected) then
          begin
            Database := TSDatabase(ActiveListView.Items[I].Data);
            if (((Session.Databases.NameCmp(Database.Name, 'mysql') <> 0) and (Session.Databases.NameCmp(Database.Name, 'sys') <> 0) and (Session.Connection.MySQLVersion >= 50707)) and not (Database is TSSystemDatabase)) then
              DExport.DBObjects.Add(Database);
          end;

      ciDatabase:
        begin
          Database := TSDatabase(CurrentData);
          if (not (Database is TSSystemDatabase)) then
            for I := 0 to ActiveListView.Items.Count - 1 do
              if (ActiveListView.Items[I].Selected) then
                DExport.DBObjects.Add(TSDBObject(ActiveListView.Items[I].Data));
        end;
      ciBaseTable:
        begin
          Database := TSTable(CurrentData).Database;
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

    DImport.FNavigator := @FNavigator;

    DImport.Execute();

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

  if (ActiveSynMemo.SelAvail) then
    SQL := ActiveSynMemo.SelText
  else
  begin
    SQL := ActiveSynMemo.Text;
    case (CurrentClassIndex) of
      ciTrigger:
        SQL := TriggerPrefix + SQL;
      ciEvent:
        SQL := EventPrefix + SQL;
    end;
  end;

  if (not Session.SQLParser.ParseSQL(SQL)) then
  begin
    if ((Session.SQLParser.ErrorPos > 0) and Assigned(Session.SQLParser.ErrorToken)) then
    begin
      ActiveSynMemo.SelStart := Session.SQLParser.ErrorPos;
      ActiveSynMemo.SelLength := Length(Session.SQLParser.ErrorToken.Text);
    end;
    MsgBox(Session.SQLParser.ErrorMessage, Preferences.LoadStr(45), MB_OK + MB_ICONERROR);
  end
  else
  begin
    SQL := Session.SQLParser.FormatSQL();

    if (ActiveSynMemo.SelAvail) then
      ActiveSynMemo.SelText := SQL
    else
    begin
      case (CurrentClassIndex) of
        ciTrigger:
          begin
            Delete(SQL, 1, Pos('FOR EACH ROW', SQL) + Length('FOR EACH ROW'));
            SQL := Trim(SQL);
          end;
        ciEvent:
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
  if (Assigned(FSQLHistoryMenuNode) and (FSQLHistoryMenuNode.ImageIndex in [iiStatement, iiQuery])) then
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

  if (Assigned(FSQLHistoryMenuNode) and (FSQLHistoryMenuNode.ImageIndex in [iiStatement, iiQuery])) then
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
      WSQLHelp.Keyword := ActiveSynMemo.SelText
    else if (ActiveSynMemo.WordAtCursor <> '') then
      WSQLHelp.Keyword := ActiveSynMemo.WordAtCursor
    else
      WSQLHelp.Keyword := ''
  else
    WSQLHelp.Keyword := '';

  WSQLHelp.Session := Session;
  WSQLHelp.Execute();
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
      vEditor3: if (PSynMemo.Visible) then Window.ActiveControl := ActiveSynMemo;
      vObjectSearch: if (PListView.Visible) then Window.ActiveControl := ObjectSearchListView;
    end;
  end;
end;

procedure TFSession.aVRefreshAllExecute(Sender: TObject);
begin
  KillTimer(Handle, tiStatusBar);
  KillTimer(Handle, tiNavigator);

  Session.Invalidate();

  CurrentAddress := CurrentAddress;
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
          case (CurrentClassIndex) of
            ciSession: Session.Invalidate();
            ciDatabase,
            ciSystemDatabase: TSDatabase(CurrentData).Invalidate();
            ciBaseTable,
            ciView,
            CiSystemView:
              begin
                TSTable(CurrentData).Invalidate();

                if ((TSTable(CurrentData) is TSBaseTable) and
                  Assigned(TSBaseTable(CurrentData).Database.Triggers)) then
                  TSBaseTable(CurrentData).Database.Triggers.Invalidate();
              end;
            ciProcedure: TSProcedure(CurrentData).Invalidate();
            ciFunction: TSFunction(CurrentData).Invalidate();
            ciEvent: TSEvent(CurrentData).Invalidate();
            ciTrigger: TSTrigger(CurrentData).Invalidate();
            ciProcesses: Session.Processes.Invalidate();
            ciUsers: Session.Users.Invalidate();
            CiVariables: Session.Variables.Invalidate();
          end;

          CurrentAddress := CurrentAddress;
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
              begin
                ActiveDBGrid.DataSource.DataSet.Close();
                CurrentAddress := CurrentAddress;
              end;
            end;
          end;
        end;
      vDiagram:
        begin
          TSDatabase(CurrentData).Tables.Invalidate();
          List := TList.Create();
          List.Add(TSDatabase(CurrentData).Tables);
          Session.Update(List);
          List.Free();
        end;
      vObjectSearch:
        begin
          FreeAndNil(ObjectSearchListView);
          FreeAndNil(ObjectSearch);
          Wanted.Update := UpdateAfterAddressChanged;
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
  if (TObject(CurrentData) is TSTrigger) then
    Trigger := TSTrigger(CurrentData);

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

function TFSession.ClassIndexByAddress(const Address: string): TClassIndex;
var
  URI: TUURI;
begin
  URI := TUURI.Create(Address);

  if (URI.Param['system'] = 'processes') then
    Result := ciProcesses
  else if (URI.Param['system'] = 'users') then
    Result := ciUsers
  else if (URI.Param['system'] = 'variables') then
    Result := ciVariables
  else if (URI.Param['system'] = 'quick') then
    Result := ciQuickAccess
  else if (URI.Database = '') then
    Result := ciSession
  else if (URI.Param['objecttype'] = 'view') then
    Result := ciView
  else if (URI.Param['objecttype'] = 'systemview') then
    Result := ciSystemView
  else if (URI.Param['objecttype'] = 'procedure') then
    Result := ciProcedure
  else if (URI.Param['objecttype'] = 'function') then
    Result := ciFunction
  else if (URI.Param['objecttype'] = 'trigger') then
    Result := ciTrigger
  else if (URI.Param['objecttype'] = 'event') then
    Result := ciEvent
  else if (URI.Table <> '') then
    Result := ciBaseTable
  else if (Assigned(Session.InformationSchema) and (Session.Databases.NameCmp(URI.Database, Session.InformationSchema.Name) = 0)
    or Assigned(Session.PerformanceSchema) and (Session.Databases.NameCmp(URI.Database, Session.PerformanceSchema.Name) = 0)) then
    Result := ciSystemDatabase
  else if (URI.Database <> '') then
    Result := ciDatabase
  else
    raise ERangeError.Create('Unknown ClassIndex for: ' + URI.Address);

  URI.Free();
end;

function TFSession.ClassIndexByData(const Data: TCustomData): TClassIndex;
begin
  if (not Assigned(Data)) then
    Result := ciUnknown
  else if (TObject(Data) is TSSession) then
    Result := ciSession
  else if (TObject(Data) is TSSystemDatabase) then
    Result := ciSystemDatabase
  else if (TObject(Data) is TSDatabase) then
    Result := ciDatabase
  else if (TObject(Data) is TSBaseTable) then
    Result := ciBaseTable
  else if (TObject(Data) is TSView) then
    Result := ciView
  else if (TObject(Data) is TSSystemView) then
    Result := ciSystemView
  else if (TObject(Data) is TSProcedure) then
    Result := ciProcedure
  else if (TObject(Data) is TSFunction) then
    Result := ciFunction
  else if (TObject(Data) is TSTrigger) then
    Result := ciTrigger
  else if (TObject(Data) is TSEvent) then
    Result := ciEvent
  else if (TObject(Data) is TSKey) then
    Result := ciKey
  else if (TObject(Data) is TSBaseField) then
    Result := ciBaseField
  else if (TObject(Data) is TSViewField) then
    Result := ciViewField
  else if (TObject(Data) is TSForeignKey) then
    Result := ciForeignKey
  else if (TObject(Data) is TSProcesses) then
    Result := ciProcesses
  else if (TObject(Data) is TSProcess) then
    Result := ciProcess
  else if (TObject(Data) is TSUsers) then
    Result := ciUsers
  else if (TObject(Data) is TSUser) then
    Result := ciUser
  else if (TObject(Data) is TSVariables) then
    Result := ciVariables
  else if (TObject(Data) is TSVariable) then
    Result := ciVariable
  else if (TObject(Data) is TSItemSearch) then
    Result := ciObjectSearch
  else
    raise ERangeError.Create('ClassType: ' + TObject(Data).ClassName);
end;

function TFSession.ChangeCurrentAddress(const AAddress: string): Boolean;
var
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
  Assert(AAddress <> '');

  NewAddress := AAddress;
  Result := True; Node := nil;
  AddressChanging(nil, NewAddress, Result);
  if (Result) then
  begin
    Node := FNavigatorNodeByAddress(NewAddress);
    Result := Assigned(Node);
    if (not Result) then
      Wanted.Address := Session.Account.ExpandAddress('/');
  end;
  if (Result) then
  begin
    ChangingEvent := FNavigator.OnChanging; FNavigator.OnChanging := nil;
    ChangeEvent := FNavigator.OnChange; FNavigator.OnChange := nil;
    FNavigator.Selected := Node;
    FNavigator.OnChanging := ChangingEvent;
    FNavigator.OnChange := ChangeEvent;

    URI := TUURI.Create(NewAddress);

    if ((URI.Param['view'] = Null) and ((URI.Param['objecttype'] = 'procedure') or (URI.Param['objecttype'] = 'function') or (URI.Param['objecttype'] = 'trigger') or (URI.Param['objecttype'] = 'event'))) then
      URI.Param['view'] := 'ide';

    // Debug 2017-02-03
    if ((ParamToView(URI.Param['view']) in [vBrowser]) and (URI.Table = '')) then
      raise ERangeError.Create('AAddress: ' + AAddress + #13#10
        + 'URI.Address: ' + URI.Address);
    // Debug 2017-02-04
    Assert((ParamToView(URI.Param['view']) <> vObjectSearch) or (URI.Param['text'] <> Null),
      'URI.Address: ' + URI.Address);

    FCurrentAddress := URI.Address;
    if ((Session.Account.Desktop.Addresses.Count = 0)
      or (FCurrentAddress <> Session.Account.Desktop.Addresses[Session.Account.Desktop.Addresses.Count - 1])) then
      Session.Account.Desktop.Addresses.Add(FCurrentAddress);

    NewView := ParamToView(URI.Param['view']);
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

          // Debug 2017-01-30
          if (not Assigned(Table)) then
            raise ERangeError.Create('Table: ' + URI.Table + #13#10
              + 'URI.Address: ' + URI.Address + #13#10
              + 'AAddress: ' + AAddress);

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

    if (Assigned(QuickAccessListView) and (URI.Param['view'] <> 'quick')) then
    begin
      Session.Account.Desktop.QuickAccessMFUVisible := not (lgsCollapsed in QuickAccessListView.Groups[0].State);
      Session.Account.Desktop.QuickAccessMRUVisible := not (lgsCollapsed in QuickAccessListView.Groups[1].State);
      FreeListView(QuickAccessListView);
      QuickAccessListView := nil;
    end;

    URI.Free();

    AddressChanged(nil);
  end;
end;

function TFSession.ColumnWidthKindByListView(const ListView: TListView): TPAccount.TDesktop.TListViewKind;
begin
  if (TObject(ListView.Tag) is TSSession) then
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
  else if (TObject(ListView.Tag) is TSItemSearch) then
    Result := lkObjectSearch
  else if (TObject(ListView.Tag) is TSQuickAccess) then
    Result := lkQuickAccess
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
  Profile: TProfile;
begin
  CreateProfile(Profile);

  inherited Create(AOwner);

  ProfilingPoint(Profile, 1);

  ASession.Account.RegisterTab(Self);

  Parent := TWinControl(AParent);
  OleCheck(RegisterDragDrop(Handle, Self));


  Width := Window.ClientWidth;
  Height := Window.ClientHeight;

  FrameState := [fsLoading];

  NMListView := nil;
  Session := ASession;
  SQLEditor := TSQLEditor.Create(Self, FSQLEditorSynMemo, PSQLEditorDBGrid);
  Param := AParam;
  ActiveControlOnDeactivate := nil;
  ActiveDBGrid := nil;
  ActiveIDEInputDataSet := nil;
  ActiveListView := nil;
  ActiveWorkbench := nil;
  CloseButtonNormal := nil;
  CloseButtonPushed := nil;
  CloseButtonHot := nil;
  FCurrentAddress := Session.Account.ExpandAddress('/');
  ObjectSearch := nil;
  ObjectSearchListView := nil;
  ProcessesListView := nil;
  QuickAccessListView := nil;
  SBlobDebug := SBlob;
  ServerListView := nil;
  UsersListView := nil;
  VariablesListView := nil;
  for Kind := Low(ListViewSortData) to High(ListViewSortData) do
  begin
    ListViewSortData[Kind].Kind := Kind;
    ListViewSortData[Kind].ColumnIndex := 0;
    if (Kind = lkTable) then
      ListViewSortData[Kind].Order := 0
    else
      ListViewSortData[Kind].Order := 1;
  end;
  ListViewSortData[lkQuickAccess].Order := 0;
  FNavigatorDragDisabled := False;
  FNavigatorHotTrackDisabled := False;
  FNavigatorIgnoreChange := False;
  FNavigatorNodeAfterActivate := nil;
  FNavigatorNodeToExpand := nil;
  PanelMouseDownPoint := Point(-1, -1);
  SynCompletionPending.Active := False;

  TimeMonitor := TMySQLMonitor.Create(nil);
  TimeMonitor.CacheSize := 10000;


  TBSideBar.Images := Preferences.Images;
  ToolBar.Images := Preferences.Images;
  TBObjectSearch.Images := Preferences.Images;
  FNavigator.Images := Preferences.Images;
  FSQLHistory.Images := Preferences.Images;
  TBLimitEnabled.Images := Preferences.Images;
  TBQuickSearchEnabled.Images := Preferences.Images;
  TBFilterEnabled.Images := Preferences.Images;
  FListView.SmallImages := Preferences.Images;

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

  FListView.RowSelect := CheckWin32Version(6);
  SetWindowLong(ListView_GetHeader(FListView.Handle), GWL_STYLE, GetWindowLong(ListView_GetHeader(FListView.Handle), GWL_STYLE) or HDS_DRAGDROP);

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
  LastSelectedObjectIDE := '';
  LastSelectedTable := '';
  LastTableView := vObjects;


  Session.CreateDesktop := CreateDesktop;
  Session.RegisterEventProc(FormSessionEvent);
  Session.Account.Favorites.RegisterEventProc(FavoritesEvent);

  Wanted := TWanted.Create(Self);

  FilterMRU := TPPreferences.TMRUList.Create(100);

  FOffset.Constraints.MaxWidth := FOffset.Width;

  ProfilingPoint(Profile, 2);
  Perform(UM_CHANGEPREFERENCES, 0, 0);
  ProfilingPoint(Profile, 3);
  Perform(CM_SYSFONTCHANGED, 0, 0);
  ProfilingPoint(Profile, 4);

  NonClientMetrics.cbSize := SizeOf(NonClientMetrics);
  if (SystemParametersInfo(SPI_GETNONCLIENTMETRICS, SizeOf(NonClientMetrics), @NonClientMetrics, 0)) then
    Window.ApplyWinAPIUpdates(Self, NonClientMetrics.lfStatusFont);


  PostMessage(Handle, UM_POST_SHOW, 0, 0);

  if (ProfilingTime(Profile) > 1000) then
    SendToDeveloper(ProfilingReport(Profile));
  CloseProfile(Profile);
end;

procedure TFSession.CreateParams(var Params: TCreateParams);
begin
  inherited;

  Params.Style := Params.Style
    and not (WS_THICKFRAME or WS_SYSMENU or WS_DLGFRAME or WS_BORDER);
end;

procedure TFSession.CMSysFontChanged(var Msg: TMessage);
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

    R.Inflate(- 3 * GetSystemMetrics(SM_CXEDGE) div 2, - 3 * GetSystemMetrics(SM_CYEDGE) div 2);
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
    R.Inflate(- 3 * GetSystemMetrics(SM_CXEDGE) div 2, - 3 * GetSystemMetrics(SM_CYEDGE) div 2);
    DrawCloseBitmap(CloseButtonNormal.Bitmap, R);
    DrawCloseBitmap(CloseButtonHot.Bitmap, R);
    R.Offset(GetSystemMetrics(SM_CXEDGE) div 2, GetSystemMetrics(SM_CYEDGE) div 2);
    DrawCloseBitmap(CloseButtonPushed.Bitmap, R);
  end;

  PHeader.ClientHeight := ToolBar.Height + PHeader.Canvas.Pen.Width;
  if (not StyleServices.Enabled or not StyleServices.GetElementColor(StyleServices.GetElementDetails(ttTopTabItemSelected), ecGlowColor, Color)) then
  begin
    PHeader.Color := clBtnFace;
    PDataBrowser.Color := clBtnFace;
  end
  else
  begin
    PHeader.Color := Color;
    PDataBrowser.Color := Color;
  end;

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
  TBLimitEnabled.ButtonHeight := FUDLimit.Height;
  TBLimitEnabled.Height := TBLimitEnabled.ButtonHeight;
  TBFilterEnabled.ButtonHeight := FFilter.Height;
  TBFilterEnabled.Height := TBFilterEnabled.ButtonHeight;
  TBQuickSearchEnabled.ButtonHeight := FQuickSearch.Height;
  TBQuickSearchEnabled.Height := TBQuickSearchEnabled.ButtonHeight;

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
  Result.DataSource := FGridDataSource;
  Result.DefaultDrawing := False;
  Result.DragMode := dmAutomatic;
  Result.HelpType := htContext;
  Result.HelpContext := 1155;
  Result.Options := [dgTitles, dgColumnResize, dgColLines, dgRowLines, dgTabs, dgConfirmDelete, dgMultiSelect, dgTitleClick, dgTitleHotTrack];
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
  Result.OnDragOver := DBGridDragOver;
  Result.OnEnter := DBGridEnter;
  Result.OnExit := DBGridExit;
  Result.OnHeaderSplitButton := DBGridHeaderSplitButton;
  Result.OnKeyDown := DBGridKeyDown;
  Result.OnMouseMove := DBGridMouseMove;
  Result.OnTitleClick := DBGridTitleClick;

  Result.DataSource := DataSource;

  Result.Parent := PDBGrid;
  Result.Header.OnMouseMove := DBGridHeaderMouseMove;

  Result.Constraints.MinHeight := 3 * Result.Canvas.TextHeight('I');

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
  Result := TListView.Create(FListView.Owner);

  Result.Left := 0;
  Result.Top := 0;
  Result.Width := PListView.ClientWidth;
  Result.Height := PListView.ClientHeight;
  Result.Align := alClient;
  Result.BorderStyle := FListView.BorderStyle;
  Result.DoubleBuffered := FListView.DoubleBuffered;
  Result.DragMode := FListView.DragMode;
  Result.HelpType := htContext;
  Result.HelpContext := FListView.HelpContext;
  Result.HideSelection := FListView.HideSelection;
  Result.MultiSelect := FListView.MultiSelect;
  Result.GroupView := FListView.GroupView;
  Result.PopupMenu := FListView.PopupMenu;
  Result.RowSelect := FListView.RowSelect;
  Result.SmallImages := Preferences.Images;
  Result.ViewStyle := FListView.ViewStyle;
  Result.Visible := False;
  if (TObject(Data) is TSTable) then
  begin
    Result.OnAdvancedCustomDrawItem := ListViewAdvancedCustomDrawItem;
    Result.OnAdvancedCustomDrawSubItem := ListViewAdvancedCustomDrawSubItem;
  end;

  Result.Parent := FListView.Parent;

  Result.OnChange := FListView.OnChange;
  Result.OnChanging := FListView.OnChanging;
  Result.OnColumnClick := FListView.OnColumnClick;
  Result.OnCompare := FListView.OnCompare;
  Result.OnDblClick := FListView.OnDblClick;
  Result.OnEdited := FListView.OnEdited;
  Result.OnEditing := FListView.OnEditing;
  Result.OnEnter := FListView.OnEnter;
  Result.OnExit := FListView.OnExit;
  Result.OnDragDrop := FListView.OnDragDrop;
  Result.OnDragOver := FListView.OnDragOver;
  Result.OnKeyDown := FListView.OnKeyDown;
  Result.OnSelectItem := FListView.OnSelectItem;

  Result.Tag := NativeInt(Data);

  SetWindowLong(ListView_GetHeader(Result.Handle), GWL_STYLE, GetWindowLong(ListView_GetHeader(FListView.Handle), GWL_STYLE) or HDS_DRAGDROP);

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

function TFSession.DataByAddress(const Address: string): TCustomData;
var
  ClassIndex: TClassIndex;
  Database: TSDatabase;
  URI: TUURI;
begin
  URI := TUURI.Create(Address);

  ClassIndex := ClassIndexByAddress(URI.Address);
  case (ClassIndex) of
    ciSession: Result := Session;
    ciDatabase,
    ciSystemDatabase: Result := Session.DatabaseByName(URI.Database);
    ciBaseTable,
    ciView,
    ciSystemView,
    ciProcedure,
    ciFunction,
    ciEvent,
    ciTrigger:
      begin
        Database := Session.DatabaseByName(URI.Database);
        if (not Assigned(Database)) then
          Result := nil
        else
          case (ClassIndex) of
            ciBaseTable,
            ciView,
            ciSystemView: Result := Database.TableByName(URI.Table);
            ciProcedure: Result := Database.ProcedureByName(URI.Param['object']);
            ciFunction: Result := Database.FunctionByName(URI.Param['object']);
            ciEvent: Result := Database.EventByName(URI.Param['object']);
            ciTrigger: Result := Database.TriggerByName(URI.Param['object']);
            else raise ERangeError.Create('Unknown ClassIndex for: ' + Address);
          end;
      end;
    ciProcesses: Result := Session.Processes;
    ciUsers: Result := Session.Users;
    ciVariables: Result := Session.Variables;
    ciQuickAccess: Result := Session.QuickAccess;
    else raise ERangeError.Create('Unknown ClassIndex for: ' + Address);
  end;

  URI.Free();
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
    MainAction('aDInsertRecord').Enabled := (Window.ActiveControl = ActiveDBGrid) and ActiveDBGrid.DataSource.DataSet.CanModify and (DataSet.State in [dsBrowse, dsEdit]) and (DataSet.FieldCount > 0) and Assigned(ActiveDBGrid) and (ActiveDBGrid.SelectedRows.Count < 1) and not InputDataSet;
    MainAction('aDDeleteRecord').Enabled := (Window.ActiveControl = ActiveDBGrid) and ActiveDBGrid.DataSource.DataSet.CanModify and (DataSet.State in [dsBrowse, dsEdit]) and not DataSet.IsEmpty() and not InputDataSet;

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
    MainAction('aDEditRecord').Enabled := Assigned(DBGrid.SelectedField) and DBGrid.DataSource.DataSet.CanModify and (View <> vIDE);
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

  if ((Sender = FObjectIDEGrid) and (CurrentClassIndex = ciTrigger) and (TMySQLDBGrid(Sender).DataSource.DataSet is TMySQLDataSet)) then
  begin
    Trigger := TSTrigger(CurrentData);

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

    if ((DBGrid.SelectedRows.CurrentRowSelected or (DBGrid.SelectedRows.Count = 0) and (DBGrid.SelectedFields.IndexOf(Column.Field) >= 0))
      and ((DBGrid.SelectedFields.Count = 0) or (DBGrid.SelectedFields.IndexOf(Column.Field) >= 0))) then
    begin // Cell is selected
      DBGrid.Canvas.Font.Color := clHighlightText;
      DBGrid.Canvas.Brush.Color := clHighlight;
    end
    else if ((gdFocused in State)
      or (gdSelected in State)) then
      if (DBGrid.Parent = PObjectIDE) then
      begin // Cell is focused, Grid is PObjectIDE child
        DBGrid.Canvas.Font.Color := clWindowText;
        DBGrid.Canvas.Brush.Color := clWindow;
      end
      else if (DBGrid.Focused) then
      begin // Cell is focused, Grid is focused
        DBGrid.Canvas.Font.Color := clHighlightText;
        DBGrid.Canvas.Brush.Color := clHighlight;
      end
      else
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

    if (DBGrid.Focused and (gdFocused in State)) then
      DBGrid.Canvas.DrawFocusRect(Rect);
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
  Assert(Sender is TMySQLDBGrid);

  DBGrid := TMySQLDBGrid(Sender);

  Wanted.Clear();

  // Debug 2017-01-05
  if (not DBGrid.DataSource.Enabled) then
    raise ERangeERror.Create('Visible: ' + BoolToStr(DBGrid.Visible, True) + #13#10
      + 'Assigned(DataSet): ' + BoolToStr(Assigned(DBGrid.DataSource.DataSet), True));

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
      if (FHexEditor.DataSize > 0) then
      begin
        FHexEditor.SelStart := FHexEditor.DataSize - 1;
        SendMessage(FHexEditor.Handle, WM_VSCROLL, SB_BOTTOM, 0);
      end;
      Window.ActiveControl := FHexEditor;
    end;
end;

procedure TFSession.DBGridDragOver(Sender, Source: TObject; X, Y: Integer;
  State: TDragState; var Accept: Boolean);
var
  DBGrid: TMySQLDBGrid;
  Effect: Longint;
  GridCoord: TGridCoord;
begin
  Assert(Sender is TDBGrid);

  DBGrid := TMySQLDBGrid(Sender);
  DBGrid.EndDrag(False);

  GridCoord := DBGrid.MouseCoord(X, Y);
  if ((State = dsDragEnter) and (GridCoord.X >= 0)) then
  begin
    if (DBGrid.SelText <> '') then
      OleCheck(DoDragDrop(TDBGridDropData.Create(DBGrid), Self, DROPEFFECT_COPY, Effect));
  end;

  Accept := False;
end;

function TFSession.DBGridDrop(const DBGrid: TMySQLDBGrid; const dataObj: IDataObject;
  grfKeyState: Longint; pt: TPoint; var dwEffect: Longint): HResult;
var
  Format: TFormatEtc;
  Medium: STGMEDIUM;
  Text: string;
begin
  Format.cfFormat := CF_UNICODETEXT;
  Format.ptd := nil;
  Format.dwAspect := DVASPECT_CONTENT;
  Format.lindex := -1;
  Format.tymed := TYMED_HGLOBAL;

  Result := dataObj.GetData(Format, Medium);

  if (Result = S_OK) then
  begin
    SetString(Text, PChar(GlobalLock(Medium.hGlobal)), GlobalSize(Medium.hGlobal) div SizeOf(Text[1]));

    DBGrid.DataSource.DataSet.Edit();
    DBGrid.PasteText(Text);

    if (not Assigned(Medium.unkForRelease)) then
      ReleaseStgMedium(Medium)
    else
      IUnknown(Medium.unkForRelease)._Release();

    dwEffect := DROPEFFECT_COPY;
  end;
end;

procedure TFSession.DBGridEditExecute(Sender: TObject);
begin
  Wanted.Clear();

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

procedure TFSession.DBGridHeaderMouseMove(Sender: TObject; Shift: TShiftState; X,
  Y: Integer);
{$IFDEF Debug}
var
  DBGrid: TMySQLDBGrid;
  GridCoord: TGridCoord;
  HDItem: THDItem;
  Msg: TMsg;
{$ENDIF}
begin
  {$IFDEF Debug}
  if ((Sender is THeaderControl) and (THeaderControl(Sender).Parent is TMySQLDBGrid)) then
  begin
    DBGrid := TMySQLDBGrid(THeaderControl(Sender).Parent);

    if ((X < 0) or (Y < 0)) then
    begin
      GridCoord.X := -1;
      GridCoord.Y := -1;
    end
    else
      GridCoord := DBGrid.MouseCoord(X, Y);

    HDItem.Mask := HDI_FORMAT;

    if (Assigned(MGridHeaderColumn) and ((GridCoord.X < 0) or (GridCoord.Y <> 0) or (MGridHeaderColumn <> DBGrid.Columns[GridCoord.X]))
      and BOOL(SendMessage(DBGrid.Header.Handle, HDM_GETITEM, MGridHeaderColumn.Index - DBGrid.LeftCol, LParam(@HDItem)))) then
    begin
      HDItem.fmt := HDItem.fmt and not HDF_SPLITBUTTON;
      SendMessage(DBGrid.Header.Handle, HDM_SETITEM, MGridHeaderColumn.Index - DBGrid.LeftCol, LParam(@HDItem));
      ReleaseCapture();
    end;

    if ((GridCoord.X >= 0) and (GridCoord.Y = 0)
      and not (DBGrid.Columns[GridCoord.X].Field.DataType in BinaryDataTypes)) then
    begin
      DBGrid.PopupMenu := MGridHeader;
      MGridHeaderColumn := DBGrid.Columns[GridCoord.X];
      if (BOOL(SendMessage(DBGrid.Header.Handle, HDM_GETITEM, MGridHeaderColumn.Index - DBGrid.LeftCol, LParam(@HDItem)))
        and (HDItem.fmt and HDF_SPLITBUTTON = 0)) then
      begin
        HDItem.fmt := HDItem.fmt or HDF_SPLITBUTTON;
        SendMessage(DBGrid.Header.Handle, HDM_SETITEM, MGridHeaderColumn.Index - DBGrid.LeftCol, LParam(@HDItem));
      end;
      if (not (PeekMessage(Msg, 0, 0, 0, PM_NOREMOVE) and (Msg.Message = WM_MOUSEMOVE) and (Msg.hwnd = DBGrid.Header.Handle) and (KeysToShiftState(Msg.wParam) = Shift))) then
        SetCapture(DBGrid.Header.Handle);
    end
    else
    begin
      if (Assigned(MGridHeaderColumn) and ((GridCoord.X < 0) or (GridCoord.Y <> 0) or (MGridHeaderColumn <> DBGrid.Columns[GridCoord.X]))
        and BOOL(SendMessage(DBGrid.Header.Handle, HDM_GETITEM, MGridHeaderColumn.Index - DBGrid.LeftCol, LParam(@HDItem)))) then
        ReleaseCapture();

      DBGrid.PopupMenu := MGrid;
      MGridHeaderColumn := nil;
    end;

    inherited;
  end;
  {$ENDIF}
end;

procedure TFSession.DBGridHeaderSplitButton(DBGrid: TMySQLDBGrid; Column: TColumn; Shift: TShiftState);
var
  Rect: TRect;
begin
  if (Shift = [ssLeft]) then
  begin
    if (not Assigned(PDBGridFilter)) then
    begin
      PDBGridFilter := TPDBGridFilter.Create(nil);
      PDBGridFilter.Color := DBGrid.Color;
      PDBGridFilter.Perform(CM_SYSFONTCHANGED, 0, 0);
      PDBGridFilter.Perform(UM_CHANGEPREFERENCES, 0, 0);
      PDBGridFilter.PopupParent := Window;
    end;

    if (Assigned(PDBGridFilter)) then
      if (PDBGridFilter.Visible and (PDBGridFilter.Column = Column)) then
        PDBGridFilter.Hide()
      else if (Header_GetItemDropDownRect(DBGrid.Header.Handle, Column.Index - DBGrid.LeftCol, Rect)) then
      begin
        if (PDBGridFilter.Visible) then
          PDBGridFilter.Hide();

        PDBGridFilter.Column := Column;
        PDBGridFilter.Left := DBGrid.ClientToScreen(Point(Rect.Left, 0)).X;
        PDBGridFilter.Top := DBGrid.ClientToScreen(Point(0, DBGrid.DefaultRowHeight)).Y;
        if (PDBGridFilter.Left + PDBGridFilter.Width > Screen.Width) then
          PDBGridFilter.Left := Screen.Width - PDBGridFilter.Width;
        PDBGridFilter.Visible := True;
        Header_SetFocusedItem(DBGrid.Header.Handle, Column.Index - DBGrid.LeftCol);
      end;
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

  SResult.Visible := PResult.Visible and (PQueryBuilder.Visible or PSynMemo.Visible);
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
  DBGrid: TMySQLDBGrid;
begin
  inherited;

  if (Sender is TMySQLDBGrid) then
  begin
    DBGrid := TMySQLDBGrid(Sender);

    if (not (ssLeft in Shift) and DBGrid.Dragging()) then
      DBGrid.EndDrag(False);
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

destructor TFSession.Destroy();
var
  DatabasesXML: IXMLNode;
  I: Integer;
  TempB: Boolean;
  URI: TUURI;
  View: TView;
begin
  Session.Account.Favorites.UnRegisterEventProc(FavoritesEvent);
  Session.UnRegisterEventProc(FormSessionEvent);
  Session.CreateDesktop := nil;

  if (TimeMonitor.CacheText <> '') then
    SendToDeveloper(TimeMonitor.CacheText);
  TimeMonitor.Free();

  FNavigatorChanging(nil, nil, TempB);

  if (Assigned(PObjectSearch)) then PObjectSearch.Free();
  if (Assigned(PDBGridFilter)) then PDBGridFilter.Free();

  Window.ActiveControl := nil;
  OnResize := nil;

  FNavigator.Items.BeginUpdate();
  FNavigator.Items.Clear();
  FNavigator.Items.EndUpdate();

  for View in [vEditor, vEditor2, vEditor3] do
    if (Assigned(SQLEditors[View])) then
    begin
      if ((SQLEditors[View].Filename <> '') and not SQLEditors[View].SynMemo.Modified) then
        Session.Account.Desktop.EditorContent[ToolbarTabByView[View]] := ''
      else
        Session.Account.Desktop.EditorContent[ToolbarTabByView[View]] := SQLEditors[View].SynMemo.Text;
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
  if (CurrentAddress <> '') then
  begin
    URI := TUURI.Create(CurrentAddress);
    URI.Param['file'] := Null;
    URI.Param['cp'] := Null;
    Session.Account.Desktop.Address := URI.Address;
    URI.Free();
  end;

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

  if (Assigned(QuickAccessListView)) then
  begin
    Session.Account.Desktop.QuickAccessMFUVisible := not (lgsCollapsed in QuickAccessListView.Groups[0].State);
    Session.Account.Desktop.QuickAccessMRUVisible := not (lgsCollapsed in QuickAccessListView.Groups[1].State);
    FreeListView(QuickAccessListView);
  end;
  if (Assigned(ServerListView)) then FreeListView(ServerListView);
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

  RevokeDragDrop(Handle);

  FreeAndNil(FilterMRU);
  FreeAndNil(Wanted);

  inherited;
end;

function TFSession.DragEnter(const dataObj: IDataObject;
  grfKeyState: Longint; pt: TPoint; var dwEffect: Longint): HResult;
var
  Format: FORMATETC;
begin
  Format.cfFormat := CF_UNICODETEXT;
  Format.ptd := nil;
  Format.dwAspect := DVASPECT_CONTENT;
  Format.lindex := -1;
  Format.tymed := TYMED_HGLOBAL;

  if (dataObj.QueryGetData(Format) <> S_OK) then
  begin
    dwEffect := DROPEFFECT_NONE;
    Result := S_OK;
  end
  else
    Result := DragOver(grfKeyState, pt, dwEffect);
end;

function TFSession.Dragging(const Sender: TObject): Boolean;
begin
  Result := LeftMousePressed and (Window.ActiveControl = FNavigator) and ((Window.ActiveControl as TTreeView).Selected <> MouseDownNode);
end;

function TFSession.DragLeave(): HResult;
begin
  if (Assigned(ActiveSynMemo) and ActiveSynMemo.AlwaysShowCaret) then
  begin
    ActiveSynMemo.SelStart := SynMemoBeforeDrag.SelStart;
    ActiveSynMemo.SelLength := SynMemoBeforeDrag.SelLength;
    ActiveSynMemo.AlwaysShowCaret := False;
  end;

  Result := S_OK;
end;

function TFSession.DragOver(grfKeyState: Longint; pt: TPoint;
  var dwEffect: Longint): HResult;
var
  ClientCoord: TPoint;
  Control: TControl;
  DBGrid: TMySQLDBGrid;
  GridCoord: TGridCoord;
  SynMemo: TSynMemo;
begin
  Control := FindDragTarget(pt, False);
  ClientCoord := Control.ScreenToClient(Point(pt.X, pt.Y));

  if (Control is TSynMemo) then
  begin
    SynMemo := TSynMemo(Control);

    if (not SynMemo.AlwaysShowCaret) then
    begin
      SynMemoBeforeDrag.SelStart := ActiveSynMemo.SelStart;
      SynMemoBeforeDrag.SelLength := ActiveSynMemo.SelLength;
      SynMemo.AlwaysShowCaret := True;
    end;

    if (not SynMemo.Gutter.Visible) then
      SynMemo.CaretX := (ClientCoord.X) div SynMemo.CharWidth + 1
    else
      SynMemo.CaretX := (ClientCoord.X - SynMemo.Gutter.RealGutterWidth(SynMemo.CharWidth)) div SynMemo.CharWidth + 1;
    SynMemo.CaretY := (ClientCoord.Y div SynMemo.LineHeight) + 1;
    Result := S_OK;
  end
  else if (Control is TMySQLDBGrid) then
  begin
    DBGrid := TMySQLDBGrid(Control);

    GridCoord := DBGrid.MouseCoord(ClientCoord.X, ClientCoord.Y);

    if ((GridCoord.X >= 0)
      and DBGrid.DataSource.DataSet.CanModify
      and not DBGrid.Columns[GridCoord.X].ReadOnly) then
      dwEffect := DROPEFFECT_COPY
    else
      dwEffect := DROPEFFECT_NONE;
    Result := S_OK;
  end
  else if ((Control = FFilter)
    or (Control = FObjectSearch)
    or (Control = FQuickSearch)) then
  begin
    dwEffect := DROPEFFECT_COPY;
    Result := S_OK;
  end
  else
  begin
    dwEffect := DROPEFFECT_NONE;
    Result := DragLeave();
  end;
end;

function TFSession.Drop(const dataObj: IDataObject;
  grfKeyState: Longint; pt: TPoint; var dwEffect: Longint): HResult;
var
  Control: TControl;
begin
  Control := FindDragTarget(pt, False);

  if (Control is TSynMemo) then
    SynMemoDrop(TSynMemo(Control), dataObj, grfKeyState, pt, dwEffect)
  else if (Control is TMySQLDBGrid) then
    DBGridDrop(TMySQLDBGrid(Control), dataObj, grfKeyState, pt, dwEffect)
  else if (Control = FFilter) then
    FFilterDrop(dataObj, grfKeyState, pt, dwEffect)
  else if (Control is TEdit) then
    EditDrop(TEdit(Control), dataObj, grfKeyState, pt, dwEffect)
  else
    dwEffect := DROPEFFECT_NONE;

  Result := DragLeave();
end;

function TFSession.EditDrop(const Edit: TEdit; const dataObj: IDataObject;
  grfKeyState: Longint; pt: TPoint; var dwEffect: Longint): HResult;
var
  Format: FORMATETC;
  Medium: STGMEDIUM;
  Text: string;
begin
  if (dwEffect <> DROPEFFECT_COPY) then
    Result := E_INVALIDARG
  else
  begin
    Text := '';

    Format.cfFormat := CF_UNICODETEXT;
    Format.ptd := nil;
    Format.dwAspect := DVASPECT_CONTENT;
    Format.lindex := -1;
    Format.tymed := TYMED_HGLOBAL;

    Result := dataObj.QueryGetData(Format);
    if (Result = S_OK) then
    begin
      OleCheck(dataObj.GetData(Format, Medium));
      SetString(Text, PChar(GlobalLock(Medium.hGlobal)), GlobalSize(Medium.hGlobal) div SizeOf(Text[1]));
      Edit.Text := Text;
    end;

    if (not Assigned(Medium.unkForRelease)) then
      ReleaseStgMedium(Medium)
    else
      IUnknown(Medium.unkForRelease)._Release();
  end;
end;

procedure TFSession.EndEditLabel(Sender: TObject);
begin
  aDCreate.ShortCut := VK_INSERT;
  aDDelete.ShortCut := VK_DELETE;
end;

procedure TFSession.FavoritesAdd(const Objects: TList);
var
  I: Integer;
begin
  FNavigator.Items.BeginUpdate();
  for I := 0 to Objects.Count - 1 do
    if (Session.Account.Favorites.IndexOf(AddressByData(Objects[I])) < 0) then
      Session.Account.Favorites.Add(AddressByData(Objects[I]));
  FNavigator.Items.EndUpdate();
end;

procedure TFSession.FavoritesEvent(const Favorites: TPAccount.TFavorites);
var
  FavoritesNode: TTreeNode;
  I: Integer;
  ImageIndex: Integer;
  Node: TTreeNode;
  Text: string;
  URI: TUURI;
begin
  FavoritesNode := FNavigator.Items.GetFirstNode();

  if (Assigned(FavoritesNode) and (FavoritesNode.ImageIndex = iiQuickAccess)) then
  begin
    FNavigator.Items.BeginUpdate();
    URI := TUURI.Create();

    FNavigatorRemoveFavorites();

    for I := 0 to Favorites.Count - 1 do
    begin
      URI.Address := Favorites[I].Address;
      if (URI.Param['objecttype'] = 'procedure') then
      begin
        ImageIndex := iiProcedure;
        Text := URI.Param['object'];
      end
      else if (URI.Param['objecttype'] = 'function') then
      begin
        ImageIndex := iiFunction;
        Text := URI.Param['object'];
      end
      else if (URI.Param['objecttype'] = 'event') then
      begin
        ImageIndex := iiEvent;
        Text := URI.Param['object'];
      end
      else if (URI.Param['objecttype'] = 'trigger') then
      begin
        ImageIndex := iiTrigger;
        Text := URI.Param['object'];
      end
      else if (URI.Param['objecttype'] = 'view') then
      begin
        ImageIndex := iiView;
        Text := URI.Table;
      end
      else if (URI.Param['objecttype'] = 'systemview') then
      begin
        ImageIndex := iiSystemView;
        Text := URI.Table;
      end
      else if (URI.Table <> '') then
      begin
        ImageIndex := iiBaseTable;
        Text := URI.Table;
      end
      else if (URI.Database <> '') then
      begin
        ImageIndex := iiDatabase;
        Text := URI.Database;
      end
      else
        raise ERangeError.Create(SRangeError);

      Node := FNavigator.Items.AddChild(FavoritesNode, Text);
      Node.ImageIndex := ImageIndex;
      Node.Data := Favorites[I];
    end;

    URI.Free();
    FavoritesNode.Expand(False);
    FNavigator.Items.EndUpdate();
  end;
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

function TFSession.FFilterDrop(const dataObj: IDataObject; grfKeyState: Longint;
  pt: TPoint; var dwEffect: Longint): HResult;
var
  Format: FORMATETC;
  HasFormat: Boolean;
  Medium: STGMEDIUM;
  Text: string;
begin
  if (dwEffect <> DROPEFFECT_COPY) then
    Result := E_INVALIDARG
  else
  begin
    Text := '';

    Format.ptd := nil;
    Format.dwAspect := DVASPECT_CONTENT;
    Format.lindex := -1;
    Format.tymed := TYMED_HGLOBAL;

    Format.cfFormat := CF_MYSQLSQLDATA;
    HasFormat := dataObj.QueryGetData(Format) = S_OK;
    if (HasFormat) then
    begin
      OleCheck(dataObj.GetData(Format, Medium));
      SetString(Text, PChar(GlobalLock(Medium.hGlobal)), GlobalSize(Medium.hGlobal) div SizeOf(Text[1]));
      FFilter.SelText := ReplaceStr(Text, #13#10, '');
    end;
    if (not HasFormat) then
    begin
      Format.cfFormat := CF_UNICODETEXT;
      HasFormat := dataObj.QueryGetData(Format) = S_OK;
      if (HasFormat) then
      begin
        OleCheck(dataObj.GetData(Format, Medium));
        SetString(Text, PChar(GlobalLock(Medium.hGlobal)), GlobalSize(Medium.hGlobal) div SizeOf(Text[1]));
        if (Pos(#13#10, Text) > 0) then
          Text := '(' + ReplaceStr(Text, #13#10, '),(') + ')'
        else if (Pos(#10, Text) > 0) then
          Text := '(' + ReplaceStr(Text, #10, '),(') + ')';
        FFilter.SelText := ReplaceStr(Text, #9, ',');
      end;
    end;

    if (not Assigned(Medium.unkForRelease)) then
      ReleaseStgMedium(Medium)
    else
      IUnknown(Medium.unkForRelease)._Release();

    if (not HasFormat) then
      Result := E_UNEXPECTED
    else
      Result := S_OK;
  end;
end;

procedure TFSession.FFilterDropDown(Sender: TObject);
var
  I: Integer;
begin
  FFilter.Items.Clear();
  for I := FilterMRU.Count - 1 downto 0 do
    FFilter.Items.Add(FilterMRU[I]);
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
  if (not (fsLoading in FrameState) and PExplorer.Visible and Visible) then
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
  MainAction('aHIndex').ShortCut := 0;
  MainAction('aHSQL').ShortCut := ShortCut(VK_F1, []);
end;

procedure TFSession.FLogExit(Sender: TObject);
begin
  MainAction('aHIndex').ShortCut := ShortCut(VK_F1, []);
  MainAction('aHSQL').ShortCut := 0;
end;

procedure TFSession.FLogUpdate();
var
  Profile: TProfile;
  Text: string;
begin
  if (MainAction('aVSQLLog').Checked) then
  begin
    CreateProfile(Profile);

    Text := Session.SQLMonitor.CacheText;

    ProfilingPoint(Profile, 1);

    FLog.Text := Text;

    ProfilingPoint(Profile, 2);

    PLogResize(nil);

    if (ProfilingTime(Profile) > 200) then
      SendToDeveloper(ProfilingReport(Profile));
    CloseProfile(Profile);
  end;
end;

procedure TFSession.FNavigatorChange(Sender: TObject; Node: TTreeNode);
begin
  FNavigatorMenuNode := Node;

  if (not (fsLoading in FrameState) and Assigned(Node) and not FNavigatorIgnoreChange) then
  begin
    KillTimer(Handle, tiNavigator);
    if (NavigatorElapse = 0) then
      FNavigatorChange2(Sender, Node)
    else if (NavigatorElapse > 0) then
    begin
      SetTimer(Self.Handle, tiNavigator, NavigatorElapse, nil);
      NavigatorElapse := 0;
    end;
  end;
end;

procedure TFSession.FNavigatorChange2(Sender: TObject; Node: TTreeNode);
var
  ScrollPos: record
    Horz: Integer;
    Vert: Integer;
  end;
  URI: TUURI;
begin
  KillTimer(Handle, tiNavigator);
  FNavigatorNodeAfterActivate := nil;

  if (not Assigned(Node) or (Node.ImageIndex = iiServer)) then
    URI := TUURI.Create(Session.Account.ExpandAddress('/'))
  else if (Node.ImageIndex = iiQuickAccess) then
  begin
    URI := TUURI.Create(Session.Account.ExpandAddress('/'));
    URI.Param['system'] := 'quick';
  end
  else if (TObject(Node.Data) is TPAccount.TFavorite) then
    URI := TUURI.Create(TPAccount.TFavorite(Node.Data).Address)
  else if (Node.ImageIndex = iiProcesses) then
  begin
    URI := TUURI.Create(Session.Account.ExpandAddress('/'));
    URI.Param['system'] := 'processes';
  end
  else if (Node.ImageIndex = iiUsers) then
  begin
    URI := TUURI.Create(Session.Account.ExpandAddress('/'));
    URI.Param['system'] := 'users';
  end
  else if (Node.ImageIndex = iiVariables) then
  begin
    URI := TUURI.Create(Session.Account.ExpandAddress('/'));
    URI.Param['system'] := 'variables';
  end
  else if (TObject(Node.Data) is TSItem) then
    URI := TUURI.Create(AddressByData(Node.Data))
  else if (Assigned(Node.Data)) then
    raise ERangeError.Create('ClassType: ' + TObject(Node.Data).ClassName)
  else
    raise ERangeError.Create(SRangeError);

  URI.Param['view'] := ViewToParam(View);

  if ((ParamToView(URI.Param['view']) in [vBrowser]) and not (Node.ImageIndex in [iiBaseTable, iiView, iiSystemView])) then
    URI.Param['view'] := Null;
  if ((ParamToView(URI.Param['view']) in [vIDE]) and not (Node.ImageIndex in [iiView, iiProcedure, iiFunction, iiTrigger, iiEvent])) then
    URI.Param['view'] := Null;
  if ((ParamToView(URI.Param['view']) in [vBuilder, vDiagram]) and not (Node.ImageIndex in [iiDatabase, iiSystemDatabase])) then
    URI.Param['view'] := Null;
  if ((ParamToView(URI.Param['view']) in [vEditor, vEditor2, vEditor3]) and not (Node.ImageIndex in [iiServer, iiDatabase, iiSystemDatabase])) then
    URI.Param['view'] := Null;
  if ((ParamToView(URI.Param['view']) in [vObjectSearch])) then
    URI.Param['view'] := Null;

  if ((URI.Param['view'] = Null) and (URI.Table <> '') and (URI.Param['objecttype'] <> 'trigger')) then
    URI.Param['view'] := ViewToParam(LastTableView);

  LockWindowUpdate(FNavigator.Handle);
  ScrollPos.Horz := GetScrollPos(FNavigator.Handle, SB_HORZ);
  ScrollPos.Vert := GetScrollPos(FNavigator.Handle, SB_VERT);
  CurrentAddress := URI.Address;
  SetScrollPos(FNavigator.Handle, SB_HORZ, ScrollPos.Horz, TRUE);
  SetScrollPos(FNavigator.Handle, SB_VERT, ScrollPos.Vert, TRUE);
  LockWindowUpdate(0);

  URI.Free();
end;

procedure TFSession.FNavigatorChanging(Sender: TObject; Node: TTreeNode;
  var AllowChange: Boolean);
begin
  AllowChange := Assigned(Node)
    and (Node.ImageIndex >= 0) and (Node.Text <> '')
    and not Dragging(Sender)
    and not (Node.ImageIndex in [iiKey, iiBaseField, iiVirtualField, iiSystemViewField, iiViewField, iiForeignKey])
    and not ((Node.ImageIndex = iiQuickAccess) and (Session.Connection.MySQLVersion < 50000));

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
  Index: Integer;
  Objects: string;
  SourceNode: TTreeNode;
  TargetNode: TTreeNode;
begin
  if (Sender is TTreeView) then
    TargetNode := TTreeView(Sender).GetNodeAt(X, Y)
  else
    TargetNode := FNavigatorNodeByAddress(CurrentAddress);

  if ((Sender is TTreeView)
    and ((TargetNode.ImageIndex = iiQuickAccess)
      or (TObject(TargetNode.Data) is TPAccount.TFavorite)
      or (Assigned(TargetNode.GetPrevVisible()) and (TObject(TargetNode.GetPrevVisible().Data) is TPAccount.TFavorite)) and (Y < TargetNode.DisplayRect(False).Top + GetSystemMetrics(SM_CYFIXEDFRAME)))) then
  begin
    if ((TargetNode.ImageIndex = iiQuickAccess) and (Y >= TargetNode.DisplayRect(False).Bottom - GetSystemMetrics(SM_CYFIXEDFRAME))) then
      Index := 0
    else if ((TObject(TargetNode.Data) is TPAccount.TFavorite) and (Y < TargetNode.DisplayRect(False).Top + GetSystemMetrics(SM_CYFIXEDFRAME))) then
      Index := TargetNode.Index
    else if ((TObject(TargetNode.Data) is TPAccount.TFavorite) and (Y >= TargetNode.DisplayRect(False).Bottom - GetSystemMetrics(SM_CYFIXEDFRAME))) then
      Index := TargetNode.Index + 1
    else
      Index := Session.Account.Favorites.Count;

    if (Source = FNavigator) then
    begin
      SourceNode := TFSession(TTreeView(Source).Owner).MouseDownNode;

      if (TObject(SourceNode.Data) is TPAccount.TFavorite) then
        if (Index < SourceNode.Index) then
          Session.Account.Favorites.Move(SourceNode.Index, Index)
        else
          Session.Account.Favorites.Move(SourceNode.Index, Index - 1)
      else
        Session.Account.Favorites.Insert(Index, AddressByData(SourceNode.Data));
    end
    else if ((Source is TListView) and (TListView(Source).Parent = PListView)) then
      for I := TListView(Source).Items.Count - 1 downto 0 do
        if (TListView(Source).Items[I].Selected) then
          Session.Account.Favorites.Insert(Index, AddressByData(TListView(Source).Items[I].Data));
  end
  else
  begin
    if ((Source is TTreeView) and (TTreeView(Source).Name = FNavigator.Name)) then
    begin
      SourceNode := TFSession(TTreeView(Source).Owner).MouseDownNode;

      case (SourceNode.ImageIndex) of
        iiDatabase:   Objects := Objects + 'Database='    + SourceNode.Text + #13#10;
        iiBaseTable:  Objects := Objects + 'Table='       + SourceNode.Text + #13#10;
        iiView:       Objects := Objects + 'View='        + SourceNode.Text + #13#10;
        iiProcedure:  Objects := Objects + 'Procedure='   + SourceNode.Text + #13#10;
        iiFunction:   Objects := Objects + 'Function='    + SourceNode.Text + #13#10;
        iiEvent:      Objects := Objects + 'Event='       + SourceNode.Text + #13#10;
        iiKey:        Objects := Objects + 'Index='       + SourceNode.Text + #13#10;
        iiBaseField,
        iiVirtualField,
        iiViewField:  Objects := Objects + 'Field='       + SourceNode.Text + #13#10;
        iiForeignKey: Objects := Objects + 'ForeignKey='  + SourceNode.Text + #13#10;
        iiTrigger:    Objects := Objects + 'Trigger='     + SourceNode.Text + #13#10;
        iiUser:       Objects := Objects + 'User='        + SourceNode.Text + #13#10;
      end;
      if (Objects <> '') then
        Objects := 'Address=' + AddressByData(SourceNode.Data) + #13#10 + Objects;
    end
    else if ((Source is TListView) and (TListView(Source).Parent.Name = PListView.Name)) then
    begin
      SourceNode := TFSession(TComponent(TListView(Source).Owner)).FNavigatorNodeByAddress(TFSession(TComponent(TListView(Source).Owner)).CurrentAddress);

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
            iiBaseField,
            iiVirtualField,
            iiViewField:  Objects := Objects + 'Field='      + TListView(Source).Items[I].Caption + #13#10;
            iiForeignKey: Objects := Objects + 'ForeignKey=' + TListView(Source).Items[I].Caption + #13#10;
            iiTrigger:    Objects := Objects + 'Trigger='    + TListView(Source).Items[I].Caption + #13#10;
            iiUser:       Objects := Objects + 'User='       + TListView(Source).Items[I].Caption + #13#10;
          end;
      if (Objects <> '') then
        Objects := 'Address=' + AddressByData(SourceNode.Data) + #13#10 + Objects;
    end;

    if (Objects <> '') then
      PasteExecute(TargetNode, Objects);
  end;
end;

procedure TFSession.FNavigatorDragOver(Sender, Source: TObject; X, Y: Integer;
  State: TDragState; var Accept: Boolean);

  procedure InvalidateNode(const Node: TTreeNode);
  begin
    if (Assigned(Node)) then
      InvalidateRect(TTreeView(Sender).Handle, Node.DisplayRect(False), not (csOpaque in ControlStyle));
  end;

var
  Rect: TRect;
  SourceItem: TListItem;
  SourceNode: TTreeNode;
  TargetNode: TTreeNode;
begin
  Assert(Sender is TTreeView);

  Accept := False;

  if (Source is TTreeView and (TTreeView(Source).Name = FNavigator.Name)) then
    SourceNode := TFSession(TTreeView(Source).Owner).MouseDownNode
  else
    SourceNode := nil;
  if ((Source is TListView) and (TListView(Source).Parent.Name = PListView.Name) and (TListView(Source).SelCount = 1)) then
    SourceItem := TListView(Source).Selected
  else
    SourceItem := nil;
  TargetNode := TTreeView(Sender).GetNodeAt(X, Y);

  if (Assigned(TargetNode)
    and ((TargetNode.ImageIndex = iiQuickAccess) and (Y >= TargetNode.DisplayRect(False).Bottom - GetSystemMetrics(SM_CYFIXEDFRAME))
      or (TObject(TargetNode.Data) is TPAccount.TFavorite)
      or Assigned(TargetNode.GetPrevVisible()) and (TObject(TargetNode.GetPrevVisible().Data) is TPAccount.TFavorite) and (Y < TargetNode.DisplayRect(False).Top + GetSystemMetrics(SM_CYFIXEDFRAME)))
    and (Assigned(SourceNode) and (TObject(SourceNode.Data) is TSItem) and (SourceNode.ImageIndex in [iiDatabase, iiBaseTable, iiView, iiProcedure, iiFunction, iiEvent, iiTrigger])
      or (Assigned(SourceItem) and (TObject(SourceItem.Data) is TSItem) and (SourceItem.ImageIndex in [iiDatabase, iiBaseTable, iiView, iiProcedure, iiFunction, iiEvent, iiTrigger]))
      or Assigned(SourceNode) and (TObject(SourceNode.Data) is TPAccount.TFavorite))) then
  begin
    Rect := TargetNode.DisplayRect(False);
    if (TargetNode.ImageIndex = iiQuickAccess) then
    begin
      if (State in [dsDragEnter, dsDragMove]) then
      begin
        TreeView_SetInsertMark(TTreeView(Sender).Handle, Integer(TargetNode.GetNextVisible().ItemId), FALSE);
        TreeView_SetItemState(TTreeView(Sender).Handle, TargetNode.ItemId, TreeView_GetItemState(TTreeView(Sender).Handle, TargetNode.ItemId, TVIF_STATE) and not TVIS_DROPHILITED, TVIF_STATE);
      end
      else
        TreeView_SetInsertMark(TTreeView(Sender).Handle, 0, TRUE);
      Accept := True;
    end
    else if (TargetNode.ImageIndex < 0) then
    begin
      if (State in [dsDragEnter, dsDragMove]) then
      begin
        TreeView_SetInsertMark(TTreeView(Sender).Handle, Integer(TargetNode.GetPrevVisible().ItemId), TRUE);
        TreeView_SetItemState(TTreeView(Sender).Handle, TargetNode.ItemId, TreeView_GetItemState(TTreeView(Sender).Handle, TargetNode.ItemId, TVIF_STATE) and not TVIS_DROPHILITED, TVIF_STATE);
      end
      else
        TreeView_SetInsertMark(TTreeView(Sender).Handle, 0, TRUE);
      Accept := True;
    end
    else if (Y < Rect.Top + GetSystemMetrics(SM_CYFIXEDFRAME)) then
    begin
      if (State in [dsDragEnter, dsDragMove]) then
      begin
        TreeView_SetInsertMark(TTreeView(Sender).Handle, Integer(TargetNode.ItemId), FALSE);
        TreeView_SetItemState(TTreeView(Sender).Handle, TargetNode.ItemId, TreeView_GetItemState(TTreeView(Sender).Handle, TargetNode.ItemId, TVIF_STATE) and not TVIS_DROPHILITED, TVIF_STATE);
      end
      else
        TreeView_SetInsertMark(TTreeView(Sender).Handle, 0, TRUE);
      Accept := True;
    end
    else
    if (Y >= Rect.Bottom - GetSystemMetrics(SM_CYFIXEDFRAME)) then
    begin
      if (State in [dsDragEnter, dsDragMove]) then
      begin
        TreeView_SetInsertMark(TTreeView(Sender).Handle, Integer(TargetNode.ItemId), TRUE);
        TreeView_SetItemState(TTreeView(Sender).Handle, TargetNode.ItemId, TreeView_GetItemState(TTreeView(Sender).Handle, TargetNode.ItemId, TVIF_STATE) and not TVIS_DROPHILITED, TVIF_STATE);
      end
      else
        TreeView_SetInsertMark(TTreeView(Sender).Handle, 0, TRUE);
      Accept := True;
    end
    else
    begin
      if (State in [dsDragEnter, dsDragMove]) then
      begin
        TreeView_SetInsertMark(TTreeView(Sender).Handle, 0, TRUE);
        TreeView_SetItemState(TTreeView(Sender).Handle, TargetNode.ItemId, TreeView_GetItemState(TTreeView(Sender).Handle, TargetNode.ItemId, TVIF_STATE) or TVIS_DROPHILITED, TVIF_STATE);
      end
      else
        TreeView_SetInsertMark(TTreeView(Sender).Handle, 0, TRUE);
      Accept := False;
    end;
  end
  else
  begin
    TreeView_SetInsertMark(TTreeView(Sender).Handle, 0, TRUE);

    if (Assigned(TargetNode) and (TargetNode.ImageIndex = iiQuickAccess)
      and (Assigned(SourceNode) and (TObject(SourceNode.Data) is TSItem) and (SourceNode.ImageIndex in [iiDatabase, iiBaseTable, iiView, iiProcedure, iiFunction, iiEvent, iiTrigger])
        or Assigned(SourceItem) and (TObject(SourceItem.Data) is TSItem) and (SourceItem.ImageIndex in [iiDatabase, iiBaseTable, iiView, iiProcedure, iiFunction, iiEvent, iiTrigger]))) then
      Accept := True
    else if (Assigned(TargetNode) and (TObject(TargetNode.Data) is TSItem)) then
      if (Assigned(SourceNode)) then
      begin
        if ((TargetNode <> SourceNode)
          and (TObject(SourceNode.Data) is TSItem)) then
          case (SourceNode.ImageIndex) of
            iiDatabase:
              Accept := (TObject(TargetNode.Data) is TSSession)
                and (TargetNode <> SourceNode.Parent);
            iiBaseTable,
            iiView,
            iiProcedure,
            iiFunction:
              Accept := (TObject(TargetNode.Data) is TSDatabase)
                and (TargetNode <> SourceNode.Parent);
            iiBaseField,
            iiVirtualField:
              Accept := (TObject(TargetNode.Data) is TSBaseTable)
                and (TargetNode <> SourceNode.Parent);
          end;
      end
      else if (Assigned(SourceItem)) then
      begin
        if (TObject(SourceItem.Data) is TSItem) then
          case (SourceItem.ImageIndex) of
            iiDatabase:
              Accept := (TObject(TargetNode.Data) is TSSession)
                and (TargetNode <> TFSession(TListView(Source).Owner).FNavigatorNodeByAddress(TFSession(TListView(Source).Owner).CurrentAddress));
            iiBaseTable,
            iiView,
            iiProcedure,
            iiFunction:
              Accept := (TObject(TargetNode.Data) is TSDatabase)
                and (TargetNode <> TFSession(TListView(Source).Owner).FNavigatorNodeByAddress(TFSession(TListView(Source).Owner).CurrentAddress));
            iiBaseField,
            iiVirtualField:
              Accept := (TObject(TargetNode.Data) is TSBaseTable)
                and (TargetNode <> TFSession(TListView(Source).Owner).FNavigatorNodeByAddress(TFSession(TListView(Source).Owner).CurrentAddress));
          end;
      end;
  end;

  if ((State in [dsDragEnter, dsDragMove]) and Assigned(TargetNode) and (TargetNode.ImageIndex < 0)) then
    TreeView_SetItemState(TTreeView(Sender).Handle, TargetNode.ItemId, TreeView_GetItemState(TTreeView(Sender).Handle, TargetNode.ItemId, TVIF_STATE) and not TVIS_DROPHILITED, TVIF_STATE);
end;

procedure TFSession.FNavigatorEdited(Sender: TObject; Node: TTreeNode; var S: string);
begin
  if (not RenameSItem(Node.Data, S)) then
    S := Node.Text;
end;

procedure TFSession.FNavigatorEditing(Sender: TObject; Node: TTreeNode;
  var AllowEdit: Boolean);
begin
  AllowEdit := (Node.ImageIndex = iiDatabase) and (Session.Connection.MySQLVersion >= 50107) or (Node.ImageIndex = iiForeignKey) and (Session.Connection.MySQLVersion >= 40013) or (Node.ImageIndex in [iiBaseTable, iiView, iiEvent, iiTrigger, iiBaseField, iiVirtualField]);
end;

procedure TFSession.FNavigatorEmptyExecute(Sender: TObject);
var
  Database: TSDatabase;
  Field: TSBaseField;
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

      if ((View = vBrowser) and (FNavigatorMenuNode.Data = Table)) then
        Wanted.Update := UpdateAfterAddressChanged;
    end;
  end
  else if (FocusedSItem is TSBaseField) then
  begin
    Field := TSBaseField(FocusedSItem);
    Table := Field.Table;
    if (Assigned(Field) and (MsgBox(Preferences.LoadStr(376, Field.Name), Preferences.LoadStr(101), MB_YESNOCANCEL + MB_ICONQUESTION) = IDYES)) then
    begin
      List := TList.Create();
      List.Add(Field);
      Table.EmptyFields(List);
      List.Free();

      if ((View = vBrowser) and (TSBaseField(FNavigatorMenuNode.Data).Table = Table)) then
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
  // Debug 2017-01-22
  if (not Assigned(Node)) then
    raise ERangeError.Create(SRangeError);

  if (Node.ImageIndex = iiQuickAccess) then
    AllowExpansion := True
  else if (Node.HasChildren) then
  begin
    Database := nil;
    Table := nil;
    case (Node.ImageIndex) of
      iiDatabase,
      iiSystemDatabase:
        begin
          Database := TSDatabase(Node.Data);
          AllowExpansion := Database.Valid;
          if (not AllowExpansion) then
            Wanted.Update := Database.Update;
        end;
      iiBaseTable,
      iiView,
      iiSystemView:
        begin
          Table := TSTable(Node.Data);
          AllowExpansion := Table.Valid;
          if (not AllowExpansion) then
            Wanted.Update := Table.Update;
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
  Node := FNavigator.Items.getFirstNode();
  while (Assigned(Node) and (Node.ImageIndex <> iiServer)) do
    Node := Node.getNextSibling();
  if (Assigned(Node) and (Node.ImageIndex = iiServer)) then
  begin
    Node := Node.getFirstChild();
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
end;

procedure TFSession.FNavigatorKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  if (not TTreeView(Sender).IsEditing()) then
  begin
    if ((Key = Ord('C')) and (Shift = [ssCtrl]) or (Key = VK_INSERT) and (Shift = [ssCtrl])) then
      begin aECopyExecute(Sender); Key := 0; end
    else if ((Key = Ord('V')) and (Shift = [ssCtrl]) or (Key = VK_INSERT) and (Shift = [ssShift])) then
      begin aEPasteExecute(Sender); Key := 0; end
    else if ((Key = VK_RETURN) and CheckWin32Version(6, 1)) then
      FNavigatorChange2(Sender, FNavigator.Selected)
    else if (not (Key in [VK_SHIFT, VK_CONTROL])) then
      if (not CheckWin32Version(6, 1)) then
        NavigatorElapse := 500
      else
        FNavigatorIgnoreChange := not (Key in [VK_SHIFT, VK_CONTROL, VK_MENU ]);
  end;

  FNavigatorKeyDownNode := FNavigator.Selected;
end;

procedure TFSession.FNavigatorKeyPress(Sender: TObject; var Key: Char);
begin
  if (Key = #3) then
    Key := #0 // Why is there a Beep on <Ctrl+C> without this?
  else if ((Key = #13) and CheckWin32Version(6, 1)) then
    Key := #0;
end;

procedure TFSession.FNavigatorKeyUp(Sender: TObject; var Key: Word;
  Shift: TShiftState);
var
  AllowChange: Boolean;
  Node: TTreeNode;
begin
  if ((Shift = []) and (Key = VK_UP) and (FNavigator.Selected = FNavigatorKeyDownNode)) then
  begin
    Node := FNavigator.Selected;
    AllowChange := False;
    while (Assigned(Node) and not AllowChange) do
    begin
      Node := Node.GetPrevVisible();
      AllowChange := Assigned(Node);
      if (AllowChange) then
        FNavigatorChanging(FNavigator, Node, AllowChange);
    end;
    if (AllowChange) then
      FNavigator.Selected := Node;
  end
  else if ((Shift = []) and (Key = VK_DOWN) and (FNavigator.Selected = FNavigatorKeyDownNode)) then
  begin
    Node := FNavigator.Selected;
    AllowChange := False;
    while (Assigned(Node) and not AllowChange) do
    begin
      Node := Node.GetNextVisible();
      AllowChange := Assigned(Node);
      if (AllowChange) then
        FNavigatorChanging(FNavigator, Node, AllowChange);
    end;
    if (AllowChange) then
      FNavigator.Selected := Node;
  end;

  FNavigatorIgnoreChange := False;
end;

procedure TFSession.FNavigatorMouseMove(Sender: TObject; Shift: TShiftState; X,
  Y: Integer);
var
  Node: TTreeNode;
begin
  if (not Dragging(FNavigator)) then
  begin
    Node := FNavigator.GetNodeAt(X, Y);

    if (Assigned(Node) and (Node.ImageIndex < 0)) then
    begin
      if (FNavigator.HotTrack) then
      begin
        FNavigator.HotTrack := False;
        FNavigatorHotTrackDisabled := True;
      end;
    end
    else if (FNavigatorHotTrackDisabled) then
    begin
      FNavigator.HotTrack := True;
      FNavigatorHotTrackDisabled := False;
    end;

    if (Assigned(Node) and (Node.ImageIndex < 0)) then
    begin
      if (FNavigator.DragMode = dmAutomatic) then
      begin
        FNavigator.DragMode := dmManual;
        FNavigatorDragDisabled := True;
      end;
    end
    else if (FNavigatorDragDisabled) then
    begin
      FNavigator.DragMode := dmAutomatic;
      FNavigatorDragDisabled := False;
    end;
  end;
end;

function TFSession.FNavigatorNodeByAddress(const Address: string): TTreeNode;
var
  AllowExpansion: Boolean;
  Child: TTreeNode;
  DatabaseNode: TTreeNode;
  ServerNode: TTreeNode;
  TableName: string;
  TableNode: TTreeNode;
  URI: TUURI;
begin
  Result := nil;

  if (Address <> '') then
  begin
    URI := TUURI.Create(Address);

    ServerNode := FNavigator.Items.getFirstNode();
    while (Assigned(ServerNode) and (ServerNode.ImageIndex <> iiServer)) do
      ServerNode := ServerNode.getNextSibling();

    if (URI.Param['system'] = 'quick') then
      Result := FNavigator.Items.getFirstNode()
    else if (URI.Param['system'] <> Null) then
    begin
      Child := ServerNode.getFirstChild();
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
      Child := ServerNode.getFirstChild(); DatabaseNode := nil;
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
        if (URI.Table <> '') then
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
          if ((URI.Param['objecttype'] <> 'trigger') or (URI.Param['object'] = Null)) then
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
      Result := ServerNode;

    URI.Free();
  end;
end;

procedure TFSession.FNavigatorRemoveFavorites();
var
  QuickAccessNode: TTreeNode;
begin
  if (Assigned(FNavigator.Items.GetFirstNode()) and (FNavigator.Items.GetFirstNode().ImageIndex = iiQuickAccess)) then
  begin
    QuickAccessNode := FNavigator.Items.GetFirstNode();

    QuickAccessNode.DeleteChildren();
  end;
end;

procedure TFSession.FNavigatorUpdate(const Event: TSSession.TEvent);
var
  LastChild: TTreeNode;
  Profile: TProfile;

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

  function Compare(const Item1, Item2: TTreeNode): Integer;
  const
    ImageIndexSort = Chr(iiProcesses) + Chr(iiUsers) + Chr(iiVariables);
  begin
    if (GroupIDByImageIndex(Item1.ImageIndex) <> GroupIDByImageIndex(Item2.ImageIndex)) then
      Result := Sign(GroupIDByImageIndex(Item1.ImageIndex) - GroupIDByImageIndex(Item2.ImageIndex))
    else if (GroupIDByImageIndex(Item1.ImageIndex) = giSystemTools) then
      Result := Sign(Pos(Chr(Item1.ImageIndex), ImageIndexSort) - Pos(Chr(Item2.ImageIndex), ImageIndexSort))
    else if ((TObject(Item1.Data) is TSItem) and (TObject(Item2.Data) is TSItem)) then
      Result := Sign(TSItem(Item1.Data).Index - TSItem(Item2.Data).Index)
    else
      raise ERangeError.Create('Item1.ImageIndex: ' + IntToStr(Item1.ImageIndex) + #13#10
        + 'Item1.Text: ' + Item1.Text + #13#10
        + 'Item2.ImageIndex: ' + IntToStr(Item2.ImageIndex) + #13#10
        + 'Item2.Text: ' + Item2.Text);
  end;

  function InsertOrUpdateChild(const Parent: TTreeNode; const Data: TObject): TTreeNode;
  var
    Added: Boolean;
    Node: TTreeNode;
    Text: string;
  begin
    ProfilingPoint(Profile, 3);

    Node := TTreeNode.Create(Parent.Owner);
    Node.Data := Data;
    Node.ImageIndex := ImageIndexByData(Data);
    Node.Text := TSItem(Data).Caption;

    if (Assigned(LastChild) and Assigned(LastChild.GetNextSibling()) and (Compare(LastChild.GetNextSibling(), Node) = 0)) then
      Result := LastChild.GetNextSibling()
    else
    begin
      ProfilingPoint(Profile, 4);

      Result := Parent.getFirstChild();
      while (Assigned(Result)) do
      begin
        if (Compare(Result, Node) >= 0) then
          break;
        Result := Result.getNextSibling();
      end;

      ProfilingPoint(Profile, 5);
    end;

    Node.Free();

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

    if (not Assigned(Result)) then
    begin
      Result := FNavigator.Items.AddChild(Parent, Text);
      Added := True;
    end
    else if (Result.Data <> Data) then
    begin
      Result := FNavigator.Items.Insert(Result, Text);
      Added := True;
    end
    else
      Added := False;
    Result.Data := Data;
    Result.ImageIndex := ImageIndexByData(Data);
    Result.Text := Text;
    if (Added and (Result.ImageIndex in [iiDatabase, iiSystemDatabase, iiBaseTable, iiView, iiSystemView])) then
      Result.HasChildren := True;

    if (Assigned(Result)) then
      SetNodeBoldState(Result, (Result.ImageIndex = iiKey) and TSKey(Result.Data).PrimaryKey or (Result.ImageIndex in [iiBaseField, iiVirtualField]) and TSTableField(Result.Data).InPrimaryKey);

    ProfilingPoint(Profile, 6);
  end;

  function AddChild(const Parent: TTreeNode; const Data: TObject): TTreeNode;
  var
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

    ProfilingPoint(Profile, 7);
    Result := FNavigator.Items.AddChild(Parent, Text);

    ProfilingPoint(Profile, 8);

    Result.Data := Data;
    Result.ImageIndex := ImageIndexByData(Data);
    Result.Text := Text;
    if (Result.ImageIndex in [iiDatabase, iiSystemDatabase, iiBaseTable, iiView, iiSystemView]) then
      Result.HasChildren := True;

    ProfilingPoint(Profile, 9);

    if (Assigned(Result)) then
      SetNodeBoldState(Result, (Result.ImageIndex = iiKey) and TSKey(Result.Data).PrimaryKey or (Result.ImageIndex in [iiBaseField, iiVirtualField]) and TSTableField(Result.Data).InPrimaryKey);

    ProfilingPoint(Profile, 10);
  end;

  procedure DeleteChild(const Child: TTreeNode);
  var
    Node: TTreeNode;
  begin
    Node := FNavigatorNodeToExpand;
    while (Assigned(Node)) do
    begin
      if (Child = Node) then
        FNavigatorNodeToExpand := nil;
      Node := Node.Parent;
    end;

    Child.Data := nil;
    Child.Delete();
  end;

  procedure UpdateGroup(const Parent: TTreeNode; const GroupID: Integer; const Items: TSItems);
  var
    Add: Boolean;
    Child: TTreeNode;
    Destination: TTreeNode;
    Node: TTreeNode;
    I: Integer;
  begin
    LastChild := nil;

    case (Event.EventType) of
      etItemsValid:
        begin
          ProfilingPoint(Profile, 1);

          Child := Parent.getFirstChild();
          while (Assigned(Child)) do
            if ((GroupIDByImageIndex(Child.ImageIndex) <> GroupID) or (Items.IndexOf(Child.Data) >= 0)) then
              Child := Child.getNextSibling()
            else
            begin
              if ((Child = FNavigatorNodeToExpand) or (Child.Parent = FNavigatorNodeToExpand)) then
                FNavigatorNodeToExpand := nil;
              Node := Child;
              Child := Child.getNextSibling();
              DeleteChild(Node);
            end;

          ProfilingPoint(Profile, 1);

          Add := not Assigned(Parent.getFirstChild());
          for I := 0 to Items.Count - 1 do
            if (not (Items is TSTriggers) or (TSTriggers(Items)[I].Table = Parent.Data)) then
            begin
              // Debug 2017-01-20
              if (not Assigned(Items[I])) then
                raise ERangeError.Create('Items = Event.Items: ' + BoolToStr(Items = Event.Items, True) + #13#10
                  + 'ClassType: ' + Event.Items.ClassName);

              if (not Add) then
                LastChild := InsertOrUpdateChild(Parent, Items[I])
              else
                LastChild := AddChild(Parent, Items[I]);
            end;

          ProfilingPoint(Profile, 11);
        end;
      etItemCreated:
        if (GroupIDByImageIndex(ImageIndexByData(Event.Item)) = GroupID) then
          if (not (Event.Item is TSTrigger) or (Parent.Count > 0)) then
            InsertOrUpdateChild(Parent, Event.Item);
      etItemRenamed:
        if (GroupIDByImageIndex(ImageIndexByData(Event.Item)) = GroupID) then
        begin
          Child := Parent.getFirstChild();
          while (Assigned(Child) and (Child.Data <> Event.Item)) do
            Child := Child.getNextSibling();

          if (not Assigned(Child)) then
            raise ERangeError.Create('Node not found: ' + Event.Item.Name + ' (ClassType: ' + Event.Item.ClassName + ')');

          Child.Text := Event.Item.Caption;

          Destination := Parent.getFirstChild();

          while (Assigned(Destination) and ((Destination = Child) or (Compare(Destination, Child) < 0))) do
            Destination := Destination.getNextSibling();

          if (Assigned(Destination)) then
            Child.MoveTo(Destination, naInsert)
          else
            Child.MoveTo(Parent, naAddChild);

          if (Assigned(Child)) then
            SetNodeBoldState(Child, (Child.ImageIndex = iiKey) and TSKey(Child.Data).PrimaryKey or (Child.ImageIndex in [iiBaseField, iiVirtualField]) and TSTableField(Child.Data).InPrimaryKey);
        end;
      etItemDeleted:
        if (GroupIDByImageIndex(ImageIndexByData(Event.Item)) = GroupID) then
        begin
          Child := FNavigatorNodeByAddress(AddressByData(Event.Item));

          if (Assigned(Child)) then
          begin
            Node := FNavigatorNodeToExpand;
            while (Assigned(Node)) do
              if (Node = Child) then
                FNavigatorNodeToExpand := nil
              else
                Node := Node.Parent;

            if (Child <> FNavigator.Selected) then
              Node := nil
            else
            begin
              Node := Child.getNextSibling();
              if (not Assigned(Node)) then
                Node := Child.getPrevSibling();
              if (not Assigned(Node) or (Node.ImageIndex in [iiKey, iiBaseField, iiSystemViewField, iiVirtualField, iiViewField, iiSystemViewField, iiForeignKey])) then
                Node := Child.Parent;
            end;

            DeleteChild(Child);

            if (Assigned(Node)) then
              Wanted.Address := AddressByData(Node.Data);
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
  S: string;
  Table: TSTable;
begin
  CreateProfile(Profile);
  OldSelected := FNavigator.Selected;

  ChangingEvent := FNavigator.OnChanging; FNavigator.OnChanging := nil;
  ChangeEvent := FNavigator.OnChange; FNavigator.OnChange := nil;

  if (Event.Sender is TSSession) then
  begin
    Node := FNavigator.Items.getFirstNode();
    while (Assigned(Node) and (Node.ImageIndex <> iiServer)) do
      Node := Node.getNextSibling();

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

    Node := FNavigator.Items.getFirstNode();
    while (Assigned(Node) and (Node.ImageIndex <> iiServer)) do
      Node := Node.getNextSibling();
    Node := Node.getFirstChild();
    while (Assigned(Node) and (Node.Data <> Database)) do
      Node := Node.getNextSibling();

    if (Assigned(Node) and (not Node.HasChildren or Assigned(Node.getFirstChild()) or (Node = FNavigatorNodeToExpand))) then
    begin
      FNavigator.Items.BeginUpdate();

      Expanded := Node.Expanded;

      if (Expanded or (Node = FNavigatorNodeToExpand)) then
      begin
        if (Event.Items is TSTables) then
          UpdateGroup(Node, giTables, Event.Items)
        else if (Event.Items is TSRoutines) then
          UpdateGroup(Node, giRoutines, Event.Items)
        else if (Event.Items is TSEvents) then
          UpdateGroup(Node, giEvents, Event.Items);
      end;

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

    Node := FNavigator.Items.getFirstNode();
    while (Assigned(Node) and (Node.ImageIndex <> iiServer)) do
      Node := Node.getNextSibling();
    Node := Node.getFirstChild();
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
        begin
          if (Table is TSBaseTable) then
            UpdateGroup(Node, giKeys, TSBaseTable(Table).Keys);
          UpdateGroup(Node, giFields, Table.Fields);
          if ((Table is TSBaseTable) and Assigned(TSBaseTable(Table).ForeignKeys)) then
            UpdateGroup(Node, giForeignKeys, TSBaseTable(Table).ForeignKeys);
          if ((Table is TSBaseTable) and Assigned(Table.Database.Triggers)) then
            UpdateGroup(Node, giTriggers, Table.Database.Triggers);
        end;

        Node.HasChildren := Assigned(Node.getFirstChild());
        Node.Expanded := Expanded;

        FNavigator.Items.EndUpdate();
      end;
    end;
  end;

  ProfilingPoint(Profile, 12);

  FNavigator.OnChanging := ChangingEvent;
  FNavigator.OnChange := ChangeEvent;

  ProfilingPoint(Profile, 13);

  if (FNavigator.Selected <> OldSelected) then
    SetTimer(Self.Handle, tiNavigator, 1, nil); // We're inside a Monitor call. So we can't call FNavigatorNodeChange2 directly

  ProfilingPoint(Profile, 14);

  if (Assigned(FNavigatorNodeToExpand) and (FNavigatorNodeToExpand.Count > 0)) then
  begin
    ExpandingEvent := FNavigator.OnExpanding;
    FNavigator.OnExpanding := nil;
    FNavigatorNodeToExpand.Expand(False);
    FNavigatorNodeToExpand := nil;
    FNavigator.OnExpanding := ExpandingEvent;
  end;

  if (ProfilingTime(Profile) > 1000) then
  begin
    S := 'FNavigatorUpdate - '
      + 'EventType: ' + IntToStr(Ord(Event.EventType)) + ', '
      + 'Sender: ' + Event.Sender.ClassName + ', ';
    if (Assigned(Event.Items)) then
      S := S
        + 'Items: ' + Event.Items.ClassName + ', '
        + 'Count: ' + IntToStr(Event.Items.Count) + ', ';
    if (Event.Item is TSTable) then
      S := S
        + 'FieldCount: ' + IntToStr(TSTable(Event.Item).Fields.Count) + ', ';
    S := S + ProfilingReport(Profile) + #13#10;
    TimeMonitor.Append(S, ttDebug);
  end;
  CloseProfile(Profile);
end;

procedure TFSession.FNavigatorChanged(Sender: TObject; const Node: TTreeNode);
begin
  if (not Assigned(Node) or not (TObject(Node.Data) is TPAccount.TFavorite)) then
  begin
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
    MainAction('aECopy').Enabled := Assigned(Node) and (Node.ImageIndex in [iiDatabase, iiSystemDatabase, iiBaseTable, iiView, iiSystemView, iiProcedure, iiFunction, iiEvent, iiTrigger, iiBaseField, iiSystemViewField, iiVirtualField, iiViewField, iiSystemViewField]);
    MainAction('aEPaste').Enabled := Assigned(Node) and ((Node.ImageIndex = iiServer) and IsClipboardFormatAvailable(CF_MYSQLSERVER) or (Node.ImageIndex = iiDatabase) and IsClipboardFormatAvailable(CF_MYSQLDATABASE) or (Node.ImageIndex = iiBaseTable) and IsClipboardFormatAvailable(CF_MYSQLTABLE) or (Node.ImageIndex = iiUsers) and IsClipboardFormatAvailable(CF_MYSQLUSERS));
    MainAction('aERename').Enabled := Assigned(Node) and ((Node.ImageIndex = iiForeignKey) and (Session.Connection.MySQLVersion >= 40013) or (Node.ImageIndex in [iiBaseTable, iiView, iiEvent, iiTrigger, iiBaseField, iiVirtualField]));
    MainAction('aDCreateDatabase').Enabled := Assigned(Node) and (Node.ImageIndex in [iiServer]) and (not Assigned(Session.UserRights) or Session.UserRights.RCreate);
    MainAction('aDCreateTable').Enabled := Assigned(Node) and (Node.ImageIndex = iiDatabase);
    MainAction('aDCreateView').Enabled := Assigned(Node) and (Node.ImageIndex = iiDatabase) and (Session.Connection.MySQLVersion >= 50001);
    MainAction('aDCreateProcedure').Enabled := Assigned(Node) and (Node.ImageIndex = iiDatabase) and Assigned(TSDatabase(Node.Data).Routines);
    MainAction('aDCreateFunction').Enabled := MainAction('aDCreateProcedure').Enabled;
    MainAction('aDCreateEvent').Enabled := Assigned(Node) and (Node.ImageIndex = iiDatabase) and Assigned(TSDatabase(Node.Data).Events);
    MainAction('aDCreateTrigger').Enabled := Assigned(Node) and (Node.ImageIndex = iiBaseTable) and Assigned(TSBaseTable(Node.Data).Database.Triggers);
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
    MainAction('aDDeleteField').Enabled := Assigned(Node) and (Node.ImageIndex in [iiBaseField, iiVirtualField]) and (TObject(Node.Data) is TSTableField) and (TSTableField(Node.Data).Fields.Count > 1);
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
    MainAction('aDEditField').Enabled := Assigned(Node) and (Node.ImageIndex in [iiBaseField, iiVirtualField]);
    MainAction('aDEditForeignKey').Enabled := Assigned(Node) and (Node.ImageIndex = iiForeignKey);
    MainAction('aDEditTrigger').Enabled := Assigned(Node) and (Node.ImageIndex = iiTrigger);
    MainAction('aDEmpty').Enabled := Assigned(Node) and ((Node.ImageIndex = iiDatabase) or (Node.ImageIndex = iiBaseTable) or ((Node.ImageIndex in [iiBaseField]) and TSTableField(Node.Data).NullAllowed));

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
        iiBaseField,
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

    FNavigator.ReadOnly := not MainAction('aERename').Enabled;
  end;
end;

procedure TFSession.FObjectSearchChange(Sender: TObject);
begin
  FObjectSearchStart.Enabled := Trim(FObjectSearch.Text) <> '';
end;

procedure TFSession.FObjectSearchExit(Sender: TObject);
begin
  if (Assigned(PObjectSearch)) then
  begin
    PObjectSearch.Hide();
    FObjectSearch.SelectAll();
  end;
end;

procedure TFSession.FObjectSearchKeyPress(Sender: TObject; var Key: Char);
begin
  if (Key = #13) then
  begin
    if (Trim(FObjectSearch.Text) = '') then
      MessageBeep(MB_ICONERROR)
    else
      FObjectSearchStart.Click();
    Key := #0;
  end
  else if (Key = #27) then
  begin
    if (Assigned(PObjectSearch)) then
    begin
      PObjectSearch.Hide();
      FObjectSearch.SelectAll();
    end;
    Key := #0;
  end
  else
    inherited;
end;

procedure TFSession.FObjectSearchMouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
var
  Node: TTreeNode;
begin
  if (Shift = [ssLeft]) then
  begin
    if (not Assigned(PObjectSearch)) then
    begin
      PObjectSearch := TPObjectSearch.Create(nil);
      PObjectSearch.Color := FObjectSearch.Color;
      PObjectSearch.Perform(CM_SYSFONTCHANGED, 0, 0);
      PObjectSearch.Perform(UM_CHANGEPREFERENCES, 0, 0);
      PObjectSearch.PopupParent := Window;

      PObjectSearch.Session := Session;
    end;

    if (Assigned(PObjectSearch) and not PObjectSearch.Visible) then
    begin
      Node := FNavigatorNodeByAddress(CurrentAddress);
      while (Assigned(Node) and not (Node.ImageIndex in [iiServer, iiDatabase, iiSystemDatabase, iiBaseTable, iiView, iiSystemView])) do
        Node := Node.Parent;
      if (not Assigned(Node)) then
        PObjectSearch.Location := Session
      else
        PObjectSearch.Location := Node.Data;
      PObjectSearch.Left := ClientToScreen(Point(FObjectSearch.Left + FObjectSearch.Width - PObjectSearch.Width, 0)).X;
      PObjectSearch.Top := ClientToScreen(Point(0, FObjectSearch.Top + FObjectSearch.Height)).Y;
      PObjectSearch.Visible := not (CurrentClassIndex in [ciProcesses, ciUsers, ciVariables]);
    end;
  end;
end;

procedure TFSession.FObjectSearchStartClick(Sender: TObject);
var
  ScrollPos: record
    Horz: Integer;
    Vert: Integer;
  end;
  URI: TUURI;
begin
  Assert(Trim(FObjectSearch.Text) <> '');

  if (Assigned(ObjectSearchListView)) then
    FreeAndNil(ObjectSearchListView);
  if (Assigned(ObjectSearch)) then
    FreeAndNil(ObjectSearch);

  PObjectSearch.Hide();
  FObjectSearch.SelectAll();

  URI := TUURI.Create(CurrentAddress);
  URI.Param['view'] := 'objectsearch';
  if (URI.Param['system'] = Null) then
  begin
    if (not (PObjectSearch.Location is TSDatabase) and not (PObjectSearch.Location is TSTable)) then
      URI.Database := '';
    if (not (PObjectSearch.Location is TSTable)) then
      URI.Table := '';
  end;
  URI.Param['objecttype'] := Null;
  URI.Param['object'] := Null;
  URI.Param['filter'] := Null;
  URI.Param['search'] := Null;
  URI.Param['offset'] := Null;
  URI.Param['file'] := Null;
  URI.Param['cp'] := Null;
  URI.Param['text'] := Trim(FObjectSearch.Text);
  if (PObjectSearch.Databases) then
    URI.Param['databases'] := '1'
  else
    URI.Param['databases'] := Null;
  if (PObjectSearch.Databases) then
    URI.Param['tables'] := '1'
  else
    URI.Param['tables'] := Null;
  if (PObjectSearch.Tables) then
    URI.Param['routines'] := '1'
  else
    URI.Param['routines'] := Null;
  if (PObjectSearch.Routines) then
    URI.Param['events'] := '1'
  else
    URI.Param['events'] := Null;
  if (PObjectSearch.Fields) then
    URI.Param['fields'] := '1'
  else
    URI.Param['fields'] := Null;
  if (PObjectSearch.Triggers) then
    URI.Param['triggers'] := '1'
  else
    URI.Param['triggers'] := Null;
  if (PObjectSearch.Name) then
    URI.Param['name'] := '1'
  else
    URI.Param['name'] := Null;
  if (PObjectSearch.Comment) then
    URI.Param['comment'] := '1'
  else
    URI.Param['comment'] := Null;

  LockWindowUpdate(FNavigator.Handle);
  ScrollPos.Horz := GetScrollPos(FNavigator.Handle, SB_HORZ);
  ScrollPos.Vert := GetScrollPos(FNavigator.Handle, SB_VERT);
  CurrentAddress := URI.Address;
  SetScrollPos(FNavigator.Handle, SB_HORZ, ScrollPos.Horz, TRUE);
  SetScrollPos(FNavigator.Handle, SB_VERT, ScrollPos.Vert, TRUE);
  LockWindowUpdate(0);

  URI.Free();
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
  // Debug 2017-02-05
  Assert(Assigned(SBlob),
    'SBlob = SBlobDebug: ' + BoolToStr(SBlob = SBlobDebug, True));
  // 2017-02-08:
  // aDDeleteExecute - start - SBlob: True
  // # 2017-02-08 15:39:58
  // DROP TABLE `mixer`.`extranet`;
  // aDDeleteExecute - end - SBlob: True

  if (not (csDestroying in ComponentState)) then
    case (Event.EventType) of
      etItemsValid,
      etItemValid,
      etItemCreated,
      etItemRenamed,
      etItemDeleted:
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

  // Debug 2017-02-05
  Assert(Assigned(SBlob),
    'EventType: ' + IntToStr(Ord(Event.EventType)));
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

        for J := 0 to TSDatabase(CurrentData).Tables.Count - 1 do
        begin
          MenuItem := TMenuItem.Create(Self);
          MenuItem.Caption := TSDatabase(CurrentData).Tables[J].Name;
          MenuItem.OnClick := FQueryBuilderAddTable;
          MenuItem.Tag := Integer(TSDatabase(CurrentData).Tables[J]);
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

  ListView.OnChanging := nil;
  ListView.Items.BeginUpdate();
  ListView.Items.Clear();
  ListView.Items.EndUpdate();
  try
    ListView.Free();
  except
    // Without this, sometimes there is a #5 Access Denied error...

    // Was this a problem of Delphi XE2?
    // 2017-01-27
    on E: Exception do
      SendToDeveloper(E.Message, 7);
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
  TreeView: TTreeView;
begin
  TreeView := TTreeView(Sender);

  if (not TreeView.IsEditing()) then
    if (Key = #3) then
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
begin
  TreeViewMouseDown(Sender, Button, Shift, X, Y);

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

// Debug 2017-01-24
type
  TUnprotectedDBGrid = class(TMySQLDBGrid)
  end;

function TFSession.GetActiveDBGrid(): TMySQLDBGrid;
var
  I: Integer;
begin
  case (View) of
    vBrowser:
      begin
        // Debug 2017-01-26
        if (not (TObject(CurrentData) is TSTable)) then
          if (not Assigned(CurrentData)) then
            raise ERangeError.Create('CurrentAddress: ' + CurrentAddress + #13#10
              + 'CurrentClassIndex: ' + IntToStr(Ord(CurrentClassIndex)))
          else
            raise ERangeError.Create('CurrentAddress: ' + CurrentAddress + #13#10
              + 'CurrentClassIndex: ' + IntToStr(Ord(CurrentClassIndex)) + #13#10
              + 'CurrentData Class: ' + TObject(CurrentData).ClassName);

        Result := Desktop(TSTable(CurrentData)).CreateDBGrid();
      end;
    vIDE:
      case (CurrentClassIndex) of
        ciProcedure,
        ciFunction: Result := Desktop(TSRoutine(CurrentData)).ActiveDBGrid;
        else Result := nil;
      end;
    vBuilder:
      Result := Desktop(TSDatabase(CurrentData)).BuilderDBGrid;
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
    // Debug 2017-01-24
    if (not Assigned(TUnprotectedDBGrid(Result).DataLink)) then
      raise ERangeError.Create(SRangeError);

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
  case (CurrentClassIndex) of
    ciProcedure,
    ciFunction:
      begin
        Routine := TSRoutine(CurrentData);

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
    ciTrigger:
      if (not Session.Connection.InUse()) then
        FObjectIDEGrid.DataSource.DataSet := TSTrigger(CurrentData).InputDataSet;
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

  PObjectIDETrigger.Visible := (CurrentClassIndex = ciTrigger);
  if (PObjectIDETrigger.Visible) then
    DBGridColExit(FObjectIDEGrid);

  PObjectIDEResize(nil);

  Result := FObjectIDEGrid.DataSource.DataSet;
end;

function TFSession.GetActiveListView(): TListView;
var
  I: Integer;
begin
  if (View = vObjectSearch) then
  begin
    if (not Assigned(ObjectSearchListView) and Assigned(ObjectSearch)) then
      ObjectSearchListView := CreateListView(ObjectSearch);
    Result := ObjectSearchListView;
  end
  else
    case (CurrentClassIndex) of
      ciSession:
        begin
          if (not Assigned(ServerListView)) then
          begin
            ServerListView := CreateListView(Session);
            Session.PushBuildEvents();
          end;
          Result := ServerListView;
        end;
      ciDatabase,
      ciSystemDatabase:
        Result := Desktop(TSDatabase(CurrentData)).CreateListView();
      ciBaseTable,
      ciView,
      ciSystemView:
        Result := Desktop(TSTable(CurrentData)).CreateListView();
      ciProcesses:
        begin
          if (not Assigned(ProcessesListView)) then
          begin
            ProcessesListView := CreateListView(Session.Processes);
            Session.Processes.PushBuildEvent(Session.Processes);
          end;
          Result := ProcessesListView;
        end;
      ciUsers:
        begin
          if (not Assigned(UsersListView)) then
          begin
            UsersListView := CreateListView(Session.Users);
            Session.Users.PushBuildEvent(Session.Users);
          end;
          Result := UsersListView;
        end;
      ciVariables:
        begin
          if (not Assigned(VariablesListView)) then
          begin
            VariablesListView := CreateListView(Session.Variables);
            Session.Variables.PushBuildEvent(Session.Variables);
          end;
          Result := VariablesListView;
        end;
      ciQuickAccess:
        begin
          if (not Assigned(QuickAccessListView)) then
            QuickAccessListView := CreateListView(Session.QuickAccess);
          Result := QuickAccessListView;
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
        case (CurrentClassIndex) of
          ciView: Result := Desktop(TSView(CurrentData)).CreateSynMemo();
          ciProcedure,
          ciFunction: Result := Desktop(TSRoutine(CurrentData)).CreateSynMemo();
          ciTrigger: Result := Desktop(TSTrigger(CurrentData)).CreateSynMemo();
          ciEvent: Result := Desktop(TSEvent(CurrentData)).CreateSynMemo();
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

  if (Assigned(Result)) then
  begin
    Result.Visible := True;
    Result.BringToFront();
  end;
  for I := 0 to PSynMemo.ControlCount - 1 do
    if (PSynMemo.Controls[I] <> Result) then
      PSynMemo.Controls[I].Visible := False;
end;

function TFSession.GetActiveWorkbench(): TWWorkbench;
var
  I: Integer;
begin
  case (View) of
    vDiagram:
      Result := Desktop(TSDatabase(CurrentData)).Workbench;
    else
      Result := nil;
  end;

  if (Assigned(Result)) then
    for I := 0 to PWorkbench.ControlCount - 1 do
      PWorkbench.Controls[I].Visible := PWorkbench.Controls[I] = Result;
end;

function TFSession.GetCurrentClassIndex(): TClassIndex;
begin
  Result := ClassIndexByAddress(CurrentAddress);
end;

function TFSession.GetCurrentData(): TCustomData;
begin
  Result := DataByAddress(CurrentAddress);
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
  else if ((Window.ActiveControl = FNavigator) and Assigned(FNavigatorMenuNode) and (TObject(FNavigatorMenuNode.Data) is TSItem)) then
    Result := TSItem(FNavigatorMenuNode.Data)
  else if ((Window.ActiveControl = FNavigator) and (TObject(CurrentData) is TSItem)) then
    Result := TSItem(CurrentData)
  else if ((Window.ActiveControl = ActiveListView) and Assigned(ActiveListView.Selected) and (TSObject(ActiveListView.Selected.Data) is TSItem)) then
    Result := TSItem(ActiveListView.Selected.Data)
  else if ((Window.ActiveControl = ActiveWorkbench) and (ActiveWorkbench.Selected is TWTable)) then
    Result := TWTable(ActiveWorkbench.Selected).BaseTable
  else if ((Window.ActiveControl = ActiveWorkbench) and (ActiveWorkbench.Selected is TWForeignKey)) then
    Result := TWForeignKey(ActiveWorkbench.Selected).BaseForeignKey
  else if ((Window.ActiveControl = ActiveListView) and not Assigned(ActiveListView.Selected) and (TObject(CurrentData) is TSItem)) then
    Result := TSItem(CurrentData)
  else if ((Window.ActiveControl = ActiveWorkbench) and not Assigned(ActiveWorkbench.Selected) and (TObject(CurrentData) is TSItem)) then
    Result := TSItem(CurrentData)
  else
    Result := nil;

  // Debug 2016-12-19
  if (Assigned(Result)) then
  begin
    if (not (TObject(Result) is TSItem)) then
      if ((Window.ActiveControl is TTreeView) and Assigned(TTreeView(Window.ActiveControl).Selected)) then
        raise ERangeError.Create('ImageIndex: ' + IntToStr(TTreeView(Window.ActiveControl).Selected.ImageIndex) + #13#10
          + 'Text: ' + TTreeView(Window.ActiveControl).Selected.Text)
      else if ((Window.ActiveControl is TListView) and Assigned(TListView(Window.ActiveControl).Selected)) then
        raise ERangeError.Create('ImageIndex: ' + IntToStr(TListView(Window.ActiveControl).Selected.ImageIndex) + #13#10
          + 'Text: ' + TListView(Window.ActiveControl).Selected.Caption + #13#10
          + 'Assigned(Data): ' + BoolToStr(Assigned(TListView(Window.ActiveControl).Selected.Data), True) + #13#10
          + 'Is User: ' + BoolToStr(Assigned(TListView(Window.ActiveControl).Selected.Data) and (Session.Users.IndexOf(TListView(Window.ActiveControl).Selected.Data) >= 0), True))
      else
        raise ERangeError.Create('ActiveControl.ClassType: ' + Window.ActiveControl.ClassName);
  end;
end;

function TFSession.GetMenuDatabase(): TSDatabase;
var
  Node: TTreeNode;
  URI: TUURI;
begin
  if (Window.ActiveControl = FNavigator) then
  begin
    Node := FNavigatorMenuNode;
    while (Assigned(Node) and not (Node.ImageIndex in [iiDatabase, iiSystemDatabase])) do
      Node := Node.Parent;
    if (Assigned(Node)) then
      Result := TSDatabase(Node.Data)
    else
      Result := nil;
  end
  else
  begin
    URI := TUURI.Create(CurrentAddress);
    Result := Session.DatabaseByName(URI.Database);
    URI.Free();
  end;
end;

function TFSession.GetPath(): TFileName;
begin
  Result := ExcludeTrailingPathDelimiter(Preferences.Path);
end;

function TFSession.GetSelectedDatabase(): string;
var
  URI: TUURI;
begin
  URI := TUURI.Create(CurrentAddress);
  Result := URI.Database;
  URI.Free();
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

function TFSession.GetView(): TView;
var
  URI: TUURI;
begin
  URI := TUURI.Create(CurrentAddress);
  Result := ParamToView(URI.Param['view']);
  URI.Free();
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
  Len: Integer;
begin
  if (OpenClipboard(Handle)) then
    try
      EmptyClipboard();

      Content := Session.Connection.EscapeIdentifier(MGridHeaderColumn.DisplayName);

      Len := Length(Content);
      ClipboardData := GlobalAlloc(GMEM_MOVEABLE + GMEM_DDESHARE, (Len + 1) * SizeOf(Content[1]));
      Move(PChar(Content)^, GlobalLock(ClipboardData)^, (Len + 1) * SizeOf(Content[1]));
      SetClipboardData(CF_UNICODETEXT, ClipboardData);
      GlobalUnlock(ClipboardData);
    finally
      CloseClipboard();
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

procedure TFSession.ghmSelectAllClick(Sender: TObject);
begin
  if (ActiveDBGrid.SelectedFields.IndexOf(MGridHeaderColumn.Field) < 0) then
    ActiveDBGrid.SelectedFields.Add(MGridHeaderColumn.Field)
  else
    ActiveDBGrid.SelectedFields.Delete(ActiveDBGrid.SelectedFields.IndexOf(MGridHeaderColumn.Field));
  ActiveDBGrid.Invalidate();
end;

function TFSession.GiveFeedback(dwEffect: Longint): HResult;
begin
  Result := DRAGDROP_S_USEDEFAULTCURSORS;
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
      Desktop(TSTable(CurrentData)).etItemResort(Format(Filters[FilterIndex].Text, [Session.Connection.EscapeIdentifier(ActiveDBGrid.SelectedField.FieldName), Value]));
    end
    else
    begin
      ActiveDBGrid.DataSource.DataSet.FilterOptions := [foCaseInsensitive];
      ActiveDBGrid.DataSource.DataSet.Filter := Format(Filters[FilterIndex].Text, ['[' + ActiveDBGrid.SelectedField.FieldName + ']', Value]);
      ActiveDBGrid.DataSource.DataSet.Filtered := True;
      StatusBarRefresh();
    end;
end;

function TFSession.GroupIDByImageIndex(const ImageIndex: Integer): Integer;
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
    iiBaseField,
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
      Result := iiBaseField
  else if (TObject(Data) is TSForeignKey) then
    Result := iiForeignKey
  else if (TObject(Data) is TSTrigger) then
    Result := iiTrigger
  else if (TObject(Data) is TSProcess) then
    Result := iiProcess
  else if (TObject(Data) is TSUser) then
    Result := iiUser
  else if (TObject(Data) is TSVariable) then
    Result := iiVariable
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
    and ((Item.ImageIndex = iiKey) and TSKey(Item.Data).PrimaryKey or (Item.ImageIndex in [iiBaseField, iiVirtualField]) and TSTableField(Item.Data).InPrimaryKey)) then
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
  ListView: TListView;
  Kind: TPAccount.TDesktop.TListViewKind;
begin
  Assert(Sender is TListView);

  ListView := TListView(Sender);

  Kind := ColumnWidthKindByListView(ListView);

  if ((Kind = lkTable) and (Column.Index = 0) and (ListViewSortData[Kind].ColumnIndex = 0) and (ListViewSortData[Kind].Order < 0)) then
    ListViewSortData[Kind].Order := 0
  else if ((Column.Index <> ListViewSortData[Kind].ColumnIndex)
    or (ListViewSortData[Kind].Order <= 0)) then
    ListViewSortData[Kind].Order := 1
  else
    ListViewSortData[Kind].Order := -1;
  ListViewSortData[Kind].ColumnIndex := Column.Index;

  ListView.CustomSort(nil, LPARAM(@ListViewSortData[Kind]));

  ListViewHeaderUpdate(Sender);
end;

procedure TFSession.ListViewCompare(Sender: TObject; Item1: TListItem;
  Item2: TListItem; Data: Integer; var Compare: Integer);
const
  ImageIndexSort = Chr(iiDatabase) + Chr(iiSystemDatabase) + Chr(iiBaseTable)
    + Chr(iiView) + Chr(iiSystemView) + Chr(iiKey) + Chr(iiBaseField)
    + Chr(iiViewField) + Chr(iiForeignKey) + Chr(iiProcesses) + Chr(iiUsers)
    + Chr(iiVariables);
var
  DateTime1, DateTime2: TDateTime;
  Int1, Int2: Integer;
  SortRec: ^TListViewSortRec;
begin
  SortRec := @TListViewSortRec(Pointer(Data)^);

  // Debug 2017-02-04
  Assert(Assigned(Item1.Data),
    'ImageIndex: ' + IntToStr(Item1.ImageIndex) + #13#10
    + 'Caption: ' + Item1.Caption);
  Assert(Assigned(Item2.Data),
    'ImageIndex: ' + IntToStr(Item2.ImageIndex) + #13#10
    + 'Caption: ' + Item2.Caption);

  if (Item1.GroupID <> Item2.GroupID) then
    Compare := Sign(Item1.GroupID - Item2.GroupID)
  else if (Item1.GroupID = giSystemTools) then
    Compare := Sign(Pos(Chr(Item1.ImageIndex), ImageIndexSort) - Pos(Chr(Item2.ImageIndex), ImageIndexSort))
  else
    case (SortRec^.Kind) of
      lkServer:
        case (SortRec^.ColumnIndex) of
          0:
            Compare := Sign(TSItem(Item1.Data).Index - TSItem(Item2.Data).Index);
          1:
            begin
              case (Item1.ImageIndex) of
                iiDatabase,
                iiSystemDatabase:
                  Int1 := TSDatabase(Item1.Data).Count;
                iiUsers:
                  Int1 := TSUsers(Item1.Data).Count;
                iiVariables:
                  Int1 := TSUsers(Item1.Data).Count;
                else
                  raise ERangeError.Create(SRangeError);
              end;
              case (Item2.ImageIndex) of
                iiDatabase,
                iiSystemDatabase:
                  Int2 := TSDatabase(Item2.Data).Count;
                iiUsers:
                  Int2 := TSUsers(Item2.Data).Count;
                iiVariables:
                  Int2 := TSUsers(Item2.Data).Count;
                else
                  raise ERangeError.Create(SRangeError);
              end;
              Compare := Sign(Int1 - Int2);
            end;
          2:
            begin
              case (Item1.ImageIndex) of
                iiDatabase,
                iiSystemDatabase:
                  Int1 := TSDatabase(Item1.Data).DataSize;
                else
                  raise ERangeError.Create(SRangeError);
              end;
              case (Item2.ImageIndex) of
                iiDatabase,
                iiSystemDatabase:
                  Int2 := TSDatabase(Item2.Data).DataSize;
                else
                  raise ERangeError.Create(SRangeError);
              end;
              Compare := Sign(Int1 - Int2);
            end;
          3:
            begin
              case (Item1.ImageIndex) of
                iiDatabase,
                iiSystemDatabase:
                  DateTime1 := TSDatabase(Item1.Data).Created;
                else
                  raise ERangeError.Create(SRangeError);
              end;
              case (Item2.ImageIndex) of
                iiDatabase,
                iiSystemDatabase:
                  DateTime2 := TSDatabase(Item2.Data).Created;
                else
                  raise ERangeError.Create(SRangeError);
              end;
              Compare := Sign(DateTime1 - DateTime2);
            end;
          4:
            Compare := lstrcmpi(PChar(Item1.SubItems[3]), PChar(Item2.SubItems[3]));
          else
            raise ERangeError.Create(SRangeError);
        end;
      lkDatabase:
        case (SortRec^.ColumnIndex) of
          0:
            Compare := Sign(TSItem(Item1.Data).Index - TSItem(Item2.Data).Index);
          1:
            Compare := Sign(Pos(Chr(Item1.ImageIndex), ImageIndexSort) - Pos(Chr(Item2.ImageIndex), ImageIndexSort));
          2:
            begin
              case (Item1.ImageIndex) of
                iiBaseTable:
                  Int1 := TSBaseTable(Item1.Data).RecordCount;
                else
                  Int1 := 0;
              end;
              case (Item2.ImageIndex) of
                iiBaseTable:
                  Int2 := TSBaseTable(Item2.Data).RecordCount;
                else
                  Int2 := 0;
              end;
              Compare := Sign(Int1 - Int2);
            end;
          3:
            begin
              case (Item1.ImageIndex) of
                iiBaseTable:
                  Int1 := TSBaseTable(Item1.Data).DataSize;
                iiView,
                iiSystemView:
                  Int1 := 0;
                iiProcedure,
                iiFunction:
                  Int1 := Length(TSRoutine(Item1.Data).Source);
                iiEvent:
                  Int1 := Length(TSEvent(Item1.Data).Source);
                else
                  raise ERangeError.Create(SRangeError);
              end;
              case (Item2.ImageIndex) of
                iiBaseTable:
                  Int2 := TSBaseTable(Item2.Data).DataSize;
                iiView,
                iiSystemView:
                  Int2 := 0;
                iiProcedure,
                iiFunction:
                  Int2 := Length(TSRoutine(Item2.Data).Source);
                iiEvent:
                  Int2 := Length(TSEvent(Item2.Data).Source);
                else
                  raise ERangeError.Create(SRangeError);
              end;
              Compare := Sign(Int1 - Int2);
            end;
          4:
            begin
              case (Item1.ImageIndex) of
                iiBaseTable:
                  DateTime1 := TSBaseTable(Item1.Data).Updated;
                iiView,
                iiSystemView:
                  DateTime1 := 0;
                iiProcedure,
                iiFunction:
                  DateTime1 := TSRoutine(Item1.Data).Updated;
                iiEvent:
                  DateTime1 := TSEvent(Item1.Data).Updated;
                else
                  raise ERangeError.Create(SRangeError);
              end;
              case (Item2.ImageIndex) of
                iiBaseTable:
                  DateTime2 := TSBaseTable(Item2.Data).Updated;
                iiView,
                iiSystemView:
                  DateTime2 := 0;
                iiProcedure,
                iiFunction:
                  DateTime2 := TSRoutine(Item2.Data).Updated;
                iiEvent:
                  DateTime2 := TSEvent(Item2.Data).Updated;
                else
                  raise ERangeError.Create(SRangeError);
              end;
              Compare := Sign(DateTime1 - DateTime2);
            end;
          5:
            Compare := lstrcmpi(PChar(Item1.SubItems[4]), PChar(Item2.SubItems[4]));
          6:
            Compare := lstrcmpi(PChar(Item1.SubItems[5]), PChar(Item2.SubItems[5]));
          else
            raise ERangeError.Create(SRangeError);
        end;
      lkTable:
        case (SortRec^.ColumnIndex) of
          0:
            begin
              // Debug 2017-02-08
              Assert(TObject(Item1.Data) is TSItem,
                'ClassType: ' + TObject(Item1.Data).ClassName);
              Assert(TObject(Item2.Data) is TSItem,
                'ClassType: ' + TObject(Item2.Data).ClassName);

              if (SortRec^.Order = 0) then
                Compare := Sign(TSItem(Item1.Data).Index - TSItem(Item2.Data).Index)
              else
                Compare := lstrcmpi(PChar(TSItem(Item1.Data).Caption), PChar(TSItem(Item2.Data).Caption));
            end;
          1:
            Compare := lstrcmpi(PChar(Item1.SubItems[0]), PChar(Item2.SubItems[0]));
          2:
            begin
              if (not (Item1.ImageIndex in [iiBaseField, iiViewField])) then
                Int1 := 0
              else if (TSTableField(Item1.Data).NullAllowed) then
                Int1 := 1
              else
                Int1 := 2;
              if (not (Item2.ImageIndex in [iiBaseField, iiViewField])) then
                Int2 := 0
              else if (TSTableField(Item2.Data).NullAllowed) then
                Int2 := 1
              else
                Int2 := 2;
              Compare := Int1 - Int2;
            end;
          3:
            Compare := lstrcmpi(PChar(Item1.SubItems[2]), PChar(Item2.SubItems[2]));
          4:
            Compare := lstrcmpi(PChar(Item1.SubItems[3]), PChar(Item2.SubItems[3]));
          5:
            Compare := lstrcmpi(PChar(Item1.SubItems[4]), PChar(Item2.SubItems[4]));
          else
            raise ERangeError.Create(SRangeError);
        end;
      lkProcesses:
        case (SortRec^.ColumnIndex) of
          0:
            Compare := Sign(TSItem(Item1.Data).Index - TSItem(Item2.Data).Index);
          1:
            Compare := lstrcmpi(PChar(TSProcess(Item1.Data).UserName), PChar(TSProcess(Item2.Data).UserName));
          2:
            Compare := lstrcmpi(PChar(TSProcess(Item1.Data).Host), PChar(TSProcess(Item2.Data).Host));
          3:
            Compare := lstrcmpi(PChar(TSProcess(Item1.Data).DatabaseName), PChar(TSProcess(Item2.Data).DatabaseName));
          4:
            Compare := lstrcmpi(PChar(TSProcess(Item1.Data).Command), PChar(TSProcess(Item2.Data).Command));
          5:
            Compare := lstrcmpi(PChar(TSProcess(Item1.Data).SQL), PChar(TSProcess(Item2.Data).SQL));
          6:
            Compare := Sign(TSProcess(Item1.Data).Time - TSProcess(Item2.Data).Time);
          7:
            Compare := lstrcmpi(PChar(TSProcess(Item1.Data).State), PChar(TSProcess(Item2.Data).State));
          else
            raise ERangeError.Create(SRangeError);
        end;
      lkUsers:
        case (SortRec^.ColumnIndex) of
          0:
            Compare := Sign(TSItem(Item1.Data).Index - TSItem(Item2.Data).Index);
          else
            raise ERangeError.Create(SRangeError);
        end;
      lkVariables:
        case (SortRec^.ColumnIndex) of
          0:
            Compare := Sign(TSItem(Item1.Data).Index - TSItem(Item2.Data).Index);
          1:
            Compare := lstrcmpi(PChar(TSVariable(Item1.Data).Value), PChar(TSVariable(Item2.Data).Value));
          else
            raise ERangeError.Create(SRangeError);
        end;
      lkObjectSearch:
        case (SortRec^.ColumnIndex) of
          0:
            begin
              Compare := lstrcmpi(PChar(TSItem(Item1.Data).Name), PChar(TSItem(Item1.Data).Name));
              if (Compare = 0) then
                Compare := lstrcmpi(PChar(Item1.SubItems[1]), PChar(Item2.SubItems[1]));
              if (Compare = 0) then
                Compare := Sign(Pos(Chr(Item1.ImageIndex), ImageIndexSort) - Pos(Chr(Item2.ImageIndex), ImageIndexSort));
            end;
          1:
            begin
              Compare := Sign(Pos(Chr(Item1.ImageIndex), ImageIndexSort) - Pos(Chr(Item2.ImageIndex), ImageIndexSort));
              if (Compare = 0) then
                Compare := lstrcmpi(PChar(TSItem(Item1.Data).Name), PChar(TSItem(Item1.Data).Name));
              if (Compare = 0) then
                Compare := lstrcmpi(PChar(Item1.SubItems[1]), PChar(Item2.SubItems[1]));
            end;
          2:
            begin
              Compare := lstrcmpi(PChar(Item1.SubItems[1]), PChar(Item2.SubItems[1]));
              if (Compare = 0) then
                Compare := Sign(Pos(Chr(Item1.ImageIndex), ImageIndexSort) - Pos(Chr(Item2.ImageIndex), ImageIndexSort));
              if (Compare = 0) then
                Compare := lstrcmpi(PChar(TSItem(Item1.Data).Name), PChar(TSItem(Item1.Data).Name));
            end;
          3:
            begin
              case (Item1.ImageIndex) of
                iiDatabase,
                iiSystemDatabase:
                  DateTime1 := TSDatabase(Item1.Data).Updated;
                iiBaseTable:
                  DateTime1 := TSBaseTable(Item1.Data).Updated;
                iiView,
                iiSystemView:
                  DateTime1 := 0;
                iiProcedure,
                iiFunction:
                  DateTime1 := TSRoutine(Item1.Data).Updated;
                iiEvent:
                  DateTime1 := TSEvent(Item1.Data).Updated;
                else
                  raise ERangeError.Create(SRangeError);
              end;
              case (Item2.ImageIndex) of
                iiDatabase,
                iiSystemDatabase:
                  DateTime2 := TSDatabase(Item2.Data).Updated;
                iiBaseTable:
                  DateTime2 := TSBaseTable(Item2.Data).Updated;
                iiView,
                iiSystemView:
                  DateTime2 := 0;
                iiProcedure,
                iiFunction:
                  DateTime2 := TSRoutine(Item2.Data).Updated;
                iiEvent:
                  DateTime2 := TSEvent(Item2.Data).Updated;
                else
                  raise ERangeError.Create(SRangeError);
              end;
              Compare := Sign(DateTime1 - DateTime2);
              if (Compare = 0) then
                Compare := lstrcmpi(PChar(TSItem(Item1.Data).Name), PChar(TSItem(Item1.Data).Name));
              if (Compare = 0) then
                Compare := lstrcmpi(PChar(Item1.SubItems[1]), PChar(Item2.SubItems[1]));
              if (Compare = 0) then
                Compare := Sign(Pos(Chr(Item1.ImageIndex), ImageIndexSort) - Pos(Chr(Item2.ImageIndex), ImageIndexSort));
            end;
          4:
            begin
              Compare := lstrcmpi(PChar(Item1.SubItems[3]), PChar(Item2.SubItems[3]));
              if (Compare = 0) then
                Compare := lstrcmpi(PChar(TSItem(Item1.Data).Name), PChar(TSItem(Item1.Data).Name));
              if (Compare = 0) then
                Compare := lstrcmpi(PChar(Item1.SubItems[1]), PChar(Item2.SubItems[1]));
              if (Compare = 0) then
                Compare := Sign(Pos(Chr(Item1.ImageIndex), ImageIndexSort) - Pos(Chr(Item2.ImageIndex), ImageIndexSort));
            end
          else
            raise ERangeError.Create(SRangeError);
        end;
      lkQuickAccess:
        case (SortRec^.ColumnIndex) of
          0:
            Compare := lstrcmpi(PChar(Item1.Caption), PChar(Item2.Caption));
          else
            Compare := lstrcmpi(PChar(Item1.SubItems[SortRec^.ColumnIndex - 1]), PChar(Item2.SubItems[SortRec^.ColumnIndex - 1]));
        end;
      else
        raise ERangeError.Create(SRangeError);
    end;

  if (Compare = 0) then
    Compare := Sign(TSItem(Item1.Data).Index - TSItem(Item2.Data).Index);

  if (ListViewSortData[SortRec^.Kind].Order < 0) then
    Compare := - Compare;
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
        iiBaseField,
        iiVirtualField: Accept := (ClassIndexByData(SourceNode.Parent.Data) = CurrentClassIndex) and (SourceNode.Parent <> FNavigatorNodeByAddress(CurrentAddress));
      end
    else if (((TargetItem.Caption <> SourceNode.Text) or (SourceNode.Parent <> FNavigatorNodeByAddress(CurrentAddress))) and (SourceNode.Parent.Text <> TargetItem.Caption)) then
      case (TargetItem.ImageIndex) of
        iiDatabase: Accept := (SourceNode.ImageIndex in [iiDatabase, iiBaseTable, iiProcedure, iiFunction]);
        iiBaseTable: Accept := SourceNode.ImageIndex in [iiBaseField, iiVirtualField];
      end;
  end
  else if ((Source is TListView) and (TListView(Source).SelCount = 1) and (TListView(Source).Parent.Name = PListView.Name)) then
  begin
    SourceItem := TListView(Source).Selected;
    SourceSession := TFSession(TTreeView(Source).Owner).Session;
    SourceDatabase := TFSession(TTreeView(Source).Owner).MenuDatabase;

    if (not Assigned(TargetItem)) then
      case (SourceItem.ImageIndex) of
        iiBaseTable,
        iiProcedure,
        iiFunction: Accept := (CurrentClassIndex = ciDatabase) and (not Assigned(TargetItem) and (Session.DatabaseByName(SelectedDatabase) <> SourceDatabase));
      end
    else if ((TargetItem <> SourceItem) and (TargetItem.ImageIndex = SourceItem.ImageIndex)) then
      case (SourceItem.ImageIndex) of
        iiBaseTable: Accept := (CurrentClassIndex = ciDatabase);
      end
    else if ((TargetItem <> SourceItem) and (SourceSession <> Session)) then
      case (SourceItem.ImageIndex) of
        iiBaseTable: Accept := (CurrentClassIndex = ciSession);
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
    and (Session.Connection.MySQLVersion >= 50107) or (Item.ImageIndex = iiForeignKey) and (Session.Connection.MySQLVersion >= 40013) or (Item.ImageIndex in [iiBaseTable, iiView, iiEvent, iiBaseField, iiVirtualField, iiTrigger, iiUser]);
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
    if (TObject(ListView.Tag) is TSSession) then
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
    else if ((TObject(ListView.Tag) is TSProcesses)
      or (TObject(ListView.Tag) is TSItemSearch) and (TSItemSearch(ListView.Tag).Location is TSProcesses)) then
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
    else if ((TObject(ListView.Tag) is TSUsers)
      or (TObject(ListView.Tag) is TSItemSearch) and (TSItemSearch(ListView.Tag).Location is TSUsers)) then
    begin
      ListView.Columns.Add();
      ListView.Columns.EndUpdate();
      SetColumnWidths(ListView, lkUsers);

      ListView.Groups.Add().GroupID := giUsers;
    end
    else if ((TObject(ListView.Tag) is TSVariables)
      or (TObject(ListView.Tag) is TSItemSearch) and (TSItemSearch(ListView.Tag).Location is TSVariables)) then
    begin
      ListView.Columns.Add();
      ListView.Columns.Add();
      ListView.Columns.EndUpdate();
      SetColumnWidths(ListView, lkVariables);

      ListView.Groups.Add().GroupID := giVariables;
    end
    else if (TObject(ListView.Tag) is TSItemSearch) then
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
      ListView.Groups.Add().GroupID := giProcesses;
      ListView.Groups.Add().GroupID := giTriggers;
      ListView.Groups.Add().GroupID := giUsers;
      ListView.Groups.Add().GroupID := giVariables;
    end
    else if (TObject(ListView.Tag) is TSQuickAccess) then
    begin
      ListView.Columns.Add();
      ListView.Columns.Add();
      ListView.Columns.Add();
      ListView.Columns.Add();
      ListView.Columns.Add();
      ListView.Columns.EndUpdate();
      SetColumnWidths(ListView, lkQuickAccess);

      ListView.Groups.Add().GroupID := giFrequentObjects;
      ListView.Groups[ListView.Groups.Count - 1].State := ListView.Groups[ListView.Groups.Count - 1].State + [lgsCollapsible];
      if (Session.Account.Desktop.QuickAccessMFUVisible) then
        ListView.Groups[ListView.Groups.Count - 1].State := ListView.Groups[ListView.Groups.Count - 1].State - [lgsCollapsed]
      else
        ListView.Groups[ListView.Groups.Count - 1].State := ListView.Groups[ListView.Groups.Count - 1].State + [lgsCollapsed];
      ListView.Groups.Add().GroupID := giRecentObjects;
      ListView.Groups[ListView.Groups.Count - 1].State := ListView.Groups[ListView.Groups.Count - 1].State + [lgsCollapsible];
      if (Session.Account.Desktop.QuickAccessMRUVisible) then
        ListView.Groups[ListView.Groups.Count - 1].State := ListView.Groups[ListView.Groups.Count - 1].State - [lgsCollapsed]
      else
        ListView.Groups[ListView.Groups.Count - 1].State := ListView.Groups[ListView.Groups.Count - 1].State + [lgsCollapsed];
    end;
  end;

  ListView.Groups.EndUpdate();

  if (TObject(ListView.Tag) is TSSession) then
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
  else if ((TObject(ListView.Tag) is TSProcesses)
    or (TObject(ListView.Tag) is TSItemSearch) and (TSItemSearch(ListView.Tag).Location is TSProcesses)) then
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
  else if ((TObject(ListView.Tag) is TSUsers)
    or (TObject(ListView.Tag) is TSItemSearch) and (TSItemSearch(ListView.Tag).Location is TSUsers)) then
  begin
    ListView.Columns[0].Caption := Preferences.LoadStr(561);
  end
  else if ((TObject(ListView.Tag) is TSVariables)
    or (TObject(ListView.Tag) is TSItemSearch) and (TSItemSearch(ListView.Tag).Location is TSVariables)) then
  begin
    ListView.Columns[0].Caption := Preferences.LoadStr(267);
    ListView.Columns[1].Caption := Preferences.LoadStr(268);
  end
  else if ((TObject(ListView.Tag) is TSItemSearch)
    or (TObject(ListView.Tag) is TSQuickAccess)) then
  begin
    ListView.Columns[0].Caption := Preferences.LoadStr(35);
    ListView.Columns[1].Caption := Preferences.LoadStr(69);
    ListView.Columns[2].Caption := Preferences.LoadStr(935);
    ListView.Columns[3].Caption := Preferences.LoadStr(119);
    ListView.Columns[4].Caption := Preferences.LoadStr(111);
  end;
end;

procedure TFSession.ListViewHeaderUpdate(Sender: TObject);
var
  HDItem: THDItem;
  I: Integer;
  ListView: TListView;
  Kind: TPAccount.TDesktop.TListViewKind;
begin
  Assert(Sender is TListView);

  ListView := TListView(Sender);
  Kind := ColumnWidthKindByListView(ListView);

  HDItem.Mask := HDI_FORMAT;
  for I := 0 to ListView.Columns.Count - 1 do
    if (BOOL(SendMessage(ListView_GetHeader(ListView.Handle), HDM_GETITEM, I, LParam(@HDItem)))) then
    begin
      if ((ListViewSortData[Kind].Order = 0) or (I <> ListViewSortData[Kind].ColumnIndex)) then
        HDItem.fmt := HDItem.fmt and not HDF_SORTUP and not HDF_SORTDOWN
      else if (ListViewSortData[Kind].Order > 0) then
        HDItem.fmt := HDItem.fmt and not HDF_SORTDOWN or HDF_SORTUP
      else
        HDItem.fmt := HDItem.fmt and not HDF_SORTUP or HDF_SORTDOWN;
      SendMessage(ListView_GetHeader(ListView.Handle), HDM_SETITEM, I, LParam(@HDItem));
    end;

  if ((ComCtl32MajorVersion >= 6) and not CheckWin32Version(6, 1)) then
    if (ListViewSortData[Kind].Order = 0) then
      SendMessage(ListView.Handle, LVM_SETSELECTEDCOLUMN, -1, 0)
    else
      SendMessage(ListView.Handle, LVM_SETSELECTEDCOLUMN, ListViewSortData[Kind].ColumnIndex, 0);
end;

procedure TFSession.ListViewKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
var
  I: Integer;
  MenuItem: TMenuItem;
begin
  if (not TListView(Sender).IsEditing()) then
    if ((Sender = ActiveListView) and (Key = VK_BACK)) then
      if (TObject(TListView(Sender).Tag) is TSTable) then
        CurrentAddress := AddressByData(TSTable(TListView(Sender).Tag).Database)
      else
        CurrentAddress := AddressByData(Session)
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
    case (CurrentClassIndex) of
      ciSession:
        begin
          for I := 0 to ActiveListView.Items.Count - 1 do
            if (ActiveListView.Items[I].Selected and (ActiveListView.Items[I].ImageIndex in [iiDatabase, iiSystemDatabase])) then
              List.Add(ActiveListView.Items[I].Data);
          if (not Session.Update(List)) then
            Wanted.Action := TAction(Sender)
          else if (MsgBox(Preferences.LoadStr(405), Preferences.LoadStr(101), MB_YESNOCANCEL + MB_ICONQUESTION) = IDYES) then
            Session.EmptyDatabases(List);
        end;
      ciDatabase:
        begin
          for I := 0 to ActiveListView.Items.Count - 1 do
            if (ActiveListView.Items[I].Selected and (ActiveListView.Items[I].ImageIndex in [iiBaseTable])) then
              List.Add(ActiveListView.Items[I].Data);
          if (not Session.Update(List)) then
            Wanted.Action := TAction(Sender)
          else if (MsgBox(Preferences.LoadStr(406), Preferences.LoadStr(101), MB_YESNOCANCEL + MB_ICONQUESTION) = IDYES) then
            Session.DatabaseByName(SelectedDatabase).EmptyTables(List);
        end;
      ciBaseTable:
        if (MsgBox(Preferences.LoadStr(407), Preferences.LoadStr(101), MB_YESNOCANCEL + MB_ICONQUESTION) = IDYES) then
        begin
          for I := 0 to ActiveListView.Items.Count - 1 do
            if (ActiveListView.Items[I].Selected and (ActiveListView.Items[I].ImageIndex in [iiBaseField, iiVirtualField])) then
              List.Add(ActiveListView.Items[I].Data);
          TSBaseTable(CurrentData).EmptyFields(List);
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
  Count: Integer;
  I: Integer;
  Width: Integer;
begin
  if (Sender is TListView) then
  begin
    Count := TListView(Sender).Items.Count;
    for I := 0 to TListView(Sender).Columns.Count - 1 do
    begin
      if (Count = 0) then
        Width := TListView(Sender).Column[I].Width
      else
        Width := ListView_GetColumnWidth(TListView(Sender).Handle, I);
      Session.Account.Desktop.ColumnWidths[ColumnWidthKindByListView(TListView(Sender)), I] := Width;
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
var
  Changes: Integer; // Debug 2017-02-04
  LastItem: TListItem;
  Profile: TProfile;
  ReorderGroupIndex: Integer;

  function Compare(const Kind: TPAccount.TDesktop.TListViewKind; const Item1, Item2: TListItem): Integer;
  begin
    ListViewCompare(ListView, Item1, Item2, LPARAM(@ListViewSortData[Kind]), Result);
  end;

  procedure UpdateItem(const Item: TListItem; const GroupID: Integer; const Data: TObject);
  var
    I: Integer;
    S: string;
    S2: string;
  begin
    Assert(Item.Data = Data);

    ProfilingPoint(Profile, 9);

    Item.SubItems.BeginUpdate();
    ProfilingPoint(Profile, 10);
    Item.SubItems.Clear();
    ProfilingPoint(Profile, 11);

    Item.GroupID := GroupID;

    // 3.2 seconds for 329 items

    ProfilingPoint(Profile, 12);
    Item.ImageIndex := ImageIndexByData(Data);

    // 3.8 seconds for 329 items

    ProfilingPoint(Profile, 13);

    if ((TObject(ListView.Tag) is TSItemSearch)
        and not (Data is TSProcess)
        and not (Data is TSUser)
        and not (Data is TSVariable)
      or (TObject(ListView.Tag) is TSQuickAccess)) then
    begin
      if (Data is TSDatabase) then
      begin
        Item.Caption := TSDatabase(Data).Name;
        Item.SubItems.Add(Preferences.LoadStr(38));
        Item.SubItems.Add('');
        if (TSDatabase(Data).Updated = 0) then
          Item.SubItems.Add('')
        else
          Item.SubItems.Add(SysUtils.DateTimeToStr(TSDatabase(Data).Updated, LocaleFormatSettings));
        Item.SubItems.Add('');
      end
      else if (Data is TSBaseTable) then
      begin
        Item.Caption := TSTable(Data).Caption;
        if (not Assigned(TSBaseTable(Data).Engine)) then
          Item.SubItems.Add('')
        else
          Item.SubItems.Add(TSBaseTable(Data).Engine.Name);
        Item.SubItems.Add(TSBaseTable(Data).Database.Name);
        if (TSBaseTable(Data).Updated = 0) then
          Item.SubItems.Add('')
        else
          Item.SubItems.Add(SysUtils.DateTimeToStr(TSBaseTable(Data).Updated, LocaleFormatSettings));
        Item.SubItems.Add(TSBaseTable(Data).Comment);
      end
      else if (Data is TSView) then
      begin
        Item.Caption := TSView(Data).Caption;
        Item.SubItems.Add(Preferences.LoadStr(738));
        Item.SubItems.Add(TSView(Data).Database.Name);
        Item.SubItems.Add('');
        Item.SubItems.Add('');
      end
      else if (Data is TSRoutine) then
      begin
        Item.Caption := TSRoutine(Data).Caption;
        if (TSRoutine(Data).RoutineType = rtProcedure) then
          Item.SubItems.Add(Preferences.LoadStr(768))
        else
          Item.SubItems.Add(Preferences.LoadStr(769));
        Item.SubItems.Add(TSRoutine(Data).Database.Name);
        Item.SubItems.Add(SysUtils.DateTimeToStr(TSRoutine(Data).Created, LocaleFormatSettings));
        Item.SubItems.Add(TSRoutine(Data).Comment);
      end
      else if (Data is TSEvent) then
      begin
        Item.Caption := TSEvent(Data).Caption;
        Item.SubItems.Add(Preferences.LoadStr(793));
        Item.SubItems.Add(TSEvent(Data).Database.Name);
        Item.SubItems.Add(SysUtils.DateTimeToStr(TSEvent(Data).Updated, LocaleFormatSettings));
        Item.SubItems.Add(TSEvent(Data).Comment);
      end
      else if (Data is TSTableField) then
      begin
        Item.Caption := TSTableField(Data).Caption;
        if (Data is TSViewField) then
          Item.SubItems.Add(TSViewField(Data).DBTypeStr())
        else if (TSBaseField(Data).FieldKind = mkReal) then
          Item.SubItems.Add(SQLUnescape(TSBaseField(Data).DBTypeStr()))
        else // mkVirtual
          Item.SubItems.Add(TSBaseField(Data).Expression);
        Item.SubItems.Add(TSTableField(Data).Table.Database.Name + '.' + TSTableField(Data).Table.Name);
        Item.SubItems.Add('');
        if (not (Data is TSBaseField)) then
          Item.SubItems.Add('')
        else
          Item.SubItems.Add(TSBaseField(Data).Comment);
      end
      else if (Data is TSTrigger) then
      begin
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
        Item.SubItems.Add(TSTrigger(Data).DatabaseName + '.' + TSTrigger(Data).TableName);
        if (TSTrigger(Data).Created = 0) then
          Item.SubItems.Add('')
        else
          Item.SubItems.Add(SysUtils.DateTimeToStr(TSTrigger(Data).Created, LocaleFormatSettings));
        Item.SubItems.Add('');
      end
      else
        raise ERangeError.Create('Invalid ClassType: ' + TObject(Data).ClassName);
    end
    else
    begin
      if (Data is TSDatabase) then
      begin
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
          Item.SubItems.Add(TSBaseTable(Data).Comment)
        else
          Item.SubItems.Add('');
      end
      else if (Data is TSRoutine) then
      begin
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
        if (TSRoutine(Data).Updated = 0) then
          Item.SubItems.Add('')
        else
          Item.SubItems.Add(SysUtils.DateTimeToStr(TSRoutine(Data).Updated, LocaleFormatSettings));
        if (not Assigned(TSRoutine(Data).FunctionResult) or (TSRoutine(Data).FunctionResult.Charset = '') or (TSRoutine(Data).FunctionResult.Charset = TSRoutine(Data).Database.Charset)) then
          Item.SubItems.Add('')
        else
          Item.SubItems.Add(TSRoutine(Data).FunctionResult.Charset);
        Item.SubItems.Add(TSRoutine(Data).Comment);
      end
      else if (Data is TSEvent) then
      begin
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
      else if (Data is TSBaseField) then
      begin
        Item.Caption := TSBaseField(Data).Caption;
        Item.SubItems.Add(SQLUnescape(TSBaseField(Data).DBTypeStr()));
        if (TSBaseField(Data).NullAllowed) then
          Item.SubItems.Add(Preferences.LoadStr(74))
        else
          Item.SubItems.Add(Preferences.LoadStr(75));
        if (TSBaseField(Data).FieldKind = mkReal) then
        begin
          if (TSBaseField(Data).AutoIncrement) then
            Item.SubItems.Add('<auto_increment>')
          else if (TSBaseField(Data).Default = 'NULL') then
            Item.SubItems.Add('<' + Preferences.LoadStr(71) + '>')
          else if (TSBaseField(Data).Default = 'CURRENT_TIMESTAMP') then
            Item.SubItems.Add('<INSERT-TimeStamp>')
          else
            Item.SubItems.Add(TSBaseField(Data).UnescapeValue(TSBaseField(Data).Default));
          S := '';
          if (TSBaseField(Data).FieldType in TextFieldTypes) then
          begin
            if ((TSBaseField(Data).Charset <> '') and (TSBaseField(Data).Charset <> TSBaseField(Data).Table.Charset)) then
              S := S + TSBaseField(Data).Charset;
            if ((TSBaseField(Data).Collation <> '') and (TSBaseField(Data).Collation <> TSBaseField(Data).Table.Collation)) then
            begin
              if (S <> '') then S := S + ', ';
              S := S + TSBaseField(Data).Collation;
            end;
          end;
          Item.SubItems.Add(S);
          if (Session.Connection.MySQLVersion >= 40100) then
            Item.SubItems.Add(TSBaseField(Data).Comment);
        end
        else if (TSBaseField(Data).FieldKind = mkVirtual) then
        begin
          Item.SubItems.Add(TSBaseField(Data).Expression);
          Item.SubItems.Add('');
          Item.SubItems.Add(TSBaseField(Data).Comment);
          Item.SubItems.Add('');
          Item.SubItems.Add('');
        end;
      end
      else if (Data is TSForeignKey) then
      begin
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
        Item.SubItems.Add('');
      end
      else if (Data is TSTrigger) then
      begin
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
        Item.SubItems.Add('');
        Item.SubItems.Add('');
        Item.SubItems.Add('');
        Item.SubItems.Add('');
      end
      else if (Data is TSTableField) then
      begin
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
        if (TSProcesses(Data).Count > 0) then
          Item.SubItems.Add(FormatFloat('#,##0', TSProcesses(Data).Count, LocaleFormatSettings));
      end
      else if (Data is TSProcess) then
      begin
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
        if (Session.Users.Count >= 0) then
          Item.SubItems.Add(FormatFloat('#,##0', Session.Users.Count, LocaleFormatSettings))
        else
          Item.SubItems.Add('???');
      end
      else if (Data is TSUser) then
      begin
        Item.Caption := TSUser(Data).Caption;
      end
      else if (Data is TSVariables) then
      begin
        Item.SubItems.Add(FormatFloat('#,##0', Session.Variables.Count, LocaleFormatSettings));
      end
      else if (Data is TSVariable) then
      begin
        Item.Caption := TSVariable(Data).Caption;
        Item.SubItems.Add(TSVariable(Data).Value);
      end;
    end;

    // 2.3 seconds for 301 items
    // 7.0 seconds for 329 items
    // 1.2 seconds for 7 items
    // 2.4 seconds for 39 items
    // 1.4 seconds for 17 items
    // 11.7 seconds for 7 items
    // 3.5 seconds for 70 items
    // 1.4 seconds for 61 items, TSTables

    // Event.Items.ClassType ???

    ProfilingPoint(Profile, 14);

    Item.SubItems.EndUpdate();

    ProfilingPoint(Profile, 15);
    Inc(Changes);
  end;

  function InsertOrUpdateItem(const Kind: TPAccount.TDesktop.TListViewKind; GroupID: Integer; const Data: TObject): TListItem;
  var
    Item: TListItem;
    Index: Integer;
    Left: Integer;
    Mid: Integer;
    Right: Integer;
  begin
    ProfilingPoint(Profile, 4);

    if (Assigned(LastItem) and (LastItem.Index + 1 < ListView.Items.Count - 1) and (ListView.Items[LastItem.Index + 1].Data = Data))  then
      Index := LastItem.Index + 1
    else
    begin
      Item := TListItem.Create(ListView.Items);
      Item.Data := Data;
      UpdateItem(Item, GroupID, Data);

      Index := ListView.Items.Count;
      Left := 0;
      Right := ListView.Items.Count - 1;
      while (Left <= Right) do
      begin
        Mid := (Right - Left) div 2 + Left;
        case (Compare(Kind, ListView.Items[Mid], Item)) of
          -1: begin Left := Mid + 1; Index := Mid + 1; end;
          0: begin Index := Mid; break; end;
          1: begin Right := Mid - 1; Index := Mid; end;
        end;
      end;

      Item.Free();
    end;

    ProfilingPoint(Profile, 5);

    if (Index = ListView.Items.Count) then
    begin
      Result := ListView.Items.Add();
      Result.Data := Data;
    end
    else if (ListView.Items[Index].Data <> Data) then
    begin
      Result := ListView.Items.Insert(Index);
      Result.Data := Data;
      if (ReorderGroupIndex < 0) then
        ReorderGroupIndex := Index
      else
        ReorderGroupIndex := Min(ReorderGroupIndex, Index);
    end
    else
    begin
      Result := ListView.Items[Index];
      if (not (Data is TSItem) or (TSItem(Data).Caption <> Result.Caption)) then
        if (ReorderGroupIndex < 0) then
          ReorderGroupIndex := Index
        else
          ReorderGroupIndex := Min(ReorderGroupIndex, Index);
    end;
    ProfilingPoint(Profile, 6);

    UpdateItem(Result, GroupID, Data);
  end;

  function AddItem(const GroupID: Integer; const Data: TObject): TListItem;
  begin
    ProfilingPoint(Profile, 7);
    Result := ListView.Items.Add();

    Result.Data := Data;

    ProfilingPoint(Profile, 8);

    UpdateItem(Result, GroupID, Data);
  end;

  procedure UpdateObjectSearchGroupHeaders();
  var
    Count: Integer;
    DatabaseCount: Integer;
    EventCount: Integer;
    FieldCount: Integer;
    I: Integer;
    TableCount: Integer;
    TriggerCount: Integer;
    ProcessCount: Integer;
    RoutineCount: Integer;
    UserCount: Integer;
    VariableCount: Integer;
  begin
    DatabaseCount := 0;
    TableCount := 0;
    RoutineCount := 0;
    EventCount := 0;
    FieldCount := 0;
    ProcessCount := 0;
    TriggerCount := 0;
    UserCount := 0;
    VariableCount := 0;
    Count := ListView.Items.Count; // Cache for speeding
    for I := 0 to Count - 1 do
      case (ListView.Items[I].GroupID) of
        giDatabases: Inc(DatabaseCount);
        giTables: Inc(TableCount);
        giRoutines: Inc(RoutineCount);
        giEvents: Inc(EventCount);
        giFields: Inc(FieldCount);
        giProcesses: Inc(ProcessCount);
        giTriggers: Inc(TriggerCount);
        giUsers: Inc(UserCount);
        giVariables: Inc(VariableCount);
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
    if (ProcessCount > 0) then
      SetListViewGroupHeader(ListView, giProcesses, Preferences.LoadStr(24) + ' (' + IntToStr(ProcessCount) + ')');
    if (TriggerCount > 0) then
      SetListViewGroupHeader(ListView, giTriggers, Preferences.LoadStr(797) + ' (' + IntToStr(TriggerCount) + ')');
    if (UserCount > 0) then
      SetListViewGroupHeader(ListView, giUsers, Preferences.LoadStr(561) + ' (' + IntToStr(UserCount) + ')');
    if (VariableCount > 0) then
      SetListViewGroupHeader(ListView, giVariables, Preferences.LoadStr(22) + ' (' + IntToStr(VariableCount) + ')');
  end;

  procedure UpdateGroup(const Kind: TPAccount.TDesktop.TListViewKind; const GroupID: Integer; const SItems: TSItems);

    function ListViewDescription(const ListView: TListView): string;
    begin
      if (TObject(ListView.Tag) is TSSession) then
        Result := 'Session'
      else if (TObject(ListView.Tag) is TSDatabase) then
        Result := 'Database ' + TSDatabase(ListView.Tag).Name
      else if (TObject(ListView.Tag) is TSTable) then
        Result := 'Table ' + TSTable(ListView.Tag).Database.Name + '.' + TSTable(ListView.Tag).Name
      else if (ListView.Tag <> 0) then
        Result := TObject(ListView.Tag).ClassName
      else
        Result := '???';
    end;

  var
    Add: Boolean;
    Count: Integer;
    Data: TCustomData;
    Header: string;
    I: Integer;
    Index: Integer;
    Item: TListItem;
    ItemFocused: Boolean;
    ItemSelected: Boolean;
    J: Integer;
    NewIndex: Integer;
    S: string;
    UserCount: Integer;
  begin
    ReorderGroupIndex := -1;

    case (Event.EventType) of
      etItemsValid:
        begin
          ProfilingPoint(Profile, 2);

          Count := ListView.Items.Count;
          for I := 0 to ListView.Columns.Count - 1 do
            if (Count = 0) then
              ListView.Columns[I].Width := 50
            else if (ListView.Columns[I].WidthType < 0) then
              ListView.Columns[I].Width := ListView_GetColumnWidth(ListView.Handle, I);

          for I := ListView.Items.Count - 1 downto 0 do
            if ((ListView.Items[I].GroupID = GroupID) and (SItems.IndexOf(ListView.Items[I].Data) < 0)) then
              ListView.Items.Delete(I);

          ProfilingPoint(Profile, 3);

          Add := (ListView.Items.Count = 0) and (ListViewSortData[Kind].ColumnIndex = 0) and (ListViewSortData[Kind].Order = 1);
          for I := 0 to SItems.Count - 1 do
            if (not (SItems is TSTriggers) or (TSTriggers(SItems)[I].Table = TObject(ListView.Tag))) then
              if (not Add) then
                LastItem := InsertOrUpdateItem(Kind, GroupID, SItems[I])
              else
                LastItem := AddItem(GroupID, SItems[I]);

          for I := 0 to ListView.Columns.Count - 1 do
            if ((Kind = lkProcesses) and (I = 5)) then
              ListView.Columns[I].Width := Preferences.GridMaxColumnWidth
            else if (Count = 0) then
              ListView.Columns[I].Width := Session.Account.Desktop.ColumnWidths[Kind, I];
        end;
      etItemValid:
        if (GroupID = GroupIDByImageIndex(ImageIndexByData(Event.Item))) then
        begin
          if (Event.Item is TSTrigger) then
          begin
            Add := True;
            Count := ListView.Items.Count; // Cache for speeding
            for I := 0 to Count - 1 do
              if (ListView.Items[I].Data = Event.Item) then
              begin
                UpdateItem(ListView.Items[I], GroupID, Event.Item);
                Add := False;
              end;
            if (Add) then
              InsertOrUpdateItem(Kind, GroupID, Event.Item);
          end
          else
          begin
            Count := ListView.Items.Count; // Cache for speeding
            for I := 0 to Count - 1 do
              if (ListView.Items[I].Data = Event.Item) then
                UpdateItem(ListView.Items[I], GroupID, Event.Item);
          end;
        end;
      etItemCreated:
        if (GroupID = GroupIDByImageIndex(ImageIndexByData(Event.Item))) then
        begin
          Item := InsertOrUpdateItem(Kind, GroupID, Event.Item);
          if (not Assigned(ListView.Selected)) then
          begin
            Item.Selected := True;
            Item.Focused := True;
          end;
        end;
      etItemRenamed:
        if (GroupID = GroupIDByImageIndex(ImageIndexByData(Event.Item))) then
        begin
          Index := 0;
          Count := ListView.Items.Count; // Cache for speeding
          while (Index < Count) do
          begin
            if (ListView.Items[Index].Data = Event.Item) then
              break;
            Inc(Index);
          end;
          ItemSelected := (Index < Count) and ListView.Items[Index].Selected;
          ItemFocused := (Index < Count) and ListView.Items[Index].Focused;
          if (Index < Count) then
            ListView.Items.Delete(Index);
          Item := InsertOrUpdateItem(Kind, GroupID, Event.Item);
          Item.Selected := ItemSelected;
          Item.Focused := ItemFocused;
        end;
      etItemDeleted:
        if (GroupID = GroupIDByImageIndex(ImageIndexByData(Event.Item))) then
        begin
          ProfilingPoint(Profile, 17);
          for I := ListView.Items.Count - 1 downto 0 do
            if (ListView.Items[I].Data = Event.Item) then
            begin
              ProfilingPoint(Profile, 17);
              ListView.Items.Delete(I);
              ProfilingPoint(Profile, 19);
              Inc(Changes);
              break;
            end;

          ProfilingPoint(Profile, 20);
        end;
    end;

    ProfilingPoint(Profile, 21);

    if ((ReorderGroupIndex >= 0) and ListView.GroupView) then
    begin
      // This code is needed in Delphi XE4 to place inserted items into the
      // right place. Without this code, they will be placed to the bottom
      Count := ListView.Items.Count; // Cache for speeding
      for I := ReorderGroupIndex + 1 to Count - 1 do
        if (ListView.Items[I].GroupID = GroupID) then
        begin
          ListView.Items[I].GroupID := -1;
          ListView.Items[I].GroupID := GroupID;
        end;
    end;

    ProfilingPoint(Profile, 22);

    if (Event.EventType in [etItemsValid, etItemCreated, etItemDeleted]) then
      if (TObject(ListView.Tag) is TSItemSearch) then
        UpdateObjectSearchGroupHeaders()
      else
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
            if (TObject(ListView.Tag) is TSBaseTable) then
              SetListViewGroupHeader(ListView, GroupID, Preferences.LoadStr(458) + ' (' + IntToStr(TSBaseTable(ListView.Tag).Keys.Count) + ')');
          giFields:
            if (TObject(ListView.Tag) is TSTable) then
              SetListViewGroupHeader(ListView, GroupID, Preferences.LoadStr(253) + ' (' + IntToStr(TSTable(ListView.Tag).Fields.Count) + ')');
          giForeignKeys:
            if (TObject(ListView.Tag) is TSBaseTable) then
              SetListViewGroupHeader(ListView, GroupID, Preferences.LoadStr(459) + ' (' + IntToStr(TSBaseTable(ListView.Tag).ForeignKeys.Count) + ')');
          giTriggers:
            if (TObject(ListView.Tag) is TSBaseTable) then
            begin
              Count := 0;
              for I := 0 to TSTriggers(SItems).Count - 1 do
                if (TSTriggers(SItems)[I].Table = TObject(ListView.Tag)) then
                  Inc(Count);
              SetListViewGroupHeader(ListView, GroupID, Preferences.LoadStr(797) + ' (' + IntToStr(Count) + ')');
            end;
          giProcesses:
            SetListViewGroupHeader(ListView, GroupID, Preferences.LoadStr(24) + ' (' + IntToStr(Session.Processes.Count) + ')');
          giUsers:
            SetListViewGroupHeader(ListView, GroupID, Preferences.LoadStr(561) + ' (' + IntToStr(Session.Users.Count) + ')');
          giVariables:
            SetListViewGroupHeader(ListView, GroupID, Preferences.LoadStr(22) + ' (' + IntToStr(Session.Variables.Count) + ')');
        end;

    ProfilingPoint(Profile, 23);
  end;

  procedure UpdateQuickAccess();
  var
    Count: Integer;
    Data: TCustomData;
    I: Integer;
    ItemCount: Integer;
    FrequentObjects: TStringList;
    RecentObjects: TStringList;
    URI1: TUURI;
    URI2: TUURI;
  begin
    case (Event.EventType) of
      etItemsValid:
        begin
          FrequentObjects := TStringList.Create();
          RecentObjects := TStringList.Create();
          URI1 := TUURI.Create();
          URI2 := TUURI.Create();

          for I := Session.Account.Desktop.Addresses.Count - 1 downto 0 do
          begin
            URI1.Address := Session.Account.Desktop.Addresses[I];

            URI2.Address := Session.Account.ExpandAddress('/');
            URI2.Database := URI1.Database;
            URI2.Table := URI1.Table;
            URI2.Param['objecttype'] := URI1.Param['objecttype'];
            URI2.Param['object'] := URI1.Param['object'];

            if ((URI2.Table <> '') or (URI2.Param['object'] <> Null)) then
            begin
              if (not TryStrToInt(FrequentObjects.Values[ReplaceStr(URI2.Address, '=', QuickAccessListEscaper)], Count)) then
                Count := 0;
              FrequentObjects.Values[ReplaceStr(URI2.Address, '=', QuickAccessListEscaper)] := IntToStr(Count + 1);

              if (RecentObjects.IndexOf(URI2.Address) < 0) then
                RecentObjects.Insert(0, URI2.Address);
            end;
          end;

          if (FrequentObjects.Count > 0) then
          begin
            FrequentObjects.CustomSort(QuickAccessFrequentObjectsCompare);

            ItemCount := 0;
            for I := 0 to FrequentObjects.Count - 1 do
            if (ItemCount < QuickAccessItemCount) then
              begin
                Data := DataByAddress(ReplaceStr(FrequentObjects.Names[I], QuickAccessListEscaper, '='));

                if (ClassIndexByData(Data) in [ciBaseTable, ciView, ciProcedure, ciFunction, ciTrigger, ciEvent]) then
                begin
                  AddItem(giFrequentObjects, Data);
                  Inc(ItemCount);
                end;
              end;

            SetListViewGroupHeader(ListView, giFrequentObjects, Preferences.LoadStr(940));
          end;

          if (RecentObjects.Count > 0) then
          begin
            ItemCount := 0;
            for I := 0 to RecentObjects.Count - 1 do
              if (ItemCount < QuickAccessItemCount) then
              begin
                Data := DataByAddress(RecentObjects[I]);

                if (ClassIndexByData(Data) in [ciBaseTable, ciView, ciProcedure, ciFunction, ciTrigger, ciEvent]) then
                begin
                  AddItem(giRecentObjects, Data);
                  Inc(ItemCount);
                end;
              end;

            SetListViewGroupHeader(ListView, giRecentObjects, Preferences.LoadStr(941));
          end;

          FrequentObjects.Free();
          RecentObjects.Free();
          URI1.Free();
          URI2.Free();
        end;
      etItemValid,
      etItemRenamed:
        for I := 0 to ListView.Items.Count - 1 do
          if (ListView.Items[I].Data = Event.Item) then
            UpdateItem(ListView.Items[I], ListView.Items[I].GroupID, Event.Item);
      etItemDeleted:
        for I := ListView.Items.Count - 1 downto 0 do
          if (ListView.Items[I].Data = Event.Item) then
          begin
            ListView.Items.Delete(I);
            break;
          end;
    end;
  end;

var
  ChangingEvent: TLVChangingEvent;
  Count: Integer;
  I: Integer;
  Kind: TPAccount.TDesktop.TListViewKind;
  S: string;
begin
  if (Assigned(ListView)
    and (Assigned(Event.Items) or (Event.Sender is TSTable) or (Event.Sender is TSItemSearch))) then
  begin
    CreateProfile(Profile);

    LastItem := nil;
    Changes := 0;

    ChangingEvent := ListView.OnChanging;
    ListView.OnChanging := nil;

    ListView.Items.BeginUpdate();
    ListView.DisableAlign();

    Kind := ColumnWidthKindByListView(ListView);

    ProfilingPoint(Profile, 1);

    if (TObject(ListView.Tag) is TSSession) then
    begin
      if (ListView.Items.Count = 0) then
      begin
        if (Assigned(Session.Processes)) then
          AddItem(giSystemTools, Session.Processes);
        if (Assigned(Session.Users)) then
          AddItem(giSystemTools, Session.Users);
        if (Assigned(Session.Variables)) then
          AddItem(giSystemTools, Session.Variables);
        ListViewInitialize(ListView);
      end;
      if ((Event.Items is TSProcesses)
        or (Event.Items is TSUsers)
        or (Event.Items is TSVariables)) then
      begin
        Count := ListView.Items.Count; // Cache for speeding
        for I := 0 to Count - 1 do
          if (ListView.Items[I].Data = Event.Items) then
            UpdateItem(ListView.Items[I], giSystemTools, Event.Items);
      end;
    end;

    if ((Kind in [lkServer, lkObjectSearch]) and (Event.Items is TSDatabases)) then
      UpdateGroup(Kind, giDatabases, Event.Items)
    else if ((Kind in [lkDatabase, lkObjectSearch, lkQuickAccess]) and (Event.Items is TSTables)) then
      UpdateGroup(Kind, giTables, Event.Items)
    else if ((Kind in [lkDatabase, lkObjectSearch, lkQuickAccess]) and (Event.Items is TSRoutines)) then
      UpdateGroup(Kind, giRoutines, Event.Items)
    else if ((Kind in [lkDatabase, lkObjectSearch, lkQuickAccess]) and (Event.Items is TSEvents)) then
      UpdateGroup(Kind, giEvents, Event.Items)
    else if ((Kind in [lkTable, lkObjectSearch]) and (Event.Sender is TSTable)) then
    begin
      if (TObject(ListView.Tag) is TSBaseTable) then
        UpdateGroup(Kind, giKeys, TSBaseTable(ListView.Tag).Keys);
      UpdateGroup(Kind, giFields, TSTable(Event.Sender).Fields);
      if ((TObject(ListView.Tag) is TSBaseTable) and Assigned(TSBaseTable(ListView.Tag).ForeignKeys)) then
        UpdateGroup(Kind, giForeignKeys, TSBaseTable(ListView.Tag).ForeignKeys);
      if ((TObject(ListView.Tag) is TSBaseTable) and Assigned(TSBaseTable(ListView.Tag).Database.Triggers)) then
        UpdateGroup(Kind, giTriggers, TSBaseTable(ListView.Tag).Database.Triggers);
    end
    else if ((Kind in [lkTable, lkObjectSearch, lkQuickAccess]) and (Event.Items is TSTriggers)) then
      UpdateGroup(Kind, giTriggers, Event.Items)
    else if ((Kind in [lkProcesses, lkObjectSearch]) and (Event.Items is TSProcesses)) then
      UpdateGroup(Kind, giProcesses, Event.Items)
    else if ((Kind in [lkUsers, lkObjectSearch]) and (Event.Items is TSUsers)) then
      UpdateGroup(Kind, giUsers, Event.Items)
    else if ((Kind in [lkVariables, lkObjectSearch]) and (Event.Items is TSVariables)) then
      UpdateGroup(Kind, giVariables, Event.Items)
    else if (Kind in [lkQuickAccess]) then
      UpdateQuickAccess()
    else if ((Kind in [lkObjectSearch]) and (Event.Items is TSItemSearch)) then
    begin
      for I := ListView.Items.Count to Event.Items.Count - 1 do
        AddItem(GroupIDByImageIndex(ImageIndexByData(Event.Items[I])), Event.Items[I]);
      UpdateObjectSearchGroupHeaders();
    end;

    if ((Window.ActiveControl = ListView) and Assigned(ListView.OnSelectItem)) then
      ListView.OnSelectItem(nil, ListView.Selected, Assigned(ListView.Selected));

    ListView.EnableAlign();
    ListView.Items.EndUpdate();

    ProfilingPoint(Profile, 24);

    ListView.OnChanging := ChangingEvent;

    ListViewHeaderUpdate(ListView);

    if (ProfilingTime(Profile) > 1000) then
    begin
      S := 'ListViewUpdate - '
        + 'EventType: ' + IntToStr(Ord(Event.EventType)) + ', ';
      if (Assigned(Event.Items)) then
        S := S
          + 'Items: ' + Event.Items.ClassName + ', '
          + 'Count: ' + IntToStr(Event.Items.Count) + ', ';
      if (Event.Item is TSTable) then
        S := S
          + 'FieldCount: ' + IntToStr(TSTable(Event.Item).Fields.Count) + ', ';
      S := S + 'Changes: ' + IntToStr(Changes) + ', ';
      S := S + ProfilingReport(Profile) + #13#10;
      TimeMonitor.Append(S, ttDebug);
    end;

    CloseProfile(Profile);
  end;

  Inc(ListViewUpdateCount);
end;

procedure TFSession.ListViewSelectItem(Sender: TObject; Item: TListItem;
  Selected: Boolean);
var
  I: Integer;
  ListView: TListView;
begin
  if (not (Sender is TListView)) then
    ListView := nil
  else
    ListView := TListView(Sender);

  if (Assigned(ListView)) then
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
      MainAction('aECopy').Enabled := ListView.SelCount >= 1;
      MainAction('aEPaste').Enabled := (ListView.SelCount = 1) and ((Item.ImageIndex = iiDatabase) and IsClipboardFormatAvailable(CF_MYSQLDATABASE) or (Item.ImageIndex = iiBaseTable) and IsClipboardFormatAvailable(CF_MYSQLTABLE) or (Item.ImageIndex = iiView) and IsClipboardFormatAvailable(CF_MYSQLVIEW));
      MainAction('aERename').Enabled := (ListView.SelCount = 1) and ((Item.ImageIndex in [iiBaseTable, iiView, iiEvent, iiBaseField, iiVirtualField, iiTrigger]) or (Item.ImageIndex = iiForeignKey) and (Session.Connection.MySQLVersion >= 40013));
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

      // Debug 2017-01-15
      if ((ListView.SelCount >= 1) and (Item.ImageIndex in [iiBaseField, iiVirtualField])) then
        if (not (TObject(Item.Data) is TSBaseField)) then
          raise ERangeError.Create('ClassType: ' + TObject(Item.Data).ClassName)
        else if (not Assigned(TSBaseField(Item.Data).Table)) then
          raise ERangeError.Create(SRangeError)
        else if (not Assigned(TSBaseField(Item.Data).Table.Fields)) then
          raise ERangeError.Create(SRangeError);

      MainAction('aDDeleteField').Enabled := (ListView.SelCount >= 1) and (Item.ImageIndex in [iiBaseField, iiVirtualField]) and (TSBaseField(Item.Data).Table.Fields.Count > ListView.SelCount);
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
      MainAction('aDEditField').Enabled := (ListView.SelCount = 1) and (Item.ImageIndex in [iiBaseField, iiVirtualField]);
      MainAction('aDEditForeignKey').Enabled := (ListView.SelCount = 1) and (Item.ImageIndex = iiForeignKey);
      MainAction('aDEditProcess').Enabled := (ListView.SelCount = 1) and (Item.ImageIndex in [iiProcess]);
      MainAction('aDEditUser').Enabled := (ListView.SelCount = 1) and (Item.ImageIndex in [iiUser]);
      MainAction('aDEditVariable').Enabled := (ListView.SelCount = 1) and (Item.ImageIndex in [iiVariable]);
      MainAction('aDEmpty').Enabled := (ListView.SelCount >= 1) and ((Item.ImageIndex in [iiDatabase, iiBaseTable]) or (Item.ImageIndex in [iiBaseField]) and TSBaseField(Item.Data).NullAllowed);

      mlOpen.Enabled := (ListView.SelCount = 1) and (Item.ImageIndex in [iiDatabase, iiSystemDatabase, iiBaseTable, iiView, iiSystemView, iiProcedure, iiFunction, iiEvent, iiTrigger, iiProcesses, iiUsers, iiVariables]);
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
        iiBaseField,
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
          MainAction('aDDeleteDatabase').Enabled := MainAction('aDDeleteDatabase').Enabled and (ListView.Items[I].ImageIndex in [iiDatabase]);
          MainAction('aDDeleteTable').Enabled := MainAction('aDDeleteTable').Enabled and (ListView.Items[I].ImageIndex in [iiBaseTable]);
          MainAction('aDDeleteView').Enabled := MainAction('aDDeleteView').Enabled and (ListView.Items[I].ImageIndex in [iiView]);
          MainAction('aDDeleteRoutine').Enabled := MainAction('aDDeleteRoutine').Enabled and (ListView.Items[I].ImageIndex in [iiProcedure, iiFunction]);
          MainAction('aDDeleteEvent').Enabled := MainAction('aDDeleteEvent').Enabled and (ListView.Items[I].ImageIndex in [iiEvent]);
          MainAction('aDDeleteTrigger').Enabled := MainAction('aDDeleteTrigger').Enabled and (ListView.Items[I].ImageIndex in [iiTrigger]);
          MainAction('aDDeleteKey').Enabled := MainAction('aDDeleteKey').Enabled and (ListView.Items[I].ImageIndex in [iiKey]);
          MainAction('aDDeleteField').Enabled := MainAction('aDDeleteField').Enabled and (ListView.Items[I].ImageIndex in [iiBaseField, iiVirtualField]);
          MainAction('aDDeleteForeignKey').Enabled := MainAction('aDDeleteForeignKey').Enabled and (ListView.Items[I].ImageIndex in [iiForeignKey]);
          MainAction('aDEditDatabase').Enabled := MainAction('aDEditDatabase').Enabled and (ListView.Items[I].ImageIndex in [iiDatabase]);
          MainAction('aDEditTable').Enabled := MainAction('aDEditTable').Enabled and (ListView.Items[I].ImageIndex in [iiBaseTable]);
          MainAction('aDEditKey').Enabled := MainAction('aDEditKey').Enabled and (ListView.Items[I].ImageIndex in [iiKey]);
          MainAction('aDEditField').Enabled := MainAction('aDEditField').Enabled and (ListView.Items[I].ImageIndex in [iiBaseField, iiVirtualField]);
          MainAction('aDEditForeignKey').Enabled := MainAction('aDEditForeignKey').Enabled and (ListView.Items[I].ImageIndex in [iiForeignKey]);
          MainAction('aDEditTrigger').Enabled := MainAction('aDEditTrigger').Enabled and (ListView.Items[I].ImageIndex in [iiTrigger]);
          MainAction('aDEmpty').Enabled := MainAction('aDEmpty').Enabled and (ListView.Items[I].ImageIndex in [iiDatabase, iiBaseTable, iiBaseField]);
          aDDelete.Enabled := aDDelete.Enabled and (ListView.Items[I].ImageIndex in [iiDatabase, iiBaseTable, iiView, iiProcedure, iiFunction, iiTrigger, iiEvent, iiKey, iiBaseField, iiVirtualField, iiForeignKey, iiUser]);
        end;
    end
    else if (View = vObjects) then
    begin
      FNavigatorChanged(FNavigator, FNavigatorNodeByAddress(CurrentAddress));

      MainAction('aECopy').Enabled := False;
      MainAction('aERename').Enabled := False;
      MainAction('aDEmpty').Enabled := False;
      aDDelete.Enabled := False;

      case (CurrentClassIndex) of
        ciDatabase: mlEProperties.Action := MainAction('aDEditDatabase');
        ciBaseTable: mlEProperties.Action := MainAction('aDEditTable');
        ciView: mlEProperties.Action := MainAction('aDEditView');
        ciProcedure,
        ciFunction: mlEProperties.Action := MainAction('aDEditRoutine');
        ciTrigger: mlEProperties.Action := MainAction('aDEditTrigger');
        ciEvent: mlEProperties.Action := MainAction('aDEditEvent');
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

  if (Assigned(FSQLHistoryMenuNode) and (FSQLHistoryMenuNode.ImageIndex in [iiStatement, iiQuery])
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

procedure TFSession.miHStatementIntoSQLEditorClick(Sender: TObject);
var
  SelLength: Integer;
  SelStart: Integer;
begin
  Wanted.Clear();

  if (Assigned(FSQLHistoryMenuNode) and (FSQLHistoryMenuNode.ImageIndex in [iiStatement, iiQuery])) then
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

procedure TFSession.miNFavoriteAddClick(Sender: TObject);
var
  List: TList;
begin
  List := TList.Create();
  List.Add(CurrentData);
  FavoritesAdd(List);
  List.Free();
end;

procedure TFSession.miNFavoriteRemoveClick(Sender: TObject);
var
  Index: Integer;
begin
  Index := Session.Account.Favorites.IndexOf(TPAccount.TFavorite(FNavigatorMenuNode.Data));
  Session.Account.Favorites.Delete(Index);
end;

procedure TFSession.miNFavoriteOpenClick(Sender: TObject);
begin
  FNavigator.Selected := FNavigatorMenuNode;
end;

procedure TFSession.MListPopup(Sender: TObject);
var
  I: Integer;
  Rect: TRect;
begin
  ListViewSelectItem(ActiveListView, ActiveListView.Selected, Assigned(ActiveListView.Selected));

  if (not BOOL(Header_GetItemRect(ListView_GetHeader(ActiveListView.Handle), 0, @Rect)) or (MList.PopupPoint.Y - ActiveListView.ClientOrigin.Y < Rect.Bottom)) then
    for I := 0 to MList.Items.Count - 1 do
      MList.Items[I].Visible := False;
end;

procedure TFSession.mlOpenClick(Sender: TObject);
var
  URI: TUURI;
begin
  Wanted.Clear();

  URI := TUURI.Create(CurrentAddress);
  case (ActiveListView.Selected.ImageIndex) of
    iiForeignKey:
      begin
        URI.Database := TSForeignKey(ActiveListView.Selected.Data).Parent.DatabaseName;
        URI.Table := TSForeignKey(ActiveListView.Selected.Data).Parent.TableName;
      end;
    else
      URI.Address := AddressByData(ActiveListView.Selected.Data);
  end;
  CurrentAddress := URI.Address;
  URI.Free();
end;

procedure TFSession.MNavigatorPopup(Sender: TObject);
var
  AllowChange: Boolean;
  P: TPoint;
  URI: TUURI;
begin
  KillTimer(Handle, tiStatusBar);
  KillTimer(Handle, tiNavigator);

  if (Sender = FNavigator.PopupMenu) then
  begin
    // On click to the whitespace, FNavigator.Selected is set to the last selected node. :-/
    P := GetClientOrigin();
    FNavigatorMenuNode := FNavigator.GetNodeAt(MNavigator.PopupPoint.X - P.x - (PSideBar.Left + PNavigator.Left + FNavigator.Left), MNavigator.PopupPoint.y - P.y - (PSideBar.Top + PNavigator.Top + FNavigator.Top));
  end
  else
    FNavigatorMenuNode := FNavigatorNodeByAddress(CurrentAddress);

  AllowChange := True;
  FNavigatorChanging(Sender, FNavigatorMenuNode, AllowChange);

  aPExpand.Enabled := Assigned(FNavigatorMenuNode) and not FNavigatorMenuNode.Expanded and FNavigatorMenuNode.HasChildren;
  aPCollapse.Enabled := Assigned(FNavigatorMenuNode) and FNavigatorMenuNode.Expanded and Assigned(FNavigatorMenuNode.Parent);

  FNavigatorChanged(Sender, FNavigatorMenuNode);

  URI := TUURI.Create(CurrentAddress);

  miNFavoriteAdd.Enabled := Assigned(FNavigatorMenuNode) and (FNavigatorMenuNode.ImageIndex = iiQuickAccess) and (URI.Database <> '');
  miNFavoriteOpen.Enabled := Assigned(FNavigatorMenuNode) and Assigned(FNavigatorMenuNode.Parent) and (FNavigatorMenuNode.Parent.ImageIndex = iiQuickAccess);
  miNFavoriteRemove.Enabled := Assigned(FNavigatorMenuNode) and Assigned(FNavigatorMenuNode.Parent) and (FNavigatorMenuNode.Parent.ImageIndex = iiQuickAccess);

  ShowEnabledItems(MNavigator.Items);

  miNExpand.Default := miNExpand.Visible;
  miNCollapse.Default := miNCollapse.Visible;

  miNImport.Visible := miNImport.Enabled and Assigned(FNavigatorMenuNode) and (FNavigatorMenuNode.ImageIndex <> iiQuickAccess) and (not Assigned(FNavigatorMenuNode.Parent) or (FNavigatorMenuNode.Parent.ImageIndex <> iiQuickAccess));
  miNExport.Visible := miNExport.Enabled and Assigned(FNavigatorMenuNode) and (FNavigatorMenuNode.ImageIndex <> iiQuickAccess) and (not Assigned(FNavigatorMenuNode.Parent) or (FNavigatorMenuNode.Parent.ImageIndex <> iiQuickAccess));
  miNCopy.Visible := miNCopy.Enabled and Assigned(FNavigatorMenuNode) and (FNavigatorMenuNode.ImageIndex <> iiQuickAccess) and (not Assigned(FNavigatorMenuNode.Parent) or (FNavigatorMenuNode.Parent.ImageIndex <> iiQuickAccess));
  miNPaste.Visible := miNPaste.Enabled and Assigned(FNavigatorMenuNode) and (FNavigatorMenuNode.ImageIndex <> iiQuickAccess) and (not Assigned(FNavigatorMenuNode.Parent) or (FNavigatorMenuNode.Parent.ImageIndex <> iiQuickAccess));
  miNCreate.Visible := miNCreate.Enabled and Assigned(FNavigatorMenuNode) and (FNavigatorMenuNode.ImageIndex <> iiQuickAccess) and (not Assigned(FNavigatorMenuNode.Parent) or (FNavigatorMenuNode.Parent.ImageIndex <> iiQuickAccess));
  miNDelete.Visible := miNDelete.Enabled and Assigned(FNavigatorMenuNode) and (FNavigatorMenuNode.ImageIndex <> iiQuickAccess) and (not Assigned(FNavigatorMenuNode.Parent) or (FNavigatorMenuNode.Parent.ImageIndex <> iiQuickAccess));
  miNEmpty.Visible := miNEmpty.Enabled and Assigned(FNavigatorMenuNode) and (FNavigatorMenuNode.ImageIndex <> iiQuickAccess) and (not Assigned(FNavigatorMenuNode.Parent) or (FNavigatorMenuNode.Parent.ImageIndex <> iiQuickAccess));
  miNRename.Visible := miNRename.Enabled and Assigned(FNavigatorMenuNode) and (FNavigatorMenuNode.ImageIndex <> iiQuickAccess) and (not Assigned(FNavigatorMenuNode.Parent) or (FNavigatorMenuNode.Parent.ImageIndex <> iiQuickAccess));
  miNProperties.Visible := miNProperties.Enabled and Assigned(FNavigatorMenuNode) and (FNavigatorMenuNode.ImageIndex <> iiQuickAccess) and (not Assigned(FNavigatorMenuNode.Parent) or (FNavigatorMenuNode.Parent.ImageIndex <> iiQuickAccess));

  URI.Free();
end;

procedure TFSession.MSQLEditorPopup(Sender: TObject);
var
  I: Integer;
begin
  // Debug 2017-01-05
  if (not Assigned(ActiveSynMemo)) then
    raise ERangeError.Create('Address: ' + CurrentAddress + #13#10
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
    // On click to the whitespace, FSQLHistory.Selected is set to the last selected node. :-/
    Point := GetClientOrigin();
    FSQLHistoryMenuNode := FSQLHistory.GetNodeAt(MSQLHistory.PopupPoint.x - Point.x - (PSideBar.Left + PSQLHistory.Left + FSQLHistory.Left), MSQLHistory.PopupPoint.y - Point.y - (PSideBar.Top + PSQLHistory.Top + FSQLHistory.Top));
  end
  else if (Assigned(FSQLHistory.Selected)) then
    FSQLHistoryMenuNode := FSQLHistory.Selected;

  MainAction('aECopy').Enabled := Assigned(FSQLHistoryMenuNode) and (FSQLHistoryMenuNode.ImageIndex in [iiStatement, iiQuery]);

  miHStatementIntoSQLEditor.Enabled := Assigned(FSQLHistoryMenuNode) and (View in [vEditor, vEditor2, vEditor3]) and (FSQLHistoryMenuNode.ImageIndex in [iiStatement, iiQuery]);
  aPExpand.Enabled := Assigned(FSQLHistoryMenuNode) and not FSQLHistoryMenuNode.Expanded and FSQLHistoryMenuNode.HasChildren;
  aPCollapse.Enabled := Assigned(FSQLHistoryMenuNode) and FSQLHistoryMenuNode.Expanded;
  miHOpen.Enabled := Assigned(FSQLHistoryMenuNode) and (FSQLHistoryMenuNode.ImageIndex in [iiStatement, iiQuery]);
  miHRun.Enabled := Assigned(FSQLHistoryMenuNode) and (FSQLHistoryMenuNode.ImageIndex in [iiStatement, iiQuery]);
  miHProperties.Enabled := Assigned(FSQLHistoryMenuNode) and (FSQLHistoryMenuNode.ImageIndex in [iiStatement, iiQuery]) and not FSQLHistoryMenuNode.HasChildren;

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
  mtObjectSearch.Checked := ttObjectSearch in Preferences.ToolbarTabs;

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
  mwEDelete.Enabled := (ActiveWorkbench.SelCount >= 1);
  for I := 0 to ActiveWorkbench.ControlCount - 1 do
    if ((ActiveWorkbench.Controls[I] is TWForeignKey)
      and TWForeignKey(ActiveWorkbench.Controls[I]).Selected) then
      mwEDelete.Enabled := False;
  mwEDelete.Caption := Preferences.LoadStr(559);


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

  aDDelete.Enabled := ActiveWorkbench.SelCount >= 1;

  ShowEnabledItems(MWorkbench.Items);
end;

function TFSession.ObjectSearchFinish(): Boolean;
begin
  if (Assigned(ObjectSearch) and (ObjectSearch.Count = 0)) then
    MsgBox(Preferences.LoadStr(533, ObjectSearch.Text), Preferences.LoadStr(43), MB_OK + MB_ICONINFORMATION);

  Result := True; // For compiler warning only...
end;

function TFSession.ObjectSearchStep2(): Boolean;
begin
  if (Assigned(ObjectSearch)) then
  begin
    if (ObjectSearch.Step2()) then
      ObjectSearchFinish()
    else
      Wanted.Update := ObjectSearchFinish;
  end;

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

      DImport.Execute();

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
          URI := TUURI.Create(CurrentAddress);
          URI.Param['file'] := EscapeURL(SQLEditors[View].Filename);
          if (SQLEditors[View].FileCodePage = 0) then
            URI.Param['cp'] := Null
          else
            URI.Param['cp'] := IntToStr(SQLEditors[View].FileCodePage);
          FCurrentAddress := URI.Address;
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
    if (Assigned(TPanel(Sender).OnMouseMove)) then
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
  Rect: TRect;
begin
  if ((Button = mbLeft)
    and (Sender is TPanel_Ext) and (Sender <> PHeader)
    and Assigned(CloseButtonNormal)) then
  begin
    Rect.Left := GetSystemMetrics(SM_CXEDGE);
    Rect.Top := GetSystemMetrics(SM_CYEDGE);
    Rect.Width := CloseButtonNormal.Width;
    Rect.Height := CloseButtonNormal.Width;

    if (PtInRect(Rect, Point(X, Y)) and PtInRect(Rect, PanelMouseDownPoint)) then
      if (Sender = PResultHeader) then
      begin
        SResult.Visible := False;
        PResult.Visible := False;
        case (View) of
          vIDE: if (CurrentClassIndex in [ciProcedure, ciFunction]) then Desktop(TSRoutine(CurrentData)).CloseIDEResult();
          vBuilder: Desktop(TSDatabase(CurrentData)).CloseBuilderResult();
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

                DExecutingSQL.Update := TSEntities(SourceDatabase.Tables).Update;
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
                            DExecutingSQL.Session := SourceSession;
                            DExecutingSQL.Update := SourceTable.Update;
                            if (SourceTable.Valid or DExecutingSQL.Execute()) then
                            begin
                              Name := Session.TableName(CopyName(SourceTable.Name, Database.Tables));

                              Session.Connection.BeginSynchron();
                              Success := Database.CloneTable(SourceTable, Name, DPaste.Data);
                              Session.Connection.EndSynchron();
                            end;
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
                          DExecutingSQL.Session := SourceSession;
                          DExecutingSQL.Update := SourceView.Update;
                          if (SourceView.Valid or DExecutingSQL.Execute()) then
                          begin
                            Name := CopyName(SourceView.Name, Database.Tables);
                            if (Session.LowerCaseTableNames = 1) then
                              Name := LowerCase(Name);

                            Session.Connection.BeginSynchron();
                            Success := Database.CloneTable(SourceView, Name, False);
                            Session.Connection.EndSynchron();
                          end;
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
                          DExecutingSQL.Session := SourceSession;
                          DExecutingSQL.Update := SourceRoutine.Update;
                          if (SourceRoutine.Valid or DExecutingSQL.Execute()) then
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
                        end;
                      end
                      else if (StringList.Names[I] = 'Function') then
                      begin
                        SourceRoutine := SourceDatabase.FunctionByName(StringList.ValueFromIndex[I]);

                        if (not Assigned(SourceRoutine)) then
                          MessageBeep(MB_ICONERROR)
                        else
                        begin
                          DExecutingSQL.Session := SourceSession;
                          DExecutingSQL.Update := SourceRoutine.Update;
                          if (SourceRoutine.Valid or DExecutingSQL.Execute()) then
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
          end;
        iiBaseTable:
          begin
            SourceDatabase := SourceSession.DatabaseByName(SourceURI.Database);

            if (not Assigned(SourceDatabase)) then
              MessageBeep(MB_ICONERROR)
            else
            begin
              DExecutingSQL.Session := SourceSession;
              DExecutingSQL.Update := TSEntities(SourceDatabase.Tables).Update;
              if (SourceDatabase.Tables.Valid or DExecutingSQL.Execute()) then
              begin
                SourceTable := SourceDatabase.BaseTableByName(SourceURI.Table);

                if (Assigned(SourceTable)) then
                  DExecutingSQL.Update := SourceTable.Update;
                if (not Assigned(SourceTable) or not SourceTable.Valid and not DExecutingSQL.Execute()) then
                  MessageBeep(MB_ICONERROR)
                else
                begin
                  Table := TSBaseTable(Node.Data);
                  Database := Table.Database;

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
                          NewField := TSBaseField.Create(NewTable.Fields);
                          NewField.Assign(SourceField);
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
    Top: Integer;
  begin
    if (Control.Top >= 0) then Top := Control.Top else Top := 0;

try
    SendMessage(Control.Handle, WM_MOVE, 0, MAKELPARAM(Control.Left, Top));
except
  on E: Exception do
    raise ERangeError.Create('Left: ' + IntToStr(Control.Left) + #13#10
      + 'Top: ' + IntToStr(Top) + #13#10
      + 'Name: ' + Control.Name + #13#10
      + 'ClassType: ' + Control.ClassName + #13#10
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
        + 'SBlob = SBlobDebug: ' + BoolToStr(SBlob = SBlobDebug, True) + #13#10
        + 'Assigned(PBlob): ' + BoolToStr(Assigned(PBlob), True));
      // 2017-01-04: DROP TABLE was the last stmt
      // 2017-01-10: DROP TABLE (generated by MF) was the last stmt
      // 2017-01-10: DROP TABLE (generated by MF) was the last stmt
      // 2017-01-11: DROP TABLE (generated by MF) was the last stmt
      // 2017-01-26: DROP TABLE (generated by MF) was the last stmt
      // 2017-01-26: DROP DATABASE was in the log, but not the last stmt
      // 2017-02-04: DROP TABLE (generated by MF) was the last stmt

    SBlob.Align := alNone;
    PBlob.Align := alNone;

    PBlob.Visible := False;


    EnableAligns(PContent);

    if (View in [vObjects, vObjectSearch]) then ActiveListView := GetActiveListView() else ActiveListView := nil;
    if (View in [vIDE]) then ActiveIDEInputDataSet := GetActiveIDEInputDataSet() else ActiveIDEInputDataSet := nil;
    if (View in [vBrowser, vIDE, vBuilder, vEditor, vEditor2, vEditor3]) then ActiveSynMemo := GetActiveSynMemo() else ActiveSynMemo := nil;
    if (View in [vBrowser, vIDE, vBuilder, vEditor, vEditor2, vEditor3]) then ActiveDBGrid := GetActiveDBGrid() else ActiveDBGrid := nil;
    if (View in [vDiagram]) then ActiveWorkbench := GetActiveWorkbench() else ActiveWorkbench := nil;

    if ((View = vBrowser) and (TObject(CurrentData) is TSTable)) then
    begin
      FUDOffset.Position := 0;
      FUDLimit.Position := Desktop(TSTable(CurrentData)).Limit;
      FLimitEnabled.Down := Desktop(TSTable(CurrentData)).Limited;

      PDataBrowser.Top := 0;
      PDataBrowser.Align := alTop;
      PDataBrowser.Visible := True;
    end
    else
      PDataBrowser.Visible := False;

    if (Assigned(ActiveIDEInputDataSet)) then
    begin
      PObjectIDETrigger.Visible := (CurrentClassIndex = ciTrigger);
      PObjectIDEResize(Sender);

      PObjectIDE.Top := 0;
      PObjectIDE.Align := alTop;
      PObjectIDE.Visible := True;
    end
    else
      PObjectIDE.Visible := False;

    if (not Assigned(CurrentData)) then
      PResultVisible := True
    else
      case (View) of
        vBrowser:
          PResultVisible := True;
        vIDE:
          case (CurrentClassIndex) of
            ciProcedure,
            ciFunction:
              PResultVisible := Assigned(Desktop(TSRoutine(CurrentData)).ActiveDBGrid);
            else PResultVisible := False;
          end;
        vBuilder:
          PResultVisible := Assigned(Desktop(TSDatabase(CurrentData)).BuilderDBGrid);
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
    end
    else
      PWorkbench.Visible := False;

    if ((View = vBuilder) and (CurrentClassIndex in [ciSession, ciDatabase, ciSystemDatabase])) then
    begin
      PQueryBuilder.Align := alClient;
      PQueryBuilder.Visible := True;
    end
    else
      PQueryBuilder.Visible := False;

    if (Assigned(ActiveSynMemo)
      and ((View in [vEditor, vEditor2, vEditor3]) and (CurrentClassIndex in [ciSession, ciDatabase, ciSystemDatabase])
        or (View = vIDE) and (CurrentClassIndex in [ciView, ciFunction, ciProcedure, ciEvent, ciTrigger]))) then
    begin
      PSynMemo.Align := alClient;
      PSynMemo.Visible := True;
    end
    else
      PSynMemo.Visible := False;

    if ((View = vObjects)
      or ((View = vBrowser) and (CurrentClassIndex = ciSession))
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
begin
  // With higher DPI system, the width of the following components are not
  // applyed in a "frame" (Delphi XE4). So we calculate them...

  for I := 0 to PDataBrowser.ControlCount - 1 do
    if (PDataBrowser.Controls[I] <> PDataBrowserSpacer) then
      PDataBrowser.Controls[I].Height := PDataBrowser.ClientHeight - PDataBrowserSpacer.Height;

  FOffset.Left := 0;
  FOffset.Width := 40 * Screen.PixelsPerInch div USER_DEFAULT_SCREEN_DPI;
  FUDOffset.Left := FOffset.Left + FOffset.Width;
  FUDOffset.Width := 14 * Screen.PixelsPerInch div USER_DEFAULT_SCREEN_DPI;
  FLimit.Left := 55 * Screen.PixelsPerInch div USER_DEFAULT_SCREEN_DPI;
  FLimit.Width := 33 * Screen.PixelsPerInch div USER_DEFAULT_SCREEN_DPI;
  FUDLimit.Left := FLimit.Left + FLimit.Width;
  FUDLimit.Width := 14 * Screen.PixelsPerInch div USER_DEFAULT_SCREEN_DPI;
  TBLimitEnabled.Left := FUDLimit.Left + FUDLimit.Width;
  TBLimitEnabled.Width := 31 * Screen.PixelsPerInch div USER_DEFAULT_SCREEN_DPI;

  TBQuickSearchEnabled.Width := 31 * Screen.PixelsPerInch div USER_DEFAULT_SCREEN_DPI;
  TBQuickSearchEnabled.Left := PDataBrowser.ClientWidth - TBQuickSearchEnabled.Width - GetSystemMetrics(SM_CXVSCROLL);
  TBFilterEnabled.Width := TBFilterEnabled.Height;
  FQuickSearch.Width := 130 * Screen.PixelsPerInch div USER_DEFAULT_SCREEN_DPI;
  FQuickSearch.Left := TBQuickSearchEnabled.Left - FQuickSearch.Width;

  TBFilterEnabled.Width := 31 * Screen.PixelsPerInch div USER_DEFAULT_SCREEN_DPI;
  TBFilterEnabled.Left := FQuickSearch.Left - TBFilterEnabled.Width;
  FFilter.Left := TBLimitEnabled.Left + TBLimitEnabled.Width;
  FFilter.Width := TBFilterEnabled.Left - FFilter.Left;
end;

procedure TFSession.PDBGridResize(Sender: TObject);
begin
  if (Assigned(ActiveDBGrid)) then
    ActiveDBGrid.Invalidate();
  if (Assigned(PDBGridFilter)) then
    PDBGridFilter.Hide();
end;

procedure TFSession.PHeaderCheckElements(Sender: TObject);
begin
  FObjectSearch.Visible := Assigned(Session)
    and (ttObjectSearch in Preferences.ToolbarTabs)
    and (Session.Connection.MySQLVersion >= 50002)
    and ((CurrentClassIndex <> ciUsers) or (Session.Connection.MySQLVersion >= 50107))
    and (FObjectSearch.Left > Toolbar.Left + Toolbar.Width + GetSystemMetrics(SM_CXFIXEDFRAME));
  TBObjectSearch.Visible := FObjectSearch.Visible;
  if (not FObjectSearch.Visible and Assigned(PObjectSearch)) then
  begin
    PObjectSearch.Hide();
    FObjectSearch.SelectAll();
  end;
end;

procedure TFSession.PHeaderPaint(Sender: TObject);
begin
  if (StyleServices.Enabled) then
  begin
    PHeader.Canvas.Pen.Color := SplitColor;
    PHeader.Canvas.MoveTo(0, PHeader.ClientHeight - 1);
    PHeader.Canvas.LineTo(PHeader.ClientWidth, PHeader.ClientHeight - 1);
  end;
end;

procedure TFSession.PHeaderResize(Sender: TObject);
begin
  TBObjectSearch.Left := PHeader.ClientWidth - GetSystemMetrics(SM_CXEDGE) - TBObjectSearch.Width;
  TBObjectSearch.Top := 0;
  TBObjectSearch.Height := Toolbar.Height;
  FObjectSearch.Left := TBObjectSearch.Left - FObjectSearch.Width;
  FObjectSearch.Top := 2 * Toolbar.BorderWidth;
  FObjectSearch.Height := Toolbar.Height - 2 * 2 * Toolbar.BorderWidth;
  if (Assigned(PObjectSearch)) then
  begin
    PObjectSearch.Hide();
    FObjectSearch.SelectAll();
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

  case (CurrentClassIndex) of
    ciView:
      begin
        View := TSView(CurrentData);

        NewView := TSView.Create(Database.Tables);
        NewView.Assign(View);

        NewView.Stmt := Trim(ActiveSynMemo.Text);

        Result := Database.UpdateView(View, NewView);

        NewView.Free();
      end;
    ciProcedure,
    ciFunction:
      begin
        Routine := TSRoutine(CurrentData);

        if (CurrentClassIndex = ciProcedure) then
          NewRoutine := TSProcedure.Create(Routine.Database.Routines)
        else
          NewRoutine := TSFunction.Create(Routine.Database.Routines);
        NewRoutine.Assign(Routine);
        NewRoutine.Source := Trim(ActiveSynMemo.Text);

        Result := Database.UpdateRoutine(Routine, NewRoutine);

        NewRoutine.Free();
      end;
    ciEvent:
      begin
        Event := TSEvent(CurrentData);

        NewEvent := TSEvent.Create(Database.Events);
        NewEvent.Assign(Event);

        NewEvent.Stmt := Trim(ActiveSynMemo.Text);

        Result := Database.UpdateEvent(Event, NewEvent);

        NewEvent.Free();
      end;
    ciTrigger:
      begin
        Trigger := TSTrigger(CurrentData);

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

function TFSession.QueryContinueDrag(fEscapePressed: BOOL;
  grfKeyState: Longint): HResult;
begin
  if (fEscapePressed) then
    Result := DRAGDROP_S_CANCEL
  else if (grfKeyState and MK_LBUTTON = 0) then
    Result := DRAGDROP_S_DROP
  else
    Result := S_OK;
end;

function TFSession.QuickAccessStep1(): Boolean;
var
  Addresses: TStringList;
  Count: Integer;
  FirstIndex: Integer;
  I: Integer;
  FrequentObjects: TStringList;
  RecentObjects: TStringList;
  Items: array of TSQuickAccess.TItem;
  URI1: TUURI;
  URI2: TUURI;
begin
  Addresses := TStringList.Create();
  FrequentObjects := TStringList.Create();
  RecentObjects := TStringList.Create();
  URI1 := TUURI.Create();
  URI2 := TUURI.Create();

  for I := 0 to Session.Account.Desktop.Addresses.Count - 1 do
  begin
    URI1.Address := Session.Account.Desktop.Addresses[I];

    URI2.Address := Session.Account.ExpandAddress('/');
    URI2.Database := URI1.Database;
    URI2.Table := URI1.Table;
    URI2.Param['objecttype'] := URI1.Param['objecttype'];
    URI2.Param['object'] := URI1.Param['object'];

    if ((URI2.Table <> '') and (URI2.Param['objecttype'] <> 'systemview') or (URI2.Param['object'] <> Null)) then
    begin
      if ((FrequentObjects.Count = 0) or (FrequentObjects.Names[FrequentObjects.Count - 1] <> ReplaceStr(URI2.Address, '=', QuickAccessListEscaper))) then
      begin
        if (not TryStrToInt(FrequentObjects.Values[ReplaceStr(URI2.Address, '=', QuickAccessListEscaper)], Count)) then
          Count := 0;
        FrequentObjects.Values[ReplaceStr(URI2.Address, '=', QuickAccessListEscaper)] := IntToStr(Count + 1);
      end;

      if (RecentObjects.IndexOf(URI2.Address) < 0) then
        RecentObjects.Add(URI2.Address);
    end;
  end;

  FirstIndex := Max(0, RecentObjects.Count - QuickAccessItemCount);
  for I := FirstIndex to Min(FirstIndex + QuickAccessItemCount, RecentObjects.Count) - 1 do
    Addresses.Add(RecentObjects[I]);

  FrequentObjects.CustomSort(QuickAccessFrequentObjectsCompare);
  FirstIndex := Max(0, FrequentObjects.Count - QuickAccessItemCount);
  for I := FirstIndex to Min(FirstIndex + QuickAccessItemCount, FrequentObjects.Count) - 1 do
    if (Addresses.IndexOf(FrequentObjects.Names[I]) < 0) then
      Addresses.Add(ReplaceStr(FrequentObjects.Names[I], QuickAccessListEscaper, '='));

  if (RecentObjects.Count > 0) then
  begin
    FirstIndex := Max(0, RecentObjects.Count - QuickAccessItemCount);
    for I := FirstIndex to Min(FirstIndex + QuickAccessItemCount, RecentObjects.Count) - 1 do
    begin
      URI1.Address := RecentObjects[I];

      SetLength(Items, Length(Items) + 1);
      Items[Length(Items) - 1].DatabaseName := URI1.Database;
      if (URI1.Param['object'] <> Null) then
        Items[Length(Items) - 1].Name := URI1.Param['object']
      else if (URI1.Table <> '') then
        Items[Length(Items) - 1].Name := URI1.Table
      else
        raise ERangeError.Create('Unknown object name for: ' + URI1.Address);
      if (URI1.Param['objecttype'] = 'view') then
        Items[Length(Items) - 1].ClassType := TSView
      else if (URI1.Param['objecttype'] = 'systemview') then
      else if (URI1.Param['objecttype'] = 'procedure') then
        Items[Length(Items) - 1].ClassType := TSProcedure
      else if (URI1.Param['objecttype'] = 'function') then
        Items[Length(Items) - 1].ClassType := TSFunction
      else if (URI1.Param['objecttype'] = 'trigger') then
        Items[Length(Items) - 1].ClassType := TSTrigger
      else if (URI1.Param['objecttype'] = 'event') then
        Items[Length(Items) - 1].ClassType := TSEvent
      else if (URI1.Table <> '') then
        Items[Length(Items) - 1].ClassType := TSBaseTable
      else
        raise ERangeError.Create('Unknown object type for: ' + URI1.Address);
    end;

    if (Session.QuickAccess.Step1(Items)) then
      QuickAccessStep2()
    else
      Wanted.Update := QuickAccessStep2;
  end;

  Addresses.Free();
  FrequentObjects.Free();
  RecentObjects.Free();
  URI1.Free();
  URI2.Free();

  Result := False; // ... to avoid compiler warnings only
end;

function TFSession.QuickAccessStep2(): Boolean;
begin
  Session.QuickAccess.PushBuildEvent();

  Result := False;
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
    BaseTable := TSBaseField(SItem).Table;

    NewBaseTable := TSBaseTable.Create(BaseTable.Database.Tables);
    NewBaseTable.Assign(BaseTable);
    NewBaseTable.KeyByCaption(SItem.Caption).Name := NewName;
    Result := BaseTable.Database.UpdateBaseTable(BaseTable, NewBaseTable);
    NewBaseTable.Free();
  end
  else if (SItem is TSBaseField) then
  begin
    BaseTable := TSBaseField(SItem).Table;

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
        URI := TUURI.Create(CurrentAddress);
        URI.Param['file'] := EscapeURL(SQLEditors[View].Filename);
        if (SQLEditors[View].FileCodePage = 0) then
          URI.Param['cp'] := Null
        else
          URI.Param['cp'] := IntToStr(SQLEditors[View].FileCodePage);
        FCurrentAddress := URI.Address;
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
      case (CurrentClassIndex) of
        ciProcedure,
        ciFunction:
          begin
            Desktop(TSRoutine(CurrentData)).CloseIDEResult();
            PContentChange(Sender);
            Session.Connection.SendSQL(SQL, Desktop(TSRoutine(CurrentData)).IDEResultEvent);
          end;
        ciEvent:
          Session.Connection.SendSQL(SQL);
      end;
    vBuilder:
      case (CurrentClassIndex) of
        ciDatabase,
        ciSystemDatabase:
          begin
            Desktop(TSDatabase(CurrentData)).CloseBuilderResult();
            PContentChange(Sender);
            Session.Connection.SendSQL(SQL, Desktop(TSDatabase(CurrentData)).BuilderResultEvent);
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

  function ApplyObjectRenamed(const ClassIndex: TClassIndex; var URI: TUURI): Boolean;
  var
    Database: TSDatabase;
  begin
    Result := False;

    Database := Session.DatabaseByName(URI.Database);
    if (Assigned(Database)) then
      case (ClassIndex) of
        ciDatabase:
          if (Session.Databases.NameCmp(URI.Database, Database.OriginalName) = 0) then
          begin
            URI.Database := Event.Item.Name;
            Result := True;
          end;
        ciBaseTable:
          if ((URI.Table <> '') and (URI.Param['objecttype'] = Null) and (URI.Param['object'] = Null)
            and (Session.Databases.NameCmp(URI.Database, Database.Name) = 0)
            and (Database.Tables.NameCmp(URI.Table, TSBaseTable(Event.Item).OriginalName) = 0)) then
          begin
            URI.Table := Event.Item.Name;
            Result := True;
          end;
        ciView:
          if ((URI.Table <> '') and (URI.Param['objecttype'] = 'view')
            and (Session.Databases.NameCmp(URI.Database, Database.Name) = 0)
            and (Database.Tables.NameCmp(URI.Table, TSView(Event.Item).OriginalName) = 0)) then
          begin
            URI.Table := Event.Item.Name;
            Result := True;
          end;
        ciProcedure:
          if ((URI.Param['objecttype'] = 'procedure')
            and (Session.Databases.NameCmp(URI.Database, Database.Name) = 0)
            and (Database.Routines.NameCmp(URI.Param['object'], TSProcedure(Event.Item).OriginalName) = 0)) then
          begin
            URI.Param['object'] := Event.Item.Name;
            Result := True;
          end;
        ciFunction:
          if ((URI.Param['objecttype'] = 'function')
            and (Session.Databases.NameCmp(URI.Database, Database.Name) = 0)
            and (Database.Routines.NameCmp(URI.Param['object'], TSFunction(Event.Item).OriginalName) = 0)) then
          begin
            URI.Param['object'] := Event.Item.Name;
            Result := True;
          end;
        ciTrigger:
          if ((URI.Param['objecttype'] = 'trigger')
            and (Session.Databases.NameCmp(URI.Database, Database.Name) = 0)
            and (Database.Triggers.NameCmp(URI.Param['object'], TSTrigger(Event.Item).OriginalName) = 0)) then
          begin
            URI.Param['object'] := Event.Item.Name;
            Result := True;
          end;
        ciEvent:
          if ((URI.Param['objecttype'] = 'event')
            and (Session.Databases.NameCmp(URI.Database, Database.Name) = 0)
            and (Database.Events.NameCmp(URI.Param['object'], TSEvent(Event.Item).OriginalName) = 0)) then
          begin
            URI.Param['object'] := Event.Item.Name;
            Result := True;
          end;
      end;
  end;

var
  ClassIndex: TClassIndex;
  Control: TWinControl;
  Database: TSDatabase;
  I: Integer;
  S: string;
  S1: string;
  S2: string;
  Table: TSTable;
  TempActiveControl: TWinControl;
  URI: TUURI;
  Profile: TProfile;
begin
  LeftMousePressed := False;

  TempActiveControl := Window.ActiveControl;

  ListViewUpdateCount := 0;
  CreateProfile(Profile);

  if (Assigned(Event)) then
  begin
    ProfilingPoint(Profile, 1);

    if (Event.EventType = etItemDeleted) then
    begin
      if (Assigned(FNavigatorMenuNode) and (Event.Item = TObject(FNavigatorMenuNode.Data))) then
        FNavigatorMenuNode := nil;
      if (Assigned(FNavigatorNodeToExpand) and (Event.Item = TObject(FNavigatorNodeToExpand.Data))) then
        FNavigatorNodeToExpand := nil;
    end;

    if (Event.EventType in [etItemDeleted, etItemRenamed]) then
    begin
      ClassIndex := ClassIndexByData(Event.Item);
      if (ClassIndex in [ciDatabase, ciBaseTable, ciView, ciProcedure, ciFunction, ciTrigger, ciEvent]) then
      begin
        URI := TUURI.Create();

        for I := Session.Account.Desktop.Addresses.Count - 1 downto 0 do
          if (ClassIndexByAddress(Session.Account.Desktop.Addresses[I]) = ClassIndex) then
          begin
            URI.Address := Session.Account.Desktop.Addresses[I];
            if ((DataByAddress(URI.Address) = Event.Item)
              or ((Event.Item is TSDatabase) and (Session.Databases.NameCmp(URI.Database, Event.Item.Name) = 0))) then
              case (Event.EventType) of
                etItemDeleted:
                  Session.Account.Desktop.Addresses.Delete(I);
                etItemRenamed:
                  if (ApplyObjectRenamed(ClassIndex, URI)) then
                    Session.Account.Desktop.Addresses[I] := URI.Address;
              end;
          end;

        for I := Session.Account.Favorites.Count - 1 downto 0 do
          if (DataByAddress(Session.Account.Favorites[I].Address) = Event.Item) then
            case (Event.EventType) of
              etItemDeleted:
                Session.Account.Favorites.Delete(I);
              etItemRenamed:
                Session.Account.Favorites[I].Address := AddressByData(Event.Item);
            end;

        if (Event.EventType = etItemRenamed) then
        begin
          URI.Address := FCurrentAddress;
          if (ApplyObjectRenamed(ClassIndex, URI)) then
          begin
            FCurrentAddress := URI.Address;
            AddressChanged(nil);
          end;
        end;

        URI.Free();
      end;
    end;

    ProfilingPoint(Profile, 2);

    if (Event.Items is TSItemSearch) then
      ListViewUpdate(Event, ObjectSearchListView)
    else if (Event.Items is TSQuickAccess) then
      ListViewUpdate(Event, QuickAccessListView)
    else
    begin
      ProfilingPoint(Profile, 3);
      if (Event.EventType in [etItemsValid, etItemValid, etItemCreated, etItemRenamed, etItemDeleted]) then
        FNavigatorUpdate(Event);

      ProfilingPoint(Profile, 4);

      if (Event.EventType in [etItemsValid, etItemValid, etItemCreated, etItemRenamed, etItemDeleted]) then
      begin
        if (Event.Items is TSDatabases) then
        begin
          ListViewUpdate(Event, ServerListView);
          if (Event.Sender is TSDatabase) then
            ListViewUpdate(Event, Desktop(TSDatabase(Event.Sender)).ListView);
        end
        else if (Event.Items is TSProcesses) then
        begin
          ListViewUpdate(Event, ServerListView);
          ListViewUpdate(Event, ProcessesListView);
        end
        else if (Event.Items is TSUsers) then
        begin
          ListViewUpdate(Event, ServerListView);
          ListViewUpdate(Event, UsersListView);
        end
        else if (Event.Items is TSVariables) then
        begin
          ListViewUpdate(Event, ServerListView);
          ListViewUpdate(Event, VariablesListView);
        end
        else if ((Event.Sender is TSDatabase) and not (Event.Items is TSTriggers)) then
        begin
          ListViewUpdate(Event, ServerListView);

          if (not (Event.Items is TSTriggers)) then
            ListViewUpdate(Event, Desktop(TSDatabase(Event.Sender)).ListView)
          else if (Event.EventType = etItemDeleted) then
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

        if (Assigned(QuickAccessListView) and (Event.EventType in [etItemValid, etItemRenamed, etItemDeleted])) then
          ListViewUpdate(Event, QuickAccessListView);

        if (Assigned(ObjectSearchListView)
          and ((Event.Items = TObject(ObjectSearchListView.Tag))
            or (Event.EventType in [etItemValid, etItemRenamed, etItemDeleted]) and (TSItemSearch(ObjectSearchListView.Tag).IndexOf(Event.Item) >= 0))) then
          ListViewUpdate(Event, ObjectSearchListView);
      end;

      ProfilingPoint(Profile, 5);

      if ((Event.EventType = etItemValid)) then
      begin
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

        if ((View = vBrowser) and (Event.Item = CurrentData)) then
          Wanted.Update := UpdateAfterAddressChanged;

        if ((View = vIDE) and ((Event.Item is TSView) or (Event.Item is TSFunction))) then
          PContentChange(nil);
      end;
    end;

    ProfilingPoint(Profile, 6);
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
    and ((Event.EventType in [etItemCreated, etItemRenamed])
      or (Event.EventType in [etItemValid]) and (Event.Item is TSObject) and not TSObject(Event.Item).Valid)
    and (Screen.ActiveForm = Window)
    and Wanted.Nothing) then
    Wanted.Update := Session.Update;

  if (ProfilingTime(Profile) > 1000) then
  begin
    S := 'SessionUpdate - '
      + 'EventType: ' + IntToStr(Ord(Event.EventType));
    if (Assigned(Event.Items)) then
      S := S
        + ', Items: ' + Event.Items.ClassName
        + ', Count: ' + IntToStr(Event.Items.Count);
    if (Event.Item is TSTable) then
      S := S
        + ', FieldCount: ' + IntToStr(TSTable(Event.Item).Fields.Count);
    S := S
      + ', ListViewUpdateCount: ' + IntToStr(ListViewUpdateCount) + #13#10
      + ProfilingReport(Profile);
    TimeMonitor.Append(S, ttDebug);
  end;
  CloseProfile(Profile);
end;

procedure TFSession.SetCurrentAddress(const AAddress: string);
begin
  ChangeCurrentAddress(AAddress);
end;

procedure TFSession.SetView(const AView: TView);
var
  ScrollPos: record
    Horz: Integer;
    Vert: Integer;
  end;
  URI: TUURI;
begin
  if (CurrentAddress <> '') then
    URI := TUURI.Create(CurrentAddress)
  else if (CurrentClassIndex = ciSession) then
    URI := TUURI.Create(Session.Account.ExpandAddress('/'))
  else if (TObject(CurrentData) is TSItem) then
    URI := TUURI.Create(AddressByData(CurrentData))
  else
    raise ERangeError.Create(SRangeError);

  case (AView) of
    vObjects:
      begin
        URI.Param['view'] := Null;
      end;
    vBrowser:
      begin
        if ((URI.Table = '') and (LastSelectedTable <> '')) then
          URI.Address := LastSelectedTable;

        URI.Param['view'] := 'browser';
      end;
    vIDE:
      begin
        if ((URI.Param['objecttype'] = Null) or (URI.Param['object'] = Null)
          and (LastSelectedObjectIDE <> '')
          and (not Assigned(Session.DatabaseByName(URI.Database))
            or not Assigned(Session.DatabaseByName(URI.Database).TableByName(URI.Table))
            or not (Session.DatabaseByName(URI.Database).TableByName(URI.Table) is TSView))) then
          URI.Address := LastSelectedObjectIDE;
        URI.Param['view'] := 'ide';
      end;
    vBuilder:
      begin
        if ((URI.Database = '') and (LastSelectedDatabase <> '')) then
          URI.Address := LastSelectedDatabase;
        URI.Table := '';
        URI.Param['view'] := 'builder';
      end;
    vDiagram:
      begin
        if ((URI.Database = '') and (LastSelectedDatabase <> '')) then
          URI.Address := LastSelectedDatabase;
        URI.Table := '';
        URI.Param['view'] := 'diagram';
      end;
    vEditor,
    vEditor2,
    vEditor3:
      begin
        if ((URI.Database = '') and (LastSelectedDatabase <> '')) then
          URI.Address := LastSelectedDatabase;
        if ((URI.Database = '') and (LastSelectedDatabase <> '')) then
          URI.Database := Session.Connection.DatabaseName;
        URI.Table := '';
        if (not Assigned(SQLEditors[AView]) or (SQLEditors[AView].Filename = '')) then
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
        URI.Param['view'] := ViewToParam(AView);
      end;
    else
      raise ERangeError.Create(SRangeError);
  end;

  if (not (AView in [vObjects])) then
  begin
    URI.Param['system'] := Null;
  end;
  if (not (AView in [vBrowser])) then
  begin
    URI.Param['filter'] := Null;
    URI.Param['search'] := Null;
    URI.Param['offset'] := Null;
  end;
  if (not (AView in [vIDE])) then
  begin
    URI.Param['objecttype'] := Null;
    URI.Param['object'] := Null;
  end;
  if (not (AView in [vEditor, vEditor2, vEditor3])) then
  begin
    URI.Param['file'] := Null;
    URI.Param['cp'] := Null;
  end;
  if (not (AView in [vObjectSearch])) then
  begin
    URI.Param['text'] := Trim(FObjectSearch.Text);
    URI.Param['databases'] := Null;
    URI.Param['tables'] := Null;
    URI.Param['routines'] := Null;
    URI.Param['events'] := Null;
    URI.Param['fields'] := Null;
    URI.Param['triggers'] := Null;
    URI.Param['name'] := Null;
    URI.Param['comment'] := Null;
  end;

  LockWindowUpdate(FNavigator.Handle);
  ScrollPos.Horz := GetScrollPos(FNavigator.Handle, SB_HORZ);
  ScrollPos.Vert := GetScrollPos(FNavigator.Handle, SB_VERT);
  CurrentAddress := URI.Address;
  SetScrollPos(FNavigator.Handle, SB_HORZ, ScrollPos.Horz, TRUE);
  SetScrollPos(FNavigator.Handle, SB_VERT, ScrollPos.Vert, TRUE);
  LockWindowUpdate(0);

  URI.Free();
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
    IDABORT:
      begin
        Action := daAbort;
        PostMessage(Handle, UM_ACTIVATE_DBGRID, 0, LPARAM(ActiveDBGrid));

        // Debug 2017-01-02
        if (not Assigned(ActiveDBGrid)) then
          raise ERangeError.Create('CurrentAddress: ' + CurrentAddress);
        if (not ActiveDBGrid.Visible) then
          raise ERangeError.Create('CurrentAddress: ' + CurrentAddress);
        if (not ActiveDBGrid.Enabled) then
          raise ERangeError.Create('CurrentAddress: ' + CurrentAddress);
      end;
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
  Control: TWinControl; // Debug 2017-02-12
  Count: Integer;
  QueryBuilderWorkArea: TacQueryBuilderWorkArea;
  SelCount: Integer;
begin
  Control := Window.ActiveControl;

  if (Assigned(StatusBar) and (Immediately or (fsActive in FrameState)) and not (csDestroying in ComponentState) and Assigned(Window)) then
  begin
    if (not Assigned(Window.ActiveControl)) then
      StatusBar.Panels[sbNavigation].Text := ''
    else if (Window.ActiveControl = ActiveSynMemo) then
      StatusBar.Panels[sbNavigation].Text := IntToStr(ActiveSynMemo.CaretXY.Line) + ':' + IntToStr(ActiveSynMemo.CaretXY.Char)
    else if (Window.ActiveControl = ActiveListView) then
    begin
      // Debug 2017-02-12
      Assert(Window.ActiveControl = Control);
      Assert(not Assigned(ActiveListView.Selected) or Assigned(ActiveListView.Selected.Data),
        'Count: ' + IntToStr(ActiveListView.Items.Count));
    
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
      else if ((View = vBrowser) and (CurrentClassIndex in [ciBaseTable]) and Assigned(CurrentData) and not Session.Connection.InUse() and TSBaseTable(CurrentData).ValidData and TSBaseTable(CurrentData).DataSet.LimitedDataReceived and (TSBaseTable(CurrentData).RecordCount >= 0)) then
        StatusBar.Panels[sbSummarize].Text := Preferences.LoadStr(889, FormatFloat('#,##0', Count, LocaleFormatSettings), FormatFloat('#,##0', TSBaseTable(CurrentData).RecordCount, LocaleFormatSettings))
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
                    if (Assigned(Database) and Assigned(Database.Routines)) then
                      for J := 0 to Database.Routines.Count - 1 do
                        if (Database.Routines[J] is TSFunction) then
                          SynCompletionListAdd(
                            Database.Routines[J].Name,
                            Session.Connection.EscapeIdentifier(Database.Routines[J].Name));
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
//                  ditConst:
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
    if (not Preferences.Editor.CurrRowBGColorEnabled) then
      SynMemo.ActiveLineColor := clNone
    else
      SynMemo.ActiveLineColor := Preferences.Editor.CurrRowBGColor;
    SynMemo.Font.Name := Preferences.SQLFontName;
    SynMemo.Font.Color := Preferences.SQLFontColor;
    SynMemo.Font.Size := Preferences.SQLFontSize;
    SynMemo.Font.Charset := Preferences.SQLFontCharset;
    SynMemo.Gutter.Font.Name := SynMemo.Font.Name;
    SynMemo.Gutter.Font.Style := Preferences.Editor.LineNumbersStyle;
    SynMemo.Gutter.Font.Color := SynMemo.Font.Color;
    SynMemo.Gutter.Font.Size := SynMemo.Font.Size;
    SynMemo.Gutter.Font.Charset := SynMemo.Font.Charset;
    SynMemo.Gutter.Visible := FSQLEditorSynMemo.Gutter.Visible;
    if (Preferences.Editor.LineNumbersBackground = clNone) then
      SynMemo.Gutter.Color := clBtnFace
    else
      SynMemo.Gutter.Color := Preferences.Editor.LineNumbersBackground;
    SynMemo.Highlighter := FSQLEditorSynMemo.Highlighter;
    SynMemo.Options := FSQLEditorSynMemo.Options;
    SynMemo.RightEdge := FSQLEditorSynMemo.RightEdge;
    SynMemo.TabWidth := FSQLEditorSynMemo.TabWidth;
    SynMemo.WantTabs := FSQLEditorSynMemo.WantTabs;
    SynMemo.WordWrap := Preferences.Editor.WordWrap;
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
    Accept := MouseDownNode.ImageIndex in [iiDatabase, iiSystemDatabase, iiBaseTable, iiView, iiSystemView, iiProcedure, iiFunction, iiEvent, iiTrigger, iiKey, iiBaseField, iiVirtualField, iiSystemViewField, iiViewField, iiForeignKey]
  else if (Source = FSQLHistory) then
    Accept := MouseDownNode.ImageIndex in [iiStatement, iiQuery]
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

function TFSession.SynMemoDrop(const SynMemo: TSynMemo; const dataObj: IDataObject;
  grfKeyState: Longint; pt: TPoint; var dwEffect: Longint): HResult;
var
  Format: FORMATETC;
  HasFormat: Boolean;
  Medium: STGMEDIUM;
  Text: string;
begin
  if (dwEffect <> DROPEFFECT_COPY) then
    Result := E_INVALIDARG
  else
  begin
    Text := '';

    Format.ptd := nil;
    Format.dwAspect := DVASPECT_CONTENT;
    Format.lindex := -1;
    Format.tymed := TYMED_HGLOBAL;

    Format.cfFormat := CF_MYSQLSQLDATA;
    HasFormat := dataObj.QueryGetData(Format) = S_OK;
    if (HasFormat) then
    begin
      OleCheck(dataObj.GetData(Format, Medium));
      SetString(Text, PChar(GlobalLock(Medium.hGlobal)), GlobalSize(Medium.hGlobal) div SizeOf(Text[1]));
    end;
    if (not HasFormat) then
    begin
      Format.cfFormat := CF_UNICODETEXT;
      HasFormat := dataObj.QueryGetData(Format) = S_OK;
      if (HasFormat) then
      begin
        OleCheck(dataObj.GetData(Format, Medium));
        SetString(Text, PChar(GlobalLock(Medium.hGlobal)), GlobalSize(Medium.hGlobal) div SizeOf(Text[1]));
      end;
    end;

    if (Text <> '') then
      SynMemo.SelText := Text;

    if (not Assigned(Medium.unkForRelease)) then
      ReleaseStgMedium(Medium)
    else
      IUnknown(Medium.unkForRelease)._Release();

    if (not HasFormat) then
      Result := E_UNEXPECTED
    else
      Result := S_OK;
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
  MainAction('aEFormatSQL').Enabled := False;

  MainAction('aHIndex').ShortCut := ShortCut(VK_F1, []);
  MainAction('aHSQL').ShortCut := 0;

  SynCompletionPending.Active := False;
end;

procedure TFSession.SynMemoStatusChange(Sender: TObject; Changes: TSynStatusChanges);
var
  ClassIndex: TClassIndex; // Cache for speeding
  DDLStmt: TSQLDDLStmt;
  Empty: Boolean; // Cache for speeding
  Parse: TSQLParse;
  SelSQL: string; // Cache for speeding
  SQL: string; // Cache for speeding
begin
  if (not (csDestroying in ComponentState)) then
  begin
    KillTimer(Handle, tiShowSynCompletion);

    if (((scCaretX in Changes) or (scSelection in Changes) or (scModified in Changes) or (scAll in Changes)) and Assigned(ActiveSynMemo)) then
    begin
      SynCompletionPending.Active := False;

      SelSQL := ActiveSynMemo.SelText; // Cache for speeding
      if (View <> vIDE) then
      begin
        SQL := '';
        ClassIndex := ciUnknown;
      end
      else
      begin
        SQL := ActiveSynMemo.Text; // Cache for speeding
        ClassIndex := CurrentClassIndex; // Cache for speeding
      end;
      Empty := ((ActiveSynMemo.Lines.Count <= 1) and (ActiveSynMemo.Text = '')); // Cache for speeding

      MainAction('aFSave').Enabled := not Empty and (View in [vEditor, vEditor2, vEditor3]) and (SQLEditors[View].Filename = '');
      MainAction('aFSaveAs').Enabled := not Empty and (View in [vEditor, vEditor2, vEditor3]);
      MainAction('aERedo').Enabled := ActiveSynMemo.CanRedo;
      MainAction('aECopyToFile').Enabled := (SelSQL <> '');
      MainAction('aEPasteFromFile').Enabled := (View in [vEditor, vEditor2, vEditor3]);
      MainAction('aDPostObject').Enabled := (View = vIDE)
        and ActiveSynMemo.Modified
        and SQLSingleStmt(SQL)
        and ((ClassIndex in [ciView]) and SQLCreateParse(Parse, PChar(SQL), Length(SQL),Session.Connection.MySQLVersion) and (SQLParseKeyword(Parse, 'SELECT'))
          or (ClassIndex in [ciProcedure, ciFunction]) and SQLParseDDLStmt(DDLStmt, PChar(SQL), Length(SQL), Session.Connection.MySQLVersion) and (DDLStmt.DefinitionType = dtCreate) and (DDLStmt.ObjectType in [otProcedure, otFunction])
          or (ClassIndex in [ciEvent, ciTrigger]));
      MainAction('aDRun').Enabled :=
        ((View in [vEditor, vEditor2, vEditor3]) and not Empty
        or (View in [vBuilder]) and FQueryBuilder.Visible
        or (View in [vIDE]) and MainAction('aDPostObject').Enabled);
      MainAction('aDRunSelection').Enabled :=
        ((View in [vEditor, vEditor2, vEditor3]) and not Empty);
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
  Table := TSTable(CurrentData);

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
    if ((Table is TSBaseTable) and Assigned(TSBaseTable(Table).PrimaryKey)) then
      TSBaseTable(Table).PrimaryKey.GetSortDef(SortDef);
    Table.DataSet.AfterOpen := Desktop(Table).DataSetAfterOpen;
    Table.DataSet.AfterRefresh := Desktop(Table).DataSetAfterRefresh;
    if (Table is TSView) then
      Table.DataSet.BeforeOpen := Desktop(TSView(Table)).DataSetBeforeOpen;
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
  if (Sender = mtObjectSearch) then
    if (mtObjectSearch.Checked) then
      Exclude(Preferences.ToolbarTabs, ttObjectSearch)
    else
      Include(Preferences.ToolbarTabs, ttObjectSearch);

  PostMessage(Handle, UM_CHANGEPREFERENCES, 0, 0);

  PHeaderCheckElements(nil);
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

  if ((Sender is TTreeView) and Assigned(TTreeView(Sender).PopupMenu)) then
    ShowEnabledItems(TTreeView(Sender).PopupMenu.Items);
end;

procedure TFSession.TreeViewCollapsing(Sender: TObject;
  Node: TTreeNode; var AllowCollapse: Boolean);
begin
  Assert(Sender is TTreeView);
  Assert(Assigned(Node));

  if ((Sender is TTreeView) and Assigned(TTreeView(Sender).Selected) and Assigned(TTreeView(Sender).OnChange)) then
  begin
    if ((View = vBrowser)
      and not (Node.ImageIndex in [iiBaseTable, iiView, iiSystemView])
      and (Node = TTreeView(Sender).Selected.Parent)) then
      TTreeView(Sender).Selected := Node;

    AllowCollapse := Assigned(Node.Parent);
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

  if ((Sender is TTreeView) and Assigned(TTreeView(Sender).PopupMenu)) then
    ShowEnabledItems(TTreeView(Sender).PopupMenu.Items);
end;

procedure TFSession.TreeViewGetSelectedIndex(Sender: TObject; Node: TTreeNode);
begin
  Node.SelectedIndex := Node.ImageIndex;
end;

procedure TFSession.TreeViewMouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  LeftMousePressed := (Sender is TTreeView) and (Button = mbLeft);
  if (LeftMousePressed) then
    MouseDownNode := TTreeView(Sender).GetNodeAt(X, Y);
  if (Assigned(MouseDownNode) and (MouseDownNode.Text = '')) then
    MouseDownNode := nil;

  Exclude(FrameState, fsLoading);
end;

procedure TFSession.TreeViewMouseUp(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  LeftMousePressed := False;
end;

procedure TFSession.UMActivateDBGrid(var Msg: TMessage);
begin
  if (PResult.Visible and (TWinControl(Msg.LParam) = ActiveDBGrid) and ActiveDBGrid.Visible) then
  begin
    Window.ActiveControl := ActiveDBGrid;
    ActiveDBGrid.EditorMode := False;
  end;
end;

procedure TFSession.UMActivateFText(var Msg: TMessage);
const
  KEYEVENTF_UNICODE = 4;
var
  Input: TInput;
begin
  if (Msg.WParam <> 0) then
  begin
    ZeroMemory(@Input, SizeOf(Input));
    Input.Itype := INPUT_KEYBOARD;
    Input.ki.wVk := Msg.WParam;
    Input.ki.dwFlags := KEYEVENTF_UNICODE;
    SendInput(1, Input, SizeOf(Input));
  end;
  FText.SelStart := Length(FText.Text);
end;

procedure TFSession.UMChangePreferences(var Msg: TMessage);
var
  I: Integer;
  ItemEx: TTVItemEx;
  Node: TTreeNode;
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
  miNFavoriteAdd.Caption := Preferences.LoadStr(937);
  miNFavoriteOpen.Caption := Preferences.LoadStr(581);
  miNFavoriteRemove.Caption := Preferences.LoadStr(938);

  miHOpen.Caption := Preferences.LoadStr(581);
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

  ghmGoto.Caption := Preferences.LoadStr(676);
  ghmCopy.Caption := Preferences.LoadStr(64);
  ghmSelectAll.Caption := Preferences.LoadStr(572);

  mtObjects.Caption := tbObjects.Caption;
  mtBrowser.Caption := tbBrowser.Caption;
  mtIDE.Caption := tbIDE.Caption;
  mtBuilder.Caption := tbBuilder.Caption;
  mtDiagram.Caption := tbDiagram.Caption;
  mtEditor.Caption := tbEditor.Caption;
  mtEditor2.Caption := tbEditor2.Caption;
  mtEditor3.Caption := tbEditor3.Caption;
  mtObjectSearch.Caption := Preferences.LoadStr(934);

  tbObjects.Visible := ttObjects in Preferences.ToolbarTabs;
  tbBrowser.Visible := ttBrowser in Preferences.ToolbarTabs;
  tbIDE.Visible := ttIDE in Preferences.ToolbarTabs;
  tbBuilder.Visible := ttBuilder in Preferences.ToolbarTabs;
  tbDiagram.Visible := ttDiagram in Preferences.ToolbarTabs;
  tbEditor.Visible := ttEditor in Preferences.ToolbarTabs;
  tbEditor2.Visible := ttEditor2 in Preferences.ToolbarTabs;
  tbEditor3.Visible := ttEditor3 in Preferences.ToolbarTabs;
  PHeaderResize(nil);


  if (not Preferences.QuickAccessVisible and Assigned(FNavigator.Items.GetFirstNode()) and (FNavigator.Items.GetFirstNode().ImageIndex = iiQuickAccess)) then
  begin
    FNavigatorRemoveFavorites();
    FNavigator.Items.Delete(FNavigator.Items.GetFirstNode());
    FNavigator.Items.Delete(FNavigator.Items.GetFirstNode());
  end
  else if (Preferences.QuickAccessVisible and (not Assigned(FNavigator.Items.GetFirstNode()) or (FNavigator.Items.GetFirstNode().ImageIndex <> iiQuickAccess))) then
  begin
    Node := FNavigator.Items.AddFirst(nil, '');
    Node.ImageIndex := -1;
    FillChar(ItemEx, SizeOf(ItemEx), 0);
    ItemEx.mask := TVIF_STATEEX;
    ItemEx.hItem := Node.ItemId;
    ItemEx.uStateEx := 0;
    TreeView_SetItem(FNavigator.Handle, ItemEx);

    Node := FNavigator.Items.AddFirst(nil, '');
    Node.ImageIndex := iiQuickAccess;
    FavoritesEvent(Session.Account.Favorites);
  end;
  if (Assigned(FNavigator.Items.GetFirstNode()) and (FNavigator.Items.GetFirstNode().ImageIndex = iiQuickAccess)) then
    FNavigator.Items.GetFirstNode().Text := Preferences.LoadStr(939);

  if (not (fsLoading in FrameState)) then
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

procedure TFSession.UMCloseFrame(var Msg: TMessage);
begin
  MainAction('aFClose').Execute();
end;

procedure TFSession.UMCloseTabQuery(var Msg: TMessage);
var
  CanClose: Boolean;
  I: Integer;
  J: Integer;
  SObject: TSObject;
  SynMemo: TSynMemo;
  View: TView;
begin
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
                CurrentAddress := AddressByData(FNavigator.Items[J].Data);
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
        Window.ActiveControl := ActiveSynMemo;
        case (MsgBox(Preferences.LoadStr(584, ExtractFileName(SQLEditors[View].Filename)), Preferences.LoadStr(101), MB_YESNOCANCEL + MB_ICONQUESTION)) of
          IDYES: SaveSQLFile(MainAction('aFSave'));
          IDCANCEL: CanClose := False;
        end;
      end;

  for I := 0 to Session.Databases.Count - 1 do
    if (CanClose) then
    begin
      if (not Assigned(Session.Databases[I])) then
        raise ERangeError.Create(SRangeError);
      if (not Assigned(Desktop(Session.Databases[I]))) then
        raise ERangeError.Create('Database: ' + Session.Databases[I].Name);

      Desktop(Session.Databases[I]).CloseQuery(nil, CanClose);
    end;

  if (not CanClose) then
    Msg.Result := 0
  else
    Msg.Result := 1;
end;

procedure TFSession.UMFrameActivate(var Msg: TMessage);
begin
  Include(FrameState, fsActive);

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
    MainAction('aVBrowser').Enabled := (CurrentClassIndex in [ciBaseTable, ciView, ciSystemView, ciTrigger]) or (LastSelectedTable <> '');
    MainAction('aVIDE').Enabled := (CurrentClassIndex in [ciView, ciProcedure, ciFunction, ciEvent, ciTrigger]) or (LastSelectedObjectIDE <> '');
    MainAction('aVBuilder').Enabled := LastSelectedDatabase <> '';
    MainAction('aVSQLEditor').Enabled := True;
    MainAction('aVSQLEditor2').Enabled := True;
    MainAction('aVSQLEditor3').Enabled := True;
    MainAction('aVDiagram').Enabled := LastSelectedDatabase <> '';
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

procedure TFSession.UMFrameDeactivate(var Msg: TMessage);
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

  Exclude(FrameState, fsActive);
end;

procedure TFSession.UMPostBuilderQueryChange(var Msg: TMessage);
begin
  FQueryBuilderEditorPageControlCheckStyle();
end;

procedure TFSession.UMPostShow(var Msg: TMessage);
var
  AllowChange: Boolean;
  Node: TTreeNode;
  ServerNode: TTreeNode;
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
  PHeaderCheckElements(nil);

  Perform(UM_ACTIVATEFRAME, 0, 0);

  ServerNode := FNavigator.Items.Add(nil, Session.Caption);
  ServerNode.Data := Session;
  ServerNode.ImageIndex := iiServer;

  if (Assigned(Session.Processes)) then
  begin
    Node := FNavigator.Items.AddChild(ServerNode, '');
    Node.Data := Session.Processes;
    Node.ImageIndex := iiProcesses;
  end;
  if (Assigned(Session.Users)) then
  begin
    Node := FNavigator.Items.AddChild(ServerNode, '');
    Node.Data := Session.Users;
    Node.ImageIndex := iiUsers;
  end;
  if (Assigned(Session.Variables)) then
  begin
    Node := FNavigator.Items.AddChild(ServerNode, '');
    Node.Data := Session.Variables;
    Node.ImageIndex := iiVariables;
  end;

  ServerNode.Expand(False);

  FNavigatorInitialize(nil);


  if (Param <> '') then
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
    CurrentAddress := URI.Address;
    URI.Free();
  end
  else
  begin
    AllowChange := True;
    if (Session.Account.Desktop.Address = '') then
      AddressChanging(nil, Session.Account.ExpandAddress('/'), AllowChange)
    else
      AddressChanging(nil, Session.Account.Desktop.Address, AllowChange);
    Wanted.Address := Session.Account.Desktop.Address;
  end;
end;

procedure TFSession.UMStausBarRefresh(var Msg: TMessage);
begin
  StatusBarRefresh();
end;

procedure TFSession.UMSynCompletionTime(var Msg: TMessage);
begin
  // SetTimer must be called after SynMemoStatusChange
  SetTimer(Handle, Msg.WParam, Msg.LParam, nil);
end;

procedure TFSession.UMWantedSynchronize(var Msg: TMessage);
begin
  if (not (csDestroying in ComponentState)) then
    Wanted.Synchronize();
end;

function TFSession.UpdateAfterAddressChanged(): Boolean;
var
  B: Boolean;
  Database: TSDatabase;
  I: Integer;
  List: TList;
  URI: TUURI;
begin
  Result := False;

  case (View) of
    vObjects:
      case (CurrentClassIndex) of
        ciSession:
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
        ciDatabase,
        ciSystemDatabase:
          begin
            Database := TSDatabase(CurrentData);
            if (not Database.Tables.Update(True)) then
              Wanted.Update := UpdateAfterAddressChanged
            else if (Session.Connection.MySQLVersion < 50002) then
              Database.Update(True);
          end;
        ciBaseTable,
        ciView,
        ciSystemView:
          begin
            // Debug 2017-02-13
            Assert(Assigned(CurrentData),
              'CurrentAddress: ' + CurrentAddress);
            TSTable(CurrentData).Update();
          end;
        ciProcesses:
          Session.Processes.Update();
        ciUsers:
          Session.Users.Update();
        ciVariables:
          Session.Variables.Update();
        ciQuickAccess:
          QuickAccessStep1();
      end;
    vBrowser:
      if ((TObject(CurrentData) is TSView and not TSView(CurrentData).Update())
        or (TObject(CurrentData) is TSBaseTable and not TSBaseTable(CurrentData).Update(True))) then
        Wanted.Update := UpdateAfterAddressChanged
      else if ((TObject(CurrentData) is TSTable) and not TSTable(CurrentData).DataSet.Active) then
        TableOpen(nil);
    vIDE:
      TSDBObject(CurrentData).Update();
    vDiagram:
      if (not Assigned(ActiveWorkbench) and Assigned(CurrentData)) then
      begin
        Desktop(TSDatabase(CurrentData)).CreateWorkbench();
        ActiveWorkbench := GetActiveWorkbench();
        if (FileExists(Session.Account.DataPath + ActiveWorkbench.Database.Name + PathDelim + 'Diagram.xml')) then
          ActiveWorkbench.LoadFromFile(Session.Account.DataPath + ActiveWorkbench.Database.Name + PathDelim + 'Diagram.xml');
      end;
    vObjectSearch:
      begin
        if (not Assigned(ObjectSearch)) then
        begin
          URI := TUURI.Create(CurrentAddress);
          ObjectSearch := TSItemSearch.Create(Session);
          if (URI.Param['system'] = 'processes') then
            ObjectSearch.Location := Session.Processes
          else if (URI.Param['system'] = 'users') then
            ObjectSearch.Location := Session.Users
          else if (URI.Param['system'] = 'variables') then
            ObjectSearch.Location := Session.Variables
          else if (URI.Table <> '') then
            ObjectSearch.Location := Session.DatabaseByName(URI.Database).TableByName(URI.Table)
          else if (URI.Database <> '') then
            ObjectSearch.Location := Session.DatabaseByName(URI.Database)
          else
            ObjectSearch.Location := Session;
          ObjectSearch.Comment := (URI.Param['comment'] <> Null) and TryStrToBool(URI.Param['comment'], B) and B;
          ObjectSearch.Databases := (URI.Param['databases'] <> Null) and TryStrToBool(URI.Param['databases'], B) and B;
          ObjectSearch.Events := (URI.Param['events'] <> Null) and TryStrToBool(URI.Param['events'], B) and B;
          ObjectSearch.Fields := (URI.Param['fields'] <> Null) and TryStrToBool(URI.Param['fields'], B) and B;
          ObjectSearch.Name := (URI.Param['name'] <> Null) and TryStrToBool(URI.Param['name'], B) and B;
          ObjectSearch.Routines := (URI.Param['routines'] <> Null) and TryStrToBool(URI.Param['routines'], B) and B;
          ObjectSearch.Tables := (URI.Param['tables'] <> Null) and TryStrToBool(URI.Param['tables'], B) and B;
          ObjectSearch.Triggers := (URI.Param['triggers'] <> Null) and TryStrToBool(URI.Param['triggers'], B) and B;
          ObjectSearch.Text := URI.Param['text'];
          URI.Free();

          ActiveListView := GetActiveListView();

          if (ObjectSearch.Step1()) then
            ObjectSearchStep2()
          else
            Wanted.Update := ObjectSearchStep2;
        end;
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

  if ((View <> vObjectSearch) and Assigned(ObjectSearch)) then
  begin
    if (Assigned(ObjectSearchListView)) then
    begin
      ObjectSearchListView.Free();
      ObjectSearchListView := nil;
    end;

    ObjectSearch.Free();
    ObjectSearch := nil;
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
    vObjectSearch: Result := 'objectsearch';
    else Result := Null;
  end;
end;

procedure TFSession.WMNotify(var Msg: TWMNotify);
begin
  case (Msg.NMHdr^.code) of
    TVN_BEGINLABELEDIT,
    LVN_BEGINLABELEDIT: BeginEditLabel(Window.ActiveControl);
    TVN_ENDLABELEDIT,
    LVN_ENDLABELEDIT: EndEditLabel(Window.ActiveControl);
    LVN_ITEMCHANGING: NMListView := PNMListView(Msg.NMHdr);
  end;

  inherited;

  NMListView := nil;
end;

procedure TFSession.WMParentNotify(var Msg: TWMParentNotify);
var
  ClientPoint: TPoint;
  GridPoint: TPoint;
  GridCoord: TGridCoord;
  ScreenPoint: TPoint;
begin
  ClientPoint := Point(Msg.XPos, Msg.YPos);
  ScreenPoint := ClientToScreen(ClientPoint);

  if ((Msg.Event = WM_RBUTTONDOWN)
    and (ControlAtPos(ClientPoint, False, True) = PContent)
    and (PContent.ControlAtPos(PContent.ScreenToClient(ScreenPoint), False, True) = PResult)
    and (PResult.ControlAtPos(PResult.ScreenToClient(ScreenPoint), False, True) = ActiveDBGrid.Parent)
    and (ActiveDBGrid.Parent.ControlAtPos(ActiveDBGrid.Parent.ScreenToClient(ScreenPoint), False, True) = ActiveDBGrid)) then
  begin
    Window.ActiveControl := ActiveDBGrid;
    GridPoint := ActiveDBGrid.Parent.ScreenToClient(ScreenPoint);
    GridCoord := ActiveDBGrid.MouseCoord(GridPoint.X, GridPoint.Y);
    if ((GridCoord.X >= 0) and (GridCoord.Y = 0)) then
    begin
      ActiveDBGrid.PopupMenu := MGridHeader;
      MGridHeaderColumn := ActiveDBGrid.Columns[GridCoord.X];
    end
    else
    begin
      ActiveDBGrid.PopupMenu := MGrid;
      MGridHeaderColumn := nil;
    end;
  end;

  inherited;
end;

procedure TFSession.WMTimer(var Msg: TWMTimer);
begin
  case (Msg.TimerID) of
    tiHideSynCompletion:
      begin
        KillTimer(Handle, Msg.TimerID);
        SynCompletion.CancelCompletion();
      end;
    tiNavigator:
      begin
        KillTimer(Handle, Msg.TimerID);
        if (Window.Active) then
          FNavigatorChange2(FNavigator, FNavigator.Selected)
        else
          FNavigatorNodeAfterActivate := FNavigator.Selected;
      end;
    tiShowSynCompletion:
      begin
        KillTimer(Handle, Msg.TimerID);
        if (Window.Active and (View in [vEditor, vEditor2, vEditor3])) then
        begin
          SynCompletion.Form.CurrentEditor := ActiveSynMemo;
          SynCompletion.ActivateCompletion();
          PostMessage(Handle, UM_SYNCOMPLETION_TIMER, tiHideSynCompletion, 5000);
        end;
      end;
    tiStatusBar:
      begin
        KillTimer(Handle, Msg.TimerID);
        StatusBar.Panels[sbMessage].Text := '';
        StatusBarRefresh();
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
  if (not IsClipboardFormatAvailable(CF_MYSQLTABLE)) then
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
    and (FNavigatorNodeByAddress(CurrentAddress) = MouseDownNode.Parent) and (MouseDownNode.ImageIndex = iiBaseTable)) then
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
    and (FNavigatorNodeByAddress(CurrentAddress) = MouseDownNode.Parent) and (MouseDownNode.ImageIndex = iiBaseTable)) then
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

  if (not IsClipboardFormatAvailable(CF_MYSQLTABLE)) then
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

initialization
  OleCheck(OleInitialize(nil));
finalization
  OleUninitialize();
end.
