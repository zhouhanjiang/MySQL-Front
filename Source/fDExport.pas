unit fDExport;

interface {********************************************************************}

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms,
  Dialogs, ComCtrls, ExtCtrls, StdCtrls, DB, DBGrids,
  ODBCAPI,
  ComCtrls_Ext, Forms_Ext, StdCtrls_Ext, ExtCtrls_Ext, Dialogs_Ext,
  fClient, fPreferences, fBase, fTools;

type
  TDExport = class (TForm_Ext)
    FAccessFile: TRadioButton;
    FAllQuote: TRadioButton;
    FBBack: TButton;
    FBCancel: TButton;
    FBFilename: TButton;
    FBForward: TButton;
    FBHelp: TButton;
    FCreateDatabase: TCheckBox;
    FCSVHeadline: TCheckBox;
    FDatabaseAttribute: TEdit;
    FDatabaseTag: TEdit;
    FDatabaseTagDisabled: TRadioButton;
    FDatabaseTagFree: TRadioButton;
    FDatabaseTagName: TRadioButton;
    FDestField1: TEdit;
    FDisableKeys: TCheckBox;
    FDoneRecords: TLabel;
    FDoneTables: TLabel;
    FDoneTime: TLabel;
    FDrop: TCheckBox;
    FEnabled: TCheckBox;
    FEntieredRecords: TLabel;
    FEntieredTables: TLabel;
    FEntieredTime: TLabel;
    FErrorMessages: TRichEdit;
    FErrors: TLabel;
    FExcelFile: TRadioButton;
    FStartDate: TDateTimePicker;
    FStartTime: TDateTimePicker;
    FField1: TComboBox_Ext;
    FField2: TComboBox_Ext;
    FFieldAttribute: TEdit;
    FFieldTag: TEdit;
    FFieldTagFree: TRadioButton;
    FFieldTagName: TRadioButton;
    FFilename: TEdit;
    FHTMLData: TCheckBox;
    FHTMLFile: TRadioButton;
    FHTMLNullText: TCheckBox;
    FHTMLRowBGColorEnabled: TCheckBox;
    FHTMLShowMemoContent: TCheckBox;
    FHTMLStructure: TCheckBox;
    FL1DatabaseTagFree: TLabel;
    FL1FieldTagFree: TLabel;
    FL1TableTagFree: TLabel;
    FL2DatabaseTagFree: TLabel;
    FL2FieldTagFree: TLabel;
    FL2RecordTag: TLabel;
    FL2RootTag: TLabel;
    FL2TableTagFree: TLabel;
    FL3RecordTag: TLabel;
    FL3RootTag: TLabel;
    FLCSVHeadline: TLabel;
    FLDatabaseHandling: TLabel;
    FLDatabaseTag: TLabel;
    FLDestFields: TLabel;
    FLDone: TLabel;
    FLDrop: TLabel;
    FLEnabled: TLabel;
    FLEntiered: TLabel;
    FLErrors: TLabel;
    FLStart: TLabel;
    FLExportType: TLabel;
    FLFields: TLabel;
    FLFieldTag: TLabel;
    FLFilename: TLabel;
    FLGeneral: TLabel;
    FLHTMLBGColorEnabled: TLabel;
    FLHTMLNullValues: TLabel;
    FLHTMLViewDatas: TLabel;
    FLHTMLWhat: TLabel;
    FLName: TLabel;
    FLProgressRecords: TLabel;
    FLProgressTables: TLabel;
    FLProgressTime: TLabel;
    FLQuoteChar: TLabel;
    FLQuoteValues: TLabel;
    FLRecordTag: TLabel;
    FLReferrer1: TLabel;
    FLRootTag: TLabel;
    FLSeparator: TLabel;
    FLExecution: TLabel;
    FLSQLWhat: TLabel;
    FLTableTag: TLabel;
    FDaily: TRadioButton;
    FName: TEdit;
    FNoQuote: TRadioButton;
    FODBC: TRadioButton;
    FODBCSelect: TListView_Ext;
    FPDFFile: TRadioButton;
    FProgressBar: TProgressBar;
    FQuoteChar: TEdit;
    FRecordTag: TEdit;
    FReplaceData: TCheckBox;
    FRootTag: TEdit;
    FSelect: TTreeView_Ext;
    FSeparator: TEdit;
    FSeparatorChar: TRadioButton;
    FSeparatorTab: TRadioButton;
    FSingle: TRadioButton;
    FSQLData: TCheckBox;
    FSQLFile: TRadioButton;
    FSQLiteFile: TRadioButton;
    FSQLStructure: TCheckBox;
    FStringQuote: TRadioButton;
    FTableAttribute: TEdit;
    FTableTag: TEdit;
    FTableTagDisabled: TRadioButton;
    FTableTagFree: TRadioButton;
    FTableTagName: TRadioButton;
    FTextFile: TRadioButton;
    FUseDatabase: TCheckBox;
    FXMLFile: TRadioButton;
    GBasics: TGroupBox_Ext;
    GCSVOptions: TGroupBox_Ext;
    GErrors: TGroupBox_Ext;
    GFields: TGroupBox_Ext;
    GHTMLOptions: TGroupBox_Ext;
    GHTMLWhat: TGroupBox_Ext;
    GODBCSelect: TGroupBox_Ext;
    GProgress: TGroupBox_Ext;
    GSelect: TGroupBox_Ext;
    GSQLOptions: TGroupBox_Ext;
    GSQLWhat: TGroupBox_Ext;
    GTask: TGroupBox_Ext;
    GXMLHow: TGroupBox_Ext;
    PageControl: TPageControl;
    PDatabaseTag: TPanel_Ext;
    PErrorMessages: TPanel_Ext;
    PFieldTag: TPanel_Ext;
    PODBCSelect: TPanel_Ext;
    PQuote: TPanel_Ext;
    PSelect: TPanel_Ext;
    PSeparator: TPanel_Ext;
    PSQLWait: TPanel;
    PTableTag: TPanel_Ext;
    SaveDialog: TSaveDialog_Ext;
    ScrollBox: TScrollBox;
    TSCSVOptions: TTabSheet;
    TSExecute: TTabSheet;
    TSFields: TTabSheet;
    TSHTMLOptions: TTabSheet;
    TSJob: TTabSheet;
    TSODBCSelect: TTabSheet;
    TSSelect: TTabSheet;
    TSSQLOptions: TTabSheet;
    TSTask: TTabSheet;
    TSXMLOptions: TTabSheet;
    FWeekly: TRadioButton;
    FMonthly: TRadioButton;
    procedure FBBackClick(Sender: TObject);
    procedure FBCancelClick(Sender: TObject);
    procedure FBForwardClick(Sender: TObject);
    procedure FBHelpClick(Sender: TObject);
    procedure FDatabaseDblClick(Sender: TObject);
    procedure FDatabaseTagClick(Sender: TObject);
    procedure FDatabaseTagKeyPress(Sender: TObject; var Key: Char);
    procedure FDestField1Change(Sender: TObject);
    procedure FField1Change(Sender: TObject);
    procedure FField1Exit(Sender: TObject);
    procedure FFieldTagClick(Sender: TObject);
    procedure FFieldTagKeyPress(Sender: TObject; var Key: Char);
    procedure FHTMLDataClick(Sender: TObject);
    procedure FHTMLDataKeyPress(Sender: TObject; var Key: Char);
    procedure FHTMLStructureClick(Sender: TObject);
    procedure FHTMLStructureKeyPress(Sender: TObject; var Key: Char);
    procedure FODBCSelectChange(Sender: TObject; Item: TListItem;
      Change: TItemChange);
    procedure FODBCSelectDblClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormHide(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure FQuoteCharExit(Sender: TObject);
    procedure FQuoteClick(Sender: TObject);
    procedure FQuoteKeyPress(Sender: TObject; var Key: Char);
    procedure FSeparatorClick(Sender: TObject);
    procedure FSeparatorKeyPress(Sender: TObject; var Key: Char);
    procedure FSQLOptionClick(Sender: TObject);
    procedure FSQLOptionKeyPress(Sender: TObject; var Key: Char);
    procedure FTableTagClick(Sender: TObject);
    procedure FTableTagKeyPress(Sender: TObject; var Key: Char);
    procedure TSCSVOptionsShow(Sender: TObject);
    procedure TSExecuteShow(Sender: TObject);
    procedure TSFieldsShow(Sender: TObject);
    procedure TSHTMLOptionsShow(Sender: TObject);
    procedure TSODBCSelectShow(Sender: TObject);
    procedure TSOptionsHide(Sender: TObject);
    procedure TSSQLOptionsShow(Sender: TObject);
    procedure TSXMLOptionChange(Sender: TObject);
    procedure TSXMLOptionsHide(Sender: TObject);
    procedure TSXMLOptionsShow(Sender: TObject);
    procedure FJobOptionChange(Sender: TObject);
    procedure TSJobShow(Sender: TObject);
    procedure FSelectGetImageIndex(Sender: TObject; Node: TTreeNode);
    procedure FSelectExpanding(Sender: TObject; Node: TTreeNode;
      var AllowExpansion: Boolean);
    procedure FSelectChange(Sender: TObject; Node: TTreeNode);
    procedure TSSelectShow(Sender: TObject);
    procedure FBFilenameClick(Sender: TObject);
    procedure TSTaskShow(Sender: TObject);
    procedure FExportTypeChange(Sender: TObject);
  private
    Export: TTExport;
    FDestFields: array of TEdit;
    FFields: array of TComboBox_Ext;
    Filename: string;
    FLReferrers: array of TLabel;
    FObjects: TList;
    ODBC: SQLHDBC;
    ODBCEnv: SQLHENV;
    ProgressInfos: TTools.TProgressInfos;
    SQLWait: Boolean;
    Title: string;
    WantedNodeExpand: TTreeNode;
    function BuildTitle(): TCDatabase;
    procedure CheckActivePageChange(const ActivePageIndex: Integer);
    procedure ClearTSFields();
    procedure FormClientEvent(const Event: TCClient.TEvent);
    function GetFilename(): Boolean;
    procedure InitTSFields();
    procedure InitTSJob();
    function InitTSSelect(): Boolean;
    function ObjectsFromFSelect(): Boolean;
    procedure OnError(const Sender: TObject; const Error: TTools.TError; const Item: TTools.TItem; const ShowRetry: Boolean; var Success: TDataAction);
    procedure OnExecuted(const ASuccess: Boolean);
    procedure OnUpdate(const AProgressInfos: TTools.TProgressInfos);
    procedure CMChangePreferences(var Message: TMessage); message CM_CHANGEPREFERENCES;
    procedure CMExecutionDone(var Message: TMessage); message CM_EXECUTIONDONE;
    procedure CMPostAfterExecuteSQL(var Message: TMessage); message CM_POST_AFTEREXECUTESQL;
    procedure CMSysFontChanged(var Message: TMessage); message CM_SYSFONTCHANGED;
    procedure CMUpdateProgressInfo(var Message: TMessage); message CM_UPDATEPROGRESSINFO;
  public
    Client: TCClient;
    CodePage: Cardinal;
    DBGrid: TDBGrid;
    DialogType: (edtNormal, edtCreateJob, edtEditJob, edtExecuteJob);
    ExportType: TPExportType;
    Job: TAJobExport;
    Window: TForm;
    function Execute(): Boolean;
    property Objects: TList read FObjects;
  end;

function DExport(): TDExport;

implementation {***************************************************************}

{$R *.dfm}

uses
  Registry, Math, StrUtils, RichEdit, DBCommon,
  MySQLDB, SQLUtils,
  fDLogin;

var
  FExport: TDExport;

function DExport(): TDExport;
begin
  if (not Assigned(FExport)) then
  begin
    Application.CreateForm(TDExport, FExport);
    FExport.Perform(CM_CHANGEPREFERENCES, 0, 0);
  end;

  Result := FExport;
end;

function DBObjectsSortItem(Item1, Item2: Pointer): Integer;
var
  Client: TCClient;
  Index1: Integer;
  Index2: Integer;
begin
  Client := TCDBObject(Item1).Database.Client;

  if (Client.LowerCaseTableNames = 0) then
    Result := lstrcmp(PChar(TCDBObject(Item1).Database.Name), PChar(TCDBObject(Item2).Database.Name))
  else
    Result := lstrcmpi(PChar(TCDBObject(Item1).Database.Name), PChar(TCDBObject(Item2).Database.Name));

  if (Result = 0) then
  begin
    if ((TCDBObject(Item1) is TCBaseTable) and Assigned(TCBaseTable(Item1).Engine) and not TCBaseTable(Item1).Engine.IsMerge) then
      Index1 := 0
    else if (TCDBObject(Item1) is TCBaseTable) then
      Index1 := 1
    else if (TCDBObject(Item1) is TCFunction) then
      Index1 := 2
    else if (TCDBObject(Item1) is TCView) then
      Index1 := 3
    else if (TCDBObject(Item1) is TCProcedure) then
      Index1 := 4
    else if (TCDBObject(Item1) is TCTrigger) then
      Index1 := 5
    else if (TCDBObject(Item1) is TCEvent) then
      Index1 := 6
    else
      Index1 := 7;
    if ((TCDBObject(Item2) is TCBaseTable) and Assigned(TCBaseTable(Item2).Engine) and not TCBaseTable(Item2).Engine.IsMerge) then
      Index2 := 0
    else if (TCDBObject(Item2) is TCBaseTable) then
      Index2 := 1
    else if (TCDBObject(Item2) is TCFunction) then
      Index2 := 2
    else if (TCDBObject(Item2) is TCView) then
      Index2 := 3
    else if (TCDBObject(Item2) is TCProcedure) then
      Index2 := 4
    else if (TCDBObject(Item2) is TCTrigger) then
      Index2 := 5
    else if (TCDBObject(Item2) is TCEvent) then
      Index2 := 6
    else
      Index2 := 7;
    Result := Sign(Index1 - Index2);
  end;

  if (Result = 0) then
    if ((TCDBObject(Item1) is TCTable) and (Client.LowerCaseTableNames = 0)) then
      Result := lstrcmp(PChar(TCDBObject(Item1).Name), PChar(TCDBObject(Item2).Name))
    else
      Result := lstrcmpi(PChar(TCDBObject(Item1).Name), PChar(TCDBObject(Item2).Name));
end;

{ TDExport ********************************************************************}

function TDExport.BuildTitle(): TCDatabase;
var
  I: Integer;
begin
  Result := nil;
  for I := 0 to Objects.Count - 1 do
    if (TObject(Objects[I]) is TCDatabase) then
      if (not Assigned(Result) or (Objects[I] = Result)) then
        Result := TCDatabase(Objects[I])
      else
      begin
        Result := nil;
        break;
      end
    else if (TObject(Objects[I]) is TCDBObject) then
      if (not Assigned(Result) or (TCDBObject(Objects[I]).Database = Result)) then
        Result := TCDBObject(Objects[I]).Database
      else
      begin
        Result := nil;
        break;
      end;

  if (Assigned(DBGrid)) then
    Title := Preferences.LoadStr(362)
  else if (Objects.Count = 1) then
    Title := TCObject(Objects[0]).Name
  else if (Assigned(Result)) then
    Title := Result.Name
  else
    Title := Client.Caption;
end;

procedure TDExport.CheckActivePageChange(const ActivePageIndex: Integer);
var
  I: Integer;
  NextActivePageIndex: Integer;
begin
  FBBack.Enabled := False;
  for I := ActivePageIndex - 1 downto 0 do
    FBBack.Enabled := FBBack.Enabled or PageControl.Pages[I].Enabled;

  NextActivePageIndex := -1;
  for I := PageControl.PageCount - 1 downto ActivePageIndex + 1 do
    if (PageControl.Pages[I].Enabled) then
      NextActivePageIndex := I;

  if (NextActivePageIndex >= 0) then
    for I := NextActivePageIndex + 1 to PageControl.PageCount - 1 do
      PageControl.Pages[I].Enabled := False;

  if (ActivePageIndex = TSTask.PageIndex) then
    FBForward.Caption := Preferences.LoadStr(230)
  else if (NextActivePageIndex = TSExecute.PageIndex) then
    FBForward.Caption := Preferences.LoadStr(174)
  else
    FBForward.Caption := Preferences.LoadStr(229) + ' >';

  FBForward.Enabled := FBForward.Visible and (NextActivePageIndex >= 0) and ((NextActivePageIndex < TSExecute.PageIndex) or not SQLWait) or (ActivePageIndex = TSTask.PageIndex);
  FBForward.Default := True;

  if (not FBForward.Enabled and SQLWait) then
    FBForward.Cursor := crSQLWait
  else
    FBForward.Cursor := crDefault;

  FBCancel.Caption := Preferences.LoadStr(30);
  FBCancel.Default := False;

  if ((ActiveControl = FBCancel) and FBForward.Enabled) then
    ActiveControl := FBForward;
end;

procedure TDExport.ClearTSFields();
var
  I: Integer;
begin
  for I := 0 to Length(FFields) - 1 do FFields[I].Free();
  for I := 0 to Length(FLReferrers) - 1 do FLReferrers[I].Free();
  for I := 0 to Length(FDestFields) - 1 do FDestFields[I].Free();
  SetLength(FFields, 0);
  SetLength(FLReferrers, 0);
  SetLength(FDestFields, 0);
end;

procedure TDExport.CMChangePreferences(var Message: TMessage);
begin
  GSelect.Caption := Preferences.LoadStr(721);

  GBasics.Caption := Preferences.LoadStr(85);
  FLName.Caption := Preferences.LoadStr(35) + ':';
  FLExportType.Caption := Preferences.LoadStr(200) + ':';
  FSQLFile.Caption := Preferences.LoadStr(409);
  FTextFile.Caption := Preferences.LoadStr(410);
  FExcelFile.Caption := Preferences.LoadStr(801);
  FAccessFile.Caption := Preferences.LoadStr(695);
  FSQLiteFile.Caption := Preferences.LoadStr(870);
  FODBC.Caption := Preferences.LoadStr(607);
  FHTMLFile.Caption := Preferences.LoadStr(453);
  FXMLFile.Caption := Preferences.LoadStr(454);
  FPDFFile.Caption := Preferences.LoadStr(890);
  FLFilename.Caption := Preferences.LoadStr(348) + ':';

  GODBCSelect.Caption := Preferences.LoadStr(265) + ':';

  PSQLWait.Caption := Preferences.LoadStr(882);

  GSQLWhat.Caption := Preferences.LoadStr(227);
  FLSQLWhat.Caption := Preferences.LoadStr(218) + ':';
  FSQLStructure.Caption := Preferences.LoadStr(215);
  FSQLData.Caption := Preferences.LoadStr(216);

  GSQLOptions.Caption := Preferences.LoadStr(238);
  FLGeneral.Caption := Preferences.LoadStr(108) + ':';
  FLDrop.Caption := Preferences.LoadStr(242) + ':';
  FDrop.Caption := Preferences.LoadStr(243);
  FReplaceData.Caption := LowerCase(ReplaceStr(Preferences.LoadStr(416), '&', ''));
  FLDatabaseHandling.Caption := ReplaceStr(Preferences.LoadStr(38), '&', '') + ':';
  FCreateDatabase.Caption := Preferences.LoadStr(245);
  FUseDatabase.Caption := Preferences.LoadStr(246);
  FDisableKeys.Caption := Preferences.LoadStr(621);

  GCSVOptions.Caption := Preferences.LoadStr(238);
  FLCSVHeadline.Caption := Preferences.LoadStr(393) + ':';
  FCSVHeadline.Caption := Preferences.LoadStr(408);
  FLSeparator.Caption := Preferences.LoadStr(352) + ':';
  FSeparatorTab.Caption := Preferences.LoadStr(354);
  FSeparatorChar.Caption := Preferences.LoadStr(355) + ':';
  FLQuoteValues.Caption := Preferences.LoadStr(353) + ':';
  FNoQuote.Caption := Preferences.LoadStr(359);
  FStringQuote.Caption := Preferences.LoadStr(360);
  FAllQuote.Caption := Preferences.LoadStr(361);
  FLQuoteChar.Caption := Preferences.LoadStr(356) + ':';

  GProgress.Caption := Preferences.LoadStr(224);
  FLEntiered.Caption := Preferences.LoadStr(211) + ':';
  FLDone.Caption := Preferences.LoadStr(232) + ':';
  FLProgressTables.Caption := Preferences.LoadStr(234) + ':';
  FLProgressRecords.Caption := Preferences.LoadStr(235) + ':';
  FLProgressTime.Caption := ReplaceStr(Preferences.LoadStr(661), '&', '') + ':';
  FLErrors.Caption := Preferences.LoadStr(391) + ':';

  GXMLHow.Caption := Preferences.LoadStr(238);
  FLRootTag.Caption := 'Root:';
  FLDatabaseTag.Caption := ReplaceStr(Preferences.LoadStr(265), '&', '') + ':';
  FDatabaseTagDisabled.Caption := Preferences.LoadStr(554);
  FLTableTag.Caption := ReplaceStr(Preferences.LoadStr(234), '&', '') + ':';
  FTableTagDisabled.Caption := Preferences.LoadStr(554);
  FLRecordTag.Caption := Preferences.LoadStr(124) + ':';
  FLFieldTag.Caption := ReplaceStr(Preferences.LoadStr(253), '&', '') + ':';

  GHTMLWhat.Caption := Preferences.LoadStr(227);
  FLHTMLWhat.Caption := Preferences.LoadStr(218) + ':';
  FHTMLData.Caption := Preferences.LoadStr(216);

  GHTMLOptions.Caption := Preferences.LoadStr(238);
  FLHTMLNullValues.Caption := Preferences.LoadStr(498) + ':';
  FHTMLNullText.Caption := Preferences.LoadStr(499);
  FLHTMLViewDatas.Caption := ReplaceStr(Preferences.LoadStr(574), '&', '') + ':';
  FHTMLShowMemoContent.Caption := Preferences.LoadStr(575);
  FLHTMLBGColorEnabled.Caption := Preferences.LoadStr(740) + ':';
  FHTMLRowBGColorEnabled.Caption := Preferences.LoadStr(600);

  GFields.Caption := Preferences.LoadStr(253);
  FLFields.Caption := Preferences.LoadStr(401) + ':';
  FLDestFields.Caption := Preferences.LoadStr(400) + ':';

  GErrors.Caption := Preferences.LoadStr(392);

  GTask.Caption := Preferences.LoadStr(661);
  FLStart.Caption := Preferences.LoadStr(817) + ':';
  FLExecution.Caption := Preferences.LoadStr(174) + ':';
  FSingle.Caption := Preferences.LoadStr(902);
  FDaily.Caption := Preferences.LoadStr(903);
  FWeekly.Caption := Preferences.LoadStr(904);
  FMonthly.Caption := Preferences.LoadStr(905);
  FLEnabled.Caption := Preferences.LoadStr(812) + ':';
  FEnabled.Caption := Preferences.LoadStr(529);

  FBHelp.Caption := Preferences.LoadStr(167);
  FBBack.Caption := '< ' + Preferences.LoadStr(228);
end;

procedure TDExport.CMExecutionDone(var Message: TMessage);
var
  Success: Boolean;
begin
  Success := Boolean(Message.WParam);

  if (Assigned(Export)) then
    FreeAndNil(Export);

  FBBack.Enabled := ExportType <> etSQLiteFile;
  FBCancel.Caption := Preferences.LoadStr(231);

  if (Success) then
    FBCancel.ModalResult := mrOk
  else
    FBCancel.ModalResult := mrCancel;

  ActiveControl := FBCancel;
end;

procedure TDExport.CMPostAfterExecuteSQL(var Message: TMessage);
var
  Database: TCDatabase;
  I: Integer;
  J: Integer;
begin
  if ((DialogType in [edtEditJob, edtExecuteJob]) and InitTSSelect() and (DialogType in [edtExecuteJob]) and ObjectsFromFSelect()
    or (DialogType = edtNormal)) then
  begin
    I := 0;
    while (I < Objects.Count) do
      if (TObject(Objects[I]) is TCDatabase) then
      begin
        Database := TCDatabase(Objects[I]);
        if (not Database.Valid) then
          Inc(I)
        else
        begin
          for J := 0 to Database.Tables.Count - 1 do
            if (Objects.IndexOf(Database.Tables[J]) < 0) then
              Objects.Add(Database.Tables[J]);
          if (Assigned(Database.Routines)) then
            for J := 0 to Database.Routines.Count - 1 do
              if (Objects.IndexOf(Database.Routines[J]) < 0) then
                Objects.Add(Database.Routines[J]);
          if (Assigned(Database.Triggers)) then
            for J := 0 to Database.Triggers.Count - 1 do
              if (Objects.IndexOf(Database.Triggers[J]) < 0) then
                Objects.Add(Database.Triggers[J]);
          Objects.Delete(I);
        end;
      end
      else
        Inc(I);
  end;

  BuildTitle();


  Message.Result := LRESULT(Client.Update(Objects));
  if (Boolean(Message.Result)) then
    if (Assigned(WantedNodeExpand)) then
      WantedNodeExpand.Expand(False)
    else
    begin
      if (not PageControl.Visible) then
      begin
        PageControl.Visible := True;
        PSQLWait.Visible := not PageControl.Visible;
      end;

      if (TSFields.Enabled) then
        InitTSFields();
      if (TSJob.Enabled) then
        InitTSJob();
      CheckActivePageChange(PageControl.ActivePageIndex);
    end;
end;

procedure TDExport.CMSysFontChanged(var Message: TMessage);
begin
  inherited;

  FBFilename.Height := FFilename.Height;

  FDatabaseTag.Left := FL1DatabaseTagFree.Left + FL1DatabaseTagFree.Width;
  FDatabaseAttribute.Left := FDatabaseTag.Left + FDatabaseTag.Width + PDatabaseTag.Canvas.TextWidth('  ') + 2;
  FL2DatabaseTagFree.Left := FDatabaseAttribute.Left + FDatabaseAttribute.Width + 1;

  FTableTag.Left := FL1TableTagFree.Left + FL1TableTagFree.Width;
  FTableAttribute.Left := FTableTag.Left + FTableTag.Width + PTableTag.Canvas.TextWidth('  ') + 2;
  FL2TableTagFree.Left := FTableAttribute.Left + FTableAttribute.Width + 1;

  FFieldTag.Left := FL1FieldTagFree.Left + FL1FieldTagFree.Width;
  FFieldAttribute.Left := FFieldTag.Left + FFieldTag.Width + PFieldTag.Canvas.TextWidth('  ') + 2;
  FL2FieldTagFree.Left := FFieldAttribute.Left + FFieldAttribute.Width + 1;
end;

procedure TDExport.CMUpdateProgressInfo(var Message: TMessage);
var
  Infos: TTools.PProgressInfos;
begin
  Infos := TTools.PProgressInfos(Message.LParam);

  if (Infos^.TablesSum < 0) then
    FEntieredTables.Caption := '???'
  else
    FEntieredTables.Caption := FormatFloat('#,##0', Infos^.TablesSum, LocaleFormatSettings);
  if (Infos^.TablesDone < 0) then
    FDoneTables.Caption := '???'
  else
    FDoneTables.Caption := FormatFloat('#,##0', Infos^.TablesDone, LocaleFormatSettings);

  if (Infos^.RecordsSum < 0) then
    FEntieredRecords.Caption := '???'
  else
    FEntieredRecords.Caption := FormatFloat('#,##0', Infos^.RecordsSum, LocaleFormatSettings);
  if (Infos^.RecordsDone < 0) then
    FDoneRecords.Caption := '???'
  else
    FDoneRecords.Caption := FormatFloat('#,##0', Infos^.RecordsDone, LocaleFormatSettings);

  FEntieredTime.Caption := TimeToStr(Infos^.TimeSum, DurationFormatSettings);
  FDoneTime.Caption := TimeToStr(Infos^.TimeDone, DurationFormatSettings);

  FProgressBar.Position := Infos^.Progress;

  if (Assigned(Export) and Export.Suspended) then
    Application.ProcessMessages();
end;

function TDExport.Execute(): Boolean;
begin
  PageControl.ActivePageIndex := -1;
  ModalResult := mrNone;

  Filename := '';
  Title := '';

  if ((Assigned(DBGrid) or (Objects.Count >= 1)) and (DialogType = edtNormal) and not (ExportType in [etODBC, etPrinter])) then
    if (not GetFilename()) then
      ModalResult := mrCancel;

  Result := (ModalResult = mrNone) and (ShowModal() = mrOk);
end;

procedure TDExport.FBBackClick(Sender: TObject);
var
  ActivePageIndex: Integer;
begin
  for ActivePageIndex := PageControl.ActivePageIndex - 1 downto 0 do
    if (PageControl.Pages[ActivePageIndex].Enabled) then
    begin
      PageControl.ActivePageIndex := ActivePageIndex;
      Exit;
    end;
end;

procedure TDExport.FBCancelClick(Sender: TObject);
begin
  if (Assigned(Export)) then
  begin
    Export.UserAbort.SetEvent();
    if (not Export.Suspended) then
      Export.WaitFor();
  end;
end;

procedure TDExport.FBFilenameClick(Sender: TObject);
begin
  Filename := FFilename.Text;
  if (GetFilename()) then
    FFilename.Text := Filename;
end;

procedure TDExport.FBForwardClick(Sender: TObject);
var
  ActivePageIndex: Integer;
begin
  if (PageControl.ActivePageIndex = TSTask.PageIndex) then
    ModalResult := mrOk
  else
    for ActivePageIndex := PageControl.ActivePageIndex + 1 to PageControl.PageCount - 1 do
      if (PageControl.Pages[ActivePageIndex].Enabled) then
      begin
        PageControl.ActivePageIndex := ActivePageIndex;
        Exit;
      end;
end;

procedure TDExport.FBHelpClick(Sender: TObject);
begin
  Application.HelpContext(HelpContext);
end;

procedure TDExport.FDatabaseDblClick(Sender: TObject);
begin
  FBForward.Click();
end;

procedure TDExport.FDatabaseTagClick(Sender: TObject);
begin
  FDatabaseTag.Enabled := FDatabaseTagFree.Checked;

  TSXMLOptionChange(Sender);
end;

procedure TDExport.FDatabaseTagKeyPress(Sender: TObject;
  var Key: Char);
begin
  FDatabaseTagClick(Sender);
end;

procedure TDExport.FDestField1Change(Sender: TObject);
var
  I: Integer;
  J: Integer;
  TabSheet: TTabSheet;
begin
  if (DialogType in [edtCreateJob, edtEditJob]) then
    TabSheet := TSTask
  else
    TabSheet := TSExecute;
  TabSheet.Enabled := False;
  for I := 0 to Length(FFields) - 1 do
    if ((FFields[I].ItemIndex > 0) and (FDestFields[I].Text <> '')) then
      TabSheet.Enabled := True;

  for I := 0 to Length(FFields) - 1 do
    for J := 0 to I - 1 do
      if ((I <> J) and FDestFields[I].Enabled and FDestFields[J].Enabled and (lstrcmpi(PChar(FDestFields[J].Text), PChar(FDestFields[I].Text)) = 0)) then
        TabSheet.Enabled := False;

  CheckActivePageChange(TSFields.PageIndex);
end;

procedure TDExport.FExportTypeChange(Sender: TObject);
begin
  FFilename.Text := '';

  FJobOptionChange(Sender);
end;

procedure TDExport.FField1Change(Sender: TObject);
var
  I: Integer;
begin
  for I := 0 to Length(FDestFields) - 1 do
    if (Sender = FFields[I]) then
    begin
      FDestFields[I].Enabled := (FFields[I].ItemIndex > 0) and (ExportType in [etTextFile, etExcelFile, etHTMLFile, etPDFFile, etXMLFile, etPrinter]);
      FDestFields[I].Text := FFields[I].Text;
    end;

  if (Length(FDestFields) > 0) then
    FDestField1Change(Sender);
end;

procedure TDExport.FField1Exit(Sender: TObject);
var
  I: Integer;
begin
  if (Sender is TComboBox_Ext) then
    for I := 0 to Length(FFields) - 1 do
      if ((FFields[I] <> Sender) and (FFields[I].ItemIndex = TComboBox_Ext(Sender).ItemIndex)) then
      begin
        FFields[I].ItemIndex := 0;
        FField1Change(FFields[I]);
      end;
end;

procedure TDExport.FFieldTagClick(Sender: TObject);
begin
  FFieldTag.Enabled := FFieldTagFree.Checked;

  TSXMLOptionChange(Sender);
end;

procedure TDExport.FFieldTagKeyPress(Sender: TObject;
  var Key: Char);
begin
  FFieldTagClick(Sender);
end;

procedure TDExport.FHTMLDataClick(Sender: TObject);
var
  TabSheet: TTabSheet;
begin
  if (DialogType in [edtCreateJob, edtEditJob]) then
    TabSheet := TSTask
  else
    TabSheet := TSExecute;

  FLHTMLNullValues.Enabled := FHTMLData.Checked;
  FHTMLNullText.Enabled := FHTMLData.Checked;
  FLHTMLViewDatas.Enabled := FHTMLData.Checked;
  FHTMLShowMemoContent.Enabled := FHTMLData.Checked;
  FLHTMLBGColorEnabled.Enabled := FHTMLData.Checked;
  FHTMLRowBGColorEnabled.Enabled := FHTMLData.Checked;

  TabSheet.Enabled := FHTMLStructure.Checked or FHTMLData.Checked;
  CheckActivePageChange(TSHTMLOptions.PageIndex);
end;

procedure TDExport.FHTMLDataKeyPress(Sender: TObject; var Key: Char);
begin
  FHTMLDataClick(Sender);
end;

procedure TDExport.FHTMLStructureClick(Sender: TObject);
var
  TabSheet: TTabSheet;
begin
  if (DialogType in [edtCreateJob, edtEditJob]) then
    TabSheet := TSTask
  else
    TabSheet := TSExecute;

  TabSheet.Enabled := FHTMLStructure.Checked or FHTMLData.Checked;

  CheckActivePageChange(TSHTMLOptions.PageIndex);
end;

procedure TDExport.FHTMLStructureKeyPress(Sender: TObject; var Key: Char);
begin
  FHTMLStructureClick(Sender);
end;

procedure TDExport.FJobOptionChange(Sender: TObject);
begin
  FFilename.Visible := True;
  if (FSQLFile.Checked) then
    ExportType := etSQLFile
  else if (FTextFile.Checked) then
    ExportType := etTextFile
  else if (FExcelFile.Checked) then
    ExportType := etExcelFile
  else if (FAccessFile.Checked) then
    ExportType := etAccessFile
  else if (FSQLiteFile.Checked) then
    ExportType := etSQLiteFile
  else if (FHTMLFile.Checked) then
    ExportType := etHTMLFile
  else if (FXMLFile.Checked) then
    ExportType := etXMLFile
  else if (FPDFFile.Checked) then
    ExportType := etPDFFile
  else
    FFilename.Visible := False;
  FLFilename.Visible := FFilename.Visible;

  TSODBCSelect.Enabled := (ExportType in [etODBC]);
  TSSQLOptions.Enabled := (ExportType in [etSQLFile]) and (Filename <> '');
  TSCSVOptions.Enabled := (ExportType in [etTextFile]) and (Filename <> '');
  TSXMLOptions.Enabled := (ExportType in [etXMLFile]) and (Filename <> '');
  TSHTMLOptions.Enabled := (ExportType in [etHTMLFile, etPDFFile]) and (Filename <> '');
  TSFields.Enabled := (ExportType in [etExcelFile]) and (Objects.Count = 1);
  TSTask.Enabled := True;

  CheckActivePageChange(TSJob.PageIndex);

  if (not ValidJobName(Trim(FName.Text)) or (DialogType in [edtCreateJob]) and Assigned(Client.Account.JobByName(Trim(FName.Text))) or (DialogType in [edtEditJob]) and (Client.Account.JobByName(Trim(FName.Text)) <> Job)) then
    FBForward.Enabled := False;
  if (FFilename.Visible and not DirectoryExists(ExtractFilePath(FFilename.Text))) then
    FBForward.Enabled := False;
end;

procedure TDExport.FODBCSelectChange(Sender: TObject; Item: TListItem;
  Change: TItemChange);
var
  Cancel: Boolean;
  cbMessageText: SQLSMALLINT;
  MessageText: PSQLTCHAR;
  SQLState: array [0 .. SQL_SQLSTATE_SIZE] of SQLTCHAR;
  Success: Boolean;
  TabSheet: TTabSheet;
begin
  if (DialogType in [edtCreateJob, edtEditJob]) then
    TabSheet := TSTask
  else
    TabSheet := TSExecute;

  if (ODBC <> SQL_NULL_HANDLE) then
  begin
    SQLDisconnect(ODBC);
    SQLFreeHandle(SQL_HANDLE_DBC, ODBC); ODBC := SQL_NULL_HANDLE;
  end;

  if ((Change = ctState) and Assigned(Item) and Item.Selected) then
    repeat
      DLogin.Account := nil;
      DLogin.Filename := Item.Caption;
      DLogin.Window := Window;
      Success := DLogin.Execute();
      Cancel := not Success;
      if (Success) then
      begin
        Success := SQL_SUCCEEDED(SQLAllocHandle(SQL_HANDLE_DBC, ODBCEnv, @ODBC));
        Success := Success and SQL_SUCCEEDED(SQLConnect(ODBC, PSQLTCHAR(PChar(Item.Caption)), SQL_NTS, PSQLTCHAR(DLogin.Username), SQL_NTS, PSQLTCHAR(DLogin.Password), SQL_NTS));
        if (not Success and (ODBC <> SQL_NULL_HANDLE)) then
        begin
          if (SQL_SUCCEEDED(SQLGetDiagRec(SQL_HANDLE_DBC, ODBC, 1, nil, nil, nil, 0, @cbMessageText))) then
          begin
            GetMem(MessageText, (cbMessageText + 1) * SizeOf(SQLTCHAR));
            if (SQL_SUCCEEDED(SQLGetDiagRec(SQL_HANDLE_DBC, ODBC, 1, @SQLState, nil, @MessageText, cbMessageText + 1, nil))) then
              MsgBox(PChar(MessageText) + ' (' + SQLState + ')', Preferences.LoadStr(45), MB_OK + MB_ICONERROR);
            FreeMem(MessageText);
          end;
          SQLFreeHandle(SQL_HANDLE_DBC, ODBC); ODBC := SQL_NULL_HANDLE;
        end;
      end;
    until (Success or Cancel);

  TabSheet.Enabled := ODBC <> SQL_NULL_HANDLE;
  FBForward.Enabled := TabSheet.Enabled;
end;

procedure TDExport.FODBCSelectDblClick(Sender: TObject);
begin
  FBForward.Click();
end;

procedure TDExport.FormClientEvent(const Event: TCClient.TEvent);
begin
  if (Event.EventType = ceAfterExecuteSQL) then
    PostMessage(Handle, CM_POST_AFTEREXECUTESQL, 0, 0);
end;

procedure TDExport.FormCreate(Sender: TObject);
begin
  Export := nil;

  FSelect.Images := Preferences.SmallImages;

  FObjects := TList.Create();
  FODBCSelect.SmallImages := Preferences.SmallImages;

  FCSVHeadline.Checked := Preferences.Export.CSVHeadline;
  FSeparatorTab.Checked := Preferences.Export.CSVSeparatorType = stTab;
  FSeparatorChar.Checked := Preferences.Export.CSVSeparatorType = stChar;
  FSeparator.Text := Preferences.Export.CSVSeparator;
  FNoQuote.Checked := Preferences.Export.CSVQuote = 0;
  FStringQuote.Checked := Preferences.Export.CSVQuote = 1;
  FAllQuote.Checked := Preferences.Export.CSVQuote = 2;
  FQuoteChar.Text := Preferences.Export.CSVQuoteChar;

  FSQLStructure.Checked := Preferences.Export.SQLStructure;
  FSQLData.Checked := Preferences.Export.SQLData;
  FUseDatabase.Checked := Preferences.Export.SQLUseDatabase;
  FDrop.Checked := Preferences.Export.SQLDropBeforeCreate;
  FReplaceData.Checked := Preferences.Export.SQLReplaceData;
  FDisableKeys.Checked := Preferences.Export.SQLDisableKeys;

  FSQLOptionClick(Sender);

  FHTMLStructure.Checked := Preferences.Export.HTMLStructure;
  FHTMLData.Checked := Preferences.Export.HTMLData;
  FHTMLNullText.Checked := Preferences.GridNullText;
  FHTMLShowMemoContent.Checked := Preferences.GridShowMemoContent;
  FHTMLRowBGColorEnabled.Checked := Preferences.GridRowBGColorEnabled;

  FMonthly.Visible := CheckWin32Version(6, 1);

  SendMessage(FErrorMessages.Handle, EM_SETTEXTMODE, TM_PLAINTEXT, 0);
  SendMessage(FErrorMessages.Handle, EM_SETWORDBREAKPROC, 0, LPARAM(@EditWordBreakProc));

  PageControl.ActivePage := nil; // Make sure, not ___OnShowPage will be executed

  ExportType := etSQLFile;
end;

procedure TDExport.FormDestroy(Sender: TObject);
begin
  FObjects.Free();
end;

procedure TDExport.FormHide(Sender: TObject);
var
  Export: TPExport;
  I: Integer;
  Hour, Min, Sec, MSec: Word;
  Year, Month, Day: Word;
begin
  Client.UnRegisterEventProc(FormClientEvent);

  if (Assigned(DBGrid)) then
    DBGrid.DataSource.DataSet.EnableControls();

  if (ModalResult = mrOk) then
  begin
    if (DialogType in [edtCreateJob, edtEditJob]) then
      Export := TAJobExport.Create(Client.Account.Jobs, Trim(FName.Text))
    else
      Export := Preferences.Export;

    Export.CSVHeadline := FCSVHeadline.Checked;
    if (ExportType = etTextFile) then
    begin
      if (FSeparatorTab.Checked) then
        Export.CSVSeparatorType := stTab
      else if (FSeparatorChar.Checked) then
        Export.CSVSeparatorType := stChar;
      Export.CSVSeparator := FSeparator.Text;
      if (FNoQuote.Checked) then
        Export.CSVQuote := 0
      else if (FAllQuote.Checked) then
        Export.CSVQuote := 0
      else
        Export.CSVQuote := 1;
      Export.CSVQuoteChar := FQuoteChar.Text;
    end;

    Export.SQLStructure := FSQLStructure.Checked;
    Export.SQLData := FSQLData.Checked;
    Export.SQLDropBeforeCreate := FDrop.Checked;
    Export.SQLReplaceData := FReplaceData.Checked;
    Export.SQLDisableKeys := FDisableKeys.Checked;
    if (Objects.Count > 1) then
      Export.SQLCreateDatabase := FCreateDatabase.Checked;
    Export.SQLUseDatabase := FUseDatabase.Checked;

    Export.HTMLStructure := FHTMLStructure.Checked;
    Export.HTMLData := FHTMLData.Checked;

    if (DialogType in [edtCreateJob, edtEditJob]) then
    begin
      TAJobExport(Export).ClearObjects();
      for I := 0 to FSelect.Items.Count - 1 do
        if (FSelect.Items[I].Selected) then
        begin
          SetLength(TAJobExport(Export).Objects, Length(TAJobExport(Export).Objects) + 1);
          if (not Assigned(FSelect.Items[I].Parent)) then
            TAJobExport(Export).Objects[Length(TAJobExport(Export).Objects) - 1].ObjectType := jotServer
          else if (TObject(FSelect.Items[I].Data) is TCDatabase) then
          begin
            TAJobExport(Export).Objects[Length(TAJobExport(Export).Objects) - 1].ObjectType := jotDatabase;
            TAJobExport(Export).Objects[Length(TAJobExport(Export).Objects) - 1].Name := TCDatabase(FSelect.Items[I].Data).Name;
          end
          else if (TObject(FSelect.Items[I].Data) is TCDBObject) then
          begin
            if (TObject(FSelect.Items[I].Data) is TCTable) then
              TAJobExport(Export).Objects[Length(TAJobExport(Export).Objects) - 1].ObjectType := jotTable
            else if (TObject(FSelect.Items[I].Data) is TCProcedure) then
              TAJobExport(Export).Objects[Length(TAJobExport(Export).Objects) - 1].ObjectType := jotProcedure
            else if (TObject(FSelect.Items[I].Data) is TCFunction) then
              TAJobExport(Export).Objects[Length(TAJobExport(Export).Objects) - 1].ObjectType := jotFunction
            else if (TObject(FSelect.Items[I].Data) is TCTrigger) then
              TAJobExport(Export).Objects[Length(TAJobExport(Export).Objects) - 1].ObjectType := jotTrigger
            else if (TObject(FSelect.Items[I].Data) is TCEvent) then
              TAJobExport(Export).Objects[Length(TAJobExport(Export).Objects) - 1].ObjectType := jotEvent;
            TAJobExport(Export).Objects[Length(TAJobExport(Export).Objects) - 1].Name := TCDBObject(FSelect.Items[I].Data).Name;
            TAJobExport(Export).Objects[Length(TAJobExport(Export).Objects) - 1].DatabaseName := TCDBObject(FSelect.Items[I].Data).Database.Name;
          end;
        end;
      TAJobExport(Export).CodePage := CodePage;
      TAJobExport(Export).ExportType := ExportType;
      TAJobExport(Export).Filename := FFilename.Text;
      DecodeDate(FStartDate.Date, Year, Month, Day);
      DecodeTime(FStartTime.Time, Hour, Min, Sec, MSec);
      TAJobExport(Export).Start := EncodeDate(Year, Month, Day) + EncodeTime(Hour, Min, Sec, MSec);
      if (FDaily.Checked) then
        TAJobExport(Export).TriggerType := ttDaily
      else if (FWeekly.Checked) then
        TAJobExport(Export).TriggerType := ttWeekly
      else if (FMonthly.Checked) then
        TAJobExport(Export).TriggerType := ttMonthly
      else
        TAJobExport(Export).TriggerType := ttSingle;
      TAJobExport(Export).Enabled := FEnabled.Checked;

      if (DialogType = edtCreateJob) then
        Client.Account.Jobs.AddJob(Export)
      else if (DialogType = edtEditJob) then
        Client.Account.Jobs.UpdateJob(Job, Export);
      Export.Free();
    end;
  end;

  FSelect.Selected := nil; // Make sure, not to call FSelectedChange with a selcted node
  FSelect.Items.BeginUpdate();
  FSelect.Items.Clear();
  FSelect.Items.EndUpdate();

  FODBCSelect.Items.BeginUpdate();
  FODBCSelect.Items.Clear();
  FODBCSelect.Items.EndUpdate();
  ClearTSFields();
  PageControl.ActivePage := nil;

  if (ODBC <> SQL_NULL_HANDLE) then
  begin
    SQLDisconnect(ODBC);
    SQLFreeHandle(SQL_HANDLE_DBC, ODBC); ODBC := SQL_NULL_HANDLE;
  end;
  if (ODBCEnv <> SQL_NULL_HANDLE) then
    begin SQLFreeHandle(SQL_HANDLE_ENV, ODBCEnv); ODBCEnv := SQL_NULL_HANDLE; end;

  PageControl.ActivePage := nil;
end;

procedure TDExport.FormShow(Sender: TObject);
var
  I: Integer;
  Node: TTreeNode;
begin
  Client.RegisterEventProc(FormClientEvent);

  ModalResult := mrNone;
  if (DialogType = edtCreateJob) then
    Caption := Preferences.LoadStr(897)
  else if (DialogType = edtEditJob) then
    Caption := Preferences.LoadStr(842, Job.Name)
  else if (DialogType = edtExecuteJob) then
    Caption := Preferences.LoadStr(210) + ' ' + ExtractFileName(Job.Filename)
  else if (ExportType = etPrinter) then
    Caption := Preferences.LoadStr(577)
  else if (ExtractFileName(Filename) = '') then
    Caption := Preferences.LoadStr(210)
  else
    Caption := Preferences.LoadStr(210) + ' ' + ExtractFileName(Filename);

  if (DialogType = edtCreateJob) then
    HelpContext := 1138
  else if (DialogType = edtEditJob) then
    HelpContext := 1140
  else if (DialogType = edtExecuteJob) then
    HelpContext := -1
  else
    case (ExportType) of
      etSQLFile: HelpContext := 1014;
      etTextFile: HelpContext := 1134;
      etExcelFile: HelpContext := 1107;
      etAccessFile: HelpContext := 1129;
      etSQLiteFile: HelpContext := 1128;
      etXMLFile: HelpContext := 1017;
      etHTMLFile: HelpContext := 1016;
      etPDFFile: HelpContext := 1137;
      etPrinter: HelpContext := 1018;
      else HelpContext := -1;
    end;
  FBHelp.Visible := HelpContext >= 0;

  FStartDate.Time := 0;
  if (DialogType = edtCreateJob) then
  begin
    Node := FSelect.Items.Add(nil, Client.Caption);
    Node.ImageIndex := iiServer;
    Node.HasChildren := True;

    FName.Text := Title;
    FFilename.Visible := False; FLFilename.Visible := FFilename.Visible;
    FFilename.Text := '';

    FStartDate.Date := Now() + 1; FStartTime.Time := 0;
    FSingle.Checked := True;
    FEnabled.Checked := True;
  end
  else if (DialogType in [edtEditJob, edtExecuteJob]) then
  begin
    Node := FSelect.Items.Add(nil, Client.Caption);
    Node.ImageIndex := iiServer;
    Node.HasChildren := True;

    FName.Text := Job.Name;
    case (Job.ExportType) of
      etSQLFile: FSQLFile.Checked := True;
      etTextFile: FTextFile.Checked := True;
      etExcelFile: FExcelFile.Checked := True;
      etAccessFile: FAccessFile.Checked := True;
      etSQLiteFile: FSQLiteFile.Checked := True;
      etODBC: FODBC.Checked := True;
      etHTMLFile: FHTMLFile.Checked := True;
      etXMLFile: FXMLFile.Checked := True;
      etPDFFile: FPDFFile.Checked := True;
    end;
    FFilename.Visible := False; FLFilename.Visible := FFilename.Visible;
    CodePage := Job.CodePage;
    Filename := Job.Filename;
    FFilename.Text := Job.Filename;

    FStartDate.Date := Job.Start; FStartTime.Time := Job.Start;
    case (Job.TriggerType) of
      ttDaily: FDaily.Checked := True;
      ttWeekly: FWeekly.Checked := True;
      ttMonthly: FMonthly.Checked := True;
      else FSingle.Checked := True;
    end;
    FEnabled.Checked := Job.Enabled;
  end;

  if (Assigned(DBGrid)) then
    FHTMLStructure.Caption := Preferences.LoadStr(794)
  else
    FHTMLStructure.Caption := Preferences.LoadStr(215);

  FCreateDatabase.Checked := (Objects.Count > 1) and Preferences.Export.SQLCreateDatabase;

  TSSelect.Enabled := DialogType in [edtCreateJob, edtEditJob];
  TSJob.Enabled := False;
  TSODBCSelect.Enabled := (DialogType in [edtNormal]) and (ExportType in [etODBC]);
  TSSQLOptions.Enabled := (DialogType in [edtNormal]) and (ExportType in [etSQLFile]);
  TSCSVOptions.Enabled := (DialogType in [edtNormal]) and (ExportType in [etTextFile]);
  TSXMLOptions.Enabled := (DialogType in [edtNormal]) and (ExportType in [etXMLFile]) and not Assigned(DBGrid);
  TSHTMLOptions.Enabled := (DialogType in [edtNormal]) and (ExportType in [etHTMLFile, etPrinter, etPDFFile]);
  TSFields.Enabled := (DialogType in [edtNormal]) and (ExportType in [etExcelFile]) and ((Objects.Count = 1) or Assigned(DBGrid)) or (ExportType in [etXMLFile]) and Assigned(DBGrid);
  TSTask.Enabled := False;
  TSExecute.Enabled := not TSODBCSelect.Enabled and not TSSQLOptions.Enabled and not TSCSVOptions.Enabled and not TSHTMLOptions.Enabled and not TSFields.Enabled;

  FBBack.Visible := TSSelect.Enabled or TSODBCSelect.Enabled or TSSQLOptions.Enabled or TSCSVOptions.Enabled or TSXMLOptions.Enabled or TSHTMLOptions.Enabled or TSFields.Enabled;
  FBForward.Visible := FBBack.Visible;

  if (Assigned(DBGrid)) then
    DBGrid.DataSource.DataSet.DisableControls();

  if (DialogType in [edtCreateJob, edtEditJob]) then
  begin
    PageControl.Visible := Client.Databases.Update() and Boolean(Perform(CM_POST_AFTEREXECUTESQL, 0, 0));
    if (PageControl.Visible) then
      FSelect.Items.GetFirstNode().Expand(False)
    else
      WantedNodeExpand := FSelect.Items.GetFirstNode();
  end
  else
    PageControl.Visible := Boolean(Perform(CM_POST_AFTEREXECUTESQL, 0, 0));
  PSQLWait.Visible := not PageControl.Visible;

  for I := 0 to PageControl.PageCount - 1 do
    if ((PageControl.ActivePageIndex < 0) and PageControl.Pages[I].Enabled) then
      PageControl.ActivePageIndex := I;

  FBCancel.ModalResult := mrCancel;

  if (PageControl.Visible and TSSelect.Visible) then
    ActiveControl := FSelect
  else if (FBForward.Visible and FBForward.Enabled) then
    ActiveControl := FBForward
  else
    ActiveControl := FBCancel;
end;

procedure TDExport.FQuoteCharExit(Sender: TObject);
begin
  if (FQuoteChar.Text = '') then
    FNoQuote.Checked := True;
end;

procedure TDExport.FQuoteClick(Sender: TObject);
begin
  FQuoteChar.Enabled := not FNoQuote.Checked;
  FLQuoteChar.Enabled := FQuoteChar.Enabled;

  if (not FNoQuote.Checked and (FQuoteChar.Text = '')) then
    FQuoteChar.Text := '"';
end;

procedure TDExport.FQuoteKeyPress(Sender: TObject; var Key: Char);
begin
  FQuoteClick(Sender);
end;

procedure TDExport.FSelectChange(Sender: TObject; Node: TTreeNode);
begin
  TSJob.Enabled := Assigned(FSelect.Selected) and (DialogType in [edtCreateJob, edtEditJob]);
  CheckActivePageChange(TSSelect.PageIndex);
end;

procedure TDExport.FSelectExpanding(Sender: TObject; Node: TTreeNode;
  var AllowExpansion: Boolean);
var
  Database: TCDatabase;
  I: Integer;
  NewNode: TTreeNode;
  Table: TCBaseTable;
  TreeView: TTreeView_Ext;
begin
  TreeView := TTreeView_Ext(Sender);

  if (Assigned(WantedNodeExpand)) then
    WantedNodeExpand := nil;

  if (Assigned(Node)) then
    if (Node.HasChildren and not Assigned(Node.getFirstChild())) then
    begin
      case (Node.ImageIndex) of
        iiServer:
          begin
            if (not Client.Update() and Client.Asynchron) then
              WantedNodeExpand := Node
            else
            begin
              for I := 0 to Client.Databases.Count - 1 do
                if ((Client.Databases.NameCmp(Client.Databases[I].Name, 'mysql') <> 0) and not (Client.Databases[I] is TCSystemDatabase)) then
                begin
                  NewNode := TreeView.Items.AddChild(Node, Client.Databases[I].Name);
                  NewNode.ImageIndex := iiDatabase;
                  NewNode.Data := Client.Databases[I];
                  NewNode.HasChildren := True;
                end;
              Node.HasChildren := Assigned(Node.getFirstChild());
            end;
          end;
        iiDatabase:
          begin
            Database := Client.DatabaseByName(Node.Text);
            if ((not Database.Tables.Update() or not Client.Update(Database.Tables)) and Client.Asynchron) then
              WantedNodeExpand := Node
            else
            begin
              for I := 0 to Database.Tables.Count - 1 do
              begin
                NewNode := TreeView.Items.AddChild(Node, Database.Tables[I].Name);
                if (Database.Tables[I] is TCBaseTable) then
                  NewNode.ImageIndex := iiBaseTable
                else
                  NewNode.ImageIndex := iiView;
                NewNode.Data := Database.Tables[I];
                NewNode.HasChildren := Database.Tables[I] is TCBaseTable;
              end;
              if (Assigned(Database.Routines)) then
                for I := 0 to Database.Routines.Count - 1 do
                begin
                  NewNode := TreeView.Items.AddChild(Node, Database.Routines[I].Name);
                  if (Database.Routines[I] is TCProcedure) then
                    NewNode.ImageIndex := iiProcedure
                  else
                    NewNode.ImageIndex := iiFunction;
                  NewNode.Data := Database.Routines[I];
                end;
              if (Assigned(Database.Events)) then
                for I := 0 to Database.Events.Count - 1 do
                begin
                  NewNode := TreeView.Items.AddChild(Node, Database.Events[I].Name);
                  NewNode.ImageIndex := iiEvent;
                  NewNode.Data := Database.Events[I];
                end;
              Node.HasChildren := Assigned(Node.getFirstChild());
            end;
          end;
        iiBaseTable:
          begin
            Database := Client.DatabaseByName(Node.Parent.Text);
            Table := Database.BaseTableByName(Node.Text);
            if (not Database.Triggers.Update()) then
              WantedNodeExpand := Node
            else
            begin
              for I := 0 to Table.TriggerCount - 1 do
              begin
                NewNode := TreeView.Items.AddChild(Node, Table.Triggers[I].Name);
                NewNode.ImageIndex := iiTrigger;
                NewNode.Data := Table.Triggers[I];
              end;
              Node.HasChildren := Assigned(Node.getFirstChild());
            end;
          end;
      end;
    end;

end;

procedure TDExport.FSelectGetImageIndex(Sender: TObject; Node: TTreeNode);
begin
  Node.SelectedIndex := Node.ImageIndex;
end;

procedure TDExport.FSeparatorClick(Sender: TObject);
begin
  if (not FStringQuote.Enabled and FStringQuote.Checked) then
    FAllQuote.Checked := True;
end;

procedure TDExport.FSeparatorKeyPress(Sender: TObject; var Key: Char);
begin
  FSeparatorClick(Sender);
end;

procedure TDExport.FSQLOptionClick(Sender: TObject);
var
  TabSheet: TTabSheet;
begin
  if (DialogType in [edtCreateJob, edtEditJob]) then
    TabSheet := TSTask
  else
    TabSheet := TSExecute;

  FCreateDatabase.Enabled := FSQLStructure.Checked;
  FCreateDatabase.Checked := FCreateDatabase.Checked and FCreateDatabase.Enabled;
  FUseDatabase.Enabled := not FCreateDatabase.Checked;
  FUseDatabase.Checked := FUseDatabase.Checked or FCreateDatabase.Checked;

  FDrop.Enabled := FSQLStructure.Checked;
  FDrop.Checked := FDrop.Checked and FDrop.Enabled;
  FReplaceData.Enabled := not FCreateDatabase.Checked and FSQLData.Checked and not FDrop.Checked;
  FReplaceData.Checked := FReplaceData.Checked and FReplaceData.Enabled;

  FDisableKeys.Enabled := FSQLData.Checked;
  FDisableKeys.Checked := FDisableKeys.Checked and FDisableKeys.Enabled;

  TabSheet.Enabled := FSQLStructure.Checked or FSQLData.Checked;
  CheckActivePageChange(TSSQLOptions.PageIndex);
end;

procedure TDExport.FSQLOptionKeyPress(Sender: TObject; var Key: Char);
begin
  FSQLOptionClick(Sender);
end;

procedure TDExport.FTableTagClick(Sender: TObject);
begin
  FTableTag.Enabled := FTableTagFree.Checked;

  TSXMLOptionChange(Sender);
end;

procedure TDExport.FTableTagKeyPress(Sender: TObject;
  var Key: Char);
begin
  FTableTagClick(Sender);
end;

function TDExport.GetFilename(): Boolean;
var
  Database: TCDatabase;
begin
  Database := BuildTitle();

  if (Assigned(Client) and (Client.Charset <> '')) then
    CodePage := Client.CharsetToCodePage(Client.Charset)
  else if ((DExport.Objects.Count = 1) and (TObject(DExport.Objects[0]) is TCBaseTable)) then
    CodePage := Client.CharsetToCodePage(TCBaseTable(DExport.Objects[0]).DefaultCharset)
  else if (Assigned(Database)) then
    CodePage := Client.CharsetToCodePage(Database.DefaultCharset)
  else
    CodePage := Client.CodePage;

  SaveDialog.Title := ReplaceStr(Preferences.LoadStr(582), '&', '');
  SaveDialog.InitialDir := Preferences.Path;
  SaveDialog.Filter := '';
  SaveDialog.DefaultExt := '';
  case (ExportType) of
    etSQLFile:
      begin
        SaveDialog.Filter := FilterDescription('sql') + ' (*.sql)|*.sql';
        SaveDialog.DefaultExt := '.sql';
        SaveDialog.Encodings.Text := EncodingCaptions();
      end;
    etTextFile:
      if (Objects.Count <= 1) then
      begin
        SaveDialog.Filter := FilterDescription('txt') + ' (*.txt;*.csv;*.tab;*.asc)|*.txt;*.csv;*.tab;*.asc';
        SaveDialog.DefaultExt := '.csv';
        SaveDialog.Encodings.Text := EncodingCaptions();
      end
      else
      begin
        SaveDialog.Filter := FilterDescription('zip') + ' (*.zip)|*.zip';
        SaveDialog.DefaultExt := '.zip';
        SaveDialog.Encodings.Clear();
      end;
    etExcelFile:
      begin
        SaveDialog.Filter := FilterDescription('xls') + ' (*.xls)|*.xls|' + FilterDescription('xlsx') + ' (*.xlsx)|*.xlsx';
        SaveDialog.DefaultExt := '.xls';
        SaveDialog.Encodings.Clear();
      end;
    etAccessFile:
      begin
        SaveDialog.Filter := FilterDescription('mdb') + ' (*.mdb)|*.mdb|' + FilterDescription('accdb') + ' (*.accdb)|*.accdb';
        SaveDialog.DefaultExt := '.mdb';
        SaveDialog.Encodings.Clear();
      end;
    etSQLiteFile:
      begin
        SaveDialog.Filter := FilterDescription('sqlite') + ' (*.db3;*.sqlite)|*.db3;*.sqlite';
        SaveDialog.DefaultExt := '.db3';
        SaveDialog.Encodings.Clear();
      end;
    etHTMLFile:
      begin
        SaveDialog.Filter := FilterDescription('html') + ' (*.html;*.htm)|*.html;*.htm';
        SaveDialog.DefaultExt := '.html';
        SaveDialog.Encodings.Text := EncodingCaptions(True);
      end;
    etPDFFile:
      begin
        SaveDialog.Filter := FilterDescription('pdf') + ' (*.pdf)|*.pdf';
        SaveDialog.DefaultExt := '.pdf';
        SaveDialog.Encodings.Clear();
      end;
    etXMLFile:
      begin
        SaveDialog.Filter := FilterDescription('xml') + ' (*.xml)|*.xml';
        SaveDialog.DefaultExt := '.xml';
        SaveDialog.Encodings.Text := EncodingCaptions(True);
      end;
  end;
  SaveDialog.Filter := SaveDialog.Filter + '|' + FilterDescription('*') + ' (*.*)|*.*';

  if (Filename <> '') then
    SaveDialog.FileName := Filename
  else
    SaveDialog.FileName := Title + SaveDialog.DefaultExt;

  if (SaveDialog.Encodings.Count = 0) then
    SaveDialog.EncodingIndex := -1
  else
    SaveDialog.EncodingIndex := SaveDialog.Encodings.IndexOf(CodePageToEncoding(CodePage));

  Result := SaveDialog.Execute();
  if (Result) then
  begin
    Preferences.Path := ExtractFilePath(SaveDialog.FileName);

    if ((SaveDialog.EncodingIndex < 0) or (SaveDialog.Encodings.Count = 0)) then
      CodePage := CP_ACP
    else
      CodePage := EncodingToCodePage(SaveDialog.Encodings[SaveDialog.EncodingIndex]);
    Filename := SaveDialog.FileName;
  end;
end;

procedure TDExport.InitTSFields();
var
  I: Integer;
  J: Integer;
  K: Integer;
begin
  ClearTSFields();

  if ((Objects.Count > 0) and (TCDBObject(Objects[0]) is TCTable)) then
    SetLength(FFields, TCTable(Objects[0]).Fields.Count)
  else if (Assigned(DBGrid)) then
    SetLength(FFields, DBGrid.FieldCount);

  FLDestFields.Visible :=
    (ExportType = etTextFile) and FCSVHeadline.Checked
    or (ExportType = etExcelFile)
    or (ExportType in [etXMLFile, etHTMLFile, etPrinter, etPDFFile]) and not FHTMLStructure.Checked;

  if (FLDestFields.Visible) then
  begin
    SetLength(FLReferrers, Length(FFields));
    SetLength(FDestFields, Length(FFields));
  end;

  ScrollBox.DisableAlign();

  for I := 0 to Length(FFields) - 1 do
  begin
    FFields[I] := TComboBox_Ext.Create(ScrollBox);
    FFields[I].Parent := ScrollBox;
    FFields[I].Left := FField1.Left;
    FFields[I].Top := FField1.Top + I * (FField2.Top - FField1.Top);
    FFields[I].Width := FField1.Width;
    FFields[I].Height := FField1.Height;
    FFields[I].Style := FField1.Style;
    FFields[I].Items.Add('');
    if ((Objects.Count > 0) and (TCDBObject(Objects[0]) is TCTable)) then
      for J := 0 to TCTable(Objects[0]).Fields.Count - 1 do
        FFields[I].Items.Add(TCTable(Objects[0]).Fields[J].Name)
    else if (Assigned(DBGrid)) then
      for J := 0 to DBGrid.FieldCount - 1 do
        FFields[I].Items.Add(DBGrid.Fields[J].DisplayName);
    FFields[I].ItemIndex := I + 1;
    FFields[I].OnChange := FField1.OnChange;
    FFields[I].OnExit := FField1.OnExit;

    if (FLDestFields.Visible) then
    begin
      FLReferrers[I] := TLabel.Create(ScrollBox);
      FLReferrers[I].Parent := ScrollBox;
      FLReferrers[I].Left := FLReferrer1.Left;
      FLReferrers[I].Top := FLReferrer1.Top + I * (FField2.Top - FField1.Top);
      FLReferrers[I].Width := FLReferrer1.Width;
      FLReferrers[I].Height := FLReferrer1.Height;
      FLReferrers[I].Caption := FLReferrer1.Caption;

      FDestFields[I] := TEdit.Create(ScrollBox);
      FDestFields[I].Parent := ScrollBox;
      FDestFields[I].Left := FDestField1.Left;
      FDestFields[I].Top := FDestField1.Top + I * (FField2.Top - FField1.Top);
      FDestFields[I].Width := FDestField1.Width;
      FDestFields[I].Height := FDestField1.Height;
      FDestFields[I].Enabled := ExportType in [etTextFile, etExcelFile, etHTMLFile, etPrinter, etPDFFile, etXMLFile];
      TEdit(FDestFields[I]).Text := FFields[I].Text;
      K := 2;
      for J := 0 to I - 1 do
        if (FFields[I].Text = FFields[J].Text) then
        begin
          TEdit(FDestFields[I]).Text := FFields[J].Text + '_' + IntToStr(K);
          Inc(K);
        end;
      FDestFields[I].OnChange := FDestField1.OnChange;
    end;
  end;

  ScrollBox.EnableAlign();
end;

procedure TDExport.InitTSJob();
var
  I: Integer;
  JobName: string;
begin
  if (DialogType in [edtCreateJob]) then
  begin
    FName.Text := Title;
    while (Assigned(Client.Account.JobByName(FName.Text))) do
    begin
      JobName := FName.Text;
      Delete(JobName, 1, Length(Title));
      if (JobName = '') then JobName := '1';
      JobName := Title + IntToStr(StrToInt(JobName) + 1);
      FName.Text := JobName;
    end;
  end;

  FSQLFile.Enabled := True;
  FTextFile.Enabled := False;
  FExcelFile.Enabled := False;
  FAccessFile.Enabled := False;
  FSQLiteFile.Enabled := False;
  FODBC.Enabled := False;
  FHTMLFile.Enabled := True;
  FXMLFile.Enabled := True;
  FPDFFile.Enabled := True;

  for I := 0 to Objects.Count - 1 do
    if (TObject(Objects[I]) is TCTable) then
    begin
      FTextFile.Enabled := True;
      FExcelFile.Enabled := True;
      FAccessFile.Enabled := True;
      FSQLiteFile.Enabled := True;
      FODBC.Enabled := True;
      FXMLFile.Enabled := True;
    end;
  FSQLFile.Checked := True;
end;

function TDExport.InitTSSelect(): Boolean;
var
  Database: TCDatabase;
  I: Integer;
  J: Integer;
  K: Integer;
  L: Integer;
  Nodes: TList;
begin
  Result := True;
  if (FSelect.Items[0].Count = 0) then
  begin
    Nodes := TList.Create();
    if (not Client.Update()) then
      Result := False
    else
      for I := 0 to Length(Job.Objects) - 1 do
        if (Job.Objects[I].ObjectType = jotServer) then
          Nodes.Add(FSelect.Items[0])
        else if (not Client.Databases.Update()) then
          Result := False
        else
        begin
          FSelect.Items[0].Expand(False);
          if (Job.Objects[I].ObjectType = jotDatabase) then
          begin
            for J := 0 to FSelect.Items[0].Count - 1 do
              if (Client.Databases.NameCmp(FSelect.Items[0].Item[J].Text, Job.Objects[I].Name) = 0) then
                Nodes.Add(FSelect.Items[0].Item[J]);
          end
          else
          begin
            for J := 0 to FSelect.Items[0].Count - 1 do
              if (Client.Databases.NameCmp(FSelect.Items[0].Item[J].Text, Job.Objects[I].DatabaseName) = 0) then
              begin
                Database := Client.DatabaseByName(Job.Objects[I].DatabaseName);
                if (not Database.Update()) then
                  Result := False
                else
                begin
                  FSelect.Items[0].Item[J].Expand(False);
                  for K := 0 to FSelect.Items[0].Item[J].Count - 1 do
                    if (Job.Objects[I].ObjectType in [jotTable, jotProcedure, jotFunction, jotEvent]) then
                    begin
                      if ((Job.Objects[I].ObjectType = jotTable) and (TObject(FSelect.Items[0].Item[J].Item[K].Data) is TCTable) and (Database.Tables.NameCmp(TCTable(FSelect.Items[0].Item[J].Item[K].Data).Name, Job.Objects[I].Name) = 0)
                        or (Job.Objects[I].ObjectType = jotProcedure) and (TObject(FSelect.Items[0].Item[J].Item[K].Data) is TCProcedure) and (Database.Routines.NameCmp(TCProcedure(FSelect.Items[0].Item[J].Item[K].Data).Name, Job.Objects[I].Name) = 0)
                        or (Job.Objects[I].ObjectType = jotFunction) and (TObject(FSelect.Items[0].Item[J].Item[K].Data) is TCFunction) and (Database.Routines.NameCmp(TCFunction(FSelect.Items[0].Item[J].Item[K].Data).Name, Job.Objects[I].Name) = 0)
                        or (Job.Objects[I].ObjectType = jotEvent) and (TObject(FSelect.Items[0].Item[J].Item[K].Data) is TCEvent) and (Database.Events.NameCmp(TCEvent(FSelect.Items[0].Item[J].Item[K].Data).Name, Job.Objects[I].Name) = 0)) then
                        Nodes.Add(FSelect.Items[0].Item[J].Item[K]);
                    end
                    else if (Job.Objects[I].ObjectType = jotTrigger) then
                    begin
                      if ((Job.Objects[I].ObjectType = jotTable) and (TObject(FSelect.Items[0].Item[J].Item[K].Data) = Database.TriggerByName(Job.Objects[I].Name).Table)) then
                      begin
                        FSelect.Items[0].Item[J].Item[K].Expand(False);
                        for L := 0 to FSelect.Items[0].Item[J].Item[K].Count - 1 do
                          if ((TObject(FSelect.Items[0].Item[J].Item[K].Data) is TCTrigger) and (Database.Triggers.NameCmp(TCTrigger(FSelect.Items[0].Item[J].Item[K].Item[L].Data).Name, Job.Objects[I].Name) = 0)) then
                            Nodes.Add(FSelect.Items[0].Item[J].Item[K].Item[L]);
                      end;
                    end;
                end;
              end;
          end;
        end;
    FSelect.Select(Nodes);
    Nodes.Free();
  end;
end;

function TDExport.ObjectsFromFSelect(): Boolean;
var
  Child: TTreeNode;
  I: Integer;
begin
  Result := True;

  Objects.Clear();
  for I := 0 to FSelect.Items.Count - 1 do
    if (FSelect.Items[I].Selected) then
      if (FSelect.Items[I].ImageIndex = iiServer) then
      begin
        Child := FSelect.Items[I].getFirstChild();
        while (Assigned(Child)) do
        begin
          Objects.Add(Child.Data);
          Child := Child.getNextSibling();
        end;
      end
      else
        Objects.Add(FSelect.Items[I].Data);
end;

procedure TDExport.OnError(const Sender: TObject; const Error: TTools.TError; const Item: TTools.TItem; const ShowRetry: Boolean; var Success: TDataAction);
var
  ErrorMsg: string;
  Flags: Integer;
  Msg: string;
  Text: string;
begin
  ErrorMsg := '';
  case (Error.ErrorType) of
    TE_Database:
      begin
        Msg := Preferences.LoadStr(165, IntToStr(Item.Client.ErrorCode), Item.Client.ErrorMessage);
        ErrorMsg := SQLUnwrapStmt(Item.Client.ErrorMessage);
        if (Item.Client.ErrorCode > 0) then
          ErrorMsg := ErrorMsg + ' (#' + IntToStr(Item.Client.ErrorCode) + ')';
        ErrorMsg := ErrorMsg + '  -  ' + SQLUnwrapStmt(Item.Client.CommandText);
      end;
    TE_File:
      begin
        Msg := Error.ErrorMessage + ' (#' + IntToStr(Error.ErrorCode) + ')';
        ErrorMsg := Msg;
      end;
    TE_ODBC:
      begin
        if (Error.ErrorCode = 0) then
          Msg := Error.ErrorMessage
        else
          Msg := Error.ErrorMessage + ' (#' + IntToStr(Error.ErrorCode) + ')';
        ErrorMsg := Msg;
      end;
    else
      begin
        Msg := Error.ErrorMessage;
        ErrorMsg := Msg;
      end;
  end;

  if (not ShowRetry) then
    Flags := MB_OK + MB_ICONERROR
  else
    Flags := MB_CANCELTRYCONTINUE + MB_ICONERROR;
  case (MsgBox(Msg, Preferences.LoadStr(45), Flags, Handle)) of
    IDOK,
    IDCANCEL,
    IDABORT: Success := daAbort;
    IDRETRY,
    IDTRYAGAIN: Success := daRetry;
    IDCONTINUE,
    IDIGNORE: Success := daFail;
  end;

  if ((Success in [daAbort, daFail]) and (ErrorMsg <> '')) then
  begin
    Text := FErrorMessages.Text;
    if (Text <> '') then
      Text := Text + #13#10;
    Text := Text + Trim(ErrorMsg);

    try
      SendMessage(FErrorMessages.Handle, WM_SETTEXT, 0, LPARAM(PChar(Text)));
    except
    end;

    PostMessage(FErrorMessages.Handle, WM_VSCROLL, SB_BOTTOM, 0);

    FErrors.Caption := IntToStr(TTools(Sender).ErrorCount);
  end;
end;

procedure TDExport.OnExecuted(const ASuccess: Boolean);
begin
  if (not Export.Suspended) then
    PostMessage(Handle, CM_EXECUTIONDONE, WPARAM(ASuccess), 0)
  else
  begin
    Perform(CM_EXECUTIONDONE, WPARAM(ASuccess), 0);
    Application.ProcessMessages();
  end;
end;

procedure TDExport.OnUpdate(const AProgressInfos: TTools.TProgressInfos);
begin
  MoveMemory(@ProgressInfos, @AProgressInfos, SizeOf(AProgressInfos));

  if (not Export.Suspended) then
    PostMessage(Handle, CM_UPDATEPROGRESSINFO, 0, LPARAM(@ProgressInfos))
  else
  begin
    Perform(CM_UPDATEPROGRESSINFO, 0, LPARAM(@ProgressInfos));
    Application.ProcessMessages();
  end;
end;

procedure TDExport.TSCSVOptionsShow(Sender: TObject);
var
  TabSheet: TTabSheet;
begin
  if (DialogType in [edtCreateJob, edtEditJob]) then
    TabSheet := TSTask
  else
    TabSheet := TSExecute;

  ClearTSFields();

  FSeparatorClick(Self);

  FBForward.Default := True;

  FBCancel.Caption := Preferences.LoadStr(30);
  FBCancel.ModalResult := mrCancel;
  FBCancel.Default := False;

  TabSheet.Enabled := not TSFields.Enabled;
  CheckActivePageChange(TSCSVOptions.PageIndex);
end;

procedure TDExport.TSExecuteShow(Sender: TObject);
var
  ExportExcel: TTExportExcel;
  ExportHTML: TTExportHTML;
  ExportPDF: TTExportCanvas;
  ExportSQL: TTExportSQL;
  ExportText: TTExportText;
  ExportXML: TTExportXML;
  I: Integer;
begin
  Client.UnRegisterEventProc(FormClientEvent);

  CheckActivePageChange(TSExecute.PageIndex);
  FBBack.Enabled := False;

  FBForward.Default := False;
  FBCancel.Default := True;
  ActiveControl := FBCancel;

  FErrors.Caption := '0';
  FErrorMessages.Lines.Clear();

  ProgressInfos.TablesDone := 0;
  ProgressInfos.TablesSum := 0;
  ProgressInfos.RecordsDone := 0;
  ProgressInfos.RecordsSum := 0;
  ProgressInfos.TimeDone := 0;
  ProgressInfos.TimeSum := 0;
  ProgressInfos.Progress := 0;
  SendMessage(Handle, CM_UPDATEPROGRESSINFO, 0, LPARAM(@ProgressInfos));

  if (Assigned(DBGrid)) then
  begin
    case (ExportType) of
      etSQLFile:
        try
          ExportSQL := TTExportSQL.Create(Client, Filename, CodePage);
          ExportSQL.CreateDatabaseStmts := FCreateDatabase.Checked;
          ExportSQL.Data := FHTMLData.Checked;
          ExportSQL.DisableKeys := FDisableKeys.Checked;
          ExportSQL.IncludeDropStmts := FDrop.Checked;
          ExportSQL.ReplaceData := FReplaceData.Checked;
          ExportSQL.Structure := FSQLStructure.Checked;
          ExportSQL.UseDatabaseStmts := FUseDatabase.Checked;

          Export := ExportSQL;
        except
          MsgBox(Preferences.LoadStr(522, Filename), Preferences.LoadStr(45), MB_OK + MB_ICONERROR);
        end;
      etTextFile:
        try
          ExportText := TTExportText.Create(Client, Filename, CodePage);
          ExportText.Data := True;
          if (FSeparatorTab.Checked) then
            ExportText.Delimiter := #9;
          if (FSeparatorChar.Checked) then
            ExportText.Delimiter := FSeparator.Text;
          ExportText.QuoteStringValues := FStringQuote.Checked;
          ExportText.QuoteValues := FAllQuote.Checked;
          ExportText.Quoter := FQuoteChar.Text[1];
          ExportText.Structure := FCSVHeadline.Checked;

          Export := ExportText;
        except
          MsgBox(Preferences.LoadStr(522, Filename), Preferences.LoadStr(45), MB_OK + MB_ICONERROR);
        end;
      etExcelFile:
        try
          ExportExcel := TTExportExcel.Create(Client, Filename);
          ExportExcel.Structure := True;

          Export := ExportExcel;
        except
          MsgBox(Preferences.LoadStr(522, Filename), Preferences.LoadStr(45), MB_OK + MB_ICONERROR);
        end;
      etXMLFile:
        try
          ExportXML := TTExportXML.Create(Client, Filename, CodePage);
          if (FDatabaseTagName.Checked) then
          begin
            ExportXML.DatabaseTag := 'database';
            ExportXML.DatabaseAttribute := '';
          end
          else if (FDatabaseTagFree.Checked) then
          begin
            ExportXML.DatabaseTag := FDatabaseTag.Text;
            ExportXML.DatabaseAttribute := FDatabaseAttribute.Text;
          end
          else
          begin
            ExportXML.DatabaseTag := '';
            ExportXML.DatabaseAttribute := '';
          end;
          ExportXML.RootTag := FRootTag.Text;
          ExportXML.Structure := True;
          if (FTableTagName.Checked) then
          begin
            ExportXML.TableTag := 'Table';
            ExportXML.TableAttribute := '';
          end
          else if (FTableTagFree.Checked) then
          begin
            ExportXML.TableTag := FTableTag.Text;
            ExportXML.TableAttribute := FTableAttribute.Text;
          end
          else
          begin
            ExportXML.TableTag := '';
            ExportXML.TableAttribute := '';
          end;
          ExportXML.RecordTag := FRecordTag.Text;
          if (FFieldTagName.Checked) then
          begin
            ExportXML.FieldTag := 'database';
            ExportXML.FieldAttribute := '';
          end
          else
          begin
            ExportXML.FieldTag := FFieldTag.Text;
            ExportXML.FieldAttribute := FFieldAttribute.Text;
          end;

          Export := ExportXML;
        except
          MsgBox(Preferences.LoadStr(522, Filename), Preferences.LoadStr(45), MB_OK + MB_ICONERROR);
        end;
      etHTMLFile:
        try
          ExportHTML := TTExportHTML.Create(Client, Filename, CodePage);
          ExportHTML.Data := FHTMLData.Checked;
          ExportHTML.TextContent := FHTMLShowMemoContent.Checked;
          ExportHTML.NULLText := FHTMLNullText.Checked;
          ExportHTML.RowBackground := FHTMLRowBGColorEnabled.Checked;
          ExportHTML.Structure := FHTMLStructure.Checked;

          Export := ExportHTML;
        except
          MsgBox(Preferences.LoadStr(522, Filename), Preferences.LoadStr(45), MB_OK + MB_ICONERROR);
        end;
      etPrinter,
      etPDFFile:
        try
          if (ExportType = etPrinter) then
            ExportPDF := TTExportPrint.Create(Client, Title)
          else
            ExportPDF := TTExportPDF.Create(Client, Filename);
          ExportPDF.Data := FHTMLData.Checked;
          ExportPDF.NULLText := FHTMLNullText.Checked;
          ExportPDF.Structure := FHTMLStructure.Checked;

          Export := ExportPDF;
        except
          MsgBox(Preferences.LoadStr(522, Filename), Preferences.LoadStr(45), MB_OK + MB_ICONERROR);
        end;
    end;

    if (Assigned(Export)) then
    begin
      if (Length(FFields) = 0) then
      begin
        for I := 0 to DBGrid.Columns.Count - 1 do
          if (DBGrid.Columns[I].Visible) then
            begin
              SetLength(Export.Fields, Length(Export.Fields) + 1);
              Export.Fields[Length(Export.Fields) - 1] := DBGrid.Columns[I].Field;
              SetLength(Export.DestinationFields, Length(Export.DestinationFields) + 1);
              Export.DestinationFields[Length(Export.DestinationFields) - 1].Name := DBGrid.Columns[I].DisplayName;
            end;
      end
      else
        for I := 0 to Length(FFields) - 1 do
          if (FFields[I].ItemIndex > 0) then
          begin
            SetLength(Export.Fields, Length(Export.Fields) + 1);
            Export.Fields[Length(Export.Fields) - 1] := DBGrid.Fields[FFields[I].ItemIndex - 1];
            SetLength(Export.DestinationFields, Length(Export.DestinationFields) + 1);
            Export.DestinationFields[Length(Export.DestinationFields) - 1].Name := FDestFields[I].Text;
          end;

      Export.Wnd := Handle;
      Export.OnUpdate := OnUpdate;
      Export.OnExecuted := OnExecuted;
      Export.OnError := OnError;
      Export.Add(DBGrid);
      if (Export.Client.Asynchron) then
        Export.Start()
      else
        Export.Execute();
    end;
  end
  else
  begin
    Objects.Sort(DBObjectsSortItem);

    case (ExportType) of
      etSQLFile:
        try
          ExportSQL := TTExportSQL.Create(Client, Filename, CodePage);
          ExportSQL.CreateDatabaseStmts := FCreateDatabase.Checked;
          ExportSQL.Data := FSQLData.Checked;
          ExportSQL.DisableKeys := FDisableKeys.Checked;
          ExportSQL.IncludeDropStmts := FDrop.Checked;
          ExportSQL.ReplaceData := FReplaceData.Checked;
          ExportSQL.Structure := FSQLStructure.Checked;
          ExportSQL.UseDatabaseStmts := FUseDatabase.Checked;
          for I := 0 to Objects.Count - 1 do
            ExportSQL.Add(TCDBObject(Objects[I]));

          Export := ExportSQL;
        except
          MsgBox(Preferences.LoadStr(522, Filename), Preferences.LoadStr(45), MB_OK + MB_ICONERROR);
        end;
      etTextFile:
        try
          ExportText := TTExportText.Create(Client, Filename, CodePage);
          ExportText.Data := True;
          if (FSeparatorTab.Checked) then
            ExportText.Delimiter := #9;
          if (FSeparatorChar.Checked) then
            ExportText.Delimiter := FSeparator.Text;
          ExportText.QuoteStringValues := FStringQuote.Checked;
          ExportText.QuoteValues := FAllQuote.Checked;
          ExportText.Quoter := FQuoteChar.Text[1];
          ExportText.Structure := FCSVHeadline.Checked;
          for I := 0 to Objects.Count - 1 do
            ExportText.Add(TCDBObject(Objects[I]));

          Export := ExportText;
        except
          MsgBox(Preferences.LoadStr(522, Filename), Preferences.LoadStr(45), MB_OK + MB_ICONERROR);
        end;
      etExcelFile:
        try
          Export := TTExportExcel.Create(Client, Filename);
          Export.Data := True;
          Export.Structure := True;
          for I := 0 to Objects.Count - 1 do
            Export.Add(TCDBObject(Objects[I]));
        except
          MsgBox(Preferences.LoadStr(522, Filename), Preferences.LoadStr(45), MB_OK + MB_ICONERROR);
        end;
      etAccessFile:
        try
          Export := TTExportAccess.Create(Client, Filename);
          Export.Structure := True;
          Export.Data := True;
          for I := 0 to Objects.Count - 1 do
            if (TCDBObject(Objects[I]) is TCBaseTable) then
              Export.Add(TCDBObject(Objects[I]));
        except
          MsgBox(Preferences.LoadStr(522, Filename), Preferences.LoadStr(45), MB_OK + MB_ICONERROR);
        end;
      etODBC:
        begin
          Export := TTExportODBC.Create(Client, ODBCEnv, ODBC);
          Export.Data := True;
          Export.Structure := True;
          for I := 0 to Objects.Count - 1 do
            if (TCDBObject(Objects[I]) is TCBaseTable) then
              Export.Add(TCDBObject(Objects[I]));
        end;
      etSQLiteFile:
        try
          Export := TTExportSQLite.Create(Client, Filename);
          Export.Data := True;
          Export.Structure := True;
          for I := 0 to Objects.Count - 1 do
            if (TCDBObject(Objects[I]) is TCBaseTable) then
              Export.Add(TCDBObject(Objects[I]));
        except
          MsgBox(Preferences.LoadStr(522, Filename), Preferences.LoadStr(45), MB_OK + MB_ICONERROR);
        end;
      etXMLFile:
        try
          ExportXML := TTExportXML.Create(Client, Filename, CodePage);
          ExportXML.Data := True;
          if (FDatabaseTagName.Checked) then
          begin
            ExportXML.DatabaseTag := 'database';
            ExportXML.DatabaseAttribute := '';
          end
          else if (FDatabaseTagFree.Checked) then
          begin
            ExportXML.DatabaseTag := FDatabaseTag.Text;
            ExportXML.DatabaseAttribute := FDatabaseAttribute.Text;
          end
          else
          begin
            ExportXML.DatabaseTag := '';
            ExportXML.DatabaseAttribute := '';
          end;
          ExportXML.RootTag := FRootTag.Text;
          ExportXML.Structure := True;
          if (FTableTagName.Checked) then
          begin
            ExportXML.TableTag := 'Table';
            ExportXML.TableAttribute := '';
          end
          else if (FTableTagFree.Checked) then
          begin
            ExportXML.TableTag := FTableTag.Text;
            ExportXML.TableAttribute := FTableAttribute.Text;
          end
          else
          begin
            ExportXML.TableTag := '';
            ExportXML.TableAttribute := '';
          end;
          ExportXML.RecordTag := FRecordTag.Text;
          if (FFieldTagName.Checked) then
          begin
            ExportXML.FieldTag := 'database';
            ExportXML.FieldAttribute := '';
          end
          else
          begin
            ExportXML.FieldTag := FFieldTag.Text;
            ExportXML.FieldAttribute := FFieldAttribute.Text;
          end;
          for I := 0 to Objects.Count - 1 do
            ExportXML.Add(TCDBObject(Objects[I]));

          Export := ExportXML;
        except
          MsgBox(Preferences.LoadStr(522, Filename), Preferences.LoadStr(45), MB_OK + MB_ICONERROR);
        end;
      etHTMLFile:
        try
          ExportHTML := TTExportHTML.Create(Client, Filename, CodePage);
          ExportHTML.Data := FHTMLData.Checked;
          ExportHTML.TextContent := FHTMLShowMemoContent.Checked;
          ExportHTML.NULLText := FHTMLNullText.Checked;
          ExportHTML.RowBackground := FHTMLRowBGColorEnabled.Checked;
          ExportHTML.Structure := FHTMLStructure.Checked;
          for I := 0 to Objects.Count - 1 do
            ExportHTML.Add(TCDBObject(Objects[I]));

          Export := ExportHTML;
        except
          MsgBox(Preferences.LoadStr(522, Filename), Preferences.LoadStr(45), MB_OK + MB_ICONERROR);
        end;
      etPrinter,
      etPDFFile:
        try
          if (ExportType = etPrinter) then
            ExportPDF := TTExportPrint.Create(Client, Title)
          else
            ExportPDF := TTExportPDF.Create(Client, Filename);
          ExportPDF.Data := FHTMLData.Checked;
          ExportPDF.NULLText := FHTMLNullText.Checked;
          ExportPDF.Structure := FHTMLStructure.Checked;
          for I := 0 to Objects.Count - 1 do
            ExportPDF.Add(TCDBObject(Objects[I]));

          Export := ExportPDF;
        except
          MsgBox(Preferences.LoadStr(522, Filename), Preferences.LoadStr(45), MB_OK + MB_ICONERROR);
        end;
    end;

    if (Assigned(Export)) then
    begin
      if ((Objects.Count = 1) and (TCDBObject(Objects[0]) is TCTable)) then
        for I := 0 to Length(FFields) - 1 do
          if (FFields[I].ItemIndex > 0) then
          begin
            SetLength(Export.TableFields, Length(Export.TableFields) + 1);
            Export.TableFields[Length(Export.TableFields) - 1] := TCTable(Objects[0]).Fields[FFields[I].ItemIndex - 1];
            SetLength(Export.DestinationFields, Length(Export.DestinationFields) + 1);
            Export.DestinationFields[Length(Export.DestinationFields) - 1].Name := FDestFields[I].Text;
          end;

      Export.Wnd := Handle;
      Export.OnUpdate := OnUpdate;
      Export.OnExecuted := OnExecuted;
      Export.OnError := OnError;

      if (Export.Client.Asynchron) then
        Export.Start()
      else
        Export.Execute();
    end;
  end;
end;

procedure TDExport.TSFieldsShow(Sender: TObject);
var
  TabSheet: TTabSheet;
begin
  if (DialogType in [edtCreateJob, edtEditJob]) then
    TabSheet := TSTask
  else
    TabSheet := TSExecute;

  TabSheet.Enabled := True;
  CheckActivePageChange(TSFields.PageIndex);
end;

procedure TDExport.TSHTMLOptionsShow(Sender: TObject);
begin
  FHTMLStructure.Enabled := not Assigned(DBGrid) or not (DBGrid.DataSource.DataSet is TMySQLTable);
  FHTMLStructure.Checked := FHTMLStructure.Checked and FHTMLStructure.Enabled;
  FHTMLStructureClick(Sender);
  FHTMLDataClick(Sender);

  FHTMLShowMemoContent.Visible := not (ExportType in [etPrinter, etPDFFile]); FLHTMLViewDatas.Visible := FHTMLShowMemoContent.Visible;
  FHTMLRowBGColorEnabled.Visible := not (ExportType in [etPrinter, etPDFFile]); FLHTMLBGColorEnabled.Visible := FHTMLRowBGColorEnabled.Visible;
end;

procedure TDExport.TSJobShow(Sender: TObject);
begin
  ObjectsFromFSelect();

  PageControl.Visible := Boolean(Perform(CM_POST_AFTEREXECUTESQL, 0, 0));
  PSQLWait.Visible := not PageControl.Visible;

  FJobOptionChange(Sender);
end;

procedure TDExport.TSODBCSelectShow(Sender: TObject);
var
  DataSourceName: array [0 .. SQL_MAX_DSN_LENGTH] of SQLTCHAR;
  Item: TListItem;
begin
  if (ODBCEnv = SQL_NULL_HANDLE) then
  begin
    if (SQL_SUCCEEDED(SQLAllocHandle(SQL_HANDLE_ENV, SQL_NULL_HANDLE, @ODBCEnv))
      and SQL_SUCCEEDED(SQLSetEnvAttr(ODBCEnv, SQL_ATTR_ODBC_VERSION, SQLPOINTER(SQL_OV_ODBC3), SQL_IS_UINTEGER))
      and SQL_SUCCEEDED(SQLAllocHandle(SQL_HANDLE_DBC, ODBCEnv, @ODBC))
      and SQL_SUCCEEDED(SQLDataSources(ODBCEnv, SQL_FETCH_FIRST, @DataSourceName, Length(DataSourceName), nil, nil, 0, nil))) then
      repeat
        Item := FODBCSelect.Items.Add();
        Item.Caption := DataSourceName;
        Item.ImageIndex := iiDatabase;
      until (not SQL_SUCCEEDED(SQLDataSources(ODBCEnv, SQL_FETCH_NEXT, @DataSourceName, Length(DataSourceName), nil, nil, 0, nil)));
  end;

  FBForward.Default := True;

  FBCancel.Caption := Preferences.LoadStr(30);
  FBCancel.ModalResult := mrCancel;
  FBCancel.Default := False;

  CheckActivePageChange(TSODBCSelect.PageIndex);
end;

procedure TDExport.TSOptionsHide(Sender: TObject);
begin
  if (TSFields.Enabled) then
    InitTSFields();
end;

procedure TDExport.TSSelectShow(Sender: TObject);
begin
  FSelectChange(nil, FSelect.Selected);
end;

procedure TDExport.TSSQLOptionsShow(Sender: TObject);
begin
  FSQLStructure.Enabled := not Assigned(DBGrid) or (DBGrid.DataSource.DataSet is TMySQLDataSet) and (TMySQLDataSet(DBGrid.DataSource.DataSet).TableName <> '');
  FSQLData.Enabled := not Assigned(DBGrid);

  FSQLStructure.Checked := FSQLStructure.Checked and FSQLStructure.Enabled;
  FSQLData.Checked := FSQLData.Checked or Assigned(DBGrid);

  FBForward.Default := True;

  FBCancel.Caption := Preferences.LoadStr(30);
  FBCancel.ModalResult := mrCancel;
  FBCancel.Default := False;

  FSQLOptionClick(Sender);
end;

procedure TDExport.TSTaskShow(Sender: TObject);
begin

  CheckActivePageChange(TSTask.PageIndex);
end;

procedure TDExport.TSXMLOptionChange(Sender: TObject);
begin
  FBForward.Enabled :=
    (FRootTag.Text <> '')
    and (not FDatabaseTagDisabled.Checked or FDatabaseTagDisabled.Enabled)
    and (not FDatabaseTagName.Checked or FDatabaseTagName.Enabled)
    and (not FDatabaseTagFree.Checked or FDatabaseTagFree.Enabled and (FDatabaseTag.Text <> '') and (FDatabaseAttribute.Text <> ''))
    and (not FTableTagDisabled.Checked or FTableTagDisabled.Enabled)
    and (not FTableTagName.Checked or FTableTagName.Enabled)
    and (not FTableTagFree.Checked or FTableTagFree.Enabled and (FTableTag.Text <> '') and (FTableAttribute.Text <> ''))
    and (FRecordTag.Text <> '')
    and (not FFieldTagName.Checked or FFieldTagName.Enabled)
    and (not FFieldTagFree.Checked or FFieldTagFree.Enabled and (FFieldTag.Text <> '') and (FFieldAttribute.Text <> ''));

  FDatabaseTag.Enabled := FDatabaseTagFree.Checked;
  FDatabaseAttribute.Enabled := FDatabaseTagFree.Checked;

  FTableTag.Enabled := FTableTagFree.Checked;
  FTableAttribute.Enabled := FTableTagFree.Checked;
end;

procedure TDExport.TSXMLOptionsHide(Sender: TObject);
begin
  if (TSFields.Enabled) then
    InitTSFields();
end;

procedure TDExport.TSXMLOptionsShow(Sender: TObject);
var
  DatabaseCount: Integer;
  I: Integer;
  OldDatabase: TCDatabase;
begin
  DatabaseCount := 0;
  OldDatabase := nil;
  for I := 0 to Objects.Count - 1 do
  begin
    if (TCDBObject(Objects[I]).Database <> OldDatabase) then
      Inc(DatabaseCount);
    OldDatabase := TCDBObject(Objects[I]).Database;
  end;

  FDatabaseTagDisabled.Enabled := DatabaseCount <= 1;
  if (FDatabaseTagDisabled.Checked and not FDatabaseTagDisabled.Enabled) then
    FDatabaseTagFree.Checked := True;

  FTableTagDisabled.Enabled := Objects.Count <= 1;
  if (FTableTagDisabled.Checked and not FTableTagDisabled.Enabled) then
    FTableTagFree.Checked := True;

  CheckActivePageChange(TSXMLOptions.PageIndex);
  TSXMLOptionChange(Sender);
end;

initialization
  FExport := nil;
end.

