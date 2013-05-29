unit fDImport;

interface {********************************************************************}

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms,
  Dialogs, ComCtrls, ExtCtrls, StdCtrls, DB, DBTables, Consts, Contnrs,
  ComCtrls_Ext, Forms_Ext, StdCtrls_Ext, ExtCtrls_Ext, Dialogs_Ext,
  MySQLDB,
  fSession, fTools, fPreferences,
  fBase;

const
  LargeSQLScriptSize = 100 * 1024;

type
  TLoadFromStream = procedure(Stream: TStream) of object;

  TDImport = class (TForm_Ext)
    FAccessFile: TRadioButton;
    FBBack: TButton;
    FBCancel: TButton;
    FBDataSource: TButton;
    FBFilename: TButton;
    FBForward: TButton;
    FBHelp: TButton;
    FCollation: TComboBox_Ext;
    FCSVHeadline: TCheckBox;
    FCSVPreview: TListView;
    FDaily: TRadioButton;
    FData: TCheckBox;
    FDataSource: TEdit;
    FCharset: TComboBox_Ext;
    FDelimiter: TEdit;
    FDelimiterChar: TRadioButton;
    FDelimiterTab: TRadioButton;
    FDoneRecords: TLabel;
    FDoneTables: TLabel;
    FDoneTime: TLabel;
    FEnabled: TCheckBox;
    FEngine: TComboBox_Ext;
    FEntieredRecords: TLabel;
    FEntieredTables: TLabel;
    FEntieredTime: TLabel;
    FErrorMessages: TRichEdit;
    FErrors: TLabel;
    FExcelFile: TRadioButton;
    FField1: TComboBox_Ext;
    FFilename: TEdit;
    FInsert: TRadioButton;
    FL2RecordTag: TLabel;
    FL3RecordTag: TLabel;
    FLCollation: TLabel;
    FLCSVHeadline: TLabel;
    FLDataSource: TLabel;
    FLCharset: TLabel;
    FLDelimiter: TLabel;
    FLDone: TLabel;
    FLEnabled: TLabel;
    FLEngine: TLabel;
    FLEntiered: TLabel;
    FLErrors: TLabel;
    FLExecution: TLabel;
    FLImportType: TLabel;
    FLFields: TLabel;
    FLFilename: TLabel;
    FLStmtType: TLabel;
    FLName: TLabel;
    FLProgressRecords: TLabel;
    FLProgressTables: TLabel;
    FLProgressTime: TLabel;
    FLQuoteValues: TLabel;
    FLRecordTag: TLabel;
    FLReferrer1: TLabel;
    FLRowFormat: TLabel;
    FLSourceFields: TLabel;
    FLStart: TLabel;
    FLWhat: TLabel;
    FMonthly: TRadioButton;
    FName: TEdit;
    FODBC: TRadioButton;
    FProgressBar: TProgressBar;
    FQuoteChar: TEdit;
    FQuoteNothing: TRadioButton;
    FQuoteStrings: TRadioButton;
    FRecordTag: TEdit;
    FReplace: TRadioButton;
    FRowFormat: TComboBox_Ext;
    FSelect: TTreeView_Ext;
    FSingle: TRadioButton;
    FSourceField1: TEdit;
    FSourceField2: TEdit;
    FSQLFile: TRadioButton;
    FStartDate: TDateTimePicker;
    FStartTime: TDateTimePicker;
    FStructure: TCheckBox;
    FTables: TListView;
    FTextFile: TRadioButton;
    FUpdate: TRadioButton;
    FWeekly: TRadioButton;
    FXMLFile: TRadioButton;
    GBasics: TGroupBox_Ext;
    GCSVHow: TGroupBox_Ext;
    GCSVPreview: TGroupBox_Ext;
    GErrorMessages: TGroupBox_Ext;
    GFields: TGroupBox_Ext;
    GImportType: TGroupBox_Ext;
    GStructure: TGroupBox_Ext;
    GWhat: TGroupBox_Ext;
    GProgress: TGroupBox_Ext;
    GSelect: TGroupBox_Ext;
    GTables: TGroupBox_Ext;
    GTask: TGroupBox_Ext;
    GXMLHow: TGroupBox_Ext;
    OpenDialog: TOpenDialog_Ext;
    PageControl: TPageControl;
    PCSVPreview: TPanel_Ext;
    PDelimiter: TPanel_Ext;
    PErrorMessages: TPanel_Ext;
    PQuoting: TPanel_Ext;
    PSelect: TPanel_Ext;
    PSQLWait: TPanel_Ext;
    PTables: TPanel_Ext;
    ScrollBox: TScrollBox;
    Sizer: TRadioButton;
    TSCSVOptions: TTabSheet;
    TSExecute: TTabSheet;
    TSFields: TTabSheet;
    TSStmtType: TTabSheet;
    TSJob: TTabSheet;
    TSWhat: TTabSheet;
    TSSelect: TTabSheet;
    TSTables: TTabSheet;
    TSTask: TTabSheet;
    TSXMLOptions: TTabSheet;
    procedure FBBackClick(Sender: TObject);
    procedure FBCancelClick(Sender: TObject);
    procedure FBFilenameClick(Sender: TObject);
    procedure FBForwardClick(Sender: TObject);
    procedure FBHelpClick(Sender: TObject);
    procedure FCSVKeyPress(Sender: TObject; var Key: Char);
    procedure FCSVPreviewUpdate(Sender: TObject);
    procedure FDataClick(Sender: TObject);
    procedure FDataKeyPress(Sender: TObject; var Key: Char);
    procedure FCharsetChange(Sender: TObject);
    procedure FDelimiterClick(Sender: TObject);
    procedure FDelimiterKeyPress(Sender: TObject; var Key: Char);
    procedure FFieldExit(Sender: TObject);
    procedure FImportTypeChange(Sender: TObject);
    procedure FJobOptionChange(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure FormCreate(Sender: TObject);
    procedure FormHide(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure FQuoteClick(Sender: TObject);
    procedure FQuoteKeyPress(Sender: TObject; var Key: Char);
    procedure FSelectChange(Sender: TObject; Node: TTreeNode);
    procedure FSelectGetImageIndex(Sender: TObject; Node: TTreeNode);
    procedure FSelectExpanding(Sender: TObject; Node: TTreeNode;
      var AllowExpansion: Boolean);
    procedure FStmtTypeClick(Sender: TObject);
    procedure FStmtTypeKeyPress(Sender: TObject; var Key: Char);
    procedure FStructureClick(Sender: TObject);
    procedure FStructureKeyPress(Sender: TObject; var Key: Char);
    procedure FTablesDblClick(Sender: TObject);
    procedure FTablesSelectItem(Sender: TObject; Item: TListItem;
      Selected: Boolean);
    procedure TSCSVOptionsHide(Sender: TObject);
    procedure TSCSVOptionsShow(Sender: TObject);
    procedure TSExecuteShow(Sender: TObject);
    procedure TSFieldsChange(Sender: TObject);
    procedure TSFieldsShow(Sender: TObject);
    procedure TSStmtTypeShow(Sender: TObject);
    procedure TSWhatHide(Sender: TObject);
    procedure TSWhatShow(Sender: TObject);
    procedure TSTablesHide(Sender: TObject);
    procedure TSTablesShow(Sender: TObject);
    procedure TSXMLOptionChange(Sender: TObject);
    procedure TSXMLOptionsHide(Sender: TObject);
    procedure TSXMLOptionsShow(Sender: TObject);
    procedure FFilenameChange(Sender: TObject);
    procedure FDataSourceChange(Sender: TObject);
    procedure FBDataSourceClick(Sender: TObject);
    procedure TSSelectShow(Sender: TObject);
    procedure TSTaskShow(Sender: TObject);
    procedure TSJobHide(Sender: TObject);
    procedure FTablesChange(Sender: TObject; Item: TListItem;
      Change: TItemChange);
  type
    TTableName = class
    private
      FSourceName: string;
      function GetCaption(): string;
      function GetTableName(): string;
    public
      constructor Create(const ASourceName: string);
      property Caption: string read GetCaption;
      property TableName: string read GetTableName;
      property SourceName: string read FSourceName;
    end;
    TTableNames = class(TList)
    private
      function GetTableName(Index: Integer): TTableName; inline;
    public
      procedure Add(const SourceName: string);
      procedure Clear(); override;
      property TableName[Index: Integer]: TTableName read GetTableName; default;
    end;
  private
    Database: TSDatabase;
    FFields: array of TComboBox_Ext;
    FLReferrers: array of TLabel;
    FSourceFields: array of TEdit;
    Import: TTImport;
    ProgressInfos: TTool.TProgressInfos;
    TableNames: TTableNames;
    WantedNodeExpand: TTreeNode;
    procedure CheckActivePageChange(const ActivePageIndex: Integer);
    procedure ClearTSFields(Sender: TObject);
    procedure FormSessionEvent(const Event: TSSession.TEvent);
    function GetDataSource(): Boolean;
    function GetFilename(): Boolean;
    procedure InitTSFields(Sender: TObject);
    function InitTSSelect(): Boolean;
    procedure OnError(const Sender: TObject; const Error: TTool.TError; const Item: TTool.TItem; const ShowRetry: Boolean; var Success: TDataAction);
    procedure OnExecuted(const ASuccess: Boolean);
    procedure OnUpdate(const AProgressInfos: TTool.TProgressInfos);
    procedure CMChangePreferences(var Message: TMessage); message CM_CHANGEPREFERENCES;
    procedure CMExecutedDone(var Message: TMessage); message CM_EXECUTIONDONE;
    procedure CMPostAfterExecuteSQL(var Message: TMessage); message CM_POST_AFTEREXECUTESQL;
    procedure CMSysFontChanged(var Message: TMessage); message CM_SYSFONTCHANGED;
    procedure CMUpdateProgressInfo(var Message: TMessage); message CM_UPDATEPROGRESSINFO;
  public
    CodePage: Cardinal;
    DialogType: (idtNormal, idtCreateJob, idtEditJob, idtExecuteJob);
    Filename: TFileName;
    ImportType: TPImportType;
    Job: TAJobImport;
    Session: TSSession;
    SObject: TSObject;
    Window: TForm;
    function Execute(): Boolean;
  end;

function DImport(): TDImport;

implementation {***************************************************************}

{$R *.dfm}

uses
  CommDlg, Dlgs, Math, StrUtils, RichEdit,
  SQLUtils, CSVUtils,
  fDLogin, fDODBC;

const
  ImportBufferSize = 1024 * 1024;
  STR_LEN = 128;

var
  FImport: TDImport;

function DImport(): TDImport;
begin
  if (not Assigned(FImport)) then
  begin
    Application.CreateForm(TDImport, FImport);
    FImport.Perform(CM_CHANGEPREFERENCES, 0, 0);
  end;

  Result := FImport;
end;

{ TDImport.TTableName *********************************************************}

constructor TDImport.TTableName.Create(const ASourceName: string);
begin
  inherited Create();

  FSourceName := ASourceName;
end;

function TDImport.TTableName.GetCaption(): string;
begin
  if (DImport.ImportType <> itExcelFile) then
    Result := SourceName
  else if (Pos('$', SourceName) = Length(SourceName)) then
    Result := LeftStr(SourceName, Length(SourceName) - 1)
  else
    Result := LeftStr(SourceName, Pos('$', SourceName) - 1) + ' (' + RightStr(SourceName, Length(SourceName) - Pos('$', SourceName)) + ')';
end;

function TDImport.TTableName.GetTableName(): string;
begin
  if (DImport.ImportType <> itExcelFile) then
    Result := SourceName
  else if (Pos('$', SourceName) = Length(SourceName)) then
    Result := LeftStr(SourceName, Length(SourceName) - 1)
  else
    Result := LeftStr(SourceName, Pos('$', SourceName) - 1) + '_' + RightStr(SourceName, Length(SourceName) - Pos('$', SourceName));

  Result := DImport.Session.ApplyIdentifierName(Result);
end;

{ TDImport.TTableNames ********************************************************}

procedure TDImport.TTableNames.Add(const SourceName: string);
begin
  inherited Add(TTableName.Create(SourceName));
end;

procedure TDImport.TTableNames.Clear();
var
  I: Integer;
begin
  for I := 0 to Count - 1 do
    TableName[I].Free();

  inherited;
end;

function TDImport.TTableNames.GetTableName(Index: Integer): TTableName;
begin
  Result := TTableName(Items[Index]);
end;

{ TDImport ********************************************************************}

procedure TDImport.CheckActivePageChange(const ActivePageIndex: Integer);
var
  I: Integer;
  NextActivePageIndex: Integer;
begin
  FBBack.Enabled := False;
  for I := 0 to PageControl.PageCount - 1 do
    FBBack.Enabled := FBBack.Enabled or PageControl.Pages[I].Enabled and (I < ActivePageIndex);

  NextActivePageIndex := -1;
  for I := PageControl.PageCount - 1 downto ActivePageIndex + 1 do
    if (PageControl.Pages[I].Enabled) then
      NextActivePageIndex := I;
  if (NextActivePageIndex >= 0) then
    for I := NextActivePageIndex + 1 to PageControl.PageCount - 1 do
      PageControl.Pages[I].Enabled := False;

  if (ActivePageIndex = TSTask.PageIndex) then
    FBForward.Caption := Preferences.LoadStr(230)
  else if (NextActivePageIndex < TSExecute.PageIndex) then
    FBForward.Caption := Preferences.LoadStr(229) + ' >'
  else
    FBForward.Caption := Preferences.LoadStr(174);

  FBForward.Enabled := FBForward.Visible and ((NextActivePageIndex >= 0) or (ActivePageIndex = TSTask.PageIndex));
  FBForward.Default := True;

  FBCancel.Caption := Preferences.LoadStr(30);
  FBCancel.Default := False;
end;

procedure TDImport.ClearTSFields(Sender: TObject);
var
  I: Integer;
begin
  for I := 0 to Length(FSourceFields) - 1 do FSourceFields[I].Free();
  for I := 0 to Length(FLReferrers) - 1 do FLReferrers[I].Free();
  for I := 0 to Length(FFields) - 1 do FFields[I].Free();
  SetLength(FSourceFields, 0);
  SetLength(FLReferrers, 0);
  SetLength(FFields, 0);
end;

procedure TDImport.CMChangePreferences(var Message: TMessage);
begin
  OpenDialog.EncodingLabel := Preferences.LoadStr(682) + ':';

  PSQLWait.Caption := Preferences.LoadStr(882);

  GBasics.Caption := Preferences.LoadStr(85);
  FLName.Caption := Preferences.LoadStr(35) + ':';
  FLImportType.Caption := Preferences.LoadStr(371) + ':';
  FSQLFile.Caption := Preferences.LoadStr(409);
  FTextFile.Caption := Preferences.LoadStr(410);
  FExcelFile.Caption := Preferences.LoadStr(801);
  FAccessFile.Caption := Preferences.LoadStr(695);
  FODBC.Caption := Preferences.LoadStr(607);
  FXMLFile.Caption := Preferences.LoadStr(454);
  FLFilename.Caption := Preferences.LoadStr(348) + ':';
  FLDataSource.Caption := Preferences.LoadStr(38) + ':';

  GSelect.Caption := ReplaceStr(Preferences.LoadStr(755), '&', '');

  GTables.Caption := Preferences.LoadStr(234);

  GCSVHow.Caption := Preferences.LoadStr(238);
  FLCSVHeadline.Caption := Preferences.LoadStr(393) + ':';
  FCSVHeadline.Caption := Preferences.LoadStr(398);
  FLDelimiter.Caption := Preferences.LoadStr(352) + ':';
  FDelimiterTab.Caption := Preferences.LoadStr(394);
  FDelimiterChar.Caption := Preferences.LoadStr(355) + ':';
  FLQuoteValues.Caption := Preferences.LoadStr(353) + ':';
  FQuoteNothing.Caption := Preferences.LoadStr(396);
  FQuoteStrings.Caption := Preferences.LoadStr(397) + ':';
  GCSVPreview.Caption := Preferences.LoadStr(423);

  GWhat.Caption := Preferences.LoadStr(227);
  FLWhat.Caption := Preferences.LoadStr(218) + ':';
  FStructure.Caption := Preferences.LoadStr(215);
  FData.Caption := Preferences.LoadStr(216);
  GStructure.Caption := Preferences.LoadStr(238);
  FLEngine.Caption := Preferences.LoadStr(110) + ':';
  FLCharset.Caption := Preferences.LoadStr(682) + ':';
  FLCollation.Caption := Preferences.LoadStr(702) + ':';
  FLRowFormat.Caption := Preferences.LoadStr(129) + ':';

  GXMLHow.Caption := Preferences.LoadStr(238);
  FLRecordTag.Caption := Preferences.LoadStr(124) + ':';

  GFields.Caption := Preferences.LoadStr(253);
  FLFields.Caption := Preferences.LoadStr(401) + ':';

  GImportType.Caption := Preferences.LoadStr(238);
  FLStmtType.Caption := Preferences.LoadStr(124) + ':';
  FInsert.Caption := LowerCase(Preferences.LoadStr(880)) + ' (INSERT)';
  FReplace.Caption := LowerCase(Preferences.LoadStr(416)) + ' (REPLACE)';
  FUpdate.Caption := LowerCase(Preferences.LoadStr(726)) + ' (UPDATE)';

  GProgress.Caption := Preferences.LoadStr(224);
  FLEntiered.Caption := Preferences.LoadStr(211) + ':';
  FLDone.Caption := Preferences.LoadStr(232) + ':';
  FLProgressTables.Caption := Preferences.LoadStr(234) + ':';
  FLProgressTime.Caption := ReplaceStr(Preferences.LoadStr(661), '&', '') + ':';
  FLErrors.Caption := Preferences.LoadStr(391) + ':';

  GTask.Caption := Preferences.LoadStr(661);
  FLStart.Caption := Preferences.LoadStr(817) + ':';
  FLExecution.Caption := Preferences.LoadStr(174) + ':';
  FSingle.Caption := Preferences.LoadStr(902);
  FDaily.Caption := Preferences.LoadStr(903);
  FWeekly.Caption := Preferences.LoadStr(904);
  FMonthly.Caption := Preferences.LoadStr(905);
  FLEnabled.Caption := Preferences.LoadStr(812) + ':';
  FEnabled.Caption := Preferences.LoadStr(529);

  GErrorMessages.Caption := Preferences.LoadStr(392);

  FBHelp.Caption := Preferences.LoadStr(167);
  FBBack.Caption := '< ' + Preferences.LoadStr(228);
  FBForward.Caption := Preferences.LoadStr(229) + ' >';
  FBCancel.Caption := Preferences.LoadStr(30);
end;

procedure TDImport.CMExecutedDone(var Message: TMessage);
var
  Success: Boolean;
begin
  Success := Boolean(Message.WParam);

  GProgress.Enabled := True;
  FLEntiered.Enabled := True;
  FLDone.Enabled := True;
  FLProgressRecords.Enabled := True;
  FEntieredRecords.Enabled := True;
  FDoneRecords.Enabled := True;
  FLProgressTime.Enabled := True;
  FEntieredTime.Enabled := True;
  FDoneTime.Enabled := True;
  FProgressBar.Enabled := True;

  FBBack.Enabled := True;
  FBForward.Enabled := False;

  FBCancel.Caption := Preferences.LoadStr(231);

  if (Success) then
    FBCancel.ModalResult := mrOk
  else
    FBCancel.ModalResult := mrCancel;

  if (Assigned(Import)) then
    FreeAndNil(Import);
end;

procedure TDImport.CMPostAfterExecuteSQL(var Message: TMessage);
begin
  if (((DialogType = idtNormal) or (DialogType in [idtEditJob, idtExecuteJob]) and InitTSSelect()) and Assigned(FSelect.Selected)) then
    SObject := TSObject(FSelect.Selected.Data);

  Message.Result := LRESULT(not Assigned(SObject) or SObject.Update());
  if (Boolean(Message.Result)) then
  begin
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
        InitTSFields(nil);
      CheckActivePageChange(PageControl.ActivePageIndex);
    end;
  end;
end;

procedure TDImport.CMSysFontChanged(var Message: TMessage);
begin
  inherited;

  FBFilename.Height := FFilename.Height;
  FBDataSource.Height := FFilename.Height;

  FDelimiter.Left := FDelimiterChar.Left + Sizer.Width + PageControl.Canvas.TextWidth(FDelimiterChar.Caption);
  FQuoteChar.Left := FQuoteStrings.Left + Sizer.Width + PageControl.Canvas.TextWidth(FQuoteStrings.Caption);
end;

procedure TDImport.CMUpdateProgressInfo(var Message: TMessage);
var
  Infos: TTool.PProgressInfos;
begin
  Infos := TTool.PProgressInfos(Message.LParam);

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
end;

function TDImport.Execute(): Boolean;
begin
  ModalResult := mrNone;
  PageControl.ActivePageIndex := -1;

  if ((ImportType in [itSQLFile, itTextFile, itAccessFile, itExcelFile, itXMLFile]) and (Filename = '') and (DialogType = idtNormal)) then
    if (not GetFilename()) then
      ModalResult := mrCancel;
  if ((ImportType in [itODBC]) and (DialogType = idtNormal)) then
    if (not GetDataSource()) then
      ModalResult := mrCancel;

  if (ModalResult = mrCancel) then
    Result := False
  else
    Result := ShowModal() = mrOk;
end;

procedure TDImport.FBBackClick(Sender: TObject);
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

procedure TDImport.FBCancelClick(Sender: TObject);
begin
  if (Assigned(Import) and not Import.Suspended) then
  begin
    Import.UserAbort.SetEvent();
    Import.WaitFor();
    FreeAndNil(Import);
  end;
end;

procedure TDImport.FBDataSourceClick(Sender: TObject);
begin
  DODBC.DataSource := FDataSource.Text;
  if (DODBC.Execute()) then
    FDataSource.Text := DODBC.DataSource;
end;

procedure TDImport.FBFilenameClick(Sender: TObject);
begin
  Filename := FFilename.Text;
  if (GetFilename()) then
    FFilename.Text := Filename;
end;

procedure TDImport.FBForwardClick(Sender: TObject);
var
  ActivePageIndex: Integer;
begin
  if (PageControl.ActivePageIndex = TSTask.PageIndex) then
    ModalResult := mrOk
  else
  begin
    if (PageControl.ActivePageIndex = TSJob.PageIndex) then
      TSJobHide(Sender);

    for ActivePageIndex := PageControl.ActivePageIndex + 1 to PageControl.PageCount - 1 do
      if (PageControl.Pages[ActivePageIndex].Enabled) then
      begin
        PageControl.ActivePageIndex := ActivePageIndex;
        Exit;
      end;
  end;
end;

procedure TDImport.FBHelpClick(Sender: TObject);
begin
  Application.HelpContext(HelpContext);
end;

procedure TDImport.FCSVKeyPress(Sender: TObject; var Key: Char);
begin
  FCSVPreviewUpdate(Sender);
end;

procedure TDImport.FCSVPreviewUpdate(Sender: TObject);
var
  I: Integer;
  Item: TListItem;
  Values: TSQLStrings;
begin
  if (Visible) then
  begin
    if (not FDelimiterTab.Checked and (FDelimiter.Text = '') or not FQuoteNothing.Checked and (FQuoteChar.Text = '')) then
    begin
      TSFields.Enabled := False;
      TSWhat.Enabled := False;
    end
    else
    begin
      TSFields.Enabled := (SObject is TSBaseTable);
      TSWhat.Enabled := not TSFields.Enabled;
    end;


    FCSVPreview.DisableAlign(); FCSVPreview.Items.BeginUpdate();
    FCSVPreview.Columns.Clear();
    FCSVPreview.Items.Clear();

    if (TSFields.Enabled or TSWhat.Enabled) then
    begin
      if (FDelimiterTab.Checked) then
        TTImportText(Import).Delimiter := #9
      else if (FDelimiter.Text = '') then
        TTImportText(Import).Delimiter := #0
      else
        TTImportText(Import).Delimiter := FDelimiter.Text[1];
      if (not FQuoteStrings.Checked) or (FQuoteChar.Text = '') then
        TTImportText(Import).Quoter := #0
      else
        TTImportText(Import).Quoter := FQuoteChar.Text[1];
      TTImportText(Import).UseHeadLine := FCSVHeadline.Checked;
      TTImportText(Import).Open();

      for I := 0 to TTImportText(Import).HeadlineNameCount - 1 do
        FCSVPreview.Columns.Add().Caption := TTImportText(Import).HeadlineNames[I];

      Item := nil;
      while ((FCSVPreview.Items.Count < 10) and TTImportText(Import).GetPreviewValues(Values)) do
        for I := 0 to Min(Length(Values), FCSVPreview.Columns.Count) - 1 do
          if (I = 0) then
          begin
            Item := FCSVPreview.Items.Add();
            Item.Caption := Values[I];
          end
          else
            Item.SubItems.Add(Values[I]);

      for I := 0 to FCSVPreview.Columns.Count - 1 do
        FCSVPreview.Column[I].AutoSize := True;

      TTImportText(Import).Close();
      SetLength(Values, 0);
    end;

    FCSVPreview.Items.EndUpdate(); FCSVPreview.EnableAlign();

    ClearTSFields(Sender);

    FTables.Items.BeginUpdate();
    FTables.Items.Clear();
    FTables.Items.EndUpdate();

    CheckActivePageChange(TSCSVOptions.PageIndex);
  end;
end;

procedure TDImport.FDataClick(Sender: TObject);
begin
  FStructure.Checked := FStructure.Checked or FData.Checked and not (SObject is TSBaseTable);

  TSFields.Enabled := FData.Checked and (SObject is TSBaseTable);
  TSExecute.Enabled := not TSFields.Enabled and FStructure.Checked;
  CheckActivePageChange(TSWhat.PageIndex);

  ClearTSFields(Sender);
end;

procedure TDImport.FDataKeyPress(Sender: TObject; var Key: Char);
begin
  FDataClick(Sender);
end;

procedure TDImport.FDataSourceChange(Sender: TObject);
var
  Index: Integer;
begin
  Filename := Trim(FDataSource.Text);

  if ((FName.Text = '') and (DialogType = idtCreateJob)) then
  begin
    if (Pos('.', Filename) = 0) then
      FName.Text := ExtractFileName(Filename)
    else
      FName.Text := Copy(ExtractFileName(Filename), 1, Length(ExtractFileName(Filename)) - Length(ExtractFileExt(Filename)));

    if (Assigned(Session.Account.JobByName(FName.Text))) then
    begin
      Index := 2;
      while (Assigned(Session.Account.JobByName(FName.Text + ' (' + IntToStr(Index) + ')'))) do
        Inc(Index);
      FName.Text := FName.Text + ' (' + IntToStr(Index) + ')';
    end;
  end;

  FJobOptionChange(Sender);
end;

procedure TDImport.FCharsetChange(Sender: TObject);
var
  I: Integer;
begin
  FCollation.Items.Clear();
  FCollation.Items.Add('');
  if (Assigned(Session.Collations)) then
    for I := 0 to Session.Collations.Count - 1 do
    begin
      if (lstrcmpi(PChar(Session.Collations[I].Charset.Name), PChar(FCharset.Text)) = 0) then
      begin
        FCollation.Items.Add(Session.Collations[I].Name);
        if (Session.Collations[I].Default) then
          FCollation.ItemIndex := FCollation.Items.Count - 1;
      end;
    end;
  FCollation.Enabled := FCharset.Text <> ''; FLCollation.Enabled := FCollation.Enabled;
end;

procedure TDImport.FDelimiterClick(Sender: TObject);
begin
  FDelimiter.Enabled := FDelimiterChar.Checked;
  FCSVPreviewUpdate(Sender);
end;

procedure TDImport.FDelimiterKeyPress(Sender: TObject; var Key: Char);
begin
  FDelimiterClick(Sender);
end;

procedure TDImport.FFieldExit(Sender: TObject);
var
  I: Integer;
begin
  for I := 0 to Length(FFields) - 1 do
    if ((Sender is TComboBox_Ext) and (FFields[I] <> Sender) and (FFields[I].ItemIndex = TComboBox_Ext(Sender).ItemIndex)) then
      FFields[I].ItemIndex := 0;
end;

procedure TDImport.FFilenameChange(Sender: TObject);
var
  Index: Integer;
begin
  Filename := Trim(FFilename.Text);

  if (FName.Text = '') then
  begin
    if (Pos('.', Filename) = 0) then
      FName.Text := ExtractFileName(Filename)
    else
      FName.Text := Copy(ExtractFileName(Filename), 1, Length(ExtractFileName(Filename)) - Length(ExtractFileExt(Filename)));

    if (Assigned(Session.Account.JobByName(FName.Text))) then
    begin
      Index := 2;
      while (Assigned(Session.Account.JobByName(FName.Text + ' (' + IntToStr(Index) + ')'))) do
        Inc(Index);
      FName.Text := FName.Text + ' (' + IntToStr(Index) + ')';
    end;
  end;

  FJobOptionChange(Sender);
end;

procedure TDImport.FImportTypeChange(Sender: TObject);
begin
  FFilename.Text := '';

  FJobOptionChange(Sender);
end;

procedure TDImport.FJobOptionChange(Sender: TObject);
var
  Enabled: Boolean;
begin
  if (FSQLFile.Checked) then
    ImportType := itSQLFile
  else if (FTextFile.Checked) then
    ImportType := itTextFile
  else if (FExcelFile.Checked) then
    ImportType := itExcelFile
  else if (FAccessFile.Checked) then
    ImportType := itAccessFile
  else if (FODBC.Checked) then
    ImportType := itODBC
  else if (FXMLFile.Checked) then
    ImportType := itXMLFile
  else
    ImportType := itUnknown;

  FFilename.Visible := ImportType in [itSQLFile, itTextFile, itExcelFile, itAccessFile, itXMLFile];
  FLFilename.Visible := FFilename.Visible;
  FBFilename.Visible := FFilename.Visible;
  FDataSource.Visible := ImportType in [itODBC];
  FLDataSource.Visible := FDataSource.Visible;
  FBDataSource.Visible := FDataSource.Visible;


  Enabled := ValidJobName(Trim(FName.Text))
    and ((DialogType <> idtCreateJob) or not Assigned(Session.Account.JobByName(Trim(FName.Text))))
    and ((DialogType <> idtEditJob) or (Session.Account.JobByName(Trim(FName.Text)) = Job))
    and (not FFilename.Visible or (DirectoryExists(ExtractFilePath(FFilename.Text)) and (ExtractFileName(FFilename.Text) <> '')))
    and (not FDataSource.Visible or (FDataSource.Text <> ''));

  TSTables.Enabled := (ImportType in [itExcelFile, itAccessFile, itODBC]) and Enabled;
  TSSelect.Enabled := not TSTables.Enabled and Enabled;

  CheckActivePageChange(TSJob.PageIndex);
end;

procedure TDImport.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  PageControl.ActivePage := nil;
end;

procedure TDImport.FormCreate(Sender: TObject);
begin
  FSelect.Images := Preferences.SmallImages;
  FTables.SmallImages := Preferences.SmallImages;

  FCSVHeadline.Checked := Preferences.Import.CSV.Headline;
  FDelimiterTab.Checked := Preferences.Import.CSV.DelimiterType = dtTab;
  FDelimiterChar.Checked := Preferences.Import.CSV.DelimiterType = dtChar;
  FDelimiter.Text := Preferences.Import.CSV.Delimiter;
  FQuoteNothing.Checked := Preferences.Import.CSV.Quote = qtNothing;
  FQuoteStrings.Checked := Preferences.Import.CSV.Quote = qtStrings;
  FQuoteChar.Text := Preferences.Import.CSV.QuoteChar;
  case (Preferences.Import.ImportStmt) of
    isReplace: FReplace.Checked := True;
    isUpdate: FUpdate.Checked := True;
    else FInsert.Checked := True;
  end;

  SendMessage(FErrorMessages.Handle, EM_SETTEXTMODE, TM_PLAINTEXT, 0);
  SendMessage(FErrorMessages.Handle, EM_SETWORDBREAKPROC, 0, LPARAM(@EditWordBreakProc));

  PageControl.ActivePage := nil; // Make sure, not ___OnShowPage will be executed
  FDataSource.Text := '';
  FFilename.Text := '';
end;

procedure TDImport.FormHide(Sender: TObject);
var
  Hour, Min, Sec, MSec: Word;
  Year, Month, Day: Word;
  I: Integer;
  J: Integer;
  PImport: TPImport;
begin
  Session.UnRegisterEventProc(FormSessionEvent);

  if (Assigned(Import)) then
    Import.Free();

  if (ModalResult = mrOk) then
  begin
    if (DialogType in [idtCreateJob, idtEditJob]) then
      PImport := TAJobImport.Create(Session.Account.Jobs, Trim(FName.Text))
    else
      PImport := Preferences.Import;

    case (ImportType) of
      itTextFile:
        begin
          PImport.CSV.Headline := FCSVHeadline.Checked;
          if (FDelimiterTab.Checked) then
            PImport.CSV.DelimiterType := dtTab
          else if (FDelimiterChar.Checked) then
            PImport.CSV.DelimiterType := dtChar;
          PImport.CSV.Delimiter := FDelimiter.Text;
          PImport.CSV.QuoteChar := FQuoteChar.Text;
          if (FQuoteNothing.Checked) then
            PImport.CSV.Quote := qtNothing
          else
            PImport.CSV.Quote := qtStrings;
        end;
      itODBC:
        begin
          PImport.Structure := FStructure.Checked;
          PImport.Data := FData.Checked;
        end;
    end;
    if (FReplace.Checked) then
      PImport.ImportStmt := isReplace
    else if (FUpdate.Checked) then
      PImport.ImportStmt := isUpdate
    else
      PImport.ImportStmt := isInsert;

    if (DialogType in [idtCreateJob, idtEditJob]) then
    begin
      if (Assigned(FSelect.Selected)) then
        if (not Assigned(FSelect.Selected.Parent)) then
          TAJobImport(PImport).JobObject.ObjectType := jotServer
        else if (TObject(FSelect.Selected.Data) is TSDatabase) then
        begin
          TAJobImport(PImport).JobObject.ObjectType := jotDatabase;
          TAJobImport(PImport).JobObject.Name := TSDatabase(FSelect.Selected.Data).Name;
        end
        else if (TObject(FSelect.Selected.Data) is TSDBObject) then
        begin
          if (TObject(FSelect.Selected.Data) is TSTable) then
            TAJobImport(PImport).JobObject.ObjectType := jotTable;
          TAJobImport(PImport).JobObject.Name := TSDBObject(FSelect.Selected.Data).Name;
          TAJobImport(PImport).JobObject.DatabaseName := TSDBObject(FSelect.Selected.Data).Database.Name;
        end;
      TAJobImport(PImport).CodePage := CodePage;
      TAJobImport(PImport).ImportType := ImportType;
      TAJobImport(PImport).Filename := FFilename.Text;
      TAJobImport(PImport).ODBC.DataSource := FDataSource.Text;

      SetLength(TAJobImport(PImport).SourceObjects, 0);
      case (ImportType) of
        itAccessFile,
        itExcelFile,
        itODBC:
          for I := 0 to FTables.Items.Count - 1 do
            if (FTables.Items[I].Selected) then
            begin
              SetLength(TAJobImport(PImport).SourceObjects, Length(TAJobImport(PImport).SourceObjects) + 1);
              TAJobImport(PImport).SourceObjects[Length(TAJobImport(PImport).SourceObjects) - 1].Name := TTableName(FTables.Items[I].Data).SourceName;
            end;
        itXMLFile:
          begin
            SetLength(TAJobImport(PImport).SourceObjects, Length(TAJobImport(PImport).SourceObjects) + 1);
            TAJobImport(PImport).SourceObjects[Length(TAJobImport(PImport).SourceObjects) - 1].Name := FRecordTag.Text;
          end;
      end;

      SetLength(TAJobImport(PImport).FieldMappings, 0);
      if (SObject is TSTable) then
        for I := 0 to TSTable(SObject).Fields.Count - 1 do
          for J := 0 to Length(FFields) - 1 do
            if ((FSourceFields[J].Text <> '') and (FFields[J].ItemIndex = I + 1)) then
            begin
              SetLength(TAJobImport(PImport).FieldMappings, Length(TAJobImport(PImport).FieldMappings) + 1);
              TAJobImport(PImport).FieldMappings[Length(TAJobImport(PImport).FieldMappings) - 1].Name := TSTable(SObject).Fields[I].Name;
              TAJobImport(PImport).FieldMappings[Length(TAJobImport(PImport).FieldMappings) - 1].SourceName := FSourceFields[J].Text;
            end;

      DecodeDate(FStartDate.Date, Year, Month, Day);
      DecodeTime(FStartTime.Time, Hour, Min, Sec, MSec);
      TAJobImport(PImport).Start := EncodeDate(Year, Month, Day) + EncodeTime(Hour, Min, Sec, MSec);
      if (FDaily.Checked) then
        TAJobImport(PImport).TriggerType := ttDaily
      else if (FWeekly.Checked) then
        TAJobImport(PImport).TriggerType := ttWeekly
      else if (FMonthly.Checked) then
        TAJobImport(PImport).TriggerType := ttMonthly
      else
        TAJobImport(PImport).TriggerType := ttSingle;
      TAJobImport(PImport).Enabled := FEnabled.Checked;

      if (DialogType = idtCreateJob) then
        Session.Account.Jobs.AddJob(PImport)
      else if (DialogType = idtEditJob) then
        Session.Account.Jobs.UpdateJob(Job, PImport);
      PImport.Free();
    end;
  end;

  FDataSource.Text := '';
  FFilename.Text := '';

  FSelect.Items.BeginUpdate();
  FSelect.Items.Clear();
  FSelect.Items.EndUpdate();

  FTables.Items.BeginUpdate();
  FTables.Items.Clear();
  FTables.Items.EndUpdate();
  ClearTSFields(Sender);

  FCSVPreview.Items.BeginUpdate();
  FCSVPreview.Items.Clear();
  FCSVPreview.Columns.Clear();
  FCSVPreview.Items.EndUpdate();

  TableNames.Free();
end;

procedure TDImport.FormSessionEvent(const Event: TSSession.TEvent);
begin
  if (Event.EventType = ceAfterExecuteSQL) then
    PostMessage(Handle, CM_POST_AFTEREXECUTESQL, 0, 0);
end;

procedure TDImport.FormShow(Sender: TObject);
var
  I: Integer;
  J: Integer;
  Node: TTreeNode;
begin
  Session.RegisterEventProc(FormSessionEvent);

  Import := nil;
  TableNames := TTableNames.Create();

  ModalResult := mrNone;
  if (DialogType = idtCreateJob) then
    Caption := Preferences.LoadStr(897)
  else if (DialogType = idtEditJob) then
    Caption := Preferences.LoadStr(842, Job.Name)
  else if (DialogType = idtExecuteJob) then
    if (Job.ImportType <> itODBC) then
      Caption := Preferences.LoadStr(386) + ' ' + ExtractFileName(Job.Filename)
    else
      Caption := Preferences.LoadStr(386) + ' ' + ExtractFileName(Job.ODBC.DataSource)
  else if (ExtractFileName(Filename) = '') then
    Caption := Preferences.LoadStr(386)
  else
    Caption := Preferences.LoadStr(386) + ' ' + ExtractFileName(Filename);

  if (DialogType = idtCreateJob) then
    HelpContext := 1148
  else if (DialogType = idtEditJob) then
    HelpContext := 1140
  else if (DialogType = idtExecuteJob) then
    HelpContext := -1
  else
    case (ImportType) of
      itSQLFile: HelpContext := 1010;
      itTextFile: HelpContext := 1133;
      itAccessFile: HelpContext := 1013;
      itExcelFile: HelpContext := 1106;
      itODBC: HelpContext := 1012;
      itXMLFile: HelpContext := 1132;
      else HelpContext := -1;
    end;
  FBHelp.Visible := HelpContext >= 0;

  FEngine.Clear();
  if (Session.Engines.Count = 0) then
    FEngine.Style := csDropDown
  else
  begin
    FEngine.Style := csDropDownList;
    for I := 0 to Session.Engines.Count - 1 do
      FEngine.Items.Add(Session.Engines.Engine[I].Name);
  end;

  FCharset.Items.Clear();
  if (Session.Charsets.Count = 0) then
    FCharset.Style := csDropDown
  else
  begin
    FCharset.Style := csDropDownList;
    for I := 0 to Session.Charsets.Count - 1 do
      FCharset.Items.Add(Session.Charsets.Charset[I].Name);
  end;
  FCharset.Visible := Session.ServerVersion >= 40101; FLCharset.Visible := FCharset.Visible;
  FCollation.Visible := Session.ServerVersion >= 40101; FLCollation.Visible := FCollation.Visible;

  if (DialogType = idtCreateJob) then
  begin
    Node := FSelect.Items.Add(nil, Session.Caption);
    Node.ImageIndex := iiServer;
    Node.HasChildren := True;

    FName.Text := '';
    FSQLFile.Checked := False;
    FTextFile.Checked := False;
    FExcelFile.Checked := False;
    FAccessFile.Checked := False;
    FODBC.Checked := False;
    FXMLFile.Checked := False;
    FFilename.Text := '';
    FDataSource.Text := '';
    FJobOptionChange(Sender);

    FStructure.Checked := Preferences.Import.Structure;
    FData.Checked := Preferences.Import.Data;
    FEngine.ItemIndex := FEngine.Items.IndexOf(Preferences.Import.Engine);
    FCharset.ItemIndex := FCharset.Items.IndexOf(Preferences.Import.Charset);
    FRowFormat.ItemIndex := Preferences.Import.RowType;

    FStartDate.Date := Now() + 1; FStartTime.Time := 0;
    FSingle.Checked := True;
    FEnabled.Checked := True;
  end
  else if (DialogType in [idtEditJob, idtExecuteJob]) then
  begin
    Node := FSelect.Items.Add(nil, Session.Caption);
    Node.ImageIndex := iiServer;
    Node.HasChildren := True;

    FName.Text := Job.Name;
    case (Job.ImportType) of
      itSQLFile: FSQLFile.Checked := True;
      itTextFile: FTextFile.Checked := True;
      itExcelFile: FExcelFile.Checked := True;
      itAccessFile: FAccessFile.Checked := True;
      itODBC: FODBC.Checked := True;
      itXMLFile: FXMLFile.Checked := True;
    end;
    FDataSource.Text := Job.ODBC.DataSource;
    FFilename.Text := Job.Filename;
    FJobOptionChange(Sender);
    DODBC.DataSource := Job.ODBC.DataSource;
    DODBC.Username := Job.ODBC.Username;
    DODBC.Password := Job.ODBC.Password;

    FStructure.Checked := Job.Structure;
    FData.Checked := Job.Data;
    FEngine.ItemIndex := FEngine.Items.IndexOf(Job.Engine);
    FCharset.ItemIndex := FCharset.Items.IndexOf(Job.Charset);
    FRowFormat.ItemIndex := Job.RowType;

    case (Job.JobObject.ObjectType) of
      jotServer: Database := nil;
      jotDatabase: Database := Session.DatabaseByName(Job.JobObject.Name);
      jotTable,
      jotProcedure,
      jotFunction,
      jotTrigger,
      jotEvent: Database := Session.DatabaseByName(Job.JobObject.DatabaseName);
    end;
    case (Job.JobObject.ObjectType) of
      jotTable:
        SObject := Database.TableByName(Job.JobObject.Name);
    end;

    TSJobHide(Sender);
    TSTablesShow(nil);
    for I := 0 to FTables.Items.Count - 1 do
      for J := 0 to Length(Job.SourceObjects) - 1 do
      begin // why is this needed here??? (Without it, it will be executed too many times
        if (TTableName(FTables.Items[I].Data).SourceName = Job.SourceObjects[J].Name) then
          FTables.Items[I].Selected := True;
      end;
    InitTSFields(nil);

    FStartDate.Date := Job.Start; FStartTime.Time := Job.Start;
    case (Job.TriggerType) of
      ttDaily: FDaily.Checked := True;
      ttWeekly: FWeekly.Checked := True;
      ttMonthly: FMonthly.Checked := True;
      else FSingle.Checked := True;
    end;
    FEnabled.Checked := Job.Enabled;
  end
  else
  begin
    if (SObject is TSTable) then
      Database := TSDBObject(SObject).Database
    else
    begin
      if (DialogType <> idtNormal) then
        Database := nil
      else if (SObject is TSDatabase) then
        Database := TSDatabase(SObject)
      else if (SObject is TSDBObject) then
        Database := TSDBObject(SObject).Database
      else
        Database := nil;
    end;

    if (ImportType in [itTextFile, itODBC, itAccessFile, itExcelFile]) then
    begin
      FStructure.Checked := Preferences.Import.Structure and not (SObject is TSBaseTable);
      FData.Checked := Preferences.Import.Data and (FStructure.Checked or (SObject is TSBaseTable));
      FEngine.ItemIndex := FEngine.Items.IndexOf(Preferences.Import.Engine);
      FCharset.ItemIndex := FCharset.Items.IndexOf(Preferences.Import.Charset);
      FRowFormat.ItemIndex := Preferences.Import.RowType;
    end;

    TSJobHide(Sender);
  end;

  if (FEngine.ItemIndex < 0) then
    FEngine.ItemIndex := FEngine.Items.IndexOf(Session.Engines.DefaultEngine.Name);
  if (FCharset.ItemIndex < 0) then
    if (SObject is TSDatabase) then
      FCharset.ItemIndex := FCharset.Items.IndexOf(TSDatabase(SObject).DefaultCharset)
    else if (SObject is TSDBObject) then
      FCharset.ItemIndex := FCharset.Items.IndexOf(TSDBObject(SObject).Database.DefaultCharset)
    else if (Session.ServerVersion < 40101) then
      FCharset.ItemIndex := FCharset.Items.IndexOf(Session.DefaultCharset)
    else
      FCharset.ItemIndex := FCharset.Items.IndexOf('utf8');
  FCharsetChange(Sender);

  TSJob.Enabled := DialogType in [idtCreateJob, idtEditJob];
  TSTables.Enabled := (DialogType in [idtNormal, idtCreateJob, idtEditJob]) and (ImportType in [itAccessFile, itExcelFile, itODBC]);
  TSSelect.Enabled := DialogType in [idtEditJob];
  TSCSVOptions.Enabled := (ImportType in [itTextFile]);
  TSXMLOptions.Enabled := (SObject is TSTable) and (ImportType in [itXMLFile]);
  TSWhat.Enabled := False;
  TSFields.Enabled := False;
  TSStmtType.Enabled := False;
  TSTask.Enabled := False;
  TSExecute.Enabled := (DialogType in [idtExecuteJob]) or (ImportType in [itSQLFile]);

  if (TSFields.Enabled) then
    InitTSFields(Sender);

  for I := 0 to PageControl.PageCount - 1 do
    if ((PageControl.ActivePageIndex < 0) and PageControl.Pages[I].Enabled) then
      PageControl.ActivePageIndex := I;
  CheckActivePageChange(PageControl.ActivePageIndex);

  if (DialogType in [idtCreateJob, idtEditJob]) then
  begin
    PageControl.Visible := Session.Databases.Update() and Boolean(Perform(CM_POST_AFTEREXECUTESQL, 0, 0));
    if (PageControl.Visible) then
      FSelect.Items.GetFirstNode().Expand(False)
    else
      WantedNodeExpand := FSelect.Items.GetFirstNode();
  end
  else
    PageControl.Visible := Boolean(Perform(CM_POST_AFTEREXECUTESQL, 0, 0));
  PSQLWait.Visible := not PageControl.Visible;

  FBBack.Visible := (DialogType in [idtNormal, idtCreateJob, idtEditJob]) and not (ImportType in [itSQLFile]);
  FBForward.Visible := FBBack.Visible;
  FBCancel.ModalResult := mrCancel;
  FBCancel.Caption := Preferences.LoadStr(30);

  CheckActivePageChange(PageControl.ActivePageIndex);
  if (PageControl.Visible and TSJob.Visible) then
    ActiveControl := FName
  else if (FBForward.Visible and FBForward.Enabled) then
    ActiveControl := FBForward
  else
    ActiveControl := FBCancel;
end;

procedure TDImport.FQuoteClick(Sender: TObject);
begin
  FQuoteChar.Enabled := not FQuoteNothing.Checked;

  FCSVPreviewUpdate(Sender);
end;

procedure TDImport.FQuoteKeyPress(Sender: TObject; var Key: Char);
begin
  FQuoteClick(Sender);
end;

procedure TDImport.FSelectChange(Sender: TObject; Node: TTreeNode);
begin
  if (not Assigned(FSelect.Selected)) then
    SObject := nil
  else
    SObject := TSObject(FSelect.Selected.Data);

  TSCSVOptions.Enabled := (ImportType in [itTextFile]);
  TSXMLOptions.Enabled := (SObject is TSTable) and (ImportType in [itXMLFile]);
  TSWhat.Enabled := not Assigned(SObject) and (ImportType = itSQLFile) or (SObject is TSDatabase) or (SObject is TSTable) and (ImportType <> itSQLFile) and (FTables.SelCount = 1);
  TSTask.Enabled := (ImportType = itSQLFile);

  CheckActivePageChange(TSSelect.PageIndex);
end;

procedure TDImport.FSelectExpanding(Sender: TObject; Node: TTreeNode;
  var AllowExpansion: Boolean);
var
  Database: TSDatabase;
  I: Integer;
  NewNode: TTreeNode;
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
            if (not Session.Update()) then
              WantedNodeExpand := Node
            else
            begin
              for I := 0 to Session.Databases.Count - 1 do
                if ((Session.Databases.NameCmp(Session.Databases[I].Name, 'mysql') <> 0) and not (Session.Databases[I] is TSSystemDatabase)) then
                begin
                  NewNode := TreeView.Items.AddChild(Node, Session.Databases[I].Name);
                  NewNode.ImageIndex := iiDatabase;
                  NewNode.Data := Session.Databases[I];
                  NewNode.HasChildren := True;
                end;
              Node.HasChildren := Assigned(Node.getFirstChild());
            end;
          end;
        iiDatabase:
          begin
            Database := Session.DatabaseByName(Node.Text);
            if ((not Database.Tables.Update() or not Session.Update(Database.Tables))) then
              WantedNodeExpand := Node
            else
            begin
              for I := 0 to Database.Tables.Count - 1 do
              begin
                NewNode := TreeView.Items.AddChild(Node, Database.Tables[I].Name);
                if (Database.Tables[I] is TSBaseTable) then
                  NewNode.ImageIndex := iiBaseTable
                else
                  NewNode.ImageIndex := iiView;
                NewNode.Data := Database.Tables[I];
              end;
              Node.HasChildren := Assigned(Node.getFirstChild());
            end;
          end;
      end;
    end;
end;

procedure TDImport.FSelectGetImageIndex(Sender: TObject; Node: TTreeNode);
begin
  Node.SelectedIndex := Node.ImageIndex;
end;

procedure TDImport.FStmtTypeClick(Sender: TObject);
begin
  TSExecute.Enabled := (DialogType = idtNormal) and (FInsert.Checked and FInsert.Enabled or FReplace.Checked and FReplace.Enabled or FUpdate.Checked and FUpdate.Enabled);
  TSTask.Enabled := (DialogType <> idtNormal) and (FInsert.Checked and FInsert.Enabled or FReplace.Checked and FReplace.Enabled or FUpdate.Checked and FUpdate.Enabled);
  CheckActivePageChange(TSStmtType.PageIndex);
end;

procedure TDImport.FStmtTypeKeyPress(Sender: TObject; var Key: Char);
begin
  FStmtTypeClick(Sender);
end;

procedure TDImport.FStructureClick(Sender: TObject);
begin
  FData.Checked := FData.Checked and (SObject is TSTable) or FStructure.Checked;

  TSFields.Enabled := FData.Checked and (SObject is TSTable);
  TSExecute.Enabled := not TSFields.Enabled and FStructure.Checked;
  CheckActivePageChange(TSWhat.PageIndex);

  ClearTSFields(Sender);
end;

procedure TDImport.FStructureKeyPress(Sender: TObject; var Key: Char);
begin
  FStructureClick(Sender);
end;

procedure TDImport.FTablesChange(Sender: TObject; Item: TListItem;
  Change: TItemChange);
begin
  TSSelect.Enabled := (DialogType in [idtCreateJob, idtEditJob, idtExecuteJob]) and Assigned(FTables.Selected);
  TSCSVOptions.Enabled := (DialogType in [idtNormal]) and (ImportType in [itTextFile]);
  TSXMLOptions.Enabled := (DialogType in [idtNormal]) and (SObject is TSTable) and (ImportType in [itXMLFile]);
  TSWhat.Enabled := (DialogType in [idtNormal]) and not Assigned(SObject) and (ImportType = itSQLFile) or (SObject is TSDatabase) or (SObject is TSTable) and (ImportType <> itSQLFile) and (FTables.SelCount = 1);
  TSTask.Enabled := (DialogType in [idtCreateJob, idtEditJob]) and (ImportType = itSQLFile);

  CheckActivePageChange(TSTables.PageIndex);
end;

procedure TDImport.FTablesDblClick(Sender: TObject);
begin
  FTablesSelectItem(Sender, FTables.Selected, Assigned(FTables.Selected));
  if (FBForward.Enabled) then
    FBForward.Click();
end;

procedure TDImport.FTablesSelectItem(Sender: TObject; Item: TListItem;
  Selected: Boolean);
begin
  TSWhat.Enabled := (FTables.SelCount > 0) and (ImportType in [itTextFile, itAccessFile, itExcelFile, itODBC, itXMLFile]) and not (SObject is TSBaseTable);
  TSFields.Enabled := (FTables.SelCount > 0) and not TSWhat.Enabled;
  CheckActivePageChange(TSTables.PageIndex);
end;

function TDImport.GetDataSource(): Boolean;
begin
  Result := DODBC.Execute();
end;

function TDImport.GetFilename(): Boolean;
begin
  OpenDialog.Title := ReplaceStr(Preferences.LoadStr(581), '&', '');
  OpenDialog.InitialDir := Preferences.Path;
  OpenDialog.Filter := '';

  case (ImportType) of
    itSQLFile:
      begin
        OpenDialog.Filter := FilterDescription('sql') + ' (*.sql)|*.sql';
        OpenDialog.DefaultExt := 'sql';
        OpenDialog.Encodings.Text := EncodingCaptions();
      end;
    itTextFile:
      begin
        OpenDialog.Filter := FilterDescription('txt') + ' (*.txt;*.csv;*.tab;*.asc)|*.txt;*.csv;*.tab;*.asc';
        OpenDialog.DefaultExt := 'csv';
        OpenDialog.Encodings.Text := EncodingCaptions();
      end;
    itExcelFile:
      begin
        if (odExcel2007 in ODBCDrivers) then
          OpenDialog.Filter := FilterDescription('xls') + ' (*.xls;*.xlsm;*.xlsb;*.xlsx)|*.xls;*.xlsm;*.xlsb;*.xlsx'
        else
          OpenDialog.Filter := FilterDescription('xls') + ' (*.xls)|*.xls';
        OpenDialog.DefaultExt := 'xls';
        OpenDialog.Encodings.Clear();
      end;
    itAccessFile:
      begin
        if (odAccess2007 in ODBCDrivers) then
          OpenDialog.Filter := FilterDescription('mdb') + ' (*.mdb;*.accdb)|*.mdb;*.accdb'
        else
          OpenDialog.Filter := FilterDescription('mdb') + ' (*.mdb)|*.mdb';
        OpenDialog.DefaultExt := 'mdb';
        OpenDialog.Encodings.Clear();
      end;
    itXMLFile:
      begin
        OpenDialog.Filter := FilterDescription('xml') + ' (*.xml)|*.xml';
        OpenDialog.DefaultExt := 'xml';
        OpenDialog.Encodings.Clear();
      end;
  end;
  OpenDialog.Filter := OpenDialog.Filter + '|' + FilterDescription('*') + ' (*.*)|*.*';

  OpenDialog.FileName := '';
  OpenDialog.EncodingIndex := OpenDialog.Encodings.IndexOf(CodePageToEncoding(CP_ACP));

  Result := OpenDialog.Execute();
  if (Result) then
  begin
    Preferences.Path := ExtractFilePath(OpenDialog.FileName);

    if ((OpenDialog.EncodingIndex < 0) or (OpenDialog.Encodings.Count = 0)) then
      CodePage := GetACP()
    else
      CodePage := EncodingToCodePage(OpenDialog.Encodings[OpenDialog.EncodingIndex]);
    Filename := OpenDialog.FileName;
  end;
end;

procedure TDImport.InitTSFields(Sender: TObject);
var
  FieldNames: TStringList;
  I: Integer;
  J: Integer;
begin
  if (TSFields.Enabled) then
  begin
    ClearTSFields(Sender);

    FieldNames := TStringList.Create();

    if (ImportType in [itTextFile]) then
    begin
      FLSourceFields.Caption := Preferences.LoadStr(400) + ':';

      for I := 0 to FCSVPreview.Columns.Count - 1 do
        FieldNames.Add(FCSVPreview.Column[I].Caption);
    end
    else if ((ImportType in [itExcelFile, itAccessFile, itODBC]) and (SObject is TSBaseTable)) then
    begin
      if (ImportType = itODBC) then
        FLSourceFields.Caption := Preferences.LoadStr(610) + ':'
      else
        FLSourceFields.Caption := Preferences.LoadStr(400) + ':';

      TTImportODBC(Import).GetFieldNames(TTableName(FTables.Selected.Data).SourceName, FieldNames);
    end
    else if ((ImportType in [itXMLFile]) and (SObject is TSBaseTable)) then
    begin
      FLSourceFields.Caption := Preferences.LoadStr(400) + ':';

      for I := 0 to TSBaseTable(SObject).Fields.Count - 1 do
        FieldNames.Add(TSBaseTable(SObject).Fields[I].Name);
    end;

    SetLength(FSourceFields, FieldNames.Count);
    SetLength(FLReferrers, Length(FSourceFields));
    SetLength(FFields, Length(FSourceFields));

    if (SObject is TSBaseTable) then
    begin
      ScrollBox.Visible := False;
      ScrollBox.AutoScroll := False;

      for I := 0 to Length(FSourceFields) - 1 do
      begin
        FSourceFields[I] := TEdit.Create(ScrollBox);
        FSourceFields[I].Parent := ScrollBox;
        FSourceFields[I].Left := FSourceField1.Left;
        FSourceFields[I].Top := FSourceField1.Top + I * (FSourceField2.Top - FSourceField1.Top);
        FSourceFields[I].Width := FSourceField1.Width;
        FSourceFields[I].Height := FSourceField1.Height;
        FSourceFields[I].Enabled := ImportType = itXMLFile;
        FSourceFields[I].Text := FieldNames[I];
        FSourceFields[I].OnChange := FSourceField1.OnChange;

        FLReferrers[I] := TLabel.Create(ScrollBox);
        FLReferrers[I].Parent := ScrollBox;
        FLReferrers[I].Left := FLReferrer1.Left;
        FLReferrers[I].Top := FLReferrer1.Top + I * (FSourceField2.Top - FSourceField1.Top);
        FLReferrers[I].Width := FLReferrer1.Width;
        FLReferrers[I].Height := FLReferrer1.Height;
        FLReferrers[I].Caption := FLReferrer1.Caption;

        FFields[I] := TComboBox_Ext.Create(ScrollBox);
        FFields[I].Parent := ScrollBox;
        FFields[I].Left := FField1.Left;
        FFields[I].Top := FField1.Top + I * (FSourceField2.Top - FSourceField1.Top);
        FFields[I].Width := FField1.Width;
        FFields[I].Height := FField1.Height;
        FFields[I].Style := FField1.Style;
        FFields[I].Enabled := not FSourceFields[I].Enabled;
        if (I = 0) then
        begin
          FFields[I].Items.Add('');
          for J := 0 to TSBaseTable(SObject).Fields.Count - 1 do
            FFields[I].Items.Add(TSBaseTable(SObject).Fields.Field[J].Name);
        end
        else
          FFields[I].Items.Text := FFields[0].Items.Text;
        if ((ImportType in [itTextFile]) and FCSVHeadline.Checked or (ImportType in [itExcelFile, itAccessFile, itODBC])) then
          FFields[I].ItemIndex := FFields[I].Items.IndexOf(FSourceFields[I].Text)
        else
          FFields[I].ItemIndex := I + 1;
        FFields[I].OnChange := FField1.OnChange;
        FFields[I].OnExit := FField1.OnExit;
      end;

      ScrollBox.AutoScroll := True;
      ScrollBox.Visible := True;

      TSFieldsChange(Sender);
      FFieldExit(Sender);
    end;

    FieldNames.Free();
  end;
end;

function TDImport.InitTSSelect(): Boolean;
var
  Database: TSDatabase;
  J: Integer;
  K: Integer;
  Nodes: TList;
begin
  Result := True;
  if (FSelect.Items[0].Count = 0) then
  begin
    Nodes := TList.Create();
    if (not Session.Update()) then
      Result := False
    else
      if (Job.JobObject.ObjectType = jotServer) then
        Nodes.Add(FSelect.Items[0])
      else if (not Session.Databases.Update()) then
        Result := False
      else if (Job.JobObject.ObjectType = jotServer) then
      begin
        FSelect.Items[0].Expand(False);
        Nodes.Add(FSelect.Items[0]);
      end
      else
      begin
        FSelect.Items[0].Expand(False);
        if (Job.JobObject.ObjectType = jotDatabase) then
        begin
          for J := 0 to FSelect.Items[0].Count - 1 do
            if (Session.Databases.NameCmp(FSelect.Items[0].Item[J].Text, Job.JobObject.Name) = 0) then
              Nodes.Add(FSelect.Items[0].Item[J]);
        end
        else
        begin
          for J := 0 to FSelect.Items[0].Count - 1 do
            if (Session.Databases.NameCmp(FSelect.Items[0].Item[J].Text, Job.JobObject.DatabaseName) = 0) then
            begin
              Database := Session.DatabaseByName(Job.JobObject.DatabaseName);
              if (not Database.Update()) then
                Result := False
              else
              begin
                FSelect.Items[0].Item[J].Expand(False);
                for K := 0 to FSelect.Items[0].Item[J].Count - 1 do
                  if ((Job.JobObject.ObjectType = jotTable) and (TObject(FSelect.Items[0].Item[J].Item[K].Data) is TSTable) and (Database.Tables.NameCmp(TSTable(FSelect.Items[0].Item[J].Item[K].Data).Name, Job.JobObject.Name) = 0)) then
                    Nodes.Add(FSelect.Items[0].Item[J].Item[K]);
              end;
            end;
        end;
      end;
    FSelect.Select(Nodes);
    Nodes.Free();
  end;
end;

procedure TDImport.OnError(const Sender: TObject; const Error: TTool.TError; const Item: TTool.TItem; const ShowRetry: Boolean;  var Success: TDataAction);
var
  ErrorMsg: string;
  Flags: Integer;
  Msg: string;
begin
  ErrorMsg := '';
  case (Error.ErrorType) of
    TE_Database:
      begin
        Msg := Preferences.LoadStr(165, IntToStr(Error.Session.ErrorCode), Error.Session.ErrorMessage);
        ErrorMsg := Error.Session.ErrorMessage;
        if (Error.Session.ErrorCode > 0) then
          ErrorMsg := ErrorMsg + ' (#' + IntToStr(Error.Session.ErrorCode) + ')';
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
    TE_Warning:
      begin
        Msg := '';
        ErrorMsg := Error.ErrorMessage;
      end;
    else
      begin
        Msg := Error.ErrorMessage;
        ErrorMsg := Msg;
      end;
  end;

  if (Error.ErrorType = TE_Warning) then
    Success := daFail
  else
  begin
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
  end;

  if (((Success in [daAbort, daFail]) or (Error.ErrorType = TE_Warning)) and (ErrorMsg <> '')) then
  begin
    FErrors.Caption := IntToStr(TTool(Sender).ErrorCount);
    FErrorMessages.Lines.Add(ErrorMsg);
  end;
end;

procedure TDImport.OnExecuted(const ASuccess: Boolean);
begin
  PostMessage(Handle, CM_EXECUTIONDONE, WPARAM(ASuccess), 0);
end;

procedure TDImport.OnUpdate(const AProgressInfos: TTool.TProgressInfos);
begin
  MoveMemory(@ProgressInfos, @AProgressInfos, SizeOf(AProgressInfos));

  PostMessage(Handle, CM_UPDATEPROGRESSINFO, 0, LPARAM(@ProgressInfos))
end;

procedure TDImport.TSCSVOptionsHide(Sender: TObject);
begin
  if (Length(FFields) = 0) then
    InitTSFields(Sender);
end;

procedure TDImport.TSCSVOptionsShow(Sender: TObject);
begin
  if (FCSVPreview.Items.Count = 0) then
    FCSVPreviewUpdate(Sender)
  else
  begin
    if (not FDelimiterTab.Checked and (FDelimiter.Text = '') or not FQuoteNothing.Checked and (FQuoteChar.Text = '')) then
    begin
      TSFields.Enabled := False;
      TSWhat.Enabled := False;
    end
    else
    begin
      TSFields.Enabled := (SObject is TSBaseTable);
      TSWhat.Enabled := not TSFields.Enabled;
    end;
    CheckActivePageChange(TSCSVOptions.PageIndex);
  end;
end;

procedure TDImport.TSExecuteShow(Sender: TObject);
var
  Answer: Integer;

  procedure ImportAdd(TableName: string; const SourceTableName: string = '');
  begin
    TableName := Session.ApplyIdentifierName(TableName);
    if ((Answer <> IDYESALL) and not (SObject is TSTable) and Assigned(Database.TableByName(TableName))) then
      Answer := MsgBox(Preferences.LoadStr(700, Database.Name + '.' + TableName), Preferences.LoadStr(101), MB_YESYESTOALLNOCANCEL + MB_ICONQUESTION);
    if (Answer in [IDYES, IDYESALL]) then
      Import.Add(TableName, SourceTableName);
  end;

var
  I: Integer;
  J: Integer;
  Success: Boolean;
begin
  Session.UnRegisterEventProc(FormSessionEvent);

  FLProgressTables.Visible := ImportType in [itODBC, itAccessFile, itExcelFile, itXMLFile];
  FEntieredTables.Visible := FLProgressTables.Visible;
  FDoneTables.Visible := FLProgressTables.Visible;
  if (ImportType in [itODBC, itAccessFile, itExcelFile, itXMLFile]) then
    FLProgressRecords.Caption := Preferences.LoadStr(235) + ':'
  else
    FLProgressRecords.Caption := Preferences.LoadStr(67) + ':';

  FBBack.Enabled := False;
  FBForward.Enabled := False;
  FBForward.Default := False;
  FBCancel.Default := True;

  ActiveControl := FBCancel;

  FErrors.Caption := '0';
  FErrorMessages.Lines.Clear();

  Answer := IDYES;
  case (ImportType) of
    itTextFile:
      begin
        if (FDelimiterTab.Checked) then
          TTImportText(Import).Delimiter := #9
        else if (FDelimiter.Text = '') then
          TTImportText(Import).Delimiter := #0
        else
          TTImportText(Import).Delimiter := FDelimiter.Text[1];
        if (not FQuoteStrings.Checked) or (FQuoteChar.Text = '') then
          TTImportText(Import).Quoter := #0
        else
          TTImportText(Import).Quoter := FQuoteChar.Text[1];
        TTImportText(Import).UseHeadline := FCSVHeadline.Checked;

        if (SObject is TSTable) then
          ImportAdd(SObject.Name)
        else
          ImportAdd(Copy(Filename, 1 + Length(ExtractFilePath(Filename)), Length(Filename) - Length(ExtractFilePath(Filename)) - Length(ExtractFileExt(Filename))));
      end;
    itAccessFile:
      begin
        if (SObject is TSTable) then
          ImportAdd(SObject.Name, TTableName(FTables.Selected.Data).SourceName)
        else
          for I := 0 to FTables.Items.Count - 1 do
            if (FTables.Items[I].Selected) then
              ImportAdd(TTableName(FTables.Items[I].Data).TableName, TTableName(FTables.Items[I].Data).SourceName);
      end;
    itExcelFile:
      begin
        if (SObject is TSTable) then
          ImportAdd(SObject.Name, TTableName(FTables.Selected.Data).SourceName)
        else
          for I := 0 to FTables.Items.Count - 1 do
            if (FTables.Items[I].Selected) then
              ImportAdd(TTableName(FTables.Items[I].Data).TableName, TTableName(FTables.Items[I].Data).SourceName);
      end;
    itODBC:
      begin
        if (SObject is TSTable) then
          ImportAdd(SObject.Name, TTableName(FTables.Selected.Data).SourceName)
        else
          for I := 0 to FTables.Items.Count - 1 do
            if (FTables.Items[I].Selected) then
              ImportAdd(TTableName(FTables.Items[I].Data).TableName, TTableName(FTables.Items[I].Data).SourceName);
      end;
    itXMLFile:
      if (SObject is TSBaseTable) then
      begin
        ImportAdd(SObject.Name, FRecordTag.Text);
      end;
  end;

  Success := (Answer <> IDCANCEL);
  if (not Success) then
    SendMessage(Self.Handle, CM_EXECUTIONDONE, WPARAM(Success), 0)
  else
  begin
    Import.Data := (SObject is TSTable) or not (SObject is TSTable) and FData.Checked;
    Import.Charset := FCharset.Text;
    Import.Collation := FCollation.Text;
    Import.Engine := FEngine.Text;
    if (Import.Structure) then
      Import.StmtType := stInsert
    else if (FReplace.Checked) then
      Import.StmtType := stReplace
    else if (FUpdate.Checked) then
      Import.StmtType := stUpdate
    else
      Import.StmtType := stInsert;
    Import.RowType := TMySQLRowType(FRowFormat.ItemIndex);
    Import.Structure := not (SObject is TSTable) and FStructure.Checked;

    if (SObject is TSBaseTable) then
    begin
      TSBaseTable(SObject).InvalidateData();
      for I := 0 to TSBaseTable(SObject).Fields.Count - 1 do
        for J := 0 to Length(FFields) - 1 do
          if ((FSourceFields[J].Text <> '') and (FFields[J].ItemIndex = I + 1)) then
          begin
            SetLength(Import.Fields, Length(Import.Fields) + 1);
            Import.Fields[Length(Import.Fields) - 1] := TSBaseTable(SObject).Fields[I];
            SetLength(Import.SourceFields, Length(Import.SourceFields) + 1);
            Import.SourceFields[Length(Import.Fields) - 1].Name := FSourceFields[J].Text;
          end;
    end;

    Import.Wnd := Self.Handle;
    Import.OnError := OnError;
    Import.OnExecuted := OnExecuted;
    Import.OnUpdate := OnUpdate;
    Import.Start();
  end;
end;

procedure TDImport.TSFieldsChange(Sender: TObject);
var
  I: Integer;
begin
  TSStmtType.Enabled := False;
  for I := 0 to Length(FFields) - 1 do
    if ((FSourceFields[I].Text <> '') and (FFields[I].ItemIndex > 0)) then
      TSStmtType.Enabled := True;
  CheckActivePageChange(TSFields.PageIndex);
end;

procedure TDImport.TSFieldsShow(Sender: TObject);
begin
  TSFieldsChange(Sender);
end;

procedure TDImport.TSJobHide(Sender: TObject);
begin
  if (Assigned(Import)) then
    Import.Free();

  case (ImportType) of
    itSQLFile: Import := TTImportSQL.Create(Filename, CodePage, Session, Database);
    itTextFile: Import := TTImportText.Create(Filename, CodePage, Session, Database);
    itAccessFile: Import := TTImportAccess.Create(Session, Database, Filename);
    itExcelFile: Import := TTImportExcel.Create(Session, Database, Filename);
    itODBC: Import := TTImportODBC.Create(Session, Database, DODBC.DataSource, DODBC.Username, DODBC.Password);
    itXMLFile: if (SObject is TSBaseTable) then Import := TTImportXML.Create(Filename, TSBaseTable(SObject));
  end;
end;

procedure TDImport.TSStmtTypeShow(Sender: TObject);
var
  I: Integer;
  J: Integer;
  Selected: Boolean;
begin
  if (not (SObject is TSBaseTable) or not Assigned(TSBaseTable(SObject).Keys.PrimaryKey)) then
    FUpdate.Enabled := False
  else
  begin
    Selected := True;
    for I := 0 to TSBaseTable(SObject).Keys.PrimaryKey.Columns.Count - 1 do
      if (Selected) then
      begin
        Selected := False;
        for J := 0 to Length(FFields) - 1 do
          if ((FSourceFields[J].Text <> '') and (FFields[J].ItemIndex = TSBaseTable(SObject).Keys.PrimaryKey.Columns[I].Field.Index + 1)) then
            Selected := True;
      end;
    if (Selected) then
    begin
      Selected := False;
      for J := 0 to Length(FFields) - 1 do
        if ((FSourceFields[J].Text <> '') and (FFields[J].ItemIndex > 0) and not TSBaseTable(SObject).Fields[FFields[J].ItemIndex - 1].InPrimaryKey) then
          Selected := True;
    end;

    FUpdate.Enabled := Selected;
  end;

  FStmtTypeClick(Sender);
end;

procedure TDImport.TSWhatHide(Sender: TObject);
begin
  InitTSFields(Sender);
end;

procedure TDImport.TSWhatShow(Sender: TObject);
begin
  FStructure.Enabled := not (SObject is TSBaseTable);
  FData.Enabled := not (SObject is TSBaseTable);

  TSFields.Enabled := (FStructure.Checked or FData.Checked) and (SObject is TSBaseTable);
  TSExecute.Enabled := (DialogType = idtNormal) and (FStructure.Checked or FData.Checked) and not TSFields.Enabled;
  TSTask.Enabled := (DialogType <> idtNormal) and (FStructure.Checked or FData.Checked) and not TSFields.Enabled;
  CheckActivePageChange(TSWhat.PageIndex);
end;

procedure TDImport.TSSelectShow(Sender: TObject);
begin
  FSelectChange(Sender, FSelect.Selected);
end;

procedure TDImport.TSTablesHide(Sender: TObject);
begin
  if (ImportType in [itExcelFile, itAccessFile, itODBC, itXMLFile]) then
  begin
    ClearTSFields(Sender);
    if (TSFields.Enabled) then
      InitTSFields(Sender);
  end;
end;

procedure TDImport.TSTablesShow(Sender: TObject);
var
  I: Integer;
  ListItem: TListItem;
  StringList: TStringList;
begin
  if (FTables.Items.Count = 0) then
  begin
    TableNames.Clear();

    StringList := TStringList.Create();
    case (ImportType) of
      itExcelFile:
        TTImportExcel(Import).GetTableNames(StringList);
      itAccessFile:
        TTImportAccess(Import).GetTableNames(StringList);
      itODBC:
        TTImportODBC(Import).GetTableNames(StringList);
    end;
    for I := 0 to StringList.Count - 1 do
      TableNames.Add(StringList[I]);
    StringList.Free();

    FTables.MultiSelect := not (SObject is TSBaseTable);
    FTables.SortType := stNone;
    FTables.Items.BeginUpdate();
    for I := 0 to TableNames.Count - 1 do
    begin
      ListItem := FTables.Items.Add();
      ListItem.Caption := TableNames[I].Caption;
      ListItem.ImageIndex := iiTable;
      ListItem.Data := TCustomData(TableNames[I]);
    end;
    FTables.Items.EndUpdate();
    FTables.SortType := stText;
    FTables.Column[0].AutoSize := False;
    FTables.Column[0].Width := ColumnTextWidth;
    FTables.Column[0].AutoSize := True;

    if (FTables.Items.Count = 1) then
    begin
      FTables.Selected := FTables.Items[0];
      FTables.ItemFocused := FTables.Items[0];
    end
    else if (FTables.Items.Count > 1) then
      FTables.ItemFocused := FTables.Items[0];
  end;

  FTablesSelectItem(FTables, FTables.Selected, Assigned(FTables.Selected));
end;

procedure TDImport.TSTaskShow(Sender: TObject);
begin
  CheckActivePageChange(TSTask.PageIndex);
end;

procedure TDImport.TSXMLOptionChange(Sender: TObject);
begin
  TSFields.Enabled := FRecordTag.Text <> '';

  CheckActivePageChange(TSXMLOptions.PageIndex);
end;

procedure TDImport.TSXMLOptionsHide(Sender: TObject);
begin
  if (Length(FFields) = 0) then
    InitTSFields(Sender);
end;

procedure TDImport.TSXMLOptionsShow(Sender: TObject);
begin
  TSXMLOptionChange(Sender);
end;

initialization
  FImport := nil;
end.

