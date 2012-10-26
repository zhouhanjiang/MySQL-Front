unit fDImport;

interface {********************************************************************}

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms,
  Dialogs, ComCtrls, ExtCtrls, StdCtrls, DB, DBTables, Consts, Contnrs,
  ODBCAPI,
  DISQLite3Api,
  ComCtrls_Ext, Forms_Ext, StdCtrls_Ext, ExtCtrls_Ext, Dialogs_Ext,
  MySQLDB,
  fSession, fTools,
  fBase;

const
  LargeSQLScriptSize = 100 * 1024;

type
  TImportType = (itSQLFile, itTextFile, itAccessFile, itExcelFile, itSQLiteFile, itODBC, itODBCFile, itXMLFile);
  TLoadFromStream = procedure(Stream: TStream) of object;

  TDImport = class (TForm_Ext)
    FBBack: TButton;
    FBCancel: TButton;
    FBForward: TButton;
    FBHelp: TButton;
    FCollation: TComboBox_Ext;
    FCSVHeadline: TCheckBox;
    FCSVPreview: TListView;
    FData: TCheckBox;
    FDefaultCharset: TComboBox_Ext;
    FDelimiter: TEdit;
    FDelimiterChar: TRadioButton;
    FDelimiterTab: TRadioButton;
    FDoneRecords: TLabel;
    FDoneTables: TLabel;
    FDoneTime: TLabel;
    FEngine: TComboBox_Ext;
    FEntieredRecords: TLabel;
    FEntieredTables: TLabel;
    FEntieredTime: TLabel;
    FErrorMessages: TRichEdit;
    FErrors: TLabel;
    FField1: TComboBox_Ext;
    FInsert: TRadioButton;
    FL2RecordTag: TLabel;
    FL3RecordTag: TLabel;
    FLCollation: TLabel;
    FLCSVHeadline: TLabel;
    FLDefaultCharset: TLabel;
    FLDelimiter: TLabel;
    FLDone: TLabel;
    FLEngine: TLabel;
    FLEntiered: TLabel;
    FLErrors: TLabel;
    FLFields: TLabel;
    FLImportType: TLabel;
    FLProgressRecords: TLabel;
    FLProgressTables: TLabel;
    FLProgressTime: TLabel;
    FLQuoteValues: TLabel;
    FLRecordTag: TLabel;
    FLReferrer1: TLabel;
    FLRowFormat: TLabel;
    FLSourceFields: TLabel;
    FLWhat: TLabel;
    FQuoteNothing: TRadioButton;
    FObjects: TCheckBox;
    FProgressBar: TProgressBar;
    FQuoteChar: TEdit;
    FRecordTag: TEdit;
    FReplace: TRadioButton;
    FRowFormat: TComboBox_Ext;
    FSourceField1: TEdit;
    FSourceField2: TEdit;
    FQuoteStrings: TRadioButton;
    FTables: TListView;
    FUpdate: TRadioButton;
    GCSVHow: TGroupBox_Ext;
    GCSVPreview: TGroupBox_Ext;
    GErrorMessages: TGroupBox_Ext;
    GFields: TGroupBox_Ext;
    GImportType: TGroupBox_Ext;
    GODBCDatabaseHow: TGroupBox_Ext;
    GODBCWhat: TGroupBox_Ext;
    GProgress: TGroupBox_Ext;
    GTable: TGroupBox_Ext;
    GXMLHow: TGroupBox_Ext;
    OpenDialog: TOpenDialog_Ext;
    PageControl: TPageControl;
    PCSVPreview: TPanel_Ext;
    PDelimiter: TPanel_Ext;
    PErrorMessages: TPanel_Ext;
    PQuoting: TPanel_Ext;
    PTables: TPanel_Ext;
    ScrollBox: TScrollBox;
    Sizer: TRadioButton;
    TSCSVOptions: TTabSheet;
    TSExecute: TTabSheet;
    TSFields: TTabSheet;
    TSImportType: TTabSheet;
    TSODBCOptions: TTabSheet;
    TSTables: TTabSheet;
    TSXMLOptions: TTabSheet;
    procedure FBBackClick(Sender: TObject);
    procedure FBCancelClick(Sender: TObject);
    procedure FBForwardClick(Sender: TObject);
    procedure FBHelpClick(Sender: TObject);
    procedure FCSVKeyPress(Sender: TObject; var Key: Char);
    procedure FCSVPreviewUpdate(Sender: TObject);
    procedure FDataClick(Sender: TObject);
    procedure FDataKeyPress(Sender: TObject; var Key: Char);
    procedure FDefaultCharsetChange(Sender: TObject);
    procedure FDelimiterClick(Sender: TObject);
    procedure FDelimiterKeyPress(Sender: TObject; var Key: Char);
    procedure FFieldExit(Sender: TObject);
    procedure FImportTypeClick(Sender: TObject);
    procedure FImportTypeKeyPress(Sender: TObject; var Key: Char);
    procedure FObjectsClick(Sender: TObject);
    procedure FObjectsKeyPress(Sender: TObject; var Key: Char);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure FormCreate(Sender: TObject);
    procedure FormHide(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure FQuoteClick(Sender: TObject);
    procedure FQuoteKeyPress(Sender: TObject; var Key: Char);
    procedure FTablesDblClick(Sender: TObject);
    procedure FTablesSelectItem(Sender: TObject; Item: TListItem;
      Selected: Boolean);
    procedure TSCSVOptionsHide(Sender: TObject);
    procedure TSCSVOptionsShow(Sender: TObject);
    procedure TSExecuteShow(Sender: TObject);
    procedure TSFieldsChange(Sender: TObject);
    procedure TSFieldsShow(Sender: TObject);
    procedure TSImportTypeShow(Sender: TObject);
    procedure TSODBCOptionsHide(Sender: TObject);
    procedure TSODBCOptionsShow(Sender: TObject);
    procedure TSTablesHide(Sender: TObject);
    procedure TSTablesShow(Sender: TObject);
    procedure TSXMLOptionChange(Sender: TObject);
    procedure TSXMLOptionsHide(Sender: TObject);
    procedure TSXMLOptionsShow(Sender: TObject);
  type
    TTableName = class
    private
      FSourceName: string;
      function GetCaption(): string;
      function GetMySQLName(): string;
    public
      constructor Create(const ASourceName: string);
      property Caption: string read GetCaption;
      property MySQLName: string read GetMySQLName;
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
    FFields: array of TComboBox_Ext;
    FLReferrers: array of TLabel;
    FSourceFields: array of TEdit;
    Import: TTImport;
    ODBC: SQLHDBC;
    ProgressInfos: TTool.TProgressInfos;
    SQLite: sqlite3_ptr;
    TableNames: TTableNames;
    procedure CheckActivePageChange(const ActivePageIndex: Integer);
    procedure ClearTSFields(Sender: TObject);
    procedure InitTSFields(Sender: TObject);
    procedure OnError(const Sender: TObject; const Error: TTool.TError; const Item: TTool.TItem; const ShowRetry: Boolean; var Success: TDataAction);
    procedure OnExecuted(const ASuccess: Boolean);
    procedure OnUpdate(const AProgressInfos: TTool.TProgressInfos);
    procedure CMChangePreferences(var Message: TMessage); message CM_CHANGEPREFERENCES;
    procedure CMExecutedDone(var Message: TMessage); message CM_EXECUTIONDONE;
    procedure CMSysFontChanged(var Message: TMessage); message CM_SYSFONTCHANGED;
    procedure CMUpdateProgressInfo(var Message: TMessage); message CM_UPDATEPROGRESSINFO;
  public
    Session: TSSession;
    CodePage: Cardinal;
    Database: TSDatabase;
    Filename: TFileName;
    ImportType: TImportType;
    Table: TSBaseTable;
    Window: TForm;
    function Execute(): Boolean;
  end;

function DImport(): TDImport;

implementation {***************************************************************}

{$R *.dfm}

uses
  CommDlg, Dlgs, Math, StrUtils, RichEdit,
  SQLUtils, CSVUtils,
  fPreferences, fDLogin, fDDatabases;

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

function TDImport.TTableName.GetMySQLName(): string;
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

  if (NextActivePageIndex > 0) then
    if (NextActivePageIndex < TSExecute.PageIndex) then
      FBForward.Caption := Preferences.LoadStr(229) + ' >'
    else
      FBForward.Caption := Preferences.LoadStr(174);

  FBForward.Enabled := (NextActivePageIndex >= 0) and ((NextActivePageIndex < TSExecute.PageIndex) or True);
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
  GTable.Caption := Preferences.LoadStr(234);

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

  GODBCWhat.Caption := Preferences.LoadStr(227);
  FLWhat.Caption := Preferences.LoadStr(218) + ':';
  FObjects.Caption := Preferences.LoadStr(215);
  FData.Caption := Preferences.LoadStr(216);
  GODBCDatabaseHow.Caption := Preferences.LoadStr(238);
  FLEngine.Caption := Preferences.LoadStr(110) + ':';
  FLDefaultCharset.Caption := Preferences.LoadStr(682) + ':';
  FLCollation.Caption := Preferences.LoadStr(702) + ':';
  FLRowFormat.Caption := Preferences.LoadStr(129) + ':';

  GXMLHow.Caption := Preferences.LoadStr(238);
  FLRecordTag.Caption := Preferences.LoadStr(124) + ':';

  GFields.Caption := Preferences.LoadStr(253);
  FLFields.Caption := Preferences.LoadStr(401) + ':';

  GImportType.Caption := Preferences.LoadStr(238);
  FLImportType.Caption := Preferences.LoadStr(124) + ':';
  FInsert.Caption := LowerCase(Preferences.LoadStr(880)) + ' (INSERT)';
  FReplace.Caption := LowerCase(Preferences.LoadStr(416)) + ' (REPLACE)';
  FUpdate.Caption := LowerCase(Preferences.LoadStr(726)) + ' (UPDATE)';

  GProgress.Caption := Preferences.LoadStr(224);
  FLEntiered.Caption := Preferences.LoadStr(211) + ':';
  FLDone.Caption := Preferences.LoadStr(232) + ':';
  FLProgressTables.Caption := Preferences.LoadStr(234) + ':';
  FLProgressTime.Caption := ReplaceStr(Preferences.LoadStr(661), '&', '') + ':';
  FLErrors.Caption := Preferences.LoadStr(391) + ':';

  GErrorMessages.Caption := Preferences.LoadStr(392);

  OpenDialog.EncodingLabel := Preferences.LoadStr(682) + ':';

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

procedure TDImport.CMSysFontChanged(var Message: TMessage);
begin
  inherited;

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

  if (Assigned(Import) and Import.Suspended) then
    Application.ProcessMessages();
end;

function TDImport.Execute(): Boolean;
var
  Cancel: Boolean;
  cbMessageText: SQLSMALLINT;
  ConnStrIn: string;
  DatabaseName: string;
  MessageText: PSQLTCHAR;
  SQLState: array [0 .. SQL_SQLSTATE_SIZE] of SQLWCHAR;
  Success: Boolean;
begin
  ModalResult := mrNone;
  PageControl.ActivePageIndex := -1;

  ODBC := SQL_NULL_HANDLE;
  TableNames := TTableNames.Create();

  case (ImportType) of
    itExcelFile,
    itAccessFile:
      begin
        if (ImportType = itExcelFile) then
          ConnStrIn := 'Driver={Microsoft Excel Driver (*.xls, *.xlsx, *.xlsm, *.xlsb)};' + 'DBQ=' + Filename + ';' + 'READONLY=TRUE'
        else
          ConnStrIn := 'Driver={Microsoft Access Driver (*.mdb, *.accdb)};' + 'DBQ=' + Filename + ';' + 'READONLY=TRUE';

        Success := SQL_SUCCEEDED(SQLAllocHandle(SQL_HANDLE_DBC, ODBCEnv, @ODBC));
        if (Success) then
        begin
          Success := SQL_SUCCEEDED(SQLDriverConnect(ODBC, Application.Handle, PSQLTCHAR(ConnStrIn), SQL_NTS, nil, 0, nil, SQL_DRIVER_COMPLETE));
          if (not Success) then
          begin
            if (ImportType = itExcelFile) then
              ConnStrIn := 'Driver={Microsoft Excel Driver (*.xls)};' + 'DBQ=' + Filename + ';' + 'READONLY=TRUE'
            else
              ConnStrIn := 'Driver={Microsoft Access Driver (*.mdb)};' + 'DBQ=' + Filename + ';' + 'READONLY=TRUE';
            Success := SQL_SUCCEEDED(SQLDriverConnect(ODBC, Application.Handle, PSQLTCHAR(ConnStrIn), SQL_NTS, nil, 0, nil, SQL_DRIVER_COMPLETE));
          end;
        end;
        if (not Success) then
          if ((ODBC <> SQL_NULL_HANDLE) and SQL_SUCCEEDED(SQLGetDiagRec(SQL_HANDLE_DBC, ODBC, 1, nil, nil, nil, 0, @cbMessageText))) then
          begin
            if (SQL_SUCCEEDED(SQLGetDiagRec(SQL_HANDLE_DBC, ODBC, 1, nil, nil, nil, 0, @cbMessageText))) then
            begin
              GetMem(MessageText, (cbMessageText + 1) * SizeOf(SQLTCHAR));
              if (SQL_SUCCEEDED(SQLGetDiagRec(SQL_HANDLE_DBC, ODBC, 1, @SQLState, nil, MessageText, cbMessageText + 1, nil))) then
                MsgBox(PChar(MessageText) + ' (' + SQLState + ')', Preferences.LoadStr(45), MB_OK + MB_ICONERROR);
              FreeMem(MessageText);
            end;
            SQLFreeHandle(SQL_HANDLE_DBC, ODBC); ODBC := SQL_NULL_HANDLE;
          end
          else
            MsgBox('Unknown ODBC Error.', Preferences.LoadStr(45), MB_OK + MB_ICONERROR);
      end;
    itODBC:
      begin
        Success := SQL_SUCCEEDED(SQLAllocHandle(SQL_HANDLE_DBC, ODBCEnv, @ODBC));
        Success := Success and SQL_SUCCEEDED(SQLSetConnectAttr(ODBC, SQL_ATTR_ACCESS_MODE, SQLPOINTER(SQL_MODE_READ_ONLY), SQL_IS_POINTER));

        DDatabases.Session := nil;
        DDatabases.SelectedDatabases := '';
        if (Success) then
          repeat
            Success := DDatabases.Execute();
            Cancel := not Success;
            if (Success) then
            begin
              DatabaseName := DDatabases.SelectedDatabases;
              if ((Copy(DatabaseName, 1, 1) = '"') and (Copy(DatabaseName, Length(DatabaseName), 1) = '"')) then
                DatabaseName := Copy(DatabaseName, 2, Length(DatabaseName) - 2);

              DLogin.Account := nil;
              DLogin.Filename := DatabaseName;
              DLogin.Window := Window;
              Success := DLogin.Execute();
              Cancel := not Success;
              if (Success) then
              begin
                Success := Success and SQL_SUCCEEDED(SQLConnect(ODBC, PSQLTCHAR(PChar(DatabaseName)), SQL_NTS, PSQLTCHAR(PChar(DLogin.Username)), SQL_NTS, PSQLTCHAR(PChar(DLogin.Password)), SQL_NTS));
                if (not Success and (ODBC <> SQL_NULL_HANDLE)) then
                begin
                  if (SQL_SUCCEEDED(SQLGetDiagRec(SQL_HANDLE_DBC, ODBC, 1, nil, nil, nil, 0, @cbMessageText))) then
                  begin
                    GetMem(MessageText, (cbMessageText + 1) * SizeOf(SQLTCHAR));
                    if (SQL_SUCCEEDED(SQLGetDiagRec(SQL_HANDLE_DBC, ODBC, 1, @SQLState, nil, MessageText, cbMessageText + 1, nil))) then
                      MsgBox(PChar(MessageText) + ' (' + SQLState + ')', Preferences.LoadStr(45), MB_OK + MB_ICONERROR);
                    FreeMem(MessageText);
                  end;
                  SQLFreeHandle(SQL_HANDLE_DBC, ODBC); ODBC := SQL_NULL_HANDLE;
                end;
              end;
            end;
          until (Success or Cancel)
        else
          if ((ODBC <> SQL_NULL_HANDLE) and SQL_SUCCEEDED(SQLGetDiagRec(SQL_HANDLE_DBC, ODBC, 1, nil, nil, nil, 0, @cbMessageText))) then
          begin
            if (SQL_SUCCEEDED(SQLGetDiagRec(SQL_HANDLE_DBC, ODBC, 1, nil, nil, nil, 0, @cbMessageText))) then
            begin
              GetMem(MessageText, (cbMessageText + 1) * SizeOf(SQLTCHAR));
              if (SQL_SUCCEEDED(SQLGetDiagRec(SQL_HANDLE_DBC, ODBC, 1, @SQLState, nil, MessageText, cbMessageText + 1, nil))) then
                MsgBox(PChar(MessageText) + ' (' + SQLState + ')', Preferences.LoadStr(45), MB_OK + MB_ICONERROR);
              FreeMem(MessageText);
            end;
            SQLFreeHandle(SQL_HANDLE_DBC, ODBC); ODBC := SQL_NULL_HANDLE;
          end
          else
            MsgBox('Unknown ODBC Error.', Preferences.LoadStr(45), MB_OK + MB_ICONERROR);
      end;
    else
      Success := True;
  end;

  Result := Success and (ShowModal() = mrOk);

  TableNames.Free();
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
  if (Assigned(Import)) then
  begin
    Import.UserAbort.SetEvent();
    if (Session.Asynchron) then
    begin
      Import.WaitFor();
      FreeAndNil(Import);
    end;
  end;
end;

procedure TDImport.FBForwardClick(Sender: TObject);
var
  PageIndex: Integer;
begin
  for PageIndex := PageControl.ActivePageIndex + 1 to PageControl.PageCount - 1 do
    if (PageControl.Pages[PageIndex].Enabled) then
    begin
      PageControl.ActivePageIndex := PageIndex;
      Exit;
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
  Import: TTImportText;
  Item: TListItem;
  Values: TSQLStrings;
begin
  if (Visible) then
  begin
    if (not FDelimiterTab.Checked and (FDelimiter.Text = '') or not FQuoteNothing.Checked and (FQuoteChar.Text = '')) then
    begin
      TSFields.Enabled := False;
      TSODBCOptions.Enabled := False;
    end
    else
    begin
      TSFields.Enabled := Assigned(Table);
      TSODBCOptions.Enabled := not TSFields.Enabled;
    end;


    FCSVPreview.DisableAlign(); FCSVPreview.Items.BeginUpdate();
    FCSVPreview.Columns.Clear();
    FCSVPreview.Items.Clear();

    if (TSFields.Enabled or TSODBCOptions.Enabled) then
    begin
      Import := TTImportText.Create(Filename, CodePage, Session, Database);

      if (FDelimiterTab.Checked) then
        Import.Delimiter := #9
      else if (FDelimiter.Text = '') then
        Import.Delimiter := #0
      else
        Import.Delimiter := FDelimiter.Text[1];
      if (not FQuoteStrings.Checked) or (FQuoteChar.Text = '') then
        Import.Quoter := #0
      else
        Import.Quoter := FQuoteChar.Text[1];
      Import.UseHeadLine := FCSVHeadline.Checked;
      Import.Open();

      for I := 0 to Import.HeadlineNameCount - 1 do
        FCSVPreview.Columns.Add().Caption := Import.HeadlineNames[I];

      Item := nil;
      while ((FCSVPreview.Items.Count < 10) and Import.GetPreviewValues(Values)) do
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

      SetLength(Values, 0);

      Import.Close();
      Import.Free();
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
  FObjects.Checked := FObjects.Checked or FData.Checked and not Assigned(Table);

  TSFields.Enabled := FData.Checked and Assigned(Table);
  TSExecute.Enabled := FObjects.Checked and not TSFields.Enabled;
  CheckActivePageChange(TSODBCOptions.PageIndex);

  ClearTSFields(Sender);
end;

procedure TDImport.FDataKeyPress(Sender: TObject; var Key: Char);
begin
  FDataClick(Sender);
end;

procedure TDImport.FDefaultCharsetChange(Sender: TObject);
var
  I: Integer;
begin
  FCollation.Items.Clear();
  FCollation.Items.Add('');
  if (Assigned(Session.Collations)) then
    for I := 0 to Session.Collations.Count - 1 do
    begin
      if (lstrcmpi(PChar(Session.Collations[I].Charset.Name), PChar(FDefaultCharset.Text)) = 0) then
      begin
        FCollation.Items.Add(Session.Collations[I].Name);
        if (Session.Collations[I].Default) then
          FCollation.ItemIndex := FCollation.Items.Count - 1;
      end;
    end;
  FCollation.Enabled := FDefaultCharset.Text <> ''; FLCollation.Enabled := FCollation.Enabled;
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

procedure TDImport.FImportTypeClick(Sender: TObject);
begin
  TSExecute.Enabled := FInsert.Checked and FInsert.Enabled or FReplace.Checked and FReplace.Enabled or FUpdate.Checked and FUpdate.Enabled;
  CheckActivePageChange(TSImportType.PageIndex);
end;

procedure TDImport.FImportTypeKeyPress(Sender: TObject; var Key: Char);
begin
  FImportTypeClick(Sender);
end;

procedure TDImport.FObjectsClick(Sender: TObject);
begin
  FData.Checked := FData.Checked and (Assigned(Table) or FObjects.Checked);

  TSFields.Enabled := FData.Checked and Assigned(Table);
  TSExecute.Enabled := FObjects.Checked and not TSFields.Enabled;
  CheckActivePageChange(TSODBCOptions.PageIndex);

  ClearTSFields(Sender);
end;

procedure TDImport.FObjectsKeyPress(Sender: TObject; var Key: Char);
begin
  FObjectsClick(Sender);
end;

procedure TDImport.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  if (Assigned(SQLite)) then
    begin sqlite3_close(SQLite); SQLite := nil; end;

  PageControl.ActivePage := nil;
end;

procedure TDImport.FormCreate(Sender: TObject);
begin
  Import := nil;

  FTables.SmallImages := Preferences.SmallImages;

  FCSVHeadline.Checked := Preferences.Import.CSVHeadline;
  FDelimiterTab.Checked := Preferences.Import.CSVSeparatorType = dtTab;
  FDelimiterChar.Checked := Preferences.Import.CSVSeparatorType = dtChar;
  FDelimiter.Text := Preferences.Import.CSVSeparator;
  FQuoteNothing.Checked := Preferences.Import.CSVQuote = qtNothing;
  FQuoteStrings.Checked := Preferences.Import.CSVQuote = qtStrings;
  FQuoteChar.Text := Preferences.Import.CSVQuoteChar;
  if (Preferences.Import.ImportType = itReplace) then
    FReplace.Checked := True
  else if (Preferences.Import.ImportType = itUpdate) then
    FUpdate.Checked := True
  else
    FInsert.Checked := True;

  FObjects.Checked := Preferences.Import.ODBCObjects;
  FData.Checked := Preferences.Import.ODBCData;
  FRowFormat.ItemIndex := Preferences.Import.ODBCRowType;

  SendMessage(FErrorMessages.Handle, EM_SETTEXTMODE, TM_PLAINTEXT, 0);
  SendMessage(FErrorMessages.Handle, EM_SETWORDBREAKPROC, 0, LPARAM(@EditWordBreakProc));

  PageControl.ActivePage := nil; // Make sure, not ___OnShowPage will be executed
end;

procedure TDImport.FormHide(Sender: TObject);
begin
  if (ModalResult = mrOk) then
  begin
    Preferences.Import.CSVHeadline := FCSVHeadline.Checked;
    if (FDelimiterTab.Checked) then
      Preferences.Import.CSVSeparatorType := dtTab
    else if (FDelimiterChar.Checked) then
      Preferences.Import.CSVSeparatorType := dtChar;
    Preferences.Import.CSVSeparator := FDelimiter.Text;
    Preferences.Import.CSVQuoteChar := FQuoteChar.Text;
    if (FQuoteNothing.Checked) then
      Preferences.Import.CSVQuote := qtNothing
    else
      Preferences.Import.CSVQuote := qtStrings;

    if (FInsert.Checked) then Preferences.Import.ImportType := itInsert
    else if (FReplace.Checked) then Preferences.Import.ImportType := itReplace
    else if (FUpdate.Checked) then Preferences.Import.ImportType := itUpdate;

    Preferences.Import.ODBCObjects := FObjects.Checked;
    Preferences.Import.ODBCData := FData.Checked;
    Preferences.Import.ODBCRowType := FRowFormat.ItemIndex;
  end;

  FTables.Items.BeginUpdate();
  FTables.Items.Clear();
  FTables.Items.EndUpdate();
  ClearTSFields(Sender);

  FCSVPreview.Items.BeginUpdate();
  FCSVPreview.Items.Clear();
  FCSVPreview.Columns.Clear();
  FCSVPreview.Items.EndUpdate();

  if (ODBC <> SQL_NULL_HANDLE) then
  begin
    SQLDisconnect(ODBC);
    SQLFreeHandle(SQL_HANDLE_DBC, ODBC);
  end;
end;

procedure TDImport.FormShow(Sender: TObject);
var
  I: Integer;
begin
  if (ExtractFileName(Filename) = '') then
    Caption := Preferences.LoadStr(386)
  else
    Caption := Preferences.LoadStr(386) + ' ' + ExtractFileName(Filename);

  case (ImportType) of
    itSQLFile: HelpContext := 1010;
    itTextFile: HelpContext := 1133;
    itAccessFile: HelpContext := 1013;
    itExcelFile: HelpContext := 1106;
    itSQLiteFile: HelpContext := 1127;
    itODBC: HelpContext := 1012;
    itXMLFile: HelpContext := 1132;
    else HelpContext := 0;
  end;

  SQLite := nil;

  TSTables.Enabled := ImportType in [itAccessFile, itExcelFile, itSQLiteFile, itODBC];
  TSCSVOptions.Enabled := ImportType in [itTextFile];
  TSODBCOptions.Enabled := False;
  TSXMLOptions.Enabled := ImportType in [itXMLFile];
  TSFields.Enabled := False;
  TSExecute.Enabled := not TSTables.Enabled and not TSCSVOptions.Enabled and not TSODBCOptions.Enabled and not TSXMLOptions.Enabled and not TSFields.Enabled;

  FObjects.Enabled := not Assigned(Table);

  if ((ImportType in [itTextFile, itODBC, itAccessFile, itExcelFile, itSQLiteFile]) and not Assigned(Table)) then
  begin
    FEngine.Clear();
    if (Session.Engines.Count = 0) then
      FEngine.Style := csDropDown
    else
    begin
      FEngine.Style := csDropDownList;
      for I := 0 to Session.Engines.Count - 1 do
        FEngine.Items.Add(Session.Engines.Engine[I].Name);
      if (not Assigned(Session.Engines.DefaultEngine)) then
        FEngine.ItemIndex := -1
      else
        FEngine.ItemIndex := FEngine.Items.IndexOf(Session.Engines.DefaultEngine.Name);
    end;

    FDefaultCharset.Items.Clear();
    if (Session.Charsets.Count = 0) then
      FDefaultCharset.Style := csDropDown
    else
    begin
      FDefaultCharset.Style := csDropDownList;
      for I := 0 to Session.Charsets.Count - 1 do
        FDefaultCharset.Items.Add(Session.Charsets.Charset[I].Name);
    end;
    if (Session.ServerVersion < 40101) then
      FDefaultCharset.ItemIndex := FDefaultCharset.Items.IndexOf(Session.DefaultCharset)
    else
      FDefaultCharset.ItemIndex := FDefaultCharset.Items.IndexOf('utf8');
    FDefaultCharsetChange(Sender);

    FDefaultCharset.Visible := Database.Session.ServerVersion >= 40101; FLDefaultCharset.Visible := FDefaultCharset.Visible;
    FCollation.Visible := Database.Session.ServerVersion >= 40101; FLCollation.Visible := FCollation.Visible;
  end;

  FLProgressTables.Visible := ImportType in [itODBC, itAccessFile, itExcelFile, itSQLiteFile, itXMLFile];
  FEntieredTables.Visible := FLProgressTables.Visible;
  FDoneTables.Visible := FLProgressTables.Visible;
  if (ImportType in [itODBC, itAccessFile, itExcelFile, itSQLiteFile, itXMLFile]) then
    FLProgressRecords.Caption := Preferences.LoadStr(235) + ':'
  else
    FLProgressRecords.Caption := Preferences.LoadStr(67) + ':';

  if (TSFields.Enabled) then
    InitTSFields(Sender);
  if (not TSTables.Enabled) then
    TSTablesHide(Sender);

  for I := 0 to PageControl.PageCount - 1 do
    if ((PageControl.ActivePageIndex < 0) and PageControl.Pages[I].Enabled) then
      PageControl.ActivePageIndex := I;

  FBBack.Visible := not (ImportType in [itSQLFile]);
  FBForward.Visible := not (ImportType in [itSQLFile]);
  FBCancel.ModalResult := mrCancel;
  FBCancel.Caption := Preferences.LoadStr(30);

  if (FBForward.Visible and FBForward.Enabled) then
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

procedure TDImport.FTablesDblClick(Sender: TObject);
begin
  FTablesSelectItem(Sender, FTables.Selected, Assigned(FTables.Selected));
  if (FBForward.Enabled) then
    FBForward.Click();
end;

procedure TDImport.FTablesSelectItem(Sender: TObject; Item: TListItem;
  Selected: Boolean);
begin
  TSODBCOptions.Enabled := (FTables.SelCount > 0) and (ImportType in [itAccessFile, itExcelFile, itSQLiteFile, itODBC]) and not Assigned(Table);
  TSFields.Enabled := (FTables.SelCount > 0) and not TSODBCOptions.Enabled;
  CheckActivePageChange(TSTables.PageIndex);
end;

procedure TDImport.InitTSFields(Sender: TObject);
var
  cbCOLUMN_NAME: SQLINTEGER;
  COLUMN_NAME: array [0 .. STR_LEN] of SQLWCHAR;
  FieldNames: TStringList;
  Handle: SQLHSTMT;
  I: Integer;
  J: Integer;
  Stmt: sqlite3_stmt_ptr;
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
    else if ((ImportType in [itExcelFile, itAccessFile, itODBC]) and Assigned(Table)) then
    begin
      if (ImportType = itODBC) then
        FLSourceFields.Caption := Preferences.LoadStr(610) + ':'
      else
        FLSourceFields.Caption := Preferences.LoadStr(400) + ':';

      ODBCException(ODBC, SQLAllocHandle(SQL_HANDLE_STMT, ODBC, @Handle));
      ODBCException(Handle, SQLColumns(Handle, nil, 0, nil, 0, PSQLTCHAR(TTableName(FTables.Selected.Data).SourceName), SQL_NTS, nil, 0));
      ODBCException(Handle, SQLBindCol(Handle, 4, SQL_C_WCHAR, @COLUMN_NAME, SizeOf(COLUMN_NAME), @cbCOLUMN_NAME));
      while (SQL_SUCCEEDED(ODBCException(Handle, SQLFetch(Handle)))) do
        FieldNames.Add(COLUMN_NAME);
      SQLFreeHandle(SQL_HANDLE_STMT, Handle);
    end
    else if (ImportType in [itSQLiteFile]) then
    begin
      FLSourceFields.Caption := Preferences.LoadStr(400) + ':';

      SQLiteException(SQLite, sqlite3_prepare_v2(SQLite, PAnsiChar(UTF8Encode('SELECT * FROM "' + TTableName(FTables.Selected.Data).SourceName + '" WHERE 1<>1')), -1, @Stmt, nil));
      if (sqlite3_step(Stmt) = SQLITE_DONE) then
        for I := 0 to sqlite3_column_count(Stmt) - 1 do
          FieldNames.Add(UTF8ToString(StrPas(sqlite3_column_name(Stmt, I))));

      SQLiteException(SQLite, sqlite3_finalize(Stmt));
    end
    else if ((ImportType in [itXMLFile]) and Assigned(Table)) then
    begin
      FLSourceFields.Caption := Preferences.LoadStr(400) + ':';

      for I := 0 to Table.Fields.Count - 1 do
        FieldNames.Add(Table.Fields[I].Name);
    end;

    SetLength(FSourceFields, FieldNames.Count);
    SetLength(FLReferrers, Length(FSourceFields));
    SetLength(FFields, Length(FSourceFields));

    if (Assigned(Table)) then
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
          for J := 0 to Table.Fields.Count - 1 do
            FFields[I].Items.Add(Table.Fields.Field[J].Name);
        end
        else
          FFields[I].Items.Text := FFields[0].Items.Text;
        if ((ImportType in [itTextFile]) and FCSVHeadline.Checked or (ImportType in [itExcelFile, itAccessFile, itODBC, itSQLiteFile])) then
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

procedure TDImport.OnError(const Sender: TObject; const Error: TTool.TError; const Item: TTool.TItem; const ShowRetry: Boolean;  var Success: TDataAction);
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
  if (not Import.Suspended) then
    PostMessage(Handle, CM_EXECUTIONDONE, WPARAM(ASuccess), 0)
  else
  begin
    Perform(CM_EXECUTIONDONE, WPARAM(ASuccess), 0);
    Application.ProcessMessages();
  end;
end;

procedure TDImport.OnUpdate(const AProgressInfos: TTool.TProgressInfos);
begin
  MoveMemory(@ProgressInfos, @AProgressInfos, SizeOf(AProgressInfos));

  if (not Import.Suspended) then
    PostMessage(Handle, CM_UPDATEPROGRESSINFO, 0, LPARAM(@ProgressInfos))
  else
  begin
    Perform(CM_UPDATEPROGRESSINFO, 0, LPARAM(@ProgressInfos));
    Application.ProcessMessages();
  end;
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
    CheckActivePageChange(TSCSVOptions.PageIndex);
end;

procedure TDImport.TSExecuteShow(Sender: TObject);
var
  Answer: Integer;
  I: Integer;
  ImportODBC: TTImportODBC;
  ImportSQL: TTImportSQL;
  ImportSQLite: TTImportSQLite;
  ImportText: TTImportText;
  ImportXML: TTImportXML;
  J: Integer;
  ProgressInfos: TTool.TProgressInfos;
  Success: Boolean;
  MySQLName: string;
begin
  FBBack.Enabled := False;
  FBForward.Enabled := False;
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
  SendMessage(Self.Handle, CM_UPDATEPROGRESSINFO, 0, LPARAM(@ProgressInfos));

  if (ImportType = itSQLFile) then
  begin
    ImportSQL := TTImportSQL.Create(Filename, CodePage, Session, Database);

    Import := ImportSQL;
  end
  else if (ImportType in [itTextFile]) then
  begin
    ImportText := TTImportText.Create(Filename, CodePage, Session, Database);

    if (Assigned(ImportText)) then
    begin
      if (FDelimiterTab.Checked) then
        ImportText.Delimiter := #9
      else if (FDelimiter.Text = '') then
        ImportText.Delimiter := #0
      else
        ImportText.Delimiter := FDelimiter.Text[1];
      if (not FQuoteStrings.Checked) or (FQuoteChar.Text = '') then
        ImportText.Quoter := #0
      else
        ImportText.Quoter := FQuoteChar.Text[1];
      ImportText.UseHeadline := FCSVHeadline.Checked;

      if (Assigned(Table)) then
      begin
        if (FReplace.Checked) then
          ImportText.ImportType := fTools.itReplace
        else if (FUpdate.Checked) then
          ImportText.ImportType := fTools.itUpdate
        else
          ImportText.ImportType := fTools.itInsert;
        ImportText.Structure := False;

        ImportText.Add(Table.Name);
      end
      else
      begin
        ImportText.Charset := FDefaultCharset.Text;
        ImportText.Collation := FCollation.Text;
        ImportText.Data := FData.Checked;
        ImportText.Engine := FEngine.Text;
        ImportText.ImportType := fTools.itInsert;
        ImportText.RowType := TMySQLRowType(FRowFormat.ItemIndex);
        ImportText.Structure := True;

        Answer := IDYES;
        MySQLName := ExtractFileName(Filename);
        MySQLName := Copy(MySQLName, 1, Length(MySQLName) - Length(ExtractFileExt(MySQLName)));
        MySQLName := Session.ApplyIdentifierName(MySQLName);
        if (not Assigned(Database.TableByName(MySQLName))) then
          Answer := IDYES
        else if (Answer <> IDYESALL) then
          Answer := MsgBox(Preferences.LoadStr(700, Database.Name + '.' + MySQLName), Preferences.LoadStr(101), MB_YESYESTOALLNOCANCEL + MB_ICONQUESTION);
        if (Answer in [IDYES, IDYESALL]) then
          ImportText.Add(MySQLName)
        else if (Answer = IDCANCEL) then
          FreeAndNil(ImportText);
      end;
    end;

    Import := ImportText;
  end
  else if (ImportType in [itExcelFile, itAccessFile, itODBC]) then
  begin
    ImportODBC := TTImportODBC.Create(ODBC, Database);

    if (Assigned(ImportODBC)) then
    begin
      if (Assigned(Table)) then
      begin
        ImportODBC.Charset := Table.DefaultCharset;
        ImportODBC.Collation := Table.Collation;
        ImportODBC.Data := True;
        if (not Assigned(Table.Engine)) then
          ImportODBC.Engine := ''
        else
          ImportODBC.Engine := Table.Engine.Name;
        if (FReplace.Checked) then
          ImportODBC.ImportType := fTools.itReplace
        else if (FUpdate.Checked) then
          ImportODBC.ImportType := fTools.itUpdate
        else
          ImportODBC.ImportType := fTools.itInsert;
        ImportODBC.RowType := Table.RowType;
        ImportODBC.Structure := False;

        ImportODBC.Add(Table.Name, TTableName(FTables.Selected.Data).SourceName);
      end
      else
      begin
        ImportODBC.Charset := FDefaultCharset.Text;
        ImportODBC.Collation := FCollation.Text;
        ImportODBC.Data := FData.Checked;
        ImportODBC.Engine := FEngine.Text;
        ImportODBC.ImportType := fTools.itInsert;
        ImportODBC.RowType := TMySQLRowType(FRowFormat.ItemIndex);
        ImportODBC.Structure := not Assigned(Table) and FObjects.Checked;

        Answer := IDYES;
        for I := 0 to FTables.Items.Count - 1 do
          if (Assigned(ImportODBC) and (FTables.Items[I].Selected)) then
          begin
            MySQLName := TTableName(FTables.Items[I].Data).MySQLName;
            if (not Assigned(Database.TableByName(MySQLName))) then
              Answer := IDYES
            else if (Answer <> IDYESALL) then
              Answer := MsgBox(Preferences.LoadStr(700, Database.Name + '.' + MySQLName), Preferences.LoadStr(101), MB_YESYESTOALLNOCANCEL + MB_ICONQUESTION);
            if (Answer in [IDYES, IDYESALL]) then
              ImportODBC.Add(MySQLName, TTableName(FTables.Items[I].Data).SourceName)
            else if (Answer = IDCANCEL) then
              FreeAndNil(ImportODBC);
          end;
      end;
    end;

    Import := ImportODBC;
  end
  else if (ImportType = itSQLiteFile) then
  begin
    ImportSQLite := TTImportSQLite.Create(SQLite, Database);

    if (Assigned(ImportSQLite)) then
      if (Assigned(Table)) then
      begin
        ImportSQLite.Charset := Table.DefaultCharset;
        ImportSQLite.Collation := Table.Collation;
        ImportSQLite.Data := True;
        if (not Assigned(Table.Engine)) then
          ImportSQLite.Engine := ''
        else
          ImportSQLite.Engine := Table.Engine.Name;
        if (FReplace.Checked) then
          ImportSQLite.ImportType := fTools.itReplace
        else if (FUpdate.Checked) then
          ImportSQLite.ImportType := fTools.itUpdate
        else
          ImportSQLite.ImportType := fTools.itInsert;
        ImportSQLite.RowType := Table.RowType;
        ImportSQLite.Structure := False;

        ImportSQLite.Add(Table.Name, TTableName(FTables.Selected.Data).SourceName);
      end
      else
      begin
        ImportSQLite.Charset := FDefaultCharset.Text;
        ImportSQLite.Collation := FCollation.Text;
        ImportSQLite.Data := FData.Checked;
        ImportSQLite.Engine := FEngine.Text;
        ImportSQLite.ImportType := fTools.itInsert;
        ImportSQLite.RowType := TMySQLRowType(FRowFormat.ItemIndex);
        ImportSQLite.Structure := not Assigned(Table) and FObjects.Checked;

        Answer := IDYES;
        for I := 0 to FTables.Items.Count - 1 do
          if (Assigned(ImportSQLite) and (FTables.Items[I].Selected)) then
          begin
            MySQLName := TTableName(FTables.Items[I].Data).MySQLName;
            if (not Assigned(Database.TableByName(MySQLName))) then
              Answer := IDYES
            else if (Answer <> IDYESALL) then
              Answer := MsgBox(Preferences.LoadStr(700, Database.Name + '.' + MySQLName), Preferences.LoadStr(101), MB_YESYESTOALLNOCANCEL + MB_ICONQUESTION);
            if (Answer in [IDYES, IDYESALL]) then
              ImportSQLite.Add(MySQLName, TTableName(FTables.Items[I].Data).SourceName)
            else if (Answer = IDCANCEL) then
              FreeAndNil(ImportSQLite);
          end;
      end;

    Import := ImportSQLite;
  end
  else if (ImportType = itXMLFile) then
  begin
    ImportXML := TTImportXML.Create(Filename, Table);
    if (FReplace.Checked) then
      ImportXML.ImportType := fTools.itReplace
    else if (FUpdate.Checked) then
      ImportXML.ImportType := fTools.itUpdate
    else
      ImportXML.ImportType := fTools.itInsert;
    ImportXML.RecordNodeText := FRecordTag.Text;

    Import := ImportXML;
  end
  else
    Import := nil;

  if (Assigned(Import) and Assigned(Table)) then
  begin
    Table.InvalidateData();
    for I := 0 to Table.Fields.Count - 1 do
      for J := 0 to Length(FFields) - 1 do
        if ((FSourceFields[J].Text <> '') and (FFields[J].ItemIndex = I + 1)) then
        begin
          SetLength(Import.Fields, Length(Import.Fields) + 1);
          Import.Fields[Length(Import.Fields) - 1] := Table.Fields[I];
          SetLength(Import.SourceFields, Length(Import.SourceFields) + 1);
          Import.SourceFields[Length(Import.Fields) - 1].Name := FSourceFields[J].Text;
        end;
  end;

  Success := Assigned(Import);

  if (not Success) then
    SendMessage(Self.Handle, CM_EXECUTIONDONE, WPARAM(Success), 0)
  else
  begin
    Import.Wnd := Self.Handle;
    Import.OnError := OnError;
    Import.OnExecuted := OnExecuted;
    Import.OnUpdate := OnUpdate;
    if (Session.Asynchron) then
      Import.Start()
    else
      Import.Execute();
  end;
end;

procedure TDImport.TSFieldsChange(Sender: TObject);
var
  I: Integer;
begin
  TSImportType.Enabled := False;
  for I := 0 to Length(FFields) - 1 do
    if ((FSourceFields[I].Text <> '') and (FFields[I].ItemIndex > 0)) then
      TSImportType.Enabled := True;
  CheckActivePageChange(TSFields.PageIndex);
end;

procedure TDImport.TSFieldsShow(Sender: TObject);
begin
  TSFieldsChange(Sender);
end;

procedure TDImport.TSImportTypeShow(Sender: TObject);
var
  I: Integer;
  J: Integer;
  Selected: Boolean;
begin
  if (not Assigned(Table) or not Assigned(Table.Keys.PrimaryKey)) then
    FUpdate.Enabled := False
  else
  begin
    Selected := True;
    for I := 0 to Table.Keys.PrimaryKey.Columns.Count - 1 do
      if (Selected) then
      begin
        Selected := False;
        for J := 0 to Length(FFields) - 1 do
          if ((FSourceFields[J].Text <> '') and (FFields[J].ItemIndex = Table.Keys.PrimaryKey.Columns[I].Field.Index + 1)) then
            Selected := True;
      end;
    if (Selected) then
    begin
      Selected := False;
      for J := 0 to Length(FFields) - 1 do
        if ((FSourceFields[J].Text <> '') and (FFields[J].ItemIndex > 0) and not Table.Fields[FFields[J].ItemIndex - 1].InPrimaryKey) then
          Selected := True;
    end;

    FUpdate.Enabled := Selected;
  end;

  FImportTypeClick(Sender);
end;

procedure TDImport.TSODBCOptionsHide(Sender: TObject);
begin
  InitTSFields(Sender);
end;

procedure TDImport.TSODBCOptionsShow(Sender: TObject);
begin
  FObjects.Enabled := not Assigned(Table);
  FData.Enabled := not Assigned(Table);

  FObjects.Checked := FObjects.Checked and not Assigned(Table);
  FData.Checked := FObjects.Checked or Assigned(Table);

  GODBCDatabaseHow.Visible := not Assigned(Table);

  TSFields.Enabled := (FObjects.Checked or FData.Checked) and Assigned(Table);
  TSExecute.Enabled := not TSFields.Enabled;
  CheckActivePageChange(TSODBCOptions.PageIndex);
end;

procedure TDImport.TSTablesHide(Sender: TObject);
begin
  if (ImportType in [itExcelFile, itAccessFile, itODBC, itSQLiteFile, itXMLFile]) then
  begin
    ClearTSFields(Sender);
    if (TSFields.Enabled) then
      InitTSFields(Sender);
  end;
end;

procedure TDImport.TSTablesShow(Sender: TObject);
const
  TABLE_TYPE_LEN = 30;
var
  cbTABLE_NAME: SQLINTEGER;
  cbTABLE_TYPE: SQLINTEGER;
  Handle: SQLHSTMT;
  I: Integer;
  ListItem: TListItem;
  Stmt: sqlite3_stmt_ptr;
  TableName: string;
  TABLE_NAME: PSQLTCHAR;
  TABLE_NAME_LEN: SQLINTEGER;
  TABLE_TYPE: PSQLTCHAR;
begin
  if (FTables.Items.Count = 0) then
  begin
    TableNames.Clear();

    if (ODBC <> SQL_NULL_HANDLE) then
    begin
      ODBCException(ODBC, SQLAllocHandle(SQL_HANDLE_STMT, ODBC, @Handle));
      ODBCException(ODBC, SQLGetInfo(ODBC, SQL_MAX_TABLE_NAME_LEN, @TABLE_NAME_LEN, SizeOf(TABLE_NAME_LEN), nil));
      GetMem(TABLE_NAME, (TABLE_NAME_LEN + 1) * SizeOf(SQLWCHAR));
      GetMem(TABLE_TYPE, (TABLE_TYPE_LEN + 1) * SizeOf(SQLWCHAR));

      ODBCException(Handle, SQLTables(Handle, nil, 0, nil, 0, nil, 0, nil, 0));
      ODBCException(Handle, SQLBindCol(Handle, 3, SQL_C_WCHAR, TABLE_NAME, (TABLE_NAME_LEN + 1) * SizeOf(SQLWCHAR), @cbTABLE_NAME));
      ODBCException(Handle, SQLBindCol(Handle, 4, SQL_C_WCHAR, TABLE_TYPE, (TABLE_TYPE_LEN + 1) * SizeOf(SQLWCHAR), @cbTABLE_TYPE));
      while (SQL_SUCCEEDED(ODBCException(Handle, SQLFetch(Handle)))) do
        if ((lstrcmpi(PChar(TABLE_TYPE), 'TABLE') = 0) or (ImportType = itExcelFile) and (lstrcmpi(PChar(TABLE_TYPE), 'SYSTEM TABLE') = 0))  then
        begin
          SetString(TableName, PChar(TABLE_NAME), cbTABLE_NAME div SizeOf(SQLTCHAR));
          if ((ImportType <> itExcelFile) or (Pos('$', TableName) > 0)) then
            TableNames.Add(TableName);
        end;
      SQLFreeStmt(Handle, SQL_CLOSE);

      if ((ImportType = itExcelFile) and (TableNames.Count = 0)) then
      begin
        ODBCException(Handle, SQLTables(Handle, nil, 0, nil, 0, nil, 0, nil, 0));
        ODBCException(Handle, SQLBindCol(Handle, 3, SQL_C_WCHAR, TABLE_NAME, (TABLE_NAME_LEN + 1) * SizeOf(SQLWCHAR), @cbTABLE_NAME));
        ODBCException(Handle, SQLBindCol(Handle, 4, SQL_C_WCHAR, TABLE_TYPE, (TABLE_TYPE_LEN + 1) * SizeOf(SQLWCHAR), @cbTABLE_TYPE));
        while (SQL_SUCCEEDED(ODBCException(Handle, SQLFetch(Handle)))) do
          if (lstrcmpi(PChar(TABLE_TYPE), 'TABLE') = 0)  then
          begin
            SetString(TableName, PChar(TABLE_NAME), cbTABLE_NAME div SizeOf(SQLTCHAR));
            TableNames.Add(TableName);
          end;
        SQLFreeStmt(Handle, SQL_CLOSE);
      end;

      FreeMem(TABLE_NAME);
      FreeMem(TABLE_TYPE);
      SQLFreeHandle(SQL_HANDLE_STMT, Handle);
    end
    else if (ImportType = itSQLiteFile) then
    begin
      if (sqlite3_open_v2(PAnsiChar(UTF8Encode(Filename)), @SQLite, SQLITE_OPEN_READONLY, nil) <> SQLITE_OK) then
        MsgBox(Preferences.LoadStr(523, Filename), Preferences.LoadStr(45), MB_OK + MB_ICONERROR)
      else
      begin
        SQLiteException(SQLite, sqlite3_prepare_v2(SQLite, 'SELECT "name" FROM "sqlite_master" WHERE type=''table'' ORDER BY "name"', -1, @Stmt, nil));
        while (sqlite3_step(Stmt) = SQLITE_ROW) do
          TableNames.Add(UTF8ToString(sqlite3_column_text(Stmt, 0)));
        SQLiteException(SQLite, sqlite3_finalize(Stmt));
      end;
    end;

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

    FTables.MultiSelect := not Assigned(Table);
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

procedure TDImport.TSXMLOptionChange(Sender: TObject);
begin
  FBForward.Enabled :=
    (FRecordTag.Text <> '');
end;

procedure TDImport.TSXMLOptionsHide(Sender: TObject);
begin
  if (Length(FFields) = 0) then
    InitTSFields(Sender);
end;

procedure TDImport.TSXMLOptionsShow(Sender: TObject);
begin
  TSFields.Enabled := True;

  CheckActivePageChange(TSXMLOptions.PageIndex);
  TSXMLOptionChange(Sender);
end;

initialization
  FImport := nil;
end.

