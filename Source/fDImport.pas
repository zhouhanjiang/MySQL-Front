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
    FCharset: TComboBox_Ext;
    FCollation: TComboBox_Ext;
    FCSVHeadline: TCheckBox;
    FCSVPreview: TListView;
    FDaily: TRadioButton;
    FData: TCheckBox;
    FDataSource: TEdit;
    FDelimiter: TEdit;
    FDelimiterChar: TRadioButton;
    FDelimiterTab: TRadioButton;
    FDestinationField1: TComboBox_Ext;
    FDoneObjects: TLabel;
    FDoneRecords: TLabel;
    FDoneTime: TLabel;
    FEnabled: TCheckBox;
    FEngine: TComboBox_Ext;
    FEntieredObjects: TLabel;
    FEntieredRecords: TLabel;
    FEntieredTime: TLabel;
    FErrorMessages: TRichEdit;
    FErrors: TLabel;
    FExcelFile: TRadioButton;
    FFilename: TEdit;
    FInsert: TRadioButton;
    FInsertOrUpdate: TRadioButton;
    FLCharset: TLabel;
    FLCollation: TLabel;
    FLCSVHeadline: TLabel;
    FLDataSource: TLabel;
    FLDelimiter: TLabel;
    FLDestinationFields: TLabel;
    FLDone: TLabel;
    FLEnabled: TLabel;
    FLEngine: TLabel;
    FLEntiered: TLabel;
    FLErrors: TLabel;
    FLExecution: TLabel;
    FLFilename: TLabel;
    FLImportType: TLabel;
    FLName: TLabel;
    FLProgressObjects: TLabel;
    FLProgressRecords: TLabel;
    FLProgressTime: TLabel;
    FLQuoteValues: TLabel;
    FLReferrer1: TLabel;
    FLRowFormat: TLabel;
    FLSourceFields: TLabel;
    FLStart: TLabel;
    FLStmtType: TLabel;
    FLWhat: TLabel;
    FMonthly: TRadioButton;
    FName: TEdit;
    FODBC: TRadioButton;
    FProgressBar: TProgressBar;
    FQuoteChar: TEdit;
    FQuoteNothing: TRadioButton;
    FQuoteStrings: TRadioButton;
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
    GBasics: TGroupBox_Ext;
    GCSVHow: TGroupBox_Ext;
    GCSVPreview: TGroupBox_Ext;
    GErrorMessages: TGroupBox_Ext;
    GFields: TGroupBox_Ext;
    GProgress: TGroupBox_Ext;
    GSelect: TGroupBox_Ext;
    GStmtType: TGroupBox_Ext;
    GStructure: TGroupBox_Ext;
    GTables: TGroupBox_Ext;
    GTask: TGroupBox_Ext;
    GWhat: TGroupBox_Ext;
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
    TSJob: TTabSheet;
    TSSelect: TTabSheet;
    TSStmtType: TTabSheet;
    TSTables: TTabSheet;
    TSTask: TTabSheet;
    TSWhat: TTabSheet;
    FLInsertUpdate: TLabel;
    procedure FBBackClick(Sender: TObject);
    procedure FBCancelClick(Sender: TObject);
    procedure FBDataSourceClick(Sender: TObject);
    procedure FBFilenameClick(Sender: TObject);
    procedure FBForwardClick(Sender: TObject);
    procedure FBHelpClick(Sender: TObject);
    procedure FCharsetChange(Sender: TObject);
    procedure FCSVKeyPress(Sender: TObject; var Key: Char);
    procedure FCSVPreviewUpdate(Sender: TObject);
    procedure FDataSourceChange(Sender: TObject);
    procedure FDelimiterClick(Sender: TObject);
    procedure FDelimiterKeyPress(Sender: TObject; var Key: Char);
    procedure FFieldExit(Sender: TObject);
    procedure FFilenameChange(Sender: TObject);
    procedure StmtTypeChange(Sender: TObject);
    procedure FJobOptionChange(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure FormCreate(Sender: TObject);
    procedure FormHide(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure FQuoteClick(Sender: TObject);
    procedure FQuoteKeyPress(Sender: TObject; var Key: Char);
    procedure FSelectChange(Sender: TObject; Node: TTreeNode);
    procedure FSelectExpanding(Sender: TObject; Node: TTreeNode;
      var AllowExpansion: Boolean);
    procedure FSelectGetImageIndex(Sender: TObject; Node: TTreeNode);
    procedure FStmtTypeClick(Sender: TObject);
    procedure FStmtTypeKeyPress(Sender: TObject; var Key: Char);
    procedure FTablesChange(Sender: TObject; Item: TListItem;
      Change: TItemChange);
    procedure FTablesDblClick(Sender: TObject);
    procedure FTablesSelectItem(Sender: TObject; Item: TListItem;
      Selected: Boolean);
    procedure ScrollBoxResize(Sender: TObject);
    procedure TSCSVOptionsHide(Sender: TObject);
    procedure TSExecuteShow(Sender: TObject);
    procedure TSFieldsChange(Sender: TObject);
    procedure TSFieldsShow(Sender: TObject);
    procedure TSJobHide(Sender: TObject);
    procedure TSJobShow(Sender: TObject);
    procedure TSSelectHide(Sender: TObject);
    procedure TSSelectShow(Sender: TObject);
    procedure TSTablesHide(Sender: TObject);
    procedure TSTablesShow(Sender: TObject);
    procedure TSTaskShow(Sender: TObject);
    procedure TSWhatShow(Sender: TObject);
    procedure TSXMLOptionsHide(Sender: TObject);
    procedure WhatClick(Sender: TObject);
    procedure WhatKeyPress(Sender: TObject; var Key: Char);
    procedure TSStmtTypeShow(Sender: TObject);
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
    FDestinationFields: array of TComboBox_Ext;
    FLReferrers: array of TLabel;
    FSourceFields: array of TEdit;
    Import: TTImport;
    ProgressInfos: TTool.TProgressInfos;
    TableNames: TTableNames;
    WantedNodeExpand: TTreeNode;
    procedure CheckActivePageChange(const ActivePageIndex: Integer);
    procedure ClearTSFields(Sender: TObject);
    procedure CMSysFontChanged(var Message: TMessage); message CM_SYSFONTCHANGED;
    procedure FormSessionEvent(const Event: TSSession.TEvent);
    function GetDataSource(): Boolean;
    function GetFilename(): Boolean;
    procedure InitTSFields(Sender: TObject);
    function InitTSSelect(): Boolean;
    procedure OnError(const Sender: TObject; const Error: TTool.TError; const Item: TTool.TItem; const ShowRetry: Boolean; var Success: TDataAction);
    procedure OnTerminate(Sender: TObject);
    procedure OnUpdate(const AProgressInfos: TTool.TProgressInfos);
    procedure UMChangePreferences(var Message: TMessage); message UM_CHANGEPREFERENCES;
    procedure UMPostAfterExecuteSQL(var Message: TMessage); message UM_POST_AFTEREXECUTESQL;
    procedure UMPostShow(var Message: TMessage); message UM_POST_SHOW;
    procedure UMTerminate(var Message: TMessage); message UM_TERMINATE;
    procedure UMUpdateProgressInfo(var Message: TMessage); message UM_UPDATEPROGRESSINFO;
    procedure WMHelp(var Message: TWMHelp); message WM_HELP;
  public
    CodePage: Cardinal;
    DialogType: (idtNormal, idtCreateJob, idtEditJob, idtExecuteJob);
    Filename: TFileName;
    ImportType: TAAccount.TJobImport.TImportType;
    Job: TAAccount.TJobImport;
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
    FImport.Perform(UM_CHANGEPREFERENCES, 0, 0);
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
  else if (NextActivePageIndex >= 0) then
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
  for I := 0 to Length(FSourceFields) - 1 do
  begin
    FSourceFields[I].Free();
    FSourceFields[I] := nil;
  end;
  SetLength(FSourceFields, 0);
  for I := 0 to Length(FLReferrers) - 1 do
  begin
    FLReferrers[I].Free();
    FLReferrers[I] := nil;
  end;
  SetLength(FLReferrers, 0);
  for I := 0 to Length(FDestinationFields) - 1 do
  begin
    FDestinationFields[I].Free();
    FDestinationFields[I] := nil;
  end;
  SetLength(FDestinationFields, 0);
end;

procedure TDImport.CMSysFontChanged(var Message: TMessage);
begin
  inherited;

  FBFilename.Height := FFilename.Height;
  FBDataSource.Height := FFilename.Height;

  FDelimiter.Left := FDelimiterChar.Left + Sizer.Width + PageControl.Canvas.TextWidth(FDelimiterChar.Caption);
  FQuoteChar.Left := FQuoteStrings.Left + Sizer.Width + PageControl.Canvas.TextWidth(FQuoteStrings.Caption);
end;

function TDImport.Execute(): Boolean;
begin
  ModalResult := mrNone;
  PageControl.ActivePageIndex := -1;

  if ((ImportType in [itSQLFile, itTextFile, itAccessFile, itExcelFile]) and (Filename = '') and (DialogType = idtNormal)) then
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
  if (Assigned(Import)) then
  begin
    Import.Terminate();
    FBCancel.Enabled := False;
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
    FCSVPreview.DisableAlign(); FCSVPreview.Items.BeginUpdate();
    FCSVPreview.Columns.Clear();
    FCSVPreview.Items.Clear();

    if (Assigned(Import)) then
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
      while ((FCSVPreview.Items.Count < 10) and TTImportText(Import).GetPreviewValues(nil, Values)) do
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
  Charset: TSCharset;
  I: Integer;
begin
  Charset := Session.CharsetByName(FCharset.Text);

  FCollation.Items.Clear();
  if (Assigned(Session.Collations)) then
  begin
    for I := 0 to Session.Collations.Count - 1 do
      if (Assigned(Charset) and (Session.Collations[I].Charset = Charset)) then
        FCollation.Items.Add(Session.Collations[I].Name);
    if (Assigned(Charset)) then
      FCollation.ItemIndex := FCollation.Items.IndexOf(Charset.DefaultCollation.Caption);
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
  for I := 0 to Length(FDestinationFields) - 1 do
    if ((Sender is TComboBox_Ext) and (FDestinationFields[I] <> Sender) and (FDestinationFields[I].ItemIndex = TComboBox_Ext(Sender).ItemIndex)) then
      FDestinationFields[I].ItemIndex := 0;
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

procedure TDImport.StmtTypeChange(Sender: TObject);
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
  else
    ImportType := itUnknown;

  FFilename.Visible := ImportType in [itSQLFile, itTextFile, itExcelFile, itAccessFile];
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
  Constraints.MinWidth := Width;
  Constraints.MinHeight := Height;

  BorderStyle := bsSizeable;

  if ((Preferences.Import.Width >= Width) and (Preferences.Import.Height >= Height)) then
  begin
    Width := Preferences.Import.Width;
    Height := Preferences.Import.Height;
  end;

  FSelect.Images := Preferences.SmallImages;
  FTables.SmallImages := Preferences.SmallImages;

  FCSVHeadline.Checked := Preferences.Import.CSV.Headline;
  FDelimiterTab.Checked := Preferences.Import.CSV.DelimiterType = dtTab;
  FDelimiterChar.Checked := Preferences.Import.CSV.DelimiterType = dtChar;
  FDelimiter.Text := Preferences.Import.CSV.Delimiter;
  FQuoteNothing.Checked := Preferences.Import.CSV.Quote = qtNone;
  FQuoteStrings.Checked := Preferences.Import.CSV.Quote = qtStrings;
  FQuoteChar.Text := Preferences.Import.CSV.QuoteChar;

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
  NewJob: TAAccount.TJobImport;
begin
  Session.UnRegisterEventProc(FormSessionEvent);

  Preferences.Import.Width := Width;
  Preferences.Import.Height := Height;

  if (Assigned(Import)) then
    Import.Free();

  if (ModalResult = mrOk) then
    if (DialogType in [idtNormal]) then
    begin
      case (ImportType) of
        itTextFile:
          begin
            Preferences.Import.CSV.Headline := FCSVHeadline.Checked;
            if (FDelimiterTab.Checked) then
              Preferences.Import.CSV.DelimiterType := dtTab
            else if (FDelimiterChar.Checked) then
              Preferences.Import.CSV.DelimiterType := dtChar;
            Preferences.Import.CSV.Delimiter := FDelimiter.Text;
            Preferences.Import.CSV.QuoteChar := FQuoteChar.Text;
            if (FQuoteNothing.Checked) then
              Preferences.Import.CSV.Quote := qtNone
            else
              Preferences.Import.CSV.Quote := qtStrings;
          end;
        itODBC:
          begin
            Preferences.Import.Structure := FStructure.Checked;
            Preferences.Import.Data := FData.Checked;
          end;
      end;

      if (FReplace.Checked) then
        Preferences.Import.StmtType := stReplace
      else if (FUpdate.Checked) then
        Preferences.Import.StmtType := stUpdate
      else if (FInsertOrUpdate.Checked) then
        Preferences.Import.StmtType := stInsertOrUpdate
      else
        Preferences.Import.StmtType := stInsert;
    end
    else
    begin
      NewJob := TAAccount.TJobImport.Create(Session.Account.Jobs, Trim(FName.Text));

      case (ImportType) of
        itTextFile:
          begin
            NewJob.CSV.Headline := FCSVHeadline.Checked;
            if (FDelimiterTab.Checked) then
              NewJob.CSV.DelimiterType := dtTab
            else if (FDelimiterChar.Checked) then
              NewJob.CSV.DelimiterType := dtChar;
            NewJob.CSV.Delimiter := FDelimiter.Text;
            NewJob.CSV.QuoteChar := FQuoteChar.Text;
            if (FQuoteNothing.Checked) then
              NewJob.CSV.Quote := qtNone
            else
              NewJob.CSV.Quote := qtStrings;
          end;
        itODBC:
          begin
            NewJob.Structure := FStructure.Checked;
            NewJob.Data := FData.Checked;
          end;
      end;

      if (FReplace.Checked) then
        NewJob.StmtType := stReplace
      else if (FUpdate.Checked) then
        NewJob.StmtType := stUpdate
      else if (FInsertOrUpdate.Checked) then
        NewJob.StmtType := stInsertOrUpdate
      else
        NewJob.StmtType := stInsert;

      if (DialogType in [idtCreateJob, idtEditJob]) then
      begin
        if (Assigned(FSelect.Selected)) then
          if (not Assigned(FSelect.Selected.Parent)) then
            NewJob.JobObject.ObjectType := jotServer
          else if (TObject(FSelect.Selected.Data) is TSDatabase) then
          begin
            NewJob.JobObject.ObjectType := jotDatabase;
            NewJob.JobObject.Name := TSDatabase(FSelect.Selected.Data).Name;
          end
          else if (TObject(FSelect.Selected.Data) is TSDBObject) then
          begin
            if (TObject(FSelect.Selected.Data) is TSTable) then
              NewJob.JobObject.ObjectType := jotTable;
            NewJob.JobObject.Name := TSDBObject(FSelect.Selected.Data).Name;
            NewJob.JobObject.DatabaseName := TSDBObject(FSelect.Selected.Data).Database.Name;
          end;
        NewJob.CodePage := CodePage;
        NewJob.ImportType := ImportType;
        NewJob.Filename := FFilename.Text;
        NewJob.ODBC.DataSource := FDataSource.Text;

        SetLength(NewJob.SourceObjects, 0);
        case (ImportType) of
          itAccessFile,
          itExcelFile,
          itODBC:
            for I := 0 to FTables.Items.Count - 1 do
              if (FTables.Items[I].Selected) then
              begin
                SetLength(NewJob.SourceObjects, Length(NewJob.SourceObjects) + 1);
                NewJob.SourceObjects[Length(NewJob.SourceObjects) - 1].Name := TTableName(FTables.Items[I].Data).SourceName;
              end;
        end;

        SetLength(NewJob.FieldMappings, 0);
        if (SObject is TSTable) then
          for I := 0 to TSTable(SObject).Fields.Count - 1 do
            for J := 0 to Length(FDestinationFields) - 1 do
              if ((FSourceFields[J].Text <> '') and (FDestinationFields[J].ItemIndex = I + 1)) then
              begin
                SetLength(NewJob.FieldMappings, Length(NewJob.FieldMappings) + 1);
                NewJob.FieldMappings[Length(NewJob.FieldMappings) - 1].DestinationFieldName := TSTable(SObject).Fields[I].Name;
                NewJob.FieldMappings[Length(NewJob.FieldMappings) - 1].SourceFieldName := FSourceFields[J].Text;
              end;

        DecodeDate(FStartDate.Date, Year, Month, Day);
        DecodeTime(FStartTime.Time, Hour, Min, Sec, MSec);
        NewJob.Start := EncodeDate(Year, Month, Day) + EncodeTime(Hour, Min, Sec, MSec);
        if (FDaily.Checked) then
          NewJob.TriggerType := ttDaily
        else if (FWeekly.Checked) then
          NewJob.TriggerType := ttWeekly
        else if (FMonthly.Checked) then
          NewJob.TriggerType := ttMonthly
        else
          NewJob.TriggerType := ttSingle;
        NewJob.Enabled := FEnabled.Checked;

        if (DialogType = idtCreateJob) then
          Session.Account.Jobs.AddJob(NewJob)
        else if (DialogType = idtEditJob) then
          Session.Account.Jobs.UpdateJob(Job, NewJob);
        NewJob.Free();
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
  if (Event.EventType = etAfterExecuteSQL) then
    PostMessage(Handle, UM_POST_AFTEREXECUTESQL, 0, 0);
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
  FCharset.Visible := Session.Connection.ServerVersion >= 40101; FLCharset.Visible := FCharset.Visible;

  FCollation.Items.Clear();
  if (Session.Charsets.Count = 0) then
    FCollation.Style := csDropDown
  else
  begin
    FCollation.Style := csDropDownList;
    for I := 0 to Session.Charsets.Count - 1 do
      FCollation.Items.Add(Session.Charsets.Charset[I].Name);
  end;
  FCollation.Visible := Session.Connection.ServerVersion >= 40101; FLCollation.Visible := FCollation.Visible;

  FUpdate.Enabled := (SObject is TSBaseTable) and Assigned(TSBaseTable(SObject).PrimaryKey);
  FInsertOrUpdate.Enabled := (ImportType = itTextFile) and FUpdate.Enabled; FLInsertUpdate.Enabled := FInsertOrUpdate.Enabled;

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
    FFilename.Text := '';
    FDataSource.Text := '';
    FJobOptionChange(Sender);

    FStructure.Checked := Preferences.Import.Structure;
    FData.Checked := Preferences.Import.Data;
    FEngine.ItemIndex := FEngine.Items.IndexOf(Preferences.Import.Engine);
    FCharset.ItemIndex := FCharset.Items.IndexOf(Preferences.Import.Charset);
    FRowFormat.ItemIndex := Preferences.Import.RowType;
    case (Preferences.Import.StmtType) of
      stReplace: FReplace.Checked := True;
      stUpdate: FUpdate.Checked := True;
      stInsertOrUpdate: FInsertOrUpdate.Checked := True;
      else FInsert.Checked := True;
    end;

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
    end;
    FDataSource.Text := Job.ODBC.DataSource;
    FFilename.Text := Job.Filename;
    FJobOptionChange(Sender);
    DODBC.DataSource := Job.ODBC.DataSource;
    DODBC.Username := Job.ODBC.Username;
    DODBC.Password := Job.ODBC.Password;

    FCSVHeadline.Checked := Job.CSV.Headline;
    FDelimiterTab.Checked := Job.CSV.DelimiterType = dtTab;
    FDelimiterChar.Checked := Job.CSV.DelimiterType = dtChar;
    FDelimiter.Text := Job.CSV.Delimiter;
    FQuoteNothing.Checked := Job.CSV.Quote = qtNone;
    FQuoteStrings.Checked := Job.CSV.Quote = qtStrings;
    FQuoteChar.Text := Job.CSV.QuoteChar;

    FStructure.Checked := Job.Structure;
    FData.Checked := Job.Data;
    FEngine.ItemIndex := FEngine.Items.IndexOf(Job.Engine);
    FCharset.ItemIndex := FCharset.Items.IndexOf(Job.Charset);
    FRowFormat.ItemIndex := Job.RowType;
    case (Job.StmtType) of
      stReplace: FReplace.Checked := True;
      stUpdate: FUpdate.Checked := True;
      stInsertOrUpdate: FInsertOrUpdate.Checked := True;
      else FInsert.Checked := True;
    end;

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
      case (Preferences.Import.StmtType) of
        stReplace: FReplace.Checked := True;
        stUpdate: FUpdate.Checked := True;
        stInsertOrUpdate: FInsertOrUpdate.Checked := True;
        else FInsert.Checked := True;
      end;
    end;

    TSJobHide(Sender);
  end;
  FName.Enabled := DialogType = idtCreateJob;

  if ((FEngine.ItemIndex < 0) and Assigned(Session.Engines.DefaultEngine)) then
    FEngine.ItemIndex := FEngine.Items.IndexOf(Session.Engines.DefaultEngine.Name);

  if (FCharset.ItemIndex < 0) then
    if (SObject is TSDatabase) then
      FCharset.ItemIndex := FCharset.Items.IndexOf(TSDatabase(SObject).DefaultCharset)
    else if (SObject is TSDBObject) then
      FCharset.ItemIndex := FCharset.Items.IndexOf(TSDBObject(SObject).Database.DefaultCharset)
    else if (Session.Connection.ServerVersion < 40101) then
      FCharset.ItemIndex := FCharset.Items.IndexOf(Session.DefaultCharset)
    else
      FCharset.ItemIndex := FCharset.Items.IndexOf('utf8');
  FCharsetChange(Sender);

  TSJob.Enabled := DialogType in [idtCreateJob, idtEditJob];
  TSTables.Enabled := (DialogType in [idtNormal, idtCreateJob, idtEditJob]) and (ImportType in [itAccessFile, itExcelFile, itODBC]);
  TSSelect.Enabled := DialogType in [idtEditJob];
  TSCSVOptions.Enabled := (DialogType in [idtNormal]) and (ImportType in [itTextFile]);
  TSWhat.Enabled := False;
  TSFields.Enabled := False;
  TSStmtType.Enabled := False;
  TSTask.Enabled := False;
  TSExecute.Enabled := False;

  if ((DialogType in [idtExecuteJob]) or (ImportType in [itSQLFile])) then
    PostMessage(Handle, UM_POST_SHOW, 0, 0);

  if (TSFields.Enabled) then
    InitTSFields(Sender);

  for I := 0 to PageControl.PageCount - 1 do
    if ((PageControl.ActivePageIndex < 0) and PageControl.Pages[I].Enabled) then
      PageControl.ActivePageIndex := I;
  CheckActivePageChange(PageControl.ActivePageIndex);

  if (DialogType in [idtCreateJob, idtEditJob]) then
  begin
    PageControl.Visible := Session.Databases.Update() and Boolean(Perform(UM_POST_AFTEREXECUTESQL, 0, 0));
    if (PageControl.Visible) then
      FSelect.Items.GetFirstNode().Expand(False)
    else
      WantedNodeExpand := FSelect.Items.GetFirstNode();
  end
  else
    PageControl.Visible := Boolean(Perform(UM_POST_AFTEREXECUTESQL, 0, 0));
  PSQLWait.Visible := not PageControl.Visible;

  FBBack.Visible := (DialogType in [idtNormal, idtCreateJob, idtEditJob]) and not (ImportType in [itSQLFile]);
  FBForward.Visible := FBBack.Visible;
  FBCancel.Enabled := True;
  FBCancel.ModalResult := mrCancel;
  FBCancel.Caption := Preferences.LoadStr(30);

  CheckActivePageChange(PageControl.ActivePageIndex);
  if (PageControl.Visible and TSJob.Visible and FName.Enabled) then
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

  TSCSVOptions.Enabled := (ImportType in [itTextFile]) and (SObject is TSTable);
  TSWhat.Enabled := (ImportType in [itTextFile, itAccessFile, itExcelFile, itODBC]) and (SObject is TSDatabase) and not TSCSVOptions.Enabled;
  TSFields.Enabled := (DialogType in [idtNormal, idtCreateJob, idtEditJob]) and not TSCSVOptions.Enabled and not TSWhat.Enabled and (SObject is TSTable);
  TSTask.Enabled := (DialogType <> idtNormal) and (ImportType in [itSQLFile]) and not (TSCSVOptions.Enabled or TSWhat.Enabled or TSFields.Enabled);

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
            if (not Session.Databases.Update()) then
              WantedNodeExpand := Node
            else
            begin
              for I := 0 to Session.Databases.Count - 1 do
                if (((Session.Databases.NameCmp(Session.Databases[I].Name, 'mysql') <> 0) or (Session.Databases.NameCmp(Session.Databases[I].Name, 'sys') <> 0) and (Session.Connection.ServerVersion >= 50707)) and not (Session.Databases[I] is TSSystemDatabase)) then
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
  TSTask.Enabled := (DialogType <> idtNormal) and (FInsert.Checked and FInsert.Enabled or FReplace.Checked and FReplace.Enabled or FUpdate.Checked and FUpdate.Enabled or FInsertOrUpdate.Checked and FInsertOrUpdate.Enabled);
  TSExecute.Enabled := (DialogType = idtNormal) and (FInsert.Checked and FInsert.Enabled or FReplace.Checked and FReplace.Enabled or FUpdate.Checked and FUpdate.Enabled or FInsertOrUpdate.Checked and FInsertOrUpdate.Enabled);
  CheckActivePageChange(TSStmtType.PageIndex);
end;

procedure TDImport.FStmtTypeKeyPress(Sender: TObject; var Key: Char);
begin
  FStmtTypeClick(Sender);
end;

procedure TDImport.FTablesChange(Sender: TObject; Item: TListItem;
  Change: TItemChange);
begin
  TSSelect.Enabled := (DialogType in [idtCreateJob, idtEditJob, idtExecuteJob]) and Assigned(FTables.Selected);
  TSCSVOptions.Enabled := (ImportType in [itTextFile]) and (SObject is TSTable);
  TSWhat.Enabled := (ImportType in [itTextFile, itAccessFile, itExcelFile, itODBC]) and (SObject is TSDatabase) and not TSCSVOptions.Enabled;
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
  TSWhat.Enabled := (FTables.SelCount > 0) and (ImportType in [itTextFile, itAccessFile, itExcelFile, itODBC]) and not (SObject is TSBaseTable);
  TSFields.Enabled := (FTables.SelCount > 0) and not TSWhat.Enabled;
  CheckActivePageChange(TSTables.PageIndex);
end;

function TDImport.GetDataSource(): Boolean;
begin
  Result := DODBC.Execute();
end;

function TDImport.GetFilename(): Boolean;
begin
  OpenDialog.Title := Preferences.LoadStr(581);
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
  if (TSFields.Enabled or not Assigned(Sender)) then
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

      if (not Assigned(Import)) then TSJobHide(nil);
      TTImportODBC(Import).GetFieldNames(TTableName(FTables.Selected.Data).SourceName, FieldNames);
    end;

    if (SObject is TSBaseTable) then
    begin
      Session.Connection.BeginSynchron();
      TSBaseTable(SObject).Update();
      Session.Connection.EndSynchron();

      ScrollBox.DisableAlign();

      SetLength(FSourceFields, FieldNames.Count);
      for I := 0 to Length(FSourceFields) - 1 do FSourceFields[I] := nil;
      SetLength(FLReferrers, Length(FSourceFields));
      for I := 0 to Length(FLReferrers) - 1 do FLReferrers[I] := nil;
      SetLength(FDestinationFields, Length(FSourceFields));
      for I := 0 to Length(FDestinationFields) - 1 do FDestinationFields[I] := nil;

      for I := 0 to Length(FSourceFields) - 1 do
      begin
        FSourceFields[I] := TEdit.Create(ScrollBox);
        FSourceFields[I].Parent := ScrollBox;
        FSourceFields[I].Left := FSourceField1.Left;
        FSourceFields[I].Top := FSourceField1.Top + I * (FSourceField2.Top - FSourceField1.Top);
        FSourceFields[I].Width := FSourceField1.Width;
        FSourceFields[I].Height := FSourceField1.Height;
        FSourceFields[I].Enabled := False;
        FSourceFields[I].Text := FieldNames[I];
        FSourceFields[I].OnChange := FSourceField1.OnChange;

        FLReferrers[I] := TLabel.Create(ScrollBox);
        FLReferrers[I].Parent := ScrollBox;
        FLReferrers[I].Left := FLReferrer1.Left;
        FLReferrers[I].Top := FLReferrer1.Top + I * (FSourceField2.Top - FSourceField1.Top);
        FLReferrers[I].Width := FLReferrer1.Width;
        FLReferrers[I].Height := FLReferrer1.Height;
        FLReferrers[I].Caption := FLReferrer1.Caption;

        FDestinationFields[I] := TComboBox_Ext.Create(ScrollBox);
        FDestinationFields[I].Parent := ScrollBox;
        FDestinationFields[I].Left := FDestinationField1.Left;
        FDestinationFields[I].Top := FDestinationField1.Top + I * (FSourceField2.Top - FSourceField1.Top);
        FDestinationFields[I].Width := FDestinationField1.Width;
        FDestinationFields[I].Height := FDestinationField1.Height;
        FDestinationFields[I].Style := FDestinationField1.Style;
        FDestinationFields[I].Enabled := not FSourceFields[I].Enabled;
        if (I = 0) then
        begin
          FDestinationFields[I].Items.Add('');
          for J := 0 to TSBaseTable(SObject).Fields.Count - 1 do
            if (TSBaseTable(SObject).Fields[J].FieldKind = mkReal) then
              FDestinationFields[I].Items.Add(TSBaseTable(SObject).Fields.Field[J].Name);
        end
        else
          FDestinationFields[I].Items.Text := FDestinationFields[0].Items.Text;
        if (DialogType in [idtNormal, idtCreateJob]) then
          if ((ImportType in [itTextFile]) and FCSVHeadline.Checked or (ImportType in [itExcelFile, itAccessFile, itODBC])) then
            FDestinationFields[I].ItemIndex := FDestinationFields[I].Items.IndexOf(FSourceFields[I].Text)
          else
            FDestinationFields[I].ItemIndex := I + 1
        else if (DialogType in [idtEditJob]) then
          FDestinationFields[I].ItemIndex := FDestinationFields[I].Items.IndexOf(Job.FieldMappings[I].DestinationFieldName);
        FDestinationFields[I].OnChange := FDestinationField1.OnChange;
        FDestinationFields[I].OnExit := FDestinationField1.OnExit;
      end;

      ScrollBoxResize(ScrollBox);
      ScrollBox.EnableAlign();

      TSFieldsChange(Sender);
      FFieldExit(Sender);
    end;

    FieldNames.Free();

    ScrollBoxResize(ScrollBox);
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
    Session.Connection.BeginSynchron();

    Nodes := TList.Create();
    if (not Session.Databases.Update()) then
      Result := False
    else
      if (Job.JobObject.ObjectType = jotServer) then
        Nodes.Add(FSelect.Items[0])
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
              if (not Database.Tables.Update()) then
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

    Session.Connection.EndSynchron();
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
        Msg := Preferences.LoadStr(165, IntToStr(Error.Session.Connection.ErrorCode), Error.Session.Connection.ErrorMessage);
        ErrorMsg := Error.ErrorMessage;
        if (Error.ErrorCode > 0) then
          ErrorMsg := ErrorMsg + ' (#' + IntToStr(Error.Session.Connection.ErrorCode) + ')';
        ErrorMsg := ErrorMsg + ' - ' + Trim(Session.Connection.CommandText);
      end;
    TE_File:
      begin
        Msg := Error.ErrorMessage;
        ErrorMsg := Msg;
        if (Error.ErrorCode = ERROR_NO_UNICODE_TRANSLATION) then
          MsgBoxHelpContext := 1150;
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
    if (MsgBoxHelpContext <> 0) then
      Flags := Flags or MB_HELP;
    DisableApplicationActivate := True;
    case (MsgBox(Msg, Preferences.LoadStr(45), Flags, Handle)) of
      IDOK,
      IDCANCEL,
      IDABORT: Success := daAbort;
      IDRETRY,
      IDTRYAGAIN: Success := daRetry;
      IDCONTINUE,
      IDIGNORE: Success := daFail;
    end;
    DisableApplicationActivate := False;
  end;

  if (((Success in [daAbort, daFail]) or (Error.ErrorType = TE_Warning)) and (ErrorMsg <> '')) then
  begin
    FErrors.Caption := IntToStr(TTool(Sender).ErrorCount);
    FErrorMessages.Lines.Add(ErrorMsg);
  end;
end;

procedure TDImport.OnTerminate(Sender: TObject);
begin
  PostMessage(Handle, UM_TERMINATE, WPARAM(not Import.Terminated), 0);
end;

procedure TDImport.OnUpdate(const AProgressInfos: TTool.TProgressInfos);
begin
  MoveMemory(@ProgressInfos, @AProgressInfos, SizeOf(AProgressInfos));

  PostMessage(Handle, UM_UPDATEPROGRESSINFO, 0, LPARAM(@ProgressInfos))
end;

procedure TDImport.ScrollBoxResize(Sender: TObject);
var
  I: Integer;
  Msg: TMsg;
  Padding: Integer;
begin
  if (not (PeekMessage(Msg, 0, 0, 0, PM_NOREMOVE) and (Msg.Message = WM_MOUSEMOVE) and (Msg.wParam = MK_LBUTTON))) then
  begin
    Padding := ScrollBox.Left + FSourceField1.Left;

    for I := 0 to Length(FSourceFields) - 1 do
      if ((Length(FSourceFields) > I) and Assigned(FSourceFields[I])) then
        FSourceFields[I].Width :=
          (ScrollBox.ClientWidth
            - FLReferrer1.Canvas.TextWidth(FLReferrer1.Caption)
            - 3 * Padding
            - GetSystemMetrics(SM_CXVSCROLL)) div 2;

    for I := 0 to Length(FLReferrers) - 1 do
      if ((Length(FLReferrers) > I) and Assigned(FLReferrers[I])) then
        FLReferrers[I].Left := FSourceFields[I].Left + FSourceFields[I].Width + Padding;

    for I := 0 to Length(FDestinationFields) - 1 do
      if ((Length(FDestinationFields) > I) and Assigned(FDestinationFields[I]) and (Length(FLReferrers) > 0) and Assigned(FLReferrers[I]) and (Length(FSourceFields) > 0) and Assigned(FSourceFields[I])) then
      begin
        FDestinationFields[I].Left := FLReferrers[I].Left + FLReferrers[I].Width + Padding;
        FDestinationFields[I].Width := FSourceFields[I].Width;
      end;

    if ((Length(FDestinationFields) > 0) and (Assigned(FDestinationFields[0]))) then
      FLDestinationFields.Left := FDestinationFields[0].Left + 6;
  end;
end;

procedure TDImport.TSCSVOptionsHide(Sender: TObject);
begin
  if (Length(FDestinationFields) = 0) then
    InitTSFields(Sender);
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
      Import.AddTable(TableName, SourceTableName);
  end;

var
  I: Integer;
  Success: Boolean;
begin
  if (DialogType = idtExecuteJob) then
    InitTSFields(nil);

  Session.UnRegisterEventProc(FormSessionEvent);

  FLProgressObjects.Visible := ImportType in [itODBC, itAccessFile, itExcelFile];
  FEntieredObjects.Visible := FLProgressObjects.Visible;
  FDoneObjects.Visible := FLProgressObjects.Visible;
  if (ImportType in [itODBC, itAccessFile, itExcelFile]) then
    FLProgressRecords.Caption := Preferences.LoadStr(235) + ':'
  else
    FLProgressRecords.Caption := Preferences.LoadStr(67) + ':';

  FEntieredObjects.Caption := '';
  FDoneObjects.Caption := '';
  FEntieredRecords.Caption := '';
  FDoneRecords.Caption := '';
  FEntieredTime.Caption := '';
  FDoneTime.Caption := '';
  FProgressBar.Position := 0;
  FErrors.Caption := '0';
  FErrorMessages.Lines.Clear();

  FBBack.Enabled := False;
  FBForward.Enabled := False;
  FBForward.Default := False;
  FBCancel.Enabled := True;
  FBCancel.Default := True;
  FBCancel.ModalResult := mrNone;

  ActiveControl := FBCancel;

  if (not Assigned(Import)) then
    TSJobHide(Self);

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
  end;

  Success := (Answer <> IDCANCEL);
  if (not Success) then
    SendMessage(Self.Handle, UM_TERMINATE, WPARAM(Success), 0)
  else
  begin
    Import.Data := (SObject is TSTable) or not (SObject is TSTable) and FData.Checked;
    Import.DefaultCharset := FCharset.Text;
    Import.DefaultCollation := FCollation.Text;
    Import.Engine := FEngine.Text;
    if (Import.Structure) then
      Import.StmtType := stInsert
    else if (FReplace.Checked) then
      Import.StmtType := stReplace
    else if (FUpdate.Checked and FUpdate.Enabled) then
      Import.StmtType := stUpdate
    else if (FInsertOrUpdate.Checked and FInsertOrUpdate.Enabled) then
      Import.StmtType := stInsertOrUpdate
    else
      Import.StmtType := stInsert;
    Import.RowType := TSTableField.TRowType(FRowFormat.ItemIndex);
    Import.Structure := not (SObject is TSTable) and FStructure.Checked;

    if (SObject is TSBaseTable) then
    begin
      TSBaseTable(SObject).InvalidateData();
      if (DialogType = idtNormal) then
      begin
        for I := 0 to Length(FSourceFields) - 1 do
          if (FDestinationFields[I].ItemIndex > 0) then
            Import.AddField(TSBaseTable(SObject).Fields[FDestinationFields[I].ItemIndex - 1], FSourceFields[I].Text);
      end
      else if (DialogType = idtExecuteJob) then
      begin
        for I := 0 to Length(Job.FieldMappings) - 1 do
          Import.AddField(TSBaseTable(SObject).FieldByName(Job.FieldMappings[I].DestinationFieldName), Job.FieldMappings[I].SourceFieldName);
      end;
    end;

    Import.Wnd := Self.Handle;
    Import.OnError := OnError;
    Import.OnTerminate := OnTerminate;
    Import.OnUpdate := OnUpdate;
    Import.Start();
  end;
end;

procedure TDImport.TSFieldsChange(Sender: TObject);
var
  Found: Boolean;
  I: Integer;
  J: Integer;
begin
  TSStmtType.Enabled := False;
  for I := 0 to Length(FDestinationFields) - 1 do
    if ((FSourceFields[I].Text <> '') and (FDestinationFields[I].ItemIndex > 0)) then
      TSStmtType.Enabled := True;

  FUpdate.Enabled := (SObject is TSBaseTable) and Assigned(TSBaseTable(SObject).PrimaryKey);
  if (FUpdate.Enabled and Assigned(TSBaseTable(SObject).PrimaryKey)) then
    for J := 0 to TSBaseTable(SObject).PrimaryKey.Columns.Count - 1 do
    begin
      Found := False;
      for I := 0 to Length(FDestinationFields) - 1 do
        Found := Found or (TSBaseTable(SObject).Database.Tables.NameCmp(FDestinationFields[I].Text, TSBaseTable(SObject).PrimaryKey.Columns[J].Field.Name) = 0);
      FUpdate.Enabled := FUpdate.Enabled and Found;
    end;
  FInsertOrUpdate.Enabled := (ImportType in [itTextFile, itExcelFile]) and FUpdate.Enabled; FLInsertUpdate.Enabled := FInsertOrUpdate.Enabled;

  CheckActivePageChange(TSFields.PageIndex);
end;

procedure TDImport.TSFieldsShow(Sender: TObject);
begin
  InitTSFields(Sender);
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
  end;
end;

procedure TDImport.TSJobShow(Sender: TObject);
begin
  CheckActivePageChange(TSJob.PageIndex);
end;

procedure TDImport.TSWhatShow(Sender: TObject);
begin
  TSFields.Enabled := (DialogType = idtNormal) and (FStructure.Checked or FData.Checked) and (SObject is TSBaseTable);
  TSTask.Enabled := (DialogType <> idtNormal) and (FStructure.Checked or FData.Checked) and not TSFields.Enabled;
  TSExecute.Enabled := (DialogType = idtNormal) and (FStructure.Checked or FData.Checked) and not TSFields.Enabled;
  CheckActivePageChange(TSWhat.PageIndex);
end;

procedure TDImport.TSSelectHide(Sender: TObject);
begin
  InitTSFields(Sender);
end;

procedure TDImport.TSSelectShow(Sender: TObject);
begin
  FSelectChange(Sender, FSelect.Selected);
end;

procedure TDImport.TSStmtTypeShow(Sender: TObject);
begin
  FStmtTypeClick(Sender);
end;

procedure TDImport.TSTablesHide(Sender: TObject);
begin
//  if (TSFields.Enabled) then
//  begin
//    ClearTSFields(Sender);
//    if (TSFields.Enabled) then
//      InitTSFields(Sender);
//  end;
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
  FTablesChange(FTables, FTables.Selected, ctState);
end;

procedure TDImport.TSTaskShow(Sender: TObject);
begin
  CheckActivePageChange(TSTask.PageIndex);
end;

procedure TDImport.TSXMLOptionsHide(Sender: TObject);
begin
  if (Length(FDestinationFields) = 0) then
    InitTSFields(Sender);
end;

procedure TDImport.UMChangePreferences(var Message: TMessage);
begin
  Preferences.SmallImages.GetIcon(iiImport, Icon);

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
  FLFilename.Caption := Preferences.LoadStr(348) + ':';
  FLDataSource.Caption := Preferences.LoadStr(38) + ':';

  GSelect.Caption := Preferences.LoadStr(755);

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

  GFields.Caption := Preferences.LoadStr(253);
  FLDestinationFields.Caption := Preferences.LoadStr(401) + ':';

  GStmtType.Caption := Preferences.LoadStr(238);
  FLStmtType.Caption := Preferences.LoadStr(124) + ':';
  FInsert.Caption := LowerCase(Preferences.LoadStr(880)) + ' (INSERT)';
  FReplace.Caption := LowerCase(Preferences.LoadStr(416)) + ' (REPLACE)';
  FUpdate.Caption := LowerCase(Preferences.LoadStr(726)) + ' (UPDATE)';
  FInsertOrUpdate.Caption := LowerCase(Preferences.LoadStr(910));
  FLInsertUpdate.Caption := '(INSERT / UPDATE)';

  GProgress.Caption := Preferences.LoadStr(224);
  FLEntiered.Caption := Preferences.LoadStr(211) + ':';
  FLDone.Caption := Preferences.LoadStr(232) + ':';
  FLProgressObjects.Caption := Preferences.LoadStr(234) + ':';
  FLProgressTime.Caption := Preferences.LoadStr(661) + ':';
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

procedure TDImport.UMPostAfterExecuteSQL(var Message: TMessage);
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

      CheckActivePageChange(PageControl.ActivePageIndex);
    end;
  end;
end;

procedure TDImport.UMPostShow(var Message: TMessage);
begin
  TSExecute.Enabled := True;
  PageControl.ActivePage := TSExecute;
end;

procedure TDImport.UMTerminate(var Message: TMessage);
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
  FBCancel.Enabled := True;

  FBCancel.Caption := Preferences.LoadStr(231);
  if (Success) then
    FBCancel.ModalResult := mrOk
  else
    FBCancel.ModalResult := mrCancel;

  if (Assigned(Import)) then
  begin
    Import.WaitFor();
    FreeAndNil(Import);
  end;
end;

procedure TDImport.UMUpdateProgressInfo(var Message: TMessage);
var
  Infos: TTool.PProgressInfos;
begin
  Infos := TTool.PProgressInfos(Message.LParam);

  if (Infos^.ObjectsSum < 0) then
    FEntieredObjects.Caption := '???'
  else
    FEntieredObjects.Caption := FormatFloat('#,##0', Infos^.ObjectsSum, LocaleFormatSettings);
  if (Infos^.ObjectsDone < 0) then
    FDoneObjects.Caption := '???'
  else
    FDoneObjects.Caption := FormatFloat('#,##0', Infos^.ObjectsDone, LocaleFormatSettings);

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

procedure TDImport.WhatClick(Sender: TObject);
begin
  if ((Sender = FStructure) and not FStructure.Checked) then
    FData.Checked := False;
  if ((Sender = FData) and FData.Checked) then
    FStructure.Checked := True;

  TSFields.Enabled := (DialogType = idtNormal) and FStructure.Checked and FData.Checked and (SObject is TSBaseTable);
  TSTask.Enabled := (DialogType <> idtNormal) and not TSFields.Enabled and FStructure.Checked;
  TSExecute.Enabled := (DialogType = idtNormal) and not TSFields.Enabled and FStructure.Checked;
  CheckActivePageChange(TSWhat.PageIndex);
end;

procedure TDImport.WhatKeyPress(Sender: TObject; var Key: Char);
begin
  WhatClick(Sender);
end;

procedure TDImport.WMHelp(var Message: TWMHelp);
begin
  if (Message.HelpInfo.iContextType = HELPINFO_MENUITEM) then
    inherited
  else if (MsgBoxHelpContext <> 0) then
    Application.HelpCommand(HELP_CONTEXT, MsgBoxHelpContext);
end;

initialization
  FImport := nil;
end.

