unit fJob;

interface {********************************************************************}

uses
  DB,
  fPreferences, fSession, fTools;

type
  TJobExecution = class(TObject)
  private
    Account: TAAccount;
    Job: TAAccount.TJob;
    ErrorLogFile: THandle;
    Session: TSSession;
    StdErr: THandle;
    StdOut: THandle;
    function ExecuteExport(const Job: TAAccount.TJobExport): Integer;
    function ExecuteImport(const Job: TAAccount.TJobImport): Integer;
    procedure ExportError(const Sender: TObject; const Error: TTool.TError; const Item: TTool.TItem; const ShowRetry: Boolean; var Success: TDataAction);
  public
    constructor Create(const AccountName, JobName: string);
    destructor Destroy(); override;
    procedure Execute();
    procedure WriteLn(const Handle: THandle; const Text: string);
  end;

implementation {***************************************************************}

uses
  Windows, Classes,
  SysUtils, StrUtils,
  ODBCAPI,
  MySQLDB, SQLUtils;

constructor TJobExecution.Create(const AccountName, JobName: string);
begin
  inherited Create();

  ErrorLogFile := INVALID_HANDLE_VALUE;

  AllocConsole();
  SetConsoleTitle(PChar(SysUtils.LoadStr(1000)));
  SetConsoleCP(CP_UTF8);
  SetConsoleOutputCP(CP_UTF8);

  StdOut := GetStdHandle(STD_OUTPUT_HANDLE);
  StdErr := GetStdHandle(STD_ERROR_HANDLE);


  WriteLn(StdOut, SysUtils.LoadStr(1000) + ' ' + Preferences.VersionStr);

  Accounts := TAAccounts.Create(nil);

  Account := Accounts.AccountByName(AccountName);
  if (not Assigned(Account)) then
  begin
    WriteLn(StdErr, 'Account not found: ' + AccountName);
    Job := nil;
  end
  else
  begin
    WriteLn(StdOut, 'Account: ' + Account.Name);
    Job := Account.JobByName(JobName);
    if (not Assigned(Job)) then
      WriteLn(StdErr, 'Job not found: ' + JobName)
    else
    begin
      WriteLn(StdOut, 'Job: ' + Job.Name);

      if (FileExists(Job.LogFilename)) then
        DeleteFile(Job.LogFilename);
    end;
  end;
end;

destructor TJobExecution.Destroy();
begin
  if (ErrorLogFile <> INVALID_HANDLE_VALUE) then
  begin
    CloseHandle(ErrorLogFile);
    Sleep(5000);
  end;

  Accounts.Free();

  CloseHandle(StdOut);
  CloseHandle(StdErr);
  FreeConsole();

  inherited;
end;

procedure TJobExecution.Execute();
begin
  Session := TSSession.Create(Sessions, Account);
  Session.Connection.Connect();
  if (not Session.Connection.Connected) then
    WriteLn(StdErr, Session.Connection.ErrorMessage)
  else if (Job is TAAccount.TJobImport) then
    ExecuteImport(TAAccount.TJobImport(Job))
  else if (Job is TAAccount.TJobExport) then
    ExecuteExport(TAAccount.TJobExport(Job));
  Session.Free();
end;

function TJobExecution.ExecuteExport(const Job: TAAccount.TJobExport): Integer;
var
  Database: TSDatabase;
  DBObject: TSDBObject;
  Export: TTExport;
  I: Integer;
  J: Integer;
  K: Integer;
begin
  Result := 1;
  if (not Session.Update()) then
    WriteLn(StdErr, Session.Connection.ErrorMessage)
  else
  begin
    Export := nil;
    case (Job.ExportType) of
      etSQLFile:
        begin
          Export := TTExportSQL.Create(Session, Job.Filename, Job.CodePage);
          TTExportSQL(Export).Data := Job.SQL.Data;
          TTExportSQL(Export).DropStmts := Job.SQL.DropStmts;
          TTExportSQL(Export).ReplaceData := Job.SQL.ReplaceData;
          TTExportSQL(Export).Structure := Job.SQL.Structure;
        end;
      etTextFile:
        begin
          Export := TTExportText.Create(Session, Job.Filename, Job.CodePage);
          TTExportText(Export).Data := True;
          case (Job.CSV.DelimiterType) of
            dtTab: TTExportText(Export).Delimiter := #9;
            dtChar: TTExportText(Export).Delimiter := Job.CSV.Delimiter;
          end;
          TTExportText(Export).QuoteValues := Job.CSV.QuoteValues;
          TTExportText(Export).Quoter := Job.CSV.Quoter;
          TTExportText(Export).Structure := Job.CSV.Headline;
        end;
      etAccessFile:
        begin
          Export := TTExportAccess.Create(Session, Job.Filename);
          TTExportAccess(Export).Data := True;
          TTExportAccess(Export).Structure := True;
        end;
      etExcelFile:
        begin
          Export := TTExportExcel.Create(Session, Job.Filename);
          TTExportExcel(Export).Data := True;
          TTExportExcel(Export).Structure := True;
          TTExportExcel(Export).Excel2007 := Job.Excel.Excel2007;
        end;
      etODBC:
        begin
          Export := TTExportODBC.Create(Session, Job.ODBC.DataSource, Job.ODBC.Username, Job.ODBC.Password);
          TTExportBaseODBC(Export).Data := True;
          TTExportBaseODBC(Export).Structure := True;
        end;
      etHTMLFile:
        begin
          Export := TTExportHTML.Create(Session, Job.Filename);
          TTExportHTML(Export).Data := Job.HTML.Data;
          TTExportHTML(Export).TextContent := Job.HTML.MemoContent;
          TTExportHTML(Export).NULLText := Job.HTML.NULLText;
          TTExportHTML(Export).RowBackground := Job.HTML.RowBGColor;
          TTExportHTML(Export).Structure := Job.HTML.Structure
        end;
      etXMLFile:
        begin
          Export := TTExportXML.Create(Session, Job.Filename);
          TTExportXML(Export).Data := True;
          case (Job.XML.Database.NodeType) of
            ntDisabled:
              begin
                TTExportXML(Export).DatabaseNodeText := '';
                TTExportXML(Export).DatabaseNodeAttribute := '';
              end;
            ntName:
              begin
                TTExportXML(Export).DatabaseNodeText := Job.XML.Database.NodeText;
                TTExportXML(Export).DatabaseNodeAttribute := '';
              end;
            ntCustom:
              begin
                TTExportXML(Export).DatabaseNodeText := Job.XML.Database.NodeText;
                TTExportXML(Export).DatabaseNodeAttribute := Job.XML.Database.NodeAttribute;
              end;
          end;
          case (Job.XML.Field.NodeType) of
            ntDisabled:
              begin
                TTExportXML(Export).FieldNodeText := '';
                TTExportXML(Export).FieldNodeAttribute := '';
              end;
            ntName:
              begin
                TTExportXML(Export).FieldNodeText := Job.XML.Field.NodeText;
                TTExportXML(Export).FieldNodeAttribute := '';
              end;
            ntCustom:
              begin
                TTExportXML(Export).FieldNodeText := Job.XML.Field.NodeText;
                TTExportXML(Export).FieldNodeAttribute := Job.XML.Field.NodeAttribute;
              end;
          end;
          TTExportXML(Export).RecoreNodeText := Job.XML.Row.NodeText;
          TTExportXML(Export).RootNodeText := Job.XML.Root.NodeText;
          TTExportXML(Export).Structure := True;
          case (Job.XML.Table.NodeType) of
            ntDisabled:
              begin
                TTExportXML(Export).TableNodeText := '';
                TTExportXML(Export).TableNodeAttribute := '';
              end;
            ntName:
              begin
                TTExportXML(Export).TableNodeText := Job.XML.Table.NodeText;
                TTExportXML(Export).TableNodeAttribute := '';
              end;
            ntCustom:
              begin
                TTExportXML(Export).TableNodeText := Job.XML.Table.NodeText;
                TTExportXML(Export).TableNodeAttribute := Job.XML.Table.NodeAttribute;
              end;
          end;
        end;
      etPDFFile:
        begin
          Export := TTExportPDF.Create(Session, Job.Filename);
          TTExportCanvas(Export).Data := Job.HTML.Data;
          TTExportCanvas(Export).NULLText := Job.HTML.NullText;
          TTExportCanvas(Export).Structure := Job.HTML.Structure;
        end;
    end;

    if (not Assigned(Export)) then
      WriteLn(StdErr, 'Export type not supported')
    else
    begin
      for I := 0 to Length(Job.JobObjects) - 1 do
      begin
        if (Job.JobObjects[I].ObjectType = jotServer) then
          for J := 0 to Session.Databases.Count - 1 do
          begin
            Database := Session.Databases[J];
            if (not Database.Update()) then
              WriteLn(StdErr, Session.Connection.ErrorMessage)
            else
            begin
              for K := 0 to Database.Tables.Count - 1 do
                Export.Add(Database.Tables[K]);
              if (Assigned(Database.Routines)) then
                for K := 0 to Database.Routines.Count - 1 do
                  Export.Add(Database.Routines[K]);
              if (Assigned(Database.Events)) then
                for K := 0 to Database.Events.Count - 1 do
                  Export.Add(Database.Events[K]);
            end;
          end
        else if (Job.JobObjects[I].ObjectType = jotDatabase) then
        begin
          Database := Session.DatabaseByName(Job.JobObjects[I].Name);
          if (not Assigned(Database)) then
            WriteLn(StdErr, 'Database not found: ' + Job.JobObjects[I].Name)
          else if (not Database.Update()) then
            WriteLn(StdErr, Session.Connection.ErrorMessage)
          else
          begin
            for K := 0 to Database.Tables.Count - 1 do
              Export.Add(Database.Tables[K]);
            if (Assigned(Database.Routines)) then
              for K := 0 to Database.Routines.Count - 1 do
                Export.Add(Database.Routines[K]);
            if (Assigned(Database.Events)) then
              for K := 0 to Database.Events.Count - 1 do
                Export.Add(Database.Events[K]);
            DBObject := nil;
          end;
        end
        else
        begin
          Database := Session.DatabaseByName(Job.JobObjects[I].DatabaseName);
          if (not Assigned(Database)) then
            WriteLn(StdErr, 'Database not found: ' + Job.JobObjects[I].DatabaseName)
          else if (not Database.Update()) then
            WriteLn(StdErr, Session.Connection.ErrorMessage)
          else
            case (Job.JobObjects[I].ObjectType) of
              jotTable:
                begin
                  DBObject := Database.TableByName(Job.JobObjects[I].Name);
                  if (not Assigned(DBObject)) then
                    WriteLn(StdErr, 'Table not found: ' + Job.JobObjects[I].DatabaseName + '.' + Job.JobObjects[I].Name)
                  else
                  begin
                    Export.Add(DBObject);
                    if ((DBObject is TSBaseTable) and Assigned(DBObject.Database.Triggers)) then
                      for J := 0 to TSBaseTable(DBObject).TriggerCount - 1 do
                        Export.Add(TSBaseTable(DBObject).Triggers[J]);
                  end;
                end;
              jotProcedure:
                begin
                  DBObject := Database.ProcedureByName(Job.JobObjects[I].Name);
                  if (not Assigned(DBObject)) then
                    WriteLn(StdErr, 'Procedure not found: ' + Job.JobObjects[I].DatabaseName + '.' + Job.JobObjects[I].Name)
                  else
                    Export.Add(DBObject);
                end;
              jotFunction:
                begin
                  DBObject := Database.FunctionByName(Job.JobObjects[I].Name);
                  if (not Assigned(DBObject)) then
                    WriteLn(StdErr, 'Function not found: ' + Job.JobObjects[I].DatabaseName + '.' + Job.JobObjects[I].Name)
                  else
                    Export.Add(DBObject);
                end;
              jotTrigger:
                begin
                  DBObject := Database.TriggerByName(Job.JobObjects[I].Name);
                  if (not Assigned(DBObject)) then
                    WriteLn(StdErr, 'Trigger not found: ' + Job.JobObjects[I].DatabaseName + '.' + Job.JobObjects[I].Name)
                  else
                    Export.Add(DBObject);
                end;
              jotEvent:
                begin
                  DBObject := Database.EventByName(Job.JobObjects[I].Name);
                  if (not Assigned(DBObject)) then
                    WriteLn(StdErr, 'Event not found: ' + Job.JobObjects[I].DatabaseName + '.' + Job.JobObjects[I].Name)
                  else
                    Export.Add(DBObject);
                end;
            end;
          end;

      end;

      Export.OnError := ExportError;
      Export.Execute();
      Export.Free();

      Result := 0;
    end;
  end;
end;

function TJobExecution.ExecuteImport(const Job: TAAccount.TJobImport): Integer;
var
  Database: TSDatabase;
  Import: TTImport;

  procedure ImportAdd(TableName: string; const SourceTableName: string = '');
  begin
    TableName := Session.ApplyIdentifierName(TableName);
    Import.AddTable(TableName, SourceTableName);
  end;

  function TableName(const SourceName: string): string;
  begin
    if (Job.ImportType <> itExcelFile) then
      Result := SourceName
    else if (Pos('$', SourceName) = Length(SourceName)) then
      Result := LeftStr(SourceName, Length(SourceName) - 1)
    else
      Result := LeftStr(SourceName, Pos('$', SourceName) - 1) + '_' + RightStr(SourceName, Length(SourceName) - Pos('$', SourceName));

    Result := Session.ApplyIdentifierName(Result);
  end;

var
  I: Integer;
  ODBC: SQLHDBC;
  Table: TSBaseTable;
begin
  Result := 1;
  if (not Session.Update()) then
    WriteLn(StdErr, Session.Connection.ErrorMessage)
  else
  begin
    case (Job.JobObject.ObjectType) of
      jotDatabase: Database := Session.DatabaseByName(Job.JobObject.Name);
      jotTable,
      jotProcedure,
      jotFunction,
      jotTrigger,
      jotEvent: Database := Session.DatabaseByName(Job.JobObject.DatabaseName);
      else Database := nil;
    end;

    if ((Job.JobObject.ObjectType <> jotServer) and not Assigned(Database)) then
      case (Job.JobObject.ObjectType) of
        jotDatabase: WriteLn(StdErr, 'Database not found: ' + Job.JobObject.Name);
        jotTable,
        jotProcedure,
        jotFunction,
        jotTrigger,
        jotEvent: WriteLn(StdErr, 'Database not found: ' + Job.JobObject.DatabaseName);
      end
    else if (not Assigned(Database) or not Database.Update()) then
      WriteLn(StdErr, Session.Connection.ErrorMessage)
    else
    begin
      if (Job.JobObject.ObjectType <> jotTable) then
        Table := nil
      else
        Table := Database.BaseTableByName(Job.JobObject.Name);

      if ((Job.JobObject.ObjectType = jotTable) and not Assigned(Table)) then
        WriteLn(StdErr, 'Table not found: ' + Database.Name + '.' + Job.JobObject.Name)
      else
      begin
        ODBC := SQL_NULL_HANDLE;

        Import := nil;
        case (Job.ImportType) of
          itSQLFile:
            begin
              Import := TTImportSQL.Create(Job.Filename, Job.CodePage, Session, Database);
            end;
          itTextFile:
            begin
              Import := TTImportText.Create(Job.Filename, Job.CodePage, Session, Database);
              case (Job.CSV.DelimiterType) of
                dtTab: TTImportText(Import).Delimiter := #9;
                dtChar: TTImportText(Import).Delimiter := Job.CSV.Delimiter[1];
              end;
              case (Job.CSV.Quote) of
                qtNone: TTImportText(Import).Quoter := #0;
                qtStrings: TTImportText(Import).Quoter := Job.CSV.QuoteChar[1];
                qtAll: TTImportText(Import).Quoter := Job.CSV.QuoteChar[1];
              end;
              TTImportText(Import).UseHeadline := Job.CSV.Headline;

              if (Job.JobObject.ObjectType = jotTable) then
                ImportAdd(Job.JobObject.Name)
              else
                ImportAdd(Copy(Job.Filename, 1 + Length(ExtractFilePath(Job.Filename)), Length(Job.Filename) - Length(ExtractFilePath(Job.Filename)) - Length(ExtractFileExt(Job.Filename))));
            end;
          itExcelFile:
            begin
              Import := TTImportExcel.Create(Session, Database, Job.Filename);

              if (Job.JobObject.ObjectType = jotTable) then
                ImportAdd(Job.JobObject.Name, Job.SourceObjects[0].Name)
              else
                for I := 0 to Length(Job.SourceObjects) - 1 do
                  ImportAdd(TableName(Job.SourceObjects[I].Name), Job.SourceObjects[I].Name);
            end;
          itAccessFile:
            begin
              Import := TTImportAccess.Create(Session, Database, Job.Filename);

              if (Job.JobObject.ObjectType = jotTable) then
                ImportAdd(Job.JobObject.Name, Job.SourceObjects[0].Name)
              else
                for I := 0 to Length(Job.SourceObjects) - 1 do
                  ImportAdd(TableName(Job.SourceObjects[I].Name), Job.SourceObjects[I].Name);
            end;
          itODBC:
            begin
              Import := TTImportODBC.Create(Session, Database, Job.ODBC.DataSource, Job.ODBC.Username, Job.ODBC.Password);

              if (Job.JobObject.ObjectType = jotTable) then
                ImportAdd(Job.JobObject.Name, Job.SourceObjects[0].Name)
              else
                for I := 0 to Length(Job.SourceObjects) - 1 do
                  ImportAdd(TableName(Job.SourceObjects[I].Name), Job.SourceObjects[I].Name);
            end;
        end;

        if (Assigned(Import)) then
        begin
          Import.Data := Job.Data;
          Import.DefaultCharset := Job.Charset;
          Import.DefaultCollation := Job.Collation;
          Import.Engine := Job.Engine;
          Import.RowType := mrUnknown;
          Import.Structure := Job.Structure;

          if (Assigned(Table) and Table.Update()) then
          begin
            Table.InvalidateData();
            for I := 0 to Length(Job.FieldMappings) - 1 do
              if (Assigned(Import)) then
                if (not Assigned(Table.FieldByName(Job.FieldMappings[I].DestinationFieldName))) then
                begin
                  WriteLn(StdErr, 'Field not found: ' + Database.Name + '.' + Table.Name + '.' + Job.FieldMappings[I].DestinationFieldName);
                  FreeAndNil(Import);
                end
                else
                  Import.AddField(Table.FieldByName(Job.FieldMappings[I].DestinationFieldName), Job.FieldMappings[I].SourceFieldName);
          end;

          if (Assigned(Import)) then
          begin
            Import.OnError := ExportError;
            Import.Execute();
            Import.Free();
          end;
        end;

        if (ODBC <> SQL_NULL_HANDLE) then
          SQLFreeHandle(SQL_HANDLE_DBC, ODBC);
      end;
    end;
  end;
end;

procedure TJobExecution.ExportError(const Sender: TObject; const Error: TTool.TError; const Item: TTool.TItem; const ShowRetry: Boolean; var Success: TDataAction);
var
  ErrorMsg: string;
begin
  ErrorMsg := '';
  case (Error.ErrorType) of
    TE_Database:
      begin
        ErrorMsg := SQLUnwrapStmt(Session.Connection.ErrorMessage, Session.Connection.ServerVersion);
        if (Session.Connection.ErrorCode > 0) then
          ErrorMsg := ErrorMsg + ' (#' + IntToStr(Session.Connection.ErrorCode) + ')';
        ErrorMsg := ErrorMsg + '  -  ' + SQLUnwrapStmt(Session.Connection.CommandText, Session.Connection.ServerVersion);
      end;
    TE_File:
      ErrorMsg := Error.ErrorMessage + ' (#' + IntToStr(Error.ErrorCode) + ')';
    TE_ODBC:
      if (Error.ErrorCode = 0) then
        ErrorMsg := Error.ErrorMessage
      else
        ErrorMsg := Error.ErrorMessage + ' (#' + IntToStr(Error.ErrorCode) + ')';
    else
      ErrorMsg := Error.ErrorMessage;
  end;

  WriteLn(StdErr, Trim(ErrorMsg));

  Success := daAbort;
end;

procedure TJobExecution.WriteLn(const Handle: THandle; const Text: string);
var
  BytesWritten: DWORD;
  CharsWritten: DWORD;
  Size: DWORD;
begin
  WriteConsole(Handle, PChar(Text), Length(Text), CharsWritten, nil);
  WriteConsole(Handle, PChar(#13#10), 2, CharsWritten, nil);

  if ((Handle = StdErr) and Assigned(Job)) then
  begin
    if ((ErrorLogFile = INVALID_HANDLE_VALUE) and ForceDirectories(ExtractFilePath(Job.LogFilename))) then
    begin
      ErrorLogFile := CreateFile(PChar(Job.LogFilename),
                            GENERIC_WRITE,
                            FILE_SHARE_READ,
                            nil,
                            CREATE_ALWAYS, FILE_ATTRIBUTE_NORMAL, 0);

      if (ErrorLogFile <> INVALID_HANDLE_VALUE) then
        WriteFile(ErrorLogFile, BOM_UNICODE_LE^, Length(BOM_UNICODE_LE), Size, nil)
    end;

    if (ErrorLogFile <> INVALID_HANDLE_VALUE) then
      WriteFile(ErrorLogFile, PChar(Text)^, Length(Text) * SizeOf(Char), BytesWritten, nil);

    if (ExitCode = 0) then
      ExitCode := 1;
  end;
end;

end.

