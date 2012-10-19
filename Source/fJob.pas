unit fJob;

interface {********************************************************************}

uses
  DB,
  fPreferences, fSession, fTools;

type
  TJobExecution = class(TObject)
  private
    Account: TAAccount;
    Job: TAJob;
    Session: TSSession;
    StdErr: THandle;
    StdOut: THandle;
    function ExecuteExport(const Job: TAJobExport): Integer;
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
  SysUtils,
  MySQLDB, SQLUtils;

constructor TJobExecution.Create(const AccountName, JobName: string);
begin
  inherited Create();

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
      WriteLn(StdOut, 'Job: ' + Job.Name);
  end;
end;

destructor TJobExecution.Destroy();
begin
  Accounts.Free();

  CloseHandle(StdOut);
  CloseHandle(StdErr);
  FreeConsole();

  inherited;
end;

procedure TJobExecution.Execute();
begin
  Session := TSSession.Create(Sessions, Account);
  Session.FirstConnect();
  if (not Session.Connected) then
    WriteLn(StdErr, Session.ErrorMessage)
  else if (Job is TAJobExport) then
    ExecuteExport(TAJobExport(Job));
  Session.Free();
end;

function TJobExecution.ExecuteExport(const Job: TAJobExport): Integer;
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
    WriteLn(StdErr, Session.ErrorMessage)
  else
  begin
    Export := nil;
    case (Job.ExportType) of
      etSQLFile:
        try
          Export := TTExportSQL.Create(Session, Job.Filename, Job.CodePage);
          TTExportSQL(Export).CreateDatabaseStmts := Job.SQL.CreateDatabase;
          TTExportSQL(Export).Data := Job.SQL.Data;
          TTExportSQL(Export).DisableKeys := Job.SQL.DisableKeys;
          TTExportSQL(Export).DropStmts := Job.SQL.DropStmts;
          TTExportSQL(Export).ReplaceData := Job.SQL.ReplaceData;
          TTExportSQL(Export).Structure := Job.SQL.Structure;
          TTExportSQL(Export).UseDatabaseStmts := Job.SQL.UseDatabase;
        except
          WriteLn(StdErr, Preferences.LoadStr(522, Job.Filename));
        end;
      etTextFile:
        try
          Export := TTExportText.Create(Session, Job.Filename, Job.CodePage);
          TTExportText(Export).Data := True;
          case (Job.CSV.DelimiterType) of
            dtTab: TTExportText(Export).Delimiter := #9;
            dtChar: TTExportText(Export).Delimiter := Job.CSV.Delimiter;
          end;
          TTExportText(Export).QuoteStringValues := Job.CSV.Quote in [qtStrings, qtAll];
          TTExportText(Export).QuoteValues := Job.CSV.Quote in [qtAll];
          TTExportText(Export).Quoter := Job.CSV.Quoter;
          TTExportText(Export).Structure := Job.CSV.Headline;
        except
          WriteLn(StdErr, Preferences.LoadStr(522, Job.Filename));
        end;
      etExcelFile:
        try
          Export := TTExportExcel.Create(Session, Job.Filename);
          TTExportExcel(Export).Data := True;
          TTExportExcel(Export).Structure := True;
        except
          WriteLn(StdErr, Preferences.LoadStr(522, Job.Filename));
        end;
      etHTMLFile:
        try
          Export := TTExportHTML.Create(Session, Job.Filename, Job.CodePage);
          TTExportHTML(Export).Data := Job.HTML.Data;
          TTExportHTML(Export).TextContent := Job.HTML.MemoContent;
          TTExportHTML(Export).NULLText := Job.HTML.NULLText;
          TTExportHTML(Export).RowBackground := Job.HTML.RowBGColor;
          TTExportHTML(Export).Structure := Job.HTML.Structure
        except
          WriteLn(StdErr, Preferences.LoadStr(522, Job.Filename));
        end;
      etXMLFile:
        try
          Export := TTExportXML.Create(Session, Job.Filename, Job.CodePage);
          TTExportXML(Export).Data := True;
          case (Job._XML.Database.NodeType) of
            ntDisabled:
              begin
                TTExportXML(Export).DatabaseNodeText := '';
                TTExportXML(Export).DatabaseNodeAttribute := '';
              end;
            ntName:
              begin
                TTExportXML(Export).DatabaseNodeText := Job._XML.Database.NodeText;
                TTExportXML(Export).DatabaseNodeAttribute := '';
              end;
            ntCustom:
              begin
                TTExportXML(Export).DatabaseNodeText := Job._XML.Database.NodeText;
                TTExportXML(Export).DatabaseNodeAttribute := Job._XML.Database.NodeAttribute;
              end;
          end;
          case (Job._XML.Field.NodeType) of
            ntDisabled:
              begin
                TTExportXML(Export).FieldNodeText := '';
                TTExportXML(Export).FieldNodeAttribute := '';
              end;
            ntName:
              begin
                TTExportXML(Export).FieldNodeText := Job._XML.Field.NodeText;
                TTExportXML(Export).FieldNodeAttribute := '';
              end;
            ntCustom:
              begin
                TTExportXML(Export).FieldNodeText := Job._XML.Field.NodeText;
                TTExportXML(Export).FieldNodeAttribute := Job._XML.Field.NodeAttribute;
              end;
          end;
          TTExportXML(Export).RecordNodeText := Job._XML.Row.NodeText;
          TTExportXML(Export).RootNodeText := Job._XML.Root.NodeText;
          TTExportXML(Export).Structure := True;
          case (Job._XML.Table.NodeType) of
            ntDisabled:
              begin
                TTExportXML(Export).TableNodeText := '';
                TTExportXML(Export).TableNodeAttribute := '';
              end;
            ntName:
              begin
                TTExportXML(Export).TableNodeText := Job._XML.Table.NodeText;
                TTExportXML(Export).TableNodeAttribute := '';
              end;
            ntCustom:
              begin
                TTExportXML(Export).TableNodeText := Job._XML.Table.NodeText;
                TTExportXML(Export).TableNodeAttribute := Job._XML.Table.NodeAttribute;
              end;
          end;
        except
          WriteLn(StdErr, Preferences.LoadStr(522, Job.Filename));
        end;
      etPDFFile:
        try
          Export := TTExportPDF.Create(Session, Job.Filename);
          TTExportCanvas(Export).Data := Job.HTML.Data;
          TTExportCanvas(Export).NULLText := Job.HTML.NullText;
          TTExportCanvas(Export).Structure := Job.HTML.Structure;
        except
          WriteLn(StdErr, Preferences.LoadStr(522, Job.Filename));
        end;
    end;

    if (not Assigned(Export)) then
      WriteLn(StdErr, 'Export type not supported')
    else
    begin
      for I := 0 to Length(Job.Objects) - 1 do
      begin
        if (Job.Objects[I].ObjectType = jotServer) then
          for J := 0 to Session.Databases.Count - 1 do
          begin
            Database := Session.Databases[J];
            if (not Database.Update()) then
              WriteLn(StdErr, Session.ErrorMessage)
            else
            begin
              for K := 0 to Database.Tables.Count - 1 do
                Export.Add(Database.Tables[K]);
              if (Assigned(Database.Routines)) then
                for K := 0 to Database.Routines.Count - 1 do
                  Export.Add(Database.Routines[K]);
              if (Assigned(Database.Triggers)) then
                for K := 0 to Database.Triggers.Count - 1 do
                  Export.Add(Database.Triggers[K]);
              if (Assigned(Database.Events)) then
                for K := 0 to Database.Events.Count - 1 do
                  Export.Add(Database.Events[K]);
            end;
          end
        else if (Job.Objects[I].ObjectType = jotDatabase) then
        begin
          Database := Session.DatabaseByName(Job.Objects[I].Name);
          if (not Assigned(Database)) then
            WriteLn(StdErr, 'Database not found: ' + Job.Objects[I].Name)
          else if (not Database.Update()) then
            WriteLn(StdErr, Session.ErrorMessage)
          else
          begin
            for K := 0 to Database.Tables.Count - 1 do
              Export.Add(Database.Tables[K]);
            if (Assigned(Database.Routines)) then
              for K := 0 to Database.Routines.Count - 1 do
                Export.Add(Database.Routines[K]);
            if (Assigned(Database.Triggers)) then
              for K := 0 to Database.Triggers.Count - 1 do
                Export.Add(Database.Triggers[K]);
            if (Assigned(Database.Events)) then
              for K := 0 to Database.Events.Count - 1 do
                Export.Add(Database.Events[K]);
            DBObject := nil;
          end;
        end
        else
        begin
          Database := Session.DatabaseByName(Job.Objects[I].DatabaseName);
          if (not Assigned(Database)) then
            WriteLn(StdErr, 'Database not found: ' + Job.Objects[I].DatabaseName)
          else if (not Database.Update()) then
            WriteLn(StdErr, Session.ErrorMessage)
          else
            case (Job.Objects[I].ObjectType) of
              jotTable:
                begin
                  DBObject := Database.TableByName(Job.Objects[I].Name);
                  if (not Assigned(DBObject)) then
                    WriteLn(StdErr, 'Table not found: ' + Job.Objects[I].DatabaseName + '.' + Job.Objects[I].Name)
                  else
                    Export.Add(DBObject);
                end;
              jotProcedure:
                begin
                  DBObject := Database.ProcedureByName(Job.Objects[I].Name);
                  if (not Assigned(DBObject)) then
                    WriteLn(StdErr, 'Procedure not found: ' + Job.Objects[I].DatabaseName + '.' + Job.Objects[I].Name)
                  else
                    Export.Add(DBObject);
                end;
              jotFunction:
                begin
                  DBObject := Database.FunctionByName(Job.Objects[I].Name);
                  if (not Assigned(DBObject)) then
                    WriteLn(StdErr, 'Function not found: ' + Job.Objects[I].DatabaseName + '.' + Job.Objects[I].Name)
                  else
                    Export.Add(DBObject);
                end;
              jotTrigger:
                begin
                  DBObject := Database.TriggerByName(Job.Objects[I].Name);
                  if (not Assigned(DBObject)) then
                    WriteLn(StdErr, 'Trigger not found: ' + Job.Objects[I].DatabaseName + '.' + Job.Objects[I].Name)
                  else
                    Export.Add(DBObject);
                end;
              jotEvent:
                begin
                  DBObject := Database.EventByName(Job.Objects[I].Name);
                  if (not Assigned(DBObject)) then
                    WriteLn(StdErr, 'Event not found: ' + Job.Objects[I].DatabaseName + '.' + Job.Objects[I].Name)
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

procedure TJobExecution.ExportError(const Sender: TObject; const Error: TTool.TError; const Item: TTool.TItem; const ShowRetry: Boolean; var Success: TDataAction);
var
  ErrorMsg: string;
begin
  ErrorMsg := '';
  case (Error.ErrorType) of
    TE_Database:
      begin
        ErrorMsg := SQLUnwrapStmt(Session.ErrorMessage);
        if (Session.ErrorCode > 0) then
          ErrorMsg := ErrorMsg + ' (#' + IntToStr(Session.ErrorCode) + ')';
        ErrorMsg := ErrorMsg + '  -  ' + SQLUnwrapStmt(Session.CommandText);
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

  if (not ShowRetry) then
    Success := daAbort
  else
    Success := daFail;
end;

procedure TJobExecution.WriteLn(const Handle: THandle; const Text: string);
var
  CharsWritten: DWORD;
begin
  WriteConsole(Handle, PChar(Text), Length(Text), CharsWritten, nil);
  WriteConsole(Handle, PChar(#13#10), 2, CharsWritten, nil);

  if ((Handle = StdErr) and (ExitCode = 0)) then
    ExitCode := 1;
end;

end.

