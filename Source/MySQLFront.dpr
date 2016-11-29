program MySQLFront;

uses
  {$IFDEF Debug}
  FastMM4,
  {$ENDIF}
  {$IFDEF EurekaLog}
  ExceptionLog,
  {$ENDIF}
  Windows,
  ShellAPI,
  SysUtils,
  Forms,
  HTMLHelpViewer,
  MySQLClient in 'MySQL\Source\MySQLClient.pas',
  MySQLConsts in 'MySQL\Source\MySQLConsts.pas',
  MySQLDB in 'MySQL\Source\MySQLDB.pas',
  HTTPTunnel in 'MySQL\Source\HTTPTunnel.pas',
  SQLUtils in 'MySQL\Source\SQLUtils.pas',
  CSVUtils in 'MySQL\Source\CSVUtils.pas',
  MySQLDBGrid in 'MySQL\Source\MySQLDBGrid.pas',
  ExtCtrls_Ext in 'VCL\Source\ExtCtrls_Ext.pas',
  StdCtrls_Ext in 'VCL\Source\StdCtrls_Ext.pas',
  StdActns_Ext in 'VCL\Source\StdActns_Ext.pas',
  Dialogs_Ext in 'VCL\Source\Dialogs_Ext.pas',
  ComCtrls_Ext in 'VCL\Source\ComCtrls_Ext.pas',
  CommCtrl_Ext in 'VCL\Source\CommCtrl_Ext.pas',
  Forms_Ext in 'VCL\Source\Forms_Ext.pas',
  uDeveloper in 'uDeveloper.pas',
  uJob in 'uJob.pas',
  uPreferences in 'uPreferences.pas',
  uSession in 'uSession.pas',
  uSQLParser in 'uSQLParser.pas',
  uTools in 'uTools.pas',
  uURI in 'uURI.pas',
  uBase in 'uBase.pas',
  uDAccount in 'uDAccount.pas' {DAccount},
  uDAccounts in 'uDAccounts.pas' {DAccounts},
  uDConnecting in 'uDConnecting.pas' {DConnecting},
  uDDatabase in 'uDDatabase.pas' {DDatabase},
  uDDatabases in 'uDDatabases.pas' {DDatabases},
  uDEvent in 'uDEvent.pas' {DEvent},
  uDExecutingSQL in 'uDExecutingSQL.pas' {DExecutingSQL},
  uDExport in 'uDExport.pas' {DExport},
  uDField in 'uDField.pas' {DField},
  uDImport in 'uDImport.pas' {DImport},
  uDInfo in 'uDInfo.pas' {DInfo},
  uDKey in 'uDKey.pas' {DIndex},
  uDLogin in 'uDLogin.pas' {DDBLogin},
  uDODBC in 'uDODBC.pas' {DDBODBC},
  uDOptions in 'uDOptions.pas' {DOptions},
  uDPartition in 'uDPartition.pas' {DPartition},
  uDPaste in 'uDPaste.pas' {DPaste},
  uDQuickFilter in 'uDQuickFilter.pas' {DQuickFilter},
  uDRoutine in 'uDRoutine.pas' {DRoutine},
  uDSearch in 'uDSearch.pas' {DSearch},
  uDSegment in 'uDSegment.pas' {DSegment},
  uDSelection in 'uDSelection.pas' {DSelection},
  uDServer in 'uDServer.pas' {DServer},
  uDSQLHelp in 'uDSQLHelp.pas' {DSQLHelp},
  uDStatement in 'uDStatement.pas' {DStatement},
  uDTable in 'uDTable.pas' {DTable},
  uDTables in 'uDTables.pas' {DTables},
  uDTransfer in 'uDTransfer.pas' {DTransfer},
  uDTrigger in 'uDTrigger.pas' {DTrigger},
  uDUpdate in 'uDUpdate.pas' {DUpdate},
  uDUser in 'uDUser.pas' {DUser},
  uDUserRight in 'uDUserRight.pas' {DUserRight},
  uDVariable in 'uDVariable.pas' {DVariable},
  uDView in 'uDView.pas' {DView},
  uCWorkbench in 'uCWorkbench.pas',
  uFSession in 'uFSession.pas' {FSession},
  uPDataBrowserDummy in 'uPDataBrowserDummy.pas' {PDataBrowserDummy},
  uWWindow in 'uWWindow.pas' {WWindow};

{$R *.res}

var
  AccountName: string;
  UseConsole: Boolean;
  I: Integer;
  JobExecution: TJobExecution;
  JobName: string;
  Name: string;
  SetupProgram: TFileName;
  SetupProgramExecute: Boolean;
  Value: string;
  Value2: string;
begin
  Preferences := TPPreferences.Create();

  AccountName := '';
  JobName := '';
  for I := 1 to ParamCount() do
    if (TrySplitParam(ParamStr(I), Name, Value)) then
      if (lstrcmpi(PChar(Name), 'Account') = 0) then
        AccountName := Value
      else if (lstrcmpi(PChar(Name), 'Job') = 0) then
        JobName := Value;

  UseConsole := (AccountName <> '') and (JobName <> '');
  if (UseConsole) then
  begin
    JobExecution := TJobExecution.Create(AccountName, JobName);
    JobExecution.Execute();
    JobExecution.Free();
  end
  else
  begin
    if (Preferences.SetupProgramInstalled) then
    begin
      if (FileExists(PChar(Preferences.SetupProgram))) then
        DeleteFile(PChar(Preferences.SetupProgram));
      Preferences.SetupProgram := '';
      Preferences.SetupProgramInstalled := False;
    end
    else if (Preferences.SetupProgram <> '') then
    begin
      Preferences.SetupProgramExecute := FindWindow(cWindowClassName, nil) = 0;
      if (not Preferences.SetupProgramExecute) then
        MsgBox(Preferences.LoadStr(908, SysUtils.LoadStr(1000)), Preferences.LoadStr(45), MB_OK + MB_ICONERROR)
    end;

    if (not Preferences.SetupProgramExecute) then
    begin
      Application.Initialize();
      Application.Title := LoadStr(1000);
      {$IFDEF Debug}
        if (Application.Title = '') then
          Application.Title := Copy(ExtractFileName(Application.ExeName), 1, Length(ExtractFileName(Application.ExeName)) - Length(ExtractFileExt(Application.ExeName)));
      {$ENDIF}
      Application.Icon.Handle := LoadImage(hInstance, 'MAINICON', IMAGE_ICON, Application.Icon.Height, Application.Icon.Height, LR_DEFAULTCOLOR);
      {$IFDEF Debug}
        if (Application.Icon.Handle = 0) then
          Application.Icon.Handle := LoadImage(hInstance, PChar('..\Images\MySQLFront.ico'), IMAGE_ICON, Application.Icon.Height, Application.Icon.Height, LR_DEFAULTCOLOR + LR_LOADFROMFILE);
      {$ENDIF}
      Application.CreateForm(TWWindow, WWindow);
      Application.CreateForm(TPDataBrowserDummy, PDataBrowserDummy);
      Application.MainForm.Perform(UM_CHANGEPREFERENCES, 0, 0);
      GetClassName(Application.MainForm.Handle, PChar(SetupProgram), 100);
      Application.Run();
      if (Application.Handle <> 0) then
        ShowOwnedPopups(Application.Handle, False);
      Application.ShowHint := False;
      Application.Destroying();
      Application.DestroyComponents();
    end;
  end;

  if (UseConsole) then
    Preferences.Free()
  else
  begin
    SetupProgram := Preferences.SetupProgram;
    SetupProgramExecute := Preferences.SetupProgramExecute;
    Preferences.Free();

    if (SetupProgramExecute) then
      ShellExecute(0, 'open', PChar(SetupProgram), '/SILENT /NOICONS /TASKS=""', nil, SW_SHOWNORMAL);
  end;
end.
