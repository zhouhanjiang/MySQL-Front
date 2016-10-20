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
  fJob in 'fJob.pas',
  fPreferences in 'fPreferences.pas',
  fSession in 'fSession.pas',
  fSQLParser in 'fSQLParser.pas',
  fTools in 'fTools.pas',
  fURI in 'fURI.pas',
  fBase in 'fBase.pas',
  fDAccount in 'fDAccount.pas' {DAccount},
  fDAccounts in 'fDAccounts.pas' {DAccounts},
  fDConnecting in 'fDConnecting.pas' {DConnecting},
  fDDatabase in 'fDDatabase.pas' {DDatabase},
  fDDatabases in 'fDDatabases.pas' {DDatabases},
  fDEvent in 'fDEvent.pas' {DEvent},
  fDExecutingSQL in 'fDExecutingSQL.pas' {DExecutingSQL},
  fDExport in 'fDExport.pas' {DExport},
  fDField in 'fDField.pas' {DField},
  fDForeignKey in 'fDForeignKey.pas' {DForeignKey},
  fDImport in 'fDImport.pas' {DImport},
  fDInfo in 'fDInfo.pas' {DInfo},
  fDInstallUpdate in 'fDInstallUpdate.pas' {DInstallUpdate},
  fDKey in 'fDKey.pas' {DIndex},
  fDLogin in 'fDLogin.pas' {DDBLogin},
  fDODBC in 'fDODBC.pas' {DDBODBC},
  fDOptions in 'fDOptions.pas' {DOptions},
  fDPartition in 'fDPartition.pas' {DPartition},
  fDPaste in 'fDPaste.pas' {DPaste},
  fDQuickFilter in 'fDQuickFilter.pas' {DQuickFilter},
  fDRoutine in 'fDRoutine.pas' {DRoutine},
  fDSearch in 'fDSearch.pas' {DSearch},
  fDSegment in 'fDSegment.pas' {DSegment},
  fDSelection in 'fDSelection.pas' {DSelection},
  fDServer in 'fDServer.pas' {DServer},
  fDSQLHelp in 'fDSQLHelp.pas' {DSQLHelp},
  fDStatement in 'fDStatement.pas' {DStatement},
  fDTable in 'fDTable.pas' {DTable},
  fDTransfer in 'fDTransfer.pas' {DTransfer},
  fDTrigger in 'fDTrigger.pas' {DTrigger},
  fDUser in 'fDUser.pas' {DUser},
  fDUserRight in 'fDUserRight.pas' {DUserRight},
  fDVariable in 'fDVariable.pas' {DVariable},
  fDView in 'fDView.pas' {DView},
  fCWorkbench in 'fCWorkbench.pas',
  fFSession in 'fFSession.pas' {FSession},
  fPDataBrowserDummy in 'fPDataBrowserDummy.pas' {PDataBrowserDummy},
  fWWindow in 'fWWindow.pas' {WWindow};

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
