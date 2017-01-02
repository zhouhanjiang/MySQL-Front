unit uEurekaLog;

interface {********************************************************************}

uses
  Classes,
  EClasses, ECallStack, ETypes;

type
  TStackList = class(TEurekaBaseStackList)
  end;

  TStackFormatter = class(ECallStack.TEurekaStackFormatter)
  private
    FHasEncryptedData: Boolean;
    FLineLen: Integer;
    FMaxLine: Integer;
    FMaxModule: Integer;
    FMaxProc: Integer;
    FMaxUnit: Integer;
    function FormatLine(const AMethods, ADetails, AStackAddress, AAddress, AName, AOffset, AUnit, AClass, AProcedure, ALine: String): String;
  protected
    function AcceptableCallStackItem(const AInd: Integer): Boolean; override;
    procedure CalculateLengths; override;
    function ContainEncryptedItems(const CallStack: TEurekaBaseStackList): Boolean; virtual;
    function CreateThreadStr(const Index, No: Integer; const DebugInfo: TEurekaDebugInfo; const TextLog: Boolean; const Session: THandle; const ExternalWCTSession: Boolean = False): String; override;
    function GetItemText(const AIndex: Integer): String; override;
    function GetStrings(): TStrings; override;
  public
    constructor Create(); override;
  end;


implementation {***************************************************************}

uses
  Windows, WinInet,
  SysUtils,
  Forms,
  EException, EStackTracing, ESysInfo, EInfoFormat, EThreadsManager, EConsts,
  EWCTSupport, EEvents,
  MySQLDB,
  uDeveloper, uPreferences, uSession,
  uBase;

{ TEurekaStackFormatter *******************************************************}

function TStackFormatter.AcceptableCallStackItem(const AInd: Integer): Boolean;
var
  Buffer: TEurekaDebugInfo;
  Item: PEurekaDebugInfo;
begin
  Item := CallStack.GetItem(AInd, Buffer);

  Result := Item^.ErrorLine // Is exception itself (a.k.a. FirstAddr)?
    or not Assigned(Item^.StackAddr) and ((AInd = 0) or (Item^.ThreadID <> CallStack.Items[AInd - 1].ThreadID)); // always keep first line in any call stack

  if (not Result) then
  begin
    // Second+ entries should be filtered upon details/methods
    Result := (Item^.Location.DebugDetail in DebugDetails) and
              (
                (Item^.Methods and TracerUndefinedMask <> 0) or
                (
                  ((Item^.Methods and AllowedMethods) <> 0) and
                  ((Item^.Methods and DisabledMethods) = 0)
                )
              );

    // For pointers into dynamic code blocks (outside of any module) - use return addresses only
    if (Result and (Item^.Location.DebugDetail = ddNone)) then
      Result := Item^.Location.Address <> Item^.ReturnAddr;
  end;
end;

procedure TStackFormatter.CalculateLengths();
var
  ClassName: string;
  ProcName: string;
  DI: PEurekaDebugInfo;
  I: Integer;
  S: String;
begin
  if not Calculated then
  begin
    FMaxModule := Length(CaptionModule);
    FMaxUnit := Length(CaptionUnit);
    FMaxProc := Length(CaptionProcedure);
    FMaxLine := Length(CaptionLine);
    FHasEncryptedData := ContainEncryptedItems(CallStack);

    for I := 0 to CallStack.Count - 1 do
    begin
      if AcceptableCallStackItem(I) then
      begin
        DI := CallStack.Item[I];
        S := PrepareName(ExtractFileNameEx(DI.Location.ModuleName), DI^, FHasEncryptedData);
        if Length(S) > FMaxModule then
          FMaxModule := Length(S);
        S := PrepareName(DI.Location.UnitName, DI^, FHasEncryptedData);
        if Length(S) > FMaxUnit then
          FMaxUnit := Length(S);
        ClassName := PrepareName(DI.Location.ClassName, DI^, FHasEncryptedData);
        ProcName := PrepareName(DI.Location.ProcedureName, DI^, FHasEncryptedData);
        if ((ClassName <> '') and (ProcName <> '')) then
          S := ClassName + '.' + ProcName
        else if (ClassName <> '') then
          S := ClassName
        else
          S := ProcName;
        if Length(S) > FMaxProc then
          FMaxProc := Length(S);
        if DI.Location.LineNumber <> 0 then
        begin
          S := Format('%d[%d]', [DI.Location.LineNumber, DI.Location.OffsetFromProcName]); // Do Not Localize
          if Length(S) > FMaxLine then
            FMaxLine := Length(S);
        end;
      end;
    end;

    FLineLen := FMaxModule + FMaxUnit + FMaxProc + FMaxLine + FLineLen + 5;
    FCalculated := True;
  end;
end;

constructor TStackFormatter.Create();
begin
  inherited;

  DebugDetails := [ddSourceCode];
end;

function TStackFormatter.CreateThreadStr(const Index, No: Integer; const DebugInfo: TEurekaDebugInfo; const TextLog: Boolean; const Session: THandle; const ExternalWCTSession: Boolean = False): String;
var
  CallerAddress: Pointer;
  Line1: string;
  Line2: string;
  ParentID: Cardinal;
  ThreadClassName: string;
  ThreadData: TEurekaThreadData;
  ThreadName: String;
begin
  try
    if (not DebugInfo.ErrorLine) then
      Line1 := ''
    else
      Line1 := '*' + CaptionExceptionThread;
    if (DebugInfo.ThreadID = MainThreadId) then
    begin
      if (Line1 <> '') then Line1 := Line1 + ', ';
      Line1 := Line1 + 'Main Thread';
    end;

    if (DebugInfo.ThreadID > 0) then
    begin
      if (Line1 <> '') then Line1 := Line1 + ', ';
      Line1 := Line1 + CaptionThreadID + ': ' + IntToStr(DebugInfo.ThreadID);
    end;

    GetThreadInfo(DebugInfo.ThreadID, ParentID, ThreadClassName, ThreadName, CallerAddress);
    if (ParentID > 0) then
      Line1 := Line1 + ', ' + CaptionParentID + ' ID: ' + IntToStr(ParentID);


    if (DebugInfo.ThreadClass = '') then
      Line2 := ''
    else
      Line2 := CaptionThreadClass + ': ' + DebugInfo.ThreadClass;

    if (Line2 <> '') then
    begin
      ThreadData := AquireThreadData(DebugInfo.ThreadID);
      if (Assigned(ThreadData)) then
      begin
        if (ThreadData.Thread is TThread) then
        begin
          if (TThread(ThreadData.Thread).Finished) then
            Line2 := Line2 + ', finished';
        end;
        ThreadData.Free();
      end;
    end;

    if (Line1 = '') then
      Result := ''
    else
      Result := Line1 + #13#10;
    if (Line2 <> '') then
      Result := Result + Line2 + #13#10;
  except
    on E: Exception do
      Result := 'Error in CreateThreadStr(): ' + E.Message;
  end;
end;

function TStackFormatter.FormatLine(const AMethods, ADetails, AStackAddress, AAddress, AName, AOffset, AUnit, AClass, AProcedure, ALine: String): String;
begin
  Assert(Calculated);
  Result := Format('|%s|%s|%s|%s|',
    [FmtStrForLog(FmtCompleteStr(AName, FMaxModule)),
     FmtStrForLog(FmtCompleteStr(AUnit, FMaxUnit)),
     FmtStrForLog(FmtCompleteStr(AProcedure, FMaxProc)),
     FmtStrForLog(FmtCompleteStr(ALine, FMaxLine))]);
end;

function TStackFormatter.GetItemText(const AIndex: Integer): String;

  function DetailsToStr(const ADetail: TEurekaDebugDetail): String;
  begin
    Result := IntToHex(Ord(ADetail), 2);
  end;

var
  ClassName: string;
  I: Integer;
  L: String;
  ProcName: string;
  S: string;
begin
  CalculateLengths;
  I := CallStack.Items[AIndex].Location.LineNumber;
  if I <> 0 then
    L := Format('%d[%d]', [I, CallStack.Items[AIndex].Location.ProcOffsetLine]) // Do Not Localize
  else
    L := '';

  ClassName := PrepareName(CallStack.Items[AIndex].Location.ClassName, CallStack.Items[AIndex], FHasEncryptedData);
  ProcName := PrepareName(CallStack.Items[AIndex].Location.ProcedureName, CallStack.Items[AIndex], FHasEncryptedData);
  if ((ClassName <> '') and (ProcName <> '')) then
    ProcName := ClassName + '.' + ProcName
  else if (ClassName <> '') then
    ProcName := ClassName;

  S := CallStack.Items[AIndex].Location.ModuleName;
  Result := FormatLine(IntToHex(CallStack.Items[AIndex].Methods, (TracerMax div 8) * 2),
                       DetailsToStr(CallStack.Items[AIndex].Location.DebugDetail),
                       FmtPointerToStr(CallStack.Items[AIndex].StackAddr),
                       FmtPointerToStr(CallStack.Items[AIndex].Location.Address),
                       PrepareName(ExtractFileNameEx(S), CallStack.Items[AIndex], FHasEncryptedData),
                       FmtPointerToStr(Pointer(PtrUInt(CallStack.Items[AIndex].Location.Address) - PtrUInt(CallStack.Items[AIndex].Location.Module))),
                       PrepareName(CallStack.Items[AIndex].Location.UnitName, CallStack.Items[AIndex], FHasEncryptedData),
                       '',
                       ProcName,
                       L);
end;

function TStackFormatter.GetStrings(): TStrings;

  procedure AddHeader(const LineStr: String);
  var
    S: String;
  begin
    S := FormatLine(CaptionMethods, CaptionDebugLevel, CaptionStackAddress,
                    CaptionAddress, CaptionModule, CaptionOffset, CaptionUnit, CaptionClass,
                    CaptionProcedure, CaptionLine);
    FStr.Add(LineStr);
    FStr.Add(S);
    FStr.Add(LineStr);
  end;

  procedure AddLine(const ALine: String);
  var
    Strs: TStringList;
    LineIndex: Integer;
  begin
    if Pos(sLineBreak, ALine) <= 0 then
    begin
      FStr.Add(Format('|%s|', [FmtCompleteStr(ALine, FLineLen - 2)])); // Do Not Localize
      Exit;
    end;

    Strs := TStringList.Create;
    try
      Strs.Text := ALine;
      for LineIndex := 0 to Strs.Count - 1 do
        AddLine(Strs[LineIndex]);
    finally
      FreeAndNil(Strs);
    end;
  end;

var
  I, Z: Integer;
  LineStr, SubLineStr: String;
  LastUsedThreadID: Cardinal;
  Empty, RunningThread: Boolean;
begin
  if not Assigned(FStr) then
  begin
    FStr := TStringList.Create;
    FModified := True;
  end;
  if FModified then
  begin
    CalculateLengths;
    FStr.BeginUpdate;
    try
      FStr.Clear;
      FStr.Capacity := CallStack.Count;

      Empty := True;
      LineStr := StringOfChar(Char('-'), FLineLen); // Do Not Localize
      SubLineStr := StringOfChar(Char('-'), FLineLen - 2); // Do Not Localize

      if CaptionHeader <> '' then // Do Not Localize
        FStr.Add(CaptionHeader);

      LastUsedThreadID := 0;
      Z := 1;
      RunningThread := False;
      // Removed by Nils: CallStack.CreateWCTSession();
      for I := 0 to CallStack.Count - 1 do
      begin
        if AcceptableCallStackItem(I) then
        begin
          if Empty then
            AddHeader(LineStr);

          if (LastUsedThreadID <> CallStack.Items[I].ThreadID) or
             (RunningThread <> CallStack.Items[I].RunningThread) or
             (CallStack.Items[I].ErrorLine = True) then
          begin
            if (CallStack.Items[i].RunningThread) and (not Empty) then
            begin
              AddLine(SubLineStr);
//              AddLine(FmtCompleteStr('', FLineLen - 2));
            end
            else
              if not Empty then
                AddLine(SubLineStr);
            AddLine(CreateThreadStr(I, Z, CallStack.Items[I], True, 0)); // Removed by Nils: CallStack.FWCH));
            Inc(Z);
            AddLine(SubLineStr);
            LastUsedThreadID := CallStack.Items[I].ThreadID;
            RunningThread := CallStack.Items[I].RunningThread;
          end;
          Empty := False;

          FStr.Add(ItemText[I]);
        end;
      end;

      if (not Empty) or (CaptionHeader <> '') then
        FStr.Add(LineStr);
    finally
      FStr.EndUpdate;
    end;
    FModified := False;
  end;
  Result := FStr;
end;

function TStackFormatter.ContainEncryptedItems(const CallStack: TEurekaBaseStackList): Boolean;
var
  X: Integer;
begin
  Result := False;
  for X := 0 to CallStack.Count - 1 do
    if (Length(CallStack.Item[X].Location.UnitName) > 2) and (CallStack.Item[X].Location.UnitName[1] = '?') and (CallStack.Item[X].Location.UnitName[2] = EUnitEncodedPrefix) then
    begin
      Result := True;
      Break;
    end;
end;

{******************************************************************************}

procedure CustomButtonClick(const Custom: Pointer;
  ExceptionInfo: TEurekaExceptionInfo; Dialog: TObject;
  var CloseDialog: Boolean; var CallNextHandler: Boolean);
var
  ClipboardData: HGLOBAL;
  S: string;
begin
  if (not OpenClipboard(Application.Handle)) then
    MessageBox(0, PChar(SysErrorMessage(GetLastError())), 'Error', MB_OK)
  else
  begin
    try
      EmptyClipboard();

      S := ExceptionInfo.ToString;
      ClipboardData := GlobalAlloc(GMEM_MOVEABLE + GMEM_DDESHARE, (Length(S) + 1) * SizeOf(S[1]));
      StrPCopy(GlobalLock(ClipboardData), S);
      SetClipboardData(CF_UNICODETEXT, ClipboardData);
      GlobalUnlock(ClipboardData);
    finally
      CloseClipboard();
    end;

    CloseDialog := False;
  end;
end;

procedure ExceptionNotify(const Custom: Pointer;
  ExceptionInfo: TEurekaExceptionInfo; var Handle: Boolean;
  var CallNextHandler: Boolean);
var
  CheckOnlineVersionThread: TCheckOnlineVersionThread;
  E: Exception;
  ExceptionClass: TClass;
  ExceptionMessage: string;
  I: Integer;
  Report: string;
begin
  if ((OnlineProgramVersion < 0) and InternetGetConnectedState(nil, 0)) then
  begin
    CheckOnlineVersionThread := TCheckOnlineVersionThread.Create();
    CheckOnlineVersionThread.Execute();
    FreeAndNil(CheckOnlineVersionThread);
  end;

  Handle := Preferences.Version >= OnlineProgramVersion;

  if (GetCurrentThreadId() = MainThreadId) then
    SendMessage(Application.MainFormHandle, UM_CRASH_RESCUE, 0, 0)
  else
    PostMessage(Application.MainFormHandle, UM_CRASH_RESCUE, 0, 0);

  if (not Handle) then
  begin
    MessageBox(0, PChar('Internal Program Error:' + #10 + ExceptionInfo.ExceptionMessage), PChar(Preferences.LoadStr(45)), MB_OK + MB_ICONERROR);

    if ((OnlineProgramVersion > Preferences.Version) and (OnlineProgramVersion > Preferences.ObsoleteVersion)) then
      PostMessage(Application.MainFormHandle, UM_ONLINE_UPDATE_FOUND, 0, 0);
    if (Preferences.ObsoleteVersion < Preferences.Version) then
      Preferences.ObsoleteVersion := Preferences.Version;
  end
  else
  begin
    Report := '';

    try
      if (Preferences.ObsoleteVersion < Preferences.Version) then
        Preferences.ObsoleteVersion := Preferences.Version;
    except
      on E: Exception do
        try SendToDeveloper('EurekaLogExceptionNotify(1)' + #13#10#13#10 + E.Message); except end;
    end;

    try
      Report := Report + LoadStr(1000) + ' ' + Preferences.VersionStr + #13#10#13#10;
    except
      on E: Exception do
        try SendToDeveloper('EurekaLogExceptionNotify(2)' + #13#10#13#10 + E.Message); except end;
    end;

    try
      // In EurekaLog 7.5.0.0 is ExceptionInfo.ExceptionObject not always the exception.
      // Is this a bug of 7.5.0.0?
      if (not (TObject(ExceptionInfo.ExceptionObject) is Exception)) then
        E := nil
      else
        E := ExceptionInfo.ExceptionObject;
    except
      E := nil;
    end;

    if (not Assigned(E)) then
    begin
      Report := Report + ExceptionInfo.ExceptionClass + ':' + #13#10;
      Report := Report + ExceptionInfo.ExceptionMessage + #13#10#13#10;
    end
    else
    begin
      Report := Report + Exception(ExceptionInfo.ExceptionObject).ClassName + ':' + #13#10;
      Report := Report + Exception(ExceptionInfo.ExceptionObject).Message + #13#10#13#10;

      if (E is EOutOfMemory) then
      begin
        Report := Report + 'Free Memory: ' + IntToStr(GetFreeMemory()) + #13#10;
        Report := Report + 'Total Memory: ' + IntToStr(GetMemPhysicalInstalled()) + #13#10#13#10;
      end;
    end;

    try
      if (Assigned(ExceptionInfo.CallStack)) then
      begin
        ExceptionInfo.CallStack.Formatter := TStackFormatter.Create();
        Report := Report + ExceptionInfo.CallStack.ToString + #13#10;
      end;
    except
      on E: Exception do
        try SendToDeveloper('EurekaLogExceptionNotify(5)' + #13#10#13#10 + E.Message); except end;
    end;

    try
      if (GetCurrentThreadId() = MainThreadId) then
      begin
        for I := 0 to Sessions.Count - 1 do
        begin
          Report := Report + #13#10;
          Report := Report + 'MySQL:' + #13#10;
          Report := Report + StringOfChar('-', Length('Version: ' + Sessions[I].Connection.ServerVersionStr)) + #13#10;
          Report := Report + 'Version: ' + Sessions[I].Connection.ServerVersionStr;
          if (Sessions[I].Connection.LibraryType <> MySQLDB.ltBuiltIn) then
            Report := Report + ' (LibraryType: ' + IntToStr(Ord(Sessions[I].Connection.LibraryType)) + ')';
          Report := Report + #13#10#13#10;

          Report := Report + 'SQL Log:' + #13#10;
          Report := Report + StringOfChar('-', 72) + #13#10;
          Report := Report + Sessions[I].Connection.DebugMonitor.CacheText + #13#10;
        end;
      end;
    except
      on E: Exception do
        try SendToDeveloper('EurekaLogExceptionNotify(6)' + #13#10#13#10 + E.Message); except end;
    end;

    try
      MySQLSyncThreads.Lock();
      for I := 0 to MySQLSyncThreads.Count - 1 do
      begin
        Report := Report + #13#10;
        Report := Report + 'SyncThread ID: ' + IntToStr(MySQLSyncThreads[I].ThreadID) + #13#10;
        Report := Report + StringOfChar('-', 72) + #13#10;
        Report := Report + MySQLSyncThreads[I].Log + #13#10;
      end;
      MySQLSyncThreads.Release();
    except
      on E: Exception do
        try SendToDeveloper('EurekaLogExceptionNotify(6)' + #13#10#13#10 + E.Message); except end;
    end;

    try
      SendToDeveloper(Report);
    except
      on E: Exception do
        try SendToDeveloper('EurekaLogExceptionNotify(7)' + #13#10#13#10 + E.Message); except end;
    end;

    try
      ExceptionInfo.Options.EMailSubject
        := SysUtils.LoadStr(1000) + ' ' + IntToStr(Preferences.VerMajor) + '.' + IntToStr(Preferences.VerMinor)
        + ' (Build: ' + IntToStr(Preferences.VerPatch) + '.' + IntToStr(Preferences.VerBuild) + ')'
        + ' - Error Report';
    except
      on E: Exception do
        try SendToDeveloper('EurekaLogExceptionNotify(8)' + #13#10#13#10 + E.Message); except end;
    end;
  end;
end;

initialization
  RegisterEventExceptionNotify(nil, ExceptionNotify);
  RegisterEventCustomButtonClick(nil, CustomButtonClick);
finalization
  UnRegisterEventCustomButtonClick(nil, CustomButtonClick);
  UnRegisterEventExceptionNotify(nil, ExceptionNotify);
end.
