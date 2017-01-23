unit uDeveloper;

interface {********************************************************************}

uses
  Windows, Classes, WinInet;

type
  THTTPThread = class(TThread)
  public type
    TProgressEvent = procedure (Sender: TObject; const Done, Size: Int64) of object;
  private
    FErrorCode: Integer;
    FErrorMessage: string;
    FHTTPMessage: string;
    FHTTPStatus: Integer;
    FOnProgress: TProgressEvent;
    SendStream: TStream;
    ReceiveStream: TStream;
    URI: string;
  protected
    procedure Execute(); override;
  public
    constructor Create(const AURI: string; const ASendStream, AReceiveStream: TStream);
    property OnProgress: TProgressEvent read FOnProgress write FOnProgress default nil;
    property ErrorCode: Integer read FErrorCode;
    property ErrorMessage: string read FErrorMessage;
    property HTTPMessage: string read FHTTPMessage;
    property HTTPStatus: Integer read FHTTPStatus;
  end;

  TCheckOnlineVersionThread = class(THTTPThread)
  private
    PADFileStream: TStringStream;
  public
    constructor Create();
    destructor Destroy(); override;
    procedure Execute(); override;
  end;

function CheckOnlineVersion(const Stream: TStringStream; var VersionStr: string; var SetupProgramURI: string): Boolean;
function CompileTime(): TDateTime;
procedure SendToDeveloper(const Text: string; const Days: Integer = 7; const DisableSource: Boolean = False);

var
  OnlineVersion: Integer;
  OnlineRecommendedVersion: Integer;

implementation {***************************************************************}

uses
  XMLIntf, XMLDoc, ActiveX, SysUtils, SyncObjs, DateUtils,
  Forms,
  {$IFDEF EurekaLog}
  ExceptionLog7, EExceptionManager, ECallStack, EStackTracing, EClasses,
  ETypes, EException, ESysInfo, EInfoFormat, EThreadsManager, EConsts,
  EWCTSupport, EEvents, ELogBuilder,
  {$ENDIF}
  MySQLDB,
  uPreferences, uSession,
  uBase;

var
  SendThreads: TList;
  {$IFDEF EurekaLog}
  CheckOnlineVersionThreadErrorCode: Integer;
  CheckOnlineVersionThreadHTTPStatus: Integer;
  {$ENDIF}
  CheckOnlineVersionProgress: string;

{******************************************************************************}

function CheckOnlineVersion(const Stream: TStringStream; var VersionStr: string; var SetupProgramURI: string): Boolean;
var
  Build: Integer;
  Infos: IXMLNode;
  Major: Integer;
  Minor: Integer;
  PAD: IXMLNode;
  Patch: Integer;
  Node: IXMLNode;
  XML: IXMLDocument;
begin
  SetupProgramURI := '';

  CheckOnlineVersionProgress := CheckOnlineVersionProgress + 'a';

  XML := NewXMLDocument();
  try
    XML.LoadFromStream(Stream, xetUnknown);
  CheckOnlineVersionProgress := CheckOnlineVersionProgress + 'b';
  except
    // Corrupt PAD File.
  end;
  if (XML.Active and (Assigned(XML.Node))) then
  begin
  CheckOnlineVersionProgress := CheckOnlineVersionProgress + 'c';
    PAD := XML.Node.ChildNodes.FindNode('XML_DIZ_INFO');
    if (Assigned(PAD)) then
    begin
  CheckOnlineVersionProgress := CheckOnlineVersionProgress + 'd';
      Major := -1; Minor := -1; Patch := -1; Build := -1;
      Infos := PAD.ChildNodes.FindNode('Program_Info');
      if (Assigned(Infos)) then
      begin
  CheckOnlineVersionProgress := CheckOnlineVersionProgress + 'e';
        Node := Infos.ChildNodes.FindNode('Program_Version_Major');
        if (Assigned(Node)) and (Node.IsTextElement) then
          Major := StrToInt(Node.GetText());
        Node := Infos.ChildNodes.FindNode('Program_Version_Minor');
        if (Assigned(Node)) and (Node.IsTextElement) then
          Minor := StrToInt(Node.GetText());
        Node := Infos.ChildNodes.FindNode('Program_Version_Patch');
        if (Assigned(Node)) and (Node.IsTextElement) then
          Patch := StrToInt(Node.GetText());
        Node := Infos.ChildNodes.FindNode('Program_Version_Build');
        if (Assigned(Node)) and (Node.IsTextElement) then
          Build := StrToInt(Node.GetText());
      end;
      if ((Major >= 0) and (Minor >= 0) and (Patch >= 0) and (Build >= 0)) then
      begin
  CheckOnlineVersionProgress := CheckOnlineVersionProgress + 'f';
        OnlineVersion := EncodeVersion(Major, Minor, Patch, Build);
        VersionStr := IntToStr(Major) + '.' + IntToStr(Minor) + '  (Build ' + IntToStr(Patch) + '.' + IntToStr(Build) + ')';
      end;

      Major := -1; Minor := -1; Patch := -1; Build := -1;
      Infos := PAD.ChildNodes.FindNode('Recommended_Info');
      if (Assigned(Infos)) then
      begin
        Node := Infos.ChildNodes.FindNode('Program_Version_Major');
        if (Assigned(Node)) and (Node.IsTextElement) then
          Major := StrToInt(Node.GetText());
        Node := Infos.ChildNodes.FindNode('Program_Version_Minor');
        if (Assigned(Node)) and (Node.IsTextElement) then
          Minor := StrToInt(Node.GetText());
        Node := Infos.ChildNodes.FindNode('Program_Version_Patch');
        if (Assigned(Node)) and (Node.IsTextElement) then
          Patch := StrToInt(Node.GetText());
        Node := Infos.ChildNodes.FindNode('Program_Version_Build');
        if (Assigned(Node)) and (Node.IsTextElement) then
          Build := StrToInt(Node.GetText());
      end;
      if ((Major >= 0) and (Minor >= 0) and (Patch >= 0) and (Build >= 0)) then
        OnlineRecommendedVersion := EncodeVersion(Major, Minor, Patch, Build);

      Infos := PAD.ChildNodes.FindNode('Web_Info');
      if (Assigned(Node)) then
      begin
        Node := Infos.ChildNodes.FindNode('Download_URLs');
        if (Assigned(Node)) then
        begin
          Node := Node.ChildNodes.FindNode('Primary_Download_URL');
          if (Assigned(Node)) and (Node.IsTextElement) then
            SetupProgramURI := Node.Text;
        end;
      end;
    end;
  end;

  Result := (OnlineVersion >= 0) and (SetupProgramURI <> '');
end;

function CompileTime(): TDateTime;
begin
  Result := PImageNtHeaders(HInstance + Cardinal(PImageDosHeader(HInstance)^._lfanew))^.FileHeader.TimeDateStamp / SecsPerDay + UnixDateDelta;
end;

procedure SendToDeveloper(const Text: string; const Days: Integer = 7; const DisableSource: Boolean = False);
var
  {$IFDEF EurekaLog}
  Buffer: TEurekaDebugInfo;
  CallStack: TEurekaBaseStackList;
  Filename: array[0 .. MAX_PATH] of Char;
  Index: Integer;
  Item: PEurekaDebugInfo;
  StackItem: Integer;
  Source: string;
  {$ENDIF}
  Body: String;
  Flags: DWORD;
  Thread: THTTPThread;
  Size: Integer;
  Stream: TMemoryStream;
begin
  if ((Days = 0) or (Now() < IncDay(CompileTime(), Days + 1))) then
  begin
    Body := Text;

    {$IFDEF EurekaLog}
    if (not DisableSource or (Trim(Text) = '')) then
    begin
      CallStack := GetCurrentCallStack();
      Index := 0; StackItem := 0; Item := nil;
      while ((Index < CallStack.Count) and (StackItem < 2)) do
      begin
        Item := CallStack.GetItem(1, Buffer); GetModuleFileName(GetModuleHandle(nil), @Filename, Length(Filename));
        if ((Item^.Location.DebugDetail = ddSourceCode) and (StrIComp(PChar(Item^.Location.ModuleName), PChar(@Filename)) = 0)) then
          Inc(StackItem);
        Inc(Index);
      end;
      if (Assigned(Item) and (StackItem = 2)) then
      begin
        if ((Item^.Location.ClassName <> '') and (Item^.Location.ProcedureName <> '')) then
          Source := Item^.Location.ClassName + '.' + Item^.Location.ProcedureName
        else if (Item^.Location.ClassName <> '') then
          Source := Item^.Location.ClassName
        else if (Item^.Location.ProcedureName <> '') then
          Source := Item^.Location.ProcedureName
        else
          Source := '';
        Source := ExtractFileName(Item^.Location.ModuleName) + '|' + Item^.Location.UnitName + '|' + Source + '|' + IntToStr(Item^.Location.LineNumber) + '[' + IntToStr(Item^.Location.ProcOffsetLine) + ']' + #13#10#13#10;

        Body := Source + Body;
      end;
    end;
    {$ENDIF}

    Stream := TMemoryStream.Create();

    if (not CheckWin32Version(6)) then Flags := 0 else Flags := WC_ERR_INVALID_CHARS;
    Size := WideCharToMultiByte(CP_UTF8, Flags, PChar(Body), Length(Body), nil, 0, nil, nil);
    Stream.SetSize(Size);
    WideCharToMultiByte(CP_UTF8, Flags, PChar(Body), Length(Body), PAnsiChar(Stream.Memory), Stream.Size, nil, nil);

    Thread := THTTPThread.Create(LoadStr(1006), Stream, nil);
    SendThreads.Add(Thread);
    Thread.Start();
  end;
end;

{ THTTPThread *****************************************************************}

constructor THTTPThread.Create(const AURI: string; const ASendStream, AReceiveStream: TStream);
begin
  inherited Create(True);

  URI := AURI;
  SendStream := ASendStream;
  ReceiveStream := AReceiveStream;
end;

procedure THTTPThread.Execute();
const
  GET: PChar = 'GET';
  POST: PChar = 'POST';
var
  Body: RawByteString;
  Buffer: array[0 .. 32768 - 1] of Byte;
  Client: HInternet;
  FileSize: Int64;
  Headers: string;
  Index: Cardinal;
  Internet: HInternet;
  L: Longint;
  Len: Integer;
  MessageBuffer: array [0 .. 2048 - 1] of Char;
  Method: PChar;
  QueryIndex: DWORD;
  QueryInfo: array [0..2048] of Char;
  Request: HInternet;
  RequestTry: Integer;
  Size: Cardinal;
  Success: Boolean;
  URLComponents: TURLComponents;
  URLComponentsExtraInfo: array [0 .. INTERNET_MAX_PATH_LENGTH] of Char;
  URLComponentsHostName: array [0 .. INTERNET_MAX_HOST_NAME_LENGTH] of Char;
  URLComponentsPassword: array [0 .. INTERNET_MAX_PASSWORD_LENGTH] of Char;
  URLComponentsPath: array [0 .. INTERNET_MAX_PATH_LENGTH] of Char;
  URLComponentsSchemeName: array [0 .. INTERNET_MAX_SCHEME_LENGTH] of Char;
  URLComponentsUserName: array [0 .. INTERNET_MAX_USER_NAME_LENGTH] of Char;

  // Debug 2017-01-23
  lpszHeaders: LPWSTR;
  dwHeadersLength: DWORD;
  lpOptional: Pointer;
  dwOptionalLength: DWORD;
begin
  {$IFDEF EurekaLog}
  try
    SetEurekaLogStateInThread(0, True);
  {$ENDIF}

  FErrorCode := 0; FErrorMessage := '';
  FHTTPStatus := HTTP_STATUS_OK; FHTTPMessage := '';
  RequestTry := 0;

  URLComponents.dwStructSize := SizeOf(URLComponents);
  URLComponents.dwSchemeLength := Length(URLComponentsSchemeName);
  URLComponents.dwHostNameLength := Length(URLComponentsHostName);
  URLComponents.dwUserNameLength := Length(URLComponentsUserName);
  URLComponents.dwPasswordLength := Length(URLComponentsPassword);
  URLComponents.dwUrlPathLength := Length(URLComponentsPath);
  URLComponents.dwExtraInfoLength := Length(URLComponentsExtraInfo);
  URLComponents.lpszScheme := @URLComponentsSchemeName;
  URLComponents.lpszHostName := @URLComponentsHostName;
  URLComponents.lpszUserName := @URLComponentsUserName;
  URLComponents.lpszPassword := @URLComponentsPassword;
  URLComponents.lpszUrlPath := @URLComponentsPath;
  URLComponents.lpszExtraInfo := @URLComponentsExtraInfo;

  if (not InternetCrackUrl(PChar(URI), Length(URI), ICU_DECODE, URLComponents)) then
    Internet := nil
  else
    Internet := InternetOpen(PChar(Preferences.InternetAgent), INTERNET_OPEN_TYPE_PRECONFIG, nil, nil, 0);

  if (not Assigned(Internet)) then
    FErrorCode := GetLastError()
  else
  begin
    L := 120000;
    InternetSetOption(Internet, INTERNET_OPTION_SEND_TIMEOUT, @L, SizeOf(L));
    InternetSetOption(Internet, INTERNET_OPTION_RECEIVE_TIMEOUT, @L, SizeOf(L));

    Client := InternetConnect(Internet, URLComponents.lpszHostName, URLComponents.nPort, URLComponents.lpszUserName, URLComponents.lpszPassword, INTERNET_SERVICE_HTTP, 0, 0);

    if (not Assigned(Client)) then
      FErrorCode := GetLastError()
    else
    begin
      if (not Assigned(SendStream)) then
      begin
        Method := GET;
        Headers := '';
        Body := '';
      end
      else
      begin
        Method := POST;
        Headers := 'Content-Type: text/plain; charset=UTF-8' + #10
          + 'Content-Transfer-Encoding: binary' + #10
          + 'Program-Version: ' + IntToStr(Preferences.VerMajor) + '.' + IntToStr(Preferences.VerMinor) + '.' + IntToStr(Preferences.VerPatch) + '.' + IntToStr(Preferences.VerBuild) + #10;
        SetLength(Body, SendStream.Size);
        SendStream.Seek(0, soBeginning);
        SendStream.Read(PAnsiChar(Body)^, SendStream.Size);
        SendStream.Free();
      end;

      Request := HttpOpenRequest(Client, Method, PChar(StrPas(URLComponents.lpszUrlPath) + StrPas(URLComponents.lpszExtraInfo)), 'HTTP/1.1', nil, nil, INTERNET_FLAG_RELOAD + INTERNET_FLAG_NO_CACHE_WRITE, 0);

      if (not Assigned(Request)) then
        FErrorCode := GetLastError()
      else
      begin
        repeat
          FileSize := -1;

          // Debug 2017-01-23
          lpszHeaders := PChar(Headers);
          dwHeadersLength := Length(Headers);
          lpOptional := PAnsiChar(Body);
          dwOptionalLength := Length(Body);
          // ... an AV occurred in the following line...

          if (not HttpSendRequest(Request, lpszHeaders, dwHeadersLength, lpOptional, dwOptionalLength)) then
            FErrorCode := GetLastError()
          else
          begin
            Size := SizeOf(Buffer); Index := 0;
            if (HttpQueryInfo(Request, HTTP_QUERY_CONTENT_LENGTH, @Buffer, Size, Index)) then
              FileSize := StrToInt(PChar(@Buffer));

            if (Assigned(ReceiveStream)) then
              repeat
                Success := InternetReadFile(Request, @Buffer, SizeOf(Buffer), Size);
                if (Success and (Size > 0)) then
                  ReceiveStream.Write(Buffer, Size);

                if (not Terminated and Assigned(OnProgress) and (ReceiveStream.Size < FileSize)) then
                  OnProgress(Self, ReceiveStream.Size, FileSize);
              until (Terminated or (Success and (Size = 0)));

            Size := SizeOf(Buffer); Index := 0;
            if (not Terminated) then
              if (not HttpQueryInfo(Request, HTTP_QUERY_STATUS_CODE, @Buffer, Size, Index)) then
                FErrorCode := GetLastError()
              else
              begin
                FHTTPStatus := StrToInt(PChar(@Buffer));
                QueryIndex := Length(QueryInfo);
                if (HttpQueryInfo(Request, HTTP_QUERY_STATUS_TEXT, @QueryInfo, Size, Index)) then
                  SetString(FHTTPMessage, PChar(@QueryInfo[0]), QueryIndex);
              end;
          end;

          Inc(RequestTry);
        until (Terminated or (FErrorCode <> 0) or (FHTTPStatus = HTTP_STATUS_OK) or Assigned(ReceiveStream) and (ReceiveStream.Size = 0) or (RequestTry >= 3));
      end;
      InternetCloseHandle(Request);
    end;

    InternetCloseHandle(Internet);
  end;

  if (FErrorCode <> 0) then
    if ((INTERNET_ERROR_BASE <= FErrorCode) and (FErrorCode <= INTERNET_ERROR_LAST)) then
    begin
      Len := FormatMessage(FORMAT_MESSAGE_FROM_HMODULE,
        Pointer(GetModuleHandle('Wininet.dll')), FErrorCode, 0, @MessageBuffer, Length(MessageBuffer), nil);
      while (Len > 0) and (CharInSet(MessageBuffer[Len - 1], [#0..#32])) do Dec(Len);
      SetString(FErrorMessage, PChar(@MessageBuffer[0]), Len);
    end
    else
      FErrorMessage := SysErrorMessage(FErrorCode);

  if (not Terminated and Assigned(OnProgress)) then
    OnProgress(Self, ReceiveStream.Size, ReceiveStream.Size);

  {$IFDEF EurekaLog}
  except
    on E: Exception do
      ExceptionManager.StandardEurekaNotify(E);
  end;
  {$ENDIF}
end;

{ TCheckOnlineVersionThread ***************************************************}

constructor TCheckOnlineVersionThread.Create();
begin
  PADFileStream := TStringStream.Create();

  inherited Create(SysUtils.LoadStr(1005), nil, PADFileStream);
end;

destructor TCheckOnlineVersionThread.Destroy();
begin
  PADFileStream.Free();

  inherited;
end;

procedure TCheckOnlineVersionThread.Execute();
var
  SetupProgramURI: string;
  VersionStr: string;
begin
  CheckOnlineVersionProgress := CheckOnlineVersionProgress + '1';

  inherited;

  if (ReturnValue = HTTP_STATUS_OK) then
  begin
  CheckOnlineVersionProgress := CheckOnlineVersionProgress + '2';
    Preferences.UpdateChecked := Now();

    CoInitialize(nil);
  CheckOnlineVersionProgress := CheckOnlineVersionProgress + '3';
    CheckOnlineVersion(PADFileStream, VersionStr, SetupProgramURI);
  CheckOnlineVersionProgress := CheckOnlineVersionProgress + '4';
    CoUninitialize();
  end;
  CheckOnlineVersionProgress := CheckOnlineVersionProgress + '5';
end;

{$IFDEF EurekaLog}

{ TEurekaStackFormatter *******************************************************}

type
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

    FLineLen := FMaxModule + FMaxUnit + FMaxProc + FMaxLine + 5;
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

{ TLogBuilder *****************************************************************}

type
  TLogBuilder = class(ELogBuilder.TLogBuilder)
  protected
    function BuildReport(): String; override;
  end;

function TLogBuilder.BuildReport(): String;
var
  I: Integer;
  ExceptionMessage: string;
begin
  ExceptionMessage := Trim(ExceptionInfo.ExceptionMessage);
  if ((Length(ExceptionMessage) > 0) and (ExceptionMessage[Length(ExceptionMessage)] = '.')) then
    Delete(ExceptionMessage, Length(ExceptionMessage), 1);

  Result := ExceptionInfo.ExceptionClass + ':' + #13#10;
  Result := Result + ExceptionMessage + #13#10#13#10;

  if (ExceptionInfo.ExceptionClass = 'EOutOfMemory') then
  begin
    Result := Result + 'Free Memory: ' + IntToStr(GetFreeMemory()) + #13#10;
    Result := Result + 'Total Memory: ' + IntToStr(GetMemPhysicalInstalled()) + #13#10#13#10;
  end;

  if (ExceptionInfo.ExceptionClass = 'EFrozenApplication') then
    Result := Result + 'FreezeTimeout: ' + IntToStr(CurrentEurekaLogOptions.FreezeTimeout) + ' Seconds' + #13#10#13#10;

  if (Assigned(ExceptionInfo.CallStack)) then
  begin
    ExceptionInfo.CallStack.Formatter := TStackFormatter.Create();
    Result := Result + ExceptionInfo.CallStack.ToString + #13#10;
  end;

  if (GetCurrentThreadId() = MainThreadId) then
    for I := 0 to Sessions.Count - 1 do
    begin
      Result := Result + #13#10;
      Result := Result + 'MySQL: ' + Sessions[I].Connection.ServerVersionStr;
      if (Sessions[I].Connection.LibraryType <> MySQLDB.ltBuiltIn) then
        Result := Result + ' (LibraryType: ' + IntToStr(Ord(Sessions[I].Connection.LibraryType)) + ')';
      Result := Result + #13#10;
      Result := Result + StringOfChar('-', 72) + #13#10;
      Result := Result + Sessions[I].Connection.DebugMonitor.CacheText + #13#10;
    end;

  if (OnlineVersion <= 0) then
  begin
    Result := Result + #13#10;
    Result := Result + 'OnlineVersion: ' + IntToStr(OnlineVersion) + #13#10;
    Result := Result + 'InternetConneced: ' + BoolToStr(InternetGetConnectedState(nil, 0), True) + #13#10;
    Result := Result + 'CheckOnlineVersionThreadErrorCode: ' + IntToStr(CheckOnlineVersionThreadErrorCode) + #13#10;
    Result := Result + 'CheckOnlineVersionThreadHTTPStatus: ' + IntToStr(CheckOnlineVersionThreadHTTPStatus) + #13#10;
    Result := Result + 'CheckOnlineVersionProgress: ' + CheckOnlineVersionProgress + #13#10;
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

      S := BuildBugReport(ExceptionInfo);
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
  ExceptionInfo: TEurekaExceptionInfo; var ShowDialog: Boolean;
  var CallNextHandler: Boolean);
var
  CheckOnlineVersionThread: TCheckOnlineVersionThread;
begin
  if ((OnlineVersion < 0) and InternetGetConnectedState(nil, 0)) then
  begin
    CheckOnlineVersionThread := TCheckOnlineVersionThread.Create();
    CheckOnlineVersionThread.Execute();
    CheckOnlineVersionThreadErrorCode := CheckOnlineVersionThread.ErrorCode;
    CheckOnlineVersionThreadHTTPStatus := CheckOnlineVersionThread.HTTPStatus;
    FreeAndNil(CheckOnlineVersionThread);
  end;

  if (Preferences.ObsoleteVersion < Preferences.Version) then
    Preferences.ObsoleteVersion := OnlineVersion;

  if (GetCurrentThreadId() = MainThreadId) then
    SendMessage(Application.MainFormHandle, UM_CRASH_RESCUE, 0, 0)
  else
    PostMessage(Application.MainFormHandle, UM_CRASH_RESCUE, 0, 0);

  if (ExceptionInfo.ExceptionClass = 'EFrozenApplication') then
  begin
    ShowDialog := False;

    SendToDeveloper(BuildBugReport(ExceptionInfo), 0, True);
  end
  else
    ShowDialog := (Preferences.Version >= OnlineVersion);

  if (not ShowDialog) then
  begin
    MessageBox(0, PChar('Internal Program Error:' + #10 + ExceptionInfo.ExceptionMessage), PChar(Preferences.LoadStr(45)), MB_OK + MB_ICONERROR);

    if ((OnlineVersion > Preferences.Version) and (OnlineVersion > Preferences.ObsoleteVersion)) then
      PostMessage(Application.MainFormHandle, UM_ONLINE_UPDATE_FOUND, 0, 0);
    if (Preferences.ObsoleteVersion < Preferences.Version) then
      Preferences.ObsoleteVersion := Preferences.Version;
  end
  else
  begin
    SendToDeveloper(BuildBugReport(ExceptionInfo), 0, True);

    ExceptionInfo.Options.EMailSubject
      := SysUtils.LoadStr(1000) + ' ' + IntToStr(Preferences.VerMajor) + '.' + IntToStr(Preferences.VerMinor)
      + ' (Build: ' + IntToStr(Preferences.VerPatch) + '.' + IntToStr(Preferences.VerBuild) + ')'
      + ' - Error Report';
  end;
end;
{$ENDIF}

initialization
  {$IFDEF EurekaLog}
  CheckOnlineVersionThreadErrorCode := -1;
  CheckOnlineVersionThreadHTTPStatus := -1;
  CheckOnlineVersionProgress := '';

  LogBuilderClass := TLogBuilder;
  RegisterEventExceptionNotify(nil, ExceptionNotify);
  RegisterEventCustomButtonClick(nil, CustomButtonClick);
  {$ENDIF}

  SendThreads := TList.Create();
  OnlineVersion := -1;
finalization
  while (SendThreads.Count > 0) do
  begin
    TThread(SendThreads[0]).WaitFor();
    TThread(SendThreads[0]).Free();
    SendThreads.Delete(0);
  end;
  SendThreads.Free();

  {$IFDEF EurekaLog}
  UnRegisterEventCustomButtonClick(nil, CustomButtonClick);
  UnRegisterEventExceptionNotify(nil, ExceptionNotify);
  {$ENDIF}
end.
