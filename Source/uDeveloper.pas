unit uDeveloper;

interface {********************************************************************}

uses
  Windows, Classes, WinInet;

type
  THTTPThread = class(TThread)
  public type
    TProgressEvent = procedure (Sender: TObject; const Done, Size: Int64) of object;
  private
    FOnProgress: TProgressEvent;
    SendStream: TStream;
    ReceiveStream: TStream;
    URI: string;
  protected
    procedure Execute(); override;
  public
    constructor Create(const AURI: string; const ASendStream, AReceiveStream: TStream);
    property OnProgress: TProgressEvent read FOnProgress write FOnProgress default nil;
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
procedure SendBugToDeveloper(const Text: string);

var
  OnlineProgramVersion: Integer;
  OnlineRecommendedVersion: Integer;

implementation {***************************************************************}

uses
  XMLIntf, XMLDoc, ActiveX, SysUtils,
  {$IFDEF EurekaLog}
  ExceptionLog,
  {$ENDIF}
  uPreferences;

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

  XML := NewXMLDocument();
  try
    XML.LoadFromStream(Stream, xetUnknown);
  except
    // Corrupt PAD File.
  end;
  if (XML.Active and (Assigned(XML.Node))) then
  begin
    PAD := XML.Node.ChildNodes.FindNode('XML_DIZ_INFO');
    if (Assigned(PAD)) then
    begin
      Major := -1; Minor := -1; Patch := -1; Build := -1;
      Infos := PAD.ChildNodes.FindNode('Program_Info');
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
      begin
        OnlineProgramVersion := EncodeVersion(Major, Minor, Patch, Build);
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

  Result := (OnlineProgramVersion >= 0) and (SetupProgramURI <> '');
end;

procedure SendBugToDeveloper(const Text: string);
var
  Flags: DWORD;
  HTTPThread: THTTPThread;
  Size: Integer;
  Stream: TMemoryStream;
begin
  Stream := TMemoryStream.Create();

  if (not CheckWin32Version(6)) then Flags := 0 else Flags := WC_ERR_INVALID_CHARS;
  Size := WideCharToMultiByte(CP_UTF8, Flags, PChar(Text), Length(Text), nil, 0, nil, nil);
  Stream.SetSize(Size);
  WideCharToMultiByte(CP_UTF8, Flags, PChar(Text), Length(Text), PAnsiChar(Stream.Memory), Stream.Size, nil, nil);

  HTTPThread := THTTPThread.Create(LoadStr(1010), Stream, nil);
  HTTPThread.FreeOnTerminate := True;
  HTTPThread.Start();
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
  Error: Boolean;
  FileSize: Int64;
  Headers: string;
  Index: Cardinal;
  Internet: HInternet;
  L: Longint;
  Request: HInternet;
  RequestTry: Integer;
  Size: Cardinal;
  StatusCode: Word;
  Success: Boolean;
  URLComponents: TURLComponents;
  URLComponentsExtraInfo: array [0 .. INTERNET_MAX_PATH_LENGTH] of Char;
  URLComponentsHostName: array [0 .. INTERNET_MAX_HOST_NAME_LENGTH] of Char;
  URLComponentsPassword: array [0 .. INTERNET_MAX_PASSWORD_LENGTH] of Char;
  URLComponentsPath: array [0 .. INTERNET_MAX_PATH_LENGTH] of Char;
  URLComponentsSchemeName: array [0 .. INTERNET_MAX_SCHEME_LENGTH] of Char;
  URLComponentsUserName: array [0 .. INTERNET_MAX_USER_NAME_LENGTH] of Char;
  Method: PChar;
begin
  {$IFDEF EurekaLog}
  try
  {$ENDIF}

  ReturnValue := 0; RequestTry := 0; Error := False;

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
    ReturnValue := 1
  else
  begin
    L := 120000;
    InternetSetOption(Internet, INTERNET_OPTION_SEND_TIMEOUT, @L, SizeOf(L));
    InternetSetOption(Internet, INTERNET_OPTION_RECEIVE_TIMEOUT, @L, SizeOf(L));

    Client := InternetConnect(Internet, URLComponents.lpszHostName, URLComponents.nPort, URLComponents.lpszUserName, URLComponents.lpszPassword, INTERNET_SERVICE_HTTP, 0, 0);

    if (not Assigned(Client)) then
      ReturnValue := 1
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
        ReturnValue := 1
      else
      begin
        repeat
          FileSize := -1;

          if (not HttpSendRequest(Request, PChar(Headers), Length(Headers), PAnsiChar(Body), Length(Body))) then
            StatusCode := 1
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
            if (Terminated or not HttpQueryInfo(Request, HTTP_QUERY_STATUS_CODE, @Buffer, Size, Index)) then
              StatusCode := 1
            else
              StatusCode := StrToInt(PChar(@Buffer));
          end;

          Inc(RequestTry);
        until (Error or Terminated or (StatusCode = HTTP_STATUS_OK) or (ReceiveStream.Size = 0) or (RequestTry >= 3));

        ReturnValue := StatusCode;
      end;
      InternetCloseHandle(Request);
    end;

    InternetCloseHandle(Internet);
  end;

  if (not Terminated and Assigned(OnProgress)) then
    OnProgress(Self, ReceiveStream.Size, ReceiveStream.Size);

  {$IFDEF EurekaLog}
  except
    StandardEurekaNotify(GetLastExceptionObject(), GetLastExceptionAddress());
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
  inherited;

  if (ReturnValue = HTTP_STATUS_OK) then
  begin
    Preferences.UpdateChecked := Now();

    CoInitialize(nil);
    CheckOnlineVersion(PADFileStream, VersionStr, SetupProgramURI);
    CoUninitialize();
  end;
end;

end.
