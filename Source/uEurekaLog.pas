unit uEurekaLog;

interface {********************************************************************}

uses
  Classes,
  ECallStack, ETypes;

type
  TEurekaStackFormatter = class(ECallStack.TEurekaStackFormatter)
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
    function GetItemText(const AIndex: Integer): String; override;
    function GetStrings(): TStrings; override;
  end;


implementation {***************************************************************}

uses
  SysUtils,
  EClasses, EStackTracing, ESysInfo, EInfoFormat, EConsts;

{ TEurekaStackFormatter *******************************************************}

function IsAcceptableCallStackItem(const ACallStack: TEurekaBaseStackList; const AInd: Integer; const ADebugDetails: TEurekaDebugDetails; const AAllowedMethods, ADisabledMethods: DWORD): Boolean;
var
  Buffer: TEurekaDebugInfo;
  Item: PEurekaDebugInfo;
begin
  Item := ACallStack.GetItem(AInd, Buffer);

  // Is exception itself (a.k.a. FirstAddr)?
  Result := Item.ErrorLine or
            ((AInd = 0) and (Item.StackAddr = nil)) or // this is FirstAddr, but not marked as error line
            ((AInd > 0) and (Item.StackAddr = nil) and (ACallStack.Items[AInd - 1].ThreadID <> Item.ThreadID)); // this is the same, but in another thread
  if Result then    // always keep first line in any call stack
    Exit;

  // Second+ entries should be filtered upon details/methods
  Result := (Item.Location.DebugDetail in ADebugDetails) and
            (
              (Item.Methods and TracerUndefinedMask <> 0) or
              (
                ((Item.Methods and AAllowedMethods) <> 0) and
                ((Item.Methods and ADisabledMethods) = 0)
              )
            );

  // For pointers into dynamic code blocks (outside of any module) - use return addresses only
  if Result and (Item.Location.DebugDetail = ddNone) then
    Result := Item.Location.Address <> Item.ReturnAddr;
end;

function TEurekaStackFormatter.AcceptableCallStackItem(const AInd: Integer): Boolean;
begin
  Result := IsAcceptableCallStackItem(CallStack, AInd, DebugDetails, AllowedMethods, DisabledMethods);
end;

procedure TEurekaStackFormatter.CalculateLengths();
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

function TEurekaStackFormatter.FormatLine(const AMethods, ADetails, AStackAddress, AAddress, AName, AOffset, AUnit, AClass, AProcedure, ALine: String): String;
begin
  Assert(Calculated);
  Result := Format('|%s|%s|%s|%s|',
    [FmtStrForLog(FmtCompleteStr(AName, FMaxModule)),
     FmtStrForLog(FmtCompleteStr(AUnit, FMaxUnit)),
     FmtStrForLog(FmtCompleteStr(AProcedure, FMaxProc)),
     FmtStrForLog(FmtCompleteStr(ALine, FMaxLine))]);
end;

function TEurekaStackFormatter.GetItemText(const AIndex: Integer): String;

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

function TEurekaStackFormatter.GetStrings: TStrings;

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
      // Nils removed: CallStack.CreateWCTSession();
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
              AddLine(FmtCompleteStr('', FLineLen - 2));
            end
            else
              if not Empty then
                AddLine(SubLineStr);
            AddLine(CreateThreadStr(I, Z, CallStack.Items[I], True, 0)); // Nils removed: CallStack.FWCH));
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

function TEurekaStackFormatter.ContainEncryptedItems(const CallStack: TEurekaBaseStackList): Boolean;
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

end.
