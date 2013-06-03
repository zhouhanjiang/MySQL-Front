unit fProfiling;

interface {********************************************************************}

procedure ProfilingDisablePoint(const Index: Integer);
procedure ProfilingEnablePoint(const Index: Integer);
procedure ProfilingPoint(const Index: Integer);
function ProfilingReport(const Filename: string = ''): string;
procedure ProfilingReset();

implementation {***************************************************************}

uses
  Windows, SysUtils, SyncObjs;

var
  CriticalSection: TCriticalSection;
  LastCount: Int64;
  LocaleFormatSettings: TFormatSettings;
  Points: array of record
    Disabled: Integer;
    Sum: Int64;
  end;

procedure ProfilingDisablePoint(const Index: Integer);
begin
  CriticalSection.Enter();
  Inc(Points[Index].Disabled);
  CriticalSection.Leave();
end;

procedure ProfilingEnablePoint(const Index: Integer);
begin
  CriticalSection.Enter();
  Dec(Points[Index].Disabled);
  CriticalSection.Leave();
end;

procedure ProfilingPoint(const Index: Integer);
var
  Count: Int64;
begin
  CriticalSection.Enter();

  while (Index >= Length(Points)) do
  begin
    SetLength(Points, Length(Points) + 1);
    Points[Length(Points) - 1].Disabled := 0;
    Points[Length(Points) - 1].Sum := 0;
  end;

  if (Points[Index].Disabled = 0) then
  begin
    QueryPerformanceCounter(Count);
    Inc(Points[Index].Sum, Count - LastCount);
    LastCount := Count;
  end;

  CriticalSection.Leave();
end;

function ProfilingReport(const Filename: string = ''): string;
const
  BOM: PAnsiChar = #$FF + #$FE;
var
  BytesWritten: DWord;
  Frequency: Int64;
  Index: Integer;
  Handle: THandle;
  Sum: Int64;
begin
  CriticalSection.Enter();
  Index := Length(Points);
  CriticalSection.Leave();

  ProfilingPoint(Index);

  CriticalSection.Enter();

  Result := '';

  QueryPerformanceFrequency(Frequency);

  Sum := 0;
  for Index := 1 to Length(Points) - 1 do
    Inc(Sum, Points[Index].Sum);

  for Index := 1 to Length(Points) - 1 do
    Result := Result + Format('%2d:  %7s %s  %3d %s' + #13#10, [Index, FormatFloat('#,##0.000', Points[Index].Sum * 1000 div Frequency / 1000, LocaleFormatSettings), 's', Points[Index].Sum * 100 div Sum, '%']);

  Result := Result + Format('----------------------' + #13#10, []);
  Result := Result + Format('     %7s %s  %3d %s' + #13#10, [FormatFloat('#,##0.000', Sum * 1000 div Frequency / 1000, LocaleFormatSettings), 's', 100, '%']);

  if (Filename <> '') then
  begin
    Handle := CreateFile(PChar(Filename),
                         GENERIC_WRITE,
                         FILE_SHARE_READ,
                         nil,
                         CREATE_ALWAYS, 0, 0);
    if (Handle = INVALID_HANDLE_VALUE) then
      RaiseLastOSError()
    else
    begin
      WriteFile(Handle, BOM^, StrLen(BOM), BytesWritten, nil);
      WriteFile(Handle, Result[1], Length(Result) * SizeOf(Result[1]), BytesWritten, nil);
      CloseHandle(Handle);
    end;
  end;

  CriticalSection.Leave();
end;

procedure ProfilingReset();
begin
  CriticalSection.Enter();

  QueryPerformanceCounter(LastCount);

  SetLength(Points, 1);
  Points[0].Disabled := 0;
  Points[0].Sum := 0;

  CriticalSection.Leave();
end;

initialization
  CriticalSection := TCriticalSection.Create();
  LocaleFormatSettings := TFormatSettings.Create(LOCALE_USER_DEFAULT);
  SetLength(Points, 0);
finalization
  SetLength(Points, 0);
  CriticalSection.Free();
end.
