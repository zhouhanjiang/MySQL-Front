unit fJob;

interface {********************************************************************}

uses
  fPreferences;

type
  TJobExecution = class(TObject)
  private
    Account: TAAccount;
    Job: TAJob;
    function ExecuteExport(): Integer;
  public
    constructor Create(const AccountName, JobName: string);
    destructor Destroy(); override;
    function Execute(): Integer;
  end;

implementation {***************************************************************}

constructor TJobExecution.Create(const AccountName, JobName: string);
begin
  inherited Create();

  Accounts := TAAccounts.Create(nil);

  Account := Accounts.AccountByName(AccountName);
  if (not Assigned(Account)) then
    Job := nil
  else
    Job := Account.JobByName(JobName);
end;

destructor TJobExecution.Destroy();
begin
  Accounts.Free();

  inherited;
end;

function TJobExecution.Execute(): Integer;
begin
  if (Job is TAJobExport) then
    Result := ExecuteExport()
  else
    Result := 1;
end;

function TJobExecution.ExecuteExport(): Integer;
begin
  Result := 0;
end;

end.
