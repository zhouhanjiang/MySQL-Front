unit uDConnecting;

interface {********************************************************************}

uses
  Messages, Classes,
  Forms, Controls,StdCtrls,
  Forms_Ext,
  uSession, uBase;

type
  TDConnecting = class (TForm_Ext)
    FBCancel: TButton;
    FInfo: TLabel;
    procedure AfterConnect(Sender: TObject);
    procedure FBCancelClick(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure FormHide(Sender: TObject);
  private
    procedure UMChangePreferences(var Message: TMessage); message UM_CHANGEPREFERENCES;
  public
    Session: TSSession;
    function Execute(): Boolean;
  end;

function DConnecting(): TDConnecting;

implementation {***************************************************************}

{$R *.dfm}

uses
  Windows, SysUtils, 
  MySQLDB, MySQLConsts,
  uPreferences, uDAccount;

var
  FDConnecting: TDConnecting;

function DConnecting(): TDConnecting;
begin
  if (not Assigned(FDConnecting)) then
  begin
    Application.CreateForm(TDConnecting, FDConnecting);
    FDConnecting.Perform(UM_CHANGEPREFERENCES, 0, 0);
  end;

  Result := FDConnecting;
end;

{ TDOpenConnection ************************************************************}

procedure TDConnecting.AfterConnect(Sender: TObject);
begin
  if (Session.Connection.Connected) then
    ModalResult := mrOk
  else if (((Session.Connection.ErrorCode = ER_ACCESS_DENIED_ERROR) or (Session.Connection.ErrorCode = ER_DBACCESS_DENIED_ERROR)) and Accounts.DBLogin(Session.Account)) then
    PostMessage(Handle, UM_POST_SHOW, 0, 0)
  else
    ModalResult := mrCancel;
end;

procedure TDConnecting.FBCancelClick(Sender: TObject);
begin
  Session.Connection.Terminate();
  ModalResult := mrCancel;
end;

function TDConnecting.Execute(): Boolean;
begin
  Result := ShowModal() = mrOk;
end;

procedure TDConnecting.FormHide(Sender: TObject);
begin
  Session.Connection.AfterConnect := nil;
end;

procedure TDConnecting.FormShow(Sender: TObject);
begin
  Caption := Session.Account.Name;

  Session.Connection.AfterConnect := AfterConnect;

  Session.Connection.Connect();
end;

procedure TDConnecting.UMChangePreferences(var Message: TMessage);
begin
  FInfo.Caption := Preferences.LoadStr(195) + '...';

  FBCancel.Caption := Preferences.LoadStr(30);
end;

initialization
  FDConnecting := nil;
end.

