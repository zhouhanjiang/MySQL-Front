unit fDConnecting;

interface {********************************************************************}

uses
  Messages, Classes,
  Forms, Controls,StdCtrls,
  Forms_Ext,
  fSession, fBase;

type
  TDConnecting = class (TForm_Ext)
    FBCancel: TButton;
    FInfo: TLabel;
    procedure AfterConnect(Sender: TObject);
    procedure FBCancelClick(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure CMChangePreferences(var Message: TMessage); message CM_CHANGEPREFERENCES;
    procedure CMPostShow(var Message: TMessage); message CM_POST_SHOW;
    procedure FormHide(Sender: TObject);
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
  fPreferences, fDAccount;

var
  FConnecting: TDConnecting;

function DConnecting(): TDConnecting;
begin
  if (not Assigned(FConnecting)) then
  begin
    Application.CreateForm(TDConnecting, FConnecting);
    FConnecting.Perform(CM_CHANGEPREFERENCES, 0, 0);
  end;

  Result := FConnecting;
end;

{ TDOpenConnection ************************************************************}

procedure TDConnecting.AfterConnect(Sender: TObject);
begin
  if (Session.Connection.Connected) then
    ModalResult := mrOk
  else if (((Session.Connection.ErrorCode = ER_ACCESS_DENIED_ERROR) or (Session.Connection.ErrorCode = ER_DBACCESS_DENIED_ERROR)) and Accounts.DBLogin(Session.Account)) then
    PostMessage(Handle, CM_POST_SHOW, 0, 0)
  else
    ModalResult := mrCancel;
end;

procedure TDConnecting.CMChangePreferences(var Message: TMessage);
begin
  FInfo.Caption := Preferences.LoadStr(195) + '...';

  FBCancel.Caption := Preferences.LoadStr(30);
end;

procedure TDConnecting.CMPostShow(var Message: TMessage);
begin
  Session.Connection.Connect();
end;

procedure TDConnecting.FBCancelClick(Sender: TObject);
begin
  Session.Connection.Terminate();
  ModalResult := mrCancel;
end;

function TDConnecting.Execute(): Boolean;
begin
  ShowModal();
  Result := ModalResult = mrOk;
end;

procedure TDConnecting.FormHide(Sender: TObject);
begin
  Session.Connection.AfterConnect := nil;
  Session := nil;
end;

procedure TDConnecting.FormShow(Sender: TObject);
begin
  Caption := Session.Account.Name;

  Session.Connection.AfterConnect := AfterConnect;

  PostMessage(Handle, CM_POST_SHOW, 0, 0);
end;

initialization
  FConnecting := nil;
end.

