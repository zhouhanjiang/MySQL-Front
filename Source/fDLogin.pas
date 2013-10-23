unit fDLogin;

interface {********************************************************************}

uses
  Windows, Messages, SysUtils, Classes, Controls, Forms,
  Dialogs, StdCtrls, ExtCtrls,
  Forms_Ext, StdCtrls_Ext,
  fSession, fPreferences,
  fBase;


type
  TDLogin = class (TForm_Ext)
    FBCancel: TButton;
    FBOk: TButton;
    FBSettings: TButton;
    FLPassword: TLabel;
    FLUsername: TLabel;
    FPassword: TEdit;
    FUsername: TEdit;
    GAccount: TGroupBox_Ext;
    procedure FBSettingsClick(Sender: TObject);
    procedure FormHide(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure CMChangePreferences(var Message: TMessage); message CM_CHANGEPREFERENCES;
  public
    Filename: TFileName;
    Password: string;
    Account: TAAccount;
    Username: string;
    Window: TForm;
    function Execute(): Boolean;
  end;

function DLogin(): TDLogin;

implementation {***************************************************************}

{$R *.dfm}

uses
  fDAccount;

var
  FLogin: TDLogin;

function DLogin(): TDLogin;
begin
  if (not Assigned(FLogin)) then
  begin
    Application.CreateForm(TDLogin, FLogin);
    FLogin.Perform(CM_CHANGEPREFERENCES, 0, 0);
  end;

  Result := FLogin;
end;

{ TDLogin *********************************************************************}

procedure TDLogin.CMChangePreferences(var Message: TMessage);
begin
  Caption := Preferences.LoadStr(49);

  GAccount.Caption := Preferences.LoadStr(34);
  FLUsername.Caption := Preferences.LoadStr(561) + ':';
  FLPassword.Caption := Preferences.LoadStr(40) + ':';
  FBSettings.Caption := Preferences.LoadStr(27) + '...';

  FBOk.Caption := Preferences.LoadStr(29);
  FBCancel.Caption := Preferences.LoadStr(30);
end;

function TDLogin.Execute(): Boolean;
begin
  ShowModal();
  Result := ModalResult = mrOk;
end;

procedure TDLogin.FBSettingsClick(Sender: TObject);
begin
  DAccount.Account := Account;
  DAccount.Username := Trim(FUsername.Text);
  DAccount.Password := Trim(FPassword.Text);
  DAccount.ShowType := stLogin;
  if (not DAccount.Execute()) then
    ActiveControl := FPassword
  else
    FormShow(Sender);
end;

procedure TDLogin.FormHide(Sender: TObject);
begin
  if (ModalResult = mrOk) then
  begin
    if (Assigned(Account)) then
    begin
      Account.Connection.Username := Trim(FUsername.Text);
      Account.Connection.Password := Trim(FPassword.Text);
    end;
    Username := Trim(FUsername.Text);
    Password := Trim(FPassword.Text);
  end;
end;

procedure TDLogin.FormShow(Sender: TObject);
begin
  if (not Assigned(Account)) then
  begin
    FUsername.Text := '';
    FPassword.Text := '';
  end
  else
  begin
    FUsername.Text := Account.Connection.Username;
    FPassword.Text := Account.Connection.Password;
  end;
  FBSettings.Visible := Assigned(Account);

  ActiveControl := FBCancel;
  if (not Assigned(Account)) then
    ActiveControl := FUsername
  else
    ActiveControl := FPassword;
end;

initialization
  FLogin := nil;
end.
