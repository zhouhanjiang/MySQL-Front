unit fDAccount;

interface {********************************************************************}

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, ComCtrls, ExtCtrls, Menus,
  SynEdit, SynMemo,
  ComCtrls_Ext, Forms_Ext, StdCtrls_Ext, ExtCtrls_Ext,
  fPreferences, fBase, fSession;

type
  TDAccountShowType = (stDefault, stLogin);

type
  TDAccount = class (TForm_Ext)
    FBCancel: TButton;
    FBDatabase: TButton;
    FBHelp: TButton;
    FBOk: TButton;
    FConnectionType: TComboBox_Ext;
    FDatabase: TEdit;
    FHost: TEdit;
    FHTTPTunnelURI: TEdit;
    FLConnectionType: TLabel;
    FLDatabase: TLabel;
    FLHost: TLabel;
    FLHTTPTunnelURI: TLabel;
    FLibraryFilename: TEdit;
    FLLibraryFilename: TLabel;
    FLName: TLabel;
    FLPassword: TLabel;
    FLPort: TLabel;
    FLUser: TLabel;
    FName: TEdit;
    FPassword: TEdit;
    FPort: TEdit;
    FUDPort: TUpDown;
    FUser: TEdit;
    GBasics: TGroupBox_Ext;
    GLogin: TGroupBox_Ext;
    GConnection: TGroupBox_Ext;
    procedure FBDatabaseClick(Sender: TObject);
    procedure FBHelpClick(Sender: TObject);
    procedure FConnectionTypeChange(Sender: TObject);
    procedure FEditChange(Sender: TObject);
    procedure FHostExit(Sender: TObject);
    procedure FHTTPTunnelURIChange(Sender: TObject);
    procedure FHTTPTunnelURIEnter(Sender: TObject);
    procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);
    procedure FormCreate(Sender: TObject);
    procedure FormShow(Sender: TObject);
    function GetAccountName(): string;
    procedure FormHide(Sender: TObject);
    procedure FormResize(Sender: TObject);
    procedure FormCanResize(Sender: TObject; var NewWidth, NewHeight: Integer;
      var Resize: Boolean);
    procedure FHostChange(Sender: TObject);
    procedure FLibraryFilenameEnter(Sender: TObject);
  private
    function CheckConnectInfos(): Boolean;
    procedure CMSysFontChanged(var Message: TMessage); message CM_SYSFONTCHANGED;
    procedure UMChangePreferences(var Message: TMessage); message UM_CHANGEPREFERENCES;
  public
    Password: string;
    Account: TPAccount;
    ShowType: TDAccountShowType;
    Username: string;
    function Execute(): Boolean;
    property AccountName: string read GetAccountName;
  end;

function DAccount(): TDAccount;

implementation {***************************************************************}

{$R *.dfm}

uses
  WinINet, UITypes,
  StrUtils,
  MySQLConsts,
  MySQLDB,
  fDDatabases;

var
  FAccount: TDAccount;

function DAccount(): TDAccount;
begin
  if (not Assigned(FAccount)) then
  begin
    Application.CreateForm(TDAccount, FAccount);
    FAccount.Perform(UM_CHANGEPREFERENCES, 0, 0);
  end;

  Result := FAccount;
end;

{ TDAccount *******************************************************************}

function TDAccount.CheckConnectInfos(): Boolean;
begin
  Result := False;

  if (FHost.Text = '') then
    begin MessageBeep(MB_ICONERROR); ActiveControl := FHost; end
  else if ((FConnectionType.ItemIndex = 1) and (FLibraryFilename.Text = '')) then
    begin MessageBeep(MB_ICONERROR); ActiveControl := FLibraryFilename; end
  else if ((FConnectionType.ItemIndex = 2) and (FHTTPTunnelURI.Text = '')) then
    begin MessageBeep(MB_ICONERROR); ActiveControl := FHTTPTunnelURI; end
  else
    Result := True;
end;

procedure TDAccount.CMSysFontChanged(var Message: TMessage);
begin
  inherited;

  FBDatabase.Height := FDatabase.Height;
end;

function TDAccount.Execute(): Boolean;
begin
  ShowModal();
  Result := ModalResult = mrOk;
end;

procedure TDAccount.FBDatabaseClick(Sender: TObject);
var
  LibraryName: string;
  LibraryType: TMySQLLibrary.TLibraryType;
  Session: TSSession;
begin
  if (CheckConnectInfos()) then
  begin
    Session := TSSession.Create(Sessions);
    if (Assigned(Session)) then
    begin
      case (FConnectionType.ItemIndex) of
        1: LibraryType := ltDLL;
        2: LibraryType := ltHTTP;
        else LibraryType := ltBuiltIn;
      end;
      case (FConnectionType.ItemIndex) of
        1: LibraryName := FLibraryFilename.Text;
        2: LibraryName := FHTTPTunnelURI.Text;
        else LibraryName := '';
      end;

      Session.Connection.BeginSilent();
      Session.Connection.Connect(LibraryType, LibraryName, FHost.Text, FUser.Text, FPassword.Text, '', FUDPort.Position, True);
      if (Session.Connection.ErrorCode <> 0) then
        Session.Connection.OnSQLError(Session.Connection, Session.Connection.ErrorCode, Session.Connection.ErrorMessage)
      else if (Session.Connection.Connected) then
      begin
        DDatabases.Session := Session;
        DDatabases.SelectedDatabases := FDatabase.Text;
        if (DDatabases.Execute()) then
          FDatabase.Text := DDatabases.SelectedDatabases;
      end;
      Session.Connection.EndSilent();

      Session.Free();
    end;

    ActiveControl := FDatabase;
  end;
end;

procedure TDAccount.FBHelpClick(Sender: TObject);
begin
  Application.HelpContext(HelpContext)
end;

procedure TDAccount.FConnectionTypeChange(Sender: TObject);
begin
  FLibraryFilename.Visible := FConnectionType.ItemIndex = 1;
  FHTTPTunnelURI.Visible := FConnectionType.ItemIndex = 2;
  FLLibraryFilename.Visible := FLibraryFilename.Visible;
  FLHTTPTunnelURI.Visible := FHTTPTunnelURI.Visible;

  if (FConnectionType.ItemIndex = 2) then
    FHTTPTunnelURIChange(Sender);
end;

procedure TDAccount.FEditChange(Sender: TObject);
var
  Name: string;
begin
  if (FName.Text = '') then
    Name := FHost.Text
  else
    Name := FName.Text;
  FBOk.Enabled := (FHost.Text <> '')
    and (not Assigned(Accounts.AccountByName(Name)) or Assigned(Account) and (Accounts.AccountByName(Name) = Account));
  FBDatabase.Enabled := FHost.Text <> '';
end;

procedure TDAccount.FHostChange(Sender: TObject);
begin
  FPort.Visible := Trim(FHost.Text) <> LOCAL_HOST_NAMEDPIPE;
  FUDPort.Visible := FPort.Visible;
  FLPort.Visible := FPort.Visible;

  FEditChange(Sender);
end;

procedure TDAccount.FHostExit(Sender: TObject);
var
  Index: Integer;
begin
  if (FName.Text = '') then
    if (not Assigned(Accounts.AccountByName(FHost.Text))) then
      FName.Text := FHost.Text
    else
    begin
      Index := 2;
      while (Assigned(Accounts.AccountByName(FHost.Text + ' (' + IntToStr(Index) + ')'))) do
        Inc(Index);
      FName.Text := FHost.Text + ' (' + IntToStr(Index) + ')';
    end;
end;

procedure TDAccount.FHTTPTunnelURIChange(Sender: TObject);
begin
  FBOk.Enabled := (FHTTPTunnelURI.Text = '') or (Copy(FHTTPTunnelURI.Text, 1, 7) = 'http://') or (Copy(FHTTPTunnelURI.Text, 1, 8) = 'https://');
end;

procedure TDAccount.FHTTPTunnelURIEnter(Sender: TObject);
begin
  if ((FHTTPTunnelURI.Text = '') and (Trim(FHost.Text) <> '') and (lstrcmpi(PChar(Trim(FHost.Text)), LOCAL_HOST) <> 0)) then
    FHTTPTunnelURI.Text := 'http://' + Trim(FHost.Text) + '/libMySQL.php';
end;

procedure TDAccount.FLibraryFilenameEnter(Sender: TObject);
begin
  if (FLibraryFilename.Text = '') then
    FLibraryFilename.Text := 'libMySQL.dll';
end;

procedure TDAccount.FormCanResize(Sender: TObject; var NewWidth,
  NewHeight: Integer; var Resize: Boolean);
begin
  NewHeight := Height;
end;

procedure TDAccount.FormCloseQuery(Sender: TObject; var CanClose: Boolean);
var
  NewAccount: TPAccount;
begin
  if (ModalResult = mrOk) then
  begin
    ActiveControl := FBOk;

    if ((FConnectionType.ItemIndex = 1) and (FLibraryFilename.Text = '')) then
      begin ActiveControl := FLibraryFilename; MessageBeep(MB_ICONERROR); CanClose := False; end
    else if ((FConnectionType.ItemIndex = 2) and not ((Copy(FHTTPTunnelURI.Text, 1, 7) = 'http://') or (Copy(FHTTPTunnelURI.Text, 1, 8) = 'https://'))) then
      begin ActiveControl := FHTTPTunnelURI; MessageBeep(MB_ICONERROR); CanClose := False; end;

    if (CanClose) then
    begin
      NewAccount := TPAccount.Create(Accounts);
      if (Assigned(Account)) then
        NewAccount.Assign(Account);

      NewAccount.Name := Trim(FName.Text);
      NewAccount.Connection.Host := Trim(FHost.Text);
      NewAccount.Connection.Port := FUDPort.Position;
      case (FConnectionType.ItemIndex) of
        0: NewAccount.Connection.LibraryType := fPreferences.ltBuiltIn;
        1: NewAccount.Connection.LibraryType := fPreferences.ltDLL;
        2: NewAccount.Connection.LibraryType := fPreferences.ltHTTP;
      end;
      NewAccount.Connection.LibraryFilename := Trim(FLibraryFilename.Text);
      NewAccount.Connection.HTTPTunnelURI := Trim(FHTTPTunnelURI.Text);

      NewAccount.Connection.Username := Trim(FUser.Text);
      NewAccount.Connection.Password := Trim(FPassword.Text);
      NewAccount.Connection.Database := ReplaceStr(Trim(FDatabase.Text), ';', ',');

      Username := NewAccount.Connection.Username;
      Password := NewAccount.Connection.Password;

      if (not Assigned(Account)) then
        Accounts.AddAccount(NewAccount)
      else
        Accounts.UpdateAccount(Account, NewAccount);

      NewAccount.Free();
    end;
  end;
end;

procedure TDAccount.FormCreate(Sender: TObject);
begin
  Constraints.MinWidth := Width;
  Constraints.MinHeight := Height;

  BorderStyle := bsSizeable;
end;

procedure TDAccount.FormHide(Sender: TObject);
begin
  Preferences.Account.Width := Width;
  Preferences.Account.Height := Height;

  if (ModalResult = mrOk) then
    Preferences.Save();
end;

procedure TDAccount.FormResize(Sender: TObject);
begin
  FBDatabase.Height := FDatabase.Height; FBDatabase.Width := FBDatabase.Height;
  FBDatabase.Left := FDatabase.Left + FDatabase.Width;
end;

procedure TDAccount.FormShow(Sender: TObject);
begin
  if ((Preferences.Database.Width >= Width) and (Preferences.Database.Height >= Height)) then
  begin
    Width := Preferences.Account.Width;
    Height := Preferences.Account.Height;
  end;

  if (not Assigned(Account)) then
    Caption := Preferences.LoadStr(204)
  else
    Caption := Preferences.LoadStr(842, Account.Name);

  FConnectionType.Items.Text := Preferences.LoadStr(649) + #13#10 + Preferences.LoadStr(650) + #13#10 + Preferences.LoadStr(651);

  if (not Assigned(Account)) then
  begin
    FName.Text := '';

    FHost.Text := '';
    FUDPort.Position := MYSQL_PORT;
    FConnectionType.ItemIndex := 0;
    FLibraryFilename.Text := 'libMySQL.dll';
    FHTTPTunnelURI.Text := '';

    FUser.Text := 'root';
    FPassword.Text := '';
    FDatabase.Text := '';
  end
  else
  begin
    FName.Text := Account.Name;

    FHost.Text := Account.Connection.Host;
    if (Account.Connection.Port = 0) then
      FUDPort.Position := MYSQL_PORT
    else
      FUDPort.Position := Account.Connection.Port;
    case (Account.Connection.LibraryType) of
      fPreferences.ltBuiltIn: FConnectionType.ItemIndex := 0;
      fPreferences.ltDLL: FConnectionType.ItemIndex := 1;
      fPreferences.ltHTTP: FConnectionType.ItemIndex := 2;
    end;
    FLibraryFilename.Text := Account.Connection.LibraryFilename;
    FHTTPTunnelURI.Text := Account.Connection.HTTPTunnelURI;

    FUser.Text := Username;
    FPassword.Text := Password;
    FDatabase.Text := Account.Connection.Database;
  end;


  FEditChange(nil);
  FConnectionTypeChange(nil);

  ActiveControl := FBCancel;
  if (ShowType = stLogin) then
    ActiveControl := FUser
  else
    ActiveControl := FHost;
end;

function TDAccount.GetAccountName(): string;
begin
  Result := Trim(FName.Text);
end;

procedure TDAccount.UMChangePreferences(var Message: TMessage);
begin
  Preferences.Images.GetIcon(iiServer, Icon);

  GBasics.Caption := Preferences.LoadStr(85);
  FLName.Caption := Preferences.LoadStr(35) + ':';

  GConnection.Caption := Preferences.LoadStr(486);
  FLHost.Caption := Preferences.LoadStr(906) + ':';
  FLPort.Caption := Preferences.LoadStr(436) + ':';
  FLConnectionType.Caption := Preferences.LoadStr(648) + ':';
  FConnectionType.Items.Text := '';
  FConnectionTypeChange(nil);
  FLLibraryFilename.Caption := Preferences.LoadStr(568) + ':';
  FLHTTPTunnelURI.Caption := Preferences.LoadStr(652) + ':';

  GLogin.Caption := Preferences.LoadStr(34);
  FLUser.Caption := Preferences.LoadStr(561) + ':';
  FLPassword.Caption := Preferences.LoadStr(40) + ':';
  FLDatabase.Caption := Preferences.LoadStr(38) + ':';

  FBHelp.Caption := Preferences.LoadStr(167);
  FBOk.Caption := Preferences.LoadStr(29);
  FBCancel.Caption := Preferences.LoadStr(30);
end;

initialization
  FAccount := nil;
end.
