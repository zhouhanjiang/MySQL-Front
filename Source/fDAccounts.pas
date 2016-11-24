unit fDAccounts;

interface {********************************************************************}

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, ComCtrls, ExtCtrls, Menus, ActnList, ImgList, ToolWin,
  StdCtrls_Ext, ComCtrls_Ext, ExtCtrls_Ext, Forms_Ext,
  fSession, fPreferences, fBase;

type
  TDAccounts = class (TForm_Ext)
    ActionList: TActionList;
    aDelete: TAction;
    aEdit: TAction;
    aNew: TAction;
    aOpen: TAction;
    FBCancel: TButton;
    FBDelete: TButton;
    FBEdit: TButton;
    FBNew: TButton;
    FBOk: TButton;
    FAccounts: TListView_Ext;
    GAccounts: TGroupBox_Ext;
    miDelete: TMenuItem;
    miEdit: TMenuItem;
    miNew: TMenuItem;
    miOpen: TMenuItem;
    N2: TMenuItem;
    PopupMenu: TPopupMenu;
    PAccounts: TPanel_Ext;
    procedure aDeleteExecute(Sender: TObject);
    procedure aEditExecute(Sender: TObject);
    procedure aNewExecute(Sender: TObject);
    procedure aOpenExecute(Sender: TObject);
    procedure FBOkEnabledCheck(Sender: TObject);
    procedure FormActivate(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormHide(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure FAccountsColumnClick(Sender: TObject; Column: TListColumn);
    procedure FAccountsCompare(Sender: TObject; Item1, Item2: TListItem;
      Data: Integer; var Compare: Integer);
    procedure FAccountsDblClick(Sender: TObject);
    procedure FAccountsResize(Sender: TObject);
    procedure FAccountsSelectItem(Sender: TObject; Item: TListItem;
      Selected: Boolean);
    procedure PopupMenuPopup(Sender: TObject);
    procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);
  private
    procedure ListViewShowSortDirection(const ListView: TListView);
    procedure SetFAccounts(const ASelected: TPAccount);
    procedure UMChangePreferences(var Message: TMessage); message UM_CHANGEPREFERENCES;
    procedure UMPostShow(var Message: TMessage); message UM_POST_SHOW;
  public
    Account: TPAccount;
    Session: TSSession;
    Open: Boolean;
    function Execute(): Boolean;
  end;

function DAccounts(): TDAccounts;

implementation {***************************************************************}

{$R *.dfm}

uses
  CommCtrl, Math, StrUtils,
  CommCtrl_Ext,
  MySQLConsts,
  fDAccount, fDConnecting;

var
  FAccounts: TDAccounts;

function DAccounts(): TDAccounts;
begin
  if (not Assigned(FAccounts)) then
  begin
    Application.CreateForm(TDAccounts, FAccounts);
    FAccounts.Perform(UM_CHANGEPREFERENCES, 0, 0);
  end;

  Result := FAccounts;
end;

{ TDAccounts ******************************************************************}

procedure TDAccounts.aDeleteExecute(Sender: TObject);
begin
  if (MsgBox(Preferences.LoadStr(46, FAccounts.Selected.Caption), Preferences.LoadStr(101), MB_YESNOCANCEL + MB_ICONQUESTION) = IDYES) then
    if (Accounts.DeleteAccount(Accounts.AccountByName(FAccounts.Selected.Caption))) then
    begin
      SetFAccounts(nil);
      FBCancel.Caption := Preferences.LoadStr(231);
    end;

  ActiveControl := FAccounts;
end;

procedure TDAccounts.aEditExecute(Sender: TObject);
begin
  DAccount.Account := Accounts.AccountByName(FAccounts.Selected.Caption);
  DAccount.Username := DAccount.Account.Connection.Username;
  DAccount.Password := DAccount.Account.Connection.Password;
  DAccount.ShowType := stDefault;
  if (DAccount.Execute()) then
  begin
    SetFAccounts(Accounts.AccountByName(DAccount.AccountName));
    FBCancel.Caption := Preferences.LoadStr(231);
  end;
  ActiveControl := FAccounts;
end;

procedure TDAccounts.aNewExecute(Sender: TObject);
begin
  DAccount.Account := nil;
  DAccount.Username := 'root';
  DAccount.Password := '';
  DAccount.ShowType := stDefault;
  if (DAccount.Execute()) then
  begin
    SetFAccounts(Accounts.AccountByName(DAccount.AccountName));
    FBCancel.Caption := Preferences.LoadStr(231);
  end;

  ActiveControl := FAccounts;
end;

procedure TDAccounts.aOpenExecute(Sender: TObject);
begin
  if (Open) then
    FBOk.Click()
  else
    if (Boolean(SendMessage(Application.MainForm.Handle, UM_ADDTAB, 0, LPARAM(Accounts.AccountByName(FAccounts.Selected.Caption).Desktop.Address)))) then
      FBOk.Click();
end;

function TDAccounts.Execute(): Boolean;
begin
  ShowModal();
  Result := ModalResult = mrOk;
end;

procedure TDAccounts.FBOkEnabledCheck(Sender: TObject);
begin
  FBOk.Enabled := Assigned(FAccounts.Selected);
end;

procedure TDAccounts.FormActivate(Sender: TObject);
begin
  if (FAccounts.Items.Count = 0) then
    aNew.Execute();
end;

procedure TDAccounts.FormCloseQuery(Sender: TObject; var CanClose: Boolean);
begin
  if ((ModalResult = mrOk) and not Assigned(Session)) then
  begin
    Session := TSSession.Create(Sessions, Accounts.AccountByName(FAccounts.Selected.Caption));
    DConnecting.Session := Session;
    CanClose := DConnecting.Execute();
    if (not CanClose) then
      FreeAndNil(Session);
  end;
end;

procedure TDAccounts.FormCreate(Sender: TObject);
begin
  FAccounts.SmallImages := Preferences.Images;

  Constraints.MinWidth := Width;
  Constraints.MinHeight := Height;

  BorderStyle := bsSizeable;

  SetWindowLong(ListView_GetHeader(FAccounts.Handle), GWL_STYLE, GetWindowLong(ListView_GetHeader(FAccounts.Handle), GWL_STYLE) or HDS_NOSIZING);
end;

procedure TDAccounts.FormHide(Sender: TObject);
begin
  if (ModalResult = mrOk) then
    Accounts.Default := Accounts.AccountByName(FAccounts.Selected.Caption);

  Preferences.Accounts.Height := Height;
  Preferences.Accounts.SelectOrder := FAccounts.Tag;
  Preferences.Accounts.Width := Width;
end;

procedure TDAccounts.FormShow(Sender: TObject);
begin
  if ((Preferences.Accounts.Width >= Width) and (Preferences.Accounts.Height >= Height)) then
  begin
    Width := Preferences.Accounts.Width;
    Height := Preferences.Accounts.Height;
  end;

  if (not Open) then
    Caption := Preferences.LoadStr(25)
  else
    Caption := Preferences.LoadStr(1);

  FAccounts.Tag := Preferences.Accounts.SelectOrder;
  if (FAccounts.Tag = 1) then
    FAccounts.Column[FAccounts.Tag].Tag := -1
  else
    FAccounts.Column[FAccounts.Tag].Tag := 1;

  SetFAccounts(Accounts.Default);

  Session := nil;

  FBOk.Visible := Open;
  if (not Open) then
    FBCancel.Caption := Preferences.LoadStr(231)
  else
    FBCancel.Caption := Preferences.LoadStr(30);

  FBOk.Default := Open;
  FBCancel.Default := not FBOk.Default;

  ActiveControl := FAccounts;

  FBOkEnabledCheck(Sender);

  PostMessage(Handle, UM_POST_SHOW, 0, 0);
end;

procedure TDAccounts.FAccountsColumnClick(Sender: TObject;
  Column: TListColumn);
var
  I: Integer;
begin
  if (Assigned(Sender)) then
    for I := 0 to FAccounts.Columns.Count - 1 do
      if (FAccounts.Column[I] <> Column) then
        FAccounts.Column[I].Tag := 0
      else if (FAccounts.Column[I].Tag < 0) then
        FAccounts.Column[I].Tag := 1
      else if (FAccounts.Column[I].Tag > 0) then
        FAccounts.Column[I].Tag := -1
      else if (I = 1) then
        FAccounts.Column[I].Tag := -1
      else
        FAccounts.Column[I].Tag := 1;

  FAccounts.Tag := Column.Index;
  FAccounts.AlphaSort();

  ListViewShowSortDirection(FAccounts);
end;

procedure TDAccounts.FAccountsCompare(Sender: TObject; Item1,
  Item2: TListItem; Data: Integer; var Compare: Integer);
var
  Column: TListColumn;
  DateTime1: TDateTime;
  DateTime2: TDateTime;
begin
  Column := FAccounts.Column[FAccounts.Tag];

  if (Column.Index = 0) then
    Compare := Sign(lstrcmpi(PChar(Item1.Caption), PChar(Item2.Caption)))
  else
  begin
    DateTime1 := Accounts.AccountByName(Item1.Caption).LastLogin;
    DateTime2 := Accounts.AccountByName(Item2.Caption).LastLogin;
    Compare := Sign(DateTime1 - DateTime2);
  end;

  if (Column.Tag < 0) then
    Compare := - Compare;
end;

procedure TDAccounts.FAccountsDblClick(Sender: TObject);
begin
  if (Open and FBOk.Enabled) then
    FBOk.Click()
  else if (not Open and aEdit.Enabled) then
    aEdit.Execute();
end;

procedure TDAccounts.FAccountsResize(Sender: TObject);
var
  I: Integer;
  LastLoginWidth: Integer;
begin
  LastLoginWidth := 0;
  for I := 0 to FAccounts.Items.Count - 1 do
    if (LastLoginWidth < FAccounts.Canvas.TextWidth(FAccounts.Items[I].SubItems[0])) then
      LastLoginWidth := FAccounts.Canvas.TextWidth(FAccounts.Items[I].SubItems[0]);
  if (LastLoginWidth = 0) then
    LastLoginWidth := FAccounts.Width div 2
  else
    Inc(LastLoginWidth, 35);

  FAccounts.Column[0].Width := FAccounts.ClientWidth - (LastLoginWidth);
  FAccounts.Column[1].Width := LastLoginWidth;

  if (Assigned(FAccounts.ItemFocused) and (FAccounts.Items.Count > 1) and (FAccounts.ItemFocused.Position.Y - FAccounts.ClientHeight + (FAccounts.Items[1].Top - FAccounts.Items[0].Top) > 0)) then
    FAccounts.Scroll(0, FAccounts.ItemFocused.Position.Y - FAccounts.ClientHeight + (FAccounts.Items[1].Top - FAccounts.Items[0].Top));
end;

procedure TDAccounts.FAccountsSelectItem(Sender: TObject; Item: TListItem;
  Selected: Boolean);
var
  Account: TPAccount;
begin
  if (not Assigned(Item)) then
    Account := nil
  else
    Account := Accounts.AccountByName(Item.Caption);

  aEdit.Enabled := Assigned(Item) and Selected;
  aDelete.Enabled := Assigned(Item) and Selected and Assigned(Account) and (Account.DesktopCount = 0);

  FBOkEnabledCheck(Sender);
  FBOk.Default := FBOk.Enabled;
end;

procedure TDAccounts.ListViewShowSortDirection(const ListView: TListView);
var
  Column: TListColumn;
  HDItem: THDItem;
  I: Integer;
begin
  Column := ListView.Column[ListView.Tag];

  HDItem.Mask := HDI_WIDTH or HDI_FORMAT;
  for I := 0 to ListView.Columns.Count - 1 do
    if (BOOL(SendMessage(ListView_GetHeader(ListView.Handle), HDM_GETITEM, I, LParam(@HDItem)))) then
    begin
      case (ListView.Column[I].Tag) of
        -1: HDItem.fmt := HDItem.fmt and not HDF_SORTUP or HDF_SORTDOWN;
        1: HDItem.fmt := HDItem.fmt and not HDF_SORTDOWN or HDF_SORTUP;
        else HDItem.fmt := HDItem.fmt and not HDF_SORTUP and not HDF_SORTDOWN;
      end;

      SendMessage(ListView_GetHeader(ListView.Handle), HDM_SETITEM, I, LParam(@HDItem));
    end;

  if ((ComCtl32MajorVersion >= 6) and not CheckWin32Version(6, 1)) then
    SendMessage(ListView.Handle, LVM_SETSELECTEDCOLUMN, Column.Index, 0);
end;

procedure TDAccounts.PopupMenuPopup(Sender: TObject);
begin
  aOpen.Enabled := Assigned(FAccounts.Selected);
  aNew.Enabled := not Assigned(FAccounts.Selected);
  miOpen.Default := Open;
  miEdit.Default := not miOpen.Default;
  ShowEnabledItems(PopupMenu.Items);
end;

procedure TDAccounts.SetFAccounts(const ASelected: TPAccount);
var
  I: Integer;
  Item: TListItem;
begin
  FAccounts.DisableAlign(); FAccounts.Items.BeginUpdate();

  FAccounts.Items.Clear();

  if (Accounts.Count = 0) then
    FAccountsSelectItem(FAccounts, nil, False)
  else
    for I := 0 to Accounts.Count - 1 do
    begin
      Item := FAccounts.Items.Add();
      Item.Caption := Accounts[I].Name;
      if (Accounts[I].LastLogin = 0) then
        Item.SubItems.Add('???')
      else
        Item.SubItems.Add(DateTimeToStr(Accounts[I].LastLogin, LocaleFormatSettings));
      Item.ImageIndex := 23;
    end;

  FAccountsColumnClick(Account, FAccounts.Column[FAccounts.Tag]);

  if (not Assigned(ASelected)) and (FAccounts.Items.Count > 0) then
    FAccounts.Selected := FAccounts.Items.Item[0]
  else
    for I := 0 to FAccounts.Items.Count - 1 do
      if (ASelected <> nil) and (FAccounts.Items.Item[I].Caption = ASelected.Name) then
        FAccounts.Selected := FAccounts.Items.Item[I];

  FAccounts.ItemFocused := FAccounts.Selected;
  FAccountsResize(nil);

  FAccounts.EnableAlign(); FAccounts.Items.EndUpdate();
end;

procedure TDAccounts.UMChangePreferences(var Message: TMessage);
begin
  FAccounts.Canvas.Font := Font;

  Preferences.Images.GetIcon(40, Icon);

  GAccounts.Caption := Preferences.LoadStr(25);
  FAccounts.Columns.Items[0].Caption := Preferences.LoadStr(35);
  FAccounts.Columns.Items[1].Caption := Preferences.LoadStr(693);
  aOpen.Caption := Preferences.LoadStr(581);
  aNew.Caption := Preferences.LoadStr(26) + '...';
  aEdit.Caption := Preferences.LoadStr(97) + '...';
  aDelete.Caption := Preferences.LoadStr(28);

  FBOk.Caption := Preferences.LoadStr(581);
end;

procedure TDAccounts.UMPostShow(var Message: TMessage);
begin
  ListViewShowSortDirection(FAccounts);
end;

initialization
  FAccounts := nil;
end.

