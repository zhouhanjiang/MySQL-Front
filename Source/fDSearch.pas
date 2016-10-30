unit fDSearch;

interface {********************************************************************}

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Menus,
  Dialogs, StdCtrls, ComCtrls, DB, ExtCtrls,
  ComCtrls_Ext, Forms_Ext, StdCtrls_Ext, ExtCtrls_Ext,
  MySQLDB,
  fSession, fPreferences, fTools,
  fBase, fFSession;

type
  TDSTableItem = record
    Account: TPAccount;
    DatabaseName: string;
    TableName: string;
  end;

  TDSearch = class(TForm_Ext)
    FBBack: TButton;
    FBCancel: TButton;
    FBForward: TButton;
    FBHelp: TButton;
    FDoneRecords: TLabel;
    FDoneTables: TLabel;
    FDoneTime: TLabel;
    FEntieredRecords: TLabel;
    FEntieredTables: TLabel;
    FEntieredTime: TLabel;
    FErrorMessages: TRichEdit;
    FErrors: TLabel;
    FFFindText: TComboBox_Ext;
    FFMatchCase: TCheckBox;
    FFRegExpr: TCheckBox;
    FFWholeValue: TCheckBox;
    FLDone: TLabel;
    FLEntiered: TLabel;
    FLErrors: TLabel;
    FLFFindText: TLabel;
    FLFSearchOptions: TLabel;
    FLProgressRecords: TLabel;
    FLProgressTables: TLabel;
    FLProgressTime: TLabel;
    FLReplaceText: TLabel;
    FLRFindText: TLabel;
    FLRSearchOptions: TLabel;
    FProgressBar: TProgressBar;
    FReplaceText: TComboBox_Ext;
    FRFindText: TComboBox_Ext;
    FRMatchCase: TCheckBox;
    FRRegExpr: TCheckBox;
    FRWholeValue: TCheckBox;
    FSelect: TTreeView_Ext;
    FTables: TListView_Ext;
    GFOptions: TGroupBox_Ext;
    GFWhat: TGroupBox_Ext;
    GMessages: TGroupBox_Ext;
    GProgress: TGroupBox_Ext;
    GROptions: TGroupBox_Ext;
    GRWhat: TGroupBox_Ext;
    GSelect: TGroupBox_Ext;
    MTables: TPopupMenu;
    mTCopy: TMenuItem;
    PageControl: TPageControl;
    PErrorMessages: TPanel_Ext;
    PSelect: TPanel_Ext;
    TSExecute: TTabSheet;
    TSFOptions: TTabSheet;
    TSROptions: TTabSheet;
    TSSelect: TTabSheet;
    procedure FBBackClick(Sender: TObject);
    procedure FBCancelClick(Sender: TObject);
    procedure FBForwardClick(Sender: TObject);
    procedure FBHelpClick(Sender: TObject);
    procedure FFFindTextChange(Sender: TObject);
    procedure FFRegExprClick(Sender: TObject);
    procedure FFRegExprKeyPress(Sender: TObject; var Key: Char);
    procedure FormCreate(Sender: TObject);
    procedure FormHide(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure FRFindTextChange(Sender: TObject);
    procedure FRRegExprClick(Sender: TObject);
    procedure FRRegExprKeyPress(Sender: TObject; var Key: Char);
    procedure FSelectChange(Sender: TObject; Node: TTreeNode);
    procedure FSelectExpanding(Sender: TObject; Node: TTreeNode;
      var AllowExpansion: Boolean);
    procedure FTablesDblClick(Sender: TObject);
    procedure mTCopyClick(Sender: TObject);
    procedure TSExecuteShow(Sender: TObject);
    procedure TSFOptionsShow(Sender: TObject);
    procedure TSROptionsShow(Sender: TObject);
    procedure TSSelectShow(Sender: TObject);
    procedure FSelectGetImageIndex(Sender: TObject; Node: TTreeNode);
    procedure FSelectChanging(Sender: TObject; Node: TTreeNode;
      var AllowChange: Boolean);
  private
    Sessions: array of TSSession;
    ExecuteSession: TSSession;
    Search: TTSearch;
    ProgressInfos: TTool.TProgressInfos;
    ReplaceSession: TSSession;
    SQLWait: Boolean;
    Tables: array of TDSTableItem;
    WantedExecute: Boolean;
    WantedNodeExpand: TTreeNode;
    procedure FormSessionEvent(const Event: TSSession.TEvent);
    function GetSession(const Index: Integer): TSSession;
    procedure OnError(const Sender: TObject; const Error: TTool.TError; const Item: TTool.TItem; const ShowRetry: Boolean; var Success: TDataAction);
    procedure OnSearched(const AItem: TTSearch.TItem);
    procedure OnTerminate(Sender: TObject);
    procedure OnUpdate(const AProgressInfos: TTool.TProgressInfos);
    procedure UMChangePreferences(var Message: TMessage); message UM_CHANGEPREFERENCES;
    procedure UMTerminate(var Message: TMessage); message UM_TERMINATE;
    procedure UMUpdateProgressInfo(var Message: TMessage); message UM_UPDATEPROGRESSINFO;
  public
    Session: TSSession;
    Database: TSDatabase;
    Frame: TFSession;
    SearchOnly: Boolean;
    function Execute(): Boolean;
  end;

function DSearch(): TDSearch;

implementation {***************************************************************}

{$R *.dfm}

uses
  Consts, StrUtils, CommCtrl, RichEdit,
  SQLUtils,
  fURI,
  fDConnecting;

var
  FSearch: TDSearch;

function DSearch(): TDSearch;
begin
  if (not Assigned(FSearch)) then
  begin
    Application.CreateForm(TDSearch, FSearch);
    FSearch.Perform(UM_CHANGEPREFERENCES, 0, 0);
  end;

  Result := FSearch;
end;

{ TDSearch ********************************************************************}

function TDSearch.Execute(): Boolean;
begin
  ShowModal();
  Result := ModalResult = mrOk;
end;

procedure TDSearch.FBBackClick(Sender: TObject);
begin
  if ((PageControl.ActivePage = TSFOptions) or (PageControl.ActivePage = TSROptions)) then
    PageControl.ActivePage := TSSelect
  else if ((PageControl.ActivePage = TSExecute) and SearchOnly) then
    PageControl.ActivePage := TSFOptions
  else if ((PageControl.ActivePage = TSExecute) and not SearchOnly) then
    PageControl.ActivePage := TSROptions;
end;

procedure TDSearch.FBCancelClick(Sender: TObject);
begin
  if (Assigned(Search)) then
  begin
    Search.Terminate();
    FBCancel.Enabled := False;
  end;
end;

procedure TDSearch.FBForwardClick(Sender: TObject);
begin
  if (PageControl.ActivePage = TSSelect) then
    if (SearchOnly) then
      PageControl.ActivePage := TSFOptions
    else
      PageControl.ActivePage := TSROptions
  else if (PageControl.ActivePage = TSFOptions) then
    PageControl.ActivePage := TSExecute
  else if (PageControl.ActivePage = TSROptions) then
    PageControl.ActivePage := TSExecute;
end;

procedure TDSearch.FBHelpClick(Sender: TObject);
begin
  Application.HelpContext(HelpContext);
end;

procedure TDSearch.FFFindTextChange(Sender: TObject);
begin
  FBForward.Enabled := (FFFindText.Text <> '') and not SQLWait;
end;

procedure TDSearch.FFRegExprClick(Sender: TObject);
begin
  FFWholeValue.Enabled := not FFRegExpr.Checked;
end;

procedure TDSearch.FFRegExprKeyPress(Sender: TObject; var Key: Char);
begin
  FFRegExprClick(Sender);
end;

procedure TDSearch.FormSessionEvent(const Event: TSSession.TEvent);
begin
  if (Event.EventType in [etAfterExecuteSQL]) then
  begin
    FSelect.Cursor := crDefault;
    if (Assigned(WantedNodeExpand)) then
      WantedNodeExpand.Expand(False)
    else if (WantedExecute) then
      TSExecuteShow(Event.Sender);
  end;
end;

procedure TDSearch.FormCreate(Sender: TObject);
var
  I: Integer;
begin
  Constraints.MinWidth := Width;
  Constraints.MinHeight := Height;

  BorderStyle := bsSizeable;

  Search := nil;

  FSelect.Images := Preferences.Images;

  FFFindText.Items.Clear();
  for I := Preferences.Find.FindTextMRU.Count - 1 downto 0 do
    FFFindText.Items.Insert(0, Preferences.Find.FindTextMRU.Values[I]);

  FFMatchCase.Checked := foMatchCase in Preferences.Find.Options;
  FFWholeValue.Checked := foWholeValue in Preferences.Find.Options;
  FFRegExpr.Checked := foRegExpr in Preferences.Find.Options;
  FFRegExprClick(Sender);

  FRFindText.Items.Clear();
  for I := Preferences.Replace.FindTextMRU.Count - 1 downto 0 do
    FRFindText.Items.Insert(0, Preferences.Replace.FindTextMRU.Values[I]);
  FReplaceText.Items.Clear();
  for I := Preferences.Replace.ReplaceTextMRU.Count - 1 downto 0 do
    FReplaceText.Items.Insert(0, Preferences.Replace.ReplaceTextMRU.Values[I]);

  FRMatchCase.Checked := roMatchCase in Preferences.Replace.Options;
  FRWholeValue.Checked := roWholeValue in Preferences.Replace.Options;
  FRRegExpr.Checked := roRegExpr in Preferences.Replace.Options;
  FRRegExprClick(Sender);

  SendMessage(FErrorMessages.Handle, EM_SETTEXTMODE, TM_PLAINTEXT, 0);
  SendMessage(FErrorMessages.Handle, EM_SETWORDBREAKPROC, 0, LPARAM(@EditWordBreakProc));

  PageControl.ActivePage := nil; // Make sure, not ___OnShowPage will be executed
end;

procedure TDSearch.FormHide(Sender: TObject);
var
  I: Integer;
begin
  FSelect.Selected := nil; // Make sure, not to call FSelectedChange with a selected node
  FSelect.Items.BeginUpdate();
  FSelect.Items.Clear();
  FSelect.Items.EndUpdate();

  for I := 0 to Length(Sessions) - 1 do
    if (Assigned(Sessions[I])) then
    begin
      Sessions[I].UnRegisterEventProc(FormSessionEvent);
      if (Sessions[I] <> Session) then
        Sessions[I].Free();
    end;
  SetLength(Sessions, 0);

  if (SearchOnly) then
  begin
    Preferences.Find.Height := Height;
    Preferences.Find.Width := Width;
    Preferences.Find.Left := Left;
    Preferences.Find.Top := Top;
  end
  else
  begin
    Preferences.Replace.Height := Height;
    Preferences.Replace.Width := Width;
    Preferences.Replace.Left := Left;
    Preferences.Replace.Top := Top;
  end;

  PageControl.ActivePage := nil; // Make sure, not ___OnShowPage will be executed
end;

procedure TDSearch.FormShow(Sender: TObject);
var
  I: Integer;
  J: Integer;
  Node: TTreeNode;
begin
  if (SearchOnly) then
  begin
    Caption := Preferences.LoadStr(187);
    Preferences.Images.GetIcon(12, Icon);
    HelpContext := 1093;

    GMessages.Caption := Preferences.LoadStr(234);

    if ((Preferences.Find.Width > 0) and (Preferences.Find.Height > 0)) then
    begin
      Width := Preferences.Find.Width;
      Height := Preferences.Find.Height;
    end
    else
    begin
      Width := Constraints.MinWidth;
      Height := Constraints.MinHeight;
    end;
    if ((Preferences.Find.Left > 0) and (Preferences.Find.Top > 0)) then
    begin
      Left := Preferences.Find.Left;
      Top := Preferences.Find.Top;
    end;
  end
  else
  begin
    Caption := Preferences.LoadStr(416);
    Preferences.Images.GetIcon(29, Icon);
    HelpContext := 1090;

    GMessages.Caption := Preferences.LoadStr(392);

    if ((Preferences.Replace.Width > 0) and (Preferences.Replace.Height > 0)) then
    begin
      Width := Preferences.Replace.Width;
      Height := Preferences.Replace.Height;
    end
    else
    begin
      Width := Constraints.MinWidth;
      Height := Constraints.MinHeight;
    end;
    if ((Preferences.Replace.Left > 0) and (Preferences.Replace.Top > 0)) then
    begin
      Left := Preferences.Replace.Left;
      Top := Preferences.Replace.Top;
    end;
  end;

  WantedExecute := False;
  WantedNodeExpand := nil;
  FErrorMessages.Visible := not SearchOnly;
  FTables.Visible := SearchOnly;
  FFFindTextChange(Sender);

  SetLength(Sessions, Accounts.Count);
  for I := 0 to Length(Sessions) - 1 do
    if (Assigned(Session) and (Accounts[I] = Session.Account)) then
      Sessions[I] := Session
    else
      Sessions[I] := nil;

  for I := 0 to Accounts.Count - 1 do
  begin
    Node := FSelect.Items.Add(nil, Accounts[I].Name);
    Node.ImageIndex := iiServer;
    Node.HasChildren := True;

    if (Assigned(Session) and (Accounts[I] = Session.Account) and Session.Databases.Valid) then
    begin
      FSelect.Selected := Node;
      Node.Expand(False);
      if (Assigned(Database)) then
        for J := 0 to Node.Count - 1 do
          if (Node[J].Data = Database) then
          begin
            FSelect.Selected := Node[J];
            Node[J].Expand(False);
          end;
    end;
  end;

  FFFindText.Text := '';

  FRFindText.Text := '';
  FReplaceText.Text := '';

  PageControl.ActivePage := TSSelect;

  FBForward.Default := True;

  FBCancel.Caption := Preferences.LoadStr(30);
  FBCancel.Enabled := True;
  FBCancel.ModalResult := mrCancel;
  FBCancel.Default := False;

  ActiveControl := FSelect;
end;

procedure TDSearch.FRFindTextChange(Sender: TObject);
begin
  FBForward.Enabled := (FRFindText.Text <> '') and (FRFindText.Text <> FReplaceText.Text) and not SQLWait;
end;

procedure TDSearch.FRRegExprClick(Sender: TObject);
begin
  FRWholeValue.Enabled := not FRRegExpr.Checked;
end;

procedure TDSearch.FRRegExprKeyPress(Sender: TObject; var Key: Char);
begin
  FRRegExprClick(Sender);
end;

procedure TDSearch.FSelectChange(Sender: TObject; Node: TTreeNode);
begin
  if ((ModalResult = mrNone) and Assigned(Node)) then
    FSelect.MultiSelect := Assigned(Node.Parent);

  FBForward.Enabled := Assigned(FSelect.Selected);
end;

procedure TDSearch.FSelectChanging(Sender: TObject; Node: TTreeNode;
  var AllowChange: Boolean);
begin
  AllowChange := not Assigned(Node) or (Node.ImageIndex <> iiServer);
end;

procedure TDSearch.FSelectExpanding(Sender: TObject; Node: TTreeNode;
  var AllowExpansion: Boolean);
var
  Session: TSSession;
  Database: TSDatabase;
  I: Integer;
  NewNode: TTreeNode;
  Table: TSBaseTable;
  TreeView: TTreeView_Ext;
begin
  TreeView := TTreeView_Ext(Sender);

  if (Assigned(WantedNodeExpand)) then
  begin
    for I := 0 to Length(Sessions) - 1 do
      if (Assigned(Sessions[I])) then
        Sessions[I].UnRegisterEventProc(FormSessionEvent);
    WantedNodeExpand := nil;
  end;

  if (Assigned(Node)) then
    if (Node.HasChildren and not Assigned(Node.getFirstChild())) then
    begin
      case (Node.ImageIndex) of
        iiServer:
          begin
            Session := GetSession(Node.Index);
            if (Assigned(Session)) then
              if (not Session.Update()) then
              begin
                WantedNodeExpand := Node;
                TreeView.Cursor := crSQLWait;
              end
              else
              begin
                for I := 0 to Session.Databases.Count - 1 do
                  if (not (Session.Databases[I] is TSSystemDatabase)) then
                  begin
                    NewNode := TreeView.Items.AddChild(Node, Session.Databases[I].Name);
                    NewNode.ImageIndex := iiDatabase;
                    NewNode.Data := Session.Databases[I];
                    NewNode.HasChildren := True;
                  end;
                Node.HasChildren := Assigned(Node.getFirstChild());
              end;
          end;
        iiDatabase:
          begin
            Session := GetSession(Node.Parent.Index);
            Database := Session.DatabaseByName(Node.Text);
            if ((not Database.Tables.Update() or not Session.Update(Database.Tables))) then
            begin
              WantedNodeExpand := Node;
              TreeView.Cursor := crSQLWait;
            end
            else
            begin
              for I := 0 to Database.Tables.Count - 1 do
                if ((Database.Tables[I] is TSBaseTable) and Assigned(TSBaseTable(Database.Tables[I]).Engine) and not TSBaseTable(Database.Tables[I]).Engine.IsMerge) then
                begin
                  NewNode := TreeView.Items.AddChild(Node, Database.Tables[I].Name);
                  NewNode.ImageIndex := iiBaseTable;
                  NewNode.Data := Database.Tables[I];
                  NewNode.HasChildren := True;
                end;
              Node.HasChildren := Assigned(Node.getFirstChild());
            end;
          end;
        iiBaseTable:
          begin
            Session := GetSession(Node.Parent.Parent.Index);
            Database := Session.DatabaseByName(Node.Parent.Text);
            Table := Database.BaseTableByName(Node.Text);
            if (not Table.Update()) then
            begin
              WantedNodeExpand := Node;
              TreeView.Cursor := crSQLWait;
            end
            else
            begin
              for I := 0 to Table.Fields.Count - 1 do
              begin
                NewNode := TreeView.Items.AddChild(Node, Table.Fields[I].Name);
                NewNode.ImageIndex := iiField;
                NewNode.Data := Table.Fields[I];
              end;
              Node.HasChildren := Assigned(Node.getFirstChild());
            end;
          end;
      end;
    end;
end;

procedure TDSearch.FSelectGetImageIndex(Sender: TObject; Node: TTreeNode);
begin
  Node.SelectedIndex := Node.ImageIndex;
end;

procedure TDSearch.FTablesDblClick(Sender: TObject);
var
  Result: Boolean;
  URI: TUURI;
  ViewFrame: TFSession;
begin
  if (Assigned(FTables.Selected)) then
  begin
    URI := TUURI.Create('');

    URI.Scheme := 'mysql';
    URI.Host := Tables[FTables.Selected.Index].Account.Connection.Host;
    URI.Port := Tables[FTables.Selected.Index].Account.Connection.Port;
    URI.Database := Tables[FTables.Selected.Index].DatabaseName;
    URI.Table := Tables[FTables.Selected.Index].TableName;
    URI.Param['view'] := 'browser';

    if (Assigned(Frame)) then
      ViewFrame := Frame
    else
      ViewFrame := TFSession(Tables[FTables.Selected.Index].Account.Tab());

    Result := True;
    if (Assigned(ViewFrame)) then
      ViewFrame.Address := URI.Address
    else
      Result := Boolean(SendMessage(Application.MainForm.Handle, UM_ADDTAB, 0, LPARAM(URI.Address)));

    URI.Free();

    if (Result) then
      FBCancel.Click();
  end;
end;

function TDSearch.GetSession(const Index: Integer): TSSession;
begin
  if (not Assigned(Sessions[Index])) then
  begin
    Sessions[Index] := TSSession.Create(fSession.Sessions, Accounts[Index]);
    DConnecting.Session := Sessions[Index];
    if (not DConnecting.Execute()) then
      FreeAndNil(Sessions[Index]);
  end;

  Result := Sessions[Index];

  if (Assigned(Result)) then
    Result.RegisterEventProc(FormSessionEvent);
end;

procedure TDSearch.mTCopyClick(Sender: TObject);
var
  ClipboardData: HGLOBAL;
  I: Integer;
  S: string;
begin
  S := '';
  for I := 0 to FTables.Items.Count - 1 do
    if ((FTables.SelCount = 0) or FTables.Items[I].Selected) then
      S := S + FTables.Items[I].Caption + #13#10;

  if ((S <> '') and OpenClipboard(Handle)) then
  begin
    EmptyClipboard();

    ClipboardData := GlobalAlloc(GMEM_MOVEABLE + GMEM_DDESHARE, SizeOf(S[1]) * (Length(S) + 1));
    StrPCopy(GlobalLock(ClipboardData), S);
    SetClipboardData(CF_UNICODETEXT, ClipboardData);
    GlobalUnlock(ClipboardData);

    CloseClipboard();
  end;
end;

procedure TDSearch.OnError(const Sender: TObject; const Error: TTool.TError; const Item: TTool.TItem; const ShowRetry: Boolean; var Success: TDataAction);
var
  ErrorMsg: string;
  Flags: Integer;
  Msg: string;
begin
  ErrorMsg := '';
  case (Error.ErrorType) of
    TE_Database:
      begin
        Msg := Preferences.LoadStr(165, IntToStr(Error.Session.Connection.ErrorCode), Error.Session.Connection.ErrorMessage);
        ErrorMsg := SQLUnwrapStmt(Error.Session.Connection.ErrorMessage, Error.Session.Connection.MySQLVersion);
        if (Error.Session.Connection.ErrorCode > 0) then
          ErrorMsg := ErrorMsg + ' (#' + IntToStr(Error.Session.Connection.ErrorCode) + ')';
      end;
    TE_File:
      begin
        Msg := Error.ErrorMessage + ' (#' + IntToStr(Error.ErrorCode) + ')';
        ErrorMsg := Msg;
      end;
    TE_NoPrimaryIndex:
      Msg := Preferences.LoadStr(722, TTSearch.TItem(Item).TableName);
    else
      Msg := Error.ErrorMessage;
  end;

  if (not ShowRetry) then
    Flags := MB_OK + MB_ICONERROR
  else
    Flags := MB_CANCELTRYCONTINUE + MB_ICONERROR;
  case (MsgBox(Msg, Preferences.LoadStr(45), Flags)) of
    IDOK,
    IDCANCEL,
    IDABORT: Success := daAbort;
    IDRETRY,
    IDTRYAGAIN: Success := daRetry;
    IDCONTINUE,
    IDIGNORE: Success := daFail;
  end;

  if ((Success in [daAbort, daFail]) and (ErrorMsg <> '')) then
  begin
    FErrors.Caption := IntToStr(TTool(Sender).ErrorCount);
    FErrorMessages.Lines.Add(ErrorMsg);
  end;
end;

procedure TDSearch.OnSearched(const AItem: TTSearch.TItem);
var
  Found: Boolean;
  I: Integer;
  Item: TListItem;
begin
  if (AItem.Done and (AItem.RecordsFound > 0)) then
  begin
    Found := False;
    for I := 0 to Length(Tables) - 1 do
      if ((Tables[I].Account = ExecuteSession.Account) and (Tables[I].DatabaseName = AItem.DatabaseName) and (Tables[I].TableName = AItem.TableName)) then
        Found := True;

    if (not Found) then
    begin
      SetLength(Tables, Length(Tables) + 1);

      Tables[Length(Tables) - 1].Account := ExecuteSession.Account;
      Tables[Length(Tables) - 1].DatabaseName := AItem.DatabaseName;
      Tables[Length(Tables) - 1].TableName := AItem.TableName;

      Item := FTables.Items.Add();
      if (not (FSelect.Selected.ImageIndex in [iiDatabase, iiBaseTable, iiField])) then
        Item.Caption := Item.Caption + ExecuteSession.Account.Name + '.';
      Item.Caption := Item.Caption + AItem.DatabaseName + '.';
      Item.Caption := Item.Caption + AItem.TableName + ' (' + IntToStr(AItem.RecordsFound) + ')';
    end;
  end;
end;

procedure TDSearch.OnTerminate(Sender: TObject);
begin
  PostMessage(Handle, UM_TERMINATE, WPARAM(not Search.Terminated), 0);
end;

procedure TDSearch.OnUpdate(const AProgressInfos: TTool.TProgressInfos);
begin
  MoveMemory(@ProgressInfos, @AProgressInfos, SizeOf(AProgressInfos));

  PostMessage(Handle, UM_UPDATEPROGRESSINFO, 0, LPARAM(@ProgressInfos))
end;

procedure TDSearch.TSExecuteShow(Sender: TObject);

  procedure InitializeNode(const Session: TSSession; const Node: TTreeNode);
  var
    Database: TSDatabase;
    I: Integer;
    J: Integer;
    Objects: TList;
  begin
    Objects := TList.Create();
    case (Node.ImageIndex) of
      iiServer:
        begin
          WantedExecute := not Session.Update();
          if (not WantedExecute) then
            for I := 0 to Session.Databases.Count - 1 do
              if (not WantedExecute and not (Session.Databases[I] is TSSystemDatabase)) then
              begin
                Database := Session.Databases[I];
                WantedExecute := not Database.Tables.Update();
                if (not WantedExecute) then
                begin
                  for J := 0 to Database.Tables.Count - 1 do
                    if (Database.Tables[J] is TSBaseTable) then
                      Objects.Add(Database.Tables[J]);
                  WantedExecute := not Session.Update(Objects);
                end;
              end;
        end;
      iiDatabase:
        begin
          Database := Session.DatabaseByName(Node.Text);
          WantedExecute := not Database.Tables.Update();
          if (not WantedExecute) then
          begin
            for J := 0 to Database.Tables.Count - 1 do
              if (Database.Tables[J] is TSBaseTable) then
                Objects.Add(Database.Tables[J]);
            WantedExecute := not Session.Update(Objects);
          end;
        end;
      iiBaseTable:
        begin
          for J := 0 to Node.Parent.Count - 1 do
            if (Node.Parent.Item[J].Selected) then
              Objects.Add(Node.Parent.Item[J].Data);
          WantedExecute := not Session.Update(Objects);
        end;
    end;
    Objects.Free();
  end;

var
  Session: TSSession;
  Database: TSDatabase;
  I: Integer;
  J: Integer;
  K: Integer;
  List: TList;
  Node: TTreeNode;
  Table: TSBaseTable;
begin
  FTables.Items.BeginUpdate();
  FTables.Items.Clear();
  FTables.Items.EndUpdate();

  FEntieredTables.Caption := '';
  FDoneTables.Caption := '';
  FEntieredRecords.Caption := '';
  FDoneRecords.Caption := '';
  FEntieredTime.Caption := '';
  FDoneTime.Caption := '';
  FProgressBar.Position := 0;
  FErrors.Caption := '0';
  FErrorMessages.Lines.Clear();

  FBBack.Enabled := False;
  FBForward.Enabled := False;
  FBCancel.Default := True;
  FBCancel.ModalResult := mrNone;
  ActiveControl := FBCancel;

  Node := FSelect.Selected;
  while (Assigned(Node.Parent)) do Node := Node.Parent;
  Session := GetSession(Node.Index);
  InitializeNode(Session, FSelect.Selected);

  if (not WantedExecute) then
  begin
    Preferences.Find.FindTextMRU.Add(Trim(FFFindText.Text));

    Preferences.Find.Options := [];
    if (FFMatchCase.Checked) then
      Include(Preferences.Find.Options, foMatchCase);
    if (FFWholeValue.Checked) then
      Include(Preferences.Find.Options, foWholeValue);
    if (FFRegExpr.Checked) then
      Include(Preferences.Find.Options, foRegExpr);

    Preferences.Replace.FindTextMRU.Add(Trim(FRFindText.Text));
    Preferences.Replace.ReplaceTextMRU.Add(Trim(FReplaceText.Text));

    Preferences.Replace.Options := [];
    if (FRMatchCase.Checked) then
      Include(Preferences.Replace.Options, roMatchCase);
    if (FRWholeValue.Checked) then
      Include(Preferences.Replace.Options, roWholeValue);
    if (FRRegExpr.Checked) then
      Include(Preferences.Replace.Options, roRegExpr);

    ExecuteSession := Session;

    if (SearchOnly) then
    begin
      SetLength(Tables, 0);

      Search := TTSearch.Create(ExecuteSession);

      Search.Wnd := Self.Handle;
      Search.FindText := FFFindText.Text;
      Search.MatchCase := FFMatchCase.Checked;
      Search.WholeValue := FFWholeValue.Checked;
      Search.RegExpr := FFRegExpr.Checked;
    end
    else
    begin
      ReplaceSession := TSSession.Create(fSession.Sessions, ExecuteSession.Account);
      DConnecting.Session := ReplaceSession;
      if (not DConnecting.Execute()) then
        FreeAndNil(ReplaceSession)
      else
      begin
        Search := TTReplace.Create(ExecuteSession, ReplaceSession);

        TTReplace(Search).Wnd := Self.Handle;
        TTReplace(Search).OnError := OnError;
        TTReplace(Search).FindText := FRFindText.Text;
        TTReplace(Search).ReplaceText := FReplaceText.Text;
        TTReplace(Search).MatchCase := FRMatchCase.Checked;
        TTReplace(Search).WholeValue := FRWholeValue.Checked;
        TTReplace(Search).RegExpr := FRRegExpr.Checked;
      end;
    end;
    Search.OnSearched := OnSearched;
    Search.OnTerminate := OnTerminate;
    Search.OnUpdate := OnUpdate;

    List := TList.Create();
    for I := 0 to FSelect.Items.Count - 1 do
      if (FSelect.Items[I].Selected) then
      begin
        if (FSelect.Selected.ImageIndex = iiField) then
        begin
          Database := ExecuteSession.DatabaseByName(FSelect.Items[I].Parent.Parent.Text);
          Table := Database.BaseTableByName(FSelect.Items[I].Parent.Text);
          List.Add(Table);
          Search.Add(Table, Table.FieldByName(FSelect.Items[I].Text));
        end
        else if (FSelect.Items[I].ImageIndex = iiBaseTable) then
        begin
          Database := ExecuteSession.DatabaseByName(FSelect.Items[I].Parent.Text);
          Table := Database.BaseTableByName(FSelect.Items[I].Text);
          List.Add(Table);
          Search.Add(Table, nil);
        end
        else if (FSelect.Items[I].ImageIndex = iiDatabase) then
        begin
          Database := ExecuteSession.DatabaseByName(FSelect.Items[I].Text);
          for J := 0 to Database.Tables.Count - 1 do
            if (Database.Tables[J] is TSBaseTable) then
            begin
              Table := TSBaseTable(Database.Tables[J]);
              List.Add(Table);
              Search.Add(Table, nil);
            end;
        end
        else // iiConnection
        begin
          for K := 0 to ExecuteSession.Databases.Count - 1 do
          begin
            Database := ExecuteSession.Databases[K];
            if (not (Database is TSSystemDatabase)) then
              for J := 0 to Database.Tables.Count - 1 do
                if (Database.Tables[J] is TSBaseTable) then
                begin
                  Table := TSBaseTable(Database.Tables[J]);
                  List.Add(Table);
                  Search.Add(Table, nil);
                end;
          end;
        end;
      end;
    if (not SearchOnly) then
      for I := 0 to List.Count - 1 do
        TSTable(List[I]).InvalidateData();
    List.Free();

    Search.Start();
  end;
end;

procedure TDSearch.TSFOptionsShow(Sender: TObject);
begin
  FFFindTextChange(Sender);

  FBBack.Enabled := True;
  FBForward.Caption := Preferences.LoadStr(174);
  FBForward.Default := True;
  FBCancel.Caption := Preferences.LoadStr(30);
  FBCancel.ModalResult := mrCancel;
  FBCancel.Default := False;

  ActiveControl := FFFindText;
end;

procedure TDSearch.TSROptionsShow(Sender: TObject);
begin
  FBBack.Enabled := True;
  FBForward.Caption := Preferences.LoadStr(174);
  FBForward.Default := True;
  FBCancel.Caption := Preferences.LoadStr(30);
  FBCancel.ModalResult := mrCancel;
  FBCancel.Default := False;

  FRFindTextChange(Sender);

  ActiveControl := FRFindText;
end;

procedure TDSearch.TSSelectShow(Sender: TObject);
begin
  FBBack.Enabled := False;
  FBForward.Caption := Preferences.LoadStr(229) + ' >';

  FSelectChange(Sender, FSelect.Selected);
end;

procedure TDSearch.UMChangePreferences(var Message: TMessage);
begin
  GSelect.Caption := Preferences.LoadStr(721);

  GFWhat.Caption := Preferences.LoadStr(227);
  FLFFindText.Caption := Preferences.LoadStr(719) + ':';

  GFOptions.Caption := Preferences.LoadStr(238);
  FLFSearchOptions.Caption := Preferences.LoadStr(715) + ':';
  FFMatchCase.Caption := Preferences.LoadStr(716);
  FFWholeValue.Caption := Preferences.LoadStr(717);
  FFRegExpr.Caption := Preferences.LoadStr(718);

  GRWhat.Caption := Preferences.LoadStr(227);
  FLRFindText.Caption := Preferences.LoadStr(719) + ':';
  FLRFindText.Caption := Preferences.LoadStr(719) + ':';
  FLReplaceText.Caption := Preferences.LoadStr(720) + ':';

  GROptions.Caption := Preferences.LoadStr(238);
  FLRSearchOptions.Caption := Preferences.LoadStr(715) + ':';
  FRMatchCase.Caption := Preferences.LoadStr(716);
  FRWholeValue.Caption := Preferences.LoadStr(717);
  FRRegExpr.Caption := Preferences.LoadStr(718);

  GProgress.Caption := Preferences.LoadStr(224);
  FLEntiered.Caption := Preferences.LoadStr(211);
  FLDone.Caption := Preferences.LoadStr(232);
  FLProgressTables.Caption := Preferences.LoadStr(234) + ':';
  FLProgressRecords.Caption := Preferences.LoadStr(235) + ':';
  FLProgressTime.Caption := Preferences.LoadStr(661) + ':';
  FLErrors.Caption := Preferences.LoadStr(391) + ':';

  mtCopy.Caption := MainAction('aECopy').Caption;

  FBHelp.Caption := Preferences.LoadStr(167);
  FBBack.Caption := '< ' + Preferences.LoadStr(228);
end;

procedure TDSearch.UMTerminate(var Message: TMessage);
var
  Success: Boolean;
begin
  Success := Boolean(Message.WParam);

  if (Success and SearchOnly and (FTables.Items.Count = 0)) then
    MsgBox(Preferences.LoadStr(533, Search.FindText), Preferences.LoadStr(43), MB_OK + MB_ICONINFORMATION);

  if (Assigned(ExecuteSession)) then
    ExecuteSession := nil;
  if (Assigned(ReplaceSession)) then
    FreeAndNil(ReplaceSession);

  FBBack.Enabled := True;
  FBForward.Enabled := False;
  FBCancel.Enabled := True;

  FBCancel.Caption := Preferences.LoadStr(231);
  if (Success) then
    FBCancel.ModalResult := mrOk
  else
    FBCancel.ModalResult := mrCancel;

  ActiveControl := FBCancel;

  if (Assigned(Search)) then
  begin
    Search.WaitFor();
    FreeAndNil(Search);
  end;
end;

procedure TDSearch.UMUpdateProgressInfo(var Message: TMessage);
var
  Infos: TTool.PProgressInfos;
begin
  Infos := TTool.PProgressInfos(Message.LParam);

  if (Infos.ObjectsSum < 0) then
    FEntieredTables.Caption := '???'
  else
    FEntieredTables.Caption := FormatFloat('#,##0', Infos.ObjectsSum, LocaleFormatSettings);
  if (Infos.ObjectsDone < 0) then
    FDoneTables.Caption := '???'
  else
    FDoneTables.Caption := FormatFloat('#,##0', Infos.ObjectsDone, LocaleFormatSettings);

  if (Infos.RecordsSum < 0) then
    FEntieredRecords.Caption := '???'
  else
    FEntieredRecords.Caption := FormatFloat('#,##0', Infos.RecordsSum, LocaleFormatSettings);
  if (Infos.RecordsDone < 0) then
    FDoneRecords.Caption := '???'
  else
    FDoneRecords.Caption := FormatFloat('#,##0', Infos.RecordsDone, LocaleFormatSettings);

  FEntieredTime.Caption := TimeToStr(Infos.TimeSum, DurationFormatSettings);
  FDoneTime.Caption := TimeToStr(Infos.TimeDone, DurationFormatSettings);

  FProgressBar.Position := Infos.Progress;
end;

initialization
  FSearch := nil;
end.

