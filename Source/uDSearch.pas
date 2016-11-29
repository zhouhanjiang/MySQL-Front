unit uDSearch;

interface {********************************************************************}

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Menus,
  Dialogs, StdCtrls, ComCtrls, DB, ExtCtrls, RichEdit,
  ComCtrls_Ext, Forms_Ext, StdCtrls_Ext, ExtCtrls_Ext,
  uSession, uPreferences, uTools,
  uBase, uFSession;

type
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
    procedure TSExecuteResize(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FTablesKeyPress(Sender: TObject; var Key: Char);
  private
    ExecuteSession: TSSession;
    ProgressInfos: TTool.TProgressInfos;
    ReplaceSession: TSSession;
    Search: TTSearch;
    Sessions: array of record
      Created: Boolean;
      Session: TSSession;
    end;
    Space: Integer;
    Wanted: record
      Node: TTreeNode;
      Page: TTabSheet;
    end;
    procedure FormSessionEvent(const Event: TSSession.TEvent);
    function GetSession(const TreeNode: TTreeNode): TSSession;
    procedure OnError(const Sender: TObject; const Error: TTool.TError; const Item: TTool.TItem; const ShowRetry: Boolean; var Success: TDataAction);
    procedure OnSearched(const AItem: TTSearch.TItem);
    procedure OnTerminate(Sender: TObject);
    procedure OnUpdate(const AProgressInfos: TTool.TProgressInfos);
    procedure UMChangePreferences(var Message: TMessage); message UM_CHANGEPREFERENCES;
    procedure UMPostAfterExecuteSQL(var Message: TMessage); message UM_POST_AFTEREXECUTESQL;
    procedure UMTerminate(var Message: TMessage); message UM_TERMINATE;
    procedure UMUpdateProgressInfo(var Message: TMessage); message UM_UPDATEPROGRESSINFO;
  public
    Session: TSSession;
    Database: TSDatabase;
    Tab: TFSession;
    SearchOnly: Boolean;
    function Execute(): Boolean;
  end;

function DSearch(): TDSearch;

implementation {***************************************************************}

{$R *.dfm}

uses
  Consts, StrUtils, CommCtrl, SysConst,
  SQLUtils,
  uURI,
  uDConnecting;

var
  FDSearch: TDSearch;

function DSearch(): TDSearch;
begin
  if (not Assigned(FDSearch)) then
  begin
    Application.CreateForm(TDSearch, FDSearch);
    FDSearch.Perform(UM_CHANGEPREFERENCES, 0, 0);
  end;

  Result := FDSearch;
end;

{ TDSearch ********************************************************************}

function TDSearch.Execute(): Boolean;
begin
  ShowModal();
  Result := ModalResult = mrOk;
end;

procedure TDSearch.FBBackClick(Sender: TObject);
var
  PageIndex: Integer;
begin
  for PageIndex := PageControl.ActivePageIndex - 1 downto 0 do
    if (PageControl.Pages[PageIndex].Enabled) then
    begin
      PageControl.ActivePageIndex := PageIndex;
      exit;
    end;
end;

procedure TDSearch.FBCancelClick(Sender: TObject);
begin
  if (Assigned(Search)) then
    Search.Terminate();
end;

procedure TDSearch.FBForwardClick(Sender: TObject);
var
  PageIndex: Integer;
begin
  for PageIndex := PageControl.ActivePageIndex + 1 to PageControl.PageCount - 1 do
    if (PageControl.Pages[PageIndex].Enabled) then
    begin
      PageControl.ActivePageIndex := PageIndex;
      exit;
    end;
end;

procedure TDSearch.FBHelpClick(Sender: TObject);
begin
  Application.HelpContext(HelpContext);
end;

procedure TDSearch.FFFindTextChange(Sender: TObject);
begin
  FBForward.Enabled := Trim(FFFindText.Text) <> '';
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
    if (Assigned(Wanted.Node) or Assigned(Wanted.Page) and Assigned(Wanted.Page.OnShow)) then
      PostMessage(Handle, UM_POST_AFTEREXECUTESQL, 0, 0);
  end;
end;

procedure TDSearch.FormCreate(Sender: TObject);
var
  I: Integer;
begin
  Space := (FLEntiered.Left + FLEntiered.Width) - (FLDone.Left + FLDone.Width);

  Constraints.MinWidth := Width;
  Constraints.MinHeight := Height;

  BorderStyle := bsSizeable;

  Search := nil;

  FSelect.Images := Preferences.Images;

  FFFindText.Items.Clear();
  for I := Preferences.Find.FindTextMRU.Count - 1 downto 0 do
    FFFindText.Items.Add(Preferences.Find.FindTextMRU.Values[I]);
  FFFindText.Text := '';

  FFMatchCase.Checked := foMatchCase in Preferences.Find.Options;
  FFWholeValue.Checked := foWholeValue in Preferences.Find.Options;
  FFRegExpr.Checked := foRegExpr in Preferences.Find.Options;
  FFRegExprClick(Sender);

  FRFindText.Items.Clear();
  for I := Preferences.Replace.FindTextMRU.Count - 1 downto 0 do
    FRFindText.Items.Add(Preferences.Replace.FindTextMRU.Values[I]);
  FRFindText.Text := '';
  FReplaceText.Items.Clear();
  for I := Preferences.Replace.ReplaceTextMRU.Count - 1 downto 0 do
    FReplaceText.Items.Add(Preferences.Replace.ReplaceTextMRU.Values[I]);
  FReplaceText.Text := '';

  FRMatchCase.Checked := roMatchCase in Preferences.Replace.Options;
  FRWholeValue.Checked := roWholeValue in Preferences.Replace.Options;
  FRRegExpr.Checked := roRegExpr in Preferences.Replace.Options;
  FRRegExprClick(Sender);

  SendMessage(FErrorMessages.Handle, EM_SETTEXTMODE, TM_PLAINTEXT, 0);
  SendMessage(FErrorMessages.Handle, EM_SETWORDBREAKPROC, 0, LPARAM(@EditWordBreakProc));

  PageControl.ActivePage := nil; // Make sure, not ___OnShowPage will be executed
end;

procedure TDSearch.FormDestroy(Sender: TObject);
begin
  if (Assigned(Search)) then
    TerminateThread(Search.Handle, 0);
end;

procedure TDSearch.FormHide(Sender: TObject);
var
  I: Integer;
begin
  FSelect.Selected := nil; // Make sure, not to call FSelectedChange with a selected node
  FSelect.Items.BeginUpdate();
  FSelect.Items.Clear();
  FSelect.Items.EndUpdate();
  FTables.Items.BeginUpdate();
  FTables.Items.Clear();
  FTables.Items.EndUpdate();

  for I := 0 to Length(Sessions) - 1 do
    if (Assigned(Sessions[I].Session)) then
    begin
      Sessions[I].Session.ReleaseEventProc(FormSessionEvent);
      if (Sessions[I].Created) then
        Sessions[I].Session.Free();
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

  ExecuteSession := nil;
  Wanted.Node := nil;
  Wanted.Page := nil;
  FErrorMessages.Visible := not SearchOnly;
  FTables.Visible := SearchOnly;

  SetLength(Sessions, Accounts.Count);
  for I := 0 to Length(Sessions) - 1 do
  begin
    Sessions[I].Created := False;
    Sessions[I].Session := nil;
  end;

  TSFOptions.Enabled := SearchOnly;
  TSROptions.Enabled := not SearchOnly;

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
  FBForward.Enabled := (Trim(FRFindText.Text) <> '')
    and (Trim(FReplaceText.Text) <> '')
    and (Trim(FRFindText.Text) <> Trim(FReplaceText.Text));
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
  Table: TSTable;
  TreeView: TTreeView_Ext;
begin
  TreeView := TTreeView_Ext(Sender);

  if (Assigned(Wanted.Node)) then
    Wanted.Node := nil;

  if (Assigned(Node)) then
    if (Node.HasChildren and not Assigned(Node.getFirstChild())) then
    begin
      case (Node.ImageIndex) of
        iiServer:
          begin
            Session := GetSession(Node);
            if (Assigned(Session)) then
            begin
              Node.Data := Session;
              if (not Session.Update()) then
                Wanted.Node := Node
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
          end;
        iiDatabase:
          begin
            Session := TSSession(Node.Parent.Data);
            Database := TSDatabase(Node.Data);
            if ((not Database.Tables.Update() or not Session.Update(Database.Tables))) then
              Wanted.Node := Node
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
            Table := TSTable(Node.Data);
            if (not Table.Update()) then
              Wanted.Node := Node
            else
            begin
              for I := 0 to Table.Fields.Count - 1 do
              begin
                NewNode := TreeView.Items.AddChild(Node, Table.Fields[I].Name);
                if (Table is TSBaseTable) then
                  NewNode.ImageIndex := iiField
                else
                  NewNode.ImageIndex := iiViewField;
                NewNode.Data := Table.Fields[I];
              end;
              Node.HasChildren := Assigned(Node.getFirstChild());
            end;
          end;
      end;
    end;

  if (not Assigned(Wanted.Node)) then
    TreeView.Cursor := crDefault
  else
    TreeView.Cursor := crSQLWait;
end;

procedure TDSearch.FSelectGetImageIndex(Sender: TObject; Node: TTreeNode);
begin
  Node.SelectedIndex := Node.ImageIndex;
end;

procedure TDSearch.FTablesDblClick(Sender: TObject);
var
  Result: Boolean;
  URI: TUURI;
  FSession: TFSession;
begin
  if (Assigned(FTables.Selected)) then
  begin
    URI := TUURI.Create('');

    URI.Scheme := 'mysql';
    URI.Host := TSTable(FTables.Selected.Data).Database.Session.Account.Connection.Host;
    URI.Port := TSTable(FTables.Selected.Data).Database.Session.Account.Connection.Port;
    URI.Database := TSTable(FTables.Selected.Data).Database.Name;
    URI.Table := TSTable(FTables.Selected.Data).Name;
    URI.Param['view'] := 'browser';

    if (Assigned(Tab)) then
      FSession := Tab
    else
      FSession := TFSession(TSTable(FTables.Selected.Data).Database.Session.Account.Tab());

    Result := True;
    if (Assigned(FSession)) then
      FSession.Address := URI.Address
    else
      Result := Boolean(SendMessage(Application.MainForm.Handle, UM_ADDTAB, 0, LPARAM(URI.Address)));

    URI.Free();

    if (Result) then
      FBCancel.Click();
  end;
end;

procedure TDSearch.FTablesKeyPress(Sender: TObject; var Key: Char);
begin
  if (Key = #13) then
    FTablesDblClick(Sender)
  else
    MessageBeep(MB_ICONERROR);
end;

function TDSearch.GetSession(const TreeNode: TTreeNode): TSSession;
var
  Index: Integer;
  Node: TTreeNode;
begin
  Node := TreeNode;
  while (Assigned(Node.Parent)) do
    Node := Node.Parent;

  if (not Assigned(Node.Data)) then
  begin
    Index := Node.Index; // Cache for speeding - Index is slow

    if (Assigned(Session) and (Session.Account = Accounts[Index])) then
      Sessions[Index].Session := Session;

    if (not Assigned(Sessions[Index].Session)) then
      Sessions[Index].Session := uSession.Sessions.SessionByAccount(Accounts[Index]);

    if (not Assigned(Sessions[Index].Session)) then
    begin
      DConnecting.Session := TSSession.Create(uSession.Sessions, Accounts[Index]);
      if (not DConnecting.Execute()) then
        DConnecting.Session.Free()
      else
      begin
        Sessions[Index].Created := True;
        Sessions[Index].Session := DConnecting.Session;
      end;
    end;

    if (Assigned(Sessions[Index].Session)) then
      Sessions[Index].Session.RegisterEventProc(FormSessionEvent);

    Node.Data := Sessions[Index].Session;
  end;

  Result := TSSession(Node.Data);
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
        ErrorMsg := Error.ErrorMessage
          + ' (#' + IntToStr(Error.ErrorCode) + ') - ' + Trim(Session.Connection.ErrorCommandText);
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
    FErrorMessages.Text := FErrorMessages.Text + ErrorMsg;
  end;
end;

procedure TDSearch.OnSearched(const AItem: TTSearch.TItem);
var
  Item: TListItem;
begin
  if (AItem.Done and (AItem.RecordsFound > 0)) then
  begin
    Item := FTables.Items.Add();
    Item.Caption := AItem.DatabaseName + '.' + AItem.TableName + ' (' + IntToStr(AItem.RecordsFound) + ')';
    Item.Data := ExecuteSession.DatabaseByName(AItem.DatabaseName).TableByName(AItem.TableName);
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

procedure TDSearch.TSExecuteResize(Sender: TObject);
begin
  FLEntiered.Left := GProgress.ClientWidth - 2 * FProgressBar.Left - FLEntiered.Width;
  FLDone.Left := GProgress.ClientWidth - 2 * FProgressBar.Left - Space - FLDone.Width;
  FEntieredTables.Left := GProgress.ClientWidth - 2 * FProgressBar.Left - FEntieredTables.Width;
  FDoneTables.Left := GProgress.ClientWidth - 2 * FProgressBar.Left - Space - FDoneTables.Width;
  FEntieredRecords.Left := GProgress.ClientWidth - 2 * FProgressBar.Left - FEntieredRecords.Width;
  FDoneRecords.Left := GProgress.ClientWidth - 2 * FProgressBar.Left - Space - FDoneRecords.Width;
  FEntieredTime.Left := GProgress.ClientWidth - 2 * FProgressBar.Left - FEntieredTime.Width;
  FDoneTime.Left := GProgress.ClientWidth - 2 * FProgressBar.Left - Space - FDoneTime.Width;
  FErrors.Left := GProgress.ClientWidth - 2 * FProgressBar.Left - FErrors.Width;
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
          if (not Session.Update()) then
            Wanted.Page := TSExecute
          else
            for I := 0 to Session.Databases.Count - 1 do
              if (not (Session.Databases[I] is TSSystemDatabase)) then
              begin
                Database := Session.Databases[I];
                if (not Database.Tables.Update()) then
                  Wanted.Page := TSExecute
                else
                begin
                  for J := 0 to Database.Tables.Count - 1 do
                    if (Database.Tables[J] is TSBaseTable) then
                      Objects.Add(Database.Tables[J]);
                  if (not Session.Update(Objects)) then
                    Wanted.Page := TSExecute;
                end;
              end;
        end;
      iiDatabase:
        begin
          Database := Session.DatabaseByName(Node.Text);
          if (not Database.Tables.Update()) then
            Wanted.Page := TSExecute
          else
          begin
            for J := 0 to Database.Tables.Count - 1 do
              if (Database.Tables[J] is TSBaseTable) then
                Objects.Add(Database.Tables[J]);
            if (not Session.Update(Objects)) then
              Wanted.Page := TSExecute;
          end;
        end;
      iiBaseTable:
        begin
          for J := 0 to Node.Parent.Count - 1 do
            if (Node.Parent.Item[J].Selected) then
              Objects.Add(Node.Parent.Item[J].Data);
          if (not Session.Update(Objects)) then
            Wanted.Page := TSExecute;
        end;
    end;
    Objects.Free();
  end;

var
  Database: TSDatabase;
  I: Integer;
  J: Integer;
  K: Integer;
  List: TList;
  Node: TTreeNode;
  Table: TSTable;
begin
  FEntieredTables.Caption := '';
  FDoneTables.Caption := '';
  FEntieredRecords.Caption := '';
  FDoneRecords.Caption := '';
  FEntieredTime.Caption := '';
  FDoneTime.Caption := '';
  FProgressBar.Position := 0;
  FErrors.Caption := '0';
  FErrorMessages.Lines.Clear();
  FTables.Items.BeginUpdate();
  FTables.Items.Clear();
  FTables.Items.EndUpdate();

  Node := FSelect.Selected;
  while (Assigned(Node.Parent)) do Node := Node.Parent;
  ExecuteSession := TSSession(Node.Data);
  InitializeNode(ExecuteSession, FSelect.Selected);

  if (not Assigned(Wanted.Page)) then
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

    if (Assigned(Search)) then
      TerminateThread(Search.Handle, 0);
    if (SearchOnly) then
    begin
      Search := TTSearch.Create(ExecuteSession);
      Search.Wnd := Self.Handle;
      Search.FindText := Trim(FFFindText.Text);
      Search.MatchCase := FFMatchCase.Checked;
      Search.WholeValue := FFWholeValue.Checked;
      Search.RegExpr := FFRegExpr.Checked;
    end
    else
    begin
      DConnecting.Session := TSSession.Create(uSession.Sessions, ExecuteSession.Account);
      if (not DConnecting.Execute()) then
        DConnecting.Session.Free()
      else
      begin
        ReplaceSession := DConnecting.Session;
        Search := TTReplace.Create(ExecuteSession, ReplaceSession);

        TTReplace(Search).Wnd := Self.Handle;
        TTReplace(Search).OnError := OnError;
        TTReplace(Search).FindText := Trim(FRFindText.Text);
        TTReplace(Search).ReplaceText := Trim(FReplaceText.Text);
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
        if (FSelect.Selected.ImageIndex in [iiField, iiViewField]) then
        begin
          Table := TSTable(FSelect.Items[I].Parent.Data);
          List.Add(Table);
          Search.Add(Table, Table.FieldByName(FSelect.Items[I].Text));
        end
        else if (FSelect.Items[I].ImageIndex in [iiBaseTable, iiView]) then
        begin
          Table := TSTable(FSelect.Items[I].Data);
          List.Add(Table);
          Search.Add(Table, nil);
        end
        else if (FSelect.Items[I].ImageIndex = iiDatabase) then
        begin
          Database := ExecuteSession.DatabaseByName(FSelect.Items[I].Text);
          for J := 0 to Database.Tables.Count - 1 do
          begin
            Table := Database.Tables[J];
            List.Add(Table);
            Search.Add(Table, nil);
          end;
        end
        else // iiServer
        begin
          for K := 0 to ExecuteSession.Databases.Count - 1 do
          begin
            Database := ExecuteSession.Databases[K];
            if (not (Database is TSSystemDatabase)) then
              for J := 0 to Database.Tables.Count - 1 do
              begin
                Table := Database.Tables[J];
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

    FBBack.Enabled := False;

    Search.Start();
  end;

  if (not Assigned(Wanted.Page)) then
    SetControlCursor(GProgress, crDefault)
  else
    SetControlCursor(GProgress, crSQLWait);

  FBBack.Enabled := False;
  FBForward.Enabled := False;
  FBForward.Default := False;
  FBCancel.Default := True;
  ActiveControl := FBCancel;
end;

procedure TDSearch.TSFOptionsShow(Sender: TObject);
begin
  FFFindTextChange(Sender);

  FBBack.Enabled := True;
  FBForward.Caption := Preferences.LoadStr(174);
  FBForward.Default := True;
  FBCancel.Caption := Preferences.LoadStr(30);
  FBCancel.Default := False;

  ActiveControl := FFFindText;
end;

procedure TDSearch.TSROptionsShow(Sender: TObject);
begin
  FBBack.Enabled := True;
  FBForward.Caption := Preferences.LoadStr(174);
  FBForward.Default := True;
  FRFindTextChange(Sender);
  FBCancel.Caption := Preferences.LoadStr(30);
  FBCancel.Default := False;

  ActiveControl := FRFindText;
end;

procedure TDSearch.TSSelectShow(Sender: TObject);
var
  I: Integer;
  J: Integer;
  Node: TTreeNode;
begin
  if (FSelect.Items.Count = 0) then
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

procedure TDSearch.UMPostAfterExecuteSQL(var Message: TMessage);
var
  Node: TTreeNode;
begin
  if (Assigned(Wanted.Node)) then
  begin
    Node := Wanted.Node;
    Wanted.Node := nil;
    Node.Expand(False);
  end
  else if (Assigned(Wanted.Page)) then
  begin
    Wanted.Page.OnShow(nil);
    Wanted.Page := nil;
  end;
end;

procedure TDSearch.UMTerminate(var Message: TMessage);
var
  Success: Boolean;
begin
  Success := Boolean(Message.WParam);

  if (Success and SearchOnly and (FTables.Items.Count = 0)) then
    MsgBox(Preferences.LoadStr(533, Search.FindText), Preferences.LoadStr(43), MB_OK + MB_ICONINFORMATION);

  if (Assigned(Search)) then
  begin
    Search.WaitFor();
    FreeAndNil(Search);

    ReplaceSession.Free();
  end;

  FBBack.Enabled := True;
  FBCancel.Caption := Preferences.LoadStr(231);
  if (Success) then
    FBCancel.ModalResult := mrOk
  else
    FBCancel.ModalResult := mrCancel;

  ActiveControl := FBCancel;
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
  FDSearch := nil;
end.

