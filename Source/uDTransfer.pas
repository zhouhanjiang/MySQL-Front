unit uDTransfer;

interface {********************************************************************}

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms,
  Dialogs, ComCtrls, ExtCtrls, StdCtrls, DB, Menus, RichEdit,
  ComCtrls_Ext, Forms_Ext, ExtCtrls_Ext, StdCtrls_Ext,
  MySQLDB,
  uSession, uTools,
  uBase;

type
  TDTransfer = class(TForm_Ext)
    FBBack: TButton;
    FBCancel: TButton;
    FBForward: TButton;
    FBHelp: TButton;
    FData: TCheckBox;
    FDestination: TTreeView_Ext;
    FDoneRecords: TLabel;
    FDoneObjects: TLabel;
    FDoneTime: TLabel;
    FEntieredRecords: TLabel;
    FEntieredObjects: TLabel;
    FEntieredTime: TLabel;
    FErrorMessages: TRichEdit;
    FErrors: TLabel;
    FLDone: TLabel;
    FLEntiered: TLabel;
    FLErrors: TLabel;
    FLProgressRecords: TLabel;
    FLProgressObjects: TLabel;
    FLProgressTime: TLabel;
    FLWhat: TLabel;
    FProgressBar: TProgressBar;
    FSource: TTreeView_Ext;
    FStructure: TCheckBox;
    GDestination: TGroupBox_Ext;
    GErrorMessages: TGroupBox_Ext;
    GProgress: TGroupBox;
    GSource: TGroupBox_Ext;
    GWhat: TGroupBox;
    PageControl: TPageControl;
    PDestination: TPanel_Ext;
    PErrorMessages: TPanel_Ext;
    PSource: TPanel_Ext;
    TSExecute: TTabSheet;
    TSSelect: TTabSheet;
    TSWhat: TTabSheet;
    procedure FBBackClick(Sender: TObject);
    procedure FBCancelClick(Sender: TObject);
    procedure FBForwardClick(Sender: TObject);
    procedure FBHelpClick(Sender: TObject);
    procedure FDataClick(Sender: TObject);
    procedure FDataKeyPress(Sender: TObject; var Key: Char);
    procedure FormCreate(Sender: TObject);
    procedure FormHide(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure FStructureClick(Sender: TObject);
    procedure FStructureKeyPress(Sender: TObject; var Key: Char);
    procedure TreeViewChange(Sender: TObject; Node: TTreeNode);
    procedure TreeViewExpanding(Sender: TObject; Node: TTreeNode;
      var AllowExpansion: Boolean);
    procedure TreeViewGetSelectedIndex(Sender: TObject; Node: TTreeNode);
    procedure TSExecuteShow(Sender: TObject);
    procedure TSSelectResize(Sender: TObject);
    procedure TSSelectShow(Sender: TObject);
    procedure TSWhatShow(Sender: TObject);
    procedure FSourceChanging(Sender: TObject; Node: TTreeNode;
      var AllowChange: Boolean);
    procedure FormDestroy(Sender: TObject);
  private
    ProgressInfos: TTool.TProgressInfos;
    Sessions: array of record
      Created: Boolean;
      Session: TSSession;
    end;
    Transfer: TTTransfer;
    Wanted: record
      Page: TTabSheet;
      Node: TTreeNode;
    end;
    procedure CheckActivePageChange(const ActivePageIndex: Integer);
    procedure FormSessionEvent(const Event: TSSession.TEvent);
    function GetSession(const TreeNode: TTreeNode): TSSession;
    procedure OnError(const Sender: TObject; const Error: TTool.TError; const Item: TTool.TItem; const ShowRetry: Boolean; var Success: TDataAction);
    procedure OnTerminate(Sender: TObject);
    procedure OnUpdate(const AProgressInfos: TTool.TProgressInfos);
    procedure UMChangePreferences(var Message: TMessage); message UM_CHANGEPREFERENCES;
    procedure UMPostAfterExecuteSQL(var Message: TMessage); message UM_POST_AFTEREXECUTESQL;
    procedure UMTerminate(var Message: TMessage); message UM_TERMINATE;
    procedure UMUpdateProgressInfo(var Message: TMessage); message UM_UPDATEPROGRESSINFO;
  public
    SourceSession: TSSession;
    SourceDatabase: TSDatabase;
    function Execute(): Boolean;
  end;

function DTransfer(): TDTransfer;

implementation {***************************************************************}

{$R *.dfm}

uses
  StrUtils, CommCtrl, Consts,
  SQLUtils,
  uPreferences,
  uDConnecting, uDExecutingSQL;

var
  FDTransfer: TDTransfer;

function DTransfer(): TDTransfer;
begin
  if (not Assigned(FDTransfer)) then
  begin
    Application.CreateForm(TDTransfer, FDTransfer);
    FDTransfer.Perform(UM_CHANGEPREFERENCES, 0, 0);
  end;

  Result := FDTransfer;
end;

function TreeViewSelCount(const TreeView: TTreeView_Ext): Integer;
var
  I: Integer;
begin
  Result := 0;

  for I := 0 to TreeView.Items.Count - 1 do
    if (TreeView.Items[I].Selected) then
      Inc(Result);
end;

{ TDTransfer ******************************************************************}

procedure TDTransfer.CheckActivePageChange(const ActivePageIndex: Integer);
var
  I: Integer;
  NextActivePageIndex: Integer;
begin
  FBBack.Enabled := False;
  for I := 0 to PageControl.PageCount - 1 do
    FBBack.Enabled := FBBack.Enabled or PageControl.Pages[I].Enabled and (I < ActivePageIndex);

  NextActivePageIndex := -1;
  for I := PageControl.PageCount - 1 downto ActivePageIndex + 1 do
    if (PageControl.Pages[I].Enabled) then
      NextActivePageIndex := I;
  if (NextActivePageIndex >= 0) then
    for I := NextActivePageIndex + 1 to PageControl.PageCount - 1 do
      PageControl.Pages[I].Enabled := False;

  if (not FBBack.Enabled) then
    FBForward.Caption := Preferences.LoadStr(229) + ' >'
  else
    FBForward.Caption := Preferences.LoadStr(174);

  FBForward.Enabled := FBForward.Visible and (NextActivePageIndex >= 0);
  FBForward.Default := PageControl.ActivePage <> TSExecute;
  FBCancel.Default := not FBForward.Default;
  FBCancel.Caption := Preferences.LoadStr(30);
end;

function TDTransfer.Execute(): Boolean;
begin
  ShowModal();
  Result := ModalResult = mrOk;
end;

procedure TDTransfer.FBBackClick(Sender: TObject);
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

procedure TDTransfer.FBCancelClick(Sender: TObject);
begin
  if (Assigned(Transfer)) then
    Transfer.Terminate();
end;

procedure TDTransfer.FBForwardClick(Sender: TObject);
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

procedure TDTransfer.FBHelpClick(Sender: TObject);
begin
  Application.HelpContext(HelpContext);
end;

procedure TDTransfer.FDataClick(Sender: TObject);
begin
  FStructure.Checked := FStructure.Checked or FData.Checked;

  TSExecute.Enabled := FStructure.Checked;
  CheckActivePageChange(TSWhat.PageIndex);
end;

procedure TDTransfer.FDataKeyPress(Sender: TObject; var Key: Char);
begin
  FDataClick(Sender);
end;

procedure TDTransfer.FormSessionEvent(const Event: TSSession.TEvent);
begin
  if (Event.EventType = etError) then
  begin
    FSource.Cursor := crDefault;
    FDestination.Cursor := crDefault;
    SetControlCursor(GProgress, crDefault);

    Wanted.Node := nil;
    Wanted.Page := nil;
  end
  else if (Event.EventType = etAfterExecuteSQL) then
  begin
    if (Assigned(Wanted.Node) or Assigned(Wanted.Page) and Assigned(Wanted.Page.OnShow)) then
      PostMessage(Handle, UM_POST_AFTEREXECUTESQL, 0, 0);
  end;
end;

procedure TDTransfer.FormCreate(Sender: TObject);
begin
  Constraints.MinWidth := Width;
  Constraints.MinHeight := Height;

  BorderStyle := bsSizeable;

  Transfer := nil;

  FSource.Images := Preferences.Images;
  FDestination.Images := Preferences.Images;

  SendMessage(FErrorMessages.Handle, EM_SETTEXTMODE, TM_PLAINTEXT, 0);
  SendMessage(FErrorMessages.Handle, EM_SETWORDBREAKPROC, 0, LPARAM(@EditWordBreakProc));

  FStructure.Checked := Preferences.Transfer.Structure;
  FData.Checked := Preferences.Transfer.Data;

  PageControl.ActivePage := nil; // Make sure, not ___OnShowPage will be executed
end;

procedure TDTransfer.FormDestroy(Sender: TObject);
begin
  if (Assigned(Transfer)) then
    TerminateThread(Transfer.Handle, 0);
end;

procedure TDTransfer.FormHide(Sender: TObject);
var
  I: Integer;
begin
  for I := 0 to Length(Sessions) - 1 do
    if (Assigned(Sessions[I].Session)) then
    begin
      Sessions[I].Session.ReleaseEventProc(FormSessionEvent);
      if (Sessions[I].Created) then
        Sessions[I].Session.Free();
    end;
  SetLength(Sessions, 0);

  Preferences.Transfer.Height := Height;
  Preferences.Transfer.Width := Width;

  if (ModalResult = mrOK) then
  begin
    Preferences.Transfer.Data := FData.Checked;
    Preferences.Transfer.Structure := FStructure.Checked;
  end;

  FSource.Items.BeginUpdate();
  FSource.Items.Clear();
  FSource.Items.EndUpdate();
  FDestination.Items.BeginUpdate();
  FDestination.Items.Clear();
  FDestination.Items.EndUpdate();

  PageControl.ActivePage := nil; // Make sure, not ___OnShowPage will be executed
end;

procedure TDTransfer.FormShow(Sender: TObject);
var
  I: Integer;
begin
  HelpContext := 1094;

  if ((Preferences.Transfer.Width >= Width) and (Preferences.Transfer.Height >= Height)) then
  begin
    Width := Preferences.Transfer.Width;
    Height := Preferences.Transfer.Height;
  end;

  Wanted.Node := nil;
  Wanted.Page := nil;

  if (Assigned(Transfer)) then
  begin
    TerminateThread(Transfer.Handle, 0);
    Transfer := nil;
  end;

  SetLength(Sessions, Accounts.Count);
  for I := 0 to Length(Sessions) - 1 do
  begin
    Sessions[I].Created := False;
    Sessions[I].Session := nil;
  end;


  TSSelect.Enabled := True;
  TSWhat.Enabled := False;
  TSExecute.Enabled := False;

  PageControl.ActivePage := TSSelect;
  CheckActivePageChange(PageControl.ActivePageIndex);

  FBCancel.Caption := Preferences.LoadStr(30);
  FBCancel.ModalResult := mrCancel;
  FBCancel.Default := False;

  if (Assigned(SourceSession)) then
    ActiveControl := FSource
  else
    ActiveControl := FBCancel;
end;

procedure TDTransfer.FSourceChanging(Sender: TObject; Node: TTreeNode;
  var AllowChange: Boolean);
begin
  AllowChange := Assigned(Node) and (Node.ImageIndex <> iiServer);
end;

procedure TDTransfer.FStructureClick(Sender: TObject);
begin
  FData.Checked := FData.Checked and FStructure.Checked;

  TSExecute.Enabled := FStructure.Checked;
  CheckActivePageChange(TSWhat.PageIndex);
end;

procedure TDTransfer.FStructureKeyPress(Sender: TObject; var Key: Char);
begin
  FStructureClick(Sender);
end;

function TDTransfer.GetSession(const TreeNode: TTreeNode): TSSession;
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

    if (Assigned(SourceSession) and (SourceSession.Account = Accounts[Index])) then
      Sessions[Index].Session := SourceSession;

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

procedure TDTransfer.OnError(const Sender: TObject; const Error: TTool.TError; const Item: TTool.TItem; const ShowRetry: Boolean; var Success: TDataAction);
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
          + ' (#' + IntToStr(Error.ErrorCode) + ') - ' + Trim(Error.Session.Connection.ErrorCommandText);
      end;
    TE_File:
      begin
        Msg := Error.ErrorMessage + ' (#' + IntToStr(Error.ErrorCode) + ')';
        ErrorMsg := Msg;
      end;
    TE_NoPrimaryIndex:
      if ((Sender is TTTransfer) and (Error.Session = TTTransfer(Sender).Session)) then
        Msg := Preferences.LoadStr(722, TTTransfer.TItem(Item).DBObject.Name)
      else if ((Sender is TTTransfer) and (Error.Session = TTTransfer(Sender).DestinationSession)) then
        Msg := Preferences.LoadStr(722, TTTransfer.TItem(Item).DBObject.Name)
      else
        raise ERangeError.CreateFmt(SPropertyOutOfRange, ['Sender | Error.Session']);
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

procedure TDTransfer.OnTerminate(Sender: TObject);
begin
  if (Assigned(Transfer)) then
    PostMessage(Handle, UM_TERMINATE, WPARAM(not Transfer.Terminated), 0);
end;

procedure TDTransfer.OnUpdate(const AProgressInfos: TTool.TProgressInfos);
begin
  MoveMemory(@ProgressInfos, @AProgressInfos, SizeOf(AProgressInfos));

  PostMessage(Handle, UM_UPDATEPROGRESSINFO, 0, LPARAM(@ProgressInfos));
end;

procedure TDTransfer.TreeViewChange(Sender: TObject; Node: TTreeNode);
begin
  if (ModalResult = mrNone) then
  begin
    if ((Sender = FSource) and Assigned(Node)) then
      FSource.MultiSelect := Assigned(Node.Parent);

    TSWhat.Enabled := Assigned(FSource.Selected) and Assigned(FSource.Selected.Parent) and Assigned(FDestination.Selected)
      and (FSource.Selected.Parent.Level = FDestination.Selected.Level)
      and ((FSource.Selected.ImageIndex <> iiDatabase) or (FSource.Selected.Parent.Text <> FDestination.Selected.Text))
      and ((FSource.Selected.ImageIndex <> iiBaseTable) or (FSource.Selected.Parent.Text <> FDestination.Selected.Text) or (FSource.Selected.Parent.Parent.Text <> FDestination.Selected.Parent.Text));

    CheckActivePageChange(PageControl.ActivePageIndex);
  end;
end;

procedure TDTransfer.TreeViewExpanding(Sender: TObject; Node: TTreeNode;
  var AllowExpansion: Boolean);
var
  Session: TSSession;
  Database: TSDatabase;
  I: Integer;
  NewNode: TTreeNode;
  TreeView: TTreeView_Ext;
begin
  TreeView := TTreeView_Ext(Sender);

  if (not Assigned(TreeView.Selected)) then
    TreeView.Selected := Node;

  if (Assigned(Node)) then
  begin
    if (Assigned(Wanted.Node)) then
    begin
      GetSession(Wanted.Node).Connection.Terminate();
      Wanted.Node := nil;
    end;
    if (Node.HasChildren and not Assigned(Node.getFirstChild())) then
    begin
      case (Node.ImageIndex) of
        iiServer:
          begin
            Session := GetSession(Node);
            if (Assigned(Session)) then
              if (not Session.Databases.Update()) then
                Wanted.Node := Node
              else
              begin
                for I := 0 to Session.Databases.Count - 1 do
                  if (not (Session.Databases[I] is TSSystemDatabase)) then
                  begin
                    NewNode := TreeView.Items.AddChild(Node, Session.Databases[I].Name);
                    NewNode.ImageIndex := iiDatabase;
                    NewNode.Data := Session.Databases[I];
                    NewNode.HasChildren := TreeView = FSource;
                  end;
                Node.HasChildren := Assigned(Node.getFirstChild());
              end;
          end;
        iiDatabase:
          begin
            Session := TSSession(Node.Parent.Data);
            Database := Session.DatabaseByName(Node.Text);
            if (not Database.Update()) then
              Wanted.Node := Node
            else
            begin
              for I := 0 to Database.Tables.Count - 1 do
                if (Database.Tables[I] is TSBaseTable) then
                begin
                  NewNode := TreeView.Items.AddChild(Node, Database.Tables[I].Name);
                  NewNode.ImageIndex := iiBaseTable;
                  NewNode.Data := Database.Tables[I];
                end
                else if (Database.Tables[I] is TSView) then
                begin
                  NewNode := TreeView.Items.AddChild(Node, Database.Tables[I].Name);
                  NewNode.ImageIndex := iiView;
                  NewNode.Data := Database.Tables[I];
                end;
              if (Assigned(Database.Routines)) then
                for I := 0 to Database.Routines.Count - 1 do
                  if (Database.Routines[I] is TSProcedure) then
                  begin
                    NewNode := TreeView.Items.AddChild(Node, Database.Routines[I].Name);
                    NewNode.ImageIndex := iiProcedure;
                    NewNode.Data := Database.Routines[I];
                  end
                  else if (Database.Routines[I] is TSFunction) then
                  begin
                    NewNode := TreeView.Items.AddChild(Node, Database.Routines[I].Name);
                    NewNode.ImageIndex := iiFunction;
                    NewNode.Data := Database.Routines[I];
                  end;
              if (Assigned(Database.Events)) then
                for I := 0 to Database.Events.Count - 1 do
                begin
                  NewNode := TreeView.Items.AddChild(Node, Database.Events[I].Name);
                  NewNode.ImageIndex := iiEvent;
                  NewNode.Data := Database.Events[I];
                end;
            end;
          end;
      end;
    end;
  end;

  if (not Assigned(Wanted.Node)) then
    TreeView.Cursor := crDefault
  else
    TreeView.Cursor := crSQLWait;
end;

procedure TDTransfer.TreeViewGetSelectedIndex(Sender: TObject; Node: TTreeNode);
begin
  Node.SelectedIndex := Node.ImageIndex;
end;

procedure TDTransfer.TSExecuteShow(Sender: TObject);
var
  Answer: Integer;

  procedure AddDBObject(const SourceDBObject: TSDBObject; const DestinationSession: TSSession; const DestinationDatabaseName: string);
  var
    DestinationDatabase: TSDatabase;
  begin
    if (Answer <> IDYESALL) then
    begin
      DestinationDatabase := DestinationSession.DatabaseByName(DestinationDatabaseName);
      if (Assigned(DestinationDatabase)) then
        if ((SourceDBObject is TSTable) and Assigned(DestinationDatabase.TableByName(SourceDBObject.Name))) then
          Answer := MsgBox(Preferences.LoadStr(700, DestinationDatabase.Name + '.' + SourceDBObject.Name), Preferences.LoadStr(101), MB_YESYESTOALLNOCANCEL + MB_ICONQUESTION)
        else if ((SourceDBObject is TSProcedure) and Assigned(DestinationDatabase.ProcedureByName(SourceDBObject.Name))) then
          Answer := MsgBox(Preferences.LoadStr(777, DestinationDatabase.Name + '.' + SourceDBObject.Name), Preferences.LoadStr(101), MB_YESYESTOALLNOCANCEL + MB_ICONQUESTION)
        else if ((SourceDBObject is TSFunction) and Assigned(DestinationDatabase.FunctionByName(SourceDBObject.Name))) then
          Answer := MsgBox(Preferences.LoadStr(778, DestinationDatabase.Name + '.' + SourceDBObject.Name), Preferences.LoadStr(101), MB_YESYESTOALLNOCANCEL + MB_ICONQUESTION)
        else if ((SourceDBObject is TSEvent) and Assigned(DestinationDatabase.EventByName(SourceDBObject.Name))) then
          Answer := MsgBox(Preferences.LoadStr(920, DestinationDatabase.Name + '.' + SourceDBObject.Name), Preferences.LoadStr(101), MB_YESYESTOALLNOCANCEL + MB_ICONQUESTION);
    end;

    if (Answer in [IDYES, IDYESALL]) then
      Transfer.Add(SourceDBObject, DestinationDatabaseName)
    else if (Answer = IDCANCEL) then
      FreeAndNil(Transfer);
  end;

var
  Database: TSDatabase;
  DestinationSession: TSSession;
  I: Integer;
  J: Integer;
  List: TList;
  Node: TTreeNode;
  SourceDatabase: TSDatabase;
  SourceSession: TSSession;
begin
  FEntieredObjects.Caption := '';
  FDoneObjects.Caption := '';
  FEntieredRecords.Caption := '';
  FDoneRecords.Caption := '';
  FEntieredTime.Caption := '';
  FDoneTime.Caption := '';
  FProgressBar.Position := 0;
  FErrors.Caption := '0';
  FErrorMessages.Lines.Clear();

  Node := FSource.Selected;
  while (Assigned(Node.Parent)) do Node := Node.Parent;
  SourceSession := TSSession(Node.Data);
  Wanted.Page := nil;

  List := TList.Create();
  case (FSource.Selected.ImageIndex) of
    iiDatabase:
      begin
        for I := 0 to FSource.Selected.Parent.Count - 1 do
          if (FSource.Selected.Parent.Item[I].Selected) then
            List.Add(TSDatabase(FSource.Selected.Parent.Item[I].Data));
        if (not SourceSession.Update(List, FData.Checked)) then
          Wanted.Page := TSExecute
        else
        begin
          List.Clear();
          for I := 0 to FSource.Selected.Parent.Count - 1 do
            if (FSource.Selected.Parent.Item[I].Selected) then
            begin
              Database := SourceSession.DatabaseByName(FSource.Selected.Parent[I].Text);
              for J := 0 to Database.Tables.Count - 1 do
                List.Add(Database.Tables[J]);
              if (Assigned(Database.Routines)) then
                for J := 0 to Database.Routines.Count - 1 do
                  List.Add(Database.Routines[J]);
              if (Assigned(Database.Triggers)) then
                for J := 0 to Database.Triggers.Count - 1 do
                  List.Add(Database.Triggers[J]);
              if (Assigned(Database.Events)) then
                for J := 0 to Database.Events.Count - 1 do
                  List.Add(Database.Events[J]);
            end;
        end;
      end;
    iiBaseTable,
    iiView,
    iiProcedure,
    iiFunction,
    iiEvent:
      begin
        for I := 0 to FSource.Selected.Parent.Count - 1 do
          if (FSource.Selected.Parent.Item[I].Selected) then
            List.Add(FSource.Selected.Parent.Item[I].Data);
      end;
  end;
  if (not Assigned(Wanted.Page) and not SourceSession.Update(List, FData.Checked)) then
    Wanted.Page := TSExecute;
  List.Free();

  if (Assigned(Wanted.Page)) then
    DestinationSession := nil
  else
  begin
    Node := FDestination.Selected;
    while (Assigned(Node.Parent)) do Node := Node.Parent;
    DestinationSession := GetSession(Node);
    if (not DestinationSession.Databases.Update()) then
      Wanted.Page := TSExecute;
  end;

  if (not Assigned(Wanted.Page)) then
  begin
    List := TList.Create();

    case (FSource.Selected.ImageIndex) of
      iiDatabase:
        for I := 0 to FSource.Selected.Parent.Count - 1 do
          if (FSource.Selected.Parent[I].Selected) then
          begin
            Database := DestinationSession.DatabaseByName(FSource.Selected.Parent[I].Text);
            if (Assigned(Database)) then
              List.Add(Database);
          end;
      iiBaseTable,
      iiView:
        begin
          if (List.IndexOf(TSDatabase(FDestination.Selected.Data).Tables) < 0) then
            List.Add(TSDatabase(FDestination.Selected.Data).Tables);
          if ((FSource.Selected.ImageIndex = iiBaseTable)
            and Assigned(TSDatabase(FDestination.Selected.Data).Triggers)
            and (List.IndexOf(TSDatabase(FDestination.Selected.Data).Triggers) < 0)) then
            List.Add(TSDatabase(FDestination.Selected.Data).Triggers);
        end;
      iiProcedure,
      iiFunction:
        if (List.IndexOf(TSDatabase(FDestination.Selected.Data).Routines) < 0) then
          List.Add(TSDatabase(FDestination.Selected.Data).Routines);
      iiEvent:
        if (List.IndexOf(TSDatabase(FDestination.Selected.Data).Events) < 0) then
          List.Add(TSDatabase(FDestination.Selected.Data).Events);
    end;
    if (not DestinationSession.Update(List)) then
      Wanted.Page := TSExecute;
    List.Free();
  end;

  if (not Assigned(Wanted.Page)) then
  begin
    Answer := IDYES;

    if (Assigned(Transfer)) then
      TerminateThread(Transfer.Handle, 0);

    Transfer := TTTransfer.Create(SourceSession, DestinationSession);
    Transfer.Wnd := Self.Handle;
    Transfer.Data := FData.Checked;
    Transfer.Structure := FStructure.Checked;
    Transfer.OnError := OnError;
    Transfer.OnTerminate := OnTerminate;
    Transfer.OnUpdate := OnUpdate;

    for I := 0 to FSource.Selected.Parent.Count - 1 do
      if (FSource.Selected.Parent[I].Selected) then
        case (FSource.Selected.Parent[I].ImageIndex) of
          iiDatabase:
            begin
              SourceDatabase := SourceSession.DatabaseByName(FSource.Selected.Parent[I].Text);
              for J := 0 to SourceDatabase.Tables.Count - 1 do
                AddDBObject(SourceDatabase.Tables[J], DestinationSession, SourceDatabase.Name);
              for J := 0 to SourceDatabase.Routines.Count - 1 do
                AddDBObject(SourceDatabase.Routines[J], DestinationSession, SourceDatabase.Name);
              for J := 0 to SourceDatabase.Events.Count - 1 do
                AddDBObject(SourceDatabase.Events[J], DestinationSession, SourceDatabase.Name);
            end;
          iiBaseTable,
          iiView:
            AddDBObject(SourceSession.DatabaseByName(FSource.Selected.Parent.Text).TableByName(FSource.Selected.Parent[I].Text),
              DestinationSession, FDestination.Selected.Text);
          iiProcedure:
            AddDBObject(SourceSession.DatabaseByName(FSource.Selected.Parent.Text).ProcedureByName(FSource.Selected.Parent[I].Text),
              DestinationSession, FDestination.Selected.Text);
          iiFunction:
            AddDBObject(SourceSession.DatabaseByName(FSource.Selected.Parent.Text).FunctionByName(FSource.Selected.Parent[I].Text),
              DestinationSession, FDestination.Selected.Text);
          iiEvent:
            AddDBObject(SourceSession.DatabaseByName(FSource.Selected.Parent.Text).EventByName(FSource.Selected.Parent[I].Text),
              DestinationSession, FDestination.Selected.Text);
        end;

    if (not Assigned(Transfer)) then
      ModalResult := mrCancel
    else
    begin
      FSource.Items.BeginUpdate();
      FSource.Items.Clear();
      FSource.Items.EndUpdate();
      FDestination.Items.BeginUpdate();
      FDestination.Items.Clear();
      FDestination.Items.EndUpdate();

      TSWhat.Enabled := False;

      FBBack.Enabled := False;

      Transfer.Start();
    end;
  end;

  if (not Assigned(Wanted.Page)) then
    SetControlCursor(GProgress, crDefault)
  else
    SetControlCursor(GProgress, crSQLWait);

  FBForward.Enabled := False;
  FBForward.Default := False;
  FBCancel.Default := True;
  ActiveControl := FBCancel;
end;

procedure TDTransfer.TSSelectResize(Sender: TObject);
var
  Msg: TMsg;
begin
  if (not (PeekMessage(Msg, 0, 0, 0, PM_NOREMOVE) and (Msg.Message = WM_MOUSEMOVE) and (Msg.wParam = MK_LBUTTON))) then
  begin
    GSource.Width := TSSelect.Width div 2 - 3 * GSource.Left;
    GDestination.Width := GSource.Width;
    GDestination.Left := TSSelect.Width - GDestination.Width - 3 * GSource.Left;
  end;
end;

procedure TDTransfer.TSSelectShow(Sender: TObject);
var
  FSourceOnChange: TTVChangedEvent;
  FDestinationOnChange: TTVChangedEvent;
  I: Integer;
  J: Integer;
  Node: TTreeNode;
begin
  if (FSource.Items.Count = 0) then
  begin
    FSourceOnChange := FSource.OnChange;
    FSource.OnChange := nil;
    FDestinationOnChange := FDestination.OnChange;
    FDestination.OnChange := nil;

    for I := 0 to Accounts.Count - 1 do
    begin
      Node := FSource.Items.Add(nil, Accounts[I].Name);
      Node.ImageIndex := iiServer;
      Node.HasChildren := True;
      if (Assigned(SourceSession) and (Accounts[I] = SourceSession.Account)) then
      begin
        Node.Expand(False);
        if (not Assigned(Wanted.Node)) then
          for J := 0 to Node.Count - 1 do
            if (Node[J].Data = SourceDatabase) then
              Node[J].Expand(False);
      end;

      Node := FDestination.Items.Add(nil, Accounts[I].Name);
      Node.ImageIndex := iiServer;
      Node.HasChildren := True;
    end;
    TreeViewChange(Sender, nil);

    FSource.OnChange := FSourceOnChange;
    FDestination.OnChange := FDestinationOnChange;
  end;

  CheckActivePageChange(TSSelect.PageIndex);
end;

procedure TDTransfer.TSWhatShow(Sender: TObject);
var
  I: Integer;
  J: Integer;
  List: TList;
  Node: TTreeNode;
begin
  Wanted.Node := nil;
  Wanted.Page := nil;

  List := TList.Create();
  for I := 0 to FSource.SelectionCount - 1 do
    if ((TObject(FSource.Selections[I].Data) is TSDatabase)
      or (TObject(FSource.Selections[I].Data) is TSBaseTable)) then
      List.Add(FSource.Selections[I].Data);
  Node := FSource.Selected;
  while (Assigned(Node.Parent)) do Node := Node.Parent;
  if (not TSSession(Node.Data).Update(List)) then
    Wanted.Page := TSWhat;
  List.Free();

  if (Assigned(Wanted.Page)) then
  begin
    FData.Enabled := True;
    FData.Checked := Preferences.Transfer.Data;
  end
  else
  begin
    FData.Enabled := False;
    for I := 0 to FSource.SelectionCount - 1 do
      if (TObject(FSource.Selections[I].Data) is TSDatabase) then
      begin
        for J := 0 to TSDatabase(FSource.Selections[I].Data).Tables.Count - 1 do
          if (TSDatabase(FSource.Selections[I].Data).Tables[J] is TSBaseTable) then
            FData.Enabled := True;
      end
      else if (FSource.Selections[I].ImageIndex = iiBaseTable) then
        FData.Enabled := True;
  end;

  FData.Checked := FData.Checked and FData.Enabled;

  TSExecute.Enabled := not Assigned(Wanted.Page) and FStructure.Checked;
  CheckActivePageChange(TSWhat.PageIndex);
end;

procedure TDTransfer.UMChangePreferences(var Message: TMessage);
begin
  Preferences.Images.GetIcon(iiTransfer, Icon);

  Caption := Preferences.LoadStr(753);;

  GSource.Caption := Preferences.LoadStr(754);
  GDestination.Caption := Preferences.LoadStr(755);

  GWhat.Caption := Preferences.LoadStr(227);
  FLWhat.Caption := Preferences.LoadStr(218) + ':';
  FStructure.Caption := Preferences.LoadStr(215);
  FData.Caption := Preferences.LoadStr(216);

  GProgress.Caption := Preferences.LoadStr(224);
  FLEntiered.Caption := Preferences.LoadStr(211) + ':';
  FLDone.Caption := Preferences.LoadStr(232) + ':';
  FLProgressObjects.Caption := Preferences.LoadStr(234) + ':';
  FLProgressRecords.Caption := Preferences.LoadStr(235) + ':';
  FLProgressTime.Caption := Preferences.LoadStr(661) + ':';
  FLErrors.Caption := Preferences.LoadStr(391) + ':';

  GErrorMessages.Caption := Preferences.LoadStr(392);

  FBHelp.Caption := Preferences.LoadStr(167);
  FBBack.Caption := '< ' + Preferences.LoadStr(228);
end;

procedure TDTransfer.UMPostAfterExecuteSQL(var Message: TMessage);
var
  Node: TTreeNode;
begin
  if (Assigned(Wanted.Node)) then
  begin
    Node := Wanted.Node;
    Wanted.Node := nil;
    Node.Expand(False);
  end
  else if (Assigned(Wanted.Page) and Assigned(Wanted.Page.OnShow)) then
    Wanted.Page.OnShow(nil);
end;

procedure TDTransfer.UMTerminate(var Message: TMessage);
var
  Success: Boolean;
begin
  Success := Boolean(Message.WParam);

  if (Assigned(Transfer)) then
  begin
    Transfer.WaitFor();

    if (Success and (Transfer.WarningCount > 0)) then
      MsgBox(Preferences.LoadStr(932, IntToStr(Transfer.WarningCount)), Preferences.LoadStr(43), MB_OK + MB_ICONINFORMATION);

    FreeAndNil(Transfer);
  end;

  FBBack.Enabled := True;
  FBCancel.Caption := Preferences.LoadStr(231);
  if (Success) then
    FBCancel.ModalResult := mrOk
  else
    FBCancel.ModalResult := mrCancel;
end;

procedure TDTransfer.UMUpdateProgressInfo(var Message: TMessage);
var
  Infos: TTool.PProgressInfos;
begin
  Infos := TTool.PProgressInfos(Message.LParam);

  if (Infos^.ObjectsSum < 0) then
    FEntieredObjects.Caption := '???'
  else
    FEntieredObjects.Caption := FormatFloat('#,##0', Infos^.ObjectsSum, LocaleFormatSettings);
  if (Infos^.ObjectsDone < 0) then
    FDoneObjects.Caption := '???'
  else
    FDoneObjects.Caption := FormatFloat('#,##0', Infos^.ObjectsDone, LocaleFormatSettings);

  if (Infos^.RecordsSum < 0) then
    FEntieredRecords.Caption := '???'
  else
    FEntieredRecords.Caption := FormatFloat('#,##0', Infos^.RecordsSum, LocaleFormatSettings);
  if (Infos^.RecordsDone < 0) then
    FDoneRecords.Caption := '???'
  else
    FDoneRecords.Caption := FormatFloat('#,##0', Infos^.RecordsDone, LocaleFormatSettings);

  FEntieredTime.Caption := TimeToStr(Infos^.TimeSum, DurationFormatSettings);
  FDoneTime.Caption := TimeToStr(Infos^.TimeDone, DurationFormatSettings);

  FProgressBar.Position := Infos^.Progress;
end;

initialization
  FDTransfer := nil;
end.
