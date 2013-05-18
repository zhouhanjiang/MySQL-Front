unit fDTransfer;

interface {********************************************************************}

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms,
  Dialogs, ComCtrls, ExtCtrls, StdCtrls, DB, Menus,
  ComCtrls_Ext, Forms_Ext, ExtCtrls_Ext, StdCtrls_Ext,
  MySQLDB,
  fSession, fTools,
  fBase;

type
  TDTransfer = class(TForm_Ext)
    FBBack: TButton;
    FBCancel: TButton;
    FBForward: TButton;
    FBHelp: TButton;
    FData: TCheckBox;
    FDestination: TTreeView_Ext;
    FDoneRecords: TLabel;
    FDoneTables: TLabel;
    FDoneTime: TLabel;
    FEntieredRecords: TLabel;
    FEntieredTables: TLabel;
    FEntieredTime: TLabel;
    FErrorMessages: TRichEdit;
    FErrors: TLabel;
    FLDone: TLabel;
    FLEntiered: TLabel;
    FLErrors: TLabel;
    FLProgressRecords: TLabel;
    FLProgressTables: TLabel;
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
    miSelectAll: TMenuItem;
    MSource: TPopupMenu;
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
    procedure FormCreate(Sender: TObject);
    procedure FormHide(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure miSelectAllClick(Sender: TObject);
    procedure MSourcePopup(Sender: TObject);
    procedure PageControlResize(Sender: TObject);
    procedure TreeViewChange(Sender: TObject; Node: TTreeNode);
    procedure TreeViewExpanding(Sender: TObject; Node: TTreeNode;
      var AllowExpansion: Boolean);
    procedure TreeViewMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure TSExecuteShow(Sender: TObject);
    procedure TreeViewGetSelectedIndex(Sender: TObject; Node: TTreeNode);
    procedure FStructureClick(Sender: TObject);
    procedure FStructureKeyPress(Sender: TObject; var Key: Char);
    procedure FDataClick(Sender: TObject);
    procedure FDataKeyPress(Sender: TObject; var Key: Char);
    procedure TSWhatShow(Sender: TObject);
    procedure TSSelectShow(Sender: TObject);
  private
    Sessions: array of TSSession;
    MouseDownNode: TTreeNode;
    ProgressInfos: TTool.TProgressInfos;
    Transfer: TTTransfer;
    WantedExecute: Boolean;
    WantedNodeExpand: TTreeNode;
    procedure CheckActivePageChange(const ActivePageIndex: Integer);
    procedure FormSessionEvent(const Event: TSSession.TEvent);
    function GetSession(const Index: Integer): TSSession;
    procedure OnError(const Sender: TObject; const Error: TTool.TError; const Item: TTool.TItem; const ShowRetry: Boolean; var Success: TDataAction);
    procedure OnExecuted(const ASuccess: Boolean);
    procedure OnUpdate(const AProgressInfos: TTool.TProgressInfos);
    procedure CMChangePreferences(var Message: TMessage); message CM_CHANGEPREFERENCES;
    procedure CMExecutedDone(var Message: TMessage); message CM_EXECUTIONDONE;
    procedure CMUpdateProgressInfo(var Message: TMessage); message CM_UPDATEPROGRESSINFO;
  public
    SourceSession: TSSession;
    SourceDatabaseName: string;
    SourceTableName: string;
    DestinationSession: TSSession;
    DestinationDatabaseName: string;
    DestinationTableName: string;
    function Execute(): Boolean;
  end;

function DTransfer(): TDTransfer;

implementation {***************************************************************}

{$R *.dfm}

uses
  StrUtils, CommCtrl, RichEdit, Consts,
  SQLUtils,
  fPreferences,
  FDConnecting;

var
  FTransfer: TDTransfer;

function DTransfer(): TDTransfer;
begin
  if (not Assigned(FTransfer)) then
  begin
    Application.CreateForm(TDTransfer, FTransfer);
    FTransfer.Perform(CM_CHANGEPREFERENCES, 0, 0);
  end;

  Result := FTransfer;
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
  FBForward.Default := True;

  FBCancel.Caption := Preferences.LoadStr(30);
  FBCancel.Default := False;
end;

procedure TDTransfer.CMChangePreferences(var Message: TMessage);
begin
  miSelectAll.Caption := Preferences.LoadStr(572);

  Caption := ReplaceStr(Preferences.LoadStr(753), '&', '');;

  GSource.Caption := ReplaceStr(Preferences.LoadStr(754), '&', '');
  GDestination.Caption := ReplaceStr(Preferences.LoadStr(755), '&', '');

  GWhat.Caption := Preferences.LoadStr(227);
  FLWhat.Caption := Preferences.LoadStr(218) + ':';
  FStructure.Caption := Preferences.LoadStr(215);
  FData.Caption := Preferences.LoadStr(216);

  GProgress.Caption := Preferences.LoadStr(224);
  FLEntiered.Caption := Preferences.LoadStr(211) + ':';
  FLDone.Caption := Preferences.LoadStr(232) + ':';
  FLProgressTables.Caption := Preferences.LoadStr(234) + ':';
  FLProgressRecords.Caption := Preferences.LoadStr(235) + ':';
  FLProgressTime.Caption := ReplaceStr(Preferences.LoadStr(661), '&', '') + ':';
  FLErrors.Caption := Preferences.LoadStr(391) + ':';

  GErrorMessages.Caption := Preferences.LoadStr(392);

  FBHelp.Caption := Preferences.LoadStr(167);
  FBBack.Caption := '< ' + Preferences.LoadStr(228);
end;

procedure TDTransfer.CMExecutedDone(var Message: TMessage);
var
  Success: Boolean;
begin
  Success := Boolean(Message.WParam);

  FreeAndNil(Transfer);

  FSource.Items.BeginUpdate();
  FSource.Items.Clear();
  FSource.Items.EndUpdate();
  FDestination.Items.BeginUpdate();
  FDestination.Items.Clear();
  FDestination.Items.EndUpdate();

  if (Success) then
  begin
    FBCancel.Caption := Preferences.LoadStr(231);
    FBCancel.ModalResult := mrOk;
  end;
end;

procedure TDTransfer.CMUpdateProgressInfo(var Message: TMessage);
var
  Infos: TTool.PProgressInfos;
begin
  Infos := TTool.PProgressInfos(Message.LParam);

  if (Infos^.TablesSum < 0) then
    FEntieredTables.Caption := '???'
  else
    FEntieredTables.Caption := FormatFloat('#,##0', Infos^.TablesSum, LocaleFormatSettings);
  if (Infos^.TablesDone < 0) then
    FDoneTables.Caption := '???'
  else
    FDoneTables.Caption := FormatFloat('#,##0', Infos^.TablesDone, LocaleFormatSettings);

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

function TDTransfer.Execute(): Boolean;
begin
  ShowModal();
  Result := ModalResult = mrOk;
end;

procedure TDTransfer.FBBackClick(Sender: TObject);
var
  ActivePageIndex: Integer;
begin
  for ActivePageIndex := PageControl.ActivePageIndex - 1 downto 0 do
    if (PageControl.Pages[ActivePageIndex].Enabled) then
    begin
      PageControl.ActivePageIndex := ActivePageIndex;
      Exit;
    end;
end;

procedure TDTransfer.FBCancelClick(Sender: TObject);
begin
  if (Assigned(Transfer)) then
  begin
    Transfer.UserAbort.SetEvent();
    if (not Transfer.Suspended) then
      Transfer.WaitFor();
  end;
end;

procedure TDTransfer.FBForwardClick(Sender: TObject);
var
  ActivePageIndex: Integer;
begin
  for ActivePageIndex := PageControl.ActivePageIndex + 1 to PageControl.PageCount - 1 do
    if (PageControl.Pages[ActivePageIndex].Enabled) then
    begin
      PageControl.ActivePageIndex := ActivePageIndex;
      Exit;
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
  if (Event.EventType in [ceAfterExecuteSQL]) then
    if (Assigned(WantedNodeExpand)) then
      WantedNodeExpand.Expand(False)
    else if (WantedExecute) then
      TSExecuteShow(Event.Sender);
end;

procedure TDTransfer.FormCreate(Sender: TObject);
begin
  Constraints.MinWidth := Width;
  Constraints.MinHeight := Height;

  Transfer := nil;
  BorderStyle := bsSizeable;

  FSource.Images := Preferences.SmallImages;
  FDestination.Images := Preferences.SmallImages;

  SendMessage(FErrorMessages.Handle, EM_SETTEXTMODE, TM_PLAINTEXT, 0);
  SendMessage(FErrorMessages.Handle, EM_SETWORDBREAKPROC, 0, LPARAM(@EditWordBreakProc));

  FStructure.Checked := Preferences.Transfer.Structure;
  FData.Checked := Preferences.Transfer.Data;

  PageControl.ActivePage := nil; // Make sure, not ___OnShowPage will be executed
end;

procedure TDTransfer.FormHide(Sender: TObject);
var
  I: Integer;
begin
  for I := 0 to Length(Sessions) - 1 do
    if (Assigned(Sessions[I])) then
    begin
      Sessions[I].UnRegisterEventProc(FormSessionEvent);
      if (Assigned(Sessions[I]) and (Sessions[I] <> SourceSession) and (Sessions[I] <> DestinationSession)) then
        FreeAndNil(Sessions[I]);
    end;
  SetLength(Sessions, 0);

  Preferences.Transfer.Height := Height;
  Preferences.Transfer.Width := Width;
  Preferences.Transfer.Left := Left;
  Preferences.Transfer.Top := Top;

  if (ModalResult = mrOK) then
  begin
    Preferences.Transfer.Data := FData.Checked;
    Preferences.Transfer.Structure := FStructure.Checked;
  end;

  PageControl.ActivePage := nil; // Make sure, not ___OnShowPage will be executed
end;

procedure TDTransfer.FormShow(Sender: TObject);
var
  I: Integer;
begin
  HelpContext := 1094;

  if ((Preferences.Transfer.Width > 0) and (Preferences.Transfer.Height > 0)) then
  begin
    Width := Preferences.Transfer.Width;
    Height := Preferences.Transfer.Height;
  end
  else
  begin
    Width := Constraints.MinWidth;
    Height := Constraints.MinHeight;
  end;
  if ((Preferences.Transfer.Left > 0) and (Preferences.Transfer.Top > 0)) then
  begin
    Left := Preferences.Transfer.Left;
    Top := Preferences.Transfer.Top;
  end;

  WantedExecute := False;
  WantedNodeExpand := nil;

  SetLength(Sessions, Accounts.Count);
  for I := 0 to Accounts.Count - 1 do
  begin
    if (Assigned(SourceSession) and (Accounts[I] = SourceSession.Account)) then
      Sessions[I] := SourceSession
    else if (Assigned(DestinationSession) and (Accounts[I] = DestinationSession.Account)) then
      Sessions[I] := DestinationSession
    else
      Sessions[I] := nil;
  end;

  TSSelect.Enabled := True;
  TSWhat.Enabled := False;
  TSExecute.Enabled := False;

  for I := 0 to PageControl.PageCount - 1 do
    if ((PageControl.ActivePageIndex < 0) and PageControl.Pages[I].Enabled) then
      PageControl.ActivePageIndex := I;
  CheckActivePageChange(PageControl.ActivePageIndex);

  if (Assigned(SourceSession)) then
    ActiveControl := FSource
  else
    ActiveControl := FBCancel;

  FBCancel.Caption := Preferences.LoadStr(30);
  FBCancel.ModalResult := mrCancel;
  FBCancel.Default := False;
end;

procedure TDTransfer.FStructureClick(Sender: TObject);
begin
  FData.Checked := FStructure.Checked;

  TSExecute.Enabled := FStructure.Checked;
  CheckActivePageChange(TSWhat.PageIndex);
end;

procedure TDTransfer.FStructureKeyPress(Sender: TObject; var Key: Char);
begin
  FStructureClick(Sender);
end;

function TDTransfer.GetSession(const Index: Integer): TSSession;
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

procedure TDTransfer.miSelectAllClick(Sender: TObject);
var
  I: Integer;
  Nodes: TList;
begin
  Nodes := TList.Create();
  if (Assigned(MouseDownNode)) then
    for I := 0 to MouseDownNode.Count - 1 do
      Nodes.Add(MouseDownNode.Item[I]);
  FSource.Select(Nodes);
  Nodes.Free();
end;

procedure TDTransfer.MSourcePopup(Sender: TObject);
begin
  miSelectAll.Enabled := FSource.MultiSelect and Assigned(MouseDownNode) and (MouseDownNode.ImageIndex <> iiBaseTable);

  ShowEnabledItems(MSource.Items);
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
        Msg := Preferences.LoadStr(165, IntToStr(Error.Session.ErrorCode), Error.Session.ErrorMessage);
        ErrorMsg := SQLUnwrapStmt(Error.Session.ErrorMessage);
        if (Error.Session.ErrorCode > 0) then
          ErrorMsg := ErrorMsg + ' (#' + IntToStr(Error.Session.ErrorCode) + ')';
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
        raise ERangeError.CreateFmt(SPropertyOutOfRange, ['Sender | Error.Session'])
    else
      Msg := Error.ErrorMessage;
  end;

  if (not ShowRetry) then
    Flags := MB_OK + MB_ICONERROR
  else
    Flags := MB_CANCELTRYCONTINUE + MB_ICONERROR;
  case (MsgBox(Msg, Preferences.LoadStr(45), Flags, Handle)) of
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

procedure TDTransfer.OnExecuted(const ASuccess: Boolean);
begin
  PostMessage(Handle, CM_EXECUTIONDONE, WPARAM(ASuccess), 0);
end;

procedure TDTransfer.OnUpdate(const AProgressInfos: TTool.TProgressInfos);
begin
  MoveMemory(@ProgressInfos, @AProgressInfos, SizeOf(AProgressInfos));

  PostMessage(Handle, CM_UPDATEPROGRESSINFO, 0, LPARAM(@ProgressInfos));
end;

procedure TDTransfer.PageControlResize(Sender: TObject);
begin
  GSource.Width := PageControl.Width div 2 - 3 * GSource.Left;
  GDestination.Width := GSource.Width;
  GDestination.Left := PageControl.Width - GDestination.Width - 3 * GSource.Left;
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
                WantedNodeExpand := Node
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
            Session := GetSession(Node.Parent.Index);
            Database := Session.DatabaseByName(Node.Text);
            if (not Database.Tables.Update() or not Session.Update(Database.Tables)) then
              WantedNodeExpand := Node
            else
            begin
              for I := 0 to Database.Tables.Count - 1 do
                if ((Database.Tables[I] is TSBaseTable) and Assigned(TSBaseTable(Database.Tables[I]).Engine) and not TSBaseTable(Database.Tables[I]).Engine.IsMerge and (RightStr(Database.Tables[I].Name, Length(BackupExtension)) <> BackupExtension)) then
                begin
                  NewNode := TreeView.Items.AddChild(Node, Database.Tables[I].Name);
                  NewNode.ImageIndex := iiBaseTable;
                  NewNode.Data := Database.Tables[I];
                end;
            end;
          end;
      end;
    end;
end;

procedure TDTransfer.TreeViewGetSelectedIndex(Sender: TObject; Node: TTreeNode);
begin
  Node.SelectedIndex := Node.ImageIndex;
end;

procedure TDTransfer.TreeViewMouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  if (Sender is TTreeView_Ext) then
    MouseDownNode := TTreeView_Ext(Sender).GetNodeAt(X, Y)
  else
    MouseDownNode := nil;
end;

procedure TDTransfer.TSExecuteShow(Sender: TObject);
var
  Answer: Integer;

  procedure AddTable(const SourceTable: TSTable; const DestinationSession: TSSession; const DestinationDatabaseName: string);
  var
    DestinationDatabase: TSDatabase;
    DestinationTableName: string;
  begin
    if (Answer <> IDYESALL) then
    begin
      DestinationDatabase := DestinationSession.DatabaseByName(DestinationDatabaseName);
      DestinationTableName := DestinationSession.TableName(SourceTable.Name);
      if (Assigned(DestinationDatabase)) then
      begin
        DestinationSession.BeginSynchron();
        if (DestinationDatabase.Update() and Assigned(DestinationDatabase.TableByName(DestinationTableName))) then
          Answer := MsgBox(Preferences.LoadStr(700, DestinationDatabase.Name + '.' + DestinationTableName), Preferences.LoadStr(101), MB_YESYESTOALLNOCANCEL + MB_ICONQUESTION);
        DestinationSession.EndSynchron();
      end;
    end;

    if (Answer in [IDYES, IDYESALL]) then
      Transfer.Add(SourceTable, DestinationDatabaseName)
    else if (Answer = IDCANCEL) then
      FreeAndNil(Transfer);
  end;

  function InitializeNode(const Session: TSSession; const Node: TTreeNode): Boolean;
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
          Result := not Session.Update();
          if (not Result) then
            for I := 0 to Session.Databases.Count - 1 do
              if (not Result and not (Session.Databases[I] is TSSystemDatabase)) then
              begin
                Database := Session.Databases[I];
                Result := not Database.Tables.Update();
                if (not Result and (Node.TreeView = FSource)) then
                begin
                  for J := 0 to Database.Tables.Count - 1 do
                    if (Database.Tables[J] is TSBaseTable) then
                      Objects.Add(Database.Tables[J]);
                  Result := not Session.Update(Objects);
                end;
              end;
        end;
      iiDatabase:
        begin
          Database := Session.DatabaseByName(Node.Text);
          Result := not Database.Tables.Update();
          if (not Result and (Node.TreeView = FSource)) then
          begin
            for J := 0 to Database.Tables.Count - 1 do
              if (Database.Tables[J] is TSBaseTable) then
                Objects.Add(Database.Tables[J]);
            Result := not Session.Update(Objects);
          end;
        end;
      iiBaseTable:
        begin
          for J := 0 to Node.Parent.Count - 1 do
            if (Node.Parent.Item[J].Selected) then
              Objects.Add(Node.Parent.Item[J].Data);
          Result := not Session.Update(Objects);
        end;
      else
        Result := False;
    end;
    Objects.Free();
  end;

var
  I: Integer;
  J: Integer;
  Database: TSDatabase;
  SourceSession: TSSession;
  Node: TTreeNode;
  ProgressInfos: TTool.TProgressInfos;
  DestinationSession: TSSession;
begin
  FErrors.Caption := '0';
  FErrorMessages.Lines.Clear();

  FBForward.Enabled := False;
  FBCancel.Default := True;
  ActiveControl := FBCancel;

  Node := FSource.Selected;
  while (Assigned(Node.Parent)) do Node := Node.Parent;
  SourceSession := GetSession(Node.Index);
  WantedExecute := InitializeNode(SourceSession, FSource.Selected);

  if (WantedExecute) then
    DestinationSession := nil
  else
  begin
    Node := FDestination.Selected;
    while (Assigned(Node.Parent)) do Node := Node.Parent;
    DestinationSession := GetSession(Node.Index);
    WantedExecute := not Assigned(DestinationSession) or InitializeNode(DestinationSession, FDestination.Selected);
  end;

  if (not WantedExecute) then
  begin
    Answer := IDYES;

    Transfer := TTTransfer.Create(SourceSession, DestinationSession);
    Transfer.Wnd := Self.Handle;
    Transfer.Data := FData.Checked;
    Transfer.Structure := FStructure.Checked;
    Transfer.OnError := OnError;
    Transfer.OnExecuted := OnExecuted;
    Transfer.OnUpdate := OnUpdate;

    for I := 0 to FSource.Selected.Parent.Count - 1 do
      if (FSource.Selected.Parent[I].Selected) then
        case (FSource.Selected.Parent[I].ImageIndex) of
          iiDatabase:
            begin
              Database := SourceSession.DatabaseByName(FSource.Selected.Parent[I].Text);
              for J := 0 to Database.Tables.Count - 1 do
                if (Assigned(Transfer) and (Database.Tables[J] is TSBaseTable) and Assigned(TSBaseTable(Database.Tables[J]).Engine) and not TSBaseTable(Database.Tables[J]).Engine.IsMerge and (RightStr(Database.Tables[J].Name, Length(BackupExtension)) <> BackupExtension)) then
                  AddTable(Database.Tables[J], DestinationSession, Database.Name);
            end;
          iiBaseTable:
            AddTable(SourceSession.DatabaseByName(FSource.Selected.Parent.Text).TableByName(FSource.Selected.Parent[I].Text),
              DestinationSession, FDestination.Selected.Text);
        end;

    if (Assigned(Transfer)) then
      Transfer.Start();
  end;
end;

procedure TDTransfer.TSSelectShow(Sender: TObject);
var
  DatabaseNames: TStringList;
  DatabaseNode: TTreeNode;
  FSourceOnChange: TTVChangedEvent;
  FDestinationOnChange: TTVChangedEvent;
  I: Integer;
  Node: TTreeNode;
  SelectedNodes: TList;
  AccountNode: TTreeNode;
  TableNames: TStringList;
  TableNode: TTreeNode;
begin
  FSourceOnChange := FSource.OnChange;
  FSource.OnChange := nil;
  FDestinationOnChange := FDestination.OnChange;
  FDestination.OnChange := nil;

  FSource.Items.BeginUpdate();
  FSource.Items.Clear();
  FSource.Items.EndUpdate();
  FDestination.Items.BeginUpdate();
  FDestination.Items.Clear();
  FDestination.Items.EndUpdate();

  for I := 0 to Accounts.Count - 1 do
  begin
    Node := FSource.Items.Add(nil, Accounts[I].Name);
    Node.ImageIndex := iiServer;
    Node.HasChildren := True;

    Node := FDestination.Items.Add(nil, Accounts[I].Name);
    Node.ImageIndex := iiServer;
    Node.HasChildren := True;
  end;

  if (Assigned(SourceSession)) then
  begin
    SourceSession.BeginSynchron();

    SelectedNodes := TList.Create();
    DatabaseNames := TStringList.Create();
    TableNames := TStringList.Create();

    DatabaseNames.Text := ReplaceStr(SourceDatabaseName, ',', #13#10);
    TableNames.Text := ReplaceStr(SourceTableName, ',', #13#10);

    AccountNode := FSource.TopItem;
    while (Assigned(AccountNode)) do
    begin
      if (AccountNode.Text = SourceSession.Account.Name) then
      begin
        if (DatabaseNames.Count = 0) then
          AccountNode.Selected := True
        else
        begin
          AccountNode.Expand(False);
          DatabaseNode := AccountNode.getFirstChild();
          while (Assigned(DatabaseNode)) do
          begin
            if (TableNames.Count = 0) then
            begin
              if (DatabaseNames.IndexOf(DatabaseNode.Text) >= 0) then
                SelectedNodes.Add(DatabaseNode);
            end
            else if (DatabaseNames.IndexOf(DatabaseNode.Text) >= 0) then
            begin
              DatabaseNode.Expand(False);
              TableNode := DatabaseNode.getFirstChild();
              while (Assigned(TableNode)) do
              begin
                if (TableNames.IndexOf(TableNode.Text) >= 0) then
                  SelectedNodes.Add(TableNode);
                TableNode := TableNode.getNextSibling();
              end;
            end;
            DatabaseNode := DatabaseNode.getNextSibling();
          end;
        end;
        break;
      end;
      AccountNode := AccountNode.getNextSibling();
    end;
    if (SelectedNodes.Count > 0) then
      FSource.Select(SelectedNodes)
    else if (Assigned(AccountNode)) then
      AccountNode.Selected := True;

    DatabaseNames.Free();
    TableNames.Free();
    SourceSession.EndSynchron();

    SelectedNodes.Clear();

    if (Assigned(DestinationSession)) then
    begin
      DestinationSession.BeginSynchron();

      AccountNode := FDestination.Items[0];
      while (Assigned(AccountNode)) do
      begin
        if (AccountNode.Text = DestinationSession.Account.Name) then
        begin
          AccountNode.Selected := True;
          if (DestinationDatabaseName <> '') then
          begin
            DestinationSession.Databases.Update();

            AccountNode.Expand(False);
            DatabaseNode := AccountNode.getFirstChild();
            while (Assigned(DatabaseNode)) do
            begin
              if (DatabaseNode.Text = DestinationDatabaseName) then
              begin
                if (DestinationTableName = '') then
                  SelectedNodes.Add(DatabaseNode)
                else
                begin
                  DatabaseNode.Expand(False);
                  TableNode := DatabaseNode.getFirstChild();
                  while (Assigned(TableNode)) do
                  begin
                    if (TableNode.Text = DestinationTableName) then
                      SelectedNodes.Add(TableNode);
                    TableNode := DatabaseNode.getNextChild(TableNode);
                  end;
                end;
              end;
              DatabaseNode := DatabaseNode.getNextSibling();
            end;
          end;
        end;
        AccountNode := AccountNode.getNextSibling();
      end;
      if (SelectedNodes.Count > 0) then
        FDestination.Select(SelectedNodes)
      else if (Assigned(AccountNode)) then
        AccountNode.Selected := True;

      DestinationSession.EndSynchron();
    end;

    SelectedNodes.Free();

    if (Assigned(FDestination.Selected) and FDestination.AutoExpand) then
      FDestination.Selected.Expand(False);
  end;

  TreeViewChange(Sender, nil);

  FSource.OnChange := FSourceOnChange;
  FDestination.OnChange := FDestinationOnChange;

  CheckActivePageChange(TSSelect.PageIndex);
end;

procedure TDTransfer.TSWhatShow(Sender: TObject);
begin
  TSExecute.Enabled := FStructure.Checked or FData.Checked;
  CheckActivePageChange(TSWhat.PageIndex);
end;

initialization
  FTransfer := nil;
end.
