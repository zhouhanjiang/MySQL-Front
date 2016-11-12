unit fDRoutine;

interface {********************************************************************}

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Menus,
  Dialogs, StdCtrls, ComCtrls, ExtCtrls,
  Forms_Ext, StdCtrls_Ext,
  SynEdit, SynMemo,
  fSession,
  fBase;

type
  TDRoutine = class(TForm_Ext)
    FBCancel: TButton;
    FBHelp: TButton;
    FBOk: TButton;
    FComment: TEdit;
    FCreated: TLabel;
    FDefiner: TLabel;
    FLComment: TLabel;
    FLCreated: TLabel;
    FLDefiner: TLabel;
    FLName: TLabel;
    FLSecurity: TLabel;
    FLSize: TLabel;
    FLUpdated: TLabel;
    FName: TEdit;
    FSecurityDefiner: TRadioButton;
    FSecurityInvoker: TRadioButton;
    FSize: TLabel;
    FSource: TSynMemo;
    FReferenced: TListView;
    FUpdated: TLabel;
    GBasics: TGroupBox_Ext;
    GDates: TGroupBox_Ext;
    GDefiner: TGroupBox_Ext;
    GSize: TGroupBox_Ext;
    msCopy: TMenuItem;
    msCut: TMenuItem;
    msDelete: TMenuItem;
    MSource: TPopupMenu;
    msPaste: TMenuItem;
    msSelectAll: TMenuItem;
    msUndo: TMenuItem;
    N1: TMenuItem;
    N2: TMenuItem;
    PageControl: TPageControl;
    PSQLWait: TPanel;
    TSBasics: TTabSheet;
    TSInformation: TTabSheet;
    TSSource: TTabSheet;
    TSReferenced: TTabSheet;
    procedure FBHelpClick(Sender: TObject);
    procedure FBOkCheckEnabled(Sender: TObject);
    procedure FCommentChange(Sender: TObject);
    procedure FNameChange(Sender: TObject);
    procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);
    procedure FormCreate(Sender: TObject);
    procedure FormHide(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure FSecurityClick(Sender: TObject);
    procedure FSecurityKeyPress(Sender: TObject; var Key: Char);
    procedure FSourceChange(Sender: TObject);
    procedure TSReferencedShow(Sender: TObject);
  private
    procedure Built();
    procedure FormSessionEvent(const Event: TSSession.TEvent);
    procedure FReferencedBuild();
    procedure UMChangePreferences(var Message: TMessage); message UM_CHANGEPREFERENCES;
  public
    Database: TSDatabase;
    Routine: TSRoutine;
    RoutineType: TSRoutine.TRoutineType;
    function Execute(): Boolean;
  end;

function DRoutine(): TDRoutine;

implementation {***************************************************************}

{$R *.dfm}

uses
  StrUtils, SysConst,
  SQLUtils,
  fPreferences;

var
  FRoutine: TDRoutine;

function DRoutine(): TDRoutine;
begin
  if (not Assigned(FRoutine)) then
  begin
    Application.CreateForm(TDRoutine, FRoutine);
    FRoutine.Perform(UM_CHANGEPREFERENCES, 0, 0);
  end;

  Result := FRoutine;
end;

{ TDRoutine *******************************************************************}

procedure TDRoutine.Built();
begin
  FName.Text := Routine.Name;

  case (Routine.Security) of
    seDefiner: FSecurityDefiner.Checked := True;
    seInvoker: FSecurityInvoker.Checked := True;
  end;
  FComment.Text := SQLUnwrapStmt(Routine.Comment, Database.Session.Connection.MySQLVersion);

  if (Double(Routine.Created) = 0) then FCreated.Caption := '???' else FCreated.Caption := SysUtils.DateTimeToStr(Routine.Created, LocaleFormatSettings);
  if (Double(Routine.Modified) = 0) then FUpdated.Caption := '???' else FUpdated.Caption := SysUtils.DateTimeToStr(Routine.Modified, LocaleFormatSettings);
  FDefiner.Caption := Routine.Definer;

  FSize.Caption := FormatFloat('#,##0', Length(Routine.Source), LocaleFormatSettings);

  FSource.Text := Routine.Source;

  TSSource.TabVisible := Routine.Source <> '';

  FName.Enabled := False; FLName.Enabled := FName.Enabled;
end;

function TDRoutine.Execute(): Boolean;
begin
  ShowModal();
  Result := ModalResult = mrOk;
end;

procedure TDRoutine.FBHelpClick(Sender: TObject);
begin
  Application.HelpContext(HelpContext);
end;

procedure TDRoutine.FBOkCheckEnabled(Sender: TObject);
var
  DDLStmt: TSQLDDLStmt;
  SQL: string;
begin
  SQL := Trim(FSource.Text);

  FBOk.Enabled := (not Assigned(Routine) or Assigned(Routine) and (Routine.Source <> ''))
    and (not TSBasics.Visible or not Assigned(Routine) or (FName.Text <> '') and ((lstrcmpi(PChar(FName.Text), PChar(Routine.Name)) = 0) or ((Routine.RoutineType = rtProcedure) and not Assigned(Database.ProcedureByName(FName.Text)) or ((Routine.RoutineType = rtFunction) and not Assigned(Database.FunctionByName(FName.Text))))))
    and (not TSSource.Visible or SQLSingleStmt(FSource.Text) and SQLParseDDLStmt(DDLStmt, PChar(FSource.Text), Length(FSource.Text), Database.Session.Connection.MySQLVersion) and (DDLStmt.DefinitionType = dtCreate) and (DDLStmt.ObjectType in [otProcedure, otFunction]) and ((DDLStmt.DatabaseName = '') or (Database.Session.DatabaseByName(DDLStmt.DatabaseName) = Database)));
end;

procedure TDRoutine.FCommentChange(Sender: TObject);
begin
  TSSource.TabVisible := False;

  FBOkCheckEnabled(Sender);
end;

procedure TDRoutine.FNameChange(Sender: TObject);
begin
  TSSource.TabVisible := False;

  FBOkCheckEnabled(Sender);
end;

procedure TDRoutine.FormCloseQuery(Sender: TObject; var CanClose: Boolean);
var
  NewRoutine: TSRoutine;
begin
  if ((ModalResult = mrOk) and PageControl.Visible) then
  begin
    if (not Assigned(Routine)) then
    begin
      if (RoutineType = rtProcedure) then
        NewRoutine := TSProcedure.Create(Database.Routines)
      else
        NewRoutine := TSFunction.Create(Database.Routines);
      NewRoutine.Source := Trim(FSource.Text);

      CanClose := Database.AddRoutine(NewRoutine);

      NewRoutine.Free();
    end
    else if (TSSource.Visible) then
    begin
      if (RoutineType = rtProcedure) then
        NewRoutine := TSProcedure.Create(Database.Routines)
      else
        NewRoutine := TSFunction.Create(Database.Routines);
      NewRoutine.Source := Trim(FSource.Text);

      CanClose := Database.UpdateRoutine(Routine, NewRoutine);

      NewRoutine.Free();
    end
    else
    begin
      NewRoutine := TSRoutine.Create(Database.Routines);
      if (Assigned(Routine)) then
        NewRoutine.Assign(Routine);

      NewRoutine.Name := Trim(FName.Text);
      if (FSecurityDefiner.Checked) then
        NewRoutine.Security := seDefiner
      else if (FSecurityInvoker.Checked) then
        NewRoutine.Security := seInvoker;
      if (not Assigned(Routine) or (Trim(FComment.Text) <> SQLUnwrapStmt(Routine.Comment, Database.Session.Connection.MySQLVersion))) then
        NewRoutine.Comment := Trim(FComment.Text);

      CanClose := Database.UpdateRoutine(Routine, NewRoutine);

      NewRoutine.Free();
    end;

// UpdateRoutine uses ExecuteSQL (not SendSQL). Because of this,
// FormSessionEvent will be called inside UpdateRoutine - and this code
// whould hide PageControl permanentely
//    if (not CanClose) then
//    begin
//      PageControl.Visible := CanClose;
//      PSQLWait.Visible := not PageControl.Visible;
//    end;

    FBOk.Enabled := False;
  end;
end;

procedure TDRoutine.FormCreate(Sender: TObject);
begin
  FReferenced.SmallImages := Preferences.Images;
  FSource.Highlighter := MainHighlighter;

  Constraints.MinWidth := Width;
  Constraints.MinHeight := Height;

  BorderStyle := bsSizeable;

  FReferenced.RowSelect := CheckWin32Version(6);

  PageControl.ActivePage := TSBasics;
end;

procedure TDRoutine.FormHide(Sender: TObject);
begin
  Database.Session.ReleaseEventProc(FormSessionEvent);

  Preferences.Routine.Width := Width;
  Preferences.Routine.Height := Height;

  FReferenced.Items.BeginUpdate();
  FReferenced.Items.Clear();
  FReferenced.Items.EndUpdate();

  PageControl.ActivePage := TSBasics;
end;

procedure TDRoutine.FormSessionEvent(const Event: TSSession.TEvent);
begin
  if ((Event.EventType = etItemValid) and (Event.Item = Routine)) then
    Built()
  else if ((Event.EventType in [etItemCreated, etItemAltered]) and (Event.Item is TSRoutine)) then
    ModalResult := mrOk;

  if (Event.EventType = etAfterExecuteSQL) then
  begin
    if (FReferenced.Cursor = crSQLWait) then
    begin
      FReferencedBuild();
      FReferenced.Cursor := crDefault;
    end;

    if (not PageControl.Visible) then
    begin
      PageControl.Visible := True;
      PSQLWait.Visible := not PageControl.Visible;
      ActiveControl := FComment;
      FBOkCheckEnabled(nil);
    end;
  end;
end;

procedure TDRoutine.FormShow(Sender: TObject);
var
  I: Integer;
  RoutineName: string;
  SQL: string;
begin
  Database.Session.RegisterEventProc(FormSessionEvent);

  if ((Preferences.Routine.Width >= Width) and (Preferences.Routine.Height >= Height)) then
  begin
    Width := Preferences.Routine.Width;
    Height := Preferences.Routine.Height;
  end;

  if (not Assigned(Routine)) then
  begin
    Caption := Preferences.LoadStr(775);
    Preferences.Images.GetIcon(iiProcedure, Icon);
    HelpContext := 1097;
  end
  else if (Routine.RoutineType = rtProcedure) then
  begin
    Caption := Preferences.LoadStr(842, Routine.Name);
    Preferences.Images.GetIcon(iiProcedure, Icon);
    HelpContext := 1099;
  end
  else if (Routine.RoutineType = rtFunction) then
  begin
    Caption := Preferences.LoadStr(842, Routine.Name);
    Preferences.Images.GetIcon(iiFunction, Icon);
    HelpContext := 1099;
  end;

  FName.Enabled := False; FLName.Enabled := FName.Enabled;
  FComment.Enabled := True; FLComment.Enabled := FComment.Enabled;

  if (not Assigned(Routine)) then
  begin
    if (RoutineType = rtProcedure) then
    begin
      RoutineName := Preferences.LoadStr(863);
      I := 2;
      while (Assigned(Database.ProcedureByName(RoutineName))) do
      begin
        RoutineName := Preferences.LoadStr(863) + IntToStr(I);
        Inc(I);
      end;

      SQL := 'CREATE PROCEDURE ' + Database.Session.Connection.EscapeIdentifier(RoutineName) + '(Param int(11))' + #13#10
        + 'BEGIN' + #13#10
        + 'END;' + #13#10;
      FSource.Text := SQL;
    end
    else if (RoutineType = rtFunction) then
    begin
      RoutineName := Preferences.LoadStr(864);
      I := 2;
      while (Assigned(Database.FunctionByName(RoutineName))) do
      begin
        RoutineName := Preferences.LoadStr(864) + IntToStr(I);
        Inc(I);
      end;

      SQL := 'CREATE FUNCTION ' + Database.Session.Connection.EscapeIdentifier(RoutineName) + '(Param int(11)) RETURNS int(11)' + #13#10
        + 'BEGIN' + #13#10
        + '  RETURN Param;' + #13#10
        + 'END;' + #13#10;
      FSource.Text := SQL;
    end
    else
      FSource.Lines.Clear();

    TSSource.TabVisible := True;

    PageControl.Visible := True;
    PSQLWait.Visible := not PageControl.Visible;
  end
  else
  begin
    PageControl.Visible := Routine.Update();
    PSQLWait.Visible := not PageControl.Visible;

    if (PageControl.Visible) then
      Built();
  end;

  FReferenced.Cursor := crDefault;

  TSInformation.TabVisible := Assigned(Routine);
  TSReferenced.TabVisible := Assigned(Routine);

  FBOk.Enabled := PageControl.Visible and not Assigned(Routine);

  ActiveControl := FBCancel;
  if (PageControl.Visible) then
    if (TSBasics.TabVisible) then
    begin
      PageControl.ActivePage := TSBasics;
      ActiveControl := FComment;
    end
    else if (TSSource.TabVisible) then
    begin
      PageControl.ActivePage := TSSource;
      ActiveControl := FSource;
    end;
end;

procedure TDRoutine.FReferencedBuild();

  procedure AddDBObject(const DBObject: TSDBObject);
  var
    I: Integer;
    Item: TListItem;
  begin
    for I := 0 to DBObject.References.Count - 1 do
      if (DBObject.References[I].DBObject = Routine) then
      begin
        Item := FReferenced.Items.Add();

        if (DBObject is TSView) then
        begin
          Item.ImageIndex := iiView;
          Item.Caption := DBObject.Caption;
          Item.SubItems.Add(Preferences.LoadStr(738));
        end
        else if (DBObject is TSProcedure) then
        begin
          Item.ImageIndex := iiProcedure;
          Item.Caption := DBObject.Caption;
          Item.SubItems.Add(Preferences.LoadStr(768));
        end
        else if (DBObject is TSFunction) then
        begin
          Item.ImageIndex := iiFunction;
          Item.Caption := DBObject.Caption;
          Item.SubItems.Add(Preferences.LoadStr(769));
        end
        else if (DBObject is TSTrigger) then
        begin
          Item.ImageIndex := iiTrigger;
          Item.Caption := DBObject.Caption;
          Item.SubItems.Add(Preferences.LoadStr(923, TSTrigger(DBObject).TableName));
        end
        else if (DBObject is TSEvent) then
        begin
          Item.ImageIndex := iiEvent;
          Item.Caption := DBObject.Caption;
          Item.SubItems.Add(Preferences.LoadStr(812));
        end
        else
          raise ERangeError.Create(SRangeError);
        Item.Data := DBObject;
      end;
  end;

var
  I: Integer;
begin
  FReferenced.Items.BeginUpdate();
  FReferenced.Items.Clear();

  for I := 0 to Database.Tables.Count - 1 do
    if (Database.Tables[I] is TSView) then
      AddDBObject(Database.Tables[I]);

  if (Assigned(Database.Routines)) then
    for I := 0 to Database.Routines.Count - 1 do
      if (Database.Routines[I] <> Routine) then
        AddDBObject(Database.Routines[I]);

  if (Assigned(Database.Triggers)) then
    for I := 0 to Database.Triggers.Count - 1 do
      AddDBObject(Database.Triggers[I]);

  if (Assigned(Database.Events)) then
    for I := 0 to Database.Events.Count - 1 do
      AddDBObject(Database.Events[I]);

  FReferenced.Items.EndUpdate();
end;

procedure TDRoutine.FSecurityClick(Sender: TObject);
begin
  TSSource.TabVisible := False;

  FBOkCheckEnabled(Sender);
end;

procedure TDRoutine.FSecurityKeyPress(Sender: TObject;
  var Key: Char);
begin
  FBOkCheckEnabled(Sender);
end;

procedure TDRoutine.FSourceChange(Sender: TObject);
begin
  MainAction('aECopyToFile').Enabled := FSource.SelText <> '';

  FName.Enabled := False; FLName.Enabled := FName.Enabled;
  FComment.Enabled := False; FLComment.Enabled := FComment.Enabled;

  TSBasics.TabVisible := False;
  TSInformation.TabVisible := False;

  FBOkCheckEnabled(Sender);
end;

procedure TDRoutine.TSReferencedShow(Sender: TObject);
var
  List: TList;
begin
  if (FReferenced.Items.Count = 0) then
  begin
    List := TList.Create();
    List.Add(Routine.ReferencedRequester);
    if (not Database.Session.Update(List)) then
      FReferenced.Cursor := crSQLWait
    else
      FReferencedBuild();
    List.Free();
  end;
end;

procedure TDRoutine.UMChangePreferences(var Message: TMessage);
begin
  PSQLWait.Caption := Preferences.LoadStr(882) + '...';

  TSBasics.Caption := Preferences.LoadStr(108);
  GBasics.Caption := Preferences.LoadStr(85);
  FLName.Caption := Preferences.LoadStr(35) + ':';
  FLSecurity.Caption := Preferences.LoadStr(798) + ':';
  FSecurityDefiner.Caption := Preferences.LoadStr(799);
  FSecurityInvoker.Caption := Preferences.LoadStr(561);
  FLComment.Caption := Preferences.LoadStr(111) + ':';

  TSInformation.Caption := Preferences.LoadStr(121);
  GDates.Caption := Preferences.LoadStr(122);
  FLCreated.Caption := Preferences.LoadStr(118) + ':';
  FLUpdated.Caption := Preferences.LoadStr(119) + ':';
  GDefiner.Caption := Preferences.LoadStr(561);
  FLDefiner.Caption := Preferences.LoadStr(799) + ':';
  GSize.Caption := Preferences.LoadStr(67);
  FLSize.Caption := Preferences.LoadStr(67) + ':';

  TSReferenced.Caption := Preferences.LoadStr(782);
  FReferenced.Column[0].Caption := Preferences.LoadStr(35);
  FReferenced.Column[1].Caption := Preferences.LoadStr(69);

  TSSource.Caption := Preferences.LoadStr(198);
  if (not Preferences.Editor.CurrRowBGColorEnabled) then
    FSource.ActiveLineColor := clNone
  else
    FSource.ActiveLineColor := Preferences.Editor.CurrRowBGColor;
  FSource.Font.Name := Preferences.SQLFontName;
  FSource.Font.Style := Preferences.SQLFontStyle;
  FSource.Font.Color := Preferences.SQLFontColor;
  FSource.Font.Size := Preferences.SQLFontSize;
  FSource.Font.Charset := Preferences.SQLFontCharset;
  if (Preferences.Editor.LineNumbersForeground = clNone) then
    FSource.Gutter.Font.Color := clWindowText
  else
    FSource.Gutter.Font.Color := Preferences.Editor.LineNumbersForeground;
  if (Preferences.Editor.LineNumbersBackground = clNone) then
    FSource.Gutter.Color := clBtnFace
  else
    FSource.Gutter.Color := Preferences.Editor.LineNumbersBackground;
  FSource.Gutter.Font.Style := Preferences.Editor.LineNumbersStyle;

  msUndo.Action := MainAction('aEUndo'); msCut.ShortCut := 0;
  msCut.Action := MainAction('aECut'); msCut.ShortCut := 0;
  msCopy.Action := MainAction('aECopy'); msCopy.ShortCut := 0;
  msPaste.Action := MainAction('aEPaste'); msPaste.ShortCut := 0;
  msDelete.Action := MainAction('aEDelete'); msDelete.ShortCut := 0;
  msSelectAll.Action := MainAction('aESelectAll'); msSelectAll.ShortCut := 0;

  FBHelp.Caption := Preferences.LoadStr(167);
  FBOk.Caption := Preferences.LoadStr(29);
  FBCancel.Caption := Preferences.LoadStr(30);
end;

initialization
  FRoutine := nil;
end.
