unit uDPartition;

interface {********************************************************************}

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, ComCtrls,
  Forms_Ext, ComCtrls_Ext, StdCtrls_Ext,
  uSession,
  uBase;

type
  TDPartition = class(TForm_Ext)
    FBCancel: TButton;
    FBOk: TButton;
    FComment: TEdit;
    FEngine: TComboBox_Ext;
    FExpression: TEdit;
    FLComment: TLabel;
    FLEngine: TLabel;
    FLExpression: TLabel;
    FLMaxRows: TLabel;
    FLMinRows: TLabel;
    FLName: TLabel;
    FMaxRows: TEdit;
    FMinRows: TEdit;
    FName: TEdit;
    FUDMaxRows: TUpDown;
    FUDMinRows: TUpDown;
    GBasics: TGroupBox_Ext;
    procedure FBOkCheckEnabled(Sender: TObject);
    procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);
    procedure FormShow(Sender: TObject);
    procedure UMChangePreferences(var Message: TMessage); message UM_CHANGEPREFERENCES;
  public
    Partition: TSPartition;
    Table: TSBaseTable;
    function Execute(): Boolean;
  end;

function DPartition(): TDPartition;

implementation {***************************************************************}

{$R *.dfm}

uses
  StrUtils,
  SQLUtils,
  uPreferences;

var
  FDPartition: TDPartition;

function DPartition(): TDPartition;
begin
  if (not Assigned(FDPartition)) then
  begin
    Application.CreateForm(TDPartition, FDPartition);
    FDPartition.Perform(UM_CHANGEPREFERENCES, 0, 0);
  end;

  Result := FDPartition;
end;

{ TDPartition *****************************************************************}

function TDPartition.Execute(): Boolean;
begin
  Result := ShowModal() = mrOk;
end;

procedure TDPartition.FBOkCheckEnabled(Sender: TObject);
begin
  FBOk.Enabled := (FExpression.Text <> '')
    and (FName.Text <> '') and (not Assigned(Table.PartitionByName(FName.Text)) or Assigned(Partition) and (lstrcmpi(PChar(FName.Text), PChar(Partition.Name)) = 0));
end;

procedure TDPartition.FormCloseQuery(Sender: TObject;
  var CanClose: Boolean);
var
  NewPartition: TSPartition;
begin
  if (ModalResult = mrOk) then
  begin
    NewPartition := TSPartition.Create(Table.Partitions, Table);
    if (Assigned(Partition)) then
      NewPartition.Assign(Partition);

    NewPartition.Name := Trim(FName.Text);
    NewPartition.Engine := Table.Database.Session.EngineByName(FEngine.Text);
    NewPartition.ValuesExpr := Trim(FExpression.Text);
    if (FMinRows.Text = '') then
      NewPartition.MinRows := -1
    else
      NewPartition.MaxRows := FUDMinRows.Position;
    if (FMaxRows.Text = '') then
      NewPartition.MaxRows := -1
    else
      NewPartition.MaxRows := FUDMaxRows.Position;
    if (not Assigned(Partition) or (Trim(FComment.Text) <> SQLUnwrapStmt(NewPartition.Comment, Table.Session.Connection.MySQLVersion))) then
      NewPartition.Comment := Trim(FComment.Text);

    if (not Assigned(Partition)) then
      Table.Partitions.AddPartition(NewPartition)
    else
      Table.Partitions.UpdatePartition(Partition, NewPartition);

    CanClose := True;

    NewPartition.Free();
  end;
end;

procedure TDPartition.FormShow(Sender: TObject);
var
  I: Integer;
begin
  if (not Assigned(Partition)) then
    Caption := Preferences.LoadStr(839)
  else
    Caption := Preferences.LoadStr(842, Partition.Name);

  if (Table.Database.Session.Engines.Count = 0) then
    FEngine.Style := csDropDown
  else
    FEngine.Style := csDropDownList;
  FEngine.Items.Clear();
  for I := 0 to Table.Database.Session.Engines.Count - 1 do
    FEngine.Items.Add(Table.Database.Session.Engines.Engine[I].Name);

  if (not Assigned(Partition)) then
  begin
    FName.Text := '';
    FEngine.ItemIndex := FEngine.Items.IndexOf(Table.Database.Session.Engines.DefaultEngine.Name);
    FExpression.Text := '';
    FMinRows.Text := '';
    FMaxRows.Text := '';
    FComment.Text := '';
  end
  else
  begin
    FName.Text := Partition.Name;
    FEngine.ItemIndex := FEngine.Items.IndexOf(Partition.Engine.Name);
    FExpression.Text := Partition.ValuesExpr;
    if (Partition.MinRows < 0) then
      FMinRows.Text := ''
    else
      FUDMinRows.Position := Partition.MinRows;
    if (Partition.MaxRows < 0) then
      FMaxRows.Text := ''
    else
      FUDMaxRows.Position := Partition.MaxRows;
    FComment.Text := SQLUnwrapStmt(Partition.Comment, Table.Session.Connection.MySQLVersion);
  end;

  FBOk.Enabled := False;

  ActiveControl := FBCancel;
  ActiveControl := FName;
end;

procedure TDPartition.UMChangePreferences(var Message: TMessage);
begin
  GBasics.Caption := Preferences.LoadStr(85);
  FLName.Caption := Preferences.LoadStr(35) + ':';
  FLEngine.Caption := Preferences.LoadStr(110) + ':';
  FLExpression.Caption := Preferences.LoadStr(836) + ':';
  FLMinRows.Caption := Preferences.LoadStr(837);
  FLMaxRows.Caption := Preferences.LoadStr(838);
  FLComment.Caption := Preferences.LoadStr(111) + ':';

  FBOk.Caption := Preferences.LoadStr(29);
  FBCancel.Caption := Preferences.LoadStr(30);
end;

initialization
  FDPartition := nil;
end.
