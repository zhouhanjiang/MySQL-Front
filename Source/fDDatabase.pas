unit fDDatabase;

interface {********************************************************************}

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, Menus, ComCtrls,
  SynEdit, SynMemo,
  Forms_Ext, StdCtrls_Ext,
  fSession, fBase, Vcl.ExtCtrls;
                                                             
type
  TDDatabase = class (TForm_Ext)
    FBCancel: TButton;
    FBHelp: TButton;
    FBOk: TButton;
    FCollation: TComboBox_Ext;
    FCreated: TLabel;
    FDefaultCharset: TComboBox_Ext;
    FLCollation: TLabel;
    FLCreated: TLabel;
    FLDefaultCharset: TLabel;
    FLName: TLabel;
    FLSize: TLabel;
    FLUpdated: TLabel;
    FName: TEdit;
    FSize: TLabel;
    FSource: TSynMemo;
    FUpdated: TLabel;
    GBasics: TGroupBox_Ext;
    GDates: TGroupBox_Ext;
    GSize: TGroupBox_Ext;
    msCopy: TMenuItem;
    MSource: TPopupMenu;
    msSelectAll: TMenuItem;
    N1: TMenuItem;
    PageControl: TPageControl;
    PSQLWait: TPanel;
    TSBasics: TTabSheet;
    TSInformations: TTabSheet;
    TSSource: TTabSheet;
    procedure FBHelpClick(Sender: TObject);
    procedure FBOkCheckEnabled(Sender: TObject);
    procedure FCollationChange(Sender: TObject);
    procedure FCollationDropDown(Sender: TObject);
    procedure FDefaultCharsetChange(Sender: TObject);
    procedure FDefaultCharsetExit(Sender: TObject);
    procedure FNameChange(Sender: TObject);
    procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);
    procedure FormCreate(Sender: TObject);
    procedure FormHide(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure FSourceChange(Sender: TObject);
    procedure TSInformationsShow(Sender: TObject);
    procedure TSSourceShow(Sender: TObject);
  private
    procedure Built();
    procedure FormSessionEvent(const Event: TSSession.TEvent);
    function GetName(): string;
    procedure CMChangePreferences(var Message: TMessage); message CM_CHANGEPREFERENCES;
  public
    Session: TSSession;
    Database: TSDatabase;
    function Execute(): Boolean;
    property Name: string read GetName;
  end;

function DDatabase(): TDDatabase;

implementation {***************************************************************}

{$R *.dfm}

uses
  fPreferences;

var
  FDatabase: TDDatabase;

function DDatabase(): TDDatabase;
begin
  if (not Assigned(FDatabase)) then
  begin
    Application.CreateForm(TDDatabase, FDatabase);
    FDatabase.Perform(CM_CHANGEPREFERENCES, 0, 0);
  end;

  Result := FDatabase;
end;

{ TDDatabase ******************************************************************}

procedure TDDatabase.Built();
begin
  FName.Text := Database.Name;

  FDefaultCharset.ItemIndex := FDefaultCharset.Items.IndexOf(Database.DefaultCharset); FDefaultCharsetChange(nil);
  FCollation.ItemIndex := FCollation.Items.IndexOf(Database.Collation);

  FSource.Lines.Text := Database.Source;

  TSSource.TabVisible := Database.Source <> '';

  PageControl.Visible := True;
  PSQLWait.Visible := not PageControl.Visible;

  if (FDefaultCharset.Visible) then
    ActiveControl := FDefaultCharset;
end;

procedure TDDatabase.CMChangePreferences(var Message: TMessage);
begin
  Preferences.SmallImages.GetIcon(iiDatabase, Icon);

  PSQLWait.Caption := Preferences.LoadStr(882);

  TSBasics.Caption := Preferences.LoadStr(108);
  GBasics.Caption := Preferences.LoadStr(85);
  FLName.Caption := Preferences.LoadStr(35) + ':';
  FLDefaultCharset.Caption := Preferences.LoadStr(682) + ':';
  FLCollation.Caption := Preferences.LoadStr(702) + ':';

  TSInformations.Caption := Preferences.LoadStr(121);
  GDates.Caption := Preferences.LoadStr(122);
  FLCreated.Caption := Preferences.LoadStr(118) + ':';
  FLUpdated.Caption := Preferences.LoadStr(119) + ':';
  GSize.Caption := Preferences.LoadStr(125);
  FLSize.Caption := Preferences.LoadStr(67) + ':';

  TSSource.Caption := Preferences.LoadStr(198);
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

  msCopy.Caption := Preferences.LoadStr(64) + #9 + ShortCutToText(scCtrl + Ord('C'));

  msSelectAll.Action := MainAction('aESelectAll'); msSelectAll.ShortCut := 0;

  FBHelp.Caption := Preferences.LoadStr(167);
  FBOk.Caption := Preferences.LoadStr(29);
  FBCancel.Caption := Preferences.LoadStr(30);
end;

function TDDatabase.Execute(): Boolean;
begin
  ShowModal();
  Result := ModalResult = mrOk;
end;

procedure TDDatabase.FBHelpClick(Sender: TObject);
begin
  Application.HelpContext(HelpContext);
end;

procedure TDDatabase.FBOkCheckEnabled(Sender: TObject);
begin
  FBOk.Enabled := (FName.Text <> '')
    and (not Assigned(Session.DatabaseByName(FName.Text)) or (Assigned(Database) and (((Session.LowerCaseTableNames = 0) and (FName.Text = Database.Name)) or ((Session.LowerCaseTableNames > 0) and ((lstrcmpi(PChar(FName.Text), PChar(Database.Name)) = 0))))));
end;

procedure TDDatabase.FCollationChange(Sender: TObject);
begin
  FBOkCheckEnabled(Sender);
  TSSource.TabVisible := False;
end;

procedure TDDatabase.FCollationDropDown(Sender: TObject);
var
  I, J: Integer;
begin
  if (Assigned(Session.Collations) and (FCollation.ItemIndex < 0)) then
    for I := 0 to Session.Collations.Count - 1 do
      if ((lstrcmpi(PChar(Session.Collations[I].Charset.Name), PChar(FDefaultCharset.Text)) = 0) and Session.Collations[I].Default) then
        for J := 1 to FCollation.Items.Count - 1 do
          if (lstrcmpi(PChar(FCollation.Items[J]), PChar(Session.Collations[I].Name)) = 0) then
            FCollation.ItemIndex := FCollation.Items.IndexOf(FCollation.Items[J]);

  TSSource.TabVisible := False;

  FBOkCheckEnabled(Sender);
end;

procedure TDDatabase.FDefaultCharsetChange(Sender: TObject);
var
  Charset: TSCharset;
  I: Integer;
begin
  Charset := Session.CharsetByName(FDefaultCharset.Text);

  FCollation.Items.Clear();
  FCollation.Items.Add('');
  if (Assigned(Session.Collations)) then
    for I := 0 to Session.Collations.Count - 1 do
      if (Session.Collations[I].Charset = Charset) then
        FCollation.Items.Add(Session.Collations[I].Name);

  FCollation.Enabled := FDefaultCharset.Text <> ''; FLCollation.Enabled := FCollation.Enabled;

  FBOkCheckEnabled(Sender);
  TSSource.TabVisible := False;
end;

procedure TDDatabase.FDefaultCharsetExit(Sender: TObject);
begin
  if ((FDefaultCharset.Text = '') and Assigned(Database)) then
    FDefaultCharset.Text := Database.DefaultCharset;
end;

procedure TDDatabase.FNameChange(Sender: TObject);
begin
  FBOkCheckEnabled(Sender);
  TSSource.TabVisible := False;
end;

procedure TDDatabase.FormSessionEvent(const Event: TSSession.TEvent);
begin
  if ((Event.EventType = ceItemValid) and (Event.CItem = Database)) then
    Built()
  else if ((Event.EventType in [ceItemCreated, ceItemAltered]) and (Event.CItem is TSDatabase)) then
    ModalResult := mrOk
  else if ((Event.EventType = ceAfterExecuteSQL) and (Event.Session.ErrorCode <> 0)) then
  begin
    PageControl.Visible := True;
    PSQLWait.Visible := not PageControl.Visible;
  end;
end;

procedure TDDatabase.FormCloseQuery(Sender: TObject;
  var CanClose: Boolean);
var
  NewDatabase: TSDatabase;
begin
  if ((ModalResult = mrOk) and PageControl.Visible) then
  begin
    NewDatabase := TSDatabase.Create(Session);
    if (Assigned(Database)) then
      NewDatabase.Assign(Database);

    NewDatabase.Name := Trim(FName.Text);
    if (not FDefaultCharset.Visible) then
      NewDatabase.DefaultCharset := ''
    else
      NewDatabase.DefaultCharset := Trim(FDefaultCharset.Text);
    if (not FCollation.Visible) then
      NewDatabase.Collation := ''
    else
      NewDatabase.Collation := Trim(FCollation.Text);

    if (not Assigned(Database) or not Assigned(Session.DatabaseByName(Database.Name))) then
      CanClose := Session.AddDatabase(NewDatabase)
    else
      CanClose := Session.UpdateDatabase(Database, NewDatabase);

    NewDatabase.Free();

    PageControl.Visible := not CanClose;
    PSQLWait.Visible := not PageControl.Visible;
    if (not CanClose) then
      ModalResult := mrNone;

    FBOk.Enabled := False;
  end;
end;

procedure TDDatabase.FormCreate(Sender: TObject);
begin
  Constraints.MinWidth := Width;
  Constraints.MinHeight := Height;

  BorderStyle := bsSizeable;

  if ((Preferences.Database.Width >= Width) and (Preferences.Database.Height >= Height)) then
  begin
    Width := Preferences.Database.Width;
    Height := Preferences.Database.Height;
  end;

  FSource.Highlighter := MainHighlighter;

  PageControl.ActivePage := TSBasics; // TSInformationsShow soll nicht vorzeitig aufgerufen werden
end;

procedure TDDatabase.FormHide(Sender: TObject);
begin
  Session.UnRegisterEventProc(FormSessionEvent);

  Preferences.Database.Width := Width;
  Preferences.Database.Height := Height;

  FSource.Lines.Clear();

  PageControl.ActivePage := TSBasics; // TSInformationsShow soll nicht vorzeitig aufgerufen werden
end;

procedure TDDatabase.FormShow(Sender: TObject);
var
  DatabaseName: string;
  I: Integer;
begin
  Session.RegisterEventProc(FormSessionEvent);

  if (not Assigned(Database)) then
    Caption := Preferences.LoadStr(147)
  else
    Caption := Preferences.LoadStr(842, Database.Name);

  if (not Assigned(Database)) then
    HelpContext := 1044
  else
    HelpContext := 1095;

  if (not Assigned(Database) and (Session.LowerCaseTableNames = 1)) then
    FName.CharCase := ecLowerCase
  else
    FName.CharCase := ecNormal;

  FDefaultCharset.Items.Clear();
  FDefaultCharset.Items.Add('');
  for I := 0 to Session.Charsets.Count - 1 do
    FDefaultCharset.Items.Add(Session.Charsets[I].Name);
  FDefaultCharset.Text := ''; FDefaultCharsetChange(Sender);

  if (not Assigned(Database)) then
  begin
    FName.Text := Preferences.LoadStr(145);
    while (Assigned(Session.DatabaseByName(FName.Text))) do
    begin
      DatabaseName := FName.Text;
      Delete(DatabaseName, 1, Length(Preferences.LoadStr(145)));
      if (DatabaseName = '') then DatabaseName := '1';
      DatabaseName := Preferences.LoadStr(145) + IntToStr(StrToInt(DatabaseName) + 1);
      FName.Text := DatabaseName;
    end;

    FDefaultCharset.ItemIndex := FDefaultCharset.Items.IndexOf(Session.DefaultCharset); FDefaultCharsetChange(Sender);
    FCollation.ItemIndex := -1;

    TSSource.TabVisible := False;

    PageControl.Visible := True;
    PSQLWait.Visible := not PageControl.Visible;
  end
  else
  begin
    PageControl.Visible := Database.Update();
    PSQLWait.Visible := not PageControl.Visible;

    if (PageControl.Visible) then
      Built();
  end;

  FName.Enabled := not Assigned(Database) or not Assigned(Session.DatabaseByName(Database.Name));
  FDefaultCharset.Visible := Session.ServerVersion >= 40101; FLDefaultCharset.Visible := FDefaultCharset.Visible;
  FCollation.Visible := Session.ServerVersion >= 40101; FLCollation.Visible := FCollation.Visible;
  TSInformations.TabVisible := not FName.Enabled;

  FName.SelectAll();

  FBOk.Enabled := PageControl.Visible and not Assigned(Database);

  ActiveControl := FBCancel;
  if (PageControl.Visible) then
    if (FName.Enabled) then
      ActiveControl := FName
    else if (FDefaultCharset.Visible) then
      ActiveControl := FDefaultCharset
    else
      ActiveControl := FBCancel;
end;

procedure TDDatabase.FSourceChange(Sender: TObject);
begin
  MainAction('aECopyToFile').Enabled := FSource.SelText <> '';
end;

function TDDatabase.GetName(): string;
begin
  Result := FName.Text;
end;

procedure TDDatabase.TSInformationsShow(Sender: TObject);
begin
  FCreated.Caption := '???';
  FUpdated.Caption := '???';
  FSize.Caption := '???';

  if (Database.Created = 0) then FCreated.Caption := '???' else FCreated.Caption := SysUtils.DateTimeToStr(Database.Created, LocaleFormatSettings);
  if (Database.Updated = 0) then FUpdated.Caption := '???' else FUpdated.Caption := SysUtils.DateTimeToStr(Database.Updated, LocaleFormatSettings);

  FSize.Caption := SizeToStr(Database.Size);
end;

procedure TDDatabase.TSSourceShow(Sender: TObject);
begin
  if (FSource.Lines.Count = 0) then
    FSource.Lines.Text := Database.Source + #13#10;
end;

initialization
  FDatabase := nil;
end.
