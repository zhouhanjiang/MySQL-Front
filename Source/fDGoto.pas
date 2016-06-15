unit fDGoto;

interface {********************************************************************}

uses
  Windows, Messages, Graphics, Forms, Controls, Classes, StdCtrls,
  DBGrids,
  Forms_Ext,
  fBase;

type
  TDGoto = class(TForm_Ext)
    FBCancel: TButton;
    FBOk: TButton;
    FLField: TLabel;
    FField: TComboBox;
    procedure FormHide(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure FFieldChange(Sender: TObject);
    procedure FFieldDrawItem(Control: TWinControl; Index: Integer; Rect: TRect;
      State: TOwnerDrawState);
  protected
    procedure CMChangePreferences(var Message: TMessage); message UM_CHANGEPREFERENCES;
  public
    Captions: string;
    DBGrid: TDBGrid;
    function Execute(): Boolean;
  end;

function DGoto(): TDGoto;

implementation {***************************************************************}

{$R *.dfm}

uses
  StrUtils, Types,
  fPreferences;

var
  FGoto: TDGoto;

function DGoto(): TDGoto;
begin
  if (not Assigned(FGoto)) then
  begin
    Application.CreateForm(TDGoto, FGoto);
    FGoto.Perform(UM_CHANGEPREFERENCES, 0, 0);
  end;

  Result := FGoto;
end;

{ TDGoto **********************************************************************}

procedure TDGoto.CMChangePreferences(var Message: TMessage);
begin
  Caption := Preferences.LoadStr(676);

  FLField.Caption := Preferences.LoadStr(164) + ':';

  FBOk.Caption := Preferences.LoadStr(29);
  FBCancel.Caption := Preferences.LoadStr(30);
end;

function TDGoto.Execute(): Boolean;
begin
  ShowModal();
  Result := ModalResult = mrOk;
end;

procedure TDGoto.FFieldChange(Sender: TObject);
begin
  FBOk.Enabled := FField.ItemIndex >= 0;
end;

procedure TDGoto.FFieldDrawItem(Control: TWinControl; Index: Integer;
  Rect: TRect; State: TOwnerDrawState);
var
  Text: string;
begin
  Text := FField.Items[Index];

  FField.Canvas.FillRect(Rect);

  OffsetRect(Rect, 2, 1);
  if (DBGrid.DataSource.DataSet.FieldByName(Text).IsIndexField) then
    FField.Canvas.Font.Style := FField.Canvas.Font.Style + [fsBold]
  else
    FField.Canvas.Font.Style := FField.Canvas.Font.Style - [fsBold];
  FField.Canvas.TextRect(Rect, Text, [tfNoPrefix]);
end;

procedure TDGoto.FormHide(Sender: TObject);
begin
  if (ModalResult = mrOK) then
    DBGrid.SelectedField := DBGrid.DataSource.DataSet.Fields[FField.ItemIndex];

  FField.Items.BeginUpdate();
  FField.Items.Clear();
  FField.Items.EndUpdate();
end;

procedure TDGoto.FormShow(Sender: TObject);
var
  I: Integer;
begin
  FField.Items.BeginUpdate();
  for I := 0 to DBGrid.DataSource.DataSet.FieldCount - 1 do
  begin
    FField.Items.Add(DBGrid.DataSource.DataSet.Fields[I].DisplayName);
    if (DBGrid.DataSource.DataSet.Fields[I] = DBGrid.SelectedField) then
      FField.ItemIndex := I;
  end;
  FField.Items.EndUpdate();

  ActiveControl := FField;

  FBOk.Enabled := FField.ItemIndex >= 0;
end;

initialization
  FGoto := nil;
end.
