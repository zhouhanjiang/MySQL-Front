unit uPObjectSearch;

interface {********************************************************************}

uses
  Windows, Messages, Classes, Dialogs,
  Controls, Forms, StdCtrls,
  uBase, uSession;

type
  TPObjectSearch = class(TForm)
    FComment: TCheckBox;
    FDatabases: TCheckBox;
    FEvents: TCheckBox;
    FFields: TCheckBox;
    FLWhat: TLabel;
    FLWhere: TLabel;
    FName: TCheckBox;
    FRoutines: TCheckBox;
    FTables: TCheckBox;
    FTriggers: TCheckBox;
  private
    FLocation: TObject;
    FSession: TSSession;
    procedure CMShowingChanged(var Message: TMessage); message CM_SHOWINGCHANGED;
    function GetComment(): Boolean;
    function GetDatabases(): Boolean;
    function GetEvents(): Boolean;
    function GetFields(): Boolean;
    function GetName(): Boolean;
    function GetRoutines(): Boolean;
    function GetTables(): Boolean;
    function GetTriggers(): Boolean;
    procedure SetLocation(ALocation: TObject);
    procedure UMChangePreferences(var Message: TMessage); message UM_CHANGEPREFERENCES;
    procedure WMActivate(var Message: TWMActivate); message WM_ACTIVATE;
  protected
    procedure CreateParams(var Params: TCreateParams); override;
  public
    property Comment: Boolean read GetComment;
    property Databases: Boolean read GetDatabases;
    property Events: Boolean read GetEvents;
    property Fields: Boolean read GetFields;
    property Location: TObject read FLocation write SetLocation;
    property Name: Boolean read GetName;
    property Routines: Boolean read GetRoutines;
    property Session: TSSession read FSession write FSession;
    property Tables: Boolean read GetTables;
    property Triggers: Boolean read GetTriggers;
  end;

implementation {***************************************************************}

uses
  uPreferences;

{$R *.dfm}

{ TPObjectSearch **************************************************************}

procedure TPObjectSearch.CMShowingChanged(var Message: TMessage);
var
  Animation: BOOL;
begin
  Include(FFormState, fsShowing);
  try
    try
      DoShow();
    except
      Application.HandleException(Self);
    end;
    if (not Showing) then
      ShowWindow(Handle, SW_HIDE)
    else if (SystemParametersInfo(SPI_GETCLIENTAREAANIMATION, 0, @Animation, 0) and Animation) then
      AnimateWindow(Handle, 150, AW_VER_POSITIVE or AW_SLIDE)
    else
      ShowWindow(Handle, SW_SHOW);
  finally
    Exclude(FFormState, fsShowing);
  end;
end;

procedure TPObjectSearch.CreateParams(var Params: TCreateParams);
begin
  inherited;

  Params.Style := WS_POPUP or WS_BORDER;
  Params.WindowClass.Style := Params.WindowClass.Style or CS_DROPSHADOW;

  if (Assigned(PopupParent)) then
    Params.WndParent := PopupParent.Handle;
end;

function TPObjectSearch.GetComment(): Boolean;
begin
  Result := FComment.Checked;
end;

function TPObjectSearch.GetDatabases(): Boolean;
begin
  Result := FDatabases.Checked;
end;

function TPObjectSearch.GetEvents(): Boolean;
begin
  Result := FEvents.Checked;
end;

function TPObjectSearch.GetFields(): Boolean;
begin
  Result := FFields.Checked;
end;

function TPObjectSearch.GetName(): Boolean;
begin
  Result := FName.Checked;
end;

function TPObjectSearch.GetRoutines(): Boolean;
begin
  Result := FRoutines.Checked;
end;

function TPObjectSearch.GetTables(): Boolean;
begin
  Result := FTables.Checked;
end;

function TPObjectSearch.GetTriggers(): Boolean;
begin
  Result := FTriggers.Checked;
end;

procedure TPObjectSearch.SetLocation(ALocation: TObject);
begin
  FLocation := ALocation;

  FDatabases.Visible := (Location is TSSession);
  FTables.Visible := (Location is TSSession) or (Location is TSDatabase);
  FRoutines.Visible := ((Location is TSSession) or (Location is TSDatabase)) and (Session.Connection.MySQLVersion >= 50004);
  FEvents.Visible := ((Location is TSSession) or (Location is TSDatabase)) and (Session.Connection.MySQLVersion >= 50106);
  FFields.Visible := (Location is TSSession) or (Location is TSDatabase) or (Location is TSTable);
  FTriggers.Visible := ((Location is TSSession) or (Location is TSDatabase) or (Location is TSTable)) and (Session.Connection.MySQLVersion >= 50010);
end;

procedure TPObjectSearch.UMChangePreferences(var Message: TMessage);
begin
  FLWhat.Caption := Preferences.LoadStr(227) + ':';
  FDatabases.Caption := Preferences.LoadStr(265);
  FTables.Caption := Preferences.LoadStr(234) + ' + ' + Preferences.LoadStr(873);
  FRoutines.Caption := Preferences.LoadStr(874) + ' + ' + Preferences.LoadStr(875);
  FEvents.Caption := Preferences.LoadStr(876);
  FFields.Caption := Preferences.LoadStr(253);
  FTriggers.Caption := Preferences.LoadStr(797);

  FLWhere.Caption := Preferences.LoadStr(936) + ':';
  FName.Caption := Preferences.LoadStr(35);
  FComment.Caption := Preferences.LoadStr(111);
end;

procedure TPObjectSearch.WMActivate(var Message: TWMActivate);
begin
  if (Message.Active <> WA_INACTIVE) then
    SendMessage(PopupParent.Handle, WM_NCACTIVATE, WPARAM(TRUE), 0);

  inherited;
end;

end.

