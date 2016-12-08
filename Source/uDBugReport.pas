unit uDBugReport;

interface {********************************************************************}

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics,
  Controls, Forms, Dialogs, StdCtrls, ExtCtrls,
  madExcept,
  Forms_Ext;

type
  TDBugReport = class(TForm_Ext)
    Panel: TPanel;
    FIcon: TImage;
    FLHeader: TLabel;
    FLTitle: TLabel;
    FBOk: TButton;
    FBCancel: TButton;
    FBClipboard: TButton;
    FLDescription: TLabel;
    FLMessage: TLabel;
    procedure FormCreate(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure FBClipboardClick(Sender: TObject);
    procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);
  public
    MEException: IMEException;
    Report: string;
    function Execute(): Boolean;
  end;

function DBugReport(): TDBugReport;

implementation {***************************************************************}

uses
  Math, Clipbrd,
  uPreferences,
  uBase;

{$R *.dfm}

var
  FDBugReport: TDBugReport;

function DBugReport(): TDBugReport;
begin
  if (not Assigned(FDBugReport)) then
  begin
    Application.CreateForm(TDBugReport, FDBugReport);
    FDBugReport.Perform(UM_CHANGEPREFERENCES, 0, 0);
  end;

  Result := FDBugReport;
end;

{ TDBugReport *****************************************************************}

function TDBugReport.Execute(): Boolean;
begin
  ShowModal();
  Result := ModalResult = mrOk;
end;

procedure TDBugReport.FBClipboardClick(Sender: TObject);
begin
  Clipboard.Open();
  try
    Clipboard.AsText := Report;
  finally
    Clipboard.Close();
  end;
end;

procedure TDBugReport.FormCloseQuery(Sender: TObject; var CanClose: Boolean);
begin
  if (ModalResult = mrOk) then
    SendShellMail(MEException.MailAddr,
      LoadStr(1000) + ' ' + Preferences.VersionStr + ' - Bug Report',
      Report);
end;

procedure TDBugReport.FormCreate(Sender: TObject);
begin
  Caption := LoadStr(1000);

  FIcon.Left := GetSystemMetrics(SM_CXICON) div 2;
  FIcon.Width := GetSystemMetrics(SM_CXICON);
  FIcon.Top := GetSystemMetrics(SM_CYICON) div 2;
  FIcon.Height := GetSystemMetrics(SM_CYICON);
  FIcon.Picture.Icon.Handle := LoadIcon(0, IDI_ERROR);
end;

procedure TDBugReport.FormShow(Sender: TObject);
var
  MinWidth: Integer;
  Rect: TRect;
begin
  Rect.Left := 0;
  Rect.Top := 0;
  MinWidth := 4 * FBClipboard.Left + FBClipboard.Width + ClientWidth - FBOk.Left - 2 * GetSystemMetrics(SM_CXICON);

  if (not (MEException.ExceptObject is Exception)) then
    FLMessage.Caption := MEException.ExceptMessage
  else
    FLMessage.Caption := Exception(MEException.ExceptObject).Message;

  Rect.Width := MinWidth;
  Rect.Height := FLMessage.Canvas.TextHeight('I');
  FLMessage.Canvas.Font.Handle := Canvas.Font.Handle;
  FLMessage.Canvas.Font := Font;
  DrawText(Canvas.Handle, PChar(FLMessage.Caption), Length(FLMessage.Caption), Rect, DT_CALCRECT or DT_NOPREFIX or DT_WORDBREAK);
  FLMessage.Width := Rect.Width;
  FLMessage.Height := Rect.Height;

  MinWidth := Max(MinWidth, Rect.Width);

  FLHeader.Left := 2 * GetSystemMetrics(SM_CXICON);
  FLHeader.Top := GetSystemMetrics(SM_CYICON) div 2;
  FLHeader.Height := FLHeader.Canvas.TextHeight('I');
  FLHeader.Width := MinWidth;

  Rect.Width := MinWidth;
  Rect.Height := FLTitle.Canvas.TextHeight('I');
  FLTitle.Canvas.Font.Handle := Canvas.Font.Handle;
  FLTitle.Canvas.Font := Font;
  DrawText(FLTitle.Canvas.Handle, PChar(FLTitle.Caption), Length(FLTitle.Caption), Rect, DT_CALCRECT or DT_NOPREFIX or DT_WORDBREAK);
  FLTitle.Left :=  2 * GetSystemMetrics(SM_CXICON);
  FLTitle.Top := FLHeader.Top + FLHeader.Height + Canvas.TextHeight('I');
  FLTitle.Width := Rect.Width;
  FLTitle.Height := Rect.Height;

  Rect.Width := MinWidth;
  Rect.Height := FLDescription.Canvas.TextHeight('I');
  FLDescription.Canvas.Font.Handle := Canvas.Font.Handle;
  FLDescription.Canvas.Font := Font;
  DrawText(FLDescription.Canvas.Handle, PChar(FLDescription.Caption), Length(FLDescription.Caption), Rect, DT_CALCRECT or DT_NOPREFIX or DT_WORDBREAK);
  FLDescription.Left :=  2 * GetSystemMetrics(SM_CXICON);
  FLDescription.Top := FLTitle.Top + FLTitle.Height + Canvas.TextHeight('I');
  FLDescription.Width := Rect.Width;
  FLDescription.Height := Rect.Height;

  FLMessage.Left :=  2 * GetSystemMetrics(SM_CXICON);
  FLMessage.Top := FLDescription.Top + FLDescription.Height + Canvas.TextHeight('I');

  ClientHeight := FLMessage.Top + FLMessage.Height + GetSystemMetrics(SM_CYICON) + Panel.Height;
  ClientWidth := 2 * GetSystemMetrics(SM_CXICON) + MinWidth + GetSystemMetrics(SM_CXICON);

  MessageBeep(MB_ICONERROR);
  ActiveControl := FBOk;
end;

initialization
  FDBugReport := nil;
end.
