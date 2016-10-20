unit fDInfo;

interface {********************************************************************}

uses
  Windows, Messages, SysUtils, Graphics, Forms, Jpeg,
  ExtCtrls_Ext, Forms_Ext,
  fBase, Vcl.ExtCtrls, System.Classes, Vcl.Controls, Vcl.StdCtrls;

type
  TDInfo = class(TForm_Ext)
    FBuild: TLabel;
    FImage: TImage;
    FBOk: TButton;
    FURI: TLabel;
    FVersion: TLabel;
    PImage: TPanel;
    FName: TLabel;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure FURIClick(Sender: TObject);
  private
    procedure CMSysFontChanged(var Message: TMessage); message CM_SYSFONTCHANGED;
    procedure UMChangePreferences(var Message: TMessage); message UM_CHANGEPREFERENCES;
  public
    function Execute(): Boolean;
  end;

function DInfo(): TDInfo;

implementation {***************************************************************}

{$R *.dfm}

uses
  ShellAPI, GDIPAPI, GDIPObj, GDIPUTIL,
  fPreferences;

var
  FInfo: TDInfo;

function DInfo(): TDInfo;
begin
  if (not Assigned(FInfo)) then
  begin
    Application.CreateForm(TDInfo, FInfo);
    FInfo.Perform(UM_CHANGEPREFERENCES, 0, 0);
  end;

  Result := FInfo;
end;

{ TDInfo **********************************************************************}

procedure TDInfo.CMSysFontChanged(var Message: TMessage);
begin
  inherited;

  FName.Font.Name := Font.Name;
  FVersion.Font.Name := Font.Name;
  FBuild.Font.Name := Font.Name;
  FURI.Font.Name := Font.Name;
end;

function TDInfo.Execute(): Boolean;
begin
  ShowModal();
  Result := ModalResult = mrOk;
end;

procedure TDInfo.FormCreate(Sender: TObject);
var
  JPEGImage: TJPEGImage;
  Stream: TResourceStream;
begin
  Stream := TResourceStream.CreateFromID(HInstance, 1, RT_RCDATA);
  JPEGImage := TJPEGImage.Create();
  JPEGImage.LoadFromStream(Stream);
  FImage.Canvas.StretchDraw(Rect(0, 0, FImage.Width, FImage.Height), JPEGImage);
  JPEGImage.Free();
  Stream.Free();
end;

procedure TDInfo.FormDestroy(Sender: TObject);
begin
  if (Assigned(FImage.Picture.Graphic)) then
    FImage.Picture.Graphic := nil;
end;

procedure TDInfo.FormShow(Sender: TObject);
begin
  ActiveControl := FBOk;
end;

procedure TDInfo.FURIClick(Sender: TObject);
begin
  ShellExecute(Handle, 'open', PChar(FURI.Caption), '', '', SW_SHOW);
end;

procedure TDInfo.UMChangePreferences(var Message: TMessage);
begin
  Caption := Preferences.LoadStr(367) + ' ' + LoadStr(1000);

  FName.Caption := SysUtils.LoadStr(1000);
  FVersion.Caption := Preferences.LoadStr(169) + ' ' + IntToStr(Preferences.VerMajor) + '.' + IntToStr(Preferences.VerMinor);
  FBuild.Caption := '(' + Preferences.LoadStr(737) + ': ' + IntToStr(Preferences.VerPatch) + '.' + IntToStr(Preferences.VerBuild) + ')';

  FURI.Caption := SysUtils.LoadStr(1004);

  FBOk.Caption := Preferences.LoadStr(231);
end;

initialization
  FInfo := nil;
end.

