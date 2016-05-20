unit fPDataBrowserDummy;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics,
  Controls, Forms, Dialogs, StdCtrls, Vcl.ComCtrls, Vcl.ToolWin, StdCtrls_Ext;

type
  TPDataBrowserDummy = class(TForm)
    FOffset: TEdit;
    FUDOffset: TUpDown;
    FLimit: TEdit;
    FUDLimit: TUpDown;
    TBLimitEnabled: TToolBar;
    FLimitEnabled: TToolButton;
    FFilter: TComboBox_Ext;
    TBFilterEnabled: TToolBar;
    FFilterEnabled: TToolButton;
    FQuickSearch: TEdit;
    TBQuickSearchEnabled: TToolBar;
    FQuickSearchEnabled: TToolButton;
  private
  public
  end;

var
  PDataBrowserDummy: TPDataBrowserDummy;

implementation

{$R *.dfm}

end.
