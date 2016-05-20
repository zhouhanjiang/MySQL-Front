unit fPDataBrowserDummy;

interface

uses
  ToolWin, Classes, Controls, StdCtrls, ComCtrls,
  StdCtrls_Ext, Forms_Ext;

type
  TPDataBrowserDummy = class(TForm_Ext)
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
  end;

var
  PDataBrowserDummy: TPDataBrowserDummy;

implementation

{$R *.dfm}

end.
