unit MySQLReg;

interface {********************************************************************}

uses
  DBReg;

procedure Register();

implementation {***************************************************************}

uses
  Classes, SysUtils, DesignIntf, DSDesign, DesignEditors,
  MySQLDB, MySQLDBGrid;

procedure Register();
begin
  RegisterComponents('MySQL', [TMySQLConnection, TMySQLQuery, TMySQLDataSet, TMySQLTable, TMySQLMonitor, TMySQLDBGrid]);
end;

end.
