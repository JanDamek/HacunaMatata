
//////////////////////////////////////////////////
//  SQL Server Data Access Components
//  Copyright © 1998-2012 Devart. All right reserved.
//  SDAC registration
//////////////////////////////////////////////////

{$IFNDEF CLR}

{$I Sdac.inc}

unit MSReg;
{$ENDIF}

interface
uses
{$IFDEF FPC}
  LResources,
{$ENDIF}
  Classes, MSAccess, MSSQLMonitor,
{$IFNDEF STD}
  MSLoader, MSDump, MSServiceBroker, MSCompactConnection, MSTransaction,
{$ENDIF}
  MSScript, SdacVcl
  {$IFDEF VER16P}, SdacFmx{$ENDIF};

procedure Register;

implementation

uses
{$IFDEF CLR}
  Devart.Sdac.DataAdapter, 
{$ENDIF}
  DBAccess, DacReg;

{$IFNDEF FPC}
{$IFNDEF CLR}
  {$IFDEF VER9}
    {$R MSDesign9.res}
  {$ELSE}
    {$R MSDesign.res}
  {$ENDIF}
  {$IFDEF VER10P}
    {$R MSDesign10p.res}
  {$ENDIF}
{$ELSE}
  {$R MSDesign.res}
{$ENDIF}
{$ENDIF}

procedure Register;
begin
{$IFNDEF STD}
  RegisterCRBatchMove;
{$ENDIF}

  RegisterComponents('SQL Server Access', [TMSConnection]);
  RegisterComponents('SQL Server Access', [TMSQuery]);
  RegisterComponents('SQL Server Access', [TMSTable]);
  RegisterComponents('SQL Server Access', [TMSStoredProc]);
  RegisterComponents('SQL Server Access', [TMSSQL]);
  RegisterComponents('SQL Server Access', [TMSScript]);
  RegisterComponents('SQL Server Access', [TMSUpdateSQL]);
  RegisterComponents('SQL Server Access', [TMSDataSource]);

{$IFNDEF STD}
  RegisterComponents('SQL Server Access', [TMSLoader]);
  RegisterComponents('SQL Server Access', [TMSDump]);
  RegisterComponents('SQL Server Access', [TMSServiceBroker]);
  RegisterComponents('SQL Server Access', [TMSMetadata]);
{$ENDIF}

  RegisterComponents('SQL Server Access', [TMSSQLMonitor]);
  RegisterComponents('SQL Server Access', [TMSConnectDialog]);
{$IFDEF VER16P}
  RegisterComponents('SQL Server Access', [TMSConnectDialogFmx]);
{$ENDIF}

{$IFNDEF STD}
  RegisterComponents('SQL Server Access', [TMSCompactConnection]);
  RegisterComponents('SQL Server Access', [TMSTransaction]);
  RegisterComponents('SQL Server Access', [TMSChangeNotification]);
{$ENDIF}
  RegisterComponents('SQL Server Access', [TMSTableData]);
end;

{$IFDEF FPC}
initialization
  {$I MSDesign.lrs}
{$ENDIF}

end.

