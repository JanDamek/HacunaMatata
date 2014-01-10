
//////////////////////////////////////////////////
//  SQL Server Data Access Components
//  Copyright © 1998-2012 Devart. All right reserved.
//////////////////////////////////////////////////

{$IFNDEF CLR}

{$I Sdac.inc}

unit MSDesignUtils;
{$ENDIF}

interface

uses
  Classes, SysUtils, DBAccess, DADesignUtils, CRTypes;

type
  TMSDesignUtils = class(TDADesignUtils)
    class function GetProjectName: string; override;

  {$IFNDEF FPC}
    class function GetConnectionList: TObject; override;
  {$ENDIF}

  { Connection support }
    class function HasConnection(Obj: TComponent): boolean; override;
    class function GetConnection(Obj: TComponent): TCustomDAConnection; override;
    class procedure SetConnection(Obj: TComponent; Value: TCustomDAConnection); override;
    class function UsedConnection(Obj: TComponent): TCustomDAConnection; override;

  { TDATable support }
    class function GetTableName(Obj: TCustomDADAtaSet): _string; override;
    class procedure SetTableName(Obj: TCustomDADAtaSet; const Value: _string); override;
    class function GetOrderFields(Obj: TCustomDADAtaSet): _string; override;
    class procedure SetOrderFields(Obj: TCustomDADAtaSet; const Value: _string); override;
    class procedure PrepareSQL(Obj: TCustomDADAtaSet); override;
    class function GetStoredProcName(Obj: TCustomDADataSet): _string; override;
    class procedure SetStoredProcName(Obj: TCustomDADataSet; const Value: _string); override;

  {$IFDEF USE_SYNEDIT}
    class function SQLDialect: integer ; override; // SynHighlighterSQL TSQLDialect = (sqlStandard, sqlInterbase6, sqlMSSQL7, sqlMySQL, sqlOracle, sqlSybase, sqlIngres, sqlMSSQL2K);
  {$ENDIF}
  end;

  TMSDesignUtilsClass = class of TMSDesignUtils;

implementation

uses
{$IFNDEF STD}
  MSServiceBroker,
{$ENDIF}
  MSAccess{$IFDEF SDAC}, MSDesign{$ENDIF};

{ TMSDesignUtils }

class function TMSDesignUtils.GetProjectName: string;
begin
  Result := 'SDAC';
end;

{$IFNDEF FPC}
class function TMSDesignUtils.GetConnectionList: TObject;
begin
{$IFDEF SDAC}
  Result := TMSConnectionList.Create;
{$ELSE}
  Result := nil;
{$ENDIF}
end;
{$ENDIF}

class function TMSDesignUtils.HasConnection(Obj: TComponent): boolean;
begin
{$IFNDEF STD}
  if Obj is TMSServiceBroker then
    Result := True
  else
{$ENDIF}
  if Obj is TMSTableData then
    Result := True
  else
    Result := inherited HasConnection(Obj);
end;

class function TMSDesignUtils.GetConnection(Obj: TComponent): TCustomDAConnection;
begin
  Assert(Obj <> nil);
{$IFNDEF STD}
  if Obj is TMSServiceBroker then
    Result := TMSServiceBroker(Obj).Connection
  else
{$ENDIF}
  if Obj is TMSTableData then
    Result := TMSTableData(Obj).Connection
  else
    Result := inherited GetConnection(Obj);
end;

class procedure TMSDesignUtils.SetConnection(Obj: TComponent; Value: TCustomDAConnection);
begin
  Assert(Obj <> nil);
{$IFNDEF STD}
  if Obj is TMSServiceBroker then
    TMSServiceBroker(Obj).Connection := Value as TMSConnection
  else
{$ENDIF}
  if Obj is TMSTableData then
    TMSTableData(Obj).Connection := Value as TMSConnection
  else
    inherited;
end;

class function TMSDesignUtils.UsedConnection(Obj: TComponent): TCustomDAConnection;
begin
  Assert(Obj <> nil);
  if Obj is TMSTableData then
    Result := TMSAccessUtils.UsedConnection(TMSTableData(Obj))
  else
    Result := inherited UsedConnection(Obj);
end;

class function TMSDesignUtils.GetTableName(Obj: TCustomDADAtaSet): _string;
begin
  Assert(Obj is TCustomMSTable, Obj.ClassName);
  Result := TCustomMSTable(Obj).TableName;
end;

class procedure TMSDesignUtils.SetTableName(Obj: TCustomDADAtaSet;
  const Value: _string);
begin
  Assert(Obj is TCustomMSTable, Obj.ClassName);
  TCustomMSTable(Obj).TableName := Value;
end;

class procedure TMSDesignUtils.PrepareSQL(Obj: TCustomDADAtaSet);
begin
  Assert(Obj is TCustomMSTable, Obj.ClassName);
  TCustomMSTable(Obj).PrepareSQL;
end;

class function TMSDesignUtils.GetOrderFields(Obj: TCustomDADAtaSet): _string;
begin
  Assert(Obj is TCustomMSTable, Obj.ClassName);
  Result := TCustomMSTable(Obj).OrderFields;
end;

class procedure TMSDesignUtils.SetOrderFields(Obj: TCustomDADAtaSet;
  const Value: _string);
begin
  Assert(Obj is TCustomMSTable, Obj.ClassName);
  TCustomMSTable(Obj).OrderFields := Value;
end;

class function TMSDesignUtils.GetStoredProcName(Obj: TCustomDADataSet): _string;
begin
  Assert(Obj is TMSStoredProc, Obj.ClassName);
  Result := TMSStoredProc(Obj).StoredProcName;
end;

class procedure TMSDesignUtils.SetStoredProcName(Obj: TCustomDADataSet; const Value: _string);
begin
  Assert(Obj is TMSStoredProc, Obj.ClassName);
  TMSStoredProc(Obj).StoredProcName := Value;
end;

{$IFDEF USE_SYNEDIT}
class function TMSDesignUtils.SQLDialect: integer;
begin
  Result := 7; // sqlMSSQL2K
end;
{$ENDIF}

end.
