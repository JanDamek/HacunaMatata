//////////////////////////////////////////////////
//  SQL Server Data Access Components
//  Copyright © 1998-2012 Devart. All right reserved.
//  MSScript
//////////////////////////////////////////////////
{$IFNDEF CLR}

{$I Sdac.inc}

unit MSScript;
{$ENDIF}

interface

uses
  SysUtils, Classes, DAScript, CRParser, MSParser, MSAccess, DBAccess;

type
{$IFDEF VER16P}
  [ComponentPlatformsAttribute(pidWin32 or pidWin64)]
{$ENDIF}
  TMSScript = class(TDAScript)
  protected
    function GetConnection: TCustomMSConnection;
    procedure SetConnection(Value: TCustomMSConnection);
    function GetDataSet: TCustomMSDataSet;
    procedure SetDataSet(Value: TCustomMSDataSet);

    function GetProcessorClass: TDAScriptProcessorClass; override;
    function CreateCommand: TCustomDASQL; override;
  public
    function ExecuteNext: boolean; override;

  published
    property Connection: TCustomMSConnection read GetConnection write SetConnection;
    property DataSet: TCustomMSDataSet read GetDataSet write SetDataSet;
    property UseOptimization;
  end;

implementation

uses
  MSScriptProcessor;

{ TMSScript }

function TMSScript.GetProcessorClass: TDAScriptProcessorClass;
begin
  Result := TMSScriptProcessor;
end;

function TMSScript.CreateCommand: TCustomDASQL;
begin
  Result := TMSSQL.Create(nil);
end;

function TMSScript.ExecuteNext: boolean;
var
  OldCommandTimeout: integer;
begin
  OldCommandTimeout := 0;
  if Assigned(DataSet) then
    OldCommandTimeout := DataSet.CommandTimeout;
  try
    Result := inherited ExecuteNext;
  finally
    if Assigned(DataSet) then
      DataSet.CommandTimeout := OldCommandTimeout;
  end;
end;

function TMSScript.GetConnection: TCustomMSConnection;
begin
  Result := TCustomMSConnection(inherited Connection);
end;

procedure TMSScript.SetConnection(Value: TCustomMSConnection);
begin
  inherited Connection := Value;
end;

function TMSScript.GetDataSet: TCustomMSDataSet;
begin
  Result := TCustomMSDataSet(inherited DataSet);
end;

procedure TMSScript.SetDataSet(Value: TCustomMSDataSet);
begin
  inherited DataSet := Value;
end;


end.
