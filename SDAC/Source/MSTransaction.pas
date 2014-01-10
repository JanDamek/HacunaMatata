//////////////////////////////////////////////////
//  SQL Server Data Access Components
//  Copyright © 1998-2012 Devart. All right reserved.
//  MSTransaction
//////////////////////////////////////////////////

{$IFNDEF CLR}

{$I Sdac.inc}

unit MSTransaction;
{$ENDIF}

interface

uses
  Classes, DBAccess, MSAccess;
  
type
{$IFDEF VER16P}
  [ComponentPlatformsAttribute(pidWin32 or pidWin64)]
{$ENDIF}
  TMSTransaction = class(TCustomMSTransaction)
  private
    function GetIsolationLevel: TIsolationLevel;
    procedure SetIsolationLevel(Value: TIsolationLevel);
  public
    constructor Create(AOwner: TComponent); override;

    function AddConnection(Connection: TCustomDAConnection): integer;
    procedure RemoveConnection(Connection: TCustomDAConnection);

    property ConnectionsCount;
  published
    property IsolationLevel: TIsolationLevel read GetIsolationLevel write SetIsolationLevel default ilReadCommitted;
    property DefaultCloseAction;
    property OnError;
    property OnStart;
    property OnCommit;
    property OnRollback;
  end;

implementation

uses
  CRAccess, OLEDBAccess, MSSQLMonitor;

{ TMSTransaction }

constructor TMSTransaction.Create(AOwner: TComponent);
begin
  inherited;

  TransactionType := ttMTS;
end;

function TMSTransaction.AddConnection(Connection: TCustomDAConnection): integer;
begin
  Result := DoAddConnection(Connection);
end;

procedure TMSTransaction.RemoveConnection(Connection: TCustomDAConnection);
begin
  DoRemoveConnection(Connection);
end;

function TMSTransaction.GetIsolationLevel: TIsolationLevel;
begin
  Assert(inherited IsolationLevel <> ilCustom);
  
  Result := TIsolationLevel(inherited IsolationLevel);
end;

procedure TMSTransaction.SetIsolationLevel(Value: TIsolationLevel);
begin
  inherited IsolationLevel := TCRIsolationLevel(Value); 
end;

end.
