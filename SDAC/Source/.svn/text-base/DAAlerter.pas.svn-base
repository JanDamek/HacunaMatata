
//////////////////////////////////////////////////
//  DB Access Components
//  Copyright © 1998-2012 Devart. All right reserved.
//  TDAAlerter
//////////////////////////////////////////////////

{$IFNDEF CLR}

{$I Dac.inc}

unit DAAlerter;
{$ENDIF}

interface

uses
  Classes, SysUtils, DB, {$IFDEF VER6P}Variants,{$ENDIF}
  CRTypes, CRAccess, DBAccess;

type
  TDAAlerter = class;

  TAlerterEventEvent = procedure (Sender: TDAAlerter; const EventName, Message: _string) of object;
  TAlerterErrorEvent = procedure (Sender: TDAAlerter; E: Exception) of object;

{ TDAAlerter }

  TDAAlerter = class(TComponent)
  private
    procedure SetConnection(Value: TCustomDAConnection);
    procedure SetActive(Value: boolean);
    procedure SetAutoRegister(Value: boolean);
    function GetEvents: _string;
    procedure SetEvents(const Value: _string);
    procedure SetAutoCommit(Value: boolean);
    procedure ConnectChange(Sender: TObject; Connecting: boolean);
    function CanRegisterEvents: boolean;

  protected
    FDesignCreate: boolean;
    FIAlerter: TCRAlerter;
    FConnection: TCustomDAConnection;
    FTransaction: TDATransaction;
    FActive: boolean;
    FStreamedActive: boolean;
    FAutoRegister: boolean;
    FEvents: _TStrings;
    FAutoCommit: boolean;
    FOnEvent: TAlerterEventEvent;
    FOnError: TAlerterErrorEvent;

    procedure Loaded; override;
    procedure Notification(Component: TComponent; Operation: TOperation); override;
    procedure AssignTo(Dest: TPersistent); override;

    function GetInternalAlerterClass: TCRAlerterClass; virtual;
    procedure SetInternalAlerter(Value: TCRAlerter); virtual;
    procedure CreateInternalAlerter;
    procedure FreeInternalAlerter;
    procedure CheckInternalAlerter;

    function IsTransactionStored: boolean;
    function GetTransaction: TDATransaction; virtual;
    procedure SetTransaction(Value: TDATransaction); virtual;
    function UsedConnection: TCustomDAConnection; virtual;
    function UsedTransaction: TDATransaction; virtual;

    procedure BeginConnection(WithTransaction: boolean = True); virtual;
    procedure EndConnection(WithTransaction: boolean = True);
    procedure CommitData;

    procedure DoOnEvent(const EventName, Message: _string); virtual;
    procedure DoOnError(E: Exception);

    property Transaction: TDATransaction read GetTransaction write SetTransaction stored IsTransactionStored;
    property Events: _string read GetEvents write SetEvents;
    property AutoCommit: boolean read FAutoCommit write SetAutoCommit default True;
    property OnEvent: TAlerterEventEvent read FOnEvent write FOnEvent;

  public
    constructor Create(Owner: TComponent); override;
    destructor Destroy; override;

    procedure SendEvent(const EventName, Message: _string);
    procedure Start;
    procedure Stop;

    property Connection: TCustomDAConnection read FConnection write SetConnection;
    property Active: boolean read FActive write SetActive default False;
    property AutoRegister: boolean read FAutoRegister write SetAutoRegister default False;

    property OnError: TAlerterErrorEvent read FOnError write FOnError;
  end;

{ TDAAlerterUtils }

  TDAAlerterUtils = class
  public
    class procedure SetDesignCreate(Obj: TDAAlerter; Value: boolean);
    class function GetDesignCreate(Obj: TDAAlerter): boolean;
    class function GetTransaction(Obj: TDAAlerter): TDATransaction;
    class procedure SetTransaction(Obj: TDAAlerter; Value: TDATransaction);
    class function GetFTransaction(Obj: TDAAlerter): TDATransaction;
  end;

implementation

uses
{$IFDEF VER5}
  MemData,
{$ENDIF}
  DAConsts;

{ TDAAlerter }

constructor TDAAlerter.Create(Owner: TComponent);
begin
  inherited;

  FAutoCommit := True;
  FEvents := _TStringList.Create;
  FDesignCreate := csDesigning in ComponentState;
end;

destructor TDAAlerter.Destroy;
begin
  Stop;
  if FConnection <> nil then
    TDBAccessUtils.UnRegisterClient(FConnection, Self);

  FIAlerter.Free;
  FEvents.Free;

  inherited;
end;

procedure TDAAlerter.SendEvent(const EventName, Message: _string);
begin
  BeginConnection;
  try
    FIAlerter.SendEvent(EventName, Message);
    CommitData;
  finally
    EndConnection;
  end;
end;

procedure TDAAlerter.Start;
begin
  if FActive then
    exit;

  if FEvents.Count = 0 then
    raise Exception.Create(SEventsNotDefined);

  BeginConnection(False);
  try
    if not (csDesigning in ComponentState) then
      FIAlerter.Start;
    FActive := True;
  except
    EndConnection(False);
    raise;
  end;
end;

procedure TDAAlerter.Stop;
begin
  if not FActive then
    exit;

  if not (csDesigning in ComponentState) then
    FIAlerter.Stop;

  EndConnection(False);
  FActive := False;
end;

procedure TDAAlerter.Loaded;
var
  UsedConnection: TCustomDAConnection;

  function CheckKeepConnected: boolean;
  begin
    Result := True;
    if not UsedConnection.Options.KeepDesignConnected then
      Result := False
    else
      if TDBAccessUtils.GetStreamedConnected(UsedConnection) and (not UsedConnection.Connected) then
        TDBAccessUtils.Loaded(UsedConnection);
  end;

begin
  inherited;

  FDesignCreate := False;

  UsedConnection := Self.UsedConnection;
  try
    if (FStreamedActive or (FAutoRegister and CanRegisterEvents)) and
      (UsedConnection <> nil) and (CheckKeepConnected or
      (csDesigning in ComponentState))
    then
      Active := True;
  except
    if csDesigning in ComponentState then
      ApplicationHandleException(Self)
    else
      raise;
  end;
end;

procedure TDAAlerter.Notification(Component: TComponent; Operation: TOperation);
begin
  if Operation = opRemove then begin
    if Component = FConnection then
      Connection := nil
    else
    if Component = FTransaction then
      Transaction := nil
  end;

  inherited;
end;

procedure TDAAlerter.AssignTo(Dest: TPersistent);
begin
  if Dest is TDAAlerter then begin
    TDAAlerter(Dest).Connection := Connection;
    TDAAlerter(Dest).Transaction := Transaction;
    TDAAlerter(Dest).Events := Events;
    TDAAlerter(Dest).AutoCommit := AutoCommit;
    TDAAlerter(Dest).AutoRegister := AutoRegister;
  end
  else
    inherited;
end;

function TDAAlerter.GetInternalAlerterClass: TCRAlerterClass;
begin
  Assert(False);
  Result := TCRAlerter;
end;

procedure TDAAlerter.SetInternalAlerter(Value: TCRAlerter);
begin
  if Value <> FIAlerter then begin
    FreeInternalAlerter;

    FIAlerter := Value;
    if FIAlerter <> nil then begin
      FIAlerter.EventNames.Assign(FEvents);
      FIAlerter.SetProp(prAutoCommit, FAutoCommit);
      FIAlerter.OnEvent := DoOnEvent;
      FIAlerter.OnError := DoOnError;
    end;
  end;
end;

procedure TDAAlerter.CheckInternalAlerter;
begin
  if not (FIAlerter is GetInternalAlerterClass) then begin
    FreeInternalAlerter;
    CreateInternalAlerter;
  end;
end;

procedure TDAAlerter.CreateInternalAlerter;
begin
  SetInternalAlerter(GetInternalAlerterClass.Create);
end;

procedure TDAAlerter.FreeInternalAlerter;
begin
  FIAlerter.Free;
  FIAlerter := nil;
end;

function TDAAlerter.IsTransactionStored: boolean;
begin
  Result := FTransaction <> nil;
end;

function TDAAlerter.GetTransaction: TDATransaction;
begin
  Result := FTransaction;
end;

procedure TDAAlerter.SetTransaction(Value: TDATransaction);
begin
  if Value <> FTransaction then begin
    if FTransaction <> nil then
      RemoveFreeNotification(FTransaction);

    FTransaction := Value;

    if FTransaction <> nil then
      FreeNotification(FTransaction);
  end;
end;

function TDAAlerter.UsedConnection: TCustomDAConnection;
begin
  Result := FConnection;
end;

function TDAAlerter.UsedTransaction: TDATransaction;
var
  UsedConnection: TCustomDAConnection;
begin
  UsedConnection := Self.UsedConnection;
  if UsedConnection <> nil then begin
    if TDBAccessUtils.IsMultipleTransactionsSupported(UsedConnection) then
      Result := Transaction
    else
      Result := nil;

    if Result = nil then
      Result := TDBAccessUtils.UsedTransaction(UsedConnection);
  end
  else
    Result := nil;
end;

procedure TDAAlerter.BeginConnection(WithTransaction: boolean);
var
  vUsedConnection: TCustomDAConnection;
  vUsedTransaction: TDATransaction;
begin
  vUsedConnection := UsedConnection;
  if vUsedConnection = nil then
    raise Exception.Create(SConnectionNotDefined);

  TDBAccessUtils.InternalConnect(vUsedConnection);
  CheckInternalAlerter;
  FIAlerter.Connection := TDBAccessUtils.GetIConnection(vUsedConnection);

  if WithTransaction then 
    if TDBAccessUtils.IsMultipleTransactionsSupported(vUsedConnection) then begin
      vUsedTransaction := UsedTransaction;
      if vUsedTransaction = nil then
        DatabaseError(STransactionNotAssigned);

      TDBAccessUtils.GainTransaction(vUsedTransaction);
      FIAlerter.Transaction := TDBAccessUtils.GetITransaction(vUsedTransaction);
    end;
end;

procedure TDAAlerter.EndConnection(WithTransaction: boolean);
var
  vUsedConnection: TCustomDAConnection;
  vUsedTransaction: TDATransaction;
begin
  vUsedConnection := UsedConnection;

  if WithTransaction then
    if TDBAccessUtils.IsMultipleTransactionsSupported(vUsedConnection) then begin
      vUsedTransaction := UsedTransaction;
      TDBAccessUtils.ReleaseTransaction(vUsedTransaction);
    end;

  TDBAccessUtils.InternalDisconnect(vUsedConnection);
end;

procedure TDAAlerter.CommitData;
var
  AutoCommitUsed: boolean;
  Connection: TCustomDAConnection;
begin
  Connection := UsedConnection;
  AutoCommitUsed := TDBAccessUtils.GetAutoCommit(Connection) and AutoCommit;
  if TDBAccessUtils.IsMultipleTransactionsSupported(Connection) then
    TDBAccessUtils.AutoCommitTransaction(UsedTransaction, AutoCommitUsed);
end;

procedure TDAAlerter.DoOnEvent(const EventName, Message: _string);
begin
  if Assigned(FOnEvent) then
    FOnEvent(Self, EventName, Message);
end;

procedure TDAAlerter.DoOnError(E: Exception);
begin
  FActive := False;
  EndConnection(False);

  if Assigned(FOnError) then
    FOnError(Self, E);
end;

procedure TDAAlerter.SetConnection(Value: TCustomDAConnection);
begin
  if FConnection <> Value then begin
    Stop;
    if FConnection <> nil then begin
      TDBAccessUtils.UnRegisterClient(FConnection, Self);
      FConnection.RemoveFreeNotification(Self);
    end;
    FConnection := Value;
    if FConnection <> nil then begin
      FConnection.FreeNotification(Self);
      TDBAccessUtils.RegisterClient(FConnection, Self, ConnectChange);
    end;
  end;
end;

procedure TDAAlerter.SetActive(Value: boolean);
begin
  if csReading in ComponentState then begin
     FStreamedActive := Value;
  end
  else
    if Value <> FActive then
      if Value then
        Start
      else
        Stop;
end;

procedure TDAAlerter.SetAutoRegister(Value: boolean);
begin
  FAutoRegister := Value;
  if FAutoRegister and CanRegisterEvents then
    Start;
end;

function TDAAlerter.GetEvents: _string;
begin
  Result := DefaultSQLInfo.NamesFromList(FEvents);
end;

procedure TDAAlerter.SetEvents(const Value: _string);
begin
  if Value <> Events then begin
    Stop;
    DefaultSQLInfo.NamesToList(Value, FEvents);
    if FIAlerter <> nil then
      FIAlerter.EventNames.Assign(FEvents);
  end;
end;

procedure TDAAlerter.SetAutoCommit(Value: boolean);
begin
  if Value <> FAutoCommit then begin
    FAutoCommit := Value;
    if FIAlerter <> nil then
      FIAlerter.SetProp(prAutoCommit, Value);
  end;
end;

procedure TDAAlerter.ConnectChange(Sender: TObject; Connecting: boolean);
begin
  if Connecting and FAutoRegister then
    Start;
  if not Connecting then
    Stop;
end;

function TDAAlerter.CanRegisterEvents: Boolean;
var
  UsedConnection: TCustomDAConnection;
begin
  UsedConnection := Self.UsedConnection;

  Result := (not (csDesigning in ComponentState)) and
    (not (csLoading in ComponentState)) and
    (not FActive) and (UsedConnection <> nil) and UsedConnection.Connected;
end;

{ TDAAlerterUtils }

class procedure TDAAlerterUtils.SetDesignCreate(Obj: TDAAlerter; Value: boolean);
begin
  Obj.FDesignCreate := Value;
end;

class function TDAAlerterUtils.GetDesignCreate(Obj: TDAAlerter): boolean;
begin
  Result := Obj.FDesignCreate;
end;

class function TDAAlerterUtils.GetTransaction(Obj: TDAAlerter): TDATransaction;
begin
  Result := Obj.Transaction;
end;

class procedure TDAAlerterUtils.SetTransaction(Obj: TDAAlerter; Value: TDATransaction);
begin
  Obj.Transaction := Value;
end;

class function TDAAlerterUtils.GetFTransaction(Obj: TDAAlerter): TDATransaction;
begin
  Result := Obj.FTransaction;
end;

end.