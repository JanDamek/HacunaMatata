
//////////////////////////////////////////////////
//  SQL Server Data Access Components
//  Copyright © 1998-2012 Devart. All right reserved.
//////////////////////////////////////////////////

{$IFNDEF CLR}
{$IFNDEF UNIDACPRO}

{$I Sdac.inc}

unit MSConnectionPool;

{$ENDIF}
{$ENDIF}

interface

uses
{$IFDEF VER6P}
  Variants,
{$ENDIF}
  Classes, CRConnectionPool, CRAccess, CRTypes, DASQLMonitor,
{$IFNDEF UNIDACPRO}
  OLEDBAccess;
{$ELSE}
  OLEDBAccessUni;
{$ENDIF}

type
  TMSConnectionParameters = class(TCRConnectionParameters)
  protected
    procedure AssignTo(Dest: TPersistent); override;
    function ConnectParamsToString: _string; override;

  public
    Database: _string;
    IsolationLevel: TCRIsolationLevel;
    Authentication: TMSAuthentication;
    Provider: TOLEDBProvider;

    QuotedIdentifier: boolean;
    Language: _string;
    Encrypt: boolean;
    PersistSecurityInfo: boolean;
    AutoTranslate: boolean;
    NetworkLibrary: _string;
    ApplicationName: _string;
    WorkstationID: _string;
    PacketSize: integer;
    TrustServerCertificate: boolean;

    function Equals(Obj: TCRConnectionParameters): boolean; override;
    function SetProp(Prop: integer; const Value: variant): boolean; override;
  end;

  TMSLocalConnectionPool = class(TCRLocalConnectionPool)
  protected
    function CreateNewConnector: TCRConnection; override;
  end;


  TMSConnectionPoolManager = class(TCRConnectionPoolManager)
  protected
    function CreateCRConnectionPool(ConnectionParameters: TCRConnectionParameters): TCRConnectionPool; override;
  public
    class procedure Clear;  {$IFDEF CLR}static;{$ENDIF}
    class function GetConnection(ConnectionParameters: TCRConnectionParameters; SQLMonitorClass: TDASQLMonitorClass): TCRConnection; override;
  end;

implementation

uses
{$IFDEF MSWINDOWS}
  Windows,
{$ENDIF}
  SysUtils, SyncObjs, CRFunctions, MemData;

var
  ConnectionPoolManager: TMSConnectionPoolManager;
  LockPoolManagerCreate: TCriticalSection;

{ TMSConnectionParameters}

procedure TMSConnectionParameters.AssignTo(Dest: TPersistent);
begin
  if Dest is TMSConnectionParameters then begin
    TMSConnectionParameters(Dest).Database := Database;
    TMSConnectionParameters(Dest).IsolationLevel := IsolationLevel;
    TMSConnectionParameters(Dest).Authentication := Authentication;
    TMSConnectionParameters(Dest).Provider := Provider;

    TMSConnectionParameters(Dest).QuotedIdentifier := QuotedIdentifier;
    TMSConnectionParameters(Dest).Language := Language;
    TMSConnectionParameters(Dest).Encrypt := Encrypt;
    TMSConnectionParameters(Dest).PersistSecurityInfo := PersistSecurityInfo;
    TMSConnectionParameters(Dest).AutoTranslate := AutoTranslate;
    TMSConnectionParameters(Dest).NetworkLibrary := NetworkLibrary;
    TMSConnectionParameters(Dest).ApplicationName := ApplicationName;
    TMSConnectionParameters(Dest).WorkstationID := WorkstationID;
    TMSConnectionParameters(Dest).PacketSize := PacketSize;
    TMSConnectionParameters(Dest).TrustServerCertificate := TrustServerCertificate;
  end;

  inherited;
end;

function TMSConnectionParameters.ConnectParamsToString: _string;
begin
  Result := inherited ConnectParamsToString + _Format(
    'Database=%s'#13,
    [Database]);
end;

function TMSConnectionParameters.Equals(Obj: TCRConnectionParameters): boolean;
var
  O: TMSConnectionParameters;
begin
  Result := inherited Equals(Obj);
  if Result and (Obj is TMSConnectionParameters) then begin
    O := TMSConnectionParameters(obj);
    Result :=
      (_CompareText(Database, O.Database) = 0) and
      (O.IsolationLevel = IsolationLevel) and
      (O.Authentication = Authentication) and
      (O.Provider = Provider) and
      (O.QuotedIdentifier = QuotedIdentifier) and
      (_CompareText(O.Language, Language) = 0)and
      (O.Encrypt = Encrypt) and
      (O.PersistSecurityInfo = PersistSecurityInfo) and
      (O.AutoTranslate = AutoTranslate) and
      (_CompareText(O.NetworkLibrary, NetworkLibrary) = 0) and
      (_CompareText(O.ApplicationName, ApplicationName) = 0) and
      (_CompareText(O.WorkstationID, WorkstationID) = 0) and
      (O.PacketSize = PacketSize) and
      (O.TrustServerCertificate = TrustServerCertificate);
  end;
end;

function TMSConnectionParameters.SetProp(Prop: integer; const Value: variant): boolean;
begin
  Result := True;
  case Prop of
    prDatabase:
      Database := Value;
    prIsolationLevel:
      IsolationLevel := TCRIsolationLevel(Value);
    prAuthentication:
      Authentication := TMSAuthentication(Value);
    prProvider:
      Provider := TOLEDBProvider(Value);
    prQuotedIdentifier:
      QuotedIdentifier := Value;
    prLanguage:
      Language := Value;
    prEncrypt:
      Encrypt := Value;
    prPersistSecurityInfo:
      PersistSecurityInfo := Value;
    prAutoTranslate:
      AutoTranslate := Value;
    prNetworkLibrary:
      NetworkLibrary := Value;
    prApplicationName:
      ApplicationName := Value;
    prWorkstationID:
      WorkstationID := Value;
    prPacketSize:
      PacketSize := Value;
    prTrustServerCertificate:
      TrustServerCertificate := Value;
  else
    Result := inherited SetProp(Prop, Value);
  end;
end;

{ TMSLocalConnectionPool }

function TMSLocalConnectionPool.CreateNewConnector: TCRConnection;
begin
  Result := TOLEDBConnection.Create;

  Result.SetProp(prDatabase, TMSConnectionParameters(ConnectionParameters).Database);
  Result.SetProp(prAuthentication, Variant(TMSConnectionParameters(ConnectionParameters).Authentication));
  Result.SetProp(prProvider, Variant(TMSConnectionParameters(ConnectionParameters).Provider));

  Result.SetProp(prQuotedIdentifier, TMSConnectionParameters(ConnectionParameters).QuotedIdentifier);
  Result.SetProp(prLanguage, TMSConnectionParameters(ConnectionParameters).Language);
  Result.SetProp(prEncrypt, TMSConnectionParameters(ConnectionParameters).Encrypt);
  Result.SetProp(prPersistSecurityInfo, TMSConnectionParameters(ConnectionParameters).PersistSecurityInfo);
  Result.SetProp(prAutoTranslate, TMSConnectionParameters(ConnectionParameters).AutoTranslate);
  Result.SetProp(prNetworkLibrary, TMSConnectionParameters(ConnectionParameters).NetworkLibrary);
  Result.SetProp(prApplicationName, TMSConnectionParameters(ConnectionParameters).ApplicationName);
  Result.SetProp(prWorkstationID, TMSConnectionParameters(ConnectionParameters).WorkstationID);
  Result.SetProp(prPacketSize, TMSConnectionParameters(ConnectionParameters).PacketSize);
  Result.SetProp(prTrustServerCertificate, TMSConnectionParameters(ConnectionParameters).TrustServerCertificate);
  Result.OnError := ConnectionParameters.OnError;

  Result.GetInternalTransaction.SetProp(prIsolationLevel, Variant(TMSConnectionParameters(ConnectionParameters).IsolationLevel));

  StartWait;
  try
    Result.SetUsername(ConnectionParameters.Username);
    Result.SetPassword(ConnectionParameters.Password);
    Result.SetServer(ConnectionParameters.Server);

    Result.Connect('');
  finally
    StopWait
  end;
end;

{ TMSConnectionPoolManager }

function TMSConnectionPoolManager.CreateCRConnectionPool(ConnectionParameters: TCRConnectionParameters): TCRConnectionPool;
begin
  Result := TMSLocalConnectionPool.Create(Self, ConnectionParameters);
end;

class procedure TMSConnectionPoolManager.Clear;
begin
  if ConnectionPoolManager <> nil then
    ConnectionPoolManager.InternalClear;
end;

class function TMSConnectionPoolManager.GetConnection(ConnectionParameters: TCRConnectionParameters;
  SQLMonitorClass: TDASQLMonitorClass): TCRConnection;
begin
  LockPoolManagerCreate.Enter;
  try
    if ConnectionPoolManager = nil then begin
      ConnectionPoolManager := TMSConnectionPoolManager.Create;
      ConnectionPoolManager.SQLMonitorClass := SQLMonitorClass;
    end;
  finally
    LockPoolManagerCreate.Leave;
  end;

  Result := ConnectionPoolManager.InternalGetConnection(ConnectionParameters);
end;

{$IFDEF WIN32}
{$IFNDEF FPC}
{$IFNDEF VER6P}
type
  TDLLProc = procedure (Reason: Integer);
{$ENDIF}

var
  OldDLLProc: TDLLProc;

procedure LibraryProc(Reason: integer);
begin
  if Reason = DLL_PROCESS_DETACH then begin
    ConnectionPoolManager.Free;
    ConnectionPoolManager := nil;
  end;
  if Assigned(OldDLLProc) then
    OldDLLProc(Reason);
end;
{$ENDIF}
{$ENDIF}

initialization
{$IFNDEF FPC}
{$IFDEF WIN32}
  OldDLLProc := DLLProc;
  DLLProc := @LibraryProc;
{$ENDIF}
{$ENDIF}
  ConnectionPoolManager := nil;
  LockPoolManagerCreate := TCriticalSection.Create;

finalization
  ConnectionPoolManager.Free;
  ConnectionPoolManager := nil;
  LockPoolManagerCreate.Free;

end.
