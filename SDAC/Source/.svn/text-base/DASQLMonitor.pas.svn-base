
//////////////////////////////////////////////////
//  DB Access Components
//  Copyright © 1998-2012 Devart. All right reserved.
//  SQLMonitor supports
//  Created:            17.11.99
//////////////////////////////////////////////////

{$IFNDEF CLR}

{$I Dac.inc}

{$IFDEF BCB}
  {$O-}
{$ENDIF}

unit DASQLMonitor;
{$ENDIF}

interface

uses
  Classes, SysUtils, DB, {$IFDEF VER6P}Types,{$ENDIF}
  CRTypes, MemData, 
  DBAccess, DBMonitorClient, DBMonitorMessages;

type
  TDATraceFlag = (tfQPrepare, tfQExecute, tfQFetch, tfError, tfStmt, tfConnect,
    tfTransact, tfBlob, tfService, tfMisc, tfParams, tfObjDestroy, tfPool);
  TDATraceFlags = set of TDATraceFlag;

  TMonitorOption = (moDialog, moSQLMonitor, moDBMonitor, moCustom, moHandled);
  TMonitorOptions = set of TMonitorOption;

  TOnSQLEvent = procedure (Sender: TObject; Text: _string; Flag: TDATraceFlag) of object;

  TDBMonitorOptions = class(TPersistent)
  private
    FHost: string;
    FPort: integer;
    FReconnectTimeout: integer;
    FSendTimeout: integer;

  protected
    procedure AssignTo(Dest: TPersistent); override;

  public
    constructor Create;

  published
    property Host: string read FHost write FHost;
    property Port: integer read FPort write FPort default DBMonitorPort;
    property ReconnectTimeout: integer read FReconnectTimeout write FReconnectTimeout default DefaultReconnectTimeout;
    property SendTimeout: integer read FSendTimeout write FSendTimeout default DefaultSendTimeout;
  end;

{ TCustomDASQLMonitor }

  TDASQLMonitorClass = class of TCustomDASQLMonitor;

  TCustomDASQLMonitor = class(TComponent)
  private
    FActive: boolean;
    FTraceFlags: TDATraceFlags;
    FOptions: TMonitorOptions;
    FStreamedActive: boolean;
  {$IFDEF WIN32_64}
    FRegistered: boolean;
    FSMClient: IUnknown;
  {$ENDIF}
    FDBMonitor: TDBMonitor;
    FDBMonitorOptions: TDBMonitorOptions;

    procedure SetActive(Value: boolean);
    procedure SetOptions(Value: TMonitorOptions);
    procedure SetDBMonitorOptions(Value: TDBMonitorOptions);

  protected
    FOnSQLEvent: TOnSQLEvent;

  { component routines }
    procedure Loaded; override;
    procedure CheckActive;
  { Borland's SQL Monitor support }
    procedure RegisterClient; virtual;
    procedure UnRegisterClient; virtual;
    procedure AddStatement(St: string);
    procedure SMClientSignal(Sender: TObject; Data: Integer);
    function NeedAutoActivate: boolean; virtual;

  { DBMonitor support }
    procedure StartDBMonitor;
    procedure SendDBMonitorEvent(BeforeEvent: boolean;
      EventType: integer; const Description: string; Obj: TObject;
      const SQL: string; const Params: TSQLParams;
      Failed: boolean; const ErrorText: string;
      var EventID: Cardinal);

    function GetObjectHandle(Obj: TObject): string;
    function GetParent(Obj: TObject): TObject; virtual;
    function GetObjectType(Obj: TObject): integer; virtual;

    procedure InternalSQLPrepare(Obj: TObject; const SQL: _string; Params: TDAParams; BeforeEvent: boolean; var MessageID: Cardinal);
    procedure InternalSQLUnprepare(Obj: TObject; const SQL: _string; Params: TDAParams; BeforeEvent: boolean; var MessageID: Cardinal);
    procedure InternalSQLExecute(Obj: TObject; const SQL: _string; Params: TDAParams; const Caption: _string; BeforeEvent: boolean; var MessageID: Cardinal); virtual;
    procedure InternalDBConnect(Connection: TCustomDAConnection; BeforeEvent: boolean; var MessageID: Cardinal);
    procedure InternalDBDisconnect(Connection: TCustomDAConnection; BeforeEvent: boolean; var MessageID: Cardinal);
    procedure InternalTRStart(Obj: TObject; BeforeEvent: boolean; var MessageID: Cardinal); virtual;
    procedure InternalTRCommit(Obj: TObject; BeforeEvent: boolean; var MessageID: Cardinal); virtual;
    procedure InternalTRRollback(Obj: TObject; BeforeEvent: boolean; var MessageID: Cardinal); virtual;
    procedure InternalTRSavepoint(Obj: TObject; const Savepoint: _string; BeforeEvent: boolean; var MessageID: Cardinal);
    procedure InternalTRRollbackToSavepoint(Obj: TObject; const Savepoint: _string; BeforeEvent: boolean; var MessageID: Cardinal);
    procedure InternalTRReleaseSavepoint(Obj: TObject; const Savepoint: _string; BeforeEvent: boolean; var MessageID: Cardinal);
    procedure InternalTRCommitRetaining(Obj: TObject; BeforeEvent: boolean; var MessageID: Cardinal);
    procedure InternalTRRollbackRetaining(Obj: TObject; BeforeEvent: boolean; var MessageID: Cardinal);
    procedure InternalDBError(Exception: EDAError);
    procedure InternalCustomMessage(Obj: TObject; const Msg: _string);
    procedure InternalObjectDestroyed(Obj: TObject);
    procedure InternalPoolMessage(Obj: TObject; const Msg: _string; WithCount: boolean; WithParams: boolean);

    class function GetMonitor: TCustomDASQLMonitor; virtual;
    procedure SetMonitor;virtual; abstract;

  public
    class procedure SQLPrepare(Obj: TObject; const SQL: _string; Params: TDAParams; var MessageID: Cardinal; BeforeEvent: boolean);
    class procedure SQLUnprepare(Obj: TObject; const SQL: _string; Params: TDAParams; var MessageID: Cardinal; BeforeEvent: boolean);
    class procedure SQLExecute(Obj: TObject; const SQL: _string; Params: TDAParams; const Caption: string; var MessageID: Cardinal; BeforeEvent: boolean);
    class procedure DBConnect(Connection: TCustomDAConnection; var MessageID: Cardinal; BeforeEvent: boolean);
    class procedure DBDisconnect(Connection: TCustomDAConnection; var MessageID: Cardinal; BeforeEvent: boolean);

    class procedure TRStart(Obj: TObject; var MessageID: Cardinal; BeforeEvent: boolean); virtual;
    class procedure TRCommit(Obj: TObject; var MessageID: Cardinal; BeforeEvent: boolean); virtual;
    class procedure TRRollback(Obj: TObject; var MessageID: Cardinal; BeforeEvent: boolean); virtual;
    class procedure TRSavepoint(Obj: TObject; const Savepoint: _string; var MessageID: Cardinal; BeforeEvent: boolean);
    class procedure TRRollbackToSavepoint(Obj: TObject; const Savepoint: _string; var MessageID: Cardinal; BeforeEvent: boolean);
    class procedure TRReleaseSavepoint(Obj: TObject; const Savepoint: _string; var MessageID: Cardinal; BeforeEvent: boolean);
    class procedure TRCommitRetaining(Obj: TObject; var MessageID: Cardinal; BeforeEvent: boolean);
    class procedure TRRollbackRetaining(Obj: TObject; var MessageID: Cardinal; BeforeEvent: boolean);

    class procedure DBError(Exception: EDAError);
    class procedure CustomMessage(Obj: TObject; const Msg: _string);
    class procedure ObjectDestroyed(Obj: TObject);
    class procedure PoolMessage(Obj: TObject; const Msg: _string; WithCount: boolean; WithParams: boolean = False);

    class function HasMonitor: boolean;
    class function GetParamDataType(Param: TDAParam): string; virtual;
    class function GetParamParamType(Param: TDAParam): string; virtual;
    class function GetParamValue(Param: TDAParam): string; virtual;
    class function GetParam(Param: TDAParam; var SQLParam: TSQLParam): _string;
    class function GetParams(Params: TDAParams; var SQLParams: TSQLParams): _string; overload;
    class function GetParams(Params: TDAParams): string; overload;
    class function GetCaption: string; virtual;
    class procedure ShowDebug(Obj: TObject; const SQL: _string; Params: TDAParams; const Caption: string);

  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    property Active: boolean read FActive write SetActive default True;
    property TraceFlags: TDATraceFlags read FTraceFlags write FTraceFlags default [tfQPrepare, tfQExecute, tfError, tfConnect, tfTransact, tfParams, tfMisc];
    property Options: TMonitorOptions read FOptions write SetOptions default [moDialog, moSQLMonitor, moDBMonitor, moCustom];
    property DBMonitorOptions: TDBMonitorOptions read FDBMonitorOptions write SetDBMonitorOptions;

    property OnSQL: TOnSQLEvent read FOnSQLEvent write FOnSQLEvent;
  end;

var
  ShowDebugFormProc: procedure (DASQLMonitorClass: TDASQLMonitorClass;
    Component: TComponent; SQL: string; Params: TDAParams; Caption: string);
  GetCallStackProc: procedure (var ACallStack: TWideStringDynArray);

  function GetObjectID(Obj: TObject): cardinal;
  function GetObjectName(Obj: TObject): string;
  
implementation

uses
{$IFDEF WIN32_64}
  ActiveX,
{$ENDIF}
  CRFunctions, CRConnectionPool, MemUtils,{$IFDEF FPC}MemDataSet{$ELSE}MemDS{$ENDIF};


{$IFDEF WIN32_64}
const
  Class_SMClient: TGUID = '{CB9879E2-4395-11D0-9FFC-00A0248E4B9A}';

type
  ISMClient = interface(IUnknown)
    ['{CB9879E1-4395-11D0-9FFC-00A0248E4B9A}']
    function RegisterClient(ID: Integer; Name: PChar;
      Instance, SignalProc: Pointer): WordBool; stdcall;
    function AddStatement(Statement: PChar; Len: Integer): WordBool; stdcall;
  end;
{$ENDIF}


function GetObjectID(Obj: TObject): cardinal;
begin
{$IFDEF CLR}
  if Obj = nil then
    Result := 0
  else
{$ENDIF}
    Result := Cardinal(Obj{$IFDEF CLR}.GetHashCode{$ENDIF});
end;

function GetObjectName(Obj: TObject): string;
begin
  if (Obj is TComponent) and (TComponent(Obj).Name <> '') then
    Result := TComponent(Obj).Name
  else
    Result := Obj.ClassName + ' ($' + IntToHex(GetObjectID(Obj), 8) + ')';
end;

{ TDBMonitorOptions }

constructor TDBMonitorOptions.Create;
begin
  inherited;

  FPort := DBMonitorPort;
  FReconnectTimeout := DefaultReconnectTimeout;
  FSendTimeout := DefaultSendTimeout;
end;

procedure TDBMonitorOptions.AssignTo(Dest: TPersistent);
begin
  if Dest is TDBMonitorOptions then begin
    TDBMonitorOptions(Dest).Host := Host;
    TDBMonitorOptions(Dest).Port := Port;
    TDBMonitorOptions(Dest).ReconnectTimeout := ReconnectTimeout;
    TDBMonitorOptions(Dest).SendTimeout := SendTimeout;
  end;
end;

{ TCustomDASQLMonitor }

constructor TCustomDASQLMonitor.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);

  FActive := False;
  FOptions := [moDialog, moSQLMonitor, moDBMonitor, moCustom];
  FTraceFlags := [tfQPrepare, tfQExecute, tfError, tfConnect, tfTransact, tfParams, tfMisc];
  FStreamedActive := True;
  FDBMonitorOptions := TDBMonitorOptions.Create;

  SetMonitor;

  if NeedAutoActivate then
    Active := True;
end;

destructor TCustomDASQLMonitor.Destroy;
begin
  Active := False;
  FDBMonitorOptions.Free;

  inherited;
end;

class procedure TCustomDASQLMonitor.SQLPrepare(Obj: TObject;
  const SQL: _string; Params: TDAParams; var MessageID: Cardinal;
  BeforeEvent: boolean);
var
  Monitor: TCustomDASQLMonitor;
begin
  Monitor := GetMonitor;
  if Assigned(Monitor) then
    Monitor.InternalSQLPrepare(Obj, TrimRight(SQL), Params, BeforeEvent, MessageID)
  else
    if BeforeEvent then
      ShowDebug(Obj, TrimRight(SQL), Params, 'Prepare');
end;

class procedure TCustomDASQLMonitor.SQLUnprepare(Obj: TObject;
  const SQL: _string; Params: TDAParams; var MessageID: Cardinal;
  BeforeEvent: boolean);
var
  Monitor: TCustomDASQLMonitor;
begin
  Monitor := GetMonitor;
  if Assigned(Monitor) then
    Monitor.InternalSQLUnprepare(Obj, TrimRight(SQL), Params, BeforeEvent, MessageID)
  else
    if BeforeEvent then
      ShowDebug(Obj, TrimRight(SQL), Params, 'Unprepare');
end;

class procedure TCustomDASQLMonitor.SQLExecute(Obj: TObject;
  const SQL: _string; Params: TDAParams; const Caption: string;
  var MessageID: Cardinal; BeforeEvent: boolean);
var
  Monitor: TCustomDASQLMonitor;
  ACaption: string;
begin
  Monitor := GetMonitor;
  if Assigned(Monitor) then
    Monitor.InternalSQLExecute(Obj, SQL, Params, Caption, BeforeEvent, MessageID)
  else
    begin
      if Caption = '' then
        ACaption := 'Execute'
      else
        ACaption := Caption;

      if BeforeEvent then
        ShowDebug(Obj, SQL, Params, ACaption);
    end;
end;

class procedure TCustomDASQLMonitor.DBConnect(Connection: TCustomDAConnection;
  var MessageID: Cardinal; BeforeEvent: boolean);
var
  Monitor: TCustomDASQLMonitor;
begin
  Monitor := GetMonitor;
  if Assigned(Monitor) then
    Monitor.InternalDBConnect(Connection, BeforeEvent, MessageID)
end;

class procedure TCustomDASQLMonitor.DBDisconnect(Connection: TCustomDAConnection;
 var MessageID: Cardinal; BeforeEvent: boolean);
var
  Monitor: TCustomDASQLMonitor;
begin
  Monitor := GetMonitor;
  if Assigned(Monitor) then
    Monitor.InternalDBDisconnect(Connection, BeforeEvent, MessageId)
end;

class procedure TCustomDASQLMonitor.TRStart(Obj: TObject;
  var MessageID: Cardinal; BeforeEvent: boolean);
var
  Monitor: TCustomDASQLMonitor;
begin
  Monitor := GetMonitor;
  if Assigned(Monitor) then
    Monitor.InternalTRStart(Obj, BeforeEvent, MessageID)
end;

class procedure TCustomDASQLMonitor.TRCommit(Obj: TObject;
  var MessageID: Cardinal; BeforeEvent: boolean);
var
  Monitor: TCustomDASQLMonitor;
begin
  Monitor := GetMonitor;
  if Assigned(Monitor) then
    Monitor.InternalTRCommit(Obj, BeforeEvent, MessageID)
end;

class procedure TCustomDASQLMonitor.TRRollback(Obj: TObject;
  var MessageID: Cardinal; BeforeEvent: boolean);
var
  Monitor: TCustomDASQLMonitor;
begin
  Monitor := GetMonitor;
  if Assigned(Monitor) then
    Monitor.InternalTRRollback(Obj, BeforeEvent, MessageID)
end;

class procedure TCustomDASQLMonitor.TRSavepoint(Obj: TObject; const Savepoint: _string; var MessageID: Cardinal; BeforeEvent: boolean);
var
  Monitor: TCustomDASQLMonitor;
begin
  Monitor := GetMonitor;
  if Assigned(Monitor) then
    Monitor.InternalTRSavepoint(Obj, Savepoint, BeforeEvent, MessageID);
end;

class procedure TCustomDASQLMonitor.TRRollbackToSavepoint(Obj: TObject; const Savepoint: _string; var MessageID: Cardinal; BeforeEvent: boolean);
var
  Monitor: TCustomDASQLMonitor;
begin
  Monitor := GetMonitor;
  if Assigned(Monitor) then
    Monitor.InternalTRRollbackToSavepoint(Obj, Savepoint, BeforeEvent, MessageID);
end;

class procedure TCustomDASQLMonitor.TRReleaseSavepoint(Obj: TObject; const Savepoint: _string; var MessageID: Cardinal; BeforeEvent: boolean);
var
  Monitor: TCustomDASQLMonitor;
begin
  Monitor := GetMonitor;
  if Assigned(Monitor) then
    Monitor.InternalTRReleaseSavepoint(Obj, Savepoint, BeforeEvent, MessageID);
end;

class procedure TCustomDASQLMonitor.TRCommitRetaining(Obj: TObject; var MessageID: Cardinal; BeforeEvent: boolean);
var
  Monitor: TCustomDASQLMonitor;
begin
  Monitor := GetMonitor;
  if Assigned(Monitor) then
    Monitor.InternalTRCommitRetaining(Obj, BeforeEvent, MessageID)
end;

class procedure TCustomDASQLMonitor.TRRollbackRetaining(Obj: TObject; var MessageID: Cardinal; BeforeEvent: boolean);
var
  Monitor: TCustomDASQLMonitor;
begin
  Monitor := GetMonitor;
  if Assigned(Monitor) then
    Monitor.InternalTRRollbackRetaining(Obj, BeforeEvent, MessageID)
end;

class procedure TCustomDASQLMonitor.DBError(Exception: EDAError);
var
  Monitor: TCustomDASQLMonitor;
begin
  Monitor := GetMonitor;
  if Assigned(Monitor) then
    Monitor.InternalDBError(Exception);
end;

class procedure TCustomDASQLMonitor.CustomMessage(Obj: TObject; const Msg: _string);
var
  Monitor: TCustomDASQLMonitor;
begin
  Monitor := GetMonitor;
  if Assigned(Monitor) then
    Monitor.InternalCustomMessage(Obj, Msg);
end;

class procedure TCustomDASQLMonitor.ObjectDestroyed(Obj: TObject);
var
  Monitor: TCustomDASQLMonitor;
begin
  Monitor := GetMonitor;
  if Assigned(Monitor) then
    Monitor.InternalObjectDestroyed(Obj);
end;

class procedure TCustomDASQLMonitor.PoolMessage(Obj: TObject; const Msg: _string;
  WithCount: boolean; WithParams: boolean = False);
var
  Monitor: TCustomDASQLMonitor;
begin
  Monitor := GetMonitor;
  if Assigned(Monitor) then
    Monitor.InternalPoolMessage(Obj, Msg, WithCount, WithParams);
end;

class function TCustomDASQLMonitor.GetParamDataType(Param: TDAParam): string;
begin
{$IFNDEF VER10P}
  if Integer(Param.DataType) = Integer(ftFixedWideChar) then
    Result := 'FixedWideChar'
  else
{$ENDIF}
    Result := FieldTypeNames[Param.DataType];
  if (Param.DataType in [ftString,ftFixedChar,ftWideString]) or
    (Integer(Param.DataType) = Integer(ftFixedWideChar))
  then
    Result := Result + '[' + IntToStr(Length(Param.AsString)) + ']';
end;

class function TCustomDASQLMonitor.GetParamParamType(Param: TDAParam): string;
begin
  case Param.ParamType of
  ptInput:
    Result := 'IN';
  ptOutput:
    Result := 'OUT';
  ptInputOutput:
    Result := 'IN/OUT';
  ptResult:
    Result := 'RESULT';
  else
    Result := '';
  end;
end;

class function TCustomDASQLMonitor.GetParamValue(Param: TDAParam): string;
begin
  Result := '';
  if Param.IsNull then
    Result := '<NULL>'
  else
    case Param.DataType of
      ftDate:
        Result := DateToStr(Param.AsDate);
      ftDateTime:
        Result := DateTimeToStr(Param.AsDateTime);
      ftBlob, ftOraBlob, ftOraClob, ftMemo{$IFDEF VER10P}, ftWideMemo{$ENDIF}: begin
        Result := '<BLOB:' + IntToStr(Param.AsBlobRef.Size) + '>';
      end;
      ftCursor:
        Result := '<CURSOR>';
      ftBytes, ftVarBytes:
        Result := '<BLOB:' + IntToStr(Length(Param.AsBlob)) + '>'
    else
      if (Param.DataType in [ftString, ftFixedChar, ftWideString]) or
        (Integer(Param.DataType) = Integer(ftFixedWideChar))
      then
        Result := '''' + Param.AsString + ''''
      else
        Result := Param.AsString;
    end;
end;

function ConcatWith(const Args: array of string; const AConcatWith: string): string;
var
  i: integer;
begin
  Result := '';
  for i := 0 to Length(Args)-1 do begin
    if Args[i] <> '' then
      if Result <> '' then
        Result := Result + AConcatWith + Args[i]
      else
        Result := Args[i];
  end;
end;

class function TCustomDASQLMonitor.GetParam(Param: TDAParam; var SQLParam: TSQLParam): _string;
var
  DataType, ParamType, Value: _string;
begin
  DataType := GetParamDataType(Param);
  ParamType := GetParamParamType(Param);
  Value := GetParamValue(Param);

  SQLParam.Name := Param.Name;
  SQLParam.DataType := DataType;
  SQLParam.ParamType := ParamType;
  SQLParam.Value := Value;
  Result := ':' + Param.Name + '(' + ConcatWith([DataType,
    ParamType],',') + ConcatWith([')', Value], '=');
end;

class function TCustomDASQLMonitor.GetParams(Params: TDAParams; var SQLParams: TSQLParams): _string;
var
  i: integer;
begin
  Result := '';
  if Params <> nil then begin
    SetLength(SQLParams, Params.Count);
    for i := 0 to Params.Count - 1 do
      Result := Result + GetParam(Params[i], SQLParams[i]) + ' '#13#10;
  end;
end;

class function TCustomDASQLMonitor.GetParams(Params: TDAParams): string;
var
  SQLParams: TSQLParams;
begin
  Result := GetParams(Params, SQLParams);
end;

class procedure TCustomDASQLMonitor.ShowDebug(Obj: TObject;
  const SQL: _string; Params: TDAParams; const Caption: string);
begin
  if Assigned(ShowDebugFormProc) then
    ShowDebugFormProc(Self, TComponent(Obj), SQL, Params, Caption);
end;

class function TCustomDASQLMonitor.GetMonitor: TCustomDASQLMonitor;
begin
  Result := nil;
end;

class function TCustomDASQLMonitor.GetCaption: string;
begin
  Result := 'DataAccess';
end;

function TCustomDASQLMonitor.GetObjectHandle(Obj: TObject): string;
var
  Conn: TCustomDAConnection;
  Name: string;
begin
  if moHandled in Options then begin
    if Obj is TComponent then
      Name := TComponent(Obj).Name
    else
      Name := '';

    Result := ' [' + Name + '$' + IntToHex(GetObjectID(Obj), 8);

    if Obj is TCustomDADataSet then
      Conn := TCustomDADataSet(Obj).Connection
    else
      if Obj is TCustomDASQL then
        Conn := TCustomDASQL(Obj).Connection
      else
        Conn := nil;

    if Conn <> nil then
      Result := Result + '; ' + Conn.Name + '$' + IntToHex(GetObjectID(Conn), 8);

    Result := Result + '] ';
  end
  else
    Result := '';
end;

function TCustomDASQLMonitor.GetParent(Obj: TObject): TObject;
begin
  if (Obj is TDATransaction) then
    Result := TDBAccessUtils.GetConnection(TDATransaction(Obj), 0)
  else
  if (Obj is TCustomDASQL) then
    Result := TCustomDASQL(Obj).Connection
  else
  if (Obj is TCustomDADataSet) then
    Result := TCustomDADataSet(Obj).Connection
  else
  if (Obj is TCRConnectionPool) then
    Result := TCRConnectionPool(Obj).Manager
  else
    Result := nil;
end;

function TCustomDASQLMonitor.GetObjectType(Obj: TObject): integer;
begin
  if (Obj is TCustomDAConnection) then
    Result := OT_CONNECTION
  else
  if (Obj is TDATransaction) then
    Result := OT_TRANSACTION
  else
  if (Obj is TCustomDASQL) then
    Result := OT_COMMAND
  else
  if (Obj is TCustomDADataSet) then
    Result := OT_DATAREADER
  else
  if (Obj is TCRConnectionPool) or (Obj is TCRConnectionPoolManager) then
    Result := OT_CONNPOOL
  else
    Result := OT_UNKNOWN;
end;

procedure TCustomDASQLMonitor.InternalSQLPrepare(Obj: TObject; const SQL: _string; Params: TDAParams; BeforeEvent: boolean; var MessageID: Cardinal);
var
  ParamSt: _string;
  SQLParams: TSQLParams;
  ASQL, Description: _string;
begin
  CheckActive;

  if Active and (tfQPrepare in TraceFlags) then begin
    if ((moDialog in Options) and (not((moCustom in Options) or (csDesigning in ComponentState)) or
     ((Obj is TCustomDADataSet) and TCustomDADataSet(Obj).Debug or
      (Obj is TCustomDASQL) and TCustomDASQL(Obj).Debug))) and
      BeforeEvent
    then
      ShowDebug(Obj, SQL, Params, 'Prepare');

    ParamSt := TrimRight(GetParams(Params, SQLParams));

    if (moSQLMonitor in Options) and BeforeEvent then begin
      AddStatement('SQL Prepare' + GetObjectHandle(Obj) + ': ' + SQL);
      if (ParamSt <> '') and (tfParams in TraceFlags) then
        AddStatement(ParamSt);
    end;

    if moDBMonitor in Options then begin
      Description := 'SQL Prepare' + GetObjectHandle(Obj) + ': ' + SQL + #13#10 + ParamSt;
      SendDBMonitorEvent(BeforeEvent, ET_PREPARE, Description, Obj, SQL,
        SQLParams, False, '', MessageID);
    end;

    if Assigned(FOnSQLEvent) and BeforeEvent then begin
      if (ParamSt <> '') and (tfParams in TraceFlags) then
        ASQL := SQL + #13#10 + ParamSt
      else
        ASQL := SQL;
      FOnSQLEvent(Obj, 'Prepare: ' + ASQL, tfQPrepare);
    end;
  end;
end;

procedure TCustomDASQLMonitor.InternalSQLUnprepare(Obj: TObject; const SQL: _string; Params: TDAParams; BeforeEvent: boolean; var MessageID: Cardinal);
var
  ParamSt: _string;
  SQLParams: TSQLParams;
  ASQL, Description: _string;
begin
  CheckActive;

  if Active and (tfQPrepare in TraceFlags) then begin
    if ((moDialog in Options) and (not ((moCustom in Options) or (csDesigning in ComponentState)) or
     ((Obj is TCustomDADataSet) and TCustomDADataSet(Obj).Debug or
      (Obj is TCustomDASQL) and TCustomDASQL(Obj).Debug))) and
      BeforeEvent
    then
      ShowDebug(Obj, SQL, Params, 'Unprepare');

    ParamSt := TrimRight(GetParams(Params, SQLParams));

    if (moSQLMonitor in Options) then begin
      AddStatement('SQL Unprepare' + GetObjectHandle(Obj) + ': ' + SQL);
      if (ParamSt <> '') and (tfParams in TraceFlags) then
        AddStatement(ParamSt);
    end;

    if moDBMonitor in Options then begin
      Description := 'SQL Unprepare' + GetObjectHandle(Obj) + ': ' + SQL + #13#10 + ParamSt;
      SendDBMonitorEvent(BeforeEvent, ET_UNPREPARE, Description, Obj, SQL,
        SQLParams, False, '', MessageID);
    end;

    if Assigned(FOnSQLEvent) and BeforeEvent then begin
      if (ParamSt <> '') and (tfParams in TraceFlags) then
        ASQL := SQL + #13#10 + ParamSt
      else
        ASQL := SQL;
      FOnSQLEvent(Obj, 'Unprepare: ' + ASQL, tfQPrepare);
    end;
  end;
end;

procedure TCustomDASQLMonitor.InternalSQLExecute(Obj:TObject; const SQL: _string; Params: TDAParams; const Caption: _string; BeforeEvent: boolean; var MessageID: Cardinal);
var
  ParamSt: _string;
  ACaption: _string;
  SQLParams: TSQLParams;
  ASQL, Description: _string;
begin
  CheckActive;

  if Active and (tfQExecute in TraceFlags) then begin
    if (moDialog in Options) and (not ((moCustom in Options) or (csDesigning in ComponentState)) or
     ((Obj is TCustomDADataSet) and TCustomDADataSet(Obj).Debug or
      (Obj is TCustomDASQL) and TCustomDASQL(Obj).Debug)) and
      BeforeEvent
    then begin
      if Caption = '' then
        ACaption := 'Execute'
      else
        ACaption := Caption;

      ShowDebug(Obj, SQL, Params, ACaption);
    end;

    ParamSt := TrimRight(GetParams(Params, SQLParams));

    if (moSQLMonitor in Options) and BeforeEvent then begin
      ACaption := 'SQL Execute';
      if Caption <> '' then
        ACaption := ACaption + ' [' + Caption + ']';

      AddStatement(ACaption + GetObjectHandle(Obj) + ': ' + SQL);
      if (ParamSt <> '') and (tfParams in TraceFlags) then
        AddStatement(ParamSt);
    end;

    if moDBMonitor in Options then begin
      Description := 'SQL Execute' + GetObjectHandle(Obj) + ': ' + SQL + #13#10 + ParamSt;
      SendDBMonitorEvent(BeforeEvent, ET_EXECUTE, Description, Obj, SQL,
        SQLParams, False, '', MessageID);
    end;

    if Assigned(FOnSQLEvent) and BeforeEvent then begin
      if (ParamSt <> '') and (tfParams in TraceFlags) then
        ASQL := SQL + #13#10 + ParamSt
      else
        ASQL := SQL;
      FOnSQLEvent(Obj, ASQL, tfQExecute);
    end;
  end;
end;

procedure TCustomDASQLMonitor.InternalDBConnect(Connection: TCustomDAConnection; BeforeEvent: boolean; var MessageID: Cardinal);
const
  fmtConnectInfo = 'Username=%s'#13#10'Server=%s';
var
  St, SQL: _string;
begin
  CheckActive;

  if Active and (tfConnect in TraceFlags) then begin
    St := 'Connect: ' + Connection.Username + '@' + Connection.Server + GetObjectHandle(Connection);

    if (moSQLMonitor in Options) and BeforeEvent then
      AddStatement(St);

    if moDBMonitor in Options then begin
      SQL := Format(fmtConnectInfo, [Connection.Username, Connection.Server]);
      SendDBMonitorEvent(BeforeEvent, ET_CONNECT, St, Connection, SQL,
        nil, False, '', MessageID);
    end;

    if Assigned(FOnSQLEvent) and BeforeEvent then
      FOnSQLEvent(Connection, St, tfConnect);
  end;
end;

procedure TCustomDASQLMonitor.InternalDBDisconnect(Connection: TCustomDAConnection; BeforeEvent: boolean; var MessageID: Cardinal);
var
  St: _string;
begin
  CheckActive;

  if Active and (tfConnect in TraceFlags) then begin
    St := 'Disconnect: ' + Connection.Username + '@' + Connection.Server + GetObjectHandle(Connection);

    if (moSQLMonitor in Options) and BeforeEvent then
      AddStatement(St);

    if moDBMonitor in Options then
      SendDBMonitorEvent(BeforeEvent, ET_DISCONNECT, St, Connection, '',
        nil, False, '', MessageID);

    if Assigned(FOnSQLEvent) and BeforeEvent then
      FOnSQLEvent(Connection, St, tfConnect);
  end;
end;

procedure TCustomDASQLMonitor.InternalTRStart(Obj: TObject; BeforeEvent: boolean; var MessageID: Cardinal);
var
  St: _string;
begin
  CheckActive;

  if Active and (tfTransact in TraceFlags) then begin
    St := 'Start: ';
    if Obj is TCustomDAConnection then
      St := St + TCustomDAConnection(Obj).Username + '@' + TCustomDAConnection(Obj).Server;
    St := St + GetObjectHandle(Obj);

    if (moSQLMonitor in Options) and BeforeEvent then
      AddStatement(St);

    if moDBMonitor in Options then
      SendDBMonitorEvent(BeforeEvent, ET_BEGIN_TRANS, St, Obj, '',
        nil, False, '', MessageID);

    if Assigned(FOnSQLEvent) and BeforeEvent then
      FOnSQLEvent(Obj, St, tfTransact);
  end;
end;

procedure TCustomDASQLMonitor.InternalTRCommit(Obj: TObject; BeforeEvent: boolean; var MessageID: Cardinal);
var
  St: _string;
begin
  CheckActive;

  if Active and (tfTransact in TraceFlags) then begin
    St := 'Commit: ';
    if Obj is TCustomDAConnection then
      St := St + TCustomDAConnection(Obj).Username + '@' + TCustomDAConnection(Obj).Server;
    St := St + GetObjectHandle(Obj);

    if (moSQLMonitor in Options) and BeforeEvent then
      AddStatement(St);

    if moDBMonitor in Options then
      SendDBMonitorEvent(BeforeEvent, ET_COMMIT, St, Obj, '',
        nil, False, '', MessageID);

    if Assigned(FOnSQLEvent) and BeforeEvent then
      FOnSQLEvent(Obj, St, tfTransact);
  end;
end;

procedure TCustomDASQLMonitor.InternalTRRollback(Obj: TObject; BeforeEvent: boolean; var MessageID: Cardinal);
var
  St: _string;
begin
  CheckActive;

  if Active and (tfTransact in TraceFlags) then begin
    St := 'Rollback: ';
    if Obj is TCustomDAConnection then
      St := St + TCustomDAConnection(Obj).Username + '@' + TCustomDAConnection(Obj).Server;
    St := St + GetObjectHandle(Obj);

    if (moSQLMonitor in Options) and BeforeEvent then
      AddStatement(St);

    if moDBMonitor in Options then
      SendDBMonitorEvent(BeforeEvent, ET_ROLLBACK, St, Obj, '',
        nil, False, '', MessageID);

    if Assigned(FOnSQLEvent) and BeforeEvent then
      FOnSQLEvent(Obj, St, tfTransact);
  end;
end;

procedure TCustomDASQLMonitor.InternalTRSavepoint(Obj: TObject; const Savepoint: _string; BeforeEvent: boolean; var MessageID: Cardinal);
var
  St: _string;
begin
  CheckActive;

  if Active and (tfTransact in TraceFlags) then begin
    St := 'Savepoint ' + Savepoint + ' ' + GetObjectHandle(Obj);

    if (moSQLMonitor in Options) and (tfTransact in TraceFlags) then
      AddStatement(St);

    if moDBMonitor in Options then
      SendDBMonitorEvent(BeforeEvent, ET_SAVEPOINT, St, Obj, '',
        nil, False, '', MessageID);

    if Assigned(FOnSQLEvent) then
      FOnSQLEvent(Obj, St, tfTransact);
  end;
end;

procedure TCustomDASQLMonitor.InternalTRRollbackToSavepoint(Obj: TObject; const Savepoint: _string; BeforeEvent: boolean; var MessageID: Cardinal);
var
  St: _string;
begin
  CheckActive;

  if Active and (tfTransact in TraceFlags) then begin
    St := 'Rollback to savepoint ' + Savepoint + ' ' + GetObjectHandle(Obj);

    if (moSQLMonitor in Options) and (tfTransact in TraceFlags) then
      AddStatement(St);

    if moDBMonitor in Options then
      SendDBMonitorEvent(BeforeEvent, ET_SAVEPOINT, St, Obj, '',
        nil, False, '', MessageID);

    if Assigned(FOnSQLEvent) then
      FOnSQLEvent(Obj, St, tfTransact);
  end;
end;

procedure TCustomDASQLMonitor.InternalTRReleaseSavepoint(Obj: TObject; const Savepoint: _string; BeforeEvent: boolean; var MessageID: Cardinal);
var
  St: _string;
begin
  CheckActive;

  if Active and (tfTransact in TraceFlags) then begin
    St := 'Release savepoint ' + Savepoint + ' ' + GetObjectHandle(Obj);

    if (moSQLMonitor in Options) and (tfTransact in TraceFlags) then
      AddStatement(St);

    if moDBMonitor in Options then
      SendDBMonitorEvent(BeforeEvent, ET_SAVEPOINT, St, Obj, '',
        nil, False, '', MessageID);

    if Assigned(FOnSQLEvent) then
      FOnSQLEvent(Obj, St, tfTransact);
  end;
end;

procedure TCustomDASQLMonitor.InternalTRCommitRetaining(Obj: TObject; BeforeEvent: boolean; var MessageID: Cardinal);
var
  St: _string;
begin
  CheckActive;

  if Active and (tfTransact in TraceFlags) then begin
    St := 'CommitRetaining: ' + GetObjectHandle(Obj);

    if (moSQLMonitor in Options) and (tfTransact in TraceFlags) and BeforeEvent then
      AddStatement(St);

    if moDBMonitor in Options then
      SendDBMonitorEvent(BeforeEvent, ET_COMMIT, St, Obj, '',
        nil, False, '', MessageID);

    if Assigned(FOnSQLEvent) and BeforeEvent then
      FOnSQLEvent(Obj, St, tfTransact);
  end;
end;

procedure TCustomDASQLMonitor.InternalTRRollbackRetaining(Obj: TObject; BeforeEvent: boolean; var MessageID: Cardinal);
var
  St: _string;
begin
  CheckActive;

  if Active and (tfTransact in TraceFlags) then begin
    St := 'RollbackRetaining: ' + GetObjectHandle(Obj);

    if (moSQLMonitor in Options) and (tfTransact in TraceFlags) and BeforeEvent then
      AddStatement(St);

    if moDBMonitor in Options then
      SendDBMonitorEvent(BeforeEvent, ET_ROLLBACK, St, Obj, '',
        nil, False, '', MessageID);

    if Assigned(FOnSQLEvent) and BeforeEvent then
      FOnSQLEvent(Obj, St, tfTransact);
  end;
end;

procedure TCustomDASQLMonitor.InternalDBError(Exception: EDAError);
var
  St: _string;
  MessageID: cardinal;
begin
  CheckActive;

  if Active and (tfError in TraceFlags) then begin
    St := 'Error: ' + Exception.Message;

    if (moSQLMonitor in Options) then
      AddStatement(St);

    if moDBMonitor in Options then begin
      MessageID := 0;
      SendDBMonitorEvent(False, ET_MISC, St, nil, '',
        nil, True, Exception.Message, MessageID);
    end;

    if Assigned(FOnSQLEvent) then
      FOnSQLEvent(Exception, St, tfError);
  end;
end;

procedure TCustomDASQLMonitor.InternalCustomMessage(Obj: TObject; const Msg: _string);
var
  MessageID: cardinal;
  Description: _string;
begin
  CheckActive;

  if Active and (tfMisc in TraceFlags) then begin
    Description := Msg + GetObjectHandle(Obj);

    if (moSQLMonitor in Options) then
      AddStatement(Description);

    if moDBMonitor in Options then begin
      SendDBMonitorEvent(True, ET_MISC, Description, Obj, '',
        nil, False, '', MessageID);
      SendDBMonitorEvent(False, ET_MISC, Description, Obj, '',
        nil, False, '', MessageID);
    end;

    if Assigned(FOnSQLEvent) then
      FOnSQLEvent(Obj, Description, tfMisc);
  end;
end;

procedure TCustomDASQLMonitor.InternalObjectDestroyed(Obj: TObject);
var
  MessageID: cardinal;
  Description: _string;
begin
  CheckActive;

  if Active and (tfObjDestroy in TraceFlags) then begin
    Description := 'Object destroyed: ' + GetObjectName(Obj);

    if (moSQLMonitor in Options) then
      AddStatement(Description);

    if moDBMonitor in Options then begin
      SendDBMonitorEvent(True, ET_OBJ_DESTROY, Description, Obj, '',
        nil, False, '', MessageID);
      SendDBMonitorEvent(False, ET_OBJ_DESTROY, Description, Obj, '',
        nil, False, '', MessageID);
    end;

    if Assigned(FOnSQLEvent) then
      FOnSQLEvent(Obj, Description, tfObjDestroy);
  end;
end;

procedure TCustomDASQLMonitor.InternalPoolMessage(Obj: TObject; const Msg: _string;
  WithCount: boolean; WithParams: boolean);
var
  MessageID: cardinal;
  Description, SQL: _string;
begin
  CheckActive;

  if Active and (tfPool in TraceFlags) then begin
    Description := Msg;
    if WithCount then
      Description := Description + _Format('. Pool has %d connection(s)',
        [TCRLocalConnectionPool(Obj).PooledConnectionsCount]);

    Description := Description + GetObjectHandle(Obj);

    if (moSQLMonitor in Options) then
      AddStatement(Description);

    if moDBMonitor in Options then begin
      if WithParams and (Obj is TCRConnectionPool) then
        SQL := TCRConnectionPool(Obj).ConnectionParameters.AsString
      else
        SQL := '';

      SendDBMonitorEvent(True, ET_CONN_POOL, Description, Obj, SQL,
        nil, False, '', MessageID);
      SendDBMonitorEvent(False, ET_CONN_POOL, Description, Obj, SQL,
        nil, False, '', MessageID);
    end;

    if Assigned(FOnSQLEvent) then
      FOnSQLEvent(Obj, Description, tfPool);
  end;
end;

{ Borland's SQL Monitor support }

procedure TCustomDASQLMonitor.RegisterClient;
{$IFDEF WIN32_64}
var
  Title: string;
{$ENDIF}
begin
{$IFDEF WIN32_64}
  if not FRegistered and (GetMonitor = Self) then
    if Succeeded(CoInitialize(nil)) then begin
      if not Succeeded(CoCreateInstance(Class_SMClient, nil, CLSCTX_INPROC_SERVER or
        CLSCTX_LOCAL_SERVER, IUnknown, FSMClient))
      then begin
        CoUninitialize;
        Exit;
      end;

      Title := ApplicationTitle;
      if csDesigning in ComponentState then
        Title := Title + ': ' + GetCaption;
      try
        (FSMClient as ISMClient).RegisterClient(Integer(Self), PChar(Title), Self,
          @TCustomDASQLMonitor.SMClientSignal);
        FRegistered := True;
      except
      end;
    end;
{$ENDIF}
end;

procedure TCustomDASQLMonitor.UnRegisterClient;
begin
{$IFDEF WIN32_64}
  FSMClient := nil;
  if FRegistered then begin
    FRegistered := False;
    CoUninitialize;
  end;
{$ENDIF}
end;

procedure TCustomDASQLMonitor.AddStatement(St: string);
begin
{$IFDEF WIN32_64}
  St := St + #0;
  if Assigned(FSMClient) then
    (FSMClient as ISMClient).AddStatement(PChar(St), Length(St) * SizeOf(Char) - 1);
{$ENDIF}
end;

{    0  none
     1  Prepare
     2  Execute
     4  Error
     8  Statement
    16  Connect/Disconnect
    32  Transaction
    64  Blob
   128  Misc
   256  Vendor call
   512  Input params
  1024  Fetch}

procedure TCustomDASQLMonitor.SMClientSignal(Sender: TObject; Data: Integer);
begin
  FTraceFlags := {$IFNDEF FPC}TDATraceFlags(Word(Data)){$ELSE}[TDATraceFlag(Word(Data))]{$ENDIF};
end;

function TCustomDASQLMonitor.NeedAutoActivate: boolean;
begin
  Result := (csDesigning in ComponentState) or
            (Owner = nil) or
            not (csReading in Owner.ComponentState);
end;

procedure TCustomDASQLMonitor.SetActive(Value: boolean);
begin
  FStreamedActive := Value;
  if not (csReading in ComponentState) and (Value <> FActive) then begin
    FActive := Value;

    if moSQLMonitor in Options then
      if FActive then
        RegisterClient
      else
        UnRegisterClient;

    if moDBMonitor in Options then
      if FActive then begin
        FDBMonitor := GetDBMonitor;
        StartDBMonitor;
      end
      else begin
        FDBMonitor.Finish;
        FDBMonitor := nil;
      end;
  end;
end;

procedure TCustomDASQLMonitor.SetOptions(Value: TMonitorOptions);
begin
  if Value <> FOptions then begin
    if FActive then begin
      if (moSQLMonitor in Value) and not (moSQLMonitor in FOptions) then
        RegisterClient
      else
      if not (moSQLMonitor in Value) and (moSQLMonitor in FOptions) then
        UnRegisterClient;

      if (moDBMonitor in Value) and not (moDBMonitor in FOptions) then begin
        FDBMonitor := GetDBMonitor;
        StartDBMonitor;
      end
      else
      if not (moDBMonitor in Value) and (moDBMonitor in FOptions) then begin
        FDBMonitor.Finish;
        FDBMonitor := nil;
      end;
    end;
    FOptions := Value;
  end;
end;

procedure TCustomDASQLMonitor.SetDBMonitorOptions(Value: TDBMonitorOptions);
begin
  FDBMonitorOptions.Assign(Value);
end;

procedure TCustomDASQLMonitor.StartDBMonitor;
begin
  FDBMonitor.Caption := GetCaption;
  FDBMonitor.Host := FDBMonitorOptions.Host;
  FDBMonitor.Port := FDBMonitorOptions.Port;
  FDBMonitor.ReconnectTimeout := FDBMonitorOptions.ReconnectTimeout;
  FDBMonitor.SendTimeout := FDBMonitorOptions.SendTimeout;
  FDBMonitor.IsDesigning := csDesigning in ComponentState;
  try
    FDBMonitor.Startup;
  except
    on EAbort do ;
  end;
end;

procedure TCustomDASQLMonitor.SendDBMonitorEvent(BeforeEvent: boolean;
  EventType: integer; const Description: string; Obj: TObject;
  const SQL: string; const Params: TSQLParams;
  Failed: boolean; const ErrorText: string;
  var EventID: Cardinal);
var
  Msg: TMonitorEvent;
  Parent: TObject;
begin
{$IFNDEF CLR}
  FillChar(@Msg, SizeOf(TMonitorEvent), 0);
{$ENDIF}
  Msg.EventID := EventID;
  Msg.EventType := EventType;

  if BeforeEvent and (Obj <> nil) then begin
    Msg.ObjectID := GetObjectID(Obj);
    Msg.ObjectName := GetObjectName(Obj);
    Msg.ObjectType := GetObjectType(Obj);
    Msg.ObjectTypeName := Obj.ClassName;

    if EventType <> ET_OBJ_DESTROY then begin
      Parent := GetParent(Obj);
      if Parent <> nil then begin
        Msg.ParentID := GetObjectID(Parent);
        Msg.ParentName := GetObjectName(Parent);
        Msg.ParentType := GetObjectType(Parent);
        Msg.ParentTypeName := Parent.ClassName;
      end;
    end;
  end;
  Msg.Description := Description;
  Msg.SQL := SQL;
  Msg.Params := Params;
  Msg.Failed := Failed;
  Msg.ErrorText := ErrorText;

  if (EventType = ET_EXECUTE) and not BeforeEvent then begin
    if (Obj is TCustomDASQL) then
      Msg.RowsAffected := TCustomDASQL(Obj).RowsAffected
    else
    if (Obj is TCustomDADataSet) then
      Msg.RowsAffected := TCustomDADataSet(Obj).RowsAffected;
  end;

  if Assigned(GetCallStackProc) then
    GetCallStackProc(Msg.CallStack);

  try
    if BeforeEvent then
      FDBMonitor.SendEventStart(Msg)
    else
      FDBMonitor.SendEventEnd(Msg);
  except
    on EAbort do ;
  end;

  if BeforeEvent then
    EventID := Msg.EventID;
end;

class function TCustomDASQLMonitor.HasMonitor: boolean;
begin
  Result := GetMonitor <> nil;
end;

procedure TCustomDASQLMonitor.Loaded;
begin
  inherited Loaded;
  CheckActive;
end;

procedure TCustomDASQLMonitor.CheckActive;
begin
  Active := FStreamedActive;
end;


initialization
  ShowDebugFormProc := nil;
end.

