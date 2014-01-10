//////////////////////////////////////////////////
//  SQL Server Data Access Components
//  Copyright © 1998-2012 Devart. All right reserved.
//  MSCompactConnection
//////////////////////////////////////////////////

{$IFNDEF CLR}

{$I Sdac.inc}

unit MSCompactConnection;
{$ENDIF}

interface

uses
  SysUtils, Windows, Classes, DB, DBAccess,
{$IFDEF VER6P}
  Variants,
{$ENDIF}
  MSAccess, OLEDBAccess, CRTypes;

type
  TMSCompactConnection = class;

  TMSCompactConnectionOptions = class(TCustomMSConnectionOptions)
  protected
    FMaxDatabaseSize: integer;
    FMaxBufferSize: integer;
    FTempFileDirectory: _string;
    FTempFileMaxSize: integer;
    FDefaultLockEscalation: integer;
    FAutoShrinkThreshold: integer;
    FFlushInterval: integer;
    FLocaleIdentifier: cardinal;

    procedure SetProvider(const Value: TOLEDBProvider); override;
    procedure SetMaxDatabaseSize(const Value: integer);
    procedure SetMaxBufferSize(const Value: integer);
    procedure SetTempFileDirectory(const Value: _string);
    procedure SetTempFileMaxSize(const Value: integer);
    procedure SetDefaultLockEscalation(const Value: integer);
    procedure SetAutoShrinkThreshold(const Value: integer);
    procedure SetFlushInterval(const Value: integer);
    function GetLocaleIdentifier: _string;
    procedure SetLocaleIdentifier(const Value: _string);

    procedure AssignTo(Dest: TPersistent); override;

    procedure LoadLocaleIdentifier(Reader: TReader);
    procedure StoreLocaleIdentifier(Writer: TWriter);
    procedure DefineProperties(Filer: TFiler); override;

  public
    constructor Create(Owner: TMSCompactConnection);

  published
    property QuotedIdentifier;
    property NumericType;
    property Encrypt;
    property MaxDatabaseSize: integer read FMaxDatabaseSize write SetMaxDatabaseSize default 128;
    property MaxBufferSize: integer read FMaxBufferSize write SetMaxBufferSize default 640;
    property TempFileDirectory: _string read FTempFileDirectory write SetTempFileDirectory;
    property TempFileMaxSize: integer read FTempFileMaxSize write SetTempFileMaxSize default 128;
    property DefaultLockEscalation: integer read FDefaultLockEscalation write SetDefaultLockEscalation default 100;
    property AutoShrinkThreshold: integer read FAutoShrinkThreshold write SetAutoShrinkThreshold default 60;
    property FlushInterval: integer read FFlushInterval write SetFlushInterval default 10;
    property LocaleIdentifier: _string read GetLocaleIdentifier write SetLocaleIdentifier stored False;
    property DefaultLockTimeout;
    property KeepDesignConnected;
    property DisconnectedMode;
    property LocalFailover;
    property CompactVersion;
  {$IFDEF VER10P}
    property UseWideMemos;
  {$ENDIF}
  end;

{$IFDEF VER16P}
  [ComponentPlatformsAttribute(pidWin32 or pidWin64)]
{$ENDIF}
  TMSCompactConnection = class(TCustomMSConnection)
  private
    FInitMode: TMSInitMode;
    FTransactionCommitMode: TCompactCommitMode;
    FLockTimeout: integer;
    FLockEscalation: integer;
    
  protected
    function CreateOptions: TDAConnectionOptions; override;
    
    function GetOptions: TMSCompactConnectionOptions;
    procedure SetOptions(Value: TMSCompactConnectionOptions);

    procedure AssignTo(Dest: TPersistent); override;

    procedure SetInitMode(const Value: TMSInitMode);
    procedure SetLockTimeout(const Value: integer);
    procedure SetLockEscalation(const Value: integer);
    procedure SetTransactionCommitMode(const Value: TCompactCommitMode);

    procedure FillConnectionProps(OLEDBConnection: TOLEDBConnection); override;

    function NeedPrompt: boolean; override;
    function GetConnectString: _string; override;

    procedure InitConnectStringOptions; override;
    procedure ProcessConnectStringParam(const paramName: string; paramValue: _string); override;
  public
    constructor Create(Owner: TComponent); override;

  published
    property Database;
    property IsolationLevel;
    property Options: TMSCompactConnectionOptions read GetOptions write SetOptions;

    property InitMode: TMSInitMode read FInitMode write SetInitMode default imReadWrite;
    property LockTimeout: integer read FLockTimeout write SetLockTimeout default 2000;
    property LockEscalation: integer read FLockEscalation write SetLockEscalation default 100;
    property TransactionCommitMode: TCompactCommitMode read FTransactionCommitMode write SetTransactionCommitMode default cmAsynchCommit;

    property PoolingOptions;
    property Pooling;
    property Password;
    property Connected stored IsConnectedStored;

    property AfterConnect;
    property BeforeConnect;
    property AfterDisconnect;
    property BeforeDisconnect;
    property OnLogin;
    property OnError;
    property ConnectDialog;
    property LoginPrompt;
    property ConnectString;
    property OnConnectionLost;
  end;

implementation

uses
  CRFunctions, MSConsts;

{ TMSCompactConnectionOptions }

constructor TMSCompactConnectionOptions.Create(Owner: TMSCompactConnection);
begin
  inherited Create(Owner);

  Provider := prCompact;
  FMaxDatabaseSize := DefaultMaxDatabaseSize;
  FMaxBufferSize := DefaultMaxBufferSize;
  FTempFileDirectory := '';
  FTempFileMaxSize := DefaultTempFileMaxSize;
  FDefaultLockEscalation := DefaultDefaultLockEscalation;
  FAutoShrinkThreshold := DefaultAutoShrinkThreshold;
  FFlushInterval := DefaultFlushInterval;
  FLocaleIdentifier := GetSystemDefaultLCID;
end;

procedure TMSCompactConnectionOptions.SetProvider(const Value: TOLEDBProvider);
begin
  if Value <> prCompact then
    DatabaseError(SBadProviderName)
  else
    inherited SetProvider(Value);
end;

procedure TMSCompactConnectionOptions.SetMaxDatabaseSize(const Value: integer);
begin
  if FMaxDatabaseSize <> Value then begin
    TMSCompactConnection(FOwner).CheckInactive;
    FMaxDatabaseSize := Value;
    if TMSCompactConnection(FOwner).IConnection <> nil then
      TMSCompactConnection(FOwner).IConnection.SetProp(prMaxDatabaseSize, Value);
  end;
end;

procedure TMSCompactConnectionOptions.SetMaxBufferSize(const Value: integer);
begin
  if FMaxBufferSize <> Value then begin
    TMSCompactConnection(FOwner).CheckInactive;
    FMaxBufferSize := Value;
    if TMSCompactConnection(FOwner).IConnection <> nil then
      TMSCompactConnection(FOwner).IConnection.SetProp(prMaxBufferSize, Value);
  end;
end;

procedure TMSCompactConnectionOptions.SetTempFileDirectory(const Value: _string);
begin
  if FTempFileDirectory <> Value then begin
    TMSCompactConnection(FOwner).CheckInactive;
    FTempFileDirectory := Value;
    if TMSCompactConnection(FOwner).IConnection <> nil then
      TMSCompactConnection(FOwner).IConnection.SetProp(prTempFileDirectory, Value);
  end;
end;

procedure TMSCompactConnectionOptions.SetTempFileMaxSize(const Value: integer);
begin
  if FTempFileMaxSize <> Value then begin
    TMSCompactConnection(FOwner).CheckInactive;
    FTempFileMaxSize := Value;
    if TMSCompactConnection(FOwner).IConnection <> nil then
      TMSCompactConnection(FOwner).IConnection.SetProp(prTempFileMaxSize, Value);
  end;
end;

procedure TMSCompactConnectionOptions.SetDefaultLockEscalation(const Value: integer);
begin
  if FDefaultLockEscalation <> Value then begin
    TMSCompactConnection(FOwner).CheckInactive;
    FDefaultLockEscalation := Value;
    if TMSCompactConnection(FOwner).IConnection <> nil then
      TMSCompactConnection(FOwner).IConnection.SetProp(prDefaultLockEscalation, Value);
  end;
end;

procedure TMSCompactConnectionOptions.SetAutoShrinkThreshold(const Value: integer);
begin
  if FAutoShrinkThreshold <> Value then begin
    TMSCompactConnection(FOwner).CheckInactive;
    FAutoShrinkThreshold := Value;
    if TMSCompactConnection(FOwner).IConnection <> nil then
      TMSCompactConnection(FOwner).IConnection.SetProp(prAutoShrinkThreshold, Value);
  end;
end;

function TMSCompactConnectionOptions.GetLocaleIdentifier: _string;
begin
  Result := LocaleIdentifierToStr(FLocaleIdentifier);
end;

procedure TMSCompactConnectionOptions.SetLocaleIdentifier(const Value: _string);
var
  lcid: cardinal;
begin
  if Value = '' then
    lcid := GetSystemDefaultLCID
  else
    lcid := LocaleIdentifierToCardinal(Value);
    
  if FLocaleIdentifier <> lcid then begin
    TMSCompactConnection(FOwner).CheckInactive;
    FLocaleIdentifier := lcid;
    if TMSCompactConnection(FOwner).IConnection <> nil then
      TMSCompactConnection(FOwner).IConnection.SetProp(prLocaleIdentifier, variant(FLocaleIdentifier));
  end;
end;

procedure TMSCompactConnectionOptions.SetFlushInterval(const Value: integer);
begin
  if FFlushInterval <> Value then begin
    TMSCompactConnection(FOwner).CheckInactive;
    FFlushInterval := Value;
    if TMSCompactConnection(FOwner).IConnection <> nil then
      TMSCompactConnection(FOwner).IConnection.SetProp(prFlushInterval, Value);
  end;
end;

procedure TMSCompactConnectionOptions.AssignTo(Dest: TPersistent);
begin
  inherited;

  if Dest is TMSCompactConnectionOptions then begin
    TMSCompactConnectionOptions(Dest).MaxDatabaseSize := MaxDatabaseSize;
    TMSCompactConnectionOptions(Dest).MaxBufferSize := MaxBufferSize;
    TMSCompactConnectionOptions(Dest).TempFileDirectory := TempFileDirectory;
    TMSCompactConnectionOptions(Dest).TempFileMaxSize := TempFileMaxSize;
    TMSCompactConnectionOptions(Dest).DefaultLockEscalation := DefaultLockEscalation;
    TMSCompactConnectionOptions(Dest).AutoShrinkThreshold := AutoShrinkThreshold;
    TMSCompactConnectionOptions(Dest).FlushInterval := FlushInterval;
    TMSCompactConnectionOptions(Dest).FLocaleIdentifier := FLocaleIdentifier;
  end;
end;

procedure TMSCompactConnectionOptions.LoadLocaleIdentifier(Reader: TReader);
begin
  FLocaleIdentifier := cardinal(Reader.ReadInteger);
end;

procedure TMSCompactConnectionOptions.StoreLocaleIdentifier(Writer: TWriter);
begin
  Writer.WriteInteger(integer(FLocaleIdentifier));
end;

procedure TMSCompactConnectionOptions.DefineProperties(Filer: TFiler);
begin
  inherited;
  Filer.DefineProperty('LCID', LoadLocaleIdentifier, StoreLocaleIdentifier, FLocaleIdentifier <> GetSystemDefaultLCID);
end;

{ TMSCompactConnection }

constructor TMSCompactConnection.Create(Owner: TComponent);
begin
  inherited Create(Owner);

  FInitMode := imReadWrite;
  FLockTimeout := DefaultDefaultLockTimeout;  
  FLockEscalation := DefaultDefaultLockEscalation;
  FTransactionCommitMode := cmAsynchCommit;
end;

function TMSCompactConnection.CreateOptions: TDAConnectionOptions;
begin
  Result := TMSCompactConnectionOptions.Create(Self);
end;

function TMSCompactConnection.GetOptions: TMSCompactConnectionOptions;
begin
  Result := FOptions as TMSCompactConnectionOptions;
end;

procedure TMSCompactConnection.SetOptions(Value: TMSCompactConnectionOptions);
begin
  FOptions.Assign(Value);
end;

procedure TMSCompactConnection.FillConnectionProps(OLEDBConnection: TOLEDBConnection);
begin
  inherited;

  OLEDBConnection.SetProp(prInitMode, Integer(InitMode));
  OLEDBConnection.SetProp(prLockTimeout, LockTimeout);
  OLEDBConnection.SetProp(prLockEscalation, LockEscalation);
  OLEDBConnection.SetProp(prTransactionCommitMode, Integer(TransactionCommitMode));

  if FOptions <> nil then begin
    OLEDBConnection.SetProp(prMaxDatabaseSize, Options.MaxDatabaseSize);
    OLEDBConnection.SetProp(prMaxBufferSize, Options.MaxBufferSize);
    OLEDBConnection.SetProp(prTempFileDirectory, Options.TempFileDirectory);
    OLEDBConnection.SetProp(prTempFileMaxSize, Options.TempFileMaxSize);
    OLEDBConnection.SetProp(prDefaultLockEscalation, Options.DefaultLockEscalation);
    OLEDBConnection.SetProp(prAutoShrinkThreshold, Options.AutoShrinkThreshold);
    OLEDBConnection.SetProp(prFlushInterval, Options.FlushInterval);
    OLEDBConnection.SetProp(prLocaleIdentifier, variant(Options.FLocaleIdentifier));
  end;
end;

procedure TMSCompactConnection.AssignTo(Dest: TPersistent);
begin
  inherited AssignTo(Dest);

  if Dest is TMSCompactConnection then begin
    TMSCompactConnection(Dest).InitMode := InitMode;
    TMSCompactConnection(Dest).LockTimeout := LockTimeout;
    TMSCompactConnection(Dest).LockEscalation := LockEscalation;
    TMSCompactConnection(Dest).TransactionCommitMode := TransactionCommitMode;
  end;
end;

procedure TMSCompactConnection.SetInitMode(const Value: TMSInitMode);
begin
  if FInitMode <> Value then begin
    CheckInactive;
    FInitMode := Value;
    if IConnection <> nil then
      IConnection.SetProp(prInitMode, Integer(Value));
  end;
end;

procedure TMSCompactConnection.SetLockTimeout(const Value: integer);
begin
  if FLockTimeout <> Value then begin
    CheckInactive;
    FLockTimeout := Value;
    if IConnection <> nil then
      IConnection.SetProp(prLockTimeout, Value);
  end;
end;

procedure TMSCompactConnection.SetLockEscalation(const Value: integer);
begin
  if FLockEscalation <> Value then begin
    CheckInactive;
    FLockEscalation := Value;
    if IConnection <> nil then
      IConnection.SetProp(prLockEscalation, Value);
  end;
end;

procedure TMSCompactConnection.SetTransactionCommitMode(const Value: TCompactCommitMode);
begin
  if FTransactionCommitMode <> Value then begin
    CheckInactive;
    FTransactionCommitMode := Value;
    if IConnection <> nil then
      IConnection.SetProp(prTransactionCommitMode, Integer(Value));
  end;
end;

function TMSCompactConnection.NeedPrompt: boolean;
begin
  Result := False;
  if not LoginPrompt and (csDesigning in ComponentState) then
    Exit;

  Result := inherited NeedPrompt;
end;

function InitModeToStr(Mode: TMSInitMode): string;
begin
  Result := '';
  case Mode of
    imReadOnly:
      Result := 'Read Only';
    imReadWrite:
      Result := 'Read Write';
    imExclusive:
      Result := 'Exclusive';
    imShareRead:
      Result := 'Shared Read';
    else
      Assert(False);
  end;
end;

function StrToInitMode(const Value: string; var Mode: TMSInitMode): boolean;
var
  LowValue: string;
begin
  Result := True;
  LowValue := LowerCase(Value);
  if LowValue = 'read only' then
    Mode := imReadOnly
  else
  if LowValue = 'read write' then
    Mode := imReadWrite
  else
  if LowValue = 'exclusive' then
    Mode := imExclusive
  else
  if LowValue = 'shared read' then
    Mode := imShareRead
  else
    Result := False;
end;

function TMSCompactConnection.GetConnectString: _string;
begin
  Result := inherited GetConnectString;

  AddConnectStringParam(Result, 'ssce: encrypt database', BoolToStr(Options.Encrypt, True), 'False');
  AddConnectStringParam(Result, 'ssce: max buffer size', IntToStr(Options.MaxBufferSize), IntToStr(DefaultMaxBufferSize));
  AddConnectStringParam(Result, 'ssce: max database size', IntToStr(Options.MaxDatabaseSize), IntToStr(DefaultMaxDatabaseSize));
  AddConnectStringParam(Result, 'ssce: mode', InitModeToStr(InitMode), InitModeToStr(imReadWrite));
  AddConnectStringParam(Result, 'ssce: locale identifier', Options.LocaleIdentifier, LocaleIdentifierToStr(GetSystemDefaultLCID));
  AddConnectStringParam(Result, 'ssce: default lock timeout', IntToStr(Options.DefaultLockTimeout), IntToStr(DefaultDefaultLockTimeout));
  AddConnectStringParam(Result, 'ssce: default lock escalation', IntToStr(Options.DefaultLockEscalation), IntToStr(DefaultDefaultLockEscalation));
  AddConnectStringParam(Result, 'ssce: flush interval', IntToStr(Options.FlushInterval), IntToStr(DefaultFlushInterval));
  AddConnectStringParam(Result, 'ssce: autoshrink threshold', IntToStr(Options.AutoShrinkThreshold), IntToStr(DefaultAutoShrinkThreshold));
  AddConnectStringParam(Result, 'ssce: temp file directory', Options.TempFileDirectory, '');
  AddConnectStringParam(Result, 'ssce: temp file max size', IntToStr(Options.TempFileMaxSize), IntToStr(DefaultTempFileMaxSize));
end;

procedure TMSCompactConnection.InitConnectStringOptions;
begin
  UserName := '';
  Password := '';
  Server := '';
  Database := '';

  Options.MaxBufferSize := DefaultMaxBufferSize;
  Options.MaxDatabaseSize := DefaultMaxDatabaseSize;
  InitMode := imReadWrite;
  Options.DefaultLockTimeout := DefaultDefaultLockTimeout;
  Options.DefaultLockEscalation := DefaultDefaultLockEscalation;
  Options.FlushInterval := DefaultFlushInterval;
  Options.AutoShrinkThreshold := DefaultAutoShrinkThreshold;
  Options.TempFileDirectory := '';
  Options.TempFileMaxSize := DefaultTempFileMaxSize;
  Options.FLocaleIdentifier := GetSystemDefaultLCID;
end;

procedure TMSCompactConnection.ProcessConnectStringParam(const paramName: string; paramValue: _string);
var
  Mode: TMSInitMode;
begin
  if RecognizedParameter(['ssce: database password'], paramName) then
    Password := paramValue
  else
  if RecognizedParameter(['ssce: encrypt database'], paramName) then
    Options.Encrypt := {$IFDEF VER6}CRFunctions.{$ENDIF}StrToBool(paramValue)
  else
  if RecognizedParameter(['ssce: max buffer size'], paramName) then
    Options.MaxBufferSize := StrToInt(paramValue)
  else
  if RecognizedParameter(['ssce: max database size'], paramName) then
    Options.MaxDatabaseSize := StrToInt(paramValue)
  else
  if RecognizedParameter(['ssce: mode'], paramName) then begin
    if not StrToInitMode(paramValue, Mode) then
      raise Exception.CreateFmt(SBadParamValue, [paramName, paramValue]);
    InitMode := Mode;
  end
  else
  if RecognizedParameter(['ssce: locale identifier'], paramName) then
    Options.LocaleIdentifier := paramValue
  else
  if RecognizedParameter(['ssce: default lock timeout'], paramName) then
    Options.DefaultLockTimeout := StrToInt(paramValue)
  else
  if RecognizedParameter(['ssce: default lock escalation'], paramName) then
    Options.DefaultLockEscalation := StrToInt(paramValue)
  else
  if RecognizedParameter(['ssce: flush interval'], paramName) then
    Options.FlushInterval := StrToInt(paramValue)
  else
  if RecognizedParameter(['ssce: autoshrink threshold'], paramName) then
    Options.AutoShrinkThreshold := StrToInt(paramValue)
  else
  if RecognizedParameter(['ssce: temp file directory'], paramName) then
    Options.TempFileDirectory := paramValue
  else
  if RecognizedParameter(['ssce: temp file max size'], paramName) then
    Options.TempFileMaxSize := StrToInt(paramValue)
  else
    inherited ProcessConnectStringParam(paramName, paramValue);
end;

end.
