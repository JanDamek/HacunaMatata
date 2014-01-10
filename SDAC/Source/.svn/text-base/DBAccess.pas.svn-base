/////////////////////////////////////////////////
//  DB Access Components
//  Copyright © 1998-2012 Devart. All right reserved.
//  DB Access
//  Created:            01.07.00
//////////////////////////////////////////////////

{$J+}
{$IFNDEF CLR}

{$I Dac.inc}

unit DBAccess;
{$ENDIF}
interface

uses
{$IFDEF MSWINDOWS}
  Windows,
{$ENDIF}
  SysUtils, Classes, DB, SyncObjs, TypInfo,
{$IFDEF VER6P}
  Variants, {$IFNDEF FPC}SqlTimSt,{$ENDIF}
{$ENDIF}
{$IFDEF MSWINDOWS}
  Registry,
{$ENDIF}
{$IFDEF CLR}
  System.XML, System.Runtime.InteropServices, System.Text, Contnrs,
{$ELSE}
  CLRClasses, CRXml,
{$ENDIF}
  MemData, MemUtils, {$IFDEF FPC}MemDataSet,{$ELSE}MemDS,{$ENDIF}
  CRTypes, CRAccess, CRParser, CRVio;

const
  OperationsStackDelta = 50;

type
  TCustomDAConnection = class;
  TDATransaction = class;
  TDATransactions = class;
  TCustomDASQL = class;
  TCustomDADataSet = class;
  TDAMetaData = class;
  TCustomDASQLClass = class of TCustomDASQL;
  TCustomDADataSetClass = class of TCustomDADataSet;
  TDADataSetUpdater = class;
  TDADataSetService = class;
  TDADataSetServiceClass = class of TDADataSetService;
  TDADataSetUpdaterClass = class of TDADataSetUpdater;

  TCustomDAUpdateSQL = class;
  TDAParam = class;
  TMacro = class;
  TMacros = class;
  TCustomConnectDialog = class;
  TConnectDialogClass = class of TCustomConnectDialog;

{$IFDEF FPC}
  TConnectChangeEvent = procedure(Sender: TObject; Connecting: Boolean) of object;
{$ENDIF}

{ EDAError }

  EDAError = class (EDatabaseError)
  protected
    FErrorCode: integer;
    FComponent: TObject;
  {$IFDEF CRUNICODE}
    FWideMessage: WideString;
  {$ENDIF}

  {$IFDEF CRUNICODE}
    procedure SetWideMessage(const Value: WideString);
  {$ENDIF}

  public
    constructor Create(ErrorCode: integer; Msg: _string);

    function IsFatalError: boolean; virtual;
    function IsKeyViolation: boolean; virtual;

  {$IFDEF CRUNICODE}
    property Message: WideString read FWideMessage write SetWideMessage;
  {$ENDIF}
    property ErrorCode: integer read FErrorCode;
    property Component: TObject read FComponent write FComponent;
  end;

{ TCustomDAConnection }
  TRetryMode = (rmRaise, rmReconnect, rmReconnectExecute);

  TFailOverOperation = record
    Operation: TConnLostCause;
    AllowFailOver: boolean;
  end;
  TOperationsStack = array of TFailOverOperation;// executed operations stack used to track dowm connection lost cause

  TDAConnectionErrorEvent = procedure (Sender: TObject; E: EDAError; var Fail: boolean) of object;
  TConnectionLostEvent = procedure (Sender: TObject; Component: TComponent; ConnLostCause: TConnLostCause;
                           var RetryMode: TRetryMode) of object;

  TDAConnectionOptions = class (TPersistent)
  private
    FKeepDesignConnected: boolean;
    FDisconnectedMode: boolean;
    FLocalFailover: boolean;
    FDefaultSortType: TSortType;
    FEnableBCD: boolean;
  {$IFDEF VER6P}
  {$IFNDEF FPC}
    FEnableFMTBCD: boolean;
  {$ENDIF}
  {$ENDIF}

    procedure SetDisconnectedMode(Value: boolean);
    procedure SetDefaultSortType(Value: TSortType);
    procedure SetEnableBCD(Value: boolean);
  {$IFDEF VER6P}
  {$IFNDEF FPC}
    procedure SetEnableFMTBCD(Value: boolean);
  {$ENDIF}
  {$ENDIF}

  protected
    FOwner: TCustomDAConnection;

    procedure AssignTo(Dest: TPersistent); override;

    property EnableBCD: boolean read FEnableBCD write SetEnableBCD default False;
  {$IFDEF VER6P}
  {$IFNDEF FPC}
    property EnableFMTBCD: boolean read FEnableFMTBCD write SetEnableFMTBCD default False;
  {$ENDIF}
  {$ENDIF}

  public
    constructor Create(Owner: TCustomDAConnection);

    property DisconnectedMode: boolean read FDisconnectedMode write SetDisconnectedMode default False;
    property KeepDesignConnected: boolean read FKeepDesignConnected write FKeepDesignConnected default True;
    property LocalFailover: boolean read FLocalFailover write FLocalFailover default False;
    property DefaultSortType: TSortType read FDefaultSortType write SetDefaultSortType default stCaseSensitive;
  end;

  TPoolingOptions = class(TPersistent)
  protected
    FOwner: TCustomDAConnection;
    FMaxPoolSize: integer;
    FMinPoolSize: integer;
    FConnectionLifetime: integer;
    FValidate: boolean;

    procedure AssignTo(Dest: TPersistent); override;

  public
    constructor Create(Owner: TCustomDAConnection); virtual;

  published
    property MaxPoolSize: integer read FMaxPoolSize write FMaxPoolSize default 100;
    property MinPoolSize: integer read FMinPoolSize write FMinPoolSize default 0;
    property ConnectionLifetime: integer read FConnectionLifetime write FConnectionLifetime default 0;
    property Validate: boolean read FValidate write FValidate default False;
  end;

  TCustomDAConnection = class (TCustomConnection)
  private
  {$IFDEF FPC}
    FClients: TList;
    FDataSets: TList;
    FConnectEvents: TList;
  {$ENDIF}
    FTransactions: TDATransactions;
    FUsername: _string;
    FAutoCommit: boolean;
    FInProcessError: boolean;
    FConnectDialog: TCustomConnectDialog;
    FDebug: boolean;

    FOnError: TDAConnectionErrorEvent;
    FConvertEOL: boolean;

    FOptions: TDAConnectionOptions;
    FPoolingOptions: TPoolingOptions;
    FPooling: boolean;
    FOnConnectionLost: TConnectionLostEvent;
    hRegisterClient: TCriticalSection;

    procedure SetDefaultTransaction(Value: TDATransaction);
    function GetDefaultTransaction: TDATransaction;
    function GetTransaction(Index: integer): TDATransaction;
    function GetTransactionsCount: integer;
    procedure SetUsername(const Value: _string);
    procedure SetPassword(const Value: _string);
    procedure SetAutoCommit(Value: boolean);
    procedure SetConnectDialog(Value: TCustomConnectDialog);
    procedure SetPooling(Value: boolean);
    procedure SetDebug(Value: boolean);

    procedure DoAfterConnect;
  protected
    FDefaultTransaction: TDATransaction;
    FInternalDefTransaction: TDATransaction;
    FConnectCount: integer;
    FSQLs: TDAList;
    FIConnection: TCRConnection;
    FStreamedConnected: boolean;
    FServer: _string;
    FPassword: _string;
    FShouldShowPrompt: boolean; //Disconnect mode flag that allow to avoid unnecessary Login porompt showing
    FOperationsStack: TOperationsStack ;  //FailOver support
    FOperationsStackLen: integer;
    FCommand: TCustomDASQL;
    FLockLoginPrompt: boolean;
    FIOHandler: TCRIOHandler;

    procedure ClearRefs; virtual;
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
  {$IFNDEF LITE}
    procedure SetIOHandler(Value: TCRIOHandler);
  {$ENDIF}

  {$IFDEF FPC}
    procedure SendConnectEvent(Connecting: Boolean);
    function GetDataSet(Index: Integer): TDataSet; override;
    function GetDataSetCount: Integer; override;
  {$ENDIF}

    function GetIConnectionClass: TCRConnectionClass; virtual;
    function GetICommandClass: TCRCommandClass; virtual;
    function GetIRecordSetClass: TCRRecordSetClass; virtual;
    function GetIMetaDataClass: TCRMetaDataClass; virtual;

    function IsMultipleTransactionsSupported: boolean; virtual;

    procedure CreateIConnection; virtual;
    function CreateICommand: TCRCommand;
    function CreateIRecordSet: TCRRecordSet;
    procedure FreeIConnection;
    procedure SetIConnection(Value: TCRConnection); virtual;
    procedure ClearTransactionRefs;

    procedure Loaded; override;

    procedure RegisterClient(Client: TObject; Event: TConnectChangeEvent = nil); {$IFNDEF FPC}override;{$ENDIF}
    procedure UnRegisterClient(Client: TObject); {$IFNDEF FPC}override;{$ENDIF}

    function SQLMonitorClass: TClass; virtual; // TDASQLMonitorClass
    function ConnectDialogClass: TConnectDialogClass; virtual;

    procedure DoConnect; override;
    procedure DoDisconnect; override;
    procedure DisconnectTransaction; virtual;
    procedure InternalConnect; virtual;
    procedure InternalDisconnect; virtual;

    function InternalGetServer: _string; virtual; // for IBDAC
    function IsConnectedStored: boolean; virtual;
    function NeedPrompt: boolean; virtual;
  //Operations stack functionality
    function PushOperation(Operation: TConnLostCause; AllowFailOver: boolean = true): integer; virtual;
    function PopOperation: TConnLostCause; virtual;

    procedure ResetOnFatalError; virtual;
    procedure RestoreAfterFailOver; virtual;
    function IsFailOverAllowed: boolean; virtual;
    function DetectConnLostCause(Component: TObject): TConnLostCause; virtual;
    procedure DoError(E: Exception; var Fail, Reconnect, Reexecute: boolean; ReconnectAttempt: integer;
      var ConnLostCause: TConnLostCause); virtual;

    procedure AssignTo(Dest: TPersistent); override;

    function GetConnected: boolean; override;
    procedure SetConnected(Value: boolean); override;
    function GetConnectString: _string; virtual;
    procedure SetConnectString(Value: _string); virtual;
    procedure SetServer(const Value: _string); virtual;

    function DefaultTableSchema: _string; virtual;

    procedure SuppressAutoCommit;
    procedure RestoreAutoCommit;
    function DetectInTransaction(CanActivate: boolean = False): boolean; virtual;
    function GetInTransaction: boolean; virtual;
    function UsedTransaction: TDATransaction; virtual;
    procedure SetConvertEOL(Value: boolean);
    procedure CheckCommand; virtual;
    procedure AssignConnectOptions(Source: TCustomDAConnection); virtual;

    function CreateOptions: TDAConnectionOptions; virtual;
    procedure SetOptions(Value: TDAConnectionOptions);
    function CreatePoolingOptions: TPoolingOptions; virtual;
    procedure SetPoolingOptions(Value: TPoolingOptions);

  { Transaction control }
    function InternalAddTransaction(TR: TDATransaction): integer;
    procedure InternalRemoveTransaction(TR: TDATransaction);
    function DoAddTransaction(TR: TDATransaction): integer; virtual;
    procedure DoRemoveTransaction(TR: TDATransaction); virtual;

    procedure DoCommitRetaining; virtual;
    procedure DoRollbackRetaining; virtual;
    procedure DoSavepoint(const Name: _string); virtual;
    procedure DoRollbackToSavepoint(const Name: _string); virtual;
    procedure DoReleaseSavepoint(const Name: _string); virtual;

    property DefaultTransaction: TDATransaction read GetDefaultTransaction write SetDefaultTransaction;
    property TransactionCount: integer read GetTransactionsCount;
    property Transactions[Index: integer]: TDATransaction read GetTransaction;
    property AutoCommit: boolean read FAutoCommit write SetAutoCommit default True;
    property ConnectString: _string read GetConnectString write SetConnectString stored False;

  {$IFNDEF LITE}
    property IOHandler: TCRIOHandler read FIOHandler write SetIOHandler;
  {$ENDIF}
  public
    constructor Create(Owner: TComponent); override;
    destructor Destroy; override;

    procedure Connect;
    procedure Disconnect;
    procedure PerformConnect(Retry: boolean = False);
    procedure AssignConnect(Source: TCustomDAConnection);

    function ParamByName(Name: _string): TDAParam;
    function ExecSQL(Text: _string; const Params: array of variant): variant; virtual;
    function ExecSQLEx(Text: _string; const Params: array of variant): variant; virtual;
    function ExecProc(Name: _string; const Params: array of variant): variant; virtual;
    function ExecProcEx(Name: _string; const Params: array of variant): variant; virtual;

    procedure GetTableNames(List: _TStrings; AllTables: boolean = False); virtual;
    procedure GetDatabaseNames(List: _TStrings); virtual;
    procedure GetStoredProcNames(List: _TStrings; AllProcs: boolean = False); virtual;

  { Transaction control }
    procedure StartTransaction; virtual;
    procedure Commit; virtual;
    procedure Rollback; virtual;

    procedure ApplyUpdates; overload; virtual;
    procedure ApplyUpdates(DataSets: array of TCustomDADataSet); overload; virtual;

    function CreateTransaction: TDATransaction; virtual;
    function CreateDataSet: TCustomDADataSet; virtual;
    function CreateSQL: TCustomDASQL; virtual;
    function CreateMetaData: TDAMetaData; virtual;

    procedure RemoveFromPool;
    procedure MonitorMessage(const Msg: string);

    property Username: _string read FUsername write SetUsername;
    property Password: _string read FPassword write SetPassword;
    property Server: _string read FServer write SetServer;
    property InTransaction: boolean read GetInTransaction;
    property ConnectDialog: TCustomConnectDialog read FConnectDialog write SetConnectDialog;

    property OnError: TDAConnectionErrorEvent read FOnError write FOnError;
    property OnConnectionLost: TConnectionLostEvent read FOnConnectionLost write FOnConnectionLost;

    property LoginPrompt default True;
    property ConvertEOL: boolean read FConvertEOL write SetConvertEOL default False;
    property Debug: boolean read FDebug write SetDebug default False;

    property Options: TDAConnectionOptions read FOptions write SetOptions;
    property PoolingOptions: TPoolingOptions read FPoolingOptions write SetPoolingOptions;
    property Pooling: boolean read FPooling write SetPooling default False;

  end;

{ TDAConnections }
  TDAConnections = class(TDAList)
  private
    function GetItems(Index: integer): TCustomDAConnection;
  public
    property Items[Index: integer]: TCustomDAConnection read GetItems; default;

  end;

{ TDATransactions }
  TDATransactions = class (TDAList)
  private
    function GetItems(Index: Integer): TDATransaction;
  public
    property Items[Index: Integer]: TDATransaction read GetItems; default;
  end;

{ TDATransaction }

  TTransactionType = (ttNative, ttMTS); 

  TDATransactionErrorEvent = procedure (Sender: TObject; E: EDAError; var Fail: boolean) of object;

  TDATransaction = class(TComponent)
  private
    procedure SetDefaultConnection(Value: TCustomDAConnection);
    procedure SetIsolationLevel(Value: TCRIsolationLevel);
    procedure SetReadOnly(Value: boolean);
    procedure SetTransactionType(Value: TTransactionType);
    function GetConnection(Index: integer): TCustomDAConnection;
    function GetConnectionsCount: integer;
    function GetActive: boolean;

  protected
    FDesignCreate: boolean;
    FDefaultConnection: TCustomDAConnection;

    FTrStartCount: integer;
    FUnCommitedStatementCount: integer;
    FExplicitlyStarted: boolean; // True if transaction is started by user
    FDisconnectedMode: boolean;
    FFailOverSatus: integer;
    FPrepared: boolean;

    FTransactionType: TTransactionType;
    FDefaultCloseAction: TCRTransactionAction;
    FIsolationLevel: TCRIsolationLevel;
    FReadOnly: boolean;
    FInProcessError: boolean;
    FOnError: TDATransactionErrorEvent;

    FITransaction: TCRTransaction;
    FShareTransaction: boolean; // use ITransaction from connection
    FConnections: TDAConnections;
    FOnStart: TNotifyEvent;
    FOnCommit: TNotifyEvent;
    FOnRollback: TNotifyEvent;

    function IsInternalTrStored: boolean;
    procedure Loaded; override;
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;

    function GetITransactionClass: TCRTransactionClass; virtual;
    procedure CheckITransaction;
    procedure CreateITransaction;
    procedure SetITransaction(Value: TCRTransaction); virtual;
    procedure FreeITransaction;
    procedure ClearRefs;

    function DetectInTransaction(CanActivate: boolean = False): boolean; virtual; // for ODAC
    procedure CheckActive;
    procedure CheckInactive;

    procedure Reset;
    procedure Restore;
    procedure CloseDataSets;
    procedure CloseTransaction(Force: boolean = False); virtual;
    procedure GainTransaction; virtual;
    procedure AutoCommitTransaction(NeedCommit: boolean); virtual;
    procedure ReleaseTransaction; virtual;
    function CanAutoCommitExplicitTransaction: boolean; virtual;

    function SQLMonitorClass: TClass; virtual;
    function UsedConnection: TCustomDAConnection; virtual;

    procedure PrepareTransaction(CheckOnly: boolean = False); // setup to start
    procedure UnPrepareTransaction; // reset after Commit or Rollback

    // Server specific methods
    procedure DoCommitRetaining; virtual;
    procedure DoRollbackRetaining; virtual;

    procedure DoSavepoint(const Name: _string); virtual;
    procedure DoReleaseSavepoint(const Name: _string); virtual;
    procedure DoRollbackToSavepoint(const Name: _string); virtual;

    function InternalAddConnection(Connection: TCustomDAConnection): integer;
    procedure InternalRemoveConnection(Connection: TCustomDAConnection);

    function DoAddConnection(Connection: TCustomDAConnection): integer; virtual;
    procedure DoRemoveConnection(Connection: TCustomDAConnection); virtual;
    procedure DoClearConnections;

    procedure DoError(E: Exception; var Fail: boolean);

    property Connections[Index: integer]: TCustomDAConnection read GetConnection;
    property ConnectionsCount: integer read GetConnectionsCount;
    property TransactionType: TTransactionType read FTransactionType write SetTransactionType default ttNative;
    property IsolationLevel: TCRIsolationLevel read FIsolationLevel write SetIsolationLevel default ilReadCommitted;
    property ReadOnly: boolean read FReadOnly write SetReadOnly default False;

  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    property Active: boolean read GetActive;

    procedure StartTransaction; virtual;
    procedure Commit; virtual;
    procedure Rollback; virtual;

    property DefaultConnection: TCustomDAConnection read FDefaultConnection write SetDefaultConnection;
    property DefaultCloseAction: TCRTransactionAction read FDefaultCloseAction write FDefaultCloseAction default taRollback;
    property OnError: TDATransactionErrorEvent read FOnError write FOnError;
    property OnStart: TNotifyEvent read FOnStart write FOnStart;
    property OnCommit: TNotifyEvent read FOnCommit write FOnCommit;
    property OnRollback: TNotifyEvent read FOnRollback write FOnRollback;
  end;

{ TDAParam }
  TDAParams = class;

  TDAParamInfoClass = class of TDAParamInfo;
  TDAParamInfo = class(TCollectionItem)
  protected
    FField: TField;
    FOld: boolean;
    FParamIndex: integer;
  public
    property Field: TField read FField write FField;
    property Old: boolean read FOld write FOld;
    property ParamIndex: integer read FParamIndex write FParamIndex;
  end;

  TDAParamsInfo = class(TCollection)
  protected
    function GetItem(Index: Integer): TDAParamInfo;
    procedure SetItem(Index: Integer; Value: TDAParamInfo);
  public
    property Items[Index: Integer]: TDAParamInfo read GetItem write SetItem; default;
  end;

  TDAParam = class (TParam)
  private
    FSize: integer;
    FSubDataType: word;

    function IsDataTypeStored: boolean;
    function IsValueStored: boolean;
  protected
    FParamObject: TSharedObject;
    FNational: boolean;

    function GetNativeParamObject: TSharedObject; virtual;
    procedure SetParamObject(Value: TSharedObject); virtual;
   
    function IsObjectDataType(DataType: TFieldType): boolean; overload; virtual;
    function IsObjectDataType: boolean; overload;
    function IsBlobDataType(DataType: TFieldType): boolean; overload; virtual;
    function IsBlobDataType: boolean; overload; virtual;
    function GetDataType: TFieldType; virtual;
    procedure SetDataType(Value: TFieldType); virtual;
    function GetSize: integer; virtual;
    procedure SetSize(Value: integer); virtual;

    function GetAsString: string; virtual;
    procedure SetAsString(const Value: string); virtual;
    function GetAsAnsiString: AnsiString; virtual;
    procedure SetAsAnsiString(const Value: AnsiString); virtual;
    function GetAsWideString: WideString; virtual;
    procedure SetAsWideString(const Value: WideString); virtual;
  {$IFDEF VER12P}
    function GetAsBytes: TBytes; virtual;
    procedure SetAsBytes(const Value: TBytes); virtual;
  {$ENDIF}

    function GetAsInteger: integer; virtual;
    procedure SetAsInteger(Value: integer); virtual;
    procedure SetAsSmallInt(Value: LongInt); virtual;
    procedure SetAsWord(Value: LongInt); virtual;
    function GetAsFloat: double; virtual;
    procedure SetAsFloat(Value: double); virtual;
  {$IFDEF VER6P}
    function GetAsLargeInt: Int64; virtual;
    procedure SetAsLargeInt(const Value: Int64); virtual;
  {$ENDIF}
  {$IFDEF VER12P}
    procedure SetAsShortInt(Value: LongInt); virtual;
    procedure SetAsByte(Value: LongInt); virtual;
  {$ENDIF}

    procedure SetAsBlob(const Value: TBlobData); virtual;
    procedure SetAsMemo(const Value: string); virtual;

    function GetAsBlobRef: TBlob; virtual;
    procedure SetAsBlobRef(const Value: TBlob); virtual;

    function GetAsMemoRef: TBlob; virtual;
    procedure SetAsMemoRef(const Value: TBlob); virtual;

    function GetAsVariant: variant; virtual;
    procedure SetAsVariant(const Value: variant); virtual;

  {$IFDEF VER6P}
  {$IFNDEF FPC}
    function GetAsSQLTimeStamp: TSQLTimeStamp; virtual;
    procedure SetAsSQLTimeStamp(const Value: TSQLTimeStamp); virtual;
  {$ENDIF}
  {$ENDIF}

    function GetAsCursor: TCRCursor;
    procedure SetAsCursor(Value: TCRCursor);

    procedure SetText(const Value: string); virtual;

    function GetIsNull: boolean; virtual;
    procedure SetIsNull(Value: boolean);

    function GetNational: boolean;
    procedure SetNational(Value: boolean); virtual;

    procedure DefineProperties(Filer: TFiler); override;
    procedure ReadExtDataType(Reader: TReader);
    procedure WriteExtDataType(Writer: TWriter);

    procedure CreateObject; virtual;
    procedure FreeObject; virtual;

    procedure AssignParam(Param: TParam);
    procedure AssignTo(Dest: TPersistent); override;

    property ParamObject: TSharedObject read FParamObject write SetParamObject;
    property SubDataType: word read FSubDataType write FSubDataType;
    property National: Boolean read GetNational write SetNational;

    property AsCursor: TCRCursor read GetAsCursor write SetAsCursor;
  public
    constructor Create(Collection: TCollection); override;
    destructor Destroy; override;

    procedure Clear; virtual;

    procedure Assign(Source: TPersistent); override;
    procedure AssignField(Field: TField);
    procedure AssignFieldValue(Field: TField; const Value: Variant); virtual;

    procedure LoadFromFile(const FileName: string; BlobType: TBlobType);
    procedure LoadFromStream(Stream: TStream; BlobType: TBlobType); virtual;
    procedure SetBlobData(Buffer: {$IFNDEF CLR}Pointer{$ELSE}TBytes{$ENDIF}; Size: Integer); overload;
    procedure SetBlobData(Buffer: TValueBuffer); overload;

    property AsString: string read GetAsString write SetAsString;
    property AsAnsiString: AnsiString read GetAsAnsiString write SetAsAnsiString;
    property AsWideString: WideString read GetAsWideString write SetAsWideString;
  {$IFDEF VER12P}
    property AsBytes: TBytes read GetAsBytes write SetAsBytes;
  {$ENDIF}
    property AsInteger: integer read GetAsInteger write SetAsInteger;
    property AsSmallInt: LongInt read GetAsInteger write SetAsSmallInt;
    property AsWord: LongInt read GetAsInteger write SetAsWord;
    property AsFloat: double read GetAsFloat write SetAsFloat;
  {$IFDEF VER6P}
    property AsLargeInt: Int64 read GetAsLargeInt write SetAsLargeInt;
  {$ENDIF}
  {$IFDEF VER12P}
    property AsShortInt: LongInt read GetAsInteger write SetAsShortInt;
    property AsByte: LongInt read GetAsInteger write SetAsByte;
  {$ENDIF}
    property AsBlob: TBlobData
      read {$IFDEF VER12P}GetAsBytes{$ELSE}GetAsString{$ENDIF} write SetAsBlob;
    property AsMemo: string read GetAsString write SetAsMemo;
    property AsBlobRef: TBlob read GetAsBlobRef write SetAsBlobRef;
    property AsMemoRef: TBlob read GetAsMemoRef write SetAsMemoRef;
  {$IFDEF VER6P}
  {$IFNDEF FPC}
    property AsSQLTimeStamp: TSQLTimeStamp read GetAsSQLTimeStamp write SetAsSQLTimeStamp;
  {$ENDIF}
  {$ENDIF}
    property IsNull: boolean read GetIsNull;
    property Text: string read GetAsString write SetText;

  published
    property DataType: TFieldType read GetDataType write SetDataType stored IsDataTypeStored;
    property ParamType default DB.ptUnknown;
    property Size: integer read GetSize write SetSize default 0;
    property Value: variant read GetAsVariant write SetAsVariant stored IsValueStored;
  end;

{ TDAParams }

  TParamsChangeType = (ctGenerated, ctUsers, ctUserChecked);

  TDAParams = class (TParams)
  private
    function GetItem(Index: integer): TDAParam;
    procedure SetItem(Index: integer; Value: TDAParam);

  protected
    FOwner: TPersistent;
    FNeedsUpdateItem: boolean;
    FParamsChangeType: TParamsChangeType;

    function GetOwner: TPersistent; override;
    procedure Update(Item: TCollectionItem); override;
    procedure Disconnect;

  public
    constructor Create(Owner: TPersistent); overload;

    function ParamByName(const Value: _string): TDAParam;
    function FindParam(const Value: _string): TDAParam;
    function CreateParam(FldType: TFieldType; const ParamName: _string;
      ParamType: TParamType): TDAParam;
    property Items[Index: integer]: TDAParam read GetItem write SetItem; default;
  end;

{ TDACursorField }

  TDACursorField = class (TField)
  private
    function GetAsCursor: TCRCursor;
  protected
    function GetValue(var Value: TCRCursor): boolean;

  public
    constructor Create(Owner: TComponent); override;

    property AsCursor: TCRCursor read GetAsCursor;
  end;

{ TDASQLGenerator }

  TLockMode = (lmNone, lmPessimistic, lmOptimistic);

  TFieldArray = array of TField;
  TFieldDescArray = array of TCRFieldDesc;
  TKeyAndDataFields = record
    KeyFieldDescs: TFieldDescArray;
    DataFieldDescs: TFieldDescArray;
  end;

  TStatementType = (stQuery, stInsert, stUpdate, stDelete, stLock, stRefresh,
    stCustom, stRefreshQuick, stRefreshCheckDeleted, stBatchUpdate);
  TStatementTypes = set of TStatementType;

  TDASQLGenerator = class
  protected
    FDataSet: TCustomDADataSet;
    FDataSetService: TDADataSetService;
    
                                    // stInsert                 stUpdate                 stDelete                 stRefresh
    FHeaderSB,                      // INSERT INTO Tbl(         UPDATE Tbl SET           DELETE FROM Tbl WHERE    SELECT
    FFldSB,                         // f1, f2, f3, ...          f1 = :p1, f2 = :p2, ...                           f1, f2, f3, ...
    FMiddleSB,                      // ) VALUES (               WHERE                                             FROM Tbl WHERE
    FFldParamSB,                    // :p1, :p2, :p3, ...
    FCondSB,                        //                          f0 = :p0                 f0 = :p0                 f0 = :p0
    FFooterSB: _StringBuilder;       // )
    FOldRecBuf, FNewRecBuf: IntPtr;
    FParams: TDAParams;
    FParamsInfo: TDAParamsInfo;
    FTableInfo: TCRTableInfo;
    FDesignMode: boolean;

    procedure Clear; virtual;
    function AssembleSB(const StatementType: TStatementType): _string; virtual;

    function GetIRecordSet: TCRRecordSet;
    function SQLInfo: TSQLInfo;
    function OldRecBuf: IntPtr;
    function NewRecBuf: IntPtr;

    function IsBlobDataType(DataType: word): boolean; virtual; //TODO: Map
    function IsObjectDataType(DataType: word): boolean; virtual; //TODO: Map

    function FieldIsNull(FieldDesc: TCRFieldDesc; OldValue: boolean; Data: TData; OldRecBuf, NewRecBuf: IntPtr): boolean; overload; virtual;
    function FieldIsNull(FieldDesc: TCRFieldDesc; OldValue: boolean): boolean; overload; virtual;

    function FieldModified(FieldDesc: TCRFieldDesc; Data: TData; OldRecBuf, NewRecBuf: IntPtr): boolean; overload; virtual;
    function FieldModified(FieldDesc: TCRFieldDesc): boolean; overload; virtual;

    //function GetActualFieldNameEx(FieldDesc: TCRFieldDesc; TableInfo: TCRTableInfo): _string; virtual;
    function GetActualFieldName(FieldDesc: TCRFieldDesc; IsRefresh: boolean): _string; virtual;

    function IndexedPrefix: _string;

    function GenerateIndexName(Name: _string): _string; virtual;
    function DecodeFieldIndex(FieldName: _string): integer; virtual;
    function MaxIdentLength: integer; virtual;

    function IsSubstituteParamName: boolean; virtual;
    procedure AddParam(SB: _StringBuilder; FieldDesc: TFieldDesc;
      const StatementType: TStatementType;
      Index: integer = -1;
      Old: boolean = False); reintroduce; overload;
    procedure AddParam(SB: _StringBuilder; FieldDesc: TFieldDesc;
      const StatementType: TStatementType;
      const ParamType: TParamType;
      Index: integer = -1;
      Old: boolean = False); overload; virtual;

    procedure AddFieldToInsertSQL(FieldDesc: TCRFieldDesc; const Index: integer = -1); virtual;
    procedure AddFieldToUpdateSQL(FieldDesc: TCRFieldDesc;
      const ModifiedFieldsOnly: boolean;
      const Index: integer = -1); virtual;
    procedure AddFieldToRefreshSQL(FieldDesc: TCRFieldDesc); virtual;
    procedure AddFieldToCondition(SB: _StringBuilder;
      FieldDesc: TCRFieldDesc;
      const StatementType: TStatementType;
      const ModifiedFieldsOnly: boolean;
      const Index: integer = -1); virtual;

    procedure GenerateInsertSQL(
      const KeyAndDataFields: TKeyAndDataFields;
      const ModifiedFieldsOnly: boolean;
      const Index: integer = -1); virtual;
    procedure GenerateUpdateSQL(
      const KeyAndDataFields: TKeyAndDataFields;
      const ModifiedFieldsOnly: boolean;
      const Index: integer = -1); virtual;
    procedure GenerateDeleteSQL(
      const KeyAndDataFields: TKeyAndDataFields;
      const ModifiedFieldsOnly: boolean;
      const Index: integer = -1); virtual;
    procedure GenerateLockSQL(
      const KeyAndDataFields: TKeyAndDataFields;
      const Index: integer = -1); virtual;
    procedure GenerateRefreshSQLSelectPart(const KeyAndDataFields: TKeyAndDataFields); virtual;
    procedure GenerateRefreshSQLFromPart; virtual;
    procedure GenerateRefreshSQL(
      const KeyAndDataFields: TKeyAndDataFields;
      const ModifiedFieldsOnly: boolean); virtual;
    procedure GenerateRefreshQuickSQL(const KeyAndDataFields: TKeyAndDataFields); virtual;
    procedure GenerateRefreshCheckDeletedSQL(const KeyAndDataFields: TKeyAndDataFields); virtual;

    procedure GenerateConditions(SB: _StringBuilder; const StatementType: TStatementType;
      const ModifiedFieldsOnly: boolean;
      const KeyAndDataFields: TKeyAndDataFields;
      const Index: integer = -1); virtual;// Generate WHERE part for UPDATE, DELETE, REFRESH SQLs

    function GetParamInfoClass: TDAParamInfoClass; virtual;
  public
    constructor Create(AOwner: TDADataSetService); virtual;
    destructor Destroy; override;

    // Generate insert, update, delete or refresh SQL statements
    function GenerateSQLforUpdTable(TableInfo: TCRTableInfo;
      const KeyAndDataFields: TKeyAndDataFields;
      const StatementType: TStatementType;
      const ModifiedFieldsOnly: boolean;
      Params: TDAParams;
      const Index: integer = -1): _string; virtual;

    function GenerateSQL(const StatementType: TStatementType;
      const ModifiedFieldsOnly: boolean;
      Params: TDAParams;
      const Index: Integer = -1): _string; virtual;

    function GenerateTableSQL(const TableName, OrderFields: _string): _string; virtual;
    function GenerateSelectValues(const ValuesList: _string): _string; virtual;

    property ParamsInfo: TDAParamsInfo read FParamsInfo;
    property DesignMode: boolean read FDesignMode write FDesignMode;
  end;

  TLockTrStarted = (ltsNone, ltsOnLock, ltsOnLockCachedUpdates, ltsBeforeLockCachedUpdates);

  TDADataSetUpdater = class(TDataSetUpdater)
  protected

    FDataSet: TCustomDADataSet;
    FDataSetService: TDADataSetService;

    FFixedTransaction: TDATransaction;
    FLockTrStarted: TLockTrStarted; // when transaction has been started
    FUpdateComponents: array [TStatementType] of TComponent;
    FUpdateQuery: TComponent;

    FBatchSQLs: _StringBuilder;
    FBatchParams: TDAParams;
    FBatchStatements: integer;

    FRefreshInUpdate: boolean; // for correct behavior of refresh record
    FCheckOnLock: boolean;

    function UseParamType: boolean; virtual; //This function indicates ParamType using in PerformSQL
    function NeedReturnParams: boolean; virtual; // перевести в IsReturnParams
    function RetunParamsAsFields: boolean; virtual;

    function RefreshAfterInsertAllowed: boolean; virtual; // сделать нормально

    function IsNeedInsertPreconnect: boolean; virtual;
    function IsNeedEditPreconnect: boolean; virtual;
    function IsPreconnected: boolean; virtual;
    function CanRefreshByLock: boolean; virtual;

    function GetICommand: TCRCommand;
    function GetIRecordSet: TCRRecordSet;
    procedure CheckIRecordSet;
    function GetLockMode: TLockMode;
    function GetUpdateObject: TCustomDAUpdateSQL;
    function GetUpdateSQL(StatementType: TStatementType): _string;
    function UsedConnection: TCustomDAConnection;
    function UsedTransaction: TDATransaction;
    function UsedUpdateTransaction: TDATransaction;
    procedure SetRowsAffected(Value: Integer);
    procedure BeginConnection;
    procedure EndConnection;

    procedure SetIdentityFieldValue;
    function GetIdentityFieldValue(var Value: variant): boolean; virtual;
    function GetSavepointName(CachedUpdates: boolean): _string;
    function GetLockSavepointName: _string;
    function GetLockCachedUpdatesSavepointName: _string;
    function SavepointAllowed: boolean; virtual;
    procedure SetSavepoint(SavepointName: _string; CachedUpdates: boolean); virtual;
    procedure SetLockSavepoint;
    procedure SetLockCachedUpdatesSavepoint;
    procedure RollbackToSavepoint(SavepointName: _string; CachedUpdates: boolean); virtual;
    procedure RollbackToLockSavepoint;
    procedure RollbackToLockCachedUpdatesSavepoint;
    procedure ResetLockCachedUpdatesSavepoint;

    function FieldByParamName(var ParamName: _string; var Old: boolean; var AFieldNo: integer): TField; virtual;
    function GetUpdateStatement(const StatementType: TStatementType): _string; virtual;
    procedure CheckUpdateQuery(const StatementType: TStatementType); virtual;
    procedure SetUpdateQueryOptions(const StatementType: TStatementType); virtual;
    procedure CheckUpdateSQL(const SQL: _string; const StatementTypes: TStatementTypes;
      out ParamsInfo: TDAParamsInfo; UseGenerator: boolean = True); virtual;
    procedure UpdateExecute(const StatementTypes: TStatementTypes); virtual;

  { RefreshQuick }
    function IsRefreshQuickField(FieldDesc: TFieldDesc): boolean; virtual;
    procedure SaveMaxRefreshQuickValue(FieldDesc: TFieldDesc; const Value: variant); virtual;
    
    procedure PrepareAppend; virtual;
    procedure PrepareUpdate; virtual;
    procedure PrepareDelete; virtual;
    procedure UnPrepareAppendUpdateDelete; virtual;
    procedure PrepareCachedUpdate; virtual;
    procedure FinishCachedUpdate; virtual;
    procedure UnPrepareCachedUpdate; virtual;
    procedure UnLockCachedUpdate; virtual;
    function PerformLock: boolean; virtual;
    function PerformUnLock: boolean; virtual;
    procedure EndUpdate(Success: boolean);
    function PerformAppend: boolean; override;
    function PerformDelete: boolean; override;
    function PerformUpdate: boolean; override;
    function PerformRefreshRecord: boolean; virtual;
    function PerformRefreshRecordInUpdate: boolean; virtual;
    function PerformRefreshQuick(const CheckDeleted: boolean): boolean; virtual;
    function PerformPSUpdateRecord(UpdateKind: TUpdateKind; Delta: TDataSet): boolean;
    function CacheChanged: boolean; override;
    function CacheApplied: boolean; override;
    function CacheCanceled: boolean; override;

    function BatchUpdate: boolean; override;
    function CanFlushBatch: boolean; override;
    procedure ClearBatch;
    procedure FlushBatch; override;
    function PrepareBatch(SQL: _string): _string; virtual;

    procedure UnprepareUpdateObjects;
    function LockCompare(const Value1, Value2: variant): boolean; virtual;

  public
    constructor Create(AOwner: TDataSetService); override;
    destructor Destroy; override;

    function SelectDbValue(const OperationName, SQL: _string): variant;
    function GetDefaultExpressionValue(DefExpr: _string; var Value: variant): boolean; virtual;

    procedure WriteUQParams(ParamsInfo: TDAParamsInfo; const StatementTypes: TStatementTypes);
    function PerformSQL(const SQL: _string; const StatementTypes: TStatementTypes): boolean; virtual;

    property UpdateQuery: TComponent read FUpdateQuery;
    property CheckOnLock: boolean read FCheckOnLock write FCheckOnLock;
  end;

  TDADataSetService = class(TDataSetService)
  private

    function GetUpdatingTableInfo: TCRTableInfo;
  protected
    FDataSet: TCustomDADataSet;
    FUpdater: TDADataSetUpdater;
    FSQLGenerator: TDASQLGenerator;
    FUpdatingTableInfoIdx: integer;
    FUpdTableIsArtificial: boolean;
    FFieldsExtInited: boolean;
    FIdentityField: TCRFieldDesc;
    FKeyGeneratorField: TField;
    FIsAnyFieldCanBeModified: boolean;

    FCachedKeyFieldDescs: array[Boolean] of TFieldDescArray;
    FKeyFieldDescsIsCached: array[Boolean] of Boolean;
    FCachedDataFieldDescs: array[Boolean] of TFieldDescArray;
    FDataFieldDescsIsCached: array[Boolean] of Boolean;

    procedure SetDataSetUpdater(Value: TDataSetUpdater); override;

    procedure CreateSQLGenerator; virtual;
    procedure FreeSQLGenerator;
    procedure SetSQLGenerator(Value: TDASQLGenerator); virtual;

    procedure PreInitCursor; override; // occured just after Data.Open
    procedure InitCursor; virtual;
    procedure CloseCursor; virtual;

    procedure InitUpdatingTableIdx; virtual;
    procedure InitUpdatingTableFields; virtual;
    procedure InitUpdatingTable; virtual;

    procedure SetFieldOrigin(Field: TField; FieldDesc: TCRFieldDesc); virtual;
    procedure FillFieldsDefaultValues; virtual;
    procedure SetFieldsReadOnly; virtual;
    procedure SetFieldsReadOnlyOld;
    procedure GetExtFieldsInfo;
    function ExtFieldsInfoIsInternal: boolean; virtual;
    procedure RequestFieldsInfo(Tables: TExtTablesInfo; Columns: TColumnsInfo); virtual;

    function DetectIdentityField: TCRFieldDesc; virtual;
    function DetectKeyGeneratorField: TField; virtual;
    function DetectHiddenFields: TFieldArray; virtual;

    function DetectCanModify: boolean; virtual;

    function CanUseAllKeyFields: boolean; virtual;
    function IdentityFieldIsData: boolean; virtual;
    procedure FillKeyFieldDescs(out KeyFieldDescs: TFieldDescArray; KeyFields: _string; CheckKeyFields: boolean = True); overload;
    procedure FillKeyFieldDescs(out KeyFieldDescs: TFieldDescArray; ForceUseAllKeyFields: boolean); overload; virtual;
    procedure FillDataFieldDescs(out DataFieldDescs: TFieldDescArray; ForceUseAllKeyFields: boolean); virtual;

    function GetRecCount: integer; virtual;
    procedure BreakExec; virtual;
    function Executing: boolean; virtual;
    function GetCurrentSchema: _string; virtual;

    procedure InitMasterParams(Params: TDAParams); virtual;

  { XML }
    procedure WriteFieldXMLAttributeType(Field: TField; FieldDesc: TFieldDesc; const FieldAlias: _string;
      XMLWriter: XMLTextWriter); override;

    function GetIConnection: TCRConnection;
    function GetICommand: TCRCommand;
    function GetIRecordSet: TCRRecordSet;
    procedure CheckIRecordSet;
    function GetTablesInfo: TCRTablesInfo;
    function UsedConnection: TCustomDAConnection;
    function IsFullRefresh: boolean;
    function IsDMLRefresh: boolean;
    function IsInCacheProcessing: boolean;
    function IsAutoCommit: boolean;
    function IsFetchAll: boolean;
    procedure SetAutoCommit(Value: boolean);
    procedure SetNeedAddRef(Value: boolean);
    function GetKeyFields: _string;
    procedure BeginConnection;
    procedure EndConnection;

    function PreventPSKeyFields(var PSKeyFields: string): boolean; virtual;
    function NeedPreparePSExecuteCommand: boolean; virtual;
  public
    constructor Create(AOwner: TMemDataSet); override;
    destructor Destroy; override;

    procedure ClearCachedKeyFieldDescs;
    procedure ClearCachedDataFieldDescs;
    procedure ClearFieldDescRefs; virtual;
    procedure GetKeyFieldDescs(out KeyFieldDescs: TFieldDescArray; ForceUseAllFields: boolean = False);
    procedure GetDataFieldDescs(out DataFieldDescs: TFieldDescArray; ForceUseAllFields: boolean = False);
    procedure GetKeyAndDataFields(out KeyAndDataFields: TKeyAndDataFields; ForceUseAllFields: boolean);

    function GetDBKeyList(TableName: _string; IndexName: _string = ''): _string; virtual;
    function OpenNext: boolean; virtual;
    function NeedParamValuesOnPrepare: boolean; virtual;
    function QuoteName(const AName: _string): _string;

    property IdentityField: TCRFieldDesc read FIdentityField;
    property KeyGeneratorField: TField read FKeyGeneratorField;
    property UpdatingTableInfoIdx: integer read FUpdatingTableInfoIdx;
    property UpdatingTableInfo: TCRTableInfo read GetUpdatingTableInfo;
    property Updater: TDADataSetUpdater read FUpdater;
    property SQLGenerator: TDASQLGenerator read FSQLGenerator;
  end;

{ TCustomDADataSet }

  TRefreshOption = (roAfterInsert, roAfterUpdate, roBeforeEdit);
  TRefreshOptions = set of TRefreshOption;


  TBeforeExecuteEvent = procedure (Sender: TObject) of object;
  TAfterExecuteEvent = procedure (Sender: TObject; Result: boolean) of object;
  TUpdateExecuteEvent = procedure (Sender: TDataSet; StatementTypes: TStatementTypes;
    Params: TDAParams) of object;
  TBeforeFetchEvent = procedure (DataSet: TCustomDADataSet; var Cancel: boolean) of object;
  TAfterFetchEvent = procedure (DataSet: TCustomDADataSet) of object;

  TQuickOpenInfo = record
    OldActive: boolean;
    OldDebug: boolean;
    OldFetchAll: boolean;
    OldFetchRows: integer;
  end;

  TDADataSetOptions = class (TPersistent)
  private
    FSetFieldsReadOnly: boolean;
    FRequiredFields: boolean;
    FStrictUpdate: boolean;
    FNumberRange: boolean;
    FQueryRecCount: boolean;
    FAutoPrepare: boolean;
    FReturnParams: boolean;
    FTrimFixedChar: boolean;
    FTrimVarChar: boolean;
    FSetEmptyStrToNull: boolean;
    FLongStrings: boolean;
    FRemoveOnRefresh: boolean;
    FFlatBuffers: boolean;
    FQuoteNames: boolean;
  {$IFDEF HAVE_COMPRESS}
    FCompressBlobMode: TCompressBlobMode;
  {$ENDIF}
    FFullRefresh: boolean;
    FLocalMasterDetail: boolean;
    FFieldsOrigin: boolean;
    FDefaultValues: boolean;
    FExtendedFieldsInfo: boolean;
    FUpdateBatchSize: integer;
    FUpdateAllFields: boolean;
    FPrepareUpdateSQL: boolean;
    FEnableBCD: boolean;
  {$IFDEF VER6P}
  {$IFNDEF FPC}
    FEnableFMTBCD: boolean;
  {$ENDIF}
  {$ENDIF}

    procedure SetRequiredFields(Value: boolean);
    procedure SetNumberRange(Value: boolean);
    procedure SetTrimFixedChar(Value: boolean);
    procedure SetTrimVarChar(Value: boolean);
    procedure SetSetEmptyStrToNull(Value: boolean);
    procedure SetLongStrings(Value: boolean);
    procedure SetAutoPrepare(Value: boolean);
    procedure SetFlatBuffers(const Value: boolean);
    function GetDetailDelay: integer;
    procedure SetDetailDelay(Value: integer);
  {$IFDEF HAVE_COMPRESS}
    procedure SetCompressBlobMode(Value: TCompressBlobMode);
  {$ENDIF}
    procedure SetLocalMasterDetail(Value: boolean);
    function GetCacheCalcFields: boolean;
    procedure SetCacheCalcFields(Value: boolean);
    procedure SetQuoteNames(Value: boolean);
    procedure SetFieldsOrigin(Value: boolean);
    procedure SetDefaultValues(Value: boolean);
    procedure SetExtendedFieldsInfo(Value: boolean);
    procedure SetEnableBCD(Value: boolean);
  {$IFDEF VER6P}
  {$IFNDEF FPC}
    procedure SetEnableFMTBCD(Value: boolean);
  {$ENDIF}
  {$ENDIF}

  protected
    FOwner: TCustomDADataSet;

    procedure AssignTo(Dest: TPersistent); override;

    property FullRefresh: boolean read FFullRefresh write FFullRefresh default False;
    property TrimVarChar: boolean read FTrimVarChar write SetTrimVarChar default False;
    property SetEmptyStrToNull: boolean read FSetEmptyStrToNull write SetSetEmptyStrToNull default False;
    property ExtendedFieldsInfo: boolean read FExtendedFieldsInfo write SetExtendedFieldsInfo default False;
    property EnableBCD: boolean read FEnableBCD write SetEnableBCD default False;
  {$IFDEF VER6P}
  {$IFNDEF FPC}
    property EnableFMTBCD: boolean read FEnableFMTBCD write SetEnableFMTBCD default False;
  {$ENDIF}
  {$ENDIF}

  public
    constructor Create(Owner: TCustomDADataSet);

    property SetFieldsReadOnly: boolean read FSetFieldsReadOnly write FSetFieldsReadOnly default True;
    property RequiredFields: boolean read FRequiredFields write SetRequiredFields default True;
    property StrictUpdate: boolean read FStrictUpdate write FStrictUpdate default True;
    property PrepareUpdateSQL: boolean read FPrepareUpdateSQL write FPrepareUpdateSQL default False;
    property NumberRange: boolean read FNumberRange write SetNumberRange default False;
    property QueryRecCount: boolean read FQueryRecCount write FQueryRecCount default False;
    property AutoPrepare: boolean read FAutoPrepare write SetAutoPrepare default False;
    property ReturnParams: boolean read FReturnParams write FReturnParams default False;
    property TrimFixedChar: boolean read FTrimFixedChar write SetTrimFixedChar default True;
    property LongStrings: boolean read FLongStrings write SetLongStrings default True;
    property FlatBuffers: boolean read FFlatBuffers write SetFlatBuffers default False;
    property RemoveOnRefresh: boolean read FRemoveOnRefresh write FRemoveOnRefresh default True;
    property QuoteNames: boolean read FQuoteNames write SetQuoteNames default False;
    property DetailDelay: integer read GetDetailDelay write SetDetailDelay default 0;
  {$IFDEF HAVE_COMPRESS}
    property CompressBlobMode: TCompressBlobMode read FCompressBlobMode write SetCompressBlobMode default cbNone;
  {$ENDIF}
    property LocalMasterDetail: boolean read FLocalMasterDetail write SetLocalMasterDetail default False;
    property CacheCalcFields: boolean read GetCacheCalcFields write SetCacheCalcFields default False;
    property FieldsOrigin: boolean read FFieldsOrigin write SetFieldsOrigin default False;
    property DefaultValues: boolean read FDefaultValues write SetDefaultValues default False;
    property UpdateBatchSize: Integer read FUpdateBatchSize write FUpdateBatchSize default 1;
    property UpdateAllFields: boolean read FUpdateAllFields write FUpdateAllFields default False;
  end;

  TCustomDADataSet = class (TMemDataSet)
  private
    FConnection: TCustomDAConnection;
    FTransaction: TDATransaction;
    FUpdateTransaction: TDATransaction;
    FParams: TDAParams; // for easy reference of FCommand.Params
    FMacros: TMacros; // for easy reference of FCommand.Macros
    FFetchRows: integer;
    FDebug: boolean;
    FReadOnly: boolean;
    FUniDirectional: boolean;
    FAutoCommit: boolean;
    FUpdateObject: TCustomDAUpdateSQL;
    FRefreshOptions: TRefreshOptions;
    FOptions: TDADataSetOptions;
    FBaseSQL: _string;
    FLockMode: TLockMode;
    FKeyFields: _string;
    FDMLRefresh: boolean;
    FFindKeyOptions: TLocateExOptions;
    FDisconnected: boolean;

    FBeforeExecute: TBeforeExecuteEvent;
    FAfterExecute: TAfterExecuteEvent;
    FBeforeFetch: TBeforeFetchEvent;
    FAfterFetch: TAfterFetchEvent;
    FBeforeUpdateExecute: TUpdateExecuteEvent;
    FAfterUpdateExecute: TUpdateExecuteEvent;

    procedure SetUpdateTransaction(Value: TDATransaction);
    function GetSQL: _TStrings;
    procedure SetSQL(Value: _TStrings);
    procedure SetFetchRows(Value: integer);
    function GetParams: TDAParams;
    procedure SetParams(Value: TDAParams);
    function GetParamCount: word;
    function GetParamCheck: boolean;
    procedure SetParamCheck(Value: boolean);
    function GetMacros: TMacros;
    procedure SetMacros(Value: TMacros);
    function GetMacroCount: word;
    function GetRowsAffected: integer;
    procedure SetUniDirectional(Value: boolean);
    procedure SetAutoCommit(Value: boolean);
    //procedure SetUpdateMode(const Value: TUpdateMode);
    procedure SetUpdateObject(Value: TCustomDAUpdateSQL);
    procedure SetOptions(Value: TDADataSetOptions);
    procedure SaveModifiedSQL(NewSQL: _string);
    function GetBaseSQL: _string;

  {$IFDEF VER5P}
  {$IFDEF WITH_IPROVIDER}
  protected
  { IProviderSupport }
    FOldKeyFields: _string; // To PSGetKeyFields after closing table (see SDAC 3034)
    FOldTableName: _string; // PSGetTableName must return right value even after DataSet.Close
    function PSInTransaction: Boolean; override;
    procedure PSStartTransaction; override;
    procedure PSEndTransaction(Commit: Boolean); override;
    procedure PSExecute; override;
    function PSExecuteStatement(const ASQL: string; AParams: TParams;
      {$IFDEF CLR}var ResultSet: TObject{$ELSE}ResultSet: Pointer = nil{$ENDIF}): Integer; override;
    function PSGetParams: DB.TParams; override;
    function PSGetQuoteChar: string; override;
    function PSGetTableName: string; override;
    function PSIsSQLBased: Boolean; override;
    function PSIsSQLSupported: Boolean; override;
    procedure PSReset; override;
    procedure PSSetParams(AParams: DB.TParams); override;
    procedure PSSetCommandText(const CommandText: string); override;
    function PSUpdateRecord(UpdateKind: TUpdateKind; Delta: TDataSet): Boolean; override;
    function PSGetDefaultOrder: TIndexDef; override;
    function PSGetKeyFields: string; override;
  {$ENDIF}
  {$ENDIF}

  protected
    FRowsAffected: integer;

    FIRecordSet: TCRRecordSet;
    FICommand: TCRCommand;  // for perf
    FCommand: TCustomDASQL;
    FDataSetService: TDADataSetService;

    FFilterSQL: _string;  //CR ODAC (8387) vs (8751)
    FUpdatingTable: _string;//Can not delete because FTablesInfo created only on ~open

    FDesignCreate: boolean;
    FNonBlocking: boolean;
    FLockDebug: boolean; // locking trans debug info
    FUpdateSQL: array [TStatementType] of _TStrings; // SQLInsert, SQLUpdate etc

    FRecordCount: integer;
    FLastInsertId: int64;

    FFetchAll: boolean;
    FFetchCanceled: boolean;

    FStreamedOpen: Boolean;

    function GetFieldObject(Field: TField): TSharedObject; overload;
    function GetFieldObject(FieldDesc: TFieldDesc): TSharedObject; overload;

    procedure CheckInactive; override;
    procedure CreateIRecordSet; override;
    procedure FreeIRecordSet;
    procedure SetIRecordSet(Value: TData{TRecordSet}); override;
    procedure CheckIRecordSet;

    procedure CreateCommand; virtual;
    procedure FreeCommand;
    procedure SetCommand(Value: TCustomDASQL);

    procedure SetDataSetService(Value: TDataSetService); override;

    function CreateOptions: TDADataSetOptions; virtual;

    procedure Loaded; override;
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;

    procedure SetConnection(Value: TCustomDAConnection);
    function UsedConnection: TCustomDAConnection; virtual;
    procedure CheckConnection; virtual;
    procedure BeginConnection(NoConnectCheck: boolean = True); virtual;
    procedure EndConnection; virtual;
    procedure Disconnect(NeedClose: boolean = True); virtual;
    procedure ConnectChange(Sender: TObject; Connecting: boolean); virtual;

    function IsTransactionStored: boolean;
    function GetTransaction: TDATransaction; virtual;
    procedure SetTransaction(Value: TDATransaction); virtual;
    function GetUsedTransaction: TDATransaction;
    function UsedTransaction: TDATransaction; virtual;
    function UsedUpdateTransaction: TDATransaction; virtual;

  { Fields }
    procedure SetKeyFields(const Value: _string); virtual;
    function GetDataTypesMap: TDataTypesMapClass; override;

  { TablesInfo }
    function GetTablesInfo: TCRTablesInfo;
    function GetSQLInfo: TSQLInfo;
    procedure SetUpdatingTable(const Value: _string); virtual;

  { Open/Close }
    procedure SetActive(Value: Boolean); override;
    procedure BeforeOpenCursor(InfoQuery: boolean); virtual;
    procedure OpenCursor(InfoQuery: boolean); override;
    procedure AfterOpenCursor(InfoQuery: boolean); virtual;
    procedure CloseCursor; override;
    function GetCursor: TCRCursor;
    function GetCRCursor: TCRCursor; virtual;
    procedure SetCRCursor(Value: TCRCursor); virtual;

    procedure GetCurrentKeys(out KeyFields: TFieldArray; out Values: variant);
    procedure DataReopen; override;
    procedure InternalRefresh; override;
    procedure InternalRefreshQuick(const CheckDeleted: boolean); virtual;
    procedure InternalExecute; virtual;
    procedure InternalClose; override;
    procedure DoAfterOpen; override;

    procedure SetRefreshOptions(Value: TRefreshOptions); virtual;
    function GetFetchAll: boolean; virtual;
    procedure SetFetchAll(Value: boolean); virtual;
    function SQLAutoGenerated: boolean; virtual;

    function DoOpenNext: boolean;

    procedure QuickOpen(var Info: TQuickOpenInfo; Refresh: boolean = False); virtual;
    procedure Restore(const Info: TQuickOpenInfo; RestoreActive: boolean = True); virtual;

  { Edit }
    procedure SetReadOnly(Value: boolean); virtual;

    procedure InternalEdit; override;
    procedure InternalDelete; override;
    procedure InternalInsert; override;
    procedure InternalCancel; override;
    procedure InternalPost; override;
    procedure InternalDeferredPost; override;

    function GetUpdateSQLStatementTypes: TStatementTypes; virtual;
    function GetUpdateSQLIndex(Index: integer): _TStrings;
    procedure SetUpdateSQLIndex(Index: integer; Value: _TStrings);

    procedure SetFilterSQL(const Value: _string); virtual; //CR ODAC (8387) vs (8751)
    procedure SetFiltered(Value: boolean); override;

    function GetCanModify: boolean; override;
  {$IFNDEF FPC}
    procedure SetStateFieldValue(State: TDataSetState; Field: TField; const Value: Variant); override; // Need to support int64 fields on PerformSQL in RefreshRecord
  {$ENDIF}
    function CanRefreshField(Field: TField): boolean; virtual;
    procedure AssignFieldValue(Param: TDAParam; Field: TField; Old: boolean); overload; virtual;
    procedure AssignFieldValue(Param: TDAParam; FieldDesc: TFieldDesc; Old: boolean); overload; virtual;
    procedure AssignFieldType(Param: TDAParam; FieldDesc: TFieldDesc); virtual;
    procedure SetDefaultExpressionValues; override;

  { Master/Detail }
    function UseLocalMasterDetailFilter: boolean; override;
    function NeedDetailRefresh(Param: TDAParam; FieldValue: TSharedObject): boolean; virtual;
    function MDLinksRefreshed: boolean; override;
    procedure RefreshDetail(Sender: TObject); override;
    function SetMasterParams(AParams: TDAParams): boolean;
    procedure MDPropertiesChanged; override;
    procedure InitMasterParams;

    procedure AssembleSQL;
    procedure InternalCreateProcCall(const Name: _string; NeedDescribe: boolean; IsQuery: boolean = False);
    procedure ScanMacros(Sender: TObject {$IFNDEF FPC}= nil{$ENDIF}); virtual;

    procedure DefineProperties(Filer: TFiler); override;

    procedure AssignTo(Dest: TPersistent); override;

    procedure DoBeforeExecute; virtual;
    procedure DoAfterExecute(Result: boolean); virtual;
    procedure DoAfterExecFetch(Result: boolean);
    procedure DoAfterFetchAll(Result: boolean);
    procedure DoAfterScroll; override;
    procedure DoOnBeforeFetch(out Cancel: boolean); virtual;
    procedure DoOnAfterFetch; virtual;
    procedure DoOnDataChanged;
    procedure DoOnFieldsChanged;

    function GetRecordCount: integer; override;
    function GetIsQuery: boolean; virtual;

  { Before / After UpdateExecute }
    function AssignedBeforeUpdateExecute: boolean; virtual;
    procedure DoBeforeUpdateExecute(Sender: TDataSet; StatementTypes: TStatementTypes;
      Params: TDAParams); virtual;
    function AssignedAfterUpdateExecute: boolean; virtual;
    procedure DoAfterUpdateExecute(Sender: TDataSet; StatementTypes: TStatementTypes;
      Params: TDAParams); virtual;

  { KeySequence }
    procedure InternalOpen; override;

  { SQL Modifications }
    function SQLGetFrom(SQLText: _string): _string; virtual;
    function SQLAddWhere(SQLText, Condition: _string): _string; virtual;
    function SQLDeleteWhere(SQLText: _string): _string; virtual;
    function SQLGetWhere(SQLText: _string): _string; virtual;
    function SQLSetOrderBy(SQLText: _string; Fields: _string): _string; virtual;
    function SQLGetOrderBy(SQLText: _string): _string; virtual;

    procedure CheckSQL; virtual;
    function GetFinalSQL: _string; virtual;

  { Misc }
  {$IFDEF CLR}
    procedure DataEvent(Event: TDataEvent; Info: TObject); override;
  {$ELSE} {$IFDEF FPC}
    procedure DataEvent(Event: TDataEvent; Info: PtrInt); override;
  {$ELSE} {$IFDEF VER16P}
    procedure DataEvent(Event: TDataEvent; Info: NativeInt); override;
  {$ELSE}
    procedure DataEvent(Event: TDataEvent; Info: Longint); override;
  {$ENDIF} {$ENDIF} {$ENDIF}

    function GetNextRecord: Boolean; override;

   { TablesInfo }
    property TablesInfo: TCRTablesInfo read GetTablesInfo;
    property SQLInfo: TSQLInfo read GetSQLInfo;
    //property IdentityField: TField read FIdentityField;
    //property UpdatingTableInfoIdx: integer read FUpdatingTableInfoIdx;
    property UpdatingTable: _string read FUpdatingTable write SetUpdatingTable; // Does not need for public use
    property Transaction: TDATransaction read GetTransaction write SetTransaction stored IsTransactionStored;
    property UpdateTransaction: TDATransaction read FUpdateTransaction write SetUpdateTransaction;
    property AutoCommit: boolean read FAutoCommit write SetAutoCommit default True;
    property FetchAll: boolean read GetFetchAll write SetFetchAll default False;
    property UpdateObject: TCustomDAUpdateSQL read FUpdateObject write SetUpdateObject;
    property DMLRefresh: boolean read FDMLRefresh write FDMLRefresh default False;
    property LockMode: TLockMode read FLockMode write FLockMode default lmNone;
    property Cursor: TCRCursor read GetCRCursor write SetCRCursor;

  public
    constructor Create(Owner: TComponent); override;
    destructor Destroy; override;

  { Open/Close }
    procedure Prepare; override;
    procedure UnPrepare; override;

    procedure Execute; virtual;
    procedure ExecSQL;  // for BDE compatibility
    function Executing: boolean;
    function Fetching: boolean;
    function FetchingAll: boolean;
    function Fetched: boolean; virtual;
    procedure BreakExec; virtual;

    procedure Resync(Mode: TResyncMode); override;

    procedure GetDetailLinkFields(MasterFields, DetailFields: {$IFDEF CLR}TObjectList{$ELSE}TList{$ENDIF}); override;

    {for BDE compatibility}
    function FindKey(const KeyValues: array of const): Boolean;
    procedure FindNearest(const KeyValues: array of const);
    procedure GotoCurrent(DataSet: TCustomDADataSet);

    function CreateBlobStream(Field: TField; Mode: TBlobStreamMode): TStream; override;

  { Edit }
    procedure ApplyUpdates(const UpdateRecKinds: TUpdateRecKinds); override;

    property SQLInsert: _TStrings index Integer(stInsert) read GetUpdateSQLIndex write SetUpdateSQLIndex;
    property SQLDelete: _TStrings index Integer(stDelete) read GetUpdateSQLIndex write SetUpdateSQLIndex;
    property SQLUpdate: _TStrings index Integer(stUpdate) read GetUpdateSQLIndex write SetUpdateSQLIndex;
    property SQLRefresh: _TStrings index Integer(stRefresh) read GetUpdateSQLIndex write SetUpdateSQLIndex;
    property SQLLock: _TStrings index Integer(stLock) read GetUpdateSQLIndex write SetUpdateSQLIndex;
    
    procedure RefreshRecord;
    procedure Lock; virtual;
    procedure UnLock;

    function FindParam(const Value: _string): TDAParam;
    function ParamByName(const Value: _string): TDAParam;

    function FindMacro(const Value: _string): TMacro;
    function MacroByName(const Value: _string): TMacro;

  { SQL Modifications }
    procedure SaveSQL;
    procedure RestoreSQL;
    function SQLSaved: boolean;

    procedure AddWhere(Condition: _string);
    procedure DeleteWhere;
    procedure SetOrderBy(Fields: _string);
    function GetOrderBy: _string;

  { Additional data types }
    function GetField(FieldDesc: TFieldDesc): TField;
    function GetDataType(const FieldName: _string): integer; virtual;
    function GetFieldDesc(const FieldName: _string): TFieldDesc; overload;
    function GetFieldDesc(const FieldNo: integer): TFieldDesc; overload; virtual;
    function GetFieldPrecision(const FieldName: _string): integer;
    function GetFieldScale(const FieldName: _string): integer;
    function GetFieldObject(const FieldName: _string): TSharedObject; overload;

    property Connection: TCustomDAConnection read FConnection write SetConnection;
    property ParamCheck: boolean read GetParamCheck write SetParamCheck default True; // before SQL
    property SQL: _TStrings read GetSQL write SetSQL;
    property FetchRows: integer read FFetchRows write SetFetchRows default 25;
    property Debug: boolean read FDebug write FDebug default False;
    property Params: TDAParams read GetParams write SetParams stored False;
    property ParamCount: word read GetParamCount;
    property Macros: TMacros read GetMacros write SetMacros stored False;
    property MacroCount: word read GetMacroCount;
    property UniDirectional: boolean read FUniDirectional write SetUniDirectional default False;
    property ReadOnly: boolean read FReadOnly write SetReadOnly default False;
    property RowsAffected: integer read GetRowsAffected;
    property IsQuery: boolean read GetIsQuery;
    property RefreshOptions: TRefreshOptions read FRefreshOptions write SetRefreshOptions default [];
    property Options: TDADataSetOptions read FOptions write SetOptions;
    property BaseSQL: _string read GetBaseSQL;
    property FinalSQL: _string read GetFinalSQL;
    property FilterSQL: _string read FFilterSQL write SetFilterSQL;
    property KeyFields: _string read FKeyFields write SetKeyFields;
    property Disconnected: boolean read FDisconnected write FDisconnected;
    property MasterSource;
    property MasterFields;
    property DetailFields;

    property BeforeExecute: TBeforeExecuteEvent read FBeforeExecute write FBeforeExecute;
    property AfterExecute: TAfterExecuteEvent read FAfterExecute write FAfterExecute;
    property BeforeUpdateExecute: TUpdateExecuteEvent read FBeforeUpdateExecute write FBeforeUpdateExecute;
    property AfterUpdateExecute: TUpdateExecuteEvent read FAfterUpdateExecute write FAfterUpdateExecute;
    property BeforeFetch: TBeforeFetchEvent read FBeforeFetch write FBeforeFetch;
    property AfterFetch: TAfterFetchEvent read FAfterFetch write FAfterFetch;
  end;

{ TCustomDASQL }

  TCustomDASQL = class (TComponent)
  private
    FConnection: TCustomDAConnection;
    FTransaction: TDATransaction;
    FSQL: _TStrings;
    FParams: TDAParams;
    FParamCheck: boolean;
    FMacros: TMacros;
    FDebug: boolean;
    FChangeCursor: boolean;

    FBeforeExecute: TBeforeExecuteEvent;
    FAfterExecute: TAfterExecuteEvent;
    {FOnDisconnect: TNotifyEvent;
    FGetFinalSQL: TGetFinalSQLEvent;
    FOnScanMacros: TNotifyEvent;}

    procedure SetTransaction(Value: TDATransaction);
    procedure SetSQL(Value: _TStrings);
    function GetPrepared: boolean;
    procedure SetPrepared(Value: boolean);
    procedure SetParams(Value: TDAParams);
    function GetParamCount: word;
    procedure SetParamCheck(Value: boolean);
    function GetParamValues(ParamName: _string): variant;
    procedure SetParamValues(ParamName: _string; Value: variant);
    procedure SetMacros(Value: TMacros);
    function GetMacroCount: word;
    function GetRowsAffected: integer;

  protected
    FAutoCommit: boolean;
    FICommand: TCRCommand;
    FDataSet: TCustomDADataSet; // dataset that owns
    FDesignCreate: boolean;  // for design-time only
    FNonBlocking: boolean;
    FLockDebug: boolean; // locking trans debug info
    FLockAssembleSQL, FLockMacros, FLockScanParams: boolean;
    FStoredProcName: _string;
    FStoredProcIsQuery: boolean;
    FLastInsertId: int64;

    function IsTransactionStored: boolean;

    procedure CreateICommand; virtual;
    procedure FreeICommand;
    procedure SetICommand(Value: TCRCommand); virtual;
    procedure CheckICommand;

    function CreateParamsObject: TDAParams; virtual;
    function GetDataTypesMap: TDataTypesMapClass; virtual;

    procedure Loaded; override;
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;

    procedure SetAutoCommit(Value: boolean);
    procedure SetConnection(Value: TCustomDAConnection);
    function UsedConnection: TCustomDAConnection; virtual;
    procedure CheckConnection; virtual;
    procedure BeginConnection(NoConnectCheck: boolean = True); virtual;
    procedure EndConnection; virtual;
    procedure Disconnect; virtual;
    procedure ConnectChange(Sender: TObject; Connecting: boolean); virtual;

    function GetTransaction: TDATransaction; virtual;
    function UsedTransaction: TDATransaction; virtual;

    procedure InternalPrepare; virtual;
    procedure InternalUnPrepare; virtual;
    procedure InternalExecute(Iters: integer); virtual;
    procedure InternalCreateProcCall(const Name: _string; NeedDescribe: boolean; IsQuery: boolean = False); virtual;

    procedure DoBeforeExecute; virtual;
    procedure DoAfterExecute(Result: boolean); virtual;

    // for Script
    function ParseSQL(const SQL: _string; Params: TDAParams; RenamePrefix: _string = ''): _string; virtual;

    procedure SQLChanged(Sender: TObject);
    procedure ProcessSQLChanged(LockMacros, SaveBaseSQL: boolean);
    procedure ScanMacros; virtual;
    function GetFinalSQL: _string; virtual;
    procedure SetICommandSQL;
    procedure AssembleSQL; virtual;

    function NeedRecreateProcCall: boolean; virtual;
    procedure CheckSQL(Iters: integer = 1); virtual;

    function IsInOutParamSupported: boolean; virtual;
    function NeedConvertEOLForBlob: boolean; virtual;
    procedure AssignParam(ParamDesc: TParamDesc; Param: TDAParam); virtual;
    procedure AssignParamValue(ParamDesc: TParamDesc; Param: TDAParam); virtual;
    procedure AssignParamDesc(Param: TDAParam; ParamDesc: TParamDesc); virtual;
    procedure AssignParamDescValue(Param: TDAParam; ParamDesc: TParamDesc); virtual;
    procedure CreateParams; overload; virtual;
    procedure CreateParams(Params: TDAParams; ParamDescs: TParamDescs); overload; virtual;
    procedure WriteParams(WriteValue: boolean = True); virtual;
    procedure ReadParams; virtual;
    procedure UpdateParams;
    function FindResultParam: TDAParam; virtual;

    procedure DefineProperties(Filer: TFiler); override;
    procedure ReadParamData(Reader: TReader);
    procedure WriteParamData(Writer: TWriter);
    procedure ReadMacroData(Reader: TReader);
    procedure WriteMacroData(Writer: TWriter);
    procedure ReadStoredProcName(Reader: TReader);
    procedure WriteStoredProcName(Writer: TWriter);
    procedure SetStoredProcName(const StoredProcName: _string);
    procedure ReadStoredProcIsQuery(Reader: TReader);
    procedure WriteStoredProcIsQuery(Writer: TWriter);

    procedure AssignTo(Dest: TPersistent); override;

    property Transaction: TDATransaction read GetTransaction write SetTransaction stored IsTransactionStored;
    property AutoCommit: boolean read FAutoCommit write SetAutoCommit default False;
    property StoredProcIsQuery: boolean read FStoredProcIsQuery write FStoredProcIsQuery;

  public
    constructor Create(Owner: TComponent); override;
    destructor Destroy; override;

    procedure Prepare; virtual;
    procedure UnPrepare; virtual;

    procedure Execute; overload; virtual;
    procedure Execute(Iters: integer); overload; virtual;

    function Executing: boolean;
    function WaitExecuting(TimeOut: integer = 0): boolean;

    function FindParam(const Value: _string): TDAParam;
    function ParamByName(const Value: _string): TDAParam;

    function FindMacro(const Value: _string): TMacro;
    function MacroByName(const Value: _string): TMacro;

    property Connection: TCustomDAConnection read FConnection write SetConnection;
    property ParamCheck: boolean read FParamCheck write SetParamCheck default True; // before SQL
    property SQL: _TStrings read FSQL write SetSQL;
    property Prepared: boolean read GetPrepared write SetPrepared;
    property Params: TDAParams read FParams write SetParams stored False;
    property ParamCount: word read GetParamCount;
    property ParamValues[ParamName: _string]: variant read GetParamValues write SetParamValues; default;
    property Macros: TMacros read FMacros write SetMacros stored False;
    property MacroCount: word read GetMacroCount;
    property Debug: boolean read FDebug write FDebug default False;
    property ChangeCursor: boolean read FChangeCursor write FChangeCursor;
    property RowsAffected: integer read GetRowsAffected;
    property FinalSQL: _string read GetFinalSQL;

    property BeforeExecute: TBeforeExecuteEvent read FBeforeExecute write FBeforeExecute;
    property AfterExecute: TAfterExecuteEvent read FAfterExecute write FAfterExecute;
  end;

{ TDAMetaData }

  TDAMetaData = class(TMemDataSet)
  private
    FConnection: TCustomDAConnection;
    FTransaction: TDATransaction;
    FMetaDataKind: _string;
    FRestrictions: _TStrings;
    FIMetaData: TCRMetaData;
    FDesignCreate: boolean;

    procedure SetConnection(Value: TCustomDAConnection);
    procedure ConnectChange(Sender: TObject; Connecting: boolean);
    procedure SetMetaDataKind(const Value: _string);
    procedure SetRestrictions(Value: _TStrings);
    procedure RestrictionsChanged(Sender: TObject);

  protected
    procedure Loaded; override;
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;

    function UsedConnection: TCustomDAConnection;
    function UsedTransaction: TDATransaction; virtual;
    function GetTransaction: TDATransaction; virtual;
    procedure SetTransaction(Value: TDATransaction); virtual;
    function IsTransactionStored: boolean;
    procedure BeginConnection; virtual;
    procedure EndConnection; virtual;
    procedure CheckIMetaData;
    procedure OpenCursor(InfoQuery: boolean); override;
    procedure InternalOpen; override;
    procedure CloseCursor; override;

    property Transaction: TDATransaction read GetTransaction write SetTransaction stored IsTransactionStored;

  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    procedure GetMetaDataKinds(List: _TStrings);
    procedure GetRestrictions(List: _TStrings; const MetaDataKind: _string);

    property Connection: TCustomDAConnection read FConnection write SetConnection;
    property MetaDataKind: _string read FMetaDataKind write SetMetaDataKind;
    property Restrictions: _TStrings read FRestrictions write SetRestrictions;
  end;

{ TCustomDAUpdateSQL }

  TCustomDAUpdateSQL = class (TComponent)
  private
    FDataSet: TCustomDADataSet;
    FSQLText: array [TStatementType] of _TStrings;

    FUpdateObject: array [TStatementType] of TComponent;

  protected
    FDesignCreate: boolean;  // for design-time only

    // get/set FSQLText by TStatementType
    function GetSQLIndex(Index: integer): _TStrings;
    procedure SetSQLIndex(Index: integer; Value: _TStrings);

    // get/set FSQLText by TUpdateKind
    function GetSQL(UpdateKind: TUpdateKind): _TStrings; virtual;
    procedure SetSQL(UpdateKind: TUpdateKind; Value: _TStrings);

    // get/set FUpdateObject by TStatementType
    function GetObjectIndex(Index: integer): TComponent;
    procedure SetObjectIndex(Index: integer; Value: TComponent);

    function GetDataSet: TCustomDADataSet; virtual;
    procedure SetDataSet(DataSet: TCustomDADataSet); virtual;
    procedure Loaded; override;
    procedure AssignTo(Dest: TPersistent); override;

    function DataSetClass: TCustomDADataSetClass; virtual;
    function SQLClass: TCustomDASQLClass; virtual;
    procedure CheckUpdateComponent(Component: TComponent); overload;
    procedure CheckUpdateComponent(Component: TComponent; NewDataset: TCustomDADataset); overload;
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
  public
    constructor Create(Owner: TComponent); override;
    destructor Destroy; override;
    procedure Apply(UpdateKind: TUpdateKind); virtual;
    procedure ExecSQL(UpdateKind: TUpdateKind);
    property DataSet: TCustomDADataSet read GetDataSet write SetDataSet;
    property SQL[UpdateKind: TUpdateKind]: _TStrings read GetSQL write SetSQL;

  published
    property InsertSQL: _TStrings index Integer(stInsert) read GetSQLIndex write SetSQLIndex;
    property DeleteSQL: _TStrings index Integer(stDelete) read GetSQLIndex write SetSQLIndex;
    property ModifySQL: _TStrings index Integer(stUpdate) read GetSQLIndex write SetSQLIndex;
    property RefreshSQL: _TStrings index Integer(stRefresh) read GetSQLIndex write SetSQLIndex;
    property LockSQL: _TStrings index Integer(stLock) read GetSQLIndex write SetSQLIndex;

    property InsertObject: TComponent index Integer(stInsert) read GetObjectIndex write SetObjectIndex;
    property DeleteObject: TComponent index Integer(stDelete) read GetObjectIndex write SetObjectIndex;
    property ModifyObject: TComponent index Integer(stUpdate) read GetObjectIndex write SetObjectIndex;
    property RefreshObject: TComponent index Integer(stRefresh) read GetObjectIndex write SetObjectIndex;
    property LockObject: TComponent index Integer(stLock) read GetObjectIndex write SetObjectIndex;
  end;

{ TMacro }

  TMacro = class (TCollectionItem)
  private
    FName: _string;
    FValue: _string;
    FActive: boolean;
    //FOwner: TComponent;

    procedure SetValue(Value: _string);
    procedure SetActive(Value: boolean);

    function GetAsDateTime: TDateTime;
    procedure SetAsDateTime(Value: TDateTime);
    function GetAsFloat: double;
    procedure SetAsFloat(Value: double);
    function GetAsInteger: integer;
    procedure SetAsInteger(Value: integer);
    function GetAsString: _string;
    procedure SetAsString(Value: _string);

  protected
    procedure AssignTo(Dest: TPersistent); override;

    function IsEqual(Value: TMacro): boolean;

    function GetDisplayName: string; override;

  public
    constructor Create(Collection: TCollection); override;

    property AsDateTime: TDateTime read GetAsDateTime write SetAsDateTime;
    property AsFloat: double read GetAsFloat write SetAsFloat;
    property AsInteger: integer read GetAsInteger write SetAsInteger;
    property AsString: _string read GetAsString write SetAsString;

  published
    property Name: _string read FName write FName;
    property Value: _string read FValue write SetValue;
    property Active: boolean read FActive write SetActive default True;
  end;

{ TMacros }

  TMacros = class (TCollection)
  private
    FOwner: TPersistent;

    procedure ReadBinaryData(Stream: TStream);
    //procedure WriteBinaryData(Stream: TStream);

    function GetItem(Index: integer): TMacro;
    procedure SetItem(Index: integer; Value: TMacro);
    procedure NotifyOwner(Item: TMacro);

  protected
    FParserClass: TSQLParserClass;

    procedure AssignTo(Dest: TPersistent); override;

    procedure DefineProperties(Filer: TFiler); override;

    function GetOwner: TPersistent; override;
    procedure Update(Item: TCollectionItem); override;

    function GetMacroValue(Macro: TMacro): _string; virtual;

  public
    constructor Create(Owner: TPersistent);

    procedure Scan(SQL: _string); // ParseSQL
    procedure AssignValues(Value: TMacros);
    function IsEqual(Value: TMacros): boolean;

    function FindMacro(const Value: _string): TMacro;
    function MacroByName(const Value: _string): TMacro;

    procedure Expand(var SQL: _string);

    procedure SetParserClass(Value: TSQLParserClass);

    property Items[Index: integer]: TMacro read GetItem write SetItem; default;
  end;

{ TCRServerEnumerator }

  TCRServerEnumerator = class
  public
    constructor Create; virtual;
    function SetProp(Prop: integer; const Value: variant): boolean; virtual;
    function GetProp(Prop: integer; var Value: variant): boolean; virtual;

    procedure GetServerList(List: _TStrings); virtual;
  end;

  TCRServerEnumeratorClass = class of TCRServerEnumerator;

{ TCustomConnectDialog }

  TLabelSet = (lsCustom, lsEnglish, lsFrench, lsGerman, lsItalian, lsPolish,
    lsPortuguese, lsRussian, lsSpanish);

  TCustomConnectDialog = class(TComponent)
  private
    FConnection: TCustomDAConnection;
    FRetries: word;
    FDialogClass: string;
    FSavePassword: boolean;
    FStoreLogInfo: boolean;
    FUseServerHistory: boolean;
    FNeedConnect: boolean;

    FLabelSet: TLabelSet;
    FCaption: string;
    FUsernameLabel: string;
    FPasswordLabel: string;
    FServerLabel: string;
    FConnectButton: string;
    FCancelButton: string;

    procedure SetCaption(Value: string);
    procedure SetUsernameLabel(Value: string);
    procedure SetPasswordLabel(Value: string);
    procedure SetServerLabel(Value: string);
    procedure SetConnectButton(Value: string);
    procedure SetCancelButton(Value: string);

  protected
    FServerEnumerator: TCRServerEnumerator;

    function GetServerEnumeratorClass: TCRServerEnumeratorClass; virtual;
    procedure SetServerEnumerator(Value: TCRServerEnumerator); virtual;
    procedure CreateServerEnumerator;
    procedure FreeServerEnumerator;
    procedure CheckServerEnumerator;

  {$IFDEF WIN32_64}
    function GetString(Id: integer): string;
  {$ENDIF}
    procedure SetLabelSet(Value: TLabelSet); virtual;
    
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;

    function DefDialogClass: TClass; virtual;

  {$IFDEF MSWINDOWS}
    function GetKeyPath: string; virtual;
    function GetServerStoreName: string; virtual;
    function GetApplicationKeyPath: string;
    function GetServerListKeyPath: string; virtual;
    procedure SaveServerListToRegistry; virtual;
    procedure LoadServerListFromRegistry(List: _TStrings); virtual;
    procedure SaveInfoToRegistry(Registry: TRegistry); virtual;
    procedure LoadInfoFromRegistry(Registry: TRegistry); virtual;
  {$ENDIF}
    //class function AcceptBlankPassword: boolean; virtual;

  public
    constructor Create(Owner: TComponent); override;
    destructor Destroy; override;

    function Execute: boolean; virtual;

    procedure GetServerList(List: _TStrings); virtual;

    property Connection: TCustomDAConnection read FConnection;

    property Retries: word read FRetries write FRetries default 3;
    property SavePassword: boolean read FSavePassword write FSavePassword default False;
    property StoreLogInfo: boolean read FStoreLogInfo write FStoreLogInfo default True;

    property DialogClass: string read FDialogClass write FDialogClass;

    property Caption: string read FCaption write SetCaption;
    property UsernameLabel: string read FUsernameLabel write SetUsernameLabel;
    property PasswordLabel: string read FPasswordLabel write SetPasswordLabel;
    property ServerLabel: string read FServerLabel write SetServerLabel;
    property ConnectButton: string read FConnectButton write SetConnectButton;
    property CancelButton: string read FCancelButton write SetCancelButton;

    property LabelSet: TLabelSet read FLabelSet write SetLabelSet default lsEnglish;

    // transferred from protected section
    property UseServerHistory: boolean read FUseServerHistory write FUseServerHistory
      default{$IFDEF MSWINDOWS} True {$ELSE} False {$ENDIF};
  end;

  TTableInfo = record
    Name: _string;
    Alias: _string;
  end;
  TTablesInfo = array of TTableInfo;

{$IFDEF MSWINDOWS}

{ TCRNetManager }

  TCRServiceStatus = (ssStopped, ssStartPending, ssStopPending, ssRunning, ssContinuePending, ssPausePending, ssPaused); // equal to TCurrentStatus from SvcMgr

  TCRServiceInfo = record
    ServiceName, DisplayName: string;
    Status: TCRServiceStatus;
  end;

  TCRServicesInfo = array of TCRServiceInfo;
  TCRServiceNamesThread = class;
  TCRServicesThread = class(TThread)
  private
    FList: TStrings;
    FKeywords: string;
  protected
    property Terminated;
    procedure Execute; override;
  public
    constructor Create(List: TStrings; const Keywords: string);
    destructor Destroy; override;
  end;

  TCRServiceNamesThread = class(TThread)
  protected
    FKeywords: string;
    FServices: TCRServicesThread;
    FServer: string;
    FServiceNames: TCRServicesInfo;
    procedure Execute; override;
  public
    constructor Create(const Server: string; Services: TCRServicesThread; const Keywords: string);
  end;

  SC_HANDLE = THandle;

  TCRNetManager = class
  protected
    FServicesCS: TCriticalSection;
    FCachedServerList: TStringList;
    FLastTickCount: LongWord;

    class procedure ServiceManagerOpen(const Server: string; const ReadOnly: boolean; out sch: SC_HANDLE);
    class procedure ServiceManagerClose(const sch: SC_HANDLE);
    class procedure ServiceOpen(const Server: string; const ServiceName: string; const ReadOnly: boolean; out sch: SC_HANDLE; out sh: SC_HANDLE);
    class procedure ServiceClose(const sch: SC_HANDLE; const sh: SC_HANDLE);

    procedure ClearCachedServerList;
    procedure AddToCachedServerList(const Keywords: string; const Server: string);
  public
    constructor Create;
    destructor Destroy; override;

    // Service Control
    class function GetServiceNames(const Server: string): TCRServicesInfo;
    class function GetServiceStatus(const Server: string; const ServiceName: string): TCRServiceStatus;
    class procedure ServiceStart(const Server: string; const ServiceName: string; ParamStr: string = '');
    class procedure ServiceStop(const Server: string; const ServiceName: string);

    // Net control
    class procedure GetServerList(List: TStrings); overload;
    procedure GetServerList(List: TStrings; const Keywords: string; const Timeout: Longword = 1; const CacheTimeout: Longword = 120); overload;
  end;

var
  CRNetManager: TCRNetManager;

type
{$ENDIF}

{ TCRDataSource }

  TCRDataSource = class (TDataSource)
  protected
    FDesignCreate: boolean;

    procedure Loaded; override;
    procedure AssignTo(Dest: TPersistent); override;

  public
    constructor Create(Owner: TComponent); override;
  end;

  TDBAccessUtils = class
  public
    class function IsObjectDataType(Obj: TDAParam; DataType: TFieldType): boolean;
    class function GetNational(Obj: TDAParam): boolean;

    class procedure CheckConnection(Obj: TCustomDADataSet); overload;
    class procedure CheckConnection(Obj: TCustomDASQL); overload;
    class function UsedConnection(Obj: TCustomDADataSet): TCustomDAConnection; overload;
    class function UsedConnection(Obj: TCustomDASQL): TCustomDAConnection; overload;
    class function UsedConnection(Obj: TDAMetaData): TCustomDAConnection; overload;
    class procedure SetAutoCommit(Obj: TComponent; Value: boolean);
    class function GetAutoCommit(Obj: TCustomDAConnection): boolean; overload;
    class function GetAutoCommit(Obj: TCustomDADataSet): boolean; overload;
    class function GetAutoCommit(Obj: TCustomDASQL): boolean; overload;

    class procedure SetDesignCreate(Obj: TDATransaction; Value: boolean); overload;
    class function GetDesignCreate(Obj: TDATransaction): boolean; overload;
    class procedure SetDesignCreate(Obj: TCustomDADataSet; Value: boolean); overload;
    class function GetDesignCreate(Obj: TCustomDADataSet): boolean; overload;
    class procedure SetDesignCreate(Obj: TCustomDASQL; Value: boolean); overload;
    class function GetDesignCreate(Obj: TCustomDASQL): boolean; overload;
    class procedure SetDesignCreate(Obj: TCustomDAUpdateSQL; Value: boolean); overload;
    class function GetDesignCreate(Obj: TCustomDAUpdateSQL): boolean; overload;
    class procedure SetDesignCreate(Obj: TDAMetaData; Value: boolean); overload;
    class function GetDesignCreate(Obj: TDAMetaData): boolean; overload;
    class procedure SetDesignCreate(Obj: TCRDataSource; Value: boolean); overload;
    class function GetDesignCreate(Obj: TCRDataSource): boolean; overload;
    class procedure SetLockLoginPrompt(Obj: TCustomDAConnection; Value: Boolean);

    class function CreateIRecordSet(Obj: TCustomDAConnection): TCRRecordSet;
    class function GetIConnection(Obj: TCustomDAConnection): TCRConnection;
    class procedure CreateIConnection(Obj: TCustomDAConnection);
    class function GetUpdateQuery(Obj: TCustomDADataSet): TComponent;
    class function GetTablesInfo(Obj: TCustomDADataSet): TCRTablesInfo;
    class function GetSQLInfo(Obj: TCustomDADataSet): TSQLInfo;
    class function GetUpdatingTable(Obj: TCustomDADataSet): _string;
    class procedure SetUpdatingTable(Obj: TCustomDADataSet; Value: _string);
    class function GetUpdatingTableIdx(Obj: TCustomDADataSet): integer;
    //class procedure SetUpdatingTableIdx(Obj: TCustomDADataSet; Value: integer);
    class procedure InternalConnect(Obj: TCustomDAConnection);
    class procedure InternalDisconnect(Obj: TCustomDAConnection);
    class procedure DisconnectTransaction(Obj: TCustomDAConnection);
    class procedure SuppressAutoCommit(Obj: TCustomDAConnection);
    class procedure RestoreAutoCommit(Obj: TCustomDAConnection);
    class function IsMultipleTransactionsSupported(Obj: TCustomDAConnection): boolean;
    class function PushOperation(Obj: TCustomDAConnection; Operation: TConnLostCause; AllowFailOver: boolean = true): integer;
    class function PopOperation(Obj: TCustomDAConnection): TConnLostCause;
    class procedure RestoreAfterFailOver(Obj: TCustomDAConnection);
    class function IsFailOverAllowed(Obj: TCustomDAConnection): boolean;

    class function UsedTransaction(Obj: TCustomDAConnection): TDATransaction; overload;
    class function UsedTransaction(Obj: TCustomDADataSet): TDATransaction; overload;
    class function UsedTransaction(Obj: TCustomDASQL): TDATransaction; overload;

    class function GetTransaction(Obj: TCustomDADataSet): TDATransaction; overload;
    class function GetTransaction(Obj: TCustomDASQL): TDATransaction; overload;
    class function GetTransaction(Obj: TDAMetaData): TDATransaction; overload;
    class function GetDefaultTransaction(Obj: TCustomDAConnection): TDATransaction;

    class procedure SetTransaction(Obj: TCustomDADataSet; Value: TDATransaction); overload;
    class procedure SetTransaction(Obj: TCustomDASQL; Value: TDATransaction); overload;
    class procedure SetTransaction(Obj: TDAMetaData; Value: TDATransaction); overload;
    class procedure SetDefaultTransaction(Obj: TCustomDAConnection; Value: TDATransaction);

    class function GetFTransaction(Obj: TCustomDADataSet): TDATransaction; overload;
    class function GetFTransaction(Obj: TCustomDASQL): TDATransaction; overload;
    class function GetFTransaction(Obj: TDAMetaData): TDATransaction; overload;
    class function GetFDefaultTransaction(Obj: TCustomDAConnection): TDATransaction;

    class function GetITransaction(Obj: TDATransaction): TCRTransaction;
    class function GetConnectionCount(Obj: TDATransaction): integer;
    class function GetConnection(Obj: TDATransaction; Index: integer): TCustomDAConnection;
    class procedure Savepoint(Obj: TDATransaction; const Name: _string);
    class procedure RollbackToSavepoint(Obj: TDATransaction; const Name: _string);
    class procedure ReleaseSavepoint(Obj: TDATransaction; const Name: _string);
    class procedure CommitRetaining(Obj: TDATransaction);
    class procedure RollbackRetaining(Obj: TDATransaction);
    class procedure GainTransaction(Obj: TDATransaction);
    class procedure ReleaseTransaction(Obj: TDATransaction);
    class procedure AutoCommitTransaction(Obj: TDATransaction; NeedCommit: boolean);

    class procedure Disconnect(Obj: TCustomDASQL);

    class function SQLGenerator(Obj: TCustomDADataSet): TDASQLGenerator;
    class function GetSQLs(Obj: TCustomDAConnection): TDAList;

    class procedure GetKeyAndDataFields(
      Obj: TCustomDADataSet;
      out KeyAndDataFields: TKeyAndDataFields;
      const ForceUseAllKeyFields: boolean);

    class function GetLockDebug(Obj: TComponent): boolean;
    class procedure SetLockDebug(Obj: TComponent; Value: boolean);
    class function FOwner(Obj: TDAConnectionOptions): TCustomDAConnection; overload;
    class function FOwner(Obj: TDADataSetOptions): TCustomDADataSet; overload;
    class function SQLMonitorClass(Obj: TCustomDAConnection): TClass;
    class function ConnectDialogClass(Obj: TCustomDAConnection): TConnectDialogClass;
    class function QuoteName(Obj: TCustomDADataSet; const AName: _string): _string;

    class procedure RegisterClient(Obj: TCustomDAConnection; Client: TObject; Event: TConnectChangeEvent = nil);
    class procedure UnRegisterClient(Obj: TCustomDAConnection; Client: TObject);

    class function GetIdentityField(Obj: TCustomDADataSet): TCRFieldDesc;

    class function GetSQL(Obj: TComponent): _TStrings;
    class procedure SetSQL(Obj: TComponent; Value: _TStrings);
    class function GetSQLText(Obj: TComponent): _string;
    class procedure SetSQLText(Obj: TComponent; const SQLText: _string; const LockScanParams, LockMacros: boolean);

    class function GetParams(Obj: TComponent): TDAParams;
    class procedure Execute(Obj: TComponent);
    class procedure Open(Obj: TComponent);
    class function GetRowsAffected(Obj: TComponent): integer;
    class function GetUpdateSQLStatementTypes(Obj: TCustomDADataSet): TStatementTypes;
    class function GetUpdateSQLIndex(Obj: TCustomDADataSet; StatementType: TStatementType): _TStrings;
    class function ParseSQL(Obj: TCustomDASQL; const SQL: _string; Params: TDAParams; RenamePrefix: _string = ''): _string;
    class function CreateParamsObject(Obj: TCustomDASQL): TDAParams;

    class procedure SetDesigning(Obj: TComponent; Value: Boolean; SetChildren: Boolean = True);
    class function GetIRecordSet(Obj: TCustomDADataSet): TCRRecordSet;
    class procedure CheckIRecordSet(Obj: TCustomDADataSet);
    class function GetICommand(Obj: TCustomDADataSet): TCRCommand; overload;
    class function GetICommand(Obj: TCustomDASQL): TCRCommand; overload;
    class function GetUpdater(Obj: TCustomDADataSet): TDADataSetUpdater;
    class function GetDataSetService(Obj: TCustomDADataSet): TDADataSetService;

    class function GetDataSetClass(Obj: TCustomDAUpdateSQL): TCustomDADataSetClass;
    class function GetSQLClass(Obj: TCustomDAUpdateSQL): TCustomDASQLClass;

    class function GetParserClass(Obj: TMacros): TSQLParserClass;
  {$IFDEF MSWINDOWS}
    class procedure SaveServerListToRegistry(Obj: TCustomConnectDialog); // used in connection editor
  {$ENDIF}
    class procedure SetConnection(Obj: TCustomConnectDialog; Value: TCustomDAConnection);
    class procedure SetUseServerHistory(Obj: TCustomConnectDialog; Value: boolean);
    class function GetNeedConnect(Obj: TCustomConnectDialog): boolean;
    class procedure SetNeedConnect(Obj: TCustomConnectDialog; Value: boolean);

    class procedure CreateProcCall(Obj: TCustomDASQL; const Name: _string; NeedDescribe: boolean; IsQuery: boolean = False); overload;
    class procedure CreateProcCall(Obj: TCustomDADataSet; const Name: _string; NeedDescribe: boolean; IsQuery: boolean = False); overload;
    class function GetCommand(Obj: TCustomDAConnection): TCustomDASQL;
    class function GetStreamedConnected(Obj: TCustomDAConnection): boolean;
    class procedure Loaded(Obj: TCustomDAConnection);
    class function GetAsCursor(Obj: TDAParam): TCRCursor;
    class function GetCursor(Obj: TCustomDADataSet): TCRCursor;
    class procedure SetCursor(Obj: TCustomDADataSet; Value: TCRCursor);
    class function GetFetchAll(Obj: TCustomDADataSet): boolean;
    class procedure SetFetchAll(Obj: TCustomDADataSet; Value: boolean);
    class function GetKeyFields(Obj: TCustomDADataSet): _string;
    class procedure SetKeyFields(Obj: TCustomDADataSet; const Value: _string);
    class procedure QuickOpen(Obj: TCustomDADataSet; var Info: TQuickOpenInfo);
    class procedure Restore(Obj: TCustomDADataSet; const Info: TQuickOpenInfo);
    class function GetLockMode(Obj: TCustomDADataSet): TLockMode;
    class procedure SetLockMode(Obj: TCustomDADataSet; const Value: TLockMode);
    class function GetLastInsertId(Obj: TCustomDADataSet): int64;
  end;

const
  crSQLArrow            = -30;

  procedure SetCursor(Value: integer);

var
  ChangeCursor: boolean;
  MacroChar: _char;
  SetCursorProc: procedure(Value: integer);
  ShowConnectFormProc: function(ConnectDialog: TCustomConnectDialog): boolean;
  ShowConnectFormProcFmx: function(ConnectDialog: TCustomConnectDialog): boolean;
  BaseSQLOldBehavior: boolean;
  SQLGeneratorCompatibility: boolean;

  function _GetFrom(
    const SQL: _string;
    ParserClass: TSQLParserClass; // TOraParser, TMSparser, TMyParser
    OmitComment: boolean
  ): _string;
  function _AddWhere(
    const SQL: _string; Condition: _string;
    ParserClass: TSQLParserClass; // TOraParser, TMSparser, TMyParser
    OmitComment: boolean
  ): _string;
  function _SetWhere(
    const SQL: _string; Condition: _string;
    ParserClass: TSQLParserClass; // TOraParser, TMSparser, TMyParser
    OmitComment: boolean
  ): _string;
  function _GetWhere(
    const SQL: _string;
    ParserClass: TSQLParserClass; // TOraParser, TMSparser, TMyParser
    OmitComment: boolean
  ): _string;
  function _SetOrderBy(
    const SQL: _string; Fields: _string;
    ParserClass: TSQLParserClass // TOraParser, TMSparser, TMyParser
  ): _string;
  function _GetOrderBy(
    const SQL: _string;
    ParserClass: TSQLParserClass // TOraParser, TMSparser, TMyParser
  ): _string;


  function UpdateKindToStatementType(const UpdateKind: TUpdateKind): TStatementType;
  function StatementTypeToUpdateKind(const StatementType: TStatementType): TUpdateKind;
  procedure RecreateParamsRef(Params: TParams);

var
  ResyncBeforeFetch: boolean = False; // prevents AV on refresh
  BoundParams: boolean = True;
  OldFieldsReadOnly: boolean = False; // remove
  ParamStringAsAnsiString: boolean = False;
  OldCachedUpdateLockMode: boolean = False; // Old behavior LockMode for the CachedUpdate mode

var
  DefaultSQLInfo: TSQLInfo = nil;

implementation

uses
{$IFDEF PERF_COUNTER}
  Debug,
{$ENDIF}
{$IFDEF MSWINDOWS}
  Messages,
{$ENDIF}
{$IFDEF CLR}
  System.Security,
{$ENDIF}
{$IFNDEF FPC}
  DBCommon, DBConsts,
{$ELSE}
  DBConst,
{$ENDIF}
  CRFunctions, DAConsts, DASQLMonitor, CRConnectionPool;

{$IFDEF WIN32_64}
{$R *.res}
{$ENDIF}

procedure RecreateParamsRef(Params: TParams);
var
  i: integer;
  s: _string;
  Param: TParam;
begin
  for i := 0 to Params.Count - 1 do begin
    Param := TParam(TCollection(Params).Items[i]);
    s := Param.Name;
    Param.Name := '';
    Params[i].Name := s; // Don't optimize this line!  This is necessary to force ParamRef initialization
  end;
end;

procedure SetCursor(Value: integer);
begin
  if Assigned(SetCursorProc) then
    SetCursorProc(Value);
end;

function _AddWhere(
  const SQL: _string; Condition: _string;
  ParserClass: TSQLParserClass; // TOraParser, TMSparser, TMyParser
  OmitComment: boolean
): _string;
var
  Parser: TSQLParser;
  IsWhere: boolean;
  Code: integer;
  PrevCode: integer;
  St, StLex: _string;
  BracketCount: integer;
  WherePos: integer;
  NeedBracket: boolean;
  Bracket: _string;
  IsMacro: boolean;
  MacroSt: _string;

begin
  Result := SQL;

  if Trim(Condition) = '' then
    Exit;

  Parser := ParserClass.Create(SQL);
  Parser.OmitComment := OmitComment;
  WherePos := 0;
  IsMacro := False;
  MacroSt := MacroChar;

  try
    Code := Parser.ToLexem([lxWITH, lxSELECT]);
    if Code = lxWITH then
      Code := Parser.ToLexem(lxSELECT, True);

    if Code <> lcEnd then begin
      IsWhere := False;
      NeedBracket := False;
      Code := 0;
      BracketCount := 0;
      repeat
        PrevCode := Code;
        Code := Parser.GetNext(StLex); //+++ char instead of string
        if (Code = lxWHERE) and (not IsMacro) and (BracketCount = 0) then begin
          IsWhere := True;
          WherePos := Parser.CurrPos + 2;
          NeedBracket := True;
        end
        else
          if Code = lcSymbol then
            if StLex = '(' then
              Inc(BracketCount)
            else
              if StLex = ')' then
                Dec(BracketCount);

        IsMacro := (Code <> lcString) and (StLex = MacroSt);
        if (BracketCount = 0) and Parser.IsClauseLexem(Code) and (Code <> lxWHERE) then
          Break;
      until Code = lcEnd;

      if NeedBracket then begin
        if PrevCode = lcComment then
          Bracket := LineSeparator + ')'
        else
          Bracket := ')';
      end;

      if not IsWhere then
        St := LineSeparator + 'WHERE ' + Condition + ' '
      else
        if (PrevCode = lcComment) and not NeedBracket then
          St := LineSeparator +'AND ' + Condition + ' '
        else
          St := ' AND ' + Condition + ' ';

      if Code = lcEnd then begin
        if NeedBracket then begin
          Insert('(', Result, WherePos);
          Result := Trim(Result) + Bracket + St;
        end
        else
          if PrevCode <> 7 {';'} then
            Result := Trim(Result) + St
          else
            Insert(St, Result, Parser.PrevPos);
      end
      else
        if NeedBracket then begin
          Insert(Bracket + St, Result, Parser.PrevPos + 1);
          Insert('(', Result, WherePos);
        end
        else
          Insert(St, Result, Parser.PrevPos + 1);
    end;
  finally
    Parser.Free;
  end;
end;

//++ Must be merged with _AddWhere 
function _SetWhere(
  const SQL: _string; Condition: _string;
  ParserClass: TSQLParserClass; // TOraParser, TMSparser, TMyParser
  OmitComment: boolean
): _string;
var
  Parser: TSQLParser;
  FirstPos: integer;
  LastPos: integer;
  Code, PrevCode: integer;
  StLex: _string;
  BracketCount: integer;
begin
  Result := SQL;
  Parser := ParserClass.Create(SQL);
  Parser.OmitBlank := False;
  Parser.OmitComment := True;
  try
    Code := Parser.ToLexem([lxWITH, lxSELECT]);
    if Code = lxWITH then
      Code := Parser.ToLexem(lxSELECT, True);

    if Code <> lcEnd then begin
      FirstPos := 0;
      LastPos := 0;
      BracketCount := 0;
      PrevCode := lcEnd;
      Code := Parser.GetNext(StLex); //+++ char instead of string
      repeat
        if Code = lcBlank then begin
          if LastPos = 0 then
            LastPos := Parser.PrevPos;
        end
        else begin
          LastPos := 0;

          if Code = lxWHERE then begin
            if BracketCount = 0 then begin
              if Condition = '' then begin
                if PrevCode = lcBlank then
                  FirstPos := Parser.PrevPrevPos + 1
                else
                  FirstPos := Parser.PrevPos + 1;
              end
              else begin
                Parser.GetNext(StLex); // blank
                FirstPos := Parser.CurrPos + 1;
              end;
            end;
          end
          else
          if Code = lcSymbol then begin
            if StLex = '(' then
              Inc(BracketCount)
            else
              if StLex = ')' then
                Dec(BracketCount);
          end;
        end;

        PrevCode := Code;
        Code := Parser.GetNext(StLex);
      until (Code = lcEnd) or (Parser.IsClauseLexem(Code) and (Code <> lxWHERE)) and (BracketCount = 0);

      if LastPos = 0 then
        LastPos := Length(Result);

      if FirstPos > 0 then
        Delete(Result, FirstPos, LastPos - FirstPos + 1);

      if Condition <> '' then begin
        if FirstPos = 0 then begin
          FirstPos := LastPos + 1;
          Condition := ' WHERE ' + Condition;
          if Pos(#13, Copy(Result, 1, FirstPos)) > 0 then
            Condition := LineSeparator + ' ' + Condition;
        end;
        Insert(Condition, Result, FirstPos);
      end;
    end;
  finally
    Parser.Free;
  end;
end;

function _GetFrom_or_Where(
    const SQL: _string;
    Parser: TSQLParser;
    NeedCode: integer // FromCode or WhereCode
  ): _string;
var
  IsFound: boolean;
  Code: integer;
  StLex, MacroSt: _string;
  BracketCount: integer;
  BeginPos: integer;
  IsMacro: boolean;
begin
  Result := '';
  BeginPos := 0;
  IsMacro := False;
  MacroSt := MacroChar;
  if Parser.ToLexem(lxSELECT) <> lcEnd then begin
    IsFound := False;
    BracketCount := 0;
    repeat
      Code := Parser.GetNext(StLex); //+++ char instead of string
      if (Code = NeedCode) and (not IsMacro) and (BracketCount = 0) then begin
        IsFound := True;
        BeginPos := Parser.CurrPos + 2;
      end
      else
        if Code = lcSymbol then
          if StLex = '(' then
            Inc(BracketCount)
          else
            if StLex = ')' then
              Dec(BracketCount);

      IsMacro := (Code <> lcString) and (StLex = MacroSt);
      if (BracketCount = 0) and Parser.IsClauseLexem(Code) and (Code <> NeedCode) then begin
        Parser.Back;
        Break;
      end;
    until Code = lcEnd;

    if IsFound then
      Result := Copy(SQL, BeginPos, Parser.CurrPos - BeginPos + 1);
  end;
end;

function _GetFrom(
    const SQL: _string;
    ParserClass: TSQLParserClass; // TOraParser, TMSparser, TMyParser
    OmitComment: boolean
  ): _string;
var
  Parser: TSQLParser;
begin
  Parser := ParserClass.Create(SQL);
  Parser.OmitComment := OmitComment;
  try
    Result := _GetFrom_or_Where(SQL, Parser, lxFROM);
  finally
    Parser.Free;
  end;
end;

function _GetWhere(
    const SQL: _string;
    ParserClass: TSQLParserClass; // TOraParser, TMSparser, TMyParser
    OmitComment: boolean
  ): _string;
var
  Parser: TSQLParser;
begin
  Parser := ParserClass.Create(SQL);
  Parser.OmitComment := OmitComment;
  try
    Result := _GetFrom_or_Where(SQL, Parser, lxWHERE);
  finally
    Parser.Free;
  end;
end;

function _SetOrderBy(
  const SQL: _string; Fields: _string;
  ParserClass: TSQLParserClass
  ): _string;
var
  Parser: TSQLParser;
  FirstPos: integer;
  LastPos: integer;
  Code, PrevCode: integer;
  StLex: _string;
  i, BracketCount: integer;
begin
  for i := 1 to Length(Fields) do
    if Fields[i] = ';' then
      Fields[i] := ',';

  Result := SQL;
  Parser := ParserClass.Create(Result);
  Parser.OmitBlank := False;
  Parser.OmitComment := True;
  try
    if Parser.ToLexem(lxSELECT) <> lcEnd then begin
      FirstPos := 0;
      LastPos := 0;
      BracketCount := 0;
      PrevCode := lcEnd;
      Code := Parser.GetNext(StLex);
      repeat
        if Code = lcBlank then begin
          if LastPos = 0 then
            LastPos := Parser.PrevPos;
        end
        else begin
          LastPos := 0;

          if Code = lxORDER then begin
            if BracketCount = 0 then begin
              if Fields = '' then
                if PrevCode = lcBlank then
                  FirstPos := Parser.PrevPrevPos + 1
                else
                  FirstPos := Parser.PrevPos + 1;
              Parser.GetNext(StLex);  // blank
              if Parser.GetNext(StLex) = lxBY then begin
                if Fields <> '' then begin
                  Parser.GetNext(StLex); // blank
                  FirstPos := Parser.CurrPos + 1;
                end;
              end
              else
                FirstPos := 0;
            end;
          end
          else
          if Code = lcSymbol then begin
            if StLex = '(' then
              Inc(BracketCount)
            else
              if (StLex = ')') and (BracketCount > 0) then
                Dec(BracketCount);
          end;
        end;

        PrevCode := Code;
        Code := Parser.GetNext(StLex);
      until (Code = lcEnd) or ((Parser.CompareClauseLexems(Code, lxORDER) > 0) and (BracketCount = 0));

      if LastPos = 0 then
        LastPos := Length(Result);

      if FirstPos > 0 then
        Delete(Result, FirstPos, LastPos - FirstPos + 1);

      if Fields <> '' then begin
        if FirstPos = 0 then begin
          FirstPos := LastPos + 1;
          Fields := ' ORDER BY ' + Fields;
          if Pos(#13, Copy(Result, 1, FirstPos)) > 0 then
            Fields := LineSeparator + ' ' + Fields;
        end;
        Insert(Fields, Result, FirstPos);
      end;
    end;
  finally
    Parser.Free;
  end;
end;

function _GetOrderBy(
  const SQL: _string;
  ParserClass: TSQLParserClass
): _string;
var
  Parser: TSQLParser;
  FirstPos: integer;
  LastPos: integer;
  StLex: _string;
  Code, BracketCount: integer;
begin
  Result := '';
  Parser := ParserClass.Create(SQL);
  Parser.OmitBlank := False;
  Parser.OmitComment := True;
  try
    FirstPos := 0;
    LastPos := Length(SQL);
    if Parser.ToLexem(lxSELECT) <> lcEnd then begin
      Code := Parser.GetNext(StLex);
      BracketCount := 0;
      repeat
        if (Code = lxORDER) and (BracketCount = 0) then begin
          Parser.GetNext(StLex);  //blank
          if Parser.GetNextCode = lxBY then begin
            Parser.GetNext(StLex);  //blank
            FirstPos := Parser.CurrPos + 1;
          end
          else
            FirstPos := 0;
        end
        else if Code = lcSymbol then
          if StLex = '(' then
            Inc(BracketCount)
          else
            if (StLex = ')') and (BracketCount > 0) then
              Dec(BracketCount);
        Code := Parser.GetNext(StLex);
      until (FirstPos <> 0) or (Code = lcEnd);

      if FirstPos <> 0 then begin
        Result := Copy(SQL, FirstPos, LastPos - FirstPos + 1);

        Parser.SetText(Result);
        repeat
          Code := Parser.GetNextCode;
          if Parser.IsClauseLexem(Code) then
            Result := Copy(Result, 0, Parser.PrevPos);
        until Code = lcEnd;

        Result := Trim(Result);
      end;
    end;
  finally
    Parser.Free;
  end;
end;

function UpdateKindToStatementType(const UpdateKind: TUpdateKind): TStatementType;
begin
  case UpdateKind of
    DB.ukModify:
      Result := stUpdate;
    DB.ukInsert:
      Result := stInsert;
    DB.ukDelete:
      Result := stDelete;
    else
    begin
      Result := stCustom; // To prevent compiler warning
      Assert(False);
    end;
  end;
end;

function StatementTypeToUpdateKind(const StatementType: TStatementType): TUpdateKind;
begin
  case StatementType of
    stUpdate:
      Result := DB.ukModify;
    stInsert:
      Result := DB.ukInsert;
    stDelete:
      Result := DB.ukDelete;
    else
    begin
      Result := DB.ukInsert; // To prevent compiler warning
      Assert(False);
    end;
  end;
end;

{ EDAError }

constructor EDAError.Create(ErrorCode: integer; Msg: _string);
begin
  inherited Create(Msg);

{$IFDEF CRUNICODE}
  FWideMessage := Msg;
{$ENDIF}
  FErrorCode := ErrorCode;
end;

function EDAError.IsFatalError: boolean;
begin
  Result := False;
end;

function EDAError.IsKeyViolation: boolean;
begin
  Result := False;
end;

{$IFDEF CRUNICODE}
procedure EDAError.SetWideMessage(const Value: WideString);
begin
  FWideMessage := Value;
  inherited Message := Value;
end;
{$ENDIF}

{ TDAConnectionOptions }

constructor TDAConnectionOptions.Create(Owner: TCustomDAConnection);
begin
  inherited Create;

  FOwner := Owner;
  KeepDesignConnected := True;
end;

procedure TDAConnectionOptions.AssignTo(Dest: TPersistent);
begin
  if Dest is TDAConnectionOptions then begin
    TDAConnectionOptions(Dest).KeepDesignConnected := KeepDesignConnected;
    TDAConnectionOptions(Dest).DisconnectedMode := DisconnectedMode;
    TDAConnectionOptions(Dest).LocalFailover := LocalFailover;
    TDAConnectionOptions(Dest).EnableBCD := EnableBCD;
  {$IFDEF VER6P}
  {$IFNDEF FPC}
    TDAConnectionOptions(Dest).EnableFMTBCD := EnableFMTBCD;
  {$ENDIF}
  {$ENDIF}
  end
  else
    inherited;
end;

procedure TDAConnectionOptions.SetDisconnectedMode(Value: boolean);
begin
  if Value <> DisconnectedMode then begin
    FOwner.Disconnect;
    FDisconnectedMode := Value;
    if FOwner.FIConnection <> nil then
      FOwner.FIConnection.SetProp(prDisconnectedMode, Value);
  end;
end;

procedure TDAConnectionOptions.SetDefaultSortType(Value: TSortType);
begin
  if Value <> FDefaultSortType then begin
    FDefaultSortType := Value;
    if FOwner.FIConnection <> nil then
      FOwner.FIConnection.SetProp(prDefaultSortType, Variant(Value));
  end;
end;

procedure TDAConnectionOptions.SetEnableBCD(Value: boolean);
begin
  FEnableBCD := Value;
  if FOwner.FIConnection <> nil then
    FOwner.FIConnection.SetProp(prEnableBCD, Value);
end;

{$IFDEF VER6P}
{$IFNDEF FPC}
procedure TDAConnectionOptions.SetEnableFMTBCD(Value: boolean);
begin
  FEnableFMTBCD := Value;
  if FOwner.FIConnection <> nil then
    FOwner.FIConnection.SetProp(prEnableFmtBCD, Value);
end;
{$ENDIF}
{$ENDIF}

{ TPoolingOptions }

constructor TPoolingOptions.Create(Owner: TCustomDAConnection);
begin
  inherited Create;

  FOwner := Owner;
  FMaxPoolSize := 100;
end;

procedure TPoolingOptions.AssignTo(Dest: TPersistent);
begin
  if Dest is TPoolingOptions then begin
    TPoolingOptions(Dest).FMaxPoolSize := FMaxPoolSize;
    TPoolingOptions(Dest).FMinPoolSize := FMinPoolSize;
    TPoolingOptions(Dest).FConnectionLifetime := FConnectionLifetime;
    TPoolingOptions(Dest).FValidate := FValidate;
  end
  else
    inherited;
end;

{ TCustomDAConnection }

constructor TCustomDAConnection.Create(Owner: TComponent);
begin
  inherited Create(Owner);

{$IFDEF FPC}
  FDataSets := TList.Create;
  FClients := TList.Create;
  FConnectEvents := TList.Create;
{$ENDIF}

  FTransactions := TDATransactions.Create;
  FInternalDefTransaction := CreateTransaction;
{$IFDEF VER6P}
  InsertComponent(FInternalDefTransaction);
  FInternalDefTransaction.SetSubComponent(True);
{$ENDIF}
  if Name <> '' then
    FInternalDefTransaction.Name := 'tr' + Name;
  FInternalDefTransaction.DefaultConnection := Self;
  FInternalDefTransaction.FShareTransaction := True;
  FTransactions.Add(FInternalDefTransaction);

  FSQLs := TDAList.Create;
  FAutoCommit := True;
  LoginPrompt := True;

  FOptions := CreateOptions;
  FPoolingOptions := CreatePoolingOptions;
  FShouldShowPrompt := True;

  hRegisterClient := TCriticalSection.Create;
end;

destructor TCustomDAConnection.Destroy;
var
  i: integer;
begin
  try
    Disconnect;
  finally
    if Assigned(FIOHandler) then
      TCRIOHandlerUtils.UnregisterClient(FIOHandler, Self);

    ClearRefs;
    FCommand.Free;

    FTransactions.Remove(FInternalDefTransaction);
    FInternalDefTransaction.Free;
    FInternalDefTransaction := nil;

    for i := FTransactions.Count - 1 downto 0 do
      DoRemoveTransaction(FTransactions[i]);

    TDASQLMonitorClass(SQLMonitorClass).ObjectDestroyed(Self);

    inherited;

    FSQLs.Free; // placed after inherited for successful UnregisterClient on destroy
    FreeIConnection;
    FTransactions.Free;
    FPoolingOptions.Free;
    FOptions.Free;
    hRegisterClient.Free;

  {$IFDEF FPC}
    FreeAndNil(FConnectEvents);
    FreeAndNil(FClients);
    FreeAndNil(FDataSets);
  {$ENDIF}
  end;
end;

procedure TCustomDAConnection.Notification(AComponent: TComponent;
  Operation: TOperation);
begin
  inherited;

  if Operation = opRemove then begin
    if AComponent = FConnectDialog then
      FConnectDialog := nil;
    if AComponent = FDefaultTransaction then
      DefaultTransaction := nil;
  {$IFNDEF LITE}
    if AComponent = FIOHandler then
      IOHandler := nil;
  {$ENDIF}
  end;
end;

{$IFNDEF LITE}
procedure TCustomDAConnection.SetIOHandler(Value: TCRIOHandler);
begin
  if FIOHandler <> Value then begin
    if FIOHandler <> nil then begin
      TCRIOHandlerUtils.UnregisterClient(FIOHandler, Self);
      FIOHandler.RemoveFreeNotification(Self);
    end;

    Disconnect;
    FIOHandler := Value;

    if Value <> nil then begin
      TCRIOHandlerUtils.RegisterClient(Value, Self);
      FIOHandler.FreeNotification(Self);
    end;

    if FIConnection <> nil then
      FIConnection.IOHandler := FIOHandler;
  end;
end;
{$ENDIF}

function TCustomDAConnection.GetIConnectionClass: TCRConnectionClass;
begin
  Assert(False, 'Must be overrided');
  Result := TCRConnection;
end;

function TCustomDAConnection.GetICommandClass: TCRCommandClass;
begin
  Assert(False, 'Must be overrided');
  Result := TCRCommand;
end;

function TCustomDAConnection.GetIRecordSetClass: TCRRecordSetClass;
begin
  Assert(False, 'Must be overrided');
  Result := TCRRecordSet;
end;

function TCustomDAConnection.GetIMetaDataClass: TCRMetaDataClass;
begin
  Assert(False, 'Must be overrided');
  Result := TCRMetaData;
end;

function TCustomDAConnection.IsMultipleTransactionsSupported: boolean;
begin
  Result := False;
end;

procedure TCustomDAConnection.CreateIConnection;
begin
  SetIConnection(nil);
end;

function TCustomDAConnection.CreateICommand: TCRCommand;
begin
  Result := GetICommandClass.Create;
  Result.SetConnection(nil);
end;

function TCustomDAConnection.CreateIRecordSet: TCRRecordSet;
begin
  Result := GetIRecordSetClass.Create;
  Result.SetConnection(FIConnection);
end;

procedure TCustomDAConnection.FreeIConnection;
begin
  if FIConnection = nil then
    exit;

  ClearTransactionRefs;
  {$IFNDEF LITE}
  if FIConnection.Pool <> nil then
    FIConnection.ReturnToPool
  else
  {$ENDIF}
    FIConnection.Free;
  SetIConnection(nil);
end;

procedure TCustomDAConnection.SetIConnection(Value: TCRConnection);
var
  i: integer;
begin
  if (Value <> nil) or
    ((FIConnection <> nil) and not (csDestroying in ComponentState)) then begin
    for i := 0 to FSQLs.Count - 1 do
      if TCustomDASQL(FSQLs[i]).FICommand <> nil then
        TCustomDASQL(FSQLs[i]).FICommand.SetConnection(Value);

    for i := 0 to DataSetCount - 1 do
      if (DataSets[i] is TCustomDADataSet) and (TCustomDADataSet(DataSets[i]).FIRecordSet <> nil) then
        TCustomDADataSet(DataSets[i]).FIRecordSet.SetConnection(Value);
  end;

  FIConnection := Value;

  if FIConnection <> nil then begin
  {$IFNDEF LITE}
    FIConnection.OnError := DoError;
  {$ENDIF}
    FIConnection.OnReconnectError := Disconnect;
    FIConnection.OnReconnectSuccess := DoAfterConnect;
    FIConnection.Component := Self;
  end;
end;

procedure TCustomDAConnection.ClearTransactionRefs;
var
  i: integer;
begin
  for i := 0 to FTransactions.Count - 1 do begin
    if FTransactions[i].FITransaction <> nil then
      if FTransactions[i].FShareTransaction then
        FTransactions[i].FreeITransaction
      else
        FTransactions[i].FITransaction.RemoveConnection(FIConnection);
  end;
end;

function TCustomDAConnection.CreateDataSet: TCustomDADataSet;
begin
  Result := TCustomDADataSet.Create(nil);  // Self ???
  Result.Connection := Self;
end;

function TCustomDAConnection.CreateSQL: TCustomDASQL;
begin
  Result := TCustomDASQL.Create(nil);
  Result.Connection := Self;
end;

function TCustomDAConnection.CreateTransaction: TDATransaction;
begin
  Result := TDATransaction.Create(nil);
  Result.DefaultConnection := Self;
end;

function TCustomDAConnection.CreateMetaData: TDAMetaData;
begin
  Result := TDAMetaData.Create(nil);
  Result.Connection := Self;
end;

procedure TCustomDAConnection.RemoveFromPool;
begin
  if FIConnection <> nil then
    FIConnection.IsValid := False;
end;

procedure TCustomDAConnection.MonitorMessage(const Msg: string);
begin
  TDASQLMonitorClass(SQLMonitorClass).CustomMessage(Self, Msg);
end;

procedure TCustomDAConnection.Loaded;
begin
  inherited;

  try
    try
      if FStreamedConnected then
        SetConnected(True);
    except
      on E: Exception do
        if csDesigning in ComponentState then
          ShowException(E, ExceptAddr)
        else
          raise;
    end;
  finally
    FStreamedConnected := False;
  end;

{$IFNDEF VER7P}
  FInternalDefTransaction.Loaded;
{$ENDIF}
end;

procedure TCustomDAConnection.ClearRefs;
var
  i: integer;
begin
  while FSQLs.Count > 0 do
    TCustomDASQL(FSQLs[0]).Connection := nil;

  i := 0;
  while i < DataSetCount do
    if DataSets[i] is TCustomDADataSet then
      TCustomDADataSet(DataSets[i]).Connection := nil
    else
    if DataSets[i] is TDAMetaData then
      TDAMetaData(DataSets[i]).Connection := nil
    else
      Inc(i);
end;

procedure TCustomDAConnection.RegisterClient(Client: TObject; Event: TConnectChangeEvent = nil);
begin
  hRegisterClient.Enter;
  try
  {$IFDEF FPC}
    FClients.Add(Client);
    FConnectEvents.Add(TMethod(Event).Code);
    if Client is TDataSet then
      FDataSets.Add(Client);
  {$ELSE}
    inherited;
  {$ENDIF}

    if Client is TCustomDASQL then
      FSQLs.Add(Client);
  finally
    hRegisterClient.Leave;
  end;
end;

procedure TCustomDAConnection.UnRegisterClient(Client: TObject);
{$IFDEF FPC}
var
  Index: Integer;
{$ENDIF}
begin
  hRegisterClient.Enter;
  try
  {$IFDEF FPC}
    if Client is TDataSet then
      FDataSets.Remove(Client);
    Index := FClients.IndexOf(Client);
    if Index <> -1 then begin
      FClients.Delete(Index);
      FConnectEvents.Delete(Index);
    end;
  {$ELSE}
    inherited;
  {$ENDIF}

    if Client is TCustomDASQL then
      FSQLs.Remove(Client);
  finally
    hRegisterClient.Leave;
  end;
end;

{$IFDEF FPC}
procedure TCustomDAConnection.SendConnectEvent(Connecting: Boolean);
var
  I: Integer;
  ConnectEvent: TConnectChangeEvent;
begin
  for I := 0 to FClients.Count - 1 do
  begin
    if FConnectEvents[I] <> nil then
    begin
      TMethod(ConnectEvent).Code := FConnectEvents[I];
      TMethod(ConnectEvent).Data := FClients[I];
      ConnectEvent(Self, Connecting);
    end;
//    if TObject(FClients[I]) is TDataset then
//      TDataSet(FClients[I]).DataEvent(deConnectChange, Integer(Connecting));
  end;
end;

function TCustomDAConnection.GetDataSet(Index: Integer): TDataSet;
begin
  Result := FDataSets[Index];
end;

function TCustomDAConnection.GetDataSetCount: Integer;
begin
  Result := FDataSets.Count;
end;
{$ENDIF}

function TCustomDAConnection.SQLMonitorClass: TClass;
begin
  Result := TCustomDASQLMonitor;
end;

function TCustomDAConnection.ConnectDialogClass: TConnectDialogClass;
begin
  Result := TCustomConnectDialog;
end;

function TCustomDAConnection.NeedPrompt: boolean;
begin
  Result := not FLockLoginPrompt and
    (LoginPrompt or (csDesigning in ComponentState) and
    ((Username = '') and (Password = ''))) and
    not ((csDesigning in ComponentState) and ((csReading in ComponentState) or FStreamedConnected))
end;

function TCustomDAConnection.IsConnectedStored: boolean;
begin
  Result := Connected and not Options.DisconnectedMode; //In disconnect mode Design-time connection disabled
end;

function TCustomDAConnection.InternalGetServer: _string;
begin
  Result := FServer;
end;

procedure TCustomDAConnection.DoConnect;
var
  MessageID: cardinal;
begin
  TDASQLMonitorClass(SQLMonitorClass).DBConnect(Self, MessageID, True);

  CreateIConnection;
  PushOperation(clConnect);
  StartWait;
  try
    // FIConnection.InternalTransaction -> FInternalDefTransaction.FITransaction
    FInternalDefTransaction.CheckITransaction;
    
    FIConnection.SetUsername(FUsername);
    FIConnection.SetPassword(FPassword);
    FIConnection.SetServer(InternalGetServer);

    FIConnection.Connect('');
  finally
    StopWait;
    PopOperation;
  end;

  TDASQLMonitorClass(SQLMonitorClass).DBConnect(Self, MessageID, False);
end;

procedure TCustomDAConnection.DoDisconnect;
var
  MessageID: cardinal;
begin
  try
    try
      DisconnectTransaction;
    finally
      if Connected then begin  //Disconnect can be done in Commit or Rollback
        TDASQLMonitorClass(SQLMonitorClass).DBDisconnect(Self, MessageID, True);

        {$IFNDEF LITE}
        if (FIConnection.Pool <> nil) and FIConnection.IsValid then begin
          FIConnection.ReturnToPool;
          SetIConnection(nil);
        end
        else
        {$ENDIF}
          FIConnection.Disconnect;

        TDASQLMonitorClass(SQLMonitorClass).DBDisconnect(Self, MessageID, False);
      end;
    end;
  except
    on E: EDAError do begin
      if not((csDestroying in ComponentState) and E.IsFatalError) then
        raise;
    end
    else
      raise;
  end;
end;

procedure TCustomDAConnection.DisconnectTransaction;
var
  i: integer;
begin
  for i := 0 to FTransactions.Count - 1 do
    FTransactions[i].CloseTransaction(True);

  ClearTransactionRefs;
end;

procedure TCustomDAConnection.InternalConnect;
var
  StoredConnectCount: integer;
  StoredLoginPrompt: boolean;
begin
  Inc(FConnectCount);
  StoredConnectCount := FConnectCount;
  StoredLoginPrompt := LoginPrompt;
  LoginPrompt := LoginPrompt and FShouldShowPrompt;
  try
    try
      Connect;
    except
      on EFailOver do;
      else begin
        if not Connected then
          Dec(StoredConnectCount);//Restore ConnectCount in case of Connection Failure
        raise;
      end;
    end;
  finally
    FConnectCount := StoredConnectCount;
    LoginPrompt := StoredLoginPrompt;
    FShouldShowPrompt := not Connected; //in case of Connect Exception LogPrompt appears again
  end;
end;

procedure TCustomDAConnection.InternalDisconnect;
begin
  Dec(FConnectCount);
  if FConnectCount < 0 then //This could happen in case of Commit/RollBack after Execute with AutoCommit = False
    FConnectCount := 0;
  if (FConnectCount = 0) and Options.DisconnectedMode then
    if not InTransaction then //Execute with AutoCommit = False, after execute InTransaction = True, so wait for Commit/RollBack or for
                              //execute with AutoCommit = True
      Disconnect;
  FShouldShowPrompt := False;
end;

procedure TCustomDAConnection.Connect;
begin
  SetConnected(True);
end;

procedure TCustomDAConnection.Disconnect;
begin
  SetConnected(False);
end;

procedure TCustomDAConnection.PerformConnect(Retry: boolean);
begin
  if csReading in ComponentState then
    FStreamedConnected := True
  else begin
    if GetConnected then
      Exit;
    if not Retry and Assigned(BeforeConnect) then
      BeforeConnect(Self);
    DoConnect;
    Inc(FConnectCount);
    SendConnectEvent(True);
    if Assigned(AfterConnect) then
      AfterConnect(Self);
  end;
end;

procedure TCustomDAConnection.AssignConnect(Source: TCustomDAConnection);
begin
  Disconnect;
  if Pooling or (Assigned(Source) and Source.Pooling) then
    raise Exception.Create(SCannotPerformIfPooling);
  if Source <> nil then begin
    AssignConnectOptions(Source);

    if (FIConnection = nil) and (Source.FIConnection <> nil) then
      CreateIConnection;
    if (FIConnection <> nil) and (Source.FIConnection = nil) then
      FreeIConnection;

    if (FIConnection <> nil) and (Source.FIConnection <> nil) then begin
      FIConnection.AssignConnect(Source.FIConnection);
      if FIConnection.GetInternalTransaction.GetInTransaction then
        DefaultTransaction.PrepareTransaction;
    end;
  end
  else
    if FIConnection <> nil then
      FIConnection.AssignConnect(nil);
end;

function TCustomDAConnection.ParamByName(Name: _string): TDAParam;
begin
  Result := FCommand.ParamByName(Name);
end;

procedure ParamsError;
begin
  raise Exception.Create(SInvalidParamsArray);
end;

function TCustomDAConnection.ExecSQL(Text: _string; const Params: array of variant): variant;
var
  i: integer;
  Param: TParam;
begin
  CheckCommand;
  FCommand.SQL.Text := ''; // drop params from previous sql
  FCommand.SQL.Text := Text;

  for i := 0 to FCommand.ParamCount - 1 do
    if i <= High(Params) then
      FCommand.Params[i].Value := Params[i];

  Param := FCommand.FindResultParam;
  if Param <> nil then
    if Param.DataType = ftUnknown then // from ODAC
      Param.DataType := ftString;

  FCommand.Execute;

  if Param <> nil then
    Result := Param.Value
  else
    Result := Null;
end;

function TCustomDAConnection.ExecSQLEx(Text: _string; const Params: array of variant): variant;
var
  i: integer;
  PName: _string;
  Param: TParam;
begin
  CheckCommand;
  FCommand.SQL.Text := ''; // drop params from previous sql
  FCommand.SQL.Text := Text;

  if High(Params) mod 2 <> 1 then
    if (High(Params) >= 0) and not((High(Params) = 0) and VarIsNull(Params[0])) then
      ParamsError;

  for i := 0 to (High(Params) + 1) div 2 - 1  do begin
    if VarIsStr(Params[i*2]) then
      PName := Params[i*2]
    else
      ParamsError;

    FCommand.ParamByName(PName).Value := Params[i*2+1];
  end;

  Param := FCommand.FindResultParam;
  if Param <> nil then
    if Param.DataType = ftUnknown then // from ODAC
      Param.DataType := ftString;

  FCommand.Execute;

  if Param <> nil then
    Result := Param.Value
  else
    Result := Unassigned;
end;

function TCustomDAConnection.ExecProc(Name: _string; const Params: array of variant): variant;
var
  i, j, n: integer;
  Param: TParam;
begin
  CheckCommand;
  FCommand.SQL.Text := ''; // drop params from previous sql
  FCommand.InternalCreateProcCall(Name, True);

  Param := FCommand.FindResultParam;
  j := 0;
  n := FCommand.ParamCount;
  for i := 0 to FCommand.ParamCount - 1 do begin
    if j > High(Params) then begin
      n := i;
      break;
    end;
    if (Param = nil) or (i <> Param.Index) then begin
      FCommand.Params[i].Value := Params[j];
      j := j + 1;
    end;
  end;

  for i := n to FCommand.ParamCount - 1 do
    if FCommand.Params[i].ParamType = ptInput then
      FCommand.Params[i].Bound := False;

  FCommand.Execute;

  if Param <> nil then
    Result := Param.Value
  else
    Result := Unassigned;
end;

function TCustomDAConnection.ExecProcEx(Name: _string; const Params: array of variant): variant;
var
  i: integer;
  PName: _string;
  Param: TParam;
begin
  CheckCommand;
  FCommand.SQL.Text := ''; // drop params from previous sql
  FCommand.InternalCreateProcCall(Name, True);

  if (High(Params) + 1) mod 2 <> 0 then
    if not((High(Params) = 0) and VarIsNull(Params[0])) then
      ParamsError;

  for i := 0 to FCommand.ParamCount - 1 do
    if FCommand.Params[i].ParamType = ptInput then
      FCommand.Params[i].Bound := False;

  for i := 0 to (High(Params) + 1) div 2 - 1 do begin
    if VarIsStr(Params[i*2]) then
      PName := Params[i*2]
    else
      ParamsError;

    FCommand.ParamByName(PName).Value := Params[i*2+1];
  end;

  FCommand.Execute;

  Param := FCommand.FindResultParam;
  if Param <> nil then
    Result := Param.Value
  else
    Result := Unassigned;
end;

function TCustomDAConnection.DefaultTableSchema: _string;
begin
  Result := '';
end;

procedure TCustomDAConnection.GetTableNames(List: _TStrings; AllTables: boolean = False);
var
  MetaData: TDAMetaData;
  Name, s: _string;
  NameField: TField;
  SchemaField: TField;
  CatalogField: TField;
begin
  MetaData := CreateMetaData;
  try
    InternalConnect;
    try
      MetaData.MetaDataKind := 'tables';
      if not AllTables then
        MetaData.Restrictions.Add('SCOPE=LOCAL');
      MetaData.Open;
      NameField := MetaData.FindField('TABLE_NAME');
      SchemaField := MetaData.FindField('TABLE_SCHEMA');
      CatalogField := MetaData.FieldByName('TABLE_CATALOG');
      List.Clear;
      while not MetaData.Eof do begin
        Name := _VarToStr(NameField.Value);
        if FIConnection.SQLInfo.QuotesNeeded(Name) then
          Name := FIConnection.SQLInfo.Quote(Name);
        if AllTables then begin
          s := _VarToStr(SchemaField.Value);
          if s <> '' then begin
            if FIConnection.SQLInfo.QuotesNeeded(s) then
              s := FIConnection.SQLInfo.Quote(s);
            Name := s + '.' + Name;
          end;
          s := _VarToStr(CatalogField.Value);
          if s <> '' then begin
            if FIConnection.SQLInfo.QuotesNeeded(s) then
              s := FIConnection.SQLInfo.Quote(s);
            Name := s + '.' + Name;
          end;
        end
        else
          if DefaultTableSchema <> '' then begin
            s := _VarToStr(SchemaField.Value);
            if s <> '' then begin
              if _LowerCase(s) <> DefaultTableSchema then
                Name := s + '.' + Name;
            end;
          end;

        List.Add(Name);
        MetaData.Next;
      end;
    finally
      InternalDisconnect;
    end;
  finally
    MetaData.Free;
  end;
end;

procedure TCustomDAConnection.GetDatabaseNames(List: _TStrings);
var
  MetaData: TDAMetaData;
  Name: _string;
  DatabaseField: TField;
begin
  MetaData := CreateMetaData;
  try
    MetaData.MetaDataKind := 'Databases';
    MetaData.Open;
    DatabaseField := MetaData.FindField('DATABASE_NAME');
    List.Clear;
    while not MetaData.Eof do begin
      Name := _VarToStr(DatabaseField.Value);
      List.Add(Name);
      MetaData.Next;
    end;
  finally
    MetaData.Free;
  end;
end;

procedure TCustomDAConnection.GetStoredProcNames(List: _TStrings; AllProcs: boolean = False);
var
  MetaData: TDAMetaData;
  Name, s: _string;
  CatalogField, SchemaField, NameField, PackageField: TField;
  OverloadField: TField;
begin
  MetaData := CreateMetaData;
  try
    InternalConnect;
    try
      MetaData.MetaDataKind := 'procedures';
      if not AllProcs then
        MetaData.Restrictions.Add('SCOPE=LOCAL');
      MetaData.Open;
      NameField := MetaData.FindField('PROCEDURE_NAME');
      SchemaField := MetaData.FindField('PROCEDURE_SCHEMA');
      CatalogField := MetaData.FindField('PROCEDURE_CATALOG');
      PackageField := MetaData.FindField('PROCEDURE_PACKAGE');
      OverloadField := MetaData.FindField('OVERLOAD');
      List.Clear;
      while not MetaData.Eof do begin
        Name := _VarToStr(NameField.Value);
        if FIConnection.SQLInfo.QuotesNeeded(Name) then
          Name := FIConnection.SQLInfo.Quote(Name);
        if PackageField <> nil then begin
          s := _VarToStr(PackageField.Value);
          if s <> '' then begin
            if FIConnection.SQLInfo.QuotesNeeded(s) then
              s := FIConnection.SQLInfo.Quote(s);
            Name := s + '.' + Name;
          end;
        end;
        if AllProcs then begin
          s := _VarToStr(SchemaField.Value);
          if s <> '' then begin
            if FIConnection.SQLInfo.QuotesNeeded(s) then
              s := FIConnection.SQLInfo.Quote(s);
            Name := s + '.' + Name;
          end;
          s := _VarToStr(CatalogField.Value);
          if s <> '' then begin
            if FIConnection.SQLInfo.QuotesNeeded(s) then
              s := FIConnection.SQLInfo.Quote(s);
            Name := s + '.' + Name;
          end;
        end;
        if OverloadField <> nil then begin
          s := OverloadField.AsString;
          if s <> '' then
            Name := Name + FIConnection.SQLInfo.ProcedureOverloadSeparator + s;
        end;
        List.Add(Name);
        MetaData.Next;
      end;
    finally
      InternalDisconnect;
    end;
  finally
    MetaData.Free;
  end;
end;

{ Transaction control }

procedure TCustomDAConnection.SuppressAutoCommit;
var
  Temp: boolean;
begin
  Temp := False;
  FIConnection.SetProp(prAutoCommit, Temp);
end;

procedure TCustomDAConnection.RestoreAutoCommit;
begin
  FIConnection.SetProp(prAutoCommit, FAutoCommit);
end;

function TCustomDAConnection.DetectInTransaction(CanActivate: boolean = False): boolean;
var
  i: integer;
begin
  for i := 0 to FTransactions.Count - 1 do begin
    Result := FTransactions[i].DetectInTransaction(CanActivate and (FTransactions[i] = FInternalDefTransaction));
    if Result then
      exit;
  end;
  Result := False;
end;

function TCustomDAConnection.GetInTransaction: boolean;
begin
  Result := DetectInTransaction;
end;

function TCustomDAConnection.UsedTransaction: TDATransaction;
var
  i: integer;
begin
  Result := nil;
  if not IsMultipleTransactionsSupported then begin
    for i := 0 to FTransactions.Count - 1 do
      if FTransactions[i].Active then begin
        Result := FTransactions[i];
        exit;
      end;
  end;
  if Result = nil then
    Result := DefaultTransaction;
end;

function TCustomDAConnection.InternalAddTransaction(TR: TDATransaction): integer;
begin
  Result := FTransactions.IndexOf(TR);
  if Result = -1 then
    Result := FTransactions.Add(TR);
end;

procedure TCustomDAConnection.InternalRemoveTransaction(TR: TDATransaction);
begin
  FTransactions.Remove(TR);
  if TR = FDefaultTransaction then
    FDefaultTransaction := nil;
end;

function TCustomDAConnection.DoAddTransaction(TR: TDATransaction): integer;
begin
  Result := InternalAddTransaction(TR);
  if TR <> nil then
    TR.InternalAddConnection(Self);
end;

procedure TCustomDAConnection.DoRemoveTransaction(TR: TDATransaction);
begin
  if TR <> nil then                     //First we should close Transaction
    TR.InternalRemoveConnection(Self);  //Bug with DataSet.DefaultTransaction = nil
                                        //after InternalRemoveTransaction
  InternalRemoveTransaction(TR);        //Remove CLOSED transaction from Transaction list
                                        //and set DefaultTransaction to nil
end;

procedure TCustomDAConnection.StartTransaction;
begin
  if DefaultTransaction <> nil then
    DefaultTransaction.StartTransaction;
end;

procedure TCustomDAConnection.Commit;
begin
  if DefaultTransaction <> nil then
    DefaultTransaction.Commit;
end;

procedure TCustomDAConnection.Rollback;
begin
  if DefaultTransaction <> nil then
    DefaultTransaction.Rollback;
end;

procedure TCustomDAConnection.DoCommitRetaining;
begin
  if DefaultTransaction <> nil then
    DefaultTransaction.DoCommitRetaining;
end;

procedure TCustomDAConnection.DoRollbackRetaining;
begin
  if DefaultTransaction <> nil then
    DefaultTransaction.DoRollbackRetaining;
end;

procedure TCustomDAConnection.DoSavepoint(const Name: _string);
begin
  if DefaultTransaction <> nil then
    DefaultTransaction.DoSavepoint(Name);
end;

procedure TCustomDAConnection.DoRollbackToSavepoint(const Name: _string);
begin
  if DefaultTransaction <> nil then
    DefaultTransaction.DoRollbackToSavepoint(Name);
end;

procedure TCustomDAConnection.DoReleaseSavepoint(const Name: _string);
begin
  if DefaultTransaction <> nil then
    DefaultTransaction.DoReleaseSavepoint(Name);
end;

procedure TCustomDAConnection.ApplyUpdates(DataSets: array of TCustomDADataSet);
var
  DataSet: TCustomDADataSet;
  i: integer;
  ReApply: boolean;
  CUTransactions: TDATransactions;
  ImplicitStarts: array of boolean;
  CUTransaction: TDATransaction;
  CUDataSets: array of TCustomDADataSet;
  CUDataSetCount, CUTransactionCount: integer;
  OldAutoCommit: boolean;
begin
  CUTransactions := TDATransactions.Create;
  try
    CUTransactions.Capacity := TransactionCount;
    SetLength(CUDataSets, DataSetCount);
    CUTransactionCount := 0;
    CUDataSetCount := 0;

    //Form Update Transaction array and determine if we can use failover
    for i := 0 to High(DataSets) do begin
      DataSet := DataSets[i];
      if DataSet.Connection <> Self then
        DatabaseError(Format(SUpdateWrongDB, [DataSet.Name, Name]));

      if DataSet.Active and DataSet.CachedUpdates then begin
        CUTransaction := DataSet.UsedUpdateTransaction;
        if CUTransaction= nil then
          raise Exception.Create(STransactionNotAssigned);
        if CUTransactions.IndexOf(CUTransaction) = -1 then begin
          CUTransactions.Add(CUTransaction);
          Inc(CUTransactionCount);
        end;
        CUDataSets[CUDataSetCount] := DataSet;
        Inc(CUDataSetCount);
      end;
    end;
    SetLength(ImplicitStarts, CUTransactionCount);

    ReApply := False;
    PushOperation(clConnectionApply, IsFailOverAllowed);
    try
      repeat
        try
          //Start update transactions and detrmine the commit status
          for i := 0 to CUTransactionCount - 1 do
            if not CUTransactions[i].Active then begin
              if not ReApply then          //do not change transaction start flag in case of failover
                ImplicitStarts[i] := True;
              CUTransactions[i].StartTransaction;
            end
            else
              if not ReApply then
                ImplicitStarts[i] := False;

          ReApply := False;
          //Perform updates on each DataSet
          for i := 0 to CUDataSetCount - 1 do begin
            DataSet := CUDataSets[i];
            OldAutoCommit := DataSet.AutoCommit;
            DataSet.AutoCommit := False;
            try
              DataSet.ApplyUpdates;
            finally
              DataSet.AutoCommit := OldAutoCommit;
            end;
          end;

          //Commit all UpdateTransaction (this shoud be quick operation - it could break FailOver feature)
          if AutoCommit then
            for i := 0 to CUTransactionCount - 1 do
              if not IsMultipleTransactionsSupported or ImplicitStarts[i] then
                CUTransactions[i].Commit
              else
                CUTransactions[i].DoCommitRetaining;

        except
          on EFailOver do begin
            RestoreAfterFailOver; //restart read transactions
            ReApply := True;
          end
          else begin
            //RollBack all (uncommited !) changes
            for i := 0 to CUTransactionCount - 1 do
              if CUTransactions[i].Active then  //Transaction could be closed during FatalError handling
                                                //or during commiting, in case that exception was raised at commit time
                if not IsMultipleTransactionsSupported or ImplicitStarts[i] then
                  CUTransactions[i].Rollback
                else
                  CUTransactions[i].DoRollbackRetaining;
            raise;
          end;
        end;
      until (not ReApply);
    finally
      PopOperation;
    end;

    for i := 0 to CUDataSetCount - 1 do begin //this is not server operation so it couldn't raise failover
      DataSet := CUDataSets[i];
      DataSet.CommitUpdates;
    end;
  finally
    CUTransactions.Free;
  end;
end;

procedure TCustomDAConnection.ApplyUpdates;
var
  i: integer;
  DataSetArray: array of TCustomDADataSet;
begin
  SetLength(DataSetArray, 0);
  for i := 0 to DataSetCount - 1 do
    if DataSets[i] is TCustomDADataSet then begin
      SetLength(DataSetArray, Length(DataSetArray) + 1);
      DataSetArray[Length(DataSetArray) - 1] := TCustomDADataSet(DataSets[i]);
    end;

  ApplyUpdates(DataSetArray);
end;

//Operations stack
function TCustomDAConnection.PushOperation(Operation: TConnLostCause; AllowFailOver: boolean = true): integer;
var
  FOOperation: TFailOverOperation;
begin
  Result := 0;
  if Options.LocalFailover then begin
    if FOperationsStackLen = Length(FOperationsStack) then
      SetLength(FOperationsStack, FOperationsStackLen + OperationsStackDelta);

    Result := FOperationsStackLen;
    FOOperation.Operation := Operation;
    FOOperation.AllowFailOver := AllowFailOver;
    FOperationsStack[Result] := FOOperation;
    Inc(FOperationsStackLen);
  end;  
end;

function TCustomDAConnection.PopOperation: TConnLostCause;
begin
  Result := clUnknown;
  if Options.LocalFailover then begin
    Result := FOperationsStack[FOperationsStackLen].Operation;
    Dec(FOperationsStackLen);
  end;
end;

procedure TCustomDAConnection.ResetOnFatalError;
var
  i: integer;
begin
  //Close quietly all transactions cause of invalid connection (FatalError + FailOver)
  for i := 0 to FTransactions.Count - 1 do
    FTransactions[i].Reset;
end;

procedure TCustomDAConnection.RestoreAfterFailOver;
var
  i: integer;
begin
  for i := 0 to FTransactions.Count - 1 do
    FTransactions[i].Restore;
end;

function TCustomDAConnection.IsFailOverAllowed: boolean;
//This function check all transactions started against this connection
//and detrmine if FaiOver allowed according to transactions states:
//Connection must not have any active non ReadOnly ReadCommited transactions to perform FailOver
var
  i: integer;
  CRTrans: TCRTransaction;
begin
  for i := 0 to FTransactions.Count - 1 do begin
    CRTrans := FTransactions[i].FITransaction;
    if (CRTrans <> nil) and CRTrans.GetInTransaction and not CRTrans.CanRestoreAfterFailover then begin
      Result := False;
      exit;
    end;
  end;
  Result := True;
end;

function TCustomDAConnection.DetectConnLostCause(Component: TObject): TConnLostCause;
var
  i: integer;
  AllowFailOver: boolean;
begin
  Result := clUnknown;
  AllowFailOver := True;
  for i := FOperationsStackLen - 1 downto 0 do begin

    if Result < FOperationsStack[i].Operation then begin
      Result := FOperationsStack[i].Operation;
      AllowFailOver := FOperationsStack[i].AllowFailOver;
    end;

    case Result of
      clConnect: begin
        if TCustomDAConnection(Component).FShouldShowPrompt then
          Result := clUnknown;// This is the first connect or non DisconnectedMode - so we should raise exception
        break;
      end;
      clOpen, clExecute: begin
        if ((Component is TCustomDADataSet) and not TCustomDADataSet(Component).IsQuery) or
          (Component is TCustomDASQL) then
          Inc(FConnectCount); // Add ConnectCount - > cause of EndConnection in TCustomDADataSet.DoAfterExecute
      end;
    end;
  end;

  if not AllowFailOver then
    Result := clUnknown;

  case Result of
    clExecute, clOpen, clServiceQuery, clTransStart:
      if not IsFailOverAllowed then
        Result := clUnknown;                    //can't perform FailOver cause of possible unnoticed server changes lost
  end;
end;

procedure TCustomDAConnection.DoError(E: Exception; var Fail, Reconnect, Reexecute: boolean;
  ReconnectAttempt: integer; var ConnLostCause: TConnLostCause);
var
  i: integer;
  FatalError: boolean;
  RetryMode: TRetryMode;
begin
  ConnLostCause := clUnknown;
  TDASQLMonitorClass(SQLMonitorClass).DBError(EDAError(E));

  FatalError := EDAError(E).IsFatalError;

  if FatalError then begin

    with EDAError(E) do begin
      ConnLostCause := DetectConnLostCause(Component);

      Reconnect :=
        (Connected
        or ((ReconnectAttempt > 0) and Options.LocalFailover)       // After first abortive attempt Connected = False
        or (ConnLostCause = clConnect) and Options.DisconnectedMode)  // For disconnect mode TODO:
        and (IsFailOverAllowed or (ConnLostCause = clConnectionApply));

      if Reconnect then
        for i := 0 to DataSetCount - 1 do begin
          if not (DataSets[i] is TCustomDADataSet) then
            continue;
          if TCustomDADataSet(DataSets[i]).Prepared or
            ((DataSets[i].Active and not TCustomDADataSet(DataSets[i]).FetchAll
              and not TCustomDADataSet(DataSets[i]).Fetched) and
              not ((ConnLostCause = clRefresh) and (DataSets[i] = Component)))  //In case of Refresh and Active dataset with unfetched data.
            then begin
            Reconnect := False;
            Break;
          end;
        end;

      if Reconnect then
        for i := 0 to FSQLs.Count - 1 do begin
          Assert(TObject(FSQLs[i]) is TCustomDASQL);
          if TCustomDASQL(FSQLs[i]).Prepared and not TCustomDASQL(FSQLs[i]).Executing then begin
            Reconnect := False;
            Break;
          end;
        end;
    end;
  end;

  if Reconnect then
    if Options.LocalFailover then begin
      if (ConnLostCause = clUnknown) or (ConnLostCause = clExecute) then
        RetryMode := rmRaise
      else
        RetryMode := rmReconnectExecute;

      if Assigned(FOnConnectionLost) then
        FOnConnectionLost(Self, TComponent(EDAError(E).Component), ConnLostCause, RetryMode);
      Reconnect := RetryMode > rmRaise;
      Reexecute := ((RetryMode > rmReconnect) and not (ConnLostCause = clUnknown)) or
        ((ConnLostCause = clConnect) and (RetryMode >= rmReconnect));
      Fail := not Reexecute;
    end;

  if not Reexecute then
    if Assigned(FOnError) then
      FOnError(Self, EDAError(E), Fail);

  if FatalError and (FIConnection <> nil) then begin
    FIConnection.IsValid := False;
    if FIConnection.Pool <> nil then
      TCRConnectionPool(FIConnection.Pool).Invalidate;
  end;

  if FatalError and (ReconnectAttempt = 0) then
    ResetOnFatalError;

  if not FInProcessError and not Reconnect and FatalError and
    (ReconnectAttempt = 0) // If Attempt > 0 disconnect was called on CRAccess level
  then begin
    FInProcessError := True;
    try
      Disconnect;
    except // don't raise exception
    end;
    FInProcessError := False;
  end;
end;

procedure TCustomDAConnection.AssignTo(Dest: TPersistent);
begin
  if Dest is TCustomDAConnection then begin
    TCustomDAConnection(Dest).Username := Username;
    TCustomDAConnection(Dest).Password := Password;
    TCustomDAConnection(Dest).Server := Server;
    TCustomDAConnection(Dest).LoginPrompt := LoginPrompt;
    PoolingOptions.AssignTo(TCustomDAConnection(Dest).PoolingOptions);
    TCustomDAConnection(Dest).Pooling := Pooling;
    TCustomDAConnection(Dest).ConnectDialog := ConnectDialog;
    TCustomDAConnection(Dest).OnError := OnError;
    TCustomDAConnection(Dest).ConvertEOL := ConvertEOL;
    Options.AssignTo(TCustomDAConnection(Dest).Options);
    TCustomDAConnection(Dest).AutoCommit := AutoCommit;

    TCustomDAConnection(Dest).AfterConnect := AfterConnect;
    TCustomDAConnection(Dest).BeforeConnect := BeforeConnect;
    TCustomDAConnection(Dest).AfterDisconnect := AfterDisconnect;
    TCustomDAConnection(Dest).BeforeDisconnect := BeforeDisconnect;
    TCustomDAConnection(Dest).OnLogin := OnLogin;
  end
  else
    inherited;
end;

function TCustomDAConnection.GetConnected: boolean;
begin
  Result := (FIConnection <> nil) and FIConnection.GetConnected;
end;

procedure TCustomDAConnection.SetConnected(Value: boolean);
var
  Dialog: TCustomConnectDialog;
  DialogResult: boolean;
  OldBeforeDisconnect: TNotifyEvent;
  StoredConnectCount: integer;
begin
  OldBeforeDisconnect := nil;
  try
    if Value <> GetConnected then begin
      try
        // ignore exceptions to disconnect all client
        if not Value then begin
          FConnectCount := 0; //Explicit disconnect
          FShouldShowPrompt := True;
          if Assigned(BeforeDisconnect) then BeforeDisconnect(Self);
          while True do
            try
              SendConnectEvent(False);
              break;
            except
              on E: EDAError do
                if not E.IsFatalError then
                  raise
            end;
        end
        else
          if not FStreamedConnected then //DFM loading issue
            Inc(FConnectCount);
      finally
        if csReading in ComponentState then begin
          if FOptions.KeepDesignConnected or (csDesigning in ComponentState) then
            FStreamedConnected := Value;
        end
        else if not Value then begin
          OldBeforeDisconnect := BeforeDisconnect;
          if Assigned(BeforeDisconnect) then //Design-time event lose fix
            BeforeDisconnect := nil;

          inherited SetConnected(False); //There is no server operations
        end;
      end;
      if not (csReading in ComponentState) and Value then begin
        if NeedPrompt and (ConnectDialogClass <> nil) then begin
          if FConnectDialog = nil then
            Dialog := ConnectDialogClass.Create(nil)
          else
            Dialog := FConnectDialog;
          StoredConnectCount := FConnectCount;
          DialogResult := False;
          try
            Dialog.FConnection := Self;
            DialogResult := Dialog.Execute;
          finally
            if not DialogResult then
              Dec(StoredConnectCount);
            FConnectCount := StoredConnectCount;
            if FConnectDialog = nil then
              Dialog.Free;
          end;

          if not DialogResult then begin
            if FStreamedConnected or (csDesigning in ComponentState) then
              DatabaseError(SCannotConnect)
            else
              Abort;
          end;
        end
        else begin
          StoredConnectCount := FConnectCount;
          try
            try
              PerformConnect;
            except
              if not Connected then
                Dec(StoredConnectCount);//Restore ConnectCount in case of Connection Failure
              raise;
            end;
          finally
            FConnectCount := StoredConnectCount;
          end;
        end;
      end;
    end;
  finally
    if Assigned(OldBeforeDisconnect) then
      BeforeDisconnect := OldBeforeDisconnect;
  end;
end;

procedure TCustomDAConnection.SetDefaultTransaction(Value: TDATransaction);
begin
  if Value = FInternalDefTransaction then
    Value := nil;
  if FDefaultTransaction <> Value then begin
    if FDefaultTransaction <> nil then begin
      FDefaultTransaction.RemoveFreeNotification(Self);
      DoRemoveTransaction(FDefaultTransaction);
    end;

    if Value <> nil then begin
      DoAddTransaction(Value);
      Value.FreeNotification(Self);
    end;

    FDefaultTransaction := Value;
  end;
end;

function TCustomDAConnection.GetDefaultTransaction: TDATransaction;
begin
  Result := FDefaultTransaction;
  if Result = nil then
    Result := FInternalDefTransaction;
end;

function TCustomDAConnection.GetTransaction(Index: integer): TDATransaction;
begin
  Result := FTransactions[Index];
end;

function TCustomDAConnection.GetTransactionsCount: integer;
begin
  Result := FTransactions.Count;
end;

procedure TCustomDAConnection.SetUsername(const Value: _string);
begin
  if Value <> FUsername then begin
    Disconnect;
    FUsername := Value;
  end;
end;

procedure TCustomDAConnection.SetPassword(const Value: _string);
begin
  if Value <> FPassword then begin
    Disconnect;
    FPassword := Value;
  end;
end;

procedure TCustomDAConnection.SetServer(const Value: _string);
begin
  if Value <> FServer then begin
    Disconnect;
    FServer := Value;
  end;
end;

function TCustomDAConnection.GetConnectString: _string;
begin
  Result := '';
end;

procedure TCustomDAConnection.SetConnectString(Value: _string);
begin
end;

procedure TCustomDAConnection.SetAutoCommit(Value: boolean);
begin
  FAutoCommit := Value;
  if FIConnection <> nil then
    FIConnection.SetProp(prAutoCommit, FAutoCommit);
end;

procedure TCustomDAConnection.SetConvertEOL(Value: boolean);
begin
  FConvertEOL := Value;
  if FIConnection <> nil then
    FIConnection.SetProp(prConvertEOL, Value);
end;

procedure TCustomDAConnection.CheckCommand;
begin
  if FCommand = nil then begin
    FCommand := CreateSQL;
    FCommand.Debug := Debug;
  end;
end;

procedure TCustomDAConnection.AssignConnectOptions(Source: TCustomDAConnection);
begin
  Username := Source.Username;
  Password := Source.Password;
  Server := Source.Server;
end;

function TCustomDAConnection.CreateOptions: TDAConnectionOptions;
begin
  Result := TDAConnectionOptions.Create(Self);
end;

procedure TCustomDAConnection.SetOptions(Value: TDAConnectionOptions);
begin
  FOptions.Assign(Value);
end;

function TCustomDAConnection.CreatePoolingOptions: TPoolingOptions;
begin
  Result := TPoolingOptions.Create(Self);
end;

procedure TCustomDAConnection.SetPoolingOptions(Value: TPoolingOptions);
begin
  FPoolingOptions.Assign(Value);
end;

procedure TCustomDAConnection.SetConnectDialog(Value: TCustomConnectDialog);
begin
  if Value <> FConnectDialog then begin
    if FConnectDialog <> nil then begin
      RemoveFreeNotification(FConnectDialog);
      if FConnectDialog.FConnection = Self then
        FConnectDialog.FConnection := nil;
    end;

    FConnectDialog := Value;

    if FConnectDialog <> nil then begin
      FreeNotification(FConnectDialog);
      FConnectDialog.FConnection := Self;
    end;
  end;
end;

procedure TCustomDAConnection.SetPooling(Value: boolean);
begin
  if FPooling <> Value then begin
    SetConnected(False);
    FreeIConnection;
  end;
  FPooling := Value;
end;

procedure TCustomDAConnection.SetDebug(Value: boolean);
begin
  FDebug := Value;
  if FCommand <> nil then
    FCommand.Debug := Value;
end;

procedure TCustomDAConnection.DoAfterConnect;
begin
  if Assigned(AfterConnect) then
    AfterConnect(Self);
end;

{ TDAParamsInfo }

function TDAParamsInfo.GetItem(Index: Integer): TDAParamInfo;
begin
  Result := TDAParamInfo(inherited GetItem(Index));
end;

procedure TDAParamsInfo.SetItem(Index: Integer; Value: TDAParamInfo);
begin
  inherited SetItem(Index, Value);
end;

{ TDAParam }
constructor TDAParam.Create(Collection: TCollection);
begin
  inherited;

  Bound := BoundParams;
end;

destructor TDAParam.Destroy;
begin
  FreeObject;

  inherited;
end;

procedure TDAParam.Clear;
begin
  if IsBlobDataType then
    TBlob(FParamObject).Clear
  else
    inherited Clear;
end;

procedure TDAParam.Assign(Source: TPersistent);
{$IFDEF VER6P}
var
  StreamPersist: IStreamPersist;
{$ENDIF}

{$IFDEF VER6P}
  procedure Load(const StreamPersist: IStreamPersist);
  var
    MS: TMemoryStream;
  begin
    if not (DataType in [ftBlob, ftMemo{$IFDEF VER10P}, ftWideMemo{$ENDIF}, ftOraBlob, ftOraClob]) then
      raise Exception.Create(SDataTypeNotSupported);

    MS := TMemoryStream.Create;
    try
      StreamPersist.SaveToStream(MS);
      LoadFromStream(MS, DataType);
    finally
      MS.Free;
    end;
  end;
{$ENDIF}

begin
  if Source is TDAParam then begin
    AssignParam(TParam(Source));
  {$IFDEF VER6P}
    TParam(Self).Size := TParam(Source).Size; // CR11511
  {$ENDIF}
    FSize := TDAParam(Source).FSize; // CR11511
    ParamObject := TDAParam(Source).ParamObject;
    National := TDAParam(Source).National;
  end
  else
  if Source is TParam then
    AssignParam(TParam(Source))
  else
  if Source is TField then
    AssignField(TField(Source))
  else
  if Source is TStrings then
    AsMemo := TStrings(Source).Text
  else
{$IFDEF VER6P}
  if Supports(Source, IStreamPersist, StreamPersist) then
    Load(StreamPersist)
  else
{$ENDIF}
    inherited Assign(Source);
end;

procedure TDAParam.AssignParam(Param: TParam);
begin
  if Param <> nil then begin
    DataType := Param.DataType;
    if Param.IsNull then
      Clear
    else
      if IsBlobDataType and not (Param is TDAParam) then
        // in MIDAS we need to do such assignment
        // as TDAParam.Value = TParam.Value
        Value := Param.Value
      else
        inherited Value := Param.Value;
    Name := Param.Name;
    if ParamType = ptUnknown then
      ParamType := Param.ParamType;
  end;
end;

procedure TDAParam.AssignTo(Dest: TPersistent);
begin
  if Dest is TField then
    TField(Dest).Value := Value
  else
    inherited AssignTo(Dest);
end;

procedure TDAParam.AssignField(Field: TField);
begin
  if Field <> nil then begin
    AssignFieldValue(Field, Field.Value);
    Name := Field.FieldName;
  end;
end;

procedure TDAParam.AssignFieldValue(Field: TField; const Value: Variant);
begin
  if Field <> nil then begin
    if (Field.DataType = ftString) and TStringField(Field).FixedChar then
      DataType := ftFixedChar
    else
    if (Field.DataType = ftWideString) and TStringField(Field).FixedChar then
      DataType := TFieldType(ftFixedWideChar)
    else
    if (Field.DataType = ftMemo) and (Field.Size > 255) then
      DataType := ftString
    else
  {$IFDEF VER10P}
    if (Field.DataType = ftWideMemo) and (Field.Size > 255) then
      DataType := ftWideString
    else
  {$ENDIF}
      DataType := Field.DataType;
    if
      VarIsNull(Value)
    {$IFDEF CLR}
      or ((Field is TBytesField) and Field.IsNull) // See d7 TField.GetAsByteArray and d8 TVarBytesField(TField).GetAsByteArray for details
    {$ENDIF}
    then
      Clear
    else
      Self.Value := Value;
  end;
end;

procedure TDAParam.LoadFromFile(const FileName: string; BlobType: TBlobType);
var
  Stream: TStream;
begin
  Stream := TFileStream.Create(FileName, fmOpenRead);
  try
    LoadFromStream(Stream, BlobType);
  finally
    Stream.Free;
  end;
end;

procedure TDAParam.LoadFromStream(Stream: TStream; BlobType: TBlobType);
begin
  if not (BlobType in [ftBlob, ftMemo{$IFDEF VER10P}, ftWideMemo{$ENDIF}, ftOraBlob, ftOraClob]) then
    raise Exception.Create(SDataTypeNotSupported);
  with Stream do begin
    DataType := BlobType;
    Position := 0;
    Assert(FParamObject <> nil, SDataTypeNotSupported);
    TBlob(FParamObject).LoadFromStream(Stream);
  end;
end;

procedure TDAParam.SetBlobData(Buffer: {$IFNDEF CLR}Pointer{$ELSE}TBytes{$ENDIF}; Size: Integer);
{$IFDEF VER12P}
var
  NewBuf: TBytes;
{$ENDIF}
begin
{$IFNDEF VER12P}
  AsBlob := Encoding.Default.GetString(TBytes(Buffer), 0, Size);
{$ELSE}
{$IFNDEF CLR}
  SetLength(NewBuf, Size);
  Move(Buffer^, Pointer(NewBuf)^, Size);
{$ELSE}
  NewBuf := Copy(Buffer, 0, Size);
{$ENDIF}
  AsBlob := NewBuf;
{$ENDIF}
end;

procedure TDAParam.SetBlobData(Buffer: TValueBuffer);
begin
  DataType := ftBlob;
  AsAnsiString := Marshal.PtrToStringAnsi(Buffer);
end;

procedure TDAParam.CreateObject;
begin
  Assert(FParamObject = nil);

  if DataType in [ftBlob, ftMemo{$IFDEF VER10P}, ftWideMemo{$ENDIF}] then begin
    FParamObject := TBlob.Create;
  {$IFDEF VER10P}
    if DataType = ftWideMemo then
      TBlob(FParamObject).IsUnicode := True;
  {$ENDIF}
  end;
end;

procedure TDAParam.FreeObject;
begin
  if FParamObject <> nil then begin
    FParamObject.Free;
    FParamObject := nil;
  end;
end;

function TDAParam.GetNativeParamObject: TSharedObject;
begin
  Result := FParamObject;
end;

procedure TDAParam.SetParamObject(Value: TSharedObject);
begin
  FreeObject;

  FParamObject := Value;
  if FParamObject <> nil then begin
    FParamObject.AddRef;
    inherited Value := 'Object'; // for IsNull = False
  end;
end;

function TDAParam.IsDataTypeStored: boolean;
begin
  Result := Integer(DataType) <= Integer(High(TFieldType));
end;

function TDAParam.IsValueStored: boolean;
begin
  Result := {$IFNDEF FPC}Bound and not VarIsArray(Value){$ELSE}False{$ENDIF};
end;

procedure TDAParam.DefineProperties(Filer: TFiler);

  function WriteData: boolean;
  begin
    Result := not IsDataTypeStored;
  end;

begin
  inherited DefineProperties(Filer);
  
  Filer.DefineProperty('ExtDataType', ReadExtDataType, WriteExtDataType, WriteData);
end;

procedure TDAParam.ReadExtDataType(Reader: TReader);
begin
  DataType := TFieldType(Reader.ReadInteger);
end;

procedure TDAParam.WriteExtDataType(Writer: TWriter);
begin
  Writer.WriteInteger(Integer(DataType));
end;

function TDAParam.IsObjectDataType(DataType: TFieldType): boolean;
begin
  Result := DataType in [ftMemo{$IFDEF VER10P}, ftWideMemo{$ENDIF}, ftBlob];
end;

function TDAParam.IsObjectDataType: boolean;
begin
  Result := IsObjectDataType(DataType);
end;

function TDAParam.IsBlobDataType(DataType: TFieldType): boolean;
begin
  Result := DataType in [ftMemo{$IFDEF VER10P}, ftWideMemo{$ENDIF}, ftBlob];
end;

function TDAParam.IsBlobDataType: boolean;
begin
  Result := IsBlobDataType(DataType);
end;

function TDAParam.GetDataType: TFieldType;
begin
  Result := inherited DataType;
end;

procedure TDAParam.SetDataType(Value: TFieldType);
begin
  if Value <> inherited DataType then begin
    if IsObjectDataType then
      FreeObject;

    inherited DataType := Value;

    SubDataType := dtUnknown;

    if IsObjectDataType then
      CreateObject;
  end;
end;

function TDAParam.GetSize: integer;
begin
  Result := FSize;
end;

procedure TDAParam.SetSize(Value: integer);
begin
  FSize := Value;
end;

function TDAParam.GetAsString: string;
begin
{$IFDEF VER12P}
  Result := AsWideString;
{$ELSE}
{$IFDEF CLR}
  Result := AsWideString;
{$ELSE}
  Result := AsAnsiString;
{$ENDIF}
{$ENDIF}
end;

procedure TDAParam.SetAsString(const Value: string);
begin
{$IFDEF VER12P}
  if ParamStringAsAnsiString then
    AsAnsiString := AnsiString(Value)
  else
    AsWideString := Value;
{$ELSE}
{$IFDEF CLR}
  if ParamStringAsAnsiString then
    AsAnsiString := AnsiString(Value)
  else
    AsWideString := Value;
{$ELSE}
  AsAnsiString := Value;
{$ENDIF}
{$ENDIF}
end;

function TDAParam.GetAsAnsiString: AnsiString;
begin
  if IsNull then
    Result := ''
  else
  if IsBlobDataType then begin
    Assert(FParamObject is TBlob);
    Result := TBlob(FParamObject).AsAnsiString;
  end
  else
  {$IFNDEF CLR}
    if DataType in [ftDate, ftDateTime] then
      Result := AnsiString(DateToStr(TVarData(Value).VDate))
    else
  {$ENDIF}
      Result := inherited {$IFDEF VER12P}AsAnsiString{$ELSE}AsString{$ENDIF};
end;

procedure TDAParam.SetAsAnsiString(const Value: AnsiString);
begin
  if IsBlobDataType then
    TBlob(FParamObject).AsAnsiString := Value
  else
  if (DataType = ftWideString) or (Integer(DataType) = Integer(ftFixedWideChar)) then
    AsWideString := WideString(Value)
  else
    inherited {$IFDEF VER12P}AsAnsiString{$ELSE}AsString{$ENDIF} := Value;
end;

function TDAParam.GetAsWideString: WideString;
begin
  if IsNull then
    Result := ''
  else
  if IsBlobDataType then begin
    Assert(FParamObject is TBlob);
    Result := TBlob(FParamObject).AsWideString;
  end
  else
  {$IFNDEF CLR}
    if DataType in [ftDate, ftDateTime] then
      Result := WideString(DateToStr(TVarData(Value).VDate))
  else
  {$ENDIF}
    Result := inherited {$IFDEF VER12P}AsWideString{$ELSE}Value{$ENDIF};
end;

procedure TDAParam.SetAsWideString(const Value: WideString);
begin
  if IsBlobDataType then
    TBlob(FParamObject).AsWideString := Value
  else
    inherited {$IFDEF VER12P}AsWideString{$ELSE}Value{$ENDIF} := Value;
end;

{$IFDEF VER12P}
function TDAParam.GetAsBytes: TBytes;
begin
  if IsNull then
    Result := nil
  else
  if IsBlobDataType then begin
    Assert(FParamObject is TBlob);
    Result := TBlob(FParamObject).AsBytes;
  end
  else
    Result := inherited AsBytes;
end;

procedure TDAParam.SetAsBytes(const Value: TBytes);
begin
  if IsBlobDataType then
    TBlob(FParamObject).AsBytes := Value
  else
    inherited AsBytes := Value;
end;
{$ENDIF}

function TDAParam.GetAsInteger: integer;
begin
  Result := inherited AsInteger;
end;

procedure TDAParam.SetAsInteger(Value: integer);
begin
  inherited AsInteger := Value;
end;

procedure TDAParam.SetAsSmallInt(Value: LongInt);
begin
  inherited AsSmallInt := Value;
end;

procedure TDAParam.SetAsWord(Value: LongInt);
begin
  inherited AsWord := Value;
end;

function TDAParam.GetAsFloat: double;
begin
  Result := inherited AsFloat;
end;

procedure TDAParam.SetAsFloat(Value: double);
begin
  inherited AsFloat := Value;
end;

{$IFDEF VER6P}
function TDAParam.GetAsLargeInt: Int64;
begin
{$IFNDEF VER12P}
  if IsNull then
    Result := 0
  else
    Result := Value;
{$ELSE}
  Result := inherited AsLargeInt;
{$ENDIF}
end;

procedure TDAParam.SetAsLargeInt(const Value: Int64);
begin
{$IFNDEF VER12P}
  DataType := ftLargeInt;
  Self.Value := Value;
{$ELSE}
  inherited AsLargeInt := Value;
{$ENDIF}
end;
{$ENDIF}

{$IFDEF VER12P}
procedure TDAParam.SetAsShortInt(Value: LongInt);
begin
  inherited AsShortInt := Value;
end;

procedure TDAParam.SetAsByte(Value: LongInt);
begin
  inherited AsByte := Value;
end;
{$ENDIF}

procedure TDAParam.SetAsBlob(const Value: TBlobData);
begin
  DataType := ftBlob;
  TBlob(FParamObject).{$IFDEF VER12P}AsBytes{$ELSE}AsString{$ENDIF} := Value;
end;

procedure TDAParam.SetAsMemo(const Value: string);
begin
  DataType := ftMemo;
  Assert(FParamObject <> nil);
  TBlob(FParamObject).AsString := Value
end;

function TDAParam.GetAsBlobRef: TBlob;
begin
  if DataType = ftUnknown then
    DataType := ftBlob;

  if IsBlobDataType then
    Result := FParamObject as TBlob
  else
    Result := nil;
end;

procedure TDAParam.SetAsBlobRef(const Value: TBlob);
begin
  FreeObject;

  inherited DataType := ftBlob;

  ParamObject := Value;
end;

function TDAParam.GetAsMemoRef: TBlob;
begin
  if DataType = ftUnknown then begin
  {$IFDEF VER10P}
    if IsBlobDataType and (FParamObject is TBlob) and TBlob(FParamObject).IsUnicode then
      DataType := ftWideMemo
    else
  {$ENDIF}
      DataType := ftMemo;
  end;

  if IsBlobDataType then
    Result := FParamObject as TBlob
  else
    Result := nil;
end;

procedure TDAParam.SetAsMemoRef(const Value: TBlob);
begin
  FreeObject;

{$IFDEF VER10P}
  if Value.IsUnicode then
    inherited DataType := ftWideMemo
  else
{$ENDIF}
    inherited DataType := ftMemo;

  ParamObject := Value;
end;

function TDAParam.GetAsVariant: variant;
begin
  if IsBlobDataType then
    Result := TBlob(FParamObject).AsString
  else
    Result := inherited Value;
end;

procedure TDAParam.SetAsVariant(const Value: variant);
begin
  if IsBlobDataType then begin
    if VarType(Value) = varArray or varByte then
      TBlob(FParamObject).AsBytes := Value
    else
      TBlob(FParamObject).AsString := VarToStr(Value);
  end
  else
    inherited Value := Value;
  Bound := True;  
end;

{$IFDEF VER6P}
{$IFNDEF FPC}
function TDAParam.GetAsSQLTimeStamp: TSQLTimeStamp;
begin
  Result := inherited AsSQLTimeStamp;
end;

procedure TDAParam.SetAsSQLTimeStamp(const Value: TSQLTimeStamp);
begin
  inherited AsSQLTimeStamp := Value;
end;
{$ENDIF}
{$ENDIF}

function TDAParam.GetAsCursor: TCRCursor;
begin
  if DataType = ftUnknown then
    DataType := ftCursor;

  if DataType = ftCursor then
    Result := FParamObject as TCRCursor
  else
    Result := nil;
end;

procedure TDAParam.SetAsCursor(Value: TCRCursor);
begin
  FreeObject;

  inherited DataType := ftCursor;

  ParamObject := Value;
end;

procedure TDAParam.SetText(const Value: string);
begin
  if IsBlobDataType then
    TBlob(FParamObject).AsString := Value
  else
    inherited SetText(Value);
end;

function TDAParam.GetIsNull: boolean;
begin
  if IsBlobDataType then
    Result := TBlob(FParamObject).Size = 0
  else
    Result := inherited IsNull;
end;

procedure TDAParam.SetIsNull(Value: boolean);
begin
  if Value then
    inherited Value := Null
  else
    inherited Value := '';
end;

function TDAParam.GetNational: Boolean;
begin
  Result := FNational;
end;

procedure TDAParam.SetNational(Value: Boolean);
begin
  FNational := Value;
end;

{ TDAParams }

constructor TDAParams.Create(Owner: TPersistent);
begin
  inherited Create(TDAParam);

  FOwner := Owner;
  FNeedsUpdateItem := True;
  FParamsChangeType := ctGenerated;
end;

function TDAParams.GetOwner: TPersistent;
begin
  Result := FOwner;
end;

procedure TDAParams.Update(Item: TCollectionItem);
begin
  if FParamsChangeType = ctUserChecked then
    FParamsChangeType := ctUsers;

  if FNeedsUpdateItem then
    inherited;
end;

procedure TDAParams.Disconnect;
var
  i: integer;
  ParamObject: TSharedObject;
begin
  for i := 0 to Count - 1 do begin
    ParamObject := Items[i].ParamObject;
    if ParamObject <> nil then
      ParamObject.Disconnect;
  end;
end;

function TDAParams.GetItem(Index: integer): TDAParam;
begin
  Result := TDAParam(inherited Items[Index]);
end;

procedure TDAParams.SetItem(Index: integer; Value: TDAParam);
begin
  inherited Items[Index] := Value;
end;

function TDAParams.ParamByName(const Value: _string): TDAParam;
begin
  Result := TDAParam(inherited ParamByName(Value));
end;

function TDAParams.FindParam(const Value: _string): TDAParam;
begin
  Result := TDAParam(inherited FindParam(Value));
end;

function TDAParams.CreateParam(FldType: TFieldType; const ParamName: _string;
  ParamType: TParamType): TDAParam;
begin
  Result := TDAParam(inherited CreateParam(ftUnknown, ParamName, ParamType));
  Result.DataType := FldType;
{$IFDEF FPC}
  Result.ParamType := ParamType;
{$ENDIF}  
end;

{ TDACursorField }

constructor TDACursorField.Create(Owner: TComponent);
begin
  inherited Create(Owner);

  SetDataType(ftCursor);
end;

function TDACursorField.GetAsCursor: TCRCursor;
begin
  if not GetValue(Result) then
    Result := nil;
end;

function TDACursorField.GetValue(var Value: TCRCursor): boolean;
var
  Handle: IntPtr;
begin
  Handle := Marshal.AllocHGlobal(sizeof(IntPtr));
  Marshal.WriteIntPtr(Handle, nil);
  try
    Result := GetData(Handle);
    if Result then
      Value := TCRCursor(GetGCHandleTarget(Marshal.ReadIntPtr(Handle)));
  finally
    Marshal.FreeHGlobal(Handle);
  end;
end;

{ TCustomDADataSet }

constructor TCustomDADataSet.Create(Owner: TComponent);
var
  stIdx: TStatementType;
begin
  inherited Create(Owner);

  for stIdx := Low(TStatementType) to High(TStatementType) do
    if stIdx in GetUpdateSQLStatementTypes then begin
      Assert(FUpdateSQL[stIdx] = nil);
      FUpdateSQL[stIdx] := _TStringList.Create;
      _TStringList(FUpdateSQL[stIdx]).OnChange := ScanMacros;
    end;

  FFetchRows := 25;
  FAutoCommit := True;
  FRowsAffected := -1;
  FRefreshOptions := [];

  CreateCommand;

  FDesignCreate := csDesigning in ComponentState;
  FOptions := CreateOptions;
end;

destructor TCustomDADataSet.Destroy;
var
  stIdx: TStatementType;
begin
  Close;
  UnPrepare;

  if UsedConnection <> nil then
    UsedConnection.UnregisterClient(Self);

  FreeCommand;

  FOptions.Free;

  for stIdx := Low(FUpdateSQL) to High(FUpdateSQL) do begin
    FUpdateSQL[stIdx].Free;
    FUpdateSQL[stIdx] := nil;
  end;

  if UsedConnection <> nil then
    TDASQLMonitorClass(UsedConnection.SQLMonitorClass).ObjectDestroyed(Self);

  inherited;

  SetUpdateObject(nil);
end;

procedure TCustomDADataSet.CheckInactive;
begin
  inherited CheckInactive;
end;

procedure TCustomDADataSet.CreateIRecordSet;
begin
  if UsedConnection <> nil then
    SetIRecordSet(UsedConnection.CreateIRecordSet)
  else
    SetIRecordSet(nil);
end;

procedure TCustomDADataSet.FreeIRecordSet;
begin
  FIRecordSet.Free;
  SetIRecordSet(nil);
end;

procedure TCustomDADataSet.SetIRecordSet(Value: TData);
begin
  inherited;

  FIRecordSet := TCRRecordSet(Value); // Value as TCRRecordSet;
  if FIRecordSet <> nil then begin
    if FConnection <> nil then
      FIRecordSet.SetConnection(FConnection.FIConnection)
    else
      FIRecordSet.SetConnection(nil);

    if FTransaction <> nil then
      FIRecordSet.SetTransaction(FTransaction.FITransaction)
    else
      FIRecordSet.SetTransaction(nil);

    FICommand := FIRecordSet.GetCommand;
    FIRecordSet.SetProp(prUniDirectional, FUniDirectional);
    FIRecordSet.SetProp(prFetchRows, FFetchRows);
    FIRecordSet.SetProp(prFetchAll, FFetchAll);
    FIRecordSet.SetProp(prReadOnly, FReadOnly);
    FIRecordSet.SetProp(prRoAfterUpdate, roAfterUpdate in RefreshOptions);
    if FOptions <> nil then begin
      FIRecordSet.SetProp(prLongStrings, FOptions.FLongStrings);
      FIRecordSet.SetProp(prFlatBuffers, FOptions.FFlatBuffers);
      FIRecordSet.TrimFixedChar := FOptions.TrimFixedChar;
      FIRecordSet.TrimVarChar := FOptions.TrimVarChar;
      FIRecordSet.SetEmptyStrToNull := FOptions.SetEmptyStrToNull;

      FICommand.SetProp(prQuoteNames, FOptions.QuoteNames);
    {$IFDEF HAVE_COMPRESS}
      FIRecordSet.SetProp(prCompressBlobMode, Integer(FOptions.CompressBlobMode));
    {$ENDIF}
      FIRecordSet.SetProp(prDefaultValues, Options.DefaultValues);
      FIRecordSet.SetProp(prFieldsOrigin, Options.FieldsOrigin);
      FIRecordSet.SetProp(prExtendedFieldsInfo, Options.ExtendedFieldsInfo);
      FICommand.SetProp(prEnableBCD, Options.EnableBCD);
    {$IFDEF VER6P}
    {$IFNDEF FPC}
      FICommand.SetProp(prEnableFmtBCD, Options.EnableFMTBCD);
    {$ENDIF}
    {$ENDIF}
    end;

    FIRecordSet.AfterExecFetch := DoAfterExecFetch;
    FIRecordSet.AfterFetchAll := DoAfterFetchAll;
    FIRecordSet.OnBeforeFetch := DoOnBeforeFetch;
    FIRecordSet.OnAfterFetch := DoOnAfterFetch;
    FIRecordSet.OnDataChanged := DoOnDataChanged;
    FIRecordSet.OnFieldsChanged := DoOnFieldsChanged;
    FIRecordSet.Component := Self;
  end
  else
    FICommand := nil;

  if FCommand <> nil then
    FCommand.SetICommand(FICommand);

  if FICommand <> nil then begin
    FICommand.SetProp(prAutoCommit, FAutoCommit);
    FICommand.AfterExecute := DoAfterExecute;
  end;
end;

procedure TCustomDADataSet.CheckIRecordSet;
var
  ClassType: TClass;
begin
  if (UsedConnection <> nil) then
    ClassType := UsedConnection.GetIRecordSetClass
  else
    ClassType := nil;

  if (ClassType = nil) or not IsClass(FIRecordSet, ClassType) then begin
    FreeIRecordSet;
    CreateIRecordSet;
  end;
end;

procedure TCustomDADataSet.CreateCommand;
begin
  SetCommand(TCustomDASQL.Create(Self));
end;

procedure TCustomDADataSet.FreeCommand;
begin
  FCommand.Free;
  FCommand := nil;
end;

procedure TCustomDADataSet.SetCommand(Value: TCustomDASQL);
begin
  //FreeCommand;

  FCommand := Value;
  if FCommand <> nil then begin
    FCommand.FDataSet := Self;
    FCommand.SetICommand(FICommand);

    FParams := FCommand.Params;
    FMacros := FCommand.Macros;
  end;
end;

procedure TCustomDADataSet.SetDataSetService(Value: TDataSetService);
begin
  inherited;
  
  FDataSetService := TDADataSetService(Value);
end;

function TCustomDADataSet.CreateOptions: TDADataSetOptions;
begin
  Result := TDADataSetOptions.Create(Self);
end;

procedure TCustomDADataSet.Loaded;
begin
  FStreamedOpen := True;
  try
    inherited;
    FDesignCreate := False;
  finally
    FStreamedOpen := False;
  end;
end;

function TCustomDADataSet.UsedConnection: TCustomDAConnection;
begin
  Result := FConnection
end;

procedure TCustomDADataSet.CheckConnection;
begin
  BeginConnection(False);
end;

procedure TCustomDADataSet.BeginConnection(NoConnectCheck: boolean);
  function HasDataSet(DAConnection: TCustomDAConnection; DataSet: TDataSet): boolean;
  var
    i: integer;
  begin
    for i := 0 to DAConnection.DataSetCount - 1 do begin
      if DAConnection.DataSets[i] = DataSet then begin
        Result := True;
        exit;
      end;
    end;
    Result := False;
  end;
var
  UseDefaultConnection: boolean;  
  vUsedConnection: TCustomDAConnection;
  vUsedTransaction: TDATransaction;
begin
  vUsedConnection := UsedConnection;
  if vUsedConnection = nil then
    DatabaseError(SConnectionNotDefined);

  if NoConnectCheck then
    vUsedConnection.InternalConnect // We should call connect each time to update ConnectCount
  else
    if not vUsedConnection.Connected then
      vUsedConnection.Connect;

  UseDefaultConnection := (FConnection = nil) and not HasDataSet(vUsedConnection, Self);

  CheckIRecordSet;
  CheckDataSetService;

  if vUsedConnection.IsMultipleTransactionsSupported then begin
    vUsedTransaction := UsedTransaction;
    if vUsedTransaction = nil then
      DatabaseError(STransactionNotAssigned);

    if NoConnectCheck then
      vUsedTransaction.GainTransaction // We should call each time to update TrStartCount
    else
     if not vUsedTransaction.Active then
       vUsedTransaction.StartTransaction;

    FIRecordSet.SetTransaction(vUsedTransaction.FITransaction);
  end;

  // use default session
  if UseDefaultConnection then begin
    vUsedConnection.RegisterClient(Self, ConnectChange);

    FIRecordSet.SetConnection(vUsedConnection.FIConnection)
  end;
end;

procedure TCustomDADataSet.EndConnection;
var
  vUsedConnection: TCustomDAConnection;
  vUsedTransaction: TDATransaction;
begin
  vUsedConnection := UsedConnection;

  if vUsedConnection <> nil then begin
    if vUsedConnection.IsMultipleTransactionsSupported then begin
      vUsedTransaction := UsedTransaction;
      vUsedTransaction.ReleaseTransaction; // Release and Stop transaction
    end;

    vUsedConnection.InternalDisconnect;
  end;  
end;

procedure TCustomDADataSet.Disconnect(NeedClose: boolean = True);
begin
  if NeedClose then begin
    Close;
    UnPrepare;
    FieldDefs.Updated := False;
  end
  else
    if FIRecordSet <> nil then
      FIRecordSet.Disconnect;

  Params.Disconnect;
end;

procedure TCustomDADataSet.ConnectChange(Sender: TObject; Connecting: boolean);
begin
  if not Connecting then begin
    Disconnect(not (TCustomDAConnection(Sender).Options.DisconnectedMode or FDisconnected));
  end
  else
    if not (Sender is TCustomDAConnection) and (FIRecordSet <> nil) then begin // Dll call
      Assert(UsedConnection <> nil);
      Assert(UsedConnection.FIConnection <> nil);
      FIRecordSet.SetConnection(UsedConnection.FIConnection);
    end;
end;

function TCustomDADataSet.IsTransactionStored: boolean;
begin
  Result := FTransaction <> nil;
end;

function TCustomDADataSet.GetUsedTransaction: TDATransaction;
var
  UsedCon: TCustomDAConnection;
begin
  UsedCon := UsedConnection;
  if UsedCon <> nil then begin
    if UsedCon.IsMultipleTransactionsSupported then
      Result := Transaction
    else
      Result := nil;

    if Result = nil then
      Result := UsedCon.UsedTransaction;
  end
  else
    Result := nil;
end;

function TCustomDADataSet.UsedTransaction: TDATransaction;
begin
  Result := GetUsedTransaction;
end;

function TCustomDADataSet.UsedUpdateTransaction: TDATransaction;
var
  UsedCon: TCustomDAConnection;
begin
  UsedCon := UsedConnection;
  if UsedCon <> nil then begin
    if (FUpdateTransaction <> nil) and UsedCon.IsMultipleTransactionsSupported then
      Result := FUpdateTransaction
    else
      Result := GetUsedTransaction;
  end
  else
    Result := nil;
end;

procedure TCustomDADataSet.Notification(AComponent: TComponent; Operation: TOperation);
begin
  inherited;

  if (Operation = opRemove) then
    if (AComponent = FTransaction) then
      FTransaction := nil
    else
    if (AComponent = FUpdateTransaction) then
      FUpdateTransaction := nil;
end;

procedure TCustomDADataSet.SetKeyFields(const Value: _string);
begin
  if FKeyFields <> Value then begin
    FKeyFields := Value;
    if FDataSetService <> nil then
      FDataSetService.ClearCachedKeyFieldDescs;
  end;
end;

function TCustomDADataSet.GetDataTypesMap: TDataTypesMapClass;
begin
  Result := FCommand.GetDataTypesMap;
end;

{ TablesInfo }

function TCustomDADataSet.GetTablesInfo: TCRTablesInfo;
begin
  if FIRecordSet <> nil then
    Result := FIRecordSet.TablesInfo
  else
    Result := nil;
end;

function TCustomDADataSet.GetSQLInfo: TSQLInfo;
begin
  if FICommand <> nil then
    Result := FICommand.SQLInfo
  else
    Result := DefaultSQLInfo;
end;

procedure TCustomDADataSet.SetUpdatingTable(const Value: _string);
begin
  // Behaviour of UpdatingTable corrected:
  // We does not change value of the property,
  // but raise exception on open with invalid updating table
  if FUpdatingTable <> Value then begin
    FUpdatingTable := Value;

    if Active then
      FDataSetService.InitUpdatingTable;
  end;
end;

{ Open/Close }

procedure TCustomDADataSet.Prepare;
var
  MessageID: cardinal;
  v: variant;
begin
  if not Prepared and not Active then begin
    BeginConnection;

    // Get param values from master dataset to avoid bug with master/detail and
    // Execute method on detail dataset.
    if IsMasterDatasetActive and not FOptions.LocalMasterDetail then
      SetMasterParams(Params);

    if not FLockDebug and (TDASQLMonitorClass(UsedConnection.SQLMonitorClass).HasMonitor or Debug) then
      TDASQLMonitorClass(UsedConnection.SQLMonitorClass).SQLPrepare(Self, FinalSQL, FParams, MessageID, True);

    StartWait;

    FCommand.WriteParams(FDataSetService.NeedParamValuesOnPrepare);

    inherited;

    FICommand.GetProp(prUseDescribeParams, v);
    if Boolean(v) then
      FCommand.UpdateParams;

    if not FLockDebug and (TDASQLMonitorClass(UsedConnection.SQLMonitorClass).HasMonitor or Debug) then
      TDASQLMonitorClass(UsedConnection.SQLMonitorClass).SQLPrepare(Self, FinalSQL, FParams, MessageID, False);
  end;
end;

procedure TCustomDADataSet.UnPrepare;
var
  NeedDisconnect: boolean;
  MessageID: cardinal;
  UnpreparePending: boolean;
begin
  NeedDisconnect := Prepared;
  UnpreparePending := False;

  if Prepared and (UsedConnection <> nil) then
    if not FLockDebug and (TDASQLMonitorClass(UsedConnection.SQLMonitorClass).HasMonitor or Debug) then begin
      TDASQLMonitorClass(UsedConnection.SQLMonitorClass).SQLUnprepare(Self, FinalSQL, FParams, MessageID, True);
      UnpreparePending := True;
    end;

  try
    inherited;
  finally
    if NeedDisconnect then
      EndConnection;
  end;
  if FIRecordSet <> nil then
    FIRecordSet.TablesInfo.Clear;
  //FUpdatingTableInfoIdx := -1;

  if UnpreparePending and (UsedConnection <> nil) then
    if not FLockDebug and (TDASQLMonitorClass(UsedConnection.SQLMonitorClass).HasMonitor or Debug) then
      TDASQLMonitorClass(UsedConnection.SQLMonitorClass).SQLUnprepare(Self, FinalSQL, FParams, MessageID, False);
end;

procedure TCustomDADataSet.SetActive(Value: Boolean);
begin
  if not FStreamedOpen or (csDesigning in ComponentState) or (UsedConnection = nil) or
    UsedConnection.Options.KeepDesignConnected
  then
    inherited;
end;

procedure TCustomDADataSet.BeforeOpenCursor(InfoQuery: boolean);
begin
end;

procedure TCustomDADataSet.AfterOpenCursor(InfoQuery: boolean);
begin
end;

procedure TCustomDADataSet.OpenCursor(InfoQuery: boolean);
var
  ReOpen: boolean;
  v: variant;
begin
  if UsedConnection <> nil then
    UsedConnection.PushOperation(clOpen, UsedConnection.IsFailOverAllowed);
  try
  {$IFDEF WITH_IPROVIDER}
    FOldKeyFields := '';
  {$ENDIF}

    BeginConnection;
    try
      if Active then
        Exit;       // for open OnChangeConnect
      repeat
        ReOpen := False;

        BeforeOpenCursor(InfoQuery);
        // get param values from master dataset
        if not FOptions.LocalMasterDetail then
          if IsMasterDatasetActive then
            SetMasterParams(Params)
          else
            if IsConnectedToMaster then
              InitMasterParams;

        if (not FetchAll or FNonBlocking) and FOptions.QueryRecCount then
          FRecordCount := FDataSetService.GetRecCount
        else
          FRecordCount := 0;

        if FNonBlocking then begin
          if not InfoQuery then begin
            SetCursor(crSQLArrow);
            DisableControls;
          end;
        end
        else
          StartWait;

        DoBeforeExecute;

        try
          FCommand.WriteParams;
          FCommand.CheckSQL;
          inherited;
        except
          on E: TObject do begin
            if FNonBlocking then begin
              EnableControls;
              StopWait;
            end;
            if E is EFailOver then begin
              UsedConnection.RestoreAfterFailOver;
              Reopen := True
            end
            else begin
              raise;
            end;
          end;
        end;
        FICommand.GetProp(prIsSelectParams, v);
        if v then begin
          // This is select of out parameters' values.
          // Set RowsAffected to a value received on execution of stored proc.
          FICommand.GetProp(prRowsProcessed, v);
          FRowsAffected := v;
        end
        else
          FRowsAffected := -1;

        AfterOpenCursor(InfoQuery);
      until (not ReOpen);
    finally
      if InfoQuery then
        EndConnection;
    end;
  finally
    if UsedConnection <> nil then
      UsedConnection.PopOperation;
  end;
end;

procedure TCustomDADataset.CloseCursor;
var
  NeedDisconnect: boolean;
begin
  NeedDisconnect := (FIRecordSet <> nil) and
    (FICommand.GetCursorState <> csInactive) and (not FIRecordSet.CanDisconnect); // if command is active and we doesn't
                                                                                  //already substract ConnectCount after all data fetch
  inherited;

  // FUpdateQuery may be prepared for optimization purposes
  if FDataSetService <> nil then
    FDataSetService.FUpdater.UnprepareUpdateObjects;

  if not Prepared or ((FIRecordSet <> nil) and FIRecordSet.FieldListDependsOnParams) then
    FieldDefs.Updated := False;

  if NeedDisconnect then //If there is opened cursor then we should disconnect
    EndConnection;
end;

procedure TCustomDADataset.InternalExecute;
begin
  FIRecordSet.ExecCommand;

  //if FIRecordSet.CommandType = ctCursor then
  //  InternalInitFieldDefs;   // TODO: does not work with AutoPrepare
end;

function TCustomDADataSet.DoOpenNext: boolean;
begin
  CheckDataSetService;
  Result := FDataSetService.OpenNext;
end;

procedure TCustomDADataSet.QuickOpen(var Info: TQuickOpenInfo; Refresh: boolean = False);
begin
  Info.OldActive := Active;
  Info.OldDebug := Debug;
  Info.OldFetchAll := FetchAll;
  Info.OldFetchRows := FetchRows;

  if not Active or Refresh then begin
    Debug := False;
    FetchAll := False;
    Close;
    FetchRows := 1;
    try
      Execute;
    except
      Restore(Info);
    end;
  end;
end;

procedure TCustomDADataSet.Restore(const Info: TQuickOpenInfo; RestoreActive: boolean = True);
begin
  Debug := Info.OldDebug;
  FetchAll := Info.OldFetchAll;
  if RestoreActive then begin
    if FetchRows <> Info.OldFetchRows then begin
      Close;
      FetchRows := Info.OldFetchRows;
    end;
    Active := Info.OldActive;
  end;
end;

procedure TCustomDADataSet.Execute;
var
  ReExecute: boolean;
  MessageID: cardinal;
  OldLockDebug: boolean;
begin
  if UsedConnection <> nil then
    UsedConnection.PushOperation(clExecute);
  try
    if Executing then
      Exit;
    BeginConnection;
    if Active then
       Close;
    repeat
      ReExecute := False;

      if not FNonBlocking then
        StartWait;
      try
        if Options.AutoPrepare then
          Prepare;
        if IsQuery then begin
          Open;
          EndConnection; //Here we decrement UsedConection.FConnectCount that was incremented in InternalExecute and then
                         //in OpenCursor, also we make disconection in case of all data fetched during Opening (Less or equal to one fetch block)
        end
        else begin
          DoBeforeExecute;

          // get param values from master dataset
          if not FOptions.LocalMasterDetail then
            if IsMasterDatasetActive then
              SetMasterParams(Params)
            else
              if IsConnectedToMaster then
                InitMasterParams;

          if FNonBlocking then
            SetCursor(crSQLArrow);
          FCommand.WriteParams;
          FCommand.CheckSQL;

          if not FLockDebug and (TDASQLMonitorClass(UsedConnection.SQLMonitorClass).HasMonitor or Debug) then
            TDASQLMonitorClass(UsedConnection.SQLMonitorClass).SQLExecute(Self, FinalSQL, FParams, '', MessageID, True);

          OldLockDebug := FLockDebug;
          try
            FLockDebug := True;

            InternalExecute;
            if IsQuery then begin
              Open;
              EndConnection; //Here we decrement UsedConection.FConnectCount that was incremented in InternalExecute and then
                             //in OpenCursor, also we make disconection in case of all data fetched during Opening (Less or equal to one fetch block)
            end;
          finally
            FLockDebug := OldLockDebug;
          end;

          if not FLockDebug and (TDASQLMonitorClass(UsedConnection.SQLMonitorClass).HasMonitor or Debug) then
            TDASQLMonitorClass(UsedConnection.SQLMonitorClass).SQLExecute(Self, FinalSQL, FParams, '', MessageID, False);
        end;
      except
        on E: TObject do begin
          if FNonBlocking then begin
            StopWait;
          end;
          if (E is EFailOver) and (EFailOver(E).FConnLostCause = clExecute) then begin
            UsedConnection.RestoreAfterFailOver; //Restore all read transactions
            ReExecute := True; //We should pass clConnectionApplyUpdates FailOver
          end
          else begin
            raise;
          end;
        end;
      end;
    until (not ReExecute);
  finally
    if UsedConnection <> nil then
      UsedConnection.PopOperation;
  end;
end;

procedure TCustomDADataSet.ExecSQL;
begin
  Execute;
end;

procedure TCustomDADataSet.DoBeforeExecute;
begin
  //This routine should be used for actions that performed before execute and
  //affected by local failover feature (like PrepareSQL in stored proc)

  if Assigned(FBeforeExecute) then
    FBeforeExecute(self);
end;

procedure TCustomDADataSet.DoAfterExecute(Result: boolean);
var
  Value: variant;
  AutoCommitUsed: boolean;
begin
  if Result then begin
    FCommand.ReadParams;

    FICommand.GetProp(prRowsProcessed, Value);
    FRowsAffected := Value;

    if not IsQuery then begin
      UsedConnection.FIConnection.GetProp(prLastInsertId, Value);
    {$IFNDEF VER6P}
      if VarType(Value) = $E then
        FLastInsertId := PInt64(@TVarData(Value).VInteger)^
      else
    {$ELSE}
      FLastInsertId := Value;
    {$ENDIF}
    end;
  end;

  if FNonBlocking then
    StopWait;

  if not IsQuery then begin //Leave connection alive in case of SELECT .Execute instead of .Open to perform Fetch
    AutoCommitUsed := UsedConnection.AutoCommit and AutoCommit;

    if UsedConnection.Options.DisconnectedMode and UsedConnection.Connected then
      UsedConnection.DetectInTransaction(not AutoCommitUsed);

    if UsedConnection.IsMultipleTransactionsSupported then
      UsedTransaction.AutoCommitTransaction(AutoCommitUsed);
  end;

  if not Result or not IsQuery then
    EndConnection;    //we should read all Out parameters before disconnect
                      //In NonBlocking Mode this event must be called exactly after server execute
  if Assigned(FAfterExecute) then
    FAfterExecute(Self, Result);
end;

procedure TCustomDADataSet.DoAfterExecFetch(Result: boolean);
begin
  if Result then
    FCommand.ReadParams;

  if FNonBlocking then begin
    if Result then begin
      if State <> dsInactive then
        Resync([])
    end
    else
      Close;

    if not(FetchAll and Result) then
      StopWait;
    EnableControls;
  end;

  if not Result then begin
    EndConnection;
    if not IsQuery and UsedConnection.IsMultipleTransactionsSupported then
      UsedTransaction.AutoCommitTransaction(UsedConnection.AutoCommit and AutoCommit);
  end;

  if Assigned(FAfterExecute) then
    FAfterExecute(Self, Result);
end;

procedure TCustomDADataSet.DoAfterFetchAll(Result: boolean);
begin
  if FNonBlocking then begin
    StopWait;
    if Trim(IndexFieldNames) <> '' then
      Resync([]);
  end;
end;

procedure TCustomDADataSet.DoAfterScroll;
begin
  if FFetchCanceled then begin
    Resync([]);
    FFetchCanceled := False;
  end;

  inherited;
end;

procedure TCustomDADataSet.DoOnBeforeFetch(out Cancel: boolean);
begin
  if not FNonBlocking then
    StartWait;

  if Assigned(FBeforeFetch) then
    FBeforeFetch(Self, Cancel);
  FFetchCanceled := Cancel;

end;

procedure TCustomDADataSet.DoOnAfterFetch;
begin
  if not FetchAll or (FICommand.GetCursorState = csFetched) then
    if Assigned(FAfterFetch) then
      FAfterFetch(Self);

  if FIRecordSet.CanDisconnect then
    EndConnection; //Close connection after all data was fetched.
end;

procedure TCustomDADataSet.DoOnDataChanged;
begin
  if ResyncBeforeFetch then
    Resync([]);
end;

function TCustomDADataSet.Executing: boolean;
var
  Value: variant;
begin
  if FDataSetService <> nil then
    Result := FDataSetService.Executing
  else
    Result := False;

  if not Result then
    if FICommand <> nil then begin
      FICommand.GetProp(prExecuting, Value);
      Result := Value;
    end;
end;

function TCustomDADataSet.Fetching: boolean;
begin
  if FICommand <> nil then
    Result := FICommand.GetCursorState in [csFetching, csFetchingAll]
  else
    Result := False;
end;

function TCustomDADataSet.FetchingAll: boolean;
begin
  if FICommand <> nil then
    Result := FICommand.GetCursorState = csFetchingAll
  else
    Result := False;
end;

function TCustomDADataSet.Fetched: boolean;
begin
  if FICommand <> nil then
    Result := (FICommand.GetCursorState >= csFetched) or
      Active and (FICommand.GetCursorState = csInactive)
  else
    Result := False;
end;

procedure TCustomDADataSet.BreakExec;
begin
  if FDataSetService <> nil then
    FDataSetService.BreakExec;
    
  if FIRecordSet <> nil then begin
    if FICommand <> nil then
      FICommand.BreakExec;
    FIRecordSet.BreakFetch;
  end;
end;

procedure TCustomDADataSet.DoOnFieldsChanged;
begin
  if FDataSetService <> nil then
    FDataSetService.ClearFieldDescRefs;
end;

{ Before / After UpdateExecute }

function TCustomDADataSet.AssignedBeforeUpdateExecute: boolean;
begin
  Result := Assigned(FBeforeUpdateExecute);
end;

procedure TCustomDADataSet.DoBeforeUpdateExecute(Sender: TDataSet; StatementTypes: TStatementTypes;
  Params: TDAParams);
begin
  if AssignedBeforeUpdateExecute then
    FBeforeUpdateExecute(Sender, StatementTypes, Params);
end;

function TCustomDADataSet.AssignedAfterUpdateExecute: boolean;
begin
  Result := Assigned(FAfterUpdateExecute);
end;

procedure TCustomDADataSet.DoAfterUpdateExecute(Sender: TDataSet; StatementTypes: TStatementTypes;
  Params: TDAParams);
begin
  if AssignedAfterUpdateExecute then
    FAfterUpdateExecute(Sender, StatementTypes, Params);
end;

procedure TCustomDADataSet.GetCurrentKeys(out KeyFields: TFieldArray; out Values: variant);
var
  KeyFieldDescs, DataFieldDescs: TFieldDescArray;
  KeyFieldsCount, DataFieldsCount: integer;
  i, j: integer;
  RecBuf: TRecordBuffer;
  TmpVar: variant;
  Delta: integer;
  EmptyRecBuf: boolean;
begin
  DataFieldDescs := nil;

  Assert(FDataSetService <> nil);
  FDataSetService.GetKeyFieldDescs(KeyFieldDescs);
  KeyFieldsCount := Length(KeyFieldDescs);
  SetLength(KeyFields, KeyFieldsCount);
  Values := Unassigned;

  EmptyRecBuf := not GetActiveRecBuf(RecBuf);

  if KeyFieldsCount = 1 then begin
    KeyFields[0] := GetField(KeyFieldDescs[0]);
    if not EmptyRecBuf then
      Data.GetFieldAsVariant(KeyFieldDescs[0].FieldNo, RecBuf, Values);
  end
  else
  if KeyFieldsCount > 1 then begin
    Values := VarArrayCreate([0, KeyFieldsCount - 1], varVariant);
    for i := 0 to KeyFieldsCount - 1 do begin
      KeyFields[i] := GetField(KeyFieldDescs[i]);
      Values[i] := Unassigned;
      if not EmptyRecBuf then begin
        Data.GetFieldAsVariant(KeyFieldDescs[i].FieldNo, RecBuf, TmpVar);
        Values[i] := TmpVar;
      end;
    end;
  end
  else begin
    FDataSetService.GetDataFieldDescs(DataFieldDescs);
    DataFieldsCount := Length(DataFieldDescs);
    if DataFieldsCount > 0 then begin
      Delta := 0;
      Assert(FDataSetService.FSQLGenerator <> nil);
      for i := DataFieldsCount - 1 downto 0 do
        if FDataSetService.FSQLGenerator.IsBlobDataType(DataFieldDescs[i].DataType) or
          FDataSetService.FSQLGenerator.IsObjectDataType(DataFieldDescs[i].DataType) //TODO:
        then begin
          Inc(Delta);
          for j := i to DataFieldsCount - Delta - 1 do
            DataFieldDescs[j] := DataFieldDescs[j + 1];
        end;
      SetLength(KeyFields, DataFieldsCount - Delta);
      Values := VarArrayCreate([0, DataFieldsCount - Delta - 1], varVariant);
      for i := 0 to DataFieldsCount - Delta - 1 do begin
        KeyFields[i] := GetField(DataFieldDescs[i]);
      {$IFNDEF CLR}
        Values[i] := Unassigned;
      {$ENDIF}
        if not EmptyRecBuf then begin
          Data.GetFieldAsVariant(DataFieldDescs[i].FieldNo, RecBuf, TmpVar);
          Values[i] := TmpVar;
        end;
      end;
    end;
  end;
end;

procedure TCustomDADataSet.DataReopen;
var
  FullReopen: boolean;
  OldRecordSize: longint;
begin
  FullReopen := Data.IsFullReopen;
  OldRecordSize := RecordSize;

  try
    Data.Reopen;//  RecordSize can be changed here
  except
    on E: Exception do begin
      if not (E is EFailOver) then
        Close;
      raise;
    end;
  end;

  if RecordSize <> OldRecordSize then begin  /// CR-D24236
    Close;
    Open;
  end
  else
    // On full reopen FieldDescs are recreated
    if FullReopen then
      FDataSetService.PreInitCursor;
end;

procedure TCustomDADataSet.InternalRefresh;
var
  MessageID: cardinal;
  KeyFields: TFieldArray;
  Values: variant;
  KeyFieldsReaded: boolean;
  Retry: boolean;
begin
  if UsedConnection <> nil then
    UsedConnection.PushOperation(clRefresh, UsedConnection.IsFailOverAllowed);
  try
    KeyFieldsReaded := False;
    repeat
      BeginConnection;
      Retry := False;
      try
        if not KeyFieldsReaded then
          GetCurrentKeys(KeyFields, Values);
        KeyFieldsReaded := True; //this will allow us to restore active record after failover
        if (not FetchAll or FNonBlocking) and FOptions.QueryRecCount then
          FRecordCount := FDataSetService.GetRecCount
        else
          FRecordCount := 0;

        if not FLockDebug and (TDASQLMonitorClass(UsedConnection.SQLMonitorClass).HasMonitor or Debug) then
          TDASQLMonitorClass(UsedConnection.SQLMonitorClass).SQLExecute(Self, FinalSQL, FParams, 'Refresh', MessageID, True);

        StartWait;
        DoBeforeExecute;
        FCommand.WriteParams;

        inherited;

        if not FLockDebug and (TDASQLMonitorClass(UsedConnection.SQLMonitorClass).HasMonitor or Debug) then
          TDASQLMonitorClass(UsedConnection.SQLMonitorClass).SQLExecute(Self, FinalSQL, FParams, 'Refresh', MessageID, False);

        FRowsAffected := -1;

        if Length(KeyFields) = 0 then // CR N 11512
          First
        else begin
          DoBeforeScroll;
          if not LocateRecord(KeyFields, Values, [], False) then
            First
          else begin
            Resync([]);
            DoAfterScroll;
          end;
        end;

      except
        on E: Exception do begin
{         if FNonBlocking then begin
            EnableControls;
            StopWait;
          end;}
          if E is EFailOver then begin
            UsedConnection.RestoreAfterFailOver;
            Retry := True
          end
          else
            raise;
        end;
      end;

    until not Retry;
  finally
    if UsedConnection <> nil then
      UsedConnection.PopOperation;
  end;
end;

procedure TCustomDADataSet.InternalRefreshQuick(const CheckDeleted: boolean);
begin
  CheckActive;
  FDataSetService.FUpdater.PerformRefreshQuick(CheckDeleted);
end;

procedure TCustomDADataSet.InternalClose;
begin
  try
    try
      inherited;
    except
      on E: EDAError do  // Borland's bug in DoInternalClose with FBufferCount
        if not E.IsFatalError then
          raise;
      else
        raise;
    end;
  finally
    // In case that User doesn't call Prepare directly
    if (FIRecordSet <> nil) and not Prepared then
      FIRecordSet.TablesInfo.Clear;
    if FDataSetService <> nil then
      FDataSetService.CloseCursor;
  end;
end;

procedure TCustomDADataSet.DoAfterOpen;
begin
  inherited;

{$IFDEF WITH_IPROVIDER}
  FOldTableName := PSGetTableName;
{$ENDIF}
end;

procedure TCustomDADataSet.SetRefreshOptions(Value: TRefreshOptions);
begin
  FRefreshOptions := Value;

  if FIRecordSet <> nil then
    FIRecordSet.SetProp(prRoAfterUpdate, roAfterUpdate in RefreshOptions);
end;

{ Edit }

procedure TCustomDADataSet.SetReadOnly(Value: boolean); 
begin
  if FReadOnly <> Value then begin
    if FIRecordSet <> nil then
      FIRecordSet.SetProp(prReadOnly, Value);

    FReadOnly := Value;
  end;
end;

procedure TCustomDADataSet.InternalEdit;
begin
  if FDataSetService.FUpdater.IsNeedEditPreconnect then   //TODO:
    BeginConnection;
  try
    FDataSetService.FUpdater.PrepareUpdate;

    inherited;
  except
    if FDataSetService.FUpdater.IsNeedEditPreconnect then
      EndConnection;
    raise;
  end;
end;

procedure TCustomDADataSet.InternalDelete;
begin
  FDataSetService.FUpdater.PrepareDelete;
  
  inherited;
end;

procedure TCustomDADataSet.InternalInsert;
begin
  if FDataSetService.FUpdater.IsNeedInsertPreconnect then   //TODO:
    BeginConnection;
  try
    FDataSetService.FUpdater.PrepareAppend;

    inherited;
  except
    if FDataSetService.FUpdater.IsNeedInsertPreconnect then
      EndConnection;
  end;
end;

procedure TCustomDADataSet.InternalCancel;
begin
  try
    try
      FDataSetService.FUpdater.UnPrepareAppendUpdateDelete;  //TODO:
    finally
      inherited;
    end;
  finally
    if FDataSetService.FUpdater.IsPreconnected then
      EndConnection;
  end;
end;

procedure TCustomDADataSet.InternalPost;
var
  DataSet: TDataSet;
  MasterField, DetailField: TField;
  MasterName, DetailName: _string;
  MasterPos, DetailPos: integer;
  RemoveRecord: Boolean;
begin
  try
    inherited;

    if IsConnectedToMaster and not FOptions.LocalMasterDetail then begin
      DataSet := DataSource.DataSet;
      if (DataSet <> nil) and (DataSet.Active) then begin
        Assert(FIRecordSet <> nil);
        MasterPos := 1;
        DetailPos := 1;
        while True do begin
          MasterName := ExtractFieldName(FMasterFields, MasterPos);
          DetailName := ExtractFieldName(FDetailFields, DetailPos);
          if (MasterName <> '') and (DetailName <> '') then begin
            MasterField := DataSet.FindField(MasterName);
            if Assigned(MasterField) then begin
              DetailField := FindField(DetailName);
              if Assigned(DetailField) then begin // Fixed bug with case insensitive master/detail
              {$IFNDEF CLR}
                if (not FIRecordSet.IsCaseSensitive) and
                  (VarType(DetailField.AsVariant) = varString) and
                  (VarType(MasterField.AsVariant) = varString) then
                    RemoveRecord := AnsiCompareTextS(AnsiString(DetailField.AsVariant), AnsiString(MasterField.AsVariant)) <> 0
                else
              {$ENDIF}
                  if (not FIRecordSet.IsCaseSensitive) and
                    ((VarType(DetailField.AsVariant) = {$IFDEF CLR} varChar {$ELSE} varOleStr {$ENDIF})
                    {$IFDEF VER12P} or (VarType(DetailField.AsVariant) = varUString){$ENDIF}) and
                    ((VarType(MasterField.AsVariant) = {$IFDEF CLR} varChar {$ELSE} varOleStr {$ENDIF})
                    {$IFDEF VER12P} or (VarType(MasterField.AsVariant) = varUString){$ENDIF})
                    then
                      RemoveRecord := AnsiStrICompWS(DetailField.AsVariant, MasterField.AsVariant) <> 0
                  else
                    RemoveRecord := not VarEqual(DetailField.AsVariant, MasterField.AsVariant);
                if RemoveRecord then begin
                  Assert(not CachedUpdates, 'Can not use Master/Detail with CachedUpdates');
                  FIRecordSet.RemoveRecord;
                  Exit;
                end;
              end;
            end;
          end
          else
            break;
        end;
      end;
    end;
  finally
    if FDataSetService.FUpdater.IsPreconnected then
      EndConnection;
  end;
end;

procedure TCustomDADataSet.InternalDeferredPost;
begin
  inherited;
end;

procedure TCustomDADataSet.SetUpdateSQLIndex(Index: integer; Value: _TStrings);
begin
  with FUpdateSQL[TStatementType(Index)] do begin
    BeginUpdate;
    try
      Assign(Value);
    finally
      EndUpdate;
    end;
  end;
end;

function TCustomDADataSet.GetUpdateSQLStatementTypes: TStatementTypes;
begin
  Result := [stInsert, stDelete, stUpdate, stRefresh, stLock];
end;

function TCustomDADataSet.GetUpdateSQLIndex(Index: integer): _TStrings;
begin
  Result := FUpdateSQL[TStatementType(Index)];
end;

procedure TCustomDADataSet.Resync(Mode: TResyncMode);
begin
  // this need if Resync called for closed dataset (AV BUG !!!)
  if Active then
    inherited;
end;

procedure TCustomDADataSet.GetDetailLinkFields(MasterFields, DetailFields: {$IFDEF CLR}TObjectList{$ELSE}TList{$ENDIF});
var
  i: integer;
begin
  if (DataSource <> nil) and (DataSource.DataSet <> nil) and (Params.Count > 0) then begin
    MasterFields.Clear;
    DetailFields.Clear;
    for i := 0 to Params.Count - 1 do
      if AddFieldToList(Params[i].Name, DataSource.DataSet, MasterFields) then
        AddFieldToList(Params[i].Name, Self, DetailFields)
  end
  else
    inherited;
end;

function TCustomDADataSet.FindKey(const KeyValues: array of const): Boolean;
  function GetKeyValue(Index: integer): variant; // nearly copied from TField.AssignValue
    procedure Error;
    begin
      DatabaseErrorFmt(SFieldValueError, [IntToStr(Index)]);
    end;
  {$IFDEF CLR}
  var
    Value: Variant;
  {$ENDIF}
  begin
    Result := Unassigned;
  {$IFDEF CLR}
    Value := Variant(KeyValues[Index]); 
    case VarType(Value) of
      varInteger, varSmallInt, varShortInt, varByte, varWord, varLongWord, varInt64, varUInt64:
        Result := Integer(Value);
      varBoolean:
        Result := Boolean(Value);
      varChar, varString:
        Result := String(Value);
      varDouble, varSingle:
        Result := Double(Value);
      varObject:
        Result := Value;
      varEmpty, varNull:
        {Clear};
      varDateTime, varDate:
        Result := TDateTime(Value);
      {varDecimal:
        AsBcd := Value;}
      varCurrency:
        Result := Currency(Value);
      else
        Error;
    end;
  {$ELSE}
    with KeyValues[Index] do
      case VType of
        vtInteger:
          Result := Integer(VInteger);
        vtInt64: begin /// CR-D12558
        {$IFDEF VER6P}
          Result := VInt64^;
        {$ELSE}
          TVarData(Result).VType := varDecimal;
          TVarDataD6(Result).VInt64 := VInt64^;
        {$ENDIF}
        end;
        vtBoolean:
          Result := Boolean(VBoolean);
        vtChar:
          Result := String(VChar);
        vtExtended:
          Result := Extended(VExtended^);
        vtString:
          Result := String(VString^);
        vtPointer, vtObject:
          if VPointer <> nil then Error;
        vtPChar:
          Result := String(VPChar);
        vtAnsiString:
          Result := String(VAnsiString);
        vtCurrency:
          Result := Currency(VCurrency^);
        vtVariant:
          if not {$IFDEF VER6P}VarIsClear{$ELSE}VarIsEmpty{$ENDIF}(VVariant^) then
            Result := Variant(VVariant^);
        vtWideString:
          Result := WideString(VWideString);
      {$IFDEF VER12P}
        vtUnicodeString:
          Result := UnicodeString(VUnicodeString);
      {$ENDIF}
      else
        Error;
      end;
  {$ENDIF}
  end;

var
  KeyFieldDescs: TFieldDescArray;
  Values: variant;
  KeyFieldsCount: integer;
  KeyFields: TFieldArray;
  i: integer;
begin
  CheckBrowseMode;
  FDataSetService.GetKeyFieldDescs(KeyFieldDescs);

  Values := Unassigned; // To prevent warning
  KeyFieldsCount := Length(KeyFieldDescs);
  SetLength(KeyFields, KeyFieldsCount);
  case KeyFieldsCount of
    0:
      DatabaseError(SKeyFieldsRequired);
    1: begin
      KeyFields[0] := GetField(KeyFieldDescs[0]);
      Values := GetKeyValue(0);
    end;
    else
    begin
      Values := VarArrayCreate([0, KeyFieldsCount - 1], varVariant);
      for i := 0 to KeyFieldsCount - 1 do begin
          KeyFields[i] := GetField(KeyFieldDescs[i]);

        if i <= High(KeyValues) then
          Values[i] := GetKeyValue(i)
        else
          Values[i] := Unassigned;
      end;
    end;
  end;

  Result := LocateEx(KeyFields, Values, FFindKeyOptions);
end;

procedure TCustomDADataSet.FindNearest(const KeyValues: array of const);
begin
  FFindKeyOptions := [lxNearest];
  try
    FindKey(KeyValues);
  finally
    FFindKeyOptions := [];
  end;
end;

procedure TCustomDADataSet.GotoCurrent(DataSet: TCustomDADataSet);
var
  KeyFields, OwnKeyFields: TFieldArray;
  Values: variant;
  i: integer;
begin
  DataSet.GetCurrentKeys(KeyFields, Values);
  SetLength(OwnKeyFields, Length(KeyFields));
  for i := 0 to Length(KeyFields) - 1 do
    OwnKeyFields[i] := FieldByName(KeyFields[i].FieldName);

  if (Length(KeyFields) = 0) or not Locate(OwnKeyFields, Values, []) then
    First;
end;

procedure TCustomDADataSet.ApplyUpdates(const UpdateRecKinds: TUpdateRecKinds);
var
  ReApply, AllowFailover: boolean;
  OldAutoCommit: boolean;
  ImplicitStart: boolean;
  ApplyTransaction: TDATransaction;
begin
  CheckActive;
  if UsedConnection = nil then
    DatabaseError(SConnectionNotDefined);

  ApplyTransaction := UsedUpdateTransaction;
  AllowFailover := UsedConnection.IsFailOverAllowed and
    (not AutoCommit or UsedConnection.IsMultipleTransactionsSupported);
  UsedConnection.PushOperation(clApply, AllowFailover); //Add ApplyUpdates Operation to Operations Stack (FailOver)
  try
    repeat
      ReApply := False;
      try
        OldAutoCommit := AutoCommit;
        if UsedConnection.IsMultipleTransactionsSupported or
          (FConnection.Options.DisconnectedMode and (AutoCommit = False))
        then begin
          ImplicitStart := not ApplyTransaction.Active;
          if ImplicitStart then
            ApplyTransaction.StartTransaction;
          AutoCommit := False;  //This will turn ApplyUpdates to single Transaction operation
        end
        else
          ImplicitStart := False;

        // if transaction has been started in the Edit or Delete operation when LockMode = lmPessimistic
        if not ImplicitStart then
          ImplicitStart := FDataSetService.Updater.FLockTrStarted = ltsOnLockCachedUpdates;

        try
          inherited ApplyUpdates(UpdateRecKinds);
        finally
          AutoCommit := OldAutoCommit;
        end;

        if UsedConnection.IsMultipleTransactionsSupported then begin
          if AutoCommit then
            if ImplicitStart then
              ApplyTransaction.Commit
            else
              ApplyTransaction.DoCommitRetaining;
        end;
      except
        on E: EFailOver do
          if E.FConnLostCause = clApply then begin
            UsedConnection.RestoreAfterFailOver;
            ReApply := True;
          end
          else
            raise;
      end;
    until (not ReApply);
  finally
    UsedConnection.PopOperation; //Remove ApplyUpdates Operation from Operations Stack
  end;
end;

procedure TCustomDADataSet.RefreshRecord;
begin
  CheckActive;

  case State of
    dsInsert:
    begin
      Cancel;
      Exit;
    end;
    dsEdit:
      Cancel;
    dsSetKey:
      Post;
  end;
  
  if not IsEmpty then begin
    UpdateCursorPos;
    FDataSetService.FUpdater.PerformRefreshRecord;
    if FRowsAffected = 0 then begin
      // remove deleted record from dataset
      if Options.RemoveOnRefresh then
        FIRecordSet.RemoveRecord;
      Resync([]);
    end
    else
      if FRowsAffected > 0 then
        Resync([]);
    //DataEvent(deRecordChange, 0);
  end;
end;

procedure TCustomDADataSet.Lock;
begin
  CheckActive;
  FDataSetService.FUpdater.PerformLock;
end;

procedure TCustomDADataSet.UnLock;
begin
  CheckActive;
  FDataSetService.FUpdater.PerformUnLock;
end;

procedure TCustomDADataSet.AssignFieldValue(Param: TDAParam; Field: TField;
  Old: boolean);
var
  FieldDesc: TFieldDesc;
  FieldObject: TSharedObject;
  RollbackBlob, Blob: TBlob;
begin
  if Param.IsObjectDataType(Field.DataType) then begin
    Param.DataType := Field.DataType;
    FieldObject := GetFieldObject(Field);
    if Old and (FieldObject is TBlob) then begin
      Blob := TBlob(FieldObject);
      RollbackBlob := TBlob.Create(Blob.IsUnicode); // RefCount = 1
      Blob.UseRollback := True;
      try
        RollbackBlob.SetData(Blob.GetData);
      finally
        Blob.UseRollback := False;
      end;
      Param.ParamObject := RollbackBlob; // Inc(RefCount); RefCount = 2
      RollbackBlob.Release; // Dec(RefCount); RefCount = 1
    end
    else
      Param.ParamObject := GetFieldObject(Field);
  end
  else
    if Old then
      Param.AssignFieldValue(Field, Field.OldValue)
    else
      Param.AssignFieldValue(Field, Field.NewValue);

  FieldDesc := GetFieldDesc(Field);
  if FieldDesc <> nil then
    AssignFieldType(Param, FieldDesc);
end;

procedure TCustomDADataSet.AssignFieldValue(Param: TDAParam; FieldDesc: TFieldDesc; Old: boolean);
var
  FieldType: TFieldType;
  RecBuf: TRecordBuffer;
  Value: variant;
begin
  FieldType := GetFieldType(FieldDesc.DataType);
  Param.DataType := FieldType;

  if Param.IsObjectDataType(FieldType) then begin
    Param.ParamObject := GetFieldObject(FieldDesc);
  end
  else begin
    if Old then
      RecBuf := GetOldRecBuf
    else
      RecBuf := GetNewRecBuf;

    if RecBuf <> nil then
      Data.GetFieldAsVariant(FieldDesc.FieldNo, RecBuf, Value)
    else
      Value := Null;

    if VarIsNull(Value) then
      Param.Clear
    else
      Param.Value := Value;

    AssignFieldType(Param, FieldDesc);
  end;
end;

procedure TCustomDADataSet.AssignFieldType(Param: TDAParam; FieldDesc: TFieldDesc);
begin
  Param.SubDataType := FieldDesc.SubDataType;
  Param.National := (FieldDesc as TCRFieldDesc).IsNational;
  Param.Size := FieldDesc.Length;
end;

procedure TCustomDADataSet.SetDefaultExpressionValues;
var
  FieldList: TDAList;
  Parser: TSQLParser;

  procedure SelectDbValues(const OperationName, SQL: _string);
  var
    MonitorClass: TDASQLMonitorClass;
    MessageID: cardinal;
    UpdateQuery: TCustomDADataSet;
    i: integer;
  begin
    FDataSetService.FUpdater.CheckUpdateQuery(stCustom);

    UpdateQuery := TCustomDADataSet(FDataSetService.FUpdater.FUpdateQuery);
    UpdateQuery.SQL.Text := SQL;

    BeginConnection;
    try
      MonitorClass := TDASQLMonitorClass(UsedConnection.SQLMonitorClass);
      if not FLockDebug and (MonitorClass.HasMonitor or Debug) then
        MonitorClass.SQLExecute(Self, SQL, UpdateQuery.Params, OperationName, MessageID, True);

      UpdateQuery.Open;

      if not FLockDebug and (MonitorClass.HasMonitor or Debug) then
        MonitorClass.SQLExecute(Self, SQL, UpdateQuery.Params, OperationName, MessageID, False);

      for i := 0 to FieldList.Count - 1 do
        TField(FieldList[i]).Value := UpdateQuery.Fields[i].Value;

      UpdateQuery.Close;
    finally
      EndConnection;
    end;
  end;

  function ExtractSimpleValue(Field: TField; const DefExpr: _string; var Value: _string): boolean;
  var
    Code, BrCount, p, ValCode: integer;
    Lexem: _string;
  begin
    Parser.SetText(DefExpr);
    BrCount := 0;

    repeat
      Code := Parser.GetNext(Lexem);
      if Lexem = '(' then
        Inc(BrCount)
      else
        break;
    until False;

    Result := True;
    Value := '';
    ValCode := lcEnd;

    if Code <> lcEnd then begin
      // default value is simple if there is only one lexem and this lexem is number or string
      if (Code = lcNumber) or (Code = lcString)
        or ((Code = lcIdent) and ((_LowerCase(Lexem) = 'true') or (_LowerCase(Lexem) = 'false')))
      then begin
        // strings are unquoted by parser
        Value := Lexem;
        ValCode := Code;

        if Code <> lcEnd then begin
          repeat
            Code := Parser.GetNext(Lexem);
            if Lexem = ')' then
              Dec(BrCount)
            else begin
              if Code <> lcEnd then
                Result := False;
              break;
            end;
          until False;
        end;
      end
      else
        Result := False;
    end;

    if BrCount <> 0 then
      Result := False;

    if Result and ((ValCode = lcNumber) or (Field is TNumericField)) and
      ({$IFDEF VER16P}FormatSettings.{$ENDIF}DecimalSeparator{$IFDEF CLR}[1]{$ENDIF} <> '.')
    then begin
      p := Pos('.', Value);
      if p > 0 then
        Value[p] := _char({$IFDEF VER16P}FormatSettings.{$ENDIF}DecimalSeparator{$IFDEF CLR}[1]{$ENDIF});
    end;
  end;

{$IFDEF FPC}
  {$DEFINE OWN_DATE_FORMAT}
{$ENDIF}
{$IFNDEF MSWINDOWS}
  {$DEFINE OWN_DATE_FORMAT}
{$ENDIF}
var
  i: integer;
  VarValue: variant;
  DefExpr, StrValue, ExprList, SQL: _string;
  ComplexDefault: boolean;
{$IFDEF OWN_DATE_FORMAT}
  OldShortDateFormat: string;
  OldDateSeparator: char;
{$ENDIF}
begin
{$IFDEF OWN_DATE_FORMAT}
  OldShortDateFormat := {$IFDEF VER16P}FormatSettings.{$ENDIF}ShortDateFormat;
  OldDateSeparator := {$IFDEF VER16P}FormatSettings.{$ENDIF}DateSeparator;
  {$IFDEF VER16P}FormatSettings.{$ENDIF}ShortDateFormat := 'YYYY-MM-DD';
  {$IFDEF VER16P}FormatSettings.{$ENDIF}DateSeparator := '-';
{$ENDIF}

  FInSettingDefaultExpressionValues := True;
  try
    ExprList := '';
    FieldList := TDAList.Create;
    Parser := FICommand.GetParserClass.Create('');
    try
      Parser.OmitBlank := True;
      Parser.OmitComment := True;
      Parser.QuotedString := False; 
      Parser.DecSeparator := '.';

      for i := 0 to FieldCount - 1 do begin
        DefExpr := Fields[i].DefaultExpression;
        if DefExpr <> '' then begin
          ComplexDefault := False;
          if DefaultExpressionOldBehavior then begin
            try
              Fields[i].AsString := DefExpr;
            except
              ComplexDefault := True;
            end;
          end
          else begin
            if ExtractSimpleValue(Fields[i], DefExpr, StrValue) then begin
            {$IFDEF FPC}
              if Fields[i] is TBooleanField then
                Fields[i].Value := StrToBool(StrValue)
              else
            {$ENDIF}
            {$IFDEF OWN_DATE_FORMAT}
              if Fields[i] is TDateTimeField then
                Fields[i].AsString := StrValue
              else
            {$ENDIF}
              if Fields[i] is TLargeintField then
                Fields[i].AsString := StrValue
              else
                Fields[i].Value := StrValue;
            end
            else
              ComplexDefault := True;
          end;

          if ComplexDefault then begin
            if FDataSetService.FUpdater.GetDefaultExpressionValue(DefExpr, VarValue) then
              Fields[i].Value := VarValue
            else begin
              if ExprList <> '' then
                ExprList := ExprList + ', ';
              ExprList := ExprList + DefExpr;
              FieldList.Add(Fields[i]);
            end;
          end;
        end;
      end;

      if ExprList <> '' then begin
        // select default values from Server
        SQL := FDataSetService.FSQLGenerator.GenerateSelectValues(ExprList);
        SelectDbValues('Select default values', SQL);
      end;
    finally
      FieldList.Free;
      Parser.Free;
    end;
  finally
    FInSettingDefaultExpressionValues := False;
  {$IFDEF OWN_DATE_FORMAT}
    {$IFDEF VER16P}FormatSettings.{$ENDIF}ShortDateFormat := OldShortDateFormat;
    {$IFDEF VER16P}FormatSettings.{$ENDIF}DateSeparator := OldDateSeparator;
  {$ENDIF}
  end;
end;

function TCustomDADataSet.GetCanModify: boolean;
begin
  Result := Active and FDataSetService.DetectCanModify;
end;

{$IFNDEF FPC}
procedure TCustomDADataSet.SetStateFieldValue(State: TDataSetState; Field: TField; const Value: Variant); // Need to support int64 fields on PerformSQL in RefreshRecord
var
  SaveState: TDataSetState;
{$IFNDEF VER6P}
  i64: int64;
{$ENDIF}
begin
  if not (Field is TLargeintField) then
    inherited
  else
  begin
    // Nearly copied from inherited
    if Field.FieldKind <> fkData then Exit;
  {$IFNDEF VER6P}
    if not TVarDataD6(Value).VType in [varSmallint, varInteger, varByte, $12{vt_ui2}, $13{vt_ui4}] then begin
      inherited;
      exit;
    end;

    i64 := TVarDataD6(Value).VInt64;
  {$ENDIF}
    
    SaveState := SetTempState(State);
    try
    {$IFDEF VER6P}
      if VarIsNull(Value) then
        Field.Clear
      else
        TLargeintField(Field).AsLargeInt := Value;
    {$ELSE}
      TLargeintField(Field).AsLargeInt := i64;
    {$ENDIF}
    finally
      RestoreState(SaveState);
    end;
  end;
end;
{$ENDIF}

function TCustomDADataSet.CanRefreshField(Field: TField): boolean;
begin
  Result := True;
end;

{ Master/Detail }

function TCustomDADataSet.UseLocalMasterDetailFilter: boolean;
begin
  Result := FOptions.LocalMasterDetail;
end;

procedure TCustomDADataSet.RefreshDetail(Sender: TObject);
var
  MessageID: cardinal;
  Retry: boolean;
begin
  inherited;

  if not FOptions.LocalMasterDetail then begin
  {$IFDEF VER5P}
    DoBeforeRefresh;
  {$ENDIF}

    if UsedConnection <> nil then
      UsedConnection.PushOperation(clRefresh, UsedConnection.IsFailOverAllowed);
    try
      BeginConnection;
      DisableControls;
      try
        repeat
          Retry := False;
          try
            if not FLockDebug and (TDASQLMonitorClass(UsedConnection.SQLMonitorClass).HasMonitor or Debug) then
              TDASQLMonitorClass(UsedConnection.SQLMonitorClass).SQLExecute(Self, FinalSQL, FParams, 'Refresh', MessageID, True);

            StartWait;
            DoBeforeExecute;
            FCommand.WriteParams;

            CheckBrowseMode;
            UpdateCursorPos;
            try
            {$IFDEF VER5P}
              DoBeforeScroll;
            {$ENDIF}
              DataReopen;
              if FIRecordSet.IndexFields.Count > 0 then
                FIRecordSet.SortItems;
            finally
              Resync([]);
            {$IFDEF VER5P}
              DoAfterScroll;
            {$ENDIF}
            end;

          except
            on E: Exception do begin
              if E is EFailOver then begin
                UsedConnection.RestoreAfterFailOver;
                Retry := True
              end
              else
                raise;
            end;
          end;
        until not Retry;

      finally
        EnableControls;
      end;

      if not FLockDebug and (TDASQLMonitorClass(UsedConnection.SQLMonitorClass).HasMonitor or Debug) then
        TDASQLMonitorClass(UsedConnection.SQLMonitorClass).SQLExecute(Self, FinalSQL, FParams, 'Refresh', MessageID, False);

      if (not FetchAll or FNonBlocking) and FOptions.QueryRecCount then
        FRecordCount := FDataSetService.GetRecCount
      else
        FRecordCount := 0;

      FRowsAffected := -1;
    finally
      if UsedConnection <> nil then
        UsedConnection.PopOperation;

    {$IFDEF VER5P}
      DoAfterRefresh;
    {$ENDIF}
    end;
  end;
end;

function TCustomDADataSet.NeedDetailRefresh(Param: TDAParam; FieldValue: TSharedObject): boolean;
begin
  Result := Param.ParamObject <> FieldValue;

  if FieldValue <> nil then
    Param.ParamObject := FieldValue
  else begin
    Param.FreeObject;
    Param.CreateObject;
  end
end;

function TCustomDADataSet.MDLinksRefreshed: boolean;
begin
  Result := SetMasterParams(Params);
end;

function TCustomDADataSet.SetMasterParams(AParams: TDAParams): boolean;
var
  DataSet: TDataSet;
  Field: TField;
  i: integer;
  SharedObject: TSharedObject;
begin
  Result := False;

  if FOptions.LocalMasterDetail then
    Result := SetLocalMDLinks
  else
  if DataSource <> nil then begin
    DataSet := DataSource.DataSet;
    if (DataSet <> nil) and DataSet.Active then begin
      for i := 0 to AParams.Count - 1 do begin
        Field := DataSet.FindField(AParams[i].Name);

        if not Assigned(Field) then
          Continue;

        if AParams[i].IsObjectDataType(Field.DataType) then begin
          SharedObject := nil;
          if (DataSet is TCustomDADataset) then
            SharedObject := TCustomDADataset(DataSet).GetFieldObject(Field);

          AParams[i].DataType := Field.DataType;
          Result := NeedDetailRefresh(AParams[i], SharedObject) or Result;
        end
        else
          if not Active or
            ((VarIsEmpty(AParams[i].Value) or not VarEqual(AParams[i].Value, Field.Value)) and
            (not (VarIsEmpty(AParams[i].Value) and Field.IsNull)))
          then begin
            AParams[i].AssignField(Field);
            Result := True;
          end;
        AParams[i].ParamType := ptInput;
      end;
    end;
  end;
end;

procedure TCustomDADataSet.InitMasterParams;
begin
  FDataSetService.InitMasterParams(Params);
end;

procedure TCustomDADataSet.MDPropertiesChanged;
var
  OldActive: boolean;
begin
  OldActive := Active;

  Close;
  UnPrepare;
  AssembleSQL;

  if OldActive then
    Open;
end;

procedure TCustomDADataSet.AssembleSQL;
begin
  FCommand.AssembleSQL;

// close and unprepare to apply new sql
  UnPrepare;
end;

procedure TCustomDADataSet.InternalCreateProcCall(const Name: _string; NeedDescribe: boolean; IsQuery: boolean = False);
begin
  FCommand.InternalCreateProcCall(Name, NeedDescribe, IsQuery);
end;

procedure TCustomDADataSet.ScanMacros(Sender: TObject = nil);
var
  AllSQL: _string;
  stIdx: TStatementType;
begin
  AllSQL := SQL.Text;
  for stIdx := Low(FUpdateSQL) to High(FUpdateSQL) do
    if Assigned(FUpdateSQL[stIdx]) then
      AllSQL := AllSQL + FUpdateSQL[stIdx].Text;
  Macros.Scan(AllSQL);    
end;

{function TCustomDADataSet.DoGetFinalSQL: _string;
begin
  Result := GetFinalSQL;
end;

procedure TCustomDADataSet.DoScanMacros(Sender: TObject);
begin
  ScanMacros;
end;}

procedure TCustomDADataSet.DefineProperties(Filer: TFiler);
  function InternalWriteParams: boolean;
  begin
    if Filer.Ancestor <> nil then
      Result := not FParams.IsEqual(TCustomDADataSet(Filer.Ancestor).FParams)
    else
      Result := FParams.Count > 0;
  end;

  function WriteMacros: boolean;
  begin
    if Filer.Ancestor <> nil then
      Result := not FMacros.IsEqual(TCustomDADataSet(Filer.Ancestor).FMacros)
    else
      Result := FMacros.Count > 0;
  end;
begin
  inherited DefineProperties(Filer);

  Filer.DefineProperty('ParamData', FCommand.ReadParamData, FCommand.WriteParamData, InternalWriteParams);
  Filer.DefineProperty('MacroData', FCommand.ReadMacroData, FCommand.WriteMacroData, WriteMacros);
  Filer.DefineProperty('CommandStoredProcName', FCommand.ReadStoredProcName, FCommand.WriteStoredProcName,
    FCommand.FStoredProcName <> '');
  Filer.DefineProperty('StoredProcIsQuery', FCommand.ReadStoredProcIsQuery,
    FCommand.WriteStoredProcIsQuery, FCommand.FStoredProcIsQuery);
end;

function TCustomDADataSet.FindParam(const Value: _string): TDAParam;
begin
  Result := FParams.FindParam(Value);
end;

function TCustomDADataSet.ParamByName(const Value: _string): TDAParam;
begin
  Result := FParams.ParamByName(Value);
end;

function TCustomDADataSet.FindMacro(const Value: _string): TMacro;
begin
  Result := FMacros.FindMacro(Value);
end;

function TCustomDADataSet.MacroByName(const Value: _string): TMacro;
begin
  Result := FMacros.MacroByName(Value);
end;

{ Additional data types }

function TCustomDADataSet.GetField(FieldDesc: TFieldDesc): TField;
var
  i: integer;
begin
  Assert(FieldDesc <> nil);
  Result := nil;
  for i := 0 to Fields.Count - 1 do
    if Fields[i].FieldNo = FieldDesc.FieldNo then begin
      Result := Fields[i];
      Break;
    end;
end;

function TCustomDADataSet.GetDataType(const FieldName: _string): integer;
begin
  if FIRecordSet = nil then
    raise Exception.Create(Format(SFieldNotFound, [FieldName]));

  Result := FIRecordSet.FieldByName(FieldName).DataType
end;

function TCustomDADataSet.GetFieldDesc(const FieldName: _string): TFieldDesc;
begin
  if FIRecordSet = nil then
    raise Exception.Create(Format(SFieldNotFound, [FieldName]));

  Result := FIRecordSet.FieldByName(FieldName);
end;

function TCustomDADataSet.GetFieldDesc(const FieldNo: integer): TFieldDesc;
begin
  if (FIRecordSet = nil) or (FieldNo <= 0) then {fkCalculated, fkLookup}
    Result := nil
  else
    Result := TFieldDesc(FIRecordSet.Fields[FieldNo - 1])
end;

function TCustomDADataSet.GetFieldPrecision(const FieldName: _string): integer;
var
  Field: TFieldDesc;
begin
  if FIRecordSet = nil then
    raise Exception.Create(Format(SFieldNotFound, [FieldName]));

  Field := FIRecordSet.FieldByName(FieldName);
  if (Field <> nil) and (Field.DataType in [dtInteger,dtLargeint,dtFloat,
    dtBCD{$IFDEF VER6P}{$IFNDEF FPC},dtFmtBCD{$ENDIF}{$ENDIF}])
  then
    Result := Field.Length
  else
    Result := 0;
end;

function TCustomDADataSet.GetFieldScale(const FieldName: _string): integer;
var
  Field: TFieldDesc;
begin
  if FIRecordSet = nil then
    raise Exception.Create(Format(SFieldNotFound, [FieldName]));

  Field := FIRecordSet.FieldByName(FieldName);
  if (Field <> nil) and (Field.DataType in [dtInteger,dtLargeint,dtFloat,
    dtBCD{$IFDEF VER6P}{$IFNDEF FPC},dtFmtBCD{$ENDIF}{$ENDIF}])
  then
    Result := Field.Scale
  else
    Result := 0;
end;

function TCustomDADataSet.GetFieldObject(FieldDesc: TFieldDesc): TSharedObject;
var
  RecBuf: TRecordBuffer;
begin
  if GetActiveRecBuf(RecBuf) then begin
    Assert(FIRecordSet <> nil);
    if not FIRecordSet.IsComplexFieldType(FieldDesc.DataType) then
      DatabaseError(SNeedBlobType);

    Result := TSharedObject(GetGCHandleTarget(Marshal.ReadIntPtr(RecBuf, FieldDesc.Offset)));
  end
  else
    Result := nil;
end;

function TCustomDADataSet.GetFieldObject(Field: TField): TSharedObject;
var
  FieldDesc: TFieldDesc;
begin
  FieldDesc := GetFieldDesc(Field);
  Result := GetFieldObject(FieldDesc);
end;

function TCustomDADataSet.GetFieldObject(const FieldName: _string): TSharedObject;
var
  FieldDesc: TFieldDesc;
begin
  if FIRecordSet = nil then
    raise Exception.Create(Format(SFieldNotFound, [FieldName]));

  FieldDesc := FIRecordSet.FieldByName(FieldName);
  Result := GetFieldObject(FieldDesc);
end;

{$IFDEF VER5P}
{$IFDEF WITH_IPROVIDER}

{ IProviderSupport }

function TCustomDADataSet.PSInTransaction: Boolean;
begin
  Result := UsedUpdateTransaction.Active;
end;

procedure TCustomDADataSet.PSStartTransaction;
begin
  if (UsedConnection <> nil) and
    AutoCommit and
    UsedConnection.AutoCommit
  then
    UsedUpdateTransaction.StartTransaction;
end;

procedure TCustomDADataSet.PSEndTransaction(Commit: Boolean);
begin
  if (UsedConnection <> nil) and
    AutoCommit and
    UsedConnection.AutoCommit
  then
    if Commit then
      UsedUpdateTransaction.Commit
    else
      UsedUpdateTransaction.Rollback;
end;

procedure TCustomDADataSet.PSExecute;
begin
  Execute;
end;

function TCustomDADataSet.PSExecuteStatement(const ASQL: string; AParams: TParams;
  {$IFDEF CLR}var ResultSet: TObject{$ELSE}ResultSet: Pointer = nil{$ENDIF}): Integer;

  procedure SetSQL(SQL: _TStrings);
  var
    St: _StringBuilder;
    i, j: integer;
  begin
    // replace parameters in SQL
    St := _StringBuilder.Create(Length(ASQL) + Length(ASQL) div 2);
    try
      j := 1;
      for i := 1 to Length(ASQL) do
        if ASQL[i] = '?' then begin
          St.Append(':');
          St.Append(IntToStr(j));
          Inc(j)
        end
        else
          St.Append(ASQL[i]);
      SQL.Text := St.ToString;
    finally
      St.Free;
    end;
  end;

  procedure SetParams(Params: TDAParams);
  var
    i: integer;
  begin
    //FQuery.Params.Assign(AParams);  // params doesn't name
    for i := 0 to Params.Count - 1 do begin
      Params[i].Assign(AParams[i]);
      Params[i].Name := IntToStr(i + 1);
      if Params[i].ParamType = ptUnknown then
        Params[i].ParamType := ptInput;
    end;
  end;

var
  Query: TCustomDADataSet;
begin
  if Assigned(ResultSet) then begin
    Query := UsedConnection.CreateDataSet;
    try
      Query.Debug := Debug;
      SetSQL(Query.SQL);
      SetParams(Query.Params);
      Query.Execute;
      Result := Query.RowsAffected;
    {$IFDEF CLR}
      ResultSet := Query;
    {$ELSE}
      TDataSet(ResultSet^) := Query;
    {$ENDIF}
    except
      Query.Free;
      raise;
    end;
  end
  else
  begin
    CheckDataSetService;
    FDataSetService.FUpdater.CheckUpdateQuery(stCustom);
    Query := TCustomDADataSet(FDataSetService.FUpdater.FUpdateQuery);
    Query.Debug := Debug;
    Query.FLockDebug := False;
    Query.Transaction := UsedUpdateTransaction;

    SetSQL(Query.SQL);
    SetParams(Query.Params);

    Query.Execute;
    Result := Query.RowsAffected;
  end;
end;

function TCustomDADataSet.PSGetParams: DB.TParams;
begin
  Result := Params;
end;

function TCustomDADataSet.PSGetQuoteChar: string;
begin
  Result := '"';
end;

function TCustomDADataSet.PSGetTableName: string;
var
  UpdatingTableInfoIdx: integer;
begin
  if Active then begin
    UpdatingTableInfoIdx := FDataSetService.UpdatingTableInfoIdx;
    if (UpdatingTableInfoIdx < 0) or (UpdatingTableInfoIdx >= TablesInfo.Count) then
      Result := SQLInfo.NormalizeName(FUpdatingTable)
    else
      Result := TablesInfo[UpdatingTableInfoIdx].TableName;
  end
  else begin
    Result := FOldTableName;

    if Result = '' then
      Result := FUpdatingTable;
    if Result = '' then
      Result := GetTableNameFromSQL(SQL.Text);

    Result := SQLInfo.NormalizeName(Result);
  end;
end;

function TCustomDADataSet.PSIsSQLBased: Boolean;
begin
  Result := True;
end;

function TCustomDADataSet.PSIsSQLSupported: Boolean;
begin
  Result := True;
end;

procedure TCustomDADataSet.PSReset;
begin
  inherited PSReset;

  if Active then begin
    Close;
    Open;
  end;
end;

procedure TCustomDADataSet.PSSetParams(AParams: DB.TParams);
var
  Param: TDAParam;
  i: integer;
begin
  if AParams.Count <> 0 then begin
    if AParams.Count >= Params.Count then
      Params.Assign(AParams)
    else begin
      Params.BeginUpdate;
      try
        for i := 0 to AParams.Count - 1 do begin
          Param := Params.FindParam(AParams.Items[i].Name);
          if Param <> nil then
            Param.Assign(AParams.Items[i])
          else
            Params.Items[i].Assign(AParams.Items[i])
        end;
      finally
        Params.EndUpdate;
      end;
    end;
  end;
end;

procedure TCustomDADataSet.PSSetCommandText(const CommandText: string);
begin
  if CommandText <> '' then
    SQL.Text := CommandText;
end;

function TCustomDADataSet.PSUpdateRecord(UpdateKind: TUpdateKind; Delta: TDataSet): Boolean;
var
  UpdateAction: TUpdateAction;
begin
  Result := False;
  if Assigned(OnUpdateRecord) then
  begin
    UpdateAction := uaFail;
    OnUpdateRecord(Delta, UpdateKind, UpdateAction);
    Result := UpdateAction = uaApplied;
  end;

  if not Result and Assigned(Delta) then begin
    CheckDataSetService;
    Result := FDataSetService.FUpdater.PerformPSUpdateRecord(UpdateKind, Delta);
  end;
end;

function TCustomDADataSet.PSGetDefaultOrder: TIndexDef;

  function AddField(const Fields, NewField: _string): _string;
  begin
    if Fields <> '' then
      Result := Fields + ';' + NewField
    else
      Result := NewField;
  end;

var
  S, Token, SaveField: _string;
  Parser: TBoolParser;
  Code, Index: integer;
begin
  Result := nil;
  S := GetOrderBy;
  if S = '' then
    Exit;

  Parser := TBoolParser.Create(S);
  Parser.DecSeparator := '.';
  try
    Result := TIndexDef.Create(nil);
    Parser.ToBegin();

    Code := Parser.GetNext(Token);
    while Code <> lcEnd do begin
      case Code of
        lcIdent, lcString: begin
          if 'DESC' = UpperCase(Token) then begin
            Result.DescFields := AddField(Result.DescFields, SaveField);
          end
          else
          if Assigned(FindField(Token)) then begin
            Result.Fields := AddField(Result.Fields, Token);
            SaveField := Token;
          end;
        end;
        lcNumber: begin
          try
            Index := StrToInt(Token);
            SaveField := FieldDefs[Index - 1].Name;
          except // float number
            Code := Parser.GetNext(Token);  //to prevent freezeng on errors. CR
            continue;
          end;
          Result.Fields := AddField(Result.Fields, SaveField);
        end;
      end;

      Code := Parser.GetNext(Token);
    end;
  finally
    Parser.Free;
  end;
end;

function TCustomDADataSet.PSGetKeyFields: string;
var
  i: integer;
  KeyFieldDescs: TFieldDescArray;
  QO: boolean;
  QOInfo: TQuickOpenInfo;
begin
  KeyFieldDescs := nil;
  Result := inherited PSGetKeyFields;
  if Result = '' then
    if FOldKeyFields = '' then begin
      if FKeyFields <> '' then
        Result := FKeyFields
      else begin
        CheckDataSetService;
        if not FDataSetService.PreventPSKeyFields(Result) then begin //Set product specific KeyField values or omit Server roundtrip on DS opening
          QO := not Active and (Connection <> nil) and ((FIRecordSet = nil) or (FIRecordSet.FieldCount = 0));
          if QO then
            QuickOpen(QOInfo);
          try
            FDataSetService.GetKeyFieldDescs(KeyFieldDescs, True); // For TClientDataSet with SDAC

            for i := 0 to High(KeyFieldDescs) do
              if FindField(KeyFieldDescs[i].Name) <> nil then
                if Result = '' then
                  Result := KeyFieldDescs[i].Name
                else
                  Result := Result + ';' + KeyFieldDescs[i].Name;
          finally
            if QO then
              Restore(QOInfo);
          end;
        end;
      end;
      FOldKeyFields := Result;
    end
    else
      Result := FOldKeyFields;
end;
{$ENDIF}
{$ENDIF}

procedure TCustomDADataSet.AssignTo(Dest: TPersistent);
var
  stIdx: TStatementType;
begin
  inherited;

  if Dest is TCustomDADataSet then begin
    TCustomDADataSet(Dest).Connection := Connection;
    TCustomDADataSet(Dest).Transaction := FTransaction;
    TCustomDADataSet(Dest).UpdateTransaction := FUpdateTransaction;
    TCustomDADataSet(Dest).ParamCheck := ParamCheck;  // before SQL
    TCustomDADataSet(Dest).SQL.Text := SQL.Text;
    for stIdx := Low(FUpdateSQL) to High(FUpdateSQL) do
      if Assigned(TCustomDADataSet(Dest).FUpdateSQL[stIdx]) and Assigned(FUpdateSQL[stIdx]) then
        TCustomDADataSet(Dest).FUpdateSQL[stIdx].Text := FUpdateSQL[stIdx].Text;

    TCustomDADataSet(Dest).FilterSQL := FilterSQL;
    TCustomDADataSet(Dest).Macros.Assign(Macros);
    TCustomDADataSet(Dest).Params.Assign(Params);
    TCustomDADataSet(Dest).Debug := Debug;

    TCustomDADataSet(Dest).FetchRows := FetchRows;
    TCustomDADataSet(Dest).UniDirectional := UniDirectional;
    TCustomDADataSet(Dest).AutoCommit := AutoCommit;
    TCustomDADataSet(Dest).RefreshOptions := RefreshOptions;
    TCustomDADataSet(Dest).UpdatingTable := UpdatingTable;
    TCustomDADataSet(Dest).KeyFields := KeyFields;
    TCustomDADataSet(Dest).LockMode := LockMode;
    TCustomDADataSet(Dest).DMLRefresh := DMLRefresh;
    TCustomDADataSet(Dest).Options.Assign(Options);

    if (FCommand <> nil) and (TCustomDADataSet(Dest).FCommand <> nil) then begin
      TCustomDADataSet(Dest).FCommand.FStoredProcName := FCommand.FStoredProcName;
      TCustomDADataSet(Dest).FCommand.FStoredProcIsQuery := FCommand.FStoredProcIsQuery;
    end;
  end;
end;

procedure TCustomDADataSet.SetConnection(Value: TCustomDAConnection);
begin
  if (Value <> FConnection) or (Value <> UsedConnection) then begin

    if UsedConnection <> nil then begin
      Disconnect;
      UsedConnection.UnregisterClient(Self);
      UsedConnection.UnregisterClient(FCommand);
    end;

    FConnection := Value;

    if FConnection <> nil then begin
      Value.RegisterClient(Self, ConnectChange);

      if FIRecordSet <> nil then
        FIRecordSet.SetConnection(FConnection.FIConnection)
    end
    else
      if FIRecordSet <> nil then
        FIRecordSet.SetConnection(nil);
  {$IFDEF CLR}
    DataEvent(dePropertyChange, nil);
  {$ELSE}
    DataEvent(dePropertyChange, 0);
  {$ENDIF}
  end;
end;

function TCustomDADataSet.GetTransaction: TDATransaction;
begin
  Result := FTransaction;
end;

procedure TCustomDADataSet.SetTransaction(Value: TDATransaction);
begin
  if (FTransaction <> Value) then begin
    if FTransaction <> nil then
      RemoveFreeNotification(FTransaction);
    FTransaction := Value;
    if FTransaction <> nil then
      FreeNotification(FTransaction);
  end;
end;

procedure TCustomDADataSet.SetUpdateTransaction(Value: TDATransaction);
begin
  if (FUpdateTransaction <> Value) then begin
    if FUpdateTransaction <> nil then
      RemoveFreeNotification(FUpdateTransaction);
    FUpdateTransaction := Value;
    if FUpdateTransaction <> nil then
      FreeNotification(FUpdateTransaction);
  end;
end;

function TCustomDADataSet.GetSQL: _TStrings;
begin
  Result := FCommand.SQL;
end;

procedure TCustomDADataSet.SetSQL(Value: _TStrings);
begin
  FCommand.SQL := Value;
end;

procedure TCustomDADataSet.SetFetchRows(Value: integer);
begin
  if FFetchRows <> Value then begin
    CheckInactive;
    if (Value < 1) or (Value > 65535) then
      DatabaseError(SInvalidFetchRows);

    FFetchRows := Value;
    if FIRecordSet <> nil then
      FIRecordSet.SetProp(prFetchRows, FetchRows);
  end;
end;

function TCustomDADataSet.GetFetchAll: boolean;
begin
  Result := FFetchAll;
end;

procedure TCustomDADataSet.SetFetchAll(Value: boolean);
begin
  FFetchAll := Value;
  if FIRecordSet <> nil then
    FIRecordSet.SetProp(prFetchAll, FFetchAll);
  if FFetchAll then begin
    UniDirectional := False;
    if Active then begin
      FIRecordSet.FetchAll;
      Resync([]);
    end;
  end;
end;

function TCustomDADataSet.SQLAutoGenerated: boolean;
begin
  Result := False;
end;

function TCustomDADataSet.CreateBlobStream(Field: TField; Mode: TBlobStreamMode): TStream;
var
  Blob: TBlob;
  OldRollback: boolean;
begin
  if Field.DataSet = Self then
    Blob := GetBlob(Field)
  else
    Blob := GetBlob(Field.FieldName);
  if (Blob <> nil) and (Mode <> bmWrite) and UsedConnection.ConvertEOL
    and (Field.DataType in [ftMemo{$IFDEF VER10P}, ftWideMemo{$ENDIF}, ftOraClob])
  then begin
    OldRollback := Blob.RollbackEnabled;
    Blob.RollbackEnabled := False;
    try
      Blob.AddCR;
    finally
      Blob.RollbackEnabled := OldRollback;
    end;
  end;
    
  Result := inherited CreateBlobStream(Field, Mode);
end;

function TCustomDADataSet.GetParams: TDAParams;
begin
  Result := FCommand.Params;
end;

procedure TCustomDADataSet.SetParams(Value: TDAParams);
begin
  FCommand.Params := Value;
end;

function TCustomDADataSet.GetParamCount: word;
begin
  Result := FParams.Count;
end;

function TCustomDADataSet.GetParamCheck: boolean;
begin
  Result := FCommand.ParamCheck;
end;

procedure TCustomDADataSet.SetParamCheck(Value: boolean);
begin
  FCommand.ParamCheck := Value;
end;

function TCustomDADataSet.GetMacros: TMacros;
begin
  Result := FCommand.Macros;
end;

procedure TCustomDADataSet.SetMacros(Value: TMacros);
begin
  FCommand.Macros := Value;
end;

function TCustomDADataSet.GetMacroCount: word;
begin
  Result := FMacros.Count;
end;

function TCustomDADataSet.GetRecordCount: integer;
var
  RowsFetched: variant;
begin
  if not Active or Fetched or not Options.QueryRecCount or (FRecordCount = 0) then
    Result := inherited GetRecordCount
  else begin
    FIRecordSet.GetProp(prRowsFetched, RowsFetched);
    Result := FRecordCount - RowsFetched + Data.RecordCount;
  end;
end;

function TCustomDADataSet.GetRowsAffected: integer;
begin
  Result := FRowsAffected;
end;

procedure TCustomDADataSet.SetUniDirectional(Value: boolean);
begin
  if Value <> FUniDirectional then begin
    CheckInactive;
    FUniDirectional := Value;
    if FIRecordSet <> nil then
      FIRecordSet.SetProp(prUniDirectional, FUniDirectional);
    if FUniDirectional then
      FetchAll := False;
  end;
end;

procedure TCustomDADataSet.SetAutoCommit(Value: boolean);
begin
  FAutoCommit := Value;
  if FICommand <> nil then
    FICommand.SetProp(prAutoCommit, FAutoCommit);
end;

function TCustomDADataSet.GetIsQuery: boolean;
begin
  if FIRecordSet <> nil then
    Result := FIRecordSet.RowsReturn
  else
    Result := False;
end;

//------------------------------------------------------------------------------
// SQL Modification methods
//------------------------------------------------------------------------------

procedure TCustomDADataSet.SaveSQL;
begin
  FBaseSQL := SQL.Text;
end;

procedure TCustomDADataSet.RestoreSQL;
begin
  if FBaseSQL <> '' then begin
    SQL.Text := FBaseSQL;
    FBaseSQL := '';
  end;
end;

function TCustomDADataSet.SQLSaved: boolean;
begin
  Result := FBaseSQL <> '';
end;

/// SaveModifiedSQL is used to back up original sql text before modification.
procedure TCustomDADataSet.SaveModifiedSQL(NewSQL: _string);
var
  BaseSQL: _string;
begin
  if NewSQL <> Trim(SQL.Text) then begin
    if FBaseSQL = '' then
      FBaseSQL := SQL.Text;
    if not BaseSQLOldbehavior then
      BaseSQL := FBaseSQL;

    SQL.Text := NewSQL;
    if not BaseSQLOldbehavior then
      FBaseSQL := BaseSQL;
  end;
end;

procedure TCustomDADataSet.InternalOpen;
var
  MessageID: cardinal;
begin
  if Options.AutoPrepare then
    Prepare;

  if not FLockDebug and (TDASQLMonitorClass(UsedConnection.SQLMonitorClass).HasMonitor or Debug) then
    TDASQLMonitorClass(UsedConnection.SQLMonitorClass).SQLExecute(Self, FinalSQL, FParams, '', MessageID, True);

  // When FetchAll is True and fetch is performed on recordset openning
  // we should avoid connection releasing to perform all operations
  // in InitCursor in the same connection
  BeginConnection;
  try
    inherited;

    FDataSetService.InitCursor;
  finally
    EndConnection;
  end;  

  if not FLockDebug and (TDASQLMonitorClass(UsedConnection.SQLMonitorClass).HasMonitor or Debug) then
    TDASQLMonitorClass(UsedConnection.SQLMonitorClass).SQLExecute(Self, FinalSQL, FParams, '', MessageID, False);

  FNeedAddRef := FUniDirectional;

  if IsMasterDatasetActive and FOptions.LocalMasterDetail then begin
    MDLinksRefreshed;
    Data.FilterUpdated;
  end;
end;

/// SQLAddWhere, SQLDeleteWhere, SQLSetOrderBy, SQlGetOrderBy are SQL-server
/// specific functions and they should be overriden in descendants. They operate
/// with strings only. Return value is new SQL text or it's part being
/// requested.
function TCustomDADataSet.SQLGetFrom(SQLText: _string): _string;
begin
  Result := '';
end;

function TCustomDADataSet.SQLAddWhere(SQLText, Condition: _string): _string;
begin
  Result := '';
end;

function TCustomDADataSet.SQLDeleteWhere(SQLText: _string): _string;
begin
  Result := '';
end;

function TCustomDADataSet.SQLGetWhere(SQLText: _string): _string;
begin
  Result := '';
end;

function TCustomDADataSet.SQLSetOrderBy(SQLText: _string; Fields: _string): _string;
begin
  Result := '';
end;

function TCustomDADataSet.SQLGetOrderBy(SQLText: _string): _string;
begin
  Result := '';
end;

/// AddWhere, DeleteWhere, SetOrderBy, GetOrderBy are public SQL-server
/// indepedant methods. They can modify SQL property value of the dataset. Each
/// method calls it's internal equivalent with 'SQL' prefix.
procedure TCustomDADataSet.AddWhere(Condition: _string);
begin
  CheckSQL;
  SaveModifiedSQL(SQLAddWhere(Trim(SQL.Text), Condition));
end;

procedure TCustomDADataSet.DeleteWhere;
begin
  CheckSQL;
  SaveModifiedSQL(SQLDeleteWhere(Trim(SQL.Text)));
end;

procedure TCustomDADataSet.SetOrderBy(Fields: _string);
begin
  CheckSQL;
  SaveModifiedSQL(SQLSetOrderBy(Trim(SQL.Text), Fields));
end;

function TCustomDADataSet.GetOrderBy: _string;
begin
  CheckSQL;
  Result := SQLGetOrderBy(Trim(SQL.Text));
end;

/// GetBaseSQL returns original sql text with expanded macros.
function TCustomDADataSet.GetBaseSQL: _string;
begin
  if FBaseSQL <> '' then
    Result := FBaseSQL
  else
    Result := SQL.Text;

  if Macros.Count > 0 then
    Macros.Expand(Result);
end;

procedure TCustomDADataSet.CheckSQL;
begin
  
end;

function TCustomDADataSet.GetFinalSQL: _string;
var
  Str, Where: _string;
  MasterName: _string;
  DetailName: _string;
  MasterPos: integer;
  DetailPos: integer;
begin
  Str := FCommand.FinalSQL;

  if FFilterSQL <> '' then
    Str := SQLAddWhere(Str, FilterSQL);

  if IsConnectedToMaster and not FOptions.LocalMasterDetail then begin
    MasterPos := 1;
    DetailPos := 1;
    Where := '';
    while True do begin
      MasterName := ExtractFieldName(FMasterFields, MasterPos);
      DetailName := ExtractFieldName(FDetailFields, DetailPos);
      if (MasterName <> '') and (DetailName <> '') then begin
        if Where <> '' then
          Where := Where + ' and ';
        Where := Where + SQLInfo.NormalizeName(DetailName, Options.QuoteNames) + ' = :' + SQLInfo.NormalizeName(MasterName, Options.QuoteNames);
      end
      else
        break;
    end;
    if Where <> '' then
      Str := SQLAddWhere(Str, Where);
  end;

  Result := Str;
end;

{$IFDEF CLR}
procedure TCustomDADataSet.DataEvent(Event: TDataEvent; Info: TObject);
{$ELSE} {$IFDEF FPC}
procedure TCustomDADataSet.DataEvent(Event: TDataEvent; Info: PtrInt);
{$ELSE} {$IFDEF VER16P}
procedure TCustomDADataSet.DataEvent(Event: TDataEvent; Info: NativeInt);
{$ELSE}
procedure TCustomDADataSet.DataEvent(Event: TDataEvent; Info: Longint);
{$ENDIF} {$ENDIF} {$ENDIF}
begin
  inherited DataEvent(Event, Info);

  // design-time only for improving performance
  if (csDesigning in ComponentState) and (not FDesignCreate)  then
    if Event = deFieldListChange then
      if (not Active) and Options.DefaultValues and (FDataSetService <> nil) then begin
        if Data.Fields.Count = 0 then
          InternalInitFieldDefs;
        FDataSetService.FillFieldsDefaultValues;
      end;
end;

function TCustomDADataSet.GetNextRecord: Boolean;
var
  OldCurrentRecord: integer;
  Buffer: TRecordBuffer;
begin
  OldCurrentRecord := CurrentRecord + 1;
  Result := inherited GetNextRecord;
  if Result and FUniDirectional then
    if (OldCurrentRecord > 0) and (OldCurrentRecord >= BufferCount) then begin
      Buffer := Buffers[CurrentRecord + 1];
      if Buffer <> nil then
        FreeRefComplexFields(Buffer);
    end;
end;

procedure TCustomDADataSet.SetUpdateObject(Value: TCustomDAUpdateSQL);
begin
  if Value <> nil then begin
    Value.CheckUpdateComponent(Value.ModifyObject, Self);
    Value.CheckUpdateComponent(Value.InsertObject, Self);
    Value.CheckUpdateComponent(Value.DeleteObject, Self);
    Value.CheckUpdateComponent(Value.RefreshObject, Self);
  end;
  if Value <> FUpdateObject then begin
    if Assigned(FUpdateObject) and (FUpdateObject.DataSet = Self) then
      FUpdateObject.DataSet := nil;
    FUpdateObject := Value;
    if Assigned(FUpdateObject) then begin
      if Assigned(FUpdateObject.DataSet) and
        (FUpdateObject.DataSet <> Self) then
        FUpdateObject.DataSet.UpdateObject := nil;
      FUpdateObject.DataSet := Self;
    end;
  end;
end;

procedure TCustomDADataSet.SetOptions(Value: TDADataSetOptions);
begin
  FOptions.Assign(Value);
end;

procedure TCustomDADataSet.SetFilterSQL(const Value: _string);
var
  OldFilter: _string;
  OldActive: boolean;
begin
  if Value <> FFilterSQL then begin
    OldFilter := FFilterSQL;
    FFilterSQL := Trim(Value);

    OldActive := Active;
    if not (csReading in ComponentState) then begin
      Close;
      UnPrepare;
    end;

    AssembleSQL;

    if OldActive then
      try
        Open;
        if Filtered and (Filter <> '') then begin
          Data.FilterUpdated;
          Resync([]);
          First;
        end;
      except
        FFilterSQL := OldFilter;
        AssembleSQL;
        raise;
      end;
  end;
end;

procedure TCustomDADataSet.SetFiltered(Value: boolean);
var
  KeyFields: TFieldArray;
  Values: variant;
begin
  if Value <> Filtered then begin
    if Active then
      GetCurrentKeys(KeyFields, Values);

    inherited;

    if Active and (Length(KeyFields) <> 0) then
      Locate(KeyFields, Values, []);
  end;
end;

function TCustomDADataSet.GetCursor: TCRCursor;
begin
  if FICommand <> nil then
    Result := FICommand.GetCursor
  else
    Result := nil;
end;

function TCustomDADataSet.GetCRCursor: TCRCursor;
begin
  Result := GetCursor;
end;

procedure TCustomDADataSet.SetCRCursor(Value: TCRCursor);
begin
  if Value <> GetCursor then begin
    CheckConnection;  //We doesn't increase FConnectCount using this funct
    Close;
    UnPrepare;
    FieldDefs.Updated := False;
    CheckIRecordSet;

    FICommand.SetCursor(Value);
    FIRecordSet.CommandType := ctCursor;
  end;
end;

{ TDADataSetOptions }

constructor TDADataSetOptions.Create(Owner: TCustomDADataSet);
begin
  inherited Create;

  FOwner := Owner;

  SetFieldsReadOnly := True;
  RequiredFields := True;
  StrictUpdate := True;
  TrimFixedChar := True;
  LongStrings := True;
  FlatBuffers := False;
  RemoveOnRefresh := True;
  UpdateBatchSize := 1;
end;

procedure TDADataSetOptions.AssignTo(Dest: TPersistent);
begin
  if Dest is TDADataSetOptions then begin
    TDADataSetOptions(Dest).SetFieldsReadOnly := SetFieldsReadOnly;
    TDADataSetOptions(Dest).RequiredFields := RequiredFields;
    TDADataSetOptions(Dest).StrictUpdate := StrictUpdate;
    TDADataSetOptions(Dest).NumberRange := NumberRange;
    TDADataSetOptions(Dest).QueryRecCount := QueryRecCount;
    TDADataSetOptions(Dest).AutoPrepare := AutoPrepare;
    TDADataSetOptions(Dest).ReturnParams := ReturnParams;
    TDADataSetOptions(Dest).TrimFixedChar := TrimFixedChar;
    TDADataSetOptions(Dest).LongStrings := LongStrings;
    TDADataSetOptions(Dest).FlatBuffers := FlatBuffers;
    TDADataSetOptions(Dest).RemoveOnRefresh := RemoveOnRefresh;
    TDADataSetOptions(Dest).QuoteNames := QuoteNames;
    TDADataSetOptions(Dest).DetailDelay := DetailDelay;
  {$IFDEF HAVE_COMPRESS}
    TDADataSetOptions(Dest).CompressBlobMode := CompressBlobMode;
  {$ENDIF}
    TDADataSetOptions(Dest).LocalMasterDetail := LocalMasterDetail;
    TDADataSetOptions(Dest).CacheCalcFields := CacheCalcFields;
    TDADataSetOptions(Dest).FieldsOrigin := FieldsOrigin;
    TDADataSetOptions(Dest).DefaultValues := DefaultValues;
    TDADataSetOptions(Dest).UpdateBatchSize := UpdateBatchSize;
    TDADataSetOptions(Dest).UpdateAllFields := UpdateAllFields;

    TDADataSetOptions(Dest).FullRefresh := FullRefresh;
    TDADataSetOptions(Dest).TrimVarChar := TrimVarChar;
    TDADataSetOptions(Dest).SetEmptyStrToNull := SetEmptyStrToNull;
    TDADataSetOptions(Dest).PrepareUpdateSQL := PrepareUpdateSQL;
    TDADataSetOptions(Dest).ExtendedFieldsInfo := ExtendedFieldsInfo;
    TDADataSetOptions(Dest).EnableBCD := EnableBCD;
  {$IFDEF VER6P}
  {$IFNDEF FPC}
    TDADataSetOptions(Dest).EnableFMTBCD := EnableFMTBCD;
  {$ENDIF}
  {$ENDIF}
  end
  else
    inherited;
end;

procedure TDADataSetOptions.SetRequiredFields(Value: boolean);
begin
  if FRequiredFields <> Value then begin
    FRequiredFields := Value;
    FOwner.FLocalConstraints := FRequiredFields;
    FOwner.FieldDefs.Updated := False;
    if FOwner.Active then // for change RequiredFields in runtime
      FOwner.FieldDefs.Update;
  end;
end;

procedure TDADataSetOptions.SetNumberRange(Value: boolean);
begin
  FNumberRange := Value;
  FOwner.FNumberRange := FNumberRange;
end;

procedure TDADataSetOptions.SetTrimFixedChar(Value: boolean);
begin
  FTrimFixedChar := Value;
  if FOwner.Data <> nil then
    FOwner.Data.TrimFixedChar := FTrimFixedChar;
end;

procedure TDADataSetOptions.SetTrimVarChar(Value: boolean);
begin
  FTrimVarChar := Value;
  if FOwner.Data <> nil then
    FOwner.Data.TrimVarChar := FTrimVarChar;
end;

procedure TDADataSetOptions.SetSetEmptyStrToNull(Value: boolean);
begin
  FSetEmptyStrToNull := Value;
  if FOwner.Data <> nil then
    FOwner.Data.SetEmptyStrToNull := FSetEmptyStrToNull;
end;

procedure TDADataSetOptions.SetLongStrings(Value: boolean);
begin
  FOwner.CheckInactive;
  FLongStrings := Value;
  if FOwner.FIRecordSet <> nil then
    FOwner.FIRecordSet.SetProp(prLongStrings, FLongStrings);
  FOwner.FieldDefs.Updated := False;
end;

procedure TDADataSetOptions.SetAutoPrepare(Value: boolean);
begin
  if FAutoPrepare = Value then
    Exit;

  FOwner.Unprepare;
  FAutoPrepare := Value;
end;

procedure TDADataSetOptions.SetFlatBuffers(const Value: boolean);
begin
  FOwner.CheckInactive;
  FFlatBuffers := Value;
  if FOwner.FIRecordSet <> nil then
    FOwner.FIRecordSet.SetProp(prFlatBuffers, Value);
end;

function TDADataSetOptions.GetDetailDelay: integer;
begin
  Result := FOwner.DetailDelay;
end;

procedure TDADataSetOptions.SetDetailDelay(Value: integer);
begin
  FOwner.DetailDelay := Value;
end;

procedure TDADataSetOptions.SetLocalMasterDetail(Value: boolean);
begin
  if Value <> FLocalMasterDetail then begin
    FOwner.CheckInactive;
    FLocalMasterDetail := Value;
    FOwner.SetLocalDetailFilter;

    if Value then
      FOwner.AssembleSQL;
  end;
end;

function TDADataSetOptions.GetCacheCalcFields: boolean;
begin
  Result := FOwner.FCacheCalcFields;
end;

procedure TDADataSetOptions.SetCacheCalcFields(Value: boolean);
begin
  if CacheCalcFields <> Value then begin
    FOwner.CheckInactive;
    FOwner.FCacheCalcFields := Value;
  end;
end;

procedure TDADataSetOptions.SetQuoteNames(Value: boolean);
begin
  if QuoteNames <> Value then begin
    FQuoteNames := Value;
    if FOwner.FICommand <> nil then begin
      FOwner.FICommand.SetProp(prQuoteNames, Value);
      // Refresh Master-Detail link
      FOwner.MDPropertiesChanged;
    end;
  end;
end;

{$IFDEF HAVE_COMPRESS}
procedure TDADataSetOptions.SetCompressBlobMode(Value: TCompressBlobMode);
begin
  if FCompressBlobMode <> Value then begin
    TCustomDADataSet(FOwner).CheckInactive;
    FCompressBlobMode := Value;
    if FOwner.FIRecordSet <> nil then
      FOwner.FIRecordSet.SetProp(prCompressBlobMode, Integer(Value));
    FOwner.FieldDefs.Updated := False;
  end;
end;
{$ENDIF}

procedure TDADataSetOptions.SetFieldsOrigin(Value: boolean);
begin
  if FFieldsOrigin <> Value then begin
    FOwner.CheckInactive;
    FFieldsOrigin := Value;
    if FOwner.FIRecordSet <> nil then
      FOwner.FIRecordSet.SetProp(prFieldsOrigin, Value);
  end;
end;

procedure TDADataSetOptions.SetDefaultValues(Value: boolean);
begin
  if FDefaultValues <> Value then begin
    FOwner.CheckInactive;
    FDefaultValues := Value;
    if FOwner.FIRecordSet <> nil then
      FOwner.FIRecordSet.SetProp(prDefaultValues, Value);
  end;
end;

procedure TDADataSetOptions.SetExtendedFieldsInfo(Value: boolean);
begin
  if FExtendedFieldsInfo <> Value then begin
    FOwner.CheckInactive;
    FExtendedFieldsInfo := Value;
    if FOwner.FIRecordSet <> nil then
      FOwner.FIRecordSet.SetProp(prExtendedFieldsInfo, Value);
  end;
end;

procedure TDADataSetOptions.SetEnableBCD(Value: boolean);
begin
  if FEnableBCD <> Value then begin
    FOwner.CheckInactive;
    FEnableBCD := Value;
    if FOwner.FICommand <> nil then
      FOwner.FICommand.SetProp(prEnableBCD, Value);
  end;
end;

{$IFDEF VER6P}
{$IFNDEF FPC}
procedure TDADataSetOptions.SetEnableFMTBCD(Value: boolean);
begin
  if FEnableFMTBCD <> Value then begin
    FOwner.CheckInactive;
    FEnableFMTBCD := Value;
    if FOwner.FICommand <> nil then
      FOwner.FICommand.SetProp(prEnableFmtBCD, Value);
  end;
end;
{$ENDIF}
{$ENDIF}

{ TDASQLGenerator }

constructor TDASQLGenerator.Create(AOwner: TDADataSetService);
begin
  inherited Create;

  Assert(AOwner <> nil);
  FDataSetService := AOwner;
  Assert(FDataSetService.FDataSet <> nil);
  FDataSet := FDataSetService.FDataSet;

  FHeaderSB := _StringBuilder.Create(100);
  FFldSB := _StringBuilder.Create(100);
  FMiddleSB := _StringBuilder.Create(100);
  FFldParamSB := _StringBuilder.Create(100);
  FCondSB := _StringBuilder.Create(100);
  FFooterSB := _StringBuilder.Create(100);

  FParamsInfo := TDAParamsInfo.Create(GetParamInfoClass);
end;

destructor TDASQLGenerator.Destroy;
begin
  FParamsInfo.Free;

  FHeaderSB.Free;
  FFldSB.Free;
  FMiddleSB.Free;
  FFldParamSB.Free;
  FCondSB.Free;
  FFooterSB.Free;
  inherited;
end;

function TDASQLGenerator.GetParamInfoClass: TDAParamInfoClass;
begin
  Result := TDAParamInfo;
end;

procedure TDASQLGenerator.Clear;
begin
  FHeaderSB.Length := 0;
  FFldSB.Length := 0;
  FMiddleSB.Length := 0;
  FFldParamSB.Length := 0;
  FCondSB.Length := 0;
  FFooterSB.Length := 0;

  FOldRecBuf := nil;
  FNewRecBuf := nil;
end;

function TDASQLGenerator.AssembleSB(const StatementType: TStatementType): _string;
begin
  // TODO: may be optimized for Win32
  Result :=
    FHeaderSB.ToString +
    FFldSB.ToString +
    FMiddleSB.ToString +
    FFldParamSB.ToString +
    FCondSB.ToString +
    FFooterSB.ToString;
end;

function TDASQLGenerator.GetIRecordSet: TCRRecordSet;
begin
  Result := FDataSet.FIRecordSet;
end;

function TDASQLGenerator.SQLInfo: TSQLInfo;
begin
  Result := FDataSet.FICommand.SQLInfo;
end;

function TDASQLGenerator.OldRecBuf: IntPtr;
begin
  if FOldRecBuf = nil then
    FOldRecBuf := FDataSet.GetOldRecBuf;

  Result := FOldRecBuf;
end;

function TDASQLGenerator.NewRecBuf: IntPtr;
begin
  if FNewRecBuf = nil then
    FNewRecBuf := FDataSet.GetNewRecBuf;

  Result := FNewRecBuf;
end;

function TDASQLGenerator.IsBlobDataType(DataType: word): boolean;
begin
  Result := DataType in [dtBlob, dtMemo, dtWideMemo];  //Data.IsBlobFieldType
end;

function TDASQLGenerator.IsObjectDataType(DataType: word): boolean;
begin
  Result := DataType in [dtObject, dtArray];
end;

function TDASQLGenerator.FieldIsNull(FieldDesc: TCRFieldDesc; OldValue: boolean; Data: TData; OldRecBuf, NewRecBuf: IntPtr): boolean;
//This function added for Expand fields support
var
  i: integer;
begin
  Result := not FDataSet.Active;
  if Result then
    Exit;
    
  if IsObjectDataType(FieldDesc.DataType) then begin
    i := FieldDesc.FieldNo;
    while (i < Data.FieldCount) and (Data.Fields[i].ParentField = FieldDesc) do begin
      Result := FieldIsNull(TCRFieldDesc(Data.Fields[i]), OldValue);
      inc(i);
      if not Result then
        Break;
    end;
  end
  else
    if OldValue then
      Result := Data.GetNull(FieldDesc.FieldNo, OldRecBuf)
    else
      Result := Data.GetNull(FieldDesc.FieldNo, NewRecBuf);
end;

function TDASQLGenerator.FieldIsNull(FieldDesc: TCRFieldDesc; OldValue: boolean): boolean;
begin
  Result := not FDataSet.Active;
  if Result then
    Exit;

  Result := FieldIsNull(FieldDesc, OldValue, GetIRecordSet, OldRecBuf, NewRecBuf);
end;

function TDASQLGenerator.FieldModified(FieldDesc: TCRFieldDesc; Data: TData; OldRecBuf, NewRecBuf: IntPtr): boolean;
//This function added for Expand fields support
var
  i: integer;
  Field: TField;
begin
  if IsBlobDataType(FieldDesc.DataType) then begin
    if FDataSet.FInCacheProcessing then
      Result := Data.GetObject(FieldDesc.FieldNo, NewRecBuf) <>
        Data.GetObject(FieldDesc.FieldNo, OldRecBuf)
    else begin
      Field := FDataSet.GetField(FieldDesc);
      // for Modified can be set manually for TBlobField
      Result := ((Field <> nil) and TBlobField(Field).Modified) or
        TBlob(Data.GetObject(FieldDesc.FieldNo, NewRecBuf)).CanRollback;
    end;
  end
  else
  if IsObjectDataType(FieldDesc.DataType) then begin
    Result := False;
    i := FieldDesc.FieldNo;
    while (i < Data.FieldCount) do begin
      // Child FieldDescs always next to parent FielDescs
      // But child fields can be mixed with their own childs in case of nested objects
      Result := (Data.Fields[i].ParentField = FieldDesc) and
        FieldModified(TCRFieldDesc(Data.Fields[i]));
      inc(i);
      if Result then
        break;
    end;
  end
  else
    Result := TMemData(Data).CompareFields(OldRecBuf, NewRecBuf, FieldDesc, [coOrdinalCompare]) <> 0;
end;

function TDASQLGenerator.FieldModified(FieldDesc: TCRFieldDesc): boolean;
begin
  Result := FieldModified(FieldDesc, GetIRecordSet, OldRecBuf, NewRecBuf);
end;

function TDASQLGenerator.GetActualFieldName(FieldDesc: TCRFieldDesc; IsRefresh: boolean): _string;
begin
  Result := FieldDesc.ActualNameQuoted(SQLInfo, FDataSet.FOptions.FQuoteNames);
  //FDataSetService.QuoteName(GetActualFieldNameEx(FieldDesc, FieldDesc.TableInfo));
  if IsRefresh and (FDataSet.Options.FullRefresh or FDataSet.ReadOnly) and
    not FDataSetService.FUpdTableIsArtificial
  then begin//Use original Select part, so we could use Field aliasess
    if (FieldDesc.TableInfo <> nil) then
      if FieldDesc.TableInfo.TableAlias <> '' then
        Result := SQLInfo.NormalizeName(FieldDesc.TableInfo.TableAlias, FDataSet.FOptions.FQuoteNames) +
          '.' + Result
      else
        Result := SQLInfo.NormalizeName(FieldDesc.TableInfo.TableName, FDataSet.FOptions.FQuoteNames) +
          '.' + Result;
  end;
end;

(*function TDASQLGenerator.GetActualFieldNameEx(FieldDesc: TCRFieldDesc; TableInfo: TCRTableInfo): _string;
var
  p: integer;
begin
  Result := FieldDesc.ActualName;
  if (FieldDesc.TableInfo = TableInfo) or (FieldDesc.TableInfo.TableName = TableInfo.TableName) then begin
    p := Pos('.', Result);
    if (p <> 0) and (Pos(FieldDesc.TableInfo.TableName, Result) = 1) then //Here we should detremine that Result is not something like "Dot.FieldName"
      Result := Copy(Result, p + 1, MaxInt);{Delete table name from fieldname}
  end;
end;*)

function TDASQLGenerator.GenerateIndexName(Name: _string): _string;
begin
  Result := '_' + Name;
end;

function TDASQLGenerator.DecodeFieldIndex(FieldName: _string): integer;
var
  e: integer;
begin
  Result := -1;
  if (Length(FieldName) >= 2) and (FieldName[1] = '_') then begin
    Val(Copy(FieldName, 2, MaxInt), Result, e);
    if e <> 0 then
      Result := -1;
  end;
end;

function TDASQLGenerator.IndexedPrefix: _string;
begin
  Result := 'P_';
end;

function TDASQLGenerator.MaxIdentLength: integer;
begin
  Result := MaxInt;
end;

function TDASQLGenerator.IsSubstituteParamName: boolean;
begin
  Result := True;
end;

procedure TDASQLGenerator.AddParam(SB: _StringBuilder; FieldDesc: TFieldDesc;
  const StatementType: TStatementType; Index: integer = -1; Old: boolean = False);
begin
  AddParam(SB, FieldDesc, StatementType, ptUnknown, Index, Old);
end;

procedure TDASQLGenerator.AddParam(SB: _StringBuilder; FieldDesc: TFieldDesc;
      const StatementType: TStatementType;
      const ParamType: TParamType;
      Index: integer = -1;
      Old: boolean = False);
const
  MaxNameLength = 30;
var
  ParamName, Prefix: _string;
  ParamInfo: TDAParamInfo;
  Param: TDAParam;
  CheckQuoteNeeded: boolean;
  Params: TDAParams;
begin
  { Nonsense after adding ParamsInfo support
  if not (csDesigning in FOwner.ComponentState) and (FieldDesc.FieldNo <> 0) then
    ParamName := IntToStr(FieldDesc.FieldNo)
  else}
    ParamName := FieldDesc.Name;
    CheckQuoteNeeded := True;

  if Old then
    Prefix := 'Old_';

  if stRefresh = StatementType then begin
    Params := TDBAccessUtils.GetParams(FDataSet);
    if Params.FindParam(ParamName) <> nil then
      Index := 1;
  end;

  if Index > - 1 then
    Prefix := IndexedPrefix + IntToStr(Index) + '_' + Prefix;

  if Length(Prefix + ParamName) <= MaxIdentLength then
    ParamName := Prefix + ParamName
  else begin
    ParamName := Prefix + IntToStr(FieldDesc.FieldNo);
    CheckQuoteNeeded := False;
  end;

//  if csDesigning in FOwner.ComponentState then // Don't call QuoteName IntToStr(FieldDesc.FieldNo)
  if SQLInfo.ParamQuoteAllowed then
    if FDataSet.Options.QuoteNames or
       CheckQuoteNeeded and SQLInfo.QuotesNeeded(FieldDesc.Name)
    then
      ParamName := SQLInfo.Quote(ParamName);

  if ((FParams = nil) or not IsSubstituteParamName) or (((FDataSet.Params.Count > 0) or FDataSet.Options.FullRefresh) and (StatementType = stRefresh)) then begin
    SB.Append(':');
    SB.Append(ParamName);
  end
  else
    SB.Append('?');

  if FParams <> nil then begin
    Param := TDAParam(FParams.Add);
    Param.ParamType := ParamType;
    Param.Name := ParamName;
    ParamInfo := TDAParamInfo(FParamsInfo.Add);
    ParamInfo.Field := FDataSet.GetField(FieldDesc);
    ParamInfo.Old := Old;
    ParamInfo.ParamIndex := Index;
  end;
end;

procedure TDASQLGenerator.AddFieldToCondition(SB: _StringBuilder; FieldDesc: TCRFieldDesc; const StatementType: TStatementType;
  const ModifiedFieldsOnly: boolean; const Index: integer = -1);
var
  ActualName: _string;
begin
  if SB.Length > 0 then
    SB.Append(' AND ');

  Assert(FieldDesc <> nil);
  ActualName := GetActualFieldName(FieldDesc, StatementType = stRefresh);
  SB.Append(ActualName);

  if not FDesignMode and FieldIsNull(FieldDesc, StatementType <> stRefresh) then  //Refresh generated with current field values
    SB.Append(' IS NULL')
  else
  begin
    SB.Append(' = ');
    AddParam(SB,FieldDesc, StatementType, ptInput, Index,
      (StatementType <> stRefresh) or
      (not FDataSet.FInCacheProcessing and                         // no need to use old value after insert
                                                                   // TODO: UPD: should be used for refresh after insaert (NEW_VALUE)
                                                                   // can not be used for refresh before update (OLD_VALUE)
       FDataSet.CachedUpdates and (StatementType = stRefresh))); //Refresh generated with current field values
  end;
end;

procedure TDASQLGenerator.GenerateConditions(SB: _StringBuilder; const StatementType: TStatementType;
      const ModifiedFieldsOnly: boolean; const KeyAndDataFields: TKeyAndDataFields;
      const Index: integer = -1);
var
  i: integer;
begin
  if Length(KeyAndDataFields.KeyFieldDescs) > 0 then
    for i := 0 to High(KeyAndDataFields.KeyFieldDescs) do
      AddFieldToCondition(SB, KeyAndDataFields.KeyFieldDescs[i], StatementType, ModifiedFieldsOnly, Index)
  else begin
    if FDataSetService.FIdentityField <> nil then
      AddFieldToCondition(SB, FDataSetService.FIdentityField,
        StatementType, ModifiedFieldsOnly, Index)
    else begin
      if Length(KeyAndDataFields.DataFieldDescs) = 0 then
        DatabaseError(SNoKeyFields);
      for i := 0 to High(KeyAndDataFields.DataFieldDescs) do
        if not IsBlobDataType(KeyAndDataFields.DataFieldDescs[i].DataType) then 
          AddFieldToCondition(SB, KeyAndDataFields.DataFieldDescs[i], StatementType, ModifiedFieldsOnly, Index);
      if SB.Length = 0 then
        DatabaseError(SNoKeyFields);
    end;
  end;
end;

procedure TDASQLGenerator.AddFieldToInsertSQL(FieldDesc: TCRFieldDesc; const Index: integer = -1);
begin
  if FFldSB.Length > 0 then begin
    FFldSB.Append(', ');
    FFldParamSB.Append(', ');
  end;

  FFldSB.Append(FieldDesc.ActualNameQuoted(SQLInfo, FDataSet.Options.QuoteNames));
  AddParam(FFldParamSB, FieldDesc, stInsert, ptInput, Index);
end;

procedure TDASQLGenerator.GenerateInsertSQL(
  const KeyAndDataFields: TKeyAndDataFields;
  const ModifiedFieldsOnly: boolean;
  const Index: integer = -1);
var
  i: integer;
  FieldDesc: TCRFieldDesc;
begin
  for i := 0 to High(KeyAndDataFields.DataFieldDescs) do begin
    FieldDesc := KeyAndDataFields.DataFieldDescs[i];
    // Insert all(!) field values
    if not ModifiedFieldsOnly or not FieldIsNull(FieldDesc, False) then
      AddFieldToInsertSQL(FieldDesc, Index);
  end;

  FHeaderSB.Append('INSERT INTO ');
  FHeaderSB.Append(SQLInfo.NormalizeName(FTableInfo.TableNameFull, FDataSet.FOptions.FQuoteNames));
  FHeaderSB.Append(SLLineSeparator);
  FHeaderSB.Append('  (');
  // Append FFldSB
  FMiddleSB.Append(')');
  FMiddleSB.Append(SLLineSeparator);
  FMiddleSB.Append('VALUES');
  FMiddleSB.Append(SLLineSeparator);
  FMiddleSB.Append('  (');
  // Append FFldParamSB
  FFooterSB.Append(')');
end;

procedure TDASQLGenerator.AddFieldToUpdateSQL(FieldDesc: TCRFieldDesc; const ModifiedFieldsOnly: boolean;
  const Index: integer = -1);
begin
  if FFldSB.Length > 0 then
    FFldSB.Append(', ');

  FFldSB.Append(FieldDesc.ActualNameQuoted(SQLInfo, FDataSet.Options.QuoteNames));
  FFldSB.Append(' = ');
  AddParam(FFldSB, FieldDesc, stUpdate, ptInput, Index);
end;

procedure TDASQLGenerator.GenerateUpdateSQL(
  const KeyAndDataFields: TKeyAndDataFields;
  const ModifiedFieldsOnly: boolean;
  const Index: integer = -1);
var
  i: integer;
  FieldDesc: TCRFieldDesc;
begin
  for i := 0 to High(KeyAndDataFields.DataFieldDescs) do begin
    FieldDesc := KeyAndDataFields.DataFieldDescs[i];

    if not ModifiedFieldsOnly or FieldModified(FieldDesc) then
      AddFieldToUpdateSQL(FieldDesc, ModifiedFieldsOnly, Index);
  end;

  if FFldSB.Length > 0 then begin
    FHeaderSB.Append('UPDATE ');
    FHeaderSB.Append(SQLInfo.NormalizeName(FTableInfo.TableNameFull, FDataSet.FOptions.FQuoteNames));
    FHeaderSB.Append(SLLineSeparator);
    FHeaderSB.Append('SET');
    FHeaderSB.Append(SLLineSeparator);
    FHeaderSB.Append('  ');
    // Append FFldSB
    FMiddleSB.Append(SLLineSeparator);
    FMiddleSB.Append('WHERE');
    FMiddleSB.Append(SLLineSeparator);
    FMiddleSB.Append('  ');
    // Append FFldParamSB
    GenerateConditions(FCondSB, stUpdate, ModifiedFieldsOnly, KeyAndDataFields, Index);
  end;
end;

procedure TDASQLGenerator.GenerateDeleteSQL(
  const KeyAndDataFields: TKeyAndDataFields;
  const ModifiedFieldsOnly: boolean;
  const Index: integer = -1);
begin
  FHeaderSB.Append('DELETE FROM ');
  FHeaderSB.Append(SQLInfo.NormalizeName(FTableInfo.TableNameFull, FDataSet.FOptions.FQuoteNames));
  FHeaderSB.Append(SLLineSeparator);
  FHeaderSB.Append('WHERE');
  FHeaderSB.Append(SLLineSeparator);
  FHeaderSB.Append('  ');
  GenerateConditions(FCondSB, stDelete, ModifiedFieldsOnly, KeyAndDataFields, Index);
end;
 
procedure TDASQLGenerator.GenerateLockSQL(
  const KeyAndDataFields: TKeyAndDataFields;
  const Index: integer = -1);
begin
  
end;

procedure TDASQLGenerator.AddFieldToRefreshSQL(FieldDesc: TCRFieldDesc);
begin
  if FFldSB.Length > 0 then
    FFldSB.Append(', ');

  FFldSB.Append(FieldDesc.ActualNameQuoted(SQLInfo, FDataSet.Options.QuoteNames));

  if not (csDesigning in FDataSet.ComponentState) then
    FFldSB.Append(' AS ' + GenerateIndexName(IntToStr(FDataSet.FIRecordSet.Fields.IndexOf(FieldDesc))));
end;

procedure TDASQLGenerator.GenerateRefreshSQLSelectPart(const KeyAndDataFields: TKeyAndDataFields);
var
  i: integer;
  FieldArrHigh: integer;
  UseDataFields: boolean;
  FieldDesc: TCRFieldDesc;
begin
  FHeaderSB.Append('SELECT ');

  UseDataFields := Length(KeyAndDataFields.DataFieldDescs) > 0;
  if UseDataFields then
    FieldArrHigh := Length(KeyAndDataFields.DataFieldDescs) - 1
  else
    FieldArrHigh := High(KeyAndDataFields.KeyFieldDescs);

  // SELECT ... FROM .... {WITH NOLOCK}
  // Add SELECT section
  for i := 0 to FieldArrHigh do begin
    if UseDataFields then
      FieldDesc := KeyAndDataFields.DataFieldDescs[i]
    else
      FieldDesc := KeyAndDataFields.KeyFieldDescs[i];

    AddFieldToRefreshSQL(FieldDesc);
  end;
end;

procedure TDASQLGenerator.GenerateRefreshSQLFromPart;
begin
  FMiddleSB.Append(SQLInfo.NormalizeName(FTableInfo.TableNameFull, FDataSet.FOptions.FQuoteNames));
  if FTableInfo.TableAlias <> '' then begin
    FMiddleSB.Append(' ');
    FMiddleSB.Append(SQLInfo.NormalizeName(FTableInfo.TableAlias, FDataSet.FOptions.FQuoteNames));
  end;
end;

procedure TDASQLGenerator.GenerateRefreshSQL(
  const KeyAndDataFields: TKeyAndDataFields;
  const ModifiedFieldsOnly: boolean);
var
  SelectSQL: _string;
begin
  GenerateConditions(FCondSB, stRefresh, ModifiedFieldsOnly, KeyAndDataFields);
  if FDataSet.Options.FullRefresh or FDataSet.ReadOnly then begin
    if FCondSB.Length = 0 then begin
      SelectSQL := FDataSet.FinalSQL;
      FHeaderSB.Append(SelectSQL);
    end
    else begin
      SelectSQL := FDataSet.BaseSQL;
      FHeaderSB.Append(FDataSet.SQLAddWhere(SelectSQL, SLLineSeparator + '  ' + FCondSB.ToString));
      FCondSB.Length := 0; // WHERE clause already added to FHeaderSB
    end;
  end
  else begin
    GenerateRefreshSQLSelectPart(KeyAndDataFields);
    FMiddleSB.Append(' FROM ');
    GenerateRefreshSQLFromPart;
    FMiddleSB.Append(SLLineSeparator);
    FMiddleSB.Append('WHERE');
    FMiddleSB.Append(SLLineSeparator);
    FMiddleSB.Append('  ');
  end;
end;

procedure TDASQLGenerator.GenerateRefreshQuickSQL(const KeyAndDataFields: TKeyAndDataFields);
begin
  GenerateConditions(FCondSB, stRefreshQuick, False{ignored}, KeyAndDataFields);
  FHeaderSB.Append(FDataSet.SQLAddWhere(FDataSet.FinalSQL, FCondSB.ToString));
  FCondSB.Length := 0; // WHERE clause already added to FHeaderSB
end;

procedure TDASQLGenerator.GenerateRefreshCheckDeletedSQL(const KeyAndDataFields: TKeyAndDataFields);
var
  i: integer;
  FieldDesc: TCRFieldDesc;
  ActualFieldName: _string;
  Condition, FromClause: _string;
  FinalSQL: _string;
begin
  FHeaderSB.Append('SELECT ');

  for i:= 0 to Length(KeyAndDataFields.KeyFieldDescs) - 1 do begin
    FieldDesc := KeyAndDataFields.KeyFieldDescs[i];
    if i > 0 then begin
      FFldSB.Append(', ');
      FFooterSB.Append(', ');
    end;
    ActualFieldName := GetActualFieldName(FieldDesc, True);
    FFldSB.Append(ActualFieldName);
    FFooterSB.Append(ActualFieldName);
    if not _SameText(FieldDesc.ActualName, FieldDesc.Name) then
      FFldSB.Append(' AS ' + FDataSetService.QuoteName(FieldDesc.Name));
  end;

  FinalSQL := FDataSet.FinalSQL;
  FromClause := FDataSet.SQLGetFrom(FinalSQL);
  FMiddleSB.Append(' FROM ' + FromClause);
  Condition := FDataSet.SQLGetWhere(FinalSQL);
  if Condition <> '' then
    FMiddleSB.Append(' WHERE ' + Condition);
  FMiddleSB.Append(' ORDER BY ');
end;

function TDASQLGenerator.GenerateSQLforUpdTable(TableInfo: TCRTableInfo;
  const KeyAndDataFields: TKeyAndDataFields;
  const StatementType: TStatementType;
  const ModifiedFieldsOnly: boolean;
  Params: TDAParams;
  const Index: integer = -1): _string;
begin
  if TableInfo.TableName = '' then
    DatabaseError(SBadTableInfoName);

  Clear;

  FTableInfo := TableInfo;
  FParams := Params;
  if FParams <> nil then begin
    FParams.BeginUpdate;
    FParams.Clear;
    FParamsInfo.Clear;
  end;
  try
    case StatementType of
      stInsert:
        GenerateInsertSQL(KeyAndDataFields, ModifiedFieldsOnly, Index);
      stUpdate:
        GenerateUpdateSQL(KeyAndDataFields, ModifiedFieldsOnly, Index);
      stDelete:
        GenerateDeleteSQL(KeyAndDataFields, ModifiedFieldsOnly, Index);
      stLock:
        GenerateLockSQL(KeyAndDataFields, Index);
      stRefresh:
        GenerateRefreshSQL(KeyAndDataFields, ModifiedFieldsOnly);
      stRefreshQuick:
        GenerateRefreshQuickSQL(KeyAndDataFields);
      stRefreshCheckDeleted:
        GenerateRefreshCheckDeletedSQL(KeyAndDataFields);
    else
      DatabaseError(SBadStatementType);
    end;

    Result := AssembleSB(StatementType);
  finally
    if FParams <> nil then begin
      FParams.EndUpdate;
      RecreateParamsRef(FParams);
    end;
  end;
end;

function TDASQLGenerator.GenerateSQL(const StatementType: TStatementType;
  const ModifiedFieldsOnly: boolean;
  Params: TDAParams;
  const Index: Integer = -1): _string;
var
  KeyAndDataFields: TKeyAndDataFields;
  ForceUseAllFields: boolean;
begin
{$IFDEF PERF_COUNTER}
  PerfCounters[4].Start;
{$ENDIF}
  if FDataSetService.FUpdatingTableInfoIdx = -1 then
    Result := ''
  else
  begin
    ForceUseAllFields := (StatementType = stRefresh) and FDataSet.Options.FullRefresh and
      (FDataSet.TablesInfo.Count <> 1);
    FDataSetService.GetKeyAndDataFields(KeyAndDataFields, ForceUseAllFields);
    Result := GenerateSQLforUpdTable(FDataSetService.UpdatingTableInfo,
      KeyAndDataFields, StatementType, ModifiedFieldsOnly, Params, Index);
  end;
{$IFDEF PERF_COUNTER}
  PerfCounters[4].Stop;
{$ENDIF}
end;

function TDASQLGenerator.GenerateTableSQL(const TableName, OrderFields: _string): _string;
var
  AOrderFields: _string;
begin
  FDataSet.CheckIRecordSet;
  Result := 'SELECT * FROM ' + SQLInfo.NormalizeName(TableName, FDataSet.FOptions.FQuoteNames) + LineSeparator;

  AOrderFields := Trim (OrderFields);
  if AOrderFields <> '' then
    Result := Result + 'ORDER BY ' + AOrderFields;

  Result := Result + LineSeparator;
end;

function TDASQLGenerator.GenerateSelectValues(const ValuesList: _string): _string;
begin
  Result := 'SELECT ' + ValuesList;
end;

{ TDADataSetUpdater }

constructor TDADataSetUpdater.Create(AOwner: TDataSetService);
begin
  FDataSetService := TDADataSetService(AOwner);
  Assert(FDataSetService <> nil);
  FDataSet := TCustomDADataSet(FDataSetService.FDataSet);

  inherited Create(AOwner);

  FBatchSQLs := _StringBuilder.Create(100);
  FCheckOnLock := True;
end;

destructor TDADataSetUpdater.Destroy;
var
  stIdx: TStatementType;
begin
  FBatchParams.Free;
  FBatchSQLs.Free;

  for stIdx := Low(FUpdateComponents) to High(FUpdateComponents) do begin
    FUpdateComponents[stIdx].Free;
    FUpdateComponents[stIdx] := nil;
  end;

  inherited;
end;

procedure TDADataSetUpdater.SetIdentityFieldValue();
var
  Value: variant;
  OldReadOnly: boolean;
  Field: TField;
  RecBuf: TRecordBuffer;
begin
  if (FDataSetService.FIdentityField <> nil) and GetIdentityFieldValue(Value) then begin
    Field := FDataSet.GetField(FDataSetService.FIdentityField);
    if Field = nil then begin
      RecBuf := FDataSet.GetNewRecBuf;
      if RecBuf <> nil then
        FDataSet.Data.PutFieldAsVariant(FDataSetService.FIdentityField.FieldNo, RecBuf, Value);
    end
    else begin
      OldReadOnly := Field.ReadOnly;
      try
        FDataSet.SetTempState(FDataSet.State); // DisableControls
        Field.ReadOnly := False;
        Field.NewValue := Value;
      finally
        Field.ReadOnly := OldReadOnly;
        FDataSet.RestoreState(FDataSet.State);
      end;
    end;
  end;
end;

function TDADataSetUpdater.GetIdentityFieldValue(var Value: variant): boolean;
begin
  Result := False;
end;

function TDADataSetUpdater.GetSavepointName(CachedUpdates: boolean): _string;
const
  LockPrefix = 'LOCK_';
  LockCachedUpdatesPrefix = 'LOCK_CU_';
  MaxLen = 30; // 30 - max length of Oracle ident
begin
  if CachedUpdates then
    Result := LockCachedUpdatesPrefix + FDataSet.Name
  else
    Result := LockPrefix + FDataSet.Name;

  if Length(Result) > MaxLen then
    Result := Copy(Result, 1, MaxLen);
end;

function TDADataSetUpdater.GetLockSavepointName: _string;
begin
  Result := GetSavepointName(False);
end;

function TDADataSetUpdater.GetLockCachedUpdatesSavepointName: _string;
begin
  Result := GetSavepointName(True);
end;

function TDADataSetUpdater.SavepointAllowed: boolean;
begin
  Result := True;
end;

procedure TDADataSetUpdater.SetSavepoint(SavepointName: _string; CachedUpdates: boolean);
var
  UpdTransaction: TDATransaction;
begin
  UpdTransaction := UsedUpdateTransaction;
  if not UpdTransaction.Active then begin
    UpdTransaction.StartTransaction;
    if CachedUpdates then
      FLockTrStarted := ltsOnLockCachedUpdates
    else
      FLockTrStarted := ltsOnLock;
  end
  else begin
    if SavepointAllowed then
      UpdTransaction.DoSavepoint(SavepointName);
    if not FDataSet.CachedUpdates then
      FLockTrStarted := ltsNone
    else if CachedUpdates then
      FLockTrStarted := ltsBeforeLockCachedUpdates;  
  end;

  FFixedTransaction := UpdTransaction;
end;

procedure TDADataSetUpdater.SetLockSavepoint;
begin
  SetSavepoint(GetLockSavepointName, False);
end;

procedure TDADataSetUpdater.SetLockCachedUpdatesSavepoint;
begin
  SetSavepoint(GetLockCachedUpdatesSavepointName, True);
end;

procedure TDADataSetUpdater.RollbackToSavepoint(SavepointName: _string; CachedUpdates: boolean);
begin
  if FFixedTransaction = nil then
    exit;
  try
    if not CachedUpdates and (FLockTrStarted = ltsOnLock) then begin
      FFixedTransaction.Rollback;
      FLockTrStarted := ltsNone;
    end
    else if CachedUpdates and (FLockTrStarted = ltsOnLockCachedUpdates) then begin
      FFixedTransaction.Rollback;
      FLockTrStarted := ltsNone;
    end
    else begin
      if SavepointAllowed then begin
        FFixedTransaction.DoRollbackToSavepoint(SavepointName);
        if CachedUpdates and (FLockTrStarted = ltsBeforeLockCachedUpdates) then
          FLockTrStarted := ltsNone;
      end
      else begin
        if UsedConnection.IsMultipleTransactionsSupported then
          FFixedTransaction.DoRollbackRetaining
        else
          FFixedTransaction.Rollback;
        FLockTrStarted := ltsNone;
      end;
    end;
  except  // WAR for COMMIT after Lock call
  end;

  if not FDataSet.CachedUpdates or (FLockTrStarted = ltsNone) then
    FFixedTransaction := nil;
end;

procedure TDADataSetUpdater.RollbackToLockSavepoint;
begin
  RollbackToSavepoint(GetLockSavepointName, False);
end;

procedure TDADataSetUpdater.RollbackToLockCachedUpdatesSavepoint;
begin
  RollbackToSavepoint(GetLockCachedUpdatesSavepointName, True);
end;

procedure TDADataSetUpdater.ResetLockCachedUpdatesSavepoint;
begin
  // Reset transaction info on ApplyChanges
  FFixedTransaction := nil;
  FLockTrStarted := ltsNone;
end;

function TDADataSetUpdater.IsNeedInsertPreconnect: boolean;
begin
  Result := False;
end;

function TDADataSetUpdater.IsNeedEditPreconnect: boolean;
begin
  Result := False;
end;

function TDADataSetUpdater.IsPreconnected: boolean;
begin
  Result := ((FDataSet.State = dsEdit) and IsNeedEditPreconnect) or
    ((FDataSet.State = dsInsert) and IsNeedInsertPreconnect);
end;

function TDADataSetUpdater.CanRefreshByLock: boolean;
var
  SQLLock: _string;
  SQLRefresh: _string;
begin
  SQLLock := GetUpdateStatement(stLock);
  SQLRefresh := GetUpdateStatement(stRefresh);
  Result := not (  ((SQLLock <> '') and (Pos('FOR', _UpperCase(SQLLock)) <> 1)) or
   (roBeforeEdit in FDataSet.RefreshOptions) and ((SQLRefresh <> '') or FDataSetService.IsFullRefresh)  );
end;

function TDADataSetUpdater.FieldByParamName(var ParamName: _string; var Old: boolean; var AFieldNo: integer): TField;
// Returns field that corresponds to ParamName
  function FindFieldByFieldNo(FieldNo: integer): TField;
  var
    i: integer;
  begin
    for i := 0 to FDataSet.Fields.Count - 1 do begin
      Result := FDataSet.Fields[i];
      if Result.FieldNo = FieldNo then
        Exit;
    end;
    Result := nil;
  end;

var
  e: integer;
begin
  Old := CompareText(Copy(ParamName, 1, 4), 'OLD_') = 0;
  Result := nil;
  if Old then begin
    Result := FDataSet.FindField(ParamName);
    if Result <> nil then
      Old := False // fieldname is starting with OLD_
    else
      ParamName := Copy(ParamName, 5, Length(ParamName) - 4);
  end;

  if Result = nil then begin
    if AnsiChar(ParamName[1]) in ['0'..'9'] then
      Val(ParamName, AFieldNo, e)
    else
      e := 1;
    if e = 0 then
      Result := FindFieldByFieldNo(AFieldNo)
    else
      AFieldNo := -1;
  end
  else begin
    AFieldNo := -1;
  end;

  if Result = nil then
    Result := FDataSet.FindField(ParamName);
end;

function TDADataSetUpdater.GetUpdateStatement(const StatementType: TStatementType): _string;
var
  UpdateSQL: _TStrings;
  SelectSQL: _string;
begin
  UpdateSQL := FDataSet.FUpdateSQL[StatementType];
  if UpdateSQL = nil then
    Result := ''
  else
  begin
    Result := UpdateSQL.Text;
    if StatementType = stRefresh then begin
      Result := Trim(Result);
      if Pos('WHERE', _UpperCase(Result)) = 1 then begin
        if SQLGeneratorCompatibility then
          SelectSQL := FDataSet.BaseSQL
        else
          SelectSQL := FDataSet.FinalSQL;
        Result := FDataSet.SQLAddWhere(SelectSQL, Trim(Copy(Result, 6, Length(Result))));
      end;
    end
    else
    if StatementType = stLock then begin
      Result := Trim(Result);
      if Pos('FOR', _UpperCase(Result)) = 1 then
        Result := GetUpdateStatement(stRefresh) + LineSeparator + Result;
    end;
  end;
end;

procedure TDADataSetUpdater.CheckUpdateQuery(const StatementType: TStatementType);

  function GetTransactionForUpdate: TDATransaction;
  begin
    if FFixedTransaction <> nil then
      Result := FFixedTransaction
    else
    // Refresh uses update transaction if it is active. Otherwise it uses read transaction
    if ((StatementType = stCustom) or // used in information queries
      (StatementType = stRefresh)) and
      ((FDataSet.FUpdateTransaction = nil) or not (FDataSet.FUpdateTransaction.Active))
    then
      Result := UsedTransaction
    else
      Result := UsedUpdateTransaction;
  end;

begin
  FUpdateQuery := FUpdateComponents[StatementType];
  if FUpdateQuery = nil then begin
    Assert(UsedConnection <> nil);
    FUpdateQuery := UsedConnection.CreateDataSet;
  end
  else
    if IsClass(FUpdateQuery, TCustomDADataSet) then
      TCustomDADataSet(FUpdateQuery).Connection := UsedConnection
    else
    if FUpdateQuery is TCustomDASQL then
      TCustomDASQL(FUpdateQuery).Connection := UsedConnection
    else
      Assert(False, 'FUpdateQuery is ' + FUpdateQuery.ClassName);

  TDBAccessUtils.SetLockDebug(FUpdateQuery, True);

  if FUpdateQuery is TCustomDADataSet then begin
    TCustomDADataSet(FUpdateQuery).Close; // To prevent exception raising on setting properties
    TCustomDADataSet(FUpdateQuery).Transaction := GetTransactionForUpdate;
    TCustomDADataSet(FUpdateQuery).CheckIRecordSet;
    TCustomDADataSet(FUpdateQuery).CheckDataSetService;
    TCustomDADataSet(FUpdateQuery).FIRecordSet.Component := FUpdateQuery;

    if StatementType = stBatchUpdate then begin
      TCustomDADataSet(FUpdateQuery).ParamCheck := False;
      TCustomDADataSet(FUpdateQuery).Params.Clear;
    end;

    TCustomDADataSet(FUpdateQuery).ReadOnly := True;
    TCustomDADataSet(FUpdateQuery).Options.NumberRange := False;
    TCustomDADataSet(FUpdateQuery).Options.TrimFixedChar := FDataSet.Options.TrimFixedChar;
    TCustomDADataSet(FUpdateQuery).Options.TrimVarChar := FDataSet.Options.TrimVarChar;
    TCustomDADataSet(FUpdateQuery).Options.SetEmptyStrToNull := FDataSet.Options.SetEmptyStrToNull;
    TCustomDADataSet(FUpdateQuery).Options.FlatBuffers := True;
    TCustomDADataSet(FUpdateQuery).Options.FullRefresh := FDataSet.Options.FullRefresh;
  {$IFDEF HAVE_COMPRESS}
    TCustomDADataSet(FUpdateQuery).Options.CompressBlobMode := FDataSet.Options.CompressBlobMode;
  {$ENDIF}
    if StatementType in [stRefresh, stRefreshQuick, stRefreshCheckDeleted] then
      TCustomDADataSet(FUpdateQuery).Options.LongStrings := FDataSet.Options.LongStrings;

    SetUpdateQueryOptions(StatementType);
  end
  else
  if FUpdateQuery is TCustomDASQL then begin
    TCustomDASQL(FUpdateQuery).Transaction := GetTransactionForUpdate;
  end;

  FUpdateComponents[StatementType] := FUpdateQuery;
end;

procedure TDADataSetUpdater.SetUpdateQueryOptions(const StatementType: TStatementType);
begin
end;

procedure TDADataSetUpdater.CheckUpdateSQL(const SQL: _string;
  const StatementTypes: TStatementTypes; out ParamsInfo: TDAParamsInfo;
  UseGenerator: boolean = True);
var
  NewSQL, OldSQL: _string;
  StTypes: TStatementTypes;
  StatementType: TStatementType;
  stIdx: TStatementType;
  CheckSQLNeeded: boolean;
  UpdateObjectSQL: _TStrings;
  IsSQLAutoGenerated: boolean;
  Index: Integer;
  LockMacros, LockScanParams: boolean;
  UQParams: TDAParams;
begin
  ParamsInfo := nil;
  IsSQLAutoGenerated := False;
  CheckSQLNeeded := True;
  StTypes := StatementTypes;
  if (stLock in StTypes) and (stRefresh in StTypes) then
    Exclude(StTypes, stRefresh);
  StatementType := stCustom;
  for stIdx := Low(TStatementType) to High(TStatementType) do
    if stIdx in StTypes then begin
      if StatementType <> stCustom then
        StatementType := stCustom
      else
        StatementType := stIdx;
      if StatementType = stCustom then
        Break;
    end;

  FUpdateQuery := nil;
  try
    if GetUpdateObject <> nil then begin
      FUpdateQuery := GetUpdateObject.GetObjectIndex(Ord(StatementType));
      if FUpdateQuery <> nil then begin
        CheckSQLNeeded := False;
        if not ((FUpdateQuery is TCustomDADataSet) and TCustomDADataSet(FUpdateQuery).SQLAutoGenerated) and
          (TDBAccessUtils.GetSQL(FUpdateQuery).Count = 0)
        then
          DatabaseError(SUpdateObjectEmptySQL);
      end;
    end;
    if FUpdateQuery = nil then
      CheckUpdateQuery(StatementType);
  finally
    Assert(FUpdateQuery <> nil, 'FUpdateQuery = nil'{$IFNDEF FPC} + '. StatementTypes = ' + IntToStr(Word(StatementTypes)){$ENDIF});
  end;

  if CheckSQLNeeded then begin
    TDBAccessUtils.SetAutoCommit(FUpdateQuery,
      (StatementTypes * [stInsert, stUpdate, stDelete, stBatchUpdate] <> []) and FDataSetService.IsAutoCommit);

    NewSQL := SQL;
    if StatementType = stBatchUpdate then
      NewSQL := PrepareBatch(FBatchSQLs.ToString)
    else
      if NewSQL = '' then begin
        if GetUpdateObject <> nil then
          UpdateObjectSQL := GetUpdateObject.GetSQLIndex(Ord(StatementType))
        else
          UpdateObjectSQL := nil;

        if Assigned(UpdateObjectSQL) then
          NewSQL := UpdateObjectSQL.Text;

        if NewSQL = '' then begin
          NewSQL := GetUpdateStatement(StatementType);
          if (NewSQL <> '') and (FDataSet.Macros.Count > 0) then
            FDataSet.Macros.Expand(NewSQL);

          if (NewSQL = '') and UseGenerator then begin
            IsSQLAutoGenerated := True;
            if BatchUpdate then
              Index := FBatchStatements
            else
              Index := -1;

            if ((StatementType = stRefreshCheckDeleted) or (StatementType = stRefreshQuick))
              and (
                (FDataSet.FilterSQL <> '')
                or ((FDataSet.DataSource <> nil) and (FDataSet.MasterFields <> '') and
                (FDataSet.DetailFields <> '') and not FDataSet.Options.LocalMasterDetail)
              )
            then /// CR S22583 (RefreshQuick + MD)
              UQParams := nil
            else
              UQParams := TDBAccessUtils.GetParams(FUpdateQuery);
            NewSQL := FDataSetService.FSQLGenerator.GenerateSQL(StatementType,
              not (csDesigning in FDataSet.ComponentState) and not
              FDataSet.Options.UpdateAllFields, UQParams, Index);
            case StatementType of
              stInsert, stUpdate, stDelete:
                LockMacros := True;
              else
                LockMacros := False;
            end;
            if UQParams = nil then
              LockScanParams := False
            else
              LockScanParams := not (StatementType in [stRefreshQuick, stRefreshCheckDeleted]) and
                 not (((FDataSet.Params.Count > 0) or FDataSetService.IsFullRefresh) and
                 (StatementType = stRefresh));
            if (TDBAccessUtils.GetSQLText(FUpdateQuery) <> NewSQL) or
              not (FDataSet.Options.PrepareUpdateSQL and not UsedConnection.Options.DisconnectedMode) then
              TDBAccessUtils.SetSQLText(FUpdateQuery, NewSQL,
                LockScanParams,
                LockMacros);
            ParamsInfo := FDataSetService.FSQLGenerator.ParamsInfo;
          end;
        end;
      end;

    // Check whether SQL text is the same. For multiple update operations.
    if BatchUpdate and not ((StatementType = stBatchUpdate) or (StatementType = stLock) or (StatementType = stRefresh)) then begin
      if not IsSQLAutoGenerated then
         NewSQL := GetICommand.ParseSQL(NewSQL, nil, True, ':' + FDataSetService.FSQLGenerator.IndexedPrefix + IntToStr(FBatchStatements) + '_');
      if NewSQL <> '' then begin
        if FBatchSQLs.Length <> 0 then
          FBatchSQLs.Append(#13#10);
        FBatchSQLs.Append(NewSQL);
        FBatchSQLs.Append(';');
        inc(FBatchStatements);
      end;
    end;

    Assert(FUpdateQuery <> nil);
    if not IsSQLAutoGenerated then begin
      OldSQL := TDBAccessUtils.GetSQL(FUpdateQuery).Text;
      NewSQL := NewSQL;
      if OldSQL <> NewSQL then begin
        TDBAccessUtils.GetParams(FUpdateQuery).Clear; /// Performance optimization - skipping reassigning old params values on changing SQL
        TDBAccessUtils.GetSQL(FUpdateQuery).Text := NewSQL;
      end;
    end;

    //Used user defined SQL and internal update object used so we could prepare Update object to obtain some performance gain
    if FDataSet.Options.PrepareUpdateSQL and not UsedConnection.Options.DisconnectedMode and not BatchUpdate then begin
      if FUpdateQuery is TCustomDADataSet then
        TCustomDADataSet(FUpdateQuery).Options.AutoPrepare := True
      else
      if FUpdateQuery is TCustomDASQL then
        TCustomDASQL(FUpdateQuery).Prepared := True;
    end;
  end;
end;

/// UpdateExecute performes execute of the UpdateQuery. We need this procedure
/// to get two update models: with and without explicit prepare.
procedure TDADataSetUpdater.UpdateExecute(const StatementTypes: TStatementTypes);
var
  MessageID: cardinal;
  i: integer;
  St: string;
begin
  if not FDataSet.FLockDebug and (TDASQLMonitorClass(UsedConnection.SQLMonitorClass).HasMonitor or FDataSet.Debug) then begin
    St := '';
    for i := 0 to Integer(High(TStatementType)) do
      if TStatementType(i) in StatementTypes then begin
        if St <> '' then
          St := St + ',';
        St := St + Copy(GetEnumName(TypeInfo(TStatementType), i), 3,
          Length(GetEnumName(TypeInfo(TStatementType), i)));
      end;
    TDASQLMonitorClass(UsedConnection.SQLMonitorClass).SQLExecute(FDataSet, TDBAccessUtils.GetSQL(FUpdateQuery).Text, TDBAccessUtils.GetParams(FUpdateQuery), St, MessageID, True);
  end;

  TDBAccessUtils.Execute(FUpdateQuery);

  if not FDataSet.FLockDebug and (TDASQLMonitorClass(FDataSet.UsedConnection.SQLMonitorClass).HasMonitor or FDataSet.Debug) then
    TDASQLMonitorClass(FDataSet.UsedConnection.SQLMonitorClass).SQLExecute(FDataSet, TDBAccessUtils.GetSQL(FUpdateQuery).Text, TDBAccessUtils.GetParams(FUpdateQuery), St, MessageID, False);
end;

function TDADataSetUpdater.LockCompare(const Value1, Value2: variant): boolean;
var
  DValue1, DValue2: Double;
begin
 Result := VarEqual(Value1, Value2);

 if not Result and (VarType(Value1) = VarType(Value2)) and
   ((VarType(Value1) = varDate) {$IFDEF VER6P}{$IFNDEF FPC} or (VarType(Value1) = VarSQLTimeStamp){$ENDIF}{$ENDIF})
 then begin
   DValue1 := Value1;
   DValue2 := Value2;

   if Abs(DValue1 - DValue2) < DateTimeFilterDelta then  // 1 millisecond
     Result := True;
 end;
end;

procedure TDADataSetUpdater.WriteUQParams(ParamsInfo: TDAParamsInfo; const StatementTypes: TStatementTypes);

  function FindFieldDescByFieldNo(FieldNo: integer): TFieldDesc;
  var
    i: integer;
  begin
    for i := 0 to GetIRecordSet.Fields.Count - 1 do begin
      Result := GetIRecordSet.Fields[i];
      if Result.FieldNo = FieldNo then
        Exit;
    end;
    Result := nil;
  end;

  function SuppressBatchPrefix(Value: _string): _string;
  var
    i,e: integer;
  begin
    Result := Value;
    if BatchUpdate then begin
      i := Pos(FDataSetService.FSQLGenerator.IndexedPrefix, Value);
      if i = 1 then begin // if parameter name starts from "P_"
        e := i + 2;
        while (e <= Length(Result)) and (Result[e] <> '_') do
          inc(e);

        {$IFDEF CLR}Borland.Delphi.{$ENDIF}System.Delete(Result, i, e - i + 1);
      end;
    end;
  end;

  procedure AssignFieldValueEx(Param: TDAParam; Field: TField; Old: boolean);
  begin
    if UseParamType and
      ((Param.ParamType = ptOutput) or (Param.ParamType = ptResult)) then begin
      Param.DataType := Field.DataType;
      Param.Value := Null;
    end
    else
      FDataSet.AssignFieldValue(Param, Field, Old);
  end;

var
  i: integer;
  Param, Param1: TDAParam;
  ParamName: _string;
  Old: boolean;
  Field: TField;
  AFieldNo: integer;
  AFieldDesc: TFieldDesc;
  Params: TDAParams;
  LowIndex: integer;
  ParamInfoIndex: integer;
begin
{$IFDEF PERF_COUNTER}
  PerfCounters[5].Start;
{$ENDIF}

  // assigning parameter values from fields of the same name
  if BatchUpdate then begin
    // copy parameters from UpdateQuery to common collection
    if FBatchParams = nil then begin
      FBatchParams := FDataSet.FCommand.CreateParamsObject;
      FBatchParams.FOwner := nil;
    end;

    Params := TDBAccessUtils.GetParams(FUpdateQuery);
    FBatchParams.BeginUpdate;
    try
      for i := 0 to Params.Count - 1 do
        FBatchParams.CreateParam(Params[i].DataType, Params[i].Name, Params[i].ParamType);
    finally
      FBatchParams.FNeedsUpdateItem := False;
      FBatchParams.EndUpdate;
      RecreateParamsRef(FBatchParams);
      FBatchParams.FNeedsUpdateItem := True;
    end;

    Params := FBatchParams;
    LowIndex := Params.Count - TDBAccessUtils.GetParams(FUpdateQuery).Count;
  end
  else begin
    Params := TDBAccessUtils.GetParams(FUpdateQuery);
    LowIndex := 0;
  end;

  for i := LowIndex to Params.Count - 1 do begin
    Param := Params[i];
    AFieldNo := -1;
    ParamName := '';
    Field := nil;
    if BatchUpdate then
      ParamInfoIndex := i - LowIndex
    else if (stRefresh in StatementTypes) and (ParamsInfo <> nil) then
      ParamInfoIndex := i - (Params.Count - ParamsInfo.Count)
    else
      ParamInfoIndex := i;
    if (ParamsInfo <> nil) and ([stRefreshQuick, stRefreshCheckDeleted] * StatementTypes = []) and not ((ParamInfoIndex < 0) and (stRefresh in StatementTypes)) then begin
      Old := ParamsInfo.Items[ParamInfoIndex].Old;
      Field := ParamsInfo.Items[ParamInfoIndex].Field;
    end;

    if (ParamsInfo = nil) or (Field = nil) then begin
      // param name can be quoted if generated by SQL Generator
      ParamName := FDataSet.SQLInfo.UnQuote(Param.Name);
      // should remove additional index before assigning value from field
      ParamName := SuppressBatchPrefix(ParamName);
      if (FDataSet.MasterSource <> nil) and (FDataSet.MasterFields = '') and (FDataSet.DetailFields = '') and (FDataSet.Params.Count > 0) and (stRefresh in StatementTypes) then begin
        if FDataSet.FindParam(ParamName) = nil then
          Field := FieldByParamName(ParamName, Old, AFieldNo)
      end
      else
        Field := FieldByParamName(ParamName, Old, AFieldNo);
    end;

    if Field <> nil then
      AssignFieldValueEx(Param, Field, Old and (not ((FDataSet.State = dsInsert) and not FDataSet.FInDeferredPost))) // OldValue is Null on Insert
    else begin
      Assert(ParamName <> '');

      AFieldDesc := nil;
      if AFieldNo >= 0 then
        AFieldDesc := FindFieldDescByFieldNo(AFieldNo);
      if AFieldDesc = nil then
        AFieldDesc := GetIRecordSet.FindField(ParamName);
      if (AFieldDesc <> nil) and (AFieldDesc.DataType in [dtObject,dtArray,dtTable]) then // object fields in not objectview
        FDataSet.AssignFieldValue(Param, AFieldDesc, False)
      else begin
        Param1 := FDataSet.FindParam(ParamName);
        if (Param1 <> nil) and ([stRefresh, stRefreshQuick, stRefreshCheckDeleted] * StatementTypes <> []) then
          Param.Assign(Param1)  // assign param from param of SQL
        else
          if AFieldDesc <> nil then
            FDataSet.AssignFieldValue(Param, AFieldDesc, Old and (not ((FDataSet.State = dsInsert) and not FDataSet.FInDeferredPost)))
          else
          if not FDataSet.AssignedBeforeUpdateExecute then
            DatabaseError(Format(SNoCorrespondParam, [Param.Name]));
      end;
    end;
    if (([stRefreshQuick, stRefreshCheckDeleted] * StatementTypes) <> []) and
       FDataSet.IsMasterDatasetActive then
      FDataSet.SetMasterParams(Params);
    if not UseParamType then
      Param.ParamType := ptInput;
  end;
{$IFDEF PERF_COUNTER}
  PerfCounters[5].Stop;
{$ENDIF}
end;

function TDADataSetUpdater.PerformSQL(const SQL: _string; const StatementTypes: TStatementTypes): boolean;
  function FindFieldByFieldNo(FieldNo: integer): TField;
  var
    i: integer;
  begin
    for i := 0 to FDataSet.Fields.Count - 1 do begin
      Result := FDataSet.Fields[i];
      if Result.FieldNo = FieldNo then
        Exit;
    end;
    Result := nil;
  end;

  procedure ReadUQParams(RecordSet: TCRRecordSet; Buffer: TValueBuffer; ParamsInfo: TDAParamsInfo);
  var
    i: integer;
    Param: TDAParam;
    ParamName: _string;
    Old: boolean;
    Field: TField;
    AFieldNo: integer;
    FieldDesc: TFieldDesc;
    RecBuf: TRecordBuffer;
    IsBlank: boolean;
    SharedObject: TSharedObject;
    ReadOnly: boolean;

  begin
  {$IFDEF PERF_COUNTER}
    PerfCounters[5].Start;
  {$ENDIF}
    for i := 0 to TDBAccessUtils.GetParams(FUpdateQuery).Count - 1 do begin
      Param := TDBAccessUtils.GetParams(FUpdateQuery)[i];
      if UseParamType then
        if Param.ParamType < ptOutput then
          Continue;

      if ParamsInfo <> nil then begin
        Old := ParamsInfo.Items[i].Old;
        Field := ParamsInfo.Items[i].Field;
      end
      else
      begin
        ParamName := Param.Name;
        Field := FieldByParamName(ParamName, Old, AFieldNo);
      end;

      if Assigned(Field) and not Old and FDataSet.CanRefreshField(Field)
      then begin
        ReadOnly := Field.ReadOnly;
        if Field.ReadOnly then begin
          FDataSet.SetTempState(FDataSet.State); // DisableControls
          Field.ReadOnly := False;
        end;

        FieldDesc := GetIRecordSet.Fields[Field.FieldNo - 1];

        if (Param.ParamObject <> nil) and (RecordSet <> nil)
          and (RecordSet.IsComplexFieldType(FieldDesc.DataType)
          and not((FieldDesc.DataType = dtExtString)
          or (FieldDesc.DataType = dtExtWideString)
          or (FieldDesc.DataType = dtExtVarBytes)
          {$IFDEF VER5P}or (FieldDesc.DataType = dtVariant){$ENDIF}))
        then begin
          // pass SharedObject from parameter to recordset
          if FDataSet.FInCacheProcessing then
            RecBuf := GetIRecordSet.NewCacheRecBuf
          else
            RecBuf := FDataSet.ActiveBuffer;
          GetIRecordSet.GetField(FieldDesc.FieldNo, RecBuf, Buffer, IsBlank);
          SharedObject := TSharedObject(GetGCHandleTarget(Marshal.ReadIntPtr(Buffer)));
          if SharedObject <> nil then
            SharedObject.Free;

          SharedObject := Param.ParamObject;
          SharedObject.AddRef;

          if Param.IsNull then begin
            GetIRecordSet.PutField(FieldDesc.FieldNo, RecBuf, Buffer);
            GetIRecordSet.SetNull(FieldDesc.FieldNo, RecBuf, True);
          end
          else
            GetIRecordSet.PutField(FieldDesc.FieldNo, RecBuf, Buffer);

          if (FieldDesc.DataType in [dtBlob, dtMemo, dtWideMemo]) and
           (FDataSet.State in [dsInsert,dsEdit])
          then
            TBlob(SharedObject).EnableRollback;
        end
        else
          if Param.IsNull then
            Field.NewValue := Null
          else
            if not VarEqual(Field.Value, Param.Value) then
              Field.NewValue := Param.Value;

        if ReadOnly then begin
          Field.ReadOnly := True;
          FDataSet.RestoreState(FDataSet.State);
        end
      end;
    end;
  {$IFDEF PERF_COUNTER}
    PerfCounters[5].Stop;
  {$ENDIF}
  end;

  procedure CopyRecBuf(SrcRecordSet: TData; SrcRecBuf: TRecordBuffer; Buffer: TValueBuffer);
  var
    i: integer;
    RecBuf: TRecordBuffer;
    FieldDesc: TFieldDesc;
    FieldName: _string;
    FieldDescIdx: integer;
    AFieldDesc: TFieldDesc;
    RecordSetFieldNo: integer;
    IsBlank: boolean;
    SharedObject: TSharedObject;
    Value, NewValue: variant;
    Field: TField;
    UQFieldDesc: TFieldDesc;
    RecordSet: TCRRecordSet;
  begin
    if FDataSet.FInCacheProcessing then
      RecBuf := GetIRecordSet.NewCacheRecBuf
    else
      RecBuf := FDataSet.ActiveBuffer;

    RecordSet := FDataSet.FIRecordSet;

    for i := 0 to SrcRecordSet.FieldCount - 1 do
      if not SrcRecordSet.Fields[i].HasParent then begin
        FieldDesc := SrcRecordSet.Fields[i];
        FieldName := FieldDesc.Name;

        // FindField optimization
        AFieldDesc := nil;
        FieldDescIdx := FDataSetService.FSQLGenerator.DecodeFieldIndex(FieldName);
        if FieldDescIdx >= 0 then
          AFieldDesc := RecordSet.Fields[FieldDescIdx];

        if AFieldDesc = nil then
          AFieldDesc := RecordSet.FindField(FieldDesc.Name);
        if (AFieldDesc <> nil){ and CanRefreshField(Field) and}
        then
          if SrcRecordSet.IsComplexFieldType(FieldDesc.DataType)
            and not((FieldDesc.DataType = dtExtString)
            or (FieldDesc.DataType = dtExtWideString)
            or (FieldDesc.DataType = dtExtVarBytes)
            {$IFDEF VER5P}or (FieldDesc.DataType = dtVariant){$ENDIF})
          then begin
            if ([stRefresh, stRefreshQuick, stRefreshCheckDeleted] * StatementTypes) <> [] then begin
              RecordSetFieldNo := AFieldDesc.FieldNo;
              RecordSet.GetField(RecordSetFieldNo, RecBuf, Buffer, IsBlank);
              SharedObject := TSharedObject(GetGCHandleTarget(Marshal.ReadIntPtr(Buffer)));
              if SharedObject <> nil then
                SharedObject.Free;

              SrcRecordSet.GetField(FieldDesc.FieldNo, SrcRecBuf, Buffer, IsBlank);
              SharedObject := TSharedObject(GetGCHandleTarget(Marshal.ReadIntPtr(Buffer)));
              SharedObject.AddRef;
              RecordSet.PutField(RecordSetFieldNo, RecBuf, Buffer);

              if IsBlank then begin
                RecordSet.SetNull(RecordSetFieldNo, RecBuf, True);
              end;

              if (FieldDesc.DataType in [dtBlob, dtMemo, dtWideMemo]) and
               (FDataSet.State in [dsInsert,dsEdit])
              then
                TBlob(SharedObject).EnableRollback;
            end;
          end
        {$IFDEF VER5P}
          else
          if FieldDesc.DataType = dtVariant then begin
            RecordSetFieldNo := AFieldDesc.FieldNo;

            UQFieldDesc := RecordSet.Fields[RecordSetFieldNo - 1];
            CopyBuffer(PtrOffset(RecBuf, UQFieldDesc.Offset), Buffer, sizeof(TVariantObject));

            SharedObject := TSharedObject(GetGCHandleTarget(Marshal.ReadIntPtr(Buffer)));
            if SharedObject <> nil then
              SharedObject.Free;

            IsBlank := SrcRecordSet.GetNull(FieldDesc.FieldNo, SrcRecBuf);
            UQFieldDesc := SrcRecordSet.Fields[FieldDesc.FieldNo - 1];
            CopyBuffer(PtrOffset(SrcRecBuf, UQFieldDesc.Offset), Buffer, sizeof(TVariantObject));

            SharedObject := TSharedObject(GetGCHandleTarget(Marshal.ReadIntPtr(Buffer)));
            SharedObject.AddRef;

            UQFieldDesc := RecordSet.Fields[RecordSetFieldNo - 1];
            CopyBuffer(Buffer, PtrOffset(RecBuf, UQFieldDesc.Offset), sizeof(TVariantObject));
            RecordSet.SetNull(RecordSetFieldNo, RecBuf, IsBlank);
          end
        {$ENDIF}
          else begin
            SrcRecordSet.GetFieldAsVariant(FieldDesc.FieldNo,
              SrcRecBuf, NewValue);
            {This code raises AV with disposed ExtStrings
            if FieldDesc.DataType in [dtExtString, dtExtWideString, dtExtVarBytes] then
              SrcRecordSet.PutFieldData(FieldDesc, SrcRecBuf, nil);}
            Field := FindFieldByFieldNo(AFieldDesc.FieldNo);
            if Field <> nil then begin
              if (stLock in StatementTypes) and FCheckOnLock and
                not (roBeforeEdit in FDataSet.RefreshOptions)
              then begin
              // check
                if (FDataSet.State in dsEditModes) or FDataSet.CachedUpdates then
                  Value := Field.OldValue
                else
                  Value := Field.NewValue;

                if not LockCompare(Value, NewValue) then
                  DatabaseError(SRecordChanged);
              end
              else
                RecordSet.PutFieldAsVariant(AFieldDesc.FieldNo, RecBuf, NewValue, True)
            end;
          end;
      end;

    if (RecordSet.Fields.Count > 0) and (RecordSet.Fields[RecordSet.Fields.Count - 1].FieldDescKind = fdkCached) then
      FDataSet.DoGetCachedBuffer(RecBuf);

    if (([stRefresh, stRefreshQuick, stRefreshCheckDeleted] * StatementTypes) <> []) and
      not FDataSet.FInCacheProcessing
    then begin
      {if CachedUpdates then
        FIRecordSet.RevertRecord; }  /// ??? problem with one record

      // no need to store Active RecBuf when updating record
      if not FRefreshInUpdate then
        RecordSet.PutRecord(RecBuf);
      RecordSet.AddRefComplexFields(RecBuf);
      PRecInfo(PtrOffset(RecBuf, FDataSet.FRecInfoOfs)).RefComplexFields := True;
    end;
  end;

  procedure ReadUQFields(RecordSet: TCRRecordSet; Buffer: TValueBuffer);
  var
    RefreshRecBuf: TRecordBuffer;
  begin
    RecordSet.AllocRecBuf(IntPtr(RefreshRecBuf));
    try
      RecordSet.SetToBegin;  // temp
      RecordSet.GetNextRecord(RefreshRecBuf);
      if not RecordSet.EOF then
        CopyRecBuf(RecordSet, RefreshRecBuf, Buffer);
    finally
      GetIRecordSet.FreeRecBuf(RefreshRecBuf);
    end;
  end;

  procedure GetUQFields(const KeyFields: TFieldArray; out KeyFieldsUQ: TFieldArray);
  var
    i: integer;
  begin
    Assert(FUpdateQuery is TCustomDADataSet);
    SetLength(KeyFieldsUQ, Length(KeyFields));
    for i := 0 to Length(KeyFields) - 1 do
      KeyFieldsUQ[i] := TCustomDADataSet(FUpdateQuery).Fields.FindField(KeyFields[i].FieldName);
  end;

var
  i: integer;
  RecordSet: TCRRecordSet;
  RefreshRecBuf: TRecordBuffer;
  RecBuf: TRecordBuffer;
  Buffer: TValueBuffer;
  s, OldIndexFieldNames: _string;
  KeyFields, KeyFieldsUQ: TFieldArray;
  v: variant;
  RQFieldDesc: TFieldDesc;
  NewValue: variant;
  FieldDesc: TFieldDesc;

  OldOnAppend: TOnModifyRecord;
  ParamsInfo: TDAParamsInfo;
  TempParams: TDAParams;

begin
  BeginConnection;
  try
  {$IFDEF FPC}
    ParamsInfo := nil;
  {$ENDIF}
    CheckUpdateSQL(SQL, StatementTypes, ParamsInfo);

    if TDBAccessUtils.GetSQL(FUpdateQuery).Count = 0 then begin
      Result := False;
      Exit;
    end;

    if StatementTypes = [stBatchUpdate] then begin
      TempParams := TDBAccessUtils.GetParams(FUpdateQuery);
      TempParams.Assign(FBatchParams);
      RecreateParamsRef(TempParams);
    end
    else
      // assigning parameter values from fields of the same name
      WriteUQParams(ParamsInfo, StatementTypes);

    // No need to call events and UpdateExecute when collecting statemets
    if not BatchUpdate or (StatementTypes = [stBatchUpdate]) then begin
      TDBAccessUtils.GetParams(FUpdateQuery).FParamsChangeType := ctUserChecked; //to allow writing added parameters for UniDAC
      FDataSet.DoBeforeUpdateExecute(FDataSet, StatementTypes, TDBAccessUtils.GetParams(FUpdateQuery));
      if FDataSet.AssignedBeforeUpdateExecute then begin
        for i := 0 to TDBAccessUtils.GetParams(FUpdateQuery).Count - 1 do
          if TDBAccessUtils.GetParams(FUpdateQuery)[i].DataType = ftUnknown then
            DatabaseError(Format(SUnknownParamDataType, [TDBAccessUtils.GetParams(FUpdateQuery)[i].Name]));
      end;

      try
        UpdateExecute(StatementTypes);
      finally
        TDBAccessUtils.GetParams(FUpdateQuery).FParamsChangeType := ctGenerated;
      end;
    end;

    if FUpdateQuery is TCustomDADataSet then
      RecordSet := TCustomDADataSet(FUpdateQuery).FIRecordSet
    else
      RecordSet := nil;

    Buffer := Marshal.AllocHGlobal(sizeof(integer));
    try
      if (StatementTypes = [stRefreshCheckDeleted]) and (FUpdateQuery is TCustomDADataSet) then begin
        FDataSet.UpdateCursorPos;
        FDataSet.First;
        FDataSet.GetCurrentKeys(KeyFields, v);
        GetUQFields(KeyFields, KeyFieldsUQ);
        s := '';
        for i := Low(KeyFields) to High(KeyFields) do begin
          if s <> '' then
            s := s + ';';
          FieldDesc := FDataSet.GetFieldDesc(KeyFields[i]);
          s := s + FieldDesc.Name;
        end;

        OldIndexFieldNames := TCustomDADataSet(FUpdateQuery).IndexFieldNames;
        try
          TCustomDADataSet(FUpdateQuery).IndexFieldNames := s;
          while not FDataSet.Eof do begin
            FDataSet.GetCurrentKeys(KeyFields, v);
            if not TCustomDADataSet(FUpdateQuery).Locate(KeyFieldsUQ, v, []) then
              GetIRecordSet.RemoveRecord;
            FDataSet.Next;
          end;
        finally
          TCustomDADataSet(FUpdateQuery).Close;
          TCustomDADataSet(FUpdateQuery).IndexFieldNames := OldIndexFieldNames;
        end;
        FDataSet.Resync([]);
      end
      else
      if (([stRefreshQuick, stRefreshCheckDeleted] * StatementTypes) <> []) and (FUpdateQuery is TCustomDADataSet) then begin
      // Refresh from fields and check

        FDataSet.FRowsAffected := TCustomDADataSet(FUpdateQuery).RecordCount;

        FDataSet.UpdateCursorPos;
        RecordSet.AllocRecBuf(IntPtr(RefreshRecBuf));
        try
          TCustomDADataSet(FUpdateQuery).First;
          // Get key fields list from base DataSet
          FDataSet.GetCurrentKeys(KeyFields, v);
          GetUQFields(KeyFields, KeyFieldsUQ);
          while not TCustomDADataSet(FUpdateQuery).Eof do begin
            // And get values from FUpdateQuery
            v := VarArrayCreate([0, Length(KeyFieldsUQ) - 1], varVariant);
            for i := 0 to Length(KeyFieldsUQ) - 1 do
              v[i] := KeyFieldsUQ[i].Value;

            if not FDataSet.LocateEx(KeyFields, v, []) then begin
              FDataSet.FIRecordSet.AllocRecBuf(IntPtr(RecBuf));
              OldOnAppend := GetIRecordSet.OnAppend;
              try
                GetIRecordSet.OnAppend := nil;
                GetIRecordSet.InitRecord(RecBuf);
                GetIRecordSet.AppendRecord(RecBuf);
                FDataSet.Resync([]);
              finally
                GetIRecordSet.OnAppend := OldOnAppend;
                GetIRecordSet.FreeRecBuf(RecBuf);
              end;
            end;

            RecordSet.GetRecord(RefreshRecBuf);
            CopyRecBuf(RecordSet, RefreshRecBuf, Buffer);

            for i := 0 to RecordSet.Fields.Count - 1 do begin
              RQFieldDesc := RecordSet.Fields[i];
              if IsRefreshQuickField(RQFieldDesc) then begin
                RecordSet.GetFieldAsVariant(RQFieldDesc.FieldNo, RefreshRecBuf, NewValue);
                SaveMaxRefreshQuickValue(RQFieldDesc, NewValue);
              end;
            end;

            TCustomDADataSet(FUpdateQuery).Next;
          end;
        finally
          GetIRecordSet.FreeRecBuf(RefreshRecBuf);
        end;
        GetIRecordSet.SortItems;
        FDataSet.Resync([]);
      end
      else
      if ([stRefresh, stLock] * StatementTypes <> []) and (FUpdateQuery is TCustomDADataSet)
        and TCustomDADataSet(FUpdateQuery).IsQuery then begin
        // Refresh from fields and check
        FDataSet.FRowsAffected := TCustomDADataSet(FUpdateQuery).RecordCount;

        if FDataSet.Options.StrictUpdate and (FDataSet.FRowsAffected <> 1) then
          DatabaseError(Format(SRefreshFailed, [FDataSet.FRowsAffected]));

        if (stRefresh in StatementTypes) or
          ((stLock in StatementTypes) and FCheckOnLock and not (roBeforeEdit in FDataSet.RefreshOptions))
        then
          ReadUQFields(RecordSet, Buffer);
      end
      else
      // strict update and DMLRefresh don't work in BatchUpdate mode
      if not BatchUpdate then begin
        FDataSet.FRowsAffected := TDBAccessUtils.GetRowsAffected(FUpdateQuery);

        if FDataSet.Options.StrictUpdate and
          //(Command.SQLType in [SQL_INSERT,SQL_UPDATE,SQL_DELETE]) and  /// for ODAC
          ((FDataSet.FRowsAffected = 0) or (FDataSet.FRowsAffected > 1))
        then
          DatabaseError(Format(SUpdateFailed, [FDataSet.FRowsAffected]));

          // Refresh fields from params of the same name
          if NeedReturnParams or (stRefresh in StatementTypes) then // DML Refresh
            if RetunParamsAsFields then
              ReadUQFields(RecordSet, Buffer)  // PostgreSQL returning support
            else
              ReadUQParams(RecordSet, Buffer, ParamsInfo);
      end;

      // Lock statement can be UPDATE or SELECT FOR UPDATE for InterBase 
      if (stLock in StatementTypes) and UsedConnection.IsMultipleTransactionsSupported and
        (FFixedTransaction <> nil)
      then begin
        Assert(FUpdateQuery is TCustomDADataSet);
        if not TCustomDADataSet(FUpdateQuery).IsQuery then
          Dec(FFixedTransaction.FUnCommitedStatementCount);
      end;

      // No need to call events and UpdateExecute when collecting statemets
      if not BatchUpdate or (StatementTypes = [stBatchUpdate]) then
        FDataSet.DoAfterUpdateExecute(FDataSet, StatementTypes, TDBAccessUtils.GetParams(FUpdateQuery));

      Result := True;
    finally
      Marshal.FreeHGlobal(Buffer);
      if FUpdateQuery is TCustomDADataSet then
        TCustomDADataSet(FUpdateQuery).Close;
    end;
  finally
    EndConnection;
  end;
end;

function TDADataSetUpdater.IsRefreshQuickField(FieldDesc: TFieldDesc): boolean;
begin
  Result := False;
end;

procedure TDADataSetUpdater.SaveMaxRefreshQuickValue(FieldDesc: TFieldDesc; const Value: variant);
begin

end;

function TDADataSetUpdater.PerformPSUpdateRecord(UpdateKind: TUpdateKind; Delta: TDataSet): boolean;
var
  I: Integer;
  Old: Boolean;
  Param: TDAParam;
  Params: TDAParams;
  PName: _string;
  Field: TField;
  Value: Variant;
  StatementTypes: TStatementTypes;
  ParamsInfo: TDAParamsInfo;
begin
  StatementTypes := [];
  Include(StatementTypes, UpdateKindToStatementType(UpdateKind));
  // modified fields in this dataset must not affect Update SQL
  // (real modification is in Delta).
  CheckUpdateSQL('', StatementTypes, ParamsInfo, False);

  if TDBAccessUtils.GetSQL(FUpdateQuery).Count = 0 then begin
    Result := False;
    Exit;
  end;

  Params := TDBAccessUtils.GetParams(FUpdateQuery);

  // Nearly copied from TUpdateSQL.SetParams
  with FUpdateQuery do
    for I := 0 to Params.Count - 1 do
    begin
      Param := Params[I];
      PName := Param.Name;
      Old := _SameText(Copy(PName, 1, 4), 'OLD_');
      if Old then {$IFDEF CLR}Borland.Delphi.{$ENDIF}System.Delete(PName, 1, 4);
      Field := Delta.FindField(PName);
      if not Assigned(Field) then Continue;
      if Old then Param.AssignFieldValue(Field, Field.OldValue) else
      begin
        Value := Field.NewValue;
        if {$IFNDEF VER6P} VarIsEmpty {$ELSE}VarIsClear{$ENDIF}(Value) then Value := Field.OldValue;
        Param.AssignFieldValue(Field, Value);
      end;
    end;

  UpdateExecute(StatementTypes);
  Result := True;
end;

procedure TDADataSetUpdater.PrepareAppend;
begin

end;

procedure TDADataSetUpdater.PrepareUpdate;
var
  Statements: TStatementTypes;
begin
  // keep old behavior by the OldCachedUpdateLockMode variable
  if FDataSet.CachedUpdates and OldCachedUpdateLockMode then
    exit;

  Statements := [];
  // record that has been inserted (UpdateStatus = usInserted)
  // but hasn't been saved to database (UpdateResult <> uaApplied)
  // cannot be refreshed before Edit
  if roBeforeEdit in FDataSet.RefreshOptions then
    if (not FDataSet.CachedUpdates) or
       (FDataSet.InternalGetUpdateResult = urApplied) or
       (FDataSet.UpdateStatus <> usInserted)
    then
      if CanRefreshByLock then
        Include(Statements, stRefresh)
      else
        PerformRefreshRecord;

  // need lock for not modified records only
  // if record is modified then it is already locked
  if GetLockMode = lmPessimistic then
    if (not FDataSet.CachedUpdates) or
       (FDataSet.InternalGetUpdateResult = urApplied) or
       (FDataSet.UpdateStatus = usUnmodified)
    then
      Include(Statements, stLock);

  if Statements <> [] then begin
    if stLock in Statements then begin
      if FDataSet.CachedUpdates then
        PrepareCachedUpdate;
      SetLockSavepoint;
    end;

    try
      PerformSQL('', Statements);
    except
      // savepoint can be lost if connection is lost
      // or exception can be raised before set savepoint
      // so ignore all errors on rollback to savepoint
      if stLock in Statements then begin
        try
          // remove lock from the current record
          PerformUnlock;
        except
        end;
        if FDataSet.CachedUpdates then
        try
          // for disconnected mode: if cache doesn't have changed records
          // then finish transaction and close connection
          UnPrepareCachedUpdate;
        except
        end;
      end;

      raise;
    end;
  end;
end;

procedure TDADataSetUpdater.PrepareDelete;
begin
  // keep old behavior by the OldCachedUpdateLockMode variable
  if FDataSet.CachedUpdates and OldCachedUpdateLockMode then
    exit;

  if GetLockMode = lmPessimistic then
    // need lock for not modified records only
    // if record is modified then it is already locked
    if (not FDataSet.CachedUpdates) or
       (FDataSet.InternalGetUpdateResult = urApplied) or
       (FDataSet.UpdateStatus = usUnmodified)
    then begin
      if FDataSet.CachedUpdates then
        PrepareCachedUpdate;

      try
        PerformLock;
      except
        // savepoint can be lost if connection is lost
        // or exception can be raised before set savepoint
        // so ignore all errors on rollback to savepoint
        try
          // remove lock from the current record
          PerformUnlock;
        except
        end;
        if FDataSet.CachedUpdates then
        try
          // for disconnected mode: if cache doesn't have changed records
          // then finish transaction and close connection
          UnPrepareCachedUpdate;
        except
        end;

        raise;
      end;
    end;
end;

procedure TDADataSetUpdater.UnPrepareAppendUpdateDelete;
begin
  // keep old behavior by the OldCachedUpdateLockMode variable
  if FDataSet.CachedUpdates and OldCachedUpdateLockMode then
    exit;

  if (GetLockMode = lmPessimistic) and (FDataSet.State <> dsInsert) then
    // in the CachedUpdate mode if user will execute the following steps:
    // Edit - Post - Edit - Cancel
    // record must be locked after cancel: check InternalGetUpdateResult and UpdateStatus
    if (not FDataSet.CachedUpdates) or
       (FDataSet.InternalGetUpdateResult = urApplied) or (FDataSet.UpdateStatus = usUnmodified)
    then begin
      PerformUnlock;
      // for disconnected mode: if cache doesn't have changed records
      // then finish transaction and close connection
      if FDataSet.CachedUpdates then
        UnPrepareCachedUpdate;
    end;
end;

procedure TDADataSetUpdater.PrepareCachedUpdate;
begin
  if FDataSet.CachedUpdates and
     (not OldCachedUpdateLockMode) and
     (GetLockMode = lmPessimistic) and
     (not FDataSet.Data.HasUpdatedOrDeletedRecords) // if no updated records
  then
    // set savepoint for the CachedUpdates mode
    SetLockCachedUpdatesSavepoint;
end;

procedure TDADataSetUpdater.FinishCachedUpdate;
begin
  if FDataSet.CachedUpdates and
     (not OldCachedUpdateLockMode) and
     (GetLockMode = lmPessimistic) and
     (not FDataSet.Data.HasUpdatedOrDeletedRecords) // if no updated records
  then
    // on ApplyChanges reset information when transaction started
    ResetLockCachedUpdatesSavepoint;
end;

procedure TDADataSetUpdater.UnPrepareCachedUpdate;
begin
  if FDataSet.CachedUpdates and
     (not OldCachedUpdateLockMode) and
     (GetLockMode = lmPessimistic) and
     (not FDataSet.Data.HasUpdatedOrDeletedRecords) // if has locked records
  then
    // for disconnected mode: if cache doesn't have changed records
    // then finish transaction and close connection
    RollbackToLockCachedUpdatesSavepoint;
end;

procedure TDADataSetUpdater.UnLockCachedUpdate;
begin
  if FDataSet.CachedUpdates and
     (not OldCachedUpdateLockMode) and
     (GetLockMode = lmPessimistic) and
     FDataSet.Data.HasUpdatedOrDeletedRecords // if has locked records
  then
    // then rollback to the savepoint before first lock in the CachedUpdate mode
    RollbackToLockCachedUpdatesSavepoint;
end;

function TDADataSetUpdater.PerformLock: boolean;
begin
  Result := True;
  if not FDataSet.EOF then begin
    FDataSet.UpdateCursorPos;
    SetLockSavepoint;
    PerformSQL('', [stLock]);
  end;
end;

function TDADataSetUpdater.PerformUnLock: boolean;
begin
  Result := True;
  RollbackToLockSavepoint;
end;

function TDADataSetUpdater.PerformAppend: boolean;
begin
  Result := PerformSQL('', [stInsert]);

  if not Result and FDataSet.Options.StrictUpdate then
    DatabaseError(Format(SUpdateFailed, [0]));

  if Result then begin
    SetIdentityFieldValue;

    if (FUpdateQuery <> nil) and (FUpdateQuery is TCustomDADataSet) then
      TCustomDADataSet(FDataSet).FLastInsertId := TCustomDADataSet(FUpdateQuery).FLastInsertId;

    if (roAfterInsert in FDataSet.RefreshOptions) and
      RefreshAfterInsertAllowed
    then
      PerformRefreshRecordInUpdate;
  end;
end;

procedure TDADataSetUpdater.EndUpdate(Success: boolean);
begin
  // when record was locked and nothing was done
  if not Success and (GetLockMode <> lmNone) then
    PerformUnlock;

  // when transaction was started by lock operation AutoCommit on internal layer is not performed
  if Success and
    (FFixedTransaction <> nil) and FDataSetService.IsAutoCommit and
    (FLockTrStarted in [ltsOnLock, ltsOnLockCachedUpdates])
  then begin
    // for InterBase autocommit is performed in DoAferExecute
    if not UsedConnection.IsMultipleTransactionsSupported then
      FFixedTransaction.Commit;
    FFixedTransaction := nil;
  end;
end;

function TDADataSetUpdater.PerformDelete: boolean;
begin
  Result := False;
  if FDataSet.LockMode = lmOptimistic then
    BeginConnection;
  try
    if GetLockMode = lmOptimistic then
      if (not FDataSet.CachedUpdates) or
         (FDataSet.InternalGetUpdateResult = urApplied) or
         (FDataSet.UpdateStatus = usUnmodified)
      then begin
        // set savepoint for Lock in the CachedUpdate mode
        if FDataSet.CachedUpdates then
          PrepareCachedUpdate;

        try
          PerformLock;
        except
          // savepoint can be lost if connection is lost
          // or exception can be raised before set savepoint
          // so ignore all errors on rollback to savepoint
          try
            // remove lock from the current record
            PerformUnlock;
          except
          end;
          if FDataSet.CachedUpdates then
          try
            // for disconnected mode: if cache doesn't have changed records
            // then finish transaction and close connection
            UnPrepareCachedUpdate;
          except
          end;

          raise;
        end;
      end;

    try
      Result := PerformSQL('', [stDelete]);
      EndUpdate(Result);

    except
      if GetLockMode = lmOptimistic then begin
        // savepoint can be lost if connection is lost
        // or exception can be raised before set savepoint
        // so ignore all errors on rollback to savepoint
        try
          // remove lock from the current record
          PerformUnlock;
        except
        end;
        if FDataSet.CachedUpdates then
        try
          // for disconnected mode: if cache doesn't have changed records
          // then finish transaction and close connection
          UnPrepareCachedUpdate;
        except
        end;
      end;

      raise;
    end;

  finally
    if GetLockMode = lmOptimistic then
      EndConnection;
  end;
end;

function TDADataSetUpdater.PerformUpdate: boolean;
begin
  Result := False;
  BeginConnection;
  try
    if GetLockMode = lmOptimistic then
      if (not FDataSet.CachedUpdates) or
         (FDataSet.InternalGetUpdateResult = urApplied) or
         (FDataSet.UpdateStatus = usUnmodified)
      then begin
        // set savepoint for Lock in the CachedUpdate mode
        if FDataSet.CachedUpdates then
          PrepareCachedUpdate;

        try
          PerformLock; // can raise exception after lock
        except
          // savepoint can be lost if connection is lost
          // or exception can be raised before set savepoint
          // so ignore all errors on rollback to savepoint
          try
            // remove lock from the current record
            PerformUnlock;
          except
          end;
          if FDataSet.CachedUpdates then
          try
            // for disconnected mode: if cache doesn't have changed records
            // then finish transaction and close connection
            UnPrepareCachedUpdate;
          except
          end;

          raise;
        end;
      end;

    try
      Result := PerformSQL('', [stUpdate]);

      if Result and (roAfterUpdate in FDataSet.RefreshOptions) and
        (FDataSetService.IsFullRefresh or not FDataSetService.IsDMLRefresh)
      then
        PerformRefreshRecordInUpdate;

      EndUpdate(Result);
    except
      if GetLockMode = lmOptimistic then begin
        // savepoint can be lost if connection is lost
        // or exception can be raised before set savepoint
        // so ignore all errors on rollback to savepoint
        try
          // remove lock from the current record
          PerformUnlock;
        except
        end;
        if FDataSet.CachedUpdates then
        try
          // for disconnected mode: if cache doesn't have changed records
          // then finish transaction and close connection
          UnPrepareCachedUpdate;
        except
        end;
      end;

      raise;
    end;
    
  finally
    EndConnection;
  end;
end;

function TDADataSetUpdater.PerformRefreshRecord: boolean;
begin
  if FDataSet.CachedUpdates and (FDataSet.UpdateStatus = usModified) and
    (not FDataSetService.IsInCacheProcessing)
  then
    FDataSet.RevertRecord; {TODO -cMemoryLeak: cause memory leak and DisposeBuf failed}

  FDataSet.FreeRefComplexFields(FDataSet.ActiveBuffer);
  Result := PerformSQL('', [stRefresh]);
end;

function TDADataSetUpdater.PerformRefreshRecordInUpdate: boolean;
begin
  FRefreshInUpdate := True;
  try
    Result := PerformRefreshRecord;
  finally
    FRefreshInUpdate := False;
  end;  
end;

function TDADataSetUpdater.PerformRefreshQuick(const CheckDeleted: boolean): boolean;
var
  KeyFieldDescs: TFieldDescArray;
  OldStrictUpdate, OldFiltered: boolean;
  KeyFields: TFieldArray;
  Values: variant;
  KeyFieldsCount: integer;

begin
  Result := True;
  FDataSet.DoBeforeRefresh;
  BeginConnection;
  try
    FDataSetService.GetKeyFieldDescs(KeyFieldDescs);
    KeyFieldsCount := Length(KeyFieldDescs);
    if KeyFieldsCount = 0 then
      DatabaseError(SKeyFieldsReq);

    FDataSet.CheckBrowseMode;
    if FDataSetService.FUpdatingTableInfoIdx = - 1 then
      Exit;

    OldStrictUpdate := FDataSet.Options.StrictUpdate;
    OldFiltered := FDataSet.Filtered;
    FDataSet.DisableControls;
    try
      FDataSet.Filtered := False;
      FDataSet.Options.StrictUpdate := False;
      FDataSet.GetCurrentKeys(KeyFields, Values);

      if CheckDeleted and not FDataSet.IsEmpty then
        PerformSQL('', [stRefreshCheckDeleted]);

      PerformSQL('', [stRefreshQuick]);

      if not FDataSet.Locate(KeyFields, Values, []) then
        FDataSet.First;
    finally
      FDataSet.Options.StrictUpdate := OldStrictUpdate;
      FDataSet.Filtered := OldFiltered;
      FDataSet.EnableControls;
      FDataSet.DoAfterRefresh;
    end;
  finally
    EndConnection;
  end;
end;

function TDADataSetUpdater.CacheChanged: boolean;
begin
  UnPrepareCachedUpdate;
  Result := True;
end;

function TDADataSetUpdater.CacheApplied: boolean;
begin
  FinishCachedUpdate;
  Result := True;
end;

function TDADataSetUpdater.CacheCanceled: boolean;
begin
  UnLockCachedUpdate;
  Result := True;
end;

function TDADataSetUpdater.BatchUpdate: boolean;
begin
  Result := FDataSetService.IsInCacheProcessing and (FDataSet.Options.UpdateBatchSize > 1) and
    not (GetUpdateObject <> nil);
end;

function TDADataSetUpdater.CanFlushBatch: boolean;
begin
  Result := BatchUpdate and (FBatchStatements > 0) and (FBatchStatements >= FDataSet.Options.UpdateBatchSize);
end;

function TDADataSetUpdater.PrepareBatch(SQL: _string): _string;
begin
  Result := SQL;
end;

procedure TDADataSetUpdater.FlushBatch;
begin
  if FBatchStatements > 0 then
    try
      PerformSQL('', [stBatchUpdate]);
    finally
      ClearBatch;
    end;
end;

procedure TDADataSetUpdater.ClearBatch;
begin
  FBatchSQLs.Length := 0;
  FBatchParams.Clear;
  FBatchStatements := 0;
end;

function TDADataSetUpdater.SelectDbValue(const OperationName, SQL: _string): variant;
var
  MonitorClass: TDASQLMonitorClass;
  MessageID: cardinal;
  UpdateQuery: TCustomDADataSet;
begin
  CheckUpdateQuery(stCustom);

  UpdateQuery := TCustomDADataSet(FUpdateQuery);
  UpdateQuery.SQL.Text := SQL;

  BeginConnection;
  try
    MonitorClass := TDASQLMonitorClass(UsedConnection.SQLMonitorClass);
    if not FDataSet.FLockDebug and (MonitorClass.HasMonitor or FDataSet.Debug) then
      MonitorClass.SQLExecute(FDataSet, SQL, UpdateQuery.Params, OperationName, MessageID, True);

    UpdateQuery.Execute;
    Result := UpdateQuery.Fields[0].Value;
    UpdateQuery.Close;

    if not FDataSet.FLockDebug and (MonitorClass.HasMonitor or FDataSet.Debug) then
      MonitorClass.SQLExecute(FDataSet, SQL, UpdateQuery.Params, OperationName, MessageID, False);
  finally
    EndConnection;
  end;
end;

function TDADataSetUpdater.GetDefaultExpressionValue(DefExpr: _string; var Value: variant): boolean;
begin
  Result := False;
end;

procedure TDADataSetUpdater.UnprepareUpdateObjects;
var
  stIdx: TStatementType;
begin
  for stIdx := Low(FUpdateComponents) to High(FUpdateComponents) do
    if FUpdateComponents[stIdx] is TCustomDADataSet then
      TCustomDADataSet(FUpdateComponents[stIdx]).UnPrepare;
end;

function TDADataSetUpdater.GetLockMode: TLockMode;
begin
  Result := FDataSet.LockMode;
end;

function TDADataSetUpdater.GetUpdateObject: TCustomDAUpdateSQL;
begin
  Result := FDataSet.UpdateObject;
end;

function TDADataSetUpdater.GetUpdateSQL(StatementType: TStatementType): _string;
begin
  Result := FDataSet.FUpdateSQL[StatementType].Text;
end;

function TDADataSetUpdater.GetIRecordSet: TCRRecordSet;
begin
  Result := FDataSet.FIRecordSet;
end;

function TDADataSetUpdater.GetICommand: TCRCommand;
begin
  Result := FDataSet.FICommand;
end;

procedure TDADataSetUpdater.CheckIRecordSet;
begin
  FDataSet.CheckIRecordSet;
end;

function TDADataSetUpdater.UsedConnection: TCustomDAConnection;
begin
  Result := FDataSet.UsedConnection;
end;

function TDADataSetUpdater.UsedTransaction: TDATransaction;
begin
  Result := FDataSet.UsedTransaction;
end;

function TDADataSetUpdater.UsedUpdateTransaction: TDATransaction;
begin
  Result := FDataSet.UsedUpdateTransaction;
end;

procedure TDADataSetUpdater.SetRowsAffected(Value: Integer);
begin
  FDataSet.FRowsAffected := Value;
end;

procedure TDADataSetUpdater.BeginConnection;
begin
  FDataSet.BeginConnection;
end;

procedure TDADataSetUpdater.EndConnection;
begin
  FDataSet.EndConnection;
end;

function TDADataSetUpdater.UseParamType: boolean;
begin
  Result := False;
end;

function TDADataSetUpdater.NeedReturnParams: boolean;
begin
  Result := FDataSet.Options.ReturnParams or FDataSet.FDMLRefresh;
end;

function TDADataSetUpdater.RetunParamsAsFields: boolean;
begin
  Result := False;
end;

function TDADataSetUpdater.RefreshAfterInsertAllowed: boolean;
begin
  Result := not FDataSet.FDMLRefresh or FDataSet.Options.FullRefresh;
end;

{ TDADataSetService }

constructor TDADataSetService.Create(AOwner: TMemDataSet);
begin
  FDataSet := TCustomDADataSet(AOwner);

  inherited;

  CreateSQLGenerator;
end;

destructor TDADataSetService.Destroy;
begin
  FreeSQLGenerator;
  
  inherited;
end;

procedure TDADataSetService.SetDataSetUpdater(Value: TDataSetUpdater);
begin
  inherited;

  FUpdater := TDADataSetUpdater(Value);
end;

procedure TDADataSetService.CreateSQLGenerator;
begin
  SetSQLGenerator(TDASQLGenerator.Create(Self));
end;

procedure TDADataSetService.FreeSQLGenerator;
begin
  FSQLGenerator.Free;
  FSQLGenerator := nil;
end;

procedure TDADataSetService.SetSQLGenerator(Value: TDASQLGenerator);
begin
  FreeSQLGenerator;
  FSQLGenerator := Value;
end;

procedure TDADataSetService.PreInitCursor;
begin
  if FFieldsExtInited then
    exit;

  InitUpdatingTableIdx;

  if not ExtFieldsInfoIsInternal then
    GetExtFieldsInfo;

  FIdentityField := DetectIdentityField;
  SetFieldsReadOnly;

  FFieldsExtInited := True;
end;

procedure TDADataSetService.InitCursor;
var
  i: integer;
  HiddenFields: TFieldArray;
  Field: TField;
  FieldDesc: TCRFieldDesc;
begin
  InitUpdatingTableFields;

  for i := 0 to FDataSet.Fields.Count - 1 do begin
    Field := FDataSet.Fields[i];
    if Field.FieldKind <> fkData then
      continue;

    FieldDesc := TCRFieldDesc(FDataSet.GetFieldDesc(Field));
    if FDataSet.Options.FieldsOrigin then
      SetFieldOrigin(Field, FieldDesc);

  {$IFNDEF FPC}
    if FieldDesc.IsAutoIncrement then
      Field.AutoGenerateValue := arAutoInc;
  {$ENDIF}
  end;

  HiddenFields := nil;
  if FDataSet.DefaultFields then begin
    HiddenFields := DetectHiddenFields;
    for i := 0 to High(HiddenFields) do begin
      HiddenFields[i].Visible := False;
      HiddenFields[i].ReadOnly := True;
    end;
  end;

  FKeyGeneratorField := DetectKeyGeneratorField;
  if FKeyGeneratorField <> nil then
    FKeyGeneratorField.Required := False;
end;

procedure TDADataSetService.CloseCursor;
begin
end;

procedure TDADataSetService.InitUpdatingTableIdx;
var
  i: integer;
  NormalizedName: _string;
  TablesInfo: TCRTablesInfo;
  TableInfo: TCRTableInfo;
begin
  TablesInfo := GetIRecordSet.TablesInfo;
  NormalizedName := FDataSet.SQLInfo.NormalizeName(FDataSet.UpdatingTable);

  FUpdTableIsArtificial := False;
  if TablesInfo.Count = 0 then begin
    if NormalizedName <> '' then begin
      TableInfo := TablesInfo.Add;
      TableInfo.TableName := NormalizedName;
      FUpdatingTableInfoIdx := 0;
      FUpdTableIsArtificial := True;
    end
    else
      FUpdatingTableInfoIdx := -1;
  end
  else
  if NormalizedName = '' then // Select default updating table
    FUpdatingTableInfoIdx := 0
  else begin
    i := TablesInfo.IndexByName(NormalizedName);
    if i = -1 then
      i := TablesInfo.IndexByAlias(NormalizedName);

    if i = - 1 then
      DatabaseErrorFmt(SBadUpdatingTable, [NormalizedName]);

    FUpdatingTableInfoIdx := i;
  end;
end;

procedure TDADataSetService.InitUpdatingTableFields;

  function GetAnyFieldCanBeModified: boolean;
  var
    i: integer;
  begin
    Result := False;
    for i := 0 to FDataSet.Fields.Count - 1 do
      if not FDataSet.Fields[i].ReadOnly then begin
        Result := True;
        Break;
      end;
  end;
begin
  FIsAnyFieldCanBeModified := GetAnyFieldCanBeModified;

{$IFDEF WITH_IPROVIDER}
  if GetIRecordSet.TablesInfo.Count > 0 then
    FDataSet.FOldKeyFields := FDataSet.PSGetKeyFields;
{$ENDIF}

  if FDataSet.Options.DefaultValues then
    FillFieldsDefaultValues;
end;

procedure TDADataSetService.InitUpdatingTable;
begin
  ClearCachedKeyFieldDescs;
  ClearCachedDataFieldDescs;
  InitUpdatingTableIdx;
  SetFieldsReadOnlyOld;
  InitUpdatingTableFields;
end;

procedure TDADataSetService.SetFieldOrigin(Field: TField; FieldDesc: TCRFieldDesc);
var
  TblName: _string;
begin
  if FieldDesc.TableInfo <> nil then begin
    TblName := FieldDesc.TableInfo.TableName;
    TblName := FDataSet.SQLInfo.NormalizeName(TblName, False, True);
    Field.Origin := TblName + '.' + FieldDesc.ActualName;
  end
  else
    Field.Origin := FieldDesc.ActualName;
end;

procedure TDADataSetService.FillFieldsDefaultValues;
var
  Field: TField;
  FieldDesc: TCRFieldDesc;
  i: integer;
begin
  for i := 0 to FDataSet.Fields.Count - 1 do begin
    Field := FDataSet.Fields[i];
    if Field.FieldKind <> fkData then
      continue;

    FieldDesc := TCRFieldDesc(FDataSet.GetFieldDesc(Field));
    if FieldDesc <> nil then
      Field.DefaultExpression := FieldDesc.DefaultExpr;
  end;
end;

procedure TDADataSetService.SetFieldsReadOnly;
var
  i: integer;
  RecordSet: TCRRecordSet;
  FieldDesc: TCRFieldDesc;
begin
  if OldFieldsReadOnly and not FDataSet.Options.SetFieldsReadOnly then
    exit;

  RecordSet := GetIRecordSet;
  for i := 0 to RecordSet.FieldCount - 1 do begin
    FieldDesc := TCRFieldDesc(RecordSet.Fields[i]);
    if FieldDesc.FieldDescKind <> fdkData then
      continue;

    if FDataSet.Options.SetFieldsReadOnly then begin
      if (FieldDesc.TableInfo = nil) or (FieldDesc.TableInfo.Index <> FUpdatingTableInfoIdx) then
        FieldDesc.ReadOnly := True;
    end
    else begin
      FieldDesc.ReadOnly := False;
      if (FieldDesc.TableInfo = nil) or (FieldDesc.TableInfo.Index <> FUpdatingTableInfoIdx) then
        FieldDesc.Required := False;
    end;
  end;
end;

procedure TDADataSetService.SetFieldsReadOnlyOld;
var
  i, j: integer;
  FieldDesc: TFieldDesc;
  DataFieldDescs: TFieldDescArray;

  function GetRootParent(FieldDesc: TFieldDesc): TFieldDesc;
  begin
    Result := FieldDesc;
    while Result.ParentField <> nil do
      Result := Result.ParentField;
  end;

begin
  if not FDataSet.ReadOnly then begin
    if FDataSet.Options.SetFieldsReadOnly then begin

      for i := 0 to FDataSet.Fields.Count - 1 do
        if FDataSet.Fields[i].FieldKind = fkData then
          FDataSet.Fields[i].ReadOnly := True;

      for i := 0 to FDataSet.FIRecordSet.Fields.Count - 1 do
        FDataSet.FIRecordSet.Fields[i].ReadOnly := False;

      GetDataFieldDescs(DataFieldDescs);

      for i := 0 to FDataSet.Fields.Count - 1 do begin
        FieldDesc := FDataSet.GetFieldDesc(FDataSet.Fields[i]);
        for j := 0 to High(DataFieldDescs) do
          if (FieldDesc = DataFieldDescs[j]) or (GetRootParent(FieldDesc) = DataFieldDescs[j]) then begin
            FDataSet.Fields[i].ReadOnly := False;
            Break;
          end;
      end;
    end;
  end;  
end;

function TDADataSetService.DetectIdentityField: TCRFieldDesc;
begin
  Result := nil;
end;

function TDADataSetService.DetectKeyGeneratorField: TField;
begin
  Result := nil;
end;

function TDADataSetService.DetectHiddenFields: TFieldArray;
begin
  SetLength(Result, 0);
end;

function TDADataSetService.DetectCanModify: boolean;
begin
  Result := not (FDataSet.ReadOnly or FDataSet.UniDirectional) and
    (FDataSet.LocalUpdate or
    FDataSet.CachedUpdates and
    Assigned(FDataSet.OnUpdateRecord) or
    Assigned(FDataSet.UpdateObject));
end;

function TDADataSetService.GetRecCount: integer;
begin
  Result := 0;
end;

procedure TDADataSetService.BreakExec;
begin

end;

function TDADataSetService.Executing: boolean;
begin
  Result := False;
end;

function TDADataSetService.GetCurrentSchema: _string;
begin
  Result := '';
end;

procedure TDADataSetService.InitMasterParams(Params: TDAParams);
begin

end;

procedure TDADataSetService.WriteFieldXMLAttributeType(Field: TField; FieldDesc: TFieldDesc; const FieldAlias: _string; XMLWriter: XMLTextWriter);
begin
  inherited;

  if (FieldDesc is TCRFieldDesc)
    and (TCRFieldDesc(FieldDesc).TableInfo <> nil)
    and (TCRFieldDesc(FieldDesc).TableInfo.TableName <> '') then
    XmlWriter.WriteAttributeString('rs:basetable', TCRFieldDesc(FieldDesc).TableInfo.TableName);
end;

function TDADataSetService.QuoteName(const AName: _string): _string;
begin
  if FDataSet.FOptions.FQuoteNames or FDataSet.SQLInfo.QuotesNeeded(AName) then
    Result := FDataSet.SQLInfo.Quote(AName)
  else
    Result := AName;
end;

procedure TDADataSetService.ClearCachedKeyFieldDescs;
begin
  SetLength(FCachedKeyFieldDescs[False], 0);
  SetLength(FCachedKeyFieldDescs[True], 0);
  FKeyFieldDescsIsCached[False] := False;
  FKeyFieldDescsIsCached[True] := False;
end;

procedure TDADataSetService.ClearCachedDataFieldDescs;
begin
  SetLength(FCachedDataFieldDescs[False], 0);
  SetLength(FCachedDataFieldDescs[True], 0);
  FDataFieldDescsIsCached[False] := False;
  FDataFieldDescsIsCached[True] := False;    
end;

procedure TDADataSetService.ClearFieldDescRefs;
begin
  FFieldsExtInited := False;
  FUpdatingTableInfoIdx := -1;
  FIdentityField := nil;
  ClearCachedKeyFieldDescs;
  ClearCachedDataFieldDescs;
end;

procedure TDADataSetService.GetKeyFieldDescs(out KeyFieldDescs: TFieldDescArray;
  ForceUseAllFields: boolean = False);
var
  UseAllKeyFields: boolean;
begin
// ForceUseAllFields parameter is used to generate condtion for full refresh
// When ForceUseAllFields is True GetKeyFieldsDesc should return KeyFields from
// all tables (not only updating). If there is no possibility do this (ODAC, IBDAC)
// CanUseAllKeyFields function should return False

  UseAllKeyFields := ForceUseAllFields and CanUseAllKeyFields;
  if FKeyFieldDescsIsCached[UseAllKeyFields] then
    KeyFieldDescs := FCachedKeyFieldDescs[UseAllKeyFields]
  else begin
    FillKeyFieldDescs(KeyFieldDescs, UseAllKeyFields);
    FCachedKeyFieldDescs[UseAllKeyFields] := KeyFieldDescs;
    FKeyFieldDescsIsCached[UseAllKeyFields] := True;
  end;
end;

procedure TDADataSetService.GetDataFieldDescs(out DataFieldDescs: TFieldDescArray;
  ForceUseAllFields: boolean = False);
begin
  if FDataFieldDescsIsCached[ForceUseAllFields] then
    DataFieldDescs := FCachedDataFieldDescs[ForceUseAllFields]
  else begin
    FillDataFieldDescs(DataFieldDescs, ForceUseAllFields);
    FCachedDataFieldDescs[ForceUseAllFields] := DataFieldDescs;
    FDataFieldDescsIsCached[ForceUseAllFields] := True;
  end;
end;

procedure TDADataSetService.GetKeyAndDataFields(out KeyAndDataFields: TKeyAndDataFields;
  ForceUseAllFields: boolean);
begin
  GetKeyFieldDescs(KeyAndDataFields.KeyFieldDescs, ForceUseAllFields);
  GetDataFieldDescs(KeyAndDataFields.DataFieldDescs, ForceUseAllFields);
end;

function TDADataSetService.GetDBKeyList(TableName: _string; IndexName: _string = ''): _string;
var
  MetaData: TDAMetaData;
  OldIndexName, OldIndexSchema, NewIndexName, NewIndexSchema: _string;
  Info: TExtTableInfo;
begin
  BeginConnection; // GetCurrentSchema requires an active connection
  try
    FDataSet.SQLInfo.SplitObjectName(TableName, Info);
    if Info.Schema = '' then
      Info.Schema := GetCurrentSchema;

    MetaData := TDAMetaData.Create(nil);
    try
      MetaData.Connection := UsedConnection;
      MetaData.Transaction := FDataSet.UsedTransaction; //TODO:
      MetaData.MetaDataKind := 'indexcolumns';
      MetaData.Restrictions.Text := 'table_catalog=' + Info.Catalog +
        #13#10'table_schema=' + Info.Schema +
        #13#10'table_name=' + Info.Table +
        #13#10'index_name=' + IndexName +
        #13#10'unique=1';
      MetaData.Open;
      while not MetaData.Eof do begin
        NewIndexName := _VarToStr(MetaData.FieldByName('INDEX_NAME').Value);
        NewIndexSchema := _VarToStr(MetaData.FieldByName('INDEX_SCHEMA').Value);
        if (OldIndexName <> '') and
          ((NewIndexName <> OldIndexName) or (NewIndexSchema <> OldIndexSchema))
        then
          Break;
        if Result <> '' then
          Result := Result + ';';
        Result := Result + _VarToStr(MetaData.FieldByName('COLUMN_NAME').Value);
        OldIndexName := NewIndexName;
        OldIndexSchema := NewIndexSchema;
        MetaData.Next;
      end;
      MetaData.Close;
    finally
      MetaData.Free;
    end;
  finally
    EndConnection;
  end;
end;

function TDADataSetService.OpenNext: boolean;
begin
  raise Exception.Create(SOperationNotSupported);
end;

function TDADataSetService.NeedParamValuesOnPrepare: boolean;
begin
  Result := False;
end;

function TDADataSetService.CanUseAllKeyFields: boolean;
begin
  Result := False;
end;

function TDADataSetService.IdentityFieldIsData: boolean;
begin
  Result := False;
end;

procedure TDADataSetService.FillKeyFieldDescs(out KeyFieldDescs: TFieldDescArray; KeyFields: _string;
  CheckKeyFields: boolean = True);
var
  Pos: integer;
  FieldName: _string;
  FieldDesc: TCRFieldDesc;
begin
  Pos := 1;
  while Pos <= Length(KeyFields) do begin
    FieldName := ExtractFieldName(KeyFields, Pos);
    if FieldName <> '' then begin
      FieldDesc := TCRFieldDesc(GetIRecordSet.FindFieldByName(FieldName));
      if FieldDesc = nil then
        FieldDesc := TCRFieldDesc(GetIRecordSet.FindFieldByActualName(FieldName));
      if FieldDesc <> nil then begin
        SetLength(KeyFieldDescs, Length(KeyFieldDescs) + 1);
        KeyFieldDescs[Length(KeyFieldDescs) - 1] := FieldDesc;
      end
      else begin
        SetLength(KeyFieldDescs, 0);
        if CheckKeyFields then
          DatabaseErrorFmt(SKeyFieldNotFound,[FDataSet.Name, FieldName]);
      end;
    end;
  end;
end;

procedure TDADataSetService.FillKeyFieldDescs(out KeyFieldDescs: TFieldDescArray;
  ForceUseAllKeyFields: boolean);
begin
  if FDataSet.KeyFields <> '' then
    FillKeyFieldDescs(KeyFieldDescs, GetKeyFields)
  else
  if FIdentityField <> nil then begin
    SetLength(KeyFieldDescs, 1);
    KeyFieldDescs[0] := FIdentityField;
  end
  else
  if UpdatingTableInfo <> nil then // Read key fields info from server
    FillKeyFieldDescs(KeyFieldDescs, GetDBKeyList(UpdatingTableInfo.TableName), False);
end;

procedure TDADataSetService.FillDataFieldDescs(out DataFieldDescs: TFieldDescArray;
  ForceUseAllKeyFields: boolean);

  function CheckField(FieldDesc: TCRFieldDesc): boolean;
  begin
    Result := False;

    if FieldDesc.HasParent or (FieldDesc.FieldDescKind <> fdkData) or
      (FieldDesc = FIdentityField) and not IdentityFieldIsData
    then
      exit;

    if not ForceUseAllKeyFields and
      ((FieldDesc.TableInfo <> UpdatingTableInfo) or (FieldDesc.TableInfo = nil))
    then
      exit;

    Result := not FieldDesc.ReadOnly;
  end;

  procedure AddFieldDesc(FieldDesc: TCRFieldDesc);
  begin
    SetLength(DataFieldDescs, Length(DataFieldDescs) + 1);
    DataFieldDescs[High(DataFieldDescs)] := FieldDesc;
  end;

var
  i: integer;
  FieldDesc: TCRFieldDesc;
  RootFieldDesc: TCRFieldDesc;
  RecordSet: TCRRecordSet;
begin
  RecordSet := FDataSet.FIRecordSet;

  if FDataSet.DefaultFields then
    for i := 0 to RecordSet.Fields.Count - 1 do begin
      FieldDesc := TCRFieldDesc(RecordSet.Fields[i]);
      if CheckField(FieldDesc) then
        AddFieldDesc(FieldDesc);
    end
  else
    for i := 0 to RecordSet.Fields.Count - 1 do begin
      FieldDesc := TCRFieldDesc(RecordSet.Fields[i]);
      // add standard fields if they is found in the dataset
      if FieldDesc.ParentField = nil then begin
        if CheckField(FieldDesc) and
          ((FieldDesc.DataType = dtTable) or (FDataSet.FindField(FieldDesc.Name) <> nil))
        then
          AddFieldDesc(FieldDesc);
      end
      // add object fields
      else begin
        // find root field
        RootFieldDesc := TCRFieldDesc(FieldDesc.ParentField);
        while RootFieldDesc.ParentField <> nil do
          RootFieldDesc := TCRFieldDesc(RootFieldDesc.ParentField);
        // check if root field was added (it is always last)
        if DataFieldDescs[High(DataFieldDescs)] = RootFieldDesc then
          continue;

        // if root has not been added and child field is found in the dataset
        // then add root field to the field list
        if FDataSet.FindField(FieldDesc.ActualName) <> nil then
          if CheckField(RootFieldDesc) then
            AddFieldDesc(RootFieldDesc);
      end;
    end
end;

procedure TDADataSetService.GetExtFieldsInfo;
var
  Tables: TExtTablesInfo;
  Columns: TColumnsInfo;
  FieldDesc: TCRFieldDesc;
  ReadFieldsFromServer, ExtFieldsInfo: boolean;
  Str, TableName, FieldName: _string;
  AsteriskCount, DefaultTable: integer;
  ColumnInfo: TColumnInfo;
  i, j: integer;
  TablesInfo: TCRTablesInfo;
  RecordSet: TCRRecordSet;
  SQLInfo: TSQLInfo;
  ParserClass: TSQLParserClass;
  v: variant;
  IdentCase: TIdentCase;
begin
  TablesInfo := GetTablesInfo;

  // for the case when we can not determine tables list
  // select from (select from)
  if TablesInfo.Count = 0 then
    exit;

  SQLInfo := FDataSet.SQLInfo;
  ParserClass := GetICommand.GetParserClass;
  SetLength(Tables, TablesInfo.Count);
  for i := 0 to High(Tables) do begin
    SQLInfo.SplitObjectName(TablesInfo[i].TableName, Tables[i]);
    Tables[i].Table := SQLInfo.NormalizeName(Tables[i].Table, False, True);
    Tables[i].Schema := SQLInfo.NormalizeName(Tables[i].Schema, False, True);
    Tables[i].Catalog := SQLInfo.NormalizeName(Tables[i].Catalog, False, True);
    Tables[i].DBLink := SQLInfo.NormalizeName(Tables[i].DBLink, False, True);
  end;

  Str := FDataSet.FinalSQL;
  Columns := TColumnsInfo.Create;
  try
    SQLInfo.ParseColumnsInfo(Str, Columns);
    AsteriskCount := 0;
    DefaultTable := -1; // table index for fields from '*'
    ReadFieldsFromServer := FUpdTableIsArtificial;
    for i := 0 to Columns.Count - 1 do begin
      ColumnInfo := Columns[i];
      if (ColumnInfo.Table <> '') and not FUpdTableIsArtificial then
        for j := 0 to TablesInfo.Count - 1 do begin
          if TablesInfo[j].TableAlias <> '' then
            TableName := SQLInfo.NormalizeName(TablesInfo[j].TableAlias, False, True)
          else
            TableName := SQLInfo.NormalizeName(TablesInfo[j].TableName, False, True);
          if (ColumnInfo.Table = TableName) or
            // table name without schema
            (TablesInfo[j].TableAlias = '') and (ColumnInfo.Table = Tables[j].Table)
          then begin
            ColumnInfo.TableIndex := j;
            break;
          end;
        end;

      // We use Expr to set Alias.
      // Currently Expr is not properly normalized by ParseColumnsInfo.
      // Moreover Expr is returned without spaces. So Alias will not always correct.
      ColumnInfo.Expr := _UpperCase(ColumnInfo.Expr);

      if (ColumnInfo.Alias = '') and (ColumnInfo.Name <> '*') then begin
        if ColumnInfo.Name <> '' then
          ColumnInfo.Alias := ColumnInfo.Name
        else
        // We need correct Alias do find corresponding FieldDesc.
        // Otherwise expression can be treated as field from '*'.
          ColumnInfo.Alias := ColumnInfo.Expr;
      end;

      if ColumnInfo.Name = '*' then begin
        Inc(AsteriskCount);
        // If Table <> '', we use this ColumnInfo.TableIndex for all FieldDescs that does not
        // have corresponding ColumnInfo.
        // If tables count > 1 and Table = '' or there are several '*' with different Table,
        // we cannot determine FieldDesc's table without query to ALL_TAB_COLUMNS.
        // There can be several tables even if TableInfo.Count = 1:
        // select * from t1, (select a from b) t2
        if (ColumnInfo.Table = '') or
          (AsteriskCount > 1) and (ColumnInfo.TableIndex <> DefaultTable)
        then
          ReadFieldsFromServer := True;

        // ReadFieldsFromServer will be reset to False if ExtendedFieldsInfo = False.
        // So DefaultTable must be set.
        // When ExtendedFieldsInfo = False we suggest that all fields w/o ColumnInfo
        // belong to Table from first '*' in query
        if AsteriskCount = 1 then begin // for first '*'
          if (ColumnInfo.Table = '') or FUpdTableIsArtificial then
            DefaultTable := 0
          else
            DefaultTable := ColumnInfo.TableIndex; // can be -1 for table that is not present in TablesInfo
        end;
      end
      else
      if ParserClass.IsQuasiColumn(ColumnInfo.Name) then begin
      // Column not present in metadata - ROWID
        if ColumnInfo.Table = '' then
          ColumnInfo.TableIndex := 0;
      end
      else begin
        if (ColumnInfo.Table = '') and (ColumnInfo.Name <> '') then
           // Possibly it is a value or system variable
           ReadFieldsFromServer := True;
      end;
    end;

    RecordSet := GetIRecordSet;
    RecordSet.GetProp(prExtendedFieldsInfo, v);
    ExtFieldsInfo := v;

    ReadFieldsFromServer := ReadFieldsFromServer and
      (ExtFieldsInfo or FDataSet.Options.DefaultValues or FDataSet.Options.FieldsOrigin);

    if ReadFieldsFromServer or FDataSet.Options.DefaultValues then
      RequestFieldsInfo(Tables, Columns);

    IdentCase := SQLInfo.IdentCase;
    for i := 0 to RecordSet.FieldCount - 1 do begin
      FieldDesc := TCRFieldDesc(RecordSet.Fields[i]);
      if FieldDesc.FieldDescKind <> fdkData then
        continue;

      FieldName := FieldDesc.ActualName;
      for j := 0 to Columns.Count - 1 do begin
        ColumnInfo := Columns[j];
        if not ColumnInfo.Used and
          ((IdentCase <> icMixed) and (FieldName = ColumnInfo.Alias) or
          (IdentCase = icMixed) and _SameText(FieldName, ColumnInfo.Alias)) and
          (not ReadFieldsFromServer or (ColumnInfo.TableIndex <> -1))
        then begin
          if ColumnInfo.Name <> '' then begin
            ColumnInfo.Used := True;

            if ColumnInfo.TableIndex <> - 1 then
              FieldDesc.TableInfo := TablesInfo[ColumnInfo.TableIndex]
            else
              // When ExtendedFieldsInfo = False we suggest that field w/o Table
              // belong to first table in TablesInfo
              if (ColumnInfo.Table = '') and not ReadFieldsFromServer then
                FieldDesc.TableInfo := TablesInfo[0];

            if ColumnInfo.Alias <> '' then
              FieldDesc.ActualName := ColumnInfo.Name;
            FieldDesc.DefaultExpr := ColumnInfo.Expr;
          end;
          break;
        end
        else
          // If we don't find corresponding ColumnInfo,
          // we suggest that field belong to the same table that '*' belong
          if j = Columns.Count - 1 then begin
            if not ReadFieldsFromServer and (DefaultTable <> -1) then
              FieldDesc.TableInfo := TablesInfo[DefaultTable];
          end;
      end;
    end;
  finally
    Columns.Free;
  end;
end;

function TDADataSetService.ExtFieldsInfoIsInternal: boolean;
begin
  Result := True;
end;

procedure TDADataSetService.RequestFieldsInfo(Tables: TExtTablesInfo; Columns: TColumnsInfo);
begin
end;

function TDADataSetService.GetIConnection: TCRConnection;
begin
  if FDataSet.FICommand <> nil then
    Result := FDataSet.FICommand.GetConnection
  else
    Result := nil;
end;

function TDADataSetService.GetICommand: TCRCommand;
begin
  Result := FDataSet.FICommand;
end;

function TDADataSetService.GetIRecordSet: TCRRecordSet;
begin
  Result := FDataSet.FIRecordSet;
end;

procedure TDADataSetService.CheckIRecordSet;
begin
  FDataSet.CheckIRecordSet;
end;

function TDADataSetService.GetTablesInfo: TCRTablesInfo;
begin
  Result := FDataSet.GetTablesInfo;
end;

function TDADataSetService.UsedConnection: TCustomDAConnection;
begin
  Result := FDataSet.UsedConnection;
end;

function TDADataSetService.IsFullRefresh: boolean;
begin
  Result := FDataSet.Options.FullRefresh;
end;

function TDADataSetService.IsDMLRefresh: boolean;
begin
  Result := FDataSet.DMLRefresh;
end;

function TDADataSetService.IsInCacheProcessing: boolean;
begin
  Result := FDataSet.FInCacheProcessing;
end;

function TDADataSetService.IsAutoCommit: boolean;
begin
  Result := FDataSet.AutoCommit;
end;

procedure TDADataSetService.SetAutoCommit(Value: boolean);
begin
  FDataSet.AutoCommit := Value;
end;

function TDADataSetService.IsFetchAll: boolean;
begin
  Result := FDataSet.FetchAll;
end;

procedure TDADataSetService.SetNeedAddRef(Value: boolean);
begin
  FDataSet.FNeedAddRef := Value;
end;

function TDADataSetService.GetKeyFields: _string;
begin
  Result := FDataSet.KeyFields;
end;

procedure TDADataSetService.BeginConnection;
begin
  FDataSet.BeginConnection;
end;

procedure TDADataSetService.EndConnection;
begin
  FDataSet.EndConnection;
end;

function TDADataSetService.PreventPSKeyFields(var PSKeyFields: string): boolean;
begin
  Result := False;
end;

function TDADataSetService.NeedPreparePSExecuteCommand: boolean;
begin
  Result := False;
end;

function TDADataSetService.GetUpdatingTableInfo: TCRTableInfo;
begin
  if FUpdatingTableInfoIdx > - 1 then
    Result := GetIRecordSet.TablesInfo[FUpdatingTableInfoIdx]
  else
    Result := nil
end;

{ TCustomDASQL }

constructor TCustomDASQL.Create(Owner: TComponent);
begin
  inherited Create(Owner);

  FSQL := _TStringList.Create;
  _TStringList(FSQL).OnChange := SQLChanged;
  FParams := CreateParamsObject;
  FParamCheck := True;
  if Owner is TCustomDADataSet then
    FMacros := TMacros.Create(Owner)
  else
    FMacros := TMacros.Create(Self);
  FChangeCursor := True;

  FDesignCreate := csDesigning in ComponentState;
end;

destructor TCustomDASQL.Destroy;
var
  Owner: TComponent;
begin
  UnPrepare;

  if UsedConnection <> nil then
    UsedConnection.UnregisterClient(Self);

{$IFDEF FPC}
  FMacros.FOwner := nil; // to prevent assemble SQL on removing items
{$ENDIF}
  FMacros.Free;
  FParams.Clear; // To prevent SharedObj leak on CLR
  FParams.Free;
  FSQL.Free;

  Owner := Self.Owner;

  if (FDataSet = nil) and (UsedConnection <> nil) then
    TDASQLMonitorClass(UsedConnection.SQLMonitorClass).ObjectDestroyed(Self);

  inherited;

  if not (Owner is TCustomDADataSet) then  // temp
    FreeICommand;
end;

procedure TCustomDASQL.CreateICommand;
begin
  if UsedConnection <> nil then
    SetICommand(UsedConnection.CreateICommand)
  else
    SetICommand(nil);
end;

procedure TCustomDASQL.FreeICommand;
begin
  FICommand.Free;
  SetICommand(nil);
end;

procedure TCustomDASQL.SetICommand(Value: TCRCommand);
begin
  FICommand := Value;

  if FICommand <> nil then begin
    if FDataSet = nil then begin
      if FConnection <> nil then
        FICommand.SetConnection(FConnection.FIConnection)
      else
        FICommand.SetConnection(nil);

      if FTransaction <> nil then
        FICommand.SetTransaction(FTransaction.FITransaction)
      else
        FICommand.SetTransaction(nil);
    end;

    FICommand.SetProp(prAutoCommit, FAutoCommit);
    FICommand.AfterExecute := DoAfterExecute;
    if (Owner is TCustomDADataSet) or (Owner is TCustomDAConnection) then
      FICommand.Component := Owner
    else
      FICommand.Component := Self;

    FICommand.SetProp(prScanParams, ParamCheck);

    FICommand.SetProp(prIsStoredProc, FStoredProcName <> '');
    FICommand.ReadParams := ReadParams; // Used in SDAC

    SetICommandSQL; // in UniDAC FICommand can be recreated on provider change
  end;
end;

procedure TCustomDASQL.CheckICommand;
var
  ClassType: TClass;
begin
  if FDataSet <> nil then begin
    FDataSet.CheckIRecordSet;
    exit;
  end;
  if (UsedConnection <> nil) then
    ClassType := UsedConnection.GetICommandClass
  else
    ClassType := nil;

  if (ClassType = nil) or not (FICommand is ClassType) then begin
    FreeICommand;
    CreateICommand;
  end;
end;

function TCustomDASQL.CreateParamsObject: TDAParams;
begin
  Result := TDAParams.Create(Self);
end;

function TCustomDASQL.GetDataTypesMap: TDataTypesMapClass;
begin
  Result := TDataTypesMap;
end;

procedure TCustomDASQL.Loaded;
begin
  inherited;

  FDesignCreate := False;
end;

function TCustomDASQL.UsedConnection: TCustomDAConnection;
begin
  Result := FConnection;
  if (Result = nil) and (FDataSet <> nil) then
    Result := FDataSet.UsedConnection;
end;

procedure TCustomDASQL.CheckConnection;
begin
  BeginConnection(False);
end;

procedure TCustomDASQL.BeginConnection(NoConnectCheck: boolean = True);
var
  UseDefaultConnection: boolean;
  vUsedConnection: TCustomDAConnection;
  vUsedTransaction: TDATransaction;
begin
  vUsedConnection := UsedConnection;
  if vUsedConnection = nil then
    DatabaseError(SConnectionNotDefined);

  if NoConnectCheck then
    vUsedConnection.InternalConnect // We should call connect each time to update ConnectCount
  else
    if not vUsedConnection.Connected then
      vUsedConnection.Connect;

  UseDefaultConnection := (FConnection = nil) and (vUsedConnection.FSQLs.IndexOf(Self) = -1);

  CheckICommand;
  
  if vUsedConnection.IsMultipleTransactionsSupported then begin
    vUsedTransaction := UsedTransaction;
    if vUsedTransaction = nil then
      DatabaseError(STransactionNotAssigned);

    if NoConnectCheck then
      vUsedTransaction.GainTransaction // We should call each time to update TrStartCount
    else
     if not vUsedTransaction.Active then
       vUsedTransaction.StartTransaction;
       
    FICommand.SetTransaction(vUsedTransaction.FITransaction);
  end;

  // use default connection
  if UseDefaultConnection then begin
    vUsedConnection.RegisterClient(Self, ConnectChange);

    FICommand.SetConnection(vUsedConnection.FIConnection)
  end;
end;

procedure TCustomDASQL.EndConnection;
var
  vUsedConnection: TCustomDAConnection;
  vUsedTransaction: TDATransaction;
begin
  vUsedConnection := UsedConnection;
  if vUsedConnection.IsMultipleTransactionsSupported then begin
    vUsedTransaction := UsedTransaction;
    vUsedTransaction.ReleaseTransaction; // Release and Stop transaction
  end;  

  if vUsedConnection <> nil then
    vUsedConnection.InternalDisconnect;
end;

procedure TCustomDASQL.Disconnect;
begin
  if FDataSet = nil then begin
    UnPrepare;
    Params.Disconnect;
  end
  else
    FDataSet.Disconnect;
end;

procedure TCustomDASQL.ConnectChange(Sender: TObject; Connecting: boolean);
begin
  if not Connecting then
    Disconnect;
end;

function TCustomDASQL.UsedTransaction: TDATransaction;
var
  UsedCon: TCustomDAConnection;
begin
  UsedCon := UsedConnection;
  if UsedCon <> nil then begin
    if UsedCon.IsMultipleTransactionsSupported then begin
      Result := Transaction;
      if (Result = nil) and (FDataSet <> nil) then
        Result := FDataSet.UsedTransaction;
    end
    else
      Result := nil;

    if Result = nil then
      Result := UsedCon.UsedTransaction;
  end
  else
    Result := nil;
end;

procedure TCustomDASQL.Notification(AComponent: TComponent; Operation: TOperation);
begin
  inherited;

  if (Operation = opRemove) and (AComponent = FTransaction) then
    FTransaction := nil;
end;

procedure TCustomDASQL.InternalPrepare;
begin
  WriteParams(False);
  
  FICommand.Prepare;
end;

procedure TCustomDASQL.Prepare;
var
  MessageID: cardinal;
  v: variant;
begin
  if not Prepared then begin
    BeginConnection;

    if not FLockDebug and (TDASQLMonitorClass(UsedConnection.SQLMonitorClass).HasMonitor or Debug) then
      TDASQLMonitorClass(UsedConnection.SQLMonitorClass).SQLPrepare(Self, FinalSQL, FParams, MessageID, True);

    InternalPrepare;

    FICommand.GetProp(prUseDescribeParams, v);
    if Boolean(v) then
      UpdateParams;

    if not FLockDebug and (TDASQLMonitorClass(UsedConnection.SQLMonitorClass).HasMonitor or Debug) then
      TDASQLMonitorClass(UsedConnection.SQLMonitorClass).SQLPrepare(Self, FinalSQL, FParams, MessageID, False);
  end;
end;

procedure TCustomDASQL.InternalUnPrepare;
begin
  FICommand.Unprepare;
end;

procedure TCustomDASQL.UnPrepare;
begin
  if Prepared then begin
    try
      InternalUnPrepare;
    finally
      EndConnection; //Diconnect after no longer prepared
    end;
  end;
end;

procedure TCustomDASQL.InternalCreateProcCall(const Name: _string; NeedDescribe: boolean;
  IsQuery: boolean = False);
var
  ProcCallSQL: _string;
begin
  if Name = '' then
    DatabaseError(SStoredProcNotDefined);

  BeginConnection;
  try
    if NeedDescribe then begin
      ProcCallSQL := FICommand.CreateProcCall(Name, NeedDescribe, IsQuery);
      CreateParams;
    end
    else begin
      WriteParams(False);
      ProcCallSQL := FICommand.CreateProcCall(Name, NeedDescribe, IsQuery);
    end;

    FLockAssembleSQL := True;
    try
      SQL.Text := ProcCallSQL;
    finally
      FLockAssembleSQL := False;
    end;
  finally
    EndConnection;
  end;

  FStoredProcIsQuery := IsQuery;
  SetStoredProcName(Name);
end;

procedure TCustomDASQL.InternalExecute(Iters: integer);
var
  ReExecute: boolean;
begin
  if UsedConnection <> nil then
    UsedConnection.PushOperation(clExecute);
  try
    repeat
      ReExecute := False;
      try
        FICommand.Execute(Iters);
      except
        on E: EFailOver do
          if E.FConnLostCause = clExecute then begin
            Connection.RestoreAfterFailOver; //Restore all read transactions
            ReExecute := True; //We should pass clConnectionApplyUpdates FailOver
          end  
          else
            raise;
      end;
    until (not ReExecute);
  finally
    if UsedConnection <> nil then
      UsedConnection.PopOperation;
  end;
end;

procedure TCustomDASQL.Execute;
begin
  Execute(1);
end;

procedure TCustomDASQL.Execute(Iters: integer);
var
  MessageID: cardinal;
begin
  if not Executing then begin
    BeginConnection;

    if not FLockDebug and (TDASQLMonitorClass(UsedConnection.SQLMonitorClass).HasMonitor or Debug) then
      TDASQLMonitorClass(UsedConnection.SQLMonitorClass).SQLExecute(Self, FinalSQL, FParams, '', MessageID, True);

    if FChangeCursor then
      if FNonBlocking then
        SetCursor(crSQLArrow)
      else
        StartWait;

    WriteParams;
    CheckSQL(Iters);
    InternalExecute(Iters);

    if not FLockDebug and (TDASQLMonitorClass(UsedConnection.SQLMonitorClass).HasMonitor or Debug) then
      TDASQLMonitorClass(UsedConnection.SQLMonitorClass).SQLExecute(Self, FinalSQL, FParams, '', MessageID, False);
  end;
end;

procedure TCustomDASQL.DoBeforeExecute;
begin
  if Assigned(FBeforeExecute) then
    FBeforeExecute(self);
end;

procedure TCustomDASQL.DoAfterExecute(Result: boolean);
var
  Connection: TCustomDAConnection;
  Value: variant;
  AutoCommitUsed: boolean;
begin
  Connection := UsedConnection;

  if Result then begin
    ReadParams;

    Connection.FIConnection.GetProp(prLastInsertId, Value);
  {$IFNDEF VER6P}
    if VarType(Value) = $E then
      FLastInsertId := PInt64(@TVarData(Value).VInteger)^
    else
  {$ELSE}
    FLastInsertId := Value;
  {$ENDIF}
  end;

  if FChangeCursor and FNonBlocking then
    StopWait;

  AutoCommitUsed := Connection.AutoCommit and AutoCommit;

  if Connection.Options.DisconnectedMode and Connection.Connected then
    UsedConnection.DetectInTransaction(not AutoCommitUsed);

  if UsedConnection.IsMultipleTransactionsSupported then
    UsedTransaction.AutoCommitTransaction(AutoCommitUsed);

  EndConnection; //we should read all Out parameters before disconnect, so
                 //in NonBlocking Mode this event must be called exactly after server execute
  if Assigned(FAfterExecute) then
    FAfterExecute(Self, Result);
end;

function TCustomDASQL.Executing: boolean;
var
  Value: variant;
begin
  if FICommand <> nil then begin
    FICommand.GetProp(prExecuting, Value);
    Result := Value;
  end
  else
    Result := False;
end;

function TCustomDASQL.WaitExecuting(TimeOut: integer): boolean;
{$IFDEF MSWINDOWS}
var
  Msg: TMSG;
  T: DWORD;
{$ENDIF}
begin
{$IFDEF MSWINDOWS}
  T := GetTickCount;
  while Executing and ((TimeOut = 0) or (GetTickInterval(T, GetTickCount) < DWORD(TimeOut * 1000))) do
    if PeekMessage(Msg, 0, 0, 0, PM_REMOVE) then begin
      if Msg.Message <> WM_QUIT then begin
        TranslateMessage(Msg);
        DispatchMessage(Msg);
      end;
    end;
{$ELSE}
  while Executing do;
{$ENDIF}

  Result := not Executing;
end;

procedure TCustomDASQL.ScanMacros;
begin
  if FDataSet = nil then
    FMacros.Scan(FSQL.Text)
  else
    FDataSet.ScanMacros{$IFDEF FPC}(nil){$ENDIF};
end;

function TCustomDASQL.GetFinalSQL: _string;
var
  i: Integer;
begin
  Result := FSQL.Text;

  // Copied from SysUtils
  i := Length(Result);
  while (i > 0) and (Result[i] <= ' ') do
    Dec(i);
  SetLength(Result, i);

  if FMacros.Count > 0 then
    FMacros.Expand(Result);
end;


function TCustomDASQL.ParseSQL(const SQL: _string; Params: TDAParams; RenamePrefix: _string = ''): _string;
var
  ParamDescs: TParamDescs;
begin
  CheckICommand;
  ParamDescs := TParamDescs.Create;
  try
    Result := FICommand.ParseSQL(SQL, ParamDescs, True, RenamePrefix);
    CreateParams(Params, ParamDescs);
  finally
    ParamDescs.Free;
  end;
end;

procedure TCustomDASQL.SetICommandSQL;
var
  FinSQL: _string;
begin
  FinSQL := '';
  if FDataSet = nil then begin
    if SQL.Count > 0 then // don't generate SQL here
      FinSQL := FinalSQL;
  end
  else begin
    if FDataSet.SQL.Count > 0 then // don't generate SQL here
      FinSQL := FDataSet.FinalSQL;
  end;

  if FLockScanParams then begin
    FICommand.SetProp(prDisableParamScan, True);
    try
      FICommand.SetSQL(FinSQL);
    finally
      FICommand.SetProp(prDisableParamScan, False);
    end;
  end
  else
    FICommand.SetSQL(FinSQL); // replace parameters if it's needed
end;

procedure TCustomDASQL.AssembleSQL;
var
  List: TDAParams;
  OldICommand: TCRCommand;
begin
{$IFDEF PERF_COUNTER}
  //PerfCounters[3].Start;
{$ENDIF}

  OldICommand := FICommand;
  // FDataSet.FinalSQL uses TablesInfo from FIRecordSet
  CheckICommand;

  if FICommand = OldICommand then
    SetICommandSQL;
  // else SetICommandSQL was called from SetICommand

  if (ParamCheck or (csDesigning in ComponentState)) and not FLockScanParams
  then begin
    List := CreateParamsObject;
    try
      // Internal param parsing
      List.Assign(FParams);
      CreateParams;
      FParams.AssignValues(List);
    finally
      List.Clear;
      List.Free;
    end;
  end;
{$IFDEF PERF_COUNTER}
  //PerfCounters[3].Stop;
{$ENDIF}
end;

function TCustomDASQL.NeedRecreateProcCall: boolean;
begin
  Result := False;
end;

procedure TCustomDASQL.CheckSQL(Iters: integer);
var
  OldSQL, NewUserSQL: _string;
  OldPrepared: boolean;
begin
  OldSQL := FICommand.SQL;
  NewUserSQL := '';
  OldPrepared := Prepared;

  // NeedRecreateProcCall used for MSSQL that has a flag for default params
  if (Iters = 1) and (FStoredProcName <> '') and NeedRecreateProcCall then
    NewUserSQL := FICommand.CreateProcCall(FStoredProcName, False, FStoredProcIsQuery);

  if FICommand.SQL <> OldSQL then begin
    if OldPrepared then
      if FDataSet = nil then
        Unprepare
      else
        FDataSet.UnPrepare;

    if NewUserSQL <> '' then begin
      FLockAssembleSQL := True;
      try
        SQL.Text := NewUserSQL;
      finally
        FLockAssembleSQL := False;
      end;
    end;

    if OldPrepared then
      if FDataSet = nil then
        Prepare
      else
        FDataSet.Prepare;
  end;
end;

// creates TDAParam objects if parameters was parsed by FICommand
procedure TCustomDASQL.CreateParams;
begin
  CreateParams(FParams, FICommand.Params);
end;

procedure TCustomDASQL.CreateParams(Params: TDAParams; ParamDescs: TParamDescs);
var
  i: integer;
begin
  Params.BeginUpdate;
  try
    Params.Clear;
    for i := 0 to ParamDescs.Count - 1 do begin
      AssignParamDesc(Params.Add as TDAParam, ParamDescs[i]);
    end;
  finally
    Params.EndUpdate;
  end;
end;

// Write values of parameters to FICommand
procedure TCustomDASQL.WriteParams(WriteValue: boolean = True);
var
  Param: TDAParam;
  ParamDesc: CRAccess.TParamDesc;
  i: integer;
begin
  for i := 0 to Params.Count - 1 do begin
    Param := Params[i];
    if (Params.FParamsChangeType = ctUsers) and (Param.Name <> '') then
      ParamDesc := FICommand.FindParam(Param.Name)
    else
      if i < FICommand.GetParamCount then
        ParamDesc := FICommand.GetParam(i)
      else
        ParamDesc := nil;
    if ParamDesc = nil then
      ParamDesc := FICommand.AddParam;

    AssignParam(ParamDesc, Param);

    if WriteValue then begin
    {$IFDEF PERF_COUNTER}
      PerfCounters[5].Start;
    {$ENDIF}
      AssignParamValue(ParamDesc, Param);
    {$IFDEF PERF_COUNTER}
      PerfCounters[5].Stop;
    {$ENDIF}
    end;
  end;

  if Params.FParamsChangeType <> ctUsers then
    while Params.Count < FICommand.GetParamCount do
      FICommand.DeleteParam(FICommand.GetParamCount - 1);
  Params.FParamsChangeType := ctGenerated;
end;

// Read values of parameters from FICommand
procedure TCustomDASQL.ReadParams;
var
  CanReadParams: Variant;
  i: integer;
  Param: TDAParam;
  ParamDesc: TParamDesc;
begin
  Assert(FICommand <> nil);
  FICommand.GetProp(prCanReadParams, CanReadParams);
  if CanReadParams then begin
    for i := 0 to FParams.Count - 1 do begin
      Param := Params[i];
      if (Param.ParamType <> ptInput) and
        (IsInOutParamSupported or (Param.ParamType <> ptUnknown)) // if in/out not supported treat Unknown as Input
      then begin
        ParamDesc := FICommand.GetParam(i);
        if ParamDesc <> nil then
          AssignParamDescValue(Param, ParamDesc);
      end;
    end;
    FICommand.SetProp(prCanReadParams, False); // For SDAC
  end;
end;

procedure TCustomDASQL.UpdateParams;
var
  i: integer;
  Param: TDAParam;
begin
  Params.BeginUpdate;
  try
    for i := 0 to FICommand.Params.Count - 1 do begin
      if i < Params.Count then
        Param := Params[i]
      else
        Param := Params.Add as TDAParam;
      AssignParamDesc(Param, FICommand.Params[i]);
    end;
    for i := Params.Count - 1 downto FICommand.Params.Count do
      Params[i].Free;
  finally
    Params.EndUpdate;
  end;
end;

function TCustomDASQL.IsInOutParamSupported: boolean;
begin
  Result := True;
end;

function TCustomDASQL.NeedConvertEOLForBlob: boolean;
begin
  Result := False;
end;

procedure TCustomDASQL.AssignParam(ParamDesc: TParamDesc; Param: TDAParam);
var
  ParamSize: integer;
  MaxStringSize: Variant;
begin
  ParamDesc.SetName(Param.Name);
  ParamDesc.SetDataType(GetDataTypesMap.GetDataType(Param.DataType));
  ParamDesc.SetSubDataType(Param.SubDataType);
{$IFDEF VER14P}
  if (Param.DataType = TFieldType.ftSingle) and (ParamDesc.GetSubDataType = 0) then
    ParamDesc.SetSubDataType(dtSingle);
{$ENDIF}
  ParamDesc.SetNational(Param.National);
  ParamDesc.SetParamType(TParamDirection(Param.ParamType));
  ParamDesc.SetIsBound(Param.Bound);

  ParamSize := Param.Size;
  if (Param.DataType in [ftString, ftFixedChar, ftWideString, ftBytes, ftVarBytes]) or
    (Integer(Param.DataType) = Integer(ftFixedWideChar))
  then begin
    if Param.Size = 0 then begin
      if (Param.ParamType = ptInput) or
        (Param.ParamType = ptUnknown) and not IsInOutParamSupported // if in/out not supported treat Unknown as Input
      then begin
        case VarType(Param.Value) of
          varArray + varByte:
            ParamSize := VarArrayHighBound(Param.Value, 1) + 1;
        else
          if VarIsStr(Param.Value) then
            ParamSize := Length(AnsiString(VarToStr(Param.Value)));
        end;
      end;

      if ParamSize = 0 then begin
        UsedConnection.FIConnection.GetProp(prMaxStringSize, MaxStringSize);
        ParamSize := MaxStringSize;
      end;
    end;
  end;

  ParamDesc.SetSize(ParamSize); // Note: ParamSize in chars
end;

procedure TCustomDASQL.AssignParamValue(ParamDesc: TParamDesc; Param: TDAParam);
var
  Value: variant;
  Blob: TBlob;
  OldRollback: boolean;
  Value1: TBytes;
  ParamObject: TSharedObject;
begin
  Value1 := nil;
  if Param.IsObjectDataType then begin
    ParamObject := Param.GetNativeParamObject;
    if Param.ParamType in [ptUnknown, ptInput, ptInputOutput] then begin
      if UsedConnection.ConvertEOL and
        ((Param.DataType in [ftMemo{$IFDEF VER10P}, ftWideMemo{$ENDIF}, ftOraClob]) or
        (Param.DataType = ftBlob) and NeedConvertEOLForBlob)
      then begin
        Blob := TBlob(ParamObject);
        OldRollback := Blob.RollbackEnabled;
        Blob.RollbackEnabled := False;
        try
          Blob.RemoveCR;
        finally
          Blob.RollbackEnabled := OldRollback;
        end;
      end;
    end
    else
      if (Param.DataType in [ftMemo{$IFDEF VER10P}, ftWideMemo{$ENDIF}, ftBlob, ftOraBlob, ftOraClob])
      then begin
        TBlob(ParamObject).FreeBlob;
        TBlob(ParamObject).Clear;
      end;
    ParamDesc.SetObject(ParamObject);
    ParamDesc.SetNull(Param.IsNull);
  end
  else begin
    ParamDesc.SetConvertEOL(UsedConnection.ConvertEOL);
    if Param.ParamType in [ptUnknown, ptInput, ptInputOutput] then begin
      Value := Param.Value;

      if (Param.DataType = ftDate) and not (VarIsEmpty(Value) or VarIsNull(Value)) then
        Value := {$IFDEF CLR}Borland.Delphi.{$ENDIF}System.Int(Value); // drop time info

      // Convert param values if necessary
      if ((ParamDesc.GetDataType = dtBytes) or (ParamDesc.GetDataType = dtVarBytes))
        and VarIsStr(Value)
      then begin
        Value1 := Encoding.Default.GetBytes(AnsiString(Value));
        Value := Unassigned;
        Value := Value1;
      end;

      ParamDesc.SetValue(Unassigned);
      ParamDesc.SetValue(Value);
    end;
  end;
end;

procedure TCustomDASQL.AssignParamDesc(Param: TDAParam; ParamDesc: TParamDesc);
begin
  Param.Name := ParamDesc.GetName;
  Param.DataType := GetDataTypesMap.GetFieldType(ParamDesc.GetDataType);
  Param.ParamType := TParamType(ParamDesc.GetParamType);
  Param.Size := ParamDesc.GetSize;
end;

procedure TCustomDASQL.AssignParamDescValue(Param: TDAParam; ParamDesc: TParamDesc);
var
  Blob: TBlob;
  OldRollback: boolean;
begin
  if Param.IsObjectDataType then begin
    if (Param.DataType in [ftBlob, ftMemo{$IFDEF VER10P}, ftWideMemo{$ENDIF}, ftOraClob]) and
      UsedConnection.ConvertEOL
    then begin
      Blob := TBlob(Param.ParamObject);
      OldRollback := Blob.RollbackEnabled;
      Blob.RollbackEnabled := False;
      try
        Blob.AddCR;
      finally
        Blob.RollbackEnabled := OldRollback;
      end;
    end;

    if not Param.IsBlobDataType then
      Param.SetIsNull(ParamDesc.GetNull);
  end
  else begin
    ParamDesc.SetConvertEOL(UsedConnection.ConvertEOL);
    Param.Value := ParamDesc.GetValue;
  end;
end;

function TCustomDASQL.FindResultParam: TDAParam;
var
  i: integer;
begin
  Result := nil;

  for i := 0 to Params.Count - 1 do
    if Params[i].ParamType = ptResult then
      Result := Params[i];
end;

procedure TCustomDASQL.DefineProperties(Filer: TFiler);
  function InternalWriteParams: boolean;
  begin
    if Filer.Ancestor <> nil then
      Result := not FParams.IsEqual(TCustomDASQL(Filer.Ancestor).FParams)
    else
      Result := FParams.Count > 0;
  end;

  function WriteMacros: boolean;
  begin
    if Filer.Ancestor <> nil then
      Result := not FMacros.IsEqual(TCustomDASQL(Filer.Ancestor).FMacros)
    else
      Result := FMacros.Count > 0;
  end;

begin
  inherited DefineProperties(Filer);

  Filer.DefineProperty('ParamData', ReadParamData, WriteParamData, InternalWriteParams);
  Filer.DefineProperty('MacroData', ReadMacroData, WriteMacroData, WriteMacros);
  Filer.DefineProperty('CommandStoredProcName', ReadStoredProcName, WriteStoredProcName,
    FStoredProcName <> '');
  Filer.DefineProperty('StoredProcIsQuery', ReadStoredProcIsQuery, WriteStoredProcIsQuery,
    FStoredProcIsQuery);
end;

procedure TCustomDASQL.ReadParamData(Reader: TReader);
begin
  Reader.ReadValue;
  Reader.ReadCollection(FParams);
end;

procedure TCustomDASQL.WriteParamData(Writer: TWriter);
begin
  Writer.WriteCollection(FParams);
end;

procedure TCustomDASQL.ReadMacroData(Reader: TReader);
begin
  Reader.ReadValue;
  Reader.ReadCollection(FMacros);
end;

procedure TCustomDASQL.WriteMacroData(Writer: TWriter);
begin
  Writer.WriteCollection(FMacros);
end;

procedure TCustomDASQL.ReadStoredProcName(Reader: TReader);
begin
  SetStoredProcName(Reader.ReadString);
end;

procedure TCustomDASQL.WriteStoredProcName(Writer: TWriter);
begin
  Writer.WriteString(FStoredProcName);
end;

procedure TCustomDASQL.SetStoredProcName(const StoredProcName: _string);
begin
  FStoredProcName := StoredProcName;
  if FICommand <> nil then
    FICommand.SetProp(prIsStoredProc, StoredProcName <> '');
end;

procedure TCustomDASQL.ReadStoredProcIsQuery(Reader: TReader);
begin
  FStoredProcIsQuery := Reader.ReadBoolean;
end;

procedure TCustomDASQL.WriteStoredProcIsQuery(Writer: TWriter);
begin
  Writer.WriteBoolean(FStoredProcIsQuery);
end;

function TCustomDASQL.FindParam(const Value: _string): TDAParam;
begin
  Result := FParams.FindParam(Value);
end;

function TCustomDASQL.ParamByName(const Value: _string): TDAParam;
begin
  Result := FParams.ParamByName(Value);
end;

function TCustomDASQL.FindMacro(const Value: _string): TMacro;
begin
  Result := FMacros.FindMacro(Value);
end;

function TCustomDASQL.MacroByName(const Value: _string): TMacro;
begin
  Result := FMacros.MacroByName(Value);
end;

procedure TCustomDASQL.AssignTo(Dest: TPersistent);
begin
  if Dest is TCustomDASQL then begin
    TCustomDASQL(Dest).Connection := Connection;
    TCustomDASQL(Dest).ParamCheck := ParamCheck;  // before SQL
    TCustomDASQL(Dest).SQL.Text := SQL.Text;
    TCustomDASQL(Dest).Macros.Assign(Macros);
    TCustomDASQL(Dest).Params.Assign(Params);
    TCustomDASQL(Dest).Debug := Debug;
    TCustomDASQL(Dest).AutoCommit := AutoCommit;
    TCustomDASQL(Dest).FStoredProcName := FStoredProcName;
    TCustomDASQL(Dest).FStoredProcIsQuery := FStoredProcIsQuery;
  end
  else
    inherited;
end;

procedure TCustomDASQL.SetConnection(Value: TCustomDAConnection);
begin
  if (Value <> FConnection) or (Value <> UsedConnection) then begin
    if UsedConnection <> nil then begin
      Disconnect;
      UsedConnection.UnregisterClient(Self);
    end;

    FConnection := Value;

    if FConnection <> nil then begin
      Value.RegisterClient(Self, ConnectChange);

      if FICommand <> nil then
        FICommand.SetConnection(FConnection.FIConnection);
    end
    else
      if FICommand <> nil then
        FICommand.SetConnection(nil);
  end;
end;

procedure TCustomDASQL.SetTransaction(Value: TDATransaction);
begin
  if Value <> FTransaction then begin
    if FTransaction <> nil then
      RemoveFreeNotification(FTransaction);
    FTransaction := Value;
    if FTransaction <> nil then
      FreeNotification(FTransaction);
  end;
end;

function TCustomDASQL.GetTransaction: TDATransaction;
begin
  Result := FTransaction;
end;

procedure TCustomDASQL.SetSQL(Value: _TStrings);
begin
 if FSQL.Text <> Value.Text then begin
    FSQL.BeginUpdate;
    try
      FSQL.Assign(Value);
    finally
      FSQL.EndUpdate;
    end;
  end;
end;

procedure TCustomDASQL.SQLChanged(Sender: TObject);
begin
  ProcessSQLChanged(FLockMacros, BaseSQLOldBehavior);
end;

procedure TCustomDASQL.ProcessSQLChanged(LockMacros, SaveBaseSQL: boolean);
var
  Cmd: TCRCommand;
begin
  //if not (csReading in ComponentState) then begin
  if FDataSet = nil then
    UnPrepare
  else begin
    if not SaveBaseSQL then
      FDataSet.FBaseSQL := '';
    FDataSet.Close;
    FDataSet.UnPrepare;
    FDataSet.FieldDefs.Updated := False;

    if not FLockAssembleSQL and (FDataSet.Data is TCRRecordSet) then begin
      Cmd := TCRRecordSet(FDataSet.Data).GetCommand;
      if Cmd <> nil then
        Cmd.SetCursorState(csInactive);
      TCRRecordSet(FDataSet.Data).CommandType := ctUnknown;
    end;

  {$IFDEF WITH_IPROVIDER}
    FDataSet.FOldTableName := '';
    FDataSet.FOldKeyFields := '';
  {$ENDIF}
  end;

  if not LockMacros then
    ScanMacros;

  if not FLockAssembleSQL then
    AssembleSQL;

  SetStoredProcName('');
end;

function TCustomDASQL.GetPrepared: boolean;
begin
  if FICommand <> nil then
    Result := FICommand.GetPrepared
  else
    Result := False;
end;

procedure TCustomDASQL.SetPrepared(Value: boolean);
begin
  if Value then
    Prepare
  else
    UnPrepare;
end;

procedure TCustomDASQL.SetParams(Value: TDAParams);
begin
  FParams.AssignValues(Value);
end;

function TCustomDASQL.GetParamCount: word;
begin
  Result := FParams.Count;
end;

procedure TCustomDASQL.SetParamCheck(Value: boolean);
begin
  FParamCheck := Value;

  Value := Value or (csDesigning in ComponentState); // set value of ScanParams
  if FICommand <> nil then
    FICommand.SetProp(prScanParams, Value);

  if Value then
    AssembleSQL;
end;

function TCustomDASQL.GetParamValues(ParamName: _string): variant;
begin
  Result := FParams.ParamValues[ParamName];
end;

procedure TCustomDASQL.SetParamValues(ParamName: _string; Value: variant);
begin
  FParams.ParamValues[ParamName] := Value;
end;

procedure TCustomDASQL.SetMacros(Value: TMacros);
begin
  FMacros.Assign(Value);
end;

function TCustomDASQL.GetMacroCount: word;
begin
  Result := FMacros.Count;
end;

procedure TCustomDASQL.SetAutoCommit(Value: boolean);
begin
  FAutoCommit := Value;
  if FICommand <> nil then
    FICommand.SetProp(prAutoCommit, FAutoCommit);
end;

function TCustomDASQL.GetRowsAffected: integer;
var
  Value: variant;
begin
  if FICommand <> nil then begin
    FICommand.GetProp(prRowsProcessed, Value);
    Result := Value;
  end
  else
    Result := 0;
end;

function TCustomDASQL.IsTransactionStored: boolean;
begin
  Result := FTransaction <> nil;
end;

{ TDAMetaData }

constructor TDAMetaData.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);

  FreeIRecordSet;
  SetIRecordSet(nil);

  FRestrictions := _TStringList.Create;
  _TStringList(FRestrictions).OnChange := RestrictionsChanged;

  FDesignCreate := csDesigning in ComponentState;
end;

destructor TDAMetaData.Destroy;
begin
  Close;
  FIMetaData.Free;
  FRestrictions.Free;
  SetIRecordSet(nil);
  if FConnection <> nil then
    FConnection.UnRegisterClient(Self);

  inherited Destroy;
end;

procedure TDAMetaData.GetMetaDataKinds(List: _TStrings);
begin
  CheckIMetaData;

  FIMetaData.GetMetaDataKindsList(List);
end;

procedure TDAMetaData.GetRestrictions(List: _TStrings; const MetaDataKind: _string);
begin
  CheckIMetaData;

  FIMetaData.GetRestrictionsList(List, MetaDataKind);
end;

procedure TDAMetaData.Loaded;
begin
  inherited;

  FDesignCreate := False;
end;

procedure TDAMetaData.Notification(AComponent: TComponent; Operation: TOperation);
begin
  inherited;

  if (Operation = opRemove) then
    if (AComponent = FConnection) then
      Connection := nil
    else
    if (AComponent = FTransaction) then
      Transaction := nil;
end;

function TDAMetaData.UsedConnection: TCustomDAConnection;
begin
  Result := FConnection;
end;

function TDAMetaData.UsedTransaction: TDATransaction;
var
  UsedCon: TCustomDAConnection;
begin
  UsedCon := UsedConnection;
  if UsedCon <> nil then begin
    if UsedCon.IsMultipleTransactionsSupported then
      Result := Transaction
    else
      Result := nil;

    if Result = nil then
      Result := UsedCon.UsedTransaction;
  end
  else
    Result := nil;
end;

function TDAMetaData.IsTransactionStored: boolean;
begin
  Result := FTransaction <> nil;
end;

procedure TDAMetaData.BeginConnection;
var
  vUsedConnection: TCustomDAConnection;
  vUsedTransaction: TDATransaction;
begin
  vUsedConnection := UsedConnection;
  if vUsedConnection = nil then
    DatabaseError(SConnectionNotDefined);

  vUsedConnection.InternalConnect;

  if vUsedConnection.IsMultipleTransactionsSupported then begin
    vUsedTransaction := UsedTransaction;
    Assert(vUsedTransaction <> nil);
    vUsedTransaction.GainTransaction;
  end;
end;

procedure TDAMetaData.EndConnection;
var
  vUsedConnection: TCustomDAConnection;
  vUsedTransaction: TDATransaction;
begin
  vUsedConnection := UsedConnection;

  vUsedConnection.InternalDisconnect;
  if vUsedConnection.IsMultipleTransactionsSupported then begin
    vUsedTransaction := UsedTransaction;
    vUsedTransaction.ReleaseTransaction;
  end;
end;

procedure TDAMetaData.CheckIMetaData;
var
  ClassType: TCRMetaDataClass;
begin
  Assert(UsedConnection <> nil);
  ClassType := UsedConnection.GetIMetaDataClass;

  if not (FIMetaData is ClassType) then begin
    FIMetaData.Free;
    FIMetaData := ClassType.Create;
  end;
end;

procedure TDAMetaData.OpenCursor(InfoQuery: boolean);
var
  ReOpen: boolean;
begin
  if UsedConnection <> nil then
    UsedConnection.PushOperation(clOpen, UsedConnection.IsFailOverAllowed);
  try
  BeginConnection;
  //StartWait;
  try
    repeat
      ReOpen := False;
      try
        inherited;
      except
          on EFailOver do begin //TODO: Add check for clOpen operation
            UsedConnection.RestoreAfterFailOver;
            Reopen := True;
          end
          else
            raise;
      end;
    until (not ReOpen);
  finally
    EndConnection;
  end;
  finally
    if UsedConnection <> nil then
      UsedConnection.PopOperation;
  end;
end;

procedure TDAMetaData.InternalOpen;
var
  AData: TData;
begin
  CheckIMetaData;
  AData := FIMetaData.GetMetaData(UsedConnection.FIConnection, UsedTransaction.FITransaction,
    FMetaDataKind, FRestrictions);
  SetIRecordSet(AData);

  inherited InternalOpen;
end;

procedure TDAMetaData.CloseCursor;
begin
  inherited;

  FieldDefs.Updated := False;
  SetIRecordSet(nil);
end;

procedure TDAMetaData.SetConnection(Value: TCustomDAConnection);
begin
  if (Value <> FConnection) or (Value <> UsedConnection) then begin

    if UsedConnection <> nil then begin
      if not (csReading in ComponentState) then
        Close;
      UsedConnection.UnregisterClient(Self);
    end;

    FConnection := Value;

    if FConnection <> nil then
      Value.RegisterClient(Self, ConnectChange);
  end;
end;

procedure TDAMetaData.ConnectChange(Sender: TObject; Connecting: boolean);
begin
  if not Connecting then begin
    if not TCustomDAConnection(Sender).Options.DisconnectedMode then
      Close
    else
      if Data is TCRRecordSet then
        TCRRecordSet(Data).Disconnect;
  end;
end;

function TDAMetaData.GetTransaction: TDATransaction;
begin
  Result := FTransaction;
end;

procedure TDAMetaData.SetTransaction(Value: TDATransaction);
begin
  if FTransaction <> Value then begin
    if FTransaction <> nil then
      RemoveFreeNotification(FTransaction);
    FTransaction := Value;
    if FTransaction <> nil then
      FreeNotification(FTransaction);
  end;
end;

procedure TDAMetaData.SetMetaDataKind(const Value: _string);
begin
  if Value <> FMetaDataKind then begin
    if not (csReading in ComponentState) then
      Close;
    FMetaDataKind := Value;
  end;
end;

procedure TDAMetaData.SetRestrictions(Value: _TStrings);
begin
  FRestrictions.Assign(Value);
end;

procedure TDAMetaData.RestrictionsChanged(Sender: TObject);
begin
  if not (csReading in ComponentState) then
    Close;
end;

{ TCustomDAUpdateSQL }

constructor TCustomDAUpdateSQL.Create(Owner: TComponent);
var
  UpdateKind: TUpdateKind;
begin
  inherited;

  for UpdateKind := Low(TUpdateKind) to High(TUpdateKind) do
    FSQLText[UpdateKindToStatementType(UpdateKind)] := _TStringList.Create;
  FSQLText[stRefresh] := _TStringList.Create;
  FSQLText[stLock] := _TStringList.Create;
end;

destructor TCustomDAUpdateSQL.Destroy;
var
  StatementType: TStatementType;
begin
  if Assigned(FDataSet) and (FDataSet.UpdateObject = Self) then
    FDataSet.UpdateObject := nil;
  for StatementType := Low(TStatementType) to High(TStatementType) do
    FSQLText[StatementType].Free;

  inherited;
end;

procedure TCustomDAUpdateSQL.ExecSQL(UpdateKind: TUpdateKind);
var
  StatementType: TStatementType;
begin
  StatementType := UpdateKindToStatementType(UpdateKind);
  FDataSet.CheckDataSetService;
  FDataSet.FDataSetService.FUpdater.PerformSQL(FSQLText[StatementType].Text, [StatementType]);
end;

function TCustomDAUpdateSQL.GetSQL(UpdateKind: TUpdateKind): _TStrings;
begin
  Result := GetSQLIndex(Ord(UpdateKindToStatementType(UpdateKind)));
end;

procedure TCustomDAUpdateSQL.SetSQL(UpdateKind: TUpdateKind; Value: _TStrings);
begin
  SetSQLIndex(Ord(UpdateKindToStatementType(UpdateKind)), Value);
end;

function TCustomDAUpdateSQL.GetSQLIndex(Index: integer): _TStrings;
begin
  Result := FSQLText[TStatementType(Index)];
end;

procedure TCustomDAUpdateSQL.SetSQLIndex(Index: integer; Value: _TStrings);
begin
  FSQLText[TStatementType(Index)].Assign(Value);
end;

function TCustomDAUpdateSQL.GetDataSet: TCustomDADataSet;
begin
  Result := FDataSet;
end;

procedure TCustomDAUpdateSQL.SetDataSet(DataSet: TCustomDADataSet);
begin
  FDataSet := DataSet;
end;

procedure TCustomDAUpdateSQL.SetObjectIndex(Index: integer; Value: TComponent);
begin
  CheckUpdateComponent(Value);
  FUpdateObject[TStatementType(Index)] := Value;
end;

function TCustomDAUpdateSQL.GetObjectIndex(Index: integer): TComponent;
begin
  Result := FUpdateObject[TStatementType(Index)];
end;

function TCustomDAUpdateSQL.DataSetClass: TCustomDADataSetClass;
begin
  Result := TCustomDADataSet;
end;

function TCustomDAUpdateSQL.SQLClass: TCustomDASQLClass;
begin
  Result := TCustomDASQL;
end;

procedure TCustomDAUpdateSQL.CheckUpdateComponent(Component: TComponent; NewDataset: TCustomDADataset);
begin
  if Component <> nil then begin
    if not ((Component is SQLClass) or (Component is DataSetClass)) then
      raise Exception.Create(Format(SUpdateComponentInvalidType, [DataSetClass.ClassName, SQLClass.ClassName]));
    if NewDataSet = Component then
      raise Exception.Create(SUpdateComponentCircularReferences);
  end;
end;

procedure TCustomDAUpdateSQL.CheckUpdateComponent(Component: TComponent);
begin
  CheckUpdateComponent(Component, FDataset);
end;

procedure TCustomDAUpdateSQL.Notification(AComponent: TComponent; Operation: TOperation);
var
  stIdx: TStatementType;
begin
  inherited;
  if Operation = opRemove then
    for stIdx := Low(FUpdateObject) to High(FUpdateObject) do
      if FUpdateObject[stIdx] = AComponent then
        FUpdateObject[stIdx] := nil;
end;

procedure TCustomDAUpdateSQL.Apply(UpdateKind: TUpdateKind);
begin
  ExecSQL(UpdateKind);
end;

procedure TCustomDAUpdateSQL.Loaded;
begin
  inherited;

  FDesignCreate := False;
end;

procedure TCustomDAUpdateSQL.AssignTo(Dest: TPersistent);
begin
  if Dest is TCustomDAUpdateSQL then begin
    TCustomDAUpdateSQL(Dest).RefreshSQL := RefreshSQL;
    TCustomDAUpdateSQL(Dest).ModifySQL := ModifySQL;
    TCustomDAUpdateSQL(Dest).InsertSQL := InsertSQL;
    TCustomDAUpdateSQL(Dest).DeleteSQL := DeleteSQL;
    TCustomDAUpdateSQL(Dest).DataSet := DataSet;
    TCustomDAUpdateSQL(Dest).RefreshObject := RefreshObject;
    TCustomDAUpdateSQL(Dest).ModifyObject := ModifyObject;
    TCustomDAUpdateSQL(Dest).InsertObject := InsertObject;
    TCustomDAUpdateSQL(Dest).DeleteObject := DeleteObject;
  end
  else
    inherited;
end;

{ TMacro }

constructor TMacro.Create(Collection: TCollection);
begin
  inherited;

  FActive := True;
end;

procedure TMacro.AssignTo(Dest: TPersistent);
begin
  if Dest is TMacro then begin
    TMacro(Dest).Name := Name;
    TMacro(Dest).Value := Value;
    TMacro(Dest).Active := Active;
  end
  else
    inherited;
end;

function TMacro.IsEqual(Value: TMacro): boolean;
begin
  Result := (Name = Value.Name) and
    (Self.Value = Value.Value) and
    (Active = Value.Active);
end;

function TMacro.GetDisplayName: string;
begin
  if FName = '' then
    Result := inherited GetDisplayName
  else
    Result := FName;
end;

procedure TMacro.SetValue(Value: _string);
begin
  if Value <> FValue then begin
    FValue := Value;
    TMacros(Collection).NotifyOwner(Self);
  end;
end;

function TMacro.GetAsDateTime: TDateTime;
var
  St: string;
  iStart: integer;
  iEnd: integer;
  Len: integer;
begin
  St := Trim(FValue);
  Len := Length(FValue);
  if (Len > 0) and (St[1] = '''') then
    iStart := 2
  else
    iStart := 1;

  if (Len > 0) and (St[Length(St)] = '''') then
    iEnd := Length(St) - 1
  else
    iEnd := Length(St);

  Result := StrToDateTime(Copy(St, iStart, iEnd - iStart + 1));
end;

procedure TMacro.SetAsDateTime(Value: TDateTime);
begin
  Self.Value := '''' + DateTimeToStr(Value) + '''';
end;

function TMacro.GetAsFloat: double;
begin
  Result := StrToFloat(FValue);
end;

procedure TMacro.SetAsFloat(Value: double);
begin
  Self.Value := FloatToStr(Value);
end;

function TMacro.GetAsInteger: integer;
begin
  Result := StrToInt(FValue);
end;

procedure TMacro.SetAsInteger(Value: integer);
begin
  Self.Value := IntToStr(Value);
end;

function TMacro.GetAsString: _string;
var
  St: _string;
  iStart: integer;
  iEnd: integer;
  Len: integer;
begin
  St := Trim(FValue);
  Len := Length(FValue);
  if (Len > 0) and (St[1] = '''') then
    iStart := 2
  else
    iStart := 1;

  if (Len > 0) and (St[Length(St)] = '''') then
    iEnd := Length(St) - 1
  else
    iEnd := Length(St);

  Result := Copy(St, iStart, iEnd - iStart + 1);
end;

procedure TMacro.SetAsString(Value: _string);
begin
  Self.Value := '''' + Value + '''';
end;

procedure TMacro.SetActive(Value: boolean);
begin
  if Value <> FActive then begin
    FActive := Value;
    TMacros(Collection).NotifyOwner(Self);
  end;
end;

{ TMacros }

constructor TMacros.Create(Owner: TPersistent);
begin
  inherited Create(TMacro);

  FOwner := Owner;
  FParserClass := TSQLParser;
end;

procedure TMacros.Scan(SQL: _string);
var
  Macro: TMacro;
  NewMacros: TMacros;
  Parser: TSQLParser;
  CodeLexem: integer;
  St, St2: _string;
  MacroSt: _string; // Delphi problem with compare MacroChar = St
  Changed, NeedNext: boolean;
  i: integer;

begin
  // performance reason
{$IFDEF CLR}
  if SQL.IndexOf(Char(MacroChar)) = -1 then begin
{$ELSE}
  if Pos(_string(MacroChar), SQL) = 0 then begin
{$ENDIF}
    Clear;
    Exit;
  end;

  NewMacros := TMacros.Create(nil);
  NewMacros.BeginUpdate;

  Parser := FParserClass.Create(SQL);
  MacroSt := MacroChar;
  Parser.OmitBlank := False;
  Parser.Uppered := False;
  try
    Parser.ToBegin;
    repeat
      repeat
        CodeLexem := Parser.GetNext(St); //+++ char instead of string
      until (CodeLexem = lcEnd) or (St = MacroSt);
      repeat
        NeedNext := True;
        if (St = MacroSt) and Parser.IsMacroAllowed(CodeLexem) then begin
          CodeLexem := Parser.GetNext(St);
          if (CodeLexem = lcIdent) or
            Parser.IsNumericMacroNameAllowed and (CodeLexem = lcNumber) or
            (CodeLexem >= lxSQLFirst) // SQL reserved words are allowed
          then begin
            St2 := St;
            if CodeLexem = lcNumber then begin
              CodeLexem := Parser.GetNext(St);
              if (CodeLexem = lcIdent) or (CodeLexem >= lxSQLFirst) then
                St2 := St2 + St
              else
                NeedNext := False
            end;
            Macro := NewMacros.FindMacro(St2);
            if Macro = nil then begin
              Macro := TMacro(NewMacros.Add);
              if FindMacro(St2) <> nil then
                Macro.Assign(FindMacro(St2))
              else
                Macro.Name := St2;
            end;
          end;
        end;
      until NeedNext;
    until CodeLexem = lcEnd;

    if Count <> NewMacros.Count then
      Changed := True
    else
    begin
      Changed := False;
      for i := 0 to Count - 1 do
        if not Items[i].IsEqual(NewMacros.Items[i]) then begin
          Changed := True;
          Break;
        end;
    end;

    if Changed then
      Assign(NewMacros);
  finally
    Parser.Free;
    NewMacros.Free;
  end;
end;

function TMacros.GetMacroValue(Macro: TMacro): _string;
begin
  if Macro.Active then
    Result := Macro.Value
  else
    Result := '';
end;

procedure TMacros.Expand(var SQL: _string);
var
  Parser: TSQLParser;
  CodeLexem: integer;
  Macro: TMacro;
  St, St2: _string;
  MacroSt: _string; // Delphi problem with compare MacroChar = St
  Result: _string;
  NeedNext: boolean;
begin
  // performance reason
{$IFDEF CLR}
  if SQL.IndexOf(Char(MacroChar)) = -1 then begin
{$ELSE}
  if Pos(_string(MacroChar), SQL) = 0 then begin
{$ENDIF}
    Exit;
  end;

  Parser := FParserClass.Create(SQL);
  MacroSt := MacroChar;
  Parser.OmitBlank := False;
  Parser.Uppered := False;
  Parser.QuotedString := True;
  try
    Result := '';
    St := '';
    NeedNext := True;
    CodeLexem := 0; // to prevent warning
    Parser.ToBegin;
    while True do begin
      if NeedNext then
        CodeLexem := Parser.GetNext(St);

      if CodeLexem = lcEnd then
        Break;

      NeedNext := True;
      if (St = MacroSt) and Parser.IsMacroAllowed(CodeLexem) then begin
        CodeLexem := Parser.GetNext(St);
        if (CodeLexem = lcIdent) or
          Parser.IsNumericMacroNameAllowed and (CodeLexem = lcNumber) or
         (CodeLexem >= lxSQLFirst) // SQL reserved words is allowed
        then begin
          St2 := St;
          if CodeLexem = lcNumber then begin
            CodeLexem := Parser.GetNext(St);
            if (CodeLexem = lcIdent) or (CodeLexem >= lxSQLFirst) then
              St2 := St2 + St
            else
              NeedNext := False
          end;
          Macro := FindMacro(St2);
          if Macro <> nil then
            Result := Result + GetMacroValue(Macro);
        end
        else
          Result := Result + MacroSt + St;
      end
      else
        Result := Result + St;
    end;
  finally
    Parser.Free;
  end;

  SQL := Result;
end;

procedure TMacros.AssignTo(Dest: TPersistent);
begin
  if Dest is TMacros then
    TMacros(Dest).Assign(Self)
  else
    inherited AssignTo(Dest);
end;

procedure TMacros.AssignValues(Value: TMacros);
var
  i: integer;
  Macro: TMacro;
begin
  for i := 0 to Value.Count - 1 do begin
    Macro := FindMacro(Value[i].Name);
    if Macro <> nil then
      Macro.Assign(Value[i]);
  end;
end;

procedure TMacros.DefineProperties(Filer: TFiler);
begin
  Filer.DefineBinaryProperty('Data', ReadBinaryData, nil, False);
end;

function TMacros.IsEqual(Value: TMacros): boolean;
var
  i: integer;
begin
  Result := True;
  if Self = Value then
    Exit;
  if Count = Value.Count then begin
    for i := 0 to Count - 1 do
      if (Items[i].Name <> Value[i].Name) or
        (Items[i].Value <> Value[i].Value) or
        (Items[i].Active <> Value[i].Active)
      then
        Result := False;
  end
  else
    Result := False;
end;

{  Structure of Data
  Version        1 (100) -- !!! Add in 100
  ItemCount      1
    NameLength   1
    Name         Length(Name)
    ValueLength  2
    Value        Length(Value)
    Active       1       -- !!! Add in 100
}

procedure TMacros.ReadBinaryData(Stream: TStream);
const
  BufLen = 1000;
var
  i, Len: word;
  Version: byte;
  B: boolean;
  Buf: TBytes;
  St: string;
begin
  SetLength(Buf, BufLen + 1{??? - array [0..BufLen] of byte});

  with Stream do begin
    ReadBuffer(Version, 1);  // Version or Count

    if Version = 100 then begin
      Len := 0;
      ReadBuffer(Len, 1);
    end;

    for i := 0 to Count - 1 do begin
      Len := 0;
      ReadBuffer(Len, 1);
      if Len > BufLen then
        Len := BufLen;
      ReadBuffer(Buf{$IFNDEF CLR}[0]{$ENDIF}, Len);
      Buf[Len] := 0;
      St := string(Encoding.Default.GetString(Buf, 0, Len));
      with MacroByName(St) do begin
        ReadBuffer(Len, 2);
        if Len > BufLen then
          Len := BufLen;
        ReadBuffer(Buf{$IFNDEF CLR}[0]{$ENDIF}, Len);
        Buf[Len] := 0;
        St := string(Encoding.Default.GetString(Buf, 0, Len));
        Value := St;

        if Version = 100 then begin
          ReadBuffer(B, 1);  // Active
          Active := B;
        end;
      end;
    end;
  end;
end;

function TMacros.GetOwner: TPersistent;
begin
  Result := FOwner;
end;

procedure TMacros.Update(Item: TCollectionItem);
begin
  inherited;

  NotifyOwner(TMacro(Item));
end;

function TMacros.GetItem(Index: integer): TMacro;
begin
  Result := TMacro(inherited Items[Index]);
end;

procedure TMacros.SetItem(Index: integer; Value: TMacro);
begin
  inherited SetItem(Index, TCollectionItem(Value));
end;

procedure TMacros.NotifyOwner(Item: TMacro);
begin
  if (UpdateCount <> 0) or (FOwner = nil) then
    exit;

  if FOwner is TCustomDADataSet then
    with TCustomDADataSet(FOwner) do begin
      if not Active or (Item = nil) or (Pos(MacroChar + Item.Name, SQL.Text) <> 0) then
        FCommand.ProcessSQLChanged(True, True);
    end
  else begin
    TCustomDASQL(FOwner).ProcessSQLChanged(True, True);
  end;
end;

function TMacros.FindMacro(const Value: _string): TMacro;
var
  i: integer;
begin
  for i := 0 to Count - 1 do begin
    Result := TMacro(inherited Items[i]);
    if _SameText(Result.Name, Value) then
      Exit;
  end;
  Result := nil;
end;

function TMacros.MacroByName(const Value: _string): TMacro;
begin
  Result := FindMacro(Value);

  if Result = nil then
    DatabaseErrorFmt(SMacroNotFound, [Value], FOwner as TComponent);
end;

procedure TMacros.SetParserClass(Value: TSQLParserClass);
begin
  FParserClass := Value;
end;

{ TCRServerEnumerator }

constructor TCRServerEnumerator.Create;
begin
  inherited;
end;

procedure TCRServerEnumerator.GetServerList(List: _TStrings);
begin
  List.Clear;
end;

function TCRServerEnumerator.GetProp(Prop: integer; var Value: variant): boolean;
begin
  Assert(False, IntToStr(Prop));
  Result := False;
end;

function TCRServerEnumerator.SetProp(Prop: integer; const Value: variant): boolean;
begin
  Assert(False, IntToStr(Prop));
  Result := False;
end;

{ TCustomConnectDialog }

constructor TCustomConnectDialog.Create(Owner: TComponent);
begin
  inherited;

  FRetries := 3;
  LabelSet := lsEnglish;
  FStoreLogInfo := True;
{$IFDEF MSWINDOWS}
  FUseServerHistory := True;
{$ENDIF}
  FNeedConnect := True;
end;

destructor TCustomConnectDialog.Destroy;
begin
  FreeServerEnumerator;

  inherited;
end;

procedure TCustomConnectDialog.Notification(AComponent: TComponent;
  Operation: TOperation);
begin
  inherited;

  if (Operation = opRemove) and (FConnection = AComponent) then
    FConnection := nil;
end;

function TCustomConnectDialog.DefDialogClass: TClass;
begin
  Result := nil;
end;

procedure TCustomConnectDialog.GetServerList(List: _TStrings);
begin
{$IFDEF MSWINDOWS}
  if FUseServerHistory then
    LoadServerListFromRegistry(List)
  else
{$ENDIF}
  begin
    CheckServerEnumerator;
    SetServerEnumerator(FServerEnumerator); // set props
    FServerEnumerator.GetServerList(List);
  {$IFDEF MSWINDOWS}
    if List.Count = 0 then
      LoadServerListFromRegistry(List);
  {$ENDIF}
  end;
end;

{$IFDEF MSWINDOWS}
function TCustomConnectDialog.GetKeyPath: string;
begin
  Result := '';
end;

function TCustomConnectDialog.GetApplicationKeyPath: string;
begin
  Result := GetKeyPath + 'Connect\' + ApplicationTitle;
end;

function TCustomConnectDialog.GetServerListKeyPath: string;
begin
  Result := GetKeyPath + 'Connect';
end;

function TCustomConnectDialog.GetServerStoreName: string;
begin
  Result := 'Server';
end;

procedure TCustomConnectDialog.LoadServerListFromRegistry(List: _TStrings);
var
  ListKey, ServerKey: string;
  Registry: TRegistry;
  i: integer;
  KeyOpened: boolean;
begin
  Registry := TRegistry.Create(KEY_READ OR KEY_WRITE);
  try
    ListKey := GetServerListKeyPath;
    KeyOpened := Registry.OpenKey(ListKey, False);
    if not KeyOpened then begin
      ListKey := StringReplace(ListKey, 'Devart', 'CoreLab', [rfIgnoreCase]);
      KeyOpened := Registry.OpenKey(ListKey, False);
    end;
    if KeyOpened then begin
      List.Clear;
      ServerKey := GetServerStoreName;
      i := 1;
      while Registry.ValueExists(Format('%s %d', [ServerKey, i])) do begin
        List.Add(Registry.ReadString(Format('%s %d', [ServerKey, i])));
        Inc(i);
      end;
      Registry.CloseKey;
    end;
  finally
    Registry.Free;
  end;
end;

procedure TCustomConnectDialog.SaveServerListToRegistry;
var
  Registry: TRegistry;
  List: _TStrings;
  i,j: integer;
  ServerKey: string;
begin
  if Connection.Server = '' then
    exit; // nothing to save

  Registry := TRegistry.Create(KEY_READ OR KEY_WRITE);
  try
    List := _TStringList.Create;
    try
      LoadServerListFromRegistry(List); // call before creating Devart key
      if Registry.OpenKey(GetServerListKeyPath, True) then begin
        ServerKey := GetServerStoreName;
        Registry.WriteString(Format('%s %d', [ServerKey, 1]), Connection.Server);
        i := 2;
        for j := 0 to List.Count - 1 do
          if not _SameText(List[j], Connection.Server) then begin
            Registry.WriteString(Format('%s %d', [ServerKey, i]), List[j]);
            Inc(i);
          end;
        Registry.CloseKey;
      end;
    finally
      List.Free;
    end;
  finally
    Registry.Free;
  end;
end;

procedure TCustomConnectDialog.SaveInfoToRegistry(Registry: TRegistry);
begin
  Registry.WriteString('Username', Connection.Username);
  Registry.WriteString(GetServerStoreName, Connection.Server);
end;

procedure TCustomConnectDialog.LoadInfoFromRegistry(Registry: TRegistry);
var
  ServerKey: string;
begin
  ServerKey := GetServerStoreName;
  if Registry.ValueExists('Username') then
    Connection.FUsername := Registry.ReadString('Username');
  if Registry.ValueExists(ServerKey) then
    Connection.FServer := Registry.ReadString(ServerKey);
end;
{$ENDIF}

{class function TCustomConnectDialog.AcceptBlankPassword: boolean;
begin
  Result := False;
end;}

function TCustomConnectDialog.Execute: boolean;
var
  OldUsername, OldPassword, OldServer: _string;
  IDE: boolean;
{$IFDEF MSWINDOWS}
  Key, CrKey: string;
  KeyOpened: boolean;
  Registry: TRegistry;
{$ENDIF}
begin
  Result := False;

  if Connection = nil then
    DatabaseError(SConnectionNotDefined);

  OldUsername := Connection.Username;
  OldPassword := Connection.Password;
  OldServer := Connection.Server;

{$IFDEF MSWINDOWS}
  Key := GetApplicationKeyPath;
  Registry := nil;
{$ENDIF}
  try
    IDE := (Pos('Delphi', ApplicationTitle) = 1) or
      (Pos('C++Builder', ApplicationTitle) = 1) or
      (Pos('RAD Studio', ApplicationTitle) > 0);
    if FStoreLogInfo then begin
    {$IFDEF MSWINDOWS}
      Registry := TRegistry.Create(KEY_READ OR KEY_WRITE);

      KeyOpened := Registry.OpenKey(Key, False);
      if not KeyOpened then begin
        CrKey := StringReplace(Key, 'Devart', 'CoreLab', [rfIgnoreCase]);
        KeyOpened := Registry.OpenKey(CrKey, False);
      end;

      if KeyOpened and (not IDE or (Connection.Username = '')) then
        LoadInfoFromRegistry(Registry);

      if not SavePassword and not IDE or
        not _SameText(Connection.Username, OldUsername)
      then
         Connection.FPassword := '';

      if KeyOpened then
        Registry.CloseKey;
    {$ENDIF}
    end;

    if IDE and (LowerCase(Copy(ClassName, Length(ClassName) - 2, 3)) = 'fmx') and Assigned(ShowConnectFormProcFmx) then
      Result := ShowConnectFormProcFmx(Self)
    else
    if Assigned(ShowConnectFormProc) then
      Result := ShowConnectFormProc(Self)
    else
      Result := False;

    if Result then begin
      if FStoreLogInfo then begin
      {$IFDEF MSWINDOWS}
        SaveServerListToRegistry;

        // StoreLogInfo can be changed by user since previous check
        if Registry = nil then
          Registry := TRegistry.Create(KEY_READ OR KEY_WRITE);

        if Registry.OpenKey(Key, True) then begin
          SaveInfoToRegistry(Registry);
          Registry.CloseKey;
        end;
      {$ENDIF}
      end;
    end;
  finally
    if not Result then begin
      Connection.FUsername := OldUsername;
      Connection.FPassword := OldPassword;
      Connection.FServer := OldServer;
    end;
  {$IFDEF MSWINDOWS}
    Registry.Free;
  {$ENDIF}
  end;
end;

function TCustomConnectDialog.GetServerEnumeratorClass: TCRServerEnumeratorClass;
begin
  Assert(False, 'Must be overrided');
  Result := TCRServerEnumerator;
end;

procedure TCustomConnectDialog.SetServerEnumerator(Value: TCRServerEnumerator);
begin
  if FServerEnumerator <> Value then begin
    if FServerEnumerator <> nil then
      FreeServerEnumerator;

    FServerEnumerator := Value;
  end;
end;

procedure TCustomConnectDialog.CreateServerEnumerator;
begin
  SetServerEnumerator(GetServerEnumeratorClass.Create);
end;

procedure TCustomConnectDialog.FreeServerEnumerator;
begin
  FServerEnumerator.Free;
  FServerEnumerator := nil;
end;

procedure TCustomConnectDialog.CheckServerEnumerator;
begin
  if not (FServerEnumerator is GetServerEnumeratorClass) then begin
    FreeServerEnumerator;
    CreateServerEnumerator;
  end;
end;

{$IFDEF WIN32_64}
function TCustomConnectDialog.GetString(Id: integer): string;
const
  BufLen = 50;
var
  Buf: array [0..BufLen] of char;
  Base: integer;
begin
  case FLabelSet of
    lsEnglish:
      Base := 100;
    lsFrench:
      Base := 200;
    lsGerman:
      Base := 300;
    lsItalian:
      Base := 400;
    lsPolish:
      Base := 500;
    lsPortuguese:
      Base := 600;
    lsRussian:
      Base := 0;
    lsSpanish:
      Base := 700;
  else
      Base := 100;    
  end;

  Buf[0] := #0;
  LoadString(hInstance, Id + Base, @Buf, BufLen);
  Result := Buf;
end;
{$ENDIF}

procedure TCustomConnectDialog.SetLabelSet(Value: TLabelSet);
{$IFDEF WIN32_64}
begin
  FLabelSet := Value;
  if FLabelSet <> lsCustom then begin

    FCaption := GetString(0);
    FUsernameLabel := GetString(1);
    FPasswordLabel := GetString(2);
    FServerLabel := GetString(3);
    FConnectButton := GetString(4);
    FCancelButton := GetString(5);
  end;
{$ELSE}
begin
  FCaption := 'Connect';
  FUsernameLabel := 'Username';
  FPasswordLabel := 'Password';
  FServerLabel := 'Server';
  FConnectButton := 'Connect';
  FCancelButton := 'Cancel';
{$ENDIF}
end;

procedure TCustomConnectDialog.SetCaption(Value: string);
begin
  if not(csLoading in ComponentState) then
    FLabelSet := lsCustom;

  FCaption := Value;
end;

procedure TCustomConnectDialog.SetUsernameLabel(Value: string);
begin
  if not(csLoading in ComponentState) then
    FLabelSet := lsCustom;

  FUsernameLabel := Value;
end;

procedure TCustomConnectDialog.SetPasswordLabel(Value: string);
begin
  if not(csLoading in ComponentState) then
    FLabelSet := lsCustom;

  FPasswordLabel := Value;
end;

procedure TCustomConnectDialog.SetServerLabel(Value: string);
begin
  if not(csLoading in ComponentState) then
    FLabelSet := lsCustom;

  FServerLabel := Value;
end;

procedure TCustomConnectDialog.SetConnectButton(Value: string);
begin
  if not(csLoading in ComponentState) then
    FLabelSet := lsCustom;

  FConnectButton := Value;
end;

procedure TCustomConnectDialog.SetCancelButton(Value: string);
begin
  if not(csLoading in ComponentState) then
    FLabelSet := lsCustom;

  FCancelButton := Value;
end;

{$IFDEF MSWINDOWS}
const
  advapi32 = 'advapi32.dll';
  netapi32 = 'netapi32.dll'; 

  // Service State -- for CurrentState
  SERVICE_STOPPED                = $00000001;
  SERVICE_START_PENDING          = $00000002;
  SERVICE_STOP_PENDING           = $00000003;
  SERVICE_RUNNING                = $00000004;
  SERVICE_CONTINUE_PENDING       = $00000005;
  SERVICE_PAUSE_PENDING          = $00000006;
  SERVICE_PAUSED                 = $00000007;

  // Service object specific access type
  SERVICE_QUERY_CONFIG           = $0001;
  SERVICE_CHANGE_CONFIG          = $0002;
  SERVICE_QUERY_STATUS           = $0004;
  SERVICE_ENUMERATE_DEPENDENTS   = $0008;
  SERVICE_START                  = $0010;
  SERVICE_STOP                   = $0020;
  SERVICE_PAUSE_CONTINUE         = $0040;
  SERVICE_INTERROGATE            = $0080;
  SERVICE_USER_DEFINED_CONTROL   = $0100;
  SERVICE_ALL_ACCESS             = (STANDARD_RIGHTS_REQUIRED or
                                    SERVICE_QUERY_CONFIG or
                                    SERVICE_CHANGE_CONFIG or
                                    SERVICE_QUERY_STATUS or
                                    SERVICE_ENUMERATE_DEPENDENTS or
                                    SERVICE_START or
                                    SERVICE_STOP or
                                    SERVICE_PAUSE_CONTINUE or
                                    SERVICE_INTERROGATE or
                                    SERVICE_USER_DEFINED_CONTROL);
  
  // Service Control Manager object specific access types
  SC_MANAGER_CONNECT             = $0001;
  SC_MANAGER_CREATE_SERVICE      = $0002;
  SC_MANAGER_ENUMERATE_SERVICE   = $0004;
  SC_MANAGER_LOCK                = $0008;
  SC_MANAGER_QUERY_LOCK_STATUS   = $0010;
  SC_MANAGER_MODIFY_BOOT_CONFIG  = $0020;
  SC_MANAGER_ALL_ACCESS          = (STANDARD_RIGHTS_REQUIRED or SC_MANAGER_CONNECT or
    SC_MANAGER_CREATE_SERVICE or SC_MANAGER_ENUMERATE_SERVICE or SC_MANAGER_LOCK or
    SC_MANAGER_QUERY_LOCK_STATUS or SC_MANAGER_MODIFY_BOOT_CONFIG);

  // Service Types    
  SERVICE_WIN32_OWN_PROCESS     = $00000010;
  SERVICE_WIN32_SHARE_PROCESS   = $00000020;
  SERVICE_WIN32                 = (SERVICE_WIN32_OWN_PROCESS or
                                   SERVICE_WIN32_SHARE_PROCESS);

  // Service State -- for Enum Requests (Bit Mask)
  SERVICE_ACTIVE                 = $00000001;
  SERVICE_INACTIVE               = $00000002;
  SERVICE_STATE_ALL              = (SERVICE_ACTIVE   or
                                    SERVICE_INACTIVE);

  // Controls
  SERVICE_CONTROL_STOP           = $00000001;
                                   
type
  // Service Status Enumeration Structure
{$IFDEF CLR}
  [StructLayout(LayoutKind.Sequential)]
{$ENDIF}  
  _SERVICE_STATUS = record
    dwServiceType: DWORD;
    dwCurrentState: DWORD;
    dwControlsAccepted: DWORD;
    dwWin32ExitCode: DWORD;
    dwServiceSpecificExitCode: DWORD;
    dwCheckPoint: DWORD;
    dwWaitHint: DWORD;
  end;
  TServiceStatus = _SERVICE_STATUS;

{$IFDEF CLR}  
  [StructLayout(LayoutKind.Sequential, CharSet = CharSet.Auto)]
  _ENUM_SERVICE_STATUS = record
    [MarshalAs(UnmanagedType.LPTStr)]
    lpServiceName: string;
    [MarshalAs(UnmanagedType.LPTStr)]
    lpDisplayName: string;
    ServiceStatus: TServiceStatus;
  end;
  TEnumServiceStatus = _ENUM_SERVICE_STATUS;
{$ELSE}
  _ENUM_SERVICE_STATUSA = record
    lpServiceName: PAnsiChar;
    lpDisplayName: PAnsiChar;
    ServiceStatus: TServiceStatus;
  end;
  TEnumServiceStatus = _ENUM_SERVICE_STATUSA;
{$ENDIF}

  TOpenSCManager = function (lpMachineName: {$IFDEF CLR}string{$ELSE}PAnsiChar{$ENDIF}; lpDatabaseName: {$IFDEF CLR}IntPtr{$ELSE}PAnsiChar{$ENDIF}; dwDesiredAccess: DWORD): SC_HANDLE;{$IFNDEF CLR} stdcall;{$ENDIF}
  TCloseServiceHandle = function (hSCObject: SC_HANDLE): BOOL;{$IFNDEF CLR} stdcall;{$ENDIF}
  TOpenService = function (hSCManager: SC_HANDLE; lpServiceName: PAnsiChar; dwDesiredAccess: DWORD): SC_HANDLE;{$IFNDEF CLR} stdcall;{$ENDIF}
  TEnumServicesStatus = function (hSCManager: SC_HANDLE; dwServiceType, dwServiceState: DWORD;{$IFDEF CLR}lpServices: IntPtr{$ELSE}var lpServices: TEnumServiceStatus{$ENDIF};
    cbBufSize: DWORD; {$IFDEF CLR}out{$ELSE}var{$ENDIF} pcbBytesNeeded, lpServicesReturned: DWORD; var lpResumeHandle: DWORD): BOOL;{$IFNDEF CLR} stdcall;{$ENDIF}
  TQueryServiceStatus = function (hService: SC_HANDLE; {$IFDEF CLR}out{$ELSE}var{$ENDIF} lpServiceStatus: TServiceStatus): BOOL;{$IFNDEF CLR} stdcall;{$ENDIF}
  TStartService = function (hService: SC_HANDLE; dwNumServiceArgs: DWORD; {$IFNDEF CLR}var{$ENDIF} lpServiceArgVectors: {$IFDEF CLR}IntPtr{$ELSE}PAnsiChar{$ENDIF}): BOOL;{$IFNDEF CLR} stdcall;{$ENDIF}
  TControlService = function (hService: SC_HANDLE; dwControl: DWORD; {$IFDEF CLR}out{$ELSE}var{$ENDIF} lpServiceStatus: TServiceStatus): BOOL;{$IFNDEF CLR} stdcall;{$ENDIF}
  TNetServerEnum = function (ServerName: IntPtr; Level: longint; var BufPtr: IntPtr; PrefMaxLen: longint;
    var EntriesRead, TotalEntries: longint; ServType: longint; Domain: {$IFDEF CLR}IntPtr{$ELSE}PWideChar{$ENDIF}; var ResumeHandle: integer): longint;{$IFNDEF CLR} stdcall;{$ENDIF}
  TNetApiBufferFree = function (BufPtr: IntPtr): longint;{$IFNDEF CLR} stdcall;{$ENDIF}
  
{$IFDEF CLR}
[SuppressUnmanagedCodeSecurity, DllImport(advapi32, CharSet = CharSet.Auto, SetLastError = True, EntryPoint = 'OpenSCManager')]
function FnOpenSCManager(lpMachineName: string; lpDatabaseName: IntPtr; dwDesiredAccess: DWORD): SC_HANDLE; external;

[SuppressUnmanagedCodeSecurity, DllImport(advapi32, CharSet = CharSet.Ansi, SetLastError = True, EntryPoint = 'CloseServiceHandle')]
function FnCloseServiceHandle(hSCObject: SC_HANDLE): BOOL; external;

[SuppressUnmanagedCodeSecurity, DllImport(advapi32, CharSet = CharSet.Auto, SetLastError = True, EntryPoint = 'OpenService')]
function FnOpenService(hSCManager: SC_HANDLE; lpServiceName: string; dwDesiredAccess: DWORD): SC_HANDLE; external;

[SuppressUnmanagedCodeSecurity, DllImport(advapi32, CharSet = CharSet.Auto, SetLastError = True, EntryPoint = 'EnumServicesStatus')]
function FnEnumServicesStatus(hSCManager: SC_HANDLE; dwServiceType,
  dwServiceState: DWORD; lpServices: IntPtr; cbBufSize: DWORD;
  out pcbBytesNeeded, lpServicesReturned: DWORD; var lpResumeHandle: DWORD): BOOL; external;

[SuppressUnmanagedCodeSecurity, DllImport(advapi32, CharSet = CharSet.Ansi, SetLastError = True, EntryPoint = 'QueryServiceStatus')]  
function FnQueryServiceStatus(hService: SC_HANDLE; out lpServiceStatus: TServiceStatus): BOOL; external;

[SuppressUnmanagedCodeSecurity, DllImport(advapi32, CharSet = CharSet.Auto, SetLastError = True, EntryPoint = 'StartService')]
function FnStartService(hService: SC_HANDLE; dwNumServiceArgs: DWORD; lpServiceArgVectors: IntPtr): BOOL; external;

[SuppressUnmanagedCodeSecurity, DllImport(advapi32, CharSet = CharSet.Ansi, SetLastError = True, EntryPoint = 'StartServiceA')]
function StartServiceA(hService: SC_HANDLE; dwNumServiceArgs: DWORD; lpServiceArgVectors: IntPtr): BOOL; external;

[SuppressUnmanagedCodeSecurity, DllImport(advapi32, CharSet = CharSet.Ansi, SetLastError = True, EntryPoint = 'ControlService')]
function FnControlService(hService: SC_HANDLE; dwControl: DWORD; out lpServiceStatus: TServiceStatus): BOOL; external;

[SuppressUnmanagedCodeSecurity, DllImport(netapi32, CharSet = CharSet.Auto, SetLastError = True, EntryPoint = 'NetServerEnum')]
function FnNetServerEnum(ServerName: IntPtr;  Level: longint; var BufPtr: IntPtr; PrefMaxLen: longint;
  var EntriesRead, TotalEntries: longint; ServType: longint; Domain: IntPtr; var ResumeHandle: integer): longint; external;

[SuppressUnmanagedCodeSecurity, DllImport(netapi32, CharSet = CharSet.Auto, SetLastError = True, EntryPoint = 'NetApiBufferFree')]
function FnNetApiBufferFree(BufPtr: IntPtr): longint; external;
{$ENDIF}  

var
  hAdvapi32Lib: HMODULE;
  hNetapi32Lib: HMODULE;
  OpenSCManager: TOpenSCManager;
  CloseServiceHandle: TCloseServiceHandle;
  OpenService: TOpenService;
  EnumServicesStatus: TEnumServicesStatus;
  QueryServiceStatus: TQueryServiceStatus;
  StartService: TStartService;
  ControlService: TControlService;
  NetServerEnum: TNetServerEnum;
  NetApiBufferFree: TNetApiBufferFree;

function NotLink: integer;
begin
  raise Exception.Create('function is not linked');
  Result := 0;
end;

procedure LoadNetManagerLib;
{$IFNDEF CLR}
  function GetProc(hLib: HMODULE; Name: string): FARPROC;
  begin
    Result := GetProcAddress(hLib, PChar(Name));
    if Result = nil then
      Result := @NotLink;
  end;
{$ENDIF} 
begin
  hAdvapi32Lib := LoadLibrary(PChar(advapi32));

  if hAdvapi32Lib > 0 then begin
  {$IFDEF CLR}
    OpenSCManager := FnOpenSCManager;
    CloseServiceHandle := FnCloseServiceHandle;
    OpenService := FnOpenService;
    EnumServicesStatus := FnEnumServicesStatus;
    QueryServiceStatus := FnQueryServiceStatus;
    StartService := FnStartService;
    ControlService := FnControlService;
  {$ELSE}
    OpenSCManager := GetProc(hAdvapi32Lib, 'OpenSCManagerA');
    CloseServiceHandle := GetProc(hAdvapi32Lib, 'CloseServiceHandle');
    OpenService := GetProc(hAdvapi32Lib, 'OpenServiceA');
    EnumServicesStatus := GetProc(hAdvapi32Lib, 'EnumServicesStatusA');
    QueryServiceStatus := GetProc(hAdvapi32Lib, 'QueryServiceStatus');
    StartService := GetProc(hAdvapi32Lib, 'StartServiceA');
    ControlService := GetProc(hAdvapi32Lib, 'ControlService');
  {$ENDIF}
  end;

  hNetapi32Lib := LoadLibrary(PChar(netapi32));
  
  if hNetapi32Lib > 0 then begin
  {$IFDEF CLR}
    if GetProcAddress(hNetapi32Lib, 'NetServerEnum') <> nil then
      NetServerEnum := FnNetServerEnum;
    if GetProcAddress(hNetapi32Lib, 'NetApiBufferFree') <> nil then
      NetApiBufferFree := FnNetApiBufferFree;
  {$ELSE}
    NetServerEnum := GetProc(hNetapi32Lib, 'NetServerEnum');
    NetApiBufferFree := GetProc(hNetapi32Lib, 'NetApiBufferFree');
  {$ENDIF}
  end;
end;

procedure FreeNetManagerLib;
begin
  if hAdvapi32Lib > 0 then begin
    FreeLibrary(hAdvapi32Lib);
    hAdvapi32Lib := 0;
  end;

  if hNetapi32Lib > 0 then begin
    FreeLibrary(hNetapi32Lib);
    hNetapi32Lib := 0;
  end;
end;

{ TCRServiceNamesThread }

constructor TCRServiceNamesThread.Create(const Server: string; Services: TCRServicesThread; const Keywords: string);
begin
  inherited Create(True);
  FServer := Server;
  FKeywords := Keywords;
  FServices := Services;
  Priority := tpHighest;

  Resume;
end;

procedure TCRServiceNamesThread.Execute;
{var
  tc: cardinal;}
var
  j, k: integer;
  sl: TStringList;
  b: boolean;
begin
  // tc := GetTickCount;
  try
    FServiceNames := TCRNetManager.GetServiceNames(FServer);

    sl := TStringList.Create;
    try
      sl.Text := FKeywords;

      b := False;
      for j := 0 to Length(FServiceNames) - 1 do begin
        for k := 0 to sl.Count - 1 do
          if (Pos(sl[k], LowerCase(FServiceNames[j].ServiceName)) > 0) or
             (Pos(sl[k], LowerCase(FServiceNames[j].DisplayName)) > 0) then begin
            b := True;
            CRNetManager.AddToCachedServerList(FKeywords, FServer);
            Break;
          end;
        if b then
          Break;
      end;
    finally
      sl.Free;
    end;
  except
    // Silent
  end;
  {tc := GetTickInterval(tc, GetTickCount);
  OFS(FServer + ' ' + IntToStr(tc) + ' ' + IntToStr(Length(FServices.FServiceNames[FIndex])));}
end;

constructor TCRServicesThread.Create(List: TStrings; const Keywords: string);
begin
  inherited Create(True);

  FList := TStringList.Create;
  FList.Assign(List);
  FKeywords := Keywords;
  FreeOnTerminate := True;

  Resume;
end;

destructor TCRServicesThread.Destroy;
begin
  FList.Free;

  inherited;
end;

procedure TCRServicesThread.Execute;
var
  i: integer;
  Threads: array of TCRServiceNamesThread;
begin
  SetLength(Threads, FList.Count);
  for i := 0 to FList.Count - 1 do
    Threads[i] := nil;
  try
    for i := 0 to FList.Count - 1 do
      Threads[i] := TCRServiceNamesThread.Create(FList[i], Self, FKeywords);

    for i := 0 to FList.Count - 1 do
      Threads[i].WaitFor;

  finally
    for i := 0 to FList.Count - 1 do 
      Threads[i].Free;
  end;
end;

{ TCRNetManager }

function ServiceStatusToCurrentStatus(const CurrentState: DWORD): TCRServiceStatus;
begin
  case CurrentState of
    SERVICE_STOPPED:
      Result := ssStopped;
    SERVICE_START_PENDING:
      Result := ssStartPending;
    SERVICE_STOP_PENDING:
          Result := ssStopPending;
    SERVICE_RUNNING:
      Result := ssRunning;
    SERVICE_CONTINUE_PENDING:
      Result := ssContinuePending;
    SERVICE_PAUSE_PENDING:
      Result := ssPausePending;
    SERVICE_PAUSED:
      Result := ssPaused;
    else
    begin
      DatabaseErrorFmt('Unknown service status $%X (%d)', [CurrentState, CurrentState]);
      Result := ssStopped;
    end;
  end;
end;

constructor TCRNetManager.Create;
begin
  inherited;
  
  FServicesCS := TCriticalSection.Create;
end;

destructor TCRNetManager.Destroy;
begin
  ClearCachedServerList;
  FServicesCS.Free;
  
  inherited;
end;

class procedure TCRNetManager.ServiceManagerOpen(const Server: string; const ReadOnly: boolean; out sch: SC_HANDLE);
var
  s: AnsiString;
  dwDesiredAccess: DWORD;
begin
  sch := 0;
  if Trim(LowerCase(Server)) = 'localhost' then
    s := ''
  else
    s := AnsiString(Server);
  if ReadOnly then
    dwDesiredAccess := GENERIC_READ
  else
    dwDesiredAccess := SC_MANAGER_ALL_ACCESS;
  sch := OpenSCManager({$IFDEF CLR}s{$ELSE}PAnsiChar(s){$ENDIF}, nil, dwDesiredAccess);
end;

class procedure TCRNetManager.ServiceManagerClose(const sch: SC_HANDLE);
begin
  if sch <> 0 then
    CloseServiceHandle(sch);
end;

class procedure TCRNetManager.ServiceOpen(const Server: string; const ServiceName: string; const ReadOnly: boolean; out sch: SC_HANDLE; out sh: SC_HANDLE);
begin
  ServiceManagerOpen(Server, ReadOnly, sch);
  Win32Check(sch <> 0);
  try
    sh := OpenService(sch, {$IFDEF CLR}ServiceName{$ELSE}PAnsiChar(AnsiString(ServiceName)){$ENDIF}, SERVICE_ALL_ACCESS);
    Win32Check(sh <> 0);
  except
    ServiceManagerClose(sch);
    raise;
  end;
end;

class procedure TCRNetManager.ServiceClose(const sch: SC_HANDLE; const sh: SC_HANDLE);
begin
  if sh <> 0 then
    CloseServiceHandle(sh);
  ServiceManagerClose(sch);
end;

class function TCRNetManager.GetServiceNames(const Server: string): TCRServicesInfo;
var
  sch: SC_HANDLE;
{$IFDEF CLR}
  pService: TEnumServiceStatus;
{$ELSE}
  pService: ^TEnumServiceStatus;
{$ENDIF}
  pServices: IntPtr;
  pcbBytesNeeded, lpServicesReturned, lpResumeHandle: DWORD;
  i: integer;
  SizeOfTEnumServiceStatus: Integer;

begin
  SetLength(Result, 0);

  ServiceManagerOpen(Server, True, sch);
  if sch = 0 then
    exit;

  pServices := nil;
  try
    //lpServices := nil;
    lpResumeHandle := 0;
    pcbBytesNeeded := 0;
    lpServicesReturned := 0;
    EnumServicesStatus(sch, SERVICE_WIN32, SERVICE_STATE_ALL, {$IFDEF CLR}nil{$ELSE}TEnumServiceStatus(pServices^){$ENDIF}, 0, pcbBytesNeeded, lpServicesReturned, lpResumeHandle);
    SizeOfTEnumServiceStatus := SizeOf(TEnumServiceStatus);
    lpServicesReturned := 0;
    pServices := Marshal.AllocHGlobal(pcbBytesNeeded);
    Win32Check(EnumServicesStatus(sch, SERVICE_WIN32, SERVICE_STATE_ALL, {$IFDEF CLR}pServices{$ELSE}TEnumServiceStatus(pServices^){$ENDIF}, pcbBytesNeeded, pcbBytesNeeded, lpServicesReturned, lpResumeHandle));
//    Win32Check(EnumServicesStatus(sch, SERVICE_WIN32, SERVICE_STATE_ALL, Services{$IFNDEF CLR}[0]{$ENDIF}, cbBufSize, pcbBytesNeeded, lpServicesReturned, lpResumeHandle));
    SetLength(Result, lpServicesReturned);

    for i := 0 to lpServicesReturned - 1 do begin
    {$IFDEF CLR}
      pService := TEnumServiceStatus(Marshal.PtrToStructure(PtrOffset(pServices, SizeOfTEnumServiceStatus * i), TypeOf(TEnumServiceStatus)));
    {$ELSE}
      pService := PtrOffset(pServices, SizeOfTEnumServiceStatus * i);
    {$ENDIF}

      Result[i].ServiceName := string(AnsiString(pService.lpServiceName));
      Result[i].DisplayName := string(AnsiString(pService.lpDisplayName));
      Result[i].Status := ServiceStatusToCurrentStatus(pService.ServiceStatus.dwCurrentState);
    end;
  finally
    ServiceManagerClose(sch);
    Marshal.FreeHGlobal(pServices);
  end;
end;

class function TCRNetManager.GetServiceStatus(const Server: string; const ServiceName: string): TCRServiceStatus;
var
  sch: SC_HANDLE;
  sh: SC_HANDLE;
  ss: TServiceStatus;
begin
  ServiceOpen(Server, ServiceName, True, sch, sh);
  try
    Win32Check(QueryServiceStatus(sh, ss));
    Result := ServiceStatusToCurrentStatus(ss.dwCurrentState);
  finally
    ServiceClose(sch, sh);
  end;
end;

class procedure TCRNetManager.ServiceStart(const Server: string; const ServiceName: string; ParamStr: string = '');
  // based on Delphi7 system.pas GetParamStr function
  function GetParamStr(Idx: integer; var Param: string): integer;
  var
    Len: Integer;
    StartIdx, SIdx, QIdx: Integer;
  begin
    while True do
    begin
      while (ParamStr[Idx] <> #0) and (ParamStr[Idx] <= ' ') do
        Inc(Idx);
      if (ParamStr[Idx] = '"') and (ParamStr[Idx + 1] = '"') then Inc(Idx, 2) else Break;
    end;
    Len := 0;
    StartIdx := Idx;
    while ParamStr[Idx] > ' ' do
    begin
      if ParamStr[Idx] = '"' then
      begin
        Inc(Idx);
        while (ParamStr[Idx] <> #0) and (ParamStr[Idx] <> '"') do
        begin
          QIdx := Idx + 1;
          Inc(Len, QIdx - Idx);
          Idx := QIdx;
        end;
        if ParamStr[Idx] <> #0 then
          Inc(Idx);
      end
      else
      begin
        QIdx := Idx + 1;
        Inc(Len, QIdx - Idx);
        Idx := QIdx;
      end;
    end;

    SetLength(Param, Len);

    Idx := StartIdx;
    SIdx := 1;
    while ParamStr[Idx] > ' ' do
    begin
      if ParamStr[Idx] = '"' then
      begin
        Inc(Idx);
        while (ParamStr[Idx] <> #0) and (ParamStr[Idx] <> '"') do
        begin
          QIdx := Idx + 1;
          while Idx < QIdx do
          begin
            Param[SIdx] := ParamStr[Idx];
            Inc(Idx);
            Inc(SIdx);
          end;
        end;
        if ParamStr[Idx] <> #0 then
          Inc(Idx);
      end
      else
      begin
        QIdx := Idx + 1;
        while Idx < QIdx do
        begin
          Param[SIdx] := ParamStr[Idx];
          Inc(Idx);
          Inc(SIdx);
        end;
      end;
    end;

    Result := Idx;
  end;
var
  sch: SC_HANDLE;
  sh: SC_HANDLE;
  i, Idx: integer;
  Param: string;
  Args: array of string;
  pArgs: array of {$IFDEF CLR}IntPtr{$ELSE}PAnsiChar{$ENDIF};
  p: {$IFDEF CLR}IntPtr{$ELSE}PAnsiChar{$ENDIF};
{$IFDEF CLR}
  b: TBytes;
  p1: IntPtr;
  Len: integer;
{$ENDIF}

begin
  ServiceOpen(Server, ServiceName, False, sch, sh);
  try
    ParamStr := Trim(ParamStr);
    if ParamStr <> '' then begin
      Idx := 1;
      SetLength(Args, 0);

      ParamStr := ParamStr + #0;
      while True do
      begin
        Idx := GetParamStr(Idx, Param);
        if Param = '' then Break;

        i := Length(Args);
        SetLength(Args, i + 1);
        Args[i] := Param;
      end;

      SetLength(pArgs, Length(Args));
    {$IFDEF CLR}
      for i := 0 to Length(Args) - 1 do
        pArgs[i] := nil;
      p := Marshal.AllocHGlobal(Length(Args) * SizeOf(IntPtr));
      try
        for i := 0 to Length(Args) - 1 do begin
          Len := Length(Args[i]);
          SetLength(b, Len + 1);
          Encoding.Default.GetBytes(Args[i], 0, Len, b, 0);
          b[Len] := 0;
          p1 := Marshal.AllocHGlobal(Len + 1);
          Marshal.Copy(b, 0, p1, Len + 1);
          pArgs[i] := p1;
          Marshal.WriteIntPtr(p, i * SizeOf(IntPtr), p1);
        end;

        i := Length(Args);
        Win32Check(StartServiceA(sh, i, p));
      finally
        for i := 0 to Length(Args) - 1 do
          Marshal.FreeHGlobal(pArgs[i]);
        Marshal.FreeHGlobal(p);
      end;
    {$ELSE}
      for i := 0 to Length(Args) - 1 do
        pArgs[i] := @Args[i][1];

      i := Length(Args);
      Win32Check(StartService(sh, i, pArgs[0]));
    {$ENDIF}
    end
    else
    begin
      p := nil;
      Win32Check(StartService(sh, 0, p));
    end;
  finally
    ServiceClose(sch, sh);
  end;
end;

class procedure TCRNetManager.ServiceStop(const Server: string; const ServiceName: string);
var
  sch: SC_HANDLE;
  sh: SC_HANDLE;
  ss: TServiceStatus;
begin
  ServiceOpen(Server, ServiceName, False, sch, sh);
  try
    Win32Check(ControlService(sh, SERVICE_CONTROL_STOP, ss));
  finally
    ServiceClose(sch, sh);
  end;
end;

procedure TCRNetManager.ClearCachedServerList;
var
  i: integer;
begin
  if FCachedServerList = nil then
    Exit;

  try
    for i := 0 to FCachedServerList.Count - 1 do
      FCachedServerList.Objects[i].Free;
  finally
    FreeAndNil(FCachedServerList);
  end;
end;

procedure TCRNetManager.AddToCachedServerList(const Keywords: string; const Server: string);
var
//  s: string;
  i: integer;
  sl: TStringList;
begin
{  s := '';
  for i := Low(Keywords) to High(Keywords) do begin
    if s <> '' then
      s := s + #$D#$A;
    s := s + Keywords[i];
  end;}

  FServicesCS.Acquire;
  try
    if FCachedServerList = nil then begin
      FCachedServerList := TStringList.Create;
    {$IFDEF VER6P}
      FCachedServerList.CaseSensitive := False;
    {$ENDIF}
      FCachedServerList.Sorted := True;
    end;
    i := FCachedServerList.IndexOf(Keywords);
    if i = - 1 then begin
      sl := TStringList.Create;
    {$IFDEF VER6P}
      sl.CaseSensitive := False;
    {$ENDIF}
      sl.Sorted := True;
      FCachedServerList.AddObject(Keywords, sl);
    end
    else
      sl := FCachedServerList.Objects[i] as TStringList;
    if sl.IndexOf(Server) = -1 then
      sl.Add(Server);
  finally
    FServicesCS.Release;
  end;
end;

class procedure TCRNetManager.GetServerList(List: TStrings);
var
  pData, psvr_Name: IntPtr;
  EntRead, EntTotal, Resume, i : integer;
  s: string;
  Info : integer;
begin
  List.Clear;
  pData := nil;
  try
    Resume := 0;
    Info := NetServerEnum(nil, 100, pData, -1{MAX_PREFERRED_LENGTH}, EntRead, EntTotal, 1{SV_TYPE_WORKSTATION - All LAN Manager workstations}, nil, Resume);
    if Info <> 0 then
      raise Exception.Create('NetServerEnum error ' + IntToStr(Info));

    Assert(pData <> nil);
    for i := 0 to EntRead - 1 do begin
      psvr_Name := Marshal.ReadIntPtr(pData, i * 8 {sizeof(SERVER_INFO_100 )} + 4);
      s := Marshal.PtrToStringUni(psvr_Name);
      List.Add(s);
    end;
  finally
    if pData <> nil then
      NetApiBufferFree(pData);
  end;
end;

procedure TCRNetManager.GetServerList(List: TStrings; const Keywords: string; const Timeout: Longword = 1; const CacheTimeout: Longword = 120);
var
  mList, sl: TStringList;
  i: integer;
  Threads: TCRServicesThread;
{$IFDEF CLR}
  ReturnValue: LongWord;
{$ENDIF}
begin
  List.Clear;
  if Timeout = 0 then
    Exit;

  StartWait;
  mList := nil;
  try
    mList := TStringList.Create;
  {$IFDEF VER6P}
    mList.CaseSensitive := False;
  {$ENDIF}
    mList.Sorted := True;

    if GetTickInterval(FLastTickCount, GetTickCount) > CacheTimeout * 1000 then begin
      GetServerList(mList);
      mList.Add('localhost');
      Threads := TCRServicesThread.Create(mList, Keywords);
    {$IFDEF CLR}
      Threads.WaitFor(Timeout * 1000, ReturnValue);
    {$ELSE}
      WaitForSingleObject(Threads.Handle, Timeout * 1000);
    {$ENDIF}
      FLastTickCount := GetTickCount;
    end;

    if FCachedServerList <> nil then begin
      mList.Clear;
      FServicesCS.Acquire;
      try
        i := FCachedServerList.IndexOf(Keywords);
        if i <> -1 then begin
          sl := FCachedServerList.Objects[i] as TStringList;
          for i := 0 to sl.Count - 1 do begin
            if mList.IndexOf(sl[i]) = -1 then
              mList.Add(sl[i]);
          end;
        end;
      finally
        FServicesCS.Release;
      end;
    end;

    List.Assign(mList);
  finally
    StopWait;
    mList.Free;
  end;
end;
{$ENDIF}

{ TDBAccessUtils }

class function TDBAccessUtils.IsObjectDataType(Obj: TDAParam; DataType: TFieldType): boolean;
begin
  Result := Obj.IsObjectDataType(DataType);
end;

class function TDBAccessUtils.GetNational(Obj: TDAParam): boolean;
begin
  Result := Obj.National;
end;

class procedure TDBAccessUtils.CheckConnection(Obj: TCustomDADataSet);
begin
  Obj.CheckConnection;
end;

class procedure TDBAccessUtils.CheckConnection(Obj: TCustomDASQL);
begin
  Obj.CheckConnection;
end;

class function TDBAccessUtils.UsedConnection(Obj: TCustomDADataSet): TCustomDAConnection;
begin
  Result := Obj.UsedConnection;
end;

class function TDBAccessUtils.UsedConnection(Obj: TCustomDASQL): TCustomDAConnection;
begin
  Result := Obj.UsedConnection;
end;

class function TDBAccessUtils.UsedConnection(Obj: TDAMetaData): TCustomDAConnection;
begin
  Result := Obj.UsedConnection;
end;

class procedure TDBAccessUtils.SetAutoCommit(Obj: TComponent; Value: boolean);
begin
  Assert(Obj <> nil);
  if Obj is TCustomDASQL then
    TCustomDASQL(Obj).AutoCommit := Value
  else
  if IsClass(Obj, TCustomDADataSet) then
    TCustomDADataSet(Obj).AutoCommit := Value
  else
    Assert(False, Obj.ClassName);
end;

class function TDBAccessUtils.GetAutoCommit(Obj: TCustomDAConnection): boolean;
begin
  Result := Obj.AutoCommit;
end;

class function TDBAccessUtils.GetAutoCommit(Obj: TCustomDADataSet): boolean;
begin
  Result := Obj.AutoCommit;
end;

class function TDBAccessUtils.GetAutoCommit(Obj: TCustomDASQL): boolean;
begin
  Result := Obj.AutoCommit;
end;

class procedure TDBAccessUtils.SetDesignCreate(Obj: TDATransaction; Value: boolean);
begin
  Obj.FDesignCreate := Value;
end;

class function TDBAccessUtils.GetDesignCreate(Obj: TDATransaction): boolean;
begin
  Result := Obj.FDesignCreate;
end;

class procedure TDBAccessUtils.SetDesignCreate(Obj: TCustomDADataSet; Value: boolean);
begin
  Obj.FDesignCreate := Value;
end;

class function TDBAccessUtils.GetDesignCreate(Obj: TCustomDADataSet): boolean;
begin
  Result := Obj.FDesignCreate;
end;

class procedure TDBAccessUtils.SetDesignCreate(Obj: TCustomDASQL; Value: boolean);
begin
  Obj.FDesignCreate := Value;
end;

class function TDBAccessUtils.GetDesignCreate(Obj: TCustomDASQL): boolean;
begin
  Result := Obj.FDesignCreate;
end;

class procedure TDBAccessUtils.SetDesignCreate(Obj: TCustomDAUpdateSQL; Value: boolean);
begin
  Obj.FDesignCreate := Value;
end;

class function TDBAccessUtils.GetDesignCreate(Obj: TCustomDAUpdateSQL): boolean;
begin
  Result := Obj.FDesignCreate;
end;

class procedure TDBAccessUtils.SetDesignCreate(Obj: TDAMetaData; Value: boolean);
begin
  Obj.FDesignCreate := Value;
end;

class function TDBAccessUtils.GetDesignCreate(Obj: TDAMetaData): boolean;
begin
  Result := Obj.FDesignCreate;
end;

class procedure TDBAccessUtils.SetDesignCreate(Obj: TCRDataSource; Value: boolean);
begin
  Obj.FDesignCreate := Value;
end;

class function TDBAccessUtils.GetDesignCreate(Obj: TCRDataSource): boolean;
begin
  Result := Obj.FDesignCreate;
end;

class procedure TDBAccessUtils.SetLockLoginPrompt(Obj: TCustomDAConnection; Value: Boolean);
begin
  Obj.FLockLoginPrompt := Value;
end;

class function TDBAccessUtils.CreateIRecordSet(Obj: TCustomDAConnection): TCRRecordSet;
begin
  Result := Obj.CreateIRecordSet;
end;

class function TDBAccessUtils.GetIConnection(Obj: TCustomDAConnection): TCRConnection;
begin
  Result := Obj.FIConnection;
end;

class procedure TDBAccessUtils.CreateIConnection(Obj: TCustomDAConnection);
begin
  Obj.CreateIConnection;
end;

class function TDBAccessUtils.GetUpdateQuery(Obj: TCustomDADataSet): TComponent;
begin
  Result := Obj.FDataSetService.FUpdater.FUpdateQuery;
end;

class function TDBAccessUtils.GetTablesInfo(Obj: TCustomDADataSet): TCRTablesInfo;
begin
  Result := Obj.TablesInfo;
end;

class function TDBAccessUtils.GetSQLInfo(Obj: TCustomDADataSet): TSQLInfo;
begin
  Result := Obj.SQLInfo;
end;

class function TDBAccessUtils.GetUpdatingTable(Obj: TCustomDADataSet): _string;
begin
  Result := Obj.UpdatingTable;
end;

class procedure TDBAccessUtils.SetUpdatingTable(Obj: TCustomDADataSet; Value: _string);
begin
  Obj.UpdatingTable := Value;
end;

class function TDBAccessUtils.GetUpdatingTableIdx(Obj: TCustomDADataSet): integer;
begin
  Assert(Obj.FDataSetService <> nil);
  Result := Obj.FDataSetService.UpdatingTableInfoIdx;
end;

{class procedure TDBAccessUtils.SetUpdatingTableIdx(Obj: TCustomDADataSet; Value: integer);
begin
  Obj.FUpdatingTableInfoIdx := Value;
end;}

class procedure TDBAccessUtils.InternalConnect(Obj: TCustomDAConnection);
begin
  Assert(Obj <> nil);
  Obj.InternalConnect;
end;

class procedure TDBAccessUtils.InternalDisconnect(Obj: TCustomDAConnection);
begin
  Assert(Obj <> nil);
  Obj.InternalDisconnect;
end;

class procedure TDBAccessUtils.DisconnectTransaction(Obj: TCustomDAConnection);
begin
  Obj.DisconnectTransaction;
end;

class procedure TDBAccessUtils.SuppressAutoCommit(Obj: TCustomDAConnection);
begin
  Obj.SuppressAutoCommit;
end;

class procedure TDBAccessUtils.RestoreAutoCommit(Obj: TCustomDAConnection);
begin
  Obj.RestoreAutoCommit;
end;

class function TDBAccessUtils.IsMultipleTransactionsSupported(Obj: TCustomDAConnection): boolean;
begin
  Result := Obj.IsMultipleTransactionsSupported;
end;

class function TDBAccessUtils.PushOperation(Obj: TCustomDAConnection; Operation: TConnLostCause; AllowFailOver: boolean = true): integer;
begin
  Result := Obj.PushOperation(Operation, AllowFailOver);
end;

class function TDBAccessUtils.PopOperation(Obj: TCustomDAConnection): TConnLostCause;
begin
  Result := Obj.PopOperation;
end;

class procedure TDBAccessUtils.RestoreAfterFailOver(Obj: TCustomDAConnection);
begin
  Obj.RestoreAfterFailOver;
end;

class function TDBAccessUtils.IsFailOverAllowed(Obj: TCustomDAConnection): boolean;
begin
  Result := Obj.IsFailOverAllowed;
end;

class function TDBAccessUtils.UsedTransaction(Obj: TCustomDAConnection): TDATransaction;
begin
  Result := Obj.UsedTransaction;
end;

class function TDBAccessUtils.UsedTransaction(Obj: TCustomDADataSet): TDATransaction;
begin
  Result := Obj.UsedTransaction;
end;

class function TDBAccessUtils.UsedTransaction(Obj: TCustomDASQL): TDATransaction;
begin
  Result := Obj.UsedTransaction;
end;

class function TDBAccessUtils.GetTransaction(Obj: TCustomDADataSet): TDATransaction;
begin
  Result := Obj.Transaction;
end;

class function TDBAccessUtils.GetTransaction(Obj: TCustomDASQL): TDATransaction;
begin
  Result := Obj.Transaction;
end;

class function TDBAccessUtils.GetTransaction(Obj: TDAMetaData): TDATransaction;
begin
  Result := Obj.Transaction;
end;

class function TDBAccessUtils.GetDefaultTransaction(Obj: TCustomDAConnection): TDATransaction;
begin
  Result := Obj.DefaultTransaction;
end;

class procedure TDBAccessUtils.SetTransaction(Obj: TCustomDADataSet; Value: TDATransaction);
begin
  Obj.Transaction := Value;
end;

class procedure TDBAccessUtils.SetTransaction(Obj: TCustomDASQL; Value: TDATransaction);
begin
  Obj.Transaction := Value;
end;

class procedure TDBAccessUtils.SetTransaction(Obj: TDAMetaData; Value: TDATransaction);
begin
  Obj.Transaction := Value;
end;

class procedure TDBAccessUtils.SetDefaultTransaction(Obj: TCustomDAConnection; Value: TDATransaction);
begin
  Obj.DefaultTransaction := Value;
end;

class function TDBAccessUtils.GetFTransaction(Obj: TCustomDADataSet): TDATransaction;
begin
  Result := Obj.FTransaction;
end;

class function TDBAccessUtils.GetFTransaction(Obj: TCustomDASQL): TDATransaction;
begin
  Result := Obj.FTransaction;
end;

class function TDBAccessUtils.GetFTransaction(Obj: TDAMetaData): TDATransaction;
begin
  Result := Obj.FTransaction;
end;

class function TDBAccessUtils.GetFDefaultTransaction(Obj: TCustomDAConnection): TDATransaction;
begin
  Result := Obj.FDefaultTransaction;
end;

class function TDBAccessUtils.GetITransaction(Obj: TDATransaction): TCRTransaction;
begin
  Result := Obj.FITransaction;
end;

class function TDBAccessUtils.GetConnectionCount(Obj: TDATransaction): integer;
begin
  Result := Obj.ConnectionsCount;
end;

class function TDBAccessUtils.GetConnection(Obj: TDATransaction; Index: integer): TCustomDAConnection;
begin
  Result := Obj.Connections[Index];
end;

class procedure TDBAccessUtils.Savepoint(Obj: TDATransaction; const Name: _string);
begin
  Obj.DoSavepoint(Name);
end;

class procedure TDBAccessUtils.RollbackToSavepoint(Obj: TDATransaction; const Name: _string);
begin
  Obj.DoRollbackToSavepoint(Name);
end;

class procedure TDBAccessUtils.ReleaseSavepoint(Obj: TDATransaction; const Name: _string);
begin
  Obj.DoReleaseSavepoint(Name);
end;

class procedure TDBAccessUtils.CommitRetaining(Obj: TDATransaction);
begin
  Obj.DoCommitRetaining;
end;

class procedure TDBAccessUtils.RollbackRetaining(Obj: TDATransaction);
begin
  Obj.DoRollbackRetaining;
end;

class procedure TDBAccessUtils.GainTransaction(Obj: TDATransaction);
begin
  Obj.GainTransaction;
end;

class procedure TDBAccessUtils.ReleaseTransaction(Obj: TDATransaction);
begin
  Obj.ReleaseTransaction;
end;

class procedure TDBAccessUtils.AutoCommitTransaction(Obj: TDATransaction; NeedCommit: boolean);
begin
  Obj.AutoCommitTransaction(NeedCommit);
end;

class procedure TDBAccessUtils.Disconnect(Obj: TCustomDASQL);
begin
  Obj.Disconnect;
end;

class function TDBAccessUtils.SQLGenerator(Obj: TCustomDADataSet): TDASQLGenerator;
begin
  Result := Obj.FDataSetService.FSQLGenerator;
end;

class procedure TDBAccessUtils.GetKeyAndDataFields(
  Obj: TCustomDADataSet;
  out KeyAndDataFields: TKeyAndDataFields;
  const ForceUseAllKeyFields: boolean);
begin
  Assert(Obj.FDataSetService <> nil);
  Obj.FDataSetService.GetKeyAndDataFields(KeyAndDataFields, ForceUseAllKeyFields);
end;

class function TDBAccessUtils.GetLockDebug(Obj: TComponent): boolean;
begin
  if Obj is TCustomDADataSet then 
    Result := TCustomDADataSet(Obj).FLockDebug
  else
  if Obj is TCustomDASQL then 
    Result := TCustomDASQL(Obj).FLockDebug
  else
  begin
    Result := False;
    Assert(False, 'Obj is ' + Obj.ClassName);
  end;
end;

class procedure TDBAccessUtils.SetLockDebug(Obj: TComponent; Value: boolean);
begin
  if IsClass(Obj, TCustomDADataSet) then
    TCustomDADataSet(Obj).FLockDebug := Value
  else
  if IsClass(Obj, TCustomDASQL) then
    TCustomDASQL(Obj).FLockDebug := Value
  else
    Assert(False, 'Obj is ' + Obj.ClassName);
end;

class function TDBAccessUtils.FOwner(Obj: TDAConnectionOptions): TCustomDAConnection;
begin
  Result := Obj.FOwner;
end;

class function TDBAccessUtils.FOwner(Obj: TDADataSetOptions): TCustomDADataSet;
begin
  Result := Obj.FOwner;
end;

class function TDBAccessUtils.SQLMonitorClass(Obj: TCustomDAConnection): TClass;
begin
  Result := Obj.SQLMonitorClass;
end;

class function TDBAccessUtils.ConnectDialogClass(Obj: TCustomDAConnection): TConnectDialogClass;
begin
  Result := Obj.ConnectDialogClass;
end;

class function TDBAccessUtils.QuoteName(Obj: TCustomDADataSet; const AName: _string): _string;
begin
  Obj.CheckDataSetService;
  Result := Obj.FDataSetService.QuoteName(AName);
end;

class function TDBAccessUtils.GetSQLs(Obj: TCustomDAConnection): TDAList;
begin
  Result := Obj.FSQLs;
end;

class procedure TDBAccessUtils.RegisterClient(Obj: TCustomDAConnection; Client: TObject; Event: TConnectChangeEvent = nil);
begin
  Obj.RegisterClient(Client, Event);
end;

class procedure TDBAccessUtils.UnRegisterClient(Obj: TCustomDAConnection; Client: TObject);
begin
  Obj.UnRegisterClient(Client);
end;

class function TDBAccessUtils.GetIdentityField(Obj: TCustomDADataSet): TCRFieldDesc;
begin
  Assert(Obj <> nil);
  Assert(Obj.FDataSetService <> nil);
  Result := Obj.FDataSetService.FIdentityField;
end;

class function TDBAccessUtils.GetSQL(Obj: TComponent): _TStrings;
begin
  Result := nil;
  Assert(Obj <> nil);
  if IsClass(Obj, TCustomDASQL) then
    Result := TCustomDASQL(Obj).SQL
  else
  if IsClass(Obj, TCustomDADataSet) then
    Result := TCustomDADataSet(Obj).SQL
  else
    Assert(False, Obj.ClassName);
end;

class procedure TDBAccessUtils.SetSQL(Obj: TComponent; Value: _TStrings);
begin
  Assert(Obj <> nil);
  if Obj is TCustomDASQL then
    TCustomDASQL(Obj).SQL := Value
  else
  if Obj is TCustomDADataSet then
    TCustomDADataSet(Obj).SQL := Value
  else
    Assert(False, Obj.ClassName);
end;

class function TDBAccessUtils.GetSQLText(Obj: TComponent): _string;
begin
  Assert(Obj <> nil);
  if IsClass(Obj, TCustomDASQL) then
    Result := TCustomDASQL(Obj).FinalSQL
  else
  if IsClass(Obj, TCustomDADataSet) then
    Result := TCustomDADataSet(Obj).FinalSQL
  else
    Assert(False, Obj.ClassName);
end;

class procedure TDBAccessUtils.SetSQLText(Obj: TComponent; const SQLText: _string;
  const LockScanParams, LockMacros: boolean);
begin
  Assert(Obj <> nil);
  if IsClass(Obj, TCustomDASQL) then begin
    try
      TCustomDASQL(Obj).FLockMacros := LockMacros;
      TCustomDASQL(Obj).FLockScanParams := LockScanParams;
      TCustomDASQL(Obj).SQL.Text := SQLText;
    finally
      TCustomDASQL(Obj).FLockMacros := False;
      TCustomDASQL(Obj).FLockScanParams := False;
    end;
  end
  else
  if IsClass(Obj, TCustomDADataSet) then
    SetSQLText(TCustomDADataSet(Obj).FCommand, SQLText, LockScanParams, LockMacros)
  else
    Assert(False, Obj.ClassName);
end;

class function TDBAccessUtils.GetParams(Obj: TComponent): TDAParams;
begin
  Result := nil;
  Assert(Obj <> nil);
  if IsClass(Obj, TCustomDASQL) then
    Result := TCustomDASQL(Obj).Params
  else
  if IsClass(Obj, TCustomDADataSet) then
    Result := TCustomDADataSet(Obj).Params
  else
    Assert(False, Obj.ClassName);
end;

class procedure TDBAccessUtils.Execute(Obj: TComponent);
begin
  Assert(Obj <> nil);
  if IsClass(Obj, TCustomDASQL) then
    TCustomDASQL(Obj).Execute
  else
  if IsClass(Obj, TCustomDADataSet) then
    TCustomDADataSet(Obj).Execute
  else
    Assert(False, Obj.ClassName);
end;

class procedure TDBAccessUtils.Open(Obj: TComponent);
begin
  Assert(Obj <> nil);
  Assert(Obj is TCustomDADataSet);
  TCustomDADataSet(Obj).Open;
end;

class function TDBAccessUtils.GetRowsAffected(Obj: TComponent): integer;
begin
  Result := 0;
  Assert(Obj <> nil);
  if IsClass(Obj, TCustomDASQL) then
    Result := TCustomDASQL(Obj).RowsAffected
  else
  if IsClass(Obj, TCustomDADataSet) then
    Result := TCustomDADataSet(Obj).RowsAffected
  else
    Assert(False, Obj.ClassName);
end;

class function TDBAccessUtils.GetUpdateSQLStatementTypes(Obj: TCustomDADataSet): TStatementTypes;
begin
  Result := Obj.GetUpdateSQLStatementTypes;
end;

class function TDBAccessUtils.GetUpdateSQLIndex(Obj: TCustomDADataSet; StatementType: TStatementType): _TStrings;
begin
  Result := nil;
  if Assigned(Obj.UpdateObject) then
    if Obj.UpdateObject.GetObjectIndex(Ord(StatementType)) = nil then
      Result := Obj.UpdateObject.GetSQLIndex(Ord(StatementType))
    else
      Exit;
  if Result = nil then
    Result := Obj.GetUpdateSQLIndex(Ord(StatementType));
end;

class function TDBAccessUtils.ParseSQL(Obj: TCustomDASQL; const SQL: _string; Params: TDAParams; RenamePrefix: _string = ''): _string;
begin
  Result := Obj.ParseSQL(SQL, Params, RenamePrefix);
end;

class function TDBAccessUtils.CreateParamsObject(Obj: TCustomDASQL): TDAParams;
begin
  Result := Obj.CreateParamsObject;
end;

class procedure TDBAccessUtils.SetDesigning(Obj: TComponent; Value: Boolean; SetChildren: Boolean = True);
begin
  if Obj is TCustomDADataSet then
    TCustomDADataSet(Obj).SetDesigning(Value{$IFNDEF FPC}, SetChildren{$ENDIF})
  else
  if Obj is TCustomDASQL then
    TCustomDASQL(Obj).SetDesigning(Value{$IFNDEF FPC}, SetChildren{$ENDIF})
  else
    Assert(False, Obj.ClassName);
end;

class function TDBAccessUtils.GetIRecordSet(Obj: TCustomDADataSet): TCRRecordSet;
begin
  Result := Obj.FIRecordSet;
end;

class procedure TDBAccessUtils.CheckIRecordSet(Obj: TCustomDADataSet);
begin
  Obj.CheckIRecordSet;
end;

class function TDBAccessUtils.GetICommand(Obj: TCustomDADataSet): TCRCommand;
begin
  Result := Obj.FICommand;
end;

class function TDBAccessUtils.GetICommand(Obj: TCustomDASQL): TCRCommand;
begin
  Result := Obj.FICommand;
end;

class function TDBAccessUtils.GetUpdater(Obj: TCustomDADataSet): TDADataSetUpdater;
begin
  Assert(Obj.FDataSetService <> nil);
  Result := Obj.FDataSetService.FUpdater;
end;

class function TDBAccessUtils.GetDataSetService(Obj: TCustomDADataSet): TDADataSetService;
begin
  Result := Obj.FDataSetService;
end;

class function TDBAccessUtils.GetDataSetClass(Obj: TCustomDAUpdateSQL): TCustomDADataSetClass;
begin
  Result := Obj.DataSetClass;
end;

class function TDBAccessUtils.GetSQLClass(Obj: TCustomDAUpdateSQL): TCustomDASQLClass;
begin
  Result := Obj.SQLClass;
end;

class function TDBAccessUtils.GetParserClass(Obj: TMacros): TSQLParserClass;
begin
  Result := Obj.FParserClass;
end;

{$IFDEF MSWINDOWS}
class procedure TDBAccessUtils.SaveServerListToRegistry(Obj: TCustomConnectDialog);
begin
  Obj.SaveServerListToRegistry;
end;
{$ENDIF}

class procedure TDBAccessUtils.SetConnection(Obj: TCustomConnectDialog; Value: TCustomDAConnection);
begin
  Obj.FConnection := Value;
end;

class procedure TDBAccessUtils.SetUseServerHistory(Obj: TCustomConnectDialog; Value: boolean);
begin
  Obj.UseServerHistory := Value;
end;

class function TDBAccessUtils.GetNeedConnect(Obj: TCustomConnectDialog): boolean;
begin
  Result := Obj.FNeedConnect;
end;

class procedure TDBAccessUtils.SetNeedConnect(Obj: TCustomConnectDialog; Value: boolean);
begin
  Obj.FNeedConnect := Value;
end;

class procedure TDBAccessUtils.CreateProcCall(Obj: TCustomDASQL; const Name: _string; NeedDescribe: boolean;
  IsQuery: boolean = False);
begin
  Obj.InternalCreateProcCall(Name, NeedDescribe, IsQuery);
end;

class procedure TDBAccessUtils.CreateProcCall(Obj: TCustomDADataSet; const Name: _string; NeedDescribe: boolean;
  IsQuery: boolean = False);
begin
  Obj.InternalCreateProcCall(Name, NeedDescribe, IsQuery);
end;

class function TDBAccessUtils.GetCommand(Obj: TCustomDAConnection): TCustomDASQL;
begin
  Result := Obj.FCommand;
end;

class function TDBAccessUtils.GetStreamedConnected(Obj: TCustomDAConnection): boolean;
begin
  Result := Obj.FStreamedConnected;
end;

class procedure TDBAccessUtils.Loaded(Obj: TCustomDAConnection);
begin
  Obj.Loaded;
end;

class function TDBAccessUtils.GetAsCursor(Obj: TDAParam): TCRCursor;
begin
  Result := Obj.AsCursor;
end;

class function TDBAccessUtils.GetCursor(Obj: TCustomDADataSet): TCRCursor;
begin
  Result := Obj.Cursor;
end;

class procedure TDBAccessUtils.SetCursor(Obj: TCustomDADataSet; Value: TCRCursor);
begin
  Obj.Cursor := Value;
end;

class function TDBAccessUtils.GetFetchAll(Obj: TCustomDADataSet): boolean;
begin
  // in UniDAC returns correct value only if DataSet.Active
  Result := Obj.FetchAll;
end;

class procedure TDBAccessUtils.SetFetchAll(Obj: TCustomDADataSet; Value: boolean);
begin
  // in UniDAC this value can be overriden from specific options
  Obj.FetchAll := Value;
end;

class function TDBAccessUtils.GetKeyFields(Obj: TCustomDADataSet): _string;
begin
  Result := Obj.KeyFields;
end;

class procedure TDBAccessUtils.SetKeyFields(Obj: TCustomDADataSet; const Value: _string);
begin
  Obj.KeyFields := Value;
end;

class procedure TDBAccessUtils.QuickOpen(Obj: TCustomDADataSet; var Info: TQuickOpenInfo);
begin
  Obj.QuickOpen(Info, False);
end;

class procedure TDBAccessUtils.Restore(Obj: TCustomDADataSet; const Info: TQuickOpenInfo);
begin
  Obj.Restore(Info, True);
end;

class function TDBAccessUtils.GetLockMode(Obj: TCustomDADataSet): TLockMode;
begin
  Result := Obj.LockMode;
end;

class procedure TDBAccessUtils.SetLockMode(Obj: TCustomDADataSet; const Value: TLockMode);
begin
  Obj.LockMode := Value;
end;

class function TDBAccessUtils.GetLastInsertId(Obj: TCustomDADataSet): int64;
begin
  Result := Obj.FLastInsertId;
end;

{ TCRDataSource }

constructor TCRDataSource.Create(Owner: TComponent);
begin
  inherited Create(Owner);

  FDesignCreate := csDesigning in ComponentState;
end;

procedure TCRDataSource.Loaded;
begin
  inherited;

  FDesignCreate := False;
end;

procedure TCRDataSource.AssignTo(Dest: TPersistent);
begin
  if Dest is TDataSource then begin
    TDataSource(Dest).DataSet := DataSet;
    TDataSource(Dest).AutoEdit := AutoEdit;
    TDataSource(Dest).Enabled := Enabled;
  end else
    inherited;
end;

{ TDAConnections }

function TDAConnections.GetItems(Index: integer): TCustomDAConnection;
begin
  Result := TCustomDAConnection(inherited Items[Index]);
end;

{ TDATransactions}

function TDATransactions.GetItems(Index: Integer): TDATransaction;
begin
  Result := TDATransaction(inherited Items[Index]);
end;

{ TDATransaction }

constructor TDATransaction.Create(AOwner: TComponent);
begin
  inherited;

  FDesignCreate := csDesigning in ComponentState;

  FConnections := TDAConnections.Create;
  FTransactionType := ttNative;
  FIsolationLevel := ilReadCommitted;
  FDefaultCloseAction := taRollback;
end;

destructor TDATransaction.Destroy;
begin
  CloseTransaction(True);
  ClearRefs;

  FreeITransaction;
  FConnections.Free;

  if not FShareTransaction then
    TDASQLMonitorClass(SQLMonitorClass).ObjectDestroyed(Self);

  inherited;
end;

procedure TDATransaction.Loaded;
begin
  inherited;

  FDesignCreate := False;
end;

procedure TDATransaction.Notification(AComponent: TComponent; Operation: TOperation);
begin
  if (Operation = opRemove) and (AComponent = DefaultConnection) then
    DefaultConnection := nil;

  inherited;
end;

function TDATransaction.GetITransactionClass: TCRTransactionClass;
begin
{$IFDEF MSWINDOWS}
{$IFNDEF LITE}
  if FTransactionType = ttMTS then
    Result := TMTSTransaction
  else
{$ENDIF}
{$ENDIF}
  begin
    Assert(False, 'Must be overrided');
    Result := TCRTransaction;
  end;  
end;

procedure TDATransaction.CheckITransaction;
var
  ClassType: TCRTransactionClass;
begin
  ClassType := GetITransactionClass;

  if not (FITransaction is ClassType) then begin
    FreeITransaction;

    Assert(not FShareTransaction or (FDefaultConnection <> nil));
    if FShareTransaction then begin
      Assert(FDefaultConnection.FIConnection <> nil);
      SetITransaction(FDefaultConnection.FIConnection.GetInternalTransaction);
    end
    else begin
      CreateITransaction;
    end;

  end;
end;

procedure TDATransaction.CreateITransaction;
begin
  SetITransaction(GetITransactionClass.Create);
end;

procedure TDATransaction.SetITransaction(Value: TCRTransaction);
begin
  if FITransaction <> nil then
    FreeITransaction;

  FITransaction := Value;

  if FITransaction <> nil then begin
    FITransaction.SetProp(prIsolationLevel, Variant(FIsolationLevel));
    FITransaction.SetProp(prTransactionReadOnly, FReadOnly);

    FITransaction.OnError := DoError;
    FITransaction.Component := Self;
  end;
end;

procedure TDATransaction.FreeITransaction;
begin
  if not FShareTransaction then
    FITransaction.Free;
  FITransaction := nil;
end;

procedure TDATransaction.ClearRefs;
var
  i,j: integer;
begin
  for i := 0 to FConnections.Count - 1 do begin
    for j := 0 to FConnections[i].DataSetCount - 1 do
      if FConnections[i].DataSets[j] is TCustomDADataSet then begin
        if TCustomDADataSet(FConnections[i].DataSets[j]).Transaction = Self then
          TCustomDADataSet(FConnections[i].DataSets[j]).Transaction := nil;
      end
      else
      if FConnections[i].DataSets[j] is TDAMetaData then begin
        if TDAMetaData(FConnections[i].DataSets[j]).Transaction = Self then
          TDAMetaData(FConnections[i].DataSets[j]).Transaction := nil;
      end;

    for j := 0 to FConnections[i].FSQLs.Count - 1 do
      if TCustomDASQL(FConnections[i].FSQLs[j]).Transaction = Self then
        TCustomDASQL(FConnections[i].FSQLs[j]).Transaction := nil;
  end;

  for i := FConnections.Count - 1 downto 0 do
    DoRemoveConnection(FConnections[i]);
end;

procedure TDATransaction.SetIsolationLevel(Value: TCRIsolationLevel);
begin
  CheckInactive;
  if FITransaction <> nil then
    FITransaction.SetProp(prIsolationLevel, Variant(Value));
  FIsolationLevel := Value;
end;

procedure TDATransaction.SetReadOnly(Value: boolean);
begin
  CheckInactive;
  if FITransaction <> nil then
    FITransaction.SetProp(prTransactionReadOnly, Value);
  FReadOnly := Value;
end;

procedure TDATransaction.SetTransactionType(Value: TTransactionType);
begin
  CheckInactive;

  if FTransactionType <> Value then begin
    FreeITransaction;

    FTransactionType := Value;
  end;
end;

function TDATransaction.GetConnection(Index: integer): TCustomDAConnection;
begin
  Result := FConnections.Items[Index];
end;

function TDATransaction.GetConnectionsCount: integer;
begin
  Result := FConnections.Count;
end;

function TDATransaction.DetectInTransaction(CanActivate: boolean = False): boolean;
begin
  if FITransaction <> nil then
    Result := FITransaction.GetInTransaction
  else
    Result := False;
end;

function TDATransaction.GetActive: boolean;
begin
  Result := DetectInTransaction;
end;

procedure TDATransaction.CheckActive;
begin
  if not Active then
    raise Exception.Create(SNotInTransaction);
end;

procedure TDATransaction.CheckInactive;
begin
  if Active then
    raise Exception.Create(SInTransaction);
end;

procedure TDATransaction.Reset; //This function called in case of FatalError + Failover
var
  i, j: Integer;
begin
  if Active then begin
    if FITransaction.CanRestoreAfterFailover then
      FFailOverSatus := FTrStartCount
    else
      FFailOverSatus := 0;

    //Close RecordSets handles
    for i := 0 to FConnections.Count - 1 do begin
      for j := 0 to FConnections[i].DataSetCount - 1 do
        if (FConnections[i].DataSets[j] is TCustomDADataSet) and
          (TCustomDADataSet(FConnections[i].DataSets[j]).UsedTransaction = Self) and
          (TCustomDADataSet(FConnections[i].DataSets[j]).FIRecordSet <> nil)
        then
          try
            TCustomDADataSet(FConnections[i].DataSets[j]).FIRecordSet.Disconnect;
          except
            on E: Exception do
              ; //catch handle freeing exceptions
          end;
      for j := 0 to FConnections[i].FSQLs.Count - 1 do
        if TCustomDASQL(FConnections[i].FSQLs[j]).UsedTransaction = Self then
          try
            TCustomDASQL(FConnections[i].FSQLs[j]).Disconnect;
          except
            on E: Exception do
              ; //catch handle freeing exceptions
          end;
    end;

    // Close transaction handle
    try
      case FDefaultCloseAction of
        taCommit:
          FITransaction.Commit;
        taRollback:
          FITransaction.Rollback;
      end;
    except
      FITransaction.Reset;
    end;

    if FTrStartCount = 1 then //to reduce conflict with ConnectCount Restoring in StartTransaction
      for i := 0 to FConnections.Count - 1 do
        FConnections[i].InternalDisconnect; //Decrease ConnectCount
  end
  else
    FFailOverSatus := 0;
  FTrStartCount := 0; //Transaction is closing so reset FTrStartCount
  FExplicitlyStarted := False;
  FUnCommitedStatementCount := 0;
  UnPrepareTransaction;
end;

procedure TDATransaction.Restore; //This function restore transaction at failover time
begin
  if FFailOverSatus > 0 then begin
    if not Active then
      FITransaction.StartTransaction;
    FTrStartCount := FFailOverSatus;
  end;
end;

procedure TDATransaction.CloseDataSets;
var
  i,j: Integer;
begin
  for i := 0 to FConnections.Count - 1 do begin
    if not FConnections[i].IsMultipleTransactionsSupported then
      continue;

    for j := 0 to FConnections[i].DataSetCount - 1 do
      if FConnections[i].DataSets[j] is TCustomDADataSet then begin
        if TCustomDADataSet(FConnections[i].DataSets[j]).UsedTransaction = Self then
          TCustomDADataSet(FConnections[i].DataSets[j]).ConnectChange(FConnections[i], False);//We can't use SendConectEvent(False)
      end
      else
      if FConnections[i].DataSets[j] is TDAMetaData then begin
        if TDAMetaData(FConnections[i].DataSets[j]).UsedTransaction = Self then
          TDAMetaData(FConnections[i].DataSets[j]).ConnectChange(FConnections[i], False);//We can't use SendConectEvent(False)
      end;
    for j := 0 to TCustomDAConnection(FConnections[i]).FSQLs.Count - 1 do
      if TCustomDASQL(FConnections[i].FSQLs[j]).UsedTransaction = Self then
         TCustomDASQL(TCustomDAConnection(FConnections[i]).FSQLs[j]).ConnectChange(FConnections[i], False);
  end;
end;

function TDATransaction.SQLMonitorClass: TClass;
begin
  Result := nil;
  Assert(False, 'Must be overrided');
end;

function TDATransaction.UsedConnection: TCustomDAConnection;
begin
  if FDefaultConnection <> nil then
    Result := FDefaultConnection
  else
  if FConnections.Count > 0 then
    Result := FConnections[0]
  else
    Result := nil;
end;

procedure TDATransaction.CloseTransaction(Force: boolean = False);
begin
  if Active then
    try
      case FDefaultCloseAction of
        taCommit:
          Commit;
        taRollback:
          Rollback;
      end;
    except
      on E: EDAError do begin
        if not((csDestroying in ComponentState) and E.IsFatalError) then
          if not Force then
            raise
          else
            if FITransaction <> nil then
              FITransaction.Reset;
      end
      else
        if not Force then
          raise;
    end;
end;

procedure TDATransaction.GainTransaction;
var
  StoredTrStartCount: integer;
begin
  Inc(FTrStartCount);
  StoredTrStartCount := FTrStartCount;
  // register using ConnectCount only once
  if (FTrStartCount = 1) and not Active then begin
    // transaction could be active here in non disconnect mode
    StartTransaction;
    FExplicitlyStarted := False;
  end;
  // restore TrStartCount only if sucessfuly started
  // (FTrStartCount will be 0 in case of fatal error Reset)
  FTrStartCount := StoredTrStartCount;
end;

procedure TDATransaction.AutoCommitTransaction(NeedCommit: boolean);
begin
  if NeedCommit and (not FExplicitlyStarted or CanAutoCommitExplicitTransaction) then begin
    if FTrStartCount = 1 then
      Commit
    else
      if FTrStartCount > 1 then
        DoCommitRetaining;
  end
  else
    Inc(FUnCommitedStatementCount);
end;

procedure TDATransaction.ReleaseTransaction;
begin
  if FTrStartCount = 1 then begin
    if FDisconnectedMode then begin
      if FUnCommitedStatementCount = 0 then
        CloseTransaction
      else
        Dec(FTrStartCount);
    end
  end
  else
    if FTrStartCount > 1 then
      Dec(FTrStartCount);
end;

function TDATransaction.CanAutoCommitExplicitTransaction: boolean;
begin
  Result := True;
end;

procedure TDATransaction.PrepareTransaction(CheckOnly: boolean = False);
var
  i, j: integer;
begin
  for i := 0 to FConnections.Count - 1 do begin
    if not (not CheckOnly and FConnections[i].Options.DisconnectedMode or
      FConnections[i].Connected)
    then
      raise Exception.Create(SConnectionInTransactionNotActive);
  end;

  if not CheckOnly then
    for i := 0 to FConnections.Count - 1 do
      if FConnections[i].Options.DisconnectedMode then
        try
          FConnections[i].InternalConnect;
        except
          //restore Connection.ConnectCount
          for j := 0 to i do
            if FConnections[i].Options.DisconnectedMode and FConnections[j].Connected then
              FConnections[j].InternalDisconnect; //To avoid ConnectClose exception with the [i] connection
          raise;
        end;

  CheckITransaction;

  FDisconnectedMode := True;
  for i := 0 to FConnections.Count - 1 do begin
    FITransaction.AddConnection(FConnections[i].FIConnection);
    FDisconnectedMode := FDisconnectedMode and FConnections[i].Options.DisconnectedMode;
  end;

  FPrepared := not CheckOnly;
end;

procedure TDATransaction.UnPrepareTransaction;
var
  i: integer;
begin
  if not FPrepared then
    exit;

  FPrepared := False;

  FTrStartCount := 0;
  FExplicitlyStarted := False;
  FUnCommitedStatementCount := 0;

  for i := 0 to FConnections.Count - 1 do
    if not FConnections[i].IsMultipleTransactionsSupported then
      FConnections[i].RestoreAutoCommit;

  for i := 0 to FConnections.Count - 1 do
    FConnections[i].InternalDisconnect; //Decrease ConnectCount
end;

procedure TDATransaction.StartTransaction;
var
  i: integer;
  ReStart: boolean;
  Connection: TCustomDAConnection;
  MessageID: cardinal;
begin
  CheckInactive;

  for i := 0 to FConnections.Count - 1 do begin
    if not FConnections[i].IsMultipleTransactionsSupported and
      FConnections[i].DetectInTransaction
    then
      raise Exception.Create(SMultipleTransactionsNotSupported);
  end;

  PrepareTransaction;

  TDASQLMonitorClass(SQLMonitorClass).TRStart(Self, MessageID, True);

  Connection := nil;
  if FConnections.Count = 1 then begin //Failover allowed only with non-distributed transactions
    Connection := FConnections[0];
    Connection.PushOperation(clTransStart, Connection.IsFailOverAllowed);
  end;

  try
    repeat
      ReStart := False;
      try
        FITransaction.StartTransaction;
      except
        on E: EFailOver do
          if E.FConnLostCause = clTransStart then begin
            Connection.RestoreAfterFailOver; //Restore all read transactions
            PrepareTransaction;
            ReStart := True  //We should pass clConnectionApplyUpdates FailOver
          end
          else
            raise;
      end;
    until not ReStart;
  finally
    if Connection <> nil then
      Connection.PopOperation;
    if not Active then  //In case of fatal error during transaction start restore ConnectCount
                        //or even close connection in DisconnectedMode
      for i := 0 to FConnections.Count - 1 do
        TDBAccessUtils.InternalDisconnect(FConnections[i]);
  end;

  for i := 0 to FConnections.Count - 1 do
    if not FConnections[i].IsMultipleTransactionsSupported then
      FConnections[i].SuppressAutoCommit;

  FTrStartCount := 1;
  FExplicitlyStarted := True;
  TDASQLMonitorClass(SQLMonitorClass).TRStart(Self, MessageID, False);

  if Assigned(FOnStart) then
    FOnStart(Self);
end;

procedure TDATransaction.Commit;
var
  MessageID: cardinal;
begin
  // In ODAC Commit/Rollback can be used without explicit transaction start.
  if not FPrepared then
    PrepareTransaction;

  TDASQLMonitorClass(SQLMonitorClass).TRCommit(Self, MessageID, True);
  CloseDataSets;
  try
    FITransaction.Commit;
    TDASQLMonitorClass(SQLMonitorClass).TRCommit(Self, MessageID, False);
  finally
    if (FITransaction = nil) or not FITransaction.GetInTransaction then
      UnPrepareTransaction;
  end;

  if Assigned(FOnCommit) then
    FOnCommit(Self);
end;

procedure TDATransaction.Rollback;
var
  MessageID: cardinal;
begin
  if not FPrepared then
    PrepareTransaction;

  TDASQLMonitorClass(SQLMonitorClass).TRRollback(Self, MessageID, True);
  CloseDatasets;
  try
    FITransaction.Rollback;
    TDASQLMonitorClass(SQLMonitorClass).TRRollback(Self, MessageID, False);
  finally
    if (FITransaction = nil) or not FITransaction.GetInTransaction then
      UnPrepareTransaction;
  end;

  if Assigned(FOnRollback) then
    FOnRollback(Self);
end;

procedure TDATransaction.DoCommitRetaining;
var
  MessageID: cardinal;
begin
  CheckActive;
  TDASQLMonitorClass(SQLMonitorClass).TRCommitRetaining(Self, MessageID, True);

  FITransaction.CommitRetaining;
  FUnCommitedStatementCount := 0;

  TDASQLMonitorClass(SQLMonitorClass).TRCommitRetaining(Self, MessageID, False);
end;

procedure TDATransaction.DoRollbackRetaining;
var
  MessageID: cardinal;
begin
  CheckActive;
  TDASQLMonitorClass(SQLMonitorClass).TRRollbackRetaining(Self, MessageID, True);

  FITransaction.RollbackRetaining;
  FUnCommitedStatementCount := 0;

  TDASQLMonitorClass(SQLMonitorClass).TRRollbackRetaining(Self, MessageID, False);
end;

procedure TDATransaction.DoSavepoint(const Name: _string);
var
  MessageID: cardinal;
begin
  // In ODAC savepoints can be used without explicit transaction start
  if not FPrepared then
    PrepareTransaction(True);

  TDASQLMonitorClass(SQLMonitorClass).TRSavepoint(Self, Name, MessageID, True);

  FITransaction.Savepoint(Name);

  TDASQLMonitorClass(SQLMonitorClass).TRSavepoint(Self, Name, MessageID, False);
end;

procedure TDATransaction.DoReleaseSavepoint(const Name: _string);
var
  MessageID: cardinal;
begin
  if not FPrepared then
    PrepareTransaction(True);

  TDASQLMonitorClass(SQLMonitorClass).TRReleaseSavepoint(Self, Name, MessageID, True);

  FITransaction.ReleaseSavepoint(Name);

  TDASQLMonitorClass(SQLMonitorClass).TRReleaseSavepoint(Self, Name, MessageID, False);
end;

procedure TDATransaction.DoRollbackToSavepoint(const Name: _string);
var
  MessageID: cardinal;
begin
  if not FPrepared then
    PrepareTransaction(True);

  TDASQLMonitorClass(SQLMonitorClass).TRRollbackToSavepoint(Self, Name, MessageID, True);

  FITransaction.RollbackToSavepoint(Name);

  TDASQLMonitorClass(SQLMonitorClass).TRRollbackToSavepoint(Self, Name, MessageID, False);
end;

function TDATransaction.InternalAddConnection(Connection: TCustomDAConnection): integer;
begin
  if Active then
    if TransactionType = ttMTS then
      FITransaction.AddConnection(Connection.FIConnection)
    else
      CloseTransaction;
  Result := FConnections.IndexOf(Connection);
  if Result = -1 then
    Result := FConnections.Add(Connection);
end;

procedure TDATransaction.InternalRemoveConnection(Connection: TCustomDAConnection);
begin
  if Active and (TransactionType <> ttMTS) then
    CloseTransaction;

  if Connection = FDefaultConnection then
    FDefaultConnection := nil;
  FConnections.Remove(Connection);
  if not FShareTransaction and (Connection <> nil) and (FITransaction <> nil) then
    FITransaction.RemoveConnection(Connection.FIConnection);
end;

function TDATransaction.DoAddConnection(Connection: TCustomDAConnection): integer;
begin
  Result := InternalAddConnection(Connection);

  if Connection <> nil then
    Connection.InternalAddTransaction(Self);
end;

procedure TDATransaction.DoRemoveConnection(Connection: TCustomDAConnection);
begin
  InternalRemoveConnection(Connection);

  if Connection <> nil then
    Connection.InternalRemoveTransaction(Self);
end;

procedure TDATransaction.DoClearConnections;
begin
  while FConnections.Count > 0 do
    DoRemoveConnection(FConnections[0]);
end;

procedure TDATransaction.DoError(E: Exception; var Fail: boolean);
begin
  TDASQLMonitorClass(SQLMonitorClass).DBError(EDAError(E));

  if Assigned(FOnError) then
    FOnError(Self, EDAError(E), Fail);

  if not FInProcessError and EDAError(E).IsFatalError then begin
    FInProcessError := True;
    try
      CloseTransaction(True);
    except // don't raise exception
    end;
    FInProcessError := False;
  end;
end;

procedure TDATransaction.SetDefaultConnection(Value: TCustomDAConnection);
begin
  if Value <> FDefaultConnection then begin
    if FDefaultConnection <> nil then begin
      FDefaultConnection.RemoveFreeNotification(Self);
      DoRemoveConnection(FDefaultConnection);
    end;

    if Value <> nil then begin
      DoAddConnection(Value);
      Value.FreeNotification(Self);
    end;
    
    FDefaultConnection := Value;
  end;
end;

function TDATransaction.IsInternalTrStored: boolean;
begin
  if FDefaultConnection <> nil then
    Result := FDefaultConnection.FInternalDefTransaction <> Self
  else
    Result := False;
end;

initialization
  ChangeCursor := True;
  MacroChar := '&';
  SetCursorProc := nil;
  ShowConnectFormProc := nil;
  ShowConnectFormProcFmx := nil;
  BaseSQLOldBehavior := False;
  SQLGeneratorCompatibility := False;
  DefaultSQLInfo := TSQLInfo.Create(nil);
{$IFDEF MSWINDOWS}
{$IFNDEF CLR}
  OpenSCManager := @NotLink;
  CloseServiceHandle := @NotLink;
  OpenService := @NotLink;
  EnumServicesStatus := @NotLink;
  QueryServiceStatus := @NotLink;
  StartService := @NotLink;
  ControlService := @NotLink;
  NetServerEnum := @NotLink;
  NetApiBufferFree := @NotLink;
{$ENDIF}
  LoadNetManagerLib;
  CRNetManager := TCRNetManager.Create;
{$ENDIF}

finalization
  DefaultSQLInfo.Free;
{$IFDEF MSWINDOWS}
  CRNetManager.Free;
  FreeNetManagerLib;
{$ENDIF}

end.

