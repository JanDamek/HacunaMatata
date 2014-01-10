
//////////////////////////////////////////////////
//  SQL Server Data Access Components
//  Copyright © 1998-2012 Devart. All right reserved.
//  MSAccess
//////////////////////////////////////////////////

{$IFNDEF CLR}

{$I Sdac.inc}

unit MSAccess;
{$ENDIF}

{ $R MSAccess.res}

interface

uses
  Classes, SysUtils, DB, Windows,
{$IFDEF CLR}
  Variants, System.XML, System.Text, System.IO,
{$ELSE}
  CLRClasses, CRXml,
{$ENDIF}
  MemUtils, MemData, {$IFDEF FPC}MemDataSet{$ELSE}MemDS{$ENDIF},
  CRTypes, CRAccess, CRParser, CRConnectionPool, DBAccess, DAConsts, DASQLMonitor,
  OLEDBC, OLEDBIntf, OLEDBAccess,
  MSConsts, MSConnectionPool, MSServices,
{$IFNDEF CLR}
  MSUDT,
{$ENDIF}
  Win32Timer;


{$I SdacVer.inc}

const
  ftMSXML = integer(High(TFieldType)) + 1;
  ftMSUDT = integer(High(TFieldType)) + 2;

{ TMSParam }
const
  /// WAR Need to sync with EdMSParams.FieldTypesWithSize on change
  FieldTypesWithSize: set of TFieldType = [ftString, ftFixedChar, {ftMemo - BLOB, }ftBytes, ftVarBytes, ftWideString];
  {$EXTERNALSYM FieldTypesWithSize}

type
  TMSDataSetService = class;
  TMSFileStream = class;

  TIsolationLevel = (ilReadCommitted, ilReadUnCommitted, ilRepeatableRead, ilIsolated, ilSnapshot);

  TMSSqlFilestreamDesiredAccess = (daRead, daWrite, daReadWrite);
  TMSSqlFilestreamOpenOption = (ooAsync, ooNoBuffering, ooNoWriteThrough,
    ooSequentialScan, ooRandomAccess);
  TMSSqlFilestreamOpenOptions = set of TMSSqlFilestreamOpenOption;

  TMSParam = class (TDAParam)
  private
    FTableTypeName: string;
  protected
    function GetSize: integer; override;
    procedure SetSize(Value: integer); override;
    procedure SetDataType(Value: TFieldType); override;

    function GetIsNull: boolean; override;

    procedure SetAsString(const Value: string); override;
    procedure SetAsWideString(const Value: WideString); override;
    procedure SetAsVariant(const Value: Variant); override;
    function GetAsTable: TMSTableObject;
    procedure SetAsTable(Value: TMSTableObject);

    procedure CreateObject; override;

    function IsObjectDataType(DataType: TFieldType): boolean; override;
    property ParamObject;
  public
    constructor Create(Collection: TCollection); override;

    property AsString: string read GetAsString write SetAsString;
    property AsWideString: WideString read GetAsWideString write SetAsWideString;
    property AsTable: TMSTableObject read GetAsTable write SetAsTable;
    property TableTypeName: string read FTableTypeName;
  end;

{ TMSParams }
  TMSParams = class (TDAParams)
  private
    function GetItem(Index: Integer): TMSParam;
    procedure SetItem(Index: Integer; Value: TMSParam);

  public
    constructor Create(Owner: TPersistent);

    procedure Assign(Source: TPersistent); override;

    function ParamByName(const Value: _string): TMSParam;
    function FindParam(const Value: _string): TMSParam;
    property Items[Index: integer]: TMSParam read GetItem write SetItem; default;
  end;

{ TMSConnection }

  TCustomMSConnection = class;

  TCustomMSConnectionOptions = class (TDAConnectionOptions)
  protected
    FQuotedIdentifier: boolean;
    FEncrypt: boolean;
    FProvider: TOLEDBProvider;
    FCompactVersion: TCompactVersion;
    FNativeClientVersion: TNativeClientVersion;
    FDefaultLockTimeout: integer;
  {$IFDEF VER10P}
    FUseWideMemos: boolean;
  {$ENDIF}

    procedure SetQuotedIdentifier(const Value: boolean);
    procedure SetEncrypt(const Value: boolean);
    function GetNumericType: TDANumericType;
    procedure SetNumericType(Value: TDANumericType);
    procedure SetProvider(const Value: TOLEDBProvider); virtual;
    procedure SetCompactVersion(const Value: TCompactVersion);
    procedure SetNativeClientVersion(const Value: TNativeClientVersion);
    procedure SetDefaultLockTimeout(const Value: integer);
  {$IFDEF VER10P}
    procedure SetUseWideMemos(const Value: Boolean);
  {$ENDIF}

  protected
    procedure AssignTo(Dest: TPersistent); override;

    property EnableBCD;
  {$IFDEF VER6P}
  {$IFNDEF FPC}
    property EnableFMTBCD; // CLR cross-assembly
  {$ENDIF}
  {$ENDIF}

    property CompactVersion: TCompactVersion read FCompactVersion write SetCompactVersion default cvAuto;
    property NativeClientVersion: TNativeClientVersion read FNativeClientVersion write SetNativeClientVersion default ncAuto;

  public
    constructor Create(Owner: TCustomMSConnection);

    property QuotedIdentifier: boolean read FQuotedIdentifier write SetQuotedIdentifier default True;
    property Encrypt: boolean read FEncrypt write SetEncrypt default False;

    property NumericType: TDANumericType read GetNumericType write SetNumericType default ntFloat;
    property Provider: TOLEDBProvider read FProvider write SetProvider default prAuto;
    property DefaultLockTimeout: integer read FDefaultLockTimeout write SetDefaultLockTimeout default 2000;
  {$IFDEF VER10P}
    property UseWideMemos: boolean read FUseWideMemos write SetUseWideMemos default True;
  {$ENDIF}

    property KeepDesignConnected;
    property DisconnectedMode;
    property LocalFailover;
  end;

  TMSConnectionInfoMessageEvent = procedure (Sender: TObject; E: EMSError) of object;
  TCustomMSDataSet = class;
  TMSSQL = class;

  TCustomMSConnection = class (TCustomDAConnection)
  protected
    FDatabase: _string;
    FOptions: TCustomMSConnectionOptions;
    FMSSQL: TMSSQL;
    FDataSet: TCustomMSDataSet;

    function GetIConnectionClass: TCRConnectionClass; override;
    function GetICommandClass: TCRCommandClass; override;
    function GetIRecordSetClass: TCRRecordSetClass; override;
    function GetIMetaDataClass: TCRMetaDataClass; override;

    procedure ClearRefs; override;
    procedure CreateIConnection; override;
    procedure SetIConnection(Value: TCRConnection); override;
    procedure SetOptions(Value: TCustomMSConnectionOptions);

    procedure FillConnectionParameters(var ConnectionParameters: TMSConnectionParameters); virtual;
    procedure FillConnectionProps(OLEDBConnection: TOLEDBConnection); virtual;
    function GetOLEDBConnection: TOLEDBConnection;

    function SQLMonitorClass: TClass; override;
    function ConnectDialogClass: TConnectDialogClass; override;

    procedure AssignTo(Dest: TPersistent); override;

    procedure SetDatabase(Value: _string);

    function GetIsolationLevel: TIsolationLevel;
    procedure SetIsolationLevel(const Value: TIsolationLevel);

    procedure DoError(E: Exception; var Fail, Reconnect, Reexecute: boolean; ReconnectAttempt: integer;
      var ConnLostCause: TConnLostCause); override;

    procedure Check(const Status: HRESULT; Sender: TObject);
    procedure CheckInactive;

  { Transaction control }

    procedure DoConnect; override;

    procedure AddConnectStringParam(var Result: _string; const ParamName: _string;
      const Value: _string; const DefValue: _string);
    function GetConnectString: _string; override;

    procedure InitConnectStringOptions; virtual;
    procedure ProcessConnectStringParam(const paramName: string; paramValue: _string); virtual;
    function RecognizedParameter(const Args: array of string; const paramName: string): boolean;
    procedure SetConnectString(Value: _string); override;
    function DefaultTableSchema: _string; override;

    function GetClientVersion: string;
    function GetServerVersion: string;

    function CreateOptions: TDAConnectionOptions; override;

    function IConnection: TOLEDBConnection;
    procedure AssignConnectOptions(Source: TCustomDAConnection); override;
  public
    constructor Create(Owner: TComponent); override;
    destructor Destroy; override;

    function CreateDataSet: TCustomDADataSet; override;
    function CreateSQL: TCustomDASQL; override;
    function CreateTransaction: TDATransaction; override;

    procedure AssignConnect(Source: TCustomMSConnection);
    //procedure GetDatabaseNames(List: TStrings); override;
    //procedure GetStoredProcNames(List: TStrings; AllProcs: boolean = False); overload; override;
//    procedure GetStoredProcNames(List: TStrings; System: boolean); reintroduce; overload;
    procedure GetTableTypeNames(List: _TStrings); virtual;

    procedure OpenDatasets(const ds: array of TCustomMSDataSet);

    property ClientVersion: string read GetClientVersion;
    property ServerVersion: string read GetServerVersion;

    property Database: _string read FDatabase write SetDatabase;
    property IsolationLevel: TIsolationLevel read GetIsolationLevel write SetIsolationLevel default ilReadCommitted;

    property PoolingOptions;
    property Pooling;

    property Options: TCustomMSConnectionOptions read FOptions write SetOptions;

    property Password;

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

  TMSConnection = class;

  TMSConnectionOptions = class(TCustomMSConnectionOptions)
  protected
    FLanguage: _string;
    FPersistSecurityInfo: boolean;
    FAutoTranslate: boolean;
    FNetworkLibrary: _string;
    FApplicationName: _string;
    FWorkstationID: _string;
    FPacketSize: integer;
    FInitialFileName: _string;
    FMultipleActiveResultSets: boolean;
    FFailoverPartner: _string;
    FTrustServerCertificate: boolean;

    procedure SetLanguage(const Value: _string);
    procedure SetPersistSecurityInfo(const Value: boolean);
    procedure SetAutoTranslate(const Value: boolean);
    procedure SetNetworkLibrary(const Value: _string);
    procedure SetApplicationName(const Value: _string);
    procedure SetWorkstationID(const Value: _string);
    procedure SetPacketSize(const Value: integer);
    procedure SetInitialFileName(const Value: _string);
    procedure SetMultipleActiveResultSets(const Value: boolean);
    procedure SetFailoverPartner(const Value: _string);
    procedure SetTrustServerCertificate(const Value: boolean);

    procedure AssignTo(Dest: TPersistent); override;

    procedure LoadCompactVersionProperty(Reader: TReader);
    procedure StoreCompactVersionProperty(Writer: TWriter);
    procedure LoadNativeClientVersionProperty(Reader: TReader);
    procedure StoreNativeClientVersionProperty(Writer: TWriter);
    procedure DefineProperties(Filer: TFiler); override;
  public
    constructor Create(Owner: TMSConnection);
  published
    property QuotedIdentifier;
    property Language: _string read FLanguage write SetLanguage;
    property NumericType;
    property Encrypt;
    property PersistSecurityInfo: boolean read FPersistSecurityInfo write SetPersistSecurityInfo default False;
    property AutoTranslate: boolean read FAutoTranslate write SetAutoTranslate default True;
    property NetworkLibrary: _string read FNetworkLibrary write SetNetworkLibrary;
    property ApplicationName: _string read FApplicationName write SetApplicationName;
    property WorkstationID: _string read FWorkstationID write SetWorkstationID;
    property PacketSize: integer read FPacketSize write SetPacketSize default 4096;
    property InitialFileName: _string read FInitialFileName write SetInitialFileName;
    property MultipleActiveResultSets: boolean read FMultipleActiveResultSets write SetMultipleActiveResultSets default False;
    property FailoverPartner: _string read FFailoverPartner write SetFailoverPartner;
    property TrustServerCertificate: boolean read FTrustServerCertificate write SetTrustServerCertificate default False;
    property DefaultLockTimeout;
    property Provider;
    property KeepDesignConnected;
    property DisconnectedMode;
    property LocalFailover;
    property DefaultSortType;
  {$IFDEF VER10P}
    property UseWideMemos;
  {$ENDIF}
  end;

{$IFDEF VER16P}
  [ComponentPlatformsAttribute(pidWin32 or pidWin64)]
{$ENDIF}
  TMSConnection = class(TCustomMSConnection)
  protected
    FAuthentication: TMSAuthentication;
    FConnectionTimeout: integer;
    FOnInfoMessage: TMSConnectionInfoMessageEvent;
    FIntegratedSecuritySSPI: boolean;
    FSPID: integer;

    procedure SetIConnection(Value: TCRConnection); override;

    procedure FillConnectionParameters(var ConnectionParameters: TMSConnectionParameters); override;
    procedure FillConnectionProps(OLEDBConnection: TOLEDBConnection); override;
    
    procedure AssignTo(Dest: TPersistent); override;

    function CreateOptions: TDAConnectionOptions; override;

    function GetOptions: TMSConnectionOptions;
    procedure SetOptions(Value: TMSConnectionOptions);

    procedure SetAuthentication(const Value: TMSAuthentication);
    procedure SetConnectionTimeout(const Value: integer);
    function NeedPrompt: boolean; override;

    function GetConnectString: _string; override;
    procedure ProcessConnectStringParam(const paramName: string; paramValue: _string); override;
    procedure InitConnectStringOptions; override;
    procedure SetConnectString(Value: _string); override;

    procedure DoInfoMessage(E: EMSError);

    function GetSPID: integer;
    property SPID: integer read GetSPID;
  public
    constructor Create(Owner: TComponent); override;

    procedure ChangePassword(NewPassword: _string);

  published
    property Database;
    property IsolationLevel;
    property Authentication: TMSAuthentication read FAuthentication write SetAuthentication default auServer;
    property ConnectionTimeout: integer read FConnectionTimeout write SetConnectionTimeout default 15;

    property Options: TMSConnectionOptions read GetOptions write SetOptions;

    property PoolingOptions;
    property Pooling;
    property Username;
    property Password;
    property Server;
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
    property OnInfoMessage: TMSConnectionInfoMessageEvent read FOnInfoMessage write FOnInfoMessage;
    property OnConnectionLost;
  end;

  TCustomMSTransaction = class(TDATransaction)
  protected
    function SQLMonitorClass: TClass; override;
    function GetITransactionClass: TCRTransactionClass; override;

    property IsolationLevel;
  end;

{$IFNDEF STD}
{ TMSChangeNotification }

  TMSNotificationType = (ntChange, ntSubscribe, ntUnknown);

  TMSNotificationInfo = (niAlter, niDelete, niDrop, niError, niInsert, niInvalid, niIsolation,
    niOptions, niPreviousFire, niQuery, niResource, niRestart, niTemplateLimit, niTruncate, niUnknown, niUpdate);

  TMSNotificationSource = (nsClient, nsData, nsDatabase, nsEnvironment, nsExecution,
    nsObject, nsStatement, nsSystem, nsTimeout, nsUnknown);

  TMSChangeNotificationEvent = procedure(Sender: TObject; DataSet: TCustomMSDataSet; NotificationInfo: TMSNotificationInfo;
    NotificationSource: TMSNotificationSource; NotificationType: TMSNotificationType) of object;

{$IFDEF VER16P}
  [ComponentPlatformsAttribute(pidWin32 or pidWin64)]
{$ENDIF}
  TMSChangeNotification = class(TComponent)
  private
    FEnabled: boolean;
    FTimeOut: integer;
    FService: _string;
    FOnChange: TMSChangeNotificationEvent;
    FDataSets: TDAList;
    FNotificators: TDAList;

    procedure SetEnabled(Value: boolean);
    procedure SetTimeOut(Value: integer);
    procedure SetService(Value: _string);

    procedure DoOnNotification(Sender: TObject);

    function GetSubscriptionKey(DataSet: TCustomMSDataSet): string;
  protected
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;

    procedure AddDataSet(DataSet: TCustomMSDataSet);
    procedure RemoveDataSet(DataSet: TCustomMSDataSet);
    procedure CheckDataSet(DataSet: TCustomMSDataSet);

    function FindNotificator(DataSet: TCustomMSDataSet): TObject;
    procedure AddNotificator(DataSet: TCustomMSDataSet);
    procedure RemoveNotificator(DataSet: TCustomMSDataSet);

    procedure StartNotification(DataSet: TCustomMSDataSet);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  published
    property Enabled: boolean read FEnabled write SetEnabled default True;
    property TimeOut: integer read FTimeOut write SetTimeOut default 432000;
    property Service: _string read FService write SetService;
    property OnChange: TMSChangeNotificationEvent read FOnChange write FOnChange;
  end;
{$ENDIF}

{ TCustomMSDataSet }

  TMSDataSetOptions = class (TDADataSetOptions)
  private
    FUniqueRecords: boolean;
    FCursorUpdate: boolean;
    FQueryIdentity: boolean;
    FCheckRowVersion: boolean;
    FAutoRefresh: boolean;
    FAutoRefreshInterval: integer;
    FNonBlocking: boolean;
    FReflectChangeNotify: boolean;
    FDisableMultipleResults: boolean;
    FDescribeParams: boolean;

    procedure SetUniqueRecords(Value: boolean);
    procedure SetCursorUpdate(Value: boolean);
    function GetAllFieldsEditable: boolean;
    procedure SetAllFieldsEditable(const Value: boolean);
    procedure SetAutoRefresh(Value: boolean);
    procedure SetAutoRefreshInterval(Value: integer);
    procedure SetNonBlocking(Value: boolean);
    procedure SetReflectChangeNotify(const Value: boolean);
    procedure SetQueryIdentity(const Value: boolean);
    procedure SetCheckRowVersion(const Value: boolean);
    function GetDMLRefresh: boolean;
    procedure SetDMLRefresh(const Value: boolean);
    procedure SetDisableMultipleResults(Value: boolean);
    procedure SetDescribeParams(Value: boolean);
  protected
    procedure AssignTo(Dest: TPersistent); override;

  public
    constructor Create(Owner: TCustomDADataSet);

    property AllFieldsEditable: boolean read GetAllFieldsEditable write SetAllFieldsEditable;
  published
    property UniqueRecords: boolean read FUniqueRecords write SetUniqueRecords default False;
    property CursorUpdate: boolean read FCursorUpdate write SetCursorUpdate default True;
    property QueryIdentity: boolean read FQueryIdentity write SetQueryIdentity default True;
    property CheckRowVersion: boolean read FCheckRowVersion write SetCheckRowVersion default False;
    property FullRefresh default False;
    property DMLRefresh: boolean read GetDMLRefresh write SetDMLRefresh default False;
    property AutoRefresh: boolean read FAutoRefresh write SetAutoRefresh default False;
    property AutoRefreshInterval: integer read FAutoRefreshInterval write SetAutoRefreshInterval default 60; /// Seconds dac6.txt
    property NonBlocking: boolean read FNonBlocking write SetNonBlocking default False;
    property ReflectChangeNotify: boolean read FReflectChangeNotify write SetReflectChangeNotify default False;
    property DisableMultipleResults: boolean read FDisableMultipleResults write SetDisableMultipleResults default False;
    property DescribeParams: boolean read FDescribeParams write SetDescribeParams default False;

    property SetFieldsReadOnly;
    property LongStrings;
    property RequiredFields default False;
    property StrictUpdate;
    property NumberRange;
    property ReturnParams;
    property TrimFixedChar;
    property TrimVarChar;
    property SetEmptyStrToNull;
    property QueryRecCount;
    property AutoPrepare;
    property RemoveOnRefresh;
    property FlatBuffers;
    property QuoteNames;
    property DetailDelay;
  {$IFDEF HAVE_COMPRESS}
    property CompressBlobMode;
  {$ENDIF}
    property FieldsOrigin;
    property LocalMasterDetail;
    property CacheCalcFields;
    property DefaultValues;
    property UpdateBatchSize;
    property UpdateAllFields;
    property EnableBCD;
  end;

  TMSDataTypesMap = class(TCustomMSDataTypesMap)
  public
    class function GetFieldType(DataType: Word): TFieldType; override;
    class function GetDataType(FieldType: TFieldType): integer; override;
  end;

  TMSSQLGenerator = class(TCustomMSSQLGenerator)
  end;

  TMSDataSetUpdater = class(TCustomMSDataSetUpdater)
  private
    FDataSetService: TMSDataSetService;
  protected
    procedure CheckUpdateQuery(const StatementType: TStatementType); override;
    procedure SetUpdateQueryOptions(const StatementType: TStatementType); override;

  public
    constructor Create(AOwner: TDataSetService); override;
  end;

  TMSDataSetService = class(TCustomMSDataSetService)
  protected
    function GetOLEDBProvider: TOLEDBProvider;

    procedure CreateSQLGenerator; override;
    procedure CreateDataSetUpdater; override;

    function DetectCanModify: boolean; override;
    procedure SetFieldOrigin(Field: TField; FieldDesc: TCRFieldDesc); override;
    procedure InitCursor; override;
  end;

  TMSLockType = (ltUpdate, ltExclusive); // Note: MSServices.TMSLockTypeI
  TMSLockObject = (loRow, loTable);      // Note: MSServices.TMSLockObjectI
  
  TMSUpdateExecuteEvent = procedure (Sender: TCustomMSDataSet;
    StatementTypes: TStatementTypes; Params: TMSParams) of object;

  TMSUpdateSQL = class;

  TCustomMSDataSet = class (TCustomDADataSet)
  private
    FFileStreamList: TDAList;

    function GetConnection: TCustomMSConnection;
    procedure SetConnection(Value: TCustomMSConnection);
    procedure SetCursorType(const Value: TMSCursorType);
    procedure SetCommandTimeout(const Value: integer);

    function GetUpdateObject: TMSUpdateSQL;
    procedure SetUpdateObject(Value: TMSUpdateSQL);
    procedure ChecktAutoRefreshTimer(Enabled: boolean);

    procedure ClearFileStreams;

  protected
  {$IFDEF WITH_IPROVIDER}
  { IProviderSupport }
    function PSGetKeyFields: string; override;
  {$ENDIF}

  protected
    FBulkExecuting: boolean;
    FIRecordSet: TOLEDBRecordSet;
    FICommand: TOLEDBCommand;

    FDataSetService: TMSDataSetService;
    
    FBeforeUpdateExecute: TMSUpdateExecuteEvent;
    FAfterUpdateExecute: TMSUpdateExecuteEvent;

    FOptions: TMSDataSetOptions;
    FCursorType: TMSCursorType;

    FCommandTimeout: integer;

    FUseParamType: boolean;

    FAutoRefreshTimer: TWin32Timer;
    
  {$IFNDEF STD}
    FChangeNotification: TMSChangeNotification;
    FNeedReflectChanges: boolean;
  {$ENDIF}

    function ServerCursorUsed: boolean;
    procedure AutoRefreshTimer(Sender: TObject);
    
    function GetParams: TMSParams;
    procedure SetParams(Value: TMSParams);

    procedure CreateIRecordSet; override;
    procedure SetIRecordSet(Value: TData); override;

    function GetDataSetServiceClass: TDataSetServiceClass; override;
    procedure SetDataSetService(Value: TDataSetService); override;

    procedure CreateCommand; override;

    function CreateOptions: TDADataSetOptions; override;
    procedure SetOptions(Value: TMSDataSetOptions);

    procedure AssignTo(Dest: TPersistent); override;

    procedure BeginConnection(NoConnectCheck: boolean = True); override;
    procedure EndConnection; override;

  {$IFNDEF STD}
    procedure SetChangeNotification(Value: TMSChangeNotification);
    procedure ReflectChanges(NotificationInfo: TMSNotificationInfo;
      NotificationSource: TMSNotificationSource; NotificationType: TMSNotificationType);
  {$ENDIF}
    
  { Open/Close }
    procedure SetActive(Value: Boolean); override;
    procedure DataReopen; override;
    procedure InternalRefresh; override;
    procedure InternalRefreshQuick(const CheckDeleted: boolean); override;
    procedure InternalExecute; override;
    procedure InternalOpen; override;
    procedure OpenCursor(InfoQuery: boolean); override;
    procedure CloseCursor; override;

    procedure DoCursorTypeChanged; // Callback from internal level. Called if CursorType or ReadOnly is changed on OLEDB execute
    function AllowedCursor(const Value: TMSCursorType): boolean; virtual;

    procedure CheckInactive; override;

  { Fields }
    function GetFielDefSize(FieldType: TFieldType; FieldDesc: TFieldDesc): integer; override;
    function GetFieldType(FieldDesc: TFieldDesc): TFieldType; override;
    function GetFieldClass(FieldType: TFieldType): TFieldClass; override;
  {$IFDEF USE_FTAUTOINC}
    function GetFieldType(FieldDesc: TFieldDesc): TFieldType; override;
  {$ENDIF}

  { Edit }
    procedure SetReadOnly(Value: boolean); override;
    function PerformLockSQL(SQL: _string; StatementTypes: TStatementTypes): boolean;

    function GetRecord(Buffer: TRecordBuffer; GetMode: TGetMode; DoCheck: boolean): TGetResult; override;
   
  { Navigation }
    procedure InternalSetToRecord(Buffer: TRecordBuffer); override;

    procedure DoBeforeExecute; override;
    procedure DoAfterExecute(Result: boolean); override;

  { Before / After UpdateExecute }
    function AssignedBeforeUpdateExecute: boolean; override;
    procedure DoBeforeUpdateExecute(Sender: TDataSet; StatementTypes: TStatementTypes;
      Params: TDAParams); override;
    function AssignedAfterUpdateExecute: boolean; override;
    procedure DoAfterUpdateExecute(Sender: TDataSet; StatementTypes: TStatementTypes;
      Params: TDAParams); override;

  { SQL Modifications }
    function SQLGetFrom(SQLText: _string): _string; override;
    function SQLAddWhere(SQLText, Condition: _string): _string; override;
    function SQLDeleteWhere(SQLText: _string): _string; override;
    function SQLGetWhere(SQLText: _string): _string; override;
    function SQLSetOrderBy(SQLText: _string; Fields: _string): _string; override;
    function SQLGetOrderBy(SQLText: _string): _string; override;

    procedure DoBeforeOpenCursor;

    property DMLRefresh; //CLR cross-assembly
  public
    constructor Create(Owner: TComponent); override;
    destructor Destroy; override;

  { Open/Close }
    function Fetched: boolean; override;
    function OpenNext: boolean; // Open next rowset in statement. if rowset not returne theh OpenNext return False. If statement has error, then raised exception
    function HasNextResultSet: boolean;
    procedure BreakExec; override;
    procedure RefreshQuick(const CheckDeleted: boolean);

    procedure LockTable(LockType: TMSLockType);
    procedure Lock; overload; override;
    procedure Lock(LockType: TMSLockType); reintroduce; overload;

    function FindParam(const Value: _string): TMSParam;
    function ParamByName(const Value: _string): TMSParam;

    function GetFileStreamForField(const FieldName: _string;
      const DesiredAccess: TMSSqlFilestreamDesiredAccess = daReadWrite;
      const OpenOptions: TMSSqlFilestreamOpenOptions = [];
      const AllocationSize: Int64 = 0): TMSFileStream;

  { Edit}
    procedure CreateProcCall(Name: _string);

    procedure Post; override;
    procedure Cancel; override;

  { SQL Modify }
    property Connection: TCustomMSConnection read GetConnection write SetConnection;

    property Options: TMSDataSetOptions read FOptions write SetOptions;

  { Edit }
    property Params: TMSParams read GetParams write SetParams stored False;

    property CursorType: TMSCursorType read FCursorType write SetCursorType default ctDefaultResultSet;
    property CommandTimeout: integer read FCommandTimeout write SetCommandTimeout default 0;

    property BeforeUpdateExecute: TMSUpdateExecuteEvent read FBeforeUpdateExecute write FBeforeUpdateExecute;
    property AfterUpdateExecute: TMSUpdateExecuteEvent read FAfterUpdateExecute write FAfterUpdateExecute;
    property BeforeFetch;
    property AfterFetch;

    property FetchAll default True;

    property UpdateObject: TMSUpdateSQL read GetUpdateObject write SetUpdateObject;

  {$IFNDEF STD}
    property ChangeNotification: TMSChangeNotification read FChangeNotification write SetChangeNotification;
  {$ENDIF}
  end;

{ TMSUpdateSQL }

{$IFDEF VER16P}
  [ComponentPlatformsAttribute(pidWin32 or pidWin64)]
{$ENDIF}
  TMSUpdateSQL = class (TCustomDAUpdateSQL)
  protected
    function DataSetClass: TCustomDADataSetClass; override;
    function SQLClass: TCustomDASQLClass; override;
  end;

{ TMSQuery }

{$IFDEF VER16P}
  [ComponentPlatformsAttribute(pidWin32 or pidWin64)]
{$ENDIF}
  TMSQuery = class (TCustomMSDataSet)
  protected
    procedure SetIRecordSet(Value: TData); override;

  published
    property SQLInsert;
    property SQLDelete;
    property SQLUpdate;
    property SQLRefresh;
    property SQLLock;

    property Connection;
    property ParamCheck;
    property SQL;
    property Debug;
    property Macros;
    property Params;
    property FetchRows;
    property ReadOnly;
    property UniDirectional;
    property CachedUpdates;

    property BeforeExecute;
    property AfterExecute;
    property BeforeUpdateExecute;
    property AfterUpdateExecute;
    property OnUpdateError;
    property OnUpdateRecord;

    property UpdateObject;
    property RefreshOptions;

    property AutoCalcFields;
    property Filtered;
    property Filter;
    property FilterOptions;

    property BeforeOpen;
    property AfterOpen;
    property BeforeClose;
    property AfterClose;
    property BeforeInsert;
    property AfterInsert;
    property BeforeEdit;
    property AfterEdit;
    property BeforePost;
    property AfterPost;
    property BeforeCancel;
    property AfterCancel;
    property BeforeDelete;
    property AfterDelete;
    property BeforeScroll;
    property AfterScroll;
    property OnCalcFields;
    property OnDeleteError;
    property OnEditError;
    property OnFilterRecord;
    property OnNewRecord;
    property OnPostError;
  {$IFNDEF VER4}
    property AfterRefresh;
    property BeforeRefresh;
  {$ENDIF}

    property Options;
    property FilterSQL;
  {$IFNDEF STD}
    property ChangeNotification;
  {$ENDIF}

    property MasterSource;
    property MasterFields;
    property DetailFields;

    property UpdatingTable;

    property FetchAll;
    property CursorType;
    property CommandTimeout;

    property IndexFieldNames;
    property Active; /// CR DAC 13049
    property BeforeFetch;
    property AfterFetch;  
    property KeyFields;
    property LockMode;
  end;

{ TMSTable }

  TCustomMSTable = class (TCustomMSDataSet)
  protected
  {$IFDEF WITH_IPROVIDER}
  { IProviderSupport }
    function PSGetTableName: string; override;
    procedure PSSetParams(AParams: DB.TParams); override;
  {$IFDEF VER5P}
    procedure PSSetCommandText(const CommandText: _string); override;
  {$ENDIF}
  {$ENDIF}

  protected
    FTableName: _string;
    FOrderFields: _string;

    procedure SetTableName(const Value: _string);

    procedure SetOrderFields(Value: _string);
    procedure AssignTo(Dest: TPersistent); override;
    function SQLAutoGenerated: boolean; override;

  { Open/Close }
    procedure OpenCursor(InfoQuery: boolean); override;
    function AllowedCursor(const Value: TMSCursorType): boolean; override;
    procedure SetIRecordSet(Value: TData); override;

  { SQL Modifications }
    procedure CheckSQL; override;

  public
  { Open/Close }
    procedure Prepare; override;
    procedure PrepareSQL;
    procedure Execute; override;

    property TableName: _string read FTableName write SetTableName;
    property OrderFields: _string read FOrderFields write SetOrderFields;
  end;

{$IFDEF VER16P}
  [ComponentPlatformsAttribute(pidWin32 or pidWin64)]
{$ENDIF}
  TMSTable = class (TCustomMSTable)
  published
    property TableName;
    property OrderFields;
    property MasterFields;
    property DetailFields;
    property MasterSource;
    property ReadOnly;

    property Connection;

    property Debug;
    property FetchRows;
    property UniDirectional;
    property CachedUpdates;

    property OnUpdateError;
    property OnUpdateRecord;

    property UpdateObject;
    property RefreshOptions;

    property Active;
    property AutoCalcFields;
    property Filtered;
    property Filter;
    property FilterOptions;

    property BeforeOpen;
    property AfterOpen;
    property BeforeClose;
    property AfterClose;
    property BeforeInsert;
    property AfterInsert;
    property BeforeEdit;
    property AfterEdit;
    property BeforePost;
    property AfterPost;
    property BeforeCancel;
    property AfterCancel;
    property BeforeDelete;
    property AfterDelete;
    property BeforeScroll;
    property AfterScroll;
    property OnCalcFields;
    property OnDeleteError;
    property OnEditError;
    property OnFilterRecord;
    property OnNewRecord;
    property OnPostError;
  {$IFNDEF VER4}
    property AfterRefresh;
    property BeforeRefresh;
  {$ENDIF}
    property Options;
    property FilterSQL;

    property FetchAll;
    property CursorType;
    property CommandTimeout;

    property IndexFieldNames;
    
    property BeforeFetch;
    property AfterFetch;
    property KeyFields;
    property LockMode;
  end;

{$IFDEF VER16P}
  [ComponentPlatformsAttribute(pidWin32 or pidWin64)]
{$ENDIF}
  TMSTableData = class (TMemDataSet)
  private
    FConnection: TCustomMSConnection;
    FTableTypeName: _string;
    FTableObject: TMSTableObject;

    procedure SetConnection(Value: TCustomMSConnection);
    procedure SetTableTypeName(const Value: _string);
    function GetTable: TMSTableObject;
  protected
    FIRecordSet: TOLEDBTableTypeRecordSet;
    FDesignCreate: boolean;
    FStreamedOpen: Boolean;

    function GetFieldObject(FieldDesc: TFieldDesc): TSharedObject; overload;
    procedure CreateIRecordSet; override;
    procedure SetIRecordSet(Value: TData); override;
    procedure Loaded; override;

    function UsedConnection: TCustomMSConnection;
    procedure BeginConnection;
    procedure EndConnection;
    procedure Disconnect(NeedClose: boolean = True);
    procedure ConnectChange(Sender: TObject; Connecting: boolean);

  { Open/Close }
    procedure SetActive(Value: Boolean); override;
    procedure OpenCursor(InfoQuery: boolean); override;
    procedure CloseCursor; override;

    procedure DataReopen; override;
    procedure InternalRefresh; override;
    procedure InternalOpen; override;
    procedure InternalClose; override;
    function GetDataTypesMap: TDataTypesMapClass; override;

    procedure AssignTo(Dest: TPersistent); override;

  public
    constructor Create(Owner: TComponent); override;
    destructor Destroy; override;

    procedure Prepare; override;
    procedure Resync(Mode: TResyncMode); override;

    function CreateBlobStream(Field: TField; Mode: TBlobStreamMode): TStream; override;

  { Additional data types }
    function GetField(FieldDesc: TFieldDesc): TField;
    function GetDataType(const FieldName: _string): integer;
    function GetFieldDesc(const FieldName: _string): TFieldDesc; overload;
    function GetFieldDesc(const FieldNo: integer): TFieldDesc; overload;
    function GetFieldPrecision(const FieldName: _string): integer;
    function GetFieldScale(const FieldName: _string): integer;
    function GetFieldObject(const FieldName: _string): TSharedObject; overload;

    property Table: TMSTableObject read GetTable;
  published
    property Connection: TCustomMSConnection read FConnection write SetConnection;
    property TableTypeName: _string read FTableTypeName write SetTableTypeName;

    property AutoCalcFields;
    property Filtered;
    property Filter;
    property FilterOptions;
    property IndexFieldNames;
    property Active;

    property BeforeOpen;
    property AfterOpen;
    property BeforeClose;
    property AfterClose;
    property BeforeInsert;
    property AfterInsert;
    property BeforeEdit;
    property AfterEdit;
    property BeforePost;
    property AfterPost;
    property BeforeCancel;
    property AfterCancel;
    property BeforeDelete;
    property AfterDelete;
    property BeforeScroll;
    property AfterScroll;
    property OnCalcFields;
    property OnDeleteError;
    property OnEditError;
    property OnFilterRecord;
    property OnNewRecord;
    property OnPostError;
    property AfterRefresh;
    property BeforeRefresh;
  end;

{ TMSStoredProc }

  TCustomMSStoredProc = class (TCustomMSDataSet)
  protected
  {$IFDEF WITH_IPROVIDER}
  { IProviderSupport }
  {$IFDEF VER5P}
    procedure PSSetCommandText(const CommandText: string); override;
  {$ENDIF}
  {$ENDIF}

  protected
    FStoredProcName: _string;

    procedure SetIRecordSet(Value: TData); override;
    procedure SetStoredProcName(const Value: _string);
    procedure AssignTo(Dest: TPersistent); override;

    function SQLAutoGenerated: boolean; override;
    procedure BeforeOpenCursor(InfoQuery: boolean); override;
    procedure DoBeforeExecute; override;

  public
    procedure ExecProc; // for BDE compatibility
    
    procedure Prepare; override;
    procedure PrepareSQL; 

    property UpdatingTable;
    property StoredProcName: _string read FStoredProcName write SetStoredProcName;
  end;

{$IFDEF VER16P}
  [ComponentPlatformsAttribute(pidWin32 or pidWin64)]
{$ENDIF}
  TMSStoredProc = class(TCustomMSStoredProc)
  published
    property StoredProcName;

    property SQLInsert;
    property SQLDelete;
    property SQLUpdate;
    property SQLRefresh;
    property SQLLock;

    property Connection;
    property ParamCheck stored False;
    property SQL;
    property Debug;
    property Params;
    property FetchRows;
    property ReadOnly;
    property UniDirectional;
    property CachedUpdates;

    property BeforeExecute;
    property AfterExecute;
    property BeforeUpdateExecute;
    property AfterUpdateExecute;
    property OnUpdateError;
    property OnUpdateRecord;

    property Options;
    property UpdateObject;
    property RefreshOptions;
  {$IFNDEF STD}
    property ChangeNotification;
  {$ENDIF}

    property Active;
    property AutoCalcFields;
    property Filtered;
    property Filter;
    property FilterOptions;
    property BeforeOpen;
    property AfterOpen;
    property BeforeClose;
    property AfterClose;
    property BeforeInsert;
    property AfterInsert;
    property BeforeEdit;
    property AfterEdit;
    property BeforePost;
    property AfterPost;
    property BeforeCancel;
    property AfterCancel;
    property BeforeDelete;
    property AfterDelete;
    property BeforeScroll;
    property AfterScroll;
    property OnCalcFields;
    property OnDeleteError;
    property OnEditError;
    property OnFilterRecord;
    property OnNewRecord;
    property OnPostError;
  {$IFNDEF VER4}
    property AfterRefresh;
    property BeforeRefresh;
  {$ENDIF}

    property UpdatingTable;

    property FetchAll;
    property CursorType;
    property CommandTimeout;
    property KeyFields;
    property LockMode;
  end;

{ TMSSQL }

{$IFDEF VER16P}
  [ComponentPlatformsAttribute(pidWin32 or pidWin64)]
{$ENDIF}
  TMSSQL = class (TCustomDASQL)
  private
    FDescribeParams: boolean;
    function GetConnection: TCustomMSConnection;
    procedure SetConnection(Value: TCustomMSConnection);

    function GetParams: TMSParams;
    procedure SetParams(Value: TMSParams);
    procedure SetDescribeParams(Value: boolean);

  protected
    FICommand: TOLEDBCommand;
    FCommandTimeout: integer;
    FNonBlocking: boolean;

    function GetPermitPrepare: boolean;
    procedure SetPermitPrepare(Value: boolean);

    procedure CreateICommand; override;
    procedure SetICommand(Value: TCRCommand); override;
    procedure InternalPrepare; override;
    procedure InternalExecute(Iters: integer); override;
    procedure AssignTo(Dest: TPersistent); override;

    function GetDataTypesMap: TDataTypesMapClass; override;
    function CreateParamsObject: TDAParams; override;
    procedure AssignParamDesc(Param: TDAParam; ParamDesc: TParamDesc); override;

    function FindResultParam: TDAParam; override;
    
    procedure SetCommandTimeout(const Value: integer);
    procedure SetNonBlocking(const Value: boolean);

    function UsedConnection: TCustomDAConnection; override;
  public
    constructor Create(Owner: TComponent); override;
    procedure Execute(Iters: integer); override;
    procedure BreakExec;
    procedure UnPrepare; override;
    procedure ExecuteForXML(out XML: String); overload;

  {$IFNDEF CLR}
  {$IFDEF VER6P}
    procedure ExecuteForXML(out XML: WideString); overload;
  {$ENDIF}
  {$ENDIF}
    procedure ExecuteForXML(Stream: TStream; OutputEncoding: TOLEDBOutputEncoding); overload;

    procedure CreateProcCall(Name: _string);
    function FindParam(const Value: _string): TMSParam;
    function ParamByName(const Value: _string): TMSParam;

  published
    property Connection: TCustomMSConnection read GetConnection write SetConnection;
    property Params: TMSParams read GetParams write SetParams stored False;

    property DescribeParams: boolean read FDescribeParams write SetDescribeParams default False;
    property ParamCheck;
    property SQL;
    property Macros;
    property Debug;

    property BeforeExecute;
    property AfterExecute;
    property CommandTimeout: integer read FCommandTimeout write SetCommandTimeout default 0;
    property NonBlocking: boolean read FNonBlocking write SetNonBlocking default False;

    property PermitPrepare: boolean write SetPermitPrepare stored False;
  end;

{ TMSMetadata }

  TMSObjectType = (otDatabases,
    otColumnPrivileges, otColumns,
    otForeignKeys, otPrimaryKeys,
    otIndexes,
    otServerTypes,
    otSchemata, otStatistics,
    otStoredProcs, otStoredProcParams,
    otAliases, otTables, otSynonyms, otSystemTables, otViews, otGlobalTempTables, otLocalTempTables, otSystemViews,
    otAliasesInfo, otTablesInfo, otSynonymsInfo, otSystemTablesInfo, otViewsInfo, otGlobalTempTablesInfo, otLocalTempTablesInfo, otExternalTablesInfo, otSystemViewsInfo,
    otTableConstraints, otTablePrivileges,
    otLinkedServers,
    otAssemblies,
    otAssemblyDependencies,
    otUserTypes,
    otXMLCollections,
    otCheckConstraints,
    otCheckConstraintsByTable,
    otTableStatistics,
    otConstraintColumnUsage,
    otTableTypes, otTableTypePrimaryKeys, otTableTypeColumns);

{$IFDEF VER16P}
  [ComponentPlatformsAttribute(pidWin32 or pidWin64)]
{$ENDIF}
  TMSMetadata = class (TCustomMSDataSet)
  private
    FIMetaData: TOLEDBMetaData;
    FRestrictions: _TStrings;
    FNativeRecordset: boolean;
    procedure RequestRecordSet;
  protected
    FObjectType: TMSObjectType;

    FDatabaseName: _string;
    FSchemaName: _string;
    FTableName: _string;
    FStoredProcName: _string;
    FColumnName: _string;
    FIndexName: _string;
    FConstraintName: _string;
    FLinkedServer: _string;
    FAssemblyName: _string;
    FAssemblyID: integer;
    FReferencedAssemblyID: integer;
    FUDTName: _string;
    FSchemaCollectionName: _string;
    FTargetNamespaceURI: _string;
    FStatisticsName: _string;
    FTableTypeName: _string;

    procedure SetDatabaseName(Value: _string);
    procedure SetSchemaName(Value: _string);
    procedure SetObjectType(Value: TMSObjectType);
    procedure SetTableName(Value: _string);
    procedure SetStoredProcName(Value: _string);
    procedure SetColumnName(Value: _string);
    procedure SetIndexName(Value: _string);
    procedure SetConstraintName(Value: _string);
    procedure SetLinkedServer(Value: _string);
    procedure SetAssemblyName(Value: _string);
    procedure SetAssemblyID(Value: integer);
    procedure SetReferencedAssemblyID(Value: integer);
    procedure SetUDTName(Value: _string);
    procedure SetSchemaCollectionName(Value: _string);
    procedure SetTargetNamespaceURI(Value: _string);
    procedure SetStatisticsName(Value: _string);
    procedure SetTableTypeName(Value: _string);

    function RequestIRowset: IRowset;
    procedure InternalExecute; override;
    procedure OpenCursor(InfoQuery: boolean); override;
    procedure CloseCursor; override;

  public
    constructor Create(Owner: TComponent); override;
    destructor Destroy; override;

  published
    property ObjectType: TMSObjectType read FObjectType write SetObjectType default otDatabases;

    property DatabaseName: _string read FDatabaseName write SetDatabaseName;
    property SchemaName: _string read FSchemaName write SetSchemaName;
    property TableName: _string read FTableName write SetTableName;
    property StoredProcName: _string read FStoredProcName write SetStoredProcName;
    property ColumnName: _string read FColumnName write SetColumnName;
    property IndexName: _string read FIndexName write SetIndexName;
    property ConstraintName: _string read FConstraintName write SetConstraintName;

    property LinkedServer: _string read FLinkedServer write SetLinkedServer;
    property AssemblyName: _string read FAssemblyName write SetAssemblyName;
    property AssemblyID: integer read FAssemblyID write SetAssemblyID default 0;
    property ReferencedAssemblyID: integer read FReferencedAssemblyID write SetReferencedAssemblyID default 0;
    property UDTName: _string read FUDTName write SetUDTName;
    property SchemaCollectionName: _string read FSchemaCollectionName write SetSchemaCollectionName;
    property TargetNamespaceURI: _string read FTargetNamespaceURI write SetTargetNamespaceURI;
    property StatisticsName: _string read FStatisticsName write SetStatisticsName;
    property TableTypeName: _string read FTableTypeName write SetTableTypeName;

    property Active;
    property Connection;
  end;

{ TMSXMLField }

  TMSSchemaCollection = record
    Name: _string;
    CatalogName: _string;
    SchemaName: _string;
  end;

  TMSXMLField = class({$IFDEF VER10P}TWideMemoField{$ELSE}TMemoField{$ENDIF})
  private
    function GetBlobType: TBlobType;
    procedure SetBlobType(Value: TBlobType);
  protected
    FTyped: boolean;
    FSchemaCollection: TMSSchemaCollection;
    procedure GetText(var Text: string; DisplayText: Boolean); override;
    procedure SetSchemaCollection(Name, CatalogName, SchemaName: _string);

  public
    constructor Create(AOwner: TComponent); override;
    property XML: string read GetAsString write SetAsString;
    property Typed: boolean read FTyped;
    property SchemaCollection: TMSSchemaCollection read FSchemaCollection;
  published
    property BlobType: TBlobType read GetBlobType write SetBlobType;
  end;

{ TMSUDTField }

  TMSUDTField = class(TBlobField)
  private
    FUDTSchemaname: WideString;
    FUDTName: WideString;
    FUDTCatalogname: WideString;
    FAssemblyTypename: WideString;
  {$IFNDEF CLR}
    FUDTDispatcher: TUDTDispatcher;
  {$ENDIF}
  protected
  {$IFNDEF CLR}
    function GetClassDesc: string; override;
    function GetAsUDT: Variant;
    procedure RetrieveUDTData;
    procedure AfterInvoke(Sender: TObject);
  {$ENDIF}
    procedure SetUDTTypeInfo(AUDTSchemaname, AUDTName, AUDTCatalogname,
      AAssemblyTypename: _string{$IFNDEF CLR}; AUDTDispatcher: TUDTDispatcher{$ENDIF});
    procedure SetFieldDataType(Value: TFieldType);
  public
    constructor Create(AOwner: TComponent); override;
  {$IFNDEF CLR}
    property AsUDT: Variant read GetAsUDT;
  {$ENDIF}
    property UDTSchemaname: WideString read FUDTSchemaname;
    property UDTName: WideString read FUDTName;
    property UDTCatalogname: WideString read FUDTCatalogname;
    property AssemblyTypename: WideString read FAssemblyTypename;
  end;

{ TMSFileStream }

  TMSFileStream = class(TStream)
  protected
    FOwner: TDAList;
    FHandle: Integer;
  {$IFNDEF CLR}
    procedure SetSize(NewSize: Longint); overload; override;
  {$ENDIF}
  {$IFDEF VER6P}
    procedure SetSize({$IFNDEF CLR}const{$ENDIF} NewSize: Int64); overload; override;
  {$ENDIF}
  public
    constructor Create(AHandle: Integer);
    destructor Destroy; override;
  {$IFDEF CLR}
    function Read(var Buffer: array of Byte; Offset, Count: Longint): Longint; override;
    function Write(const Buffer: array of Byte; Offset, Count: Longint): Longint; override;
  {$ELSE}
    function Read(var Buffer; Count: Longint): Longint; override;
    function Write(const Buffer; Count: Longint): Longint; override;
  {$ENDIF}
  {$IFDEF VER6P}
    function Seek(const Offset: Int64; Origin: TSeekOrigin): Int64; override;
  {$ELSE}
    function Seek(Offset: Longint; Origin: Word): Longint; override;
  {$ENDIF}
    procedure Flush;
    procedure Close;
    property Handle: Integer read FHandle;
  end;

{ TMSDataSource }

{$IFDEF VER16P}
  [ComponentPlatformsAttribute(pidWin32 or pidWin64)]
{$ENDIF}
  TMSDataSource = class(TCRDataSource)
  end;

  TMSAccessUtils = class
  public
    class procedure SetDesigning(Obj: TCustomMSDataSet; Value: Boolean; SetChildren: Boolean = True);

    class function FIConnection(Obj: TCustomMSConnection): TOLEDBConnection;
    class function FIRecordSet(Obj: TCustomMSDataSet): TOLEDBRecordSet;
    class function FICommand(Obj: TCustomMSDataSet): TOLEDBCommand;

    class function GetOLEDBSQL(Obj: TCustomMSDataSet): _string; overload;
    class function GetOLEDBSQL(Obj: TMSSQL): _string; overload;

    class function FIDBCreateSession(Obj: TOLEDBConnection): IDBCreateSession;

    class procedure DoError(Obj: TCustomMSConnection; E: Exception; var Fail: boolean);

    class function UsedConnection(Obj: TMSTableData): TCustomMSConnection;
  end;

  procedure GetServerList(List: _TStrings);
  procedure GetDatabasesList(const Connection: TCustomMSConnection; List: _TStrings);
  procedure GetTablesList(const Connection: TCustomMSConnection; List: _TStrings);

  function IsLargeDataTypeUsed(const Field: TField): boolean; overload;

(*
  function GetFieldType(DataType: word): TFieldType;
  function GetDataType(FieldType: TFieldType): word;
*)

var
  DefConnectDialogClassProc: function: TClass = nil;
  CurrentProjectOutputDir: string;
  __UseUpdateOptimization: boolean;


implementation

uses
{$IFDEF CLR}
  System.Runtime.InteropServices, System.Reflection, Contnrs,
{$ENDIF}
{$IFDEF VER7P}
  StrUtils,
{$ENDIF}
{$IFDEF VER6P}
{$IFNDEF CLR}
  Variants, VarUtils,
{$ENDIF}
{$ENDIF}
{$IFNDEF FPC}
  DBCommon, DBConsts,
{$ENDIF}
  MSParser, Registry, ActiveX, MSSQLMonitor, Math,
{$IFNDEF STD}
  MSServiceBroker,
{$ENDIF}
  Messages, TypInfo, ComObj, CRFunctions;


{$IFNDEF STD}
const
  sNotificationService = 'SDAC_NS_';
  sNotificationContract = 'http://schemas.microsoft.com/SQL/Notifications/PostQueryNotification';

type
  TMSNotificationParser = class
  private
    FNotificationInfo: TMSNotificationInfo;
    FNotificationSource: TMSNotificationSource;
    FNotificationType: TMSNotificationType;
    FKey: string;
  public
    constructor Create;

    procedure ProcessMessage(Message: {$IFDEF CLR}TBytes{$ELSE}WideString{$ENDIF});
    property NotificationInfo: TMSNotificationInfo read FNotificationInfo;
    property NotificationSource: TMSNotificationSource read FNotificationSource;
    property NotificationType: TMSNotificationType read FNotificationType;
    property Key: string read FKey;
  end;
{$ENDIF}

{$IFNDEF VER6P}
const
  varShortInt = $0010; { vt_i1          }
  varWord     = $0012; { vt_ui2         }
  varLongWord = $0013; { vt_ui4         }
  varInt64    = $0014; { vt_i8          }
{$ENDIF}

function IsLargeDataTypeUsed(const Field: TField): boolean; overload;
begin
  Result :=
    (Field is TBlobField) or
    ((Field is TMemoField) and (TMemoField(Field).BlobSize > MaxNonBlobFieldLen));
end;

function IsLargeDataTypeUsed(const Param: TParam): boolean; overload;
begin
  Result :=
       (Param.DataType = ftBlob)
    or (Param.DataType = ftMemo)
  {$IFDEF VER10P}
    or (Param.DataType = ftWideMemo)
  {$ENDIF};
end;

function SetWhere(SQL: _string; Condition: _string): _string;
begin
  Result := _SetWhere(SQL, Condition, TMSParser, True);
end;

function AddWhere(SQL: _string; Condition: _string): _string;
begin
  Result := _AddWhere(SQL, Condition, TMSParser, False);
end;

function DeleteWhere(SQL: _string): _string;
begin
  Result := SetWhere(SQL, '');
end;

function GetWhere(SQL: _string): _string;
begin
  Result := _GetWhere(SQL, TMSParser, False);
end;

function SetOrderBy(SQL: _string; Fields: _string): _string;
begin
  Result := _SetOrderBy(SQL, Fields, TMSParser);
end;

function GetOrderBy(SQL: _string): _string;
begin
  Result := _GetOrderBy(SQL, TMSParser);
end;

function GetFrom(SQL: _string): _string;
begin
  Result := _GetFrom(SQL, TMSParser, False);
end;

procedure GetServerList(List: _TStrings);
var
  MSServerEnum: TMSServerEnumerator;
begin
  MSServerEnum := TMSServerEnumerator.Create;
  try
    MSServerEnum.GetServerList(List);
  finally
    MSServerEnum.Free;
  end;
end;

procedure GetDatabasesList(const Connection: TCustomMSConnection; List: _TStrings);
begin
  if Connection = nil then
    Exit;

  Connection.GetDatabaseNames(List);
end;

procedure GetTablesList(const Connection: TCustomMSConnection; List: _TStrings);
begin
  if Connection = nil then
    Exit;

  Connection.GetTableNames(List);
end;

{ TMSParam }

constructor TMSParam.Create(Collection: TCollection);
begin
  inherited Create(Collection);

  Bound := True;
end;

procedure TMSParam.SetAsString(const Value: string);
begin
  if not (DataType in FieldTypesWithSize) then
    inherited
  else
{$IFDEF VER12P}
  if ParamStringAsAnsiString then
    AsAnsiString := AnsiString(Value)
  else
    Self.Value := Value;
{$ELSE}
{$IFDEF CLR}
  if ParamStringAsAnsiString then
    AsAnsiString := AnsiString(Value)
  else
    Self.Value := Value;
{$ELSE}
    Self.Value := Value;
{$ENDIF}
{$ENDIF}

end;

procedure TMSParam.SetAsWideString(const Value: WideString);
begin
  DataType := ftWideString;
  if not (DataType in FieldTypesWithSize) then
    inherited
  else
    Self.Value := Value;
end;

function TMSParam.GetSize: integer;
begin
  if DataType in FieldTypesWithSize then begin
    if inherited GetSize > 0 then
      Result := inherited GetSize
    else
      if (DataType in [ftBytes, ftVarBytes]) and VarIsArray(Value) then
      {$IFDEF CLR}
        Result := VarArrayHighBound(Value, 1) - VarArrayLowBound(Value, 1) + 1
      {$ELSE}
        Result := TVarData(Value).VArray.Bounds[0].ElementCount
      {$ENDIF}
      else
      Result := Length(GetAsString);
  end
  else
    Result := 0;
end;

procedure TMSParam.SetSize(Value: integer);
begin
  if DataType in FieldTypesWithSize then
    inherited;
end;

procedure TMSParam.SetDataType(Value: TFieldType);
begin
  inherited;
  if Value = TFieldType(ftMSUDT) then
    SubDataType := dtMSUDT;
end;

function TMSParam.GetIsNull: boolean;
begin
  if DataType = ftDataSet then
    Result := (FParamObject as TMSTableObject).GetIsNull
  else
    Result := inherited GetIsNull;
end;

procedure TMSParam.SetAsVariant(const Value: Variant);
var
  l, lold: integer;
begin
  inherited;
  if (DataType in FieldTypesWithSize) and not (VarIsNull(Value) or VarIsEmpty(Value)) then begin
    lold := inherited GetSize;
    if lold > 0 then begin
    {$IFDEF CLR}
      if VarType(Value) = varArray + varByte then
        l := Length(TBytes(Value))
      else
    {$ENDIF}
    {$IFDEF FPC}
      if Variants.VarType(Value) = varArray + varByte then
        l := VarArrayHighBound(Value, 1) - VarArrayLowBound(Value, 1) + 1
      else
    {$ENDIF}
        l := Length(Value);
      if l > lold then
        inherited SetSize(l);
    end;
  end;
  Bound := True;
end;

function TMSParam.GetAsTable: TMSTableObject;
begin
  if DataType = ftUnknown then
    DataType := ftDataSet;

  if DataType = ftDataSet then
    Result := FParamObject as TMSTableObject
  else
    Result := nil;
end;

procedure TMSParam.SetAsTable(Value: TMSTableObject);
begin
  FreeObject;

  inherited DataType := ftDataSet;

  ParamObject := Value;
end;

procedure TMSParam.CreateObject;
begin
  Assert(FParamObject = nil);
  if (DataType in [ftBlob, ftMemo{$IFDEF VER10P}, ftWideMemo{$ENDIF}]) or (DataType = TFieldType(ftMSXML)) then
  {$IFDEF HAVE_COMPRESS}
    FParamObject := TCompressedBlob.Create{$IFDEF VER12P}(DataType = ftWideMemo){$ENDIF}
  {$ELSE}
    FParamObject := {$IFDEF CLR}Devart.Dac.MemData.{$ENDIF}TBlob.Create{$IFDEF VER12P}(DataType = ftWideMemo){$ENDIF}
  {$ENDIF}
  else
    if DataType = ftDataSet then begin
      FParamObject := TMSTableObject.Create;
      TMSTableObject(FParamObject).TableTypeName := FTableTypeName;
    end;
end;

function TMSParam.IsObjectDataType(DataType: TFieldType): boolean;
begin
  Result := inherited IsObjectDataType(DataType) or
    (DataType = TFieldType(ftMSUDT)) or
    (DataType = ftDataSet);
end;

{ TMSParams }

constructor TMSParams.Create(Owner: TPersistent);
begin
  inherited Create(TMSParam);

  FOwner := Owner;
  FNeedsUpdateItem := True;  
end;

procedure TMSParams.Assign(Source: TPersistent); 
var
  i: integer;
begin
  inherited;

  if Source is TMSParams then
    for i := 0 to Count - 1 do
      TMSParams(Source)[i].Assign(Items[i]);
end;
    
function TMSParams.GetItem(Index: Integer): TMSParam;
begin
  Result := inherited Items[Index] as TMSParam;
end;

procedure TMSParams.SetItem(Index: Integer; Value: TMSParam);
begin
  inherited SetItem(Index, Value);
end;

function TMSParams.ParamByName(const Value: _string): TMSParam;
begin
  Result := TMSParam(inherited ParamByName(GetParamNameWODog(Value)));
end;

function TMSParams.FindParam(const Value: _string): TMSParam;
begin
  Result := TMSParam(inherited FindParam(GetParamNameWODog(Value)));
end;

{ TMSConnectionOptions }

procedure TCustomMSConnectionOptions.AssignTo(Dest: TPersistent);
begin
  inherited;

  if Dest is TCustomMSConnectionOptions then begin
    TCustomMSConnectionOptions(Dest).QuotedIdentifier := QuotedIdentifier;
    TCustomMSConnectionOptions(Dest).Encrypt := Encrypt;
    TCustomMSConnectionOptions(Dest).Provider := Provider;
    TCustomMSConnectionOptions(Dest).DefaultLockTimeout := DefaultLockTimeout;
  {$IFDEF VER10P}
    TCustomMSConnectionOptions(Dest).UseWideMemos := UseWideMemos;
  {$ENDIF}
  end;
end;

constructor TCustomMSConnectionOptions.Create(Owner: TCustomMSConnection);
begin
  inherited Create(Owner);

  FQuotedIdentifier := True;
  FProvider := prAuto;
  FCompactVersion := cvAuto;
  FNativeClientVersion := ncAuto;
  FDefaultLockTimeout := DefaultDefaultLockTimeout;
{$IFDEF VER10P}
  FUseWideMemos := True;
{$ENDIF}
end;

procedure TCustomMSConnectionOptions.SetEncrypt(const Value: boolean);
begin
  if FEncrypt <> Value then begin
    TCustomMSConnection(FOwner).CheckInactive;
    FEncrypt := Value;
    if TCustomMSConnection(FOwner).IConnection <> nil then
      TCustomMSConnection(FOwner).IConnection.SetProp(prEncrypt, Value);
  end;
end;

procedure TCustomMSConnectionOptions.SetProvider(const Value: TOLEDBProvider);
begin
  if FProvider <> Value then begin
    TCustomMSConnection(FOwner).CheckInactive;
    FProvider := Value;
    if TCustomMSConnection(FOwner).IConnection <> nil then
      TCustomMSConnection(FOwner).IConnection.SetProp(prProvider, Variant(Value));
  end;
end;

procedure TCustomMSConnectionOptions.SetCompactVersion(const Value: TCompactVersion);
begin
  if FCompactVersion <> Value then begin
    TCustomMSConnection(FOwner).CheckInactive;
    FCompactVersion := Value;
    if TCustomMSConnection(FOwner).IConnection <> nil then
      TCustomMSConnection(FOwner).IConnection.SetProp(prCompactVersion, Variant(Value));
  end;
end;

procedure TCustomMSConnectionOptions.SetNativeClientVersion(const Value: TNativeClientVersion);
begin
  if FNativeClientVersion <> Value then begin
    TCustomMSConnection(FOwner).CheckInactive;
    FNativeClientVersion := Value;
    if TCustomMSConnection(FOwner).IConnection <> nil then
      TCustomMSConnection(FOwner).IConnection.SetProp(prNativeClientVersion, Variant(Value));
  end;
end;

procedure TCustomMSConnectionOptions.SetDefaultLockTimeout(const Value: integer);
begin
  if FDefaultLockTimeout <> Value then begin
    TCustomMSConnection(FOwner).CheckInactive;
    FDefaultLockTimeout := Value;
    if TCustomMSConnection(FOwner).IConnection <> nil then
      if Value > 0 then begin
        TCustomMSConnection(FOwner).IConnection.SetProp(prSetLockTimeout, True);
        TCustomMSConnection(FOwner).IConnection.SetProp(prDefaultLockTimeout, Value);
      end
      else
        TCustomMSConnection(FOwner).IConnection.SetProp(prSetLockTimeout, False);
  end;
end;

{$IFDEF VER10P}
procedure TCustomMSConnectionOptions.SetUseWideMemos(const Value: Boolean);
begin
  if FUseWideMemos <> Value then begin
    FUseWideMemos := Value;
    if TCustomMSConnection(FOwner).IConnection <> nil then
      TCustomMSConnection(FOwner).IConnection.SetProp(prWideMemos, Variant(Value));
  end;
end;
{$ENDIF}

function TCustomMSConnectionOptions.GetNumericType: TDANumericType;
begin
  if EnableBCD then
    Result := ntBCD
{$IFDEF VER6P}
{$IFNDEF FPC}
  else
  if EnableFMTBCD then
    Result := ntFmtBCD
{$ENDIF}
{$ENDIF}
  else
    Result := ntFloat;
end;

procedure TCustomMSConnectionOptions.SetNumericType(Value: TDANumericType);
begin
  EnableBCD := Value = ntBCD;
{$IFDEF VER6P}
{$IFNDEF FPC}
  EnableFMTBCD := Value = ntFmtBCD;
{$ENDIF}
{$ENDIF}
end;

procedure TCustomMSConnectionOptions.SetQuotedIdentifier(const Value: boolean);
begin
  if FQuotedIdentifier <> Value then begin
    // CheckInactive is not need

    FQuotedIdentifier := Value;
    if TCustomMSConnection(FOwner).IConnection <> nil then
      TCustomMSConnection(FOwner).IConnection.SetProp(prQuotedIdentifier, Value);
  end;
end;

{ TCustomMSConnection }

constructor TCustomMSConnection.Create(Owner: TComponent);
begin
  inherited;

  FLockLoginPrompt := False;

  Database := '';
 // IsolationLevel := ilReadCommitted;

  FOptions := inherited Options as TCustomMSConnectionOptions;
end;

destructor TCustomMSConnection.Destroy;
begin
  FMSSQL.Free;
  FDataSet.Free;

  inherited;
end;

function TCustomMSConnection.CreateOptions: TDAConnectionOptions;
begin
  Result := TCustomMSConnectionOptions.Create(Self);
end;

function TCustomMSConnection.IConnection: TOLEDBConnection;
begin
  Result := TOLEDBConnection(FIConnection);
end;

procedure TCustomMSConnection.AssignConnectOptions(Source: TCustomDAConnection);
begin
  inherited;

  Database := TCustomMSConnection(Source).Database;
end;

procedure TCustomMSConnection.Check(const Status: HRESULT; Sender: TObject);
begin
  Assert(FIConnection <> nil);
  TOLEDBConnection(FIConnection).Check(Status, Sender);
end;

procedure TCustomMSConnection.CheckInactive;
begin
  if Connected then
    if ([csUpdating, csDesigning] * ComponentState) <> [] then
      Close
    else
      DatabaseError(SConnectionOpen, Self);
end;

(*
procedure TCustomMSConnection.InternalStartTransaction;  //upd1
begin
  inherited;

  if {Assigned(OnConnectionLost) and}
    ((TOLEDBCOnnection(FIConnection).DBMSPrimaryVer <= 8) or (TOLEDBCOnnection(FIConnection).ProviderPrimaryVer <= 8))and
    not (Options.Provider = prCompact) then begin
    FTransactionID := '';
    TOLEDBConnection(FIConnection).ExecSQL(SCheckConnection);
  end;
  FTransactionID := 'Local';
end;
*)

procedure TCustomMSConnection.DoConnect;
var
  Database: variant;
begin
  inherited;

  if FDatabase = '' then begin
    FIConnection.GetProp(prDatabase, Database);
    FDatabase := _string(Database);
  end;
end;

procedure TCustomMSConnection.AddConnectStringParam(var Result: _string; const ParamName: _string;
  const Value: _string; const DefValue: _string);
begin
  if Value <> DefValue then begin
    if Result <> '' then
      Result := Result + ';';
    Result := Result + ParamName + '=' + Value;
  end;
end;

function TCustomMSConnection.GetConnectString: _string;
begin
  Result := '';

  /// list of supported parameters must be syncronized with SetConnectString (ProcessParam and set param to default)
  if Options.FProvider <> prAuto then
    AddConnectStringParam(Result, 'Provider', GetProviderName(Options.FProvider, Options.FCompactVersion, Options.FNativeClientVersion), '');
  AddConnectStringParam(Result, 'User ID', UserName, '');
  AddConnectStringParam(Result, 'Password', Password, '');
  if Options.Provider <> prCompact then begin
    AddConnectStringParam(Result, 'Data Source', Server, '');
    AddConnectStringParam(Result, 'Initial Catalog', Database, '');
  end
  else
    AddConnectStringParam(Result, 'Data Source', '"' + Database + '"', '');
  AddConnectStringParam(Result, 'Use Encryption for Data', BoolToStr(Options.Encrypt, True), 'False');
end;

function TCustomMSConnection.RecognizedParameter(const Args: array of string; const paramName: string): boolean;
var
  i: integer;
begin
  Result := False;
  for i := 0 to Length(Args) - 1 do begin
    Result := SameText(paramName, Args[i]);
    if Result then
      Break;
  end;
end;

procedure TCustomMSConnection.ProcessConnectStringParam(const paramName: string; paramValue: _string);
var
  s: _string;
begin
  /// list of supported parameters must be syncronized with GetConnectString and dbxsda.ParseIniString
  if RecognizedParameter(['Provider'], paramName) then begin
    s := _UpperCase(paramValue);
    if (s <> SProviderSQLOLEDB) and (s <> SProviderNativeClient) and (s <> SProviderNativeClient10)
      and (s <> SProviderCompact) and (s <> SProviderCompact35) and (s <> SProviderCompact40) then
      raise Exception.CreateFmt(SBadParamValue, [paramName, paramValue]);
    Options.Provider := GetProvider(s);
    if Options.Provider = prNativeClient then
      Options.NativeClientVersion := GetNativeClientVersion(s);
    if Options.Provider = prCompact then
      Options.CompactVersion := GetCompactVersion(s);
  end
  else
  if RecognizedParameter(['User ID', 'UID', 'Username'], paramName) then
    UserName := paramValue
  else
  if RecognizedParameter(['Password', 'PWD'], paramName) then
    Password := paramValue
  else
  if RecognizedParameter(['Data Source', 'Server'], paramName) then begin
    if Options.Provider <> prCompact then
      Server := paramValue
    else
      Database := paramValue;
  end
  else
  if RecognizedParameter(['Initial Catalog', 'Database'], paramName) then
    Database := paramValue
  else
  if RecognizedParameter(['Use Encryption for Data', 'Encryption', 'Encrypt'], paramName) then
    Options.Encrypt := {$IFDEF VER6}CRFunctions.{$ENDIF}StrToBool(paramValue)
  else
  if RecognizedParameter([
    'Persist Security Info',
    'Use Procedure for Prepare',
    'Asynchronous Connection',
    'Client Failover',
    'Tag with column collation when possible'], paramName) then
    // ignored
  else
    raise Exception.CreateFmt(SParamNameUnknown, [paramName]);
end;

procedure TCustomMSConnection.InitConnectStringOptions;
begin
  UserName := '';
  Password := '';
  Server := '';
  Database := '';
  // bug in editor - ConnectionTimeout is not returned;
  // ConnectionTimeout := DefaultConnectionTimeout;
  Options.Encrypt := False;
  Options.Provider := prAuto;
end;

procedure TCustomMSConnection.SetConnectString(Value: _string);

  procedure ParseIniString;
  var
    i, l: integer;
    cs: _string;
    inString{, inValue, allowSpaces}: boolean;
    newIndex, startIndex: integer;
    paramName: string;
    paramValue: _string;
    csLength: integer;

  begin
    cs := Trim(Value);

    // parse connection string
    startIndex := 1;
    paramName := '';
    paramValue := '';

    csLength := Length(cs);

    while startIndex < csLength do begin
      // look for param name
      newIndex := PosEx('=', cs, startIndex);
      if newIndex > 0 then begin
        paramName := Trim(Copy(cs, startIndex, newIndex - startIndex));
        if paramName = '' then
          raise Exception.Create(SParamNameMissing);
      end;

      // look for param value
      inString := False;
//      inValue := False;     // Shows that we are parsing non-quoted value. Must be single word.
//      allowSpaces := True;  // Brought in to avoid two-word non-quoted values.

      for i := newIndex + 1 to csLength do begin
        if cs[i] = '"' then
          inString := not inString
        else
          if not inString then begin
            if cs[i] = ';' then begin
              paramValue := Trim(Copy(cs, newIndex + 1, i - newIndex - 1));
              break;
            end
            else
{              if cs[i] <= ' ' then begin
                if inValue then
                // no more spaces are allowed
                  allowSpaces := False;
              end
              else}
              // check whether char is valid for non-quoted param value
              case cs[i] of
                '!'..'/', '\', '_', ' ', '@', '0'..'9', 'a'..'z', 'A'..'Z':
                begin
{                  inValue := True;
                  if not allowSpaces then
                    raise Exception.Create(SInvalidChar);}
                end
                else
                  if paramName <> 'Password' then
                    raise Exception.Create(SInvalidChar);
              end;
          end;

        if i = csLength then
          paramValue := Trim(Copy(cs, newIndex + 1, i - newIndex));
      end;

      if paramValue = '' then
        raise Exception.Create(SParamValueMissing);

      l := Length(paramValue);
      if (l >= 2) and (paramValue[1] = '"') and (paramValue[l] = '"') then
        paramValue := Trim(Copy(paramValue, 2, l - 2));

      ProcessConnectStringParam(paramName, paramValue);

      startIndex := i + 1;
    end;
  end;

var
  OldCS: _string;

begin
  OldCS := ConnectString;
  try
    InitConnectStringOptions;
    ParseIniString;
  except
    ConnectString := OldCS;
    raise;
  end;
end;
  
procedure TCustomMSConnection.ClearRefs;
var
  i: integer;
begin
  i := 0;
  while i < DataSetCount do
    if DataSets[i] is TMSTableData then
      TMSTableData(DataSets[i]).Connection := nil
    else
      Inc(i);

  inherited;
end;

procedure TCustomMSConnection.CreateIConnection;
begin
  if FIConnection <> nil then
    Exit;

  SetIConnection(GetOLEDBConnection);
end;

procedure TCustomMSConnection.SetIConnection(Value: TCRConnection);
var
  i: integer;
begin
  if (Value <> nil) or
    ((FIConnection <> nil) and not (csDestroying in ComponentState)) then begin
    for i := 0 to DataSetCount - 1 do
      if (DataSets[i] is TMSTableData) and (TMSTableData(DataSets[i]).FIRecordSet <> nil) then
        TMSTableData(DataSets[i]).FIRecordSet.SetConnection(Value);
  end;

  inherited;

  FIConnection := Value as TOLEDBConnection;

  Database := FDatabase; // To prevent empty value
end;

function TCustomMSConnection.GetIConnectionClass: TCRConnectionClass;
begin
  Result := TOLEDBConnection;
end;

function TCustomMSConnection.GetICommandClass: TCRCommandClass;
begin
  Result := TOLEDBCommand;
end;

function TCustomMSConnection.GetIRecordSetClass: TCRRecordSetClass;
begin
  Result := TOLEDBRecordSet;
end;

function TCustomMSConnection.GetIMetaDataClass: TCRMetaDataClass;
begin
  Result := TOLEDBMetaData;
end;

procedure TCustomMSConnection.SetOptions(Value: TCustomMSConnectionOptions);
begin
  FOptions.Assign(Value);
end;

procedure TCustomMSConnection.FillConnectionParameters(var ConnectionParameters: TMSConnectionParameters);
begin
  ConnectionParameters.MinPoolSize := PoolingOptions.MinPoolSize;
  ConnectionParameters.MaxPoolSize := PoolingOptions.MaxPoolSize;
  ConnectionParameters.ConnectionLifeTime := PoolingOptions.ConnectionLifetime;
  ConnectionParameters.Validate := PoolingOptions.Validate;
  ConnectionParameters.Username := Username;
  ConnectionParameters.Server := Server;
  ConnectionParameters.Password := Password;
  ConnectionParameters.Database := Database;

  ConnectionParameters.IsolationLevel := TCRIsolationLevel(IsolationLevel);

  ConnectionParameters.QuotedIdentifier := Options.QuotedIdentifier;
  ConnectionParameters.Encrypt := Options.Encrypt;
  ConnectionParameters.Provider := Options.FProvider;
  ConnectionParameters.OnError := DoError;
end;

procedure TCustomMSConnection.FillConnectionProps(OLEDBConnection: TOLEDBConnection);
begin
  OLEDBConnection.SetProp(prDatabase, FDatabase);
  if FOptions <> nil then begin
    OLEDBConnection.SetProp(prQuotedIdentifier, FOptions.FQuotedIdentifier);
    OLEDBConnection.SetProp(prEncrypt, FOptions.FEncrypt);
    OLEDBConnection.SetProp(prProvider, Variant(FOptions.FProvider));
    OLEDBConnection.SetProp(prCompactVersion, Variant(Options.CompactVersion));
    OLEDBConnection.SetProp(prNativeClientVersion, Variant(Options.NativeClientVersion));
    if Options.DefaultLockTimeout > 0 then begin
      OLEDBConnection.SetProp(prSetLockTimeout, True);
      OLEDBConnection.SetProp(prDefaultLockTimeout, Options.DefaultLockTimeout);
    end
    else
      OLEDBConnection.SetProp(prSetLockTimeout, False);
  end;
end;

function TCustomMSConnection.GetOLEDBConnection: TOLEDBConnection;
var
  ConnectionParameters: TMSConnectionParameters;
begin
  if Pooling then begin
    ConnectionParameters := TMSConnectionParameters.Create;
    try
      FillConnectionParameters(ConnectionParameters);
      Result := TMSConnectionPoolManager.GetConnection(ConnectionParameters,
        TMSSQLMonitor) as TOLEDBConnection;
    finally
      ConnectionParameters.Free;
    end;
  end
  else begin
    Result := TOLEDBConnection.Create;
    FillConnectionProps(Result);
  end;

  if FIConnection <> nil then
    Result.Assign(FIConnection as TOLEDBConnection);

  Result.SetProp(prAutoCommit, AutoCommit);
  Result.SetProp(prDisconnectedMode, Options.DisconnectedMode);
  Result.SetProp(prEnableBCD, Options.EnableBCD);
{$IFDEF VER6P}
{$IFNDEF FPC}
  Result.SetProp(prEnableFMTBCD, Options.EnableFMTBCD);
{$ENDIF}
{$ENDIF}
{$IFDEF VER10P}
  Result.SetProp(prWideMemos, Options.UseWideMemos);
{$ENDIF}
end;

procedure TCustomMSConnection.AssignTo(Dest: TPersistent);
begin
  inherited AssignTo(Dest);

  if Dest is TCustomMSConnection then begin
    TCustomMSConnection(Dest).Database := Database;
    TCustomMSConnection(Dest).IsolationLevel := IsolationLevel;
  end;
end;

procedure TCustomMSConnection.SetDatabase(Value: _string);
begin
  if Value <> FDatabase then begin
    if Options.Provider = prCompact then
      Disconnect;

    FDatabase := Value;

    if (Options.Provider = prCompact) and (csDesigning in ComponentState)
      and (Value <> '') and (Value[1] = '.') and (CurrentProjectOutputDir <> '')
    then
      Value := IncludeTrailingBackslash(CurrentProjectOutputDir) + Value;

    if FIConnection <> nil then
      FIConnection.SetProp(prDatabase, Value);
  end;
end;

function TCustomMSConnection.CreateDataSet: TCustomDADataSet;
begin
  Result := TCustomMSDataSet.Create(nil);
  TCustomMSDataSet(Result).SetDesigning(csDesigning in ComponentState);
  Result.Connection := Self;
end;

function TCustomMSConnection.CreateSQL: TCustomDASQL;
begin
  Result := TMSSQL.Create(nil);
  TMSSQL(Result).SetDesigning(csDesigning in ComponentState);
  Result.Connection := Self;
end;

function TCustomMSConnection.CreateTransaction: TDATransaction;
begin
  Result := TCustomMSTransaction.Create(nil);
  Result.DefaultConnection := Self;
end;

procedure TCustomMSConnection.AssignConnect(Source: TCustomMSConnection);
begin
  inherited AssignConnect(Source);
end;

function TCustomMSConnection.GetClientVersion: string;
begin
  Connect;
  Assert(FIConnection <> nil);
  Result := IConnection.ProviderVer;
end;

function TCustomMSConnection.GetServerVersion: string;
begin
  Connect;
  Assert(FIConnection <> nil);
  Result := IConnection.DBMSVer;
end;

function TCustomMSConnection.DefaultTableSchema: _string;
begin
  Result := 'dbo';
end;

(*procedure TCustomMSConnection.GetTableNames(List: TStrings; AllTables: boolean = False);
var
  MDDS: TMSMetadata;

  procedure AddNamesToList;
  var
    NameFld: TStringField;
    SchemaFld: TStringField;
  begin
    MDDS.Open;

    NameFld := MDDS.FieldByName('TABLE_NAME') as TStringField;
    SchemaFld := nil;
    if Options.Provider <> prCompact then
      SchemaFld := MDDS.FieldByName('TABLE_SCHEMA') as TStringField;
    while not MDDS.Eof do begin
      if (SchemaFld <> nil) and (SchemaFld.Value <> '') then
        List.Add(SchemaFld.Value + '.' + NameFld.Value)
      else
        List.Add(NameFld.Value);
      MDDS.Next;
    end;
  end;

begin
  List.Clear;
  MDDS := nil;
  try
    MDDS := TMSMetadata.Create(nil);
    MDDS.Connection := Self;
    MDDS.DatabaseName := MDDS.Connection.Database;

    MDDS.ObjectType := otTables;
    AddNamesToList;

    if Options.Provider <> prCompact then begin
      MDDS.ObjectType := otViews;
      AddNamesToList;
    end;
    
    if List is TStringList then
      TStringList(List).Sort;
  finally
    MDDS.Free;
  end;
end;


procedure TCustomMSConnection.GetDatabaseNames(List: TStrings);
var
  MDDS: TMSMetadata;
  NameFld: TStringField;
begin
  List.Clear;

  if Options.Provider = prCompact then
    Exit;
    
  MDDS := nil;
  try
    MDDS := TMSMetadata.Create(nil);
    MDDS.Connection := Self;
    MDDS.ObjectType := otDatabases;

    try
      MDDS.Open;
    except
      on E: EMSError do begin
        if E.ErrorCode = 4060 then  
          Database := '';
        raise;
      end;
    end;

    NameFld := MDDS.FieldByName('CATALOG_NAME') as TStringField;
    while not MDDS.Eof do begin
      List.Add(NameFld.Value);
      MDDS.Next
    end;

    if List is TStringList then
      TStringList(List).Sort;
  finally
    MDDS.Free;
  end;
end;

procedure TCustomMSConnection.GetStoredProcNames(List: TStrings);
var
  MDDS: TMSMetadata;
  NameFld: TStringField;
begin
  List.Clear;

  if Options.Provider = prCompact then
    Exit;

  MDDS := nil;
  try
    MDDS := TMSMetadata.Create(nil);
    MDDS.Connection := Self;
    MDDS.ObjectType := otStoredProcs;

    try
      MDDS.Open;
    except
      on E: EMSError do begin
        if E.ErrorCode = 4060 then
          Database := '';
        raise;
      end;
    end;

    NameFld := MDDS.FieldByName('PROCEDURE_NAME') as TStringField;
    while not MDDS.Eof do begin
      List.Add(NameFld.Value);
      MDDS.Next
    end;

    if List is TStringList then
      TStringList(List).Sort;
  finally
    MDDS.Free;
  end;
end;

procedure TCustomMSConnection.GetStoredProcNames(List: TStrings; System: boolean);
var
  Query: TMSQuery;
  NameFld: TStringField;
begin
  if System then
    GetStoredProcNames(List)
  else
  begin
    List.Clear;
    
    if Options.Provider = prCompact then
      Exit;

    Query := TMSQuery.Create(nil);
    try
      Query.Connection := Self;
      Query.SQL.Clear;
      Query.SQL.Add('SELECT name FROM sysobjects');
      Query.SQL.Add('WHERE OBJECTPROPERTY(id, N''IsProcedure'') = 1 or OBJECTPROPERTY(id, N''IsExtendedProc'') = 1');
      Query.SQL.Add('ORDER BY name');
      try
        Query.Open;
      except
        on E: EMSError do begin
          if E.ErrorCode = 4060 then
            Database := '';
          raise;
        end;
      end;

      NameFld := Query.FieldByName('name') as TStringField;
      while not Query.Eof do begin
        List.Add(NameFld.Value);
        Query.Next;
      end;
    finally
      Query.Free;
    end;
  end;
end;*)

procedure TCustomMSConnection.GetTableTypeNames(List: _TStrings);
var
  MetaData: TMSMetadata;
  NameFld: TStringField;
begin
  List.Clear;

  if Options.Provider = prCompact then
    Exit;

  MetaData := TMSMetadata.Create(nil);
  try
    MetaData.Connection := Self;
    MetaData.ObjectType := otTableTypes;
    try
      MetaData.Open;
    except
      on E: EOLEDBError do begin
        if E.OLEDBErrorCode = -2147024809 then // 'The parameter is incorrect.'
      {$IFDEF CLR}
          E := EOLEDBError.Create(E.ErrorCode, STableTypeNotSupported);
        raise E;
      {$ELSE}
          E.Message := STableTypeNotSupported;
        raise;
      {$ENDIF}
      end;
    end;

    NameFld := MetaData.FieldByName('TABLE_NAME') as TStringField;
    while not MetaData.Eof do begin
      List.Add(_VarToStr(NameFld.Value));
      MetaData.Next;
    end;
  finally
    MetaData.Free;
  end;
end;

function TCustomMSConnection.SQLMonitorClass: TClass;
begin
  Result := TMSSQLMonitor;
end;

function TCustomMSConnection.ConnectDialogClass: TConnectDialogClass;
begin
  if Assigned(DefConnectDialogClassProc) then
    Result := TConnectDialogClass(DefConnectDialogClassProc)
  else
    Result := nil;
end;

function TCustomMSConnection.GetIsolationLevel: TIsolationLevel;
begin
  Result := TIsolationLevel(TCustomMSTransaction(DefaultTransaction).IsolationLevel);
end;

procedure TCustomMSConnection.SetIsolationLevel(const Value: TIsolationLevel);
begin
  TCustomMSTransaction(DefaultTransaction).IsolationLevel := TCRIsolationLevel(Value);
end;

procedure TCustomMSConnection.DoError(E: Exception; var Fail, Reconnect, Reexecute: boolean;
  ReconnectAttempt: integer; var ConnLostCause: TConnLostCause);
var
  i: integer;
begin
  inherited;

  if Reconnect then
    for i := 0 to DataSetCount - 1 do begin
      if DataSets[i] is TCustomMSDataSet then begin
        if (TCustomMSDataSet(DataSets[i]).FIRecordSet <> nil) and       //since DAC 6.50 can be nill
          (TCustomMSDataSet(DataSets[i]).FIRecordSet.GetIRowset <> nil) //can be because of Servercursor, ...
        then begin
          Reconnect := False;
          Exit;
        end;
      end;
    end;
end;

procedure TCustomMSConnection.OpenDatasets(const ds: array of TCustomMSDataSet);
var
  OldAutoPrepare: boolean;
  i, p: Integer;
begin
  if FDataSet = nil then
    FDataSet := CreateDataSet as TCustomMSDataSet;

  FDataSet.FBulkExecuting := True;
  FDataSet.SQL.Text := '';
  FDataSet.ParamCheck := False;
  FDataSet.Params.Clear;

  for i := Low(ds) to High(ds) do begin
    ds[i].DoBeforeOpenCursor;
    if ds[i].CursorType <> ctDefaultResultSet then
      DatabaseError(SUnallowableCursorType, ds[i]);

    FDataSet.SQL.Add(ds[i].SQL.Text);
    for p := 0 to ds[i].Params.Count - 1 do
      FDataSet.Params.Add.Assign(ds[i].Params[p]);
  end;

  try
    if FDataSet.FIRecordSet <> nil then
      FDataSet.FIRecordSet.SetProp(prBulkExecuting, True);

    FDataSet.Execute;
    Assert(FDataSet.FIRecordSet <> nil);

    for i := Low(ds) to High(ds) do begin
      FDataSet.FIRecordSet.AssignToNextResult(ds[i].FIRecordSet);

      OldAutoPrepare := ds[i].Options.AutoPrepare;
      try
        ds[i].Options.AutoPrepare := False;
        ds[i].Open;
      finally
        ds[i].Options.AutoPrepare := OldAutoPrepare;
      end;
    end;
  finally
    FDataSet.FIRecordSet.Active := True;
    FDataSet.CloseCursor;
  end;
end;

{ TMSConnectionOptions }

constructor TMSConnectionOptions.Create(Owner: TMSConnection);
begin
  inherited Create(Owner);

  FAutoTranslate := True;
  FPacketSize := DefaultPacketSize;
  FFailoverPartner := '';
end;

procedure TMSConnectionOptions.AssignTo(Dest: TPersistent);
begin
  inherited;

  if Dest is TMSConnectionOptions then begin
    TMSConnectionOptions(Dest).Language := Language;
    TMSConnectionOptions(Dest).PersistSecurityInfo := PersistSecurityInfo;
    TMSConnectionOptions(Dest).AutoTranslate := AutoTranslate;
    TMSConnectionOptions(Dest).NetworkLibrary := NetworkLibrary;
    TMSConnectionOptions(Dest).ApplicationName := ApplicationName;
    TMSConnectionOptions(Dest).WorkstationID := WorkstationID;
    TMSConnectionOptions(Dest).PacketSize := PacketSize;
    TMSConnectionOptions(Dest).FailoverPartner := FailoverPartner;
    TMSConnectionOptions(Dest).TrustServerCertificate := TrustServerCertificate;
  end;
end;

procedure TMSConnectionOptions.SetLanguage(const Value: _string);
begin
  if FLanguage <> Value then begin
    TMSConnection(FOwner).CheckInactive;
    FLanguage := Value;
    if TMSConnection(FOwner).IConnection <> nil then
      TMSConnection(FOwner).IConnection.SetProp(prLanguage, Value);
  end;
end;

procedure TMSConnectionOptions.SetPersistSecurityInfo(const Value: boolean);
begin
  if FPersistSecurityInfo <> Value then begin
    TMSConnection(FOwner).CheckInactive;
    FPersistSecurityInfo := Value;
    if TMSConnection(FOwner).IConnection <> nil then
      TMSConnection(FOwner).IConnection.SetProp(prPersistSecurityInfo, Value);
  end;
end;

procedure TMSConnectionOptions.SetAutoTranslate(const Value: boolean);
begin
  if FAutoTranslate <> Value then begin
    TMSConnection(FOwner).CheckInactive;
    FAutoTranslate := Value;
    if TMSConnection(FOwner).IConnection <> nil then
      TMSConnection(FOwner).IConnection.SetProp(prAutoTranslate, Value);
  end;
end;

procedure TMSConnectionOptions.SetNetworkLibrary(const Value: _string);
begin
  if FNetworkLibrary <> Value then begin
    TMSConnection(FOwner).CheckInactive;
    FNetworkLibrary := Value;
    if TMSConnection(FOwner).IConnection <> nil then
      TMSConnection(FOwner).IConnection.SetProp(prNetworkLibrary, Value);
  end;
end;

procedure TMSConnectionOptions.SetApplicationName(const Value: _string);
begin
  if FApplicationName <> Value then begin
    TMSConnection(FOwner).CheckInactive;
    FApplicationName := Value;
    if TMSConnection(FOwner).IConnection <> nil then
      TMSConnection(FOwner).IConnection.SetProp(prApplicationName, Value);
  end;
end;

procedure TMSConnectionOptions.SetWorkstationID(const Value: _string);
begin
  if FWorkstationID <> Value then begin
    TMSConnection(FOwner).CheckInactive;
    FWorkstationID := Value;
    if TMSConnection(FOwner).IConnection <> nil then
      TMSConnection(FOwner).IConnection.SetProp(prWorkstationID, Value);
  end;
end;

procedure TMSConnectionOptions.SetPacketSize(const Value: integer);
begin
  if FPacketSize <> Value then begin
    TMSConnection(FOwner).CheckInactive;
    FPacketSize := Value;
    if TMSConnection(FOwner).IConnection <> nil then
      TMSConnection(FOwner).IConnection.SetProp(prPacketSize, Value);
  end;
end;

procedure TMSConnectionOptions.SetInitialFileName(const Value: _string);
begin
  if FInitialFileName <> Value then begin
    TMSConnection(FOwner).CheckInactive;
    FInitialFileName := Value;
    if TMSConnection(FOwner).IConnection <> nil then
      TCustomMSConnection(FOwner).IConnection.SetProp(prInitialFileName, Value);
  end;
end;

procedure TMSConnectionOptions.SetMultipleActiveResultSets(const Value: boolean);
begin
  if FMultipleActiveResultSets <> Value then begin
    TMSConnection(FOwner).CheckInactive;
    FMultipleActiveResultSets := Value;
    if TMSConnection(FOwner).IConnection <> nil then
      TMSConnection(FOwner).IConnection.SetProp(prMARS, Value);
  end;
end;

procedure TMSConnectionOptions.SetFailoverPartner(const Value: _string);
begin
  if FFailoverPartner <> Value then begin
    TMSConnection(FOwner).CheckInactive;
    FFailoverPartner := Value;
    if TMSConnection(FOwner).IConnection <> nil then
      TMSConnection(FOwner).IConnection.SetProp(prFailoverPartner, Value);
  end;
end;

procedure TMSConnectionOptions.SetTrustServerCertificate(const Value: boolean);
begin
  if FTrustServerCertificate <> Value then begin
    TMSConnection(FOwner).CheckInactive;
    FTrustServerCertificate := Value;
    if TMSConnection(FOwner).IConnection <> nil then
      TMSConnection(FOwner).IConnection.SetProp(prTrustServerCertificate, Value);
  end;
end;

procedure TMSConnectionOptions.LoadCompactVersionProperty(Reader: TReader);
begin
  SetCompactVersion(TCompactVersion(GetEnumValue(TypeInfo(TCompactVersion), Reader.ReadIdent)));
end;

procedure TMSConnectionOptions.StoreCompactVersionProperty(Writer: TWriter);
begin
  Writer.WriteIdent(GetEnumName(TypeInfo(TCompactVersion), Integer(FCompactVersion)));
end;

procedure TMSConnectionOptions.LoadNativeClientVersionProperty(Reader: TReader);
begin
  SetNativeClientVersion(TNativeClientVersion(GetEnumValue(TypeInfo(TNativeClientVersion), Reader.ReadIdent)));
end;

procedure TMSConnectionOptions.StoreNativeClientVersionProperty(Writer: TWriter);
begin
  Writer.WriteIdent(GetEnumName(TypeInfo(TNativeClientVersion), Integer(FNativeClientVersion)));
end;

procedure TMSConnectionOptions.DefineProperties(Filer: TFiler);
begin
  inherited;
  Filer.DefineProperty('CompactVersion', LoadCompactVersionProperty, StoreCompactVersionProperty, Provider = prCompact);
  Filer.DefineProperty('NativeClientVersion', LoadNativeClientVersionProperty, StoreNativeClientVersionProperty, Provider = prNativeClient);
  Filer.DefineProperty('NativeClientVerison', LoadNativeClientVersionProperty, nil, False);
end;

{ TMSConnection }

constructor TMSConnection.Create(Owner: TComponent);
begin
  inherited;

  Authentication := auServer;
  ConnectionTimeout := DefaultConnectionTimeout;
  FSPID := -1;
end;

procedure TMSConnection.SetIConnection(Value: TCRConnection);
begin
  inherited;

  if FIConnection <> nil then
    TOLEDBConnection(FIConnection).OnInfoMessage := DoInfoMessage;
end;

procedure TMSConnection.FillConnectionParameters(var ConnectionParameters: TMSConnectionParameters);
begin
  inherited;

  ConnectionParameters.Language := Options.Language;
  ConnectionParameters.PersistSecurityInfo := Options.PersistSecurityInfo;
  ConnectionParameters.AutoTranslate := Options.AutoTranslate;
  ConnectionParameters.Authentication := Authentication;
  ConnectionParameters.NetworkLibrary := Options.NetworkLibrary;
  ConnectionParameters.ApplicationName := Options.ApplicationName;
  ConnectionParameters.WorkstationID := Options.WorkstationID;
  ConnectionParameters.PacketSize := Options.PacketSize;
  ConnectionParameters.TrustServerCertificate := Options.TrustServerCertificate;
end;

procedure TMSConnection.FillConnectionProps(OLEDBConnection: TOLEDBConnection);
begin
  inherited;

  OLEDBConnection.SetProp(prAuthentication, Variant(FAuthentication));
  OLEDBConnection.SetProp(prConnectionTimeout, FConnectionTimeout);

  if FOptions <> nil then begin
    OLEDBConnection.SetProp(prLanguage, Options.FLanguage);
    OLEDBConnection.SetProp(prPersistSecurityInfo, Options.FPersistSecurityInfo);
    OLEDBConnection.SetProp(prAutoTranslate, Options.FAutoTranslate);
    OLEDBConnection.SetProp(prNetworkLibrary, Options.FNetworkLibrary);
    OLEDBConnection.SetProp(prApplicationName, Options.FApplicationName);
    OLEDBConnection.SetProp(prWorkstationID, Options.FWorkstationID);
    OLEDBConnection.SetProp(prPacketSize, Options.FPacketSize);
    OLEDBConnection.SetProp(prInitialFileName, Options.FInitialFileName);
    OLEDBConnection.SetProp(prMARS, Options.FMultipleActiveResultSets);
    OLEDBConnection.SetProp(prFailoverPartner, Options.FFailoverPartner);
    OLEDBConnection.SetProp(prTrustServerCertificate, Options.FTrustServerCertificate);
  end;
end;

procedure TMSConnection.AssignTo(Dest: TPersistent);
begin
  inherited AssignTo(Dest);

  if Dest is TMSConnection then begin
    TMSConnection(Dest).Authentication := Authentication;
    TMSConnection(Dest).ConnectionTimeout := ConnectionTimeout;
  end;
end;

function TMSConnection.CreateOptions: TDAConnectionOptions;
begin
  Result := TMSConnectionOptions.Create(Self);
end;

function TMSConnection.GetOptions: TMSConnectionOptions;
begin
  Result := FOptions as TMSConnectionOptions;
end;

procedure TMSConnection.SetOptions(Value: TMSConnectionOptions);
begin
  FOptions.Assign(Value);
end;

function TMSConnection.NeedPrompt: boolean;
begin
  Result := False;
  if Authentication = auWindows then
    Exit;

  Result := inherited NeedPrompt;
end;

procedure TMSConnection.SetAuthentication(const Value: TMSAuthentication);
begin
  if FAuthentication <> Value then begin
    Disconnect;
    FAuthentication := Value;
    if FIConnection <> nil then
      FIConnection.SetProp(prAuthentication, Variant(Value));
  end;
end;

procedure TMSConnection.SetConnectionTimeout(const Value: integer);
begin
  if FConnectionTimeout <> Value then begin
    FConnectionTimeout := Value;
    if FIConnection <> nil then
      FIConnection.SetProp(prConnectionTimeout, Value);
  end;
end;

function TMSConnection.GetConnectString: _string;
begin
  Result := inherited GetConnectString;

  AddConnectStringParam(Result, 'Connect Timeout', IntToStr(ConnectionTimeout), IntToStr(DefaultConnectionTimeout));
  AddConnectStringParam(Result, 'Current Language', Options.Language, '');
  AddConnectStringParam(Result, 'Persist Security Info', BoolToStr(Options.PersistSecurityInfo, True), 'False');
  AddConnectStringParam(Result, 'Auto Translate', BoolToStr(Options.AutoTranslate, True), 'True');
  AddConnectStringParam(Result, 'Network Library', Options.NetworkLibrary, '');
  AddConnectStringParam(Result, 'Application Name', Options.ApplicationName, '');
  AddConnectStringParam(Result, 'Workstation ID', Options.WorkstationID, '');
  AddConnectStringParam(Result, 'Packet Size', IntToStr(Options.PacketSize), IntToStr(DefaultPacketSize));
  if Options.InitialFileName <> '' then
    AddConnectStringParam(Result, 'AttachDBFileName', Options.InitialFileName, '');
  AddConnectStringParam(Result, 'MultipleActiveResultSets', BoolToStr(Options.MultipleActiveResultSets, True), 'False');
  if Options.FailoverPartner <> '' then
    AddConnectStringParam(Result, 'Failover Partner', Options.FailoverPartner, '');
  AddConnectStringParam(Result, 'Trust Server Certificate', BoolToStr(Options.TrustServerCertificate, True), 'False');
  
  if Authentication = auWindows then
    Result := Result + ';Integrated Security=SSPI';
end;

procedure TMSConnection.SetConnectString(Value: _string);
begin
  inherited SetConnectString(Value);
  
  if FIntegratedSecuritySSPI then
    Authentication := auWindows;
end;

procedure TMSConnection.ProcessConnectStringParam(const paramName: string; paramValue: _string);
begin
  if RecognizedParameter(['Integrated Security', 'Trusted_Connection'], paramName) then begin
    if paramValue = 'SSPI' then
      FIntegratedSecuritySSPI := True
    else
      if not TryStrToBool(paramValue, FIntegratedSecuritySSPI) then
        raise Exception.CreateFmt(SBadParamValue, [paramName, paramValue]);
  end
  else
  if RecognizedParameter(['Connect Timeout', 'ConnectTimeout', 'Timeout'], paramName) then
    ConnectionTimeout := StrToInt(paramValue)
  else
  if RecognizedParameter(['Current Language', 'Language'], paramName) then
    Options.Language := paramValue
  else
  if RecognizedParameter(['PersistSecurityInfo', 'Persist Security Info'], paramName) then
    Options.PersistSecurityInfo := {$IFDEF VER6}CRFunctions.{$ENDIF}StrToBool(paramValue)
  else
  if RecognizedParameter(['AutoTranslate', 'Auto Translate'], paramName) then
    Options.AutoTranslate := {$IFDEF VER6}CRFunctions.{$ENDIF}StrToBool(paramValue)
  else
  if RecognizedParameter(['Network Library', 'Network', 'NetworkLibrary', 'NetLibrary'], paramName) then
    Options.NetworkLibrary := paramValue
  else
  if RecognizedParameter(['ApplicationName', 'Application Name', 'AppName'], paramName) then
    Options.ApplicationName := paramValue
  else
  if RecognizedParameter(['WorkstationID', 'Workstation ID', 'WSID'], paramName) then
    Options.WorkstationID := paramValue
  else
  if RecognizedParameter(['Packet Size', 'PacketSize'], paramName) then
    Options.PacketSize := StrToInt(paramValue)
  else
  if RecognizedParameter(['AttachDBFileName', 'InitialFileName', 'InitFileName'], paramName) then
    Options.InitialFileName := paramValue
  else
  if RecognizedParameter(['MultipleActiveResultSets', 'Multiple Active Result Sets', 'MARS Connection', 'MARS'], paramName) then
    Options.MultipleActiveResultSets := {$IFDEF VER6}CRFunctions.{$ENDIF}StrToBool(paramValue)
  else
  if RecognizedParameter(['FailoverPartner', 'Failover Partner', 'Failover_Partner'], paramName) then
    Options.FailoverPartner := paramValue
  else
  if RecognizedParameter(['TrustServerCertificate', 'Trust Server Certificate'], paramName) then
    Options.TrustServerCertificate := {$IFDEF VER6}CRFunctions.{$ENDIF}StrToBool(paramValue)
  else
    inherited ProcessConnectStringParam(paramName, paramValue);
end;

procedure TMSConnection.InitConnectStringOptions;
begin
  inherited;

  Options.Language := '';
  Options.PersistSecurityInfo := False;
  Options.AutoTranslate := True;
  Options.NetworkLibrary := '';
  Options.PacketSize := DefaultPacketSize;
  Options.InitialFileName := '';
  Options.MultipleActiveResultSets := False;
  Options.FailoverPartner := '';
  Options.TrustServerCertificate := False;
        
  Authentication := auServer;
  FIntegratedSecuritySSPI := False;
end;

procedure TMSConnection.DoInfoMessage(E: EMSError);
begin
  TMSSQLMonitorClass(SQLMonitorClass).InfoMessage(Self, E.Message);

  if Assigned(FOnInfoMessage) then
    FOnInfoMessage(Self, E);
end;

function TMSConnection.GetSPID: integer;
begin
  if FSPID = -1 then
    FSPID := ExecSQL('SET :Result = @@SPID', []);
  Result := FSPID;
end;

procedure TMSConnection.ChangePassword(NewPassword: _string);
var
  OldConnected: boolean;
  OldPassword: _string;
  OldLoginPrompt: boolean;
begin
  OldConnected := Connected;
  OldPassword := Password;
  OldLoginPrompt := LoginPrompt;
  try
    if not Connected then
      CreateIConnection
    else
      Disconnect;
    LoginPrompt := False;
    Assert(FIConnection <> nil);
    FIConnection.SetProp(prOldPassword, Password);
    Password := NewPassword;
    try
      Connect;
    except
      Password := OldPassword;
      raise;
    end;
  finally
    Assert(FIConnection <> nil);
    FIConnection.SetProp(prOldPassword, '');
    LoginPrompt := OldLoginPrompt;
    if not OldConnected then
      Disconnect;
  end;
end;

{ TCustomMSTransaction }

function TCustomMSTransaction.SQLMonitorClass: TClass;
begin
  Result := TMSSQLMonitor;
end;

function TCustomMSTransaction.GetITransactionClass: TCRTransactionClass;
begin
  if TransactionType = ttNative then
    Result := TOLEDBTransaction
  else
    Result := inherited GetITransactionClass;  
end;

{$IFNDEF STD}
{ TMSNotificationParser }

constructor TMSNotificationParser.Create;
begin
  inherited Create;
  
  FNotificationInfo := niUnknown;
  FNotificationSource := nsUnknown;
  FNotificationType := ntUnknown;
  FKey := '';
end;

procedure TMSNotificationParser.ProcessMessage(Message: {$IFDEF CLR}TBytes{$ELSE}WideString{$ENDIF});
const
  InfoAttribute = 'info';
  MessageNode = 'Message';
  RootNode = 'QueryNotification';
  SourceAttribute = 'source';
  TypeAttribute = 'type';
var
  XmlReader: XmlTextReader;
  Value: _string;
begin
  XmlReader := nil;
  try
    FNotificationInfo := niUnknown;
    FNotificationSource := nsUnknown;
    FNotificationType := ntUnknown;
    FKey := '';
  {$IFDEF CLR}
    XmlReader := XmlTextReader.Create(MemoryStream.Create(Message));
  {$ELSE}
    XmlReader := XmlTextReader.Create(AnsiString(Message));
  {$ENDIF}
    if XmlReader.Read then begin
      if (XmlReader.NodeType = ntElement) and (_LowerCase(XmlReader.Name) = 'qn:querynotification') and (XmlReader.AttributeCount >= 3) then begin
        XmlReader.MoveToAttribute(InfoAttribute);
        Value := _LowerCase(XmlReader.Value);
        if Value = 'alter' then
          FNotificationInfo := niAlter
        else
        if Value = 'delete' then
          FNotificationInfo := niDelete
        else
        if Value = 'drop' then
          FNotificationInfo := niDrop
        else
        if Value = 'error' then
          FNotificationInfo := niError
        else
        if Value = 'insert' then
          FNotificationInfo := niInsert
        else
        if Value = 'invalid' then
          FNotificationInfo := niInvalid
        else
        if Value = 'isolation' then
          FNotificationInfo := niIsolation
        else
        if Value = 'set options' then
          FNotificationInfo := niOptions
        else
        if Value = 'previous invalid' then
          FNotificationInfo := niPreviousFire
        else
        if Value = 'query' then
          FNotificationInfo := niQuery
        else
        if Value = 'resource' then
          FNotificationInfo := niResource
        else
        if Value = 'restart' then
          FNotificationInfo := niRestart
        else
        if Value = 'query template limit' then
          FNotificationInfo := niTemplateLimit
        else
        if Value = 'truncate' then
          FNotificationInfo := niTruncate
        else
        if Value = 'update' then
          FNotificationInfo := niUpdate;

        XmlReader.MoveToAttribute(SourceAttribute);
        Value := _LowerCase(XmlReader.Value);
        if Value = 'client' then
          FNotificationSource := nsClient
        else
        if Value = 'data' then
          FNotificationSource := nsData
        else
        if Value = 'database' then
          FNotificationSource := nsDatabase
        else
        if Value = 'environment' then
          FNotificationSource := nsEnvironment
        else
        if Value = 'execution' then
          FNotificationSource := nsExecution
        else
        if Value = 'object' then
          FNotificationSource := nsObject
        else
        if Value = 'statement' then
          FNotificationSource := nsStatement
        else
        if Value = 'system' then
          FNotificationSource := nsSystem
        else
        if Value = 'timeout' then
          FNotificationSource := nsTimeout;

        XmlReader.MoveToAttribute(TypeAttribute);
        Value := _LowerCase(XmlReader.Value);
        if Value = 'change' then
          FNotificationType := ntChange
        else
        if Value = 'subscribe' then
          FNotificationType := ntSubscribe;

        XmlReader.Read;
        if (XmlReader.NodeType = ntElement) and (_LowerCase(XmlReader.Name) = 'qn:message') then begin
          XmlReader.Read;
          FKey := XmlReader.Value;
        end;
      end;
    end;
  finally
    XmlReader.Free;
  end;
end;

{ TMSChangeNotification }

constructor TMSChangeNotification.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);

  FDataSets := TDAList.Create;
  FNotificators := TDAList.Create;
  FTimeOut := SSPROPVAL_DEFAULT_NOTIFICATION_TIMEOUT;
  FEnabled := True;
end;

destructor TMSChangeNotification.Destroy;
var
  i: integer;
begin
  for i := FDatasets.Count - 1 downto 0 do begin
    RemoveNotificator(TCustomMSDataSet(FDatasets[i]));
    TCustomMSDataSet(FDatasets[i]).ChangeNotification := nil;
  end;
  Assert(FDataSets.Count = 0);
  FDataSets.Free;
  Assert(FNotificators.Count = 0);
  FNotificators.Free;
  
  inherited;
end;

procedure TMSChangeNotification.SetEnabled(Value: boolean);
var
  i: integer;
begin
  if FEnabled <> Value then begin
    for i := 0 to FNotificators.Count - 1 do
      TMSServiceBroker(FNotificators[i]).AsyncNotification := Value;
    FEnabled := Value;
  end;
end;

procedure TMSChangeNotification.SetTimeOut(Value: integer);
begin
  // if (Value > SSPROPVAL_MAX_NOTIFICATION_TIMEOUT) then
  if (Value < 0) then
    DatabaseError(SInvalidNotificationTimeout);
  FTimeOut := Value;
end;

procedure TMSChangeNotification.SetService(Value: _string);
begin
  FService := Value;
end;

procedure TMSChangeNotification.Notification(AComponent: TComponent; Operation: TOperation);
begin
  if (Operation = opRemove) and (AComponent <> nil) and IsClass(AComponent, TCustomMSDataSet) then
    RemoveDataSet(TCustomMSDataSet(AComponent));
  
  inherited;
end;

procedure TMSChangeNotification.AddDataSet(DataSet: TCustomMSDataSet);
begin
  FDataSets.Add(DataSet);
  DataSet.FreeNotification(Self);
end;

procedure TMSChangeNotification.RemoveDataSet(DataSet: TCustomMSDataSet);
begin
  FDataSets.Remove(DataSet);
  DataSet.RemoveFreeNotification(Self);
end;

function TMSChangeNotification.GetSubscriptionKey(DataSet: TCustomMSDataSet): string;
var
  i: integer;
begin
  Result := '';
  for i := 1 to Length(DataSet.SQL.Text) do
    Result := Result + IntToStr(Ord(DataSet.SQL.Text[i]));
end;

procedure TMSChangeNotification.CheckDataSet(DataSet: TCustomMSDataSet);
begin
  if not IsClass(DataSet.Connection, TMSConnection) then
    DatabaseError(SCompactEditionNotSupported, Self);
  if TMSConnection(DataSet.Connection).Options.Provider = prCompact then
    DatabaseError(SCompactEditionNotSupported, Self);
  //if (not DataSet.ReadOnly) and (not TMSConnection(DataSet.Connection).Options.MultipleActiveResultSets) then
  //  DatabaseError(SChangeNotificationNeedMARS, Self);
end;

function TMSChangeNotification.FindNotificator(DataSet: TCustomMSDataSet): TObject;
var
  i: integer;
begin
  Result := nil;
  for i := 0 to FNotificators.Count - 1 do
    if TMSServiceBroker(FNotificators[i]).Connection = DataSet.Connection then begin
      Result := TMSServiceBroker(FNotificators[i]);
      Break;
    end;
end;

procedure TMSChangeNotification.AddNotificator(DataSet: TCustomMSDataSet);
var
  Notificator: TMSServiceBroker;
begin
  CheckDataSet(DataSet);
  Notificator := TMSServiceBroker(FindNotificator(DataSet));
  if Notificator = nil then begin
    Notificator := TMSServiceBroker.Create(nil);
    Notificator.Connection := TMSConnection(DataSet.Connection);
    Notificator.OnMessage := DoOnNotification;
    if FService = '' then begin
      Notificator.Service := sNotificationService + IntToStr(TMSConnection(DataSet.Connection).SPID);
      Notificator.CreateServerObjects(sNotificationContract);
    end
    else
      Notificator.Service := FService;
    FNotificators.Add(Notificator);
    Assert(Notificator.Queue <> ''); // cache queue name
  end;

  if DataSet.FICommand <> nil then begin
    DataSet.FICommand.SetProp(prNotificationService, Notificator.Service);
    DataSet.FICommand.SetProp(prNotificationMessage, GetSubscriptionKey(DataSet));
    DataSet.FICommand.SetProp(prNotificationTimeout, TimeOut);
  end;
end;

procedure TMSChangeNotification.RemoveNotificator(DataSet: TCustomMSDataSet);
var
  Notificator: TMSServiceBroker;
  i: integer;
begin
  Notificator := TMSServiceBroker(FindNotificator(DataSet));
  if Notificator <> nil then begin
    for i := 0 to FDataSets.Count - 1 do
      if (Notificator.Connection = TCustomMSDataSet(FDataSets[i]).Connection) then
        if (TCustomMSDataSet(FDataSets[i]) <> DataSet) then
          Exit;
    try
      if FService = '' then begin
        // Drop service, queue and services and queues for invalid connections
        Notificator.AsyncNotification := False;
        Notificator.Connection.ExecSQL(
          'DECLARE @INVALID_SERVICE nvarchar(128);' + LineSeparator +
          'SET @INVALID_SERVICE = N''' + Notificator.Service + ''';' + LineSeparator +
          'DECLARE @DROP_STATEMENT nvarchar(300);' + LineSeparator +
          'WHILE @INVALID_SERVICE <> N''''' + LineSeparator +
          'BEGIN' + LineSeparator +
          '  SET @DROP_STATEMENT = N''DROP SERVICE '' + @INVALID_SERVICE;' + LineSeparator +
          '  EXECUTE sp_executesql @DROP_STATEMENT;' + LineSeparator +
          '  SET @DROP_STATEMENT = N''DROP QUEUE '' + @INVALID_SERVICE + N''_QUEUE'';' + LineSeparator +
          '  EXECUTE sp_executesql @DROP_STATEMENT;' + LineSeparator +
          '  SET @INVALID_SERVICE = N'''';' + LineSeparator +
          '  SELECT TOP(1) @INVALID_SERVICE = [name] FROM sys.services' + LineSeparator +
          '  WHERE' + LineSeparator +
          '    [name] LIKE ''' + sNotificationService + '%'' ' + LineSeparator +
          '    AND [name] not in (' + LineSeparator +
          '      SELECT ''' + sNotificationService + ''' + CAST([spid] AS VARCHAR(32)) AS [name] ' + LineSeparator +
          '      FROM master.dbo.sysprocesses WHERE [spid] >= 0 and spid <= 32767)' + LineSeparator +
          'END;', []);
      end;
    finally
      FNotificators.Remove(Notificator);
      try
        TDBAccessUtils.InternalDisconnect(Notificator.Connection);
      finally
        Notificator.Free;
      end;
    end;
  end;
end;

procedure TMSChangeNotification.StartNotification(DataSet: TCustomMSDataSet);
var
  Notificator: TMSServiceBroker;
  DelayedSubsciption: variant;
  Subscriber: TMSSQL;
begin
  Notificator := TMSServiceBroker(FindNotificator(DataSet));
  Assert(Notificator <> nil);

  TDBAccessUtils.InternalConnect(Notificator.Connection);
  Assert(DataSet.FICommand <> nil);
  DataSet.FICommand.GetProp(prDelayedSubsciption, DelayedSubsciption); 
  if Boolean(DelayedSubsciption) then begin
    Subscriber := TMSSQL.Create(nil);
    try
      Subscriber.Connection := DataSet.Connection;
      Subscriber.SQL.Text := SCheckConnection;
      Subscriber.Execute;
      Subscriber.SQL.Text := DataSet.FinalSQL;   // to expand macros
      Subscriber.FICommand.SetProp(prNotification, True);
      Subscriber.FICommand.SetProp(prNotificationService, Notificator.Service);
      Subscriber.FICommand.SetProp(prNotificationTimeout, TimeOut);
      Subscriber.FICommand.SetProp(prNotificationMessage, GetSubscriptionKey(DataSet));
      Subscriber.Params.Assign(DataSet.Params);
      Subscriber.Execute;
    finally
      Subscriber.Free;
    end;
  end;
  Notificator.AsyncNotification := True;
end;

procedure TMSChangeNotification.DoOnNotification(Sender: TObject);
var
  ServiceBroker: TMSServiceBroker;
  DataSet: TCustomMSDataSet;
  NotificationParser: TMSNotificationParser;
  i: integer;
begin
  Assert(IsClass(Sender, TMSServiceBroker));
  ServiceBroker := TMSServiceBroker(Sender);
  NotificationParser := TMSNotificationParser.Create;
  try
    while ServiceBroker.Receive do begin
    {$IFDEF CLR}
      NotificationParser.ProcessMessage(ServiceBroker.CurrentMessage.AsBytes);
    {$ELSE}
      NotificationParser.ProcessMessage(ServiceBroker.CurrentMessage.AsWideString);
    {$ENDIF}
      for i := 0 to FDataSets.Count - 1 do begin
        DataSet := TCustomMSDataSet(FDataSets[i]);
        if (DataSet.Connection = ServiceBroker.Connection) and (GetSubscriptionKey(DataSet) = NotificationParser.Key) then begin
          if Assigned(FOnChange) then
            FOnChange(Self, TCustomMSDataSet(DataSet), NotificationParser.NotificationInfo,
              NotificationParser.NotificationSource, NotificationParser.NotificationType);
          if DataSet.Active and DataSet.Options.ReflectChangeNotify then
            DataSet.ReflectChanges(NotificationParser.NotificationInfo,
              NotificationParser.NotificationSource, NotificationParser.NotificationType);
        end;
      end;
    end;
  finally
    NotificationParser.Free;
  end;
end;
{$ENDIF}

{ TMSDataSetOptions }

constructor TMSDataSetOptions.Create(Owner: TCustomDADataSet);
begin
  inherited Create(Owner);

  LongStrings := True;
  RequiredFields := False;
  UniqueRecords := False;
  CursorUpdate := True;
  QueryIdentity := True;
  CheckRowVersion := False;
  FullRefresh := False;
  DMLRefresh := False;
  FAutoRefresh := False;
  FAutoRefreshInterval := 60;
  FNonBlocking := False;
  FDisableMultipleResults := False;
end;

procedure TMSDataSetOptions.SetUniqueRecords(Value: boolean);
begin
  if FUniqueRecords <> Value then begin
    FUniqueRecords := Value;
    if TCustomMSDataSet(FOwner).FIRecordSet <> nil then
      TCustomMSDataSet(FOwner).FIRecordSet.SetProp(prUniqueRecords, FUniqueRecords);
  end;
end;

procedure TMSDataSetOptions.SetCursorUpdate(Value: boolean);
begin
  if FCursorUpdate <> Value then begin
    FCursorUpdate := Value;
    if TCustomMSDataSet(FOwner).FIRecordSet <> nil then
      TCustomMSDataSet(FOwner).FIRecordSet.SetProp(prCursorUpdate, FCursorUpdate);
  end;
end;

function TMSDataSetOptions.GetAllFieldsEditable: boolean;
begin
  Result := not SetFieldsReadOnly;
end;

procedure TMSDataSetOptions.SetAllFieldsEditable(const Value: boolean);
begin
  SetFieldsReadOnly := not Value;
end;

procedure TMSDataSetOptions.SetAutoRefresh(Value: boolean);
begin
  if FAutoRefresh <> Value then begin
    FAutoRefresh := Value;
    if not (csDesigning in FOwner.ComponentState) then
      TCustomMSDataSet(FOwner).ChecktAutoRefreshTimer(Value);
  end;
end;

procedure TMSDataSetOptions.SetAutoRefreshInterval(Value: integer);
begin
  if FAutoRefreshInterval <> Value then begin
    TCustomMSDataSet(FOwner).ChecktAutoRefreshTimer(False);
    FAutoRefreshInterval := Value;
    TCustomMSDataSet(FOwner).ChecktAutoRefreshTimer(FAutoRefresh);
  end;
end;

procedure TMSDataSetOptions.SetNonBlocking(Value: boolean);
begin
  if FNonBlocking <> Value then begin
    TCustomMSDataSet(FOwner).CheckInactive;
    FNonBlocking := Value;
    if TCustomMSDataSet(FOwner).FIRecordSet <> nil then
      TCustomMSDataSet(FOwner).FIRecordSet.SetProp(prNonBlocking, FNonBlocking);
  end;
end;

procedure TMSDataSetOptions.SetReflectChangeNotify(const Value: boolean);
begin
  FReflectChangeNotify := Value;
end;

procedure TMSDataSetOptions.SetQueryIdentity(const Value: boolean);
begin
  FQueryIdentity := Value;
  if TCustomMSDataSet(FOwner).FDataSetService <> nil then
    TCustomMSDataSet(FOwner).FDataSetService.SetProp(prQueryIdentity, Value);
end;

procedure TMSDataSetOptions.SetCheckRowVersion(const Value: boolean);
begin
  FCheckRowVersion := Value;
  if TCustomMSDataSet(FOwner).FDataSetService <> nil then
    TCustomMSDataSet(FOwner).FDataSetService.SetProp(prCheckRowVersion, Value);
end;

function TMSDataSetOptions.GetDMLRefresh: boolean;
begin
  Result := TCustomMSDataSet(FOwner).DMLRefresh;
end;

procedure TMSDataSetOptions.SetDMLRefresh(const Value: boolean);
begin
  TCustomMSDataSet(FOwner).DMLRefresh := Value;
end;

procedure TMSDataSetOptions.SetDisableMultipleResults(Value: boolean);
begin
  if FDisableMultipleResults <> Value then begin
    FDisableMultipleResults := Value;
    if TCustomMSDataSet(FOwner).FIRecordSet <> nil then
      TCustomMSDataSet(FOwner).FIRecordSet.SetProp(prDisableMultipleResults, Value);
  end;
end;

procedure TMSDataSetOptions.SetDescribeParams(Value: boolean);
begin
  if Value <> FDescribeParams then begin
    FDescribeParams := Value;
    if TCustomMSDataSet(FOwner).FICommand <> nil then
      TCustomMSDataSet(FOwner).FICommand.SetProp(prUseDescribeParams, Value);
  end;
end;

procedure TMSDataSetOptions.AssignTo(Dest: TPersistent);
begin
  inherited;

  if Dest is TMSDataSetOptions then begin
    TMSDataSetOptions(Dest).LongStrings := LongStrings;
    TMSDataSetOptions(Dest).UniqueRecords := UniqueRecords;
    TMSDataSetOptions(Dest).AllFieldsEditable := AllFieldsEditable;
    TMSDataSetOptions(Dest).CursorUpdate := CursorUpdate;
    TMSDataSetOptions(Dest).FullRefresh := FullRefresh;
    TMSDataSetOptions(Dest).DMLRefresh := DMLRefresh;
    TMSDataSetOptions(Dest).AutoRefresh := AutoRefresh;
    TMSDataSetOptions(Dest).AutoRefreshInterval := AutoRefreshInterval;
    TMSDataSetOptions(Dest).ReflectChangeNotify := ReflectChangeNotify;
    TMSDataSetOptions(Dest).DisableMultipleResults := DisableMultipleResults;
    TMSDataSetOptions(Dest).DescribeParams := DescribeParams;
  end;
end;

{ TCustomMSDataSet}

procedure TCustomMSDataSet.AutoRefreshTimer(Sender: TObject);
{$IFNDEF FPC}
var
  List: {$IFDEF CLR}TObjectList{$ELSE}TList{$ENDIF};
  i: Integer;
{$ENDIF}
begin
  if State = dsBrowse then begin
  {$IFNDEF FPC}
    List := {$IFDEF CLR}TObjectList{$ELSE}TList{$ENDIF}.Create;
    try
      GetDetailDataSets(List);
      for i := 0 to List.Count - 1 do
        if TDataSet(List[i]).State in dsEditModes then
          Exit;
    finally
      List.Free;
    end;
  {$ENDIF}

    try
      if TCustomMSDataSetService(FDataSetService).TimestampField <> nil then
        RefreshQuick(True)
      else
        Refresh;
    except
      Options.AutoRefresh := False;
      raise;
    end;

    // Reset timer
    FAutoRefreshTimer.Enabled := False;
    FAutoRefreshTimer.Enabled := True;
  end;
end;

function TCustomMSDataSet.GetParams: TMSParams;
begin
  Result := TMSParams(inherited Params);
end;

procedure TCustomMSDataSet.SetParams(Value: TMSParams);
begin
  inherited Params := Value;
end;

procedure TCustomMSDataSet.ChecktAutoRefreshTimer(Enabled: boolean);
Begin
  if Enabled then begin
    if not Assigned(FAutoRefreshTimer) then begin
      FAutoRefreshTimer := TWin32Timer.Create(Self);
      FAutoRefreshTimer.OnTimer := AutoRefreshTimer;
      FAutoRefreshTimer.Interval := Options.AutoRefreshInterval * MSecsPerSec;
    end;
    FAutoRefreshTimer.Enabled := True;
  end else begin
    if Assigned(FAutoRefreshTimer) then
      FAutoRefreshTimer.Enabled := False;
    FAutoRefreshTimer.Free;
    FAutoRefreshTimer := nil;  
  end;
end;

constructor TCustomMSDataSet.Create(Owner: TComponent);
begin
  inherited Create(Owner);

  FOptions := inherited Options as TMSDataSetOptions;

  CursorType := ctDefaultResultSet;

  FCommandTimeout := 0;
  FetchAll := True;

  FAutoRefreshTimer := nil;
  ChecktAutoRefreshTimer(Options.AutoRefresh);

  FFileStreamList := TDAList.Create;

{$IFNDEF STD}
  FNeedReflectChanges := False;
{$ENDIF}
end;

destructor TCustomMSDataSet.Destroy;
begin
  ChecktAutoRefreshTimer(False);

  inherited;

  ClearFileStreams;
  FFileStreamList.Free;
end;

procedure TCustomMSDataSet.AssignTo(Dest: TPersistent);
begin
  inherited;

  if Dest is TCustomMSDataSet then begin
    TCustomMSDataSet(Dest).SQLInsert := SQLInsert;
    TCustomMSDataSet(Dest).SQLDelete := SQLDelete;
    TCustomMSDataSet(Dest).SQLUpdate := SQLUpdate;
    TCustomMSDataSet(Dest).SQLRefresh := SQLRefresh;
    TCustomMSDataSet(Dest).SQLLock := SQLLock;
    TCustomMSDataSet(Dest).CursorType := CursorType;

    TCustomMSDataSet(Dest).Params.Assign(Params);
  end;
end;

function TCustomMSDataSet.ServerCursorUsed: boolean;
begin
  Result := (FCursorType in ServerCursorTypes) or
    ((FCursorType = ctBaseTable) and FOptions.FCursorUpdate and not ReadOnly);
end;

{ Open/Close }

procedure TCustomMSDataSet.SetActive(Value: Boolean);
begin
  if Value <> Active then
  try

    inherited;

    if Active and (FIRecordSet.FetchExecutor <> nil) then
      FIRecordSet.FetchExecutor.Resume;

  except
    on E: EDAError do begin
      if (E.Message = SCursorTypeChanged) then
        Unprepare;
      raise;
    end;
  end;
end;

procedure TCustomMSDataSet.DataReopen; 
var
  Field: TField;
  FieldDesc: TOLEDBFieldDesc;
  i: integer; 
begin
  // User-defined type
  for i := 0 to Fields.Count - 1 do begin
    Field := Fields[i];
    if Field.DataType = TFieldType(ftMSUDT) then begin
      Assert(Field is TMSUDTField);
      TMSUDTField(Field).SetUDTTypeInfo('', '', '', ''{$IFNDEF CLR}, nil{$ENDIF});
    end;
  end;

  if ServerCursorUsed then
    ClearBuffers;

  inherited;
  
  // User-defined type
  for i := 0 to Fields.Count - 1 do begin
    Field := Fields[i];
    if Field.DataType = TFieldType(ftMSUDT) then begin
      FieldDesc := TOLEDBFieldDesc(GetFieldDesc(Field));
      Assert(Field is TMSUDTField);
      TMSUDTField(Field).SetUDTTypeInfo(FieldDesc.UDTSchemaname, FieldDesc.UDTName, FieldDesc.UDTCatalogname,
        FieldDesc.AssemblyTypename{$IFNDEF CLR}, FieldDesc.UDTDispatcher{$ENDIF});
    end;
  end;
end;

procedure TCustomMSDataSet.InternalRefresh;
begin
  if Options.NonBlocking and (FIRecordSet.FetchExecutor <> nil) then // break previous fetch 
    FIRecordSet.BreakFetch;

  inherited;

  if Options.NonBlocking and (FIRecordSet.FetchExecutor <> nil) then // start new fetch
    FIRecordSet.FetchExecutor.Resume;
end;

procedure TCustomMSDataSet.InternalRefreshQuick(const CheckDeleted: boolean);
begin
  if Options.NonBlocking and (FIRecordSet.FetchExecutor <> nil) then // waiting until the previous fetch completes,
    FIRecordSet.FetchExecutor.WaitFor;                               // because the RefreshQuick function requires all data to be fetched

  inherited;

  if Options.NonBlocking and (FIRecordSet.FetchExecutor <> nil) then // start new fetch
    FIRecordSet.FetchExecutor.Resume;
end;

procedure TCustomMSDataSet.InternalExecute;
begin
  inherited;

  if TCRRecordSet(Data).CommandType = ctCursor then
  else begin
    Assert(TCRRecordSet(Data).GetCommand.GetCursorState = csInactive);
    TCRRecordSet(Data).GetCommand.SetCursorState(csInactive); // To prevent blocking execute on second exec
  end;
end;

procedure TCustomMSDataSet.InternalOpen;
begin
  if CachedUpdates and ServerCursorUsed then
    DatabaseError(SCUandServerCursors);

  inherited;
  //Assert(not FIRecordSet.NativeRowset or (Length(FSQLObjects) <> 0));

  FNeedAddRef := ServerCursorUsed or UniDirectional;
end;

procedure TCustomMSDataSet.OpenCursor(InfoQuery: boolean);
begin
  if (not Assigned(FIRecordSet) or FIRecordSet.NativeRowset) and (SQL.Count = 0) then
    DatabaseError(SEmptySQLStatement, Self);

{$IFNDEF STD}
  if (FChangeNotification <> nil) and FChangeNotification.Enabled then begin
    BeginConnection;
    FChangeNotification.AddNotificator(Self);
  end;
{$ENDIF}

  inherited;
end;

procedure TCustomMSDataSet.CloseCursor;
begin
  inherited;

  ClearFileStreams;

{$IFNDEF STD}
  if (FChangeNotification <> nil) and FChangeNotification.Enabled then begin
    try
      FChangeNotification.RemoveNotificator(Self);
    except
      if not (csDestroying in ComponentState) then
        raise;
    end;
    EndConnection;
  end;
{$ENDIF}
end;

procedure TCustomMSDataSet.ClearFileStreams;
begin
  while FFileStreamList.Count > 0 do
    TMSFileStream(FFileStreamList[0]).Free;
end;

function TCustomMSDataSet.GetFielDefSize(FieldType: TFieldType; FieldDesc: TFieldDesc): integer;
begin
  Result := inherited GetFielDefSize(FieldType, FieldDesc);
end;

function TCustomMSDataSet.GetFieldType(FieldDesc: TFieldDesc): TFieldType;
begin
  if FieldDesc.SubDataType = dtMSUDT then
    Result := TFieldType(ftMSUDT)
  else
    Result := inherited GetFieldType(FieldDesc);
end;

function TCustomMSDataSet.GetFieldClass(FieldType: TFieldType): TFieldClass;
begin
  if Integer(FieldType) = ftMSXML then
    Result := TMSXMLField
  else
  if Integer(FieldType) = ftMSUDT then
    Result := TMSUDTField 
  else
    Result := inherited GetFieldClass(Fieldtype);
end;

{$IFDEF USE_FTAUTOINC}
function TCustomMSDataSet.GetFieldType(FieldDesc: TFieldDesc): TFieldType;
begin
  Result := inherited GetFieldType(FieldDesc);
  Assert(FieldDesc is TOLEDBFieldDesc);
  if (Result = ftInteger) and TOLEDBFieldDesc(FieldDesc).IsAutoIncrement then
    Result := ftAutoInc;
end;
{$ENDIF}

procedure TCustomMSDataSet.CreateIRecordSet;
begin
  inherited;

  if FIRecordSet = nil then
    SetIRecordSet(TOLEDBRecordSet.Create);
end;

procedure TCustomMSDataSet.SetIRecordSet(Value: TData);
begin
  inherited;

  FIRecordSet := TOLEDBRecordSet(Value);

  if FIRecordSet <> nil then begin
    FICommand := TOLEDBCommand(FIRecordSet.GetCommand);
  {$IFNDEF STD}
    FICommand.SetProp(prNotification, FChangeNotification <> nil);
  {$ENDIF}

    if FOptions <> nil then begin
      FIRecordSet.SetProp(prUniqueRecords, FOptions.FUniqueRecords);
      FIRecordSet.SetProp(prCursorUpdate, FOptions.FCursorUpdate);
      FIRecordSet.SetProp(prDisableMultipleResults, FOptions.FDisableMultipleResults);
      FICommand.SetProp(prUseDescribeParams, FOptions.FDescribeParams);
    end;

    FIRecordSet.SetProp(prCursorType, Variant(CursorType));
    FIRecordSet.SetProp(prCommandTimeout, CommandTimeout);
    FIRecordSet.SetProp(prRequestSQLObjects, True);

    FIRecordSet.CursorTypeChanged := DoCursorTypeChanged;
    FIRecordSet.SetProp(prBulkExecuting, FBulkExecuting);
  end
  else
    FICommand := nil;
end;

function TCustomMSDataSet.GetDataSetServiceClass: TDataSetServiceClass;
begin
  Result := TMSDataSetService;
end;

procedure TCustomMSDataSet.SetDataSetService(Value: TDataSetService);
begin
  inherited;

  FDataSetService := TMSDataSetService(Value);

  if FDataSetService <> nil then begin
    FDataSetService.SetProp(prQueryIdentity, Options.QueryIdentity);
    FDataSetService.SetProp(prCheckRowVersion, Options.CheckRowVersion);
  end;
end;

procedure TCustomMSDataSet.CreateCommand;
begin
  SetCommand(TMSSQL.Create(Self));
end;

function TCustomMSDataSet.CreateOptions: TDADataSetOptions;
begin
  Result := TMSDataSetOptions.Create(Self);
end;

function TCustomMSDataSet.GetConnection: TCustomMSConnection;
begin
  Result := TCustomMSConnection(inherited Connection);
end;

procedure TCustomMSDataSet.SetConnection(Value: TCustomMSConnection);
begin
  inherited Connection := Value;
end;

{$IFDEF WITH_IPROVIDER}
function TCustomMSDataSet.PSGetKeyFields: string;
begin
  if (FIRecordSet <> nil) and FIRecordSet.NativeRowset then
    Result := inherited PSGetKeyFields
  else
    Result := '';
end;
{$ENDIF}

procedure TCustomMSDataSet.SetReadOnly(Value: boolean);
begin
  if ReadOnly <> Value then begin
    if CursorType <> ctDefaultResultSet then
      CheckInactive;
      
    inherited;
  end;
end;

function TCustomMSDataSet.OpenNext: boolean; // Open next rowset in statement. if rowset is not provided then OpenNext return False. If statement has error, then raised exception
begin
  Result := DoOpenNext;
end;

function TCustomMSDataSet.HasNextResultSet: boolean;
begin
  Result := (FIRecordSet <> nil) and not TOLEDBCommand(FIRecordSet.GetCommand).IUnknownNextIsNull;
end;

function TCustomMSDataSet.FindParam(const Value: _string): TMSParam;
begin
  Result := inherited FindParam(GetParamNameWODog(Value)) as TMSParam;
end;

function TCustomMSDataSet.ParamByName(const Value: _string): TMSParam;
begin
  Result := inherited ParamByName(GetParamNameWODog(Value)) as TMSParam;
end;

procedure TCustomMSDataSet.CreateProcCall(Name: _string);
begin
  InternalCreateProcCall(Name, True);
end;

procedure TCustomMSDataSet.Post;
begin
  inherited;

{$IFNDEF STD}
  if FNeedReflectChanges then
    Refresh;
{$ENDIF}
end;

procedure TCustomMSDataSet.Cancel;
begin
  inherited;

{$IFNDEF STD}
  if FNeedReflectChanges then
    Refresh;
{$ENDIF}
end;

function TCustomMSDataSet.SQLGetFrom(SQLText: _string): _string;
begin
  Result := {$IFDEF CLR}Devart.Sdac.{$ENDIF}MSAccess.GetFrom(SQLText);
end;

function TCustomMSDataSet.SQLAddWhere(SQLText, Condition: _string): _string;
begin
  Result := {$IFDEF CLR}Devart.Sdac.{$ENDIF}MSAccess.AddWhere(SQLText, Condition);
end;

function TCustomMSDataSet.SQLDeleteWhere(SQLText: _string): _string;
begin
  Result := {$IFDEF CLR}Devart.Sdac.{$ENDIF}MSAccess.DeleteWhere(SQLText);
end;

function TCustomMSDataSet.SQLGetWhere(SQLText: _string): _string;
begin
  Result := {$IFDEF CLR}Devart.Sdac.{$ENDIF}MSAccess.GetWhere(SQLText);
end;

function TCustomMSDataSet.SQLSetOrderBy(SQLText: _string; Fields: _string): _string;
begin
  Result := {$IFDEF CLR}Devart.Sdac.{$ENDIF}MSAccess.SetOrderBy(SQLText, Fields);
end;

function TCustomMSDataSet.SQLGetOrderBy(SQLText: _string): _string;
begin
  Result := {$IFDEF CLR}Devart.Sdac.{$ENDIF}MSAccess.GetOrderBy(SQLText);
end;

procedure TCustomMSDataSet.BeginConnection(NoConnectCheck: boolean = True);
begin
  if (FIRecordSet = nil) or FIRecordSet.NativeRowset then
    inherited;
end;

procedure TCustomMSDataSet.EndConnection; 
begin
  if (FIRecordSet = nil) or FIRecordSet.NativeRowset then
    inherited;
end;

procedure TCustomMSDataSet.SetOptions(Value: TMSDataSetOptions);
begin
  Options.Assign(Value);
end;

{
procedure TCustomMSDataSet.AssignFieldValue(Param: TParam; Field: TField; Old: boolean);
begin
  inherited;
  if Field.IsNull then
    Param.Clear;
end;
}

function TCustomMSDataSet.GetRecord(Buffer: TRecordBuffer; GetMode: TGetMode; DoCheck: boolean): TGetResult;
begin
  Result := grError;
  if ((FCursorType = ctDynamic) or ((FCursorType = ctBaseTable) and FOptions.FCursorUpdate and not ReadOnly))
    and (BufferCount > 1) then
    DatabaseError(SBookmarksRequired)
  else
    Result := inherited GetRecord(Buffer, GetMode, DoCheck);
end;

procedure TCustomMSDataSet.InternalSetToRecord(Buffer: TRecordBuffer);
begin
  inherited;

  if ServerCursorUsed and (GetBookmarkFlag(Buffer) <> bfInserted) then
    GetRecord(Buffer, gmCurrent, True);
end;

procedure TCustomMSDataSet.DoBeforeExecute;
begin
  inherited;
  
{$IFNDEF STD}
  if (FChangeNotification <> nil) and FChangeNotification.Enabled then begin
    BeginConnection;
    FChangeNotification.AddNotificator(Self);
  end;
{$ENDIF}
end;

procedure TCustomMSDataSet.DoAfterExecute(Result: boolean);
begin
  inherited;

{$IFNDEF STD}
  if Result and (FChangeNotification <> nil) and FChangeNotification.Enabled then
    FChangeNotification.StartNotification(Self);
{$ENDIF}
end;

{ Before / After UpdateExecute }

function TCustomMSDataSet.AssignedBeforeUpdateExecute: boolean;
begin
  Result := Assigned(FBeforeUpdateExecute);
end;

procedure TCustomMSDataSet.DoBeforeUpdateExecute(Sender: TDataSet; StatementTypes: TStatementTypes;
  Params: TDAParams);
begin
  if AssignedBeforeUpdateExecute then
    FBeforeUpdateExecute(Sender as TCustomMSDataSet, StatementTypes, Params as TMSParams);
end;

function TCustomMSDataSet.AssignedAfterUpdateExecute: boolean;
begin
  Result := Assigned(FAfterUpdateExecute);
end;

procedure TCustomMSDataSet.DoAfterUpdateExecute(Sender: TDataSet; StatementTypes: TStatementTypes;
  Params: TDAParams);
begin
  if AssignedAfterUpdateExecute then
    FAfterUpdateExecute(Sender as TCustomMSDataSet, StatementTypes, Params as TMSParams);
end;

function TCustomMSDataSet.Fetched: boolean; 
begin
  if ServerCursorUsed then
    Result := False
  else
    Result := inherited Fetched;
end;

procedure TCustomMSDataSet.DoCursorTypeChanged;
var
  v: Variant;
begin
  // Does not need to read ReadOnly property (for changes to static cursor)
  //  FIRecordSet.GetProp(prReadOnly, @ReadOnly);
  FIRecordSet.GetProp(prCursorType, v);
  FCursorType := TMSCursorType(v);
  raise EDAError.Create(0, SCursorTypeChanged);
end;

function TCustomMSDataSet.PerformLockSQL(SQL: _string; StatementTypes: TStatementTypes): boolean;
var
  OldStrictUpdate: boolean;
begin
  OldStrictUpdate := Options.StrictUpdate;
  try
    Options.StrictUpdate := False;
    Assert(FDataSetService <> nil);
    Result := FDataSetService.FUpdater.PerformSQL(SQL, StatementTypes);
  finally
    Options.StrictUpdate := OldStrictUpdate;
  end;
end;

function ConvertCRParamTypeToBDE(const Value: TParamDirection): TParamType;
begin
  case Value of
    pdInput:
      Result := ptInput;
    pdInputOutput:
      Result := ptInputOutput;
    pdResult:
      Result := ptResult;
    else
      Assert(False, 'Invalid value in ConvertCRParamTypeToBDE(const Value: TParamDirection): TParamType');
      Result := ptUnknown; // To prevent compiler warning
  end;
end;

function TCustomMSDataSet.AllowedCursor(const Value: TMSCursorType): boolean;
begin
  Result := Value <> ctBaseTable;
end;

procedure TCustomMSDataSet.SetCursorType(const Value: TMSCursorType);
begin
  if FCursorType <> Value then begin
    if not AllowedCursor(Value) then
      raise Exception.Create(SBaseTableCursors);

    CheckInactive;
    FCursorType := Value;
    if FIRecordSet <> nil then
      FIRecordSet.SetProp(prCursorType, Variant(CursorType));
  end;
end;

procedure TCustomMSDataSet.SetCommandTimeout(const Value: integer);
begin
  if FCommandTimeout <> Value then begin
    FCommandTimeout := Value;
    if FIRecordSet <> nil then
      FIRecordSet.SetProp(prCommandTimeout, FCommandTimeout);
  end;
end;

procedure TCustomMSDataSet.BreakExec;
begin
  if FIRecordSet <> nil then begin
    Assert(FICommand <> nil);
    if FICommand.Executing then
      FICommand.BreakExec;
    FIRecordSet.BreakFetch;
  end;  
end;

procedure TCustomMSDataSet.RefreshQuick(const CheckDeleted: boolean);
begin
  InternalRefreshQuick(CheckDeleted);
end;

procedure TCustomMSDataSet.LockTable(LockType: TMSLockType);
begin
  CheckActive;

  //if FetchAll = False then
  //  DatabaseError('SLockVsFetchAll');
  if not Connection.InTransaction then
    DatabaseError(SMustBeInTransaction);

  Assert(FDataSetService <> nil);
  FDataSetService.SetProp(prLockObject, Variant(loTable));
  FDataSetService.SetProp(prLockType, Variant(LockType));
  PerformLockSQL('', [stLock]);
end;

procedure TCustomMSDataSet.Lock;
begin
  Lock(ltUpdate);
end;

procedure TCustomMSDataSet.Lock(LockType: TMSLockType);
begin
  CheckActive;

  //if FetchAll = False then
  //  DatabaseError('SLockVsFetchAll');

  if not Connection.InTransaction then
    DatabaseError(SMustBeInTransaction);

  Assert(FDataSetService <> nil);
  FDataSetService.SetProp(prLockObject, Variant(loRow));
  FDataSetService.SetProp(prLockType, Variant(LockType));
  inherited Lock;
end;

procedure TCustomMSDataSet.CheckInactive;
begin
  inherited;
end;

function TCustomMSDataSet.GetUpdateObject: TMSUpdateSQL;
begin
  Result := TMSUpdateSQL(inherited UpdateObject);
end;

procedure TCustomMSDataSet.SetUpdateObject(Value: TMSUpdateSQL);
begin
  inherited UpdateObject := Value;
end;

procedure TCustomMSDataSet.DoBeforeOpenCursor;
begin
  if SQL.Count = 0 then
    DatabaseError(SEmptySQLStatement, Self);

  if Active then
    Close;

  // get param values from master dataset
  if not Foptions.LocalMasterDetail then
    SetMasterParams(Params);
end;

{$IFNDEF VER6P}
procedure RaiseLastOSError;
begin
  RaiseLastWin32Error;
end;
{$ENDIF}

function TCustomMSDataSet.GetFileStreamForField(const FieldName: _string;
  const DesiredAccess: TMSSqlFilestreamDesiredAccess = daReadWrite;
  const OpenOptions: TMSSqlFilestreamOpenOptions = [];
  const AllocationSize: Int64 = 0): TMSFileStream;

const
  FILESTREAM_OPEN_FLAGS: array[TMSSqlFilestreamOpenOption] of ULONG = (
    SQL_FILESTREAM_OPEN_FLAG_ASYNC,
    SQL_FILESTREAM_OPEN_FLAG_NO_BUFFERING,
    SQL_FILESTREAM_OPEN_FLAG_NO_WRITE_THROUGH,
    SQL_FILESTREAM_OPEN_FLAG_SEQUENTIAL_SCAN,
    SQL_FILESTREAM_OPEN_FLAG_RANDOM_ACCESS);
var
  PathName: WideString;
  TrContext: variant;
  TrContextLen: LongWord;
  PData: {$IFDEF CLR}TBytes{$ELSE}Pointer{$ENDIF};
  o: TMSSqlFilestreamOpenOption;
  UOptions: ULONG;
  PAllocationSize: IntPtr;
  Handle: HResult;
  i: Integer;
begin
  CheckActive;
  if not Connection.InTransaction then
    raise Exception.Create(SNotInTransaction);

  BeginConnection;
  try
    FDataSetService.GetFilestreamContext(FieldName, PathName, TrContext);
  finally
    EndConnection;
  end;

  PAllocationSize := nil;
  TrContextLen := VarArrayHighBound(TrContext, 1) - VarArrayLowBound(TrContext, 1) + 1;
{$IFDEF CLR}
  PData := TrContext;
{$ELSE}
  PData := VarArrayLock(TrContext);
{$ENDIF}
  try
    UOptions := 0;
    for o := Low(TMSSqlFilestreamOpenOption) to High(TMSSqlFilestreamOpenOption) do
      if o in OpenOptions then
        UOptions := UOptions or FILESTREAM_OPEN_FLAGS[o];

    if AllocationSize > 0 then begin
      PAllocationSize := Marshal.AllocHGlobal(SizeOf(Int64));
      Marshal.WriteInt64(PAllocationSize, AllocationSize);
    end;

    Handle := OpenSqlFilestream(PWideChar(PathName), Integer(DesiredAccess), UOptions, PData, TrContextLen, PAllocationSize);
    if Handle = -1 then
      RaiseLastOSError;
  finally
  {$IFNDEF CLR}
    VarArrayUnlock(TrContext);
  {$ENDIF}
    Marshal.FreeHGlobal(PAllocationSize);
  end;

  for i := 0 to FFileStreamList.Count - 1 do
    if Handle = TMSFileStream(FFileStreamList[i]).Handle then begin
      Result := TMSFileStream(FFileStreamList[i]);
      Exit;
    end;

  Result := TMSFileStream.Create(Handle);
  Result.FOwner := FFileStreamList;
  FFileStreamList.Add(Result);
end;

{$IFNDEF STD}
procedure TCustomMSDataSet.SetChangeNotification(Value: TMSChangeNotification);
begin
  if FChangeNotification <> nil then
    FChangeNotification.RemoveDataSet(Self);

  FChangeNotification := Value;

  if FChangeNotification <> nil then begin
    CheckInactive;
    FChangeNotification.AddDataSet(Self);
  end;

  if FICommand <> nil then
    FICommand.SetProp(prNotification, FChangeNotification <> nil);
end;

procedure TCustomMSDataSet.ReflectChanges(NotificationInfo: TMSNotificationInfo;
  NotificationSource: TMSNotificationSource; NotificationType: TMSNotificationType);
begin
  case NotificationSource of
    nsData:
      if NotificationInfo in [niTruncate, niInsert, niUpdate, niDelete] then
        if State in [dsInsert, dsEdit] then
          FNeedReflectChanges := True
        else begin
          if (TCustomMSDataSetService(FDataSetService).TimestampField = nil) or (NotificationInfo = niTruncate) then
            Refresh
          else
            if NotificationInfo = niDelete then
              RefreshQuick(True)
            else
              RefreshQuick(False);
          FNeedReflectChanges := False;
        end;
    nsTimeout:
      DatabaseError(SSubscriptionTimedOut);
    nsObject:
      case NotificationInfo of
        niDrop:
          DatabaseError(SQNObjectDropped);
        niAlter:
          DatabaseError(SQNObjectAltered);
      end;
    nsSystem:
      case NotificationInfo of
        niError:
          DatabaseError(SSQLInternalError);
        niResource:
          DatabaseError(SSubscriptionRemoved);
      end;
    nsStatement:
      case NotificationInfo of
        niQuery:
          DatabaseError(SInvalidQNStatement);
        niInvalid:
          DatabaseError(SStatementNotSupported);
        niPreviousFire:
          DatabaseError(SSPreviousInvalid);
        niOptions:
          DatabaseError(SInvalidQNSetOptions);
        niIsolation:
          DatabaseError(SInvalidQNIsolation);
        niTemplateLimit:
          DatabaseError(STemplateLimit);
      end;
  end;
end;
{$ENDIF}

{ TMSUpdateSQL }

function TMSUpdateSQL.DataSetClass: TCustomDADataSetClass;
begin
  Result := TCustomMSDataSet;
end;

function TMSUpdateSQL.SQLClass: TCustomDASQLClass;
begin
  Result := TMSSQL;
end;

{ TMSQuery }

procedure TMSQuery.SetIRecordSet(Value: TData);
begin
  inherited;

  FIRecordSet := TOLEDBRecordSet(Value);
end;

{ TMSTable }

{$IFDEF WITH_IPROVIDER}
function TCustomMSTable.PSGetTableName: string;
begin
  Result := TableName;
end;

procedure TCustomMSTable.PSSetParams(AParams: DB.TParams);
var
  St: _string;
  i: integer;
begin
  if (Params.Count <> AParams.Count) then begin
    SQL.Text := '';
    St := '';

    for i := 0 to AParams.count - 1 do begin
      if St <> '' then
        St := St + ' AND ';
      St := AParams[i].Name + ' = :' + AParams[i].Name;
    end;

    PrepareSQL;

    if St <> '' then
      AddWhere(St);
  end;

  inherited;
end;

{$IFDEF VER5P}
procedure TCustomMSTable.PSSetCommandText(const CommandText: _string);
begin
  if CommandText <> '' then
    TableName := CommandText;
end;
{$ENDIF}
{$ENDIF}

procedure TCustomMSTable.SetTableName(const Value: _string);
begin
  if not (csReading in ComponentState) then
    Active := False;
  FTableName := OLEDBSQLInfo.NormalizeName(Trim(Value), False, True);

  SQL.Clear;

  if FIRecordSet <> nil then
    FIRecordSet.SetProp(prBaseTableName, FTableName);
end;

procedure TCustomMSTable.SetOrderFields(Value: _string);
var
  OldActive: boolean;
begin
  Value := Trim(Value);
  if Value <> FOrderFields then begin
    FOrderFields := Value;
    OldActive := Active;
    if not (csLoading in ComponentState) then
      SQL.Text := '';

    if OldActive then
      Open;
  end;
end;

procedure TCustomMSTable.PrepareSQL;
var
  MasterPos: integer;
  MasterName: _string;
  Param: TDAParam;
begin
  //WAR TCustomMSStoredProc.PrepareSQL and TCustomMSTable.PrepareSQL is based on different principies and work in different ways
  if TableName = '' then
    DatabaseError(STableNameNotDefined);

  if SQL.Count = 0 then begin // TableName = [TableName]
    CheckDataSetService;
    SQL.Text := FDataSetService.SQLGenerator.GenerateTableSQL(TableName, OrderFields);
  end;

  // CR 8883
  // for TMSQuery second MD-way is more useful
  if (DataSource <> nil) and (FMasterFields <> '') and (FDetailFields <> '') and not DataSource.DataSet.Active// see TCustomDADataSet.GetFinalSQL
  then begin
    MasterPos := 1;
    while True do begin
      MasterName := ExtractFieldName(FMasterFields, MasterPos);
      if MasterName <> '' then begin
        Param := Params.FindParam(MasterName);
        if Param <> nil then
          Param.DataType := ftString;
      end
      else
        break;
    end;
  end;
end;

procedure TCustomMSTable.Prepare;
begin
  PrepareSQL;

  inherited;
end;

function TCustomMSTable.AllowedCursor(const Value: TMSCursorType): boolean;
begin
  Result := True;
end;

procedure TCustomMSTable.SetIRecordSet(Value: TData);
begin
  inherited;

  if FIRecordSet <> nil then
    FIRecordSet.SetProp(prBaseTableName, FTableName);
end;

procedure TCustomMSTable.OpenCursor(InfoQuery: boolean);
begin
  PrepareSQL;

  inherited;
end;

procedure TCustomMSTable.CheckSQL;
begin
  PrepareSQL;
end;

procedure TCustomMSTable.Execute;
begin
  PrepareSQL;

  if FCursorType = ctBaseTable then
    Open
  else
    inherited;
end;

procedure TCustomMSTable.AssignTo(Dest: TPersistent);
begin
  inherited;

  if Dest is TCustomMSTable then begin
    TCustomMSTable(Dest).OrderFields := OrderFields;
    TCustomMSTable(Dest).TableName := TableName;
    TCustomMSTable(Dest).MasterSource := MasterSource;
    TCustomMSTable(Dest).MasterFields := MasterFields;
    TCustomMSTable(Dest).DetailFields := DetailFields;
  end;
end;

function TCustomMSTable.SQLAutoGenerated: boolean;
begin
  Result := True;
end;

{ TCustomMSStoredProc }

{$IFDEF WITH_IPROVIDER}
{$IFDEF VER5P}

procedure TCustomMSStoredProc.PSSetCommandText(const CommandText: string);
begin
  if CommandText <> '' then
    StoredProcName := CommandText;
end;

{$ENDIF}
{$ENDIF}

procedure TCustomMSStoredProc.SetIRecordSet(Value: TData);
begin
  inherited;
end;

procedure TCustomMSStoredProc.SetStoredProcName(const Value: _string);
begin
  if Value <> FStoredProcName then begin
    SQL.Text := '';

    FStoredProcName := Trim(Value);

    if (Connection <> nil) and Connection.Connected and (FStoredProcName <> '') then
      PrepareSQL;

    if FICommand <> nil then
      FICommand.SetProp(prIsStoredProc, True);
  end;
end;

function TCustomMSStoredProc.SQLAutoGenerated: boolean;
begin
  Result := True;
end;

procedure TCustomMSStoredProc.PrepareSQL;
begin
  //WAR TCustomMSStoredProc.PrepareSQL and TCustomMSTable.PrepareSQL is based on different principies and work in different ways

  if SQL.Text = '' then begin
    if StoredProcName = '' then
      DatabaseError(SStoredProcNotDefined);

     InternalCreateProcCall(StoredProcName, Params.Count = 0);
  end;
end;

procedure TCustomMSStoredProc.Prepare;
begin
  PrepareSQL;

  inherited;
end;

procedure TCustomMSStoredProc.DoBeforeExecute;
begin
  if not Prepared then
    PrepareSQL;

  inherited;
end;

procedure TCustomMSStoredProc.BeforeOpenCursor(InfoQuery: boolean);
begin
  PrepareSQL;

  inherited;
end;

procedure TCustomMSStoredProc.ExecProc;
begin
  Execute;
end;

procedure TCustomMSStoredProc.AssignTo(Dest:TPersistent);
var
  I: Integer;
  P: TMSParam;
begin
  inherited;

  if Dest is TCustomMSStoredProc then begin
    TCustomMSStoredProc(Dest).StoredProcName := FStoredProcName;

    for I := 0 to Params.Count - 1 do begin
      P := TCustomMSStoredProc(Dest).FindParam(Params[I].Name);
      if (P <> nil) and (P.DataType = Params[I].DataType) then begin
        P.Assign(Params[I]);
      end;
    end;
  end;
end;

{ TMSMetadata }

constructor TMSMetadata.Create(Owner: TComponent);
begin
  inherited;

  Debug := False;
  ReadOnly := True;
  FRestrictions := nil;
  FIMetaData := nil;
end;

destructor TMSMetadata.Destroy;
begin
  Close;
  FRestrictions.Free;
  FIMetaData.Free;

  inherited;
end;

procedure TMSMetadata.SetDatabaseName(Value: _string);
begin
  Active := False;
  FDatabaseName := Value;
end;

procedure TMSMetadata.SetSchemaName(Value: _string);    
begin
  Active := False;
  FSchemaName := Value;
end;

procedure TMSMetadata.SetObjectType(Value: TMSObjectType);    
begin
  Active := False;
  FieldDefs.Updated := False;
  FObjectType := Value;
end;

procedure TMSMetadata.SetTableName(Value: _string);    
begin
  Active := False;
  FTableName := Value;
end;

procedure TMSMetadata.SetStoredProcName(Value: _string);    
begin
  Active := False;
  FStoredProcName := Value;
end;

procedure TMSMetadata.SetColumnName(Value: _string);    
begin
  Active := False;
  FColumnName := Value;
end;

procedure TMSMetadata.SetIndexName(Value: _string);    
begin
  Active := False;
  FIndexName := Value;
end;

procedure TMSMetadata.SetConstraintName(Value: _string);    
begin
  Active := False;
  FConstraintName := Value;
end;

procedure TMSMetadata.SetLinkedServer(Value: _string);
begin
  Active := False;
  FLinkedServer := Value;
end;

procedure TMSMetadata.SetAssemblyName(Value: _string);
begin
  Active := False;
  FAssemblyName := Value;
end;

procedure TMSMetadata.SetAssemblyID(Value: integer);
begin
  Active := False;
  FAssemblyID := Value;
end;

procedure TMSMetadata.SetReferencedAssemblyID(Value: integer);
begin
  Active := False;
  FReferencedAssemblyID := Value;
end;

procedure TMSMetadata.SetUDTName(Value: _string);
begin
  Active := False;
  FUDTName := Value;
end;

procedure TMSMetadata.SetSchemaCollectionName(Value: _string);
begin
  Active := False;
  FSchemaCollectionName := Value;
end;

procedure TMSMetadata.SetTargetNamespaceURI(Value: _string);
begin
  Active := False;
  FTargetNamespaceURI := Value;
end;

procedure TMSMetadata.SetStatisticsName(Value: _string);
begin
  Active := False;
  FStatisticsName := Value;
end;

procedure TMSMetadata.SetTableTypeName(Value: _string);
begin
  Active := False;
  FTableTypeName := Value;
end;

function TMSMetadata.RequestIRowset: IRowset;
  function GetTableType: string;
  begin
    case ObjectType of
      otAliases, otAliasesInfo:
        Result := 'ALIAS';
      otTables, otTablesInfo: 
        Result := 'TABLE';
      otSynonyms, otSynonymsInfo: 
        Result := 'SYNONYM';
      otSystemTables, otSystemTablesInfo: 
        Result := 'SYSTEM TABLE';
      otViews, otViewsInfo:
        Result := 'VIEW';
      otGlobalTempTables, otGlobalTempTablesInfo:
        Result := 'GLOBAL TEMPORARY';
      otLocalTempTables, otLocalTempTablesInfo:
        Result := 'LOCAL TEMPORARY';
      otSystemViews, otSystemViewsInfo:
        Result := 'SYSTEM VIEW';
      otExternalTablesInfo:
        Result := 'EXTERNAL TABLE';
    end;
  end;

var
  Schema: TGUID;
  rgRestrictions: TRestrictions;
begin
  case ObjectType of
    otDatabases: begin
      SetLength(rgRestrictions, 1);
      Schema := DBSCHEMA_CATALOGS;
      rgRestrictions[0] := DatabaseName;
    end;
    otColumnPrivileges: begin
      SetLength(rgRestrictions, 6);
      Schema := DBSCHEMA_COLUMN_PRIVILEGES;
      rgRestrictions[0] := DatabaseName;
      rgRestrictions[1] := SchemaName;
      rgRestrictions[2] := TableName;
      rgRestrictions[3] := ColumnName;
    end;
    otColumns: begin
      SetLength(rgRestrictions, 4);
      Schema := DBSCHEMA_COLUMNS;
      if Connection.Options.Provider <> prCompact then begin
        rgRestrictions[0] := DatabaseName;
        rgRestrictions[1] := SchemaName;
      end;
      rgRestrictions[2] := TableName;
      rgRestrictions[3] := ColumnName;
    end;
    otForeignKeys: begin
      SetLength(rgRestrictions, 6);
      Schema := DBSCHEMA_FOREIGN_KEYS;
      rgRestrictions[3] := DatabaseName;
      rgRestrictions[4] := SchemaName;
      rgRestrictions[5] := TableName;
    end;
    otPrimaryKeys: begin
      if Connection.Options.Provider <> prCompact then begin
        SetLength(rgRestrictions, 3);
        rgRestrictions[0] := DatabaseName;
        rgRestrictions[1] := SchemaName;
        rgRestrictions[2] := TableName;
        Schema := DBSCHEMA_PRIMARY_KEYS;
      end
      else begin
        SetLength(rgRestrictions, 7);
        rgRestrictions[2] := ConstraintName;
        rgRestrictions[5] := TableName;
        // Other Restriction columns not supported
        Schema := DBSCHEMA_KEY_COLUMN_USAGE;
      end;
    end;
    otIndexes: begin
      SetLength(rgRestrictions, 5);
      Schema := DBSCHEMA_INDEXES;
      if Connection.Options.Provider <> prCompact then begin
        rgRestrictions[0] := DatabaseName;
        rgRestrictions[1] := SchemaName;
      end;
      rgRestrictions[2] := IndexName;
      rgRestrictions[4] := TableName;
    end;
    otServerTypes: begin
      SetLength(rgRestrictions, 0);
      Schema := DBSCHEMA_PROVIDER_TYPES;
    end;
    otSchemata: begin
      SetLength(rgRestrictions, 2);
      Schema := DBSCHEMA_SCHEMATA;
      rgRestrictions[0] := DatabaseName;
      rgRestrictions[1] := SchemaName;
    end;
    otStatistics: begin
      SetLength(rgRestrictions, 3);
      Schema := DBSCHEMA_STATISTICS;
      rgRestrictions[0] := DatabaseName;
      rgRestrictions[1] := SchemaName;
      rgRestrictions[2] := TableName;
    end;
    otStoredProcs: begin
      SetLength(rgRestrictions, 3);
      Schema := DBSCHEMA_PROCEDURES;
      rgRestrictions[0] := DatabaseName;
      rgRestrictions[1] := SchemaName;
      rgRestrictions[2] := StoredProcName;
    end;
    otStoredProcParams: begin
      SetLength(rgRestrictions, 3);
      Schema := DBSCHEMA_PROCEDURE_PARAMETERS;
      rgRestrictions[0] := DatabaseName;
      rgRestrictions[1] := SchemaName;
      rgRestrictions[2] := StoredProcName;
    end;
    otAliases, otTables, otSynonyms, otSystemTables, otViews, otGlobalTempTables, otLocalTempTables, otSystemViews,
    otAliasesInfo, otTablesInfo, otSynonymsInfo, otSystemTablesInfo, otViewsInfo, otGlobalTempTablesInfo, otLocalTempTablesInfo, otExternalTablesInfo, otSystemViewsInfo:
    begin
      SetLength(rgRestrictions, 4);

      if ObjectType in [otAliases, otTables, otSynonyms, otSystemTables, otViews, otGlobalTempTables, otLocalTempTables, otSystemViews] then
        Schema := DBSCHEMA_TABLES
      else
        Schema := DBSCHEMA_TABLES_INFO;
      if Connection.Options.Provider <> prCompact then begin
        rgRestrictions[0] := DatabaseName;
        rgRestrictions[1] := SchemaName;
      end;
      rgRestrictions[2] := TableName;
      rgRestrictions[3] := GetTableType;
    end;
    otTableConstraints: begin
      SetLength(rgRestrictions, 6);
      Schema := DBSCHEMA_TABLE_CONSTRAINTS;
      if Connection.Options.Provider <> prCompact then begin
        rgRestrictions[0] := DatabaseName;
        rgRestrictions[1] := SchemaName;
      end;
      rgRestrictions[2] := ConstraintName;
      rgRestrictions[5] := TableName;
    end;
    otTablePrivileges: begin
      SetLength(rgRestrictions, 3);
      Schema := DBSCHEMA_TABLE_PRIVILEGES;
      rgRestrictions[0] := DatabaseName;
      rgRestrictions[1] := SchemaName;
      rgRestrictions[2] := TableName;
    end;
    otLinkedServers: begin
      SetLength(rgRestrictions, CRESTRICTIONS_DBSCHEMA_LINKEDSERVERS{1});
      Schema := DBSCHEMA_LINKEDSERVERS;
      rgRestrictions[0] := LinkedServer;
    end;
    otAssemblies: begin
      SetLength(rgRestrictions, CRESTRICTIONS_DBSCHEMA_SQL_ASSEMBLIES{4});
      Schema := DBSCHEMA_SQL_ASSEMBLIES;
      rgRestrictions[0] := DatabaseName;
      rgRestrictions[1] := SchemaName;
      rgRestrictions[2] := AssemblyName;
      if AssemblyID <> 0 then
        rgRestrictions[3] := AssemblyID;
    end;
    otAssemblyDependencies: begin
      SetLength(rgRestrictions, CRESTRICTIONS_DBSCHEMA_SQL_ASSEMBLY_DEPENDENCIES{4});
      Schema := DBSCHEMA_SQL_ASSEMBLY_DEPENDENCIES;
      rgRestrictions[0] := DatabaseName;
      rgRestrictions[1] := SchemaName;
      if AssemblyID <> 0 then
        rgRestrictions[2] := AssemblyID;
      if ReferencedAssemblyID <> 0 then
        rgRestrictions[3] := ReferencedAssemblyID;
    end;
    otUserTypes: begin
      SetLength(rgRestrictions, CRESTRICTIONS_DBSCHEMA_SQL_USER_TYPES{3});
      Schema := DBSCHEMA_SQL_USER_TYPES;
      rgRestrictions[0] := DatabaseName;
      rgRestrictions[1] := SchemaName;
      rgRestrictions[2] := UDTName;
    end;
    otXMLCollections: begin
      SetLength(rgRestrictions, CRESTRICTIONS_DBSCHEMA_XML_COLLECTIONS{4});
      Schema := DBSCHEMA_XML_COLLECTIONS;
      rgRestrictions[0] := DatabaseName;
      rgRestrictions[1] := SchemaName;
      rgRestrictions[2] := SchemaCollectionName;
      rgRestrictions[3] := TargetNamespaceURI;
    end;
    otCheckConstraints: begin
      SetLength(rgRestrictions, 3);
      Schema := DBSCHEMA_CHECK_CONSTRAINTS;
      rgRestrictions[0] := DatabaseName;
      rgRestrictions[1] := SchemaName;
      rgRestrictions[2] := ConstraintName;
    end;
    otCheckConstraintsByTable: begin
      SetLength(rgRestrictions, 6);
      Schema := DBSCHEMA_CHECK_CONSTRAINTS_BY_TABLE;
      rgRestrictions[0] := DatabaseName;
      rgRestrictions[1] := SchemaName;
      rgRestrictions[2] := TableName;
      rgRestrictions[3] := DatabaseName;
      rgRestrictions[4] := SchemaName;
      rgRestrictions[5] := ConstraintName;
    end;
    otTableStatistics: begin
      SetLength(rgRestrictions, 7);
      Schema := DBSCHEMA_TABLE_STATISTICS;
      rgRestrictions[0] := DatabaseName;
      rgRestrictions[1] := SchemaName;
      rgRestrictions[2] := TableName;
      rgRestrictions[3] := DatabaseName;
      rgRestrictions[4] := SchemaName;
      rgRestrictions[5] := StatisticsName;
    end;
    otConstraintColumnUsage: begin
      SetLength(rgRestrictions, 7);
      Schema := DBSCHEMA_CONSTRAINT_COLUMN_USAGE;
      rgRestrictions[0] := DatabaseName;
      rgRestrictions[1] := SchemaName;
      rgRestrictions[2] := TableName;
      rgRestrictions[3] := ColumnName;
      rgRestrictions[6] := ConstraintName;
    end;
    otTableTypes: begin
      SetLength(rgRestrictions, 3);
      Schema := DBSCHEMA_SQL_TABLE_TYPES;
      if Connection.Options.Provider <> prCompact then begin
        rgRestrictions[0] := DatabaseName;
        rgRestrictions[1] := SchemaName;
      end;
      rgRestrictions[2] := TableTypeName;
    end;
    otTableTypePrimaryKeys: begin
      SetLength(rgRestrictions, 3);
      Schema := DBSCHEMA_SQL_TABLE_TYPE_PRIMARY_KEYS;
      rgRestrictions[0] := DatabaseName;
      rgRestrictions[1] := SchemaName;
      rgRestrictions[2] := TableTypeName;
    end;
    otTableTypeColumns: begin
      SetLength(rgRestrictions, 4);
      Schema := DBSCHEMA_SQL_TABLE_TYPE_COLUMNS;
      if Connection.Options.Provider <> prCompact then begin
        rgRestrictions[0] := DatabaseName;
        rgRestrictions[1] := SchemaName;
      end;
      rgRestrictions[2] := TableTypeName;
      rgRestrictions[3] := ColumnName;
    end;
  end;

  Result := FIRecordSet.GetSchemaRowset(Schema, rgRestrictions);
end;

procedure TMSMetadata.RequestRecordSet;
const
  DBSchemaRowsetsSupportedBySQLAzure =
    [otAliases, otDatabases, otGlobalTempTables, otLocalTempTables,
    otSynonyms, otSystemTables, otSystemViews, otTables, otViews];
var
  Rowset: IRowset;
  RecordSet: TOLEDBRecordSet;
  MetaDataKind: _string;
begin
  BeginConnection;
  try
    if GetConnection.IConnection.IsSQLAzureEdition and not (ObjectType in DBSchemaRowsetsSupportedBySQLAzure) then begin
      case ObjectType of
        otColumns:
          MetaDataKind := 'columns';
        otStoredProcs:
          MetaDataKind := 'procedures';
        otStoredProcParams:
          MetaDataKind := 'procedureparameters';
        otIndexes:
          MetaDataKind := 'indexcolumns';
        otTableConstraints:
          MetaDataKind := 'constraints';
      else
        raise Exception.Create(SAzureNotSupportMetaDataKind);
      end;

      if FIMetaData = nil then begin
        FIMetaData := TOLEDBMetaData.Create;
        FRestrictions := _TStringList.Create;
      end;

      FRestrictions.Values['TABLE_CATALOG'] := DatabaseName;
      FRestrictions.Values['TABLE_SCHEMA'] := SchemaName;
      FRestrictions.Values['TABLE_NAME'] := TableName;
      FRestrictions.Values['COLUMN_NAME'] := ColumnName;
      FRestrictions.Values['PROCEDURE_CATALOG'] := DatabaseName;
      FRestrictions.Values['PROCEDURE_SCHEMA'] := SchemaName;
      FRestrictions.Values['PROCEDURE_NAME'] := StoredProcName;
      FRestrictions.Values['INDEX_NAME'] := IndexName;
      FRestrictions.Values['CONSTRAINT_NAME'] := ConstraintName;

      RecordSet := FIMetaData.GetMetaData(TDBAccessUtils.GetIConnection(UsedConnection),
        TDBAccessUtils.GetITransaction(UsedTransaction), MetaDataKind, FRestrictions) as TOLEDBRecordSet;
      FreeIRecordSet;
      SetIRecordSet(RecordSet);
      SQL.Add('');
      FNativeRecordset := False;
    end
    else begin
      Rowset := RequestIRowset;
      FNativeRecordset := True;
      FIRecordSet.SetIRowset(Rowset, False);
      TCRRecordSet(FIRecordSet).GetCommand.SetCursorState(csInactive);
    end;
  finally
    EndConnection;
  end;
end;

procedure TMSMetadata.InternalExecute;
begin
  RequestRecordSet;

  inherited;
end;

procedure TMSMetadata.OpenCursor(InfoQuery: boolean);
begin
  RequestRecordSet;

  inherited;
end;

procedure TMSMetadata.CloseCursor;
begin
  inherited;

  if not FNativeRecordset then begin
    SetIRecordSet(nil);
    FNativeRecordset := True;
  end;
end;

{ TMSSQL }

constructor TMSSQL.Create(Owner: TComponent);
begin
  inherited;

  FAutoCommit := True;
  FCommandTimeout := 0;
  Macros.SetParserClass(TMSParser);
end;

function TMSSQL.GetPermitPrepare: boolean;
begin
  Result := False;
end;

procedure TMSSQL.SetPermitPrepare(Value: boolean);
begin
end;

procedure TMSSQL.CreateICommand;
begin
  inherited;

  if FICommand = nil then
    SetICommand(TOLEDBCommand.Create);
end;

procedure TMSSQL.SetICommand(Value: TCRCommand);
begin
  FICommand := TOLEDBCommand(Value);

  if FICommand <> nil then begin
    FICommand.SetProp(prCommandTimeout, CommandTimeout);
    FICommand.SetProp(prNonBlocking, NonBlocking);
    FICommand.SetProp(prUseDescribeParams, DescribeParams);
  end;

  inherited;
end;

procedure TMSSQL.InternalPrepare; 
begin
  if SQL.Count = 0 then
    DatabaseError(SEmptySQLStatement, Self);

  WriteParams;
    
  inherited;
end;

procedure TMSSQL.InternalExecute(Iters: integer); 
begin
  inherited;
end;

function TMSSQL.GetConnection: TCustomMSConnection;
begin
  Result := TCustomMSConnection(inherited Connection);
  if (FDataSet <> nil) and (Result = nil) then
    Result := TCustomMSConnection(FDataset.Connection);
end;

procedure TMSSQL.SetConnection(Value: TCustomMSConnection);
begin
  inherited Connection := Value;
end;

function TMSSQL.GetParams: TMSParams;
begin
  Result := TMSParams(inherited Params);
end;

procedure TMSSQL.SetParams(Value: TMSParams);
begin
  inherited Params := Value;
end;

procedure TMSSQL.AssignTo(Dest: TPersistent); 
begin
  inherited;

  if Dest is TMSSQL then begin
    TMSSQL(Dest).Params.Assign(Params);
    TMSSQL(Dest).NonBlocking := NonBlocking;
    TMSSQL(Dest).BeforeExecute := BeforeExecute;
    TMSSQL(Dest).AfterExecute := AfterExecute;
  end;
end;

function TMSSQL.GetDataTypesMap: TDataTypesMapClass;
begin
  Result := TMSDataTypesMap;
end;

function TMSSQL.CreateParamsObject: TDAParams;
begin
  Result := TMSParams.Create(Self);
end;

function TMSSQL.FindResultParam: TDAParam;
begin
  Result := Params.FindParam('Result');
  if Result <> nil then begin
    Result.ParamType := ptInputOutput;
    if Result.DataType = ftUnknown then
      Result.DataType := ftInteger;
  end;    
end;

function TMSSQL.FindParam(const Value: _string): TMSParam;
begin
  Result := inherited FindParam(GetParamNameWODog(Value)) as TMSParam;
end;

function TMSSQL.ParamByName(const Value: _string): TMSParam;
begin
  Result := inherited ParamByName(GetParamNameWODog(Value)) as TMSParam;
end;

procedure TMSSQL.AssignParamDesc(Param: TDAParam; ParamDesc: {$IFDEF CLR}Devart.Dac.{$ENDIF}CRAccess.TParamDesc);
begin
  inherited;

  TMSParam(Param).FTableTypeName := TOLEDBParamDesc(ParamDesc).TableTypeName;
end;

procedure TMSSQL.SetCommandTimeout(const Value: integer);
begin
  if FCommandTimeout <> Value then begin
    FCommandTimeout := Value;
    if FICommand <> nil then
      FICommand.SetProp(prCommandTimeout, FCommandTimeout);
  end;
end;

procedure TMSSQL.SetNonBlocking(const Value: boolean);
begin
  if FNonBlocking <> Value then begin
    FNonBlocking := Value;
    if FICommand <> nil then
      FICommand.SetProp(prNonBlocking, FNonBlocking);
  end;
end;

procedure TMSSQL.SetDescribeParams(Value: boolean);
begin
  if Value <> FDescribeParams then begin
    FDescribeParams := Value;
    if FICommand <> nil then
      FICommand.SetProp(prUseDescribeParams, FDescribeParams);
  end;
end;

function TMSSQL.UsedConnection: TCustomDAConnection;
begin
  Result := Connection;
end;

procedure TMSSQL.Execute(Iters: integer);
begin
  if NonBlocking and Executing then
    DatabaseError(SAsynchExecuting);

  inherited Execute(Iters);
end;

procedure TMSSQL.BreakExec;
begin
  if FICommand <> nil then
    FICommand.BreakExec;
end;

procedure TMSSQL.UnPrepare;
begin
{  if Executing then
    BreakExec;
  Assert(not Executing);}

  inherited;
end;

procedure TMSSQL.ExecuteForXML(out XML: String);
var
  Stream: TStringStream;
begin
  Stream := TStringStream.Create(''{$IFDEF CLR}{$IFDEF VER9P}, Encoding.Unicode{$ENDIF}{$ENDIF});
  try
    ExecuteForXML(Stream,
    {$IFDEF CLR}
      oeUnicode
    {$ELSE}
      oeANSI
    {$ENDIF}
    );
    XML := Stream.DataString;
  finally
    Stream.Free;
  end;
end;

{$IFNDEF CLR}
{$IFDEF VER6P}
procedure TMSSQL.ExecuteForXML(out XML: WideString);
var
  Stream: TMemoryStream;
begin
  Stream := TMemoryStream.Create;
  try
    ExecuteForXML(Stream, oeUnicode);
    XML := Marshal.PtrToStringUni(Stream.Memory, Stream.Size div 2);
  finally
    Stream.Free;
  end;
end;
{$ENDIF}
{$ENDIF}

procedure TMSSQL.ExecuteForXML(Stream: TStream; OutputEncoding: TOLEDBOutputEncoding);
begin
  FICommand.SetProp(prOutputStream, {$IFDEF CLR}Variant{$ELSE}Integer{$ENDIF}(Stream));
  try
    FICommand.SetProp(prOutputEncoding, Integer(OutputEncoding));
    Execute;
  finally
    FICommand.SetProp(prOutputStream, {$IFDEF CLR}Variant{$ELSE}Integer{$ENDIF}(nil));
  end;
end;

procedure TMSSQL.CreateProcCall(Name: _string);
begin
  InternalCreateProcCall(Name, Params.Count = 0);
end;

{ TMSXMLField }

constructor TMSXMLField.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);

  SetDataType(TFieldType(ftMSXML));
end;


function TMSXMLField.GetBlobType: TBlobType;
begin
  Result := ftBlob;
end;

procedure TMSXMLField.SetBlobType(Value: TBlobType);
begin
  inherited BlobType := ftBlob;

  SetDataType(TFieldType(ftMSXML));
end;

procedure TMSXMLField.GetText(var Text: string; DisplayText: Boolean);
begin
  Text := '(xml)';
  if not GetIsNull then
    Text := AnsiUpperCase(Text);
end;

procedure TMSXMLField.SetSchemaCollection(Name, CatalogName, SchemaName: _string);
begin
  FSchemaCollection.Name := Name;
  FSchemaCollection.CatalogName := CatalogName;
  FSchemaCollection.SchemaName := SchemaName;
  FTyped := (FSchemaCollection.Name <> '') or (FSchemaCollection.CatalogName <> '') or
    (FSchemaCollection.SchemaName <> '');
end;

{ TMSUDTField }

constructor TMSUDTField.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);

  SetDataType(TFieldType(ftMSUDT));
end;

procedure TMSUDTField.SetFieldDataType(Value: TFieldType);
begin
  SetDataType(Value);
end;

procedure TMSUDTField.SetUDTTypeInfo(AUDTSchemaname, AUDTName, AUDTCatalogname,
  AAssemblyTypename: _string{$IFNDEF CLR}; AUDTDispatcher: TUDTDispatcher{$ENDIF});
begin
  FUDTSchemaname := AUDTSchemaname;
  FUDTName := AUDTName;
  FUDTCatalogname := AUDTCatalogname;
  FAssemblyTypename := AAssemblyTypename;
{$IFNDEF CLR}
  FUDTDispatcher := AUDTDispatcher;
  if FUDTDispatcher <> nil then
    FUDTDispatcher.AfterInvoke := AfterInvoke;
{$ENDIF}
end;

{$IFNDEF CLR}
function TMSUDTField.GetClassDesc: string; 
begin
  if IsNull then
    Result := '(msudt)'
  else
    Result := '(MSUDT)';
end;

function TMSUDTField.GetAsUDT: Variant;
var
  ped: pExceptionData;
begin
  Result := Unassigned;
  if IsClass(DataSet, TCustomMSDataSet) then begin
    FUDTDispatcher.InitUDTProxy;
    UDTCheck(FUDTDispatcher.UDTProxy.LoadAssemblyTypeName(PWideChar(FAssemblyTypename), ped), ped);
    //if DataSet.State <> dsInsert then
    if not IsNull then
      UDTCheck(FUDTDispatcher.UDTProxy.DeserializeInstance(PSafeArray(VarArrayAsPSafeArray(
        (DataSet as TCustomDADataSet).GetBlob(Self).AsBytes)), ped), ped)
    else begin
      UDTCheck(FUDTDispatcher.UDTProxy.DeserializeInstance(nil, ped), ped);
      RetrieveUDTData;
    end;
    Result := IDispatch(FUDTDispatcher);
  end;
end;

procedure TMSUDTField.RetrieveUDTData;
var
  PData: PVarArray;
  PVal: IntPtr;
  LBound, UBound: integer;
  ped: pExceptionData;
begin
  if DataSet.State in dsWriteModes then begin
    UDTCheck(FUDTDispatcher.UDTProxy.SerializeInstance(PSafeArray(PData), ped), ped);
    VarResultCheck(SafeArrayGetLBound({$IFNDEF FPC}PSafeArray{$ENDIF}(PData), 1, LBound));
    VarResultCheck(SafeArrayGetUBound({$IFNDEF FPC}PSafeArray{$ENDIF}(PData), 1, UBound));
    if SafeArrayLock({$IFNDEF FPC}PSafeArray{$ENDIF}(PData)) = VAR_OK then
    try
      if SafeArrayAccessData({$IFNDEF FPC}PSafeArray{$ENDIF}(PData), PVal) = VAR_OK then
      try
        (DataSet as TCustomDADataSet).GetBlob(Self).Write(0, UBound - LBound + 1, PVal);
      finally
        SafeArrayUnaccessData({$IFNDEF FPC}PSafeArray{$ENDIF}(PData));
      end;
    finally
      SafeArrayUnlock({$IFNDEF FPC}PSafeArray{$ENDIF}(PData));
      SafeArrayDestroy({$IFNDEF FPC}PSafeArray{$ENDIF}(PData));
    end;
  end;
end;

procedure TMSUDTField.AfterInvoke(Sender: TObject);
begin
  RetrieveUDTData;
end;
{$ENDIF}

{ TMSDataTypesMap }

class function TMSDataTypesMap.GetFieldType(DataType: Word): TFieldType;
begin
  case DataType of
    dtMSXML:
      Result := TFieldType(ftMSXML);
  {$IFDEF VER5P}
{    dtVariant:
      Result := ftVariant;}
{    dtIUnknown:
      Result := ftInterface;}
  {$ENDIF}
  else
    Result :=inherited GetFieldType(DataType);
  end;
end;

class function TMSDataTypesMap.GetDataType(FieldType: TFieldType): integer;
begin
  if Integer(FieldType) = ftMSXML then
    Result := dtMSXML
  else
  if Integer(FieldType) = ftMSUDT then
    Result := dtBlob
  else
    Result := inherited GetDataType(FieldType);
end;

{ TMSDataSetUpdater }

constructor TMSDataSetUpdater.Create(AOwner: TDataSetService);
begin
  inherited;

  FDataSetService := TMSDataSetService(AOwner);
end;

procedure TMSDataSetUpdater.CheckUpdateQuery(const StatementType: TStatementType);
var
  UseMSSQL: boolean;
begin
  FUpdateQuery := FUpdateComponents[StatementType];
  if FUpdateQuery = nil then begin
    if not __UseUpdateOptimization then
      UseMSSQL := False
    else
      case StatementType of
        stInsert, stUpdate:
          UseMSSQL := not TCustomMSDataSet(FDataSet).Options.DMLRefresh;
        stDelete:
          UseMSSQL := True;
        else
          UseMSSQL := False;
      end;
    if UseMSSQL and (FDataSetService.GetOleDBProvider = prCompact) then
      UseMSSQL := not (TCustomMSDataSet(FDataSet).Options.QueryIdentity and
        (FDataSetService.IdentityField <> nil));

    if UseMSSQL then begin
      Assert(UsedConnection <> nil);
      FUpdateQuery := TMSSQL.Create(nil);
      TMSSQL(FUpdateQuery).Connection := TCustomMSConnection(UsedConnection);
    {$IFDEF HAVE_COMPRESS}
      TMSSQL(FUpdateQuery).CheckICommand;
      TDBAccessUtils.GetICommand(TMSSQL(FUpdateQuery)).SetProp(prCompressBlobMode, TCustomMSDataSet(FDataSet).Options.CompressBlobMode);
    {$ENDIF}
    end;
  end;
  FUpdateComponents[StatementType] := FUpdateQuery;

  inherited;
end;

procedure TMSDataSetUpdater.SetUpdateQueryOptions(const StatementType: TStatementType);
begin
  if FUpdateQuery is TCustomMSDataSet then begin
    TCustomMSDataSet(UpdateQuery).FetchAll := True;
    TCustomMSDataSet(UpdateQuery).Options.UniqueRecords := False;
    TCustomMSDataSet(UpdateQuery).Options.QuoteNames := False;
    TCustomMSDataSet(UpdateQuery).Options.DescribeParams := TCustomMSDataSet(FDataSet).Options.DescribeParams;
  end;
end;

{ TMSDataSetService }

procedure TMSDataSetService.CreateSQLGenerator;
begin
  SetSQLGenerator(TMSSQLGenerator.Create(Self));
end;

procedure TMSDataSetService.CreateDataSetUpdater;
begin
  SetDataSetUpdater(TMSDataSetUpdater.Create(Self));
end;

function TMSDataSetService.GetOLEDBProvider: TOLEDBProvider;
begin
  Result := inherited GetOLEDBProvider;
end;

function TMSDataSetService.DetectCanModify: boolean;
begin
  Result := inherited DetectCanModify and
    (TCustomMSDataSet(FDataSet).CursorType <> ctStatic);
end;

procedure TMSDataSetService.SetFieldOrigin(Field: TField; FieldDesc: TCRFieldDesc);
begin
  if (FDataSet is TCustomMSTable) and (Field.FieldKind = fkData) then
    Field.Origin := TCustomMSTable(FDataSet).FTableName + '.' + Field.FieldName
  else
    inherited;
end;

procedure TMSDataSetService.InitCursor;
var
  i: integer;
  Field: TField;
  OLEDBFieldDesc: TOLEDBFieldDesc;
begin
  inherited;

  for i := 0 to FDataSet.Fields.Count - 1 do begin
    Field := FDataSet.Fields[i];
    if Field.FieldKind <> fkData then
      continue;

    OLEDBFieldDesc := TOLEDBFieldDesc(FDataSet.GetFieldDesc(Field));
    if Field.DataType = TFieldType(ftMSXML) then begin
      Assert(Field is TMSXMLField);
      TMSXMLField(Field).SetSchemaCollection(OLEDBFieldDesc.XMLSchemaCollectionName,
        OLEDBFieldDesc.XMLSchemaCollectionCatalogName, OLEDBFieldDesc.XMLSchemaCollectionSchemaName);
    end
    else
    if Field.DataType = TFieldType(ftMSUDT) then begin
      Assert(Field is TMSUDTField);
      TMSUDTField(Field).SetUDTTypeInfo(OLEDBFieldDesc.UDTSchemaname, OLEDBFieldDesc.UDTName,
      OLEDBFieldDesc.UDTCatalogname, OLEDBFieldDesc.AssemblyTypename{$IFNDEF CLR},
      OLEDBFieldDesc.UDTDispatcher{$ENDIF});
    end;
  end;
end;

{ TMSTableData }

constructor TMSTableData.Create(Owner: TComponent);
begin
  inherited Create(Owner);

  FDesignCreate := csDesigning in ComponentState;
  CachedUpdates := True;
  CreateIRecordSet;
end;

destructor TMSTableData.Destroy;
begin
  Close;

  if UsedConnection <> nil then
    TDBAccessUtils.UnregisterClient(UsedConnection, Self);

  FTableObject.Free;
  inherited;
end;

procedure TMSTableData.CreateIRecordSet;
begin
  SetIRecordSet(TOLEDBTableTypeRecordSet.Create);
end;

procedure TMSTableData.SetIRecordSet(Value: TData);
begin
  inherited;

  FIRecordSet := TOLEDBTableTypeRecordSet(Value);
  if FIRecordSet <> nil then begin
    if FConnection <> nil then
      FIRecordSet.SetConnection(TDBAccessUtils.GetIConnection(FConnection))
    else
      FIRecordSet.SetConnection(nil);

    FIRecordSet.SetTransaction(nil);
    FIRecordSet.Component := Self;

    FIRecordSet.SetProp(prTableTypeName, FTableTypeName);
  end;
end;

procedure TMSTableData.Loaded;
begin
  FStreamedOpen := True;
  try
    inherited;
    FDesignCreate := False;
  finally
    FStreamedOpen := False;
  end;
end;

function TMSTableData.UsedConnection: TCustomMSConnection;
begin
  Result := FConnection;
end;

procedure TMSTableData.BeginConnection;
var
  vUsedConnection: TCustomDAConnection;
begin
  vUsedConnection := UsedConnection;
  if vUsedConnection = nil then
    DatabaseError(SConnectionNotDefined);

  TDBAccessUtils.InternalConnect(vUsedConnection); // We should call connect each time to update ConnectCount

  CheckDataSetService;
  FIRecordSet.SetConnection(TDBAccessUtils.GetIConnection(UsedConnection));
end;

procedure TMSTableData.EndConnection;
var
  vUsedConnection: TCustomDAConnection;
begin
  vUsedConnection := UsedConnection;
  if vUsedConnection <> nil then
    TDBAccessUtils.InternalDisconnect(vUsedConnection);
end;

procedure TMSTableData.Disconnect(NeedClose: boolean = True);
begin
  if NeedClose then begin
    Close;
    UnPrepare;
    FieldDefs.Updated := False;
  end
  else
    if FIRecordSet <> nil then
      FIRecordSet.Disconnect;
end;

procedure TMSTableData.ConnectChange(Sender: TObject; Connecting: boolean);
begin
  if not Connecting then begin
    Disconnect(not (TCustomDAConnection(Sender).Options.DisconnectedMode));
  end
  else
    if not (Sender is TCustomDAConnection) and (FIRecordSet <> nil) then begin // Dll call
      Assert(UsedConnection <> nil);
      FIRecordSet.SetConnection(TDBAccessUtils.GetIConnection(UsedConnection));
    end;
end;

function TMSTableData.GetDataTypesMap: TDataTypesMapClass;
begin
  Result := TMSDataTypesMap;
end;

{ Open/Close }

procedure TMSTableData.Prepare;
begin
  DatabaseError(SPrepareNotSupported, self);
end;

procedure TMSTableData.SetActive(Value: Boolean);
begin
  if not FStreamedOpen or (csDesigning in ComponentState) or (UsedConnection = nil) or
    UsedConnection.Options.KeepDesignConnected
  then
    inherited;
end;

procedure TMSTableData.OpenCursor(InfoQuery: boolean);
var
  ReOpen: boolean;
begin
  if TableTypeName = '' then
    DatabaseError(STableTypeNameNotDefined);

  if UsedConnection <> nil then
    TDBAccessUtils.PushOperation(UsedConnection, clOpen, TDBAccessUtils.IsFailOverAllowed(UsedConnection));
  try
    BeginConnection;
    try
      if Active then
        Exit; // for open OnChangeConnect
      repeat
        ReOpen := False;

        try
          inherited;
        except
          on E: TObject do begin
            if E is EFailOver then begin
              TDBAccessUtils.RestoreAfterFailOver(UsedConnection);
              Reopen := True;
            end
            else
              raise;
          end;
        end;
      until (not ReOpen);
    finally
      EndConnection;
    end;
  finally
    if UsedConnection <> nil then
      TDBAccessUtils.PopOperation(UsedConnection);
  end;
end;

procedure TMSTableData.CloseCursor;
var
  NeedDisconnect: boolean;
begin
  NeedDisconnect := (FIRecordSet <> nil) and
    (FIRecordSet.GetCommand.GetCursorState <> csInactive) and
    (not FIRecordSet.CanDisconnect); // if command is active and we doesn't
                                     // already substract ConnectCount after all data fetch
  inherited;

  FieldDefs.Updated := False;

  if NeedDisconnect then // if there is opened cursor then we should disconnect
    EndConnection;
end;

procedure TMSTableData.DataReopen;
var
  OldRecordSize: longint;
begin
  OldRecordSize := RecordSize;

  try
    Data.Reopen; // RecordSize can be changed here
  except
    on E: Exception do begin
      if not (E is EFailOver) then
        Close;
      raise;
    end;
  end;

  if RecordSize <> OldRecordSize then begin
    Close;
    Open;
  end;
end;

procedure TMSTableData.InternalRefresh;
var
  Retry: boolean;
begin
  if UsedConnection <> nil then
    TDBAccessUtils.PushOperation(UsedConnection, clRefresh, TDBAccessUtils.IsFailOverAllowed(UsedConnection));
  try
    BeginConnection;
    try
      repeat
        Retry := False;
        try
          inherited;
        except
          on E: Exception do begin
            if E is EFailOver then begin
              TDBAccessUtils.RestoreAfterFailOver(UsedConnection);
              Retry := True;
            end
            else
              raise;
          end;
        end;

      until not Retry;
    finally
      EndConnection;
    end;
  finally
    if UsedConnection <> nil then
      TDBAccessUtils.PopOperation(UsedConnection);
  end;
end;

procedure TMSTableData.InternalClose;
begin
  try
    inherited;
  except
    on E: EDAError do  // Borland's bug in DoInternalClose with FBufferCount
      if not E.IsFatalError then
        raise;
    else
      raise;
  end;
end;

procedure TMSTableData.InternalOpen;
begin
  inherited;
  // FDataSetService.InitCursor;
end;

procedure TMSTableData.Resync(Mode: TResyncMode);
begin
  // this need if Resync called for closed dataset (AV BUG !!!)
  if Active then
    inherited;
end;

procedure TMSTableData.SetConnection(Value: TCustomMSConnection);
begin
  if (Value <> FConnection) or (Value <> UsedConnection) then begin

    if UsedConnection <> nil then begin
      Disconnect;
      TDBAccessUtils.UnregisterClient(UsedConnection, Self);
    end;

    FConnection := Value;

    if FConnection <> nil then begin
      TDBAccessUtils.RegisterClient(Value, Self, ConnectChange);

      if FIRecordSet <> nil then
        FIRecordSet.SetConnection(TDBAccessUtils.GetIConnection(FConnection))
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

procedure TMSTableData.SetTableTypeName(const Value: _string);
begin
  if not (csReading in ComponentState) then
    Active := False;
  FTableTypeName := OLEDBSQLInfo.NormalizeName(Trim(Value), False, True);
  if FIRecordSet <> nil then
    FIRecordSet.SetProp(prTableTypeName, FTableTypeName);
end;

function TMSTableData.GetTable: TMSTableObject;
begin
  if FTableObject = nil then begin
    FTableObject := TMSTableObject.Create;
    FTableObject.SetData(FIRecordSet);
  end;
  FTableObject.TableTypeName := FTableTypeName;
  Result := FTableObject;
end;

{ Additional data types }

function TMSTableData.GetField(FieldDesc: TFieldDesc): TField;
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

function TMSTableData.GetDataType(const FieldName: _string): integer;
begin
  if FIRecordSet = nil then
    raise Exception.Create(Format(SFieldNotFound, [FieldName]));

  Result := FIRecordSet.FieldByName(FieldName).DataType
end;

function TMSTableData.GetFieldDesc(const FieldName: _string): TFieldDesc;
begin
  if FIRecordSet = nil then
    raise Exception.Create(Format(SFieldNotFound, [FieldName]));

  Result := FIRecordSet.FieldByName(FieldName);
end;

function TMSTableData.GetFieldDesc(const FieldNo: integer): TFieldDesc;
begin
  if (FIRecordSet = nil) or (FieldNo <= 0) then {fkCalculated, fkLookup}
    Result := nil
  else
    Result := TFieldDesc(FIRecordSet.Fields[FieldNo - 1])
end;

function TMSTableData.GetFieldPrecision(const FieldName: _string): integer;
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

function TMSTableData.GetFieldScale(const FieldName: _string): integer;
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

function TMSTableData.GetFieldObject(FieldDesc: TFieldDesc): TSharedObject;
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

function TMSTableData.GetFieldObject(const FieldName: _string): TSharedObject;
var
  FieldDesc: TFieldDesc;
begin
  if FIRecordSet = nil then
    raise Exception.Create(Format(SFieldNotFound, [FieldName]));

  FieldDesc := FIRecordSet.FieldByName(FieldName);
  Result := GetFieldObject(FieldDesc);
end;

procedure TMSTableData.AssignTo(Dest: TPersistent);
begin
  inherited;

  if Dest is TMSTableData then begin
    TMSTableData(Dest).Connection := Connection;
    TMSTableData(Dest).TableTypeName := TableTypeName;
  end;
end;

function TMSTableData.CreateBlobStream(Field: TField; Mode: TBlobStreamMode): TStream;
var
  Blob: {$IFDEF CLR}Devart.Dac.{$ENDIF}MemData.TBlob;
  OldRollback: boolean;
begin
  if Field.DataSet = Self then
    Blob := GetBlob(Field)
  else
    Blob := GetBlob(Field.FieldName);
  if (Blob <> nil) and (Mode <> bmWrite) and UsedConnection.ConvertEOL
    and (Field.DataType in [ftMemo{$IFDEF VER10P}, ftWideMemo{$ENDIF}])
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

{ TMSFileStream }

{$IFDEF CLR}
const
  SInputErr = 'Invalid input value';
{$ENDIF}

constructor TMSFileStream.Create(AHandle: Integer);
begin
  inherited Create;
  FHandle := AHandle;
end;

destructor TMSFileStream.Destroy;
begin
  Close;

  if FOwner <> nil then
    FOwner.Remove(Self);
  FOwner := nil;
  inherited;
end;

{$IFDEF CLR}
function TMSFileStream.Read(var Buffer: array of Byte; Offset, Count: Longint): Longint;
var
  b: IntPtr;
begin
  if (Count < 0) or (Offset < 0) or ((Length(Buffer) - Offset) < Count) then
    raise EReadError.Create(SInputErr);

  b := Marshal.UnsafeAddrOfPinnedArrayElement(Buffer, Offset);
  Win32Check(ReadFile(THandle(Handle), b, Count, Result, nil));
end;
{$ELSE}
function TMSFileStream.Read(var Buffer; Count: Longint): Longint;
begin
  Win32Check(ReadFile(THandle(Handle), Buffer, Count, LongWord(Result), nil));
end;
{$ENDIF}

{$IFDEF CLR}
function TMSFileStream.Write(const Buffer: array of Byte; Offset, Count: Longint): Longint;
var
  b: IntPtr;
begin
  if (Count < 0) or (Offset < 0) or ((Length(Buffer) - Offset) < Count) then
    raise EReadError.Create(SInputErr);

  b := Marshal.UnsafeAddrOfPinnedArrayElement(Buffer, Offset);
  Win32Check(WriteFile(THandle(Handle), b, Count, Result, nil));
end;
{$ELSE}
function TMSFileStream.Write(const Buffer; Count: Longint): Longint;
begin
  Win32Check(WriteFile(THandle(Handle), Buffer, Count, LongWord(Result), nil));
end;
{$ENDIF}

{$IFDEF VER6P}
function TMSFileStream.Seek(const Offset: Int64; Origin: TSeekOrigin): Int64;
{$IFDEF CLR}
var
  DistanceLow, DistanceHigh: Longint;
{$ENDIF}
begin
{$IFDEF CLR}
  DistanceLow := Longint(Offset and $FFFFFFFF);
  DistanceHigh := Longint((Offset shr 32) and $FFFFFFFF);
  DistanceLow := SetFilePointer(THandle(Handle), DistanceLow,
    DistanceHigh, LongWord(Origin));

  if (DistanceLow = -1) and (GetLastError <> 0) then
    RaiseLastOSError;

  Result := (Int64(DistanceHigh) shl 32) + Int64(DistanceLow);
{$ELSE}
  Result := Offset;
  Int64Rec(Result).Lo := SetFilePointer(THandle(Handle), Int64Rec(Result).Lo,
    @Int64Rec(Result).Hi, Integer(Origin));

  if (Int64Rec(Result).Lo = LongWord(-1)) and (GetLastError <> 0) then
    RaiseLastOSError;
{$ENDIF}
end;
{$ELSE}
function TMSFileStream.Seek(Offset: Longint; Origin: Word): Longint;
begin
  Result := SetFilePointer(THandle(Handle), Offset, nil, Integer(Origin));

  if (Result = -1) and (GetLastError <> 0) then
    RaiseLastOSError;
end;
{$ENDIF}

{$IFNDEF CLR}
procedure TMSFileStream.SetSize(NewSize: Longint);
begin
{$IFDEF VER6P}
  SetSize(Int64(NewSize));
{$ELSE}
  Seek(NewSize, 0{soBeginning});
  Win32Check(SetEndOfFile(THandle(FHandle)));
{$ENDIF}
end;
{$ENDIF}

{$IFDEF VER6P}
procedure TMSFileStream.SetSize({$IFNDEF CLR}const{$ENDIF} NewSize: Int64);
begin
  Seek(NewSize, soBeginning);
  Win32Check(SetEndOfFile(THandle(FHandle)));
end;
{$ENDIF}

procedure TMSFileStream.Flush;
begin
  Win32Check(FlushFileBuffers(THandle(FHandle)));
end;

procedure TMSFileStream.Close;
begin
  if FHandle >= 0 then begin
    Win32Check(CloseHandle(THandle(FHandle)));
    FHandle := -1;
  end;
end;

{ TMSAccessUtils }

class procedure TMSAccessUtils.SetDesigning(Obj: TCustomMSDataSet; Value: Boolean; SetChildren: Boolean = True);
begin
  Obj.SetDesigning(Value{$IFNDEF FPC}, SetChildren{$ENDIF});
end;

class function TMSAccessUtils.FIConnection(Obj: TCustomMSConnection): TOLEDBConnection;
begin
  Result := Obj.IConnection;
end;

class function TMSAccessUtils.FIRecordSet(Obj: TCustomMSDataSet): TOLEDBRecordSet;
begin
  Result := Obj.FIRecordSet;
end;

class function TMSAccessUtils.FICommand(Obj: TCustomMSDataSet): TOLEDBCommand;
begin
  Result := Obj.FICommand;
end;

class function TMSAccessUtils.GetOLEDBSQL(Obj: TCustomMSDataSet): _string;
begin
  if Obj.FICommand = nil then
    Result := ''
  else
    Result := Obj.FICommand.SQL;
end;

class function TMSAccessUtils.GetOLEDBSQL(Obj: TMSSQL): _string;
begin
  if Obj.FICommand = nil then
    Result := ''
  else
    Result := Obj.FICommand.SQL;
end;

type
  _TOLEDBConnection = class(TOLEDBConnection);

class function TMSAccessUtils.FIDBCreateSession(Obj: TOLEDBConnection): IDBCreateSession;
begin
  Result := _TOLEDBConnection(Obj).FIDBCreateSession;
end;

class procedure TMSAccessUtils.DoError(Obj: TCustomMSConnection; E: Exception; var Fail: boolean);
begin
  Assert(Obj.IConnection <> nil);
  _TOLEDBConnection(Obj.IConnection).DoError(E, Fail);
end;

class function TMSAccessUtils.UsedConnection(Obj: TMSTableData): TCustomMSConnection;
begin
  Result := Obj.UsedConnection;
end;

initialization
  __UseUpdateOptimization := True;

  try
    TMSConnectionPoolManager.Clear;
  except
  end;

  if
  {$IFDEF CLR}
    CompareText(Assembly.GetCallingAssembly.GetName.Name, 'Devart.Sdac') = 0
  {$ELSE}
    not IsLibrary
  {$ENDIF}
  then begin
    Classes.RegisterClass(TMSXMLField);
    Classes.RegisterClass(TMSUDTField);
  end;

finalization
    
end.
