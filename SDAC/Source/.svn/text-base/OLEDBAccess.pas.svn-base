
//////////////////////////////////////////////////
//  SQL Server Data Access Components
//  Copyright © 1998-2012 Devart. All right reserved.
//  Access via OLE DB
//////////////////////////////////////////////////

{$IFNDEF CLR}
{$IFNDEF UNIDACPRO}

{$I Sdac.inc}

unit OLEDBAccess;
{$ENDIF}
{$ENDIF}

interface

uses
{$IFDEF CLR}
  Borland.Vcl.TypInfo, Variants, System.Runtime.InteropServices,
{$ELSE}
  CLRClasses,
{$ENDIF}
  Windows, Classes,
{$IFNDEF FPC}
  DBConsts,
{$ENDIF}
{$IFNDEF LITE}
  DB, DBAccess,
{$ENDIF}
{$IFDEF VER6P}
  FMTBcd,
{$ENDIF}
  DAConsts, SysUtils, ActiveX, SyncObjs, CRTypes, CRParser, CRAccess, MemData,
{$IFNDEF UNIDACPRO}
  OLEDBC, OLEDBIntf, MSConsts, CRThread,
{$ELSE}
  OLEDBCUni, OLEDBIntfUni, MSConstsUni, CRThreadUni,
{$ENDIF}
{$IFNDEF LITE}
  MTSCall,
{$IFNDEF CLR}
  {$IFNDEF UNIDACPRO}MSUDT{$ELSE}MSUDTUni{$ENDIF},
{$ENDIF}
{$ENDIF}
  ComObj, Contnrs;

const
  DefaultSDACDatabase = 'master';
  DefaultPacketSize = 4096;
  DefaultConnectionTimeout = 15;
  // Compact Edition specific
  DefaultMaxDatabaseSize = 128;
  DefaultMaxBufferSize = 640;
  DefaultTempFileMaxSize = 128;
  DefaultDefaultLockEscalation = 100;
  DefaultDefaultLockTimeout = 2000;
  DefaultAutoShrinkThreshold = 60;
  DefaultFlushInterval = 10;

  OLE_DB_INDICATOR_SIZE = sizeof(DBSTATUS);

  MaxNonBlobFieldLen = 8000;  // Maximum length of "char", "varchar", "nchar", "nvarchar", fields

  // properties
  prOleDBBase       = 1000;

  // connection properties
  prAuthentication  = prOleDBBase + 2; // TMSAuthentication

  // dataset options
  prUniqueRecords       = prOleDBBase + 8;  // boolean
  prCursorType          = prOleDBBase + 9;  // TMSCursorType
  prRequestSQLObjects   = prOleDBBase + 10; // boolean
  prCursorUpdate        = prOleDBBase + 12; // boolean
  prLockClearMultipleResults = prOleDBBase + 13; // boolean
  prConnectionTimeout   = prOleDBBase + 14; // integer
  prCommandTimeout      = prOleDBBase + 15; // integer
  prQuotedIdentifier    = prOleDBBase + 16; // boolean
  prLanguage            = prOleDBBase + 17; // string
  prEncrypt             = prOleDBBase + 18; // boolean
  prNetworkLibrary      = prOleDBBase + 19; // string
  prPacketSize          = prOleDBBase + 20; // integer
  //prMultipleConnections = prOleDBBase + 18; // boolean
  // if False then unicode fields is supported as TStringsField else as TWideStringField
  prWideStrings         = prOleDBBase + 22; // boolean
  prApplicationName     = prOleDBBase + 23; // string
  prWorkstationID       = prOleDBBase + 24; // string
  prAutoTranslate       = prOleDBBase + 26; // boolean
  prProvider            = prOleDBBase + 28; // string
  prPersistSecurityInfo = prOleDBBase + 30; // boolean
  prInitialFileName     = prOleDBBase + 31; // string
  prMARS                = prOleDBBase + 32; // boolean
  // Query notification
  prNotification        = prOleDBBase + 33; // boolean
  prNotificationMessage = prOleDBBase + 34; // string
  prNotificationService = prOleDBBase + 35; // string
  prNotificationTimeout = prOleDBBase + 36; // cardinal
  prDelayedSubsciption  = prOleDBBase + 55; // boolean
  prNonBlocking         = prOleDBBase + 37; // boolean
  prOldPassword         = prOleDBBase + 38; // string
  prMaxDatabaseSize     = prOleDBBase + 39; // integer
  prFailoverPartner     = prOleDBBase + 40; // string
  prOutputStream        = prOleDBBase + 41; // TStream
  prOutputEncoding      = prOleDBBase + 42; // string
  prTrustServerCertificate = prOleDBBase + 43; // boolean
  // Compact Edition specific
  prTempFileDirectory   = prOleDBBase + 44; // string
  prTempFileMaxSize     = prOleDBBase + 45; // integer
  prDefaultLockEscalation = prOleDBBase + 46; // integer
  prDefaultLockTimeout  = prOleDBBase + 47; // integer
  prAutoShrinkThreshold = prOleDBBase + 48; // integer
  prMaxBufferSize       = prOleDBBase + 49; // integer
  prFlushInterval       = prOleDBBase + 50; // integer
  prTransactionCommitMode = prOleDBBase + 51; // TCompactCommitMode
  prLockTimeout         = prOleDBBase + 52; // integer
  prLockEscalation      = prOleDBBase + 53; // integer
  prInitMode            = prOleDBBase + 54; // integer
  prWideMemos           = prOleDBBase + 55; // boolean
  prCompactVersion      = prOleDBBase + 56; // TCompactVersion
  prSensibleBCDMapping  = prOleDBBase + 57; // boolean UniDAC behavior
  prSetLockTimeout      = prOleDBBase + 58; // boolean UniDAC behavior
  prNativeClientVersion = prOleDBBase + 59; // TNativeClientVersion
  prKeepIdentity        = prOleDBBase + 60;
  prKeepNulls           = prOleDBBase + 61;
  prRowsPerBatch        = prOleDBBase + 62;
  prKilobytesPerBatch   = prOleDBBase + 63;
  prLockTable           = prOleDBBase + 64;
  prCheckConstraints    = prOleDBBase + 65;
  prDisableMultipleResults = prOleDBBase + 66;
  prBaseTableName       = prOleDBBase + 67;
  prBulkExecuting       = prOleDBBase + 68;
  prTableTypeName       = prOleDBBase + 69;
  prLocaleIdentifier    = prOleDBBase + 70; // cardinal

type
  TCursorTypeChangedProc = procedure of object;
  TMSCursorType = (ctDefaultResultSet, ctStatic, ctKeyset, ctDynamic, ctBaseTable);
  TMSCursorTypes = set of TMSCursorType;

  // Compact Edition specific
  TCompactCommitMode = (cmAsynchCommit, cmSynchCommit);

{ internal data types }
const
  dtMSXML = 100;

{ internal sub data types }
const
  dtChar = 20;
  dtText = 22;
  dtWide = $8000;
  dtMSUDT = 23;
  dtNumeric = 24;
  dtUInt64 = 25;

  SizeOf_OleVariant = {$IFNDEF CLR}SizeOf(OleVariant){$ELSE}16{$ENDIF};

const
  ServerCursorTypes: TMSCursorTypes = [ctStatic, ctKeyset, ctDynamic];

const
  ctTableType = 10;

type
  TOLEDBParamDesc = class(TParamDesc)
  protected
    FTableTypeName: string;
  public
    function GetValue: variant; override;
    procedure SetValue(const Value: variant); override;

    function GetAsBlobRef: TBlob;

    property TableTypeName: string read FTableTypeName;
  end;
  
  TRunMethod = procedure of object;

  TOLEDBThreadWrapper = class(TCRThreadWrapper)
  protected
    procedure DoException(E: Exception); override;
  end;
  
  TExecuteThread = class(TCRThread)
  protected
    FRunMethod: TRunMethod;
    procedure InternalExecute; override;
  {$IFDEF CLR}
    property Terminated;
  {$ENDIF}
  end;

{ TOLEDBConnection }

  TOLEDBProvider = (prAuto, prSQL, prNativeClient, prCompact);

  TCompactVersion = (cvAuto, cv30, cv35, cv40);

  TNativeClientVersion = (ncAuto, nc2008);

  TMSAuthentication = (auWindows, auServer);

  TMSInitMode = (imReadOnly, imReadWrite, imExclusive, imShareRead);

  EOLEDBError = class;
  EMSError = class;
  // TMSErrorProc = procedure (E: {$IFDEF LITE}Exception{$ELSE}EDAError{$ENDIF}; var Fail: boolean) of object;
  TMSInfoMessageProc = procedure (E: EMSError) of object;

  // ----------
  // Must be declared before(!) TOLEDBConnection declaration to prevent CBuilder compiler bug (pas -> hpp)

  {
  AccessorBlock types:
    1. Ordinary. May contain many ordinary fields and only one BLOB field
      1.1. For DefaultResultSet or for CursorUpdate = False. All fields in block must be not ReadOnly
      1.2. In other cases ReadOnly and not ReadOnly fields may contains in one AccessorBlock
    2. ReadOnly. Used only for KeySet and Dynamic if CursorUpdate is True. All fields in block must be ReadOnly
    3. BLOB. May contain only one BLOB field
    4. FetchBlock. Used for dtVariant and dtMemo (not BLOB) fields

    RecordSet may contain only one each of accessor types, and some BLOB accessors if need
  }

  TAccessorBlockType = (abOrdinary, abReadOnly, abBLOB, abFetchBlock);
  TAccessorBlock = record
    BlockType: TAccessorBlockType;
    hAcc: HACCESSOR; // OLE DB accessor
    BlobFieldNum: integer; // -1, if BLOB field not avaible. This member useful to fetch BLOB streams
    FieldNums: array of integer;
  end;

  TOLEDBRecordSet = class;
  TRestrictions = array of OleVariant;

  TAnalyzeMethod = function(const Status: HRESULT; const Arg: IntPtr = nil): WideString of object;

  TOLEDBConnection = class (TCRConnection)
  protected
    //FIMalloc: IMalloc;

  { DataSource }
    FIDBInitialize: IDBInitialize;
    FIDBProperties: IDBProperties;
    FIDBCreateSession: IDBCreateSession;

  { Session }
    FISessionProperties: ISessionProperties;
  {$IFNDEF LITE}
    FITransactionJoin: ITransactionJoin;
  {$ENDIF}

    FCommand: TCRCommand; // Used for ExecSQL. Created on first use

    FDatabase: _string;
    // FMultipleConnections: boolean;

    FAuthentication: TMSAuthentication;
    FProvider: TOLEDBProvider;
    FCompactVersion: TCompactVersion;
    FNativeClientVersion: TNativeClientVersion;

    // Init properties
    FConnectionTimeout: integer;
    FQuotedIdentifier: boolean;
    FLanguage: _string;
    FAutoTranslate: boolean;
    FEncrypt: boolean;
    FPersistSecurityInfo: boolean;
    FNetworkLibrary: _string;
    FPacketSize: integer;
    FApplicationName: _string;
    FWorkstationID: _string;
    FInitialFileName: _string;
    FFailoverPartner: _string;
    FMultipleActiveResultSets: boolean;
    FTrustServerCertificate: boolean;
    FOldPassword: _string;
    //Compact Edition specific
    FMaxDatabaseSize: integer;
    FMaxBufferSize: integer;
    FTempFileDirectory: _string;
    FTempFileMaxSize: integer;
    FDefaultLockEscalation: integer;
    FDefaultLockTimeout: integer;
    FAutoShrinkThreshold: integer;
    FFlushInterval: integer;
    FTransactionCommitMode: TCompactCommitMode;
    FLockTimeout: integer;
    FLockEscalation: integer;
    FInitMode: TMSInitMode;
    FLocaleIdentifier: Cardinal;

    FDBMSName: _string;
    FDBMSVer: _string;
    FDBMSPrimaryVer: integer;
    FProviderFriendlyName: _string;
    FProviderVer: _string;
    FProviderPrimaryVer: integer;
    FProviderId: TGuid;
    FServerEdition: string;

    FColumnsRowsetFieldDescs: TFieldDescs; // FieldDescs for non-Native rowsets
    FColumnsMetaInfo: TOLEDBRecordSet; // ColumnsRowset
  {$IFNDEF LITE}
    FFldCatalogNameIdx, FFldSchemaNameIdx, FFldTableNameIdx, FFldColumnNameIdx,
    FFldFieldNameIdx, {FFldActualFieldNameIdx,} FFldPrecisionIdx, FFldScaleIdx, FFldGuidIdx: integer;
    FFldColumnNumberIdx, FFldIsAutoIncIdx, FFldTypeIdx, FFldFlagsIdx, FFldColumnSizeIdx, FFldComputeModeIdx: integer;
    FFldXMLSchemaCollCatalogNameIdx, FFldXMLSchemaCollSchemaNameIdx, FFldXMLSchemaCollNameIdx: integer;
    FFldAssemblyTypenameIdx, FFldUDTNameIdx, FFldUDTSchemanameIdx, FFldUDTCatalognameIdx: integer;
  {$ENDIF}

    FOnInfoMessage: TMSInfoMessageProc;
    FWideMemos: boolean;
    FSetLockTimeout: boolean; // UniDAC behavior

    procedure ReleaseInterfaces;
    function GetSchemaRowset(const Schema: TGUID; rgRestrictions: TRestrictions): IRowset;

    procedure GetConnectionProperties;
    procedure SetConnectionProperties;

    procedure SetQuotedIdentifier(const Value: boolean);

    class procedure AssignFieldDescs(Source, Dest: TFieldDescs);

  {$IFDEF CLR}
    procedure DoError(E: Exception; var Fail: boolean); override;
  {$ENDIF}
    // Compact Edition specific
    procedure CreateCompactDatabase;

  {$IFNDEF LITE}
    procedure Enlist(Transaction: TMTSTransaction); override;
    procedure UnEnlist(Transaction: TMTSTransaction); override;
  {$ENDIF}
    property EnableBCD;
  {$IFDEF VER6P}
  {$IFNDEF FPC}
    property EnableFMTBCD;
  {$ENDIF}
  {$ENDIF}

  public
    constructor Create; override;
    destructor Destroy; override;

    function GetCommandClass: TCRCommandClass; override;
    function GetTransactionClass: TCRTransactionClass; override;

    procedure Check(const Status: HRESULT; Component: TObject;
      AnalyzeMethod: TAnalyzeMethod = nil; const Arg: IntPtr = nil);
    procedure OLEDBError(const ErrorCode: HRESULT; Component: TObject;
      AnalyzeMethod: TAnalyzeMethod = nil; const Arg: IntPtr = nil);

    procedure Connect(const ConnectString: _string); override;
    procedure Disconnect; override;

    procedure Assign(Source: TCRConnection); override;
    procedure AssignConnect(Source: TCRConnection); override;

    function GetIDBCreateSession: IDBCreateSession;
    procedure SetIDBCreateSession(CreateSession: IDBCreateSession);

    procedure ExecSQL(const Text: _string);

    function GetServerVersion: _string; override;
    function GetServerVersionFull: _string; override;
    function GetClientVersion: _string; override;
    function GetServerEdition: string;
    function IsSQLAzureEdition: boolean;

    function CanChangeDatabase: boolean; override;

    function SetProp(Prop: integer; const Value: variant): boolean; override;
    function GetProp(Prop: integer; var Value: variant): boolean; override;

    procedure SetDatabase(const Value: _string);

    function CheckIsValid: boolean; override;

    //property Malloc: IMalloc read FIMalloc;
    property SessionProperties: ISessionProperties read FISessionProperties;
    property DBProperties: IDBProperties read FIDBProperties;

    property DBMSName: _string read FDBMSName;
    property DBMSVer: _string read FDBMSVer;
    property DBMSPrimaryVer: integer read FDBMSPrimaryVer;
    property ProviderFriendlyName: _string read FProviderFriendlyName;
    property ProviderVer: _string read FProviderVer;
    property ProviderPrimaryVer: integer read FProviderPrimaryVer;

    property OnInfoMessage: TMSInfoMessageProc read FOnInfoMessage write FOnInfoMessage;
  end;

{ TOLEDBCommand }

  TOLEDBStream = class;

  TIntPtrDynArray = array of IntPtr;

  TParamsAccessorData = record
    Accessor: IAccessor;
    ExecuteParams: DBPARAMS;
    rgBindings: TDBBindingArray;
  end;

  TOLEDBOutputEncoding = (oeANSI, oeUTF8, oeUnicode);

  TOLEDBCommand = class (TCRCommand)
  private
    function GetWideMemos: boolean;

  protected
    FQueryIntCnt: integer; // Quantity of calls to QueryInterfaces
    FIsSProc: boolean;
    FRPCCall: boolean;

    FPrepared: boolean;
    FConnection: TOLEDBConnection;

    FBreakExecCS: TCriticalSection; // Used to prevent clear FICommandText on BreakExec
    FWaitForBreak: boolean;

    // If statement executed w/o errors and warnings then FLastExecWarning is False
    // If statement executed with warning then FLastExecWarning is True
    // If statement executed with error then raising exception
    // Used to analyze CursorType changes in RecordSet
    FLastExecWarning: boolean;

    FCommandTimeout: integer;

  { Output Stream }
    FOutputStream: TStream;
    FOLEDBStream: TOLEDBStream;
    FOutputEncoding: TOLEDBOutputEncoding;

  { Query Notification }
    FNotification: boolean;
    FDelayedSubsciption: boolean;
    FNotificationMessage: _string;
    FNotificationService: _string;
    FNotificationTimeout: integer;

  { NonBlocking}
    FNonBlocking: boolean;
    FISSAsynchStatus: ISSAsynchStatus;
    FExecutor: TOLEDBThreadWrapper;

  { Command }
    FICommandText: ICommandText;
    FICommandPrepare: ICommandPrepare;
    FICommandProperties: ICommandProperties;

  { Rowset }
    FRequestIUnknown: boolean; // Indicate current command owner - MSSQL(False) or MSDataSet(True)
    FRequestMultipleResults: boolean; // True for ctDefaultResultSet only
    FIUnknown: IUnknown; // If requested then must be setted to a nil as it possible
    FIUnknownNext: IUnknown;
    FIMultipleResults: IMultipleResults;

    FCursorState: TCursorState;

  { Params }
    FParamsAccessorData: TParamsAccessorData;
    FParamsAccessorDataAvaible: boolean;
    FCanReadParams: boolean;
    FUseDescribeParams: boolean;
  {$IFDEF CLR}
    FParamsGC: TIntPtrDynArray; // List of AllocGCHandle(ParamDesc.Value, True)
  {$ENDIF}

  { Rows }
    FRowsAffected: NativeInt;
    FRowsAffectedNext: NativeInt;

    FNextResultRequested: boolean; // FIUnknownNext getted from OLE DB

    FSensibleBCDMapping: boolean;

    procedure Check(const Status: HRESULT; AnalyzeMethod: TAnalyzeMethod = nil);
    procedure QueryInterfaces(const QueryPrepare: boolean); // QueryPrepare must be True to request IID_ICommandPrepare
    procedure ReleaseInterfaces;

    procedure SetCommandProp;
    procedure SetParameterInfo;
    procedure GetParameterInfo;

    procedure SetOutputStream(Stream: TStream);
    function GetUseOutputStream: boolean;

  { Params }
    procedure CreateAndFillParamAccs;
    procedure RequestAndFreeParamAccs;
    procedure RequestParamsIfPossible; // Call RequestAndFreeParamAccs if interfaces is cleared

    function Analyze(const Status: HRESULT; const Arg: IntPtr = nil): WideString;
    procedure GetNextResult(out ResultSet: IUnknown; out RowsAffected: NativeInt; PrevErr: EOLEDBError = nil);
    function CalcEnableBCD: boolean;
    function CalcEnableFMTBCD: boolean;

    property EnableBCD;
  {$IFDEF VER6P}
  {$IFNDEF FPC}
    property EnableFMTBCD;
  {$ENDIF}
  {$ENDIF}

  public
    constructor Create; override;
    destructor Destroy; override;

    class function GetTableInfoClass: TTableInfoClass; override;
    class function GetSQLInfoClass: TSQLInfoClass; override;
    class function GetParserClass: TSQLParserClass; override;

    procedure Prepare; override;
    procedure Unprepare; override;
    function GetPrepared: boolean; override;

    function CreateProcCall(const Name: _string; NeedDescribe: boolean; IsQuery: boolean): _string; override;
    procedure Execute(Iters: integer = 1); override;
    procedure DoExecuteTerminate(Sender: TObject);
    procedure DoExecuteException(Sender: TObject; E: Exception; var Fail: boolean);
    procedure WaitAsynchCompletion;
    procedure EndExecute(E: Exception);

    procedure SetConnection(Value: TCRConnection); override;
    function GetCursorState: TCursorState; override;
    procedure SetCursorState(Value: TCursorState); override;

    function ParseSQL(const SQL: _string; Params: TParamDescs; ReplaceAll: boolean = True; const RenamePrefix: _string = ''): _string; override;

    function GetProp(Prop: integer; var Value: variant): boolean; override;
    function SetProp(Prop: integer; const Value: variant): boolean; override;

    procedure BreakExec; override;

    // Interface management
    function IUnknownIsNull: boolean;
    function IUnknownNextIsNull: boolean;
    function IMultipleResultsIsNull: boolean;
    procedure ClearIUnknown;
    procedure ClearIUnknownNext;
    procedure ClearIMultipleResults;
    procedure ClearISSAsynchStatus;

  { Params }
    function GetParamDescType: TParamDescClass; override;
    function AddParam: TParamDesc; override;
    function GetParam(Index: integer): TOLEDBParamDesc;
    property Params;

    property Executing;
  end;


{ TOLEDBRecordSet }

  TFetchAccessorData = record
    Accessor: IAccessor;

    AccessorBlocks: array of TAccessorBlock;
  end;

  TOLEDBFieldDesc = class (TCRFieldDesc)
  private
    FBaseCatalogName: _string;
    FBaseSchemaName: _string;
    FBaseTableName: _string;
    FIsTimestamp: boolean;
    FXMLSchemaCollectionCatalogName: _string;
    FXMLSchemaCollectionSchemaName: _string;
    FXMLSchemaCollectionName: _string;
    FXMLTyped: boolean;
    FUDTSchemaname: _string;
    FUDTName: _string;
    FUDTCatalogname: _string;
    FAssemblyTypename: _string;
  {$IFNDEF LITE}
  {$IFNDEF CLR}
    FUDTDispatcher: TUDTDispatcher;
  {$ENDIF}
  {$ENDIF}
  public
    destructor Destroy; override;

    property BaseCatalogName: _string read FBaseCatalogName;
    property BaseSchemaName: _string read FBaseSchemaName;
    property BaseTableName: _string read FBaseTableName;
    property IsTimestamp: boolean read FIsTimestamp;

    property XMLSchemaCollectionCatalogName: _string read FXMLSchemaCollectionCatalogName;
    property XMLSchemaCollectionSchemaName: _string read FXMLSchemaCollectionSchemaName;
    property XMLSchemaCollectionName: _string read FXMLSchemaCollectionName;
    property XMLTyped: boolean read FXMLTyped;

    property UDTSchemaname: _string read FUDTSchemaname;
    property UDTName: _string read FUDTName;
    property UDTCatalogname: _string read FUDTCatalogname;
    property AssemblyTypename: _string read FAssemblyTypename;
  {$IFNDEF LITE}
  {$IFNDEF CLR}
    property UDTDispatcher: TUDTDispatcher read FUDTDispatcher;
  {$ENDIF}
  {$ENDIF}
  end;

  TOLEDBTableInfo = class(TCRTableInfo)
  protected
    FMaxTimestamp: Int64;
  public
    property MaxTimestamp: Int64 read FMaxTimestamp write FMaxTimestamp;
  end;

  TOLEDBSQLInfo = class(TSQLInfo)
  public
    function LeftQuote: _char; override;
    function RightQuote: _char; override;
    function IsQuoted(const Value: _string): boolean; override;
    function ProcedureOverloadSeparator: _char; override;

    procedure SplitObjectName(const Name: _string; out DataBase: _string; out Owner: _string; out ObjName: _string); reintroduce;
    function NamesFromList(List: _TStrings): _string; override;
    procedure NamesToList(Value: _string; List: _TStrings); override;
  end;

  TOLEDBRecordSet = class (TCRRecordSet)
  private
    FProviderPrimaryVer: integer;
    FDBMSPrimaryVer: integer;
    FProviderId: TGuid;
    FProvider: TOLEDBProvider;
    FDisconnectedMode: boolean;
    FDatabase: _string;
    function GetProviderPrimaryVer: integer;
    function GetDBMSPrimaryVer: integer;
    function GetProviderId: TGuid;
    function GetProvider: TOLEDBProvider;
    function GetDisconnectedMode: boolean;
    function GetDatabase: _string;
    function GetWideMemos: boolean;
    function CursorTypeForFetching: TMSCursorType;
  protected
    FLockClearMultipleResults: boolean; // Used to prevent clear FCommand.FIMultipleResult on Close by OpenNext
    FroAfterUpdate: boolean; // DataSet.RefreshOptions.roAfterUpdate
    FDisableMultipleResults: boolean;

    FNativeRowset: boolean; // For non-native rowsets output parameters not supported
    FIsColumnsRowset: boolean; // If True then FieldDescs was stored in FColumnsRowsetFieldDescs

    FCommand: TOLEDBCommand;
    FFetchAccessorData: TFetchAccessorData;

  { Rowset }
    FIRowset: IRowset;
    FIRowsetLocate: IRowsetLocate;
    FIRowsetUpdate: IRowsetChange;

    FCursorType: TMSCursorType;
    FReadOnly: boolean;

    FRequestSQLObjects: boolean;

    FUniqueRecords: boolean;
    FWideStrings: boolean;
    FWideMemos: boolean;

    // Bookmarks
    FFetchFromBookmark: boolean;
    FProcessDynBofEof: boolean; // True, if processing NoResult for ctDynamic
    FBookmarkValue: NativeInt; // OLEDB IRowsetLocate Bookmark value. If FBookmarkValue = - 1 then last fetch is not OK for KeySet cursor
    FBookmarkSize: integer; // OLEDB IRowsetLocate Bookmark size. If FBookmarkSize = 4 then bookmark is ordinal otherwise bookmark is special (DBBMK_LAST, DBBMK_FIRST)
    FBookmarkOffset: integer; // Offset from pHBlock to bookmark field value

    FFetchBlock: IntPtr;
    FFetchBlockSize: integer;
    FLastFetchOK: boolean; // True, if previous Fetch was called succesfulity (Result is True)
    FLastFetchEnd: boolean; // True, If previous FIRowset.GetNextRows has return DB_S_ENDOFROWSET
    FLastFetchBack: boolean; // True, if previous Fetch was called with True parameter

    // HRow for IRowsetUpdate
    FHRow: HROW;
    FHRowAccessible: boolean; // True, if FHRow is setted to valid value

    FCursorUpdate: boolean;

    FCursorTypeChanged: TCursorTypeChangedProc;

    FSchema: TGUID;
    FRestrictions: TRestrictions;

    FFetching: boolean;
    FFetchExecutor: TOLEDBThreadWrapper;
    FBeforeFetch: boolean;
    FAfterFetch: boolean;

    FPopulatingKeyInfo: boolean;
    FBaseTableName: string;
    FBulkExecuting: boolean;
    FIUnknownIsAssigned: boolean; //True, if FIUnknown was assigned with AssignToNextResult

    function ServerCursorUsed: boolean; virtual;
    procedure ClearHRowIfNeed;

    procedure Check(const Status: HRESULT; AnalyzeMethod: TAnalyzeMethod = nil; const Arg: IntPtr = nil); virtual;
    procedure CheckBCDOverflow(const FieldNo: integer {from 1}; RecBuf: IntPtr);
    function AnalyzeFieldsStatus(const Status: HRESULT; const Arg: IntPtr = nil): WideString;

    procedure QueryCommandInterfaces(const QueryPrepare: boolean); // Create ConnectionSwap if need. Call FCommand.QueryInterfaces.
    procedure ReleaseCommandInterfaces;
    procedure QueryRecordSetInterfaces; // Reqests IRowset, IRowsetLocate, IRowsetUpdate
    procedure ReleaseRecordSetInterfaces;
    procedure ReleaseAllInterfaces(const ReleaseMultipleResults: boolean);

    procedure CreateCommand; override;
    procedure SetCommand(Value: TCRCommand); override;

  { Open / Close }
    function NeedInitFieldsOnFetch: boolean; override;
    procedure InternalPrepare; override;
    procedure InternalUnPrepare; override;
    procedure InternalOpen(DisableInitFields: boolean = False); override;
    procedure InternalClose; override;

  { Fields }
    procedure InternalInitFields; override;
    function GetIndicatorSize: word; override;

  { Fetch }
    procedure InitFetchedBlock(Block: PBlockHeader; RowsObtained: Integer; FetchBack: boolean); override;
    procedure AllocFetchBlock; // Also create fetch accessors
    function Fetch(FetchBack: boolean = False): boolean; override;
    procedure FreeFetchBlock; // Also free fetch accessors
    function CanFetchBack: boolean; override; // Return True, if BlockMan is store only one block of records

  { Modify }
    procedure RowsetUpdateCommit;
    procedure RowsetUpdateRollback;
    procedure InternalAppend(RecBuf: IntPtr); override;
    procedure InternalDelete; override;
    procedure InternalUpdate(RecBuf: IntPtr); override;
    procedure InternalAppendOrUpdate(RecBuf: IntPtr; const IsAppend: boolean);

    procedure SetCommandProp;

    procedure RequestParamsIfPossible; // Call FCommand.RequestAndFreeParamAccs if interfaces is cleared

    //PreCached FConection properties
    property ProviderPrimaryVer: integer read GetProviderPrimaryVer;
    property DBMSPrimaryVer: integer read GetDBMSPrimaryVer;
    property ProviderID: TGuid read GetProviderId;
    property Provider: TOLEDBProvider read GetProvider;
    property DisconnectedMode: boolean read GetDisconnectedMode;
    property Database: _string read GetDatabase;
  public
    constructor Create; override;
    destructor Destroy; override;

  { Fields}
    function GetFieldDescType: TFieldDescClass; override;
    procedure ExplicitInitFields; override;

  { Open / Close }
    procedure ExecCommand; override;
    procedure Open; override;
    procedure Reopen; override;
    function GetSchemaRowset(const Schema: TGUID; rgRestrictions: TRestrictions): IRowset;
    procedure Disconnect; override;

  { Fetch }
    procedure FetchAll; override;
    procedure DoFetchAll; virtual;
    procedure DoFetchTerminate(Sender: TObject);
    procedure DoFetchException(Sender: TObject; E: Exception; var Fail: boolean);
    procedure DoFetchSendEvent(Sender: TObject; Event: TObject);
    procedure EndFetchAll(E: Exception); virtual;
    procedure BreakFetch; override;
    function CanDisconnect: boolean; override;

  { Fields }
    function GetNull(FieldNo: word; RecBuf: IntPtr): boolean; override;
    procedure SetNull(FieldNo: word; RecBuf: IntPtr; Value: boolean); override;
    function GetStatus(FieldNo: word; RecBuf: IntPtr): DWORD;
    procedure SetStatus(FieldNo: word; RecBuf: IntPtr; Value: DWORD);
    procedure GetFieldData(Field: TFieldDesc; FieldBuf: IntPtr; Dest: IntPtr); override;
    procedure GetField(FieldNo: word; RecBuf: IntPtr; Dest: IntPtr; var IsBlank: boolean); override;
    procedure GetFieldAsVariant(FieldNo: word; RecBuf: IntPtr; var Value: variant); override;
    procedure PutFieldData(Field: TFieldDesc; FieldBuf: IntPtr; Source: IntPtr; IsDatabaseValue: boolean = False); override;
    class function IsBlobFieldType(DataType: word): boolean; override;

  { Records }
    procedure InsertRecord(RecBuf: IntPtr); override;
    procedure UpdateRecord(RecBuf: IntPtr); override;
    procedure DeleteRecord; override;

  { Sorting }
    procedure SetIndexFieldNames(Value: _string); override;
    
  { Filter/Find/Locate }
    function IsCaseSensitive: boolean; override;

    procedure CreateComplexField(RecBuf: IntPtr; FieldIndex: integer; WithBlob: boolean); override;
    procedure CreateComplexFields(RecBuf: IntPtr; WithBlob: boolean); override;
    procedure FreeComplexFields(RecBuf: IntPtr; WithBlob: boolean); override;

  { Navigation }
    procedure SetToBegin; override;
    procedure SetToEnd; override;
    //procedure GetBookmark(Bookmark: PRecBookmark); override;
    procedure SetToBookmark(Bookmark: PRecBookmark); override;
    function FetchToBookmarkValue(FetchBack: boolean = False): boolean; // Fetch to Bookmark. Bookmark value is stored in FBookmarkValue. Bookmark value used only for ctStatic and ctKeyset. For ctDynamic method refetched current record in specified direction
    function CompareBookmarks(Bookmark1, Bookmark2: PRecBookmark): integer; override;

    function RowsReturn: boolean; override;

    function GetIRowset: IRowset;
    function GetICommandText: ICommandText;
    procedure SetIRowset(
      Rowset: IRowset;
      const IsColumnsRowset: boolean); // If True then FieldDescs was stored in FColumnsRowsetFieldDescs
    procedure AssignToNextResult(Dest: TOLEDBRecordSet);

    function GetProp(Prop: integer; var Value: variant): boolean; override;
    function SetProp(Prop: integer; const Value: variant): boolean; override;

    property NativeRowset: boolean read FNativeRowset;
    property CursorTypeChanged: TCursorTypeChangedProc read FCursorTypeChanged write FCursorTypeChanged;

    property FetchExecutor: TOLEDBThreadWrapper read FFetchExecutor;
    procedure SortItems; override;
  end;

  TOLEDBTableTypeRecordSet = class (TOLEDBRecordSet)
  protected
    FTableTypeName: string;

    function ServerCursorUsed: boolean; override;
    procedure OpenTVPRowset;
    function GetFilledRowset: IRowset;

    procedure InternalOpen(DisableInitFields: boolean = False); override;
    procedure InternalPrepare; override;
    function Fetch(FetchBack: boolean = False): boolean; override;
  public
    procedure ExplicitInitFields; override;
    function SetProp(Prop: integer; const Value: variant): boolean; override;
  end;

  TMSTableObject = class(TSharedObject)
  protected
    FTableTypeName: string;
    FRecordSet: TOLEDBTableTypeRecordSet;
    procedure ReleaseAllInterfaces;
  public
    function GetIsNull: boolean;
    procedure SetData(RecordSet: TOLEDBTableTypeRecordSet);
    property TableTypeName: string read FTableTypeName write FTableTypeName;
  end;

{ TOLEDBTransaction }

  TOLEDBTransaction = class(TCRTransaction)
  private
    FITransactionLocal: ITransactionLocal;

    procedure ReleaseInterfaces;

  public
    destructor Destroy; override;

    procedure AssignConnect(Source: TCRTransaction); override;

    procedure StartTransaction; override;
    procedure Commit; override;
    procedure Rollback; override;

    procedure Savepoint(const Name: _string); override;
    procedure RollbackToSavepoint(const Name: _string); override;

    procedure Check(const Status: HRESULT; Component: TObject;
      AnalyzeMethod: TAnalyzeMethod = nil; const Arg: IntPtr = nil); virtual;
  end;

{$IFDEF LITE}
  {$DEFINE DBX_METADATA}
{$ENDIF}

{$IFNDEF LITE}
{ TOLEDBMetaData }

  TOLEDBMetaData = class (TCRMetaData)
  private
    function GetSchemaNameFunc: string;
  protected
    FRecordSet: TOLEDBRecordSet;

    function CreateRecordSet: TCRRecordSet; override;

    procedure InternalGetMetaDataKindsList(List: _TStringList); override;

    function GetTables(Restrictions: _TStrings): TData; override;
    procedure CopyTablesData(Restrictions: _TStrings); virtual;

    function GetColumns(Restrictions: _TStrings): TData; override;
    procedure CopyColumnsData(Restrictions: _TStrings); virtual;

    function GetProcedures(Restrictions: _TStrings): TData; override;

    function GetProcedureParameters(Restrictions: _TStrings): TData; override;
    procedure CopyProcedureParametersData(Restrictions: _TStrings); virtual;

    function GetIndexes(Restrictions: _TStrings): TData; override;

    function GetIndexColumns(Restrictions: _TStrings): TData; override;
    procedure CopyIndexColumnsData(Restrictions: _TStrings); virtual;

    function GetConstraints(Restrictions: _TStrings): TData; override;
    procedure CopyConstraintsData(Restrictions: _TStrings); virtual;

    function GetDatabases(Restrictions: _TStrings): TData; override;
    procedure CopyDatabasesData(Restrictions: _TStrings); // TODO: DBX

  {$IFNDEF DBX_METADATA}
    procedure CreateTablesFields; override;
    procedure CreateColumnsFields; override;
    procedure CreateProceduresFields; override;
    procedure CreateProcedureParametersFields; override;
    procedure CreateIndexColumnsFields; override;
    procedure CreateConstraintsFields; override;
    procedure CreateDatabasesFields; override;
  {$ENDIF}
  end;

{$ENDIF}

{ TOLEDBLoader }

  TOLEDBLoaderColumn = class(TCRLoaderColumn)
  private
    FIsWide: boolean;
  public
    property IsWide: boolean read FIsWide write FIsWide;
  end;

  TDBIDAccessor = class
  protected
    FPDBID: PDBID;
    function GeteKind: DBKIND;
    procedure SeteKind(Value: DBKIND);
    function GetpwszName: IntPtr;
    procedure SetpwszName(Value: IntPtr);
    procedure SetPDBID(Value: PDBID);
  public
    constructor Create(APDBID: PDBID);
    property eKind: DBKIND read GeteKind write SeteKind;
    property pwszName: IntPtr read GetpwszName write SetpwszName;
  end;

  TOLEDBLoader = class(TCRLoader)
  private
    FConnection: TOLEDBConnection;
    FIOpenRowset: IOpenRowset;
    FIRowsetFastLoad: IRowsetFastLoad;
    FParamsAccessorData: TParamsAccessorData;
    FParamDescs: TParamDescs;
    FBlobList: TObjectList;

    FKeepIdentity: boolean;
    FKeepNulls: boolean;
    FRowsPerBatch: integer;
    FKilobytesPerBatch: integer;
    FLockTable: boolean;
    FCheckConstraints: boolean;

  protected
    class function GetRecordSetClass: TCRRecordSetClass; override;
    procedure SetConnection(Value: TCRConnection); override;
    procedure FillColumn(Column: TCRLoaderColumn; FieldDesc: TFieldDesc); override;

    procedure LoadRow;

  public
    constructor Create; override;
    destructor Destroy; override;

    function SetProp(Prop: integer; const Value: variant): boolean; override;
    function GetProp(Prop: integer; var Value: variant): boolean; override;

    class function GetColumnClass: TCRLoaderColumnClass; override;
    procedure Prepare; override;
    procedure PutColumnData(Col: integer; Row: integer; const Value: variant); override;
    procedure DoLoad; override;
    procedure Finish; override;
  end;

{ TOLEDBPropertiesSet }

  TOLEDBPropertiesSet = class
  protected
    FConnection: TOLEDBConnection;
    FInitPropSet: PDBPROPSET;

    procedure Check(const Status: HRESULT);
    function GetInitPropSetStatus: _string;
    function GetDBPropPtr(Index: UINT): PDBProp;

    function InitProp(const dwPropertyID: DBPROPID; const Required: boolean = False): PDBProp;

  public
    constructor Create(Connection: TOLEDBConnection; const GuidPropertySet: TGUID);
    destructor Destroy; override;

//    procedure AddProp(const dwPropertyID: DBPROPID; const Value: OleVariant);
    procedure AddPropSmallInt(const dwPropertyID: DBPROPID; const Value: smallint);

    procedure AddPropInt(const dwPropertyID: DBPROPID; const Value: integer);
    procedure AddPropUInt(const dwPropertyID: DBPROPID; const Value: Cardinal);
    procedure AddPropBool(const dwPropertyID: DBPROPID; const Value: boolean; const Required: boolean = False);
    procedure AddPropStr(const dwPropertyID: DBPROPID; const Value: _string; const Required: boolean = False);
    procedure AddPropIntf(const dwPropertyID: DBPROPID; const Value: TInterfacedObject; const Required: boolean = False);

    procedure SetProperties(Obj: IDBProperties); overload;
    procedure SetProperties(Obj: ISessionProperties); overload;
    procedure SetProperties(Obj: ICommandProperties); overload;

    property InitPropSet: PDBPROPSET read FInitPropSet;
  end;

{ TOLEDBPropertiesGet }

  TPropValues = array of Variant;

  TOLEDBPropertiesGet = class
  protected
    FConnection: TOLEDBConnection;
    FInitPropSet: PDBPROPSET;

    FPropIds: array of DBPROPID;
    FPropIdsGC: IntPtr;

    procedure Check(const Status: HRESULT);
    function GetDBPropPtr(rgProperties: PDBPropArray; Index: UINT): PDBProp;

    procedure PrepareToGet;
    procedure ProcessResult(rgPropertySets: PDBPropSet; var PropValues: TPropValues);
    procedure ClearResult(rgPropertySets: PDBPropSet);

  public
    constructor Create(Connection: TOLEDBConnection; const GuidPropertySet: TGUID);
    destructor Destroy; override;

    procedure AddPropId(Id: DBPROPID);

    procedure GetProperties(Obj: IDBProperties; var PropValues: TPropValues); overload;
    procedure GetProperties(Obj: IRowsetInfo; var PropValues: TPropValues); overload;
    procedure GetProperties(Obj: ICommandProperties; var PropValues: TPropValues); overload;
  end;

{ TOLEDBErrors }
  TOLEDBErrors = class
  protected
    FList: TList;

    function GetCount: integer;
    function GetError(Index: Integer): EOLEDBError;

    procedure Assign(Source: TOLEDBErrors);
    procedure Add(Source: TOLEDBErrors);
    procedure Clear;

  public
    constructor Create;
    destructor Destroy; override;

    property Count: integer read GetCount;
    property Errors[Index: Integer]: EOLEDBError read GetError; default;
  end;

{ EOLEDBError }

{$IFDEF LITE}
  EOLEDBError = class(Exception)
  protected
    FErrorCode: integer;

  public
    property ErrorCode: integer read FErrorCode;

{$ELSE}
  EOLEDBError = class(EDAError)
{$ENDIF}
  protected
    FErrors: TOLEDBErrors;
    FOLEDBErrorCode: integer;

    // GetBasicErrorInfo - ERRORINFO struct
    // FhrError: HResult; - equal to FOLEDBErrorCode
    // FMinor: UINT; - only for EMSError and equal to FMSSQLErrorCode
    // Fclsid: TGUID; - always CLSID_SQLOLEDB
    Fiid: TGUID;
    // Fdispid: Integer; - always 0

    // GetErrorInfo - IErrorInfo interface
    // Fguid: TGUID; - same as Fiid
    // FSource: WideString; - always "Microsoft OLE DB Provider for SQL Server"
    // FDescription: WideString; - equal to Message
    // FHelpFile: WideString; - not used by sqloledb
    // FHelpContext: Longint; - not used by sqloledb

    FMessageWide: WideString;

    function GetErrorCount: integer;
    function GetError(Index: Integer): EOLEDBError;

    procedure Assign(Source: EOLEDBError); virtual;

  protected
    property iid: TGUID read Fiid;

  public
    constructor Create(ErrorCode: integer; Msg: WideString);
    destructor Destroy; override;

    property ErrorCount: integer read GetErrorCount;
    property Errors[Index: Integer]: EOLEDBError read GetError; default;

    property OLEDBErrorCode: integer read FOLEDBErrorCode;
    property MessageWide: WideString read FMessageWide;
    // property hrError: HResult read FhrError;
    // property Minor: UINT read FMinor;
    // property clsid: TGUID read Fclsid;
    // property iid: TGUID read Fiid; - protected
    // property dispid: Integer read Fdispid;
    // property Source: WideString read FSource;
    // property Description: WideString read FDescription;
  end;


{ EMSError }

  EMSError = class(EOLEDBError)
  protected
    FMSSQLErrorCode: integer;

    FServerName: _string;
    FProcName: _string;
    FState: BYTE;
    FSeverityClass: BYTE;
    FLineNumber: WORD;
    FLastMessage: _string;

    procedure Assign(Source: EOLEDBError); override;

  public
    constructor Create(
      const pServerErrorInfo: SSERRORINFO;
      OLEDBErrorCode: integer;
      Msg: WideString); overload;

  {$IFNDEF LITE}
    function IsFatalError: boolean; override;
    function IsKeyViolation: boolean; override;
  {$ENDIF}
    property MSSQLErrorCode: integer read FMSSQLErrorCode;
    property ServerName: _string read FServerName;
    property ProcName: _string read FProcName;
    property State: BYTE read FState;
    property SeverityClass: BYTE read FSeverityClass;
    property LineNumber: WORD read FLineNumber;

    property LastMessage: _string read FLastMessage;
  end;

  TStorageType = (stBlob, stStream);

  TOLEDBStream = class(TInterfacedObject, ISequentialStream)
  protected
    FSize: UINT;
    FBlob: TBlob;
    FStream: TStream;
    FStorageType: TStorageType;
    FOutputEncoding: TOLEDBOutputEncoding;
    FPosInBlob: cardinal;
    FStreamList: TList;
    procedure InitStream(StreamList: TList);
  public
  {$IFDEF CLR}
    [PreserveSig]
  {$ENDIF}
  {$IFNDEF FPC}
    function Read(pv: IntPtr; cb: Longint; pcbRead: PLongint): HResult;{$IFNDEF CLR} stdcall;{$ENDIF}
  {$ELSE}
    function Read(pv: Pointer; cb: DWord; pcbRead: PDWord): HRESULT; stdcall;
  {$ENDIF}
  {$IFDEF CLR}
    [PreserveSig]
  {$ENDIF}
  {$IFNDEF FPC}
    function Write(pv: IntPtr; cb: Longint; pcbWritten: PLongint): HResult;{$IFNDEF CLR} stdcall;{$ENDIF}
  {$ELSE}
    function Write(pv: Pointer; cb: DWord; pcbWritten: PDWord): HRESULT; stdcall;
  {$ENDIF}
  public
    constructor Create(Blob: TBlob; StreamList: TList); overload;
    constructor Create(Stream: TStream; StreamList: TList); overload;
    destructor Destroy; override;

    property Size: UINT read FSize; // Return size of stream in bytes
    property OutputEncoding: TOLEDBOutputEncoding read FOutputEncoding write FOutputEncoding;
  end;

  function ConvertInternalTypeToOLEDB(const InternalType: word; const IsParam: boolean;
    ServerVersion: Integer): word;

  function ConvertOLEDBTypeToInternalFormat(
    const OLEDBType: DBTYPE;
    const IsLong: Boolean;
    const EnableBCD, EnableFMTBCD, SensibleBCDMapping: boolean;
    const Precision, Scale: integer;
    const WideStrings, WideMemos: boolean;
    const IsParam: boolean;
    var InternalType: word; ServerVersion: Integer): boolean;

  function DBNumericToBCD(Value: TDBNumeric): TBCD;
  function BcdToDBNumeric(const Bcd: TBcd): TDBNumeric;
  
  function DBNumericToDouble(Value: TDBNumeric): double;
  function DoubleToDBNumeric(Value: double; Precision, Scale: integer): TDBNumeric;

  function IsLargeDataTypeUsed(const FieldDesc: TFieldDesc): boolean; overload;
  function IsLargeDataTypeUsed(const ParamDesc: TParamDesc): boolean; overload;

  function GetProviderName(const Provider: TOLEDBProvider; const CompactVersion: TCompactVersion = cvAuto;
    const NativeClientVersion: TNativeClientVersion = ncAuto): _string;
  function GetProvider(const ProviderName: _string): TOLEDBProvider;
  function GetCompactVersion(const ProviderName: _string): TCompactVersion;
  function GetNativeClientVersion(const ProviderName: _string): TNativeClientVersion;

  function GenerateTableName(const CatalogName: _string;
    const SchemaName: _string;
    const TableName: _string;
    const DefaultCatalogName: _string): _string;

  function ChangeDecimalSeparator(const Value: Variant): _string;
  function GetParamNameWODog(const ParamName: _string): _string;
  function IsCompactEdition(ServerVersion: integer): boolean;

{$IFDEF CLR}
  procedure QueryIntf(Source: IUnknown; const IID: TInterfaceRef; out Intf); // OLE QueryInterface analog
{$ELSE}
  procedure QueryIntf(Source: IUnknown; const IID: TGuid; out Intf); // OLE QueryInterface analog
{$ENDIF}

{$IFDEF DEBUG}
var
  StreamCnt: integer = 0;
{$ENDIF}

var
  __UseRPCCallStyle: boolean; // temporary
  ParamsInfoOldBehavior: boolean; // do not remove (cr-s23142)
  OldProvidersSupport: boolean = False;
  OLEDBSQLInfo: TOLEDBSQLInfo;
  UniqueFieldNamePrefix: _string = '__COLUMN';

//procedure AddInfoToErr(var S: string; const FormatStr: string; const Args: array of const); overload; // Add additional info to exception message
procedure AddInfoToErr(var E: Exception; const FormatStr: _string; const Args: array of const); overload; // Add additional info to exception message

{$IFDEF AUTOTEST}
var
  __ServerPrepareCount: integer;
  __ServerExecuteCount: integer;
  __SetCommandPropCount: integer;
  __SetRecordSetCommandPropCount: integer;
{$ENDIF}

const
  LeftQuote = '[';
  RightQuote = ']';
  EmptyString = '';
  EmptyWString = WideString('');
const
  BytesByRef = [dtBlob, dtBytes, dtVarBytes];
  CharsByRef = [dtMemo, dtWideMemo, dtMSXML, dtString, dtWideString];

procedure FillBindingForParam(Ordinal: integer; ParamDesc: TOLEDBParamDesc; Connection: TOLEDBConnection;
  var pBind: TDBBinding; var BindMemorySize: NativeUInt; const ValueAvaliable: boolean; const IsWide: boolean);

procedure SaveParamValue(const ParamDesc: TParamDesc; const pBind: TDBBinding;
  var ParamsAccessorData: TParamsAccessorData
  {$IFDEF HAVE_COMPRESS}; const CompressBlobMode: TCompressBlobMode{$ENDIF}
  {$IFDEF CLR}; var ParamsGC: TIntPtrDynArray{$ENDIF}; ServerVersion, ClientVersion: integer);

{$IFDEF FPC}
function DispGetParam(const dispparams: TDispParams; position: Integer;
  vtTarg: TVarType; var varResult: TVarData; var puArgErr: Integer): HResult; stdcall;
  external 'oleaut32.dll' name 'DispGetParam';
{$ENDIF}

function LocaleIdentifierToStr(LCID: Cardinal): _string;
function LocaleIdentifierToCardinal(LCID: _string): Cardinal;
procedure GetLocaleIdentifierList(List: TStrings);

implementation

uses
  Math,
{$IFDEF VER6P}
  DateUtils, {$IFNDEF FPC}SqlTimSt,{$ENDIF}
{$ENDIF}
{$IFDEF CLR}
  System.Text, System.Globalization, System.IO, System.Threading,
{$ELSE}
{$IFDEF VER6P}
  Variants,
{$ENDIF}
{$IFDEF FPC}
  VarUtils,
{$ENDIF}
{$ENDIF}
  CRFunctions, MemUtils, {$IFNDEF UNIDACPRO}MSParser;{$ELSE}MSParserUni;{$ENDIF}

const
  MaxLength = 130;
  SizeOfLongWord = 4;

type
{$IFDEF CLR}
  [StructLayout(LayoutKind.Sequential, Pack = 1, CharSet = CharSet.Auto)]
{$ENDIF}
  RBigInteger = packed record
    Data: array[0..MaxLength - 1] of LongWord;
    DataLength: integer;
  end;

const
  SizeOfRBigInteger = 524;

type
  PBigInteger = IntPtr;

  TBigIntegerAccessor = class
  protected
    FSelfAllocated: boolean;
    FBigIntegerPtr: PBigInteger;
    procedure SetBigIntegerPtr(Value: PBigInteger);
    function GetDataLength: integer;
    procedure SetDataLength(Value: integer);
    function GetDataPtr: IntPtr;
    function GetData(Index: integer): LongWord;
    procedure SetData(Index: integer; Value: LongWord);
    function GetBigInteger: RBigInteger;
    procedure SetBigInteger(const Value: RBigInteger);
  public
    constructor Create(BigIntegerPtr: PBigInteger; Allocate: boolean);
    destructor Destroy; override;
    procedure ClearData;
    property DataLength: integer read GetDataLength write SetDataLength;
    property DataPtr: IntPtr read GetDataPtr;
    property Data[Index: integer]: LongWord read GetData write SetData;
    property BigIntegerPtr: PBigInteger read FBigIntegerPtr write SetBigIntegerPtr;
    property BigInteger: RBigInteger read GetBigInteger write SetBigInteger;
  end;

  TBigInteger = class
  public
    class procedure CreatePBigIntegerPtr(PBigInt: PBigInteger);
    class procedure CreateRBigIntegerStruct(var BigInt: RBigInteger);
    class procedure CreatePBigInteger64(PBigInt: PBigInteger; Value: Int64);
    class procedure CreatePBigIntegerBCD(PBigInt: PBigInteger; const Value: TBcd);

    class function Add(PBigInt1: PBigInteger; PBigInt2: PBigInteger): RBigInteger;
    class function NEGATEPtr(PBigInt: PBigInteger): RBigInteger;
    class function NEGATE(const BigInt: RBigInteger): RBigInteger;
    class function Mul(PBigInt1: PBigInteger; PBigInt2: PBigInteger): RBigInteger;
    class function Mul64(PBigInt1: PBigInteger; Value: Int64): RBigInteger;
  end;

var
  dstSmallint: IntPtr;
  dstInt: IntPtr;
  dstReal: IntPtr;
  dstFloat: IntPtr;
  dstMoney: IntPtr;
  dstDateTime: IntPtr;
  dstNVarChar: IntPtr;
  dstNVarCharMax: IntPtr;
  dstVarChar: IntPtr;
  dstVarCharMax: IntPtr;

  dstBit: IntPtr;
  dstTinyInt: IntPtr;
  dstBigint: IntPtr;
  dstSql_variant: IntPtr;
  dstImage: IntPtr;
  dstBinary: IntPtr;
  dstVarBinary: IntPtr;
  dstGuid: IntPtr;
  dstXML: IntPtr;
  dstTable: IntPtr;

var
  IsWindowsVista: boolean;
{$IFNDEF CLR}
  GlobaIMalloc: IMalloc;
{$ENDIF}
  LocaleIdentifierList: TStringList = nil;
  LocalesCallbackDelegate: {$IFDEF FPC}TFarProc{$ELSE}TFNLocaleEnumProc{$ENDIF};

function GetLocaleData(ID: LCID; Flag: DWORD): string;
var
{$IFDEF CLR}
  SBBuffer: StringBuilder;
{$ENDIF}
  BufSize: Integer;
begin
  BufSize := GetLocaleInfo(ID, Flag, nil, 0);
{$IFDEF CLR}
  SBBuffer := StringBuilder.Create(BufSize);
  try
{$ENDIF}
    SetLength(Result, BufSize);
    GetLocaleInfo(ID, Flag, {$IFDEF CLR}SBBuffer{$ELSE}PChar(Result){$ENDIF}, BufSize);
{$IFNDEF CLR}
    SetLength(Result, BufSize - 1);
{$ELSE}
    Result := SBBuffer.ToString;
  finally
    SBBuffer.Free;
  end;
{$ENDIF}
end;

function LocalesEnumProc(Locale: {$IFDEF CLR}IntPtr{$ELSE}PChar{$ENDIF}): integer; {$IFNDEF CLR}stdcall;{$ENDIF}
var
  LCID: Cardinal;
begin
  LCID := StrToInt('$' + Copy({$IFDEF CLR}Marshal.PtrToStringUni{$ENDIF}(Locale), 5, 4));
  Assert(LocaleIdentifierList <> nil);
  LocaleIdentifierList.AddObject(GetLocaleData(LCID, LOCALE_SLANGUAGE), TObject(LCID));
  Result := 1;
end;

procedure CheckLocaleIdentifierList;
begin
  if LocaleIdentifierList = nil then begin
    LocaleIdentifierList := TStringList.Create;
  {$IFDEF VER6P}
    LocaleIdentifierList.CaseSensitive := False;
  {$ENDIF}
    LocaleIdentifierList.Duplicates := dupIgnore;
    LocaleIdentifierList.Sorted := True;
    LocalesCallbackDelegate := {$IFNDEF CLR}@{$ENDIF}LocalesEnumProc;
    EnumSystemLocales(LocalesCallbackDelegate, LCID_INSTALLED);
  end;
end;

function LocaleIdentifierToStr(LCID: Cardinal): _string;
var
  k: integer;
begin
  CheckLocaleIdentifierList;
  k := LocaleIdentifierList.IndexOfObject(TObject(LCID));
  if k <> -1 then
    Result := LocaleIdentifierList[k]
  else
    raise Exception.Create(SLocaleIdentifierUnknown);
end;

function LocaleIdentifierToCardinal(LCID: _string): Cardinal;
var
  e, k: Integer;
  _lcid: Cardinal;
begin
  CheckLocaleIdentifierList;
  Val(LCID, _lcid, e);
  if e = 0 then
    Result := _lcid
  else begin
    k := LocaleIdentifierList.IndexOf(LCID);
    if k = -1 then
      raise Exception.Create(SLocaleIdentifierUnknown)
    else
      Result := Cardinal(LocaleIdentifierList.Objects[k]);
  end;
end;

procedure GetLocaleIdentifierList(List: TStrings);
begin
  CheckLocaleIdentifierList;
  List.Assign(LocaleIdentifierList);
end;

procedure FreeCoMem(ptr: IntPtr);
begin
{$IFDEF CLR}
  Marshal.FreeCoTaskMem(ptr);
{$ELSE}
  if GlobaIMalloc = nil then
    CoGetMalloc(1, GlobaIMalloc);
  GlobaIMalloc.Free(ptr);
{$ENDIF}
end;

function VarAsBooleanType(const Value: Boolean): Variant;
begin
  Result := VarAsType(Value, VT_BOOL);
{$IFDEF FPC}
  if Value then
    TVardata(Result).vsmallint := -1;
{$ENDIF}
end;

function IsCompactEdition(ServerVersion: integer): boolean;
begin
  Result := (ServerVersion = 3) or (ServerVersion = 4);
end;

{$IFDEF LITE}
procedure DatabaseError(const Message: string; Component: TComponent = nil);
begin
  if Assigned(Component) and (Component.Name <> '') then
    raise Exception.Create(Format('%s: %s', [Component.Name, Message])) else
    raise Exception.Create(Message);
end;

procedure DatabaseErrorFmt(const Message: string; const Args: array of const;
  Component: TComponent = nil);
begin
  DatabaseError(Format(Message, Args), Component);
end;
{$ENDIF}

procedure AddInfoToErr(var S: WideString; const FormatStr: _string; const Args: array of const); overload; // Add additional info to exception message
var
  s1: WideString;
begin
  s1 := _Format(FormatStr, Args);
  if s1 <> '' then
    S := S + #10 + s1;
end;

procedure AddInfoToErr(var E: Exception; const FormatStr: _string; const Args: array of const); overload; // Add additional info to exception message
var
  S: WideString;
{ $IFDEF CLR}
  ENew: Exception;
{ $ENDIF}
begin
  if IsClass(E, EOLEDBError) then
    S := EOLEDBError(E).MessageWide
  else
    S := E.Message;
  AddInfoToErr(S, FormatStr, Args);
{ $IFDEF CLR}
  if IsClass(E, EMSError) then begin
    ENew := EMSError.Create(EMSError(E).ErrorCode, S);
    EMSError(ENew).Assign(EMSError(E));
  end
  else
  if IsClass(E, EOLEDBError) then begin
    ENew := EOLEDBError.Create(EOLEDBError(E).ErrorCode, S);
    EOLEDBError(ENew).Assign(EOLEDBError(E));
  end
  else
{$IFNDEF LITE}
  if IsClass(E, EDAError) then
    ENew := EDAError.Create(EDAError(E).ErrorCode, S)
  else
{$ENDIF}
    ENew := Exception.Create(S);
  E := ENew;
{ $ELSE}
//  E.Message := S;
{ $ENDIF}
end;

procedure AddInfoToErr(var E: EOLEDBError; const PrevError: EOLEDBError); overload;
var
  Msg: WideString;
  ErrorCode: integer;
  ENew: EOLEDBError;
begin
  Assert(E <> nil);

  if PrevError <> nil then begin
    Msg := E.MessageWide + WideString(#$D#$A) + PrevError.MessageWide;
    ErrorCode := PrevError.ErrorCode;
  end
  else begin
    Msg := E.MessageWide;
    ErrorCode := E.ErrorCode;
  end;

  if IsClass(E, EMSError) or IsClass(PrevError, EMSError) then
    ENew := EMSError.Create(ErrorCode, Msg)
  else
    ENew := EOLEDBError.Create(ErrorCode, Msg);

  if PrevError <> nil then begin
    ENew.Assign(PrevError);
    ENew.FErrors.Assign(E.FErrors); // E.Errors must be first
    ENew.FErrors.Add(PrevError.FErrors); // PrevError.Errors must be last
  end
  else
    ENew.Assign(E);

  E := ENew;
end;

{$WARNINGS OFF}
function ConvertInternalTypeToOLEDB(const InternalType: word; const IsParam: boolean;
  ServerVersion: Integer): word;
begin
  case InternalType of
    // Integer fields
    dtBoolean:
      Result := DBTYPE_BOOL;
    dtInt8:
      if IsCompactEdition(ServerVersion) then
        Result := DBTYPE_UI1
      else
        Result := DBTYPE_I1;
    dtWord:
      if IsCompactEdition(ServerVersion) then
        Result := DBTYPE_UI2
      else
        Result := DBTYPE_I2;
    dtInt16:
      Result := DBTYPE_I2;
    dtInt32:
      Result := DBTYPE_I4;
    dtUInt32:
      Result := DBTYPE_UI4;
    dtInt64:
      Result := DBTYPE_I8;

    // Float fields
    dtFloat:
      Result := DBTYPE_R8;
    dtCurrency:
      if IsCompactEdition(ServerVersion) then
        Result := DBTYPE_CY
      else
        Result := DBTYPE_R8;
        // Result := DBTYPE_CY; Currency type cannot be used over TCurrencyField uses double to store
    // Multibyte fields
    dtDateTime{$IFDEF VER6P}{$IFNDEF FPC}, dtSQLTimeStamp{$ENDIF}{$ENDIF}:
      if IsCompactEdition(ServerVersion) then
        Result := DBTYPE_DBTIMESTAMP
      else
        Result := DBTYPE_DATE;
    dtDate:
      Result := DBTYPE_DATE;
    dtTime:
      Result := DBTYPE_DATE;
    dtString:
      Result := DBTYPE_STR;
    dtWideString:
      Result := DBTYPE_WSTR;
    dtExtString:
      Result := DBTYPE_STR;
    dtExtWideString:
      Result := DBTYPE_WSTR;
    dtBytes, dtVarBytes:
      Result := DBTYPE_BYTES;
    dtExtVarBytes:
      Result := DBTYPE_BYTES;
    dtMSXML:
      if IsParam then
        Result := DBTYPE_VARIANT
      else
        Result := DBTYPE_IUNKNOWN;
    dtMemo, dtWideMemo, dtBlob:
      Result := DBTYPE_IUNKNOWN;
  {$IFDEF VER5P}
    dtGuid:
      Result := DBTYPE_GUID;
    dtVariant:
      Result := DBTYPE_VARIANT;
  {$ENDIF}
    dtBCD:
      if IsCompactEdition(ServerVersion) then
        Result := DBTYPE_NUMERIC
      else
        Result := DBTYPE_CY;
  {$IFDEF VER6P}
  {$IFNDEF FPC}
    dtFmtBCD:
        Result := DBTYPE_NUMERIC;
  {$ENDIF}
  {$ENDIF}
    dtTable:
      Result := DBTYPE_TABLE;
    dtUnknown:
      Result := DBTYPE_VARIANT;
    else
      Assert(False, Format('Invalid internal field type $%X (%d)', [InternalType, InternalType]));
  end;
end;
{$WARNINGS ON}

function ConvertOLEDBTypeToInternalFormat(
  const OLEDBType: DBTYPE;
  const IsLong: boolean;
  const EnableBCD, EnableFMTBCD, SensibleBCDMapping: boolean;
  const Precision, Scale: integer;
  const WideStrings, WideMemos: boolean;
  const IsParam: boolean;
  var InternalType: word; ServerVersion: Integer): boolean;
begin
  Result := True;
  case OLEDBType of // List of types must be synchronized with InternalInitFields types list
    // Integer fields
    DBTYPE_BOOL:
      InternalType := dtBoolean;
    DBTYPE_UI1:
      InternalType := dtWord;
    DBTYPE_I2:
      InternalType := dtInt16;
    DBTYPE_I4:
      InternalType := dtInt32;
    DBTYPE_UI2, DBTYPE_UI4:{WAR For OLE DB info only. Signed/unsigned conversion}
      if IsCompactEdition(ServerVersion) then
        case OLEDBType of
          DBTYPE_UI2:
            InternalType := dtUInt16;
          DBTYPE_UI4:
            InternalType := dtUInt32;
        end
      else
        InternalType := dtInt32;
    DBTYPE_I8, DBTYPE_UI8:
      InternalType := dtInt64;

    // Float fields
    DBTYPE_NUMERIC:
      if SensibleBCDMapping and EnableBCD and (Precision <= MaxBCDPrecision - MaxBCDScale + 1) and (Scale <= MaxBCDScale) then //max currency value
        InternalType := dtBCD
      else
    {$IFDEF VER6P}
    {$IFNDEF FPC}
      if EnableFMTBCD then
        InternalType := dtFmtBCD
      else
    {$ENDIF}
    {$ENDIF}
      if not SensibleBCDMapping and EnableBCD then
        InternalType := dtBCD
      else
        InternalType := dtFloat;
    DBTYPE_R4, DBTYPE_R8:
      InternalType := dtFloat;
    DBTYPE_CY:
      InternalType := dtCurrency;

    // Multibyte fields
    DBTYPE_DBTIMESTAMP, DBTYPE_DATE:
      InternalType := dtDateTime;
    DBTYPE_STR:
    begin
      if IsLong then
        InternalType := dtMemo
      else
        InternalType := dtString;
    end;
    DBTYPE_WSTR:
    begin
      if IsLong then begin
        if WideStrings and WideMemos then
          InternalType := dtWideMemo
        else
          InternalType := dtMemo;
      end
      else
        if WideStrings then
          InternalType := dtWideString
        else
          InternalType := dtString;
    end;
    DBTYPE_BYTES:
    begin
      if IsLong then
        InternalType := dtBlob
      else
        InternalType := dtBytes;
    end;
    DBTYPE_GUID:
    {$IFDEF VER5P}
      InternalType := dtGuid;
    {$ELSE}
      InternalType := dtString;
    {$ENDIF}
    DBTYPE_VARIANT:
    {$IFDEF VER5P}
    {$IFDEF LITE}
      InternalType := dtString;
    {$ELSE}
    {$IFDEF CLR}
      if IsParam and (ServerVersion = 9) then
        InternalType := dtVariant
      else
        InternalType := dtString;
    {$ELSE}
      InternalType := dtVariant;
    {$ENDIF}
    {$ENDIF}
    {$ELSE}
      InternalType := dtString;
    {$ENDIF}
    DBTYPE_XML:
    {$IFDEF LITE}
      InternalType := dtMemo; // CR-DBXSDA 22872
    {$ELSE}
      InternalType := dtMSXML;
    {$ENDIF}
    DBTYPE_UDT:
      InternalType := dtBlob; //Subdata type dtMSUDT
    DBTYPE_DBDATE:
      InternalType := dtDate;
    DBTYPE_DBTIME:
      InternalType := dtTime;
    DBTYPE_DBDATETIMEOFFSET:
      InternalType := dtDateTime;
    DBTYPE_TABLE:
      InternalType := dtTable;
    else
      Result := False;
  end;
end;

function ConvertCRParamTypeToOLEDB(const InternalType: TParamDirection): word;
begin
  case InternalType of       
    pdUnknown:
      Result := DBPARAMIO_INPUT;
    pdInput:
      Result := DBPARAMIO_INPUT;
    pdOutput, pdResult:
      Result := DBPARAMIO_OUTPUT;
    pdInputOutput:
      Result := DBPARAMIO_INPUT + DBPARAMIO_OUTPUT;
    else
      Result := DBPARAMIO_INPUT;
  end;
end;

{$WARNINGS OFF}
function ConvertOLEDBParamTypeToCR(const Value: word): TParamDirection;
begin
  case Value of
    DBPARAMTYPE_INPUT:
      Result := pdInput;
    DBPARAMTYPE_INPUTOUTPUT:
      Result := pdInputOutput;
    DBPARAMTYPE_RETURNVALUE:
      Result := pdResult;
    else
      Assert(False, Format('Invalid value %d', [Value]));
  end;
end;
{$WARNINGS ON}

{$WARNINGS OFF}
function ConvertIsolationLevelToOLEDBIsoLevel(const Value: TCRIsolationLevel):Integer;
begin
  case Value of
    ilReadCommitted:
      Result := ISOLATIONLEVEL_READCOMMITTED;
    ilReadUnCommitted:
      Result := ISOLATIONLEVEL_READUNCOMMITTED;
    ilRepeatableRead:
      Result := ISOLATIONLEVEL_REPEATABLEREAD;
    ilIsolated:
      Result := ISOLATIONLEVEL_ISOLATED;
    ilSnapshot:
      Result := ISOLATIONLEVEL_SNAPSHOT;
  else
    raise Exception.Create(SUnsupportedIsolationLevel);
  end;
end;
{$WARNINGS ON}

{$IFNDEF CLR}
const
  SizeOfFraction = 32;
{$ENDIF}

function DBNumericToBCD(Value: TDBNumeric): TBCD;
var
  i, j, k: integer;
  SignificantBytes: integer;
  Remainder, tmp: word;
{$IFDEF CLR}
  ResultCLR: array[0..33] of byte;
{$ENDIF}
  function FindStart: boolean;
  begin
    SignificantBytes := 16;
    while (SignificantBytes > 0) and (Value.Val[SignificantBytes - 1] = 0) do
      dec(SignificantBytes);
    Result := SignificantBytes > 0;
  end;

begin
  if Value.Sign = 0 then
    {$IFDEF CLR}ResultCLR[1]{$ELSE}Result.SignSpecialPlaces{$ENDIF} := (1 shl 7) + Value.Scale
  else
    {$IFDEF CLR}ResultCLR[1]{$ELSE}Result.SignSpecialPlaces{$ENDIF} := (0 shl 7) + Value.Scale;

  if not FindStart then begin
  {$IFDEF CLR}
    for i := 2 to 33 do
      ResultCLR[i] := 0;
    Result.FromBytes(ResultCLR);
  {$ELSE}
    System.FillChar(Result.Fraction, Length(Result.Fraction), 0);
  {$ENDIF}

  {$IFDEF CLR}ResultCLR[0]{$ELSE}Result.Precision{$ENDIF} := 8;       // if value is zero
  {$IFDEF CLR}ResultCLR[1]{$ELSE}Result.SignSpecialPlaces{$ENDIF} := 2;

  {$IFDEF CLR}
    Result.FromBytes(ResultCLR);
  {$ENDIF}
    Exit;
  end;

  {$IFDEF CLR}
    for i := 2 to 33 do
      ResultCLR[i] := 0;
    Result.FromBytes(ResultCLR);
  {$ELSE}
    System.FillChar(Result.Fraction, Length(Result.Fraction), 0);
  {$ENDIF}

  k := SignificantBytes - 1;
  j := 31;
  while k >= 0 do begin
    Remainder := 0;
    for i := k downto 0 do begin
      tmp := (Byte(Value.Val[i]) + Remainder);
      Value.Val[i] := tmp div 100;
      Remainder := (tmp mod 100) shl 8;
    end;
    {$IFDEF CLR}ResultCLR[j + 2]{$ELSE}Result.Fraction[j]{$ENDIF} := (((Remainder shr 8) mod 10)) + (((Remainder shr 8) div 10) shl 4);
    if Value.Val[k] = 0 then
      dec(k);
    dec(j);
  end;

  {$IFDEF CLR}ResultCLR[0]{$ELSE}Result.Precision{$ENDIF} := Value.Precision;
  i := 31 - (Value.Precision div 2);
  j := i;

  if (Value.Precision mod 2) = 1 then begin
    while i <= 30 do begin
      {$IFDEF CLR}ResultCLR[i - j + 2]{$ELSE}Result.Fraction[i - j]{$ENDIF} :=
        ({$IFDEF CLR}ResultCLR[i + 2]{$ELSE}Result.Fraction[i]{$ENDIF} and $0f) shl 4 +
        {$IFDEF CLR}ResultCLR[i + 1 + 2]{$ELSE}Result.Fraction[i + 1]{$ENDIF} shr 4;
      inc(i);
    end;
    {$IFDEF CLR}ResultCLR[i - j + 2]{$ELSE}Result.Fraction[i - j]{$ENDIF} :=
      ({$IFDEF CLR}ResultCLR[i + 2]{$ELSE}Result.Fraction[i]{$ENDIF} and $0f) shl 4;
  end
  else begin
    while i <= 30 do begin
      {$IFDEF CLR}ResultCLR[i - j + 2]{$ELSE}Result.Fraction[i - j]{$ENDIF} :=
        {$IFDEF CLR}ResultCLR[i + 1 + 2]{$ELSE}Result.Fraction[i + 1]{$ENDIF};
      inc(i);
    end;
  end;

  for i := 31 - j + 1 to 31 do
    {$IFDEF CLR}ResultCLR[i + 2]{$ELSE}Result.Fraction[i]{$ENDIF} := 0;
{$IFDEF CLR}
  Result := TBcd.FromBytes(ResultCLR);
{$ENDIF}
end;

{$IFNDEF VER6P}
function IsBcdNegative(const Bcd: TBcd): Boolean;
begin
  Result := (Bcd.SignSpecialPlaces and (1 shl 7)) <> 0;
end;
{$ENDIF}

function BcdToDBNumeric(const Bcd: TBcd): TDBNumeric;
var
  Value: TBigIntegerAccessor;
  BytesCount: integer;
{$IFDEF CLR}
  i: integer;
{$ENDIF}  
begin
  Value := TBigIntegerAccessor.Create(nil, True);
  try
    Value.ClearData;
    TBigInteger.CreatePBigIntegerBCD(Value.BigIntegerPtr, Bcd);

    if Value.DataLength > (Length(Result.val) div SizeOfLongWord) then
      raise Exception.Create(SNumericOverflow);

    Result.precision := Bcd.Precision;

    Result.scale := Bcd.SignSpecialPlaces and $3F;

    if IsBcdNegative(Bcd) then
      Result.Sign := 0
    else
      Result.Sign := 1;

    BytesCount := Result.precision div 2;
    if (Result.precision mod 2) <> 0 then
      Inc(BytesCount);
  {$IFDEF CLR}
    for i := 0 to BytesCount - 1 do
      Result.val[i] := Marshal.ReadByte(Value.DataPtr, i);
  {$ELSE}
    FillChar(@Result.val[0], Length(Result.val), $00);
    CopyBuffer(Value.DataPtr, @Result.val[0], BytesCount);
  {$ENDIF}
  finally
    Value.Free;
  end;
end;

{$IFNDEF VER6P}
const
  MaxFMTBcdFractionSize = 64;
  SInvalidBcdValue = '%s is not a valid BCD value';
  
type
{ Exception classes }

  EBcdException = class(Exception);
  EBcdOverflowException = class(EBcdException);
  
procedure OverflowError(const Message: string);
begin
  raise EBcdOverflowException.Create(Message);
end;

procedure BcdErrorFmt(const Message: string; BcdAsString: string);
begin
  raise EBcdException.Create(Format(Message, [BcdAsString]));
end;

function FractionToStr(const pIn: PChar; count: SmallInt;
         DecPosition: ShortInt; Negative: Boolean;
         StartWithDecimal: Boolean): string;
var
  NibblesIn, BytesIn, DigitsOut: Integer;
  P, POut: PChar;
  Dot: Char;

  procedure AddOneChar(Value: Char);
  begin
    P[0] := Value;
    Inc(P);
    Inc(DigitsOut);
  end;

  procedure AddDigit(Value: Char);
  begin
    if ((DecPosition > 0) and (NibblesIn  = DecPosition)) or
       ((NibblesIn = 0) and StartWithDecimal) then
    begin
      if DigitsOut = 0 then AddOneChar('0');
      AddOneChar(Dot);
    end;
    if (Value > #0) or (DigitsOut > 0) then
      AddOneChar(Char(Integer(Value)+48));
    Inc(NibblesIn);
  end;

begin
  POut := AllocMem(Count + 3);  // count + negative/decimal/zero
  try
    Dot := DecimalSeparator;
    P := POut;
    DigitsOut := 0;
    BytesIn := 0;
    NibblesIn := 0;
    while NibblesIn < Count do
    begin
      AddDigit(Char(Integer(pIn[BytesIn]) SHR 4));
      if NibblesIn < Count then
        AddDigit(Char(Integer(pIn[BytesIn]) AND 15));
      Inc(BytesIn);
    end;
    while (DecPosition > 0) and (NibblesIn  > DecPosition) and (DigitsOut > 1) do
    begin
      if POut[DigitsOut-1] = '0' then
      begin
        Dec(DigitsOut);
        POut[DigitsOut] := #0;
      end else
        break;
    end;
    if POut[DigitsOut-1] = Dot then
      Dec(DigitsOut);
    POut[DigitsOut] := #0;
    SetString(Result, POut, DigitsOut);
  finally
    FreeMem(POut, Count + 2);
  end;
  if Result = '' then Result := '0'
  else if Negative then Result := '-' + Result;
end;

function BcdToStr(const Bcd: TBcd): string;
var
  NumDigits: Integer;
  pStart: PChar;
  DecPos: SmallInt;
  Negative: Boolean;
begin
  if (Bcd.Precision = 0) or (Bcd.Precision > MaxFMTBcdFractionSize) then
    OverFlowError(SBcdOverFlow)
  else
  begin
    Negative := Bcd.SignSpecialPlaces and (1 shl 7) <> 0;
    NumDigits := Bcd.Precision;
    pStart := pCHAR(@Bcd.Fraction);   // move to fractions
    // use lower 6 bits of iSignSpecialPlaces.
    if (Bcd.SignSpecialPlaces and 63) > 0 then
    begin
      DecPos := ShortInt(NumDigits - (Bcd.SignSpecialPlaces and 63));
    end else
      DecPos := NumDigits + 1;     // out of range
    Result := FractionToStr(pStart, NumDigits, DecPos, Negative,
           (NumDigits = Bcd.SignSpecialPlaces and 63));
    if Result[1] in ['0', '-'] then
      if (Result = '-0') or (Result = '0.0') or (Result = '-0.0') then Result := '0';
  end;
end;

function BcdToDouble(const Bcd: TBcd): Double;
begin
  Result := StrToFloat(BcdToStr(Bcd));
end;

function InvalidBcdString(PValue: PChar): Boolean; 
var
  Dot: Char;
  P: PChar;
begin
  Dot := DecimalSeparator;
  P := PValue;
  Result := False;
  while P^ <> #0 do
  begin
    if not (P^ in ['0'..'9', '-', Dot]) then
    begin
      Result := True;
      break;
    end;
    Inc(P);
  end;
end;

procedure StrToFraction(pTo: PChar; pFrom: PChar; count: SmallInt); pascal;
var
  Dot: Char;        
begin
  Dot := DecimalSeparator;
  asm
   // From bytes to nibbles, both left aligned
        PUSH    ESI
        PUSH    EDI
        PUSH    EBX
        MOV     ESI,pFrom  // move pFrom to ESI
        MOV     EDI,pTo    // move pTo to EDI
        XOR     ECX,ECX    // set ECX to 0
        MOV     CX,count   // store count in CX
        MOV     DL,0       // Flag: when to store
        CLD
@@1:    LODSB              // moves [ESI] into al
        CMP     AL,Dot
        JE      @@4
        SUB     AL,'0'
        CMP     DL,0
        JNE     @@2
        SHL     AL,4
        MOV     AH,AL
        JMP     @@3
@@2:    OR      AL,AH     // takes AH and ors in AL
        STOSB             // always moves AL into [EDI]
@@3:    NOT     dl        // flip all bits
@@4:    LOOP    @@1       // decrements cx and checks if it's 0
        CMP     DL,0      // are any bytes left unstored?
        JE      @@5
        MOV     AL,AH     // if so, move to al
        STOSB             // and store to [EDI]
@@5:    POP     EBX
        POP     EDI
        POP     ESI
  end;
end;

function TryStrToBcd(const AValue: string; var Bcd: TBcd): Boolean;
const
  spaceChars: set of Char = [ ' ', #6, #10, #13, #14];     
  digits: set of Char = ['0'..'9'];
var
  Neg: Boolean;
  NumDigits, DecimalPos: Word;
  pTmp, pSource: PChar;
  Dot : Char;
begin
  Dot := DecimalSeparator;
  if InvalidBcdString(PChar(AValue)) then
  begin
    Result := False;
    Exit;
  end;
  if (AValue = '0') or (AValue = '') then
  begin
    Result := True;
    Bcd.Precision := 8;
    Bcd.SignSpecialPlaces := 2;
    pSource := PChar(@Bcd.Fraction);
    System.FillChar(PSource^, SizeOf(Bcd.Fraction), 0);
    Exit;
  end;
  Result := True;
  Neg := False;
  DecimalPos := Pos(Dot, AValue);

  pSource := pCHAR(AValue);
  { Strip leading whitespace }
  while (pSource^ in spaceChars) or (pSource^ = '0') do
  begin
    Inc(pSource);
    if DecimalPos > 0 then Dec(DecimalPos);
  end;

  { Strip trailing whitespace }
  pTmp := @pSource[ StrLen( pSource ) -1 ];
  while pTmp^ in spaceChars do
  begin
    pTmp^ := #0;
    Dec(pTmp);
  end;

  { Is the number negative? }
  if pSource^ = '-' then
  begin
    Neg := TRUE;
    if DecimalPos > 0 then Dec(DecimalPos);
  end;
  if (pSource^ = '-') or (pSource^ ='+') then
    Inc(pSource);

  { Clear structure }
  pTmp := pCHAR(@Bcd.Fraction);
  System.FillChar(pTmp^, SizeOf(Bcd.Fraction), 0);
  if (pSource[0] = '0') then
  begin
    Inc(PSource);  // '0.' scenario
    if DecimalPos > 0 then Dec(DecimalPos);
  end;
  NumDigits := StrLen(pSource);
  if (NumDigits > MaxFMTBcdFractionSize) then
  begin
    if (DecimalPos > 0) and (DecimalPos <= MaxFMTBcdFractionSize) then
      NumDigits := MaxFMTBcdFractionSize // truncate to 64
    else begin
      Bcd.Precision := NumDigits;
      Exit;
    end;
  end;

  if NumDigits > 0 then
    StrToFraction(pTmp, pSource, SmallInt(NumDigits))
  else begin
    Bcd.Precision := 10;
    Bcd.SignSpecialPlaces := 2;
  end;

  if DecimalPos > 0 then
  begin
    Bcd.Precision := Byte(NumDigits-1);
    if Neg then
      Bcd.SignSpecialPlaces := ( 1 shl 7 ) + (BYTE(NumDigits - DecimalPos))
    else
      Bcd.SignSpecialPlaces := ( 0 shl 7 ) + (BYTE(NumDigits - DecimalPos));
  end else
  begin
    Bcd.Precision := Byte(NumDigits);
    if Neg then
      Bcd.SignSpecialPlaces := (1 shl 7)
    else
      Bcd.SignSpecialPlaces := (0 shl 7);
  end;
end;

function StrToBcd(const AValue: string): TBcd;
var
  Success: Boolean;
begin
  Success := TryStrToBcd(AValue, Result);
  if not Success then
    BcdErrorFmt(SInvalidBcdValue, AValue);
end;

procedure DoubleToBcd(const AValue: Double; var bcd: TBcd); overload;
begin
  bcd := StrToBcd(FloatToStr(AValue));
end;

function DoubleToBcd(const AValue: Double): TBcd; overload;
begin
  DoubleToBcd(AValue, Result);
end;

{$ENDIF}

function DBNumericToDouble(Value: TDBNumeric): double;
var
  Bcd: TBcd;
begin
  Bcd := DBNumericToBcd(Value);
  Result := BcdToDouble(Bcd);
end;

function DoubleToDBNumeric(Value: double; Precision, Scale: integer): TDBNumeric;
var
  Bcd: TBcd;
  Str: string;
begin
  //Str := FloatToStrF(Value, ffFixed, Precision, Scale);
  if (Precision = 0) and (Scale = 0) then
    Str := FloatToStr(Value) //Format('%f', [Value])
  else
    Str := Format('%.' + IntToStr(Scale) + 'f', [Value]);
  Bcd := StrToBcd(Str);
  Result := BcdToDBNumeric(Bcd);
end;

function IsLargeDataTypeUsed(const FieldDesc: TFieldDesc): boolean;
begin
  if FieldDesc.SubDataType = dtMSUDT then
    Result := FieldDesc.Length = 0
  else
    Result := FieldDesc.DataType = dtBlob;

  if not Result then
    Result := ((FieldDesc.DataType = dtMemo) or (FieldDesc.DataType = dtWideMemo)) and (((not dtWide) and FieldDesc.SubDataType) = dtText);
  if not Result then
  {$IFDEF LITE}
    Result := ((not dtWide) and FieldDesc.SubDataType) = dtMSXML;
  {$ELSE}
    Result := FieldDesc.DataType = dtMSXML;
  {$ENDIF}
end;

function IsLargeDataTypeUsed(const ParamDesc: TParamDesc): boolean;
begin
  Result :=
    (ParamDesc.GetDataType = dtBlob) or
    (ParamDesc.GetDataType = dtMemo) or
    (ParamDesc.GetDataType = dtWideMemo);
end;

function IsOutputLOB(ParamDesc: TParamDesc; ServerVersion, ClientVersion: integer): boolean;
begin
  Result := ((ParamDesc.GetDataType = dtMemo) or (ParamDesc.GetDataType = dtWideMemo)) and
    (ParamDesc.GetParamType in [pdOutput, pdInputOutput]) and
    (ServerVersion >= 9) and
    (ClientVersion >= 9);
end;

function GetProviderName(const Provider: TOLEDBProvider; const CompactVersion: TCompactVersion = cvAuto;
  const NativeClientVersion: TNativeClientVersion = ncAuto): _string;
begin
  case Provider of
    prAuto, prSQL:
      Result := SProviderSQLOLEDB;
    prNativeClient:
      case NativeClientVersion of
        ncAuto:
          Result := SProviderNativeClient;
        nc2008:
          Result := SProviderNativeClient10;
      else
        Assert(False);
        Result := '';
      end;
    prCompact:
      case CompactVersion of
        cv30:
          Result := SProviderCompact;
        cv35:
          Result := SProviderCompact35;
        cvAuto, cv40:
          Result := SProviderCompact40;
      else
        Assert(False);
        Result := '';
      end;
  else
    Assert(False);
    Result := '';
  end;
end;

function GetProvider(const ProviderName: _string): TOLEDBProvider;
var
  Name: _string;
begin
  Name := _UpperCase(ProviderName);
  if Name = SProviderSQLOLEDB then
    Result := prSQL
  else
    if (Name = SProviderNativeClient) or (Name = SProviderNativeClient10) then
      Result := prNativeClient
    else
      if (Name = SProviderCompact) or (Name = SProviderCompact35) or (Name = SProviderCompact40) then
        Result := prCompact
      else
        Result := prAuto;
end;

function GetCompactVersion(const ProviderName: _string): TCompactVersion;
var
  Name: _string;
begin
  Name := _UpperCase(ProviderName);
  if Name = SProviderCompact then
    Result := cv30
  else
    if Name = SProviderCompact35 then
      Result := cv35
    else
      if Name = SProviderCompact40 then
        Result := cv40
      else
        Result := cvAuto;
end;

function GetNativeClientVersion(const ProviderName: _string): TNativeClientVersion;
var
  Name: _string;
begin
  Name := _UpperCase(ProviderName);
  if Name = SProviderNativeClient10 then
    Result := nc2008
  else
    Result := ncAuto;
end;

// FetchBlock support
function IsNeedFetchBlock(const FieldDesc: TFieldDesc; ServerVersion: integer): boolean; // Return True if field need to fetch into separate buffer
var
  DataType: word;
begin
  Assert(FieldDesc.FieldDescKind = fdkData, 'IsNeedFetchBlock ' + FieldDesc.Name);
  DataType := FieldDesc.DataType;
  case DataType of
    dtExtString, dtExtWideString, dtExtVarBytes, dtVariant:
      Result := True;
    dtBlob, dtMemo, dtWideMemo, dtMSXML:
      Result := not IsLargeDataTypeUsed(FieldDesc);
    dtFloat, dtBcd:
      Result := {IsCompactEdition(ServerVersion) and} (TOLEDBFieldDesc(FieldDesc).SubDataType = dtNumeric);
    else
      Result := False;
  end;
end;

procedure IncFetchBlockOffset(var FetchBlockOffset: integer; Field: TFieldDesc);
begin
  case Field.DataType of
    dtExtString, dtExtWideString:
      Inc(FetchBlockOffset, OLE_DB_INDICATOR_SIZE + MaxNonBlobFieldLen + 2 {#0#0 terminator} + sizeof(DBLENGTH) {Length});
    dtMemo, dtWideMemo, dtMSXML:
      Inc(FetchBlockOffset, OLE_DB_INDICATOR_SIZE + MaxNonBlobFieldLen + 2 {#0#0 terminator});
    dtBlob:
      Inc(FetchBlockOffset, OLE_DB_INDICATOR_SIZE + Field.Length + sizeof(DBLENGTH) {Length});
    dtExtVarBytes:
      Inc(FetchBlockOffset, OLE_DB_INDICATOR_SIZE + MaxNonBlobFieldLen + sizeof(DBLENGTH) {Length});
    dtVariant:
      Inc(FetchBlockOffset, OLE_DB_INDICATOR_SIZE + SizeOf_OleVariant);
    dtFloat, dtBcd:
      Inc(FetchBlockOffset, OLE_DB_INDICATOR_SIZE + SizeOfTDBNumeric);
    else
      Assert(False);
  end;
end;

function GenerateTableName(const CatalogName: _string;
  const SchemaName: _string;
  const TableName: _string;
  const DefaultCatalogName: _string): _string;
begin
  if (CatalogName <> '') and not _SameText(CatalogName, DefaultCatalogName) then
    Result := _Format('%s.%s.%s',
      [OLEDBSQLInfo.QuoteIfNeed(CatalogName),
       OLEDBSQLInfo.QuoteIfNeed(SchemaName),
       OLEDBSQLInfo.QuoteIfNeed(TableName)])
  else
  if SchemaName <> '' then
    Result := _Format('%s.%s',
      [OLEDBSQLInfo.QuoteIfNeed(SchemaName),
       OLEDBSQLInfo.QuoteIfNeed(TableName)])
  else
    Result := _Format('%s',
      [OLEDBSQLInfo.QuoteIfNeed(TableName)]);
end;

function ChangeDecimalSeparator(const Value: Variant): _string;
var
  i: integer;
begin
{$IFDEF VER6P}
  Result := Value;
{$ELSE}
  if TVarData(Value).VType = varSingle then
    Result := FloatToStr(TVarData(Value).VSingle)
  else
  if TVarData(Value).VType = varDouble then
    Result := FloatToStr(TVarData(Value).VDouble)
  else
    Result := Value;
{$ENDIF}
  if DecimalSeparator <> '.' then begin
    i := 2;
    while i < Length(Result) do begin
      if Result[i] = _string(DecimalSeparator) then begin
        Result[i] := '.';
        Break;
      end;
      Inc(i);
    end;
  end;
end;

function GetParamNameWODog(const ParamName: _string): _string;
begin
  if (ParamName <> '') and (ParamName[1] = '@') then
    Result := Copy(ParamName, 2, 1000)
  else
    Result := ParamName;
end;

{$IFDEF CLR}
procedure QueryIntf(Source: IUnknown; const IID: TInterfaceRef; out Intf); // OLE QueryInterface analog
begin
  Assert(Source <> nil);
  Intf := nil;
  if Source is IID then
    Intf := Source as IID
  else
    raise EOLEDBError.Create(E_NOINTERFACE, 'QueryInterface failed');
end;

{$ELSE}

procedure QueryIntf(Source: IUnknown; const IID: TGuid; out Intf); // OLE QueryInterface analog
begin
  Assert(Source <> nil);
  if Source.QueryInterface(IID, Intf) <> S_OK then
    raise EOLEDBError.Create(E_NOINTERFACE, 'QueryInterface failed');
end;
{$ENDIF}

function Min(const A, B: Integer): Integer;
begin
  if A < B then
    Result := A
  else
    Result := B;
end;

{ TOLEDBParamDesc }

function TOLEDBParamDesc.GetValue: variant;
begin
  if not GetNull
    and not VarIsEmpty(FData)
    and not VarIsNull(FData)
  {$IFDEF CLR}
    and (FData is TSharedObject)
  {$ELSE}
    and (TVarData(FData).VType = varByRef{$IFDEF FPC} or varVariant{$ENDIF})
  {$ENDIF}
  then begin
    if GetAsBlobRef.IsUnicode then
      Result := GetAsBlobRef.AsWideString
    else
      Result := GetAsBlobRef.AsString;
  end
  else
    Result := inherited GetValue;
end;

procedure TOLEDBParamDesc.SetValue(const Value: variant);
var
  ConvertedValue: Variant;
{$IFDEF CLR}
  Value1, Value2: TBytes;
{$ELSE}
  Value1: Variant;
{$ENDIF}
  i, l: integer;
begin
  if (DataType = dtDate) and not (VarIsEmpty(Value) or VarIsNull(Value)) then
    inherited SetValue(Int(Value)) // drop time info
  else
  if (DataType in [dtBytes, dtVarBytes{, dtBlob}]) and
    (VarType(Value) <> varArray + varByte) and
    not (VarIsEmpty(Value) or VarIsNull(Value))
  then begin
    case VarType(Value) of
      varShortInt {1 byte},
      varSmallint {2 bytes},
      varInteger {4 bytes},
      varInt64 {8 bytes},
      varByte {1 byte},
      varWord {2 bytes},
      varLongWord {4 bytes}: begin
        case VarType(Value) of
          varShortInt, varByte:
            l := 1;
          varSmallint, varWord:
            l := 2;
          varInteger, varLongWord:
            l := 4;
          varInt64:
            l := 8;
          else
          begin
            Assert(False);
            l := - 1;
          end;
        end;

        // reverse bytes order
      {$IFDEF CLR}
        Value1 := BitConverter.GetBytes(Int64(Value));
        //Assert(l = Length(Value1));
        SetLength(Value2, l);
        for i := 0 to l - 1 do
          Value2[i] := Value1[l - 1 - i];
        ConvertedValue := Value2;
      {$ELSE}
        Value1 := VarArrayCreate([0, l - 1], varByte);

        for i := 0 to l - 1 do
          PAChar(TVarData(Value1).VArray.Data)[i] := PAnsiChar(@TVarData(Value).VInteger)[l - 1 - i];
        ConvertedValue := Value1;
      {$ENDIF}
      end;
    else
      if VarIsStr(Value) then
        ConvertedValue := Encoding.Default.GetBytes(AnsiString(Value))
      else
        ConvertedValue := Value;
    end;
    inherited SetValue(ConvertedValue)
  end
  else
  if (((ParamType = pdInput) and (DataType in CharsByRef + BytesByRef) {$IFDEF CLR}and (DataType <> dtString){$ENDIF})
    or (DataType in [dtBlob, dtMemo, dtWideMemo])) and
    not (VarIsEmpty(Value) or VarIsNull(Value)) and
    (VarType(Value) <> varArray + varByte) and
    {$IFDEF CLR}not (Value is TBlob){$ELSE}(VarType(Value) <> varByRef{$IFDEF FPC} or varVariant{$ENDIF}){$ENDIF} and
    not VarIsStr(Value)
  then begin
    inherited SetValue(VarToStr(Value))
  end
  else
    inherited;
end;

function TOLEDBParamDesc.GetAsBlobRef: TBlob;
begin
  Result := TBlob({$IFDEF CLR}FData{$ELSE}TVarData(FData).VPointer{$ENDIF});
end;

{ TExecuteThread }

procedure TExecuteThread.InternalExecute;
begin
{$IFDEF CLR}
  OleCheck(CoInitializeEx(nil, COINIT_MULTITHREADED));
  try
{$ENDIF}
    if Assigned(FRunMethod) then
      FRunMethod;
{$IFDEF CLR}
  finally
    CoUninitialize;
  end;
{$ENDIF}
end;

{ TOLEDBThreadWrapper }

procedure TOLEDBThreadWrapper.DoException(E: Exception);
begin
  if E is EOLEDBError then begin
    Assert(FException = nil);
    FException := EOLEDBError.Create(EOLEDBError(E).ErrorCode, EOLEDBError(E).Message);
    EOLEDBError(FException).Assign(EOLEDBError(E));
  end
  else
    inherited;
end;

{ TOLEDBStream }

constructor TOLEDBStream.Create(Blob: TBlob {to avoid data copy}; StreamList: TList);
begin
  inherited Create;

  FBlob := Blob;
  FStorageType := stBlob;
  FPosInBlob := 0;
  InitStream(StreamList);
end;

constructor TOLEDBStream.Create(Stream: TStream; StreamList: TList);
begin
  inherited Create;

  FStream := Stream;
  FStorageType := stStream;
  FOutputEncoding := oeUTF8;
  InitStream(StreamList);
end;

procedure TOLEDBStream.InitStream(StreamList: TList);
begin
{$IFDEF DEBUG}
  Inc(StreamCnt);
{$ENDIF}

  case FStorageType of
    stBlob:
      FSize := FBlob.Size;
    stStream:
      FSize := FStream.Size;
    else
      Assert(False);
  end;

  FStreamList := StreamList;

  if FStreamList <> nil then
    FStreamList.Add(Self);
end;

destructor TOLEDBStream.Destroy;
begin
  if FStreamList <> nil then
  {$IFDEF CLR}
    FStreamList.Remove(Self);
  {$ELSE}
    FStreamList.Remove(pointer(Self));
  {$ENDIF}

  inherited;
{$IFDEF DEBUG}
  Dec(StreamCnt);
{$ENDIF}
end;

{$IFNDEF FPC}
function TOLEDBStream.Read(pv: IntPtr; cb: Longint; pcbRead: PLongint): HResult;
{$ELSE}
function TOLEDBStream.Read(pv: Pointer; cb: DWord; pcbRead: PDWord): HRESULT;
{$ENDIF}
var
  cbSrcReadBytes: longint;
  cbDstWriteBytes: longint;
{$IFDEF CLR}
  Bytes: TBytes;
{$ENDIF}
begin
  try
    case FStorageType of
      stBlob: begin
        cbSrcReadBytes := Min(Integer(FSize - FPosInBlob), cb);
        cbDstWriteBytes := FBlob.Read(FPosInBlob, cbSrcReadBytes, pv);
        Inc(FPosInBlob, cbDstWriteBytes);
      end;
      stStream: begin
        cbSrcReadBytes := Min(Integer(Integer(FSize) - FStream.Position), cb);
      {$IFDEF CLR}
        SetLength(Bytes, cb);
        cbDstWriteBytes := FStream.Read(Bytes, cbSrcReadBytes);
        Marshal.Copy(Bytes, 0, pv, cbDstWriteBytes);
      {$ELSE}
        cbDstWriteBytes := FStream.Read(pv^, cbSrcReadBytes);
      {$ENDIF}
      end;
      else
        Assert(False);
        cbDstWriteBytes := 0; // To prevent compiler warning
    end;

    if pcbRead <> nil then
      Marshal.WriteInt32(pcbRead, cbDstWriteBytes);
    Result := S_OK;
  except
    Result := S_FALSE;
  end;
end;

{$IFNDEF FPC}
function TOLEDBStream.Write(pv: IntPtr; cb: Longint; pcbWritten: PLongint): HResult;
{$ELSE}
function TOLEDBStream.Write(pv: Pointer; cb: DWord; pcbWritten: PDWord): HRESULT;
{$ENDIF}
{$IFDEF CLR}
var
  Bytes: TBytes;
{$ENDIF}
begin
  try
    case FStorageType of
      stBlob: begin
        FBlob.Write(FPosInBlob, cb, pv);
        Inc(FPosInBlob, cb);
      end;
      stStream: begin
        {$IFDEF CLR}
          SetLength(Bytes, cb);
          Marshal.Copy(pv, Bytes, 0, cb);
          FStream.Write(Bytes, cb);
        {$ELSE}
          FStream.Write(pv^, cb);
        {$ENDIF}
      end;
      else
        Assert(False);
    end;
    Inc(FSize, cb);

    if pcbWritten <> nil then
      Marshal.WriteInt32(pcbWritten, cb);
    Result := S_OK;
  except
    Result := S_FALSE;
  end;
end;

{ TBigIntegerAccessor }

constructor TBigIntegerAccessor.Create(BigIntegerPtr: PBigInteger; Allocate: boolean);
begin
  inherited Create;

  if not Allocate then
    FBigIntegerPtr := BigIntegerPtr
  else begin
    FBigIntegerPtr := Marshal.AllocHGlobal(SizeOfRBigInteger);
    FSelfAllocated := True;
  end;
end;

destructor TBigIntegerAccessor.Destroy;
begin
  if FSelfAllocated then
    Marshal.FreeHGlobal(FBigIntegerPtr);

  inherited;
end;

procedure TBigIntegerAccessor.SetBigIntegerPtr(Value: PBigInteger);
begin
  if FSelfAllocated then begin
    Marshal.FreeHGlobal(FBigIntegerPtr);
    FSelfAllocated := False;
  end;
  FBigIntegerPtr := Value;
end;

function TBigIntegerAccessor.GetDataLength: integer;
begin
  Result := Marshal.ReadInt32(FBigIntegerPtr, MaxLength * SizeOfLongWord)
end;

procedure TBigIntegerAccessor.SetDataLength(Value: integer);
begin
  Marshal.WriteInt32(FBigIntegerPtr, MaxLength * SizeOfLongWord, Value);
end;

function TBigIntegerAccessor.GetDataPtr: IntPtr;
begin
  Result := FBigIntegerPtr;
end;

function TBigIntegerAccessor.GetData(Index: integer): LongWord;
begin
  Result := LongWord(Marshal.ReadInt32(FBigIntegerPtr, Index * SizeOfLongWord));
end;

procedure TBigIntegerAccessor.SetData(Index: integer; Value: LongWord);
begin
  Marshal.WriteInt32(FBigIntegerPtr, Index * SizeOfLongWord, Integer(Value));
end;

procedure TBigIntegerAccessor.ClearData;
var
  i: integer;
begin
  for i := 0 to MaxLength - 1 do
    Data[i] := 0;
end;

function TBigIntegerAccessor.GetBigInteger: RBigInteger;
{$IFDEF CLR}
var
  i: integer;
{$ENDIF}  
begin
{$IFDEF CLR}
  Result.DataLength := GetDataLength;
  for i := 0 to MaxLength - 1 do
    Result.Data[i] := Data[i];
  //Result := RBigInteger(Marshal.PtrToStructure(FBigIntegerPtr, TypeOf(RBigInteger)));
{$ELSE}
  Result := RBigInteger(FBigIntegerPtr^);
{$ENDIF}
end;

procedure TBigIntegerAccessor.SetBigInteger(const Value: RBigInteger);
{$IFDEF CLR}
var
  i: integer;
{$ENDIF}
begin
  Assert(FBigIntegerPtr <> nil);
{$IFDEF CLR}
  DataLength := Value.DataLength;
  for i := 0 to MaxLength - 1 do
    Data[i] := Value.Data[i];
  //Marshal.StructureToPtr(TObject(Value), FBigIntegerPtr, False);
{$ELSE}
  RBigInteger(FBigIntegerPtr^) := Value;
{$ENDIF}  
end;

{ TBigInteger }

class procedure TBigInteger.CreatePBigIntegerPtr(PBigInt: PBigInteger);
var
  BigIntAcc: TBigIntegerAccessor;
begin
  BigIntAcc := TBigIntegerAccessor.Create(PBigInt, False);
  try
    BigIntAcc.DataLength := 1;
    BigIntAcc.ClearData;
  finally
    BigIntAcc.Free;
  end;
end;

class procedure TBigInteger.CreateRBigIntegerStruct(var BigInt: RBigInteger);
{$IFDEF CLR}
var
  i: integer;
{$ENDIF}  
begin
  BigInt.DataLength := 1;
{$IFDEF CLR}
  for i := 0 to MaxLength - 1 do
    BigInt.Data[i] := 0;
{$ELSE}
  MemUtils.FillChar(@BigInt.Data[0], MaxLength * SizeOfLongWord, $00);
{$ENDIF}    
end;

class procedure TBigInteger.CreatePBigInteger64(PBigInt: PBigInteger; Value: Int64);
var
  BigIntAcc: TBigIntegerAccessor;
begin
  BigIntAcc := TBigIntegerAccessor.Create(PBigInt, False);
  try
    BigIntAcc.ClearData;
    BigIntAcc.DataLength := 0;

    while (Value <> 0) and (BigIntAcc.DataLength < MaxLength) do begin
      BigIntAcc.Data[BigIntAcc.DataLength] := LongWord(Value and $FFFFFFFF);
      Value := Value shr 32;
      BigIntAcc.DataLength := BigIntAcc.DataLength + 1;
    end;

    if (Value <> 0) or (LongWord(BigIntAcc.Data[MaxLength - 1] and $80000000) <> 0) then
      raise Exception.Create('Positive overflow in constructor.');

    if BigIntAcc.DataLength = 0 then
      BigIntAcc.DataLength := 1;
  finally
    BigIntAcc.Free;
  end;
end;

class procedure TBigInteger.CreatePBigIntegerBCD(PBigInt: PBigInteger; const Value: TBcd);
var
  Multiplier: TBigIntegerAccessor;
  Result, Tmp: TBigIntegerAccessor;
  BigIntAcc: TBigIntegerAccessor;
  Radix: integer;
  posVal: Integer;
  Val: TBytes;
  i: Integer;
  k: integer;
begin
  Multiplier := nil;
  Result := nil;
  Tmp := nil;
  BigIntAcc := nil;
  try
    Multiplier := TBigIntegerAccessor.Create(nil, True);
    Result := TBigIntegerAccessor.Create(nil, True);
    Tmp := TBigIntegerAccessor.Create(nil, True);
    BigIntAcc := TBigIntegerAccessor.Create(PBigInt, False);

    TBigInteger.CreatePBigInteger64(Multiplier.BigIntegerPtr, 1);
    TBigInteger.CreatePBigIntegerPtr(Result.BigIntegerPtr);

    SetLength(Val, SizeOfFraction);
  {$IFDEF CLR}
    for i := 0 to SizeOfFraction - 1 do
      Val[i] := Value.Fraction[i];
  {$ELSE}
    MemUtils.CopyBuffer(@Value.Fraction[0], @Val[0], Length(Val));
  {$ENDIF}

    k := (Value.Precision div 2);
    if (Value.Precision mod 2) = 0 then
      Dec(k);

    for i := k downto 0 do begin
      Radix := 100;
      if (i = k) then begin
        if (Value.Precision mod 2) = 0 then
          posVal := (val[i] and $0F) + ((val[i] and $F0) shr 4) * 10
        else begin
          posVal := (val[i] and $F0) shr 4;
          Radix := 10;
        end;
      end
      else
        posVal := (val[i] and $0F) + ((val[i] and $F0) shr 4) * 10;

      Assert(posVal < Radix);

      Tmp.BigInteger := Mul64(Multiplier.BigIntegerPtr, posVal);
      Result.BigInteger := Add(Result.BigIntegerPtr, Tmp.BigIntegerPtr);

      if (i - 1) >= 0 then
        Multiplier.BigInteger := Mul64(Multiplier.BigIntegerPtr, Radix);
    end;

    BigIntAcc.ClearData;
    for i := 0 to Result.DataLength - 1 do
      BigIntAcc.Data[i] := Result.Data[i];

    BigIntAcc.DataLength := Result.DataLength;
  finally
    Multiplier.Free;
    Result.Free;
    Tmp.Free;
    BigIntAcc.Free;
  end;
end;

class function TBigInteger.Add(PBigInt1: PBigInteger; PBigInt2: PBigInteger): RBigInteger;
var
  carry, sum: Int64;
  i, lastPos: Integer;
  BigInt1Acc, BigInt2Acc: TBigIntegerAccessor;
begin
  BigInt1Acc := TBigIntegerAccessor.Create(PBigInt1, False);
  BigInt2Acc := TBigIntegerAccessor.Create(PBigInt2, False);
  try
    TBigInteger.CreateRBigIntegerStruct(Result);

    if BigInt1Acc.DataLength > BigInt2Acc.DataLength then
      Result.DataLength := BigInt1Acc.DataLength
    else
      Result.dataLength := BigInt2Acc.DataLength;
    carry := 0;
    for i := 0 to Result.DataLength - 1 do begin
      sum := Int64(BigInt1Acc.data[i]) + Int64(BigInt2Acc.data[i]) + carry;
      carry := sum shr 32;
      Result.Data[i] := LongWord(sum and $FFFFFFFF);
    end;

    if (carry <> 0) and (Result.DataLength < MaxLength) then begin
      Result.Data[Result.DataLength] := LongWord(carry);
      Result.DataLength := Result.DataLength + 1;
    end;

    while (Result.DataLength > 1) and (Result.Data[Result.DataLength - 1] = 0) do
      Result.DataLength := Result.DataLength - 1;

    // overflow check
    lastPos := maxLength - 1;
    if (longword(BigInt1Acc.Data[lastPos] and $80000000) = longword(BigInt2Acc.Data[lastPos] and $80000000)) and
       (longword(Result.Data[lastPos] and $80000000) <> longword(BigInt1Acc.Data[lastPos] and $80000000)) then
    begin
      raise Exception.Create('overflow!');
    end;
  finally
    BigInt1Acc.Free;
    BigInt2Acc.Free;
  end;
end;

class function TBigInteger.NEGATEPtr(PBigInt: PBigInteger): RBigInteger;
var
  carry, val: Int64;
  i, index: Integer;
  BigIntAcc: TBigIntegerAccessor;
begin
  BigIntAcc := TBigIntegerAccessor.Create(PBigInt, False);
  try
    // handle neg of zero separately since it'll cause an overflow
    // if we proceed.
    if (BigIntAcc.DataLength = 1) and (BigIntAcc.Data[0] = 0) then begin
      TBigInteger.CreateRBigIntegerStruct(Result);
      Exit;
    end;

    // 1's complement
    for i := 0 to MaxLength - 1 do
      Result.Data[i] := LongWord(not(BigIntAcc.Data[i]));

    // add one to result of 1's complement
    carry := 1;
    index := 0;

    while (carry <> 0) and (index < maxLength) do begin
      val := Int64(Result.Data[index]);
      Inc(val);

      Result.Data[index] := LongWord(val and $FFFFFFFF);
      carry := val shr 32;
      Inc(index);
    end;

    if LongWord(BigIntAcc.Data[maxLength-1] and $80000000) = LongWord(Result.Data[MaxLength-1] and $80000000) then
      raise Exception.Create('Overflow in negation.');

    Result.DataLength := MaxLength;

    while (Result.DataLength > 1) and (Result.Data[Result.DataLength - 1] = 0) do
      Result.DataLength := Result.DataLength - 1;
  finally
    BigIntAcc.Free;
  end;
end;

class function TBigInteger.NEGATE(const BigInt: RBigInteger): RBigInteger;
{$IFDEF CLR}
var
  BigIntAcc: TBigIntegerAccessor;
{$ENDIF}  
begin
{$IFDEF CLR}
  BigIntAcc := TBigIntegerAccessor.Create(nil, True);
  try
    BigIntAcc.BigInteger := BigInt; 
    Result := NEGATEPtr(BigIntAcc.BigIntegerPtr);
  finally
    BigIntAcc.Free;
  end;
{$ELSE}
  Result := NEGATEPtr(@BigInt);
{$ENDIF}
end;

class function TBigInteger.Mul(PBigInt1: PBigInteger; PBigInt2: PBigInteger): RBigInteger;

{$IFNDEF CLR}
  function MulUInt(Val1, Val2: LongWord): Int64;
  asm
  {$IFDEF CPUX64}
    MOV EAX, Val1
    MUL Val2
    SHL RDX, 32
    ADD RAX, RDX
  {$ELSE}
    MUL    Val2
  {$ENDIF}
  end;
{$ENDIF}

var
  BigInt1Acc, BigInt2Acc: TBigIntegerAccessor;
  nbi1, nbi2: RBigInteger;
  mcarry, val: Int64;
  bi1Neg, bi2Neg, isMaxNeg: Boolean;
  b1, b2: Longword;
  i, j, k, lastPos: Integer;
begin
  BigInt1Acc := TBigIntegerAccessor.Create(PBigInt1, False);
  BigInt2Acc := TBigIntegerAccessor.Create(PBigInt2, False);
  try
    lastPos := maxLength - 1;
    bi1Neg := false;
    bi2Neg := false;

    // take the absolute value of the inputs
    try
      if longword(BigInt1Acc.Data[lastPos] and $80000000) <> 0 then begin
        bi1Neg := true;
        nbi1 := NEGATEPtr(PBigInt1);
      end
      else
        //nbi1 := bi1^;
        nbi1 := BigInt1Acc.BigInteger;


      if longword(BigInt2Acc.Data[lastPos] and $80000000) <> 0 then begin
        bi2Neg := true;
        nbi2 := NEGATEPtr(PBigInt2);
      end
      else
        //nbi2 := bi2^;
        nbi2 := BigInt2Acc.BigInteger;
    except
    end;

    TBigInteger.CreateRBigIntegerStruct(Result);

    // multiply the absolute values
    for i := 0 to nbi1.dataLength - 1 do begin
      if nbi1.data[i] = 0 then
        continue;

      mcarry := 0;
      k := i;
      for j := 0 to nbi2.dataLength - 1 do begin
        // k = i + j
  //      val := Int64(nbi1.data[i]) * Int64(nbi2.data[j]);
        b1 := nbi1.data[i];
        b2 := nbi2.data[j];
      {$IFDEF CLR}
        val := Int64(b1) * Int64(b2);
      {$ELSE}
        val := MulUInt(b1, b2);
      {$ENDIF}
        val := val + Int64(result.data[k]) + mcarry;

        result.data[k] := longword(val and $FFFFFFFF);
        mcarry := val shr 32;
        Inc(k);
      end;

      if mcarry <> 0 then
        result.data[i + nbi2.dataLength] := longword(mcarry);
    end;

    result.dataLength := nbi1.dataLength + nbi2.dataLength;
    if result.dataLength > maxLength then
      result.dataLength := maxLength;

    while (result.dataLength > 1) and (result.data[result.dataLength - 1] = 0) do
      result.dataLength := result.dataLength - 1;

    // overflow check (result is -ve)
    if longword(result.data[lastPos] and $80000000) <> 0 then begin
      if (bi1Neg <> bi2Neg) and (result.data[lastPos] = $80000000) then begin
        // handle the special case where multiplication produces
        // a max negative number in 2's complement.

        if result.dataLength = 1 then
          Exit
        else begin
          isMaxNeg := true;
          i := 0;
          while (i < result.dataLength - 1) and isMaxNeg do begin
            if result.data[i] <> 0 then
              isMaxNeg := false;
            Inc(i);
          end;

          if isMaxNeg then
            Exit;
        end;
      end;

      raise Exception.Create('Multiplication overflow.');
    end;

    // if input has different signs, then result is -ve
    if bi1Neg <> bi2Neg then
      Result := NEGATE(Result);
  finally
    BigInt1Acc.Free;
    BigInt2Acc.Free;
  end;
end;

class function TBigInteger.Mul64(PBigInt1: PBigInteger; value: Int64): RBigInteger;
var
  BigIntAcc2: TBigIntegerAccessor;
begin
  BigIntAcc2 := TBigIntegerAccessor.Create(nil, True);
  try
    TBigInteger.CreatePBigInteger64(BigIntAcc2.BigIntegerPtr, Value);
    Result := Mul(PBigInt1, BigIntAcc2.BigIntegerPtr);
  finally
    BigIntAcc2.Free;
  end;
end;

{ TOLEDBConnection }

constructor TOLEDBConnection.Create;
begin
  inherited;

  FCommand := nil;

  FAuthentication := auServer;
  FProvider := prSQL;
  FCompactVersion := cvAuto;
  FNativeClientVersion := ncAuto;

  FDatabase := '';
  FQuotedIdentifier := True;
  FPacketSize := DefaultPacketSize;
  FAutoTranslate := True;
  FConnectionTimeout := DefaultConnectionTimeout;
  FOldPassword := '';

  FFailoverPartner := '';
  // Compact Edition specific
  FMaxDatabaseSize := DefaultMaxDatabaseSize;
  FMaxBufferSize := DefaultMaxBufferSize;
  FTempFileDirectory := '';
  FTempFileMaxSize := DefaultTempFileMaxSize;
  FDefaultLockEscalation := DefaultDefaultLockEscalation;
  FDefaultLockTimeout := DefaultDefaultLockTimeout;
  FAutoShrinkThreshold := DefaultAutoShrinkThreshold;
  FFlushInterval := DefaultFlushInterval;
  FTransactionCommitMode := cmAsynchCommit;
  FLockTimeout := DefaultDefaultLockTimeout;
  FLockEscalation := DefaultDefaultLockEscalation;
  FInitMode := imReadWrite;
  FLocaleIdentifier := GetSystemDefaultLCID;
end;

destructor TOLEDBConnection.Destroy;
begin
  Disconnect;

  FCommand.Free;

  inherited;
end;

function TOLEDBConnection.GetCommandClass: TCRCommandClass;
begin
  Result := TOLEDBCommand;
end;

function TOLEDBConnection.GetTransactionClass: TCRTransactionClass;
begin
  Result := TOLEDBTransaction;
end;

procedure TOLEDBConnection.ReleaseInterfaces;
begin
{$IFDEF CLR}
  if FISessionProperties <> nil then
    Marshal.ReleaseComObject(FISessionProperties);
  if FIDBInitialize <> nil then
    Marshal.ReleaseComObject(FIDBInitialize);
  if FIDBProperties <> nil then
    Marshal.ReleaseComObject(FIDBProperties);
  if FIDBCreateSession <> nil then
    Marshal.ReleaseComObject(FIDBCreateSession);
{$ENDIF}

  TOLEDBTransaction(GetInternalTransaction).ReleaseInterfaces;

  FISessionProperties := nil;
{$IFNDEF CLR}
  if FIDBInitialize <> nil then
    FIDBInitialize.Uninitialize;// check not need
{$ENDIF}
  FIDBInitialize := nil;
  FIDBProperties := nil;
  FIDBCreateSession := nil;
end;

procedure TOLEDBConnection.Check(const Status: HRESULT; Component: TObject;
  AnalyzeMethod: TAnalyzeMethod = nil; const Arg: IntPtr = nil);
begin
  if (Status <> S_OK) and (Status <> DB_S_ERRORSOCCURRED) and ((FProvider <> prCompact) or (Status <> DB_S_MULTIPLECHANGES)) then
    OLEDBError(Status, Component, AnalyzeMethod, Arg);
end;

procedure TOLEDBConnection.OLEDBError(const ErrorCode: HRESULT; Component: TObject;
  AnalyzeMethod: TAnalyzeMethod = nil; const Arg: IntPtr = nil);

{$IFDEF CLR}
  function MarshalErrInfo(Err: PSSERRORINFO): SSERRORINFO;
  begin
    Result := SSERRORINFO(Marshal.PtrToStructure(Err, TypeOf(SSERRORINFO)));
  end;
{$ELSE}
  function MarshalErrInfo(Err: PSSERRORINFO): SSERRORINFO;
  begin
    Result.pwszMessage := Err.pwszMessage;
    Result.pwszServer := Err.pwszServer;
    Result.pwszProcedure := Err.pwszProcedure;
    Result.lNative := Err.lNative;
    Result.bState := Err.bState;
    Result.bClass := Err.bClass;
    Result.wLineNumber := Err.wLineNumber;
  end;
{$ENDIF}

{$IFNDEF CLR}
  procedure ObtainErrorParameters(var Msg: WideString; var pIErrorRecords: IErrorRecords; RecNo: integer);

    function GotoBegin(var Position: integer): boolean;
    var
      i, n: integer;
    begin
      Position := -1;
      Result := False;
      Trim(Msg);
      n := length(Msg);
      if Msg[n] = ']' then     // find  [,,,,,] section
        for i := n downto 1 do
          if msg[i] = '[' then begin
            Position := i;
            Result := True;
            Break;
          end;
    end;

    function GotoNextParamPos(var Position: integer): Boolean;
    begin
      Result := True;
      if Msg[Position] = ']' then
        Result := False
      else begin
        inc(Position);
        while((Msg[Position] <> ',') and (Msg[Position] <> ']')) do
          inc(Position);
      end;
    end;

    procedure InsertParam(param: WideString; var msg: WideString; var Position: integer);
    var
      n: integer;
    begin
      n := length(param);
      if n > 0 then begin
        case msg[Position - 1] of
          '[': begin
            Insert(param, msg, Position);
            inc(Position, n);
          end;
          ',': begin
            Insert(' ' + param, msg, Position);
            inc(Position, n + 1);
          end;
          else begin
            Insert(': ' + param, msg, Position);
            inc(Position, n + 2)
          end;
        end;
      end;
    end;

  var
    i: integer;
    idp: DISPPARAMS;
    ov: OleVariant;
    ae: integer;
    Position: integer;
  begin
    if not GotoBegin(Position) then
      Exit;

    if (pIErrorRecords.GetErrorParameters (RecNo, idp) = S_OK) then begin
      VariantInit({$IFDEF FPC}TVarData(ov){$ELSE}ov{$ENDIF});
      for i := pred(idp.cArgs) downto 0  do begin
        if DispGetParam(idp, i, VT_BSTR, {$IFDEF FPC}TVarData(ov){$ELSE}ov{$ENDIF}, ae) <> S_OK then
          VariantClear({$IFDEF FPC}TVarData(ov){$ELSE}ov{$ENDIF});
        if GotoNextParamPos(Position) then
          InsertParam(ov, msg, Position)
      end;
    end;
  end;
{$ENDIF}

  function GetErrMessage(const ErrInfo: {$IFDEF CLR}SSERRORINFO{$ELSE}PSSERRORINFO{$ENDIF}): WideString;
  begin
  {$IFNDEF LITE}
    Result := ErrInfo.pwszMessage;
  {$ELSE}
    Result := 'SQL State: ' + IntToStr(ErrInfo.bState) +
      ', SQL Error Code: ' + IntToStr(ErrInfo.lNative) +
      ', Level: ' + IntToStr(ErrInfo.bClass) +
      ', Procedure: ' + ErrInfo.pwszProcedure +
      ', Line: ' + IntToStr(ErrInfo.wLineNumber) +
      WideString(#$D#$A) + ErrInfo.pwszMessage;
  {$ENDIF};
  end;

var
  pIErrorInfoAll: IErrorInfo;
  pIErrorRecords: IErrorRecords;
  pISQLServerErrorInfo: ISQLServerErrorInfo;
  pssErrInfo: PSSERRORINFO;
  ssErrInfo: SSERRORINFO;
  pStrBuf: IntPtr; // pWideChar;
  Msg: WideString;
  s1: WideString;
  Analysis: _string;
  ErrInfo: ERRORINFO;
  iu: IUnknown;
  ssErrInfoBuf: SSERRORINFO;
  i, RecordCount: cardinal;

  Err: Exception;
  LastErr: EOLEDBError;
  Fail: boolean;
begin
  if (ErrorCode = 0) and not Assigned(FOnInfoMessage) then
    Exit;

  s1 := '';
  RecordCount := 0;
  Err := nil;

  if GetErrorInfo(0, pIErrorInfoAll) = S_OK then begin
    pIErrorInfoAll.GetDescription(Msg);
  {$IFDEF CLR}
    pIErrorRecords := pIErrorInfoAll as IErrorRecords;
  {$ELSE}
    pIErrorInfoAll.QueryInterface(IID_IErrorRecords, pIErrorRecords);
  {$ENDIF}
    if pIErrorRecords <> nil then
      pIErrorRecords.GetRecordCount(RecordCount);

    if RecordCount > 0 then begin
      for i := RecordCount - 1 downto 0 do begin
        if (pIErrorRecords.GetCustomErrorObject(i, IID_ISQLServerErrorInfo, iu) = S_OK) then
          pISQLServerErrorInfo := ISQLServerErrorInfo(iu);
          iu := nil;
          if (pISQLServerErrorInfo <> nil) and
            (pISQLServerErrorInfo.GetErrorInfo(pssErrInfo, pStrBuf) = S_OK) and
            (pssErrInfo <> nil) then begin
          {$IFDEF CLR}
            ssErrInfo := SSERRORINFO(Marshal.PtrToStructure(pssErrInfo, TypeOf(SSERRORINFO)));
            if s1 = '' then begin
              ssErrInfoBuf := ssErrInfo;
              s1 := GetErrMessage(ssErrInfo);
            end
            else
              s1 := GetErrMessage(ssErrInfo) + #$D#$A + s1;
          {$ELSE}
            if s1 = '' then begin
              ssErrInfoBuf := MarshalErrInfo(pssErrInfo);
              s1 := GetErrMessage(pssErrInfo);
            end
            else
              s1 := GetErrMessage(pssErrInfo) + WideString(#$D#$A) + s1;
          {$ENDIF}
            FreeCoMem(pStrBuf);
            FreeCoMem(pssErrInfo);
          end;
      end;
      if s1 <> '' then
        Err := EMSError.Create(ssErrInfoBuf, ErrorCode, s1);
    end;
  end;

  if Err = nil then // this is OLE DB error. As example - repeated Connection.Rollback or non-convergence types of field and parameter
  begin
    if ErrorCode = 0 then
      Exit; // No error and no message

    if Msg = '' then
      Msg := _Format(SOLEDBError, [ErrorCode]);
    if ErrorCode = CO_E_NOTINITIALIZED then
      Msg := Msg + '.'#$D#$A'CoInitialize has not been called.';

    Err := EOLEDBError.Create(ErrorCode, Msg);
  end;

  if Assigned(AnalyzeMethod) then begin
    Analysis := AnalyzeMethod(ErrorCode, Arg);
    if Analysis <> '' then
      AddInfoToErr(Err, Analysis, []);
  end;

{$IFNDEF LITE}
  EOLEDBError(Err).Component := Component;
{$ENDIF}
  EOLEDBError(Err).FOLEDBErrorCode := ErrorCode;
  if RecordCount > 0 then
    for i := 0 to RecordCount - 1 do begin
      if pIErrorRecords.GetCustomErrorObject(i, IID_ISQLServerErrorInfo, iu) = S_OK then begin
        pISQLServerErrorInfo := ISQLServerErrorInfo(iu);
        if (pISQLServerErrorInfo <> nil) and
          (pISQLServerErrorInfo.GetErrorInfo(pssErrInfo, pStrBuf) = S_OK) and
          (pssErrInfo <> nil) then begin
          // EMSError
          ssErrInfo := MarshalErrInfo(pssErrInfo);
          LastErr := EMSError.Create(ssErrInfo, 0, ssErrInfo.pwszMessage);

          FreeCoMem(pStrBuf);
          FreeCoMem(pssErrInfo);
        end
        else begin
        {$IFNDEF CLR}
          if FProvider = prCompact then begin
            ObtainErrorParameters(Msg, pIErrorRecords, i);
            Err.Message := Msg;
          end;
        {$ENDIF}
          LastErr := EOLEDBError.Create(ErrorCode, Msg);
        end;
        EOLEDBError(Err).FErrors.FList.Add(LastErr);

        with LastErr do begin
          if pIErrorRecords.GetBasicErrorInfo(i, ErrInfo) = S_OK then begin
            FOLEDBErrorCode := ErrInfo.hrError;
            // FMinor := ErrInfo.dwMinor;
            // Fclsid := ErrInfo.clsid;
            Fiid := ErrInfo.iid;
            // Fdispid := ErrInfo.dispid;
          end;

          (*if pIErrorRecords.GetErrorInfo(i, GetUserDefaultLCID(), pIErrorInfo) = S_OK then begin
            // GetErrorInfo - IErrorInfo interface
            // pIErrorInfo.GetGUID(Fguid); - same as Fiid
            pIErrorInfo.GetSource(FSource);
            pIErrorInfo.GetDescription(FDescription);
            // pIErrorInfo.GetHelpFile(FHelpFile); - not used by sqloledb
            // pIErrorInfo.GetHelpContext(FdwHelpContext); - not used by sqloledb
          end;*)
        end;
      end;
    end;

  if (ErrorCode = 0) and Assigned(FOnInfoMessage) then begin
    Assert(Err <> nil);
    try
      FOnInfoMessage(Err as EMSError);
    finally
      Err.Free;
    end;
  end
  else
  begin
    Fail := True;
    try
      if Assigned(OnError) then
        DoError(Err{$IFNDEF LITE} as EDAError{$ENDIF}, Fail);
      if Fail then
        raise Err
      else
        Abort;
    finally
      if not Fail then
        Err.Free;
    end;
  end;
end;

procedure TOLEDBConnection.GetConnectionProperties;
var
  PropValues: TPropValues;
begin
  if (FDatabase = '') and (FProvider <> prCompact) then begin
    with TOLEDBPropertiesGet.Create(Self, DBPROPSET_DATASOURCE) do
      try
        AddPropId(DBPROP_CURRENTCATALOG);
        GetProperties(FIDBProperties, PropValues);
      finally
        Free;
      end;
    FDatabase := PropValues[0];;
  end;

  with TOLEDBPropertiesGet.Create(Self, DBPROPSET_DATASOURCEINFO) do
    try
      AddPropId(DBPROP_DBMSNAME);
      AddPropId(DBPROP_DBMSVER);
      AddPropId(DBPROP_PROVIDERFRIENDLYNAME);
      AddPropId(DBPROP_PROVIDERVER);
      if FProvider = prCompact then
        AddPropId(DBPROP_PROVIDERNAME);
      GetProperties(FIDBProperties, PropValues);
    finally
      Free;
    end;

  FDBMSName := PropValues[0];
  FDBMSVer := PropValues[1];
  FDBMSPrimaryVer := StrToInt(Copy(FDBMSVer, 1, Pos('.', FDBMSVer) - 1));

  FProviderFriendlyName := PropValues[2];
  FProviderVer := PropValues[3];
  FProviderPrimaryVer := StrToInt(Copy(FProviderVer, 1, Pos('.', FProviderVer) - 1));

  if (FProvider = prCompact) and (FProviderFriendlyName = '') then begin
    FProviderFriendlyName := PropValues[4];
    if Pos('.dll', _LowerCase(FProviderFriendlyName)) > 0 then
      SetLength(FProviderFriendlyName, Length(FProviderFriendlyName) - 4);
  end;

  if FProvider = prCompact then
    if not IsCompactEdition(FProviderPrimaryVer) then
      DatabaseError(SBadProviderName);
  FServerEdition := '';
end;

procedure TOLEDBConnection.SetConnectionProperties;
var
  BufLen: cardinal;
//  ComputerName: array[0..MAX_COMPUTERNAME_LENGTH] of char;
{$IFDEF CLR}
  AppName: StringBuilder;
{$ELSE}
  AppName: array[0..MAX_PATH + 100] of Char;
{$ENDIF}
  s: _string;

  procedure SetCompactConnectionProperties;
  begin
    with TOLEDBPropertiesSet.Create(Self, DBPROPSET_DBINIT) do
      try
        AddPropStr(DBPROP_INIT_DATASOURCE, FDatabase, True);
        if FInitMode <> imReadWrite then
          case FInitMode of
            imReadOnly:
              AddPropInt(DBPROP_INIT_MODE, DB_MODE_READ);
            imReadWrite:
              AddPropInt(DBPROP_INIT_MODE, DB_MODE_READWRITE);
            imExclusive:
              AddPropInt(DBPROP_INIT_MODE, DB_MODE_SHARE_EXCLUSIVE);
            imShareRead:
              AddPropInt(DBPROP_INIT_MODE, DB_MODE_SHARE_DENY_READ);
            else
              Assert(False);
          end;
        if FLocaleIdentifier <> GetSystemDefaultLCID then
          AddPropUInt(DBPROP_INIT_LCID, FLocaleIdentifier);
        try
          SetProperties(FIDBProperties);
        except
          on E: Exception do begin
            AddInfoToErr(E, SBadDatabaseFile, []);
            raise E;
          end;
        end;
      finally
        Free;
      end;

      with TOLEDBPropertiesSet.Create(Self, DBPROPSET_SSCE_DBINIT) do
        try
          if FPassword <> '' then
            AddPropStr(DBPROP_SSCE_DBPASSWORD, FPassword);
          if FMaxDatabaseSize <> DefaultMaxDatabaseSize then
            AddPropInt(DBPROP_SSCE_MAX_DATABASE_SIZE, FMaxDatabaseSize);
          if FMaxBufferSize <> DefaultMaxBufferSize then
            AddPropInt(DBPROP_SSCE_MAXBUFFERSIZE, FMaxBufferSize);
          if FTempFileDirectory <> '' then
            AddPropStr(DBPROP_SSCE_TEMPFILE_DIRECTORY, FTempFileDirectory);
          if FTempFileMaxSize <> DefaultTempFileMaxSize then
            AddPropInt(DBPROP_SSCE_TEMPFILE_MAX_SIZE, FTempFileMaxSize);
          if FDefaultLockEscalation <> DefaultDefaultLockEscalation then
            AddPropInt(DBPROP_SSCE_DEFAULT_LOCK_ESCALATION, FDefaultLockEscalation);
          if FDefaultLockTimeout <> DefaultDefaultLockTimeout then
            AddPropInt(DBPROP_SSCE_DEFAULT_LOCK_TIMEOUT, FDefaultLockTimeout);
          if FAutoShrinkThreshold <> DefaultAutoShrinkThreshold then
            AddPropInt(DBPROP_SSCE_AUTO_SHRINK_THRESHOLD, FAutoShrinkThreshold);
          if FFlushInterval <> DefaultFlushInterval then
            AddPropInt(DBPROP_SSCE_FLUSH_INTERVAL, FFlushInterval);
          SetProperties(FIDBProperties);
        finally
          Free;
        end;
  end;

begin
  // Set initialization properties
  if FProvider = prCompact then
    SetCompactConnectionProperties
  else begin
    with TOLEDBPropertiesSet.Create(Self, DBPROPSET_DBINIT) do
      try
        // Auth props
        if FServer <> '' then
          AddPropStr(DBPROP_INIT_DATASOURCE, FServer)
        else
          AddPropStr(DBPROP_INIT_DATASOURCE, '(local)');
        AddPropStr(DBPROP_INIT_CATALOG, FDatabase);
        AddPropInt(DBPROP_INIT_TIMEOUT, FConnectionTimeout);

        case FAuthentication of
          auWindows:
            AddPropStr(DBPROP_AUTH_INTEGRATED, '');
          auServer:
            if (FUserName = '') and (FPassword = '') then
              AddPropStr(DBPROP_AUTH_USERID, 'sa')
            else begin
              AddPropStr(DBPROP_AUTH_USERID, FUserName);
              AddPropStr(DBPROP_AUTH_PASSWORD, FPassword);
            end;
        end;

        if FPersistSecurityInfo then
          AddPropBool(DBPROP_AUTH_PERSIST_SENSITIVE_AUTHINFO, FPersistSecurityInfo);

        // Prompt props
        AddPropSmallInt(DBPROP_INIT_PROMPT, DBPROMPT_NOPROMPT);
        AddPropInt(DBPROP_INIT_HWND, 0);

        SetProperties(FIDBProperties);
      finally
        Free;
      end;

    // Set common SQL Server properties
    with TOLEDBPropertiesSet.Create(Self, DBPROPSET_SQLSERVERDBINIT) do
      try
        if FWorkstationID <> '' then
          AddPropStr(SSPROP_INIT_WSID, FWorkstationID);

        if FApplicationName <> '' then
          AddPropStr(SSPROP_INIT_APPNAME, FApplicationName)
        else
        begin
        {$IFDEF CLR}
          AppName := StringBuilder.Create(MAX_PATH + 100);
          try
            BufLen := MAX_PATH + 100;
            GetModuleFileName(0, AppName, BufLen);
            s := ExtractFileName(AppName.ToString);
          finally
            AppName.Free;
          end;
        {$ELSE}
          BufLen := sizeof(AppName);
          GetModuleFileName(0, AppName, BufLen);
          s := ExtractFileName(AppName);
        {$ENDIF}
          AddPropStr(SSPROP_INIT_APPNAME, s);
        end;

        if FLanguage <> '' then
          AddPropStr(SSPROP_INIT_CURRENTLANGUAGE, FLanguage, True);

        AddPropBool(SSPROP_INIT_AUTOTRANSLATE, FAutoTranslate, True);

        if (GUIDToString(FProviderId) = GUIDToString(CLSID_SQLNCLI)) or
           (GUIDToString(FProviderId) = GUIDToString(CLSID_SQLNCLI10)) then begin
          if FMultipleActiveResultSets then
            AddPropBool(SSPROP_INIT_MARSCONNECTION, FMultipleActiveResultSets);
          if FTrustServerCertificate then
            AddPropBool(SSPROP_INIT_TRUST_SERVER_CERTIFICATE, FTrustServerCertificate);
          if FOldPassword <> '' then
            AddPropStr(SSPROP_AUTH_OLD_PASSWORD, FOldPassword);
        end
        else
          if FOldPassword <> '' then
            raise Exception.Create(SSQLNCLINeedsChangePwd);

        if FInitialFileName <> '' then
          AddPropStr(SSPROP_INIT_FILENAME, FInitialFileName);
          
        if FFailoverPartner <> '' then
          AddPropStr(SSPROP_INIT_FAILOVERPARTNER, FFailoverPartner);
          
        SetProperties(FIDBProperties);
      finally
        Free;
      end;
      
    /// Isolated for easy error detection
    if FEncrypt then /// Set only if FEncrypt = True. This needs for prevent troubles with 7.xxx clients
      with TOLEDBPropertiesSet.Create(Self, DBPROPSET_SQLSERVERDBINIT) do
        try
          AddPropBool(SSPROP_INIT_ENCRYPT, FEncrypt);
          try
            SetProperties(FIDBProperties);
          except
            on E: Exception do
            begin
              AddInfoToErr(E, SBadEncrypt, []);
              raise E;
            end;
          end;
        finally
          Free;
        end;
    /// Isolated for easy error detection
    if FNetworkLibrary <> '' then
      with TOLEDBPropertiesSet.Create(Self, DBPROPSET_SQLSERVERDBINIT) do
        try
          AddPropStr(SSPROP_INIT_NETWORKLIBRARY, FNetworkLibrary, True);
          try
            SetProperties(FIDBProperties);
          except
            on E: Exception do begin
              AddInfoToErr(E, SBadNetworkLibrary, []);
              raise E;
            end;
          end;
        finally
          Free;
        end;

    /// Isolated for easy error detection
    with TOLEDBPropertiesSet.Create(Self, DBPROPSET_SQLSERVERDBINIT) do
      try
        AddPropInt(SSPROP_INIT_PACKETSIZE, FPacketSize);
        try
          SetProperties(FIDBProperties);
        except
          on E: Exception do begin
            AddInfoToErr(E, SBadPacketSize, []);
            raise E;
          end;
        end;
      finally
        Free;
      end;
  end;
end;

procedure TOLEDBConnection.Connect(const ConnectString: _string);
  procedure SetSessionProperties;
  var
    IsolationLevel: Variant;
    OLEDBProperties: TOLEDBPropertiesSet;

    procedure SetCompactSessionProperties;
    begin
      OLEDBProperties := TOLEDBPropertiesSet.Create(Self, DBPROPSET_SSCE_SESSION);
      try
        with OLEDBProperties do begin
          if FLockTimeout <> DefaultDefaultLockTimeout then
            AddPropInt(DBPROP_SSCE_LOCK_TIMEOUT, FLockTimeout);
          if FLockEscalation <> DefaultDefaultLockEscalation then
            AddPropInt(DBPROP_SSCE_LOCK_ESCALATION, FLockEscalation);
          case FTransactionCommitMode of
            cmAsynchCommit:
              AddPropInt(DBPROP_SSCE_TRANSACTION_COMMIT_MODE, DBPROPVAL_SSCE_TCM_DEFAULT);
            cmSynchCommit:
              AddPropInt(DBPROP_SSCE_TRANSACTION_COMMIT_MODE, DBPROPVAL_SSCE_TCM_FLUSH);
            else
              Assert(False);
          end;
        end;
        OLEDBProperties.SetProperties(FISessionProperties);
      finally
        OLEDBProperties.Free;
      end;
    end;
    
  begin
    if FProvider = prCompact then
      SetCompactSessionProperties
    else begin
      OLEDBProperties := TOLEDBPropertiesSet.Create(Self, DBPROPSET_SESSION);
      try
        //Set transaction properties
        with OLEDBProperties do begin
          Assert(GetInternalTransaction <> nil);
          GetInternalTransaction.GetProp(prIsolationLevel, IsolationLevel);
          AddPropInt(DBPROP_SESS_AUTOCOMMITISOLEVELS, ConvertIsolationLevelToOLEDBIsoLevel(TCRIsolationLevel(IsolationLevel)));
        end;
        OLEDBProperties.SetProperties(FISessionProperties);
      finally
        OLEDBProperties.Free;
      end;
    end;
  end;

  procedure SetDataSourceProperties;
  var
    OLEDBProperties: TOLEDBPropertiesSet;
  begin
    if FProvider <> prCompact then begin
      OLEDBProperties := TOLEDBPropertiesSet.Create(Self, DBPROPSET_DATASOURCE);
      try
        //Set transaction properties
        with OLEDBProperties do begin
          AddPropBool(DBPROP_MULTIPLECONNECTIONS, True);
          SetProperties(FIDBProperties);
        end;
      finally
        OLEDBProperties.Free;
      end;
    end;
  end;

  function OpenProvider(const clsid: array of TGuid): HRESULT;
  var
    i: integer;
  begin
    Result := 0;
    for i := Low(clsid) to High(clsid) do begin
      Result := CoCreateInstance(clsid[i],
        nil,
        CLSCTX_INPROC_SERVER,
        IID_IDBInitialize,
        FIDBInitialize);
      if Result <> REGDB_E_CLASSNOTREG then begin
        FProviderId := clsid[i];
        Exit;
      end;
    end;
  end;

  // if database file exists, this method tries to determine its version
  // do nothing if db file does not exists or signature is not recognized
  // if signature is recognized, CompactVer will be corrected if FCompactVersion = prAuto
  // raise error if actual file version was determined successfully, and it differs from provided FCompactVersion
  // signature is 4 bytes at offset 0x10 (16) in the database file
  procedure CheckCompactDBFile(var CompactVer: TCompactVersion);
  var
    fs: TFileStream;
    w: cardinal;
    Determined: boolean;
  begin
    Determined := False;
    if not FileExists(FDatabase) then
      Exit;

    try // hide exception if access to the file denied (it may be open by another connection)
      fs := TFileStream.Create(FDatabase, fmOpenRead or fmShareDenyNone);

      try
        if fs.Size < 20 then
          Exit;
        fs.Seek(16, soFromBeginning);
        fs.ReadBuffer(w, 4);

        case w of
          4000000: begin
            CompactVer := cv40;
            Determined := True;
          end;
          3505053: begin
            CompactVer := cv35;
            Determined := True;
          end;
          3004180: begin
            CompactVer := cv30;
            Determined := True;
          end;
        end;

      finally
        fs.Free;
      end;

    except
    end;

    if Determined and (FCompactVersion <> cvAuto) and (FCompactVersion <> CompactVer) then
      DatabaseError(SDBVerAndCompactVerDiffer);
  end;

var
  hr: HRESULT;
  iu: IUnknown;
  TmpCompactVer: TCompactVersion;
begin
  if not FConnected then begin
    try
      hr := 0;
      case FProvider of
        prAuto:
          hr := OpenProvider([CLSID_SQLNCLI10, CLSID_SQLNCLI, CLSID_SQLOLEDB]);
        prSQL:
          hr := OpenProvider([CLSID_SQLOLEDB]);
        prNativeClient:
          case FNativeClientVersion of
            ncAuto:
              hr := OpenProvider([CLSID_SQLNCLI10, CLSID_SQLNCLI]);
            nc2008:
              hr := OpenProvider([CLSID_SQLNCLI10]);
          else
            Assert(False);
          end;
        prCompact: begin
          TmpCompactVer := FCompactVersion;
          CheckCompactDBFile(TmpCompactVer);
          case TmpCompactVer of
            cvAuto:
              hr := OpenProvider([CLSID_SQLSERVERCE_4_0, CLSID_SQLSERVERCE_3_5, CLSID_SQLSERVERCE_3_0]);
            cv30:
              hr := OpenProvider([CLSID_SQLSERVERCE_3_0]);
            cv35:
              hr := OpenProvider([CLSID_SQLSERVERCE_3_5]);
            cv40:
              hr := OpenProvider([CLSID_SQLSERVERCE_4_0]);
          else
            Assert(False);
          end;
        end;
      else
        DatabaseError(SBadProviderName);
      end;

      if hr = REGDB_E_CLASSNOTREG then
        DatabaseError(SMSSQLNotFound)
      else
        Check(hr, Component);

      QueryIntf(FIDBInitialize, {$IFDEF CLR}IDBProperties{$ELSE}IID_IDBProperties{$ENDIF}, FIDBProperties);

      //Set initialization properties.
      SetConnectionProperties;
      // SetDatabase(FDatabase); - setted on SetConnectionProperties

      //Now establish the connection to the data source.
      hr := FIDBInitialize.Initialize;
      if (hr = E_FAIL) and (FProvider = prCompact) then begin // Database file does not exist
        try
          OLEDBError(hr, Component);
        except
          on e: EOLEDBError do begin
            if Pos('The database file cannot be found', e.Message) > 0 then begin
              CreateCompactDatabase;
              hr := FIDBInitialize.Initialize;
            end
            else
              raise;
          end;
        end;
      end;
      Check(hr, Component);

      SetDataSourceProperties;

      //Create the SessionObject
      QueryIntf(FIDBInitialize, {$IFDEF CLR}IDBCreateSession{$ELSE}IID_IDBCreateSession{$ENDIF}, FIDBCreateSession);
      Check(FIDBCreateSession.CreateSession(nil, IID_ISessionProperties, iu), Component);
      FISessionProperties := ISessionProperties(iu);
      SetSessionProperties;

      // Get properties ------------------------------
      GetConnectionProperties;
      inherited;
      FConnected := True;
      FNativeConnection := True;

      if not FQuotedIdentifier then
        SetQuotedIdentifier(FQuotedIdentifier);

      if (FProvider <> prCompact) and FSetLockTimeout then
        ExecSQL('SET LOCK_TIMEOUT ' + IntToStr(FDefaultLockTimeout));

    except
      on EFailOver do;
      else begin
        ReleaseInterfaces;
        raise;
      end;
    end;
  end;
end;

procedure TOLEDBConnection.Disconnect;
begin
  if FConnected then begin
    if FNativeConnection then begin
      ReleaseInterfaces;
    end;

    FConnected := False;
    FreeAndNil(FColumnsMetaInfo);
    FreeAndNil(FColumnsRowsetFieldDescs);
  end;
end;

procedure TOLEDBConnection.Assign(Source: TCRConnection);
var
  Src: TOLEDBConnection;
begin
  inherited;

  Src := TOLEDBConnection(Source);
  FConnectionTimeout := Src.FConnectionTimeout;
  FDatabase := Src.FDatabase;
  FAuthentication := Src.FAuthentication;
  FProvider := Src.FProvider;
  FQuotedIdentifier := Src.FQuotedIdentifier;
  FLanguage := Src.FLanguage;
  FEncrypt := Src.FEncrypt;
  FPersistSecurityInfo := Src.FPersistSecurityInfo;
  FAutoTranslate := Src.FAutoTranslate;
  FNetworkLibrary := Src.FNetworkLibrary;
  FApplicationName := Src.FApplicationName;
  FWorkstationID := Src.FWorkstationID;
  FPacketSize := Src.FPacketSize;
  FMaxDatabaseSize := Src.FMaxDatabaseSize;
  FTrustServerCertificate := Src.FTrustServerCertificate;
end;

procedure TOLEDBConnection.AssignConnect(Source: TCRConnection);
var
  Src: TOLEDBConnection;
begin
  if Source <> Self then begin
    Disconnect;
    if Source <> nil then begin
      Src := TOLEDBConnection(Source);
      Assign(Src);
      FISessionProperties := Src.FISessionProperties;
      FIDBInitialize := Src.FIDBInitialize;
      FIDBProperties := Src.FIDBProperties;
      FIDBCreateSession := Src.FIDBCreateSession;
      FInternalTransaction.AssignConnect(Src.FInternalTransaction);
      FConnected := True;
      FNativeConnection := False;
    end;
  end;
end;

function TOLEDBConnection.GetIDBCreateSession: IDBCreateSession;
begin
  Result := FIDBCreateSession;
end;

procedure TOLEDBConnection.SetIDBCreateSession(CreateSession: IDBCreateSession);
var
  iu: IUnknown;
begin
  Assert(not FConnected);

  //Obtain access to the SQLOLEDB provider.
  try
    FIDBCreateSession := CreateSession;
    Check(FIDBCreateSession.CreateSession(nil, IID_ISessionProperties, iu), Component);
    FISessionProperties := ISessionProperties(iu);
    FConnected := True;
    if not FQuotedIdentifier then
      SetQuotedIdentifier(FQuotedIdentifier);
  except
    ReleaseInterfaces;
    raise;
  end;
end;

function TOLEDBConnection.GetProp(Prop: integer; var Value: variant): boolean;
begin
  Result := True;

  case Prop of
    prDatabase:
      Value := FDatabase; // string
    prAuthentication:
      Value := Variant(FAuthentication); //TMSAuthentication
    prConnectionTimeout:
      Value := Integer(FConnectionTimeout);
    prMaxDatabaseSize:
      Value := Integer(FMaxDatabaseSize);
    prProvider:
      Value := Variant(FProvider);
  else
    Result := inherited GetProp(Prop, Value);
  end;

end;

function TOLEDBConnection.GetServerVersion: _string;
begin
  Result := DBMSVer;
end;

function TOLEDBConnection.GetServerVersionFull: _string;
begin
  Result := DBMSName + ' ' + DBMSVer;
end;

function TOLEDBConnection.GetClientVersion: _string;
begin
  Result := ProviderVer;
end;

function TOLEDBConnection.GetServerEdition: string;
var
  RecordSet: TOLEDBRecordSet;
  RecBuf: IntPtr;
begin
  if FProvider = prCompact then
    FServerEdition := 'compact edition';

  if FServerEdition = '' then begin
    RecordSet := TOLEDBRecordSet.Create;
    try
      RecordSet.SetConnection(Self);
      RecordSet.SetProp(prFetchAll, True);
      RecordSet.SetSQL('SELECT CAST(SERVERPROPERTY(''EDITION'') AS VARCHAR(128))');
      RecordSet.Open;
      RecBuf := nil;
      RecordSet.AllocRecBuf(RecBuf);
      try
        RecordSet.GetNextRecord(RecBuf);
        if not RecordSet.Eof and not RecordSet.GetNull(1, RecBuf) then
          FServerEdition := string(Marshal.PtrToStringAnsi(PtrOffset(RecBuf, 0)));
      finally
        if RecBuf <> nil then
          RecordSet.FreeRecBuf(RecBuf);
      end;
    finally
      RecordSet.Close;
      RecordSet.Free;
    end;
  end;

  Result := FServerEdition;
end;

function TOLEDBConnection.IsSQLAzureEdition: boolean;
begin
  Result := LowerCase(GetServerEdition) = 'sql azure';
end;

function TOLEDBConnection.CanChangeDatabase: boolean;
begin
  Result := FProvider <> prCompact;
end;

function TOLEDBConnection.SetProp(Prop: integer; const Value: variant): boolean;
begin
  Result := True;
  case Prop of
    prDatabase:
      SetDatabase(Value);
    {prMultipleConnections:
      FMultipleConnections := boolean(Value^);}
    prAuthentication:
      FAuthentication := TMSAuthentication(Value); //TMSAuthentication
    prProvider:
      FProvider := TOLEDBProvider(Value);
    prCompactVersion:
      FCompactVersion := TCompactVersion(Value);
    prNativeClientVersion:
      FNativeClientVersion := TNativeClientVersion(Value);
    prConnectionTimeout:
      FConnectionTimeout := Integer(Value);
    prQuotedIdentifier:
      if FQuotedIdentifier <> boolean(Value) then
        SetQuotedIdentifier(boolean(Value));
    prLanguage:
      FLanguage := Value;
    prAutoTranslate:
      FAutoTranslate := Boolean(Value);
    prEncrypt:
      FEncrypt := Boolean(Value);
    prPersistSecurityInfo:
      FPersistSecurityInfo := Boolean(Value);
    prNetworkLibrary:
      FNetworkLibrary := Value;
    prApplicationName:
      FApplicationName := Value;
    prWorkstationID:
      FWorkstationID := Value;
    prPacketSize:
      FPacketSize := Integer(Value);
    prInitialFileName:
      FInitialFileName := Value;
    prMARS:
      FMultipleActiveResultSets := Value;
    prWideMemos:
      FWideMemos := Value;
    prTrustServerCertificate:
      FTrustServerCertificate := Value;
    prOldPassword:
      FOldPassword := Value;
    prMaxDatabaseSize:
      FMaxDatabaseSize := Value;
    prFailoverPartner:
      FFailoverPartner := Value;
    // Compact Edition specific
    prTempFileDirectory:
      FTempFileDirectory := Value;
    prTempFileMaxSize:
      FTempFileMaxSize := Value;
    prDefaultLockEscalation:
      FDefaultLockEscalation := Value;
    prDefaultLockTimeout:
      FDefaultLockTimeout := Value;
    prAutoShrinkThreshold:
      FAutoShrinkThreshold := Value;
    prMaxBufferSize:
      FMaxBufferSize := Value;
    prFlushInterval:
      FFlushInterval := Value;
    prTransactionCommitMode:
      FTransactionCommitMode := TCompactCommitMode(Integer(Value));
    prLockTimeout:
      FLockTimeout := Value;
    prLockEscalation:
      FLockEscalation := Value;
    prInitMode:
      FInitMode := TMSInitMode(Integer(Value));
    prLocaleIdentifier:
      FLocaleIdentifier := Value;
    prSetLockTimeout:
      FSetLockTimeout := Value;
  else
    Result := inherited SetProp(Prop, Value);
  end;
end;

function TOLEDBConnection.CheckIsValid: boolean;
begin
  FIsValid := FIDBInitialize <> nil;
  if FIsValid then
    try
      ExecSQL(SCheckConnection);
    except
      FIsValid := False;
    end;
  Result := FIsValid;
end;

procedure TOLEDBConnection.SetDatabase(const Value: _string);
var
  OLEDBProperties: TOLEDBPropertiesSet;
begin
  if FConnected and (Value = '') then
    DatabaseError(SWrongDatabaseName);

  if FIDBProperties <> nil then begin
    OLEDBProperties := TOLEDBPropertiesSet.Create(Self, DBPROPSET_DATASOURCE);
    try
      with OLEDBProperties do
        AddPropStr(DBPROP_CURRENTCATALOG, OLEDBSQLInfo.NormalizeName(Value));
      OLEDBProperties.SetProperties(FIDBProperties);
    finally
      OLEDBProperties.Free;
    end;
  end;
  FDatabase := Value;
end;

procedure TOLEDBConnection.SetQuotedIdentifier(const Value: boolean);
begin
  FQuotedIdentifier := Value;

  if FConnected then
    if Value then
      ExecSQL('SET QUOTED_IDENTIFIER ON')
    else
      ExecSQL('SET QUOTED_IDENTIFIER OFF');
end;

procedure TOLEDBConnection.ExecSQL(const Text: _string);
begin
  if FCommand = nil then begin
    FCommand := TOLEDBCommand.Create;
    TOLEDBCommand(FCommand).FConnection := Self;
  end;

  with FCommand do begin
    SetSQL(Text);
    try
      Execute;
    finally
      FCommand.SetCursorState(csInactive); // To prevent blocking execute on second exec
    end;
  end;
end;

class procedure TOLEDBConnection.AssignFieldDescs(Source, Dest: TFieldDescs);
var
  i: integer;
  Field, FieldSource: TOLEDBFieldDesc;
begin
  Dest.Clear;
  Dest.Capacity := Source.Count;
  for i := 0 to Source.Count - 1 do begin
    FieldSource := TOLEDBFieldDesc(Source[i]);
    Field := TOLEDBFieldDesc.Create;
    Dest.Add(Field);
    Field.Name := FieldSource.Name;
    Field.ActualName := FieldSource.ActualName;
    // FTableName: string;      // table of name that holds this field
    Field.DataType := FieldSource.DataType;
    Field.SubDataType := FieldSource.SubDataType;
    Field.Length := FieldSource.Length;       // precision for number
    Field.Scale := FieldSource.Scale;
    Field.FieldNo := FieldSource.FieldNo;
    Field.ActualFieldNo := FieldSource.ActualFieldNo;
    Field.Size := FieldSource.Size;
    //Field.DataSize := FieldSource.DataSize;
    Field.Offset := FieldSource.Offset;
    Field.DataOffset := FieldSource.DataOffset;
    Field.Required := FieldSource.Required;
    Field.ReadOnly := FieldSource.ReadOnly;
    Field.IsKey := FieldSource.IsKey;
    Field.Fixed := FieldSource.Fixed;
    Field.Hidden := FieldSource.Hidden;
    //Field.ObjectType := FieldSource.ObjectType;
    //Field.ParentField := FieldSource.ParentField;
    //Field.HiddenObject := FieldSource.HiddenObject;
    //FHandle: IntPtr;    // IntPtr to field specific data
    //FReserved: boolean;  // reserved flag for perfomance optimization
  end;
end;

{$IFDEF CLR}
procedure TOLEDBConnection.DoError(E: Exception; var Fail: boolean);
begin
  inherited;
end;
{$ENDIF}

procedure TOLEDBConnection.CreateCompactDatabase;
const
  SSCEPropCount = 10;
var
  pIDBDataSourceAdmin: IDBDataSourceAdmin;
  pIUnknownSession: IUnknown;

  PPropertySet: IntPtr;
  PropertySet: PDBPropSet;
  PDBProperty: IntPtr;
  DBProperty: PDBProp;
  PSSCEProperty: IntPtr;
  SSCEProperty: PDBPROP;
begin
  PPropertySet := nil;
  PDBProperty := nil;
  PSSCEProperty := nil;
  try
    Check(CoCreateInstance(FProviderId, nil, CLSCTX_INPROC_SERVER,
      IID_IDBDataSourceAdmin, pIDBDataSourceAdmin), Component);

    PDBProperty := Marshal.AllocHGlobal(SizeOfDBProp);
    FillChar(PDBProperty, SizeOfDBProp, $00);
    DBProperty := PDBProperty;
    DBProperty.dwPropertyID := DBPROP_INIT_DATASOURCE;
    DBProperty.dwOptions := DBPROPOPTIONS_REQUIRED;
    DBProperty.vValue := FDatabase;

    PSSCEProperty := Marshal.AllocHGlobal(SizeOfDBProp * SSCEPropCount);
    FillChar(PSSCEProperty, SizeOfDBProp * SSCEPropCount, $00);

    SSCEProperty := PSSCEProperty;
    SSCEProperty.dwPropertyID := DBPROP_SSCE_ENCRYPTDATABASE;
    SSCEProperty.dwOptions := DBPROPOPTIONS_REQUIRED;
    SSCEProperty.vValue := VarAsBooleanType(FEncrypt);

    SSCEProperty := PtrOffset(PSSCEProperty, SizeOfDBProp);
    SSCEProperty.dwPropertyID := DBPROP_SSCE_DBPASSWORD;
    SSCEProperty.dwOptions := DBPROPOPTIONS_REQUIRED;
    SSCEProperty.vValue := FPassword;

    SSCEProperty := PtrOffset(PSSCEProperty, SizeOfDBProp * 2);
    SSCEProperty.dwPropertyID := DBPROP_SSCE_MAX_DATABASE_SIZE;
    SSCEProperty.dwOptions := DBPROPOPTIONS_REQUIRED;
    SSCEProperty.vValue := VarAsType(FMaxDatabaseSize, VT_I4);

    SSCEProperty := PtrOffset(PSSCEProperty, SizeOfDBProp * 3);
    SSCEProperty.dwPropertyID := DBPROP_SSCE_MAXBUFFERSIZE;
    SSCEProperty.dwOptions := DBPROPOPTIONS_REQUIRED;
    SSCEProperty.vValue := VarAsType(FMaxBufferSize, VT_I4);

    SSCEProperty := PtrOffset(PSSCEProperty, SizeOfDBProp * 4);
    SSCEProperty.dwPropertyID := DBPROP_SSCE_TEMPFILE_DIRECTORY;
    SSCEProperty.dwOptions := DBPROPOPTIONS_REQUIRED;
    SSCEProperty.vValue := FTempFileDirectory;

    SSCEProperty := PtrOffset(PSSCEProperty, SizeOfDBProp * 5);
    SSCEProperty.dwPropertyID := DBPROP_SSCE_TEMPFILE_MAX_SIZE;
    SSCEProperty.dwOptions := DBPROPOPTIONS_REQUIRED;
    SSCEProperty.vValue := VarAsType(FTempFileMaxSize, VT_I4);

    SSCEProperty := PtrOffset(PSSCEProperty, SizeOfDBProp * 6);
    SSCEProperty.dwPropertyID := DBPROP_SSCE_DEFAULT_LOCK_ESCALATION;
    SSCEProperty.dwOptions := DBPROPOPTIONS_REQUIRED;
    SSCEProperty.vValue := VarAsType(FDefaultLockEscalation, VT_I4);

    SSCEProperty := PtrOffset(PSSCEProperty, SizeOfDBProp * 7);
    SSCEProperty.dwPropertyID := DBPROP_SSCE_DEFAULT_LOCK_TIMEOUT;
    SSCEProperty.dwOptions := DBPROPOPTIONS_REQUIRED;
    SSCEProperty.vValue := VarAsType(FDefaultLockTimeout, VT_I4);

    SSCEProperty := PtrOffset(PSSCEProperty, SizeOfDBProp * 8);
    SSCEProperty.dwPropertyID := DBPROP_SSCE_AUTO_SHRINK_THRESHOLD;
    SSCEProperty.dwOptions := DBPROPOPTIONS_REQUIRED;
    SSCEProperty.vValue := VarAsType(FAutoShrinkThreshold, VT_I4);

    SSCEProperty := PtrOffset(PSSCEProperty, SizeOfDBProp * 9);
    SSCEProperty.dwPropertyID := DBPROP_SSCE_FLUSH_INTERVAL;
    SSCEProperty.dwOptions := DBPROPOPTIONS_REQUIRED;
    SSCEProperty.vValue := VarAsType(FFlushInterval, VT_I4);

    PPropertySet := Marshal.AllocHGlobal(SizeOf(DBPROPSET) * 2);
    FillChar(PPropertySet, SizeOf(DBPROPSET) * 2, $00);
    PropertySet := PPropertySet;
    PropertySet.guidPropertySet := DBPROPSET_DBINIT;
    PropertySet.rgProperties := PDBProperty;
    PropertySet.cProperties := 1;

    PropertySet := PtrOffset(PPropertySet, SizeOf(DBPROPSET));
    PropertySet.guidPropertySet := DBPROPSET_SSCE_DBINIT ;
    PropertySet.rgProperties := PSSCEProperty;
    PropertySet.cProperties := SSCEPropCount;

    Check(pIDBDataSourceAdmin.CreateDataSource(2, PPropertySet, nil,
      IID_IUnknown, pIUnknownSession), Component);
  finally
    pIUnknownSession := nil;
    if PPropertySet <> nil then
      Marshal.FreeHGlobal(PPropertySet);
    if PSSCEProperty <> nil then
      Marshal.FreeHGlobal(PSSCEProperty);
    if PDBProperty <> nil then
      Marshal.FreeHGlobal(PDBProperty);
  end;
end;

function TOLEDBConnection.GetSchemaRowset(const Schema: TGUID; rgRestrictions: TRestrictions): IRowset;
var
  i: integer;
  iu: IUnknown;
  DBSchemaRowset: IDBSchemaRowset;
  rgRestrictionsPtr: IntPtr;
begin
  for i := Low(rgRestrictions) to High(rgRestrictions) do
    if (VarType(rgRestrictions[i]) = varOleStr) and (_string(rgRestrictions[i]) = '') then
      rgRestrictions[i] := Null; //
      // rgRestrictions[i] := Unassigned;
      // TVarData(rgRestrictions[i]).VType := varNull;

  QueryIntf(SessionProperties, {$IFDEF CLR}IDBSchemaRowset{$ELSE}IID_IDBSchemaRowset{$ENDIF}, DBSchemaRowset);

{$IFDEF CLR}
  i := SizeOf_OleVariant * Length(rgRestrictions);
  rgRestrictionsPtr := Marshal.AllocHGlobal(i);
  try
    FillChar(rgRestrictionsPtr, i, 0);
    for i := Low(rgRestrictions) to High(rgRestrictions) do
      SetOleVariant(PtrOffset(rgRestrictionsPtr, SizeOf_OleVariant * (Integer(i) - Low(rgRestrictions))), rgRestrictions[i]);
{$ELSE}
    rgRestrictionsPtr := @rgRestrictions[0];
{$ENDIF}
    Check(DBSchemaRowset.GetRowset(nil, Schema, Length(rgRestrictions), rgRestrictionsPtr,
      IID_IRowset, 0, nil, iu), Component);
{$IFDEF CLR}
  finally
    for i := Low(rgRestrictions) to High(rgRestrictions) do
      OleVarClear(PtrOffset(rgRestrictionsPtr, SizeOf_OleVariant * (Integer(i) - Low(rgRestrictions))));
    Marshal.FreeHGlobal(rgRestrictionsPtr);
  end;
{$ENDIF}
  Result := IRowset(iu);
  iu := nil;
end;

{$IFNDEF LITE}
procedure TOLEDBConnection.Enlist(Transaction: TMTSTransaction);
var
  TRIsolationLevel: Variant;
  IsolationLevel: integer;
  IsolationFlags: integer;
  TransactionOptions: ITransactionOptions;
begin
  Transaction.GetProp(CRAccess.prIsolationLevel, TRIsolationLevel);
  case TCRIsolationLevel(Integer(TRIsolationLevel)) of
{    CRAccess.ilReadCommitted:
      IsolationLevel := ISOLATIONLEVEL_READCOMMITTED;
    CRAccess.ilIsolated:
      IsolationLevel := ISOLATIONLEVEL_SERIALIZABLE;   }

    ilReadUnCommitted:
      IsolationLevel := ISOLATIONLEVEL_READUNCOMMITTED;
    ilReadCommitted:
      IsolationLevel := ISOLATIONLEVEL_READCOMMITTED;
    ilRepeatableRead:
      IsolationLevel := ISOLATIONLEVEL_REPEATABLEREAD;
    ilIsolated:
      IsolationLevel := ISOLATIONLEVEL_SERIALIZABLE;
  else
    Assert(False);
    IsolationLevel := 0;
  end;
  IsolationFlags := 0;
  TransactionOptions := nil;

  QueryIntf(FISessionProperties, {$IFDEF CLR}ITransactionJoin{$ELSE}IID_ITransactionJoin{$ENDIF}, FITransactionJoin);
  Check(FITransactionJoin.JoinTransaction(Transaction.MTSTransaction, IsolationLevel, IsolationFlags, TransactionOptions), Component);
end;

procedure TOLEDBConnection.UnEnlist(Transaction: TMTSTransaction);
begin
  if FITransactionJoin <> nil then
    Check(FITransactionJoin.JoinTransaction(nil, 0, 0, nil), Component);
end;
{$ENDIF}

{ TOLEDBCommand }

procedure TOLEDBCommand.Check(const Status: HRESULT; AnalyzeMethod: TAnalyzeMethod = nil);
begin
  Assert(FConnection <> nil);
  if Status <> S_OK then
    FConnection.Check(Status, Component, AnalyzeMethod, nil);
end;

constructor TOLEDBCommand.Create;
begin
  inherited;
  FQueryIntCnt := 0;
  FRequestIUnknown := False;
  FParamsAccessorDataAvaible := False;

  FRowsAffected := -1;

  FCursorState := csInactive;

  FBreakExecCS := TCriticalSection.Create;
end;

destructor TOLEDBCommand.Destroy;
begin
  if FNonBlocking and (FExecutor <> nil) then
    BreakExec;
  FISSAsynchStatus := nil;
  ClearIMultipleResults;
  FIUnknownNext := nil; /// Clear this interface before RequestParams to avoid AV. See TDbxSdaTestSet.DoTestSPNextRecordSet
  RequestParamsIfPossible;
  UnPrepare;
  Assert(FQueryIntCnt = 0, Format('TOLEDBCommand.Destroy - interfaces not released (%d)', [FQueryIntCnt]));
  FBreakExecCS.Free;

  inherited;
end;

procedure TOLEDBCommand.QueryInterfaces(const QueryPrepare: boolean); // QueryPrepare must be True to request IID_ICommandPrepare
var
  CreateCommand: IDBCreateCommand;
  iu: IUnknown;
begin
  if FQueryIntCnt = 0 then begin
    Assert(FConnection <> nil);
    QueryIntf(FConnection.FISessionProperties, {$IFDEF CLR}IDBCreateCommand{$ELSE}IID_IDBCreateCommand{$ENDIF}, CreateCommand);

    Check(CreateCommand.CreateCommand(nil, IID_ICommandText, iu));
    FICommandText := ICommandText(iu);
    if QueryPrepare then
      QueryIntf(FICommandText, {$IFDEF CLR}ICommandPrepare{$ELSE}IID_ICommandPrepare{$ENDIF}, FICommandPrepare)
    else
      FICommandPrepare := nil;
    QueryIntf(FICommandText, {$IFDEF CLR}ICommandProperties{$ELSE}IID_ICommandProperties{$ENDIF}, FICommandProperties);
  end;

  Inc(FQueryIntCnt);
end;

procedure TOLEDBCommand.ReleaseInterfaces;
begin
  if FQueryIntCnt = 0 then // Exception on TOLEDBRecordSet.InternalOpen -> TOLEDBRecordSet.QueryCommandInterfaces -> TOLEDBCommand.QueryInterfaces
    Exit;

  if FQueryIntCnt = 1 then begin
    FBreakExecCS.Acquire;
    try
      FICommandText := nil;
    finally
      FBreakExecCS.Release;
    end;
    FICommandPrepare := nil;
    FICommandProperties := nil;
  end;

  Dec(FQueryIntCnt);
end;

class function TOLEDBCommand.GetTableInfoClass: TTableInfoClass;
begin
  Result := TOLEDBTableInfo;
end;

class function TOLEDBCommand.GetSQLInfoClass: TSQLInfoClass;
begin
  Result := TOLEDBSQLInfo;
end;

class function TOLEDBCommand.GetParserClass: TSQLParserClass;
begin
  Result := TMSParser;
end;

var
  AnsiCodePageName: _string;

function GetCurrentAnsiCodePage: _string;
  function LCIDToCodePage(ALcid: LongWord): Integer;
  const
    CP_ACP = 0;
    LOCALE_IDEFAULTANSICODEPAGE = $00001004;
    BufSize = 7;
  var
    ResultCode: Integer;
  {$IFDEF CLR}
    SBBuffer: StringBuilder;
  {$ELSE}
    Buffer: array [0..BufSize - 1] of Char;
  {$ENDIF}
  begin
  {$IFDEF CLR}
    SBBuffer := StringBuilder.Create(BufSize);
    try
  {$ENDIF}
      GetLocaleInfo(ALcid, LOCALE_IDEFAULTANSICODEPAGE, {$IFDEF CLR}SBBuffer{$ELSE}Buffer{$ENDIF}, BufSize);
      Val({$IFDEF CLR}SBBuffer.ToString{$ELSE}Buffer{$ENDIF}, Result, ResultCode);
  {$IFDEF CLR}
    finally
      SBBuffer.Free;
    end;
  {$ENDIF}
    if ResultCode <> 0 then
      Result := CP_ACP;
  end;
var
  CodePage: integer;
  pIMultiLanguage: IMultiLanguage;
  CodePageInfo: TMIMECPInfo;
begin
  if AnsiCodePageName = '' then begin
    CodePage := LCIDToCodePage(GetThreadLocale);
    AnsiCodePageName := 'ASCII';
    if CoCreateInstance(CLSID_CMultiLanguage, nil, CLSCTX_INPROC_SERVER, IID_IMultiLanguage, pIMultiLanguage) = S_OK then
      if pIMultiLanguage.GetCodePageInfo(CodePage, CodePageInfo) = S_OK then
        AnsiCodePageName := CodePageInfo.wszWebCharset;
  end;
  Result := AnsiCodePageName;
end;

procedure TOLEDBCommand.SetCommandProp;
var
  OLEDBProperties: TOLEDBPropertiesSet;
  Str: WideString;
  g: TGUID;
begin
{$IFDEF AUTOTEST}
  Inc(__SetCommandPropCount);
{$ENDIF}
  Assert(FICommandText <> nil);

  Str := FSQL;
  g := DBGUID_DEFAULT;

  Check(FICommandText.SetCommandText(g, {$IFDEF CLR}Str{$ELSE}PWideChar(Str){$ENDIF}));

  OLEDBProperties := TOLEDBPropertiesSet.Create(FConnection, DBPROPSET_ROWSET);
  try
    with OLEDBProperties do begin
      if FConnection.FProvider <> prCompact then
        AddPropInt(DBPROP_COMMANDTIMEOUT, FCommandTimeout);
       if FNonBlocking and (not FRequestIUnknown) then begin
        if (GUIDToString(FConnection.FProviderId) <> GUIDToString(CLSID_SQLNCLI)) and
           (GUIDToString(FConnection.FProviderId) <> GUIDToString(CLSID_SQLNCLI10)) then
          raise Exception.Create(SSQLNCLINeeds);
        AddPropInt(DBPROP_ROWSET_ASYNCH, DBPROPVAL_ASYNCH_INITIALIZE);
      end;
    end;
    OLEDBProperties.SetProperties(FICommandProperties);
  finally
    OLEDBProperties.Free;
  end;

  if FNotification and not FDelayedSubsciption then begin
    OLEDBProperties := TOLEDBPropertiesSet.Create(FConnection, DBPROPSET_SQLSERVERROWSET);
    try
      with OLEDBProperties do begin
        AddPropStr(SSPROP_QP_NOTIFICATION_MSGTEXT, WideString(FNotificationMessage), True);
        AddPropUInt(SSPROP_QP_NOTIFICATION_TIMEOUT, FNotificationTimeout);
        AddPropStr(SSPROP_QP_NOTIFICATION_OPTIONS, _Format('service=%s', [FNotificationService]), True);
      end;
      OLEDBProperties.SetProperties(FICommandProperties);
    finally
      OLEDBProperties.Free;
    end;
  end;

  if GetUseOutputStream then begin
    OLEDBProperties := TOLEDBPropertiesSet.Create(FConnection, DBPROPSET_STREAM);
    try
      with OLEDBProperties do begin
        case FOutputEncoding of
          oeANSI:
            AddPropStr(DBPROP_OUTPUTENCODING, GetCurrentAnsiCodePage);
          oeUTF8:
            AddPropStr(DBPROP_OUTPUTENCODING, 'UTF-8');
          oeUnicode:
            AddPropStr(DBPROP_OUTPUTENCODING, 'Unicode');
          else
            Assert(False);
        end;
        AddPropIntf(DBPROP_OUTPUTSTREAM, FOLEDBStream, True);
      end;
      OLEDBProperties.SetProperties(FICommandProperties);
    finally
      OLEDBProperties.Free;
    end;
  end;
end;

procedure TOLEDBCommand.SetParameterInfo;
var
  CommandWithParameters: ICommandWithParameters;
  SSCommandWithParameters: ISSCommandWithParameters;
  i, j: integer;
  rgParamOrdinals: array of DB_UPARAMS;
  rgParamBindInfo: array of TDBParamBindInfo;
  ParamDesc: TParamDesc;
  hr: HResult;
  ParamCount: integer;

  ParamVarType: TVarType;
  prgParamOrdinals: IntPtr;
  prgParamBindInfo: IntPtr;
{$IFDEF CLR}
  rgParamOrdinalsGC: GCHandle;
  rgParamBindInfoGC: GCHandle;
{$ENDIF}
  IsUnicode: boolean;
  ParamName: _string;

  TableObject: TMSTableObject;
  FieldsArray: Variant;
  SSParamProps: array of TSSParamProps;
  DBPropSet: array of TDBPropSet;
  DBProp: array of TDBProp;
  pSSParamProps: IntPtr;
  pDBPropSet: IntPtr;
  pDBProp: IntPtr;
{$IFDEF CLR}
  SSParamPropsGC: GCHandle;
  DBPropSetGC: GCHandle;
  DBPropGC: GCHandle;
{$ENDIF}
  SSParamsLength: integer;

begin
  QueryIntf(FICommandText, {$IFDEF CLR}ICommandWithParameters{$ELSE}IID_ICommandWithParameters{$ENDIF}, CommandWithParameters);
  CommandWithParameters.SetParameterInfo(0, nil, nil); /// Clear, just in case

  ParamCount := FParams.Count;
  if ParamCount <> 0 then begin
    SetLength(rgParamOrdinals, ParamCount);
    SetLength(rgParamBindInfo, ParamCount);
    SSParamsLength := 0;
    try

      //OFS('=================================');
      for i := 0 to ParamCount - 1 do begin
        ParamDesc := FParams[i];
        rgParamOrdinals[i] := i + 1;

        case ParamDesc.GetDataType and varTypeMask of
          dtUnknown: begin
            ParamVarType := VarType(ParamDesc.Value);
            case ParamVarType of
              varSmallint: { vt_i2           2 }
                rgParamBindInfo[i].pwszDataSourceType := dstSmallint;
              varInteger:  { vt_i4           3 }
                rgParamBindInfo[i].pwszDataSourceType := dstInt;
              varSingle:   { vt_r4           4 }
                rgParamBindInfo[i].pwszDataSourceType := dstReal;
              varDouble:   { vt_r8           5 }
                rgParamBindInfo[i].pwszDataSourceType := dstFloat;
              varCurrency: { vt_cy           6 }
                rgParamBindInfo[i].pwszDataSourceType := dstMoney;
              varDate:     { vt_date         7 }
                rgParamBindInfo[i].pwszDataSourceType := dstDatetime;
              varOleStr{$IFDEF VER12P}, varUString{$ENDIF}: { vt_bstr         8 }
                rgParamBindInfo[i].pwszDataSourceType := dstNVarchar;
            {$IFNDEF CLR}
              varString:
                rgParamBindInfo[i].pwszDataSourceType := dstVarchar;
            {$ENDIF}
              varBoolean:  { vt_bool        11 }
                rgParamBindInfo[i].pwszDataSourceType := dstBit;
            {$IFDEF VER6P}
              varShortInt:
                rgParamBindInfo[i].pwszDataSourceType := dstTinyint;
              varWord:
                rgParamBindInfo[i].pwszDataSourceType := dstInt;
              varInt64:    { vt_i8          20 }
                rgParamBindInfo[i].pwszDataSourceType := dstBigint;
            {$ENDIF}
              varByte:     { vt_ui1         17 }
                rgParamBindInfo[i].pwszDataSourceType := dstSmallint;
              varLongWord: { vt_ui4         19 }
                rgParamBindInfo[i].pwszDataSourceType := dstBigint;
              else
                rgParamBindInfo[i].pwszDataSourceType := dstSql_variant;
            end;
          end;

          dtString, dtExtString:
            if IsOutputLOB(ParamDesc, FConnection.DBMSPrimaryVer, FConnection.ProviderPrimaryVer) then
              rgParamBindInfo[i].pwszDataSourceType := dstVarcharMax
            else
              rgParamBindInfo[i].pwszDataSourceType := dstVarchar;

          dtWideString, dtExtWideString:
            if IsOutputLOB(ParamDesc, FConnection.DBMSPrimaryVer, FConnection.ProviderPrimaryVer) then
              rgParamBindInfo[i].pwszDataSourceType := dstNVarcharMax
            else
              rgParamBindInfo[i].pwszDataSourceType := dstNVarchar;

          dtInt8:
            rgParamBindInfo[i].pwszDataSourceType := dstTinyint;
          dtInt16:
            rgParamBindInfo[i].pwszDataSourceType := dstSmallint;
          dtInt32, dtUInt16:
            rgParamBindInfo[i].pwszDataSourceType := dstInt;
          dtInt64, dtUInt32:
            rgParamBindInfo[i].pwszDataSourceType := dstBigint;
          dtFloat:
            rgParamBindInfo[i].pwszDataSourceType := dstFloat;
          dtDate, dtTime, dtDateTime{$IFDEF VER6P}{$IFNDEF FPC}, dtSQLTimeStamp{$ENDIF}{$ENDIF}:
            rgParamBindInfo[i].pwszDataSourceType := dstDatetime;
          dtBoolean:
            rgParamBindInfo[i].pwszDataSourceType := dstBit;
          dtCurrency:
            rgParamBindInfo[i].pwszDataSourceType := dstMoney;
          dtBlob:
            if ParamDesc.GetSubDataType = dtMSUDT then
              rgParamBindInfo[i].pwszDataSourceType := dstVarBinary
            else
              rgParamBindInfo[i].pwszDataSourceType := dstImage;
          dtMemo, dtWideMemo: begin
          {$IFDEF CLR}
            IsUnicode := VarIsStr(ParamDesc.Value);
            if not IsUnicode
              and (ParamDesc.Value <> nil)
              and (ParamDesc.Value is TBlob) then
              IsUnicode := TBlob(ParamDesc.Value).IsUnicode;
          {$ELSE}
            case VarType(ParamDesc.Value) of
              varOleStr{$IFDEF VER12P}, varUString{$ENDIF}:
                IsUnicode := True;
            else
              IsUnicode := False;
            end;
            if not IsUnicode
              and (VarType(ParamDesc.Value) = varByRef{$IFDEF FPC} or varVariant{$ENDIF})
              and (TVarData(ParamDesc.Value).VPointer <> nil) then begin
              // Assert(TObject(TVarData(ParamDesc.Value).VPointer) is TBlob); - trial
              IsUnicode := TBlob(TVarData(ParamDesc.Value).VPointer).IsUnicode;
            end;
          {$ENDIF}

            if IsUnicode then
              rgParamBindInfo[i].pwszDataSourceType := dstNVarchar
            else
              rgParamBindInfo[i].pwszDataSourceType := dstVarchar;
          end;
          dtMSXML:
            rgParamBindInfo[i].pwszDataSourceType := dstXML;

        {$IFDEF VER5P}
          dtVariant:
            rgParamBindInfo[i].pwszDataSourceType := dstSql_variant;
        {$ENDIF}
          dtBytes:
            rgParamBindInfo[i].pwszDataSourceType := dstBinary;
          dtVarBytes, dtExtVarBytes:
            rgParamBindInfo[i].pwszDataSourceType := dstVarbinary;

        {$IFDEF VER6P}
        {$IFNDEF FPC}
          dtFMTBCD,
        {$ENDIF}
        {$ENDIF}
          dtBCD:
            rgParamBindInfo[i].pwszDataSourceType := dstMoney;

          dtGuid:
            rgParamBindInfo[i].pwszDataSourceType := dstGuid;

          dtTable: begin
            rgParamBindInfo[i].pwszDataSourceType := dstTable;
            TableObject := ParamDesc.GetObject as TMSTableObject;
            if (TableObject <> nil) and not TableObject.GetIsNull then begin
              FieldsArray := null;
              for j := 0 to TableObject.FRecordSet.Fields.Count - 1 do begin
                if TableObject.FRecordSet.Fields[j].ReadOnly then begin
                  if VarIsNull(FieldsArray) then
                    FieldsArray := VarArrayCreate([0, 0], VT_UI2)
                  else
                  {$IFNDEF CLR}
                    VarArrayRedim(FieldsArray, VarArrayHighBound(FieldsArray, 1) + 1);
                  {$ELSE}
                    FieldsArray := VarArrayCreate([0, VarArrayHighBound(FieldsArray, 1) + 1], VT_UI2);
                  {$ENDIF}
                  FieldsArray[VarArrayHighBound(FieldsArray, 1)] := TableObject.FRecordSet.Fields[j].FieldNo;
                end;
              end;

              if not VarIsNull(FieldsArray) then begin
                Inc(SSParamsLength);
                SetLength(DBProp, SSParamsLength);
                DBProp[SSParamsLength - 1].dwPropertyID := SSPROP_PARAM_TABLE_DEFAULT_COLUMNS;
                DBProp[SSParamsLength - 1].dwOptions := DBPROPOPTIONS_REQUIRED;
                DBProp[SSParamsLength - 1].dwStatus := 0;
                DBProp[SSParamsLength - 1].colid := DB_NULLID;
                DBProp[SSParamsLength - 1].vValue := FieldsArray;

                SetLength(DBPropSet, SSParamsLength);
                DBPropSet[SSParamsLength - 1].cProperties := 1;
                DBPropSet[SSParamsLength - 1].guidPropertySet := DBPROPSET_SQLSERVERPARAMETER;

                SetLength(SSParamProps, SSParamsLength);
                SSParamProps[SSParamsLength - 1].iOrdinal := i + 1;
                SSParamProps[SSParamsLength - 1].cPropertySets := 1;
              end;
            end;
          end;

          else
            Assert(False, Format('Unknown datatype for param %s[%d] = %X', [ParamDesc.GetName, i, ParamDesc.GetDataType]));
        end;

        case ParamDesc.GetDataType of
          dtString, dtExtString, dtBytes, dtVarBytes, dtExtVarBytes:
            if ParamDesc.GetSize > 0 then
              rgParamBindInfo[i].ulParamSize := ParamDesc.GetSize
            else
              rgParamBindInfo[i].ulParamSize := MaxNonBlobFieldLen;
          dtWideString, dtExtWideString:
            if ParamDesc.GetSize > 0 then
              rgParamBindInfo[i].ulParamSize := ParamDesc.GetSize
            else
              rgParamBindInfo[i].ulParamSize := MaxNonBlobFieldLen div SizeOf(WideChar);
          dtDate, dtTime: begin
            rgParamBindInfo[i].ulParamSize := SizeOf(TDateTime);
            rgParamBindInfo[i].bScale := 3;
          end;
          dtDatetime{$IFDEF VER6P}{$IFNDEF FPC}, dtSQLTimeStamp{$ENDIF}{$ENDIF}: begin
            rgParamBindInfo[i].ulParamSize := SizeOf(TDBTimeStamp);
            rgParamBindInfo[i].bScale := 3;
          end;
        else
          rgParamBindInfo[i].ulParamSize := $FFFFFFFF;
        end;
        case ParamDesc.GetParamType of
          pdInput:
            rgParamBindInfo[i].dwFlags := DBPARAMFLAGS_ISINPUT + DBPARAMFLAGS_ISNULLABLE;
          pdOutput, pdResult:
            rgParamBindInfo[i].dwFlags := DBPARAMFLAGS_ISOUTPUT + DBPARAMFLAGS_ISNULLABLE;
          pdUnknown, pdInputOutput:
            rgParamBindInfo[i].dwFlags := DBPARAMFLAGS_ISINPUT + DBPARAMFLAGS_ISOUTPUT + DBPARAMFLAGS_ISNULLABLE;
        end;

        if (FConnection.FProvider = prCompact) and (ParamDesc.GetName <> '') then begin
          ParamName := ParamDesc.GetName;
          rgParamBindInfo[i].pwszName := Marshal.AllocHGlobal(Length(ParamName) * SizeOf(WideChar) + SizeOf(WideChar));
          CopyBuffer(Marshal.StringToHGlobalUni(WideString(ParamName)), rgParamBindInfo[i].pwszName, Length(ParamName) * SizeOf(WideChar) + SizeOf(WideChar));
        end
        else
          rgParamBindInfo[i].pwszName := nil;

        {OFS(ParamDesc.GetName);
        OFS('  ' + Marshal.PtrToStringUni(rgParamBindInfo[i].pwszDataSourceType));
        OFS('  ulParamSize = ' + IntToStr(rgParamBindInfo[i].ulParamSize));
        OFS('  dwFlags = ' + IntToStr(rgParamBindInfo[i].dwFlags));
        OFS('  bPrecision = ' + IntToStr(rgParamBindInfo[i].bPrecision));
        OFS('  bScale = ' + IntToStr(rgParamBindInfo[i].bScale));//}
      end;
    {$IFDEF CLR}
      try
        rgParamOrdinalsGC := GCHandle.Alloc(rgParamOrdinals, GCHandleType.Pinned);
        prgParamOrdinals := Marshal.UnsafeAddrOfPinnedArrayElement(rgParamOrdinals, 0);

        rgParamBindInfoGC := GCHandle.Alloc(rgParamBindInfo, GCHandleType.Pinned);
        prgParamBindInfo := Marshal.UnsafeAddrOfPinnedArrayElement(rgParamBindInfo, 0);
    {$ELSE}
      prgParamOrdinals := @rgParamOrdinals[0];
      prgParamBindInfo := @rgParamBindInfo[0];
    {$ENDIF}

      {OFS('prgParamOrdinals');
      OFS(prgParamOrdinals, ParamCount * 4, OFSFileName);
      OFS('prgParamBindInfo');
      OFS(prgParamBindInfo, ParamCount * sizeof(rgParamBindInfo[0]), OFSFileName);//}
      hr := CommandWithParameters.SetParameterInfo(ParamCount, prgParamOrdinals, prgParamBindInfo);
    {$IFDEF CLR}
      finally
        if IntPtr(rgParamOrdinalsGC) <> nil then
          rgParamOrdinalsGC.Free;
        if IntPtr(rgParamBindInfoGC) <> nil then
          rgParamBindInfoGC.Free;
      end;
    {$ENDIF}
    finally
      if FConnection.FProvider = prCompact then
        for i := 0 to ParamCount - 1 do begin
          if rgParamBindInfo[i].pwszName <> nil then
            Marshal.FreeHGlobal(rgParamBindInfo[i].pwszName);
          rgParamBindInfo[i].pwszName := nil;
        end;
    end;

    if hr <> DB_S_TYPEINFOOVERRIDDEN then
      Check(hr);

    if SSParamsLength > 0 then begin
    {$IFDEF CLR}
      try
        DBPropGC := GCHandle.Alloc(DBProp, GCHandleType.Pinned);
        DBPropSetGC := GCHandle.Alloc(DBPropSet, GCHandleType.Pinned);
        SSParamPropsGC := GCHandle.Alloc(SSParamProps, GCHandleType.Pinned);
        pSSParamProps := Marshal.UnsafeAddrOfPinnedArrayElement(SSParamProps, 0);
    {$ELSE}
      pSSParamProps := @SSParamProps[0];
    {$ENDIF}

      for i := 0 to SSParamsLength - 1 do begin
      {$IFDEF CLR}
        pDBProp := Marshal.UnsafeAddrOfPinnedArrayElement(DBProp, i);
        pDBPropSet := Marshal.UnsafeAddrOfPinnedArrayElement(DBPropSet, i);
      {$ELSE}
        pDBProp := @DBProp[i];
        pDBPropSet := @DBPropSet[i];
      {$ENDIF}

        DBPropSet[i].rgProperties := pDBProp;
        SSParamProps[i].rgPropertySets := pDBPropSet;
      end;

      QueryIntf(FICommandText, {$IFDEF CLR}ISSCommandWithParameters{$ELSE}IID_ISSCommandWithParameters{$ENDIF}, SSCommandWithParameters);
      Check(SSCommandWithParameters.SetParameterProperties(SSParamsLength, pSSParamProps));
    {$IFDEF CLR}
      finally
        if IntPtr(DBPropGC) <> nil then
          DBPropGC.Free;
        if IntPtr(DBPropSetGC) <> nil then
          DBPropSetGC.Free;
        if IntPtr(SSParamPropsGC) <> nil then
          SSParamPropsGC.Free;
      end;
    {$ENDIF}
    end;
  end;
end;

procedure TOLEDBCommand.GetParameterInfo;
var
  CommandWithParameters: ICommandWithParameters;
  cParams: NativeUInt;
  prgParamInfo, prgParamInfoEl: PDBPARAMINFO;
  ppNamesBuffer: PPOleStr;
  ParamDesc: TParamDesc;
  rgParamInfo: {$IFDEF CLR}TDBPARAMINFO{$ELSE}PDBPARAMINFO{$ENDIF};
  DataType: word;
  IsLong: Boolean;
  i: Integer;
begin
  QueryIntf(FICommandText, {$IFDEF CLR}ICommandWithParameters{$ELSE}IID_ICommandWithParameters{$ENDIF}, CommandWithParameters);

  ppNamesBuffer := nil;
  try
    Check(CommandWithParameters.GetParameterInfo(cParams, prgParamInfo, ppNamesBuffer));
    prgParamInfoEl := prgParamInfo;

    for i := 0 to cParams - 1 do begin
      if i >= FParams.Count then
        break;

      ParamDesc := FParams[i];
    {$IFDEF CLR}
      Assert(prgParamInfoEl <> nil);
      rgParamInfo := DBPARAMINFO(Marshal.PtrToStructure(prgParamInfoEl, TypeOf(DBPARAMINFO)));
    {$ELSE}
      rgParamInfo := prgParamInfoEl;
    {$ENDIF}

      case rgParamInfo.dwFlags and (DBPARAMFLAGS_ISINPUT + DBPARAMFLAGS_ISOUTPUT) of
        DBPARAMFLAGS_ISINPUT + DBPARAMFLAGS_ISOUTPUT:
          ParamDesc.SetParamType(pdInputOutput);
        DBPARAMFLAGS_ISINPUT:
          ParamDesc.SetParamType(pdInput);
        DBPARAMFLAGS_ISOUTPUT:
          ParamDesc.SetParamType(pdOutput);
      end;

      IsLong := (rgParamInfo.dwFlags and DBPARAMFLAGS_ISLONG) <> 0;
      if ConvertOLEDBTypeToInternalFormat(rgParamInfo.wType, IsLong,
           CalcEnableBCD, CalcEnableFMTBCD, False, 0, 0,
           True, GetWideMemos, True, DataType, FConnection.DBMSPrimaryVer) then
      begin
        ParamDesc.SetDataType(DataType);
        if rgParamInfo.ulParamSize <> $FFFFFFFF then
          ParamDesc.SetSize(rgParamInfo.ulParamSize);

        case DataType of
          dtString, dtWideString, dtMemo, dtWideMemo, dtMSXML: begin
            if IsLong then
              ParamDesc.SetSubDataType(dtText); // This is a MS SQL 'text' or 'ntext' field
            if (rgParamInfo.wType = DBTYPE_WSTR) or (rgParamInfo.wType = DBTYPE_XML) then
              ParamDesc.SetSubDataType(ParamDesc.GetSubDataType or dtWide);
          end;
          dtBytes, dtVarBytes, dtBlob: begin
            if rgParamInfo.wType = DBTYPE_UDT then
              ParamDesc.SetSubDataType(dtMSUDT);
          end;
        end;

        prgParamInfoEl := PtrOffset(prgParamInfoEl, {$IFDEF CLR}Marshal.SizeOf(TypeOf(DBPARAMINFO)){$ELSE}sizeof(DBPARAMINFO){$ENDIF});
      end;
    end;
  finally
    FreeCoMem(prgParamInfo);
    FreeCoMem(ppNamesBuffer);
  end;
end;

procedure TOLEDBCommand.Prepare;
begin
  if GetPrepared then
    Exit;

  QueryInterfaces(True);
  try
    FRPCCall := FIsSProc and __UseRPCCallStyle;
    SetCommandProp;
    if FRPCCall or (not FUseDescribeParams and not ParamsInfoOldBehavior) then
      SetParameterInfo;

    Check(FICommandPrepare.Prepare(0)); // If statement is wrong in some cases exception may be occured in other place
  {$IFDEF AUTOTEST}
    Inc(__ServerPrepareCount);
  {$ENDIF}
    if FUseDescribeParams then
      GetParameterInfo;

    inherited;
    FPrepared := True;
  except
    ReleaseInterfaces;
    raise;
  end;
end;

procedure TOLEDBCommand.Unprepare;
{$IFDEF CLR}
var
  p: ICommandPrepare;
{$ENDIF}
begin
  if GetPrepared then begin
    FIUnknown := nil;
    RequestParamsIfPossible;

    if (FICommandPrepare <> nil) and not FRPCCall then
      Check(FICommandPrepare.UnPrepare);
    inherited;
    FPrepared := False;
    FRPCCall := False;

  {$IFDEF CLR}
    p := FICommandPrepare;
  {$ENDIF}
    ReleaseInterfaces;
  {$IFDEF CLR}
    if p <> nil then
      Marshal.ReleaseComObject(p);
  {$ENDIF}
  end;
end;

function TOLEDBCommand.GetPrepared: boolean;
begin
  Result := FPrepared;
end;

procedure TOLEDBCommand.SetOutputStream(Stream: TStream);
begin
  if Stream <> nil then
    FOLEDBStream := TOLEDBStream.Create(Stream, nil)
  else
  if FOLEDBStream <> nil then begin
  {$IFDEF CLR}
    FOLEDBStream.Free;
  {$ELSE}
    FOLEDBStream._Release;
  {$ENDIF}
    FOLEDBStream := nil;
  end;
end;

function TOLEDBCommand.GetUseOutputStream: boolean;
begin
  Result := FOLEDBStream <> nil;
end;

{$IFNDEF VER6P}
type
  _TParamDesc = class (TParamDesc);
{$ENDIF}

function NeedOptimizationByRefParameter(ParamDesc: TParamDesc; ServerVersion, ClientVersion: integer): boolean;
begin
  // Optimization for input-only parameters, store by ref
  Result := (((ParamDesc.GetParamType = pdInput) and
    (ParamDesc.GetDataType in CharsByRef + BytesByRef) {$IFDEF CLR}and (ParamDesc.GetDataType <> dtString){$ENDIF})
    or (ParamDesc.GetDataType in [dtBlob, dtMemo, dtWideMemo]))
    and not IsCompactEdition(ServerVersion)
    and not IsOutputLOB(ParamDesc, ServerVersion, ClientVersion);
end;

procedure FillBindingForParam(Ordinal: integer; ParamDesc: TOLEDBParamDesc; Connection: TOLEDBConnection;
  var pBind: TDBBinding; var BindMemorySize: NativeUInt; const ValueAvaliable: boolean; const IsWide: boolean);

  procedure SetBindDefaults;
  begin
    pBind.dwPart := DBPART_STATUS or DBPART_LENGTH or DBPART_VALUE; // WAR Length is not always need and may be removed in some cases
  {$IFNDEF CLR}
    pointer(pBind.pTypeInfo) := nil;
  {$ENDIF}
    pBind.pBindExt := nil;
    pBind.dwMemOwner := DBMEMOWNER_CLIENTOWNED;
    pBind.dwFlags := 0;
    pBind.bPrecision := 11;
    pBind.bScale := 0;
  end;

  procedure SetBindData;
    function GetMaxLen: NativeInt; // Also correct pBind.wType. Must be called AFTER ConvertInternalTypeToOLEDB
    var
      IsUnicode: boolean;
      DataType: word;
    begin
      DataType := ParamDesc.GetDataType;
      case DataType of
        dtUnknown:
          Result := SizeOf_OleVariant;
        dtString:
          if not ValueAvaliable or (ParamDesc.GetSize = 0) then
            Result := MaxNonBlobFieldLen
          else
            Result := ParamDesc.GetSize + 1{#0};
        dtWideString:
          if not ValueAvaliable or (ParamDesc.GetSize = 0) then
            Result := MaxNonBlobFieldLen
          else
            Result := (ParamDesc.GetSize + 1{#0}) * sizeof(WideChar);
        dtBytes, dtVarBytes:
          Result := ParamDesc.GetSize;
        dtInt8:
          Result := sizeof(Byte);
        dtInt16, dtWord:
          if Connection.FProvider = prCompact then begin
            if (ParamDesc.GetSubDataType = dtUInt8) then begin
              Result := sizeof(Byte);
              pBind.wType := DBTYPE_UI1;
            end
            else
              Result := sizeof(Word);
          end
          else
            Result := sizeof(Word);
        dtInt32:
          Result := sizeof(DWORD);
        dtFloat:
          if Connection.FProvider = prCompact then begin
            if (ParamDesc.GetSubDataType = dtSingle) then begin
              Result := sizeof(Single);
              pBind.wType := DBTYPE_R4;
            end
            else
              if (ParamDesc.GetSubDataType = dtNumeric) then begin
                Result := SizeOfTDBNumeric;
                pBind.wType := DBTYPE_NUMERIC;
              end
              else
                Result := sizeof(Double);
          end
          else
            Result := sizeof(Double);
        dtCurrency:
          Result := sizeof(Double);
          // Result := sizeof(Currency); Currency type cannot be used over TCurrencyField uses double to store
        dtDate:
          Result := sizeof(TDateTime);
        dtTime: begin
          if (Connection.DBMSPrimaryVer >= 10) and (Connection.ProviderPrimaryVer >= 10) then begin
            Result := sizeof(TDBTime);
            pBind.wType := DBTYPE_DBTIME;
          end
          else
            Result := sizeof(TDateTime);
        end;
        dtDateTime{$IFDEF VER6P}{$IFNDEF FPC}, dtSQLTimeStamp{$ENDIF}{$ENDIF}: begin
          Result := sizeof(TDBTimeStamp);
          pBind.wType := DBTYPE_DBTIMESTAMP;
        end;
        dtBoolean:
          Result := sizeof(WordBool);
        dtInt64:
          Result := sizeof(Int64);
        dtMSXML:
          Result := SizeOf_OleVariant;
        dtBlob, dtMemo, dtWideMemo:
          if Connection.FProvider <> prCompact then
            if IsOutputLOB(ParamDesc, Connection.DBMSPrimaryVer, Connection.ProviderPrimaryVer) then
              Result := sizeof(TOLEDBStream) // varchar(max)
            else
              Result := 0 // Only Input, store ByRef
          else begin
            if ((DataType = dtMemo) or (DataType = dtWideMemo)) {$IFDEF LITE} and ((not dtWide) and ParamDesc.GetSubDataType = dtMSXML){$ENDIF} then begin
              Result := TBlob(ParamDesc.GetObject).Size + 2{#0#0};

              if ParamDesc.GetSubDataType and dtWide = 0 then
                pBind.wType := DBTYPE_STR
              else
                pBind.wType := DBTYPE_WSTR;
            end
            else
              Result := sizeof(TOLEDBStream);
          end;
      {$IFDEF VER5P}
        dtGuid:
          Result := sizeof(TGuid);
        dtVariant:
          Result := SizeOf_OleVariant;
      {$ENDIF}
        dtBCD:
          if Connection.FProvider <> prCompact then
            Result := sizeof(Currency)
          else begin
            Result := SizeOfTDBNumeric;
            pBind.wType := DBTYPE_NUMERIC;
          end;
      {$IFDEF VER6P}
      {$IFNDEF FPC}
        dtFmtBCD:
          if Connection.FProvider <> prCompact then
            Result := SizeOfTBcd * 2
          else
            Result := SizeOfTDBNumeric;
      {$ENDIF}
      {$ENDIF}
        dtTable:
          Result := sizeof(TObject);
      else
        Result := 0;
      end;

      if (Connection.FProvider = prCompact) and IsLargeDataTypeUsed(ParamDesc) and ParamDesc.GetNull then begin
        Result := 0;
        case DataType of
          dtBlob:
            pBind.wType := DBTYPE_BYTES;
          dtMemo, dtWideMemo:
            pBind.wType := DBTYPE_WSTR;
          else
            Assert(False);
        end;
      end;

      if NeedOptimizationByRefParameter(ParamDesc, Connection.DBMSPrimaryVer, Connection.ProviderPrimaryVer) then begin
        // Optimization for input-only parameters, store by ref
        Result := SizeOf(IntPtr);
        if (DataType in BytesByRef) then // This is Bytes by Ref
          pBind.wType := DBTYPE_BYREF or DBTYPE_BYTES
        else // This is (Wide)String by Ref
        begin
        {$IFDEF CLR}
          if not ValueAvaliable then
            IsUnicode := (ParamDesc.GetDataType in [dtWideString, dtExtWideString]) or IsWide
          else
            IsUnicode := VarIsStr(ParamDesc.Value);
          if not IsUnicode
            and (ParamDesc.Value <> nil)
            and (ParamDesc.Value is TBlob) then
            IsUnicode := TBlob(ParamDesc.Value).IsUnicode;
        {$ELSE}
          if not ValueAvaliable then
            IsUnicode := (ParamDesc.GetDataType in [dtWideString, dtExtWideString]) or IsWide
          else
            case VarType(ParamDesc.Value) of
              varOleStr{$IFDEF VER12P}, varUString{$ENDIF}:
                IsUnicode := True;
            else
              IsUnicode := False;
            end;
          if not IsUnicode
            and (VarType(ParamDesc.Value) = varByRef{$IFDEF FPC} or varVariant{$ENDIF})
            and (TVarData(ParamDesc.Value).VPointer <> nil) then begin
            // Assert(TObject(TVarData(ParamDesc.Value).VPointer) is TBlob); - trial
            IsUnicode := TBlob(TVarData(ParamDesc.Value).VPointer).IsUnicode;
          end;
        {$ENDIF}

          if IsUnicode then // WideString
            pBind.wType := DBTYPE_BYREF or DBTYPE_WSTR
          else // (VType = varString) String
            pBind.wType := DBTYPE_BYREF or DBTYPE_STR;
        end;
      end
      else
        if (ParamDesc.GetParamType in [pdOutput, pdInputOutput]) // Truncate params, if too long
          and (DataType in [dtString, dtWideString, dtBytes, dtVarBytes]) then begin
          if not ((Connection.DBMSPrimaryVer >= 9) and (Connection.ProviderPrimaryVer >= 9) and (Result > MaxNonBlobFieldLen)) then
            if DataType = dtString then
              Result := MaxNonBlobFieldLen + 1
            else
              Result := MaxNonBlobFieldLen;
        end;

      Assert(Result >= 0);
      // Assert(Result <= MaxNonBlobFieldLen + 1);
    end;

  var
    ServerVersion: integer;
    Obj: {$IFNDEF UNIDACPRO}OLEDBIntf{$ELSE}OLEDBIntfUni{$ENDIF}.DBOBJECT;
  begin
{    if pBind.iOrdinal = 1 then
      OFS('==========================================');
    OFS('--------------------------');
    OFS('ParamDesc.GetName ' + ParamDesc.GetName);
    OFS('VarType(ParamDesc.Value) = ' + IntToStr(VarType(ParamDesc.Value)));}

    pBind.iOrdinal := Ordinal;
    pBind.eParamIO := ConvertCRParamTypeToOLEDB(ParamDesc.GetParamType);
    if IsLargeDataTypeUsed(ParamDesc) and // "text", "ntext", "image"
      (pBind.eParamIO in [DBPARAMIO_OUTPUT, DBPARAMIO_INPUT + DBPARAMIO_OUTPUT]) then
      if not IsOutputLOB(ParamDesc, Connection.DBMSPrimaryVer, Connection.ProviderPrimaryVer) then // "varchar(max)"
        DatabaseErrorFmt(SBadOutputParam, [ParamDesc.GetName]);

    // int64 parameters conversion
    if ((Connection.ProviderPrimaryVer < 8) and not IsWindowsVista and OldProvidersSupport)
       and (Connection.FProvider <> prCompact) and (ParamDesc.GetDataType = dtInt64) then
      ParamDesc.SetDataType(dtFloat);

    // currency parameters conversion
    if (ParamDesc.GetDataType = dtCurrency) and (VarType(ParamDesc.Value) = varCurrency) then
      ParamDesc.SetDataType(dtBCD); // To prevent SQL Server exception on setting float value to smallmoney parameter

    if Connection <> nil then
      ServerVersion := Connection.DBMSPrimaryVer
    else
      ServerVersion := 0;
    pBind.wType := ConvertInternalTypeToOLEDB(ParamDesc.GetDataType, True, ServerVersion);

    pBind.obStatus := BindMemorySize;
    pBind.obLength := pBind.obStatus + sizeof(DBSTATUS);
    pBind.obValue := pBind.obLength + sizeof(DBLENGTH);

    pBind.cbMaxLen := GetMaxLen; // Also correct pBind.wType. Must be called AFTER ConvertInternalTypeToOLEDB
    if not ({IsCompactEdition(ServerVersion) and} ((pBind.wType = DBTYPE_BYTES) or (pBind.wType = DBTYPE_WSTR))) then
      Assert(pBind.cbMaxLen > 0, Format('Unknown datatype for param %s[%d] = %X', [ParamDesc.GetName, Ordinal, ParamDesc.GetDataType]));
    BindMemorySize := pBind.obValue + pBind.cbMaxLen;

    if pBind.wType = DBTYPE_IUNKNOWN then begin
      Obj.iid := IID_ISequentialStream;
      if pBind.eParamIO = DBPARAMIO_INPUT then
        Obj.dwFlags := STGM_READ
      else
        Obj.dwFlags := STGM_READWRITE;
      pBind.pObject := Marshal.AllocHGlobal(sizeof(DBOBJECT));
    {$IFDEF CLR}
      Marshal.StructureToPtr(TObject(Obj), pBind.pObject, False);
    {$ELSE}
      DBOBJECT(pBind.pObject^) := Obj;
    {$ENDIF}
    end;
    if pBind.wType = DBTYPE_TABLE then begin
      Obj.iid := IID_IRowset;
      Obj.dwFlags := 0;
      pBind.pObject := Marshal.AllocHGlobal(sizeof(DBOBJECT));
    {$IFDEF CLR}
      Marshal.StructureToPtr(TObject(Obj), pBind.pObject, False);
    {$ELSE}
      DBOBJECT(pBind.pObject^) := Obj;
    {$ENDIF}
    end;

    {OFS('iOrdinal = ' + IntToStr(pBind.iOrdinal));
    OFS('obValue = ' + IntToStr(pBind.obValue));
    OFS('obLength = ' + IntToStr(pBind.obLength));
    OFS('obStatus = ' + IntToStr(pBind.obStatus));
    OFS('pTypeInfo = ' + IntToStr(Integer(pBind.pTypeInfo)));
    OFS('pObject = ' + IntToStr(Integer(pBind.pObject)));
    OFS('pBindExt = ' + IntToStr(Integer(pBind.pBindExt)));
    OFS('dwPart = ' + IntToStr(pBind.dwPart));
    OFS('dwMemOwner = ' + IntToStr(pBind.dwMemOwner));
    OFS('eParamIO = ' + IntToStr(pBind.eParamIO));
    OFS('cbMaxLen = ' + IntToStr(pBind.cbMaxLen));
    OFS('dwFlags = ' + IntToStr(pBind.dwFlags));
    OFS('wType = ' + IntToStr(pBind.wType));
    OFS('bPrecision = ' + IntToStr(pBind.bPrecision));
    OFS('bScale = ' + IntToStr(pBind.bScale));}
  end;

begin
  SetBindDefaults;
  SetBindData;
end;

procedure SaveParamValue(const ParamDesc: TParamDesc; const pBind: TDBBinding;
  var ParamsAccessorData: TParamsAccessorData{$IFDEF HAVE_COMPRESS};
  const CompressBlobMode: TCompressBlobMode{$ENDIF}
  {$IFDEF CLR}; var ParamsGC: TIntPtrDynArray{$ENDIF}; ServerVersion, ClientVersion: integer);
var
  pValue, pLength, pStatus: IntPtr;
  // pParamData: PVarData;
  ParamVarType: TVarType;
  ParamVarPtr: IntPtr;

  s: AnsiString;
  l: UINT;
  ws: WideString;

  c: Currency;
  i64: Int64;

  Blob: TBlob;
{$IFDEF CLR}
  d: double;
  b: TBytes;
{$ENDIF}
{$IFDEF VER6P}
  DotPos: integer;
  Bcd: TBcd;
{$ENDIF}
{$IFDEF HAVE_COMPRESS}
  Compress: boolean;
{$ENDIF}
  dt: TDateTime;
  AYear, AMonth, ADay, AHour, AMinute, ASecond, AMilliSecond: Word;
  DBTimeStamp: TDBTimeStamp;
{$IFDEF VER6P}{$IFNDEF FPC}
  SQLTimeStamp: TSQLTimeStamp;
{$ENDIF}{$ENDIF}
  DBNumeric: TDBNumeric;
  Stream: IntPtr;
  OLEDBStream: TOLEDBStream;
  DBTime: TDBTime;
  Rowset: IRowset;
begin
  {OFS('-------');
  OFS('[' + IntToStr(pBind.iOrdinal) + ']');
  OFS('pBind.eParamIO=' + IntToStr(pBind.eParamIO));
  OFS('pBind.obValue=' + IntToStr(pBind.obValue));
  OFS('pBind.obLength=' + IntToStr(pBind.obLength));
  OFS('pBind.obStatus=' + IntToStr(pBind.obStatus));
  OFS('pBind.wType=' + IntToStr(pBind.wType));
  OFS('pBind.dwFlags=' + IntToStr(pBind.dwFlags));
  OFS('pBind.bPrecision=' + IntToStr(pBind.bPrecision));
  OFS('pBind.bScale=' + IntToStr(pBind.bScale));
  OFS('pBind.cbMaxLen=' + IntToStr(pBind.cbMaxLen));
  OFS('ParamDesc.GetNull=' + BoolToStr(ParamDesc.GetNull, True));
  OFS('ParamDesc.GetParamType=' + IntToStr(Integer(ParamDesc.GetParamType)));
  OFS('ParamDesc.GetDataType=' + IntToStr(ParamDesc.GetDataType));}
  if pBind.eParamIO in [DBPARAMIO_INPUT, DBPARAMIO_INPUT + DBPARAMIO_OUTPUT] then begin
    pStatus := PtrOffset(ParamsAccessorData.ExecuteParams.pData, pBind.obStatus);
    pValue := PtrOffset(ParamsAccessorData.ExecuteParams.pData, pBind.obValue); // Destination

    if not TOLEDBParamDesc(ParamDesc).GetIsBound then begin
      FillChar(pValue, pBind.cbMaxLen, 0);
      Marshal.WriteInt32(pStatus, Integer(DBSTATUS_S_DEFAULT));
    end
    else
    if ParamDesc.GetNull  then begin
      FillChar(pValue, pBind.cbMaxLen, 0);
      Marshal.WriteInt32(pStatus, Integer(DBSTATUS_S_ISNULL));
    end
    else begin
      Marshal.WriteInt32(pStatus, Integer(DBSTATUS_S_OK));

      pLength := PtrOffset(ParamsAccessorData.ExecuteParams.pData, pBind.obLength);
      ParamVarType := VarType(ParamDesc.Value);
      // pParamData := @TVarData(ParamDesc.Value); // Source

      if NeedOptimizationByRefParameter(ParamDesc, ServerVersion, ClientVersion) then begin
        // Optimization for input-only parameters, store by ref
        if ParamVarType = varArray + varByte then begin
        {$IFDEF CLR}
          l := Length(ParamsGC);
          SetLength(ParamsGC, l + 1);
          ParamsGC[l] := AllocGCHandle(ParamDesc.Value, True);
          ParamVarPtr := GetAddrOfPinnedObject(ParamsGC[l]);
          l := VarArrayHighBound(ParamDesc.Value, 1) - VarArrayLowBound(ParamDesc.Value, 1) + 1;
        {$ELSE}
          ParamVarPtr := TVarData(ParamDesc.Value).VArray.Data;
          l := TVarData(ParamDesc.Value).VArray.Bounds[0].ElementCount;
        {$ENDIF}
          Marshal.WriteIntPtr(pValue, ParamVarPtr);
          Marshal.WriteInt32(pLength, l);
        end
        else
      {$IFDEF CLR}
        if ParamDesc.Value is TBlob then begin
          Assert(ParamDesc.Value <> nil);
          Blob := TBlob(ParamDesc.Value);
      {$ELSE}
        if ParamVarType = varByRef{$IFDEF FPC} or varVariant{$ENDIF} then begin
          Assert(TVarData(ParamDesc.Value).VPointer <> nil);
          // Assert(TObject(TVarData(ParamDesc.Value).VPointer) is TBlob); - trial
          Blob := TVarData(ParamDesc.Value).VPointer;
      {$ENDIF}
          Blob.Defrag;
        {$IFDEF HAVE_COMPRESS}
          Compress := Blob is TCompressedBlob;
          if Compress then
            TCompressedBlob(Blob).Compressed := (CompressBlobMode = cbServer) or (CompressBlobMode = cbClientServer);
          if Compress and TCompressedBlob(Blob).Compressed then
            Marshal.WriteInt32(pLength, Integer(TCompressedBlob(Blob).CompressedSize))
          else
        {$ENDIF}
            Marshal.WriteInt32(pLength, Integer(Blob.Size));
          if IntPtr(Blob.FirstPiece) = nil then
            Marshal.WriteIntPtr(pValue, nil)
          else
            Marshal.WriteIntPtr(pValue, PtrOffset(Blob.FirstPiece, sizeof(TPieceHeader)));
        end
        else
        begin // CharsByRef Input parameter
        {$IFDEF CLR}
          l := Length(ParamsGC);
          SetLength(ParamsGC, l + 1);
          ParamsGC[l] := AllocGCHandle(ParamDesc.Value, True);
          ParamVarPtr := GetAddrOfPinnedObject(ParamsGC[l]);
        {$ELSE}
          ParamVarPtr := TVarData(ParamDesc.Value).VPointer;
          if (ParamVarPtr = nil) and (VarIsEmpty(ParamDesc.Value) or (ParamDesc.Value = '')) then
            if (ParamVarType = varOleStr) {$IFDEF VER12P} or (ParamVarType = varUString) {$ENDIF} then // WideString
              ParamVarPtr := PWideChar(EmptyWString)
            else
              ParamVarPtr := PAnsiChar(EmptyString);
        {$ENDIF}
          Marshal.WriteIntPtr(pValue, ParamVarPtr);
          if (ParamDesc.GetDataType in CharsByRef) then
            if ParamVarPtr <> nil then begin
            {$IFDEF CLR}
              l := Length(ParamDesc.Value) * sizeof(WideChar);
            {$ELSE}
              if (ParamVarType = varOleStr) {$IFDEF VER12P} or (ParamVarType = varUString) {$ENDIF} then // WideString
                l := Integer(StrLenW(ParamVarPtr) * sizeof(WideChar))
              else // Pascal string
              {$IFDEF VER12P}
                l := Length(AnsiString(ParamDesc.Value));
              {$ELSE}
                l := Length(ParamDesc.Value);
              {$ENDIF}
            {$ENDIF}
            end
            else
              l := 0
          else
            l := Integer(ParamDesc.GetSize);
          Marshal.WriteInt32(pLength, l);
        end;
      end
      else
        case ParamDesc.GetDataType of
          dtUnknown:
            SetOleVariant(pValue, ParamDesc.Value);
          dtString: begin
            s := AnsiString(ParamDesc.Value);
            l := Length(s);
            if l > pBind.cbMaxLen - 1{#0} then // Truncate too long values
              l := pBind.cbMaxLen - 1{#0};
            Marshal.WriteInt32(pLength, l);

            if l > 0 then
              CopyBufferAnsi(s, pValue, l + 1{#0});
          end;
          dtWideString: begin
            ws := ParamDesc.Value;
            l := Length(ws) * sizeof(WideChar);
            if l > (pBind.cbMaxLen - 1{#0}) * sizeof(WideChar) then // Truncate too long values
              l := (pBind.cbMaxLen - 1{#0}) * sizeof(WideChar);
            Marshal.WriteInt32(pLength, l);

            if l > 0 then
              CopyBufferUni(ws, pValue, l + 2{#0#0});
          end;
          dtBytes, dtVarBytes: begin
            case ParamVarType of
              varArray + varByte: begin
              {$IFDEF CLR}
                b := ParamDesc.Value;
                l :=  Length(b);
                if l > pBind.cbMaxLen then // Truncate too long values
                  l := pBind.cbMaxLen;
                Marshal.Copy(b, 0, pValue, l);
              {$ELSE}
                l :=  TVarData(ParamDesc.Value).VArray.Bounds[0].ElementCount;
                if l > pBind.cbMaxLen then // Truncate too long values
                  l := pBind.cbMaxLen;
                Move(TVarData(ParamDesc.Value).VArray.Data^, pValue^, l);
              {$ENDIF}
                Marshal.WriteInt32(pLength, l);
              end;
              varOleStr{$IFDEF VER12P}, varUString{$ENDIF}: begin
                s := AnsiString(ParamDesc.Value);
                l :=  Length(s);
                if l > pBind.cbMaxLen {without #0} then // Truncate too long values
                  l := pBind.cbMaxLen {without #0};
                Marshal.WriteInt32(pLength, l);

                if l > 0 then
                  CopyBufferAnsi(s, pValue, l);
              end;
              else
              {$IFDEF LITE}
                raise Exception.Create('Unknown BLOB field type (must be varArray + varByte, varOleStr)');
              {$ELSE}
                raise EDatabaseError.Create('Unknown BLOB field type (must be varArray + varByte, varOleStr)');
              {$ENDIF}
            end;
          end;
          dtInt8:
            Marshal.WriteInt16(pValue, Byte(ParamDesc.Value));
          dtInt16:
            Marshal.WriteInt16(pValue, SmallInt(ParamDesc.Value));
          dtWord:
            if IsCompactEdition(ServerVersion) and (pBind.wType = DBTYPE_UI1) then
              Marshal.WriteByte(pValue, Byte(ParamDesc.Value))
            else
              Marshal.WriteInt16(pValue, SmallInt(Word(ParamDesc.Value)));
          dtInt32:
            Marshal.WriteInt32(pValue, Integer(ParamDesc.Value));
          dtFloat:
            if IsCompactEdition(ServerVersion) then begin
              if (pBind.wType = DBTYPE_R4) then
                Marshal.WriteInt32(pValue, BitConverter.ToInt32(BitConverter.GetBytes(Single(ParamDesc.Value)), 0))
              else
                if (pBind.wType = DBTYPE_NUMERIC) then begin
                  DBNumeric := BcdToDBNumeric(DoubleToBcd(Double(ParamDesc.Value)));
                {$IFDEF CLR}
                  Marshal.StructureToPtr(TObject(DBNumeric), pValue, False);
                {$ELSE}
                  PDBNumeric(pValue)^ := DBNumeric;
                {$ENDIF}
                end
                else
                  Marshal.WriteInt64(pValue, BitConverter.DoubleToInt64Bits(Double(ParamDesc.Value)));
            end
            else
              Marshal.WriteInt64(pValue, BitConverter.DoubleToInt64Bits(Double(ParamDesc.Value)));
          dtCurrency: begin
            if IsCompactEdition(ServerVersion) and (pBind.wType = DBTYPE_CY) then begin
            {$IFDEF CLR}
              c := ParamDesc.Value;
              d := c;
              d := d * 10000;
              i64 := Convert.ToInt64(d);
              Marshal.WriteInt64(pValue, i64);
            {$ELSE}
              PCurrency(pValue)^ := ParamDesc.Value;
            {$ENDIF}
            end
            else
              Marshal.WriteInt64(pValue, BitConverter.DoubleToInt64Bits(Double(ParamDesc.Value)));
            // PCurrency(pValue)^ := ParamDesc.Value; Currency type cannot be used over TCurrencyField uses double to store
          end;
          dtDate:
            Marshal.WriteInt64(pValue, BitConverter.DoubleToInt64Bits(TDateTime(ParamDesc.Value)));
          dtTime: begin
            if (ServerVersion >= 10) and (ClientVersion >= 10) then begin
              dt := TDateTime(ParamDesc.Value);
              DecodeDateTime(dt, AYear, AMonth, ADay, AHour, AMinute, ASecond, AMilliSecond);
              DBTime.hour := AHour;
              DBTime.minute := AMinute;
              DBTime.second := ASecond;
              DBTime.fraction := AMilliSecond * 1000000 ; // milliseconds to billionths of a second
            {$IFDEF CLR}
              Marshal.StructureToPtr(TObject(DBTime), pValue, False);
            {$ELSE}
              PDBTime(pValue)^ := DBTime;
            {$ENDIF}
            end
            else
              Marshal.WriteInt64(pValue, BitConverter.DoubleToInt64Bits(TDateTime(ParamDesc.Value)));
          end;
          dtDateTime: begin
            dt := TDateTime(Double(ParamDesc.Value));
            DecodeDateTime(dt, AYear, AMonth, ADay, AHour, AMinute, ASecond, AMilliSecond);
            DBTimeStamp.year := AYear;
            DBTimeStamp.month := AMonth;
            DBTimeStamp.day := ADay;
            DBTimeStamp.hour := AHour;
            DBTimeStamp.minute := AMinute;
            DBTimeStamp.second := ASecond;
            DBTimeStamp.fraction := AMilliSecond * 1000000 ; // milliseconds to billionths of a second
          {$IFDEF CLR}
            Marshal.StructureToPtr(TObject(DBTimeStamp), pValue, False);
          {$ELSE}
            PDBTimeStamp(pValue)^ := DBTimeStamp;
          {$ENDIF}
          end;
        {$IFDEF VER6P}{$IFNDEF FPC}
          dtSQLTimeStamp: begin
            SQLTimeStamp := VarToSQLTimeStamp(ParamDesc.Value);
            SQLTimeStamp.Fractions := SQLTimeStamp.Fractions * 1000000 ; // milliseconds to billionths of a second
          {$IFDEF CLR}
            Marshal.StructureToPtr(TObject(SQLTimeStamp), pValue, False);
          {$ELSE}
            PSQLTimeStamp(pValue)^ := SQLTimeStamp;
          {$ENDIF}
          end;
        {$ENDIF}{$ENDIF}
          dtBoolean:
            Marshal.WriteInt16(pValue, SmallInt(WordBool(Boolean(ParamDesc.Value)))); // Convert to boolean is useful to bypass Delphi bug
          dtInt64: begin
          {$IFDEF VER6P}
            i64 := ParamDesc.Value; // Explicit Convert!
            Marshal.WriteInt64(pValue, i64);
          {$ELSE}
            if ParamVarType in [$000E, $0014] then
              PInt64(pValue)^ := PInt64(@TVarData(ParamDesc.Value).VInteger)^
            else
              PInt64(pValue)^ := TVarData(ParamDesc.Value).VInteger;
          {$ENDIF}
          end;
        {$IFDEF VER5P}
          dtGuid:
          {$IFDEF CLR}
            Marshal.StructureToPtr(TObject(StringToGUID(ParamDesc.Value)), pValue, False);
          {$ELSE}
            PGuid(pValue)^ := StringToGUID(ParamDesc.Value);
          {$ENDIF}
          dtVariant:
            SetOleVariant(pValue, ParamDesc.Value);
        {$ENDIF}
          dtBCD:
            if IsCompactEdition(ServerVersion) then begin
              Assert(pBind.wType = DBTYPE_NUMERIC);
              DBNumeric := DoubleToDBNumeric(ParamDesc.Value, 0, 0);
            {$IFDEF CLR}
              Marshal.StructureToPtr(TObject(DBNumeric), pValue, False);
            {$ELSE}
              PDBNumeric(pValue)^ := DBNumeric;
            {$ENDIF}
            end
            else begin
            {$IFNDEF VER6P}
              if ParamVarType in [$000E, $0014] then
                PCurrency(pValue)^ := PInt64(@TVarData(ParamDesc.Value).VInteger)^
              else
            {$ENDIF}
              begin
                c := ParamDesc.Value;
              {$IFDEF CLR}
                d := c;
                d := d * 10000;
                i64 := Convert.ToInt64(d);
              {$ELSE}
                i64 := PInt64(@c)^;
              {$ENDIF}
                Marshal.WriteInt64(pValue, i64);
              end;
            end;
        {$IFDEF VER6P}
        {$IFNDEF FPC}
          dtFmtBCD:
            if IsCompactEdition(ServerVersion) then begin
              Assert(pBind.wType = DBTYPE_NUMERIC);
              if VarIsFMTBcd(ParamDesc.Value) then
              //Assert(VarIsFMTBcd(ParamDesc.Value));
                DBNumeric := BcdToDBNumeric(VarToBcd(ParamDesc.Value))
              else begin
                s := AnsiString(ParamDesc.Value);
                if DecimalSeparator <> '.' then begin
                  DotPos := Pos(AnsiString(DecimalSeparator), s);
                  if DotPos <> 0 then
                    s[DotPos] := '.';
                end;
                Bcd := StrToBcd(string(s));
                DBNumeric := BcdToDBNumeric(Bcd);
              end;
            {$IFDEF CLR}
              Marshal.StructureToPtr(TObject(DBNumeric), pValue, False);
            {$ELSE}
              PDBNumeric(pValue)^ := DBNumeric;
            {$ENDIF}
            end
            else begin
              s := AnsiString(ParamDesc.Value);
              if DecimalSeparator <> '.' then begin
                DotPos := Pos(AnsiString(DecimalSeparator), s);
                if DotPos <> 0 then
                  s[DotPos] := '.';
              end;
              l := Length(s);
              Marshal.WriteInt32(pLength, l);

              if l > 0 then
                CopyBufferAnsi(s, pValue, l + 1{#0});
            end;
        {$ENDIF}
        {$ENDIF}
          dtMSXML:
            SetOleVariant(pValue, ParamDesc.Value);
          dtMemo, dtWideMemo, dtBlob:
            if pBind.wType = DBTYPE_IUNKNOWN then begin
              // Create stream
              OLEDBStream := TOLEDBStream.Create(TBlob(ParamDesc.GetObject), nil{FStreamList});
              Stream := Marshal.GetIUnknownForObject(OLEDBStream);
              Marshal.WriteIntPtr(pValue, Stream);
              // Set stream size
              if IsCompactEdition(ServerVersion) then
                Marshal.WriteInt32(pLength, Integer(OLEDBStream.Size))
              else
                Marshal.WriteInt32(pLength, -1);
            end
            else begin
              ws := TBlob(ParamDesc.GetObject).AsWideString;
              l := Length(ws) * sizeof(WideChar) + 2{#0#0};
              if l > (pBind.cbMaxLen - 1{#0}) * sizeof(WideChar) then // Truncate too long values
                l := (pBind.cbMaxLen - 1{#0}) * sizeof(WideChar);
              Marshal.WriteInt32(pLength, l);

              if l > 0 then
                CopyBufferUni(ws, pValue, l);
            end;
          dtTable: begin
            Rowset := (TOLEDBParamDesc(ParamDesc).GetObject as TMSTableObject).FRecordSet.GetFilledRowset;
          {$IFNDEF CLR}
            Rowset._AddRef;
          {$ENDIF};
            Marshal.WriteIntPtr(pValue, {$IFDEF CLR}Marshal.GetIUnknownForObject{$ELSE}IntPtr{$ENDIF}(Rowset));
            Marshal.WriteInt32(pLength, -1);
          end;
        else
          Assert(False, Format('ParamDesc - %s, Unknown DataType = %d', [ParamDesc.GetName, ParamDesc.GetDataType]));
        end;
      end;
  end;
end;

procedure TOLEDBCommand.CreateAndFillParamAccs;
var
  ParamCnt: integer;

  procedure PrepareParamAcc(out BindMemorySize: NativeUInt); // Set preliminary binding data
  var
    i: integer;
  begin
    Assert(Length(FParamsAccessorData.rgBindings) <> 0);
    for i := 0 to ParamCnt - 1 do
      FParamsAccessorData.rgBindings[i].pObject := nil;

    BindMemorySize := 0;
    for i := 0 to ParamCnt - 1 do
      FillBindingForParam(i + 1, TOLEDBParamDesc(FParams[i]), FConnection, FParamsAccessorData.rgBindings[i], BindMemorySize, True, False);
  end;

var
{$IFDEF CLR}
  rgBindingsGC: GCHandle;
{$ENDIF}
  rgBindings: PDBBinding;
  rgStatus: PDBBINDSTATUSArray;
  i: integer;
  BindMemorySize: NativeUInt;

begin
  Assert(FICommandText <> nil, 'FICommandText must be setted to CreateAndFillParamAccs');
  Assert(not FParamsAccessorDataAvaible, 'procedure CreateAndFillParamAccs already called');
  ParamCnt := FParams.Count;
  rgStatus := Marshal.AllocHGlobal(ParamCnt * SizeOf(DBBINDSTATUS));
{$IFNDEF CLR}
  IntPtr(FParamsAccessorData.Accessor) := nil;
{$ENDIF}

  FCanReadParams := False;

  try
    try
      FParamsAccessorData.ExecuteParams.HACCESSOR := 0;
      FParamsAccessorData.ExecuteParams.pData := nil;
      FParamsAccessorData.ExecuteParams.cParamSets := 1;

      SetLength(FParamsAccessorData.rgBindings, ParamCnt);
      PrepareParamAcc(BindMemorySize);

      FParamsAccessorData.ExecuteParams.pData := Marshal.AllocHGlobal(BindMemorySize);
      FillChar(FParamsAccessorData.ExecuteParams.pData, BindMemorySize, 0);

      for i := 0 to ParamCnt - 1 do
        SaveParamValue(FParams[i], FParamsAccessorData.rgBindings[i],
          FParamsAccessorData{$IFDEF HAVE_COMPRESS}, FCompressBlob{$ENDIF}
          {$IFDEF CLR}, FParamsGC{$ENDIF}, FConnection.DBMSPrimaryVer, FConnection.ProviderPrimaryVer);

      QueryIntf(FICommandText, {$IFDEF CLR}IAccessor{$ELSE}IID_IAccessor{$ENDIF}, FParamsAccessorData.Accessor);
    {$IFDEF CLR}
      rgBindingsGC := GCHandle.Alloc(FParamsAccessorData.rgBindings, GCHandleType.Pinned);
      rgBindings := Marshal.UnsafeAddrOfPinnedArrayElement(FParamsAccessorData.rgBindings, 0);
    {$ELSE}
      rgBindings := @FParamsAccessorData.rgBindings[0];
    {$ENDIF}
      Check(FParamsAccessorData.Accessor.CreateAccessor(
        DBACCESSOR_PARAMETERDATA, ParamCnt, rgBindings, BindMemorySize,
        FParamsAccessorData.ExecuteParams.HACCESSOR, rgStatus));
      FParamsAccessorDataAvaible := True;
    except
      if FParamsAccessorData.ExecuteParams.pData <> nil then
        Marshal.FreeHGlobal(FParamsAccessorData.ExecuteParams.pData);
      FParamsAccessorData.ExecuteParams.pData := nil;

      if Length(FParamsAccessorData.rgBindings) <> 0 then begin
        for i := 0 to ParamCnt - 1 do begin
          if FParamsAccessorData.rgBindings[i].pObject <> nil then
            Marshal.FreeHGlobal(FParamsAccessorData.rgBindings[i].pObject);
          if FParams[i].GetDataType = dtTable then
            TMSTableObject(FParams[i].GetObject).ReleaseAllInterfaces;
        end;
        SetLength(FParamsAccessorData.rgBindings, 0);
      end;

      FParamsAccessorDataAvaible := False;
      raise;
    end;
  finally
    Marshal.FreeHGlobal(rgStatus);
  {$IFDEF CLR}
    if IntPtr(rgBindingsGC) <> nil then
      rgBindingsGC.Free;
  {$ENDIF}
  end;
end;

procedure TOLEDBCommand.Execute(Iters: integer = 1);
  procedure DoExecute;
  var
    AsynchComplete: boolean;

    function OpenOrExec: IUnknown;
    var
      pParams: PDBPARAMS;
      RequestInt: TGUID;
      hr: HRESULT;
      i: Integer;

    begin
      if not GetPrepared then
        SetCommandProp;

      if FRequestIUnknown or (FNonBlocking and (not FRequestIUnknown)) then
        RequestInt := IID_IUnknown
      else
        if GetUseOutputStream then
          RequestInt := IID_ISequentialStream
        else
          RequestInt := IID_NULL;

      FWaitForBreak := False;
      try
        if FRPCCall then
          SetParameterInfo
        else
          if FIsSProc then
            for i := 0 to FParams.Count - 1 do
              if FParams[i].GetDataType = dtTable then begin
                SetParameterInfo;
                Break;
              end;

        if FParams.Count = 0 then
          hr := ICommand(FICommandText).Execute(nil, RequestInt, nil, FRowsAffected, Result)
        else begin
          CreateAndFillParamAccs;
        {$IFNDEF CLR}
          pParams := @FParamsAccessorData.ExecuteParams;
        {$ELSE}
          pParams := Marshal.AllocHGlobal(sizeof(DBPARAMS));
          try
            Marshal.StructureToPtr(FParamsAccessorData.ExecuteParams, pParams, False);
        {$ENDIF}
            hr := ICommand(FICommandText).Execute(nil, RequestInt, pParams, FRowsAffected, Result);
        {$IFDEF CLR}
          finally
            Marshal.FreeHGlobal(pParams);
          end;
        {$ENDIF}
        end;

        AsynchComplete := True;
        if FNonBlocking and (hr = DB_S_ASYNCHRONOUS) then
          AsynchComplete := False
        else
          Check(hr, Analyze);

      {$IFDEF AUTOTEST}
        Inc(__ServerExecuteCount);
      {$ENDIF}
      except
        on E: Exception do begin
          if (E is EOLEDBError) and (EOLEDBError(E).ErrorCode = DB_E_ABORTLIMITREACHED) then
            Unprepare;
          Result := nil;
          FIUnknown := nil;
          ClearIMultipleResults;
          FISSAsynchStatus := nil;

          raise;
        end;
      end;
    end;

  var
    IUnk: IUnknown;
  begin
    Assert(FIUnknown = nil);
    FLastExecWarning := False;
    if FIMultipleResults = nil then begin
      IUnk := OpenOrExec; // After call IUnk may be [nil, IUnknown (message), IUnknown (cursor)]
      Assert(FRequestIUnknown or FNonBlocking or GetUseOutputStream or (IUnk = nil));

      if FRequestIUnknown and (IUnk <> nil) then begin
        if FRequestMultipleResults then
          QueryIntf(IUnk, {$IFDEF CLR}IMultipleResults{$ELSE}IID_IMultipleResults{$ENDIF}, FIMultipleResults)
        else // This is a server cursor or DbxSda call
          FIUnknown := IUnk;
        IUnk := nil;
      end;

      if not FRequestIUnknown and FNonBlocking and not AsynchComplete then begin
        Assert(IUnk <> nil);
        QueryIntf(IUnk, {$IFNDEF CLR}IID_ISSAsynchStatus{$ELSE}ISSAsynchStatus{$ENDIF}, FISSAsynchStatus);
      end;
    end;

    if FRequestIUnknown and (FIMultipleResults <> nil) then
      if FIUnknownNext = nil then
        GetNextResult(FIUnknown, FRowsAffected)
      else
      begin
        FIUnknown := FIUnknownNext;
        FIUnknownNext := nil;
        FRowsAffected := FRowsAffectedNext;
      end;
  end;
begin
  if (FCursorState <> csInactive) and (FCursorState <> csPrepared) then
    Exit;

  FExecuting := True;
  SetCursorState(csExecuting);
  QueryInterfaces(False); // If QueryInterfaces already called then do nothing
  try
    DoExecute;
  except
    on E: Exception do begin
      if (not FNonBlocking) or FRequestIUnknown or (FISSAsynchStatus = nil) then
        EndExecute(E);

      ClearIMultipleResults;
      FIUnknownNext := nil;
      FIUnknown := nil;
      RequestParamsIfPossible;
      raise;
    end;
  end;
  if FNonBlocking and (FISSAsynchStatus <> nil) then begin
    Assert(FExecutor = nil);
    FExecutor := TOLEDBThreadWrapper.Create(TExecuteThread, True);
    FExecutor.OnException := DoExecuteException;
    FExecutor.OnTerminate := DoExecuteTerminate;
    TExecuteThread(FExecutor.FThread).FRunMethod := WaitAsynchCompletion;
    FExecutor.FreeOnTerminate := True;
    FExecutor.Resume;
  end;
  if (not FNonBlocking) or FRequestIUnknown or (FISSAsynchStatus = nil) then
    EndExecute(nil);
end;

procedure TOLEDBCommand.DoExecuteTerminate(Sender: TObject); // MainThread context
begin
  EndExecute(FExecutor.FException);
  FExecutor := nil;
end;

procedure TOLEDBCommand.DoExecuteException(Sender: TObject; E: Exception; var Fail: boolean); // MainThread context
begin
  if (E is EOLEDBError) then
    FConnection.DoError(EOLEDBError(E), Fail);
end;

procedure TOLEDBCommand.WaitAsynchCompletion; // FExecuter.FThread context
var
  hr: HRESULT;
  Completed: boolean;

begin
  if FISSAsynchStatus <> nil then begin
    try
      Completed := False;
      while not TExecuteThread(FExecutor.FThread).Terminated do begin
        hr := FISSAsynchStatus.WaitForAsynchCompletion(100);
        case hr of
          S_OK:
            Completed := True;
          DB_S_ASYNCHRONOUS:;
          else
            FConnection.Check(hr, nil);
        end;
        if Completed then
          break;
      end;
      if not Completed then
        FISSAsynchStatus.Abort(DB_NULL_HCHAPTER, DBASYNCHOP_OPEN);
    finally
      ClearISSAsynchStatus;
    end;
  end;
end;

procedure TOLEDBCommand.EndExecute(E: Exception); // MainThread context
begin
  try
    try
      RequestParamsIfPossible;
    finally
      ReleaseInterfaces;
    end;

    if FIUnknown = nil then
      FCursorState := csInactive
    else
      FCursorState := csExecuted;

    if Assigned(FAfterExecute) then // Must be after RequestParamsIfPossible
      FAfterExecute(E = nil);
  finally
    FExecuting := False;
  end;
end;

procedure ConvertStreamToBlob(const pValue: IntPtr; const Length: integer; Blob: TBlob;
  {$IFDEF HAVE_COMPRESS}CompressBlobMode: TCompressBlobMode;{$ENDIF} OmitXMLPreamble: boolean = False);
var
  Stream: ISequentialStream;
  BytesReadedFromStream: Longint;
  p, pXML: IntPtr;

  Piece: PPieceHeader;
  PieceSize: integer;
{$IFDEF CLR}
  o: System.Object;
  // otype: System.Type;
{$ENDIF}
  pBytesReadedFromStream: IntPtr;
  FirstRead: boolean;
  BlobSize: integer;
  SizeAvailable: boolean;
begin
  p := Marshal.ReadIntPtr(pValue);
  Assert(p <> nil);
  BlobSize := Length;
  SizeAvailable := BlobSize > 0;
{$IFDEF CLR}
  o := Marshal.GetObjectForIUnknown(p);
  // d11 otype := System.Type.GetTypeFromCLSID(CLSID_SQLOLEDB);
  // d11 Stream := ISequentialStream(Marshal.CreateWrapperOfType(o, otype));
  Stream := ISequentialStream(o);
  o := nil;
{$ELSE}
  IntPtr(Stream) := IntPtr(p);
{$ENDIF}
  pBytesReadedFromStream := Marshal.AllocHGlobal(SizeOf(Integer));
  try
    Assert(Stream <> nil);

    if OmitXMLPreamble then begin
      // Skip FF FE bytes, CR 16149
      pXML := Marshal.AllocHGlobal(SizeOf(WideChar));
      try
        Stream.Read(pXML, SizeOf(WideChar), pBytesReadedFromStream);
      finally
        Marshal.FreeHGlobal(pXML);
      end;
    end;

    PieceSize := 10 * 1024;
    FirstRead := True;
    repeat
      if (BlobSize >= DefaultPieceSize) or not SizeAvailable then begin
        if not FirstRead then
          PieceSize := DefaultPieceSize;
      end
      else
        PieceSize := BlobSize;

      Blob.AllocPiece(Piece, PieceSize);
      try
      {$IFDEF CLR}
        Stream.Read(PtrOffset(Piece, SizeOf(TPieceHeader)), PieceSize, pBytesReadedFromStream);
        BytesReadedFromStream := Marshal.ReadInt32(pBytesReadedFromStream);
      {$ELSE}
        Stream.Read(PtrOffset(Piece, SizeOf(TPieceHeader)), PieceSize, @BytesReadedFromStream);
      {$ENDIF}
        Piece.Used := BytesReadedFromStream;
        if SizeAvailable then
          Dec(BlobSize, BytesReadedFromStream);
      finally
        Blob.CompressPiece(Piece);
        if IntPtr(Piece) <> nil then
          Blob.AppendPiece(Piece);
        FirstRead := False;
      end;
    until ((BlobSize = 0) and SizeAvailable) or ((PieceSize <> BytesReadedFromStream) and not SizeAvailable);
  {$IFDEF HAVE_COMPRESS}
    if (Blob is TCompressedBlob) and ((CompressBlobMode = cbClient) or (CompressBlobMode = cbClientServer)) then
      TCompressedBlob(Blob).Compressed := True;
  {$ENDIF}
  finally
  {$IFDEF CLR}
    Marshal.ReleaseComObject(Stream);
    Marshal.Release(p);
  {$ENDIF}
    Marshal.FreeHGlobal(pBytesReadedFromStream);
  end;
end;

procedure TOLEDBCommand.RequestAndFreeParamAccs;
  procedure ProcessParam(ParamDesc: TParamDesc; var pBind: TDBBinding);
  var
    Status: DWORD;
    pLength, pValue, pStatus: IntPtr;
    l: UINT;
    c: Currency;
    i64: Int64;
    IsNull: boolean;

  {$IFDEF CLR}
    Bcd: TBcd;
    b: TBytes;
    d: double;
  {$ENDIF}
    DBTimeStamp: TDBTimeStamp;
  {$IFDEF VER6P}{$IFNDEF FPC}
    SQLTimeStamp: TSQLTimeStamp;
  {$ENDIF}{$ENDIF}
    dt: TDateTime;
  {$IFDEF VER6P}
    DBNumeric: TDBNumeric;
  {$ENDIF}
    Blob: TSharedObject;
  begin
    if (ParamDesc.GetParamType in [pdUnknown, pdInputOutput, pdOutput, pdResult])
      and not ((ParamDesc.GetParamType = pdUnknown) and (ParamDesc.GetDataType in [dtBlob, dtMemo, dtWideMemo, dtMSXML])) then begin
      pStatus :=  PtrOffset(FParamsAccessorData.ExecuteParams.pData, pBind.obStatus);
      Status := DWORD(Marshal.ReadInt32(pStatus));
      IsNull := (Status = DBSTATUS_S_ISNULL) or ((Status = DBSTATUS_E_UNAVAILABLE) and (ParamDesc.GetNull or (ParamDesc.GetParamType <> pdInputOutput)));
      if not IsOutputLOB(ParamDesc, FConnection.DBMSPrimaryVer, FConnection.ProviderPrimaryVer) then
        ParamDesc.SetNull(IsNull)
      else begin
        Blob := ParamDesc.GetObject;
        try
          ParamDesc.SetNull(IsNull);
        finally
          ParamDesc.SetObject(Blob);
        end;
      end;

      if ParamDesc.GetNull and not (ParamDesc.GetDataType in [dtMemo, dtWideMemo]) then
        Marshal.WriteInt32(pStatus, Integer(DBSTATUS_S_ISNULL))
      else begin
        pValue := PtrOffset(FParamsAccessorData.ExecuteParams.pData, pBind.obValue);
        pLength := PtrOffset(FParamsAccessorData.ExecuteParams.pData, pBind.obLength);
        case ParamDesc.GetDataType of
          dtUnknown:
            ParamDesc.SetValue(GetOleVariant(pValue));
          dtString:
            ParamDesc.SetValue(Marshal.PtrToStringAnsi(pValue));
          dtWideString:
            ParamDesc.SetValue(Marshal.PtrToStringUni(pValue));
          dtBytes, dtVarBytes: begin
            l := Marshal.ReadInt32(pLength);
            if l > 0 then begin
            {$IFDEF CLR}
              SetLength(b, l);
              Marshal.Copy(pValue, b, 0, l);
              ParamDesc.Value := b;
            {$ELSE}
              ParamDesc.SetValue(VarArrayCreate([0, l - 1], varByte));
              CopyBuffer(pValue, TVarData(ParamDesc.Value).VArray.Data, l);
            {$ENDIF}
            end;
          end;
          dtInt8:
            ParamDesc.SetValue(Byte(Marshal.ReadByte(pValue)));
          dtInt16:
            ParamDesc.SetValue(SmallInt(Marshal.ReadInt16(pValue)));
          dtWord:
            ParamDesc.SetValue(Word(Marshal.ReadInt16(pValue)));
          dtInt32:
            ParamDesc.SetValue(Integer(Marshal.ReadInt32(pValue)));
          dtFloat:
            ParamDesc.SetValue(BitConverter.Int64BitsToDouble(Marshal.ReadInt64(pValue)));
          dtCurrency:
            ParamDesc.SetValue(BitConverter.Int64BitsToDouble(Marshal.ReadInt64(pValue)));
            // ParamDesc.SetValue(PCurrency(pValue)^); Currency type cannot be used over TCurrencyField uses double to store
          dtDate, dtTime:
            ParamDesc.SetValue(TDateTime(BitConverter.Int64BitsToDouble(Marshal.ReadInt64(pValue))));
          dtDateTime: begin
          {$IFDEF CLR}
            DBTimeStamp := TDBTimeStamp(Marshal.PtrToStructure(pValue, TypeOf(TDBTimeStamp)));
          {$ELSE}
            DBTimeStamp := PDBTimeStamp(pValue)^;
          {$ENDIF}
            dt := EncodeDateTime(DBTimeStamp.year, DBTimeStamp.month, DBTimeStamp.day, DBTimeStamp.hour, DBTimeStamp.minute, DBTimeStamp.second, DBTimeStamp.fraction div 1000000{Billionths of a second to milliseconds});
            ParamDesc.SetValue(dt);
          end;
        {$IFDEF VER6P}{$IFNDEF FPC}
          dtSQLTimeStamp: begin
          {$IFDEF CLR}
            SQLTimeStamp := TSQLTimeStamp(Marshal.PtrToStructure(pValue, TypeOf(TSQLTimeStamp)));
            SQLTimeStamp.Fractions := SQLTimeStamp.Fractions div 1000000 ; // billionths to milliseconds of a second
          {$ELSE}
            SQLTimeStamp := PSQLTimeStamp(pValue)^;
          {$ENDIF}
            SQLTimeStamp.Fractions := SQLTimeStamp.Fractions div 1000000 ; // billionths to milliseconds of a second
            ParamDesc.SetValue(VarSQLTimeStampCreate(SQLTimeStamp));
          end;
        {$ENDIF}{$ENDIF}
          dtBoolean:
            ParamDesc.SetValue(Boolean(WordBool(Marshal.ReadInt16(pValue))));
          dtInt64:
        {$IFDEF VER6P}
            ParamDesc.SetValue(Marshal.ReadInt64(pValue));
        {$ELSE}
          begin
            TVarData(_TParamDesc(ParamDesc).FData).VType := $000E;
            PInt64(@TVarData(ParamDesc.Value).VInteger)^ := PInt64(pValue)^;
          end;
        {$ENDIF}
        {$IFDEF VER5P}
          dtGuid:
          {$IFDEF CLR}
            ParamDesc.SetValue(GUIDToString(TGuid(Marshal.PtrToStructure(pValue, TypeOf(TGuid)))));
          {$ELSE}
            ParamDesc.SetValue(GUIDToString(PGuid(pValue)^));
          {$ENDIF}
          dtVariant:
            ParamDesc.SetValue(GetOleVariant(pValue));
        {$ENDIF}
          dtBCD: begin
            i64 := Marshal.ReadInt64(pValue);
          {$IFDEF CLR}
            d := i64;
            d := d / 10000;
            c := d;
          {$ELSE}
            c := PCurrency(@i64)^;
          {$ENDIF}
            ParamDesc.SetValue(c);
          end;
        {$IFDEF VER6P}
        {$IFNDEF FPC}
          dtFmtBCD:
            if FConnection.FProvider <> prCompact then
              ParamDesc.SetValue(Marshal.PtrToStringAnsi(pValue))
            else begin
            {$IFDEF CLR}
              DBNumeric := TDBNumeric(Marshal.PtrToStructure(pValue, TypeOf(TDBNumeric)));
            {$ELSE}
              DBNumeric := PDBNumeric(pValue)^;
            {$ENDIF}
              ParamDesc.SetValue(VarFMTBcdCreate(DBNumericToBcd(DBNumeric)));
            end;
        {$ENDIF}
        {$ENDIF}
          dtMSXML:
            ParamDesc.SetValue(GetOleVariant(pValue));
          dtMemo, dtWideMemo: begin
            if ParamDesc.GetNull then begin
              Marshal.WriteInt32(pStatus, Integer(DBSTATUS_S_ISNULL));
              if (Marshal.ReadIntPtr(pValue) = nil) or (Marshal.ReadInt32(pLength) <= 0) then
                Exit;
            end;

            Blob := ParamDesc.GetObject;
            Assert(Blob <> nil);
            TBlob(Blob).Clear;
            ConvertStreamToBlob(pValue, 0, TBlob(Blob){$IFDEF HAVE_COMPRESS}, FCompressBlob{$ENDIF});
          end;
        else
          Assert(False, Format('ParamDesc - %s, Unknown DataType = %d', [ParamDesc.GetName, ParamDesc.GetDataType]));
        end;
      end;
    end;
  end;

var
  i: integer;
  ParamCnt: integer;
begin
  Assert(FIUnknown = nil, 'Before RequestAndFreeParamAccs interface FIUnknown must be released');
  // Assert(FIMultipleResults = nil, 'Before RequestAndFreeParamAccs interface FIMultipleResults must be released');

  if FParamsAccessorDataAvaible and (FParams <> nil) and (FParams.Count > 0) then begin
    ParamCnt := FParams.Count;
    try
      if FConnection <> nil then begin
        if FParamsAccessorData.ExecuteParams.HACCESSOR <> 0 then
          Check(FParamsAccessorData.Accessor.ReleaseAccessor(FParamsAccessorData.ExecuteParams.HACCESSOR, nil));
        FParamsAccessorData.ExecuteParams.HACCESSOR := 0;
        FParamsAccessorData.Accessor := nil;

        for i := 0 to ParamCnt - 1 do
          ProcessParam(FParams[i], FParamsAccessorData.rgBindings[i]);
      end;

      if FParamsAccessorData.ExecuteParams.pData <> nil then
        Marshal.FreeHGlobal(FParamsAccessorData.ExecuteParams.pData);
      FParamsAccessorData.ExecuteParams.pData := nil;

    finally
      if Length(FParamsAccessorData.rgBindings) <> 0 then begin
        for i := 0 to ParamCnt - 1 do begin
          if FParamsAccessorData.rgBindings[i].pObject <> nil then
            Marshal.FreeHGlobal(FParamsAccessorData.rgBindings[i].pObject);
          if FParams[i].GetDataType = dtTable then
            TMSTableObject(FParams[i].GetObject).ReleaseAllInterfaces;
        end;
        SetLength(FParamsAccessorData.rgBindings, 0);
      end;

      FParamsAccessorDataAvaible := False;

      (*// Remove unused streams
      for i := FStreamList.Count - 1 downto 0 do
        TOLEDBStream(FStreamList[i])._Release;
      Assert(FStreamList.Count = 0);*)
    end;

    FCanReadParams := True;
    if Assigned(FReadParams) and (FConnection <> nil) then
      FReadParams;
  end;
end;

procedure TOLEDBCommand.RequestParamsIfPossible; // Call RequestAndFreeParamAccs if interfaces is cleared
{$IFDEF CLR}
var
  i: integer;
{$ENDIF}
begin
{$IFDEF CLR}
  for i := Low(FParamsGC) to High(FParamsGC) do
    FreeGCHandle(FParamsGC[i]);
  SetLength(FParamsGC, 0);
{$ENDIF}

  if (FIUnknown = nil) then begin
    if (FIMultipleResults <> nil) and not FNextResultRequested then begin
      GetNextResult(FIUnknownNext, FRowsAffectedNext);
      FNextResultRequested := True;
    end;

    if FIMultipleResults = nil then
      RequestAndFreeParamAccs;
  end;
end;

function TOLEDBCommand.Analyze(const Status: HRESULT; const Arg: IntPtr = nil): WideString;
const
  ParamHeader = 'Parameter[%d] %s - %s (Status = %Xh).';
var
  i: integer;
  ParamStatus: DWORD;
  ParamName: _string;
  Msg: WideString;
begin
  Result := '';
  if (Status and $80000000) <> 0 then begin // Severity bit (see OLEDBC.pas line 5489) is not 0
    if Status = DB_E_OBJECTOPEN then
      Result := SObjectOpen
    else
      if FParamsAccessorData.ExecuteParams.pData <> nil then begin
        Msg := '';
        with FParamsAccessorData do
          for i := 0 to FParams.Count - 1 do begin
            ParamName := FParams[i].GetName;
            if ParamName = '' then
              ParamName := IntToStr(i)
            else
              ParamName := ':' + ParamName;
            ParamStatus := DWORD(Marshal.ReadInt32(ExecuteParams.pData, rgBindings[i].obStatus));
            case ParamStatus of
              DBSTATUS_S_OK, DBSTATUS_S_ISNULL, DBSTATUS_S_DEFAULT:;
              DBSTATUS_E_BADACCESSOR:
                AddInfoToErr(Msg, ParamHeader, [i, ParamName, SInvalidParamType, ParamStatus]);
              DBSTATUS_E_CANTCONVERTVALUE:
                AddInfoToErr(Msg, ParamHeader, [i, ParamName, SInvalidValue, ParamStatus]);
              DBSTATUS_S_TRUNCATED:
                AddInfoToErr(Msg, ParamHeader, [i, ParamName, SDataTruncated, ParamStatus]);
              DBSTATUS_E_SIGNMISMATCH:
                AddInfoToErr(Msg, ParamHeader, [i, ParamName, SSignMismatch, ParamStatus]);
              DBSTATUS_E_DATAOVERFLOW:
                AddInfoToErr(Msg, ParamHeader, [i, ParamName, SDataOverflow, ParamStatus]);
              DBSTATUS_E_CANTCREATE:
                AddInfoToErr(Msg, ParamHeader, [i, ParamName, SOutOfMemory, ParamStatus]);
              DBSTATUS_E_UNAVAILABLE:
                {AddInfoToErr(Msg, ParamHeader, [i, ParamName, SUnavaible, ParamStatus])};
              DBSTATUS_E_INTEGRITYVIOLATION:
                AddInfoToErr(Msg, ParamHeader, [i, ParamName, SIntegrityViolation, ParamStatus]);
              DBSTATUS_E_SCHEMAVIOLATION:
                AddInfoToErr(Msg, ParamHeader, [i, ParamName, SShemaViolation, ParamStatus]);
              DBSTATUS_E_BADSTATUS:
                AddInfoToErr(Msg, ParamHeader, [i, ParamName, SBadStatus, ParamStatus]);
            else
              AddInfoToErr(Msg, ParamHeader, [i, ParamName, SUnknownStatus, ParamStatus]);
            end;
          end; // for
        Result := Msg;
      end; // if Status = DB_E_OBJECTOPEN
  end
  else // if (Status and $80000000) <> 0 then begin
    if Status = DB_S_ERRORSOCCURRED then
      FLastExecWarning := True;
end;

procedure TOLEDBCommand.GetNextResult(out ResultSet: IUnknown; out RowsAffected: NativeInt;
  PrevErr: EOLEDBError = nil);
var
  hr, lasthr: HRESULT;
  OldRowsAffected: NativeInt;
  CurrentErr: EOLEDBError;
begin
  try
    lasthr := 0;
    repeat
      OldRowsAffected := FRowsAffected;
      hr := FIMultipleResults.GetResult(nil, 0, IID_IUnknown, RowsAffected, ResultSet);

      try
        if (hr = DB_E_OBJECTOPEN) and (ResultSet = nil) and (lasthr <> DB_E_OBJECTOPEN) then
          RequestAndFreeParamAccs
        else
          if (hr <> DB_S_NORESULT) then
            Check(hr, Analyze);
        lasthr := hr;
        FConnection.OLEDBError(0, Component); // Check Info messages

        if ResultSet <> nil then begin
          OldRowsAffected := RowsAffected;
          Break;
        end;

        if FWaitForBreak then begin
          FWaitForBreak := False;
          FConnection.OLEDBError(DB_E_CANCELED, Component);
        end;

      except
        on E: EOLEDBError do begin
          CurrentErr := E;
          try
            if PrevErr <> nil then
              AddInfoToErr(CurrentErr, PrevErr);

            if (FIMultipleResults <> nil) and (ResultSet = nil) and (hr <> DB_S_NORESULT) then
              GetNextResult(ResultSet, RowsAffected, CurrentErr);
          except
            if CurrentErr <> E then
              CurrentErr.Free;
            raise;
          end;

          if CurrentErr <> E then
            raise CurrentErr
          else
            raise;
        end
        else
          raise;
      end;

    until (hr <> S_OK) and (hr <> DB_E_OBJECTOPEN);
    RowsAffected := OldRowsAffected;
  finally
    if ResultSet = nil then
      ClearIMultipleResults; // Rowsets not also provided
  end;

end;

function TOLEDBCommand.CreateProcCall(const Name: _string; NeedDescribe: boolean; IsQuery: boolean): _string;

  procedure ParseProcName(var ProcName: _string; out OverloadNum: _string);
  var
    i: integer;
  begin
    OverloadNum := '';

    i := Pos(';', ProcName);
    if i > 0 then begin
      i := Length(ProcName);
      while AnsiChar(ProcName[i]) in (['0'..'9']) do
        Dec(i);

      if ProcName[i] = ';' then begin
        OverloadNum := Copy(ProcName, i + 1, 1024);
        Delete(ProcName, i, 1024);
      end;
    end;
  end;

  procedure DescribeParams(const MasterDatabase: boolean; out OriginalDatabase: _string);
  var
    ParamsMetaInfo: TOLEDBRecordSet;

    procedure FillParams;
      function GetOffsetByName(const Name: _string): integer;
      var
        i: integer;
      begin
        for i := 0 to ParamsMetaInfo.Fields.Count - 1 do
          if ParamsMetaInfo.Fields[i].Name = Name then begin
            Result := ParamsMetaInfo.Fields[i].Offset;
            Exit;
          end;
        Result := - MaxInt;
        Assert(False, 'Unknown field name ' + Name);
      end;

    var
      NameFld, TypeNameFld, TypeFld, HasDefaultFld, DefaultFld, DataTypeFld, OctetLengthFld, CharMaxLenFld: integer; // offsets
      Param: TOLEDBParamDesc;
      ParamType: TParamDirection;
      s, ParamName: _string;
      DataType: word;
      IsLong: boolean;
      RecBuf: IntPtr;
      ws: WideString;
      dt: integer;
      OctetLength: integer;
    begin
      FParams.Clear;

      NameFld := GetOffsetByName('PARAMETER_NAME'); // DataSize = 129
      TypeNameFld := GetOffsetByName('TYPE_NAME'); // DataSize = 129
      TypeFld := GetOffsetByName('PARAMETER_TYPE'); // DataSize = 4
      HasDefaultFld := GetOffsetByName('PARAMETER_HASDEFAULT'); // DataSize = 2
      DefaultFld := GetOffsetByName('PARAMETER_DEFAULT'); // DataSize = 256
      DataTypeFld := GetOffsetByName('DATA_TYPE'); // DataSize = 4
      OctetLengthFld := GetOffsetByName('CHARACTER_OCTET_LENGTH'); // DataSize = 4
      CharMaxLenFld := GetOffsetByName('CHARACTER_MAXIMUM_LENGTH'); // DataSize = 4

      RecBuf := nil;
      ParamsMetaInfo.AllocRecBuf(RecBuf);
      try
        while True do begin
          ParamsMetaInfo.GetNextRecord(RecBuf);
          if ParamsMetaInfo.Eof then
            Break;

          OctetLength := Marshal.ReadInt32(RecBuf, OctetLengthFld);
          IsLong := (OctetLength >= MaxInt - 1) or (OctetLength = 0);
          dt := Marshal.ReadInt32(RecBuf, DataTypeFld);

          if ConvertOLEDBTypeToInternalFormat(dt, IsLong, CalcEnableBCD, CalcEnableFMTBCD, False,
            0, 0, True, GetWideMemos,
            True, DataType, FConnection.DBMSPrimaryVer)
          then begin
            ParamType := ConvertOLEDBParamTypeToCR(Marshal.ReadInt32(RecBuf, TypeFld));
            Param := TOLEDBParamDesc.Create;
            Param.SetParamType(ParamType);
            FParams.Add(Param);

            //Choice "Bytes" and "VarBytes"
            s := Marshal.PtrToStringUni(PtrOffset(RecBuf, TypeNameFld));
            if (DataType = dtBytes) and (s = 'varbinary') then // WAR user-defined types?
              DataType := dtVarBytes
            else
              if DataType = dtTable then
                Param.FTableTypeName := s;
            Param.SetDataType(DataType);

            ParamName := GetParamNameWODog(Marshal.PtrToStringUni(PtrOffset(RecBuf, NameFld)));
            Param.SetName(ParamName);

            if Word(Marshal.ReadInt16(RecBuf, HasDefaultFld)) <> 0 then begin
              ws := Marshal.PtrToStringUni(PtrOffset(RecBuf, DefaultFld));
              Param.SetValue(ws);
            end;

            if IsLargeDataTypeUsed(Param) and
              ((ParamType = pdOutput) or (ParamType = pdInputOutput)) and
              ((FConnection.DBMSPrimaryVer < 9) or (FConnection.ProviderPrimaryVer < 9)) then
              Param.SetParamType(pdInput);

            Param.SetSize(Marshal.ReadInt32(RecBuf, CharMaxLenFld));
          end
          else begin
            s := Marshal.PtrToStringUni(PtrOffset(RecBuf, TypeNameFld));
            DatabaseErrorFmt(SBadFieldType, [s, dt]);
          end;
        end;
      finally
        if RecBuf <> nil then
          ParamsMetaInfo.FreeRecBuf(RecBuf);
      end;
    end;

    procedure ParseFullName(out Database: _string; out Owner: _string; out ProcName: _string);
    begin
      OLEDBSQLInfo.SplitObjectName(Name, OriginalDatabase, Owner, ProcName);
      Owner := SQLInfo.NormalizeName(Owner, False, True);
      ProcName := SQLInfo.NormalizeName(ProcName, False, True);
      if OriginalDatabase <> '' then
        Database := SQLInfo.NormalizeName(OriginalDatabase, False, True)
      else
        if not MasterDatabase then
          Database := FConnection.FDatabase
        else
          Database := DefaultSDACDatabase;
    end;

  var
    Database, Owner, ProcName, OverloadNum: _string;
    rgRestrictions: TRestrictions;
    Rowset: IRowset;
  begin
    ParseFullName(Database, Owner, ProcName);

    ParamsMetaInfo := TOLEDBRecordSet.Create;
    try
      ParamsMetaInfo.SetConnection(FConnection);
      ParamsMetaInfo.SetProp(prFetchAll, True);

      if FConnection.IsSQLAzureEdition then begin
        ParseProcName(ProcName, OverloadNum);
        if OverloadNum = '' then
          OverloadNum := '1';

        ParamsMetaInfo.SetSQL(_Format('exec sys.sp_procedure_params_managed N''%s'', %s, N''%s'', NULL', [ProcName, OverloadNum, Owner]));
      end
      else begin
        SetLength(rgRestrictions, 3);
        if (Length(ProcName) > 1) and (ProcName[1] = '#') then // for temporary stored procedures
          rgRestrictions[0] := 'tempdb'
        else
          rgRestrictions[0] := DataBase;
        rgRestrictions[1] := Owner;
        rgRestrictions[2] := ProcName;

        Rowset := FConnection.GetSchemaRowset(DBSCHEMA_PROCEDURE_PARAMETERS, rgRestrictions);
        ParamsMetaInfo.SetIRowset(Rowset, False);
      end;

      ParamsMetaInfo.Open;
      FillParams;
    finally
      ParamsMetaInfo.Close;
      ParamsMetaInfo.UnPrepare;
      ParamsMetaInfo.Free;
    end;
  end;

var
  i: integer;
  BracketAdded: boolean;
  Database, ProcName, OverloadNum, s: _string;
  ParamName: _string;
begin
  if FConnection = nil then
    DatabaseError(SConnectionNotDefined);

  FConnection.Connect('');

  if NeedDescribe then begin
    DescribeParams(False, Database);
    if (FParams.Count = 0) and (Database = '') then begin
      s := _LowerCase(Copy(Name, 1, 3));
      if (s = 'sp_') or (s = 'xp_') then
        DescribeParams(True, Database);
    end;
  end;

  ProcName := Name;
  ParseProcName(ProcName, OverloadNum);
  FSQL := 'CALL ' + SQLInfo.NormalizeName(ProcName, _char('"'), _char('"'){May be bug in MS SQL Server});
  if OverloadNum <> '' then
    FSQL := FSQL + ';' + OverloadNum;

  Result := FSQL;

  BracketAdded := False;
  for i := 0 to FParams.Count - 1 do begin
    ParamName := FParams[i].GetName;
    if ParamName = '' then
      ParamName := '?'
    else
      ParamName := ':' + ParamName;
    if FParams[i].GetParamType = pdResult then begin
      FSQL := '? = ' + FSQL;
      Result := ParamName + ' = ' + Result
    end
    else begin
      if BracketAdded then begin
        FSQL := FSQL + ', ?';
        Result := Result + ', ' + ParamName
      end
      else begin
        BracketAdded := True;
        FSQL := FSQL + ' (?';
        Result := Result + ' (' + ParamName
      end;
    end;
  end;
  if BracketAdded then begin
    FSQL := FSQL + ')';
    Result := Result + ')';
  end;
  FSQL := '{' + FSQL + '}';
  Result := '{' + Result + '}';
  FUserSQL := Result;
end;

function TOLEDBCommand.CalcEnableBCD: boolean;
begin
  Result := EnableBCD or ((FConnection <> nil) and FConnection.EnableBCD);
end;

function TOLEDBCommand.CalcEnableFMTBCD: boolean;
begin
  Result := {$IFDEF VER6P}{$IFNDEF FPC}EnableFMTBCD or ((FConnection <> nil) and
    FConnection.EnableFMTBCD){$ELSE}False{$ENDIF}{$ELSE}False{$ENDIF};
  if not FSensibleBCDMapping then
    Result := Result and not EnableBCD;
end;

procedure TOLEDBCommand.SetConnection(Value: TCRConnection);
begin
  if Value <> FConnection then begin
    inherited;

    FConnection := TOLEDBConnection(Value);
  end;
end;

function TOLEDBCommand.GetCursorState: TCursorState;
begin
  Result := FCursorState;
end;

procedure TOLEDBCommand.SetCursorState(Value: TCursorState);
begin
  FCursorState := Value;
end;

function TOLEDBCommand.ParseSQL(const SQL: _string; Params: TParamDescs; ReplaceAll: boolean = True; const RenamePrefix: _string = ''): _string;
begin
  if Pos(':', SQL) = 0 then begin
    if Params <> nil then
      Params.Clear;
    Result := SQL;
  end
  else
    Result := inherited ParseSQL(SQL, Params, ReplaceAll, RenamePrefix);
end;

function TOLEDBCommand.GetProp(Prop: integer; var Value: variant): boolean;
begin
  Result := True;

  case Prop of
    prRowsProcessed: 
      if FRowsAffected = -1 then
        Value := 0
      else
        Value := FRowsAffected;
    prCanReadParams:
      Value := FCanReadParams and IUnknownIsNull and IMultipleResultsIsNull;
    prIsStoredProc:
      Value := FIsSProc;
    prNonBlocking:
      Value := FNonBlocking;
    prDelayedSubsciption:
      Value := FDelayedSubsciption;
    prUseDescribeParams:
      Value := FUseDescribeParams;
  else
    Result := inherited GetProp(Prop, Value);
  end;
end;

function TOLEDBCommand.SetProp(Prop: integer; const Value: variant): boolean;
begin
  Result := True;
  case Prop of
    prCommandTimeout:
      FCommandTimeout := Value;
    prIsStoredProc:
      FIsSProc := Boolean(Value);
    prCanReadParams:
      FCanReadParams := Value;
    prNotification:
      FNotification := Value;
    prNotificationMessage:
      FNotificationMessage := Value;
    prNotificationService:
      FNotificationService := Value;
    prNotificationTimeout:
      FNotificationTimeout := Value;
    prNonBlocking:
      FNonBlocking := Value;
    prOutputStream:
      SetOutputStream(TStream({$IFNDEF CLR}Integer{$ENDIF}(Value)));
    prOutputEncoding: begin
      FOutputEncoding := TOLEDBOutputEncoding({$IFDEF CLR}Integer{$ENDIF}(Value));
      if FOLEDBStream <> nil then
        FOLEDBStream.OutputEncoding := FOutputEncoding;
    end;
    prSensibleBCDMapping:
      FSensibleBCDMapping := Value;
    prUseDescribeParams:
      FUseDescribeParams := Value;
  else
    Result := inherited SetProp(Prop, Value);
  end;
end;

function TOLEDBCommand.IUnknownIsNull: boolean;
begin
  Result := FIUnknown = nil;
end;

function TOLEDBCommand.IUnknownNextIsNull: boolean;
begin
  Result := FIUnknownNext = nil;
end;

function TOLEDBCommand.IMultipleResultsIsNull: boolean;
begin
  Result := FIMultipleResults = nil;
end;

procedure TOLEDBCommand.ClearIUnknown;
begin
  FIUnknown := nil;
end;

procedure TOLEDBCommand.ClearIUnknownNext;
begin
  FIUnknownNext := nil;
end;

procedure TOLEDBCommand.ClearIMultipleResults;
begin
{$IFDEF CLR}
  if FIMultipleResults <> nil then
    Marshal.ReleaseComObject(FIMultipleResults);
{$ENDIF}
  FIMultipleResults := nil; // Rowsets not also provided
end;

procedure TOLEDBCommand.ClearISSAsynchStatus;
begin
  FISSAsynchStatus := nil;
end;

procedure TOLEDBCommand.BreakExec;
begin
  if FNonBlocking and (FExecutor <> nil) then
    FExecutor.Terminate
  else begin
    FBreakExecCS.Acquire;
    try
      if FICommandText <> nil then
        Check(ICommand(FICommandText).Cancel);
      FWaitForBreak := True;
    finally
      FBreakExecCS.Release;
    end;
  end;
end;

function TOLEDBCommand.GetParamDescType: TParamDescClass;
begin
  Result := TOLEDBParamDesc;
end;

function TOLEDBCommand.AddParam: TParamDesc;
begin
  Result := TOLEDBParamDesc.Create;
  FParams.Add(Result);
end;

function TOLEDBCommand.GetParam(Index: integer): TOLEDBParamDesc;
begin
  Result := TOLEDBParamDesc(FParams[Index]);
end;

function TOLEDBCommand.GetWideMemos: boolean;
begin
  Assert(FConnection <> nil);
  Result := FConnection.FWideMemos;
end;

{ TOLEDBFieldDesc }

destructor TOLEDBFieldDesc.Destroy;
begin
{$IFNDEF LITE}
{$IFNDEF CLR}
  if FUDTDispatcher <> nil then begin
    FUDTDispatcher.ReleaseUDTProxy;
    //Assert(FUDTDispatcher.RefCount = 1);
    IDispatch(FUDTDispatcher)._Release;
  end;
{$ENDIF}
{$ENDIF}  
  inherited;
end;

{ TOLEDBSQLInfo }

function TOLEDBSQLInfo.LeftQuote: _char;
begin
  Result := {$IFDEF CLR}Devart.Sdac.{$ENDIF}{$IFNDEF UNIDACPRO}OLEDBAccess{$ELSE}OLEDBAccessUni{$ENDIF}.LeftQuote;
end;

function TOLEDBSQLInfo.RightQuote: _char;
begin
  Result := {$IFDEF CLR}Devart.Sdac.{$ENDIF}{$IFNDEF UNIDACPRO}OLEDBAccess{$ELSE}OLEDBAccessUni{$ENDIF}.RightQuote;
end;

function TOLEDBSQLInfo.ProcedureOverloadSeparator: _char;
begin
  Result := _char(';');
end;

function TOLEDBSQLInfo.IsQuoted(const Value: _string): boolean;
var
  len: integer;
begin
  len := Length(Value);
  if (len <= 1) then
    Result := False
  else
    Result := ((Value[1] = LeftQuote) and (Value[len] = RightQuote)) or  // [ ]
              ((Value[1] = '"') and (Value[len] = '"')) // " "
end;

procedure TOLEDBSQLInfo.SplitObjectName(const Name: _string; out DataBase: _string; out Owner: _string; out ObjName: _string);
var
  Info: TExtTableInfo;
begin
  inherited SplitObjectName(Name, Info);

  DataBase := Info.Catalog;
  Owner := Info.Schema;
  ObjName := Info.Table;
end;

function TOLEDBSQLInfo.NamesFromList(List: _TStrings): _string;
var
  i: integer;
begin
  Result := '';
  for i := 0 to List.Count - 1 do begin
    if i > 0 then
      Result := Result + ', ';
    Result := Result + NormalizeName(List[i]);
  end;
end;

procedure TOLEDBSQLInfo.NamesToList(Value: _string; List: _TStrings);
var
  St: _string;
  i: integer;
begin
  Value := Trim(Value);
  List.Clear;

  St := '';
  for i := 1 to Length(Value) do
    if (Value[i] = ',') or (Value[i] = ';') then begin
      St := NormalizeName(Trim(St), False, True);
      if St <> '' then
        List.Add(St);
      St := '';
    end
    else
      St := St + Value[i];

  St := NormalizeName(Trim(St), False, True);
  if St <> '' then
    List.Add(St);
end;

{ TOLEDBRecorSet }

constructor TOLEDBRecordSet.Create;
begin
  FUniqueRecords := False;
  FRequestSQLObjects := False;
  FCursorUpdate := True;
  FCursorTypeChanged := nil;

  FLockClearMultipleResults := False;
  FroAfterUpdate := False;

  FWideStrings := True; // Native fields mapping

  inherited;
  FNativeRowset := True;
  FCursorType := ctDefaultResultSet;
  FFetchRows := 25;
  FDisableMultipleResults := False;
end;

destructor TOLEDBRecordSet.Destroy;
begin
  Close;
  Assert((not FNativeRowset) or (FIRowset = nil), 'TOLEDBRecordSet.Destroy - interfaces not released');
  inherited;
end;

function TOLEDBRecordSet.GetFieldDescType: TFieldDescClass;
begin
  Result := TOLEDBFieldDesc;
end;

function TOLEDBRecordSet.GetIndicatorSize: word;
begin
  Result := FieldCount * OLE_DB_INDICATOR_SIZE;
end;

class function TOLEDBRecordSet.IsBlobFieldType(DataType: word): boolean;
begin
  Result := inherited IsBlobFieldType(DataType) or (DataType = dtMSXML);
end;

procedure TOLEDBRecordSet.SortItems;
begin
  if (FFetchExecutor <> nil) and
     (FCommand.GetCursorState = csFetchingAll) then begin
    FFetchExecutor.Resume;
    FFetchExecutor.WaitFor;
  end;
  inherited SortItems;
  if FFetchExecutor <> nil then
    FFetchExecutor.Resume;
end;

function TOLEDBRecordSet.IsCaseSensitive: boolean;
begin
  Result := False;
end;

procedure TOLEDBRecordSet.CreateComplexField(RecBuf: IntPtr; FieldIndex: integer; WithBlob: boolean);
var
  Blob: TSharedObject;
  FieldDesc: TFieldDesc;
begin
  FieldDesc := Fields[FieldIndex];
  case FieldDesc.DataType of
    dtBlob, dtMemo, dtWideMemo, dtMSXML:
      if WithBlob then begin
      {$IFDEF HAVE_COMPRESS}
        if FieldDesc.DataType = dtBlob then
          Blob := TCompressedBlob.Create
        else
      {$ENDIF}
          Blob := TBlob.Create;
        TBlob(Blob).EnableRollback;
        SetObject(FieldIndex + 1, RecBuf, Blob);
      end;
    else
      inherited;
  end;
end;

procedure TOLEDBRecordSet.CreateComplexFields(RecBuf: IntPtr; WithBlob: boolean);
var
  i: integer;
  Blob: TBlob;
  Field: TFieldDesc;
begin
  inherited;

  if WithBlob then
    for i := 0 to FieldCount - 1 do begin
      Field := Fields[i];
      if (Field.FieldDescKind <> fdkCalculated) and (((Field.DataType = dtMemo) or (Field.DataType = dtWideMemo)) and ((Field.SubDataType and dtWide) <> 0)) or (Field.DataType = dtMSXML) then begin
        Blob := TBlob(GetGCHandleTarget(Marshal.ReadIntPtr(RecBuf, Field.Offset)));
        Blob.IsUnicode := True;
      end
    end;
end;

procedure TOLEDBRecordSet.FreeComplexFields(RecBuf: IntPtr; WithBlob: boolean);
var
  i: integer;
  Handle: IntPtr;
  so: TSharedObject;
  b: boolean;
  Field: TFieldDesc;
begin
  inherited;

  for i := 0 to FieldCount - 1 do begin
    Field := Fields[i];
    if (Field.FieldDescKind <> fdkCalculated) and (Field.DataType = dtMSXML) and WithBlob then begin
      Handle := Marshal.ReadIntPtr(RecBuf, Field.Offset);
      so := TSharedObject(GetGCHandleTarget(Handle));
      b := (so <> nil) and (so.RefCount = 1);
      so.Free;
      if b then
        Marshal.WriteIntPtr(RecBuf, Field.Offset, nil);
    end;
  end;
end;

function TOLEDBRecordSet.GetStatus(FieldNo: word; RecBuf: IntPtr): DWORD;
begin
  Result := DWORD(Marshal.ReadInt32(RecBuf, DataSize + FieldNo{numeration from 0?} * OLE_DB_INDICATOR_SIZE));
end;

procedure TOLEDBRecordSet.SetStatus(FieldNo: word; RecBuf: IntPtr; Value: DWORD);
begin
  Marshal.WriteInt32(RecBuf, DataSize + FieldNo{numeration from 0?} * OLE_DB_INDICATOR_SIZE, Integer(Value));
end;

function TOLEDBRecordSet.GetNull(FieldNo: word; RecBuf: IntPtr): boolean;
begin
  Result := GetStatus(FieldNo - 1 {numeration from 1?}, RecBuf) = DBSTATUS_S_ISNULL;
  if Result then
    Result := GetNullByBlob(FieldNo, RecBuf);
end;

procedure TOLEDBRecordSet.SetNull(FieldNo: word; RecBuf: IntPtr; Value: boolean);
begin
  if Value then
    SetStatus(FieldNo - 1 {numeration from 1?}, RecBuf, DBSTATUS_S_ISNULL)
  else
    SetStatus(FieldNo - 1 {numeration from 1?}, RecBuf, DBSTATUS_S_OK);
end;

procedure TOLEDBRecordSet.GetFieldData(Field: TFieldDesc; FieldBuf: IntPtr; Dest: IntPtr);
begin
  case Field.DataType of
    dtString, dtWideString, dtMemo, dtWideMemo, dtMSXML:
    begin
      if Field.Length = 0 then
        CopyBuffer(FieldBuf, Dest, sizeof(IntPtr) {ISeqStream})
      else
        inherited;
    end;
    dtBlob:
      CopyBuffer(FieldBuf, Dest, sizeof(TBlob));
  else
    inherited;
  end;
end;

procedure TOLEDBRecordSet.GetField(FieldNo: word; RecBuf: IntPtr; Dest: IntPtr; var IsBlank: boolean);
begin
  CheckBCDOverflow(FieldNo, RecBuf);

  inherited;
end;

procedure TOLEDBRecordSet.GetFieldAsVariant(FieldNo: word; RecBuf: IntPtr; var Value: variant);
begin
  CheckBCDOverflow(FieldNo, RecBuf);

  inherited;
end;

procedure TOLEDBRecordSet.PutFieldData(Field: TFieldDesc; FieldBuf: IntPtr; Source: IntPtr; IsDatabaseValue: boolean = False);
begin
  case Field.DataType of
    dtString, dtWideString, dtMemo, dtWideMemo, dtMSXML:
    begin
      if Field.Length = 0 then
        CopyBuffer(Source, FieldBuf, sizeof(IntPtr) {ISeqStream})
      else
        inherited;
    end;
    dtBlob:
      CopyBuffer(Source, FieldBuf, sizeof(TBlob));
  else
    inherited;
  end;
end;

{ Open/Close }

function TOLEDBRecordSet.CursorTypeForFetching: TMSCursorType;
begin
  if (FCursorType = ctBaseTable) and not FReadOnly and FCursorUpdate then
    Result := ctDynamic
  else
    Result := FCursorType;
end;

function TOLEDBRecordSet.ServerCursorUsed: boolean;
begin
  Result := CursorTypeForFetching in ServerCursorTypes;
end;

function TOLEDBRecordSet.NeedInitFieldsOnFetch: boolean;
begin
  Result := FCommand.FIsSProc;
end;

procedure TOLEDBRecordSet.InternalPrepare;
var
  ColumnsInfo: IColumnsInfo;
  cColumns: NativeUInt;
  prgInfo: PDBCOLUMNINFO;
  pStringsBuffer: IntPtr;
  //Malloc: IMalloc;
begin
  QueryCommandInterfaces(True);
  try
    SetCommandProp;
    inherited;
  finally
    ReleaseCommandInterfaces; /// FCommand.QueryIntCnt counter is increased in inherited
  end;

  try
    // Detect CommandType
    if FNativeRowset and not FCommand.FRPCCall then begin
      // If statement is wrong in some cases exception may be occured now
      QueryIntf(FCommand.FICommandPrepare, {$IFDEF CLR}IColumnsInfo{$ELSE}IID_IColumnsInfo{$ENDIF}, ColumnsInfo);
      Assert(ColumnsInfo <> nil);

      pStringsBuffer := nil;
      try
        // If statement is wrong in some cases exception may be occured now
        Check(ColumnsInfo.GetColumnInfo(cColumns, PDBCOLUMNINFO(prgInfo), pStringsBuffer));

        if cColumns > 0 then
          CommandType := ctCursor
        else
          CommandType := ctStatement;
      finally
        FreeCoMem(prgInfo);
        FreeCoMem(pStringsBuffer);
      end;
    end;
  except
    InternalUnPrepare;
    raise;
  end;
end;

procedure TOLEDBRecordSet.InternalUnPrepare;
begin
  try
    inherited;
  finally
    if FNativeRowset then begin
      // ReleaseCommandInterfaces;
      if not FLockClearMultipleResults then begin
        FCommand.ClearIMultipleResults;
        FCommand.FIUnknownNext := nil;
      end;
      FCommand.FIUnknown := nil;
      ReleaseRecordSetInterfaces;
    end;
    CommandType := ctUnknown;
  end;
end;

procedure TOLEDBRecordSet.ClearHRowIfNeed;
{$IFDEF CLR}
var
  rghRows: PHROWArray;
  GCHandle: IntPtr;
{$ENDIF}
begin
  if FHRowAccessible and ServerCursorUsed then begin
    Assert(FIRowset <> nil, 'TOLEDBRecordSet.ClearHRowIfNeed - FIRowset must be setted');

  {$IFDEF CLR}
    GCHandle := AllocGCHandle(TObject(FHRow), True);
    try
      rghRows := GetAddrOfPinnedObject(GCHandle);
      Check(FIRowset.ReleaseRows(1, rghRows, nil, nil, nil));
    finally
      FreeGCHandle(GCHandle);
    end;
  {$ELSE}
    Check(FIRowset.ReleaseRows(1, @FHRow, nil, nil, nil));
  {$ENDIF}
    FHRow := MaxLongint;
    FHRowAccessible := False;
  end;
end;

procedure TOLEDBRecordSet.InsertRecord(RecBuf: IntPtr); 
  procedure AppItem;
  var
    Block: PBlockHeader;
    Item: PItemHeader;
  begin
    Assert(IntPtr(BlockMan.FirstBlock) = nil);

    // Nearly copied from TBlockManager.AddFreeBlock
    BlockMan.AllocBlock(Block, 1);
    Item := PtrOffset(Block, sizeof(TBlockHeader));
    Item.Prev := nil;
    Item.Next := nil;
    Item.Block := Block;
    Item.Flag := flFree;

    BlockMan.FirstFree := Item;
    Block.UsedItems := 0;
    //------------------

    CurrentItem := AppendItem;
  end;

var
  Appended: boolean;
  i: integer;
begin
//  ODS(Format('+ TOLEDBRecordSet.InsertRecord   FI = %d, CI = %d, LI = %d, BOF = %s, EOF = %s', [Integer(FirstItem), Integer(CurrentItem), Integer(LastItem), BoolToStr(BOF, True), BoolToStr(EOF, True)]));
  if FIRowsetUpdate <> nil then begin
    if IntPtr(CurrentItem) = nil then begin
      if IntPtr(FirstItem) <> nil then
        CurrentItem := FirstItem
      else
      if IntPtr(LastItem) <> nil then
        CurrentItem := LastItem;
    end;

    Appended := IntPtr(CurrentItem) = nil;
    if Appended then
      AppItem
    else
      if HasComplexFields then
        FreeComplexFields(PtrOffset(CurrentItem.Block, sizeof(TBlockHeader) + sizeof(TItemHeader)), True);

    InternalAppend(RecBuf);
    PutRecord(RecBuf);
    ReorderItems(CurrentItem, roInsert);

    if FCursorType = ctKeySet then begin
      FirstItem := nil;
      CurrentItem := nil;
      LastItem := nil;

      SetToEnd;
    end
    else begin
      Assert(FCursorType in [ctDynamic, ctBaseTable]);
      if HasBlobFields then
        for i := 0 to FieldCount - 1 do
          if IsBlobFieldType(Fields[i].DataType) then
            TBlob(GetObject(Fields[i].FieldNo, RecBuf)).Commit;

      FirstItem := nil;
      CurrentItem := nil;
      LastItem := nil;

      FLastFetchBack := False;
      SetToEnd;
    end;
  end
  else
  if (FCursorType in ServerCursorTypes) and not FCursorUpdate then  begin
    if FRecordCount = 0 then begin
      InternalAppend(RecBuf);

      AppItem;
      PutRecord(RecBuf);
      ReorderItems(CurrentItem, roInsert);

      CurrentItem := nil;
//      SetToEnd; - no sense for keyset cursors
      FBof := False;
{      SetToEnd;

      FBof := FirstItem = nil;
      FEof := LastItem = nil;}
    end
    else begin
      InternalAppend(RecBuf);

      if HasComplexFields then
        FreeComplexFields(RecBuf, True);
      SetToEnd;
      CurrentItem := LastItem;

      FBof := IntPtr(FirstItem) = nil;
      FEof := IntPtr(LastItem) = nil;
    end;
  end
  else
    inherited;
//  ODS(Format('+ TOLEDBRecordSet.InsertRecord   FI = %d, CI = %d, LI = %d, BOF = %s, EOF = %s', [Integer(FirstItem), Integer(CurrentItem), Integer(LastItem), BoolToStr(BOF, True), BoolToStr(EOF, True)]));
end;

procedure TOLEDBRecordSet.UpdateRecord(RecBuf: IntPtr);
begin
  inherited;
  if (FCursorType in ServerCursorTypes) and FroAfterUpdate then
    FetchToBookmarkValue;
end;

procedure TOLEDBRecordSet.DeleteRecord;
begin
  if (FIRowsetUpdate <> nil)
    or ((FCursorType in ServerCursorTypes) and not FCursorUpdate) then begin
    InternalDelete;
    RemoveRecord;
    if FCursorType in [ctDynamic, ctBaseTable] then
      FRecordCount := - 1;

    Fetch;
    CurrentItem := FirstItem;

    if IntPtr(CurrentItem) = nil then begin
      Fetch(True);
      CurrentItem := FirstItem;
    end;
  end
  else
    inherited;
end;

procedure TOLEDBRecordSet.SetIndexFieldNames(Value: _string);
begin
  if ServerCursorUsed and not CachedUpdates then // for TableTypeRecordSet
    DatabaseError(SLocalSortingServerCursor);

  inherited SetIndexFieldNames(Value);
end;

procedure TOLEDBRecordSet.SetToBegin;
begin
  case CursorTypeForFetching of
    ctStatic, ctKeyset:
      try
        FFetchFromBookmark := True;
        FBookmarkSize := sizeof(byte);
        FBookmarkValue := DBBMK_FIRST;
        Fetch;
      finally
        FFetchFromBookmark := False;
        FBookmarkSize := sizeof(FBookmarkValue);
      end;
    ctDynamic:
      while Fetch(True) do;
  end;
  inherited;
end;

procedure TOLEDBRecordSet.SetToEnd; 
begin
  if FCursorType in [ctStatic, ctKeyset] then
    try
      FFetchFromBookmark := True;
      FBookmarkSize := sizeof(byte);
      FBookmarkValue := DBBMK_LAST;
      Fetch;
    finally
      FFetchFromBookmark := False;
      FBookmarkSize := sizeof(FBookmarkValue);
    end
  else
    FetchAll;
  inherited;
end;

function TOLEDBRecordSet.FetchToBookmarkValue(FetchBack: boolean = False): boolean; // Fetch to Bookmark. Bookmark value is stored in FBookmarkValue. Bookmark value used only for ctStatic and ctKeyset. For ctDynamic method refetched current record in specified direction
begin
  Assert(ServerCursorUsed);
  try
    FFetchFromBookmark := True;
    Result := Fetch(FetchBack);
  finally
    FFetchFromBookmark := False;
  end;
end;

procedure TOLEDBRecordSet.SetToBookmark(Bookmark: PRecBookmark);
begin
  if FCursorType in [ctStatic, ctKeyset] then begin
    // Cannot optimize - used to RefreshRecord
    // if (FBookmarkValue <> Bookmark.Order) or (CurrentItem = nil) then begin
    FBookmarkValue := Bookmark.Order;
    FetchToBookmarkValue;
  end;

  inherited;
end;

function TOLEDBRecordSet.CompareBookmarks(Bookmark1, Bookmark2: PRecBookmark): integer;
const
  RetCodes: array[Boolean, Boolean] of ShortInt = ((2,-1), (1,0));
begin
  if FCursorType in [ctKeyset, ctStatic] then begin
    // Copied from TData.CompareBookmarks
    Result := RetCodes[IntPtr(Bookmark1) = nil, IntPtr(Bookmark2) = nil];
    if Result = 2 then begin
      if Bookmark1.Order >= Bookmark2.Order then
        if Bookmark1.Order = Bookmark2.Order then
          Result := 0
        else
          Result := 1
      else
        Result := -1
    end;
  end
  else
    Result := inherited CompareBookmarks(Bookmark1, Bookmark2);

end;

function TOLEDBRecordSet.CanFetchBack: boolean;
begin
  Result := ServerCursorUsed;
end;

procedure TOLEDBRecordSet.DoFetchTerminate(Sender: TObject); // MainThread context
begin
  EndFetchAll(FFetchExecutor.FException);
  FFetchExecutor := nil;
end;

procedure TOLEDBRecordSet.DoFetchException(Sender: TObject; E: Exception; var Fail: boolean); // MainThread context
begin
  if (E is EOLEDBError) then
    FCommand.FConnection.DoError(EOLEDBError(E), Fail);
end;

const
  FE_AFTERFETCH = 1;

procedure TOLEDBRecordSet.DoFetchSendEvent(Sender: TObject; Event: TObject);
begin
  if Integer(Event) = FE_AFTERFETCH then
    DoAfterFetch;
end;

procedure TOLEDBRecordSet.FetchAll;
begin
  if (FCommand.GetCursorState < csFetchingAll) or (CursorTypeForFetching = ctDynamic) then begin
    FCommand.SetCursorState(csFetchingAll);
    if FCommand.FNonBlocking then begin
      FFetchExecutor := TOLEDBThreadWrapper.Create(TExecuteThread, True);
      TExecuteThread(FFetchExecutor.FThread).FRunMethod := DoFetchAll;
      FFetchExecutor.OnException := DoFetchException;
      FFetchExecutor.OnTerminate := DoFetchTerminate;
      FFetchExecutor.OnSendEvent := DoFetchSendEvent;
      FFetchExecutor.FreeOnTerminate := True;
      StringHeap.ThreadSafety := True;
      // FFetchExecutor.Resume; // moved to TCustomMSDataSet.SetActive
    end
    else
      DoFetchAll;
  end;
end;

procedure TOLEDBRecordSet.DoFetchAll; // FFetchExecutor.FThread context
begin
  FFetching := True;
  try
    while Fetch do
      if (FFetchExecutor <> nil) and FWaitForFetchBreak then
        Break;
  finally
    FFetching := False;
  end;
end;

procedure TOLEDBRecordSet.EndFetchAll(E: Exception); // MainThread context
begin
  if FCommand.FNonBlocking then
    StringHeap.ThreadSafety := True;
end;

procedure TOLEDBRecordSet.BreakFetch;
begin
  inherited;

  if FCommand.FNonBlocking and (FFetchExecutor <> nil) and not FFetchExecutor.Thread.Suspended then
    FFetchExecutor.Terminate;
end;

function TOLEDBRecordSet.CanDisconnect: boolean;
begin
  Result := inherited CanDisconnect
    and (FCommand.FIUnknown = nil)
    and (FIRowset = nil)
    and (FCommand.FIUnknownNext = nil)
    and (FCommand.FIMultipleResults = nil);
end;

function TOLEDBRecordSet.RowsReturn: boolean;
begin
  if CommandType <> ctUnknown then
    Result := inherited RowsReturn
  else                              //we need to know this info even if CommandType is not set(TCustomDADataSet.DoAfterExecute)
    Result := (FCommand.FIUnknown <> nil) or (FIRowset <> nil);
end;

procedure TOLEDBRecordSet.RowsetUpdateCommit;
const
  RowHeader = '%s (Status = %Xh).';
var
{$IFDEF CLR}
  rghRows: PHROWArray;
  GCHandle: IntPtr;
{$ENDIF}
  pRowStatus: PDBRowStatus;
  RowStatus: DBRowStatus;
  Msg: WideString;
  RowsetUpdate: IRowsetUpdate;
begin
  if not Supports(FIRowsetUpdate, {$IFDEF CLR}IRowsetUpdate{$ELSE}IID_IRowsetUpdate{$ENDIF}, RowsetUpdate) then
    Exit;

  try
  {$IFDEF CLR}
    GCHandle := AllocGCHandle(TObject(FHRow), True);
    try
      rghRows := GetAddrOfPinnedObject(GCHandle);
      Check(IRowsetUpdate(FIRowsetUpdate).Update(DB_NULL_HCHAPTER, 1, rghRows, nil, nil, pRowStatus));
    finally
      FreeGCHandle(GCHandle);
    end;
  {$ELSE}
    Check(IRowsetUpdate(FIRowsetUpdate).Update(DB_NULL_HCHAPTER, 1, @FHRow, nil, nil, pRowStatus));
  {$ENDIF}
  except
    on e: Exception do begin
      if pRowStatus <> nil then begin
        Msg := '';
        RowStatus := DBRowStatus(Marshal.ReadInt32(pRowStatus));
        case RowStatus of
          DBROWSTATUS_S_OK:;
          DBROWSTATUS_S_MULTIPLECHANGES:
            AddInfoToErr(Msg, RowHeader, [SRowMultipleChanges, RowStatus]);
          DBROWSTATUS_S_PENDINGCHANGES:
            AddInfoToErr(Msg, RowHeader, [SRowPendingChanges, RowStatus]);
          DBROWSTATUS_E_CANCELED:
            AddInfoToErr(Msg, RowHeader, [SRowCanceled, RowStatus]);
          //DBROWSTATUS_E_CANTRELEASE = $00000006;
          DBROWSTATUS_E_CONCURRENCYVIOLATION:
            AddInfoToErr(Msg, RowHeader, [SRowConcurrencyViolation, RowStatus]);
          DBROWSTATUS_E_DELETED:
            AddInfoToErr(Msg, RowHeader, [SRowDeleted, RowStatus]);
          //DBROWSTATUS_E_PENDINGINSERT = $00000009;
          //DBROWSTATUS_E_NEWLYINSERTED = $0000000A;
          DBROWSTATUS_E_INTEGRITYVIOLATION:
            AddInfoToErr(Msg, RowHeader, [SRowIntegrityViolation, RowStatus]);
          DBROWSTATUS_E_INVALID:
            Assert(False);
          //DBROWSTATUS_E_MAXPENDCHANGESEXCEEDED = $0000000D;
          //DBROWSTATUS_E_OBJECTOPEN = $0000000E;
          //DBROWSTATUS_E_OUTOFMEMORY = $0000000F;
          DBROWSTATUS_E_PERMISSIONDENIED:
            AddInfoToErr(Msg, RowHeader, [SRowPermissionDenied, RowStatus]);
          DBROWSTATUS_E_LIMITREACHED:
            AddInfoToErr(Msg, RowHeader, [SRowLimitReached, RowStatus]);
          DBROWSTATUS_E_SCHEMAVIOLATION:
            AddInfoToErr(Msg, RowHeader, [SRowSchemaViolation, RowStatus]);
          DBROWSTATUS_E_FAIL:
            AddInfoToErr(Msg, RowHeader, [SRowFail, RowStatus]);
        else
          AddInfoToErr(Msg, RowHeader, [SUnknownStatus, RowStatus]);
        end;

        FreeCoMem(pRowStatus);
        AddInfoToErr(E, Msg, []);
      end;

      // -----
      RowsetUpdateRollback;
      // -----}

      raise E;
    end;
  end;
end;

procedure TOLEDBRecordSet.RowsetUpdateRollback;
var
{$IFDEF CLR}
  rghRows: PHROWArray;
  GCHandle: IntPtr;
{$ENDIF}
  RowsetUpdate: IRowsetUpdate;
begin
  if not Supports(FIRowsetUpdate, {$IFDEF CLR}IRowsetUpdate{$ELSE}IID_IRowsetUpdate{$ENDIF}, RowsetUpdate) then
    Exit;

{$IFDEF CLR}
  GCHandle := AllocGCHandle(TObject(FHRow), True);
  try
    rghRows := GetAddrOfPinnedObject(GCHandle);
    Check(IRowsetUpdate(FIRowsetUpdate).Undo(DB_NULL_HCHAPTER, 1, rghRows, nil, nil, nil));
  finally
    FreeGCHandle(GCHandle);
  end;
{$ELSE}
  Check(IRowsetUpdate(FIRowsetUpdate).Undo(DB_NULL_HCHAPTER, 1, @FHRow, nil, nil, nil));
{$ENDIF}
//Assert(cRows = 1);

{ Check(FIRowsetUpdate.GetRowStatus(DB_NULL_HCHAPTER, 1, @FHRow, @pRowStatus));
  FHRowAccessible := True; ClearHRowIfNeed;

  Check(FIRowsetUpdate.GetRowStatus(DB_NULL_HCHAPTER, 1, @FHRow, @pRowStatus));
  FHRowAccessible := True; ClearHRowIfNeed;
//Check(FIRowsetUpdate.GetRowStatus(DB_NULL_HCHAPTER, 1, @FHRow, @pRowStatus));
}
end;

procedure TOLEDBRecordSet.InternalAppend(RecBuf: IntPtr);
begin
  if (FIRowsetUpdate <> nil) then begin
    ClearHRowIfNeed;
    InternalAppendOrUpdate(RecBuf, True);
  end
  else
    inherited;

  if FCursorType = ctKeySet then
    Inc(FRecordCount);
end;

procedure TOLEDBRecordSet.InternalDelete;
var
  hr: HResult;
{$IFDEF CLR}
  rghRows: PHROWArray;
  GCHandle: IntPtr;
{$ENDIF}
begin
  if FIRowsetUpdate = nil then
    inherited
  else begin
    Assert(FIRowsetUpdate <> nil, 'FCommand.FIRowsetUpdate must be setted');
    Assert(FHRowAccessible, 'FHRow must be accessible');

  {$IFDEF CLR}
    GCHandle := AllocGCHandle(TObject(FHRow), True);
    try
      rghRows := GetAddrOfPinnedObject(GCHandle);
      hr := FIRowsetUpdate.DeleteRows(DB_NULL_HCHAPTER, 1, rghRows, nil);
    finally
      FreeGCHandle(GCHandle);
    end;
  {$ELSE}
    hr := FIRowsetUpdate.DeleteRows(DB_NULL_HCHAPTER, 1, @FHRow, nil);
  {$ENDIF}
    Check(hr);
    RowsetUpdateCommit;
  end;
end;

procedure TOLEDBRecordSet.InternalUpdate(RecBuf: IntPtr);
begin
  if FIRowsetUpdate = nil then
    inherited
  else begin
    Assert(FHRowAccessible, 'FHRow must be accessible');
    InternalAppendOrUpdate(RecBuf, False);
  end;
end;

procedure TOLEDBRecordSet.InternalAppendOrUpdate(RecBuf: IntPtr; const IsAppend: boolean);
var
  StreamList: TList;

  procedure SetDataToRow(const Row: HROW; const pRec: IntPtr);
    procedure PrepareConvertableFields; // Server Cursors. Before send data to OLEDB
    var
      i: integer;
      pValue: IntPtr;
      HeapBuf: IntPtr;
      Size: integer;
      Field: TFieldDesc;
      dt: TDateTime;
      AYear, AMonth, ADay, AHour, AMinute, ASecond, AMilliSecond: Word;
      DBTimeStamp: TDBTimeStamp;
    {$IFDEF VER5P}
      g: TGUID;
    {$ENDIF}
    {$IFDEF VER6P}
      Bcd: TBcd;
      s: AnsiString;
      DotPos, l: integer;
    {$ENDIF}
    {$IFDEF CLR}
      BcdBuf: TBytes;
      d: double;
      i64: Int64;
    {$ENDIF}
    begin
    {$IFDEF VER8}
      SetLength(BcdBuf, 0); // To avoid compiler warning. Delphi8 bug.
    {$ENDIF}
      for i := 0 to Fields.Count - 1 do begin
        Field := Fields[i];
        if Field.FieldDescKind = fdkData then begin
          pValue := PtrOffset(pRec, Field.Offset);
          case Field.DataType of
            dtBytes:
              Marshal.{$IFDEF WIN64}WriteInt64{$ELSE}WriteInt32{$ENDIF}(pValue, Field.Length, Field.Length);
            dtVarBytes:
              Marshal.{$IFDEF WIN64}WriteInt64{$ELSE}WriteInt32{$ENDIF}(pValue, sizeof(word) + Field.Length, Marshal.ReadInt16(pRec, Field.Offset));
            dtDateTime:
              if (Provider = prCompact) and not GetNull(i + 1, pRec) then begin
                dt := BitConverter.Int64BitsToDouble(Marshal.ReadInt64(pValue));
                DecodeDateTime(dt, AYear, AMonth, ADay, AHour, AMinute, ASecond, AMilliSecond);
                DBTimeStamp.year := AYear;
                DBTimeStamp.month := AMonth;
                DBTimeStamp.day := ADay;
                DBTimeStamp.hour := AHour;
                DBTimeStamp.minute := AMinute;
                DBTimeStamp.second := ASecond;
                DBTimeStamp.fraction := AMilliSecond * 1000000 ; // milliseconds to billionths of a second
              {$IFDEF CLR}
                Marshal.StructureToPtr(TObject(DBTimeStamp), pValue, False);
              {$ELSE}
                PDBTimeStamp(pValue)^ := DBTimeStamp;
              {$ENDIF}
              end;
            dtCurrency:
              if (Provider = prCompact) and not GetNull(i + 1, pRec) then begin
              {$IFDEF CLR}
                d := BitConverter.Int64BitsToDouble(Marshal.ReadInt64(pValue));
                d := d * 10000;
                i64 := Convert.ToInt64(d);
                Marshal.WriteInt64(pValue, i64);
              {$ELSE}
                PCurrency(pValue)^ := PDouble(pValue)^;
              {$ENDIF}
              end;
            dtExtVarBytes:
              if not GetNull(i + 1, pRec) then begin
                HeapBuf := Marshal.ReadIntPtr(pValue); //HeapBuf := IntPtr(IntPtr(pValue)^);
                Size := Marshal.ReadInt16(HeapBuf);
                Marshal.WriteInt32(pValue, sizeof(IntPtr {IntPtr to OLEDBBuf/StringHeap}), Size);
                Marshal.WriteIntPtr(pValue, PtrOffset(HeapBuf, sizeof(word)));
              end;
          {$IFDEF VER6P}
          {$IFNDEF FPC}
            dtFmtBCD: begin
            {$IFDEF CLR}
              Marshal.Copy(pValue, BcdBuf, 0, 34);
              Bcd := TBcd.FromBytes(BcdBuf);
            {$ELSE}
              Bcd := PBcd(pValue)^;
            {$ENDIF}
              // DBNumeric
              s := AnsiString(BcdToStr(Bcd));
              if DecimalSeparator <> '.' then begin
                DotPos := Pos(AnsiString(DecimalSeparator), s);
                if DotPos <> 0 then
                  s[DotPos] := '.';
              end;
              l := Length(s);
              // Marshal.WriteInt32(pLength, l);

              if l > 0 then
                CopyBufferAnsi(s, pValue, l + 1{#0});
            end;
          {$ENDIF}
          {$ENDIF}
          {$IFDEF VER5P}
            dtGuid:
              if not GetNull(i + 1, pRec) then begin
                g := StringToGUID(string(Marshal.PtrToStringAnsi(pValue)));
              {$IFDEF CLR}
                Marshal.StructureToPtr(TObject(g), pValue, False);
              {$ELSE}
                CopyBuffer(@g, pValue, sizeof(g));
              {$ENDIF}
              end;
          {$ENDIF}
          end;
        end;
      end;
    end;

    procedure PostPlainAccessorBlock(
      const AccessorBlock: TAccessorBlock;
      out NeedToPost: boolean // Used to skip unchanged BLOB fields
    );
      procedure ConvertBlobToStream(BlobField: TFieldDesc; const pValue: IntPtr);
      var
        OLEDBStream: TOLEDBStream;
        Blob: TBlob;
        pUnk: IntPtr;
      begin
        Blob := TBlob(GetGCHandleTarget(Marshal.ReadIntPtr(pValue)));

        // Assert(FCursorType in [ctKeyset, ctDynamic] and FCursorUpdate);
        NeedToPost := Blob.CanRollback or (FCursorType = TMSCursorType(ctTableType));

        if NeedToPost then begin
          // Create stream
          OLEDBStream := TOLEDBStream.Create(Blob, StreamList);
          pUnk := Marshal.GetIUnknownForObject(OLEDBStream);
          Marshal.AddRef(pUnk);
          Marshal.WriteIntPtr(pValue, pUnk);

          // Set stream size
          Marshal.WriteInt32(pRec, BlobField.Offset + sizeof(IntPtr{ISeqStream}), OLEDBStream.Size);
        end;
      end;

    var
      pValue: IntPtr;
      BlobField: TFieldDesc;
    begin
      NeedToPost := True;
      if AccessorBlock.BlobFieldNum = -1 then
        Exit;

      BlobField := Fields[AccessorBlock.BlobFieldNum];
      pValue := PtrOffset(pRec, BlobField.Offset);
      if not GetNull(AccessorBlock.BlobFieldNum + 1, pRec) then
        ConvertBlobToStream(BlobField, pValue);
    end;

  var
    FetchBlockOffset: integer;

    procedure PostExternalAccessorBlock(const AccessorBlock: TAccessorBlock);
    var
      i, l, p: integer;
      Blob: TSharedObject;
      pValue, pc: IntPtr;
      pov: POleVariant;

      Field: TFieldDesc;
      FieldNum: integer;
      ServerVersion: integer;
    begin
      Assert(AccessorBlock.BlobFieldNum = - 1);
      ServerVersion := ProviderPrimaryVer;
      
      for i := 0 to Length(AccessorBlock.FieldNums) - 1 do begin
        FieldNum := AccessorBlock.FieldNums[i];
        Field := Fields[FieldNum];
        if IsNeedFetchBlock(Field, ServerVersion) then begin
          pValue := PtrOffset(pRec, Field.Offset);
          case Field.DataType of
            dtMemo, dtWideMemo, dtMSXML: begin
              Blob := TSharedObject(GetGCHandleTarget(Marshal.ReadIntPtr(pValue)));
              pc := PtrOffset(FFetchBlock, FetchBlockOffset + OLE_DB_INDICATOR_SIZE);

              p := 0;
              if not GetNull(FieldNum + 1, pRec) then begin
                l := TBlob(Blob).Size;
                if l > MaxNonBlobFieldLen then // see IncFetchBlockOffset
                  l := MaxNonBlobFieldLen;

                if l > 0 then
                  p := TBlob(Blob).Read(0, l, pc);
              end;
              Marshal.WriteByte(pc, p, 0{#0});
              if TBlob(Blob).IsUnicode then
                Marshal.WriteByte(pc, p + 1, 0{#0});
            end;
            dtVariant: begin
              Blob := TSharedObject(GetGCHandleTarget(Marshal.ReadIntPtr(pValue)));
              pov := POleVariant(PtrOffset(FFetchBlock, FetchBlockOffset + OLE_DB_INDICATOR_SIZE));
              Marshal.WriteInt16(pov, varEmpty); // TVarData(pov^).VType := varEmpty;
              SetOleVariant(pov, TVariantObject(Blob).Value);
            end;
          else
            Assert(False);
          end;
          IncFetchBlockOffset(FetchBlockOffset, Field);
        end;
      end;
    end;

    procedure CopyStatusFBlock(const ToFBlock: boolean); // if ToFBlock is True then status is copied from pRec to FetchBlock. Otherwise - from FetchBlock to pRec
    var
      i: integer;
      FetchBlockOffset: integer;
      Status: DWORD;
    begin
      FetchBlockOffset := 0;
      for i := 0 to Fields.Count - 1 do
        if (Fields[i].FieldDescKind = fdkData) and IsNeedFetchBlock(Fields[i], ProviderPrimaryVer) then begin
          if ToFBlock then begin
            Status := GetStatus(i, pRec);
            Marshal.WriteInt32(FFetchBlock, FetchBlockOffset, Integer(Status));
          end
          else begin
            Status := DWORD(Marshal.ReadInt32(FFetchBlock, FetchBlockOffset));
            SetStatus(i, pRec, Status);
          end;
          IncFetchBlockOffset(FetchBlockOffset, Fields[i]);
        end;
    end;

  var
    AccNum: integer;
    hr: HResult;
    NeedToPost: boolean;
  begin
    PrepareConvertableFields;

    FetchBlockOffset := 0;
    try
      for AccNum := 0 to Length(FFetchAccessorData.AccessorBlocks) - 1 do begin
        Assert((Length(FFetchAccessorData.AccessorBlocks[AccNum].FieldNums) = 1) // CR 4082
          or (FCursorType = TMSCursorType(ctTableType)));
        if (FCursorType = TMSCursorType(ctTableType)) // for TableTypeRecordSet
          or not IsAppend // Edit
          or not GetNull(FFetchAccessorData.AccessorBlocks[AccNum].FieldNums[0] + 1, pRec)
          or (FFetchAccessorData.AccessorBlocks[AccNum].BlockType = abBlob) then begin
          case FFetchAccessorData.AccessorBlocks[AccNum].BlockType of
            abFetchBlock: begin
              // Prepare data
              CopyStatusFBlock(True);
              PostExternalAccessorBlock(FFetchAccessorData.AccessorBlocks[AccNum]);
              // Set data to IRowset
              if FHRowAccessible then
                hr := FIRowsetUpdate.SetData(FHRow, FFetchAccessorData.AccessorBlocks[AccNum].hAcc, FFetchBlock)
              else
                hr := FIRowsetUpdate.InsertRow(DB_NULL_HCHAPTER, FFetchAccessorData.AccessorBlocks[AccNum].hAcc, FFetchBlock, FHRow);
              CopyStatusFBlock(False);

              // Analyze OLE DB result
              Check(hr, AnalyzeFieldsStatus, pRec);
              FHRowAccessible := True;
            end;
            abOrdinary, abBLOB: begin
              // Prepare data
              PostPlainAccessorBlock(FFetchAccessorData.AccessorBlocks[AccNum], NeedToPost);
              if NeedToPost then begin
                // Set data to IRowset
                if FHRowAccessible then
                  hr := FIRowsetUpdate.SetData(FHRow, FFetchAccessorData.AccessorBlocks[AccNum].hAcc, pRec)
                else
                  hr := FIRowsetUpdate.InsertRow(DB_NULL_HCHAPTER, FFetchAccessorData.AccessorBlocks[AccNum].hAcc, pRec, FHRow);

                // Analyze OLE DB result
                Check(hr, AnalyzeFieldsStatus, pRec);
                FHRowAccessible := True;
              end;
            end;
            abReadOnly:;
          end;
        end;
      end;
    except
      if FHRowAccessible then
        RowsetUpdateRollback;
      raise;
    end;

    if FHRowAccessible then
      RowsetUpdateCommit;
  end;

var
  pRec, pRecOld: IntPtr;
  i: integer;
  OLEDBStream: TOLEDBStream;
begin
  Assert(FIRowsetUpdate <> nil, 'FCommand.FIRowsetUpdate must be setted');

  StreamList := nil;
  pRecOld := RecBuf;

  /// Store old values to prevent conversion. Blob fields is not stored
  pRec := Marshal.AllocHGlobal(RecordSize);
  try
    StreamList := TList.Create;
    CopyBuffer(pRecOld, pRec, RecordSize);
    SetDataToRow(FHRow, pRec);
  finally
    Marshal.FreeHGlobal(pRec);

    // Remove streams
    for i := StreamList.Count - 1 downto 0 do begin
      OLEDBStream := TOLEDBStream(StreamList[i]);
    {$IFDEF CLR}
      OLEDBStream.Free;
    {$ELSE}
      OLEDBStream._Release;
    {$ENDIF}
      if (GUIDToString(ProviderId) = GUIDToString(CLSID_SQLNCLI)) or
         (GUIDToString(ProviderId) = GUIDToString(CLSID_SQLNCLI10)) or
         (ProviderPrimaryVer = 6) then
        OLEDBStream.FStreamList := nil;
    end;
    Assert((StreamList.Count = 0) or (GUIDToString(ProviderId) = GUIDToString(CLSID_SQLNCLI)) or (GUIDToString(ProviderId) = GUIDToString(CLSID_SQLNCLI10)) or (ProviderPrimaryVer = 6), 'StreamList.Count = ' + IntToStr(StreamList.Count));
    StreamList.Free;
  end;
end;

procedure TOLEDBRecordSet.SetCommandProp;
var
  OLEDBProperties: TOLEDBPropertiesSet;
  IRowsetUpdateRequired: boolean;
  IsSQLEverywhere: boolean;
begin
{$IFDEF AUTOTEST}
  Inc(__SetRecordSetCommandPropCount);
{$ENDIF}

  Assert(FNativeRowset, 'FNativeRowset must be True');
  Assert(FCommand.FIMultipleResults = nil, 'FCommand.FIMultipleResults must be nil');

  IsSQLEverywhere := Provider = prCompact;
  if FCursorType in ServerCursorTypes then begin
    // Server cursor has no sence in disconnected mode
    if DisconnectedMode then
      DatabaseError(SDMandServerCursors);
    FFetchRows := 1; /// !!!
    FFetchAll := False;
    FCommand.FRequestMultipleResults := False;
  end
  else
    FCommand.FRequestMultipleResults := not IsSQLEverywhere and not FDisableMultipleResults;

  IRowsetUpdateRequired := (ProviderPrimaryVer = 7);

  OLEDBProperties := TOLEDBPropertiesSet.Create(FCommand.FConnection, DBPROPSET_ROWSET);
  try
    with OLEDBProperties do begin
      // AddPropInt(DBPROP_ACCESSORDER, DBPROPVAL_AO_SEQUENTIALSTORAGEOBJECTS); - no performance improvement
      AddPropInt(DBPROP_ACCESSORDER, DBPROPVAL_AO_RANDOM);

      case FCursorType of
        ctDefaultResultSet:
        begin
          if not IsSQLEverywhere then begin
            if FCommand.FNotification then
              FCommand.FDelayedSubsciption := FUniqueRecords or not FReadOnly;
            AddPropBool(DBPROP_UNIQUEROWS, FUniqueRecords or not FReadOnly);
          end;
          AddPropBool(DBPROP_IColumnsRowset, FRequestSQLObjects);
          if not IsSQLEverywhere then begin
            AddPropBool(DBPROP_IMultipleResults, FCommand.FRequestMultipleResults);
            AddPropBool(DBPROP_SERVERCURSOR, False);
          end;
          AddPropBool(DBPROP_OWNINSERT, False);
          AddPropBool(DBPROP_OTHERINSERT, False);
          AddPropBool(DBPROP_OTHERUPDATEDELETE, False);
          AddPropBool(DBPROP_OWNUPDATEDELETE, False);
          AddPropBool(DBPROP_IRowsetChange, False);
          AddPropBool(DBPROP_IRowsetUpdate, False);
        end;
        ctStatic:
        begin
          {Static RO}
          AddPropBool(DBPROP_SERVERCURSOR, True);
          AddPropBool(DBPROP_IRowsetChange, False);
          AddPropBool(DBPROP_IRowsetUpdate, False);
          AddPropBool(DBPROP_OTHERINSERT, False);
          AddPropBool(DBPROP_OTHERUPDATEDELETE, False);
          AddPropBool(DBPROP_OWNINSERT, False);
          AddPropBool(DBPROP_OWNUPDATEDELETE, False);
          AddPropBool(DBPROP_REMOVEDELETED, False);
          AddPropBool(DBPROP_IRowsetResynch, False);
          AddPropBool(DBPROP_CHANGEINSERTEDROWS, False);
          AddPropBool(DBPROP_SERVERDATAONINSERT, False);
          AddPropBool(DBPROP_UNIQUEROWS, False);
          AddPropBool(DBPROP_CANFETCHBACKWARDS, not FUniDirectional);

          // Bookmarks
          AddPropBool(DBPROP_IRowsetLocate, True);
          AddPropBool(DBPROP_BOOKMARKS, True);

          // Transactions support
          AddPropBool(DBPROP_COMMITPRESERVE, True);
          AddPropBool(DBPROP_ABORTPRESERVE, True);
        end;
        ctKeyset:
        begin
          {Keyset}
          AddPropBool(DBPROP_SERVERCURSOR, True);
          AddPropBool(DBPROP_OTHERINSERT, False);
          AddPropBool(DBPROP_OTHERUPDATEDELETE, True);
          AddPropBool(DBPROP_OWNINSERT, True);
          AddPropBool(DBPROP_OWNUPDATEDELETE, True);
          AddPropBool(DBPROP_UNIQUEROWS, False);
          AddPropBool(DBPROP_IMMOBILEROWS, True);
          AddPropBool(DBPROP_CANFETCHBACKWARDS, not FUniDirectional);

          // Bookmarks
          AddPropBool(DBPROP_IRowsetLocate, True);
          AddPropBool(DBPROP_BOOKMARKS, True);
          AddPropBool(DBPROP_REMOVEDELETED, True);

          // RO or RW cursor
          AddPropBool(DBPROP_IRowsetUpdate, not FReadOnly and FCursorUpdate, IRowsetUpdateRequired);
          AddPropBool(DBPROP_CHANGEINSERTEDROWS, True);

          // Transactions support
          AddPropBool(DBPROP_COMMITPRESERVE, True);
          AddPropBool(DBPROP_ABORTPRESERVE, True);
        end;
        ctDynamic:
        begin
          {Dynamic}
          AddPropBool(DBPROP_SERVERCURSOR, True);
          AddPropBool(DBPROP_CANHOLDROWS, False);
          AddPropBool(DBPROP_OTHERINSERT, True);
          AddPropBool(DBPROP_OTHERUPDATEDELETE, True);
          AddPropBool(DBPROP_OWNINSERT, True);
          AddPropBool(DBPROP_OWNUPDATEDELETE, True);
          AddPropBool(DBPROP_REMOVEDELETED, True);
          AddPropBool(DBPROP_CHANGEINSERTEDROWS, False);
          AddPropBool(DBPROP_SERVERDATAONINSERT, False);
          AddPropBool(DBPROP_UNIQUEROWS, False);
          AddPropBool(DBPROP_IMMOBILEROWS, False);
          AddPropBool(DBPROP_CANFETCHBACKWARDS, not FUniDirectional);

          // Bookmarks
          AddPropBool(DBPROP_IRowsetLocate, False);
          AddPropBool(DBPROP_IRowsetScroll, False);
          AddPropBool(DBPROP_BOOKMARKS, False);
          AddPropBool(DBPROP_LITERALBOOKMARKS, False);

          // RO or RW cursor
          AddPropBool(DBPROP_IRowsetUpdate, not FReadOnly and FCursorUpdate, IRowsetUpdateRequired);

          // Transactions support
          AddPropBool(DBPROP_COMMITPRESERVE, True);
          AddPropBool(DBPROP_ABORTPRESERVE, True);
        end;
      end;
    end;
    OLEDBProperties.SetProperties(FCommand.FICommandProperties);
  finally
    OLEDBProperties.Free;
  end;
end;

procedure TOLEDBRecordSet.QueryCommandInterfaces(const QueryPrepare: boolean); // Create ConnectionSwap if need. Call FCommand.QueryInterfaces.
begin
  FCommand.QueryInterfaces(QueryPrepare);
end;

procedure TOLEDBRecordSet.ReleaseCommandInterfaces;
begin
  FCommand.ReleaseInterfaces;
end;
    
procedure TOLEDBRecordSet.QueryRecordSetInterfaces;
{  procedure GetProperties;
  const
    PropSetCnt = 1;
    PropCnt = 12;
  var
    RowsetInfo: IRowsetInfo;

    rgPropertyIDSets: PDBPropIDSetArray;
    DBPropIDArray: array[0..PropCnt - 1] of DBPROPID;

    rgPropertySets: PDBPropSet;
    cPropertySets: UINT;
    b: boolean;
    i: integer;
    st: DBPROPSTATUS;
  begin
    FIRowset.QueryInterface(IID_IRowsetInfo, RowsetInfo);

    DBPropIDArray[0] := DBPROP_SERVERCURSOR;
    DBPropIDArray[1] := DBPROP_IRowsetChange;
    DBPropIDArray[2] := DBPROP_IRowsetUpdate;
    DBPropIDArray[3] := DBPROP_OWNINSERT;
    DBPropIDArray[4] := DBPROP_OTHERINSERT;
    DBPropIDArray[5] := DBPROP_OTHERUPDATEDELETE;
    DBPropIDArray[6] := DBPROP_OWNUPDATEDELETE;
    DBPropIDArray[7] := DBPROP_REMOVEDELETED;
    DBPropIDArray[8] := DBPROP_IRowsetResynch;
    DBPropIDArray[9] := DBPROP_CHANGEINSERTEDROWS;
    DBPropIDArray[10] := DBPROP_SERVERDATAONINSERT;
    DBPropIDArray[11] := DBPROP_UNIQUEROWS;

    GetMem1(rgPropertyIDSets, PropSetCnt * sizeof(DBPropIDSet));
    try
      rgPropertyIDSets[0].guidPropertySet := DBPROPSET_ROWSET;
      rgPropertyIDSets[0].cPropertyIDs := PropCnt;
      rgPropertyIDSets[0].rgPropertyIDs := @DBPropIDArray;

      Check(RowsetInfo.GetProperties(PropSetCnt, rgPropertyIDSets,
        cPropertySets, PDBPropSet(rgPropertySets)));

      Assert(rgPropertySets <> nil, 'Cannot get properties');

      for i := 0 to PropCnt - 1 do
      begin
        b := rgPropertySets.rgProperties[i].vValue;
        st := rgPropertySets.rgProperties[i].dwStatus;
        b := not b;
      end;

    finally
      FreeMem1(rgPropertyIDSets);
      FCommand.FConnection.Malloc.Free(rgPropertySets.rgProperties);
      FCommand.FConnection.Malloc.Free(rgPropertySets);
    end;
  end;
}
begin
  try
    if FNativeRowset then begin
      Assert(FIRowset = nil, 'Duplicate call to TOLEDBRecordSet.QueryRecordSetInterfaces');
      Assert(FCommand.FIUnknown <> nil, 'FCommand.FIUnknown must be setted');

      QueryIntf(FCommand.FIUnknown, {$IFDEF CLR}IRowset{$ELSE}IID_IRowset{$ENDIF}, FIRowset);

      if FCursorType in [ctKeyset, ctStatic] then begin
        try
          QueryIntf(FIRowset, {$IFDEF CLR}IRowsetLocate{$ELSE}IID_IRowsetLocate{$ENDIF}, FIRowsetLocate);
        finally
          if FIRowsetLocate = nil then
            DatabaseError(SRecordsetBookmarksRequired);
        end;
      end;

      {if (FCursorType in [ctKeyset, ctDynamic]) then
        GetProperties;}
      if (FCursorType in [ctKeyset, ctDynamic])
        and not FReadOnly
        and FCursorUpdate then
        QueryIntf(FIRowset, {$IFDEF CLR}IRowsetUpdate{$ELSE}IID_IRowsetUpdate{$ENDIF}, FIRowsetUpdate);

      FCommand.FIUnknown := nil;
    end;
  except
    ReleaseRecordSetInterfaces;
    raise;
  end;
end;

procedure TOLEDBRecordSet.ReleaseRecordSetInterfaces;
begin
  FIRowsetUpdate := nil;

  FIRowset := nil;
  FIRowsetLocate := nil;

  if FNativeRowset or (CursorTypeForFetching = ctBaseTable) then
    RequestParamsIfPossible;

  FNativeRowset := True;
end;

procedure TOLEDBRecordSet.ReleaseAllInterfaces(const ReleaseMultipleResults: boolean);
begin
  FreeFetchBlock;
  FCommand.FIUnknown := nil;
  ReleaseRecordSetInterfaces;
end;

procedure TOLEDBRecordSet.InternalOpen(DisableInitFields: boolean = False);

  procedure OpenBaseTableRowset;
  var
    IUnk: IUnknown;
    FOpenRowset: IOpenRowset;
    TableID: PDBID;
    TableIDAccessor: TDBIDAccessor;
    OLEDBPropertiesSet: TOLEDBPropertiesSet;
  begin
    if DisconnectedMode and not FReadOnly and FCursorUpdate then
      DatabaseError(SDMandServerCursors);

    FCommand.FConnection.Check(FCommand.FConnection.GetIDBCreateSession.CreateSession(nil, IID_IOpenRowset, IUnk), nil);
    FOpenRowset := IOpenRowset(IUnk);

    TableID := Marshal.AllocHGlobal(SizeOf(TDBID));
    TableIDAccessor := TDBIDAccessor.Create(TableID);
    OLEDBPropertiesSet := TOLEDBPropertiesSet.Create(FCommand.FConnection, DBPROPSET_ROWSET);
    try
      TableIDAccessor.eKind := DBKIND_NAME;
      TableIDAccessor.pwszName := Marshal.AllocHGlobal(Length(FBaseTableName) * SizeOf(WideChar) + SizeOf(WideChar));
      CopyBuffer(Marshal.StringToHGlobalUni(WideString(Encoding.Default.GetString(Encoding.Default.GetBytes(FBaseTableName), 0, Length(FBaseTableName)))),
        TableIDAccessor.pwszName, Length(FBaseTableName) * SizeOf(WideChar) + SizeOf(WideChar));

      OLEDBPropertiesSet.AddPropBool(DBPROP_BOOKMARKS, True);
      OLEDBPropertiesSet.AddPropBool(DBPROP_OWNUPDATEDELETE, True);
      OLEDBPropertiesSet.AddPropBool(DBPROP_OWNINSERT, True);
      OLEDBPropertiesSet.AddPropBool(DBPROP_OTHERUPDATEDELETE, True);
      OLEDBPropertiesSet.AddPropBool(DBPROP_OTHERINSERT, True);
      OLEDBPropertiesSet.AddPropBool(DBPROP_CANFETCHBACKWARDS, True);
      OLEDBPropertiesSet.AddPropBool(DBPROP_QUICKRESTART, True);

      OLEDBPropertiesSet.AddPropBool(DBPROP_CANHOLDROWS, False);
      if Provider = prCompact then
        OLEDBPropertiesSet.AddPropInt(DBPROP_MAXOPENROWS, 1);

      // Transactions support
      OLEDBPropertiesSet.AddPropBool(DBPROP_COMMITPRESERVE, True);
      OLEDBPropertiesSet.AddPropBool(DBPROP_ABORTPRESERVE, True);

      OLEDBPropertiesSet.AddPropBool(DBPROP_IColumnsRowset, FRequestSQLObjects);
      OLEDBPropertiesSet.AddPropBool(DBPROP_IRowsetUpdate, not FReadOnly and FCursorUpdate, Boolean(ProviderPrimaryVer = 7));

      FCommand.FConnection.Check(FOpenRowset.OpenRowset(nil, TableID, nil,
        IID_IRowset, 1, PDBPropIDSetArray(OLEDBPropertiesSet.InitPropSet), IUnk), nil);

      SetIRowset(IRowset(IUnk), False);
      FWaitForFetchBreak := False;

      if not FReadOnly and FCursorUpdate then begin
        QueryIntf(FIRowset, {$IFDEF CLR}IRowsetUpdate{$ELSE}IID_IRowsetUpdate{$ENDIF}, FIRowsetUpdate);
        FFlatBuffers := True;
        FFetchAll := False;
        FFetchRows := 1;
      end;
    finally
      OLEDBPropertiesSet.Free;
      Marshal.FreeHGlobal(TableIDAccessor.pwszName);
      Marshal.FreeHGlobal(TableID);
      TableIDAccessor.Free;
    end;
  end;

begin
  FLastFetchBack := False;
  FLastFetchOK := True;
  FLastFetchEnd := False;
  FHRowAccessible := False;
  FProcessDynBofEof := False;

  try
    if FCursorType = ctBaseTable then
      OpenBaseTableRowset;

    if CursorTypeForFetching = ctDynamic then
      FRecordCount := - 1;

    if FNativeRowset then
      QueryCommandInterfaces(False); // If QueryInterfaces already called then do nothing. Need to prevent clear FICommandText & FICommandProperties after command.execute before CreateFieldDescByRowset

    inherited;
  except
    if FNativeRowset then
      ReleaseCommandInterfaces;

    if FCommand.GetCursorState = csFetched then
      FCommand.SetCursorState(csInactive);

    raise;
  end;
end;

procedure TOLEDBRecordSet.InternalClose;
begin
  if FCommand.FNonBlocking and (FFetchExecutor <> nil) then
    BreakFetch;

  ClearHRowIfNeed;
  FreeFetchBlock;
  if FNativeRowset then begin
    if not FLockClearMultipleResults then begin
      FCommand.ClearIMultipleResults;
      FCommand.FIUnknownNext := nil;
    end;
    FCommand.FIUnknown := nil;
    RequestParamsIfPossible;
  end;
  if FIRowset <> nil then // If FIRowset is not closed on Fetch by any reason
    ReleaseRecordSetInterfaces;

  FreeData; // To destroy Blobs

  if FNativeRowset then
    ReleaseCommandInterfaces;

  FCommand.FCursorState := csInactive;
  if (not Prepared) or FCommand.FIsSProc then
    CommandType := ctUnknown;

  FCommand.FNextResultRequested := False;
  FLastFetchEnd := False;
  FBulkExecuting := False;
  FIUnknownIsAssigned := False;

  inherited;
end;

procedure TOLEDBRecordSet.ExecCommand;
  procedure ProcessCursorType;// Analyze CursorType changes
  var
    ActualCursorType: TMSCursorType; // Cursor type after OLEDB execute. 
    ActualSCReadOnly: boolean; // ReadOnly after OLEDB execute. Only for server cursors and CursorUpdate = True
  
    procedure AnalyzeCursorType; // Analyzed by FCommand.FIUnknown
    var
      RowsetInfo: IRowsetInfo;
      PropValues: TPropValues;

    begin
      with TOLEDBPropertiesGet.Create(FCommand.FConnection, DBPROPSET_ROWSET) do
        try
          AddPropId(DBPROP_SERVERCURSOR);
          AddPropId(DBPROP_OTHERUPDATEDELETE);
          AddPropId(DBPROP_IRowsetLocate);
          AddPropId(DBPROP_IRowsetUpdate);

          // Getting info interface
          if FCommand.FIUnknown <> nil then begin
            QueryIntf(FCommand.FIUnknown, {$IFDEF CLR}IRowsetInfo{$ELSE}IID_IRowsetInfo{$ENDIF}, RowsetInfo);
            GetProperties(RowsetInfo, PropValues);
          end
          else
          begin
            Assert(FCommand.FICommandProperties <> nil);
            GetProperties(FCommand.FICommandProperties, PropValues);
          end;
        finally
          Free;
        end;

      if {DBPROP_SERVERCURSOR} PropValues[0] = False then
        ActualCursorType := ctDefaultResultSet
      else
      begin // Static, KeySet, Dynamic
        if {DBPROP_OTHERUPDATEDELETE} PropValues[1] = False then begin // Static
          ActualCursorType := ctStatic;
          ActualSCReadOnly := True;
        end
        else
        begin // KeySet, Dynamic
          if {DBPROP_IRowsetLocate} PropValues[2] = False then // Dynamic
            ActualCursorType := ctDynamic
          else
            ActualCursorType := ctKeySet; // KeySet

          ActualSCReadOnly := {DBPROP_IRowsetUpdate} PropValues[3] = False;
        end;
      end;
    end;
    
  var
    IsChanged, IsROChanged: boolean;
  begin
    ActualCursorType := FCursorType;
    ActualSCReadOnly := FReadOnly;
    AnalyzeCursorType;

    IsROChanged := (FCursorType in ServerCursorTypes) and FCursorUpdate and (FReadOnly <> ActualSCReadOnly);
    IsChanged := (FCursorType <> ActualCursorType) 
      or IsROChanged;

    if IsChanged then begin
      FCursorType := ActualCursorType;
      if IsROChanged then
        FReadOnly := ActualSCReadOnly;

      Assert(Assigned(FCursorTypeChanged));
      try
        FCursorTypeChanged; // may be exception
      except
        FCommand.FIUnknown := nil;
        FCommand.ClearIMultipleResults;
        raise;
      end;
    end;
  end;

begin
  if not FNativeRowset then
    Exit; // CommandExec is not need for non-Native rowsets

  if FIUnknownIsAssigned then begin
    CommandType := ctCursor;
    Exit;
  end;

  QueryCommandInterfaces(False);
  try
    if not Prepared
      and (FCommand.FIMultipleResults = nil)
      and FNativeRowset then // This is a first call to non-prepared DataSet.Command.Execute
      SetCommandProp;
    inherited;

    if (FCommand.FIUnknown <> nil)
      and FCommand.FLastExecWarning then
        ProcessCursorType;

    /// Must be after ProcessCursorType to prevent wrong setting CommandType
    if ((FCommand.FIUnknown <> nil) or (FIRowset <> nil)) and
      not FBulkExecuting then
      CommandType := ctCursor
    else
      CommandType := ctStatement;

  finally
    ReleaseCommandInterfaces;
    if CommandType <> ctCursor then
      FCommand.SetCursorState(csInactive);
  end;
end;

procedure TOLEDBRecordSet.Open;
begin
  try
    inherited;
  except
    if FCommand.FQueryIntCnt > 0 then
      FCommand.ReleaseInterfaces;
    raise;
  end;
end;

procedure TOLEDBRecordSet.Reopen;
begin
  if not FNativeRowset and (FCursorType <> ctBaseTable) then begin
    Close;
    SetIRowset(FCommand.FConnection.GetSchemaRowset(FSchema, FRestrictions), False);
  end;

  inherited;
end;

function TOLEDBRecordSet.GetSchemaRowset(const Schema: TGUID; rgRestrictions: TRestrictions): IRowset;
begin
  FSchema := Schema;
  FRestrictions := rgRestrictions;

  Assert(FCommand.FConnection <> nil);
  Result := FCommand.FConnection.GetSchemaRowset(FSchema, FRestrictions);
end;

procedure TOLEDBRecordSet.Disconnect;
begin
  Assert(FCommand <> nil);
  FCommand.FIUnknown := nil;
  // FCommand.FIMultipleRes
  
  //Cache connection depenednt information
  GetProviderPrimaryVer;
  GetDBMSPrimaryVer;
  GetProviderId;
  GetProvider;
  GetDisconnectedMode;
  GetDatabase;

  ReleaseAllInterfaces(True);
  inherited;
end;

procedure TOLEDBRecordSet.CreateCommand;
var
  Cmd: TOLEDBCommand;
begin
  Cmd := TOLEDBCommand.Create;
  Cmd.FRequestIUnknown := True;
  SetCommand(Cmd);
end;

procedure TOLEDBRecordSet.SetCommand(Value: TCRCommand);
begin
  inherited;

  FCommand := TOLEDBCommand(Value);
end;

function TOLEDBRecordSet.GetIRowset: IRowset;
begin
  Result := FIRowset;
end;

function TOLEDBRecordSet.GetICommandText: ICommandText;
begin
  Result := FCommand.FICommandText;
end;

procedure TOLEDBRecordSet.SetIRowset(
  Rowset: IRowset;
  const IsColumnsRowset: boolean); // If True then FieldDescs was stored in FColumnsRowsetFieldDescs
begin
  Close;
  Unprepare;

  FIRowset := Rowset;
  FNativeRowset := False;
  if FCursorType <> ctBaseTable then begin
    FReadOnly := True; // Non-native rowset cannot be modified
    FFlatBuffers := True;
  end;

  FCommand.FCursorState := csExecuted;
  CommandType := ctUnknown;
  FIsColumnsRowset := IsColumnsRowset;
end;

procedure TOLEDBRecordSet.AssignToNextResult(Dest: TOLEDBRecordSet);
begin
  while (FCommand.FIMultipleResults <> nil) and (FCommand.FIUnknown = nil) do
    FCommand.GetNextResult(FCommand.FIUnknown, FCommand.FRowsAffected);

  if FCommand.FIUnknown = nil then
    DatabaseError(SNoNextResultSet, nil);

  Assert(Dest <> nil);
  Dest.FCommand.FIUnknown := FCommand.FIUnknown;
  Dest.FCommand.FRowsAffected := FCommand.FRowsAffected;
  Dest.FIUnknownIsAssigned := True;
  FCommand.FIUnknown := nil;
end;

procedure TOLEDBRecordSet.Check(const Status: HRESULT; AnalyzeMethod: TAnalyzeMethod = nil; const Arg: IntPtr = nil);
begin
  if FNativeRowset or (FCursorType = TMSCursorType(ctTableType)) then
    FCommand.FConnection.Check(Status, Component, AnalyzeMethod, Arg)
  else
    if (Status <> S_OK) and (Status <> DB_S_ERRORSOCCURRED) then
      raise EOLEDBError.Create(Status, _Format(SOLEDBError, [Status]));
end;

procedure TOLEDBRecordSet.RequestParamsIfPossible;
begin
  if (FIRowsetLocate = nil) and (FIRowsetUpdate = nil) and (FIRowset = nil) then begin
    FCommand.RequestParamsIfPossible;
    if (FCommand.FIUnknown = nil)
      and ((FCommand.GetCursorState = csFetching) or (FCommand.GetCursorState = csFetchingAll)) then begin
      FCommand.SetCursorState(csFetched);
      if not Prepared then
        ReleaseCommandInterfaces;
    end;
  end;
end;

function TOLEDBRecordSet.GetProviderPrimaryVer: integer;
begin
  if (FCommand <> nil) and (FCommand.FConnection <> nil) then
    FProviderPrimaryVer := FCommand.FConnection.ProviderPrimaryVer;
  Result := FProviderPrimaryVer;
end;

function TOLEDBRecordSet.GetDBMSPrimaryVer: integer;
begin
  if (FCommand <> nil) and (FCommand.FConnection <> nil) then
    FDBMSPrimaryVer := FCommand.FConnection.DBMSPrimaryVer;
  Result := FDBMSPrimaryVer;
end;

function TOLEDBRecordSet.GetProviderId: TGuid;
begin
  if (FCommand <> nil) and (FCommand.FConnection <> nil) then
    FProviderId := FCommand.FConnection.FProviderId;
  Result := FProviderId;
end;

function TOLEDBRecordSet.GetProvider: TOLEDBProvider;
begin
  if (FCommand <> nil) and (FCommand.FConnection <> nil) then
    FProvider := FCommand.FConnection.FProvider;
  Result := FProvider;
end;

function TOLEDBRecordSet.GetDisconnectedMode: boolean;
begin
  if (FCommand <> nil) and (FCommand.FConnection <> nil) then
    FDisconnectedMode := FCommand.FConnection.DisconnectedMode;
  Result := FDisconnectedMode;
end;

function TOLEDBRecordSet.GetDatabase: _string;
begin
  if (FCommand <> nil) and (FCommand.FConnection <> nil) then
    FDatabase := FCommand.FConnection.FDatabase;
  Result := FDatabase;
end;

function TOLEDBRecordSet.GetWideMemos: boolean;
begin
  if (FCommand <> nil) and (FCommand.FConnection <> nil) then
    Result := FCommand.GetWideMemos
  else
    Result := FWideMemos;
end;

procedure TOLEDBRecordSet.CheckBCDOverflow(const FieldNo: integer {from 1}; RecBuf: IntPtr);
begin
  if (Fields[FieldNo - 1].DataType = dtBCD)
    and (GetStatus(FieldNo - 1, RecBuf) = DBSTATUS_E_DATAOVERFLOW) then
    raise Exception.Create(SBCDOverflow);
end;

function TOLEDBRecordSet.AnalyzeFieldsStatus(const Status: HRESULT; const Arg: IntPtr = nil): WideString;
const
  FieldHeader = 'Field[%d] %s - %s (Status = %Xh).';
var
  i: integer;
  FieldStatus: DWORD;
  FieldName: _string;
  Msg: WideString;
  pRec: IntPtr;
begin
  Result := '';
  if (Status <> S_OK) and (Status <> DB_S_ERRORSOCCURRED) then begin
    pRec := Arg;
    Msg := '';
    for i := 0 to FFields.Count - 1 do
      if FFields[i].FieldDescKind = fdkData then begin
        FieldName := FFields[i].Name;
        if FieldName = '' then
          FieldName := IntToStr(i)
        else
          FieldName := ':' + FieldName;
        FieldStatus := GetStatus(i, pRec);
        case FieldStatus of
          DBSTATUS_S_OK, DBSTATUS_S_ISNULL, DBSTATUS_S_DEFAULT:;
          DBSTATUS_E_BADACCESSOR:
            AddInfoToErr(Msg, FieldHeader, [i, FieldName, SBadAccessor, FieldStatus]);
          DBSTATUS_E_CANTCONVERTVALUE:
            AddInfoToErr(Msg, FieldHeader, [i, FieldName, SInvalidValue, FieldStatus]);
          DBSTATUS_S_TRUNCATED:
            AddInfoToErr(Msg, FieldHeader, [i, FieldName, SDataTruncated, FieldStatus]);
          DBSTATUS_E_SIGNMISMATCH:
            AddInfoToErr(Msg, FieldHeader, [i, FieldName, SSignMismatch, FieldStatus]);
          DBSTATUS_E_DATAOVERFLOW:
            AddInfoToErr(Msg, FieldHeader, [i, FieldName, SDataOverflow, FieldStatus]);
          DBSTATUS_E_CANTCREATE:
            AddInfoToErr(Msg, FieldHeader, [i, FieldName, SCantCreate, FieldStatus]);
          DBSTATUS_E_UNAVAILABLE:
            {AddInfoToErr(Msg, FieldHeader, [i, FieldName, SUnavaible, FieldStatus])};
          DBSTATUS_E_PERMISSIONDENIED:
            AddInfoToErr(Msg, FieldHeader, [i, FieldName, SPermissionDenied, FieldStatus]);
          DBSTATUS_E_INTEGRITYVIOLATION:
            AddInfoToErr(Msg, FieldHeader, [i, FieldName, SIntegrityViolation, FieldStatus]);
          DBSTATUS_E_SCHEMAVIOLATION:
            AddInfoToErr(Msg, FieldHeader, [i, FieldName, SShemaViolation, FieldStatus]);
          DBSTATUS_E_BADSTATUS:
            AddInfoToErr(Msg, FieldHeader, [i, FieldName, SBadStatus, FieldStatus]);
        else
          AddInfoToErr(Msg, FieldHeader, [i, FieldName, SUnknownStatus, FieldStatus]);
        end;
      end;
    Result := Msg;
  end;
end;

procedure TOLEDBRecordSet.InternalInitFields;
  function ConvertDBCOLUMNINFOToFieldDesc(
    // const prgInfoEl: PDBCOLUMNINFO;
    const FieldName: _string; // prgInfoEl.pwszName
    const ActualFieldName: _string; // if prgInfoEl.columnid.eKind = DBKIND_NAME then ActualFieldName := prgInfoEl.columnid.uName.pwszName else ActualFieldName := prgInfoEl.pwszName;
    const FieldNo: DBORDINAL; // prgInfoEl.iOrdinal
    const OLEDBType: DBTYPE; // prgInfoEl.wType
    const dwFlags: DBCOLUMNFLAGS; // prgInfoEl.dwFlags
    const IsAutoIncrement: boolean;
    const Precision: integer; //prgInfoEl.bPrecision
    const Scale: integer; //prgInfoEl.bScale
    const ColumnSize: DBLENGTH; // prgInfoEl.ulColumnSize

    // Options
    const LongStrings: boolean;
    // Result
    Field: TOLEDBFieldDesc
    ): boolean;// return False, if this field type not supported

    function IsFlagSetted(const Flag: UINT): boolean;
    begin
      Result := (dwFlags and Flag) <> 0;
    end;

    procedure ConvertFlags;
    begin
      // DBCOLUMNFLAGS_ISFIXEDLENGTH, DBCOLUMNFLAGS_ISLONG,
      // DBCOLUMNFLAGS_ISNULLABLE, DBCOLUMNFLAGS_MAYBENULL
      // DBCOLUMNFLAGS_WRITE, DBCOLUMNFLAGS_WRITEUNKNOWN
      // DBCOLUMNFLAGS_ISROWID, DBCOLUMNFLAGS_ISROWVER
      // DBCOLUMNFLAGS_SCALEISNEGATIVE

      Field.Fixed := IsFlagSetted(DBCOLUMNFLAGS_ISFIXEDLENGTH);
      Field.IsKey := IsFlagSetted(DBCOLUMNFLAGS_KEYCOLUMN);
      Field.Required := not IsFlagSetted(DBCOLUMNFLAGS_ISNULLABLE);
      Field.FIsTimestamp := IsFlagSetted(DBCOLUMNFLAGS_ISROWVER);

      Field.ReadOnly :=
        (not IsFlagSetted(DBCOLUMNFLAGS_WRITE) and
         not IsFlagSetted(DBCOLUMNFLAGS_WRITEUNKNOWN));

      if FNativeRowset
        and not (FCursorType in ServerCursorTypes)
        and Field.ReadOnly then
        Field.ReadOnly := Field.FIsTimestamp or IsAutoIncrement;

      Field.IsAutoIncrement := IsAutoIncrement;
    end;

    function CreateUniqueFieldName(const FieldName: _string): _string;
    var
      AliasNum: integer;
    begin
      if FieldName = '' then begin
        AliasNum := 1;
        repeat
          Result := UniqueFieldNamePrefix + IntToStr(AliasNum);
          Inc(AliasNum);
        until FindField(Result) = nil;
      end
      else
        Result := FieldName;
    end;

  var
    InternalType: word;
  begin
    Field.SubDataType := dtUnknown;

    Result := ConvertOLEDBTypeToInternalFormat(OLEDBType, IsFlagSetted(DBCOLUMNFLAGS_ISLONG),
      FCommand.CalcEnableBCD, FCommand.CalcEnableFMTBCD, FCommand.FSensibleBCDMapping,
      Precision, Scale, FWideStrings, GetWideMemos, False, InternalType, DBMSPrimaryVer);
    if not Result then
      Exit;

    // --- Correct access to SQL 2000 server from 7.0 Client
    if FNativeRowset
      and ((ProviderPrimaryVer < 8) and not IsWindowsVista and OldProvidersSupport)
      and (Provider <> prCompact)
      and (OLEDBType = DBTYPE_NUMERIC)
      and (Precision = 19) then
      InternalType := dtInt64;
    // ---

  {$IFDEF LITE}
    if (InternalType = dtFmtBCD)
      and not ((Scale > 4) or (Precision > 14)) then
      InternalType := dtBCD;
  {$ENDIF}

    Field.DataType := InternalType;

    if FNativeRowset then
      Field.Name := CreateUniqueFieldName(FieldName)
    else
      Field.Name := FieldName;

    if ActualFieldName <> '' then
      Field.ActualName := ActualFieldName
    else
      Field.ActualName := Field.Name;

    Field.FieldNo := FieldNo;

    ConvertFlags;
    // Correct access to 'bigint' fields from Delphi4
  {$IFDEF VER4}
    if Field.DataType = dtInt64 then
      Field.ReadOnly := True;
  {$ENDIF}

    {if FNativeRowset then
      Field.Hidden := integer(prgInfoEl.columnid.uGuid.pguid) <> 1;
    // :( This is impossible becouse we needs TField for update SQLs and for ColumnsMetaInfo}

    // WAR Field.Size must be syncronized with actual data size in PutFieldData, GetFieldData, and possible PutFieldAsVariant, GetFieldAsVariant

    case InternalType of
      // Integer fields
      dtBoolean:
        Field.Size := sizeof(WordBool);
      dtInt8: begin
        Field.Size := sizeof(byte);
        if IsCompactEdition(DBMSPrimaryVer) then
          Field.SubDataType := dtUInt8;
      end;
      dtWord: begin
        if IsCompactEdition(DBMSPrimaryVer) and (OLEDBType = DBTYPE_UI1) then
          Field.SubDataType := dtUInt8;
        Field.Size := sizeof(word);
      end;
      dtInt16:
        Field.Size := sizeof(smallint);
      dtInt32, dtUInt32:
        Field.Size := sizeof(integer);
      dtInt64: begin
        Field.Size := sizeof(int64);
        if IsCompactEdition(DBMSPrimaryVer) and (OLEDBType = DBTYPE_UI8) then
          Field.SubDataType := dtUInt64;
      end;

      // Float fields
      dtFloat, dtBCD {$IFDEF VER6P}{$IFNDEF FPC}, dtFmtBCD {$ENDIF}{$ENDIF}: begin
        case InternalType of
          dtFloat:
          begin
            Field.Size := sizeof(double);
            Field.Scale := Scale;
            Field.Length := Precision; // Precision cannot be greater then 15

            if OLEDBType = DBTYPE_R4 then begin
              Field.Length := 7;
              if IsCompactEdition(DBMSPrimaryVer) then
                Field.SubDataType := dtSingle;
            end
            else
              if OLEDBType = DBTYPE_R8 then
                Field.Length := 15;
          end;
          // Multibyte fields
          dtBCD:
          begin
            Field.Size := sizeof(Currency);
            Field.Scale := Scale;
            Field.Length := Precision;
          end;
        {$IFDEF VER6P}
        {$IFNDEF FPC}
          dtFmtBCD:
          begin
            if Precision < SizeOfTBcd then
              Field.Size := SizeOfTBcd + 1{'+/-'} + 1{'.'} + 1{#0}
            else
              Field.Size := Precision + 1{'.'} + 1 {#0}; // To right notation of large NUMERIC values
            Field.Scale := Scale;
            Field.Length := Precision;

            if Field.Length > MaxFMTBcdDigits then
              Field.Length := MaxFMTBcdDigits;
            if Field.Scale > Field.Length then // if length was reduced
              Field.Scale := Field.Length;
          end;
       {$ENDIF}
       {$ENDIF}
        end;
        if OLEDBType = DBTYPE_NUMERIC then
          if {IsCompactEdition(DBMSPrimaryVer)} not ServerCursorUsed then
            Field.SubDataType := dtNumeric;
      end;
      dtCurrency:
      begin
        Field.Size := sizeof(double);
        //Field.Size := sizeof(currency); //Currency type cannot be used over TCurrencyField uses double to store
        Field.Scale := Scale;
        Field.Length := Precision; // Precision cannot be greater then 15
      end;

      dtDateTime:
        if IsCompactEdition(DBMSPrimaryVer) then
          Field.Size := sizeof(TDBTimeStamp)
        else
          Field.Size := sizeof(TDateTime);
      dtDate, dtTime:
        Field.Size := sizeof(TDateTime);
    {$IFDEF VER5P}
      dtGuid:
      begin
        Field.Length := 38; { Length(GuidString) }
        Field.Size := Field.Length + 1;
      end;
   {$ENDIF}
      dtString, dtWideString, dtMemo, dtWideMemo, dtMSXML:
      begin
        Field.Length := 0; // WAR Field.Size must be syncronized with actual data size in PutFieldData, GetFieldData, and possible PutFieldAsVariant, GetFieldAsVariant
      {$IFNDEF VER5P}
        if prgInfoEl.wType = DBTYPE_GUID then
        begin
          Field.Length := 38; { Length(GuidString) }
          Field.Size := Word(Field.Length + 1);
        end
        else
      {$ENDIF}
        if IsFlagSetted(DBCOLUMNFLAGS_ISLONG) then begin
          // This is a MS SQL 'text' or 'ntext' field
          Field.SubDataType := dtText;
          Field.Size := Word(sizeof(IntPtr)) {ISeqStream};
          if not FReadOnly then
            Field.Size := Word(Field.Size + Word(sizeof(DBLENGTH))) {DBLENGTH};
        end
        else  // This is a MS SQL 'char', 'varchar', 'nchar' or 'nvarchar' field
        begin
          if not LongStrings and (ColumnSize > 255) then begin
            if InternalType = dtWideString then
              Field.DataType := dtWideMemo
            else
              Field.DataType := dtMemo;
            Field.Size := Word(sizeof(IntPtr)) {ISeqStream};
            if not FReadOnly then
              Field.Size := Word(Field.Size + Word(sizeof(DBLENGTH))) {DBLENGTH};
          end
          else
          begin
            Field.Length := ColumnSize;
            if InternalType = dtWideString then
              Field.Size := Word((ColumnSize + 1) * sizeof(WideChar))
            else
              Field.Size := Word(ColumnSize + 1);

            if not FFlatBuffers and (Field.Size >= FlatBufferLimit) and (FCursorType = ctDefaultResultSet) then
            begin
              Field.Size := Word(sizeof(IntPtr));
              if InternalType = dtString then
                Field.DataType := dtExtString
              else
                Field.DataType := dtExtWideString;
            end;
          end;
        end;

      {$IFDEF LITE}
        if (OLEDBType = DBTYPE_XML) then
          Field.SubDataType := dtMSXML;
      {$ENDIF}
        if (OLEDBType = DBTYPE_WSTR) or (OLEDBType = DBTYPE_XML) then
          Field.SubDataType := Field.SubDataType or dtWide;
      end;
      dtBytes, dtVarBytes, dtBlob:
      begin
        if IsFlagSetted(DBCOLUMNFLAGS_ISLONG) or (OLEDBType = DBTYPE_UDT) then begin
          // This is a MS SQL 'image' field
          // WAR Field.Size must be syncronized with actual data size in PutFieldData, GetFieldData, and possible PutFieldAsVariant, GetFieldAsVariant
          Field.Size := Word(sizeof(IntPtr)) {ISeqStream};
          if not FReadOnly then
            Field.Size := Word(Field.Size + Word(sizeof(DBLENGTH))) {DBLENGTH};

          if not IsFlagSetted(DBCOLUMNFLAGS_ISLONG) then
            Field.Length := Word(ColumnSize);
          if OLEDBType = DBTYPE_UDT then
            Field.SubDataType := dtMSUDT;
        end
        else begin
          Field.Length := Word(ColumnSize);

          if Field.Fixed then begin
            if FReadOnly then
              Field.Size := Word(Field.Length)
            else
              Field.Size := Word(Field.Length + Word(sizeof(DBLENGTH))) {OLE DB readed bytes};
          end
          else
            if FFlatBuffers or (ColumnSize < FlatBufferLimit) or (FCursorType <> ctDefaultResultSet) then begin
              Field.DataType := dtVarBytes;
              Field.Size := Word(Word(sizeof(word)) {Readed bytes} + Field.Length + Word(sizeof(DBLENGTH))) {OLE DB readed bytes};
            end
            else begin
              Field.DataType := dtExtVarBytes;
              Field.Size := Word(sizeof(IntPtr) {IntPtr to OLEDBBuf/StringHeap} + sizeof(DBLENGTH)) {OLE DB readed bytes};
            end;
        end;
      end;
    {$IFDEF VER5P}
      dtVariant:
        Field.Size := sizeof(TVariantObject);
    {$ENDIF}
      else
        Result := False;
    end;
  end;

  procedure CreateFieldDescs(
    const IUnk: IUnknown;
    const ByInfo: boolean // if True, then FieldDescs creates with using ColumnsInfo, otherwise - using ColumnsRowset
  );

    procedure CreateFieldDescsByInfo;
      function GetHiddenColumnsCount: UINT;
      var
        PropValues: TPropValues;
      begin
        with TOLEDBPropertiesGet.Create(FCommand.FConnection, DBPROPSET_ROWSET) do
          try
            AddPropId(DBPROP_HIDDENCOLUMNS);
            GetProperties(FCommand.FICommandProperties, PropValues);
          finally
            Free;
          end;
        Result := PropValues[0];
      end;

    var
      ColumnsInfo: IColumnsInfo;
      i: integer;
      Field, BookmarkField: TOLEDBFieldDesc;

      cColumns: NativeUInt;
      prgInfo: PDBCOLUMNINFO;
      prgInfoEl: PDBCOLUMNINFO;
      rgInfoEl: {$IFDEF CLR}DBCOLUMNINFO{$ELSE}PDBCOLUMNINFO{$ENDIF};
      pStringsBuffer: IntPtr;
      //Malloc: IMalloc;

      ActualFieldName: _string;
      TableInfo: TCRTableInfo;
    begin
      if IUnk = nil then
        Exit; // This query does not return rowset

      QueryIntf(IUnk, {$IFDEF CLR}IColumnsInfo{$ELSE}IID_IColumnsInfo{$ENDIF}, ColumnsInfo);
      Assert(ColumnsInfo <> nil);

      pStringsBuffer := nil;
      Check(ColumnsInfo.GetColumnInfo(cColumns, PDBCOLUMNINFO(prgInfo), pStringsBuffer));

    {$IFDEF LITE}
      Assert(not FUniqueRecords);
    {$ENDIF}
      // Add hidden columns count for SQL 2000
      if FNativeRowset and (((ProviderPrimaryVer >= 8) or IsWindowsVista) or not OldProvidersSupport) and FUniqueRecords then
        cColumns := cColumns + GetHiddenColumnsCount;

      if cColumns > 0 then
        try
          BookmarkField := nil;
          prgInfoEl := prgInfo;
          for i := 0 to cColumns - 1 do begin
          {$IFDEF CLR}
            Assert(prgInfoEl <> nil);
            rgInfoEl := DBCOLUMNINFO(Marshal.PtrToStructure(prgInfoEl, TypeOf(DBCOLUMNINFO)));
          {$ELSE}
            rgInfoEl := prgInfoEl;
          {$ENDIF}
            Field := TOLEDBFieldDesc.Create;
            try
              if rgInfoEl.columnid.eKind = DBKIND_NAME then
                ActualFieldName := Marshal.PtrToStringUni(rgInfoEl.columnid.uName{$IFNDEF CLR}.pwszName{$ENDIF})
              else
                ActualFieldName := rgInfoEl.pwszName;

              if rgInfoEl.bScale = $FF then
                rgInfoEl.bScale := 0;
                
              if ConvertDBCOLUMNINFOToFieldDesc(
                // rgInfoEl
                rgInfoEl.pwszName,
                ActualFieldName,
                rgInfoEl.iOrdinal,
                rgInfoEl.wType,
                rgInfoEl.dwFlags,
                False,
                rgInfoEl.bPrecision,
                rgInfoEl.bScale,
                rgInfoEl.ulColumnSize,
                FLongStrings, Field) then
              begin
                if FCursorType = ctBaseTable then begin
                  // Fill TablesInfo structure
                  Assert(FBaseTableName <> '');
                  TableInfo := TablesInfo.FindByName(FBaseTableName);
                  TablesInfo.BeginUpdate;
                  try
                    if TableInfo = nil then begin
                      TableInfo := TablesInfo.Add;
                      TableInfo.TableName := FBaseTableName;
                      TableInfo.TableAlias := '';
                    end;
                  finally
                    TablesInfo.EndUpdate;
                  end;
                  Field.TableInfo := TableInfo;
                end;

                if Field.FieldNo > 0 then
                  FFields.Add(Field)
                else // Bookmark column have FieldNo = 0
                begin
                  Field.Hidden := True;
                  BookmarkField := Field;
                  FBookmarkOffset := - 1;
                end;
              end
              else
                if FNativeRowset then
                  DatabaseErrorFmt(SBadFieldType, [rgInfoEl.pwszName, rgInfoEl.wType])
                else
                  Field.Free;
            except
              Field.Free;
              BookmarkField.Free;
              raise;
            end;
            prgInfoEl := PtrOffset(prgInfoEl, {$IFDEF CLR}Marshal.SizeOf(TypeOf(DBCOLUMNINFO)){$ELSE}sizeof(DBCOLUMNINFO){$ENDIF});
          end;
          if BookmarkField <> nil then
            FFields.Add(BookmarkField);

        finally
          {if FCommand.FConnection = nil then
            CoGetMalloc(1, Malloc)
          else
            Malloc := FCommand.FConnection.Malloc;}

          FreeCoMem(prgInfo);
          FreeCoMem(pStringsBuffer);
          //Malloc.Free(prgInfo);
          //Malloc.Free(pStringsBuffer);

          //Malloc := nil;
        end;
    end;

  {$IFNDEF LITE}
    procedure CreateFieldDescsByRowset;
    var
      RecBuf: IntPtr;
      ColumnsMetaInfo: TOLEDBRecordSet;

      procedure CheckColumnsMetaInfo;
      begin
        if FCommand.FConnection.FColumnsMetaInfo <> nil then
          ColumnsMetaInfo := FCommand.FConnection.FColumnsMetaInfo
        else
        begin
          ColumnsMetaInfo := TOLEDBRecordSet.Create;
          FCommand.FConnection.FColumnsMetaInfo := ColumnsMetaInfo;

          ColumnsMetaInfo.SetConnection(FCommand.FConnection);
          ColumnsMetaInfo.SetProp(prFetchAll, False);
          ColumnsMetaInfo.SetProp(prFetchRows, 1);
          ColumnsMetaInfo.SetProp(prUniDirectional, True);
          FCommand.FConnection.FFldCatalogNameIdx := -1;
        end;
      end;

      procedure CheckColumnsMetaInfoIdx;
      begin
        Assert(ColumnsMetaInfo <> nil);
        if FCommand.FConnection.FFldCatalogNameIdx <> - 1 then
          Exit;

        with FCommand.FConnection do begin
          FFldCatalogNameIdx := FColumnsMetaInfo.Fields.IndexOf(FColumnsMetaInfo.FindField('DBCOLUMN_BASECATALOGNAME')); // SQL Everywhere
          FFldSchemaNameIdx := FColumnsMetaInfo.Fields.IndexOf(FColumnsMetaInfo.FindField('DBCOLUMN_BASESCHEMANAME')); // SQL Everywhere

          FFldTableNameIdx := FColumnsMetaInfo.Fields.IndexOf(FColumnsMetaInfo.FieldByName('DBCOLUMN_BASETABLENAME'));
          FFldColumnNameIdx := FColumnsMetaInfo.Fields.IndexOf(FColumnsMetaInfo.FieldByName('DBCOLUMN_BASECOLUMNNAME'));

          FFldPrecisionIdx := FColumnsMetaInfo.Fields.IndexOf(FColumnsMetaInfo.FieldByName('DBCOLUMN_PRECISION'));
          FFldScaleIdx := FColumnsMetaInfo.Fields.IndexOf(FColumnsMetaInfo.FieldByName('DBCOLUMN_SCALE'));
          FFldGuidIdx := FColumnsMetaInfo.Fields.IndexOf(FColumnsMetaInfo.FieldByName('DBCOLUMN_GUID'));

          {
          SELECT BaseColumnName AS ColumnAlias ...

          FIConnection.ProviderVer = [07.01.0623, 07.01.0690, 07.01.0819, 07.01.0961]
            IDNAME = BaseColumnName
            NAME = ColumnAlias
            BaseColumnName = BaseColumnName

          FIConnection.ProviderVer = [08.00.0194, 08.00.0528, 08.10.7430, 08.10.9001]
            IDNAME = ColumnAlias
            NAME = ColumnAlias
            BaseColumnName = BaseColumnName
          }

          FFldFieldNameIdx := FColumnsMetaInfo.Fields.IndexOf(FColumnsMetaInfo.FieldByName('DBCOLUMN_NAME'));
          /// ??? May be swap?
          //FFldFieldNameIdx := FColumnsMetaInfo.Fields.IndexOf(FColumnsMetaInfo.FieldByName('DBCOLUMN_IDNAME'));
          //FFldActualFieldNameIdx := FColumnsMetaInfo.Fields.IndexOf(FColumnsMetaInfo.FieldByName('DBCOLUMN_NAME'));

          FFldColumnNumberIdx := FColumnsMetaInfo.Fields.IndexOf(FColumnsMetaInfo.FieldByName('DBCOLUMN_NUMBER'));

          FFldIsAutoIncIdx := FColumnsMetaInfo.Fields.IndexOf(FColumnsMetaInfo.FindField('DBCOLUMN_ISAUTOINCREMENT')); // SQL Everywhere
          FFldTypeIdx := FColumnsMetaInfo.Fields.IndexOf(FColumnsMetaInfo.FieldByName('DBCOLUMN_TYPE'));
          FFldFlagsIdx := FColumnsMetaInfo.Fields.IndexOf(FColumnsMetaInfo.FieldByName('DBCOLUMN_FLAGS'));
          FFldColumnSizeIdx := FColumnsMetaInfo.Fields.IndexOf(FColumnsMetaInfo.FieldByName('DBCOLUMN_COLUMNSIZE'));
          FFldComputeModeIdx := FColumnsMetaInfo.Fields.IndexOf(FColumnsMetaInfo.FindField('DBCOLUMN_COMPUTEMODE'));

          // xml schema support
          FFldXMLSchemaCollCatalogNameIdx := FColumnsMetaInfo.Fields.IndexOf(FColumnsMetaInfo.FindField('DBCOLUMN_XML_SCHEMACOLLECTION_CATALOGNAME'));
          FFldXMLSchemaCollSchemaNameIdx := FColumnsMetaInfo.Fields.IndexOf(FColumnsMetaInfo.FindField('DBCOLUMN_XML_SCHEMACOLLECTION_SCHEMANAME'));
          FFldXMLSchemaCollNameIdx := FColumnsMetaInfo.Fields.IndexOf(FColumnsMetaInfo.FindField('DBCOLUMN_XML_SCHEMACOLLECTIONNAME'));

          // UDT support
          FFldUDTSchemanameIdx := FColumnsMetaInfo.Fields.IndexOf(FColumnsMetaInfo.FindField('DBCOLUMN_UDT_SCHEMANAME'));
          FFldUDTNameIdx := FColumnsMetaInfo.Fields.IndexOf(FColumnsMetaInfo.FindField('DBCOLUMN_UDT_NAME'));
          FFldUDTCatalognameIdx := FColumnsMetaInfo.Fields.IndexOf(FColumnsMetaInfo.FindField('DBCOLUMN_UDT_CATALOGNAME'));
          FFldAssemblyTypenameIdx := FColumnsMetaInfo.Fields.IndexOf(FColumnsMetaInfo.FindField('DBCOLUMN_ASSEMBLY_TYPENAME'));
        end;

      end;

      procedure FillTablesAliases;
      var
        Parser: TMSParser;
        TableName: _string;// Table or view name
        StLex, Alias: _string;
        Code: integer;
        TableInfo: TCRTableInfo;
        BracketCount: integer;
      begin
        TablesInfo.BeginUpdate;
        Parser := TMSParser.Create(FCommand.SQL);
        Parser.OmitBlank := False;
        Parser.OmitComment := True;
        try
          Code := Parser.ToLexem([lxWITH, lxSELECT]);
          if Code = lxWITH then
            Code := Parser.ToLexem(lxSELECT, True);

          if Code <> lcEnd then
            if Parser.ToLexem(lxFROM, True) <> lcEnd then
              repeat
                repeat
                  Code := Parser.GetNext(StLex); // Omit blank
                until Code <> lcBlank;

                BracketCount := 0;

                // TableName
                TableName := StLex;
                while True do begin
                  Code := Parser.GetNext(StLex);

                  if (Length(StLex) > 0) and (StLex[1] = ',') and (BracketCount = 0) then
                    Break;

                  if {(Length(StLex) > 0) and (StLex[1] in ['[, ' + '], ' + '", ' + '.']) and}
                    ((Code <> 0) and (Code <> lcBlank)) or (BracketCount > 0) then begin
                    if StLex = '(' then
                      Inc(BracketCount);
                    if StLex = ')' then
                      Dec(BracketCount);
                    TableName := TableName + StLex;
                  end
                  else
                    Break;
                end;

                // 'AS' clause
                if Parser.GetNext(Alias) = lxAS then
                  Parser.GetNext(Alias)
                else
                  Parser.Back;

                // Alias
                if Parser.GetNext(Alias) = lcIdent then
                  Parser.GetNext(StLex)
                else begin
                  Alias := '';
                  Parser.Back;
                end;

                TableName := FCommand.SQLInfo.NormalizeName(TableName);
                Assert(TableName <> '', 'TableName cannot be empty'); // ++++

                TableInfo := TablesInfo.FindByName(TableName);
                if TableInfo = nil then begin
                  TableInfo := TablesInfo.Add;
                  TableInfo.TableName := TableName;
                  TableInfo.TableAlias := '';
                  TableInfo.IsView := True;
                end;

                if Alias <> '' then
                  TableInfo.TableAlias := FCommand.SQLInfo.NormalizeName(Alias);

                if (StLex <> ',') and (Parser.ToLexem(lxJOIN) <> lcEnd) then
                  StLex := ',';
              until (StLex <> ',');
        finally
          Parser.Free;
          TablesInfo.EndUpdate;
        end;
      end;

      function GetStrValue(Idx: integer): _string;
      var
        Field: TOLEDBFieldDesc;
      begin
        if ColumnsMetaInfo.GetNull(Idx + 1, RecBuf) then
          Result := ''
        else
        begin
          Field := TOLEDBFieldDesc(ColumnsMetaInfo.Fields[Idx]);
          if (Field.SubDataType and dtWide) = 0 then
            Result := _string(Marshal.PtrToStringAnsi(PtrOffset(RecBuf, Field.Offset)))
          else
            Result := Marshal.PtrToStringUni(PtrOffset(RecBuf, Field.Offset));
        end;
      end;

      function GetWordValue(Idx: integer): Word;
      begin
        if ColumnsMetaInfo.GetNull(Idx + 1, RecBuf) then
          Result := 0
        else
          Result := Word(Marshal.ReadInt16(RecBuf, ColumnsMetaInfo.Fields[Idx].Offset));
      end;

      function GetLongWordValue(Idx: integer): Word;
      begin
        if ColumnsMetaInfo.GetNull(Idx + 1, RecBuf) then
          Result := 0
        else
          Result := LongWord(Marshal.ReadInt32(RecBuf, ColumnsMetaInfo.Fields[Idx].Offset));
      end;

      function GetUINTValue(Idx: integer): UINT;
      begin
        if ColumnsMetaInfo.GetNull(Idx + 1, RecBuf) then
          Result := 0
        else
          Result := UINT(Marshal.ReadInt32(RecBuf, ColumnsMetaInfo.Fields[Idx].Offset));
      end;

      function GetSmallIntValue(Idx: integer): SmallInt;
      begin
        if ColumnsMetaInfo.GetNull(Idx + 1, RecBuf) then
          Result := 0
        else
          Result := SmallInt(Marshal.ReadInt16(RecBuf, ColumnsMetaInfo.Fields[Idx].Offset));
      end;

    var
      ColumnsRowset: IColumnsRowset;
      CMIRowset: IRowset;

      Field, BookmarkField: TOLEDBFieldDesc;
      FieldNo: integer;
      TableName: _string;

      FieldName, ActualFieldName: _string;
      iu: IUnknown;
      FldGuidValue: _string;

      TableInfo: TCRTableInfo;
      IsAutoIncrement: boolean;

      ColumnsRecordSet: TOLEDBRecordSet;
      Value: variant;
    begin
      if IUnk = nil then
        Exit; // This query does not return rowset

      Assert (FRequestSQLObjects and FNativeRowset);

      QueryIntf(IUnk, {$IFDEF CLR}IColumnsRowset{$ELSE}IID_IColumnsRowset{$ENDIF}, ColumnsRowset);
      Check(ColumnsRowset.GetColumnsRowset(nil, 0, nil, IID_IRowset, 0, nil, iu));{Default properties - default result set}
      CMIRowset := IRowset(iu);
      Assert(CMIRowset <> nil);
      try
        FBookmarkOffset := - 2;
        BookmarkField := nil;
        CheckColumnsMetaInfo;

        ColumnsMetaInfo.SetIRowset(CMIRowset, True);
        //ColumnsMetaInfo.Prepare;
        ColumnsMetaInfo.Open;

        RecBuf := nil;
        try
          CheckColumnsMetaInfoIdx;

          ColumnsMetaInfo.AllocRecBuf(RecBuf);

          while True do begin
            ColumnsMetaInfo.GetNextRecord(RecBuf);
            if ColumnsMetaInfo.Eof then
              Break;

            FieldNo := GetLongWordValue(FCommand.FConnection.FFldColumnNumberIdx);
            Assert(FieldNo >= 0); // Bookmark column have FieldNo = 0

            Field := TOLEDBFieldDesc.Create;
            try
              FieldName := GetStrValue(FCommand.FConnection.FFldFieldNameIdx);
              if FCommand.FConnection.FFldColumnNameIdx <> -1 then
                ActualFieldName := GetStrValue(FCommand.FConnection.FFldColumnNameIdx)
              else
                ActualFieldName := '';

              IsAutoIncrement := False;
              if FCommand.FConnection.FFldIsAutoIncIdx <> - 1 then
                IsAutoIncrement := WordBool(GetWordValue(FCommand.FConnection.FFldIsAutoIncIdx));

              if ConvertDBCOLUMNINFOToFieldDesc(
                // prgInfoEl
                FieldName,
                ActualFieldName,
                FieldNo,
                GetWordValue(FCommand.FConnection.FFldTypeIdx),
                GetUINTValue(FCommand.FConnection.FFldFlagsIdx),
                IsAutoIncrement,
                GetWordValue(FCommand.FConnection.FFldPrecisionIdx),
                GetSmallIntValue(FCommand.FConnection.FFldScaleIdx),
                GetUINTValue(FCommand.FConnection.FFldColumnSizeIdx),
                // Options
                FLongStrings, Field) then begin

                if FCommand.FConnection.FFldCatalogNameIdx <> -1 then
                  Field.FBaseCatalogName := GetStrValue(FCommand.FConnection.FFldCatalogNameIdx);
                if FCommand.FConnection.FFldSchemaNameIdx <> -1 then
                  Field.FBaseSchemaName := GetStrValue(FCommand.FConnection.FFldSchemaNameIdx);
                Field.FBaseTableName := GetStrValue(FCommand.FConnection.FFldTableNameIdx);

                if FCommand.FConnection.FFldXMLSchemaCollCatalogNameIdx <> -1 then
                  Field.FXMLSchemaCollectionCatalogName := GetStrValue(FCommand.FConnection.FFldXMLSchemaCollCatalogNameIdx);
                if FCommand.FConnection.FFldXMLSchemaCollSchemaNameIdx <> -1 then
                  Field.FXMLSchemaCollectionSchemaName := GetStrValue(FCommand.FConnection.FFldXMLSchemaCollSchemaNameIdx);
                if FCommand.FConnection.FFldXMLSchemaCollNameIdx <> -1 then
                  Field.FXMLSchemaCollectionName := GetStrValue(FCommand.FConnection.FFldXMLSchemaCollNameIdx);
                Field.FXMLTyped := (Field.XMLSchemaCollectionCatalogName <> '') or (Field.XMLSchemaCollectionSchemaName <> '') or
                  (Field.FXMLSchemaCollectionName <> '');

                if FCommand.FConnection.FFldUDTSchemanameIdx <> -1 then
                  Field.FUDTSchemaname := GetStrValue(FCommand.FConnection.FFldUDTSchemanameIdx);
                if FCommand.FConnection.FFldUDTNameIdx <> -1 then
                  Field.FUDTName := GetStrValue(FCommand.FConnection.FFldUDTNameIdx);
                if FCommand.FConnection.FFldUDTCatalognameIdx <> -1 then
                  Field.FUDTCatalogname := GetStrValue(FCommand.FConnection.FFldUDTCatalognameIdx);
                if FCommand.FConnection.FFldAssemblyTypenameIdx <> -1 then
                  Field.FAssemblyTypename := GetStrValue(FCommand.FConnection.FFldAssemblyTypenameIdx);
              {$IFNDEF LITE}
              {$IFNDEF CLR}
                if (Field.FUDTName <> '') and (Field.FAssemblyTypename <> '') then
                  Field.FUDTDispatcher := TUDTDispatcher.Create;
              {$ENDIF}
              {$ENDIF}

                Field.ReadOnly := Field.ReadOnly or (ActualFieldName = '');

                if FCommand.FConnection.FFldComputeModeIdx <> -1 then
                  Field.ReadOnly := Field.ReadOnly or not (GetUINTValue(FCommand.FConnection.FFldComputeModeIdx) in [0, DBCOMPUTEMODE_NOTCOMPUTED]);

                if (not FUniqueRecords) or (FCursorType in ServerCursorTypes) then // Hide implicitly requested columns
                  if ((ProviderPrimaryVer >= 8) or IsWindowsVista) or not OldProvidersSupport then begin
                    FldGuidValue := _LowerCase(GetStrValue(FCommand.FConnection.FFldGuidIdx));
                    Field.Hidden := (FldGuidValue <> '') and (FldGuidValue <> '{' + _LowerCase(IntToHex(FieldNo, 8))+'-0000-0000-0000-000000000000' + '}')
                  end
                  else
                    Field.Hidden := not ColumnsMetaInfo.GetNull(FCommand.FConnection.FFldGuidIdx + 1, RecBuf);

                // Fill TablesInfo structure
                TableName := GenerateTableName(Field.BaseCatalogName, Field.BaseSchemaName, Field.BaseTableName,
                  FCommand.SQLInfo.NormalizeName(Database, False, True));
                if TableName <> '' then begin
                  TableInfo := TablesInfo.FindByName(TableName);
                  TablesInfo.BeginUpdate;
                  try
                    if TableInfo = nil then begin
                      TableInfo := TablesInfo.Add;
                      TableInfo.TableName := TableName;
                      TableInfo.TableAlias := '';
                    end;
                  finally
                    TablesInfo.EndUpdate;
                  end;
                  Field.TableInfo := TableInfo;
                end
                else
                  Field.TableInfo := nil;

                if FieldNo > 0 then
                  FFields.Add(Field)
                else // Bookmark column have FieldNo = 0
                begin
                  Field.Hidden := True;
                  BookmarkField := Field;
                  FBookmarkOffset := - 1;
                end;

              end
              else
                if FNativeRowset then
                  DatabaseErrorFmt(SBadFieldType, [FieldName, GetWordValue(FCommand.FConnection.FFldTypeIdx)])
                else
                  Field.Free;
            except
              Field.Free;
              BookmarkField.Free;
              BookmarkField := nil;
              raise;
            end;
          end;

          if BookmarkField <> nil then
            FFields.Add(BookmarkField);

          if (Provider = prCompact) and (not FPopulatingKeyInfo) and (FUniqueRecords or not FReadOnly) then begin
            if TablesInfo.Count > 0 then begin
              TableInfo := TablesInfo[0];
              ColumnsRecordSet := TOLEDBRecordSet.Create;
              FPopulatingKeyInfo := True;
              try
                ColumnsRecordSet.SetConnection(FCommand.FConnection);
                ColumnsRecordSet.SetSQL(_Format(
                  'SELECT' + LineSeparator +
                  '  A.COLUMN_NAME, B.AUTOINC_INCREMENT' + LineSeparator +
                  'FROM' + LineSeparator +
                  '  INFORMATION_SCHEMA.KEY_COLUMN_USAGE A INNER JOIN INFORMATION_SCHEMA.COLUMNS B' + LineSeparator +
                  '  ON A.TABLE_NAME = B.TABLE_NAME and A.COLUMN_NAME = B.COLUMN_NAME' + LineSeparator +
                  '  LEFT JOIN INFORMATION_SCHEMA.REFERENTIAL_CONSTRAINTS C' + LineSeparator +
                  '  ON A.CONSTRAINT_NAME = C.CONSTRAINT_NAME' + LineSeparator +
                  'WHERE' + LineSeparator +
                  '  C.CONSTRAINT_NAME IS NULL AND A.TABLE_NAME = %s', [AnsiQuotedStr(TableInfo.TableName, '''')]));
                ColumnsRecordSet.Open;
                if RecBuf <> nil then
                  ColumnsMetaInfo.FreeRecBuf(RecBuf);
                ColumnsRecordSet.AllocRecBuf(RecBuf);
                while True do begin
                  ColumnsRecordSet.GetNextRecord(RecBuf);
                  if ColumnsRecordSet.Eof then
                    Break;
                  if not ColumnsRecordSet.GetNull(ColumnsRecordSet.FieldByName('COLUMN_NAME').FieldNo, RecBuf) then begin
                    ColumnsRecordSet.GetFieldAsVariant(ColumnsRecordSet.FieldByName('COLUMN_NAME').FieldNo, RecBuf, Value);
                    Field := TOLEDBFieldDesc(FFields.FindField(_string(Value)));
                    if Field <> nil then begin
                      Field.IsKey := True;
                      Field.IsAutoIncrement := not ColumnsRecordSet.GetNull(ColumnsRecordSet.FieldByName('AUTOINC_INCREMENT').FieldNo, RecBuf);
                      if FNativeRowset and not (FCursorType in ServerCursorTypes) then
                        Field.ReadOnly := Field.FIsTimestamp or Field.IsAutoIncrement;
                    end;
                  end;
                end;
              finally
                ColumnsRecordSet.FreeRecBuf(RecBuf);
                RecBuf := nil;
                ColumnsRecordSet.Close;
                ColumnsRecordSet.Free;
                FPopulatingKeyInfo := False;
              end;
            end;
          end;

        finally
          if RecBuf <> nil then
            ColumnsMetaInfo.FreeRecBuf(RecBuf);
          if ColumnsMetaInfo <> nil then begin
            ColumnsMetaInfo.Close;
            ColumnsMetaInfo.UnPrepare;
          end;
          if IsLibrary then
            FreeAndNil(FCommand.FConnection.FColumnsMetaInfo);
        end;
      finally
      {$IFDEF CLR}
        if CMIRowset <> nil then
          Marshal.ReleaseComObject(CMIRowset);
      {$ENDIF}
      end;
      Assert(not (FCursorType in [ctStatic, ctKeySet]) or not ((FBookmarkOffset = - 2) and (BookmarkField <> nil)));

      FillTablesAliases;
    // For
    //   SELECT FieldName FieldAlias FROM TableName TableAlias
    // must be
    //   Field.Name = 'FieldAlias'
    //   Field.ActualName = 'FieldAlias'
    //   Field.TableName = '' ???
    //   Field.BaseColumnName = 'c_int'
    //   Field.BaseTableName = 'ALL_TYPES'

    end;
  {$ENDIF}

  var
    Connection: TOLEDBConnection;

  begin
    if ByInfo then begin
      if FNativeRowset or not FIsColumnsRowset then
        CreateFieldDescsByInfo
      else
      begin
        // Detect true connection
        Connection := FCommand.FConnection;

        if Connection.FColumnsRowsetFieldDescs <> nil then
          Connection.AssignFieldDescs(Connection.FColumnsRowsetFieldDescs, FFields)
        else
        begin
          CreateFieldDescsByInfo;
          Connection.FColumnsRowsetFieldDescs := TFieldDescs.Create;
          try
            try
              Connection.AssignFieldDescs(FFields, Connection.FColumnsRowsetFieldDescs) // Save fields for columnsrowset
            except
              FreeAndNil(Connection.FColumnsRowsetFieldDescs);
              raise;
            end;
          finally
            if IsLibrary then
              FreeAndNil(Connection.FColumnsRowsetFieldDescs);
          end;
        end;
      end;
    end
    else
    {$IFNDEF LITE}
      CreateFieldDescsByRowset;
    {$ELSE}
      Assert(False);
    {$ENDIF}
  end;

begin
  inherited; // Empty proc call

  if not FNativeRowset then
    CommandType := ctCursor;

  if CommandType = ctUnknown then begin // This is a FieldDefs.Update call

    QueryCommandInterfaces(False);
    try
      SetCommandProp;
      FCommand.Execute;
    finally
      ReleaseCommandInterfaces; /// FCommand.QueryIntCnt counter is increased in inherited
      FCommand.SetCursorState(csInactive); // copied from OpenCursor method to prevent
                                           // blocking execute after fielddefs.update
    end;
    CreateFieldDescs(FCommand.FIUnknown, not FRequestSQLObjects);

    // Free interfaces
  {$IFDEF CLR}
    if FCommand.FIMultipleResults <> nil then
      Marshal.ReleaseComObject(FCommand.FIMultipleResults);
    if FCommand.FIUnknown <> nil then
      Marshal.ReleaseComObject(FCommand.FIUnknown);
  {$ENDIF}
    FCommand.FIUnknown := nil;
    FCommand.ClearIMultipleResults;

    // Free param accessors (must be after clearing interfaces)
    RequestParamsIfPossible;

    // We does not need to process non-Native rowsets or ServerCursors
    // QueryRecordSetInterfaces not required too
    Exit;
  end;

  if not FNativeRowset then
    CreateFieldDescs(FIRowset, True)
  else
  begin
    Assert(FIRowset = nil);
    if Prepared and not FCommand.FRPCCall then
      CreateFieldDescs(FCommand.FICommandPrepare, not FRequestSQLObjects)
    else
      CreateFieldDescs(FCommand.FIUnknown, not FRequestSQLObjects);
  end;
end;

procedure TOLEDBRecordSet.ExplicitInitFields;
var
  NeedReset: boolean;
begin
  NeedReset := GetCommand.GetCursorState <= csPrepared;
  try
    inherited;
  finally
    if NeedReset then begin
      GetCommand.SetCursorState(csInactive);
      CommandType := ctUnknown;
    end;
  end;
end;

procedure TOLEDBRecordSet.AllocFetchBlock;
var
  UsedIRowsetUpdate: boolean;

  function AddFieldToABList(const FieldNum: integer): integer; // Add new AB, if need. If AB already present then return its index
    function AddAB(const BlockType: TAccessorBlockType): integer;
    var
      l: integer;
    begin
      l := Length(FFetchAccessorData.AccessorBlocks);
      SetLength(FFetchAccessorData.AccessorBlocks, l + 1);

      FFetchAccessorData.AccessorBlocks[l].BlockType := BlockType;
      FFetchAccessorData.AccessorBlocks[l].hAcc := 0;
      FFetchAccessorData.AccessorBlocks[l].BlobFieldNum := -1;

      Result := l;
    end;

  var
    BlockType: TAccessorBlockType;
    FieldDesc: TOLEDBFieldDesc;
    i: integer;
    IsLarge: boolean;
    IsDBNumeric: boolean;
  begin
    Result := -1;
    try
      FieldDesc := Fields[FieldNum] as TOLEDBFieldDesc;
      IsLarge := IsLargeDataTypeUsed(FieldDesc);
      IsDBNumeric := IsCompactEdition(ProviderPrimaryVer) and (FieldDesc.SubDataType = dtNumeric);
      if IsNeedFetchBlock(FieldDesc, ProviderPrimaryVer) then
        BlockType := abFetchBlock // This is a string by ref - long string conversion used
      else
        if FieldDesc.ReadOnly and UsedIRowsetUpdate then
          BlockType := abReadOnly
        else
          if IsLarge and UsedIRowsetUpdate then
            BlockType := abBLOB
          else
            BlockType := abOrdinary;

      if not UsedIRowsetUpdate // CR 4082
        and (BlockType <> abBLOB) then
        for i := 0 to Length(FFetchAccessorData.AccessorBlocks) - 1 do
          if FFetchAccessorData.AccessorBlocks[i].BlockType = BlockType then begin
            // Test BLOB fields
            if IsLarge then begin
              if FFetchAccessorData.AccessorBlocks[i].BlobFieldNum = - 1 then
                FFetchAccessorData.AccessorBlocks[i].BlobFieldNum := FieldNum
              else begin
                BlockType := abBLOB;
                Result := AddAB(BlockType);
                FFetchAccessorData.AccessorBlocks[Result].BlobFieldNum := FieldNum;
                Exit;
              end;
            end;
            // Test numeric fields in Everywhere
            if IsDBNumeric then begin
              Result := AddAB(BlockType);
              Exit;
            end;

            Result := i;
            Exit;
          end;

      // Accessor block not found! Create new
      Result := AddAB(BlockType);
      if IsLarge then
        FFetchAccessorData.AccessorBlocks[Result].BlobFieldNum := FieldNum;
    finally
      Assert(Result <> - 1);

      i := Length(FFetchAccessorData.AccessorBlocks[Result].FieldNums);
      SetLength(FFetchAccessorData.AccessorBlocks[Result].FieldNums, i + 1);
      FFetchAccessorData.AccessorBlocks[Result].FieldNums[i] := FieldNum;
    end;

  end;

  // Fill internal structures in accessor block
  procedure FillBindingStructInAccBlock(
    rgBindings: TDBBindingArray;
    var AccessorBlock: TAccessorBlock);
  var
    Cnt, i, l: integer;
    FieldNum: integer;
    Field: TFieldDesc;

    Obj: {$IFNDEF UNIDACPRO}OLEDBIntf{$ELSE}OLEDBIntfUni{$ENDIF}.DBOBJECT;
  begin
    Cnt := Length(AccessorBlock.FieldNums);
    Assert(Cnt > 0);
    //OFS('+FillBindingStructInAccBlock');
    for i := 0 to Cnt - 1 do
      with rgBindings[i] do begin
        FieldNum := AccessorBlock.FieldNums[i];
        Field := FFields[FieldNum];
        iOrdinal := Field.FieldNo;

        obValue := Field.Offset;
        obLength := 0;
        obStatus := DataSize + FieldNum * OLE_DB_INDICATOR_SIZE;

        dwPart := DBPART_VALUE or DBPART_STATUS;
        dwMemOwner := DBMEMOWNER_CLIENTOWNED;
        eParamIO := DBPARAMIO_NOTPARAM;
        cbMaxLen := Field.Size;

        if (FCursorType in [ctStatic, ctKeyset, ctBaseTable]) and (iOrdinal = 0) then
          dwFlags := DBCOLUMNFLAGS_ISBOOKMARK
        else
          dwFlags := 0;

        wType := ConvertInternalTypeToOLEDB(Field.dataType, False, DBMSPrimaryVer);
        if IsCompactEdition(DBMSPrimaryVer) then begin
          case TOLEDBFieldDesc(Field).SubDataType of
            dtSingle: begin
              wType := DBTYPE_R4;
            end;
            dtUInt8: begin
              wType := DBTYPE_UI1;
              cbMaxLen := SizeOf(Word);
            end;
            dtNumeric: begin
              wType := DBTYPE_NUMERIC;
              cbMaxLen := SizeOfTDBNumeric;
            end;
            dtUInt64: begin
              wType := DBTYPE_UI8;
            end;
          end;
        end;

        if not IsLargeDataTypeUsed(Field) then begin //???
          case Field.DataType of
            dtExtString, dtExtWideString:
              begin
                if Field.DataType = dtExtString then
                  l := 1
                else
                if Field.DataType = dtExtWideString then
                  l := 2
                else begin
                  l := 0;
                  Assert(False);
                end;

                Assert(AccessorBlock.BlockType = abFetchBlock);
                dwPart := DBPART_VALUE or DBPART_STATUS or DBPART_LENGTH;
                cbMaxLen := MaxNonBlobFieldLen + l;
                obStatus := FFetchBlockSize;
                obLength := obStatus + OLE_DB_INDICATOR_SIZE;
                obValue := obLength + sizeof(DBLENGTH);
                IncFetchBlockOffset(FFetchBlockSize, Field);
              end;
            dtMemo, dtWideMemo, dtMSXML: // Long string conversion used
              begin
                Assert(AccessorBlock.BlockType = abFetchBlock);
                if (Field.SubDataType and dtWide) = 0 then begin
                  wType := DBTYPE_STR;
                  l := 1;
                end
                else begin
                  wType := DBTYPE_WSTR;
                  l := 2;
                end;
                cbMaxLen := MaxNonBlobFieldLen + l;
                obStatus := FFetchBlockSize;
                obValue := obStatus + OLE_DB_INDICATOR_SIZE;
                IncFetchBlockOffset(FFetchBlockSize, Field);
              end;
            dtVariant:
              begin
                Assert(AccessorBlock.BlockType = abFetchBlock);
                wType := DBTYPE_VARIANT;
                cbMaxLen := SizeOf_OleVariant;
                obStatus := FFetchBlockSize;
                obValue := obStatus + OLE_DB_INDICATOR_SIZE;
                IncFetchBlockOffset(FFetchBlockSize, Field);
              end;
            dtBytes:
              if not FReadOnly then begin
                dwPart := DBPART_VALUE or DBPART_STATUS or DBPART_LENGTH;
                obValue := Field.Offset;
                obLength := obValue + Field.Length;
                cbMaxLen := Field.Length;
              end;
            dtVarBytes:
              begin
                dwPart := DBPART_VALUE or DBPART_STATUS or DBPART_LENGTH;
                obValue := Field.Offset + sizeof(word);
                obLength := obValue + Field.Length;
                cbMaxLen := Field.Length;
              end;
            dtExtVarBytes:
              begin
                Assert(AccessorBlock.BlockType = abFetchBlock);
                cbMaxLen := MaxNonBlobFieldLen; // WAR on changing must change FetchExternalAccessorBlock
                dwPart := DBPART_VALUE or DBPART_STATUS or DBPART_LENGTH;
                obStatus := FFetchBlockSize;
                obValue := obStatus + OLE_DB_INDICATOR_SIZE;
                obLength := obValue + cbMaxLen;
                IncFetchBlockOffset(FFetchBlockSize, Field);
              end;
            dtBlob:
              begin
                Assert(AccessorBlock.BlockType = abFetchBlock);
                wType := DBTYPE_BYTES;
                cbMaxLen := Field.Length;
                dwPart := DBPART_VALUE or DBPART_STATUS or DBPART_LENGTH;
                obStatus := FFetchBlockSize;
                obValue := obStatus + OLE_DB_INDICATOR_SIZE;
                obLength := obValue + cbMaxLen;
                IncFetchBlockOffset(FFetchBlockSize, Field);
              end;
            dtFloat, dtBcd:
              if {IsCompactEdition(DBMSPrimaryVer) and} (TOLEDBFieldDesc(Field).SubDataType = dtNumeric) then begin
                Assert(AccessorBlock.BlockType = abFetchBlock);
                wType := DBTYPE_NUMERIC;
                cbMaxLen := SizeOfTDBNumeric;
                obStatus := FFetchBlockSize;
                obValue := obStatus + OLE_DB_INDICATOR_SIZE;
                IncFetchBlockOffset(FFetchBlockSize, Field);
              end;
          end;
        end
        else
        begin
          Assert(Field.DataType in [dtMemo, dtWideMemo, dtBlob, dtMSXML], 'Non-compartible values of Field.DataType and IsStreamUsed');

          Obj.iid := IID_ISequentialStream;
          Obj.dwFlags := STGM_READ;
          pObject := Marshal.AllocHGlobal(sizeof(DBOBJECT));
        {$IFDEF CLR}
          Marshal.StructureToPtr(TObject(Obj), pObject, False);
        {$ELSE}
          DBOBJECT(pObject^) := Obj;
        {$ENDIF}

          if not FReadOnly then begin
            dwPart := DBPART_VALUE or DBPART_STATUS or DBPART_LENGTH;
            obLength := obValue + sizeof(IntPtr);
          end;

        end;

        if wType in [DBTYPE_NUMERIC, DBTYPE_VARNUMERIC] then begin
          bPrecision := Field.Length;
          bScale := Field.Scale;
        end;

        {OFS('---');
        OFS('Field.Name = ' + Field.Name);
        OFS('iOrdinal = ' + IntToStr(iOrdinal));
        OFS('obValue = ' + IntToStr(obValue));
        OFS('obLength = ' + IntToStr(obLength));
        OFS('obStatus = ' + IntToStr(obStatus));
        OFS('pTypeInfo = ' + IntToStr(Integer(pTypeInfo)));
        //OFS('pObject = ' + IntToStr(Integer(pObject)));
        OFS('pBindExt = ' + IntToStr(Integer(pBindExt)));
        OFS('dwPart = ' + IntToStr(Integer(dwPart)));
        //dwMemOwner: DBMEMOWNER;
        //eParamIO: DBPARAMIO;
        OFS('cbMaxLen = ' + IntToStr(cbMaxLen));
        OFS('dwFlags = ' + IntToStr(dwFlags));
        OFS('wType = ' + IntToStr(Integer(wType)));
        OFS('bPrecision = ' + IntToStr(bPrecision));
        OFS('bScale = ' + IntToStr(bScale));}
      end;
    //OFS('-FillBindingStructInAccBlock');
  end;

var
  i, j: integer;

  rgStatus: PDBBINDSTATUSArray;
  rgBindings: TDBBindingArray;
{$IFDEF CLR}
  rgBindingsGC: GCHandle;
{$ENDIF}
  hr: HResult;
  FieldCntAB: integer;

begin
  rgStatus := nil;

  FFetchBlock := nil;
  FFetchBlockSize := 0;
  UsedIRowsetUpdate := (FIRowsetUpdate <> nil) and
    (FCursorType <> TMSCursorType(ctTableType)); // for TableTypeRecordSet
  QueryIntf(FIRowset, {$IFDEF CLR}IAccessor{$ELSE}IID_IAccessor{$ENDIF}, FFetchAccessorData.Accessor);

  // Separate fields to AccessorBlocks
  for i := 0 to Fields.Count - 1 do
    if (Fields[i].FieldDescKind = fdkData) and
       ((FCursorType <> TMSCursorType(ctTableType)) or not Fields[i].ReadOnly) then // don't set readonly fields for TVP
      AddFieldToABList(i);

  // CreateAccessors
  for i := 0 to Length(FFetchAccessorData.AccessorBlocks) - 1 do begin
    FieldCntAB := Length(FFetchAccessorData.AccessorBlocks[i].FieldNums);
    try
      rgStatus := Marshal.AllocHGlobal(FieldCntAB * SizeOf(DBBINDSTATUS));
      SetLength(rgBindings, FieldCntAB);

      for j := 0 to FieldCntAB - 1 do
        with rgBindings[j] do begin
        {$IFDEF CLR}
          if pTypeInfo <> nil then
            Marshal.Release(pTypeInfo);
        {$ELSE}
          pTypeInfo := nil;
        {$ENDIF}
          pObject := nil;
          pBindExt := nil;
        end;

      FillBindingStructInAccBlock(rgBindings, FFetchAccessorData.AccessorBlocks[i]);

      // Create accessor
    {$IFDEF CLR}
      rgBindingsGC := GCHandle.Alloc(rgBindings, GCHandleType.Pinned);
      try
        hr := FFetchAccessorData.Accessor.CreateAccessor(DBACCESSOR_ROWDATA, FieldCntAB, Marshal.UnsafeAddrOfPinnedArrayElement(rgBindings, 0), 0, FFetchAccessorData.AccessorBlocks[i].hAcc, rgStatus);
      finally
        rgBindingsGC.Free;
      end;
    {$ELSE}
      hr := FFetchAccessorData.Accessor.CreateAccessor(DBACCESSOR_ROWDATA, FieldCntAB, rgBindings, 0, FFetchAccessorData.AccessorBlocks[i].hAcc, rgStatus);
    {$ENDIF}
      Check(hr);
    finally
      if Length(rgBindings) <> 0 then begin
        for j := 0 to FieldCntAB - 1 do
          if rgBindings[j].pObject <> nil then
            Marshal.FreeHGlobal(rgBindings[j].pObject);
        SetLength(rgBindings, 0);
      end;
      if rgStatus <> nil then begin
        Marshal.FreeHGlobal(rgStatus);
        rgStatus := nil;
      end;
    end;
  end;

  if FFetchBlockSize <> 0 then
    FFetchBlock := Marshal.AllocHGlobal(FFetchBlockSize);
end;

procedure TOLEDBRecordSet.FreeFetchBlock;
var
  AccNum: integer;
begin
  if FFetchAccessorData.Accessor = nil then
    Exit;

  with FFetchAccessorData do begin
    for AccNum := 0 to Length(AccessorBlocks) - 1 do
      Check(Accessor.ReleaseAccessor(AccessorBlocks[AccNum].hAcc, nil));
  {$IFDEF CLR}
    Marshal.ReleaseComObject(Accessor);
  {$ENDIF}
    Accessor := nil;
    SetLength(AccessorBlocks, 0);
  end;

  if FFetchBlock <> nil then
    Marshal.FreeHGlobal(FFetchBlock);
end;

procedure TOLEDBRecordSet.InitFetchedBlock(Block: PBlockHeader; RowsObtained: Integer; FetchBack: boolean);
begin
  case CursorTypeForFetching of
    ctDefaultResultSet, ctBaseTable:
      inherited;
    ctStatic, ctKeySet: begin
      if FBookmarkOffset = - 1 then
        FBookmarkOffset := sizeof(TBlockHeader) + sizeof(TItemHeader) + Fields[Fields.Count - 1].Offset;
      LastItem.Order := Marshal.ReadInt32(IntPtr(Block), FBookmarkOffset);
      FBookmarkValue := LastItem.Order;
    end;
    ctDynamic:
      LastItem.Order := 0;
  end;
end;

function TOLEDBRecordSet.Fetch(FetchBack: boolean = False): boolean;
var
  OldFetchFromBookmark: boolean;

  procedure GetDataFromRow(const Row: HROW; const pRec: IntPtr);
    procedure PrepareConvertableFields; // After get data from OLEDB
    var
      i: integer;
      pValue: IntPtr;

      OleDbBuf: IntPtr;
      Field: TOLEDBFieldDesc;

    {$IFDEF VER6P}
    {$IFOPT C+}
      FieldStatus: DWORD;
    {$ENDIF}
    {$IFDEF CLR}
      BcdOut: TBcd;
      g: TGuid;
      p: IntPtr;
      j: integer;
      BcdBuf: TBytes;
      s: AnsiString;
      DBNum: TDBNumeric;
      Data: TBytes;
    {$ELSE}
      FieldLength, FieldScale: word;
    {$IFDEF VER9}
      Delta: word;
    {$ENDIF}
    {$ENDIF}
      Bcd: TBcd;
    {$ENDIF}
      DBTimeStamp: TDBTimeStamp;
      dt: TDateTime;
      d: double;
      CurrTimestamp: Int64;
      TimeStamp: TTimeStamp;
    begin
      for i := 0 to Fields.Count - 1 do begin
        Field := TOLEDBFieldDesc(Fields[i]);
        if Field.FieldDescKind <> fdkData then
          SetNull(i + 1, pRec, True)
        else
        begin
          pValue := PtrOffset(pRec, Field.Offset);

          // Get max Timestamp value for RefreshQuick
          if Field.IsTimestamp and (Field.TableInfo <> nil) then begin
          {$IFDEF CLR}
            SetLength(Data, SizeOf(Int64));
            Marshal.Copy(pValue, Data, 0, SizeOf(Int64));
            System.Array.Reverse(Data, 0, SizeOf(Int64));
            CurrTimestamp := BitConverter.ToInt64(Data, 0);
          {$ELSE}
            CurrTimestamp := Marshal.ReadInt64(pValue);
            Reverse8(@CurrTimestamp);
          {$ENDIF}
            if {$IFDEF VER7P}UInt64{$ENDIF}(TOLEDBTableInfo(Field.TableInfo).FMaxTimestamp) < {$IFDEF VER7P}UInt64{$ENDIF}(CurrTimestamp) then
              TOLEDBTableInfo(Field.TableInfo).FMaxTimestamp := CurrTimestamp;
          end;

          OleDbBuf := nil;
          try
            case Field.DataType of
              dtVarBytes:
                Marshal.WriteInt16(pValue, SmallInt(UINT(Marshal.ReadInt32(pValue, sizeof(word) + Field.Length))));
            {$IFDEF VER6P}
            {$IFNDEF FPC}
              dtFmtBCD:
                if not GetNull(i + 1, pRec) then begin
                {$IFOPT C+}
                  FieldStatus := GetStatus(i, pRec);
                  Assert(FieldStatus = DBSTATUS_S_OK, Field.Name +  ': FieldStatus = $' + IntToHex(FieldStatus, 8){ + ', Value = ' + Marshal.PtrToStringAnsi(pValue)});
                {$ENDIF}
                {$IFDEF CLR}
                  DBNum := TDBNumeric(Marshal.PtrToStructure(pValue, TypeOf(TDBNumeric)));
                  Bcd := DBNumericToBCD(DBNum);
                  NormalizeBcd(Bcd, BcdOut, Field.Length, Field.Scale);

                  // Copied from TBcd.ToBytes
                  SetLength(BcdBuf, 34);
                  BcdBuf[0] := BcdOut.Precision;
                  BcdBuf[1] := BcdOut.SignSpecialPlaces;
                  for j := 0 to 31 do
                    BcdBuf[j + 2] := BcdOut.Fraction[j];
                  Marshal.Copy(BcdBuf, 0, pValue, 34);
                {$ELSE}
                  Bcd := DBNumericToBCD(TDBNumeric(pValue^));

                  FieldLength := Field.Length;
                  FieldScale := Field.Scale;
                {$IFDEF VER9} // Delphi 9 NormalizeBcd Bug
                  Delta := FieldLength - FieldScale;
                  if Delta > 34 then begin
                    Delta := 34;
                    FieldLength := FieldScale + Delta;
                  end;
                {$ENDIF}
                  NormalizeBcd(Bcd, PBcd(pValue)^, FieldLength, FieldScale);
                {$ENDIF}
                end;
            {$ENDIF}
            {$ENDIF}
            {$IFDEF VER5P}
              dtGuid:
                if not GetNull(i + 1, pRec) then
                {$IFDEF CLR}
                begin
                  g := TGUID(Marshal.PtrToStructure(pValue, TypeOf(TGUID)));
                  s := '{' + GUIDToString(g) + '}';
                  p := Marshal.StringToHGlobalAnsi(s);
                  try
                    StrLCopy(pValue, p, 38);
                  finally
                    Marshal.FreeHGlobal(p);
                  end;
                end;
                {$ELSE}
                  StrLCopy(PAChar(pValue), PAChar(@AnsiString(GUIDToString(PGUID(pValue)^))[1]), 38);
                {$ENDIF}
            {$ENDIF}
              dtTime:
                if not GetNull(i + 1, pRec) then begin
                {$IFDEF CLR}
                  dt := BitConverter.Int64BitsToDouble(Marshal.ReadInt64(pValue));
                {$ELSE}
                  dt := PDateTime(pValue)^;
                {$ENDIF}
                  TimeStamp := DateTimeToTimeStamp(dt);
                  TimeStamp.Date := DateDelta;
                  dt := TimeStampToDateTime(TimeStamp);
                  Marshal.WriteInt64(pValue, BitConverter.DoubleToInt64Bits(Double(dt)));
                end;
              dtDateTime:
                if (Provider = prCompact) and (not GetNull(i + 1, pRec)) then begin
                {$IFDEF CLR}
                  DBTimeStamp := TDBTimeStamp(Marshal.PtrToStructure(pValue, TypeOf(TDBTimeStamp)));
                {$ELSE}
                  DBTimeStamp := PDBTimeStamp(pValue)^;
                {$ENDIF}
                  dt := {$IFNDEF CLR}MemUtils.{$ENDIF}EncodeDateTime(DBTimeStamp.year, DBTimeStamp.month, DBTimeStamp.day, DBTimeStamp.hour, DBTimeStamp.minute, DBTimeStamp.second, DBTimeStamp.fraction div 1000000{Billionths of a second to milliseconds});
                  Marshal.WriteInt64(pValue, BitConverter.DoubleToInt64Bits(Double(dt)));
                end;
              dtCurrency:
                if Provider = prCompact then begin
                {$IFDEF CLR}
                  d := Marshal.ReadInt64(pValue);
                  d := d / 10000;
                {$ELSE}
                  d := Currency(pValue^);
                {$ENDIF}
                  Marshal.WriteInt64(pValue, BitConverter.DoubleToInt64Bits(d));
                end;
              dtFloat:
                if (not GetNull(i + 1, pRec)) and (Provider = prCompact) and (FCursorType <> ctBaseTable) then begin
                  if (Field.SubDataType = dtSingle) then begin
                  {$IFDEF CLR}
                    BcdBuf := BitConverter.GetBytes(Marshal.ReadInt64(pValue));
                    d := BitConverter.ToSingle(BcdBuf, 0);
                  {$ELSE}
                    d := Single(pValue^);
                  {$ENDIF}
                    Marshal.WriteInt64(pValue, BitConverter.DoubleToInt64Bits(d));
                  end
                end;
              dtWord:
                if (not GetNull(i + 1, pRec)) and (Provider = prCompact) and (FCursorType <> ctBaseTable) and (Field.SubDataType = dtUInt8) then
                  Marshal.WriteByte(PtrOffset(pValue, 1), 0);
            end;
          finally
            if OleDbBuf <> nil then
              FreeCoMem(OleDbBuf);
          end;
        end;
      end;
    end;

    procedure FetchPlainAccessorBlock(const AccessorBlock: TAccessorBlock);
    var
      hr: HResult;
      Blob: TBlob;
      Field: TFieldDesc;
      pValue: IntPtr;
      Length: integer;

    begin
      // Get data from IRowset
      hr := FIRowset.GetData(Row, AccessorBlock.hAcc, pRec);
      Check(hr, AnalyzeFieldsStatus, pRec);

      // ConvertMemoToBlob;
      if AccessorBlock.BlobFieldNum <> -1 then begin
        Field := Fields[AccessorBlock.BlobFieldNum];
        pValue := PtrOffset(pRec, Field.Offset);
        Length := Marshal.ReadInt32(pRec, Field.Offset + SizeOf(IntPtr));
        if FReadOnly then
          Length := 0;
      {$IFDEF HAVE_COMPRESS}
        if Field.DataType = dtBlob then
          Blob := TCompressedBlob.Create((Field.SubDataType and dtWide) <> 0)
        else
      {$ENDIF}
          Blob := TBlob.Create((Field.SubDataType and dtWide) <> 0);

        try
          if (GetStatus(AccessorBlock.BlobFieldNum, pRec) <> DBSTATUS_S_ISNULL) then // Can't use GetNull->GetNullByBlob
            ConvertStreamToBlob(pValue, Length, Blob{$IFDEF HAVE_COMPRESS}, FCommand.FCompressBlob{$ENDIF},
            {$IFDEF LITE}((not dtWide) and Field.SubDataType = dtMSXML){$ELSE}(Field.DataType = dtMSXML){$ENDIF});
        finally
          Marshal.WriteIntPtr(pValue, Blob.GCHandle);
        end;
      end;
    end;

  var
    FetchBlockOffset: integer;

    procedure FetchExternalAccessorBlock(const AccessorBlock: TAccessorBlock);
    var
      hr: HResult;
      i, FieldNum: integer;
      Status: DWORD;
      Obj: TSharedObject;
      Blob: TBlob;
      pc: IntPtr;
      pValue, pFetchBlockValue: IntPtr;

      Field: TFieldDesc;
      l: integer;

      Size: word;
      HeapBuf: IntPtr;
      t: boolean;
      DBNumeric: TDBNumeric;
      d: double;
    {$IFNDEF CLR}
      c: currency;
    {$ENDIF}
      i64: Int64;

    begin
      Assert(AccessorBlock.BlobFieldNum = - 1);
      // Get data from IRowset
      hr := FIRowset.GetData(Row, AccessorBlock.hAcc, FFetchBlock);

      // Copy status from external buf to pRec. Need to correct work CheckAndAnalyzeFieldsStatus
      for i := 0 to Length(AccessorBlock.FieldNums) - 1 do begin
        FieldNum := AccessorBlock.FieldNums[i];
        Field := Fields[FieldNum];
        if IsNeedFetchBlock(Field, ProviderPrimaryVer) and (Field.FieldDescKind = fdkData) then begin
          Status := DWORD(Marshal.ReadInt32(FFetchBlock, FetchBlockOffset));
          SetStatus(FieldNum, pRec, Status);

          pFetchBlockValue := PtrOffset(FFetchBlock, FetchBlockOffset + OLE_DB_INDICATOR_SIZE);
          pValue := PtrOffset(pRec, Field.Offset);

          case Field.DataType of
            dtExtString, dtExtWideString: begin
              if GetNull(FieldNum + 1, pRec) then
                Marshal.WriteIntPtr(pValue, nil)
              else begin
                if Field.Fixed then
                  t := TrimFixedChar
                else
                  t := TrimVarChar;
                l := Marshal.ReadInt32(pFetchBlockValue);
                pFetchBlockValue := PtrOffset(pFetchBlockValue, sizeof(DBLENGTH));
                if Field.DataType = dtExtString then
                  Marshal.WriteIntPtr(pValue, StringHeap.AllocStr(pFetchBlockValue, t, l))
                else
                  Marshal.WriteIntPtr(pValue, StringHeap.AllocWideStr(pFetchBlockValue, t, l div 2));
              end;
            end;
            dtExtVarBytes:
              if GetNull(FieldNum + 1, pRec) then
                Marshal.WriteIntPtr(pValue, nil)
              else begin
                Size := UINT(Marshal.ReadInt32(pFetchBlockValue, MaxNonBlobFieldLen));
                HeapBuf := StringHeap.NewBuf(Size + sizeof(Word));
                CopyBuffer(pFetchBlockValue, PtrOffset(HeapBuf, sizeof(Word)), Size);
                Marshal.WriteIntPtr(pValue, HeapBuf);
                Marshal.WriteInt16(HeapBuf, SmallInt(Word(Size)));
              end;
            dtMemo, dtWideMemo, dtMSXML: begin
              Blob := TBlob.Create((Field.SubDataType and dtWide) <> 0);
              if Status <> DBSTATUS_S_ISNULL then begin
                pc := pFetchBlockValue;
                if Blob.IsUnicode then
                  l := integer(StrLenW(pc)) * integer(sizeof(WideChar)) // D2005 CLR bug
                else
                  l := StrLen(PAChar(pc));
                if l > 0 then begin
                  Blob.RollbackEnabled := False;
                  Blob.Write(0, l, pc);
                  Blob.RollbackEnabled := True;
                end;
              end;
              Marshal.WriteIntPtr(pValue, Blob.GCHandle);
            end;
            dtBlob: begin
              Assert(Field.SubDataType = dtMSUDT);
              Blob := TBlob.Create;
              if Status <> DBSTATUS_S_ISNULL then begin
                l := Marshal.ReadInt32(pFetchBlockValue, Field.Length);
                if l > 0 then begin
                  Blob.RollbackEnabled := False;
                  Blob.Write(0, l, pFetchBlockValue);
                  Blob.RollbackEnabled := True;
                end;
              end;
              Marshal.WriteIntPtr(pValue, Blob.GCHandle);
            end;
            dtVariant: begin
              Obj := TVariantObject.Create;

              TVariantObject(Obj).Value := GetOleVariant(pFetchBlockValue);
              OleVarClear(pFetchBlockValue);

              Marshal.WriteIntPtr(pValue, Obj.GCHandle);
            end;
            dtFloat, dtBcd:
              if not GetNull(FieldNum + 1, pRec) then begin
              {$IFDEF CLR}
                DBNumeric := TDBNumeric(Marshal.PtrToStructure(pFetchBlockValue, TypeOf(TDBNumeric)));
              {$ELSE}
                DBNumeric := TDBNumeric(pFetchBlockValue^);
              {$ENDIF}
                d := DBNumericToDouble(DBNumeric);
                if Field.DataType = dtFloat then
                  Marshal.WriteInt64(pValue, BitConverter.DoubleToInt64Bits(d))
                else begin
                {$IFDEF CLR}
                  d := d * 10000;
                  i64 := Round(d);
                {$ELSE}
                  c := d;
                  i64 := Int64((@c)^);
                {$ENDIF}
                  Marshal.WriteInt64(pValue, i64);
                end;
              end;
            else
              Assert(False);
          end;
          IncFetchBlockOffset(FetchBlockOffset, Field);
          Assert(FetchBlockOffset <= FFetchBlockSize);

        end;
      end;
      Check(hr, AnalyzeFieldsStatus, pRec);
    end;

  var
    AccNum: integer;

  begin
    FetchBlockOffset := 0;
    for AccNum := 0 to Length(FFetchAccessorData.AccessorBlocks) - 1 do
      if FFetchAccessorData.AccessorBlocks[AccNum].BlockType = abFetchBlock then
        FetchExternalAccessorBlock(FFetchAccessorData.AccessorBlocks[AccNum])
      else
        FetchPlainAccessorBlock(FFetchAccessorData.AccessorBlocks[AccNum]);

    PrepareConvertableFields;
  end;

  procedure InitBlock(pHBlock: PBlockHeader);
  var
    i, j: integer;
    Ptr: IntPtr;
    Field: TFieldDesc;
  begin
    if not HasComplexFields then
      Exit;

  // Create complex filds
    for i := 0 to pHBlock.ItemCount - 1 do begin
      Ptr := PtrOffset(pHBlock, sizeof(TBlockHeader) + i * (RecordSize + sizeof(TItemHeader)) + sizeof(TItemHeader));

      /// We does not need to call CreateComplexFields(Ptr, True) because (in difference with ODAC) we fetch BLOB(IStream) IntPtrs directly to RecBuf
      for j := 0 to FieldCount - 1 do begin
        Field := Fields[j];
        if Field.FieldDescKind <> fdkCalculated then
          case Field.DataType of
            dtBlob, dtMemo, dtWideMemo, dtMSXML,  dtVariant, dtExtString, dtExtWideString, dtExtVarBytes:
              Marshal.WriteIntPtr(Ptr, Field.Offset, nil);
          end;
      end;
    end;
  end;

  procedure ClearBlock(pHBlock: PBlockHeader);
  var
    i: integer;
    Free: PItemHeader;
  begin
    if IntPtr(pHBlock) = nil then
      Exit;

    // Free complex filds
    Free := PtrOffset(pHBlock, sizeof(TBlockHeader));
    for i := 1 to pHBlock.ItemCount do begin
      if HasComplexFields and (Free.Flag <> flFree) then
        FreeComplexFields(PtrOffset(Free, sizeof(TItemHeader)), True);
      Free := PtrOffset(Free, sizeof(TItemHeader) + RecordSize);
    end;
  end;

  function GetRowsFromOLEDB(var RowsObtained: NativeUInt; prghRows: PHROWArray): HResult;
  var
    RowsOffset, RowsRequested: NativeInt;
  {$IFDEF CLR}
    p: IntPtr;
  {$ENDIF}
  begin
    // Backward fetch processing
    if FetchBack then
      RowsRequested := - FFetchRows
    else
      RowsRequested := FFetchRows;

    RowsOffset := 0;

    // Get data from OLEDB
    if FCursorType in [ctKeyset, ctStatic] then begin
      if not FFetchFromBookmark then
        if FetchBack then
          RowsOffset := - 1
        else
          RowsOffset := + 1;

      Assert(FIRowsetLocate <> nil);
      /// FIRowsetLocate.GetRowsAt does not change current IRowset fetch position

    {$IFDEF CLR}
      p := Marshal.AllocHGlobal(SizeOf(Integer));
      try
        Marshal.WriteInt32(p, FBookmarkValue);
        Result := FIRowsetLocate.GetRowsAt(0, DB_NULL_HCHAPTER, FBookmarkSize, p, RowsOffset, RowsRequested, RowsObtained, prghRows);
      finally
        Marshal.FreeHGlobal(p);
      end;
    {$ELSE}
      Result := FIRowsetLocate.GetRowsAt(0, DB_NULL_HCHAPTER, FBookmarkSize, @FBookmarkValue, RowsOffset, RowsRequested, RowsObtained, prghRows);
    {$ENDIF}
    end
    else begin
      if CursorTypeForFetching = ctDynamic then begin
        if FLastFetchOK then begin
          if FFetchFromBookmark then begin // Reread previous readed row
            if FLastFetchBack = FetchBack then begin
              if FetchBack then
                RowsOffset := + 1
              else
                RowsOffset := - 1;
            end;
          end
          else
            if FLastFetchBack <> FetchBack then
              if FetchBack then
                RowsOffset := - 1
              else
                RowsOffset := + 1;
        end
        else
          if FLastFetchBack = FetchBack then
            if FetchBack then
              RowsOffset := + 1
            else
              RowsOffset := - 1;
      end;

      Result := FIRowset.GetNextRows(DB_NULL_HCHAPTER, RowsOffset, RowsRequested, RowsObtained, prghRows);
    end;
    FLastFetchBack := FetchBack;
    FFetchFromBookmark := False; // Clear flag, setted on InternalOpen
  end;

  procedure ProcessNoResult;
  begin
    case CursorTypeForFetching of
      ctDefaultResultSet, ctBaseTable:
        ReleaseAllInterfaces(False); // Process parameters
      ctStatic: begin
        Assert(not FFetchFromBookmark, 'Cannot fetch to bookmark with Static cursor type');
        if not OldFetchFromBookmark {to prevent recursion on empty resultset} then
          FetchToBookmarkValue;
      end;
      ctKeySet: begin
        if FCursorUpdate then
          Assert(not FFetchFromBookmark, 'Cannot fetch to bookmark with KeySet cursor type');
{        else
          FBookmarkValue := - 1;}
      end;
      ctDynamic: begin
        CurrentItem := nil; // FHRow is not accessible and we need to refetch data from server

        if not FProcessDynBofEof then begin
          try
            FProcessDynBofEof := True;
            if FetchBack then begin
              FBof := True;

              // Server cursor position is under first row, FHRow is not accessible
              // Need to call GetNextRows with params (RowsOffset = 1, RowsRequested = - 1)
              FLastFetchOK := True;
              if not FetchToBookmarkValue(True) then
                FEof := True;
            end
            else
            begin
              FEof := True;

              // Server cursor position is below last row, FHRow is not accessible
              // Need to call GetNextRows with params (RowsOffset = - 1, RowsRequested = 1)
              FLastFetchOK := True;
              if not FetchToBookmarkValue then
                FBof := True;
            end;

            if not FLastFetchOK then begin
              FLastFetchOK := True;
              FLastFetchBack := False;
            end;
          finally
            FProcessDynBofEof := False;
          end;

          if FBof or FEof then
            CurrentItem := nil;
        end;
      end;
    end;
  end;

  procedure FirstFetch;
  var
    Field: TOLEDBFieldDesc;
    i: integer;
  begin
    // This is a first call to Fetch.
    // Query interfaces, create accessors etc

    Assert(FCommand.GetCursorState >= csExecuted);
    if FCommand.GetCursorState = csExecuted then
      FCommand.SetCursorState(csFetching);

    QueryRecordSetInterfaces;
    AllocFetchBlock;

    if FCursorType in [ctStatic, ctKeySet] then begin
      // Setting FRecordCount for ctStatic, ctKeySet
      SetToEnd;
      if IntPtr(LastItem) <> nil then begin
        FRecordCount := LastItem.Order;
        SetToBegin;
      end;
      FFetchFromBookmark := FCursorType in [ctKeyset, ctStatic]; /// First record reading without offsetting
    end;
    FBookmarkValue := Integer(DBBMK_FIRST);
    FBookmarkSize := sizeof(FBookmarkValue);

    // Clear MaxTimestamp for RefreshQuick
    for i := 0 to Fields.Count - 1 do begin
      Field := TOLEDBFieldDesc(Fields[i]);
      if Field.IsTimestamp and (Field.TableInfo <> nil) then
        TOLEDBTableInfo(Field.TableInfo).FMaxTimestamp := 0;
    end;
  end;

{$IFDEF FPC}
  procedure ReleaseRows(RowsObtained: UINT; prghRows: PHROWArray);
  begin
    Check(FIRowset.ReleaseRows(RowsObtained, prghRows, nil, nil, nil));
  end;
{$ENDIF}

var
  pHBlock: PBlockHeader;
  NewBlock: boolean;

  hr: HResult;
  RowsObtained: NativeUInt;
  rghRows: THROWArray;
  prghRows: PHROWArray;
  GCHandle: IntPtr;

  i: integer;
  pRec, pData: IntPtr;

  Cancel: boolean;

  IsThisFirstFetch: boolean;
  InThread: boolean;
begin
  Result := False;
  Assert(FCommand <> nil);
  if FCommand.GetCursorState in [csFetched, csInactive] then
    Exit;

{$IFDEF CLR}
  InThread := FCommand.FNonBlocking and (FFetchExecutor <> nil) and (Thread.CurrentThread = FFetchExecutor.Thread.Handle);
{$ELSE}
  InThread := FCommand.FNonBlocking and (FFetchExecutor <> nil) and (GetCurrentThreadId = FFetchExecutor.Thread.ThreadID);
{$ENDIF}
  try
    if Fields.Count = 0 then
      DatabaseError(SNoResultSet, nil); /// Warning - constant SNoResultSet used for detecting in TCustomMSDataSet.OpenNext

    IsThisFirstFetch :=
      ((FCommand.FIUnknown <> nil) or not FNativeRowset)
       and (Length(FFetchAccessorData.AccessorBlocks) = 0);
    try
      if IsThisFirstFetch then
        FirstFetch; // This is a first call to Fetch. FIUnknown tested for prevent recreating accessors after fetching all strings

      DoBeforeFetch(Cancel);

      if Cancel or FWaitForFetchBreak then begin
        if FCommand.GetCursorState in [csFetching, csFetchingAll] then
          FCommand.SetCursorState(csFetched);
        Exit;
      end;

      if FIRowset = nil then
      begin
        Result := False;
        Exit;
      end;

      if (FCursorType = ctKeySet) and not FCursorUpdate and (FBookmarkValue = - 1) then
      begin
        Result := False;
        Exit;
      end;

      OldFetchFromBookmark := FFetchFromBookmark;

       // Clear previous obtained rows
      ClearHRowIfNeed;
      Assert(not FHRowAccessible);

      pHBlock := nil;
      GCHandle := nil;
      prghRows := nil;
      RowsObtained := 0;
      SetLength(rghRows, FFetchRows);
      try
        // Get next rows
        GCHandle := AllocGCHandle(rghRows, True);
        prghRows := GetAddrOfPinnedObject(GCHandle);
        // prghRows := @rghRows[0];
        if FLastFetchEnd then
          RowsObtained := 0
        else
        begin
          hr := GetRowsFromOLEDB(RowsObtained, prghRows);
          FLastFetchEnd := ((FNativeRowset and (FCursorType = ctDefaultResultSet)) or
            (CursorTypeForFetching = ctBaseTable)) and (hr = DB_S_ENDOFROWSET); // CR10007

          // Process rows
          if (hr <> DB_S_ENDOFROWSET)
            and (hr <> DB_S_ROWLIMITEXCEEDED)
            and not ((hr = DB_E_BADBOOKMARK) and (FBookmarkValue = DBBMK_FIRST))
            and not ((hr = DB_E_BADSTARTPOSITION) and (ProviderPrimaryVer = 7))then
            Check(hr);
        end;
        if RowsObtained > 0 then begin
          NewBlock := (IntPtr(BlockMan.FirstBlock) = nil) or (not FUniDirectional and not ServerCursorUsed);
          if NewBlock then
            BlockMan.AllocBlock(pHBlock, RowsObtained)
          else begin
            pHBlock := BlockMan.FirstBlock;
            // Refresh block: drop values of blobs
            ClearBlock(pHBlock);
          end;
          InitBlock(pHBlock);
          pRec := PtrOffset(pHBlock, sizeof(TBlockHeader) + sizeof(TItemHeader));
          for i := 0 to RowsObtained - 1 do
          begin
            pData := PtrOffset(pRec, i * (RecordSize + sizeof(TItemHeader)));
            GetDataFromRow(rghRows[i], pData);
          end;
        end;
      finally
        FreeGCHandle(GCHandle);
        if RowsObtained > 0 then begin
          // Release row handle(s) if need
          if ServerCursorUsed then begin
            Assert(RowsObtained = 1);
            FHRow := rghRows[0];
            FHRowAccessible := True;
          end
          else
          {$IFNDEF FPC}
            Check(FIRowset.ReleaseRows(RowsObtained, prghRows, nil, nil, nil));
          {$ELSE}
            ReleaseRows(RowsObtained, prghRows);
          {$ENDIF}
        end;

        if Length(rghRows) <> 0 then
          SetLength(rghRows, 0);
      end;
      Result := RowsObtained > 0;
      FLastFetchOK := Result;

      if IntPtr(pHBlock) <> nil then begin
        if Result then
          CreateBlockStruct(pHBlock, RowsObtained, FetchBack)
        else
          BlockMan.FreeBlock(pHBlock);
      end;

      if (FNativeRowset or (FCursorType = ctBaseTable)) and (not Result or FLastFetchEnd) then
        ProcessNoResult;

      Assert(not (FCursorType in [ctDynamic]{ServerCursorTypes}) or FProcessDynBofEof or (FHRowAccessible or (FBOF and FEOF)), 'Row must be accessible after Fetch for non-empty dataset with server cursor');
    except
      on e: exception do begin
        if FetchBack then
          FBOF := True
        else
          FEOF := True;

        ClearBlock(pHBlock);
        FreeFetchBlock;
        ReleaseRecordSetInterfaces;

        raise;
      end;
    end;
  finally
    if Assigned(FOnAfterFetch) then
      if InThread and FAfterFetch then
        FFetchExecutor.Thread.SendEvent(TObject(FE_AFTERFETCH))
      else
        DoAfterFetch;
  end;
end;

function TOLEDBRecordSet.GetProp(Prop: integer; var Value: variant): boolean;
begin
  Result := True;
  case Prop of
    prReadOnly:
      Value := FReadOnly;
    prUniqueRecords:
      Value := FUniqueRecords;
    prCursorType:
      Value := Variant(FCursorType);
    prCursorUpdate:
      Value := FCursorUpdate;
    prLockClearMultipleResults:
      Value := FLockClearMultipleResults;
    prIsStoredProc:
      FCommand.GetProp(prIsStoredProc, Value);
    prNonBlocking:
      FCommand.GetProp(prNonBlocking, Value);
    prRowsFetched:
      Value := FRecordCount; //upd1 temporary for RecordCount prop
    prDisableMultipleResults:
      Value := FDisableMultipleResults;
    else
      Result := inherited GetProp(Prop, Value);
  end;
end;

function TOLEDBRecordSet.SetProp(Prop: integer; const Value: variant): boolean;
begin
  Result := True;
  case Prop of
    prReadOnly: begin
      FReadOnly := Value;
      FRequestSQLObjects := FUniqueRecords or not FReadOnly;
    end; 
    prCommandTimeout:
    begin
      Assert(FCommand <> nil);
      FCommand.FCommandTimeout := Value;
    end;
    prUniqueRecords: begin
      FUniqueRecords := Value;
      FRequestSQLObjects := FUniqueRecords or not FReadOnly;
    end;
    prCursorType:
    begin
      FCursorType := TMSCursorType(Value);
      if FCursorType in ServerCursorTypes then
        FFetchAll := False;
    end;
    prRequestSQLObjects:
      FRequestSQLObjects := Value;
    prCursorUpdate:
      FCursorUpdate := Value;
    prLockClearMultipleResults:
      FLockClearMultipleResults := Value;
    prRoAfterUpdate:
      FroAfterUpdate := Value;
    prWideStrings:
      FWideStrings := Value;
    prIsStoredProc:
      FCommand.SetProp(prIsStoredProc, Value);
    prNotificationMessage:
      FCommand.SetProp(prNotificationMessage, Value);
    prNotificationService:
      FCommand.SetProp(prNotificationService, Value);
    prNotificationTimeout:
      FCommand.SetProp(prNotificationTimeout, Value);
    prNonBlocking:
      FCommand.SetProp(prNonBlocking, Value);
    prWideMemos:
      FWideMemos := Value;
    prDisableMultipleResults:
      FDisableMultipleResults := Value;
    prBaseTableName:
      FBaseTableName := Value;
    prBulkExecuting:
      FBulkExecuting := Value;
  else
    Result := inherited SetProp(Prop, Value);
  end;
end;

{ TOLEDBTableTypeRecordSet }

function TOLEDBTableTypeRecordSet.ServerCursorUsed: boolean;
begin
  Result := True;
end;

procedure TOLEDBTableTypeRecordSet.OpenTVPRowset;
var
  IUnk: IUnknown;
  FOpenRowset: IOpenRowset;
  TableID: TDBID;
{$IFDEF CLR}
  GCHandle: IntPtr;
  pTableID: PDBID;
{$ENDIF}
begin
  FCommand.FConnection.Check(FCommand.FConnection.GetIDBCreateSession.CreateSession(nil, IID_IOpenRowset, IUnk), nil);
  FOpenRowset := IOpenRowset(IUnk);

  TableID.eKind := DBKIND_GUID_NAME;
  TableID.uGuid.guid := CLSID_ROWSET_TVP;
  TableID.uName{$IFNDEF CLR}.pwszName{$ENDIF} := Marshal.AllocHGlobal(Length(FTableTypeName) * SizeOf(WideChar) + SizeOf(WideChar));
  try
    CopyBuffer(Marshal.StringToHGlobalUni(WideString(Encoding.Default.GetString(Encoding.Default.GetBytes(FTableTypeName), 0, Length(FTableTypeName)))),
      TableID.uName{$IFNDEF CLR}.pwszName{$ENDIF}, Length(FTableTypeName) * SizeOf(WideChar) + SizeOf(WideChar));
    IUnk := nil;

  {$IFDEF CLR}
    GCHandle := AllocGCHandle(TObject(TableID), True);
    try
      pTableID := GetAddrOfPinnedObject(GCHandle);
      FCommand.FConnection.Check(FOpenRowset.OpenRowset(nil, pTableID, nil, IID_IRowset, 0, nil, IUnk), nil);
    finally
      FreeGCHandle(GCHandle);
    end;
  {$ELSE}
    FCommand.FConnection.Check(FOpenRowset.OpenRowset(nil, @TableID, nil, IID_IRowset, 0, nil, IUnk), nil);
  {$ENDIF}
  finally
    Marshal.FreeHGlobal(TableID.uName{$IFNDEF CLR}.pwszName{$ENDIF});
  end;

  if IUnk = nil then
    raise Exception.Create(STableTypeNotSupported);

  Assert(FIRowset = nil);
  FIRowset := IRowset(IUnk);
  FNativeRowset := False;
  FFlatBuffers := True;
  FFetchAll := False;
  FFetchRows := 1;
  FCommand.FCursorState := csExecuted;
end;

procedure TOLEDBTableTypeRecordSet.InternalOpen(DisableInitFields: boolean = False);
begin
  OpenTVPRowset;

  CommandType := ctUnknown;
  FCursorType := ctDefaultResultSet;
  CachedUpdates := True;
  LocalUpdate := True;

  inherited;
end;

procedure TOLEDBTableTypeRecordSet.ExplicitInitFields;
var
  NeedClose: boolean;
begin
  if CommandType = ctUnknown then begin
    OpenTVPRowset;
    FCursorType := ctDefaultResultSet;
    NeedClose := True;
  end
  else
    NeedClose := False;

  try
    inherited;
  finally
    if NeedClose then begin
      ReleaseRecordSetInterfaces;
      FCommand.FCursorState := csInactive;
      CommandType := ctUnknown;
    end;
  end;
end;

procedure TOLEDBTableTypeRecordSet.InternalPrepare;
begin
//
end;

function TOLEDBTableTypeRecordSet.Fetch(FetchBack: boolean = False): boolean;
begin
  Result := False;
  if FCursorType = ctDefaultResultSet then
    ReleaseRecordSetInterfaces;
end;

function TOLEDBTableTypeRecordSet.GetFilledRowset: IRowset;
var
  OldCurrentItem: PItemHeader;
  RecBuf: IntPtr;
begin
  try
    OpenTVPRowset;
    FCursorType := TMSCursorType(ctTableType);
    QueryIntf(FIRowset, {$IFDEF CLR}IRowsetChange{$ELSE}IID_IRowsetChange{$ENDIF}, FIRowsetUpdate);
    AllocFetchBlock;

    OldCurrentItem := CurrentItem;
    try
      AllocRecBuf(RecBuf);
      try
        SetToBegin;
        while True do begin
          GetNextRecord(RecBuf);
          if EOF then
            break;

          InternalAppendOrUpdate(RecBuf, True);
          ClearHRowIfNeed;
        end;
      finally
        FreeRecBuf(RecBuf);
      end;
    finally
      CurrentItem := OldCurrentItem;
    end;

    Result := FIRowset;
  except
    ReleaseAllInterfaces(True);
    raise;
  end;
end;

function TOLEDBTableTypeRecordSet.SetProp(Prop: integer; const Value: variant): boolean;
begin
  Result := True;
  case Prop of
    prTableTypeName:
      FTableTypeName := Value;
  else
    Result := inherited SetProp(Prop, Value);
  end;
end;

{ TMSTableObject }

procedure TMSTableObject.ReleaseAllInterfaces;
begin
  if FRecordSet <> nil then
    FRecordSet.ReleaseAllInterfaces(True);
end;

function TMSTableObject.GetIsNull: boolean;
begin
  Result := (FRecordSet = nil) or not FRecordSet.Active;
end;

procedure TMSTableObject.SetData(RecordSet: TOLEDBTableTypeRecordSet);
begin
  FRecordSet := RecordSet;
end;

{ TOLEDBTransaction }

destructor TOLEDBTransaction.Destroy;
begin
  ReleaseInterfaces;

  inherited;
end;

procedure TOLEDBTransaction.ReleaseInterfaces;
begin
{$IFDEF CLR}
  if FITransactionLocal <> nil then
    Marshal.ReleaseComObject(FITransactionLocal);
{$ENDIF}

  FITransactionLocal := nil;
end;

{$IFDEF SQL_TRANSACTION}
procedure TOLEDBTransaction.StartTransaction;
begin
{$WARNINGS OFF}
  case FIsolationLevel of
    ilReadCommitted:
      ExecSQL('SET TRANSACTION ISOLATION LEVEL READ COMMITTED');
    ilReadUnCommitted:
      ExecSQL('SET TRANSACTION ISOLATION LEVEL READ UNCOMMITTED');
    ilRepeatableRead:
      ExecSQL('SET TRANSACTION ISOLATION LEVEL REPEATABLE READ');
    ilIsolated:
      ExecSQL('SET TRANSACTION ISOLATION LEVEL SERIALIZABLE ');
    else
      Assert(False, Format('Invalid value %d', [Integer(FIsolationLevel)]));
  end;
{$WARNINGS ON}
  ExecSQL('BEGIN TRAN');
end;

procedure TOLEDBTransaction.Commit;
begin
  ExecSQL('COMMIT');
end;

procedure TOLEDBTransaction.Rollback;
begin
  ExecSQL('ROLLBACK');
end;
{$ELSE}

procedure TOLEDBTransaction.Check(const Status: HRESULT; Component: TObject;
  AnalyzeMethod: TAnalyzeMethod = nil; const Arg: IntPtr = nil);
var
  Connection: TOLEDBConnection;
begin
  if Status <> S_OK then begin
    Assert(FConnections.Count > 0);
    Connection := TOLEDBConnection(FConnections[0]);
    Connection.Check(Status, Component, AnalyzeMethod, Arg);
  end;
end;

procedure TOLEDBTransaction.AssignConnect(Source: TCRTransaction);
begin
  inherited;

  FITransactionLocal := TOLEDBTransaction(Source).FITransactionLocal;
end;

procedure TOLEDBTransaction.StartTransaction;
var
  i: integer;
  Connection: TOLEDBConnection;
  ulTransactionLevel: UINT;
  pulTransactionLevel: IntPtr;
begin
  if FConnections.Count = 0 then
    raise Exception.Create(SNoConnectionsInTransaction);

  if FConnections.Count > 1 then
    raise Exception.Create(SMultiConnectionsInTransaction);

  for i := 0 to FConnections.Count - 1 do
    if not FConnections[i].GetConnected then
      raise Exception.Create(SConnectionInTransactionNotActive);

  Connection := TOLEDBConnection(FConnections[0]);
  QueryIntf(Connection.FISessionProperties, {$IFDEF CLR}ITransactionLocal{$ELSE}IID_ITransactionLocal{$ENDIF}, FITransactionLocal);
  try
    if FITransactionLocal <> nil then begin
    {$IFDEF CLR}
      ulTransactionLevel := 0;
    {$ENDIF}
      pulTransactionLevel := AllocOrdinal(ulTransactionLevel);
      try
        Check(FITransactionLocal.StartTransaction(ConvertIsolationLevelToOLEDBIsoLevel(FIsolationLevel), 0, nil, pulTransactionLevel), Component);
      finally
        FreeOrdinal(pulTransactionLevel);
      end;
      FActive := True;
      FNativeTransaction := True;
    end;
  except
    ReleaseInterfaces;
    raise;
  end;
end;

procedure TOLEDBTransaction.Commit;
begin
  if (FITransactionLocal <> nil) and FNativeTransaction then
    Check(ITransaction(FITransactionLocal).Commit(False{WAR may be troubles with server cursors}, XACTTC_SYNC, 0), Component);
  FActive := False;
end;

procedure TOLEDBTransaction.Rollback;
begin
  if (FITransactionLocal <> nil) and FNativeTransaction then
    Check(ITransaction(FITransactionLocal).Abort(nil, False, False), Component);
  FActive := False;
end;

procedure TOLEDBTransaction.Savepoint(const Name: _string);
var
  Connection: TOLEDBConnection;
  cmd: _string;
begin
  Assert(FConnections.Count = 1);

  Connection := TOLEDBConnection(FConnections[0]);
  //  SELECT is necessary to execute a statement to implicitly start the transaction
  // because ITransactionLocal::BeginTransaction actually execute SET   ON in some cases
  if ((Connection.ProviderPrimaryVer <= 8) or (Connection.DBMSPrimaryVer <= 8))
    and not IsCompactEdition(Connection.DBMSPrimaryVer) then
    cmd := 'IF (@@TRANCOUNT = 0) SELECT 1 FROM SYSOBJECTS WHERE 0=1; ';
  cmd := cmd + 'SAVE TRANSACTION ' + Name;
  Connection.ExecSQL(cmd);
end;

procedure TOLEDBTransaction.RollbackToSavepoint(const Name: _string);
var
  Connection: TOLEDBConnection;
begin
  Assert(FConnections.Count = 1);

  Connection := TOLEDBConnection(FConnections[0]);
  Connection.ExecSQL('ROLLBACK TRANSACTION ' + Name);
end;

{$ENDIF}

{$IFNDEF LITE}
{ TOLEDBMetaData }

function TOLEDBMetaData.CreateRecordSet: TCRRecordSet;
begin
  FRecordSet := TOLEDBRecordSet.Create;
  Result := FRecordSet;
end;

procedure TOLEDBMetaData.InternalGetMetaDataKindsList(List: _TStringList);
begin
  inherited;

  List.Add('Databases');
  List.Sort;
end;

function TOLEDBMetaData.GetSchemaNameFunc: string;
begin
  Assert(FRecordSet <> nil);
  Assert(FRecordSet.FCommand.FConnection <> nil);
  if FRecordSet.FCommand.FConnection.DBMSPrimaryVer >= 9 then
    Result := 'schema_name'
  else
    Result := 'user_name';  
end;

function TOLEDBMetaData.GetTables(Restrictions: _TStrings): TData;
var
  Catalog, TableSchema, TableName, TableTypes, Scope: _string;
  TypesList: _TStringList;
  i: integer;
  rgRestrictions: TRestrictions;
  Rowset: IRowset;
begin
  Catalog := Trim(Restrictions.Values['TABLE_CATALOG']);
  TableSchema := Trim(Restrictions.Values['TABLE_SCHEMA']);
  TableName := Trim(Restrictions.Values['TABLE_NAME']);
  Scope := AnsiUpperCase(Trim(Restrictions.Values['SCOPE']));
  if Scope = 'LOCAL' then
    TableTypes := 'TABLE,VIEW'
  else
    TableTypes := Trim(Restrictions.Values['TABLE_TYPE']);

  SetLength(rgRestrictions, 4);
  if FRecordSet.FCommand.FConnection.FProvider <> prCompact then begin
    rgRestrictions[0] := Catalog;
    rgRestrictions[1] := TableSchema;
  end;
  rgRestrictions[2] := TableName;

  TypesList := _TStringList.Create;
  try
    ParseTypes(TableTypes, TypesList);
    CreateTablesFields;
    FMemData.Open;
    for i := 0 to TypesList.Count - 1 do begin
      rgRestrictions[3] := TypesList[i];
      Rowset := FRecordSet.FCommand.FConnection.GetSchemaRowset(DBSCHEMA_TABLES, rgRestrictions);
      FRecordSet.SetIRowset(Rowset, False);
      FRecordSet.Open;
      CopyTablesData(Restrictions);
      FRecordSet.Close;
    end;
  finally
    TypesList.Free;
  end;

  FMemData.SetToBegin;
  Result := FMemData;
end;

{$IFNDEF DBX_METADATA}
procedure TOLEDBMetaData.CreateTablesFields;
begin
  FMemData.Fields.Clear;
  AddField('TABLE_CATALOG', dtString, 128);
  AddField('TABLE_SCHEMA', dtString, 128);
  AddField('TABLE_NAME', dtString, 128);
  AddField('TABLE_TYPE', dtString, 20);
  AddField('DATE_CREATED', dtDateTime);
  AddField('DATE_MODIFIED', dtDateTime);
  FMemData.InitFields;
end;
{$ENDIF}

procedure TOLEDBMetaData.CopyTablesData(Restrictions: _TStrings);
const
  snCATALOG       = 1;
  snSCHEMA        = 2;
  snTABLE_NAME    = 3;
  snTABLE_TYPE    = 4;
  snDATE_CREATED  = 8;
  snDATE_MODIFIED = 9;

  dnCATALOG       = 1;
  dnSCHEMA        = 2;
  dnTABLE_NAME    = 3;
  dnTABLE_TYPE    = 4;
  dnDATE_CREATED  = 5;
  dnDATE_MODIFIED = 6;
begin
  FMemDataHelper.AllocBuffer;
  FRecordSetHelper.AllocBuffer;
  while FRecordSetHelper.NextRecord do begin
    FMemDataHelper.InitRecord;
    CopyRecord([snCATALOG, snSCHEMA, snTABLE_NAME, snTABLE_TYPE, snDATE_CREATED, snDATE_MODIFIED],
      [dnCATALOG, dnSCHEMA, dnTABLE_NAME, dnTABLE_TYPE, dnDATE_CREATED, dnDATE_MODIFIED]);
    FMemDataHelper.AppendRecord;
  end;
end;

function TOLEDBMetaData.GetColumns(Restrictions: _TStrings): TData;
const
  fmtGetColumnsSQLAzure =
    'SELECT '#$D#$A +
    '  CAST(db_name() AS VARCHAR(128)) AS TABLE_CATALOG,'#$D#$A +
    '  CAST(schema_name(o.uid) AS VARCHAR(128)) AS TABLE_SCHEMA,'#$D#$A +
    '  CAST(o.name AS VARCHAR(128)) AS TABLE_NAME,'#$D#$A +
    '  CAST(c.name AS VARCHAR(128)) AS COLUMN_NAME,'#$D#$A +
    '  c.colorder /*WAR undoc*/ AS POSITION,'#$D#$A +
    '  c.xtype AS DATA_TYPE,'#$D#$A +
    '  CASE '#$D#$A +
    '    WHEN c.xtype = 36 THEN 38 -- uniqueidentifier (GUID) '#$D#$A +
    '    ELSE c.length '#$D#$A +
    '  END AS DATA_LENGTH,'#$D#$A +
    '  CASE '#$D#$A +
    '    WHEN c.scale IS NULL THEN NULL '#$D#$A +
    '    ELSE c.prec '#$D#$A +
    '  END AS DATA_PRECISION,'#$D#$A +
    '  c.scale AS DATA_SCALE,'#$D#$A +
    '  c.isnullable AS NULLABLE,'#$D#$A +
    '  dc.definition AS DEFAULT_VALUE '#$D#$A +
    'FROM '#$D#$A +
    '  sysobjects o, syscolumns c '#$D#$A +
    '  LEFT JOIN sys.default_constraints dc ON (c.cdefault = dc.object_id) '#$D#$A +
    'WHERE '#$D#$A +
    '  o.type in (''U'', ''V'', ''S'') '#$D#$A +
    '  AND o.id = c.id '#$D#$A +
    '  %s'#$D#$A +
    'ORDER BY o.uid, o.name, c.colorder';

var
  Catalog, TableSchema, TableName, ColumnName: _string;
  rgRestrictions: TRestrictions;
  Rowset: IRowset;
  WhereClause: _string;
begin
  Catalog := Trim(Restrictions.Values['TABLE_CATALOG']);
  TableSchema := Trim(Restrictions.Values['TABLE_SCHEMA']);
  TableName := Trim(Restrictions.Values['TABLE_NAME']);
  ColumnName := Trim(Restrictions.Values['COLUMN_NAME']);

  if FRecordSet.FCommand.FConnection.IsSQLAzureEdition then begin
    WhereClause := '';
    AddWhere(WhereClause, 'schema_name(o.uid)', TableSchema);
    AddWhere(WhereClause, 'o.name', TableName);
    AddWhere(WhereClause, 'c.name', ColumnName);
    if WhereClause <> '' then
      WhereClause := ' AND ' + WhereClause;

    FRecordSet.SetSQL(_Format(fmtGetColumnsSQLAzure, [WhereClause]));
    FRecordSet.Open;
    Result := FRecordSet;
    Exit;
  end;

  SetLength(rgRestrictions, 4);
  if FRecordSet.FCommand.FConnection.FProvider <> prCompact then begin
    rgRestrictions[0] := Catalog;
    rgRestrictions[1] := TableSchema;
  end;
  rgRestrictions[2] := TableName;
  rgRestrictions[3] := ColumnName;

  Rowset := FRecordSet.FCommand.FConnection.GetSchemaRowset(DBSCHEMA_COLUMNS, rgRestrictions);
  FRecordSet.SetIRowset(Rowset, False);
  FRecordSet.Open;

  CreateColumnsFields;
  FMemData.Open;
  CopyColumnsData(Restrictions);
  FRecordSet.Close;

  FMemData.SetToBegin;
  Result := FMemData;
end;

{$IFNDEF DBX_METADATA}
procedure TOLEDBMetaData.CreateColumnsFields;
begin
  FMemData.Fields.Clear;
  AddField('TABLE_CATALOG', dtString, 128);
  AddField('TABLE_SCHEMA', dtString, 128);
  AddField('TABLE_NAME', dtString, 128);
  AddField('COLUMN_NAME', dtString, 128);
  AddField('POSITION', dtInt32);
  AddField('DATA_TYPE', dtInt32);
  AddField('DATA_LENGTH', dtInt32);
  AddField('DATA_PRECISION', dtInt32);
  AddField('DATA_SCALE', dtInt32);
  AddField('NULLABLE', dtInt32);
  AddField('DEFAULT_VALUE', dtMemo);
  FMemData.InitFields;
end;
{$ENDIF}

procedure TOLEDBMetaData.CopyColumnsData(Restrictions: _TStrings);
const
  dnCATALOG       = 1;
  dnSCHEMA        = 2;
  dnTABLE_NAME    = 3;
  dnCOLUMN_NAME   = 4;
  dnPOSITION      = 5;
  dnDATA_TYPE     = 6;
  dnLENGTH        = 7;
  dnPRECISION     = 8;
  dnSCALE         = 9;
  dnNULLABLE      = 10;
  dnDEFAULT       = 11;
var
  snCATALOG, snSCHEMA, snTABLE_NAME, snCOLUMN_NAME, snPOSITION, snDATA_TYPE,
  snLENGTH, snNUM_PRECISION, snNUM_SCALE, snDATETIME_PRECISION, snNULLABLE, snDEFAULT: integer;
  Value: variant;
  Nullable: integer;
begin
  snCATALOG            := FRecordSet.FieldByName('TABLE_CATALOG').FieldNo;
  snSCHEMA             := FRecordSet.FieldByName('TABLE_SCHEMA').FieldNo;
  snTABLE_NAME         := FRecordSet.FieldByName('TABLE_NAME').FieldNo;
  snCOLUMN_NAME        := FRecordSet.FieldByName('COLUMN_NAME').FieldNo;
  snPOSITION           := FRecordSet.FieldByName('ORDINAL_POSITION').FieldNo;
  snDATA_TYPE          := FRecordSet.FieldByName('DATA_TYPE').FieldNo;
  snLENGTH             := FRecordSet.FieldByName('CHARACTER_MAXIMUM_LENGTH').FieldNo;
  snNUM_PRECISION      := FRecordSet.FieldByName('NUMERIC_PRECISION').FieldNo;
  snNUM_SCALE          := FRecordSet.FieldByName('NUMERIC_SCALE').FieldNo;
  snDATETIME_PRECISION := FRecordSet.FieldByName('DATETIME_PRECISION').FieldNo;
  snNULLABLE           := FRecordSet.FieldByName('IS_NULLABLE').FieldNo;
  snDEFAULT            := FRecordSet.FieldByName('COLUMN_DEFAULT').FieldNo;

  FMemDataHelper.AllocBuffer;
  FRecordSetHelper.AllocBuffer;
  while FRecordSetHelper.NextRecord do begin
    FMemDataHelper.InitRecord;
    CopyRecord(
      [snCATALOG, snSCHEMA, snTABLE_NAME, snCOLUMN_NAME, snPOSITION, snDATA_TYPE, snLENGTH, snNUM_PRECISION, snNUM_SCALE, snDEFAULT],
      [dnCATALOG, dnSCHEMA, dnTABLE_NAME, dnCOLUMN_NAME, dnPOSITION, dnDATA_TYPE, dnLENGTH, dnPRECISION, dnSCALE, dnDEFAULT]);

    Value := FRecordSetHelper.FieldValues[snDATETIME_PRECISION];
    if not VarIsNull(Value) then
      FMemDataHelper.FieldValues[dnPRECISION] := Value;

    Value := FRecordSetHelper.FieldValues[snNULLABLE];
    if Boolean(Value) then
      Nullable := 1
    else
      Nullable := 0;
    FMemDataHelper.FieldValues[dnNULLABLE] := Nullable;

    FMemDataHelper.AppendRecord;
  end;
end;

function TOLEDBMetaData.GetProcedures(Restrictions: _TStrings): TData;
const
  fmtGetNonSystemProceduresSQL =
    'SELECT '#$D#$A +
    '  CAST(db_name() AS VARCHAR(128)) AS PROCEDURE_CATALOG,'#$D#$A +
    '  CAST(%0:s(uid) AS VARCHAR(128)) AS PROCEDURE_SCHEMA,'#$D#$A +
    '  CAST(name AS VARCHAR(128)) AS PROCEDURE_NAME,'#$D#$A +
    '  CAST(xtype AS VARCHAR(2)) AS PROCEDURE_TYPE,'#$D#$A +
    '  crdate AS DATE_CREATED,'#$D#$A +
    '  CAST(null AS DATETIME) AS DATE_MODIFIED,'#$D#$A +
    '  CASE '#$D#$A +
    '    WHEN xtype IN (''P'',''X'') THEN 1 '#$D#$A +
    '    ELSE 0 '#$D#$A +
    '  END AS OVERLOAD '#$D#$A +
    'FROM sysobjects o'#$D#$A +
    'WHERE %1:s (xtype in (''P'',''FN'',''TF'',''X''))'#$D#$A +
    'UNION '#$D#$A +
    'SELECT '#$D#$A +
    '  CAST(db_name() AS VARCHAR(128)) AS PROCEDURE_CATALOG,'#$D#$A +
    '  CAST(%0:s(uid) AS VARCHAR(128)) AS PROCEDURE_SCHEMA,'#$D#$A +
    '  CAST(name AS VARCHAR(128)) AS PROCEDURE_NAME,'#$D#$A +
    '  CAST(xtype AS VARCHAR(2)) AS PROCEDURE_TYPE,'#$D#$A +
    '  crdate AS DATE_CREATED,'#$D#$A +
    '  CAST(null AS DATETIME) AS DATE_MODIFIED,'#$D#$A +
    '  procedure_number AS OVERLOAD '#$D#$A +
    'FROM sysobjects o JOIN sys.numbered_procedures p on (p.object_id = o.id)'#$D#$A +
    'WHERE %1:s (xtype in (''P'',''X''))';

  fmtGetSystemProceduresSQL =
    'SELECT '#$D#$A +
    '  CAST(db_name() AS VARCHAR(128)) AS PROCEDURE_CATALOG,'#$D#$A +
    '  CAST(%0:s(schema_id) AS VARCHAR(128)) AS PROCEDURE_SCHEMA,'#$D#$A +
    '  CAST(name AS VARCHAR(128)) AS PROCEDURE_NAME,'#$D#$A +
    '  CAST(type AS VARCHAR(2)) AS PROCEDURE_TYPE,'#$D#$A +
    '  create_date AS DATE_CREATED,'#$D#$A +
    '  modify_date AS DATE_MODIFIED,'#$D#$A +
    '  CASE '#$D#$A +
    '    WHEN type IN (''P'',''X'') THEN 1 '#$D#$A +
    '    ELSE 0 '#$D#$A +
    '  END AS OVERLOAD '#$D#$A +
    'FROM sys.all_objects o'#$D#$A +
    'WHERE %1:s (type in (''P'',''FN'',''TF'',''X'',''IF''))'#$D#$A +
    'UNION '#$D#$A +
    'SELECT '#$D#$A +
    '  CAST(db_name() AS VARCHAR(128)) AS PROCEDURE_CATALOG,'#$D#$A +
    '  CAST(%0:s(schema_id) AS VARCHAR(128)) AS PROCEDURE_SCHEMA,'#$D#$A +
    '  CAST(name AS VARCHAR(128)) AS PROCEDURE_NAME,'#$D#$A +
    '  CAST(type AS VARCHAR(2)) AS PROCEDURE_TYPE,'#$D#$A +
    '  create_date AS DATE_CREATED,'#$D#$A +
    '  modify_date AS DATE_MODIFIED,'#$D#$A +
    '  procedure_number AS OVERLOAD '#$D#$A +
    'FROM sys.all_objects o JOIN sys.numbered_procedures p on (p.object_id = o.object_id)'#$D#$A +
    'WHERE %1:s (type in (''P'',''X''))';

  fmtOrderBySQL = ' ORDER BY 2, 3';

var
  Catalog, ProcSchema, ProcName, Scope, WhereClause, SQL: _string;
begin
  if FRecordSet.FCommand.FConnection.FProvider = prCompact then begin
    CreateProceduresFields;
    FMemData.Open;
    Result := FMemData;
    Exit;
  end;

  Catalog := Trim(Restrictions.Values['PROCEDURE_CATALOG']);
  ProcSchema := Trim(Restrictions.Values['PROCEDURE_SCHEMA']);
  ProcName := Trim(Restrictions.Values['PROCEDURE_NAME']);
  Scope := AnsiUpperCase(Trim(Restrictions.Values['SCOPE']));

  WhereClause := '';
  if Scope = 'LOCAL' then
    AddWhere(WhereClause, GetSchemaNameFunc + '(uid)', ProcSchema)
  else
    AddWhere(WhereClause, GetSchemaNameFunc + '(schema_id)', ProcSchema);
  AddWhere(WhereClause, 'name', ProcName);
  if WhereClause <> '' then
    WhereClause := WhereClause + ' AND';
  SQL := _Format(fmtGetNonSystemProceduresSQL, [GetSchemaNameFunc, WhereClause]);

  if Scope = 'LOCAL' then
    SQL := _Format(fmtGetNonSystemProceduresSQL, [GetSchemaNameFunc, WhereClause])
  else
    SQL := _Format(fmtGetSystemProceduresSQL, [GetSchemaNameFunc, WhereClause]);

  FRecordSet.SetSQL(SQL + fmtOrderBySQL);
  FRecordSet.Open;
  Result := FRecordSet;
end;

{$IFNDEF DBX_METADATA}
procedure TOLEDBMetaData.CreateProceduresFields;
begin
  FMemData.Fields.Clear;
  AddField('PROCEDURE_CATALOG', dtString, 128);
  AddField('PROCEDURE_SCHEMA', dtString, 128);
  AddField('PROCEDURE_NAME', dtString, 128);
  AddField('PROCEDURE_TYPE', dtString, 0);
  AddField('DATE_CREATED', dtDateTime);
  AddField('DATE_MODIFIED', dtDateTime);
  AddField('OVERLOAD', dtInt32);
  FMemData.InitFields;
end;
{$ENDIF}

function TOLEDBMetaData.GetProcedureParameters(Restrictions: _TStrings): TData;
var
  Catalog, ProcSchema, ProcName, ParamName: _string;
  rgRestrictions: TRestrictions;
  Rowset: IRowset;
begin
  if FRecordSet.FCommand.FConnection.FProvider = prCompact then begin
    CreateProcedureParametersFields;
    FMemData.Open;
    Result := FMemData;
    Exit;
  end;

  Catalog := Trim(Restrictions.Values['PROCEDURE_CATALOG']);
  ProcSchema := Trim(Restrictions.Values['PROCEDURE_SCHEMA']);
  ProcName := Trim(Restrictions.Values['PROCEDURE_NAME']);
  ParamName := Trim(Restrictions.Values['PARAMETER_NAME']);

  if FRecordSet.FCommand.FConnection.IsSQLAzureEdition then begin
    if ProcName = '' then
      raise Exception.CreateFmt(SRestrictionMustBeSet, ['PROCEDURE_NAME']);

    if ProcSchema = '' then
      ProcSchema := 'NULL'
    else
      ProcSchema := 'N''' + ProcSchema + '''';
    if ParamName = '' then
      ParamName := 'NULL'
    else
      ParamName := 'N''' + ParamName + '''';

    FRecordSet.SetSQL(_Format('exec sys.sp_procedure_params_managed N''%s'', NULL, %s, %s', [ProcName, ProcSchema, ParamName]));
    FRecordSet.Open;
  end
  else begin
    SetLength(rgRestrictions, 4);
    rgRestrictions[0] := Catalog;
    rgRestrictions[1] := ProcSchema;
    rgRestrictions[2] := ProcName;
    rgRestrictions[3] := ParamName;

    Rowset := FRecordSet.FCommand.FConnection.GetSchemaRowset(DBSCHEMA_PROCEDURE_PARAMETERS, rgRestrictions);
    FRecordSet.SetIRowset(Rowset, False);
    FRecordSet.Open;
  end;

  CreateProcedureParametersFields;
  FMemData.Open;
  CopyProcedureParametersData(Restrictions);
  FRecordSet.Close;

  FMemData.SetToBegin;
  Result := FMemData;
end;

{$IFNDEF DBX_METADATA}
procedure TOLEDBMetaData.CreateProcedureParametersFields;
begin
  FMemData.Fields.Clear;
  AddField('PROCEDURE_CATALOG', dtString, 128);
  AddField('PROCEDURE_SCHEMA', dtString, 128);
  AddField('PROCEDURE_NAME', dtString, 128);
  AddField('PARAMETER_NAME', dtString, 128);
  AddField('POSITION', dtInt32);
  AddField('DIRECTION', dtString, 10);
  AddField('DATA_TYPE', dtInt32);
  AddField('DATA_LENGTH', dtInt32);
  AddField('DATA_PRECISION', dtInt32);
  AddField('DATA_SCALE', dtInt32);
  FMemData.InitFields;
end;
{$ENDIF}

procedure TOLEDBMetaData.CopyProcedureParametersData(Restrictions: _TStrings);
const
  dnCATALOG    = 1;
  dnSCHEMA     = 2;
  dnPROC_NAME  = 3;
  dnPARAM_NAME = 4;
  dnPOSITION   = 5;
  dnDIRECTION  = 6;
  dnDATA_TYPE  = 7;
  dnLENGTH     = 8;
  dnPRECISION  = 9;
  dnSCALE      = 10;
var
  snCATALOG, snSCHEMA, snPROC_NAME, snPARAM_NAME, snPOSITION, snPARAM_TYPE,
  snDATA_TYPE, snLENGTH, snNUM_PRECISION, snNUM_SCALE: integer;
  ParamType: integer;
  Direction: _string;
begin
  snCATALOG       := FRecordSet.FieldByName('PROCEDURE_CATALOG').FieldNo;
  snSCHEMA        := FRecordSet.FieldByName('PROCEDURE_SCHEMA').FieldNo;
  snPROC_NAME     := FRecordSet.FieldByName('PROCEDURE_NAME').FieldNo;
  snPARAM_NAME    := FRecordSet.FieldByName('PARAMETER_NAME').FieldNo;
  snPOSITION      := FRecordSet.FieldByName('ORDINAL_POSITION').FieldNo;
  snPARAM_TYPE    := FRecordSet.FieldByName('PARAMETER_TYPE').FieldNo;
  snDATA_TYPE     := FRecordSet.FieldByName('DATA_TYPE').FieldNo;
  snLENGTH        := FRecordSet.FieldByName('CHARACTER_MAXIMUM_LENGTH').FieldNo;
  snNUM_PRECISION := FRecordSet.FieldByName('NUMERIC_PRECISION').FieldNo;
  snNUM_SCALE     := FRecordSet.FieldByName('NUMERIC_SCALE').FieldNo;

  FMemDataHelper.AllocBuffer;
  FRecordSetHelper.AllocBuffer;
  while FRecordSetHelper.NextRecord do begin
    FMemDataHelper.InitRecord;
    CopyRecord(
      [snCATALOG, snSCHEMA, snPROC_NAME, snPARAM_NAME, snPOSITION, snDATA_TYPE, snLENGTH, snNUM_PRECISION, snNUM_SCALE],
      [dnCATALOG, dnSCHEMA, dnPROC_NAME, dnPARAM_NAME, dnPOSITION, dnDATA_TYPE, dnLENGTH, dnPRECISION, dnSCALE]);

    ParamType := FRecordSetHelper.FieldValues[snPARAM_TYPE];
    case ParamType of
      DBPARAMTYPE_INPUT:
        Direction := 'IN';
      DBPARAMTYPE_INPUTOUTPUT:
        Direction := 'IN/OUT';
      DBPARAMTYPE_OUTPUT, DBPARAMTYPE_RETURNVALUE:
        Direction := 'OUT';
    end;
    FMemDataHelper.FieldValues[dnDIRECTION] := Direction;

    FMemDataHelper.AppendRecord;
  end;
end;

function TOLEDBMetaData.GetIndexes(Restrictions: _TStrings): TData;
const
  fmtGetIndexesSQL =
    'SELECT '#$D#$A +
    '  %0:s AS TABLE_CATALOG,'#$D#$A +
    '  CAST(%3:s(o.uid) AS VARCHAR(128)) AS TABLE_SCHEMA,'#$D#$A +
    '  CAST(o.name AS VARCHAR(128)) AS TABLE_NAME,'#$D#$A +
    '  %0:s AS INDEX_CATALOG,'#$D#$A +
    '  CAST(%3:s(o.uid) AS VARCHAR(128)) AS INDEX_SCHEMA,'#$D#$A +
    '  CAST(x.name AS VARCHAR(128)) AS INDEX_NAME,'#$D#$A +
    '  (CASE WHEN x.status & 0x2 <> 0 THEN 1 ELSE 0 END) AS [UNIQUE] '#$D#$A +
    'FROM %1:s..sysobjects o, %1:s..sysindexes x '#$D#$A +
    'WHERE '#$D#$A +
    '  o.id = x.id '#$D#$A +
    '  AND o.xtype <> ''S'' '#$D#$A +
    '  AND LEFT(x.name, 8/*_WA_Sys_*/) <> ''_WA_Sys_'''#$D#$A +
    '  %2:s '#$D#$A +
    'ORDER BY o.uid, o.name, x.name';

  fmtGetIndexesSQLAzure =
    'SELECT '#$D#$A +
    '  CAST(db_name() AS VARCHAR(128)) AS TABLE_CATALOG,'#$D#$A +
    '  CAST(schema_name(o.uid) AS VARCHAR(128)) AS TABLE_SCHEMA,'#$D#$A +
    '  CAST(o.name AS VARCHAR(128)) AS TABLE_NAME,'#$D#$A +
    '  CAST(db_name() AS VARCHAR(128)) AS INDEX_CATALOG,'#$D#$A +
    '  CAST(schema_name(o.uid) AS VARCHAR(128)) AS INDEX_SCHEMA,'#$D#$A +
    '  CAST(x.name AS VARCHAR(128)) AS INDEX_NAME,'#$D#$A +
    '  CASE x.is_unique '#$D#$A +
    '    WHEN 0 THEN 0 ELSE 1 '#$D#$A +
    '  END AS [UNIQUE] '#$D#$A +
    'FROM sysobjects o, sys.indexes x '#$D#$A +
    'WHERE '#$D#$A +
    '  o.id = x.object_id '#$D#$A +
    '  AND o.xtype <> ''S'' '#$D#$A +
    '  AND LEFT(x.name, 8/*_WA_Sys_*/) <> ''_WA_Sys_'''#$D#$A +
    '  %s '#$D#$A +
    'ORDER BY o.uid, o.name, x.name';

  fmtCompactGetIndexesSQL =
    'SELECT TABLE_CATALOG, TABLE_SCHEMA, TABLE_NAME, '#$D#$A +
    '  INDEX_CATALOG, INDEX_SCHEMA, INDEX_NAME, '#$D#$A +
    '  CASE [UNIQUE] WHEN 0 THEN 0 ELSE 1 END AS [UNIQUE] '#$D#$A + // UNIQUE is boolean
    'FROM INFORMATION_SCHEMA.INDEXES '#$D#$A +
    '  %s '#$D#$A +
    'GROUP BY TABLE_CATALOG, TABLE_SCHEMA, TABLE_NAME, '#$D#$A +
    '  INDEX_CATALOG, INDEX_SCHEMA, INDEX_NAME, [UNIQUE] '#$D#$A +
    'ORDER BY TABLE_NAME, INDEX_NAME';
    
var
  Catalog, TableSchema, TableName, IndexName,
  WhereClause, CatalogVal, CatalogPrefix: _string;
begin
  Catalog := Trim(Restrictions.Values['TABLE_CATALOG']);
  TableSchema := Trim(Restrictions.Values['TABLE_SCHEMA']);
  TableName := Trim(Restrictions.Values['TABLE_NAME']);
  IndexName := Trim(Restrictions.Values['INDEX_NAME']);

  if FRecordSet.FCommand.FConnection.IsSQLAzureEdition then begin
    WhereClause := '';
    AddWhere(WhereClause, 'schema_name(o.uid)', TableSchema);
    AddWhere(WhereClause, 'o.name', TableName);
    AddWhere(WhereClause, 'x.name', IndexName);
    if WhereClause <> '' then
      WhereClause := ' AND ' + WhereClause;

    FRecordSet.SetSQL(_Format(fmtGetIndexesSQLAzure, [WhereClause]));
  end
  else
  if FRecordSet.FCommand.FConnection.FProvider <> prCompact then begin
    if Catalog <> '' then begin
      CatalogPrefix := OLEDBSQLInfo.NormalizeName(Catalog);

      CatalogVal := OLEDBSQLInfo.UnQuote(CatalogPrefix); // remove [] ; add ' '
      CatalogVal := AnsiQuotedStr(CatalogVal, '''');
    end
    else begin
      CatalogPrefix := '';
      CatalogVal := 'CAST(db_name() AS VARCHAR(128))';
    end;

    WhereClause := '';
    AddWhere(WhereClause, GetSchemaNameFunc + '(o.uid)', TableSchema);
    AddWhere(WhereClause, 'o.name', TableName);
    AddWhere(WhereClause, 'x.name', IndexName);
    if WhereClause <> '' then
      WhereClause := ' AND ' + WhereClause;

    FRecordSet.SetSQL(_Format(fmtGetIndexesSQL, [CatalogVal, CatalogPrefix, WhereClause, GetSchemaNameFunc]));
  end
  else begin
    WhereClause := '';
    AddWhere(WhereClause, 'TABLE_NAME', TableName);
    AddWhere(WhereClause, 'INDEX_NAME', IndexName);
    if WhereClause <> '' then
      WhereClause := 'WHERE ' + WhereClause;

    FRecordSet.SetSQL(_Format(fmtCompactGetIndexesSQL, [WhereClause]));
  end;
  FRecordSet.Open;
  Result := FRecordSet;
end;

function TOLEDBMetaData.GetIndexColumns(Restrictions: _TStrings): TData;
const
  fmtGetIndexColumnsSQLAzure =
    'SELECT '#$D#$A +
    '  CAST(db_name() AS VARCHAR(128)) AS TABLE_CATALOG,'#$D#$A +
    '  CAST(schema_name(o.uid) AS VARCHAR(128)) AS TABLE_SCHEMA,'#$D#$A +
    '  CAST(o.name AS VARCHAR(128)) AS TABLE_NAME,'#$D#$A +
    '  CAST(db_name() AS VARCHAR(128)) AS INDEX_CATALOG,'#$D#$A +
    '  CAST(schema_name(o.uid) AS VARCHAR(128)) AS INDEX_SCHEMA,'#$D#$A +
    '  CAST(x.name AS VARCHAR(128)) AS INDEX_NAME,'#$D#$A +
    '  CAST(c.name AS VARCHAR(128)) AS COLUMN_NAME,'#$D#$A +
    '  xc.key_ordinal AS COLUMN_POSITION,'#$D#$A +
    '  (CASE WHEN xc.is_descending_key <> 0 THEN ''DESC'' ELSE ''ASC'' END) AS SORT_ORDER '#$D#$A +
    'FROM sysobjects o, sys.indexes x, syscolumns c, sys.index_columns xc '#$D#$A +
    'WHERE '#$D#$A +
    '  o.id = x.object_id '#$D#$A +
    '  AND o.id = c.id '#$D#$A +
    '  AND o.id = xc.object_id '#$D#$A +
    '  AND x.index_id = xc.index_id '#$D#$A +
    '  AND c.colid = xc.column_id '#$D#$A +
    '  AND o.xtype <> ''S'' '#$D#$A +
    '  AND LEFT(x.name, 8/*_WA_Sys_*/) <> ''_WA_Sys_'''#$D#$A +
    '  %s '#$D#$A +
    'ORDER BY o.uid, o.name, x.name, xc.key_ordinal';

var
  Catalog, TableSchema, TableName, IndexName: _string;
  rgRestrictions: TRestrictions;
  Rowset: IRowset;
  WhereClause: _string;
begin
  Catalog := Trim(Restrictions.Values['TABLE_CATALOG']);
  TableSchema := Trim(Restrictions.Values['TABLE_SCHEMA']);
  TableName := Trim(Restrictions.Values['TABLE_NAME']);
  IndexName := Trim(Restrictions.Values['INDEX_NAME']);

  if FRecordSet.FCommand.FConnection.IsSQLAzureEdition then begin
    WhereClause := '';
    AddWhere(WhereClause, 'schema_name(o.uid)', TableSchema);
    AddWhere(WhereClause, 'o.name', TableName);
    AddWhere(WhereClause, 'x.name', IndexName);
    if WhereClause <> '' then
      WhereClause := ' AND ' + WhereClause;

    FRecordSet.SetSQL(_Format(fmtGetIndexColumnsSQLAzure, [WhereClause]));
    FRecordSet.Open;
    Result := FRecordSet;
    Exit;
  end;

  SetLength(rgRestrictions, 5);
  if FRecordSet.FCommand.FConnection.FProvider <> prCompact then begin
    rgRestrictions[0] := Catalog;
    rgRestrictions[1] := TableSchema;
  end;
  rgRestrictions[2] := IndexName;
  rgRestrictions[4] := TableName;

  Rowset := FRecordSet.FCommand.FConnection.GetSchemaRowset(DBSCHEMA_INDEXES, rgRestrictions);
  FRecordSet.SetIRowset(Rowset, False);
  FRecordSet.Open;

  CreateIndexColumnsFields;
  FMemData.Open;
  CopyIndexColumnsData(Restrictions);
  FRecordSet.Close;

  FMemData.SetToBegin;
  Result := FMemData;
end;

{$IFNDEF DBX_METADATA}
procedure TOLEDBMetaData.CreateIndexColumnsFields;
begin
  FMemData.Fields.Clear;
  AddField('TABLE_CATALOG', dtString, 128);
  AddField('TABLE_SCHEMA', dtString, 128);
  AddField('TABLE_NAME', dtString, 128);
  AddField('INDEX_CATALOG', dtString, 128);
  AddField('INDEX_SCHEMA', dtString, 128);
  AddField('INDEX_NAME', dtString, 128);
  AddField('COLUMN_NAME', dtString, 128);
  AddField('COLUMN_POSITION', dtInt32);
  AddField('SORT_ORDER', dtString, 4);
  FMemData.InitFields;
end;
{$ENDIF}

procedure TOLEDBMetaData.CopyIndexColumnsData(Restrictions: _TStrings);
const
  snTABLE_CATALOG   = 1;
  snTABLE_SCHEMA    = 2;
  snTABLE_NAME      = 3;
  snINDEX_CATALOG   = 4;
  snINDEX_SCHEMA    = 5;
  snINDEX_NAME      = 6;

  dnTABLE_CATALOG   = 1;
  dnTABLE_SCHEMA    = 2;
  dnTABLE_NAME      = 3;
  dnINDEX_CATALOG   = 4;
  dnINDEX_SCHEMA    = 5;
  dnINDEX_NAME      = 6;
  dnCOLUMN_NAME     = 7;
  dnPOSITION        = 8;
  dnSORT_ORDER      = 9;
var
  SortOrder: _string;
  IntValue: integer;
  snCOLUMN_NAME, snPOSITION, snCOLLATION: integer;
begin
  snCOLUMN_NAME := FRecordSet.FieldByName('COLUMN_NAME').FieldNo;
  snPOSITION    := FRecordSet.FieldByName('ORDINAL_POSITION').FieldNo;
  snCOLLATION   := FRecordSet.FieldByName('COLLATION').FieldNo;

  FMemDataHelper.AllocBuffer;
  FRecordSetHelper.AllocBuffer;
  while FRecordSetHelper.NextRecord do begin
    FMemDataHelper.InitRecord;
    CopyRecord(
      [snTABLE_CATALOG, snTABLE_SCHEMA, snTABLE_NAME, snINDEX_CATALOG, snINDEX_SCHEMA, snINDEX_NAME, snCOLUMN_NAME, snPOSITION],
      [dnTABLE_CATALOG, dnTABLE_SCHEMA, dnTABLE_NAME, dnINDEX_CATALOG, dnINDEX_SCHEMA, dnINDEX_NAME, dnCOLUMN_NAME, dnPOSITION]);

    IntValue := FRecordSetHelper.FieldValues[snCOLLATION];
    if IntValue = DB_COLLATION_DESC then
      SortOrder := 'DESC'
    else
      SortOrder := 'ASC';
    FMemDataHelper.FieldValues[dnSORT_ORDER] := SortOrder;

    FMemDataHelper.AppendRecord;
  end;
end;

function TOLEDBMetaData.GetConstraints(Restrictions: _TStrings): TData;
const
  fmtGetConstraintsSQLAzure =
    'SELECT '#$D#$A +
    '  CAST(db_name() AS VARCHAR(128)) AS TABLE_CATALOG,'#$D#$A +
    '  CAST(schema_name(t_obj.schema_id) AS VARCHAR(128)) AS TABLE_SCHEMA,'#$D#$A +
    '  CAST(t_obj.name AS VARCHAR(128)) AS TABLE_NAME,'#$D#$A +
    '  CAST(o.name AS VARCHAR(128)) AS CONSTRAINT_NAME,'#$D#$A +
    '  CASE o.type '#$D#$A +
    '    WHEN ''PK'' THEN ''PRIMARY KEY'' '#$D#$A +
    '    WHEN ''UQ'' THEN ''UNIQUE'' '#$D#$A +
    '    WHEN ''F'' THEN ''FOREIGN KEY'' '#$D#$A +
    '    WHEN ''C'' THEN ''CHECK'' '#$D#$A +
    '    ELSE null '#$D#$A +
    '  END AS CONSTRAINT_TYPE,'#$D#$A +
    '  CONVERT(varchar(1), null) AS INDEX_CATALOG,'#$D#$A +
    '  CONVERT(varchar(1), null) AS INDEX_SCHEMA,'#$D#$A +
    '  CONVERT(varchar(1), null) AS INDEX_NAME '#$D#$A +
    'FROM '#$D#$A +
    '  sys.all_objects o, sys.all_objects t_obj '#$D#$A +
    'WHERE '#$D#$A +
    '  o.type in (''PK'', ''UQ'', ''F'', ''C'') '#$D#$A +
    '  AND t_obj.type in (''U'', ''V'', ''S'') '#$D#$A +
    '  AND o.parent_object_id = t_obj.object_id '#$D#$A +
    '  %s'#$D#$A +
    'ORDER BY t_obj.name, o.name';

var
  Catalog, TableSchema, TableName, ConstraintName, Types: _string;
  TypesList: _TStringList;
  i: integer;
  rgRestrictions: TRestrictions;
  Rowset: IRowset;
  WhereClause, CType: _string;
begin
  Catalog := Trim(Restrictions.Values['TABLE_CATALOG']);
  TableSchema := Trim(Restrictions.Values['TABLE_SCHEMA']);
  TableName := Trim(Restrictions.Values['TABLE_NAME']);
  ConstraintName := Trim(Restrictions.Values['CONSTRAINT_NAME']);
  if FRecordSet.FCommand.FConnection.FProvider <> prCompact then
    Types := Trim(Restrictions.Values['CONSTRAINT_TYPE'])
  else
    Types := '';

  if FRecordSet.FCommand.FConnection.IsSQLAzureEdition then begin
    WhereClause := '';
    if Types <> '' then begin
      TypesList := _TStringList.Create;
      try
        ParseTypes(Types, TypesList);
        for i := 0 to TypesList.Count - 1 do begin
          if TypesList[i] = 'PRIMARY KEY' then
            CType := '''PK'''
          else
          if TypesList[i] = 'UNIQUE' then
            CType := '''UQ'''
          else
          if TypesList[i] = 'FOREIGN KEY' then
            CType := '''F'''
          else
          if TypesList[i] = 'CHECK' then
            CType := '''C'''
          else
            CType := '';
          if (WhereClause <> '') and (CType <> '') then
            WhereClause := WhereClause + ',';
          WhereClause := WhereClause + CType;
        end;
      finally
        TypesList.Free;
      end;
      if WhereClause <> '' then
        WhereClause := 'o.type in (' + WhereClause + ')';
    end;

    AddWhere(WhereClause, 'schema_name(t_obj.schema_id)', TableSchema);
    AddWhere(WhereClause, 't_obj.name', TableName);
    AddWhere(WhereClause, 'o.name', ConstraintName);
    if WhereClause <> '' then
      WhereClause := ' AND ' + WhereClause;

    FRecordSet.SetSQL(_Format(fmtGetConstraintsSQLAzure, [WhereClause]));
    FRecordSet.Open;
    Result := FRecordSet;
    Exit;
  end;

  SetLength(rgRestrictions, 7);
  if FRecordSet.FCommand.FConnection.FProvider <> prCompact then begin
    rgRestrictions[3] := Catalog;
    rgRestrictions[4] := TableSchema;
  end;
  rgRestrictions[2] := ConstraintName;
  rgRestrictions[5] := TableName;

  TypesList := _TStringList.Create;
  try
    ParseTypes(Types, TypesList);
    CreateConstraintsFields;
    FMemData.Open;
    for i := 0 to TypesList.Count - 1 do begin
      if FRecordSet.FCommand.FConnection.FProvider <> prCompact then
        rgRestrictions[6] := TypesList[i];
      Rowset := FRecordSet.FCommand.FConnection.GetSchemaRowset(DBSCHEMA_TABLE_CONSTRAINTS, rgRestrictions);
      FRecordSet.SetIRowset(Rowset, False);
      FRecordSet.Open;
      CopyConstraintsData(Restrictions);
      FRecordSet.Close;
    end;
  finally
    TypesList.Free;
  end;

  FMemData.SetToBegin;
  Result := FMemData;
end;

{$IFNDEF DBX_METADATA}
procedure TOLEDBMetaData.CreateConstraintsFields;
begin
  FMemData.Fields.Clear;
  AddField('TABLE_CATALOG', dtString, 128);
  AddField('TABLE_SCHEMA', dtString, 128);
  AddField('TABLE_NAME', dtString, 128);
  AddField('CONSTRAINT_NAME', dtString, 128);
  AddField('CONSTRAINT_TYPE', dtString, 11);
  AddField('INDEX_CATALOG', dtString, 0);
  AddField('INDEX_SCHEMA', dtString, 0);
  AddField('INDEX_NAME', dtString, 0);
  FMemData.InitFields;
end;
{$ENDIF}

procedure TOLEDBMetaData.CopyConstraintsData(Restrictions: _TStrings);
const
  dnCATALOG         = 1;
  dnSCHEMA          = 2;
  dnTABLE_NAME      = 3;
  dnCONSTRAINT_NAME = 4;
  dnCONSTRAINT_TYPE = 5;
var
  snCATALOG, snSCHEMA, snTABLE_NAME, snCONSTRAINT_NAME, snCONSTRAINT_TYPE: integer;
begin
  snCATALOG         := FRecordSet.FieldByName('TABLE_CATALOG').FieldNo;
  snSCHEMA          := FRecordSet.FieldByName('TABLE_SCHEMA').FieldNo;
  snTABLE_NAME      := FRecordSet.FieldByName('TABLE_NAME').FieldNo;
  snCONSTRAINT_NAME := FRecordSet.FieldByName('CONSTRAINT_NAME').FieldNo;
  snCONSTRAINT_TYPE := FRecordSet.FieldByName('CONSTRAINT_TYPE').FieldNo;

  FMemDataHelper.AllocBuffer;
  FRecordSetHelper.AllocBuffer;
  while FRecordSetHelper.NextRecord do begin
    FMemDataHelper.InitRecord;
    CopyRecord(
      [snCATALOG, snSCHEMA, snTABLE_NAME, snCONSTRAINT_NAME, snCONSTRAINT_TYPE],
      [dnCATALOG, dnSCHEMA, dnTABLE_NAME, dnCONSTRAINT_NAME, dnCONSTRAINT_TYPE]);
    FMemDataHelper.AppendRecord;
  end;
end;

function TOLEDBMetaData.GetDatabases(Restrictions: _TStrings): TData;
var
  Catalog: _string;
  rgRestrictions: TRestrictions;
  Rowset: IRowset;
begin
  if FRecordSet.FCommand.FConnection.FProvider = prCompact then begin
    CreateDatabasesFields;
    FMemData.Open;
    Result := FMemData;
    Exit;
  end;

  Catalog := Trim(Restrictions.Values['CATALOG_NAME']);
  SetLength(rgRestrictions, 1);
  rgRestrictions[0] := Catalog;

  Rowset := FRecordSet.FCommand.FConnection.GetSchemaRowset(DBSCHEMA_CATALOGS, rgRestrictions);
  FRecordSet.SetIRowset(Rowset, False);
  FRecordSet.Open;

  CreateDatabasesFields;
  FMemData.Open;
  CopyDatabasesData(Restrictions);
  FRecordSet.Close;

  FMemData.SetToBegin;
  Result := FMemData;
end;

procedure TOLEDBMetaData.CreateDatabasesFields;
begin
  FMemData.Fields.Clear;
  AddField('DATABASE_NAME', dtString, 128);
  FMemData.InitFields;
end;

procedure TOLEDBMetaData.CopyDatabasesData(Restrictions: _TStrings);
const
  snCATALOG_NAME = 1;
  dnCATALOG_NAME = 1;
begin
  FMemDataHelper.AllocBuffer;
  FRecordSetHelper.AllocBuffer;
  while FRecordSetHelper.NextRecord do begin
    FMemDataHelper.InitRecord;
    CopyRecord([snCATALOG_NAME], [dnCATALOG_NAME]);
    FMemDataHelper.AppendRecord;
  end;
end;

{$ENDIF}

{ TDBIDAccessor }

constructor TDBIDAccessor.Create(APDBID: PDBID);
begin
  inherited Create;

  FPDBID := APDBID;
end;

procedure TDBIDAccessor.SetPDBID(Value: PDBID);
begin
  FPDBID := Value;
end;

function TDBIDAccessor.GeteKind: DBKIND;
begin
  Result := Marshal.ReadInt32(FPDBID, OffsetOf_DBID_eKind);
end;

procedure TDBIDAccessor.SeteKind(Value: DBKIND);
begin
  Marshal.WriteInt32(FPDBID, OffsetOf_DBID_eKind, Value);
end;

function TDBIDAccessor.GetpwszName: IntPtr;
begin
  Result := Marshal.ReadIntPtr(FPDBID, OffsetOf_DBID_uName);
end;

procedure TDBIDAccessor.SetpwszName(Value: IntPtr);
begin
  Marshal.WriteIntPtr(FPDBID, OffsetOf_DBID_uName, Value);
end;

{ TOLEDBLoader }

constructor TOLEDBLoader.Create;
begin
  inherited;

  FSkipReadOnlyFieldDescs := False;
end;

destructor TOLEDBLoader.Destroy;
begin
  inherited;
end;

function TOLEDBLoader.SetProp(Prop: integer; const Value: variant): boolean;
begin
  Result := True;
  case Prop of
    prKeepIdentity:
      FKeepIdentity := Value;
    prKeepNulls:
      FKeepNulls := Value;
    prRowsPerBatch:
      FRowsPerBatch := Value;
    prKilobytesPerBatch:
      FKilobytesPerBatch := Value;
    prLockTable:
      FLockTable := Value;
    prCheckConstraints:
      FCheckConstraints := Value;
  else
    Assert(False, IntToStr(Prop));
    Result := False;
  end;
end;

function TOLEDBLoader.GetProp(Prop: integer; var Value: variant): boolean;
begin
  Result := True;
  case Prop of
    prKeepIdentity:
      Value := FKeepIdentity;
    prKeepNulls:
      Value := FKeepNulls;
    prRowsPerBatch:
      Value := FRowsPerBatch;
    prKilobytesPerBatch:
      Value := FKilobytesPerBatch;
    prLockTable:
      Value := FLockTable;
    prCheckConstraints:
      Value := FCheckConstraints;
  else
    Assert(False, IntToStr(Prop));
    Result := False;
  end;
end;

class function TOLEDBLoader.GetColumnClass: TCRLoaderColumnClass;
begin
  Result := TOLEDBLoaderColumn;
end;

procedure TOLEDBLoader.Prepare;
const
  MaxPropCount = 3;
var
  v: variant;
  TableID: PDBID;
  TableIDAccessor: TDBIDAccessor;
  BindMemorySize: NativeUInt;
{$IFDEF CLR}
  rgBindingsGC: GCHandle;
  ParamGCHandle: TIntPtrDynArray;
{$ENDIF}
  rgBindings: PDBBinding;
  rgStatus: PDBBINDSTATUSArray;
  i: integer;
  OLEDBPropertiesSet: TOLEDBPropertiesSet;
  OptionsStr: _string;
  IUnk: IUnknown;
  ParamDesc: TOLEDBParamDesc;

  procedure AddOption(var OptionStr: _string; const Value: _string);
  begin
    if OptionStr <> '' then
      OptionStr := OptionStr + ', ';
    OptionStr := OptionStr + Value;
  end;

begin
  inherited;

  FConnection.GetProp(prProvider, v);
  if TOLEDBProvider(v) = prCompact then
    raise Exception.Create(SLoaderNotSupport);

  FConnection.Check(FConnection.GetIDBCreateSession.CreateSession(nil, IID_IOpenRowset, IUnk), nil);
  FIOpenRowset := IOpenRowset(IUnk);

  TableID := Marshal.AllocHGlobal(SizeOf(TDBID));
  TableIDAccessor := TDBIDAccessor.Create(TableID);
  OLEDBPropertiesSet := TOLEDBPropertiesSet.Create(FConnection, DBPROPSET_SQLSERVERROWSET);
  try
    TableIDAccessor.eKind := DBKIND_NAME;
    TableIDAccessor.pwszName := Marshal.AllocHGlobal(Length(TableName) * SizeOf(WideChar) + SizeOf(WideChar));
    CopyBuffer(Marshal.StringToHGlobalUni(WideString(Encoding.Default.GetString(Encoding.Default.GetBytes(TableName), 0, Length(TableName)))),
      TableIDAccessor.pwszName, Length(TableName) * SizeOf(WideChar) + SizeOf(WideChar));

    OLEDBPropertiesSet.AddPropBool(SSPROP_FASTLOADKEEPIDENTITY, FKeepIdentity);
    OLEDBPropertiesSet.AddPropBool(SSPROP_FASTLOADKEEPNULLS, FKeepNulls);
    OptionsStr := '';
    if FRowsPerBatch > 0 then
      AddOption(OptionsStr, 'ROWS_PER_BATCH = ' + IntToStr(FRowsPerBatch));
    if FKilobytesPerBatch > 0 then
      AddOption(OptionsStr, 'KILOBYTES_PER_BATCH = ' + IntToStr(FKilobytesPerBatch));
    if FLockTable then
      AddOption(OptionsStr, 'TABLOCK');
    if FCheckConstraints then
      AddOption(OptionsStr, 'CHECK_CONSTRAINTS');
    if OptionsStr <> '' then
      OLEDBPropertiesSet.AddPropStr(SSPROP_FASTLOADOPTIONS, OptionsStr);

    FConnection.Check(FIOpenRowset.OpenRowset(nil, TableID, nil, IID_IRowsetFastLoad, 1,
      PDBPropIDSetArray(OLEDBPropertiesSet.InitPropSet), IUnk), nil);
    FIRowsetFastLoad := IRowsetFastLoad(IUnk);
  finally
    OLEDBPropertiesSet.Free;
    Marshal.FreeHGlobal(TableIDAccessor.pwszName);
    Marshal.FreeHGlobal(TableID);
    TableIDAccessor.Free;
  end;
  
  rgStatus := Marshal.AllocHGlobal(Columns.Count * SizeOf(DBBINDSTATUS));
{$IFNDEF CLR}
  IntPtr(FParamsAccessorData.Accessor) := nil;
{$ENDIF}
  try
    try
      FParamsAccessorData.ExecuteParams.HACCESSOR := 0;
      FParamsAccessorData.ExecuteParams.pData := nil;
      FParamsAccessorData.ExecuteParams.cParamSets := 1;

      SetLength(FParamsAccessorData.rgBindings, Columns.Count);

      for i := 0 to Columns.Count - 1 do
        FParamsAccessorData.rgBindings[i].pObject := nil;

      BindMemorySize := 0;

      FBlobList := TObjectList.Create;
      FParamDescs := TParamDescs.Create;
      for i := 0 to Columns.Count - 1 do begin
        ParamDesc := TOLEDBParamDesc.Create;
        try
          ParamDesc.SetName(Columns[i].Name); // +++
          ParamDesc.SetDataType(Columns[i].DataType);
          ParamDesc.SetParamType(pdInput);
          FillBindingForParam(i + 1, ParamDesc, FConnection,
            FParamsAccessorData.rgBindings[i], BindMemorySize, False,
            TOLEDBLoaderColumn(Columns[i]).FIsWide);
          FParamsAccessorData.rgBindings[i].eParamIO := DBPARAMIO_NOTPARAM;
          ParamDesc.SetSize(FParamsAccessorData.rgBindings[i].cbMaxLen);
          if ParamDesc.GetDataType = dtGuid then
            ParamDesc.SetNull(True);
        finally
          FParamDescs.Add(ParamDesc);
        end;
      end;

      FParamsAccessorData.ExecuteParams.pData := Marshal.AllocHGlobal(BindMemorySize);
      FillChar(FParamsAccessorData.ExecuteParams.pData, BindMemorySize, 0);

      QueryIntf(FIRowsetFastLoad, {$IFDEF CLR}IAccessor{$ELSE}IID_IAccessor{$ENDIF}, FParamsAccessorData.Accessor);

    {$IFDEF CLR}
      rgBindingsGC := GCHandle.Alloc(FParamsAccessorData.rgBindings, GCHandleType.Pinned);
      rgBindings := Marshal.UnsafeAddrOfPinnedArrayElement(FParamsAccessorData.rgBindings, 0);
    {$ELSE}
      rgBindings := @FParamsAccessorData.rgBindings[0];
    {$ENDIF}

      for i := 0 to Columns.Count - 1 do begin
        FParamsAccessorData.rgBindings[i].eParamIO := DBPARAMIO_INPUT;
        SaveParamValue(FParamDescs[i], FParamsAccessorData.rgBindings[i], FParamsAccessorData{$IFDEF HAVE_COMPRESS}, cbNone{$ENDIF},
          {$IFDEF CLR}ParamGCHandle, {$ENDIF}FConnection.DBMSPrimaryVer, FConnection.ProviderPrimaryVer);
        FParamsAccessorData.rgBindings[i].eParamIO := DBPARAMIO_NOTPARAM;
      end;

      FConnection.Check(FParamsAccessorData.Accessor.CreateAccessor(DBACCESSOR_ROWDATA, Columns.Count,
        rgBindings, BindMemorySize, FParamsAccessorData.ExecuteParams.HACCESSOR, rgStatus), nil);
    except
      if FParamsAccessorData.ExecuteParams.pData <> nil then
        Marshal.FreeHGlobal(FParamsAccessorData.ExecuteParams.pData);
      FParamsAccessorData.ExecuteParams.pData := nil;

      if Length(FParamsAccessorData.rgBindings) <> 0 then begin
        for i := 0 to Columns.Count - 1 do begin
          if FParamsAccessorData.rgBindings[i].pObject <> nil then
            Marshal.FreeHGlobal(FParamsAccessorData.rgBindings[i].pObject);
        end;
        SetLength(FParamsAccessorData.rgBindings, 0);
      end;

      raise;
    end;
  finally
    Marshal.FreeHGlobal(rgStatus);
  {$IFDEF CLR}
    if IntPtr(rgBindingsGC) <> nil then
      rgBindingsGC.Free;
  {$ENDIF}
  end;
end;

procedure TOLEDBLoader.LoadRow;
begin
  FConnection.Check(FIRowsetFastLoad.InsertRow(FParamsAccessorData.ExecuteParams.HACCESSOR,
    FParamsAccessorData.ExecuteParams.pData), nil);
  Inc(FLoadedRows);
end;

procedure TOLEDBLoader.DoLoad;
begin
  if FLastRow >= FLoadedRows then
    LoadRow;
end;

procedure TOLEDBLoader.PutColumnData(Col: integer; Row: integer; const Value: variant);
var
  ParamDesc: CRAccess.TParamDesc;
  pLength: IntPtr;
{$IFDEF CLR}
  ParamGCHandle: TIntPtrDynArray;
{$ENDIF}
  Blob: MemData.TBlob;
begin
  if (FLastRow <> -1) and (Row > FLastRow + 1) then
    LoadRow;

  inherited;

  ParamDesc := FParamDescs.Items[Col];
  if (Columns[Col].DataType = dtBlob) and VarIsStr(Value) then begin
  {$IFDEF HAVE_COMPRESS}
    Blob := TCompressedBlob.Create;
  {$ELSE}
    Blob := {$IFDEF CLR}Devart.Dac.{$ENDIF}MemData.TBlob.Create;
  {$ENDIF}
    FBlobList.Add(Blob);
    Blob.AsString := Value;
    ParamDesc.SetObject(Blob);
    ParamDesc.SetNull(IntPtr(Blob.FirstPiece) = nil);
  end
  else begin
    ParamDesc.SetValue(Unassigned);
    ParamDesc.SetValue(Value);
  end;

  FParamsAccessorData.rgBindings[Col].eParamIO := DBPARAMIO_INPUT;
  SaveParamValue(ParamDesc, FParamsAccessorData.rgBindings[Col], FParamsAccessorData{$IFDEF HAVE_COMPRESS}, cbNone{$ENDIF},
    {$IFDEF CLR}ParamGCHandle, {$ENDIF}FConnection.DBMSPrimaryVer, FConnection.ProviderPrimaryVer);
  FParamsAccessorData.rgBindings[Col].eParamIO := DBPARAMIO_NOTPARAM;

  if not (ParamDesc.GetDataType in CharsByRef + BytesByRef) and not (ParamDesc.GetDataType in [dtUnknown]) then begin
    pLength := PtrOffset(FParamsAccessorData.ExecuteParams.pData, FParamsAccessorData.rgBindings[Col].obLength);
    Marshal.WriteInt32(pLength, FParamsAccessorData.rgBindings[Col].cbMaxLen);
  end;
end;

procedure TOLEDBLoader.Finish;
begin
  inherited;

  try
    if FIRowsetFastLoad <> nil then
      FConnection.Check(FIRowsetFastLoad.Commit(True), nil);

    FBlobList.Free;
    FParamDescs.Free;

    if FParamsAccessorData.ExecuteParams.pData <> nil then
      FConnection.Check(FParamsAccessorData.Accessor.ReleaseAccessor(FParamsAccessorData.ExecuteParams.HACCESSOR, nil), nil);
  finally
    FParamsAccessorData.ExecuteParams.HACCESSOR := 0;
    FParamsAccessorData.Accessor := nil;
    if FParamsAccessorData.ExecuteParams.pData <> nil then
      Marshal.FreeHGlobal(FParamsAccessorData.ExecuteParams.pData);
    FParamsAccessorData.ExecuteParams.pData := nil;

    FIOpenRowset := nil;
    FIRowsetFastLoad := nil;
    Reset;
  end;
end;

class function TOLEDBLoader.GetRecordSetClass: TCRRecordSetClass;
begin
  Result := TOLEDBRecordSet;
end;

procedure TOLEDBLoader.SetConnection(Value: TCRConnection);
begin
  inherited;

  FConnection := TOLEDBConnection(Value);
end;

procedure TOLEDBLoader.FillColumn(Column: TCRLoaderColumn; FieldDesc: TFieldDesc);
begin
  inherited;

  with TOLEDBLoaderColumn(Column) do begin
    FIsWide := (FieldDesc.SubDataType and dtWide) <> 0;
  end;
end;

{ TOLEDBProperties }

{ TOLEDBPropertiesSet }

const
  MaxPropCount = 20;

constructor TOLEDBPropertiesSet.Create(Connection: TOLEDBConnection; const GuidPropertySet: TGUID);
begin
  inherited Create;

  FConnection := Connection;

  FInitPropSet := Marshal.AllocHGlobal(SizeOf(DBPROPSET));
  FInitPropSet.cProperties := 0;
  FInitPropSet.guidPropertySet := GuidPropertySet;
  FInitPropSet.rgProperties := nil;

  FInitPropSet.rgProperties := Marshal.AllocHGlobal(MaxPropCount * SizeOfDBProp);
  FillChar(FInitPropSet.rgProperties, MaxPropCount * SizeOfDBProp, 0);
end;

destructor TOLEDBPropertiesSet.Destroy;
var
  i: integer;
  rgProperty: PDBProp;
begin
  if IntPtr(FInitPropSet) <> nil then begin
    if FInitPropSet.rgProperties <> nil then begin
      for i := 0 to Integer(FInitPropSet.cProperties) - 1 do begin
        rgProperty := GetDBPropPtr(i);
        rgProperty.vValue := Unassigned;
      end;

      Marshal.FreeHGlobal(FInitPropSet.rgProperties);
    end;

    Marshal.FreeHGlobal(FInitPropSet);
  end;

  inherited;
end;

procedure TOLEDBPropertiesSet.Check(const Status: HRESULT);
begin
  try
    FConnection.Check(Status, nil);
  except
    on E: Exception do begin
      AddInfoToErr(E, GetInitPropSetStatus, []);
      raise E;
    end;
  end;
end;

function TOLEDBPropertiesSet.GetInitPropSetStatus: _string;
var
  i: integer;
  p: PDBProp;
begin
  Result := GUIDToString(FInitPropSet.guidPropertySet);

  for i := 0 to FInitPropSet.cProperties - 1 do begin
    p := GetDBPropPtr(i);
    if p.dwStatus <> 0 then
      Result := _Format('%s'#$D#$A'[%d] := $%X. PropId := %d', [Result, i, p.dwStatus, p.dwPropertyID]);
  end;

end;

function TOLEDBPropertiesSet.GetDBPropPtr(Index: UINT): PDBProp;
begin
  Assert(Index <= FInitPropSet.cProperties);
  Result := PtrOffset(FInitPropSet.rgProperties, Integer(Index * SizeOfDBProp));
end;

function TOLEDBPropertiesSet.InitProp(const dwPropertyID: DBPROPID; const Required: boolean = False): PDBProp;
begin
  Assert(FInitPropSet.cProperties <= MaxPropCount);

  Result := GetDBPropPtr(FInitPropSet.cProperties);
  Result.dwPropertyID := dwPropertyID;
  if Required then
    Result.dwOptions := DBPROPOPTIONS_REQUIRED
  else
    Result.dwOptions := DBPROPOPTIONS_OPTIONAL;
  Result.colid := DB_NULLID;
end;


procedure TOLEDBPropertiesSet.AddPropSmallInt(const dwPropertyID: DBPROPID; const Value: Smallint);
var
  p: PDBProp;
begin
  p := InitProp(dwPropertyID);
  p.vValue := VarAsType(Value, VT_I2);

  FInitPropSet.cProperties := FInitPropSet.cProperties + 1;
end;

procedure TOLEDBPropertiesSet.AddPropInt(const dwPropertyID: DBPROPID; const Value: Integer);
var
  p: PDBProp;
begin
  p := InitProp(dwPropertyID, True);
  p.vValue := VarAsType(Value, VT_I4);

  FInitPropSet.cProperties := FInitPropSet.cProperties + 1;
end;

procedure TOLEDBPropertiesSet.AddPropUInt(const dwPropertyID: DBPROPID; const Value: Cardinal);
var
  p: PDBProp;
begin
  p := InitProp(dwPropertyID, True);
  p.vValue := VarAsType(Integer(Value), VT_UI4);
{$IFNDEF CLR}
  TVarData(p.vValue).VType := VT_UI4;
{$ENDIF}

  FInitPropSet.cProperties := FInitPropSet.cProperties + 1;
end;

procedure TOLEDBPropertiesSet.AddPropBool(const dwPropertyID: DBPROPID; const Value: boolean; const Required: boolean = False);
var
  p: PDBProp;
begin
  p := InitProp(dwPropertyID, Required);
  p.vValue := VarAsBooleanType(Value);

  FInitPropSet.cProperties := FInitPropSet.cProperties + 1;
end;

procedure TOLEDBPropertiesSet.AddPropStr(const dwPropertyID: DBPROPID; const Value: _string; const Required: boolean = False);
var
  p: PDBProp;
begin
  p := InitProp(dwPropertyID, Required);
  p.vValue := Value;

  FInitPropSet.cProperties := FInitPropSet.cProperties + 1;
end;

procedure TOLEDBPropertiesSet.AddPropIntf(const dwPropertyID: DBPROPID; const Value: TInterfacedObject; const Required: boolean = False);
var
  p: PDBProp;
  pData: IntPtr;
begin
  p := InitProp(dwPropertyID, Required);
  pData := Marshal.GetIUnknownForObject(Value);
{$IFDEF CLR}
  Marshal.WriteInt64(p, 36, 0);
  Marshal.WriteInt64(p, 36 + 8, 0);
  Marshal.WriteInt16(IntPtr(p), 36, vt_unknown);
  Marshal.WriteIntPtr(IntPtr(p), 44, IntPtr(pData));
{$ELSE}
  p.vValue := IUnknown(pData);
{$ENDIF}

  FInitPropSet.cProperties := FInitPropSet.cProperties + 1;
end;

procedure TOLEDBPropertiesSet.SetProperties(Obj: IDBProperties);
begin
  Assert(Obj <> nil);
  Check(Obj.SetProperties(1, PDBPropSetArray(FInitPropSet)));
end;

procedure TOLEDBPropertiesSet.SetProperties(Obj: ISessionProperties);
begin
  Assert(Obj <> nil);
  Check(Obj.SetProperties(1, PDBPropSetArray(FInitPropSet)));
end;

procedure TOLEDBPropertiesSet.SetProperties(Obj: ICommandProperties);
begin
  Assert(Obj <> nil);
  Check(Obj.SetProperties(1, PDBPropSetArray(FInitPropSet)));
end;

{ TOLEDBPropertiesGet }

constructor TOLEDBPropertiesGet.Create(Connection: TOLEDBConnection; const GuidPropertySet: TGUID);
begin
  inherited Create;

  FConnection := Connection;

  FInitPropSet := Marshal.AllocHGlobal(SizeOf(DBPROPSET));
  FInitPropSet.cProperties := 0;
  FInitPropSet.rgProperties := nil;
  FInitPropSet.guidPropertySet := GuidPropertySet;
end;

destructor TOLEDBPropertiesGet.Destroy;
begin
  Marshal.FreeHGlobal(FInitPropSet);
  inherited;
end;

procedure TOLEDBPropertiesGet.AddPropId(Id: DBPROPID);
begin
  Assert(FPropIdsGC = nil);
  SetLength(FPropIds, Length(FPropIds) + 1);
  FPropIds[Length(FPropIds) - 1] := Id;
end;

procedure TOLEDBPropertiesGet.Check(const Status: HRESULT);
begin
  FConnection.Check(Status, nil);
end;

function TOLEDBPropertiesGet.GetDBPropPtr(rgProperties: PDBPropArray; Index: UINT): PDBProp;
begin
  Assert(Index <= FInitPropSet.cProperties);
  Result := PtrOffset(rgProperties, Integer(Index * SizeOfDBProp));
end;

procedure TOLEDBPropertiesGet.PrepareToGet;
begin
  FPropIdsGC := AllocGCHandle(FPropIds, True);
  FInitPropSet.rgProperties := GetAddrOfPinnedObject(FPropIdsGC);
  FInitPropSet.cProperties := Length(FPropIds);
end;

procedure TOLEDBPropertiesGet.ProcessResult(rgPropertySets: PDBPropSet; var PropValues: TPropValues);
var
  i: integer;
begin
  SetLength(PropValues, FInitPropSet.cProperties);
  for i := 0 to FInitPropSet.cProperties - 1 do
    PropValues[i] := GetDBPropPtr(rgPropertySets.rgProperties, i).vValue;
end;

procedure TOLEDBPropertiesGet.ClearResult(rgPropertySets: PDBPropSet);
var
  i: integer;
begin
  FreeGCHandle(FPropIdsGC);
  FPropIdsGC := nil;
  FInitPropSet.rgProperties := nil;

  for i := 0 to FInitPropSet.cProperties - 1 do
    GetDBPropPtr(rgPropertySets.rgProperties, i).vValue := Unassigned;

  FreeCoMem(rgPropertySets.rgProperties);
  FreeCoMem(rgPropertySets);
  //FConnection.Malloc.Free(rgPropertySets.rgProperties);
  //FConnection.Malloc.Free(rgPropertySets);
end;

procedure TOLEDBPropertiesGet.GetProperties(Obj: IDBProperties; var PropValues: TPropValues);
var
  cPropertySets: UINT;
  rgPropertySets: PDBPropSet;
begin
  Assert(Obj <> nil);
  PrepareToGet;
  try
    Check(Obj.GetProperties(1, PDBPropIDSetArray(FInitPropSet), cPropertySets, rgPropertySets));
    ProcessResult(rgPropertySets, PropValues);
  finally
    ClearResult(rgPropertySets);
  end;
end;

procedure TOLEDBPropertiesGet.GetProperties(Obj: IRowsetInfo; var PropValues: TPropValues);
var
  cPropertySets: UINT;
  rgPropertySets: PDBPropSet;
begin
  Assert(Obj <> nil);
  PrepareToGet;
  try
    Check(Obj.GetProperties(1, PDBPropIDSetArray(FInitPropSet), cPropertySets, rgPropertySets));
    ProcessResult(rgPropertySets, PropValues);
  finally
    ClearResult(rgPropertySets);
  end;
end;

procedure TOLEDBPropertiesGet.GetProperties(Obj: ICommandProperties; var PropValues: TPropValues);
var
  cPropertySets: UINT;
  rgPropertySets: PDBPropSet;
begin
  Assert(Obj <> nil);
  PrepareToGet;
  try
    Check(Obj.GetProperties(1, PDBPropIDSetArray(FInitPropSet), cPropertySets, rgPropertySets));
    ProcessResult(rgPropertySets, PropValues);
  finally
    ClearResult(rgPropertySets);
  end;
end;

{ TOLEDBErrors }

constructor TOLEDBErrors.Create;
begin
  inherited;
  FList := TList.Create;
end;

destructor TOLEDBErrors.Destroy;
begin
  Clear;
  FList.Free;
  inherited;
end;

procedure TOLEDBErrors.Clear;
var
  i: integer;
begin
  for i := 0 to FList.Count - 1 do
    TObject(FList[i]).Free;
  FList.Clear;
end;

function TOLEDBErrors.GetCount: integer;
begin
  Result := FList.Count;
end;

function TOLEDBErrors.GetError(Index: Integer): EOLEDBError;
begin
  Result := EOLEDBError(FList[Index]);
end;

procedure TOLEDBErrors.Assign(Source: TOLEDBErrors);
begin
  Clear;
  Add(Source);
end;

procedure TOLEDBErrors.Add(Source: TOLEDBErrors);
var
  i: integer;
  SrcErr, DstErr: EOLEDBError;
begin
  for i := 0 to Source.Count - 1 do begin
    SrcErr := Source.Errors[i];
    DstErr := nil;
    if SrcErr is EMSError then
      DstErr := EMSError.Create(SrcErr.ErrorCode, SrcErr.Message)
    else
      if SrcErr is EOLEDBError then
        DstErr := EOLEDBError.Create(SrcErr.ErrorCode, SrcErr.Message)
      else
        Assert(False);

    DstErr.Assign(SrcErr);
    FList.Add(DstErr);
  end;
end;

{ EOLEDBError }

constructor EOLEDBError.Create(ErrorCode: integer; Msg: WideString);
begin
{$IFDEF LITE}
  inherited Create(Msg);

  FErrorCode := ErrorCode;
  Message := Msg;
{$ELSE}
  inherited Create(ErrorCode, Msg);
{$ENDIF}
  FMessageWide := Msg;

  FErrors := TOLEDBErrors.Create;
end;

destructor EOLEDBError.Destroy;
begin
  FErrors.Free;
  inherited;
end;

function EOLEDBError.GetErrorCount: integer;
begin
  Result := FErrors.Count;
end;

function EOLEDBError.GetError(Index: Integer): EOLEDBError;
begin
  Result := FErrors[Index];
end;

procedure EOLEDBError.Assign(Source: EOLEDBError);
begin
  FOLEDBErrorCode := Source.FOLEDBErrorCode;
  Fiid := Source.Fiid;
{$IFNDEF LITE}
  Component := Source.Component;
{$ENDIF}
  FErrors.Assign(Source.FErrors);
end;

{ EMSError }

constructor EMSError.Create(const pServerErrorInfo: SSERRORINFO; OLEDBErrorCode: integer; Msg: WideString);
begin
  inherited Create(pServerErrorInfo.lNative, Msg);
{$IFDEF LITE}
  FErrorCode := pServerErrorInfo.lNative;
{$ENDIF}

  FMSSQLErrorCode := pServerErrorInfo.lNative;
  FServerName := pServerErrorInfo.pwszServer;
  FProcName := pServerErrorInfo.pwszProcedure;
  FState := pServerErrorInfo.bState;
  FSeverityClass := pServerErrorInfo.bClass;
  FLineNumber := pServerErrorInfo.wLineNumber;
  FLastMessage := pServerErrorInfo.pwszMessage;
end;

procedure EMSError.Assign(Source: EOLEDBError);
var
  Src: EMSError;
begin
  inherited;

  if Source is EMSError then begin
    Src := EMSError(Source);
    FMSSQLErrorCode := Src.FMSSQLErrorCode;

    FServerName := Src.ServerName;
    FProcName := Src.ProcName;
    FState := Src.State;
    FSeverityClass := Src.SeverityClass;
    FLineNumber := Src.LineNumber;
    FLastMessage := Src.LastMessage;
  end;
end;

{$IFNDEF LITE}
function EMSError.IsFatalError: boolean;
begin
  Result := SeverityClass >= 20; {fatal error}
  if not Result then
    Result :=
      ((SeverityClass = 16) and (State = 1) and (MSSQLErrorCode <= NE_MAX_NETERROR))  // Network Library
    or
      ((SeverityClass = 10) and (State = 1) and (MSSQLErrorCode = WSAECONNRESET)) // Win sockets
    or
      ((SeverityClass = 16) and (State = 1) and (MSSQLErrorCode = WSAECONNRESET)) // Win sockets
    or
      ((SeverityClass = 16) and (State = 1) and (MSSQLErrorCode = ERROR_PIPE_NOT_CONNECTED)) // Named pipes
    or
      ((SeverityClass = 16) and (State = 1) and (MSSQLErrorCode = ERROR_NETNAME_DELETED)) // Named pipes
    or
      ((SeverityClass = 16) and (State = 1) and (MSSQLErrorCode = ERROR_NO_DATA)); // Named pipes being closed
end;

function EMSError.IsKeyViolation: boolean;
begin
  Result := (ErrorCode = 2627{Violation of %ls constraint '%.*ls'. Cannot insert duplicate key in object '%.*ls'.})
end;
{$ENDIF}

var
  DataSourceTypes: TBytes;
{$IFDEF CLR}
  DataSourceTypesGC: GCHandle;
{$ENDIF}

procedure InitDataSourceTypes;
var
  pDataSourceTypes: IntPtr;
  byteIndex: integer;

  function AddType(const TypeName: WideString): IntPtr;
  var
    Cnt: integer;
  begin
    Result := PtrOffset(pDataSourceTypes, byteIndex);
    Cnt := Encoding.Unicode.{$IFNDEF VER5}GetBytes{$ELSE}GetBytesWide{$ENDIF}(TypeName, 0, Length(TypeName), DataSourceTypes, byteIndex);
    DataSourceTypes[byteIndex + Cnt] := 0;
    DataSourceTypes[byteIndex + Cnt + 1] := 0;
    byteIndex := byteIndex + Cnt + 2;
    Assert(byteIndex + Cnt < Length(DataSourceTypes));
  end;

begin
  SetLength(DataSourceTypes, 350);
{$IFDEF CLR}
  DataSourceTypesGC := GCHandle.Alloc(DataSourceTypes, GCHandleType.Pinned);
  pDataSourceTypes := Marshal.UnsafeAddrOfPinnedArrayElement(DataSourceTypes, 0);
{$ELSE}
  pDataSourceTypes := @DataSourceTypes[0];
{$ENDIF}
  byteIndex := 0;

  dstSmallint := AddType('smallint');
  dstInt := AddType('int');
  dstReal := AddType('real');
  dstFloat := AddType('float');
  dstMoney := AddType('money');
  dstDateTime := AddType('datetime');
  dstNVarChar := AddType('nvarchar');
  dstNVarCharMax := AddType('nvarchar(max)');
  dstVarChar := AddType('varchar');
  dstVarCharMax := AddType('varchar(max)');
  dstBit := AddType('bit');
  dstTinyInt := AddType('tinyint');
  dstBigint := AddType('bigint');
  dstSql_variant := AddType('sql_variant');
  dstImage := AddType('image');
  dstBinary := AddType('binary');
  dstVarBinary := AddType('varbinary');
  dstGuid := AddType('uniqueidentifier');
  dstXML := AddType('xml');
  dstTable := AddType('table');
end;

procedure FinalizeDataSourceTypes;
begin
{$IFDEF CLR}
  if IntPtr(DataSourceTypesGC) <> nil then
    DataSourceTypesGC.Free;
{$ENDIF}
end;

var
  OSVersionInfo: TOSVersionInfo;

initialization
  __UseRPCCallStyle := True;
  InitDataSourceTypes;
  ParamsInfoOldBehavior := False; // delete 03.06.2006
  // Windows Vista detecting
  OSVersionInfo.dwOSVersionInfoSize := SizeOf(TOSVersionInfo);
  Windows.GetVersionEx(OSVersionInfo);
  IsWindowsVista := OSVersionInfo.dwMajorVersion = 6;
  AnsiCodePageName := '';
  OLEDBSQLInfo := TOLEDBSQLInfo.Create(nil);

finalization
  OLEDBSQLInfo.Free;
  LocaleIdentifierList.Free;
{$IFDEF DEBUG} if StreamCnt <> 0 then MessageBox(0, PChar(IntToStr(StreamCnt) + ' Stream(s) hasn''t been released'), 'DA warning', MB_OK); {$ENDIF}
  FinalizeDataSourceTypes;
{$IFNDEF CLR}
  GlobaIMalloc := nil;
{$ENDIF}
end.
