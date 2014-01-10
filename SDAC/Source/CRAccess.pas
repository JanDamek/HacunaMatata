
//////////////////////////////////////////////////
//  DB Access Components
//  Copyright © 1998-2012 Devart. All right reserved.
//  Core Access
//  Created:            01.07.00
//////////////////////////////////////////////////

{$IFNDEF CLR}

{$I Dac.inc}

unit CRAccess;
{$ENDIF}
interface

uses
  Classes, SysUtils, {$IFDEF VER6P}Variants,{$ENDIF}
{$IFDEF CLR}
  System.Runtime.InteropServices, Borland.Vcl.TypInfo, System.Text,
{$ELSE}
  CLRClasses,
{$ENDIF}
{$IFDEF UNIX}
  unix,
{$ENDIF} 
{$IFNDEF LITE}
  CRVio, {$IFDEF MSWINDOWS}MTSCall,{$ENDIF}
{$ENDIF}
  CRTypes, CRParser, MemData, MemUtils;

const
// Props
  prUsername           = 1;  // char*
  prPassword           = 2;  // char*
  prServer             = 4;  // char*
  prAutoCommit         = 5;  // bool
  prSQL                = 10; // char*
  prScanParams         = 11; // bool
  prSQLType            = 12; // integer
  prRowsProcessed      = 13; // integer

  prUniDirectional     = 20; // bool
  prFetchRows          = 21; // integer
  prFetchAll           = 22; // bool
  prRowsFetched        = 23; // integer
  prExecuting          = 24; // bool
  prLongStrings        = 25; // bool
  prFlatBuffers        = 27; // bool
  prConvertEOL         = 28; // bool
  prIndexFieldNames    = 29; // char*
{$IFDEF HAVE_COMPRESS}
  prCompressBlobMode   = 30; // TCompressBlobMode
{$ENDIF}
  prDisconnectedMode   = 31; // bool
  prDisableParamScan   = 32; // bool
  prStoredProcIsQuery  = 33; // bool    TODO: Remove
  prQuoteNames         = 34; // bool

  prIsolationLevel     = 35; // TCRIsolationLevel
  prTransactionReadOnly = 37; // bool
  prDatabase           = 38; // string
  prPort               = 39; // integer
  prMaxStringSize      = 40; // integer
  prEnableBCD          = 41; // bool
{$IFDEF VER6P}
{$IFNDEF FPC}
  prEnableFmtBCD       = 42; // bool
{$ENDIF}
{$ENDIF}
  prCanReadParams      = 43; // boolean
  prReadOnly           = 44; // boolean
  prIsStoredProc       = 45; // boolean
  prIsSelectParams     = 46; // boolean   // for MySQL stored proc
  prLockFetchAll       = 47; // boolean;
  prDefaultSortType    = 48; // TSortType
  prLastInsertId       = 49; // int64
  prFieldsAsString     = 50; // boolean
  prExtendedFieldsInfo = 51; // boolean
  prDefaultValues      = 52; // boolean
  prFieldsOrigin       = 53; // boolean
  prUseDescribeParams  = 54; // boolean
  prRoAfterUpdate      = 55; // boolean

// Sub data types
const
  dtSingle = 1;
  dtUInt8  = 2;

type
  TCRConnection   = class;
  TCRTransaction  = class;
  TCRCommand      = class;
  TCRRecordSet    = class;
  TParamDesc      = class;
  TParamDescs     = class;
  TCRTablesInfo   = class;
  TSQLInfo        = class;
  TCRCursor       = class;
{$IFNDEF LITE}
{$IFDEF MSWINDOWS}
  TMTSTransaction = class;
{$ENDIF}
{$ENDIF}
  TCRMetaData     = class;
  TCRLoaderColumn = class;
  TCRLoader       = class;
  TCRAlerter      = class;

  TCRConnectionClass = class of TCRConnection;
  TCRTransactionClass = class of TCRTransaction;
  TCRCommandClass = class of TCRCommand;
  TCRRecordSetClass = class of TCRRecordSet;
  TParamDescClass = class of TParamDesc;
  TTableInfoClass = class of TCRTableInfo;
  TSQLInfoClass = class of TSQLInfo;
  TCRMetaDataClass = class of TCRMetaData;
  TCRLoaderColumnClass = class of TCRLoaderColumn;
  TCRLoaderClass = class of TCRLoader;
  TCRAlerterClass = class of TCRAlerter;

  TCommandType = (ctUnknown, ctStatement, ctCursor);

  TCursorState = (
    csInactive, // default state (TCRRecordSet.InternalOpen, TCustomDASQL.SQLChanged)
    csOpen, // ODAC only: OCI73 TOCICommand.InternalOpen
    csParsed, // ODAC only: OCI73 - statement parsed
    csPrepared, // statement prepared
    csBound, // ODAC only: parameters bound
    csExecuteFetchAll, // ODAC only: 
    csExecuting, // ODAC only(?): statement is executing (TCRCommand.Execute)
    csExecuted, // statement successfully executed
    csFetching, // setted on first TCRRecordSet.Fetch
    csFetchingAll, // ODAC, IbDAC specific. Setted on the FetchAll start
    csFetched // fetch finished or canceled
  );

  TErrorProc = procedure (E: Exception; var Fail, Reconnect: boolean {$IFNDEF LITE}; var Reexecute: boolean{$ENDIF}
    ; ReconnectAttempt: integer{$IFNDEF LITE}; var ConnLostCause: TConnLostCause{$ENDIF}) of object;
  TReconnectProc = procedure of object;
  TBoolProc = procedure (Value: boolean) of object;
  TBeforeFetchProc = procedure (out Cancel: boolean) of object;
  TAfterFetchProc = procedure of object;
  TDataChangeProc = procedure of object;
  TReadParamsProc = procedure of object;

  EFailOver = class(Exception)
  public
    FConnLostCause: TConnLostCause;

    constructor Create(ConnLostCause: TConnLostCause);
  end;

{ TCRConnection }

  TCRConnection = class
  private
    FOnError: TErrorProc;
    FOnReconnectError: TReconnectProc;
    FOnReconnectSuccess: TReconnectProc;
    FConnectionTime: Longword;
    FSQLinfo: TSQLInfo;

    function GetSQLInfo: TSQLInfo;
    
  protected
    FInternalTransaction: TCRTransaction;
    FConnected: boolean;
    FNativeConnection: boolean;
    FUsername: _string;
    FPassword: _string;
    FServer: _string;
    FAutoCommit: boolean;
    FConvertEOL: boolean;
    FIsValid: boolean;
    FPool: TObject;
    FPoolVersion: integer;
    FComponent: TObject;
    FDisconnectedMode: boolean;
    FEnableBCD: boolean;
  {$IFDEF VER6P}
  {$IFNDEF FPC}
    FEnableFMTBCD: boolean;
  {$ENDIF}
  {$ENDIF}

    FInProcessError: boolean;
    FReconnected: boolean;
    FDefaultSortType: TSortType;

  {$IFNDEF LITE}
    FIOHandler: TCRIOHandler;
    FHttpOptions: THttpOptions;
  {$ENDIF}

    function CreateSQLInfo: TSQLInfo; virtual;
  {$IFNDEF LITE}
    procedure SetHttpOptions(Value: THttpOptions);
  {$IFDEF MSWINDOWS}
    procedure Enlist(MTSTransaction: TMTSTransaction); virtual;
    procedure UnEnlist(MTSTransaction: TMTSTransaction); virtual;
  {$ENDIF}
  {$ENDIF}
    procedure DoError(E: Exception; var Fail: boolean); virtual;
    property AutoCommit: boolean read FAutoCommit write FAutoCommit;
    property EnableBCD: boolean read FEnableBCD write FEnableBCD;
  {$IFDEF VER6P}
  {$IFNDEF FPC}
    property EnableFMTBCD: boolean read FEnableFMTBCD write FEnableFMTBCD;
  {$ENDIF}
  {$ENDIF}

  public
    constructor Create; virtual;
    destructor Destroy; override;

    function GetCommandClass: TCRCommandClass; virtual; abstract;
    function GetTransactionClass: TCRTransactionClass; virtual; abstract;

    procedure Connect(const ConnectString: _string); virtual;
    procedure Disconnect; virtual; abstract;
    procedure Assign(Source: TCRConnection); virtual;
    procedure AssignConnect(Source: TCRConnection); virtual; abstract;

    function GetConnected: boolean;
    procedure SetConnected(Value: boolean);

    function GetInternalTransaction: TCRTransaction;
    procedure SetUsername(const Value: _string); virtual;
    procedure SetPassword(const Value: _string); virtual;
    procedure SetServer(const Value: _string); virtual;
    function GetUsername: _string;
    function GetPassword: _string;
    function GetServer: _string;

    function CheckIsValid: boolean; virtual; abstract;
  {$IFNDEF LITE}
    procedure ReturnToPool; virtual;
  {$ENDIF}

    function GetServerVersion: _string; virtual; abstract;
    function GetServerVersionFull: _string; virtual; abstract;
    function GetClientVersion: _string; virtual; abstract;

    function CanChangeDatabase: boolean; virtual;

    function SetProp(Prop: integer; const Value: variant): boolean; virtual;
    function GetProp(Prop: integer; var Value: variant): boolean; virtual;

    property OnError: TErrorProc read FOnError write FOnError;
    property OnReconnectError: TReconnectProc read FOnReconnectError write FOnReconnectError;
    property OnReconnectSuccess: TReconnectProc read FOnReconnectSuccess write FOnReconnectSuccess;

    property ConnectionTime: Longword read FConnectionTime; 
    property IsValid: boolean read FIsValid write FIsValid;
    property Pool: TObject read FPool write FPool;
    property PoolVersion: integer read FPoolVersion write FPoolVersion;
    property Component: TObject read FComponent write FComponent; // Is needed for failover
    property DisconnectedMode: boolean read FDisconnectedMode write FDisconnectedMode;
    property NativeConnection: boolean read FNativeConnection;
  {$IFNDEF LITE}
    property IOHandler: TCRIOHandler read FIOHandler write FIOHandler;
    property HttpOptions: THttpOptions read FHttpOptions write SetHttpOptions;
  {$ENDIF}
    property SQLInfo: TSQLInfo read GetSQLInfo;
  end;

{ TCRCommand }

  TColumnInfo = class
  public
    Name: _string;
    Table: _string;
    TableIndex: Integer;
    Expr: _string; // expression
    Alias: _string;
    Used: boolean; // is ColumnInfo used by another Field
    Described: boolean;
    Required: boolean;
  end;

  TColumnsInfo = class(TList)
  private
    function GetItem(Index: integer): TColumnInfo;
  public
    procedure Clear; override;
    property Items[Index: Integer]: TColumnInfo read GetItem; default;
  end;

  TCRCommand = class
  private
    FSQLInfo: TSQLInfo;

    function GetSQLInfo: TSQLInfo;
    
  protected
    FComponent: TObject;
    FConnection: TCRConnection;
    FSQL: _string;
    FUserSQL: _string;
    FParams: TParamDescs;
    FAutoCommit: boolean;
    FAfterExecute: TBoolProc;
    FExecuting: boolean;
    FScanParams: boolean;
    FDisableParamScan: boolean;
    FQuoteNames: boolean;
  {$IFDEF HAVE_COMPRESS}
    FCompressBlob: TCompressBlobMode;
  {$ENDIF}
    FEnableBCD: boolean;
  {$IFDEF VER6P}
  {$IFNDEF FPC}
    FEnableFMTBCD: boolean;
  {$ENDIF}
  {$ENDIF}
    FReadParams: TReadParamsProc;

    function CreateSQLInfo: TSQLInfo; virtual;
    procedure ParseSQLParam(ParsedSQL: _StringBuilder; Parser: TSQLParser; Params: TParamDescs; LeftQuote, RightQuote: _char; const RenamePrefix: _string); virtual;

    property Executing: boolean read FExecuting write FExecuting;
    property EnableBCD: boolean read FEnableBCD write FEnableBCD;
  {$IFDEF VER6P}
  {$IFNDEF FPC}
    property EnableFMTBCD: boolean read FEnableFMTBCD write FEnableFMTBCD;
  {$ENDIF}
  {$ENDIF}

  public
    constructor Create; virtual;
    destructor Destroy; override;

    class function GetTableInfoClass: TTableInfoClass; virtual;
    class function GetSQLInfoClass: TSQLInfoClass; virtual;
    class function GetParserClass: TSQLParserClass; virtual;

    procedure Prepare; virtual;
    procedure Unprepare; virtual;
    function GetPrepared: boolean; virtual; abstract;

    procedure Execute(Iters: integer = 1); virtual; abstract;

    procedure SetConnection(Value: TCRConnection); virtual;
    function GetConnection: TCRConnection;
    procedure SetTransaction(Value: TCRTransaction); virtual;
    function GetTransaction: TCRTransaction;
    function GetCursorState: TCursorState; virtual; abstract;
    procedure SetCursorState(Value: TCursorState); virtual; abstract;

    procedure SetSQL(const Value: _string); virtual;
    function ParseSQL(const SQL: _string; Params: TParamDescs; ReplaceAll: boolean = True; const RenamePrefix: _string = ''): _string; overload; virtual;
    procedure ParseSQL(ReplaceAll: boolean = True); overload;
    function CreateProcCall(const Name: _string; NeedDescribe: boolean; IsQuery: boolean): _string; virtual; abstract;

  { Params }
    function GetParamDescType: TParamDescClass; virtual;
    procedure ClearParams;
    function AddParam: TParamDesc; virtual;
    procedure DeleteParam(Index: integer);
    function GetParamCount: integer;
    function GetParam(Index: integer): TParamDesc;
    function FindParam(Name: _string): TParamDesc;

    function GetCursor: TCRCursor; virtual;
    procedure SetCursor(Value: TCRCursor); virtual;

    function SetProp(Prop: integer; const Value: variant): boolean; virtual;
    function GetProp(Prop: integer; var Value: variant): boolean; virtual;

    procedure BreakExec; virtual;

    property SQL: _string read FSQL write SetSQL;
    property Component: TObject read FComponent write FComponent; // Is needed for failover
    property AfterExecute: TBoolProc read FAfterExecute write FAfterExecute;
    property ReadParams: TReadParamsProc read FReadParams write FReadParams; // for SDAC
    property Params: TParamDescs read FParams write FParams;
    property SQLInfo: TSQLInfo read GetSQLInfo;
  end;

{ $IFNDEF LITE}

{ TCRTableInfo }

  TCRTableInfo = class(TObject)
  protected
    FOwner: TCRTablesInfo;
    FIndex: Integer;
    FTableName: _string;
    FTableAlias: _string;
    FIsView: boolean;
    procedure SetTableName(const Value: _string);
    procedure SetTableAlias(const Value: _string);
    function GetTableNameFull: _string; virtual;
    procedure SetTableNameFull(const Value: _string); virtual;
  public
    constructor Create(Owner: TCRTablesInfo); virtual;

    property TableName: _string read FTableName write SetTableName;
    property TableAlias: _string read FTableAlias write SetTableAlias;
    property TableNameFull: _string read GetTableNameFull write SetTableNameFull;
    property IsView: boolean read FIsView write FIsView;
    property Index: Integer read FIndex write FIndex;
  end;

  TCRTablesInfo = class
  private
    function GetItem(Index: Integer): TCRTableInfo;
    procedure SetItem(Index: Integer; const Value: TCRTableInfo);
  protected
    FCaseSensitive: boolean;
    FList: array of TCRTableInfo;
    FUpdateCount: Integer;
    FTableInfoClass: TTableInfoClass;
    FTableNameList: _TStringList;
    FTableAliasList: _TStringList;
    procedure InternalAdd(TableInfo: TCRTableInfo);
    procedure Changed;
    procedure TableNameChanged;
    procedure TableAliasChanged;
    function GetCount: Integer;
    procedure SetCaseSensitive(Value: boolean);
  public
    constructor Create(TableInfoClass: TTableInfoClass);
    destructor Destroy; override;
    function Add: TCRTableInfo;
    procedure Clear;
    procedure BeginUpdate; virtual;
    procedure EndUpdate; virtual;
    function FindByName(TableName: _string): TCRTableInfo;
    function IndexOf(TableInfo: TCRTableInfo): Integer;
    function IndexByName(TableName: _string): Integer;
    function IndexByAlias(TableAlias: _string): Integer;
    property Items[Index: Integer]: TCRTableInfo read GetItem write SetItem; default;
    property Count: Integer read GetCount;
    property TableInfoClass: TTableInfoClass read FTableInfoClass;
    property CaseSensitive: boolean read FCaseSensitive write SetCaseSensitive;
  end;
{ $ENDIF}

  TIdentCase = (icUpper, icLower, icMixed, icMixedCaseSensitive);

  TExtTableInfo = record
    Table: _string;
    Schema: _string;
    Catalog: _string;
    DBLink: _string;     
    Synonym: _string;
    Flag: byte;
  end;

  TExtTablesInfo = array of TExtTableInfo;

{ TSQLInfo }

  TSQLInfo = class
  private
    FParserClass: TSQLParserClass;
    
  protected
    procedure ParseExtColumnName(Parser: TSQLParser; var Code: integer; var Str: _string); virtual;
    function FirstCharQuotesNeed(Ch: _Char; IdCase: TIdentCase): boolean; virtual;
    function NextCharQuotesNeed(Ch: _Char; IdCase: TIdentCase): boolean; virtual;

  public
    constructor Create(ParserClass: TSQLParserClass); virtual;

    function LeftQuote: _char; virtual;
    function RightQuote: _char; virtual;
    function IdentCase: TIdentCase; virtual;
    function ParamQuoteAllowed: boolean; virtual;
    function ProcedureOverloadSeparator: _char; virtual;

    function Quote(const Value: _string): _string; overload;
    function Quote(const Value: _string; const LeftQ: _char; const RightQ: _char): _string; overload; virtual;
    function UnQuote(const Value: _string): _string; virtual;
    function IsQuoted(const Value: _string): boolean; virtual;
    function QuotesNeeded(const Value: _string): boolean; virtual;
    function QuoteIfNeed(const Value: _string): _string;
    function NormalizeName(const Value: _string; QuoteNames: boolean = False; UnQuoteNames: boolean = False): _string; overload; virtual;
    function NormalizeName(const Value: _string; const LeftQ: _char; const RightQ: _char; QuoteNames: boolean = False; UnQuoteNames: boolean = False): _string; overload; virtual;
    function ToStringConst(const Value: _string): _string;
    procedure SplitObjectName(const Name: _string; var Info: TExtTableInfo); virtual;

    procedure ParseTablesInfo(const SQL: _string; TablesInfo: TCRTablesInfo); virtual;
    procedure ParseColumnsInfo(const SQL: _string; ColumnsInfo: TColumnsInfo); virtual;

    function NamesFromList(List: _TStrings): _string; virtual;
    procedure NamesToList(Value: _string; List: _TStrings); virtual;
  end;

{ TCRFieldDesc}

  TCRFieldDesc = class (TFieldDesc)
  { $IFNDEF LITE}
  protected
    FTableInfo: TCRTableInfo;
    FActualNameQuoted: array[boolean] of _string; // cache for [QuoteNames]
    FDefaultExpr: _string;
  public
    function ActualNameQuoted(SQLInfo: TSQLInfo; QuoteNames: boolean): _string; virtual;

    function IsNational: boolean; virtual;

    property TableInfo: TCRTableInfo read FTableInfo write FTableInfo;
    property DefaultExpr: _string read FDefaultExpr write FDefaultExpr;
  { $ENDIF}
  end;

{ TCRRecordSet }

  TCRRecordSet = class (TMemData)
  private
  protected
    FCommand: TCRCommand;
    FUniDirectional: boolean;
    FFetchRows: integer;
    FNoData: boolean;
    FFetchAll: boolean;
    FLockFetchAll: boolean;
    FLongStrings: boolean;
    FFlatBuffers: boolean;
    FExtendedFieldsInfo: boolean;
    FDefaultValues: boolean;
    FFieldsOrigin: boolean;

    FAfterExecFetch: TBoolProc;
    FAfterFetchAll: TBoolProc;
    FOnBeforeFetch: TBeforeFetchProc;
    FOnAfterFetch: TAfterFetchProc;
    FOnDataChanged: TDataChangeProc;
    FWaitForFetchBreak: boolean;
    FCommandType: TCommandType;

  { $IFNDEF LITE}
    FTablesInfo: TCRTablesInfo;
  { $ENDIF}

    procedure CreateCommand; virtual; abstract;
    procedure FreeCommand;
    procedure SetCommand(Value: TCRCommand); virtual;

    function CanFetchBack: boolean; virtual; // Return True, if BlockMan is store only one block of records

  { Open/Close }
    function NeedInitFieldsOnPrepare: boolean; virtual;
    procedure InternalPrepare; override;
    procedure InternalUnPrepare; override;
    procedure InternalOpen(DisableInitFields: boolean = False); override;
    procedure InternalClose; override;

    function NeedInitFieldsOnFetch: boolean; virtual;
    procedure ExecFetch(DisableInitFields: boolean); virtual;
    procedure DecryptBuffer(Item: PItemHeader);
    procedure CreateBlockStruct(Block: PBlockHeader; RowsObtained: Integer; FetchBack: boolean = False);
    procedure InitFetchedBlock(Block: PBlockHeader; RowsObtained: Integer; FetchBack: boolean); virtual;
    procedure DoBeforeFetch(out Cancel: boolean); virtual;
    procedure DoAfterFetch; virtual;

  { Items/Data }
   // procedure PrepareData; override;

  { Fields }
    function NeedConvertEOL: boolean; override;

  { TablesInfo }
    function GetTableInfoClass: TTableInfoClass;

  { Sorting }
    procedure SetSortDefaults(SortColumn: TSortColumn); override;

    function GetComponent: TObject;
    procedure SetComponent(Value: TObject);
  public
    constructor Create; virtual;
    destructor Destroy; override;

  { Fields}
    function GetFieldDescType: TFieldDescClass; override;

  { Open/Close }
    procedure Prepare; override;
    procedure UnPrepare; override;
    procedure Disconnect; virtual;

    procedure Open; override;
    procedure Close; override;
    procedure ExecCommand; virtual; // Execute command

  { Records }
    procedure GetNextRecord(RecBuf: IntPtr); override;
    procedure GetPriorRecord(RecBuf: IntPtr); override;

  { Fetch }
    procedure FetchAll; virtual;
    procedure BreakFetch; virtual;// Breaks fetch. Can be called from other thread or in non-blocking mode 
    function CanDisconnect: boolean; virtual;

    function RowsReturn: boolean; virtual;

    function GetCommand: TCRCommand;
    procedure SetConnection(Value: TCRConnection); virtual;
    procedure SetTransaction(Value: TCRTransaction); virtual;
    procedure SetSQL(const Value: _string); virtual;

    function SetProp(Prop: integer; const Value: variant): boolean; virtual;
    function GetProp(Prop: integer; var Value: variant): boolean; virtual;

  { Filter }
    function IsCaseSensitive: boolean; virtual;

    procedure FilterUpdated; override;

    property CommandType: TCommandType read FCommandType write FCommandType;
    property AfterExecFetch: TBoolProc read FAfterExecFetch write FAfterExecFetch;
    property AfterFetchAll: TBoolProc read FAfterFetchAll write FAfterFetchAll;
    property OnBeforeFetch: TBeforeFetchProc read FOnBeforeFetch write FOnBeforeFetch;
    property OnAfterFetch: TAfterFetchProc read FOnAfterFetch write FOnAfterFetch;
    property OnDataChanged: TDataChangeProc read FOnDataChanged write FOnDataChanged;

  { Sorting }
    procedure SortItems; override;

  { $IFNDEF LITE}
  { TablesInfo }
    property TablesInfo: TCRTablesInfo read FTablesInfo;
  { $ENDIF}
    property Component: TObject read GetComponent write SetComponent;
  end;

{ TParamDesc }

  TParamDirection = (pdUnknown, pdInput, pdOutput, pdInputOutput, pdResult);

  TParamDesc = class
  private
  protected
    FName: _string;
    FDataType: word;
    FSubDataType: word;
    FParamType: TParamDirection;

    FSize: integer;
    FData: variant; // pointer;
    FIsNull: boolean;
    FIsBound: boolean;
    FConvertEOL: boolean;
    FNational: boolean;

    property Name: _string read FName write FName;
    property DataType: word read FDataType write FDataType;
    property SubDataType: word read FSubDataType write FSubDataType;
    property ParamType: TParamDirection read FParamType write FParamType;
    property Size: integer read FSize write FSize;

  public
    constructor Create; virtual;
    destructor Destroy; override;

    procedure Clear;

    function GetName: _string;
    procedure SetName(Value: _string);

    function GetDataType: word;
    procedure SetDataType(Value: word); virtual;

    function GetSubDataType: word;
    procedure SetSubDataType(Value: word); virtual;

    function GetParamType: TParamDirection;
    procedure SetParamType(Value: TParamDirection); virtual;

    function GetSize: integer;
    procedure SetSize(Value: integer); virtual;

    function GetValue: variant; virtual;
    procedure SetValue(const Value: variant); virtual;

    function GetObject: TSharedObject; virtual;
    procedure SetObject(Value: TSharedObject); virtual;

    function GetNull: boolean; virtual;
    procedure SetNull(const Value: boolean); virtual;

    function GetIsBound: boolean;
    procedure SetIsBound(Value: boolean); virtual;

    function GetNational: boolean; virtual;
    procedure SetNational(Value: boolean); virtual;

    procedure SetConvertEOL(const Value: boolean);

    property Value: variant read FData write SetValue;
  end;

{ TParamDescs }

  TParamDescs = class (TDAList)
  private
    function GetItems(Index: integer): TParamDesc;

  public
    destructor Destroy; override;

    procedure Clear; override;

    function FindParam(Name: _string): TParamDesc;
    function ParamByName(Name: _string): TParamDesc;

    property Items[Index: integer]: TParamDesc read GetItems; default;
  end;

{ TCRConnections }

  TCRConnections = class(TDAList)
  private
    function GetItems(Index: integer): TCRConnection;
  public
    property Items[Index: integer]: TCRConnection read GetItems; default;
  end;

{ TCRTransaction }

  TCRErrorProc = procedure (E: Exception; var Fail: boolean) of object;

  TCRIsolationLevel = (ilReadCommitted, ilReadUnCommitted, ilRepeatableRead, ilIsolated, ilSnapshot, ilCustom);
  TCRTransactionAction = (taCommit, taRollback);

  TCRTransaction = class
  protected
    FActive: boolean;
    FComponent: TObject;
    FOnError: TCRErrorProc;
    FConnections: TCRConnections;
    FIsolationLevel: TCRIsolationLevel;
    FReadOnly: boolean;
    FNativeTransaction: boolean;

  public
    constructor Create; virtual;
    destructor Destroy; override;
    
    procedure CheckInactive;
    procedure CheckActive;

    function AddConnection(Connection: TCRConnection): boolean; virtual;
    function RemoveConnection(Connection: TCRConnection): boolean; virtual;

    function SetProp(Prop: integer; const Value: variant): boolean; virtual;
    function GetProp(Prop: integer; var Value: variant): boolean; virtual;

    // return transaction state w/o server query
    function GetInTransaction: boolean; virtual;
    // In ODAC executes query to detect actual transaction state.
    // If CanActivate = True, transaction start and transaction end on server side can be detected.
    // If CanActivate = False, only transaction end can be detected.
    function DetectInTransaction(CanActivate: boolean): boolean; virtual;
    procedure AssignConnect(Source: TCRTransaction); virtual;

    procedure StartTransaction; virtual;
    procedure Commit; virtual;
    procedure Rollback; virtual;

    procedure CommitRetaining; virtual;
    procedure RollbackRetaining; virtual;
    procedure Savepoint(const Name: _string); virtual;
    procedure ReleaseSavepoint(const Name: _string); virtual;
    procedure RollbackToSavepoint(const Name: _string); virtual;

    procedure Reset; virtual;
    function CanRestoreAfterFailover: boolean; virtual;

    property Component: TObject read FComponent write FComponent; // Is needed for failover    
    property OnError: TCRErrorProc read FOnError write FOnError;
  end;

{$IFDEF MSWINDOWS}
{$IFNDEF LITE}
  TMTSTransaction = class(TCRTransaction)
  private
    FMTSGC: ICRTransactionDispenserSC;
    FMTSTrans: ICRTransactionSC;

  protected
    procedure StartMTSTransaction; virtual;
    procedure CompleteMTSTransaction(Commit: boolean); virtual;

    procedure EnlistLink(Connection: TCRConnection); virtual;
    procedure UnEnlistLink(Connection: TCRConnection); virtual;
  public
    destructor Destroy; override;

    function AddConnection(Connection: TCRConnection): boolean; override;
    function RemoveConnection(Connection: TCRConnection): boolean; override;

    procedure StartTransaction; override;
    procedure Commit; override;
    procedure Rollback; override;

    procedure Savepoint(const Name: _string); override;
    procedure ReleaseSavepoint(const Name: _string); override;
    procedure RollbackToSavepoint(const Name: _string); override;

    property MTSTransaction: ICRTransactionSC read FMTSTrans;
  end;
{$ENDIF}
{$ENDIF}

{ TCRMetaData }

{$IFDEF LITE}
  {$DEFINE DBX_METADATA}
{$ENDIF}

  TDataHelper = class
  private
    FData: TData;
    FRecBuf: IntPtr;

    procedure FreeBuffer;
    function GetFieldValue(Index: integer): variant;
    procedure SetFieldValue(Index: integer; const Value: variant);

  public
    constructor Create(Data: TData);
    destructor Destroy; override;

    procedure AllocBuffer;
    procedure InitRecord;
    procedure AppendRecord;
    function NextRecord: boolean;

    property FieldValues[Index: integer]: variant read GetFieldValue write SetFieldValue;
  end;

  TBooleanArray = array of boolean;

  TCRMetaData = class
  protected
    FMemData: TMemData;
    FMemDataHelper: TDataHelper;
    FRecordSet: TCRRecordSet;
    FRecordSetHelper: TDataHelper;
    FOperator: string;

    function CreateRecordSet: TCRRecordSet; virtual; abstract;
    procedure AddField(const AName: _string; AType: integer; ALength: integer = -1);
    procedure CopyRecord(const SourceIndices, DestIndices: array of integer);
    function ParseTypes(const ObjectTypes: _string; AllTypes: array of _string): TBooleanArray; overload;
    procedure ParseTypes(const ObjectTypes: _string; {out}TypesList: _TStringList); overload;
    procedure AddWhere(var WhereClause: _string; const Name, Value: _string; AddEmpty: boolean = False);

    function InternalGetMetaData(const MetaDataKind: _string; Restrictions: _TStrings): TData; overload; virtual;

    procedure CreateMetaDataKindsFields; virtual;
    procedure InternalGetMetaDataKindsList(List: _TStringList); overload; virtual;
    function GetMetaDataKinds: TData; virtual;
    procedure CreateRestrictionsFields; virtual;
    procedure InternalGetRestrictionsList(List: _TStringList; const MetaDataKind: _string); overload; virtual;
    function GetRestrictions(Restrictions: _TStrings): TData; virtual;
    procedure CreateTablesFields; virtual;
    function GetTables(Restrictions: _TStrings): TData; virtual; abstract;
    procedure CreateColumnsFields; virtual;
    function GetColumns(Restrictions: _TStrings): TData; virtual; abstract;
    procedure CreateProceduresFields; virtual;
    function GetProcedures(Restrictions: _TStrings): TData; virtual; abstract;
    procedure CreateProcedureParametersFields; virtual;
    function GetProcedureParameters(Restrictions: _TStrings): TData; virtual; abstract;
    procedure CreateIndexesFields; virtual;
    function GetIndexes(Restrictions: _TStrings): TData; virtual; abstract;
    procedure CreateIndexColumnsFields; virtual;
    function GetIndexColumns(Restrictions: _TStrings): TData; virtual; abstract;
    procedure CreateConstraintsFields; virtual;
    function GetConstraints(Restrictions: _TStrings): TData; virtual; abstract;

    function GetDatabases(Restrictions: _TStrings): TData; virtual;
    procedure CreateDatabasesFields; virtual;
    function GetDataTypes(Restrictions: _TStrings): TData; virtual;
    function GetUsers(Restrictions: _TStrings): TData; virtual;
    function GetRoles(Restrictions: _TStrings): TData; virtual;
    function GetUDTs(Restrictions: _TStrings): TData; virtual;
    function GetPackages(Restrictions: _TStrings): TData; virtual;
  {$IFDEF DBX_METADATA}
    procedure CreateObjectListFields; virtual;
  {$ENDIF}

  public
    constructor Create; virtual;
    destructor Destroy; override;

    function GetMetaData(Connection: TCRConnection; Transaction: TCRTransaction;
      const MetaDataKind: _string; Restrictions: _TStrings): TData; virtual;

    procedure GetMetaDataKindsList(List: _TStrings);
    procedure GetRestrictionsList(List: _TStrings; const MetaDataKind: _string);
  end;

{ TCRCursor }

  TCRCursor = class (TSharedObject)
  public
    function CanFetch: boolean; virtual; abstract;
  end;

{ TCRLoader }

  TCRLoaderColumn = class
  private
    FName: _string;
    FDataType: word;
    FSize: integer;
    FDataSize: integer;
    FPrecision: integer;
    FScale: integer;
  public
    constructor Create; virtual;

    property Name: _string read FName write FName;
    property DataType: word read FDataType write FDataType;
    property Size: integer read FSize write FSize;
    property DataSize: integer read FDataSize write FDataSize;
    property Precision: integer read FPrecision write FPrecision;
    property Scale: integer read FScale write FScale;
  end;

  TCRLoaderColumns = class(TList)
  private
    function GetColumn(Index: integer): TCRLoaderColumn;
    procedure SetColumn(Index: integer; Value: TCRLoaderColumn);
  public
    procedure Clear; override;
    function GetColumnIndexByName(Name: _string): Integer;
    function FindColumnByName(Name: _string): TCRLoaderColumn;
    function GetColumnByName(Name: _string): TCRLoaderColumn;

    property Items[Index: integer]: TCRLoaderColumn read GetColumn write SetColumn; default;
  end;

  TCRLoader = class
  private
  protected
    FConnection: TCRConnection;
    FTransaction: TCRTransaction;
    FTableName: _string;
    FColumns: TCRLoaderColumns;
    FLastRow: integer;
    FLoadedRows: integer;
    FSkipReadOnlyFieldDescs: boolean;

    class function GetRecordSetClass: TCRRecordSetClass; virtual;
    procedure SetConnection(Value: TCRConnection); virtual;
    procedure SetTransaction(Value: TCRTransaction); virtual;
    procedure CheckTableName;
    procedure FillColumn(Column: TCRLoaderColumn; FieldDesc: TFieldDesc); virtual;

    procedure DoPrepare; virtual;
    procedure DoPutColumnData(Col: integer; Row: integer; const Value: variant); virtual;
  public
    constructor Create; virtual;
    destructor Destroy; override;

    function SetProp(Prop: integer; const Value: variant): boolean; virtual;
    function GetProp(Prop: integer; var Value: variant): boolean; virtual;

    class function GetColumnClass: TCRLoaderColumnClass; virtual;
    procedure Prepare; virtual;
    procedure Reset; virtual;
    procedure DoLoad; virtual;
    procedure Finish; virtual;
    procedure PutColumnData(Col: integer; Row: integer; const Value: variant); virtual;
    procedure CreateColumns;
    procedure DiscardRow;

    property Connection: TCRConnection read FConnection write SetConnection;
    property Transaction: TCRTransaction read FTransaction write SetTransaction;
    property TableName: _string read FTableName write FTableName;
    property Columns: TCRLoaderColumns read FColumns;
  end;

{ TCRSimpleLoader }

  TCRSimpleLoader = class (TCRLoader)
  protected
    FCommand: TCRCommand;

    procedure CreateCommand; virtual; abstract;

    procedure DoLoadRow; virtual;
    procedure DoPrepare; override;
    procedure DoPutColumnData(Col: integer; Row: integer; const Value: variant); override;
  public
    constructor Create; override;
    destructor Destroy; override;

    procedure Reset; override;
    procedure PutColumnData(Col: integer; Row: integer; const Value: variant); override;
    procedure DoLoad; override;
    procedure Finish; override;
  end;

{ TCRAlerter }

  TCRAlerterEventCallback = procedure(const EventName, Message: _string) of object;
  TCRAlerterErrorCallback = procedure(E: Exception) of object;

  TCRAlerter = class
  protected
    FConnection: TCRConnection;
    FTransaction: TCRTransaction;
    FEventNames: _TStrings;
    FActive: boolean;
    FOnEvent: TCRAlerterEventCallback;
    FOnError: TCRAlerterErrorCallback;

    procedure SetConnection(Value: TCRConnection); virtual;
    procedure SetTransaction(Value: TCRTransaction); virtual;

  public
    constructor Create; virtual;
    destructor Destroy; override;

    function SetProp(Prop: integer; const Value: variant): boolean; virtual;
    function GetProp(Prop: integer; var Value: variant): boolean; virtual;

    procedure SendEvent(const EventName, Message: _string); virtual; abstract;
    procedure Start; virtual; abstract;
    procedure Stop; virtual; abstract;
    function IsActive: boolean; virtual;

    property Connection: TCRConnection read FConnection write SetConnection;
    property Transaction: TCRTransaction read FTransaction write SetTransaction;
    property EventNames: _TStrings read FEventNames;
    property OnEvent: TCRAlerterEventCallback read FOnEvent write FOnEvent;
    property OnError: TCRAlerterErrorCallback read FOnError write FOnError;
  end;

{$IFDEF UNIX}
function GetTickCount: Cardinal;
{$ENDIF}
function GetTickInterval(StartTickCount, FinishTickCount: Cardinal): Cardinal;
function GenerateTableName(const FieldDesc: TFieldDesc): _string;


implementation

uses
{$IFDEF VER6P}
  RTLConsts,
{$ELSE}
  Consts,
{$ENDIF}
{$IFDEF MSWINDOWS}
  Windows, {$IFNDEF LITE}ComObj,{$ENDIF}
{$ENDIF}
{$IFNDEF LITE}
  CRConnectionPool,
{$ENDIF}
  CRFunctions, DAConsts;

{ TCRAccess }

{$IFDEF UNIX}
function GetTickCount: Cardinal;
var
  tv: timeval;
begin
  fpgettimeofday(@tv, nil);
  {$RANGECHECKS OFF}
  Result := int64(tv.tv_sec) * 1000 + tv.tv_usec div 1000;
end;
{$ENDIF}

function GetTickInterval(StartTickCount, FinishTickCount: Cardinal): Cardinal;
begin
  // each 49.7 days ticks are reseted, so we should take it into attention
  // and use GetTickInterval for avoiding the out of range error
  if FinishTickCount >= StartTickCount then
    Result := FinishTickCount - StartTickCount
  else
    Result := Longword($FFFFFFFF) - StartTickCount + FinishTickCount + 1;
end;

function GenerateTableName(const FieldDesc: TFieldDesc): _string;
begin
  if TCRFieldDesc(FieldDesc).TableInfo <> nil then
    Result := TCRFieldDesc(FieldDesc).TableInfo.TableName
  else
    Result := '';
end;

{ EFailOver }

constructor EFailOver.Create(ConnLostCause: TConnLostCause);
begin
  FConnLostCause := ConnLostCause;
  inherited Create(''{$IFDEF FPC}+#0{$ENDIF});
end;

{ TCRConnection }

constructor TCRConnection.Create;
begin
  inherited;

  FConnected := False;
  FNativeConnection := True;
  FIsValid := True;
{$IFNDEF LITE}
  FHttpOptions := THttpOptions.Create;
{$ENDIF}
  FInternalTransaction := GetTransactionClass.Create;
  FInternalTransaction.AddConnection(Self);
  FSQLinfo := nil;
end;

destructor TCRConnection.Destroy;
begin
  Disconnect;
{$IFNDEF LITE}
  FHttpOptions.Free;
{$ENDIF}
  FInternalTransaction.Free;
  FSQLinfo.Free;

  inherited;
end;

procedure TCRConnection.Connect(const ConnectString: _string);
begin
  FConnectionTime := GetTickCount;
  FIsValid := True;
end;

procedure TCRConnection.Assign(Source: TCRConnection);
begin
  FServer := Source.FServer;
  FUsername := Source.FUsername;
  FPassword := Source.FPassword;
  FEnableBCD := Source.FEnableBCD;
{$IFDEF VER6P}
{$IFNDEF FPC}
  FEnableFMTBCD := Source.FEnableFMTBCD;
{$ENDIF}
{$ENDIF}
  FOnError := Source.FOnError;
{$IFNDEF LITE}
  FIOHandler := Source.FIOHandler;
{$ENDIF}
end;

function TCRConnection.GetSQLInfo: TSQLInfo;
begin
  if FSQLInfo = nil then
    FSQLInfo := CreateSQLInfo;
  Result := FSQLInfo;
end;

function TCRConnection.CreateSQLInfo: TSQLInfo;
begin
  Result := GetCommandClass.GetSQLInfoClass.Create(GetCommandClass.GetParserClass);
end;

procedure TCRConnection.DoError(E: Exception; var Fail: boolean);
var
  Reconnect: boolean;
  Attempt: Integer;
{$IFNDEF LITE}
  Reexecute: boolean;
  ConnLostCause: TConnLostCause;
{$ENDIF}
begin
  Attempt := 0;
  while not FInProcessError do begin
    Reconnect := Attempt > 0;
  {$IFNDEF LITE}
    Reexecute := False;
  {$ENDIF}
    if Assigned(OnError) then begin
      FInProcessError := True;
      try
        OnError(E, Fail, Reconnect{$IFNDEF LITE}, Reexecute{$ENDIF}, Attempt{$IFNDEF LITE}, ConnLostCause{$ENDIF});
      finally
        FInProcessError := False;
      end;
    end;

    if Reconnect then begin
      FReconnected := False;
      try
        FInProcessError := True;
        if Attempt = 0 then
          Disconnect;
      except // don't raise exception
      end;
      try
        Connect('');
        if Assigned(FOnReconnectSuccess) then
          FOnReconnectSuccess;
        FReconnected := True;
      except // don't raise exception
      end;
      FInProcessError := False;

    {$IFNDEF LITE}
      if FReconnected and Reexecute then
        raise EFailOver.Create(ConnLostCause); //Failover
    {$ENDIF}

      inc(Attempt);
    end;

    if not Reconnect and (Attempt > 0) then
      if not FReconnected and Assigned(OnReconnectError) then begin
        FInProcessError := True;
        FConnected := True; // to bypass "Value <> GetConnected" check in TCustomDAConnection.SetConnected
        try
          OnReconnectError;
        except // don't raise exception
        end;
        FConnected := False;
        FInProcessError := False;
      end;
    
    if not Reconnect or FReconnected then
      break;
  end;
end;

function TCRConnection.GetConnected: boolean;
begin
  Result := FConnected;
end;

procedure TCRConnection.SetConnected(Value: boolean);
begin
  if Value then
    Connect('')
  else
    Disconnect;
end;

function TCRConnection.GetInternalTransaction: TCRTransaction;
begin
  Result := FInternalTransaction;
end;

procedure TCRConnection.SetUsername(const Value: _string);
begin
  FUsername := Value;
end;

procedure TCRConnection.SetPassword(const Value: _string);
begin
  FPassword := Value;
end;

procedure TCRConnection.SetServer(const Value: _string);
begin
  FServer := Value;
end;

function TCRConnection.GetUsername: _string;
begin
  Result := FUsername;
end;

function TCRConnection.GetPassword: _string;
begin
  Result := FPassword;
end;

function TCRConnection.GetServer: _string;
begin
  Result := FServer;
end;

{$IFDEF MSWINDOWS}
{$IFNDEF LITE}
procedure TCRConnection.Enlist(MTSTransaction: TMTSTransaction);
begin
  raise Exception.Create('MS DTC transactions not supported');
end;

procedure TCRConnection.UnEnlist(MTSTransaction: TMTSTransaction);
begin
  raise Exception.Create('MS DTC transactions not supported');
end;
{$ENDIF}
{$ENDIF}

{$IFNDEF LITE}
procedure TCRConnection.ReturnToPool;
begin
  Assert(FPool <> nil);
  FOnError := nil;
  Component := nil;
  TCRConnectionPool(FPool).PutConnection(Self);
end;
{$ENDIF}

function TCRConnection.CanChangeDatabase: boolean;
begin
  Result := True;
end;

{$IFNDEF LITE}
procedure TCRConnection.SetHttpOptions(Value: THttpOptions);
begin
  FHttpOptions.Assign(Value);
end;
{$ENDIF}

function TCRConnection.SetProp(Prop: integer; const Value: variant): boolean;
begin
  Result := True;
  case Prop of
    prAutoCommit:
      FAutoCommit := Value;
    prConvertEOL:
      FConvertEOL := Value;
    prDisconnectedMode:
      FDisconnectedMode := Boolean(Value);
    prDefaultSortType:
      FDefaultSortType := TSortType(Value);
    prDatabase: ;
    prPort: ;
    prEnableBCD:
      FEnableBCD := Value;
  {$IFDEF VER6P}
  {$IFNDEF FPC}
    prEnableFmtBCD:
      FEnableFMTBCD := Value;
  {$ENDIF}
  {$ENDIF}
  else
    Assert(False, IntToStr(Prop));
    Result := False;
  end;
end;

function TCRConnection.GetProp(Prop: integer; var Value: variant): boolean;
begin
  Result := True;
  case Prop of
    prUsername: // used in Oracle dbExpress driver (TOraSQLMetaData.getProcedureParams)
                // to detect if schema name need to be included in procedure name
      Value := FUsername;
    prMaxStringSize:
      Value := 0;
    prLastInsertId:
      Value := 0;
  else
    Assert(False, IntToStr(Prop));
    Result := False;
  end;
end;

{ TColumnsInfo }

function TColumnsInfo.GetItem(Index: integer): TColumnInfo;
begin
  Result := TColumnInfo(inherited Items[Index]);
end;

procedure TColumnsInfo.Clear;
var
  i: integer;
begin
  for i := 0 to Count - 1 do
    Items[i].Free;

  inherited;
end;

{ TCRCommand }

constructor TCRCommand.Create;
begin
  inherited;

  FParams:= TParamDescs.Create;
  FScanParams := True;
  FSQLInfo := nil;
end;

destructor TCRCommand.Destroy;
begin
  FParams.Free;
  FSQLInfo.Free;

  inherited;
end;

procedure TCRCommand.Prepare;
begin
  SetCursorState(csPrepared);
end;

procedure TCRCommand.Unprepare;
begin
  SetCursorState(csInactive);
end;

procedure TCRCommand.SetConnection(Value: TCRConnection);
begin
  FConnection := Value;
end;

function TCRCommand.GetConnection: TCRConnection;
begin
  Result := FConnection;
end;

procedure TCRCommand.SetTransaction(Value: TCRTransaction);
begin
  // should be overloaded for Multiple Transactions databases
end;

function TCRCommand.GetTransaction: TCRTransaction;
begin
  // should be overloaded for Multiple Transactions databases
  Result := nil;
end;

procedure TCRCommand.SetSQL(const Value: _string);
begin
  if FUserSQL <> Value then begin
    FUserSQL := Value;
    if FDisableParamScan then
      FSQL := Value
    else
      FSQL := ParseSQL(Value, FParams);
  end;
end;

class function TCRCommand.GetTableInfoClass: TTableInfoClass;
begin
  Result := TCRTableInfo;
end;

class function TCRCommand.GetSQLInfoClass: TSQLInfoClass;
begin
  Result := TSQLInfo;
end;

class function TCRCommand.GetParserClass: TSQLParserClass;
begin
  Assert(False);
  Result := TSQLParser;
end;

function TCRCommand.GetSQLInfo: TSQLInfo;
begin
  if FConnection <> nil then
    Result := FConnection.SQLInfo
  else begin
    if FSQLInfo = nil then
      FSQLInfo := CreateSQLInfo;
    Result := FSQLInfo;
  end;
end;

function TCRCommand.CreateSQLInfo: TSQLInfo;
begin
  Result := GetSQLInfoClass.Create(Self.GetParserClass);
end;

procedure TCRCommand.ParseSQLParam(ParsedSQL: _StringBuilder; Parser: TSQLParser; Params: TParamDescs; LeftQuote, RightQuote: _char; const RenamePrefix: _string);
var
  Code: integer;
  St: _string;
  DogPresent: boolean;
  l: integer;
  ParamName: _string;
begin
  Code := Parser.GetNext(St);
  DogPresent := St = '@';
  if DogPresent then
    Code := Parser.GetNext(St); // Omit '@' in ParamName for BDE compatibility

  if ((Params <> nil) or (RenamePrefix <> '')) and ((Code = lcIdent) or (Code = lcNumber) or (Parser.KeywordLexems.IndexOf(St) <> -1)) // and (St <> '=')
  then begin
    if DogPresent then
      ParamName := '@' + St
    else
      ParamName := St;

    l := Length(ParamName);
    // remove quotes
    if (ParamName[1] = LeftQuote) and (ParamName[l] = RightQuote) then
      ParamName := Copy(ParamName, 2, l - 2);
    if Params <> nil then begin
      AddParam.SetName(ParamName);
      ParsedSQL.Append('?');
    end
    else
      ParsedSQL.Append(RenamePrefix + ParamName);
  end
  else // Labels in SQL Server, MySQL syntax and PL SQL Blocks (a := b).
  begin
    ParsedSQL.Append(':');
    if DogPresent then
      ParsedSQL.Append('@');
    ParsedSQL.Append(St);
  end;
end;

function TCRCommand.ParseSQL(const SQL: _string; Params: TParamDescs; ReplaceAll: boolean = True; const RenamePrefix: _string = ''): _string;
var
  ParsedSQL: _StringBuilder;
  Parser: TSQLParser;
  StartPos: integer;

  LeftQuote, RightQuote: _char;
begin
  LeftQuote := SQLInfo.LeftQuote;
  RightQuote := SQLInfo.RightQuote;

  ParsedSQL := _StringBuilder.Create(Length(SQL) + Length(SQL) div 2);
  try
    Parser := GetParserClass.Create(SQL);
    try
      if Params <> nil then
        Params.Clear;
      Parser.OmitBlank := False;
      Parser.OmitComment := True;
      Parser.QuotedString := True;
      Parser.DecSeparator := '.';
      Parser.ToBegin;
      StartPos := Parser.CurrPos;
      while Parser.ToLexem(':') do begin
        ParsedSQL.Append(Copy(SQL, StartPos + 1, Parser.CurrPos - StartPos - 1));
        ParseSQLParam(ParsedSQL, Parser, Params, LeftQuote, RightQuote, RenamePrefix);

        StartPos := Parser.CurrPos;
      end;
      ParsedSQL.Append(Copy(SQL, StartPos + 1, Parser.CurrPos - StartPos));
    finally
      Parser.Free;
    end;

    Result := ParsedSQL.ToString;
  finally
    ParsedSQL.Free;
  end;
end;

procedure TCRCommand.ParseSQL(ReplaceAll: boolean = True);
begin
  FSQL := ParseSQL(FUserSQL, FParams, ReplaceAll);
end;

{function TCRCommand.GetSQL: PChar;
begin
  Result := PChar(FSQL);
end;}

{ Params }

function TCRCommand.GetParamDescType: TParamDescClass;
begin
  Result := TParamDesc;
end;

procedure TCRCommand.ClearParams;
begin
  FParams.Clear;
end;

function TCRCommand.AddParam: TParamDesc;
begin
  Result := GetParamDescType.Create;
  FParams.Add(Result);
end;

procedure TCRCommand.DeleteParam(Index: integer);
begin
  TParamDesc(FParams[Index]).Free;
  FParams.Delete(Index);
end;

function TCRCommand.GetParamCount: integer;
begin
  Result := FParams.Count;
end;

function TCRCommand.GetParam(Index: integer): TParamDesc;
begin
  Result := FParams[Index];
end;

function TCRCommand.FindParam(Name: _string): TParamDesc;
begin
  Result := FParams.FindParam(Name);
end;

function TCRCommand.GetCursor: TCRCursor;
begin
  Result := nil;
end;

procedure TCRCommand.SetCursor(Value: TCRCursor);
begin
  raise Exception.Create(SOperationNotSupported);
end;

function TCRCommand.SetProp(Prop: integer; const Value: variant): boolean;
begin
  Result := True;
  case Prop of
    prAutoCommit:
      FAutoCommit := Value;
  {$IFDEF HAVE_COMPRESS}
    prCompressBlobMode:
      FCompressBlob := TCompressBlobMode(Integer(Value));
  {$ENDIF}
    prScanParams:
      if FScanParams <> Boolean(Value) then begin
        FScanParams := Boolean(Value);
        if FScanParams and (FUserSQL <> '') then
          ParseSQL;
      end;
    prDisableParamScan:
      FDisableParamScan := Value;
    prQuoteNames:
      FQuoteNames := Value;
    prStoredProcIsQuery:  //TODO: Remove
      ;
    prCanReadParams:
      ;
    prEnableBCD:
      FEnableBCD := Value;
  {$IFDEF VER6P}
  {$IFNDEF FPC}
    prEnableFmtBCD:
      FEnableFMTBCD := Value;
  {$ENDIF}
  {$ENDIF}
    prIsStoredProc:
      ;
  else
    Assert(False, IntToStr(Prop));
    Result := False;
  end;
end;

function TCRCommand.GetProp(Prop: integer; var Value: variant): boolean;
begin
  Result := True;
  case Prop of
    prExecuting:
      Value := FExecuting;
    prCanReadParams:
      Value := True;
    prIsSelectParams:
      Value := False;
    prUseDescribeParams:
      Value := False;
  else
    Assert(False, IntToStr(Prop));
    Result := False;
  end;
end;

procedure TCRCommand.BreakExec;
begin
end;

{ $IFNDEF LITE}
{ TCRTableInfo }

constructor TCRTableInfo.Create(Owner: TCRTablesInfo);
begin
  inherited Create;
  FOwner := Owner;
  FIndex := -1;
end;

procedure TCRTableInfo.SetTableName(const Value: _string);
begin
  FTableName := Value;
  if FOwner <> nil then
    FOwner.TableNameChanged;
end;

procedure TCRTableInfo.SetTableAlias(const Value: _string);
begin
  FTableAlias := Value;
  if FOwner <> nil then
    FOwner.TableAliasChanged;
end;

function TCRTableInfo.GetTableNameFull: _string;
begin
  Result := FTableName;
end;

procedure TCRTableInfo.SetTableNameFull(const Value: _string);
begin
end;

{ TCRTablesInfo }

constructor TCRTablesInfo.Create(TableInfoClass: TTableInfoClass);
begin
  inherited Create;
  FTableInfoClass := TableInfoClass;
  FTableNameList := _TStringList.Create;
  FTableAliasList := _TStringList.Create;
{$IFDEF VER6P}
  FTableNameList.CaseSensitive := False;
  FTableAliasList.CaseSensitive := False;
{$ENDIF}
  FUpdateCount := 0;
end;

destructor TCRTablesInfo.Destroy;
begin
  Clear;
  FTableNameList.Free;
  FTableAliasList.Free;
  inherited Destroy;
end;

procedure TCRTablesInfo.InternalAdd(TableInfo: TCRTableInfo);
var
  c: integer;
begin
  c := Count;
  SetLength(FList, c + 1);
  FList[c] := TableInfo;
  TableInfo.Index := c;
end;

function TCRTablesInfo.Add: TCRTableInfo;
begin
  Result := FTableInfoClass.Create(Self);
  InternalAdd(Result);
end;

procedure TCRTablesInfo.Clear;
var
  i: Integer;
begin
  if Count > 0 then begin
    for i := 0 to Count - 1 do
      FList[i].Free;
    SetLength(FList, 0);
    Changed;
  end;
end;

procedure TCRTablesInfo.Changed;
begin
  TableNameChanged;
  TableAliasChanged;
end;

procedure TCRTablesInfo.TableNameChanged;
var
  i: Integer;
begin
  if FUpdateCount = 0 then begin
    FTableNameList.Clear;
    for i := 0 to Count - 1 do
      FTableNameList.AddObject(FList[i].TableName, FList[i]);
    FTableNameList.Sort;
  end;
end;

procedure TCRTablesInfo.TableAliasChanged;
var
  i: Integer;
begin
  if FUpdateCount = 0 then begin
    FTableAliasList.Clear;
    for i := 0 to Count - 1 do
      FTableAliasList.AddObject(FList[i].TableAlias, FList[i]);
    FTableAliasList.Sort;
  end;
end;

function TCRTablesInfo.GetCount: Integer;
begin
  Result := Length(FList);
end;

procedure TCRTablesInfo.SetCaseSensitive(Value: boolean);
begin
  FCaseSensitive := Value;
{$IFDEF VER6P}
  FTableNameList.CaseSensitive := Value;
  FTableAliasList.CaseSensitive := Value;
{$ENDIF}
end;

procedure TCRTablesInfo.BeginUpdate;
begin
  Inc(FUpdateCount);
end;

procedure TCRTablesInfo.EndUpdate;
begin
  Dec(FUpdateCount);
  Changed;
end;

function TCRTablesInfo.FindByName(TableName: _string): TCRTableInfo;
var
  Index: integer;
begin
  Index := IndexByName(TableName);
  if Index = -1 then
    Result := nil
  else
    Result := FList[Index];
end;

function TCRTablesInfo.IndexOf(TableInfo: TCRTableInfo): Integer;
begin
  Result := 0;
  while (Result < Count) and (FList[Result] <> TableInfo) do
    Inc(Result);
  if Result = Count then
    Result := -1;
end;

function TCRTablesInfo.IndexByName(TableName: _string): Integer;
var
  i: integer;
begin
  if FUpdateCount = 0 then begin
    Result := FTableNameList.IndexOf(TableName);
    if Result <> -1 then
      Result := TCRTableInfo(FTableNameList.Objects[Result]).Index;
  end
  else
  begin
    Result := -1;
    for i := 0 to Count - 1 do
      if FCaseSensitive and (FList[i].TableName = TableName) or
        not FCaseSensitive and _SameText(FList[i].TableName, TableName)
      then begin
        Result := i;
        Break;
      end;
  end;
end;

function TCRTablesInfo.IndexByAlias(TableAlias: _string): Integer;
var
  i: integer;
begin
  if FUpdateCount = 0 then begin
    Result := FTableAliasList.IndexOf(TableAlias);
    if Result <> -1 then
      Result := TCRTableInfo(FTableAliasList.Objects[Result]).Index;
  end
  else
  begin
    Result := -1;
    for i := 0 to Count - 1 do
      if FCaseSensitive and (FList[i].TableAlias = TableAlias) or
        not FCaseSensitive and _SameText(FList[i].TableAlias, TableAlias)
      then begin
        Result := i;
        Break;
      end;
  end;
end;

function TCRTablesInfo.GetItem(Index: Integer): TCRTableInfo;
begin
  if (Index < 0) or (Index >= Count) then
    raise Exception.CreateFmt(SListIndexError, [Index]);

  Result := FList[Index];
end;

procedure TCRTablesInfo.SetItem(Index: Integer; const Value: TCRTableInfo);
begin
  if (Index >= 0) and (Index < Count) then
    if Value <> FList[Index] then
      FList[Index] := Value;
end;

{ TSQLInfo }

constructor TSQLInfo.Create(ParserClass: TSQLParserClass);
begin
  inherited Create;
  FParserClass := ParserClass;
end;

function TSQLInfo.LeftQuote: _char;
begin
  Result := _char('"');
end;

function TSQLInfo.RightQuote: _char;
begin
  Result := _char('"');
end;

function TSQLInfo.IdentCase: TIdentCase;
begin
  Result := icMixed;
end;

function TSQLInfo.ParamQuoteAllowed: boolean;
begin
  Result := true;
end;

function TSQLInfo.ProcedureOverloadSeparator: _char;
begin
  Result := _char(':');
end;

function TSQLInfo.IsQuoted(const Value: _string): boolean;
var
  l: integer;
begin
  l := Length(Value);
  if (l <= 1) then
    Result := False
  else
    Result :=  (Value[1] = LeftQuote) and (Value[l] = RightQuote);
end;

function TSQLInfo.Quote(const Value: _string): _string;
begin
  Result := Quote(Value, LeftQuote, RightQuote);
end;

function TSQLInfo.Quote(const Value: _string; const LeftQ: _char; const RightQ: _char): _string;
begin
  if not IsQuoted(Value) then
    Result := LeftQ + Value + RightQ
  else
    Result := Value;
end;

function TSQLInfo.UnQuote(const Value: _string): _string;
begin
  if IsQuoted(Value) then
    Result := Copy(Value, 2, length(Value) - 2)
  else
    Result := Value;
end;

function TSQLInfo.QuotesNeeded(const Value: _string): boolean;
var
  i: integer;
  IdCase: TIdentCase;
begin
  Result := False;
  if Value = '' then
    exit;

  IdCase := IdentCase;

  Result := FirstCharQuotesNeed(Value[1], IdCase);

  i := 2;
  while not Result and (i <= Length(Value)) do begin
    Result := NextCharQuotesNeed(Value[i], IdCase);
    Inc(i);  
  end;
end;

function TSQLInfo.QuoteIfNeed(const Value: _string): _string;
begin
  if QuotesNeeded(Value) then
    Result := Quote(Value)
  else
    Result := Value;
end;

function TSQLInfo.NormalizeName(const Value: _string; QuoteNames: boolean = False; UnQuoteNames: boolean = False): _string;
begin
  Result := NormalizeName(Value, LeftQuote, RightQuote, QuoteNames, UnQuoteNames);
end;

function TSQLInfo.NormalizeName(const Value: _string; const LeftQ: _char; const RightQ: _char; QuoteNames: boolean = False; UnQuoteNames: boolean = False): _string;
var
  i: integer;
begin
  if Value = '' then begin
    Result := '';
    Exit;
  end;

  if Value[1] = LeftQ then begin
    i := Pos(RightQ + '.', Value);
    if i > 0 then
      Inc(i);
  end
  else begin
    i := Pos('.', Value);
  end;

  if i <> 0 then
    Result := NormalizeName(Copy(Value, 1, i - 1), LeftQ, RightQ, QuoteNames, UnQuoteNames) + '.' +
      NormalizeName(Copy(Value, i + 1, Length(Value) - i), LeftQ, RightQ, QuoteNames, UnQuoteNames)
  else begin
    Result := Value;
    if not QuoteNames then begin
      case IdentCase of
        icUpper:
          if not IsQuoted(Result) then
            Result := _UpperCase(Result);
        icLower:
          if not IsQuoted(Result) then
            Result := _LowerCase(Result);
      end;
    end;

    if not UnQuoteNames and (QuoteNames or QuotesNeeded(Result)) then
      Result := Quote(Result, LeftQ, RightQ)
    else
      Result := UnQuote(Result);
  end;
end;

function TSQLInfo.ToStringConst(const Value: _string): _string;
begin
  Result := '''' + StringReplace(Value, '''', '''''', [rfReplaceAll]) + '''';
end;

procedure TSQLInfo.SplitObjectName(const Name: _string; var Info: TExtTableInfo);
var
  Str: _string;
  i, FirstIdx, Len, ParamIdx: integer;
  InQuote: boolean;
  Lq, Rq: _char;
begin
  Info.Table := '';
  Info.Schema := '';
  Info.Catalog := '';
  Info.DBLink := '';

  Len := Length(Name);
  FirstIdx := Len;
  ParamIdx := 1;
  InQuote := False;
  Lq := LeftQuote;
  Rq := RightQuote;

  for i := Len downto 0 do begin
    if i > 0 then begin
      if not InQuote then begin
        if Name[i] = Rq then
          InQuote := True;
      end
      else
        if Name[i] = Lq then
          InQuote := False;
    end;

    if (i = 0) or (not InQuote and (Name[i] = '.')) then begin
      Str := Copy(Name, i + 1, FirstIdx - i);
      case ParamIdx of
        1: Info.Table := Str;
        2: Info.Schema := Str;
        3: Info.Catalog := Str;
      end;
      Inc(ParamIdx);
      FirstIdx := i - 1;
    end;
  end;
end;

procedure TSQLInfo.ParseTablesInfo(const SQL: _string; TablesInfo: TCRTablesInfo);

  // find close bracket and skip all inside
  procedure ToEndBracket(Parser: TSQLParser; var CodeLexem: integer; var StLex: _string);
  var
    BracketCount: integer;
  begin
    BracketCount := 1;
    repeat
      CodeLexem := Parser.GetNext(StLex);

      if StLex = '(' then
        Inc(BracketCount)
      else
      if StLex = ')' then
        Dec(BracketCount);
    until (BracketCount = 0) or (CodeLexem = lcEnd);
  end;

  procedure ParseTableName(Parser: TSQLParser; var CodeLexem: integer; var StLex: _string; InBrackets: boolean);
  var
    Name, Alias: _string;
    TableInfo: TCRTableInfo;
  begin
    repeat
      // exit on WHERE and other clause lexems
      if not InBrackets and Parser.IsClauseLexem(CodeLexem) then
        exit
      // bypass end bracket
      else if StLex = ')' then
        exit
      // bypass join
      else if CodeLexem in [lxJOIN, lxINNER, lxLEFT, lxRIGHT, lxFULL, lxOUTER] then
        CodeLexem := Parser.GetNext(StLex)
      // bypass comma
      else if StLex = ',' then
        CodeLexem := Parser.GetNext(StLex)
      //parse table name
      else begin
        // parse brackets recursive
        if StLex = '(' then begin
          CodeLexem := Parser.GetNext(StLex);

          if CodeLexem <> lxSELECT then
            ParseTableName(Parser, CodeLexem, StLex, True)
          else // skip subquery
            ToEndBracket(Parser, CodeLexem, StLex);

          if CodeLexem = lcEnd then
            exit;

          // skip end bracket  
          if (StLex = ')') then
            CodeLexem := Parser.GetNext(StLex);
        end
        else begin
          Name := '';
          // PostgreSQL can containt ONLY lexeme before table name, skip it
          if CodeLexem = lxONLY then
            CodeLexem := Parser.GetNext(StLex);

          while (CodeLexem = lcIdent) or (CodeLexem >= lxSQLFirst) or (StLex = '.') do begin
              Name := Name + StLex;
              if StLex <> '.' then begin
                CodeLexem := Parser.GetNext(StLex);
                if StLex = '.' then
                  Name := Name + StLex
                else
                  break;
              end;
              CodeLexem := Parser.GetNext(StLex);
          end;

          if Name <> '' then begin
            if CodeLexem = lxAS then
              CodeLexem := Parser.GetNext(StLex);

            if (CodeLexem = lcIdent) or (CodeLexem >= lxSQLFirst) and
              (CodeLexem <> lxJOIN) and (CodeLexem <> lxINNER) and
              (CodeLexem <> lxLEFT) and (CodeLexem <> lxRIGHT) and
              (CodeLexem <> lxFULL) and (CodeLexem <> lxON) and
              not Parser.IsClauseLexem(CodeLexem)
            then begin
              Alias := StLex;
              CodeLexem := Parser.GetNext(StLex);
            end
            else
              Alias := '';

            if StLex <> '(' then begin       // skip stored functions
              TableInfo := TablesInfo.Add;
              TableInfo.TableName := NormalizeName(Name);
              TableInfo.TableAlias := NormalizeName(Alias);
            end;
          end;
        end;

        if CodeLexem = lcEnd then
          exit;

        // bypass subqueries, function calls, ON clause of join
        repeat
          if StLex = '(' then begin
            ToEndBracket(Parser, CodeLexem, StLex);
            if CodeLexem <> lcEnd then
              CodeLexem := Parser.GetNext(StLex);
          end;

          if CodeLexem in [lxJOIN, lxINNER, lxLEFT, lxRIGHT, lxFULL] then begin
            CodeLexem := Parser.GetNext(StLex);
            break;
          end
          else if StLex = ',' then begin
            CodeLexem := Parser.GetNext(StLex);
            break;
          end  
          else if StLex = ')' then
            exit
          else if not InBrackets and Parser.IsClauseLexem(CodeLexem) then
            exit;
          
          CodeLexem := Parser.GetNext(StLex);
        until (CodeLexem = lcEnd);
      end;
    until CodeLexem = lcEnd;
  end;

var
  Parser: TSQLParser;
  StLex: _string;
  CodeLexem: integer;
begin
  TablesInfo.Clear;

  Assert(FParserClass <> nil);
  Parser := FParserClass.Create(SQL);
  TablesInfo.BeginUpdate;
  Parser.OmitBlank := True;
  Parser.OmitComment := True;
  try
    if Parser.ToLexem(lxSELECT) <> lcEnd then begin
      CodeLexem := Parser.ToLexem(lxFROM, True);
      if CodeLexem <> lcEnd then begin
        CodeLexem := Parser.GetNext(StLex);
        ParseTableName(Parser, CodeLexem, StLex, False);
      end;
    end;
  finally
    TablesInfo.EndUpdate;
    Parser.Free;
  end;
end;

procedure TSQLInfo.ParseExtColumnName(Parser: TSQLParser; var Code: integer; var Str: _string);
begin
end;

function TSQLInfo.FirstCharQuotesNeed(Ch: _Char;IdCase: TIdentCase): boolean;
begin
  case IdCase of
    icUpper:
      case Ch of
        'A'..'Z': Result := False;
      else
        Result := True;
      end;
    icLower:
      case Ch of
        'a'..'z': Result := False;
      else
        Result := True;
      end;
  else
    case Ch of
      'a'..'z', 'A'..'Z': Result := False;
    else
      Result := True;
    end;
  end;
end;

function TSQLInfo.NextCharQuotesNeed(Ch: _Char; IdCase: TIdentCase): boolean;
begin
  case IdCase of
    icUpper:
      case Ch of
        'A'..'Z', '_', '0'..'9', '$': Result := False;
      else
        Result := True;
      end;
    icLower:
      case Ch of
        'a'..'z', '_', '0'..'9', '$': Result := False;
      else
        Result := True;
      end;
  else
    case Ch of
      'a'..'z', 'A'..'Z', '_', '0'..'9', '$': Result := False;
    else
      Result := True;
    end;
  end;
end;

procedure TSQLInfo.ParseColumnsInfo(const SQL: _string; ColumnsInfo: TColumnsInfo);
var
  Info: TColumnInfo;
  Parser: TSQLParser;
  Str, PrevStr: _string;
  PrevCode, Code: integer;
  Name, Alias, Table, Expr: _string;
  IsExpr: boolean;
  BracketCount: integer;
begin
  Assert(FParserClass <> nil);
  Parser := FParserClass.Create(SQL);
  try
    Parser.OmitComment := True;
    if Parser.ToLexem(lxSELECT) = lxSELECT then begin
      BracketCount := 0;
      while True do begin
        Code := Parser.GetNext(Str);

        if Parser.IsSelectModifier(Code) then
          Continue;

        IsExpr := False;
        Name := '';
        Table := '';
        Alias := '';
        Expr := '';

        if (Code = lcIdent) or (Code >= lxSQLFirst) and (Code <> lxFROM)
          or (Str = '*')
        then begin
          Name := Str;
          Code := Parser.GetNext(Str);
          if Str = '.' then begin
            Code := Parser.GetNext(Str);
            if (Code = lcIdent) or (Code >= lxSQLFirst) or (Str = '*') then begin
              Table := Name;
              Name := Str;
              Code := Parser.GetNext(Str);
              if Str = '.' then begin
                Code := Parser.GetNext(Str);
                if (Code = lcIdent) or (Code >= lxSQLFirst) or (Str = '*') then begin
                  Table := Table + '.' + Name;
                  Name := Str;
                  Code := Parser.GetNext(Str);
                  if Str = '.' then begin
                    Code := Parser.GetNext(Str);
                    if (Code = lcIdent) or (Code >= lxSQLFirst) or (Str = '*') then begin
                      Table := Table + '.' + Name;
                      Name := Str;
                      Code := Parser.GetNext(Str);
                    end;
                  end;
                end;
              end;
            end;
          end;

          ParseExtColumnName(Parser, Code, Str);

          if Code = lxAS then
            Parser.GetNext(Str);

          if (Code = lcIdent) or (Code >= lxSQLFirst) and (Code <> lxFROM) then begin
            Alias := Str;
            Code := Parser.GetNext(Str);
          end;
        end;

        PrevCode := lcEnd;
        while True do begin
          if (Code = lcEnd) or
            (((Str = ',') or (Code = lxFROM)) and (BracketCount = 0))
          then
            Break;

          if not IsExpr then begin
            if Table = '' then
              Expr := Name
            else
              Expr := Table + '.' + Name;
            Alias := '';
            IsExpr := True;
          end
          else
          if Alias <> '' then begin
            Expr := Expr + Alias;
            Alias := '';
          end;

          if Str = '(' then
            Inc(BracketCount)
          else
          if Str = ')' then
            Dec(BracketCount);

          if BracketCount = 0 then begin
            if Code = lxAS then begin
              Code := Parser.GetNext(Str);
              continue;
            end;

            if ((Code = lcIdent) or (Code >= lxSQLFirst)) and
              ((PrevCode = lcIdent) or (PrevCode = lcNumber) or (PrevCode = lcString)
              or (PrevCode >= lxSQLFirst) or (PrevStr = ')'))
            then
              Alias := Str
            else
              Expr := Expr + Str;
          end;

          PrevCode := Code;
          PrevStr := Str;
          Code := Parser.GetNext(Str);
        end;

        if not IsExpr and (Table = '') and Parser.IsFunctionOrConst(Name) then begin
          IsExpr := True;
          Expr := Name;
        end;

        Info := TColumnInfo.Create;
        if not IsExpr then begin
          Info.Name := NormalizeName(Name, False, True);
          Info.Table := NormalizeName(Table, False, True);
        end
        else
          Info.Expr := Expr;
        Info.TableIndex := -1;
        Info.Alias := NormalizeName(Alias, False, True);
        ColumnsInfo.Add(Info);

        if (Code = lcEnd) or (Code = lxFROM) then
          Break;
      end;
    end;
  finally
    Parser.Free;
  end;
end;

function TSQLInfo.NamesFromList(List: _TStrings): _string;
var
  i: integer;
begin
  for i := 0 to List.Count - 1 do begin
    if i > 0 then
      Result := Result + ';';
    Result := Result + List[i];
  end;
end;

procedure TSQLInfo.NamesToList(Value: _string; List: _TStrings);
var
  St: _string;
  i: integer;
begin
  Value := Trim(Value);
  List.Clear;

  St := '';
  for i := 1 to Length(Value) do
    if (Value[i] = ',') or (Value[i] = ';') then begin
      St := Trim(St);
      if St <> '' then
        List.Add(St);
      St := '';
    end
    else
      St := St + Value[i];

  St := Trim(St);
  if St <> '' then
    List.Add(St);
end;

{ TCRFieldDesc }

function TCRFieldDesc.ActualNameQuoted(SQLInfo: TSQLInfo; QuoteNames: boolean): _string;
begin
  if FActualNameQuoted[QuoteNames] <> '' then
    Result := FActualNameQuoted[QuoteNames]
  else
  begin
    if QuoteNames or SQLInfo.QuotesNeeded(ActualName) then
      Result := SQLInfo.Quote(ActualName)
    else
      Result := ActualName;
    FActualNameQuoted[QuoteNames] := Result;
  end;
end;

function TCRFieldDesc.IsNational: boolean;
begin
  Result := False;
end;

{ $ENDIF}

{ TCRRecordSet }

constructor TCRRecordSet.Create;
begin
  inherited;

  FAfterExecFetch := nil;
  FAfterFetchAll := nil;

  CreateCommand;
  FTablesInfo := TCRTablesInfo.Create(GetTableInfoClass);

  FLongStrings := True;
  FFlatBuffers := True;
end;

destructor TCRRecordSet.Destroy;
begin
  FreeCommand;
  
{ $IFNDEF LITE}
  FTablesInfo.Free;
{ $ENDIF}

  inherited;
end;

{procedure TCRRecordSet.CreateCommand;
begin
  SetCommand(nil);
end;}

procedure TCRRecordSet.FreeCommand;
begin
  FCommand.Free;
  SetCommand(nil);
end;

procedure TCRRecordSet.SetCommand(Value: TCRCommand);
begin
  FCommand := Value;
end;

function TCRRecordSet.CanFetchBack: boolean; 
begin
  Result := False;
end;
    
{ Open/Close }

function TCRRecordSet.NeedInitFieldsOnPrepare: boolean;
begin
  Result := CommandType = ctCursor;
end;

procedure TCRRecordSet.InternalPrepare;
begin
  FCommand.Prepare;
end;

procedure TCRRecordSet.InternalUnPrepare;
begin
  FCommand.UnPrepare;
end;

procedure TCRRecordSet.InternalOpen(DisableInitFields: boolean = False);
begin
  try
    FNoData := False;

    inherited;

    FEOF := False;

    ExecFetch(DisableInitFields);
  except
    if not Prepared then
      InternalUnprepare;

    if FCommand.GetCursorState = csExecuted then
      FCommand.SetCursorState(csInactive);

    raise;
  end
end;

procedure TCRRecordSet.InternalClose;
begin
  FNoData := False;
end;

procedure TCRRecordSet.Prepare;
begin
  if not Prepared then begin

    inherited;

    if NeedInitFieldsOnPrepare then
      try
        InitFields;
        Prepared := True;
      except
        Prepared := False;
        InternalUnPrepare;
        raise;
      end;
  end;
end;

procedure TCRRecordSet.Unprepare;
begin
  try
    inherited;
  finally
  {$IFNDEF LITE}
    FTablesInfo.Clear;
  {$ENDIF}
  end;
end;

procedure TCRRecordSet.Disconnect;
begin
  InternalUnprepare; //Remove all links to DB but not close Data
  Prepared := False; //Set recordset unprepared in case of disconnect mode and
                     //explicit disconnection this will prevent from wrong Prepare state
end;

procedure TCRRecordSet.Open;
begin
  inherited;
end;

procedure TCRRecordSet.Close;
begin
  inherited;
end;

procedure TCRRecordSet.ExecCommand;
begin
  FCommand.Execute;
  FWaitForFetchBreak := False;
end;

function TCRRecordSet.NeedInitFieldsOnFetch: boolean;
begin
  Result := False;
end;

procedure TCRRecordSet.ExecFetch(DisableInitFields: boolean);
begin
  ExecCommand;

  if not DisableInitFields and
    (not Prepared or (Fields.Count = 0) or NeedInitFieldsOnFetch)
  then
    InitFields;

  if FFetchAll then
    FetchAll
  else
    Fetch;
end;

procedure TCRRecordSet.BreakFetch; // Breaks fetch. Can be called from other thread or in non-blocking mode
begin
  FWaitForFetchBreak := True;
end;

procedure TCRRecordSet.DecryptBuffer(Item: PItemHeader);
begin
end;

procedure TCRRecordSet.InitFetchedBlock(Block: PBlockHeader; RowsObtained: Integer; FetchBack: boolean);
begin
  if Filtered and not FFetchAll then begin
    if FetchBack then
      InitFetchedItems(PtrOffset(Block, sizeof(TBlockHeader) + (RowsObtained - 1) * (sizeof(TItemHeader) + RecordSize)), FNoData, FetchBack)
    else
      InitFetchedItems(PtrOffset(Block, sizeof(TBlockHeader)), FNoData, FetchBack);
  end
  else
    if not (FetchBack or FNoData) then
      Inc(FRecordCount, RowsObtained);
end;

procedure TCRRecordSet.CreateBlockStruct(Block: PBlockHeader; RowsObtained: Integer; FetchBack: boolean = False);
var
  Item: PItemHeader;
  ItemSize: integer;
  i: integer;
  ui: integer;
begin
  // Create Items
  Item := PtrOffset(Block, sizeof(TBlockHeader));
  ItemSize := RecordSize + sizeof(TItemHeader);

  for i := 1 to RowsObtained do begin
    Item.Block := Block;
    Item.Flag := flUsed;
    Item.Rollback := nil;
    Item.Status := isUnmodified;
    Item.UpdateResult := urNone;
    Item.FilterResult := fsNotChecked;
    Item := PtrOffset(Item, ItemSize);
  end;

  if FetchBack then begin
    Item := PtrOffset(Block, sizeof(TBlockHeader) + (RowsObtained - 1) * ItemSize);
    if IntPtr(LastItem) = nil then
      LastItem := Item;
    for i := RowsObtained downto 1 do begin
      Item.Prev := nil;
      Item.Next := FirstItem;

      if IntPtr(FirstItem) <> nil then begin
        FirstItem.Prev := Item;
        Item.Order := FirstItem.Order - 1;
      end
      else
        Item.Order := FRecordCount;

      FirstItem := Item;
      DecryptBuffer(Item);
      Item := PtrOffset(Item, - ItemSize);
    end;
  end
  else begin
    Item := PtrOffset(Block, sizeof(TBlockHeader));
    if IntPtr(FirstItem) = nil then
      FirstItem := Item;
    for i := 1 to RowsObtained do begin
      Item.Prev := LastItem;
      Item.Next := nil;

      if IntPtr(LastItem) <> nil then begin
        LastItem.Next := Item;
        Item.Order := LastItem.Order + 1;
      end
      else
        Item.Order := 1;

      LastItem := Item;
      DecryptBuffer(Item);
      Item := PtrOffset(Item, ItemSize);
    end;
  end;

  FirstItem.Prev := nil;
  LastItem.Next := nil;

  UpdateCachedBuffer(PtrOffset(Block, sizeof(TBlockHeader)), PtrOffset(Block, sizeof(TBlockHeader) + (RowsObtained - 1) * ItemSize));
  InitFetchedBlock(Block, RowsObtained, FetchBack);

  // Free items
  ui := Block.ItemCount - RowsObtained;
  if ui > 0 then begin
    if not FetchBack then
      FNoData := True;

    Item := PtrOffset(Block, sizeof(TBlockHeader) + RowsObtained * ItemSize);
    for i := 1 to ui do begin
      Item.Prev := nil;
      Item.Next := BlockMan.FirstFree;
      Item.Block := Block;
      Item.Flag := flFree;
      Item.Rollback := nil;
      Item.Status := isUnmodified;
      Item.UpdateResult := urNone;

      if IntPtr(BlockMan.FirstFree) <> nil then
        BlockMan.FirstFree.Prev := Item;
      BlockMan.FirstFree := Item;

      // Free complex fields
      if HasComplexFields then
        FreeComplexFields(PtrOffset(Item, SizeOf(TItemHeader)), True);

      Item := PtrOffset(Item, ItemSize);
   end;
  end;

  Block.UsedItems := RowsObtained;
end;

procedure TCRRecordSet.DoBeforeFetch(out Cancel: boolean);
begin
  Cancel := FWaitForFetchBreak;

  if Assigned(FOnBeforeFetch) then
    FOnBeforeFetch(Cancel);

  if Cancel then begin
    // reset cursor state for FetchAll
    if (FCommand.GetCursorState = csFetchingAll) then
      FCommand.SetCursorState(csFetching);
  end;
end;

procedure TCRRecordSet.DoAfterFetch;
begin
  if Assigned(FOnAfterFetch) then
    FOnAfterFetch;
end;

procedure TCRRecordSet.SortItems;
begin
  if IndexFields.Count = 0 then
    Exit;
  FetchAll;

  inherited SortItems;
end;

{ Fields }
function TCRRecordSet.GetFieldDescType: TFieldDescClass;
begin
  Result := TCRFieldDesc;
end;

function TCRRecordSet.NeedConvertEOL: boolean;
begin
  if (FCommand = nil) or (FCommand.FConnection = nil) then
    Result := False
  else
    Result := FCommand.FConnection.FConvertEOL;
end;

{ Records }

procedure TCRRecordSet.GetNextRecord(RecBuf: IntPtr);
var
  Found: boolean;
  Item: PItemHeader;
begin
  if not EOF then begin
    if IntPtr(FirstItem) = nil then begin
      if not Fetch then begin
        FEOF := True;
        Exit;
      end
      else
        CurrentItem := FirstItem;
    end
    else
      if IntPtr(CurrentItem) = nil then
        CurrentItem := FirstItem
      else
        CurrentItem := CurrentItem.Next;

    repeat
      if IntPtr(CurrentItem) = nil then begin
        Item := LastItem;

        if not Fetch then begin
          FEOF := True;
          Exit;
        end
        else begin
          if FUniDirectional or CanFetchBack then begin
            FirstItem.Prev := nil;  // remove cycle link
            LastItem.Next := nil;
          end;
          if (IntPtr(Item.Next) = nil) or FUniDirectional then
            CurrentItem := FirstItem
          else
            CurrentItem := Item.Next;
        end
      end;

      Found := not OmitRecord(CurrentItem);
      if not Found then
        CurrentItem := CurrentItem.Next;
    until Found;

    FBOF := False;
    FEOF := False;
    if RecBuf <> nil then
      GetRecord(RecBuf);
  end;
end;

procedure TCRRecordSet.GetPriorRecord(RecBuf: IntPtr);
var
  Found: boolean;
  Item: PItemHeader;
begin
  if FUniDirectional then begin
    FBOF := True;
    CurrentItem := nil;
  end
  else
    if not CanFetchBack then
      inherited
    else
      if not BOF then begin
        if IntPtr(LastItem) = nil then begin
          if not Fetch(True){FetchBack!} then begin
            FBOF := True;
            Exit;
          end
          else
            CurrentItem := LastItem;
        end
        else
          if IntPtr(CurrentItem) = nil then
            CurrentItem := LastItem
          else
            CurrentItem := CurrentItem.Prev;

        repeat
          if IntPtr(CurrentItem) = nil then begin
            Item := FirstItem;

            if not Fetch(True){FetchBack!} then begin
              FBOF := True;
              Exit;
            end
            else begin
              FirstItem.Prev := nil;  // remove cycle link
              LastItem.Next := nil;
              if IntPtr(Item.Prev) = nil then
                CurrentItem := LastItem
              else
                CurrentItem := Item.Prev;
            end;
          end;

          Found := not OmitRecord(CurrentItem);
          if not Found then
            CurrentItem := CurrentItem.Prev;
        until Found;

        FBOF := False;
        FEOF := False;
        if RecBuf <> nil then
          GetRecord(RecBuf);
      end;
end;

{ Fetch }

procedure TCRRecordSet.FetchAll;
begin
  while Fetch do;
end;

function TCRRecordSet.CanDisconnect: boolean;
var
  CursorState: TCursorState;
begin
  Assert(FCommand <> nil);
  CursorState := FCommand.GetCursorState;
  Result := (CursorState = csInactive) or (CursorState = csFetched);
end;

function TCRRecordSet.RowsReturn: boolean;
begin
  Result := (CommandType = ctCursor);
end;

function TCRRecordSet.GetCommand: TCRCommand;
begin
  Result := FCommand;
end;

procedure TCRRecordSet.SetConnection(Value: TCRConnection);
begin
  FCommand.SetConnection(Value);
end;

procedure TCRRecordSet.SetTransaction(Value: TCRTransaction);
begin
  FCommand.SetTransaction(Value);
end;

procedure TCRRecordSet.SetSQL(const Value: _string);
begin
  FCommand.SetSQL(Value);
end;

function TCRRecordSet.SetProp(Prop: integer; const Value: variant): boolean;
begin
  Result := True;
  case Prop of
    prUniDirectional: begin
      FUniDirectional := Value;
      if FUniDirectional then
        FFetchAll := False;
    end;
    prFetchRows:
      FFetchRows := Value;
    prFetchAll:
      if not FLockFetchAll then
        FFetchAll := Value and not FUniDirectional;
    prLockFetchAll:
      FLockFetchAll := Value;
    prLongStrings:
      FLongStrings := Value;
    prFlatBuffers:
      FFlatBuffers := Value;
    prIndexFieldNames:
      SetIndexFieldNames(Value);
  {$IFDEF HAVE_COMPRESS}
    prCompressBlobMode:
      FCommand.FCompressBlob := TCompressBlobMode(Integer(Value));
  {$ENDIF}
    prReadOnly:;
    prExtendedFieldsInfo:
      FExtendedFieldsInfo := Value;
    prDefaultValues:
      FDefaultValues := Value;
    prFieldsOrigin:
      FFieldsOrigin := Value;
    prRoAfterUpdate:;
  else
    Assert(False, IntToStr(Prop));
    Result := False;
  end;
end;

procedure TCRRecordSet.SetComponent(Value: TObject);
begin
  FCommand.Component := Value;
end;

function TCRRecordSet.GetComponent: TObject;
begin
  Result := FCommand.Component;
end;

function TCRRecordSet.GetProp(Prop: integer; var Value: variant): boolean;
begin
  Result := True;
  case Prop of
    prFetchAll:
      Value := FFetchAll;
    prExtendedFieldsInfo:
      Value := FExtendedFieldsInfo;
    prDefaultValues:
      Value := FDefaultValues;
    prFieldsOrigin:
      Value := FFieldsOrigin;
  else
    Assert(False, IntToStr(Prop));
    Result := False;
  end;
end;

function TCRRecordSet.IsCaseSensitive: boolean;
begin
  Result := True;
end;

procedure TCRRecordSet.FilterUpdated;
var
  NotFetched: boolean;
begin
  NotFetched := (FCommand.GetCursorState < csFetched) and (FCommand.GetCursorState <> csInactive);
  inherited FilterUpdated;
  FEOF := FEOF and not NotFetched;
end;

function TCRRecordSet.GetTableInfoClass: TTableInfoClass;
begin
  Result := FCommand.GetTableInfoClass;
end;

procedure TCRRecordSet.SetSortDefaults(SortColumn: TSortColumn);
begin
  inherited;

  if FCommand.FConnection <> nil then
    SortColumn.SortType := FCommand.FConnection.FDefaultSortType;
end;

{ TParamDesc }

constructor TParamDesc.Create;
begin
  inherited;

  FDataType := dtUnknown;
  FIsBound := True;
end;

destructor TParamDesc.Destroy;
begin
end;

procedure TParamDesc.Clear;
begin
  FDataType := dtUnknown;
end;

function TParamDesc.GetName: _string;
begin
  Result := FName;
end;

procedure TParamDesc.SetName(Value: _string);
begin
  FName := Value;
end;

function TParamDesc.GetDataType: word;
begin
  Result := FDataType;
end;

procedure TParamDesc.SetDataType(Value: word);
begin
  FDataType := Value;
end;

function TParamDesc.GetSubDataType: word;
begin
  Result := FSubDataType;
end;

procedure TParamDesc.SetSubDataType(Value: word);
begin
  FSubDataType := Value;
end;

function TParamDesc.GetParamType: TParamDirection;
begin
  Result := FParamType;
end;

procedure TParamDesc.SetParamType(Value: TParamDirection);
begin
  FParamType := Value;
end;

function TParamDesc.GetSize: integer;
begin
  Result := FSize;
end;

procedure TParamDesc.SetSize(Value: integer);
begin
  FSize := Value;
end;

function TParamDesc.GetValue: variant;
begin
  Result := FData;
end;

procedure TParamDesc.SetValue(const Value: variant);
begin
{$IFNDEF VER6P}
  if TVarData(Value).VType = varByRef{$IFDEF FPC} or varVariant{$ENDIF} then
    SetObject(TVarData(Value).VPointer)
  else
{$ENDIF}
    FData := Value;
  FIsNull := VarIsEmpty(FData) or VarIsNull(FData);
end;

function TParamDesc.GetNull: boolean;
begin
  Result := FIsNull;
end;

function TParamDesc.GetObject: TSharedObject;
begin
  if VarIsEmpty(FData) or VarIsNull(FData) then
    Result := nil
  else
  begin
  {$IFDEF CLR}
    Assert(FData is TSharedObject);
    Result := TSharedObject(FData);
  {$ELSE}
    Assert(TVarData(FData).VType = varByRef{$IFDEF FPC} or varVariant{$ENDIF});
    Result := TVarData(FData).VPointer;
  {$ENDIF}
  end;
end;

procedure TParamDesc.SetObject(Value: TSharedObject);
begin
{$IFDEF CLR}
  FData := Variant(Value);
{$ELSE}
  FData := Unassigned;
  TVarData(FData).VType := varByRef{$IFDEF FPC} or varVariant{$ENDIF};
  TVarData(FData).VPointer := Value;
{$ENDIF}
end;

procedure TParamDesc.SetNull(const Value: boolean);
begin
  FIsNull := Value;
{$IFDEF CLR}
  if not (FData is TSharedObject) then
{$ELSE}
  if TVarData(FData).VType <> varByRef{$IFDEF FPC} or varVariant{$ENDIF} then
{$ENDIF}
    FData := Unassigned;
end;

function TParamDesc.GetIsBound: boolean;
begin
  Result := FIsBound;
end;

procedure TParamDesc.SetIsBound(Value: boolean);
begin
  FIsBound := Value;
end;

function TParamDesc.GetNational: boolean;
begin
  Result := FNational;
end;

procedure TParamDesc.SetNational(Value: boolean);
begin
  FNational := Value;
end;

procedure TParamDesc.SetConvertEOL(const Value: boolean);
begin
  FConvertEOL := Value;
end;

{ TParamDescs }

destructor TParamDescs.Destroy;
begin
  Clear;

  inherited;
end;

procedure TParamDescs.Clear;
var
  i:integer;
begin
  for i:= 0 to Count - 1 do
    if Items[i] <> nil then
      TParamDesc(Items[i]).Free;

  inherited Clear;
end;

function TParamDescs.FindParam(Name: _string): TParamDesc;
var
  i: integer;
begin
  Result := nil;

  for i := 0 to Count - 1 do
    if (Items[i] <> nil) then
      if _SameText(TParamDesc(Items[i]).FName, Name) then begin
        Result := Items[i];
        break;
      end;
end;

function TParamDescs.ParamByName(Name: _string): TParamDesc;
begin
  Result := FindParam(Name);

  if Result = nil then
    Assert(False);
    //raise Exception.Create(Format(SParamNotFound, [Name]));
end;

function TParamDescs.GetItems(Index: integer): TParamDesc;
begin
  Result := TParamDesc(inherited Items[Index]);
end;

{ TCRConnections }

function TCRConnections.GetItems(Index: integer): TCRConnection;
begin
  Result := TCRConnection(inherited Items[Index]);
end;

{ TCRTransaction }

constructor TCRTransaction.Create;
begin
  inherited;

  FConnections := TCRConnections.Create;
  FNativeTransaction := True;
end;

destructor TCRTransaction.Destroy;
begin
  // Transaction is closed by component layer.
  // Destructor of derived class can free resouces that are need to close transaction
  // before calling inherited.
  // CloseTransaction;

  FConnections.Free;

  inherited;
end;

procedure TCRTransaction.CheckInactive;
begin
  if GetInTransaction then
    raise Exception.Create(SInTransaction);
end;

procedure TCRTransaction.CheckActive;
begin
  if not GetInTransaction then
    raise Exception.Create(SNotInTransaction);
end;

function TCRTransaction.AddConnection(Connection: TCRConnection): boolean;
begin
  if FConnections.IndexOf(Connection) = -1 then begin
    FConnections.Add(Connection);
    Result := True;
  end
  else
    Result := False;
end;

function TCRTransaction.RemoveConnection(Connection: TCRConnection): boolean;
begin
  if FConnections.IndexOf(Connection) >= 0 then begin
    FConnections.Remove(Connection);
    Result := True;
  end
  else
    Result := False;
end;

function TCRTransaction.SetProp(Prop: integer; const Value: variant): boolean;
begin
  Result := True;
  case Prop of
    prIsolationLevel:
      FIsolationLevel := TCRIsolationLevel(Value);
    prTransactionReadOnly:
      FReadOnly := Value;
  else
    Assert(False, IntToStr(Prop));
    Result := False;
  end;
end;

function TCRTransaction.GetProp(Prop: integer; var Value: variant): boolean;
begin
  Result := True;
  case Prop of
    prIsolationLevel:
      Value := Variant(FIsolationLevel);
    prTransactionReadOnly:
      Value := FReadOnly;
  else
    Assert(False, IntToStr(Prop));
    Result := False;
  end;  
end;

procedure TCRTransaction.StartTransaction;
begin
  Assert(False);
end;

procedure TCRTransaction.Commit;
begin
  Assert(False);
end;

procedure TCRTransaction.Rollback;
begin
  Assert(False);
end;

procedure TCRTransaction.CommitRetaining;
begin
  raise Exception.Create(SOperationNotSupported);
end;

procedure TCRTransaction.RollbackRetaining;
begin
  raise Exception.Create(SOperationNotSupported);
end;

procedure TCRTransaction.Savepoint(const Name: _string);
begin
  raise Exception.Create(SOperationNotSupported);
end;

procedure TCRTransaction.ReleaseSavepoint(const Name: _string);
begin
  raise Exception.Create(SOperationNotSupported);
end;

procedure TCRTransaction.RollbackToSavepoint(const Name: _string);
begin
  raise Exception.Create(SOperationNotSupported);
end;

function TCRTransaction.GetInTransaction: boolean;
begin
  Result := FActive;
end;

function TCRTransaction.DetectInTransaction(CanActivate: boolean): boolean;
begin
  Result := GetInTransaction;
end;

procedure TCRTransaction.AssignConnect(Source: TCRTransaction);
begin
  FActive := Source.FActive;
  if FActive then
    FNativeTransaction := False;
end;

procedure TCRTransaction.Reset;
begin
  FActive := False;
end;

function TCRTransaction.CanRestoreAfterFailover: boolean;
begin
  Result := False;
end;

{ TMTSTransaction }

{$IFNDEF LITE}
{$IFDEF MSWINDOWS}

destructor TMTSTransaction.Destroy;
begin
  FMTSGC := nil;

  inherited;
end;

procedure TMTSTransaction.StartMTSTransaction;
var
  MTSIsolationLevel: integer;
  MTSFlags: integer;
  MTSTransaction: ICRTransaction;
  MTSOptions: ICRTransactionOptions;
{$IFDEF CLR}
  hGuid: IntPtr;
  aGuid: array of byte;
  i: integer;
const
  GuidSize = 16;
{$ENDIF}
begin
{$IFDEF FPC}
  raise Exception.Create(SFPCNotSupported);
{$ENDIF}

  InitMSDTC;

  if FMTSGC = nil then begin
  {$IFDEF CLR}
    hGuid := Marshal.AllocHGlobal(GuidSize);
    try
      aGuid := StringToGUID(IID_ITransactionDispenser).ToByteArray;
      for i := 0 to GuidSize - 1 do
        Marshal.WriteByte(hGuid, i, aGuid[i]);
      OleCheck(DtcGetTransactionManagerEx(nil, nil, hGuid, 0, nil, FMTSGC));
    finally
      Marshal.FreeHGlobal(hGuid);
    end;
  {$ELSE}
    OleCheck(DtcGetTransactionManagerEx(nil, nil, IID_ITransactionDispenser, 0, nil, FMTSGC));
  {$ENDIF}
  end;

  case FIsolationLevel of
    ilReadUnCommitted:
      MTSIsolationLevel := ISOLATIONLEVEL_READUNCOMMITTED;
    ilReadCommitted:
      MTSIsolationLevel := ISOLATIONLEVEL_READCOMMITTED;
    ilRepeatableRead:
      MTSIsolationLevel := ISOLATIONLEVEL_REPEATABLEREAD;
    ilIsolated:
      MTSIsolationLevel := ISOLATIONLEVEL_SERIALIZABLE;
  else
    raise Exception.Create(SIsolationLevelNotSupportedWithMTS);
  end;
  MTSFlags := 0;
  MTSOptions := nil; 

  FMTSGC.BeginTransaction(nil, MTSIsolationLevel, MTSFlags, MTSOptions, MTSTransaction);
  FMTSTrans := ICRTransactionSC(MTSTransaction);
end;

procedure TMTSTransaction.CompleteMTSTransaction(Commit: boolean);
var
  i: integer;
begin
  Assert(FMTSTrans <> nil);
  FActive := False;
  try
    if Commit then
      FMTSTrans.Commit(False, 0, 0)
    else
      FMTSTrans.Abort(nil, False, False);
  finally
    FMTSTrans := nil;
  end;

  for i := 0 to FConnections.Count - 1 do //needunenlist 
    UnEnlistLink(FConnections[i]);  
end;

procedure TMTSTransaction.EnlistLink(Connection: TCRConnection);
begin
//  Connection.FDefaultTransaction := Self; //upd зачем такое делать, не допустимо в новом даке
  Connection.Enlist(Self);
end;

procedure TMTSTransaction.UnEnlistLink(Connection: TCRConnection);
begin
  Connection.UnEnlist(Self);
//  Connection.FDefaultTransaction := nil; //upd зачем такое делать, не допустимо в новом даке
end;

function TMTSTransaction.AddConnection(Connection: TCRConnection): boolean;
begin
  Result := inherited AddConnection(Connection);
  if Result and FActive then
    EnlistLink(Connection);
end;

function TMTSTransaction.RemoveConnection(Connection: TCRConnection): boolean;
begin
  Result := inherited RemoveConnection(Connection);
  if Result and FActive then
    UnEnlistLink(Connection);
end;

procedure TMTSTransaction.StartTransaction;
var
  i: integer;
begin
  CheckInactive;

  for i := 0 to FConnections.Count - 1 do
    if not FConnections[i].GetConnected then
      raise Exception.Create(SConnectionInTransactionNotActive);

  StartMTSTransaction;

  FActive := True;
  try
    for i := 0 to FConnections.Count - 1 do
      EnlistLink(FConnections[i]);
  except
    Rollback;
    raise;
  end;
end;

procedure TMTSTransaction.Commit;
begin
  CheckActive;
  CompleteMTSTransaction(True);
end;

procedure TMTSTransaction.Rollback;
begin
  CheckActive;
  CompleteMTSTransaction(False);
end;

procedure TMTSTransaction.Savepoint(const Name: _string);
var
  i: integer;
begin
  for i := 0 to FConnections.Count - 1 do begin
    FConnections[i].FInternalTransaction.Savepoint(Name);
  end;
end;

procedure TMTSTransaction.ReleaseSavepoint(const Name: _string);
var
  i: integer;
begin
  for i := 0 to FConnections.Count - 1 do begin
    FConnections[i].FInternalTransaction.ReleaseSavepoint(Name);
  end;
end;

procedure TMTSTransaction.RollbackToSavepoint(const Name: _string);
var
  i: integer;
begin
  for i := 0 to FConnections.Count - 1 do begin
    FConnections[i].FInternalTransaction.RollbackToSavepoint(Name);
  end;
end;

{$ENDIF}
{$ENDIF}

{ TCRMetaData }

constructor TCRMetaData.Create;
begin
  inherited;

  FMemData := TMemData.Create;
  FMemData.AutoInitFields := False;
  FRecordSet := CreateRecordSet;
  FRecordSet.SetProp(prFetchAll, True);
  FRecordSet.SetProp(prFlatBuffers, False);

  FMemDataHelper := TDataHelper.Create(FMemData);
  FRecordSetHelper := TDataHelper.Create(FRecordSet);

  FOperator := '=';
end;

destructor TCRMetaData.Destroy;
begin
  FMemDataHelper.Free;
  FRecordSetHelper.Free;
  FRecordSet.Free;
  FMemData.Free;

  inherited;
end;

function TCRMetaData.GetMetaData(Connection: TCRConnection; Transaction: TCRTransaction;
  const MetaDataKind: _string; Restrictions: _TStrings): TData;
begin
  FRecordSet.SetConnection(Connection);
  FRecordSet.SetTransaction(Transaction);
  Result := InternalGetMetaData(_LowerCase(Trim(MetaDataKind)), Restrictions);
end;

function TCRMetaData.InternalGetMetaData(const MetaDataKind: _string; Restrictions: _TStrings): TData;
begin
  // kinds required for all products
  if (MetaDataKind = '') or (MetaDataKind = 'metadatakinds') then
    Result := GetMetaDataKinds
  else
  if MetaDataKind = 'restrictions' then
    Result := GetRestrictions(Restrictions)
  else
  if MetaDataKind = 'tables' then
    Result := GetTables(Restrictions)
  else
  if MetaDataKind = 'columns' then
    Result := GetColumns(Restrictions)
  else
  if MetaDataKind = 'procedures' then
    Result := GetProcedures(Restrictions)
  else
  if MetaDataKind = 'procedureparameters' then
    Result := GetProcedureParameters(Restrictions)
  else
  if MetaDataKind = 'indexes' then
    Result := GetIndexes(Restrictions)
  else
  if MetaDataKind = 'indexcolumns' then
    Result := GetIndexColumns(Restrictions)
  else
  if MetaDataKind = 'constraints' then
    Result := GetConstraints(Restrictions)

  // the following kinds are reqiured for DBXpress only
  else
  if MetaDataKind = 'databases' then
    Result := GetDatabases(Restrictions)
  else
  if MetaDataKind = 'datatypes' then
    Result := GetDatatypes(Restrictions)
  else
  if MetaDataKind = 'users' then
    Result := GetUsers(Restrictions)
  else
  if MetaDataKind = 'roles' then
    Result := GetRoles(Restrictions)
  else
  if MetaDataKind = 'userdefinedtypes' then
    Result := GetUDTs(Restrictions)
  else
  if MetaDataKind = 'packages' then
    Result := GetPackages(Restrictions)
  else
    raise Exception.Create(SUnsupportedMetaDataKind);
end;

procedure TCRMetaData.GetMetaDataKindsList(List: _TStrings);
var
  StrList: _TStringList;
begin
  StrList := _TStringList.Create;
  try
    InternalGetMetaDataKindsList(StrList);
    List.Assign(StrList);
  finally
    StrList.Free;
  end;
end;

procedure TCRMetaData.GetRestrictionsList(List: _TStrings; const MetaDataKind: _string);
var
  StrList: _TStringList;
begin
  StrList := _TStringList.Create;
  try
    InternalGetRestrictionsList(StrList, _LowerCase(Trim(MetaDataKind)));
    List.Assign(StrList);
  finally
    StrList.Free;
  end;
end;

procedure TCRMetaData.AddField(const AName: _string; AType: integer; ALength: integer = -1);
var
  Field: TFieldDesc;
  Size: integer;
begin
  Field := TFieldDesc.Create;
  Field.Name := AName;
  Field.ActualName := Field.Name;
  Field.DataType := AType;
  case AType of
    dtInt8: Size := SizeOf(Byte);
    dtInt16: Size := SizeOf(Word);
    dtInt32: Size := SizeOf(Integer);
    dtInt64: Size := SizeOf(Int64);
    dtFloat: Size := SizeOf(Double);
    dtBoolean: Size := SizeOf(Boolean);
    dtDateTime: Size := SizeOf(TDateTime);
    dtString: Size := Alength + 1;
    dtWideString: Size := (Alength + 1) * 2;
  else
    Size := SizeOf(IntPtr);
  end;
  Field.Size := Size;
  if ALength > 0 then
    Field.Length := ALength;

  Field.FieldNo := FMemData.Fields.Add(Field) + 1;
end;

procedure TCRMetaData.CopyRecord(const SourceIndices, DestIndices: array of integer);
var
  i: integer;
begin
  for i := 0 to High(SourceIndices) do
    FMemDataHelper.FieldValues[DestIndices[i]] := FRecordSetHelper.FieldValues[SourceIndices[i]];

{$IFDEF DBX_METADATA}
  FMemDataHelper.FieldValues[1] := FRecordSet.RecordNo;
{$ENDIF}
end;

function TCRMetaData.ParseTypes(const ObjectTypes: _string; AllTypes: array of _string): TBooleanArray;
var
  TypeName, RestTypes: _string;
  i, p: integer;
begin
  SetLength(Result, High(AllTypes) + 1);

  if ObjectTypes <> '' then begin
    for i := 0 to High(AllTypes) do
      Result[i] := False;
    RestTypes := ObjectTypes;
    repeat
      p := Pos(',', RestTypes);
      if p > 0 then
        TypeName := Copy(RestTypes, 1, p - 1)
      else
        TypeName := RestTypes;
      TypeName := _UpperCase(Trim(TypeName));
      for i := 0 to High(AllTypes) do
        if TypeName = _UpperCase(AllTypes[i]) then begin
          Result[i] := True;
          break;
        end;
      RestTypes := Copy(RestTypes, p + 1, Length(RestTypes) - p);
    until p = 0;
  end
  else
    for i := 0 to High(AllTypes) do
      Result[i] := True;
end;

procedure TCRMetaData.ParseTypes(const ObjectTypes: _string; TypesList: _TStringList);
var
  Types, S: _string;
  p: integer;
begin
  TypesList.Clear;
  TypesList.Sorted := True;
  TypesList.Duplicates := dupIgnore;
  Types := ObjectTypes;
  repeat
    p := Pos(',', Types);
    if p > 0 then begin
      S := Trim(Copy(Types, 1, p - 1));
      Types := Copy(Types, p + 1, MaxInt);
    end
    else
      S := Trim(Types);
    if S <> '' then
      TypesList.Add(_UpperCase(S));
  until p = 0;

  if TypesList.Count = 0 then
    TypesList.Add('');
end;

procedure TCRMetaData.AddWhere(var WhereClause: _string; const Name, Value: _string;
  AddEmpty: boolean = False);
var
  NormValue: _string;
begin
  if AddEmpty or (Value <> '') then begin
    NormValue := FRecordSet.GetCommand.SQLInfo.NormalizeName(Value, False, True);
    if AddEmpty or (NormValue <> '') then begin
      if WhereClause <> '' then
        WhereClause := WhereClause + ' AND ';
      if Value <> '' then
        WhereClause := WhereClause + Name + ' ' + FOperator + ' ' + _QuotedStr(NormValue, '''')
      else
        WhereClause := WhereClause + Name + ' IS NULL';
    end;
  end;
end;

procedure TCRMetaData.CreateMetaDataKindsFields;
begin
  FMemData.Fields.Clear;
  AddField('METADATA_NAME', dtString, 50);
  FMemData.InitFields;
end;

procedure TCRMetaData.InternalGetMetaDataKindsList(List: _TStringList);
begin
  List.Clear;
  List.Add('MetaDataKinds');
  List.Add('Restrictions');
  List.Add('Tables');
  List.Add('Columns');
  List.Add('Procedures');
  List.Add('ProcedureParameters');
  List.Add('Indexes');
  List.Add('IndexColumns');
  List.Add('Constraints');
  List.Sort;
end;

function TCRMetaData.GetMetaDataKinds: TData;
var
  List: _TStringList;
  i: integer;
begin
  List := _TStringList.Create;
  try
    GetMetaDataKindsList(List);
    CreateMetaDataKindsFields;
    FMemData.Open;
    Result := FMemData;
    FMemDataHelper.AllocBuffer;
    for i := 0 to List.Count - 1 do begin
      FMemDataHelper.InitRecord;
      FMemDataHelper.FieldValues[1] := List[i];
      FMemDataHelper.AppendRecord;
    end;
    FMemData.SetToBegin;
  finally
    List.Free;
  end;
end;

procedure TCRMetaData.CreateRestrictionsFields;
begin
  FMemData.Fields.Clear;
  AddField('METADATA_NAME', dtString, 50);
  AddField('RESTRICTION_NAME', dtString, 50);
  FMemData.InitFields;
end;

procedure TCRMetaData.InternalGetRestrictionsList(List: _TStringList; const MetaDataKind: _string);
begin
  List.Clear;

  if MetaDataKind = 'restrictions' then begin
    List.Add('METADATA_NAME');
  end
  else
  if MetaDataKind = 'tables' then begin
    List.Add('TABLE_CATALOG');
    List.Add('TABLE_SCHEMA');
    List.Add('TABLE_NAME');
    List.Add('TABLE_TYPE');
  end
  else
  if MetaDataKind = 'columns' then begin
    List.Add('TABLE_CATALOG');
    List.Add('TABLE_SCHEMA');
    List.Add('TABLE_NAME');
    List.Add('COLUMN_NAME');
  end
  else
  if MetaDataKind = 'procedures' then begin
    List.Add('PROCEDURE_CATALOG');
    List.Add('PROCEDURE_SCHEMA');
    List.Add('PROCEDURE_NAME');
    List.Add('PROCEDURE_TYPE');
  end
  else
  if MetaDataKind = 'procedureparameters' then begin
    List.Add('PROCEDURE_CATALOG');
    List.Add('PROCEDURE_SCHEMA');
    List.Add('PROCEDURE_NAME');
    List.Add('PARAMETER_NAME');
  end
  else
  if MetaDataKind = 'indexes' then begin
    List.Add('TABLE_CATALOG');
    List.Add('TABLE_SCHEMA');
    List.Add('TABLE_NAME');
    List.Add('INDEX_NAME');
  end
  else
  if MetaDataKind = 'indexcolumns' then begin
    List.Add('TABLE_CATALOG');
    List.Add('TABLE_SCHEMA');
    List.Add('TABLE_NAME');
    List.Add('INDEX_NAME');
  end
  else
  if MetaDataKind = 'constraints' then begin
    List.Add('TABLE_CATALOG');
    List.Add('TABLE_SCHEMA');
    List.Add('TABLE_NAME');
    List.Add('CONSTRAINT_NAME');
    List.Add('CONSTRAINT_TYPE');
  end;
end;

function TCRMetaData.GetRestrictions(Restrictions: _TStrings): TData;
var
  MetaInfoName: _string;
  MetaDataKinds, RestrList: _TStringList;
  i, j: integer;
begin
  MetaInfoName := _LowerCase(Trim(Restrictions.Values['METAINFO_NAME']));
  CreateRestrictionsFields;
  FMemData.Open;
  FMemDataHelper.AllocBuffer;

  RestrList := _TStringList.Create;
  try
    if MetaInfoName <> '' then begin
      GetRestrictionsList(RestrList, MetaInfoName);
      for j := 0 to RestrList.Count - 1 do begin
        FMemDataHelper.InitRecord;
        FMemDataHelper.FieldValues[1] := MetaInfoName;
        FMemDataHelper.FieldValues[2] := RestrList[j];
        FMemDataHelper.AppendRecord;
      end;
    end
    else begin
      MetaDataKinds := _TStringList.Create;
      try
        GetMetaDataKindsList(MetaDataKinds);
        for i := 0 to MetaDataKinds.Count - 1 do begin
          GetRestrictionsList(RestrList, _LowerCase(MetaDataKinds[i]));

          for j := 0 to RestrList.Count - 1 do begin
            FMemDataHelper.InitRecord;
            FMemDataHelper.FieldValues[1] := MetaDataKinds[i];
            FMemDataHelper.FieldValues[2] := RestrList[j];
            FMemDataHelper.AppendRecord;
          end;
        end;
      finally
        MetaDataKinds.Free;
      end;
    end;
  finally
    RestrList.Free;
  end;

  FMemData.SetToBegin;
  Result := FMemData;
end;

procedure TCRMetaData.CreateTablesFields;
begin
  FMemData.Fields.Clear;
{$IFDEF DBX_METADATA}
  AddField('RECNO', dtInt32);
  AddField('CATALOG_NAME', dtString, 100);
  AddField('SCHEMA_NAME', dtString, 100);
{$ELSE}
  AddField('TABLE_CATALOG', dtString, 100);
  AddField('TABLE_SCHEMA', dtString, 100);
{$ENDIF}
  AddField('TABLE_NAME', dtString, 100);
{$IFNDEF DBX_METADATA}
  AddField('TABLE_TYPE', dtString, 50);
{$ELSE}
  AddField('TABLE_TYPE', dtInt32);
{$ENDIF}
  FMemData.InitFields;
end;

procedure TCRMetaData.CreateColumnsFields;
begin
  FMemData.Fields.Clear;
{$IFDEF DBX_METADATA}
  AddField('RECNO', dtInt32);
  AddField('CATALOG_NAME', dtString, 100);
  AddField('SCHEMA_NAME', dtString, 100);
{$ELSE}
  AddField('TABLE_CATALOG', dtString, 100);
  AddField('TABLE_SCHEMA', dtString, 100);
{$ENDIF}
  AddField('TABLE_NAME', dtString, 100);
  AddField('COLUMN_NAME', dtString, 100);
{$IFNDEF DBX_METADATA}
  AddField('POSITION', dtInt32);
  AddField('DATA_TYPE', dtString, 50);
  AddField('DATA_LENGTH', dtInt32);
  AddField('DATA_PRECISION', dtInt32);
  AddField('DATA_SCALE', dtInt32);
  AddField('NULLABLE', dtInt32);
  AddField('DEFAULT_VALUE', dtMemo);
{$ELSE}
  AddField('COLUMN_POSITION', dtInt32);
  AddField('COLUMN_TYPE', dtInt32);
  AddField('COLUMN_DATATYPE', dtInt32);
  AddField('COLUMN_TYPENAME', dtString, 50);
  AddField('COLUMN_SUBTYPE', dtInt32);
  AddField('COLUMN_LENGTH', dtInt32);
  AddField('COLUMN_PRECISION', dtInt32);
  AddField('COLUMN_SCALE', dtInt32);
  AddField('COLUMN_NULLABLE', dtInt32);
{$ENDIF}
  FMemData.InitFields;
end;

procedure TCRMetaData.CreateProceduresFields;
begin
  FMemData.Fields.Clear;
{$IFDEF DBX_METADATA}
  AddField('RECNO', dtInt32);
  AddField('CATALOG_NAME', dtString, 100);
  AddField('SCHEMA_NAME', dtString, 100);
  AddField('PROC_NAME', dtString, 100);
  AddField('PROC_TYPE', dtInt32);
  AddField('IN_PARAMS', dtInt32);
  AddField('OUT_PARAMS', dtInt32);
{$ELSE}
  AddField('PROCEDURE_CATALOG', dtString, 100);
  AddField('PROCEDURE_SCHEMA', dtString, 100);
  AddField('PROCEDURE_NAME', dtString, 100);
  AddField('PROCEDURE_TYPE', dtString, 50);
{$ENDIF}
  FMemData.InitFields;
end;

procedure TCRMetaData.CreateProcedureParametersFields;
begin
  FMemData.Fields.Clear;
{$IFDEF DBX_METADATA}
  AddField('RECNO', dtInt32);
  AddField('CATALOG_NAME', dtString, 100);
  AddField('SCHEMA_NAME', dtString, 100);
  AddField('PROC_NAME', dtString, 100);
  AddField('PARAM_NAME', dtString, 100);
  AddField('PARAM_POSITION', dtInt32);
  AddField('PARAM_TYPE', dtInt32);
  AddField('PARAM_DATATYPE', dtInt32);
  AddField('PARAM_TYPENAME', dtString, 50);
  AddField('PARAM_SUBTYPE', dtInt32);
  AddField('PARAM_LENGTH', dtInt32);
  AddField('PARAM_PRECISION', dtInt32);
  AddField('PARAM_SCALE', dtInt32);
  AddField('PARAM_NULLABLE', dtInt32);
{$ELSE}
  AddField('PROCEDURE_CATALOG', dtString, 100);
  AddField('PROCEDURE_SCHEMA', dtString, 100);
  AddField('PROCEDURE_NAME', dtString, 100);
  AddField('PARAMETER_NAME', dtString, 100);
  AddField('POSITION', dtInt32);
  AddField('DIRECTION', dtString, 10);
  AddField('DATA_TYPE', dtString, 50);
  AddField('DATA_LENGTH', dtInt32);
  AddField('DATA_PRECISION', dtInt32);
  AddField('DATA_SCALE', dtInt32);
{$ENDIF}
  FMemData.InitFields;
end;

procedure TCRMetaData.CreateIndexesFields;
begin
  FMemData.Fields.Clear;
  AddField('TABLE_CATALOG', dtString, 100);
  AddField('TABLE_SCHEMA', dtString, 100);
  AddField('TABLE_NAME', dtString, 100);
  AddField('INDEX_CATALOG', dtString, 100);
  AddField('INDEX_SCHEMA', dtString, 100);
  AddField('INDEX_NAME', dtString, 100);
  AddField('UNIQUE', dtInt32);
  FMemData.InitFields;
end;

procedure TCRMetaData.CreateIndexColumnsFields;
begin
  FMemData.Fields.Clear;
{$IFDEF DBX_METADATA}
  AddField('RECNO', dtInt32);
  AddField('CATALOG_NAME', dtString, 100);
  AddField('SCHEMA_NAME', dtString, 100);
  AddField('TABLE_NAME', dtString, 100);
{$ELSE}
  AddField('TABLE_CATALOG', dtString, 100);
  AddField('TABLE_SCHEMA', dtString, 100);
  AddField('TABLE_NAME', dtString, 100);
  AddField('INDEX_CATALOG', dtString, 100);
  AddField('INDEX_SCHEMA', dtString, 100);
{$ENDIF}
  AddField('INDEX_NAME', dtString, 100);
  AddField('COLUMN_NAME', dtString, 100);
  AddField('COLUMN_POSITION', dtInt32);
{$IFDEF DBX_METADATA}
  AddField('PKEY_NAME', dtString, 100);
  AddField('INDEX_TYPE', dtInt32);
  AddField('SORT_ORDER', dtString, 1);
  AddField('FILTER', dtString, 6);
{$ELSE}
  AddField('SORT_ORDER', dtString, 4);
{$ENDIF}
  FMemData.InitFields;
end;

procedure TCRMetaData.CreateConstraintsFields;
begin
  FMemData.Fields.Clear;
  AddField('TABLE_CATALOG', dtString, 100);
  AddField('TABLE_SCHEMA', dtString, 100);
  AddField('TABLE_NAME', dtString, 100);
  AddField('CONSTRAINT_NAME', dtString, 100);
  AddField('CONSTRAINT_TYPE', dtString, 100);
  AddField('INDEX_CATALOG', dtString, 100);
  AddField('INDEX_SCHEMA', dtString, 100);
  AddField('INDEX_NAME', dtString, 100);
  FMemData.InitFields;
end;

{$IFDEF DBX_METADATA}
procedure TCRMetaData.CreateObjectListFields;
begin
  FMemData.Fields.Clear;
  AddField('RECNO', dtInt32);
  AddField('CATALOG_NAME', dtString, 100);
  AddField('SCHEMA_NAME', dtString, 100);
  AddField('OBJECT_NAME', dtString, 100);
  FMemData.InitFields;
end;
{$ENDIF}

procedure TCRMetaData.CreateDatabasesFields;
begin
  FMemData.Fields.Clear;
  AddField('DATABASE_NAME', dtString, 100);
  FMemData.InitFields;
end;

function TCRMetaData.GetDatabases(Restrictions: _TStrings): TData;
begin
  raise Exception.Create(SUnsupportedMetaDataKind);
end;

function TCRMetaData.GetDataTypes(Restrictions: _TStrings): TData;
begin
  raise Exception.Create(SUnsupportedMetaDataKind);
end;

function TCRMetaData.GetUsers(Restrictions: _TStrings): TData;
begin
  raise Exception.Create(SUnsupportedMetaDataKind);
end;

function TCRMetaData.GetRoles(Restrictions: _TStrings): TData;
begin
  raise Exception.Create(SUnsupportedMetaDataKind);
end;

function TCRMetaData.GetUDTs(Restrictions: _TStrings): TData;
begin
  raise Exception.Create(SUnsupportedMetaDataKind);
end;

function TCRMetaData.GetPackages(Restrictions: _TStrings): TData;
begin
  raise Exception.Create(SUnsupportedMetaDataKind);
end;

{ TDataHelper }

constructor TDataHelper.Create(Data: TData);
begin
  inherited Create;

  FData := Data;
end;

destructor TDataHelper.Destroy;
begin
  FreeBuffer;

  inherited Destroy;
end;

procedure TDataHelper.AllocBuffer;
begin
  FreeBuffer;
  FData.AllocRecBuf(FRecBuf);
end;

procedure TDataHelper.FreeBuffer;
begin
  if FRecBuf <> nil then begin
    Marshal.FreeHGlobal(FRecBuf);
    FRecBuf := nil;
  end;
end;

procedure TDataHelper.InitRecord;
begin
  FData.InitRecord(FRecBuf);
  if FData.HasComplexFields then
    FData.CreateComplexFields(FRecBuf, True);
end;

procedure TDataHelper.AppendRecord;
begin
  FData.AppendRecord(FRecBuf);
end;

function TDataHelper.NextRecord: boolean;
begin
  FData.GetNextRecord(FRecBuf);
  Result := not FData.Eof;
end;

function TDataHelper.GetFieldValue(Index: integer): variant;
begin
  FData.GetFieldAsVariant(Index, FRecBuf, Result);
end;

procedure TDataHelper.SetFieldValue(Index: integer; const Value: variant);
begin
  FData.PutFieldAsVariant(Index, FRecBuf, Value);
end;

{ TCRLoaderColumn }

constructor TCRLoaderColumn.Create;
begin
  inherited;
end;

{ TCRLoaderColumns }

procedure TCRLoaderColumns.Clear;
var
  i: integer;
begin
  for i := 0 to Count - 1 do
    Items[i].Free;

  inherited;
end;

function TCRLoaderColumns.GetColumnIndexByName(Name: _string): Integer;
var
  i: Integer;
  ColumnNameUpper: _string;
begin
  Result := -1;

  ColumnNameUpper := UpperCase(Name);
  for i := 0 to Count - 1 do
    if UpperCase(Items[i].Name) = ColumnNameUpper then begin
      Result := i;
      exit;
    end;
end;

function TCRLoaderColumns.FindColumnByName(Name: _string): TCRLoaderColumn;
var
  ColumnIndex: integer;
begin
  ColumnIndex := GetColumnIndexByName(Name);
  if ColumnIndex <> -1 then
    Result := Items[ColumnIndex]
  else
    Result := nil;
end;


function TCRLoaderColumns.GetColumnByName(Name: _string): TCRLoaderColumn;
var
  ColumnIndex: integer;
begin
  ColumnIndex := GetColumnIndexByName(Name);
  if ColumnIndex <> -1 then
    Result := Items[ColumnIndex]
  else
    raise Exception.Create('Invalid column name');
end;

function TCRLoaderColumns.GetColumn(Index: integer): TCRLoaderColumn;
begin
  Result := TCRLoaderColumn(inherited Items[Index]);
end;

procedure TCRLoaderColumns.SetColumn(Index: integer; Value: TCRLoaderColumn);
begin
  inherited Items[Index] := Value;
end;

{ TCRLoader }

constructor TCRLoader.Create;
begin
  inherited;

  FSkipReadOnlyFieldDescs := True;
  FColumns := TCRLoaderColumns.Create;
end;

destructor TCRLoader.Destroy;
begin
  FColumns.Free;

  inherited;
end;

function TCRLoader.SetProp(Prop: integer; const Value: variant): boolean;
begin
  Assert(False, IntToStr(Prop));
  Result := False;
end;

function TCRLoader.GetProp(Prop: integer; var Value: variant): boolean;
begin
  Assert(False, IntToStr(Prop));
  Result := False;
end;

procedure TCRLoader.DoPutColumnData(Col: integer; Row: integer; const Value: variant);
begin
end;

procedure TCRLoader.PutColumnData(Col: integer; Row: integer; const Value: variant);
begin
  if Col >= FColumns.Count then
    raise Exception.Create('Invalid column number');

  if (Row < FLastRow + 1) or (Row < 1) or (Row > FLastRow + 2) then
    raise Exception.Create('Invalid row number');

  FLastRow := Row - 1;

  DoPutColumnData(Col, Row, Value);
end;

procedure TCRLoader.CheckTableName;
begin
  if Trim(FTableName) = '' then
    raise Exception.Create(STableNameNotDefined);
end;

procedure TCRLoader.DoPrepare;
begin
end;

procedure TCRLoader.Prepare;
begin
  CheckTableName;
  Reset;

  DoPrepare;
end;

procedure TCRLoader.Finish;
begin
end;

procedure TCRLoader.Reset;
begin
  FLastRow := -1;
  FLoadedRows := 0;
end;

procedure TCRLoader.DoLoad;
begin
end;

procedure TCRLoader.FillColumn(Column: TCRLoaderColumn; FieldDesc: TFieldDesc);
begin
  Column.Name := FieldDesc.Name;
  Column.DataType := FieldDesc.DataType;
  case FieldDesc.DataType of
    dtInt8, dtSmallint, dtInteger, dtLargeint, dtWord, dtLongword,
    dtFloat, dtCurrency,
    dtBCD{$IFDEF VER6P}{$IFNDEF FPC}, dtFMTBcd{$ENDIF}{$ENDIF}: begin
      Column.Precision := FieldDesc.Length;
      Column.Scale := FieldDesc.Scale;
    end;
  else
    Column.Size := FieldDesc.Length;
    Column.DataSize := FieldDesc.Size;
  end;
end;

procedure TCRLoader.CreateColumns;
var
  RecordSet: TCRRecordSet;
  FieldDesc: TFieldDesc;
  Col: TCRLoaderColumn;
  i: integer;
begin
  CheckTableName;
  FColumns.Clear;
  RecordSet := GetRecordSetClass.Create;
  RecordSet.SetConnection(FConnection);
  RecordSet.SetTransaction(FTransaction);
  RecordSet.SetProp(prFetchAll, True);
  RecordSet.SetSQL('SELECT * FROM ' + FTableName + ' WHERE 1=0'); // CR-M15322
  try
    RecordSet.Open;
    for i := 0 to RecordSet.FieldCount - 1 do begin
      FieldDesc := RecordSet.Fields[i];
      if not (FieldDesc.ReadOnly and FSkipReadOnlyFieldDescs) then begin
        Col := GetColumnClass.Create;
        FillColumn(Col, FieldDesc);
        FColumns.Add(Col);
      end;
    end;
    RecordSet.Close;
  finally
    RecordSet.Free;
  end;
end;

procedure TCRLoader.DiscardRow;
begin
  if FLastRow >= FLoadedRows then
    Dec(FLastRow);
end;

class function TCRLoader.GetColumnClass: TCRLoaderColumnClass;
begin
  Result := TCRLoaderColumn;
end;

class function TCRLoader.GetRecordSetClass: TCRRecordSetClass;
begin
  Assert(False);
  Result := TCRRecordSet;
end;

procedure TCRLoader.SetConnection(Value: TCRConnection);
begin
  FConnection := Value;
end;

procedure TCRLoader.SetTransaction(Value: TCRTransaction);
begin
  FTransaction := Value;
end;

{ TCRSimpleLoader }

constructor TCRSimpleLoader.Create;
begin
  inherited;

  CreateCommand;
  FCommand.SetProp(prDisableParamScan, True);
end;

destructor TCRSimpleLoader.Destroy;
begin
  FCommand.Free;

  inherited;
end;

procedure TCRSimpleLoader.Reset;
begin
  inherited;

  FCommand.Params.Clear;
end;

procedure TCRSimpleLoader.DoPrepare;
var
  i: integer;
  Col: TCRLoaderColumn;
  Param: TParamDesc;
  InsertSB: _StringBuilder;
begin
  FCommand.Unprepare;
  FCommand.SetConnection(FConnection);
  InsertSB := _StringBuilder.Create(1024);
  try
    InsertSB.Append('INSERT INTO ' + FCommand.SQLInfo.NormalizeName(FTableName) + '(');
    for i := 0 to Columns.Count - 1 do begin
      Col := Columns[i];
      if i > 0 then
        InsertSB.Append(',');
      InsertSB.Append(FCommand.SQLInfo.NormalizeName(Col.Name));
    end;
    InsertSB.Append(') VALUES (');

    for i := 0 to Columns.Count - 1 do begin
      Col := Columns[i];
      if FCommand.Params.Count <= i then begin
        Param := FCommand.GetParamDescType.Create;
        Param.SetDataType(Col.DataType);
        Param.SetSize(Col.Size);
        Param.SetParamType(pdInput);
        FCommand.Params.Add(Param);
      end;

      if i > 0 then
        InsertSB.Append(',?')
      else
        InsertSB.Append('?');
    end;
    InsertSB.Append(')');

    FCommand.SetSQL(InsertSB.ToString);
  finally
    InsertSB.Free;
  end;
  FCommand.Prepare;
end;

procedure TCRSimpleLoader.DoPutColumnData(Col: integer; Row: integer; const Value: variant);
var
  Param: TParamDesc;
begin
  Param := FCommand.Params[Col];
  Param.SetValue(Value);
end;

procedure TCRSimpleLoader.PutColumnData(Col, Row: integer; const Value: variant);
begin
  if (FLastRow <> -1) and (Row > FLastRow + 1) then
    DoLoadRow;

  inherited;
end;

procedure TCRSimpleLoader.DoLoad;
begin
  if FLastRow >= FLoadedRows then
    DoLoadRow;
end;

procedure TCRSimpleLoader.DoLoadRow;
begin
  FCommand.Execute;
  FLoadedRows := FLastRow + 1; 
end;

procedure TCRSimpleLoader.Finish;
begin
  FCommand.Unprepare;
  Reset;

  inherited;
end;

{ TCRAlerter }

constructor TCRAlerter.Create;
begin
  inherited;

  FEventNames := _TStringList.Create;
end;

destructor TCRAlerter.Destroy;
begin
  FEventNames.Free;

  inherited;
end;

function TCRAlerter.SetProp(Prop: integer; const Value: variant): boolean;
begin
  Result := True;
  case Prop of
    prAutoCommit:
      ;
  else
    Assert(False, IntToStr(Prop));
    Result := False;
  end;
end;

function TCRAlerter.GetProp(Prop: integer; var Value: variant): boolean;
begin
  Assert(False, IntToStr(Prop));
  Result := False;
end;

function TCRAlerter.IsActive: boolean;
begin
  Result := FActive;
end;

procedure TCRAlerter.SetConnection(Value: TCRConnection);
begin
  FConnection := Value;
end;

procedure TCRAlerter.SetTransaction(Value: TCRTransaction);
begin
  FTransaction := Value;
end;

end.
