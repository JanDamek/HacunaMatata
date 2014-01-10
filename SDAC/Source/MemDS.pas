//////////////////////////////////////////////////
//  DB Access Components
//  Copyright © 1998-2012 Devart. All right reserved.
//  Memory Data Set
//  Created:            01.02.98
//////////////////////////////////////////////////

{$IFNDEF CLR}
{$IFNDEF FPC}
{$I Dac.inc}

unit MemDS;
{$ENDIF}
{$ENDIF}
interface
uses
  Classes, SysUtils, {$IFDEF VER6P}Variants,{$ENDIF} DB,
{$IFDEF BDE_SHARED}
  DBTables,
{$ENDIF}
{$IFDEF MSWINDOWS}
  Windows, Win32Timer,
{$ENDIF}
{$IFDEF CLR}
  System.Text, System.IO, System.XML, System.Runtime.InteropServices,
  Contnrs, ExtCtrls,
{$ELSE}
  CLRClasses, CRXml,
{$ENDIF}
  CRTypes, CRFunctions, MemData, MemUtils;

const
  uaDefault = 10; // TUpdateAction

{$IFNDEF FPC}
{$IFNDEF VER10P}
  ftFixedWideChar = 52;
{$ENDIF}
{$ENDIF}

type
  TDataSetService = class;
  TDataSetServiceClass = class of TDataSetService;
  TMemDataSet = class;

{ TMemDataSet }

{$IFDEF VER4}

{$IFNDEF BDE_SHARED}
  TUpdateAction = (uaFail, uaAbort, uaSkip, uaRetry, uaApplied); // uaDefault
{$ENDIF}

{$ENDIF}

  TUpdateRecordTypes = set of (rtModified, rtInserted, rtDeleted, rtUnmodified);
  TUpdateErrorEvent = procedure(DataSet: TDataSet; E: EDatabaseError;
    UpdateKind: TUpdateKind; var UpdateAction: TUpdateAction) of object;
  TUpdateRecordEvent = procedure(DataSet: TDataSet; UpdateKind: TUpdateKind;
    var UpdateAction: TUpdateAction) of object;

{$IFDEF CLR}
  PRecInfo = packed record
  private
    Ptr: IntPtr;

    function GetRecordNumber: longint;
    procedure SetRecordNumber(const Value: longint);
    function GetUpdateStatus: TUpdateStatus;
    procedure SetUpdateStatus(const Value: TUpdateStatus);
    function GetBookmarkFlag: TBookmarkFlag;
    procedure SetBookmarkFlag(const Value: TBookmarkFlag);
    function GetRefComplexFields: boolean;
    procedure SetRefComplexFields(const Value: boolean);

  public
    property RecordNumber: longint read GetRecordNumber write SetRecordNumber;
    property UpdateStatus: TUpdateStatus read GetUpdateStatus write SetUpdateStatus;
    property BookmarkFlag: TBookmarkFlag read GetBookmarkFlag write SetBookmarkFlag;
    property RefComplexFields: boolean read GetRefComplexFields write SetRefComplexFields;

    class operator Implicit(AValue: IntPtr): PRecInfo;
  end;
{$ELSE}
{$IFDEF VER12P}
  TRecordBuffer = PByte;
{$ELSE}
  TRecordBuffer = PAnsiChar;
{$ENDIF}
  TValueBuffer = pointer;
  PRecInfo = ^TRecInfo;
{$ENDIF}
  TRecInfo = packed record
    RecordNumber: longint;
    UpdateStatus: TUpdateStatus;
    BookmarkFlag: TBookmarkFlag;
    RefComplexFields: boolean;
  end;

//  TBlobData = string;

  TCalcFieldDescMapping = record
    FieldDesc: TFieldDesc;
    Field: TField;
  end;

  TLocalMDLink = record
    IsNull: boolean;
    Buffer: IntPtr;
    BufferType: integer;
    NativeBuffer: boolean;
    FieldNo: integer;
  end;

  TLocalMDLinks = array of TLocalMDLink;

  TDataTypesMap = class
  public
    class function GetFieldType(DataType: Word): TFieldType; virtual;
    class function GetDataType(FieldType: TFieldType): integer; virtual;
  end;

  TDataTypesMapClass = class of TDataTypesMap;

{ TDADetailDataLink }

  TDADetailDataLink = class (TDetailDataLink)
  private
    FDataSet: TMemDataSet;

  protected
    procedure ActiveChanged; override;
    procedure RecordChanged(Field: TField); override;
    procedure CheckBrowseMode; override;
    function GetDetailDataSet: TDataSet; override;
  public
    constructor Create(DataSet: TMemDataSet);
  end;

  TDataSetUpdater = class
  protected
    FDataSet: TMemDataSet;
    FDataSetService: TDataSetService;

    function PerformAppend: boolean; virtual;
    function PerformDelete: boolean; virtual;
    function PerformUpdate: boolean; virtual;
    function CacheChanged: boolean; virtual;
    function CacheApplied: boolean; virtual;
    function CacheCanceled: boolean; virtual;

    procedure DoPerformAppend;
    procedure DoPerformDelete;
    procedure DoPerformUpdate;
    procedure DoApplyRecord(UpdateKind: TUpdateRecKind; var Action: TUpdateRecAction; LastItem: boolean);
    procedure DoCacheChanged;
    procedure DoCacheApplied;
    procedure DoCacheCanceled;

    function BatchUpdate: boolean; virtual;
    function CanFlushBatch: boolean; virtual;
    procedure FlushBatch; virtual;
  public
    constructor Create(AOwner: TDataSetService); virtual;
  end;

  TDataSetService = class
  protected
    FDataset: TMemDataSet;
    FUpdater: TDataSetUpdater;

    procedure CreateDataSetUpdater; virtual;
    procedure SetDataSetUpdater(Value: TDataSetUpdater); virtual;
    procedure FreeDataSetUpdater();

    procedure SetNumberRange(FieldDef: TFieldDef); virtual;
    function GetFieldClass(FieldType: TFieldType): TFieldClass; virtual;

    procedure PreInitCursor; virtual;

  { XML }
    procedure WriteFieldXMLDataType(Field: TField; FieldDesc: TFieldDesc; const FieldAlias: _string;
      XMLWriter: XMLTextWriter); virtual;
    procedure WriteFieldXMLAttributeType(Field: TField; FieldDesc: TFieldDesc; const FieldAlias: _string;
      XMLWriter: XMLTextWriter); virtual;
    function GetFieldXMLValue(Field: TField; FieldDesc: TFieldDesc): WideString; virtual;

  public
    constructor Create(AOwner: TMemDataSet); virtual;
    destructor Destroy; override;

    function SetProp(Prop: integer; const Value: variant): boolean; virtual;
    function GetProp(Prop: integer; var Value: variant): boolean; virtual;

    procedure SaveToXML(Destination: TStream);
  end;

  TMemDataSet = class(TDataSet)
  private
    FOldRecBuf: TRecordBuffer;
    FFilterBuffer: TRecordBuffer;
    FCachedUpdates: boolean;
    FLocalUpdate: boolean;
    //FInDeferredPost: boolean; to protected
    FInInserting: boolean;
    FInEditing: boolean;
    FIndexFieldNames: _string;
    FCalcFieldsMapping: array of TCalcFieldDescMapping;

    FUpdateRecordTypes: TUpdateRecordTypes;
    FOnUpdateError: TUpdateErrorEvent;
    FOnUpdateRecord: TUpdateRecordEvent;

  { Master/Detail }
    FDataLink: TDADetailDataLink;
    FLocalMDLinks: TLocalMDLinks;
    FDetailDelay: integer;
  {$IFDEF MSWINDOWS}
    FDetailRefreshTimer: TWin32Timer;
    procedure CheckRefreshDetailTimer;
  {$ENDIF}
    procedure SetDetailDelay(Value: integer);
    procedure SetMasterSource(Value: TDataSource);
    procedure SetMasterFields(Value: _string);
    procedure SetDetailFields(Value: _string);

    //Renamed LocateRecord (CBuilder5 bug - overloaded methods in different sections):
    function InternalLocateRecord(KeyFields: TDAList; KeyValues: variant;
      Options: TLocateExOptions; SavePos: boolean): boolean; overload;

    {function GetBlobData(Field:TField; Buffer: PAnsiChar):TBlobData;
    procedure SetBlobData(Field:TField; Buffer: PAnsiChar; Value:TBlobData);
    procedure ClearBlobCache(Buffer: PAnsiChar);}

    procedure SetCachedUpdates(Value: boolean);
    procedure SetLocalUpdate(Value: boolean);
    function GetUpdatesPending: boolean;
    function GetPrepared: boolean;
    procedure SetPrepared(Value: boolean);
    function ConvertUpdateRecordTypes(Value: TUpdateRecordTypes): TItemTypes;
    function GetUpdateRecordTypes: TUpdateRecordTypes;
    procedure SetUpdateRecordTypes(Value: TUpdateRecordTypes);
    procedure SetIndexFieldNames(Value: _string);

  protected
    Data: TData;  // FIRecordSet
    FDataSetService: TDataSetService;

    FBookmarkOfs: longint;
    FRecInfoOfs: longint;
{$IFNDEF CLR}
{$IFNDEF VER10P}
    FWideStringOfs: longint;
{$ENDIF}    
{$ENDIF}
    FRecBufSize: longint;
    FInCacheProcessing: boolean;
    FInDeferredPost: boolean;  // private
    NewCacheRecBuf: TRecordBuffer;
    OldCacheRecBuf: TRecordBuffer;
    OldDeferredPostBuf: TRecordBuffer;
    FParentDataSet: TMemDataSet;
    FLastParentPos: integer;
    FLocalConstraints: boolean;
    FNumberRange: boolean;
    FNeedAddRef: boolean;
    FCacheCalcFields: boolean;
    FCreateCalcFieldDescs: boolean;
    FInSettingDefaultExpressionValues: boolean;

    FMasterFields: _string;
    FDetailFields: _string;

  {$IFDEF CLR}
    function BookmarkAvailable: Boolean;
    property BookmarkSize;
  {$ENDIF}

    procedure SetModified(Value: Boolean); reintroduce;
    function SetTempState(const Value: TDataSetState): TDataSetState; reintroduce;
    procedure RestoreState(const Value: TDataSetState); reintroduce;
    procedure DoBeforeRefresh; override;
    procedure DoAfterRefresh; override;

    procedure CreateIRecordSet; virtual;
    procedure FreeIRecordSet;
    procedure SetIRecordSet(Value: TData{TRecordSet}); virtual;

    function GetDataSetServiceClass: TDataSetServiceClass; virtual;
    procedure CreateDataSetService;
    procedure FreeDataSetService;
    procedure SetDataSetService(Value: TDataSetService); virtual;
    procedure CheckDataSetService;

  { Open/Close DataSet }
    procedure OpenCursor(InfoQuery: boolean); override;
    procedure CloseCursor; override;

  {$IFDEF FPC}
    procedure CreateFields; override;
  {$ENDIF}
    procedure InternalOpen; override;
    procedure InternalClose; override;
    function IsCursorOpen: boolean; override;
    procedure DataReopen; virtual;
    procedure InternalRefresh; override;
    procedure DoAfterOpen; override;

  { Field Management }
    function GetDataTypesMap: TDataTypesMapClass; virtual;

    procedure InternalInitFieldDefs; override;
    function  NeedCreateFieldDefs: boolean; virtual;
    procedure CreateFieldDefs; virtual;
  {$IFNDEF FPC}
    procedure UpdateFieldDefList;
  {$ENDIF}
    function  NeedComplexUpdateFieldDefList: boolean; virtual;
    procedure ClearCalcFields(Buffer: TRecordBuffer); override;

    function GetObjectFieldDefName(Parent: TFieldDef; Index: integer; ObjType: TObjectType):_string; virtual;
    function GetFielDefSize(FieldType: TFieldType; FieldDesc: TFieldDesc): integer; virtual;
  {$IFNDEF FPC}
    procedure GetObjectTypeNames(Fields: TFields);
  {$ENDIF}
    function GetFieldType(DataType: word): TFieldType; overload; virtual;
    function GetFieldType(FieldDesc: TFieldDesc): TFieldType; overload; virtual;
    procedure SetFieldData(Field: TField; Buffer: TValueBuffer); override;
    procedure SetFieldData(Field: TField; Buffer: TValueBuffer; NativeFormat: Boolean); override;
    procedure DataConvert(Field: TField; Source, Dest: TValueBuffer; ToNative: Boolean); override; //TODO
    function GetSparseArrays: boolean;
    procedure SetSparseArrays(Value: boolean);
    procedure CheckFieldCompatibility(Field: TField; FieldDef: TFieldDef); {$IFNDEF FPC}override{$ELSE}virtual{$ENDIF};
    function GetFieldClass(FieldType: TFieldType): TFieldClass; override;

  { Buffer/Record Management }
    function AllocRecordBuffer: TRecordBuffer; override;
    procedure FreeRecordBuffer(var Buffer: TRecordBuffer); override;
    procedure CopyRecordBuffer(SrcBuffer: IntPtr; DstBuffer: IntPtr);

    procedure InitRecord(Buffer: TRecordBuffer); override;
    procedure InternalInitRecord(Buffer: TRecordBuffer); override;

    function GetOldRecord: TRecordBuffer;
    function GetOldRecBuf: TRecordBuffer;
    function GetNewRecBuf: TRecordBuffer;
    function GetActiveRecBuf(var RecBuf: TRecordBuffer): boolean;
    function GetRecord(Buffer: TRecordBuffer; GetMode: TGetMode; DoCheck: boolean): TGetResult; override;

  {$IFNDEF FPC}
    procedure BlockReadNext; override;
    procedure SetBlockReadSize(Value: integer); override;
  {$ENDIF}
  {$IFDEF FPC}
    procedure SetBufListSize(Value: Longint); override;
  {$ENDIF}

    procedure FreeRefBuffers;
    procedure FreeRefComplexFields(Buffer: TRecordBuffer; WithBlob: boolean = True);

  { Bookmarks }
    procedure GetBookmarkData(Buffer: TRecordBuffer; {$IFDEF CLR}var Bookmark: TBookmark{$ELSE}Bookmark: Pointer{$ENDIF}); override;
    procedure SetBookmarkData(Buffer: TRecordBuffer; {$IFDEF CLR}const Bookmark: TBookmark{$ELSE}Bookmark: Pointer{$ENDIF}); override;
    function GetBookmarkFlag(Buffer: TRecordBuffer): TBookmarkFlag; override;
    procedure SetBookmarkFlag(Buffer: TRecordBuffer; Value: TBookmarkFlag); override;
    //function GetBookmarkStr: TBookmarkStr; override;

  { Navigation }
    procedure InternalFirst; override;
    procedure InternalLast; override;
    procedure InternalGotoBookmark({$IFDEF CLR}const Bookmark: TBookmark{$ELSE}Bookmark: Pointer{$ENDIF}); override;
    procedure InternalSetToRecord(Buffer: TRecordBuffer); override;

  { Editing }
    procedure InternalAddRecord(Buffer: IntPtr; Append: boolean); override;
    procedure InternalInsert; override;
    procedure InternalDelete; override;
    procedure InternalEdit; override;

    procedure InternalPost; override;
    procedure InternalCancel; override;

    procedure InternalDeferredPost; virtual;

    procedure SetDefaultExpressionValues; virtual;
    procedure DoOnNewRecord; override;

    procedure DoPerformAppend;
    procedure DoPerformDelete;
    procedure DoPerformUpdate;
    procedure DoApplyRecord(UpdateKind: TUpdateRecKind; var Action: TUpdateRecAction; LastItem: boolean);
    procedure DoCacheChanged;
    procedure DoCacheApplied;
    procedure DoCacheCanceled;

    procedure DoGetCachedFields;
    procedure DoGetCachedBuffer(Buffer: IntPtr; Source: IntPtr = nil);

  { Filter/Find/Locate }
    procedure ActivateFilters;
    procedure DeactivateFilters;
    function RecordFilter(RecBuf: IntPtr): boolean;
    procedure SetFilterData(const Text: string; Options:TFilterOptions);

    procedure SetFiltered(Value: boolean); override;
    procedure SetFilterOptions(Value: TFilterOptions); override;
    procedure SetFilterText(const Value: string); override;
    procedure SetOnFilterRecord(const Value: TFilterRecordEvent); override;

    procedure CopyFieldValue(const Value: variant; out ValuePtr: IntPtr; out ValueType: integer; FieldDesc: TFieldDesc; UseFieldType: boolean = True); virtual;
    function LocateRecord(const KeyFields: _string; const KeyValues: variant;
      Options: TLocateExOptions; SavePos: boolean): boolean; overload;
    function LocateRecord(const KeyFields: array of TField; const KeyValues: variant;
      Options: TLocateExOptions; SavePos: boolean): boolean; overload;
    function FindRecord(Restart, GoForward: boolean): boolean; override;

  { Master/Detail }
    function AddFieldToList(const FieldName: _string; DataSet: TDataSet; List: TList): boolean;
    function GetDataSource: TDataSource; override;
    function IsMasterDatasetActive: boolean;
    function IsConnectedToMaster: boolean;
    function SetLocalMDLinks: boolean;
    function MDLinksRefreshed: boolean; virtual;
    procedure MasterRecordChanged;
    procedure RefreshDetail(Sender: TObject); virtual;
    function LocalDetailFilter(RecBuf: IntPtr): boolean;
    function UseLocalMasterDetailFilter: boolean; virtual;
    procedure SetLocalDetailFilter;
    procedure MDPropertiesChanged; virtual;

    property MasterSource: TDataSource read GetDataSource write SetMasterSource;
    property MasterFields: _string read FMasterFields write SetMasterFields;
    property DetailFields: _string read FDetailFields write SetDetailFields;
    property DetailDelay: integer read FDetailDelay write SetDetailDelay;

  { CachedUpdates }

    function InternalGetUpdateResult: TUpdateRecAction;
    procedure CheckCachedUpdateMode;

  { Blobs }
    //Renamed GetBlob (CBuilder5 bug - overloaded methods in different sections):
    function InternalGetBlob(FieldDesc: TFieldDesc): TBlob;
    function InternalSetBlob(FieldDesc: TFieldDesc; Blob: TBlob): boolean;
    function SetBlob(Field: TField; Blob: TBlob): boolean;
    procedure CloseBlob(Field: TField); override;

  { Misc }
    function GetRecordCount: integer; override;
    function GetRecordSize: word; override;

    function GetRecNo: integer; override;
    procedure SetRecNo(Value: integer); override;

    procedure InternalHandleException; override;

    procedure AssignTo(Dest: TPersistent); override;

  {$IFDEF CLR}
    procedure DataEvent(Event: TDataEvent; Info: TObject); override;
  {$ELSE} {$IFDEF FPC}
    procedure DataEvent(Event: TDataEvent; Info: PtrInt); override;
  {$ELSE} {$IFDEF VER16P}
    procedure DataEvent(Event: TDataEvent; Info: NativeInt); override;
  {$ELSE}
    procedure DataEvent(Event: TDataEvent; Info: Longint); override;
  {$ENDIF} {$ENDIF} {$ENDIF}

  {$IFNDEF FPC}
    function GetStateFieldValue(State: TDataSetState; Field: TField): Variant; override;
  {$ENDIF}

  public
    constructor Create(aOwner: TComponent); override;
    destructor Destroy; override;

    procedure Prepare; virtual;
    procedure UnPrepare; virtual;
    procedure CheckPrepared;

    procedure GetDetailLinkFields(MasterFields, DetailFields: {$IFDEF CLR}TObjectList{$ELSE}TList{$ENDIF}); {$IFNDEF FPC}override;{$ELSE}virtual;{$ENDIF}

  { Fields }
    function GetFieldDescNo(Field: TField): integer;
    function GetFieldDesc(const Field: TField): TFieldDesc; overload; virtual;
    function GetFieldData(Field: TField; Buffer: TValueBuffer): boolean; overload; override;
    //function Translate(const Src: string; var Dest: string; ToOem: boolean): integer; override;

  {$IFNDEF FPC}
    function GetFieldData(FieldNo: integer; Buffer: TValueBuffer): boolean; overload; override;
  {$ENDIF}
    function GetFieldData(Field: TField; Buffer: TValueBuffer; NativeFormat: Boolean): Boolean; override;

    function GetBlob(const FieldName: _string): TBlob; overload;
    function GetBlob(Field: TField): TBlob; overload;

  { Edit }
    procedure Cancel; override;
    procedure DeferredPost;

  { Bookmarks }
  {$IFDEF CLR} // TDataSet bug
    function GetBookmark: TBookmark; override;
    procedure FreeBookmark(var Bookmark: TBookmark); override;
  {$ENDIF}
    function BookmarkValid({$IFDEF CLR}const{$ENDIF} Bookmark: TBookmark): boolean; override;
    function CompareBookmarks({$IFDEF CLR}const{$ENDIF} Bookmark1, Bookmark2: TBookmark): integer; override;

    function CreateBlobStream(Field: TField; Mode: TBlobStreamMode): TStream; override;

    function Locate(const KeyFields: string; const KeyValues: variant;
      Options: TLocateOptions): boolean; overload; override;
    function Locate(const KeyFields: array of TField; const KeyValues: variant;
      Options: TLocateOptions): boolean; reintroduce; overload;
    function LocateEx(const KeyFields: _string; const KeyValues: variant;
      Options: TLocateExOptions): boolean; overload;
    function LocateEx(const KeyFields: array of TField; const KeyValues: variant;
      Options: TLocateExOptions): boolean; overload;
    function Lookup(const KeyFields: string; const KeyValues: variant;
      const ResultFields: string): variant; override;

  { CachedUpdates }
    function UpdateStatus: TUpdateStatus; override;
    function UpdateResult: TUpdateAction;
    procedure ApplyUpdates; overload; virtual;
    procedure ApplyUpdates(const UpdateRecKinds: TUpdateRecKinds); overload; virtual;
    procedure CommitUpdates;
    procedure CancelUpdates;
    procedure RestoreUpdates;
    procedure RevertRecord;

  { XML }
    procedure SaveToXML(Destination: TStream); overload;
    procedure SaveToXML(const FileName: string); overload;

    function IsSequenced: boolean; override;

    property Prepared: boolean read GetPrepared write SetPrepared;
    property CachedUpdates: boolean read FCachedUpdates write SetCachedUpdates default False;
    property UpdatesPending: boolean read GetUpdatesPending;
    property LocalUpdate: boolean read FLocalUpdate write SetLocalUpdate default False;
    property UpdateRecordTypes: TUpdateRecordTypes read GetUpdateRecordTypes write SetUpdateRecordTypes default [rtModified, rtInserted, rtUnmodified];
    property SparseArrays: boolean read GetSparseArrays write SetSparseArrays;

  // obsolete
    property LocalConstraints: boolean read FLocalConstraints write FLocalConstraints default True;

    property OnUpdateError: TUpdateErrorEvent read FOnUpdateError write FOnUpdateError;
    property OnUpdateRecord: TUpdateRecordEvent read FOnUpdateRecord write FOnUpdateRecord;
    property IndexFieldNames: _string read FIndexFieldNames write SetIndexFieldNames;
  end;

{ TBlobStream }

  TBlobStream = class(TStream)
  protected
    FField: TBlobField;
    FDataSet: TMemDataSet;
    FBuffer: TRecordBuffer;
    FMode: TBlobStreamMode;
    FFieldNo: integer;
    FOpened: boolean;
    FModified: boolean;
    FPosition: longint;
    function GetBlobSize: Longint;

  protected
  {$IFDEF CLR}
    procedure SetSize(NewSize: Int64); override;
  {$ELSE}
    procedure SetSize(NewSize: Longint); override;
  {$ENDIF}
  public
    constructor Create(Field: TBlobField; Mode: TBlobStreamMode);
    destructor Destroy; override;
  {$IFDEF CLR}
    function Read(var Buffer: TBytes; Offset, Count: Longint): Longint; override;
    function Write(const Buffer: TBytes; Offset, Count: Longint): Longint; override;
    function Seek(const Offset: Int64; Origin: TSeekOrigin): Int64; override;
  {$ELSE}
    function Read(var Buffer; Count: longint): longint; override;
    function Write(const Buffer; Count: Longint): Longint; override;
    function Seek(Offset: Longint; Origin: Word): Longint; override;
  {$ENDIF}
    procedure Truncate;
  end;

  TMemDSUtils = class
  public
    class function SetBlob(Obj: TMemDataSet; Field: TField; Blob: TBlob): boolean;
    class function GetBlob(Obj: TMemDataSet; FieldDesc: TFieldDesc): TBlob;
  end;

  function ChangeDecimalSeparator(const Value: string; OldSeparator, NewSeparator: string): string;


var
  SendDataSetChangeEventAfterOpen: boolean = True;  ///CR-CRC23525  remove at 18.11.07
  DoNotRaiseExcetionOnUaFail: boolean = False;      ///CR-D23937  remove at 12.12.07
  DefaultExpressionOldBehavior: boolean = False;
  LocateExOldBehavior: boolean = False;
  RefreshParamsOnInsert: boolean = False; // Old behavior

implementation

uses
  Math,
{$IFDEF VER6P}
  DateUtils, FmtBcd,
{$ENDIF}
{$IFNDEF FPC}
  DBConsts,
{$ELSE}
  DBConst,
{$ENDIF}
  DAConsts;

const
  DataTypeMap: array [TFieldType] of word = (
    // ftUnknown, ftString, ftSmallint, ftInteger, ftWord
    dtUnknown, dtString, dtInt16, dtInteger, dtUInt16,
    // ftBoolean, ftFloat, ftCurrency, ftBCD, ftDate, ftTime, ftDateTime,
    dtBoolean, dtFloat, dtCurrency, dtBCD, dtDate, dtTime, dtDateTime,
    // ftBytes, ftVarBytes, ftAutoInc, ftBlob, ftMemo, ftGraphic, ftFmtMemo,
    dtBytes, dtVarBytes, dtInteger, dtBlob, dtMemo, 0, 0,
    // ftParadoxOle, ftDBaseOle, ftTypedBinary, ftCursor, ftFixedChar, ftWideString,
    0, 0, 0, dtCursor, dtString, dtWideString,
    // ftLargeint, ftADT, ftArray, ftReference, ftDataSet, ftOraBlob, ftOraClob,
    dtInt64, dtObject, dtArray, dtReference, dtTable, 0, 0,
    // ftVariant, ftInterface, ftIDispatch, ftGuid
    dtVariant, 0, 0, dtGuid
  {$IFDEF VER6P}
    // ftTimeStamp, ftFMTBcd
    , 0, {$IFNDEF FPC}dtFmtBCD{$ELSE}0{$ENDIF}
  {$IFDEF FPC}
    // ftFixedWideChar, ftWideMemo
    , dtWideString, dtWideMemo
  {$ENDIF}
  {$IFDEF VER10P}
    // ftFixedWideChar, ftWideMemo, ftOraTimeStamp, ftOraInterval
    , dtWideString, dtWideMemo, 0, 0
  {$IFDEF VER12P}
    // ftLongWord, ftShortint, ftByte, ftExtended, ftConnection, ftParams, ftStream
    , dtUInt32, dtInt8, dtInt16, dtFloat, 0, 0, 0
  {$ENDIF}
  {$IFDEF VER14P}
    // ftTimeStampOffset, ftObject, ftSingle
    , 0, 0, dtFloat
  {$ENDIF}
  {$ENDIF}
  {$ENDIF}
    );

{$IFDEF CLR}
{ PRecInfo }

function PRecInfo.GetRecordNumber: longint;
begin
  Result := Marshal.ReadInt32(Ptr);
end;

procedure PRecInfo.SetRecordNumber(const Value: longint);
begin
  Marshal.WriteInt32(Ptr, Value);
end;

function PRecInfo.GetUpdateStatus: TUpdateStatus;
begin
  Result := TUpdateStatus(Marshal.ReadByte(Ptr, sizeof(longint)));
end;

procedure PRecInfo.SetUpdateStatus(const Value: TUpdateStatus);
begin
  Marshal.WriteByte(Ptr, sizeof(longint), byte(Value));
end;

function PRecInfo.GetBookmarkFlag: TBookmarkFlag;
begin
  Result := TBookmarkFlag(Marshal.ReadByte(Ptr, sizeof(integer) + sizeof(TUpdateStatus)));
end;

procedure PRecInfo.SetBookmarkFlag(const Value: TBookmarkFlag);
begin
  Marshal.WriteByte(Ptr, sizeof(integer) + sizeof(TUpdateStatus), byte(Value));
end;

function PRecInfo.GetRefComplexFields: boolean;
begin
  Result := boolean(Marshal.ReadByte(Ptr, sizeof(integer) + sizeof(TUpdateStatus)
   + sizeof(TBookmarkFlag)));
end;

procedure PRecInfo.SetRefComplexFields(const Value: boolean);
begin
  Marshal.WriteByte(Ptr, sizeof(integer) + sizeof(TUpdateStatus)
   + sizeof(TBookmarkFlag), byte(Value));
end;

class operator PRecInfo.Implicit(AValue: IntPtr): PRecInfo;
begin
  Result.Ptr := AValue;
end;

{$ENDIF}

class function TDataTypesMap.GetFieldType(DataType: Word): TFieldType;
begin
  case DataType of
    dtUnknown:
      Result := ftUnknown;
    dtString:
      Result := ftString;
    dtWideString:
      Result := ftWideString;
    dtInt8:
      Result := ftSmallint;
    dtInt16:
      Result := ftSmallint;
    dtWord:
      Result := ftWord;
    dtInteger:
      Result := ftInteger;
    dtUInt32:
      Result := ftLargeInt;
    dtLargeint:
      Result := ftLargeInt;
    dtFloat:
      Result := ftFloat;
    dtDate:
      Result := ftDate;
    dtTime:
      Result := ftTime;
    dtDateTime:
      Result := ftDateTime;
    dtMemo:
      Result := ftMemo;
    dtWideMemo:
      Result := {$IFDEF VER10P}ftWideMemo;{$ELSE}ftMemo;{$ENDIF}
    dtBlob:
      Result := ftBlob;
    dtObject:
      Result := ftADT;
    dtReference:
      Result := ftReference;
    dtArray:
      Result := ftArray;
    dtTable:
      Result := ftDataSet;
    dtCursor:
      Result := ftCursor;
    dtBoolean:
      Result := ftBoolean;
  {$IFDEF VER5P}
    dtVariant:
      Result := ftVariant;
  {$ENDIF}
    dtExtString:
      Result := ftString;
    dtExtWideString:
      Result := ftWideString;
    dtBytes:
      Result := ftBytes;
    dtVarBytes:
      Result := ftVarBytes;
    dtExtVarBytes:
      Result := ftVarBytes;
    dtBCD:
      Result := ftBCD;
  {$IFDEF VER6P}
  {$IFNDEF FPC}
    dtFmtBCD:
      Result := ftFMTBcd;
  {$ENDIF}
  {$ENDIF}
    dtGuid:
      Result := ftGuid;
    dtCurrency:
      Result := ftCurrency;
  else
    Assert(False, SUnknownDataType);
    Result := ftUnknown;
  end;
end;

class function TDataTypesMap.GetDataType(FieldType: TFieldType): integer;
begin
{$IFNDEF VER10P}
{$IFNDEF FPC}
  if Integer(FieldType) = ftFixedWideChar then
    Result := dtWideString
  else
{$ENDIF}
{$ENDIF}
    Result := DataTypeMap[FieldType];
end;

{ TDADetailDataLink }

constructor TDADetailDataLink.Create(DataSet: TMemDataSet);
begin
  inherited Create;

  FDataSet := DataSet;
end;

procedure TDADetailDataLink.ActiveChanged;
begin
  if FDataSet.Active and
    not (csDestroying in FDataSet.ComponentState) and
    Active
  then
    FDataSet.MasterRecordChanged;
end;

procedure TDADetailDataLink.RecordChanged(Field: TField);
begin
  if ((Field = nil) or (DataSet.Fields.IndexOf(Field) >= 0)) and
    FDataSet.Active and
    not ((Field <> nil) and ((FDataSet.State in [dsEdit, dsInsert]) or
    ((DataSet.State in [dsInsert]) and not RefreshParamsOnInsert) )) then
      FDataSet.MasterRecordChanged;
end;

procedure TDADetailDataLink.CheckBrowseMode;
begin
  if FDataSet.Active and
    not((DataSet.State in [dsInsert]) and (FDataSet.State in [dsEdit,dsInsert]))
  then // Prevent post detail before post master
    FDataSet.CheckBrowseMode;
end;

function TDADetailDataLink.GetDetailDataSet: TDataSet;
begin
  Result := FDataSet;
end;

{ TMemDataSet }

constructor TMemDataSet.Create(aOwner:TComponent);
begin
  inherited Create(aOwner);

  FUpdateRecordTypes := [rtModified, rtInserted, rtUnmodified];
  FLocalConstraints := True;
  FCreateCalcFieldDescs := True;

  FDataLink := TDADetailDataLink.Create(Self);
end;

destructor TMemDataSet.Destroy;
begin
{$IFDEF MSWINDOWS}
  FDetailRefreshTimer.Free;
{$ENDIF}

  FDataLink.Free;

  inherited;

  UnPrepare;

  FreeIRecordSet;
  FreeDataSetService;
end;

{$IFDEF CLR}
function TMemDataSet.BookmarkAvailable: Boolean;
begin
  Result := inherited BookmarkAvailable;
end;
{$ENDIF}

procedure TMemDataSet.SetModified(Value: Boolean);
begin
  inherited;
end;

function TMemDataSet.SetTempState(const Value: TDataSetState): TDataSetState;
begin
  Result := inherited SetTempState(Value);
end;

procedure TMemDataSet.RestoreState(const Value: TDataSetState);
begin
  inherited;
end;

procedure TMemDataSet.DoBeforeRefresh;
begin
  inherited;
end;

procedure TMemDataSet.DoAfterRefresh;
begin
  inherited;
end;

procedure TMemDataSet.CreateIRecordSet;
begin
  SetIRecordSet(TMemData.Create);
end;

procedure TMemDataSet.FreeIRecordSet;
begin
  Data.Free;
end;

procedure TMemDataSet.SetIRecordSet(Value: TData);
begin
  Data := Value;

  if Data <> nil then begin
    Data.CachedUpdates := FCachedUpdates;
    Data.LocalUpdate := FLocalUpdate;
    Data.SparseArrays := SparseArrays;
    Data.FilterItemTypes := ConvertUpdateRecordTypes(FUpdateRecordTypes);

    TMemData(Data).SetIndexFieldNames(FIndexFieldNames);

    Data.OnAppend := DoPerformAppend;
    Data.OnDelete := DoPerformDelete;
    Data.OnUpdate := DoPerformUpdate;
    Data.OnApplyRecord := DoApplyRecord;
    Data.OnCacheChanged := DoCacheChanged;
    Data.OnCacheApplied := DoCacheApplied;
    Data.OnCacheCanceled := DoCacheCanceled;
    if FCreateCalcFieldDescs then begin
      TMemData(Data).OnGetCachedFields := DoGetCachedFields;
      TMemData(Data).OnGetCachedBuffer := DoGetCachedBuffer;
    end;
  end;
  SetLocalDetailFilter;
end;

function TMemDataSet.GetDataSetServiceClass: TDataSetServiceClass;
begin
  Result := TDataSetService;
end;

procedure TMemDataSet.CreateDataSetService;
begin
  SetDataSetService(GetDataSetServiceClass.Create(Self));
end;

procedure TMemDataSet.FreeDataSetService;
begin
  FDataSetService.Free;
  FDataSetService := nil;
end;

procedure TMemDataSet.SetDataSetService(Value: TDataSetService);
begin
  FreeDataSetService;

  FDataSetService := Value;
end;

procedure TMemDataSet.CheckDataSetService;
begin
  if not (FDataSetService is GetDataSetServiceClass) then begin
    FreeDataSetService;
    CreateDataSetService;
  end;
end;

{ Open/Close DataSet }

procedure TMemDataSet.Prepare;
begin
  if (not Prepared) and (Data <> nil) then begin
    Data.Prepare;

    if FDataSetService <> nil then
      FDataSetService.PreInitCursor;
    CreateFieldDefs;
  end;
end;

procedure TMemDataSet.UnPrepare;
begin
  if Active then
    Close;

  if Data <> nil then
    Data.UnPrepare;

  if not (csDestroying in ComponentState) then // This line may be called after destroing FieldDefs. For details see TMemDataSet.Destroy
    FieldDefs.Updated := False;
end;

procedure TMemDataSet.CheckPrepared;
begin
  if not Prepared then
    DatabaseError(SDataSetIsNotPrepared);
end;

{$IFDEF FPC}
procedure TMemDataSet.CreateFields;
var
  Field: TField;
  i: longint;
begin
  for i := 0 to FieldDefs.Count-1 do
    with FieldDefs[i] do begin
      if (DataType <> ftUnknown) and
        not ((faHiddenCol in Attributes) and not FieldDefs.HiddenFields) then
        Field := CreateField(Self);

      if ((faFixed in Attributes) or (DataType = ftFixedChar)) and (Field <> nil) and (Field is TStringField) then
        TStringField(Field).FixedChar := True;
    end;
end;
{$ENDIF}

procedure TMemDataSet.InternalOpen;
var
  Field: TField;
  i: integer;
begin
  Assert(Data <> nil);
  Data.Open;

  // FDataSetService is not set in TVirtualTable and TMetaData
  if FDataSetService <> nil then
    FDataSetService.PreInitCursor;

  CreateFieldDefs;

  // Update FieldDefs once to avoid multiple Update calls when working with FieldDefsList
  // (Perfomance optimization)
//  FieldDefs.Updated := False;
//  FieldDefs.Update;

  if DefaultFields then
    CreateFields
  else // Setting actual size
    for i := 0 to FieldDefs.Count - 1 do
      if FieldDefs[i].DataType = ftString then begin
        Field := FindField(FieldDefs[i].Name);
        if (Field <> nil) and (Field.FieldKind = fkData) then begin
          CheckFieldCompatibility(Field, FieldDefs[i]);
          Field.Size := FieldDefs[i].Size;
        end;
      end;

  // Set number specific
  if FNumberRange and (FDataSetService <> nil) then
    for i := 0 to FieldDefs.Count - 1 do
      FDataSetService.SetNumberRange(FieldDefs[i]);

  BindFields(True);

  if (Data.Fields.Count > 0) and (Data.Fields[Data.Fields.Count - 1].FieldDescKind = fdkCached) then
    TMemData(Data).UpdateCachedBuffer(nil, nil);

{$IFNDEF FPC}
  if ObjectView then
    GetObjectTypeNames(Fields);
{$ENDIF}

  if (Data is TMemData) and (TMemData(Data).IndexFields.Count > 0) then
    TMemData(Data).SortItems;

  BookmarkSize := SizeOf(TRecBookmark);

  //FBlobCacheOfs := Data.RecordSize + CalcFieldsSize;
  if not FCreateCalcFieldDescs then
    FRecInfoOfs := Data.RecordSize + CalcFieldsSize
  else
    if FCacheCalcFields then
      FRecInfoOfs := Data.RecordSize
    else
      FRecInfoOfs := Data.RecordSize + Data.CalcRecordSize; //FBlobCacheOfs + BlobFieldCount * SizeOf(Pointer);

  FBookmarkOfs := FRecInfoOfs + SizeOf(TRecInfo);
  FRecBufSize := FBookmarkOfs + BookmarkSize;

{$IFNDEF CLR}
{$IFNDEF VER10P}
  if not FCreateCalcFieldDescs then begin
    FWideStringOfs := FRecBufSize;

    if CalcFieldsSize > 0 then
      for i := 0 to Fields.Count - 1 do
        if (Fields[i].DataType = ftWideString) and (Fields[i].FieldKind in [fkCalculated, fkLookUp]) then
          FRecBufSize := FRecBufSize + (Fields[i].Size + 1) * sizeof(WideChar);
  end;
{$ENDIF}
{$ENDIF}

  FInCacheProcessing := False;

  if Filtered then begin
    ActivateFilters;
    Data.FilterUpdated;
  end;
  if IsMasterDatasetActive and UseLocalMasterDetailFilter then begin
    MDLinksRefreshed;
    Data.FilterUpdated;
  end;
end;

function TMemDataSet.IsCursorOpen: boolean;
begin
  if Data <> nil then
    Result := Data.Active
  else
    Result := False;
end;

procedure TMemDataSet.InternalClose;
var
  i: integer;
begin
  try
    if Data <> nil then begin
      BindFields(False);
      if DefaultFields then
        DestroyFields;

      Data.Close;
    end;
  finally
    for i := 0 to Length(FLocalMDLinks) - 1 do
      if not FLocalMDLinks[i].NativeBuffer then
        Marshal.FreeHGlobal(FLocalMDLinks[i].Buffer);
    SetLength(FLocalMDLinks, 0);
  end;
end;

procedure TMemDataSet.OpenCursor(InfoQuery: boolean);
begin
  inherited;

  if not InfoQuery then begin
    if FOldRecBuf <> nil then begin
      FreeRecordBuffer(FOldRecBuf);
      FOldRecBuf := nil;
    end;

    FOldRecBuf := AllocRecordBuffer;
  end;
  //DataEvent(deDataSetChange, 0); // Notify nested datasets // DEBUG
end;

procedure TMemDataSet.CloseCursor;
var
  Buffer: IntPtr;
  RecInfo: PRecInfo;
begin
{$IFDEF MSWINDOWS}
  if FDetailRefreshTimer <> nil then
    FDetailRefreshTimer.Enabled := False;
{$ENDIF}

  if Data <> nil then begin // Data can be nil if an exception occurs in OpenCursor
    // free complex fields if call Close in dsInsert or dsEdit mode
    // TDataSet.Close doesn't call Cancel
    if Data.HasComplexFields then begin
      if FInInserting then begin
        Buffer := ActiveBuffer;
        RecInfo := PRecInfo(PtrOffset(Buffer, FRecInfoOfs));
        if RecInfo.RefComplexFields then begin
          Data.FreeComplexFields(Buffer, True);
          RecInfo.RefComplexFields := False;
        end;
      end;
      if FInEditing then begin
        Buffer := ActiveBuffer;
        RecInfo := PRecInfo(PtrOffset(Buffer, FRecInfoOfs));
        if RecInfo.RefComplexFields then begin
          Data.FreeComplexFields(Buffer, False); // Blobs isn't created
          RecInfo.RefComplexFields := False;
        end;
      end;

      if FInInserting then
        Data.FreeComplexFields(ActiveBuffer, True);
      if FInEditing then
        Data.FreeComplexFields(ActiveBuffer, False); // Blobs isn't created

      FInInserting := False;
      FInEditing := False;
    end;

    if FOldRecBuf <> nil then begin
      FreeRecordBuffer(FOldRecBuf);
      FOldRecBuf := nil;
    end;
  end;

  try
    inherited;
  finally
    FParentDataSet := nil;
  end;
end;

procedure TMemDataSet.DataReopen;
begin
  Assert(Data <> nil);
  Data.Reopen;
end;

procedure TMemDataSet.InternalRefresh;
begin
  FreeRefBuffers;
  //ClearBuffers; /// CR11512
  DataReopen;
  if (Data is TMemData) and (TMemData(Data).IndexFields.Count > 0) then
    TMemData(Data).SortItems;
end;

procedure TMemDataSet.DoAfterOpen;
var
  i: integer;
begin
  inherited;

  if SendDataSetChangeEventAfterOpen then
  {$IFDEF CLR}
    DataEvent(deDataSetChange, TObject(Integer(0)));
  {$ELSE}
    DataEvent(deDataSetChange, 0);
  {$ENDIF}

{$IFNDEF FPC}
  for i := 0 to NestedDataSets.Count - 1 do
    with TDataSet(NestedDataSets[i]) do
      if Active then
        DataEvent(deParentScroll, {$IFDEF CLR}nil{$ELSE}0{$ENDIF});
{$ENDIF}
end;

procedure TMemDataSet.FreeRefBuffers;
var
  i: integer;
begin
  if FNeedAddRef then
    for i := 0 to BufferCount do
      FreeRefComplexFields(Buffers[i]);
  FreeRefComplexFields(TempBuffer);
end;

procedure TMemDataSet.FreeRefComplexFields(Buffer: TRecordBuffer; WithBlob: boolean);
var
  RecInfo: PRecInfo;
begin
{$IFNDEF FPC}
  Assert(Data <> nil);
{$ELSE}
  if Data = nil then
    Exit;
{$ENDIF}

  RecInfo := PRecInfo(PtrOffset(Buffer, FRecInfoOfs));
  if {$IFDEF FPC}(Buffer <> nil) and (RecInfo <> nil) and {$ENDIF} RecInfo.RefComplexFields then begin
    Data.FreeComplexFields(Buffer, WithBlob);
    RecInfo.RefComplexFields := False;
  end;
end;

{ Field Management }

function TMemDataSet.GetDataTypesMap: TDataTypesMapClass;
begin
  Result := TDataTypesMap;
end;

procedure TMemDataSet.InternalInitFieldDefs;
var
  CheckDefs: boolean;
  i: integer;
  OldFieldNames: array of _string;
  OldFieldCount: integer;

begin
  // can't CreateFieldDefs if FieldDefs.Update(InitFieldDefs)
  Assert(Data <> nil);
  if not Data.Active then begin
    OldFieldCount := Data.Fields.Count;
    CheckDefs := (FieldDefs <> nil) and not FieldDefs.Updated;
    if CheckDefs then begin
      SetLength(OldFieldNames, OldFieldCount);
      for i := 0 to OldFieldCount - 1 do
        OldFieldNames[i] := Data.Fields[i].Name;
    end;

    Data.ExplicitInitFields;

    if OldFieldCount <> Data.Fields.Count then
      FieldDefs.Updated := False
    else
      if CheckDefs then
        for i := 0 to OldFieldCount - 1 do
          if OldFieldNames[i] <> Data.Fields[i].Name then begin
            FieldDefs.Updated := False;
            Break;
          end;

    if FDataSetService <> nil then
      FDataSetService.PreInitCursor;

    CreateFieldDefs;
  end;
end;

function TMemDataSet.GetFieldType(DataType: word): TFieldType;
begin
  Result := GetDataTypesMap.GetFieldType(DataType);
end;

function TMemDataSet.GetFieldType(FieldDesc: TFieldDesc): TFieldType;
begin
  Result := GetFieldType(FieldDesc.DataType);
end;

function TMemDataSet.GetObjectFieldDefName(Parent: TFieldDef; Index: integer; ObjType: TObjectType):_string;
{$IFNDEF FPC}
var
  ParentDef: TFieldDef;
{$ENDIF}
begin
{$IFNDEF FPC}
  if NeedComplexUpdateFieldDefList and (ObjType.DataType = dtArray) then begin
    Result := Parent.Name;

    // if array is property of object
    ParentDef := Parent.ParentDef;
    while (ParentDef <> nil) and (ParentDef.DataType <> ftArray) do begin
      Result := ParentDef.Name + '.' + Result;
      ParentDef := ParentDef.ParentDef;
    end;

    Result := Result + '[' + IntToStr(Index) + ']';
    //To correct DB.pas ADT name handling
  end
  else
{$ENDIF}
    Result := IntToStr(Index);
end;

function TMemDataSet.GetFielDefSize(FieldType: TFieldType; FieldDesc: TFieldDesc): integer;
begin
  Result := 0;
  case FieldType of
    ftString, ftWideString{$IFDEF VER5P},ftGuid{$ENDIF}: begin
      Result := FieldDesc.Length;
      if Result = 0 then
        Result := 1;  // For SELECT NULL FROM ...
    end;
    ftBytes, ftVarBytes:
      Result := FieldDesc.Length;
  end;
end;

function TMemDataSet.NeedCreateFieldDefs: boolean;
begin
  Result := not FieldDefs.Updated or Data.FieldListDependsOnParams;
end;

procedure TMemDataSet.CreateFieldDefs;

  function GetFieldName(ParentFieldName, FieldName: _string; Index: integer): _string;
  var
    NewFieldName: _string;
  begin
    if Index = 0 then
      NewFieldName := FieldName
    else
      NewFieldName := FieldName + '_' + IntToStr(Index);

    if (FieldDefs.IndexOf(ParentFieldName + '.' + NewFieldName) <> -1) then
      Result := GetFieldName(ParentFieldName, FieldName, Index + 1)
    else
      Result := NewFieldName;
  end;

  procedure CreateObjectFields(ObjType: TObjectType; Parent: TFieldDef);
  var
    i: integer;
    FieldDef: TFieldDef;
    FieldType: TFieldType;
    aSize: word;
    Item,CountItem: integer;
    FieldName: _string;
  begin
    if (ObjType.DataType = dtObject) or SparseArrays then
      CountItem := 1
    else begin
      CountItem := ObjType.Size;
      if CountItem > MaxArrayItem then // Restriction of array length
        CountItem := MaxArrayItem;
    end;

    for i := 0 to ObjType.AttributeCount - 1 do begin
      for Item := 0 to CountItem - 1 do begin
        with ObjType.Attributes[i] do begin
          FieldType := GetFieldType(DataType);

          aSize := 0;
          case FieldType of
            ftString, ftWideString:
              aSize := Length;
          end;

          FieldDef := TFieldDef.Create(Parent.{$IFNDEF FPC}ChildDefs{$ELSE}Collection{$ENDIF});
          if ObjType.DataType = dtObject then
            FieldName := Name
          else
            FieldName := GetObjectFieldDefName(Parent, Item, ObjType);
          FieldDef.Name := GetFieldName(Parent.Name, FieldName, 0);

          FieldDef.DataType := FieldType;
          FieldDef.Size := aSize;
          FieldDef.Required := False;
          if DB.faReadonly in Parent.Attributes then
            FieldDef.Attributes := FieldDef.Attributes + [DB.faReadonly];

          if FieldType in [ftADT,ftArray] then
            CreateObjectFields(ObjectType, FieldDef);
        end;
      end;
    end;
  end;

var
  FieldType: TFieldType;
  aSize: word;
  i: integer;
  FieldDef: TFieldDef;
begin
  Assert(Data <> nil);
  if not NeedCreateFieldDefs then Exit;
  FieldDefs.BeginUpdate;
  try
    FieldDefs.Clear;
    for i := 0 to Data.FieldCount - 1 do begin
      with Data.Fields[i] do
        if not HiddenObject and (FieldDescKind = fdkData) and (not HasParent or ParentField.HiddenObject) then begin
        // FieldNo 1..
          FieldType := GetFieldType(Data.Fields[i]);
          aSize := GetFielDefSize(FieldType, Data.Fields[i]);
          if FieldType <> ftUnknown then begin
            FieldDef := TFieldDef.Create(FieldDefs, Name, FieldType, aSize,
              Required and FLocalConstraints, i + 1);

            if ReadOnly then
              FieldDef.Attributes := FieldDef.Attributes + [DB.faReadonly];

            if FieldType in [ftFloat, ftInteger, ftLargeint, ftSmallint] then
              FieldDef.Precision := Length
            else
            if FieldType in [ftBCD{$IFDEF VER6P}, ftFMTBCD{$ENDIF}] then begin
              FieldDef.Precision := Length;
              FieldDef.Size := Scale;
            end
            else
              if FieldType in [ftADT, ftArray] then
                CreateObjectFields(ObjectType, FieldDef);

            if Hidden then
              FieldDef.Attributes := FieldDef.Attributes + [DB.faHiddenCol];
          {$IFDEF VER5P}
            if Fixed then
              FieldDef.Attributes := FieldDef.Attributes + [DB.faFixed];
          {$ENDIF}
          end
          else
            DatabaseError(SDataTypeNotSupported, Self);
        end;
    end;
  finally
    FieldDefs.EndUpdate;
  end;
{$IFNDEF FPC}
  UpdateFieldDefList;
{$ENDIF}
  FieldDefs.Updated := Data.Fields.Count <> 0; // bug with prepare method that does not get fields fixed
end;

{$IFNDEF FPC}
procedure TMemDataSet.UpdateFieldDefList;

  procedure BeforeUpdateFieldDefList(FieldDefs: TFieldDefs);
  var
    i: integer;
  begin
    // Update FieldDefs before FieldDefList updating
    for i := 0 to FieldDefs.Count - 1 do begin
      if FieldDefs[i].DataType = ftArray then begin
        FieldDefs[i].DataType := ftADT;
        FieldDefs[i].Size := 0;
        FieldDefs[i].Attributes := FieldDefs[i].Attributes + [faUnNamed];
      end;

      if FieldDefs[i].HasChildDefs then
        BeforeUpdateFieldDefList(FieldDefs[i].ChildDefs);
    end;
  end;

  procedure AfterUpdateFieldDefList(FieldDefs: TFieldDefs);
  var
    i: integer;
  begin
    // Update FieldDefs after FieldDefList updating
    for i := 0 to FieldDefs.Count - 1 do begin
      if (FieldDefs[i].DataType = ftADT) and (faUnNamed in FieldDefs[i].Attributes) then begin
        FieldDefs[i].DataType := ftArray;
        FieldDefs[i].Size := 0;
        FieldDefs[i].Attributes := FieldDefs[i].Attributes - [faUnNamed];
      end;

      if FieldDefs[i].HasChildDefs then
        AfterUpdateFieldDefList(FieldDefs[i].ChildDefs);
    end;
  end;

begin
  if NeedComplexUpdateFieldDefList then
    BeforeUpdateFieldDefList(FieldDefs);

  FieldDefList.Update;

  if NeedComplexUpdateFieldDefList then
    AfterUpdateFieldDefList(FieldDefs);
end;
{$ENDIF}

function TMemDataSet.NeedComplexUpdateFieldDefList;
begin
  Result := false;
end;

{$IFNDEF FPC}
procedure TMemDataSet.GetObjectTypeNames(Fields: TFields);
var
  i: integer;
  ObjectField: TObjectField;
begin
  for i := 0 to Fields.Count - 1 do
    if Fields[i] is TObjectField then begin
      ObjectField := TObjectField(Fields[i]);

      ObjectField.ObjectType := Data.Fields[Fields[i].FieldNo - 1].ObjectType.Name;

      with ObjectField do
        if DataType in [ftADT, ftArray] then begin
          if (DataType = ftArray) and SparseArrays and
           (Fields[0].DataType = ftADT)
          then
            GetObjectTypeNames(TObjectField(Fields[0]).Fields)
          else
            GetObjectTypeNames(Fields);
        end;
    end
end;
{$ENDIF}

function TMemDataSet.GetFieldDescNo(Field: TField): integer;
var
  FieldDesc: TFieldDesc;
begin
  FieldDesc := GetFieldDesc(Field);
  if FieldDesc <> nil then
    Result := FieldDesc.FieldNo
  else
    raise Exception.Create(Format(SFieldNotFound, [Field.FieldName]));
end;

function TMemDataSet.GetFieldDesc(const Field: TField): TFieldDesc;
var
  FieldDesc: TFieldDesc;
  i: integer;
  Found: boolean;
begin
  Assert(Data <> nil, 'FIRecordSet must be setted to this time');
  Assert(Field <> nil, 'Field cannot be nil');
  Assert((Field.DataSet = Self) or (Field.DataSet = nil {CR 22356}), 'Wrong DataSet');
  {if Field.DataSet <> Self then
    for i := 0 to Data.FieldCount - 1 do
      if _LowerCase(Field.FieldName) = _LowerCase(Data.Fields[i].Name) then begin /// Field.FieldName must be equal with FieldDesc.Name
        Result := Data.Fields[i];
        Exit;
      end;}

  Result := nil;
  if Field.FieldNo > 0 then
    Result := TFieldDesc(Data.Fields[Field.FieldNo - 1])
  else
  begin
    if DefaultFields and (Fields.Count = Data.Fields.Count {just in case; should be moved to Assert}) then begin
      i := Fields.IndexOf(Field);
      Result := Data.Fields[i];
    end
    else
      for i := 0 to Data.FieldCount - 1 do begin
        FieldDesc := Data.Fields[i];
        Found := _SameText(Field.FieldName, FieldDesc.Name); /// Field.FieldName must be equal with FieldDesc.Name
        if (Field.FieldKind <> fkData) then begin
          if FieldDesc.FieldDescKind = fdkData then
            Found := False
          else
          if not Found then
            Found := _SameText(Field.FieldName, FieldDesc.ActualName);
        end;
        if Found then begin
          Result := FieldDesc;
          Break;
        end;
      end;
  end;
  // Assert(_LowerCase(Field.FieldName) = _LowerCase(Result.Name), Format('Field.FieldName <> FieldDesc.Name' + LineSeparator + '"%s" <> "%s"' + LineSeparator, [Field.FieldName, Result.Name]));
end;

function TMemDataSet.GetFieldData(Field: TField; Buffer: TValueBuffer): boolean;
var
  IsBlank: boolean;
  RecBuf: TRecordBuffer;
  FieldBuf: IntPtr;
{$IFNDEF CLR}
{$IFNDEF VER10P}
  DataOffset: LongInt;
{$ENDIF}  
{$ENDIF}
  i: integer;
  FieldDesc: TFieldDesc;
begin
  Result := False;
  if not GetActiveRecBuf(RecBuf) then
    Exit;

  Assert(Data <> nil);
  with Field do
    if FieldNo > 0 then begin
      Data.GetField(FieldNo, RecBuf, Buffer, IsBlank);
      Result := not IsBlank;
    end
    else
      if State in  [dsBrowse, dsEdit, dsInsert, dsCalcFields, dsFilter, dsBlockRead] then
        if FCreateCalcFieldDescs then begin
          FieldDesc := nil;
          for i := 0 to Length(FCalcFieldsMapping) - 1 do
            if FCalcFieldsMapping[i].Field = Field then begin
              FieldDesc := FCalcFieldsMapping[i].FieldDesc;
              break;
            end;
          if FieldDesc <> nil then
            Data.GetField(FieldDesc.FieldNo, RecBuf, Buffer, IsBlank)
          else
            Data.GetField(GetFieldDescNo(Field), RecBuf, Buffer, IsBlank);
          Result := not IsBlank;
        end
        else begin
          FieldBuf := PtrOffset(RecBuf, RecordSize + Offset);
          Result := Boolean(Marshal.ReadByte(FieldBuf));
          if Result and (Buffer <> nil) then
          {$IFNDEF CLR}
          {$IFNDEF VER10P}
            if Field.DataType = ftWideString then begin
              DataOffset := Integer(PtrOffset(FieldBuf, 1)^);
              CopyBuffer(PtrOffset(RecBuf, FWideStringOfs + DataOffset), Buffer, (Size + 1) * SizeOf(WideChar));
            end
            else
          {$ENDIF}
          {$ENDIF}
              CopyBuffer(PtrOffset(FieldBuf, 1), Buffer, DataSize);
        end;
end;

{$IFNDEF FPC}
function TMemDataSet.GetFieldData(FieldNo: integer; Buffer: TValueBuffer): boolean;
var
  IsBlank: boolean;
  RecBuf: TRecordBuffer;
begin
//if BlockReadSize > 0 then

  Result := GetActiveRecBuf(RecBuf);
  if Result then begin
    Assert(Data <> nil);
    Data.GetField(FieldNo, RecBuf, Buffer, IsBlank);
    Result := not IsBlank;
  end
end;
{$ENDIF}

procedure TMemDataSet.SetFieldData(Field: TField; Buffer: TValueBuffer);
var
  RecBuf: TRecordBuffer;
  FieldBuf: IntPtr;
{$IFNDEF CLR}
{$IFNDEF VER10P}
  DataOffset, WideStringSize: integer;
{$ENDIF}
{$ENDIF}
  FieldDesc: TFieldDesc;
  i: integer;
begin
  with Field do begin
    if not (State in dsWriteModes) then
      DatabaseError(SNotEditing);
    Assert(Data <> nil);         // must be after state check if fields was created in DT, but SQL removed
    GetActiveRecBuf(RecBuf);
    if FieldNo > 0 then begin
      if State = dsCalcFields then
        DatabaseError(SNotEditing);
      if ReadOnly and not ((State in [dsSetKey, dsFilter]) or FInSettingDefaultExpressionValues) then
        DatabaseErrorFmt(SFieldReadOnly, [DisplayName]);
      Validate(Buffer);
      if FieldKind <> fkInternalCalc then begin
        Data.PutField(FieldNo, RecBuf, Buffer);
      end;
    end
    else
      if FCreateCalcFieldDescs then begin
        FieldDesc := nil;
        for i := 0 to Length(FCalcFieldsMapping) - 1 do
          if FCalcFieldsMapping[i].Field = Field then begin
            FieldDesc := FCalcFieldsMapping[i].FieldDesc;
            break;
          end;
        if FieldDesc <> nil then
          Data.PutField(FieldDesc.FieldNo, RecBuf, Buffer)
        else
          Data.PutField(GetFieldDescNo(Field), RecBuf, Buffer);
      end
      else begin
        FieldBuf := PtrOffset(RecBuf, RecordSize + Offset);
        Marshal.WriteByte(FieldBuf, Byte(IntPtr(Buffer) <> nil));
        if IntPtr(Buffer) <> nil then
        {$IFNDEF CLR}
        {$IFNDEF VER10P}
          if Field.DataType = ftWideString then begin
            WideStringSize := Length(WideString(Buffer^)) * Sizeof(WideChar);
            DataOffset := Integer(PtrOffset(FieldBuf, 1)^);

            CopyBuffer(IntPtr(Buffer^), PtrOffset(RecBuf, FWideStringOfs + DataOffset), WideStringSize);
            Marshal.WriteInt16(PtrOffset(RecBuf, FWideStringOfs + DataOffset + WideStringSize), 0);
          end
          else
        {$ENDIF}
        {$ENDIF}
            CopyBuffer(Buffer, PtrOffset(FieldBuf, 1), DataSize);
      end;

    if not (State in [dsCalcFields, dsFilter, dsNewValue]) then
    {$IFDEF CLR}
      DataEvent(deFieldChange, Field);
    {$ELSE} {$IFDEF FPC}
      DataEvent(deFieldChange, PtrInt(Field));
    {$ELSE} {$IFDEF VER16P} 
      DataEvent(deFieldChange, NativeInt(Field));
    {$ELSE}
      DataEvent(deFieldChange, Longint(Field));
    {$ENDIF} {$ENDIF} {$ENDIF}
  end;
end;

function TMemDataSet.GetFieldData(Field: TField; Buffer: TValueBuffer;
  NativeFormat: Boolean): Boolean;
{$IFNDEF CLR}
{$IFNDEF VER10P}
var
  Temp: PWideChar;
{$ENDIF}
{$ENDIF}
begin
  if (Field.DataType = ftWideString) and (Buffer <> nil) then begin
  {$IFDEF CLR}
    Result := inherited GetFieldData(Field, Buffer, True);
  {$ELSE}
    {$IFDEF VER10P}
      Result := inherited GetFieldData(Field, Buffer, True);
    {$ELSE}
    {$IFDEF FPC}
      Result := inherited GetFieldData(Field, Buffer, True);
    {$ELSE}
      { Cannot copy direct - may be conflict with Delphi string manager
      SetLength(WideString(Buffer^), Field.Size * sizeof(WideChar));
      Result := inherited GetFieldData(Field, PWideChar(WideString(Buffer^)), True);}
      GetMem(Temp, (Field.Size + 1 {#0} + 8) * sizeof(WideChar));//+ 8 for numbers
      try
        Result := inherited GetFieldData(Field, Temp, True);
        if Result then
          WideString(Buffer^) := Temp
        else
          WideString(Buffer^) := '';
      finally
        FreeMem(Temp);
      end;
    {$ENDIF}
    {$ENDIF}
  {$ENDIF}
  end
  else
    Result := inherited GetFieldData(Field, Buffer, NativeFormat);
end;

procedure TMemDataSet.SetFieldData(Field: TField; Buffer: TValueBuffer; NativeFormat: Boolean);
begin
  if Field is TWideStringField then begin
    //Assert(Buffer <> nil);
    inherited SetFieldData(Field, Buffer, True);
  end
  else
    inherited SetFieldData(Field, Buffer, NativeFormat);
end;

{$IFNDEF FPC}
function TMemDataSet.GetStateFieldValue(State: TDataSetState; Field: TField): Variant;
begin
  if ((Self.State = dsInsert) and (State = dsOldValue)) or (State = dsInsert) then
    Result := NULL
  else
    Result := inherited GetStateFieldValue(State, Field);
end;
{$ENDIF}

procedure TMemDataSet.DataConvert(Field: TField; Source, Dest: TValueBuffer; ToNative: Boolean);
{$IFDEF CLR}
var
  TimeStamp: TTimeStamp;
  TempDouble: Double;
{$ENDIF}
begin
  case Field.DataType of
  {$IFDEF CLR}
    ftDate:
      if ToNative then
      begin
        TimeStamp := DateTimeToTimeStamp(
          BitConverter.Int64BitsToDouble(Marshal.ReadInt64(Source)));
        Marshal.WriteInt32(Dest, TimeStamp.Date);
      end
      else
      begin
        TimeStamp.Time := 0;
        TimeStamp.Date := Marshal.ReadInt32(Source);
        Marshal.WriteInt64(Dest,
          BitConverter.DoubleToInt64Bits(MemUtils.TimeStampToDateTime(TimeStamp)));
      end;
    ftTime:
      if ToNative then
      begin
        TimeStamp := DateTimeToTimeStamp(
          BitConverter.Int64BitsToDouble(Marshal.ReadInt64(Source)));
        Marshal.WriteInt32(Dest, TimeStamp.Time);
      end
      else
      begin
        TimeStamp.Time := Marshal.ReadInt32(Source);
        TimeStamp.Date := DateDelta;
        Marshal.WriteInt64(Dest,
          BitConverter.DoubleToInt64Bits(MemUtils.TimeStampToDateTime(TimeStamp)));
      end;
    ftDateTime:
      if ToNative then
      begin
        TempDouble := TimeStampToMSecs(DateTimeToTimeStamp(
          BitConverter.Int64BitsToDouble(Marshal.ReadInt64(Source))));
        Marshal.WriteInt64(Dest, BitConverter.DoubleToInt64Bits(TempDouble));
      end
      else
      begin
        TimeStamp := MSecsToTimeStamp(
          Trunc(BitConverter.Int64BitsToDouble(Marshal.ReadInt64(Source))));
        Marshal.WriteInt64(Dest,
          BitConverter.DoubleToInt64Bits(MemUtils.TimeStampToDateTime(TimeStamp)));
      end;
{$ENDIF}
    ftBCD:
      Marshal.WriteInt64(Dest, Marshal.ReadInt64(Source));
  else
    inherited DataConvert(Field, Source, Dest, ToNative);
  end;
end;

function TMemDataSet.GetSparseArrays: boolean;
begin
{$IFNDEF FPC}
  Result := inherited SparseArrays;
{$ELSE}
  Result := False;
{$ENDIF}
end;

procedure TMemDataSet.SetSparseArrays(Value: boolean);
begin
{$IFNDEF FPC}
  if SparseArrays <> Value then begin
    UnPrepare;

    inherited SparseArrays := Value;

    if Data <> nil then
      Data.SparseArrays := Value;
  end;
{$ENDIF}
end;

procedure TMemDataSet.CheckFieldCompatibility(Field: TField; FieldDef: TFieldDef);
{$IFDEF FPC}
const
  BaseFieldTypes: array[TFieldType] of TFieldType = (
    ftUnknown, ftString, ftInteger, ftInteger, ftInteger, ftBoolean, ftFloat,
    ftFloat, ftBCD, ftDateTime, ftDateTime, ftDateTime, ftBytes, ftVarBytes,
    ftInteger, ftBlob, ftBlob, ftBlob, ftBlob, ftBlob, ftBlob, ftBlob, ftUnknown,
    ftString, ftUnknown, ftLargeInt, ftADT, ftArray, ftReference, ftDataSet,
    ftBlob, ftBlob, ftVariant, ftInterface, ftInterface, ftString, ftTimeStamp, ftFMTBcd,
    ftFixedWideChar, ftWideMemo);

  CheckTypeSizes = [ftBytes, ftVarBytes, ftBCD, ftReference];
{$ENDIF}
begin
{$IFDEF FPC}
  with Field do
  begin
    if (BaseFieldTypes[DataType] <> BaseFieldTypes[FieldDef.DataType]) then
      DatabaseErrorFmt(SFieldTypeMismatch, [DisplayName,
        FieldTypeNames[DataType], FieldTypeNames[FieldDef.DataType]], Self);
    if (DataType in CheckTypeSizes) and (Size <> FieldDef.Size) then
        DatabaseErrorFmt(SFieldSizeMismatch, [DisplayName, Size,
          FieldDef.Size], Self);
  end;

{$ELSE}
  inherited;
{$ENDIF}
end;

function TMemDataSet.GetFieldClass(FieldType: TFieldType): TFieldClass;
begin
  CheckDataSetService;
  Result := FDataSetService.GetFieldClass(FieldType);
end;

function TMemDataSet.InternalGetBlob(FieldDesc: TFieldDesc): TBlob;
var
  RecBuf: TRecordBuffer;
  Ptr: IntPtr;
  IsBlank: boolean;
begin
  Assert(Data <> nil);
  Ptr := Marshal.AllocHGlobal(sizeof(IntPtr));
  try
    if GetActiveRecBuf(RecBuf) then begin
      if not Data.IsBlobFieldType(FieldDesc.DataType) then
        DatabaseError(SNeedBlobType);

      Data.GetField(FieldDesc.FieldNo, RecBuf, Ptr, IsBlank);
      Result := TBlob(GetGCHandleTarget(Marshal.ReadIntPtr(Ptr)));
    end
    else
      Result := nil;
  finally
    Marshal.FreeHGlobal(Ptr);
  end;
end;

function TMemDataSet.GetBlob(const FieldName: _string): TBlob;
var
  FieldDesc: TFieldDesc;
begin
  Assert(Data <> nil);
  FieldDesc := Data.FieldByName(FieldName);
  Result := InternalGetBlob(FieldDesc);
end;

function TMemDataSet.GetBlob(Field: TField): TBlob;
var
  FieldDesc: TFieldDesc;
begin
  if (Field <> nil) and Active then begin
    FieldDesc := GetFieldDesc(Field);
    if FieldDesc <> nil then
      Result := InternalGetBlob(FieldDesc)
    else
      Result := nil;
  end
  else
    Result := nil;
end;

function TMemDataSet.InternalSetBlob(FieldDesc: TFieldDesc; Blob: TBlob): boolean;
var
  OldBlob: TBlob;
  RecBuf: TRecordBuffer;
begin
  Assert(FieldDesc <> nil);
  Assert(Blob <> nil);
  OldBlob := InternalGetBlob(FieldDesc);
  Assert(OldBlob <> nil);
  Result := Blob.ClassType = OldBlob.ClassType;
  if GetActiveRecBuf(RecBuf) then begin
    if not Data.IsBlobFieldType(FieldDesc.DataType) then
      DatabaseError(SNeedBlobType);

    Blob.AddRef;
    OldBlob.Free;
    Marshal.WriteIntPtr(RecBuf, FieldDesc.Offset, Blob.GCHandle);
  end
  else
    Result := False;
end;

function TMemDataSet.SetBlob(Field: TField; Blob: TBlob): boolean;
var
  FieldDesc: TFieldDesc;
begin
  if Field <> nil then begin
    FieldDesc := GetFieldDesc(Field);
    if FieldDesc = nil then
      raise Exception.Create(Format(SFieldNotFound, [Field.FieldName]));
    Result := InternalSetBlob(FieldDesc, Blob);
  end
  else
    Result := False;
end;

{function TMemDataSet.Translate(const Src: string; var Dest: string; boolean): integer;
begin
  inherited Translate(Src, Dest, ToOem);
  Result := StrLen(Src);
{  if ToOem then
    AnsiToNativeBuf(Locale, Src, Dest, Result)
  else
    NativeToAnsiBuf(Locale, Src, Dest, Result);
  if Src <> Dest then
    Dest[Result] := #0;
end;}

{ Buffer/Record Management }

function TMemDataSet.AllocRecordBuffer: TRecordBuffer;
begin
  Result := Marshal.AllocHGlobal(FRecBufSize);
  PRecInfo(PtrOffset(Result, FRecInfoOfs)).RefComplexFields := False;
end;

procedure TMemDataSet.FreeRecordBuffer(var Buffer: TRecordBuffer);
begin
  FreeRefComplexFields(Buffer);
  Marshal.FreeHGlobal(Buffer);
  Buffer := nil;
end;

procedure TMemDataSet.CopyRecordBuffer(SrcBuffer: IntPtr; DstBuffer: IntPtr);
begin
  Assert(Data <> nil);
  Data.FreeComplexFields(DstBuffer, False);
  CopyBuffer(SrcBuffer, DstBuffer, FRecBufSize);
  PRecInfo(PtrOffset(DstBuffer, FRecInfoOfs)).RefComplexFields := True;
  Data.CreateComplexFields(DstBuffer, False);  //copy complex fields
  Data.CopyComplexFields(SrcBuffer, DstBuffer, False);
end;

function TMemDataSet.GetRecord(Buffer: TRecordBuffer; GetMode: TGetMode;
  DoCheck: Boolean): TGetResult;
var
{$IFDEF CLR}
  RecInfo: IntPtr;
{$ELSE}
  RecInfo: PRecInfo;
{$ENDIF}
begin
  Assert(Data <> nil);
  FreeRefComplexFields(Buffer);
  case GetMode of
    gmCurrent:
      Data.GetRecord(Buffer);
    gmNext:
      Data.GetNextRecord(Buffer);
    gmPrior:
      Data.GetPriorRecord(Buffer);
  end;
  if Data.BOF then
    Result := grBOF
  else
    if Data.EOF then
      Result := grEOF
    else begin
    {$IFDEF CLR}
      RecInfo := PtrOffset(Buffer, FRecInfoOfs);
      Marshal.WriteInt32(RecInfo, Data.RecordNo); //RecordNumber
      Marshal.WriteInt16(RecInfo, sizeof(longint), (byte(bfCurrent) shl 8) or byte(Data.GetUpdateStatus)); //UpdateStatus
      if FNeedAddRef then begin
        Marshal.WriteByte(RecInfo, sizeof(integer) + sizeof(TUpdateStatus)
          + sizeof(TBookmarkFlag), byte(True)); //RefComplexFields
        Data.AddRefComplexFields(Buffer);
      end;
    {$ELSE}
      RecInfo := PRecInfo(PtrOffset(Buffer, FRecInfoOfs));
      RecInfo.RecordNumber := Data.RecordNo;
      RecInfo.UpdateStatus := TUpdateStatus(Data.GetUpdateStatus);
      RecInfo.BookmarkFlag := bfCurrent;
      if FNeedAddRef then begin
        RecInfo.RefComplexFields := True;
        Data.AddRefComplexFields(Buffer);
      end;
    {$ENDIF}

      //ClearBlobCache(Buffer);
      if not FCacheCalcFields then
        GetCalcFields(Buffer);
      //SetBookmarkFlag(Buffer, bfCurrent);
      Data.GetBookmark(PRecBookmark(PtrOffset(Buffer, FBookmarkOfs)));

      Result := grOK;
    end;
end;

procedure TMemDataSet.InternalInitRecord(Buffer: TRecordBuffer);
begin
  Assert(Data <> nil);
  //FInDeferredPost := False; moved to InternalInsert
  //FInInserting := True;
  FreeRefComplexFields(Buffer);
  Data.InitRecord(Buffer);
  if Data.HasComplexFields then
    Data.CreateComplexFields(Buffer, True);
end;

procedure TMemDataSet.InitRecord(Buffer: TRecordBuffer);
var
  RecInfo: PRecInfo;
begin
  inherited InitRecord(Buffer);

  Assert(Data <> nil);
  //ClearBlobCache(Buffer);
  RecInfo := PRecInfo(PtrOffset(Buffer, FRecInfoOfs));
  RecInfo.RecordNumber := 0;
  RecInfo.UpdateStatus := TUpdateStatus(usInserted);
  RecInfo.BookMarkFlag := bfInserted;
  Data.InitRecord(FOldRecBuf);  // clear OldRecBuf
end;

function TMemDataSet.GetOldRecBuf: TRecordBuffer;
begin
  if FInDeferredPost then
    Result := OldDeferredPostBuf
  else
  if FInCacheProcessing then
    Result := OldCacheRecBuf
  else
    Result := GetOldRecord;
end;

function TMemDataSet.GetNewRecBuf: TRecordBuffer;
begin
  if FInCacheProcessing then
    Result := NewCacheRecBuf
  else
    Result := ActiveBuffer;
end;

function TMemDataSet.GetActiveRecBuf(var RecBuf: TRecordBuffer): boolean;
begin
  case State of
    dsBlockRead:
      if IsEmpty then
        RecBuf := nil
      else
        RecBuf := ActiveBuffer;
    dsBrowse:
      if FInCacheProcessing then
        RecBuf := NewCacheRecBuf
      else
        if IsEmpty then
          RecBuf := nil
        else
          RecBuf := ActiveBuffer;
    dsEdit,dsInsert:
      RecBuf := ActiveBuffer;
    dsCalcFields:
      RecBuf := CalcBuffer;
    dsFilter:
      RecBuf := FFilterBuffer;
    dsNewValue:
      RecBuf := GetNewRecBuf;
    dsOldValue:
      RecBuf := GetOldRecBuf;
//    dsSetKey: RecBuf := PAnsiChar(FKeyBuffer) + SizeOf(TKeyBuffer);
  else
    RecBuf := nil;
  end;
  Result := RecBuf <> nil;
end;

function TMemDataSet.GetOldRecord: TRecordBuffer;
begin
  Assert(Data <> nil);
  UpdateCursorPos;
  Data.GetOldRecord(FOldRecBuf);
  Result := FOldRecBuf;
end;

procedure TMemDataSet.ClearCalcFields(Buffer: TRecordBuffer);
var
  i: integer;
{$IFNDEF CLR}
{$IFNDEF VER10P}
  DataOffset: integer;
{$ENDIF}
{$ENDIF}
begin
  if FCreateCalcFieldDescs then begin
    for i := 0 to Data.Fields.Count - 1 do
      if Data.Fields[i].FieldDescKind <> fdkData then
        Data.SetNull(i + 1, Buffer, True);
  end
  else begin
    FillChar(PtrOffset(Buffer, RecordSize), CalcFieldsSize, 0);

  {$IFNDEF CLR}
  {$IFNDEF VER10P}
    DataOffset := 0;
    for i := 0 to Fields.Count - 1 do
      if (Fields[i].DataType = ftWideString) and (Fields[i].FieldKind in [fkCalculated, fkLookUp]) then begin
        Marshal.WriteInt32(PtrOffset(Buffer, RecordSize + Fields[i].Offset + 1), DataOffset);
        DataOffset := DataOffset + (Fields[i].Size + 1) * sizeof(WideChar);
      end;
    FillChar(PtrOffset(Buffer, FWideStringOfs), DataOffset, 0);
  {$ENDIF}
  {$ENDIF}
  end;
end;

// WAR don't support BlockRead

{$IFNDEF FPC}
procedure TMemDataSet.SetBlockReadSize(Value: Integer);
begin
  if Value <> BlockReadSize then
    if (Value > 0) or (Value < -1) then begin
      UpdateCursorPos;
      inherited;
    end
    else
      inherited;
end;

procedure TMemDataSet.BlockReadNext;
begin
  inherited;
end;
{$ENDIF}

{$IFDEF FPC}
procedure TMemDataSet.SetBufListSize(Value: Longint);
const
  DefaultBufferCount = 10;
begin
  if (BufferCount = -1) and (Value = DefaultBufferCount) then
    Value := 1;

  inherited SetBufListSize(Value);
end;
{$ENDIF}

{ Bookmarks }

function TMemDataSet.GetBookmarkFlag(Buffer: TRecordBuffer): TBookmarkFlag;
begin
  Result := PRecInfo(PtrOffset(Buffer, FRecInfoOfs)).BookmarkFlag;
end;

procedure TMemDataSet.SetBookmarkFlag(Buffer: TRecordBuffer; Value: TBookmarkFlag);
begin
  PRecInfo(PtrOffset(Buffer, FRecInfoOfs)).BookmarkFlag := Value
end;

// Data - pointer to bookmark
procedure TMemDataSet.GetBookmarkData(Buffer: TRecordBuffer;
  {$IFDEF CLR}var Bookmark: TBookmark{$ELSE}Bookmark: Pointer{$ENDIF});
begin
  CopyBuffer(PtrOffset(Buffer, FBookmarkOfs), Bookmark, BookmarkSize);
end;

procedure TMemDataSet.SetBookmarkData(Buffer: TRecordBuffer;
  {$IFDEF CLR}const Bookmark: TBookmark{$ELSE}Bookmark: Pointer{$ENDIF});
begin
  CopyBuffer(Bookmark, PtrOffset(Buffer, FBookmarkOfs), BookmarkSize);
end;

procedure TMemDataSet.InternalGotoBookmark({$IFDEF CLR}const Bookmark: TBookmark{$ELSE}Bookmark: Pointer{$ENDIF});
begin
  Assert(Data <> nil);
  Data.SetToBookMark(PRecBookmark(Bookmark));
end;

{function TMemDataSet.GetBookmarkStr: TBookmarkStr;
begin
  if State in [dsFilter] then begin
    SetLength(Result, BookmarkSize);
    Data.GetBookmark(Pointer(Result));
  end
  else
    Result := inherited GetBookmarkStr;
end;}

function ChangeDecimalSeparator(const Value: string; OldSeparator, NewSeparator: string): string;
begin
  if OldSeparator <> NewSeparator then
    Result := StringReplace(Value, OldSeparator, NewSeparator, [rfReplaceAll])
  else
    Result := Value;
end;

procedure TMemDataSet.SaveToXML(const FileName: string);
var
  FileStream: TFileStream;
begin
  FileStream := TFileStream.Create(FileName, fmCreate);
  try
    SaveToXML(FileStream);
  finally
    FileStream.Free;
  end;
end;

procedure TMemDataSet.SaveToXML(Destination: TStream);
begin
  CheckDataSetService;
  FDataSetService.SaveToXML(Destination);
end;

{$IFDEF CLR} // TDataSet bug
var
  NativeDBBuffers: TDBBufferList;

function NativeBuffers: TDBBufferList;
begin
  if NativeDBBuffers <> nil then
    Result := NativeDBBuffers
  else
  begin
    Result := TDBBufferList.Create;
    NativeDBBuffers := Result; // fix d9 bug
  end;
end;

function TMemDataSet.GetBookmark: TBookmark;
begin
  if BookmarkAvailable then
  begin
    Result := NativeBuffers.AllocHGlobal(BookmarkSize);
    GetBookmarkData(ActiveBuffer, Result);
  end else
    Result := nil;
end;

procedure TMemDataSet.FreeBookmark(var Bookmark: TBookmark);
begin
  NativeBuffers.FreeHGlobal(Bookmark);
end;
{$ENDIF}

function TMemDataSet.BookmarkValid({$IFDEF CLR}const{$ENDIF} Bookmark: TBookmark): boolean;
begin
  if Data <> nil then
    Result := Data.BookmarkValid(PRecBookmark(Bookmark))
  else
    Result := False;
end;

function TMemDataSet.CompareBookmarks({$IFDEF CLR}const{$ENDIF} Bookmark1, Bookmark2: TBookmark): integer;
begin
  if Data <> nil then
    Result := Data.CompareBookmarks(PRecBookmark(Bookmark1), PRecBookmark(Bookmark2))
  else
    Result := 0;
end;

{ Navigation }

procedure TMemDataSet.InternalSetToRecord(Buffer: TRecordBuffer);
begin
  InternalGotoBookmark(PtrOffset(Buffer, FBookmarkOfs));

  if Data.NeedGetRecordAfterGotoBookmark then
    GetRecord(Buffer, gmCurrent, True);
end;

procedure TMemDataSet.SetIndexFieldNames(Value: _string);
begin
  if Active then begin
    CheckBrowseMode;
    UpdateCursorPos;
  end;
  if Data <> nil then begin
    TMemData(Data).SetIndexFieldNames(Value);
    if Active and (TMemData(Data).IndexFields.Count > 0) then
      Resync([]);
  end;    
  FIndexFieldNames := Value;
end;

procedure TMemDataSet.InternalFirst;
begin
  Assert(Data <> nil);
  Data.SetToBegin;
end;

procedure TMemDataSet.InternalLast;
begin
  Assert(Data <> nil);
  Data.SetToEnd;
end;

{ Editing }

procedure TMemDataSet.InternalAddRecord(Buffer: IntPtr; Append: Boolean);
begin
  Assert(Data <> nil);
  if Append then
    Data.AppendRecord(Buffer)
  else
    Data.InsertRecord(Buffer);
end;

procedure TMemDataSet.InternalInsert;
begin
  Assert(Data <> nil);
  FInDeferredPost := False;
  if OldDeferredPostBuf <> nil then begin
    FreeRefComplexFields(OldDeferredPostBuf, False);
    Marshal.FreeHGlobal(OldDeferredPostBuf);
    OldDeferredPostBuf := nil;
  end;
  FInInserting := True;
end;

procedure TMemDataSet.InternalEdit;
begin
  Assert(Data <> nil);
  FInDeferredPost := False;
  if OldDeferredPostBuf <> nil then begin
    FreeRefComplexFields(OldDeferredPostBuf, False);
    Marshal.FreeHGlobal(OldDeferredPostBuf);
    OldDeferredPostBuf := nil;
  end;
  FInEditing := True;

  FreeRefComplexFields(ActiveBuffer);
  Data.EditRecord(ActiveBuffer);
end;

procedure TMemDataSet.InternalDelete;
begin
  Assert(Data <> nil);
  if not CanModify then DatabaseError(SDataSetReadOnly, Self);
  Data.DeleteRecord;

  // CR M8107
  FInInserting := False;
  FInEditing := False;
end;

procedure TMemDataSet.InternalPost;
var
  i: integer;
  Blob: TBlob;
  Field: TField;
  FieldDesc: TFieldDesc;
begin
{$IFDEF VER6P}
  inherited;
{$ENDIF}

  Assert(Data <> nil);
  for i := 0 to FieldCount - 1 do begin
    Field := Fields[i];
    FieldDesc := GetFieldDesc(Field);
    if (FieldDesc <> nil) and Data.IsBlobFieldType(FieldDesc.DataType) then begin
      Blob := Data.GetObject(FieldDesc.FieldNo, ActiveBuffer) as TBlob;
      if Blob.CanRollback then
        TBlobField(Field).Modified := True;
    end;
  end;
  if State = dsEdit then begin
    Data.PostRecord(ActiveBuffer);
  end
  else
    Data.InsertRecord(ActiveBuffer);

  FInDeferredPost := False;
  if OldDeferredPostBuf <> nil then begin
    FreeRefComplexFields(OldDeferredPostBuf, False);
    Marshal.FreeHGlobal(OldDeferredPostBuf);
    OldDeferredPostBuf := nil;
  end;
  FInInserting := False;
  FInEditing := False;
end;

procedure TMemDataSet.Cancel;
var
  CancelBuf : IntPtr;
begin
  if Data = nil then
    Exit;
  if ((State = dsEdit) or (State = dsInsert)) and (Data.HasComplexFields) then begin
    CancelBuf := AllocRecordBuffer;
    CopyBuffer(ActiveBuffer, CancelBuf, FRecBufSize);
    Data.AddRefComplexFields(CancelBuf);
  end
  else
    CancelBuf := nil;
  try
  {$IFDEF D3}
    if State = dsInsert then
      if Data.HasComplexFields then
        Data.FreeComplexFields(ActiveBuffer, True);
  {$ENDIF}

    inherited;
  finally
    if CancelBuf <> nil then begin
      Data.FreeComplexFields(CancelBuf, True);
      Marshal.FreeHGlobal(CancelBuf);
    end;
  end;
end;


procedure TMemDataSet.InternalCancel;
begin
  Assert(Data <> nil);
  FInDeferredPost := False;
  if OldDeferredPostBuf <> nil then begin
    FreeRefComplexFields(OldDeferredPostBuf, False);
    Marshal.FreeHGlobal(OldDeferredPostBuf);
    OldDeferredPostBuf := nil;
  end;
  FInInserting := False;
  FInEditing := False;

  if State = dsEdit then
    Data.CancelRecord(ActiveBuffer);
{$IFNDEF D3}
  if State = dsInsert then
    if Data.HasComplexFields then
      Data.FreeComplexFields(ActiveBuffer, True);
{$ENDIF}
end;

procedure TMemDataSet.InternalDeferredPost;
begin
  if State = dsEdit then
    DoPerformUpdate
  else
    DoPerformAppend;
end;

procedure TMemDataSet.DeferredPost;
  procedure CheckRequiredFields;
  var
    I: Integer;
  begin
    for I := 0 to Fields.Count - 1 do
      with Fields[I] do
        if Required and not ReadOnly and (FieldKind = fkData) and IsNull then
        begin
          FocusControl;
          DatabaseErrorFmt(SFieldRequired, [DisplayName]);
        end;
  end;
begin
  Assert(Data <> nil);
  if not CachedUpdates then begin
    UpdateRecord;
    case State of
      dsEdit, dsInsert:
        begin
        {$IFDEF CLR}
          DataEvent(deCheckBrowseMode, nil);
        {$ELSE}
          DataEvent(deCheckBrowseMode, 0);
        {$ENDIF}
          CheckRequiredFields;
          UpdateCursorPos;

          InternalDeferredPost;

          if OldDeferredPostBuf = nil then begin
            OldDeferredPostBuf := AllocRecordBuffer;
            Data.CreateComplexFields(OldDeferredPostBuf, False);
            PRecInfo(PtrOffset(OldDeferredPostBuf, FRecInfoOfs)).RefComplexFields := True;//own complex fields
          end;
          CopyRecordBuffer(ActiveBuffer, OldDeferredPostBuf);
          FInDeferredPost := True;
        end;
    end;
  end;
end;

procedure TMemDataSet.SetDefaultExpressionValues;
var
  i: integer;
  Val: string;
begin
  FInSettingDefaultExpressionValues := True;
  try
    for i := 0 to FieldCount - 1 do
      if Fields[i].DefaultExpression <> '' then begin
        Val := Fields[i].DefaultExpression;
        if not DefaultExpressionOldBehavior then
          Val := AnsiDequotedStr(Val, '''');
        Fields[i].AsString := Val;
      end;
  finally
    FInSettingDefaultExpressionValues := False;
  end;
end;

procedure TMemDataSet.DoOnNewRecord;
var
  DataSet: TDataSet;
  MasterField, DetailField: TField;
  MasterName, DetailName: _string;
  MasterPos, DetailPos: integer;

  procedure LinkMDFields(const MasterName, DetailName: _string);
  begin
    MasterField := DataSet.FindField(MasterName);
    if Assigned(MasterField) then begin
      DetailField := FindField(DetailName);
      if Assigned(DetailField) and not DetailField.ReadOnly then begin // CR 11917
      {$IFDEF VER6P}
        if DetailField is TLargeintField then
          TLargeintField(DetailField).AsLargeInt := MasterField.Value
        else
      {$ENDIF}
          DetailField.Assign(MasterField);
      end;
    end;
  end;

begin
  SetDefaultExpressionValues;

  try
    inherited;
  except
    InternalCancel;
    raise;
  end;

  if (DataSource <> nil) then begin
    DataSet := DataSource.DataSet;
    if (DataSet <> nil) and DataSet.Active then begin
      //MD link by MasteFields and DetailFields
      if (FMasterFields <> '') and (FDetailFields <> '') then begin
          MasterPos := 1;
          DetailPos := 1;
          while True do begin
            MasterName := ExtractFieldName(FMasterFields, MasterPos);
            DetailName := ExtractFieldName(FDetailFields, DetailPos);
            if (MasterName <> '') and (DetailName <> '') then
              LinkMDFields(MasterName, DetailName)
            else
              break;
          end;
      end;
      //We couldn't link MD fields in case of undefined FMasterFields or FDetailFields
      //cause there is could be field names mismatch
    end;
  end;
end;

procedure TMemDataSet.DoPerformAppend;
begin
  if (FDataSetService <> nil) and (FDataSetService.FUpdater <> nil) then
    FDataSetService.FUpdater.DoPerformAppend;
end;

procedure TMemDataSet.DoPerformDelete;
begin
  if (FDataSetService <> nil) and (FDataSetService.FUpdater <> nil) then
    FDataSetService.FUpdater.DoPerformDelete;
end;

procedure TMemDataSet.DoPerformUpdate;
begin
  if (FDataSetService <> nil) and (FDataSetService.FUpdater <> nil) then
    FDataSetService.FUpdater.DoPerformUpdate;
end;

procedure TMemDataSet.DoApplyRecord(UpdateKind: TUpdateRecKind; var Action: TUpdateRecAction; LastItem: boolean);
begin
  if (FDataSetService <> nil) and (FDataSetService.FUpdater <> nil) then
    FDataSetService.FUpdater.DoApplyRecord(UpdateKind, Action, LastItem);
end;

procedure TMemDataSet.DoCacheChanged;
begin
  if (FDataSetService <> nil) and (FDataSetService.FUpdater <> nil) then
    FDataSetService.FUpdater.DoCacheChanged;
end;

procedure TMemDataSet.DoCacheApplied;
begin
  if (FDataSetService <> nil) and (FDataSetService.FUpdater <> nil) then
    FDataSetService.FUpdater.DoCacheApplied;
end;

procedure TMemDataSet.DoCacheCanceled;
begin
  if (FDataSetService <> nil) and (FDataSetService.FUpdater <> nil) then
    FDataSetService.FUpdater.DoCacheCanceled;
end;

{ Filter / Locate / Find }
procedure TMemDataSet.DoGetCachedFields;
var
  i: Integer;
  FieldDesc: TFieldDesc;
  CalcFieldCount: integer;
begin
  if Data = nil then
    exit;

  CalcFieldCount := 0;
  if not DefaultFields then begin
    for i := 0 to Fields.Count - 1 do
      case Fields[i].FieldKind of
        fkCalculated, fkLookup: begin
          FieldDesc := Data.GetFieldDescType.Create;
          try
            FieldDesc.ActualName := Fields[i].{$IFNDEF FPC}FullName{$ELSE}FieldName{$ENDIF};
            FieldDesc.Name := Fields[i].{$IFNDEF FPC}FullName{$ELSE}FieldName{$ENDIF};
            FieldDesc.FieldNo := Data.FieldCount + 1;
            FieldDesc.DataType := DataTypeMap[Fields[i].DataType];
            case Fields[i].DataType of
              ftWideString:
                FieldDesc.Size := (Fields[i].Size + 1) * SizeOf(WideChar);
              ftDate, ftTime:
                FieldDesc.Size := Fields[i].DataSize * 2;
              else
                FieldDesc.Size := Fields[i].DataSize;
            end;
            FieldDesc.Length := Fields[i].Size;
            if FCacheCalcFields then
              FieldDesc.FieldDescKind := fdkCached
            else
              FieldDesc.FieldDescKind := fdkCalculated;

            // FieldDescs with CachedField=True must be positioned after data FieldDescs
            Data.Fields.Add(FieldDesc);
            Inc(CalcFieldCount);
            SetLength(FCalcFieldsMapping, CalcFieldCount);
            FCalcFieldsMapping[CalcFieldCount - 1].Field := Fields[i];
            FCalcFieldsMapping[CalcFieldCount - 1].FieldDesc := FieldDesc;
          except
            FieldDesc.Free;
            raise;
          end;
        end;
      end;
  end;
end;

procedure TMemDataSet.DoGetCachedBuffer(Buffer: IntPtr; Source: IntPtr = nil);
var
  RecBuf: IntPtr;
begin
  if (CalcFieldsSize > 0) then begin
    Assert(Data <> nil);
    RecBuf := Marshal.AllocHGlobal(Data.RecordSize + Data.CalcRecordSize + SizeOf(TRecInfo) + BookmarkSize);
    try
      if Source = nil then
        CopyBuffer(Buffer, RecBuf, Data.RecordSize)
      else
        CopyBuffer(Source, RecBuf, Data.RecordSize);

      GetCalcFields(RecBuf);
      CopyBuffer(RecBuf, Buffer, Data.RecordSize + Data.CalcRecordSize);
    finally
      Marshal.FreeHGlobal(RecBuf);
    end;
  end;
end;

procedure TMemDataSet.ActivateFilters;
begin
  if Data = nil then
    Exit;

  DeactivateFilters;

  if Assigned(OnFilterRecord) then
    Data.FilterFunc := RecordFilter;
  if Trim(Filter) <> '' then
    Data.FilterText := Filter;

  Data.FilterCaseInsensitive := foCaseInsensitive in FilterOptions;
  Data.FilterNoPartialCompare := foNoPartialCompare in FilterOptions;
end;

procedure TMemDataSet.DeactivateFilters;
begin
  if Data = nil then
    Exit;
  Data.FilterFunc := nil;
  Data.FilterText := '';
end;

function TMemDataSet.RecordFilter(RecBuf: IntPtr): boolean;
var
  Accept: boolean;
  SaveState: TDataSetState;
begin
  SaveState := SetTempState(dsFilter);
  FFilterBuffer := RecBuf;
  try
    Accept := True;
    OnFilterRecord(Self, Accept);
  except
    InternalHandleException;
  end;
  RestoreState(SaveState);
  Result := Accept;
end;

procedure TMemDataSet.SetFiltered(Value: boolean);
begin
  if Active then begin
    CheckBrowseMode;
    UpdateCursorPos;
  end;

  if Value <> Filtered then begin
    if Value then
      ActivateFilters
    else
      DeactivateFilters;

    inherited SetFiltered(Value);

    if Active then begin
      Assert(Data <> nil);
      Data.FilterUpdated;
      Resync([]);
      First;
      //DoAfterScroll;
    end;
  end;
end;

procedure TMemDataSet.SetFilterData(const Text: string; Options: TFilterOptions);
begin
  if Active then begin
    CheckBrowseMode;
    UpdateCursorPos;
  end;

  if (Text <> Filter) or (Options <> FilterOptions) then begin

    if Data <> nil then begin
      Data.FilterCaseInsensitive := foCaseInsensitive in Options;
      Data.FilterNoPartialCompare := foNoPartialCompare in Options;

      if Filtered and (Trim(Text) <> '') then
        Data.FilterText := Text
      else
        Data.FilterText := '';
    end;    

    inherited SetFilterText(Text);
    inherited SetFilterOptions(Options);

    if Active and Filtered then begin
      Data.FilterUpdated;
      Resync([]);
      First;
      //DoAfterScroll;
    end;
  end;
end;

procedure TMemDataSet.SetFilterOptions(Value: TFilterOptions);
begin
  SetFilterData(Filter, Value);
end;

procedure TMemDataSet.SetFilterText(const Value: string);
begin
  SetFilterData(Value, FilterOptions);
end;

procedure TMemDataSet.SetOnFilterRecord(const Value: TFilterRecordEvent);
begin
  if Active then begin
    CheckBrowseMode;
    UpdateCursorPos;
  end;

  inherited SetOnFilterRecord(Value);

  if Data = nil then
    Exit;
    
  if Filtered and Assigned(OnFilterRecord) then
    Data.FilterFunc := RecordFilter
  else
    Data.FilterFunc := nil;

  if Active then begin
    Data.FilterUpdated;
    Resync([]);
    First;
  end;
end;

function TMemDataSet.FindRecord(Restart, GoForward: boolean): boolean;
begin
  CheckBrowseMode;
  DoBeforeScroll;
  SetFound(False);
  UpdateCursorPos;
  CursorPosChanged;
  Assert(Data <> nil);
  if not Filtered then begin
    ActivateFilters;
    TMemData(Data).ClearItemsOmittedStatus; /// CR DAC22529
  end;
  try
    if GoForward then begin
      if Restart then
        Data.SetToBegin;
      Data.GetNextRecord(nil);
    end
    else begin
      if Restart then
        Data.SetToEnd;
      Data.GetPriorRecord(nil);
    end;
  finally
    if not Filtered then begin
      DeactivateFilters;
      TMemData(Data).ClearItemsOmittedStatus; /// CR DAC22529
    end;
  end;

  if not Data.BOF and not Data.EOF then begin
    Resync([rmExact, rmCenter]);
    SetFound(True);
  end;
  Result := Found;
  if Result then
    DoAfterScroll;
end;

{ Master/Detail }

function TMemDataSet.AddFieldToList(const FieldName: _string; DataSet: TDataSet;
  List: TList): boolean;
var
  Field: TField;
begin
  Field := DataSet.FindField(FieldName);
  if (Field <> nil) then
    List.Add(Field);
  Result := Field <> nil;
end;

procedure TMemDataSet.GetDetailLinkFields(MasterFields, DetailFields: {$IFDEF CLR}TObjectList{$ELSE}TList{$ENDIF});
var
  DataSet: TDataSet;
  MasterName, DetailName: _string;
  MasterPos, DetailPos: integer;
begin
  MasterFields.Clear;
  DetailFields.Clear;
  if (DataSource <> nil) and (DataSource.DataSet <> nil) then begin
    if (Self.MasterFields <> '') and (Self.DetailFields <> '') then begin
      DataSet := DataSource.DataSet;
      if (DataSet <> nil) and DataSet.Active then begin
        MasterPos := 1;
        DetailPos := 1;
        while True do begin
          MasterName := ExtractFieldName(FMasterFields, MasterPos);
          DetailName := ExtractFieldName(FDetailFields, DetailPos);
          if (MasterName = '') or (DetailName = '') then
            Break;
          if AddFieldToList(MasterName, DataSource.DataSet, MasterFields) then
            AddFieldToList(DetailName, Self, DetailFields);
        end;
      end;
    end;
  end;
end;

function TMemDataSet.GetDataSource: TDataSource;
begin
  Result := FDataLink.DataSource;
end;

function TMemDataSet.IsMasterDatasetActive: boolean;
begin
  Result := (FDataLink.DataSource <> nil) and (FDataLink.DataSource.DataSet <> nil)
    and FDataLink.DataSource.DataSet.Active;
end;

function TMemDataSet.IsConnectedToMaster: boolean;
begin
  Result := (MasterSource <> nil) and (FMasterFields <> '') and (FDetailFields <> '');
end;

function TMemDataSet.SetLocalMDLinks: boolean;
var
  MemDataSet: TMemDataSet;
  i: integer;
  RecBuf: TRecordBuffer;

  MasterField: TField;
  MasterFieldDesc: TFieldDesc;
  DetailField: TField;
  DetailFieldDesc: TFieldDesc;
  MasterPos: integer;
  DetailPos: integer;
  MasterName: _string;
  DetailName: _string;
  LinksCount: integer;
begin
  Result := True;
  if Length(FLocalMDLinks) > 0 then
    for i := 0 to Length(FLocalMDLinks) - 1 do
      if not FLocalMDLinks[i].NativeBuffer then
        Marshal.FreeHGlobal(FLocalMDLinks[i].Buffer);

  if IsConnectedToMaster and IsMasterDatasetActive then begin
    if not (DataSource.DataSet is TMemDataSet) then
      raise Exception.Create(SMDIsNotMemDataSet);

    MemDataSet := TMemDataSet(DataSource.DataSet);
    LinksCount := 0;
    MasterPos := 1;
    DetailPos := 1;
    while True do begin
      MasterName := ExtractFieldName(FMasterFields, MasterPos);
      DetailName := ExtractFieldName(FDetailFields, DetailPos);
      if (MasterName <> '') and (DetailName <> '') then begin
        MasterField := MemDataSet.FindField(MasterName);
        if Assigned(MasterField) then begin
          DetailField := FindField(DetailName);
          if Assigned(DetailField) then begin
            SetLength(FLocalMDLinks, LinksCount + 1);
            with FLocalMDLinks[LinksCount] do begin
              DetailFieldDesc := Data.FindField(DetailField.FieldName);
              MasterFieldDesc := MemDataSet.GetFieldDesc(MasterField);
              if DetailFieldDesc = nil then
                raise Exception.Create(Format(SFieldNotFound, [DetailField.FieldName]));
              if MasterFieldDesc = nil then
                raise Exception.Create(Format(SFieldNotFound, [MasterField.FieldName])); //TODO: Field : cannot be used for local master/detail link
              FieldNo := DetailFieldDesc.FieldNo;
              Buffer := nil;
              IsNull := MasterField.IsNull;
              NativeBuffer := False;
              if not IsNull then
                if DetailFieldDesc.DataType = MasterFieldDesc.DataType then begin
                  if MemDataSet.GetActiveRecBuf(RecBuf) then
                    Buffer := MemDataSet.Data.GetFieldBuf(RecBuf, MasterFieldDesc, BufferType, IsNull, NativeBuffer);
                end
                else
                  CopyFieldValue(MasterField.Value, Buffer, BufferType, DetailFieldDesc);
            end;
            Inc(LinksCount);
          end;
        end;
      end
      else
        break;
    end;
  end;
end;

function TMemDataSet.MDLinksRefreshed: boolean;
begin
  Result := SetLocalMDLinks;
end;

procedure TMemDataSet.MasterRecordChanged;
var
  DataSet: TDataSet;
begin
  FreeRefBuffers;

  if FDataLink.DataSource <> nil then begin
    DataSet := FDataLink.DataSource.DataSet;
    if DataSet <> nil then
      if DataSet.Active and (DataSet.State <> dsSetKey) then begin
        if MDLinksRefreshed then begin // need refresh
        {$IFDEF MSWINDOWS}
          if (FDetailRefreshTimer <> nil) and (FDetailRefreshTimer.Interval <> 0) then begin
            FDetailRefreshTimer.Enabled := False; //reset time period
            FDetailRefreshTimer.Enabled := True;
          end
          else
        {$ENDIF}
            RefreshDetail(nil);
        end;
      end;
  end;
end;

function TMemDataSet.UseLocalMasterDetailFilter: boolean;
begin
  Result := IsConnectedToMaster;
end;

procedure TMemDataSet.RefreshDetail(Sender: TObject);
begin
{$IFDEF MSWINDOWS}
  if FDetailRefreshTimer <> nil then
    FDetailRefreshTimer.Enabled := False;
{$ENDIF}
  if not Active then
    Exit;

  if UseLocalMasterDetailFilter then begin
    Data.FilterUpdated;
    Resync([]);
    First;
  end;
end;

function TMemDataSet.LocalDetailFilter(RecBuf: IntPtr): boolean;
var
  DataSet: TDataSet;
  i: integer;
begin
  Result := True;

  if FDataLink.DataSource <> nil then begin
    DataSet := FDataLink.DataSource.DataSet;
    if (DataSet <> nil) and DataSet.Active then begin
      // RefreshParamsOnInsert - returns old behavior
      Result := RefreshParamsOnInsert or (DataSet.RecordCount <> 0);
      if not Result then
        Exit;
    end;
  end;

  for i := 0 to Length(FLocalMDLinks) - 1 do
    with FLocalMDLinks[i] do begin
      if not Result then
        Break;
      if IsNull or TMemData(Data).GetNull(FieldNo, RecBuf) then
        Result := IsNull and TMemData(Data).GetNull(FieldNo, RecBuf)
      else
        Result := TMemData(Data).CompareFieldValue(Buffer, BufferType, Data.Fields[FieldNo - 1], RecBuf, []) = 0;
    end;
end;

procedure TMemDataSet.SetLocalDetailFilter;
begin
  if Data <> nil then
    if UseLocalMasterDetailFilter then
      Data.FilterMDFunc := LocalDetailFilter
    else
      Data.FilterMDFunc := nil;
end;

{$IFDEF MSWINDOWS}
procedure TMemDataSet.CheckRefreshDetailTimer;
begin
  if FDetailRefreshTimer = nil then begin
    FDetailRefreshTimer := TWin32Timer.Create(Self);
    FDetailRefreshTimer.Enabled := False;
    FDetailRefreshTimer.OnTimer := RefreshDetail;
  end;
end;
{$ENDIF}

procedure TMemDataSet.SetDetailDelay(Value: integer);
begin
  FDetailDelay := Value;
{$IFDEF MSWINDOWS}
  CheckRefreshDetailTimer;
  FDetailRefreshTimer.Interval := Value;
{$ENDIF}
end;

procedure TMemDataSet.MDPropertiesChanged;
begin
  if Data <> nil then begin
    MDLinksRefreshed;
    Data.FilterUpdated;
  end;
  if Active then
    Resync([]);
end;

procedure TMemDataSet.SetMasterSource(Value: TDataSource);
var
  NeedRefresh: boolean;
begin
  if FDataLink.DataSource <> Value then begin
    if IsLinkedTo(Value) then
      DatabaseError(SCircularDataLink);
    NeedRefresh := IsConnectedToMaster;
    FDataLink.DataSource := Value;
    NeedRefresh := NeedRefresh or IsConnectedToMaster;
    SetLocalDetailFilter;

    if NeedRefresh then
      MDPropertiesChanged;
  end;
end;

procedure TMemDataSet.SetMasterFields(Value: _string);
var
  NeedRefresh: boolean;
begin
  if Value <> FMasterFields then begin
    NeedRefresh := IsConnectedToMaster;
    FMasterFields := Value;
    NeedRefresh := NeedRefresh or IsConnectedToMaster;
    SetLocalDetailFilter;

    if NeedRefresh then
      MDPropertiesChanged;
  end;
end;

procedure TMemDataSet.SetDetailFields(Value: _string);
var
  NeedRefresh: boolean;
begin
  if Value <> FDetailFields then begin
    NeedRefresh := IsConnectedToMaster;
    FDetailFields := Value;
    NeedRefresh := NeedRefresh or IsConnectedToMaster;
    SetLocalDetailFilter;

    if NeedRefresh then
      MDPropertiesChanged;
  end;
end;

// Allocate memory for Value (ValuePtr - must call FreeMem!) and copy Value to ValuePtr
procedure TMemDataSet.CopyFieldValue(const Value: variant; out ValuePtr: IntPtr;
  out ValueType: integer; FieldDesc: TFieldDesc; UseFieldType: boolean = True);
var
  sa: AnsiString;
  l: integer;
  Temp: IntPtr;
  ws: WideString;
  BoolValue: boolean;
{$IFDEF CLR}
  Data: TBytes;
{$ENDIF}
{$IFDEF VER6P}
  i64: Int64;
  bcd: TBcd;
{$ENDIF}
begin
  case VarType(Value) of
    varEmpty,varNull:
      ValuePtr := nil;
  else
    if VarIsStr(Value) then begin
      if not UseFieldType then
        case FieldDesc.DataType of
          dtBoolean: begin
            BoolValue := Value;
            ValuePtr := Marshal.AllocHGlobal(SizeOf(Boolean));
            Marshal.WriteByte(ValuePtr, Byte(BoolValue));
            ValueType := dtBoolean;
          end;
          dtBytes, dtVarBytes, dtExtVarBytes: begin
            sa := AnsiString(Value);
            l := Length(sa);
            ValuePtr := Marshal.AllocHGlobal(l + SizeOf(Word));

            Temp := Marshal.StringToHGlobalAnsi(sa);
            try
              CopyBuffer(Temp, PtrOffset(ValuePtr, SizeOf(Word)), l);
            finally
              Marshal.FreeCoTaskMem(Temp);
            end;
            Marshal.WriteInt16(ValuePtr, l);
            ValueType := FieldDesc.DataType;
          end;
          dtWideString, dtExtWideString:
          begin
            ws := Value;
            l := (Length(ws) + 1) * SizeOf(WideChar);
            ValuePtr := Marshal.AllocHGlobal(l);
            CopyBufferUni(ws, ValuePtr, l);
            ValueType := dtWideString;
          end;
        else
          begin
            sa := AnsiString(Value);
            l := Length(sa) + 1;
            ValuePtr := Marshal.AllocHGlobal(l);
            CopyBufferAnsi(sa, ValuePtr, l);
            ValueType := dtString;
          end;
        end
      else
        case FieldDesc.DataType of
          dtBoolean: begin
            CopyFieldValue(boolean(Value), ValuePtr, ValueType, FieldDesc);
          end;
          dtBytes, dtVarBytes, dtExtVarBytes: begin
            sa := AnsiString(Value);
            l := Length(sa);
            ValuePtr := Marshal.AllocHGlobal(l + SizeOf(Word));

            Temp := Marshal.StringToHGlobalAnsi(sa);
            try
              CopyBuffer(Temp, PtrOffset(ValuePtr, SizeOf(Word)), l);
            finally
              Marshal.FreeCoTaskMem(Temp);
            end;
            Marshal.WriteInt16(ValuePtr, l);
            ValueType := FieldDesc.DataType;
          end;
          dtWideString, dtWideMemo, dtExtWideString:
          begin
            ws := Value;
            l := (Length(ws) + 1) * SizeOf(WideChar);
            ValuePtr := Marshal.AllocHGlobal(l);
            CopyBufferUni(ws, ValuePtr, l);
            ValueType := dtWideString;
          end;
          dtString, dtExtString, dtBlob, dtMemo, dtGuid {$IFDEF VER5P}, dtVariant {$ENDIF}:
          begin
            sa := AnsiString(Value);
            l := Length(sa) + 1;
            ValuePtr := Marshal.AllocHGlobal(l);
            CopyBufferAnsi(sa, ValuePtr, l);
            ValueType := dtString;
          end;
          dtInt8, dtInt16, dtInt32, dtUInt16:
            CopyFieldValue(StrToInt(Value), ValuePtr, ValueType, FieldDesc);
          dtDate:
            CopyFieldValue(StrToDate(Value), ValuePtr, ValueType, FieldDesc);
          dtTime:
            CopyFieldValue(StrToTime(Value), ValuePtr, ValueType, FieldDesc);
          dtDateTime:
            CopyFieldValue(StrToDateTime(Value), ValuePtr, ValueType, FieldDesc);
          dtFloat, dtBCD{$IFDEF VER6P}{$IFNDEF FPC}, dtFMTBCD{$ENDIF}{$ENDIF}, dtCurrency:
            CopyFieldValue(StrToFloat(Value), ValuePtr, ValueType, FieldDesc);
          {$IFDEF VER6P}
          dtInt64, dtUint32:
            CopyFieldValue(StrToInt64(Value), ValuePtr, ValueType, FieldDesc);
          {$ENDIF}
        else
          raise EConvertError.Create(SCannotConvertType);
        end;
    end
    else begin
      ValueType := FieldDesc.DataType;
      case ValueType of
        dtInt32, dtInt8, dtInt16, dtUInt16: begin
          ValuePtr := Marshal.AllocHGlobal(SizeOf(integer));
          Marshal.WriteInt32(ValuePtr, Value);
        end;
        dtUInt32: begin
          ValuePtr := Marshal.AllocHGlobal(SizeOf(longword));
          Marshal.WriteInt32(ValuePtr, Value);
        end;
        dtBoolean: begin
          ValuePtr := Marshal.AllocHGlobal(SizeOf(Boolean));
          Marshal.WriteByte(ValuePtr, Byte(Boolean(Value)));
        end;
        dtInt64: begin
        {$IFDEF VER6P}
          ValuePtr := Marshal.AllocHGlobal(SizeOf(Int64));
          i64 := Value;
          Marshal.WriteInt64(ValuePtr, i64);
        {$ELSE}
          GetMem(ValuePtr, SizeOf(Int64));
          if TVarData(Value).VType = varDecimal then
            Int64(ValuePtr^) := TVarDataD6(Value).VInt64
          else
            Int64(ValuePtr^) := StrToInt64(Value);
        {$ENDIF}
        end;
        dtFloat, dtCurrency: begin
          ValuePtr := Marshal.AllocHGlobal(SizeOf(Int64));
          Marshal.WriteInt64(ValuePtr, BitConverter.DoubleToInt64Bits(Value));
        end;
        dtBCD: begin
          ValuePtr := Marshal.AllocHGlobal(SizeOf(Currency));
          Marshal.WriteInt64(ValuePtr, BitConverter.DoubleToInt64Bits(Value));
        end;
      {$IFDEF VER6P}
      {$IFNDEF FPC}
        dtFmtBCD: begin
          ValuePtr := Marshal.AllocHGlobal(SizeOfTBcd);
          bcd := VarToBcd(Value);
        {$IFDEF CLR}
          Data := TBcd.ToBytes(bcd);
          Marshal.Copy(Data, 0, ValuePtr, SizeOfTBcd);
        {$ELSE}
          PBcd(ValuePtr)^ := bcd;
        {$ENDIF}
        end;
      {$ENDIF}
      {$ENDIF}
        dtString, dtExtString: begin
          sa := AnsiString(Value);
          l := Length(sa) + 1;
          ValuePtr := Marshal.AllocHGlobal(l);
          CopyBufferAnsi(sa, ValuePtr, l);
          ValueType := dtString;
        end;
        dtWideString, dtExtWideString: begin
          ws := Value;
          l := (Length(ws) + 1) * SizeOf(WideChar);
          ValuePtr := Marshal.AllocHGlobal(l);
          CopyBufferUni(ws, ValuePtr, l);
          ValueType := dtWideString;
        end;
        dtDateTime, dtDate, dtTime: begin
          ValuePtr := Marshal.AllocHGlobal(SizeOf(Int64));
          Marshal.WriteInt64(ValuePtr, BitConverter.DoubleToInt64Bits(TDateTime(Value)));
        end;
        dtBytes, dtVarBytes, dtExtVarBytes: begin
          Assert(VarType(Value) = varArray + varByte);
        {$IFDEF CLR}
          SetLength(Data, VarArrayHighBound(Value, 1) + 1);
          for l := 0 to High(Data) do
            Data[l] := VarArrayGet(Value, l);
          ValuePtr := Marshal.AllocHGlobal(Length(Data) + sizeof(word));
          Marshal.WriteInt16(ValuePtr, Length(Data));
          Marshal.Copy(Data, 0, PtrOffset(ValuePtr, sizeof(word)), Length(Data));
        {$ELSE}
          GetMem(ValuePtr, TVarData(Value).VArray.Bounds[0].ElementCount + SizeOf(Word));
          Move(TVarData(Value).VArray.Data^, (PAnsiChar(ValuePtr) + SizeOf(Word))^, TVarData(Value).VArray.Bounds[0].ElementCount);
          Word(ValuePtr^) := TVarData(Value).VArray.Bounds[0].ElementCount;
        {$ENDIF}
        end;
      else
        raise EConvertError.Create(SCannotConvertType);
      end;
    end;
  end;
end;

function TMemDataSet.InternalLocateRecord(KeyFields: TDAList; KeyValues: variant;
  Options: TLocateExOptions; SavePos: boolean): boolean;
var
  FieldDesc: TFieldDesc;
  RecBuf: TRecordBuffer;
  Values: array of IntPtr;
  Types: array of integer;
  i, FieldCount: integer;
  IndexedFieldCount: integer;
  Bookmark: PRecBookmark;
  Res: integer;
  CalcKeyFields: boolean;
  FirstRecNo, LastRecNo: Integer;
  PartialSearchFirstEntry: Boolean;
  CompareOptions: TCompareOptions;
  CanUseGoldenSection: Boolean;

  procedure SetKeyFields;
  var
    i: integer;
    Index: integer;
    Value: variant;
  begin
    CalcKeyFields := False;
    FieldCount := KeyFields.Count;
    SetLength(Values, FieldCount);
    for i := 0 to FieldCount - 1 do begin
      Values[i] := nil; // Clear Values array to prevent AV in 'finally' section after Exception

      if TFieldDesc(KeyFields[i]).FieldDescKind = fdkCalculated then
        CalcKeyFields := True;
    end;

    IndexedFieldCount := 0;
    if not ((lxPartialKey in Options) or (lxPartialCompare in Options) or (lxNearest in Options))then      //lxPartialKey and lxPartialCompare incompatible with ordered locate
                                                                                                           //lxNearest can be partially supported in the next versions
      for i := 0 to TMemData(Data).IndexFields.Count - 1 do begin
        //Check ordered locate posibility for current Index field
        if (TMemData(Data).IndexFields[i].SortType = stCaseInsensitive) xor
          (lxCaseInsensitive in Options)
        then
          Break;

        //First IndexFields should be in KeyFields if not then we can't use ordered Locate
        Index := KeyFields.IndexOf(TMemData(Data).IndexFields[i].FieldDesc);
        if (Index > -1) then begin
          if IndexedFieldCount <> Index then begin
            FieldDesc := TFieldDesc(KeyFields[IndexedFieldCount]);
            KeyFields[IndexedFieldCount] := KeyFields[Index];
            KeyFields[Index] := FieldDesc;

            Value := KeyValues[IndexedFieldCount];
            KeyValues[IndexedFieldCount] := KeyValues[Index];
            KeyValues[Index] := Value;
          end;
          inc(IndexedFieldCount);
        end
        else
          break;
      end;

    SetLength(Types, FieldCount);
    for i := 0 to FieldCount - 1 do begin
      Value := Unassigned;
      if VarIsArray(KeyValues) and ((FieldCount > 1) or (VarArrayHighBound(KeyValues, 1) = 0)) then
        if i <= VarArrayHighBound(KeyValues, 1) then
          Value := KeyValues[i]
        else
          Value := Null
      else
        if i = 0 then
          Value := KeyValues
        else
          Value := Null;
      // string values should be converted to field native types only when they are used
      // in the golden section search (for compatibility)
      CopyFieldValue(Value, Values[i], Types[i], TFieldDesc(KeyFields[i]), i < IndexedFieldCount);
    end;
  end;

  function GetGoldenSectionDir(ReadRecBuf: boolean): integer;
  var
    i: integer;
    FieldBlank: boolean;
    Dir: Integer;
  begin
    Result := 0;
    i := 0;
    if ReadRecBuf then
      Data.GetRecord(RecBuf);

    if CalcKeyFields then
      GetCalcFields(RecBuf);

    while (Result = 0) and (i < IndexedFieldCount) do begin
      FieldDesc := TFieldDesc(KeyFields[i]);

      FieldBlank := TMemData(Data).GetNull(FieldDesc.FieldNo, RecBuf);

      if TMemData(Data).IndexFields[i].DescendingOrder then
        Dir := -1
      else
        Dir := 1;

      if FieldBlank and (Values[i] = nil) then
        Result := 0
      else
      if FieldBlank and not (Values[i] = nil) then
        Result := 1
      else
      if not FieldBlank and (Values[i] = nil) then
        Result := -1
      else begin
        if TMemData(Data).IndexFields[i].SortType = stBinary then
          Include(CompareOptions, coOrdinalCompare)
        else
          Exclude(CompareOptions, coOrdinalCompare);
        Result := TMemData(Data).CompareFieldValue(Values[i], Types[i], FieldDesc, RecBuf, CompareOptions);
      end;

      if Result <> 0 then
        Result := Result * Dir;
      Inc(i);
    end;
  end;

  function ExecGoldenSection: boolean;
  var
    First, Last, Current: Integer;
    Dir: Integer;
    
  begin
    Result := False;

    if FirstRecNo > LastRecNo then Exit; // cr - 23687

    First := FirstRecNo;
    Last  := LastRecNo;

    Data.RecordNo := First;
    Dir := GetGoldenSectionDir(True);
    if Dir <= 0 then begin
      Result := Dir = 0;
      Exit;
    end;

    Data.RecordNo := Last;
    Dir := GetGoldenSectionDir(True);
    if Dir >= 0 then begin
      Result := Dir = 0;
      Exit;
    end;

    repeat
      Current := (Last + First) div 2;
      TMemData(Data).RecordNo := Current;
      Dir := GetGoldenSectionDir(True);
      if Dir < 0 then
        Last := Current
      else
      if Dir > 0 then
        First := Current
      else begin
        Result := True;
        Break;
      end;
    until  Last - First <= 1;
  end;

begin
  Result := False;
  if KeyFields.Count = 0 then
    Exit;

  CheckBrowseMode;
  CursorPosChanged;
  UpdateCursorPos;

  Assert(Data <> nil);
  FieldCount := 0;

  RecBuf := TempBuffer;
  FreeRefComplexFields(RecBuf);

  CompareOptions := [];
  if lxCaseInsensitive in Options then
    Include(CompareOptions, coCaseInsensitive);
  if lxPartialKey in Options then
    Include(CompareOptions, coPartialKey);
  if lxPartialCompare in Options then
    Include(CompareOptions, coPartialCompare);

  Values := nil;
  Bookmark := Marshal.AllocHGlobal(sizeof(TRecBookmark));
  try
    CanUseGoldenSection := True;
    try
      SetKeyFields;
    except
      CanUseGoldenSection := False;
    end;

    Data.GetBookmark(Bookmark);

    //Set locate dimensions. This dimensions should be used for lxNearest ordered implementation
    FirstRecNo := 1;
    LastRecNo := Data.RecordCount;

    if (lxNext in Options) then begin//Search from current position
      if IndexedFieldCount > 0 then begin
        Data.GetNextRecord(RecBuf);  //Next RecNo in case of Ordered Search
        if Data.Eof then begin
          if not Data.Bof {Empty Data} then
            Data.SetToBookmark(Bookmark);
          exit;
        end;
      end;
      FirstRecNo := Data.RecordNo;
    end;

    if (lxUp in Options) then begin  //Search from current position downto first
      if IndexedFieldCount > 0 then
        Data.GetPriorRecord(RecBuf); //Prior RecNo in case of Ordered Search
      LastRecNo := Data.RecordNo;
    end;

    if (IndexedFieldCount > 0) and CanUseGoldenSection then begin
      TMemData(Data).PrepareRecNoCache;
      Result := ExecGoldenSection;

      //Find the first occurence of located data
      if Result then
        repeat

          if (lxUp in Options) then begin
            if Data.RecordNo >= LastRecNo then //top limit of first occurrence search
              break
            else begin
              Data.GetNextRecord(RecBuf);
              if Data.Eof or (GetGoldenSectionDir(False) <> 0) then begin
                if IndexedFieldCount >= FieldCount then  //we shouldn't restore correct position in case of mixed locate
                  Data.GetPriorRecord(RecBuf);   //Restore correct position
                break;
              end;
            end;
          end
          else
            if Data.RecordNo <= FirstRecNo then //bottom limit of first occurrence search
              break
            else begin
              Data.GetPriorRecord(RecBuf);
              if (GetGoldenSectionDir(False) <> 0) then begin
                if IndexedFieldCount >= FieldCount then  //we shouldn't restore correct position in case of mixed locate
                  Data.GetNextRecord(RecBuf);     //Restore correct position
                break;
              end;
            end;

        until Data.Bof or Data.Eof;
    end;


    if (IndexedFieldCount = 0) or not CanUseGoldenSection or ((IndexedFieldCount < FieldCount) and Result) then begin

      if not((lxNext in Options) or (lxUp in Options)) and not (IndexedFieldCount > 0) then
        Data.SetToBegin;

      Exclude(CompareOptions, coOrdinalCompare);
      PartialSearchFirstEntry := (IndexedFieldCount < FieldCount) and Result;
      while True do begin
        if not PartialSearchFirstEntry then
          if lxUp in Options then
            Data.GetPriorRecord(RecBuf)
          else
            Data.GetNextRecord(RecBuf)
        else
          PartialSearchFirstEntry := False;

        if CalcKeyFields then
          GetCalcFields(RecBuf);

        if not (Data.EOF or Data.BOF) then begin
          Result := True;
          i := 0;
          while Result and (i < FieldCount) do begin
            FieldDesc := TFieldDesc(KeyFields[i]);
            if (Values[i] = nil) or Data.GetNull(FieldDesc.FieldNo, RecBuf) then
              Result := (Values[i] = nil) and Data.GetNull(FieldDesc.FieldNo, RecBuf)
            else begin
              Res := TMemData(Data).CompareFieldValue(Values[i], Types[i], FieldDesc, RecBuf, CompareOptions);
              Result := (Res = 0) or (Res < 0) and (lxNearest in Options);
            end;
            Inc(i);
          end;

          if Result then
            break;
        end
        else begin
          Result := lxNearest in Options; // goto last record for lxNearest 
          break;
        end;
      end;
    end;

    if (SavePos or not Result)
      and not (Data.Eof and Data.Bof {Empty Data}) then
      Data.SetToBookmark(Bookmark);
  finally
    Marshal.FreeHGlobal(Bookmark);
    for i := 0 to FieldCount - 1 do
      Marshal.FreeHGlobal(Values[i]);
  end;
end;

function TMemDataSet.LocateRecord(const KeyFields: _string; const KeyValues: variant;
  Options: TLocateExOptions; SavePos: boolean): boolean;
var
  Fields: TDAList;
  FieldDesc: TFieldDesc;

  procedure ParseKeyFields;
  var
    St: _string;
    i: integer;
  begin
    i := 1;
    while True do begin
      St := ExtractFieldName(KeyFields, i);
    {$IFDEF FPC}
      St := Trim(St);
    {$ENDIF}
      if St <> '' then begin
        FieldDesc := Data.FieldByName(St);
        if FieldDesc <> nil then
          Fields.Add(FieldDesc);
      end
      else
        break;
    end;
  end;

begin
  CheckActive;

  Fields := TDAList.Create;
  try
    ParseKeyFields;
    Result := InternalLocateRecord(Fields, KeyValues, Options, SavePos);
  finally
    Fields.Free;
  end;
end;

function TMemDataSet.LocateRecord(const KeyFields: array of TField; const KeyValues: variant;
  Options: TLocateExOptions; SavePos: boolean): boolean;
var
  Fields: TDAList;
  i: integer;
begin
  CheckActive;

  Fields := TDAList.Create;
  try
    for i := 0 to Length(KeyFields) - 1 do begin
      if KeyFields[i] <> nil then
        Fields.Add(GetFieldDesc(KeyFields[i]));
    end;

    Result := InternalLocateRecord(Fields, KeyValues, Options, SavePos);
  finally
    Fields.Free;
  end;
end;

function LocateExOptions(Options: TLocateOptions): TLocateExOptions;
begin
  Result := [];
  if loCaseInsensitive in Options then
    Result := Result + [lxCaseInsensitive];

  if loPartialKey in Options then
    Result := Result + [lxPartialKey];
end;

function TMemDataSet.Locate(const KeyFields: array of TField; const KeyValues: variant;
  Options: TLocateOptions): boolean;
begin
  DoBeforeScroll;

  Result := LocateRecord(KeyFields, KeyValues, LocateExOptions(Options), False);

  if Result then begin
    Resync([rmExact, rmCenter]);
    DoAfterScroll;
  end;
end;

function TMemDataSet.Locate(const KeyFields: string;
  const KeyValues: variant; Options: TLocateOptions): boolean;
begin
  DoBeforeScroll;

  Result := LocateRecord(KeyFields, KeyValues, LocateExOptions(Options), False);

  if Result then begin
    Resync([rmExact, rmCenter]);
    DoAfterScroll;
  end;
end;

function TMemDataSet.LocateEx(const KeyFields: _string;
  const KeyValues: variant; Options: TLocateExOptions): boolean;
begin
  DoBeforeScroll;

  Result := LocateRecord(KeyFields, KeyValues, Options, False);

  if Result then begin
    if LocateExOldBehavior or Data.Eof or Data.Bof then
      Resync([{rmExact, rmCenter}])
    else
      Resync([rmExact, rmCenter]);
    DoAfterScroll;
  end;
end;

function TMemDataSet.LocateEx(const KeyFields: array of TField;
  const KeyValues: variant; Options: TLocateExOptions): boolean;
begin
  DoBeforeScroll;

  Result := LocateRecord(KeyFields, KeyValues, Options, False);

  if Result then begin
    if LocateExOldBehavior or Data.Eof or Data.Bof then
      Resync([{rmExact, rmCenter}])
    else
      Resync([rmExact, rmCenter]);
    DoAfterScroll;
  end;
end;

function TMemDataSet.Lookup(const KeyFields: string; const KeyValues: variant;
  const ResultFields: string): variant;
var
  bm: TBookmark;
begin
  Result := Null;

  if Active and Filtered then begin
    bm := GetBookmark;
    try
      Data.FilterUpdated;
      Resync([]);
      try
        GotoBookmark(bm);
      except
        First;
      end;
    finally
      if bm <> nil then
        FreeBookmark(bm);
    end;
  end;

  if LocateRecord(KeyFields, KeyValues, [], True) then begin
    SetTempState(dsCalcFields);
    try
      CalculateFields(TempBuffer);
      Result := FieldValues[ResultFields];
    finally
      RestoreState(dsBrowse);
    end;
  end;
end;

{ CachedUpdates }

function TMemDataSet.InternalGetUpdateResult: TUpdateRecAction;
begin
  UpdateCursorPos;

  Assert(Data <> nil);
  Result := Data.GetUpdateResult;
end;

procedure TMemDataSet.CheckCachedUpdateMode;
begin
  if not CachedUpdates then
    DatabaseError(SNotCachedUpdate);
end;

function TMemDataSet.UpdateStatus: TUpdateStatus;
var
  RecBuf: TRecordBuffer;
begin
  if CachedUpdates and not IsEmpty then begin
    if State = dsCalcFields then
      RecBuf := CalcBuffer
    else
      RecBuf := ActiveBuffer;

    Result := PRecInfo(PtrOffset(RecBuf, FRecInfoOfs)).UpdateStatus;
  end
  else
    Result := usUnModified;
end;

function TMemDataSet.UpdateResult: TUpdateAction;
begin
  UpdateCursorPos;

  Assert(Data <> nil);
  if Data.GetUpdateResult = urNone then
    Result := uaApplied
  else
    Result := TUpdateAction(Data.GetUpdateResult);
end;

procedure TMemDataSet.ApplyUpdates;
begin
  ApplyUpdates([ukUpdate, ukInsert, ukDelete]);
end;

procedure TMemDataSet.ApplyUpdates(const UpdateRecKinds: TUpdateRecKinds);
begin
  CheckActive;
  FreeRefBuffers;
  if State <> dsBrowse then
    Post;
  CheckCachedUpdateMode;
  UpdateCursorPos;

  Assert(Data <> nil);
  NewCacheRecBuf := AllocRecordBuffer;
  OldCacheRecBuf := AllocRecordBuffer;
  FInCacheProcessing := True;
  try
    Data.SetCacheRecBuf(NewCacheRecBuf, OldCacheRecBuf);
    Data.ApplyUpdates(UpdateRecKinds);
  finally
    FInCacheProcessing := False;
    FreeRecordBuffer(NewCacheRecBuf);
    FreeRecordBuffer(OldCacheRecBuf);
    Resync([]);
  end;
end;

procedure TMemDataSet.CommitUpdates;
begin
  CheckActive;
  CheckCachedUpdateMode;
  FreeRefBuffers;
  UpdateCursorPos;

  Assert(Data <> nil);
  NewCacheRecBuf := AllocRecordBuffer;
  OldCacheRecBuf := AllocRecordBuffer;
  FInCacheProcessing := True;
  try
    Data.SetCacheRecBuf(NewCacheRecBuf, OldCacheRecBuf);
    Data.CommitUpdates;
  finally
    FInCacheProcessing := False;
    FreeRecordBuffer(NewCacheRecBuf);
    FreeRecordBuffer(OldCacheRecBuf);
  end;
  Resync([]);
end;

procedure TMemDataSet.CancelUpdates;
begin
  CheckActive;
  FreeRefBuffers;
  Cancel;
  CheckCachedUpdateMode;
  UpdateCursorPos;
  Assert(Data <> nil);
  Data.CancelUpdates;
  Resync([]);
end;

procedure TMemDataSet.RestoreUpdates;
begin
  CheckActive;
  FreeRefBuffers;
  Cancel;
  CheckCachedUpdateMode;
  UpdateCursorPos;
  Assert(Data <> nil);
  Data.RestoreUpdates;
  Resync([]);
end;

procedure TMemDataSet.RevertRecord;
begin
  CheckActive;
  FreeRefComplexFields(ActiveBuffer);
  if State in dsEditModes then
    Cancel;
  CheckCachedUpdateMode;
  UpdateCursorPos;
  Assert(Data <> nil);
  Data.RevertRecord;
  Resync([]);
end;

{
procedure TMemDataSet.DoApplyRecord(UpdateKind: TUpdateRecKind; var Action: TUpdateRecAction; LastItem: boolean);
var
  OldModified: boolean;
  UpdateAction: TUpdateAction;
begin
  OldModified := Modified;  // NewValue change Modified ??? or MemDS
  try
    UpdateAction := uaFail;
    try
      if Assigned(OnUpdateRecord) then begin
        OnUpdateRecord(Self, TUpdateKind(UpdateKind), UpdateAction);
        if UpdateAction in [uaAbort] then
          Abort;
      end;
      //else begin
      if not Assigned(OnUpdateRecord) or (UpdateAction = TUpdateAction(uaDefault)) then begin
        case UpdateKind of
          ukUpdate:
            PerformUpdate;
          ukInsert:
            PerformAppend;
          ukDelete:
            PerformDelete;
        end;

        if BatchUpdate then
          if CanFlushBatch or LastItem then
            // Should be flushed here because of Action parameter that should be returned
            // to TMemData.ApplyUpdates function
            FlushBatch
          else begin
            Action := urSuspended;
            Exit;
          end;

        UpdateAction := uaApplied;
      end;
    except
      on E: Exception do
        if IsClass(E, EDatabaseError) and Assigned(OnUpdateError) then begin
          OnUpdateError(Self, EDatabaseError(E), TUpdateKind(UpdateKind), UpdateAction);
          case UpdateAction of
            uaFail:
              raise;
            uaAbort:
              Abort;
          end;
        end
        else
          raise;
    end;
  finally
    SetModified(OldModified);
  end;
  Action := TUpdateRecAction(UpdateAction);
end; }


{ BLOB Support }

{function TMemDataSet.GetBlobData(Field:TField; Buffer: PAnsiChar):TBlobData;
begin
  Result := PBlobDataArray(Buffer + FBlobCacheOfs)[Field.Offset];
end;

procedure TMemDataSet.SetBlobData(Field:TField; Buffer: PAnsiChar; Value:TBlobData);
begin
  if Buffer = ActiveBuffer then
    PBlobDataArray(Buffer + FBlobCacheOfs)[Field.Offset] := Value;
end;

procedure TMemDataSet.ClearBlobCache(Buffer: PAnsiChar);
var
  i: integer;
begin
  for i := 0 to BlobFieldCount - 1 do
    PBlobDataArray(Buffer + FBlobCacheOfs)[i] := '';
end;}

procedure TMemDataSet.CloseBlob(Field: TField);
begin
end;

function TMemDataSet.CreateBlobStream(Field: TField; Mode: TBlobStreamMode): TStream;
begin
  Result := TBlobStream.Create(Field as TBlobField, Mode);
end;

{ Informational }

function TMemDataSet.IsSequenced: boolean;
begin
  Result := True;
end;

function TMemDataSet.GetRecordSize: word;
begin
  Assert(Data <> nil);
  Result := word(Data.RecordSize);
end;

function TMemDataSet.GetRecordCount: integer;
begin
  if Active then
    Result := Data.RecordCount
  else
    Result := 0;
end;

function TMemDataSet.GetRecNo: integer;
var
  RecBuf: TRecordBuffer;
begin
  if GetActiveRecBuf(RecBuf) then
    Result := PRecInfo(PtrOffset(RecBuf, FRecInfoOfs)).RecordNumber
  else
    Result := 0;
end;

procedure TMemDataSet.SetRecNo(Value: integer);
begin
  CheckBrowseMode;
  DoBeforeScroll;
  Assert(Data <> nil);
  Data.RecordNo := Value;
  Resync([{rmCenter}]);
  DoAfterScroll;
end;

{ More }

procedure TMemDataSet.InternalHandleException;
begin
{$IFDEF VER6P}
  if Assigned(Classes.ApplicationHandleException) then
    Classes.ApplicationHandleException(ExceptObject)
  else
    ShowException(ExceptObject, ExceptAddr)
{$ELSE}
  ApplicationHandleException(Self);
{$ENDIF}
end;

{$IFDEF CLR}
procedure TMemDataSet.DataEvent(Event: TDataEvent; Info: TObject);
{$ELSE} {$IFDEF FPC}
procedure TMemDataSet.DataEvent(Event: TDataEvent; Info: PtrInt);
{$ELSE} {$IFDEF VER16P}
procedure TMemDataSet.DataEvent(Event: TDataEvent; Info: NativeInt);
{$ELSE}
procedure TMemDataSet.DataEvent(Event: TDataEvent; Info: Longint);
{$ENDIF} {$ENDIF} {$ENDIF}
  procedure CheckIfParentScrolled;
  var
    ParentPosition, I: Integer;
  begin
    if FParentDataSet = nil then
      Exit;
    ParentPosition := 0;
    with FParentDataSet do
      if not IsEmpty then
        for I := 0 to BookmarkSize - 1 do
          ParentPosition := ParentPosition +
            Marshal.ReadByte(ActiveBuffer, FBookmarkOfs + I);
    if (FLastParentPos = 0) or (ParentPosition <> FLastParentPos) then
    begin
      First;
      FLastParentPos := ParentPosition;
    end else
    begin
      UpdateCursorPos;
      Resync([]);
    end;
  end;

begin
  if Event = deParentScroll then
    CheckIfParentScrolled;
  inherited DataEvent(Event, Info);
end;

procedure TMemDataSet.AssignTo(Dest: TPersistent);
begin
  if Dest is TMemDataSet then begin
    TMemDataSet(Dest).CachedUpdates := CachedUpdates;
    TMemDataSet(Dest).LocalConstraints := LocalConstraints;
    TMemDataSet(Dest).LocalUpdate := LocalUpdate;
    TMemDataSet(Dest).MasterSource := MasterSource;
    TMemDataSet(Dest).MasterFields := MasterFields;
    TMemDataSet(Dest).DetailFields := DetailFields;
    TMemDataSet(Dest).DetailDelay := DetailDelay;
  end
  else
    inherited;
end;

procedure TMemDataSet.SetCachedUpdates(Value: boolean);
begin
  if FCachedUpdates <> Value then begin
    CheckInactive;
    FCachedUpdates := Value;
    if Data <> nil then
      Data.CachedUpdates := FCachedUpdates;
  end;
end;

procedure TMemDataSet.SetLocalUpdate(Value: boolean);
begin
  if FLocalUpdate <> Value then begin
    FLocalUpdate := Value;
    if Data <> nil then
      Data.LocalUpdate := FLocalUpdate;
  end;
end;

function TMemDataSet.GetUpdatesPending: boolean;
begin
  if Data <> nil then
    Result := Data.UpdatesPending
  else
    Result := False;
end;

function TMemDataSet.GetPrepared: boolean;
begin
  if Data <> nil then
    Result := Data.Prepared
  else
    Result := False;
end;

procedure TMemDataSet.SetPrepared(Value: boolean);
begin
  if Value then
    Prepare
  else
    UnPrepare;
end;

function TMemDataSet.ConvertUpdateRecordTypes(Value: TUpdateRecordTypes): TItemTypes;
begin
  Result := [];
  if rtUnmodified in Value then
    Result := Result + [isUnmodified];
  if rtModified in Value then
    Result := Result + [isUpdated];
  if rtInserted in Value then
    Result := Result + [isAppended];
  if rtDeleted in Value then
    Result := Result + [isDeleted];
end;

function TMemDataSet.GetUpdateRecordTypes: TUpdateRecordTypes;
begin
  CheckCachedUpdateMode;

  Result := FUpdateRecordTypes;
end;

procedure TMemDataSet.SetUpdateRecordTypes(Value: TUpdateRecordTypes);
begin
  CheckCachedUpdateMode;

  //CheckBrowseMode;
  if Active then begin
    UpdateCursorPos;
    case State of // CR-S22656
      dsEdit, dsInsert:
        begin
          UpdateRecord;
          if Modified then Post else Cancel;
        end;
      dsSetKey:
        Post;
    end;
  end;

  FUpdateRecordTypes := Value;

  if Data <> nil then begin
    Data.FilterItemTypes := ConvertUpdateRecordTypes(Value);

    if Active then
      Resync([]);
  end;   
end;

{ TBlobStream }

constructor TBlobStream.Create(Field: TBlobField; Mode: TBlobStreamMode);
begin
  inherited Create;

  FMode := Mode;
  FField := Field;
  FDataSet := FField.DataSet as TMemDataSet;
  FFieldNo := FField.FieldNo;
  if not FDataSet.GetActiveRecBuf(FBuffer) then
    Exit;
{  if FDataSet.State = dsFilter then
    DatabaseErrorFmt('SNoFieldAccess', [FField.DisplayName]);}
  if not FField.Modified then begin
    if Mode = bmRead then begin
{      FCached := FDataSet.FCacheBlobs and (FBuffer = FDataSet.ActiveBuffer) and
        (FField.IsNull or (FDataSet.GetBlobData(FField, FBuffer) <> ''));}
    end
    else begin
//      FDataSet.SetBlobData(FField, FBuffer, '');
      if FField.ReadOnly then
        DatabaseErrorFmt(SFieldReadOnly, [FField.DisplayName]);
      if not (FDataSet.State in [dsNewValue, dsEdit, dsInsert]) then
        DatabaseError(SNotEditing);
    end;
  end;
  FOpened := True;
  if Mode = bmWrite then
    Truncate;
end;

destructor TBlobStream.Destroy;
begin
  if FOpened then begin
    if FModified then
      FField.Modified := True;
  end;
  if FModified then
    //try
    {$IFDEF CLR}
      FDataSet.DataEvent(deFieldChange, FField);
    {$ELSE} {$IFDEF FPC}
      FDataSet.DataEvent(deFieldChange, PtrInt(FField));
    {$ELSE} {$IFDEF VER16P} 
      FDataSet.DataEvent(deFieldChange, NativeInt(FField));
    {$ELSE}
      FDataSet.DataEvent(deFieldChange, Longint(FField));
    {$ENDIF} {$ENDIF} {$ENDIF}
    {except
      Application.HandleException(Self);
    end;}
end;

{$IFDEF CLR}
function TBlobStream.Read(var Buffer: TBytes; Offset, Count: Longint): Longint;
var
  Handle: IntPtr;
{$ELSE}
function TBlobStream.Read(var Buffer; Count: longint): longint;
{$ENDIF}
begin
  Result := 0;
  if FOpened then begin
    if Count > Size - FPosition then
      Result := Size - FPosition
    else
      Result := Count;
    if Result > 0 then begin
    {$IFDEF CLR}
      Handle := AllocGCHandle(Buffer, True);
      try
        Result := FDataSet.Data.ReadBlob(FFieldNo, FBuffer, FPosition, Count,
          GetAddrOfPinnedObject(Handle), FDataSet.State = dsOldValue{$IFDEF VER10P}, (FField is TWideMemoField){$ENDIF});
      finally
        FreeGCHandle(Handle);
      end;
    {$ELSE}
      Result := FDataSet.Data.ReadBlob(FFieldNo, FBuffer, FPosition, Count, @Buffer,
        (FDataSet.State = dsOldValue){$IFDEF VER10P}, (FField is TWideMemoField){$ENDIF});
    {$ENDIF}
      Inc(FPosition, Result);
    end;
  end;
end;

{$IFDEF CLR}
function TBlobStream.Write(const Buffer: TBytes; Offset, Count: Longint): Longint;
var
  Handle: IntPtr;
{$ELSE}
function TBlobStream.Write(const Buffer; Count: Longint): Longint;
{$ENDIF}
begin
  Result := 0;
  if FOpened then begin
  {$IFDEF CLR}
    Handle := AllocGCHandle(Buffer, True);
    try
      FDataSet.Data.WriteBlob(FFieldNo, FBuffer, FPosition, Count,
        GetAddrOfPinnedObject(Handle){$IFDEF VER10P}, FField is TWideMemoField{$ENDIF});
    finally
      FreeGCHandle(Handle);
    end;

  {$ELSE}
    FDataSet.Data.WriteBlob(FFieldNo, FBuffer, FPosition, Count, @Buffer
    {$IFDEF VER10P}, FField is TWideMemoField{$ENDIF});
  {$ENDIF}
    Inc(FPosition, Count);
    Result := Count;
    FModified := True;
{    FDataSet.SetBlobData(FField, FBuffer, '');}
  end;
end;

{$IFDEF CLR}
function TBlobStream.Seek(const Offset: Int64; Origin: TSeekOrigin): Int64;
{$ELSE}
function TBlobStream.Seek(Offset: Longint; Origin: Word): Longint;
{$ENDIF}
begin
  case Origin of
    soFromBeginning:
      FPosition := Offset;
    soFromCurrent:
      Inc(FPosition, Offset);
    soFromEnd:
      FPosition := GetBlobSize + Offset;
  end;
  Result := FPosition;
end;

procedure TBlobStream.Truncate;
begin
  if FOpened then begin
    FDataSet.Data.TruncateBlob(FFieldNo, FBuffer, FPosition
      {$IFDEF VER10P}, FField is TWideMemoField{$ENDIF});
    FModified := True;
//    FDataSet.SetBlobData(FField, FBuffer, '');
  end;
end;

function TBlobStream.GetBlobSize: longint;
begin
  Result := 0;
  if FOpened then
    Result := FDataSet.Data.GetBlobSize(FFieldNo, FBuffer, (FDataSet.State = dsOldValue)
    {$IFDEF VER10P}, FField is TWideMemoField{$ENDIF});
end;

{$IFDEF CLR}
procedure TBlobStream.SetSize(NewSize: Int64);
{$ELSE}
procedure TBlobStream.SetSize(NewSize: Longint);
{$ENDIF}
begin
  if FOpened then
    FDataSet.Data.SetBlobSize(FFieldNo, FBuffer, NewSize, (FDataSet.State = dsOldValue)
    {$IFDEF VER10P}, (FField is TWideMemoField){$ENDIF});
end;

class function TMemDSUtils.SetBlob(Obj: TMemDataSet; Field: TField; Blob: TBlob): boolean;
begin
  Result := Obj.SetBlob(Field, Blob);
end;

class function TMemDSUtils.GetBlob(Obj: TMemDataSet; FieldDesc: TFieldDesc): TBlob;
begin
  Result := Obj.InternalGetBlob(FieldDesc);
end;

{ TDataSetUpdater }

constructor TDataSetUpdater.Create(AOwner: TDataSetService);
begin
  inherited Create;

  FDataSetService := AOwner;
  Assert(FDataSetService <> nil);
  FDataSet := FDataSetService.FDataset;
end;

function TDataSetUpdater.PerformAppend: boolean;
begin
  Result := True;
end;

function TDataSetUpdater.PerformDelete: boolean;
begin
  Result := True;
end;

function TDataSetUpdater.PerformUpdate: boolean;
begin
  Result := True;
end;

function TDataSetUpdater.CacheChanged: boolean;
begin
  Result := True;
end;

function TDataSetUpdater.CacheApplied: boolean;
begin
  Result := True;
end;

function TDataSetUpdater.CacheCanceled: boolean;
begin
  Result := True;
end;

procedure TDataSetUpdater.DoPerformAppend;
var
  OldModified: boolean;
begin
  OldModified := FDataSet.Modified;
  try
    if not FDataSet.FLocalUpdate then
      if not FDataSet.FInDeferredPost then   // WAR supports defer posting
        PerformAppend
      else
        DoPerformUpdate;
  finally
    FDataSet.SetModified(OldModified);
  end;
end;

procedure TDataSetUpdater.DoPerformDelete;
var
  OldModified: boolean;
begin
  OldModified := FDataSet.Modified;
  try
    if not FDataSet.FLocalUpdate then
      PerformDelete
  finally
    FDataSet.SetModified(OldModified);
  end;
end;

procedure TDataSetUpdater.DoPerformUpdate;
var
  OldModified: boolean;
begin
  OldModified := FDataSet.Modified;
  try
    if not FDataSet.FLocalUpdate then
      PerformUpdate
  finally
    FDataSet.SetModified(OldModified);
  end;
end;

procedure TDataSetUpdater.DoCacheChanged;
begin
  CacheChanged;
end;

procedure TDataSetUpdater.DoCacheApplied;
begin
  CacheApplied;
end;

procedure TDataSetUpdater.DoCacheCanceled;
begin
  CacheCanceled;
end;

procedure TDataSetUpdater.DoApplyRecord(UpdateKind: TUpdateRecKind; var Action: TUpdateRecAction; LastItem: boolean);
var
  OldModified: boolean;
  UpdateAction: TUpdateAction;
begin
  OldModified := FDataSet.Modified;  // NewValue change Modified ??? or MemDS
  try
    UpdateAction := uaFail;
    try
      if Assigned(FDataSet.OnUpdateRecord) then begin
        FDataSet.OnUpdateRecord(FDataSet, TUpdateKind(UpdateKind), UpdateAction);
        case UpdateAction of
          uaFail:
            if not DoNotRaiseExcetionOnUaFail then
              raise EDatabaseError.Create(SCustomUpdateFailed);
          uaAbort:
            Abort;
        end;
      end;
      //else begin
      if not Assigned(FDataSet.OnUpdateRecord) or (UpdateAction = TUpdateAction(uaDefault)) then begin
        case UpdateKind of
          ukUpdate:
            PerformUpdate;
          ukInsert:
            PerformAppend;
          ukDelete:
            PerformDelete;
        end;

        if BatchUpdate then
          if CanFlushBatch or LastItem then
            // Should be flushed here because of Action parameter that should be returned
            // to TMemData.ApplyUpdates function
            FlushBatch
          else begin
            Action := urSuspended;
            Exit;
          end;

        UpdateAction := uaApplied;
      end;
    except
      on E: Exception do
        if IsClass(E, EDatabaseError) and Assigned(FDataSet.OnUpdateError) then begin
          FDataSet.OnUpdateError(FDataSet, EDatabaseError(E), TUpdateKind(UpdateKind), UpdateAction);
          case UpdateAction of
            uaFail:
              raise;
            uaAbort:
              Abort;
          end;
        end
        else
          raise;
    end;
  finally
    FDataSet.SetModified(OldModified);
  end;
  Action := TUpdateRecAction(UpdateAction);
end;

function TDataSetUpdater.BatchUpdate: boolean;
begin
  Result := False;
end;

function TDataSetUpdater.CanFlushBatch: boolean;
begin
  Result := False;
end;

procedure TDataSetUpdater.FlushBatch;
begin

end;

{ TDataSetService }

constructor TDataSetService.Create(AOwner: TMemDataSet);
begin
  inherited Create;

  FDataSet := AOwner;
  CreateDataSetUpdater;
end;

destructor TDataSetService.Destroy;
begin
  FreeDataSetUpdater;

  inherited;
end;

procedure TDataSetService.CreateDataSetUpdater;
begin
  SetDataSetUpdater(TDataSetUpdater.Create(Self));
end;

procedure TDataSetService.SetDataSetUpdater(Value: TDataSetUpdater);
begin
  FreeDataSetUpdater;
  FUpdater := Value;
end;

procedure TDataSetService.FreeDataSetUpdater();
begin
  FUpdater.Free;
  FUpdater := nil;
end;

function TDataSetService.SetProp(Prop: integer; const Value: variant): boolean;
begin
  Result := False;
  Assert(False);
end;

function TDataSetService.GetProp(Prop: integer; var Value: variant): boolean;
begin
  Result := False;
  Assert(False);
end;

procedure TDataSetService.SetNumberRange(FieldDef: TFieldDef);
var
  Field: TField;
  FieldDesc: TFieldDesc;
begin
  if FieldDef.DataType in [ftSmallint, ftInteger, ftLargeint, ftFloat] then begin
    Field := FDataSet.FindField(FieldDef.Name);
    if Field <> nil then begin
      FDataSet.CheckFieldCompatibility(Field, FieldDef);
      FieldDesc := FDataSet.Data.FindField(FieldDef.Name);
      if FieldDef.DataType in [ftSmallint, ftInteger] then begin
        Assert(Field is TIntegerField);
        TIntegerField(Field).MaxValue := Round(IntPower(10, FieldDesc.Length)) - 1;
        TIntegerField(Field).MinValue := -TIntegerField(Field).MaxValue;
      end
      else
      if FieldDef.DataType = ftLargeint then begin
        Assert(Field is TLargeintField);
        TLargeintField(Field).MaxValue := Round(IntPower(10, FieldDesc.Length)) - 1;
        TLargeintField(Field).MinValue := -TIntegerField(Field).MaxValue;
      end
      else
        if (FieldDesc.Length > 0) and (FieldDesc.Length <= 15) then begin
          Assert(Field is TFloatField);
          TFloatField(Field).Precision := FieldDesc.Length;
          TFloatField(Field).MaxValue :=
            IntPower(10, FieldDesc.Length - FieldDesc.Scale) -
            IntPower(10, - FieldDesc.Scale);
          TFloatField(Field).MinValue := - TFloatField(Field).MaxValue;
        end;
    end;
  end;
end;

function TDataSetService.GetFieldClass(FieldType: TFieldType): TFieldClass;
begin
  Result := DefaultFieldClasses[FieldType];
end;

procedure TDataSetService.PreInitCursor;
begin

end;

procedure TDataSetService.SaveToXML(Destination: TStream);
var
  FieldAliases: _TStringList;

  function IsValidFieldName(const FldName: _string): boolean;
  var
    i: integer;
    ch: _char;
  begin
    for i := 1 to Length(FldName) do begin
      ch := FldName[i];
      if not ((ch = '_') or (ch >= 'A') and (ch <= 'Z') or (ch >= 'a') and (ch <= 'z') or
        (i > 1) and (ch >= '0') and (ch <= '9'))
      then begin
        Result := False;
        Exit;
      end;
    end;
    Result := True;
  end;
  
  function GetFieldAlias(FldName: _string): _string;
  var
    i: integer;
    ActualName: _string;
  begin
    for i := 0 to FieldAliases.Count - 1 do begin
      ActualName := FieldAliases[i];
      if ActualName = FldName then begin
        Result := 'c' + IntToStr(i + 1);
        exit;
      end;
    end;
    Result := '';
  end;

var
  OldActive: boolean;
  Bookmark: DB.TBookmark;
  DestWriter: StreamWriter;
  XMLWriter: XmlTextWriter;
  XML: WideString;
  FldName, FldAlias: _string;
  i: integer;
  FieldDesc: TFieldDesc;
{$IFDEF VER8}
  DestStream: Stream;
{$ENDIF}
begin
  OldActive := FDataSet.Active;
  Bookmark := nil;
  FieldAliases := nil;
  DestWriter := nil;
  XmlWriter := nil;
{$IFDEF VER8}
  DestStream := nil;
{$ENDIF}
  try
    FDataSet.DisableControls;
    FDataSet.Active := True;
  {$IFDEF CLR}
  {$IFDEF VER9P}
  {$DEFINE OWNGETBOOKMARK}
  {$ENDIF}
  {$ENDIF}
  {$IFDEF OWNGETBOOKMARK}
    if FDataSet.BookmarkAvailable then
    begin
      Bookmark := Marshal.AllocHGlobal(FDataSet.BookmarkSize);
      FDataSet.GetBookmarkData(FDataSet.ActiveBuffer, Bookmark);
    end else
      Bookmark := nil;
  {$ELSE}
    Bookmark := FDataSet.GetBookmark;
  {$ENDIF}

    FieldAliases := _TStringList.Create;

  {$IFDEF VER8}
    DestStream := TStreamToCLRStream.GetStream(Destination);
  {$ENDIF}
    DestWriter := StreamWriter.Create({$IFDEF VER8}DestStream{$ELSE}Destination{$ENDIF}, Encoding.UTF8);
    XmlWriter := XmlTextWriter.Create(DestWriter);

    XmlWriter.QuoteChar := '''';
    XmlWriter.Formatting := fmtIndented;
    XmlWriter.Indentation := 2;
    XmlWriter.IndentChar := ' ';

    // Header
    XmlWriter.WriteStartElement('xml');

    XmlWriter.WriteAttributeString('xmlns:s', 'uuid:BDC6E3F0-6DA3-11d1-A2A3-00AA00C14882');
    XmlWriter.WriteAttributeString('xmlns:dt', 'uuid:C2F41010-65B3-11d1-A29F-00AA00C14882');
    XmlWriter.WriteAttributeString('xmlns:rs', 'urn:schemas-microsoft-com:rowset');
    XmlWriter.WriteAttributeString('xmlns:z', '#RowsetSchema');

    // Fields
    XmlWriter.WriteStartElement('s:Schema');
    XmlWriter.WriteAttributeString('id', 'RowsetSchema');

    XmlWriter.WriteStartElement('s:ElementType');
    XmlWriter.WriteAttributeString('name', 'row');
    XmlWriter.WriteAttributeString('content', 'eltOnly');
    XmlWriter.WriteAttributeString('rs:updatable', 'true');

    for i := 0 to FDataSet.Fields.Count - 1 do
      if not (FDataSet.Fields[i].FieldKind in [fkCalculated, fkLookup]) then begin
        FieldDesc := FDataSet.GetFieldDesc(FDataSet.Fields[i]);
        if FieldDesc <> nil then begin
          FldName := FieldDesc.Name;
          if not IsValidFieldName(FldName) then begin
            FldAlias := 'c' + IntToStr(FieldAliases.Count + 1);
            FieldAliases.Add(FldName);
          end
          else
            FldAlias := '';
          XmlWriter.WriteStartElement('s:AttributeType');
          WriteFieldXMLAttributeType(FDataSet.Fields[i], FieldDesc, FldAlias, XmlWriter);
          XmlWriter.WriteStartElement('s:datatype');
          WriteFieldXMLDataType(FDataSet.Fields[i], FieldDesc, FldAlias, XmlWriter);
          XmlWriter.WriteEndElement; // s:datatype
          XmlWriter.WriteFullEndElement; // s:AttributeType
        end;
      end;

    XmlWriter.WriteStartElement('s:extends');
    XmlWriter.WriteAttributeString('type', 'rs:rowbase');
    XmlWriter.WriteEndElement; // s:extends
    XmlWriter.WriteFullEndElement; // s:ElementType
    XmlWriter.WriteFullEndElement; // s:Schema

    // Data
    XmlWriter.WriteStartElement('rs:data');

    FDataSet.First;
    while not FDataSet.EOF do begin
      XmlWriter.WriteStartElement('z:row');
      for i := 0 to FDataSet.Fields.Count - 1 do
        if not (FDataSet.Fields[i].FieldKind in [fkCalculated, fkLookup]) then begin
          if not FDataSet.Fields[i].IsNull then begin
            XML := GetFieldXMLValue(FDataSet.Fields[i], FDataSet.GetFieldDesc(FDataSet.Fields[i]));
            FldName := FDataSet.Fields[i].FieldName;
            if not IsValidFieldName(FldName) then
              FldName := GetFieldAlias(FldName);
            XmlWriter.WriteAttributeString(FldName, XML);
          end;
        end;
      XmlWriter.WriteEndElement;
      FDataSet.Next;
    end;

    XmlWriter.WriteFullEndElement; // rs:data
    XmlWriter.WriteFullEndElement; // xml

    XmlWriter.Close;
  finally
    FDataSet.Active := OldActive;
    if OldActive and (Bookmark <> nil) then
      FDataSet.GotoBookmark(Bookmark);
    if Bookmark <> nil then
    {$IFDEF OWNGETBOOKMARK}
      Marshal.FreeHGlobal(Bookmark);
    {$ELSE}
      FDataSet.FreeBookmark(Bookmark);
    {$ENDIF}
    FieldAliases.Free;
    XmlWriter.Free;
    DestWriter.Free;
  {$IFDEF VER8}
    DestStream.Free;
  {$ENDIF}

    FDataSet.EnableControls;
  end;
end;

procedure TDataSetService.WriteFieldXMLDataType(Field: TField; FieldDesc: TFieldDesc;
  const FieldAlias: _string; XmlWriter: XMLTextWriter);
begin
  case FieldDesc.DataType of
    dtInt64: begin
      XmlWriter.WriteAttributeString('dt:type', 'i8');
      XmlWriter.WriteAttributeString('dt:maxLength', IntToStr(FieldDesc.Size));
      XmlWriter.WriteAttributeString('rs:precision', '19');
    end;
    dtBlob, dtBytes, dtVarBytes, dtExtVarBytes: begin
      XmlWriter.WriteAttributeString('dt:type', 'bin.hex');
      if (FieldDesc.DataType = dtBlob) then
        XmlWriter.WriteAttributeString('dt:maxLength', '2147483647')
      else
        XmlWriter.WriteAttributeString('dt:maxLength', IntToStr(FieldDesc.Length));
      if Field.IsBlob and not FieldDesc.Fixed then
        XmlWriter.WriteAttributeString('rs:long', 'true');
    end;
    dtBoolean: begin
      XmlWriter.WriteAttributeString('dt:type', 'boolean');
      XmlWriter.WriteAttributeString('dt:maxLength', IntToStr(FieldDesc.Size));
    end;
    dtString, dtExtString, dtMemo, dtWideString, dtExtWideString, dtWideMemo: begin
      XmlWriter.WriteAttributeString('dt:type', 'string');
      if FieldDesc.DataType in [dtMemo, dtWideMemo] then
        XmlWriter.WriteAttributeString('dt:maxLength', '2147483647')
      else
        XmlWriter.WriteAttributeString('dt:maxLength', IntToStr(FieldDesc.Length));
      if not (FieldDesc.DataType in [dtWideString, dtExtWideString, dtWideMemo]) then
        XmlWriter.WriteAttributeString('rs:dbtype', 'str');
      if Field.IsBlob and not FieldDesc.Fixed then
        XmlWriter.WriteAttributeString('rs:long', 'true');
    end;
    dtCurrency, dtBCD: begin
      XmlWriter.WriteAttributeString('dt:type', 'number');
      XmlWriter.WriteAttributeString('rs:dbtype', 'currency');
      XmlWriter.WriteAttributeString('dt:maxLength', IntToStr(FieldDesc.Size));
      XmlWriter.WriteAttributeString('rs:precision', IntToStr(FieldDesc.Length));
    end;
    dtDateTime: begin
      XmlWriter.WriteAttributeString('dt:type', 'datetime');
      XmlWriter.WriteAttributeString('rs:dbtype', 'variantdate');
      XmlWriter.WriteAttributeString('dt:maxLength', IntToStr(FieldDesc.Size));
      XmlWriter.WriteAttributeString('rs:scale', IntToStr(FieldDesc.Scale));
      XmlWriter.WriteAttributeString('rs:precision', IntToStr(FieldDesc.Length));
    end;
    dtDate: begin
      XmlWriter.WriteAttributeString('dt:type', 'date');
      XmlWriter.WriteAttributeString('dt:maxLength', IntToStr(FieldDesc.Size));
      XmlWriter.WriteAttributeString('rs:scale', IntToStr(FieldDesc.Scale));
      XmlWriter.WriteAttributeString('rs:precision', IntToStr(FieldDesc.Length));
    end;
    dtTime: begin
      XmlWriter.WriteAttributeString('dt:type', 'time');
      XmlWriter.WriteAttributeString('dt:maxLength', IntToStr(FieldDesc.Size));
      XmlWriter.WriteAttributeString('rs:scale', IntToStr(FieldDesc.Scale));
      XmlWriter.WriteAttributeString('rs:precision', IntToStr(FieldDesc.Length));
    end;
    dtFloat: begin
      if FieldDesc.Length <= 7 then begin
        XmlWriter.WriteAttributeString('dt:type', 'r4');
        XmlWriter.WriteAttributeString('dt:maxLength', '4');
      end
      else begin
        XmlWriter.WriteAttributeString('dt:type', 'float');
        XmlWriter.WriteAttributeString('dt:maxLength', IntToStr(FieldDesc.Size));
      end;
      XmlWriter.WriteAttributeString('rs:precision', IntToStr(FieldDesc.Length));
    end;
    dtGuid: begin
      XmlWriter.WriteAttributeString('dt:type', 'uuid');
      XmlWriter.WriteAttributeString('dt:maxLength', '16');
    end;
    dtInt32: begin
      XmlWriter.WriteAttributeString('dt:type', 'int');
      XmlWriter.WriteAttributeString('dt:maxLength', IntToStr(FieldDesc.Size));
      XmlWriter.WriteAttributeString('rs:precision', '10');
    end;
  {$IFDEF VER6P}
  {$IFNDEF FPC}
    dtFmtBCD: begin
      XmlWriter.WriteAttributeString('dt:type', 'number');
      XmlWriter.WriteAttributeString('rs:dbtype', 'numeric');
      XmlWriter.WriteAttributeString('dt:maxLength', IntToStr(FieldDesc.Size));
      XmlWriter.WriteAttributeString('rs:scale', IntToStr(FieldDesc.Scale));
      XmlWriter.WriteAttributeString('rs:precision', IntToStr(FieldDesc.Length));
    end;
  {$ENDIF}
  {$ENDIF}
    dtInt16: begin
      XmlWriter.WriteAttributeString('dt:type', 'i2');
      XmlWriter.WriteAttributeString('dt:maxLength', IntToStr(FieldDesc.Size));
      XmlWriter.WriteAttributeString('rs:precision', '5');
    end;
    dtInt8: begin
      XmlWriter.WriteAttributeString('dt:type', 'i1');
      XmlWriter.WriteAttributeString('dt:maxLength', IntToStr(FieldDesc.Size));
      XmlWriter.WriteAttributeString('rs:precision', IntToStr(FieldDesc.Length));
    end;
    dtLongword: begin
      XmlWriter.WriteAttributeString('dt:type', 'ui4');
      XmlWriter.WriteAttributeString('dt:maxLength', IntToStr(FieldDesc.Size));
      XmlWriter.WriteAttributeString('rs:precision', IntToStr(FieldDesc.Length));
    end;
    dtWord: begin
      XmlWriter.WriteAttributeString('dt:type', 'ui2');
      XmlWriter.WriteAttributeString('dt:maxLength', IntToStr(FieldDesc.Size));
      XmlWriter.WriteAttributeString('rs:precision', IntToStr(FieldDesc.Length));
    end;
    dtUnknown{$IFDEF VER5P}, dtVariant{$ENDIF}:
      XmlWriter.WriteAttributeString('dt:type', 'string')
    else
      DatabaseError(SDataTypeNotSupported, FDataSet);
  end;

  if FieldDesc.Fixed and not (FieldDesc.DataType in [dtUnknown{$IFDEF VER5P}, dtVariant{$ENDIF}]) then
    XmlWriter.WriteAttributeString('rs:fixedlength', 'true');
    
  if Field.Required and not Field.ReadOnly then
    XmlWriter.WriteAttributeString('rs:maybenull', 'false');
end;

procedure TDataSetService.WriteFieldXMLAttributeType(Field: TField; FieldDesc: TFieldDesc;
  const FieldAlias: _string; XmlWriter: XMLTextWriter);
begin
  if FieldAlias = '' then
    XmlWriter.WriteAttributeString('name', FieldDesc.Name)
  else begin
    XmlWriter.WriteAttributeString('name', FieldAlias);
    XmlWriter.WriteAttributeString('rs:name',  FieldDesc.Name);
  end;

  XmlWriter.WriteAttributeString('rs:number',  IntToStr(FieldDesc.FieldNo));
  if not Field.Required and not Field.ReadOnly then /// Can't use FieldDesc.Required, see "Required and FLocalConstraints" line in TMemDataSet.CreateFieldDefs for details
    XmlWriter.WriteAttributeString('rs:nullable', 'true');
  if not Field.ReadOnly then
    XmlWriter.WriteAttributeString('rs:writeunknown', 'true');
  // XmlWriter.WriteAttributeString('rs:basecatalog', '');

  if FieldDesc.ActualName <> '' then
    XmlWriter.WriteAttributeString('rs:basecolumn', FieldDesc.ActualName);
  if FieldDesc.IsKey then
    XmlWriter.WriteAttributeString('rs:keycolumn', 'true');
  if FDataSet.GetFieldDesc(Field).IsAutoIncrement then
    XmlWriter.WriteAttributeString('rs:autoincrement', 'true');
end;

function TDataSetService.GetFieldXMLValue(Field: TField; FieldDesc: TFieldDesc): WideString;
var
  Buffer: TBytes;
{$IFDEF FPC}
  sBuffer: string;
{$ENDIF}
  Blob: TBlob;
  Piece: PPieceHeader;
{$IFDEF CLR}
  sb: StringBuilder;
  Bytes: TBytes;
{$ELSE}
  sbOffset: integer;
  StrValue: string;
{$ENDIF}

  function EncodeXMLDateTime(Value: TDateTime): string;
  var
    Year, Month, Day, Hour, Minute, Second, MilliSecond: Word;
  begin
    DecodeDateTime(Value, Year, Month, Day, Hour, Minute, Second, MilliSecond);
    Result := StringReplace(Format('%4d%s%2d%s%2d', [Year, '-', Month, '-', Day]),
      ' ', '0', [rfReplaceAll]);
    Result := Result + 'T';
    Result := StringReplace(Result + Format('%2d%s%2d%s%2d', [Hour, ':', Minute, ':', Second]),
      ' ', '0', [rfReplaceAll]);
  end;

  function EncodeXMLTime(Value: TDateTime): string;
  var
    Hour, Minute, Second, MilliSecond: Word;
  begin
    DecodeTime(Value, Hour, Minute, Second, MilliSecond);
    Result := StringReplace(Format('%2d%s%2d%s%2d', [Hour, ':', Minute, ':', Second]),
      ' ', '0', [rfReplaceAll]);
  end;

begin
  Result := '';
  SetLength(Buffer, 0);
  case FieldDesc.DataType of
    dtBoolean:
      Result := BoolToStr(Field.AsBoolean, True);
    dtInt8, dtInt16, dtInt32, dtUInt16, dtUInt32, dtInt64:
      Result := Field.AsString;
    dtFloat:
      Result := ChangeDecimalSeparator(FloatToStr(Field.AsFloat), {$IFDEF VER16P}FormatSettings.{$ENDIF}DecimalSeparator, '.');
    dtCurrency, dtBcd:
      Result := ChangeDecimalSeparator(CurrToStr(Field.Value), {$IFDEF VER16P}FormatSettings.{$ENDIF}DecimalSeparator, '.');
    dtDate, dtDateTime:
      Result := EncodeXMLDateTime(Field.AsDateTime);
    dtTime:
      Result := EncodeXMLTime(Field.AsDateTime);
  {$IFDEF VER6P}
  {$IFNDEF FPC}
    dtFmtBCD: begin
      Result := BcdToStr(Field.AsBCD);
    end;
  {$ENDIF}
  {$ENDIF}
    dtBytes, dtVarBytes, dtExtVarBytes: begin
    {$IFNDEF FPC}
      Buffer := Field.Value;
    {$ELSE}
      sBuffer := Field.Text;
    {$ENDIF}
    {$IFDEF CLR}
      SetLength(Bytes, Length(Buffer) * 2);
      BinToHex(Buffer, 0, Bytes, 0, Length(Buffer));
      Result := Encoding.Default.GetString(Bytes);
    {$ELSE}
    {$IFNDEF FPC}
      SetLength(StrValue, Length(Buffer) * 2);
      BinToHex(@Buffer[0], PChar(StrValue), Length(Buffer));
      Result := StrValue;
    {$ELSE}
      SetLength(StrValue, Length(sBuffer) * 2);
      BinToHex(PChar(sBuffer), PChar(StrValue), Length(Buffer));
      Result := StrValue;
    {$ENDIF}
    {$ENDIF}
    end;
    dtBlob: begin
      Blob := FDataSet.InternalGetBlob(FieldDesc);
      Piece := Blob.FirstPiece;
    {$IFDEF CLR}
      sb := StringBuilder.Create;
      try
        while IntPtr(Piece) <> nil do begin
          SetLength(Buffer, Piece.Used);
          Marshal.Copy(PtrOffset(Piece, sizeof(TPieceHeader)), Buffer, 0, Piece.Used);
          SetLength(Bytes, Length(Buffer) * 2);
          BinToHex(Buffer, 0, Bytes, 0, Length(Buffer));
          sb.Append(Encoding.Default.GetString(Bytes));
          Piece := Piece.Next;
        end;
        Result := sb.ToString;
      finally
        sb.Free;
      end;
    {$ELSE}
      SetLength(StrValue, Blob.Size * 2);
      sbOffset := 0;
      while Piece <> nil do begin
        BinToHex(PtrOffset(Piece, sizeof(TPieceHeader)),
          PChar(PtrOffset(PChar(StrValue), sbOffset)), Piece.Used);
        sbOffset := sbOffset + Piece.Used * 2;
        Piece := Piece.Next;
      end;
      Result := StrValue;
    {$ENDIF}
    end;
    else
      if Field is TWideStringField then
        Result := Field.Value
    {$IFDEF VER10P}
    {$IFNDEF CLR}
      else
      if Field is TWideMemoField then
        Result := Field.AsWideString
    {$ENDIF}
    {$ENDIF}
      else
        Result := Field.AsString;
  end;
end;

end.
