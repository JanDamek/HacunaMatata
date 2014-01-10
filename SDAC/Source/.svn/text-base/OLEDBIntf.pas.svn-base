
{$IFNDEF UNIDACPRO}

{$I Sdac.inc}

unit OLEDBIntf;
{$ENDIF}

interface

uses
  Windows, ActiveX,
{$IFDEF VER6P}
  FMTBcd,
{$ELSE}
  DB,
{$ENDIF}
  {$IFNDEF UNIDACPRO}OLEDBC{$ELSE}OLEDBCUni{$ENDIF},
{$IFNDEF CLR}
  CLRClasses,
{$ENDIF}
  CRTypes;

type
{$IFDEF FPC}
  POleStrList = ^TOleStrList;
  TOleStrList = array[0..65535] of POleStr;
{$ENDIF}
  PIUnknown = ^IUnknown;
  PUintArray = IntPtr;
  TUintArray = array of UINT;
  PPByte = ^PByte;
  PPByteArray = IntPtr;
  TPByteArray = array of PByte;

  TVariantArray = array [0..0] of OleVariant;
  PVariantArray = ^TVariantArray;

type
  PDBTimeStamp = ^TDBTimeStamp;
  DBTIMESTAMP = {$IFNDEF WIN64}packed{$ENDIF} record
    year: Smallint;
    month: Word;
    day: Word;
    hour: Word;
    minute: Word;
    second: Word;
    fraction: UINT;
  end;
  TDBTimeStamp = DBTIMESTAMP;
  {$EXTERNALSYM DBTIMESTAMP}

  PDBTime = ^TDBTime;
  DBTIME = {$IFNDEF WIN64}packed{$ENDIF} record
    hour: Word;
    minute: Word;
    second: UINT;
    fraction: UINT;
  end;
  TDBTime = DBTIME;
  {$EXTERNALSYM DBTIME}
  
type
  PBoid = ^TBoid;
  BOID = {$IFNDEF WIN64}packed{$ENDIF} record
    rgb_: array[0..15] of Byte;
  end;
  TBoid = BOID;
  {$EXTERNALSYM BOID}

  XACTTRANSINFO = {$IFNDEF WIN64}packed{$ENDIF} record
    uow: BOID;
    isoLevel: TISOLEVEL;
    isoFlags: UINT;
    grfTCSupported: UINT;
    grfRMSupported: UINT;
    grfTCSupportedRetaining: UINT;
    grfRMSupportedRetaining: UINT;
  end;
  TXactTransInfo = XACTTRANSINFO;
  {$EXTERNALSYM XACTTRANSINFO}

  XACTOPT = {$IFNDEF WIN64}packed{$ENDIF} record
    ulTimeout: UINT;
    szDescription: array[0..39] of Shortint;
  end;
  TXActOpt = XACTOPT;
  {$EXTERNALSYM XACTOPT}

  XACTSTATS = {$IFNDEF WIN64}packed{$ENDIF} record
    cOpen: UINT;
    cCommitting: UINT;
    cCommitted: UINT;
    cAborting: UINT;
    cAborted: UINT;
    cInDoubt: UINT;
    cHeuristicDecision: UINT;
    timeTransactionsUp: FILETIME;
  end;
  TXactStats = XACTSTATS;
  {$EXTERNALSYM XACTSTATS}

  PDBBindExt = ^TDBBindExt;
  DBBINDEXT = {$IFNDEF WIN64}packed{$ENDIF} record
    pExtension: IntPtr; // PByte;
    ulExtension: DBCOUNTITEM;
  end;
  TDBBindExt = DBBINDEXT;
  {$EXTERNALSYM DBBINDEXT}

  PDBOBJECT = ^TDBObject;
  DBOBJECT = {$IFNDEF WIN64}packed{$ENDIF} record
    dwFlags: UINT;
    iid: TGUID;
  end;
  TDBObject = DBOBJECT;
  {$EXTERNALSYM DBOBJECT}

  PDBBinding = ^TDBBinding;
  DBBINDING = {$IFNDEF WIN64}packed{$ENDIF} record
    iOrdinal: DBORDINAL;
    obValue: DBBYTEOFFSET;
    obLength: DBBYTEOFFSET;
    obStatus: DBBYTEOFFSET;
    pTypeInfo: IntPtr; // IUnknown;
    pObject: PDBOBJECT;
    pBindExt: PDBBINDEXT;
    dwPart: DBPART;
    dwMemOwner: DBMEMOWNER;
    eParamIO: DBPARAMIO;
    cbMaxLen: DBLENGTH;
    dwFlags: UINT;
    wType: DBTYPE;
    bPrecision: Byte;
    bScale: Byte;
  end;
  TDBBinding = DBBINDING;
{ $IFNDEF VER125}
  { $EXTERNALSYM DBBINDING}
{ $ENDIF}

//  PDBBindingArray = ^TDBBindingArray;
//  TDBBindingArray = array[0..MAXBOUND] of TDBBinding;
  PDBBindingArray = IntPtr;
  TDBBindingArray = array of TDBBinding;

  DBIDNAME = record
    case Integer of
      0: (pwszName: PWideChar);
      1: (ulPropid: UINT);
  end;
  TDBIDName = DBIDNAME;

  DBIDGUID = record
    case Integer of
      0: (guid: TGUID);
      1: (pguid: ^TGUID);
  end;
  TDBIDGuid = DBIDGUID;

  TGUIDArray = array [0..0] of TGUID;
  PGUIDArray = ^TGUIDArray;

  DBPROPID = UINT;
  {$EXTERNALSYM DBPROPID}
  PDBPROPID = ^DBPROPID;
  PDBPropIDArray = ^TDBPropIDArray;
  TDBPropIDArray = array[0..MAXBOUND] of DBPROPID;

  PPDBID = ^PDBID;
  PDBID = ^DBID;
  DBID = {$IFNDEF WIN64}packed{$ENDIF} record
    uGuid: DBIDGUID;
    eKind: DBKIND;
    uName: DBIDNAME;
  end;
  TDBID = DBID;
  {$EXTERNALSYM DBID}

const
  OffsetOf_DBID_uGuid = 0;
  OffsetOf_DBID_eKind = OffsetOf_DBID_uGuid + SizeOf(DBIDGUID); //16
  OffsetOf_DBID_uName = OffsetOf_DBID_eKind + SizeOf(NativeUint); // Should be 20, but Marshal.OffsetOf return 24 in x64 bit

type
  PDBIDArray = ^TDBIDArray;
  TDBIDArray = array[0..MAXBOUND] of TDBID;

  PDBPropIDSet = ^TDBPropIDSet;
  DBPROPIDSET = {$IFNDEF WIN64}packed{$ENDIF} record
    rgPropertyIDs: PDBPROPID; // PDBPropIDArray;
    cPropertyIDs: UINT;
    guidPropertySet: TGUID;
  end;
  TDBPropIDSet = DBPROPIDSET;
  {$EXTERNALSYM DBPROPIDSET}

  PDBPropIDSetArray = ^TDBPropIDSetArray;
  TDBPropIDSetArray = array[0..MAXBOUND] of TDBPropIDSet;

  PDBProp = ^TDBProp;
  DBPROP = {$IFNDEF WIN64}packed{$ENDIF} record
    dwPropertyID: DBPROPID;
    dwOptions: DBPROPOPTIONS;
    dwStatus: DBPROPSTATUS;
    colid: DBID;
    vValue: OleVariant;
  end;
  TDBProp = DBPROP;
  {$EXTERNALSYM DBPROP}

  PDBPropArray = IntPtr;
  TDBPropArray = array[0..MAXBOUND] of TDBProp;

  PPDBPropSet = ^PDBPropSet;
  PDBPropSet = ^TDBPropSet;
  DBPROPSET = {$IFNDEF WIN64}packed{$ENDIF} record
    rgProperties: PDBPropArray;
    cProperties: UINT;
    guidPropertySet: TGUID;
  end;
  TDBPropSet = DBPROPSET;
  {$EXTERNALSYM DBPROPSET}

  PPDBPropSetArray = IntPtr;
  PDBPropSetArray = ^TDBPropSetArray;
  TDBPropSetArray = array[0..MAXBOUND] of TDBPropSet;

  PDBPropInfo = ^TDBPropInfo;
  DBPROPINFO = {$IFNDEF WIN64}packed{$ENDIF} record
    pwszDescription: PWideChar;
    dwPropertyID: DBPROPID;
    dwFlags: DBPROPFLAGS;
    vtType: Word;
    vValues: OleVariant;
  end;
  TDBPropInfo = DBPROPINFO;
  {$EXTERNALSYM DBPROPINFO}

  PDBPropInfoArray = ^TDBPropInfoArray;
  TDBPropInfoArray = array[0..MAXBOUND] of TDBPropInfo;

  PDBPropInfoSet = IntPtr;
(*
  PDBPropInfoSet = ^TDBPropInfoSet;
  DBPROPINFOSET = packed record
    rgPropertyInfos: PDBPropInfoArray;
    cPropertyInfos: UINT;
    guidPropertySet: TGUID;
  end;
  TDBPropInfoSet = DBPROPINFOSET;
  {$EXTERNALSYM DBPROPINFOSET}
  *)

  PDBParams = ^TDBPARAMS;
  DBPARAMS = {$IFNDEF WIN64}packed{$ENDIF} record
    pData: IntPtr;
    cParamSets: DB_UPARAMS;
    HACCESSOR: HACCESSOR;
  end;
  TDBParams = DBPARAMS;
  {$EXTERNALSYM DBPARAMS}

  PDBColumnInfo = ^TDBColumnInfo;
  DBCOLUMNINFO = {$IFNDEF WIN64}packed{$ENDIF} record
    pwszName: PWideChar;
    pTypeInfo: IntPtr;
    iOrdinal: DBORDINAL;
    dwFlags: DBCOLUMNFLAGS;
    ulColumnSize: DBLENGTH;
    wType: DBTYPE;
    bPrecision: Byte;
    bScale: Byte;
    columnid: DBID;
  end;
  TDBColumnInfo = DBCOLUMNINFO;
  {$EXTERNALSYM DBCOLUMNINFO}

  PDBColumnInfoArray = ^TDBColumnInfoArray;
  TDBColumnInfoArray = array[0..MAXCOLS] of TDBColumnInfo;

  PDBLiteralInfo = ^DBLiteralInfo;
  DBLITERALINFO = {$IFNDEF WIN64}packed{$ENDIF} record
    pwszLiteralValue: PWideChar;
    pwszInvalidChars: PWideChar;
    pwszInvalidStartingChars: PWideChar;
    lt: DBLITERAL;
    fSupported: BOOL;
    cchMaxLen: UINT;
  end;
  TDBLiteralInfo = DBLITERALINFO;
  {$EXTERNALSYM DBLITERALINFO}

  PDBLiteralInfoArray = ^TDBLiteralInfoArray;
  TDBLiteralInfoArray = array[0..MAXBOUND] of TDBLiteralInfo;

  PDBColumnDesc = ^TDBColumnDesc;
  DBCOLUMNDESC = {$IFNDEF WIN64}packed{$ENDIF} record
    pwszTypeName: PWideChar;
    pTypeInfo: IntPtr;
    rgPropertySets: PDBPROPSET;
    pclsid: IntPtr;
    cPropertySets: UINT;
    ulColumnSize: DBLENGTH;
    dbcid: DBID;
    wType: DBTYPE;
    bPrecision: Byte;
    bScale: Byte;
  end;
  TDBColumnDesc = DBCOLUMNDESC;
  {$EXTERNALSYM DBCOLUMNDESC}

  PErrorInfo = ^TErrorInfo;
  ERRORINFO = {$IFNDEF WIN64}packed{$ENDIF} record
    hrError: HResult;
    dwMinor: UINT;
    clsid: TGUID;
    iid: TGUID;
    dispid: Integer;
  end;
  TErrorInfo = ERRORINFO;
  {$EXTERNALSYM ERRORINFO}

//  PDBParamInfo = IntPtr;
  PDBParamInfo = ^TDBParamInfo;
  DBPARAMINFO = {$IFNDEF WIN64}packed{$ENDIF} record
    dwFlags: DBPARAMFLAGS;
    iOrdinal: DBORDINAL;
    pwszName: PWideChar;
    pTypeInfo: ITypeInfo;
    ulParamSize: DBLENGTH;
    wType: DBTYPE;
    bPrecision: Byte;
    bScale: Byte;
  end;
  TDBParamInfo = DBPARAMINFO;
  {$EXTERNALSYM DBPARAMINFO}

  PDBParamBindInfo = ^TDBParamBindInfo;
  DBPARAMBINDINFO = {$IFNDEF WIN64}packed{$ENDIF} record
    pwszDataSourceType: PWideChar;
    pwszName: PWideChar;
    ulParamSize: DBLENGTH;
    dwFlags: DBPARAMFLAGS;
    bPrecision: Byte;
    bScale: Byte;
  end;
  TDBParamBindInfo = DBPARAMBINDINFO;
  {$EXTERNALSYM DBPARAMBINDINFO}

  PDBParamBindInfoArray = ^TDBParamBindInfoArray;
  TDBParamBindInfoArray = array[0..MAXBOUND] of TDBParamBindInfo;

  PSSParamProps = ^TSSParamProps;
  SSPARAMPROPS = {$IFNDEF WIN64}packed{$ENDIF} record
    iOrdinal: DBORDINAL;
    cPropertySets: UINT;
    rgPropertySets: PPDBPropSetArray;
  end;
  TSSParamProps = SSPARAMPROPS;

  PSSParamPropsArray = ^TSSParamPropsArray;
  TSSParamPropsArray = array[0..MAXBOUND] of TSSParamProps;

  PDBNumeric = ^TDBNumeric;
  DB_NUMERIC = {$IFNDEF WIN64}packed{$ENDIF} record
    precision: Byte;
    scale: Byte;
    sign: Byte;
    val: array[0..15] of Byte;
  end;
  TDBNumeric = DB_NUMERIC;
  {$EXTERNALSYM DB_NUMERIC}

  // Multi Language support
  
  PMIMECPInfo = ^TMIMECPInfo;
  MIMECPINFO = record
    dwFlags: DWORD;
    uiCodePage: UINT;
    uiFamilyCodePage: UINT;
    wszDescription: array [0..63] of WideChar;
    wszWebCharset: array [0..49] of WideChar;
    wszHeaderCharset: array [0..49] of WideChar;
    wszBodyCharset: array [0..49] of WideChar;
    wszFixedWidthFont: array [0..31] of WideChar;
    wszProportionalFont: array [0..31] of WideChar;
    bGDICharset: BYTE;
  end;
  TMIMECPInfo = MIMECPINFO;
  { $EXTERNALSYM MIMECPINFO}

  PMIMECSetInfo = ^TMIMECSetInfo;
  MIMECSETINFO = record
    uiCodePage: UINT;
    uiInternetEncoding: UINT;
    wszCharset: array [0..49] of WideChar;
  end;
  TMIMECSetInfo = MIMECSETINFO;
  { $EXTERNALSYM MIMECSETINFO}

  PRFC1766Info = ^TRFC1766Info;
  RFC1766INFO = record
    alcid: LCID;
    wszRfc1766: array [0..5] of WideChar;
    wszLocaleName: array [0..31] of WideChar;
  end;
  TRFC1766Info = RFC1766INFO;
  { $EXTERNALSYM RFC1766INFO}

// the structure returned by  ISQLServerErrorInfo::GetSQLServerInfo
  SSERRORINFO_ = record
    pwszMessage: PWideChar;
    pwszServer: PWideChar;
    pwszProcedure: PWideChar;
    lNative: Integer;
    bState: BYTE;
    bClass: BYTE;
    wLineNumber: WORD;
  end;
  SSERRORINFO = record
    pwszMessage: WideString;
    pwszServer: WideString;
    pwszProcedure: WideString;
    lNative: Integer;
    bState: BYTE;
    bClass: BYTE;
    wLineNumber: WORD;
  end;
  PSSERRORINFO = ^SSERRORINFO_;

const
  SizeOfTDBNumeric = SizeOf(TDBNumeric);

// *********************************************************************//
//  DBID Values
// *********************************************************************//
const
  DB_NULLID: DBID = (uguid: (guid: (D1: 0; D2: 0; D3:0; D4: (0, 0, 0, 0, 0, 0, 0, 0))); ekind: DBKIND_GUID_PROPID; uname: (ulpropid:0));
  {$EXTERNALSYM DB_NULLID}

type
// *********************************************************************//
// Interface: ITransaction
// GUID:      {0FB15084-AF41-11CE-BD2B-204C4F4F5020}
// *********************************************************************//
{$IFNDEF VER6P}
  {$NODEFINE ITransaction}
{$ENDIF}
  ITransaction = interface(IUnknown)
    ['{0FB15084-AF41-11CE-BD2B-204C4F4F5020}']
    function Commit(fRetaining: BOOL; grfTC: UINT; grfRM: UINT): HResult; stdcall;
    function Abort(pboidReason: PBOID; fRetaining: BOOL; fAsync: BOOL): HResult; stdcall;
    function GetTransactionInfo(out pinfo: XACTTRANSINFO): HResult; stdcall;
  end;

// *********************************************************************//
// Interface: ITransactionOptions
// GUID:      {3A6AD9E0-23B9-11CF-AD60-00AA00A74CCD}
// *********************************************************************//
  ITransactionOptions = interface(IUnknown)
    ['{3A6AD9E0-23B9-11CF-AD60-00AA00A74CCD}']
    function SetOptions(var pOptions: XACTOPT): HResult; stdcall;
    function GetOptions(var pOptions: XACTOPT): HResult; stdcall;
  end;

// *********************************************************************//
// Interface: IAccessor
// GUID:      {0C733A8C-2A1C-11CE-ADE5-00AA0044773D}
// *********************************************************************//
  IAccessor = interface(IUnknown)
    ['{0C733A8C-2A1C-11CE-ADE5-00AA0044773D}']
    function AddRefAccessor(HACCESSOR: HACCESSOR; pcRefCount: PDBREFCOUNT): HResult; stdcall;
    function CreateAccessor(dwAccessorFlags: DBACCESSORFLAGS; cBindings: DBCOUNTITEM; rgBindings: PDBBindingArray;
      cbRowSize: DBLENGTH; var phAccessor: HACCESSOR; rgStatus: PDBBINDSTATUSArray): HResult; stdcall;
    function GetBindings(HACCESSOR: HACCESSOR; pdwAccessorFlags: PDBACCESSORFLAGS; var pcBindings: DBCOUNTITEM;
      out prgBindings: PDBBinding): HResult; stdcall;
    function ReleaseAccessor(HACCESSOR: HACCESSOR; pcRefCount: PDBREFCOUNT): HResult; stdcall;
  end;

// *********************************************************************//
// Interface: IRowset
// GUID:      {0C733A7C-2A1C-11CE-ADE5-00AA0044773D}
// *********************************************************************//
  IRowset = interface(IUnknown)
    ['{0C733A7C-2A1C-11CE-ADE5-00AA0044773D}']
    function AddRefRows(cRows: DBCOUNTITEM; rghRows: PHROWArray; rgRefCounts: PDBREFCOUNTArray;
      rgRowStatus: PDBROWSTATUSArray): HResult; stdcall;
    function GetData(HROW: HROW; HACCESSOR: HACCESSOR; pData: IntPtr): HResult; stdcall;
    function GetNextRows(hReserved: HCHAPTER; lRowsOffset: DBROWOFFSET; cRows: DBROWCOUNT;
      out pcRowsObtained: DBCOUNTITEM; var prghRows: PHROWArray): HResult; stdcall;
    function ReleaseRows(
      cRows: DBCOUNTITEM;
      rghRows: PHROWArray;
      rgRowOptions: PDBROWOPTIONSArray;
      rgRefCounts: PDBREFCOUNTArray;
      rgRowStatus: PDBROWSTATUSArray): HResult; stdcall;
    function RestartPosition(hReserved: HCHAPTER): HResult; stdcall;
  end;

// *********************************************************************//
// Interface: IRowsetInfo
// GUID:      {0C733A55-2A1C-11CE-ADE5-00AA0044773D}
// *********************************************************************//
  IRowsetInfo = interface(IUnknown)
    ['{0C733A55-2A1C-11CE-ADE5-00AA0044773D}']
    function GetProperties(cPropertyIDSets: UINT; rgPropertyIDSets: PDBPropIDSetArray;
      out pcPropertySets: UINT; out prgPropertySets: PDBPropSet): HResult; stdcall;
    function GetReferencedRowset(iOrdinal: DBORDINAL; const riid: TGUID;
      out ppReferencedRowset: IUnknown): HResult; stdcall;
    function GetSpecification(const riid: TGUID; out ppSpecification: IUnknown): HResult; stdcall;
  end;

// *********************************************************************//
// Interface: IRowsetLocate
// GUID:      {0C733A7D-2A1C-11CE-ADE5-00AA0044773D}
// *********************************************************************//
  IRowsetLocate = interface(IRowset)
    ['{0C733A7D-2A1C-11CE-ADE5-00AA0044773D}']
    function Compare(hReserved: HCHAPTER; cbBookmark1: DBBKMARK; pBookmark1: PByte;
      cbBookmark2: DBBKMARK; pBookmark2: PByte; out pComparison: DBCOMPARE): HResult; stdcall;
    function GetRowsAt(hReserved1: HWATCHREGION; hReserved2: HCHAPTER; cbBookmark: DBBKMARK;
      pBookmark: PByte; lRowsOffset: DBROWOFFSET; cRows: DBROWCOUNT;
      out pcRowsObtained: DBCOUNTITEM; var prghRows: PHROWArray): HResult; stdcall;
    function GetRowsByBookmark(hReserved: HCHAPTER; cRows: DBCOUNTITEM; rgcbBookmarks: PDBBKMARKArray;
      rgpBookmarks: PPByteArray; rghRows: PHROWArray; rgRowStatus: PDBROWSTATUSArray): HResult; stdcall;
    function Hash(hReserved: HCHAPTER; cBookmarks: DBBKMARK; rgcbBookmarks: PDBBKMARKArray;
      rgpBookmarks: PPByteArray; rgHashedValues: PDBHASHVALUEArray;
      rgBookmarkStatus: PDBROWSTATUSArray): HResult; stdcall;
  end;

// *********************************************************************//
// Interface: IRowsetChange
// GUID:      {0C733A05-2A1C-11CE-ADE5-00AA0044773D}
// *********************************************************************//
  IRowsetChange = interface(IUnknown)
    ['{0C733A05-2A1C-11CE-ADE5-00AA0044773D}']
    function DeleteRows(hReserved: HCHAPTER; cRows: DBCOUNTITEM; rghRows: PHROWArray;
      rgRowStatus: PDBROWSTATUSArray): HResult; stdcall;
    function SetData(HROW: HROW; HACCESSOR: HACCESSOR; pData: IntPtr): HResult; stdcall;
    function InsertRow(hReserved: HCHAPTER; HACCESSOR: HACCESSOR; pData: IntPtr;
      out phRow: HROW): HResult; stdcall;
  end;

// *********************************************************************//
// Interface: IRowsetUpdate
// GUID:      {0C733A6D-2A1C-11CE-ADE5-00AA0044773D}
// *********************************************************************//
  IRowsetUpdate = interface(IRowsetChange)
    ['{0C733A6D-2A1C-11CE-ADE5-00AA0044773D}']
    function GetOriginalData(HROW: HROW; HACCESSOR: HACCESSOR; pData: IntPtr): HResult; stdcall;
    function GetPendingRows(hReserved: HCHAPTER; dwRowStatus: DBPENDINGSTATUS; pcPendingRows: PDBCOUNTITEM;
      prgPendingRows: PPHROW; prgPendingStatus: PPDBPENDINGSTATUS): HResult; stdcall;
    function GetRowStatus(hReserved: HCHAPTER; cRows: DBCOUNTITEM; rghRows: PHROWArray;
      rgPendingStatus: PDBPENDINGSTATUSArray): HResult; stdcall;
    function Undo(hReserved: HCHAPTER; cRows: DBCOUNTITEM; rghRows: PHROWArray;
      pcRowsUndone: PDBCOUNTITEM; prgRowsUndone: PPHROW;
      prgRowStatus: PPDBROWSTATUS): HResult; stdcall;
    function Update(hReserved: HCHAPTER; cRows: DBCOUNTITEM; rghRows: PHROWArray;
      pcRows: PDBCOUNTITEM; prgRows: PPHROW; out prgRowStatus: PDBROWSTATUS): HResult; stdcall;
  end;

// *********************************************************************//
// Interface: IMultipleResults
// GUID:      {0C733A90-2A1C-11CE-ADE5-00AA0044773D}
// *********************************************************************//
  IMultipleResults = interface(IUnknown)
    ['{0C733A90-2A1C-11CE-ADE5-00AA0044773D}']
    function GetResult(
      const punkOuter: IUnknown;
      reserved: DBRESULTFLAG;
      const riid: TGUID;
      out pcRowsAffected: DBROWCOUNT;
      out ppRowset: IUnknown): HResult; stdcall;
  end;

// *********************************************************************//
// Interface: ICommandPrepare
// GUID:      {0C733A26-2A1C-11CE-ADE5-00AA0044773D}
// *********************************************************************//
  ICommandPrepare = interface(IUnknown)
    ['{0C733A26-2A1C-11CE-ADE5-00AA0044773D}']
    function Prepare(cExpectedRuns: UINT): HResult; stdcall;
    function Unprepare: HResult; stdcall;
  end;

// *********************************************************************//
// Interface: ICommandProperties
// GUID:      {0C733A79-2A1C-11CE-ADE5-00AA0044773D}
// *********************************************************************//
  ICommandProperties = interface(IUnknown)
    ['{0C733A79-2A1C-11CE-ADE5-00AA0044773D}']
    function GetProperties(cPropertyIDSets: UINT; rgPropertyIDSets: PDBPropIDSetArray;
      var pcPropertySets: UINT; out prgPropertySets: PDBPropSet): HResult; stdcall;
    function SetProperties(cPropertySets: UINT; rgPropertySets: PDBPropSetArray): HResult; stdcall;
  end;

// *********************************************************************//
// Interface: ICommand
// GUID:      {0C733A63-2A1C-11CE-ADE5-00AA0044773D}
// *********************************************************************//
  ICommand = interface(IUnknown)
    ['{0C733A63-2A1C-11CE-ADE5-00AA0044773D}']
    function Cancel: HResult; stdcall;
    function Execute(
      const punkOuter: IUnknown;
      const riid: TGUID;
      pParams: PDBPARAMS;
      out pcRowsAffected: DBROWCOUNT;
      var ppRowset: IUnknown): HResult; stdcall;
    function GetDBSession(const riid: TGUID; out ppSession: IUnknown): HResult; stdcall;
  end;

// *********************************************************************//
// Interface: ICommandText
// GUID:      {0C733A27-2A1C-11CE-ADE5-00AA0044773D}
// *********************************************************************//
  ICommandText = interface(ICommand)
    ['{0C733A27-2A1C-11CE-ADE5-00AA0044773D}']
    function GetCommandText(
      var pguidDialect: TGUID;
      out ppwszCommand: PWideChar): HResult; stdcall;
    function SetCommandText(
      const rguidDialect: TGUID;
      pwszCommand: PWideChar): HResult; stdcall;
  end;

// *********************************************************************//
// Interface: ICommandWithParameters
// GUID:      {0C733A64-2A1C-11CE-ADE5-00AA0044773D}
// *********************************************************************//
  ICommandWithParameters = interface(IUnknown)
    ['{0C733A64-2A1C-11CE-ADE5-00AA0044773D}']
    function GetParameterInfo(var pcParams: DB_UPARAMS; out prgParamInfo: PDBPARAMINFO;
      ppNamesBuffer: PPOleStr): HResult; stdcall;
    function MapParameterNames(cParamNames: DB_UPARAMS; rgParamNames: POleStrList;
      rgParamOrdinals: PDB_LPARAMSArray): HResult; stdcall;
    function SetParameterInfo(cParams: DB_UPARAMS; rgParamOrdinals: PDB_UPARAMSArray;
      rgParamBindInfo: PDBParamBindInfoArray): HResult; stdcall;
  end;

  // *********************************************************************//
// Interface: IColumnsRowset
// GUID:      {0C733A10-2A1C-11CE-ADE5-00AA0044773D}
// *********************************************************************//
  IColumnsRowset = interface(IUnknown)
    ['{0C733A10-2A1C-11CE-ADE5-00AA0044773D}']
    function GetAvailableColumns(var pcOptColumns: DBORDINAL;
      var prgOptColumns: PDBID): HResult; stdcall;
    function GetColumnsRowset(const punkOuter: IUnknown; cOptColumns: DBORDINAL;
      rgOptColumns: PDBIDArray; const riid: TGUID; cPropertySets: UINT;
      rgPropertySets: PDBPropSetArray; out pColRowset: IUnknown): HResult; stdcall;
  end;

// *********************************************************************//
// Interface: IColumnsInfo
// GUID:      {0C733A11-2A1C-11CE-ADE5-00AA0044773D}
// *********************************************************************//
  IColumnsInfo = interface(IUnknown)
    ['{0C733A11-2A1C-11CE-ADE5-00AA0044773D}']
    function GetColumnInfo(var pcColumns: DBORDINAL; out prgInfo: PDBColumnInfo;
      out ppStringsBuffer: IntPtr): HResult; stdcall;
    function MapColumnIDs(cColumnIDs: DBORDINAL; rgColumnIDs: PDBIDArray;
      rgColumns: PDBORDINALArray): HResult; stdcall;
  end;

// *********************************************************************//
// Interface: IDBCreateCommand
// GUID:      {0C733A1D-2A1C-11CE-ADE5-00AA0044773D}
// *********************************************************************//
  IDBCreateCommand = interface(IUnknown)
    ['{0C733A1D-2A1C-11CE-ADE5-00AA0044773D}']
    function CreateCommand(const punkOuter: IUnknown; const riid: TGUID;
      out ppCommand: IUnknown): HResult; stdcall;
  end;

// *********************************************************************//
// Interface: IDBCreateSession
// GUID:      {0C733A5D-2A1C-11CE-ADE5-00AA0044773D}
// *********************************************************************//
  IDBCreateSession = interface(IUnknown)
    ['{0C733A5D-2A1C-11CE-ADE5-00AA0044773D}']
    function CreateSession(const punkOuter: IUnknown; const riid: TGUID;
      out ppDBSession: IUnknown): HResult; stdcall;
  end;

// *********************************************************************//
// Interface: ISourcesRowset
// GUID:      {0C733A1E-2A1C-11CE-ADE5-00AA0044773D}
// *********************************************************************//
  ISourcesRowset = interface(IUnknown)
    ['{0C733A1E-2A1C-11CE-ADE5-00AA0044773D}']
    function GetSourcesRowset(const punkOuter: IUnknown; const riid: TGUID; cPropertySets: UINT;
      rgProperties: PDBPropSetArray; out ppSourcesRowset: IUnknown): HResult; stdcall;
  end;

// *********************************************************************//
// Interface: IDBProperties
// GUID:      {0C733A8A-2A1C-11CE-ADE5-00AA0044773D}
// *********************************************************************//
  IDBProperties = interface(IUnknown)
    ['{0C733A8A-2A1C-11CE-ADE5-00AA0044773D}']
    function GetProperties(cPropertyIDSets: UINT; rgPropertyIDSets: PDBPropIDSetArray;
      var pcPropertySets: UINT; out prgPropertySets: PDBPropSet): HResult; stdcall;
    function GetPropertyInfo(cPropertyIDSets: UINT; rgPropertyIDSets: PDBPropIDSetArray;
      var pcPropertyInfoSets: UINT; out prgPropertyInfoSets: PDBPropInfoSet;
      ppDescBuffer: PPOleStr): HResult; stdcall;
    function SetProperties(cPropertySets: UINT; rgPropertySets: PDBPropSetArray): HResult; stdcall;
  end;

// *********************************************************************//
// Interface: IDBInitialize
// GUID:      {0C733A8B-2A1C-11CE-ADE5-00AA0044773D}
// *********************************************************************//
  IDBInitialize = interface(IUnknown)
    ['{0C733A8B-2A1C-11CE-ADE5-00AA0044773D}']
    function Initialize: HResult; stdcall;
    function Uninitialize: HResult; stdcall;
  end;

// *********************************************************************//
// Interface: IDBDataSourceAdmin
// GUID:      {0C733A7A-2A1C-11CE-ADE5-00AA0044773D}
// *********************************************************************//
  IDBDataSourceAdmin = interface(IUnknown)
    ['{0C733A7A-2A1C-11CE-ADE5-00AA0044773D}']
    function CreateDataSource(cPropertySets: UINT; rgPropertySets: PDBPropSetArray;
      const punkOuter: IUnknown; const riid: TGUID; out ppDBSession: IUnknown): HResult; stdcall;
    function DestroyDataSource: HResult; stdcall;
    function GetCreationProperties(cPropertyIDSets: UINT; rgPropertyIDSets: PDBPropIDSetArray;
      pcPropertyInfoSets: PUINT; out prgPropertyInfoSets: PDBPropInfoSet;
      ppDescBuffer: PPOleStr): HResult; stdcall;
    function ModifyDataSource(cPropertySets: UINT; rgPropertySets: PDBPropSetArray): HResult; stdcall;
  end;

// *********************************************************************//
// Interface: ISessionProperties
// GUID:      {0C733A85-2A1C-11CE-ADE5-00AA0044773d}
// *********************************************************************//
  ISessionProperties = interface(IUnknown)
    ['{0C733A85-2A1C-11CE-ADE5-00AA0044773d}']
    function GetProperties(cPropertyIDSets: UINT; rgPropertyIDSets: PDBPropIDSetArray;
      var pcPropertySets: UINT; out prgPropertySets: PDBPropSet): HResult; stdcall;
    function SetProperties(cPropertySets: UINT; rgPropertySets: PDBPropSetArray): HResult; stdcall;
  end;

// *********************************************************************//
// Interface: ITransactionLocal
// GUID:      {0C733A5F-2A1C-11CE-ADE5-00AA0044773D}
// *********************************************************************//
  ITransactionLocal = interface(ITransaction)
    ['{0C733A5F-2A1C-11CE-ADE5-00AA0044773D}']
    function GetOptionsObject(out ppOptions: ITransactionOptions): HResult; stdcall;
    function StartTransaction(isoLevel: TISOLEVEL; isoFlags: UINT;
      const pOtherOptions: ITransactionOptions; pulTransactionLevel: PUINT): HResult; stdcall;
  end;

// *********************************************************************//
// Interface: ITransactionJoin
// GUID:      {0C733A5E-2A1C-11CE-ADE5-00AA0044773D}
// *********************************************************************//
   ITransactionJoin = interface(IUnknown)
   ['{0C733A5E-2A1C-11CE-ADE5-00AA0044773D}']
     function GetOptionsObject(out ppOptions: ITransactionOptions): HResult; stdcall;
     function JoinTransaction(const punkTransactionCoord: IUnknown; isoLevel: TISOLEVEL;
       isoFlags: UINT; const pOtherOptions: ITransactionOptions): HResult; stdcall;
   end;

// *********************************************************************//
// Interface: IDBPromptInitialize
// GUID:      {2206CCB0-19C1-11D1-89E0-00C04FD7A829}
// *********************************************************************//
  IDBPromptInitialize = interface(IUnknown)
    ['{2206CCB0-19C1-11D1-89E0-00C04FD7A829}']
    function PromptDataSource(const pUnkOuter: IUnknown; hWndParent: HWND;
      dwPromptOptions: DBPROMPTOPTIONS; cSourceTypeFilter: ULONG;
      rgSourceTypeFilter: PDBSOURCETYPE; pszProviderFilter: POleStr;
      const riid: TIID; var DataSource: IUnknown): HResult; stdcall;
    function PromptFileName(hWndParent: HWND; dwPromptOptions: DBPROMPTOPTIONS;
      pwszInitialDirectory, pwszInitialFile: POleStr;
      var ppwszSelectedFile: POleStr): HResult; stdcall;
  end;

// *********************************************************************//
// Interface: IDataInitialize
// GUID:      {2206CCB1-19C1-11D1-89E0-00C04FD7A829}
// *********************************************************************//
  IDataInitialize = interface(IUnknown)
    ['{2206CCB1-19C1-11D1-89E0-00C04FD7A829}']
    function GetDataSource(
      const pUnkOuter: IUnknown; 
      dwClsCtx: DWORD;
      pwszInitializationString: POleStr; 
      const riid: TIID;
      var DataSource: IUnknown): HResult; stdcall;
    function GetInitializationString(
      const DataSource: IUnknown;
      fIncludePassword: Boolean; 
      out pwszInitString: POleStr): HResult; stdcall;
    function CreateDBInstance(const clsidProvider: TGUID;
      const pUnkOuter: IUnknown; 
      dwClsCtx: DWORD; pwszReserved: POleStr;
      riid: TIID; 
      var DataSource: IUnknown): HResult; stdcall;
    function CreateDBInstanceEx(
      const clsidProvider: TGUID;
      const pUnkOuter: IUnknown; 
      dwClsCtx: DWORD; 
      pwszReserved: POleStr;
      pServerInfo: PCoServerInfo; 
      cmq: ULONG; 
      rgmqResults: PMultiQI): HResult; stdcall;
    function LoadStringFromStorage(
      pwszFileName: POleStr;
      out pwszInitializationString: POleStr): HResult; stdcall;
    function WriteStringToStorage(
      pwszFileName, pwszInitializationString: POleStr;
      dwCreationDisposition: DWORD): HResult; stdcall;
  end;

// *********************************************************************//
// Interface: IDBSchemaRowset
// GUID:      {0c733a7b-2a1c-11ce-ade5-00aa0044773d}
// *********************************************************************//
  IDBSchemaRowset = interface (IUnknown)
    ['{0c733a7b-2a1c-11ce-ade5-00aa0044773d}']
    function GetRowset (const pUnkOuter: IUnknown; const rguidSchema: TGUID;
      cRestrictions: UINT; rgRestrictions: PVariantArray; const riid: TGUID;
      cPropertySets: UINT; rgPropertySets: PDBPropSetArray;
      out ppRowset: IUnknown): HResult; stdcall;
    function GetSchemas (out pcSchemas: UINT; out prgSchemas: PGUIDArray;
      prgRestrictionSupport: PUINTArray): HResult; stdcall;
  end;

// *********************************************************************//
// Interface: IOpenRowset
// GUID:      {0c733a69-2a1c-11ce-ade5-00aa0044773d}
// *********************************************************************//
  IOpenRowset = interface(IUnknown)
    ['{0c733a69-2a1c-11ce-ade5-00aa0044773d}']
    function OpenRowset(const punkOuter: IUnknown; pTableID: PDBID;
      pIndexID: PDBID; const riid: TGUID; cPropertySets: UINT;
      rgPropertySets: PDBPropIDSetArray; out ppReferencedRowset: IUnknown): HRESULT; stdcall;
  end;
  
// *********************************************************************//
// Interface: IRowsetFastLoad
// GUID:      {5CF4CA13-EF21-11d0-97E7-00C04FC2AD98}
// *********************************************************************//
  IRowsetFastLoad = interface(IUnknown)
    ['{5CF4CA13-EF21-11d0-97E7-00C04FC2AD98}']
    function InsertRow(HACCESSOR: HACCESSOR; pData: Pointer): HRESULT; stdcall;
    function Commit(fDone: Boolean): HRESULT; stdcall;
  end;

// *********************************************************************//
// Interface: IDBAsynchStatus
// GUID:      {0c733a95-2a1c-11ce-ade5-00aa0044773d}
// *********************************************************************//
  IDBAsynchStatus = interface(IUnknown)
    ['{0c733a95-2a1c-11ce-ade5-00aa0044773d}']
    function Abort(Chapter: HCHAPTER; Operation: DBASYNCHOP): HRESULT; stdcall;
    function GetStatus(Chapter: HCHAPTER; Operation: DBASYNCHOP; var Progress: DBCOUNTITEM;
      var ProgressMax: DBCOUNTITEM; var AsynchPhase: DBASYNCHPHASE; var StatusText: PWideChar): HRESULT; stdcall;
  end;

// *********************************************************************//
// Interface: ISSAsynchStatus
// GUID:      {1FF1F743-8BB0-4c00-ACC4-C10E43B08FC1}
// *********************************************************************//
  ISSAsynchStatus = interface(IDBAsynchStatus)
    ['{1FF1F743-8BB0-4c00-ACC4-C10E43B08FC1}']
    function WaitForAsynchCompletion(MillisecTimeOut: DWORD): HRESULT; stdcall;
  end;

// *********************************************************************//
// Interface: IErrorRecords
// GUID:      {0c733a67-2a1c-11ce-ade5-00aa0044773d}
// *********************************************************************//
  IErrorRecords = interface (IUnknown)
    ['{0c733a67-2a1c-11ce-ade5-00aa0044773d}']
    function AddErrorRecord(const pErrorInfo: ERRORINFO; dwLookupID: DWORD;
      const pdispparams: DISPPARAMS; const punkCustomError: IUnknown;
      dwDynamicErrorID: DWORD): HResult; stdcall;
    function GetBasicErrorInfo(ulRecordNum: ULONG;
      out pErrorInfo: ERRORINFO): HResult; stdcall;
    function GetCustomErrorObject(ulRecordNum: ULONG; const riid: TGUID;
      out ppObject: IUnknown): HResult; stdcall;
    function GetErrorInfo(ulRecordNum: ULONG; lcid: LCID;
      out ppErrorInfo: IErrorInfo): HResult; stdcall;
    function GetErrorParameters(ulRecordNum: ULONG;
      out pdispparams: DISPPARAMS): HResult; stdcall;
    function GetRecordCount(out pcRecords: ULONG): HResult; stdcall;
  end;
  {$EXTERNALSYM IErrorRecords}

// *********************************************************************//
// Interface: ISQLServerErrorInfo
// GUID:      
// *********************************************************************//
  ISQLServerErrorInfo = interface (IUnknown)
    function GetErrorInfo(out ppErrorInfo: PSSERRORINFO;
      out ppStringsBuffer: IntPtr{PWideChar}): HResult; stdcall;
  end;

// Multi Language support
// *********************************************************************//
// Interface: IEnumCodePage
// GUID:      {275c23e3-3747-11d0-9fea-00aa003f8646}
// *********************************************************************//
  IEnumCodePage = interface(IUnknown)
  ['{275c23e3-3747-11d0-9fea-00aa003f8646}']
    function Clone(out ppEnum: IEnumCodePage): HResult; stdcall;
    function Next(celt: ULONG; var rgelt: MIMECPINFO; var pceltFetched: ULONG): HResult; stdcall;
    function Reset: HResult; stdcall;
    function Skip(celt: ULONG): HResult; stdcall;
  end;

// *********************************************************************//
// Interface: IEnumRfc1766
// GUID:      {3dc39d1d-c030-11d0-b81b-00c04fc9b31f}
// *********************************************************************//
  IEnumRfc1766 = interface(IUnknown)
  ['{3dc39d1d-c030-11d0-b81b-00c04fc9b31f}']
    function Clone(out ppEnum: IEnumRfc1766): HResult; stdcall;
    function Next(celt: ULONG; var rgelt: RFC1766INFO; var pceltFetched: ULONG): HResult; stdcall;
    function Reset: HResult; stdcall;
    function Skip(celt: ULONG): HResult; stdcall;
  end;

// *********************************************************************//
// Interface: IMLangConvertCharset
// GUID:      {D66D6F98-CDAA-11D0-B822-00C04FC9B31F}
// *********************************************************************//
  IMLangConvertCharset = interface(IUnknown)
  ['{D66D6F98-CDAA-11D0-B822-00C04FC9B31F}']
    function Initialize(uiSrcCodePage: UINT; uiDstCodePage: UINT; dwProperty: DWORD): HResult; stdcall;
    function GetSourceCodePage(var puiSrcCodePage: UINT): HResult; stdcall;
    function GetDestinationCodePage(var puiDstCodePage: UINT): HResult; stdcall;
    function GetProperty(var pdwProperty: DWORD): HResult; stdcall;
    function DoConversion(pSrcStr: PBYTE; var pcSrcSize: UINT; pDstStr: PBYTE; var pcDstSize: UINT): HResult; stdcall;
    function DoConversionToUnicode(pSrcStr: PChar; var pcSrcSize: UINT; pDstStr: PWideChar; var pcDstSize: UINT): HResult; stdcall;
    function DoConversionFromUnicode(pSrcStr: PWideChar; var pcSrcSize: UINT; pDstStr: PChar; var pcDstSize: UINT): HResult; stdcall;
  end;

// *********************************************************************//
// Interface: IMultiLanguage
// GUID:      {275C23E1-3747-11D0-9FEA-00AA003F8646}
// *********************************************************************//
  IMultiLanguage = interface(IUnknown)
  ['{275C23E1-3747-11D0-9FEA-00AA003F8646}']
    function GetNumberOfCodePageInfo(var pcCodePage: UINT): HResult; stdcall;
    function GetCodePageInfo(uiCodePage: UINT; var pCodePageInfo: MIMECPINFO): HResult; stdcall;
    function GetFamilyCodePage(uiCodePage: UINT; var puiFamilyCodePage: UINT): HResult; stdcall;
    function EnumCodePages(grfFlags: DWORD; out ppEnumCodePage: IEnumCodePage): HResult; stdcall;
    function GetCharsetInfo(Charset: PWideChar; var pCharsetInfo: MIMECSETINFO): HResult; stdcall;
    function IsConvertible(dwSrcEncoding: DWORD; dwDstEncoding: DWORD): HResult; stdcall;
    function ConvertString(var pdwMode: DWORD; dwSrcEncoding: DWORD; dwDstEncoding: DWORD;
      pSrcStr: PBYTE; var pcSrcSize: UINT; pDstStr: PBYTE; var pcDstSize: UINT): HResult; stdcall;
    function ConvertStringToUnicode(var pdwMode: DWORD; dwEncoding: DWORD; pSrcStr: PChar;
      var pcSrcSize: UINT; pDstStr: PWideChar; var pcDstSize: UINT): HResult; stdcall;
    function ConvertStringFromUnicode(var pdwMode: DWORD; dwEncoding: DWORD; pSrcStr: PWideChar;
      var pcSrcSize: UINT; pDstStr: PChar; var pcDstSize: UINT): HResult; stdcall;
    function ConvertStringReset: HResult; stdcall;
    function GetRfc1766FromLcid(Locale: LCID; var pbstrRfc1766: WideChar): HResult; stdcall;
    function EnumRfc1766(out ppEnumRfc1766: IEnumRfc1766): HResult; stdcall;
    function GetRfc1766Info(Locale: LCID; var pRfc1766Info: RFC1766INFO): HResult; stdcall;
    function CreateConvertCharset(uiSrcCodePage: UINT; uiDstCodePage: UINT; dwProperty: DWORD;
      out ppMLangConvertCharset: IMLangConvertCharset): HResult; stdcall;
  end;

// *********************************************************************//
// Interface: ISSCommandWithParameters
// GUID:      {EEC30162-6087-467C-B995-7C523CE96561}
// *********************************************************************//
  ISSCommandWithParameters = interface(ICommandWithParameters)
    ['{EEC30162-6087-467C-B995-7C523CE96561}']
    function GetParameterProperties(var pcParams: DB_UPARAMS; out prgParamProperties: PSSPARAMPROPS): HResult; stdcall;
    function SetParameterProperties(cParams: DB_UPARAMS; rgParamProperties: PSSParamPropsArray): HResult; stdcall;
  end;

  TOpenSqlFilestream = function (
    FilestreamPath: PWideChar;
    DesiredAccess: TOleEnum; //SQL_FILESTREAM_DESIRED_ACCESS;
    OpenOptions: ULONG;
    FilestreamTransactionContext: PBYTE;
    FilestreamTransactionContextLength: DWORD; //SIZE_T
    AllocationSize: Pointer //PInt64
  ): HResult; stdcall;

var
  OpenSqlFilestream: TOpenSqlFilestream;

implementation

uses
  SysUtils,
  {$IFNDEF UNIDACPRO}MSConsts{$ELSE}MSConstsUni{$ENDIF};

const
  sqlncli10 = 'sqlncli10.dll';
var
  hSqlncliLib: HMODULE;

function NotLink: integer;
begin
  raise Exception.Create(SMSSQLNotFound);
{$IFNDEF VER16P}
  Result := 0;
{$ENDIF}
end;

procedure LoadLib;
  function GetProc(hModule: HMODULE; ProcName: string): FARPROC;
  begin
    Result := GetProcAddress(hModule, PChar(ProcName));
    if Result = nil then
      Result := @NotLink;
  end;

begin
  hSqlncliLib := LoadLibrary(PChar(sqlncli10));
  if hSqlncliLib > 0 then begin
    OpenSqlFilestream := GetProc(hSqlncliLib, 'OpenSqlFilestream');
  end;
end;

procedure FreeLib;
begin
  if hSqlncliLib > 0 then begin
    FreeLibrary(hSqlncliLib);
    hSqlncliLib := 0;
  end;
end;

initialization
  Assert(SizeOf(DBProp) = SizeOfDBProp);
  OpenSqlFilestream := @NotLink;
  LoadLib;

finalization
  FreeLib;

end.
