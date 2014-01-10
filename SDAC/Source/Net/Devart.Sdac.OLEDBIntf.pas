{$IFNDEF UNIDACPRO}
{$I ..\Sdac.inc}

unit Devart.Sdac.OLEDBIntf;
{$ENDIF}

interface

uses
  System.Security,
  CRTypes, MemUtils, {$IFNDEF UNIDACPRO}OLEDBC{$ELSE}OLEDBCUni{$ENDIF},
  ActiveX, System.Runtime.InteropServices, Variants, Windows;

const
  STGM_READ             = $00000000;

type
  IUnknown = System.Object; // ++ should move to MemUtils
  TUintArray = array of UINT;
  PUintArray = IntPtr;
  PPByteArray = IntPtr;
  PPOleStr = IntPtr;
  PCoServerInfo = IntPtr;
  PMultiQI = IntPtr;

  PUINT = IntPtr;
  PDWORD = IntPtr;
  PLongint = IntPtr;

  OleStr = string;
  POleStrList = IntPtr;

  [StructLayout(LayoutKind.Sequential, Pack = 1, CharSet = CharSet.Auto)]
  DBTIMESTAMP = packed record
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

  [StructLayout(LayoutKind.Sequential, Pack = 1, CharSet = CharSet.Auto)]
  DBTIME = packed record
    hour: Word;
    minute: Word;
    second: UINT;
    fraction: UINT;
  end;
  TDBTime = DBTIME;
  {$EXTERNALSYM DBTIME}

type
  [ComImport, GuidAttribute('1CF2B120-547D-101B-8E65-08002B2BD119'), InterfaceTypeAttribute(ComInterfaceType.InterfaceIsIUnknown)]
  IErrorInfo = interface
    [PreserveSig]
    function GetGUID(out guid: TGUID): HResult;
    [PreserveSig]
    function GetSource(out bstrSource: WideString): HResult;
    [PreserveSig]
    function GetDescription(out bstrDescription: WideString): HResult;
    [PreserveSig]
    function GetHelpFile(out bstrHelpFile: WideString): HResult;
    [PreserveSig]
    function GetHelpContext(out dwHelpContext: Longint): HResult;
  end;

  PBoid = IntPtr;
  [StructLayout(LayoutKind.Sequential, Pack = 1, CharSet = CharSet.Auto)]
  BOID = packed record
    [MarshalAs(UnmanagedType.ByValArray, SizeConst = 16, ArraySubType = UnmanagedType.U1)]
    rgb_: array[0..15] of Byte;
  end;
  TBoid = BOID;
  {$EXTERNALSYM BOID}

  [StructLayout(LayoutKind.Sequential, Pack = 1, CharSet = CharSet.Auto)]
  XACTTRANSINFO = packed record
    uow: BOID;
    isoLevel: Integer;
    isoFlags: UINT;
    grfTCSupported: UINT;
    grfRMSupported: UINT;
    grfTCSupportedRetaining: UINT;
    grfRMSupportedRetaining: UINT;
  end;
  TXactTransInfo = XACTTRANSINFO;
  {$EXTERNALSYM XACTTRANSINFO}

  [StructLayout(LayoutKind.Sequential, Pack = 1, CharSet = CharSet.Auto)]
  XACTOPT = packed record
    ulTimeout: UINT;
    [MarshalAs(UnmanagedType.ByValArray, SizeConst = 40, ArraySubType = UnmanagedType.I1)]
    szDescription: array[0..39] of Shortint;
  end;
  TXActOpt = XACTOPT;
  {$EXTERNALSYM XACTOPT}

  [StructLayout(LayoutKind.Sequential, Pack = 1, CharSet = CharSet.Auto)]
  XACTSTATS = packed record
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

  PDBBindExt = IntPtr;
  [StructLayout(LayoutKind.Sequential, Pack = 1, CharSet = CharSet.Auto)]
  DBBINDEXT = packed record
    pExtension: IntPtr; // PByte;
    ulExtension: UINT;
  end;
  TDBBindExt = DBBINDEXT;
  {$EXTERNALSYM DBBINDEXT}

  [StructLayout(LayoutKind.Sequential, Pack = 1, CharSet = CharSet.Auto)]
  DBOBJECT = packed record
    dwFlags: UINT;
    [MarshalAs(UnmanagedType.Struct)]
    iid: TGUID;
  end;
  TDBObject = DBOBJECT;
  {$EXTERNALSYM DBOBJECT}

  PDBBinding = IntPtr;
  [StructLayout(LayoutKind.Sequential, Pack = 1, CharSet = CharSet.Auto)]
  DBBINDING = packed record
    iOrdinal: UINT;
    obValue: UINT;
    obLength: UINT;
    obStatus: UINT;
    pTypeInfo: IntPtr; // IUnknown;
    pObject: IntPtr; // PDBObject;
    pBindExt: IntPtr; // PDBBindExt;
    dwPart: DBPART;
    dwMemOwner: DBMEMOWNER;
    eParamIO: DBPARAMIO;
    cbMaxLen: UINT;
    dwFlags: UINT;
    wType: DBTYPE;
    bPrecision: Byte;
    bScale: Byte;
  end;
  TDBBinding = DBBINDING;
{ $IFNDEF VER125}
  { $EXTERNALSYM DBBINDING}
{ $ENDIF}

  PDBBindingArray = IntPtr;
  TDBBindingArray = packed array of TDBBinding;

  [StructLayout(LayoutKind.Sequential, Pack = 1, CharSet = CharSet.Auto)]
  DBIDNAME = record
    [MarshalAs(UnmanagedType.LPWStr)]
    pwszName: PWideChar;
  end;
  TDBIDName = DBIDNAME;

  [StructLayout(LayoutKind.Sequential, Pack = 1, CharSet = CharSet.Auto)]
  DBIDGUID = record
    guid: TGUID;
  end;
  TDBIDGuid = DBIDGUID;

  DBPROPID = UINT;
  {$EXTERNALSYM DBPROPID}
  PDBPROPID = IntPtr;
  PDBPropIDArray = IntPtr;
  TDBPropIDArray = array[0..MAXBOUND] of DBPROPID;

  PDBID = IntPtr;
  [StructLayout(LayoutKind.Sequential, Pack = 1, CharSet = CharSet.Auto)]
  DBID = packed record
    uGuid: DBIDGUID;
    eKind: DBKIND;
    uName: IntPtr; // DBIDNAME;
  end;
  TDBID = DBID;
  {$EXTERNALSYM DBID}

  _DBID = array [0..23] of byte;

  PDBIDArray = IntPtr;
  TDBIDArray = array[0..MAXBOUND] of TDBID;

  PDBPropIDSet = IntPtr;
  [StructLayout(LayoutKind.Sequential, Pack = 1, CharSet = CharSet.Auto)]
  DBPROPIDSET = packed record
    rgPropertyIDs: IntPtr; // PDBPropIDArray;
    cPropertyIDs: UINT;
    guidPropertySet: TGUID;
  end;
  TDBPropIDSet = DBPROPIDSET;
  {$EXTERNALSYM DBPROPIDSET}

  PDBPropIDSetArray = IntPtr;
  TDBPropIDSetArray = array[0..MAXBOUND] of TDBPropIDSet;

  PDBProp = packed record
  private
    Ptr: IntPtr;

    function GetdwPropertyID: DBPROPID;
    procedure SetdwPropertyID(Value: DBPROPID);

    function GetdwOptions: DBPROPOPTIONS;
    procedure SetdwOptions(Value: DBPROPOPTIONS);

    function GetdwStatus: DBPROPSTATUS;
    procedure SetdwStatus(Value: DBPROPSTATUS);

    function Getcolid: _DBID;
    procedure Setcolid(Value: _DBID);

    function GetvValue: OleVariant;
    procedure SetvValue(Value: OleVariant);

  public
    property dwPropertyID: DBPROPID read GetdwPropertyID write SetdwPropertyID;
    property dwOptions: DBPROPOPTIONS read GetdwOptions write SetdwOptions;
    property dwStatus: DBPROPSTATUS read GetdwStatus write SetdwStatus;
    property colid: _DBID read Getcolid write Setcolid;
    property vValue: OleVariant read GetvValue write SetvValue;

    class operator Implicit(AValue: IntPtr): PDBProp;
    class operator Implicit(AValue: PDBProp): IntPtr;
  end;

  [StructLayout(LayoutKind.Sequential, Pack = 1, CharSet = CharSet.Auto)]
  DBPROP = packed record
    dwPropertyID: DBPROPID;
    dwOptions: DBPROPOPTIONS;
    dwStatus: DBPROPSTATUS;
    [MarshalAs(UnmanagedType.ByValArray, SizeConst = 24, ArraySubType = UnmanagedType.U1)]
    colid: _DBID;
    [MarshalAs(UnmanagedType.ByValArray, SizeConst = 16, ArraySubType = UnmanagedType.U1)] // ?? 
    vValue: OleVariant;
  end;
  TDBProp = DBPROP;
  {$EXTERNALSYM DBPROP}

  PDBPropArray = IntPtr;
  TDBPropArray = array[0..MAXBOUND] of TDBProp;


  PPDBPropSet = IntPtr;

  PDBPropSet = packed record
  private
    Ptr: IntPtr;

    function GetrgProperties: PDBPropArray;
    procedure SetrgProperties(Value: PDBPropArray);

    function GetcProperties: UINT;
    procedure SetcProperties(Value: UINT);

    function GetguidPropertySet: TGUID;
    procedure SetguidPropertySet(Value: TGUID);

  public
    property rgProperties: PDBPropArray read GetrgProperties write SetrgProperties;
    property cProperties: UINT read GetcProperties write SetcProperties;
    property guidPropertySet: TGUID read GetguidPropertySet write SetguidPropertySet;

    class operator Implicit(AValue: IntPtr): PDBPropSet;
    class operator Implicit(AValue: PDBPropSet): IntPtr;
  end;

  [StructLayout(LayoutKind.Sequential, Pack = 1, CharSet = CharSet.Auto)]
  DBPROPSET = packed record
    rgProperties: PDBPropArray;
    cProperties: UINT;
    [MarshalAs(UnmanagedType.Struct)]
    guidPropertySet: TGUID;
  end;
  TDBPropSet = DBPROPSET;
  {$EXTERNALSYM DBPROPSET}

  PPDBPropSetArray = IntPtr;
  PDBPropSetArray = IntPtr;
  TDBPropSetArray = array[0..MAXBOUND] of TDBPropSet;

  PDBPropInfo = IntPtr;
  [StructLayout(LayoutKind.Sequential, Pack = 1, CharSet = CharSet.Auto)]
  DBPROPINFO = packed record
    [MarshalAs(UnmanagedType.LPWStr)]
    pwszDescription: PWideChar;
    dwPropertyID: DBPROPID;
    dwFlags: DBPROPFLAGS;
    vtType: Word;
    vValues: OleVariant;
  end;
  TDBPropInfo = DBPROPINFO;
  {$EXTERNALSYM DBPROPINFO}

  PDBPropInfoArray = IntPtr;
  TDBPropInfoArray = array[0..MAXBOUND] of TDBPropInfo;

  PDBPropInfoSet = IntPtr;
(*
  PDBPropInfoSet = ^TDBPropInfoSet;
  [StructLayout(LayoutKind.Sequential, Pack = 1, CharSet = CharSet.Auto)]
  DBPROPINFOSET = packed record
    rgPropertyInfos: PDBPropInfoArray;
    cPropertyInfos: UINT;
    guidPropertySet: TGUID;
  end;
  TDBPropInfoSet = DBPROPINFOSET;
  {$EXTERNALSYM DBPROPINFOSET}
  *)

  PDBParams = IntPtr;
  [StructLayout(LayoutKind.Sequential, Pack = 1, CharSet = CharSet.Auto)]
  DBPARAMS = packed record
    pData: IntPtr;
    cParamSets: UINT;
    HACCESSOR: HACCESSOR;
  end;
  TDBParams = DBPARAMS;
  {$EXTERNALSYM DBPARAMS}

  PDBColumnInfo = IntPtr;
  [StructLayout(LayoutKind.Sequential, Pack = 1, CharSet = CharSet.Auto)]
  DBCOLUMNINFO = packed record
    [MarshalAs(UnmanagedType.LPWStr)]
    pwszName: PWideChar;
    pTypeInfo: IntPtr;
    iOrdinal: UINT;
    dwFlags: DBCOLUMNFLAGS;
    ulColumnSize: UINT;
    wType: DBTYPE;
    bPrecision: Byte;
    bScale: Byte;
    columnid: DBID;
  end;
  TDBColumnInfo = DBCOLUMNINFO;
  {$EXTERNALSYM DBCOLUMNINFO}

  PDBColumnInfoArray = IntPtr;
  TDBColumnInfoArray = array[0..MAXCOLS] of TDBColumnInfo;

  PDBLiteralInfo = IntPtr;
  [StructLayout(LayoutKind.Sequential, Pack = 1, CharSet = CharSet.Auto)]
  DBLITERALINFO = packed record
    [MarshalAs(UnmanagedType.LPWStr)]
    pwszLiteralValue: PWideChar;
    [MarshalAs(UnmanagedType.LPWStr)]
    pwszInvalidChars: PWideChar;
    [MarshalAs(UnmanagedType.LPWStr)]
    pwszInvalidStartingChars: PWideChar;
    lt: UINT;
    fSupported: BOOL;
    cchMaxLen: UINT;
  end;
  TDBLiteralInfo = DBLITERALINFO;
  {$EXTERNALSYM DBLITERALINFO}

  PDBLiteralInfoArray = IntPtr;
  TDBLiteralInfoArray = array[0..MAXBOUND] of TDBLiteralInfo;

  PDBColumnDesc = IntPtr;
  [StructLayout(LayoutKind.Sequential, Pack = 1, CharSet = CharSet.Auto)]
  DBCOLUMNDESC = packed record
    [MarshalAs(UnmanagedType.LPWStr)]
    pwszTypeName: PWideChar;
    pTypeInfo: IntPtr;
    rgPropertySets: IntPtr;
    pclsid: IntPtr;
    cPropertySets: UINT;
    ulColumnSize: UINT;
    dbcid: DBID;
    wType: DBTYPE;
    bPrecision: Byte;
    bScale: Byte;
  end;
  TDBColumnDesc = DBCOLUMNDESC;
  {$EXTERNALSYM DBCOLUMNDESC}

  PErrorInfo = IntPtr;
  [StructLayout(LayoutKind.Sequential, Pack = 1, CharSet = CharSet.Auto)]
  ERRORINFO = packed record
    hrError: HResult;
    dwMinor: UINT;
    clsid: TGUID;
    iid: TGUID;
    dispid: Integer;
  end;
  TErrorInfo = ERRORINFO;
  {$EXTERNALSYM ERRORINFO}

  PDBParamInfo = IntPtr;
//  PDBParamInfo = ^TDBParamInfo;
  [StructLayout(LayoutKind.Sequential, Pack = 1, CharSet = CharSet.Auto)]
  DBPARAMINFO = packed record
    dwFlags: DBPARAMFLAGS;
    iOrdinal: UINT;
    pwszName: PWideChar;
    pTypeInfo: ITypeInfo;
    ulParamSize: UINT;
    wType: DBTYPE;
    bPrecision: Byte;
    bScale: Byte;
  end;
  TDBParamInfo = DBPARAMINFO;
  {$EXTERNALSYM DBPARAMINFO}

  PDBParamBindInfo = IntPtr;
  [StructLayout(LayoutKind.Sequential, Pack = 1, CharSet = CharSet.Auto)]
  DBPARAMBINDINFO = packed record
    pwszDataSourceType: IntPtr;
    pwszName: IntPtr;
    ulParamSize: UINT;
    dwFlags: DBPARAMFLAGS;
    bPrecision: Byte;
    bScale: Byte;
  end;
  TDBParamBindInfo = DBPARAMBINDINFO;
  {$EXTERNALSYM DBPARAMBINDINFO}

  PDBParamBindInfoArray = IntPtr;
  TDBParamBindInfoArray = array[0..MAXBOUND] of TDBParamBindInfo;

  PSSParamProps = IntPtr;
  SSPARAMPROPS = packed record
    iOrdinal: DBORDINAL;
    cPropertySets: UINT;
    rgPropertySets: PPDBPropSetArray;
  end;
  TSSParamProps = SSPARAMPROPS;

  PSSParamPropsArray = IntPtr;
  TSSParamPropsArray = array[0..MAXBOUND] of TSSParamProps;

  PDBNumeric = IntPtr;
  [StructLayout(LayoutKind.Sequential, Pack = 1, CharSet = CharSet.Auto)]  
  DB_NUMERIC = packed record
    precision: Byte;
    scale: Byte;
    sign: Byte;
    [MarshalAs(UnmanagedType.ByValArray, SizeConst = 16, ArraySubType = UnmanagedType.U1)]
    val: array[0..15] of Byte;
  end;
  TDBNumeric = DB_NUMERIC;
  {$EXTERNALSYM DB_NUMERIC}

  // Multi Language support
  
  PMIMECPInfo = IntPtr;
  [StructLayout(LayoutKind.Sequential, Pack = 1, CharSet = CharSet.Auto)]
  MIMECPINFO = record
    dwFlags: DWORD;
    uiCodePage: UINT;
    uiFamilyCodePage: UINT;
    [MarshalAs(UnmanagedType.ByValArray, SizeConst = 64, ArraySubType = UnmanagedType.U2)]
    wszDescription: array [0..63] of WideChar;
    [MarshalAs(UnmanagedType.ByValArray, SizeConst = 50, ArraySubType = UnmanagedType.U2)]
    wszWebCharset: array [0..49] of WideChar;
    [MarshalAs(UnmanagedType.ByValArray, SizeConst = 50, ArraySubType = UnmanagedType.U2)]
    wszHeaderCharset: array [0..49] of WideChar;
    [MarshalAs(UnmanagedType.ByValArray, SizeConst = 50, ArraySubType = UnmanagedType.U2)]
    wszBodyCharset: array [0..49] of WideChar;
    [MarshalAs(UnmanagedType.ByValArray, SizeConst = 32, ArraySubType = UnmanagedType.U2)]
    wszFixedWidthFont: array [0..31] of WideChar;
    [MarshalAs(UnmanagedType.ByValArray, SizeConst = 32, ArraySubType = UnmanagedType.U2)]
    wszProportionalFont: array [0..31] of WideChar;
    bGDICharset: BYTE;
  end;
  TMIMECPInfo = MIMECPINFO;
  {$EXTERNALSYM MIMECPINFO}

  PMIMECSetInfo = IntPtr;
  [StructLayout(LayoutKind.Sequential, Pack = 1, CharSet = CharSet.Auto)]
  MIMECSETINFO = record
    uiCodePage: UINT;
    uiInternetEncoding: UINT;
    [MarshalAs(UnmanagedType.ByValArray, SizeConst = 50, ArraySubType = UnmanagedType.U2)]
    wszCharset: array [0..49] of WideChar;
  end;
  TMIMECSetInfo = MIMECSETINFO;
  {$EXTERNALSYM MIMECSETINFO}

  PRFC1766Info = IntPtr;
  [StructLayout(LayoutKind.Sequential, Pack = 1, CharSet = CharSet.Auto)]
  RFC1766INFO = record
    alcid: LCID;
    [MarshalAs(UnmanagedType.ByValArray, SizeConst = 6, ArraySubType = UnmanagedType.U2)]
    wszRfc1766: array [0..5] of WideChar;
    [MarshalAs(UnmanagedType.ByValArray, SizeConst = 32, ArraySubType = UnmanagedType.U2)]
    wszLocaleName: array [0..31] of WideChar;
  end;
  TRFC1766Info = RFC1766INFO;
  {$EXTERNALSYM RFC1766INFO}
  
const
  SizeOfTDBNumeric = 19;
// *********************************************************************//
//  DBID Values
// *********************************************************************//

var
  DB_NULLID: _DBID = ($00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $01, $00, $00, $00, $00, $00, $00, $00);

type
  TVariantArray = array [0..0] of OleVariant;
  PVariantArray = IntPtr;

  TGUIDArray = array [0..0] of TGUID;
  PGUIDArray = IntPtr;

// *********************************************************************//
// Interface: ITransaction
// GUID:      {0FB15084-AF41-11CE-BD2B-204C4F4F5020}
// *********************************************************************//
{$IFNDEF VER6P}
  {$NODEFINE ITransaction}
{$ENDIF}
  [ComImport, GuidAttribute('0FB15084-AF41-11CE-BD2B-204C4F4F5020'), InterfaceTypeAttribute(ComInterfaceType.InterfaceIsIUnknown)]
  ITransaction = interface
    [PreserveSig]
    function Commit(fRetaining: BOOL; grfTC: UINT; grfRM: UINT): HResult;
    [PreserveSig]
    function Abort(pboidReason: PBOID; fRetaining: BOOL; fAsync: BOOL): HResult;
    [PreserveSig]
    function GetTransactionInfo(out pinfo: XACTTRANSINFO): HResult;
  end;

// *********************************************************************//
// Interface: ITransactionOptions
// GUID:      {3A6AD9E0-23B9-11CF-AD60-00AA00A74CCD}
// *********************************************************************//
  [ComImport, GuidAttribute('3A6AD9E0-23B9-11CF-AD60-00AA00A74CCD'), InterfaceTypeAttribute(ComInterfaceType.InterfaceIsIUnknown)]
  ITransactionOptions = interface
    [PreserveSig]
    function SetOptions(var pOptions: XACTOPT): HResult;
    [PreserveSig]
    function GetOptions(var pOptions: XACTOPT): HResult;
  end;

// *********************************************************************//
// Interface: IAccessor
// GUID:      {0C733A8C-2A1C-11CE-ADE5-00AA0044773D}
// *********************************************************************//
  [ComImport, GuidAttribute('0C733A8C-2A1C-11CE-ADE5-00AA0044773D'), InterfaceTypeAttribute(ComInterfaceType.InterfaceIsIUnknown)]
  IAccessor = interface
    [PreserveSig]
    function AddRefAccessor(
      HACCESSOR: HACCESSOR;
      [Out, MarshalAs(UnmanagedType.U4)]
      out pcRefCount: UINT): HResult;
    [PreserveSig]
    function CreateAccessor(
      dwAccessorFlags: UINT;
      cBindings: UINT;
      rgBindings: PDBBindingArray;
      cbRowSize: UINT;
      var phAccessor: HACCESSOR;
      rgStatus: PUintArray
    ): HResult;
    [PreserveSig]
    function GetBindings(HACCESSOR: HACCESSOR;
      [Out, MarshalAs(UnmanagedType.U4)]
      out pdwAccessorFlags: UINT;
      var pcBindings: UINT;
      out prgBindings: PDBBinding): HResult;
    [PreserveSig]
    function ReleaseAccessor(HACCESSOR: HACCESSOR;
      pcRefCount: PUINT): HResult;
  end;

// *********************************************************************//
// Interface: IRowset
// GUID:      {0C733A7C-2A1C-11CE-ADE5-00AA0044773D}
// *********************************************************************//
  [ComImport, GuidAttribute('0C733A7C-2A1C-11CE-ADE5-00AA0044773D'), InterfaceTypeAttribute(ComInterfaceType.InterfaceIsIUnknown)]
  IRowset = interface
    [PreserveSig]
    function AddRefRows(cRows: UINT; rghRows: PUintArray; rgRefCounts: PUintArray;
      rgRowStatus: PUintArray): HResult;
    [PreserveSig]
    function GetData(HROW: HROW; HACCESSOR: HACCESSOR; pData: IntPtr): HResult;
    [PreserveSig]
    function GetNextRows(hReserved: HCHAPTER; lRowsOffset: Integer; cRows: Integer;
      out pcRowsObtained: NativeUInt; var prghRows: PUintArray): HResult;
    [PreserveSig]
    function ReleaseRows(
      cRows: UINT; 
      rghRows: PUintArray; 
      rgRowOptions,
      rgRefCounts, 
      rgRowStatus: PUintArray): HResult;
    [PreserveSig]
    function RestartPosition(hReserved: HCHAPTER): HResult;
  end;

// *********************************************************************//
// Interface: IRowsetInfo
// GUID:      {0C733A55-2A1C-11CE-ADE5-00AA0044773D}
// *********************************************************************//
  [ComImport, GuidAttribute('0C733A55-2A1C-11CE-ADE5-00AA0044773D'), InterfaceTypeAttribute(ComInterfaceType.InterfaceIsIUnknown)]
  IRowsetInfo = interface
    [PreserveSig]
    function GetProperties(cPropertyIDSets: UINT; rgPropertyIDSets: PDBPropIDSetArray;
      out pcPropertySets: UINT; out prgPropertySets: PDBPropSet): HResult;
    [PreserveSig]
    function GetReferencedRowset(iOrdinal: UINT;
      [In, MarshalAs(UnmanagedType.LPStruct)]
      const riid: TGUID;
      [Out, MarshalAs(UnmanagedType.IUnknown)]
      out ppReferencedRowset: IUnknown): HResult;
    [PreserveSig]
    function GetSpecification(
      [In, MarshalAs(UnmanagedType.LPStruct)]
      const riid: TGUID;
      [Out, MarshalAs(UnmanagedType.IUnknown)]
      out ppSpecification: IUnknown): HResult;
  end;

// *********************************************************************//
// Interface: IRowsetLocate
// GUID:      {0C733A7D-2A1C-11CE-ADE5-00AA0044773D}
// *********************************************************************//
  [ComImport, GuidAttribute('0C733A7D-2A1C-11CE-ADE5-00AA0044773D'), InterfaceTypeAttribute(ComInterfaceType.InterfaceIsIUnknown)]
  IRowsetLocate = interface
    // IRowset
    [PreserveSig]
    function AddRefRows(cRows: UINT; rghRows: PUintArray; rgRefCounts: PUintArray;
      rgRowStatus: PUintArray): HResult;
    [PreserveSig]
    function GetData(HROW: HROW; HACCESSOR: HACCESSOR; pData: IntPtr): HResult;
    [PreserveSig]
    function GetNextRows(hReserved: HCHAPTER; lRowsOffset: Integer; cRows: Integer;
      out pcRowsObtained: UINT; var prghRows: PUintArray): HResult;
    [PreserveSig]
    function ReleaseRows(cRows: UINT; rghRows: PUintArray; rgRowOptions,
      rgRefCounts, rgRowStatus: PUintArray): HResult;
    [PreserveSig]
    function RestartPosition(hReserved: HCHAPTER): HResult;

    // IRowsetLocate
    [PreserveSig]
    function Compare(hReserved: HCHAPTER; cbBookmark1: UINT; pBookmark1: IntPtr;
      cbBookmark2: UINT; pBookmark2: IntPtr; out pComparison: UINT): HResult;
    [PreserveSig]
    function GetRowsAt(
      hReserved1: HWATCHREGION;
      hReserved2: HCHAPTER;
      cbBookmark: UINT;
      pBookmark: IntPtr;
      lRowsOffset: Integer;
      cRows: Integer;
      out pcRowsObtained: NativeUInt;
      var prghRows: PUintArray): HResult;
    [PreserveSig]
    function GetRowsByBookmark(hReserved: HCHAPTER; cRows: UINT; rgcbBookmarks: PUintArray;
      rgpBookmarks: PPByteArray; rghRows, rgRowStatus: PUintArray): HResult;
    [PreserveSig]
    function Hash(hReserved: HCHAPTER; cBookmarks: UINT; rgcbBookmarks: PUintArray;
      rgpBookmarks: PPByteArray; rgHashedValues,
        rgBookmarkStatus: PUintArray): HResult;
  end;

// *********************************************************************//
// Interface: IRowsetChange
// GUID:      {0C733A05-2A1C-11CE-ADE5-00AA0044773D}
// *********************************************************************//
  [ComImport, GuidAttribute('0C733A05-2A1C-11CE-ADE5-00AA0044773D'), InterfaceTypeAttribute(ComInterfaceType.InterfaceIsIUnknown)]
  IRowsetChange = interface
    [PreserveSig]
    function DeleteRows(hReserved: HCHAPTER; cRows: UINT; rghRows: PUintArray;
      rgRowStatus: PUintArray): HResult;
    [PreserveSig]
    function SetData(HROW: HROW; HACCESSOR: HACCESSOR; pData: IntPtr): HResult;
    [PreserveSig]
    function InsertRow(hReserved: HCHAPTER; HACCESSOR: HACCESSOR; pData: IntPtr;
      out phRow: HROW): HResult;
  end;

// *********************************************************************//
// Interface: IRowsetUpdate
// GUID:      {0C733A6D-2A1C-11CE-ADE5-00AA0044773D}
// *********************************************************************//
  [ComImport, GuidAttribute('0C733A6D-2A1C-11CE-ADE5-00AA0044773D'), InterfaceTypeAttribute(ComInterfaceType.InterfaceIsIUnknown)]
  IRowsetUpdate = interface
    // IRowsetChange
    [PreserveSig]
    function DeleteRows(hReserved: HCHAPTER; cRows: UINT; rghRows: PUintArray;
      rgRowStatus: PUintArray): HResult;
    [PreserveSig]
    function SetData(HROW: HROW; HACCESSOR: HACCESSOR; pData: IntPtr): HResult;
    [PreserveSig]
    function InsertRow(hReserved: HCHAPTER; HACCESSOR: HACCESSOR; pData: IntPtr;
      out phRow: HROW): HResult;

    // IRowsetUpdate
    [PreserveSig]
    function GetOriginalData(HROW: HROW; HACCESSOR: HACCESSOR; pData: IntPtr): HResult;
    [PreserveSig]
    function GetPendingRows(hReserved: HCHAPTER; dwRowStatus: DBPENDINGSTATUS;
      [Out, MarshalAs(UnmanagedType.U4)]
      out pcPendingRows: UINT;
      prgPendingRows: PPHROW; prgPendingStatus: PPDBPENDINGSTATUS): HResult;
    [PreserveSig]
    function GetRowStatus(hReserved: HCHAPTER;
      [Out, MarshalAs(UnmanagedType.U4)]
      out cRows: UINT;
      rghRows: PUintArray;
      rgPendingStatus: PUintArray): HResult;
    [PreserveSig]
    function Undo(
      hReserved: HCHAPTER;
      cRows: UINT;
      rghRows: PUintArray;
      pcRowsUndone: PUINT;
      prgRowsUndone: PPHROW;
      prgRowStatus: PPDBROWSTATUS
      ): HResult;
    [PreserveSig]
    function Update(
      hReserved: HCHAPTER;
      cRows: UINT;
      rghRows: PUintArray;
      pcRows: PUINT;
      prgRows: PPHROW;
      out prgRowStatus: PDBROWSTATUS
      ): HResult;
  end;

// *********************************************************************//
// Interface: IMultipleResults
// GUID:      {0C733A90-2A1C-11CE-ADE5-00AA0044773D}
// *********************************************************************//
  [ComImport, GuidAttribute('0C733A90-2A1C-11CE-ADE5-00AA0044773D'), InterfaceTypeAttribute(ComInterfaceType.InterfaceIsIUnknown)]
  IMultipleResults = interface
    [PreserveSig]
    function GetResult(
      [In, MarshalAs(UnmanagedType.IUnknown)]
      const punkOuter: IUnknown;
      reserved: Integer;
      [In, MarshalAs(UnmanagedType.LPStruct)]
      const riid: TGUID;
      [Out, MarshalAs(UnmanagedType.I4)]
      out pcRowsAffected: NativeInt;
      [Out, MarshalAs(UnmanagedType.Interface)]
      out ppRowset: IUnknown): HResult;
  end;

// *********************************************************************//
// Interface: ICommandPrepare
// GUID:      {0C733A26-2A1C-11CE-ADE5-00AA0044773D}
// *********************************************************************//
  [ComImport, GuidAttribute('0C733A26-2A1C-11CE-ADE5-00AA0044773D'), InterfaceTypeAttribute(ComInterfaceType.InterfaceIsIUnknown)]
  ICommandPrepare = interface
    [PreserveSig]
    function Prepare(cExpectedRuns: UINT): HResult;
    [PreserveSig]
    function Unprepare: HResult;
  end;

// *********************************************************************//
// Interface: ICommandProperties
// GUID:      {0C733A79-2A1C-11CE-ADE5-00AA0044773D}
// *********************************************************************//

  [ComImport, GuidAttribute('0C733A79-2A1C-11CE-ADE5-00AA0044773D'), InterfaceTypeAttribute(ComInterfaceType.InterfaceIsIUnknown)]
  ICommandProperties = interface
    [PreserveSig]
    function GetProperties(cPropertyIDSets: UINT; rgPropertyIDSets: PDBPropIDSetArray;
      var pcPropertySets: UINT; out prgPropertySets: PDBPropSet): HResult;
    [PreserveSig]
    function SetProperties(cPropertySets: UINT; rgPropertySets: PDBPropSetArray): HResult;
  end;

// *********************************************************************//
// Interface: ICommand
// GUID:      {0C733A63-2A1C-11CE-ADE5-00AA0044773D}
// *********************************************************************//
  [ComImport, GuidAttribute('0C733A63-2A1C-11CE-ADE5-00AA0044773D'), InterfaceTypeAttribute(ComInterfaceType.InterfaceIsIUnknown)]
  ICommand = interface
    [PreserveSig]
    function Cancel: HResult;
    [PreserveSig]
    function Execute(
      [In, MarshalAs(UnmanagedType.IUnknown)]
      const punkOuter: IUnknown;
      [In, MarshalAs(UnmanagedType.LPStruct)]
      const riid: TGUID;
      pParams: PDBPARAMS;
      [Out, MarshalAs(UnmanagedType.I4)]
      out pcRowsAffected: NativeInt;
      [Out, MarshalAs(UnmanagedType.Interface)]
      out ppRowset: IUnknown): HResult;
    [PreserveSig]
    function GetDBSession(
      [In, MarshalAs(UnmanagedType.LPStruct)]
      const riid: TGUID;
      [Out, MarshalAs(UnmanagedType.IUnknown)]
      out ppSession: IUnknown): HResult;
  end;

// *********************************************************************//
// Interface: ICommandText
// GUID:      {0C733A27-2A1C-11CE-ADE5-00AA0044773D}
// *********************************************************************//
  [ComImport, GuidAttribute('0C733A27-2A1C-11CE-ADE5-00AA0044773D'), InterfaceTypeAttribute(ComInterfaceType.InterfaceIsIUnknown)]
  ICommandText = interface(ICommand)
    [PreserveSig]
    function Cancel: HResult;
    [PreserveSig]
    function Execute(
      [In, MarshalAs(UnmanagedType.IUnknown)]
      const punkOuter: IUnknown;
      [In, MarshalAs(UnmanagedType.LPStruct)]
      const riid: TGUID;
      pParams: PDBPARAMS;
      [Out, MarshalAs(UnmanagedType.I4)]
      out pcRowsAffected: Integer;
      [Out, MarshalAs(UnmanagedType.Interface)]
      out ppRowset: IUnknown): HResult;
    [PreserveSig]
    function GetDBSession(
      [In, MarshalAs(UnmanagedType.LPStruct)]
      const riid: TGUID;
      [Out, MarshalAs(UnmanagedType.IUnknown)]
      out ppSession: IUnknown): HResult;

    [PreserveSig]
    function GetCommandText( // WANING! System.Data.Common.UnsafeNativeMethods.GetCommandText has no parameters!
      [MarshalAs(UnmanagedType.LPStruct)]
      var pguidDialect: TGUID;
      out ppwszCommand: IntPtr): HResult;
    [PreserveSig]
   function SetCommandText(
      [In, MarshalAs(UnmanagedType.LPStruct)]
      const rguidDialect: TGUID;
      [In, MarshalAs(UnmanagedType.LPWStr)]
      const pwszCommand: string): HResult;
  end;

// *********************************************************************//
// Interface: ICommandWithParameters
// GUID:      {0C733A64-2A1C-11CE-ADE5-00AA0044773D}
// *********************************************************************//
  [ComImport, GuidAttribute('0C733A64-2A1C-11CE-ADE5-00AA0044773D'), InterfaceTypeAttribute(ComInterfaceType.InterfaceIsIUnknown)]
  ICommandWithParameters = interface
    [PreserveSig]
    function GetParameterInfo(
      [Out, MarshalAs(UnmanagedType.U4)]
      var pcParams: NativeUInt;
      out prgParamInfo: PDBPARAMINFO;
      ppNamesBuffer: PPOleStr): HResult;
    [PreserveSig]
    function MapParameterNames(
      cParamNames: UINT;
      rgParamNames: POleStrList;
      rgParamOrdinals: PUintArray): HResult;
    [PreserveSig]
    function SetParameterInfo(
      cParams: UINT;
      rgParamOrdinals: PUintArray;
      rgParamBindInfo: PDBParamBindInfoArray): HResult;
  end;

// *********************************************************************//
// Interface: ISSCommandWithParameters
// GUID:      {EEC30162-6087-467C-B995-7C523CE96561}
// *********************************************************************//
  [ComImport, GuidAttribute('EEC30162-6087-467C-B995-7C523CE96561'), InterfaceTypeAttribute(ComInterfaceType.InterfaceIsIUnknown)]
  ISSCommandWithParameters = interface(ICommandWithParameters)
    [PreserveSig]
    function GetParameterProperties(var pcParams: DB_UPARAMS; out prgParamProperties: PSSPARAMPROPS): HResult;
    [PreserveSig]
    function SetParameterProperties(cParams: DB_UPARAMS; rgParamProperties: PSSParamPropsArray): HResult;
  end;

// *********************************************************************//
// Interface: IColumnsRowset
// GUID:      {0C733A10-2A1C-11CE-ADE5-00AA0044773D}
// *********************************************************************//
  [ComImport, GuidAttribute('0C733A10-2A1C-11CE-ADE5-00AA0044773D'), InterfaceTypeAttribute(ComInterfaceType.InterfaceIsIUnknown)]
  IColumnsRowset = interface
    [PreserveSig]
    function GetAvailableColumns(var pcOptColumns: UINT;
      var prgOptColumns: PDBID): HResult;
    [PreserveSig]
    function GetColumnsRowset(
      [In, MarshalAs(UnmanagedType.IUnknown)]
      const punkOuter: IUnknown;
      cOptColumns: UINT;
      rgOptColumns: PDBIDArray;
      [In, MarshalAs(UnmanagedType.LPStruct)]
      const riid: TGUID;
      cPropertySets: UINT;
      rgPropertySets: PDBPropSetArray;
      [Out, MarshalAs(UnmanagedType.IUnknown)]
      out pColRowset: IUnknown): HResult;
  end;

// *********************************************************************//
// Interface: IColumnsInfo
// GUID:      {0C733A11-2A1C-11CE-ADE5-00AA0044773D}
// *********************************************************************//
  [ComImport, GuidAttribute('0C733A11-2A1C-11CE-ADE5-00AA0044773D'), InterfaceTypeAttribute(ComInterfaceType.InterfaceIsIUnknown)]
  IColumnsInfo = interface
    [PreserveSig]
    function GetColumnInfo(var pcColumns: NativeUInt; out prgInfo: PDBColumnInfo;
      out ppStringsBuffer: IntPtr): HResult;
    [PreserveSig]
    function MapColumnIDs(cColumnIDs: UINT; rgColumnIDs: PDBIDArray;
      rgColumns: PUintArray): HResult;
  end;

// *********************************************************************//
// Interface: IDBCreateCommand
// GUID:      {0C733A1D-2A1C-11CE-ADE5-00AA0044773D}
// *********************************************************************//
  [ComImport, GuidAttribute('0C733A1D-2A1C-11CE-ADE5-00AA0044773D'), InterfaceTypeAttribute(ComInterfaceType.InterfaceIsIUnknown)]
  IDBCreateCommand = interface
    [PreserveSig]
    function CreateCommand(
      [In, MarshalAs(UnmanagedType.IUnknown)]
      const punkOuter: IUnknown;
      [In, MarshalAs(UnmanagedType.LPStruct)]
      const riid: TGUID;
      [Out, MarshalAs(UnmanagedType.IUnknown)]
      out ppCommand: IUnknown): HResult;
  end;

// *********************************************************************//
// Interface: IDBCreateSession
// GUID:      {0C733A5D-2A1C-11CE-ADE5-00AA0044773D}
// *********************************************************************//
  [ComImport, GuidAttribute('0C733A5D-2A1C-11CE-ADE5-00AA0044773D'), InterfaceTypeAttribute(ComInterfaceType.InterfaceIsIUnknown)]
  IDBCreateSession = interface
    [PreserveSig]
    function CreateSession(
      [In, MarshalAs(UnmanagedType.IUnknown)]
      const punkOuter: IUnknown;
      [In, MarshalAs(UnmanagedType.LPStruct)]
      const riid: TGUID;
      [Out, MarshalAs(UnmanagedType.IUnknown)]
      out ppDBSession: IUnknown): HResult;
  end;

// *********************************************************************//
// Interface: ISourcesRowset
// GUID:      {0C733A1E-2A1C-11CE-ADE5-00AA0044773D}
// *********************************************************************//
  [ComImport, GuidAttribute('0C733A1E-2A1C-11CE-ADE5-00AA0044773D'), InterfaceTypeAttribute(ComInterfaceType.InterfaceIsIUnknown)]
  ISourcesRowset = interface
    [PreserveSig]
    function GetSourcesRowset(
      [In, MarshalAs(UnmanagedType.IUnknown)]
      const punkOuter: IUnknown;
      [In, MarshalAs(UnmanagedType.LPStruct)]
      const riid: TGUID;
      cPropertySets: UINT;
      rgProperties: PDBPropSetArray;
      [Out, MarshalAs(UnmanagedType.IUnknown)]
      out ppSourcesRowset: IUnknown): HResult;
  end;

// *********************************************************************//
// Interface: IDBProperties
// GUID:      {0C733A8A-2A1C-11CE-ADE5-00AA0044773D}
// *********************************************************************//
  [ComImport, GuidAttribute('0C733A8A-2A1C-11CE-ADE5-00AA0044773D'), InterfaceTypeAttribute(ComInterfaceType.InterfaceIsIUnknown)]
  IDBProperties = interface
    [PreserveSig]
    function GetProperties(cPropertyIDSets: UINT; rgPropertyIDSets: PDBPropIDSetArray;
      var pcPropertySets: UINT; out prgPropertySets: PDBPropSet): HResult;
    [PreserveSig]
    function GetPropertyInfo(cPropertyIDSets: UINT; rgPropertyIDSets: PDBPropIDSetArray;
      var pcPropertyInfoSets: UINT; out prgPropertyInfoSets: PDBPropInfoSet;
      ppDescBuffer: PPOleStr): HResult;
    [PreserveSig]
    function SetProperties(cPropertySets: UINT; rgPropertySets: PDBPropSetArray): HResult;
  end;

// *********************************************************************//
// Interface: IDBInitialize
// GUID:      {0C733A8B-2A1C-11CE-ADE5-00AA0044773D}
// *********************************************************************//
  [ComImport, GuidAttribute('0C733A8B-2A1C-11CE-ADE5-00AA0044773D'), InterfaceTypeAttribute(ComInterfaceType.InterfaceIsIUnknown)]
  IDBInitialize = interface
    [PreserveSig]
    function Initialize: HResult;
    [PreserveSig]
    function Uninitialize: HResult;
  end;

// *********************************************************************//
// Interface: IDBDataSourceAdmin
// GUID:      {0C733A7A-2A1C-11CE-ADE5-00AA0044773D}
// *********************************************************************//
  [ComImport, GuidAttribute('0C733A7A-2A1C-11CE-ADE5-00AA0044773D'), InterfaceTypeAttribute(ComInterfaceType.InterfaceIsIUnknown)]
  IDBDataSourceAdmin = interface
    [PreserveSig]
    function CreateDataSource(cPropertySets: UINT;
                              rgPropertySets: PDBPropSetArray;
                              [In, MarshalAs(UnmanagedType.IUnknown)]
                              const punkOuter: IUnknown;
                              [In, MarshalAs(UnmanagedType.LPStruct)]
                              const riid: TGUID;
                              [Out, MarshalAs(UnmanagedType.IUnknown)]
                              out ppDBSession: IUnknown): HResult;
    [PreserveSig]
    function DestroyDataSource: HResult;
    [PreserveSig]
    function GetCreationProperties(cPropertyIDSets: UINT;
                                   rgPropertyIDSets: PDBPropIDSetArray;
                                   pcPropertyInfoSets: PUINT;
                                   out prgPropertyInfoSets: PDBPropInfoSet;
                                   ppDescBuffer: PPOleStr): HResult;
    [PreserveSig]
    function ModifyDataSource(cPropertySets: UINT;
                              rgPropertySets: PDBPropSetArray): HResult;
  end;

// *********************************************************************//
// Interface: ISessionProperties
// GUID:      {0C733A85-2A1C-11CE-ADE5-00AA0044773d}
// *********************************************************************//
  [ComImport, GuidAttribute('0C733A85-2A1C-11CE-ADE5-00AA0044773d'), InterfaceTypeAttribute(ComInterfaceType.InterfaceIsIUnknown)]
  ISessionProperties = interface
    [PreserveSig]
    function GetProperties(cPropertyIDSets: UINT; rgPropertyIDSets: PDBPropIDSetArray;
      var pcPropertySets: UINT; out prgPropertySets: PDBPropSet): HResult;
    [PreserveSig]
    function SetProperties(cPropertySets: UINT; rgPropertySets: PDBPropSetArray): HResult;
  end;


// *********************************************************************//
// Interface: ITransactionJoin
// GUID:      {0C733A5E-2A1C-11CE-ADE5-00AA0044773D}
// *********************************************************************//
  [ComImport, GuidAttribute('0C733A5E-2A1C-11CE-ADE5-00AA0044773D'), InterfaceTypeAttribute(ComInterfaceType.InterfaceIsIUnknown)]
  ITransactionJoin = interface
    [PreserveSig]
    function GetOptionsObject([MarshalAs(UnmanagedType.Interface)] out ppOptions: ITransactionOptions): HResult;
    [PreserveSig]
    function JoinTransaction([in, MarshalAs(UnmanagedType.IUnknown)] punkTransactionCoord: TObject;
      isoLevel: Integer; isoFlags: UINT;
      [in, MarshalAs(UnmanagedType.Interface)] const pOtherOptions: ITransactionOptions): HResult;
  end;

// *********************************************************************//
// Interface: ITransactionLocal
// GUID:      {0C733A5F-2A1C-11CE-ADE5-00AA0044773D}
// *********************************************************************//
  [ComImport, GuidAttribute('0C733A5F-2A1C-11CE-ADE5-00AA0044773D'), InterfaceTypeAttribute(ComInterfaceType.InterfaceIsIUnknown)]
  ITransactionLocal = interface
    // ITransaction
    [PreserveSig]
    function Commit(fRetaining: BOOL; grfTC: UINT; grfRM: UINT): HResult;
    [PreserveSig]
    function Abort(pboidReason: PBOID; fRetaining: BOOL; fAsync: BOOL): HResult;
    [PreserveSig]
    function GetTransactionInfo(out pinfo: XACTTRANSINFO): HResult;

    // ITransactionLocal
    [PreserveSig]
    function GetOptionsObject(out ppOptions: ITransactionOptions): HResult;
    [PreserveSig]
    function StartTransaction(isoLevel: Integer; isoFlags: UINT;
      const pOtherOptions: ITransactionOptions;
      pulTransactionLevel: PUINT): HResult;
  end;

// *********************************************************************//
// Interface: IDBPromptInitialize
// GUID:      {2206CCB0-19C1-11D1-89E0-00C04FD7A829}
// *********************************************************************//
  [ComImport, GuidAttribute('2206CCB0-19C1-11D1-89E0-00C04FD7A829'), InterfaceTypeAttribute(ComInterfaceType.InterfaceIsIUnknown)]
  IDBPromptInitialize = interface
    [PreserveSig]
    function PromptDataSource(
      [In, MarshalAs(UnmanagedType.IUnknown)]
      const pUnkOuter: IUnknown;
      hWndParent: HWND;
      dwPromptOptions: DBPROMPTOPTIONS; cSourceTypeFilter: ULONG;
      rgSourceTypeFilter: PDBSOURCETYPE;
      [Out, MarshalAs(UnmanagedType.LPWStr)]
      pszProviderFilter: OleStr;
      [In, MarshalAs(UnmanagedType.LPStruct)]
      const riid: TIID;
      [MarshalAs(UnmanagedType.IUnknown)]
      var DataSource: IUnknown): HResult;
    [PreserveSig]
    function PromptFileName(hWndParent: HWND; dwPromptOptions: DBPROMPTOPTIONS;
      pwszInitialDirectory, pwszInitialFile: OleStr;
      [MarshalAs(UnmanagedType.LPWStr)]
      var ppwszSelectedFile: OleStr): HResult; 
  end;

// *********************************************************************//
// Interface: IDataInitialize
// GUID:      {2206CCB1-19C1-11D1-89E0-00C04FD7A829}
// *********************************************************************//
  [ComImport, GuidAttribute('2206CCB1-19C1-11D1-89E0-00C04FD7A829'), InterfaceTypeAttribute(ComInterfaceType.InterfaceIsIUnknown)]
  IDataInitialize = interface
    [PreserveSig]
    function GetDataSource(
      [In, MarshalAs(UnmanagedType.IUnknown)]
      const pUnkOuter: IUnknown; 
      dwClsCtx: DWORD;
      [In, MarshalAs(UnmanagedType.LPWStr)]
      const pwszInitializationString: OleStr;
      [In, MarshalAs(UnmanagedType.LPStruct)]
      const riid: TGUID;
      [Out, MarshalAs(UnmanagedType.IUnknown)]
      out DataSource: IUnknown): HResult;
    [PreserveSig]
    function GetInitializationString(
      [In, MarshalAs(UnmanagedType.IUnknown)]
      const DataSource: IUnknown;
      fIncludePassword: Boolean;
      [Out, MarshalAs(UnmanagedType.LPWStr)]
      out pwszInitString: OleStr): HResult;
    function CreateDBInstance(
      [In, MarshalAs(UnmanagedType.LPStruct)]
      const clsidProvider: TGUID;
      [In, MarshalAs(UnmanagedType.IUnknown)]
      const pUnkOuter: IUnknown; 
      dwClsCtx: DWORD;
      [MarshalAs(UnmanagedType.LPWStr)]
      pwszReserved: OleStr;
      riid: TIID;
      [MarshalAs(UnmanagedType.IUnknown)]
      var DataSource: IUnknown): HResult;
    function CreateDBInstanceEx(
      [In, MarshalAs(UnmanagedType.LPStruct)]
      const clsidProvider: TGUID;
      [In, MarshalAs(UnmanagedType.IUnknown)]
      const pUnkOuter: IUnknown; 
      dwClsCtx: DWORD;
      [MarshalAs(UnmanagedType.LPWStr)]
      pwszReserved: OleStr;
      pServerInfo: PCoServerInfo; 
      cmq: ULONG; 
      rgmqResults: PMultiQI): HResult; 
    function LoadStringFromStorage(
      [MarshalAs(UnmanagedType.LPWStr)]
      pwszFileName: OleStr;
      [Out, MarshalAs(UnmanagedType.LPWStr)]
      out pwszInitializationString: OleStr): HResult; 
    function WriteStringToStorage(
      [MarshalAs(UnmanagedType.LPWStr)]
      pwszFileName,
      [MarshalAs(UnmanagedType.LPWStr)]
      pwszInitializationString: OleStr;
      dwCreationDisposition: DWORD): HResult; 
  end;

  [ComImport, GuidAttribute('0c733a7b-2a1c-11ce-ade5-00aa0044773d'), InterfaceTypeAttribute(ComInterfaceType.InterfaceIsIUnknown)]
  IDBSchemaRowset = interface
    [PreserveSig]
    function GetRowset (
      [In, MarshalAs(UnmanagedType.IUnknown)]
      const pUnkOuter: IUnknown;
      [In, MarshalAs(UnmanagedType.LPStruct)]
      const rguidSchema: TGUID;
      cRestrictions: UINT; rgRestrictions: PVariantArray;
      [In, MarshalAs(UnmanagedType.LPStruct)]
      const riid: TGUID;
      cPropertySets: UINT; rgPropertySets: PDBPropSetArray;
      [Out, MarshalAs(UnmanagedType.IUnknown)]
      out ppRowset: IUnknown): HResult;
    [PreserveSig]
    function GetSchemas (out pcSchemas: UINT; out prgSchemas: PGUIDArray;
      prgRestrictionSupport: PUINTArray): HResult;
  end;

  [ComImport,
  GuidAttribute('0C733A69-2A1C-11CE-ADE5-00AA0044773D'),
  InterfaceTypeAttribute(ComInterfaceType.InterfaceIsIUnknown)]
  IOpenRowset = interface // (IUnknown)
    [PreserveSig]
    function OpenRowset([in, MarshalAs(UnmanagedType.IUnknown)] punkOuter: TObject;
      pTableID: PDBID; pIndexID: PDBID;
      [MarshalAs(UnmanagedType.LPStruct)] riid: TGUID; cPropertySets: UINT;
      rgPropertySets: PDBPropSetArray;
      [MarshalAs(UnmanagedType.IUnknown)] var ppRowset): HResult;
  end;

  [ComImport,
  GuidAttribute('5CF4CA13-EF21-11d0-97E7-00C04FC2AD98'),
  InterfaceTypeAttribute(ComInterfaceType.InterfaceIsIUnknown)]
  IRowsetFastLoad = interface // (IUnknown)
    [PreserveSig]
    function InsertRow(HACCESSOR: HACCESSOR; pData: IntPtr): HResult;
    [PreserveSig]
    function Commit(fDone: boolean): HResult;
  end;

  [ComImport, GuidAttribute('0c733a95-2a1c-11ce-ade5-00aa0044773d'), InterfaceTypeAttribute(ComInterfaceType.InterfaceIsIUnknown)]
  IDBAsynchStatus = interface
    [PreserveSig]
    function Abort(Chapter: HCHAPTER; Operation: DBASYNCHOP): HRESULT;
    [PreserveSig]
    function GetStatus(
      Chapter: HCHAPTER; // UINT
      Operation: DBASYNCHOP; // UINT
      var Progress: UINT;
      var ProgressMax: UINT; var AsynchPhase: DWORD;
      var pStatusText: IntPtr): HRESULT;
  end;

  [ComImport, GuidAttribute('1FF1F743-8BB0-4c00-ACC4-C10E43B08FC1'), InterfaceTypeAttribute(ComInterfaceType.InterfaceIsIUnknown)]
  ISSAsynchStatus = interface // (IDBAsynchStatus)
    [PreserveSig]
    function Abort(Chapter: HCHAPTER; Operation: DBASYNCHOP): HRESULT;
    [PreserveSig]
    function GetStatus(
      Chapter: HCHAPTER; // UINT
      Operation: DBASYNCHOP; // UINT
      var Progress: UINT;
      var ProgressMax: UINT; var AsynchPhase: DWORD;
      var pStatusText: IntPtr): HRESULT;

    [PreserveSig]
    function WaitForAsynchCompletion(MillisecTimeOut: DWORD): HRESULT;
  end;

  [ComImport, GuidAttribute('0c733a67-2a1c-11ce-ade5-00aa0044773d'), InterfaceTypeAttribute(ComInterfaceType.InterfaceIsIUnknown)]
  IErrorRecords = interface
    [PreserveSig]
    function (* [local] *) AddErrorRecord(
            (* [in] *) const pErrorInfo: ERRORINFO;
            (* [in] *) dwLookupID: DWORD;
            (* [in] *) const pdispparams: DISPPARAMS;
            [In, MarshalAs(UnmanagedType.IUnknown)]
            (* [in] *) const punkCustomError: IUnknown;
            (* [in] *) dwDynamicErrorID: DWORD): HResult;

    [PreserveSig]
    function (* [local] *) GetBasicErrorInfo(
            (* [in] *) ulRecordNum: ULONG;
            (* [out] *) out pErrorInfo: ERRORINFO): HResult;

    [PreserveSig]
    function (* [local] *) GetCustomErrorObject(
            (* [in] *) ulRecordNum: ULONG;
            [In, MarshalAs(UnmanagedType.LPStruct)]
            (* [in] *) const riid: TGUID;
            [Out, MarshalAs(UnmanagedType.IUnknown)]
            (* [iid_is][out] *) out ppObject: IUnknown): HResult;

    [PreserveSig]
    function (* [local] *) GetErrorInfo(
            (* [in] *) ulRecordNum: ULONG;
            (* [in] *) lcid: LCID;
            (* [out] *)
            [Out, MarshalAs(UnmanagedType.IUnknown)]
            out ppErrorInfo: IErrorInfo): HResult;

    [PreserveSig]
    function (* [local] *) GetErrorParameters(
            (* [in] *) ulRecordNum: ULONG;
            (* [out] *) out pdispparams: DISPPARAMS): HResult;

    [PreserveSig]
    function (* [local] *) GetRecordCount(
            (* [out] *) out pcRecords: ULONG): HResult;
  end;
  {$EXTERNALSYM IErrorRecords}

// the structure returned by  ISQLServerErrorInfo::GetSQLServerInfo
  [StructLayout(LayoutKind.Sequential, Pack = 1, CharSet = CharSet.Auto)]
  SSERRORINFO = record
    [MarshalAs(UnmanagedType.LPWStr)]
    pwszMessage: PWideChar;
    [MarshalAs(UnmanagedType.LPWStr)]
    pwszServer: PWideChar;
    [MarshalAs(UnmanagedType.LPWStr)]
    pwszProcedure: PWideChar;
    lNative: integer;
    bState: BYTE;
    bClass: BYTE;
    wLineNumber: WORD;
  end;
  PSSERRORINFO = IntPtr;

  [ComImport, GuidAttribute('5CF4CA12-EF21-11d0-97E7-00C04FC2AD98'), InterfaceTypeAttribute(ComInterfaceType.InterfaceIsIUnknown)]
  ISQLServerErrorInfo = interface
    [PreserveSig]
    function GetErrorInfo(
            (* [out] *) out ppErrorInfo: PSSERRORINFO;
            (* [out] *) out ppStringsBuffer: IntPtr{PWideChar}): HResult;
  end;

  /// WARINING - in 64bit OSes DBBKMARK is int64
  DBBKMARK = integer;

  [ComImport, GuidAttribute('0c733a30-2a1c-11ce-ade5-00aa0044773d'), InterfaceTypeAttribute(ComInterfaceType.InterfaceIsIUnknown)]
  ISequentialStream = interface
    [PreserveSig]
    function Read(
      pv: IntPtr;
      cb: Longint;
      pcbRead: PLongint): HResult;
    [PreserveSig]
    function Write(
      pv: IntPtr;
      cb: Longint;
      pcbWritten: PLongint): HResult;
  end;

function GetErrorInfo(dwReserved: Longint; out errinfo: IErrorInfo): HResult;
function CoGetMalloc(dwMemContext: Longint; out malloc: IMalloc): HResult;

// Multi Language support
type
  [ComImport, GuidAttribute('275c23e3-3747-11d0-9fea-00aa003f8646'), InterfaceTypeAttribute(ComInterfaceType.InterfaceIsIUnknown)]
  IEnumCodePage = interface
    [PreserveSig]
    function Clone(
      [Out, MarshalAs(UnmanagedType.IUnknown)]
      out ppEnum: IEnumCodePage): HResult;
    [PreserveSig]
    function Next(
      celt: ULONG;
      [Out, MarshalAs(UnmanagedType.LPStruct)]
      var rgelt: MIMECPINFO;
      var pceltFetched: ULONG): HResult;
    [PreserveSig]
    function Reset: HResult;
    [PreserveSig]
    function Skip(celt: ULONG): HResult;
  end;

  [ComImport, GuidAttribute('3dc39d1d-c030-11d0-b81b-00c04fc9b31f'), InterfaceTypeAttribute(ComInterfaceType.InterfaceIsIUnknown)]
  IEnumRfc1766 = interface
    [PreserveSig]
    function Clone(
      [Out, MarshalAs(UnmanagedType.IUnknown)]
      out ppEnum: IEnumRfc1766): HResult;
    [PreserveSig]
    function Next(
      celt: ULONG;
      [Out, MarshalAs(UnmanagedType.LPStruct)]
      var rgelt: RFC1766INFO;
      var pceltFetched: ULONG): HResult;
    [PreserveSig]
    function Reset: HResult;
    [PreserveSig]
    function Skip(celt: ULONG): HResult;
  end;
  
  [ComImport, GuidAttribute('D66D6F98-CDAA-11D0-B822-00C04FC9B31F'), InterfaceTypeAttribute(ComInterfaceType.InterfaceIsIUnknown)]
  IMLangConvertCharset = interface
    [PreserveSig]
    function Initialize(
      uiSrcCodePage: UINT;
      uiDstCodePage: UINT;
      dwProperty: DWORD): HResult;
    [PreserveSig]
    function GetSourceCodePage(
      var puiSrcCodePage: UINT): HResult;
    [PreserveSig]
    function GetDestinationCodePage(
      var puiDstCodePage: UINT): HResult;
    [PreserveSig]
    function GetProperty(
      var pdwProperty: DWORD): HResult;
    [PreserveSig]
    function DoConversion(
      pSrcStr: IntPtr;
      var pcSrcSize: UINT;
      pDstStr: IntPtr;
      var pcDstSize: UINT): HResult;
    [PreserveSig]
    function DoConversionToUnicode(
      pSrcStr: PChar;
      var pcSrcSize: UINT;
      [MarshalAs(UnmanagedType.LPWStr)]
      pDstStr: PWideChar;
      var pcDstSize: UINT): HResult;
    [PreserveSig]
    function DoConversionFromUnicode(
      [MarshalAs(UnmanagedType.LPWStr)]
      pSrcStr: PWideChar;
      var pcSrcSize: UINT;
      [MarshalAs(UnmanagedType.LPStr)]
      pDstStr: PChar;
      var pcDstSize: UINT): HResult;
  end;

  [ComImport, GuidAttribute('275C23E1-3747-11D0-9FEA-00AA003F8646'), InterfaceTypeAttribute(ComInterfaceType.InterfaceIsIUnknown)]
  IMultiLanguage = interface
    [PreserveSig]
    function GetNumberOfCodePageInfo(
      var pcCodePage: UINT): HResult;
    [PreserveSig]
    function GetCodePageInfo(
      uiCodePage: UINT;
      [Out, MarshalAs(UnmanagedType.Struct)]
      var pCodePageInfo: MIMECPINFO): HResult;
    [PreserveSig]
    function GetFamilyCodePage(
      uiCodePage: UINT;
      var puiFamilyCodePage: UINT): HResult;
    [PreserveSig]
    function EnumCodePages(
      grfFlags: DWORD;
      [Out, MarshalAs(UnmanagedType.IUnknown)]
      out ppEnumCodePage: IEnumCodePage): HResult;
    [PreserveSig]
    function GetCharsetInfo(
      [MarshalAs(UnmanagedType.LPWStr)]
      Charset: PWideChar;
      [Out, MarshalAs(UnmanagedType.Struct)]
      var pCharsetInfo: MIMECSETINFO): HResult;
    [PreserveSig]
    function IsConvertible(
      dwSrcEncoding: DWORD;
      dwDstEncoding: DWORD): HResult;
    [PreserveSig]
    function ConvertString(
      var pdwMode: DWORD;
      dwSrcEncoding: DWORD;
      dwDstEncoding: DWORD;
      pSrcStr: IntPtr;
      var pcSrcSize: UINT;
      pDstStr: IntPtr;
      var pcDstSize: UINT): HResult;
    [PreserveSig]
    function ConvertStringToUnicode(
      var pdwMode: DWORD;
      dwEncoding: DWORD;
      pSrcStr: PChar;
      var pcSrcSize: UINT;
      pDstStr: PWideChar;
      var pcDstSize: UINT): HResult;
    [PreserveSig]
    function ConvertStringFromUnicode(
      var pdwMode: DWORD;
      dwEncoding: DWORD;
      [MarshalAs(UnmanagedType.LPWStr)]
      pSrcStr: PWideChar;
      var pcSrcSize: UINT;
      pDstStr: PChar;
      var pcDstSize: UINT): HResult;
    [PreserveSig]
    function ConvertStringReset: HResult;
    [PreserveSig]
    function GetRfc1766FromLcid(
      Locale: LCID;
      [Out, MarshalAs(UnmanagedType.LPWStr)]
      var pbstrRfc1766: WideChar): HResult;
    [PreserveSig]
    function EnumRfc1766(
      [Out, MarshalAs(UnmanagedType.IUnknown)]
      out ppEnumRfc1766: IEnumRfc1766): HResult;
    [PreserveSig]
    function GetRfc1766Info(
      Locale: LCID;
      [Out, MarshalAs(UnmanagedType.Struct)]
      var pRfc1766Info: RFC1766INFO): HResult;
    [PreserveSig]
    function CreateConvertCharset(
      uiSrcCodePage: UINT;
      uiDstCodePage: UINT;
      dwProperty: DWORD;
      [Out, MarshalAs(UnmanagedType.IUnknown)]
      out ppMLangConvertCharset: IMLangConvertCharset): HResult;
  end;

const
  OffsetOf_DBID_uGuid = 0;
  OffsetOf_DBID_eKind = 16;
  OffsetOf_DBID_uName = 20; // Should be 20, but Marshal.OffsetOf return 24 in x64 bit

  function OpenSqlFilestream(
    [MarshalAs(UnmanagedType.LPWStr)]
    FilestreamPath: PWideChar;
    DesiredAccess: Integer; //SQL_FILESTREAM_DESIRED_ACCESS;
    OpenOptions: ULONG;
    const FilestreamTransactionContext: TBytes;
    FilestreamTransactionContextLength: DWORD;
    AllocationSize: IntPtr
  ): HResult;

  function ReadFile(hFile: THandle; Buffer: IntPtr; nNumberOfBytesToRead: Longint;
    out lpNumberOfBytesRead: Longint; lpOverlapped: IntPtr): BOOL; overload;

  function WriteFile(hFile: THandle; Buffer: IntPtr; nNumberOfBytesToWrite: Longint;
    out lpNumberOfBytesWritten: Longint; lpOverlapped: IntPtr): BOOL; overload;

implementation

uses
  SysUtils;

(*
procedure SetOleVariant(pValue: POleVariant; const Value: OleVariant);
var
  Temp: TBytes;
begin
  SetLength(Temp, 16);
  Marshal.Copy(pValue, Temp, 0, 16);
  Temp[0] := 1;
  Marshal.GetNativeVariantForObject(TObject(Value), pValue);
  Marshal.Copy(pValue, Temp, 0, 16);
  Temp[0] := 2;
end;
*)

{ PDBProp }

function PDBProp.GetdwPropertyID: DBPROPID;
begin
  Result := DBPROPID(Marshal.ReadInt32(Ptr, 0));
end;

procedure PDBProp.SetdwPropertyID(Value: DBPROPID);
begin
  Marshal.WriteInt32(Ptr, 0, Integer(Value));
end;

function PDBProp.GetdwOptions: DBPROPOPTIONS;
begin
  Result := DBPROPID(Marshal.ReadInt32(Ptr, 4));
end;

procedure PDBProp.SetdwOptions(Value: DBPROPOPTIONS);
begin
  Marshal.WriteInt32(Ptr, 4, Integer(Value));
end;

function PDBProp.GetdwStatus: DBPROPSTATUS;
begin
  Result := DBPROPID(Marshal.ReadInt32(Ptr, 8));
end;

procedure PDBProp.SetdwStatus(Value: DBPROPSTATUS);
begin
  Marshal.WriteInt32(Ptr, 8, Integer(Value));
end;

function PDBProp.Getcolid: _DBID;
var
  Temp: array of byte;
begin
{$IFDEF VER8}
  SetLength(Temp, 0); //To avoid compiler warning. Delphi8 bug.
{$ENDIF}
  Marshal.Copy(IntPtr(Integer(Ptr) + 12), Temp, 0, Length(DB_NULLID));
  Result := Temp;
end;

procedure PDBProp.Setcolid(Value: _DBID);
var
  Temp: array of byte;
begin
  Temp := Value;
  Marshal.Copy(Temp, 0, IntPtr(Integer(Ptr) + 12), Length(DB_NULLID));
end;

function PDBProp.GetvValue: OleVariant;
var
  pValue: POleVariant;
begin
  pValue := IntPtr(Integer(Ptr) + 12 + 24);
  Result := GetOleVariant(pValue);
end;

procedure PDBProp.SetvValue(Value: OleVariant);
var
  pValue: POleVariant;
begin
  pValue := IntPtr(Integer(Ptr) + 12 + 24);
  SetOleVariant(pValue, Value);
end;

class operator PDBProp.Implicit(AValue: IntPtr): PDBProp;
begin
  Result.Ptr := AValue;
end;

class operator PDBProp.Implicit(AValue: PDBProp): IntPtr;
begin
  Result := AValue.Ptr;
end;

{ PDBPropSet }

class operator PDBPropSet.Implicit(AValue: IntPtr): PDBPropSet;
begin
  Result.Ptr := AValue;
end;

class operator PDBPropSet.Implicit(AValue: PDBPropSet): IntPtr;
begin
  Result := AValue.Ptr;
end;

function PDBPropSet.GetrgProperties: PDBPropArray;
begin
  Result := Marshal.ReadIntPtr(Ptr);
end;

procedure PDBPropSet.SetrgProperties(Value: PDBPropArray);
begin
  Marshal.WriteIntPtr(Ptr, Value);
end;

function PDBPropSet.GetcProperties: UINT;
begin
  Result := UINT(Marshal.ReadInt32(Ptr, 4));
end;

procedure PDBPropSet.SetcProperties(Value: UINT);
begin
  Marshal.WriteInt32(Ptr, 4, Integer(Value));
end;

function PDBPropSet.GetguidPropertySet: TGUID;
begin
  Result := TGUID(Marshal.PtrToStructure(IntPtr(Integer(Ptr) + 8), TypeOf(TGUID)));
end;

procedure PDBPropSet.SetguidPropertySet(Value: TGUID);
begin
  Marshal.StructureToPtr(TObject(Value), IntPtr(Integer(Ptr) + 8), False);
end;

const
  ole32    = 'ole32.dll';
  oleaut32 = 'oleaut32.dll';
  sqlncli10 = 'sqlncli10.dll';

[DllImport(oleaut32, CharSet = CharSet.Ansi, SetLastError = True, EntryPoint = 'GetErrorInfo')]
function GetErrorInfo(dwReserved: Longint; out errinfo: IErrorInfo): HResult; external;

[DllImport(ole32, CharSet = CharSet.Ansi, SetLastError = True, EntryPoint = 'CoGetMalloc')]
function CoGetMalloc(dwMemContext: Longint; out malloc: IMalloc): HResult; external;

[SuppressUnmanagedCodeSecurity, DllImport(sqlncli10, CharSet = CharSet.Unicode, SetLastError = True, EntryPoint = 'OpenSqlFilestream')]
function OpenSqlFilestream(
  [MarshalAs(UnmanagedType.LPWStr)]
  FilestreamPath: PWideChar;
  DesiredAccess: Integer; //SQL_FILESTREAM_DESIRED_ACCESS;
  OpenOptions: ULONG;
  const FilestreamTransactionContext: TBytes;
  FilestreamTransactionContextLength: DWORD;
  AllocationSize: IntPtr
): HResult; external;

[SuppressUnmanagedCodeSecurity, DllImport(kernel32, CharSet = CharSet.Ansi, SetLastError = True, EntryPoint = 'ReadFile')]
function ReadFile(hFile: THandle; Buffer: IntPtr; nNumberOfBytesToRead: Longint;
  out lpNumberOfBytesRead: Longint; lpOverlapped: IntPtr): BOOL; external;

[SuppressUnmanagedCodeSecurity, DllImport(kernel32, CharSet = CharSet.Ansi, SetLastError = True, EntryPoint = 'WriteFile')]
function WriteFile(hFile: THandle; Buffer: IntPtr; nNumberOfBytesToWrite: Longint;
  out lpNumberOfBytesWritten: Longint; lpOverlapped: IntPtr): BOOL; external;

initialization

end.
