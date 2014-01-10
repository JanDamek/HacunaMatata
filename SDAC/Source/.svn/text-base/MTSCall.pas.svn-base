//////////////////////////////////////////////////
//  DB Access Components
//  Copyright © 1998-2012 Devart. All right reserved.
//  MTS Call
//////////////////////////////////////////////////


{$IFNDEF CLR}

{$I Dac.inc}

unit MTSCall;
{$ENDIF}

interface

uses
  SysUtils,
{$IFDEF MSWINDOWS}
  Windows,
{$ENDIF}
{$IFDEF CLR}
  System.Text, System.Runtime.InteropServices,
{$ELSE}
  CLRClasses,
{$ENDIF}
  CRTypes;

{$IFDEF MSWINDOWS}
{$IFNDEF LITE}
const
  ISOLATIONLEVEL_READUNCOMMITTED = $00000100;
  ISOLATIONLEVEL_READCOMMITTED = $00001000;
  ISOLATIONLEVEL_REPEATABLEREAD = $00010000;
  ISOLATIONLEVEL_SERIALIZABLE = $00100000;

{$IFDEF CLR}
  IID_ITransactionDispenser: string = '{3A6AD9E1-23B9-11CF-AD60-00AA00A74CCD}';
{$ELSE}
  IID_ITransactionDispenser: TGUID = '{3A6AD9E1-23B9-11CF-AD60-00AA00A74CCD}';
{$ENDIF}

type
{$IFDEF CLR}
  PCRBoid = IntPtr;
  CRBOID = packed record
    [MarshalAs(UnmanagedType.ByValArray, SizeConst = 16)]
    rgb_: array[0..15] of Byte;
  end;
  TCRBoid = CRBOID;

  CRXACTTRANSINFO = packed record
    uow: CRBOID;
    isoLevel: Integer;
    isoFlags: UINT;
    grfTCSupported: UINT;
    grfRMSupported: UINT;
    grfTCSupportedRetaining: UINT;
    grfRMSupportedRetaining: UINT;
  end;
  TCRXactTransInfo = CRXACTTRANSINFO;

  CRXACTOPT = packed record
    ulTimeout: UINT;
    [MarshalAs(UnmanagedType.ByValArray, SizeConst = 40)]
    szDescription: array[0..39] of Shortint;
  end;
  TCRXActOpt = CRXACTOPT;

  [ComImport,
  GuidAttribute('0FB15084-AF41-11CE-BD2B-204C4F4F5020'),
  InterfaceTypeAttribute(ComInterfaceType.InterfaceIsIUnknown)]
  ICRTransaction = interface // (IUnknown)
    [PreserveSig]
    function Commit(fRetaining: BOOL; grfTC: UINT; grfRM: UINT): HResult;
    [PreserveSig]
    function Abort(pboidReason: PCRBOID; fRetaining: BOOL; fAsync: BOOL): HResult;
    [PreserveSig]
    function GetTransactionInfo(out pinfo: CRXACTTRANSINFO): HResult;
  end;

  { Safecall Version }
  [ComImport,
  GuidAttribute('0FB15084-AF41-11CE-BD2B-204C4F4F5020'),
  InterfaceTypeAttribute(ComInterfaceType.InterfaceIsIUnknown)]
  ICRTransactionSC = interface // (IUnknown)
    procedure Commit(fRetaining: BOOL; grfTC: UINT; grfRM: UINT);
    procedure Abort(pboidReason: PCRBOID; fRetaining: BOOL; fAsync: BOOL);
    procedure GetTransactionInfo(out pinfo: CRXACTTRANSINFO);
  end;

  [ComImport,
  GuidAttribute('3A6AD9E0-23B9-11CF-AD60-00AA00A74CCD'),
  InterfaceTypeAttribute(ComInterfaceType.InterfaceIsIUnknown)]
  ICRTransactionOptions = interface // (IUnknown)
    [PreserveSig]
    function SetOptions(var pOptions: CRXACTOPT): HResult;
    [PreserveSig]
    function GetOptions(var pOptions: CRXACTOPT): HResult;
  end;

  { Safecall Version }
  [ComImport,
  GuidAttribute('3A6AD9E0-23B9-11CF-AD60-00AA00A74CCD'),
  InterfaceTypeAttribute(ComInterfaceType.InterfaceIsIUnknown)]
  ICRTransactionOptionsSC = interface // (IUnknown)
    procedure SetOptions(var pOptions: CRXACTOPT);
    procedure GetOptions(var pOptions: CRXACTOPT);
  end;
  
  { Safecall Version }
  [ComImport,
  GuidAttribute('3A6AD9E1-23B9-11CF-AD60-00AA00A74CCD'),
  InterfaceTypeAttribute(ComInterfaceType.InterfaceIsIUnknown)]
  ICRTransactionDispenserSC = interface // (IUnknown)
    procedure GetOptionsObject([MarshalAs(UnmanagedType.Interface)] out ppOptions: ICRTransactionOptions);
    procedure BeginTransaction([in, MarshalAs(UnmanagedType.IUnknown)] punkOuter: TObject;
      isoLevel: Integer; isoFlags: UINT;
      [in, MarshalAs(UnmanagedType.Interface)] var pOptions: ICRTransactionOptions;
      [MarshalAs(UnmanagedType.Interface)] out ppTransaction: ICRTransaction);
  end;
{$ELSE}
  PCRBoid = ^TCRBoid;
  CRBOID = packed record
    rgb_: array[0..15] of Byte;
  end;
  TCRBoid = CRBOID;

  PCRXactOpt = ^TCRXactOpt;
  CRXACTOPT = packed record
    ulTimeout: UINT;
    szDescription: array[0..39] of Shortint;
  end;
  TCRXActOpt = CRXACTOPT;


  PCRXactTransInfo = ^TCRXactTransInfo;
  CRXACTTRANSINFO = packed record
    uow: CRBOID;
    isoLevel: Integer;
    isoFlags: UINT;
    grfTCSupported: UINT;
    grfRMSupported: UINT;
    grfTCSupportedRetaining: UINT;
    grfRMSupportedRetaining: UINT;
  end;
  TCRXactTransInfo = CRXACTTRANSINFO;

  ICRTransaction = interface
    ['{0FB15084-AF41-11CE-BD2B-204C4F4F5020}']
    function Commit(fRetaining: BOOL; grfTC: UINT; grfRM: UINT): HResult; stdcall;
    function Abort(pboidReason: PCRBOID; fRetaining: BOOL; fAsync: BOOL): HResult; stdcall;
    function GetTransactionInfo(out pinfo: CRXACTTRANSINFO): HResult; stdcall;
  end;

  ICRTransactionOptions = interface
    ['{3A6AD9E0-23B9-11CF-AD60-00AA00A74CCD}']
    function SetOptions(var pOptions: CRXACTOPT): HResult; stdcall;
    function GetOptions(var pOptions: CRXACTOPT): HResult; stdcall;
  end;

  { Safecall Version }
  ICRTransactionSC = interface
    ['{0FB15084-AF41-11CE-BD2B-204C4F4F5020}']
    procedure Commit(fRetaining: BOOL; grfTC: UINT; grfRM: UINT); safecall;
    procedure Abort(pboidReason: PCRBOID; fRetaining: BOOL; fAsync: BOOL); safecall;
    procedure GetTransactionInfo(out pinfo: CRXACTTRANSINFO); safecall;
  end;

  { Safecall Version }
  ICRTransactionOptionsSC = interface
    ['{3A6AD9E0-23B9-11CF-AD60-00AA00A74CCD}']
    procedure SetOptions(var pOptions: CRXACTOPT); safecall;
    procedure GetOptions(var pOptions: CRXACTOPT); safecall;
  end;

  { Safecall Version }
  ICRTransactionDispenserSC = interface
    ['{3A6AD9E1-23B9-11CF-AD60-00AA00A74CCD}']
    procedure GetOptionsObject(out ppOptions: ICRTransactionOptions); safecall;
    procedure BeginTransaction(const punkOuter: IUnknown; isoLevel: Integer; isoFlags: UINT;
      const pOptions: ICRTransactionOptions; out ppTransaction: ICRTransaction); safecall;
  end;
{$ENDIF}
  _DtcGetTransactionManagerEx = function(pszHost: PAnsiChar; pszTmName: PAnsiChar;
                                      {$IFNDEF CLR}const riid: TGUID;{$ELSE}riid: IntPtr;{$ENDIF}
                                         grfOptions: Integer; pvConfigParams: IntPtr;
                                         out ppvObject: ICRTransactionDispenserSC): LongInt; {$IFNDEF CLR} cdecl; {$ENDIF}

var
  DtcGetTransactionManagerEx: _DtcGetTransactionManagerEx;

procedure InitMSDTC;
procedure FreeMSDTC;

{$ENDIF}
{$ENDIF}

implementation

{$IFDEF MSWINDOWS}
{$IFNDEF LITE}
var
  MSDTCInited: boolean;
  hMSDTCInit: TRTLCriticalSection;
  hxOLEhlpLib: HMODULE;

{$IFDEF CLR}
  [DllImport('xolehlp.dll', CharSet = CharSet.Ansi, SetLastError = True, CallingConvention=CallingConvention.Cdecl, EntryPoint = 'DtcGetTransactionManagerEx')]
  function xolehlp_DtcGetTransactionManagerEx(pszHost: PAnsiChar; pszTmName: PAnsiChar; riid: IntPtr; grfOptions: Integer;
    pvConfigParams: IntPtr; out ppvObject: ICRTransactionDispenserSC): LongInt; external;
{$ENDIF}

procedure InitMSDTC;
begin
  EnterCriticalSection(hMSDTCInit);
  try
    if MSDTCInited then
      Exit;

  {$IFNDEF CLR}
    hxOLEhlpLib := LoadLibrary(PChar('xolehlp.dll'));
    if hxOLEhlpLib <> 0 then
      DtcGetTransactionManagerEx := GetProcAddress(hxOLEhlpLib, PChar('DtcGetTransactionManagerEx'))
    else
      raise Exception.Create('Can not find xolehlp.dll');
  {$ELSE}
    DtcGetTransactionManagerEx := xolehlp_DtcGetTransactionManagerEx;
  {$ENDIF}

    MSDTCInited := True;
  finally
    LeaveCriticalSection(hMSDTCInit);
  end;
end;

procedure FreeMSDTC;
begin
  if not MSDTCInited then
    Exit;

  if hxOLEhlpLib <> 0 then begin
    FreeLibrary(hxOLEhlpLib);
    hxOLEhlpLib := 0;
  end;

  MSDTCInited := False;
end;
{$ENDIF}
{$ENDIF}

function NotLink: integer;
begin
  raise Exception.Create('Function is not linked');
end;

{$IFDEF MSWINDOWS}
{$IFNDEF LITE}
initialization
{$IFNDEF CLR}
  DtcGetTransactionManagerEx := @NotLink;
{$ENDIF}
  MSDTCInited := False;
  InitializeCriticalSection(hMSDTCInit);
  hxOLEhlpLib := 0;

finalization
  DeleteCriticalSection(hMSDTCInit);
  FreeMSDTC;
{$ENDIF}
{$ENDIF}
  
end.
