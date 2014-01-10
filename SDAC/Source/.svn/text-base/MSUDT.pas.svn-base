//////////////////////////////////////////////////
//  SQL Server Data Access Components
//  Copyright © 1998-2012 Devart. All right reserved.
//  MSUDT
//////////////////////////////////////////////////

{$IFNDEF UNIDACPRO}
{$I Sdac.inc}

unit MSUDT;
{$ENDIF}

interface

{$TYPEDADDRESS OFF} // Unit must be compiled without type-checked pointers.
{$IFDEF VER6P}
{$WARN SYMBOL_PLATFORM OFF}
{$ENDIF}
{$WRITEABLECONST ON}
{$IFDEF VER6P}
{$IFNDEF FPC}
{$VARPROPSETTER ON}
{$ENDIF}
{$ENDIF}

uses
  Windows, ActiveX, Classes, MemUtils, CLRClasses, SysUtils;

const
  // TypeLibrary Major and minor versions
  Devart_Sdac_UDTProxyMajorVersion = 1;
  Devart_Sdac_UDTProxyMinorVersion = 0;

  LIBID_Devart_Sdac_UDTProxy: TGUID = '{A7C92A4E-7EFD-4A24-9610-83BA8A7B09EC}';

  IID_IUDTProxy: TGUID = '{EBCB57AF-15D4-4A0B-A52F-F07871422D5C}';
  IID__UDTProxy: TGUID = '{C09BC106-A160-3DA7-B253-913B5AA2194D}';
  CLASS_UDTProxy: TGUID = '{67B585B0-589C-3168-AD3F-FCA62F2CD42E}';

type
  pExceptionData = ^TExceptionData;
  TExceptionData = packed record
    HResult: int;
    HelpLink: PWideChar;
    InnerException: pExceptionData;
    Message: PWideChar;
    Source: PWideChar;
    StackTrace: PWideChar;
  end;

  EMSUDTError = class(Exception)
  protected
    FHResult: int;
    FHelpLink: WideString;
    FInnerException: EMSUDTError;
    FMessageWide: WideString;
    FSource: WideString;
    FStackTrace: WideString;
  public
    constructor Create(const ed: TExceptionData);
    destructor Destroy; override;

    property HResult: int read FHResult;
    property HelpLink: WideString read FHelpLink;
    property InnerException: EMSUDTError read FInnerException;
    property MessageWide: WideString read FMessageWide;
    property Source: WideString read FSource;
    property StackTrace: WideString read FStackTrace;
  end;

// *********************************************************************//
// Interface: IUDTProxy
// Flags:     (256) OleAutomation
// GUID:      {EBCB57AF-15D4-4A0B-A52F-F07871422D5C}
// *********************************************************************//
  IUDTProxy = interface(IUnknown)
    ['{EBCB57AF-15D4-4A0B-A52F-F07871422D5C}']
    function LoadAssemblyTypeName(const AssemblyTypeName: PWideChar; out ped: pExceptionData): HResult; stdcall;
    function DeserializeInstance(Data: PSafeArray; out ped: pExceptionData): HResult; stdcall;
    function SerializeInstance(out Data: PSafeArray; out ped: pExceptionData): HResult; stdcall;
    function PrepareMember(const memberName: PWideChar; out Prepared: LongBool; out ped: pExceptionData): HResult; stdcall;
    function InvokeMember(Flags: Word; Params: PSafeArray; out Result: PSafeArray; out ped: pExceptionData): HResult; stdcall;
  end;

  TUDTDispatcher = class(TInterfacedObject, IDispatch)
  private
    FUDTProxy: IUDTProxy;
    FAfterInvoke: TNotifyEvent;
  public
    constructor Create;
    procedure InitUDTProxy;
    procedure ReleaseUDTProxy;
    property UDTProxy: IUDTProxy read FUDTProxy;
    property AfterInvoke: TNotifyEvent read FAfterInvoke write FAfterInvoke;
    
  { IDispatch support }
    function GetTypeInfoCount(out Count: Integer): HResult; stdcall;
    function GetTypeInfo(Index, LocaleID: Integer; out TypeInfo): HResult; stdcall;
    function GetIDsOfNames(const IID: TGUID; Names: Pointer;
      NameCount, LocaleID: Integer; DispIDs: Pointer): HResult; stdcall;
    function Invoke(DispID: Integer; const IID: TGUID; LocaleID: Integer;
      Flags: Word; var Params; VarResult, ExcepInfo, ArgErr: Pointer): HResult; stdcall;
  end;

  procedure UDTCheck(Res: HResult; ped: pExceptionData);
  procedure ExceptionDataToInfo(ped: pExceptionData; var ExcepInfo: TExcepInfo);

implementation

{ TUDTDispatcher }

uses
{$IFDEF VER6P}
  Variants, VarUtils,
{$ENDIF}
{$IFDEF FPC}
  Types,
{$ENDIF}
  ComObj, Registry;

procedure FreeExceptionData(ped: pExceptionData);
begin
  // MSDN: Marshal.FreeHGlobal exposes the LocalFree function from Kernel32.DLL

  LocalFree(Cardinal(ped.HelpLink));
  LocalFree(Cardinal(ped.Message));
  LocalFree(Cardinal(ped.Source));
  LocalFree(Cardinal(ped.StackTrace));
  if ped.InnerException <> nil then
    FreeExceptionData(ped.InnerException);

  LocalFree(Cardinal(ped));
end;

procedure UDTCheck(Res: HResult; ped: pExceptionData);
begin
  if Succeeded(Res) then
    Exit;

  if ped <> nil then
    raise EMSUDTError.Create(ped^)
  else
    OleError(Res);
end;

procedure ExceptionDataToInfo(ped: pExceptionData; var ExcepInfo: TExcepInfo);
var
  p: pExceptionData;
begin
  // in DispCallError (unit ComObj)
  // EOleException.Create(bstrDescription, scode, bstrSource, bstrHelpFile, dwHelpContext)

  p := ped;
  while p <> nil do begin
    ExcepInfo.scode := p.HResult;
    if ExcepInfo.{$IFNDEF FPC}bstrHelpFile{$ELSE}HelpFile{$ENDIF} <> '' then
      ExcepInfo.{$IFNDEF FPC}bstrHelpFile{$ELSE}HelpFile{$ENDIF} := #$D#$A + ExcepInfo.{$IFNDEF FPC}bstrHelpFile{$ELSE}HelpFile{$ENDIF};
    ExcepInfo.{$IFNDEF FPC}bstrHelpFile{$ELSE}HelpFile{$ENDIF} := p.HelpLink + ExcepInfo.{$IFNDEF FPC}bstrHelpFile{$ELSE}HelpFile{$ENDIF};
    p := p.InnerException;
  end;

  p := ped;
  while p <> nil do begin
    if ExcepInfo.{$IFNDEF FPC}bstrSource{$ELSE}Source{$ENDIF} <> '' then
      ExcepInfo.{$IFNDEF FPC}bstrSource{$ELSE}Source{$ENDIF} := #$D#$A + ExcepInfo.{$IFNDEF FPC}bstrSource{$ELSE}Source{$ENDIF};
    ExcepInfo.{$IFNDEF FPC}bstrSource{$ELSE}Source{$ENDIF} := '[' + WideString(p.Source) + ']' + WideString(p.StackTrace) + ExcepInfo.{$IFNDEF FPC}bstrSource{$ELSE}Source{$ENDIF};
    p := p.InnerException;
  end;

  p := ped;
  while p <> nil do begin
    if ExcepInfo.{$IFNDEF FPC}bstrDescription{$ELSE}Description{$ENDIF} <> '' then
      ExcepInfo.{$IFNDEF FPC}bstrDescription{$ELSE}Description{$ENDIF} := #$D#$A + ExcepInfo.{$IFNDEF FPC}bstrDescription{$ELSE}Description{$ENDIF};
    ExcepInfo.{$IFNDEF FPC}bstrDescription{$ELSE}Description{$ENDIF} := p.Message + ExcepInfo.{$IFNDEF FPC}bstrDescription{$ELSE}Description{$ENDIF};
    p := p.InnerException;
  end;

  ExcepInfo.dwHelpContext := 0;

  FreeExceptionData(ped);
end;

{ TUDTDispatcher }

constructor TUDTDispatcher.Create;
begin
  inherited;

  _AddRef;
end;

procedure TUDTDispatcher.InitUDTProxy;
  // Must be sync with CRSetupUtils.RegisterUDTProxy;
  procedure AddRegistryKey(KeyName: string; KeyValueName, KeyValue: string; CanCreate: boolean; Key: HKEY = HKEY_CURRENT_USER);
  var
    r: TRegistry;
    List: TStringList;
    i: integer;
  begin
    r := TRegistry.Create(KEY_READ OR KEY_WRITE);
    List := TStringList.Create;
    try
      r.RootKey := Key;
      if r.OpenKey(KeyName, CanCreate) then
        try
          r.GetValueNames(List);
          for i := 0 to List.Count - 1 do
            if AnsiCompareText(KeyValueName, List[i]) = 0 then
              r.DeleteValue(List[i]);
          if (KeyValueName <> '') or (KeyValue <> '') then
            r.WriteString(KeyValueName, KeyValue);
        finally
          r.CloseKey;
        end;
    finally
      r.Free;
      List.Free;
    end;
  end;

  // Must be sync with DASetupUtils.RegisterUDTProxy;
  procedure RegisterUDTProxy;
  begin
    AddRegistryKey('CLSID\{67B585B0-589C-3168-AD3F-FCA62F2CD42E}', '', 'Devart.Sdac.UDTProxy', True, HKEY_CLASSES_ROOT);
    AddRegistryKey('CLSID\{67B585B0-589C-3168-AD3F-FCA62F2CD42E}\InprocServer32', '', 'mscoree.dll', True, HKEY_CLASSES_ROOT);
    AddRegistryKey('CLSID\{67B585B0-589C-3168-AD3F-FCA62F2CD42E}\InprocServer32', 'ThreadingModel', 'Both', True, HKEY_CLASSES_ROOT);
    AddRegistryKey('CLSID\{67B585B0-589C-3168-AD3F-FCA62F2CD42E}\InprocServer32', 'Class', 'Devart.Sdac.UDTProxy', True, HKEY_CLASSES_ROOT);
    AddRegistryKey('CLSID\{67B585B0-589C-3168-AD3F-FCA62F2CD42E}\InprocServer32', 'Assembly', 'Devart.Sdac.UDTProxy, Version=1.0.0.0, Culture=neutral, PublicKeyToken=09af7300eec23701', True, HKEY_CLASSES_ROOT);
    AddRegistryKey('CLSID\{67B585B0-589C-3168-AD3F-FCA62F2CD42E}\InprocServer32', 'RuntimeVersion', 'v2.0.50727', True, HKEY_CLASSES_ROOT);
    AddRegistryKey('CLSID\{67B585B0-589C-3168-AD3F-FCA62F2CD42E}\InprocServer32\1.0.0.0', 'Class', 'Devart.Sdac.UDTProxy', True, HKEY_CLASSES_ROOT);
    AddRegistryKey('CLSID\{67B585B0-589C-3168-AD3F-FCA62F2CD42E}\InprocServer32\1.0.0.0', 'Assembly', 'Devart.Sdac.UDTProxy, Version=1.0.0.0, Culture=neutral, PublicKeyToken=09af7300eec23701', True, HKEY_CLASSES_ROOT);
    AddRegistryKey('CLSID\{67B585B0-589C-3168-AD3F-FCA62F2CD42E}\InprocServer32\1.0.0.0', 'RuntimeVersion', 'v2.0.50727', True, HKEY_CLASSES_ROOT);
    AddRegistryKey('CLSID\{67B585B0-589C-3168-AD3F-FCA62F2CD42E}\ProgId', '', 'Devart.Sdac.UDTProxy', True, HKEY_CLASSES_ROOT);
    AddRegistryKey('CLSID\{67B585B0-589C-3168-AD3F-FCA62F2CD42E}\Implemented Categories\{62C8FE65-4EBB-45E7-B440-6E39B2CDBF29}', '', '', True, HKEY_CLASSES_ROOT);
  end;

var
  hr: HResult;
begin
  if FUDTProxy = nil then begin
    // CoInitialize(nil);
    hr := CoCreateInstance(CLASS_UDTProxy, nil, CLSCTX_INPROC_SERVER, IID_IUDTProxy, FUDTProxy);
    if hr = REGDB_E_CLASSNOTREG then begin
      RegisterUDTProxy;
      hr := CoCreateInstance(CLASS_UDTProxy, nil, CLSCTX_INPROC_SERVER, IID_IUDTProxy, FUDTProxy);
    end;
    OleCheck(hr);
  end;
end;

procedure TUDTDispatcher.ReleaseUDTProxy;
begin
  FUDTProxy := nil;
end;

function TUDTDispatcher.GetTypeInfo(Index, LocaleID: Integer;
  out TypeInfo): HResult;
begin
  Pointer(TypeInfo) := nil;
  Result := S_OK;
end;

function TUDTDispatcher.GetTypeInfoCount(out Count: Integer): HResult;
begin
  Count := 0;
  Result := S_OK;
end;

function TUDTDispatcher.GetIDsOfNames(const IID: TGUID; Names: Pointer;
  NameCount, LocaleID: Integer; DispIDs: Pointer): HResult;
var
  Prepared: LongBool;
  ped: pExceptionData;
begin
  ped := nil;
  Assert(NameCount <= 1);
  UDTCheck(FUDTProxy.PrepareMember(PWideChar(Names^), Prepared, ped), ped);
  if Prepared then begin
    TDispIDList(DispIDs^)[0] := 1;
    Result := S_OK;
  end
  else begin
    TDispIDList(DispIDs^)[0] := DISPID_UNKNOWN;
    Result := DISP_E_UNKNOWNNAME;
  end;
end;

function TUDTDispatcher.Invoke(DispID: Integer; const IID: TGUID;
  LocaleID: Integer; Flags: Word; var Params; VarResult, ExcepInfo,
  ArgErr: Pointer): HResult;
{$IFNDEF VER6P}
type
  PIntArray = ^TIntArray;
  TIntArray = array[0..0] of Integer;
{$ENDIF}
var
  VarArg: TVariantArg;
  VarParam: Variant;
  VarParams: variant;
  PSafeVarParams: PVarArray;
  PVarResult: PVarArray;
  Indices: {$IFDEF VER6P}TVarArrayCoorArray{$ELSE}PIntArray{$ENDIF};
  i, ParamCount: integer;
  ped: pExceptionData;
begin
  if not CompareGuid(IID, GUID_NULL) then
    Result := DISP_E_UNKNOWNINTERFACE
  else begin
    VarParams := VarArrayCreate([0, TDispParams(Params).cArgs - 1], varVariant);
    ParamCount := TDispParams(Params).cArgs;
    for i := 0 to ParamCount - 1 do begin
      VarArg := TVariantArgList(PVariantArgList(TDispParams(Params).rgvarg)^)[ParamCount - 1 - i];
    {$IFNDEF FPC}
      VarParam := Variant(VarArg);
    {$ELSE}
      MemUtils.CopyBuffer(@VarArg, @VarParam, Sizeof(VarArg));
    {$ENDIF}
      VarParams[i] := VarParam;
    end;
    PSafeVarParams := VarArrayAsPSafeArray(VarParams);
    PVarResult := nil;
    Result := FUDTProxy.InvokeMember(Flags, PSafeArray(PSafeVarParams), PSafeArray(PVarResult), ped);
    if Result = S_OK then begin
      if VarResult <> nil then begin
      {$IFNDEF VER6P}
        GetMem(Indices, 1);
        try
      {$ENDIF}
          Indices[0] := 0;
        {$IFNDEF VER6P}
          VarResultCheck(SafeArrayGetElement(PSafeArray(PVarResult), Indices^, VarResult^));
        {$ELSE}
          VarResultCheck(SafeArrayGetElement(PVarResult, @Indices, VarResult));
        {$ENDIF}
      {$IFNDEF VER6P}
        finally
          FreeMem(Indices);
        end;
      {$ENDIF}
      end;
      if Assigned(FAfterInvoke) then
        FAfterInvoke(Self);
    end
    else
      ExceptionDataToInfo(ped, TExcepInfo(ExcepInfo^));
  end;
end;

{ EMSUDTError }

constructor EMSUDTError.Create(const ed: TExceptionData);
begin
  inherited Create(ed.Message);

  FHResult := ed.HResult;
  FHelpLink := ed.HelpLink;
  FMessageWide := ed.Message;
  FSource := ed.Source;
  FStackTrace := ed.StackTrace;

  if ed.InnerException <> nil then
    FInnerException := EMSUDTError.Create(ed.InnerException^);
end;

destructor EMSUDTError.Destroy;
begin
  FInnerException.Free;
  inherited;
end;

end.
