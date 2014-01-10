
//////////////////////////////////////////////////
//  Copyright © 1998-2012 Devart. All right reserved.
//////////////////////////////////////////////////

{$IFNDEF CLR}

{$I Dac.inc}

unit CRVioTcpSSL;
{$ENDIF}

interface

uses
{$IFDEF MSWINDOWS}
  Windows,
{$ENDIF}
  Classes, SysUtils, SyncObjs,
{$IFDEF CLR}
  System.Runtime.InteropServices, Borland.Vcl.TypInfo,
{$ELSE}
  CLRClasses,
{$ENDIF}
  CRTypes, CRVio, CRVioTcp;

{$IFNDEF HAVE_OPENSSL}
  Error
{$ENDIF}

const
  SSL_VERIFY_NONE = 0;
  SSL_CTRL_SET_TMP_DH = 3;

type
  st_VioSSLConnectorFd = packed record
    ssl_context_: IntPtr;
    ssl_method_: IntPtr;
  end;

  TCRVioTcpSSL = class(TCRVioTcp)
  protected
    FisSecure: boolean;
    FSSL_key, FSSL_cert, FSSL_ca, FSSL_capath, FSSL_cipher: AnsiString;

    Fnewcon: st_VioSSLConnectorFd;
    Fssl_arg: IntPtr;

    procedure new_VioSSLConnectorFd;
    procedure SetisSecure(Value: boolean);

  public
    constructor Create(const hostname: AnsiString; const port: integer; const SSL_key, SSL_cert, SSL_ca, SSL_capath, SSL_cipher: AnsiString);
    function ReadNoWait({$IFDEF CLR}var{performance opt}{$ENDIF} buffer: TValueArr; offset, count: integer): integer; override;
    function WriteNoWait(const{performance opt} buffer: TValueArr; offset, count: integer): integer; override;

    property isSecure: boolean read FisSecure write SetisSecure;
  end;

{$IFDEF CLR}
function SSL_write(s: IntPtr; pBuf: IntPtr; len: longint): longint; cdecl; external 'ssleay32.dll';
function SSL_read(s: IntPtr; pBuf: IntPtr; len: longint): longint; cdecl; external 'ssleay32.dll';
function SSL_get_error(const s: IntPtr; ret_code: longint): longint; cdecl; external 'ssleay32.dll';
//function SSL_shutdown(s: IntPtr): longint; cdecl; external 'ssleay32.dll';
procedure SSL_free(s: IntPtr); cdecl; external 'ssleay32.dll';

procedure SSL_load_error_strings; cdecl; external 'ssleay32.dll';

function TLSv1_client_method: IntPtr; cdecl; external 'ssleay32.dll';
function SSL_library_init: longint; cdecl; external 'ssleay32.dll';

function SSL_CTX_new(meth: IntPtr): IntPtr; cdecl; external 'ssleay32.dll';
function SSL_CTX_set_cipher_list(actx: IntPtr; const str: PAnsiChar): longint; cdecl; external 'ssleay32.dll';
function SSL_new(s: IntPtr): IntPtr; cdecl; external 'ssleay32.dll';
function SSL_clear(s: IntPtr): longint; cdecl; external 'ssleay32.dll';
function SSL_SESSION_set_timeout(s: IntPtr; t: cardinal): longint; cdecl; external 'ssleay32.dll';
function SSL_get_session(const s: IntPtr): IntPtr; cdecl; external 'ssleay32.dll';
function SSL_set_fd(s: IntPtr; fd: longint): longint; cdecl; external 'ssleay32.dll';
procedure SSL_set_connect_state(s: IntPtr); cdecl; external 'ssleay32.dll';
function SSL_do_handshake(s: IntPtr): longint; cdecl; external 'ssleay32.dll';
//function SSL_get_peer_certificate(s: IntPtr): IntPtr; cdecl; external 'ssleay32.dll';
//function SSL_set_session(_to: IntPtr; session: IntPtr): longint; cdecl; external 'ssleay32.dll';
//function SSL_connect(s: IntPtr): longint; cdecl; external 'ssleay32.dll';
//function SSL_CIPHER_get_name(c: IntPtr): PAnsiChar; cdecl; external 'ssleay32.dll';
//function SSL_get_current_cipher(s: IntPtr): IntPtr; cdecl; external 'ssleay32.dll';
procedure SSL_CTX_set_verify(actx: IntPtr;mode: longint;acallback: IntPtr); cdecl; external 'ssleay32.dll';
function SSL_CTX_load_verify_locations(actx: IntPtr; const CAfile: PAnsiChar; const CApath: PAnsiChar): longint; cdecl; external 'ssleay32.dll';
function SSL_CTX_set_default_verify_paths(actx: IntPtr): longint; cdecl; external 'ssleay32.dll';
function SSL_CTX_use_certificate_file(actx: IntPtr; const afile: PAnsiChar; atype: longint): longint; cdecl; external 'ssleay32.dll';
function SSL_CTX_use_PrivateKey_file(actx: IntPtr; const afile: PAnsiChar; atype: longint): longint; cdecl; external 'ssleay32.dll';
function SSL_CTX_check_private_key(const actx: IntPtr): longint; cdecl; external 'ssleay32.dll';
function DH_new: IntPtr; cdecl; external 'libeay32.dll';
function DH_free(dh: IntPtr): longint; cdecl; external 'libeay32.dll';
procedure OPENSSL_add_all_algorithms_noconf; cdecl; external 'libeay32.dll';
function BN_bin2bn(const s: IntPtr; len: longint;ret: IntPtr): IntPtr; cdecl; external 'libeay32.dll';
function X509_get_subject_name(a: IntPtr): IntPtr; cdecl; external 'libeay32.dll';
function X509_NAME_oneline(a: IntPtr; buf: PAnsiChar;size: longint): PAnsiChar; cdecl; external 'libeay32.dll';
function X509_STORE_CTX_get_error_depth(actx: IntPtr): longint; cdecl; external 'libeay32.dll';
function X509_STORE_CTX_get_error(actx: IntPtr): longint; cdecl; external 'libeay32.dll';
function X509_STORE_CTX_get_current_cert(actx: IntPtr): IntPtr; cdecl; external 'libeay32.dll';
function SSL_CTX_ctrl(actx: IntPtr; a1: longint;a2: longint; adh: IntPtr): longint; cdecl; external 'ssleay32.dll';
//function X509_verify_cert_error_string(n: longint): PAnsiChar; cdecl; external 'libeay32.dll';
function X509_get_issuer_name(a: IntPtr): IntPtr; cdecl; external 'libeay32.dll';
//function ERR_get_error_line_data(const afile: IntPtr; line: IntPtr; const data: IntPtr;flags: IntPtr): longint; cdecl; external 'libeay32.dll';
//function ERR_error_string(e: cardinal; buf: PAnsiChar): PAnsiChar; cdecl; external 'libeay32.dll';
//procedure X509_free(a: IntPtr); cdecl; external 'libeay32.dll';

{$ELSE}

var
  SSL_write: function(s: IntPtr; pBuf: IntPtr; len: longint): longint; cdecl;
  SSL_read: function(s: IntPtr; pBuf: IntPtr; len: longint): longint; cdecl; 
  SSL_get_error: function(const s: IntPtr; ret_code: longint): longint; cdecl;
  //SSL_shutdown: function(s: IntPtr): longint; cdecl; 
  SSL_free: procedure(s: IntPtr); cdecl;

  SSL_load_error_strings: procedure; cdecl; 
  TLSv1_client_method: function: IntPtr; cdecl;
  SSL_library_init: function: longint; cdecl;

  SSL_CTX_new: function(meth: IntPtr): IntPtr; cdecl; 
  SSL_CTX_set_cipher_list: function(actx: IntPtr; const str: PAnsiChar): longint; cdecl; 
  SSL_new: function(s: IntPtr): IntPtr; cdecl; 
  SSL_clear: function(s: IntPtr): longint; cdecl; 
  SSL_SESSION_set_timeout: function(s: IntPtr; t: cardinal): longint; cdecl; 
  SSL_get_session: function(s: IntPtr): IntPtr; cdecl; 
  SSL_set_fd: function(s: IntPtr; fd: longint): longint; cdecl; 
  SSL_set_connect_state: procedure(s: IntPtr); cdecl; 
  SSL_do_handshake: function(s: IntPtr): longint; cdecl; 
  //SSL_get_peer_certificate: function(s: IntPtr): IntPtr; cdecl;
  //SSL_set_session: function(_to: IntPtr; session: IntPtr): longint; cdecl;
  //SSL_connect: function(s: IntPtr): longint; cdecl; 
  //SSL_CIPHER_get_name: function(c: IntPtr): PAnsiChar; cdecl; 
  //SSL_get_current_cipher: function(s: IntPtr): IntPtr; cdecl; 
  SSL_CTX_set_verify: procedure(actx: IntPtr;mode: longint;acallback: IntPtr); cdecl; 
  SSL_CTX_load_verify_locations: function(actx: IntPtr; const CAfile: PAnsiChar; const CApath: PAnsiChar): longint; cdecl; 
  SSL_CTX_set_default_verify_paths: function(actx: IntPtr): longint; cdecl;
  SSL_CTX_use_certificate_file: function(actx: IntPtr; const afile: PAnsiChar; atype: longint): longint; cdecl; 
  SSL_CTX_use_PrivateKey_file: function(actx: IntPtr; const afile: PAnsiChar; atype: longint): longint; cdecl; 
  SSL_CTX_check_private_key: function(const actx: IntPtr): longint; cdecl; 
  DH_new: function: IntPtr; cdecl; 
  DH_free: function(dh: IntPtr): longint; cdecl; 
  OPENSSL_add_all_algorithms_noconf: procedure; cdecl; 
  BN_bin2bn: function(const s: IntPtr; len: longint;ret: IntPtr): IntPtr; cdecl; 
  X509_get_subject_name: function(a: IntPtr): IntPtr; cdecl; 
  X509_NAME_oneline: function(a: IntPtr; buf: PAnsiChar;size: longint): PAnsiChar; cdecl; 
  X509_STORE_CTX_get_error_depth: function(actx: IntPtr): longint; cdecl; 
  X509_STORE_CTX_get_error: function(actx: IntPtr): longint; cdecl; 
  X509_STORE_CTX_get_current_cert: function(actx: IntPtr): IntPtr; cdecl; 
  SSL_CTX_ctrl: function(actx: IntPtr; a1: longint; a2: longint; adh: IntPtr): longint; cdecl; 
  //X509_verify_cert_error_string: function(n: longint): PAnsiChar; cdecl; 
  X509_get_issuer_name: function(a: IntPtr): IntPtr; cdecl; 
  //ERR_get_error_line_data: function(const afile: IntPtr; line: IntPtr; const data: IntPtr;flags: IntPtr): longint; cdecl; 
  //ERR_error_string: function(e: cardinal; buf: PAnsiChar): PAnsiChar; cdecl; 
  //X509_free: function(a: IntPtr); cdecl; 
{$ENDIF}

var
  LIBEAY32DLL: string;
  SSLEAY32DLL: string;

var
  ssl_algorithms_added: boolean = False;
  ssl_error_strings_loaded: boolean = False;

{$IFNDEF CLR}
function vio_verify_callback(ok: longint; ctx: IntPtr): longint; cdecl;
{$ENDIF}
function vio_set_cert_stuff(ctx: IntPtr; const cert_file: AnsiString; key_file: AnsiString): longint;
function get_dh512: IntPtr;

procedure InitSSLLib;
procedure LoadSSLLib;

implementation

uses
{$IFDEF VER12P}
  AnsiStrings,
{$ENDIF}
{$IFDEF POSIX}
  Posix.Dlfcn,
{$ENDIF}
{$IFDEF UNIX}
  dl,
{$ENDIF}
  CRFunctions;

constructor TCRVioTcpSSL.Create(const hostname: AnsiString; const port: integer; const SSL_key, SSL_cert, SSL_ca, SSL_capath, SSL_cipher: AnsiString);
begin
  inherited Create(hostname, port);
  FSSL_key := SSL_key;
  FSSL_cert := SSL_cert;
  FSSL_ca := SSL_ca;
  FSSL_capath := SSL_capath;
  FSSL_cipher := SSL_cipher;

  InitSSLLib;
end;

procedure TCRVioTcpSSL.new_VioSSLConnectorFd;
var
  // r: int;
  dh: IntPtr;
begin
  Fnewcon.ssl_context_ := nil;
  Fnewcon.ssl_method_ := nil;

  if SSL_library_init = 0 then
    raise Exception.Create('SSL library initialization failed');

  if not ssl_algorithms_added then begin
    ssl_algorithms_added := True;
    OpenSSL_add_all_algorithms_noconf;
  end;
  if not ssl_error_strings_loaded then begin
    ssl_error_strings_loaded := TRUE;
    SSL_load_error_strings();
  end;

  Fnewcon.ssl_method_ := TLSv1_client_method();
  Fnewcon.ssl_context_ := SSL_CTX_new(Fnewcon.ssl_method_);

  Assert(Fnewcon.ssl_context_ <> nil);
  (*
    SSL_CTX_set_options
    SSL_CTX_set_info_callback
   *)
  if Trim(FSSL_cipher) <> '' then
    {r := }SSL_CTX_set_cipher_list(Fnewcon.ssl_context_, PAnsiChar(FSSL_cipher));
{$IFDEF CLR}
  // ???
{$ELSE}
  SSL_CTX_set_verify(Fnewcon.ssl_context_, SSL_VERIFY_NONE, @vio_verify_callback);//???
{$ENDIF}
  Assert(vio_set_cert_stuff(Fnewcon.ssl_context_, PAnsiChar(FSSL_cert), PAnsiChar(FSSL_key)) <> -1);

  if SSL_CTX_load_verify_locations(Fnewcon.ssl_context_, PAnsiChar(FSSL_ca), PAnsiChar(FSSL_capath)) = 0 then
    Assert(SSL_CTX_set_default_verify_paths(Fnewcon.ssl_context_) <> 0);

  (* DH stuff *)
  dh := get_dh512();
  Assert(dh <> nil);
  try
    // SSL_CTX_set_tmp_dh(Fnewcon.ssl_context_, dh);
    // #define SSL_CTX_set_tmp_dh(ctx,dh) \
    //   SSL_CTX_ctrl(ctx,SSL_CTRL_SET_TMP_DH,0,(char *)dh)
    SSL_CTX_ctrl(Fnewcon.ssl_context_, SSL_CTRL_SET_TMP_DH, 0, dh);
  finally
    DH_free(dh);
  end;
end;

procedure TCRVioTcpSSL.SetisSecure(Value: boolean);
var
  r, r1, r2: integer;
begin
  Assert(Value = True);
{$IFNDEF CLR}
  Assert(Fsd <> -1);
{$ENDIF}
  new_VioSSLConnectorFd;

(*???  net_blocking := vio_is_blocking(vio);
  vio_blocking(vio, 1, &unused);	(* Must be called before reset * )
  vio_reset(vio,VIO_TYPE_SSL,vio->sd,0,FALSE);*)
  Fssl_arg := SSL_new(Fnewcon.ssl_context_);
  try
    Assert(Fssl_arg <> nil, 'SSL_new failure');

    SSL_clear(Fssl_arg);
    SSL_SESSION_set_timeout(SSL_get_session(Fssl_arg), timeout);
  {$IFDEF CLR}
    SSL_set_fd(Fssl_arg, Integer(Ftcp.Handle));
  {$ELSE}
    SSL_set_fd(Fssl_arg, FSd);
  {$ENDIF}
    SSL_set_connect_state(Fssl_arg);
    r := SSL_do_handshake(Fssl_arg);
    r2 := r;
    r1 := SSL_get_error(Fssl_arg, r2);

    if r <> 1 then
      raise Exception.Create('SSL_do_handshake = ' + IntToStr(r) + #$D#$A + 'SSL_get_error(..., r2) = ' + IntToStr(r1) + #$D#$A + 'r2 = ' + IntToStr(r2));
    FisSecure := True;
  except
    // report_errors();
    SSL_free(Fssl_arg);
    Fssl_arg := nil;
    (*vio_reset(vio, old_type,vio.sd,0,FALSE);
    vio_blocking(vio, net_blocking, &unused);*)
    raise;
  end;
end;

function TCRVioTcpSSL.ReadNoWait({$IFDEF CLR}var{performance opt}{$ENDIF} buffer: TValueArr; offset,
  count: integer): integer;
var
  pbuffer: IntPtr;
{$IFDEF CLR}
  bufferGC: GCHandle;
{$ENDIF}
begin
  if not isSecure then begin
    Result := inherited ReadNoWait(buffer, offset, count);
    Exit;
  end;

{$IFDEF CLR}
  bufferGC := GCHandle.Alloc(buffer, GCHandleType.Pinned);
  pbuffer := Marshal.UnsafeAddrOfPinnedArrayElement(buffer, offset);
{$ELSE}
  pbuffer := @buffer[offset];
{$ENDIF}
  Result := SSL_read(Fssl_arg, pbuffer, count);
{$IFDEF CLR}
  bufferGC.Free;
{$ENDIF}
  if Result < 0 then
    Result := 0; // silent error handling
end;

function TCRVioTcpSSL.WriteNoWait(const buffer: TValueArr; offset,
  count: integer): integer;
var
  pbuffer: IntPtr;
{$IFDEF CLR}
  bufferGC: GCHandle;
{$ENDIF}
begin
  if not isSecure then begin
    Result := inherited WriteNoWait(buffer, offset, count);
    Exit;
  end;

{$IFDEF CLR}
  bufferGC := GCHandle.Alloc(buffer, GCHandleType.Pinned);
  pbuffer := Marshal.UnsafeAddrOfPinnedArrayElement(buffer, offset);
{$ELSE}
  pbuffer := @buffer[offset];
{$ENDIF}
  Result := SSL_write(Fssl_arg, pbuffer, count);
{$IFDEF CLR}
  bufferGC.Free;
{$ENDIF}
  if Result < 0 then
    Result := 0; // silent error handling
end;


var
{$IFDEF MSWINDOWS}
  hlibeay, hssleay: HMODULE;
{$ENDIF}
{$IFDEF UNIX}
  hlib: pointer;
{$ENDIF}
{$IFDEF POSIX}
  hlib: NativeUInt;
{$ENDIF}
  SSLInitLock: TCriticalSection;

{$IFDEF CLR}
  // ???
{$ELSE}
function vio_verify_callback(ok: longint; ctx: IntPtr): longint; cdecl;
type
  dummyctx = record
    pad: array[0..75]of byte;
    error: longint;
    current_cert: IntPtr;
  end;

var
  buf: array[0..255] of AnsiChar;
  err_cert: IntPtr;
  depth{, err}: longint;

begin
  //get the details
  err_cert := X509_STORE_CTX_get_current_cert(ctx);
  {err := }X509_STORE_CTX_get_error(ctx);
  depth := X509_STORE_CTX_get_error_depth(ctx);
  X509_NAME_oneline(X509_get_subject_name(err_cert), PAnsiChar(@buf), 256);
  if not (boolean(ok)) then begin
    if depth <= 0 then
      ok := 0
    else
      ok := 1;
  end;
  //some more details about the error
  case dummyctx(ctx^).error of
    2: //X509_V_ERR_UNABLE_TO_GET_ISSUER_CERT:
    begin
      X509_NAME_oneline(X509_get_issuer_name(dummyctx(ctx^).current_cert),buf,256);
      //showmessage('issuer= '+buf);
    end;
    9, 13: //X509_V_ERR_CERT_NOT_YET_VALID:
           //X509_V_ERR_ERROR_IN_CERT_NOT_BEFORE_FIELD:
      //DBUG_PRINT("error", ("notBefore"));
      //*ASN1_TIME_print_fp(stderr,X509_get_notBefore(ctx->current_cert));*/
    ;
    10, 14: //X509_V_ERR_CERT_HAS_EXPIRED:
            //X509_V_ERR_ERROR_IN_CERT_NOT_AFTER_FIELD:
            //DBUG_PRINT("error", ("notAfter error"));
            //*ASN1_TIME_print_fp(stderr,X509_get_notAfter(ctx->current_cert));*/
    ;
  end;
  result := ok;
end;
{$ENDIF}

////////////////////////////////////////////////////////////////////////////////
// sets the cert stuff
function vio_set_cert_stuff(ctx: IntPtr; const cert_file: AnsiString; key_file: AnsiString): longint;
begin
  Result := 0;
  if cert_file <> '' then begin //is there anything to set?
    //use it
    if (SSL_CTX_use_certificate_file(ctx, PAnsiChar(cert_file), 1{SSL_FILETYPE_PEM}) <= 0) then
      Exit;
    if key_file= '' then //do we have any key
      key_file := cert_file;
    //use it
    if SSL_CTX_use_PrivateKey_file(ctx, PAnsiChar(key_file), 1{SSL_FILETYPE_PEM}) <= 0 then
      Exit;
    //let's check it
    //if SSL_CTX_check_private_key(Marshal.ReadIntPtr(ctx)) <> 0 then
    //  Exit;
    if SSL_CTX_check_private_key(ctx) <> 0 then
      Exit;
  end;
  Result := 1;//no errors
end;

////////////////////////////////////////////////////////////////////////////////
// gets a new dh
function get_dh512: IntPtr;
const
  dh512_g: array[1..1] of byte = ($02);
  dh512_p: array[1..64] of byte =(
    $DA, $58, $3C, $16, $D9, $85, $22, $89, $D0, $E4, $AF, $75,
    $6F, $4C, $CA, $92, $DD, $4B, $E5, $33, $B8, $04, $FB, $0F,
    $ED, $94, $EF, $9C, $8A, $44, $03, $ED, $57, $46, $50, $D3,
    $69, $99, $DB, $29, $D7, $76, $27, $6B, $A2, $D3, $D4, $12,
    $E2, $18, $F4, $DD, $1E, $08, $4C, $F6, $D8, $00, $3E, $7C,
    $47, $74, $E8, $33);

type
  dhdummy = packed record
    pad: longint;
    version: longint;
    p: IntPtr;
    g: IntPtr;
    //the others are skipped
  end;

var
  dh, p, g: IntPtr;
  dh512_gp, dh512_pp: IntPtr;
{$IFDEF CLR}
  dh512_gGC, dh512_pGC: GCHandle;
{$ENDIF}

begin
  Result := nil;

{$IFDEF CLR}
  dh512_gGC := GCHandle.Alloc(dh512_g, GCHandleType.Pinned);
  dh512_pGC := GCHandle.Alloc(dh512_p, GCHandleType.Pinned);
  dh512_pp := Marshal.UnsafeAddrOfPinnedArrayElement(dh512_g, 0);
  dh512_gp := Marshal.UnsafeAddrOfPinnedArrayElement(dh512_p, 0);
  try
{$ELSE}
  dh512_pp := @dh512_p;
  dh512_gp := @dh512_g;
{$ENDIF}

  p := BN_bin2bn(dh512_pp, Length(dh512_p), nil); //set p
  g := BN_bin2bn(dh512_gp, Length(dh512_g), nil); //set g
{$IFDEF CLR}
  finally
    if IntPtr(dh512_gGC) <> nil then
      dh512_gGC.Free;
    if IntPtr(dh512_pGC) <> nil then
      dh512_pGC.Free;
  end;
{$ENDIF}

  if (p = nil) or (g = nil) then //any errors?
    Exit;
  dh := DH_new; //grab a dh
  if dh = nil then
    Exit;

{$IFDEF CLR}
  Marshal.WriteIntPtr(dh, 8, p);
  Marshal.WriteIntPtr(dh, 12, g);
{$ELSE}
  dhdummy(dh^).p := p;
  dhdummy(dh^).g := g;
{$ENDIF}
  Result := dh;
end;

{$IFNDEF CLR}
function NotLink: integer;
begin
  raise Exception.Create('SSL function is not linked. You should update SSL client library.');
end;
{$ENDIF}

function LoadedSSLLib: boolean;
begin
{$IFDEF MSWINDOWS}
  Result := (hlibeay > 0) and (hssleay > 0);
{$ENDIF}
{$IFDEF UNIX}
  Result := hlib <> nil;
{$ENDIF}
{$IFDEF POSIX}
  Result := hlib <> 0;
{$ENDIF}
end;

procedure LoadSSLLib;
{$IFNDEF CLR}
  procedure AssignProc(
  var
  {$IFDEF MSWINDOWS}
    hlib: HMODULE;
  {$ENDIF}
  {$IFDEF UNIX}
    hlib: pointer;
  {$ENDIF}
  {$IFDEF POSIX}
    hlib: NativeUInt;
  {$ENDIF}
    var Proc: pointer; const Name: string);
  begin
  {$IFDEF MSWINDOWS}
    Proc := GetProcAddress(hlib, PChar(Name));
  {$ELSE}
    Proc := dlsym(hlib, PAnsiChar(AnsiString(Name)));
  {$ENDIF}
    if Proc = nil then
      Proc := @NotLink;
  end;
{$ENDIF}
var
  Error: EOSError;
  Msg: string;
{$IFDEF UNIX}
  s: string;
  hssleay, hlibeay: pointer;
{$ENDIF}
{$IFDEF POSIX}
   hssleay, hlibeay: NativeUInt;
{$ENDIF}
begin
  if LoadedSSLLib then
    Exit;

{$IFDEF MSWINDOWS}
  if LIBEAY32DLL = '' then
    LIBEAY32DLL := 'libeay32.dll';
  if SSLEAY32DLL = '' then
    SSLEAY32DLL := 'ssleay32.dll';

  hlibeay := LoadLibraryEx(PChar(LIBEAY32DLL), 0, LOAD_WITH_ALTERED_SEARCH_PATH);
  hssleay := LoadLibraryEx(PChar(SSLEAY32DLL), 0, LOAD_WITH_ALTERED_SEARCH_PATH);
  if hssleay = 0 then begin
    SSLEAY32DLL := 'libssl32.dll';
    hssleay := LoadLibraryEx(PChar(SSLEAY32DLL), 0, LOAD_WITH_ALTERED_SEARCH_PATH);
  end;
{$ELSE}
  hlib := dlopen('libssl.so.0', RTLD_LAZY);
  hssleay := hlib;
  hlibeay := hlib;
{$ENDIF}
  if not LoadedSSLLib then begin
  {$IFDEF MSWINDOWS}
    Msg := 'OpenSSL client library couldn''t be loaded. Please place libeay32.dll and ssleay32.dll (or libssl32.dll) to system folder (included to PATH) or to the folder with executable unit of main program.';
  {$ENDIF}
  {$IFDEF UNIX}
    Msg := 'OpenSSL client library couldn''t be loaded. Please place libssl.so.0 file to system folder (included to LD_LIBRARY_PATH) or to the folder with executable unit of main program.';
    s := dlerror;
    if s <> '' then
      Msg := Msg + #$D#$A + s;
  {$ENDIF}
  {$IFDEF POSIX}
    Msg := 'OpenSSL client library couldn''t be loaded. Please place libssl.so.0 file to system folder (included to LD_LIBRARY_PATH) or to the folder with executable unit of main program.';
  {$ENDIF}
    Error := EOSError.Create(Msg);
  {$IFDEF MSWINDOWS}
    Error.ErrorCode := GetLastError;
  {$ENDIF}
    raise Error;
  end;

{$IFNDEF CLR}
  AssignProc(hssleay, @SSL_write, 'SSL_write');
  AssignProc(hssleay, @SSL_read, 'SSL_read');
  AssignProc(hssleay, @SSL_get_error, 'SSL_get_error');
  AssignProc(hssleay, @SSL_free, 'SSL_free');

  AssignProc(hssleay, @SSL_load_error_strings, 'SSL_load_error_strings');
  AssignProc(hssleay, @TLSv1_client_method, 'TLSv1_client_method');
  AssignProc(hssleay, @SSL_library_init, 'SSL_library_init');

  AssignProc(hssleay, @SSL_CTX_new, 'SSL_CTX_new');
  AssignProc(hssleay, @SSL_CTX_set_cipher_list, 'SSL_CTX_set_cipher_list');
  AssignProc(hssleay, @SSL_new, 'SSL_new');
  AssignProc(hssleay, @SSL_clear, 'SSL_clear');
  AssignProc(hssleay, @SSL_SESSION_set_timeout, 'SSL_SESSION_set_timeout');
  AssignProc(hssleay, @SSL_get_session, 'SSL_get_session');
  AssignProc(hssleay, @SSL_set_fd, 'SSL_set_fd');
  AssignProc(hssleay, @SSL_set_connect_state, 'SSL_set_connect_state');
  AssignProc(hssleay, @SSL_do_handshake, 'SSL_do_handshake');
  AssignProc(hssleay, @SSL_CTX_set_verify, 'SSL_CTX_set_verify');
  AssignProc(hssleay, @SSL_CTX_load_verify_locations, 'SSL_CTX_load_verify_locations');
  AssignProc(hssleay, @SSL_CTX_set_default_verify_paths, 'SSL_CTX_set_default_verify_paths');
  AssignProc(hssleay, @SSL_CTX_use_certificate_file, 'SSL_CTX_use_certificate_file');
  AssignProc(hssleay, @SSL_CTX_use_PrivateKey_file, 'SSL_CTX_use_PrivateKey_file');
  AssignProc(hssleay, @SSL_CTX_check_private_key, 'SSL_CTX_check_private_key');
  AssignProc(hssleay, @SSL_CTX_ctrl, 'SSL_CTX_ctrl');

  AssignProc(hlibeay, @DH_new, 'DH_new');
  AssignProc(hlibeay, @DH_free, 'DH_free');
  AssignProc(hlibeay, @OPENSSL_add_all_algorithms_noconf, 'OPENSSL_add_all_algorithms_noconf');
  if @OPENSSL_add_all_algorithms_noconf = @NotLink then
    AssignProc(hlibeay, @OPENSSL_add_all_algorithms_noconf, 'OpenSSL_add_all_algorithms');

  AssignProc(hlibeay, @BN_bin2bn, 'BN_bin2bn');
  AssignProc(hlibeay, @X509_get_subject_name, 'X509_get_subject_name');
  AssignProc(hlibeay, @X509_NAME_oneline, 'X509_NAME_oneline');
  AssignProc(hlibeay, @X509_STORE_CTX_get_error_depth, 'X509_STORE_CTX_get_error_depth');
  AssignProc(hlibeay, @X509_STORE_CTX_get_error, 'X509_STORE_CTX_get_error');
  AssignProc(hlibeay, @X509_STORE_CTX_get_current_cert, 'X509_STORE_CTX_get_current_cert');
  AssignProc(hlibeay, @X509_get_issuer_name, 'X509_get_issuer_name');
{$ENDIF}

end;

procedure InitSSLLib;
begin
  SSLInitLock.Enter;
  try
    if not LoadedSSLLib then
      LoadSSLLib;
  finally
    SSLInitLock.Leave;
  end;
end;

initialization
{$IFDEF MSWINDOWS}
  hlibeay := 0;
  hssleay := 0;
{$ENDIF}
{$IFDEF UNIX}
  hlib := nil;
{$ENDIF}
  LIBEAY32DLL := '';
  SSLEAY32DLL := '';
  SSLInitLock := TCriticalSection.Create;

finalization
{$IFDEF MSWINDOWS}
  if hlibeay <> 0 then begin
    FreeLibrary(hlibeay);
    hlibeay := 0;
  end;
  if hssleay <> 0 then begin
    FreeLibrary(hssleay);
    hssleay := 0;
  end;
{$ENDIF}
{$IFDEF POSIX}
  if hlib <> 0 then begin
    dlclose(hlib);
    hlib := 0;
  end;
{$ENDIF}
{$IFDEF UNIX}
  if hlib <> nil then begin
    dlclose(hlib);
    hlib := nil;
  end;
{$ENDIF}
  SSLInitLock.Free;

end.
