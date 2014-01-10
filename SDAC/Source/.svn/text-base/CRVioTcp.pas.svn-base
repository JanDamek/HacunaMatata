//////////////////////////////////////////////////
//  Copyright © 1998-2012 Devart. All right reserved.
//////////////////////////////////////////////////

{$IFNDEF CLR}
{$I Dac.inc}
unit CRVioTcp;
{$ENDIF}

interface

uses
  Classes, SysUtils,
{$IFNDEF CLR}
  CLRClasses,
{$ENDIF}
{$IFDEF MSWINDOWS}
  {$IFNDEF BCB}
  {$NOINCLUDE winsock}
  {$HPPEMIT '#include <winsock2.h>'}
  {$ENDIF}
  WinSock,
{$ENDIF}
{$IFDEF FPC}
  Sockets,
{$IFDEF UNIX}
  baseunix, unix, 
  {$IFNDEF IPHONESIM} // bug with using netdb in iOS 
  netdb,
  {$ENDIF}
{$ENDIF}
{$ENDIF}
  CRTypes, CRVio;

const
  LOCAL_HOST = 'localhost';

  DefaultTimeOut = 30;

  CR_IPSOCK_ERROR = 2005;
  CR_UNKNOWN_HOST = 2005;
  CR_CONN_HOST_ERROR = 2003;
  CR_SELECT_ERROR = 2006;
  CR_CONN_IS_OPEN = 2007;

{$IFNDEF MSWINDOWS}
const
  SOCKET_ERROR = -1;
{$ENDIF}

type
// bug with using netdb in iOS 
{$IFDEF FPC}
  {$IFDEF IPHONESIM}
  THostAddr = in_addr;
  
  THostEntry = record
    Name : String;
    Addr : THostAddr;
    Aliases : String;
  end;
  {$ENDIF}
{$ENDIF}

  SocketException = class(Exception)
  protected
    FErrorCode: Integer;
  public
    constructor CreateFmt(const Msg: string; const Args: array of const; ErrorCode: integer);
  {$EXTERNALSYM SocketException.CreateFmt}
    constructor Create(ErrorCode: integer);
  {$EXTERNALSYM SocketException.Create}

    property ErrorCode: integer read FErrorCode;
  end;

  TCRVioTcp = class(TCRVio)
  private
  protected
    Fhostname: AnsiString;
    Fport: integer;
    FConnectionTimeout: integer;
    FSendTimeout: integer;
    FReceiveTimeout: integer;

    FSd: longint;
    ffcntl_mode: longint;

    procedure SetHost(const Value: AnsiString);
    procedure SetPort(const Value: integer);
    function GetTimeout: integer; override;
    procedure SetTimeout(Value: integer); override;
    procedure SetConnectionTimeout(const Value: integer);
    procedure SetSendTimeout(const Value: integer);
    procedure SetReceiveTimeout(const Value: integer);
    function fastsend: longint;
    function keepalive(onoff: boolean): longint;
    function IPByHostName(const HostName: AnsiString): longword;

    procedure Init;

  public
    constructor Create; overload;
    constructor Create(const hostname: AnsiString; const port: integer); overload;

    procedure Connect; override;
    procedure Close; override;

    function ReadNoWait({$IFDEF CLR}var{performance opt}{$ENDIF} buffer: TValueArr; offset, count: integer): integer; override;
    function WriteNoWait(const{performance opt} buffer: TValueArr; offset, count: integer): integer; override;
    function Write(const buffer: TValueArr; offset, count: integer): integer; override;
  {$IFDEF MSWINDOWS}
    function WaitForData(Timeout: integer = -1): boolean; override;
  {$ENDIF}
    function IsActive: boolean;

    function GetSocket: longint; virtual;
    property ConnectionTimeout: integer read FConnectionTimeout write SetConnectionTimeout;
    property SendTimeout: integer read FSendTimeout write SetSendTimeout;
    property ReceiveTimeout: integer read FReceiveTimeout write SetReceiveTimeout;
    property Host: AnsiString read Fhostname write SetHost;
    property Port: integer read Fport write SetPort;
  end;

  THostNameResolver = class(TThread)
  private
    FVio: TCRVioTcp;
    FIPAddr: longword;
  protected
    procedure Execute; override;
  public
    constructor Create(Vio: TCRVioTcp);
  end;

function GetSocketError: integer;

implementation

uses
{$IFDEF MSWINDOWS}
  Windows, SyncObjs,
{$ENDIF}
{$IFDEF POSIX}
  Posix.ArpaInet, Posix.Errno, Posix.Fcntl,
  Posix.NetDB, Posix.NetinetIn, Posix.NetinetTCP,
  Posix.SysSocket, Posix.UniStd,
{$ENDIF}
  CRFunctions, MemUtils;

{$IFDEF MSWINDOWS}
var
  WsaData: TWSAData; // on windows winsock
  hLockWsaData: TCriticalSection;
{$ENDIF}

const
  SD_BOTH = 2;

{ Function }

{$IFDEF POSIX}

// for calling "setsockopt" with the same parameters for Windows and POSIX

function setsockopt(socket, level, option_name: Integer; option_value: IntPtr; option_len: socklen_t): Integer;
var
  temp: integer;
begin
  if option_value <> nil then
    Result := Posix.SysSocket.setsockopt(socket, level, option_name, option_value^, option_len)
  else begin
    temp := 0;
    Result := Posix.SysSocket.setsockopt(socket, level, option_name, temp, 0);
  end;
end;
{$ENDIF}

{$IFDEF UNIX}

const
  SO_NOSIGPIPE = $1022;
  {$EXTERNALSYM SO_NOSIGPIPE}

function inet_addr(cp: PAnsiChar): longword;
begin
  Result := StrToHostAddr(PAnsiChar(cp)).s_addr;
  // Invert address
  Result := SwapLongWord(Result);

  if Result = 0 then
    Result := longword(INADDR_NONE);
end;

function setsockopt(socket, level, option_name: Integer; option_value: IntPtr; option_len: socklen_t): Integer;
begin
  Result := fpsetsockopt(socket, level, option_name, option_value, option_len);
end;

function socket(af, struct, protocol: Integer): TSocket;
begin
  Result := fpsocket(af, struct, protocol);
end;

function fcntl(socket: Integer; cmd: Integer): Integer;
begin
  Result := fpfcntl(socket, cmd);
end;

function shutdown(s: TSocket; how: Integer): Integer;
begin
  Result := fpshutdown(s, how);
end;

function recv(s: TSocket; var Buf; len, flags: Integer): Integer;
begin
  Result := fprecv(s, @Buf, len, flags);
end;

function send(s: TSocket; const Buf; len, flags: Integer): Integer;
begin
  Result := fpsend(s, @Buf, len, flags);
end;

{$ENDIF}

function GetSocketError: integer;
begin
{$IFDEF MSWINDOWS}
  Result := WSAGetLastError;
{$ELSE}
  Result := errno;
{$ENDIF}
end;

{ SocketException }

constructor SocketException.CreateFmt(const Msg: string; const Args: array of const; ErrorCode: integer);
begin
  inherited CreateFmt(Msg, Args);
  FErrorCode := ErrorCode;
end;

constructor SocketException.Create(ErrorCode: integer);
begin
  inherited Create('');
  FErrorCode := ErrorCode;
end;

{ TCRVioTcp }

constructor TCRVioTcp.Create;
begin
  inherited Create;

  Fhostname := '';
  Fport := 0;

  Init;
end;

constructor TCRVioTcp.Create(const hostname: AnsiString; const port: integer);
begin
  inherited Create;

  Fhostname := hostname;
  Fport := port;

  Init;
end;

procedure TCRVioTcp.Init;
begin
  FSd := SOCKET_ERROR;
  ffcntl_mode := 0;
  FConnectionTimeout := 0;
  FReceiveTimeout := DefaultTimeOut;
  FSendTimeout := 0;
end;

procedure TCRVioTcp.SetHost(const Value: AnsiString);
begin
  if IsActive then
    raise SocketException.CreateFmt('Cannot change Host for open socket.', [], CR_CONN_IS_OPEN)
  else
    Fhostname := Value
end;

procedure TCRVioTcp.SetPort(const Value: integer);
begin
  if IsActive then
    raise SocketException.CreateFmt('Cannot change Port for open socket.', [], CR_CONN_IS_OPEN)
  else
    Fport := Value;
end;

function TCRVioTcp.GetTimeout: integer;
begin
  Result := FReceiveTimeout;
end;

procedure TCRVioTcp.SetTimeout(Value: integer);
begin
  SetReceiveTimeout(Value);
  SetConnectionTimeout(Value);
end;

procedure TCRVioTcp.SetConnectionTimeout(const Value: integer);
begin
  FConnectionTimeout := Value;
end;

procedure TCRVioTcp.SetSendTimeout(const Value: integer);
var
  i: Int64;
begin
  if Value <> FSendTimeout then begin
    FSendTimeout := Value;

    if (FSd <> SOCKET_ERROR) and (Value > 0) then begin
      i := Int64(FSendTimeout) * 1000;
    {$IFDEF MSWINDOWS}
      Win32Check(setsockopt(FSd, SOL_SOCKET, SO_SNDTIMEO, @i, sizeof(i)) = 0);
    {$ELSE}
      setsockopt(FSd, SOL_SOCKET, SO_SNDTIMEO, @i, sizeof(i));
    {$ENDIF}
    end;
  end;
end;

procedure TCRVioTcp.SetReceiveTimeout(const Value: integer);
var
  i: Int64;
begin
  if Value <> FReceiveTimeout then begin
    FReceiveTimeout := Value;

    if FSd <> SOCKET_ERROR then begin
      i := Int64(FReceiveTimeout) * 1000;
      //ms-help://MS.MSDNQTR.2003FEB.1033/winsock/winsock/socket_options_for_windows_nt_and_windows_95_98_me_2.htm
    {$IFDEF MSWINDOWS}
      Win32Check(setsockopt(FSd, SOL_SOCKET, SO_RCVTIMEO, @i, sizeof(i)) = 0);
    {$ELSE}
      setsockopt(FSd, SOL_SOCKET, SO_RCVTIMEO, @i, sizeof(i));
    {$ENDIF}
    end;
  end;
end;

function TCRVioTcp.fastsend: longint;
var
  nodelay: longint;
begin
  Result := 0;
  if setsockopt(fsd, IPPROTO_IP, IP_TOS, nil, 0) <> 0 then begin
    nodelay := 1;
    if (setsockopt(fsd, IPPROTO_TCP, TCP_NODELAY, @nodelay, sizeof(nodelay))) <> 0 then
      Result := SOCKET_ERROR;
  end;
end;

function TCRVioTcp.keepalive(onoff: boolean): longint;
var
  opt: longint;
begin
  if onoff then
    opt := 1
  else
    opt := 0;

  Result := setsockopt(fsd, SOL_SOCKET, SO_KEEPALIVE, @opt, sizeof(opt));
end;

function TCRVioTcp.IPByHostName(const HostName: AnsiString): longword;
var
{$IFDEF MSWINDOWS}
  hp: PHostEnt;
{$ENDIF}
{$IFDEF POSIX}
  Res: integer;
  Hints: AddrInfo;
  HostAddrInfo: PAddrInfo;
{$ENDIF}
{$IFDEF  UNIX}
  hp: THostEntry;
  StartPos, EndPos, DefaultDomainListLen: integer;
  Domain, FullHostName: AnsiString;
{$ENDIF}
begin
  Result := longword(INADDR_NONE);

{$IFDEF MSWINDOWS}
  hp := gethostbyname(PAnsiChar(Hostname));
  if hp <> nil then 
    Result := PLongWord(hp.h_addr^)^
  else
    FLastError := Format('Socket error on connect. WSAGetLastError return %d($%X)', [WSAGetLastError, WSAGetLastError]);
{$ENDIF}

{$IFDEF POSIX}
  FillChar(@Hints, SizeOf(Hints), 0);
  Hints.ai_family := AF_INET; // PF_UNSPEC or AF_INET6 need much time to resolve host name
  Hints.ai_socktype := SOCK_STREAM;
  Hints.ai_protocol := IPPROTO_TCP;
  HostAddrInfo := nil;

  Res := GetAddrInfo(PAnsiChar(Hostname), nil, Hints, HostAddrInfo);
  if Res = 0 then
    Result := PSockAddr_In(HostAddrInfo^.ai_addr)^.sin_addr.s_addr
  else
    FLastError := Format('Socket error on connect: %s', [gai_strerror(Res)]);
{$ENDIF}

{$IFDEF UNIX}
  // bug with using netdb in iOS 
  {$IFNDEF IPHONESIM}
  if ResolveHostByName(PAnsiChar(Hostname), hp) then
    Result := hp.Addr.s_addr
  else if Pos('.', Hostname) = 0 then begin
    EndPos := 0;
    DefaultDomainListLen := Length(DefaultDomainList);
    while EndPos < DefaultDomainListLen do begin
      StartPos := EndPos + 1;
      repeat
        Inc(EndPos);
      until (EndPos >= DefaultDomainListLen) or (DefaultDomainList[EndPos] = ',');
      Domain := Trim(copy(DefaultDomainList, StartPos, EndPos - StartPos + 1));
      if ResolveHostByName(PAnsiChar(Hostname + '.' + Domain), hp) then begin
        Result := hp.Addr.s_addr;
        break;
      end;
    end;
  end;
  {$ENDIF}    
{$ENDIF}
end;

procedure TCRVioTcp.Connect;
var
{$IFDEF MSWINDOWS}
  i: integer;
  NameResolver: THostNameResolver;
{$ELSE}
  opt: longint;
{$ENDIF}
{$IFDEF POSIX}
  sock_addr: sockaddr_in;
{$ELSE}
  sock_addr: TSockAddr;
{$ENDIF}
  ip_addr: longword;
  OldTimeout: integer;
begin
  Assert(Fsd = SOCKET_ERROR);
  inherited;

{$IFDEF MSWINDOWS}
  hLockWsaData.Enter;
  try
    if WsaData.wVersion = 0 then begin // not initialized
      i := WSAStartup($0202, WsaData);
      if i <> 0 then
        raise SocketException.CreateFmt('WSAStartup failed. Error code %d', [i], i);
    end;
  finally
    hLockWsaData.Leave;
  end;
{$ENDIF}

  if Fhostname = '' then
    Fhostname := LOCAL_HOST;
  FSd := socket(AF_INET, SOCK_STREAM, 0); //try grab a socket
  if FSd = SOCKET_ERROR then //error?
    raise SocketException.CreateFmt('Socket error %d', [GetSocketError], CR_IPSOCK_ERROR);

  try
  {$IFDEF MSWINDOWS}
    // arg:=0;
    // ioctlsocket(FSd, FIONBIO, longint(arg));
  {$ELSE}
    ffcntl_mode := fcntl(FSd, F_GETFL);
    opt := 1;
    setsockopt(fsd, SOL_SOCKET, SO_NOSIGPIPE, @opt, sizeof(opt));
  {$ENDIF}

    //try to resolve the host
    System.FillChar(sock_addr, SizeOf(sock_addr), #0);
    sock_addr.sin_family := AF_INET;
    ip_addr := longword(inet_addr(PAnsiChar(Fhostname)));

    if ip_addr = longword(INADDR_NONE) then begin
    {$IFDEF MSWINDOWS}
      if FConnectionTimeout > 0 then begin
        NameResolver := THostNameResolver.Create(Self);
        try
          if WaitForSingleObject(NameResolver.Handle, DWORD(FConnectionTimeout) * 1000) = WAIT_OBJECT_0 then
            ip_addr := NameResolver.FIPAddr
          else begin
            TerminateThread(NameResolver.Handle, 0);
            ip_addr := longword(INADDR_NONE);
            FLastError := 'Connection timed out';
          end;
        finally
          NameResolver.Free;
        end;
      end
      else
        ip_addr := IPByHostName(Fhostname);
    {$ELSE}
      ip_addr := IPByHostName(Fhostname);
    {$ENDIF}
      if ip_addr = longword(INADDR_NONE) then
        raise SocketException.CreateFmt('Socket error %d', [GetSocketError], CR_UNKNOWN_HOST);
    end;

    sock_addr.sin_addr.S_addr := {$IFDEF MSWINDOWS}Longint{$ENDIF}(ip_addr);

    sock_addr.sin_port := htons(Fport);

  {$IFNDEF FPC}
    {$IFDEF POSIX}
    if (Posix.SysSocket.connect(FSd, sockaddr(sock_addr), sizeof(sock_addr)) < 0)
    {$ELSE}
    if (WinSock.connect(FSd, TSockAddr(sock_addr), sizeof(sock_addr)) < 0)
    {$ENDIF}
  {$ELSE}
    // Libc.connect don't work due to unknown cause
    if (fpconnect(FSd, PSockAddr(@sock_addr), sizeof(sock_addr)) < 0)
  {$ENDIF}
  {$IFDEF MSWINDOWS}
       and (WSAGetLastError <> WSAEWOULDBLOCK)
  {$ELSE}
      {and ??? errno = EWOUDLBLOCK}
  {$ENDIF}
    then begin
    {$IFDEF MSWINDOWS}
      FLastError := Format('Socket error on connect. WSAGetLastError return %d($%X)', [WSAGetLastError, WSAGetLastError]);
    {$ENDIF}
      raise SocketException.CreateFmt('Socket error %d', [GetSocketError], CR_CONN_HOST_ERROR);
    end;

    fastsend; //attempt to use fastsend TODO:

    keepalive(True); // TODO:

    // Force to set timeout
    OldTimeout := FReceiveTimeout;
    try
      FReceiveTimeout := -1;
      SetReceiveTimeout(OldTimeout);
    finally
      FReceiveTimeout := OldTimeout;
    end;

    OldTimeout := FReceiveTimeout;
    try
      FSendTimeout := -1;
      SetSendTimeout(OldTimeout);
    finally
      FReceiveTimeout := OldTimeout;
    end;

  except
    ffcntl_mode := 0; //reset mode
  {$IFDEF MSWINDOWS}
    closesocket(FSd);
  {$ENDIF}
  {$IFDEF POSIX}
    Posix.UniStd.__close(FSd);
  {$ENDIF}
  {$IFDEF UNIX}
    closesocket(FSd);
  {$ENDIF}
    FSd := SOCKET_ERROR;
    raise;
  end;
end;

procedure TCRVioTcp.Close;
begin
  if FSd <> SOCKET_ERROR then begin
    shutdown(fsd, SD_BOTH);

  {$IFDEF MSWINDOWS}
    closesocket(FSd);
  {$ENDIF}
  {$IFDEF POSIX}
    Posix.UniStd.__close(FSd);
  {$ENDIF}
  {$IFDEF UNIX}
    closesocket(FSd);
  {$ENDIF}
    FSd := SOCKET_ERROR;
  end;
  ffcntl_mode := 0;
end;

function TCRVioTcp.ReadNoWait({$IFDEF CLR}var{performance opt}{$ENDIF} buffer: TValueArr; offset, count: integer): integer;
begin
  FLastError := '';
  Result := recv(FSd, buffer[offset], count, 0);
  if Result = SOCKET_ERROR then begin
  {$IFDEF MSWINDOWS}
    FLastError := Format('Socket error on read. WSAGetLastError return %d($%X)', [WSAGetLastError, WSAGetLastError]);
  {$ENDIF}
    Result := 0; // silent error handling
  end;
end;

function TCRVioTcp.WriteNoWait(const{performance opt} buffer: TValueArr; offset, count: integer): integer;
begin
  FLastError := '';
  Result := send(FSd, buffer[offset], count, 0);
  if Result = SOCKET_ERROR then begin
  {$IFDEF MSWINDOWS}
    FLastError := Format('Socket error on write. WSAGetLastError return %d($%X)', [WSAGetLastError, WSAGetLastError]);
  {$ENDIF}
    Result := 0; // silent error handling
  end;
end;

function TCRVioTcp.Write(const buffer: TValueArr; offset,
  count: integer): integer;
begin
  if FSd = SOCKET_ERROR then
    Result := 0
  else
    Result := inherited Write(buffer, offset, count);
end;

{$IFDEF MSWINDOWS}
function TCRVioTcp.WaitForData(Timeout: integer = -1): boolean;
var
  Res: integer;
  ss: TFDSet;
  time: TTimeVal;
  ptime: PTimeVal;
begin
  ss.fd_count := 1;
  ss.fd_array[0] := FSd;
  if Timeout >= 0 then begin
    time.tv_sec := Timeout;
    time.tv_usec := 0;
    ptime := @time;
  end
  else
    ptime := nil;

  FLastError := '';
  Res := select(0, @ss, nil, nil, ptime);
  if Res = SOCKET_ERROR then begin
    FLastError := Format('Socket error on select. WSAGetLastError return %d($%X)', [WSAGetLastError, WSAGetLastError]);
    raise SocketException.Create(CR_SELECT_ERROR);
  end;
  Result := Res = 1;
end;
{$ENDIF}

function TCRVioTcp.IsActive: boolean;
begin
  Result := FSd <> SOCKET_ERROR;
end;

function TCRVioTcp.GetSocket: longint;
begin
  Result := FSd;
end;

{ THostNameResolver }

constructor THostNameResolver.Create(Vio: TCRVioTcp);
begin
  inherited Create(True);

  FVio := Vio;
  Resume;
end;

procedure THostNameResolver.Execute;
begin
  FIPAddr := FVio.IPByHostName(FVio.Fhostname);
end;

{$IFDEF MSWINDOWS}
initialization
  hLockWsaData := TCriticalSection.Create;
{$ENDIF}

{$IFDEF MSWINDOWS}
finalization
  hLockWsaData.Free;
{$ENDIF}

end.
