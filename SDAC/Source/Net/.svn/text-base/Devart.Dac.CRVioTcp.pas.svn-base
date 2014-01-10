{$I ..\Dac.inc}

unit Devart.Dac.CRVioTcp;

interface

uses
  System.Text, System.Net, System.Net.Sockets, System.Threading,
  Classes, CRTypes, CRVio;

const
  DefaultTimeOut = 30;

type
  TCRTcpClient = class(TcpClient)
  public
    constructor Create;
    function Handle: IntPtr;
  end;

  TCRVioTcp = class(TCRVio)
  protected
    Ftcp: TCRTcpClient;
    Fstream: NetworkStream;
    Fhostname: string;
    Fport: integer;
    FConnectionTimeOut: integer;
    FSendTimeout: integer;
    FReceiveTimeout: integer;

    procedure SetHost(const Value: string);
    procedure SetPort(const Value: integer);
    function GetTimeout: integer; override;
    procedure SetTimeout(Value: integer); override;
    procedure SetConnectionTimeOut(Value: integer);
    procedure SetSendTimeout(Value: integer);
    procedure SetReceiveTimeout(Value: integer);

    procedure Init;

  public
    constructor Create; overload;
    constructor Create(const hostname: string; const port: integer); overload;

    procedure Connect; override;
    procedure Close; override;
    function Read(var{performance opt} buffer: TBytes; offset, count: integer): integer; override;
    function Write(const{performance opt} buffer: TBytes; offset, count: integer): integer; override;
    function ReadNoWait(var{performance opt} buffer: TBytes; offset, count: integer): integer; override;
    function WriteNoWait(const{performance opt} buffer: TBytes; offset, count: integer): integer; override;
    function IsActive: boolean;

    function GetSocket: IntPtr; virtual;
    property ConnectionTimeOut: integer read FConnectionTimeOut write SetConnectionTimeOut;
    property SendTimeout: integer read FSendTimeout write SetSendTimeout;
    property ReceiveTimeout: integer read FReceiveTimeout write SetReceiveTimeout;
    property Host: string read Fhostname write SetHost;
    property Port: integer read Fport write SetPort;
  end;

function GetSocketError: integer;

implementation

uses
  SysUtils,
  Windows, Math;

function GetSocketError: integer;
begin
  Result := GetLastError;
end;

function IsIpAddress(const hostname: string): boolean;
var
  i: integer;
  ch: char;
begin
  Result := hostname = '';
  if not Result then
    Exit;

  for i := 1 to Length(hostname) do begin
    ch := hostname[i];
    if ((ch < '0') or (ch > '9')) and (ch <> '.') then begin
      Result := False;
      Exit;
    end;
  end;
end;

{ TCRTcpClient }

constructor TCRTcpClient.Create;
begin
  inherited;
  GC.SuppressFinalize(Client);
end;

function TCRTcpClient.Handle: IntPtr;
begin
  Assert(Client <> nil);
  Result := Client.Handle;
end;

{ TCRVioTcp }

constructor TCRVioTcp.Create;
begin
  inherited Create;

  Fhostname := '';
  Fport := 0;

  Init;
end;

constructor TCRVioTcp.Create(const hostname: string; const port: integer);
begin
  inherited Create;

  Fhostname := hostname;
  Fport := port;

  Init;
end;

procedure TCRVioTcp.Init;
begin
  FConnectionTimeOut := 0;
  FReceiveTimeout := DefaultTimeOut;
  FSendTimeout := 0;
end;

procedure TCRVioTcp.SetHost(const Value: string);
begin
  if IsActive then
    raise SocketException.CreateFmt('Cannot change Host for open socket.', [])
  else
    Fhostname := Value
end;

procedure TCRVioTcp.SetPort(const Value: integer);
begin
  if IsActive then
    raise SocketException.CreateFmt('Cannot change Port for open socket.', [])
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
end;

procedure TCRVioTcp.SetConnectionTimeOut(Value: integer);
begin
  FConnectionTimeOut := Value;
end;

procedure TCRVioTcp.SetSendTimeout(Value: integer);
begin
  if Value > MaxInt div 1000 then
    Value := MaxInt div 1000;

  if FSendTimeout <> Value then begin
    if FTcp <> nil then
      FTcp.SendTimeout := value * 1000;
    FSendTimeout := value;
  end;
end;

procedure TCRVioTcp.SetReceiveTimeout(Value: integer);
begin
  if Value > MaxInt div 1000 then
    Value := MaxInt div 1000;

  if timeout <> Value then begin
    if FTcp <> nil then
      FTcp.ReceiveTimeout := value * 1000;
    FReceiveTimeout := value;
  end;
end;

procedure TCRVioTcp.Connect;
var
  ip: IPAddress;
begin
  inherited;

  Ftcp := TCRTcpClient.Create;
  Ftcp.ReceiveTimeout := FReceiveTimeout * 1000;
  Ftcp.SendTimeout := FSendTimeout * 1000;

  Ftcp.NoDelay := True;
  if IsIpAddress(Fhostname) then begin
    ip := IPAddress.Parse(Fhostname);
    Ftcp.Connect(ip, Fport);
  end
  else
    Ftcp.Connect(Fhostname, Fport);
  Fstream := Ftcp.GetStream;
  GC.SuppressFinalize(Fstream);
end;

procedure TCRVioTcp.Close;
begin
  if Fstream <> nil then begin
    Fstream.Close;
    Fstream.Free;
    Fstream := nil;
  end;

  if FTcp <> nil then begin
    Ftcp.Close;
    Ftcp.Free;
    Ftcp := nil;
  end;
end;

function TCRVioTcp.ReadNoWait(var{performance opt} buffer: TBytes; offset, count: integer): integer;
const
  MaxRecvSize: integer = 131072;
begin
  try
    Result := Fstream.Read(buffer, offset, Math.Min(MaxRecvSize, count));
  except
    Result := 0;
  end;
end;

function TCRVioTcp.WriteNoWait(const{performance opt} buffer: TBytes; offset, count: integer): integer;
begin
  try
    Fstream.Write(buffer, offset, count);
    Result := count;
  except
    Result := 0;
  end;
end;

function TCRVioTcp.Read(var{performance opt} buffer: TBytes; offset, count: integer): integer;
begin
  if Fstream = nil then
    Result := 0
  else
    Result := inherited Read(buffer, offset, count);
end;

function TCRVioTcp.Write(const{performance opt} buffer: TBytes; offset, count: integer): integer;
begin
  if Fstream = nil then
    Result := 0
  else
    Result := inherited Write(buffer, offset, count);
end;

function TCRVioTcp.GetSocket: IntPtr;
begin
  Result := Ftcp.Handle;
end;

function TCRVioTcp.IsActive: boolean;
begin
  Result := (Ftcp <> nil) and (Ftcp.Handle <> nil);
end;

end.
