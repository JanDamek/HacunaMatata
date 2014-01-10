//////////////////////////////////////////////////
//  Copyright © 1998-2012 Devart. All right reserved.
//////////////////////////////////////////////////

{$IFNDEF CLR}

{$I Dac.inc}

unit CRVio;
{$ENDIF}

interface

uses
  Classes, SysUtils,
{$IFDEF PERF_COUNTER}
  Debug,
{$ENDIF}
{$IFDEF VIO_DEBUG}
  Debug,
{$ENDIF}
{$IFDEF CLR}
  System.Threading,
{$ELSE}
  CLRClasses,
{$ENDIF}
  CRTypes;

{$DEFINE BUFFERED_READ}

{$IFDEF BUFFERED_READ}
const
  // Copied from MySQL 5.0 client for performance reason
  VIO_READ_BUFFER_SIZE = 16384;
  VIO_UNBUFFERED_READ_MIN_SIZE = 2048;
{$ENDIF}

type
  TCRVio = class
  protected
    FLastError: string;

  {$IFDEF BUFFERED_READ}
    FBuffer: TBytes;
    FBufferLen{read_end}, FBufferPos {read_pos}: integer;
  {$ENDIF}

    function GetTimeout: integer; virtual; abstract;
    procedure SetTimeout(Value: integer); virtual; abstract;

  public
    procedure Connect; virtual;
    procedure Close; virtual; abstract;

    function ReadNoWait({$IFDEF CLR}var{performance opt}{$ENDIF} buffer: TValueArr; offset, count: integer): integer; virtual; abstract;
    function WriteNoWait(const{performance opt} buffer: TValueArr; offset, count: integer): integer; virtual; abstract;
    function Read({$IFDEF CLR}var{performance opt}{$ENDIF} buffer: TValueArr; offset, count: integer): integer; virtual;
    function Write(const{performance opt} buffer: TValueArr; offset, count: integer): integer; virtual;
    function WaitForData(Timeout: integer = -1): boolean; virtual;

    property Timeout: integer read GetTimeout write SetTimeout;
    property LastError: string read FLastError;
  end;

  TCRIOHandle = TObject;

  TCRIOHandler = class (TComponent)
  protected
    procedure RegisterClient(Client: TObject); virtual;
    procedure UnRegisterClient(Client: TObject); virtual;

    {$IFNDEF CLR}class{$ENDIF} procedure SetIsSecure(Handle: TCRIOHandle; const Value: Boolean); virtual;
    {$IFNDEF CLR}class{$ENDIF} function GetIsSecure(Handle: TCRIOHandle): Boolean; virtual;
    function GetHandlerType: string; virtual;

  public
    function Connect(const Server: string; const Port: integer;
      const SSL_key, SSL_cert, SSL_ca: string): TCRIOHandle; virtual; abstract;
    {$IFNDEF CLR}class{$ENDIF} procedure Disconnect(Handle: TCRIOHandle); virtual;
    {$IFNDEF CLR}class{$ENDIF} function ReadNoWait(Handle: TCRIOHandle; {$IFDEF CLR}var{performance opt}{$ENDIF} buffer: TValueArr; offset, count: integer): integer; virtual;
    {$IFNDEF CLR}class{$ENDIF} function Read(Handle: TCRIOHandle; {$IFDEF CLR}var{performance opt}{$ENDIF} buffer: TValueArr; offset, count: integer): integer; virtual;
    {$IFNDEF CLR}class{$ENDIF} function Write(Handle: TCRIOHandle; const buffer: TValueArr; offset, count: integer): integer; virtual;
    {$IFNDEF CLR}class{$ENDIF} function WaitForData(Handle: TCRIOHandle; Timeout: integer = -1): boolean; virtual;

    {$IFNDEF CLR}class{$ENDIF} function GetTimeout(Handle: TCRIOHandle): integer; virtual;
    {$IFNDEF CLR}class{$ENDIF} procedure SetTimeout(Handle: TCRIOHandle; Value: integer); virtual;
    property HandlerType: string read GetHandlerType;
  end;

  TCRVioHandler = class (TCRVio)
  protected
    FIOHandler: TCRIOHandler;
    FHandle: TCRIOHandle;
    Fhostname: string;
    Fport: integer;
    FTimeout: integer;
    FSSL_key, FSSL_cert, FSSL_ca: string;

    function GetTimeout: integer; override;
    procedure SetTimeout(Value: integer); override;
    procedure SetIsSecure(const Value: Boolean);
    function GetIsSecure: Boolean;
  public
    constructor Create(const hostname: string; const port: integer;
      IOHandler: TCRIOHandler; const SSL_key, SSL_cert, SSL_ca: string);

    procedure Connect; override;
    procedure Close; override;

    function ReadNoWait({$IFDEF CLR}var{performance opt}{$ENDIF} buffer: TValueArr; offset, count: integer): integer; override;
    function WriteNoWait(const{performance opt} buffer: TValueArr; offset, count: integer): integer; override;
    function Read({$IFDEF CLR}var{performance opt}{$ENDIF} buffer: TValueArr; offset, count: integer): integer; override;
    function Write(const{performance opt} buffer: TValueArr; offset, count: integer): integer; override;
    function WaitForData(Timeout: integer = -1): boolean; override;
    property IsSecure: Boolean read GetIsSecure write SetIsSecure;
  end;

  TCRIOHandlerUtils = class
    class procedure RegisterClient(Obj: TCRIOHandler; Client: TObject);
    class procedure UnRegisterClient(Obj: TCRIOHandler; Client: TObject);
  end;

  TProxyOptions = class(TPersistent)
  private
    FHostname: string;
    FPort: integer;
    FUsername: string;
    FPassword: string;
  protected
    procedure AssignTo(Dest: TPersistent); override;
  published
    property Hostname: string read FHostname write FHostname;
    property Port: integer read FPort write FPort default 0;
    property Username: string read FUsername write FUsername;
    property Password: string read FPassword write FPassword;
  end;

  THttpOptions = class(TPersistent)
  private
    FUrl: string;
    FUsername: string;
    FPassword: string;
    FProxyOptions: TProxyOptions;
    procedure SetProxyOptions(Value: TProxyOptions);
  protected
    procedure AssignTo(Dest: TPersistent); override;

  public
    constructor Create;
    destructor Destroy; override;
    function Equals(HttpOptions: THttpOptions): boolean; reintroduce;
  published
    property Url: string read FUrl write FUrl;
    property Username: string read FUsername write FUsername;
    property Password: string read FPassword write FPassword;
    property ProxyOptions: TProxyOptions read FProxyOptions write SetProxyOptions;
  end;

{ $DEFINE VIO_DEBUG}
{ $DEFINE VIO_TIME}

{$IFDEF VIO_TIME}
var                        
  VIO_WriteTime: Int64;
  VIO_ReadTime: Int64;
{$ENDIF}

implementation

uses
{$IFDEF MSWINDOWS}
  Windows,
{$ENDIF}
  Math,
  MemUtils;

procedure TCRVio.Connect;
begin
{$IFDEF BUFFERED_READ}
  FBufferLen := 0;
  FBufferPos := 0;
  SetLength(FBuffer, VIO_READ_BUFFER_SIZE);
{$ENDIF}
end;

function TCRVio.Read({$IFDEF CLR}var{performance opt}{$ENDIF} buffer: TValueArr; offset, count: integer): integer;
{$IFDEF BUFFERED_READ}
  procedure ReadFromBuffer;
  var
    rc: integer;
  begin
    if FBufferPos >= FBufferLen then
      Exit;

    rc := Min(FBufferLen - FBufferPos, count);
  {$IFDEF CLR}
    System.Buffer.BlockCopy(FBuffer, FBufferPos, buffer, offset, rc);
  {$ELSE}
    CopyBuffer(@FBuffer[FBufferPos], @buffer[offset], rc);
  {$ENDIF}
    Inc(offset, rc);
    Inc(Result, rc);
    Dec(count, rc);
    Inc(FBufferPos, rc);
    if FBufferPos = FBufferLen then begin
      FBufferPos := 0;
      FBufferLen := 0;
    end;
  end;
{$ENDIF}

var
{$IFDEF VIO_DEBUG}
  o, c, c1,
{$ENDIF}
  retryCount, waitTimeOut: integer;
  readCount: integer;

{$IFDEF VIO_TIME}
var
  tc1, tc2: Int64;
{$ENDIF}
begin
{$IFDEF VIO_DEBUG}
try
  o := offset;
  c := count;//}
  // OFS('+' + Format(ClassName + '.Read (%d, %d). Self = %s', [o, c, IntToHex(Integer(Self), 8)]));
  OFS('+' + Format(ClassName + '.Read (%d, %d)', [o, c]));
{$ENDIF}
{$IFDEF PERF_COUNTER}
  PerfCounters[1].Start;
{$ENDIF}
{$IFDEF VIO_TIME}
  QueryPerformanceCounter(tc1);
{$ENDIF}

  Result := 0;
{$IFDEF BUFFERED_READ}
  ReadFromBuffer; // read cached data from FBuffer
  if count = 0 then // All data readed from FBuffer
    Exit;

  Assert(FBufferPos = 0);
  if count < VIO_UNBUFFERED_READ_MIN_SIZE then begin
    FBufferLen := ReadNoWait({$IFDEF CLR}FBuffer{$ELSE}@FBuffer[0]{$ENDIF}, 0, VIO_READ_BUFFER_SIZE);
    ReadFromBuffer; // read cached data from FBuffer
  end;
{$ENDIF}

  retryCount := 1;
  waitTimeOut := 10;

  while (count > 0) and (retryCount > 0) do begin
    readCount := ReadNoWait(buffer, offset, count);

    Inc(Result, readCount);
    Inc(offset, readCount);
    Dec(count, readCount);
    if readCount = 0 then begin
      Dec(retryCount);
      if count > 0 then
        Sleep(waitTimeOut);
    end
    else
      retryCount := 1;
  end;

{$IFDEF VIO_TIME}
  QueryPerformanceCounter(tc2);
  VIO_ReadTime := VIO_ReadTime + tc2 - tc1;
{$ENDIF}
{$IFDEF PERF_COUNTER}
  PerfCounters[1].Stop;
{$ENDIF}
{$IFDEF VIO_DEBUG}
finally
  OFS('-' + Format(ClassName + '.Read (%d, %d) = %d', [o, c, Result]));
  c1 := Result;
  if c1 > 200 then
    c1 := 200;
  OFS(Copy(buffer, o, c1));
end;
{$ENDIF}
end;

function TCRVio.Write(const buffer: TValueArr; offset, count: integer): integer;
var
  writeCount: integer;
  retryCount, waitTimeOut: integer;
  retry: integer;
{$IFDEF VIO_DEBUG}
  o, c, c1: integer;
{$ENDIF}
{$IFDEF VIO_TIME}
var
  tc1, tc2: Int64;
{$ENDIF}
begin
{$IFDEF VIO_DEBUG}
  try
    o := offset;
    c := count;
    //OFS('+' + Format(ClassName + '.Write (%d, %d). Self = %s', [o, c, IntToHex(Integer(Self), 8)]));
    OFS('+' + Format(ClassName + '.Write (%d, %d)', [o, c]));
    c1 := c;
    if c > 200 then
      c1 := 200;
  {$IFDEF CLR}
    OFS(Copy(buffer, o, c1));
  {$ELSE}
    OFS(PAnsiChar(buffer) + o, c1);
  {$ENDIF}
{$ENDIF}
{$IFDEF VIO_TIME}
  QueryPerformanceCounter(tc1);
{$ENDIF}
{$IFDEF PERF_COUNTER}
  PerfCounters[1].Start;
{$ENDIF}

  retryCount := 3;
  waitTimeOut := 10;
  Result := 0;

  retry := 0;
  while count > 0 do begin
    writeCount := WriteNoWait(buffer, offset, count);
    if writeCount = 0 then begin
      Inc(retry);
      if retry = retryCount then
        Exit;
      Sleep(waitTimeOut) // silent error handling
    end
    else
    begin
      Inc(Result, writeCount);
      Inc(offset, writeCount);
      Dec(count, writeCount);
    end;
  end;

{$IFDEF VIO_TIME}
  QueryPerformanceCounter(tc2);
  VIO_WriteTime := VIO_WriteTime + tc2 - tc1;
{$ENDIF}
{$IFDEF PERF_COUNTER}
  PerfCounters[1].Stop;
{$ENDIF}
{$IFDEF VIO_DEBUG}
  finally
    OFS('-' + Format(ClassName + '.Write (%d, %d) = %d', [o, c, Result]));
  end;
{$ENDIF}
end;

function TCRVio.WaitForData(Timeout: integer = -1): boolean;
begin
  Assert(False);
  Result := False;
end;

{ TCRIOHandler }

{$IFNDEF CLR}class{$ENDIF} procedure TCRIOHandler.Disconnect(Handle: TCRIOHandle);
begin
  Assert(False, 'Not overriden in an inherited class');
end;

{$IFNDEF CLR}class{$ENDIF} function TCRIOHandler.ReadNoWait(Handle: TCRIOHandle; {$IFDEF CLR}var{performance opt}{$ENDIF} buffer: TValueArr; offset, count: integer): integer;
begin
  Assert(False, 'Not overriden in an inherited class');
  Result := 0;
end;


{$IFNDEF CLR}class{$ENDIF} function TCRIOHandler.Read(Handle: TCRIOHandle; {$IFDEF CLR}var{performance opt}{$ENDIF} buffer: TValueArr; offset, count: integer): integer;
begin
  Assert(False, 'Not overriden in an inherited class');
  Result := 0;
end;

{$IFNDEF CLR}class{$ENDIF} function TCRIOHandler.Write(Handle: TCRIOHandle; const buffer: TValueArr; offset, count: integer): integer;
begin
  Assert(False, 'Not overriden in an inherited class');
  Result := 0;
end;

{$IFNDEF CLR}class{$ENDIF} function TCRIOHandler.WaitForData(Handle: TCRIOHandle; Timeout: integer = -1): boolean;
begin
  Assert(False, 'Not overriden in an inherited class');
  Result := False;
end;

{$IFNDEF CLR}class{$ENDIF} function TCRIOHandler.GetTimeout(Handle: TCRIOHandle): integer;
begin
  Assert(False, 'Not overriden in an inherited class');
  Result := 0;
end;

{$IFNDEF CLR}class{$ENDIF} procedure TCRIOHandler.SetTimeout(Handle: TCRIOHandle; Value: integer);
begin
  Assert(False, 'Not overriden in an inherited class');
end;

procedure TCRIOHandler.RegisterClient(Client: TObject);
begin
end;

procedure TCRIOHandler.UnRegisterClient(Client: TObject);
begin
end;

{$IFNDEF CLR}class{$ENDIF} procedure TCRIOHandler.SetIsSecure(Handle: TCRIOHandle; const Value: Boolean);
begin
end;

{$IFNDEF CLR}class{$ENDIF} function TCRIOHandler.GetIsSecure(Handle: TCRIOHandle): Boolean;
begin
  Result := False;
end;

function TCRIOHandler.GetHandlerType: string;
begin
  Result := '';
end;

{ TCRVioHandler }

constructor TCRVioHandler.Create(const hostname: string; const port: integer;
  IOHandler: TCRIOHandler; const SSL_key, SSL_cert, SSL_ca: string);
begin
  inherited Create;
  Assert(IOHandler <> nil);
  Fhostname := hostname;
  Fport := port;
  FSSL_key := SSL_key;
  FSSL_cert := SSL_cert;
  FSSL_ca := SSL_ca;
  FIOHandler := IOHandler;
  Assert(FIOHandler <> nil);
end;

function TCRVioHandler.GetTimeout: integer;
begin
  if FHandle = nil then
    Result := FTimeout
  else
    Result := FIOHandler.GetTimeout(FHandle);
end;

procedure TCRVioHandler.SetTimeout(Value: integer);
begin
  if FHandle = nil then
    FTimeout := Value
  else
    FIOHandler.SetTimeout(FHandle, Value);
end;

function TCRVioHandler.ReadNoWait({$IFDEF CLR}var{performance opt}{$ENDIF} buffer: TValueArr; offset, count: integer): integer;
begin
  if FHandle = nil then // To avoid assertion on closing connection
    Result := 0
  else
    Result := FIOHandler.ReadNoWait(FHandle, buffer, offset, count);
end;

function TCRVioHandler.WriteNoWait(const{performance opt} buffer: TValueArr; offset, count: integer): integer;
begin
  Assert(False, 'Not implemented');
  Result := 0;
end;

procedure TCRVioHandler.Connect;
begin
  inherited;
  Assert(FHandle = nil);
  FHandle := FIOHandler.Connect(Fhostname, Fport, FSSL_key, FSSL_cert, FSSL_ca);
  if FTimeout > 0 then
    FIOHandler.SetTimeout(FHandle, FTimeout);
end;

procedure TCRVioHandler.Close;
begin
  if FHandle = nil then // To avoid assertion on closing connection
    Exit;
  FIOHandler.Disconnect(FHandle);
  FHandle := nil;
end;

function TCRVioHandler.Read({$IFDEF CLR}var{performance opt}{$ENDIF} buffer: TValueArr; offset,
  count: integer): integer;
begin
  if FHandle = nil then // To avoid assertion on closing connection
    Result := 0
  else
    Result := inherited Read(buffer, offset, count);
end;

function TCRVioHandler.Write(const buffer: TValueArr; offset,
  count: integer): integer;
begin
  if FHandle = nil then // To avoid assertion on closing connection
    Result := 0
  else
    Result := FIOHandler.Write(FHandle, buffer, offset, count);
end;

function TCRVioHandler.WaitForData(Timeout: integer = -1): boolean;
begin
  if FHandle = nil then // To avoid assertion on closing connection
    Result := False
  else
    Result := FIOHandler.WaitForData(FHandle, Timeout);
end;

procedure TCRVioHandler.SetIsSecure(const Value: Boolean);
begin
  if FHandle <> nil then
    FIOHandler.SetIsSecure(FHandle, Value);
end;

function TCRVioHandler.GetIsSecure: Boolean;
begin
  if FHandle = nil then
    Result := False
  else
    Result := FIOHandler.GetIsSecure(FHandle);
end;

{ TCRIOHandlerUtils }

class procedure TCRIOHandlerUtils.RegisterClient(Obj: TCRIOHandler; Client: TObject);
begin
  Obj.RegisterClient(Client);
end;

class procedure TCRIOHandlerUtils.UnRegisterClient(Obj: TCRIOHandler; Client: TObject);
begin
  Obj.UnRegisterClient(Client);
end;

{ TProxyOptions }

procedure TProxyOptions.AssignTo(Dest: TPersistent);
begin
  if Dest is TProxyOptions then
  begin
    TProxyOptions(Dest).FHostname := FHostname;
    TProxyOptions(Dest).FPort := FPort;
    TProxyOptions(Dest).FUsername := FUsername;
    TProxyOptions(Dest).FPassword := FPassword;
  end
  else
    inherited;
end;

{ THttpOptions }

constructor THttpOptions.Create;
begin
  inherited;
  FProxyOptions := TProxyOptions.Create;
end;

destructor THttpOptions.Destroy;
begin
  FProxyOptions.Free;
  inherited;
end;

procedure THttpOptions.AssignTo(Dest: TPersistent);
begin
  if Dest is THttpOptions then
  begin
    THttpOptions(Dest).FUrl := FUrl;
    THttpOptions(Dest).FUsername := FUsername;
    THttpOptions(Dest).FPassword := FPassword;
    THttpOptions(Dest).FProxyOptions.Assign(FProxyOptions);
  end
  else
    inherited;
end;

procedure THttpOptions.SetProxyOptions(Value: TProxyOptions);
begin
  FProxyOptions.Assign(Value);
end;

function THttpOptions.Equals(HttpOptions: THttpOptions): boolean;
begin
  Result := False;
  if HttpOptions <> nil then
    Result :=
      (Url = HttpOptions.Url) and
      (Username = HttpOptions.Username) and
      (Password = HttpOptions.Password) and
      (ProxyOptions.Hostname = HttpOptions.ProxyOptions.Hostname) and
      (ProxyOptions.Port = HttpOptions.ProxyOptions.Port) and
      (ProxyOptions.Username = HttpOptions.ProxyOptions.Username) and
      (ProxyOptions.Password = HttpOptions.ProxyOptions.Password);
end;

end.
