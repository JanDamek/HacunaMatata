//////////////////////////////////////////////////
//  Copyright © 1998-2012 Devart. All right reserved.
//////////////////////////////////////////////////

{$IFNDEF CLR}
{$I Dac.inc}
unit CRVioHttp;
{$ENDIF}

interface

uses
  Classes, SysUtils, SyncObjs,
{$IFDEF MSWINDOWS}
  Win32Timer,
{$ENDIF}
{$IFDEF CLR}
  System.Text,
{$ELSE}
  CLRClasses,
{$ENDIF}
  CRTypes, CRVio, CRHttp, CRBase64;

type
  TCRVioHttp = class(TCRVio)
  private
    FHttpOptions: THttpOptions;
    FUrl: string;
    FTestUrl: string;
    FPortID: Integer;
    FClosed: Boolean;
    FConnectRequest: TCRHttp;

    FConnectEvent: TEvent;
    FLastReadRequest: TCRHttp;
    FLastReadBuffer: TBytes;
    FTimeout: Integer;
    FScriptNotificationTime: Integer;
    FSendingLock: TCriticalSection;
    FThreadException: Exception;
    FErrorMessageBuffer: TBytes;

  {$IFDEF MSWINDOWS}
    FNotificationTimer: TWin32Timer;
    procedure OnScriptNotification(Sender: TObject);
  {$ENDIF}
    procedure StartNotificationTimer;
    procedure StartHttpServerScript;
    procedure AbortConnectionScript;
    function CreateRequest(const Url: string; const Command: Char; const Method: string): TCRHttp;
    procedure CloseRequest(var Request: TCRHttp; IsConnect: Boolean = False);
    procedure CheckConnectResponse(Request: TCRHttp);
    procedure CheckResponseSuccess(Request: TCRHttp);
    procedure CheckThreadException;

    function ExecuteGetRequest(const Url: string; const Command: Char): TCRHttp; overload;
    function ExecuteGetRequest(const Url: string; const Command: Char;
      CheckLeaseException: Boolean): TCRHttp; overload;

  protected
    function GetTimeout: integer; override;
    procedure SetTimeout(Value: integer); override;

  public
    constructor Create(HttpOptions: THttpOptions; const hostname: AnsiString; const port: integer);
    destructor Destroy; override;

    procedure Connect; override;
    procedure Close; override;

    function ReadNoWait({$IFDEF CLR}var{performance opt}{$ENDIF} buffer: TValueArr; offset, count: integer): integer; override;
    function WriteNoWait(const{performance opt} buffer: TValueArr; offset, count: integer): integer; override;
    function Write(const buffer: TValueArr; offset, count: integer): integer; override;
  {$IFDEF MSWINDOWS}
    function WaitForData(Timeout: integer = -1): boolean; override;
  {$ENDIF}
  end;

implementation

uses
  CRVioTcp;

type
  ScriptCommands = (scClose, scRead, scWrite, scLease, scConnect, scTest);
  ScriptResponseFlags = (rfSuccess, rfError);

const
  SScriptCommands: array[ScriptCommands] of Char = ('x', 'r', 'w', 'l', 'c', 't');
  SScriptResponseFlags: array[ScriptResponseFlags] of string = ('OK:', 'ER:');

type
  TThreadMethod = procedure of object;

  THttpConnectThread = class(TThread)
  protected
    FExecuteMethod: TThreadMethod;
    FAbortMethod: TThreadMethod;
    procedure Execute; override;
  public
    constructor Create(ExecuteMethod, AbortMethod: TThreadMethod);
    destructor Destroy; override;
  end;

{ THttpConnectThread }

constructor THttpConnectThread.Create(ExecuteMethod, AbortMethod: TThreadMethod);
begin
  inherited Create(True);

  FExecuteMethod := ExecuteMethod;
  FAbortMethod := AbortMethod;
  Resume;
end;

destructor THttpConnectThread.Destroy;
begin
  try
    if Assigned(FAbortMethod) then
      FAbortMethod();
  except
  end;

  inherited;
end;

procedure THttpConnectThread.Execute;
begin
  try
    if Assigned(FExecuteMethod) then
      FExecuteMethod();
  except
  end;
end;

{ TCRVioHttp }

constructor TCRVioHttp.Create(HttpOptions: THttpOptions;
  const hostname: AnsiString; const port: integer);
var
  ConnectionId: string;
  ConnectUrl: string;
begin
  inherited Create;

  FHttpOptions := THttpOptions.Create;
  FHttpOptions.Assign(HttpOptions);

  Randomize;
  ConnectionId := IntToStr(Random(1000000)) + '_' + IntToStr(Random(1000000));

  FUrl := Trim(FHttpOptions.Url);
  if Copy(FUrl, Length(FUrl), 1) <> '/' then
    FUrl := FUrl + '/';

  ConnectUrl := Format(FUrl + '?a=c&s=%s&p=%d&id=%s', [hostname, port, ConnectionId]);
  FTestUrl := Format(FUrl + '?a=t&s=%s&p=%d&id=%s', [hostname, port, ConnectionId]);
  FPortID := 0;

  FConnectRequest := CreateRequest(ConnectUrl, SScriptCommands[scConnect], 'GET');
  FConnectEvent := TEvent.Create(nil, True, False, '');
  SetLength(FErrorMessageBuffer, BUF_SIZE);
  SetLength(FLastReadBuffer, 1);
  FTimeout := 15;
  FScriptNotificationTime := 0;
  FSendingLock := TCriticalSection.Create;
  FThreadException := nil;
  FLastReadRequest := nil;
  FClosed := True;
end;

destructor TCRVioHttp.Destroy;
begin
  Close;
  FSendingLock.Free;
  FHttpOptions.Free;
  FConnectEvent.Free;
  FConnectRequest.Free;

  inherited;
end;

function TCRVioHttp.GetTimeout: integer;
begin
  Result := FTimeout;
end;

procedure TCRVioHttp.SetTimeout(Value: integer);
begin
  FTimeout := Value;
end;

procedure TCRVioHttp.Close;
var
  Request: TCRHttp;
begin
  if FClosed then
    Exit;

  FClosed := True;
  try
    Request := ExecuteGetRequest(FUrl, SScriptCommands[scClose], False);
    CloseRequest(Request);
  except
  end;

  FreeAndNil(FThreadException);
{$IFDEF MSWINDOWS}
  FreeAndNil(FNotificationTimer);
{$ENDIF}
  FreeAndNil(FLastReadRequest);
end;

procedure TCRVioHttp.Connect;
var
  Request: TCRHttp;
  HttpConnectThread: THttpConnectThread;
  Connected, ConnectFinished: boolean;
  RetryCount: Integer;
  ConnectTimeout: Integer;
begin
  inherited;

  FLastError := '';

  try
    RetryCount := 0;
    ConnectTimeout := 200;

    repeat
      ConnectFinished := False;
      Connected := False;
      FConnectEvent.ResetEvent;

      HttpConnectThread := THttpConnectThread.Create(StartHttpServerScript, AbortConnectionScript);
      try
        // test whether connnected script responds
        while not ConnectFinished and not Connected and (ConnectTimeout < FTimeout * 1000) do begin
          ConnectFinished := FConnectEvent.WaitFor(ConnectTimeout) = wrSignaled;
          try
            Request := ExecuteGetRequest(FTestUrl, SScriptCommands[scTest]);
            try
              CheckConnectResponse(Request);
              Connected := True;
            finally
              CloseRequest(Request);
            end;
          except
            Connected := False;
          end;
          ConnectTimeout := ConnectTimeout * 2;
        end;
      finally
        HttpConnectThread.Free;
      end;

      Inc(RetryCount);
    until Connected or (RetryCount >= 3);

  except
    on E: Exception do begin
      FLastError := E.Message;
      raise;
    end;
  end;

  if not Connected then begin
    FLastError := 'Cannot establish HTTP connection';
    raise HttpException.Create(FLastError);
  end;

  FClosed := False;
  StartNotificationTimer;
end;

procedure TCRVioHttp.StartHttpServerScript;
begin
  try
    FConnectRequest.Timeout := FTimeout;
    FConnectRequest.Get;
  finally
    FConnectRequest.Disconnect;
  end;

  FConnectEvent.SetEvent;
end;

procedure TCRVioHttp.AbortConnectionScript;
begin
  FConnectRequest.Abort;
end;

function TCRVioHttp.CreateRequest(const Url: string; const Command: Char; const Method: string): TCRHttp;
var
  CommandStr: string;
begin
  if (Command = SScriptCommands[scTest]) or (Command = SScriptCommands[scConnect]) then
    CommandStr := ''
  else
    CommandStr := '?a=' + Command;

  Result := TCRHttp.Create(Url + CommandStr + '&port=' + IntToStr(FPortID));
  try
    Result.KeepAlive := True;
    Result.Timeout := FTimeout;

    Result.HeaderInfo.UserId := FHttpOptions.Username;
    Result.HeaderInfo.Password := FHttpOptions.Password;
    Result.Proxy := FHttpOptions.ProxyOptions.Hostname;
    Result.ProxyPort := FHttpOptions.ProxyOptions.Port;
    Result.HeaderInfo.ProxyUserId := FHttpOptions.ProxyOptions.Username;
    Result.HeaderInfo.ProxyPassword := FHttpOptions.ProxyOptions.Password;
  except
    Result.Free;
    raise;
  end;
end;

procedure TCRVioHttp.CloseRequest(var Request: TCRHttp; IsConnect: Boolean = False);
begin
  FreeAndNil(Request);
end;

procedure TCRVioHttp.StartNotificationTimer;
begin
{$IFDEF MSWINDOWS}
  if FNotificationTimer = nil then begin
    FNotificationTimer := TWin32Timer.Create(nil);
    FNotificationTimer.OnTimer := OnScriptNotification;
    FNotificationTimer.Interval := FScriptNotificationTime;
  end;

  FNotificationTimer.Enabled := False;
  FNotificationTimer.Enabled := True;
{$ENDIF}
end;

{$IFDEF MSWINDOWS}
procedure TCRVioHttp.OnScriptNotification(Sender: TObject);
var
  Request: TCRHttp;
begin
  FNotificationTimer.Enabled := False;

  FSendingLock.Enter;
  try
    try
      // ExecuteGetRequest without internal lock
      Request := CreateRequest(FUrl, SScriptCommands[scLease], 'GET');
      try
        Request.Get;
        CheckResponseSuccess(Request);
      finally
        CloseRequest(Request);
      end;

      StartNotificationTimer;
    except
    {$IFNDEF CLR}
      on E: SocketException do
        FThreadException := SocketException.Create(E.ErrorCode);
    {$ENDIF}
      on E: HttpException do
        FThreadException := HttpException.Create(E.Message);
      on E: Exception do
        FThreadException := Exception.Create(E.Message);
    end;

  finally
    FSendingLock.Leave;
  end;
end;
{$ENDIF}

procedure TCRVioHttp.CheckConnectResponse(Request: TCRHttp);

{$IFNDEF VER6P}
  function TryStrToInt(const S: string; out Value: Integer): Boolean;
  var
    E: Integer;
  begin
    Val(S, Value, E);
    Result := E = 0;
  end;
{$ENDIF}

var
  Line: string;
  IsValid: Boolean;
  RequestTime: Integer;
begin
  Line := Trim(Request.ReadLine);
  IsValid := TryStrToInt(Line, FPortID);
  Line := Trim(Request.ReadLine); // read lifetime

  if IsValid then begin
    IsValid := TryStrToInt(Line, FScriptNotificationTime);

    if IsValid then begin
      if FTimeout > 0 then
        RequestTime := FTimeout
      else
        RequestTime := 7;

      // consider maximum request transfer time, notify script before lifetime expires
      if FScriptNotificationTime > 2 * RequestTime then
        FScriptNotificationTime := FScriptNotificationTime - RequestTime
      else
        FScriptNotificationTime := FScriptNotificationTime div 2;
      FScriptNotificationTime := FScriptNotificationTime * 1000;
    end;
  end;

  if not IsValid then
    raise HttpException.Create('HTTP Script invalid connect response');
end;

procedure TCRVioHttp.CheckResponseSuccess(Request: TCRHttp);
var
  Flag, AdditionalMessage: string;
  cnt: Integer;
begin
  Assert(Request <> nil);

  try
    cnt := Request.Read(FErrorMessageBuffer, 0, Length(SScriptResponseFlags[rfSuccess]));
    if cnt < Length(SScriptResponseFlags[rfSuccess]) then
      raise HttpException.Create('Invalid script response. Failed to read success flag');

    Flag := string(Encoding.Default.GetString(FErrorMessageBuffer, 0, cnt));
    if AnsiCompareText(Flag, SScriptResponseFlags[rfSuccess]) = 0 then
      Exit;

    // Error
    if AnsiCompareText(Flag, SScriptResponseFlags[rfError]) = 0 then
      AdditionalMessage := ''
    else
      AdditionalMessage := 'HTTP Script unknown error: ' + Flag;

    // read error description
    cnt := Request.Read(FErrorMessageBuffer, 0, Length(FErrorMessageBuffer));
    AdditionalMessage := AdditionalMessage + string(Encoding.Default.GetString(FErrorMessageBuffer, 0, cnt));

    if Length(AdditionalMessage) > 0 then
      raise HttpException.Create(AdditionalMessage)
    else
      raise HttpException.Create('HTTP Script reported error');
  except
    try
      Close;
    except
    end;

    raise;
  end;
end;

procedure TCRVioHttp.CheckThreadException;
var
  Ex: Exception;
begin
  if FThreadException <> nil then begin
    Ex := FThreadException;
    FThreadException := nil;
    raise Ex;
  end;
end;

function TCRVioHttp.ExecuteGetRequest(const Url: string; const Command: Char): TCRHttp;
begin
  Result := ExecuteGetRequest(Url, Command, True);
end;

function TCRVioHttp.ExecuteGetRequest(const Url: string; const Command: Char;
  CheckLeaseException: Boolean): TCRHttp;
begin
  Result := nil;

  try
    FSendingLock.Enter;
    try
      if CheckLeaseException then
        CheckThreadException;

      Result := CreateRequest(Url, Command, 'GET');
      Result.Get;
    finally
      FSendingLock.Leave;
    end;

    CheckResponseSuccess(Result);
  except
    Result.Free;
    raise;
  end;
end;

function TCRVioHttp.WriteNoWait(const{performance opt} buffer: TValueArr; offset, count: integer): integer;

  procedure SendPostRequest(buffer: TValueArr; offset, count: integer);
  var
    Request: TCRHttp;
  begin
    FSendingLock.Enter;
    try
      CheckThreadException;

      Request := CreateRequest(FUrl, SScriptCommands[scWrite], 'POST');
      try
        Request.WriteData.Write(TBytes(buffer), offset, count);
        Request.Post;
        CheckResponseSuccess(Request);
        StartNotificationTimer;
      finally
        CloseRequest(Request);
      end;
    finally
      FSendingLock.Leave;
    end;
  end;

var
  writeCount: Integer;
begin
  Result := 0;
  if FClosed then
    Exit;

  while count > 0 do begin
    if count > 2 * 1024 * 1024 then
      writeCount := 2 * 1024 * 1024
    else
      writeCount := count;

    try
      FLastError := '';
      SendPostRequest(buffer, offset, writeCount);
      Inc(Result, writeCount);
      Inc(offset, writeCount);
      Dec(count, writeCount);
    except
      on E: Exception do begin
        FLastError := E.Message;
        exit;
      end;
    end
  end;
end;

function TCRVioHttp.Write(const buffer: TValueArr; offset, count: integer): integer;
begin
  Result := WriteNoWait(buffer, offset, count);
end;

function TCRVioHttp.ReadNoWait({$IFDEF CLR}var{performance opt}{$ENDIF} buffer: TValueArr; offset, count: integer): integer;
var
  Request: TCRHttp;
  cnt: Integer;
begin
  Result := 0;
  if FClosed or (count = 0) then
    Exit;

  Request := nil;
  FLastError := '';
  try
    try
      if FLastReadRequest <> nil then begin
        TBytes(buffer)[offset] := FLastReadBuffer[0];
        Inc(offset);
        Dec(count);
        Inc(Result);
        Request := FLastReadRequest;
        FLastReadRequest := nil;
      end
      else begin
        Request := ExecuteGetRequest(FUrl, SScriptCommands[scRead]);
        StartNotificationTimer;
      end;

      cnt := Request.Read(TBytes(buffer), offset, count);
      Result := Result + cnt;

      if cnt = count then begin
        if Request.WaitForData(0) then
          cnt := Request.Read(FLastReadBuffer, 0, 1)
        else
          cnt := 0;
        if cnt = 1 then
          FLastReadRequest := Request;
      end;

      if FLastReadRequest <> nil then
        Request := nil;
    finally
      CloseRequest(Request);
    end;

  except
    on E: Exception do
      FLastError := E.Message;
  end
end;

{$IFDEF MSWINDOWS}
function TCRVioHttp.WaitForData(Timeout: integer = -1): boolean;
begin
  Result := False;
  if FLastReadRequest <> nil then
    Result := FLastReadRequest.WaitForData(Timeout);
end;
{$ENDIF}

end.
