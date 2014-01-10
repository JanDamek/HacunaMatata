{$IFNDEF CLR}
{$IFNDEF FPCHEADER}
unit DBMonitorMessages;
{$ENDIF}
{$ENDIF}

{$IFDEF WIN32}
  {$DEFINE MSWINDOWS}
  {$DEFINE WIN32_64}
{$ENDIF}
{$IFDEF WIN64}
  {$DEFINE MSWINDOWS}
  {$DEFINE WIN32_64}
{$ENDIF}
{$IFDEF CLR}
  {$DEFINE MSWINDOWS}
{$ENDIF}

interface

uses
  Classes, SysUtils, Math,
{$IFDEF MSWINDOWS}
  Windows,
{$ENDIF}
{$IFDEF LINUX}
unix,
{$ENDIF}
{$IFDEF VER6P}
  Types,
{$ENDIF}
{$IFDEF CLR}
  System.Net.Sockets, System.Text,
{$ENDIF}
  CRTypes, CRVioTcp;

const
  MT_EVENT = 1;
  MT_STARTUP = 2;
  MT_EVENTSTART = 4;
  MT_EVENTEND = 5;
  MT_PING = 6;

  DBMonitorPort = 1000;
  DefaultReconnectTimeout = 5000;
  DefaultSendTimeout = 1000;

type
{$IFNDEF CLR}
{$IFNDEF VER11P}
  TBytes = array of byte;
{$ENDIF}  
{$ENDIF}
{$IFNDEF VER6P}
  TWideStringDynArray = array of WideString;
{$ENDIF}

  TMessagePacker = class
  public
    procedure WriteByte(Value: byte); virtual; abstract;
    procedure WriteInteger(Value: integer); virtual; abstract;
    procedure WriteString(const Value: WideString); virtual; abstract;
  end;

  TMessageUnPacker = class
  public
    procedure ReadByte(out Value: byte); virtual; abstract;
    procedure ReadInteger(out Value: integer); virtual; abstract;
    procedure ReadCardinal(out Value: cardinal); virtual; abstract;
    procedure ReadString(out Value: WideString); virtual; abstract;
  end;

  TMonitorMessage = class
  public
    MessageType: integer;
    ProcessId: integer;

    procedure Write(Packer: TMessagePacker); virtual;
    procedure Read(Packer: TMessageUnPacker); virtual;
  end;

  TEventMessage = class(TMonitorMessage)
  public
    EventType: integer;
    Description: WideString;
    // filled at server
    TimeStamp: TDateTime;
    Index: integer;

    constructor Create;
    procedure Write(Packer: TMessagePacker); override;
    procedure Read(Packer: TMessageUnPacker); override;
  end;

  TStartupMessage = class(TEventMessage)
  public
    ExeName: WideString;
    Host: WideString;

    constructor Create;
    procedure Write(Packer: TMessagePacker); override;
    procedure Read(Packer: TMessageUnPacker); override;
  end;

  //TFinishMessage = class(TEventMessage)

  TMsgSQLParam = record
    Name: WideString;
    DataType: WideString;
    ParamType: WideString;
    Value: WideString;
  end;

  TMsgSQLParams = array of TMsgSQLParam;

  TEventSender = class
  public
    ID: integer;
    Deleted: boolean;
    Name: WideString;
    ObjectType: integer;
    TypeName: WideString;
    Parent: TEventSender;
    Highlighted: boolean;
  end;

{$HPPEMIT 'class TCallStackItem;'}

  TCallStackItem = class
  private
    FParent: TCallStackItem;
    procedure SetParent(Value: TCallStackItem);
  public
    Name: WideString;
    Childs: array of TCallStackItem;
    Excluded: boolean;
    Shortened: boolean;
    Highlighted: boolean;
    property Parent: TCallStackItem read FParent write SetParent;

    destructor Destroy; override;
    function ExcludedFromTree: boolean;
    function ParentNotExcluded: TCallStackItem;
    function ParentNotExcludedFromTree: TCallStackItem;
  end;

  TEventEndMessage = class;

  TEventStartMessage = class(TEventMessage)
  public
    EventID: integer;
    StartTime: cardinal;
    ObjectID: integer;
    ObjectName: WideString;
    ObjectType: integer;
    ObjectTypeName: WideString;
    ParentID: integer;
    ParentName: WideString;
    ParentType: integer;
    ParentTypeName: WideString;
    SQL: WideString;
    Params: TMsgSQLParams;
    CallStack: TWideStringDynArray;
    // filled at server
    SenderObject: TEventSender;
    EndEvent: TEventEndMessage;
    CallStackItem: TCallStackItem;

    constructor Create;
    procedure Write(Packer: TMessagePacker); override;
    procedure Read(Packer: TMessageUnPacker); override;
  end;

  TEventEndMessage = class(TEventMessage)
  public
    EventID: integer;
    EndTime: cardinal;
    Failed: integer;
    ErrorText: WideString;
    Params: TMsgSQLParams;
    RowsAffected: integer;

    constructor Create;
    procedure Write(Packer: TMessagePacker); override;
    procedure Read(Packer: TMessageUnPacker); override;
  end;

  TSocketMessagePacker = class(TMessagePacker)
  private
    FHost: string;
    FPort: integer;
    FReconnectTimeout: cardinal;
    FSendTimeout: cardinal;
    FCRVioTcp: TCRVioTcp;
    FLastConnectTime: cardinal;
    FLastConnectFailed: boolean;
    FBuffer: TBytes;
    FBufferOffset: Integer;
  public
    constructor Create;
    destructor Destroy; override;
    function Open: boolean;
    procedure Close;
    function IsActive: boolean;
    function CheckActive: boolean;
    procedure ClearBuffer;
    procedure CheckAndRealloc(ACount: Integer);
    procedure Flush;
    procedure WriteMessage(Msg: TMonitorMessage);
    procedure WriteByte(Value: byte); override;
    procedure WriteInteger(Value: integer); override;
    procedure WriteString(const Value: WideString); override;

    property Host: string read FHost write FHost;
    property Port: integer read FPort write FPort;
    property ReconnectTimeout: cardinal read FReconnectTimeout write FReconnectTimeout;
    property SendTimeout: cardinal read FSendTimeout write FSendTimeout;
  end;

{$IFDEF LINUX}
function GetTickCount: Cardinal;
{$ENDIF}
function GetTickInterval(StartTickCount, FinishTickCount: Cardinal): Cardinal;

implementation

uses
  CRFunctions;

const
  BUF_SIZE = 1024;
  MAX_BUF_SIZE = 32768;

{ TMonitorMessage }

procedure TMonitorMessage.Write(Packer: TMessagePacker);
begin
  Packer.WriteInteger(ProcessId);
end;

procedure TMonitorMessage.Read(Packer: TMessageUnPacker);
begin
  Packer.ReadInteger(ProcessId);
end;

{ TEventMessage }

constructor TEventMessage.Create;
begin
  inherited;
  MessageType := MT_EVENT;
end;

procedure TEventMessage.Write(Packer: TMessagePacker);
begin
  inherited;
  Packer.WriteInteger(EventType);
  Packer.WriteString(Description);
end;

procedure TEventMessage.Read(Packer: TMessageUnPacker);
begin
  inherited;
  Packer.ReadInteger(EventType);
  Packer.ReadString(Description);
end;

{ TStartupMessage }

constructor TStartupMessage.Create;
begin
  inherited;
  MessageType := MT_STARTUP;
end;

procedure TStartupMessage.Write(Packer: TMessagePacker);
begin
  inherited;
  Packer.WriteString(ExeName);
end;

procedure TStartupMessage.Read(Packer: TMessageUnPacker);
begin
  inherited;
  Packer.ReadString(ExeName);
end;

{ TCallStackItem }

destructor TCallStackItem.Destroy;
var
  i: integer;
begin
  for i := 0 to Length(Childs) - 1 do
    Childs[i].Free;

  inherited;
end;

function TCallStackItem.ExcludedFromTree: boolean;
begin
  Result := Excluded or Shortened;
end;

function TCallStackItem.ParentNotExcluded: TCallStackItem;
begin
  Result := Parent;
  while (Result <> nil) and Result.Excluded do
    Result := Result.Parent;
end;

function TCallStackItem.ParentNotExcludedFromTree: TCallStackItem;
begin
  Result := Parent;
  while (Result <> nil) and Result.ExcludedFromTree do
    Result := Result.Parent;
end;

procedure TCallStackItem.SetParent(Value: TCallStackItem);
begin
  if Value <> FParent then begin
    Assert(FParent = nil);
    FParent := Value;
    if Value <> nil then begin
      SetLength(FParent.Childs, Length(FParent.Childs) + 1);
      FParent.Childs[Length(FParent.Childs) - 1] := Self;
    end;
  end;
end;

{ TEventStartMessage }

constructor TEventStartMessage.Create;
begin
  inherited;
  MessageType := MT_EVENTSTART;
end;

procedure TEventStartMessage.Write(Packer: TMessagePacker);
var
  i: integer;
begin
  inherited;
  Packer.WriteInteger(EventID);
  Packer.WriteInteger(Integer(StartTime));
  Packer.WriteInteger(ObjectID);
  Packer.WriteString(ObjectName);
  Packer.WriteInteger(ObjectType);
  Packer.WriteString(ObjectTypeName);
  Packer.WriteInteger(ParentID);
  Packer.WriteString(ParentName);
  Packer.WriteInteger(ParentType);
  Packer.WriteString(ParentTypeName);
  Packer.WriteString(SQL);
  Packer.WriteInteger(Length(Params));

  for i := 0 to Length(Params) - 1 do begin
    Packer.WriteString(Params[i].Name);
    Packer.WriteString(Params[i].DataType);
    Packer.WriteString(Params[i].ParamType);
    Packer.WriteString(Params[i].Value);
  end;

  Packer.WriteInteger(Length(CallStack));
  for i := 0 to Length(CallStack) - 1 do
    Packer.WriteString(CallStack[i]);
end;

procedure TEventStartMessage.Read(Packer: TMessageUnPacker);
var
  i, Len: integer;
begin
  inherited;
  Packer.ReadInteger(EventID);
  Packer.ReadCardinal(StartTime);
  Packer.ReadInteger(ObjectID);
  Packer.ReadString(ObjectName);
  Packer.ReadInteger(ObjectType);
  Packer.ReadString(ObjectTypeName);
  Packer.ReadInteger(ParentID);
  Packer.ReadString(ParentName);
  Packer.ReadInteger(ParentType);
  Packer.ReadString(ParentTypeName);
  Packer.ReadString(SQL);

  Packer.ReadInteger(Len);
  SetLength(Params, Len);
  for i := 0 to Len - 1 do begin
    Packer.ReadString(Params[i].Name);
    Packer.ReadString(Params[i].DataType);
    Packer.ReadString(Params[i].ParamType);
    Packer.ReadString(Params[i].Value);
  end;

  Packer.ReadInteger(Len);
  SetLength(CallStack, Len);
  for i := 0 to Len - 1 do
    Packer.ReadString(CallStack[i]);
end;

{ TEventEndMessage }

constructor TEventEndMessage.Create;
begin
  inherited;
  MessageType := MT_EVENTEND;
end;

procedure TEventEndMessage.Write(Packer: TMessagePacker);
var
  i: integer;
begin
  inherited;
  Packer.WriteInteger(EventID);
  Packer.WriteInteger(Integer(EndTime));
  Packer.WriteInteger(Failed);
  Packer.WriteString(ErrorText);
  Packer.WriteInteger(Length(Params));

  for i := 0 to Length(Params) - 1 do begin
    Packer.WriteString(Params[i].Name);
    Packer.WriteString(Params[i].DataType);
    Packer.WriteString(Params[i].ParamType);
    Packer.WriteString(Params[i].Value);
  end;

  Packer.WriteInteger(RowsAffected);
end;

procedure TEventEndMessage.Read(Packer: TMessageUnPacker);
var
  i, Len: integer;
begin
  inherited;
  Packer.ReadInteger(EventID);
  Packer.ReadCardinal(EndTime);
  Packer.ReadInteger(Failed);
  Packer.ReadString(ErrorText);

  Packer.ReadInteger(Len);
  SetLength(Params, Len);
  for i := 0 to Len - 1 do begin
    Packer.ReadString(Params[i].Name);
    Packer.ReadString(Params[i].DataType);
    Packer.ReadString(Params[i].ParamType);
    Packer.ReadString(Params[i].Value);
  end;

  Packer.ReadInteger(RowsAffected);
end;

{ TSocketMessagePacker }

constructor TSocketMessagePacker.Create;
begin
  inherited Create;

  FReconnectTimeout := DefaultReconnectTimeout;
  FSendTimeout := DefaultSendTimeout;
  SetLength(FBuffer, BUF_SIZE);
  FCRVioTcp := TCRVioTcp.Create(AnsiString(FHost), FPort);
end;

destructor TSocketMessagePacker.Destroy;
begin
  FCRVioTcp.Close;
  FCRVioTcp.Free;
  inherited;
end;

function TSocketMessagePacker.Open: boolean;
begin
  // using GetTickInterval for avoiding the out of range error
  if FLastConnectFailed and (GetTickInterval(FLastConnectTime, GetTickCount) < FReconnectTimeout) then begin
    Result := False;
    exit;
  end;

  if FHost = '' then
    FHost := 'localhost';

  if FPort = 0 then
    FPort := DBMonitorPort;

  FCRVioTcp.Host := AnsiString(FHost);
  FCRVioTcp.Port := FPort;

  try
    FCRVioTcp.Connect;
    FCRVioTcp.SendTimeout := FSendTimeout div 1000;
    FLastConnectFailed := False;
    Result := True;
  except
    on SocketException do begin
      FCRVioTcp.Close;
      FLastConnectFailed := True;
      FLastConnectTime := GetTickCount;
      Result := False;
    end;
  end;
end;

procedure TSocketMessagePacker.Close;
begin
  FCRVioTcp.Close;
end;

function TSocketMessagePacker.IsActive: boolean;
begin
  Result := FCRVioTcp.IsActive;
end;

function TSocketMessagePacker.CheckActive: boolean;
begin
  if not IsActive then
    Result := False
  else begin
    try
      ClearBuffer;
      WriteInteger(MT_PING);
      Flush;
    except
      on SocketException do
        Close;
    end;
    Result := IsActive;
  end;
end;

procedure TSocketMessagePacker.ClearBuffer;
begin
  FBufferOffset := 0;
end;

procedure TSocketMessagePacker.CheckAndRealloc(ACount: Integer);
begin
  if ACount > Length(FBuffer) - FBufferOffset then begin
    if Length(FBuffer) < MAX_BUF_SIZE then
      SetLength(FBuffer, Min(Max(FBufferOffset + ACount, Length(FBuffer) * 2), MAX_BUF_SIZE));

    if ACount > Length(FBuffer) - FBufferOffset then
      Flush;
  end;
end;

procedure TSocketMessagePacker.Flush;
var
  cnt: Integer;
begin
  cnt := FCRVioTcp.Write({$IFNDEF CLR}PAnsiChar{$ENDIF}(FBuffer), 0, FBufferOffset);
  if cnt <> FBufferOffset then
    raise SocketException.CreateFmt('Socket error %d', [GetSocketError]{$IFNDEF CLR}, GetSocketError{$ENDIF});
  FBufferOffset := 0;
end;

procedure TSocketMessagePacker.WriteMessage(Msg: TMonitorMessage);
begin
  try
    ClearBuffer;
    WriteInteger(Msg.MessageType);
    Msg.Write(Self);
    Flush;
  except
    on SocketException do
      Close;
  end;
end;

procedure TSocketMessagePacker.WriteByte(Value: byte);
begin
  CheckAndRealloc(1);
  FBuffer[FBufferOffset] := Value;
  Inc(FBufferOffset);
end;

procedure TSocketMessagePacker.WriteInteger(Value: integer);
begin
  CheckAndRealloc(4);
  FBuffer[FBufferOffset] := byte(Value shr 24);
  FBuffer[FBufferOffset + 1] := byte(Value shr 16);
  FBuffer[FBufferOffset + 2] := byte(Value shr 8);
  FBuffer[FBufferOffset + 3] := byte(Value);
  Inc(FBufferOffset, 4);
end;

procedure TSocketMessagePacker.WriteString(const Value: WideString);
var
  Len: integer;
  Utf8Str: {$IFNDEF CLR}AnsiString{$ELSE}TBytes{$ENDIF};
  offset, cnt: Integer;
begin
{$IFNDEF CLR}
  Utf8Str := {$IFNDEF VER130}Utf8Encode(Value){$ELSE}AnsiString(Value){$ENDIF};
  Len := Length(Utf8Str);
  WriteInteger(Len);
  offset := 1;
  while Len > 0 do begin
    CheckAndRealloc(Len);
    cnt := Min(Length(FBuffer) - FBufferOffset, Len);
    Move(Utf8Str[offset], FBuffer[FBufferOffset], cnt);
    Inc(FBufferOffset, cnt);
    Inc(offset, cnt);
    Dec(Len, cnt);
  end;
{$ELSE}
  if Value <> nil then begin
    Utf8Str := Encoding.UTF8.GetBytes(Value);
    Len := Length(Utf8Str);
  end
  else begin
    Utf8Str := nil;
    Len := 0;
  end;
  WriteInteger(Len);
  offset := 0;
  while Len > 0 do begin
    CheckAndRealloc(Len);
    cnt := Min(Length(FBuffer) - FBufferOffset, Len);
    Buffer.BlockCopy(Utf8Str, offset, FBuffer, FBufferOffset, cnt);
    Inc(FBufferOffset, cnt);
    Inc(offset, cnt);
    Dec(Len, cnt);
  end;
{$ENDIF}
end;

{$IFDEF LINUX}
function GetTickCount: Cardinal;
var
  tv: timeval;
begin
  fpgettimeofday(@tv, nil);
  {$RANGECHECKS OFF}
  Result := int64(tv.tv_sec) * 1000 + tv.tv_usec div 1000;
end;
{$ENDIF}

function GetTickInterval(StartTickCount, FinishTickCount: Cardinal): Cardinal;
begin
  // each 49.7 days ticks are reseted, so we should take it into attention
  // and use GetTickInterval for avoiding the out of range error
  if FinishTickCount >= StartTickCount then
    Result := FinishTickCount - StartTickCount
  else
    Result := Longword($FFFFFFFF) - StartTickCount + FinishTickCount + 1;
end;

end.
