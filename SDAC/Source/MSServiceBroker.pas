//////////////////////////////////////////////////
//  SQL Server Data Access Components
//  Copyright © 1998-2012 Devart. All right reserved.
//  MSServiceBroker
//////////////////////////////////////////////////

{$IFNDEF CLR}

{$I Sdac.inc}
unit MSServiceBroker;
{$ENDIF}
interface

uses
  SysUtils, Classes, DB,{$IFDEF VER6P} Variants,{$ENDIF}{$IFNDEF CLR} CLRClasses,{$ENDIF}
  CRThread, MSAccess, OLEDBAccess, CRTypes;

const
  SEventNotificationType = 'http://schemas.microsoft.com/SQL/Notifications/EventNotification';
  SQueryNotificationType = 'http://schemas.microsoft.com/SQL/Notifications/QueryNotification';
  SFailedRemoteServiceBindingType = 'http://schemas.microsoft.com/SQL/ServiceBroker/BrokerConfigurationNotice/FailedRemoteServiceBinding';
  SFailedRouteType = 'http://schemas.microsoft.com/SQL/ServiceBroker/BrokerConfigurationNotice/FailedRoute';
  SMissingRemoteServiceBindingType = 'http://schemas.microsoft.com/SQL/ServiceBroker/BrokerConfigurationNotice/MissingRemoteServiceBinding';
  SMissingRouteType = 'http://schemas.microsoft.com/SQL/ServiceBroker/BrokerConfigurationNotice/MissingRoute';
  SDialogTimerType = 'http://schemas.microsoft.com/SQL/ServiceBroker/DialogTimer';
  SEndDialogType = 'http://schemas.microsoft.com/SQL/ServiceBroker/EndDialog';
  SErrorType = 'http://schemas.microsoft.com/SQL/ServiceBroker/Error';
  SDescriptionType = 'http://schemas.microsoft.com/SQL/ServiceBroker/ServiceDiagnostic/Description';
  SQueryType = 'http://schemas.microsoft.com/SQL/ServiceBroker/ServiceDiagnostic/Query';
  SStatusType = 'http://schemas.microsoft.com/SQL/ServiceBroker/ServiceDiagnostic/Status';
  SEchoType = 'http://schemas.microsoft.com/SQL/ServiceBroker/ServiceEcho/Echo';

type
  // TMSMessageStatus = (msReady, msReceived, msIncomplete, msRetained); // Status of the message. For messages returned by the RECEIVE command, the status is always 0. Messages in the queue may contain one of the following values: 0=Ready, 1=Received message, 2=Not yet complete, 3=Retained sent message
  TMSMessageValidation = (mvEmpty, mvNone, mvXML);

  TMSServiceBroker = class;
  TMSConversation = class;

  TMSMessage = class
  protected
    FBody: TBytes;
    FConversation: TMSConversation;
    FParams: _TStringList;
    FParamValues: _TStringList;

    // FStatus: TMSMessageStatus; // Status of the message. For messages returned by the RECEIVE command, the status is always 0. Messages in the queue may contain one of the following values: 0=Ready, 1=Received message, 2=Not yet complete, 3=Retained sent message
    FQueuingOrder: Int64; // Message order number within the queue.
    FConversationGroupId: TGuid; // Identifier for the conversation group that this message belongs to.
    FConversationHandle: TGuid; // Handle for the conversation that this message is part of.
    FMessageSequenceNumber: Int64;// Sequence number of the message within the conversation.
    // FServiceName: WideString; // Name of the service that the conversation is to.
    // FServiceId: integer; // SQL Server object identifier of the service that the conversation is to.
    FServiceContractName: WideString; // Name of the contract that the conversation follows.
    // FServiceContractId: integer; // SQL Server object identifier of the contract that the conversation follows.
    FMessageType: WideString; // {DEFAULT, http://schemas.microsoft.com/SQL/ServiceBroker/EndDialog} Name of the message type that describes the message.
    FMessageTypeId: integer; // SQL Server object identifier of the message type that describes the message.
    FValidation: TMSMessageValidation; // Validation used for the message. E=Empty N=None X=XML
    FIsEmpty: boolean;
    FMessageId: TGuid; // Unique identifier for the message.

    function GetAsString: string;
    function GetAsAnsiString: AnsiString;
    function GetAsWideString: WideString;

    procedure Fill(Source: TMSQuery);

    function GetParams: _TStringList;
    function GetParamValues: _TStringList;
    property Params: _TStringList read GetParams;
    property ParamValues: _TStringList read GetParamValues;

  public
    destructor Destroy; override;

    property AsString: string read GetAsString;
    property AsAnsiString: AnsiString read GetAsAnsiString;
    property AsWideString: WideString read GetAsWideString;
    property AsBytes: TBytes read FBody;

    property Conversation: TMSConversation read FConversation;

    // property Status: TMSMessageStatus read FStatus; // Status of the message. For messages returned by the RECEIVE command, the status is always 0. Messages in the queue may contain one of the following values: 0=Ready, 1=Received message, 2=Not yet complete, 3=Retained sent message
    property QueuingOrder: Int64 read FQueuingOrder; // Message order number within the queue.
    // property ConversationGroupId: TGuid read FConversationGroupId; // Identifier for the conversation group that this message belongs to.
    // property ConversationHandle: TGuid read FConversationHandle; // Handle for the conversation that this message is part of.
    property MessageSequenceNumber: Int64 read FMessageSequenceNumber;// Sequence number of the message within the conversation.
    // property ServiceName: WideString read FServiceName; // Name of the service that the conversation is to.
    // property ServiceId: integer read FServiceId; // SQL Server object identifier of the service that the conversation is to.
    // property ServiceContractName: WideString read FServiceContractName; // Name of the contract that the conversation follows.
    // property ServiceContractId: integer read FServiceContractId; // SQL Server object identifier of the contract that the conversation follows.
    property MessageType: WideString read FMessageType; // {DEFAULT, http://schemas.microsoft.com/SQL/ServiceBroker/EndDialog} Name of the message type that describes the message.
    property MessageTypeId: integer read FMessageTypeId; // SQL Server object identifier of the message type that describes the message.
    property Validation: TMSMessageValidation read FValidation; // Validation used for the message. E=Empty N=None X=XML
    property IsEmpty: boolean read FIsEmpty;
    property MessageId: TGuid read FMessageId; // Unique identifier for the message.
  end;

  TMSMessageEvent = procedure(Sender: TObject) of object;
  TMSConversationBeginEvent = procedure(Sender: TObject; Conversation: TMSConversation) of object;
  TMSConversationEndEvent = procedure(Sender: TObject; Conversation: TMSConversation; ErrorMessage: _string; ErrorCode: integer) of object;

  TMSConversation = class
  protected
    FHandle: TGuid;
    FGroupId: TGuid;

    FFarService: _string;
    FIsInitiator: boolean;
    FServiceBroker: TMSServiceBroker;

    FContractName: WideString;
    // FContractId: integer;

    function GetFarService: _string;
    function GetGroupId: TGuid;
    // function GetContractId: integer;
    function GetContractName: WideString;
    procedure SetGroupId(const Value: TGuid);

    procedure InternalSend(const MessageBody: TBytes; const MessageType: WideString = ''; const IsEmpty: boolean = False);
  public
    constructor Create(ServiceBroker: TMSServiceBroker; const Handle: TGuid; const IsInitiator: boolean; const FarService: _string = '');

    procedure EndConversation(const Cleanup: boolean = False);
    procedure EndConversationWithError(const ErrorMessage: _string; const ErrorCode: integer; const Cleanup: boolean = False);

    procedure Send(const MessageBody: TBytes; const MessageType: WideString = ''); overload;
    procedure Send(const MessageBody: AnsiString; const MessageType: WideString = ''); overload;
    procedure SendEmpty(const MessageType: WideString = '');
  {$IFNDEF CLR}
  {$IFDEF VER6P}
    procedure Send(const MessageBody: WideString; const MessageType: WideString = ''); overload;
  {$ENDIF}
  {$ENDIF}

    procedure BeginTimer(const Timeout: integer);
    function GetTransmissionStatus: _string;
    property Handle: TGuid read FHandle;
    property GroupId: TGuid read GetGroupId write SetGroupId;
    property ServiceBroker: TMSServiceBroker read FServiceBroker;
    property FarService: _string read GetFarService;
    property IsInitiator: boolean read FIsInitiator;
    property ContractName: WideString read GetContractName; // Name of the contract that the conversation follows.
    // property ContractId: integer read GetContractId; // SQL Server object identifier of the contract that the conversation follows.
  end;

{$IFDEF VER16P}
  [ComponentPlatformsAttribute(pidWin32 or pidWin64)]
{$ENDIF}
  TMSServiceBroker = class(TComponent)
  protected
    FQuery: TMSQuery;
    FReceiveSQL: _string;

    FConversations: TList;
    FMsgs: TThreadList;
    FReceivePrev: boolean;

    FDesignCreate: boolean;

    FService, FQueue: _string;
    FFetchRows: integer;
    FWaitTimeout: integer;

    FOnBeginConversation: TMSConversationBeginEvent;
    FOnEndConversation: TMSConversationEndEvent;

    // Async mode
    FOnMessage: TMSMessageEvent;
    FAsyncNotification: boolean;
    FStreamedAsyncNotification: boolean;
    FListener: TOLEDBThreadWrapper;
    FStopProcessing: boolean;

    function GetConversation(Index: Integer): TMSConversation;
    function GetConversationCount: integer;
    function GetConversationIndexByHandle(Handle: TGuid): integer;
    procedure EndConversations;

    function GetCurrentMessage: TMSMessage;
    procedure GetServiceBrockerObjNames(List: _TStrings; SQL: _string);
    function GetQueue: _string;

    function GetConnection: TMSConnection;
    procedure SetConnection(const Value: TMSConnection);
    procedure SetService(Value: _string);
    procedure SetFetchRows(Value: integer);
    procedure SetWaitTimeout(Value: integer);
    procedure SetOnMessage(Value: TMSMessageEvent);
    procedure ConnectChange(Sender: TObject; Connecting: Boolean);

    procedure Loaded; override;
    procedure AssignTo(Dest: TPersistent); override;
    procedure CheckInactive;

    procedure BeginConnection;
    procedure EndConnection;

    class function GenerateReceiveSQL(const Queue: _string; Conversation: TMSConversation; const FetchRows: integer; const TimeoutMSec: integer): _string;
    class function ReceiveFromServer(Query: TMSQuery; Msgs: TThreadList): integer; // Received messages count

    function BeginDialogInternal(
      const TargetService: _string;
      const TargetDatabase: _string;
      const Contract: _string;
      const LifeTime: integer;
      const UseEncryption: boolean;
      RelatedConversation: TMSConversation;
      const GroupId: TGuid
    ): TMSConversation;

    // Async mode
    procedure DoThreadMessage(Sender: TObject; Event: TObject);
    procedure DoException(Sender: TObject; E: Exception; var Fail: boolean);
    procedure Start;
    procedure Stop;
    procedure SetAsyncNotification(const Value: boolean);

  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    function BeginDialog(
      const TargetService: _string;
      const TargetDatabase: _string = '';
      const UseEncryption: boolean = True;
      RelatedConversation: TMSConversation = nil;
      const LifeTime: integer = 0;
      const Contract: _string = ''
    ): TMSConversation; overload;

    function BeginDialog(
      const TargetService: _string;
      const TargetDatabase: _string;
      const UseEncryption: boolean;
      const GroupId: TGuid;
      const LifeTime: integer = 0;
      const Contract: _string = ''
    ): TMSConversation; overload;

    property Conversations[Index: Integer]: TMSConversation read GetConversation; default;
    property ConversationCount: integer read GetConversationCount;
    property CurrentMessage: TMSMessage read GetCurrentMessage;

    function Receive(Conversation: TMSConversation = nil): boolean;

    procedure GetQueueNames(List: _TStrings);
    procedure GetServiceNames(List: _TStrings);
    procedure GetContractNames(List: _TStrings);
    procedure GetMessageTypeNames(List: _TStrings);

    procedure CreateServerObjects(Contract: _string = 'DEFAULT');
    procedure DropServerObjects;

    property Queue: _string read GetQueue;
  published
    property Connection: TMSConnection read GetConnection write SetConnection;
    property Service: _string read FService write SetService;
    property AsyncNotification: boolean read FAsyncNotification write SetAsyncNotification default False;
    property FetchRows: integer read FFetchRows write SetFetchRows default 0;
    property WaitTimeout: integer read FWaitTimeout write SetWaitTimeout default - 1;
    property OnMessage: TMSMessageEvent read FOnMessage write SetOnMessage;
    property OnBeginConversation: TMSConversationBeginEvent read FOnBeginConversation write FOnBeginConversation;
    property OnEndConversation: TMSConversationEndEvent read FOnEndConversation write FOnEndConversation;
  end;

implementation

uses
{$IFDEF CLR}
  System.Runtime.InteropServices, System.Text, System.XML, System.IO, System.Reflection,
{$ELSE}
  CRXml,
{$ENDIF}
  Windows, ComObj, ActiveX, DAConsts, DBAccess, MSConsts, MemUtils, MemData, OLEDBC;

function GuidToUniqueidentifier(Guid: TGuid): string;
begin
  Result := GuidToString(Guid);
  if (Result[1] = '{') and (Result[Length(Result)] = '}') then
    Result := Copy(Result, 2, Length(Result) - 2);
end;

{ TListenThread }

type
  TListenThread = class(TCRThread)
  private
    FConnection: TMSConnection;
    FQuery: TMSQuery;
    FMsgs: TThreadList;

  protected
    procedure InternalExecute; override;
    
  public
    constructor Create(Owner: TCRThreadWrapper); override;
    destructor Destroy; override;
  end;

constructor TListenThread.Create(Owner: TCRThreadWrapper);
begin
  inherited Create(Owner);

  FConnection := TMSConnection.Create(nil);
  FQuery := TMSQuery.Create(nil);
  FQuery.Connection := FConnection;
end;

destructor TListenThread.Destroy;
begin
  Terminate;
  WaitFor;

  FQuery.Free;
  FConnection.Free;

  inherited;
end;

procedure TListenThread.InternalExecute;
var
  MsgCount: integer;
begin
  OleCheck(CoInitializeEx(nil, {$IFNDEF CLR}COINIT_APARTMENTTHREADED{$ELSE}COINIT_MULTITHREADED{$ENDIF}));
  try
    FQuery.Connection.Connect;
    while not Terminated do begin
      MsgCount := TMSServiceBroker.ReceiveFromServer(FQuery, FMsgs);
      PostEvent(TObject(MsgCount));
    end;
  finally
    CoUninitialize;
  end;
end;

{ TMSMessage }

destructor TMSMessage.Destroy;
begin
  FParams.Free;
  FParamValues.Free;
  
  inherited;
end;

function TMSMessage.GetAsString: string;
begin
{$IFDEF VER12P}
  Result := GetAsWideString;
{$ELSE}
{$IFDEF CLR}
  Result := AsWideString;
{$ELSE}
  Result := AsAnsiString;
{$ENDIF}
{$ENDIF}
end;

function TMSMessage.GetAsAnsiString: AnsiString;
begin
  Result := Encoding.Default.GetString(FBody);
end;

function TMSMessage.GetAsWideString: WideString;
begin
  Result := Encoding.{$IFDEF CLR}Default.GetString{$ELSE}Unicode.GetWideString{$ENDIF}(FBody);
end;

procedure TMSMessage.Fill(Source: TMSQuery);
var
  Blob: MemData.TBlob;
  Handle: IntPtr;
  s: string;

begin
  (*case Source.FieldByName('status').AsInteger of
    0:
      FStatus := msReady;
    1:
      FStatus := msReceived;
    2:
      FStatus := msIncomplete;
    3:
      FStatus := msRetained;
    else
      Assert(False);
  end;*)
  FQueuingOrder := (Source.FieldByName('queuing_order') as TLargeIntField).AsLargeInt;
  FConversationGroupId := (Source.FieldByName('conversation_group_id') as TGuidField).AsGuid;
  FConversationHandle := (Source.FieldByName('conversation_handle') as TGuidField).AsGuid;
  FMessageSequenceNumber := (Source.FieldByName('message_sequence_number') as TLargeIntField).AsLargeInt;
  // FServiceName := (Source.FieldByName('service_name') as TWideStringField).Value;
  // FServiceId := Source.FieldByName('service_id').AsInteger;
  FServiceContractName := (Source.FieldByName('service_contract_name') as TWideStringField).Value;
  // FServiceContractId := Source.FieldByName('service_contract_id').AsInteger;
  FMessageType := (Source.FieldByName('message_type_name') as TWideStringField).Value;
  FMessageTypeId := Source.FieldByName('message_type_id').AsInteger;

  s := UpperCase(Source.FieldByName('validation').AsString);
  Assert(Length(s) = 1, 'Length(' + s + ') <> 1');
  case s[1] of
    'E': // Empty
      FValidation := mvEmpty;
    'N': // None
      FValidation := mvNone;
    'X': // XML
      FValidation := mvXML;
    else
      Assert(False);
  end;
  FIsEmpty := Source.FieldByName('message_body').IsNull;
  FMessageId := (Source.FieldByName('message_id') as TGuidField).AsGuid;

  Blob := Source.GetBlob('message_body');

  SetLength(FBody, Blob.Size);
  Handle := AllocGCHandle(FBody, True);
  try
    Blob.Read(0, 0, GetAddrOfPinnedObject(Handle));
  finally
    FreeGCHandle(Handle);
  end;
end;

function TMSMessage.GetParams: _TStringList;
var
{$IFDEF CLR}
  TextReader: StringReader;
  s: string;
{$ENDIF}
  Reader: XMLTextReader;
  i: integer;
  PrevNodeType: XmlNodeType;
begin
  if (FParams = nil) and (Validation = mvXML) then begin
    FParams := _TStringList.Create;
    FParamValues := _TStringList.Create;
  {$IFDEF CLR}
    TextReader := nil;
  {$ENDIF}
    Reader := nil;
    try
    {$IFDEF CLR}
      s := Encoding.Unicode.GetString(FBody, 2, Length(FBody) - 2); // Remove '?' symbol
      TextReader := StringReader.Create(s);
      Reader := XMLTextReader.Create(TextReader);
    {$ELSE}
      Reader := XMLTextReader.Create(AnsiString(Encoding.Unicode.GetWideString(FBody)));
    {$ENDIF}
      PrevNodeType := Reader.NodeType;
      while Reader.Read do begin
        if Reader.NodeType = ntElement then begin
          FParams.Add(Reader.Name);
          FParamValues.Add('');
        end;
        if (Reader.NodeType = ntText) and (PrevNodeType = ntElement) then begin
          Assert(FParams.Count = FParamValues.Count);
          FParamValues[FParamValues.Count - 1] := Reader.Value;
        end;

        for i := 0 to Reader.AttributeCount - 1 do begin
          Reader.MoveToAttribute(i);
          FParams.Add(Reader.Name);
          FParamValues.Add(Reader.Value);
        end;
        PrevNodeType := Reader.NodeType;
      end;
    finally
      Reader.Free;
    {$IFDEF CLR}
      TextReader.Free;
    {$ENDIF}
    end;
  end;
  Result := FParams;
  Assert(Result <> nil);
end;

function TMSMessage.GetParamValues: _TStringList;
begin
  GetParams; // fill FParams and FParamValues
  Result := FParamValues;
  Assert(Result <> nil);
end;

{ TMSConversation }

constructor TMSConversation.Create(ServiceBroker: TMSServiceBroker; const Handle: TGuid; const IsInitiator: boolean; const FarService: _string = '');
begin
  inherited Create;
  Assert(ServiceBroker <> nil);
  FServiceBroker := ServiceBroker;
  FHandle := Handle;
  FFarService := FarService;
  FIsInitiator := IsInitiator;

  FServiceBroker.FConversations.Add(Self);
  if Assigned(FServiceBroker.FOnBeginConversation) then
    FServiceBroker.FOnBeginConversation(FServiceBroker, Self);
end;

procedure TMSConversation.EndConversation(const Cleanup: boolean = False);
begin
  EndConversationWithError('', 0, Cleanup);
end;

procedure TMSConversation.EndConversationWithError(const ErrorMessage: _string; const ErrorCode: integer; const Cleanup: boolean = False);
var
  WithError: string;
  WithCleanUp: string;
begin
  Assert(FServiceBroker <> nil);
  Assert(FServiceBroker.Connection <> nil);
  try
    if Assigned(FServiceBroker.FOnEndConversation) then
      FServiceBroker.FOnEndConversation(FServiceBroker, Self, ErrorMessage, ErrorCode);
    FServiceBroker.FConversations.Remove(Self);

    if (ErrorMessage <> '') and (ErrorCode <> 0) then
      WithError := ' WITH ERROR = :ErrorCode DESCRIPTION = :ErrorMessage';

    if Cleanup then
      WithCleanUp := ' WITH CLEANUP';
    FServiceBroker.FQuery.SQL.Text := 'END CONVERSATION :Handle ' + WithError + WithCleanUp;
    FServiceBroker.FQuery.ParamByName('Handle').DataType := ftGuid;
    FServiceBroker.FQuery.ParamByName('Handle').AsString := GuidToString(Handle);

    if FServiceBroker.FQuery.Params.Count > 1 then begin
      FServiceBroker.FQuery.ParamByName('ErrorCode').AsInteger := ErrorCode;
      FServiceBroker.FQuery.ParamByName('ErrorMessage').AsString := ErrorMessage;
    end;
    FServiceBroker.FQuery.Execute;
  finally
    FServiceBroker.EndConnection;
    Free;
  end;
end;

{$IFNDEF CLR}
{$IFDEF VER6P}
procedure TMSConversation.Send(const MessageBody: WideString; const MessageType: WideString = '');
var
  Buf: TBytes;
begin
  Buf := Encoding.Unicode.{$IFNDEF VER5}GetBytes{$ELSE}GetBytesWide{$ENDIF}(MessageBody);
  Send(Buf, MessageType);
end;
{$ENDIF}
{$ENDIF}

function TMSConversation.GetTransmissionStatus: _string;
begin
  FServiceBroker.FQuery.SQL.Text := 'SELECT GET_TRANSMISSION_STATUS(CONVERT(uniqueidentifier, ''' +
      GuidToUniqueidentifier(Handle) + '''))';
  FServiceBroker.FQuery.Execute;
  Result := FServiceBroker.FQuery.Fields[0].AsString;
end;

procedure TMSConversation.BeginTimer(const Timeout: integer);
begin
  FServiceBroker.FQuery.SQL.Text := 'BEGIN CONVERSATION TIMER (CONVERT(uniqueidentifier, ''' +
      GuidToUniqueidentifier(Handle) + ''')) TIMEOUT = ' + IntToStr(Timeout);
  FServiceBroker.FQuery.Execute;
end;

function TMSConversation.GetFarService: _string;
begin
  if FFarService = '' then begin
    FServiceBroker.FQuery.SQL.Text := 'SELECT far_service FROM sys.conversation_endpoints WHERE conversation_handle = CONVERT(uniqueidentifier, ''' + GuidToUniqueidentifier(Handle) + ''')';
    FServiceBroker.FQuery.Execute;
    FFarService := FServiceBroker.FQuery.Fields[0].AsString;
  end;

  Result := FFarService;
end;

function TMSConversation.GetGroupId: TGuid;
begin
  if CompareGuid(FGroupId, DB_NULLGUID) then begin
    Assert(FServiceBroker <> nil);
    Assert(FServiceBroker.FQuery <> nil);

    // sys.conversation_endpoints
    FServiceBroker.FQuery.SQL.Text :=
      'SELECT e.conversation_group_id, e.service_contract_id, c.name ' +
      'FROM sys.conversation_endpoints e, sys.service_contracts c ' +
      'WHERE (e.conversation_handle = CONVERT(uniqueidentifier, ''' + GuidToUniqueidentifier(Handle) + ''')) AND (e.service_contract_id = c.service_contract_id)';
    FServiceBroker.FQuery.Execute;
    FGroupId := StringToGUID(FServiceBroker.FQuery.Fields[0].AsString);
    // FContractId := FServiceBroker.FQuery.Fields[1].AsInteger;
    FContractName := FServiceBroker.FQuery.Fields[2].AsString;
  end;

  Result := FGroupId;
end;

(*function TMSConversation.GetContractId: integer;
begin
  if CompareGuid(FGroupId, DB_NULLGUID) then
    GetGroupId; // Fill FGroupId, FContractId, FContractName

  Result := FContractId;
end;*)

function TMSConversation.GetContractName: WideString;
begin
  if CompareGuid(FGroupId, DB_NULLGUID) then
    GetGroupId; // Fill FGroupId, FContractId, FContractName

  Result := FContractName;
end;

procedure TMSConversation.SetGroupId(const Value: TGuid);
begin
  Assert(FServiceBroker <> nil);
  Assert(FServiceBroker.Connection <> nil);

  FServiceBroker.FQuery.SQL.Text := 'MOVE CONVERSATION CONVERT(uniqueidentifier, ''' +
    GuidToUniqueidentifier(Handle) + ''')';
  FServiceBroker.FQuery.SQL.Add('TO CONVERT(uniqueidentifier, ''' +
    GuidToUniqueidentifier(Value) + ''')');
  FServiceBroker.FQuery.Execute;
  FGroupId := Value;
end;

procedure TMSConversation.SendEmpty(const MessageType: WideString = '');
begin
  InternalSend(nil, MessageType, True);
end;

procedure TMSConversation.Send(const MessageBody: AnsiString; const MessageType: WideString = '');
var
  Buf: TBytes;
begin
  Buf := Encoding.Default.GetBytes(MessageBody);
  InternalSend(Buf, MessageType);
end;

procedure TMSConversation.Send(const MessageBody: TBytes; const MessageType: WideString = '');
begin
  InternalSend(MessageBody, MessageType);
end;

procedure TMSConversation.InternalSend(const MessageBody: TBytes; const MessageType: WideString = ''; const IsEmpty: boolean = False);
var
  ParamIndex: integer;
begin
  Assert(FServiceBroker <> nil);
  Assert(FServiceBroker.Connection <> nil);
  
  ParamIndex := 0;
  FServiceBroker.FQuery.SQL.BeginUpdate;
  try
    FServiceBroker.FQuery.SQL.Text := 'SEND ON CONVERSATION CONVERT(uniqueidentifier, ''' + GuidToUniqueidentifier(Handle) + ''')';
    if MessageType <> '' then
      FServiceBroker.FQuery.SQL.Add('MESSAGE TYPE :MessageType' + LineSeparator);
    if not IsEmpty then
      FServiceBroker.FQuery.SQL.Add('(:MessageBody);');
  finally
    FServiceBroker.FQuery.SQL.EndUpdate;
  end;
  if MessageType <> '' then begin
    FServiceBroker.FQuery.Params[ParamIndex].DataType := ftWideString;
    FServiceBroker.FQuery.Params[ParamIndex].Value := MessageType;
    FServiceBroker.FQuery.Params[ParamIndex].ParamType := ptInput;
    Inc(ParamIndex);
  end;

  if not IsEmpty then begin
    FServiceBroker.FQuery.Params[ParamIndex].DataType := ftBytes;
    FServiceBroker.FQuery.Params[ParamIndex].Value := MessageBody;
    FServiceBroker.FQuery.Params[ParamIndex].ParamType := ptInput;
  end;
  FServiceBroker.FQuery.Execute;
end;

{ TMSServiceBroker }

constructor TMSServiceBroker.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);

  FWaitTimeout := -1;

  FConversations := TList.Create;

  FQuery := TMSQuery.Create(nil);
  FQuery.ReadOnly := True;
  FMsgs := TThreadList.Create;

  FDesignCreate := csDesigning in ComponentState;
end;

destructor TMSServiceBroker.Destroy;
var
  i: integer;
  MsgList: TList;
begin
  Stop;
  EndConversations;

  Connection := nil;  // UnregisterClient

  if FMsgs <> nil then begin
    MsgList := FMsgs.LockList;
    try
      for i := 0 to MsgList.Count - 1 do
        TObject(MsgList[i]).Free;
    finally
      FMsgs.UnlockList;
      FMsgs.Free;
    end;
  end;

  FQuery.Free;
  FConversations.Free;
  
  inherited;
end;

procedure TMSServiceBroker.Loaded;
begin
  inherited;

  FDesignCreate := False;
  try
    if FStreamedAsyncNotification then
      AsyncNotification := True;
  except
    if csDesigning in ComponentState then
      ApplicationHandleException(Self)
    else
      raise;
  end;
end;

procedure TMSServiceBroker.AssignTo(Dest: TPersistent);
begin
  if Dest is TMSServiceBroker then begin
    TMSServiceBroker(Dest).Connection := Connection;
    TMSServiceBroker(Dest).Service := Service;
    TMSServiceBroker(Dest).OnMessage := OnMessage;
  end
  else
    inherited;
end;

procedure TMSServiceBroker.BeginConnection;
begin
  if Connection = nil then
    raise Exception.Create(SConnectionNotDefined);
  TDBAccessUtils.InternalConnect(Connection);
end;

procedure TMSServiceBroker.EndConnection;
begin
  TDBAccessUtils.InternalDisconnect(Connection);
end;

procedure TMSServiceBroker.EndConversations;
var
  i: integer;
begin
  for i := ConversationCount - 1 downto 0 do
    Conversations[i].Free;
  FConversations.Clear;
end;

procedure TMSServiceBroker.CheckInactive;
begin
  if AsyncNotification then
    if ([csUpdating, csDesigning] * ComponentState) <> [] then
      Stop
    else
      DatabaseError(SServiceBrokerAsync, Self);
end;

procedure TMSServiceBroker.ConnectChange(Sender: TObject; Connecting: Boolean);
begin
  if not Connecting then begin
    Stop;
    EndConversations;
  end;
end;

function TMSServiceBroker.GetConversation(Index: Integer): TMSConversation;
begin
  Result := TMSConversation(FConversations[Index]);
end;

function TMSServiceBroker.GetConversationCount: integer;
begin
  Result := FConversations.Count;
end;

function TMSServiceBroker.GetConversationIndexByHandle(Handle: TGuid): integer;
var
  i: integer;
begin
  Result := -1;
  for i := 0 to FConversations.Count - 1 do
    if CompareGuid(Conversations[i].Handle, Handle) then begin
      Result := i;
      Exit;
    end;
end;

class function TMSServiceBroker.GenerateReceiveSQL(const Queue: _string; Conversation: TMSConversation; const FetchRows: integer; const TimeoutMSec: integer): _string;
begin
  if FetchRows = 0 then
    Result := 'RECEIVE '
  else
    Result := 'RECEIVE TOP(' + IntToStr(FetchRows) + ') ';

  Result := Result + 'message_id, message_body, status, queuing_order, conversation_group_id, conversation_handle, message_sequence_number, service_contract_name, service_contract_id, message_type_name, message_type_id, validation FROM ' + OLEDBSQLInfo.NormalizeName(Queue);
  if Conversation <> nil then
    Result := Result + ' WHERE conversation_handle = ' +
      'CONVERT(uniqueidentifier, ''''' + GuidToUniqueidentifier(Conversation.Handle) + ''''')';

  if TimeoutMSec <> - 1 then
    Result := 'WAITFOR (' + Result + '), TIMEOUT ' + IntToStr(TimeoutMSec);
end;

function TMSServiceBroker.BeginDialogInternal(
  const TargetService: _string;
  const TargetDatabase: _string;
  const Contract: _string;
  const LifeTime: integer;
  const UseEncryption: boolean;
  RelatedConversation: TMSConversation;
  const GroupId: TGuid
): TMSConversation;
var
  Param: TMSParam;
  ServiceBrokerGuid: string;
begin
  if FService = '' then
    raise Exception.Create(SServiceNotDefined);

  BeginConnection;
  try
    FQuery.SQL.Clear;
    FQuery.SQL.Append('BEGIN DIALOG :ch FROM SERVICE :fs TO SERVICE :ts');
    Param := FQuery.ParamByName('ch');
    Param.DataType := ftGuid;
    Param.ParamType := ptOutput;
    Param := FQuery.ParamByName('fs');
    Param.AsString := FService;
    Param := FQuery.ParamByName('ts');
    Param.AsString := TargetService;

    if TargetDatabase <> '' then begin
      ServiceBrokerGuid := FQuery.Connection.ExecSQL('SELECT :Result = service_broker_guid FROM sys.databases WHERE name = :database', ['', TargetDatabase]);
      FQuery.SQL.Append(', :bi');
      Param := FQuery.ParamByName('bi');
      Param.AsString := ServiceBrokerGuid;
    end;

    if Contract <> '' then begin
      FQuery.SQL.Append(' ON CONTRACT :cn');
      Param := FQuery.ParamByName('cn');
      Param.AsString := Contract;
    end;

    if UseEncryption then
      FQuery.SQL.Append(' WITH ENCRYPTION = ON ')
    else
      FQuery.SQL.Append(' WITH ENCRYPTION = OFF ');

    if RelatedConversation <> nil then begin
      FQuery.SQL.Append(', RELATED_CONVERSATION = :rch ');
      Param := FQuery.ParamByName('rch');
      Param.AsString := GUIDToString(RelatedConversation.Handle);
    end
    else
      if not CompareGuid(GroupId, DB_NULLGUID) then begin
        FQuery.SQL.Append(', RELATED_CONVERSATION_GROUP = :rcg ');
        Param := FQuery.ParamByName('rcg');
        Param.AsString := GUIDToString(GroupId);
      end;
    if LifeTime <> 0 then
      FQuery.SQL.Append(', LIFETIME = ' + IntToStr(LifeTime) + ' ');

    FQuery.Execute;

    Param := FQuery.ParamByName('ch');
    Result := TMSConversation.Create(Self, StringToGUID(Param.AsString), True, TargetService);
  except
    EndConnection;
    raise;
  end;
end;

function TMSServiceBroker.BeginDialog(
  const TargetService: _string;
  const TargetDatabase: _string = '';
  const UseEncryption: boolean = True;
  RelatedConversation: TMSConversation = nil;
  const LifeTime: integer = 0;
  const Contract: _string = ''): TMSConversation;
begin
  Result := BeginDialogInternal(TargetService, TargetDatabase, Contract, LifeTime, UseEncryption, RelatedConversation, DB_NULLGUID);
end;

function TMSServiceBroker.BeginDialog(
  const TargetService: _string;
  const TargetDatabase: _string;
  const UseEncryption: boolean;
  const GroupId: TGuid;
  const LifeTime: integer = 0;
  const Contract: _string = ''): TMSConversation;
begin
  Result := BeginDialogInternal(TargetService, TargetDatabase, Contract, LifeTime, UseEncryption, nil, GroupId);
end;

function TMSServiceBroker.GetCurrentMessage: TMSMessage;
var
  MsgList: TList;
begin
  MsgList := FMsgs.LockList;
  try
    if MsgList.Count > 0 then
      Result := TMSMessage(MsgList[0])
    else
      Result := nil;
  finally
    FMsgs.UnlockList;
  end;
end;

procedure TMSServiceBroker.GetServiceBrockerObjNames(List: _TStrings; SQL: _string);
var
  QueueDS: TMSQuery;
  NameFld: TField;
begin
  BeginConnection;
  List.Clear;
  QueueDS := nil;
  try
    QueueDS := TMSQuery.Create(nil);
    QueueDS.Connection := Connection;
    QueueDS.SQL.Text := SQL;

    if TOLEDBConnection(TMSAccessUtils.FIConnection(Connection)).DBMSPrimaryVer <= 8 then
      Exit;

    QueueDS.Open;

    NameFld := QueueDS.Fields[0];
    while not QueueDS.Eof do begin
      List.Add(NameFld.AsString);
      QueueDS.Next
    end;

    if List is _TStringList then
      _TStringList(List).Sort;
  finally
    QueueDS.Free;
    EndConnection;
  end;
end;

function TMSServiceBroker.GetQueue: _string;
begin
  if FQueue = '' then begin
    BeginConnection;
    try
      FQuery.SQL.Text := 'SELECT q.name FROM sys.service_queues q, sys.services s WHERE (s.service_queue_id = q.object_id) AND (s.name = ''' + Service + ''')';
      FQuery.Execute;
      FQueue := FQuery.Fields[0].AsString;
    finally
      EndConnection;
    end;
  end;
  Result := FQueue;
end;

function TMSServiceBroker.GetConnection: TMSConnection;
begin
  Assert(FQuery <> nil);
  Result := TMSConnection(FQuery.Connection);
end;

procedure TMSServiceBroker.SetConnection(const Value: TMSConnection);
begin
  if Value <> Connection then begin
    Stop;
    EndConversations;

    if Connection <> nil then
      TDBAccessUtils.UnRegisterClient(Connection, Self);

    FQuery.Connection := Value;
    if Value <> nil then 
      TDBAccessUtils.RegisterClient(Value, Self, ConnectChange);
  end;
end;

procedure TMSServiceBroker.SetService(Value: _string);
begin
  if FService <> Value then begin
    Stop;
    if FConversations.Count > 0 then
      if ([csUpdating, csDesigning] * ComponentState) <> [] then
        EndConversations
      else
        DatabaseError(SDialogActive, Self);
    FService := Value;
    FQueue := '';
  end;
end;

procedure TMSServiceBroker.SetFetchRows(Value: integer);
begin
  FFetchRows := Value;
  FReceiveSQL := '';
end;

procedure TMSServiceBroker.SetWaitTimeout(Value: integer);
begin
  FWaitTimeout := Value;
  FReceiveSQL := '';
end;

procedure TMSServiceBroker.SetOnMessage(Value: TMSMessageEvent);
begin
  CheckInactive;
  FOnMessage := Value;
end;

class function TMSServiceBroker.ReceiveFromServer(Query: TMSQuery; Msgs: TThreadList): integer; // Received messages count
var
  Msg: TMSMessage;
  MsgList: TList;
begin
  Assert(Query <> nil);
  Assert(Query.Connection <> nil);

  Result := 0;
  TDBAccessUtils.InternalConnect(Query.Connection);
  try
    Query.Execute;

    MsgList := Msgs.LockList;
    try
      while not Query.Eof do begin
        Msg := TMSMessage.Create;
        try
          Msg.Fill(Query);
        except
          Msg.Free;
          raise;
        end;
        MsgList.Add(Msg);
        Inc(Result);

        Query.Next;
      end;
    finally
      Msgs.UnlockList;
    end;
    Query.Close;
  finally
    // Query will automatically disconnect
    // TDBAccessUtils.InternalDisconnect(Query.Connection);
  end;
end;

function TMSServiceBroker.Receive(Conversation: TMSConversation = nil): boolean;
var
  MsgList: TList;

  procedure RemoveCurrentMessage;
  var
    Msg: TMSMessage;
  begin
    Msg := TMSMessage(MsgList[0]);
    try
      if (Msg.MessageType = SEndDialogType) or (Msg.MessageType = SErrorType) then begin
        FConversations.Remove(Msg.Conversation);
        Msg.Conversation.Free;
      end;
    finally
      Msg.Free;
      MsgList.Delete(0);
    end;
  end;

  procedure ActualizeCurrentMessage;
  var
    Msg: TMSMessage;
    ConversationIndex: integer;
    i: integer;
    s: _string;
  begin
    Msg := CurrentMessage;

    ConversationIndex := GetConversationIndexByHandle(Msg.FConversationHandle);
    if ConversationIndex < 0 then
      Msg.FConversation := TMSConversation.Create(Self, Msg.FConversationHandle, False)
    else
      Msg.FConversation := Conversations[ConversationIndex];

    Msg.FConversation.FGroupId := Msg.FConversationGroupId;
    Msg.FConversation.FContractName := Msg.FServiceContractName;
    // Msg.FConversation.FContractId := Msg.FServiceContractId;

    if Assigned(FOnEndConversation) then begin
      if Msg.MessageType = SEndDialogType then
        FOnEndConversation(Self, Msg.Conversation, '', 0)
      else
      if Msg.MessageType = SErrorType then begin
        i := Msg.Params.IndexOf('Description');
        s := Msg.ParamValues[i];
        i := Msg.Params.IndexOf('Code');
        i := StrToInt(Msg.ParamValues[i]);
        FOnEndConversation(Self, Msg.Conversation, s, i);
      end;
    end;
  end;

begin
  if (Conversation <> nil) and AsyncNotification then
    raise Exception.Create(SServiceBrokerAsync);

  MsgList := FMsgs.LockList;
  try
    if (MsgList.Count > 0) and FReceivePrev then
      RemoveCurrentMessage;

    if (MsgList.Count = 0) and not AsyncNotification then begin
      if (FReceiveSQL = '') or (Conversation <> nil) then
        FReceiveSQL := GenerateReceiveSQL(Queue, Conversation, FetchRows, WaitTimeout);
      FQuery.SQL.Text := FReceiveSQL;
      ReceiveFromServer(FQuery, FMsgs);
    end;

    Result := MsgList.Count > 0;
    FReceivePrev := Result;
    if Result then
      ActualizeCurrentMessage;
  finally
    FMsgs.UnlockList;
  end;
end;

procedure TMSServiceBroker.GetQueueNames(List: _TStrings);
begin
  GetServiceBrockerObjNames(List, 'SELECT name FROM sys.service_queues');
end;

procedure TMSServiceBroker.GetServiceNames(List: _TStrings);
begin
  GetServiceBrockerObjNames(List, 'SELECT name FROM sys.services');
end;

procedure TMSServiceBroker.GetContractNames(List: _TStrings);
begin
  GetServiceBrockerObjNames(List, 'SELECT name FROM sys.service_contracts');
end;

procedure TMSServiceBroker.GetMessageTypeNames(List: _TStrings);
begin
  GetServiceBrockerObjNames(List, 'SELECT name FROM sys.service_message_types');
end;

procedure TMSServiceBroker.CreateServerObjects(Contract: _string = 'DEFAULT');
begin
  Connection.ExecSQL(
    'IF NOT EXISTS (SELECT * FROM sys.service_queues WHERE name = ''' + Service + '_QUEUE'') CREATE QUEUE ' + OLEDBSQLInfo.NormalizeName(Service + '_QUEUE') + #$D#$A +
    'IF NOT EXISTS (SELECT * FROM sys.services WHERE name = ''' + Service + ''') CREATE SERVICE ' + OLEDBSQLInfo.NormalizeName(Service) + ' ON QUEUE ' + OLEDBSQLInfo.NormalizeName(Service + '_QUEUE') + '([' + Contract + '])', []);
end;

procedure TMSServiceBroker.DropServerObjects;
begin
  BeginConnection;
  try
    Stop;

    Connection.ExecSQL(
      'IF EXISTS (SELECT * FROM sys.services WHERE name = ''' + Service + ''') DROP SERVICE ' + OLEDBSQLInfo.NormalizeName(Service) + #$D#$A +
      'IF EXISTS (SELECT * FROM sys.service_queues WHERE name = ''' + Service + '_Queue'') DROP QUEUE ' + OLEDBSQLInfo.NormalizeName(Service + '_Queue'), []);
  finally
    EndConnection;
  end;
end;

procedure TMSServiceBroker.DoThreadMessage(Sender: TObject; Event: TObject);
var
  i: integer;
begin
  if Assigned(FOnMessage) then
    for i := 0 to Integer(Event) - 1do
      FOnMessage(Self);
end;

procedure TMSServiceBroker.DoException(Sender: TObject; E: Exception; var Fail: boolean);
begin
  if (E is EOLEDBError) then begin
    if EOLEDBError(E).ErrorCode = DB_E_CANCELED then
      Fail := False
    else
      if Connection.Connected then
        TMSAccessUtils.DoError(Connection, E, Fail);
    if Fail then
      Stop;
  end;
end;

procedure TMSServiceBroker.Start;
var
  lt: TListenThread;
begin
  if not FAsyncNotification then begin
    BeginConnection;
    if FService = '' then
      DatabaseError(SServiceNotDefined, Self);
    {if not Assigned(FOnMessage) then
      DatabaseError(SOnMessageNotAssigned, Self);}

    Assert(FListener = nil);
    FListener := TOLEDBThreadWrapper.Create(TListenThread, True);
    lt := TListenThread(FListener.Thread);
    Assert(lt.FConnection <> nil);
    lt.FConnection.Assign(Connection);
    lt.FConnection.LoginPrompt := False;
    lt.FQuery.SQL.Text := GenerateReceiveSQL(Queue, nil, FetchRows, MaxInt);
    lt.FMsgs := FMsgs;

    FListener.OnPostEvent := DoThreadMessage;
    FListener.OnException := DoException;
    FListener.Resume;
    FAsyncNotification := True;
  end;
end;

procedure TMSServiceBroker.Stop;
begin
  if FStopProcessing then
    Exit;

  if FAsyncNotification then begin
    FStopProcessing := True;
    try
      TListenThread(FListener.Thread).FQuery.BreakExec;
      FListener.Free;
      FListener := nil;
      FAsyncNotification := False;
    finally
      FStopProcessing := False;
      EndConnection;
    end;
  end;
end;

procedure TMSServiceBroker.SetAsyncNotification(const Value: boolean);
begin
  if csReading in ComponentState then begin
    if Value then
      FStreamedAsyncNotification := True;
  end
  else
    if Value <> FAsyncNotification then
      if Value then
        Start
      else
        Stop;
end;

end.
