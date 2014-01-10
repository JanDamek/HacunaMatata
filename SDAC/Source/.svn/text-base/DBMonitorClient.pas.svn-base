
//////////////////////////////////////////////////
//  DB Access Components
//  Copyright © 1998-2012 Devart. All right reserved.
//////////////////////////////////////////////////

{$IFNDEF CLR}

{$I Dac.inc}

unit DBMonitorClient;
{$ENDIF}

interface

uses
{$IFDEF MSWINDOWS}
  Windows,
{$ENDIF}
  Classes, SysUtils, SyncObjs, {$IFDEF VER6P}Types,{$ENDIF}
{$IFDEF CLR}
  WinUtils, System.Text, System.IO, System.Threading,
{$ENDIF}
  DBMonitorMessages;

const
  ET_APPSTARTED  = 0;
  ET_APPFINISHED = 1;
  ET_CONNECT     = 2;
  ET_DISCONNECT  = 3;
  ET_BEGIN_TRANS = 4;
  ET_COMMIT      = 5;
  ET_ROLLBACK    = 6;
  ET_SAVEPOINT   = 7;
  ET_PREPARE     = 8;
  ET_UNPREPARE   = 9;
  ET_EXECUTE     = 10;
  ET_FETCH       = 11;
  ET_BLOB        = 12;
  ET_OBJ_CREATE  = 13;
  ET_OBJ_DESTROY = 14;
  ET_CONN_POOL   = 15;
  ET_MISC        = 16;

  OT_UNKNOWN     = 0;
  OT_CONNECTION  = 1;
  OT_TRANSACTION = 2;
  OT_COMMAND     = 3;
  OT_DATAREADER  = 4;
  OT_CONNPOOL    = 5;

type
  TSQLParam = TMsgSQLParam;
  TSQLParams = TMsgSQLParams;

  TMonitorEvent = record
    EventID: cardinal;
    EventType: integer;
    ObjectID: cardinal;
    ObjectName: WideString;
    ObjectType: integer;
    ObjectTypeName: WideString;
    ParentID: cardinal;
    ParentName: WideString;
    ParentType: integer;
    ParentTypeName: WideString;
    Description: WideString;
    SQL: WideString;
    Params: TSQLParams;
    Failed: boolean;
    ErrorText: WideString;
    RowsAffected: integer;
    CallStack: TWideStringDynArray;
  end;

  TEventSendThread = class;

  TDBMonitor = class
  private
    FPacker: TSocketMessagePacker;
    FCaption: WideString;
    FHost: string;
    FPort: integer;
    FReconnectTimeout: cardinal;
    FSendTimeout: cardinal;
    FLastEventID: integer;
    FEventCache: array of TEventMessage;
    FCacheHead: integer;
    FCacheTail: integer;
    FStartupMessage: TStartupMessage;
    FAddEventLock: TCriticalSection;
    FNewMesEvent: TEvent;
    FSendThread: TEventSendThread;
    FIsDesigning: boolean;

    procedure AddEvent(Msg: TEventMessage);
    function GetStartupMessage: TStartupMessage;

  public
    constructor Create;
    destructor Destroy; override;

    procedure Startup;
    procedure Finish;
    function IsMonitorActive: boolean;

    // EventID can be used to identify events. It is autoincremented.
    procedure SendEvent(var Event: TMonitorEvent);
    procedure SendEventStart(var Event: TMonitorEvent);
    procedure SendEventEnd(const Event: TMonitorEvent);

    property Caption: WideString read FCaption write FCaption;
    property Host: string read FHost write FHost;
    property Port: integer read FPort write FPort;
    property ReconnectTimeout: cardinal read FReconnectTimeout write FReconnectTimeout;
    property SendTimeout: cardinal read FSendTimeout write FSendTimeout;
    property IsDesigning: boolean write FIsDesigning;
  end;

  TEventSendThread = class(TThread)
  private
    FDBMonitor: TDBMonitor;

  protected
    procedure Execute; override;

  public
    constructor Create(DBMonitor: TDBMonitor);
    procedure Terminate;
  end;

  function GetDBMonitor: TDBMonitor;

  function HasMonitor: boolean;
  function WhereMonitor: string;
{$IFDEF MSWINDOWS}
  function GetDBMonitorVersion: string;
{$ENDIF}

implementation

uses
{$IFDEF MSWINDOWS}
  Registry,
{$ENDIF}
  CRFunctions;

const
  EventCacheSize = 1000 + 1;

var
  DBMonitor: TDBMonitor = nil;

resourcestring
  SAppStarted         = '%s monitoring is started';
  SAppFinished        = '%s monitoring is finished';

{$IFDEF MSWINDOWS}
function GetModuleName: string;
var
{$IFNDEF CLR}
  ModName: array[0..MAX_PATH] of Char;
  Len: integer;
{$ELSE}
  ModName: StringBuilder;
{$ENDIF}
begin
{$IFNDEF CLR}
  Len := Windows.GetModuleFileName(hInstance, ModName, MAX_PATH);
  SetString(Result, ModName, Len);
{$ELSE}
  ModName := StringBuilder.Create(MAX_PATH);
  Windows.GetModuleFileName(hInstance, ModName, MAX_PATH);
  Result := ModName.ToString;
{$ENDIF}
end;
{$ENDIF}

function GetProcessId: integer;
begin
{$IFDEF MSWINDOWS}
  Result := GetCurrentProcessId;
{$ELSE}
  Result := 0;
{$ENDIF}
end;

{ TDBMonitor }

constructor TDBMonitor.Create;
begin
  inherited;

  FReconnectTimeout := DefaultReconnectTimeout;
  FSendTimeout := DefaultSendTimeout;
  SetLength(FEventCache, EventCacheSize);
  FAddEventLock := TCriticalSection.Create;
  FNewMesEvent := TEvent.Create(nil, False, False, '');
end;

destructor TDBMonitor.Destroy;
begin
  //if (FPacker <> nil) and FPacker.IsActive then
  //  Finish;

  FSendThread.Terminate;
{$IFDEF WIN32_64}
  if IsLibrary or FIsDesigning then
    TerminateThread(FSendThread.Handle, 0);
{$ENDIF}
  FSendThread.WaitFor;
  FSendThread.Free;

  FAddEventLock.Free;
  FNewMesEvent.Free;
  FPacker.Free;
  FStartupMessage.Free;

  inherited;
end;

function TDBMonitor.GetStartupMessage: TStartupMessage;
begin
  if FStartupMessage = nil then begin
    FStartupMessage := TStartupMessage.Create;
    with FStartupMessage do begin
      ProcessId := GetProcessId;
      EventType := ET_APPSTARTED;
      Description := Format(sAppStarted, [FCaption]);
      ExeName := ParamStr(0);
    {$IFDEF MSWINDOWS}
      if ExeName = '' then
        ExeName := GetModuleName;
    {$ENDIF}
    end;
  end;

  Result := FStartupMessage;
end;

procedure TDBMonitor.Startup;
begin
  AddEvent(GetStartupMessage);
end;

procedure TDBMonitor.Finish;
var
  Msg: TEventMessage;
begin
  Msg := TEventMessage.Create;
  try
    with Msg do begin
      ProcessId := GetProcessId;
      EventType := ET_APPFINISHED;
      Description := Format(sAppFinished, [FCaption]);
    end;
    AddEvent(Msg);
  except
    Msg.Free;
    raise;
  end;
end;

function TDBMonitor.IsMonitorActive: boolean;
begin
  if (FPacker = nil) or not FPacker.CheckActive then begin
    if FPacker = nil then
      FPacker := TSocketMessagePacker.Create;

    FPacker.Host := FHost;
    FPacker.Port := FPort;
    FPacker.ReconnectTimeout := FReconnectTimeout;
    FPacker.SendTimeout := FSendTimeout;
    if FPacker.Open then
      Result := True
    else
      Result := False;
  end
  else
    Result := True;
end;

procedure TDBMonitor.SendEvent(var Event: TMonitorEvent);
begin
  SendEventStart(Event);
  SendEventEnd(Event);
end;

procedure TDBMonitor.SendEventStart(var Event: TMonitorEvent);
var
  Msg: TEventStartMessage;
begin
  Msg := TEventStartMessage.Create;
  try
    Event.EventID := InterlockedIncrement(FLastEventID);

    Msg.ProcessId := GetProcessId;
    Msg.EventType := Event.EventType;
    Msg.Description := Event.Description;
    Msg.StartTime := GetTickCount;
    Msg.EventID := Event.EventID;

    Msg.ObjectID := Event.ObjectID;
    Msg.ObjectName := Event.ObjectName;
    Msg.ObjectType := Event.ObjectType;
    Msg.ObjectTypeName := Event.ObjectTypeName;
    Msg.ParentID := Event.ParentID;
    Msg.ParentName := Event.ParentName;
    Msg.ParentType := Event.ParentType;
    Msg.ParentTypeName := Event.ParentTypeName;

    Msg.SQL := Event.SQL;
    Msg.Params := Event.Params;
    Msg.CallStack := Event.CallStack;

    AddEvent(Msg);
  except
    Msg.Free;
    raise;
  end;
end;

procedure TDBMonitor.SendEventEnd(const Event: TMonitorEvent);
var
  Msg: TEventEndMessage;
begin
  Msg := TEventEndMessage.Create;
  try
    Msg.ProcessId := GetProcessId;
    Msg.EventType := Event.EventType;
    Msg.Description := Event.Description;
    Msg.EndTime := GetTickCount;
    if Event.EventID = 0 then
      Msg.EventID := FLastEventID
    else
      Msg.EventID := Event.EventID;

    Msg.Failed := Integer(Event.Failed);
    Msg.ErrorText := Event.ErrorText;
    Msg.Params := Event.Params;
    Msg.RowsAffected := Event.RowsAffected;

    AddEvent(Msg);
  except
    Msg.Free;
    raise;
  end;
end;

procedure TDBMonitor.AddEvent(Msg: TEventMessage);
var
  NewTail: integer;
begin
  FAddEventLock.Enter;
  try
    NewTail := FCacheTail + 1;
    if NewTail = EventCacheSize then
      NewTail := 0;

    if NewTail = FCacheHead then begin
      if Msg <> FStartupMessage then
        Msg.Free;
      exit;
    end;

    FEventCache[FCacheTail] := Msg;
    FCacheTail := NewTail;
    FNewMesEvent.SetEvent;

    if FSendThread = nil then
      FSendThread := TEventSendThread.Create(Self);
  finally
    FAddEventLock.Leave;
  end;
end;

{ TEventSendThread }

constructor TEventSendThread.Create(DBMonitor: TDBMonitor);
begin
  inherited Create(True);

{$IFDEF CLR}
  Handle.IsBackGround := True;
{$ENDIF}
  FDBMonitor := DBMonitor;
  Resume;
end;

procedure TEventSendThread.Terminate;
begin
  inherited;

  FDBMonitor.FNewMesEvent.SetEvent;
end;

procedure TEventSendThread.Execute;
var
  Msg: TEventMessage;
  NewHead: integer;
  OldActive, Active: boolean;
begin
{$IFDEF WIN32_64}
  //NameThreadForDebugging('DBMonitor Thread');
{$ENDIF}

  with FDBMonitor do begin
    while not Terminated do begin
      FNewMesEvent.WaitFor($FFFFFFFF);

      while FCacheHead <> FCacheTail do begin
        Msg := FEventCache[FCacheHead];

        OldActive := (FPacker <> nil) and FPacker.CheckActive;
        if Msg.EventType <> ET_APPFINISHED then
          Active := IsMonitorActive
        else
          Active := OldActive;

        if Active then begin
          if not OldActive and (Msg.EventType <> ET_APPSTARTED) then
            FPacker.WriteMessage(GetStartupMessage);

          FPacker.WriteMessage(Msg);

          if Msg.EventType = ET_APPFINISHED then
            FPacker.Close;
        end;

        if Msg <> FStartupMessage then
          Msg.Free;

        NewHead := FCacheHead + 1;
        if NewHead = EventCacheSize then
          NewHead := 0;
        FCacheHead := NewHead;
      end;
    end;
  end;
end;

function GetDBMonitor: TDBMonitor;
begin
  if DBMonitor = nil then
    DBMonitor := TDBMonitor.Create;

  Result := DBMonitor;
end;

{$IFDEF MSWINDOWS}
const
  sDBMonitorKey  = 'Software\Devart\DBMonitor';
{$ENDIF}

function HasMonitor: boolean;
begin
{$IFDEF MSWINDOWS}
  Result := FileExists(WhereMonitor);
{$ELSE}
  Result := False;
{$ENDIF}
end;

function WhereMonitor: string;
{$IFDEF MSWINDOWS}
var
  Reg: TRegistry;
{$ENDIF}
begin
{$IFDEF MSWINDOWS}
  Reg := TRegistry.Create(KEY_READ OR KEY_WRITE);
  try
    if Reg.OpenKey(sDBMonitorKey, False) then
      Result := Reg.ReadString('Self');
  finally
    Reg.Free;
  end;
{$ELSE}
  Result := '';
{$ENDIF}
end;

{$IFDEF MSWINDOWS}
function GetDBMonitorVersion: string;
var
  Reg: TRegistry;
begin
  Reg := TRegistry.Create(KEY_READ OR KEY_WRITE);
  try
    if Reg.OpenKey(sDBMonitorKey, False) then
      Result := Reg.ReadString('Version');
  finally
    Reg.Free;
  end;
  if Result = '' then
    Result := 'not available';
end;
{$ENDIF}

initialization

finalization
  DBMonitor.Free;
end.
