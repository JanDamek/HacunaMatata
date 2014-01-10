
//////////////////////////////////////////////////
//  DB Access Components
//  Copyright © 1998-2012 Devart. All right reserved.
//////////////////////////////////////////////////

{$IFNDEF CLR}
{$IFNDEF UNIDACPRO}

{$I Dac.inc}

unit CRThread;
{$ENDIF}
{$ENDIF}
interface
uses
  Classes, SysUtils, SyncObjs,
{$IFDEF MSWINDOWS}
  Windows,
{$ENDIF}
{$IFDEF CLR}
  Messages, WinUtils, System.Runtime.InteropServices;
{$ELSE}
  CLRClasses;
{$ENDIF}

type
  TCRThread = class;
  TCRThreadWrapper = class;

  TCRThreadTerminateEvent = procedure(Sender: TObject) of object;
  TCRThreadExceptionEvent = procedure(Sender: TObject; E: Exception; var Fail: boolean) of object;
  TCRThreadEvent = procedure(Sender: TObject; Event: TObject) of object;
  TCRThreadClass = class of TCRThread;

  TCRThread = class(TThread)
  protected
    FOwner: TCRThreadWrapper;
    FStartEvent: TEvent;

    procedure InternalExecute; virtual;
    procedure Execute; override;
  public
    procedure PostEvent(Event: TObject);
    procedure SendEvent(Event: TObject);

    constructor Create(Owner: TCRThreadWrapper); virtual;
    destructor Destroy; override;
  end;

  TCRThreadState = (tsSuspended, tsExecuting, tsTerminating, tsFinished);

  TCRThreadWrapper = class
  protected
    FTimerID: UInt;
    FThread: TCRThread;

    FOnPostEvent: TCRThreadEvent;
    FOnSendEvent: TCRThreadEvent;
    FOnException: TCRThreadExceptionEvent;
    FOnTerminate: TCRThreadTerminateEvent;

    FEvents: TThreadList;
    FException: Exception;
    FThreadState: TCRThreadState;
    FFreeOnTerminate: boolean;
    FDoTimerProcessing, FLockDestroy: boolean;
    FDestroyAfterTimer: boolean;
    FSendEvent: TObject;
    FSendEventProcessed: TEvent;

  {$IFDEF CLR}
    FWindowHandle: HWND;
    procedure WndProc(var Msg: TMessage);
  {$ENDIF}

    procedure SetTimer;
    procedure KillTimer;
    procedure DoTimer;

    procedure DoPostEvent(Event: TObject); virtual; // In FThread context
    procedure DoSendEvent(Event: TObject); virtual; // In FThread context

    procedure DoException(E: Exception); virtual; // In FThread context
    procedure DoTerminate; // In FThread context

  public
    constructor Create(ThreadClass: TCRThreadClass; CreateSuspended: Boolean);
    destructor Destroy; override;

    procedure Resume;
    // procedure Suspend;
    procedure Terminate;
    procedure WaitFor;

    property Thread: TCRThread read FThread;
    property FreeOnTerminate: boolean read FFreeOnTerminate write FFreeOnTerminate;
    property ThreadState: TCRThreadState read FThreadState;

    property OnPostEvent: TCRThreadEvent read FOnPostEvent write FOnPostEvent;
    property OnSendEvent: TCRThreadEvent read FOnSendEvent write FOnSendEvent;
    property OnException: TCRThreadExceptionEvent read FOnException write FOnException;
    property OnTerminate: TCRThreadTerminateEvent read FOnTerminate write FOnTerminate;
  end;

implementation

uses
{$IFDEF CLR}
  System.Collections,
{$ENDIF}
  MemData{$IFDEF VER6P}, StrUtils{$ENDIF};

var
  ThreadList: TThreadList;

{ TCRThread }

constructor TCRThread.Create(Owner: TCRThreadWrapper);
begin
  inherited Create(True);
  Assert(Owner <> nil);
  FOwner := Owner;
  FStartEvent := TEvent.Create(nil, True, False, '');
end;

destructor TCRThread.Destroy;
begin
  inherited;
  FStartEvent.Free;
end;

procedure TCRThread.InternalExecute;
begin
  // Empty
end;

procedure TCRThread.Execute;
begin
  try
    try
      FStartEvent.SetEvent;
      InternalExecute;
    except
      on E: Exception do
        if not (E is EAbort) then
          FOwner.DoException(E);
    end;
  finally
    FOwner.DoTerminate;
  end;
end;

procedure TCRThread.PostEvent(Event: TObject);
begin
  FOwner.DoPostEvent(Event);
end;

procedure TCRThread.SendEvent(Event: TObject);
begin
  if not Terminated then
    FOwner.DoSendEvent(Event);
end;

{ TCRThreadWrapper }

{$IFNDEF CLR}
procedure TimerCallBack(hWnd: HWND; Message: UInt;
{$IFDEF CLR}
  {$IFDEF VER11P}
    TimerID: UINT_PTR;
  {$ELSE}
    TimerID: UInt;
  {$ENDIF}
{$ELSE}
  TimerID: UInt;
{$ENDIF}
  SysTime: DWORD); {$IFNDEF CLR} stdcall; {$ENDIF}
var
  List: TList;
  p: TCRThreadWrapper;
  ThreadWrapper: TCRThreadWrapper;
  i: integer;
begin
  List := ThreadList.LockList;
  try
    ThreadWrapper := nil;
    for i := 0 to List.Count - 1 do begin
      p := TCRThreadWrapper(List[i]);
      if TCRThreadWrapper(p).FTimerID = TimerID then begin
        ThreadWrapper := TCRThreadWrapper(p);
        Break;
      end;
    end;
  finally
    ThreadList.UnlockList;
  end;

  if ThreadWrapper <> nil then
    ThreadWrapper.DoTimer;
end;
{$ENDIF}

{$IFDEF CLR}
{$IFNDEF VER11P}
// Copied from d11 to avoid d8..d10clr bug 
const
  UtilClassName = 'TPUtilWindow'; // do not localize

var
  DefWindowProcDelegate: TFNWNdProc;

  UtilWindowClass: TWndClass = (
    style: 0;
    lpfnWndProc: nil;
    cbClsExtra: 0;
    cbWndExtra: 0;
    hInstance: 0;
    hIcon: 0;
    hCursor: 0;
    hbrBackground: 0;
    lpszMenuName: '';
    lpszClassName: UtilClassName);
  Instances: Hashtable;

function AllocateHWnd(Method: TWndMethod): HWND;
var
  TempClassInfo: TWndClassInfo;
  ClassRegistered: Boolean;
  Instance: TFNWndProc;
begin
  UtilWindowClass.lpfnWndProc := DefWindowProcDelegate;
  UtilWindowClass.hInstance := HInstance;
  UtilWindowClass.lpszClassName := Format('%s.%d',
    [UtilClassName, AppDomain.CurrentDomain.GetHashCode]);
  ClassRegistered := GetClassInfo(HInstance, UtilWindowClass.lpszClassName,
    TempClassInfo);
  if not ClassRegistered {or (TempClass.lpfnWndProc <> @DefWindowProc)} then
    RegisterClass(UtilWindowClass);
  Result := CreateWindowEx(WS_EX_TOOLWINDOW, UtilWindowClass.lpszClassName,
    '', WS_POPUP {+ 0}, 0, 0, 0, 0, 0, 0, HInstance, nil);
  if Assigned(Method) then
  begin
    Instance := MakeObjectInstance(Method);
    if not Assigned(Instances) then
      Instances := Hashtable.Create;
    Instances[TObject(Result)] := @Instance;
    SetWindowLong(Result, GWL_WNDPROC, @Instance);
  end;
end;
{$ENDIF}
{$ENDIF}

constructor TCRThreadWrapper.Create(ThreadClass: TCRThreadClass; CreateSuspended: Boolean);
begin
  inherited Create;

  FThreadState := tsSuspended;
  FEvents := TThreadList.Create;
  FSendEventProcessed := TEvent.Create(nil, True, False, '');

{$IFDEF CLR}
  FWindowHandle := AllocateHWnd(WndProc);
  Assert(FWindowHandle <> 0);
{$ENDIF}

  SetTimer;
  ThreadList.Add(Self);

  FThread := ThreadClass.Create(Self);
  if not CreateSuspended then
    Resume;
end;

destructor TCRThreadWrapper.Destroy;
begin
  if FLockDestroy then
    Exit;

  if FDoTimerProcessing then begin
    FDestroyAfterTimer := True;
    Exit;
  end;

  if not FFreeOnTerminate then begin
    FLockDestroy := True;
    Terminate;
  end;
  FThread.Free;
  FThread := nil;
  FEvents.Free;
  FEvents := nil;

  KillTimer;
  ThreadList.Remove(Self);

{$IFDEF CLR}
  WinUtils.DeallocateHWnd(FWindowHandle);
  FWindowHandle := 0;
{$ENDIF}

  inherited;
  FSendEventProcessed.Free;
end;

procedure TCRThreadWrapper.Resume;
begin
  if (ThreadState <> tsSuspended){ and FFreeOnTerminate }then
    Exit;

  FThreadState := tsExecuting;
  Thread.Resume;
{$IFDEF FPC}
  Thread.FStartEvent.WaitFor(INFINITE);
{$ELSE}
  WaitForSingleObject(THandle(Thread.FStartEvent.Handle), INFINITE);
{$ENDIF}
end;

{procedure TCRThreadWrapper.Suspend;
begin
  FThreadState := tsSuspended;
  Thread.Suspend;
end;}

procedure TCRThreadWrapper.Terminate;
begin
  case FThreadState of
    tsSuspended, tsTerminating:
      Assert(False);
    tsExecuting: begin
      FThreadState := tsTerminating;
    {$IFDEF FPC}
      Thread.FStartEvent.WaitFor(INFINITE);
    {$ELSE}
      WaitForSingleObject(THandle(Thread.FStartEvent.Handle), INFINITE);
    {$ENDIF}
      Thread.Terminate;
      FSendEventProcessed.SetEvent;
      WaitFor;
    end;
    tsFinished:
      DoTimer;
  end;
end;

procedure TCRThreadWrapper.DoPostEvent(Event: TObject); // In FThread context
begin
  Assert(FEvents <> nil);
  FEvents.Add(Event);
end;

procedure TCRThreadWrapper.DoSendEvent(Event: TObject); // In FThread context
begin
  FSendEvent := Event;
{$IFDEF FPC}
  FSendEventProcessed.WaitFor(INFINITE);
{$ELSE}
  WaitForSingleObject(THandle(FSendEventProcessed.Handle), INFINITE);
{$ENDIF}
  FSendEventProcessed.ResetEvent;
end;

procedure TCRThreadWrapper.DoException(E: Exception); // In FThread context
begin
  Assert(FException = nil);
  FException := Exception.Create(E.Message);
end;

procedure TCRThreadWrapper.DoTerminate; // In FThread context
begin
  Assert(FThreadState <> tsFinished);
  FThreadState := tsFinished;
end;

procedure TCRThreadWrapper.SetTimer;
const
  USER_TIMER_MINIMUM = $A;
{$IFNDEF CLR}
var
  TimerFunc: {$IFNDEF FPC}TFNTimerProc{$ELSE}TIMERPROC{$ENDIF};
{$ENDIF}
begin
  Assert(FTimerID = 0);
{$IFDEF CLR}
  FTimerID := Windows.SetTimer(FWindowHandle, 1, USER_TIMER_MINIMUM, nil);
{$ELSE}
  TimerFunc := {$IFNDEF CLR}@{$ENDIF}TimerCallBack;
  FTimerID := Windows.SetTimer(0, 0, USER_TIMER_MINIMUM, TimerFunc);
{$ENDIF}

  Win32Check(FTimerID <> 0);
end;

procedure TCRThreadWrapper.KillTimer;
begin
  if FTimerID = 0 then
    Exit;
{$IFDEF CLR}
  Windows.KillTimer(FWindowHandle, 1);
{$ELSE}
  Windows.KillTimer(0, FTimerID);
{$ENDIF}
  FTimerID := 0;
end;

{$IFDEF CLR}
procedure TCRThreadWrapper.WndProc(var Msg: TMessage);
begin
  with Msg do
    if Msg = WM_TIMER then
      try
        DoTimer;
      except
        on E: Exception do
          if Assigned(ApplicationHandleException) then
            ApplicationHandleException(E)
          else
            ShowException(E, ExceptAddr);
      end               
    else
      Result := DefWindowProc(FWindowHandle, Msg, wParam, lParam);
end;
{$ENDIF}

procedure TCRThreadWrapper.DoTimer; // In main thread context
var
  Fail: boolean;
  List: TList;
  FreeThread: boolean;
begin
  if FDoTimerProcessing then
    Exit; // For example - on showing error message
  FDoTimerProcessing := True;

  KillTimer; // To prevent multiple calls to DoTimer if any event handler call ProcessMessage
  try
    if (FSendEvent <> nil) then begin
      if Assigned(FOnSendEvent) then
        FOnSendEvent(Self, FSendEvent);
      FSendEvent := nil;
      FSendEventProcessed.SetEvent;
    end;

    if FEvents <> nil then begin
      List := FEvents.LockList;
      try
        while List.Count > 0 do begin
          try
            try
              if Assigned(FOnPostEvent) then
                FOnPostEvent(Self, TObject(List[0]));
            except
              on E: Exception do
                if Assigned(ApplicationHandleException) then
                  ApplicationHandleException(E)
                else
                  ShowException(E, ExceptAddr);
            end;
          finally
            List.Delete(0);
          end;
        end;
      finally
        FEvents.UnlockList;
      end;
    end;

    if FException <> nil then begin
      try
        Fail := True;
        if Assigned(FOnException) then
          FOnException(Self, FException, Fail);

        if (ThreadState = tsFinished) and Assigned(FOnTerminate) then
          FOnTerminate(Self);

        if Fail then begin
          try
            raise FException;
          except
            if Assigned(ApplicationHandleException) then
              ApplicationHandleException(FException)
            else
              ShowException(FException, ExceptAddr);
          end;
        end
        else
          FException.Free;
      finally
        FException := nil;
      end;
    end
    else
      if (ThreadState = tsFinished) and Assigned(FOnTerminate) then
        FOnTerminate(Self);
  finally
    FDoTimerProcessing := False;
    FreeThread := ((ThreadState = tsFinished) and FFreeOnTerminate) or FDestroyAfterTimer;
    if FreeThread then
      Free
    else
      SetTimer;
  end;
end;

procedure TCRThreadWrapper.WaitFor;
begin
  if (ThreadState = tsExecuting) or (ThreadState = tsTerminating) then begin
  {$IFNDEF CLR}
    WaitForSingleObject(Thread.Handle, INFINITE);
  {$ELSE}
    Thread.WaitFor;
  {$ENDIF}
  end;
  DoTimer;
end;

initialization
  ThreadList := TThreadList.Create;

finalization
  ThreadList.Free;

end.
