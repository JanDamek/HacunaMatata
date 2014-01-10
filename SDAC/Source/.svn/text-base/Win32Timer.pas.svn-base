
//////////////////////////////////////////////////
//  DB Access Components
//  Copyright © 1998-2012 Devart. All right reserved.
//  SQLMonitor supports
//  Created:            17.11.99
//////////////////////////////////////////////////

{$IFNDEF CLR}

{$I Dac.inc}

unit Win32Timer;
{$ENDIF}

interface

uses
  Classes,
{$IFDEF WIN32_64}
  Windows, Messages,
{$ENDIF}
{$IFDEF POSIX}
  FMX.Types,
{$ENDIF}
{$IFDEF CLR}
  ExtCtrls,
{$ENDIF}
  MemData, DAConsts;

type
{$IFDEF WIN32_64}
  TWin32Timer = class(TComponent)
  private
    FInterval: Cardinal;
    FWindowHandle: HWND;
    FOnTimer: TNotifyEvent;
    FEnabled: Boolean;
    procedure UpdateTimer;
    procedure SetEnabled(Value: Boolean);
    procedure SetInterval(Value: Cardinal);
    procedure SetOnTimer(Value: TNotifyEvent);
  protected
    procedure Timer; dynamic;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  published
    property Enabled: Boolean read FEnabled write SetEnabled default True;
    property Interval: Cardinal read FInterval write SetInterval default 1000;
    property OnTimer: TNotifyEvent read FOnTimer write SetOnTimer;
  end;
{$ELSE}
  TWin32Timer = TTimer;
{$ENDIF}

implementation

{$IFDEF WIN32_64}
function TimerWndProc(Window: HWND; Message, WParam: Longint;
  LParam: Longint): Longint; stdcall;
var
  Timer: TWin32Timer;
begin
  Result := 1;
  Timer := TWin32Timer(GetWindowLong(Window, GWL_USERDATA));
  if Message = WM_TIMER then
    try
      Timer.Timer;
    except
      ApplicationHandleException(Timer);
    end
  else
    Result := DefWindowProc(Window, Message, WParam, LParam);
end;

var
  AlerterWindowClass: TWndClass = (
    style: 0;
    lpfnWndProc: @TimerWndProc;
    cbClsExtra: 0;
    cbWndExtra: 0;
    hInstance: 0;
    hIcon: 0;
    hCursor: 0;
    hbrBackground: 0;
    lpszMenuName: nil;
    lpszClassName: 'TOraAlerterWindow');

{ TWin32Timer }

constructor TWin32Timer.Create(AOwner: TComponent);
var
  TempClass: TWndClass;
  ClassRegistered: Boolean;
begin
  inherited Create(AOwner);
  FEnabled := True;
  FInterval := 1000;

  // allocate timer window
  AlerterWindowClass.hInstance := HInstance;
  ClassRegistered := GetClassInfo(HInstance, AlerterWindowClass.lpszClassName,
    TempClass);
  if not ClassRegistered or ({$IFDEF FPC}@{$ENDIF}TempClass.lpfnWndProc <> @TimerWndProc) then
  begin
    if ClassRegistered then
      Windows.UnregisterClass(AlerterWindowClass.lpszClassName, HInstance);
    Windows.RegisterClass(AlerterWindowClass);
  end;
  FWindowHandle := CreateWindowEx(WS_EX_TOOLWINDOW, AlerterWindowClass.lpszClassName,
    '', WS_POPUP {!0}, 0, 0, 0, 0, 0, 0, HInstance, nil);

  // pass Self to window
  SetWindowLong(FWindowHandle, GWL_USERDATA, Longint(Self));
end;

destructor TWin32Timer.Destroy;
begin
  FEnabled := False;
  UpdateTimer;
  DestroyWindow(FWindowHandle);
  inherited Destroy;
end;

procedure TWin32Timer.UpdateTimer;
begin
  KillTimer(FWindowHandle, 1);
  if (FInterval <> 0) and FEnabled and Assigned(FOnTimer) then
    if SetTimer(FWindowHandle, 1, FInterval, nil) = 0 then
      raise EOutOfResources.Create(SNoTimers);
end;

procedure TWin32Timer.SetEnabled(Value: Boolean);
begin
  if Value <> FEnabled then
  begin
    FEnabled := Value;
    UpdateTimer;
  end;
end;

procedure TWin32Timer.SetInterval(Value: Cardinal);
begin
  if Value <> FInterval then
  begin
    FInterval := Value;
    UpdateTimer;
  end;
end;

procedure TWin32Timer.SetOnTimer(Value: TNotifyEvent);
begin
  FOnTimer := Value;
  UpdateTimer;
end;

procedure TWin32Timer.Timer;
begin
  if Assigned(FOnTimer) then FOnTimer(Self);
end;
{$ENDIF}

end.
