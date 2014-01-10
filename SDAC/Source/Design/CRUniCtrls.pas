{$I Dac.inc}

unit CRUniCtrls;

interface
uses
  SysUtils, Classes, Consts, Windows, Messages, WideStrings, Controls, StdCtrls, Contnrs, 
  CRTypes;

type
  TMemoWideStrings = class(TWideStrings)
  private
    FMemo: TCustomMemo;
    FMemoLines: TStrings;
  protected
    function Get(Index: Integer): WideString; override;
    function GetCount: Integer; override;
    function GetTextStr: WideString; override;
    procedure Put(Index: Integer; const S: WideString); override;
    procedure SetTextStr(const Value: WideString); override;
    procedure SetUpdateState(Updating: Boolean); override;
  public
    procedure Clear; override;
    procedure Delete(Index: Integer); override;
    procedure Insert(Index: Integer; const S: WideString); override;
  end;

  TCustomUniMemo = class(TCustomMemo)
  private
    FLines: TWideStrings;
    function GetText: WideString;
    procedure SetText(const Value: WideString);
  protected
    procedure CreateWindowHandle(const Params: TCreateParams); override;
    procedure SetLines(const Value: TWideStrings);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    property Text: WideString read GetText write SetText;
    property Lines: TWideStrings read FLines write SetLines;
  end;

  TUniMemo = class(TCustomUniMemo)
  published
    property Align;
    property Alignment;
    property Anchors;
    property BevelEdges;
    property BevelInner;
    property BevelKind default bkNone;
    property BevelOuter;
    property BiDiMode;
    property BorderStyle;
    property Color;
    property Constraints;
    property Ctl3D;
    property DragCursor;
    property DragKind;
    property DragMode;
    property Enabled;
    property Font;
    property HideSelection;
    property Hint;
    property ImeMode;
    property ImeName;
    property Lines;
    property MaxLength;
    property OEMConvert;
    property ParentBiDiMode;
    property ParentColor;
    property ParentCtl3D;
    property ParentFont;
    property ParentShowHint;
    property PopupMenu;
    property ReadOnly;
    property ScrollBars;
    property ShowHint;
    property TabOrder;
    property TabStop;
    property Visible;
    property WantReturns;
    property WantTabs;
    property WordWrap;
    property OnChange;
    property OnClick;
    property OnContextPopup;
    property OnDblClick;
    property OnDragDrop;
    property OnDragOver;
    property OnEndDock;
    property OnEndDrag;
    property OnEnter;
    property OnExit;
    property OnKeyDown;
    property OnKeyPress;
    property OnKeyUp;
    property OnMouseActivate;
    property OnMouseDown;
    property OnMouseEnter;
    property OnMouseLeave;
    property OnMouseMove;
    property OnMouseUp;
    property OnStartDock;
    property OnStartDrag;
  end;

implementation

const
  CRLF = WideString(#13#10);
  ANSI_UNICODE_HOLDER = $FF;

type
  TAccessStrings = class(TStrings);
  TAccessWinControl = class(TWinControl);

var
  WideControlHelpers: TComponentList = nil;
  PendingRecreateWndTrapList: TComponentList = nil;

{ TWideComponentHelper }

type
  TWideComponentHelper = class(TComponent)
  private
    FComponent: TComponent;
  protected
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
  public
    constructor Create(AOwner: TComponent); override;
    constructor CreateHelper(AOwner: TComponent; ComponentHelperList: TComponentList);
  end;

function CompareComponentHelperToTarget(Item, Target: Pointer): Integer;
begin
  if NativeInt(TWideComponentHelper(Item).FComponent) < NativeInt(Target) then
    Result := -1
  else if NativeInt(TWideComponentHelper(Item).FComponent) > NativeInt(Target) then
    Result := 1
  else
    Result := 0;
end;

type
  TListTargetCompare = function (Item, Target: Pointer): Integer;

function FindSortedListByTarget(List: TList; TargetCompare: TListTargetCompare;
  Target: Pointer; var Index: Integer): Boolean;
var
  L, H, I, C: Integer;
begin
  Result := False;
  L := 0;
  H := List.Count - 1;
  while L <= H do
  begin
    I := (L + H) shr 1;
    C := TargetCompare(List[i], Target);
    if C < 0 then L := I + 1 else
    begin
      H := I - 1;
      if C = 0 then
      begin
        Result := True;
        L := I;
      end;
    end;
  end;
  Index := L;
end;

function FindWideComponentHelperIndex(ComponentHelperList: TComponentList; Component: TComponent; var Index: Integer): Boolean;
begin
  // find Component in sorted wide caption list (list is sorted by TWideComponentHelper.FComponent)
  Result := FindSortedListByTarget(ComponentHelperList, CompareComponentHelperToTarget, Component, Index);
end;

constructor TWideComponentHelper.Create(AOwner: TComponent);
begin
  Assert(False);
end;

constructor TWideComponentHelper.CreateHelper(AOwner: TComponent; ComponentHelperList: TComponentList);
var
  Index: Integer;
begin
  // don't use direct ownership for memory management
  inherited Create(nil);
  FComponent := AOwner;
  FComponent.FreeNotification(Self);

  // insert into list according to sort
  FindWideComponentHelperIndex(ComponentHelperList, FComponent, Index);
  ComponentHelperList.Insert(Index, Self);
end;

procedure TWideComponentHelper.Notification(AComponent: TComponent; Operation: TOperation);
begin
  inherited;
  if (AComponent = FComponent) and (Operation = opRemove) then begin
    FComponent := nil;
    Free;
  end;
end;

function FindWideComponentHelper(ComponentHelperList: TComponentList; Component: TComponent): TWideComponentHelper;
var
  Index: integer;
begin
  if FindWideComponentHelperIndex(ComponentHelperList, Component, Index) then begin
  	Result := TWideComponentHelper(ComponentHelperList[Index]);
    Assert(Result.FComponent = Component);
  end else
    Result := nil;
end;

{ TWideControlHelper }

type
  TWideControlHelper = class(TWideComponentHelper)
  private
    FControl: TWinControl;
    FWideCaption: WideString;
    FWideHint: WideString;
  public
    constructor Create(AOwner: TWinControl); reintroduce;
    property WideCaption: WideString read FWideCaption;
    property WideHint: WideString read FWideHint;
  end;

constructor TWideControlHelper.Create(AOwner: TWinControl);
begin
  inherited CreateHelper(AOwner, WideControlHelpers);
  FControl := AOwner;
end;

function FindWideControlHelper(Control: TWinControl; CreateIfNotFound: Boolean = True): TWideControlHelper;
begin
  Result := TWideControlHelper(FindWideComponentHelper(WideControlHelpers, Control));
  if (Result = nil) and CreateIfNotFound then
  	Result := TWideControlHelper.Create(Control);
end;

{ Controls }

function TntControl_GetStoredText(Control: TWinControl; const Default: WideString): WideString;
var
  WideControlHelper: TWideControlHelper;
begin
  WideControlHelper := FindWideControlHelper(Control, False);
  if WideControlHelper <> nil then
    Result := WideControlHelper.WideCaption
  else
    Result := Default;
end;

procedure TntControl_SetStoredText(Control: TWinControl; const Value: WideString);
begin
  FindWideControlHelper(Control).FWideCaption := Value;
  TAccessWinControl(Control).Text := Value;
end;

function TntControl_GetText(Control: TWinControl): WideString;
begin
  if (not Control.HandleAllocated) then begin
    Result := TntControl_GetStoredText(Control, TAccessWinControl(Control).Text)
  end
  else begin
    SetLength(Result, GetWindowTextLengthW(Control.Handle) + 1);
    GetWindowTextW(Control.Handle, PWideChar(Result), Length(Result));
    SetLength(Result, Length(Result) - 1);
  end;
end;

procedure TntControl_SetText(Control: TWinControl; const Text: WideString);
begin
  if (not Control.HandleAllocated) then begin
    TntControl_SetStoredText(Control, Text);
  end
  else if TntControl_GetText(Control) <> Text then begin
    SetWindowTextW(Control.Handle, PWideChar(Text));
    Control.Perform(CM_TEXTCHANGED, 0, 0);
  end;
end;

function TntMemo_LineStart(Handle: THandle; Index: Integer): Integer;
begin
  Result := SendMessageW(Handle, EM_LINEINDEX, Index, 0);
end;

function TntMemo_LineLength(Handle: THandle; Index: Integer; StartPos: Integer = -1): Integer;
begin
  if StartPos = -1 then
    StartPos := TntMemo_LineStart(Handle, Index);
  if StartPos < 0 then
    Result := 0
  else
    Result := SendMessageW(Handle, EM_LINELENGTH, StartPos, 0);
end;

type
  TWinControlTrap = class(TComponent)
  private
    WinControl_ObjectInstance: Pointer;
    ObjectInstance: Pointer;
    DefObjectInstance: Pointer;
    function IsInSubclassChain(Control: TWinControl): Boolean;
    procedure SubClassWindowProc;
  private
    FControl: TAccessWinControl;
    Handle: THandle;
    PrevWin32Proc: Pointer;
    PrevDefWin32Proc: Pointer;
    PrevWindowProc: TWndMethod;
  private
    LastWin32Msg: UINT;
    Win32ProcLevel: Integer;
    IDEWindow: Boolean;
    DestroyTrap: Boolean;
    TestForNull: Boolean;
    FoundNull: Boolean;
    {$IFDEF TNT_VERIFY_WINDOWPROC}
    LastVerifiedWindowProc: TWndMethod;
    {$ENDIF}
    procedure Win32Proc(var Message: TMessage);
    procedure DefWin32Proc(var Message: TMessage);
    procedure WindowProc(var Message: TMessage);
  private
    procedure SubClassControl(Params_Caption: PAnsiChar);
    procedure UnSubClassUnicodeControl;
  protected
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  end;

constructor TWinControlTrap.Create(AOwner: TComponent);
begin
  FControl := TAccessWinControl(AOwner as TWinControl);
  inherited Create(nil);
  FControl.FreeNotification(Self);

  WinControl_ObjectInstance := Classes.MakeObjectInstance(FControl.MainWndProc);
  ObjectInstance := Classes.MakeObjectInstance(Win32Proc);
  DefObjectInstance := Classes.MakeObjectInstance(DefWin32Proc);
end;

destructor TWinControlTrap.Destroy;
begin
  Classes.FreeObjectInstance(ObjectInstance);
  Classes.FreeObjectInstance(DefObjectInstance);
  Classes.FreeObjectInstance(WinControl_ObjectInstance);
  inherited;
end;

procedure TWinControlTrap.Notification(AComponent: TComponent; Operation: TOperation);
begin
  inherited;
  if (AComponent = FControl) and (Operation = opRemove) then begin
    FControl := nil;
    if Win32ProcLevel = 0 then
      Free
    else
      DestroyTrap := True;
  end;
end;

procedure TWinControlTrap.SubClassWindowProc;
begin
  if not IsInSubclassChain(FControl) then begin
    PrevWindowProc := FControl.WindowProc;
    FControl.WindowProc := Self.WindowProc;
  end;
  {$IFDEF TNT_VERIFY_WINDOWPROC}
  LastVerifiedWindowProc := FControl.WindowProc;
  {$ENDIF}
end;

procedure TWinControlTrap.SubClassControl(Params_Caption: PAnsiChar);
begin
  // initialize trap object
  Handle := FControl.Handle;
  PrevWin32Proc := Pointer(GetWindowLongW(FControl.Handle, GWL_WNDPROC));
  PrevDefWin32Proc := FControl.DefWndProc;

  // subclass Window Procedures
  SetWindowLongW(FControl.Handle, GWL_WNDPROC, NativeInt(ObjectInstance));
  FControl.DefWndProc := DefObjectInstance;
  SubClassWindowProc;

  // For some reason, caption gets garbled after calling SetWindowLongW(.., GWL_WNDPROC).
  TntControl_SetText(FControl, TntControl_GetStoredText(FControl, Params_Caption));
end;

function SameWndMethod(A, B: TWndMethod): Boolean;
begin
  Result := @A = @B;
end;

procedure TWinControlTrap.UnSubClassUnicodeControl;
begin
  // remember caption for future window creation
  if not (csDestroying in FControl.ComponentState) then
    TntControl_SetStoredText(FControl, TntControl_GetText(FControl));

  // restore window procs (restore WindowProc only if we are still the direct subclass)
  if SameWndMethod(FControl.WindowProc, Self.WindowProc) then
    FControl.WindowProc := PrevWindowProc;
  TAccessWinControl(FControl).DefWndProc := PrevDefWin32Proc;
  SetWindowLongW(FControl.Handle, GWL_WNDPROC, NativeInt(PrevWin32Proc));

  if IDEWindow then
    DestroyTrap := True
  else if not (csDestroying in FControl.ComponentState) then
    // control not being destroyed, probably recreating window
    PendingRecreateWndTrapList.Add(Self);
end;

var
  Finalized: Boolean; { If any tnt controls are still around after finalization it must be due to a memory leak.
                        Windows will still try to send a WM_DESTROY, but we will just ignore it if we're finalized. }

procedure TWinControlTrap.Win32Proc(var Message: TMessage);
begin
  if (not Finalized) then begin
    Inc(Win32ProcLevel);
    try
      with Message do begin
        {$IFDEF TNT_VERIFY_WINDOWPROC}
        if not SameWndMethod(FControl.WindowProc, LastVerifiedWindowProc) then begin
          SubClassWindowProc;
          LastVerifiedWindowProc := FControl.WindowProc;
        end;
        {$ENDIF}
        LastWin32Msg := Msg;
        Result := CallWindowProcW(PrevWin32Proc, Handle, Msg, wParam, lParam);
      end;
    finally
      Dec(Win32ProcLevel);
    end;
    if (Win32ProcLevel = 0) and (DestroyTrap) then
      Free;
  end else if (Message.Msg = WM_DESTROY) then
    FControl.WindowHandle := 0
end;

function IsTextMessage(Msg: UINT): Boolean;
begin
  // WM_CHAR is omitted because of the special handling it receives
  Result := (Msg = WM_SETTEXT)
         or (Msg = WM_GETTEXT)
         or (Msg = WM_GETTEXTLENGTH);
end;

procedure MakeWMCharMsgSafeForAnsi(var Message: TMessage);
begin
  with TWMChar(Message) do begin
    Assert(Msg = WM_CHAR);
    Assert(Unused = 0);
    if (CharCode > Word(High(AnsiChar))) then begin
      Unused := CharCode;
      CharCode := ANSI_UNICODE_HOLDER;
    end;
  end;
end;

procedure RestoreWMCharMsg(var Message: TMessage);
begin
  with TWMChar(Message) do begin
    Assert(Message.Msg = WM_CHAR);
    if (Unused > 0)
    and (CharCode = ANSI_UNICODE_HOLDER) then
      CharCode := Unused;
    Unused := 0;
  end;
end;

procedure TWinControlTrap.DefWin32Proc(var Message: TMessage);
begin
  with Message do begin
    if Msg = WM_NOTIFYFORMAT then
      Result := NFR_UNICODE
    else begin
      if (Msg = WM_CHAR) then begin
        RestoreWMCharMsg(Message)
      end
      else
      if (Msg = WM_DESTROY) then begin
        UnSubClassUnicodeControl; {The reason for doing this in DefWin32Proc is because in D9, TWinControl.WMDestroy() does a perform(WM_TEXT) operation. }
      end;
      { Normal DefWindowProc }
      Result := CallWindowProcW(PrevDefWin32Proc, Handle, Msg, wParam, lParam);
    end;
  end;
end;

function TWinControlTrap.IsInSubclassChain(Control: TWinControl): Boolean;
var
  Message: TMessage;
begin
  if SameWndMethod(Control.WindowProc, TAccessWinControl(Control).WndProc) then
    Result := False { no subclassing }
  else if SameWndMethod(Control.WindowProc, Self.WindowProc) then
    Result := True { directly subclassed }
  else begin
    TestForNull := True;
    FoundNull := False;
    ZeroMemory(@Message, SizeOf(Message));
    Message.Msg := WM_NULL;
    Control.WindowProc(Message);
    Result := FoundNull; { indirectly subclassed }
  end;
end;

procedure TWinControlTrap.WindowProc(var Message: TMessage);
var
  CameFromWindows: Boolean;
begin
  if TestForNull and (Message.Msg = WM_NULL) then
    FoundNull := True;

  if (not FControl.HandleAllocated) then
    FControl.WndProc(Message)
  else begin
    CameFromWindows := LastWin32Msg <> WM_NULL;
    LastWin32Msg := WM_NULL;
    with Message do begin
      if (not CameFromWindows)
      and (IsTextMessage(Msg)) then
        Result := SendMessageA(Handle, Msg, wParam, lParam)
      else begin
        if (Msg = WM_CHAR) then begin
          MakeWMCharMsgSafeForAnsi(Message);
        end;
        PrevWindowProc(Message)
      end;
    end;
  end;
end;

//----------------------------------------------------------------------------------

function FindOrCreateWinControlTrap(Control: TWinControl): TWinControlTrap;
var
  i: integer;
begin
  // find or create trap object
  Result := nil;
  for i := PendingRecreateWndTrapList.Count - 1 downto 0 do begin
    if TWinControlTrap(PendingRecreateWndTrapList[i]).FControl = Control then begin
      Result := TWinControlTrap(PendingRecreateWndTrapList[i]);
      PendingRecreateWndTrapList.Delete(i);
      break; { found it }
    end;
  end;
  if Result = nil then
    Result := TWinControlTrap.Create(Control);
end;

procedure SubClassUnicodeControl(Control: TWinControl; Params_Caption: PAnsiChar; IDEWindow: Boolean = False);
var
  WinControlTrap: TWinControlTrap;
begin
  Assert(IsWindowUnicode(Control.Handle));

  WinControlTrap := FindOrCreateWinControlTrap(Control);
  WinControlTrap.SubClassControl(Params_Caption);
  WinControlTrap.IDEWindow := IDEWindow;
end;

procedure RegisterUnicodeClass(Params: TCreateParams; out WideWinClassName: WideString);
var
  TempClass: TWndClassW;
  WideClass: TWndClassW;
  ClassRegistered: Boolean;
begin
  with Params do begin
    WideWinClassName := WinClassName;
    ClassRegistered := GetClassInfoW(hInstance, PWideChar(WideWinClassName), TempClass);
    if (not ClassRegistered) or (TempClass.lpfnWndProc <> @InitWndProc)
    then begin
      if ClassRegistered then Win32Check(Windows.UnregisterClassW(PWideChar(WideWinClassName), hInstance));
      // Prepare a TWndClassW record
      WideClass := TWndClassW(WindowClass);
      WideClass.hInstance := hInstance;
      WideClass.lpfnWndProc := @InitWndProc;
      WideClass.lpszClassName := PWideChar(WideWinClassName);

      // Register the UNICODE class
      if RegisterClassW(WideClass) = 0 then RaiseLastOSError;
    end;
  end;
end;

procedure CreateUnicodeHandle(Control: TWinControl; const Params: TCreateParams;
  const SubClass: WideString);
var
  TempSubClass: TWndClassW;
  WideWinClassName: WideString;
  Handle: THandle;
begin
  // SubClass the unicode version of this control by getting the correct DefWndProc
  if (SubClass <> '')
  and GetClassInfoW(Params.WindowClass.hInstance, PWideChar(SubClass), TempSubClass) then
    TAccessWinControl(Control).DefWndProc := TempSubClass.lpfnWndProc
  else
    TAccessWinControl(Control).DefWndProc := @DefWindowProcW;

  // make sure Unicode window class is registered
  RegisterUnicodeClass(Params, WideWinClassName);

  // Create UNICODE window handle
  with Params do
    Handle := CreateWindowExW(ExStyle, PWideChar(WideWinClassName), nil,
      Style, X, Y, Width, Height, WndParent, 0, hInstance, Param);
  if Handle = 0 then
    RaiseLastOSError;
  TAccessWinControl(Control).WindowHandle := Handle;

  SubClassUnicodeControl(Control, Params.Caption);
end;

{ TMemoWideStrings }

function TMemoWideStrings.GetCount: Integer;
begin
  Result := FMemoLines.Count;
end;

function TMemoWideStrings.Get(Index: Integer): WideString;
var
  Len: Integer;
begin
  SetLength(Result, TntMemo_LineLength(FMemo.Handle, Index));
  if Length(Result) > 0 then begin
    if Length(Result) > High(Word) then
      raise EOutOfResources.Create(SOutlineLongLine);
    Word((PWideChar(Result))^) := Length(Result);
    Len := SendMessageW(FMemo.Handle, EM_GETLINE, Index, Longint(PWideChar(Result)));
    SetLength(Result, Len);
  end;
end;

procedure TMemoWideStrings.Put(Index: Integer; const S: WideString);
var
  StartPos: Integer;
begin
  StartPos := TntMemo_LineStart(FMemo.Handle, Index);
  if StartPos >= 0 then
  begin
    SendMessageW(FMemo.Handle, EM_SETSEL, StartPos, StartPos + TntMemo_LineLength(FMemo.Handle, Index));
    SendMessageW(FMemo.Handle, EM_REPLACESEL, 0, Longint(PWideChar(S)));
  end;
end;

procedure TMemoWideStrings.Insert(Index: Integer; const S: Widestring);
var
  StartPos, LineLen: Integer;
  Line: WideString;
begin
  if Index >= 0 then
  begin
    StartPos := TntMemo_LineStart(FMemo.Handle, Index);
    if StartPos >= 0 then
      Line := S + CRLF
    else begin
      StartPos := TntMemo_LineStart(FMemo.Handle, Index - 1);
      LineLen := TntMemo_LineLength(FMemo.Handle, Index - 1);
      if LineLen = 0 then
        Exit;
      Inc(StartPos, LineLen);
      Line := CRLF + s;
    end;
    SendMessageW(FMemo.Handle, EM_SETSEL, StartPos, StartPos);
    SendMessageW(FMemo.Handle, EM_REPLACESEL, 0, Longint(PWideChar(Line)));
  end;
end;

procedure TMemoWideStrings.Delete(Index: Integer);
begin
  FMemoLines.Delete(Index);
end;

procedure TMemoWideStrings.Clear;
begin
  FMemoLines.Clear;
end;

procedure TMemoWideStrings.SetUpdateState(Updating: Boolean);
begin
  TAccessStrings(FMemoLines).SetUpdateState(Updating);
end;

function TMemoWideStrings.GetTextStr: WideString;
begin
  Result := TntControl_GetText(FMemo)
end;

procedure TMemoWideStrings.SetTextStr(const Value: WideString);
begin
  if Value <> GetTextStr then begin
    TntControl_SetText(FMemo, Value);
  end;
end;

{ TCustomUniMemo }

constructor TCustomUniMemo.Create(AOwner: TComponent);
begin
  inherited;

  FLines := TMemoWideStrings.Create;
  TMemoWideStrings(FLines).FMemo := Self;
  TMemoWideStrings(FLines).FMemoLines := TCustomMemo(Self).Lines;
end;

destructor TCustomUniMemo.Destroy;
begin
  FLines.Free;

  inherited;
end;

procedure TCustomUniMemo.SetLines(const Value: TWideStrings);
begin
  FLines.Assign(Value);
end;

procedure TCustomUniMemo.CreateWindowHandle(const Params: TCreateParams);
begin
  CreateUnicodeHandle(Self, Params, 'EDIT');
end;

function TCustomUniMemo.GetText: WideString;
begin
  Result := FLines.Text;
end;

procedure TCustomUniMemo.SetText(const Value: WideString);
begin
  FLines.Text := Value;
end;

initialization
  WideControlHelpers := TComponentList.Create(True);
  PendingRecreateWndTrapList := TComponentList.Create(False);

finalization
  FreeAndNil(WideControlHelpers);
  FreeAndNil(PendingRecreateWndTrapList);

end.
