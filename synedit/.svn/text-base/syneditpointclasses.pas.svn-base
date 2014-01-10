{ -------------------------------------------------------------------------------
  The contents of this file are subject to the Mozilla Public License
  Version 1.1 (the "License"); you may not use this file except in compliance
  with the License. You may obtain a copy of the License at
  http://www.mozilla.org/MPL/

  Software distributed under the License is distributed on an "AS IS" basis,
  WITHOUT WARRANTY OF ANY KIND, either express or implied. See the License for
  the specific language governing rights and limitations under the License.

  Alternatively, the contents of this file may be used under the terms of the
  GNU General Public License Version 2 or later (the "GPL"), in which case
  the provisions of the GPL are applicable instead of those above.
  If you wish to allow use of your version of this file only under the terms
  of the GPL and not to allow others to use your version of this file
  under the MPL, indicate your decision by deleting the provisions above and
  replace them with the notice and other provisions required by the GPL.
  If you do not delete the provisions above, a recipient may use your version
  of this file under either the MPL or the GPL.

  ------------------------------------------------------------------------------- }

(* Naming Conventions:
  Byte = Logical: Refers to the location any TextToken has in the String.
  In Utf8String some TextToken can have more than one byte
  Char = Physical: Refers to the (x-)location on the screen matrix.
  Some TextToken (like tab) can spawn multiply char locations
*)

unit SynEditPointClasses;

{$I synedit.inc}
{ off $DEFINE SynCaretDebug }

interface

uses
  System.Classes, System.SysUtils, System.UITypes, System.UIConsts, LazMethodList, System.Types,
  LazSynEditText, SynEditTypes, SynEditMiscProcs;

const
  LineEnding = #13#10;

type

  TInvalidateLines = procedure(FirstLine, LastLine: integer) of Object;
  TLinesCountChanged = procedure(FirstLine, Count: integer) of Object;
  TMaxLeftCharFunc = function: integer of object;

  { TSynEditPointBase }

  TSynEditPointBase = class
  private
    function GetLocked: Boolean;
  protected
    FLines: TSynEditStrings;
    FOnChangeList: TMethodList;
    FLockCount: integer;
    procedure SetLines(const AValue: TSynEditStrings); virtual;
    procedure DoLock; virtual;
    Procedure DoUnlock; virtual;
  public
    constructor Create; overload;
    constructor Create(Lines: TSynEditStrings); overload;
    destructor Destroy; override;
    procedure AddChangeHandler(AHandler: TNotifyEvent);
    procedure RemoveChangeHandler(AHandler: TNotifyEvent);
    procedure Lock;
    Procedure Unlock;
    property Lines: TSynEditStrings read FLines write SetLines;
    property Locked: Boolean read GetLocked;
  end;

  TSynEditCaret = class;

  { TSynEditSelection }

  TSynEditSelection = class(TSynEditPointBase)
  private
    FAutoExtend: Boolean;
    FCaret: TSynEditCaret;
    FHide: Boolean;
    FInternalCaret: TSynEditCaret;
    FInvalidateLinesMethod: TInvalidateLines;
    FEnabled: Boolean;
    FHookedLines: Boolean;
    FIsSettingText: Boolean;
    FActiveSelectionMode: TSynSelectionMode;
    FSelectionMode: TSynSelectionMode;
    FStartLinePos: integer; // 1 based
    FStartBytePos: integer; // 1 based
    FEndLinePos: integer; // 1 based
    FEndBytePos: integer; // 1 based
    FPersistent: Boolean;
    FPersistentLock: integer;
    FIgnoreNextCaretMove: Boolean;
    (* On any modification, remember the position of the caret.
      If it gets moved from there to either end of the block, this should be ignored
      This happens, if Block and caret are adjusted directly
    *)
    FLastCarePos: TPoint;
    function AdjustBytePosToCharacterStart(Line: integer; BytePos: integer): integer;
    function GetFirstLineBytePos: TPoint;
    function GetLastLineBytePos: TPoint;
    procedure SetCaret(const AValue: TSynEditCaret);
    procedure SetEnabled(const Value: Boolean);
    procedure SetActiveSelectionMode(const Value: TSynSelectionMode);
    procedure SetHide(const AValue: Boolean);
    procedure SetPersistent(const AValue: Boolean);
    procedure SetSelectionMode(const AValue: TSynSelectionMode);
    function GetStartLineBytePos: TPoint;
    procedure SetStartLineBytePos(Value: TPoint);
    procedure AdjustStartLineBytePos(Value: TPoint);
    function GetEndLineBytePos: TPoint;
    procedure SetEndLineBytePos(Value: TPoint);
    function GetSelText: string;
    procedure SetSelText(const Value: string);
    procedure DoCaretChanged(Sender: TObject);
    procedure AdjustAfterTrimming; // TODO: Move into TrimView?
  protected
    procedure DoLock; override;
    procedure DoUnlock; override;
    Procedure LineChanged(Sender: TSynEditStrings; AIndex, ACount: integer);
    procedure DoLinesEdited(Sender: TSynEditStrings; aLinePos, aBytePos, ACount, aLineBrkCnt: integer; aText: String);
  public
    constructor Create(ALines: TSynEditStrings; aActOnLineChanges: Boolean);
    destructor Destroy; override;
    procedure AssignFrom(Src: TSynEditSelection);
    procedure SetSelTextPrimitive(PasteMode: TSynSelectionMode; Value: PChar; AReplace: Boolean = False);
    function SelAvail: Boolean;
    function SelCanContinue(ACaret: TSynEditCaret): Boolean;
    function IsBackwardSel: Boolean; // SelStart < SelEnd ?
    procedure SortSelectionPoints;
    procedure IgnoreNextCaretMove;
    procedure IncPersistentLock;
    procedure DecPersistentLock;
    procedure Clear;
    property Enabled: Boolean read FEnabled write SetEnabled;
    property ActiveSelectionMode: TSynSelectionMode read FActiveSelectionMode write SetActiveSelectionMode;
    property SelectionMode: TSynSelectionMode read FSelectionMode write SetSelectionMode;
    property SelText: String read GetSelText write SetSelText;
    // Start and End positions are in the order they where defined
    // This may mean Startpos is behind EndPos in the text
    property StartLineBytePos: TPoint read GetStartLineBytePos write SetStartLineBytePos;
    property StartLineBytePosAdjusted: TPoint write AdjustStartLineBytePos;
    property EndLineBytePos: TPoint read GetEndLineBytePos write SetEndLineBytePos;
    property StartLinePos: integer read FStartLinePos;
    property EndLinePos: integer read FEndLinePos;
    property StartBytePos: integer read FStartBytePos;
    property EndBytePos: integer read FEndBytePos;
    // First and Last Pos are ordered according to the text flow (LTR)
    property FirstLineBytePos: TPoint read GetFirstLineBytePos;
    property LastLineBytePos: TPoint read GetLastLineBytePos;
    property InvalidateLinesMethod: TInvalidateLines write FInvalidateLinesMethod;
    property Caret: TSynEditCaret read FCaret write SetCaret;
    property Persistent: Boolean read FPersistent write SetPersistent;
    // automatically Start/Exctend selection if caret moves
    // (depends if caret was at block border or not)
    property AutoExtend: Boolean read FAutoExtend write FAutoExtend;
    property Hide: Boolean read FHide write SetHide;
  end;

  { TSynEditCaret }

  TSynEditCaret = class(TSynEditPointBase)
  private
    FAllowPastEOL: Boolean;
    FAutoMoveOnEdit: integer;
    FForcePastEOL: integer;
    FForceAdjustToNextChar: integer;
    FKeepCaretX: Boolean;
    FLinePos: integer; // 1 based
    FCharPos: integer; // 1 based
    FLastCharPos: integer; // used by KeepCaretX
    FBytePos, FBytePosOffset: integer; // 1 based
    FOldLinePos: integer; // 1 based
    FOldCharPos: integer; // 1 based
    FAdjustToNextChar: Boolean;
    FMaxLeftChar: TMaxLeftCharFunc;
    FChangeOnTouch: Boolean;
    FSkipTabs: Boolean;
    FTouched: Boolean;

    procedure AdjustToChar;
    procedure UpdateBytePos;
    function GetOldLineBytePos: TPoint;
    function GetOldLineCharPos: TPoint;
    procedure InternalSetLineCharPos(NewLine, NewCharPos: integer; KeepLastCharPos: Boolean = False;
      ForceSet: Boolean = False);
    procedure setCharPos(const AValue: integer);
    procedure SetAllowPastEOL(const AValue: Boolean);
    procedure SetKeepCaretX(const AValue: Boolean);
    procedure setLinePos(const AValue: integer);
    function GetLineCharPos: TPoint;
    procedure SetLineCharPos(AValue: TPoint);
    function GetBytePos: integer;
    procedure SetBytePos(const AValue: integer);
    function GetLineBytePos: TPoint;
    procedure SetLineBytePos(const AValue: TPoint);
    function GetLineText: string;
    procedure SetLineText(const AValue: string);
    procedure SetSkipTabs(const AValue: Boolean);
  protected
    procedure SetLines(const AValue: TSynEditStrings); override;
    procedure DoLock; override;
    Procedure DoUnlock; override;
    procedure DoLinesEdited(Sender: TSynEditStrings; aLinePos, aBytePos, ACount, aLineBrkCnt: integer; aText: String);
  public
    constructor Create;
    destructor Destroy; override;
    procedure AssignFrom(Src: TSynEditCaret);
    procedure IncForcePastEOL;
    procedure DecForcePastEOL;
    procedure IncForceAdjustToNextChar;
    procedure DecForceAdjustToNextChar;
    procedure IncAutoMoveOnEdit;
    procedure DecAutoMoveOnEdit;
    procedure ChangeOnTouch;
    function IsAtLineChar(aPoint: TPoint): Boolean;
    function IsAtLineByte(aPoint: TPoint): Boolean;
    function WasAtLineChar(aPoint: TPoint): Boolean;
    function WasAtLineByte(aPoint: TPoint): Boolean;
    function IsAtPos(ACaret: TSynEditCaret): Boolean;

    property OldLinePos: integer read FOldLinePos;
    property OldCharPos: integer read FOldCharPos;
    property OldLineCharPos: TPoint read GetOldLineCharPos;
    property OldLineBytePos: TPoint read GetOldLineBytePos;

    property LinePos: integer read FLinePos write setLinePos;
    property CharPos: integer read FCharPos write setCharPos;
    property LineCharPos: TPoint read GetLineCharPos write SetLineCharPos;
    property BytePos: integer read GetBytePos write SetBytePos;
    property LineBytePos: TPoint read GetLineBytePos write SetLineBytePos;
    property LineText: string read GetLineText write SetLineText;

    property AdjustToNextChar: Boolean read FAdjustToNextChar write FAdjustToNextChar;
    property SkipTabs: Boolean read FSkipTabs write SetSkipTabs;
    property AllowPastEOL: Boolean read FAllowPastEOL write SetAllowPastEOL;
    property KeepCaretX: Boolean read FKeepCaretX write SetKeepCaretX;
    property MaxLeftChar: TMaxLeftCharFunc write FMaxLeftChar;
  end;

  TSynCaretType = (ctVerticalLine, ctHorizontalLine, ctHalfBlock, ctBlock);
  TSynCaretLockFlags = set of (sclfUpdateDisplay, sclfUpdateDisplayType);

  { TSynEditScreenCaret }

  TSynEditScreenCaret = class
  private
    FCharHeight: integer;
    FCharWidth: integer;
    FClipRight: integer;
    FClipBottom: integer;
    FClipLeft: integer;
    FClipTop: integer;
    FDisplayPos: TPoint;
    FDisplayType: TSynCaretType;
    FExtraLinePixel, FExtraLineChars: integer;
    FOnExtraLineCharsChanged: TNotifyEvent;
    FVisible: Boolean;
    FHandleOwner: TComponent;
    // function GetHandle: Thandle;
    // function GetHandleAllocated: Boolean;
    procedure SetCharHeight(const AValue: integer);
    procedure SetCharWidth(const AValue: integer);
    procedure SetClipRight(const AValue: integer);
    procedure SetDisplayPos(const AValue: TPoint);
    procedure SetDisplayType(const AType: TSynCaretType);
    procedure SetVisible(const AValue: Boolean);
  private
    FClipExtraPixel: integer;
{$IFDEF SynCaretDebug}
    FDebugShowCount: integer;
{$ENDIF}
    FPixelWidth, FPixelHeight: integer;
    FOffsetX, FOffsetY: integer;
    FCurrentPosX, FCurrentPosY: integer;
    FCurrentVisible, FCurrentCreated: Boolean;
    FCurrentClippedWidth: integer;
    FLockCount: integer;
    FLockFlags: TSynCaretLockFlags;
    procedure SetClipBottom(const AValue: integer);
    procedure SetClipExtraPixel(AValue: integer);
    procedure SetClipLeft(const AValue: integer);
    procedure SetClipRect(const AValue: TRect);
    procedure SetClipTop(const AValue: integer);
    procedure CalcExtraLineChars;
    procedure UpdateDisplayType;
    procedure UpdateDisplay;
    procedure ShowCaret;
    procedure HideCaret;
    // property Handle: THandle read GetHandle;
    // property HandleAllocated: Boolean read GetHandleAllocated;
  public
    constructor Create(AHandleOwner: TComponent);
    destructor Destroy; override;
    procedure Hide; // Keep visible = true
    procedure DestroyCaret(SkipHide: Boolean = False);
    procedure Lock;
    procedure Unlock;
    property HandleOwner: TComponent read FHandleOwner;
    property CharWidth: integer read FCharWidth write SetCharWidth;
    property CharHeight: integer read FCharHeight write SetCharHeight;
    property ClipLeft: integer read FClipLeft write SetClipLeft;
    property ClipRight: integer read FClipRight write SetClipRight; // First pixel outside the allowed area
    property ClipTop: integer read FClipTop write SetClipTop;
    property ClipRect: TRect write SetClipRect;
    property ClipBottom: integer read FClipBottom write SetClipBottom;
    property ClipExtraPixel: integer read FClipExtraPixel write SetClipExtraPixel;
    // Amount of pixels, after  the last full char (half visible char width)
    property Visible: Boolean read FVisible write SetVisible;
    property DisplayType: TSynCaretType read FDisplayType write SetDisplayType;
    property DisplayPos: TPoint read FDisplayPos write SetDisplayPos;
    property ExtraLineChars: integer read FExtraLineChars; // Extend the longest line by x chars
    property OnExtraLineCharsChanged: TNotifyEvent read FOnExtraLineCharsChanged write FOnExtraLineCharsChanged;
  end;

implementation

{ TSynEditPointBase }

function TSynEditPointBase.GetLocked: Boolean;
begin
  Result := FLockCount > 0;
end;

procedure TSynEditPointBase.SetLines(const AValue: TSynEditStrings);
begin
  FLines := AValue;
end;

procedure TSynEditPointBase.DoLock;
begin
end;

procedure TSynEditPointBase.DoUnlock;
begin
end;

constructor TSynEditPointBase.Create;
begin
  FOnChangeList := TMethodList.Create;
end;

constructor TSynEditPointBase.Create(Lines: TSynEditStrings);
begin
  Create;
  FLines := Lines;
end;

destructor TSynEditPointBase.Destroy;
begin
  FreeAndNil(FOnChangeList);
  inherited Destroy;
end;

procedure TSynEditPointBase.AddChangeHandler(AHandler: TNotifyEvent);
begin
  FOnChangeList.Add(TMethod(AHandler));
end;

procedure TSynEditPointBase.RemoveChangeHandler(AHandler: TNotifyEvent);
begin
  FOnChangeList.Remove(TMethod(AHandler));
end;

procedure TSynEditPointBase.Lock;
begin
  if FLockCount = 0 then
    DoLock;
  inc(FLockCount);
end;

procedure TSynEditPointBase.Unlock;
begin
  dec(FLockCount);
  if FLockCount = 0 then
    DoUnlock;
end;

{ TSynEditCaret }

constructor TSynEditCaret.Create;
begin
  inherited Create;
  FMaxLeftChar := nil;
  FLinePos := 1;
  FCharPos := 1;
  FAllowPastEOL := True;
  FForcePastEOL := 0;
  FAutoMoveOnEdit := 0;
  if FLines <> nil then
    FLines.AddEditHandler(DoLinesEdited);
end;

destructor TSynEditCaret.Destroy;
begin
  if FLines <> nil then
    FLines.RemoveEditHandler(DoLinesEdited);
  inherited Destroy;
end;

procedure TSynEditCaret.AssignFrom(Src: TSynEditCaret);
begin
  FOldCharPos := FCharPos;
  FOldLinePos := FLinePos;

  FLines := Src.FLines;
  FMaxLeftChar := Src.FMaxLeftChar;
  FAllowPastEOL := Src.FAllowPastEOL;
  FKeepCaretX := Src.FKeepCaretX;
  FLinePos := Src.FLinePos;
  FCharPos := Src.FCharPos;
  FLastCharPos := Src.FLastCharPos;
end;

procedure TSynEditCaret.IncForcePastEOL;
begin
  inc(FForcePastEOL);
end;

procedure TSynEditCaret.DecForcePastEOL;
begin
  dec(FForcePastEOL);
end;

procedure TSynEditCaret.IncForceAdjustToNextChar;
begin
  inc(FForceAdjustToNextChar);
end;

procedure TSynEditCaret.DecForceAdjustToNextChar;
begin
  dec(FForceAdjustToNextChar);
end;

procedure TSynEditCaret.IncAutoMoveOnEdit;
begin
  if FAutoMoveOnEdit = 0 then
    UpdateBytePos;;
  inc(FAutoMoveOnEdit);
end;

procedure TSynEditCaret.DecAutoMoveOnEdit;
begin
  dec(FAutoMoveOnEdit);
end;

procedure TSynEditCaret.ChangeOnTouch;
begin
  FChangeOnTouch := True;
  if not Locked then
    FTouched := False;
end;

function TSynEditCaret.IsAtLineChar(aPoint: TPoint): Boolean;
begin
  Result := (FLinePos = aPoint.y) and (FCharPos = aPoint.x);
end;

function TSynEditCaret.IsAtLineByte(aPoint: TPoint): Boolean;
begin
  Result := (FLinePos = aPoint.y) and (BytePos = aPoint.x);
end;

function TSynEditCaret.WasAtLineChar(aPoint: TPoint): Boolean;
begin
  Result := (FOldLinePos = aPoint.y) and (FOldCharPos = aPoint.x);
end;

function TSynEditCaret.WasAtLineByte(aPoint: TPoint): Boolean;
begin
  Result := (FOldLinePos = aPoint.y) and (FLines.PhysicalToLogicalPos(Point(FOldCharPos, FOldLinePos)).x = aPoint.x);
end;

function TSynEditCaret.IsAtPos(ACaret: TSynEditCaret): Boolean;
begin
  Result := IsAtLineChar(ACaret.LineCharPos);
end;

procedure TSynEditCaret.setLinePos(const AValue: integer);
begin
  InternalSetLineCharPos(AValue, FLastCharPos, True);
end;

procedure TSynEditCaret.AdjustToChar;
var
  CharWidths: TPhysicalCharWidths;
  LogLen: integer;
  ScreenPos: integer;
  LogPos: integer;
  L: String;
begin
  L := LineText;
  CharWidths := FLines.GetPhysicalCharWidths(PChar(L), length(L), FLinePos - 1);
  LogLen := length(CharWidths);
  ScreenPos := 1;
  LogPos := 0;

  while LogPos < LogLen do
  begin
    if ScreenPos = FCharPos then
      exit;
    if ScreenPos + CharWidths[LogPos] > FCharPos then
    begin
      if (L[LogPos + 1] = #9) and (not FSkipTabs) then
        exit;
      if FAdjustToNextChar or (FForceAdjustToNextChar > 0) then
        FCharPos := ScreenPos + CharWidths[LogPos]
      else
        FCharPos := ScreenPos;
      exit;
    end;
    ScreenPos := ScreenPos + CharWidths[LogPos];
    inc(LogPos);
  end;
end;

procedure TSynEditCaret.UpdateBytePos;
begin
  FBytePos := FLines.LogPhysConvertor.PhysicalToLogical(FLinePos - 1, FCharPos, FBytePosOffset);
end;

function TSynEditCaret.GetOldLineBytePos: TPoint;
begin
  Result := FLines.PhysicalToLogicalPos(OldLineCharPos);
end;

function TSynEditCaret.GetOldLineCharPos: TPoint;
begin
  Result := Point(FOldCharPos, FOldLinePos);
end;

procedure TSynEditCaret.setCharPos(const AValue: integer);
begin
  InternalSetLineCharPos(FLinePos, AValue);
end;

procedure TSynEditCaret.SetAllowPastEOL(const AValue: Boolean);
begin
  if FAllowPastEOL = AValue then
    exit;
  FAllowPastEOL := AValue;
  if not FAllowPastEOL then
    InternalSetLineCharPos(FLinePos, FCharPos, True, True);
end;

procedure TSynEditCaret.SetKeepCaretX(const AValue: Boolean);
begin
  if FKeepCaretX = AValue then
    exit;
  FKeepCaretX := AValue;
  if FKeepCaretX then
    FLastCharPos := FCharPos;
end;

function TSynEditCaret.GetLineCharPos: TPoint;
begin
  Result := Point(FCharPos, FLinePos);
end;

procedure TSynEditCaret.SetLineCharPos(AValue: TPoint);
begin
  InternalSetLineCharPos(AValue.y, AValue.x);
end;

procedure TSynEditCaret.InternalSetLineCharPos(NewLine, NewCharPos: integer; KeepLastCharPos: Boolean = False;
  ForceSet: Boolean = False);
var
  nMaxX, i: integer;
  Line: string;
begin
  Lock;
  FTouched := True;
  try
    if (FCharPos <> NewCharPos) or (FLinePos <> NewLine) or ForceSet then
    begin
      if Assigned(FMaxLeftChar) then
        nMaxX := FMaxLeftChar()
      else
        nMaxX := MaxInt;
      if NewLine > FLines.Count then
        NewLine := FLines.Count;
      if NewLine < 1 then
      begin
        // this is just to make sure if Lines stringlist should be empty
        NewLine := 1;
        if (not FAllowPastEOL) and (FForcePastEOL = 0) then
          nMaxX := 1;
      end
      else
      begin
        Line := Lines[NewLine - 1];
        i := Lines.LogicalToPhysicalCol(Line, NewLine - 1, length(Line) + 1);
        if ((not FAllowPastEOL) and (FForcePastEOL = 0)) or (nMaxX < i) then
          nMaxX := i;
      end;
      if NewCharPos > nMaxX then
        NewCharPos := nMaxX;
      if NewCharPos < 1 then
        NewCharPos := 1;

      FCharPos := NewCharPos;
      FLinePos := NewLine;
      AdjustToChar;
      if (not KeepLastCharPos) or (not FKeepCaretX) then
        FLastCharPos := FCharPos;
      if FAutoMoveOnEdit <> 0 then
        UpdateBytePos;
    end;
  finally
    Unlock;
  end;
end;

function TSynEditCaret.GetBytePos: integer;
begin
  Result := LineBytePos.x;
end;

procedure TSynEditCaret.SetBytePos(const AValue: integer);
begin
  CharPos := FLines.LogicalToPhysicalPos(Point(AValue, LinePos)).x;
end;

function TSynEditCaret.GetLineBytePos: TPoint;
begin
  Result := FLines.PhysicalToLogicalPos(LineCharPos);
end;

procedure TSynEditCaret.SetLineBytePos(const AValue: TPoint);
begin
  LineCharPos := FLines.LogicalToPhysicalPos(AValue);
end;

function TSynEditCaret.GetLineText: string;
begin
  if (LinePos >= 1) and (LinePos <= FLines.Count) then
    Result := FLines[LinePos - 1]
  else
    Result := '';
end;

procedure TSynEditCaret.SetLineText(const AValue: string);
begin
  if (LinePos >= 1) and (LinePos <= Max(1, FLines.Count)) then
    FLines[LinePos - 1] := AValue;
end;

procedure TSynEditCaret.SetSkipTabs(const AValue: Boolean);
begin
  if FSkipTabs = AValue then
    exit;
  FSkipTabs := AValue;
  if FSkipTabs then
  begin
    Lock;
    AdjustToChar;
    Unlock;
  end;
end;

procedure TSynEditCaret.SetLines(const AValue: TSynEditStrings);
begin
  if FLines = AValue then
    exit;
  if FLines <> nil then
    FLines.RemoveEditHandler(DoLinesEdited);
  inherited SetLines(AValue);
  if FLines <> nil then
    FLines.AddEditHandler(DoLinesEdited);
end;

procedure TSynEditCaret.DoLock;
begin
  FTouched := False;
  FOldCharPos := FCharPos;
  FOldLinePos := FLinePos;
end;

procedure TSynEditCaret.DoUnlock;
begin
  if not FChangeOnTouch then
    FTouched := False;
  FChangeOnTouch := False;
  if (FOldCharPos <> FCharPos) or (FOldLinePos <> FLinePos) or FTouched then
    FOnChangeList.CallNotifyEvents(self);
  // All notifications called, reset oldpos
  FTouched := False;
  FOldCharPos := FCharPos;
  FOldLinePos := FLinePos;
end;

procedure TSynEditCaret.DoLinesEdited(Sender: TSynEditStrings; aLinePos, aBytePos, ACount, aLineBrkCnt: integer;
  aText: String);
// Todo: refactor / this is a copy from selection
  function AdjustPoint(aPoint: TPoint): TPoint;
  begin
    Result := aPoint;
    if aLineBrkCnt < 0 then
    begin
      (* Lines Deleted *)
      if aPoint.y > aLinePos then
      begin
        Result.y := Max(aLinePos, Result.y + aLineBrkCnt);
        if Result.y = aLinePos then
          Result.x := Result.x + aBytePos - 1;
      end;
    end
    else if aLineBrkCnt > 0 then
    begin
      (* Lines Inserted *)
      if (aPoint.y = aLinePos) and (aPoint.x >= aBytePos) then
      begin
        Result.x := Result.x - aBytePos + 1;
        Result.y := Result.y + aLineBrkCnt;
      end;
      if aPoint.y > aLinePos then
      begin
        Result.y := Result.y + aLineBrkCnt;
      end;
    end
    else if ACount <> 0 then
    begin
      (* Chars Insert/Deleted *)
      if (aPoint.y = aLinePos) and (aPoint.x >= aBytePos) then
        Result.x := Max(aBytePos, Result.x + ACount);
    end;
  end;

var
  p: TPoint;
begin
  if FAutoMoveOnEdit > 0 then
  begin
    IncForcePastEOL;
    p := AdjustPoint(Point(FBytePos, FLinePos));
    p.x := FLines.LogPhysConvertor.LogicalToPhysical(p.y - 1, p.x, FBytePosOffset);
    FBytePos := -1;
    LineCharPos := p;
    if FBytePos < 0 then
      UpdateBytePos;
    DecForcePastEOL;
  end;
end;

{ TSynEditSelection }

constructor TSynEditSelection.Create(ALines: TSynEditStrings; aActOnLineChanges: Boolean);
begin
  Inherited Create(ALines);
  FInternalCaret := TSynEditCaret.Create; // TODO: does not need FLines.AddEditHandler
  FInternalCaret.Lines := FLines;

  FActiveSelectionMode := smNormal;
  FStartLinePos := 1;
  FStartBytePos := 1;
  FEndLinePos := 1;
  FEndBytePos := 1;
  FEnabled := True;
  FHookedLines := aActOnLineChanges;
  FIsSettingText := False;
  if FHookedLines then
  begin
    FLines.AddEditHandler(DoLinesEdited);
    FLines.AddChangeHandler(senrLineChange, {$IFDEF FPC}@{$ENDIF}LineChanged);
  end;
end;

destructor TSynEditSelection.Destroy;
begin
  FreeAndNil(FInternalCaret);
  if FHookedLines then
  begin
    FLines.RemoveEditHandler(DoLinesEdited);
    FLines.RemoveChangeHandler(senrLineChange, {$IFDEF FPC}@{$ENDIF}LineChanged);
  end;
  inherited Destroy;
end;

procedure TSynEditSelection.AssignFrom(Src: TSynEditSelection);
begin
  // FEnabled             := src.FEnabled;
  FHide := Src.FHide;
  FActiveSelectionMode := Src.FActiveSelectionMode;
  FSelectionMode := Src.FSelectionMode;
  FStartLinePos := Src.FStartLinePos; // 1 based
  FStartBytePos := Src.FStartBytePos; // 1 based
  FEndLinePos := Src.FEndLinePos; // 1 based
  FEndBytePos := Src.FEndBytePos; // 1 based
  FPersistent := Src.FPersistent;
end;

procedure TSynEditSelection.AdjustAfterTrimming;
begin
  if FStartBytePos > length(FLines[FStartLinePos - 1]) + 1 then
    FStartBytePos := length(FLines[FStartLinePos - 1]) + 1;
  if FEndBytePos > length(FLines[FEndLinePos - 1]) + 1 then
    FEndBytePos := length(FLines[FEndLinePos - 1]) + 1;
  // Todo: Call ChangeNotification
end;

procedure TSynEditSelection.DoLock;
begin
  inherited DoLock;
  FLastCarePos := Point(-1, -1);
end;

procedure TSynEditSelection.DoUnlock;
begin
  inherited DoUnlock;
  FLastCarePos := Point(-1, -1);
end;

function TSynEditSelection.GetSelText: string;

  function CopyPadded(const S: string; Index, Count: integer): string;
  var
    SrcLen: integer;
    DstLen: integer;
    p: PChar;
  begin
    SrcLen := length(S);
    DstLen := Index + Count;
    if SrcLen >= DstLen then
      Result := Copy(S, Index, Count)
    else
    begin
      SetLength(Result, DstLen);
      p := PChar(Pointer(Result));
      StrPCopy(p, Copy(S, Index, Count));
      inc(p, SrcLen);
      FillChar(p^, DstLen - SrcLen, $20);
    end;
  end;

  procedure CopyAndForward(const S: string; Index, Count: integer; var p: PChar);
  var
    pSrc: PChar;
    SrcLen: integer;
    DstLen: integer;
  begin
    SrcLen := length(S);
    if (Index <= SrcLen) and (Count > 0) then
    begin
      dec(Index);
      pSrc := PChar(Pointer(S)) + Index;
      DstLen := Min(SrcLen - Index, Count);
      Move(pSrc^, p^, DstLen);
      inc(p, DstLen);
      p^ := #0;
    end;
  end;

  procedure CopyPaddedAndForward(const S: string; Index, Count: integer; var p: PChar);
  var
    OldP: PChar;
    Len: integer;
  begin
    OldP := p;
    CopyAndForward(S, Index, Count, p);
    Len := Count - (p - OldP);
    FillChar(p^, Len, #$20);
    inc(p, Len);
  end;

var
  First, Last, TotalLen: integer;
  ColFrom, ColTo: integer;
  i: integer;
  p: PChar;
  C1, C2: integer;
  Col, Len: array of integer;

begin
  if not SelAvail then
    Result := ''
  else
  begin
    if IsBackwardSel then
    begin
      ColFrom := FEndBytePos;
      First := FEndLinePos - 1;
      ColTo := FStartBytePos;
      Last := FStartLinePos - 1;
    end
    else
    begin
      ColFrom := FStartBytePos;
      First := FStartLinePos - 1;
      ColTo := FEndBytePos;
      Last := FEndLinePos - 1;
    end;
    TotalLen := 0;
    case ActiveSelectionMode of
      smNormal:
        if (First = Last) then
        begin
          Result := Copy(FLines[First], ColFrom, ColTo - ColFrom);
          i := (ColTo - ColFrom) - length(Result);
          if i > 0 then
            Result := Result + StringOfChar(' ', i);
        end
        else
        begin
          // step1: calculate total length of result string
          TotalLen := Max(0, length(FLines[First]) - ColFrom + 1);
          for i := First + 1 to Last - 1 do
            inc(TotalLen, length(FLines[i]));
          inc(TotalLen, ColTo - 1);
          inc(TotalLen, length(sLineBreak) * (Last - First));
          // step2: build up result string
          SetLength(Result, TotalLen);
          p := PChar(Pointer(Result));
          CopyAndForward(FLines[First], ColFrom, MaxInt, p);
          CopyAndForward(sLineBreak, 1, MaxInt, p);
          for i := First + 1 to Last - 1 do
          begin
            CopyAndForward(FLines[i], 1, MaxInt, p);
            CopyAndForward(sLineBreak, 1, MaxInt, p);
          end;
          CopyPaddedAndForward(FLines[Last], 1, ColTo - 1, p);
        end;
      smColumn:
        begin
          // Calculate the byte positions for each line
          SetLength(Col, Last - First + 1);
          SetLength(Len, Last - First + 1);
          FInternalCaret.AllowPastEOL := True;
          FInternalCaret.LineBytePos := FirstLineBytePos;
          C1 := FInternalCaret.CharPos;
          FInternalCaret.LineBytePos := LastLineBytePos;
          C2 := FInternalCaret.CharPos;
          if C1 > C2 then
            SwapInt(C1, C2);

          TotalLen := 0;
          for i := First to Last do
          begin
            FInternalCaret.LineCharPos := Point(C1, i + 1);
            Col[i - First] := FInternalCaret.BytePos;
            FInternalCaret.LineCharPos := Point(C2, i + 1);
            Len[i - First] := Max(0, FInternalCaret.BytePos - Col[i - First]);
            inc(TotalLen, Len[i - First]);
          end;
          inc(TotalLen, length(LineEnding) * (Last - First));
          // build up result string
          SetLength(Result, TotalLen);
          p := PChar(Pointer(Result));
          for i := First to Last do
          begin
            CopyPaddedAndForward(FLines[i], Col[i - First], Len[i - First], p);
            if i < Last then
              CopyAndForward(LineEnding, 1, MaxInt, p);
          end;
        end;
      smLine:
        begin
          // If block selection includes LastLine,
          // line break code(s) of the last line will not be added.
          // step1: calclate total length of result string
          for i := First to Last do
            inc(TotalLen, length(FLines[i]) + length(LineEnding));
          if Last = FLines.Count - 1 then
            dec(TotalLen, length(LineEnding));
          // step2: build up result string
          SetLength(Result, TotalLen);
          p := PChar(Pointer(Result));
          for i := First to Last - 1 do
          begin
            CopyAndForward(FLines[i], 1, MaxInt, p);
            CopyAndForward(LineEnding, 1, MaxInt, p);
          end;
          CopyAndForward(FLines[Last], 1, MaxInt, p);
          if Last < FLines.Count - 1 then
            CopyAndForward(LineEnding, 1, MaxInt, p);
        end;
    end;
  end;
end;

procedure TSynEditSelection.SetSelText(const Value: string);
begin
  SetSelTextPrimitive(FActiveSelectionMode, PChar(Value));
end;

procedure TSynEditSelection.DoCaretChanged(Sender: TObject);
begin
  // FIgnoreNextCaretMove => caret skip selection
  if FIgnoreNextCaretMove then
  begin
    FIgnoreNextCaretMove := False;
    FLastCarePos := Point(-1, -1);
    exit;
  end;

  if (FCaret.IsAtLineByte(StartLineBytePos) or FCaret.IsAtLineByte(EndLineBytePos)) and
    FCaret.WasAtLineChar(FLastCarePos) then
    exit;
  FLastCarePos := Point(-1, -1);

  if FAutoExtend then
  begin
    if (not FHide) and (FCaret.WasAtLineByte(EndLineBytePos)) then
      SetEndLineBytePos(FCaret.LineBytePos)
    else if (not FHide) and (FCaret.WasAtLineByte(StartLineBytePos)) then
      AdjustStartLineBytePos(FCaret.LineBytePos)
    else
    begin
      StartLineBytePos := Point(FCaret.OldCharPos, FCaret.OldLinePos);
      EndLineBytePos := FCaret.LineBytePos;
      if Persistent and IsBackwardSel then
        SortSelectionPoints;
    end;
    exit;
  end;

  if FPersistent or (FPersistentLock > 0) then
    exit;

  StartLineBytePos := FCaret.LineBytePos;
end;

procedure TSynEditSelection.LineChanged(Sender: TSynEditStrings; AIndex, ACount: integer);
begin
  if (FCaret <> nil) and (not FCaret.AllowPastEOL) and (not FIsSettingText) then
    AdjustAfterTrimming;
end;

procedure TSynEditSelection.DoLinesEdited(Sender: TSynEditStrings; aLinePos, aBytePos, ACount, aLineBrkCnt: integer;
  aText: String);

  function AdjustPoint(aPoint: TPoint): TPoint;
  begin
    Result := aPoint;
    if aLineBrkCnt < 0 then
    begin
      (* Lines Deleted *)
      if aPoint.y > aLinePos then
      begin
        Result.y := Max(aLinePos, Result.y + aLineBrkCnt);
        if Result.y = aLinePos then
          Result.x := Result.x + aBytePos - 1;
      end;
    end
    else if aLineBrkCnt > 0 then
    begin
      (* Lines Inserted *)
      if (aPoint.y = aLinePos) and (aPoint.x >= aBytePos) then
      begin
        Result.x := Result.x - aBytePos + 1;
        Result.y := Result.y + aLineBrkCnt;
      end;
      if aPoint.y > aLinePos then
      begin
        Result.y := Result.y + aLineBrkCnt;
      end;
    end
    else if ACount <> 0 then
    begin
      (* Chars Insert/Deleted *)
      if (aPoint.y = aLinePos) and (aPoint.x >= aBytePos) then
        Result.x := Max(aBytePos, Result.x + ACount);
    end;
  end;

begin
  if FIsSettingText then
    exit;
  if FPersistent or (FPersistentLock > 0) or ((FCaret <> nil) and (not FCaret.Locked)) then
  begin
    if FActiveSelectionMode <> smColumn then
    begin // TODO: adjust ypos, height in smColumn mode
      AdjustStartLineBytePos(AdjustPoint(StartLineBytePos));
      EndLineBytePos := AdjustPoint(EndLineBytePos);
    end;
    // Todo: Change Lines in smColumn
  end
  else
  begin
    // Change the Selection, if change was made by owning SynEdit (Caret.Locked)
    // (InternalSelection has no Caret)
    if (FCaret <> nil) and (FCaret.Locked) then
      StartLineBytePos := FCaret.LineBytePos;
  end;
end;

procedure TSynEditSelection.SetSelTextPrimitive(PasteMode: TSynSelectionMode; Value: PChar; AReplace: Boolean = False);
var
  BB, BE: TPoint;

  procedure DeleteSelection;
  var
    y, L, r, xb, xe: integer;
    Str: string;
    Start, p: PChar;
    // LogCaretXY: TPoint;
  begin
    case ActiveSelectionMode of
      smNormal, smLine:
        begin
          if FLines.Count > 0 then
          begin

            if AReplace and (Value <> nil) then
            begin
              // AReplace = True
              while Value^ <> #0 do
              begin
                Start := PChar(Value);
                p := GetEOL(Start);
                Value := p;

                if Value^ = #13 then
                  inc(Value);
                if Value^ = #10 then
                  inc(Value);

                SetString(Str, Start, p - Start);

                if BE.y > BB.y then
                begin
                  FLines.EditDelete(BB.x, BB.y, 1 + length(FLines[BB.y - 1]) - BB.x);
                  FLines.EditInsert(BB.x, BB.y, Str);
                  if (PasteMode = smLine) or (Value > p) then
                  begin
                    inc(BB.y);
                    BB.x := 1;
                  end
                  else
                    BB.x := BB.x + length(Str);
                end
                else
                begin
                  FLines.EditDelete(BB.x, BB.y, BE.x - BB.x);
                  // BE will be block-.nd, also used by SynEdit to set caret
                  if (ActiveSelectionMode = smLine) or (Value > p) then
                  begin
                    FLines.EditLineBreak(BB.x, BB.y);
                    inc(BE.y);
                    BE.x := 1;
                  end
                  else
                    BE.x := BB.x + length(Str);
                  FLines.EditInsert(BB.x, BB.y, Str);
                  BB := BE; // end of selection
                end;

                if (BB.y = BE.y) and (BB.x = BE.x) then
                begin
                  FInternalCaret.LineBytePos := BB;
                  exit;
                end;

              end;
            end;

            // AReplace = False
            if BE.y > BB.y + 1 then
            begin
              FLines.EditLinesDelete(BB.y + 1, BE.y - BB.y - 1);
              BE.y := BB.y + 1;
            end;
            if BE.y > BB.y then
            begin
              L := length(FLines[BB.y - 1]);
              BE.x := BE.x + Max(L, BB.x - 1);
              FLines.EditLineJoin(BB.y, StringOfChar(' ', Max(0, BB.x - (L + 1))));
              BE.y := BB.y;
            end;
            if BE.x <> BB.x then
              FLines.EditDelete(BB.x, BB.y, BE.x - BB.x);
          end;
          FInternalCaret.LineBytePos := BB;
        end;
      smColumn:
        begin
          // AReplace has no effect
          FInternalCaret.LineBytePos := BB;
          L := FInternalCaret.CharPos;
          FInternalCaret.LineBytePos := BE;
          r := FInternalCaret.CharPos;
          // swap l, r if needed
          if L > r then
{$IFDEF SYN_COMPILER_3_UP}
            SwapInt(L, r);
{$ELSE}
            begin
              y := L;
              L := r;
              r := y;
            end;
{$ENDIF}
          for y := BB.y to BE.y do
          begin
            FInternalCaret.LineCharPos := Point(L, y);
            xb := FInternalCaret.BytePos;
            FInternalCaret.LineCharPos := Point(r, y);
            xe := Min(FInternalCaret.BytePos, 1 + length(FInternalCaret.LineText));
            if xe > xb then
              FLines.EditDelete(xb, y, xe - xb);
          end;
          FInternalCaret.LineCharPos := Point(L, BB.y);
          BB := FInternalCaret.LineBytePos;
          // Column deletion never removes a line entirely,
          // so no (vertical) mark updating is needed here.
        end;
    end;
  end;

  procedure InsertText;

    function CountLines(p: PChar): integer;
    begin
      Result := 0;
      while p^ <> #0 do
      begin
        if p^ = #13 then
          inc(p);
        if p^ = #10 then
          inc(p);
        inc(Result);
        p := GetEOL(p);
      end;
    end;

    function InsertNormal: integer;
    var
      Str: string;
      Start: PChar;
      p: PChar;
      LogCaretXY: TPoint;
    begin
      Result := 0;
      LogCaretXY := FInternalCaret.LineBytePos;

      Start := PChar(Value);
      p := GetEOL(Start);
      if p^ = #0 then
      begin
        FLines.EditInsert(LogCaretXY.x, LogCaretXY.y, Value);
        FInternalCaret.BytePos := FInternalCaret.BytePos + length(Value);
      end
      else
      begin
        SetString(Str, Value, p - Start);
        FLines.EditInsert(LogCaretXY.x, LogCaretXY.y, Str);
        FLines.EditLineBreak(LogCaretXY.x + length(Str), LogCaretXY.y);
        Result := CountLines(p);
        if Result > 1 then
          FLines.EditLinesInsert(LogCaretXY.y + 1, Result - 1);
        while p^ <> #0 do
        begin
          if p^ = #13 then
            inc(p);
          if p^ = #10 then
            inc(p);
          LogCaretXY.y := LogCaretXY.y + 1;
          Start := p;
          p := GetEOL(Start);
          if p <> Start then
          begin
            SetString(Str, Start, p - Start);
            FLines.EditInsert(1, LogCaretXY.y, Str);
          end
          else
            Str := '';
        end;
        FInternalCaret.LinePos := LogCaretXY.y;
        FInternalCaret.BytePos := 1 + length(Str);
      end;
    end;

    function InsertColumn: integer;
    var
      Str: string;
      Start: PChar;
      p: PChar;
    begin
      // Insert string at current position
      Result := 0;
      FInternalCaret.IncForcePastEOL;
      Start := PChar(Value);
      repeat
        p := GetEOL(Start);
        if p <> Start then
        begin
          SetLength(Str, p - Start);
          Move(Start^, Str[1], p - Start);
          FLines.EditInsert(FInternalCaret.BytePos, FInternalCaret.LinePos, Str);
        end;
        if p^ in [#10, #13] then
        begin
          if (p[1] in [#10, #13]) and (p[1] <> p^) then
            inc(p, 2)
          else
            inc(p);
          if FInternalCaret.LinePos = FLines.Count then
            FLines.EditLinesInsert(FInternalCaret.LinePos + 1, 1);
          // No need to inc result => adding at EOF
          FInternalCaret.LinePos := FInternalCaret.LinePos + 1;
        end;
        Start := p;
      until p^ = #0;
      FInternalCaret.BytePos := FInternalCaret.BytePos + length(Str);
      FInternalCaret.DecForcePastEOL;
    end;

    function InsertLine: integer;
    var
      Start: PChar;
      p: PChar;
      Str: string;
    begin
      Result := 0;
      FInternalCaret.CharPos := 1;
      // Insert string before current line
      Start := PChar(Value);
      repeat
        p := GetEOL(Start);
        if p <> Start then
        begin
          SetLength(Str, p - Start);
          Move(Start^, Str[1], p - Start);
        end
        else
          Str := '';
        if (p^ = #0) then
        begin // Not a full line?
          FLines.EditInsert(1, FInternalCaret.LinePos, Str);
          FInternalCaret.BytePos := 1 + length(Str);
        end
        else
        begin
          FLines.EditLinesInsert(FInternalCaret.LinePos, 1, Str);
          FInternalCaret.LinePos := FInternalCaret.LinePos + 1;
          inc(Result);
          if p^ = #13 then
            inc(p);
          if p^ = #10 then
            inc(p);
          Start := p;
        end;
      until p^ = #0;
    end;

  begin
    if Value = '' then
      exit;
    if FLines.Count = 0 then
      FLines.Add('');

    // Using a TStringList to do this would be easier, but if we're dealing
    // with a large block of text, it would be very inefficient.  Consider:
    // Assign Value parameter to TStringList.Text: that parses through it and
    // creates a copy of the string for each line it finds.  That copy is passed
    // to the Add method, which in turn creates a copy.  Then, when you actually
    // use an item in the list, that creates a copy to return to you.  That's
    // 3 copies of every string vs. our one copy below.  I'd prefer no copies,
    // but we aren't set up to work with PChars that well.

    case PasteMode of
      smNormal:
        InsertNormal;
      smColumn:
        InsertColumn;
      smLine:
        InsertLine;
    end;
  end;

begin
  FIsSettingText := True;
  FLines.BeginUpdate; // Todo: can we get here, without paintlock?
  try
    // BB is lower than BE
    BB := FirstLineBytePos;
    BE := LastLineBytePos;
    if SelAvail then
    begin
      if FActiveSelectionMode = smLine then
      begin
        BB.x := 1;
        if BE.y = FLines.Count then
        begin
          // Keep the (CrLf of) last line, since no Line exists to replace it
          BE.x := 1 + length(FLines[BE.y - 1]);
        end
        else
        begin
          inc(BE.y);
          BE.x := 1;
        end;
      end;
      DeleteSelection;
      StartLineBytePos := BB; // deletes selection // calls selection changed
      // Need to update caret (syncro edit follows on every edit)
      if FCaret <> nil then
        FCaret.LineCharPos := FInternalCaret.LineCharPos; // must equal BB
    end
    else if FCaret <> nil then
      StartLineBytePos := FCaret.LineBytePos;

    FInternalCaret.LineBytePos := StartLineBytePos;
    if (Value <> nil) and (Value[0] <> #0) then
    begin
      InsertText;
      StartLineBytePos := FInternalCaret.LineBytePos; // reset selection
    end;
    if FCaret <> nil then
      FCaret.LineCharPos := FInternalCaret.LineCharPos;
  finally
    FLines.EndUpdate;
    FIsSettingText := False;
  end;
end;

function TSynEditSelection.GetStartLineBytePos: TPoint;
begin
  Result.y := FStartLinePos;
  Result.x := FStartBytePos;
end;

procedure TSynEditSelection.SetEnabled(const Value: Boolean);
begin
  if FEnabled = Value then
    exit;
  FEnabled := Value;
  if not Enabled then
    SetStartLineBytePos(StartLineBytePos);
end;

procedure TSynEditSelection.SetStartLineBytePos(Value: TPoint);
// logical position (byte)
var
  nInval1, nInval2: integer;
  WasAvail: Boolean;
begin
  WasAvail := SelAvail;
  Value.y := MinMax(Value.y, 1, FLines.Count);
  if (FCaret = nil) or FCaret.AllowPastEOL then
    Value.x := Max(Value.x, 1)
  else
    Value.x := MinMax(Value.x, 1, length(Lines[Value.y - 1]) + 1);
  if (ActiveSelectionMode = smNormal) then
    if (Value.y >= 1) and (Value.y <= FLines.Count) then
      Value.x := AdjustBytePosToCharacterStart(Value.y, Value.x)
    else
      Value.x := 1;
  if SelAvail then
  begin
    if FStartLinePos < FEndLinePos then
    begin
      nInval1 := Min(Value.y, FStartLinePos);
      nInval2 := Max(Value.y, FEndLinePos);
    end
    else
    begin
      nInval1 := Min(Value.y, FEndLinePos);
      nInval2 := Max(Value.y, FStartLinePos);
    end;
    FInvalidateLinesMethod(nInval1, nInval2);
  end;
  FActiveSelectionMode := FSelectionMode;
  FHide := False;
  FStartLinePos := Value.y;
  FStartBytePos := Value.x;
  FEndLinePos := Value.y;
  FEndBytePos := Value.x;
  if FCaret <> nil then
    FLastCarePos := Point(FCaret.OldCharPos, FCaret.OldLinePos);
  if WasAvail then
    FOnChangeList.CallNotifyEvents(self);
end;

procedure TSynEditSelection.AdjustStartLineBytePos(Value: TPoint);
begin
  if FEnabled then
  begin
    Value.y := MinMax(Value.y, 1, FLines.Count);
    if (FCaret = nil) or FCaret.AllowPastEOL then
      Value.x := Max(Value.x, 1)
    else
      Value.x := MinMax(Value.x, 1, length(Lines[Value.y - 1]) + 1);
    if (ActiveSelectionMode = smNormal) then
      if (Value.y >= 1) and (Value.y <= FLines.Count) then
        Value.x := AdjustBytePosToCharacterStart(Value.y, Value.x)
      else
        Value.x := 1;

    if (Value.x <> FStartBytePos) or (Value.y <> FStartLinePos) then
    begin
      if (ActiveSelectionMode = smColumn) and (Value.x <> FStartBytePos) then
        FInvalidateLinesMethod(Min(FStartLinePos, Min(FEndLinePos, Value.y)),
          Max(FStartLinePos, Max(FEndLinePos, Value.y)))
      else if (ActiveSelectionMode <> smColumn) or (FStartBytePos <> FEndBytePos) then
        FInvalidateLinesMethod(FStartLinePos, Value.y);
      FStartLinePos := Value.y;
      FStartBytePos := Value.x;
      if FCaret <> nil then
        FLastCarePos := Point(FCaret.OldCharPos, FCaret.OldLinePos);
      FOnChangeList.CallNotifyEvents(self);
    end;
  end;
end;

function TSynEditSelection.GetEndLineBytePos: TPoint;
begin
  Result.y := FEndLinePos;
  Result.x := FEndBytePos;
end;

procedure TSynEditSelection.SetEndLineBytePos(Value: TPoint);
{$IFDEF SYN_MBCSSUPPORT}
var
  S: string;
{$ENDIF}
begin
  if FEnabled then
  begin
    Value.y := MinMax(Value.y, 1, FLines.Count);
    if (FCaret = nil) or FCaret.AllowPastEOL then
      Value.x := Max(Value.x, 1)
    else
      Value.x := MinMax(Value.x, 1, length(Lines[Value.y - 1]) + 1);
    if (ActiveSelectionMode = smNormal) then
      if (Value.y >= 1) and (Value.y <= FLines.Count) then
        Value.x := AdjustBytePosToCharacterStart(Value.y, Value.x)
      else
        Value.x := 1;
    if (Value.x <> FEndBytePos) or (Value.y <> FEndLinePos) then
    begin
{$IFDEF SYN_MBCSSUPPORT}
      if Value.y <= FLines.Count then
      begin
        S := FLines[Value.y - 1];
        if (length(S) >= Value.x) and (mbTrailByte = ByteType(S, Value.x)) then
          dec(Value.x);
      end;
{$ENDIF}
      if (Value.x <> FEndBytePos) or (Value.y <> FEndLinePos) then
      begin
        if (ActiveSelectionMode = smColumn) and (Value.x <> FEndBytePos) then
          FInvalidateLinesMethod(Min(FStartLinePos, Min(FEndLinePos, Value.y)),
            Max(FStartLinePos, Max(FEndLinePos, Value.y)))
        else if (ActiveSelectionMode <> smColumn) or (FStartBytePos <> FEndBytePos) then
          FInvalidateLinesMethod(FEndLinePos, Value.y);
        FEndLinePos := Value.y;
        FEndBytePos := Value.x;
        if FCaret <> nil then
          FLastCarePos := Point(FCaret.OldCharPos, FCaret.OldLinePos);
        FOnChangeList.CallNotifyEvents(self);
      end;
    end;
  end;
end;

procedure TSynEditSelection.SetSelectionMode(const AValue: TSynSelectionMode);
begin
  FSelectionMode := AValue;
  SetActiveSelectionMode(AValue);
  FOnChangeList.CallNotifyEvents(self);
end;

procedure TSynEditSelection.SetActiveSelectionMode(const Value: TSynSelectionMode);
begin
  if FActiveSelectionMode <> Value then
  begin
    FActiveSelectionMode := Value;
    if SelAvail then
      FInvalidateLinesMethod(-1, -1);
    FOnChangeList.CallNotifyEvents(self);
  end;
end;

procedure TSynEditSelection.SetHide(const AValue: Boolean);
begin
  if FHide = AValue then
    exit;
  FHide := AValue;
  FInvalidateLinesMethod(Min(FStartLinePos, FEndLinePos), Max(FStartLinePos, FEndLinePos));
  FOnChangeList.CallNotifyEvents(self);
end;

procedure TSynEditSelection.SetPersistent(const AValue: Boolean);
begin
  if FPersistent = AValue then
    exit;
  FPersistent := AValue;
  if (not FPersistent) and (FCaret <> nil) and
    not(FCaret.IsAtLineByte(StartLineBytePos) or FCaret.IsAtLineByte(EndLineBytePos)) then
    Clear;
end;

function UTF8FindNearestCharStart(UTF8Str: PChar; Len: integer; BytePos: integer): integer;
begin
  Result := 0;
  if (UTF8Str <> nil) and (Len > 0) and (BytePos >= 0) then
  begin
    Result := BytePos;
    if Result > Len then
      Result := Len - 1;
    if (Result > 0) and (ord(UTF8Str[Result]) and $11000000 = $10000000) then
    begin
      dec(Result);
      if (Result > 0) and (ord(UTF8Str[Result]) and $11000000 = $10000000) then
      begin
        dec(Result);
        if (Result > 0) and (ord(UTF8Str[Result]) and $11000000 = $10000000) then
        begin
          dec(Result);
          // should be four byte character
          if (ord(UTF8Str[Result]) and $11111000 <> $11110000) then
          begin
            // broken UTF8 character
            inc(Result, 3);
          end
          else
          begin
            // is four byte character
          end;
        end
        else if (ord(UTF8Str[Result]) and $11110000 <> $11100000) then
        begin
          // broken UTF8 character, should be three byte
          inc(Result, 2);
        end
        else
        begin
          // is three byte character
        end;
      end
      else if (ord(UTF8Str[Result]) and $11100000 <> $11000000) then
      begin
        // broken UTF8 character, should be two byte
        inc(Result);
      end
      else
      begin
        // is two byte character
      end;
    end;
  end;
end;

// Only needed if the Selection is set from External
function TSynEditSelection.AdjustBytePosToCharacterStart(Line: integer; BytePos: integer): integer;
var
  S: string;
begin
  Result := BytePos;
  if Result < 1 then
    Result := 1
  else if (Line >= 1) and (Line <= FLines.Count) then
  begin
    S := FLines[Line - 1];
    if (Result <= length(S)) and FLines.IsUtf8 then
      Result := UTF8FindNearestCharStart(PChar(Pointer(S)), length(S), Result - 1) + 1;
  end;
  // if Result <> BytePos then debugln(['Selection needed byte adjustment  Line=', Line, ' BytePos=', BytePos, ' Result=', Result]);
end;

function TSynEditSelection.GetFirstLineBytePos: TPoint;
begin
  if IsBackwardSel then
    Result := EndLineBytePos
  else
    Result := StartLineBytePos;
end;

function TSynEditSelection.GetLastLineBytePos: TPoint;
begin
  if IsBackwardSel then
    Result := StartLineBytePos
  else
    Result := EndLineBytePos;
end;

procedure TSynEditSelection.SetCaret(const AValue: TSynEditCaret);
begin
  if FCaret = AValue then
    exit;
  if FCaret <> nil then
    Caret.RemoveChangeHandler(DoCaretChanged);
  FCaret := AValue;
  if FCaret <> nil then
    Caret.AddChangeHandler(DoCaretChanged);
end;

function TSynEditSelection.SelAvail: Boolean;
begin
  if FHide then
    exit(False);
  if (FActiveSelectionMode = smColumn) then
  begin
    Result := (FStartBytePos <> FEndBytePos) and (FStartLinePos = FEndLinePos);
    if (not Result) and (FStartLinePos <> FEndLinePos) then
    begin
      // Todo: Cache values, but we need notification, if ines are modified (even only by change of tabwidth...)
      Result := Lines.LogicalToPhysicalPos(StartLineBytePos).x <> Lines.LogicalToPhysicalPos(EndLineBytePos).x;
    end;
  end
  else
    Result := (FStartBytePos <> FEndBytePos) or (FStartLinePos <> FEndLinePos);
end;

function TSynEditSelection.SelCanContinue(ACaret: TSynEditCaret): Boolean;
begin
  if SelAvail then
    exit(True);
  Result := (not FHide) and (FActiveSelectionMode = smColumn) and (FEndLinePos = ACaret.LinePos) and
    (FEndBytePos = ACaret.BytePos);
end;

function TSynEditSelection.IsBackwardSel: Boolean;
begin
  Result := (FStartLinePos > FEndLinePos) or ((FStartLinePos = FEndLinePos) and (FStartBytePos > FEndBytePos));
end;

procedure TSynEditSelection.SortSelectionPoints;
begin
  if IsBackwardSel then
  begin
    SwapInt(FStartLinePos, FEndLinePos);
    SwapInt(FStartBytePos, FEndBytePos);
  end;
end;

procedure TSynEditSelection.IgnoreNextCaretMove;
begin
  FIgnoreNextCaretMove := True;
end;

procedure TSynEditSelection.IncPersistentLock;
begin
  inc(FPersistentLock);
end;

procedure TSynEditSelection.DecPersistentLock;
begin
  dec(FPersistentLock);
  if (FPersistentLock = 0) and (FCaret <> nil) and FCaret.Locked then
    FLastCarePos := Point(FCaret.OldCharPos, FCaret.OldLinePos);
end;

procedure TSynEditSelection.Clear;
begin
  if Caret <> nil then
    StartLineBytePos := Caret.LineBytePos
  else
    StartLineBytePos := StartLineBytePos;
end;

{ TSynEditScreenCaret }

constructor TSynEditScreenCaret.Create(AHandleOwner: TComponent);
begin
  inherited Create;
  FHandleOwner := AHandleOwner;
  FVisible := False;
  FCurrentVisible := False;
  FCurrentCreated := False;
  FCurrentPosX := -1;
  FCurrentPosY := -1;
  FCurrentClippedWidth := -1;
  FClipExtraPixel := 0;
  FLockCount := 0;
end;

destructor TSynEditScreenCaret.Destroy;
begin
  DestroyCaret;
  inherited Destroy;
end;

procedure TSynEditScreenCaret.Hide;
begin
  HideCaret;
end;

procedure TSynEditScreenCaret.DestroyCaret(SkipHide: Boolean = False);
begin
  // if FCurrentCreated {and HandleAllocated }then begin
  // WinApi.Windows.DestroyCaret;
  // end;
  FCurrentCreated := False;
  FCurrentVisible := False;
  if not SkipHide then
    FVisible := False;
end;

procedure TSynEditScreenCaret.Lock;
begin
  inc(FLockCount);
end;

procedure TSynEditScreenCaret.Unlock;
begin
  dec(FLockCount);
  if (FLockCount = 0) then
  begin
    if (sclfUpdateDisplayType in FLockFlags) then
      UpdateDisplayType;
    if (sclfUpdateDisplay in FLockFlags) then
      UpdateDisplay;
  end;
end;

procedure TSynEditScreenCaret.SetClipRight(const AValue: integer);
begin
  if FClipRight = AValue then
    exit;
  FClipRight := AValue;
  UpdateDisplay;
end;

procedure TSynEditScreenCaret.SetCharHeight(const AValue: integer);
begin
  if FCharHeight = AValue then
    exit;
  FCharHeight := AValue;
  UpdateDisplayType;
end;

// function TSynEditScreenCaret.GetHandle: THandle;
// begin
// Result :=FHandleOwner.Handle;
// end;

// function TSynEditScreenCaret.GetHandleAllocated: Boolean;
// begin
// Result :=FHandleOwner.HandleAllocated;
// end;

procedure TSynEditScreenCaret.SetCharWidth(const AValue: integer);
begin
  if FCharWidth = AValue then
    exit;
  FCharWidth := AValue;
  UpdateDisplayType;
end;

procedure TSynEditScreenCaret.SetDisplayPos(const AValue: TPoint);
begin
  if (FDisplayPos.x = AValue.x) and (FDisplayPos.y = AValue.y) and (FVisible = FCurrentVisible) then
    exit;
  FDisplayPos := AValue;
  UpdateDisplay;
end;

procedure TSynEditScreenCaret.SetDisplayType(const AType: TSynCaretType);
begin
  if FDisplayType = AType then
    exit;
  FDisplayType := AType;
  UpdateDisplayType;
end;

procedure TSynEditScreenCaret.SetVisible(const AValue: Boolean);
begin
  if FVisible = AValue then
    exit;
  FVisible := AValue;
  UpdateDisplay;
end;

procedure TSynEditScreenCaret.UpdateDisplayType;
begin
  if FLockCount > 0 then
  begin
    Include(FLockFlags, sclfUpdateDisplayType);
    exit;
  end;
  Exclude(FLockFlags, sclfUpdateDisplayType);

  case FDisplayType of
    ctVerticalLine:
      begin
        FPixelWidth := 2;
        FPixelHeight := FCharHeight - 2;
        FOffsetX := -1;
        FOffsetY := 1;
        FExtraLinePixel := 1;
      end;
    ctBlock:
      begin
        FPixelWidth := FCharWidth;
        FPixelHeight := FCharHeight - 2;
        FOffsetX := 0;
        FOffsetY := 1;
        FExtraLinePixel := FCharWidth;
      end;
    ctHalfBlock:
      begin
        FPixelWidth := FCharWidth;
        FPixelHeight := (FCharHeight - 2) div 2;
        FOffsetX := 0;
        FOffsetY := FPixelHeight + 1;
        FExtraLinePixel := FCharWidth;
      end;
    ctHorizontalLine:
      begin
        FPixelWidth := FCharWidth;
        FPixelHeight := 2;
        FOffsetX := 0;
        FOffsetY := FCharHeight - 1;
        FExtraLinePixel := FCharWidth;
      end;
  end;
  CalcExtraLineChars;
  DestroyCaret(True);
  UpdateDisplay;
end;

procedure TSynEditScreenCaret.SetClipBottom(const AValue: integer);
begin
  if FClipBottom = AValue then
    exit;
  FClipBottom := AValue;
  UpdateDisplay;
end;

procedure TSynEditScreenCaret.SetClipExtraPixel(AValue: integer);
begin
  if FClipExtraPixel = AValue then
    exit;
{$IFDEF SynCaretDebug}
  debugln(['SynEditCaret ClipRect for HandleOwner=', FHandleOwner, ' ExtraPixel=', dbgs(AValue)]);
{$ENDIF}
  FClipExtraPixel := AValue;
  CalcExtraLineChars;
  UpdateDisplay;
end;

procedure TSynEditScreenCaret.SetClipLeft(const AValue: integer);
begin
  if FClipLeft = AValue then
    exit;
  FClipLeft := AValue;
  UpdateDisplay;
end;

procedure TSynEditScreenCaret.SetClipRect(const AValue: TRect);
begin
  if (FClipLeft = AValue.Left) and (FClipRight = AValue.Right) and (FClipTop = AValue.Top) and
    (FClipBottom = AValue.Bottom) then
    exit;
{$IFDEF SynCaretDebug}
  debugln(['SynEditCaret ClipRect for HandleOwner=', FHandleOwner, ' Rect=', dbgs(AValue)]);
{$ENDIF}
  FClipLeft := AValue.Left;
  FClipRight := AValue.Right;
  FClipTop := AValue.Top;
  FClipBottom := AValue.Bottom;
  UpdateDisplay;
end;

procedure TSynEditScreenCaret.SetClipTop(const AValue: integer);
begin
  if FClipTop = AValue then
    exit;
  FClipTop := AValue;
  UpdateDisplay;
end;

procedure TSynEditScreenCaret.CalcExtraLineChars;
var
  OldExtraChars: integer;
begin
  OldExtraChars := FExtraLineChars;
  FExtraLineChars := Max(0, FExtraLinePixel - FClipExtraPixel + FCharWidth - 1) div FCharWidth;
  if (FExtraLineChars <> OldExtraChars) and Assigned(FOnExtraLineCharsChanged) then
    FOnExtraLineCharsChanged(self);
end;

procedure TSynEditScreenCaret.UpdateDisplay;
begin
  if FLockCount > 0 then
  begin
    Include(FLockFlags, sclfUpdateDisplay);
    exit;
  end;
  Exclude(FLockFlags, sclfUpdateDisplay);

  if FVisible then
    ShowCaret
  else
    HideCaret;
end;

procedure TSynEditScreenCaret.ShowCaret;
var
  x, y, w: integer;
begin
  // if not HandleAllocated then
  // exit;
  x := FDisplayPos.x + FOffsetX;
  y := FDisplayPos.y + FOffsetY;
  w := FPixelWidth;
  if x + w >= FClipRight then
    w := FClipRight - x - 1;
  if (w <= 0) or (x < FClipLeft) or (x >= FClipRight) or (y < FClipTop) or (y >= FClipBottom) then
  begin
    HideCaret;
    exit;
  end;

  if (not FCurrentCreated) or (FCurrentClippedWidth <> w) then
  begin
{$IFDEF SynCaretDebug}
    debugln(['SynEditCaret CreateCaret for HandleOwner=', FHandleOwner, ' DebugShowCount=', FDebugShowCount, ' Width=',
      w, ' pref-width=', FPixelWidth, ' Height=', FPixelHeight, '  FCurrentCreated=', FCurrentCreated,
      ' FCurrentVisible=', FCurrentVisible]);
    FDebugShowCount := 0;
{$ENDIF}
    // if FCurrentCreated  then
    // LCLIntf.DestroyCaret(Handle);
    // // Create caret includes destroy
    // CreateCaret(, 0, w, FPixelHeight);
    FCurrentCreated := True;
    FCurrentVisible := False;
    FCurrentClippedWidth := w;
    FCurrentPosX := x - 1;
    // WinApi.Windows.SetCaretRespondToFocus(Handle, False); // Only for GTK
  end;
  if (x <> FCurrentPosX) or (y <> FCurrentPosY) then
  begin
{$IFDEF SynCaretDebug}
    debugln(['SynEditCaret SetPos for HandleOwner=', FHandleOwner, ' x=', x, ' y=', y]);
{$ENDIF}
//    WinApi.Windows.SetCaretPos(x, y);
    FCurrentPosX := x;
    FCurrentPosY := y;
  end;
  if (not FCurrentVisible) then
  begin
{$IFDEF SynCaretDebug}
    debugln(['SynEditCaret ShowCaret for HandleOwner=', FHandleOwner, ' FDebugShowCount=', FDebugShowCount]);
    inc(FDebugShowCount);
{$ENDIF}
//    if WinApi.Windows.ShowCaret(Handle) then
//      FCurrentVisible := True;
  end;
end;

procedure TSynEditScreenCaret.HideCaret;
begin
//  if not HandleAllocated then
//    exit;
  if not FCurrentCreated then
    exit;
  if FCurrentVisible then
  begin
{$IFDEF SynCaretDebug}
    debugln(['SynEditCaret HideCaret for HandleOwner=', FHandleOwner, ' FDebugShowCount=', FDebugShowCount]);
    dec(FDebugShowCount);
{$ENDIF}
//    if WinApi.Windows.HideCaret(Handle) then
//      FCurrentVisible := False;
  end;
end;

end.
