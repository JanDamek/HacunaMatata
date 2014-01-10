
//////////////////////////////////////////////////
//  CRControls
//  Copyright (c) 1998-2012 Devart. All right reserved.
//  CRGrid component
//////////////////////////////////////////////////

{$IFNDEF CLR}

{$I CRGrid.inc}

unit CRGrid;

{$ENDIF}

interface

uses
  SysUtils, Classes, {$IFDEF VER6P}Variants,{$ENDIF}
  Windows, Messages, Graphics, Controls, StdCtrls, ComCtrls,
  Grids, DBGrids, Menus, Dialogs, Forms, DB,
  CRTypes;

type
  TCRDBGrid = class;

{ TCRColumn }

  TSortOrder = (soNone, soAsc, soDesc);
  TSummaryMode = (smNone, smSum, smAvr, smMax, smMin, smLabel);

  TOnMemoClick = procedure (Sender: TObject; Column: TColumn) of object;

  TCRColumnTitle = class(TColumnTitle)
  private
    function GetCaption: string;
    function IsCaptionStored: boolean;

  protected
    procedure SetCaption(const Value: string);

  published
    property Caption: string read GetCaption write SetCaption stored IsCaptionStored;
  end;

  TCRColumn = class (TColumn)
  private
    FMinWidth: integer;
    FTotalString: string;
    FTotalValue: Variant;
    FTotalLoaded: boolean;
    FSummaryMode: TSummaryMode;
    FTotalFloat: extended;
    FTotalInt: int64;
    FFloatDigits: integer;
    FFloatPrecision: integer;
    FFloatFormat: TFloatFormat;
    FFilterExpression: string;
    FTableSpacePercent: double;

    function GetSortOrder: TSortOrder;
    procedure SetSortOrder(Value: TSortOrder);
    function GetSortSequence: integer;
    procedure SetSortSequence(Value: integer);
    function GetTotalString: string;
    function GetTotalValue: Variant;
    procedure SetSummaryMode(Value: TSummaryMode);
    procedure SetFloatDigits(const Value: integer);
    procedure SetFloatFormat(const Value: TFloatFormat);
    procedure SetFloatPrecision(const Value: integer);
    procedure SetFilterExpression(const Value: string);
    procedure SetWidth(const Value: integer);
    function GetWidth: integer;
    procedure SetVisible(Value: Boolean);
    function GetVisible: Boolean;
    procedure ResetTotal;
    procedure LoadTotal;
    procedure SetTotal;
    function CanBeSorted: boolean;
    function GetTitle: TCRColumnTitle;
    procedure SetTitle(Value: TCRColumnTitle);

  protected
    function CreateTitle: TColumnTitle; override;
    procedure ChangedTitle(Rebild: boolean);
    function GetFilterExpression(const RawFilter: string): string;

  public
    constructor Create(Collection: TCollection); override;
    procedure Assign(Source: TPersistent); override;
    property TotalString: string read GetTotalString write FTotalString;
    property TotalValue: Variant read GetTotalValue;

  published
    property Width: integer read GetWidth write SetWidth;
    property Visible: Boolean read GetVisible write SetVisible;
    property FilterExpression: string read FFilterExpression write SetFilterExpression;
    property MinWidth: integer read FMinWidth write FMinWidth default 0;
    property SortOrder: TSortOrder read GetSortOrder write SetSortOrder default soNone;
    property SortSequence: integer read GetSortSequence write SetSortSequence default 0;
    property SummaryMode: TSummaryMode read FSummaryMode write SetSummaryMode default smNone;
    property FloatFormat: TFloatFormat read FFloatFormat write SetFloatFormat default ffGeneral;
    property FloatPrecision: integer read FFloatPrecision write SetFloatPrecision default 0;
    property FloatDigits: integer read FFloatDigits write SetFloatDigits default 0;
    property Title: TCRColumnTitle read GetTitle write SetTitle;
  end;

  TCRDBGridColumns = class(TDBGridColumns)
  private
    function GetColumn(Index: Integer): TCRColumn;
    procedure SetColumn(Index: Integer; Value: TCRColumn);
    procedure ColumnAdded;
  public
    property Items[Index: Integer]: TCRColumn read GetColumn write SetColumn; default;
  end;

{ TGridTitleEdit }

  TCRGridTitleEdit = class(TCustomStaticText)
  private
    FCRDBGrid: TCRDBGrid;
    FEdit: TEdit;
    FAsFilter: boolean;
    FActiveColumn: TColumn;
    FFilterExpressions: array of string;
    FEditingFilter: boolean;

    procedure SetCRDBGrid(const Value: TCRDBGrid);
    procedure FEditKeyPress(Sender: TObject; var Key: char);
    procedure WMMouseWheel(var Message: TWMMouseWheel); message WM_MOUSEWHEEL;
    procedure FEditKeyDown(Sender: TObject; var Key: word;
      Shift: TShiftState);
    procedure FEditChange(Sender: TObject);
    procedure FEditExit(Sender: TObject);
    procedure ProcessEdit;
    procedure WMGetDlgCode(var Message: TWMGetDlgCode); message WM_GETDLGCODE;
    procedure GotoUpperCell;
    procedure GotoLowerCell;
    procedure GotoNextCell;
    procedure GotoPrevCell;
    procedure SetEditingFilter(const Value: boolean);
    procedure PostFilter;

  protected
    procedure PaintWindow(DC: HDC); override;
    procedure KeyDown(var Key: word; Shift: TShiftState); override;
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState;
      X, Y: integer); override;
    procedure DoExit; override;
    procedure WMChar(var Message: TWMChar); message WM_CHAR;

  public
    constructor Create(AOwner: TComponent); override;
    procedure SetFocus; override;
    procedure ActivateAt(ARect: TRect; ActiveColumn: TColumn; AsFilter: boolean);
    procedure SetClientRect(ARect: TRect);
    procedure StartEdit;
    procedure StopEdit(AcceptChanges: boolean);

    property CRDBGrid: TCRDBGrid read FCRDBGrid write SetCRDBGrid;
    property Edit: TEdit read FEdit;
    property EditingFilter: boolean read FEditingFilter write SetEditingFilter;
  end;

{ TMemoEditorForm }

  TMemoEditorForm = class (TCustomForm)
  private
    FMemo: TMemo;
    FOKBtn: TButton;
    FCancelBtn: TButton;
    FReadOnly: boolean;
    FCheckBox: TCheckBox;
    procedure SetReadOnly(const Value: boolean);
    procedure MemoKeyDown(Sender: TObject; var Key: word;
      Shift: TShiftState);
    procedure CheckBoxClick(Sender: tobject);
  public
    constructor Create(AOwner: TComponent); override;
    function CloseQuery: boolean; override;
    property ReadOnly: boolean read FReadOnly write SetReadOnly;
  end;

{ TCRDBGrid }

  TCRDBGridOptionEx = (dgeEnableSort, dgeFilterBar, dgeLocalFilter, {$IFNDEF DAC450}dgeLocalSorting, {$ENDIF}dgeRecordCount,
    dgeSearchBar, dgeStretch, dgeSummary);
  TCRDBGridOptionsEx = set of TCRDBGridOptionEx;

  TGridDrawStateEx = set of (geHighlight, geActiveRow, geMultiSelected);

  TGetCellParamsEvent = procedure (Sender: TObject; Field: TField;
    AFont: TFont; var Background: TColor; State: TGridDrawState; StateEx: TGridDrawStateEx) of object;

  TSortColInfo = class
  public
    Index: integer;
    Desc: boolean;
  end;

  TIndicatorColButton = (icbNone, icbMenu, icbFilter, icbSearch);

{$IFDEF VER6P}
  TCRGridDataLink = class(TGridDataLink)
  protected
    FDataSetChanging: boolean;

    procedure DataSetChanged; override;
  end;
{$ENDIF}

  TCRDBGrid = class(TCustomDBGrid)
  private
    FDefaultDrawing: boolean;
    FOptionsEx: TCRDBGridOptionsEx;
    FSoft: boolean;
    FOnGetCellParams: TGetCellParamsEvent;
    FExecSorting: boolean;
    FExecColAjust: boolean;
    FSortInfo: TList;
    FActiveRowSelected: boolean;
    FTitleButtonDown: integer;
    FTitleBarUp: boolean;
    FOldTitleButtonDown: integer;
    FCellButtonDown: integer;
    FCellButtonRow: integer;
    FCellButtonCol: integer;
    FCellButtonPressed: boolean;
    FCellButtonRect: TRect;
    FCellButtonBRect: TRect;
    FTotalYOffset: integer;
    FOnMemoClick: TOnMemoClick;
    FLevelDelimiterChar: char;
    FIndicatorColBtnDown: TIndicatorColButton;
    FOldIndicatorColBtnDown: TIndicatorColButton;
    FOptionsMenu: TPopupMenu;
    FOptionsMenuDef: TPopupMenu;
    CRGridTitleEdit: TCRGridTitleEdit;
    FStatusRect: TRect;
    FFiltered: boolean;
    FContinueEditingFilter: boolean;
    FMemoWidth: integer;
    FMemoHeight: integer;
    FMemoWordWrap: boolean;

    procedure SetOptionsEx(Value: TCRDBGridOptionsEx);
    procedure UpdateHeaderHeight;
    procedure RecordChanged(Field: TField);
    procedure DrawButton(X,Y: integer; State: boolean);
    function  IsOnButton(X, Y: integer): boolean;
    function  GetButtonRect(Cell: TGridCoord): TRect;
    procedure SetLevelDelimiterchar(const Value: char);
    procedure WMSetCursor(var Message: TWMSetCursor); message WM_SetCursor;
    function CalcSearchBar(Column: TColumn): TRect;
    function CalcFilterBar(Column: TColumn): TRect;
    function MouseInFilterBar(X, Y: integer; Column: TColumn = nil): boolean;
    function MouseInFilterEdit(X, Y: integer; Column: TColumn = nil): boolean;
    function MouseInSortBar(X, Y: integer; Column: TColumn = nil): boolean;
    function MouseInSortEdit(X,Y: integer;Column: TColumn = nil): boolean;
    function MouseInLowerstLevel(X, Y: integer; Column: TColumn = nil): boolean;
    procedure DoOnMemoClick(Column: TColumn);

    procedure DrawTitleBarCell(Canvas: TCanvas; Column: TColumn; Rect: TRect; Text: string);
    procedure DrawTitleIndicatorCell(Canvas: TCanvas; ARect: TRect);
    function GetIndicatorButton(X,Y: integer): TIndicatorColButton;
    procedure IndicatorClick(Button: TIndicatorColButton; X, Y: integer);
    procedure BuildMenu;
    procedure FilteredItemClick(Sender: TObject);
    procedure FilterItemClick(Sender: TObject);
    procedure SearchItemClick(Sender: TObject);
    procedure CalcTableSpacePercent;
    procedure SetFiltered(const Value: boolean);
    procedure UpdateRowCount;
    function GetColumns: TCRDBGridColumns;
    procedure SetColumns(const Value: TCRDBGridColumns);

  protected
    FHeaderHeight: integer;
    FExecSizing: boolean;

    function GetClientRect: TRect; override;
    procedure Loaded; override;
    function CreateColumns: TDBGridColumns; override;
  {$IFDEF VER6P}
    function CreateDataLink: TGridDataLink; override;
  {$ENDIF}

    procedure Reorder;
    function  FindSortColInfo(Index: integer; var SortNum: integer): TSortColInfo;

    procedure ColWidthsChanged; override;
    procedure Resize; override;
    procedure ResizeColumns(ResizedColumn: integer = -1);
    function  EndColumnDrag(var Origin, Destination: integer;
      const MousePt: TPoint): boolean; override;

    procedure DrawColumnCell(const Rect: TRect; DataCol: integer;
      Column: TColumn; State: TGridDrawState); override;
    procedure GetCellProps(Field: TField; AFont: TFont; var Background: TColor;
      State: TGridDrawState; StateEx:TGridDrawStateEx); dynamic;
    procedure DrawCell(ACol, ARow: Longint; ARect: TRect; AState: TGridDrawState); override;

    procedure KeyDown(var Key: word; Shift: TShiftState); override;
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState;
      X, Y: integer); override;
    procedure MouseMove(Shift: TShiftState; X, Y: integer); override;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState;
      X, Y: integer); override;

    procedure LinkActive(Value: boolean); override;
    procedure Paint;override;
    procedure ResetTotals;
    procedure LoadTotals;
    function CanEditShow: boolean; override;
    procedure TopLeftChanged; override;
    procedure DoExit; override;
    procedure WMMouseWheel(var Message: TWMMouseWheel); message WM_MOUSEWHEEL;
    procedure TitleClick(Column: TColumn); override;

    procedure MoveColRow(ACol, ARow: Longint; MoveAnchor, Show: Boolean);
    function  DataToRawColumn(ACol: Integer): Integer;
    procedure InvalidateCol(ACol: Longint);
    procedure InvalidateRow(ARow: Longint);

    procedure LayoutChanged; override;

    property DefaultRowHeight;
    property DataLink;
  public
    function GetGridSize: integer;
    constructor Create(Owner: TComponent); override;
    procedure DataChanged; //override;
    destructor Destroy; override;

    procedure ClearSorting;
    procedure ClearFilters;
    procedure ActivateFilterEdit(Column: TColumn);
    procedure ActivateSearchEdit(Column: TColumn);

    property Canvas;
    property SelectedRows;
    procedure CalcTitleLevel(Level: integer; var aRect: TRect);
    function GetTitleLevel(Level: integer): TRect;
    procedure ApplyFilter;
    procedure AdjustColumns;
    property Col;
    property Row;
    property TopRow;
    property LeftCol;
    property OptionsMenu: TPopupMenu read FOptionsMenu write FOptionsMenu;

  published
    property DefaultDrawing: boolean read FDefaultDrawing write FDefaultDrawing
      default True;
    property LevelDelimiterChar: char read FLevelDelimiterchar write SetLevelDelimiterchar default '|';
    property Filtered: boolean read FFiltered write SetFiltered default True;
    property OptionsEx: TCRDBGridOptionsEx read FOptionsEx write SetOptionsEx
      default [dgeEnableSort, dgeLocalFilter, {$IFNDEF DAC450}dgeLocalSorting, {$ENDIF}dgeRecordCount];
    property OnMemoClick: TOnMemoClick read FOnMemoClick write FOnMemoClick;
    property OnGetCellParams: TGetCellParamsEvent read FOnGetCellParams
     write FOnGetCellParams;

    property Align;
    property Anchors;
    property BiDiMode;
    property BorderStyle;
    property Color;
    property Columns: TCRDBGridColumns read GetColumns write SetColumns stored False;
    property Constraints;
    property Ctl3D;
    property DataSource;
    property DragCursor;
    property DragMode;
    property Enabled;
    property FixedColor;
    property Font;
    property ImeMode;
    property ImeName;
    property Options;
    property ParentBiDiMode;
    property ParentColor;
    property ParentCtl3D;
    property ParentFont;
    property ParentShowHint;
    property PopupMenu;
    property ReadOnly;
    property ShowHint;
    property TabOrder;
    property TabStop;
    property TitleFont;
    property Visible;
    property OnCellClick;
    property OnColEnter;
    property OnColExit;
    property OnColumnMoved;
    property OnDrawDataCell;  { obsolete }
    property OnDrawColumnCell;
    property OnDblClick;
    property OnDragDrop;
    property OnDragOver;
    property OnEditButtonClick;
    property OnEndDock;
    property OnEndDrag;
    property OnEnter;
    property OnExit;
    property OnKeyDown;
    property OnKeyPress;
    property OnKeyUp;
    property OnMouseDown;
    property OnMouseMove;
    property OnMouseUp; 
    property OnStartDock;
    property OnStartDrag;
    property OnTitleClick;
  end;

resourcestring
  SFiltered = 'Filtered';
  SFilterBar = 'Filter bar';
  SSearchBar = 'Search bar';
  sWordWrap = 'Word Wrap'; 
  SOK = '&OK';
  SCancel = '&Cancel';
  SClose = '&Close';
  fmtModifiedWarning = 'Field "%s" is modified. Save?';

implementation

uses
  Math,
{$IFDEF CLR}
  System.Threading, Types, WinUtils,
{$ENDIF}
  CRFunctions, CRParser, MemDS, DBAccess;

{$R CRGrid.res}

var
  bmpSortAsc: TBitmap;
  bmpSortDesc: TBitmap;
  DrawBitmap: TBitmap;
  bmpFilter: TBitmap;
  bmpSearch: TBitmap;
  bmpMenu: TBitmap;
  bmpActiveFilter: TBitmap;
  bmpEditMode: TBitmap;
  UserCount: integer;

type
  TInthernalEdit = class(TEdit)
  end;

{$IFNDEF CLR}
  _TCustomGrid = class(TCustomGrid)
  end;
{$ENDIF}

function GetCaptionDepth(const Str: string; Delim: char): integer;
var
  i: integer;
  St: string;
begin
  Result := 0;
  if Str = '' then
    Exit;
  Result := 1;
  i := Pos(Delim, Str);
  St := Str;
  while i > 0 do begin
    Inc(Result);
    St[i] := #255;
    i := Pos(Delim, St);
  end;
end;

function GetCaptionLevel(const Str: string; Level: integer; Delim: char): string;
var
  i,j: integer;
  St: string;
begin
  j := 0;
  Result := '';
  if Str = '' then
    Exit;
  i := Pos(Delim, Str);
  St := Str;
  while (Level > 0) and (I > 0) do begin
    Dec(Level);
    St[i] := #255;
    if Level <= -2 then begin
      Result := Copy(St, j + 1, i - 1);
      exit;
    end;
    j := i;
    i := Pos(Delim, St);
  end;
  if Level <= 0 then begin
    if i = 0 then
      i := Length(St) + j
    else
      Dec(i);
    Result := Copy(Str, j + 1, i - j);
    exit;
  end;
end;

{ TCRColumn }

function TCRColumn.GetSortOrder: TSortOrder;
var
  SortColInfo: TSortColInfo;
  NumSort: integer;
begin
  if not CanBeSorted then begin
    Result := soNone;
    exit;
  end;
  SortColInfo := TCRDBGrid(Grid).FindSortColInfo(Index, NumSort);

  if SortColInfo <> nil then
    if SortColInfo.Desc then
      Result := soDesc
    else
      Result := soAsc
  else
    Result := soNone;
end;

procedure TCRColumn.SetSortOrder(Value: TSortOrder);
var
  SortColInfo: TSortColInfo;
  NumSort: integer;
begin
  if not CanBeSorted then
    Exit;

  SortColInfo := TCRDBGrid(Grid).FindSortColInfo(Index, NumSort);
  if SortColInfo <> nil then begin
    case Value of
      soNone: begin
        if NumSort > 0 then
          Dec(NumSort);
        TSortColInfo(TCRDBGrid(Grid).FSortInfo[NumSort]).Free;
        TCRDBGrid(Grid).FSortInfo.Delete(NumSort)
      end;
      soAsc:
        SortColInfo.Desc := False;
      soDesc:
        SortColInfo.Desc := True;
    end;
    TCRDBGrid(Grid).Reorder;
  end
  else
    if Value <> soNone then begin
      SortColInfo := TSortColInfo.Create;
      SortColInfo.Index := Index;
      SortColInfo.Desc := Value = soDesc;
      TCRDBGrid(Grid).FSortInfo.Add(SortColInfo);
      TCRDBGrid(Grid).Reorder;
    end;
end;

function TCRColumn.GetSortSequence: integer;
begin
  TCRDBGrid(Grid).FindSortColInfo(Index, Result);
end;

procedure TCRColumn.SetFilterExpression(const Value: string);
begin
    FFilterExpression := Value;
end;

procedure TCRColumn.SetSortSequence(Value: integer);
begin
end;

function TCRColumn.GetTotalString: string;
begin
  if Assigned(Field) and (Field.DataSet.Active) then
    if (FSummaryMode = smNone) then
      Result := ''
    else begin
      if not FTotalLoaded then
        LoadTotal;
      Result := FTotalString
    end
  else
    Result := '';
end;

function TCRColumn.GetTotalValue: Variant;
begin
  Result := Unassigned;
  if Assigned(Field) and (Field.DataSet.Active) then
    if  (SummaryMode = smLabel) then
      Result := FTotalString
    else
    if (FSummaryMode = smNone) then
      Result := Unassigned
    else begin
      if not FTotalLoaded then
        LoadTotal;
      Result := FTotalValue
    end
  else
    Result := Unassigned;
end;

procedure TCRColumn.ResetTotal;
begin
  FTotalLoaded := False;
end;

procedure TCRColumn.LoadTotal;
begin
  if (SummaryMode <> smLabel) and Assigned (Field) then begin
    TCRDBGrid(Grid).LoadTotals;
    if Assigned (Field) then
    case Field.DataType of
      ftSmallint, ftInteger, ftWord, ftLargeint:
        if SummaryMode = smAvr then
        begin
          FTotalValue := FTotalFloat;
          FTotalString := FloatToStrF(FTotalFloat, FFloatFormat, FFloatPrecision, FFloatDigits)
        end
        else
        begin
          FTotalValue := {$IFNDEF VER6P}Integer({$ENDIF}FTotalInt{$IFNDEF VER6P}){$ENDIF};
          FTotalString := IntToStr(FTotalInt);
        end;
      ftFloat, ftCurrency:
      begin
        FTotalValue := FTotalFloat; 
        FTotalString := FloatToStrF(FTotalFloat, FFloatFormat, FFloatPrecision, FFloatDigits);
      end;
      else
      begin
        FTotalValue := Unassigned;
        FTotalString := '';
      end;
    end
  end;
end;

procedure TCRColumn.SetSummaryMode(Value: TSummaryMode);
begin
  if Value <> smNone then
    if (Value <> smLabel) and Assigned(Field) then
      if not (Field.DataType in [ftSmallint, ftInteger, ftWord, ftLargeint, ftFloat, ftCurrency]) then
        Value := smNone;
  if FSummaryMode <> Value then begin
    FSummaryMode := Value;
    ResetTotal;
    if Assigned(Grid) then
      Grid.Invalidate;
  end;
end;

procedure TCRColumn.SetTotal;
begin
  FTotalLoaded := True;
  if (SummaryMode <> smLabel) and Assigned (Field) then
    case Field.DataType of
      ftSmallint, ftInteger, ftWord, ftLargeint:
        if SummaryMode = smAvr then
        begin
          FTotalValue := FTotalFloat;
          FTotalString := FloatToStrF(FTotalFloat, FFloatFormat, FFloatPrecision, FFloatDigits)
        end
        else
        begin
          FTotalValue := {$IFNDEF VER6P}Integer({$ENDIF}FTotalInt{$IFNDEF VER6P}){$ENDIF};
          FTotalString := IntToStr(FTotalInt);
        end;
      ftFloat, ftCurrency:
      begin
        FTotalValue := FTotalFloat; 
        FTotalString := FloatToStrF(FTotalFloat, FFloatFormat, FFloatPrecision, FFloatDigits);
      end;
      else
      begin
        FTotalValue := Unassigned;
        FTotalString := '';
      end;
    end
end;

procedure TCRColumn.SetFloatDigits(const Value: integer);
begin
  FFloatDigits := Value;
  if (SummaryMode <> smLabel) and Assigned (Field) then begin
    case Field.DataType of
      ftSmallint, ftInteger, ftWord, ftLargeint:
        if SummaryMode = smAvr then
          FTotalString := FloatToStrF(FTotalFloat,FFloatFormat,FFloatPrecision,FFloatDigits);
      ftFloat, ftCurrency:
        FTotalString := FloatToStrF(FTotalFloat,FFloatFormat,FFloatPrecision,FFloatDigits);
    end;
    if Assigned(Grid)then
      Grid.Invalidate;
  end;
end;

procedure TCRColumn.SetFloatFormat(const Value: TFloatFormat);
begin
  FFloatFormat := Value;
  if (SummaryMode <> smLabel) and Assigned(Field) then begin
    case Field.DataType of
      ftSmallint, ftInteger, ftWord, ftLargeint:
        if SummaryMode = smAvr then
          FTotalString := FloatToStrF(FTotalFloat, FFloatFormat, FFloatPrecision, FFloatDigits);
      ftFloat, ftCurrency:
        FTotalString := FloatToStrF(FTotalFloat, FFloatFormat, FFloatPrecision, FFloatDigits);
    end;
    if Assigned(Grid) then
      Grid.Invalidate;
  end;
end;

procedure TCRColumn.SetFloatPrecision(const Value: integer);
begin
  FFloatPrecision := Value;
  if (SummaryMode <> smLabel) and Assigned(Field) then begin
    case Field.DataType of
      ftSmallint, ftInteger, ftWord, ftLargeint:
        if SummaryMode = smAvr then
          FTotalString := FloatToStrF(FTotalFloat, FFloatFormat, FFloatPrecision, FFloatDigits);
      ftFloat, ftCurrency:
        FTotalString := FloatToStrF(FTotalFloat, FFloatFormat, FFloatPrecision, FFloatDigits);
    end;
    if Assigned(Grid) then
      Grid.Invalidate;
  end;
end;

procedure TCRColumn.Assign(Source: TPersistent);
begin
  if Source is TCRColumn then begin
    if Assigned(Collection) then
      Collection.BeginUpdate;
    inherited Assign(Source);
    try
      FSummaryMode := TCRColumn(Source).FSummaryMode;
      FMinWidth := TCRColumn(Source).FMinWidth;
      FTotalString := TCRColumn(Source).FTotalString;
      FTotalValue := TCRColumn(Source).FTotalValue;
      FTotalLoaded := TCRColumn(Source).FTotalLoaded;
      FSummaryMode := TCRColumn(Source).FSummaryMode;
      FTotalFloat := TCRColumn(Source).FTotalFloat;
      FTotalInt := TCRColumn(Source).FTotalInt;
      FFloatDigits := TCRColumn(Source).FFloatDigits;
      FFloatPrecision := TCRColumn(Source).FFloatPrecision;
      FFloatFormat := TCRColumn(Source).FFloatFormat;
    finally
      if Assigned(Collection) then
        Collection.EndUpdate;
    end;
  end
  else
    inherited Assign(Source);
end;

function TCRColumn.GetTitle: TCRColumnTitle;
begin
  Result := TCRColumnTitle(inherited Title);
end;

procedure TCRColumn.SetTitle(Value: TCRColumnTitle);
begin
  inherited Title := Value;
end;

function TCRColumn.CanBeSorted: boolean;
begin
  if Assigned(Field) then
    Result := {$IFNDEF DAC600} (Field.FieldKind = fkData) and {$ENDIF} not (Field.DataType in [ftFmtMemo,
      ftMemo{$IFNDEF VER4}, ftOraClob {$ENDIF}{$IFDEF VER10P}, ftWideMemo{$ENDIF}])
  else
    Result := False;
end;

function TCRColumn.GetFilterExpression(const RawFilter: string): string;

  function GetSignString(var ConstStr: string): string;
  var
    Buf: string;
  begin
    Result := '';
    ConstStr := '';
    Buf := Trim(RawFilter);
    if Buf = '' then
      Exit;

    case Buf[1] of
      '=': begin
        Result := '=';
        ConstStr := Copy(Buf, 2, Length(Buf) - 1);
      end;
      '<', '>': begin
        if (Length(Buf) >= 2) and ((Buf[2] = '=') or (Buf[2] = '>')) then begin
          Result := Copy(Buf, 1, 2);
          ConstStr := Copy(Buf, 3, Length(Buf) - 2);
        end
        else begin
          Result := Buf[1];
          ConstStr := Copy(Buf, 2, Length(Buf) - 1);
        end;
      end;
      else
      begin
        Result := '=';
        ConstStr := Copy(Buf, 1, Length(Buf));
      end;
    end;
    ConstStr := TrimLeft(ConstStr);
  end;

var
  Sign, ConstStr: string;
  s: string;
  i: integer;
begin
  Result := '';
  if RawFilter = '' then
    Exit;
  if Assigned (Field) then begin
    Sign := GetSignString(ConstStr);
    if (Sign = '') or (ConstStr = '') then
      Exit;
    if (UpperCase(Trim(ConstStr)) <> 'NULL') or (Field.DataType in [ftString,ftWideString]) then
    case Field.DataType of
      ftSmallint, ftInteger, ftWord, ftLargeint:
        StrToInt(ConstStr); // test for exception
      ftFloat, ftCurrency:
        StrToFloat(ConstStr); // test for exception
      ftDate: begin
        StrToDate(ConstStr); // test for exception
        ConstStr := '''' + ConstStr + '''';
      end;
      ftDateTime: begin
        StrToDateTime(ConstStr); // test for exception
        ConstStr := '''' + ConstStr + '''';
      end;
      ftTime: begin
        StrToTime(ConstStr); // test for exception
        ConstStr := '''' + ConstStr + '''';
      end;
      ftString,ftWideString: begin
        if not (dgeLocalFilter in TCRDBGrid(Grid).OptionsEx)
        and ((Sign = '=') or (Sign = '<>')) then begin
          for i := 1 to Length(ConstStr) do
            if ConstStr[i] = '*' then
              ConstStr[i] := '%';
          if Sign = '=' then
            Sign := ' LIKE '
          else
            Sign := ' NOT LIKE ';
        end;
        if (Field.DataSet is TCustomDADataSet) and (TCustomDADataSet(Field.DataSet).Options.QuoteNames) then
          Result := TDBAccessUtils.QuoteName(TCustomDADataSet(Field.DataSet), Field.FieldName) + Sign +
            AnsiQuotedStr(ConstStr,'''')
        else
          if (Trim(ConstStr) = '''''') or (Trim(ConstStr) = '""') then
            Result := '(' + Field.FieldName + Sign + 'null or ' + Field.FieldName + Sign + ''''')'
          else
            Result := Field.FieldName + Sign + AnsiQuotedStr(ConstStr, '''');
        Exit;
      end;
      ftBoolean: begin       /// (cr12227)
        if (TBooleanField(Field).DisplayValues <> '') and (pos(';', TBooleanField(Field).DisplayValues) > 0) then begin
          s := copy(TBooleanField(Field).DisplayValues, 0, pos(';', TBooleanField(Field).DisplayValues) - 1);
          if AnsiUpperCase(ConstStr) = AnsiUpperCase(s) then begin
            result := Field.FieldName + Sign + 'True';
            Exit;
          end
          else begin
            s := copy(TBooleanField(Field).DisplayValues, pos(';', TBooleanField(Field).DisplayValues) + 1, length(TBooleanField(Field).DisplayValues));
            if AnsiUpperCase(ConstStr) = AnsiUpperCase(s) then begin
              result := Field.FieldName + Sign + 'False';
              Exit;
            end;
          end;
        end;
      end;
    end;
  end;
  Result := Field.FieldName + Sign + ConstStr;
end;

procedure TCRColumn.ChangedTitle(Rebild: boolean);
begin
  if Rebild then
    if Assigned(Grid) then
      TCRDBGrid(Grid).LayoutChanged;
end;

function TCRColumn.CreateTitle: TColumnTitle;
begin
  Result := TCRColumnTitle.Create(Self);
end;

constructor TCRColumn.Create(Collection: TCollection);
begin
  inherited;
  FMinWidth := 0;
  TCRDBGridColumns(Collection).ColumnAdded;
end;

procedure TCRColumn.SetWidth(const Value: integer);
begin
  if Value > FMinWidth then
    inherited Width := Value
  else
    inherited Width := FMinWidth
  //if assigned(grid) then
  //  FTableSpaceProcent := Width / TCRDBGrid(grid).GetGridSize;
end;

function TCRColumn.GetWidth: integer;
begin
  Result := inherited Width;
end;

procedure TCRColumn.SetVisible(Value: Boolean);
var
  OldVisible: boolean;
begin
  OldVisible := inherited Visible;

  inherited Visible := Value;

  if (OldVisible <> Value) and Assigned(Grid) and
    (dgeStretch in TCRDBGrid(Grid).OptionsEx) and (not TCRDBGrid(Grid).FExecSizing) then
  begin
    TCRDBGrid(Grid).FExecSizing := True;
    TCRDBGrid(Grid).ResizeColumns;
    TCRDBGrid(Grid).FExecSizing := False;
  end;
end;

function TCRColumn.GetVisible: Boolean;
begin
  Result := inherited Visible;
end;

{ TCRDBGridColumns }

function TCRDBGridColumns.GetColumn(Index: Integer): TCRColumn;
begin
  Result := TCRColumn(inherited Items[Index]);
end;

procedure TCRDBGridColumns.SetColumn(Index: Integer; Value: TCRColumn);
begin
  inherited Items[Index] := Value;
end;

procedure TCRDBGridColumns.ColumnAdded;
begin
  TCRDBGrid(Grid).CalcTableSpacePercent;
end;

{ TCRDBGrid }

procedure UsesBitmap;
begin
  if UserCount = 0 then
    DrawBitmap := TBitmap.Create;
  Inc(UserCount);
end;

procedure ReleaseBitmap;
begin
  Dec(UserCount);
  if UserCount = 0 then
    DrawBitmap.Free;
end;

constructor TCRDBGrid.Create(Owner: TComponent);
begin
  inherited Create(Owner);
  Columns.State := csDefault;
  FSortInfo := TList.Create;
  FOptionsMenuDef := TPopupMenu.Create(Self);
  FFiltered := True;
  UsesBitmap;
  FOnMemoClick := nil;
  FLevelDelimiterChar := '|';
  inherited DefaultDrawing := False;
  FDefaultDrawing := True;
  FSoft := False;
  SetOptionsEx([dgeEnableSort, dgeLocalFilter, {$IFNDEF DAC450}dgeLocalSorting, {$ENDIF}dgeRecordCount]);
  DefaultColWidth := 60;  // DEBUG

  FExecSizing := False;
  FTitleButtonDown := -1;
  FOldTitleButtonDown := -1;
  FIndicatorColBtnDown := icbNone;
  FOldIndicatorColBtnDown := icbNone;
  FCellButtonDown := -1;
  CRGridTitleEdit := TCRGridTitleEdit.Create(Self);
  InsertControl(CRGridTitleEdit);
  BuildMenu;
end;

procedure TCRDBGRid.ActivateSearchEdit(Column: TColumn);
var
  CellRect: TRect;
begin
  if not (Assigned(Column) and (dgeSearchBar in OptionsEx)) then
    Exit;
  CellRect := CalcSearchBar(Column);
  InflateRect(CellRect, -5, -5);
  if not (dgRowLines in Options) then
    Dec(CellRect.Top);
  CRGridTitleEdit.ActivateAt(CellRect, Column, False);
end;

destructor TCRDBGrid.Destroy;
begin
  ReleaseBitmap;
  ClearSorting;
  ClearFilters;
  FSortInfo.Free;
  FOptionsMenuDef.Free;

  inherited;
end;

procedure TCRDBGrid.Loaded;
var
  Stretched: Boolean;
begin
  Stretched := False;
  if dgeStretch in OptionsEx then begin
    Stretched := True;
    Exclude(FOptionsEx, dgeStretch);
  end;
  inherited;
  if Stretched then Include(FOptionsEx, dgeStretch);
  CalcTableSpacePercent;
  FOptionsMenuDef.Items[0].Checked := FFiltered;
  FOptionsMenuDef.Items[2].Checked := dgeFilterBar in OptionsEx;
  FOptionsMenuDef.Items[3].Checked := dgeSearchBar in OptionsEx;

end;

function TCRDBGrid.CreateColumns: TDBGridColumns;
begin
  Result := TCRDBGridColumns.Create(Self, TCRColumn);
end;

{$IFDEF VER6P}
function TCRDBGrid.CreateDataLink: TGridDataLink;
begin
  Result := TCRGridDataLink.Create(Self);
end;
{$ENDIF}

procedure TCRDBGrid.Resize;
begin
  inherited;

  CRGridTitleEdit.StopEdit(False);
  if (dgeStretch in FOptionsEx) and not(csLoading in ComponentState) and
     (not FExecSizing) then begin
    FExecSizing := True;
    try
      ResizeColumns;
    finally
      FExecSizing := False;
    end;
  end;
  if CRGridTitleEdit.Focused then begin
    if CRGridTitleEdit.FAsFilter then
      ActivateFilterEdit(CRGridTitleEdit.FActiveColumn)
    else
      ActivateSearchEdit(CRGridTitleEdit.FActiveColumn);
  end;
  Invalidate;
end;

procedure TCRDBGrid.ColWidthsChanged;
var
  i: integer;
  ResizedColumn: integer;
begin
  if (dgeStretch in FOptionsEx) and not(csLoading in ComponentState) and
     (not FExecSizing) then begin
    FExecSizing := True;
    ResizedColumn := -1;
    for i := 0 to Columns.Count - 1 do
      if ColWidths[i + IndicatorOffset] <> Columns[i].Width then begin
        ResizedColumn := i;
        break;
      end;

    if ResizedColumn <> -1 then begin
      if ColWidths[ResizedColumn + IndicatorOffset] <= TCRColumn(Columns[ResizedColumn]).MinWidth then
         ColWidths[ResizedColumn + IndicatorOffset] := TCRColumn(Columns[ResizedColumn]).MinWidth;

      ResizeColumns(ResizedColumn);
      //ResizeColumns(-1);
    end;

    FExecSizing := False;
  end
  else
    if not (csLoading in ComponentState) and (not FExecSizing) then
      CalcTableSpacePercent;

  inherited;
end;

function  TCRDBGrid.GetGridSize: integer;
begin
  Result := ClientWidth - 1;
  if dgIndicator in Options then
    Dec(Result, IndicatorWidth);
  if dgColLines in Options then
    Dec(Result, Columns.Count*GridLineWidth);
end;

procedure TCRDBGrid.ResizeColumns(ResizedColumn: integer);
const
  MinWidth = 10;
var
  i: integer;
  GridSize, ColumnsSize:integer;
  UnresizedSize: integer;
  K: double;
  Curr,Prev: double;
  Width: integer;
  MinimizeRest: boolean;
  VisiblePercent: double;
  //Sized       : integer;

  function Max(i1,i2: integer): integer;
  begin
    if i1 > i2 then
      Result := i1
    else
      Result := i2
  end;

begin
  if Columns.Count = 0 then
    Exit;

  GridSize := ClientWidth - 1;
  if dgIndicator in Options then
    Dec(GridSize, IndicatorWidth);
  if dgColLines in Options then
    for i := 0 to Columns.Count - 1 do
      if TCRColumn(Columns[i]).Visible then
        Dec(GridSize, GridLineWidth);

  if ResizedColumn > -1 then begin
    ColumnsSize := 0;
    UnresizedSize := 0;
    MinimizeRest := False;
    for i := 0 to Columns.Count - 1 do begin
      if i <= ResizedColumn then begin
        Inc(UnresizedSize, ColWidths[i + IndicatorOffset]);
        if i = ResizedColumn then
          if ColumnsSize + ColWidths[i + IndicatorOffset] +
          (Columns.Count - i) * MinWidth > GridSize then begin
            ColWidths[i + IndicatorOffset] := GridSize - ColumnsSize -
              (Columns.Count - i - 1) * MinWidth;
            MinimizeRest := True;
          end
          else
            if i = Columns.Count - 1 then
              ColWidths[i + IndicatorOffset] := GridSize - ColumnsSize;
      end
      else
        if MinimizeRest {(ResizedColumn >= 0) and (ColumnsSize + (Columns.Count - i)*MinWidth >= GridSize)} then
          ColWidths[i + IndicatorOffset] := MinWidth;

      Inc(ColumnsSize, ColWidths[i + IndicatorOffset]);
    end;

    if ColumnsSize = UnresizedSize then
      Exit;

    K := (GridSize - UnresizedSize) / (ColumnsSize - UnresizedSize);

    ColumnsSize := 0;
    Prev := 0;
    for i := 0 to Columns.Count - 1 do begin
      if i <= ResizedColumn then
        Curr := Prev + ColWidths[i + IndicatorOffset]
      else
      begin
        Curr := Prev + ColWidths[i + IndicatorOffset]*K;

        if i < Columns.Count - 1 then
          Width := Round(Curr - Prev)
        else
          Width := GridSize - ColumnsSize;

        if Width < TCRColumn(Columns[i]).MinWidth then
          Width := TCRColumn(Columns[i]).MinWidth;
        ColWidths[i + IndicatorOffset] := Width;
      end;
      Inc(ColumnsSize, ColWidths[i + IndicatorOffset]);
      Prev := Curr;
    end;
    CalcTableSpacePercent;
  end
  else begin // for full resize
    Inc(GridSize,2);
    if (dgeStretch in FOptionsEx) then begin
      VisiblePercent := 0;
      for i := 0 to Columns.Count - 1 do
        if TCRColumn(Columns[i]).Visible then
          VisiblePercent := VisiblePercent + TCRColumn(Columns[i]).FTableSpacePercent;
      if VisiblePercent < 0.0001 then
        VisiblePercent := 1;
    end
    else
      VisiblePercent := 1;
    for i := 0 to Columns.Count - 1 do
      ColWidths[i + IndicatorOffset] := Trunc(TCRColumn(Columns[i]).FTableSpacePercent * GridSize / VisiblePercent);
  end;
end;

{ Grid drawing }

procedure TCRDBGrid.GetCellProps(Field: TField; AFont: TFont;
  var Background: TColor; State: TGridDrawState; StateEx: TGridDrawStateEx);
begin
  if Assigned(FOnGetCellParams) then
    FOnGetCellParams(Self, Field, AFont, Background, State, StateEx);
end;

procedure WriteText(ACanvas: TCanvas; ARect: TRect; DX, DY: integer;
  const Text: string; Alignment: TAlignment; ARightToLeft: boolean);
const
  AlignFlags : array [TAlignment] of integer =
    ( DT_LEFT or DT_WORDBREAK or DT_EXPANDTABS or DT_NOPREFIX,
      DT_RIGHT or DT_WORDBREAK or DT_EXPANDTABS or DT_NOPREFIX,
      DT_CENTER or DT_WORDBREAK or DT_EXPANDTABS or DT_NOPREFIX );
  RTL: array [boolean] of integer = (0, DT_RTLREADING);
var
  B, R: TRect;
  Hold, Left: integer;
  I: TColorRef;
begin
  I := ColorToRGB(ACanvas.Brush.Color);
  if GetNearestColor(ACanvas.Handle, I) = I then
  begin                       { Use ExtTextOut for solid colors }
    { In BiDi, because we changed the window origin, the text that does not
      change alignment, actually gets its alignment changed. }
    if (ACanvas.CanvasOrientation = coRightToLeft) and (not ARightToLeft) then
      ChangeBiDiModeAlignment(Alignment);
    case Alignment of
      taLeftJustify:
        Left := ARect.Left + DX;
      taRightJustify:
        Left := ARect.Right - ACanvas.TextWidth(Text) - 3;
    else { taCenter }
      Left := ARect.Left + (ARect.Right - ARect.Left) div 2
        - (ACanvas.TextWidth(Text) div 2);
    end;
    ACanvas.TextRect(ARect, Left, ARect.Top + DY, Text);
  end
  else begin                  { Use FillRect and Drawtext for dithered colors }
    DrawBitmap.Canvas.Lock;
    try
      with DrawBitmap, ARect do { Use offscreen bitmap to eliminate flicker and }
      begin                     { brush origin tics in painting / scrolling.    }
        Width := Max(Width, Right - Left);
        Height := Max(Height, Bottom - Top);
        R := Rect(DX, DY, Right - Left - 1, Bottom - Top - 1);
        B := Rect(0, 0, Right - Left, Bottom - Top);
      end;
      with DrawBitmap.Canvas do
      begin
        Font := ACanvas.Font;
        Font.Color := ACanvas.Font.Color;
        Brush := ACanvas.Brush;
        Brush.Style := bsSolid;
        FillRect(B);
        SetBkMode(Handle, TRANSPARENT);
        if (ACanvas.CanvasOrientation = coRightToLeft) then
          ChangeBiDiModeAlignment(Alignment);
        DrawText(Handle, PChar(Text), Length(Text), R,
          AlignFlags[Alignment] or RTL[ARightToLeft]);
      end;
      if (ACanvas.CanvasOrientation = coRightToLeft) then
      begin
        Hold := ARect.Left;
        ARect.Left := ARect.Right;
        ARect.Right := Hold;
      end;
      ACanvas.CopyRect(ARect, DrawBitmap.Canvas, B);
    finally
      DrawBitmap.Canvas.Unlock;
    end;
  end;
end;

function TCRDBGrid.GetButtonRect(Cell: TGridCoord): TRect;
var
  aCellRect: TRect;
begin
  aCellRect := CellRect(Cell.X, Cell.Y);
  if (aCellRect.Right - aCellRect.Left < aCellRect.Bottom - aCellRect.Top + 5)
  then begin
    Result := Rect(0,0,0,0);
    exit;
  end;
  Result.Left := aCellRect.Right - (aCellRect.Bottom - aCellRect.Top)+1;
  Result.Right := aCellRect.Right-1;
  Result.Top := aCellRect.Top+1;
  Result.Bottom := aCellRect.Bottom-1;
end;

function TCRDBGrid.IsOnButton(X, Y: integer): boolean;
var
  Cell: TGridCoord;
  Column: TColumn;
  aCellRect: TRect;
  ButtonRect: TRect;
begin
  Cell := MouseCoord(X,Y);
  Column := Columns[RawToDataColumn(Cell.X)];
  // detecting - is there a button on cell?
  if Assigned(Column.Field) then
    Result := Column.Field.DataType in [ftMemo,ftFmtMemo
      {$IFNDEF VER4}, ftOraClob {$ENDIF}{$IFDEF VER10P}, ftWideMemo{$ENDIF}]
  else
    Result := False;
  aCellRect := CellRect(Cell.X, Cell.Y);
  //Result := Result and (gdSelected in State);
  if Result and (aCellRect.Right - aCellRect.Left < aCellRect.Bottom - aCellRect.Top + 5) then
    Result := False;
  if Result then begin // button present
    ButtonRect := GetButtonRect(Cell);
    Result := PtInRect(ButtonRect,Point(X,Y))
  end
  else // there is no button on cell
    Result := False;
end;

procedure TCRDBGrid.DrawButton(X,Y: integer; State: boolean);
var
  ButtonRect: TRect;
  Cell: TGridCoord;
  Hi, i, Diam: integer;
  Flag: integer;
begin
  Cell.X := X; Cell.Y := Y;
  ButtonRect := GetButtonRect(Cell);
  Canvas.Brush.Style := bsSolid;
  Canvas.Brush.Color := clBtnFace;
  Canvas.FillRect(ButtonRect);
  Canvas.Pen.Color := clBlack;
  Canvas.Pen.Style := psSolid;
  Canvas.Brush.Color := clBlack;

  if State then
    Flag := BDR_SUNKENINNER
  else
    Flag := BDR_RAISEDINNER;

  DrawEdge(Canvas.Handle, ButtonRect, Flag, BF_TOPLEFT );
  InflateRect(ButtonRect, -1, -1);
  DrawEdge(Canvas.Handle, ButtonRect, Flag, BF_BOTTOMRIGHT);
  InflateRect(ButtonRect, 1, 1);
  Canvas.MoveTo(ButtonRect.Left, ButtonRect.Bottom - 1);
  Canvas.LineTo(ButtonRect.Right - 1, ButtonRect.Bottom - 1);
  Canvas.LineTo(ButtonRect.Right - 1, ButtonRect.Top - 1);

  Diam := (ButtonRect.Bottom - ButtonRect.Top) div 7;
  Hi := (ButtonRect.Bottom - ButtonRect.Top - Diam) div 2;
  inc(ButtonRect.Left,Diam * 2 - 1);
  if State then begin
    inc(ButtonRect.Left);
    inc(ButtonRect.Top);
  end;
  for i := 0 to 2 do
    Canvas.Ellipse(ButtonRect.Left + i * Diam * 2 ,ButtonRect.Top + Hi, ButtonRect.Left + i * Diam * 2 + Diam, ButtonRect.Top + Hi + Diam);
end;

procedure TCRDBGrid.DrawColumnCell(const Rect: TRect; DataCol: integer;
  Column: TColumn; State: TGridDrawState);
const
  ThreeDot = '...';
var
  NewBackgrnd: TColor;
  Field: TField;
  Value: string;
  TextWidth: integer;
  ThreeDotWidth: integer;
  Alignment: TAlignment;
  ColWidth: integer;
  StateEx: TGridDrawStateEx;
  TextMargin: integer;
  i: integer;
  isDrawButton: boolean;
  OldCanvasFont : TFont;

begin
  Field := Column.Field;
  if Assigned(Column.Field) then begin
    Value := Column.Field.DisplayText;
    isDrawButton := Column.Field.DataType in [ftMemo, ftFmtMemo
      {$IFNDEF VER4}, ftOraClob {$ENDIF}{$IFDEF VER10P}, ftWideMemo{$ENDIF}];
  end
  else begin
    Value := '';
    isDrawButton := False;
  end;

  isDrawButton := isDrawButton and (gdSelected in State)
    and not (dgRowSelect in Options);
  if isDrawButton and (Rect.Right - Rect.Left < Rect.Bottom - Rect.Top + 5) then
    isDrawButton := False;
  Alignment := Column.Alignment;

  if Alignment = taRightJustify then
    TextMargin:= 4
  else
    TextMargin := 2;

  ThreeDotWidth := Canvas.TextWidth(ThreeDot);
  TextWidth := Canvas.TextWidth(Value) + TextMargin;

  OldCanvasFont := TFont.Create;
  OldCanvasFont.Assign(Canvas.Font);
  try
    ColWidth := Column.Width;  // changes font and brush
    Canvas.Font.Assign(OldCanvasFont);
  finally
    OldCanvasFont.Free;
  end;

  if isDrawButton then begin
    ColWidth := ColWidth - (Rect.Bottom - Rect.Top);
  end;
  if TextWidth > ColWidth then begin
    if Field is TNumericField then begin
      for i := 1 to Length(Value) do
        if (Value[i] >= '0') and (Value[i] <= '9') then
          Value[i] := '#';
    end
    else begin
      while (TextWidth > ColWidth) and (Length(Value) > 1) do begin
        SetLength(Value, Length(Value) - 1);
        TextWidth := Canvas.TextWidth(Value) + TextMargin + ThreeDotWidth;
      end;
      Value := Value + ThreeDot;
    end;
    Alignment := taLeftJustify;
  end;

  if HighlightCell(Col, Row, Value, State) then begin
    Include(StateEx, geHighlight);
    if not FActiveRowSelected then
      Include(StateEx, geMultiSelected);
  end;
  if FActiveRowSelected then
    Include(StateEx, geActiveRow);

  if HighlightCell(Col, Row, Value, State) then begin
    Canvas.Brush.Color := clHighlight;
    Canvas.Font.Color := clHighlightText;
  end;

  if Enabled then begin
    NewBackgrnd := Canvas.Brush.Color;

    GetCellProps(Field, Canvas.Font, NewBackgrnd, State, StateEx);
    Canvas.Brush.Color := NewBackgrnd;
  end
  else
    Canvas.Font.Color := clGrayText;

  if FDefaultDrawing then
    WriteText(Canvas, Rect, 2, 2, Value, Alignment,
      UseRightToLeftAlignmentForField(Column.Field, Alignment));

  if FDefaultDrawing and (gdSelected in State)
    and ((dgAlwaysShowSelection in Options) or Focused)
    and not (csDesigning in ComponentState)
    and not (dgRowSelect in Options)
    and (UpdateLock = 0)
    and (ValidParentForm(Self).ActiveControl = Self)
  then
    Windows.DrawFocusRect(Canvas.Handle, Rect);

  inherited DrawColumnCell(Rect, DataCol, Column, State);
  if isDrawButton then
    if FCellButtonDown > -1 then
      DrawButton(Col, Row, FCellButtonPressed)
    else
      DrawButton(COl, Row, False);
end;

procedure TCRDBGrid.ClearSorting;
var
  i: integer;
begin
  for i := 0 to FSortInfo.Count - 1 do
    TSortColInfo(FSortInfo[i]).Free;
  FSortInfo.Clear;
end;

procedure TCRDBGrid.ClearFilters;
var
  i: integer;
begin
  for i := 0 to Columns.Count - 1 do
    TCRColumn(Columns[i]).FilterExpression := '';
end;

function TCRDBGrid.FindSortColInfo(Index: integer; var SortNum: integer): TSortColInfo;
var
  i: integer;
begin
  Result := nil;
  SortNum := 0;
  for i := 0 to FSortInfo.Count - 1 do
    if TSortColInfo(FSortInfo[i]).Index = Index then begin
      Result := TSortColInfo(FSortInfo[i]);
      if FSortInfo.Count > 1 then
        SortNum := i + 1;
      break;
    end;
end;

function  TCRDBGrid.GetTitleLevel(Level: integer): TRect;
begin
  if Columns.Count = 0 then begin
    Result := Rect(0, 0, 0, 0);
    Exit;
  end;
  Result.Top    := Level*(DefaultRowHeight + 1);
  Result.Bottom := Result.Top + (DefaultRowHeight + 1);
  Result.Left   := 0;
  Result.Right  := 0;
  if dgRowLines in Options then
    dec(Result.Bottom);
end;

procedure TCRDBGrid.CalcTitleLevel(Level: integer; var aRect: TRect);
var
  X: TRect;
begin
  if Columns.Count = 0 then begin
    aRect.Top   := 0;
    aRect.Bottom:= 0;
    Exit;
  end;
  X := GetTitleLevel(Level);
  aRect.Top    := X.Top;
  aRect.Bottom := X.Bottom;
end;

procedure TCRDBGrid.DrawCell(ACol,ARow: longint; ARect: TRect; AState: TGridDrawState);
var
  FrameOffs: Byte;

  procedure DrawTitleCell(ACol, ARow: integer; Column: TColumn; var AState: TGridDrawState);
  const
    ScrollArrows: array [boolean, boolean] of integer =
      ((DFCS_SCROLLRIGHT, DFCS_SCROLLLEFT), (DFCS_SCROLLLEFT, DFCS_SCROLLRIGHT));
  var
    MasterCol: TColumn;
    CellRect: TRect;
    TitleRect, TextRect: TRect;
    i: integer;
    ArrowX,
    ArrowY: integer;
    SortColInfo: TSortColInfo;
    OldBkMode: integer;
    OldTextColor: TColor;
    SortNum: integer;
    Caption: string;
    CaptionWidth: integer;
    CharWidth: integer;
    CurLevel: integer;
    CurCaption: string;
    lvCheckLeft,
    lvCheckRight,
    lvShowCaption,
    lvUpBorder,
    lvDownBorder,
    lvLeftBorder,
    lvRightBorder,
    lvCheckTextWidth : boolean;
    TmpCaption: string;
    lvTmpCol: TColumn;
    lvTmpColIndex: integer;
    lvCaptionXOffset: integer;
    lvCaptionAligment : TAlignment;
    CellFlag: cardinal;
    CaptionDepth: integer;
    PressOffset: integer;
  begin
    CellRect  := CalcTitleRect(Column, ARow, MasterCol);
    TitleRect := CellRect;

    if MasterCol = nil then begin
      Canvas.FillRect(ARect);
      Exit;
    end;
    // Prevent from drawing areas for SEARCH and FILTER Bars
    if dgeFilterBar in OptionsEx then
      dec(TitleRect.Bottom,DefaultRowHeight + 10);
    if dgeSearchBar in OptionsEx then
      dec(TitleRect.Bottom,DefaultRowHeight + 10);

    Canvas.Font := MasterCol.Title.Font;
    Canvas.Brush.Color := MasterCol.Title.Color;
    Canvas.FillRect(ARect);
    TextRect := TitleRect;

    Caption := MasterCol.Title.Caption;
    lvCheckLeft  := True;
    lvCheckRight := True;
    lvShowCaption:= True;
    lvLeftBorder := True;
    lvRightBorder:= True;
    if TCRColumnTitle(MasterCol.Title).IsCaptionStored then
      CaptionDepth := GetCaptionDepth(Caption,FLevelDelimiterChar)
    else
      CaptionDepth := 1;
    FrameOffs := 1;
    if (Column.Index = FTitleButtonDown) and (dgRowLines in Options) then
      PressOffset := 1
    else
      PressOffset := 0;
    for CurLevel := 0 to FHeaderHeight - 1 do begin
      // Check dependencies
      if TCRColumnTitle(MasterCol.Title).IsCaptionStored then
        CurCaption := GetCaptionLevel(Caption,CurLevel,FLevelDelimiterChar)
      else
      if CurLevel = 0 then
        CurCaption := Caption
      else
        CurCaption := '';
      lvDownBorder := (FHeaderHeight - 1 = CurLevel) or (GetCaptionLevel(Caption,CurLevel+1,FLevelDelimiterChar)<>'');
      lvUpBorder   := (CurCaption <> '');
      lvCaptionXOffset := 0;
      if CurCaption <> '' then begin
        if lvCheckLeft then begin
          lvLeftBorder := True;
          lvShowCaption:= True;
          if (Column.Index = 0) or (CurLevel = (CaptionDepth-1)) then
            lvCheckLeft := False
          else begin
            lvTmpColIndex := Column.Index-1;
            while lvTmpColIndex >= 0 do begin
              lvTmpCol := TColumn(MasterCol.Collection.Items[lvTmpColIndex]);
              tmpCaption := GetCaptionLevel(lvTmpCol.Title.Caption,CurLevel,FLevelDelimiterChar);
              if UpperCase(tmpCaption) <> UpperCase(CurCaption) then begin
                if lvTmpColIndex = Column.Index - 1 then
                    lvCheckLeft := False;
                break;
              end
              else begin
                lvShowCaption := False;
                lvLeftBorder := False;
                inc(lvCaptionXOffset, lvTmpCol.Width);
                if dgColLines in Options then
                  inc(lvCaptionXOffset);
                dec(lvTmpColIndex)
              end;
            end;
          end;
        end;
        if lvCheckRight then begin
          lvRightBorder := True;
          if (Column.Index = MasterCol.Collection.Count - 1) or (CurLevel = (CaptionDepth-1)) then
            lvCheckRight := False
          else begin
            lvTmpColIndex := Column.Index+1;
            lvTmpCol := TColumn(MasterCol.Collection.Items[lvTmpColIndex]);
            tmpCaption := GetCaptionLevel(lvTmpCol.Title.Caption,CurLevel,FLevelDelimiterChar);
            if UpperCase(tmpCaption) <> UpperCase(CurCaption) then
                lvCheckRight := False
            else
                lvRightBorder := False;
          end;
        end;
      end;

      //Check if we need to control caption width
      if Column.Index = MasterCol.Collection.Count - 1 then
        lvCheckTextWidth := True
      else begin
        lvTmpColIndex := Column.Index+1;
        lvTmpCol := TColumn(MasterCol.Collection.Items[lvTmpColIndex]);
        tmpCaption := GetCaptionLevel(lvTmpCol.Title.Caption,CurLevel,FLevelDelimiterChar);
        if UpperCase(tmpCaption) <> UpperCase(CurCaption) then
            lvCheckTextWidth := True
        else
            lvCheckTextWidth := False;
      end;

      // draw text for level
      TitleRect := CellRect;
      CalcTitleLevel(CurLevel,TitleRect);
      TextRect := TitleRect;
      InflateRect(TextRect,-1,-1);

      if not lvRightBorder then begin
        inc(TextRect.Right);
         if (dgColLines in Options) then
            inc(TextRect.Right);
      end;

      if CurLevel <> (CaptionDepth-1) then begin
        Canvas.Font := Self.TitleFont;
        Canvas.Brush.Color := Self.FixedColor;
        lvCaptionAligment := taLeftJustify;
       end
       else begin
        Canvas.Font := MasterCol.Title.Font;
        Canvas.Brush.Color := MasterCol.Title.Color;
        lvCaptionAligment := MasterCol.Title.Alignment;
       end;
       Canvas.FillRect(TextRect);

      if lvShowCaption then begin
        CaptionWidth := Canvas.TextWidth(CurCaption);
        if lvCheckTextWidth and (CaptionWidth > TextRect.Right - TextRect.Left) then begin
          while (CaptionWidth > TextRect.Right - TextRect.Left) and (Length(CurCaption) > 1) do begin
            SetLength(CurCaption, Length(CurCaption) - 1);
            CaptionWidth := Canvas.TextWidth(CurCaption) + Canvas.TextWidth('...');
          end;
          CurCaption := CurCaption + '...';
        end;
        WriteText(Canvas, TextRect, FrameOffs + PressOffset,
          FrameOffs + PressOffset, CurCaption, lvCaptionAligment, IsRightToLeft);
      end
      else
        if CurCaption = '' then
          WriteText(Canvas, TextRect, FrameOffs, FrameOffs, '', lvCaptionAligment,
            IsRightToLeft)
        else begin // mean there is coninue of previous column
          if dgColLines in Options then begin
            dec(TextRect.Left,1);
            dec(lvCaptionXOffset,1);
          end;
        CaptionWidth := Canvas.TextWidth(CurCaption) - lvCaptionXOffset;
        if lvCheckTextWidth and (CaptionWidth > TextRect.Right - TextRect.Left) then begin
          while (CaptionWidth > TextRect.Right - TextRect.Left) and (Length(CurCaption) > 1) do begin
            SetLength(CurCaption, Length(CurCaption) - 1);
            CaptionWidth := Canvas.TextWidth(CurCaption) + Canvas.TextWidth('...') - lvCaptionXOffset;
          end;
          CurCaption := CurCaption + '...';
        end;
        WriteText(Canvas, TextRect, FrameOffs - lvCaptionXOffset, FrameOffs, CurCaption, lvCaptionAligment,
            IsRightToLeft);
        end;
      // draw borders for level
      CellFlag := BDR_RAISEDINNER;
      if (FTitleButtonDown = Column.Index)and(CurLevel >= CaptionDepth-1) then
        CellFlag := BDR_SUNKENINNER;
      if not lvDownBorder then begin
        Inc(TitleRect.Bottom,1);
        Canvas.Pen.Color := clBtnFace;
        Canvas.MoveTo(TitleRect.Left,TitleRect.Bottom - 2);
        Canvas.LineTo(TitleRect.Right + 1, TitleRect.Bottom - 2);
        if dgRowLines in Options then begin
          Canvas.MoveTo(TitleRect.Left, TitleRect.Bottom - 1);
          Canvas.LineTo(TitleRect.Right + 1, TitleRect.Bottom - 1);
        end;
      end;
      if not lvUpBorder then begin
        Canvas.Pen.Color := clBtnFace;
        Canvas.MoveTo(TitleRect.Left, TitleRect.Top);
        Canvas.LineTo(TitleRect.Right + 1, TitleRect.Top);
      end;
      if lvRightBorder then begin
        if (dgRowLines in Options) and (dgColLines in Options) then
          DrawEdge(Canvas.Handle, TitleRect, CellFlag, BF_RIGHT);
      end
      else
        Inc(TitleRect.Right,1);
      if dgColLines in Options then begin
        Canvas.Pen.Color := clBlack;
        Canvas.MoveTo(TitleRect.Right, TitleRect.Top);
        Canvas.LineTo(TitleRect.Right, TitleRect.Bottom + 1);
      end;
      if lvDownBorder and ((dgRowLines in Options) and (dgColLines in Options)) then begin
//        if not(dgRowlines in Options) then
//          Inc(TitleRect.Bottom);
          DrawEdge(Canvas.Handle, TitleRect, CellFlag, BF_BOTTOM);
      end;
      if dgRowLines in Options then begin
        Canvas.Pen.Color := clBlack;
        Canvas.MoveTo(TitleRect.Left,TitleRect.Bottom);
        Canvas.LineTo(TitleRect.Right + 1,TitleRect.Bottom);
      end;
      if lvUpBorder and ((dgRowLines in Options) and (dgColLines in Options)) then
        DrawEdge(Canvas.Handle, TitleRect, CellFlag, BF_TOP);

      if lvLeftBorder and ((dgRowLines in Options) and (dgColLines in Options)) then
        DrawEdge(Canvas.Handle, TitleRect, CellFlag, BF_LEFT);
    end;

  // Draw sort indicators
    SortColInfo := FindSortColInfo(MasterCol.Index, SortNum);
    if (SortColInfo <> nil) then begin
      i := SaveDC(Canvas.Handle);
      try
        if SortNum = 0 then
          case Column.Title.Alignment of
            taRightJustify: ArrowX := TextRect.Left + 2;
          else
            ArrowX := TextRect.Right - 12;
          end
        else begin
          Canvas.Font := TitleFont;
          CharWidth := Canvas.TextWidth('0');
          ArrowX := TextRect.Right - 12 - CharWidth - 2;
        end;
        CaptionWidth := GetCaptionDepth(Caption, FLevelDelimiterChar);
        CalcTitleLevel(CaptionWidth - 1, TextRect);
        ArrowY := TextRect.Top + ((TextRect.Bottom - TextRect.Top - bmpSortAsc.Height) div 2);
        CurCaption := GetCaptionLevel(Caption, CaptionWidth - 1, FLevelDelimiterChar);
        CaptionWidth := Canvas.TextWidth(CurCaption);

        case Column.Title.Alignment of
          taLeftJustify: begin
                           if TextRect.Left + CaptionWidth + 20 < ArrowX then
                             ArrowX := TextRect.Left + CaptionWidth + 20;

                           if TextRect.Left + CaptionWidth + 4 > ArrowX then begin
                             ArrowX := TextRect.Left + CaptionWidth + 4;
                             IntersectClipRect(Canvas.Handle, TextRect.Left,
                               TextRect.Top, TextRect.Right - 1, TextRect.Bottom);
                           end;
                         end;
          taRightJustify: begin
                            if TextRect.Right - CaptionWidth - 20 > ArrowX + 10 then
                              ArrowX := TextRect.Right - CaptionWidth - 30;

                            if TextRect.Right - CaptionWidth - 4 < ArrowX + 10 then begin
                             ArrowX := TextRect.Right - CaptionWidth - 14;
                             IntersectClipRect(Canvas.Handle, TextRect.Left,
                               TextRect.Top, TextRect.Right - 1, TextRect.Bottom);
                            end;
                          end;
          taCenter: begin
                      if TextRect.Left + CaptionWidth +
                        (TextRect.Right - TextRect.Left - CaptionWidth) div 2 + 4 > ArrowX then begin
                          ArrowX := TextRect.Left + CaptionWidth + (TextRect.Right - TextRect.Left - CaptionWidth) div 2 + 4;
                          IntersectClipRect(Canvas.Handle, TextRect.Left,
                            TextRect.Top, TextRect.Right - 1, TextRect.Bottom);
                      end;
                    end;
        end;

        if SortColInfo.Desc then
          Canvas.Draw(ArrowX + PressOffset, ArrowY + PressOffset, bmpSortDesc)
        else
          Canvas.Draw(ArrowX + PressOffset, ArrowY + PressOffset, bmpSortAsc);

        if SortNum > 0 then begin
          OldBkMode := SetBkMode(Canvas.Handle, TRANSPARENT);
          OldTextColor := GetTextColor(Canvas.Handle);
          SetTextColor(Canvas.Handle, clWhite);
          ArrowY := TextRect.Top + ((TextRect.Bottom - TextRect.Top - canvas.textHeight('X')) div 2);
          Canvas.TextOut(ArrowX + 12 + PressOffset, ArrowY + PressOffset, IntToStr(SortNum));
          SetTextColor(Canvas.Handle, clGray);
          Canvas.TextOut(ArrowX + 11 + PressOffset, ArrowY - 1 + PressOffset, IntToStr(SortNum));
          SetBkMode(Canvas.Handle, OldBkMode);
          SetTextColor(Canvas.Handle, OldTextColor);
          Canvas.Font := MasterCol.Title.Font;
        end;
      finally
        RestoreDC(Canvas.Handle, i);
      end;
    end;

    if dgeFilterBar in OptionsEx then begin
      TitleRect.Top := TitleRect.Bottom;
      if dgRowLines in Options then
        Inc(TitleRect.Top);
//      if not(dgRowLines in Options) then
//        Dec(TitleRect.Top);
      TitleRect.Bottom :=  TitleRect.Top + DefaultRowHeight + 9;
      if CRGridTitleEdit.EditingFilter then
        DrawTitleBarCell(Canvas,Column,TitleRect,
          CRGridTitleEdit.FFilterExpressions[Column.Index])
      else
        DrawTitleBarCell(Canvas,Column,TitleRect,TCRColumn(Column).FilterExpression);
    end;
    if dgeSearchBar in OptionsEx then begin
      TitleRect.Top := TitleRect.Bottom ;
      if dgRowLines in Options then
        Inc(TitleRect.Top);
      TitleRect.Bottom :=  TitleRect.Top + DefaultRowHeight + 9;
//      if not(dgRowLines in Options) then
//        Dec(TitleRect.Top);
      DrawTitleBarCell(Canvas,Column,TitleRect,'');
    end;
    AState := AState - [gdFixed];  // prevent box drawing later
  end;

var
  DrawColumn: TColumn;
begin
  if (ARow = 0) and (dgTitles in Options) then begin
    if ACol >= IndicatorOffset then begin
      DrawColumn := Columns[ACol - IndicatorOffset];
      DrawTitleCell(ACol - IndicatorOffset, ARow, DrawColumn, AState);
    end
    else begin
      inherited DrawCell(ACol, ARow, ARect, AState);
      DrawTitleIndicatorCell(Canvas,ARect);
    end
  end
  else begin
    if DataLink.Active then
      if dgTitles in Options then
        FActiveRowSelected := ARow - 1 = DataLink.ActiveRecord
      else
        FActiveRowSelected := ARow = DataLink.ActiveRecord
    else
      FActiveRowSelected := False;
    inherited DrawCell(ACol, ARow, ARect, AState);
    if gdFixed in AState then begin
      if dgColLines in Options then begin
        Canvas.Pen.color := clBlack;
        Canvas.Pen.style := psSolid;
        Canvas.MoveTo(aRect.Right, aRect.Top);
        Canvas.LineTo(aRect.Right, aRect.Bottom + 1);
      end;
      if dgRowLines in Options then begin
        Canvas.Pen.color := clBlack;
        Canvas.Pen.style := psSolid;
        Canvas.MoveTo(aRect.Left, aRect.Bottom);
        Canvas.LineTo(aRect.Right, aRect.Bottom);
      end;
    end
  end;
end;

procedure TCRDBGrid.MouseDown(Button: TMouseButton; Shift: TShiftState;
  X, Y: integer);
var
  State: TGridState;
  DrawInfo: TGridDrawInfo;
  Index: longint;
  Pos, Ofs: integer;
  OldActive: integer;
  Cell: TGridCoord;
  i: integer;
  Column: TColumn;
  Value: string;
  ColWidth, ValueWidth: integer;
begin
  FExecColAjust := False;

  if FGridState = gsNormal then begin
    CalcDrawInfo(DrawInfo);
    CalcSizingState(X, Y, State, Index, Pos, Ofs, DrawInfo);
  end
  else
    State := FGridState;

  if not (State in [gsColSizing]) and DataLink.Active then begin
    if (Button = mbLeft) and (dgTitles in Options) then
    begin
      Cell := MouseCoord(X,Y);
      if Cell.X >= IndicatorOffset then
      begin
        if not (dgRowSelect in Options) and (Cell.Y >= FixedRows)
          and (TopRow  + Cell.Y - FixedRows = Row) and IsOnButton(X,Y)
        then begin
          FCellButtonDown := RawToDataColumn(Cell.X);
          FCellButtonRow := Cell.Y;
          FCellButtonCol := Cell.X;
          FCellButtonBRect := GetButtonRect(Cell);
          FCellButtonRect := CellRect(Cell.X,Cell.Y);
          //Paint;  // ??
          HideEditor;
          DrawButton(Cell.X,Cell.Y,PtInRect(FCellButtonBRect,Point(x,y)));
          FCellButtonPressed := True;
          //invalidaterect(GetButtonRect(Cell));
          Exit;
        end;

        if Cell.Y = 0  then
        begin
          Column := Columns[RawToDataColumn(Cell.X)];

          if MouseInFilterEdit(X, Y, Column) then
          begin
            FContinueEditingFilter := True;
            ActivateFilterEdit(Column);
            Exit;
          end
          else
            if MouseInSortEdit(X, Y, Column) then
            begin
              ActivateSearchEdit(Column);
              Exit;
            end;
        end;

        if DataLink.Active and (Cell.Y < FixedRows)
          and (dgeEnableSort in OptionsEx) and MouseInLowerstLevel(X, Y, nil)
        then begin
          i := FTitleButtonDown;
          FTitleButtonDown := RawToDataColumn(Cell.X);
          FOldTitleButtonDown := FTitleButtonDown;
          if i > -1 then
            InvalidateCol(i+1);
          invalidatecol(FTitleButtonDown+1);

        end;
      end
      else
      begin
        FIndicatorColBtnDown := GetIndicatorButton(X,Y);
        FOldIndicatorColBtnDown := FIndicatorColBtnDown;
        if FIndicatorColBtnDown <> icbNone then
          InvalidateCol(0);
      end;
    end;
  end;

  if (mbLeft = Button) and (State = gsColSizing) and DataLink.Active then begin
    if ssDouble in Shift then begin
      Index := Min(RawToDataColumn(MouseCoord(X, Y).X), RawToDataColumn(MouseCoord(X - 7, Y).X));
      if Index < 0 then
        Index := Columns.Count - 1;

      Column := Columns[Index];
      ColWidth := 0;
      OldActive := DataLink.ActiveRecord;
      try
        for i := TopRow - 1 to VisibleRowCount - 1 do begin
          Datalink.ActiveRecord := i;
          if Assigned(Column.Field) then
            Value := Column.Field.DisplayText
          else
            Value := '';
          ValueWidth := Canvas.TextWidth(Value);
          if ValueWidth > ColWidth then
            ColWidth := ValueWidth;
        end;
      finally
        DataLink.ActiveRecord := OldActive;
      end;

      //Column.Width := ColWidth + 4;
      ColWidths[Index + IndicatorOffset] := ColWidth + 4;

      FExecColAjust := True;
      //MessageBox(0, PChar('Row ' + IntToStr(Row) + #13'Row Count ' + IntToStr(RowCount) + #13'TopRow ' + IntToStr(TopRow) + #13'Vis ' + IntToStr(VisibleRowCount)), '', MB_OK);
    end;
    if CRGridTitleEdit.Focused or CRGridTitleEdit.Edit.Focused then begin
      SendMessage(Handle, WM_SETREDRAW, 0, 0);
      try
        inherited;
          CRGridTitleEdit.Visible := True;
          CRGridTitleEdit.SetFocus;
      finally
        SendMessage(Handle, WM_SETREDRAW, 1, 0);
      end;
      Column := CRGridTitleEdit.FActiveColumn;
      if CRGridTitleEdit.FAsFilter then begin
        ActivateFilterEdit(Column);
        Exit;
      end
      else
        ActivateSearchEdit(Column);
    end;
  end;

  InvalidateRect(Handle,{$IFNDEF CLR}@{$ENDIF}FStatusRect,False);
  CRGridTitleEdit.EditingFilter := False;
  FContinueEditingFilter := False;
  inherited;
end;

procedure TCRDBGrid.MouseMove(Shift: TShiftState; X, Y: integer);
var
  State: TGridState;
  DrawInfo: TGridDrawInfo;
  Index: Longint;
  Pos, Ofs: integer;
//  Column:TColumn;
//  Cell: TGridCoord;
  Rect: TRect;
  Col: TColumn;
begin
  inherited;

  if FGridState = gsNormal then begin
    CalcDrawInfo(DrawInfo);
    CalcSizingState(X, Y, State, Index, Pos, Ofs, DrawInfo);
  end
  else
    State := FGridState;
  if FCellButtonDown > -1 then
  begin
    FCellButtonPressed := PtInRect(FCellButtonBRect,Point(x,y));
    DrawButton(FCellButtonCol,FCellButtonRow,FCellButtonPressed);
  end;

  if (ssLeft in Shift) and (FOldTitleButtonDown > -1) then begin
    Rect := CalcTitleRect(Columns[FOldTitleButtonDown], 0, Col);

    if dgeSearchBar in OptionsEx then
      Dec(Rect.Bottom,DefaultRowHeight + 10);
    if dgeFilterBar in OptionsEx then
      Dec(Rect.Bottom,DefaultRowHeight + 10);

    if (FTitleButtonDown = -1) and PtInRect(Rect,Point(X,Y)) then begin
      FTitleButtonDown := FOldTitleButtonDown;
      InvalidateCol(FTitleButtonDown + 1);
    end
    else
      if (FTitleButtonDown > -1) and ((Y < Rect.Top) or (Y > Rect.Bottom)
      or ((X < Self.Left) and (Columns[FTitleButtonDown].Index = 0))
      or ((X > Self.Left + Self.Width) and (Columns[FTitleButtonDown].Index = Columns.Count - 1))) then begin
        Index := FTitleButtonDown + 1;
        FTitleButtonDown := -1;
        InvalidateCol(Index)
      end;
  end;

  if (ssLeft in Shift) and (FOldIndicatorColBtnDown <> icbNone) then begin
    if (FIndicatorColBtnDown = icbNone)
    and (GetIndicatorButton(X, Y) = FOldIndicatorColBtnDown) then begin
      FIndicatorColBtnDown := FOldIndicatorColBtnDown;
      InvalidateCol(0);
    end
    else
      if (FIndicatorColBtnDown <> icbNone)
      and (FIndicatorColBtnDown <> GetIndicatorButton(X, Y)) then begin
        FIndicatorColBtnDown := icbNone;
        InvalidateCol(0)
      end;
  end;

{  if not (State in [gsColSizing]) and DataLink.Active then begin
    Cell := MouseCoord(X,Y);
    if (Cell.X >= IndicatorOffset) and (Cell.Y >= 0) and
      (ngTitles in FOptions) and (Cell.Y = 0)
    then begin
      FTitleButtonDown := RawToDataColumn(Cell.X);
      Paint;  // ??
    end
    else begin
      FTitleButtonDown := -1;
      Paint;  // ??
    end;
  end;}
end;

procedure TCRDBGrid.Reorder;
var
  i: integer;
  St: string;
begin
  if DataLink.Active and
  {$IFDEF DAC450}
    (DataLink.DataSet is TCustomDADataSet)
  {$ELSE}
    ( ((DataLink.DataSet is TCustomDADataSet) and not(dgeLocalSorting in OptionsEx)) or
      ((DataLink.DataSet is TMemDataSet) and (dgeLocalSorting in OptionsEx))
    )
  {$ENDIF}
  then begin
    St := '';
    for i := 0 to FSortInfo.Count - 1 do
      if TCRColumn(Columns[TSortColInfo(FSortInfo[i]).Index]).CanBeSorted then
      begin
        if St <> '' then
          St := St + ',';
      {$IFNDEF DAC450}
        if dgeLocalSorting in OptionsEx then
          St := St + '''' + Columns[TSortColInfo(FSortInfo[i]).Index].Field.FieldName + ''''
        else
      {$ENDIF}
          St := St + IntToStr(Columns[TSortColInfo(FSortInfo[i]).Index].Field.FieldNo);
        if TSortColInfo(FSortInfo[i]).Desc then
          St := St + ' DESC';
      end;
  {$IFNDEF DAC450}
    if dgeLocalSorting in OptionsEx then
      TMemDataSet(DataLink.DataSet).IndexFieldNames := St
    else 
  {$ENDIF}
    begin
      TCustomDADataSet(DataLink.DataSet).SetOrderBy(St);
      DataLink.DataSet.Open;
    end;
  end;
end;

procedure TCRDBGrid.MouseUp(Button: TMouseButton; Shift: TShiftState;
  X, Y: integer);
var
  State: TGridState;
  DrawInfo: TGridDrawInfo;
  Index, i: Longint;
  Pos, Ofs: integer;
  Column: TColumn;
  Cell: TGridCoord;
  SortColInfo: TSortColInfo;
  Desc: boolean;
  SortColNum: integer;
  LastBtn: integer;
  Widths: array of integer;
begin
  if FGridState = gsNormal then begin
    CalcDrawInfo(DrawInfo);
    CalcSizingState(X, Y, State, Index, Pos, Ofs, DrawInfo);
  end
  else
    State := FGridState;

  if (mbLeft = Button) and (State = gsColSizing) and DataLink.Active then begin
    if CRGridTitleEdit.Focused then begin
      inherited;
      Column := CRGridTitleEdit.FActiveColumn;
      if CRGridTitleEdit.FAsFilter then begin
        ActivateFilterEdit(Column);
      end
      else
        ActivateSearchEdit(Column);
    end;
  end;

  FTitleBarUp := False;
  if not (State in [gsColSizing]) and DataLink.Active and not FExecColAjust
  then begin
    Cell := MouseCoord(X,Y);

    if not (dgRowSelect in Options) then
      if FCellButtonDown > -1 then begin
        DrawButton(Cell.X,Cell.Y,False);
        if FCellButtonDown = RawToDataColumn(Cell.X) then
          if FCellButtonPressed then
          begin
            FCellButtonDown := -1;
            FCellButtonRow := -1;
            FCellButtonCol := -1;
            DoOnMemoClick(Columns[RawToDataColumn(Cell.X)]);
            invalidate;
          end;
      end;
    FCellButtonDown := -1;
    FCellButtonRow := -1;
    FCellButtonCol := -1;
    LastBtn := FTitleButtonDown;
    FOldTitleButtonDown := -1;
    if FTitleButtonDown > -1 then begin
      invalidatecol(FTitleButtonDown + 1);
      FTitleButtonDown := - 1;
    end;

    if (Button = mbLeft) and (Cell.Y = 0) and (dgTitles in Options) then begin
      if Cell.X >= IndicatorOffset then
      begin
        Column := Columns[RawToDataColumn(Cell.X)];

        if not (MouseInSortBar(X,Y,Column) or MouseInFilterBar(X,Y,Column))
        then begin
          FTitleBarUp := True;
          
          if TCRColumn(Column).CanBeSorted and (dgeEnableSort in OptionsEx)
            and MouseInLowerstLevel(X,Y,Column) and (LastBtn = Column.Index)
          then begin
            FExecSorting := True;
            BeginLayout;
            try
              SetLength(Widths, Columns.Count);
              for i := 0 to Columns.Count - 1 do
                Widths[i] := Columns[i].Width;
              if (DataLink.DataSet <> nil) and
               {$IFDEF DAC450}
                 (DataLink.DataSet is TCustomDADataSet)
               {$ELSE}
                 ( ((DataLink.DataSet is TCustomDADataSet) and not(dgeLocalSorting in OptionsEx)) or
                   ((DataLink.DataSet is TMemDataSet) and (dgeLocalSorting in OptionsEx))
                 )
               {$ENDIF}
              then begin
                SortColInfo := FindSortColInfo(Column.Index, SortColNum);
                Desc := (SortColInfo <> nil) and not SortColInfo.Desc;

                if (ssCtrl in Shift) and (SortColInfo <> nil) then begin
                  SortColInfo.Free;
                  if SortColNum > 0 then
                    Dec(SortColNum);
                  FSortInfo.Delete(SortColNum);
                end
                else begin
                  if not (ssShift in Shift) then
                    ClearSorting;
                  if not (ssShift in Shift) or (SortColInfo = nil) then begin
                    SortColInfo := TSortColInfo.Create;
                    SortColInfo.Index := Column.Index;
                    FSortInfo.Add(SortColInfo);
                  end;

                  SortColInfo.Desc := Desc;
                end;

                Reorder;
              end;
            finally
              EndLayout;
              for i := 0 to Columns.Count - 1 do
                Columns[i].Width := Widths[i];
              FExecSorting := False;
            end;
          end;
        end;
      end
      else
        if FIndicatorColBtnDown <> icbNone then begin
          FIndicatorColBtnDown := icbNone;
          InvalidateCol(0);
          IndicatorClick(FOldIndicatorColBtnDown, X, Y);
        end;
    end;
    FOldIndicatorColBtnDown := icbNone;
  end;

  inherited;

  FTitleBarUp := False;
end;

procedure TCRDBGrid.LinkActive(Value: boolean);
var
  St: _string;
  Parser: TParser;
  Code: integer;
  Lex: _string;
  i: integer;
  FieldName: _string;
  SortColInfo: TSortColInfo;
  Ind: integer;
begin
  inherited;
  // need to make header to have multilines
  CRGridTitleEdit.StopEdit(False);
  if not FExecSorting then begin
    ClearSorting;

    if Value and (DataLink.DataSet is TCustomDADataSet)
      and TCustomDADataSet(DataLink.DataSet).IsQuery
    then begin
      St := TCustomDADataSet(DataLink.DataSet).GetOrderBy;
      //St := GetOrderBy(TOraDataSet(DataLink.DataSet).SQL.Text);
      //St := '';
      if St <> '' then begin
        Parser := TParser.Create(St);
        Parser.DecSeparator :=  '.';
        try
          Parser.QuotedString := True;
          repeat
            SortColInfo := nil;
            Code := Parser.GetNext(Lex);
             case Code of
              lcString:
                if Lex[1] = '"' then begin
                  Lex := Copy(Lex, 2, Length(Lex) - 2);
                  if Lex <> '' then
                    Code := lcIdent;
                end;
              lcNumber:
                try
                  Ind := StrToInt(Lex);
                  if Ind <= DataLink.DataSet.FieldDefs.Count then begin
                    Lex := DataLink.DataSet.FieldDefs[Ind - 1].Name;
                    Code := lcIdent;
                  end;
                except
                  Exit;
                end;
            end;

            if Code <> lcIdent then
              Exit;

            FieldName := _UpperCase(Lex);

            Code := Parser.GetNext(Lex);
            if Lex = '.' then begin
              Code := Parser.GetNext(Lex);
              if Code = lcIdent then begin
                FieldName := _UpperCase(Lex);
                Code := Parser.GetNext(Lex);
              end
              else
                Exit;
            end;

            for i := 0 to Columns.Count - 1 do
              if (Columns[i].Field <> nil) and (_UpperCase(Columns[i].Field.FieldName) = FieldName)
              then begin
                SortColInfo := TSortColInfo.Create;
                SortColInfo.Index := i;
                SortColInfo.Desc := False;
                FSortInfo.Add(SortColInfo);
                break;
              end;

            if _UpperCase(Lex) = 'DESC' then begin
              if SortColInfo <> nil then
                SortColInfo.Desc := True;
              Code := Parser.GetNext(Lex);
            end
            else
              if _UpperCase(Lex) = 'ASC' then
                Code := Parser.GetNext(Lex)
              else
                if (Code <> lcEnd) and (Lex <> ',') then
                  Exit;
          until Code = lcEnd;
        finally
          Parser.Free;
        end;
      end;
    end;
  end;
  if Value then
    RecordChanged(nil);

  Invalidate;
end;

procedure TCRDBGrid.UpdateHeaderHeight;
var
  Cur, i: integer;
  aHeight: integer;
begin
  if not (dgTitles in Options) then begin
    RowHeights[0]:= DefaultRowHeight;
    Exit;
  end;
  FHeaderHeight := 1;
  for i := 0 to Columns.Count - 1 do begin
    if TCRColumnTitle(Columns[i].Title).IsCaptionStored then
      Cur := GetCaptionDepth(Columns[i].Title.Caption,FLevelDelimiterChar)
    else
      Cur := 1;
    if Cur > FHeaderHeight then
      FHeaderHeight := Cur;
  end;
  aHeight := (DefaultRowHeight + 1) * FHeaderHeight;
//  if dgRowLines in Options then
//    aHeight := aHeight + FHeaderHeight;
//  if not (dgRowLines in Options) then
//    Dec(aHeight); // because of always-border cells
  if dgeFilterBar in OptionsEx  then
    aHeight := aHeight + DefaultRowHeight + 10;

  if dgeSearchBar in OptionsEx  then
    aHeight := aHeight + DefaultRowHeight + 10;

  RowHeights[0]:= aHeight - 1;
end;

procedure TCRDBGrid.SetOptionsEx(Value: TCRDBGridOptionsEx);
begin
  if FOptionsEx <> Value then begin
    if ((dgeFilterbar in FOptionsEx) <> (dgeFilterBar in Value))
    or ((dgeSearchbar in FOptionsEx) <> (dgeSearchBar in Value))
    then begin
      FOptionsEx := Value;
      FOptionsMenuDef.Items[2].Checked := dgeFilterBar in FOptionsEx;
      FOptionsMenuDef.Items[3].Checked := dgeSearchBar in FOptionsEx;
      LayoutChanged;
      UpdateRowCount;
      Invalidate;
      Exit;
    end;
    FOptionsEx := Value;
    if (dgeStretch in FOptionsEx) then begin
      if not FSoft and not (csLoading in ComponentState) then begin
        FExecSizing := True;
        ResizeColumns;
        FExecSizing := False;
      end;
      FSoft := True;
    end
    else
      FSoft := False;
    Invalidate;
  end;
end;

function TCRDBGrid.GetClientRect: TRect;
begin
  Result := inherited GetClientRect;
  if dgRowLines in options then
      Inc(Result.Bottom);
  if [dgeSummary,dgeRecordCount] * FOptionsEx <> [] then
      Dec(Result.Bottom,DefaultRowHeight + 2);
end;

procedure TCRDBGrid.LayoutChanged;
begin
  inherited;

  CRGridTitleEdit.StopEdit(False);
  UpdateHeaderHeight;
  //DoubleBuffered := true;
end;

procedure TCRDBGrid.Paint;
var
  TotalWidth: integer;
  TotalYOffs: integer;
  TotalHeight: integer;
  TmpText: string;

  procedure PaintStatusLine(YOffset: integer);
  var
    Column: TCRColumn;
    MasterCol: TColumn;
    BrdRect: TRect;
    CellRect: TRect;
    ColIndex: integer;
    FullRect: TRect;
    RightBorder: integer;
    OldDC: HDC;
  begin
    OldDC := Canvas.Handle;
    Canvas.Handle := 0;
    try
      FullRect := Rect(0, FTotalYOffset, TotalWidth, FTotalYOffset + DefaultRowHeight + 2);
      FStatusRect := FullRect;
  //    if dgRowLines in Options then
  //      Inc(FullRect.Top);
      Canvas.FillRect(FullRect);
      with Canvas do begin
        Pen.Color := clBlack;
        Pen.Style := psSolid;
        if dgRowLines in Options then begin
          MoveTo(FullRect.Left, FullRect.Top - 1);
          LineTo(FullRect.Right, FullRect.Top - 1);
          MoveTo(FullRect.Left, FullRect.Bottom - 2);
          LineTo(FullRect.Right, FullRect.Bottom - 2);
        end;
       if dgColLines in Options then begin
          Dec(FullRect.Right);
          MoveTo(FullRect.Right, FullRect.Top);
          LineTo(FullRect.Right, FullRect.Bottom + 2);
  //        MoveTo(FullRect.Left , FullRect.Bottom + 2);
  //        LineTo(FullRect.Left, FullRect.Top);
        end;
        if (dgRowLines in Options) and (dgColLines in Options) then begin
          Dec(FullRect.Bottom, 2);
          DrawEdge(Canvas.Handle, FullRect, BDR_RAISEDINNER, BF_BOTTOMRIGHT);
          DrawEdge(Canvas.Handle, FullRect, BDR_RAISEDINNER, BF_TOPLEFT);
        end;
      end;

      if (Columns.Count > 0) and (dgeSummary in FOptionsEx) then begin
        RightBorder := GetClientRect.Right - 1;
        for ColIndex := RawToDataColumn(LeftCol) to Columns.Count - 1 do begin
          Column := TCRColumn(Columns[ColIndex]);
          CellRect := CalcTitleRect(Column, 0, MasterCol);
          if (CellRect.Left <> 0) or (CellRect.Right <> 0) or
             (CellRect.Top <> 0) or (CellRect.Bottom <> 0)
          then begin
            CellRect.Top := YOffset;
            CellRect.Bottom := CellRect.Top + DefaultRowHeight;
            if Column.SummaryMode <> smNone then  begin
              tmpText := TCRColumn(Column).TotalString;
              InflateRect(CellRect, 0, -2);
              if tmpText <> '' then
                WriteText(Canvas, CellRect, 3, 0, tmpText, Column.Alignment,True);
              InflateRect(CellRect, 0, 2);
              if dgColLines in Options then begin
                if dgRowLines in Options then begin
                  BrdRect := CellRect;
                  Inc(BrdRect.Bottom);
                  DrawEdge(Canvas.Handle, BrdRect, BDR_RAISEDINNER, BF_RIGHT);
                  DrawEdge(Canvas.Handle, BrdRect, BDR_RAISEDINNER, BF_LEFT);
                  BrdRect.Right := CellRect.Left - 1;
                  BrdRect.Left  := BrdRect.Right - 1;
                  DrawEdge(Canvas.Handle, BrdRect, BDR_RAISEDINNER, BF_RIGHT);
                  Dec(BrdRect.Bottom);
                end;

                Canvas.Pen.Color := clBlack;
                Canvas.MoveTo(CellRect.Left - 1, CellRect.Top - 1);
                Canvas.LineTo(CellRect.Left - 1, CellRect.Bottom + 3);

                BrdRect.Left := CellRect.Right + 1;
                BrdRect.Right:= BrdRect.Left + 1;

                if Column.Index < MasterCol.Collection.Count - 1 then begin
                  Inc(BrdRect.Bottom);
                  DrawEdge(Canvas.Handle, BrdRect, BDR_RAISEDINNER, BF_LEFT);
                  Dec(BrdRect.Bottom);
                  Canvas.Pen.Color := clBlack;
                  Canvas.MoveTo(CellRect.Right, CellRect.Top - 1);
                  Canvas.LineTo(CellRect.Right, CellRect.Bottom + 3);
                end;
              end;
            end;
          end;
          if dgeRecordCount in FOptionsEx
          then begin
            CellRect := FullRect;
            InflateRect(CellRect, -2, -2);
            CellRect.Right := RightBorder;
            if DataLink.Active then
                tmpText := IntToStr(DataLink.DataSet.RecNo) +
                  '/' + IntToStr(DataLink.DataSet.RecordCount)
            else
                tmpText := '';//'Records count : INACTIVE';
            WriteText(Canvas, CellRect, 0, 0, tmpText, taLeftJustify,True);
          end;
        end;
      end
      else begin
        if dgeRecordCount in FOptionsEx then begin
          CellRect := FullRect;
          InflateRect(CellRect, -2, -2);
          if DataLink.Active then
            tmpText := IntToStr(DataLink.DataSet.RecNo) +
              '/' + IntToStr(DataLink.DataSet.RecordCount)
          else
            tmpText := '';//'Records count : INACTIVE';
          WriteText(Canvas, CellRect, 0, 0, tmpText, taLeftJustify,True);
        end;
      end;
    finally
      Canvas.Handle := OldDC;
    end;
  end;
{$IFNDEF CLR}//TODO
var
  Opt: TGridOptions;
//  Last: TGridOptions;
{$ENDIF}
begin
{$IFNDEF CLR}//TODO
  Opt := _TCustomGrid(Self).Options;
//  Last := Opt;
  Opt := Opt - [goFixedVertLine , goFixedHorzLine];
  _TCustomGrid(Self).Options := Opt;
{$ENDIF}
  inherited; // Draw standart grid
  with Canvas do begin
    TotalHeight := GetClientRect.Bottom;
    TotalYOffs := TotalHeight;
    TotalWidth := GetClientRect.Right;
    FTotalYOffset := TotalYOffs;
    Canvas.Font.Assign(TitleFont);
    Canvas.Brush.Color := clBtnFace;
    if [dgeSummary,dgeRecordCount] * FOptionsEx <> [] then
      PaintStatusLine(FTotalYOffset);
  end;
  //_TCustomGrid(Self).Options := Last;
end;

procedure TCRDBGrid.LoadTotals;
var
  ColNom: integer;
  Mode: TSummaryMode;
  BookM: TBookmark;
  Col: TCRColumn;
  DataSet: TDataSet;
  Field: TField;
begin
  if not ((dgeSummary in OptionsEx) and DataLink.Active) or (Columns.Count = 0) then
    Exit;
  BookM := DataLink.DataSet.GetBookMark;
  try
    DataSet := DataLink.DataSet;
    DataSet.DisableControls;
    for ColNom := 0 to Columns.Count - 1 do begin
      if (TCRColumn(Columns[ColNom]).SummaryMode <> smLabel) and (not TCRColumn(Columns[ColNom]).FTotalLoaded)and(Assigned(Columns[ColNom].Field)) then  begin
        Col := TCRColumn(Columns[ColNom]);
        Field := Col.Field;
        if Col.SummaryMode = smMin then
          Col.FTotalInt := Field.AsInteger
        else
          Col.FTotalInt := 0;
        if Col.SummaryMode = smMin then
          Col.FTotalFloat := Field.AsFloat
        else
          Col.FTotalFloat := 0;
        Col.FTotalString:= '';
        Col.FTotalValue:= Unassigned;
      end;
    end;

    DataSet.First;
    while not DataSet.Eof do begin
      for ColNom := 0 to Columns.Count - 1 do
        if (not TCRColumn(Columns[ColNom]).FTotalLoaded) and
          Assigned(TCRColumn(Columns[ColNom]).Field)
        then begin
          Col := TCRColumn(Columns.Items[ColNom]);
          Field := Col.Field;
          Mode := Col.SummaryMode;
          case Col.Field.DataType of
            ftSmallint, ftInteger, ftWord, ftLargeint:
              case Mode of
                smSum:
                  Col.FTotalInt := Col.FTotalInt + Field.AsInteger;
                smAvr:
                  Col.FTotalInt := Col.FTotalInt + Field.AsInteger;
                smMax:
                  if Col.FTotalInt < Field.AsInteger then
                    Col.FTotalInt := Field.AsInteger;
                smMin:
                  if Col.FTotalInt > Field.AsInteger then
                    Col.FTotalInt := Field.AsInteger;
              end;
            ftFloat, ftCurrency:
              case Mode of
                smSum:
                  Col.FTotalFloat := Col.FTotalFloat + Field.AsFloat;
                smAvr:
                  Col.FTotalFloat := Col.FTotalFloat + Field.AsFloat;
                smMax:
                  if Col.FTotalFloat < Field.AsFloat then
                    Col.FTotalFloat := Field.AsFloat;
                smMin:
                  if Col.FTotalFloat > Field.AsFloat then
                    Col.FTotalFloat := Field.AsFloat;
              end;
          end;
        end;
        DataSet.Next;
    end;
    for ColNom := 0 to Columns.Count - 1 do
      if (not TCRColumn(Columns[ColNom]).FTotalLoaded) and
        Assigned(TCRColumn(Columns[ColNom]).Field)
      then begin
        Col := TCRColumn(Columns.Items[ColNom]);
        if (Col.SummaryMode = smAvr) and (DataLink.DataSet.RecordCount > 0) then
          case Col.Field.DataType of
            ftSmallint, ftInteger, ftWord, ftLargeint:
              Col.FTotalFloat := Col.FTotalInt / DataLink.DataSet.RecordCount;
            ftFloat, ftCurrency:
              Col.FTotalFloat := Col.FTotalFloat / DataLink.DataSet.RecordCount;
          end;
        Col.SetTotal;
      end;
  finally
    DataLink.DataSet.GotoBookMark(BookM);
    DataLink.DataSet.FreeBookMark(BookM);
    DataLink.DataSet.EnableControls;
  end;
end;

procedure TCRDBGrid.DataChanged;
begin
  ResetTotals;
  LoadTotals;
  invalidate;
end;

procedure TCRDBGrid.RecordChanged(Field: TField);
var
  i: integer;
begin
  if not HandleAllocated then
    Exit;
  if Field = nil then
    DataChanged
  else begin
    for i := 0 to Columns.Count - 1 do
      if Columns[i].Field = Field then
        TCRColumn(Columns[i]).ResetTotal;
    LoadTotals;
    Invalidate;
  end;

  inherited;
end;

procedure TCRDBGrid.ResetTotals;
var
  i: integer;
begin
  for i := 0 to Columns.Count - 1 do
    TCRColumn(Columns[i]).ResetTotal;
end;

function TCRDBGrid.EndColumnDrag(var Origin, Destination: integer;
  const MousePt: TPoint): boolean;
var
  Mx,Mn,
  Oi: integer;
begin
  Result := inherited EndColumnDrag(Origin, Destination, MousePt);

  if Result and (Origin <> Destination) then begin
    if Origin > Destination then begin
      Mx := Origin;
      Mn := Destination
    end
    else begin
      Mx := Destination;
      Mn := Origin
    end;
    Dec(mx);
    Dec(mn);
    for Oi := 0 to FSortInfo.Count - 1 do begin
      if TSortColInfo(FSortInfo[Oi]).Index = Origin - 1 then begin
        TSortColInfo(FSortInfo[Oi]).Index := Destination - 1;
        //TSortColInfo(FSortInfo[Oi]).Desc  := not TSortColInfo(FSortInfo[Oi]).Desc;
        continue;
      end;
      if (TSortColInfo(FSortInfo[Oi]).Index > Mx)or(TSortColInfo(FSortInfo[Oi]).Index < Mn) then
        continue;
      if Destination < Origin then
        Inc(TSortColInfo(FSortInfo[Oi]).Index)
      else
        Dec(TSortColInfo(FSortInfo[Oi]).Index);
    end;
  end;
end;

procedure TCRDBGrid.SetLevelDelimiterchar(const Value: char);
begin
  FLevelDelimiterchar := Value;
end;

procedure TCRDBGrid.WMSetCursor(var Message: TWMSetCursor);
var
  P: TPoint;
begin
  GetCursorPos(P);
  P := ScreenToClient(P);
  if MouseInSortEdit(p.x,p.y) or MouseInFilterEdit(p.x,p.y) then
    Windows.SetCursor(LoadCursor(0, IDC_IBEAM))
  else
    inherited;
end;

function TCRDBGrid.MouseInSortBar(X,Y: integer; Column: TColumn = nil): boolean;
var
  Index: integer;
  Rect: TRect;
begin
  Result := False;
  if not (dgeSearchBar in OptionsEx) then
    Exit;
  if Column = nil then begin
    Index := DataToRawColumn(MouseCoord(X,Y).X);
    Column := Columns[Index];
  end;
  Rect := CalcSearchBar(Column);
  Result := PtInRect(Rect,Point(x,y));
end;

function TCRDBGrid.MouseInSortEdit(X,Y: integer; Column: TColumn = nil): boolean;
var
  Index: integer;
  Rect: TRect;
begin
  Result := False;
  if not (dgeSearchBar in OptionsEx) then
    exit;
  if Column = nil then begin
    Index := RawToDataColumn(MouseCoord(X,Y).X);
    if Index < 0 then
        exit;
    Column := Columns[Index];
  end;
  Rect := CalcSearchBar(Column);
  InflateRect(Rect, -5, -5);
  Result := PtInRect(Rect,Point(x,y)) and TCRColumn(Column).CanBeSorted;
end;

function TCRDBGrid.MouseInFilterBar(X,Y: integer; Column: TColumn = nil): boolean;
var
  Index: integer;
  Rect: TRect;
begin
  Result := False;
  if not (dgeFilterBar in OptionsEx) then
    Exit;
  if Column = nil then begin
    Index := DataToRawColumn(MouseCoord(X,Y).X);
    if Index < 0 then
      Exit;
    Column := Columns[Index];
  end;
  Rect := CalcFilterBar(Column);
  Result := PtInRect(Rect, Point(x,y));
end;

function TCRDBGrid.MouseInFilterEdit(X,Y: integer; Column: TColumn = nil): boolean;
var
  Index: integer;
  Rect: TRect;
begin
  Result := False;
  if not (dgeFilterBar in OptionsEx) then
    Exit;
  if Column = nil then begin
    Index := RawToDataColumn(MouseCoord(X, Y).X);
    if Index < 0 then
      Exit;
    Column := Columns[Index];
  end;
  Rect := CalcFilterBar(Column);
  InflateRect(rect, -5, -5);
  Result := PtInRect(Rect, Point(x,y)) and TCRColumn(Column).CanBeSorted;
end;

function TCRDBGrid.CalcSearchBar(Column: TColumn): TRect;
var
  Rect: TRect;
  MasterCol: TColumn;
  aRow: integer;
begin
  aRow := 0;
  Rect := CalcTitleRect(Column,aRow,MasterCol);
  Rect.Top := Rect.Bottom - (DefaultRowHeight + 9);
  if not (dgeSearchBar in OptionsEx) then begin
    Result.Top := Result.Bottom;
    exit;
  end;
  Result := Rect;
end;

function TCRDBGrid.CalcFilterBar(Column: TColumn): TRect;
var
  Rect: TRect;
  MasterCol: TColumn;
  aRow: integer;
begin
  aRow := 0;
  Rect := CalcTitleRect(Column, aRow, MasterCol);
  if dgeSearchBar in OptionsEx then
  begin
    Rect.Bottom := Rect.Bottom - (DefaultRowHeight + 9);
    if dgRowLines in Options then
      Dec(Rect.Bottom);
  end;
  Rect.Top := Rect.Bottom - (DefaultRowHeight + 9);
//  if not (dgRowLines in Options) then
//    Inc(Rect.Top);
  Result := Rect;
  if not (dgeFilterBar in OptionsEx) then begin
    Result.Top := Result.Bottom;
    exit;
  end;
end;

procedure TCRDBGrid.ActivateFilterEdit(Column: TColumn);
var
  CellRect: TRect;
begin
  if not (Assigned(Column) and (dgeFilterBar in OptionsEx)) then
    Exit;
  CellRect := CalcFilterBar(Column);
  InflateRect(CellRect, -5, -5);
  if not (dgRowLines in Options) then
    Dec(CellRect.Top);
  CRGridTitleEdit.ActivateAt(CellRect, Column, True);
end;

procedure TCRDBGrid.DoOnMemoClick(Column: TColumn);
var
  Window: TMemoEditorForm;
begin
  if Assigned(FOnMemoClick) then
    FOnMemoClick(Self, Column)
  else begin
    Window := TMemoEditorForm.Create(GetParentForm(Self));
    try
      Window.FMemo.Text := AdjustLineBreaks(Column.Field.AsString);
      Window.ReadOnly := ReadOnly or Column.Field.ReadOnly or not (dgEditing in Options);
      Window.Caption := Column.Field.DisplayName;
      if (FMemoWidth > 0) and (FMemoHeight > 0) then begin
        Window.Width := FMemoWidth;
        Window.Height := FMemoHeight;
      end;
      Window.FCheckBox.Checked := FMemoWordWrap;
      if Window.ShowModal = mrOK then begin
        DataLink.DataSet.Edit;
        Column.Field.AsString := Window.FMemo.Text;
      end;
      FMemoWidth := Window.Width;
      FMemoHeight := Window.Height;
      FMemoWordWrap := Window.FCheckBox.Checked;
    finally
      Window.Free;
    end;
  end;
end;

function TCRDBGrid.MouseInLowerstLevel(X, Y: integer; Column: TColumn = nil): boolean;
var
  Index: integer;
  Rect: TRect;
  MasterCol: TColumn;
begin
  Result := False;
  if Column = nil then begin
    Index   := RawToDataColumn(MouseCoord(X, Y).X);
    if Index < 0 then
        exit;
    Column  := Columns[Index];
  end;
  Index := 0;
  Rect    := CalcTitleRect(Column, Index, MasterCol);
  Index   := GetCaptionDepth(Column.Title.Caption, FLevelDelimiterChar);
  if Index > 0 then begin
    Index   := (Index-1) * (DefaultRowHeight + 1);
    Rect.Top := Index;
    Rect.Bottom := CalcFilterBar(Column).top;
    Result  := PtInRect(Rect, Point(X, Y));
  end
  else
    Result := True;
end;

procedure TCRDBGrid.DrawTitleBarCell(Canvas: TCanvas; Column: TColumn;
  Rect: TRect; Text: string);
var
  TextRect: TRect;
begin
  Canvas.Brush.Color := clBtnFace;
  Canvas.Brush.Style := bsSolid;
  Canvas.FillRect(Rect);

  if (dgRowLines in Options) and (dgColLines in Options) then begin
    DrawEdge(Canvas.Handle, Rect, BDR_RAISEDINNER, BF_BOTTOMRIGHT);
    DrawEdge(Canvas.Handle, Rect, BDR_RAISEDINNER, BF_TOPLEFT);
  end;

  if TCRColumn(column).CanBeSorted then begin
    if (dgRowLines in Options) and (dgColLines in Options) then begin
      InflateRect(Rect, -4, -4);
      if Rect.Right > Rect.Left then begin
        DrawEdge(Canvas.Handle, Rect, BDR_SUNKENINNER, BF_TOPLEFT);
        Rect.Bottom := Rect.Bottom + 1;
        DrawEdge(Canvas.Handle, Rect, BDR_SUNKENINNER, BF_BOTTOMRIGHT);
        InflateRect(Rect, 1, 1);
        DrawEdge(Canvas.Handle, Rect, BDR_SUNKENOUTER, BF_TOPLEFT);
        DrawEdge(Canvas.Handle, Rect, BDR_SUNKENOUTER, BF_BOTTOMRIGHT);
        InflateRect(Rect, 3, 3);
        Rect.Bottom := Rect.Bottom - 1;
      end
      else
        InflateRect(Rect, 4, 4);
    end;

    TextRect := Rect;
    InflateRect(TextRect, -5, -5);
    TextRect.Bottom := TextRect.Top + DefaultRowHeight;
    if TextRect.Right >= TextRect.Left then begin
      Canvas.Brush.Color := clWindow;
      Canvas.Brush.Style := bsSolid;
      Canvas.FillRect(TextRect);
    end;

    InflateRect(TextRect, -2, -2);
    if (Text <> '') and (TextRect.Right >= TextRect.Left) then
      WriteText(Canvas, TextRect, 0, 0, Text, taLeftJustify, True);
  end;

  Canvas.Pen.Color := clBlack;
  if dgRowLines in options then begin
    Canvas.MoveTo(Rect.Left, Rect.Bottom);
    Canvas.LineTo(Rect.Right + 1, Rect.Bottom);
  end;
  if dgColLines in options then begin
    Canvas.MoveTo(Rect.Right, Rect.Top);
    Canvas.LineTo(Rect.Right, Rect.Bottom);
  end;
end;

procedure TCRDBGrid.DrawTitleIndicatorCell(Canvas: TCanvas; ARect: TRect);
  procedure DrawButton(Rect: TRect; Bmp: TBitmap; IsPressed: boolean);
  var
    Delta, Flag, PressOffset: integer;
  begin
    PressOffset := 0;
    Delta := Ceil((Rect.Bottom - Rect.Top - Bmp.Height) / 2);
    if ((dgRowLines in Options) and (dgColLines in Options)) {or PtInRect(Rect,P)} then begin
      if IsPressed then begin
        PressOffset := 1;
        Flag := BDR_SUNKENINNER
      end
      else
        Flag := BDR_RAISEDINNER;
      DrawEdge(Canvas.Handle, Rect, Flag, BF_BOTTOMRIGHT);
      DrawEdge(Canvas.Handle, Rect, Flag, BF_TOPLEFT);
    end;
    Canvas.Draw(Rect.Left + 1 + PressOffset, Rect.Top + Delta + PressOffset, Bmp);
    if dgRowLines in Options then begin
      Canvas.Pen.Color := clBlack;
      Canvas.MoveTo(Rect.Left, Rect.Bottom);
      Canvas.LineTo(Rect.Right + 1, Rect.Bottom);
    end;
  end;
var
  Rect: TRect;
  Bmp: TBitmap;
  i: integer;
begin
  Canvas.FillRect(ARect);
  Rect := ARect;

  if dgeSearchBar in OptionsEx then begin
    Rect.Top := Rect.Bottom - DefaultRowHeight - 9;
    if not((dgRowLines in Options) or (dgeFilterBar in OptionsEx)) then
      Inc(Rect.Top);
    DrawButton(Rect, bmpSearch, FIndicatorColBtnDown = icbSearch);
    Dec(Rect.Bottom, DefaultRowHeight + 9);
    if dgRowLines in Options then
      Dec(Rect.Bottom)
  end;

  if dgeFilterBar in OptionsEx then begin
    Rect.Top := Rect.Bottom - DefaultRowHeight - 9;
    if not((dgRowLines in Options) or (dgeSearchBar in OptionsEx)) then
      Inc(Rect.Top);

    if CRGridTitleEdit.EditingFilter then
      Bmp := bmpEditMode
    else
      if FFiltered then begin
        Bmp := bmpFilter;
        for i := 0 to Columns.Count - 1 do
          if TCRColumn(Columns[i]).FilterExpression <> '' then begin
            Bmp := bmpActiveFilter;
            Break;
          end;
      end
      else
        Bmp := bmpFilter;

    DrawButton(Rect, Bmp, FIndicatorColBtnDown = icbFilter);
    Dec(Rect.Bottom, DefaultRowHeight + 9);
    if (dgRowLines in Options) then
      Dec(Rect.Bottom);
  end;

  Rect.Top := ARect.Top;
  if not(dgRowLines in Options)
  and ((dgeSearchBar in OptionsEx) xor (dgeFilterBar in OptionsEx)) then
    Inc(Rect.Bottom);
  DrawButton(Rect, bmpMenu, FIndicatorColBtnDown = icbMenu);

  Canvas.Pen.Color := clBlack;
  if dgColLines in Options then begin
    Canvas.MoveTo(ARect.Right, ARect.Top);
    Canvas.LineTo(ARect.Right, ARect.Bottom + 1);
  end;
end;

function TCRDBGrid.GetIndicatorButton(X, Y: integer): TIndicatorColButton;
var
  Rect: TRect;
begin
  Result := icbNone;
  Rect := CellRect(0,0);

  if dgeSearchBar in OptionsEx then begin
    Rect.Top := Rect.Bottom - DefaultRowHeight - 9;
    if not((dgRowLines in Options) or (dgeFilterBar in OptionsEx)) then
      Inc(Rect.Top);
    if PtInRect(Rect,Point(X,Y)) then
      Result := icbSearch;
    Dec(Rect.Bottom, DefaultRowHeight + 9);
    if dgRowLines in Options then
      Dec(Rect.Bottom);
  end;

  if dgeFilterBar in OptionsEx then begin
    Rect.Top := Rect.Bottom - DefaultRowHeight - 9;
    if not((dgRowLines in Options) or (dgeSearchBar in OptionsEx)) then
      Inc(Rect.Top);
    if PtInRect(Rect,Point(X,Y)) then
      Result := icbFilter;
    Dec(Rect.Bottom, DefaultRowHeight + 9);
    if dgRowLines in Options then
      Dec(Rect.Bottom);
  end;

  Rect.Top := CellRect(0,0).Top;
  if not(dgRowLines in Options)
  and ((dgeSearchBar in OptionsEx) xor (dgeFilterBar in OptionsEx)) then
    Inc(Rect.Bottom);
  if PtInRect(Rect,Point(X,Y)) then
    Result := icbMenu;
end;

procedure TCRDBGrid.IndicatorClick(Button: TIndicatorColButton; X, Y: integer);
var
  P: TPoint;
begin
  EditorMode := False;
  case Button of
    icbFilter:
      FilterItemClick(FOptionsMenuDef.Items[2]); // watch it
    icbSearch:
      SearchItemClick(FOptionsMenuDef.Items[3]); // watch it
    icbMenu:
    begin
      P := ClientToScreen(Point(X, Y));
      if FOptionsMenu <> nil then
        FOptionsMenu.Popup(P.x, P.y)
      else
        FOptionsMenuDef.Popup(P.x, P.y);
    end;
  end;
end;

procedure TCRDBGrid.BuildMenu;
var
  Item: TMenuitem;
begin
  while FOptionsMenuDef.Items.Count > 0 do
    FOptionsMenuDef.Items.Delete(0);

  Item := TMenuItem.Create(Self);
  Item.Caption := SFiltered;
  Item.OnClick := FilteredItemClick;
  Item.Checked := FFiltered;
  FOptionsMenuDef.Items.Add(Item);

  Item := TMenuItem.Create(Self);
  Item.Caption := '-';
  FOptionsMenuDef.Items.Add(Item);

  Item := TMenuItem.Create(Self);
  Item.Caption := SFilterBar;
  Item.OnClick := FilterItemClick;
  Item.Checked := dgeFilterBar in OptionsEx;
  FOptionsMenuDef.Items.Add(Item);

  Item := TMenuItem.Create(Self);
  Item.Caption := SSearchBar;
  Item.OnClick := SearchItemClick;
  Item.Checked := dgeSearchBar in OptionsEx;
  FOptionsMenuDef.Items.Add(Item);
end;

procedure TCRDBGrid.FilterItemClick(Sender: TObject);
begin
  if dgeFilterBar in OptionsEx then begin
    OptionsEx := OptionsEx - [dgeFilterBar];
    UpdateRowCount;
  end
  else
     OptionsEx :=  OptionsEx + [dgeFilterBar];
  (Sender as TMenuItem).Checked := dgeFilterBar in OptionsEx;
end;

procedure TCRDBGrid.SearchItemClick(Sender: TObject);
begin
  if dgeSearchBar in OptionsEx then begin
     OptionsEx := OptionsEx - [dgeSearchBar];
     UpdateRowCount;
  end
  else
     OptionsEx :=  OptionsEx + [dgeSearchBar];
  (Sender as TMenuItem).Checked := dgeSearchBar in OptionsEx;
end;

procedure TCRDBGrid.CalcTableSpacePercent;
var
  ColumnsSize, i: integer;
begin
  ColumnsSize := 0;
  for i := 0 to Columns.count - 1 do
    if ColWidths[i + IndicatorOffset] > 0 then
      ColumnsSize := ColumnsSize + ColWidths[i + IndicatorOffset];
  for i := 0 to Columns.Count - 1 do
    if ColumnsSize > 0 then
      TCRColumn(Columns[i]).FTableSpacePercent := ColWidths[i + IndicatorOffset] / ColumnsSize;
end;

procedure TCRDBGrid.KeyDown(var Key: word; Shift: TShiftState);
var
  Column: TColumn;
  i: integer;

  procedure ActivateEdit;
  begin
    if dgeSearchBar in OptionsEx then begin
      ActivateSearchEdit(Column);
      Key := 0;
    end
    else if dgeFilterBar in OptionsEx then begin
      ActivateFilterEdit(Column);
      Key := 0;
    end;
  end;
begin
  case Key of
    VK_UP:
      if (dgTitles in Options) and (Row = 1) and ((Shift = [ssShift])
      or (DataLink.Active and ((DataLink.DataSet.RecNo = 1) or (DataLink.DataSet.recordcount = 0)))) then begin
        Column := Columns[RawToDataColumn(Col)];
        if TCRColumn(Column).CanBeSorted then
          ActivateEdit;
      end;
    VK_TAB:
      if (dgTitles in Options) and (Row = 1) and (Col = IndicatorOffset)
      and (Shift = [ssShift]) then begin
        for i := Columns.Count - 1 downto 0 do
          if TCRColumn(Columns[i]).CanBeSorted then begin
            Column := Columns[i];
            MoveColRow(DataToRawColumn(i), TopRow, True, True);
            Break;
          end;
        if Column <> nil then
          ActivateEdit;
      end;
    VK_RETURN: begin
      if SelectedField is TMemoField then begin
        DoOnMemoClick(Columns[RawToDataColumn(Col)]);
        Key := 0;
        HideEditor;
      end;
    end;
  end;
  case Key of
    VK_UP, VK_PRIOR, VK_DOWN, VK_NEXT, VK_HOME, VK_END :
      InvalidateRect(Handle,{$IFNDEF CLR}@{$ENDIF}FStatusRect,False);
  end;

  inherited;
end;

function TCRDBGrid.CanEditShow: boolean;
begin
  if (Columns.Count > 0) and Assigned(SelectedField) and (SelectedField is TMemoField) then
    Result := False
  else
    Result := inherited CanEditShow;
end;

procedure TCRDBGrid.TopLeftChanged;
{$IFDEF VER4}
var
  R: TRect;
  DrawInfo: TGridDrawInfo;
{$ENDIF}
begin
  inherited;
{$IFDEF VER4}
  if HandleAllocated and (dgTitles in Options) then
  begin
    CalcFixedInfo(DrawInfo);
    R := Rect(0, 0, Width, DrawInfo.Vert.FixedBoundary);
    InvalidateRect(Handle, {$IFNDEF CLR}@{$ENDIF}R, False);
  end;
{$ENDIF}
  InvalidateRect(Handle,{$IFNDEF CLR}@{$ENDIF}FStatusRect,False);
end;

procedure TCRDBGrid.SetFiltered(const Value: boolean);
begin
  if FFiltered <> Value then begin
    FFiltered := Value;
    FOptionsMenuDef.Items[0].Checked := FFiltered;
    if (DataSource = nil) or (DataSource.DataSet = nil) then
      Exit;

    if Value then
      ApplyFilter;
    if dgeLocalFilter in OptionsEx then
      DataSource.DataSet.Filtered := Value
    else begin
      if not Value then
        if DataLink.DataSet is TCustomDADataSet then
          TCustomDADataSet(DataSource.DataSet).FilterSQL := '';
    end;
  end
end;

procedure TCRDBGrid.UpdateRowCount;
begin
  with DataLink do
    if Active and (RecordCount > 0) and HandleAllocated then begin
      RowCount := 1000;
      BufferCount := VisibleRowCount;
      if dgTitles in Options then
        RowCount := RecordCount + 1
      else
        RowCount := RecordCount;
    end;
 Invalidate;
end;

procedure TCRDBGrid.ApplyFilter;
var
  i: integer;
  St, FilterText: string;
  OldActiveColumn: integer;
begin
  if not FFiltered then
    Exit;

  FilterText := '';
  for i := 0 to Columns.Count - 1 do begin
    St := TCRColumn(Columns[i]).GetFilterExpression(TCRColumn(Columns[i]).FilterExpression);
    if St <> '' then begin
      if FilterText <> '' then
        FilterText := FilterText + ' AND ';
      FilterText := FilterText + St;
    end;
  end;

  if dgeLocalFilter in OptionsEx then begin
    DataSource.DataSet.Filter := FilterText;
    DataSource.DataSet.Filtered := FFiltered;
//    TOraDataSet(DataSource.DataSet).Filtered := FilterText <> '';
  end
  else
    if DataSource.DataSet is TCustomDADataSet then begin
      OldActiveColumn := -1;
      if CRGridTitleEdit.FActiveColumn <> nil then
        for i := 0 to Columns.Count - 1 do
          if CRGridTitleEdit.FActiveColumn = Columns[i] then begin
            OldActiveColumn := i;
            Break;
          end;

      CRGridTitleEdit.FActiveColumn := nil;
      TCustomDADataSet(DataSource.DataSet).FilterSQL := FilterText;

      if OldActiveColumn <> -1 then
        CRGridTitleEdit.FActiveColumn := Columns[OldActiveColumn];
      for i := 0 to High(CRGridTitleEdit.FFilterExpressions) do
        TCRColumn(Columns[i]).FilterExpression := CRGridTitleEdit.FFilterExpressions[i];
    end;

  ResetTotals;
  LoadTotals;
end;

procedure TCRDBGrid.FilteredItemClick(Sender: TObject);
begin
  Filtered := not Filtered;
  (Sender as TMenuItem).Checked := FFiltered;
end;

procedure TCRDBGrid.DoExit;
begin
  inherited;

  if not FContinueEditingFilter then
    CRGridTitleEdit.EditingFilter := False
  else
    FContinueEditingFilter := False;
end;

procedure TCRDBGrid.MoveColRow(ACol, ARow: Longint; MoveAnchor, Show: Boolean);
begin
  inherited MoveColRow(ACol, ARow, MoveAnchor, Show);
end;

function TCRDBGrid.DataToRawColumn(ACol: Integer): Integer;
begin
  Result := inherited DataToRawColumn(ACol);
end;

procedure TCRDBGrid.InvalidateCol(ACol: Longint);
begin
  inherited InvalidateCol(ACol);
end;

procedure TCRDBGrid.InvalidateRow(ARow: Longint);
begin
  inherited InvalidateRow(ARow);
end;

procedure TCRDBGrid.AdjustColumns;
var
  Width: array of integer;
  i, j, OldActive: integer;
  CurWidth: Integer;
begin
  if not DataLink.Active then
    Exit;
  SetLength(Width, Columns.Count);

  OldActive := DataLink.ActiveRecord;
  try
    for i := TopRow - 1 to VisibleRowCount - 1 do begin
      Datalink.ActiveRecord := i;
      for j := 0 to Columns.Count - 1 do begin
        if Assigned(Columns[j].Field) then
          CurWidth := Canvas.TextWidth(Columns[j].Field.DisplayText)
        else
          CurWidth := 0;
        if CurWidth > Width[j] then
          Width[j] := CurWidth;
      end;
    end;
  finally
    DataLink.ActiveRecord := OldActive;
  end;

  for i := 0 to Columns.Count - 1 do begin
    CurWidth := Canvas.TextWidth(Columns[i].Title.Caption);
    if CurWidth > Width[i] then
      ColWidths[i + IndicatorOffset] := CurWidth + 4
    else
      ColWidths[i + IndicatorOffset] := Width[i] + 4;
  end;
end;

function TCRDBGrid.GetColumns: TCRDBGridColumns;
begin
  Result := TCRDBGridColumns(inherited Columns);
end;

procedure TCRDBGrid.SetColumns(const Value: TCRDBGridColumns);
begin
  inherited Columns.Assign(Value);
end;

procedure TCRDBGrid.WMMouseWheel(var Message: TWMMouseWheel);
begin
  if Message.WheelDelta > 0 then
    SendMessage(Handle, WM_KEYDOWN, VK_UP, 0)
  else
    SendMessage(Handle, WM_KEYDOWN, VK_DOWN, 0);
end;

procedure TCRDBGrid.TitleClick(Column: TColumn);
begin
  if FTitleBarUp then
    inherited TitleClick(Column);
end;

{ TCRColumnTitle }

function TCRColumnTitle.GetCaption: string;
begin
  Result := inherited Caption;
end;

function TCRColumnTitle.IsCaptionStored: boolean;
begin
  Result := (cvTitleCaption in Column.AssignedValues) and
    (Caption <> DefaultCaption);
end;

procedure TCRColumnTitle.SetCaption(const Value: string);
begin
  if Value <> inherited Caption then begin
    inherited Caption := Value;

    TCRColumn(Column).ChangedTitle(True);
  end;
end;

{ TCRGridTitleEdit }

procedure TCRGridTitleEdit.ActivateAt(ARect: TRect; ActiveColumn: TColumn; AsFilter: boolean);
begin
  if not Assigned(CRDBGrid) then
    Exit;

  try
    StopEdit(True);
    FEdit.Visible := False;
    FAsFilter := AsFilter;
    FActiveColumn := ActiveColumn;
    SetClientRect(ARect);

    if AsFilter then begin
      if EditingFilter then
        Caption := FFilterExpressions[FActiveColumn.Index]
      else
        Caption := TCRColumn(FActiveColumn).FilterExpression;
    end
    else
      Caption := '';
  finally
    Visible := True;
    SetFocus;
  end;
end;

constructor TCRGridTitleEdit.Create(AOwner: TComponent);
begin
  inherited;

  if AOwner is TCRDBGrid then
    CRDBGrid := AOwner as TCRDBGrid;
  Visible := False;
  AutoSize := False;
  Anchors := Anchors + [akRight];
  Width := 0;
  Height := 0;

  FEdit := TEdit.Create(Self);
  FEdit.Visible := False;
  FEdit.TabStop := False;
  FEdit.BorderStyle := bsNone;
  FEdit.Width := 0;
  FEdit.Height := 0;
  InsertControl(FEdit);
  FEdit.Parent := Self;
  FEdit.ParentFont := False;
  FEdit.OnKeyPress := FEditKeyPress;
  FEdit.OnKeyDown := FEditKeyDown;
  FEdit.OnChange  := FEditChange;
  FEdit.OnExit := FEditExit;
end;

procedure TCRGridTitleEdit.DoExit;
begin
  inherited;

  FEdit.Visible := False;
  Visible := False;
end;

procedure TCRGridTitleEdit.FEditChange(Sender: TObject);
begin
  if not FEdit.Modified then
    Exit;
  if FAsFilter then
    EditingFilter := True
  else
    ProcessEdit;
end;

procedure TCRGridTitleEdit.FEditExit(Sender: TObject);
begin
  StopEdit(True);
end;

procedure TCRGridTitleEdit.WMMouseWheel(var Message: TWMMouseWheel);
begin
  if Message.WheelDelta > 0 then
    SendMessage(Handle, WM_KEYDOWN, VK_UP, 0)
  else
    SendMessage(Handle, WM_KEYDOWN, VK_DOWN, 0);
end;

procedure TCRGridTitleEdit.FEditKeyDown(Sender: TObject; var Key: word;
  Shift: TShiftState);
var
  OldKey: word;
  OldWidth : integer;
begin
  OldKey := Key;
  Key := 0;
  case OldKey of
    VK_RETURN: begin
      OldWidth := Self.Width;
      StopEdit(True);
      Self.Width := OldWidth;
      Visible := True;
      SetFocus;
    end;
    VK_ESCAPE: begin
      StopEdit(False);
      Visible := True;
      SetFocus;
    end;
    VK_UP:
      GotoUpperCell;
    VK_DOWN:
      GotoLowerCell;
    VK_TAB: begin
      if Shift = [ssShift] then
        GotoPrevCell
      else if Shift = [] then
        GotoNextCell;
    end;
    else
      Key := OldKey;
  end;
end;

procedure TCRGridTitleEdit.FEditKeyPress(Sender: TObject; var Key: char);
begin
  if (Key = #13) or (Key = #27) then
    Key := #0;
end;

procedure TCRGridTitleEdit.GotoLowerCell;
begin
  if Assigned(CRDBGrid) then begin
    if FAsFilter then begin
      StopEdit(True);
      EditingFilter := False;
      if dgeSearchBar in CRDBGrid.OptionsEx then begin
        CRDBGrid.ActivateSearchEdit(FActiveColumn);
        Exit;
      end;
    end;
    with CRDBGrid do
      if DataLink.Active then begin
        Col := DataToRawColumn(FActiveColumn.Index);
        DataLink.DataSet.MoveBy(TopRow - Row);
      end;
    CRDBGrid.SetFocus;
  end;
end;

procedure TCRGridTitleEdit.GotoNextCell;
var
  i, Start: integer;
begin
  if not (Assigned(FActiveColumn) and Assigned(CRDBGrid)) then
    Exit;
  StopEdit(True);
  if FAsFilter then begin
    Start := 0;
    for i := FActiveColumn.Index + 1 to CRDBGrid.Columns.Count - 1 do
      if TCRColumn(CRDBGrid.Columns[i]).CanBeSorted then begin
        CRDBGrid.MoveColRow(CRDBGrid.DataToRawColumn(i), CRDBGrid.TopRow, True, True);
        CRDBGrid.ActivateFilterEdit(CRDBGrid.Columns[i]);
        Exit;
      end;
    EditingFilter := False;
  end
  else
    Start := FActiveColumn.Index + 1;

  if dgeSearchBar in CRDBGrid.OptionsEx then
    for i := Start to CRDBGrid.Columns.Count - 1 do
      if TCRColumn(CRDBGrid.Columns[i]).CanBeSorted then begin
        CRDBGrid.MoveColRow(CRDBGrid.DataToRawColumn(i), CRDBGrid.TopRow, True, True);
        CRDBGrid.ActivateSearchEdit(CRDBGrid.Columns[i]);
        Exit;
      end;

  with CRDBGrid do
    if DataLink.Active then begin
      LeftCol := 1;
      Col := DataToRawColumn(0);
      DataLink.DataSet.MoveBy(TopRow - Row);
    end;
  CRDBGrid.SetFocus;
end;

procedure TCRGridTitleEdit.GotoPrevCell;
var
  i, Start: integer;
begin
  if not (Assigned(FActiveColumn) and Assigned(CRDBGrid)) then
    Exit;

  StopEdit(True);
  if not FAsFilter then begin
    Start := CRDBGrid.Columns.Count - 1;
    for i := FActiveColumn.Index - 1 downto 0 do
      if TCRColumn(CRDBGrid.Columns[i]).CanBeSorted then begin
        CRDBGrid.MoveColRow(CRDBGrid.DataToRawColumn(i), CRDBGrid.TopRow, True, True);
        CRDBGrid.ActivateSearchEdit(CRDBGrid.Columns[i]);
        Exit;
      end;
  end
  else
    Start := FActiveColumn.Index - 1;

  if dgeFilterBar in CRDBGrid.OptionsEx then
    for i := Start downto 0 do
      if TCRColumn(CRDBGrid.Columns[i]).CanBeSorted then begin
        CRDBGrid.MoveColRow(CRDBGrid.DataToRawColumn(i), CRDBGrid.TopRow, True, True);
        CRDBGrid.ActivateFilterEdit(CRDBGrid.Columns[i]);
        Exit;
      end;
end;

procedure TCRGridTitleEdit.GotoUpperCell;
begin
  if not FAsFilter and (dgeFilterBar in CRDBGrid.OptionsEx)
    and Assigned(CRDBGrid)
  then begin
    CRDBGrid.ActivateFilterEdit(FActiveColumn);
  end;
end;

procedure TCRGridTitleEdit.KeyDown(var Key: word; Shift: TShiftState);
var
  OldKey: word;
begin
  inherited;

  OldKey := Key;
  Key := 0;
  case OldKey of
    VK_RETURN :
      StartEdit;
    VK_UP:
      GotoUpperCell;
    VK_DOWN:
      GotoLowerCell;
    VK_RIGHT:
      GotoNextCell;
    VK_LEFT:
      GotoPrevCell;
    VK_TAB: begin
      if Shift = [ssShift] then
        GotoPrevCell
      else if Shift = [] then
        GotoNextCell;
    end;
    VK_ESCAPE:
      if EditingFilter then begin
        SetLength(FFilterExpressions,0);
        EditingFilter := False;
        Caption := TCRColumn(FActiveColumn).FilterExpression;
        CRDBGrid.InvalidateRow(0);
        Invalidate;
      end;
    else
      Key := OldKey;
  end;
end;

procedure TCRGridTitleEdit.MouseDown(Button: TMouseButton;
  Shift: TShiftState; X, Y: integer);
begin
  inherited;

  if Focused and (Button = mbLeft) then
    StartEdit;
end;

procedure TCRGridTitleEdit.PaintWindow(DC: HDC);
var
  Rect: TRect;
  Brush: THandle;
  OldBrush: THandle;
  OldFont: THandle;
  BrushColor: TColor;
  FontColor: TColor;
begin
  Rect := GetClientRect;
  if Focused then begin
    BrushColor := clHighlight;
    FontColor := clWhite;
  end
  else begin
    BrushColor := Color;
    FontColor := Font.Color;
  end;

  Brush := CreateSolidBrush(ColorToRGB(BrushColor));
  try
    OldBrush := SelectObject(DC, Brush);
    FillRect(DC, Rect, Brush);
    if Focused then
      DrawFocusRect(DC, Rect);
    InflateRect(Rect, -2, -2);
    SetTextColor(DC, ColorToRGB(FontColor));
    SetBkColor(DC, ColorToRGB(BrushColor));
    OldFont := SelectObject(DC, Font.Handle);
    DrawText(DC, PChar(Caption), Length(Caption), Rect, DT_LEFT or DT_VCENTER);
    SelectObject(DC, OldFont);
    SelectObject(DC, OldBrush);
  finally
    DeleteObject(Brush);
  end;
end;

procedure TCRGridTitleEdit.PostFilter;
var
  i: integer;
begin
  StopEdit(True);
  if (CRDBGrid = nil) or (Length(FFilterExpressions) = 0) then
    Exit;
  for i := 0 to High(FFilterExpressions) do begin
    TCRColumn(CRDBGrid.Columns[i]).FilterExpression := FFilterExpressions[i];
  end;
  CRDBGrid.ApplyFilter;
end;

procedure TCRGridTitleEdit.ProcessEdit;
begin
  if (FActiveColumn = nil) or (CRDBGrid = nil) or not FEdit.Modified then
    Exit;

  if FAsFilter then
    with CRDBGrid do begin
      try
        TCRColumn(FActiveColumn).GetFilterExpression(FEdit.Text);
        FFilterExpressions[FActiveColumn.Index] := FEdit.Text;
        Self.Caption := FFilterExpressions[FActiveColumn.Index];
      except
        on EConvertError do begin
          FEdit.SelectAll;
          raise;
        end;
      end;
    end
  else
    try
      with FActiveColumn.Field do
        DataSet.Locate(FieldName, {$IFDEF CLR}Variant{$ENDIF}(FEdit.Text), [loCaseInsensitive,loPartialKey]);
    except
      on EConvertError do
        Exit;
    end;
end;

procedure TCRGridTitleEdit.SetClientRect(ARect: TRect);
begin
  Top := ARect.Top;
  Left := ARect.Left;
  Width := ARect.Right - ARect.Left;
  Height := CRDBGrid.DefaultRowHeight;
  FEdit.Top := 2;
  FEdit.Left := 2;
  FEdit.Width := Width - 2;
  FEdit.Height := Height - 2;
end;

procedure TCRGridTitleEdit.SetCRDBGrid(const Value: TCRDBGrid);
begin
  FCRDBGrid := Value;
end;

procedure TCRGridTitleEdit.SetEditingFilter(const Value: boolean);
var
  i: integer;
begin
  if FEditingFilter <> Value then begin
    FEditingFilter := Value;
    if Assigned(CRDBGrid) then begin
      if not Value then begin
        PostFilter;
        CRDBGrid.FContinueEditingFilter := False;
      end
      else
        with CRDBGrid do begin
          SetLength(FFilterExpressions, Columns.Count);
          for i := 0 to Columns.Count - 1 do
            FFilterExpressions[i] := TCRColumn(Columns[i]).FilterExpression;
        end;
    end;
    if Assigned(CRDBGrid) and (dgIndicator in CRDBGrid.Options) then
      CRDBGrid.InvalidateCol(0);
  end;
end;

procedure TCRGridTitleEdit.SetFocus;
begin
  inherited;
  Invalidate;
end;

procedure TCRGridTitleEdit.StartEdit;
begin
  if CRDBGrid = nil then
    Exit;
  if FAsFilter then begin
      FEdit.Text := Caption;
  end
  else
    FEdit.Text := '';
  FEdit.Font := CRDBGrid.Font;
  FEdit.Modified := False;
  FEdit.Visible := True;
  FEdit.SetFocus;
  Invalidate;
end;

procedure TCRGridTitleEdit.StopEdit(AcceptChanges: boolean);
begin
  if not FEdit.Visible then
    Exit;
  if AcceptChanges then
    ProcessEdit;
  CRDBGrid.FContinueEditingFilter := EditingFilter;
  FEdit.Modified := False;
  FEdit.Visible := False;
  if AcceptChanges then
    EditingFilter := False;
  Invalidate;
//  Visible := True;
//  SetFocus;
end;

procedure TCRGridTitleEdit.WMChar(var Message: TWMChar);
begin
  inherited;

  if (Message.CharCode > 0) and (Message.CharCode <> VK_TAB)
    and (Message.CharCode <> VK_ESCAPE)
  then begin
    StartEdit;
    if Message.CharCode <> VK_RETURN then
      SendMessage(FEdit.Handle, Message.Msg , {$IFDEF VER9P}Message.CharCode{$ELSE}TMessage(Message).WParam{$ENDIF},
       {$IFDEF VER9P}Message.KeyData{$ELSE}TMessage(Message).LParam{$ENDIF});
  end;
end;

procedure TCRGridTitleEdit.WMGetDlgCode(var Message: TWMGetDlgCode);
begin
  Message.Result := Message.Result or DLGC_WANTARROWS;
  if Assigned(CRDBGrid) and (dgTabs in CRDBGrid.Options) then
    Message.Result := Message.Result or DLGC_WANTTAB;
end;

{ TMemoEditorForm }

function TMemoEditorForm.CloseQuery: boolean;
begin
  Result := inherited CloseQuery;
  if FMemo.Modified and (ModalResult <> mrOK) then
    case MessageDlg(Format(fmtModifiedWarning,[Caption]), mtConfirmation, mbYesNoCancel, 0) of
      mrYes:
        ModalResult := mrOK;
      mrCancel:
        Result := False;
    end;
end;

constructor TMemoEditorForm.Create(AOwner: TComponent);
{$IFDEF CLR}
var
  Cookie: LockCookie;
{$ENDIF}
begin
{$IFDEF CLR}
  Cookie := GlobalNameSpace.UpgradeToWriterLock(MaxInt);
  try
    inherited CreateNew(AOwner);
  finally
    GlobalNameSpace.DowngradeFromWriterLock(Cookie);
  end;
{$ELSE}
  GlobalNameSpace.BeginWrite;
  try
    CreateNew(AOwner);
  finally
    GlobalNameSpace.EndWrite;
  end;
{$ENDIF}

  Visible := False;
  Position := poScreenCenter;
  Width := 530;
  Height := 308;
  BorderStyle := bsSizeable;
  BorderIcons := [biSystemMenu];
  ParentFont := True;
  Constraints.MinHeight := 100;
  Constraints.MinWidth := 260;

  FMemo := TMemo.Create(Self);
  InsertControl(FMemo);
  FMemo.Top := 0;
  FMemo.Height := 270;
  FMemo.Align := alTop;
  FMemo.Anchors := FMemo.Anchors + [akBottom];
  FMemo.ScrollBars := ssBoth;
  FMemo.OnKeyDown := MemoKeyDown;

  FCheckBox := TCheckBox.Create(Self);
  InsertControl(FCheckBox);
  FCheckBox.Caption := sWordWrap;
  FCheckBox.Left := 8;
  FCheckBox.Top := 282;
  FCheckBox.Anchors := [akLeft, akBottom];
  FCheckBox.OnClick := CheckBoxClick;


  FCancelBtn := TButton.Create(Self);
  InsertControl(FCancelBtn);
  FCancelBtn.Top := 278;
  FCancelBtn.Left := Width - FCancelBtn.Width - 4;
  FCancelBtn.Anchors := [akRight, akBottom];
  FCancelBtn.Caption := SCancel;
  FCancelBtn.Cancel := True;
  FCancelBtn.ModalResult := mrCancel;

  FOKBtn := TButton.Create(Self);
  InsertControl(FOKBtn);
  FOKBtn.Top := FCancelBtn.Top;
  FOKBtn.Left := FCancelBtn.Left - FOKBtn.Width - 4;
  FOKBtn.Anchors := [akRight, akBottom];
  FOKBtn.Caption := SOK;
  FOKBtn.ModalResult := mrOK;
  FOKBtn.TabOrder := 1;
end;

procedure TMemoEditorForm.MemoKeyDown(Sender: TObject; var Key: word;
  Shift: TShiftState);
begin
  case key of
    VK_RETURN:
      if Shift = [ssCtrl] then
        ModalResult := mrOK;
    VK_ESCAPE:
      ModalResult := mrCancel;
  end;
end;

procedure TMemoEditorForm.CheckBoxClick(Sender: tobject);
begin
  FMemo.WordWrap := FCheckBox.Checked;
end;

procedure TMemoEditorForm.SetReadOnly(const Value: boolean);
begin
  if FReadOnly <> Value then begin
    FReadOnly := Value;
    FOKBtn.Visible := not Value;
    FMemo.ReadOnly := Value;
    if Value then
      FCancelBtn.Caption := SClose
    else
      FCancelBtn.Caption := SCancel;
  end;
end;

{$IFDEF VER6P}
{ TCRGridDataLink }

procedure TCRGridDataLink.DataSetChanged;
begin
  inherited;

  if FDataSetChanging or (DataSet.State <> dsBrowse) then
    Exit;

  FDataSetChanging := True;
  try
    TCRDBGrid(Grid).DataChanged;
  finally
    FDataSetChanging := False;
  end;
end;
{$ENDIF}

initialization
  bmpSortAsc := TBitmap.Create;
  bmpSortAsc.Handle := LoadBitmap(hInstance, 'SORTASC');
  bmpSortAsc.Transparent := True;

  bmpSortDesc := TBitmap.Create;
  bmpSortDesc.Handle := LoadBitmap(hInstance, 'SORTDESC');
  bmpSortDesc.Transparent := True;

  bmpFilter := TBitmap.Create;
  bmpFilter.Handle := LoadBitmap(hInstance, 'FILTER');
  bmpFilter.Transparent := True;

  bmpSearch := TBitmap.Create;
  bmpSearch.Handle := LoadBitmap(hInstance, 'SEARCH');
  bmpSearch.Transparent := True;

  bmpMenu := TBitmap.Create;
  bmpMenu.Handle := LoadBitmap(hInstance, 'MENU');
  bmpMenu.Transparent := True;

  bmpActiveFilter := TBitmap.Create;
  bmpActiveFilter.Handle := LoadBitmap(hInstance, 'ACTIVE_FILTER');
  bmpActiveFilter.Transparent := True;

  bmpEditMode := TBitmap.Create;
  bmpEditMode.Handle := LoadBitmap(hInstance, 'EDIT');
  bmpEditMode.Transparent := True;

finalization
  bmpSortDesc.Free;
  bmpSortAsc.Free;
  bmpFilter.Free;
  bmpSearch.Free;
  bmpMenu.Free;
  bmpActiveFilter.Free;
  bmpEditMode.Free;
end.
