//////////////////////////////////////////////////
//  SQL Server Data Access Components
//  Copyright © 1998-2012 Devart. All right reserved.
//  MSAccess
//////////////////////////////////////////////////

{$IFNDEF CLR}

{$I Sdac.inc}

unit MSLoader;
{$ENDIF}
interface

uses
{$IFDEF CLR}
  System.Runtime.InteropServices, Variants,
{$ELSE}
  CLRClasses,
{$ENDIF}
  Windows, Classes, DB, {$IFDEF FPC}MemDataSet{$ELSE}MemDS{$ENDIF}, 
  CRAccess, MemData, DALoader, OLEDBAccess, MSAccess;

type
  TMSColumn = class(TDAColumn)
  private
    FSize: integer;
    FPrecision: integer;
    FScale: integer;
    FIsWide: boolean;

    function GetSize: integer;
    procedure SetSize(Value: integer);

  protected
    procedure SetFieldType(Value: TFieldType); override;

  published
    property Size: integer read GetSize write SetSize;
    property Precision: integer read FPrecision write FPrecision default 0;
    property Scale: integer read FScale write FScale default 0;
  end;

  TMSLoader = class;

  TMSPutDataEvent = procedure (Sender: TMSLoader) of object;
  TMSGetColumnDataEvent = procedure (Sender: TObject; Column: TMSColumn;
    Row: integer; var Value: variant; var IsEOF: boolean) of object;

  TMSLoaderOptions = class(TPersistent)
  private
    FRowsPerBatch: integer;
    FKilobytesPerBatch: integer;
    FLockTable: boolean;
    FCheckConstraints: boolean;
    FOwner: TMSLoader;

    procedure SetRowsPerBatch(Value: integer);
    procedure SetKilobytesPerBatch(Value: integer);
    procedure SetLockTable(Value: boolean);
    procedure SetCheckConstraints(Value: boolean);

  protected
    procedure AssignTo(Dest: TPersistent); override;
  public
    constructor Create(Owner: TMSLoader);
  published
    property RowsPerBatch: integer read FRowsPerBatch write SetRowsPerBatch default 0;
    property KilobytesPerBatch: integer read FKilobytesPerBatch write SetKilobytesPerBatch default 0;
    property LockTable: boolean read FLockTable write SetLockTable default False;
    property CheckConstraints: boolean read FCheckConstraints write SetCheckConstraints default False;
  end;

{$IFDEF VER16P}
  [ComponentPlatformsAttribute(pidWin32 or pidWin64)]
{$ENDIF}
  TMSLoader = class(TDALoader)
  private
    FOnPutData: TMSPutDataEvent;
    FOnGetColumnData: TMSGetColumnDataEvent;
    FKeepIdentity: boolean;
    FKeepNulls: boolean;
    FOptions: TMSLoaderOptions;

    function GetConnection: TMSConnection;
    procedure SetConnection(Value: TMSConnection);
    procedure SetOnPutData(const Value: TMSPutDataEvent);
    procedure SetOnGetColumnData(const Value: TMSGetColumnDataEvent);
    procedure SetKeepIdentity(Value: boolean);
    procedure SetKeepNulls(Value: boolean);
    procedure SetOptions(Value: TMSLoaderOptions);
    procedure DAPutDataEvent(Sender: TDALoader);
    procedure DAGetColumnDataEvent(Sender: TObject; Column: TDAColumn;
      Row: integer; var Value: variant; var IsEOF: boolean);

  protected
    // CLR cross-assembly
    function ILoader: TCRLoader;

    function GetInternalLoaderClass: TCRLoaderClass; override;
    procedure SetInternalLoader(Value: TCRLoader); override;
    class function GetColumnClass: TDAColumnClass; override;
    function GetDataTypesMapClass: TDataTypesMapClass; override;
    procedure ReadColumn(Column: TDAColumn; CRColumn: TCRLoaderColumn); override;
    procedure WriteColumn(Column: TDAColumn; CRColumn: TCRLoaderColumn); override;

  public
    constructor Create(Owner: TComponent); override;
    destructor Destroy; override;

  published
    property Connection: TMSConnection read GetConnection write SetConnection;
    property TableName;
    property Columns;
    property KeepIdentity: boolean read FKeepIdentity write SetKeepIdentity default False;
    property KeepNulls: boolean read FKeepNulls write SetKeepNulls default False;
    property Options: TMSLoaderOptions read FOptions write SetOptions;
    property OnPutData: TMSPutDataEvent read FOnPutData write SetOnPutData;
    property OnGetColumnData: TMSGetColumnDataEvent read FOnGetColumnData write SetOnGetColumnData;
    property OnProgress;
  end;

implementation

uses
  SysUtils,
{$IFDEF CLR}
  System.Text,
{$ENDIF}
{$IFDEF VER6P}
{$IFNDEF CLR}
  Variants,
{$ENDIF}
  FmtBcd,
{$ENDIF}
  MemUtils, DBAccess, MSConsts;

{ TMSColumn }

function TMSColumn.GetSize: integer;
begin
  if (FieldType in [ftString, ftFixedChar, ftWideString, ftBytes, ftVarBytes,
    ftBlob, ftMemo{$IFDEF VER10P}, ftWideMemo{$ENDIF}, ftVariant, ftUnknown]) then
    Result := FSize
  else
    Result := 0;
end;

procedure TMSColumn.SetFieldType(Value: TFieldType);
var
  NewValue: boolean;
begin
  NewValue := Value <> FieldType;

  inherited;

  if NewValue then
    case FieldType of
      ftGuid:
        FSize := SizeOf(TGuid);
      ftSmallint:
        FSize := SizeOf(SmallInt);
      ftInteger:
        FSize := SizeOf(Integer);
      ftWord:
        FSize := SizeOf(Word);
      ftLargeint:
        FSize := SizeOf(LargeInt);
      ftBoolean:
        FSize := SizeOf(Boolean);
      ftFloat, ftCurrency:
        FSize := SizeOf(Double);
      ftBCD:
        FSize := SizeOf(Currency);
    {$IFDEF VER6P}
      ftFMTBcd:
        FSize := SizeOf(TBcd);
    {$ENDIF}
      ftDate, ftTime, ftDateTime{$IFDEF VER6P}, ftTimeStamp{$ENDIF}:
        FSize := SizeOf(Double);
      else
        FSize := 0;
    end;
end;

procedure TMSColumn.SetSize(Value: integer);
begin
  if (FieldType in [ftString, ftFixedChar, ftWideString, ftBytes, ftVarBytes,
    ftBlob, ftMemo{$IFDEF VER10P}, ftWideMemo{$ENDIF}, ftVariant, ftUnknown]) and (Value > 0) then
    FSize := Value;
end;

{ TMSLoader }

function TMSLoader.ILoader: TCRLoader;
begin
  Result := FILoader;
end;

function TMSLoader.GetInternalLoaderClass: TCRLoaderClass;
begin
  Result := TOLEDBLoader;
end;

procedure TMSLoader.SetInternalLoader(Value: TCRLoader);
begin
  inherited;

  if FILoader <> nil then begin
    FILoader.SetProp(prKeepIdentity, FKeepIdentity);
    FILoader.SetProp(prKeepNulls, FKeepNulls);
    FILoader.SetProp(prRowsPerBatch, Options.RowsPerBatch);
    FILoader.SetProp(prKilobytesPerBatch, Options.KilobytesPerBatch);
    FILoader.SetProp(prLockTable, Options.LockTable);
    FILoader.SetProp(prCheckConstraints, Options.CheckConstraints);
  end;
end;

class function TMSLoader.GetColumnClass: TDAColumnClass;
begin
  Result := TMSColumn;
end;

function TMSLoader.GetDataTypesMapClass: TDataTypesMapClass;
begin
  Result := TMSDataTypesMap;
end;

procedure TMSLoader.ReadColumn(Column: TDAColumn; CRColumn: TCRLoaderColumn);
var
  IntCol: TOLEDBLoaderColumn;
begin
  inherited;

  IntCol := TOLEDBLoaderColumn(CRColumn);
  with TMSColumn(Column) do begin
    Size := IntCol.Size;
    Precision := IntCol.Precision;
    Scale := IntCol.Scale;
    FIsWide := IntCol.IsWide;
  end;
end;

procedure TMSLoader.WriteColumn(Column: TDAColumn; CRColumn: TCRLoaderColumn);
var
  IntCol: TOLEDBLoaderColumn;
begin
  inherited;

  IntCol := TOLEDBLoaderColumn(CRColumn);
  with TMSColumn(Column) do begin
    IntCol.Size := Size;
    IntCol.Precision := Precision;
    IntCol.Scale := Scale;
    IntCol.IsWide := FIsWide;
  end;
end;

function TMSLoader.GetConnection: TMSConnection;
begin
  Result := TMSConnection(inherited Connection);
end;

procedure TMSLoader.SetConnection(Value: TMSConnection);
begin
  inherited Connection := Value;
end;

procedure TMSLoader.DAPutDataEvent(Sender: TDALoader);
begin
  if Assigned(FOnPutData) then
    FOnPutData(TMSLoader(Sender));
end;

procedure TMSLoader.DAGetColumnDataEvent(Sender: TObject; Column: TDAColumn;
  Row: integer; var Value: variant; var IsEOF: boolean);
begin
  if Assigned(FOnGetColumnData) then
    FOnGetColumnData(Sender, TMSColumn(Column), Row, Value, IsEOF);
end;

procedure TMSLoader.SetOnPutData(const Value: TMSPutDataEvent);
begin
  FOnPutData := Value;
  if Assigned(FOnPutData) then
    inherited OnPutData := DAPutDataEvent
  else
    inherited OnPutData := nil;
end;

procedure TMSLoader.SetOnGetColumnData(const Value: TMSGetColumnDataEvent);
begin
  FOnGetColumnData := Value;
  if Assigned(Value) then
    inherited OnGetColumnData := DAGetColumnDataEvent
  else
    inherited OnGetColumnData := nil;
end;

procedure TMSLoader.SetKeepIdentity(Value: boolean);
begin
  if Value <> FKeepIdentity then begin
    FKeepIdentity := Value;
    if FILoader <> nil then
      FILoader.SetProp(prKeepIdentity, Value);
  end;
end;

procedure TMSLoader.SetKeepNulls(Value: boolean);
begin
  if Value <> FKeepNulls then begin
    FKeepNulls := Value;
    if FILoader <> nil then
      FILoader.SetProp(prKeepNulls, Value);
  end;
end;

procedure TMSLoader.SetOptions(Value: TMSLoaderOptions);
begin
  FOptions.Assign(Value);
end;

constructor TMSLoader.Create(Owner: TComponent);
begin
  inherited Create(Owner);

  FOptions := TMSLoaderOptions.Create(Self);
  FKeepIdentity := False;
  FKeepNulls := False;
end;

destructor TMSLoader.Destroy;
begin
  FOptions.Free;
  
  inherited;
end;

{ TMSLoaderOptions }

constructor TMSLoaderOptions.Create(Owner: TMSLoader);
begin
  inherited Create;

  FOwner := Owner;
  FRowsPerBatch := 0;
  FKilobytesPerBatch := 0;
  FLockTable := False;
  FCheckConstraints := False;
end;

procedure TMSLoaderOptions.AssignTo(Dest: TPersistent);
begin
  if Dest is TMSLoaderOptions then begin
    TMSLoaderOptions(Dest).RowsPerBatch := RowsPerBatch;
    TMSLoaderOptions(Dest).KilobytesPerBatch := KilobytesPerBatch; 
    TMSLoaderOptions(Dest).LockTable := LockTable;
    TMSLoaderOptions(Dest).CheckConstraints := CheckConstraints;
  end
  else
    inherited;
end;

procedure TMSLoaderOptions.SetRowsPerBatch(Value: integer);
begin
  if Value <> FRowsPerBatch then begin
    FRowsPerBatch := Value;
    if (FOwner <> nil) and (FOwner.ILoader <> nil) then
      FOwner.ILoader.SetProp(prRowsPerBatch, Value);
  end;
end;

procedure TMSLoaderOptions.SetKilobytesPerBatch(Value: integer);
begin
  if Value <> FKilobytesPerBatch then begin
    FKilobytesPerBatch := Value;
    if (FOwner <> nil) and (FOwner.ILoader <> nil) then
      FOwner.ILoader.SetProp(prKilobytesPerBatch, Value);
  end;
end;

procedure TMSLoaderOptions.SetLockTable(Value: boolean);
begin
  if Value <> FLockTable then begin
    FLockTable := Value;
    if (FOwner <> nil) and (FOwner.ILoader <> nil) then
      FOwner.ILoader.SetProp(prLockTable, Value);
  end;
end;

procedure TMSLoaderOptions.SetCheckConstraints(Value: boolean);
begin
  if Value <> FCheckConstraints then begin
    FCheckConstraints := Value;
    if (FOwner <> nil) and (FOwner.ILoader <> nil) then
      FOwner.ILoader.SetProp(prCheckConstraints, Value);
  end;
end;

end.
