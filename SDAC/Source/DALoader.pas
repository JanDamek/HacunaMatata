
//////////////////////////////////////////////////
//  DB Access Components
//  Copyright © 1998-2012 Devart. All right reserved.
//  TDALoader
//////////////////////////////////////////////////

{$IFNDEF CLR}

{$I Dac.inc}

unit DALoader;
{$ENDIF}

interface

uses
  Classes, SysUtils, DB, {$IFDEF VER6P}Variants,{$ENDIF}
  MemData, {$IFDEF FPC}MemDataSet{$ELSE}MemDS{$ENDIF},
  CRTypes, CRAccess, DBAccess;

type
  TDAColumnDataType = (ctString, ctDate, ctInteger, ctUInteger, ctFloat);

  TDAColumnClass = class of TDAColumn;

  TDAColumn = class (TCollectionItem)
  private
    FName: _string;
    FFieldType: TFieldType;

  protected
    procedure AssignTo(Dest: TPersistent); override;
    function GetDataType: TDAColumnDataType; virtual;
    procedure SetDataType(Value: TDAColumnDataType); virtual;
    procedure SetFieldType(Value: TFieldType); virtual;
    function GetDisplayName: string; override;
    property DataType: TDAColumnDataType read GetDataType write SetDataType;

  public
    constructor Create(Collection: TCollection); override;

  published
    property Name: _string read FName write FName;
    property FieldType: TFieldType read FFieldType write SetFieldType default ftString;
  end;

  TDAColumns = class (TOwnedCollection)
  private
    function GetColumn(Index: integer): TDAColumn;
    procedure SetColumn(Index: integer; Value: TDAColumn);

  public
    property Items[Index: integer]: TDAColumn read GetColumn write SetColumn; default;
  end;

  TDALoader = class;

  TDAPutDataEvent = procedure (Sender: TDALoader) of object;
  TGetColumnDataEvent = procedure (Sender: TObject; Column: TDAColumn; Row: integer;
    var Value: variant; var IsEOF: boolean) of object;
  TLoaderProgressEvent = procedure (Sender: TObject; Percent: integer) of object;

  TDALoader = class (TComponent)
  private
    FTableName: _string;

    FOnPutData: TDAPutDataEvent;
    FOnGetColumnData: TGetColumnDataEvent;

    procedure SetConnection(Value: TCustomDAConnection);
    procedure SetColumns(Value: TDAColumns);

    function IsColumnsStored: boolean;
    procedure CreateColumnsByFields(Fields: TFields);
  protected
    FILoader: TCRLoader;
    FColumns: TDAColumns;
    FConnection: TCustomDAConnection;
    FTransaction: TDATransaction;
    FAutoCommit: boolean;
    FDesignCreate: boolean;
    FOnLoaderProgress: TLoaderProgressEvent;

    function GetInternalLoaderClass: TCRLoaderClass; virtual;
    procedure SetInternalLoader(Value: TCRLoader); virtual;
    procedure CreateInternalLoader;
    procedure FreeInternalLoader;
    procedure CheckInternalLoader;

    procedure DoLoaderProgress(Percent: integer);
    procedure Loaded; override;
    procedure AssignTo(Dest: TPersistent); override;

    procedure Notification(Component: TComponent; Operation: TOperation); override;
    procedure BeginConnection; virtual;
    procedure EndConnection; virtual;
    procedure CommitData;

    procedure InternalPutData; virtual;
    procedure PutData; virtual;
    procedure SetTableName(const Value: _string); virtual;

    class function GetColumnClass: TDAColumnClass; virtual;
    function GetDataTypesMapClass: TDataTypesMapClass; virtual;

    function IsTransactionStored: boolean;
    function GetTransaction: TDATransaction; virtual;
    procedure SetTransaction(Value: TDATransaction); virtual;
    function UsedConnection: TCustomDAConnection; virtual;
    function UsedTransaction: TDATransaction; virtual;

    function  NeedRecreateColumns: boolean; virtual;
    procedure ReadColumn(Column: TDAColumn; CRColumn: TCRLoaderColumn); virtual;
    procedure WriteColumn(Column: TDAColumn; CRColumn: TCRLoaderColumn); virtual;
    procedure ReadColumns;
    procedure WriteColumns;

    property Transaction: TDATransaction read GetTransaction write SetTransaction stored IsTransactionStored;
    property AutoCommit: boolean read FAutoCommit write FAutoCommit default True;

  public
    constructor Create(Owner: TComponent); override;
    destructor Destroy; override;

    procedure PutColumnData(Col: integer; Row: integer; const Value: variant); overload; virtual;
    procedure PutColumnData(const ColName: _string; Row: integer; const Value: variant); overload;

    procedure Load; virtual;

    procedure CreateColumns;
    procedure LoadFromDataSet(DataSet: TDataSet);

    property Connection: TCustomDAConnection read FConnection write SetConnection;
    property TableName: _string read FTableName write SetTableName;
    property Columns: TDAColumns read FColumns write SetColumns stored IsColumnsStored;

    property OnPutData: TDAPutDataEvent read FOnPutData write FOnPutData;
    property OnGetColumnData: TGetColumnDataEvent read FOnGetColumnData write FOnGetColumnData;
    property OnProgress: TLoaderProgressEvent read FOnLoaderProgress write FOnLoaderProgress;

  end;

  TDALoaderUtils = class
  public
    class procedure SetDesignCreate(Obj: TDALoader; Value: boolean);
    class function GetDesignCreate(Obj: TDALoader): boolean;
    class function UsedConnection(Obj: TDALoader): TCustomDAConnection;
    class function GetTransaction(Obj: TDALoader): TDATransaction;
    class procedure SetTransaction(Obj: TDALoader; Value: TDATransaction);
    class function GetFTransaction(Obj: TDALoader): TDATransaction;
  end;

implementation

uses
  CRFunctions, DAConsts;

{ TDAColumn }

constructor TDAColumn.Create(Collection: TCollection);
begin
  inherited;

  FFieldType := ftString;
end;

procedure TDAColumn.AssignTo(Dest: TPersistent);
begin
  if Dest is TDAColumn then begin
    TDAColumn(Dest).Name := Name;
    TDAColumn(Dest).FieldType := FieldType;
  end
  else
    inherited;
end;

function TDAColumn.GetDataType: TDAColumnDataType;
begin
  case FieldType of
    ftString, ftMemo, ftFmtMemo, ftFixedChar, ftWideString, ftGuid {$IFDEF VER6P}, ftTimeStamp{$ENDIF}{$IFDEF VER10P}, ftWideMemo{$ENDIF}:
      Result := ctString;
    ftSmallint, ftInteger, ftWord, ftAutoInc:
      Result := ctInteger;
    ftLargeint:
      Result := ctUInteger;
    ftFloat, ftCurrency, ftBCD{$IFDEF VER6P}, ftFMTBcd{$ENDIF}:
      Result := ctFloat;
    ftDate, ftTime, ftDateTime:
      Result := ctDate;
    else
      Result := ctString;
  end;
end;

procedure TDAColumn.SetDataType(Value: TDAColumnDataType);
begin
  case Value of
    ctString:
      FieldType := ftString;
    ctDate:
      FieldType := ftDateTime;
    ctInteger:
      FieldType := ftInteger;
    ctUInteger:
      FieldType := ftLargeint;
    ctFloat:
      FieldType := ftFloat;
  end;
end;

procedure TDAColumn.SetFieldType(Value: TFieldType);
begin
  FFieldType := Value;
end;

function TDAColumn.GetDisplayName: string;
begin
  Result := FName;
end;

{ TDAColumns }

function TDAColumns.GetColumn(Index: integer): TDAColumn;
begin
  Result := TDAColumn(inherited Items[Index]);
end;

procedure TDAColumns.SetColumn(Index: integer; Value: TDAColumn);
begin
  Items[Index].Assign(Value);
end;

{ TDALoader }

constructor TDALoader.Create(Owner: TComponent);
begin
  inherited Create(Owner);

  FColumns := TDAColumns.Create(Self, GetColumnClass);
  FAutoCommit := True;

  FDesignCreate := csDesigning in ComponentState;
end;

destructor TDALoader.Destroy;
begin
  FColumns.Free;
  FILoader.Free;

  inherited;
end;

function TDALoader.GetInternalLoaderClass: TCRLoaderClass;
begin
  Assert(False);
  Result := TCRLoader;
end;

procedure TDALoader.SetInternalLoader(Value: TCRLoader);
begin
  if Value <> FILoader then begin
    FreeInternalLoader;

    FILoader := Value;
    if FILoader <> nil then begin
      FILoader.TableName := FTableName;
    end;
  end;
end;

procedure TDALoader.CheckInternalLoader;
begin
  if not (FILoader is GetInternalLoaderClass) then begin
    FreeInternalLoader;
    CreateInternalLoader;
  end;
end;

procedure TDALoader.CreateInternalLoader;
begin
  SetInternalLoader(GetInternalLoaderClass.Create);
end;

procedure TDALoader.FreeInternalLoader;
begin
  FILoader.Free;
  FILoader := nil;
end;

procedure TDALoader.DoLoaderProgress(Percent: integer);
begin
  if Assigned(FOnLoaderProgress) then
    FOnLoaderProgress(Self, Percent);
end;

procedure TDALoader.Loaded;
begin
  inherited;

  FDesignCreate := False;
end;

procedure TDALoader.AssignTo(Dest: TPersistent);
begin
  if Dest is TDALoader then begin
    TDALoader(Dest).Connection := Connection;
    TDALoader(Dest).Transaction := Transaction;
    TDALoader(Dest).FTableName := TableName;
    TDALoader(Dest).Columns.Assign(Columns);
    TDALoader(Dest).AutoCommit := AutoCommit;
  end
  else
    inherited;
end;

procedure TDALoader.BeginConnection;
var
  vUsedConnection: TCustomDAConnection;
  vUsedTransaction: TDATransaction;
begin
  vUsedConnection := UsedConnection;
  if vUsedConnection = nil then
    raise Exception.Create(SConnectionNotDefined);

  TDBAccessUtils.InternalConnect(vUsedConnection);
  CheckInternalLoader;
  FILoader.Connection := TDBAccessUtils.GetIConnection(vUsedConnection);

  if TDBAccessUtils.IsMultipleTransactionsSupported(vUsedConnection) then begin
    vUsedTransaction := UsedTransaction;
    if vUsedTransaction = nil then
      DatabaseError(STransactionNotAssigned);

    TDBAccessUtils.GainTransaction(vUsedTransaction);
    FILoader.Transaction := TDBAccessUtils.GetITransaction(vUsedTransaction);
  end;
end;

procedure TDALoader.EndConnection;
var
  vUsedConnection: TCustomDAConnection;
  vUsedTransaction: TDATransaction;
begin
  vUsedConnection := UsedConnection;

  if TDBAccessUtils.IsMultipleTransactionsSupported(vUsedConnection) then begin
    vUsedTransaction := UsedTransaction;
    TDBAccessUtils.ReleaseTransaction(vUsedTransaction);
  end;

  TDBAccessUtils.InternalDisconnect(vUsedConnection);
end;

procedure TDALoader.CommitData;
var
  AutoCommitUsed: boolean;
  Connection: TCustomDAConnection;
begin
  Connection := UsedConnection;
  AutoCommitUsed := TDBAccessUtils.GetAutoCommit(Connection) and AutoCommit;
  if TDBAccessUtils.IsMultipleTransactionsSupported(Connection) then
    TDBAccessUtils.AutoCommitTransaction(UsedTransaction, AutoCommitUsed);
end;

procedure TDALoader.PutColumnData(Col: integer; Row: integer; const Value: variant);
begin
  CheckInternalLoader;
  FILoader.PutColumnData(Col, Row, Value);
end;

procedure TDALoader.PutColumnData(const ColName: _string; Row: integer; const Value: variant);
var
  i: integer;
begin
  for i := 0 to FColumns.Count - 1 do
    if _SameText(ColName, FColumns[i].Name) then begin
      PutColumnData(i, Row, Value);
      Exit;
    end;
  raise Exception.Create(Format(SColumnNotFound, [ColName]));
end;

procedure TDALoader.InternalPutData;
var
  Value: variant;
  EOF: boolean;
  i,Row: integer;
begin
  if Assigned(FOnGetColumnData) then begin
    Row := 1;
    EOF := False;
    while not EOF do begin
      for i := 0 to FColumns.Count - 1 do begin
        FOnGetColumnData(Self, FColumns[i], Row, Value, EOF);
        if not EOF then
          FILoader.PutColumnData(i, Row, Value)
        else begin
          if i <> 0 then
            FILoader.DiscardRow; // to prevent insertion of incomplete row. If EOF is set to True on getting value 1..last field, all values of this record is ignored.
          break;                 // stop loading immediately after getting EOF
        end;
      end;
      if not EOF then
        Inc(Row);
    end;
  end;
end;

procedure TDALoader.PutData;
begin
  if Assigned(FOnPutData) then
    FOnPutData(Self)
  else
    InternalPutData;
end;

procedure TDALoader.Load;
begin
  BeginConnection;
  try
    if Columns.Count = 0 then
      CreateColumns
    else
      WriteColumns;
    try
      FILoader.Prepare;
      StartWait;
      PutData;
      FILoader.DoLoad;
      CommitData; // InterBase
    finally
      FILoader.Finish;
      StopWait;
    end;
  finally
    EndConnection;
  end;
end;

function TDALoader.NeedRecreateColumns: boolean;
begin
  Result := False;
end;

procedure TDALoader.ReadColumn(Column: TDAColumn; CRColumn: TCRLoaderColumn);
begin
  Column.Name := CRColumn.Name;
  Column.FieldType := GetDataTypesMapClass.GetFieldType(CRColumn.DataType);
end;

procedure TDALoader.WriteColumn(Column: TDAColumn; CRColumn: TCRLoaderColumn);
begin
  CRColumn.Name := Column.Name;
  CRColumn.DataType := GetDataTypesMapClass.GetDataType(Column.FieldType);
end;

procedure TDALoader.CreateColumns;
begin
  BeginConnection;
  try
    FILoader.CreateColumns;
    ReadColumns;
  finally
    EndConnection;
  end;
end;

procedure TDALoader.ReadColumns;
var
  i: integer;
  Col: TDAColumn;
begin
  FColumns.BeginUpdate;
  try
    FColumns.Clear;
    for i := 0 to FILoader.Columns.Count - 1 do begin
      Col := TDAColumn(FColumns.Add);
      ReadColumn(Col, FILoader.Columns[i]);
    end;
  finally
    FColumns.EndUpdate;
  end;
end;

procedure TDALoader.WriteColumns;
var
  i: integer;
  Col: TCRLoaderColumn;
  ColumnIndex: Integer;
begin
  if not NeedRecreateColumns then begin
    FILoader.Columns.Clear;
    for i := 0 to Columns.Count - 1 do begin
      Col := FILoader.GetColumnClass.Create;
      WriteColumn(Columns[i], Col);
      FILoader.Columns.Add(Col);
    end;
  end
  else begin
    // Creating CR level columns by database
    FILoader.CreateColumns;

    for i := 0 to Columns.Count - 1 do begin
      // Find column in the CR level by name from DA level
      ColumnIndex := FILoader.Columns.GetColumnIndexByName(Columns[i].Name);
      if ColumnIndex <> -1 then
        Col := FILoader.Columns.Items[ColumnIndex]
      else begin
      // If column hasn't been found in the CR level then add
        Col := FILoader.GetColumnClass.Create;
        ColumnIndex := FILoader.Columns.Add(Col);
      end;

      // If column has invalid order then move it
      if ColumnIndex <> i then
        FILoader.Columns.Move(ColumnIndex, i);

      // Write CR column from DA level
      WriteColumn(Columns[i], Col);
    end;

    // Remove all CR columns that is absent in the DA level
    for i := FILoader.Columns.Count - 1 downto Columns.Count do begin
      FILoader.Columns.Items[i].Free;
      FILoader.Columns.Delete(i);
    end;
  end;  
end;

procedure TDALoader.CreateColumnsByFields(Fields: TFields);
var
  i: word;
  Field: TField;
begin
  FColumns.Clear;
  try
    FColumns.BeginUpdate;

    for i := 0 to Fields.Count - 1 do begin
      Field := Fields[i];
      if not Field.ReadOnly then
        with TDAColumn(FColumns.Add) do begin
          Name := Field.FieldName;
          FieldType := Field.DataType;
        end;
    end;

  finally
    FColumns.EndUpdate;
  end;
end;

procedure TDALoader.Notification(Component: TComponent; Operation: TOperation);
begin
  if Operation = opRemove then begin
    if Component = FConnection then
      FConnection := nil
    else
    if Component = FTransaction then
      FTransaction := nil
  end;

  inherited;
end;

procedure TDALoader.SetConnection(Value: TCustomDAConnection);
begin
  if Value <> FConnection then begin
    if FConnection <> nil then
      RemoveFreeNotification(FConnection);

    FConnection := Value;

    if FConnection <> nil then
      FreeNotification(FConnection);
  end;
end;

function TDALoader.IsTransactionStored: boolean;
begin
  Result := FTransaction <> nil;
end;

function TDALoader.GetTransaction: TDATransaction;
begin
  Result := FTransaction;
end;

procedure TDALoader.SetTransaction(Value: TDATransaction);
begin
  if Value <> FTransaction then begin
    if FTransaction <> nil then
      RemoveFreeNotification(FTransaction);

    FTransaction := Value;

    if FTransaction <> nil then
      FreeNotification(FTransaction);
  end;
end;

procedure TDALoader.SetColumns(Value: TDAColumns);
begin
  FColumns.Assign(Value);
end;

function TDALoader.IsColumnsStored: boolean;
begin
  Result := FColumns.Count > 0;
end;

procedure TDALoader.SetTableName(const Value: _string);
begin
  if Value <> FTableName then begin
    FTableName := Value;
    if FILoader <> nil then
      FILoader.TableName := Value;
    if not (csLoading in ComponentState) and (UsedConnection <> nil) and UsedConnection.Connected and (FColumns.Count = 0) then
      CreateColumns;
  end;
end;

class function TDALoader.GetColumnClass: TDAColumnClass;
begin
  Result := TDAColumn;
end;

function TDALoader.GetDataTypesMapClass: TDataTypesMapClass;
begin
  Result := TDataTypesMap;
  Assert(False);
end;

function TDALoader.UsedConnection: TCustomDAConnection;
begin
  Result := FConnection;
end;

function TDALoader.UsedTransaction: TDATransaction;
var
  UsedCon: TCustomDAConnection;
begin
  UsedCon := UsedConnection;
  if UsedCon <> nil then begin
    if TDBAccessUtils.IsMultipleTransactionsSupported(UsedCon) then
      Result := Transaction
    else
      Result := nil;

    if Result = nil then
      Result := TDBAccessUtils.UsedTransaction(UsedCon);
  end
  else
    Result := nil;
end;

procedure TDALoader.LoadFromDataSet(DataSet: TDataSet);
var
  row, col: integer;
  ColNo: array of integer;
  OldActive: boolean;

  Field: TField;
  FieldDesc: TFieldDesc;
  RecordSet: TCRRecordSet;
  ObjRef: TSharedObject;
  IsBlank: boolean;
  AValue: variant;
  Bookmark: TBookmark;
  RecCount: integer;

  procedure FillColumsNumber;
  var
    i, j: integer;
    fname: _string;
  begin
    for i := 0 to DataSet.FieldCount - 1 do begin
      ColNo[i] := -1;
      fname := DataSet.Fields[i].FieldName;

      for j := 0 to FColumns.Count - 1 do
        if _SameText(fname, FColumns[j].Name) then begin
          ColNo[i] := j;
          break;
        end;
    end;
  end;

begin
  BeginConnection;
  try
    if DataSet = nil then
      raise Exception.Create(SDataSetNotDefined);

    OldActive := DataSet.Active;
    Bookmark := nil;
    try
      DataSet.DisableControls;
      Bookmark := DataSet.GetBookmark;

      DataSet.Open;
      DataSet.First;

      SetLength(ColNo, DataSet.FieldCount);

      if Columns.Count = 0 then begin
        CreateColumnsByFields(DataSet.Fields);

        for col := 0 to DataSet.FieldCount - 1 do
          ColNo[col] := col;
      end
      else
        FillColumsNumber;
      WriteColumns;

      FILoader.Prepare;
      StartWait;
      try
        DataSet.First;
        RecCount := DataSet.RecordCount;
        for row := 1 to RecCount do begin
          for col := 0 to DataSet.FieldCount - 1 do
            if ColNo[col] >= 0 then begin
              Field := DataSet.Fields[col];
              if DataSet is TCustomDADataSet then
                with TCustomDADataSet(DataSet) do begin
                  FieldDesc := GetFieldDesc(Field);
                  if FieldDesc <> nil then begin
                    RecordSet := TDBAccessUtils.GetIRecordSet(TCustomDADataSet(DataSet));
                    if RecordSet.IsBlobFieldType(FieldDesc.DataType) then begin
                      IsBlank := RecordSet.GetNull(FieldDesc.FieldNo, ActiveBuffer);
                      ObjRef := RecordSet.GetObject(FieldDesc.FieldNo, ActiveBuffer);
                      if IsBlank then
                        AValue := Null
                      else begin
                      {$IFDEF CLR}
                        AValue := Variant(ObjRef);
                      {$ELSE}
                        AValue := Unassigned;
                        TVarData(AValue).VType := varByRef{$IFDEF FPC} or varVariant{$ENDIF};
                        TVarData(AValue).VPointer := ObjRef;
                      {$ENDIF}
                      end;
                      FILoader.PutColumnData(ColNo[col], row, AValue);
                      Continue;
                    end;
                  end;
                end;
              // To avoid memory leak
              AValue := Unassigned;
              AValue := Field.Value;
              PutColumnData(ColNo[col], row, AValue);
            end;

          DoLoaderProgress(Round((row * 100) / RecCount));
          DataSet.Next;
        end;
        FILoader.DoLoad;
        CommitData;
      finally
        FILoader.Finish;
        StopWait;
      end;
    finally
      DataSet.Active := OldActive;
      DataSet.GotoBookmark(Bookmark);
      DataSet.FreeBookmark(Bookmark);
      DataSet.EnableControls;
    end;
  finally
    EndConnection;
  end;
end;

{ TDALoaderUtils }

class procedure TDALoaderUtils.SetDesignCreate(Obj: TDALoader; Value: boolean);
begin
  Obj.FDesignCreate := Value;
end;

class function TDALoaderUtils.GetDesignCreate(Obj: TDALoader): boolean;
begin
  Result := Obj.FDesignCreate;
end;

class function TDALoaderUtils.UsedConnection(Obj: TDALoader): TCustomDAConnection;
begin
  Result := Obj.UsedConnection;
end;

class function TDALoaderUtils.GetTransaction(Obj: TDALoader): TDATransaction;
begin
  Result := Obj.Transaction;
end;

class procedure TDALoaderUtils.SetTransaction(Obj: TDALoader; Value: TDATransaction);
begin
  Obj.Transaction := Value;
end;

class function TDALoaderUtils.GetFTransaction(Obj: TDALoader): TDATransaction;
begin
  Result := Obj.FTransaction;
end;

end.
