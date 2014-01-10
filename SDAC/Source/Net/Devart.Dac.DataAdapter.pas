unit Devart.Dac.DataAdapter;
{$I ..\Dac.inc}

interface

uses Classes, DB, System.Data, Variants;

type
  TDataTableArr = array of DataTable;
  IDataParameterArr = array of IDataParameter;

  DADataAdapter = class (TComponent)
    FDataSet: TDataSet;
    FData: System.Data.DataSet;

    procedure CheckTDataSet;

  protected
    function CanLocateField(Field: TField): boolean; virtual;
    function GetItemType(Field: TField): System.Type; virtual;
    function GetItemValue(Field: TField): TObject; virtual;
    procedure SetItemValue(Field: TField; Value: TObject); virtual;
    function ItemToVariant(Field: TField; Value: TObject): Variant; virtual;

  public
    function Fill(Data: DataSet; tableName: string): integer;
    function Update(Data: DataSet; tableName: string): integer;

    property DataSet: TDataSet read FDataSet write FDataSet;
  end;

implementation

uses MemUtils, SysUtils;

procedure DADataAdapter.CheckTDataSet;
begin
  if FDataSet = nil then
    raise Exception.Create('DADataAdapter.DataSet is not defined!');
end;

function DADataAdapter.CanLocateField(Field: TField): boolean;
begin
  case Field.DataType of
    {ftBoolean,}
    ftString, ftFixedChar, ftWideString,
    ftAutoInc, ftSmallint, ftWord, ftInteger, ftLargeint,
    ftFloat, ftCurrency,
    ftDateTime, ftDate, ftTime:
      Result := True;
  else
    case Field.DataType of
      ftBlob, ftMemo{$IFDEF VER10P}, ftWideMemo{$ENDIF}:
        Result := False;
    else
      Result := True;
    end;
  end
end;

function DADataAdapter.GetItemType(Field: TField): System.Type;
begin
  case Field.DataType of
    ftBoolean: Result := typeof(Boolean);
    ftString, ftFixedChar, ftWideString, ftMemo{$IFDEF VER10P}, ftWideMemo{$ENDIF}: Result := typeof(String);
    ftAutoInc, ftSmallint, ftWord, ftInteger, ftLargeint: Result := typeof(Longint);
    ftFloat, ftCurrency: Result := typeof(Double);
    ftDateTime, ftDate, ftTime: Result := typeof(System.DateTime);
    ftBlob: Result := typeof(TBytes);
    ftBytes, ftVarBytes: Result := typeof(TBytes);
    ftBCD: Result := typeof(Borland.Delphi.System.Currency);
  else
    Result := typeof(Variant);//ftADT, ftArray, ftReference, ftDataSet, ftVariant,
  end;
end;

function DADataAdapter.GetItemValue(Field: TField): TObject;
var
  MS: TMemoryStream;
  i: integer;
  Buf: TBytes;
begin
  if Field.IsNull then begin
    Result := Convert.DBNull;
    Exit;
  end;
  case Field.DataType of
    ftDateTime, ftDate, ftTime:
      Result := {$IFDEF VER11P}Field.AsDateTime{$ELSE}System.DateTime(Field.Value){$ENDIF};
    ftBlob: begin;
      MS := TMemoryStream.Create;
      try
        TBlobField(Field).SaveToStream(MS);
        SetLength(Buf, MS.Size);
        for i := 0 to MS.Size - 1 do begin
          Buf[i] := MS.Memory[i];
        end;
      finally
        MS.Free;
      end;
      Result := TObject(Buf);
    end;
  else
    Result := Field.Value;
  end;
end;

procedure DADataAdapter.SetItemValue(Field: TField; Value: TObject);
var
  MS: TMemoryStream;
  i: integer;
begin
  if Value = Convert.DBNull then
    Field.Clear
  else
    case Field.DataType of
      ftDateTime, ftDate, ftTime:
        Field.Value := Variant(TDateTime(System.DateTime(Value)));
      ftBlob: begin;
        MS := TMemoryStream.Create;
        try
          for i := 0 to Length(TBytes(Value)) - 1 do
            MS.Write(TBytes(Value)[i]);
          TBlobField(Field).LoadFromStream(MS);
        finally
          MS.Free;
        end;
      end;
    else
      Field.Value := Variant(Value);
    end;
end;

function DADataAdapter.ItemToVariant(Field: TField; Value: TObject): Variant;
begin
  if VarIsNull(Variant(Value)) then begin
    Result := Variant(Convert.DBNull);
    Exit;
  end;
  case Field.DataType of
    ftDateTime, ftDate, ftTime:
      Result := Variant(TDateTime(System.DateTime(Value)));
  else
    Result := Variant(Value);
  end;
end;

function DADataAdapter.Fill(Data: DataSet; tableName: string): integer;
var
  dataTable: System.Data.DataTable;
  dataRow: System.Data.DataRow;
  dataColumn: System.Data.DataColumn;
  Ind, i: integer;
  OldActive: boolean;
begin
  CheckTDataSet;
  Result := 0;
  Ind := Data.Tables.IndexOf(tableName);
  if Ind < 0 then
    dataTable := Data.Tables.Add(tableName)
  else
    dataTable := Data.Tables.Item[tableName];
  OldActive := FDataSet.Active;
  if not FDataSet.Active then
    FDataSet.Open;
  try
    for i := 0 to FDataSet.Fields.Count - 1 do begin
      if not FDataSet.Fields[i].Visible or
        (dataTable.Columns.IndexOf(FDataSet.Fields[i].FieldName) >= 0)
      then
        continue;
      dataColumn := dataTable.Columns.Add(FDataSet.Fields[i].FieldName, GetItemType(FDataSet.Fields[i]));
      dataColumn.ReadOnly := not FDataSet.Fields[i].CanModify;
    end;

    FDataSet.FindFirst;
    while not FDataSet.EOF do begin
      dataRow := dataTable.NewRow;
      for i := 0 to FDataSet.Fields.Count - 1 do
        if FDataSet.Fields[i].Visible then
          dataRow.set_Item(FDataSet.Fields[i].FieldName, GetItemValue(FDataSet.Fields[i]));
      dataTable.Rows.Add(dataRow);
      FDataSet.Next;
      Inc(Result);
    end;
    dataTable.AcceptChanges;
  finally
    FDataSet.Active := OldActive
  end;
end;

function DADataAdapter.Update(Data: DataSet; tableName: string): integer;
var
  Ind, i: integer;
  dataTable: System.Data.DataTable;
  dataRow: System.Data.DataRow;
  OldActive: boolean;

  function CompareValues(Obj1, Obj2: TObject): boolean;
  var
    V1, V2: Variant;
    i: integer;
  begin
    V1 := Variant(Obj1);
    V2 := Variant(Obj2);
    if VarIsArray(V1) and VarIsArray(V2) then begin
      if (VarArrayLowBound(V1, 1) <> VarArrayLowBound(V2, 1)) or
        (VarArrayHighBound(V1, 1) <> VarArrayHighBound(V2, 1))
      then begin
        Result := False;
        Exit;
      end;
      for i := VarArrayLowBound(V1, 1) to VarArrayHighBound(V1, 1) do begin
        if V1[i] <> V2[i] then begin
          Result := False;
          Exit;
        end;
      end;
      Result := True;
    end
    else
      Result := VarEqual(V1, V2);
  end;

  procedure EditRecord;
  var
    j: integer;
    dataColumn: System.Data.DataColumn;
    Field: TField;
  begin
    for j := 0 to dataTable.Columns.Count - 1 do begin
      dataColumn := dataTable.Columns.Item[j];
      Field := FDataSet.FieldByName(dataColumn.ColumnName);
      if
        Field.CanModify and
        ((dataRow.RowState = DataRowState.Added) or
        not CompareValues(dataRow.get_Item(dataColumn.ColumnName), dataRow.get_Item(dataColumn.ColumnName, DataRowVersion.Original)))
      then begin
        SetItemValue(Field, dataRow.get_Item(dataColumn.ColumnName));
      end;
    end;
  end;

  procedure LocateRecord;
  var
    j: integer;
    KeyFields, Keys, St: string;
    KeyValues, FinalKeyValues: Variant;
    KeyCount: integer;
    dataColumn: System.Data.DataColumn;

    procedure AddKeyValue(ColumnName: string);
    var
      Field: TField;
    begin
      Field := FDataSet.FieldByName(ColumnName);
      if CanLocateField(Field) then begin
        if KeyFields <> '' then
          KeyFields := KeyFields + ';';
        KeyFields := KeyFields + ColumnName;
        KeyValues[KeyCount] := ItemToVariant(Field, dataRow.get_Item(ColumnName, DataRowVersion.Original));
        Inc(KeyCount);
      end;
    end;

  begin
    Keys := Trim(IProviderSupport(FDataSet).PSGetKeyFields);
    KeyFields := '';
    KeyCount := 0;
    KeyValues := VarArrayCreate([0, dataTable.Columns.Count - 1], varObject);
    if Keys <> '' then begin
      j := 1;
      while True do begin
        St := ExtractFieldName(Keys, j);
        if St = '' then
          break;
        AddKeyValue(St);
      end;
    end
    else begin
      for j := 0 to dataTable.Columns.Count - 1 do begin
        dataColumn := dataTable.Columns.Item[j];
        AddKeyValue(dataColumn.ColumnName);
      end;
    end;
    if KeyCount = 1 then
      FinalKeyValues := KeyValues[0]
    else
    if KeyCount > 1 then
      FinalKeyValues := KeyValues
    else
      raise Exception.Create('Cannot locate record');
    if not FDataSet.Locate(KeyFields, FinalKeyValues, []) then
      raise Exception.Create('Cannot locate record');
  end;

begin
  Result := 0;
  Ind := Data.Tables.IndexOf(tableName);
  if Ind < 0 then
    raise Exception.Create('Has no table');
  dataTable := Data.Tables.Item[tableName];
  OldActive := FDataSet.Active;
  try
    for i := 0 to dataTable.Rows.Count - 1 do begin
      dataRow := dataTable.Rows.Item[i];
      case dataRow.RowState of
        DataRowState.Added: begin
          if not FDataSet.Active then
            FDataSet.Open;
          FDataSet.Append;
          EditRecord;
          FDataSet.Post;
          Inc(Result);
        end;
        DataRowState.Deleted: begin
          if not FDataSet.Active then
            FDataSet.Open;
          LocateRecord;
          FDataSet.Delete;
          Inc(Result);
        end;
        DataRowState.Modified: begin
          if not FDataSet.Active then
            FDataSet.Open;
          LocateRecord;
          FDataSet.Edit;
          EditRecord;
          FDataSet.Post;
          Inc(Result);
        end;
      end;
    end;
    dataTable.AcceptChanges;
  finally
    FDataSet.Active := OldActive;
  end;
end;

end.
