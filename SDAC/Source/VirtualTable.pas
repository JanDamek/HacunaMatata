//////////////////////////////////////////////////
//  Copyright © 1998-2012 Devart. All right reserved.
//  Virtual table
//  Created:            11.12.98
//////////////////////////////////////////////////

{$IFNDEF CLR}

{$I Dac.inc}

unit VirtualTable;
{$ENDIF}

interface

uses
{$IFDEF MSWINDOWS}
  Windows,
{$ENDIF}
  Classes, SysUtils, DB,
  MemData, {$IFDEF FPC}MemDataSet,{$ELSE}MemDS,{$ENDIF}
  CRTypes;

type
  TCRFileFormat = (ffVTD, ffXML);

  TVirtualTableOption = (voPersistentData, voStored);
  TVirtualTableOptions = set of TVirtualTableOption;
  TVirtualTableProgressEvent = procedure (Sender: TObject; Percent: integer) of object;

{$IFDEF VER16P}
  [ComponentPlatformsAttribute(pidWin32 or pidWin64 or pidOSX32)]
{$ENDIF}
  TVirtualTable = class(TMemDataSet)
  private
    FOptions: TVirtualTableOptions;
    FStreamedActive: boolean;
    FAvoidRefreshData: boolean;
    FAvoidReload: integer;
    FRecordDataStream: TMemoryStream;
    FIsCursorOpen: boolean;
    FOnVirtualTableProgress: TVirtualTableProgressEvent;

    procedure ReadBinaryData(Stream: TStream);
    procedure WriteBinaryData(Stream: TStream);

    function IsFieldDefsStored: boolean;
    function GetFieldDefs: TFieldDefs;
    procedure SetFieldDefs(Value: TFieldDefs);

  protected
    FFieldDefsByField: boolean;

    procedure Loaded; override;

    procedure CreateIRecordSet; override;

    procedure OpenCursor(InfoQuery: boolean); override;
    procedure InternalOpen; override;
    procedure InternalClose; override;
    function IsCursorOpen: boolean; override;

    procedure CreateFieldDefs; override;
    procedure DefChanged(Sender: TObject); {$IFNDEF FPC}override;{$ENDIF}
    procedure Reload;

  {$IFDEF CLR}
    procedure DataEvent(Event: TDataEvent; Info: TObject); override;
  {$ELSE} {$IFDEF FPC}
    procedure DataEvent(Event: TDataEvent; Info: PtrInt); override;
  {$ELSE} {$IFDEF VER16P}
    procedure DataEvent(Event: TDataEvent; Info: NativeInt); override;
  {$ELSE}
    procedure DataEvent(Event: TDataEvent; Info: Longint); override;
  {$ENDIF} {$ENDIF} {$ENDIF}

    procedure DefineProperties(Filer: TFiler); override;

    procedure AssignDataSet(Source: TDataSet);

    procedure SetActive(Value:boolean); override;

    procedure DoVirtualTableProgress(Percent: integer);

  public
    constructor Create(Owner: TComponent); override;
    destructor Destroy; override;

    function IsSequenced: boolean; override;

    procedure AddField(Name: _string; FieldType: TFieldType; Size: integer = 0; Required: boolean = False);
    procedure DeleteField(Name: _string);
    procedure DeleteFields;

    procedure Clear;

  { Stream/File }
    procedure LoadFromStream(Stream: TStream; LoadFields: boolean = True);
    procedure SaveToStream(Stream: TStream; StoreFields: boolean = True);

    procedure LoadFromFile(const FileName: string; LoadFields: boolean = True);
    procedure SaveToFile(const FileName: string; StoreFields: boolean = True);

    procedure Assign(Source: TPersistent); override;

  published
    property Options: TVirtualTableOptions read FOptions write FOptions default [voPersistentData, voStored];
    property OnProgress: TVirtualTableProgressEvent read FOnVirtualTableProgress write FOnVirtualTableProgress;

    property Active;
    property AutoCalcFields;
    property Filtered;
    property Filter;
    property FilterOptions;
    property IndexFieldNames;
    property MasterSource;
    property MasterFields;
    property DetailFields;

    property BeforeOpen;
    property AfterOpen;
    property BeforeClose;
    property AfterClose;
    property BeforeInsert;
    property AfterInsert;
    property BeforeEdit;
    property AfterEdit;
    property BeforePost;
    property AfterPost;
    property BeforeCancel;
    property AfterCancel;
    property BeforeDelete;
    property AfterDelete;
    property BeforeScroll;
    property AfterScroll;
    property OnCalcFields;
    property OnDeleteError;
    property OnEditError;
    property OnFilterRecord;
    property OnNewRecord;
    property OnPostError;

    //property Fields stored False;
    property FieldDefs: TFieldDefs read GetFieldDefs write SetFieldDefs stored IsFieldDefsStored;
  end;

var
  VTOldBehavior: boolean;

implementation

uses
{$IFDEF VER6P}
  Variants, FMTBcd,
{$ENDIF}
{$IFDEF CLR}
  System.Text, System.IO, System.XML, System.Runtime.InteropServices,
  DateUtils, RTLConsts,
{$ELSE}
  CLRClasses, CRXml,
{$ENDIF}
  CRFunctions, CRParser, MemUtils, DAConsts;

const
  // Must be sync with 'case' in AddFieldDesc and 'case' in SaveToStream
  SupportFieldTypes = [ftString, ftWideString, ftSmallint, ftInteger, ftAutoInc,
    ftWord, ftBoolean, ftLargeint, ftFloat, ftCurrency, ftDate, ftTime,
    ftDateTime, ftBlob, ftMemo{$IFDEF VER10P}, ftWideMemo{$ENDIF}, ftGuid, ftBCD, {$IFDEF VER6P}ftFmtBcd,{$ENDIF}
    ftBytes, ftVarBytes, ftVariant];
  SNotSupportFieldType = 'Field type is not supported by TVirtualTable. '#13 +
    'Valid types is String, WideString, Smallint, Integer, Word, Boolean, Largeint, Float, Currency, Date, Time, DateTime, Blob, Memo, Guid, Bcd, ' + {$IFDEF VER6P} 'FmtBcd, ' + {$ENDIF} 'Bytes, VarBytes, Variant';

type
  TVirtualData = class (TMemData)
  protected
    Owner: TDataSet;

    procedure InternalOpen(DisableInitFields: boolean = False); override;
    procedure InternalInitFields; override;

  public
    constructor Create;
    procedure Reopen; override;
  end;

{ TVirtualData }

constructor TVirtualData.Create;
begin
  inherited;

  Owner := nil;
end;

procedure TVirtualData.InternalInitFields;
  procedure AddFieldDesc(const FieldName: _string; const FieldType: TFieldType;
    const FieldSize: integer; const Precision: integer; const Scale: integer;
    const Required: boolean; const Fixed: boolean);
  var
    Field: TFieldDesc;
  begin
    Field := TFieldDesc.Create;
    try
      Field.FieldNo := FFields.Count + 1;
      Field.Name := FieldName;
      Field.DataType := TDataTypesMap.GetDataType(FieldType);

      case FieldType of
        ftString: begin
          Field.Size := FieldSize + 1;
          Field.Length := FieldSize;
        end;
        ftWideString: begin
          Field.Size := (FieldSize + 1) * sizeof(WideChar);
          Field.Length := FieldSize;
        end;
        ftSmallint:
          if (Precision <= 4) and (Precision <> 0) then begin
            Field.DataType := dtInt8;
            Field.Size := sizeof(SmallInt);
            Field.Length := Precision;
          end
          else
            Field.Size := sizeof(SmallInt);
        ftInteger, ftAutoInc:
          Field.Size := sizeof(Integer);
        ftWord:
          Field.Size := sizeof(word);
        ftBoolean:
          Field.Size := sizeof(Wordbool);
        ftLargeint:
          if (Precision <= 10) and (Precision <> 0) then begin
            Field.DataType := dtUInt32;
            Field.Size := sizeof(Integer);
            Field.Length := Precision;
          end
          else
            Field.Size := sizeof(Largeint);
        ftFloat: begin
          Field.Size := sizeof(Double);
          Field.Length := Precision;
        end;
        ftCurrency:
          Field.Size := sizeof(Double);
        ftDate, ftTime, ftDateTime: begin
          Field.Size := sizeof(TDateTime);
          Field.Length := Precision;
        end;
        ftBlob, ftMemo{$IFDEF VER10P}, ftWideMemo{$ENDIF}:
          Field.Size := sizeof(Pointer);
        ftGuid: begin
          Field.Size := FieldSize + 1;
          Field.Length := FieldSize;
        end;
        ftBCD: begin
          Field.Size := sizeof(Currency);
          Field.Scale := Scale;
          Field.Length := Precision;
        end;
      {$IFDEF VER6P}
        ftFmtBcd: begin
          if Precision < SizeOfTBcd then
            Field.Size := SizeOfTBcd
          else
            Field.Size := Precision + 1{'.'} + 1 {#0}; // To right notation of large NUMERIC values
          Field.Scale := Scale;
          Field.Length := Precision;
        end;
      {$ENDIF}
        ftBytes: begin
          Field.Size := FieldSize;
          Field.Length := FieldSize;
        end;
        ftVarbytes: begin
          Field.Size := sizeof(word) + FieldSize;
          Field.Length := FieldSize;
        end;
      {$IFDEF VER5P}
        ftVariant: begin
          Field.Size := sizeof(TVariantObject);
        end;
      {$ENDIF}

{    ftBytes, ftVarBytes, ftAutoInc, ftBlob, ftMemo, ftGraphic, ftFmtMemo,
    ftParadoxOle, ftDBaseOle, ftTypedBinary, ftCursor, ftFixedChar, ftWideString,
    ftLargeint, ftADT, ftArray, ftReference, ftDataSet, ftOraBlob, ftOraClob,
    ftVariant, ftInterface, ftIDispatch, ftGuid, ftTimeStamp, ftFMTBcd, ftVariant
 }

          else
        DatabaseError(SNotSupportFieldType);
      end;
      Field.Required := Required;
      Field.Fixed := Fixed;
      FFields.Add(Field);
    except
      Field.Free;
    end;
  end;

var
  i: integer;
  OwnerField: TField;
  OwnerFieldDef: TFieldDef;
  DataFieldCount: integer;
begin
  inherited;

  DataFieldCount := 0;
  for i := 0 to Owner.FieldCount - 1 do
    if Owner.Fields[i].FieldKind = fkData then
      Inc(DataFieldCount);

  if not Owner.DefaultFields and ((DataFieldCount > Owner.FieldDefs.Count) or (TVirtualTable(Owner).FFieldDefsByField and not VTOldBehavior)) then
  // From fields
    for i := 0 to Owner.FieldCount - 1 do begin
      if Owner.Fields[i].FieldKind = fkData then begin
        OwnerField := Owner.Fields[i];
        AddFieldDesc(OwnerField.FieldName, OwnerField.DataType, OwnerField.Size, 0, 0, False, False);
      end
    end
  else
  // From FieldDefs
    for i := 0 to Owner.FieldDefs.Count - 1 do begin
      OwnerFieldDef := Owner.FieldDefs[i];

      AddFieldDesc(OwnerFieldDef.Name, OwnerFieldDef.DataType, OwnerFieldDef.Size, OwnerFieldDef.Precision,
        OwnerFieldDef.Size, faRequired in OwnerFieldDef.Attributes, faFixed in OwnerFieldDef.Attributes);
    end
end;

procedure TVirtualData.InternalOpen(DisableInitFields: boolean = False);
begin
  if not DisableInitFields then
    InitFields;

  inherited;
end;

procedure TVirtualData.Reopen;
begin
end;

{ TVirtualTable }

constructor TVirtualTable.Create(Owner: TComponent);
begin
  inherited;

  CreateIRecordSet;
  FOptions := [voPersistentData,voStored];
  FStreamedActive := False;
  FRecordDataStream := TMemoryStream.Create;
end;

destructor TVirtualTable.Destroy;
begin
  Data.Close; // Clear data
  FRecordDataStream.Free;

  inherited;
end;

procedure TVirtualTable.Loaded;
begin
  inherited;

  try
    try
      FRecordDataStream.Seek(0, soFromBeginning);
      if FRecordDataStream.Size > 0 then
        LoadFromStream(FRecordDataStream, False);
    finally
      FRecordDataStream.Clear;
    end;
    if FStreamedActive then
      Active := True;
  except
    if csDesigning in ComponentState then
      InternalHandleException
    else
      raise;
  end;
end;

procedure TVirtualTable.CreateIRecordSet;
begin
  FCreateCalcFieldDescs := False;

  SetIRecordSet(TVirtualData.Create);
  TVirtualData(Data).Owner := Self;
end;

procedure TVirtualTable.OpenCursor(InfoQuery: boolean);
begin
  Inc(FAvoidReload);
  try
    inherited;
  finally
    Dec(FAvoidReload);
  end;
end;

procedure TVirtualTable.InternalOpen;
begin
  if FAvoidReload = 0 then
    FFieldDefsByField := False;

  inherited;

  FIsCursorOpen := True;
end;

procedure TVirtualTable.InternalClose;
begin
  Inc(FAvoidReload);
  try
    BindFields(False);
    if DefaultFields then
      DestroyFields;

    if not (voPersistentData in FOptions) then
      Data.Close
    else
      Data.SetToBegin;

    FIsCursorOpen := False;
  finally
    Dec(FAvoidReload);
  end;
end;

function TVirtualTable.IsCursorOpen: boolean;
begin
  Result := FIsCursorOpen;
end;

procedure TVirtualTable.CreateFieldDefs;
var
  DataFieldCount: integer;
  OldFieldDefsCount: integer;
  i: integer;
begin
  OldFieldDefsCount := FieldDefs.Count;
  DataFieldCount := 0;
  for i := 0 to FieldCount - 1 do
    if Fields[i].FieldKind = fkData then
      Inc(DataFieldCount);

  if not DefaultFields and
    ((DataFieldCount > FieldDefs.Count) or FFieldDefsByField)then
    try
      // Used to prevent save/load table DefChanged
      Inc(FAvoidReload);
      inherited;
      if FFieldDefsByField then
        FFieldDefsByField := (DataFieldCount = FieldDefs.Count)
      else
        FFieldDefsByField := (OldFieldDefsCount = 0) and (FieldDefs.Count > 0);
    finally
      Dec(FAvoidReload);
    end;
end;

procedure TVirtualTable.Reload;
var
  OldActive: boolean;
  Stream: TMemoryStream;
begin
  if Data.RecordCount > 0 then begin
    OldActive := Active;
    Stream := TMemoryStream.Create;
    DisableControls;
    try
      SaveToStream(Stream, False);
      Close;
      Clear;
    finally
      LoadFromStream(Stream, False);
      Active := OldActive;
      Stream.Free;
      EnableControls;
    end;
  end
  else begin
    OldActive := Active;
    Close;
    Clear;
    Active := OldActive;
  end;
end;

procedure TVirtualTable.DefChanged(Sender: TObject);
var
  FieldDef: TFieldDef;
  i: integer;
begin
  if not FAvoidRefreshData then begin
    if Active then
      FFieldDefsByField := False;
    for i := 0 to TFieldDefs(Sender).Count - 1 do begin
      FieldDef := TFieldDefs(Sender)[i];
      if FieldDef.DataType = ftUnknown then begin
        FAvoidRefreshData := True;
        FieldDef.DataType := ftString;
        FieldDef.Size := 20;
        FAvoidRefreshData := False;
      end;
    end;

    if FAvoidReload = 0 then
      Reload;
  end;
end;

{$IFDEF CLR}
procedure TVirtualTable.DataEvent(Event: TDataEvent; Info: TObject);
{$ELSE} {$IFDEF FPC}
procedure TVirtualTable.DataEvent(Event: TDataEvent; Info: PtrInt);
{$ELSE} {$IFDEF VER16P}
procedure TVirtualTable.DataEvent(Event: TDataEvent; Info: NativeInt);
{$ELSE}
procedure TVirtualTable.DataEvent(Event: TDataEvent; Info: Longint);
{$ENDIF} {$ENDIF} {$ENDIF}
begin
  if FFieldDefsByField and (Event = deFieldListChange) and (FAvoidReload = 0) and not VTOldBehavior then begin
    Inc(FAvoidReload);
    try
      FieldDefs.Updated := False;
      if Data.Active {and (voPersistentData in FOptions)} then
        Reload;
    finally
      Dec(FAvoidReload);
    end;
  end;
  inherited DataEvent(Event, Info);
end;

procedure TVirtualTable.Assign(Source: TPersistent);
var
  Stream: TMemoryStream;
begin
  if Source is TVirtualTable then begin
    Stream := TMemoryStream.Create;
    try
      TVirtualTable(Source).SaveToStream(Stream);
      LoadFromStream(Stream);
    finally
      Stream.Free;
    end;
    FFieldDefsByField := TVirtualTable(Source).FFieldDefsByField;
  end
  else
  if Source is TDataSet then
    AssignDataSet(TDataSet(Source))
  else
    inherited;
end;

procedure TVirtualTable.AssignDataSet(Source: TDataSet);

  procedure InternalCreateFieldDefs(Fields: TFields; FieldDefs: TFieldDefs);
  var
    I: Integer;
    F: TField;
    FieldDef: TFieldDef;
    SourceFieldDef: TFieldDef;
    NewDataType: TFieldType;
    FieldDefIndex: integer;
  begin
    FieldDefs.BeginUpdate;
    try
      for I := 0 to Fields.Count - 1 do begin
        F := Fields[I];

        FieldDefIndex := FieldDefs.IndexOf(F.FieldName);
        if FieldDefIndex <> -1 then
          SourceFieldDef := Source.FieldDefs[FieldDefIndex]
        else
          SourceFieldDef := nil; // For non fkData fields, create FieldDef without the [faFixed] attribute

        with F do begin
          case DataType of
            ftOraBlob: NewDataType := ftBlob;
            ftOraClob: NewDataType := ftMemo;
          else
            NewDataType := DataType;
          end;
          if NewDataType in SupportFieldTypes then begin
            FieldDef := FieldDefs.AddFieldDef;
            FieldDef.Name := FieldName;
            FieldDef.DataType := NewDataType;
            FieldDef.Size := Size;
            if Required then
              FieldDef.Attributes := [faRequired];
            if ReadOnly then
              FieldDef.Attributes := FieldDef.Attributes + [faReadonly];

            if SourceFieldDef <> nil then // for non fkData fields the [faFixed] attribute is not set
              if faFixed in SourceFieldDef.Attributes then
                FieldDef.Attributes := FieldDef.Attributes + [faFixed];

            if (DataType = ftBCD) and (F is TBCDField) then
              FieldDef.Precision := TBCDField(F).Precision;

          {$IFDEF FPC}
            if (DataType = ftLargeint) and (F is TLargeIntField) then
              FieldDef.Precision := 0;
          {$ENDIF}

          {$IFNDEF FPC}
            if F is TObjectField then
              InternalCreateFieldDefs(TObjectField(F).Fields, FieldDef.ChildDefs);
          {$ENDIF}
          end;
        end;
      end;

    finally
      FieldDefs.EndUpdate;
    end;
  end;

var
  OldActive: boolean;
  Bookmark: {$IFDEF VER12P}TBytes{$ELSE}string{$ENDIF};
  i: integer;
  SourceField: TField;

  FieldsRO: array of boolean;
  Value: variant;
  FieldsMap: array of TField;
begin
  OldActive := Active;
  Close;
  Clear;
  DeleteFields;

  InternalCreateFieldDefs(Source.Fields, FieldDefs);

  if Source.Active then begin
    DisableControls;
    Source.DisableControls;
    Bookmark := Source.Bookmark;
    Source.First;

    Open;

    // Temporary clear Field.ReadOnly flag
    SetLength(FieldsRO, Fields.Count);
    for i := 0 to Fields.Count - 1 do begin
      FieldsRO[i] := Fields[i].ReadOnly;
      Fields[i].ReadOnly := False;
    end;

    try
      SetLength(FieldsMap, Fields.Count);
      for i := 0 to Fields.Count - 1 do
        FieldsMap[i] := Source.FieldByName(Fields[i].FieldName);

      while not Source.EOF do begin
        Append;
        for i := 0 to Fields.Count - 1 do begin
          SourceField := FieldsMap[i];
          if not SourceField.IsNull then
            if Fields[i] is TLargeIntField then
              TLargeIntField(Fields[i]).AsLargeInt := TLargeIntField(SourceField).AsLargeInt
          {$IFDEF FPC}
            else
            if Fields[i] is TTimeField then
              TTimeField(Fields[i]).AsString := TTimeField(SourceField).AsString
            else
            if Fields[i] is TBinaryField then
              Fields[i].AsString := SourceField.AsString
          {$ENDIF}
            else begin
              // To avoid memory leaks
              Value := Unassigned;
              Value := SourceField.Value;
              Fields[i].Value := Value;
            end;
        end;
        Post;
        Source.Next;
      end;
    finally
      First;

      // Restore Field.ReadOnly flag
      for i := 0 to Fields.Count - 1 do
        Fields[i].ReadOnly := FieldsRO[i];

      if Source.RecordCount > 0 then
        Source.Bookmark := Bookmark;

      Source.EnableControls;
      EnableControls
    end;
  end;
  Active := OldActive;
end;

procedure TVirtualTable.DefineProperties(Filer: TFiler);
begin
  inherited DefineProperties(Filer);

  Filer.DefineBinaryProperty('Data', ReadBinaryData, WriteBinaryData,
    voStored in FOptions);
end;

procedure TVirtualTable.ReadBinaryData(Stream: TStream);
begin
  if voStored in FOptions then begin
    FRecordDataStream.Clear;
    FRecordDataStream.CopyFrom(Stream, Stream.Size - Stream.Position);
  end;
end;

procedure TVirtualTable.WriteBinaryData(Stream: TStream);
begin
  if voStored in FOptions then
    SaveToStream(Stream, False);
end;

function TVirtualTable.IsSequenced: boolean;
begin
  Result := True;
end;

procedure TVirtualTable.AddField(Name: _string; FieldType: TFieldType; Size: integer; Required: boolean);
begin
  if not (FieldType in SupportFieldTypes) then
    DatabaseError(SNotSupportFieldType);

  if (FieldType = ftGuid) then
    Size := 38;
  FieldDefs.Add(Name, FieldType, Size, Required);
end;

procedure TVirtualTable.DeleteField(Name: _string);
var
  Stream: TMemoryStream;
  OldActive: boolean;
  FieldDef: TFieldDef;
begin
  FieldDef := TFieldDef(FieldDefs.Find(Name));
  if VTOldBehavior then begin
    OldActive := Active;
    Stream := TMemoryStream.Create;
    try
      SaveToStream(Stream, False);
      Close;
      Clear;

      FieldDef.Free;
      //FieldDefs.Delete(FieldDef.Index);
    finally
      LoadFromStream(Stream, False);
      Active := OldActive;
      Stream.Free;
    end;
  end
  else
    FieldDef.Free;
end;

procedure TVirtualTable.DeleteFields;
begin
  Clear;
  FieldDefs.Clear;
{$IFDEF VER3}
  while FieldCount > 0 do
    Fields[0].Free;
{$ELSE}
  Fields.Clear;
{$ENDIF}
end;

procedure TVirtualTable.Clear;
begin
  if State in [dsInsert,dsEdit] then
    Cancel;
  Data.Close;
  if Active then begin
    Data.Open;
    Resync([]);
  end;
end;

{ Stream/File }

{ Storage format:
  Version        2 // 0 = 2.00, 1 = 2.10, 2 = 5.10.1.8 (Blob storage)
-- FieldDefs
  FieldCount     2
    NameLength   2
    Name         Length(Name)
    DataType     2
    Size         2

-- Fields
  FieldCount     2              -|
    NameLength   2               |
    Name         Length(Name)    | for 1
    Kind         2               |
    DataType     2               |
    Size         2              -|

  RecordCount    4
    Size         2 (4 from Version = 2)
    Value        Size
}

type
{$IFDEF VER6P}
{$IFNDEF FPC}
  TStringListQ = class(_TStringList)
  protected
    function CompareStrings(const S1, S2: _string): Integer; override;
  end;

function TStringListQ.CompareStrings(const S1, S2: _string): Integer; // +10% to performance
begin
  if S1 > S2 then
    Result := 1
  else
  if S1 < S2 then
    Result := -1
  else
    Result := 0;
end;
{$ELSE}
  TStringListQ = _TStringList;
{$ENDIF}
{$ELSE}
  TStringListQ = _TStringList;
{$ENDIF}



procedure TVirtualTable.LoadFromStream(Stream: TStream; LoadFields: boolean);
var
  LocFieldDefs: TFieldDefs;
  FieldAliases: _TStringList;
{$IFDEF CLR}
  StrReader: StringReader;
  StreamXML: string;
{$ENDIF}
  Reader: XMLTextReader;
  Version: word;

  procedure ReadArray(var A: TBytes; Count: integer; const Offset: integer = 0);
  begin
  {$IFDEF CLR}
    if (Count <> 0) and (Stream.Read(A, Offset, Count) <> Count) then
      raise EReadError.Create(SReadError);
  {$ELSE}
    Stream.ReadBuffer(A[Offset], Count);
  {$ENDIF}
  end;

  function DetectFileFormat: TCRFileFormat;
  var
    Signature: TBytes;
    Offset: integer;
  {$IFDEF CLR}
    Bytes: TBytes;
  {$ENDIF}
  begin
    Result := ffVTD;
    Stream.Position := 0;
    SetLength(Signature, 5);
    ReadArray(Signature, 5);
    Stream.Position := 0;

    if (Signature[0] = $EF) and (Signature[1] = $BB) and (Signature[2] = $BF) then begin // UTF8 preamble
      Stream.Position := 3;
      ReadArray(Signature, 5);
      Stream.Position := 3;
    end;

    if Signature[0] <> Byte('<') then
      Exit;
    if Signature[1] = Byte('?') then
      Offset := 1
    else
      Offset := 0;
    if
      ((Signature[1 + Offset] <> Byte('x')) and (Signature[1 + Offset] <> Byte('X'))) or
      ((Signature[2 + Offset] <> Byte('m')) and (Signature[2 + Offset] <> Byte('M'))) or
      ((Signature[3 + Offset] <> Byte('l')) and (Signature[3 + Offset] <> Byte('L')))
    then
      Exit;

    Result := ffXML;
  {$IFDEF CLR}
    SetLength(Bytes, Stream.Size - Stream.Position);
    Stream.Read(Bytes, Length(Bytes));
    StreamXML := Encoding.UTF8.GetString(Bytes);
  {$ENDIF}
  end;

  procedure ReadFieldDefsVTD;
  var
    FieldCount: word;
    i: integer;
    D2: word;
    FieldName: TBytes;
    FieldType: word;
    FieldSize: word;
    FieldPrecision: integer;
    FieldDef: TFieldDef;
  begin
    Stream.Read(FieldCount, 2);
    for i := 0 to FieldCount - 1 do begin
      Stream.Read(D2, 2);
      SetLength(FieldName, D2);
      ReadArray(FieldName, D2);
      Stream.Read(FieldType, 2);
      Stream.Read(FieldSize, 2);

      if Version >= 3 then
        Stream.Read(FieldPrecision, 4);

      LocFieldDefs.Add(string(Encoding.Default.GetString(FieldName)), TFieldType(FieldType), FieldSize, False);
      FieldDef := LocFieldDefs.Items[LocFieldDefs.Count - 1];

      if TFieldType(FieldType) in [ftCurrency, ftFloat, ftInteger, ftSmallInt, ftLargeInt, ftDate, ftTime, ftDateTime] then
        FieldDef.Precision := FieldPrecision;
    end;
  end;

  procedure ReadFieldDefsXML;
    function FieldTypeFromXML(FieldType: _string; FieldDBType: _string;
      const IsLong: boolean; const FixedLength: boolean): TFieldType;
    var
      InternalType: Word;
    begin
      FieldType := _LowerCase(FieldType);
      FieldDBType := _LowerCase(FieldDBType);

      if (FieldType = 'i8') and (FieldDBType = '') {and FixedLength} then
        InternalType := dtInt64
      else
      if (FieldType = 'bin.hex') and (FieldDBType = '') then
        if IsLong then
          InternalType := dtBlob
        else
          InternalType := dtBytes
      else
      if (FieldType = 'boolean') and (FieldDBType = '') {and FixedLength} then
        InternalType := dtBoolean
      else
      if (FieldType = 'string') and (FieldDBType = 'variant') then
      {$IFDEF VER5P}
        InternalType := dtVariant
      {$ELSE}
        InternalType := dtString
      {$ENDIF}
      else
      if (FieldType = 'string') and (FieldDBType = '') then
        if IsLong then
          InternalType := {$IFDEF VER10P}dtWideMemo{$ELSE}dtMemo{$ENDIF}
        else
          InternalType := dtWideString
      else
      if (FieldType = 'string') and ((FieldDBType = 'str') or (FieldDBType = 'string')) then
        if IsLong then
          InternalType := dtMemo
        else
          InternalType := dtString
      else
      if (FieldType = 'i8') and (FieldDBType = 'currency') {and FixedLength} then
        InternalType := dtCurrency
      else
      if (FieldType = 'number') and (FieldDBType = 'currency') {and FixedLength} then
        InternalType := dtCurrency
      else
      if (FieldType = 'datetime') and (FieldDBType = 'variantdate') {and FixedLength} then
        InternalType := dtDateTime
      else
      if (FieldType = 'date') and (FieldDBType = '') {and FixedLength} then
        InternalType := dtDate
      else
      if (FieldType = 'time') and (FieldDBType = '') {and FixedLength} then
        InternalType := dtTime
      else
      if (FieldType = 'datetime') and (FieldDBType = 'timestamp') {and FixedLength} then
        InternalType := dtDateTime
      else
      if (FieldType = 'number') and (FieldDBType = 'decimal') {and FixedLength} then
        InternalType := dtCurrency
      else
      if (FieldType = 'float') and (FieldDBType = '') {and FixedLength} then
        InternalType := dtFloat
      else
      if (FieldType = 'uuid') and (FieldDBType = '') and FixedLength then
      {$IFDEF VER5P}
        InternalType := dtGuid
      {$ELSE}
        InternalType := dtString
      {$ENDIF}
      else
      if (FieldType = 'int') and (FieldDBType = '') {and FixedLength} then
        InternalType := dtInt32
      else
      if (FieldType = 'number') and (FieldDBType = 'numeric') {and FixedLength} then
      {$IFDEF VER6P}
      {$IFNDEF FPC}
        InternalType := dtFmtBCD
      {$ELSE}
        InternalType := dtBCD
      {$ENDIF}
      {$ELSE}
        InternalType := dtBCD
      {$ENDIF}
      else
      if (FieldType = 'r4') and (FieldDBType = '') {and FixedLength} then
        InternalType := dtFloat
      else
      if (FieldType = 'i2') and (FieldDBType = '') {and FixedLength} then
        InternalType := dtInt16
      else
      if (FieldType = 'i1') and (FieldDBType = '') {and FixedLength} then
        InternalType := dtInt8
      else
      if (FieldType = 'ui8') and (FieldDBType = '') {and FixedLength} then
        InternalType := dtInt64
      else
      if (FieldType = 'ui4') and (FieldDBType = '') {and FixedLength} then
        InternalType := dtUInt32
      else
      if (FieldType = 'ui2') and (FieldDBType = '') {and FixedLength} then
        InternalType := dtWord
      else
      if (FieldType = 'ui1') and (FieldDBType = '') {and FixedLength} then
        InternalType := dtWord
      else begin
        DatabaseError(SDataTypeNotSupported, Self);
        InternalType := 0; // to prevent compiler warning
      end;
      Result := GetFieldType(InternalType);
    end;
    
  var
    i: integer;
    AttrName, AttrValue: _string;
    AttributeCount: integer;
    FieldDef: TFieldDef;
    FieldName: _string;
    FieldAlias: _string;
    FieldDataType: TFieldType;
    FieldSize: integer;
    FieldPrecision: integer;
    FieldScale: integer;
    FieldRequired: boolean;
    FieldFixed: boolean;
    FieldType, FieldDBType: _string;
    FieldIsLong: boolean;
    HaveLength: boolean;
    TmpValue: _string;
  begin
    while Reader.Read do begin
      if (_UpperCase(Reader.Name) = 'S:SCHEMA') and (Reader.NodeType = ntEndElement) then
        break;
      if (_UpperCase(Reader.Name) = 'S:ATTRIBUTETYPE') and (Reader.NodeType <> ntEndElement) then begin
        AttributeCount := Reader.AttributeCount;
        FieldAlias := '';        
        for i := 0 to AttributeCount - 1 do begin
          Reader.MoveToAttribute(i);
          if _LowerCase(Reader.Name) = 'name' then
            FieldName := Reader.Value;
          if _LowerCase(Reader.Name) = 'rs:name' then begin
            FieldAlias := Reader.Value;
          (*{$IFDEF CLR}
            FieldAlias := XMLDecode(FieldAlias);
          {$ENDIF}*)
          end;
        end;
        if FieldAlias <> '' then begin
          TmpValue := FieldName;
          FieldName := FieldAlias;
          FieldAlias := TmpValue;
          FieldAliases.Add(FieldName + '=' + FieldAlias);
        end;
        while not ((_UpperCase(Reader.Name) = 'S:DATATYPE') and (Reader.NodeType <> ntEndElement)) do begin
          Reader.Read;
          if Reader.EOF then
            raise Exception.Create(SInvalidXML);
        end;
        FieldSize := 0;
        FieldPrecision := 0;
        FieldScale := 0;
        FieldType := '';
        FieldDBType := '';
        FieldIsLong := False;
        FieldRequired := False;
        FieldFixed := False;
        HaveLength := False;
        AttributeCount := Reader.AttributeCount;
        for i := 0 to AttributeCount - 1 do begin
          Reader.MoveToAttribute(i);
          AttrName := _LowerCase(Reader.Name);
          AttrValue := _LowerCase(Reader.Value);

          if AttrName = 'rs:fixedlength' then
            FieldFixed := StrToBool(AttrValue)
          else
          if AttrName = 'rs:maybenull' then
            FieldRequired := not StrToBool(AttrValue)
          else
          if AttrName = 'dt:maxlength' then begin
            FieldSize := Integer(Round(StrToFloat(AttrValue)) and $7FFFFFFF);
            HaveLength := True;
          end
          else
          if AttrName = 'rs:precision' then
            FieldPrecision := StrToInt(AttrValue)
          else
          if AttrName = 'rs:scale' then
            FieldScale := StrToInt(AttrValue)
          else
          if AttrName = 'dt:type' then
            FieldType := AttrValue
          else
          if AttrName = 'rs:dbtype' then
            FieldDBType := AttrValue
          else
          if AttrName = 'rs:long' then
            FieldIsLong := StrToBool(AttrValue);
        end;

        if (FieldType = '') and (FieldDBType = '') then
          raise Exception.Create(SInvalidXML);

        if (not HaveLength) and (FieldDBType = '') then
          FieldDBType := 'variant';
        FieldDataType := FieldTypeFromXML(FieldType, FieldDBType, FieldIsLong, FieldFixed);

        if not (FieldDataType in [ftString, ftWideString, ftVariant, ftGuid, ftBcd{$IFDEF VER6P}, ftFmtBcd{$ENDIF},
          ftBytes, ftVarBytes, ftBlob, ftMemo, ftFixedChar{$IFDEF VER6P}, ftTimeStamp{$ENDIF}{$IFDEF VER10P}, ftWideMemo{$ENDIF}]) then
          FieldSize := 0;

        if FieldIsLong then
          FieldSize := 0;

        if FieldDataType = ftGuid then
          FieldSize := 38;

        if (FieldDataType = ftBytes) and not FieldFixed then
          FieldDataType := ftVarBytes;

        LocFieldDefs.Add(FieldName, FieldDataType, FieldSize, FieldRequired);
        FieldDef := LocFieldDefs.Items[LocFieldDefs.Count - 1];

        if FieldDataType in [ftCurrency, ftFloat, ftInteger, ftSmallInt, ftLargeInt, ftDate, ftTime, ftDateTime] then
          FieldDef.Precision := FieldPrecision
        else
        if FieldDataType in [ftBCD{$IFDEF VER6P}, ftFMTBCD{$ENDIF}] then begin
          FieldDef.Precision := FieldPrecision;
          FieldDef.Size := FieldScale;
        end;

      {$IFDEF VER5P}
        if FieldFixed then
          FieldDef.Attributes := FieldDef.Attributes + [DB.faFixed];
      {$ENDIF}
      end;
    end;
  end;

  procedure SetXMLValueToField(Field: TField; const FieldValue: _string);
  var
    Year: integer;
    Month: Word;
    Day: Word;
    Hour: Word;
    Minute: Word;
    Second: Word;

    function GetNext(const Value: string; Offset: integer; Digits: Integer): string;
    var
      i: Integer;
    begin
      SetLength(Result, Digits);
      for i := 1 to Digits do begin
        if (Value[Offset + i] >= '0') and (Value[Offset + i] <= '9') then
          Result[i] := Value[Offset + i]
        else
          raise Exception.Create(SInvalidXML);
      end;
    end;

    procedure ConvertDate(const Value: string);
    var
      Offset: integer;
    begin
      Offset := 0;
      if Value[1] = '-' then
        Inc(Offset);
      Year := StrToInt(GetNext(Value, Offset, 4));
      Inc(Offset, 5);
      Month := StrToInt(GetNext(Value, Offset, 2));
      Inc(Offset, 3);
      Day := StrToInt(GetNext(Value, Offset, 2));
    end;

    procedure ConvertTime(const Value: string);
    var
      Offset: integer;
    begin
      if Length(Value) < 8 then
        raise Exception.Create(SInvalidXML);
      Offset := 0;
      Hour := StrToInt(GetNext(Value, Offset, 2));
      Inc(Offset, 3);
      Minute := StrToInt(GetNext(Value, Offset, 2));
      Inc(Offset, 3);
      Second := StrToInt(GetNext(Value, Offset, 2));
    end;

    function DecodeXMLDateTime(const XMLDateTime: string): TDateTime;
    var
      TimePosition: integer;
    begin
      TimePosition := Pos('T', XMLDateTime);
      if TimePosition > 0 then begin
        ConvertDate(Copy(XMLDateTime, 1, TimePosition -1));
        ConvertTime(Copy(XMLDateTime, TimePosition + 1, Length(XMLDateTime) - TimePosition));
      end else begin
        Hour := 0;
        Minute := 0;
        Second := 0;
        ConvertDate(XMLDateTime);
      end;
      Result := {$IFNDEF CLR}MemUtils.{$ENDIF}EncodeDateTime(Year, Month, Day, Hour, Minute, Second, 0);
    end;

    function DecodeXMLTime(const XMLTime: string): TDateTime;
    begin
      Year := 1000;
      Month := 1;
      Day := 1;
      ConvertTime(XMLTime);
      Result := {$IFNDEF CLR}MemUtils.{$ENDIF}EncodeDateTime(Year, Month, Day, Hour, Minute, Second, 0);
    end;

  var
    FieldDesc: TFieldDesc;
  {$IFDEF VER6P}
    Bcd: TBCD;
    TmpBcd: TBCD;
    FieldLength, FieldScale: integer;
  {$IFDEF VER9P}
    Delta: word;
  {$ENDIF}
  {$IFDEF FPC}
    Curr: Currency;
  {$ENDIF}
  {$ENDIF}
    Buffer: TBytes;
    i: Integer;
    IsValidChar: boolean;
    TextOffset, BuffOffset, Count: integer;
  {$IFDEF CLR}
    TextBytes: TBytes;
  {$ELSE}
    StrValue: string;
  {$ENDIF}
  begin
    FieldDesc := GetFieldDesc(Field);
    case FieldDesc.DataType of
      dtBoolean:
        Field.AsBoolean := StrToBool(FieldValue);
      dtInt8, dtInt16, dtInt32, dtInt64, dtUInt16, dtUInt32:
        Field.AsString := FieldValue;
      dtFloat:
        Field.AsFloat := StrToFloat(ChangeDecimalSeparator(FieldValue, '.', {$IFDEF VER16P}FormatSettings.{$ENDIF}DecimalSeparator));
      dtDate, dtDateTime:
        Field.AsDateTime := DecodeXMLDateTime(FieldValue);
      dtTime:
      {$IFNDEF FPC}
        Field.AsDateTime := DecodeXMLTime(FieldValue);
      {$ELSE}
        Field.AsString := FieldValue;
      {$ENDIF}
      dtCurrency, dtBcd:
        Field.AsCurrency := StrToCurr(ChangeDecimalSeparator(FieldValue, '.', {$IFDEF VER16P}FormatSettings.{$ENDIF}DecimalSeparator));
    {$IFDEF VER6P}
    {$IFNDEF FPC}
      dtFmtBCD: begin
        BCD := StrToBCD(ChangeDecimalSeparator(FieldValue, '.', {$IFDEF VER16P}FormatSettings.{$ENDIF}DecimalSeparator));
        FieldLength := FieldDesc.Length;
        FieldScale := FieldDesc.Scale;
      {$IFDEF VER9P} // Delphi 9 NormalizeBcd Bug
        Delta := FieldLength - FieldScale;
        if Delta > 34 then begin
          Delta := 34;
          FieldLength := FieldScale + Delta;
        end;
      {$ENDIF}
        NormalizeBcd(Bcd, TmpBcd, FieldLength, FieldScale);
      {$IFNDEF FPC}
        Field.AsBCD := TmpBcd;
      {$ELSE}
        BcdToCurr(TmpBcd, Curr);
        Field.AsCurrency := Curr;
      {$ENDIF}
      end;
    {$ENDIF}
    {$ENDIF}
      dtBlob, dtBytes, dtVarBytes, dtExtVarBytes: begin
      {$IFDEF CLR}
        TextBytes := Encoding.Default.GetBytes(FieldValue);
      {$ELSE}
        StrValue := FieldValue;
      {$ENDIF}
        TextOffset := 0;
        BuffOffset := 0;
        SetLength(Buffer, Length(FieldValue) div 2);
        for i := 1 to Length(FieldValue) do begin
          IsValidChar := not ((FieldValue[i] = #$D) or (FieldValue[i] = #$A) or (FieldValue[i] = #9));
          if ((not IsValidChar) or (i = Length(FieldValue))) then begin
            Count := i - TextOffset;
            if not IsValidChar then
              Dec(Count);
            if Count > 0 then begin
            {$IFNDEF CLR}
              HexToBin(PChar(PtrOffset(@StrValue[1], TextOffset)), PAnsiChar(PtrOffset(@Buffer[0], BuffOffset)), Count);
            {$ELSE}
              HexToBin(TextBytes, TextOffset, Buffer, BuffOffset, Count div 2);
            {$ENDIF}
              Inc(BuffOffset, Count div 2);
            end;
            TextOffset := i;
          end;
        end;
        if Length(Buffer) > BuffOffset then
          SetLength(Buffer, BuffOffset);
        Field.{$IFDEF VER12P}AsAnsiString{$ELSE}AsString{$ENDIF} := Encoding.Default.GetString(Buffer);
      end;
      else
        if Field is TWideStringField then
          TWideStringField(Field).Value := FieldValue
        else
          Field.AsString := FieldValue;
    end;
  end;

  procedure ProcessXMLData;
  var
    AttributeCount: integer;
    p, i, j: integer;
    Field: TField;
    FieldList: _TStringList;
    FieldIndex: integer;
    ActualName, Alias: _string;
  begin
    FieldList := TStringListQ.Create;
    try
    {$IFDEF VER6P}
      FieldList.CaseSensitive := True;
    {$ENDIF}
      FieldList.Sorted := True;
      while Reader.Read do
        if (_UpperCase(Reader.Name) = 'Z:ROW') and (Reader.NodeType <> ntEndElement) then begin
          Append;
          try
            AttributeCount := Reader.AttributeCount;
            for i := 0 to AttributeCount - 1 do begin
              Reader.MoveToAttribute(i);
              FieldIndex := FieldList.IndexOf(Reader.Name);
              if FieldIndex > -1 then
                Field := FieldList.Objects[FieldIndex] as TField
              else begin
                Field := FindField(Reader.Name);
                if Field = nil then
                  for j := 0 to FieldAliases.Count - 1 do begin
                    p := Pos('=', FieldAliases[j]);
                    if p > 0 then begin
                      ActualName := _LowerCase(Copy(FieldAliases[j], 0, p - 1));
                      Alias := Copy(FieldAliases[j], p + 1, Length(FieldAliases[j]) - p);
                      if Reader.Name = Alias then begin
                        Field := FindField(ActualName);
                        if Field <> nil then
                          break;
                      end;
                    end;
                  end;
                if Field <> nil then
                  FieldList.AddObject(Reader.Name, Field);
              end;
              if Field = nil then
                raise Exception.Create(SInvalidXML);
              SetXMLValueToField(Field, Reader.Value);
            end;
            Post;
          except
            Cancel;
            raise;
          end;
        end;
    finally
      FieldList.Free;
    end;
  end;

var
  D2: word;
  D4: cardinal;
  FieldName: TBytes;
  FieldType: word;
  FieldSize: word;
  FieldKind: word;
  RecordCount: integer;
  i, j: integer;
  OldActive: boolean;
  St: TBytes;
  WSt: TBytes;
  FieldClass: TFieldClass;
  Field: TField;
  FieldArr: Array of TField;
  Handle: IntPtr;
  {$IFNDEF VER6P}
  tmpint64: Int64;
  {$ENDIF}
  FileFormat: TCRFileFormat;
{$IFDEF FPC}
  Ts: TTimeStamp;
  Time: TDateTime;
{$ENDIF}

begin
  Inc(FAvoidReload);
  try
    OldActive := Active;
    Close;
    Clear;
    if LoadFields then begin
      LocFieldDefs := FieldDefs;
      DeleteFields;
    end
    else
      LocFieldDefs := TFieldDefs.Create(Self);

    Stream.Seek(0, soFromBeginning);
    FileFormat := DetectFileFormat;

    Stream.Seek(0, soFromBeginning);
    Stream.Read(Version, 2);  // Version

 {$IFDEF CLR}
    StrReader := nil;
 {$ENDIF}
    Reader := nil;

    FieldAliases := nil;
    try
      // FieldDefs
      FAvoidRefreshData := True;
      try
        case FileFormat of
          ffVTD:
            ReadFieldDefsVTD;
          ffXML: begin
            FieldAliases := _TStringList.Create;
          {$IFDEF CLR}
            StrReader := StringReader.Create(StreamXML);
            Reader := XMLTextReader.Create(StrReader);
          {$ELSE}
            Stream.Position := 0;
            Reader := XMLTextReader.Create(Stream);
          {$ENDIF}
            ReadFieldDefsXML;
          end;
        end;
      finally
        FAvoidRefreshData := False;
      end;

      with Stream do begin
        if (FileFormat = ffVTD) and (Version >= 1) then begin
          // Fields
          Read(D2, 2);
          for i := 0 to D2 - 1 do begin
            Read(D2, 2);
            SetLength(FieldName, D2);
            ReadArray(FieldName, D2);
            Read(FieldKind, 2);
            Read(FieldType, 2);
            Read(FieldSize, 2);

            if LoadFields then begin
              if TFieldKind(FieldKind) = fkLookup then continue;
              FieldClass := GetFieldClass(TFieldType(FieldType));

              Field := FieldClass.Create(Self.Owner);//  Self);
              try
                Field.FieldName := string(Encoding.Default.GetString(FieldName));
                Field.FieldKind := TFieldKind(FieldKind);
                case TFieldType(FieldType) of
                  ftString:
                    Field.Size := FieldSize;
                  ftWideString:
                    Field.Size := FieldSize * sizeof(WideChar);
                end;
                Field.DataSet := Self;
              except
                Field.Free;
                raise;
              end;
            end;
          end;
        end;
        if FileFormat = ffVTD then
          Read(RecordCount, 4);

        if (FileFormat = ffXML) or ((FileFormat = ffVTD) and (RecordCount > 0)) then begin
          DisableControls;
          if not (csReading in ComponentState) then
            Open
          else begin
            DoBeforeOpen;
            try
              OpenCursor(False);
              SetState(dsBrowse)
            except
              SetState(dsInactive);
              CloseCursor;
              raise;
            end;
            DoAfterOpen;
            DoAfterScroll;
          end;

          try
            case FileFormat of
              ffXML:
                ProcessXMLData;
              ffVTD: begin
                SetLength(FieldArr, LocFieldDefs.Count);
                for i := 0 to LocFieldDefs.Count - 1 do
                  FieldArr[i] := FindField(LocFieldDefs[i].Name);

                DoVirtualTableProgress(0);
                for j := 0 to RecordCount - 1 do begin
                  Append;
                  try
                    for i := 0 to LocFieldDefs.Count - 1 do begin
                      Field := FieldArr[i];

                      if Version < 2 then begin
                        Read(D2, 2);
                        D4 := D2;
                      end
                      else
                        Read(D4, 4);

                      if D4 > 0 then begin
                        if (Field <> nil) and (Field.DataType = ftWideString) then begin
                          SetLength(WSt, D4);
                          ReadArray(WSt, D4);
                          TWideStringField(Field).Value :=
                            Encoding.Unicode.{$IFDEF CLR}GetString{$ELSE}GetWideString{$ENDIF}(WSt);
                        end
                        else
                        begin
                          if (Field <> nil) and (Field.DataType = ftVarBytes) then begin
                            SetLength(St, D4 + 2);
                            D2 := D4;
                            St[0] := Lo(D2);
                            St[1] := Hi(D2);
                            ReadArray(St, D4, 2);
                          end
                          else begin
                            SetLength(St, D4);
                            ReadArray(St, D4);
                          end;

                          if Field <> nil then
                            case Field.DataType of
                              ftString, ftBlob, ftMemo:
                                Field.{$IFDEF VER12P}AsAnsiString{$ELSE}AsString{$ENDIF} := Encoding.Default.GetString(St);
                            {$IFDEF VER10P}
                              ftWideMemo:
                                TWideMemoField(Field).Value := Encoding.Unicode.{$IFDEF CLR}GetString{$ELSE}GetWideString{$ENDIF}(St);
                            {$ENDIF}
                            {$IFNDEF CLR}
                              ftVariant: begin
                                Read(D2, 2);
                                case D2 of
                                  varBoolean: begin
                                    Field.AsBoolean := WordBool(Marshal.ReadInt16(St));
                                  end;
                                  varShortInt, varByte:
                                    Field.AsInteger := Marshal.ReadByte(St);
                                  varSmallInt, varWord:
                                    Field.AsInteger := Marshal.ReadInt16(St);
                                  varInteger:
                                    Field.AsInteger := Marshal.ReadInt32(St);
                                  varLongWord, varInt64:
                                    {$IFDEF VER6P}
                                    Field.Value := Marshal.ReadInt64(St);
                                    {$ELSE}
                                    begin
                                      tmpint64 := Marshal.ReadInt64(St);
                                      if (tmpint64 <= MaxInt) and (tmpint64 >= Low(Integer)) then
                                        Field.AsInteger := tmpint64
                                      else
                                        if Field is TLargeIntField then
                                          TLargeIntField(Field).AsLargeInt := tmpint64;
                                    end;
                                    {$ENDIF}
                                  varDouble:
                                    Field.AsFloat := Double(Pointer(@St[0])^);
                                  varCurrency:
                                    Field.AsCurrency := Currency(Pointer(@St[0])^);
                                  varOleStr{$IFDEF VER12P},varUString{$ENDIF}: begin
                                    Field.Value := Encoding.Unicode.GetWideString(St);
                                  end;
                                  varString:
                                    Field.{$IFDEF VER12P}AsAnsiString{$ELSE}AsString{$ENDIF} := Encoding.Default.GetString(St)
                                  else
                                    raise Exception.Create(Format(SVarSubtypeNotSupported, [D2]));
                                end;
                              end;
                            {$ENDIF}
                            else
                              Handle := AllocGCHandle(St, True);
                              try
                              {$IFNDEF FPC}
                                Field.SetData(GetAddrOfPinnedObject(Handle));
                              {$ELSE}
                                if Field.DataType = ftTime then begin
                                  Ts.Date := DateDelta;
                                  Ts.Time := Marshal.ReadInt32(Handle);
                                  Time := MemUtils.TimeStampToDateTime(Ts);
                                  Field.SetData(@Time);
                                end
                                else
                                  Field.SetData(Handle);
                              {$ENDIF}
                              finally
                                FreeGCHandle(Handle);
                              end;
                            end;
                        end;
                      end;
                    end;
                  finally
                    Post;
                  end;
                  DoVirtualTableProgress(Round((j * 100) / RecordCount));
                end;
              end;
              else
                Assert(False);
            end;
          finally
            First;
            EnableControls;
          end;

          if not OldActive and (voPersistentData in FOptions) then
            if csReading in ComponentState then begin
              SetState(dsInactive);
              CloseCursor;
            end
            else
              Close;
        end
        else
          Active := OldActive;
      end;

    finally
      if LocFieldDefs <> FieldDefs then
        LocFieldDefs.Free;
      FieldAliases.Free;
      Reader.Free;
    {$IFDEF CLR}
      StrReader.Free;
    {$ENDIF}    
    end;
  finally
    Dec(FAvoidReload);
  end;
end;

{$IFDEF CLR}
function StrLen(S: IntPtr): integer;
begin
  Result := 0;
  while Marshal.ReadByte(S, Result) <> 0 do
    Inc(Result);
end;

function StrLenW(S: IntPtr): integer;
begin
  Result := 0;
  while Marshal.ReadInt16(S, Result * 2) <> 0 do
    Inc(Result);
end;
{$ENDIF}

procedure TVirtualTable.SaveToStream(Stream: TStream; StoreFields: boolean);
var
  D2: word;
  D4: cardinal;
  St: {$IFDEF CLR}TBytes{$ELSE}AnsiString{$ENDIF};
  i: integer;
  OldRecNo: integer;
  OldActive: boolean;
  TempFields: TFields;
  Field: TField;
  FieldDesc: TFieldDesc;
  FieldArr: array of TField;
  FieldDescArr: array of TFieldDesc;
  Buffer: TBytes;
  pBuffer: IntPtr;
  RecBuf: TRecordBuffer;
  IsNull: boolean;
  Piece: PPieceHeader;
  BufLen: cardinal;
  Blob: TBlob;
  BlobBuffer: TBytes;
  Handle: IntPtr;
  p: IntPtr;

  procedure AssignFields(Dest: TFields; Source: TFields);
  var
    Field:TField;
  begin
    Dest.Clear;
    while Source.Count > 0 do begin
      Field := Source[0];
      Source.Remove(Field);
      Dest.Add(Field);
    end;
  end;

  procedure WriteArray(const A: TBytes; Count: integer; const Offset: integer = 0);
  begin
  {$IFDEF CLR}
    if (Count <> 0) and (Stream.Write(A, Offset, Count) <> Count) then
      raise EWriteError.Create(SWriteError);
  {$ELSE}
    Stream.WriteBuffer(A[Offset], Count);
  {$ENDIF}
  end;

  function GetFieldBufLen(aFieldDesc: TFieldDesc): cardinal;
  begin
    {$IFNDEF CLR}
    if aFieldDesc.DataType = dtVariant then
      result := SizeOf(TVarData)
    else
    {$ENDIF}
      result := aFieldDesc.Size;
  end;

var
  Offset: integer;
begin
  p := nil;
  Inc(FAvoidReload);
  try
    OldActive := Active;
    with Stream do begin
      D2 := 3;
      Write(D2, 2);  // Version 0 - 2.00 1 - 2.10

    // FieldDefs
      D2 := FieldDefs.Count;
      Write(D2, 2);
      for i := 0 to FieldDefs.Count - 1 do begin
        D2 := Length(FieldDefs[i].Name);
        Write(D2, 2);
      {$IFDEF CLR}
        St := Encoding.Default.GetBytes(FieldDefs[i].Name);
        WriteArray(St, D2);
      {$ELSE}
        St := AnsiString(FieldDefs[i].Name);
        Write(PAnsiChar(St)^, Length(St));
      {$ENDIF}
        D2 := Word(FieldDefs[i].DataType);
        Write(D2, 2);
        D2 := FieldDefs[i].Size;
        Write(D2, 2);
        D4 := FieldDefs[i].Precision;
        Write(D4, 4);
      end;

    // Fields
      if DefaultFields or not StoreFields then begin
        D2 := 0;
        Write(D2, 2);
      end
      else begin
        D2 := FieldCount;
        for i := 0 to FieldCount - 1 do
          if Fields[i].FieldKind = fkLookup then Dec(D2);
        Write(D2, 2);
        for i := 0 to FieldCount - 1 do begin
          if Fields[i].FieldKind = fkLookup then continue;
          D2 := Length(Fields[i].FieldName);
          Write(D2, 2);
        {$IFDEF CLR}
          St := Encoding.Default.GetBytes(Fields[i].FieldName);
          WriteArray(St, D2);
        {$ELSE}
          St := AnsiString(Fields[i].FieldName);
          Write(PAnsiChar(St)^, Length(St));
        {$ENDIF}
          D2 := Word(Fields[i].FieldKind);  // for ver 1
          Write(D2, 2);
          D2 := Word(Fields[i].DataType);
          Write(D2, 2);
          D2 := Fields[i].Size;
          Write(D2, 2);
        end;
      end;

      if FieldDefs.Count = 0 then begin
        D4 := 0;
        Write(D4, 4);
      end
      else begin
        DisableControls;
        if Active then
          OldRecNo := RecNo
        else
          OldRecNo := -1;

        if not DefaultFields then begin
          Close;
          TempFields := TFields.Create(nil);
          AssignFields(TempFields, Fields);
          Fields.Clear;
        end
        else
          TempFields := nil;

        Open;
        First;

        BufLen := 0;
        SetLength(FieldArr, FieldDefs.Count);
        SetLength(FieldDescArr, FieldDefs.Count);
        for i := 0 to FieldDefs.Count - 1 do begin
           FieldArr[i] := FindField(FieldDefs[i].Name);
           FieldDescArr[i] := Data.FindField(FieldDefs[i].Name);
           if (FieldDescArr[i] <> nil) and (GetFieldBufLen(FieldDescArr[i]) > BufLen) then
             BufLen := GetFieldBufLen(FieldDescArr[i]);
        end;

        SetLength(Buffer, BufLen);
        Handle := AllocGCHandle(Buffer, True);
        pBuffer := GetAddrOfPinnedObject(Handle);

        DoVirtualTableProgress(0);
        try
          D4 := RecordCount;
          Write(D4, 4);
          while not EOF do begin
            for i := 0 to FieldDefs.Count - 1 do begin
              Field := FieldArr[i];
              FieldDesc := FieldDescArr[i];

              // get field desc and data from record buffer
              if FieldDesc <> nil then begin
                GetActiveRecBuf(RecBuf);
                if FieldDesc.DataType = dtVariant then
                  FillChar(pBuffer, Length(Buffer), 0);
                Data.GetField(FieldDesc.FieldNo, RecBuf, pBuffer, IsNull);
              end
              else
                IsNull := True;

              Blob := nil;
              Offset := 0;
              if (Field = nil) or (FieldDesc = nil) or IsNull then
                D4 := 0
              else begin
                // to write field data there must be Field and FieldDesc
                case FieldDesc.DataType of
                  dtString: begin
                    D4 := StrLen(PAChar(pBuffer));
                    if D4 = 0 then
                      D4 := 1;
                  end;
                  dtWideString: begin
                    D4 := StrLenW(pBuffer) * sizeof(WideChar);
                    if D4 = 0 then
                      D4 := 2;
                  end;
                  dtInt8, dtInt16, dtInt32, dtInt64, dtUInt16, dtUInt32,
                  dtBoolean, dtCurrency, dtFloat, dtGuid, dtBytes:
                    D4 := FieldDesc.Size;
                  dtDateTime, dtDate, dtTime:
                    D4 := sizeof(TDateTime);
                  dtBlob, dtMemo{$IFDEF VER10P}, dtWideMemo{$ENDIF}: begin
                    Blob := TBlob(GetGCHandleTarget(Marshal.ReadIntPtr(pBuffer)));
                    D4 := Blob.Size;
                  end;
                  {$IFNDEF CLR}
                  dtVariant: begin
                    D2 := PVarData(pBuffer).VType;
                    case PVarData(pBuffer).VType of
                      varBoolean: begin
                        D4 := 2;
                        p := IntPtr(@PVarData(pBuffer).VBoolean);
                      end;
                      varSmallInt: begin
                        D4 := 2;
                        p := PSmallInt(@PVarData(pBuffer).VSmallInt);
                      end;
                      varInteger: begin
                        D4 := 4;
                        p := PInteger(@PVarData(pBuffer).VInteger);
                      end;
                      varShortInt: begin
                        D4 := 1;
                        {$IFDEF VER6P}
                        p := PShortInt(@PVarData(pBuffer).VShortInt);
                        {$ELSE}
                        p := PShortInt(@PVarData(pBuffer).VByte);
                        {$ENDIF}
                      end;
                      varByte: begin
                        D4 := 1;
                        p := PByte(@PVarData(pBuffer).VByte);
                      end;
                      varWord: begin
                        D4 := 2;
                        {$IFDEF VER6P}
                        p := PWord(@PVarData(pBuffer).VWord);
                        {$ELSE}
                        p := PWord(@PVarData(pBuffer).VSmallInt);
                        {$ENDIF}
                      end;
                      varLongWord: begin
                        D4 := 4;
                        {$IFDEF VER6P}
                        p := PLongWord(@PVarData(pBuffer).VLongWord);
                        {$ELSE}
                        p := PLongWord(@PVarData(pBuffer).VInteger);
                        {$ENDIF}
                      end;
                      varInt64: begin
                        D4 := 8;
                        {$IFDEF VER6P}
                        p := PInt64(@PVarData(pBuffer).VInt64);
                        {$ELSE}
                        p := PInt64(@PVarData(pBuffer).VDouble);
                        {$ENDIF}
                      end;
                      varCurrency: begin
                        D4 := 8;
                        p := PCurrency(@PVarData(pBuffer).VCurrency);
                      end;
                      varDouble: begin
                        D4 := 8;
                        p := PDouble(@PVarData(pBuffer).VDouble);
                      end;
                      varOleStr: begin
                        D4 := PLongInt(PAnsiChar(PVarData(pBuffer).VOleStr) - 4)^;
                        p := PVarData(pBuffer).VOleStr;
                      end;
                      varString: begin
                        D4 := PLongInt(PAnsiChar(PVarData(pBuffer).VString) - 4)^;
                        p := PVarData(pBuffer).VString;
                      end;
                    {$IFDEF VER12P}
                      varUString: begin
                        D4 := PLongInt(PAnsiChar(PVarData(pBuffer).VUString) - 4)^;
                        p := PVarData(pBuffer).VUString;
                      end;
                    {$ENDIF}
                    else
                      raise Exception.Create(Format(SVarSubtypeNotSupported, [PVarData(pBuffer).VType]));
                    end;
                  end;
                  {$ENDIF}
                  dtVarBytes: begin
                    D4 := Marshal.ReadInt16(pBuffer);
                    Offset := 2;
                  end;
                {$IFDEF VER6P}
                {$IFNDEF FPC}
                  dtFMTBCD:
                    D4 := SizeOfTBcd;
                  dtBCD:
                    D4 := SizeOf(Double);
                {$ENDIF}
                {$ENDIF}
                else
                  Assert(False, SUnknownDataType + ' FieldDesc.DataType=' + IntToStr(Integer(FieldDesc.DataType)));
                end;
              end;
              Write(D4, 4);
              if D4 > 0 then begin
                if FieldDesc.DataType in [dtBlob, dtMemo{$IFDEF VER10P}, dtWideMemo{$ENDIF}] then begin
                  // save blob to stream
                  Piece := Blob.FirstPiece;

                  while IntPtr(Piece) <> nil do begin
                    BufLen := Piece.Used;

                    SetLength(BlobBuffer, BufLen);
                    Marshal.Copy(PtrOffset(Piece, Sizeof(TPieceHeader)), BlobBuffer, 0, BufLen);
                    WriteArray(BlobBuffer, BufLen);

                    Piece := Piece.Next;
                  end;
                end
              {$IFNDEF CLR}
                else
                if FieldDesc.DataType = dtVariant then begin
                  WriteBuffer(Integer(p^), D4);
                  WriteBuffer(PVarData(pBuffer).VType, 2);
                end
              {$ENDIF}
                else
                  WriteArray(Buffer, D4, Offset);
              end;
            end;
            DoVirtualTableProgress(round((RecNo * 100) / RecordCount));
            Next;
          end;
        finally
          FreeGCHandle(Handle);
          if TempFields <> nil then begin
            Close;
            AssignFields(Fields, TempFields);
            TempFields.Free;
          end;

          Active := OldActive;
          if OldActive and (RecordCount > 0) then
            RecNo := OldRecNo;
          EnableControls;
        end;
      end;
    end;
  finally
    Dec(FAvoidReload);
  end;
end;

{$IFNDEF D3_CB3}
function TVirtualTable.IsFieldDefsStored: boolean;
begin
  Result := FieldDefs.Count > 0;
end;
{$ENDIF}

procedure TVirtualTable.LoadFromFile(const FileName: string; LoadFields: boolean = True);
var
  Stream: TStream;
begin
  Stream := TFileStream.Create(FileName, fmOpenRead);
  try
    LoadFromStream(Stream, LoadFields);
  finally
    Stream.Free;
  end;
end;

procedure TVirtualTable.SaveToFile(const FileName: string; StoreFields: boolean = True);
var
  Stream:TStream;
begin
  Stream := TFileStream.Create(FileName, fmCreate);
  try
    SaveToStream(Stream, StoreFields);
  finally
    Stream.Free;
  end;
end;

procedure TVirtualTable.SetActive(Value: boolean);
begin
  if (csReading in ComponentState) then begin
    if not FStreamedActive then
      FStreamedActive := Value
  end
  else
    inherited;
end;

function TVirtualTable.GetFieldDefs: TFieldDefs;
begin
  Result := inherited FieldDefs;
end;

procedure TVirtualTable.SetFieldDefs(Value: TFieldDefs);
begin
  inherited FieldDefs := Value;
end;

procedure TVirtualTable.DoVirtualTableProgress(Percent: integer);
begin
  if Assigned(FOnVirtualTableProgress) then
    FOnVirtualTableProgress(Self, Percent);
end;


initialization
  VTOldBehavior := False;

end.
