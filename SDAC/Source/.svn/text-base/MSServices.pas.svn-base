
//////////////////////////////////////////////////
//  SQL Server Data Access Components
//  Copyright © 1998-2012 Devart. All right reserved.
//////////////////////////////////////////////////

{$IFNDEF CLR}
{$IFNDEF UNIDACPRO}

{$I Sdac.inc}

unit MSServices;

{$ENDIF}
{$ENDIF}

interface

uses
  SysUtils, Classes, DB, Math, ActiveX, Registry, Windows,
{$IFDEF CLR}
  System.Text, System.XML, System.Runtime.InteropServices,
{$ELSE}
  CLRClasses, CRXml,
{$ENDIF}
{$IFDEF VER6P}
  Variants,
{$ENDIF}
  CRTypes, MemData, {$IFDEF FPC}MemDataSet{$ELSE}MemDS{$ENDIF}, CRAccess,
  DBAccess, DADump,
{$IFNDEF UNIDACPRO}
  OLEDBAccess, OLEDBIntf;
{$ELSE}
  OLEDBAccessUni, OLEDBIntfUni;
{$ENDIF}

const
  prQueryIdentity      = 101;
  prCheckRowVersion    = 102;
  prLockType           = 103;
  prLockObject         = 104;
  prIdentityInsert     = 105;
  prDisableConstraints = 106;

const
  // SQL Server type
  dtMSXML = 100;

type
  _TMSLockType = (_ltUpdate, _ltExclusive);
  _TMSLockObject = (_loRow, _loTable);

  TCustomMSDataSetUpdater = class;
  TCustomMSDataSetService = class;

  TCustomMSDataTypesMap = class(TDataTypesMap)
  public
    class function GetFieldType(DataType: Word): TFieldType; override;
    class function GetDataType(FieldType: TFieldType): integer; override;
  end;

  TCustomMSSQLGenerator = class(TDASQLGenerator)
  private
    FDataSetUpdater: TCustomMSDataSetUpdater;
    FDataSetService: TCustomMSDataSetService;

  protected
    function AssembleSB(const StatementType: TStatementType): _string; override;
    procedure GenerateInsertSQL(
      const KeyAndDataFields: TKeyAndDataFields;
      const ModifiedFieldsOnly: boolean;
      const Index: integer = -1); override;
    procedure GenerateUpdateSQL(
      const KeyAndDataFields: TKeyAndDataFields;
      const ModifiedFieldsOnly: boolean;
      const Index: integer = -1); override;
    procedure GenerateRefreshSQLSelectPart(const KeyAndDataFields: TKeyAndDataFields); override;
    procedure GenerateRefreshSQL(
      const KeyAndDataFields: TKeyAndDataFields;
      const ModifiedFieldsOnly: boolean); override;
    procedure AddFieldToCondition(SB: _StringBuilder; FieldDesc: TCRFieldDesc;
      const StatementType: TStatementType;
      const ModifiedFieldsOnly: boolean;
      const Index: integer = -1); override;

    procedure GenerateConditions(SB: _StringBuilder; const StatementType: TStatementType;
      const ModifiedFieldsOnly: boolean;
      const KeyAndDataFields: TKeyAndDataFields;
      const Index: integer = -1); override;

    procedure GenerateLockSQL(
      const KeyAndDataFields: TKeyAndDataFields;
      const Index: integer = -1); override;

    function GenerateSelectFilestreamContextSQL(const FieldName: _string;
      const KeyAndDataFields: TKeyAndDataFields; Params: TDAParams): _string;
  public
    constructor Create(Owner: TDADataSetService); override;
  end;

  TCustomMSDataSetUpdater = class(TDADataSetUpdater)
  protected
    FDataSetService: TCustomMSDataSetService;

    FUseParamType: boolean;

    function GetIRecordSet: TOLEDBRecordSet;
    function UseParamType: boolean; override;
    function NeedReturnParams: boolean; override;

    procedure SetRowsAffected(Value: Integer);


    function GetIdentityFieldValue(var Value: variant): boolean; override;

    procedure CheckUpdateQuery(const StatementType: TStatementType); override;
    procedure SetUpdateQueryOptions(const StatementType: TStatementType); override;

    function SavepointAllowed: boolean; override;

  { RefreshQuick}
    function IsRefreshQuickField(FieldDesc: TFieldDesc): boolean; override;
    procedure SaveMaxRefreshQuickValue(FieldDesc: TFieldDesc; const Value: variant); override;

    procedure PrepareUpdate; override;
    function PerformRefreshRecord: boolean; override;

    function LockCompare(const Value1, Value2: variant): boolean; override;
  public
    constructor Create(AOwner: TDataSetService); override;

    function PerformSQL(const SQL: _string; const StatementTypes: TStatementTypes): boolean; override;
  end;

  TCustomMSDataSetService = class(TDADataSetService)
  protected
    FUpdater: TCustomMSDataSetUpdater;

    FQueryIdentity: boolean;
    FCheckRowVersion: boolean;
    FLockType: _TMSLockType;
    FLockObject: _TMSLockObject;

    FTimestampField: TOLEDBFieldDesc;

    procedure CreateDataSetUpdater; override;
    procedure SetDataSetUpdater(Value: TDataSetUpdater); override;
    procedure CreateSQLGenerator; override;

    function GetIConnection: TOLEDBConnection;
    function GetOLEDBProvider: TOLEDBProvider;
    function GetDatabase: _string;
    function GetIRecordSet: TOLEDBRecordSet;
    function GetCursorType: TMSCursorType;
    function GetCursorUpdate: boolean;
    function IsFullRefresh: boolean;
    function IsDMLRefresh: boolean;
    function IsUniqueRecords: boolean;

    procedure InitCursor; override;
    procedure CloseCursor; override;

    procedure SetFieldOrigin(Field: TField; FieldDesc: TCRFieldDesc); override;
    procedure DetectIdentityFields(out IdentityField, TimeStampField: TOLEDBFieldDesc);
    function DetectIdentityField: TCRFieldDesc; override;

    function DetectCanModify: boolean; override;    

    function CanUseAllKeyFields: boolean; override;
    procedure FillFieldDescs(out FieldDescs: TFieldDescArray; FillKeyFieldDescs, ForceUseAllFields: boolean);
    procedure FillKeyFieldDescs(out KeyFieldDescs: TFieldDescArray; ForceUseAllKeyFields: boolean); override;
    procedure FillDataFieldDescs(out DataFieldDescs: TFieldDescArray; ForceUseAllKeyFields: boolean); override;

    procedure FillFieldsDefaultValues; override;
    function GetRecCount: integer; override;

    procedure SetNumberRange(FieldDef: TFieldDef); override;

    procedure InitMasterParams(Params: TDAParams); override;

  { XML }
    procedure WriteFieldXMLDataType(Field: TField; FieldDesc: TFieldDesc; const FieldAlias: _string;
      XMLWriter: XMLTextWriter); override;
    procedure WriteFieldXMLAttributeType(Field: TField; FieldDesc: TFieldDesc; const FieldAlias: _string;
      XMLWriter: XMLTextWriter); override;

  public
    constructor Create(AOwner: TMemDataSet); override;

    function SetProp(Prop: integer; const Value: variant): boolean; override;
    function GetProp(Prop: integer; var Value: variant): boolean; override;

    function OpenNext: boolean; override;

    procedure GetFilestreamContext(const FieldName: _string;
      var PathName: WideString; var FSTrContext: variant);

    property TimestampField: TOLEDBFieldDesc read FTimestampField;
  end;

  TMSServerEnumerator = class (TCRServerEnumerator)
  private
    FProvider: TOLEDBProvider;
  public
    function SetProp(Prop: integer; const Value: variant): boolean; override;
    function GetProp(Prop: integer; var Value: variant): boolean; override;
  
    procedure GetServerList(List: _TStrings); override;
  end;

  TCustomMSDumpProcessor = class(TDADumpProcessor)
  protected
    FIdentityInsert: boolean;
    FDisableConstraints: boolean;

    function CreateQuery: TCustomDADataSet; override;
    procedure BackupObjects(const Query: _string); override;

  public
    constructor Create(Owner: TDADump); override;

    function SetProp(Prop: integer; const Value: variant): boolean; override;
    function GetProp(Prop: integer; var Value: variant): boolean; override;
  end;

implementation

uses
  MemUtils, CRFunctions, DAConsts, DASQLMonitor, CRParser,
{$IFDEF VER7P}
  StrUtils,
{$ENDIF}
{$IFDEF FPC}
  Sockets,
{$ENDIF}
{$IFNDEF CLR}
  {$IFNDEF BCB}
  {$NOINCLUDE winsock}
  {$HPPEMIT '#include <winsock2.h>'}
  {$ENDIF}
  WinSock,
{$ENDIF}
  {$IFNDEF UNIDACPRO}OLEDBC{$ELSE}OLEDBCUni{$ENDIF},
  {$IFNDEF UNIDACPRO}MSConsts{$ELSE}MSConstsUni{$ENDIF},
  {$IFNDEF UNIDACPRO}MSParser{$ELSE}MSParserUni{$ENDIF};

{$IFDEF WIN32}
var
  WsaData: TWSAData; // on windows winsock
{$ENDIF}

{ TCustomMSDataTypesMap }

class function TCustomMSDataTypesMap.GetFieldType(DataType: Word): TFieldType;
begin
  case DataType of
    dtMSXML:
      Result := {$IFDEF VER10P}ftWideMemo;{$ELSE}ftMemo;{$ENDIF}
  {$IFDEF VER6P}{$IFNDEF FPC}
    dtSQLTimeStamp:
      Result := ftTimeStamp;
  {$ENDIF}{$ENDIF}
  else
    Result :=inherited GetFieldType(DataType);
  end;
end;

class function TCustomMSDataTypesMap.GetDataType(FieldType: TFieldType): integer;
begin
  if FieldType = ftFmtMemo then
    Result := dtString
{$IFDEF VER6P}{$IFNDEF FPC}
  else
  if FieldType = ftTimeStamp then
    Result := dtSQLTimeStamp
{$ENDIF}{$ENDIF}
  else
    Result := inherited GetDataType(FieldType);
end;

{ TMSSQLGenerator }

constructor TCustomMSSQLGenerator.Create(Owner: TDADataSetService);
begin
  inherited;

  FDataSetService := TCustomMSDataSetService(Owner);
  FDataSetUpdater := FDataSetService.FUpdater;
end;

function TCustomMSSQLGenerator.AssembleSB(const StatementType: TStatementType): _string;
begin
  if FDataSetService.IsDMLRefresh and (StatementType = stInsert) then
    Result :=
      FHeaderSB.ToString +
      FFldSB.ToString +
      FMiddleSB.ToString +
      FFldParamSB.ToString +
      FFooterSB.ToString +
      FCondSB.ToString
  else
    Result := inherited AssembleSB(StatementType);
end;

procedure TCustomMSSQLGenerator.GenerateInsertSQL(
  const KeyAndDataFields: TKeyAndDataFields;
  const ModifiedFieldsOnly: boolean;
  const Index: integer = -1);
var
  Field: TField;
  FieldDesc, IdentityField: TCRFieldDesc;
  i: integer;
  IsFirstParam: boolean;
begin
  inherited;

  if FFldSB.Length = 0 then begin
    if not IsCompactEdition(FDataSetService.GetIConnection.ProviderPrimaryVer) then begin
      Clear;
      FHeaderSB.Append('INSERT INTO ');
      FHeaderSB.Append(SQLInfo.NormalizeName(FTableInfo.TableNameFull, FDataSet.Options.QuoteNames));
      FHeaderSB.Append(' DEFAULT VALUES');
    end
    else begin
      for i := 0 to High(KeyAndDataFields.DataFieldDescs) do begin
        FieldDesc := KeyAndDataFields.DataFieldDescs[i];
        Field := FDataSet.GetField(FieldDesc);
        Assert(Field <> nil);
        if not Field.ReadOnly then begin
          if FFldSB.Length > 0 then begin
            FFldSB.Append(', ');
            FFldParamSB.Append(', ');
          end;
          FFldSB.Append(FieldDesc.ActualNameQuoted(SQLInfo, FDataSet.Options.QuoteNames));
          FFldParamSB.Append('DEFAULT');
        end;
      end;
    end;
  end;

  if FDataSetService.GetCursorType in [ctDefaultResultSet, ctBaseTable] then begin
    { Getting Identity value }
    if FDataSetService.FQueryIdentity and not IsCompactEdition(FDataSetService.GetIConnection.DBMSPrimaryVer) then begin
      IdentityField := FDataSetService.IdentityField;
      if IdentityField <> nil then begin
        // Warning - Identity param must be last in param list, see SetIdentityParam
        // Warning - If in 'INSERT ...' statement present sql_variant value then adding 'SET ...' fails statement
        // Warning - TOLEDBFieldDesc(GetFieldDescByField(FIdentityField)).BaseColumnName cannot be used (for example see gettting identity on INSERT into View)

        FFooterSB.Append(#$D#$A'SET ');
        FDataSetUpdater.FUseParamType := True;
        AddParam(FFooterSB, IdentityField, stInsert, ptInputOutput);

        if FDataSetService.GetIConnection.DBMSPrimaryVer > 7 then
          FFooterSB.Append(' = SCOPE_IDENTITY()')
        else
          FFooterSB.Append(' = @@Identity');
      end;
    end;

    { DMLRefresh }
    if FDataSetService.IsDMLRefresh then begin
      IsFirstParam := True;
      for i := 0 to High(KeyAndDataFields.DataFieldDescs) do begin
        FieldDesc := KeyAndDataFields.DataFieldDescs[i];
        Field := FDataSet.GetField(FieldDesc);
        Assert(Field <> nil);
        if not (Field.ReadOnly or IsBlobDataType(FieldDesc.DataType)) then begin
          if not IsFirstParam then
            FFooterSB.Append(', ')
          else
            FFooterSB.Append(LineSeparator + 'SELECT ');
          IsFirstParam := False;
          FDataSetUpdater.FUseParamType := True;
          AddParam(FFooterSB, FieldDesc, stInsert, ptInputOutput);
          FFooterSB.Append(' = ' + FieldDesc.ActualNameQuoted(SQLInfo, FDataSet.Options.QuoteNames));
        end;
      end;
      if not IsFirstParam then begin
        FFooterSB.Append(' FROM ' + SQLInfo.NormalizeName(FTableInfo.TableNameFull, FDataSet.Options.QuoteNames) +
          LineSeparator + 'WHERE' + LineSeparator + '  ');
        GenerateConditions(FCondSB, stInsert, ModifiedFieldsOnly, KeyAndDataFields);
      end;
    end;
  end;
end;

procedure TCustomMSSQLGenerator.GenerateUpdateSQL(
      const KeyAndDataFields: TKeyAndDataFields;
      const ModifiedFieldsOnly: boolean;
      const Index: integer = -1);
var
  Field: TField;
  FieldDesc: TCRFieldDesc;
  i: integer;
  IsFirstParam: boolean;
  OldCondSB: _StringBuilder;
begin
  inherited;

  if FDataSetService.GetCursorType in [ctDefaultResultSet, ctBaseTable] then begin
    { DMLRefresh }
    if (FFldSB.Length > 0) and FDataSetService.IsDMLRefresh then begin
      FFooterSB.Append(LineSeparator);
      FFooterSB.Append('SELECT ');
      IsFirstParam := True;
      for i := 0 to High(KeyAndDataFields.DataFieldDescs) do begin
        FieldDesc := KeyAndDataFields.DataFieldDescs[i];
        Field := FDataSet.GetField(FieldDesc);
        if not (Field.ReadOnly or IsBlobDataType(FieldDesc.DataType)) then begin
          if not IsFirstParam then
            FFooterSB.Append(', ');
          IsFirstParam := False;
          FDataSetUpdater.FUseParamType := True;
          AddParam(FFooterSB, FieldDesc, stUpdate, ptInputOutput);
          FFooterSB.Append(' = ' + FieldDesc.ActualNameQuoted(SQLInfo, FDataSet.Options.QuoteNames));
        end;
      end;
      FFooterSB.Append(' FROM ');
      FFooterSB.Append(SQLInfo.NormalizeName(FTableInfo.TableNameFull, FDataSet.Options.QuoteNames));
      FFooterSB.Append(LineSeparator);
      FFooterSB.Append('WHERE');
      FFooterSB.Append(LineSeparator);
      FFooterSB.Append('  ');
      OldCondSB := FCondSB;
      try
        FCondSB := _StringBuilder.Create;
        try
          GenerateConditions(FCondSB, stUpdate, ModifiedFieldsOnly, KeyAndDataFields);
          FFooterSB.Append(FCondSB.ToString);
        finally
          FCondSB.Free;
        end;
      finally
        FCondSB := OldCondSB;
      end;
    end;
  end;
end;

procedure TCustomMSSQLGenerator.GenerateRefreshSQLSelectPart(const KeyAndDataFields: TKeyAndDataFields);
var
  FieldDesc: TOLEDBFieldDesc;
  UseDataFields: boolean;
  FieldArrHigh: integer;
begin
  inherited;

  if FDataSetService.FTimestampField <> nil then begin
    UseDataFields := Length(KeyAndDataFields.DataFieldDescs) + Length(KeyAndDataFields.DataFieldDescs) > 0;
    if UseDataFields then
      FieldArrHigh := Length(KeyAndDataFields.DataFieldDescs) + Length(KeyAndDataFields.DataFieldDescs) - 1
    else
      FieldArrHigh := High(KeyAndDataFields.KeyFieldDescs);

    if FieldArrHigh >= 0 then
      FFldSB.Append(', ');
    FieldDesc := FDataSetService.FTimestampField;
    FFldSB.Append(FieldDesc.ActualNameQuoted(SQLInfo, FDataSet.Options.QuoteNames));

    if not (csDesigning in FDataSet.ComponentState) then
      FFldSB.Append(' AS ' + GenerateIndexName(IntToStr(FDataSetService.GetIRecordSet.Fields.IndexOf(FieldDesc))));
  end;
end;

procedure TCustomMSSQLGenerator.GenerateRefreshSQL(
      const KeyAndDataFields: TKeyAndDataFields;
      const ModifiedFieldsOnly: boolean);

  procedure GenerateSPFullRefreshSQL;
  var
    i: integer;
    Field: TField;
    FieldDesc: TOLEDBFieldDesc;
    TableName: _string;
    TblNames: _TStringList;
  begin
    TblNames := _TStringList.Create;
    try
      // SELECT ... FROM ... WHERE ... {WITH NOLOCK}
      // Add SELECT section
      FHeaderSB.Append('SELECT ');
      for i := 0 to FDataSet.Fields.Count - 1 do begin
        Field := FDataSet.Fields[i];
        if Field.FieldKind = fkData then begin
          FieldDesc := TOLEDBFieldDesc(FDataSet.GetFieldDesc(Field));
          TableName := GenerateTableName(FieldDesc.BaseCatalogName, FieldDesc.BaseSchemaName,
            FieldDesc.BaseTableName, FDataSetService.GetDatabase);
          if TblNames.IndexOf(TableName) = - 1 then
            TblNames.Add(TableName);

          FHeaderSB.Append(TableName + '.' + FieldDesc.Name);
          if not (csDesigning in FDataSet.ComponentState) then
            FHeaderSB.Append(' AS _' + IntToStr(FDataSetService.GetIRecordSet.Fields.IndexOf(FieldDesc)));
          FHeaderSB.Append(', ');
        end;
      end;
      FHeaderSB.Length := FHeaderSB.Length - 2;

      // Add FROM section
      FHeaderSB.Append(' FROM ');
      for i := 0 to TblNames.Count - 1 do
        FHeaderSB.Append(FDataSetService.QuoteName(TblNames[i]) + ', '); // Table name without aliases
      FHeaderSB.Length := FHeaderSB.Length - 2;

      // Add WHERE section
      GenerateConditions(FCondSB, stRefresh, ModifiedFieldsOnly, (*False {Refresh does not need to testing changes applied by other users},*) KeyAndDataFields);
      if FCondSB.Length > 0 then
        FMiddleSB.Append(' WHERE ');
    finally
      TblNames.Free;
    end;
  end;

var
  IsSPProc: variant;
begin
  GetIRecordSet.GetCommand.GetProp(prIsStoredProc, IsSPProc);
  if IsSPProc and FDataSetService.IsFullRefresh then
    GenerateSPFullRefreshSQL
  else
  if FDataSetService.IsFullRefresh or (FDataSet.ReadOnly and not FDataSetService.IsUniqueRecords) then begin
    GenerateConditions(FCondSB, stRefresh, ModifiedFieldsOnly, KeyAndDataFields);
    if FCondSB.Length = 0 then
      FHeaderSB.Append(FDataSet.FinalSQL)
    else begin
      FHeaderSB.Append(_AddWhere(FDataSet.BaseSQL, FCondSB.ToString, TMSParser, False));
      FCondSB.Length := 0; // WHERE clause already added to FHeaderSB
    end;
  end
  else
    inherited;
end;

{function TCustomMSSQLGenerator.GetActualFieldName(FldDesc: TCRFieldDesc; IsRefresh: boolean): _string;
var
  TableInfo: TCRTableInfo;
  IsView: boolean;
begin
  if not (((FldDesc.TableInfo <> nil) and (FldDesc.TableInfo.IsView)) or IsRefresh) then begin
    Result := TOLEDBFieldDesc(FldDesc).BaseColumnName;
    if Result = '' then
      Result := FldDesc.ActualName;
    Result := QuoteName(Result);
    Exit;
  end;

  IsView := False;
  TableInfo := FDataSetService.UpdatingTableInfo;
  if TableInfo <> nil then
    IsView := TableInfo.IsView;

  if (FldDesc.TableInfo <> nil) and (not IsView) then
    Result := inherited GetActualFieldName(FldDesc, IsRefresh)
  else
    Result := QuoteName(FldDesc.ActualName);
end;}

procedure TCustomMSSQLGenerator.AddFieldToCondition(SB: _StringBuilder; FieldDesc: TCRFieldDesc;
      const StatementType: TStatementType;
      const ModifiedFieldsOnly: boolean;
      const Index: integer = -1);
var
  IsIdentityField: boolean;
begin
  Assert(FieldDesc <> nil);

  if ModifiedFieldsOnly then begin
    IsIdentityField := FieldDesc = FDataSetService.IdentityField;
    if (StatementType = stInsert) and IsIdentityField then begin // DMLRefresh ?
      FCondSB.Append(FieldDesc.ActualNameQuoted(SQLInfo, FDataSet.Options.QuoteNames));
      FCondSB.Append(' = ');
      if FDataSetService.GetIConnection.DBMSPrimaryVer > 7 then
        FCondSB.Append('SCOPE_IDENTITY()')
      else
        FCondSB.Append('@@Identity');
      Exit;
    end;
  end;

  inherited;
end;

procedure TCustomMSSQLGenerator.GenerateConditions(SB: _StringBuilder;
      const StatementType: TStatementType;
      const ModifiedFieldsOnly: boolean;
      const KeyAndDataFields: TKeyAndDataFields;
      const Index: integer = -1);

  procedure GenerateCondForRQ;
  {SQL Server Books Online -> Accessing and Changing Relational Data ->
   Transact-SQL Syntax Elements -> Using Data Types -> Using Special Data:

   In SQL Server version 7.0 and SQL Server 2000, @@DBTS is only incremented for use
   in timestamp columns. If a table contains a timestamp column, every time a row is
   modified by an INSERT, UPDATE, or DELETE statement, the timestamp value in the row
   is set to the current @@DBTS value, and then @@DBTS is incremented by one...}
  var
    MaxTimestamp: Int64;
    FieldDesc: TOLEDBFieldDesc;
  begin
    FieldDesc := FDataSetService.FTimestampField;
    if FieldDesc = nil then
      DatabaseError(STimestampFieldRequired);

    MaxTimestamp := TOLEDBTableInfo(FieldDesc.TableInfo).MaxTimestamp;
    FCondSB.Append(GetActualFieldName(FieldDesc, True) + ' > ' + '0x' + IntToHex(MaxTimestamp, SizeOf(MaxTimestamp) * 2));
  end;

var
  i: integer;
  FldUsed: set of byte;
  TestChanges: boolean;
  Field: TField;
begin
  FCondSB.Length := 0;

  if StatementType = stRefreshQuick then
    GenerateCondForRQ
  else
  begin
    TestChanges := (StatementType in [stInsert, stUpdate]) and FDataSetService.IsDMLRefresh and
      (FDataSetService.GetCursorType in [ctDefaultResultSet, ctBaseTable]) and FDataSetService.FCheckRowVersion;

    if not TestChanges then
      inherited
    else
    begin
      Field := nil;
      if FDataSetService.FTimestampField <> nil then
        Field := FDataSet.GetField(FDataSetService.FTimestampField);
      if (Field <> nil) and not Field.IsNull then
        AddFieldToCondition(FCondSB, FDataSetService.FTimestampField, StatementType, ModifiedFieldsOnly, Index)
      else
      begin
        FldUsed := [];
        if Length(KeyAndDataFields.KeyFieldDescs) > 0 then
          for i := 0 to High(KeyAndDataFields.KeyFieldDescs) do begin
            AddFieldToCondition(FCondSB, KeyAndDataFields.KeyFieldDescs[i], StatementType, ModifiedFieldsOnly, Index);
            FldUsed := FldUsed + [KeyAndDataFields.KeyFieldDescs[i].FieldNo];
          end;

        if Length(KeyAndDataFields.DataFieldDescs) = 0 then
          DatabaseError(SNoKeyFields);
        for i := 0 to High(KeyAndDataFields.DataFieldDescs) do
          if not IsBlobDataType(KeyAndDataFields.DataFieldDescs[i].DataType) // not "text", "ntext", "image"
            and not (KeyAndDataFields.DataFieldDescs[i].FieldNo in FldUsed) then
            AddFieldToCondition(FCondSB, KeyAndDataFields.DataFieldDescs[i], StatementType, ModifiedFieldsOnly, Index);
      end;
    end;
  end;
end;

procedure TCustomMSSQLGenerator.GenerateLockSQL(const KeyAndDataFields: TKeyAndDataFields; const Index: integer = -1);
begin
  FHeaderSB.Append('SELECT * FROM ');
  FHeaderSB.Append(FTableInfo.TableName);
  FHeaderSB.Append(SLLineSeparator);

  (* MSDN: Table Hint (Transact-SQL)
    HOLDLOCK     Makes shared locks more restrictive by holding them until a transaction is completed
    ROWLOCK      Specifies that row locks are taken when page or table locks are ordinarily taken.
    ROWLOCK      Specifies that row locks are taken when page or table locks are ordinarily taken.
    TABLOCK      Specifies that a shared lock is taken on the table held until the end-of-statement. If HOLDLOCK is also specified, the shared table lock is held until the end of the transaction.
    TABLOCKX     Specifies that an exclusive lock is taken on the table. If HOLDOCK is also specified, the lock is held until the transaction completes.
    UPDLOCK      Specifies that update locks are to be taken and held until the transaction completes.
    XLOCK        Specifies that exclusive locks are to be taken and held until the transaction completes.

    ltUpdate:    ROWLOCK, UPDLOCK, HOLDLOCK
    ltExclusive: ROWLOCK, XLOCK
  *)
  case FDataSetService.FLockType of
    _ltUpdate:
      case FDataSetService.FLockObject of
        _loRow:
          FMiddleSB.Append('WITH (UPDLOCK, ROWLOCK, HOLDLOCK)');
        _loTable:
          FMiddleSB.Append('WITH (UPDLOCK, TABLOCK, HOLDLOCK)');
      end;
    _ltExclusive:
      case FDataSetService.FLockObject of
        _loRow:
          FMiddleSB.Append('WITH (ROWLOCK, XLOCK)');
        _loTable:
          FMiddleSB.Append('WITH (TABLOCKX, HOLDLOCK)');
      end;
  end;
  FMiddleSB.Append(SLLineSeparator);
  FMiddleSB.Append('WHERE');
  FMiddleSB.Append(SLLineSeparator);
  FMiddleSB.Append('  ');
  case FDataSetService.FLockObject of
    _loRow:
      GenerateConditions(FCondSB, stLock, False, KeyAndDataFields, Index);
    _loTable:
      FCondSB.Append('1 = 0');
  end;
end;

function TCustomMSSQLGenerator.GenerateSelectFilestreamContextSQL(const FieldName: _string;
  const KeyAndDataFields: TKeyAndDataFields; Params: TDAParams): _string;
begin
  if (FDataSetService.UpdatingTableInfo = nil) or
     (FDataSetService.UpdatingTableInfo.TableName = '') then
    DatabaseError(SBadTableInfoName);
  if FieldName = '' then
    DatabaseError(SFieldNameNotDefined);

  Clear;
  FParams := Params;
  if FParams <> nil then begin
    FParams.BeginUpdate;
    FParams.Clear;
    FParamsInfo.Clear;
  end;
  try
    FHeaderSB.Append('SELECT ');
    FHeaderSB.Append(SQLInfo.NormalizeName(FieldName, FDataSet.Options.QuoteNames));
    FHeaderSB.Append('.PathName(), GET_FILESTREAM_TRANSACTION_CONTEXT()');
    FHeaderSB.Append(SLLineSeparator);
    FMiddleSB.Append('FROM ');
    FMiddleSB.Append(SQLInfo.NormalizeName(FDataSetService.UpdatingTableInfo.TableNameFull, FDataSet.Options.QuoteNames));
    FMiddleSB.Append(SLLineSeparator);
    FMiddleSB.Append('WHERE ');
    // Append FParamSB
    GenerateConditions(FCondSB, stCustom, False, KeyAndDataFields);

    Result := AssembleSB(stCustom);
  finally
    if FParams <> nil then begin
      FParams.EndUpdate;
      RecreateParamsRef(FParams);
    end;
  end;
end;

{ TCustomMSDataSetUpdater }

constructor TCustomMSDataSetUpdater.Create(AOwner: TDataSetService);
begin
  inherited;

  FDataSetService := TCustomMSDataSetService(AOwner);
end;

function TCustomMSDataSetUpdater.GetIRecordSet: TOLEDBRecordSet;
begin
  Result := TOLEDBRecordSet(inherited GetIRecordSet);
end;

function TCustomMSDataSetUpdater.UseParamType: boolean;
begin
  Result := FUseParamType;
end;

function TCustomMSDataSetUpdater.NeedReturnParams: boolean;
begin
  Result := inherited NeedReturnParams or FDataSetService.IsDMLRefresh;
end;

procedure TCustomMSDataSetUpdater.SetRowsAffected(Value: Integer);
begin
  inherited SetRowsAffected(Value);
end;

function TCustomMSDataSetUpdater.GetIdentityFieldValue(var Value: variant): boolean;
var
  UserSQL: boolean;
  IdentityParamIdx: integer;
  UQParams: TDAParams;
begin
  Result := False;             

  BeginConnection; // for Pooling
  try
    IdentityParamIdx := - 1;
    UQParams := TDBAccessUtils.GetParams(FUpdateQuery);

    if GetUpdateObject <> nil then
      UserSQL := GetUpdateObject.SQL[DB.ukInsert].Count > 0
    else
      UserSQL := Length(GetUpdateSQL(stInsert)) > 0;

    if not UserSQL then begin // This is not custom user statement and last parameter is used for Identity
      if FDataSetService.FQueryIdentity and (FDataSetService.IdentityField <> nil) and (FDataSetService.GetCursorType in [ctDefaultResultSet, ctBaseTable]) then begin
        if IsCompactEdition(FDataSetService.GetIConnection.ProviderPrimaryVer) then
          IdentityParamIdx := 0
        else begin
          IdentityParamIdx := UQParams.Count - 1;
          Assert(IdentityParamIdx >= 0);
          // UQParams[IdentityParamIdx].ParamType := ptInputOutput; - already setted by AddParam
        end;
      end;
    end;

    if (IdentityParamIdx >= 0) and not NeedReturnParams then begin
      if IsCompactEdition(FDataSetService.GetIConnection.ProviderPrimaryVer) then begin
        TDBAccessUtils.GetSQL(FUpdateQuery).Text := 'SELECT @@IDENTITY';
        TDBAccessUtils.Open(FUpdateQuery);
        Value := TCustomDADataSet(FUpdateQuery).Fields[IdentityParamIdx].Value;
      end
      else
        Value := UQParams[IdentityParamIdx].Value;
      Result := True;
    end;
  finally
    EndConnection;
  end;
end;

procedure TCustomMSDataSetUpdater.CheckUpdateQuery(const StatementType: TStatementType);
begin
  inherited;
end;

procedure TCustomMSDataSetUpdater.SetUpdateQueryOptions(const StatementType: TStatementType);
begin
  if IsClass(FUpdateQuery, TCustomDADataSet) then
    with TDBAccessUtils.GetIRecordSet(FUpdateQuery as TCustomDADataSet) do begin
      SetProp(prFetchAll, True);
      SetProp(prReadOnly, True);
      SetProp(prUniqueRecords, False);
      SetProp(prRequestSQLObjects, False);
    end;
end;

function TCustomMSDataSetUpdater.SavepointAllowed: boolean;
begin
  Result := FDataSetService.GetOLEDBProvider <> prCompact;
end;

function TCustomMSDataSetUpdater.PerformSQL(const SQL: _string;
  const StatementTypes: TStatementTypes): boolean;
begin
  try
    Result := inherited PerformSQL(SQL, StatementTypes);
  finally
    FUseParamType := False;
  end;
end;

function TCustomMSDataSetUpdater.IsRefreshQuickField(FieldDesc: TFieldDesc): boolean;
begin
  Result := TOLEDBFieldDesc(FieldDesc).IsTimestamp;
end;

procedure TCustomMSDataSetUpdater.SaveMaxRefreshQuickValue(FieldDesc: TFieldDesc; const Value: variant);
var
  Field: TOLEDBFieldDesc;
  Val: Int64;
  MaxTimestamp: Int64;
{$IFDEF CLR}
  Data: TBytes;
{$ENDIF}
begin
  Field := TOLEDBFieldDesc(GetIRecordSet.FindField(FieldDesc.Name));
  if (Field <> nil) and Field.IsTimestamp and (Field.TableInfo <> nil) then begin
  {$IFDEF CLR}
    Data := TBytes(Value);
    System.Array.Reverse(Data, 0, SizeOf(Int64));
    Val := BitConverter.ToInt64(Data, 0);
  {$ELSE}
    Val := Int64(TVarData(Value).VArray.Data^);
    Reverse8(@Val);
  {$ENDIF}
    MaxTimestamp := TOLEDBTableInfo(Field.TableInfo).MaxTimestamp;
    if {$IFDEF VER7P}UInt64{$ENDIF}(MaxTimestamp) < {$IFDEF VER7P}UInt64{$ENDIF}(Val) then
      TOLEDBTableInfo(Field.TableInfo).MaxTimestamp := Val;
  end;
end;

procedure TCustomMSDataSetUpdater.PrepareUpdate;
var
  CursorType: Variant;
begin
  inherited;

  if roBeforeEdit in FDataSet.RefreshOptions then begin
    GetIRecordSet.GetProp(prCursorType, CursorType);
    if TMSCursorType(Integer(CursorType)) in ServerCursorTypes then
      FDataSet.Resync([]); // CR 9097
  end;
end;

function TCustomMSDataSetUpdater.PerformRefreshRecord: boolean;
var
  Bookmark: TBookmark;
begin
  Result := True;
  case FDataSetService.GetCursorType of
    ctDefaultResultSet:
      Result := inherited PerformRefreshRecord;
    ctStatic, ctKeySet: begin
      Bookmark := FDataSet.GetBookmark;
      try
        GetIRecordSet.SetToBookmark(IntPtr(Bookmark)); // ReFetch
      finally
        FDataSet.FreeBookmark(Bookmark);
      end;
      SetRowsAffected(1); // Must be always OK
    end;
    ctDynamic: begin
      if GetIRecordSet.FetchToBookmarkValue then
        SetRowsAffected(1)
      else
        SetRowsAffected(0);
    end;
    ctBaseTable: begin
      if FDataSetService.GetCursorUpdate and not FDataSet.ReadOnly then begin // as for ctDynamic
        if GetIRecordSet.FetchToBookmarkValue then
          SetRowsAffected(1)
        else
          SetRowsAffected(0);
      end
      else // as for ctDefaultResultSet
        Result := inherited PerformRefreshRecord;
    end;
  end;
end;

function TCustomMSDataSetUpdater.LockCompare(const Value1, Value2: variant): boolean;
var
  DateTime1, DateTime2: TDateTime;
  Hour, Min, Sec1, MSec1, Sec2, MSec2: Word;
begin
  Result := inherited LockCompare(Value1, Value2);
  if not Result and (VarType(Value1) = varDate) and (VarType(Value2) = varDate) then begin
    DateTime1 := Value1;
    DecodeTime(DateTime1, Hour, Min, Sec1, MSec1);
    ReplaceTime(DateTime1, EncodeTime(Hour, Min, 0, 0));
    DateTime2 := Value2;
    DecodeTime(DateTime2, Hour, Min, Sec2, MSec2);
    ReplaceTime(DateTime2, EncodeTime(Hour, Min, 0, 0));
    Result := (DateTime1 = DateTime2) and (Abs(Sec1 * 1000 + MSec1 - (Sec2 * 1000 + MSec2)) < 1000);
  end;
end;

{ TCustomMSDataSetService }

constructor TCustomMSDataSetService.Create(AOwner: TMemDataSet);
begin
  inherited;
end;

procedure TCustomMSDataSetService.CreateDataSetUpdater;
begin
  SetDataSetUpdater(TCustomMSDataSetUpdater.Create(Self));
end;

procedure TCustomMSDataSetService.SetDataSetUpdater(Value: TDataSetUpdater);
begin
  inherited;

  FUpdater := TCustomMSDataSetUpdater(Value);
end;

procedure TCustomMSDataSetService.CreateSQLGenerator;
begin
  SetSQLGenerator(TCustomMSSQLGenerator.Create(Self));
end;

procedure TCustomMSDataSetService.DetectIdentityFields(out IdentityField, TimeStampField: TOLEDBFieldDesc);
var
  i: integer;
  FieldDesc: TOLEDBFieldDesc;
  RecordSet: TCRRecordSet;
begin
  IdentityField := nil;
  TimeStampField := nil;

  //Search Identity Fields
  RecordSet := GetIRecordSet;
  for i := RecordSet.FieldCount - 1 downto 0 do begin
    FieldDesc := TOLEDBFieldDesc(RecordSet.Fields[i]);

    if (FieldDesc.FieldDescKind <> fdkData) or FieldDesc.Hidden then
      continue;

    if FieldDesc.IsAutoIncrement then begin
      if FDataSet.Options.SetFieldsReadOnly then
        FieldDesc.ReadOnly := True;

      if (UpdatingTableInfo <> nil) and (FieldDesc.TableInfo = UpdatingTableInfo) then
        IdentityField := FieldDesc;
    end;

    if FieldDesc.IsTimestamp and (UpdatingTableInfo <> nil) and
      (FieldDesc.TableInfo = UpdatingTableInfo)
    then
      TimestampField := FieldDesc;
  end;
end;

function TCustomMSDataSetService.DetectIdentityField: TCRFieldDesc;
var
  IdentityField: TOLEDBFieldDesc;
begin
  DetectIdentityFields(IdentityField, FTimeStampField);
  Result := IdentityField;
end;

function TCustomMSDataSetService.DetectCanModify: boolean;
begin
  Result := inherited DetectCanModify or
    not (FDataSet.ReadOnly or FDataSet.UniDirectional) and
    (FIsAnyFieldCanBeModified or  //upd1
    (FDataSet.SQLInsert.Count > 0) or
    (FDataSet.SQLUpdate.Count > 0) or
    (FDataSet.SQLDelete.Count > 0));
end;

function TCustomMSDataSetService.CanUseAllKeyFields: boolean;
begin  
  Result := True;
end;

procedure TCustomMSDataSetService.InitCursor;
var
  i: integer;
  Field: TField;
  FieldDesc: TFieldDesc;
begin
  inherited;

  // Set right precision for TFloatField
  for i := 0 to FDataSet.Fields.Count - 1 do begin
    Field := FDataSet.Fields[i];
    FieldDesc := FDataSet.GetFieldDesc(Field);
    if (Field is TFloatField)
      and (TFloatField(Field).Precision = 15 {Default})
      and (Field.FieldKind = fkData)
    then begin
      case FieldDesc.DataType of
        dtFloat, dtCurrency: // Precision cannot be greater then 15
          TFloatField(Field).Precision := FieldDesc.Length;
      end;
    end;
  end;  
end;

procedure TCustomMSDataSetService.CloseCursor;
begin
  inherited;
end;

procedure TCustomMSDataSetService.SetFieldOrigin(Field: TField; FieldDesc: TCRFieldDesc);
var
  TableName: _string;
  OLEDBFieldDesc: TOLEDBFieldDesc;
begin
  if Field.FieldKind = fkData then begin
    OLEDBFieldDesc := TOLEDBFieldDesc(FieldDesc);

    TableName := GenerateTableName(OLEDBFieldDesc.BaseCatalogName, OLEDBFieldDesc.BaseSchemaName,
      OLEDBFieldDesc.BaseTableName, GetDatabase);
    Field.Origin := TableName + '.' + OLEDBSQLInfo.NormalizeName(OLEDBFieldDesc.ActualName);
  end;
end;

procedure TCustomMSDataSetService.FillFieldDescs(out FieldDescs: TFieldDescArray;
  FillKeyFieldDescs, ForceUseAllFields: boolean);

  procedure ProcessField(FieldDesc: TOLEDBFieldDesc);
  begin
    if FillKeyFieldDescs then begin
      if FieldDesc.IsKey or FieldDesc.IsAutoIncrement then begin
        SetLength(FieldDescs, Length(FieldDescs) + 1);
        FieldDescs[High(FieldDescs)] := FieldDesc;
      end;
    end
    else begin
      if not FieldDesc.ReadOnly then begin
        SetLength(FieldDescs, Length(FieldDescs) + 1);
        FieldDescs[High(FieldDescs)] := FieldDesc;
      end;
    end;
  end;

var
  Field: TField;
  FieldDesc: TOLEDBFieldDesc;
  i: integer;
  IsNeedProcessField: boolean;
begin
  FieldDescs := nil;

  if (FDataSet.Fields.Count = 0) or (GetIRecordSet.Fields.Count = 0) then
    Exit;

  for i := 0 to FDataSet.Fields.Count - 1 do begin
    Field := FDataSet.Fields[i];
    if Field.FieldKind = fkData then begin
      FieldDesc := FDataSet.GetFieldDesc(Field) as TOLEDBFieldDesc;

      IsNeedProcessField := ForceUseAllFields or
        (FieldDesc.TableInfo = UpdatingTableInfo);

      if IsNeedProcessField then
        ProcessField(FieldDesc);
    end;
  end;
end;

procedure TCustomMSDataSetService.FillKeyFieldDescs(out KeyFieldDescs: TFieldDescArray; ForceUseAllKeyFields: boolean);
begin
  if (GetKeyFields <> '') or (IdentityField <> nil) then
    inherited FillKeyFieldDescs(KeyFieldDescs, ForceUseAllKeyFields)
  else
    FillFieldDescs(KeyFieldDescs, True, ForceUseAllKeyFields);
end;

procedure TCustomMSDataSetService.FillDataFieldDescs(out DataFieldDescs: TFieldDescArray; ForceUseAllKeyFields: boolean);
begin
  FillFieldDescs(DataFieldDescs, False, ForceUseAllKeyFields);
end;

procedure TCustomMSDataSetService.FillFieldsDefaultValues;
var
  MetaData: TDAMetadata;
  Field: TField;
  DefValue: _string;
begin
  if not FDataSet.Options.DefaultValues or (UpdatingTableInfoIdx = -1) then
    Exit;

  MetaData := TDAMetadata.Create(nil);
  try
    MetaData.Connection := UsedConnection;
    MetaData.MetaDataKind := 'columns';
    //MetaData.DatabaseName := TCustomMSConnection(UsedConnection).Database;
    MetaData.Restrictions.Text := 'table_name=' +
      GetTablesInfo[FUpdatingTableInfoIdx].TableName;
    MetaData.Open;
    while not MetaData.EOF do begin
      Field := FDataSet.FindField(MetaData.FieldByName('COLUMN_NAME').AsString);
      if (Field <> nil) and not MetaData.FieldByName('DEFAULT_VALUE').IsNull then begin
        DefValue := MetaData.FieldByName('DEFAULT_VALUE').AsString;
        if DefaultExpressionOldBehavior then
          case Field.DataType of
            ftBoolean:
              Field.DefaultExpression := BoolToStr((DefValue <> '0') and (DefValue <> '((0))'), True);
            ftFloat, ftBCD{$IFDEF VER6P}, ftFMTBCD{$ENDIF}:
              Field.DefaultExpression := StringReplace(DefValue, '.', DecimalSeparator, [rfReplaceAll]);
            else
              Field.DefaultExpression := DefValue;
          end
        else
          Field.DefaultExpression := DefValue;
      end;
      MetaData.Next;
    end;
  finally
    MetaData.Free;
  end;
end;

function TCustomMSDataSetService.GetRecCount: integer;
var
  ParamUsed: boolean;

  function GetCount(const SQL: _string): longint;
  var
    i: integer;
    OldParamCheck: boolean;
    UQ: TCustomDADataSet;
    MonitorClass: TDASQLMonitorClass;
    MessageID: cardinal;
  begin
    FUpdater.CheckUpdateQuery(stCustom);
    UQ := FUpdater.UpdateQuery as TCustomDADataSet;

    OldParamCheck := UQ.ParamCheck;
    try
      UQ.ParamCheck := FDataSet.ParamCheck;
      UQ.SQL.Text := SQL;
      if not FDataSet.ParamCheck then
        for i := 0 to FDataSet.Params.Count {without -1!} do
          UQ.Params.Add;

      if UQ.ParamCount > 0 then begin
        UQ.Params[0].DataType := ftLargeint;
        UQ.Params[0].ParamType := ptOutput;
      end;

      UQ.Macros.Assign(FDataSet.Macros);
      for i := 0 to FDataSet.Params.Count - 1 do
        UQ.Params[i + 1].Assign(FDataSet.Params[i]);

      MonitorClass := TDASQLMonitorClass(TDBAccessUtils.SQLMonitorClass(UsedConnection));
      if MonitorClass.HasMonitor or FDataSet.Debug then
        MonitorClass.SQLExecute(FDataSet, SQL, UQ.Params, 'Get RecordCount', MessageID, True);

      if ParamUsed then begin
        UQ.Execute;
        Result := UQ.Params[0].Value;
      end
      else begin
        UQ.Open;
        Result := UQ.Fields[0].Value;
      end;

      if MonitorClass.HasMonitor or FDataSet.Debug then
        MonitorClass.SQLExecute(FDataSet, SQL, UQ.Params, 'Get RecordCount', MessageID, False);
    finally
      UQ.ParamCheck := OldParamCheck;
    end;
  end;

var
  Parser: TMSParser;
  Code, SelectPos, FromPos, DelimiterPos: integer;
  s: _string;
  CountParamName: _string;
  WithLexemUsed, IsSelectModifier: boolean;
  NonBlocking: variant;
begin
  GetIRecordSet.GetProp(prNonBlocking, NonBlocking);
  if ((not IsFetchAll or NonBlocking) and FDataSet.Options.QueryRecCount) // Server cursors or DefaultResultSet with FetchAll = False
    and not ((FDataSet.Params.Count > 0) and (FDataSet.Params[0].ParamType = ptResult)) then begin // Current SQL does not have RETURN parameter
    s := FDataSet.FinalSQL;
    s := {$IFDEF CLR}Devart.Dac.{$ENDIF}DBAccess._SetOrderBy(s, '', TMSParser);
    ParamUsed := False;
    Parser := TMSParser.Create(s);
    Parser.OmitBlank := False;
    Parser.OmitComment := True;
    try
      IsSelectModifier := False;
      Code := Parser.ToLexem([lxWITH, lxSELECT]);
      WithLexemUsed := Code = lxWITH;
      if Code = lxWITH then
        Code := Parser.ToLexem(lxSELECT, True);

      if Code <> lcEnd then begin
        SelectPos := Parser.CurrPos;
        repeat
          Code := Parser.ToLexem([lxALL, lxDISTINCT, lxTOP, lxFROM, lxSELECT]);
          case Code of
            lxSELECT:
              SelectPos := Parser.CurrPos;
            lxALL, lxDISTINCT, lxTOP:
              IsSelectModifier := True;
          end;
        until (Code = lcEnd) or (Code = lxFROM);

        if Code = lxFROM then
          FromPos := Parser.CurrPos
        else
          FromPos := 0;

        repeat
          Code := Parser.ToLexem([lxUNION, 7{';'}]);
          if Code = lxUNION then
            IsSelectModifier := True;
        until (Code = 7{';'}) or (Code = lcEnd);

        if Code = 7 then // ';'
          DelimiterPos := Parser.CurrPos
        else
          DelimiterPos := MaxInt;

        if (FromPos > 0) and not IsSelectModifier then begin
          if (GetOLEDBProvider = prCompact) or WithLexemUsed then begin
            s := Copy(s, 1, SelectPos) + ' COUNT(*) AS FCOUNT ' + Copy(s, FromPos - 4{length('FROM')}, MaxInt);
          end
          else begin
            ParamUsed := True;
            if FDataSet.ParamCheck then
              CountParamName := ':PCOUNT'
            else
              CountParamName := '?';
            s := Copy(s, 1, SelectPos - 6{length('SELECT')}) +
              'SET ' + CountParamName + ' = (SELECT COUNT(*)' + Copy(s, FromPos - 4{length('FROM')}, MaxInt) + ')';
          end;
        end
        else
          if IsSelectModifier then
            s := Copy(s, 1, SelectPos) + ' COUNT(*) AS FCOUNT FROM (SELECT ' + Copy(s, SelectPos + 1, DelimiterPos - 1 - SelectPos) + ') q'
          else
            s := Copy(s, 1, SelectPos) + ' COUNT(*) AS FCOUNT FROM (SELECT ' + Copy(s, SelectPos + 1, DelimiterPos - 1 - SelectPos) + ' f) q';
      end;
    finally
      Parser.Free;
    end;

    if s <> '' then
      Result := GetCount(s)
    else
      Result := 0;
  end
  else
    Result := inherited GetRecCount;
end;

procedure TCustomMSDataSetService.SetNumberRange(FieldDef: TFieldDef);
var
  Field: TField;
  FieldDesc: TOLEDBFieldDesc;
{$IFDEF VER6P}
  mv: Extended;
{$ENDIF}
  ServerVersion: integer;
begin
  ServerVersion := GetIConnection.ProviderPrimaryVer;
  Field := FDataSet.FindField(FieldDef.Name);
  if Field <> nil then begin
    FieldDesc := FDataSet.GetFieldDesc(Field) as TOLEDBFieldDesc;
    if (FieldDesc.DataType = dtFloat) and (FieldDesc.SubDataType and dtUInt8 <> 0) then begin
        Assert(Field is TWordField);
        TWordField(Field).MinValue := 0;
        TWordField(Field).MaxValue := 255;
    end
    else
      case FieldDesc.DataType of
        dtInt8:
        begin
          TIntegerField(Field).MinValue := -128;
          TIntegerField(Field).MaxValue := 127;
        end;
        dtInt16:
        begin
          if IsCompactEdition(ServerVersion) then begin
            TSmallIntField(Field).MinValue := -32768;
            TSmallIntField(Field).MaxValue := 32767;
          end
          else begin
            TIntegerField(Field).MinValue := -32768;
            TIntegerField(Field).MaxValue := 32767;
          end;
        end;
        dtInt32:
        begin
          TIntegerField(Field).MinValue := -2147483647;
          TIntegerField(Field).MaxValue := 2147483647;
        end;
        dtInt64:
        begin
          TLargeintField(Field).MinValue := -9223372036854775807;
          TLargeintField(Field).MaxValue := 9223372036854775807;
        end;
        dtWord:
        begin
          TWordField(Field).MinValue := 0;
          TWordField(Field).MaxValue := 65535;
        end;
        dtFloat:
        begin
          if FieldDesc.Scale = 255 then begin
            if FieldDesc.Length = 7 then begin
              TFloatField(Field).MinValue := -3.40E38;
              TFloatField(Field).MaxValue := 3.40E38;
            end
            else
            begin
              TFloatField(Field).MinValue := -1.79E308;
              TFloatField(Field).MaxValue := 1.79E308;
            end;
          end
          else
            if (FieldDesc.Length > 0) then begin
              TFloatField(Field).Precision := FieldDesc.Length;
              TFloatField(Field).MaxValue :=
                IntPower(10, FieldDesc.Length - FieldDesc.Scale) -
                IntPower(10, - Integer(FieldDesc.Scale));
              TFloatField(Field).MinValue := -TFloatField(Field).MaxValue;
            end;
        end;
        dtBCD:
        begin
          TBCDField(Field).Precision := FieldDesc.Length;
          if (FieldDesc.Length > 0) and (FieldDesc.Length <= 15) then begin
            TBCDField(Field).MaxValue :=
              IntPower(10, FieldDesc.Length - FieldDesc.Scale) -
              IntPower(10, - Integer(FieldDesc.Scale));
            TBCDField(Field).MinValue := -TBCDField(Field).MaxValue;
          end;
        end;
      {$IFDEF VER6P}
      {$IFNDEF FPC}
        dtFmtBCD:
        begin
          TFMTBCDField(Field).Precision := FieldDesc.Length;
          if (FieldDesc.Length > 0) and (FieldDesc.Length <= 15) then begin
            mv :=
              IntPower(10, FieldDesc.Length - FieldDesc.Scale) -
              IntPower(10, - Integer(FieldDesc.Scale));
            TFMTBCDField(Field).MaxValue := FloatToStr(mv);
            TFMTBCDField(Field).MinValue := FloatToStr(-mv);
          end;
        end;
      {$ENDIF}
      {$ENDIF}
        dtCurrency:
          if IsCompactEdition(ServerVersion) then begin
            TCurrencyField(Field).MinValue := -922337203685477.5808;
            TCurrencyField(Field).MaxValue := 922337203685477.5807;
          end
          else begin
            if FieldDesc.Length = 10 then begin
              TFloatField(Field).MinValue := -214748.3648 ;
              TFloatField(Field).MaxValue := 214748.3647;
            end
            else
              if FieldDesc.Length = 19 then begin
                TFloatField(Field).MinValue := -922337203685477.5808;
                TFloatField(Field).MaxValue := 922337203685477.5807;
              end;
          end;
      end;
    end;
end;

function TCustomMSDataSetService.OpenNext: boolean;
var
  MaintainConnection: boolean;
begin
  // InternalClose and FIsInInitFieldDefs. Workaround for FiedDefs.Update has
  // been removed because it was duplicated by TOLEDBRecordSet.InternalInitFields
  // method

  // UnPrepare. Code concerning OpenNext has been removed because it was duplicated
  // by internal layer

  if FDataSet.Prepared then
    DatabaseError(SOpenNextPreparedSQL);

  if not FDataSet.Active then begin
    FDataSet.Open;
    Result := True;
  end
  else begin
    MaintainConnection := UsedConnection.Connected;

    if MaintainConnection then
      BeginConnection;

    try
      Assert(GetIRecordSet <> nil);
      GetIRecordSet.SetProp(prLockClearMultipleResults, True);
      try
        FDataSet.Close;
        FDataSet.UnPrepare;
      finally
        GetIRecordSet.SetProp(prLockClearMultipleResults, False);
      end;

      Result := not TOLEDBCommand(GetIRecordSet.GetCommand).IUnknownNextIsNull;

      if Result then begin
        FDataSet.FieldDefs.Updated := False;
        FDataSet.Open;
      end;
    finally
      if MaintainConnection then 
        EndConnection;
    end;
  end;
end;

procedure TCustomMSDataSetService.InitMasterParams(Params: TDAParams);
var
  MasterPos: integer;
  MasterName: _string;
  Param: TDAParam;
begin
  if (FDataSet.DataSource <> nil) and
    (FDataSet.MasterFields <> '') and (FDataSet.DetailFields <> '') and
    not ((FDataSet.DataSource.DataSet <> nil) and FDataSet.DataSource.DataSet.Active)
    and not FDataSet.Options.LocalMasterDetail and not FDataSet.Prepared
  then begin
    MasterPos := 1;
    while True do begin
      MasterName := ExtractFieldName(FDataSet.MasterFields, MasterPos);
      if MasterName <> '' then begin
        Param := Params.FindParam(MasterName);
        if (Param <> nil) and (Param.DataType = ftUnknown) then
          Param.DataType := ftString;
      end
      else
        break;
    end;
  end;
end;

function TCustomMSDataSetService.SetProp(Prop: integer; const Value: variant): boolean;
begin
  Result := True;
  case Prop of
    prQueryIdentity:
      FQueryIdentity := Value;
    prCheckRowVersion:
      FCheckRowVersion := Value;
    prLockType:
      FLockType := _TMSLockType(Value);
    prLockObject:
      FLockObject := _TMSLockObject(Value);     
  else
    Result := inherited SetProp(Prop, Value);
  end;
end;

function TCustomMSDataSetService.GetProp(Prop: integer; var Value: variant): boolean;
begin
  Result := True;
  case Prop of
    prQueryIdentity:
      Value := FQueryIdentity;
  else
    Result := inherited GetProp(Prop, Value);
  end;  
end;

function TCustomMSDataSetService.GetIConnection: TOLEDBConnection;
begin
  Result := TOLEDBConnection(TDBAccessUtils.GetIConnection(UsedConnection));
  Assert(Result <> nil); //upd1 should be error
end;

function TCustomMSDataSetService.GetOLEDBProvider: TOLEDBProvider;
var
  Value: Variant;
begin
  GetIConnection.GetProp(prProvider, Value);
  Result := TOLEDBProvider(Value);
end;

function TCustomMSDataSetService.GetIRecordSet: TOLEDBRecordSet;
begin
  Result := TOLEDBRecordSet(inherited GetIRecordSet);
end;

function TCustomMSDataSetService.GetDatabase: _string;
var
  Value: Variant;
begin
  GetIConnection.GetProp(prDatabase, Value);
  Result := Value;
end;

function TCustomMSDataSetService.GetCursorType: TMSCursorType;
var
  Value: Variant;
begin
  Assert(GetIRecordSet <> nil); //upd1: should it be an error ???
  GetIRecordSet.GetProp(prCursorType, Value);
  Result := TMSCursorType(Value);
end;

function TCustomMSDataSetService.GetCursorUpdate: boolean;
var
  Value: Variant;
begin
  GetIRecordSet.GetProp(prCursorUpdate, Value);
  Result := Value;
end;

function TCustomMSDataSetService.IsFullRefresh: boolean;
begin
  Result := inherited IsFullRefresh;
end;

function TCustomMSDataSetService.IsDMLRefresh: boolean;
begin
  Result := inherited IsDMLRefresh;
end;

function TCustomMSDataSetService.IsUniqueRecords: boolean;
var
  Value: Variant;
begin
  GetIRecordSet.getProp(prUniqueRecords, Value);
  Result := Value;
end;

procedure TCustomMSDataSetService.WriteFieldXMLDataType(Field: TField; FieldDesc: TFieldDesc;
  const FieldAlias: _string; XMLWriter: XMLTextWriter);
begin
  inherited;

  if FieldDesc is TOLEDBFieldDesc then begin
    if TOLEDBFieldDesc(FieldDesc).IsAutoIncrement
      and not (Field.Required and not Field.ReadOnly) // Already writed in MemDS
    then
      XmlWriter.WriteAttributeString('rs:maybenull', 'false');
  end;
end;

procedure TCustomMSDataSetService.WriteFieldXMLAttributeType(Field: TField; FieldDesc: TFieldDesc;
  const FieldAlias: _string; XMLWriter: XMLTextWriter);
begin
  inherited;

  if FieldDesc is TOLEDBFieldDesc then begin
    if TOLEDBFieldDesc(FieldDesc).BaseCatalogName <> '' then
      XmlWriter.WriteAttributeString('rs:basecatalog', TOLEDBFieldDesc(FieldDesc).BaseCatalogName);

    if TOLEDBFieldDesc(FieldDesc).BaseSchemaName <> '' then
      XmlWriter.WriteAttributeString('rs:baseschema', TOLEDBFieldDesc(FieldDesc).BaseSchemaName);

    if TOLEDBFieldDesc(FieldDesc).IsTimestamp then
      XmlWriter.WriteAttributeString('rs:rowver', 'true');
  end;
end;

procedure TCustomMSDataSetService.GetFilestreamContext(const FieldName: _string;
  var PathName: WideString; var FSTrContext: variant);
var
  MonitorClass: TDASQLMonitorClass;
  MessageID: cardinal;
  UpdateQuery: TCustomDADataSet;
  KeyAndDataFields: TKeyAndDataFields;
  SQL: _string;
begin
  FUpdater.CheckUpdateQuery(stCustom);
  UpdateQuery := FUpdater.UpdateQuery as TCustomDADataSet;

  GetKeyAndDataFields(KeyAndDataFields, False);
  SQL := TCustomMSSQLGenerator(FSQLGenerator).GenerateSelectFilestreamContextSQL(FieldName, KeyAndDataFields, UpdateQuery.Params);
  TDBAccessUtils.SetSQLText(UpdateQuery, SQL, True, True);
  FUpdater.WriteUQParams(FSQLGenerator.ParamsInfo, [stCustom]);

  BeginConnection;
  try
    MonitorClass := TDASQLMonitorClass(TDBAccessUtils.SQLMonitorClass(UsedConnection));
    if not TDBAccessUtils.GetLockDebug(FDataSet) and (MonitorClass.HasMonitor or FDataSet.Debug) then
      MonitorClass.SQLExecute(FDataSet, SQL, UpdateQuery.Params, 'Get filestream context', MessageID, True);

    UpdateQuery.Open;

    if not TDBAccessUtils.GetLockDebug(FDataSet) and (MonitorClass.HasMonitor or FDataSet.Debug) then
      MonitorClass.SQLExecute(FDataSet, SQL, UpdateQuery.Params, 'Get filestream context', MessageID, False);

    try
      if UpdateQuery.Fields[0].IsNull then
        raise Exception.CreateFmt(SWrongBlob, [FieldName]);
      if UpdateQuery.Fields[1].IsNull then
        raise Exception.Create(SEmptyFSTransactionContext);

      PathName := VarToWideStr(UpdateQuery.Fields[0].Value);
      FSTrContext := UpdateQuery.Fields[1].Value;
    finally
      UpdateQuery.Close;
    end;
  finally
    EndConnection;
  end;
end;

{ TMSServerEnumerator }

function TMSServerEnumerator.SetProp(Prop: integer; const Value: variant): boolean;
begin
  Result := True;
  case Prop of
    prProvider:
      FPRovider := TOLEDBProvider(Value);
  else
    Result := inherited SetProp(Prop, Value);    
  end;  
end;

function TMSServerEnumerator.GetProp(Prop: integer; var Value: variant): boolean;
begin
  Result := inherited GetProp(Prop, Value);
end;

procedure TMSServerEnumerator.GetServerList(List: _TStrings);

  var
    ServerList: _TStringList;

  procedure GetServerListByReg;

   procedure AddComputerName(sl: TStringList);
    var
      CompName: _string;
    {$IFDEF CLR}
      sb: StringBuilder;
    {$ENDIF}
      Size: cardinal;
      i: integer;
    begin
      if sl.Count < 1 then
        Exit;
      Size := MAX_COMPUTERNAME_LENGTH;
      SetLength(CompName, Size);

    {$IFDEF CLR}
      sb := StringBuilder.Create(Size);
      try
        GetComputerName(sb, Size);
        CompName := sb.ToString;
      finally
        sb.Free;
      end;
    {$ELSE}
    {$IFDEF CRUNICODE}
      GetComputerNameW(_PChar(CompName), Size);
    {$ELSE}
      GetComputerName(PChar(CompName), Size);
    {$ENDIF}
      SetLength(CompName, Size);
    {$ENDIF}

      for i := 0 to sl.Count - 1 do
        if AnsiUpperCase(sl[i]) = 'MSSQLSERVER' then
          sl[i] := CompName                  // not named instance
        else
          sl[i] := CompName + '\' + sl[i];   // named instance
    end;

  var
    Reg: TRegistry;
    i: integer;
    Value: string;
    sl: TStringList;
  begin
    Reg := TRegistry.Create;
    sl := TStringList.Create;
    try
      // get server list to which Enterprise Manager was connected
      Reg.RootKey := HKEY_Local_Machine;
      if Reg.OpenKeyReadOnly('SOFTWARE\Microsoft\MSSQLServer\Client\ConnectTo') then begin
        Reg.GetValueNames(sl);

        // Delete non-server occurences
        for i := sl.Count - 1 downto 0 do begin
          Value := Reg.ReadString(sl[i]);
          if Pos(',', Value) = 0 then
            sl.Delete(i);
        end;

        sl.Sort;
        ServerList.AddStrings(sl);
        Reg.CloseKey;
      end;

      // get local SQL Server instances
      if Reg.OpenKeyReadOnly('SOFTWARE\Microsoft\Microsoft SQL Server\Instance Names\SQL') then begin
        Reg.GetValueNames(sl);

        AddComputerName(sl);
        ServerList.AddStrings(sl);
      end;

    finally
      Reg.Free;
      sl.Free;
    end;
  end;

  procedure GetServerListByOleDBEnum;
  var
    SourcesRowset: ISourcesRowset;
    Rowset: IRowset;
    ds: TOLEDBRecordSet;
    iu: IUnknown;
    RecBuf, FieldBuf: IntPtr;
    IsBlank: boolean;
  begin
    if CoCreateInstance(CLSID_SQLOLEDB_ENUMERATOR,
                                 nil,
                                 CLSCTX_INPROC_SERVER,
                                 IID_ISourcesRowset,
                                 SourcesRowset) <> S_OK then
      Exit;

    if SourcesRowset.GetSourcesRowset(nil, IID_IRowset, 0, nil, iu) <> S_OK then
      Exit;

    Rowset := IRowset(iu);
    ds := TOLEDBRecordSet.Create;
    try
       ds.SetIRowset(Rowset, False);
       ds.Open;

       ds.AllocRecBuf(RecBuf);
       FieldBuf := Marshal.AllocHGlobal(ds.Fields[0].Size + 1);
       try
         while True do begin
           ds.GetNextRecord(RecBuf);
           if ds.Eof then
             Break;

           ds.GetField(1, RecBuf, FieldBuf, IsBlank);
           if IsBlank then
             Continue;

           if ds.Fields[0].DataType = dtWideString then
             ServerList.Add(Marshal.PtrToStringUni(FieldBuf))
           else
             ServerList.Add(string(Marshal.PtrToStringAnsi(FieldBuf)));
         end;
       finally
         ds.Close;
         Marshal.FreeHGlobal(RecBuf);
         Marshal.FreeHGlobal(FieldBuf);
       end;
    finally
      ds.Free;
    end;
  end;

{$IFDEF WIN32}
  procedure GetServerListBySocket;
  var
    r: integer;
    FSd: longint;
    SndBuf, Nodelay, Broadcast, Timeout: longint;
    sock_addr: TSockAddr;
    buf: Byte;
    Buffer: array[0..8191] of Byte;
    Count: Integer;
    AnsiStr: AnsiString;
    str: string;
    pLexem, p1, p2: Integer;
    ServerName, InstanceName: string;
  begin
    if WsaData.wVersion = 0 then begin // not initialized
      r := WSAStartup($0101, WsaData);
      if r <> 0 then
        raise Exception.CreateFmt(SSocketError, [r]);
    end;

    FSd := socket(AF_INET, SOCK_DGRAM, IPPROTO_UDP);
    if FSd = SOCKET_ERROR then
      raise Exception.CreateFmt(SSocketError, [FSd]);

    try
      SndBuf := 0;
      setsockopt(FSd, SOL_SOCKET, SO_SNDBUF, PAnsiChar(@SndBuf), sizeof(SndBuf));
      Nodelay := 1;
      setsockopt(FSd, SOL_SOCKET, TCP_NODELAY, PAnsiChar(@Nodelay), sizeof(Nodelay));
      Broadcast := 1;
      setsockopt(FSd, SOL_SOCKET, SO_BROADCAST, PAnsiChar(@Broadcast), sizeof(Broadcast));
      Timeout := 500;
      setsockopt(FSd, SOL_SOCKET, SO_RCVTIMEO, PAnsiChar(@Timeout), sizeof(Timeout));
      sock_addr.sin_family := AF_INET;
      sock_addr.sin_addr.s_addr := -1; // inet_addr(PChar('255.255.255.255'))
      sock_addr.sin_port := htons(1434);

      buf := 3; //ping
      sendto(FSd, buf, 1, 0, sock_addr, sizeof(sock_addr));

      while True do begin
        Count := recv(FSd, Buffer, sizeof(Buffer), 0);
        if Count <= 0 then
          Break;
          
        SetLength(AnsiStr, Count - 3);
        Move(Buffer[3], AnsiStr[1], Length(AnsiStr));
        str := string(AnsiStr);
        
        pLexem := Pos('ServerName', str);
        while pLexem > 0 do begin
          p1 := PosEx(';', str, pLexem + 10{Length('ServerName')});
          if p1 = 0 then
            raise Exception.Create(SInvalidValue);
          p2 := PosEx(';', str, p1 + 1);
          if p2 = 0 then
            raise Exception.Create(SInvalidValue);
          ServerName := Copy(str, p1 + 1, p2 - p1 - 1);
          
          pLexem := PosEx('InstanceName', str, p2 + 1);
          p1 := PosEx(';', str, pLexem + 12{Length('InstanceName')});
          if p1 = 0 then
            raise Exception.Create(SInvalidValue);
          p2 := PosEx(';', str, p1 + 1);
          if p2 = 0 then
            raise Exception.Create(SInvalidValue);
          InstanceName := Copy(str, p1 + 1, p2 - p1 - 1);
          if AnsiUpperCase(InstanceName) <> 'MSSQLSERVER' then
            ServerName := ServerName + '\' + InstanceName;

          ServerList.Add(ServerName);
          pLexem := PosEx('ServerName', str, p2 + 1);
        end;
      end;

    finally
      closesocket(FSd);
    end;
  end;
{$ENDIF}

  procedure OrderList;
  var
    i: integer;
  begin
    ServerList.Sort;
    //delete duplicates
    i := 0;
    while i < ServerList.Count - 1 do
      if AnsiUpperCase(ServerList[i]) = AnsiUpperCase(ServerList[i + 1]) then
        ServerList.Delete(i + 1)
      else
        inc(i);
  end;

begin
  List.Clear;

  if FProvider = prCompact then
    Exit;

  ServerList := _TStringList.Create;
  try
    GetServerListByOleDBEnum;
    GetServerListByReg;
  {$IFDEF WIN32}
    GetServerListBySocket;
  {$ENDIF}
    OrderList;
    List.AddStrings(ServerList);
  finally
    ServerList.Free;
  end;

end;

{ TCustomMSDumpProcessor }

{$IFNDEF CLR}
type
  _StringBuilderHelper = class (_StringBuilder);

procedure _BinToHex(Buffer: PChar; Text: _PChar; BufSize: Integer);
{$IFDEF CRUNICODE}
const
  Convert: array[0..15] of WideChar = WideString('0123456789ABCDEF');
var
  I: Integer;
begin
  for I := 0 to BufSize - 1 do
  begin
    Text[0] := Convert[Byte(Buffer[I]) shr 4];
    Text[1] := Convert[Byte(Buffer[I]) and $F];
    Inc(Text, 2);
  end;
{$ELSE}
begin
  BinToHex(Buffer, Text, BufSize);
{$ENDIF}
end;
{$ENDIF}

constructor TCustomMSDumpProcessor.Create(Owner: TDADump);
begin
  inherited;
end;

function TCustomMSDumpProcessor.SetProp(Prop: integer; const Value: variant): boolean;
begin
  Result := True;
  case Prop of
    prIdentityInsert:
      FIdentityInsert := Value;
    prDisableConstraints:
      FDisableConstraints := Value;
  else
    Assert(False, IntToStr(Prop));
    Result := False;
  end;
end;

function TCustomMSDumpProcessor.GetProp(Prop: integer; var Value: variant): boolean;
begin
  Result := True;
  case Prop of
    prIdentityInsert:
      Value := FIdentityInsert;
    prDisableConstraints:
      Value := FDisableConstraints;
  else
    Assert(False, IntToStr(Prop));
    Result := False;
  end;
end;

function TCustomMSDumpProcessor.CreateQuery: TCustomDADataSet;
begin
  Result := GetConnection.CreateDataSet;
  TDBAccessUtils.CheckConnection(Result);
  Result.ReadOnly := False;
  //Result.FetchAll := True;
  Result.UniDirectional := True;
  Result.Options.SetFieldsReadOnly := True;
  Result.Options.QueryRecCount := False;
end;

procedure TCustomMSDumpProcessor.BackupObjects(const Query: _string);
var
  TableCount: integer;
  AlterTableSQL: _TStringList;

  procedure VarToMSSQL(Field: TField; FieldDesc: TOLEDBFieldDesc; sb: _StringBuilder);
  var
    dt: TDateTime;
    Blob: TBlob;
    Piece: PPieceHeader;
    Value: Variant;
    Data: TBytes;
  {$IFNDEF VER6P}
    pValueData: PVarData;
  {$ENDIF}
  {$IFDEF CLR}
    Bytes: TBytes;
  {$ELSE}
    sbOffset: integer;
  {$ENDIF}
  begin
    SetLength(Data, 0); // To avoid Hint from compiler
    case FieldDesc.DataType of
      dtBoolean:
        sb.Append(BoolToStr(Field.AsBoolean));
      dtUnknown, dtString, dtMemo, dtWideMemo, dtExtString, dtWideString, dtExtWideString, dtGuid{$IFDEF VER5P}, dtVariant{$ENDIF}:
        sb.Append(QuotedStr(Field.AsString));
      dtInt8, dtInt16, dtInt32, dtInt64,
      dtUInt16, dtUInt32: begin
      {$IFNDEF VER6P}
        Value := Field.AsVariant;
        pValueData := @TVarData(Value);
        if pValueData.VType = varDecimal then
          sb.Append(IntToStr(PInt64(@pValueData.VInteger)^))
        else
      {$ENDIF}
          sb.Append(Field.AsString);
      end;
      dtFloat, dtCurrency:
        sb.Append(ChangeDecimalSeparator(Field.AsString));
      dtDate, dtTime, dtDateTime: begin
        dt := Field.AsDateTime;
        if dt = 0 then
          sb.Append(QuotedStr('1900-01-01 00:00:00'))
        else
          sb.Append(QuotedStr(FormatDateTime('YYYY-MM-DD HH:NN:SS', dt)));
      end;
      dtBlob, dtMSXML: begin
        sb.Append('0x');
        Blob := FQuery.GetBlob(Field.FieldName);
        Piece := Blob.FirstPiece;

      {$IFDEF CLR}
        while IntPtr(Piece) <> nil do begin
          SetLength(Data, Piece.Used);
          Marshal.Copy(PtrOffset(Piece, sizeof(TPieceHeader)), Data, 0, Piece.Used);
          SetLength(Bytes, Length(Data) * 2);
          BinToHex(Data, 0, Bytes, 0, Length(Data));
          sb.Append(Encoding.Default.GetString(Bytes));
          Piece := Piece.Next;
        end;
      {$ELSE}
        sbOffset := sb.Length;
        sb.Length := sb.Length + Integer(Blob.Size) * 2;
        while Piece <> nil do begin
          _BinToHex(pchar(PtrOffset(Piece, sizeof(TPieceHeader))),
            _pchar(@_StringBuilderHelper(sb).FString[sbOffset]), Piece.Used);
          sbOffset := sbOffset + Integer(Piece.Used) * 2;
          Piece := Piece.Next;
        end;
      {$ENDIF}
      end;
      dtBytes, dtVarBytes, dtExtVarBytes: begin
        sb.Append('0x');
        Value := Field.Value;
        Data := Value;
      {$IFDEF CLR}
        SetLength(Bytes, Length(Data) * 2);
        BinToHex(Data, 0, Bytes, 0, Length(Data));
        sb.Append(Encoding.Default.GetString(Bytes));
      {$ELSE}
        sbOffset := sb.Length;
        sb.Length := sb.Length + Length(Data) * 2;
        _BinToHex(@Data[0],
          _pchar(@_StringBuilderHelper(sb).FString[sbOffset]), Length(Data));
      {$ENDIF}
      end;
      else
        Assert(False, 'Unknown datatype (' + IntToStr(FieldDesc.DataType) + ')');
    end;
  end;

  procedure BackupTablesAndData;

    function FieldIsIdentity(Field: TField): boolean;
    var
      FieldDesc: TOLEDBFieldDesc;
    begin
      FieldDesc := FQuery.GetFieldDesc(Field) as TOLEDBFieldDesc;
      Result := FieldDesc.IsAutoIncrement;
    end;

    procedure BackupTable(TableName: _string; TableNum: integer);
    var
      KeyAndDataFields: TKeyAndDataFields;

      procedure GetCurrentRow(sb: _StringBuilder);
      var
        sbOldLen: integer;

        procedure ProcessField(FieldDesc: TOLEDBFieldDesc);
        var
          Value: Variant;
          Field: TField;
        begin
          if sbOldLen <> sb.Length then
            sb.Append(', ');

          Field := FQuery.GetField(FieldDesc);
          Value := Field.AsVariant;
          if VarIsEmpty(Value) or VarIsNull(Value) or Field.IsNull then
            sb.Append('NULL')
          else
            VarToMSSQL(Field, FieldDesc, sb);
        end;

      var
        i: integer;
      begin
        sbOldLen := sb.Length;

        if Length(KeyAndDataFields.DataFieldDescs) = 0 then begin
          for i := 0 to FQuery.FieldCount - 1 do
            if not (FieldIsIdentity(FQuery.Fields[i]) and not FIdentityInsert) then
              ProcessField(TOLEDBFieldDesc(FQuery.GetFieldDesc(FQuery.Fields[i])));
        end
        else
        begin
          if (TDBAccessUtils.GetIdentityField(FQuery) <> nil) and FIdentityInsert then
            ProcessField(TOLEDBFieldDesc(TDBAccessUtils.GetIdentityField(FQuery)));
          for i := 0 to Length(KeyAndDataFields.DataFieldDescs) - 1 do
            ProcessField(TOLEDBFieldDesc(KeyAndDataFields.DataFieldDescs[i]));
        end;
      end;

    var
      RecordCount: integer;
      InsHeader: _string;
      FieldList: _string;
      i: integer;
      SQLSelect1: _string;
      sb: _StringBuilder;
      IdentityInsertUsed: Boolean;

    begin
      FieldList := '';
      if Query = '' then
        SQLSelect1 := 'SELECT * FROM ' + TableName
      else
        SQLSelect1 := Query;

      FQuery.SQL.Text := SQLSelect1;
      FQuery.AddWhere('0=1');
      try
        FQuery.Open;

        if (TableName = '')
          and (TDBAccessUtils.GetTablesInfo(FQuery).Count > 0) then
          TableName := TDBAccessUtils.GetTablesInfo(FQuery)[0].TableName;

        if (TDBAccessUtils.GetIdentityField(FQuery) <> nil) and FIdentityInsert then
          FieldList := QuoteName(TDBAccessUtils.GetIdentityField(FQuery).Name);

        TDBAccessUtils.GetKeyAndDataFields(FQuery, KeyAndDataFields, False);
        for i := 0 to Length(KeyAndDataFields.DataFieldDescs) - 1 do
          if not (TOLEDBFieldDesc(KeyAndDataFields.DataFieldDescs[i]).IsAutoIncrement and not FIdentityInsert) then
            if FieldList = '' then
              FieldList := QuoteName(KeyAndDataFields.DataFieldDescs[i].Name)
            else
              FieldList := FieldList + ', ' + QuoteName(KeyAndDataFields.DataFieldDescs[i].Name);
      finally
        FQuery.Close;
      end;

      if True {(doData in Objects)} then begin
        if FOwner.Options.GenerateHeader then
          AddLineToSQL(SBHTableData, [TableName]);

        if FOwner.Options.AddDrop {and not (doTables in Objects)} then
          Add('TRUNCATE TABLE ' + TableName + ';');

        FQuery.SQL.Text := 'SELECT COUNT(*) FROM ' + TableName;
        FQuery.Execute;
        RecordCount := FQuery.Fields[0].AsInteger;

        DoBackupProgress(TableName, TableNum, TableCount, 0);

        IdentityInsertUsed := False;

        if RecordCount > 0 then begin
          if FieldList = '' then
            InsHeader := TableName
          else
            InsHeader := TableName + '(' + FieldList + ')';

          InsHeader := 'INSERT INTO ' + InsHeader + ' VALUES';

          FQuery.SQL.Text := SQLSelect1;
          FQuery.Open;
          if FieldList <> '' then
            TDBAccessUtils.GetKeyAndDataFields(FQuery, KeyAndDataFields, False);

          if FIdentityInsert then
            for i := 0 to FQuery.FieldCount - 1 do
              if FieldIsIdentity(FQuery.Fields[i]) then begin
                Add(_Format('SET IDENTITY_INSERT %s ON;', [TableName]));
                IdentityInsertUsed := True;
                break;
              end;

          sb := _StringBuilder.Create;
          try
            while not FQuery.Eof do begin
              DoBackupProgress(TableName, TableNum, TableCount, Trunc(FQuery.RecNo * 100 / RecordCount));

              sb.Length := 0;
              sb.Append(InsHeader);
              sb.Append(' (');
              GetCurrentRow(sb);
              sb.Append(');');
              Add(sb.ToString);
              FQuery.Next;
            end;
          finally
            sb.Free;
          end;
        end;
        if IdentityInsertUsed then
          Add(_Format('SET IDENTITY_INSERT %s OFF;', [TableName]));
      end;
    end;

    procedure BackupConstraint(const TableName, Schema: _string);
    const
      NO_ACTION = 'NO ACTION';
    var
      ConstraintName: _string;
      Columns, RefTableName, RefColumns: _string;
      UpdateRule, DeleteRule: _string;
      Replication: _string;
      SQL: _string;
    begin
      FQuery.ParamByName('Table_Schema').AsString := Schema;
      FQuery.ParamByName('Table_Name').AsString := TableName;
      FQuery.Open;

      while not FQuery.Eof do begin
        ConstraintName := FQuery.FieldByName('CONSTRAINT_NAME').AsString;
        UpdateRule := FQuery.FieldByName('UPDATE_RULE').AsString;
        DeleteRule := FQuery.FieldByName('DELETE_RULE').AsString;
        RefTableName := QuoteName(FQuery.FieldByName('RK_TABLE_SCHEMA').AsString) +
          '.' + QuoteName(FQuery.FieldByName('RK_TABLE_NAME').AsString);
        Columns := '';
        RefColumns := '';
        if FQuery.FieldByName('is_not_for_replication').AsBoolean then
          Replication := ' NOT FOR REPLICATION;'
        else
          Replication := ';';

        repeat
          if Columns <> '' then
            Columns := Columns + ', ';
          Columns := Columns + QuoteName(FQuery.FieldByName('FK_COLUMN_NAME').AsString);

          if RefColumns <> '' then
            RefColumns := RefColumns + ', ';
          RefColumns := RefColumns + QuoteName(FQuery.FieldByName('RK_COLUMN_NAME').AsString);

          FQuery.Next;
        until FQuery.Eof or (ConstraintName <> FQuery.FieldByName('CONSTRAINT_NAME').AsString);

        if FOwner.Options.GenerateHeader and (AlterTableSQL.Count = 0) then
          AddLineToSQL(SBHDropConstraints);

        SQL := 'ALTER TABLE ' + QuoteName(Schema) + '.' + QuoteName(TableName) +
          ' DROP CONSTRAINT ' + QuoteName(ConstraintName) + ';';
        AddLineToSQL(SQL);
        Add('');

        SQL := 'ALTER TABLE ' + QuoteName(Schema) + '.' + QuoteName(TableName) +
          ' WITH CHECK ADD CONSTRAINT ' + QuoteName(ConstraintName) +
          ' FOREIGN KEY(' + Columns + ')' + SLLineSeparator +
          'REFERENCES ' + RefTableName + ' (' + RefColumns + ')';

        if (UpdateRule <> NO_ACTION) or (DeleteRule <> NO_ACTION) then
          SQL := SQL + SLLineSeparator +
            'ON UPDATE ' + UpdateRule + ' ON DELETE ' + DeleteRule;
        SQL := SQL + Replication;

        AlterTableSQL.Add(SQL);
        AlterTableSQL.Add('');
      end;

      FQuery.Close;
    end;

  const
    QUERY_CONSTRAINT =
      'SELECT RC.CONSTRAINT_NAME, RC.UPDATE_RULE, RC.DELETE_RULE,' + LineSeparator +
      '  FK.TABLE_SCHEMA FK_TABLE_SCHEMA, FK.TABLE_NAME FK_TABLE_NAME, FK.COLUMN_NAME FK_COLUMN_NAME,' + LineSeparator +
      '  RK.TABLE_SCHEMA RK_TABLE_SCHEMA, RK.TABLE_NAME RK_TABLE_NAME, RK.COLUMN_NAME RK_COLUMN_NAME,' + LineSeparator +
      '  sys_fk.is_not_for_replication' + LineSeparator +
      'FROM INFORMATION_SCHEMA.REFERENTIAL_CONSTRAINTS RC LEFT JOIN' + LineSeparator +
      '  INFORMATION_SCHEMA.KEY_COLUMN_USAGE FK ON RC.CONSTRAINT_NAME = FK.CONSTRAINT_NAME LEFT JOIN' + LineSeparator +
      '  INFORMATION_SCHEMA.KEY_COLUMN_USAGE RK ON (RC.UNIQUE_CONSTRAINT_NAME = RK.CONSTRAINT_NAME AND FK.ORDINAL_POSITION = RK.ORDINAL_POSITION) LEFT JOIN' + LineSeparator +
      '  sys.foreign_keys sys_fk ON RC.CONSTRAINT_NAME = sys_fk.name' + LineSeparator +
      'WHERE FK.TABLE_SCHEMA = :Table_Schema' + LineSeparator +
      '  AND FK.TABLE_NAME = :Table_Name' + LineSeparator +
      'ORDER BY RC.CONSTRAINT_NAME, FK.ORDINAL_POSITION';

  var
    i: integer;
    TablesList: _TStringList;
    TableName, Db, Schema: _string;
    ExactNames: boolean;

  begin
    if Query = '' then begin
      TablesList := nil;
      try
        TablesList := _TStringList.Create;
        AlterTableSQL := _TStringList.Create;

        if GetTables.Count = 0 then begin
          GetConnection.GetTableNames(TablesList);
          ExactNames := True;
        end
        else begin
          TablesList.Assign(GetTables);
          ExactNames := False;
        end;

        if FDisableConstraints then begin
          FQuery.SQL.Text := QUERY_CONSTRAINT;
          FQuery.Prepare;

          for i := 0 to TablesList.Count - 1 do begin
            if ExactNames then begin
              TableName := TablesList[i];
              Schema := '';
            end
            else begin
              OLEDBSQLInfo.SplitObjectName(TablesList[i], Db, Schema, TableName);
              Schema := SQLInfo.NormalizeName(Schema, False, True);
              TableName := SQLInfo.NormalizeName(TableName, False, True);
            end;

            if Schema = '' then
              Schema := 'dbo';

            BackupConstraint(TableName, Schema);
          end;
        end;

        for i := 0 to TablesList.Count - 1 do begin
          if ExactNames then
            TableName := QuoteName(TablesList[i])
          else
            TableName := SQLInfo.NormalizeName(TablesList[i], FOwner.Options.QuoteNames);

          DoBackupProgress(TableName, i, TablesList.Count, 0);

          TableCount := TablesList.Count;
          BackupTable(TableName, i);
          Add('');
        end;

        if FOwner.Options.GenerateHeader and (AlterTableSQL.Count > 0) then
          AddLineToSQL(SBHAddConstraints);
        Add(AlterTableSQL);
      finally
        TablesList.Free;
        AlterTableSQL.Free;
      end;
    end
    else
    begin
      if GetTables.Count = 1 then
        TableName := SQLInfo.NormalizeName(GetTables[0], FOwner.Options.QuoteNames)
      else
        TableName := '';

      DoBackupProgress(TableName, 0, 1, 0);

      TableCount := 1;
      BackupTable(TableName, 0);
      Add('');
    end;
  end;

begin
  CheckQuery;
  //if doData in Objects then
    BackupTablesAndData;
end;

end.
