
{$IFNDEF CLR}
{$I Dac.inc}

unit DBToolsClientImp;
{$ENDIF}

interface

{$IFDEF DBTOOLS}
uses
{$IFDEF WIN32_64}
  CLRClasses,
  VarUtils,
  DesignIntf,
  ToolsApi,
{$ELSE}
  System.Runtime.InteropServices,
  Borland.Vcl.Design.DesignIntf,
  Borland.Studio.ToolsAPI,
  System.Reflection,
  System.Diagnostics,
  CoreLab.DbTools,
  System.Text,
{$ENDIF}
  ActiveX,
  CRTypes, CRDesignUtils, DADesignUtils,
  Windows, Classes, Controls, StdCtrls, ExtCtrls, DB, DBAccess,
  DBToolsIntf, DBToolsClient, ActnMan, ActnList, Graphics, Menus, Forms,
  Messages;

type
  TSqlSource = class;

  TDBToolsService = class(TCustomDBToolsService)
  protected
    FUsedConnectionStrList: TStringList;
    FUsedConnectionCompareFlags: array of TCompareFlag;

    FConnectionStrList: TStringList;
    FConnectionsList: TStringList;
    FDefaultConnectionList: TStringList;
    FSqlService: IDbToolsService;
    FSqlEditors: array of ISqlEditor;
    FParamTypeMaps: array of TParamTypeMap;

    function GetDACSqlEditorFrameClass: TCustomDACSqlEditorFrameClass; override;
    function SqlSourceClass: TCustomSqlSourceClass; override;

    function GetSqlEditor: ISqlEditor;
    procedure ReturnSqlEditor(ASqlEditor: ISqlEditor);
    function GetSqlSource(Component: TComponent; Designer: IDesigner; SqlTextPrefix: string = ''): TSqlSource;
  {$IFDEF WIN32_64}
    class function AccessData(const V: PSafeArray): pointer;
    class procedure UnaccessData(const V: PSafeArray);
    class function DataHigh(const V: PSafeArray): integer;
  {$ENDIF}
    class function GetConnectionParamStr(const ParamName, ParamValue: string): string;
    function GetConnectionValueStr(ConnectionName: string): string;
    procedure ConnStrToList(ConnStr: string; const ConnList: TStrings);
    procedure CheckConnection(const Component: TComponent);

    procedure BeginConnectionStrGetting(const ConnectionStrList: TStringList);

    procedure DesignerClosing(DesignerName: string); override;
    class function GetNamespace: string; override;
    class function UseNewRegPath: boolean; override;    
  public
    constructor Create(ADADesignUtils: TDADesignUtilsClass; ASqlService: {$IFDEF WIN32_64}IUnknown{$ELSE}TObject{$ENDIF};ADefaultConnectionStr: string); override;
    destructor Destroy; override;
    function DBToolsTypeToDataType(AType: integer; OldType: TFieldType): TFieldType;
    function DataTypeToDBToolsType(AType: TFieldType): integer;


    function GetNativeConnectionString(const Component: TComponent): string;
    function GetConnectionString(const Component: TComponent): TString;
    function GetConnectionStringObjectTypeAndFullName(const Component: TComponent; out ConnectionString, ObjectType, FullName: TString): boolean;

    procedure GetConnections(NameList: TStrings; Condition: string = ''); override;
    function FindConnectionName(AConnection: TCustomDAConnection): string; override;//Call GetConnections
    function GetConnectionStrList(ConnectionName: string): TStringList; override;//before!

    procedure FindInDatabaseExplorer; override;
    procedure EditDatabaseObject; override;
    procedure ExecuteSql(Debug: boolean); override;
    procedure Compile(Debug: boolean); override;
    procedure RetrieveData(AsDocument: boolean); override;
    procedure EditSql(AsQuery: boolean); override;

    procedure AddParamTypeMap(ADACType: TFieldType; ADBToolsType: integer); override;

    procedure PutConnectionParam(const ConnectionParam: string; const CompareFlag: TCompareFlag = cfNormal); override;
    procedure SkipConnectionParams(const Count: integer); override;

     property SqlService: IDbToolsService read FSqlService;
  end;

  TDACSqlEditorFrame = class(TCustomDACSqlEditorFrame)
  private
    FDBToolsService: TDBToolsService;
    FSqlEditors: array[TStatementType] of ISqlEditor;
    FStatementType: TStatementType;
    FComponent: TComponent;
    FLastConnectionString: string;
    FInInit: boolean;

    function GetSqlEditor: ISqlEditor;
    procedure InternalResize;
  protected
    procedure SetStatementType(const Value: TStatementType); override;
    function GetText: _string; override;
    procedure SetText(const Value: _string); override;
    procedure SetReadOnly(Value: boolean); override;
  
    procedure Resize; override;
    function GetSqlEditorHandle: HWND;
    procedure CheckModified; override;
    procedure WndProc(var Message: TMessage); override;

    procedure EndInit; override;
    procedure CheckConnectionChange; override;        
  public
    constructor Create(AOwner: TComponent; Component: TComponent; DBToolsService: TCustomDBToolsService); override;
    destructor Destroy; override;
    procedure SetFocus; override;

    property SqlEditor: ISqlEditor read GetSqlEditor;
  end;

  TSourceNotifier = class(TCustomSourceNotifier)
    FSqlSourceNotifier: ISqlSourceNotifier;

    procedure OnSqlSourceDeleted; override;
  end;

  TSqlSource = class(TCustomSQLSource, ISqlSource)
  protected
    FParameterCount: integer;
    FParameterSetted: array of boolean;
    FDesignerName: string;
    FLastName: string;
    FComponentSQL: _string;
    FStatementType: TStatementType;
    FDBToolsService: TDBToolsService;
    FLastConnection: TCustomDAConnection;
    FLastConnectionString: string;

    function GetParams: TDAParams;
    function GetSqlText: _string;
    procedure SetSqlText(Value: _string);

    function Get_Name: TString; {$IFDEF WIN32_64}stdcall;{$ENDIF}
    function Get_ConnectionString: TString; {$IFDEF WIN32_64}stdcall;{$ENDIF}
    function Get_DesignerName: TString; {$IFDEF WIN32_64}stdcall;{$ENDIF}
    function Get_Sql: TString; {$IFDEF WIN32_64}stdcall;{$ENDIF}
    procedure Set_Sql({$IFDEF WIN32_64}const {$ENDIF}Param1: TString); {$IFDEF WIN32_64}stdcall;{$ENDIF}
    procedure GetParameter({$IFDEF WIN32_64}const {$ENDIF}Index: integer; out Info: CommandParameterInfo); {$IFDEF WIN32_64}stdcall;{$ENDIF}
    procedure SetParameter({$IFDEF WIN32_64}const {$ENDIF}Index: integer; Info: CommandParameterInfo); {$IFDEF WIN32_64}stdcall;{$ENDIF}
    procedure Set_ParameterCount({$IFDEF WIN32_64}const {$ENDIF}Value: integer); {$IFDEF WIN32_64}stdcall;{$ENDIF}
    function Get_ParameterCount: integer; {$IFDEF WIN32_64}stdcall;{$ENDIF}

    procedure Close; {$IFDEF WIN32_64}stdcall;{$ENDIF}

    function GetSourceNotifierClass: TCustomSourceNotifierClass; override; 
    procedure FreeSourceNotifier; override;
    function GetDBToolsService: TCustomDBToolsService; override;

    property Params: TDAParams read GetParams;
  public
    constructor Create(DBToolsService: TDBToolsService; Component: TComponent; Designer: IDesigner); reintroduce;
    destructor Destroy; override;
    procedure CheckRename; override;
    procedure CheckConnectionChange(InternalCheck: boolean); override;
    procedure CheckChanges; override;
  end;

{$ENDIF DBTOOLS}

implementation

{$IFDEF DBTOOLS}
uses
  DADesign, Registry, Download, DAConsts,
  Variants, SysUtils, ComObj, TypInfo;

const
  SConnectionName = '_ConnName_';
  SConnStrError = 'ConnectionString error';
  SCompilePrefix = 'compile';
  SEditPrefix = 'edit';

{$IFDEF WIN32_64}
  function ToWideChar(s: WideString): PWideChar;
  begin
    if s = '' then
      Result := nil
    else
      Result := SysAllocString(PWideChar(s));
  end;
{$ENDIF}    

{ TSqlSource }

procedure TSqlSource.Close;
begin
  Assert(DBTools.DesignNotification <> nil);
  DBTools.DesignNotification.SqlSourceList.Delete(FComponent, False);
end;

function TSqlSource.GetSourceNotifierClass: TCustomSourceNotifierClass;
begin
  Result := TSourceNotifier;
end;

procedure TSqlSource.FreeSourceNotifier;
begin
  TSourceNotifier(FSqlSourceNotifier).FSqlSourceNotifier := nil;
end;

function TSqlSource.GetDBToolsService: TCustomDBToolsService;
begin
  Result := FDBToolsService;
end;

constructor TSqlSource.Create(DBToolsService: TDBToolsService; Component: TComponent; Designer: IDesigner);
begin
  inherited Create;
  Assert(DBToolsService <> nil);
  FComponent := Component;
  FDBToolsService := DBToolsService;
  FStatementType := stQuery;
  FLastName := Get_Name;
  FComponentSQL := GetSqlText;
  FDesigner := Designer;
  FDesignerName := DBTools.GetDesignerName(FDesigner);
  FLastConnection := FDBToolsService.GetConnection(Component);
  FLastConnectionString := FDBToolsService.GetNativeConnectionString(FLastConnection);
end;

destructor TSqlSource.Destroy;
begin
  inherited;
end;

function TSqlSource.GetParams: TDAParams;
begin
  with FDBToolsService.DADesignUtils do
    if HasParams(FComponent) then
      Result := GetParams(FComponent)
    else
      Result := nil;
end;

function TSqlSource.Get_ConnectionString: TString;
begin
  Result := {$IFDEF WIN32_64}ToWideChar({$ENDIF}
    FLastConnectionString
  {$IFDEF WIN32_64}){$ENDIF};
end;

function TSqlSource.Get_DesignerName: TString;
begin
  Result := {$IFDEF WIN32_64}ToWideChar(FDesignerName){$ELSE}FDesignerName{$ENDIF};
end;

function TSqlSource.Get_Name: TString;
begin
  Result := {$IFDEF WIN32_64}ToWideChar({$ENDIF}
    FComponent.Owner.Name + '-' + FComponent.Name
  {$IFDEF WIN32_64}){$ENDIF};
end;

function TSqlSource.GetSqlText: _string;
var
  SQL: _TStrings;
  Macros: TMacros;
  NewMacros: TDesignMacros;
begin
  with FDBToolsService.DADesignUtils do
    if IsStoredProc(FComponent) then begin
      Result := Trim(GetFullName(FComponent));
      if Result <> '' then
        Result := GetObjectType(FComponent) + ':' + Result;
    end
    else begin
      SQL := GetSQL(FComponent);
      Macros := GetMacros(FComponent);
      NewMacros := TDesignMacros.Create(nil);
      try
        NewMacros.SetParserClass(TDBAccessUtils.GetParserClass(Macros));
        NewMacros.Assign(Macros);
        Result := SQL.Text;
        NewMacros.Expand(Result);        
      finally
        NewMacros.Free;
      end;
    end;
  if (FSqlTextPrefix <> '') and (FSqlTextPrefix <> SEditPrefix) then
    Result := FSqlTextPrefix + ':' + Result;
end;

procedure TSqlSource.SetSqlText(Value: _string);
var
  SQL: _TStrings;
  Macros: TMacros;
  NewMacros: TDesignMacros;
begin
  with FDBToolsService.DADesignUtils do
    if not IsStoredProc(FComponent) then begin
      SQL := GetSQL(FComponent);
      Macros := GetMacros(FComponent);
      NewMacros := TDesignMacros.Create(nil);
      try
        NewMacros.SetParserClass(TDBAccessUtils.GetParserClass(Macros));
        NewMacros.Scan(Value);
        SQL.Text := Value;
        Macros.Assign(NewMacros);
      finally
        NewMacros.Free;
      end;
    end;
end;

function TSqlSource.Get_Sql: TString;
begin
  Result := {$IFDEF WIN32_64}ToWideChar({$ENDIF}
    WideString(GetSqlText)
  {$IFDEF WIN32_64}){$ENDIF};
end;

procedure TSqlSource.Set_Sql({$IFDEF WIN32_64}const {$ENDIF}Param1: TString);
begin
  SetSqlText(Param1);
end;

procedure TSqlSource.GetParameter({$IFDEF WIN32_64}const {$ENDIF}Index: integer; out Info: CommandParameterInfo);
begin
  Assert(Index < Get_ParameterCount);
  Info.Name := {$IFDEF WIN32_64}ToWideChar({$ENDIF}Params[Index].Name
    {$IFDEF WIN32_64}){$ENDIF};
  Info.DataType := FDBToolsService.DataTypeToDBToolsType(Params[Index].DataType);
  Info.Value := Params[Index].Value;
  case Params[Index].ParamType of
    ptUnknown,
    ptInput:
      Info.ParameterType := ParameterType_Input;
    ptOutput:
      Info.ParameterType := ParameterType_Output;
    ptInputOutput:
      Info.ParameterType := ParameterType_InputOutput;
    ptResult:
      Info.ParameterType := ParameterType_ReturnValue;
  end;
end;

function TSqlSource.Get_ParameterCount: integer;
begin
  if (Params <> nil) and (Params.Count > FParameterCount) then
    Set_ParameterCount(Params.Count);
  Result := FParameterCount;
end;

procedure TSqlSource.Set_ParameterCount({$IFDEF WIN32_64}const {$ENDIF}Value: integer);
var
  i, n: integer;
begin
  if Params = nil then
    Exit;
  FParameterCount := Value;
  SetLength(FParameterSetted, Value);
  n := Params.Count;
  for i := 0 to Value - 1 do begin
    if i >= n then
      Params.Add;
    FParameterSetted[i] := False;
  end;
end;

procedure TSqlSource.SetParameter({$IFDEF WIN32_64}const {$ENDIF}Index: integer; Info: CommandParameterInfo);
var
  i, j: integer;
  TempParam: TDAParam;
begin
  Assert(Index < Get_ParameterCount);
  for i := 0 to Params.Count - 1 do
    if Params[i].Name = Info.Name then begin
      if i <> Index then begin
        TempParam := Params[Index];
        Params[Index] := Params[i];
        Params[i] := TempParam;
      end;
      Break;
    end;
  with TDAParam(Params[Index]) do begin
    Name := Info.Name;
    DataType := FDBToolsService.DBToolsTypeToDataType(Info.DataType, DataType);
    Value := Variant(Info.Value);
    case Info.ParameterType of
      ParameterType_Input:
        ParamType := ptInput;
      ParameterType_Output:
        ParamType := ptOutput;
      ParameterType_InputOutput:
        ParamType := ptInputOutput;
      ParameterType_ReturnValue:
        ParamType := ptResult;
    end;
  end;
  for i := FParameterCount - 1 downto 0 do
    if not FParameterSetted[i] then
      Break
    else
      if i = 0 then
        for j := Params.Count - 1 downto FParameterCount do
          Params.Delete(j);
end;

procedure TSqlSource.CheckRename;
begin
  if (FLastName <> Get_Name) then begin
    if TSourceNotifier(FSqlSourceNotifier).FSqlSourceNotifier <> nil then
      TSourceNotifier(FSqlSourceNotifier).FSqlSourceNotifier.OnSqlSourceRenamed(Get_Name);
    FLastName := Get_Name;
  end;
end;

procedure TSqlSource.CheckConnectionChange(InternalCheck: boolean);
var
  NewConnection: TCustomDAConnection;
  NewConnectionString: string;
begin
  NewConnection := FDBToolsService.GetConnection(FComponent);
  if InternalCheck and (FLastConnection = NewConnection) then
    Exit;
  NewConnectionString := FDBToolsService.GetNativeConnectionString(NewConnection);
  if NewConnectionString <> FLastConnectionString then begin
    FLastConnectionString := NewConnectionString;
    if not InternalCheck and (FSqlSourceNotifier <> nil) then
      TSourceNotifier(FSqlSourceNotifier).FSqlSourceNotifier.OnSqlSourceChanged;
  end;
end;

procedure TSqlSource.CheckChanges;
begin
  CheckRename;
  CheckConnectionChange(True);
  if GetSqlText <> FComponentSQL then begin
    if FSqlSourceNotifier <> nil then
      TSourceNotifier(FSqlSourceNotifier).FSqlSourceNotifier.OnSqlSourceChanged;
    FComponentSQL := GetSqlText;
  end;
end;

{ TDACSqlEditorFrame }

procedure TDACSqlEditorFrame.CheckModified;
begin
  if (SqlEditor <> nil) and (SqlEditor.Modified) then begin
    if Assigned(FOnChange) then
      FOnChange(Self);
    if Assigned(FOnExit) then
      FOnExit(Self);
    SqlEditor.Modified := False;
  end;
end;

constructor TDACSqlEditorFrame.Create(AOwner: TComponent; Component: TComponent;
  DBToolsService: TCustomDBToolsService);
begin
  inherited Create(AOwner, Component, DBToolsService);

  FInInit := True;
  FComponent := Component;
  FDBToolsService := TDBToolsService(DBToolsService);
  BevelOuter := bvNone;
  FStatementType := stQuery;
  TabStop := True;

  DBTools.AddFrame(Self);
end;

destructor TDACSqlEditorFrame.Destroy;
var
  st: TStatementType;
begin
  for st := Low(TStatementType) to High(TStatementType) do
    if FSqlEditors[st] <> nil then
      FDBToolsService.ReturnSqlEditor(FSqlEditors[st]);

  DBTools.RemoveFrame(Self);    

  inherited;
end;

procedure TDACSqlEditorFrame.EndInit;
begin
  FInInit := False;
end;

procedure TDACSqlEditorFrame.InternalResize;
begin
  if SqlEditor <> nil then
    Windows.SetWindowPos(GetSqlEditorHandle, 0, 0, 0, ClientWidth, ClientHeight, SWP_NOZORDER or SWP_SHOWWINDOW);
end;

procedure TDACSqlEditorFrame.Resize;
begin
  InternalResize;
  inherited;
end;

procedure TDACSqlEditorFrame.SetFocus;
begin
  inherited;

  if SqlEditor <> nil then
    Windows.SetFocus(GetSqlEditorHandle);
end;

procedure TDACSqlEditorFrame.WndProc(var Message: TMessage);
begin
  if not FInInit and (Message.Msg = WM_SETFOCUS) and (SqlEditor <> nil) then
    Windows.SetFocus(GetSqlEditorHandle)
  else
    inherited;
end;

procedure TDACSqlEditorFrame.CheckConnectionChange;
var
  NewConnectionString: string;
  st: TStatementType;
begin
  Assert(SqlEditor <> nil);

  NewConnectionString := FDBToolsService.GetNativeConnectionString(FComponent);
  if NewConnectionString <> FLastConnectionString then begin
    FLastConnectionString := NewConnectionString;

    if FLastConnectionString <> '' then
      for st := Low(TStatementType) to High(TStatementType) do
        if FSqlEditors[st] <> nil then
          FSqlEditors[st].SetConnection({$IFDEF WIN32_64}ToWideChar({$ENDIF}
            FLastConnectionString
          {$IFDEF WIN32_64}){$ENDIF});
  end;
end;

function TDACSqlEditorFrame.GetSqlEditorHandle: HWND;
begin
  Assert(SqlEditor <> nil);

  Result := SqlEditor.Handle{$IFDEF CLR}.ToInt32{$ENDIF};
end;

function TDACSqlEditorFrame.GetText: _string;
begin
  if SqlEditor <> nil then
    Result := SqlEditor.Text
  else
    Result := '';
end;

procedure TDACSqlEditorFrame.SetText(const Value: _string);
begin
  if SqlEditor <> nil then
    SqlEditor.Text := {$IFDEF WIN32_64}ToWideChar(Value){$ELSE}Value{$ENDIF};
end;

procedure TDACSqlEditorFrame.SetReadOnly(Value: boolean);
var
  st: TStatementType;
begin
  if Value <> FReadOnly then begin
    FReadOnly := Value;
    for st := Low(TStatementType) to High(TStatementType) do
       if FSqlEditors[st] <> nil then
          FSqlEditors[st].ReadOnly := Value;
    SetStatementType(FStatementType);
  end;
end;

function TDACSqlEditorFrame.GetSqlEditor: ISqlEditor;
begin
  if (not ReadOnly) or (FSqlEditors[FStatementType] <> nil) then
    Result := FSqlEditors[FStatementType]
  else
    Result := FSqlEditors[stQuery];
end;

procedure TDACSqlEditorFrame.SetStatementType(const Value: TStatementType);
var
  NewStatementType: TStatementType;
  FHide, FShow, FFocused: boolean;
begin
  if FReadOnly and (FSqlEditors[Value] = nil) then
    NewStatementType := stQuery
  else
    NewStatementType := Value;
  FHide := NewStatementType <> FStatementType;
  FShow := FHide;
  if FSqlEditors[NewStatementType] = nil then
    if (FSqlEditors[stQuery] <> nil) and (FSqlEditors[stQuery].Text = '') then begin
      FSqlEditors[NewStatementType] := FSqlEditors[stQuery];
      FSqlEditors[stQuery] := nil;
      FHide := False;
      FShow := False;
    end
    else begin
      FSqlEditors[NewStatementType] := FDBToolsService.GetSqlEditor;
      if FLastConnectionString <> '' then
        FSqlEditors[NewStatementType].SetConnection({$IFDEF WIN32_64}ToWideChar({$ENDIF}
          FLastConnectionString
        {$IFDEF WIN32_64}){$ENDIF});
      FSqlEditors[NewStatementType].Text := {$IFDEF WIN32_64}nil{$ELSE}''{$ENDIF};
      FShow := True;
    end;
  if FHide and (FSqlEditors[NewStatementType] <> nil) then begin
    Windows.SetParent(GetSqlEditorHandle, MAXDWORD - 2{HWND_MESSAGE});
    FFocused := Windows.GetFocus = GetSqlEditorHandle;
  end
  else
    FFocused := False;

  FStatementType := Value;
  if FShow then begin
    Windows.SetParent(GetSqlEditorHandle, Handle);
    FSqlEditors[NewStatementType].ReadOnly := FReadOnly;
    InternalResize;
    if FFocused then
      Windows.SetFocus(GetSqlEditorHandle);
  end;
end;

{ TSourceNotifier }

procedure TSourceNotifier.OnSqlSourceDeleted;
begin
  FSqlSourceNotifier.OnSqlSourceDeleted;
end;

{ TDBToolsService }

constructor TDBToolsService.Create(ADADesignUtils: TDADesignUtilsClass; ASqlService: {$IFDEF WIN32_64}IUnknown{$ELSE}TObject{$ENDIF}; ADefaultConnectionStr: string);
begin
  inherited Create(ADADesignUtils, ASqlService, ADefaultConnectionStr);

  FConnectionStrList := TStringList.Create;
  FConnectionsList := TStringList.Create;
  FDefaultConnectionList := TStringList.Create;
  FDADesignUtils := ADADesignUtils;

{$IFDEF WIN32_64}
  ASqlService.QueryInterface(IDbToolsService, FSqlService);
{$ELSE}
  FSqlService := ASqlService as IDbToolsService;
{$ENDIF}
  ConnStrToList(ADefaultConnectionStr, FDefaultConnectionList);
end;

destructor TDBToolsService.Destroy;
begin
{$IFDEF WIN32_64}
  PInteger(@FSqlService)^ := 0; // To prevent _Release calling
{$ENDIF}
  FDefaultConnectionList.Free;
  FConnectionStrList.Free;
  FConnectionsList.Free;

  inherited;
end;

function TDBToolsService.DataTypeToDBToolsType(AType: TFieldType): integer;
var
  i: integer;
begin
  Assert(Length(FParamTypeMaps) > 0);
  Result := FParamTypeMaps[0].DBToolsType;
  for i := 0 to High(FParamTypeMaps) do
    if FParamTypeMaps[i].DACType = AType then begin
      Result := FParamTypeMaps[i].DBToolsType;
      Break;
    end;
end;

function TDBToolsService.DBToolsTypeToDataType(AType: integer; OldType: TFieldType): TFieldType;
var
  i: integer;
begin
  Result := ftUnknown;
  for i := 0 to High(FParamTypeMaps) do
    if FParamTypeMaps[i].DBToolsType = AType then begin
      if (Result = ftUnknown) or (FParamTypeMaps[i].DACType = OldType) then
        Result := FParamTypeMaps[i].DACType;
      if Result = OldType then
        Break;
    end
    else
      if Result <> ftUnknown then
        Break;
end;

procedure TDBToolsService.BeginConnectionStrGetting(const ConnectionStrList: TStringList);
begin
  FUsedConnectionStrList := ConnectionStrList;
  SetLength(FUsedConnectionCompareFlags, 0);
end;

procedure TDBToolsService.DesignerClosing(DesignerName: string);
begin
  FSqlService.DesignerClosing(DesignerName);
end;

class function TDBToolsService.GetNamespace: string;
begin
  Result := 'CoreLab.DbTools';
end;

class function TDBToolsService.UseNewRegPath: boolean;
begin
  Result := False;
end;    

procedure TDBToolsService.PutConnectionParam(const ConnectionParam: string; const CompareFlag: TCompareFlag = cfNormal);
var
  i: integer;
begin
  i := Length(FUsedConnectionCompareFlags);
  Assert (i < FDefaultConnectionList.Count);
  FUsedConnectionStrList.Values[FDefaultConnectionList.Names[i]] := ConnectionParam;
  SetLength(FUsedConnectionCompareFlags, i + 1);
  FUsedConnectionCompareFlags[i] := CompareFlag;
end;

procedure TDBToolsService.SkipConnectionParams(const Count: integer);
var
  i, j: integer;
begin
  i := Length(FUsedConnectionCompareFlags);
  Assert (i + Count <= FDefaultConnectionList.Count);
  SetLength(FUsedConnectionCompareFlags, i + Count);
  for j := i to i + Count - 1 do
    FUsedConnectionCompareFlags[j] := cfNone;
end;

class function TDBToolsService.GetConnectionParamStr(const ParamName, ParamValue: string): string;
var
  i: integer;
  QuoteChar: char;
begin
  Result := ParamName + '=';
  if Pos('''', ParamValue) > 0 then
    QuoteChar := '"'
  else
    if Pos('"', ParamValue) > 0 then
      QuoteChar := ''''
    else
      if (Pos(' ', ParamValue) > 0) or (Pos(';', ParamValue) > 0) then
        QuoteChar := '"'
      else
        QuoteChar := #0;
  if QuoteChar <> #0 then
    Result := Result + QuoteChar;
  for i := 1 to Length(ParamValue) do begin
    if (QuoteChar <> #0) and (ParamValue[i] = QuoteChar) then
      Result := Result + QuoteChar;
    Result := Result + ParamValue[i];
  end;
  if QuoteChar <> #0 then
    Result := Result + QuoteChar;
  Result := Result + ';';
end;

function TDBToolsService.GetNativeConnectionString(const Component: TComponent): string;
var
  i, n: integer;
  DefStr: string;
  Connection: TCustomDAConnection;
  ConnectionList: TStringList;
begin
  Result := '';
  Connection := GetConnection(Component);
  if Connection <> nil then begin
    ConnectionList := TStringList.Create;
    try
      BeginConnectionStrGetting(ConnectionList);
      DADesignUtils.GetDBToolsConnectionList(Connection);
      n := ConnectionList.Count - 1;
      for i := n downto 0 do begin
        DefStr := FDefaultConnectionList.Values[ConnectionList.Names[i]];
        if (DefStr <> '') and (DefStr = ConnectionList.ValueFromIndex[i]) then
          ConnectionList.Delete(i);
      end;
      for i := 0 to ConnectionList.Count - 1 do
         Result := Result + GetConnectionParamStr(ConnectionList.Names[i], ConnectionList.ValueFromIndex[i]);
    finally
      ConnectionList.Free;
    end;
  end;
end;

function TDBToolsService.GetConnectionString(const Component: TComponent): TString;
begin
  Result := {$IFDEF WIN32_64}ToWideChar({$ENDIF}
    GetNativeConnectionString(Component)
  {$IFDEF WIN32_64}){$ENDIF};
end;

function TDBToolsService.GetConnectionStringObjectTypeAndFullName(const Component: TComponent; out ConnectionString, ObjectType, FullName: TString): boolean;
begin
  ConnectionString := GetConnectionString(Component);
  Result := ConnectionString <> '';
  if Result then begin
    FullName := {$IFDEF WIN32_64}ToWideChar({$ENDIF}
      DADesignUtils.GetFullName(Component){$IFDEF WIN32_64}){$ENDIF};
    ObjectType := {$IFDEF WIN32_64}ToWideChar({$ENDIF}
      DADesignUtils.GetObjectType(Component){$IFDEF WIN32_64}){$ENDIF};
    if not(Component is TCustomDAConnection) then
      Result := Length(FullName) > 0;
  end
  else begin
    FullName := {$IFDEF WIN32_64}nil{$ELSE}''{$ENDIF};
    ObjectType := {$IFDEF WIN32_64}nil{$ELSE}''{$ENDIF};
  end;
end;

procedure TDBToolsService.GetConnections(NameList: TStrings; Condition: string = '');
var
  ConnectionInfoArray: TConnectionInfoArray;
  i: integer;
  Connection: {$IFDEF WIN32_64}PConnectionInfo{$ELSE}ConnectionInfo{$ENDIF};
begin
  ConnectionInfoArray := FSqlService.GetConnections;
  NameList.BeginUpdate;
  FConnectionsList.BeginUpdate;
{$IFDEF WIN32_64}
  Connection := AccessData(ConnectionInfoArray);
{$ENDIF}
  try
    FConnectionsList.Clear;
    NameList.Clear;
    for i := 0 to {$IFDEF WIN32_64}DataHigh{$ELSE}High{$ENDIF}(ConnectionInfoArray) do begin
    {$IFDEF CLR}
      Connection := ConnectionInfoArray[i];
    {$ENDIF}
      if (Condition = '') or (Pos(Condition, string(Connection.ConnectionString)) > 0) then begin
        NameList.Add(Connection.Name);
        FConnectionsList.Add(GetConnectionValueStr(Connection.Name) + Connection.ConnectionString);
      end;
    {$IFDEF WIN32_64}
      Inc(Connection);
    {$ENDIF}
    end;
  finally
    NameList.EndUpdate;
    FConnectionsList.EndUpdate;
  {$IFDEF WIN32_64}
    UnaccessData(ConnectionInfoArray);
  {$ENDIF}
  end;
end;

function TDBToolsService.FindConnectionName(AConnection: TCustomDAConnection): string;
var
  i, j: integer;
  AConnectionStrList: TStringList;
  Str1, Str2, DefStr: string;

  function ToCommonCase(const s: string): string;
  var
    ts: string;
  begin
    Result := s;
    if FUsedConnectionCompareFlags[j] = cfNormal then begin
      ts := Trim(Result);
      if (Length(ts) < 2) or (ts[1] <> '"') or (ts[Length(ts)] <> '"') then
        Result := UpperCase(Result);
    end;
  end;

begin
  AConnectionStrList := TStringList.Create;
  try
    BeginConnectionStrGetting(AConnectionStrList);
    DADesignUtils.GetDBToolsConnectionList(AConnection);
    i := Length(FUsedConnectionCompareFlags);
    SetLength(FUsedConnectionCompareFlags, FDefaultConnectionList.Count);
    for j := i to FDefaultConnectionList.Count - 1 do
      FUsedConnectionCompareFlags[j] := cfNormal;
    for i := 0 to FConnectionsList.Count - 1 do begin
      ConnStrToList(FConnectionsList[i], FConnectionStrList);
      for j := 0 to FDefaultConnectionList.Count - 1 do begin
        if FUsedConnectionCompareFlags[j] = cfNone then
          Continue;

        DefStr := FDefaultConnectionList.ValueFromIndex[j];
        Str1 := ToCommonCase(AConnectionStrList.Values[FDefaultConnectionList.Names[j]]);
        Str2 := ToCommonCase(FConnectionStrList.Values[FDefaultConnectionList.Names[j]]);
        if (Str1 <> Str2) and (DefStr <> '') then begin
          if Str1 = '' then
            Str1 := ToCommonCase(DefStr);
          if Str2 = '' then
            Str2 := ToCommonCase(DefStr);
        end;
        if Str1 <> Str2 then
          Break
        else
          if j = FDefaultConnectionList.Count - 1 then begin
            Result := FConnectionStrList.Values[SConnectionName];
            Exit;
          end;
      end;
    end;
    Result := '';
  finally
    AConnectionStrList.Free;
  end;
end;

function TDBToolsService.GetConnectionStrList(ConnectionName: string): TStringList;
var
  i, j, k: integer;
  s: string;
begin
  Result := FConnectionStrList;
  s := GetConnectionValueStr(ConnectionName);
  for i := 0 to FConnectionsList.Count - 1 do
    if (Length(FConnectionsList[i]) >= Length(s)) and
      (Copy(FConnectionsList[i], 1, Length(s)) = s) then begin
      ConnStrToList(Copy(FConnectionsList[i], Length(s) + 1, Length(FConnectionsList[i]) - Length(s)), Result);

      for j := 0 to FDefaultConnectionList.Count - 1 do begin
        k := Result.IndexOfName(FDefaultConnectionList.Names[j]);
        if k < 0 then
          Result.Add(FDefaultConnectionList[j])
        else
          if Result.ValueFromIndex[k] = '' then
            Result[k] := FDefaultConnectionList[j];
      end;
      Exit;
    end;
  Result.Clear;
end;

procedure TDBToolsService.CheckConnection(const Component: TComponent);
var
  Connection: TCustomDAConnection;
begin
  Connection := GetConnection(Component);
  if Connection = nil then
    DatabaseError(SConnectionNotDefined);
end;

procedure TDBToolsService.FindInDatabaseExplorer;
var
  ConnectionString, ObjectType, FullName: TString;
begin
  CheckConnection(FCurrentComponent);
  if GetConnectionStringObjectTypeAndFullName(FCurrentComponent, ConnectionString, ObjectType, FullName) then
    SqlService.FindInDatabaseExplorer(ConnectionString, ObjectType, FullName);
end;

procedure TDBToolsService.EditDatabaseObject;
var
  ConnectionString, ObjectType, FullName: TString;
begin
  CheckConnection(FCurrentComponent);
  if GetConnectionStringObjectTypeAndFullName(FCurrentComponent, ConnectionString, ObjectType, FullName) then
    SqlService.EditDatabaseObject(ConnectionString, ObjectType, FullName);
end;

procedure TDBToolsService.ExecuteSql(Debug: boolean);
var
  SqlSource: TSqlSource;
begin
  CheckConnection(FCurrentComponent);
  SqlSource := GetSqlSource(FCurrentComponent, FCurrentDesigner);
  SqlService.ExecuteSql(SqlSource, Debug);
end;

procedure TDBToolsService.Compile(Debug: boolean);
var
  SqlSource: TSqlSource;
begin
  CheckConnection(FCurrentComponent);
  SqlSource := GetSqlSource(FCurrentComponent, FCurrentDesigner, SCompilePrefix);
  SqlService.ExecuteSql(SqlSource, Debug);
end;

procedure TDBToolsService.RetrieveData(AsDocument: boolean);
var
  SqlSource: TSqlSource;
begin
  CheckConnection(FCurrentComponent);
  SqlSource := GetSqlSource(FCurrentComponent, FCurrentDesigner);
  SqlService.RetrieveData(SqlSource, AsDocument);
end;

procedure TDBToolsService.EditSql(AsQuery: boolean);
var
  SqlSourceNotifier: ISqlSourceNotifier;
  SqlSource: TSqlSource;
begin
  SqlSource := GetSqlSource(FCurrentComponent, FCurrentDesigner, SEditPrefix);
  SqlService.EditSql(SqlSource, AsQuery, SqlSourceNotifier);
  TSourceNotifier(SqlSource.FSqlSourceNotifier).FSqlSourceNotifier := SqlSourceNotifier;   
  if SqlSource.FSqlSourceNotifier = nil then
    DBTools.DesignNotification.SqlSourceList.Delete(SqlSource);
end;

function TDBToolsService.GetDACSqlEditorFrameClass: TCustomDACSqlEditorFrameClass;
begin
  Result := TDACSqlEditorFrame;
end;

function TDBToolsService.SqlSourceClass: TCustomSqlSourceClass;
begin
  Result := TSqlSource;
end;

function TDBToolsService.GetSqlEditor: ISqlEditor;
var
  n: integer;
begin
  n := Length(FSqlEditors) - 1;
  if n >= 0 then begin
    Result := FSqlEditors[n];
    SetLength(FSqlEditors, n);
  end
  else
    SqlService.CreateSqlEditor(Result);
end;

function TDBToolsService.GetSqlSource(Component: TComponent; Designer: IDesigner; SqlTextPrefix: string = ''): TSqlSource;
begin
  Assert(DBTools.DesignNotification <> nil);
  Result := TSqlSource(DBTools.DesignNotification.SqlSourceList.Find(Component, SqlTextPrefix));
  if Result = nil then begin
    Result := TSqlSource.Create(Self, Component, Designer);
    Result.SqlTextPrefix := SqlTextPrefix;
    DBTools.DesignNotification.SqlSourceList.Add(Result);
  end;
end;

procedure TDBToolsService.ReturnSqlEditor(ASqlEditor: ISqlEditor);
var
  n: integer;
begin
  Windows.SetParent(ASqlEditor.Handle{$IFDEF CLR}.ToInt32{$ENDIF}, MAXDWORD - 2{HWND_MESSAGE});
  n := Length(FSqlEditors);
  SetLength(FSqlEditors, n + 1);
  FSqlEditors[n] := ASqlEditor;
end;

procedure TDBToolsService.AddParamTypeMap(ADACType: TFieldType; ADBToolsType: integer);
var
  n: integer;
begin
  n := Length(FParamTypeMaps);
  SetLength(FParamTypeMaps, n + 1);
  with FParamTypeMaps[n] do begin
    DACType := ADACType;
    DBToolsType := ADBToolsType;
  end;
end;

{$IFDEF WIN32_64}
class function TDBToolsService.AccessData(const V: PSafeArray): pointer;
begin
  if V = nil then
    Result := nil
  else
    SafeArrayCheck(SafeArrayAccessData(V, Result));
end;

class procedure TDBToolsService.UnaccessData(const V: PSafeArray);
begin
  if V <> nil then
    SafeArrayCheck(SafeArrayUnaccessData(V));
end;

class function TDBToolsService.DataHigh(const V: PSafeArray): integer;
begin
  if V = nil then
    Result := -1
  else
    SafeArrayGetUBound(V, 1, Result);
end;
{$ENDIF}

function TDBToolsService.GetConnectionValueStr(ConnectionName: string): string;
begin
  Result := SConnectionName + '=' + ConnectionName + ';';
end;

procedure TDBToolsService.ConnStrToList(ConnStr: string; const ConnList: TStrings);
var
  ParamName, ParamValue: string;
  QuoteChar: char;
  Quoted: boolean;
  i, l: integer;
begin
  ConnStr := Trim(ConnStr);
  ConnList.BeginUpdate;
  ConnList.Clear;
  try
    if ConnStr = '' then
      Exit;
    if ConnStr[Length(ConnStr)] <> ';' then
      ConnStr := ConnStr + ';';
    repeat
      i := Pos('=', ConnStr);
      if i <= 0 then
        Break;
      ParamName := Copy(ConnStr, 1, i); //with '='
      Inc(i);
      QuoteChar := ConnStr[i];
      Quoted := AnsiChar(QuoteChar) in ['''', '"'];
      if Quoted then
        Inc(i);
      ParamValue := '';
      repeat
        if i + Ord(Quoted) > Length(ConnStr) then
          raise Exception.Create(SConnStrError);

        if Quoted then
          if ConnStr[i] = QuoteChar then begin
            Quoted := ConnStr[i + 1] <> ';';
            if (ConnStr[i + 1] = QuoteChar) or not Quoted then
              Inc(i)
            else
              raise Exception.Create(SConnStrError);
          end;

        if not Quoted and (ConnStr[i] = ';') then
          Break
        else
          ParamValue := ParamValue + ConnStr[i];
        Inc(i);
      until False;
      ConnList.Add(ParamName + ParamValue);
      l := Length(ConnStr) - i;
      if l > 0 then
        ConnStr := Copy(ConnStr, i + 1, l)
      else
        Break;
    until False;
  finally
    ConnList.EndUpdate;
  end;
end;

{$ENDIF DBTOOLS}

end.
