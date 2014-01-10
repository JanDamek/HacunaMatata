
{$IFNDEF CLR}
{$I Dac.inc}

unit DBToolsClient;
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
  System.Text,
{$ENDIF}
  ActiveX,
  CRTypes, CRDesignUtils, DADesignUtils,
  Windows, Classes, Controls, StdCtrls, ExtCtrls, DB, DBAccess,
{$IFNDEF CLR}
  DBToolsIntf,
{$ENDIF}  
  ActnMan, ActnList, Graphics, Menus, Forms, Messages;

type
{$IFDEF CLR}
  TString = string;
  IIDEServices = IOTAService;
  IModuleServices = IOTAModuleServices;
{$ELSE}
  TString = PWideChar;
  PConnectionInfo = ^ConnectionInfo;
  IIDEServices = IOTAServices;
  IModuleServices = IOTAModuleServices70;
{$ENDIF}

  TParamTypeMap = record
    DACType: TFieldType;
    DBToolsType: integer;
  end;

  TDBToolsVerb = (dbtEditSql, dbtQueryBuilder,
    dbtFindInDatabaseExplorer, dbtEditDatabaseObject, dbtExecuteSql,
    dbtDebugSql, dbtRetrieveData, dbtCompile, dbtCompileDebug);

  TDBToolsVerbs = set of TDBToolsVerb;

  TCompareFlag = (cfNormal, cfCaseSensitive, cfNone);

  TCustomDBToolsService = class;
  TCustomDBToolsServiceClass = class of TCustomDBToolsService;
  TCustomDACSqlEditorFrame = class;
  TCustomDACSqlEditorFrameClass = class of TCustomDACSqlEditorFrame;
  TCustomSourceNotifier = class;
  TCustomSourceNotifierClass = class of TCustomSourceNotifier;
  TCustomSqlSource = class;
  TCustomSqlSourceClass = class of TCustomSqlSource;

  TCustomDBToolsService = class(TObject)
  protected
    FCurrentDesigner: IDesigner;
    FCurrentComponent: TComponent;
    FDADesignUtils: TDADesignUtilsClass;

    function GetDACSqlEditorFrameClass: TCustomDACSqlEditorFrameClass; virtual; abstract;
    function SqlSourceClass: TCustomSqlSourceClass; virtual; abstract;

    procedure DesignerClosing(DesignerName: string); virtual; abstract;
    class function GetNamespace: string; virtual; abstract;
    class function UseNewRegPath: boolean; virtual; abstract;
  public
    constructor Create(ADADesignUtils: TDADesignUtilsClass;
      ASqlService: {$IFDEF WIN32_64}IUnknown{$ELSE}TObject{$ENDIF};
      ADefaultConnectionStr: string); virtual;

    function GetConnection(const Component: TComponent): TCustomDAConnection;

    procedure FindInDatabaseExplorer; virtual; abstract;
    procedure EditDatabaseObject; virtual; abstract;
    procedure ExecuteSql(Debug: boolean); virtual; abstract;
    procedure Compile(Debug: boolean); virtual; abstract;
    procedure RetrieveData(AsDocument: boolean); virtual; abstract;
    procedure EditSql(AsQuery: boolean); virtual; abstract;

    procedure GetConnections(NameList: TStrings; Condition: string = ''); virtual; abstract;
    function FindConnectionName(AConnection: TCustomDAConnection): string; virtual; abstract;
    function GetConnectionStrList(ConnectionName: string): TStringList; virtual; abstract;

    procedure AddParamTypeMap(ADACType: TFieldType; ADBToolsType: integer); virtual; abstract;
    procedure PutConnectionParam(const ConnectionParam: string; const CompareFlag: TCompareFlag = cfNormal); virtual; abstract;
    procedure SkipConnectionParams(const Count: integer); virtual; abstract;

    property DADesignUtils: TDADesignUtilsClass read FDADesignUtils;
  end;

  TCustomDACSqlEditorFrame = class(TPanel)
  protected
    FReadOnly: boolean;
    FOnExit: TNotifyEvent;
    FOnChange: TNotifyEvent;

    procedure CheckModified; virtual; abstract;
    procedure CheckConnectionChange; virtual; abstract;
    procedure EndInit; virtual; abstract;

    procedure SetStatementType(const Value: TStatementType); virtual; abstract;
    function GetText: _string; virtual; abstract;
    procedure SetText(const Value: _string); reintroduce; virtual; abstract;
    procedure SetReadOnly(Value: boolean);  virtual; abstract;
  public
    constructor Create(AOwner: TComponent; Component: TComponent; DBToolsService: TCustomDBToolsService); reintroduce; virtual;

    property Text: _string read GetText write SetText;
    property ReadOnly: boolean read FReadOnly write SetReadOnly;
    property StatementType: TStatementType write SetStatementType;
    property OnExit: TNotifyEvent read FOnExit write FOnExit;
    property OnChange: TNotifyEvent read FOnChange write FOnChange;
  end;

  TDBToolsDesignNotification = class;

  TDBTools = class(TObject)
  private
    FIDEServices: IIDEServices;
    FModuleServices: IModuleServices;
    FDBToolsServices: array of TCustomDBToolsService;
    FDesignNotification: TDBToolsDesignNotification;
    FCurrentDADesignUtils: TDADesignUtilsClass;
    FCurrentDBToolsService: TCustomDBToolsService;
  {$IFDEF WIN32_64}
    FNeedUninitialize: boolean;
  {$ENDIF}
    FMenuManager: TActionManager;
    FMenuActions: array [TDBToolsVerb] of TAction;
    FLastDACSqlEditorFrame: TCustomDACSqlEditorFrame;
    FLastMemo: TWinControl;
    FDACSqlEditorFrames: array of TCustomDACSqlEditorFrame;

    function MenuItemsAvailable: boolean;
    procedure DebugSql(Sender: TObject);
    procedure EditDatabaseObject(Sender: TObject);
    procedure EditSql(Sender: TObject);
    procedure ExecuteSql(Sender: TObject);
    procedure Compile(Sender: TObject);
    procedure CompileDebug(Sender: TObject);
    procedure FindInDatabaseExplorer(Sender: TObject);
    procedure QueryBuilder(Sender: TObject);
    procedure RetrieveData(Sender: TObject);

    procedure CreateMenuActions;
    function GetMenuActions(Index: TDBToolsVerb): TAction;
  public
    constructor Create;
    destructor Destroy; override;
    function CreateDBToolsService(const DBToolsServiceClass: TCustomDBToolsServiceClass;
      const DADesignUtils: TDADesignUtilsClass;
      const ClassIDs: array of TGUID;{$IFDEF CLR} const ServiceShortName: string; {$ENDIF}
      const DefaultConnectionStr: string; const ProviderKey: string;
      out ServiceVersion: int64; out NeedToCheck: TNeedToCheckDbTools): TObject;
    function CheckDevTools(const ClassIDs: array of TGUID; const ProviderKey: string; const DADesignUtils: TDADesignUtilsClass;
      out ServiceVersion: int64 {$IFDEF WIN32_64}; FoundClassID: PGUID = nil{$ENDIF};
      UseNewRegPath: boolean = {$IFDEF VER11P}True{$ELSE}False{$ENDIF}): TNeedToCheckDbTools;
    procedure CheckDBToolsChanges(Control: TWinControl);
    procedure ReplaceMemo(var Memo: TMemo; DADesignUtils: TDADesignUtilsClass; Component: TComponent);
    procedure DesignerClosing(const FileName: string);
    function GetDesignerName(Designer: IDesigner): string;
    procedure PrepareMenu(Designer: IDesigner; Component: TComponent; DADesignUtils: TDADesignUtilsClass);
    function HasDACSqlEditorFrame(Memo: TWinControl): boolean;
    function GetDACSqlEditorFrame(Memo: TWinControl): TCustomDACSqlEditorFrame;
    procedure CheckConnectionChanges;
    function GetActiveDACSqlEditorFrame: TWinControl;

    procedure AddFrame(Value: TCustomDACSqlEditorFrame);
    procedure RemoveFrame(Value: TCustomDACSqlEditorFrame);

    property DesignNotification: TDBToolsDesignNotification read FDesignNotification;
    property IDEServices: IIDEServices read FIDEServices;
    property ModuleServices: IModuleServices read FModuleServices;

    property MenuActions[Index: TDBToolsVerb]: TAction read GetMenuActions;
  end;

  TModuleNotifier = class({$IFDEF CLR}TObject{$ELSE}TInterfacedObject, IOTANotifier, IOTAModuleNotifier, IOTAModuleNotifier80{$ENDIF})
  private
    FModule : IOTAModule;
  {$IFDEF WIN32_64}
    AllowSaveSwitch : boolean;
    FIndex : integer;
  {$ELSE}
    procedure BeforeSave(Sender: TObject; e: EventArgs);
  {$ENDIF}
    procedure RemoveNotifier;
  public
    constructor Create(const Module: IOTAModule);
    destructor Destroy; override;
  {$IFDEF WIN32_64}
  {IOTANotifier}
    procedure AfterSave;
    procedure BeforeSave;
    procedure Destroyed;
    procedure Modified;
  {IOTAModuleNotifier}
    function CheckOverwrite: Boolean;
    procedure ModuleRenamed(const NewName: string);
  {IOTAModuleNotifier80}
    function AllowSave: Boolean;
    function GetOverwriteFileNameCount: Integer;
    function GetOverwriteFileName(Index: Integer): string;
    procedure SetSaveFileName(const FileName: string);
  {$ENDIF}
    property Module: IOTAModule read FModule;
  end;

  TModuleList = class(TList)
  public
    function IndexOf(Module: IOTAModule): integer; reintroduce;
    procedure Add(Module: IOTAModule);
    procedure Delete(Index: integer); reintroduce; overload;
    procedure Delete(Module: IOTAModule); overload;
  end;

  TCustomSourceNotifier = class(TObject)
  public
    constructor Create; virtual;
    procedure OnSqlSourceDeleted; virtual; abstract;
  end;

  TCustomSqlSource = class(TInterfacedObject)
  protected  
    FDesigner: IDesigner;
    FComponent: TComponent;
    FSqlTextPrefix: string;

    FSqlSourceNotifier: TCustomSourceNotifier;

    function GetSourceNotifierClass: TCustomSourceNotifierClass; virtual; abstract;
    procedure FreeSourceNotifier; virtual; abstract;
    function GetDBToolsService: TCustomDBToolsService; virtual; abstract;
  public
    constructor Create; virtual;
    destructor Destroy; override;
     
    procedure CheckChanges; virtual; abstract;

    procedure CheckRename; virtual; abstract;
    procedure CheckConnectionChange(InternalCheck: boolean); virtual; abstract;

    property Designer: IDesigner read FDesigner;
    property SqlTextPrefix: string read FSqlTextPrefix write FSqlTextPrefix;    
  end;

  TSqlSourceList = class(TList)
  private
    FNotifiersToDelete: array of TCustomSourceNotifier;
  public
    function IndexOf(Component: TComponent): integer; reintroduce;
    function Find(Component: TComponent): TCustomSqlSource; overload;
    function Find(Component: TComponent; SqlTextPrefix: string): TCustomSqlSource; overload;
    function FindDesigner(FileName: string): IDesigner;
    procedure CheckSubordinated(AComponent: TComponent);
    procedure Delete(Index: integer; WithNotification: boolean = False); reintroduce; overload;
    procedure Delete(Component: TComponent; WithNotification: boolean = True); overload;
    procedure Delete(SqlSource: TCustomSqlSource); overload;
    procedure Add(SqlSource: TCustomSqlSource);
    procedure DeleteDesigner(Designer: IDesigner);
    procedure CheckDeletedComponents(Designer: IDesigner);    
  end;

  TDBToolsDesignNotification = class(TInterfacedObject, IDesignNotification)
  protected
    FSqlSourceList: TSqlSourceList;
    FModuleList: TModuleList;
    FDebugStr: string;
  public
    constructor Create;
    destructor Destroy; override;
    procedure ItemDeleted(const ADesigner: IDesigner; AItem: TPersistent);
    procedure ItemInserted(const ADesigner: IDesigner; AItem: TPersistent);
    procedure ItemsModified(const ADesigner: IDesigner);
    procedure SelectionChanged(const ADesigner: IDesigner; const ASelection: IDesignerSelections);
    procedure DesignerOpened(const ADesigner: IDesigner; AResurrecting: Boolean);
    procedure DesignerClosed(const ADesigner: IDesigner; AGoingDormant: Boolean);

    property SqlSourceList: TSqlSourceList read FSqlSourceList;
    property ModuleList: TModuleList read FModuleList;
  end;

var
  DBTools: TDBTools;

function GetDBToolsService(DADesignUtilsClass: TDADesignUtilsClass): TCustomDBToolsService;

{$ENDIF DBTOOLS}

implementation

{$IFDEF DBTOOLS}

{$IFDEF WIN32_64}
{$R DBToolsClient.res}
{$ENDIF}
uses
  DADesign, Registry, Download, DAConsts,
  Variants, SysUtils, ComObj, TypInfo;

{$IFDEF WIN32_64}
  function ToWideChar(s: WideString): PWideChar;
  begin
    if s = '' then
      Result := nil
    else
      Result := SysAllocString(PWideChar(s));
  end;
{$ENDIF}


{ TCustomDBToolsService }

constructor TCustomDBToolsService.Create(ADADesignUtils: TDADesignUtilsClass;
  ASqlService: {$IFDEF WIN32_64}IUnknown{$ELSE}TObject{$ENDIF};
  ADefaultConnectionStr: string);
begin
  inherited Create;

  FDADesignUtils := ADADesignUtils; 
end;

function TCustomDBToolsService.GetConnection(
  const Component: TComponent): TCustomDAConnection;
begin
  if Component = nil then
    Result := nil
  else
  if Component is TCustomDAConnection then
    Result := TCustomDAConnection(Component)
  else
    Result := DADesignUtils.UsedConnection(Component);
end;

{ TCustomDACSqlEditorFrame }

constructor TCustomDACSqlEditorFrame.Create(AOwner: TComponent; Component: TComponent;
  DBToolsService: TCustomDBToolsService);
begin
  inherited Create(AOwner);
end;

constructor TCustomSourceNotifier.Create;
begin
  inherited;
end;

{ TCustomSqlSource }

constructor TCustomSqlSource.Create;
begin
  inherited;

  FSqlSourceNotifier := GetSourceNotifierClass.Create;
end;

destructor TCustomSqlSource.Destroy;
begin
  FSqlSourceNotifier.Free;
  
  inherited;
end;

{ TSqlSourceList }

procedure TSqlSourceList.Add(SqlSource: TCustomSqlSource);
var
  ModuleFileName, s: string;
  Module: IOTAModule;
begin
  SqlSource.Designer.ModuleFileNames(ModuleFileName, s, s);

  inherited Add(SqlSource);

  Module := DBTools.ModuleServices.FindModule(ModuleFileName);
  DBTools.DesignNotification.ModuleList.Add(Module)
end;

function TSqlSourceList.IndexOf(Component: TComponent): integer;
var
  i: integer;
begin
  Result := -1;
  for i := 0 to Count - 1 do
    if TCustomSqlSource(Items[i]).FComponent = Component then begin
      Result := i;
      Break;
    end;
end;

function TSqlSourceList.Find(Component: TComponent): TCustomSqlSource;
var
  i: integer;
begin
  i := IndexOf(Component);
  if i < 0 then
    Result := nil
  else
    Result := TCustomSqlSource(Items[i]);
end;

function TSqlSourceList.Find(Component: TComponent; SqlTextPrefix: string): TCustomSqlSource;
var
  i: integer;
begin
  for i := 0 to Count - 1 do
    if (TCustomSqlSource(Items[i]).FComponent = Component) and
      (TCustomSqlSource(Items[i]).SqlTextPrefix = SqlTextPrefix) then begin
      Result := TCustomSqlSource(Items[i]);
      Exit;
    end;
  Result := nil;
end;

procedure TSqlSourceList.CheckSubordinated(AComponent: TComponent);
var
  i: integer;
begin
  if AComponent is TCustomDAConnection then begin
    for i := 0 to Count - 1 do
      with TCustomSqlSource(Items[i]) do
        if GetDBToolsService.GetConnection(FComponent) = AComponent then
          CheckConnectionChange(False);
  end
  else
    for i := 0 to Count - 1 do
      if TCustomSqlSource(Items[i]).FComponent.Owner = AComponent then
        TCustomSqlSource(Items[i]).CheckRename;
end;

procedure TSqlSourceList.Delete(Index: integer; WithNotification: boolean = False);
var
  n: integer;
  SqlSource: TCustomSqlSource;
begin
  SqlSource := TCustomSqlSource(Items[Index]);
  if WithNotification and (SqlSource.FSqlSourceNotifier <> nil) then begin
    n := Length(FNotifiersToDelete);
    SetLength(FNotifiersToDelete, n + 1);
    FNotifiersToDelete[n] := SqlSource.FSqlSourceNotifier;
    SqlSource.FreeSourceNotifier;
    inherited Delete(Index);
  end
  else
    inherited Delete(Index);
end;

procedure TSqlSourceList.Delete(Component: TComponent; WithNotification: boolean = True);
var
  Index: integer;
begin
  Index := IndexOf(Component);
  if Index >= 0 then
    Delete(Index, WithNotification);
end;

procedure TSqlSourceList.Delete(SqlSource: TCustomSqlSource);
var
  i: integer;
begin
  for i := 0 to Count - 1 do
    if TCustomSqlSource(Items[i]) = SqlSource then begin
      inherited Delete(i);
      Break;
    end;
end;

procedure TSqlSourceList.DeleteDesigner(Designer: IDesigner);
var
  i: integer;
  SqlSource: TCustomSqlSource;
begin
  i := 0;
  while i < Count do begin
    SqlSource := TCustomSqlSource(Items[i]);
    if SqlSource.Designer = Designer then begin
      if SqlSource.FSqlSourceNotifier <> nil then begin
        SqlSource.FSqlSourceNotifier.OnSqlSourceDeleted;
        SqlSource.FreeSourceNotifier;
      end;
      inherited Delete(i);
    end
    else
      Inc(i);
  end;
end;

procedure TSqlSourceList.CheckDeletedComponents(Designer: IDesigner);
var
  i, n: integer;
  Notifier: TCustomSourceNotifier;
  Sel: IDesignerSelections;
begin
  n := Length(FNotifiersToDelete);
  if n <= 0 then
    Exit;
  Sel := CreateSelectionList;
  Sel.Add(Designer.Root);
  Designer.SetSelections(Sel);
  Designer.Activate;
  for i := n - 1 downto 0 do begin
    Notifier := FNotifiersToDelete[i];
    SetLength(FNotifiersToDelete, i);
    Notifier.OnSqlSourceDeleted;
  end;
end;

function TSqlSourceList.FindDesigner(FileName: string): IDesigner;
var
  i: integer;
  ModuleFileName, s: string;
begin
  for i := 0 to Count - 1 do begin
    TCustomSqlSource(Items[i]).Designer.ModuleFileNames(ModuleFileName, s, s);
    if ModuleFileName = FileName then begin
      Result := TCustomSqlSource(Items[i]).Designer;
      Exit;
    end;
  end;
  Result := nil;
end;

{ TDBToolsDesignNotification }

constructor TDBToolsDesignNotification.Create;
begin
  inherited;
  FSqlSourceList := TSqlSourceList.Create;
  FModuleList := TModuleList.Create;
end;

procedure TDBToolsDesignNotification.DesignerClosed(const ADesigner: IDesigner;
  AGoingDormant: Boolean);
begin
  if ADesigner <> nil then
    DBTools.DesignNotification.SqlSourceList.DeleteDesigner(ADesigner);
end;

procedure TDBToolsDesignNotification.DesignerOpened(const ADesigner: IDesigner;
  AResurrecting: Boolean);
begin
end;

destructor TDBToolsDesignNotification.Destroy;
var
  i, n: integer;
begin
  n := FSqlSourceList.Count;
  for i := n - 1 downto 0 do
    FSqlSourceList.Delete(i);
  FSqlSourceList.Free;

  n := FModuleList.Count;
  for i := n - 1 downto 0 do begin
  {$IFDEF CLR}
    TModuleNotifier(FModuleList.Items[i]).Free;
  {$ENDIF}
    FModuleList.Delete(i);
  end;
  FModuleList.Free;
end;

procedure TDBToolsDesignNotification.ItemDeleted(const ADesigner: IDesigner;
  AItem: TPersistent);
begin
  if AItem is TComponent then
    FSqlSourceList.Delete(TComponent(AItem));
  FSqlSourceList.CheckDeletedComponents(ADesigner);
end;

procedure TDBToolsDesignNotification.ItemInserted(const ADesigner: IDesigner;
  AItem: TPersistent);
begin

end;

procedure TDBToolsDesignNotification.ItemsModified(const ADesigner: IDesigner);
var
  i: Integer;
  SqlSource: TCustomSqlSource;
  SelectionList: IDesignerSelections;
  Component: TComponent;
begin
  if ADesigner = nil then
    Exit;
  SelectionList := CreateSelectionList;
  ADesigner.GetSelections(SelectionList);
  for i := 0 to SelectionList.Count - 1 do begin
    if SelectionList.Items[i] is TComponent then begin
      Component := TComponent(SelectionList.Items[i]);
      SqlSource := FSqlSourceList.Find(Component);
      if SqlSource <> nil then
        SqlSource.CheckChanges
      else
        FSqlSourceList.CheckSubordinated(Component);
    end;
  end;
end;

procedure TDBToolsDesignNotification.SelectionChanged(const ADesigner: IDesigner;
  const ASelection: IDesignerSelections);
begin

end;

{ TDBTools }

procedure TDBTools.CheckDBToolsChanges(Control: TWinControl);
var
  i: integer;
begin
  if Control <> nil then
    for i := 0 to Control.ComponentCount - 1 do
      if Control.Components[i] is TCustomDACSqlEditorFrame then
        TCustomDACSqlEditorFrame(Control.Components[i]).CheckModified;
end;

function TDBTools.CreateDBToolsService(const DBToolsServiceClass: TCustomDBToolsServiceClass;
      const DADesignUtils: TDADesignUtilsClass;
      const ClassIDs: array of TGUID;{$IFDEF CLR} const ServiceShortName: string; {$ENDIF}
      const DefaultConnectionStr: string; const ProviderKey: string;
      out ServiceVersion: int64; out NeedToCheck: TNeedToCheckDbTools): TObject;
var
  n: integer;
{$IFDEF CLR}
  ServiceType: System.Type;
  Obj: TObject;
{$ELSE}
  Obj: IUnknown;
  ClassID: TGUID; 
{$ENDIF}
  DBToolsService: TCustomDBToolsService;

{$IFDEF CLR}
  function FindType(const TypeFullName, AssemblyNameFilter: string; out AType: System.Type): boolean;
  var
    i, j: integer;
    LoadedAssemblies: array of Assembly;
    Types: array of System.Type;
  begin
    Result := False;
    LoadedAssemblies := System.AppDomain.CurrentDomain.GetAssemblies;
    for i := 0 to Length(LoadedAssemblies) - 1 do
      if (LoadedAssemblies[i].FullName.ToLower.IndexOf(AssemblyNameFilter.ToLower) >= 0) then begin
        Types := LoadedAssemblies[i].GetExportedTypes;
        for j := 0 to Length(Types) - 1 do
          if Types[j].FullName = TypeFullName then begin
            AType := Types[j];
            Result := True;
            Exit;
          end;
      end;
  end;
{$ENDIF}

begin
  Result := nil;
  Obj := nil;
  try
    NeedToCheck := ncNoAddin;
    ServiceVersion := 0;
    DADesignUtils.SetDBToolsDownloadParams(False, False);
    if FIDEServices = nil then begin
    {$IFDEF WIN32_64}
      Supports(BorlandIDEServices, IIDEServices, FIDEServices);
      Supports(BorlandIDEServices, IModuleServices, FModuleServices);
    {$ELSE}
      FIDEServices := BorlandIDE.OTAService;
      FModuleServices := BorlandIDE.ModuleServices;
    {$ENDIF}
    end;

    NeedToCheck := CheckDevTools(ClassIDs, ProviderKey, DADesignUtils,
      ServiceVersion{$IFDEF WIN32_64}, @ClassId{$ENDIF}, DBToolsServiceClass.UseNewRegPath);
    if NeedToCheck <> ncNone then
      Exit;
  {$IFDEF CLR}
    if FindType(DBToolsServiceClass.GetNamespace + '.' + ServiceShortName + '.' + ServiceShortName + 'DbToolsService', ServiceShortName, ServiceType) then
  {$ENDIF}
    begin
    {$IFDEF WIN32_64}
      if not FNeedUninitialize then
        FNeedUninitialize := Succeeded(CoInitialize(nil));
      Obj := CreateComObject(ClassID);
    {$ELSE}
      Obj := BorlandIDE.GetService(ServiceType);    
    {$ENDIF}
    end;
    if Obj = nil then begin
      NeedToCheck := ncExpired;
      Exit;
    end
    else begin
      if FDesignNotification = nil then begin
        FDesignNotification := TDBToolsDesignNotification.Create;
        RegisterDesignNotification(FDesignNotification);
      end;
      DBToolsService := DBToolsServiceClass.Create(DADesignUtils, Obj, DefaultConnectionStr);
      Result := DBToolsService;
      n := Length(FDBToolsServices);
      SetLength(FDBToolsServices, n + 1);
      FDBToolsServices[n] := DBToolsService;
    end;
  except
    NeedToCheck := ncExpired;
  end;
end;

function TDBTools.CheckDevTools(const ClassIDs: array of TGUID; const ProviderKey: string; const DADesignUtils: TDADesignUtilsClass;
      out ServiceVersion: int64 {$IFDEF WIN32_64}; FoundClassID: PGUID = nil{$ENDIF};
      UseNewRegPath: boolean = {$IFDEF VER11P}True{$ELSE}False{$ENDIF}): TNeedToCheckDbTools;

  function GetFileVersion(Filename: string): int64;
  var
  {$IFDEF CLR}
    VersionInfo: FileVersionInfo;
  {$ELSE}
    Handle,
    FixedFileInfoLen,
    VersionDataLen: DWORD;
    VersionData: TBytes;
    FixedFileInfo: ^VS_FIXEDFILEINFO;
  {$ENDIF}
  begin
    Result := 0;
    if FileExists(Filename) then begin
    {$IFDEF CLR}
      VersionInfo := FileVersionInfo.GetVersionInfo(Filename);
      Result :=
        VersionInfo.FileMajorPart * $1000000000000 +
        VersionInfo.FileMinorPart * $100000000 +
        VersionInfo.FileBuildPart * $10000 +
        VersionInfo.FilePrivatePart;
    {$ELSE}
      VersionDataLen := GetFileVersionInfoSize(PChar(Filename), Handle);
      SetLength(VersionData, VersionDataLen);
      if GetFileVersionInfo(PChar(Filename), Handle, VersionDataLen, VersionData) and
        VerQueryValue(VersionData, '\', Pointer(FixedFileInfo), FixedFileInfoLen) then
        Result := FixedFileInfo.dwFileVersionMS * $100000000 + FixedFileInfo.dwFileVersionLS;
    {$ENDIF}
    end;
  end;

var
  i: integer;
  ModuleGUID: string;
  ProviderRegPath: string;
begin
  Result := ncNoAddin;
  ServiceVersion := 0;
  with TRegistry.Create(KEY_READ OR KEY_WRITE) do
    try
      try
        RootKey := HKEY_CLASSES_ROOT;

        for i := 0 to High(ClassIDs) do
          if OpenKeyReadOnly({$IFDEF WIN32_64}'CLSID\' + GUIDToString(ClassIDs[i]) + '\InprocServer32'
            {$ELSE}'CLSID\{' + GUIDToString(ClassIDs[i]) + '}\InprocServer32'{$ENDIF})
            and ValueExists('CodeBase') then begin
            Result := ncIncompatible;
            ServiceVersion := GetFileVersion(ReadString('CodeBase'));
            CloseKey;
            if ServiceVersion <> 0 then
              Result := ncNone;
          {$IFDEF WIN32_64}
            if FoundClassID <> nil then
              FoundClassID^ := ClassIDs[i];
          {$ENDIF}
            Break;
          end;

        if Result <> ncNone then
          if UseNewRegPath then begin
            ModuleGUID := '';
            RootKey := HKEY_LOCAL_MACHINE;
          {$IFDEF VER16}
            ProviderRegPath := 'SOFTWARE\Devart\Integrated Products\RAD Studio XE2\';
          {$ELSE}
          {$IFDEF VER15}
            ProviderRegPath := 'SOFTWARE\Devart\Integrated Products\RAD Studio XE\';
          {$ELSE}
          {$IFDEF VER14}
            ProviderRegPath := 'SOFTWARE\Devart\Integrated Products\RAD Studio 2010\';
          {$ELSE}
          {$IFDEF VER12}
            ProviderRegPath := 'SOFTWARE\Devart\Integrated Products\RAD Studio 2009\';
          {$ELSE}
          {$IFDEF VER11}
            ProviderRegPath := 'SOFTWARE\Devart\Integrated Products\RAD Studio 2007\';
          {$ELSE}
            Assert(False);
          {$ENDIF}
          {$ENDIF}
          {$ENDIF}
          {$ENDIF}
          {$ENDIF}

            if OpenKeyReadOnly(ProviderRegPath  + 'DatabaseProviders\' + ProviderKey)
              and ValueExists('Module')
            then begin
              ModuleGUID := ReadString('Module');
              CloseKey;
            end;

            if ModuleGUID <> '' then
              if OpenKeyReadOnly(ProviderRegPath + 'Modules\' + ModuleGUID)
                and ValueExists('CodeBase')
              then begin
                Result := ncIncompatible;
                ServiceVersion := GetFileVersion(ReadString('CodeBase'));
                CloseKey;
              end;
          end
          else begin
            RootKey := HKEY_LOCAL_MACHINE;
            if OpenKeyReadOnly('SOFTWARE\CoreLab\Database Developer Tools\DatabaseProviders\' + ProviderKey)
              and ValueExists('Assembly') then begin
              Result := ncIncompatible;
              ServiceVersion := GetFileVersion(ReadString('Assembly'));
              CloseKey;
            end;
          end;
      except
        //Silent
      end;
    finally
      Free;
      if Result <> ncNone then begin
        DADesignUtils.SetDBToolsDownloadParams(False, Result = ncIncompatible);
        CheckForTools(Result = ncIncompatible);
      end;
    end;
end;

function TDBTools.MenuItemsAvailable: boolean;
var
  Incompatible: boolean;
begin
  Result := FCurrentDADesignUtils.DBToolsAvailable;
  Incompatible := FCurrentDADesignUtils.NeedToCheckDbTools = ncIncompatible;
  if not Result then begin
    FCurrentDADesignUtils.SetDBToolsDownloadParams(True, Incompatible);
    CheckForTools(Incompatible);
  end;
end;

procedure TDBTools.EditSql(Sender: TObject);
begin
  if MenuItemsAvailable then
    FCurrentDBToolsService.EditSql(False);
end;

procedure TDBTools.QueryBuilder(Sender: TObject);
begin
  if MenuItemsAvailable then
    FCurrentDBToolsService.EditSql(True);
end;

procedure TDBTools.FindInDatabaseExplorer(Sender: TObject);
begin
  if MenuItemsAvailable then
    FCurrentDBToolsService.FindInDatabaseExplorer;
end;

procedure TDBTools.EditDatabaseObject(Sender: TObject);
begin
  if MenuItemsAvailable then
    FCurrentDBToolsService.EditDatabaseObject;
end;

procedure TDBTools.ExecuteSql(Sender: TObject);
begin
  if MenuItemsAvailable then
    FCurrentDBToolsService.ExecuteSql(False);
end;

procedure TDBTools.Compile(Sender: TObject);
begin
  if MenuItemsAvailable then
    FCurrentDBToolsService.Compile(False);
end;

procedure TDBTools.CompileDebug(Sender: TObject);
begin
  if MenuItemsAvailable then
    FCurrentDBToolsService.Compile(True);
end;

procedure TDBTools.DebugSql(Sender: TObject);
begin
  if MenuItemsAvailable then
    FCurrentDBToolsService.ExecuteSql(True);
end;

procedure TDBTools.RetrieveData(Sender: TObject);
begin
  if MenuItemsAvailable then
    FCurrentDBToolsService.RetrieveData(True);
end;

procedure TDBTools.PrepareMenu(Designer: IDesigner; Component: TComponent; DADesignUtils: TDADesignUtilsClass);
begin
  FCurrentDADesignUtils := DADesignUtils;
  FCurrentDBToolsService := GetDBToolsService(DADesignUtils);
  if FCurrentDBToolsService <> nil then
    with FCurrentDBToolsService do begin
      FCurrentDesigner := Designer;
      FCurrentComponent := Component;
    end;
end;

function TDBTools.HasDACSqlEditorFrame(Memo: TWinControl): boolean;
begin
  FLastMemo := Memo;
  FLastDACSqlEditorFrame := TCustomDACSqlEditorFrame(Memo.Owner.FindComponent(Memo.Name + '_dbt'));
  Result := FLastDACSqlEditorFrame <> nil;
end;

function TDBTools.GetDACSqlEditorFrame(Memo: TWinControl): TCustomDACSqlEditorFrame;
begin
  if Memo <> FLastMemo then
    HasDACSqlEditorFrame(Memo);
  Result := FLastDACSqlEditorFrame;
end;

procedure TDBTools.CheckConnectionChanges;
var
  i: integer;
begin
  for i := 0 to High(FDACSqlEditorFrames) do
    FDACSqlEditorFrames[i].CheckConnectionChange;
end;

function TDBTools.GetActiveDACSqlEditorFrame: TWinControl;
var
  i: integer;
  Handle: HWND;
  ClassName: {$IFDEF WIN32_64}PChar{$ELSE}StringBuilder{$ENDIF};
begin
  Result := nil;
{$IFDEF WIN32_64}
  GetMem(ClassName, 1024);
{$ELSE}
  ClassName := StringBuilder.Create;
  ClassName.Capacity := 1024;
{$ENDIF}
  try
    Handle := GetFocus;
    repeat
      Handle := GetParent(Handle);
      if Handle = 0 then
        Exit;
      GetClassName(Handle, ClassName, 1024);
  {$IFDEF CLR}
    until Pos(ClassName.ToString, 'DACSqlEditorFrame') > 0;  
  {$ELSE}
    until Pos(ClassName, 'DACSqlEditorFrame') > 0;
  {$ENDIF}  

    for i := 0 to High(FDACSqlEditorFrames) do
      if FDACSqlEditorFrames[i].Handle = Handle then begin
        Result := FDACSqlEditorFrames[i];
        Break;
      end;
  finally
  {$IFDEF WIN32_64}
    FreeMem(ClassName);
  {$ELSE}
    ClassName.Free;
  {$ENDIF}
  end;
end;

procedure TDBTools.AddFrame(Value: TCustomDACSqlEditorFrame);
var
  i: integer;
begin
  i := Length(FDACSqlEditorFrames);
  SetLength(FDACSqlEditorFrames, i + 1);
  FDACSqlEditorFrames[i] := Value;
end;

procedure TDBTools.RemoveFrame(Value: TCustomDACSqlEditorFrame);
var
  i, j, n: integer;
begin
  n := High(FDACSqlEditorFrames);
  for i := 0 to n do
    if FDACSqlEditorFrames[i] = Value then begin
      for j := i to n - 1 do
        FDACSqlEditorFrames[j] := FDACSqlEditorFrames[j + 1];
      SetLength(FDACSqlEditorFrames, n);
      Break;
    end;
end;

procedure TDBTools.CreateMenuActions;
var
  VerbIdx: TDBToolsVerb;
{$IFDEF WIN32_64}
  Bitmap: TBitmap;
  MenuImages: TImageList;
{$ENDIF}
begin
{$IFDEF WIN32_64}
  MenuImages := TImageList.Create(nil);
  Bitmap := TBitmap.Create;
  Bitmap.LoadFromResourceName(HInstance, 'MENUICONS');
  MenuImages.AddMasked(BitMap, $ff00ff);
  Bitmap.Free;
  (BorlandIDEServices as INTAServices).AddImages(MenuImages);
  MenuImages.Free;
{$ENDIF}
  FMenuManager := TActionManager.Create(nil);
  for VerbIdx := Low(TDBToolsVerb) to High(TDBToolsVerb) do begin
    FMenuActions[VerbIdx] := TAction.Create(FMenuManager);
    with FMenuActions[VerbIdx] do
      case VerbIdx of
        dbtEditSql: begin
          Caption := 'Edit SQL';
          OnExecute := EditSql;
        end;
        dbtQueryBuilder: begin
          Caption := 'Query Builder...';
          OnExecute := QueryBuilder;
        end;
        dbtFindInDatabaseExplorer: begin
          Caption := 'Find in Database Explorer';
          OnExecute := FindInDatabaseExplorer;
        end;
        dbtEditDatabaseObject: begin
          Caption := 'Edit object';
          OnExecute := EditDatabaseObject;
        end;
        dbtExecuteSql: begin
          Caption := 'Execute';
          OnExecute := ExecuteSql;
        end;
        dbtDebugSql: begin
          Caption := 'Step Into';
          OnExecute := DebugSql;
        end;
        dbtRetrieveData: begin
          Caption := 'Retreive data';
          OnExecute := RetrieveData;
        end;
        dbtCompile: begin
          Caption := 'Recompile';
          OnExecute := Compile;
        end;
        dbtCompileDebug: begin
          Caption := 'Recompile with debug info';
          OnExecute := CompileDebug;
        end;
      end;
  {$IFDEF WIN32_64}
    FMenuActions[VerbIdx].ImageIndex := Ord(VerbIdx);
    (BorlandIDEServices as INTAServices).AddActionMenu('', FMenuActions[VerbIdx], nil);
  {$ENDIF}
  end;
end;

constructor TDBTools.Create;
begin
  inherited;
  CreateMenuActions;
end;

destructor TDBTools.Destroy;
begin
  FMenuManager.Free;
  if FDesignNotification <> nil then
    UnregisterDesignNotification(FDesignNotification);
{$IFDEF WIN32_64}
  if FNeedUninitialize then
    CoUninitialize;
{$ENDIF}

  inherited;
end;

function TDBTools.GetDesignerName(Designer: IDesigner): string;
{$IFDEF CLR}
var
  Ptr: IntPtr;
{$ENDIF}
begin
{$IFDEF CLR}
  Ptr := Marshal.GetIUnknownForObject(Designer);
{$ENDIF}
  Result := 'Designer_' + IntToHex(Integer({$IFDEF WIN32_64}Designer{$ELSE}Ptr{$ENDIF}), 8) + FDesignNotification.FDebugStr;
{$IFDEF CLR}
  Marshal.Release(Ptr);
{$ENDIF}
end;

function TDBTools.GetMenuActions(Index: TDBToolsVerb): TAction;
begin
  Result := FMenuActions[Index];
end;

procedure TDBTools.ReplaceMemo(var Memo: TMemo; DADesignUtils: TDADesignUtilsClass; Component: TComponent);
var
  NewMemo: TWinControl;
  OldName: string;
  C: TComponent;
  DBToolsService: TCustomDBToolsService;
begin
  DBToolsService := GetDBToolsService(DADesignUtils);
  if (DBToolsService = nil) or HasDACSqlEditorFrame(Memo) then
    Exit;

  NewMemo := DBToolsService.GetDACSqlEditorFrameClass.Create(Memo.Owner, Component, DBToolsService);
  with TCustomDACSqlEditorFrame(NewMemo) do begin
    Parent := Memo.Parent;
    Left := Memo.Left;
    Top := Memo.Top;
    Width := Memo.Width;
    Height := Memo.Height;
    Align := Memo.Align;
    TabOrder := Memo.TabOrder;
    Anchors := Memo.Anchors;
    HelpContext := Memo.HelpContext;
    ReadOnly := Memo.ReadOnly;
    OnChange := Memo.OnChange;
    OnExit := Memo.OnExit;
    //OnKeyDown, OnKeyPress

    StatementType := stQuery;
    CheckConnectionChange;
  end;

  if Memo.Owner <> nil then begin
    Memo.OnExit := nil;
    C := NewMemo.Owner;
    while C <> nil do begin
      if C is TCustomForm then begin
        if TCustomForm(C).ActiveControl = Memo then begin
          TCustomForm(C).ActiveControl := NewMemo;
          NewMemo.SetFocus;
        end;
        Break;
      end;
      C := C.Owner;
    end;
  end;
  OldName := Memo.Name;
  Memo.Hide;

  NewMemo.Name := OldName + '_dbt';
  TCustomDACSqlEditorFrame(NewMemo).EndInit;
end;

procedure TDBTools.DesignerClosing(const FileName: string);
var
  i: integer;
  Designer: IDesigner;
  DesignerName: string;
begin
  Designer := FDesignNotification.SqlSourceList.FindDesigner(FileName);
  if Designer = nil then
    Exit;
  DesignerName := GetDesignerName(Designer);
  for i := 0 to Length(FDBToolsServices) - 1 do
    FDBToolsServices[i].DesignerClosing(DesignerName);
end;

{ TModuleNotifier}

constructor TModuleNotifier.Create(const Module : IOTAModule);
begin
  inherited Create;

  FModule := Module;
{$IFDEF WIN32_64}
  FIndex := Module.AddNotifier(Self);
{$ELSE}
  Include(Module.BeforeSave, BeforeSave);
{$ENDIF}
end;

procedure TModuleNotifier.RemoveNotifier;
begin
{$IFDEF WIN32_64}
  if Findex >= 0 then
  begin
    FModule.RemoveNotifier(FIndex);
    FIndex := -1;
  end;
{$ELSE}
  Exclude(FModule.BeforeSave, BeforeSave);
{$ENDIF}
  DBTools.DesignNotification.ModuleList.Delete(FModule);
end;

destructor TModuleNotifier.Destroy;
begin
  RemoveNotifier;

  inherited;
end;

{$IFDEF WIN32_64}
procedure TModuleNotifier.Destroyed;
begin
  RemoveNotifier;
  FModule := nil;
end;

procedure TModuleNotifier.AfterSave;
begin
end;

procedure TModuleNotifier.BeforeSave;
begin
end;

procedure TModuleNotifier.Modified;
begin
end;

function TModuleNotifier.CheckOverwrite: Boolean;
begin
  Result := True;
end;

procedure TModuleNotifier.ModuleRenamed(const NewName: string);
begin
end;

function TModuleNotifier.AllowSave: Boolean;
begin
  if AllowSaveSwitch then
    DBTools.DesignerClosing(FModule.FileName);
  AllowSaveSwitch := not AllowSaveSwitch;
  Result := True;
end;

function TModuleNotifier.GetOverwriteFileNameCount: Integer;
begin
  Result := 0;
end;

function TModuleNotifier.GetOverwriteFileName(Index: Integer): string;
begin
  Result := '';
end;

procedure TModuleNotifier.SetSaveFileName(const FileName: string);
begin
end;

{$ELSE}
procedure TModuleNotifier.BeforeSave(Sender: TObject; e: EventArgs);
begin
  DBTools.DesignerClosing(FModule.FileName);
end;
{$ENDIF}

{ TModuleList }

function TModuleList.IndexOf(Module: IOTAModule): integer;
var
  i: integer;
begin
  Result := -1;
  for i := 0 to Count - 1 do
    if TModuleNotifier(Items[i]).Module = Module then begin
      Result := i;
      Break;
    end;
end;

procedure TModuleList.Add(Module: IOTAModule);
begin
  if IndexOf(Module) < 0 then
    inherited Add(TModuleNotifier.Create(Module));
end;

procedure TModuleList.Delete(Module: IOTAModule);
var
  Index: integer;
begin
  Index := IndexOf(Module);
  if Index >= 0 then
    Delete(Index);
end;

procedure TModuleList.Delete(Index: integer);
var
  ModuleNotifier: TModuleNotifier;
begin
  ModuleNotifier := TModuleNotifier(Items[Index]);
  inherited Delete(Index);
{$IFDEF WIN32_64}
  ModuleNotifier.Destroyed;
{$ENDIF}
end;

function GetDBToolsService(DADesignUtilsClass: TDADesignUtilsClass): TCustomDBToolsService;
begin
  Result := TCustomDBToolsService(DADesignUtilsClass.DBToolsService);
end;

initialization
  DBTools := TDBTools.Create;

finalization
  DBTools.Free;

{$ENDIF DBTOOLS}

end.