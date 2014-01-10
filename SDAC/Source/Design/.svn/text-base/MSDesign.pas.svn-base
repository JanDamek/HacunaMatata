//////////////////////////////////////////////////
//  SQL Server Data Access Components
//  Copyright © 1998-2012 Devart. All right reserved.
//  SDAC Design
//////////////////////////////////////////////////

{$IFNDEF CLR}

{$I Sdac.inc}

unit MSDesign;
{$ENDIF}

interface

uses
{$IFDEF MSWINDOWS}
  Windows, Messages, Graphics, Controls, Forms, Dialogs,
  Registry, StdCtrls,
{$IFDEF CLR}
  Borland.Vcl.Design.DesignEditors, Borland.Vcl.Design.DesignIntf,
  Borland.Vcl.Design.FldLinks,
{$ELSE}
{$IFDEF FPC}
  PropEdits, ComponentEditors,
{$ELSE}
  {$IFDEF VER6P}DesignIntf, DesignEditors,{$ELSE}DsgnIntf,{$ENDIF}
  {$IFNDEF BCB}{$IFDEF VER5P}FldLinks,{$ENDIF} ColnEdit,{$ENDIF}
{$ENDIF}
{$ENDIF}
{$ENDIF}
  SysUtils, Classes, TypInfo, DADesign, CRDesign, DBAccess, MSAccess, CRTypes,
{$IFNDEF STD}
  MSCompactConnection, MSTransaction,
{$ENDIF}
  SdacVcl;


type

{ ------------  SDac property editors ----------- }

  TMSConnectStringPropertyEditor = class(TXStringProperty)
  protected
    FForm: TForm;
    FSucceeded: boolean;
    FConnectString: _string;
    procedure DoActivate(Sender: TObject);
  public
    function GetAttributes: TPropertyAttributes; override;
    procedure Edit; override;
  end;

  TMSServerNamePropertyEditor = class (TXStringProperty)
  public
    function GetAttributes: TPropertyAttributes; override;
    procedure GetValues(Proc: TGetXStrProc); override;
    function AutoFill: Boolean; override;
  end;

  TMSDatabaseNamePropertyEditor = class (TXStringProperty)
  protected
    procedure GetDialogOptions(Dialog: TOpenDialog); virtual;
  public
    procedure Edit; override;
    function GetAttributes: TPropertyAttributes; override;
    procedure GetValues(Proc: TGetXStrProc); override;
    function AutoFill: boolean; override;
  end;

{$IFNDEF STD}
  TMSQueuePropertyEditor = class(TXStringProperty)
  public
    function GetAttributes: TPropertyAttributes; override;
    procedure GetValues(Proc: TGetXStrProc); override;
  end;

  TMSServicePropertyEditor = class(TXStringProperty)
  public
    function GetAttributes: TPropertyAttributes; override;
    procedure GetValues(Proc: TGetXStrProc); override;
  end;

  TMSContractPropertyEditor = class(TXStringProperty)
  public
    function GetAttributes: TPropertyAttributes; override;
    procedure GetValues(Proc: TGetXStrProc); override;
  end;

  TMSTargetDatabaseNamePropertyEditor = class(TXStringProperty)
  public
    function GetAttributes: TPropertyAttributes; override;
    procedure GetValues(Proc: TGetXStrProc); override;
  end;

  TMSTableNamesEditor = class(TXStringProperty)
  public
    function GetAttributes: TPropertyAttributes; override;
    procedure Edit; override;
  end;

  TMSLocaleIdentifierPropertyEditor = class(TXStringProperty)
  public
    function GetAttributes: TPropertyAttributes; override;
    procedure GetValues(Proc: TGetXStrProc); override;
  end;

{$ENDIF}

  TMSConnectDialogPropertyEditor = class(TComponentProperty)
  private
    FCheckProc: TGetStrProc;
    procedure CheckComponent(const Value: string);
  public
    procedure GetValues(Proc: TGetStrProc); override;
  end;

{ ------------  SDac component editors ----------- }

  TMSConnectionEditor = class(TDAConnectionEditor)
  protected
    FQueryAnalyserIndex: integer;
    FManagementStudioIndex: integer;
    procedure InitVerbs; override;
  {$IFNDEF STD}
    procedure Convert;
  {$ENDIF}
  public
    procedure ExecuteVerb(Index: integer); override;
  end;

{$IFNDEF STD}
  TMSCompactConnectionEditor = class(TDAConnectionEditor)
  protected
    FManagementStudioIndex: integer;
    procedure InitVerbs; override;
    procedure Convert;
  public
    procedure ExecuteVerb(Index: integer); override;
  end;
{$ENDIF}

  TMSDataSetEditor = class(TDAComponentEditor);

  TMSQueryEditor = class(TMSDataSetEditor)
  protected
    FQueryAnalyserIndex: integer;
    FManagementStudioIndex: integer;
    procedure InitVerbs; override;
  public
    procedure ExecuteVerb(Index: integer); override;
  end;

  TMSSQLEditor = class(TDASQLEditor)
  protected
    procedure InitVerbs; override;
  end;

  TMSTableEditor = class(TMSDataSetEditor)
  protected
    procedure InitVerbs; override;
  end;

  TMSStoredProcEditor = class(TMSDataSetEditor)
  protected
    procedure InitVerbs; override;
  public
    procedure ExecuteVerb(Index: integer); override;
  end;

  TMSUpdateSQLEditor = class(TDAUpdateSQLEditor)
  protected
    procedure InitVerbs; override;
  end;

  TMSScriptEditor = class(TDASQLEditor)
  protected
    procedure InitVerbs; override;
  end;

{$IFNDEF STD} 
  TMSDumpEditor = class(TDAComponentEditor)
  protected
    procedure InitVerbs; override;
  end;
{$ENDIF}

  TMSTableDataEditor = class(TDAComponentEditor)
  protected
    procedure InitVerbs; override;
  end;

  TMSTableTypeNameEditor = class (TXStringProperty)
  public
    function GetAttributes: TPropertyAttributes; override;
    procedure GetValues(Proc: TGetXStrProc); override;
    function AutoFill: boolean; override;
  end;

{$IFNDEF FPC}
  TMSConnectionList = class (TDAConnectionList)
  protected
    function GetConnectionType: TCustomDAConnectionClass; override;
  end;

{$IFDEF VER6P}
  TMSDesignNotification = class(TDADesignNotification)
  public
    procedure ItemInserted(const ADesigner: IDesigner; AItem: TPersistent); override;
    function CreateConnectionList: TDAConnectionList; override;
    procedure SelectionChanged(const ADesigner: IDesigner;
      const ASelection: IDesignerSelections); override;
    function GetConnectionPropertyName: string; override;
  end;
{$ENDIF}
{$ENDIF}

procedure Register;

type
  TServerTool = (stQueryAnalyser, stManagementStudio);
  
procedure RunServerTool(ServerTool: TServerTool; Connection: TCustomMSConnection; const SQL: _TStrings = nil);
procedure RunServerToolConnection(ServerTool: TServerTool; Connection: TCustomMSConnection; const SQLText: _string);
procedure RunServerToolDataSet(ServerTool: TServerTool; DataSet: TCustomMSDataSet);
procedure RunServerToolMSSQL(ServerTool: TServerTool; MSSQL: TMSSQL);

function IsServerToolInstalled(ServerTool: TServerTool): boolean;

implementation

uses
  {$IFDEF CLR}System.Text, WinUtils, {$ENDIF}
{$IFNDEF FPC}
  {$IFNDEF CLR}ToolsAPI,{$ENDIF}
  MSMenu,
{$ENDIF}
  ShellAPI, ActiveX, ComObj, DB, DAConsts, OLEDBIntf, OLEDBC, OLEDBAccess,
  MSDesignUtils, MSConnectionEditor, MSQueryEditor, MSSQLEditor, MSStoredProcEditor,
  DATableEditor, MSUpdateSQLEditor, DAScriptEditor, MSScript 
{$IFNDEF STD}
  , MSLoader, MSDump, MSServiceBroker, MSDumpEditor, MSNamesEditor, MSCompactConnectionEditor
{$ENDIF}
  ;

var
  TmpFiles: _TStringList;
  CachedServerList: _TStrings = nil;

function GetServerToolCommand(ServerTool: TServerTool): _string;
type
  TRegKeyString = record
    Root: HKEY;
    Path, KeyName: _string;
    AdditionalPath: _string;
  end;
  TRegKeyArray = array[0..2] of TRegKeyString;
const
  QAKeyPaths: TRegKeyArray =
    ((Root: HKEY_LOCAL_MACHINE; Path: 'SOFTWARE\Microsoft\Microsoft SQL Server\80\Tools\ClientSetup'; KeyName: 'SQLPath'; AdditionalPath: '\Binn\isqlw'),
     (Root: HKEY_LOCAL_MACHINE; Path: 'SOFTWARE\Microsoft\MSSQLServer\Setup'; KeyName: 'SQLPath'; AdditionalPath: '\Binn\isqlw'),
     (Root: HKEY_CLASSES_ROOT; Path: 'SQLFile\Shell\open\command'; KeyName: ''; AdditionalPath: '\isqlw'));
  MMSKeyPaths: TRegKeyArray =
    ((Root: HKEY_LOCAL_MACHINE; Path: 'SOFTWARE\Microsoft\Microsoft SQL Server\100\Tools\ClientSetup'; KeyName: 'SQLPath'; AdditionalPath: '\Binn\VSShell\Common7\IDE\ssms'),
     (Root: HKEY_LOCAL_MACHINE; Path: 'SOFTWARE\Microsoft\Microsoft SQL Server\90\Tools\ClientSetup'; KeyName: 'SQLPath'; AdditionalPath: '\Binn\VSShell\Common7\IDE\sqlwb'),
     (Root: HKEY_CLASSES_ROOT; Path: 'sqlwb.sql.9.0\Shell\Open\Command'; KeyName: ''; AdditionalPath: '\sqlwb'));
var
  Reg: TRegistry;
  i: integer;
  KeyPaths: TRegKeyArray;
begin
  case ServerTool of
    stQueryAnalyser: begin
      Result := 'isqlw';
      KeyPaths := QAKeyPaths;
    end;
    stManagementStudio: begin
      Result := 'sqlwb';
      KeyPaths := MMSKeyPaths;
    end
    else
      Assert(False);
  end;

  Reg := TRegistry.Create{$IFDEF VER5P}(KEY_READ){$ENDIF};
  try
    for i := Low(KeyPaths) to High(KeyPaths) do begin
      Reg.RootKey := KeyPaths[i].Root;
      if Reg.OpenKeyReadOnly(KeyPaths[i].Path) then begin
        Result := Reg.ReadString(KeyPaths[i].KeyName);
        Reg.CloseKey;
        if Result <> '' then begin
          Result := Result + KeyPaths[i].AdditionalPath;
          Break;
        end;
      end;
    end;
  finally
    Reg.Free;
  end;
end;

function IsServerToolInstalled(ServerTool: TServerTool): boolean;
var
  Cmd: _string;
begin
  Cmd := GetServerToolCommand(ServerTool);
  Result := FileExists(Cmd + '.exe');
end;

procedure RunServerTool(ServerTool: TServerTool; Connection: TCustomMSConnection; const SQL: _TStrings = nil);
var
  Cmd, CmdParam: _string;
{$IFDEF CLR}
  TmpPath, TmpFileName: StringBuilder;
  SqlFileHandle: TOpenedFile;
{$ELSE}
  TmpPath, TmpFileName: array[0..MAX_PATH] of Char;
  SqlFileHandle: integer;
{$ENDIF}
  SqlFileName: _string;
  Code: integer;
begin
  if Connection = nil then
    DatabaseError(SConnectionNotDefined);

  CmdParam := ''; //'-1';

  if Connection.Server <> '' then
    CmdParam := CmdParam + ' -S ' + Connection.Server;

  if Connection.Database <> '' then
    CmdParam := CmdParam + ' -d ' + Connection.Database;

  if (Connection is TMSConnection) and (TMSConnection(Connection).Authentication = auWindows) then
    CmdParam := CmdParam + ' -E'
  else begin
    if Connection.Username <> '' then
      CmdParam := CmdParam + ' -U ' + Connection.Username;
    CmdParam := CmdParam + ' -P ' + Connection.Password;
  end;

  if (SQL <> nil) and (SQL.Count > 0) then begin
  {$IFDEF CLR}
    TmpPath := StringBuilder.Create(MAX_PATH);
    TmpFileName := StringBuilder.Create(MAX_PATH);

    Assert(GetTempPath(MAX_PATH, TmpPath) <> 0, 'Error in call GetTempPath');
    Assert(GetTempFileName(TmpPath.ToString, 'sql'#0, 0, TmpFileName) <> 0, 'Error in call GetTempFileName');

    if ServerTool = stManagementStudio then begin
      SqlFileName := ChangeFileExt(TmpFileName.ToString, '.sql');
      SqlFileHandle := FileCreate(SqlFileName);
      //Assert(SqlFileHandle > 0);
      FileClose(SqlFileHandle);
      TmpFiles.Add(SqlFileName);
      SQl.SaveToFile(SqlFileName);
    end
    else
      SQl.SaveToFile(TmpFileName.ToString);

    TmpFiles.Add(TmpFileName.ToString);
    case ServerTool of
      stQueryAnalyser:
        CmdParam := CmdParam + ' -f ' + TmpFileName.ToString;
      stManagementStudio:
        CmdParam := '"' + SqlFileName + '" ' + CmdParam;
      else
        Assert(False);
    end;
  {$ELSE}
    Assert(GetTempPath(MAX_PATH, TmpPath) <> 0, 'Error in call GetTempPath');

    Assert(GetTempFileName(TmpPath, 'sql'#0, 0, TmpFileName) <> 0, 'Error in call GetTempFileName');

    if ServerTool = stManagementStudio then begin
      SqlFileName := ChangeFileExt(TmpFileName, '.sql');
      SqlFileHandle := FileCreate(SqlFileName);
      Assert(SqlFileHandle > 0);
      FileClose(SqlFileHandle);
      TmpFiles.Add(SqlFileName);
      SQl.SaveToFile(SqlFileName);
    end
    else
      SQl.SaveToFile(TmpFileName);
      
    TmpFiles.Add(TmpFileName);
    case ServerTool of
      stQueryAnalyser:
        CmdParam := CmdParam + ' -f ' + StrPas(TmpFileName);
      stManagementStudio:
        CmdParam := '"' + SqlFileName + '" ' + CmdParam;
      else
        Assert(False);
    end;
  {$ENDIF}
  end;

  Cmd := GetServerToolCommand(ServerTool);

{$IFDEF CLR}
  Code := ShellExecute(0, '', Cmd, CmdParam, '', SW_SHOWNORMAL);
{$ELSE}
  Code := ShellExecute(0, nil, @Cmd[1], @CmdParam[1], nil, SW_SHOWNORMAL);
{$ENDIF}
  if Code <= 32 then
    raise Exception.CreateFmt('Error executing "%s %s". Code = %d', [Cmd, CmdParam, Code]);
end;

procedure RunServerToolConnection(ServerTool: TServerTool; Connection: TCustomMSConnection; const SQLText: _string);
var
  SQL: _TStringList;
begin
  SQL := _TStringList.Create;
  try
    SQL.Add(SQLtext);
    RunServerTool(ServerTool, Connection, SQL);
  finally
    SQL.Free;
  end;
end;

procedure RunServerToolDataSet(ServerTool: TServerTool; DataSet: TCustomMSDataSet);
begin
  RunServerToolConnection(ServerTool, DataSet.Connection, TMSAccessUtils.GetOLEDBSQL(DataSet));
end;

procedure RunServerToolMSSQL(ServerTool: TServerTool; MSSQL: TMSSQL);
begin
  RunServerToolConnection(ServerTool, MSSQL.Connection, TMSAccessUtils.GetOLEDBSQL(MSSQL));
end;

{ TMSConnectStringProperty }

function TMSConnectStringPropertyEditor.GetAttributes: TPropertyAttributes;
begin
  Result := [paDialog];
end;

procedure TMSConnectStringPropertyEditor.DoActivate(Sender: TObject);
var
  DataInit: IDataInitialize;
  DBPrompt: IDBPromptInitialize;
  DataSource: IUnknown;
  InitStr: {$IFDEF CLR}string{$ELSE}PWideChar{$ENDIF};
  InitialString: WideString;
begin
  FSucceeded := False;
  DataInit := CreateComObject(CLSID_DataLinks) as IDataInitialize;
  InitialString := FConnectString;

  if Pos('Provider=', FConnectString) = 0 then begin
    if InitialString <> '' then
      InitialString := InitialString + ';';
    InitialString := InitialString + 'Provider=SQLOLEDB.1';
  end;

  DataInit.GetDataSource(nil, CLSCTX_INPROC_SERVER,
  {$IFDEF CLR}
    InitialString,
  {$ELSE}
    PWideChar(InitialString),
  {$ENDIF}
    IID_IUnknown, DataSource);
  DBPrompt := CreateComObject(CLSID_DataLinks) as IDBPromptInitialize;

  if Succeeded(DBPrompt.PromptDataSource(nil, FForm.Handle,
    DBPROMPTOPTIONS_PROPERTYSHEET + DBPROMPTOPTIONS_DISABLE_PROVIDER_SELECTION, 0, nil, nil, IID_IUnknown, DataSource)) then
  begin
    InitStr := nil;
    DataInit.GetInitializationString(DataSource, True, InitStr );
    FConnectString := InitStr;
    FSucceeded := True;
  end;

  PostMessage(FForm.Handle, WM_CLOSE, 0, 0);
end;

procedure TMSConnectStringPropertyEditor.Edit;
begin
  FConnectString := TCustomMSConnection(GetComponent(0)).ConnectString;
  FForm := TForm.Create(nil);
  try
    FForm.BorderStyle := bsNone;
    FForm.Position := poScreenCenter;
    FForm.Width := 10;
    FForm.Height := 10;
    FForm.OnActivate := DoActivate;
    FForm.ShowModal;
  finally
    FForm.Free;
  end;
  if FSucceeded then begin {Cannot move to DoActivate}
    TCustomMSConnection(GetComponent(0)).ConnectString := FConnectString;
    Modified;
  end;
end;

{ TMSServerNamePropertyEditor }

function TMSServerNamePropertyEditor.GetAttributes: TPropertyAttributes;
begin
  Result := [paValueList];
end;

function TMSServerNamePropertyEditor.AutoFill: Boolean;
begin
  Result := False;
end;

procedure TMSServerNamePropertyEditor.GetValues(Proc: TGetXStrProc);
var
  List: _TStringList;
  i: integer;
  OldCursor: TCursor;
  Connection: TMSConnection;
  IsEverywhere: boolean;
begin
  List := _TStringList.Create;
  OldCursor := Screen.Cursor;
  Screen.Cursor := crSQLWait;
  try
    Connection := nil;
    if GetComponent(0) is TMSConnection then
      Connection := GetComponent(0) as TMSConnection;

    IsEverywhere := (Connection <> nil) and (Connection.Options.Provider = prCompact);
    if not IsEverywhere then begin
      if CachedServerList = nil then
        CachedServerList := _TStringList.Create;

      if CachedServerList.Count = 0 then begin
        GetServerList(List);
        CachedServerList.Assign(List);
      end else
        List.Assign(CachedServerList);
    end;

    for i := 0 to List.Count - 1 do
      Proc(List[i]);
  finally
    List.Free;
    Screen.Cursor := OldCursor;
  end;
end;

{ TMSDatabaseNamePropertyEditor }

procedure TMSDatabaseNamePropertyEditor.GetDialogOptions(Dialog: TOpenDialog);
begin
{$IFDEF MSWINDOWS}
  Dialog.Filter := 'SQL Server Database Files (*.sdf)|*.sdf|All Files (*.*)|*.*';
{$ELSE}
  Dialog.Filter := 'All Files (*)|*';
{$ENDIF}
  Dialog.Options := Dialog.Options + [ofPathMustExist];
end;

function TMSDatabaseNamePropertyEditor.GetAttributes: TPropertyAttributes;
var
  Connection: TCustomMSConnection;
begin
  Connection := nil;
  if GetComponent(0) is TCustomMSConnection then
    Connection := GetComponent(0) as TCustomMSConnection
  else
    if GetComponent(0) is TCustomMSDataset then
      Connection := TCustomMSDataset(GetComponent(0)).Connection as TCustomMSConnection;
  if Connection = nil then
    Exit;

  if Connection.Options.Provider <> prCompact then
    Result := [paValueList]
  else
    Result := [paRevertable, paDialog, paMultiSelect];
end;

function TMSDatabaseNamePropertyEditor.AutoFill: boolean;
begin
  Result := False;
end;

procedure TMSDatabaseNamePropertyEditor.Edit;
var
  OpenDialog: TOpenDialog;
begin
  OpenDialog := TOpenDialog.Create(nil);
  GetDialogOptions(OpenDialog);
  if OpenDialog.Execute then
    SetValue(_string(OpenDialog.FileName));
  OpenDialog.Free;
end;

procedure TMSDatabaseNamePropertyEditor.GetValues(Proc: TGetXStrProc);
var
  List: _TStringList;
  Connection: TCustomMSConnection;
  i: integer;
  OldConnected: boolean;
begin
  Connection := nil;
  if GetComponent(0) is TCustomMSConnection then
    Connection := GetComponent(0) as TCustomMSConnection
  else
    if GetComponent(0) is TCustomMSDataset then
      Connection := TCustomMSDataset(GetComponent(0)).Connection as TCustomMSConnection;
  if Connection = nil then
    Exit;

  OldConnected := Connection.Connected;
  List := _TStringList.Create;
  try
    try
      GetDatabasesList(Connection, List);
    except
      {$IFDEF FPC}FindRootDesigner(GetComponent(0) as TComponent){$ELSE}Designer{$ENDIF}.Modified;
      raise;
    end;

    List.Sort;
    for i := 0 to List.Count - 1 do
      Proc(List[i]);
  finally
    List.Free;

    if (OldConnected <> Connection.Connected) and
      ({$IFDEF FPC}FindRootDesigner(GetComponent(0) as TComponent){$ELSE}Designer{$ENDIF} <> nil) then
      {$IFDEF FPC}FindRootDesigner(GetComponent(0) as TComponent){$ELSE}Designer{$ENDIF}.Modified;
  end;
end;

{$IFNDEF STD}
{ TMSQueuePropertyEditor }

function TMSQueuePropertyEditor.GetAttributes: TPropertyAttributes;
begin
  Result := [paValueList];
end;

procedure TMSQueuePropertyEditor.GetValues(Proc: TGetXStrProc);
var
  List: _TStringList;
  Connection: TCustomMSConnection;
  ServiceBroker: TMSServiceBroker;
  i: integer;
  OldConnected: boolean;
begin
  ServiceBroker := nil;
  Connection := nil;
  if GetComponent(0) is TMSServiceBroker then begin
    ServiceBroker := TMSServiceBroker(GetComponent(0));
    Connection := ServiceBroker.Connection;
  end;
  if Connection = nil then
    Exit;

  OldConnected := Connection.Connected;
  List := _TStringList.Create;
  try
    try
      ServiceBroker.GetQueueNames(List);
    except
      {$IFDEF FPC}FindRootDesigner(GetComponent(0) as TComponent){$ELSE}Designer{$ENDIF}.Modified;
      raise;
    end;
    List.Sort;

    for i := 0 to List.Count - 1 do
      Proc(List[i]);
  finally
    List.Free;

    if (OldConnected <> Connection.Connected) and
      ({$IFDEF FPC}FindRootDesigner(GetComponent(0) as TComponent){$ELSE}Designer{$ENDIF} <> nil) then
      {$IFDEF FPC}FindRootDesigner(GetComponent(0) as TComponent){$ELSE}Designer{$ENDIF}.Modified;
  end;
end;

{ TMSServicePropertyEditor }

function TMSServicePropertyEditor.GetAttributes: TPropertyAttributes;
begin
  Result := [paValueList, paSortList, paMultiSelect];
end;

procedure TMSServicePropertyEditor.GetValues(Proc: TGetXStrProc);
var
  List: _TStringList;
  Connection: TCustomMSConnection;
  ServiceBroker: TMSServiceBroker;
  i: integer;
  OldConnected: boolean;
begin
  ServiceBroker := nil;
  Connection := nil;
  if GetComponent(0) is TMSServiceBroker then begin
    ServiceBroker := TMSServiceBroker(GetComponent(0));
    Connection := ServiceBroker.Connection;
  end;
  if Connection = nil then
    Exit;

  OldConnected := Connection.Connected;
  List := _TStringList.Create;
  try
    try
      ServiceBroker.GetServiceNames(List);
    except
      {$IFDEF FPC}FindRootDesigner(GetComponent(0) as TComponent){$ELSE}Designer{$ENDIF}.Modified;
      raise;
    end;

    for i := 0 to List.Count - 1 do
      Proc(List[i]);
  finally
    List.Free;

    if (OldConnected <> Connection.Connected) and
      ({$IFDEF FPC}FindRootDesigner(GetComponent(0) as TComponent){$ELSE}Designer{$ENDIF} <> nil) then
      {$IFDEF FPC}FindRootDesigner(GetComponent(0) as TComponent){$ELSE}Designer{$ENDIF}.Modified;
  end;
end;

{ TMSContractPropertyEditor }

function TMSContractPropertyEditor.GetAttributes: TPropertyAttributes;
begin
  Result := [paValueList];
end;

procedure TMSContractPropertyEditor.GetValues(Proc: TGetXStrProc);
var
  List: _TStringList;
  Connection: TCustomMSConnection;
  ServiceBroker: TMSServiceBroker;
  i: integer;
  OldConnected: boolean;
begin
  ServiceBroker := nil;
  Connection := nil;
  if GetComponent(0) is TMSServiceBroker then begin
    ServiceBroker := TMSServiceBroker(GetComponent(0));
    Connection := ServiceBroker.Connection;
  end;
  if Connection = nil then
    Exit;

  OldConnected := Connection.Connected;
  List := _TStringList.Create;
  try
    try
      ServiceBroker.GetContractNames(List);
    except
      {$IFDEF FPC}FindRootDesigner(GetComponent(0) as TComponent){$ELSE}Designer{$ENDIF}.Modified;
      raise;
    end;
    List.Sort;

    for i := 0 to List.Count - 1 do
      Proc(List[i]);
  finally
    List.Free;

    if (OldConnected <> Connection.Connected) and
      ({$IFDEF FPC}FindRootDesigner(GetComponent(0) as TComponent){$ELSE}Designer{$ENDIF} <> nil) then
      {$IFDEF FPC}FindRootDesigner(GetComponent(0) as TComponent){$ELSE}Designer{$ENDIF}.Modified;
  end;
end;

{ TMSTargetDatabaseNamePropertyEditor }

function TMSTargetDatabaseNamePropertyEditor.GetAttributes: TPropertyAttributes;
begin
  Result := [paValueList];
end;

procedure TMSTargetDatabaseNamePropertyEditor.GetValues(Proc: TGetXStrProc);
var
  List: _TStringList;
  Connection: TCustomMSConnection;
  i: integer;
  OldConnected: boolean;
begin
  Connection := nil;
  if GetComponent(0) is TMSServiceBroker then
    Connection := TMSServiceBroker(GetComponent(0)).Connection as TCustomMSConnection;
  if Connection = nil then
    Exit;

  OldConnected := Connection.Connected;
  List := _TStringList.Create;
  try
    try
      GetDatabasesList(Connection, List);
    except
      {$IFDEF FPC}FindRootDesigner(GetComponent(0) as TComponent){$ELSE}Designer{$ENDIF}.Modified;
      raise;
    end;
    List.Sort;

    for i := 0 to List.Count - 1 do
      Proc(List[i]);
  finally
    List.Free;

    if (OldConnected <> Connection.Connected) and
      ({$IFDEF FPC}FindRootDesigner(GetComponent(0) as TComponent){$ELSE}Designer{$ENDIF} <> nil) then
      {$IFDEF FPC}FindRootDesigner(GetComponent(0) as TComponent){$ELSE}Designer{$ENDIF}.Modified;
  end;
end;

{ TMSTableNamesEditor }

function TMSTableNamesEditor.GetAttributes: TPropertyAttributes;
begin
  Result := [paDialog];
end;

procedure TMSTableNamesEditor.Edit;
var
  Comp: TComponent;
  Conn: TCustomDAConnection;

begin
  Comp := TComponent(GetComponent(0));
  Conn := TMSDesignUtils.GetConnection(Comp);
  if Conn = nil then
    Exit;

  with TMSNamesEditorForm.Create(nil, TMSDesignUtils) do
    try
      Connection := Conn as TMSConnection;
      if Comp is TMSDump then
        Names := TMSDump(Comp).TableNames
      else
        Assert(False);

      ShowModal;
      if ModalResult = mrOk then begin
        if Comp is TMSDump then
          TMSDump(Comp).TableNames := Names
        else
          Assert(False);
      end;
  finally
    Free;
  end;
end;

{ TMSLocaleIdentifierPropertyEditor }

function TMSLocaleIdentifierPropertyEditor.GetAttributes: TPropertyAttributes;
begin
  Result := [paValueList];
end;

procedure TMSLocaleIdentifierPropertyEditor.GetValues(Proc: TGetXStrProc);
var
  List: TStringList;
  i: integer;
begin
  List := TStringList.Create;
  try
    GetLocaleIdentifierList(List);
    for i := 0 to List.Count - 1 do
      Proc(List[i]);
  finally
    List.Free;
  end;
end;
{$ENDIF}

{ TMSConnectDialogPropertyEditor }

procedure TMSConnectDialogPropertyEditor.CheckComponent(const Value: string);
var
  Component: TComponent;
begin
  Component := {$IFDEF FPC}PropertyHook{$ELSE}Designer{$ENDIF}.GetComponent(Value);
  if Component <> nil then begin
    if not (Component is TMSConnectDialog) then
      Exit;
  end;
  FCheckProc(Value);
end;

procedure TMSConnectDialogPropertyEditor.GetValues(Proc: TGetStrProc);
begin
  FCheckProc := Proc;
  inherited GetValues(CheckComponent);
end;

{ TMSConnectionEditor }

procedure TMSConnectionEditor.ExecuteVerb(Index: integer);
begin
  if (Index = FQueryAnalyserIndex) and (FQueryAnalyserIndex <> -1) then
    RunServerTool(stQueryAnalyser, Component as TCustomMSConnection)
  else
    if (Index = FManagementStudioIndex) and (FManagementStudioIndex <> -1) then
      RunServerTool(stManagementStudio, Component as TCustomMSConnection)
    else
      inherited ExecuteVerb(Index);
end;

{$IFNDEF STD}
procedure TMSConnectionEditor.Convert;
begin
{$IFNDEF FPC}
  if Designer <> nil then begin
    Assert(Component is TMSConnection);
    TMSConnection(Component).Options.Provider := prCompact; 
    ConvertToClass(Self.Designer, Component, TMSCompactConnection);
  end;
{$ENDIF}
end;
{$ENDIF}

procedure TMSConnectionEditor.InitVerbs;
var
  ServerToolMethod: TVerbMethod;
  Connection: TCustomMSConnection;
begin
  AddVerb('Connection Editor...', TMSConnectionEditorForm, TMSDesignUtils);
{$IFNDEF STD}
{$IFNDEF FPC}
  AddVerb('Convert to TMSCompactConnection', Convert);
{$ENDIF}
{$ENDIF}
  
  Connection := Component as TCustomMSConnection;
  ServerToolMethod := nil;
  FQueryAnalyserIndex := -1;
  FManagementStudioIndex := -1;
  if (Connection <> nil) and (Connection.Options.Provider <> prCompact) then begin
    if IsServerToolInstalled(stQueryAnalyser) then
      FQueryAnalyserIndex := AddVerb('Query Analyzer...', ServerToolMethod);
    if IsServerToolInstalled(stManagementStudio) then
      FManagementStudioIndex := AddVerb('Management Studio...', ServerToolMethod);
  end;
end;

{$IFNDEF STD}
{ TMSCompactConnectionEditor }

procedure TMSCompactConnectionEditor.ExecuteVerb(Index: integer);
begin
  if (Index = FManagementStudioIndex) and (FManagementStudioIndex <> -1) then
    RunServerTool(stManagementStudio, Component as TCustomMSConnection)
  else
    inherited ExecuteVerb(Index);
end;

procedure TMSCompactConnectionEditor.Convert;
begin
{$IFNDEF FPC}
  if Designer <> nil then begin
    Assert(Component is TMSCompactConnection);
    ConvertToClass(Self.Designer, Component, TMSConnection);
  end;
{$ENDIF}
end;

procedure TMSCompactConnectionEditor.InitVerbs;
var
  ServerToolMethod: TVerbMethod;
  Connection: TCustomMSConnection;
begin
  AddVerb('Connection Editor...', TMSCompactConnectionEditorForm, TMSDesignUtils);
{$IFNDEF FPC}
  AddVerb('Convert to TMSConnection', Convert);
{$ENDIF}

  Connection := Component as TCustomMSConnection;
  ServerToolMethod := nil;
  FManagementStudioIndex := -1;
  if (Connection <> nil) then
    if IsServerToolInstalled(stManagementStudio) then
      FManagementStudioIndex := AddVerb('Management Studio...', ServerToolMethod);
end;
{$ENDIF}

{ TMSQueryEditor }

procedure TMSQueryEditor.ExecuteVerb(Index: integer);
begin
  if (Index = FQueryAnalyserIndex) and (FQueryAnalyserIndex <> -1) then
    RunServerToolDataSet(stQueryAnalyser, Component as TCustomMSDataSet)
  else
    if (Index = FManagementStudioIndex) and (FManagementStudioIndex <> -1) then
      RunServerToolDataSet(stManagementStudio, Component as TCustomMSDataSet)
    else
      inherited ExecuteVerb(Index);
end;

procedure TMSQueryEditor.InitVerbs;
var
  Connection: TCustomMSConnection;
  ServerToolMethod: TVerbMethod;
begin
  AddVerb('Fields &Editor...', ShowFieldsEditor);
  AddVerb('MSQuery E&ditor...', TMSQueryEditorForm, TMSDesignUtils);
  AddVerb('Data Editor...', ShowDataEditor);

  Connection := (Component as TCustomMSDataSet).Connection;
  ServerToolMethod := nil;
  FQueryAnalyserIndex := -1;
  FManagementStudioIndex := -1;
  if (Connection <> nil) and (Connection.Options.Provider <> prCompact) then begin
    if IsServerToolInstalled(stQueryAnalyser) then
      FQueryAnalyserIndex := AddVerb('Query Analyzer...', ServerToolMethod);
    if IsServerToolInstalled(stManagementStudio) then
      FManagementStudioIndex := AddVerb('Management Studio...', ServerToolMethod);
  end;

  inherited;
end;

{ TMSSQLEditor }

procedure TMSSQLEditor.InitVerbs;
begin
  AddVerb('MSSQL E&ditor...', TMSSQLEditorForm, TMSDesignUtils);
end;

{ TMSStoredProcEditor }

procedure TMSStoredProcEditor.ExecuteVerb(Index: integer);
begin
{$IFNDEF FPC}
  if Index = GetVerbCount - 1 then
    ConvertToClass(Designer, Component, TMSQuery)
  else
{$ENDIF}
    inherited ExecuteVerb(Index);
end;

procedure TMSStoredProcEditor.InitVerbs;
begin
  AddVerb('Fields &Editor...', ShowFieldsEditor);
  AddVerb('MSStoredProc E&ditor...', TMSStoredProcEditorForm, TMSDesignUtils);
  AddVerb('Data Editor...', ShowDataEditor);
{$IFNDEF FPC}
  AddVerb('Convert to TMSQuery', ShowDataEditor);
{$ENDIF}

  inherited;
end;

{ TMSTableEditor }

procedure TMSTableEditor.InitVerbs;
begin
  AddVerb('Fields &Editor...', ShowFieldsEditor);
  AddVerb('MSTable E&ditor...', TDATableEditorForm, TMSDesignUtils);
  AddVerb('Data Editor...', ShowDataEditor);

  inherited;
end;

{ TMSUpdateSQLEditor }

procedure TMSUpdateSQLEditor.InitVerbs;
begin
  inherited;
  AddVerb('MSUpdateSQL E&ditor...', TMSUpdateSQLEditorForm, TMSDesignUtils);
end;

{ TMSScriptEditor }

procedure TMSScriptEditor.InitVerbs;
begin
  inherited;
  AddVerb('MSScript E&ditor...', TDAScriptEditorForm, TMSDesignUtils);
end;

{$IFNDEF STD}
{ TMSDumpEditor }

procedure TMSDumpEditor.InitVerbs;
begin
  inherited;
  AddVerb('MSDump E&ditor...', TMSDumpEditorForm, TMSDesignUtils);
end;
{$ENDIF}

procedure TMSTableDataEditor.InitVerbs;
begin
  AddVerb('Fields &Editor...', ShowFieldsEditor);

  inherited;
end;

{ TMSTableTypeNameEditor }

function TMSTableTypeNameEditor.GetAttributes: TPropertyAttributes;
begin
  Result := [paValueList];
end;

function TMSTableTypeNameEditor.AutoFill: boolean;
begin
  Result := False;
end;

procedure TMSTableTypeNameEditor.GetValues(Proc: TGetXStrProc);
var
  List: _TStringList;
  i: integer;
  Component: TComponent;
  UsedConnection: TCustomMSConnection;
begin
  Assert(PropCount > 0, 'PropCount = 0');
  Component := GetComponent(0) as TComponent;
  Assert(Component is TMSTableData, Component.ClassName);

  UsedConnection := TMSAccessUtils.UsedConnection(TMSTableData(Component));
  if UsedConnection = nil then
    Exit;

  List := _TStringList.Create;
  try
    UsedConnection.GetTableTypeNames(List);
    for i := 0 to List.Count - 1 do
      Proc(List[i]);
  finally
    List.Free;
  end;
end;

{$IFNDEF FPC}
{ TMSConnectionList }

function TMSConnectionList.GetConnectionType: TCustomDAConnectionClass;
begin
  Result := TCustomMSConnection;
end;

{$IFDEF VER6P}

{ TMSDesignNotification }

function TMSDesignNotification.CreateConnectionList: TDAConnectionList;
begin
  Result := TMSConnectionList.Create;
end;

function TMSDesignNotification.GetConnectionPropertyName: string;
begin
  Result := 'Connection';
end;

procedure TMSDesignNotification.ItemInserted(const ADesigner: IDesigner; AItem: TPersistent);
begin
  if (AItem <> nil) and ((AItem is TCustomMSDataSet) or (AItem is TMSSQL) or (AItem is TMSScript) or 
  {$IFNDEF STD}
    (AItem is TMSLoader) or (AItem is TMSDump) or (AItem is TMSServiceBroker) or
    (AItem is TMSChangeNotification) or (AItem is TMSTransaction) or
  {$ENDIF}
    (AItem is TMSTableData) or
    (AItem is TMSDataSource)) then
    FItem := AItem;
end;

procedure TMSDesignNotification.SelectionChanged(const ADesigner: IDesigner;
  const ASelection: IDesignerSelections);
{$IFDEF CLR}
begin
end;
{$ELSE}
var
  ModuleServices: IOTAModuleServices;
  CurrentModule: IOTAModule;
  Project: IOTAProject;
  ProjectOptions: IOTAProjectOptions;
  DelphiPath: string;
  s: string;
begin
  CurrentProjectOutputDir := '';

{$IFDEF CLR}
  ModuleServices := BorlandIDE.ModuleServices;
{$ELSE}
  ModuleServices :=BorlandIDEServices as IOTAModuleServices;
{$ENDIF}

  CurrentModule := ModuleServices.CurrentModule;

  if CurrentModule.OwnerCount = 0 then
    Exit;

  Project := CurrentModule.Owners[0];

  ProjectOptions := Project.ProjectOptions;

  CurrentProjectOutputDir := Trim(ProjectOptions.Values['OutputDir']);

  if (CurrentProjectOutputDir <> '') then begin
    if (CurrentProjectOutputDir[1] = '.') then begin // relative path
      s := Trim(ExtractFilePath(Project.FileName));
      if s = '' then
        CurrentProjectOutputDir := ''
      else
        CurrentProjectOutputDir := IncludeTrailingBackslash(s) + CurrentProjectOutputDir;
    end
    else
    if Pos('$(DELPHI)', UpperCase(CurrentProjectOutputDir)) > 0 then begin
      DelphiPath := GetEnvironmentVariable('DELPHI');
      CurrentProjectOutputDir := StringReplace(CurrentProjectOutputDir, '$(DELPHI)', DelphiPath, [rfReplaceAll, rfIgnoreCase]);
    end;
  end
  else
    CurrentProjectOutputDir := Trim(ExtractFilePath(Project.FileName));
end;
{$ENDIF}
{$ENDIF}
{$ENDIF}

procedure Register;
begin
  // Register property editors
  RegisterPropertyEditor(TypeInfo(_string), TMSConnection, 'ConnectString', TMSConnectStringPropertyEditor);
  RegisterPropertyEditor(TypeInfo(_string), TMSConnection, 'Server', TMSServerNamePropertyEditor);
  RegisterPropertyEditor(TypeInfo(_string), TMSConnection, 'Database', TMSDatabaseNamePropertyEditor);
  RegisterPropertyEditor(TypeInfo(TCustomConnectDialog), TMSConnection, 'ConnectDialog', TMSConnectDialogPropertyEditor);

{$IFNDEF STD}
  RegisterPropertyEditor(TypeInfo(_string), TMSCompactConnection, 'Database', TMSDatabaseNamePropertyEditor);
  RegisterPropertyEditor(TypeInfo(TCustomConnectDialog), TMSCompactConnection, 'ConnectDialog', TMSConnectDialogPropertyEditor);
  RegisterPropertyEditor(TypeInfo(_string), TMSCompactConnectionOptions, 'LocaleIdentifier', TMSLocaleIdentifierPropertyEditor);
{$ENDIF}

  RegisterPropertyEditor(TypeInfo(_TStrings), TMSQuery, 'SQL', TDAPropertyEditor);
  RegisterPropertyEditor(TypeInfo(_TStrings), TMSQuery, 'SQLDelete', TDAPropertyEditor);
  RegisterPropertyEditor(TypeInfo(_TStrings), TMSQuery, 'SQLInsert', TDAPropertyEditor);
  RegisterPropertyEditor(TypeInfo(_TStrings), TMSQuery, 'SQLLock', TDAPropertyEditor);
  RegisterPropertyEditor(TypeInfo(_TStrings), TMSQuery, 'SQLRefresh', TDAPropertyEditor);
  RegisterPropertyEditor(TypeInfo(_TStrings), TMSQuery, 'SQLUpdate', TDAPropertyEditor);

  RegisterPropertyEditor(TypeInfo(_TStrings), TMSSQL, 'SQL', TDAPropertyEditor);

  RegisterPropertyEditor(TypeInfo(_TStrings), TMSStoredProc, 'SQL', nil);
  RegisterPropertyEditor(TypeInfo(_TStrings), TMSStoredProc, 'SQLDelete', TDAPropertyEditor);
  RegisterPropertyEditor(TypeInfo(_TStrings), TMSStoredProc, 'SQLInsert', TDAPropertyEditor);
  RegisterPropertyEditor(TypeInfo(_TStrings), TMSStoredProc, 'SQLLock', TDAPropertyEditor);
  RegisterPropertyEditor(TypeInfo(_TStrings), TMSStoredProc, 'SQLRefresh', TDAPropertyEditor);
  RegisterPropertyEditor(TypeInfo(_TStrings), TMSStoredProc, 'SQLUpdate', TDAPropertyEditor);

  RegisterPropertyEditor(TypeInfo(_TStrings), TMSUpdateSQL, 'InsertSQL', TDAPropertyEditor);
  RegisterPropertyEditor(TypeInfo(_TStrings), TMSUpdateSQL, 'ModifySQL', TDAPropertyEditor);
  RegisterPropertyEditor(TypeInfo(_TStrings), TMSUpdateSQL, 'DeleteSQL', TDAPropertyEditor);
  RegisterPropertyEditor(TypeInfo(_TStrings), TMSUpdateSQL, 'RefreshSQL', TDAPropertyEditor);
  RegisterPropertyEditor(TypeInfo(_TStrings), TMSUpdateSQL, 'LockSQL', TDAPropertyEditor);
  RegisterPropertyEditor(TypeInfo(Boolean), TMSDataSetOptions, 'AllFieldsEditable', nil);

  RegisterPropertyEditor(TypeInfo(_string), TMSMetadata, 'DatabaseName', TMSDatabaseNamePropertyEditor);

  RegisterPropertyEditor(TypeInfo(_TStrings), TMSScript, 'SQL', TDAPropertyEditor);

{$IFNDEF STD}
  RegisterPropertyEditor(TypeInfo(_string), TMSServiceBroker, 'Service', TMSServicePropertyEditor);
  RegisterPropertyEditor(TypeInfo(_string), TMSDump, 'TableNames', TMSTableNamesEditor);
{$ENDIF}
  RegisterPropertyEditor(TypeInfo(_string), TMSTableData, 'TableTypeName', TMSTableTypeNameEditor);

  // Register component editors
  DARegisterComponentEditor(TMSConnection, TMSConnectionEditor, TMSConnectionEditorForm, TMSDesignUtils);
{$IFNDEF STD}
  DARegisterComponentEditor(TMSCompactConnection, TMSCompactConnectionEditor, TMSCompactConnectionEditorForm, TMSDesignUtils);
{$ENDIF}
  DARegisterComponentEditor(TMSQuery, TMSQueryEditor, TMSQueryEditorForm, TMSDesignUtils);
  DARegisterComponentEditor(TMSSQL, TMSSQLEditor, TMSSQLEditorForm, TMSDesignUtils);
  DARegisterComponentEditor(TMSTable, TMSTableEditor, TDATableEditorForm, TMSDesignUtils);
  DARegisterComponentEditor(TMSStoredProc, TMSStoredProcEditor, TMSStoredProcEditorForm, TMSDesignUtils);
  DARegisterComponentEditor(TMSUpdateSQL, TMSUpdateSQLEditor, TMSUpdateSQLEditorForm, TMSDesignUtils);
  DARegisterComponentEditor(TMSScript, TMSScriptEditor, TDAScriptEditorForm, TMSDesignUtils);
{$IFNDEF STD}
  DARegisterComponentEditor(TMSLoader, TDALoaderEditor, nil, TMSDesignUtils);
  DARegisterComponentEditor(TMSDump, TMSDumpEditor, TMSDumpEditorForm, TMSDesignUtils);

  DARegisterComponentEditor(TMSServiceBroker, TDAComponentEditor, nil, TMSDesignUtils);
  DARegisterComponentEditor(TMSMetaData, TDAComponentEditor, nil, TMSDesignUtils);
{$ENDIF}
  DARegisterComponentEditor(TMSTableData, TMSTableDataEditor, nil, TMSDesignUtils);

{$IFNDEF FPC}
  RegisterComponentEditor(TMSDataSource, TCRDataSourceEditor);

  Menu.AddItems({$IFDEF CLR}WinUtils{$ELSE}SysInit{$ENDIF}.HInstance);
{$ENDIF}
end;

procedure ClearTmpFiles;
var
  i: integer;
begin
  if TmpFiles = nil then
    exit;

  for i := 0 to TmpFiles.Count - 1 do
    DeleteFile(TmpFiles[i]);
end;

{$IFNDEF FPC}
{$IFDEF VER6P}
var
  Notificator: TMSDesignNotification;
{$ENDIF}
{$ENDIF}

initialization

  TmpFiles := nil;
  TmpFiles := _TStringList.Create;

{$IFNDEF FPC}
{$IFDEF VER6P}
  Notificator := TMSDesignNotification.Create;
  RegisterDesignNotification(Notificator);
{$ENDIF}
{$ENDIF}

  ClearTmpFiles;
  CachedServerList.Free;

{$IFNDEF FPC}
{$IFDEF VER6P}
  UnRegisterDesignNotification(Notificator);
{$ENDIF}
{$ENDIF}

end.
