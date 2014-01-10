
//////////////////////////////////////////////////
//  DB Access Components
//  Copyright © 1998-2012 Devart. All right reserved.
//  DADesign
//////////////////////////////////////////////////

{$IFNDEF CLR}

{$I Dac.inc}

unit DADesign;
{$ENDIF}
interface

uses
{$IFDEF MSWINDOWS}
  Windows, Messages, Registry,
{$ENDIF}
  Graphics, Controls, Forms, Dialogs, StdCtrls,
{$IFDEF CLR}
  Borland.Vcl.Design.DesignEditors, Borland.Vcl.Design.DesignIntf,
  Borland.Vcl.Design.FldLinks, Borland.Vcl.Design.StrEdit, Borland.Vcl.Design.ValueEdit,
  System.Runtime.InteropServices,
{$ELSE}
{$IFDEF FPC}
  PropEdits, ComponentEditors, CRFldLinks, CRValueEdit,
{$ELSE}
  {$IFDEF VER6P}DesignIntf, DesignEditors, {$ELSE}DsgnIntf, {$ENDIF}
  {$IFNDEF BCB}
    FldLinks, ColnEdit,
    {$IFDEF VER6P}StrEdit, ValueEdit, {$ELSE}CRValueEdit, {$ENDIF}
  {$ELSE}
    CRFldLinks, CRValueEdit,
  {$ENDIF}
{$ENDIF}
{$ENDIF}
{$IFDEF USE_CODE_EDITOR}
  StFilSys,
{$ENDIF}
{$IFDEF DBTOOLS}
  Menus,
  {$IFDEF CLR}Borland.Vcl.Design.DesignMenus{$ELSE}DesignMenus{$ENDIF},
  DBToolsIntf,
  DBToolsClient,
{$ENDIF}
  SysUtils, Classes, TypInfo, CRDesign,
  CRTypes, DBAccess, DAScript, DALoader, DADump,
  CREditor, CRDesignUtils, DADesignUtils, CRParser;

{$IFNDEF FPC}
  procedure ConvertToClass(Designer:{$IFDEF VER6P}IDesigner{$ELSE}IFormDesigner{$ENDIF}; Component: TComponent; NewClass: TComponentClass);
{$ENDIF}

{ ------------  DAC property editors ----------- }
type
{$IFDEF CRUNICODE}
  TXStringProperty = TWideStringProperty;
  TGetXStrProc = TGetWideStrProc;
{$ELSE}
  TXStringProperty = TStringProperty;
  TGetXStrProc = TGetStrProc;
{$ENDIF}

  TXStringPropertyUseConnect = class (TXStringProperty)
  private
  {$IFDEF DARWIN}
    FNeedFill: boolean;
  {$ENDIF}
  protected
    function InternalGetValues(Proc: TGetXStrProc): TCustomDAConnection; virtual; abstract;
  public
  {$IFDEF DARWIN}
    procedure Activate; override;
  {$ENDIF}
    procedure GetValues(Proc: TGetXStrProc); override;
  end;

  TDAPropertyEditor = class (TPropertyEditor)
  public
    function GetAttributes: TPropertyAttributes; override;
    function GetValue: string; override;
    procedure Edit; override;
  end;

  TDAPasswordProperty = class(TXStringProperty)
  protected
    FActivated: boolean;
{$IFNDEF CLR}
  public
{$ENDIF}
    procedure Initialize; override;
{$IFDEF CLR}
  public
{$ENDIF}
    procedure Activate; override;
    function {$IFDEF CRUNICODE}GetValueW{$ELSE}GetValue{$ENDIF}: _string; override;
  end;

  TDATableNameEditor = class (TXStringPropertyUseConnect)
  protected
    function InternalGetValues(Proc: TGetXStrProc): TCustomDAConnection; override;
  public
    function GetAttributes: TPropertyAttributes; override;
    function AutoFill: boolean; override;
  end;

  TDAUpdatingTableEditor = class (TXStringPropertyUseConnect)
  protected
    function InternalGetValues(Proc: TGetXStrProc): TCustomDAConnection; override;
  public
    function GetAttributes: TPropertyAttributes; override;
  end;

  TDADatabaseNameEditor = class (TXStringPropertyUseConnect)
  protected
    function InternalGetValues(Proc: TGetXStrProc): TCustomDAConnection; override;
  public
    function GetAttributes: TPropertyAttributes; override;
    function AutoFill: boolean; override;
  end;

  TDASPNameEditor = class (TXStringPropertyUseConnect)
  protected
    function InternalGetValues(Proc: TGetXStrProc): TCustomDAConnection; override;
  public
    function GetAttributes: TPropertyAttributes; override;
    function AutoFill: boolean; override;
  end;

  TDAFieldDefsListEditor = class (TXStringPropertyUseConnect) // TDATableOrderFieldsEditor
  protected
    function InternalGetValues(Proc: TGetXStrProc): TCustomDAConnection; override;
  public
    function GetAttributes: TPropertyAttributes; override;
    function AutoFill: boolean; override;
  end;

  TDAFieldsListEditor = class (TXStringPropertyUseConnect) // TDADataSetIndexFieldNamesEditor
  protected
    function InternalGetValues(Proc: TGetXStrProc): TCustomDAConnection; override;
  public
    function GetAttributes: TPropertyAttributes; override;
    function AutoFill: boolean; override;
  end;

  TDALoaderTableNameEditor = class (TXStringPropertyUseConnect)
  protected
    function InternalGetValues(Proc: TGetXStrProc): TCustomDAConnection; override;
  public
    function GetAttributes: TPropertyAttributes; override;
    function AutoFill: boolean; override;
  end;

{$IFDEF UNIX}
  TDADataSetMasterFieldsEditor = class (TCRFieldLinkProperty)
{$ELSE}
{$IFDEF BCB}
  TDADataSetMasterFieldsEditor = class (TCRFieldLinkProperty)
{$ELSE}
{$IFDEF FPC}
  TDADataSetMasterFieldsEditor = class (TCRFieldLinkProperty)
{$ELSE}
  TDADataSetMasterFieldsEditor = class (TFieldLinkProperty)
{$ENDIF}
{$ENDIF}
{$ENDIF}
  protected
    function GetMasterFields: string; override;
    procedure SetMasterFields(const Value: string); override;
    function GetIndexFieldNames: string; override;
    procedure SetIndexFieldNames(const Value: string); override;
  end;

  TVariantEditor = class (TStringProperty)
  public
    function GetAttributes: TPropertyAttributes; override;
    function GetValue: string; override;
    procedure SetValue(const Value: string); override;
  end;

  TDADatasetOrSQLProperty = class(TComponentProperty)
  private
    FCheckProc: TGetStrProc;
    procedure CheckComponent(const Value: string);
  public
    procedure GetValues(Proc: TGetStrProc); override;
  end;

  TDAUpdateSQLProperty = class(TComponentProperty)
  private
    FCheckProc: TGetStrProc;
    procedure CheckComponent(const Value: string);
  public
    procedure GetValues(Proc: TGetStrProc); override;
  end;

  TDAMetaDataKindEditor = class(TXStringPropertyUseConnect)
  protected
    function InternalGetValues(Proc: TGetXStrProc): TCustomDAConnection; override;
  public
    function GetAttributes: TPropertyAttributes; override;
  end;

{$IFDEF UNIX}
  TDAMetaDataRestrictionsEditor = class(TCRValueListProperty)
{$ELSE}
{$IFDEF BCB}
  TDAMetaDataRestrictionsEditor = class(TCRValueListProperty)
{$ELSE}
{$IFNDEF VER6P}
  TDAMetaDataRestrictionsEditor = class(TCRValueListProperty)
{$ELSE}
{$IFDEF FPC}
  TDAMetaDataRestrictionsEditor = class(TCRValueListProperty)
{$ELSE}
  TDAMetaDataRestrictionsEditor = class(TValueListProperty)
{$ENDIF}
{$ENDIF}
{$ENDIF}
{$ENDIF}
  private
    FList: TStringList;
  protected
    function GetStrings: TStrings; override;
    procedure SetStrings(const Value: TStrings); override;
  public
    destructor Destroy; override;
  end;

  TCustomDAConnectionClass = class of TCustomDAConnection;

{$IFNDEF FPC}
  TDAConnectionList = class(TObject)
  private
    procedure ListBoxDblClick(Sender: TObject);
    procedure ListBoxKeyPress(Sender: TObject; var Key: Char);
  {$IFDEF CLR}
    procedure FormShow(Sender: TObject);
  {$ENDIF}

  protected
    Items: TStrings;
    Form: TForm;
  {$IFDEF CLR}
    FormLeft: integer;
    FormTop: integer;
  {$ENDIF}
  
    procedure StrProc(const S: string);
    function GetConnectionType: TCustomDAConnectionClass; virtual; abstract;
  public
    constructor Create;
    destructor Destroy; override;

    function GetConnection(Component: TComponent; Designer: {$IFDEF VER6P}IDesigner{$ELSE}IFormDesigner{$ENDIF}): TCustomDAConnection;
  end;

{$IFDEF VER6P}

  TDAConnectionListClass = class of TDAConnectionList;

  TDADesignNotification = class(TInterfacedObject, IDesignNotification)
  protected
    FItem: TPersistent;
    FConnectionList: TDAConnectionList;
    DSItems: TStrings;
    procedure StrProc(const S: string);
    procedure DSStrProc(const S: string);
  public
    procedure ItemDeleted(const ADesigner: IDesigner; AItem: TPersistent); virtual;
    //overide this method on Product level and add all product specific classess
    procedure ItemInserted(const ADesigner: IDesigner; AItem: TPersistent); virtual; abstract;
    procedure ItemsModified(const ADesigner: IDesigner); virtual;
    procedure SelectionChanged(const ADesigner: IDesigner;
      const ASelection: IDesignerSelections); virtual;
    procedure DesignerOpened(const ADesigner: IDesigner
      {$IFNDEF K1}; AResurrecting: Boolean{$ENDIF}); virtual;
    procedure DesignerClosed(const ADesigner: IDesigner
      {$IFNDEF K1}; AGoingDormant: Boolean{$ENDIF}); virtual;

    function CreateConnectionList: TDAConnectionList; virtual; abstract;
    function GetConnectionPropertyName: string; virtual; abstract;
  end;
{$ENDIF}
{$ENDIF}

{$IFDEF USE_CODE_EDITOR}
  TStrEditDlgEx = class(TStrEditDlg)
  protected
    FLines: TStrings;
    function GetLines: TStrings; override;
    procedure SetLines(const Value: TStrings); override;
    function GetLinesControl: TWinControl; override;
  public
    function ShowModal: Integer; override;
  end;

  TStringListPropertyEx = class(TStringListProperty)
  public
    function EditDialog: TStrEditDlg; override;
  end;
{$ENDIF}

{ ------------  DAC component editors ----------- }
type
  TDAComponentEditor = class(TCRComponentEditor)
  private
  {$IFDEF DBTOOLS}
    FDBToolsVerbs: TDBToolsVerbs;    FDBToolsVerbIndex: integer;
  {$ENDIF}
    function GetDADesignUtilsClass: TDADesignUtilsClass;
  protected
  {$IFDEF DBTOOLS}
    procedure AddDBToolsVerbs(Verbs: TDBToolsVerbs);
    procedure DBToolsMenuExecute;
  {$ENDIF}
    class procedure ProcessEditorResult(ModalResult: integer;
      CREditor: TCREditorForm; Component: TComponent;
      Designer: {$IFDEF FPC}TIDesigner{$ELSE}{$IFDEF VER6P}IDesigner{$ELSE}IFormDesigner{$ENDIF}{$ENDIF}
    ); override;
    procedure ShowDataEditor; override;
    property DADesignUtilsClass: TDADesignUtilsClass read GetDADesignUtilsClass;
  public
    constructor Create(AComponent: TComponent;
      ADesigner: {$IFDEF FPC}TComponentEditorDesigner{$ELSE}{$IFDEF VER6P}IDesigner{$ELSE}IFormDesigner{$ENDIF}{$ENDIF}); override;

    function GetVerbCount: integer; override;
  {$IFDEF DBTOOLS}
    procedure PrepareItem(Index: integer; const AItem: IMenuItem); override;
  {$ENDIF}
  end;

  TDAConnectionEditor = class(TDAComponentEditor);

  TDASQLEditor = class(TDAComponentEditor);

  TDAScriptEditor = class(TDAComponentEditor);

  TDAUpdateSQLEditor = class(TDAComponentEditor);

  TDALoaderEditor = class(TDAComponentEditor)
  protected
    procedure InitVerbs; override;
    procedure ShowColEditor;
    procedure CreateColumns;
  end;

{$IFDEF MSWINDOWS}
  TDASQLMonitorEditor = class (TDAComponentEditor)
  protected
    procedure RunDBMonitor;
    procedure RunSQLMonitor;
    procedure InitVerbs; override;
  public
    procedure Edit; override;
  end;
{$ENDIF}

{$IFNDEF FPC}
  TCRDataSourceEditor = class(TDAComponentEditor)
  private
    Items: TStrings;
    FFirstProp: {$IFDEF VER6P}IProperty{$ELSE}TPropertyEditor{$ENDIF};
    procedure StrProc(const S: string);
    procedure ConvertToDataSource;
    procedure CheckEdit({$IFDEF VER6P}const Prop: IProperty{$ELSE}Prop: TPropertyEditor{$ENDIF});
  protected
    procedure InitVerbs; override;
  public
    constructor Create(Component: TComponent; aDesigner: {$IFDEF VER6P}IDesigner{$ELSE}IFormDesigner{$ENDIF}); override;
    procedure Edit; override;
  end;
{$ENDIF}

  TDesignMacros = class(TMacros)
  protected
//    function GetMacroValue(Macro: TMacro): string; override; TODO
  public
    procedure Scan(var SQL: _string); reintroduce;
  end;

procedure Register;

implementation

uses
{$IFDEF CLR}
  Borland.Studio.ToolsAPI, Borland.VCL.Design.DSDesign, Borland.Vcl.Design.ColnEdit,
{$ELSE}
  {$IFNDEF FPC}ToolsAPI,{$ENDIF}
{$IFNDEF BCB}
{$IFDEF MSWINDOWS}
  {$IFNDEF FPC}DSDesign,{$ENDIF}
{$ENDIF}
{$ENDIF}
{$ENDIF}
{$IFDEF VER6P}
  Variants,
{$ENDIF}
{$IFDEF MSWINDOWS}
  DBMonitorClient, DASQLMonitor, ShellAPI,
{$ENDIF}
{$IFDEF DBTOOLS}
  Download,
{$ENDIF}
  CRFunctions, CRAccess, DB,
  DAConnectionEditor, DATableEditor, DAQueryEditor, DASQLComponentEditor, DADataEditor, CRTabEditor,
  DAStoredProcEditor, DAScriptEditor, DADumpEditor,
  DAParamsFrame, DAMacrosFrame, DAUpdateSQLFrame, DAConsts;

var
  NotificationActive: boolean;

{$IFNDEF FPC}
procedure ConvertToClass(Designer:{$IFDEF VER6P}IDesigner{$ELSE}IFormDesigner{$ENDIF}; Component: TComponent; NewClass: TComponentClass);
type
  TPropData = record
    Component: TComponent;
    PropInfo: PPropInfo;
  end;
var
  AName: string;
  NewComponent: TComponent;
  DesignInfo: Longint;
  Instance: TComponent;

  FreeNotifies: TList;
  i, j, PropCount: integer;
{$IFDEF CLR}
  PropList: TPropList;
{$ELSE}
  PropList: PPropList;
{$ENDIF}
  Refs: array of TPropData;
  l: integer;

  Root: TComponent;
  OldNotificationActive: boolean;
begin
  DesignInfo := Component.DesignInfo;
  OldNotificationActive := NotificationActive;
  try
    NotificationActive := False;
    NewComponent := Designer.CreateComponent(NewClass, Component.Owner,
      Word(DesignInfo {$IFDEF CLR}shr 16{$ENDIF}), Word(DesignInfo  {$IFNDEF CLR}shr 16{$ENDIF}), 28, 28);
  finally
    NotificationActive := OldNotificationActive;
  end;
  AName := Component.Name;
  Component.Name := 'CRTemp_' + AName;
  FreeNotifies := TList.Create;
  try
{$IFDEF VER6P}
    Root := Designer.Root;
{$ELSE}
    Root := Designer.ContainerWindow;
{$ENDIF}
    for i := 0 to Root.ComponentCount - 1 do begin
      FreeNotifies.Add(Root.Components[i]);
    end;
    for i := 0 to FreeNotifies.Count - 1 do begin
      Instance := TComponent(FreeNotifies[i]);
  {$IFDEF CLR}
      PropList := GetPropList(Instance.ClassInfo, [tkClass]{$IFNDEF CLR}, nil{$IFDEF VER6P}, False{$ENDIF}{$ENDIF});
      PropCount := Length(PropList);
      if PropCount > 0 then begin
  {$ELSE}
      PropCount := GetPropList(Instance.ClassInfo, [tkClass]{$IFNDEF CLR}, nil{$IFDEF VER6P}, False{$ENDIF}{$ENDIF});
      if PropCount > 0 then begin
        GetMem(PropList, PropCount * SizeOf(PropList[0]));
        try
          GetPropList(Instance.ClassInfo, [tkClass]{$IFNDEF CLR}, PropList{$IFDEF VER6P}, False{$ENDIF}{$ENDIF});
  {$ENDIF}
          for j := 0 to PropCount - 1 do begin
            if (PropList[j].PropType <> nil) and
            ({$IFDEF CLR}KindOf(PropList[j].PropType){$ELSE}PropList[j].PropType^.Kind{$ENDIF}= tkClass)
              and (TComponent(GetObjectProp(Instance, PropList[j])) = Component)
            then begin
              l := Length(Refs);
              SetLength(Refs, l + 1);
              Refs[l].Component := Instance;
              Refs[l].PropInfo := PropList[j];
            end;
          end;
  {$IFNDEF CLR}
        finally
          FreeMem(PropList);
        end;
      end;
  {$ELSE}
      end;
  {$ENDIF}
    end;
  finally
    FreeNotifies.Free;
  end;
  NewComponent.Assign(Component);
  for i := 0 to Length(Refs) - 1 do begin
    SetObjectProp(Refs[i].Component, Refs[i].PropInfo, NewComponent);
  end;
  Component.Free;
  NewComponent.Name := AName;
  Designer.Modified;
end;
{$ENDIF}

{ TXStringPropertyNeedConnect }

{$IFDEF DARWIN}
procedure TXStringPropertyUseConnect.Activate;
begin
  FNeedFill := True;
end;
{$ENDIF}

procedure TXStringPropertyUseConnect.GetValues(Proc: TGetXStrProc);
{$IFDEF DARWIN}
var
  Connection: TCustomDAConnection;
begin
  if not FNeedFill then
    exit;

  FNeedFill := False;
  try
    Connection := InternalGetValues(Proc);
    if Connection <> nil then
      FNeedFill := Connection.Connected;
  except
    on E: Exception do
      Application.ShowException(E);
   end;
{$ELSE}
begin
  InternalGetValues(Proc);
{$ENDIF}
end;

{ TDAPropertyEditor }

function TDAPropertyEditor.GetAttributes: TPropertyAttributes;
begin
  Result := [paDialog, paReadOnly];
end;

function TDAPropertyEditor.GetValue: string;
var
{$IFDEF CLR}
  PropInfo: TPropInfo;
{$ELSE}
  PropInfo: PPropInfo;
{$ENDIF}
  Obj: TPersistent;
begin
  Obj := nil;
  PropInfo := GetPropInfo;
  if (PropInfo <> nil) and (PropInfo.PropType{$IFNDEF CLR}^{$ENDIF}.Kind = tkClass) then begin
  {$IFDEF CLR}
    Obj := GetObjectProp(GetComponent(0), PropInfo) as TPersistent;
  {$ELSE}
    Obj := TPersistent(NativeInt(GetPropValue(GetComponent(0), GetName)));
  {$ENDIF}
  end;
  if Obj <> nil then
    Result := '(' + string(GetPropType.Name) + ')' // CR 19906 S
  else
    Result := inherited GetValue;
end;

procedure TDAPropertyEditor.Edit;
var
  Component: TComponent;
  CREditorClass: TCREditorClass;
  CRDesignUtilsClass: TCRDesignUtilsClass;
begin
  Component := GetComponent(0) as TComponent;

  if FindComponentEditor(Component, CREditorClass, CRDesignUtilsClass) then
    TDAComponentEditor.ShowEditorEx(CREditorClass, CRDesignUtilsClass, Component, {$IFNDEF FPC}Designer{$ELSE}FindRootDesigner(Component){$ENDIF}, GetName)
  else
    Assert(False);
end;

{ TDAPasswordProperty }

procedure TDAPasswordProperty.Initialize;
begin
  inherited;
  
  FActivated := False;
end;

function TDAPasswordProperty.{$IFDEF CRUNICODE}GetValueW{$ELSE}GetValue{$ENDIF}: _string;
var
  i: Integer;
begin
  Result := inherited {$IFDEF CRUNICODE}GetValueW{$ELSE}GetValue{$ENDIF};
  if not FActivated then begin
    for i := 1 to Length(Result) do
      Result[i] := '*';
  end
  else
    FActivated := False;
end;

procedure TDAPasswordProperty.Activate;
begin
  inherited;
  
  FActivated := True;
end;

{ TDATableNameEditor }

function TDATableNameEditor.GetAttributes: TPropertyAttributes;
begin
  Result := [paValueList];
end;

function TDATableNameEditor.AutoFill: boolean;
begin
  Result := False;
end;

function TDATableNameEditor.InternalGetValues(Proc: TGetXStrProc): TCustomDAConnection;
var
  List: _TStringList;
  i: integer;
  Component: TComponent;
begin
  Assert(PropCount > 0, 'PropCount = 0');
  Component := GetComponent(0) as TComponent;
  Assert(Component is TCustomDADataSet, Component.ClassName);

  Result := TDBAccessUtils.UsedConnection(TCustomDADataSet(Component));
  if Result = nil then
    Exit;

  List := _TStringList.Create;
  try
    Result.GetTableNames(List);
    // List.Sort; 
    for i := 0 to List.Count - 1 do
      Proc(List[i]);
  finally
    List.Free;
  end;
end;

{ TDAUpdatingTableEditor }

function TDAUpdatingTableEditor.GetAttributes: TPropertyAttributes;
begin
  Result := [paValueList];
end;

function TDAUpdatingTableEditor.InternalGetValues(Proc: TGetXStrProc): TCustomDAConnection;
var
  Component: TComponent;
  DataSet: TCustomDADataset;
  TablesInfo: TCRTablesInfo;

  i: integer;
  OldSQL: _string;
  OldActive: boolean;

begin
  Component := TComponent(GetComponent(0));
  DataSet := Component as TCustomDADataset;

  if (DataSet = nil) then begin
    Result := nil;
    Exit;
  end;

  Result := TDBAccessUtils.UsedConnection(DataSet);
  if (Result = nil) or not Result.Connected then
    Exit;

  OldSQL := DataSet.SQL.text;
  OldActive := DataSet.Active;
  try
    TablesInfo := TDBAccessUtils.GetTablesInfo(DataSet);
    try
      if TablesInfo.Count = 0 then begin
        DataSet.AddWhere('0=1');
        DataSet.Active := True;
        TablesInfo := TDBAccessUtils.GetTablesInfo(DataSet);
      end;

      for i := 0 to TablesInfo.Count - 1 do
        Proc(TablesInfo[i].TableName);
    except
    end;

  finally
    if DataSet.SQL.Text <> OldSQL then
      DataSet.SQL.Text := OldSQL;

    if DataSet.Active <> OldActive then
      DataSet.Active := OldActive;
  end;
end;

{ TDADatabaseNameEditor }

function TDADatabaseNameEditor.GetAttributes: TPropertyAttributes;
begin
  Result := [paValueList];
end;

function TDADatabaseNameEditor.AutoFill: boolean;
begin
  Result := False;
end;

function TDADatabaseNameEditor.InternalGetValues(Proc: TGetXStrProc): TCustomDAConnection;
var
  List: _TStringList;
  i: integer;
  Component: TComponent;
begin
  Assert(PropCount > 0, 'PropCount = 0');
  Component := GetComponent(0) as TComponent;
  Assert(Component is TCustomDAConnection, Component.ClassName);

  Result := TCustomDAConnection(Component);
  if Result = nil then
    Exit;

  List := _TStringList.Create;
  try
    Result.GetDatabaseNames(List);
    List.Sort;
    for i := 0 to List.Count - 1 do
      Proc(List[i]);
  finally
    List.Free;
  end;
end;

{ TDASPNameEditor }

function TDASPNameEditor.GetAttributes: TPropertyAttributes;
begin
  Result := [paValueList];
end;

function TDASPNameEditor.AutoFill: boolean;
begin
  Result := False;
end;

function TDASPNameEditor.InternalGetValues(Proc: TGetXStrProc): TCustomDAConnection;
var
  List: _TStringList;
  i: integer;
  Component: TComponent;
begin
  Assert(PropCount > 0, 'PropCount = 0');
  Component := GetComponent(0) as TComponent;
  Assert(Component is TCustomDADataSet, Component.ClassName);

  Result := TDBAccessUtils.UsedConnection(TCustomDADataSet(Component));
  if Result = nil then
    Exit;

  List := _TStringList.Create;
  try
    Result.GetStoredProcNames(List);
    List.Sort;
    for i := 0 to List.Count - 1 do
      Proc(List[i]);
  finally
    List.Free;
  end;
end;

{ TDAFieldDefsListEditor }

function TDAFieldDefsListEditor.GetAttributes: TPropertyAttributes;
begin
  Result := [paValueList];
end;

function TDAFieldDefsListEditor.AutoFill: boolean;
begin
  Result := False;
end;

function TDAFieldDefsListEditor.InternalGetValues(Proc: TGetXStrProc): TCustomDAConnection;
var
  i: integer;
  Component: TComponent;
  Table: TCustomDADataSet;
  QOInfo: TQuickOpenInfo;
begin
  Assert(PropCount > 0, 'PropCount = 0');
  Component := GetComponent(0) as TComponent;
  Assert(Component is TCustomDADataSet, Component.ClassName);

  Result := TDBAccessUtils.UsedConnection(TCustomDADataSet(Component));
  if Result = nil then
    Exit;

  Table := TCustomDADataSet(GetComponent(0));
  TDBAccessUtils.QuickOpen(Table, QOInfo);
  try
    for i := 0 to Table.FieldDefs.Count - 1 do
      Proc(Table.FieldDefs[i].Name);

  finally
    TDBAccessUtils.Restore(Table, QOInfo);
  end;
end;

{ TDAFieldsListEditor }

function TDAFieldsListEditor.GetAttributes: TPropertyAttributes;
begin
  Result := [paValueList];
end;

function TDAFieldsListEditor.AutoFill: boolean;
begin
  Result := False;
end;

function TDAFieldsListEditor.InternalGetValues(Proc: TGetXStrProc): TCustomDAConnection;
var
  i: integer;
  Component: TComponent;
  Table: TCustomDADataSet;
  QOInfo: TQuickOpenInfo;
begin
  Assert(PropCount > 0, 'PropCount = 0');
  Component := GetComponent(0) as TComponent;
  Assert(Component is TCustomDADataSet, Component.ClassName);

  Result := TDBAccessUtils.UsedConnection(TCustomDADataSet(Component));
  if Result = nil then
    Exit;

  Table := TCustomDADataSet(GetComponent(0));
  TDBAccessUtils.QuickOpen(Table, QOInfo);
  try
    for i := 0 to Table.Fields.Count - 1 do
      Proc(Table.Fields[i].FieldName);

  finally
    TDBAccessUtils.Restore(Table, QOInfo);
  end;
end;

{ TDALoaderTableNameEditor }

function TDALoaderTableNameEditor.GetAttributes: TPropertyAttributes;
begin
  Result := [paValueList];
end;

function TDALoaderTableNameEditor.AutoFill: boolean;
begin
  Result := False;
end;

function TDALoaderTableNameEditor.InternalGetValues(Proc: TGetXStrProc): TCustomDAConnection;
var
  List: _TStrings;
  i: integer;
begin
  List := _TStringList.Create;
  try
    Result := TDALoaderUtils.UsedConnection(TDALoader(GetComponent(0)));
    if Result = nil then
      exit;

    Result.GetTableNames(List);
    for i := 0 to List.Count - 1 do
      Proc(List[i]);
  finally
    List.Free;
  end;
end;

{ TDADataSetMasterFieldsEditor }

function TDADataSetMasterFieldsEditor.GetMasterFields: string;
begin
  Result := (DataSet as TCustomDADataSet).MasterFields;
end;

procedure TDADataSetMasterFieldsEditor.SetMasterFields(const Value: string);
begin
  (DataSet as TCustomDADataSet).MasterFields := Value;
end;

function TDADataSetMasterFieldsEditor.GetIndexFieldNames: string;
begin
  Result := (DataSet as TCustomDADataSet).DetailFields;
end;

procedure TDADataSetMasterFieldsEditor.SetIndexFieldNames(const Value: string);
begin
  (DataSet as TCustomDADataSet).DetailFields := Value;
end;

{ TVariantEditor }

function TVariantEditor.GetAttributes: TPropertyAttributes;
begin
  if VarIsArray(GetVarValue) then
    Result := [paReadOnly]
  else
    Result := inherited GetAttributes;
end;

function TVariantEditor.GetValue: string;
begin
  if VarIsArray(GetVarValue) then
    Result := '<Array>'
  else
    Result := GetVarValue;//inherited GetValue;
end;

procedure TVariantEditor.SetValue(const Value: string);
begin
  SetVarValue(Value);
end;

{ TDADatasetOrSQLProperty }

procedure TDADatasetOrSQLProperty.CheckComponent(const Value: string);
var
  i: integer;
  Component: TComponent;
  AClass: TClass;
  DataSetClass: TCustomDADataSetClass;
  SQLClass: TCustomDASQLClass;
  UpdateSQL: TCustomDAUpdateSQL;
begin
  DataSetClass := nil;
  SQLClass := nil; 
  Component := {$IFDEF FPC}PropertyHook{$ELSE}Designer{$ENDIF}.GetComponent(Value);
  if Component <> nil then begin
    for i := 0 to PropCount - 1 do begin
      UpdateSQL := TCustomDAUpdateSQL(GetComponent(i));
      if UpdateSQL.Dataset = Component then
        Exit;
      if (i = 0) or (DataSetClass <> nil) then begin
        AClass := TDBAccessUtils.GetDataSetClass(UpdateSQL);
        if (i > 0) and (AClass <> DataSetClass) then
          DataSetClass := nil
        else
          DataSetClass := TCustomDADataSetClass(AClass);
      end;
      if (i = 0) or (SQLClass <> nil) then begin
        AClass := TDBAccessUtils.GetSQLClass(UpdateSQL);
        if (i > 0) and (AClass <> SQLClass) then
          SQLClass := nil
        else
          SQLClass := TCustomDASQLClass(AClass);
      end;
    end;
    if not ((Component is SQLClass) or (Component is DataSetClass)) then
      Exit;
  end;
  FCheckProc(Value);
end;

procedure TDADatasetOrSQLProperty.GetValues(Proc: TGetStrProc);
begin
  FCheckProc := Proc;
  inherited GetValues(CheckComponent);
end;

{ TDAUpdateSQLProperty }

procedure TDAUpdateSQLProperty.CheckComponent(const Value: string);
var
  i, j: integer;
  UpdateObject: TComponent;
  UpdateSQL: TCustomDAUpdateSQL;
  DataSetClass: TCustomDADataSetClass;
begin
  UpdateSQL := {$IFDEF FPC}PropertyHook{$ELSE}Designer{$ENDIF}.GetComponent(Value) as TCustomDAUpdateSQL;
  if UpdateSQL = nil then
    Exit;

  DataSetClass := TDBAccessUtils.GetDataSetClass(UpdateSQL);
  for i := 0 to PropCount - 1 do
    if not (GetComponent(i) is DataSetClass) then
      Exit;

  for i := 0 to 4 do begin
    UpdateObject := nil;
    case i of
      0: UpdateObject := UpdateSQL.ModifyObject;
      1: UpdateObject := UpdateSQL.InsertObject;
      2: UpdateObject := UpdateSQL.DeleteObject;
      3: UpdateObject := UpdateSQL.RefreshObject;
      4: UpdateObject := UpdateSQL.LockObject;
    end;

    if UpdateObject <> nil then
      for j := 0 to PropCount - 1 do
        if TCustomDADataSet(GetComponent(j)) = UpdateObject then
          Exit;
  end;

  FCheckProc(Value);
end;

procedure TDAUpdateSQLProperty.GetValues(Proc: TGetStrProc);
begin
  FCheckProc := Proc;
  inherited GetValues(CheckComponent);
end;

{ TDAMetaDataKindEditor }

function TDAMetaDataKindEditor.GetAttributes: TPropertyAttributes;
begin
  Result := [paValueList];
end;

function TDAMetaDataKindEditor.InternalGetValues(Proc: TGetXStrProc): TCustomDAConnection;
var
  List: _TStrings;
  i: integer;
begin
  List := _TStringList.Create;
  try
    Result := TDBAccessUtils.UsedConnection(TDAMetaData(GetComponent(0)));
    if Result = nil then
      exit;

    TDAMetaData(GetComponent(0)).GetMetaDataKinds(List);
    for i := 0 to List.Count - 1 do
      Proc(List[i]);
  finally
    List.Free;
  end;
end;

{ TDAMetaDataRestrictionsEditor }

destructor TDAMetaDataRestrictionsEditor.Destroy;
begin
  FList.Free;

  inherited;
end;

function TDAMetaDataRestrictionsEditor.GetStrings: TStrings;
var
  MetaData: TDAMetaData;
  UsedConnection: TCustomDAConnection;
  i: integer;
  Name, Value: _string;
  List: _TStringList;
begin
  MetaData := TDAMetaData(GetComponent(0));
  UsedConnection := TDBAccessUtils.UsedConnection(TDAMetaData(GetComponent(0)));

  if FList = nil then
    FList := TStringList.Create;

  if (Trim(MetaData.MetaDataKind) = '') or (UsedConnection = nil) then begin
    AssignStrings(MetaData.Restrictions, FList);
    Result := FList;
  end

  else begin
    List := _TStringList.Create;
    try
      MetaData.GetRestrictions(List, MetaData.MetaDataKind);
      AssignStrings(List, FList);
    finally
      List.Free;
    end;
    for i := 0 to FList.Count - 1 do
      FList[i] := FList[i] + '=';

    for i := 0 to MetaData.Restrictions.Count - 1 do begin
      Name := MetaData.Restrictions.Names[i];
      Value := Trim(Copy(MetaData.Restrictions[i], Length(Name) + 2, MaxInt));
      // ValueFromIndex added in D7
      //Value := Trim(Copy(MetaData.Restrictions.ValueFromIndex[i]);
      if Value <> '' then
        FList.Values[Trim(Name)] := Value;
    end;

    Result := FList;
  end;
end;

procedure TDAMetaDataRestrictionsEditor.SetStrings(const Value: TStrings);
var
  MetaData: TDAMetaData;
  i: integer;
begin
  MetaData := TDAMetaData(GetComponent(0));

  if FList = nil then
    FList := TStringList.Create;
  FList.Assign(Value);
  for i := FList.Count - 1 downto 0 do
    if Trim(Copy(FList[i], Length(FList.Names[i]) + 2, MaxInt)) = '' then
    //if Trim(FList.ValueFromIndex[i]) = '' then
      FList.Delete(i);

  AssignStrings(FList, MetaData.Restrictions);

  {$IFDEF FPC}FindRootDesigner(GetComponent(0) as TComponent){$ELSE}Designer{$ENDIF}.Modified;
end;

{$IFNDEF FPC}
{ TDAConnectionList }

constructor TDAConnectionList.Create;
begin
  inherited;

  Items := TStringList.Create;
end;

destructor TDAConnectionList.Destroy;
begin
  Items.Free;

  inherited;
end;

procedure TDAConnectionList.StrProc(const S: string);
begin
{$IFNDEF VER6P}
  Items.Add(S);
{$ENDIF}
end;

procedure TDAConnectionList.ListBoxDblClick(Sender: TObject);
begin
  Form.ModalResult := mrOk;
end;

procedure TDAConnectionList.ListBoxKeyPress(Sender: TObject; var Key: Char);
begin
  case Key of
    #13:
      Form.ModalResult := mrOk;
    #27:
      Form.ModalResult := mrCancel;
  end;
end;

{$IFDEF CLR} /// DAC 11241
procedure TDAConnectionList.FormShow(Sender: TObject);
begin
  Form.Left := FormLeft - 20;
  Form.Top := FormTop - 20;
end;
{$ENDIF}

function TDAConnectionList.GetConnection(Component: TComponent; Designer: {$IFDEF VER6P}IDesigner{$ELSE}IFormDesigner{$ENDIF}): TCustomDAConnection;
const
  Width = 124;
  Height = 180;
var
  ListBox: TListBox;
  TypeData: TTypeData;
{$IFDEF VER6P}
  DesignOffset: TPoint;
{$ENDIF}
begin
{$IFDEF CLR}
  TypeData := TTypeData.Create(TypeOf(GetConnectionType));
  Designer.GetComponentNames(TypeData, StrProc);
{$ELSE}
  TypeData.ClassType := GetConnectionType;
  Designer.GetComponentNames(@TypeData, StrProc);
{$ENDIF}

  if Items.Count = 0 then
    Result := nil
  else
    if Items.Count = 1 then
      Result := TCustomDAConnection(Designer.GetComponent(Items[0]))
    else begin
      Form := TForm.Create(nil);
      ListBox := TListBox.Create(Form);
    {$IFDEF MSWINDOWS}
      Form.BorderStyle := bsSizeToolWin;
    {$ENDIF}
    {$IFDEF UNIX}
      Form.BorderStyle := fbsSizeToolWin;
    {$ENDIF}
    {$IFDEF VER6P}
      if Designer.Root is TForm then begin
      {$IFDEF CLR}
        DesignOffset := (Designer.Root as TForm).ClientToScreen(TPoint.Create(Word(Designer.Root.DesignInfo), Word(Designer.Root.DesignInfo shr 16)));
        FormLeft := DesignOffset.X + Word(Component.DesignInfo shr 16) - Width div 3;
        FormTop := DesignOffset.Y + Word(Component.DesignInfo) - 5;
      {$ELSE}
        DesignOffset := TForm(Designer.Root).BoundsRect.TopLeft;
      {$ENDIF}
      end
      else
      {$IFDEF CLR}
        DesignOffset := TPoint.Create(Word(Designer.Root.DesignInfo), Word(Designer.Root.DesignInfo shr 16));
      {$ELSE}
        DesignOffset := Point(LongRec(Designer.Root.DesignInfo).Lo, LongRec(Designer.Root.DesignInfo).Hi);
      Form.Left := DesignOffset.X + Word(Component.DesignInfo) - Width div 3;
      Form.Top := DesignOffset.Y + Word(Component.DesignInfo shr 16) - 5;      
      {$ENDIF}
    {$ELSE}
      Form.Left := Designer.Form.Left + LongRec(Component.DesignInfo).Lo - Width div 3;
      Form.Top := Designer.Form.Top + LongRec(Component.DesignInfo).Hi - 5;
    {$ENDIF}
      Form.Width := Width;
      Form.Height := Height;
      Form.Caption := 'Connection List';
      Form.InsertControl(TControl(ListBox));//Form.InsertControl(QControls.TControl(ListBox));
      ListBox.Items.Assign(Items);
      ListBox.Align := alClient;
      ListBox.ItemIndex := 0;
      ListBox.OnDblClick := ListBoxDblClick;
      ListBox.OnKeyPress := ListBoxKeyPress;
    {$IFDEF CLR}
      Form.OnShow := FormShow;
    {$ENDIF}

      if Form.ShowModal = mrOk then
        Result := TCustomDAConnection(Designer.GetComponent(Items[ListBox.ItemIndex]))
      else
        Result := nil;
      Form.Free;
    end;
end;

{$IFDEF VER6P}

{ TDADesignNotification }

procedure TDADesignNotification.DesignerClosed(const ADesigner: IDesigner
  {$IFNDEF K1}; AGoingDormant: Boolean{$ENDIF});
begin

end;

procedure TDADesignNotification.DesignerOpened(const ADesigner: IDesigner
  {$IFNDEF K1}; AResurrecting: Boolean{$ENDIF});
begin

end;

procedure TDADesignNotification.ItemDeleted(const ADesigner: IDesigner;
  AItem: TPersistent);
begin

end;

procedure TDADesignNotification.StrProc(const S: string);
begin
  FConnectionList.Items.Add(S);
end;

procedure TDADesignNotification.DSStrProc(const S: string);
begin
  DSItems.Add(S);
end;

procedure TDADesignNotification.ItemsModified(const ADesigner: IDesigner);
var
  Component: TComponent;
  TypeData: TTypeData;
  i, Width, Height: integer;
  DS: TDataSet;
  CRDesignUtilsClass: TCRDesignUtilsClass;
  CREditorClass: TCREditorClass;
  Modified: boolean;
begin
  if (FItem <> nil) and (FItem is TCRDataSource) then begin
    try
      Component := TComponent(FItem);
      with TCRDataSource(Component) do
        if TDBAccessUtils.GetDesignCreate(TCRDataSource(Component)) then begin
          DSItems := TStringList.Create;
          try
          {$IFDEF CLR}
            TypeData := TTypeData.Create(TypeOf(TDataSet));
            ADesigner.GetComponentNames(TypeData, DSStrProc);
          {$ELSE}
            TypeData.ClassType := TDataSet;
            ADesigner.GetComponentNames(@TypeData, DSStrProc);
          {$ENDIF}
            for i := 0 to DSItems.Count - 1 do begin
              DS := TDataSet(ADesigner.GetComponent(DSItems[i]));
              Width := Word(DesignInfo) - Word(DS.DesignInfo);
              Height := Word(DesignInfo shr 16) - Word(DS.DesignInfo shr 16);
              if (Width >= -32) and (Width <= 32) and
                 (Height >= -32) and (Height <= 32)
              then begin
                DataSet := DS;
                break;
              end;
            end;
            TDBAccessUtils.SetDesignCreate(TCRDataSource(Component), False);
          finally
            DSItems.Free;
          end
        end;
    finally
      FItem := nil;
      ADesigner.Modified;
    end;
  end

  else begin
    if FConnectionList <> nil then
      exit;
    if not NotificationActive then
      FItem := nil
    else
      if FItem <> nil then begin
        Modified := False;
        CRDesignUtilsClass := nil;
        try
          if FindComponentEditor(FItem, CREditorClass, CRDesignUtilsClass) then
            Modified := True;

          Modified := Modified and (TDADesignUtilsClass(CRDesignUtilsClass).GetConnection(TComponent(FItem)) = nil);
          if Modified then
            try
              FConnectionList := CreateConnectionList;
              ADesigner.GetComponentNames(GetTypeData(FConnectionList.GetConnectionType.ClassInfo), StrProc);
              SetObjectProp(FItem, GetConnectionPropertyName, FConnectionList.GetConnection(TComponent(FItem), ADesigner));
            finally
              FreeAndNil(FConnectionList);
            end;
        finally
          FItem := nil;
          if Modified then
            ADesigner.Modified;
        end;
      end; 
  end;
end;

procedure TDADesignNotification.SelectionChanged(
  const ADesigner: IDesigner; const ASelection: IDesignerSelections);
begin

end;
{$ENDIF}
{$ENDIF}

{ TDAStrEditDlg }

{$IFDEF USE_CODE_EDITOR}
function TStrEditDlgEx.ShowModal: Integer;
begin
  Result := mrYes;
end;

function TStrEditDlgEx.GetLines: TStrings;
begin
  Result := FLines;
end;

procedure TStrEditDlgEx.SetLines(const Value: TStrings);
begin
  FLines := Value;
end;

function TStrEditDlgEx.GetLinesControl: TWinControl;
begin
  Result := OKButton;
end;

{ TDAPropertyEditor }

function TStringListPropertyEx.EditDialog: TStrEditDlg;
begin
  Result := TStrEditDlgEx.Create(Application);
end;
{$ENDIF}

{ TDAComponentEditor }

constructor TDAComponentEditor.Create(AComponent: TComponent;
    ADesigner: {$IFDEF FPC}TComponentEditorDesigner{$ELSE}{$IFDEF VER6P}IDesigner{$ELSE}IFormDesigner{$ENDIF}{$ENDIF});
{$IFNDEF VER6P}
var
  Connection: TCustomDAConnection;
{$ENDIF}
begin
{$IFDEF DBTOOLS}
  FDBToolsVerbIndex := -1;
{$ENDIF}

  inherited;

{$IFNDEF VER6P}
  if (DADesignUtilsClass <> nil)
    and DADesignUtilsClass.HasConnection(Component)
    and DADesignUtilsClass.GetDesignCreate(Component) then begin

    with (DADesignUtilsClass.GetConnectionList as TDAConnectionList) do begin
      Connection := GetConnection(Component, Designer);
      DADesignUtilsClass.SetConnection(Component, Connection);
      Free;
    end;
    DADesignUtilsClass.SetDesignCreate(Component, False);
  end;
{$ENDIF}
end;

function TDAComponentEditor.GetVerbCount: integer;
{$IFDEF DBTOOLS}
var
  i: integer;
{$ENDIF}
begin
{$IFDEF DBTOOLS}
  if (FDBToolsVerbIndex >=0) and not FCRDesignUtilsClass.DBToolsAvailable then begin
    DADesignUtilsClass.SetDBToolsDownloadParams(True, DADesignUtilsClass.NeedToCheckDbTools = ncIncompatible);
    if NoCheckForTools(DADesignUtilsClass.NeedToCheckDbTools = ncIncompatible) then begin
      for i := FDBToolsVerbIndex to Length(FVerbs) - 2 do
        FVerbs[i] := FVerbs[i + 1];
      SetLength(FVerbs, Length(FVerbs) - 1);
      FDBToolsVerbIndex := -1;
    end;
  end;
{$ENDIF}
  Result := inherited GetVerbCount;
end;

function TDAComponentEditor.GetDADesignUtilsClass: TDADesignUtilsClass;
begin
  Result := TDADesignUtilsClass(FCRDesignUtilsClass);
end;

class procedure TDAComponentEditor.ProcessEditorResult(ModalResult: integer;
  CREditor: TCREditorForm; Component: TComponent;
  Designer: {$IFDEF FPC}TIDesigner{$ELSE}{$IFDEF VER6P}IDesigner{$ELSE}IFormDesigner{$ENDIF}{$ENDIF}
);
{$IFDEF USE_CODE_EDITOR}
var
  PropEditor: TStringListPropertyEx;
  StType: TStatementType;
{$ENDIF}
begin
  inherited;

{$IFDEF USE_CODE_EDITOR}
  if ModalResult = mrYesToAll then begin
    PropEditor := TStringListPropertyEx.Create(Designer, 1);
    if (CREditor is TCRTabEditorForm) and (TCRTabEditorForm(CREditor).ActiveFrame is TDAUpdateSQLFrame) then
      StType := TDAUpdateSQLFrame(TCRTabEditorForm(CREditor).ActiveFrame).StatementType
    else
      StType := stQuery;
    PropEditor.SetPropEntry(0, Component, GetPropInfo(Component, TDADesignUtils.GetSQLPropName(CREditor.Component, StType)));
    PropEditor.Edit;
  end;
{$ENDIF}
end;

{$IFDEF DBTOOLS}
procedure TDAComponentEditor.AddDBToolsVerbs(Verbs: TDBToolsVerbs);begin
  if not DADesignUtilsClass.DBToolsAvailable then begin
    if DADesignUtilsClass.NeedToCheckDbTools = ncExpired then
      Exit;
    DADesignUtilsClass.SetDBToolsDownloadParams(True, DADesignUtilsClass.NeedToCheckDbTools = ncIncompatible);
    if NoCheckForTools(DADesignUtilsClass.NeedToCheckDbTools = ncIncompatible) then
      Exit;
  end;  FDBToolsVerbs := Verbs;
  FDBToolsVerbIndex := AddVerb(DADesignUtilsClass.GetDBToolsMenuCaption, DBToolsMenuExecute);end;

procedure TDAComponentEditor.DBToolsMenuExecute;
begin
  DBTools.PrepareMenu(Designer, Component, DADesignUtilsClass);
end;

procedure TDAComponentEditor.PrepareItem(Index: integer; const AItem: IMenuItem);
var
  VerbIdx: TDBToolsVerb;
begin
  if (Index = FDBToolsVerbIndex) and (FDBToolsVerbs <> []) then
    for VerbIdx := Low(TDBToolsVerb) to High(TDBToolsVerb) do
      if VerbIdx in FDBToolsVerbs then
        AItem.AddItem(DBTools.MenuActions[VerbIdx]);
end;
{$ENDIF}

procedure TDAComponentEditor.ShowDataEditor;
begin
  ShowEditorEx(TDADataEditorForm, FCRDesignUtilsClass, Component, Designer);
end;

{ TDALoaderEditor }

procedure TDALoaderEditor.InitVerbs;
begin
  inherited;
{$IFNDEF CLR}
{$IFNDEF BCB}
  AddVerb('Columns E&ditor...', ShowColEditor);
{$ENDIF}
{$ENDIF}
  AddVerb('Create Columns', CreateColumns);
end;

procedure TDALoaderEditor.ShowColEditor;
{$IFDEF FPC}
var
  ce: TCollectionPropertyEditor;
  Hook: TPropertyEditorHook;
{$ENDIF}
begin
{$IFNDEF CLR}
{$IFNDEF BCB}
{$IFNDEF FPC}
  Assert(Component is TDALoader);
  with ShowCollectionEditorClass(Designer, TCollectionEditor, Component,
    TDALoader(Component).Columns, 'Columns', [coAdd,coDelete{,coMove}]) do
    UpdateListbox;
{$ELSE}
  if GetHook(Hook) then begin
    ce := TCollectionPropertyEditor.Create(Hook, 1);
    try
      ce.SetPropEntry(0, Component, GetPropInfo(Component, 'Columns'));
      ce.Initialize;
      ce.Edit;
    finally
      ce.Free;
    end;
  end;
{$ENDIF}
{$ENDIF}
{$ENDIF}
end;

procedure TDALoaderEditor.CreateColumns;
begin
  Assert(Component is TDALoader);
  if (TDALoader(Component).Columns.Count = 0) or
    (MessageDlg('Do you want recreate columns for table ' +
       TDALoader(Component).TableName + '?', mtConfirmation, [mbYes,mbNo], 0) = mrYes)
  then begin
    TDALoader(Component).CreateColumns;
  {$IFDEF FPC}
    Modified;
  {$ENDIF}
    ShowColEditor;
  end;
end;

{$IFDEF MSWINDOWS}

{ TDASQLMonitorEditor }

procedure TDASQLMonitorEditor.RunDBMonitor;
begin
  Assert(HasMonitor);
  ShellExecute(0, 'open', PChar(WhereMonitor), '', '', SW_SHOW)
end;

procedure TDASQLMonitorEditor.RunSQLMonitor;
begin
  ShellExecute(0, 'open', PChar('sqlmon.exe'), '', '', SW_SHOW);
end;

procedure TDASQLMonitorEditor.InitVerbs;
begin
  if HasMonitor then
    AddVerb('Run DBMonitor...', RunDBMonitor);
  AddVerb('Run SQL Monitor...', RunSQLMonitor);
end;

procedure TDASQLMonitorEditor.Edit;
begin
  if GetVerbCount > 0 then
    ExecuteVerb(0);
end;

{$ENDIF}

{$IFNDEF FPC}
{ TCRDataSourceEditor }
constructor TCRDataSourceEditor.Create(Component: TComponent; aDesigner: {$IFDEF VER6P}IDesigner{$ELSE}IFormDesigner{$ENDIF});
var
  TypeData: TTypeData;
  i, Width, Height: integer;
  DS: TDataSet;
begin
  inherited;

  with TCRDataSource(Component) do
    if TDBAccessUtils.GetDesignCreate(TCRDataSource(Component)) then begin
      Items := TStringList.Create;
      try
      {$IFDEF CLR}
        TypeData := TTypeData.Create(TypeOf(TDataSet));
        aDesigner.GetComponentNames(TypeData, StrProc);
      {$ELSE}
        TypeData.ClassType := TDataSet;
        aDesigner.GetComponentNames(@TypeData, StrProc);
      {$ENDIF}

        for i := 0 to Items.Count - 1 do begin
          DS := TDataSet(aDesigner.GetComponent(Items[i]));
          Width := Word(DesignInfo) - Word(DS.DesignInfo);
          Height := Word(DesignInfo shr 16) - Word(DS.DesignInfo shr 16);
          if (Width >= 0) and (Width <= 28 + 4) and
             (Height >= 0) and (Height <= 28 + 4)
          then
            DataSet := DS;
        end;
        TDBAccessUtils.SetDesignCreate(TCRDataSource(Component), False);
      finally
        Items.Free;
      end
    end;
end;

procedure TCRDataSourceEditor.StrProc(const S: string);
begin
  Items.Add(S);
end;

procedure TCRDataSourceEditor.ConvertToDataSource;
begin
  if Designer <> nil then
    ConvertToClass(Self.Designer, Component, TDataSource);
end;

procedure TCRDataSourceEditor.InitVerbs;
begin
  inherited;
  AddVerb('Convert to TDataSource', ConvertToDataSource);
end;

procedure TCRDataSourceEditor.CheckEdit({$IFDEF VER6P}const Prop: IProperty{$ELSE}Prop: TPropertyEditor{$ENDIF});
begin
  if FFirstProp = nil then
    FFirstProp := Prop
{$IFNDEF VER6P}
  else
    Prop.Free;
{$ENDIF}
end;

procedure TCRDataSourceEditor.Edit;
var
  Components: {$IFDEF VER6P}IDesignerSelections;{$ELSE}TDesignerSelectionList;{$ENDIF}
begin
  Components := {$IFDEF VER6P}TDesignerSelections.Create{$ELSE}TDesignerSelectionList.Create{$ENDIF};
{$IFNDEF VER6P}
  try
{$ENDIF}
    Components.Add(Component);
    FFirstProp := nil;
    GetComponentProperties(Components, tkMethods, Designer, CheckEdit);
    if FFirstProp <> nil then
    {$IFNDEF VER6P}
      try
    {$ENDIF}
        FFirstProp.Edit;
    {$IFNDEF VER6P}
      finally
        FFirstProp.Free;
      end;
    {$ENDIF}
{$IFNDEF VER6P}
  finally
   Components.Free;
  end;
{$ENDIF}
end;
{$ENDIF}

{ TDesignMacros }

const
  SComment           = '--';
  SBeginMacroComment = 'MACRO';
  SEndMacroComment   = 'ENDMACRO';

{function TDesignMacros.GetMacroValue(Macro: TMacro): string;
var
  i: integer;
  ResultList: TStringList;
begin
  ResultList := TStringList.Create;
  try
    ResultList.Text := Macro.Value;
    if not Macro.Active then
      for i := 0 to ResultList.Count - 1 do
        ResultList[i] := SComment + ' ' + ResultList[i];

    ResultList.Insert(0, '');
    ResultList.Insert(1, SComment + ' ' + SBeginMacroComment + ' ' + Macro.Name);
    ResultList.Add(SComment + ' ' + SEndMacroComment);
  finally
    Result := ResultList.Text;
    ResultList.Free;
  end;
end;}

procedure TDesignMacros.Scan(var SQL: _string);
{var

  i, j: integer;
  s, St, CommentSt: string;
  SourceSQL: TStringList;
  MacroSQL: TStringList;
  NewMacro,
  MacroFound: boolean;
  Macro: TMacro;

  Parser: TParser;
  CodeLexem: integer;

  function TrimLineSeparator(s: string): string;
  begin
    if Copy(s, Length(s) - Length(SLLineSeparator) + 1, Length(SLLineSeparator)) = SLLineSeparator then
      Result := Copy(s, 1, Length(s) - Length(SLLineSeparator))
    else
      Result := s;
  end;

  function AtFirstPos(Substr: string; s: string): boolean;
  begin
    Result := Copy(Trim(s), 1, Length(Substr)) = Substr;
  end;

  function TrimFirst(Substr: string; s: string): string;
  begin
    s := Trim(s);
    Result := Copy(s, Length(Substr) + 1, Length(s) - Length(Substr))
  end;
}
begin
  Clear;
//  TODO:
{  MacroFound := False;
  SourceSQL := TStringList.Create;
  MacroSQL := TStringList.Create;
  Parser := FParserClass.Create('');
  Macro := nil;

  try
    Parser.OmitBlank := False;
    Parser.Uppered := False;
    SourceSQL.Text := SQL;
    SQL := '';

    for i := 0 to SourceSQL.Count - 1 do begin
      s := SourceSQL[i];

      CommentSt := '';

      if AtFirstPos(SComment, s) then begin

        Parser.SetText(Trim(s));
        Parser.ToBegin;
        if Parser.GetNext(St) = lcComment then begin
          Parser.SetText(TrimFirst(SComment, s));
          Parser.ToBegin;
          repeat
            CodeLexem := Parser.GetNext(St)
          until CodeLexem <> lcBlank;
          CommentSt := St;
        end;
      end;

      if Macro <> nil then
        if CommentSt = SEndMacroComment then begin
          if not Macro.Active then
            for j := 0 to MacroSQL.Count - 1 do begin
              St := TrimFirst(SComment, MacroSQL[j]);
              if St[1] = ' ' then
                St := Copy(St, 2, Length(St) - 1);
              MacroSQL[j] := St;
            end;

          if MacroSQL.Count = 0 then
            Macro.Active := True;             
          Macro.Value := TrimLineSeparator(MacroSQL.Text);
          MacroSQL.Clear;
          Macro := nil;
        end
        else begin
          MacroSQL.Add(s);
          if CommentSt = '' then
            Macro.Active := True;
        end

      else begin
        NewMacro := False;

        if CommentSt = SBeginMacroComment then begin

          if Parser.GetNext(St) = lcBlank then begin
            repeat
              CodeLexem := Parser.GetNext(St)
            until CodeLexem <> lcBlank;

            NewMacro := (CodeLexem = lcIdent) or
              Parser.IsNumericMacroNameAllowed and (CodeLexem = lcNumber) or
              (CodeLexem > Parser.SymbolLexems.Count) and
              (CodeLexem <= Parser.SymbolLexems.Count + Parser.KeywordLexems.Count);
            if NewMacro and (CodeLexem = lcNumber) then begin
              CodeLexem := Parser.GetNext(s);
              if (CodeLexem = lcIdent) or (CodeLexem > Parser.SymbolLexems.Count)
                and (CodeLexem <= Parser.SymbolLexems.Count + Parser.KeywordLexems.Count)
              then
                St := St + s
            end;
          end;
        end;

        if NewMacro then begin
          MacroFound := True;
          Macro := FindMacro(St);
          if Macro = nil then begin
            Macro := TMacro(Add);
            Macro.Name := St;
          end;
          Macro.Active := False;
          SQL := TrimLineSeparator(SQL);
          if (SQL <> '') and (Pos(SQL[Length(SQL)], #$9#$A#$D#$20) < 1) then
            SQL := SQL + ' ';
          SQL := SQL + MacroChar + Macro.Name;
        end
        else begin
          if MacroFound then begin
            SQL := TrimLineSeparator(SQL);
          end;
          if i < SourceSQL.Count - 1 then
            s := s + SLLineSeparator;
          SQL := SQL + s;
          MacroFound := False;
        end;
      end;
    end;

  finally
    SourceSQL.Free;
    MacroSQL.Free;
    Parser.Free;
  end;  }
end;

procedure Register;
begin
  // Register property editors
  RegisterPropertyEditor(TypeInfo(TFields), TCustomDADataSet, 'Fields', TDAFieldsEditor);
  RegisterPropertyEditor(TypeInfo(TDAParams), TCustomDASQL, 'Params', TDAPropertyEditor);
  RegisterPropertyEditor(TypeInfo(TDAParams), TCustomDADataset, 'Params', TDAPropertyEditor);
  RegisterPropertyEditor(TypeInfo(TMacros), TCustomDASQL, 'Macros', TDAPropertyEditor);
  RegisterPropertyEditor(TypeInfo(TMacros), TDAScript, 'Macros', TDAPropertyEditor);
  RegisterPropertyEditor(TypeInfo(TMacros), TCustomDADataset, 'Macros', TDAPropertyEditor);
  RegisterPropertyEditor(TypeInfo(_string), TCustomDADataset, 'TableName', TDATableNameEditor);
  RegisterPropertyEditor(TypeInfo(_string), TCustomDADataset, 'StoredProcName', TDASPNameEditor);
  RegisterPropertyEditor(TypeInfo(_string), TCustomDADataset, 'OrderFields', TDAFieldDefsListEditor);
  RegisterPropertyEditor(TypeInfo(_string), TCustomDADataset, 'IndexFieldNames', TDAFieldsListEditor);
  RegisterPropertyEditor(TypeInfo(_string), TCustomDADataSet, 'MasterFields', TDADataSetMasterFieldsEditor);
  RegisterPropertyEditor(TypeInfo(_string), TCustomDADataSet, 'DetailFields', TDADataSetMasterFieldsEditor);
  RegisterPropertyEditor(TypeInfo(Variant), TDAParam, 'Value', TVariantEditor);
  RegisterPropertyEditor(TypeInfo(_string), TCustomDAConnection, 'Database', TDADatabaseNameEditor);
  RegisterPropertyEditor(TypeInfo(_string), TCustomDADataSet, 'UpdatingTable', TDAUpdatingTableEditor);

  RegisterPropertyEditor(TypeInfo(_string), TCustomDAConnection, 'Password', TDAPasswordProperty);

  RegisterPropertyEditor(TypeInfo(TComponent), TCustomDAUpdateSQL, 'RefreshObject', TDADatasetOrSQLProperty);
  RegisterPropertyEditor(TypeInfo(TComponent), TCustomDAUpdateSQL, 'ModifyObject', TDADatasetOrSQLProperty);
  RegisterPropertyEditor(TypeInfo(TComponent), TCustomDAUpdateSQL, 'InsertObject', TDADatasetOrSQLProperty);
  RegisterPropertyEditor(TypeInfo(TComponent), TCustomDAUpdateSQL, 'DeleteObject', TDADatasetOrSQLProperty);
  RegisterPropertyEditor(TypeInfo(TComponent), TCustomDAUpdateSQL, 'LockObject', TDADatasetOrSQLProperty);
  RegisterPropertyEditor(TypeInfo(TCustomDAUpdateSQL), TCustomDADataSet, 'UpdateObject', TDAUpdateSQLProperty);
  RegisterPropertyEditor(TypeInfo(_string), TDALoader, 'TableName', TDALoaderTableNameEditor);
  RegisterPropertyEditor(TypeInfo(Boolean), TDALoader, 'Debug', nil);

  RegisterPropertyEditor(TypeInfo(_string), TDAMetaData, 'MetaDataKind', TDAMetaDataKindEditor);
  RegisterPropertyEditor(TypeInfo(_TStrings), TDAMetaData, 'Restrictions', TDAMetaDataRestrictionsEditor);

  // Register component editors
  RegisterComponentEditor(TDALoader, TDALoaderEditor);
{$IFDEF MSWINDOWS}
  RegisterComponentEditor(TCustomDASQLMonitor, TDASQLMonitorEditor);
{$ENDIF}
end;

initialization

  NotificationActive := True;

end.