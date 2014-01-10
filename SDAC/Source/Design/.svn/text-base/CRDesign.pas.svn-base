
//////////////////////////////////////////////////
//  DB Access Components
//  Copyright © 1998-2012 Devart. All right reserved.
//  CRDesign
//////////////////////////////////////////////////

{$IFNDEF CLR}

{$I Dac.inc}

unit CRDesign;
{$ENDIF}
interface

uses
{$IFDEF MSWINDOWS}
  Windows, Messages, Registry,
{$ENDIF}
  Graphics, Controls, Forms, Dialogs, StdCtrls,
{$IFDEF CLR}
  Borland.Vcl.Design.DesignEditors, Borland.Vcl.Design.DesignIntf,
  Borland.Vcl.Design.FldLinks,
  System.Runtime.InteropServices,
{$ELSE}
{$IFDEF FPC}
  PropEdits, ComponentEditors, Fieldseditor,
{$ELSE}
  {$IFDEF VER6P}DesignIntf, DesignEditors,{$ELSE}DsgnIntf,{$ENDIF}
  {$IFNDEF BCB}{$IFDEF VER5P}FldLinks, {$ENDIF}ColnEdit, {$ELSE}CRFldLinks,{$ENDIF}
{$ENDIF}
{$ENDIF}
  SysUtils, Classes, TypInfo, DB,
  CRTypes, CREditor, CRDesignUtils;

{ ------------  DAC property editors ----------- }
type
  TDAFieldsEditor = class (TPropertyEditor)
  public
    function GetAttributes: TPropertyAttributes; override;
    function GetValue: string; override;
    procedure Edit; override;
  end;

{ ------------  DAC component editors ----------- }
type
  TVerbMethod = procedure of object;
  TVerb = record
    Caption: string;
    Method: TVerbMethod;
  end;
  TVerbs = array of TVerb;

  TCRComponentEditorClass = class of TCRComponentEditor;
  TCRComponentEditor = class (TComponentEditor)
  protected
    FCREditorClass: TCREditorClass;
    FCRDesignUtilsClass: TCRDesignUtilsClass;
    FVerbs: TVerbs;

{$IFDEF MSWINDOWS}
{$IFNDEF FPC}
{$IFNDEF VER8}
    procedure ExecuteDsmAction(const ProcName: string);
    procedure DsmCreateDefaultControl;
    procedure DsmShowInDataSetManager;
    procedure Separator;
{$ENDIF}
{$ENDIF}
{$ENDIF}
    function AddVerb(const Caption: string; Method: TVerbMethod): integer; overload;
    function AddVerb(const Caption: string; CREditorClass: TCREditorClass; CRDesignUtilsClass: TCRDesignUtilsClass): integer; overload;
    procedure InitVerbs; virtual;

    procedure ShowEditor; overload;
    procedure ShowEditor(const InitialProperty: string); overload;
    class procedure ProcessEditorResult(ModalResult: integer;
      CREditor: TCREditorForm; Component: TComponent;
      Designer: {$IFDEF FPC}TIDesigner{$ELSE}{$IFDEF VER6P}IDesigner{$ELSE}IFormDesigner{$ENDIF}{$ENDIF}
    ); virtual;
    procedure ShowFieldsEditor;
    procedure ShowDataEditor; virtual;
  public
    constructor Create(AComponent: TComponent;
      ADesigner: {$IFDEF FPC}TComponentEditorDesigner{$ELSE}{$IFDEF VER6P}IDesigner{$ELSE}IFormDesigner{$ENDIF}{$ENDIF}); override;

    function GetVerbCount: integer; override;
    function GetVerb(Index: integer): string; override;
    procedure ExecuteVerb(Index: integer); override;
    procedure Edit; override;

    class procedure ShowEditorEx(
      CREditorClass: TCREditorClass;
      CRDesignUtilsClass: TCRDesignUtilsClass;
      Component: TComponent;
      Designer: {$IFDEF FPC}TIDesigner{$ELSE}{$IFDEF VER6P}IDesigner{$ELSE}IFormDesigner{$ENDIF}{$ENDIF};
      InitialProperty: string = ''
    );
  end;

procedure Register;
procedure DARegisterComponentEditor(ComponentClass: TComponentClass; ComponentEditor: TCRComponentEditorClass;
  CREditorClass: TCREditorClass;
  CRDesignUtilsClass: TCRDesignUtilsClass);
function FindComponentEditor(Component: TObject;
  var CREditorClass: TCREditorClass; var CRDesignUtilsClass: TCRDesignUtilsClass
): Boolean;

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
  CRDataEditor;

type
  TDAComponentInfo = record
    ComponentClass: TComponentClass;
    ComponentEditor: TCRComponentEditorClass;
    CREditorClass: TCREditorClass;
    CRDesignUtilsClass: TCRDesignUtilsClass
  end;

var
  ComponentsInfo: array of TDAComponentInfo;

procedure DARegisterComponentEditor(ComponentClass: TComponentClass; ComponentEditor: TCRComponentEditorClass;
  CREditorClass: TCREditorClass;
  CRDesignUtilsClass: TCRDesignUtilsClass);
var
  i: integer;
begin
  RegisterComponentEditor(ComponentClass, ComponentEditor);
  i := Length(ComponentsInfo);
  SetLength(ComponentsInfo, i + 1);
  ComponentsInfo[i].ComponentClass := ComponentClass;
  ComponentsInfo[i].ComponentEditor := ComponentEditor;
  ComponentsInfo[i].CREditorClass := CREditorClass;
  ComponentsInfo[i].CRDesignUtilsClass := CRDesignUtilsClass;
end;

function FindComponentEditor(Component: TObject;
  var CREditorClass: TCREditorClass; var CRDesignUtilsClass: TCRDesignUtilsClass): Boolean;
var
  i: Integer;
begin
  Result := False;
  for i := 0 to Length(ComponentsInfo) - 1 do
    if Component is ComponentsInfo[i].ComponentClass then begin
      CREditorClass := ComponentsInfo[i].CREditorClass;
      CRDesignUtilsClass := ComponentsInfo[i].CRDesignUtilsClass;
      Result := True;
      Break;
    end;
end;

type
{$IFDEF UNIX}
  {$DEFINE OLDDESIGNER}
{$ENDIF}
{$IFDEF BCB}
  {$DEFINE OLDDESIGNER}
{$ENDIF}
{$IFDEF FPC}
  {$DEFINE OLDDESIGNER}
{$ENDIF}

{$IFDEF OLDDESIGNER}
{$IFDEF FPC}
  TDADSDesigner = TFieldsComponentEditor;
{$ELSE}
  TDADSDesigner = class (TDataSetDesigner)
  private
    FFieldsEditor: TForm; // TFieldsEditor;  // WAR For support TDSDesigner
  public
    constructor Create(DataSet: TDataSet);
    destructor Destroy; override;
    property FieldsEditor: TForm read FFieldsEditor;
  end;
{$ENDIF}
{$ELSE}
  TDADSDesigner = TDSDesigner;
{$ENDIF}

{$IFDEF OLDDESIGNER}
var
  DataSetEditorClass: TComponentEditorClass;
  

{ TOraDSDesigner }
{$IFNDEF FPC}
constructor TDADSDesigner.Create(DataSet: TDataSet);
begin
  inherited Create(DataSet);

  FFieldsEditor := nil;
end;

destructor TDADSDesigner.Destroy;
begin
  inherited;
end;
{$ENDIF}
{$ENDIF}

{ TDAFieldsEditor }

function TDAFieldsEditor.GetAttributes: TPropertyAttributes;
begin
  Result := [paDialog, paReadOnly];
end;

function TDAFieldsEditor.GetValue: string;
begin
  Result := '(' + DB.TFields.ClassName + ')';
end;

procedure TDAFieldsEditor.Edit;
var
  NeedCreate: boolean;
  DADSDesigner: TDADSDesigner;
  Component: TComponent;
{$IFDEF OLDDESIGNER}
  DataSetEditor: TComponentEditor;
{$ENDIF}
begin
  Component := TComponent(GetComponent(0));

{$IFDEF FPC}
  NeedCreate := True;
{$ELSE}
  if (Component as TDataSet).Designer = nil then
    NeedCreate := True
  else
    if (Component as TDataSet).Designer is TDADSDesigner then begin
      (Component as TDataSet).Designer.Free;
      NeedCreate := True;
    end
    else
      NeedCreate := False;
{$ENDIF}

  if NeedCreate then begin
  {$IFDEF OLDDESIGNER}
    {$IFDEF FPC}
      DataSetEditor := DataSetEditorClass.Create(Component, FindRootDesigner(Component) as TComponentEditorDesigner) as TComponentEditor;
      DataSetEditor.ExecuteVerb(0);
    {$ELSE}
    DataSetEditor := DataSetEditorClass.Create(Component, Designer) as TComponentEditor;
    try
      DataSetEditor.ExecuteVerb(0);
    finally
      DataSetEditor.Free;
    end;
    {$ENDIF}
  {$ELSE}
    {$IFDEF CLR}Borland.VCL.Design.{$ENDIF}DSDesign.ShowFieldsEditor(Designer, TDataSet(Component), TDSDesigner);
  {$ENDIF}
  end
{$IFNDEF FPC}
  else begin
    DADSDesigner := TDADSDesigner((Component as TDataSet).Designer);
    DADSDesigner.FieldsEditor.Show;
  end;
{$ENDIF}
end;

{ TCRComponentEditor }

constructor TCRComponentEditor.Create(AComponent: TComponent;
    ADesigner: {$IFDEF FPC}TComponentEditorDesigner{$ELSE}{$IFDEF VER6P}IDesigner{$ELSE}IFormDesigner{$ENDIF}{$ENDIF});
var
  CREditorClass: TCREditorClass;
begin
  inherited;

  FindComponentEditor(AComponent, CREditorClass, FCRDesignUtilsClass);
  InitVerbs;
end;

procedure TCRComponentEditor.ShowEditor(const InitialProperty: string);
begin
  ShowEditorEx(FCREditorClass, FCRDesignUtilsClass, Component, Designer, InitialProperty);
end;

procedure TCRComponentEditor.ShowEditor;
begin
  ShowEditorEx(FCREditorClass, FCRDesignUtilsClass, Component, Designer);
end;

class procedure TCRComponentEditor.ShowEditorEx(
  CREditorClass: TCREditorClass;
  CRDesignUtilsClass: TCRDesignUtilsClass;
  Component: TComponent;
  Designer: {$IFDEF FPC}TIDesigner{$ELSE}{$IFDEF VER6P}IDesigner{$ELSE}IFormDesigner{$ENDIF}{$ENDIF};
  InitialProperty: string = ''
);
var
  CREditor: TCREditorForm;
  mr: integer;
begin
  Assert(CREditorClass <> nil);
  CREditor := CREditorClass.Create(nil, CRDesignUtilsClass);
  try
    CREditor.Component := Component;
    TCREditorForm(CREditor).InitialProperty := InitialProperty;

    mr := CREditor.ShowModal;
    ProcessEditorResult(mr, CREditor, Component, Designer);
  finally
    CREditor.Free;
  end;
end;

class procedure TCRComponentEditor.ProcessEditorResult(ModalResult: integer;
  CREditor: TCREditorForm; Component: TComponent;
  Designer: {$IFDEF FPC}TIDesigner{$ELSE}{$IFDEF VER6P}IDesigner{$ELSE}IFormDesigner{$ENDIF}{$ENDIF}
);
begin
  if ModalResult = mrOk then
    if Designer <> nil then
      Designer.Modified;
end;

function TCRComponentEditor.AddVerb(const Caption: string; Method: TVerbMethod): integer;
begin
  Result := Length(FVerbs);
  SetLength(FVerbs, Result + 1);
  FVerbs[Result].Caption := Caption;
  FVerbs[Result].Method := Method;
end;

function TCRComponentEditor.AddVerb(const Caption: string; CREditorClass: TCREditorClass; CRDesignUtilsClass: TCRDesignUtilsClass): integer;
begin
  Assert(FCREditorClass = nil);
  FCREditorClass := CREditorClass;
  Assert(FCRDesignUtilsClass <> nil);
  Result := AddVerb(Caption, ShowEditor);
end;

function TCRComponentEditor.GetVerbCount: integer;
begin
  Result := Length(FVerbs);
end;

function TCRComponentEditor.GetVerb(Index: integer): string;
begin
  Result := FVerbs[Index].Caption;
end;

procedure TCRComponentEditor.ExecuteVerb(Index: integer);
begin
  FVerbs[Index].Method;
end;

procedure TCRComponentEditor.Edit;
begin
  if FCREditorClass <> nil then
    ShowEditor
  else
    if GetVerbCount > 0 then
      ExecuteVerb(0)
    else
      inherited;
end;

procedure TCRComponentEditor.ShowFieldsEditor;
var
  NeedCreate: boolean;
  DADSDesigner: TDADSDesigner;
{$IFDEF OLDDESIGNER}
  DataSetEditor: TComponentEditor;
{$ENDIF}
begin
{$IFDEF FPC}
  NeedCreate := True;
{$ELSE}
  if (Component as TDataSet).Designer = nil then
    NeedCreate := True
  else
    if (Component as TDataSet).Designer is TDADSDesigner then begin
      (Component as TDataSet).Designer.Free;
      NeedCreate := True;
    end
    else
      NeedCreate := False;
{$ENDIF}

  if NeedCreate then begin
  {$IFDEF OLDDESIGNER}
    {$IFDEF FPC}
      DataSetEditor := DataSetEditorClass.Create(Component, FindRootDesigner(Component) as TComponentEditorDesigner) as TComponentEditor;
      DataSetEditor.ExecuteVerb(0);
    {$ELSE}
    DataSetEditor := DataSetEditorClass.Create(Component, Designer) as TComponentEditor;
    try
      DataSetEditor.ExecuteVerb(0);
    finally
      DataSetEditor.Free;
    end;
    {$ENDIF}
  {$ELSE}
    {$IFDEF CLR}Borland.VCL.Design.{$ENDIF}DSDesign.ShowFieldsEditor(Designer, TDataSet(Component), TDSDesigner);
  {$ENDIF}
  end
{$IFNDEF FPC}
  else begin
    DADSDesigner := TDADSDesigner((Component as TDataSet).Designer);
  {$IFDEF UNIX}
    DADSDesigner.FFieldsEditor.Show;
  {$ELSE}
    DADSDesigner.FieldsEditor.Show;
  {$ENDIF}
  end;
{$ENDIF}
end;

procedure TCRComponentEditor.ShowDataEditor;
begin
  ShowEditorEx(TCRDataEditorForm, FCRDesignUtilsClass, Component,  Designer);
end;

{$IFDEF MSWINDOWS}
{$IFNDEF FPC}
{$IFNDEF VER8}
const
{$IFDEF VER5}
  DsmBplName = 'DataSetManager50.bpl';
{$ENDIF}
{$IFDEF VER6}
  DsmBplName = 'DataSetManager60.bpl';
{$ENDIF}
{$IFDEF VER7}
  DsmBplName = 'DataSetManager70.bpl';
{$ENDIF}
{$IFDEF VER9}
  DsmBplName = 'DataSetManager90.bpl';
{$ENDIF}
{$IFDEF VER10}
  DsmBplName = 'DataSetManager100.bpl';
{$ENDIF}
{$IFDEF VER11}
  DsmBplName = 'DataSetManager105.bpl';
{$ENDIF}
{$IFDEF VER12}
  DsmBplName = 'DataSetManager120.bpl';
{$ENDIF}
{$IFDEF VER14}
  DsmBplName = 'DataSetManager140.bpl';
{$ENDIF}
{$IFDEF VER15}
  DsmBplName = 'DataSetManager150.bpl';
{$ENDIF}
{$IFDEF VER16}
  DsmBplName = 'DataSetManager160.bpl';
{$ENDIF}
{$IFDEF CLR}
[DllImport(DsmBplName)]
procedure CreateDefaultControl([MarshalAs(UnmanagedType.LPStr)]Owner, DataSet: string); external;
[DllImport(DsmBplName)]
procedure ShowDataSetManager([MarshalAs(UnmanagedType.LPStr)]Owner, DataSet: string); external;
{$ENDIF}
procedure TCRComponentEditor.ExecuteDsmAction(const ProcName: string);
var
  Handle: Cardinal;
{$IFNDEF CLR}
  Proc: procedure(Owner, DataSet: PChar); stdcall;
{$ENDIF}
  OwnerName: string;
  DataSetName: string;
begin
  Handle := GetModuleHandle(PChar(DsmBplName));
  if Handle <> 0 then begin
{$IFNDEF CLR}
    Proc := GetProcAddress(Handle, PChar(ProcName));
    if Assigned(Proc) and Assigned(Component.Owner) then begin
      OwnerName := (Component as TDataSet).Owner.Name;
      DataSetName := (Component as TDataSet).Name;
      Proc(@OwnerName[1], @DataSetName[1]);
    end;
{$ELSE}
    if Assigned(Component.Owner) then begin
      OwnerName := Component.Owner.Name;
      DataSetName := (Component as TDataSet).Name;
      if SameText(ProcName, 'CreateDefaultControl') then
        CreateDefaultControl(OwnerName, DataSetName)
      else
        if SameText(ProcName, 'ShowDataSetManager') then
          ShowDataSetManager(OwnerName, DataSetName);
    end;
{$ENDIF}
  end;
end;

procedure TCRComponentEditor.DsmCreateDefaultControl;
begin
{$IFDEF CLR}
  DsmShowInDataSetManager;
{$ENDIF}
  ExecuteDsmAction('CreateDefaultControl');
end;

procedure TCRComponentEditor.DsmShowInDataSetManager;
begin
  ExecuteDsmAction('ShowDataSetManager');
end;

procedure TCRComponentEditor.Separator;
begin
end;
{$ENDIF}
{$ENDIF}
{$ENDIF}

procedure TCRComponentEditor.InitVerbs;
var
  Handle: Cardinal;
begin
  inherited;

{$IFDEF MSWINDOWS}
{$IFNDEF FPC}
{$IFNDEF VER8}
  if Component is TDataSet then begin
    Handle := GetModuleHandle(PChar(DsmBplName));
    if Handle <> 0 then begin
      AddVerb('-', Separator);
      AddVerb('Create default control', DsmCreateDefaultControl);
      AddVerb('Show in DataSet Manager', DsmShowInDataSetManager);
    end;
  end;
{$ENDIF}
{$ENDIF}
{$ENDIF}
end;

procedure Register;
{$IFDEF OLDDESIGNER}
var
  DataSet: TDataSet;
  DataSetEditor: TComponentEditor;
{$ENDIF}
begin

{$IFDEF OLDDESIGNER}
{$WARNINGS OFF}
{$IFDEF VER6P}
  {$IFNDEF FPC}
  DataSet := nil;
  try
    DataSet := TDataSet.Create(nil);
    DataSetEditor := Pointer(NativeInt(GetComponentEditor(DataSet, nil)) - 20);
    DataSetEditorClass := TComponentEditorClass(DataSetEditor.ClassType);
  finally
    DataSet.Free;
  end;
{$ELSE}
    DataSet := TDataSet.Create(nil);
    DataSetEditor := TComponentEditor(GetComponentEditor(DataSet, nil));
    DataSetEditorClass := TComponentEditorClass(DataSetEditor.ClassType);
  {$ENDIF}
{$ELSE}
  DataSet := nil;
  DataSetEditor := nil;
  try
    DataSet := TDataSet.Create(nil);
    DataSetEditor := GetComponentEditor(DataSet, nil);
    DataSetEditorClass := TComponentEditorClass(DataSetEditor.ClassType);
  finally
    DataSetEditor.Free;
    DataSet.Free;
  end;
{$ENDIF}
{$WARNINGS ON}
{$ENDIF}
end;

end.
