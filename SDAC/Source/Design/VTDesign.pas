
//////////////////////////////////////////////////
//  Copyright © 1998-2012 Devart. All right reserved.
//  Virtual table design
//  Created:            23.08.99
//////////////////////////////////////////////////

{$IFNDEF CLR}
{$I Dac.inc}

unit VTDesign;
{$ENDIF}
interface
uses
{$IFDEF MSWINDOWS}
  Windows,
{$ENDIF}
  Classes, SysUtils, DB, TypInfo,
  Forms, Controls, Dialogs, Buttons, StdCtrls, Graphics,
{$IFDEF CLR}
  Borland.Vcl.Design.DesignIntf, Borland.Vcl.Design.DesignEditors,
  Borland.Vcl.Design.FldLinks,
{$ELSE}
{$IFDEF FPC}
  PropEdits, ComponentEditors, CRFldLinks,
{$ELSE}
  {$IFDEF VER6P}DesignIntf, DesignEditors, {$ELSE}DsgnIntf, {$ENDIF}
  {$IFNDEF BCB}FldLinks, {$ELSE}CRFldLinks, {$ENDIF}
{$ENDIF}
{$ENDIF}
{$IFDEF FPC}
  LCLType,
{$ENDIF}
  CRTypes, CRDesign, CRDesignUtils;

type
  TVTFieldsEditor = class(TDAFieldsEditor);

{$IFDEF UNIX}
  TVTDataSetMasterFieldsEditor = class (TCRFieldLinkProperty)
{$ELSE}
{$IFDEF BCB}
  TVTDataSetMasterFieldsEditor = class (TCRFieldLinkProperty)
{$ELSE}
{$IFDEF FPC}
  TVTDataSetMasterFieldsEditor = class (TCRFieldLinkProperty)
{$ELSE}
  TVTDataSetMasterFieldsEditor = class (TFieldLinkProperty)
{$ENDIF}
{$ENDIF}
{$ENDIF}
  protected
    function GetMasterFields: string; override;
    procedure SetMasterFields(const Value: string); override;
    function GetIndexFieldNames: string; override;
    procedure SetIndexFieldNames(const Value: string); override;
  end;

  TVirtualTableEditor = class (TCRComponentEditor)
  private
    Items: TStrings;

    procedure StrProc(const S: string);
    procedure DataSetsDblClick(Sender: TObject);

    procedure ShowVTDataEditor;
  {$IFNDEF FPC}
    procedure ShowAssignDataSet;
  {$ENDIF}
    procedure LoadData;
    procedure SaveData;
    procedure ShowAbout;
  protected
    procedure InitVerbs; override;
  public
    procedure Edit; override;
  end;

procedure Register;

implementation
uses
{$IFDEF CLR}
  Borland.VCL.Design.DSDesign,
{$ELSE}
{$IFNDEF BCB}
{$IFDEF MSWINDOWS}
{$IFNDEF FPC}
  DSDesign,
{$ENDIF}
{$ENDIF}
{$ENDIF}
{$ENDIF}
  MemUtils, VirtualTable, VTDataEditor;

//{$R DADesign.res}

{$IFDEF BCB}
  {$DEFINE OLDDESIGNER}
{$ENDIF}

{$I DacVer.inc}

{$IFDEF OLDDESIGNER}
var
  DataSetEditorClass: TComponentEditorClass;
{$ENDIF}

{ TVTDataSetMasterFieldsEditor }

function TVTDataSetMasterFieldsEditor.GetMasterFields: string;
begin
  Result := (DataSet as TVirtualTable).MasterFields;
end;

procedure TVTDataSetMasterFieldsEditor.SetMasterFields(const Value: string);
begin
  (DataSet as TVirtualTable).MasterFields := Value;
end;

function TVTDataSetMasterFieldsEditor.GetIndexFieldNames: string;
begin
  Result := (DataSet as TVirtualTable).DetailFields;
end;

procedure TVTDataSetMasterFieldsEditor.SetIndexFieldNames(const Value: string);
begin
  (DataSet as TVirtualTable).DetailFields := Value;
end;

procedure TVirtualTableEditor.InitVerbs;
begin
  AddVerb('Fields &Editor...', ShowFieldsEditor);
  AddVerb('VirtualTable E&ditor...', ShowVTDataEditor);
{$IFNDEF FPC}
  AddVerb('Assign DataSet...', ShowAssignDataSet);
{$ENDIF}
  AddVerb('Load from file...', LoadData);
  AddVerb('Save to file...', SaveData);
  AddVerb('&About...', ShowAbout);

  inherited;
end;

procedure TVirtualTableEditor.Edit;
begin
  if GetVerbCount > 1 then
    ExecuteVerb(1);
end;

procedure TVirtualTableEditor.ShowVTDataEditor;
begin
  ShowEditorEx(TVTDataEditorForm, TCRDesignUtils, Component, Designer);
end;

procedure TVirtualTableEditor.LoadData;
var
  OpenDialog: TOpenDialog;
begin
  OpenDialog := TOpenDialog.Create(nil);
  try
    OpenDialog.DefaultExt := 'vtd';
    OpenDialog.Filter :=
      'Virtual Table Data (*.vtd)|*.vtd|' +
      'XML File (*.xml)|*.xml|' +
      'Any File (*.*)|*.*';
    OpenDialog.FileName := '*.vtd';
    if OpenDialog.Execute then
      TVirtualTable(Component).LoadFromFile(OpenDialog.FileName);
  finally
    OpenDialog.Free;
  end;
end;

procedure TVirtualTableEditor.SaveData;
var
  SaveDialog: TSaveDialog;
begin
  SaveDialog := TSaveDialog.Create(nil);
  try
    SaveDialog.DefaultExt := 'vtd';
    SaveDialog.Filter :=
      'Virtual Table Data (*.vtd)|*.vtd|' +
      'XML File (*.xml)|*.xml|' +
      'Any File (*.*)|*.*';
    SaveDialog.FileName := 'Data1';
    if SaveDialog.Execute then
      if LowerCase(ExtractFileExt(SaveDialog.FileName)) = '.xml' then
        TVirtualTable(Component).SaveToXML(SaveDialog.FileName)
      else
        TVirtualTable(Component).SaveToFile(SaveDialog.FileName);
  finally
    SaveDialog.Free;
  end;
end;

procedure TVirtualTableEditor.ShowAbout;
begin
  Application.MessageBox(
    'VirtualTable component'#13 +
    'Version ' + DacVersion + ' free'#13 +
    'Copyright © 1998-2012 Devart.'#13 +
    'All right reserved.'#13#13
    +
    'Web: www.devart.com'#13 +
    'eMail: support@devart.com'#13#13
    , 'About',
    MB_OK or MB_ICONINFORMATION);
end;

procedure TVirtualTableEditor.DataSetsDblClick(Sender: TObject);
begin
  TForm(TListBox(Sender).Owner).ModalResult := mrOk;
end;

procedure TVirtualTableEditor.StrProc(const S: string);
begin
  if S <> Component.Name then
    Items.Add(S);
end;

{$IFNDEF FPC}
procedure TVirtualTableEditor.ShowAssignDataSet;
var
  Form: TForm;
  List: TListBox;
  TypeData: TTypeData;
begin
  Form := TForm.Create(nil);
  try
    Form.BorderStyle := bsDialog;
    Form.Height := 250;
    Form.Width := 300;
    Form.Position := poScreenCenter;
    Form.Caption := 'Available DataSets';

    List := TListBox.Create(Form);
    with List do begin
      Parent := Form;
      Left := 8;
      Top := 8;
      Width := Form.ClientWidth - 16;
      Height := Form.ClientHeight - 53;
      Sorted := True;
      OnDblClick := DataSetsDblClick;
    end;
    Items := List.Items;

    with TButton.Create(Form) do begin
      Parent := Form;
      Left := 128;
      Top := 192;
      Caption := 'Ok';
      Font.Style := [fsBold];
      ModalResult := mrOk;
      Default := True;
    end;

    with TButton.Create(Form) do begin
      Parent := Form;
      Left := 208;
      Top := 192;
      Caption := 'Cancel';
      Font.Style := [fsBold];
      ModalResult := mrCancel;
      Cancel := True;
    end;

  {$IFDEF CLR}
    TypeData := TTypeData.Create(TypeOf(TDataSet));
    Designer.GetComponentNames(TypeData, StrProc);
  {$ELSE}
    TypeData.ClassType := TDataSet;
    Designer.GetComponentNames(@TypeData, StrProc);
  {$ENDIF}
    if List.Items.Count > 0 then
      List.ItemIndex := 0;

    if (Form.ShowModal = mrOk) and (List.ItemIndex >= 0) then
      TVirtualTable(Component).Assign(
        TDataSet(Designer.GetComponent(List.Items[List.ItemIndex])));
  finally
    Form.Free;
  end;
end;
{$ENDIF}

procedure Register;
{$IFDEF OLDDESIGNER}
var
  DataSet: TDataSet;
  DataSetEditor: TComponentEditor;
{$ENDIF}
begin
// WAR need before register TOraQuery, TOraSmartQuery

{$IFDEF OLDDESIGNER}
{$WARNINGS OFF}
{$IFDEF VER6P}
  DataSet := nil;
  try
    DataSet := TDataSet.Create(nil);
    DataSetEditor := Pointer(NativeInt(GetComponentEditor(DataSet, nil)) - 20);
    DataSetEditorClass := TComponentEditorClass(DataSetEditor.ClassType);
  finally
    DataSet.Free;
  end;
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

  RegisterPropertyEditor(TypeInfo(DB.TFields), TVirtualTable, 'Fields',
    TVTFieldsEditor);
  RegisterPropertyEditor(TypeInfo(_string), TVirtualTable, 'MasterFields', TVTDataSetMasterFieldsEditor);
  RegisterPropertyEditor(TypeInfo(_string), TVirtualTable, 'DetailFields', TVTDataSetMasterFieldsEditor);

  RegisterComponentEditor(TVirtualTable, TVirtualTableEditor);
end;

end.
