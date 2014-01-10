
//////////////////////////////////////////////////
//  DB Access Components
//  Copyright © 1998-2012 Devart. All right reserved.
//  Params Frame
//////////////////////////////////////////////////

{$IFNDEF CLR}

{$I Dac.inc}

unit DAParamsFrame;
{$ENDIF}

interface

uses
{$IFDEF MSWINDOWS}
  Windows, Messages,
{$ENDIF}
  Graphics, Controls, Forms, Dialogs, StdCtrls, ExtCtrls,
{$IFDEF FPC}
  LResources,
{$ENDIF}
  Classes, SysUtils,
  DBAccess, DB,
  CRFrame, CRTabEditor, CRColFrame;

type
  TDataTypeInfo = record
    Name: string; 
    DataType: TFieldType;
    EmptyIsNull: boolean;
    Editable: boolean;
    Sizeable: boolean;
    DefaultValue: string;
  end;
  TDataTypeInfos = array of TDataTypeInfo;

  TParamTypeInfo = record
    Name: string;
    ParamType: TParamType;
  end;
  TParamTypeInfos = array of TParamTypeInfo;

  TDAParamsFrame = class(TCRColFrame)
    lbPName: TLabel;
    lbParamLog: TLabel;
    lbPType: TLabel;
    cbDataType: TComboBox;
    cbParamType: TComboBox;
    lbParamType: TLabel;
    lbPValue: TLabel;
    edValue: TEdit;
    bEdValue: TButton;
    cbNullValue: TCheckBox;
    lbNullValue: TLabel;
    lbSize: TLabel;
    edSize: TEdit;
    procedure cbDataTypeChange(Sender: TObject);
    procedure cbParamTypeChange(Sender: TObject);
    procedure edValueChange(Sender: TObject);
    procedure cbNullValueClick(Sender: TObject);
    procedure edSizeChange(Sender: TObject);
    procedure bEdValueClick(Sender: TObject);
    
  protected
    FDataTypeInfos: TDataTypeInfos; // Must be filled in successor's constructor
    FParamTypeInfos: TParamTypeInfos; // Must be filled in successor's constructor

    procedure AddDataType(
      Name: string; // if empty then use DB array
      DataType: TFieldType;
      EmptyIsNull: boolean;
      Editable: boolean;
      Sizeable: boolean;
      DefaultValue: string
    );
    procedure AddParamType(
      Name: string;
      ParamType: TParamType
    );

    function GetItems: TCollection; override;
    function GetParams: TDAParams;
    function GetItemName(Item: TCollectionItem): string; override;
    procedure InitItems; override;
    procedure ItemToControls(Item: TCollectionItem); override;
    procedure StoreItemValue(Item: TCollectionItem); virtual;
    procedure ControlsToItem(Item: TCollectionItem); override;
    procedure UpdateControlsState; override;

    property Params: TDAParams read GetParams;
  public
  end;

{$IFDEF FPC}
const
  FieldTypeVarMap: array[TFieldType] of Word = (
    varEmpty, varString, varInteger, varInteger, varInteger,
    varBoolean, varDouble, varCurrency, varCurrency, varDate, varDate, varDate,
    varEmpty, varEmpty, varInteger, varEmpty, varString, varEmpty,
    varEmpty, varEmpty, varEmpty, varEmpty, varEmpty, varString, varOleStr,
    varEmpty, varEmpty, varEmpty, varEmpty, varEmpty, varEmpty, varEmpty,
    varVariant, varUnknown, varDispatch, varString, varEmpty, varEmpty,
    varOleStr, varOleStr);
{$ENDIF}

implementation

{$IFNDEF FPC}
{$IFDEF IDE}
{$R *.dfm}
{$ELSE}
{$R DAParamsFrame.dfm}
{$ENDIF}
{$ELSE}
{$R *.lfm}
{$ENDIF}

uses
{$IFDEF VER6P}
  Variants,
{$ENDIF}
  DASQLComponentEditor, DAParamValueEditor, CREditor, MemData;

function TDAParamsFrame.GetItems: TCollection;
begin
  Result := Editor.DADesignUtilsClass.GetParams(Editor.LocalComponent);
end;

function TDAParamsFrame.GetParams: TDAParams;
begin
  Result := Items as TDAParams;
end;

function TDAParamsFrame.GetItemName(Item: TCollectionItem): string;
begin
  Result := TDAParam(Item).Name;
end;

procedure TDAParamsFrame.InitItems;
var
  i: integer;
begin                                    
  if (cbDataType.Items.Count = 0) and (Length(FDataTypeInfos) <> 0) then begin // First call
    for i := Low(FDataTypeInfos) to High(FDataTypeInfos) do
      cbDataType.Items.Add(FDataTypeInfos[i].Name);
    cbDataType.DropDownCount := cbDataType.Items.Count;
  end;

  if cbParamType.Items.Count = 0 then // First call
    if Length(FParamTypeInfos) <> 0 then begin
      for i := Low(FParamTypeInfos) to High(FParamTypeInfos) do
        cbParamType.Items.Add(FParamTypeInfos[i].Name);
      cbParamType.DropDownCount := cbParamType.Items.Count;
    end
    else
    begin
      cbParamType.Visible := False;
      lbParamType.Visible := False;
      cbDataType.Width := cbParamType.Left + cbParamType.Width - cbDataType.Left; 
    end;

  inherited;
end;

procedure TDAParamsFrame.ItemToControls(Item: TCollectionItem);
var
  Find: boolean;
  i: integer;
begin
  // DataType
  if TDAParam(Item).DataType = ftUnknown then
    cbDataType.ItemIndex := -1
  else begin
    Find := False;
    for i := Low(FDataTypeInfos) to High(FDataTypeInfos) do
      if FDataTypeInfos[i].DataType = TDAParam(Item).DataType then begin
        cbDataType.ItemIndex := i;
        Find := True;
      end;

    if not Find then
      //raise Exception.Create('EdItem: Data type not found');
      cbDataType.ItemIndex := -1;
  end;

  // ItemType
  if IsControlEnabled(cbParamType) then begin
    if TDAParam(Item).ParamType = ptUnknown then
      cbParamType.ItemIndex := -1
    else begin
      Find := False;
      for i := Low(FParamTypeInfos) to High(FParamTypeInfos) do
        if FParamTypeInfos[i].ParamType = TDAParam(Item).ParamType then begin
          cbParamType.ItemIndex := i;
          Find := True;
        end;

      if not Find then
        //raise Exception.Create('EdItem: Item type not found');
        cbParamType.ItemIndex := -1;
    end;
  end;

  // Null
  if cbNullValue.Enabled then
    cbNullValue.Checked := TDAParam(Item).IsNull;

  // Size
  edSize.Text := IntToStr(TDAParam(Item).Size);
  if edSize.Text = '0' then
    edSize.Text := '';

  // Value
  if edValue.Enabled then
    edValue.Text := TDAParam(Item).Text
  else
    edValue.Text := '';
end;

procedure TDAParamsFrame.StoreItemValue(Item: TCollectionItem);
var
{$IFNDEF CLR}
  v: Variant;
{$ENDIF}
  VType: {$IFDEF VER6P}TVarType{$ELSE}integer{$ENDIF};
begin
  if (cbNullValue.Checked and IsControlEnabled(cbNullValue))
    or not IsControlEnabled(edValue) then
    Exit;

  case TDAParam(Item).DataType of
    ftLargeInt:
    begin
    {$IFDEF CLR}
      TDAParam(Item).Value := StrToInt64(edValue.Text);
    {$ELSE}
    {$IFNDEF VER6P}
      TVarDataD6(v).VType := varDecimal;
      TVarDataD6(v).VInt64 := StrToInt64(edValue.Text);
    {$ELSE}
      TVarData(v).VType := varInt64;
      TVarData(v).VInt64 := StrToInt64(edValue.Text);
    {$ENDIF}

      TDAParam(Item).Value := v;
    {$ENDIF}
    end;
    {$IFDEF VER5P}
    ftVariant:
      TDAParam(Item).Value := edValue.Text;
    {$ENDIF}
    else
      with TDAParam(Item) do begin
        if (DataType = ftUnknown) or
          (Ord(DataType) > Ord(High(TFieldType))) then
          VType := varEmpty
        else
          VType := FieldTypeVarMap[DataType];
        if VType <> varEmpty then
          Value := VarAsType(edValue.Text, VType)
        else
          Value := edValue.Text;
      end;
  end;
end;

procedure TDAParamsFrame.ControlsToItem(Item: TCollectionItem);
var
  BadControl: TWinControl;
begin
  BadControl := nil;
  try
    // DataType
    BadControl := cbDataType;
    if cbDataType.ItemIndex = - 1 then
      TDAParam(Item).DataType := ftUnknown
    else
      TDAParam(Item).DataType := FDataTypeInfos[cbDataType.ItemIndex].DataType;

    // ItemType
    if IsControlEnabled(cbParamType) then begin
      BadControl := cbParamType;
      if cbParamType.ItemIndex = - 1 then
        TDAParam(Item).ParamType := ptUnknown
      else
        TDAParam(Item).ParamType := FParamTypeInfos[cbParamType.ItemIndex].ParamType;
    end;

    // Null
    // Null must be stored before value
    BadControl := cbNullValue;
    if IsControlEnabled(cbNullValue) and cbNullValue.Checked then
      TDAParam(Item).Clear;

    // Size
    BadControl := edSize;
    if (edSize.Text <> '') and edSize.Enabled then
      TDAParam(Item).Size := StrToInt(edSize.Text)
    else
      TDAParam(Item).Size := 0;

  // Value
    // Value must be stored after Null
    BadControl := edValue;
    StoreItemValue(Item);

  except
    if Page.PageControl.ActivePage = Page then begin
      lbItemName.ItemIndex := FOldItemIndex;

      if BadControl <> nil then begin
        BadControl.SetFocus;
        if BadControl is TCustomEdit then
          TCustomEdit(BadControl).SelectAll;
      end;
    end;
    raise;
  end;
end;

procedure TDAParamsFrame.UpdateControlsState;
var
  Editable: boolean;
begin
  if cbDataType.ItemIndex = - 1 then
    Editable := False
  else
    Editable := FDataTypeInfos[cbDataType.ItemIndex].Editable;

  cbNullValue.Enabled := Editable;
  edValue.Enabled := Editable;
  bEdValue.Enabled := edValue.Enabled;
  cbParamType.Enabled := Editable;
  edSize.Enabled := Editable and FDataTypeInfos[cbDataType.ItemIndex].Sizeable;

  inherited;
end;

procedure TDAParamsFrame.cbDataTypeChange(Sender: TObject);
begin
  if FInSelectItem or not IsControlEnabled(Sender as TControl) then
    Exit;

  Modified := True;
  UpdateControlsState;
end;

procedure TDAParamsFrame.cbParamTypeChange(Sender: TObject);
begin
  if FInSelectItem or not IsControlEnabled(Sender as TControl) then
    Exit;

  Modified := True;
  UpdateControlsState;
end;

procedure TDAParamsFrame.edValueChange(Sender: TObject);
var
  l, Size: integer;
begin
  if FInSelectItem or not IsControlEnabled(Sender as TControl) then
    Exit;

  Assert(cbDataType.ItemIndex <> - 1);
  l := Length(edValue.Text);
  if cbNullValue.Checked and (l <> 0) then begin
    cbNullValue.Checked := False;
  end
  else
    if not cbNullValue.Checked and (l = 0)
      and FDataTypeInfos[cbDataType.ItemIndex].EmptyIsNull then
      cbNullValue.Checked := True;

  if edSize.Enabled then begin
    if (edSize.Text <> '') then
      Size := StrToInt(edSize.Text)
    else
      Size := 0;

    if FDataTypeInfos[cbDataType.ItemIndex].Sizeable
      and (Size <> 0) and (Size < l) then
      edSize.Text := IntToStr(l);
  end;

  Modified := True;
  UpdateControlsState;
end;

procedure TDAParamsFrame.cbNullValueClick(Sender: TObject);
begin
  if FInSelectItem or not IsControlEnabled(Sender as TControl) then
    Exit;

  Modified := True;

  if cbNullValue.Checked then
    edValue.Text := ''
  else
    if (edValue.Text = '') and (cbDataType.ItemIndex <> -1) then
      edValue.Text := FDataTypeInfos[cbDataType.ItemIndex].DefaultValue;
  UpdateControlsState;
end;

procedure TDAParamsFrame.edSizeChange(Sender: TObject);
begin
  if FInSelectItem or not IsControlEnabled(Sender as TControl) then
    Exit;

  Modified := True;
  UpdateControlsState;
end;

procedure TDAParamsFrame.AddDataType(
      Name: string; // if empty then use DB array
      DataType: TFieldType;
      EmptyIsNull: boolean;
      Editable: boolean;
      Sizeable: boolean;
      DefaultValue: string
);
var
  l: integer;
begin
  l := Length(FDataTypeInfos);
  SetLength(FDataTypeInfos, l + 1);

  FDataTypeInfos[l].Name := Name;
  FDataTypeInfos[l].DataType := DataType;
  FDataTypeInfos[l].EmptyIsNull := EmptyIsNull;
  FDataTypeInfos[l].Editable := Editable;
  FDataTypeInfos[l].Sizeable := Sizeable;
  FDataTypeInfos[l].DefaultValue := DefaultValue;
end;

procedure TDAParamsFrame.AddParamType(
      Name: string;
      ParamType: TParamType
);
var
  l: integer;
begin
  l := Length(FParamTypeInfos);
  SetLength(FParamTypeInfos, l + 1);

  FParamTypeInfos[l].Name := Name;
  FParamTypeInfos[l].ParamType := ParamType;
end;

procedure TDAParamsFrame.bEdValueClick(Sender: TObject);
begin
  Assert(Owner is TCREditorForm);
  with TDAParamValueEditor.Create(Self, TCREditorForm(Owner).CRDesignUtilsClass) do
    try
      Value := edValue.Text;
      ShowModal;
      if ModalResult = mrOK then
        edValue.Text := Value;
    finally
      Free;
    end;
end;

end.
