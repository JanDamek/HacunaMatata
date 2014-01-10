
//////////////////////////////////////////////////
//  DB Access Components
//  Copyright @ 1998-2012 Devart. All right reserved.
//  CRFldLinks
//////////////////////////////////////////////////

{$I Dac.inc}

unit CRFldLinks;

interface

uses
{$IFDEF MSWINDOWS}
  Windows,
{$ENDIF}
  StdCtrls, Controls, ExtCtrls, Forms, 
{$IFDEF FPC}
  PropEdits,
{$ELSE}
  {$IFDEF VER6P}DesignIntf, DesignEditors,{$ELSE}DsgnIntf,{$ENDIF}
{$ENDIF}
{$IFDEF FPC}
  LResources, LCLType,
{$ENDIF}
  SysUtils, Classes, DB, Buttons;

type

{ TCRFieldLinkProperty }

  TCRFieldLinkProperty = class(TStringProperty)
  private
    FChanged: Boolean;
    FDataSet: TDataSet;
  protected
    function GetDataSet: TDataSet;
    procedure GetFieldNamesForIndex(List: TStrings); virtual;
    function GetIndexBased: Boolean; virtual;
    function GetIndexDefs: TIndexDefs; virtual;
    function GetIndexFieldNames: string; virtual;
    function GetIndexName: string; virtual;
    function GetMasterFields: string; virtual; abstract;
    procedure SetIndexFieldNames(const Value: string); virtual;
    procedure SetIndexName(const Value: string); virtual;
    procedure SetMasterFields(const Value: string); virtual; abstract;
  public
    constructor CreateWith(ADataSet: TDataSet); virtual;
    procedure GetIndexNames(List: TStrings);
    property IndexBased: Boolean read GetIndexBased;
    property IndexDefs: TIndexDefs read GetIndexDefs;
    property IndexFieldNames: string read GetIndexFieldNames write SetIndexFieldNames;
    property IndexName: string read GetIndexName write SetIndexName;
    property MasterFields: string read GetMasterFields write SetMasterFields;
    property Changed: Boolean read FChanged;
    procedure Edit; override;
    function GetAttributes: TPropertyAttributes; override;
    property DataSet: TDataSet read GetDataSet;
  end;

{ TCRLinkFields }

  TCRLinkFields = class(TForm)
    DetailList: TListBox;
    MasterList: TListBox;
    BindList: TListBox;
    Label30: TLabel;
    Label31: TLabel;
    IndexList: TComboBox;
    IndexLabel: TLabel;
    Label2: TLabel;
    Bevel1: TBevel;
    Bevel2: TBevel;
    AddButton: TButton;
    DeleteButton: TButton;
    ClearButton: TButton;
    Button1: TButton;
    Button2: TButton;
    Help: TButton;
    procedure FormCreate(Sender: TObject);
    procedure BindingListClick(Sender: TObject);
    procedure AddButtonClick(Sender: TObject);
    procedure DeleteButtonClick(Sender: TObject);
    procedure BindListClick(Sender: TObject);
    procedure ClearButtonClick(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure BitBtn1Click(Sender: TObject);
    procedure HelpClick(Sender: TObject);
    procedure IndexListChange(Sender: TObject);
  private
    FDataSet: TDataSet;
    FMasterDataSet: TDataSet;
    FDataSetProxy: TCRFieldLinkProperty;
    FFullIndexName: string;
    MasterFieldList: string;
    IndexFieldList: string;
    OrderedDetailList: TStringList;
    OrderedMasterList: TStringList;
    procedure OrderFieldList(OrderedList, List: TStrings);
    procedure AddToBindList(const Str1, Str2: string);
    procedure Initialize;
    property FullIndexName: string read FFullIndexName;
    procedure SetDataSet(Value: TDataSet);
  public
    property DataSet: TDataSet read FDataSet write SetDataSet;
    property DataSetProxy: TCRFieldLinkProperty read FDataSetProxy write FDataSetProxy;
    function Edit: Boolean;
  end;

function EditMasterFields(ADataSet: TDataSet; ADataSetProxy: TCRFieldLinkProperty): Boolean;

implementation

{$IFNDEF FPC}
{$IFDEF IDE}
{$R *.dfm}
{$ELSE}
{$R CRFldLinks.dfm}
{$ENDIF}
{$ELSE}
{$R *.lfm}
{$ENDIF}

uses
  Dialogs
{$IFDEF VER6P}
  {$IFNDEF FPC}, DsnDBCst{$ENDIF}
{$ENDIF}
  {$IFNDEF FPC}, DBConsts, LibHelp, TypInfo{$ENDIF}
  ;

{ Utility Functions }

function StripFieldName(const Fields: string; var Pos: Integer): string;
var
  I: Integer;
begin
  I := Pos;
  while (I <= Length(Fields)) and (Fields[I] <> ';') do Inc(I);
  Result := Copy(Fields, Pos, I - Pos);
  if (I <= Length(Fields)) and (Fields[I] = ';') then Inc(I);
  Pos := I;
end;

function StripDetail(const Value: string): string;
var
  S: string;
  I: Integer;
begin
  S := Value;
  I := 0;
  while Pos('->', S) > 0 do
  begin
    I := Pos('->', S);
    S[I] := ' ';
  end;
  Result := Copy(Value, 0, I - 2);
end;

function StripMaster(const Value: string): string;
var
  S: string;
  I: Integer;
begin
  S := Value;
  I := 0;
  while Pos('->', S) > 0 do
  begin
    I := Pos('->', S);
    S[I] := ' ';
  end;
  Result := Copy(Value, I + 3, Length(Value));
end;

function EditMasterFields(ADataSet: TDataSet; ADataSetProxy: TCRFieldLinkProperty): Boolean;
begin
  with TCRLinkFields.Create(nil) do
  try
    DataSetProxy := ADataSetProxy;
    DataSet := ADataSet;
    Result := Edit;
  finally
    Free;
  end;
end;

{ TCRFieldLinkProperty }

function TCRFieldLinkProperty.GetIndexBased: Boolean;
begin
  Result := False;
end;

function TCRFieldLinkProperty.GetIndexDefs: TIndexDefs;
begin
  Result := nil;
end;

function TCRFieldLinkProperty.GetIndexFieldNames: string;
begin
  Result := '';
end;

function TCRFieldLinkProperty.GetIndexName: string;
begin
  Result := '';
end;

procedure TCRFieldLinkProperty.GetIndexNames(List: TStrings);
begin
end;

procedure TCRFieldLinkProperty.GetFieldNamesForIndex(List: TStrings);
begin
end;

procedure TCRFieldLinkProperty.SetIndexFieldNames(const Value: string);
begin
end;

procedure TCRFieldLinkProperty.SetIndexName(const Value: string);
begin
end;

function TCRFieldLinkProperty.GetAttributes: TPropertyAttributes;
begin
  Result := [paDialog];
end;

procedure TCRFieldLinkProperty.Edit;
begin
  FChanged := EditMasterFields(DataSet, Self);
  if FChanged then Modified;
end;

constructor TCRFieldLinkProperty.CreateWith(ADataSet: TDataSet);
begin
  FDataSet := ADataSet;
end;

function TCRFieldLinkProperty.GetDataSet: TDataSet;
begin
  if FDataSet = nil then
    FDataSet := TDataSet(GetComponent(0));
  Result := FDataSet;
end;

{ TCRLinkFields }

procedure TCRLinkFields.FormCreate(Sender: TObject);
begin
  OrderedDetailList := TStringList.Create;
  OrderedMasterList := TStringList.Create;
{$IFDEF VER6P}
{$IFNDEF FPC}
  HelpContext := hcDFieldLinksDesign;
{$ENDIF}
{$ENDIF}
end;

procedure TCRLinkFields.FormDestroy(Sender: TObject);
begin
  OrderedDetailList.Free;
  OrderedMasterList.Free;
end;

function TCRLinkFields.Edit;
{$IFDEF FPC}
var
  OldMasterDataSetActive, OldDataSetActive: Boolean;
{$ENDIF}
begin
{$IFDEF FPC}
  OldMasterDataSetActive := FMasterDataSet.Active;
  OldDataSetActive := FDataSet.Active;
  try
    FMasterDataSet.Active := True;
    FDataSet.Active := True;
{$ENDIF}

  Initialize;
  if ShowModal = mrOK then
  begin
    if FullIndexName <> '' then
      DataSetProxy.IndexName := FullIndexName else
      DataSetProxy.IndexFieldNames := IndexFieldList;
    DataSetProxy.MasterFields := MasterFieldList;
    Result := True;
  end
  else
    Result := False;

{$IFDEF FPC}
  finally
    FDataSet.Active := OldDataSetActive;
    FMasterDataSet.Active := OldMasterDataSetActive;
  end;
{$ENDIF}
end;

procedure TCRLinkFields.SetDataSet(Value: TDataSet);
{$IFNDEF VER6P}
const
  SMissingDataSource = 'Missing MasterSource or DataSource';
{$ENDIF}
{$IFDEF FPC}
const
  SMissingDataSource = 'Missing MasterSource or DataSource';
{$ENDIF}
var
  IndexDefs: TIndexDefs;
begin
  Value.FieldDefs.Update;
  IndexDefs := DataSetProxy.IndexDefs;
  if Assigned(IndexDefs) then IndexDefs.Update;
  if not Assigned(Value.DataSource) or not Assigned(Value.DataSource.DataSet) then
    DatabaseError(SMissingDataSource, Value);
  Value.DataSource.DataSet.FieldDefs.Update;
  FDataSet := Value;
  FMasterDataSet := Value.DataSource.DataSet;
end;

procedure TCRLinkFields.Initialize;
var
  SIndexName: string;

  procedure SetUpLists(const MasterFieldList, DetailFieldList: string);
  var
    I, J: Integer;
    MasterFieldName, DetailFieldName: string;
  begin
    I := 1;
    J := 1;
    while (I <= Length(MasterFieldList)) and (J <= Length(DetailFieldList)) do
    begin
      MasterFieldName := StripFieldName(MasterFieldList, I);
      DetailFieldName := StripFieldName(DetailFieldList, J);
      if (MasterList.Items.IndexOf(MasterFieldName) <> -1) and
        (OrderedDetailList.IndexOf(DetailFieldName) <> -1) then
      begin
        with OrderedDetailList do
          Objects[IndexOf(DetailFieldName)] := TObject(True);
        with DetailList.Items do Delete(IndexOf(DetailFieldName));
        with MasterList.Items do Delete(IndexOf(MasterFieldName));
        BindList.Items.Add(Format('%s -> %s',
          [DetailFieldName, MasterFieldName]));
        ClearButton.Enabled := True;
      end;
    end;
  end;

begin
  if not DataSetProxy.IndexBased then
  begin
    IndexLabel.Visible := False;
    IndexList.Visible := False;
  end
  else with DataSetProxy do
  begin
    GetIndexNames(IndexList.Items);
    if IndexFieldNames <> '' then
      SIndexName := IndexDefs.FindIndexForFields(IndexFieldNames).Name
    else SIndexName := IndexName;
    if (SIndexName <> '') and (IndexList.Items.IndexOf(SIndexName) >= 0) then
      IndexList.ItemIndex := IndexList.Items.IndexOf(SIndexName) else
      IndexList.ItemIndex := 0;
  end;
  with DataSetProxy do
  begin
    MasterFieldList := MasterFields;
    if (IndexFieldNames = '') and (IndexName <> '') and
      (IndexDefs.IndexOf(IndexName) >=0) then
      IndexFieldList := IndexDefs[IndexDefs.IndexOf(IndexName)].Fields else
      IndexFieldList := IndexFieldNames;
  end;
  IndexListChange(nil);
  FMasterDataSet.GetFieldNames(MasterList.Items);
  OrderedMasterList.Assign(MasterList.Items);
  SetUpLists(MasterFieldList, IndexFieldList);
end;

procedure TCRLinkFields.IndexListChange(Sender: TObject);
var
  I: Integer;
  IndexExp: string;
begin
  DetailList.Items.Clear;
  if DataSetProxy.IndexBased then
  begin
    DataSetProxy.IndexName := IndexList.Text;
    I := DataSetProxy.IndexDefs.IndexOf(DataSetProxy.IndexName);
    if (I <> -1) then IndexExp := DataSetProxy.IndexDefs.Items[I].Expression;
    if IndexExp <> '' then
      DetailList.Items.Add(IndexExp) else
      DataSetProxy.GetFieldNamesForIndex(DetailList.Items);
  end else
    DataSet.GetFieldNames(DetailList.Items);
  MasterList.Items.Assign(OrderedMasterList);
  OrderedDetailList.Assign(DetailList.Items);
  for I := 0 to OrderedDetailList.Count - 1 do
    OrderedDetailList.Objects[I] := TObject(False);
  BindList.Clear;
  AddButton.Enabled := False;
  ClearButton.Enabled := False;
  DeleteButton.Enabled := False;
  MasterList.ItemIndex := -1;
end;

procedure TCRLinkFields.OrderFieldList(OrderedList, List: TStrings);
var
  I, J: Integer;
  MinIndex, Index, FieldIndex: Integer;
begin
  for J := 0 to List.Count - 1 do
  begin
    MinIndex := $7FFF;
    FieldIndex := -1;
    for I := J to List.Count - 1 do
    begin
      Index := OrderedList.IndexOf(List[I]);
      if Index < MinIndex then
      begin
        MinIndex := Index;
        FieldIndex := I;
      end;
    end;
    List.Move(FieldIndex, J);
  end;
end;

procedure TCRLinkFields.AddToBindList(const Str1, Str2: string);
var
  I: Integer;
  NewField: string;
  NewIndex: Integer;
begin
  NewIndex := OrderedDetailList.IndexOf(Str1);
  NewField := Format('%s -> %s', [Str1, Str2]);
  with BindList.Items do
  begin
    for I := 0 to Count - 1 do
    begin
      if OrderedDetailList.IndexOf(StripDetail(Strings[I])) > NewIndex then
      begin
        Insert(I, NewField);
        Exit;
      end;
    end;
    Add(NewField);
  end;
end;

procedure TCRLinkFields.BindingListClick(Sender: TObject);
begin
{$IFDEF MSWINDOWS}
  AddButton.Enabled := (DetailList.ItemIndex <> LB_ERR) and
    (MasterList.ItemIndex <> LB_ERR);
{$ELSE}
  AddButton.Enabled := (DetailList.ItemIndex >= 0) and (MasterList.ItemIndex >= 0);
{$ENDIF}
end;

procedure TCRLinkFields.AddButtonClick(Sender: TObject);
var
  DetailIndex: Integer;
  MasterIndex: Integer;
begin
  DetailIndex := DetailList.ItemIndex;
  MasterIndex := MasterList.ItemIndex;
  AddToBindList(DetailList.Items[DetailIndex],
    MasterList.Items[MasterIndex]);
  with OrderedDetailList do
    Objects[IndexOf(DetailList.Items[DetailIndex])] := TObject(True);
  DetailList.Items.Delete(DetailIndex);
  MasterList.Items.Delete(MasterIndex);
  ClearButton.Enabled := True;
  AddButton.Enabled := False;
end;

procedure TCRLinkFields.ClearButtonClick(Sender: TObject);
var
  I: Integer;
  BindValue: string;
begin
  for I := 0 to BindList.Items.Count - 1 do
  begin
    BindValue := BindList.Items[I];
    DetailList.Items.Add(StripDetail(BindValue));
    MasterList.Items.Add(StripMaster(BindValue));
  end;
  BindList.Clear;
  ClearButton.Enabled := False;
  DeleteButton.Enabled := False;
  OrderFieldList(OrderedDetailList, DetailList.Items);
  DetailList.ItemIndex := -1;
  MasterList.Items.Assign(OrderedMasterList);
  for I := 0 to OrderedDetailList.Count - 1 do
    OrderedDetailList.Objects[I] := TObject(False);
  AddButton.Enabled := False;
end;

procedure TCRLinkFields.DeleteButtonClick(Sender: TObject);
var
  I: Integer;
begin
  with BindList do
  begin
    for I := Items.Count - 1 downto 0 do
    begin
      if Selected[I] then
      begin
        DetailList.Items.Add(StripDetail(Items[I]));
        MasterList.Items.Add(StripMaster(Items[I]));
        with OrderedDetailList do
          Objects[IndexOf(StripDetail(Items[I]))] := TObject(False);
        Items.Delete(I);
      end;
    end;
    if Items.Count > 0 then Selected[0] := True;
    DeleteButton.Enabled := Items.Count > 0;
    ClearButton.Enabled := Items.Count > 0;
    OrderFieldList(OrderedDetailList, DetailList.Items);
    DetailList.ItemIndex := -1;
    OrderFieldList(OrderedMasterList, MasterList.Items);
    MasterList.ItemIndex := -1;
    AddButton.Enabled := False;
  end;
end;

procedure TCRLinkFields.BindListClick(Sender: TObject);
begin
{$IFDEF MSWINDOWS}
  DeleteButton.Enabled := BindList.ItemIndex <> LB_ERR;
{$ELSE}
  DeleteButton.Enabled := BindList.ItemIndex >= 0;
{$ENDIF}
end;

procedure TCRLinkFields.BitBtn1Click(Sender: TObject);
var
  I: Integer;
begin
  MasterFieldList := '';
  IndexFieldList := '';
  FFullIndexName := '';
  with BindList do
  begin
    for I := 0 to Items.Count - 1 do
    begin
      MasterFieldList := Format('%s%s;', [MasterFieldList, StripMaster(Items[I])]);
      IndexFieldList := Format('%s%s;', [IndexFieldList, StripDetail(Items[I])]);
    end;
    if MasterFieldList <> '' then
      SetLength(MasterFieldList, Length(MasterFieldList) - 1);
    if IndexFieldList <> '' then
      SetLength(IndexFieldList, Length(IndexFieldList) - 1);
  end;
end;

procedure TCRLinkFields.HelpClick(Sender: TObject);
begin
  Application.HelpContext(HelpContext);
end;

end.
