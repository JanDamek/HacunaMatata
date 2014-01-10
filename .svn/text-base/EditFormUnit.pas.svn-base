unit EditFormUnit;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Dialogs,

  ddType, ddPlugin_TLB, deType, EditorComponentUnit, FMX.Edit, FMX.TabControl, FMX.Layouts, FMX.Objects, FMX.Effects,
  FMX.Grid;

type
  TEditForm = class(TForm)
    Panel1: TPanel;
    Panel2: TPanel;
    Panel3: TPanel;
    TabControl1: TTabControl;
    TabItem1: TTabItem;
    TabItem2: TTabItem;
    Layout1: TLayout;
    cancelBtn: TButton;
    okBtn: TButton;
    StyleBook1: TStyleBook;
    procedure okBtnClick(Sender: TObject);
    procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);
    procedure cancelBtnClick(Sender: TObject);
  private
    { Private declarations }
    fHelios: IHelios;
    fDataSource: IHeQuery;
    fIDVal: Variant;
    fID: String;
    fBrowse: Integer;
    fGUID: String;

    fTempTable: String;
    fTableExist: Boolean;
    fokBtn: Boolean;

    fEditComponent: TList;
    fHlaska: THlaskaComponent;

  protected
    procedure CreateTempTable;
    procedure RemoveTempTable;
    function validatingForm: Boolean;
    function saveForm: Boolean;
  public
    { Public declarations }
    constructor CreateForm(AOwner: TComponent; ABrowse: Integer; aGUID: String; AID: String; AIDVal: Variant;
      AHelios: IHelios);
    destructor Destroy; override;
  end;

var
  EditForm: TEditForm;

implementation

uses ddBrowse, ddMain, ddTabulka, sqProc, sqString, DefEditUnit, DefEditAttrUnit;

{$R *.fmx}
{ TEditForm }

procedure TEditForm.cancelBtnClick(Sender: TObject);
begin
  fokBtn := false;
end;

constructor TEditForm.CreateForm(AOwner: TComponent; ABrowse: Integer; aGUID: String; AID: String; AIDVal: Variant;
  AHelios: IHelios);
begin
  inherited Create(AOwner);
  fokBtn := false;
  fHelios := AHelios;
  fIDVal := AIDVal;
  fID := AID;
  fBrowse := ABrowse;
  fGUID := aGUID;
  fTableExist := false;
  fEditComponent := TList.Create;
  DefEdit.findGUID(fGUID);

  fHlaska := THlaskaComponent.Create(Panel1);
  fHlaska.Parent:=Panel1;
  Panel1.InsertComponent(fHlaska);

  Caption := SeznamVychozichNastaveniBrowse[BID2Browse(ABrowse)].EditorClassStr + ' - ' + DefEdit.Nazev;

  CreateTempTable;
end;

destructor TEditForm.Destroy;
begin
  RemoveTempTable;

  fHlaska.Free;
  fEditComponent.Free;
  inherited;
end;

procedure TEditForm.FormCloseQuery(Sender: TObject; var CanClose: Boolean);
begin
  if fokBtn then
    if validatingForm then
      CanClose := saveForm
    else
      CanClose := false
  else
    CanClose := true;
  fokBtn := false;
end;

procedure TEditForm.okBtnClick(Sender: TObject);
begin
  fokBtn := true;
end;

procedure TEditForm.CreateTempTable;

var
  fTop, fLeft: Single;
  SelectStr, SelectEdit: String;
  Select, BrowseSQL, WhereSys, fColumns: TStringList;
  ABrowse: TVychoziNastaveniBrowse;
  // fAtr: PDefEditAtr;
  i: Integer;
  Sql, SqlInsert, SQlValues, fColName: String;
  Val: Variant;
  fTemNameOk: Boolean;

  function getTypeColumn(aColumn: String): String;

    procedure getLastPositionComponent(fPar: TStyledControl; var fTop, fLeft: Single);
    begin
      if fPar.ComponentCount = 0 then
      begin
        fTop := 8;
        fLeft := 16;
      end
      else
      begin
        fPar := TStyledControl(fPar.Components[fPar.ComponentCount - 1]);
        fTop := fPar.Position.Y;
        fLeft := fPar.Position.X;
      end;
    end;

  var
    fComponent: TEditorComponent;
    fParent, fPanel: TStyledControl;
    Value: Variant;
    s: String;
  begin
    // vytvori sloupec v temp tabulce a knemu vygeneruje componentu na formu
    DefAttr.findAttrFromJmenoSysInGUIDList(aColumn, DefEdit.ListOfAttr);
    Value := fDataSource.FieldByNameValues(OdstranPrefixTabulky(aColumn));

    if DefAttr.JmenoSys <> aColumn then
      fParent := Panel3
    else
    begin
      if (DefAttr.Zalozka = '') and (DefAttr.Typ <> taBrowse) then
        fParent := Panel2
      else
      begin
        if DefAttr.Typ <> taBrowse then
          s := DefAttr.Zalozka
        else
          s := DefAttr.Popis;

        s := FilterVseKrome(s, ['0' .. '9', 'A' .. 'Z', 'a' .. 'z']);
        s := 'Zalozka' + s;
        fParent := TStyledControl(TabControl1.FindComponent('TabItem' + s));
        if fParent = nil then
        begin
          fParent := TTabItem.Create(TabControl1);
          fParent.name := 'TabItem' + s;
          with TTabItem(fParent) do
          begin
            if DefAttr.Typ <> taBrowse then
              Text := DefAttr.Zalozka
            else
              Text := DefAttr.Popis;

            Parent := TabControl1;
          end;

          fPanel := TPanel.Create(fParent);
          with fPanel do
          begin
            Parent := fParent;
            Align := TAlignLayout.alClient;
            name := s;
          end;
          fParent.InsertComponent(fPanel);

          fParent := fPanel;
        end
        else
        begin
          fPanel := TStyledControl(fParent.FindComponent(s));
          fParent := fPanel;
        end;
      end;
    end;

    aColumn := StringReplace(aColumn, '.', '_', [rfReplaceAll]);
    Result := aColumn + ' NVARCHAR(255)';

    getLastPositionComponent(fParent, fTop, fLeft);

    if StringReplace(DefAttr.JmenoSys, '.', '_', [rfReplaceAll]) = aColumn then
    begin
      fComponent := TEditorComponent.CreateEdit(fParent, aColumn, fTempTable, DefAttr.Typ, fHelios, DefAttr.Browse);
      fComponent.onShow := DefAttr.onShow; // sql pro naplneni pole, je volano na zactku editoru
      fComponent.onEnter := DefAttr.onEnter; // sql validace pro focused editacniho pole
      fComponent.onExit := DefAttr.onExit; // sql validace pred opusteni editacniho pole
      fComponent.onValidate := DefAttr.onValidate; // sql validace po ulozeni hodnot
      fComponent.onButton := DefAttr.onButton; // sql event tlacitka v editaci - > naplneni prenosoveho browsu
      fComponent.onShowUser := DefAttr.onShowUser;
      fComponent.onEnterUser := DefAttr.onEnterUser;
      fComponent.onExitUser := DefAttr.onExitUser;
      fComponent.onValidateUser := DefAttr.onValidateUser;
      fComponent.onButtonUser := DefAttr.onButtonUser;
      fComponent.GUID := DefAttr.GUID;
      fComponent.Text := DefAttr.Popis;

      fTop := fTop + fComponent.Height + 5;
      if fTop > fParent.Height - (fComponent.Height + 5) then
      begin
        fTop := 8;
        fLeft := fLeft + fComponent.Width + 10;
      end;
      fComponent.Position.X := fLeft;
      fComponent.Position.Y := fTop;

    end
    else
    begin
      fComponent := TEditorComponent.CreateEdit(fParent, aColumn, fTempTable, taNeni, fHelios);

      fTop := fTop + fComponent.Height + 5;
      if fTop > fParent.Height - (fComponent.Height + 5) then
      begin
        fTop := 8;
        fLeft := fLeft + fComponent.Width + 10;
      end;
      fComponent.Position.X := fLeft;
      fComponent.Position.Y := fTop;
    end;

    // vlozeni komponenty na prislusny parent
    fComponent.Value := Value;
    fComponent.Parent := fParent;
    fParent.InsertComponent(fComponent);

    // vlozeni do seznamu editacnich poli
    fEditComponent.Add(fComponent);
  end;

begin
  Select := TStringList.Create;
  BrowseSQL := TStringList.Create;
  WhereSys := TStringList.Create;
  fColumns := TStringList.Create;
  try
    ABrowse := SeznamVychozichNastaveniBrowse[BID2Browse(fBrowse)];

    Select.Text := ABrowse.Select + DefAttr.GUIDToJmenoSys(DefEdit.ListOfAttr);

    WhereSys.Text := ABrowse.WhereSys;
    WhereSys.Add(ddTabulka_GetTabulkaDef(SeznamVychozichNastaveniBrowse[BID2Browse(fBrowse)].HlavniTabulka).JmenoSys +
      '.' + fID + '=' + VarToStr(fIDVal));

    SestavSELECT(ABrowse.HlavniTabulka, ddTabulka_GetTabulkaDef(ABrowse.HlavniTabulka).JmenoSys, BID2Browse(fBrowse),
      Select, BrowseSQL, SelectStr, SelectEdit, nil, nil, nil, WhereSys, nil, nil, nil, nil, fColumns);

    fDataSource := fHelios.OpenSQL(BrowseSQL.Text);

    SelectEdit := StringReplace(SelectEdit, '.', '_', [rfReplaceAll]);
    SelectEdit := StringReplace(SelectEdit, 'FROM ', 'FROM #Tmp', [rfReplaceAll]);
    fTempTable := Copy(SelectEdit, Pos('#Tmp', SelectEdit), Length(SelectEdit));
    if Pos(' ', fTempTable) > 0 then
      fTempTable := Copy(fTempTable, 0, Pos(' ', fTempTable));
    fTemNameOk := false;
    i := 1;
    Sql := fTempTable;
    while not fTemNameOk do
    begin
      with fHelios.OpenSQL('IF OBJECT_ID(''tempdb..' + Sql + ''')IS NOT NULL SELECT 1 ELSE SELECT 0') do
      begin
        if FieldValues(0) = 1 then
        begin
          Sql := fTempTable + '_' + IntToStr(i);
          inc(i);
        end
        else
        begin
          fTempTable := Sql;
          fTemNameOk := true;
        end;
      end;
    end;

    // vytvoreni TempTabulky a kni odpovidajici editacni polozky
    Sql := 'CREATE TABLE ' + fTempTable + ' (';
    SqlInsert := 'INSERT INTO ' + fTempTable + ' ( ';
    SQlValues := ' VALUES ( ';
    for i := 0 to fColumns.Count - 1 do
    begin
      if i > 0 then
      begin
        Sql := Sql + ', ';
        SqlInsert := SqlInsert + ', ';
        SQlValues := SQlValues + ', ';
      end;
      fColName := getTypeColumn(fColumns[i]);
      Sql := Sql + fColName;
      Val := fDataSource.FieldByNameValues(OdstranPrefixTabulky(fColumns[i]));
      fColName := Copy(fColName, 0, Pos(' ', fColName) - 1);
      SqlInsert := SqlInsert + fColName;
      if VarIsNull(Val) then
        SQlValues := SQlValues + 'NULL'
      else if VarIsEmpty(Val) then
        SQlValues := SQlValues + 'N""'
      else
        SQlValues := SQlValues + NQuotedStr(VarToStr(Val));

    end;
    Sql := Sql + ')';
    SqlInsert := SqlInsert + ' ) ' + SQlValues + ' )';
    fHelios.ExecSQL(Sql);
    fTableExist := true;
    fHelios.ExecSQL(SqlInsert);

    // nastaveni visibility pro vychozi zalozku
    TabItem1.Visible := Panel2.ChildrenCount <> 0;
  finally
    Select.Free;
    BrowseSQL.Free;
    WhereSys.Free;
    fColumns.Free;
  end;
end;

procedure TEditForm.RemoveTempTable;
begin
  if fTableExist then
    fHelios.ExecSQL('DROP TABLE ' + fTempTable);
end;

function TEditForm.saveForm: Boolean;
var
  i: Integer;
  fCom: TEditorComponent;
begin
  // ulozit hodnoty z temp tabulky do db
  // vratit tru pokud to probehlo v poradku
  Result := true;
  for i := 0 to fEditComponent.Count - 1 do
  begin
    fCom := TEditorComponent(fEditComponent.Items[i]);
    Result := Result and fCom.Save;
    if not Result then
    begin
      fCom.Edit.SetFocus;
      Break;
    end;
  end;
end;

function TEditForm.validatingForm: Boolean;
var
  i: Integer;
  fCom: TEditorComponent;
begin
  // spusti vsechny validace na formu dle toho vratit vysledek
  // prvne componenty a nasledne cely form
  Result := true;
  for i := 0 to fEditComponent.Count - 1 do
  begin
    fCom := TEditorComponent(fEditComponent.Items[i]);
    Result := Result and fCom.Validate;
    if not Result then
    begin
      fCom.Edit.SetFocus;
      Break;
    end;
  end;
  for i := 0 to fEditComponent.Count - 1 do
  begin
    fCom := TEditorComponent(fEditComponent.Items[i]);
    Result := Result and not fCom.Hlasky;
    if not Result then
    begin
      fCom.Edit.SetFocus;
      Break;
    end;
  end;

  if Assigned(fCom) then
  begin
    DefEdit.findGUID(fGUID);
    if DefEdit.onValidate <> '' then
      fHlaska.AddHlaska(mtValidace, fHelios.OpenSQL(fCom.doPrepareSql(DefEdit.onValidate)));
  end;
end;

end.
