unit HeGrid;

interface

uses
  System.SysUtils, System.Types, System.Classes, FMX.Types, FMX.Layouts, FMX.Grid, MemDS, VirtualTable, Data.DB, ddType,
  ddPlugin_TLB, FMX.Menus, deType, FMX.Controls;

type
  { THeGrid }

  THeGrid = class(TStringGrid)
  private
    { Private declarations }
    fVT: TVirtualTable;
    fDS: TDataSource;
    fBrowse: TBrowse;
    fHelios: iHelios;
    fID: String;
    fNotOpen: Boolean;

    fPopupMenu: TPopupMenu;
    fMenuItemOprava: TMenuItem;
    fMenuItemNovy: TMenuItem;
    fMenuItemOpravaPopup: TMenuItem;
    fMenuItemNovyPopup: TMenuItem;

    procedure setBrowse(const Value: TBrowse);
    procedure SetGridFields(aHlavTab: String; aColumn: TStringList);
    procedure DoOnPainting(Sender: TObject; Canvas: TCanvas; const ARect: TRectF);
    procedure SetMenuItemNovy(const Value: TMenuItem);
    procedure SetMenuItemOprava(const Value: TMenuItem);
    procedure SetMenus;
  protected
    { Protected declarations }
  public
    { Public declarations }
  published
    { Published declarations }
    constructor Create(AOwner: TComponent); override;
    constructor CreateGrid(AOwner: TComponent; aBrowse: TBrowse; aHelios: iHelios); virtual;
    destructor Destroy; override;

    procedure MenuItemNovyClick(Sender: TObject);
    procedure MenuItemOpravaClick(Sender: TObject);

    property VirtualTable: TVirtualTable read fVT;
    property Browse: TBrowse read fBrowse write setBrowse;
    property Helios: iHelios read fHelios write fHelios;
    property ID: String read fID;
    property MenuItemNovy: TMenuItem read fMenuItemNovy write SetMenuItemNovy;
    property MenuItemOprava: TMenuItem read fMenuItemOprava write SetMenuItemOprava;
  end;

procedure Register;

implementation

uses Data.Bind.DBScope, ddTabulka, ddBrowse, Data.Bind.DBLinks, Data.Bind.Components, sqProc, ddMain, DefEditUnit,
  dtHGlob, sqVazby, sqView, sqString, FMX.Bind.DBLinks, FMX.Bind.Editors, EditorUnit, FMX.Forms, deMain;

procedure Register;
begin
  RegisterComponents('HacunaMatata', [THeGrid]);
end;

{ THeGrid }

constructor THeGrid.Create(AOwner: TComponent);
var
  fBScope: TBindScopeDB;
  fBList: TBindingsList;
  fBStrList: TBindDBGridLink;
begin
  inherited;

  fVT := TVirtualTable.Create(Self);
  fDS := TDataSource.Create(Self);
  fDS.DataSet := fVT;

  fBList := TBindingsList.Create(nil);
  fBStrList := TBindDBGridLink.Create(fBList);
  fBStrList.Category := 'DB Links';
  fBStrList.AutoBufferCount := true;
  fBStrList.GridControl := Self;

  fBScope := TBindScopeDB.Create(nil);
  fBScope.DataSource := fDS;

  fBStrList.DataSource := fBScope;

  AlternatingRowBackground := true;
  MouseTracking := false;
  ShowSelectedCell := false;
  ReadOnly := true;
  fID := 'ID';
  fNotOpen := true;

  fPopupMenu := TPopupMenu.Create(Self);

  fMenuItemOpravaPopup := TMenuItem.Create(Application);
  fMenuItemOpravaPopup.Text := 'Oprava';
  fMenuItemOpravaPopup.OnClick := MenuItemOpravaClick;
  fMenuItemOpravaPopup.Parent := fPopupMenu;

  fMenuItemNovyPopup := TMenuItem.Create(Application);
  fMenuItemNovyPopup.Text := 'Novy';
  fMenuItemNovyPopup.OnClick := MenuItemNovyClick;
  fMenuItemNovyPopup.Parent := fPopupMenu;

  fPopupMenu.InsertComponent(fMenuItemNovyPopup);
  fPopupMenu.InsertComponent(fMenuItemOpravaPopup);

  fMenuItemOpravaPopup := TMenuItem.Create(Application);
  fMenuItemOpravaPopup.Text := 'Oprava uzivatelsky';
  fMenuItemOpravaPopup.OnClick := MenuItemOpravaClick;
  fMenuItemOpravaPopup.Visible := false;
  fMenuItemOpravaPopup.Parent := fPopupMenu;

  fMenuItemNovyPopup := TMenuItem.Create(Application);
  fMenuItemNovyPopup.Text := 'Novy uzivatelsky';
  fMenuItemNovyPopup.OnClick := MenuItemNovyClick;
  fMenuItemNovyPopup.Visible := false;
  fMenuItemNovyPopup.Parent := fPopupMenu;

  fPopupMenu.InsertComponent(fMenuItemNovyPopup);
  fPopupMenu.InsertComponent(fMenuItemOpravaPopup);
  PopupMenu := fPopupMenu;
end;

constructor THeGrid.CreateGrid(AOwner: TComponent; aBrowse: TBrowse; aHelios: iHelios);
begin
  Self.Create(AOwner);
  fHelios := aHelios;
  fBrowse := aBrowse;
  OnPainting := DoOnPainting;
end;

destructor THeGrid.Destroy;
begin
  fDS.Destroy;
  fVT.Destroy;
  inherited;
end;

procedure THeGrid.DoOnPainting(Sender: TObject; Canvas: TCanvas; const ARect: TRectF);
begin
  if fNotOpen and (fBrowse <> bZadny) and Assigned(fHelios) then
  begin
    setBrowse(fBrowse);
    Repaint;
  end;
end;

procedure THeGrid.MenuItemNovyClick(Sender: TObject);
begin
  // Nova polozka
end;

procedure THeGrid.MenuItemOpravaClick(Sender: TObject);
begin
  // oprava polozky
  if (Sender is TMenuItem) and not(TMenuItem(Sender).Parent is TMenuBar) then
    TStyledControl(TMenuItem(Sender).Parent).Visible := false;

  if (fBrowse <> bZadny) then
  begin
    Application.ProcessMessages;
    OpenEdit(SeznamVychozichNastaveniBrowse[fBrowse], TMenuItem(Sender).TagString, fID,
      fVT.FieldByName(fID).AsString, fHelios);
  end;
end;

procedure THeGrid.SetGridFields(aHlavTab: String; aColumn: TStringList);
var
  I: Integer;
  fAtr: PAtributTabulky;
  fField: TField;
begin
  for I := 0 to fVT.Fields.Count - 1 do
  begin
    fField := fVT.Fields[I];

    fAtr := AtributV(aHlavTab, aColumn[I]);
    if Assigned(fAtr) and (fAtr <> nil) then
    begin
      if (fAtr.Typ = taIdentity) or (fAtr.UTyp = utaSystemoveCislo) then
        fID := fAtr.JmenoSys;

      fField.DisplayLabel := sqlCtiOznam(fAtr.JmenoVerejneTxt);
      if fField.DisplayLabel = '' then
        fField.DisplayLabel := aColumn[I];

      fField.Visible := (fAtr.Verejny <> TTristavovyBool.vFalse) and (fAtr.SirkaSloupce <> 0);

      if fField.Visible then
        fField.DisplayWidth := fAtr.SirkaSloupce * round(RowHeight / 2);
    end
    else
      fField.Visible := false;
  end;
end;

procedure THeGrid.SetMenuItemNovy(const Value: TMenuItem);
begin
  fMenuItemNovy := Value;

  if Assigned(fMenuItemNovy) then
    fMenuItemNovy.OnClick := MenuItemNovyClick;
end;

procedure THeGrid.SetMenuItemOprava(const Value: TMenuItem);
begin
  fMenuItemOprava := Value;

  if Assigned(fMenuItemOprava) then
    fMenuItemOprava.OnClick := MenuItemOpravaClick;
end;

procedure THeGrid.SetMenus;
var
  mi: TMenuItem;
  I: Integer;
begin
  if fBrowse <> bZadny then
  begin
    if Assigned(fMenuItemNovy) then
    begin
      MenuItemNovy.Visible := false;
      for I := MenuItemNovy.ComponentCount - 1 downto 0 do
      begin
        if (MenuItemNovy.Components[I] is TMenuItem) then
        begin
          mi := TMenuItem(MenuItemNovy.Components[I]);
          MenuItemNovy.RemoveComponent(mi);
          mi.Free;
        end;
      end;

    end;
    if Assigned(fMenuItemOprava) then
    begin
      MenuItemOprava.Visible := false;
      for I := MenuItemOprava.ComponentCount - 1 downto 0 do
      begin
        if (MenuItemOprava.Components[I] is TMenuItem) then
        begin
          mi := TMenuItem(MenuItemOprava.Components[I]);
          MenuItemOprava.RemoveComponent(mi);
          mi.Free;
        end;
      end;

    end;
    fMenuItemOpravaPopup.Visible := false;
    fMenuItemNovyPopup.Visible := false;
    for I := fMenuItemOpravaPopup.ComponentCount - 1 downto 0 do
    begin
      if (fMenuItemOpravaPopup.Components[I] is TMenuItem) then
      begin
        mi := TMenuItem(fMenuItemOpravaPopup.Components[I]);
        fMenuItemOpravaPopup.RemoveComponent(mi);
        mi.Free;
      end;
    end;
    for I := fMenuItemNovyPopup.ComponentCount - 1 downto 0 do
    begin
      if (fMenuItemNovyPopup.Components[I] is TMenuItem) then
      begin
        mi := TMenuItem(fMenuItemNovyPopup.Components[I]);
        fMenuItemNovyPopup.RemoveComponent(mi);
        mi.Free;
      end;
    end;

    DefEdit.filterBrowse(fBrowse);
    try
      DefEdit.First;
      while not DefEdit.Eof do
      begin
        if teNovy in DefEdit.Typ then
        begin
          if Assigned(fMenuItemNovy) then
          begin
            mi := TMenuItem.Create(Application);
            with mi do
            begin
              OnClick := MenuItemNovyClick;
              TagString := DefEdit.StringGUID;
              Text := DefEdit.Nazev;
              Parent := MenuItemNovy;
            end;
            MenuItemNovy.InsertComponent(mi);
            MenuItemNovy.Visible := true;
          end;

          mi := TMenuItem.Create(Application);
          with mi do
          begin
            OnClick := MenuItemNovyClick;
            TagString := DefEdit.StringGUID;
            Text := DefEdit.Nazev;
            Parent := fMenuItemNovyPopup;
          end;
          fMenuItemNovyPopup.InsertComponent(mi);
          fMenuItemNovyPopup.Visible := true;

        end;
        if teOprava in DefEdit.Typ then
        begin
          if Assigned(fMenuItemOprava) then
          begin
            mi := TMenuItem.Create(Application);
            with mi do
            begin
              OnClick := MenuItemOpravaClick;
              TagString := DefEdit.StringGUID;
              Text := DefEdit.Nazev;
              Parent := MenuItemOprava;
            end;
            MenuItemOprava.InsertComponent(mi);
            MenuItemOprava.Visible := true;
          end;

          mi := TMenuItem.Create(Application);
          with mi do
          begin
            OnClick := MenuItemOpravaClick;
            TagString := DefEdit.StringGUID;
            Text := DefEdit.Nazev;
            Parent := fMenuItemOpravaPopup;
          end;
          fMenuItemOpravaPopup.InsertComponent(mi);
          fMenuItemOpravaPopup.Visible := true;
        end;
        DefEdit.next;
      end;
    finally
      DefEdit.stopFilter;
    end;
  end;
end;

procedure THeGrid.setBrowse(const Value: TBrowse);
var
  aBrowse: TVychoziNastaveniBrowse;
  pTab: PTabulkaDef;
  SelectStr, SelectEdit, aTable, aHlavTab: String;
  Select, BrowseSQL, WHERE, WHERESys, WHEREsysDataskop, ORDERBY, ToColumn: TStringList;
  fDataSet: iHeQuery;
  I: Integer;
begin
  if Assigned(fHelios) then
  begin
    fBrowse := Value;
    aBrowse := SeznamVychozichNastaveniBrowse[fBrowse];
    pTab := ddTabulka_GetTabulkaDef(aBrowse.HlavniTabulka);
    Select := TStringList.Create;
    BrowseSQL := TStringList.Create;
    WHERE := TStringList.Create;
    WHERESys := TStringList.Create;
    WHEREsysDataskop := TStringList.Create;
    ORDERBY := TStringList.Create;
    ToColumn := TStringList.Create;
    try

      Select.Text := aBrowse.Select;
      WHERE.Text := aBrowse.WHERE;
      WHERESys.Text := aBrowse.WHERESys;
      WHEREsysDataskop.Text := '';
      ORDERBY.Text := aBrowse.ORDERBY;

      SestavSELECT(aBrowse.HlavniTabulka, pTab^.JmenoSys, fBrowse, Select, BrowseSQL, SelectStr, SelectEdit, nil, nil,
        WHERE, WHERESys, WHEREsysDataskop, ORDERBY, nil, nil, ToColumn);

      // D.SetTable(BrowseSQL.Text, pTab.JmenoSys, ToColumn);
      aTable := BrowseSQL.Text;
      aHlavTab := pTab^.JmenoSys;
      if Pos(':__IDOBDOBI', aTable) > 0 then
        aTable := StringReplace(aTable, ':__IDOBDOBI', IntToStr(fHelios.Obdobi), [rfReplaceAll]);

      if Pos(':__SKLAD', aTable) > 0 then
        aTable := StringReplace(aTable, ':__SKLAD', fHelios.Sklad, [rfReplaceAll]);

      if Pos(':__POKLADNA', aTable) > 0 then
        aTable := StringReplace(aTable, ':__POKLADNA', fHelios.Pokladna, [rfReplaceAll]);

      if Pos(':__IDMZDOBD', aTable) > 0 then
        aTable := StringReplace(aTable, ':__IDMZDOBD', IntToStr(fHelios.MzdObd), [rfReplaceAll]);

      if Pos(':__INTRUNITS', aTable) > 0 then
        aTable := StringReplace(aTable, ':__INTRUNITS', '0', [rfReplaceAll]);

      if Pos(':__RADAUCTENEK', aTable) > 0 then
        aTable := StringReplace(aTable, ':__RADAUCTENEK', '0', [rfReplaceAll]);

      fDataSet := fHelios.OpenSQL(aTable);
      fVT.DeleteFields;
      for I := 0 to fDataSet.FieldCount - 1 do
        fVT.AddField(fDataSet.Fields(I).FieldName, ftString, 255);
      Selected := 0;

      fVT.Open;
      SetGridFields(aHlavTab, ToColumn);
      BeginUpdate;
      fVT.DisableControls;

      while not fDataSet.Eof do
      begin
        fVT.Insert;
        for I := 0 to fVT.Fields.Count - 1 do
          fVT.Fields[I].Value := fDataSet.FieldByNameValues(fVT.Fields[I].FieldName);
        fVT.Post;
        fDataSet.next;
      end;

    finally
      EndUpdate;
      fVT.EnableControls;
      fNotOpen := false;
      Select.Free;
      BrowseSQL.Free;
      WHERE.Free;
      WHERESys.Free;
      WHEREsysDataskop.Free;
      ORDERBY.Free;
      ToColumn.Free;
      SetMenus;
    end;
  end;
end;

initialization

RegisterFmxClasses([THeGrid]);

end.
