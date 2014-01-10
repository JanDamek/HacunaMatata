unit sq_EXT;

interface

uses
  Windows, Classes, FMX.Forms,
//  FMX.ComCtrls,
  FMX.Controls, ddType,
//  sqBrowse,
  DB;

const
  PriponaTabulkaExt = '_EXT';
  ExtEd_ep_ExtEd_ = 'ep_ExtEd_';

  ExtEd_TypEditoru_Standardni = 0;
  ExtEd_TypEditoru_ComboBox   = 1;
  ExtEd_TypEditoru_Prenos     = 2;
  ExtEd_TypEditoru_Memo       = 3;
  ExtEd_TypEditoru_DatumCas   = 4;
  ExtEd_TypEditoru_Cas        = 5;

type
  T_ExtEd_DynAttr = class
  public
    FTabNameEXT: String;
    FDynAttr: array of TAtributTabulky;
    FUP_Existuje: Boolean;
    destructor Destroy; override;
  end;

//function ExtEd_DefiniceExternichAtributu(SelectoTvurce: TSelectoTvurce): T_ExtEd_DynAttr;

function ExtEd_SeznamZalozek(PA: PAtributTabulky; Pocet: Integer): TStringList;
//procedure ExtEd_VytvorZalozky(PC: TPageControl; SLZalozky: TStringList;
//                              NultaZalozka: TTabSheet; SBMouseWheel: TMouseWheelEvent);

// nemohou se prirazovat primo, protoze nejsou typu "of object"
procedure ExtEd_SBMouseWheel(Sender: TObject; Shift: TShiftState; WheelDelta: Integer;
                             MousePos: TPoint; var Handled: Boolean);
//procedure ExtEd_Prenos_ButtonClick(SelectoTvurce: TSelectoTvurce; Sender: TObject);

procedure ExtEd_NastavReadOnly_NastavCas(ASeznam, ASeznamDTP, ASeznamFrame: TList; AReadOnly: Boolean);
procedure ExtEd_PrevezmiCas(SeznamDTP: TList);

//procedure ExtEd_GenerujEditoryZalozky(
//            SelectoTvurce: TSelectoTvurce; frm: TForm; DS: TDataSource;
//            const Zalozka: String; SB: TScrollBox; PA: PAtributTabulky;
//            Pocet: Integer; NovaVeta: Boolean; Seznam, SeznamDTP, SeznamFrame: TList;
//            _OnButtonClick, _OnChange, _dtp_OnChange: TNotifyEvent);

{ =========================================================================== }

implementation

uses
  SysUtils, Variants,
//  StdCtrls,
//  Menus,
  Math,
//  sqDmGlob,
  sqProc, ddUta, dtUzivAtr, ddMain,
//  DBEditHe,
//  sqPrenos,
  sqView, ddBrowse,
  Pom, sqString, sqEdit
//  sqKompon,
//  DBEditHe2,
//  cxCheckBox,
//  cxDBEdit,
//  sqFBarva
  ;

type
  T_ExtEd_DefItem = class
  public
    FTypEditoru: Integer; // 0=Standardni, 1=ComboBox, 2=Prenos
    FBrowseID: Integer;
    FAtr: String;
    FPrimaEditace: Boolean;
    FPoradi: Integer;
    FZalozka: String;
    FPodminka: String;
    FReadOnly: Boolean;
  end;

{ ########################################################################### }

destructor T_ExtEd_DynAttr.Destroy;
  var
    II : Integer;
begin
  if FDynAttr <> nil then
  begin
    for II := Low(FDynAttr) to High(FDynAttr) do
      if FDynAttr[II].Tag <> 0 then
        T_ExtEd_DefItem(FDynAttr[II].Tag).Free;
    FDynAttr := nil;
  end;

  inherited;
end;

{ =========================================================================== }

function ExtEd_DefiniceExternichAtributu(SelectoTvurce: TSelectoTvurce): T_ExtEd_DynAttr;
  var
    LTab, LTabExt, Lep_ExtEd : String;
    LAktID: Integer;
    SL: TStringList;
    II, Index: Integer;
    DefItem: T_ExtEd_DefItem;
    UP_Existuje: Boolean;
    DataSetFrom : TDataSet;
    QUP: TQueryHe;
    IDAlias: string;
begin
  Result := nil;

  LTab := SelectoTvurce.JmenoTabulkySys;
  LTabExt := LTab + PriponaTabulkaExt;

  // prioritni je pro ID editor kvuli "F2 - Nova"
  if Assigned(SelectoTvurce.EditorForm) then
    begin
      DataSetFrom := TFormSQLEdit(SelectoTvurce.EditorForm).DataSourceEdit.DataSet;
      IDAlias := TFormSQLEdit(SelectoTvurce.EditorForm).ExtEd_AliasProID;
      if IDAlias = '' then
        IDAlias := uta_SystemoveCislo;
    end
  else
    begin
      DataSetFrom := SelectoTvurce.QueryBrowse;
      IDAlias := uta_SystemoveCislo;
    end;
  LAktID := DataSetFrom.FieldByName(IDAlias).AsInteger;

  with Server.CreateQuery do
  try
    ParamCheck := False;

    SL := TStringList.Create;
    try
      SL.Sorted := True;

      UP_Existuje := False;

      // existuje uloženka na omezení editoru externích definovaných atributù ?
      QUP := Server.CreateQuery;
      try
        // [RK 01.09.2006] moznost vice procedur pro nastaveni prav
        QUP.SQL.Add(
          Format(
            'SELECT name FROM sysobjects'#13+
            'WHERE xtype=''P'' AND name LIKE %s AND USER_NAME(uid)=N''dbo''',
            [NQuotedStr(ExtEd_ep_ExtEd_ + LTab + '%')]));

        QUP.Open;

        // [RK 28.07.2009] tady chybel cyklus pro vyhodnoceni vsech procedur !!!
        while not QUP.eof do
        begin
          Lep_ExtEd := QUP.Fields[0].AsString;

          // ano - existuje - tak ji zavolej s parametrama ID a BrowseID
          SQL.Clear;
          SQL.Add(Format('EXEC dbo.%s %d,%d', [Lep_ExtEd, LAktID, SelectoTvurce.BrowseID]));
          Open;

          // [RK 28.07.2009] doplnena moznost ReadOnly
          // Sloupec 0 => nazev atributu, ktery je zakazany
          // Sloupec 1 => 0 = neni vubec videt(implicitni), 1 = ReadOnly
          while not EOF do
          begin
            if not Fields[0].IsNull then
            begin
              if FieldCount > 1 then
                begin
                  II := Fields[1].AsInteger;
                  if (II < 0) or (II > 1) then II := 0;
                end
              else
                II := 0;

              Index := SL.IndexOf(Fields[0].AsString);
              if Index = -1 then
                SL.AddObject(Fields[0].AsString, TObject(II))
              else
                // pokud tam jiz sloupce je, tak Zakaz ma prednost pred ReadOnly
                if II = 0 then
                  SL.Objects[Index] := TObject(II);
            end;
            Next;
          end;
          Close;

          UP_Existuje := True;

          QUP.Next;
        end;
      finally
        QUP.Free;
      end;

      SQL.Clear;
      SQL.Add(
        Format(
          'SELECT ' + {00}TabUzivAtr_NazevAtrSys+
                      {01}','+TabUzivAtr_NazevAtrVer+
                      {02}','+TabUzivAtr_TypAtr+
                      {03}','+TabUzivAtr_MaskaAtr+
                      {04}','+TabUzivAtr_SirkaSloupceAtr+
                      {05}','+TabUzivAtr_SumovatAtr+
                      {06}',COL_LENGTH(' + TabUzivAtr_NazevTabulkySys + '+N''_EXT'',' +
                                           TabUzivAtr_NazevAtrSys + ')' +
                      // [TZ 05.05.2004]
                      {07}','+TabUzivAtr_ExtEd_BrowseID+
                      {08}','+TabUzivAtr_ExtEd_Atr+
                      {09}','+TabUzivAtr_ExtEd_PrimaEditace+
                      // [RK 04.11.2005]
                      {10}','+TabUzivAtr_ExtEd_Poradi+
                      {11}','+TabUzivAtr_ExtEd_Zalozka+
                      {12}','+TabUzivAtr_ExtEd_Podminka+   // [RK 22.05.2008]
                      {13}','+TabUzivAtr_KonverzeAtr+      // [RK 23.05.2008]
                      {14}','+TabUzivAtr_BarvaAtr+         // [RK 04.12.2009]
                      {15}','+TabUzivAtr_StavAtr+
          ' FROM '  + TabUzivAtr +
          ' WHERE ' + TabUzivAtr_NazevTabulkySys + '=%s' +
          ' AND (' + TabUzivAtr_VerejnyAtr + '=%s OR ' + TabUzivAtr_VerejnyAtr + '=%s)' +
          ' AND ' + TabUzivAtr_Externi + '=1'+
          ' ORDER BY ' + TabUzivAtr_ExtEd_Zalozka + ',' +
                         TabUzivAtr_ExtEd_Poradi + ',' +
                         TabUzivAtr_NazevAtrSys,
          [NQuotedStr(LTab),
           NQuotedStr(TabUzivAtr_VerejnyAtr_VAno_EAno),
           NQuotedStr(TabUzivAtr_VerejnyAtr_VNe_EAno)]));

      Open;

      if IsEmpty then
      begin
        sqlChyba(Format(sqlCtiOznam(xTabulkaNemaDefinici), [LTabExt]));
        Exit;
      end;

      Result := T_ExtEd_DynAttr.Create;
      with Result do
      begin
        FTabNameEXT := LTabExt;
        FUP_Existuje := UP_Existuje;
      end;

      SetLength(Result.FDynAttr, 1);
      FillChar(Result.FDynAttr[0], SizeOf(TAtributTabulky), 0);

      with Result.FDynAttr[0] do
      begin
        JmenoSys := uta_SystemoveCislo;
        Typ      := taInt;
        NULL     := nNOTNULL;
      end;

      II := 1;
      while not EOF do
      begin
        Index := SL.IndexOf(Fields[0].AsString);
        if (Index = -1) or (Integer(SL.Objects[Index]) = 1{ReadOnly}) then
        begin
          //DynamicAtributy prodloužit o 1 a vynulovat
          SetLength(Result.FDynAttr, Length(Result.FDynAttr)+1);
          FillChar(Result.FDynAttr[II], SizeOf(TAtributTabulky), 0);

          with Result.FDynAttr[II] do
          begin
            JmenoSys     := Fields[0].AsString;
            JmenoVerejne := Fields[1].AsString;
            if JmenoVerejne = '' then
              JmenoVerejne := '[' + JmenoSys + ']'
            else
              //[TZ 12.10.2007] GUID na veøejné texty externích øešení
              if JeStringGUID(JmenoVerejne) then
                JmenoVerejne := sqlCtiOznamGUID(JmenoVerejne);
            Typ          := Str2TypAttr(Fields[2].AsString, 19, 6);
            Verejny      := vTrue;
            MaskaDisplay := Fields[3].AsString;
            SirkaSloupce := Fields[4].AsInteger;
            Sumovat      := (Fields[5].AsString = 'A');

            // [RK 23.05.2008] konverze (stejna podminka i v edUzivAtr)
            if (not (Typ in [taText, taNText, taBoolean])) and
               (SkupinaAtributu(Typ) in [skpRetezce, skpCelaCisla]) then
            begin
              Konverze := Fields[13].AsString;
              if JeStringGUID(Konverze) then
                Konverze := sqlCtiOznamGUID(Konverze);
            end;

            if Typ in TSkupinaAtributuSDelkou then
            begin
              case Typ of
                taNVarChar, taNChar:
                  Delka := Fields[6].AsInteger div 2;
                else
                  Delka := Fields[6].AsInteger;
              end;
            end;

            // editor barvy
            if Fields[14].AsBoolean and (Typ = taInt) and (Konverze = '') then
              Include(DalsiVlastnostiAtr, dvaBarva);

            if Fields[15].AsBoolean and (Typ = taInt) then
            begin
              Include(DalsiVlastnostiAtr, dvaStav);
              KonverzeStav := Konverze;
              Konverze := '';
            end;

            NULL := nNULL;

            DefItem := T_ExtEd_DefItem.Create;
            with DefItem do
            begin
              FReadOnly := (Index <> -1);
              if Fields[7].IsNull or (Konverze <> '') then
                FTypEditoru := ExtEd_TypEditoru_Standardni
              else
                case Fields[7].AsInteger of
                  // memo se generuje pouze pro retezcove atributy, pro cisla nema smysl
                  TabUzivAtr_ExtEd_BrowseID_Memo:
                    if SkupinaAtributu(Typ) = skpRetezce then
                      FTypEditoru := ExtEd_TypEditoru_Memo
                    else
                      FTypEditoru := ExtEd_TypEditoru_Standardni;

                  TabUzivAtr_ExtEd_BrowseID_ComboBox:
                    FTypEditoru := ExtEd_TypEditoru_ComboBox;

                  TabUzivAtr_ExtEd_BrowseID_DatumCas:
                    if Typ = taDateTime then
                      FTypEditoru := ExtEd_TypEditoru_DatumCas
                    else
                      FTypEditoru := ExtEd_TypEditoru_Standardni;

                  TabUzivAtr_ExtEd_BrowseID_Cas:
                    if Typ = taDateTime then
                      FTypEditoru := ExtEd_TypEditoru_Cas
                    else
                      FTypEditoru := ExtEd_TypEditoru_Standardni;

                  else
                    FTypEditoru := ExtEd_TypEditoru_Prenos;
                end;
              FBrowseID     := Fields[7].AsInteger;
              FAtr          := Trim(Fields[8].AsString);
              FPrimaEditace := Fields[9].AsBoolean;   
              FPoradi       := Fields[10].AsInteger;
              FZalozka      := Fields[11].AsString;
              // [RK 23.03.2009] externi hlasky
              if JeStringGUID(FZalozka) then
                FZalozka := sqlCtiOznamGUID(FZalozka);
              FPodminka     := Fields[12].AsString;
            end;

            Tag := Integer(DefItem);
          end;
          Inc(II);
        end;

        Next;
      end;
    finally
      SL.Free;
    end;
  finally
    Free;
  end;
end;

{ --------------------------------------------------------------------------- }

function ExtEd_SeznamZalozek(PA: PAtributTabulky; Pocet: Integer): TStringList;
  var
    II: Integer;
    LZalozka: String;
begin
  Result := TStringList.Create;

  for II := 1 to Pocet do
  begin
    if PA.Tag <> 0 then
    begin
      LZalozka := T_ExtEd_DefItem(PA.Tag).FZalozka;

      // poradi zalozek a atributu v zalozce je dan poradim v definici - viz SELECT vyse
      if Result.IndexOf(LZalozka) = -1 then
        Result.Add(LZalozka);
    end;

    Inc(PA);
  end;
end;

{ --------------------------------------------------------------------------- }

procedure ExtEd_VytvorZalozky(PC: TPageControl; SLZalozky: TStringList;
                              NultaZalozka: TTabSheet; SBMouseWheel: TMouseWheelEvent);
  var
    II : Integer;
    LScrollBox: TScrollBox;
    LTabSheet: TTabSheet;
begin
  for II := 0 to SLZalozky.Count-1 do
  begin
    if (II = 0) and Assigned(NultaZalozka) then
      LTabSheet := NultaZalozka
    else
      begin
        LTabSheet := TTabSheet.Create(PC.Owner);
        with LTabSheet do
        begin
          PageControl := PC;
          Caption := SLZalozky.Strings[II];
        end;
      end;

    LScrollBox := TScrollBox.Create(PC.Owner);
    with LScrollBox do
    begin
      Parent := LTabSheet;
      Width := LTabSheet.Width;
      Align := alClient;
      ParentBackground := True;
      BorderStyle := bsNone;
      OnMouseWheel := SBMouseWheel;
    end;

    SLZalozky.Objects[II] := LScrollBox;
  end;
end;

{ --------------------------------------------------------------------------- }

procedure ExtEd_SBMouseWheel(Sender: TObject; Shift: TShiftState; WheelDelta: Integer;
                             MousePos: TPoint; var Handled: Boolean);
begin
  with TScrollBox(Sender).VertScrollBar do
    Position := Position - WheelDelta;
  Handled := True;
end;

{ --------------------------------------------------------------------------- }

procedure ExtEd_Prenos_ButtonClick(SelectoTvurce: TSelectoTvurce; Sender: TObject);
  var
    LEditor   : TRxDBComboEditHE;
    PA        : PAtributTabulky;
    V         : Variant;
    OldWHEREsys : String;
    DefItem : T_ExtEd_DefItem;
    LOwner: TComponent;
begin
  LEditor := (Sender as TRxDBComboEditHE);
  if LEditor.Tag = 0 then Exit;

  PA := PAtributTabulky(LEditor.Tag);
  if PA.Tag = 0 then Exit;

  DefItem := T_ExtEd_DefItem(PA.Tag);

  if JeObecnyPrehled(DefItem.FBrowseID) then
    begin
      //Definovaný pøehled
      with SeznamVychozichNastaveniBrowse[bx_ObecnyPrehled] do
      begin
        OldWHEREsys := WHEREsys; //musím takto, protože v ZobrazObecnyPrehled() toto vùbec není ošetøeno
        WHEREsys    := '';
        try
          V := ZobrazObecnyPrehled(SelectoTvurce, DefItem.FBrowseID-x_ObecnePohledy_BID_Base,
                 DefItem.FPodminka, True, DefItem.FAtr);
        finally
          WHEREsys := OldWHEREsys;
        end;
      end;

      if not LEditor.ReadOnly then
        if not VarIsEmpty(V) then //když byl Escape-je tam Unassigned-nejlépe detekuji fcí VarIsEmpty()
          LEditor.Text := V;
    end
  else
    begin
      //Normální pøehled
      V := LEditor.Text;
      LOwner := SelectoTvurce.EditorForm;
      if not Assigned(LOwner) then
        LOwner := SelectoTvurce.BrowseForm;

      if Prenos(LOwner, SelectoTvurce, BID2Browse(DefItem.FBrowseID),
                [DefItem.FAtr], [V], DefItem.FPodminka,
                Prenos_Default_MuzuPrenos, Prenos_Default_NazevAkce,
                Prenos_Default_TitulekOkna, Prenos_Default_AutoSet,
                Prenos_Default_MultiSelect, LEditor.ReadOnly) then
        LEditor.Text := V;
    end;
end;

{ --------------------------------------------------------------------------- }

procedure ExtEd_GenerujEditoryZalozky(  
            SelectoTvurce: TSelectoTvurce; frm: TForm; DS: TDataSource;
            const Zalozka: String; SB: TScrollBox; PA: PAtributTabulky;
            Pocet: Integer; NovaVeta: Boolean; Seznam, SeznamDTP, SeznamFrame: TList;
            _OnButtonClick, _OnChange, _dtp_OnChange: TNotifyEvent);
  const
    CLeft = 15;
    CLeftOffsetCas = 10;
  var
    I, MaxL, LL : Integer;
    LEditor: TWinControl;
    LokDTP: TDateTimePickerHe;
    MaxWidth, LOffset: Integer;
    LHeight: Integer;
    LTop: Integer;
    DefItem: T_ExtEd_DefItem;
    LField: TField;
    DelejCas: Boolean;
    lb: TLabel;
begin
  LTop := 10;
  MaxWidth := SB.Width - 2*CLeft;

  for I := 1 to Pocet do
  begin
    if PA.Tag <> 0 then
    begin
      DefItem := T_ExtEd_DefItem(PA.Tag);
      if SameText(Zalozka, DefItem.FZalozka) then
      begin
        LHeight := 50;
        LOffset := 0;
        DelejCas := False;

        if JeToBarva(PA) and (DefItem.FTypEditoru in [ExtEd_TypEditoru_Standardni,
                                                      ExtEd_TypEditoru_ComboBox]) then
          begin
            LEditor := TFrameBarva.Create(frm);
            with TFrameBarva(LEditor) do
            begin
              Parent := SB;
              Name := '_xx_ExtEd_ed_xx_' + PA.JmenoSys;
              Left := CLeft;
              Top := LTop;
              Tag := Integer(PA);
              AutoSize := True;

              FOnChange_FrameDotah := _OnChange;

              // na formulari sprahnuteho prehledu by to jinak padalo
              FDataSourceEdit := DS;
              FSelectoTvurce := SelectoTvurce;

              lbVazba.Caption := PA.JmenoVerejne + ':';
              SetPropertiesOfObjectRTTI(dbceVazba,
                ['DataSource', 'DataField'], [DS, PA.JmenoSys]);
            end;

            Inc(LTop, LHeight);
            SeznamFrame.Add(Pointer(LEditor));
          end
        else
          begin
            if DataModuleGlob.JeToStav(PA) and
               (DefItem.FTypEditoru in [ExtEd_TypEditoru_Standardni,
                                        ExtEd_TypEditoru_ComboBox]) then
              begin
                LEditor := ThlsDBImageComboBox.Create(frm);
                with ThlsDBImageComboBox(LEditor) do
                begin
                  Properties.Images := DataModuleGlob.ImageList16_16;
                  DataModuleGlob.NaplnImageComboBoxIkonkami(PA, Properties);
                  Width := 120;
                  Properties.OnEditValueChanged := _OnChange;
                end;
              end
            else
            if PA.Konverze <> '' then
              begin
                { ComboBox }
                LEditor := TDBComboBoxHe.Create(frm);
                with TDBComboBoxHe(LEditor) do
                begin
                  DropDownCount := 15;
                  OnChange := _OnChange;
                  Style := csDropDownList;
                end;
                LEditor.Parent := SB; //musím pøiøadit už zde, jinak to padá v Items.Add() níže
                NaplnComboZVenku(TDBComboBoxHe(LEditor), PA.Konverze);

                MaxL := 145;
                with TDBComboBoxHe(LEditor) do
                  for LL := 0 to Items.Count-1 do
                    MaxL := Max(MaxL, frm.Canvas.TextWidth(Items[LL])+10);
                LEditor.Width := Max(120, Min(MaxWidth, MaxL+20{buttonek}));
              end
            else
            if PA.Typ = taBoolean then
              begin
                LEditor := ThlsDBCheckBox.Create(frm);
                with ThlsDBCheckBox(LEditor) do
                begin
                  Properties.NullStyle := nssUnchecked;
                  Width := MaxWidth; // kvuli verejnemu nazvu
                end;
                LHeight := 25;
              end
            else
            if PA.Typ = taDateTime then
              begin
                LEditor := TDBDateEditHE.Create(frm);
                with TDBDateEditHE(LEditor) do
                begin
                  OnChange := _OnChange;
                  if DefItem.FTypEditoru <> ExtEd_TypEditoru_Cas then
                    Width := 120
                  else
                    begin
                      Width := 0; // POZOR! testuje se v ExtEd_PrevezmiCas
                      LOffset := -CLeftOffsetCas;
                      Visible := False;
                    end;
                end;
                // na zakladnim editoru se to nenastavi
                DateEdit_NastavAtributy(TDBDateEditHE(LEditor));

                // pridani casu
                DelejCas := (DefItem.FTypEditoru in [ExtEd_TypEditoru_DatumCas, ExtEd_TypEditoru_Cas]);
              end
            else
              begin
                if (PA.Typ in [taText, taNText]) or
                   (DefItem.FTypEditoru = ExtEd_TypEditoru_Memo) then
                  begin
                    LEditor := TDBMemoHE.Create(frm);
                    with TDBMemoHE(LEditor) do
                    begin
                      Height := 90;
                      Width := MaxWidth;
                      ScrollBars := ssVertical;
                      OnChange := _OnChange;
                      if PA.Typ in [taText, taNText] then
                        // na zakladnim editoru se to nenastavi
                        OnDblClick := TFormSQLEdit(frm).FDBMemoHEDblClick;
                    end;
                    LHeight := 120;
                  end
                else
                  case DefItem.FTypEditoru of
                    ExtEd_TypEditoru_Standardni:
                      begin
                        LEditor := TDBEditHE.Create(frm);
                        with TDBEditHE(LEditor) do
                        begin
                          Width := 120;
                          OnChange := _OnChange;
                        end;
                      end;

                    ExtEd_TypEditoru_ComboBox:
                      begin
                        { ComboBox }
                        LEditor := TDBComboBoxHe.Create(frm);
                        with TDBComboBoxHe(LEditor) do
                        begin
                          DropDownCount := 15;
                          OnChange := _OnChange;
                          DeleteHint := sqlCtiOznam(txtSmazat) + ' = ' + ShortCutToText(VK_DELETE);
                          if not DefItem.FPrimaEditace then
                            Style := csDropDownList;
                        end;
                        LEditor.Parent := SB; //musím pøiøadit už zde, jinak to padá v Items.Add() níže
                        with Server.CreateQuery do
                        try
                          //[TZ 02.04.2008] jde parametrizovat dvojteèkovou notací (napø: 'SELECT ... WHERE IDHlavicka=:ID' nebo 'EXEC NejakaProcedura :ID')
                          if Pos(':', DefItem.FAtr)<>0 then
                          begin
                            //[TZ 27.11.2008] pro editor spec. vetev
                            if Assigned(SelectoTvurce.EditorForm) and Assigned(TFormSQLEdit(SelectoTvurce.EditorForm).DataSourceEdit) then
                              DataSource := TFormSQLEdit(SelectoTvurce.EditorForm).DataSourceEdit
                            else
                              DataSource := SelectoTvurce.DataSourceBrowse;
                          end
                          else
                            ParamCheck := False;
                          SQL.Text := '/*' + SelectoTvurce.JmenoTabulkySys + '.' + PA.JmenoSys + '*/' + DefItem.FAtr;
                          Open;
                          MaxL := 145;
                          while not EOF do
                          begin
                            TDBComboBoxHe(LEditor).Items.Add(Fields[0].AsString);
                            MaxL := Max(MaxL, frm.Canvas.TextWidth(Fields[0].AsString)+10);
                            Next;
                          end;
                        finally
                          Free;
                        end;
                        LEditor.Width := Max(120, Min(MaxWidth, MaxL+20{buttonek}));
                      end;

                    ExtEd_TypEditoru_Prenos:
                      begin
                        { ComboEdit s ... }
                        LEditor := TRxDBComboEditHE.Create(frm);
                        with TRxDBComboEditHE(LEditor) do
                        begin
                          OnButtonClick := _OnButtonClick;
                          OnChange := _OnChange;
                          DirectInput := DefItem.FPrimaEditace;
                          Width := 120;
                          // na zakladnim editoru se to nenastavi
                          DefaultHint := sqlCtiOznam(txtNavaznyCiselnik) + ' = ' + ShortCutToText(ClickKey);
                          Hint := DefaultHint;
                        end;
                      end;
                    else
                      LEditor := nil;
                  end;

                if LEditor <> nil then
                begin
                  LEditor.Anchors := [akLeft, akTop, akRight];
                  if PA.Typ in [taVarChar, taChar, taNVarChar, taNChar] then
                  begin
                    LEditor.Width := Max(LEditor.Width, Min(MaxWidth, frm.Canvas.TextWidth(CharStr('0', PA.Delka+2{at to neni tip top}))));
                    if LEditor is TRxDBComboEditHE then
                      LEditor.Width := Min(MaxWidth, LEditor.Width + TRxDBComboEditHE(LEditor).ButtonWidth);
                  end;
                  // [RK 24.06.2008] pokud se otevrel editor maximalizovane a pote ho uzivatel
                  // zminimalizoval, tak policka zmizela (nulova delka)
                  LEditor.Constraints.MinWidth := LEditor.Width;
                end;
              end;

            if LEditor <> nil then
            begin
              with LEditor do
              begin
                Parent := SB;
                Name := '_xx_ExtEd_ed_xx_' + PA.JmenoSys;
                Left := CLeft;
                Tag := Integer(PA);
                if PA.Typ = taBoolean then
                  Top := LTop
                else
                  Top := LTop + 20;
              end;

              if DelejCas then
              begin
                LokDTP := TDateTimePickerHe.Create(frm);
                with LokDTP do
                begin
                  Parent := SB;
                  Name := '_xx_ExtEd_dtp_xx_' + PA.JmenoSys;
                  SetBounds(LEditor.Left + LEditor.Width + LOffset + CLeftOffsetCas, LEditor.Top, 120, LEditor.Height);
                  Tag := Integer(LEditor); // odkaz na editor data
                  OnChange := _dtp_OnChange;
                end;
                SeznamDTP.Add(Pointer(LokDTP));
              end;

              SetPropertiesOfObjectRTTI(LEditor,
                ['DataSource', 'DataField'], [DS, PA.JmenoSys]);

              // [TZ 08.08.2003] u externích sloupcù typu INT a SMALLINT nešlo zadat -
              LField := DS.DataSet.FieldByName(PA.JmenoSys);
              if LField is TIntegerField then  //v sqBrowse je pro TIntegerFieldy pøednastaveno ValidChars = ValidChars - [+-]
                LField.ValidChars := LField.ValidChars + ['-'];

              if LEditor is ThlsDBCheckBox then
                begin
                  if NovaVeta then
                    LField.AsBoolean := False; // zruseni NULL

                  with ThlsDBCheckBox(LEditor) do
                  begin
                    Caption := PA.JmenoVerejne;
                    OnClick := _OnChange; // az po LField.AsBoolean := False !!
                  end;
                end
              else
                begin
                  lb := TLabel.Create(frm);
                  with lb do
                  begin
                    Parent := SB;
                    Name := '_xx_ExtEd_lb_xx_' + PA.JmenoSys;
                    Left := CLeft;
                    Top := LTop;
                    Caption := PA.JmenoVerejne + ':';
                    FocusControl := LEditor;
                  end;
                  Seznam.Add(Pointer(lb));
                end;

              Inc(LTop, LHeight);

              Seznam.Add(Pointer(LEditor));
            end;
          end;
      end;
    end;

    Inc(PA);
  end;
end;

{ --------------------------------------------------------------------------- }

procedure ExtEd_NastavReadOnly_NastavCas(ASeznam, ASeznamDTP, ASeznamFrame: TList; AReadOnly: Boolean);
  var
    II: Integer;
    PA: PAtributTabulky;
    DefItem: T_ExtEd_DefItem;
    LField: TField;
    LokDTP: TDateTimePickerHe;
begin
  SetPropertiesOfListOfObjectsRTTI(ASeznamFrame, ['ReadOnly'], [AReadOnly]);

  for II := 0 to ASeznam.Count-1 do
  begin
    if not (TObject(ASeznam[II]) is TLabel) then
    begin
      PA := PAtributTabulky(TComponent(ASeznam[II]).Tag);
      DefItem := T_ExtEd_DefItem(PA.Tag);

      SetPropertiesOfObjectRTTI(
        TObject(ASeznam[II]), ['ReadOnly'], [DefItem.FReadOnly or AReadOnly]);
    end;
  end;

  // nastaveni casovych editoru
  for II := 0 to ASeznamDTP.Count-1 do
  begin
    LokDTP := TDateTimePickerHe(ASeznamDTP.Items[II]);
    LokDTP.Enabled := not TDBDateEditHE(LokDTP.Tag).ReadOnly;

    LField := TDBDateEditHE(LokDTP.Tag).Field;
    if Assigned(LField) then
    begin
      if LField.IsNull then
        LokDTP.Time := 0
      else
        LokDTP.Time := Frac(LField.AsDateTime);
    end;
  end;
end;

{ --------------------------------------------------------------------------- }

procedure ExtEd_PrevezmiCas(SeznamDTP: TList);
  var
    II: Integer;
    LField: TField;
    LokDTP: TDateTimePickerHe;
    Hour, Min, Sec, MSec: Word;
begin
  for II := 0 to SeznamDTP.Count-1 do
  begin
    LokDTP := TDateTimePickerHe(SeznamDTP.Items[II]);
    if not TDBDateEditHE(LokDTP.Tag).ReadOnly then
    begin
      LField := TDBDateEditHE(LokDTP.Tag).Field;
      if Assigned(LField) then
      begin
        // oriznuti milisekund
        DecodeTime(LokDTP.Time, Hour, Min, Sec, MSec);
        MSec := 0;

        // pokud je datum "neviditelne" tak je to pouze cas
        if TDBDateEditHE(LokDTP.Tag).Width = 0 then
          // kdyby datum bylo 0, tak by to udelalo NULL pri nulovem case
          LField.AsDateTime := EncodeDate(2000,1,1) + EncodeTime(Hour, Min, Sec, MSec)
        else
          if not LField.IsNull then
            LField.AsDateTime := Trunc(LField.AsDateTime) + EncodeTime(Hour, Min, Sec, MSec);
      end;
    end;
  end;
end;

{ ########################################################################### }

end.
//  LIB  | USER: RK | DT: 08.11.2005 12:22:39 | VER: 20051024 | TXT:  | PC: RK-2K | FROM: D:\!\IQ1\HELIOS | TO: L:\H\IQ10\libq
// LOCAL | USER: RK | DT: 08.11.2005 15:55:08 | VER:          | TXT:  | PC: RK-2K | FROM: L:\H\IQ10\LibQ | TO: D:\!\IQ1\HELIOS
//  LIB  | USER: RK | DT: 08.11.2005 16:29:41 | VER: 20051025 | TXT:  | PC: RK-2K | FROM: D:\!\IQ1\HELIOS | TO: L:\H\IQ10\LibQ
// LOCAL | USER: RK | DT: 08.11.2005 16:29:41 | VER: 20051025 | TXT:  | PC: RK-2K | FROM: L:\H\IQ10\LibQ | TO: D:\!\IQ1\HELIOS
//  LIB  | USER: RK | DT: 09.11.2005 11:20:30 | VER: 20051027 | TXT:  | PC: RK-2K | FROM: D:\!\IQ1\HELIOS | TO: L:\H\IQ10\LibQ
// LOCAL | USER: RK | DT: 09.11.2005 11:23:26 | VER:          | TXT:  | PC: RK-2K | FROM: L:\H\IQ10\LibQ | TO: D:\!\IQ1\HELIOS
//  LIB  | USER: RK | DT: 09.11.2005 11:25:42 | VER: 20051027 | TXT:  | PC: RK-2K | FROM: D:\!\IQ1\HELIOS | TO: L:\H\IQ10\LibQ
// LOCAL | USER: RK | DT: 10.11.2005 09:10:57 | VER:          | TXT:  | PC: RK-2K | FROM: L:\H\IQ10\LibQ | TO: D:\!\IQ1\HELIOS
//  LIB  | USER: RK | DT: 10.11.2005 09:41:44 | VER: 20051027 | TXT: design | PC: RK-2K | FROM: D:\!\IQ1\HELIOS | TO: L:\H\IQ10\LibQ
// LOCAL | USER: RK | DT: 27.02.2006 12:23:40 | VER:          | TXT:  | PC: RK-2K | FROM: L:\H\IQ10\LibQ | TO: D:\!\IQ1\HELIOS
//  LIB  | USER: RK | DT: 27.02.2006 12:26:23 | VER: 20060139 | TXT:  | PC: RK-2K | FROM: D:\!\IQ1\HELIOS | TO: L:\H\IQ10\LibQ
// LOCAL | USER: RK | DT: 01.09.2006 13:12:55 | VER:          | TXT:  | PC: RK-XP-D | FROM: L:\H\IQ10\LibQ | TO: D:\!\IQ1\HELIOS
//  LIB  | USER: RK | DT: 01.09.2006 14:10:18 | VER: 20060623 | TXT: moznost vice procedur pro nastaveni prav | PC: RK-XP-D | FROM: D:\!\IQ1\HELIOS | TO: L:\H\IQ10\LibQ
// LOCAL | USER: TZ | DT: 12.10.2007 15:40:30 | VER:          | TXT:  | PC: TZ | FROM: L:\H\IQ10\LibQ | TO: C:\H\80\MsSQL
//  LIB  | USER: TZ | DT: 12.10.2007 16:04:21 | VER: 20070831 | TXT: GUID na veøejné texty externích øešení | PC: TZ | FROM: C:\H\80\MsSQL | TO: L:\H\IQ10\LibQ
// LOCAL | USER: TZ | DT: 02.04.2008 15:09:54 | VER:          | TXT:  | PC: TZ | FROM: L:\H\IQ10\LibQ | TO: C:\H\80\MsSQL
//  LIB  | USER: TZ | DT: 02.04.2008 15:25:33 | VER: 20080314 | TXT: externí editor typu ComboBox jde parametrizovat dvojteèkovou notací | PC: TZ | FROM: C:\H\80\MsSQL | TO: L:\H\IQ10\LibQ
// LOCAL | USER: RK | DT: 22.05.2008 10:04:12 | VER:          | TXT:  | PC: RK-XP-D | FROM: L:\H\IQ10\LibQ | TO: D:\!\IQ1\HELIOS
//  LIB  | USER: RK | DT: 23.05.2008 16:28:02 | VER: 20080513 | TXT:  | PC: RK-XP-D | FROM: D:\!\IQ1\HELIOS | TO: L:\H\IQ10\LibQ
// LOCAL | USER: RK | DT: 27.05.2008 12:32:14 | VER:          | TXT:  | PC: RK-XP-D | FROM: L:\H\IQ10\LibQ | TO: D:\!\IQ1\HELIOS
//  LIB  | USER: RK | DT: 27.05.2008 14:41:46 | VER: 20080514 | TXT:  | PC: RK-XP-D | FROM: D:\!\IQ1\HELIOS | TO: L:\H\IQ10\LibQ
// LOCAL | USER: rk | DT: 06.06.2008 09:41:44 | VER:          | TXT:  | PC: RK-XP-D | FROM: L:\H\IQ10\LibQ | TO: D:\!\IQ1\HELIOS
//  LIB  | USER: rk | DT: 06.06.2008 10:09:45 | VER: 20080606 | TXT: beze zmìn | PC: RK-XP-D | FROM: D:\!\IQ1\HELIOS | TO: L:\H\IQ10\LibQ
// LOCAL | USER: RK | DT: 24.06.2008 16:16:35 | VER:          | TXT:  | PC: RK-XP-D | FROM: L:\H\IQ10\LibQ | TO: D:\!\IQ1\HELIOS
//  LIB  | USER: RK | DT: 24.06.2008 16:31:15 | VER: 20080709 | TXT:  | PC: RK-XP-D | FROM: D:\!\IQ1\HELIOS | TO: L:\H\IQ10\LibQ
// LOCAL | USER: RK | DT: 18.07.2008 13:56:45 | VER:          | TXT:  | PC: RK-XP-D | FROM: L:\H\IQ10\LibQ | TO: D:\!\IQ1\PluginLCSEIUctenek
//  LIB  | USER: RK | DT: 18.07.2008 13:57:41 | VER: 20080733 | TXT:  | PC: RK-XP-D | FROM: D:\!\IQ1\PluginLCSEIUctenek | TO: L:\H\IQ10\LibQ
// LOCAL | USER: MS | DT: 20.03.2009 15:01:50 | VER:          | TXT:  | PC: MS-XP | FROM: L:\H\IQ10\LIBQ | TO: C:\MS\IQ
//  LIB  | USER: MS | DT: 20.03.2009 15:10:57 | VER: 20090309 | TXT:  | PC: MS-XP | FROM: C:\MS\IQ | TO: L:\H\IQ10\LIBQ
// LOCAL | USER: RK | DT: 23.03.2009 14:56:33 | VER:          | TXT:  | PC: RK-VI-D | FROM: L:\H\IQ10\LibQ | TO: D:\!\IQ1\HELIOS
//  LIB  | USER: RK | DT: 23.03.2009 15:32:14 | VER: 20090311 | TXT:  | PC: RK-VI-D | FROM: D:\!\IQ1\HELIOS | TO: L:\H\IQ10\LibQ
// LOCAL | USER: RK | DT: 09.04.2009 08:55:41 | VER:          | TXT:  | PC: RK-VI-D | FROM: L:\H\IQ10\LibQ | TO: D:\!\IQ1\HELIOS
//  LIB  | USER: RK | DT: 09.04.2009 09:14:09 | VER: 20090404 | TXT:  | PC: RK-VI-D | FROM: D:\!\IQ1\HELIOS | TO: L:\H\IQ10\LibQ
// LOCAL | USER: RK | DT: 15.05.2009 14:57:39 | VER:          | TXT:  | PC: RK-VI-D | FROM: L:\H\IQ10\LibQ | TO: D:\!\IQ1\HELIOS
//  LIB  | USER: RK | DT: 15.05.2009 15:50:00 | VER: 20090409 | TXT:  | PC: RK-VI-D | FROM: D:\!\IQ1\HELIOS | TO: L:\H\IQ10\LibQ
// LOCAL | USER: RK | DT: 15.05.2009 16:01:39 | VER:          | TXT:  | PC: RK-VI-D | FROM: L:\H\IQ10\LibQ | TO: D:\!\IQ1\HELIOS
//  LIB  | USER: RK | DT: 15.05.2009 16:02:08 | VER: 20090409 | TXT:  | PC: RK-VI-D | FROM: D:\!\IQ1\HELIOS | TO: L:\H\IQ10\LibQ
// LOCAL | USER: RK | DT: 19.05.2009 10:44:03 | VER:          | TXT:  | PC: RK-VI-D | FROM: L:\H\IQ10\LibQ | TO: D:\!\IQ1\HELIOS
//  LIB  | USER: RK | DT: 19.05.2009 11:48:11 | VER: 20090409 | TXT:  | PC: RK-VI-D | FROM: D:\!\IQ1\HELIOS | TO: L:\H\IQ10\LibQ
// LOCAL | USER: RK | DT: 16.06.2009 09:28:28 | VER:          | TXT:  | PC: RK-VI-D | FROM: L:\H\IQ10\LibQ | TO: D:\!\IQ1\HELIOS
//  LIB  | USER: RK | DT: 16.06.2009 09:42:23 | VER: 20090504 | TXT:  | PC: RK-VI-D | FROM: D:\!\IQ1\HELIOS | TO: L:\H\IQ10\LibQ
// LOCAL | USER: RK | DT: 26.06.2009 15:10:14 | VER:          | TXT:  | PC: RK-VI-D | FROM: L:\H\IQ10\LibQ | TO: D:\!\IQ2\HELIOS
//  LIB  | USER: RK | DT: 26.06.2009 15:10:19 | VER: 20090803 | TXT:  | PC: RK-VI-D | FROM: D:\!\IQ2\HELIOS | TO: L:\H\IQ10\LibQ
// LOCAL | USER: RK | DT: 28.07.2009 12:41:31 | VER:          | TXT:  | PC: RK-VI-D | FROM: L:\H\IQ10\LibQ | TO: D:\!\IQ2\HELIOS
//  LIB  | USER: RK | DT: 28.07.2009 14:49:00 | VER: 20090806 | TXT: ReadOnly pro externi atributy | PC: RK-VI-D | FROM: D:\!\IQ2\HELIOS | TO: L:\H\IQ10\LibQ
// LOCAL | USER: RK | DT: 24.08.2009 15:18:32 | VER:          | TXT:  | PC: RK-VI-D | FROM: L:\H\IQ10\LibQ | TO: D:\!\IQ2\HELIOS
//  LIB  | USER: RK | DT: 24.08.2009 15:30:12 | VER: 20090812 | TXT: uprava COL_LENGTH | PC: RK-VI-D | FROM: D:\!\IQ2\HELIOS | TO: L:\H\IQ10\LibQ
// LOCAL | USER: RK | DT: 29.10.2009 13:02:39 | VER:          | TXT:  | PC: RK-VI-D | FROM: L:\H\IQ10\LibQ | TO: D:\!\IQ2\HELIOS
//  LIB  | USER: RK | DT: 29.10.2009 13:22:16 | VER: 20090925 | TXT: TcxDBCheckBox | PC: RK-VI-D | FROM: D:\!\IQ2\HELIOS | TO: L:\H\IQ10\LibQ
// LOCAL | USER: RK | DT: 04.12.2009 14:42:42 | VER:          | TXT:  | PC: RK-VI-D | FROM: L:\H\IQ10\LibQ | TO: D:\!\IQ2\HELIOS
//  LIB  | USER: RK | DT: 05.12.2009 15:50:53 | VER: 20091004 | TXT:  | PC: RK-VI-D | FROM: D:\!\IQ2\HELIOS | TO: L:\H\IQ10\LibQ
// LOCAL | USER: RK | DT: 07.01.2010 14:25:17 | VER:          | TXT:  | PC: RK-VI-D | FROM: L:\H\IQ10\LibQ | TO: D:\!\IQ2\HELIOS
//  LIB  | USER: RK | DT: 07.01.2010 14:34:51 | VER: 20100106 | TXT:  | PC: RK-VI-D | FROM: D:\!\IQ2\HELIOS | TO: L:\H\IQ10\LibQ
// LOCAL | USER: michal.soch | DT: 04.05.2011 09:49:03 | VER:          | TXT:  | PC: SOCHMI-PC | FROM: L:\H\IQ10\LibQ | TO: C:\MS\IQ
//  LIB  | USER: michal.soch | DT: 04.05.2011 10:22:34 | VER: 20110405 | TXT:  | PC: SOCHMI-PC | FROM: C:\MS\IQ | TO: L:\H\IQ10\LibQ
// LOCAL | USER: roman.krupicka | DT: 05.05.2011 10:46:59 | VER:          | TXT:  | PC: KRUPICKARO-PC | FROM: L:\H\IQ10\LibQ | TO: D:\!\IQ2\HELIOS
//  LIB  | USER: roman.krupicka | DT: 05.05.2011 10:58:44 | VER: 20110405 | TXT: oprava ReadOnly u check boxu | PC: KRUPICKARO-PC | FROM: D:\!\IQ2\HELIOS | TO: L:\H\IQ10\LibQ
// LOCAL | USER: roman.krupicka | DT: 28.10.2011 22:34:10 | VER:          | TXT:  | PC: KRUPICKARO-PC | FROM: L:\H\IQ10\LibQ | TO: D:\!\IQ2\HELIOS
//  LIB  | USER: roman.krupicka | DT: 28.10.2011 23:02:12 | VER: 20111101 | TXT:  | PC: KRUPICKARO-PC | FROM: D:\!\IQ2\HELIOS | TO: L:\H\IQ10\LibQ
// LOCAL | USER: roman.krupicka | DT: 31.10.2011 09:07:03 | VER:          | TXT:  | PC: KRUPICKARO-PC | FROM: L:\H\IQ10\LibQ | TO: D:\!\IQ2\HELIOS
//  LIB  | USER: roman.krupicka | DT: 31.10.2011 09:39:54 | VER: 20111101 | TXT:  | PC: KRUPICKARO-PC | FROM: D:\!\IQ2\HELIOS | TO: L:\H\IQ10\LibQ
// LOCAL | USER: roman.krupicka | DT: 04.11.2011 14:26:10 | VER:          | TXT:  | PC: KRUPICKARO-PC | FROM: L:\H\IQ10\LibQ | TO: D:\!\IQ2\HELIOS
//  LIB  | USER: roman.krupicka | DT: 04.11.2011 15:39:58 | VER: 20111102 | TXT:  | PC: KRUPICKARO-PC | FROM: D:\!\IQ2\HELIOS | TO: L:\H\IQ10\LibQ
// LOCAL | USER: roman.krupicka | DT: 03.02.2012 08:57:50 | VER:          | TXT:  | PC: KRUPICKARO-PC | FROM: L:\H\IQ10\LibQ | TO: D:\!\IQ2\HELIOS
//  LIB  | USER: roman.krupicka | DT: 03.02.2012 08:58:11 | VER: 20120207 | TXT:  | PC: KRUPICKARO-PC | FROM: D:\!\IQ2\HELIOS | TO: L:\H\IQ10\LibQ
