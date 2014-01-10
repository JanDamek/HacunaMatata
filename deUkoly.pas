unit deUkoly;

interface

uses deType, ddType, ddTabulka, dtUkoly;

const
  TdeUkolyAtr: array [0 .. 59] of TDefEditAtr = (
    (
    GUID: '{9C0D8282-D71F-451F-9D65-62B7FD2FFA4B}';
    JmenoSys:'Predmet';
    Typ: taString;
    Popis: txtPredmet;
    ZalozkaTxt: 'Zaklad')
   ,(
    GUID: '{1A5F3359-87F8-469B-860C-0599B43E09FF}';
    JmenoSys:'ID';
    Typ: taLabel;
    ZalozkaTxt: 'Zaklad';
    PopisTxt: 'Identifikator ukol';
    onValidate:'')
    ,(
    GUID: '{67465730-AE66-4397-A2E9-EE75CA4F8C28}';
    JmenoSys: 'IDUkolu';
    Typ:taBrowse;
    Browse: bUkoly;
    PopisTxt: 'Navazne ukoly 1';
    onShow:'SELECT * FROM __table WHERE __tmp.__self = __table.ID'
    )
    ,(
    GUID: '{EF7ABD2B-C423-4841-ACC2-820BFFF2F9A8}';
    JmenoSys:'TerminZahajeni';
    Typ: taDatum;
    Popis: xTerminZahajeni;
    ZalozkaTxt: 'Zaklad')
    ,(
    GUID: '{AC013BB9-EEA8-43A3-A38C-8D5EA5A32B5A}';
    JmenoSys:'TerminSplneni';
    Typ: taDatum;
    Popis: xTerminSplneni;
    ZalozkaTxt: 'Zaklad')
    ,(
    GUID: '{CBD62828-4B4D-4A15-86B5-2AA1730896F9}';
    JmenoSys:'DatumZahajeni';
    Typ: taDatum;
    Popis: xDatumZahajeni;
    ZalozkaTxt: 'Datumy')
    ,(
    GUID: '{211DC196-96C0-44FD-8910-630D8EF4E20A}';
    JmenoSys:'DatumKontroly';
    Typ: taDatum;
    Popis: xDatumKontroly;
    ZalozkaTxt: 'Datumy')
    ,(
    GUID: '{2979019F-AF8A-4B26-8CE1-573D448F3551}';
    JmenoSys:'DatumDokonceni';
    Typ: taDatum;
    Popis: xDatumDokonceni;
    ZalozkaTxt: 'Datumy')
    ,(
    GUID: '{C8157F30-745F-46DB-9929-8E1F85221110}';
    JmenoSys:'Stav';
    Typ: taString;
    Popis: xStav;
    ZalozkaTxt: 'Zaklad')
    ,(
    GUID: '{D4D5B6AA-CE71-44DE-908E-6BA4B36A66FA}';
    JmenoSys:'Priorita';
    Typ: taString;
    Popis: xPriorita;
    ZalozkaTxt: 'Zaklad';
    onValidate:
    'DECLARE @ID INT'#13+
    'SELECT @ID=__field FROM __table'#13+
    'IF @ID>5 OR @ID<=0'#13+
       'SELECT 0, 1, ''Hodnota neni v rozmezi 0 az 5. Nastavit hodnotu na 5?'', ''UPDATE __table SET __field=5'' ELSE SELECT -1'
      )
    ,(
    GUID: '{68DE4133-19C8-4838-ACB5-6450F17E1013}';
    JmenoSys:'HotovoProcent';
    Typ: taInteger;
    Popis: xHotovoProc;
    ZalozkaTxt: 'Zaklad')
    ,(
    GUID: '{313F8A76-05FE-4331-BE77-17D6A11D95C6}';
    JmenoSys:'Zadavatel';
    Typ: taString;
    Popis: xZadavatel;
    ZalozkaTxt: 'Zaklad')
    ,(
    GUID: '{ACF041D7-6AD2-445F-B2A4-BA2E962F8C80}';
    JmenoSys:'Resitel';      {a nasledna vazba na seznam spolupracovniku}
    Typ: taString;
    Popis: xResitel;
    ZalozkaTxt: 'Zaklad')
    ,(
    GUID: '{8CCC3875-5086-4358-91E7-28B136820423}';
    JmenoSys:'IDUtvarZadavatel';
    Typ: taString;
    Popis: xUtvarZavazku;
    ZalozkaTxt: 'Zaklad')
    ,(
    GUID: '{11E778E5-943B-48CD-B519-50130A5C8149}';
    JmenoSys:'IDUtvarResitel';
    Typ: taString;
    Popis: xUtvar;
    ZalozkaTxt: 'Zaklad')
    ,(
    GUID: '{99138A9A-D69A-4B8B-B7C9-90D71C35DB28}';
    JmenoSys:'ResitelKonOs';
    Typ: taString;
    Popis: xResitelKonOs;
    ZalozkaTxt: 'Zaklad')
    ,(
    GUID: '{645DA231-B033-4358-9058-F71E39B18E48}';
    JmenoSys:'CelkemHod';
    Typ: taString;
    Popis: xCelkemHod;
    ZalozkaTxt: 'Zaklad';
    PopisTxt: 'Najeky predmet')
    ,(
    GUID: '{3DD80AFA-D6F9-453E-AB35-B05B4456BDE2}';
    JmenoSys:'HotovoHod';
    Typ: taString;
    Popis: xHotovoHod;
    ZalozkaTxt: 'Zaklad';
    PopisTxt: 'Najeky predmet')
    ,(
    GUID: '{80BA71DF-78BC-43B9-A800-3C54D9092051}';
    JmenoSys:'PlneniHod';
    Typ: taString;
    Popis: xPlneni_hod;
    ZalozkaTxt: 'Zaklad';
    PopisTxt: 'Najeky predmet')
    ,(
    GUID: '{D7BA3C18-500E-417B-AC28-6776E85AE231}';
    JmenoSys:'PlneniRozdil';
    Typ: taString;
    Popis: xPlneni_rozdil;
    ZalozkaTxt: 'Zaklad';
    PopisTxt: 'Najeky predmet')
    ,(
    GUID: '{F5B662AB-ADAA-419F-A450-7C9B27903F7F}';
    JmenoSys:'PlneniProcent';
    Typ: taString;
    Popis: xPlneniProc;
    ZalozkaTxt: 'Zaklad';
    PopisTxt: 'Najeky predmet')
    ,(
    GUID: '{665DDCE4-7CC3-4E92-820F-DFC8910BE162}';
    JmenoSys:'IDKontaktJed';
    Typ: taString;
    Popis: xKontaktniJednaniZkratka;
    ZalozkaTxt: 'Zaklad';
    PopisTxt: 'Najeky predmet')
    ,(
    GUID: '{7C08564D-D34E-4813-A2DE-4ECCEDBA8629}';
    JmenoSys:'IDUkolu';
    Typ: taString;
    Popis: xUkol;
    ZalozkaTxt: 'Zaklad';
    PopisTxt: 'Najeky predmet')
    ,(
    GUID: '{71BD42B2-68A4-4AD6-B81B-F315171D0284}';
    JmenoSys:'Popis';
    Typ: taString;
    Popis: xPopis;
    ZalozkaTxt: 'Zaklad';
    PopisTxt: 'Najeky predmet')
    ,(
    GUID: '{3FBAC070-8ACD-4260-B641-3084AABE99D2}';
    JmenoSys:'Vzor';
    Typ: taString;
    Popis: xVzor;
    ZalozkaTxt: 'Zaklad';
    PopisTxt: 'Najeky predmet')
    ,(
    GUID: '{76F36F86-81AB-490F-868D-C2763900D264}';
    JmenoSys:'TypStart';
    Typ: taString;
    Popis: xTyp;
    ZalozkaTxt: 'Zaklad';
    PopisTxt: 'Najeky predmet')
    ,(
    GUID: '{FFDA52EE-49DD-4283-B367-DECADD6B14B9}';
    JmenoSys:'StavStart';
    Typ: taString;
    Popis: xStav;
    ZalozkaTxt: 'Zaklad';
    PopisTxt: 'Najeky predmet')
    ,(
    GUID: '{6FD75A3E-9A59-490C-B9D8-BE5EDCC496F4}';
    JmenoSys:'DruhVystupuStart';
    Typ: taString;
    Popis: xDruh1;
    ZalozkaTxt: 'Zaklad';
    PopisTxt: 'Najeky predmet')
    ,(
    GUID: '{C8317ECF-598F-43AA-8643-2DE3E3930780}';
    JmenoSys:'TypKonec';
    Typ: taString;
    Popis: xTyp;
    ZalozkaTxt: 'Zaklad';
    PopisTxt: 'Najeky predmet')
    ,(
    GUID: '{6CC242C6-AEF0-4015-9825-F0C799D0C4E8}';
    JmenoSys:'StavKonec';
    Typ: taString;
    Popis: xStav;
    ZalozkaTxt: 'Zaklad';
    PopisTxt: 'Najeky predmet')
    ,(
    GUID: '{6DA5DC43-C1D1-4BDE-83CD-58AAD473F1F3}';
    JmenoSys:'DruhVystupuKonec';
    Typ: taString;
    ZalozkaTxt: 'Zaklad';
    PopisTxt: 'Najeky predmet')
    ,(
    GUID: '{ABEBEAA3-6259-4BB3-AC6A-BF0D9E2CF064}';
    JmenoSys:'CenovyOdhad';
    Typ: taString;
    ZalozkaTxt: 'Zaklad';
    PopisTxt: 'Najeky predmet')
    ,(
    GUID: '{FA843D5A-55F5-485D-8729-4FB40461FC77}';
    JmenoSys:'Generovan';
    Typ: taString;
    ZalozkaTxt: 'Zaklad';
    PopisTxt: 'Najeky predmet')
    ,(
    GUID: '{7CDE330F-137F-4F8B-844C-A17B6299E4AF}';
    JmenoSys:'CisloZakazky';
    Typ: taString;
    Popis: txtCisloZakazky;
    ZalozkaTxt: 'Zaklad';
    PopisTxt: 'Najeky predmet')
    ,(
    GUID: '{B0B28966-2954-44A8-AF97-A54B3C70DDFC}';
    JmenoSys:'NOkruhCislo';
    Typ: taString;
    Popis: txtNakladovyOkruh_JmenoZkr;
    ZalozkaTxt: 'Zaklad';
    PopisTxt: 'Najeky predmet')
    ,(
    GUID: '{C3FE9665-CCB9-47A0-BBE5-4CAD2BFFBE8E}';
    JmenoSys:'IDUkolyOpak';
    Typ: taString;
    ZalozkaTxt: 'Zaklad';
    PopisTxt: 'Najeky predmet')
    ,(
    GUID: '{981A298B-E4B0-4F18-95FD-2F6766377F99}';
    JmenoSys:'OpakovanyUkol';
    Typ: taString;
    Popis: xOpakovany;
    ZalozkaTxt: 'Zaklad';
    PopisTxt: 'Najeky predmet')
    ,(
    GUID: '{AF3F30E7-D64E-4097-916A-4F2BB71A0CFC}';
    JmenoSys:'IdVozidlo';
    Typ: taString;
    Popis: xVozidlo;
    ZalozkaTxt: 'Zaklad';
    PopisTxt: 'Id Vozidlo')
    ,(
    GUID: '{32909172-E711-4149-9410-1E61F43A6253}';
    JmenoSys:'PlneniKc';
    Typ: taString;
    ZalozkaTxt: 'Zaklad';
    PopisTxt: 'Najeky predmet')
    ,(
    GUID: '{28807264-655F-45D3-B0C9-7A94C65DF991}';
    JmenoSys:'PlneniKcBezDPH';
    Typ: taString;
    ZalozkaTxt: 'Zaklad';
    PopisTxt: 'Najeky predmet')
    ,(
    GUID: '{070B6504-DA75-45E7-9A00-4BF9CCDD7CE6}';
    JmenoSys:'PlneniVal';
    Typ: taString;
    ZalozkaTxt: 'Zaklad';
    PopisTxt: 'Najeky predmet')
    ,(
    GUID: '{2EC845DC-8C6D-4D3F-A2AF-A34E2CE30C80}';
    JmenoSys:'PlneniValBezDPH';
    Typ: taString;
    ZalozkaTxt: 'Zaklad';
    PopisTxt: 'Najeky predmet')
    ,(
    GUID: '{02D716E2-6D41-4C9E-9F49-670CBE469C22}';
    JmenoSys:'SearchKey';
    Typ: taString;
    ZalozkaTxt: 'Zaklad';
    PopisTxt: 'Najeky predmet')
    ,(
    GUID: '{78134605-2D97-4573-8626-E34089682128}';
    JmenoSys:'Synchronizovat';
    Typ: taString;
    ZalozkaTxt: 'Zaklad';
    PopisTxt: 'Najeky predmet')
    ,(
    GUID: '{1B5094B7-8F53-40E7-8C94-255CC926BD96}';
    JmenoSys:'IDKategorie';
    Typ: taString;
    ZalozkaTxt: 'Zaklad';
    PopisTxt: 'Najeky predmet')
    ,(
    GUID: '{FD196BA5-ECBE-4D6D-8582-B14C0C697D2B}';
    JmenoSys:'Kategorie';
    Typ: taString;
    ZalozkaTxt: 'Zaklad';
    PopisTxt: 'Najeky predmet')
    ,(
    GUID: '{10D98AA1-4D51-4E73-BC7B-9E4AEDD0E8A2}';
    JmenoSys:'TypCeny';
    Typ: taString;
    ZalozkaTxt: 'Zaklad';
    PopisTxt: 'Najeky predmet')
    ,(
    GUID: '{DEFC193A-256A-48E5-9E93-4332A14E1DAB}';
    JmenoSys:'Honorar';
    Typ: taString;
    ZalozkaTxt: 'Zaklad';
    PopisTxt: 'Najeky predmet')
    ,(
    GUID: '{00920843-899F-47F4-904D-97099BDBB754}';
    JmenoSys:'LimitCena';
    Typ: taString;
    ZalozkaTxt: 'Zaklad';
    PopisTxt: 'Limit cena';
    onExit:
    'DECLARE @ID DECIMAL(19,6)'#13+
    'SELECT @ID=__field FROM __table'#13+
    'IF @ID<=100'#13+
       'SELECT 2, 1, ''Zadana cena je priliz nizka''';
    onValidate:
    'DECLARE @ID INT'#13+
    'SELECT @ID=__field FROM __table'#13+
    'IF @ID<=0'#13+
       'SELECT 0, 2, ''Hodnota ceny musi byt vetsi jak 0, nastavit na 200?'', ''UPDATE __table SET __field=200'' ELSE SELECT -1'
       )
    ,(
    GUID: '{7BC6375F-7D29-4FB3-AD9C-F5ECC5F893D1}';
    JmenoSys:'IDZbozi';
    Typ: taList;
    ZalozkaTxt: 'Zaklad';
    PopisTxt: 'IDZbozi')
    ,(
    GUID: '{585040C6-BEBF-4EA0-B626-D386B330BA11}';
    JmenoSys:'IDZboziProcenta';
    Typ: taCombo;
    ZalozkaTxt: 'Zaklad';
    PopisTxt: 'Najeky predmet')
    ,(
    GUID: '{4D1A3CBF-72F3-4D01-A531-D524AE0947EA}';
    JmenoSys:'IDZboziCastka';
    Typ: taCombo;
    ZalozkaTxt: 'Zaklad';
    PopisTxt: 'Najeky predmet')
    ,(
    GUID: '{54251CEF-EE0C-4FB9-B26F-64FCAC74E305}';
    JmenoSys:'ZboziProcenta';
    Typ: taString;
    ZalozkaTxt: 'Zaklad';
    PopisTxt: 'Najeky predmet')
    ,(
    GUID: '{F9D1AB42-7AE6-415C-BE6E-0AF440407571}';
    JmenoSys:'ZboziCastka';
    Typ: taString;
    ZalozkaTxt: 'Zaklad';
    PopisTxt: 'Najeky predmet')
    ,(
    GUID: '{CC1EEF2B-2F18-43BA-A03B-3C3A18E5C284}';
    JmenoSys:'IDUkolyPeriod';
    Typ: taCombo;
    ZalozkaTxt: 'Zaklad';
    PopisTxt: 'Najeky predmet')
    ,(
    GUID: '{AAB7CCF2-C0DD-490D-99E1-6241E922C627}';
    JmenoSys:'DatumPosledniFakt';
    Typ: taDatum;
    ZalozkaTxt: 'Datumy';
    PopisTxt: 'Najeky predmet')
    ,(
    GUID: '{85D83DDD-66AB-4165-8740-397A553FDB61}';
    JmenoSys:'DatumNejstarsiNeFakt';
    Typ: taDatum;
    ZalozkaTxt: 'Datumy';
    PopisTxt: 'Najeky predmet')
    ,(
    GUID: '{6165714B-ABAD-4763-AD78-D74A73FF070B}';
    JmenoSys:'DatumNejblizsiBudFak';
    Typ: taDatum;
    ZalozkaTxt: 'Datumy';
    PopisTxt: 'Najeky predmet')
    ,(
    GUID: '{B03F4137-2B30-4A09-9D56-BFE3C82F6482}';
    JmenoSys:'DatumNejblizsiMinFak';
    Typ: taDatum;
    ZalozkaTxt: 'Datumy';
    PopisTxt: 'Najeky predmet')
    ,(
    GUID: '{80BD38F9-8018-4A74-9909-81EDDD019C90}';
    JmenoSys:'TypPlneni';
    Typ: taString;
    ZalozkaTxt: 'Zaklad';
    PopisTxt: 'typ plneni')
  );

  TdeUkoly: TDefEditRec = (
    //X:deType.deUkoly;
    Tabulka: TUkoly;

    povoleneBID: '240';
    povinneAttr: '';
    onNovaVeta: '';
    onValidate: '';

    PocetAtr: High(TdeUkolyAtr) - Low(TdeUkolyAtr) + 1;
    Atr: @TdeUkolyAtr;
  );

implementation

end.
