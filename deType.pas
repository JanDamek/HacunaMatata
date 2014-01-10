unit deType;

interface

uses ddType, ddVazby;

type
  //definice tabulek s definovanymi atributy
  _TDefEditList = (deUkoly);


  TTypEditoru = (teOprava, teNovy);
  TSetDefEditType = Set of TTypEditoru;

  TTypDefEditAtr = (taNeni, taInteger, taFloat, taMoney, taString, taDatum, taCas, taDatumCas, taCombo, taList, taBrowse, taLabel);

  PDefEditAtr = ^TDefEditAtr;
  TDefEditAtr = packed record
    //definice atributu
    GUID: TGUID;                // GUID pro atribut
    JmenoSys: string;           // tabulka/vazba.atribut tabulky
    Typ: TTypDefEditAtr;        // editacni typ pole
    Delka: Integer;             // nastaveni delky
    Browse: TBrowse;            // oznaceni browsu pro typ taBrowse
    Vazba: TVztah;              // Prislusna vztah atributu k Browse

    JenomCteni: Boolean;        // polozka nebude editovatelna
    Verejny: Boolean;           // polozka je verejna a viditelna
    // textove popisky
    Zalozka: Integer;           // nazev zalozky na ktere ma byt polozka
    Skupina: Integer;           // nazev skupiny ve ktere ma byt polozka
    Popis: Integer;             // popis pole
    ZalozkaTxt: string;         // nazev zalozky na ktere ma byt polozka
    SkupinaTxt: string;         // nazev skupiny ve ktere ma byt polozka
    PopisTxt: string;           // popis pole

    // SQL eventy atributu
    onShow: String;             // sql pro naplneni pole, je volano na zactku editoru
    onEnter: String;            // sql validace pro focused editacniho pole
    onExit: String;             // sql validace pred opusteni editacniho pole
    onValidate: String;         // sql validace po ulozeni hodnot, event je spusten po onExit
    onButton: String;           // sql event tlacitka v editaci - > naplneni prenosoveho browsu

    //Omezeni na browse BID
    zakazaneBID: String;        // seznam nepovolenych BID pro atribut (seznam oddeleny carkou) vyrazuje pouze ze seznamu povolenych u DefEditRec
  end;

  PDefEditRec = ^TDefEditRec;
  TDefEditRec = packed record
    //definice editoru
    //X: _TDefEditList;         // type definice
    Tabulka: TTabulka;          // tabulka od atributu
    povoleneBID: String;        // seznam povolenych BID pro atribut (seznam oddeleny carkou)
    povinneAttr:String;         // seznam povinnych Attr pro temp. tabulku
    onNovaVeta: String;         // SQL pro napleni tmp. tab pro NOVY
    onValidate: String;         // SQL pro validaci editoru nad tabulkou
    // atributy editoru
    PocetAtr: integer;          // pocet atributu pro editaci
    Atr: PDefEditAtr;           // atributy
    //Omezeni na browse BID
  end;

//  _TArrayDefEditRec = array of TDefEditRec;

implementation

end.
