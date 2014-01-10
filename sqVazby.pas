unit sqVazby;

interface

uses
   Classes, Graphics, ddType, Generics.Collections;

const
  Vazby_VID_Ident_DynVazba = -1;

type
  { TStromVazebUzel }
  TTypUzlu = (tuAutomat, // generovany z predpisu v ddVazby
    tuProgram, // vytvoreny programatorem v OnBeforePrint
    tuUser, // vytvoreny uzivatelsky
    tuJITPipe // JITPipeline - napr. CastkaSlovy, nelze nic vazat
    );

  TKteraStranaVazby = (ksvZadna, ksvLeva, ksvPrava);

  TStromVazebUzel = class;

  { +++++++++++++++++++++++++++ }

  TStromVazebUzel = class(TList)
  private
    // function GetMasterTabStr: String;
    function GetUzel(Index: Integer): TStromVazebUzel;

  public
    Uroven: Integer; // cislo vztahove urovne, zacina od 1

    Tabulka: TTabulka;
    TabulkaStr: String; // slave tabulka nize uvedeneho vztahu

    PVD: PVztahDef; // MasterTab -> Vztah -> Tabulka, odpovidajici struktura

    Alias: String; // alias pro Pipeline napr. Organizace_18_16
    Owner: TStromVazebUzel; // do ktereho seznamu tento uzel patri
    Cisilka: String; // cisla vazeb pro nazev napr. _18_16
    VerejnyAlias: String;
    TypUzlu: TTypUzlu;

    constructor Create(aOwner: TStromVazebUzel; aTabulka: TTabulka;
      const aTabulkaStr: String; aPVD: PVztahDef; aTypUzlu: TTypUzlu;
      const aAlias, aVerejnyAlias: String);
    procedure Clear; override;

    // property MasterTabStr : String read GetMasterTabStr;
    property Uzel[Index: Integer]: TStromVazebUzel read GetUzel;
  end;

  { +++++++++++++++++++++++++++ }

  TStromVazebRoot = class(TList)
  private
    SeznamVsech: TList;
    function GetUzel(Index: Integer): TStromVazebUzel;

  public
    SeznamVazeb: TStringList;
    SeznamAliasu: TStringList;
    Browse: TBrowse; // pro ktery browse to je generovano
    Prelozen: Boolean; // PFA 22.3.2011 je strom prelozen? (GER)
    constructor Create(const AHeliosPrelozen: Boolean);
    // PFA 22.3.2011 - pridana constanta AHeliosPrelozen (GER)
    destructor Destroy; override;
    procedure Clear; override;
    procedure ZrusPodurovne(const LRootUzel: TStromVazebUzel);
    procedure ZrusUzel(const LRootUzel: TStromVazebUzel);
    procedure ExpandujUzel(const LRootUzel: TStromVazebUzel); overload;
    procedure ExpandujUzel(const aAlias: String); overload;
    procedure ExpandujUzelXUrovni(const LRootUzel: TStromVazebUzel;
      KolikUrovni: Integer); overload;
    procedure ExpandujUzelXUrovni(const aAlias: String;
      KolikUrovni: Integer); overload;
    procedure PridejVztah(const LRootUzel: TStromVazebUzel;
      NewPVD: PVztahDef); overload;
    procedure PridejVztah(const aAlias: String; NewPVD: PVztahDef); overload;
    procedure PridejVztah(const LRootUzel: TStromVazebUzel;
      NewVztah: TVztah); overload;
    procedure PridejVztah(const aAlias: String; NewVztah: TVztah); overload;
    function PridejPipelineX(const aPipeSysNazev, aPipeVerejnyNazev,
      aMasterPipeline: String; aTabulka: TTabulka; aTypUzlu: TTypUzlu;
      Prelozeno: TStavPrelozeniPL = spplNeprelozeno): Boolean;
    function FindByAlias(const aAlias: String): TStromVazebUzel;
    procedure GenerujUrovne(aMasterTab: TTabulka; const aMasterTabStr: String;
      aBrowse: TBrowse; aMaxUroven: Integer; const aMasterPipeName: String);
    procedure NaplnSeznamVazeb;
    procedure NaplnSeznamAliasu;

{$IFDEF Ladit}
    procedure VypisStromVazeb(const FileName: String);
{$ENDIF}
    property Uzel[Index: Integer]: TStromVazebUzel read GetUzel;
  end;

  { +++++++++++++++++++++++++++ }

  TSlovnikVazeb = class(TDictionary<string, PVztahDef>)
  private
    FCaseSensitive: Boolean;
  public
    constructor CreateSpec(ACapacity: Integer; ACaseSensitive: Boolean);
    procedure PridejVazbu(const AKlic: string; aPVD: PVztahDef);
    function NajdiVazbu(const AKlic: string; out AValue: PVztahDef): Boolean;
  end;

  { +++++++++++++++++++++++++++ }

  // TSpecialStringList - TStringList s upravenym hledanim
  TSpecialStringListFindType = (sslfFirst, sslfNext);

  TSpecialStringList = class(TStringList)
  public
    FFindType: TSpecialStringListFindType;
    function Find(const S: string; var Index: Integer): Boolean; override;
  end;

  TJakaJeToVazba = (jjtvInterni, jjtvDefinovana, jjtvVicenasobnyVztah);

  { --------------------------------------------------------------------------- }

function StromVazebRoot(aMasterTab: TTabulka; const aMasterTabStr: String;
  aBrowse: TBrowse; aMaxUroven: Integer; aTakeSeznamVazeb: Boolean;
  const aMasterPipeName: String): TStromVazebRoot;
// PFA 22.3.2011 nova funkce StromVazebRootEng (GER)
function StromVazebRootEng(aMasterTab: TTabulka; const aMasterTabStr: String;
  aBrowse: TBrowse; aMaxUroven: Integer; aTakeSeznamVazeb: Boolean;
  const aMasterPipeName: String): TStromVazebRoot;
function JeVazbaZakazana(aBrowse: TBrowse; aVztah: TVztah): Boolean;
function JakaJeToVazba(PVD: PVztahDef): TJakaJeToVazba;
function BarvaProVazbu(PVD: PVztahDef; var Barva: TColor): Boolean;

function JeToVazbaSamaNaSebe(Tabulka: TTabulka; const TabulkaStr: String;
  PVD: PVztahDef): Boolean;

function PorovnejTabVeVazbe(Tabulka: TTabulka; const TabulkaStr: String;
  PVD: PVztahDef): TKteraStranaVazby;

procedure RegenerujSeznamyVazeb(NactiDynamicke: Boolean);

{ --------------------------------------------------------------------------- }

const
  SeznamVazebPodleVazeb: TSlovnikVazeb = nil;
  SeznamVazebPodleGUID: TSlovnikVazeb = nil;
  SeznamVazebPodleTabulek: TSpecialStringList = nil;

  { =========================================================================== }

implementation

uses
  SysUtils, sqString, sqProc, ddMain, ddVazby, ddBrowse, sqView, sqEasyHash,
  dtDynVazba, sqDynVazba, dtObecnaVazba;

var
  SeznamZakazanychVazeb: TStringList = nil;
  DynPoleObecnychVazeb: array of TVztahDef = nil;

  { ########################################################################### }

constructor TSlovnikVazeb.CreateSpec(ACapacity: Integer;
  ACaseSensitive: Boolean);
begin
  FCaseSensitive := ACaseSensitive;
  inherited Create(ACapacity);
end;

{ --------------------------------------------------------------------------- }

procedure TSlovnikVazeb.PridejVazbu(const AKlic: string; aPVD: PVztahDef);
begin
  if AKlic<>'' then
  try
    if FCaseSensitive then
      Add(AnsiUpperCase(AKlic), aPVD)
    else
      Add(AKlic, aPVD);
  except
    // pokud to bylo duplicitni, tak na to kasleme, protoze se stejne pracovalo s prvnim nalezenym
  end;
end;

{ --------------------------------------------------------------------------- }

function TSlovnikVazeb.NajdiVazbu(const AKlic: string;
  out AValue: PVztahDef): Boolean;
begin
  if FCaseSensitive then
    Result := TryGetValue(AnsiUpperCase(AKlic), AValue)
  else
    Result := TryGetValue(AKlic, AValue);
end;

{ =========================================================================== }

function JeToVazbaSamaNaSebe(Tabulka: TTabulka; const TabulkaStr: String;
  PVD: PVztahDef): Boolean;
begin
  if Tabulka = tx_ObecnyPrehled then
    // definovane vazby
    Result := SameText(PVD.TabLStr, PVD.TabPStr) and
      SameText(PVD.TabLStr, TabulkaStr)
  else
    // vazby Heliosu
    Result := (PVD.TabL = PVD.TabP) and (PVD.TabL = Tabulka);
end;

{ --------------------------------------------------------------------------- }

function PorovnejTabVeVazbe(Tabulka: TTabulka; const TabulkaStr: String;
  PVD: PVztahDef): TKteraStranaVazby;
begin
  // pokud to je vazba sama na sebe a je to ta spravna tabulka,
  // tak je treba porovnat podle Typu, protoze vazba muze byt v tomto pripade
  // definovana pouze z jedne strany
  if JeToVazbaSamaNaSebe(Tabulka, TabulkaStr, PVD) then
  begin
    if PVD.TypLP <> tvZadny then
      Result := ksvLeva
    else if PVD.TypPL <> tvZadny then
      Result := ksvPrava
    else
      Result := ksvZadna;
  end
  else if Tabulka = tx_ObecnyPrehled then
  begin
    // definovane vazby
    if SameText(PVD.TabLStr, TabulkaStr) then
      Result := ksvLeva
    else if SameText(PVD.TabPStr, TabulkaStr) then
      Result := ksvPrava
    else
      Result := ksvZadna;
  end
  else
    // vazby Heliosu
    if PVD.TabL = Tabulka then
      Result := ksvLeva
    else if PVD.TabP = Tabulka then
      Result := ksvPrava
    else
      Result := ksvZadna;
end;

{ --------------------------------------------------------------------------- }

function JakaJeToVazba(PVD: PVztahDef): TJakaJeToVazba;
begin
  if PVD.X <> vObecnaTab_ObecnaTab then
    Result := jjtvInterni
  else if PVD.VID = Vazby_VID_Ident_DynVazba then
    Result := jjtvVicenasobnyVztah
  else
    Result := jjtvDefinovana;
end;

{ --------------------------------------------------------------------------- }

function BarvaProVazbu(PVD: PVztahDef; var Barva: TColor): Boolean;
begin
  case JakaJeToVazba(PVD) of
    jjtvDefinovana:
      Barva := clBlue;

    jjtvVicenasobnyVztah:
      Barva := clGreen;

  else
    begin
      Result := False;
      Exit;
    end;
  end;

  Result := True;
end;

{ =========================================================================== }
{ =====                        TStromVazebUzel                          ===== }
{ =========================================================================== }

constructor TStromVazebUzel.Create(aOwner: TStromVazebUzel; aTabulka: TTabulka;
  const aTabulkaStr: String; aPVD: PVztahDef; aTypUzlu: TTypUzlu;
  const aAlias, aVerejnyAlias: String);
begin
  inherited Create;

  Owner := aOwner;
  Tabulka := aTabulka;
  TabulkaStr := aTabulkaStr;
  PVD := aPVD;
  TypUzlu := aTypUzlu;

  if Assigned(aOwner) then
  begin
    Uroven := aOwner.Uroven + 1;
    aOwner.Add(Self);

    // // doplneni VID pro alias vazby
    // Cisilka := Format('%s_%d', [aOwner.Cisilka, PVD.VID]);

    // [RK 04.08.2006] doplneni GUIDVazby pro alias vazby
    Cisilka := Format('%s_%s', [aOwner.Cisilka, PVD.GUIDVazby]);
  end;

  if TypUzlu = tuAutomat then
    Alias := aAlias + Cisilka
  else
    Alias := aAlias;

  if aVerejnyAlias <> '' then
    VerejnyAlias := aVerejnyAlias
  else if TypUzlu = tuAutomat then
    VerejnyAlias := Alias
  else
    VerejnyAlias := '[' + Alias + ']';
end;

{ --------------------------------------------------------------------------- }

procedure TStromVazebUzel.Clear;
var
  II: Integer;
begin
  for II := 0 to Count - 1 do
    Uzel[II].Free;
  inherited Clear;
end;

{ --------------------------------------------------------------------------- }
(*
  function TStromVazebUzel.GetMasterTabStr: String;
  begin
  if Assigned(Owner) then
  Result := Owner.TabulkaStr
  else
  Result := '';
  end;
*)
{ --------------------------------------------------------------------------- }

function TStromVazebUzel.GetUzel(Index: Integer): TStromVazebUzel;
begin
  Result := TStromVazebUzel(Items[Index]);
end;

{ =========================================================================== }
{ =====                         TStromVazebRoot                         ===== }
{ =========================================================================== }

constructor TStromVazebRoot.Create(const AHeliosPrelozen: Boolean);
begin
  inherited Create;

  SeznamVazeb := TStringList.Create;
  SeznamVsech := TList.Create;

  SeznamAliasu := TStringList.Create;
  SeznamAliasu.Sorted := True;

  Browse := bZadny;

  Prelozen := AHeliosPrelozen;
end;

{ --------------------------------------------------------------------------- }

destructor TStromVazebRoot.Destroy;
begin
  FreeAndNil(SeznamAliasu);
  FreeAndNil(SeznamVsech);
  FreeAndNil(SeznamVazeb);
  inherited Destroy;
end;

{ --------------------------------------------------------------------------- }

procedure TStromVazebRoot.Clear;
var
  II: Integer;
begin
  for II := 0 to Count - 1 do
    Uzel[II].Free;
  inherited Clear;
end;

{ --------------------------------------------------------------------------- }

function TStromVazebRoot.GetUzel(Index: Integer): TStromVazebUzel;
begin
  Result := TStromVazebUzel(Items[Index]);
end;

{ --------------------------------------------------------------------------- }

function TStromVazebRoot.FindByAlias(const aAlias: String): TStromVazebUzel;
var
  II: Integer;
begin
  for II := 0 to SeznamVsech.Count - 1 do
  begin
    Result := TStromVazebUzel(SeznamVsech.Items[II]);
    if SameText(Result.Alias, aAlias) then
      Exit;
  end;

  Result := nil;
end;

{ --------------------------------------------------------------------------- }

function TStromVazebRoot.PridejPipelineX(const aPipeSysNazev, aPipeVerejnyNazev,
  aMasterPipeline: String; aTabulka: TTabulka; aTypUzlu: TTypUzlu;
  Prelozeno: TStavPrelozeniPL = spplNeprelozeno): Boolean;
var
  Uzel: TStromVazebUzel;
begin
  Result := False;

  if (aTypUzlu <> tuUser) and (((Prelozen) and (Prelozeno = spplNeprelozeno)) or
    ((not Prelozen) and (Prelozeno = spplPrelozeno))) then
    Exit;

  if aMasterPipeline = '' then
    Uzel := nil
  else
  begin
    Uzel := FindByAlias(aMasterPipeline);
    if Uzel = nil then
      Exit;
  end;

  Uzel := TStromVazebUzel.Create(Uzel, aTabulka, JmenoTabulky(aTabulka),
    @DefiniceVztahu[vBlbe], aTypUzlu, aPipeSysNazev, aPipeVerejnyNazev);
  SeznamVsech.Add(Uzel);
  if aMasterPipeline = '' then
    Self.Add(Uzel);

  if SeznamAliasu.Count > 0 then
    SeznamAliasu.AddObject(aPipeSysNazev, Uzel);

  Result := True;
end;

{ --------------------------------------------------------------------------- }

procedure TStromVazebRoot.PridejVztah(const LRootUzel: TStromVazebUzel;
  NewPVD: PVztahDef);
var
  LTypVztahu: TTypVztahu;
  NextTab: TTabulka;
  NextTabStr: String;
  Uzlicek: TStromVazebUzel;
  PomAlias: String;
  VerAlias: String;
begin
  if NewPVD = LRootUzel.PVD then
    Exit; // zabraneni primemu cyklu
  if JeVazbaZakazana(Browse, NewPVD.X) then
    Exit;

  case PorovnejTabVeVazbe(LRootUzel.Tabulka, LRootUzel.TabulkaStr, NewPVD) of
    ksvLeva:
      with NewPVD^ do
      begin
        if (Prelozen) and (NazevLPSysEng = '') then
          Exit;
        LTypVztahu := TypLP;
        NextTab := TabP;
        NextTabStr := TabPStr;
        if (Prelozen) then
          PomAlias := NazevLPSysEng
        else
          PomAlias := NazevLPSys;
        VerAlias := NazevLP;
      end;

    ksvPrava:
      with NewPVD^ do
      begin
        if (Prelozen) and (NazevPLSysEng = '') then
          Exit;
        LTypVztahu := TypPL;
        NextTab := TabL;
        NextTabStr := TabLStr;
        if (Prelozen) then
          PomAlias := NazevPLSysEng
        else
          PomAlias := NazevPLSys;
        VerAlias := NazevPL;
      end;

  else
    begin
      LTypVztahu := tvZadny;
      NextTab := tZadna;
    end;
  end;

  if (LTypVztahu = tvZadny) or (NextTab = tZadna) then
    Exit;

  // otestovani cyklickych vazeb pouze v dane vetvi, oprava [RK 19.02.2002]
  Uzlicek := LRootUzel.Owner;
  while Uzlicek <> nil do
  begin
    if (Uzlicek.Tabulka = NextTab) and (Uzlicek.PVD = NewPVD) then
      Exit;
    Uzlicek := Uzlicek.Owner;
  end;

  if LTypVztahu = tv1N then
    PomAlias := 'x' + PomAlias;

  Uzlicek := TStromVazebUzel.Create(LRootUzel, NextTab, NextTabStr, NewPVD,
    tuAutomat, PomAlias, VerAlias);
  SeznamVsech.Add(Uzlicek);

  if SeznamAliasu.Count > 0 then
    SeznamAliasu.AddObject(Uzlicek.Alias, Uzlicek);
end;

{ --------------------------------------------------------------------------- }

procedure TStromVazebRoot.PridejVztah(const aAlias: String; NewPVD: PVztahDef);
var
  Uzlik: TStromVazebUzel;
begin
  Uzlik := FindByAlias(aAlias);
  if Uzlik <> nil then
    PridejVztah(Uzlik, NewPVD);
end;

{ --------------------------------------------------------------------------- }

procedure TStromVazebRoot.PridejVztah(const LRootUzel: TStromVazebUzel;
  NewVztah: TVztah);
begin
  PridejVztah(LRootUzel, @DefiniceVztahu[NewVztah]);
end;

{ --------------------------------------------------------------------------- }

procedure TStromVazebRoot.PridejVztah(const aAlias: String; NewVztah: TVztah);
begin
  PridejVztah(aAlias, @DefiniceVztahu[NewVztah]);
end;

{ --------------------------------------------------------------------------- }

procedure TStromVazebRoot.ZrusPodurovne(const LRootUzel: TStromVazebUzel);
var
  II, JJ: Integer;
begin
  for II := 0 to LRootUzel.Count - 1 do
  begin
    ZrusPodurovne(LRootUzel.Uzel[II]);

    JJ := SeznamVsech.IndexOf(LRootUzel.Uzel[II]);
    if JJ <> -1 then
      SeznamVsech.Delete(JJ);

    if SeznamAliasu.Count > 0 then
    begin
      JJ := SeznamAliasu.IndexOf(LRootUzel.Uzel[II].Alias);
      if JJ <> -1 then
        SeznamAliasu.Delete(JJ);
    end;
  end;

  LRootUzel.Clear;
end;

{ --------------------------------------------------------------------------- }

procedure TStromVazebRoot.ZrusUzel(const LRootUzel: TStromVazebUzel);
var
  II: Integer;
  Seznam: TList;
begin
  ZrusPodurovne(LRootUzel);

  II := SeznamVsech.IndexOf(LRootUzel);
  if II <> -1 then
    SeznamVsech.Delete(II);

  if SeznamAliasu.Count > 0 then
  begin
    II := SeznamAliasu.IndexOf(LRootUzel.Alias);
    if II <> -1 then
      SeznamAliasu.Delete(II);
  end;

  if LRootUzel.Owner = nil then
    Seznam := Self
  else
    Seznam := LRootUzel.Owner;

  II := Seznam.IndexOf(LRootUzel);
  if II <> -1 then
    Seznam.Delete(II);

  LRootUzel.Free;
end;

{ --------------------------------------------------------------------------- }

procedure TStromVazebRoot.ExpandujUzel(const LRootUzel: TStromVazebUzel);
var
  VV: Integer;
  MTStr: String;
  PVD: PVztahDef;
begin
  // [RK 28.06.2001] porychleni - nalezeni prvni vazby s danou tabulkou
  MTStr := IntToStr(Integer(LRootUzel.Tabulka));
  VV := SeznamVazebPodleTabulek.IndexOf(MTStr);
  if VV = -1 then
    Exit;

  for VV := VV to SeznamVazebPodleTabulek.Count - 1 do
  begin
    if not(SameText(SeznamVazebPodleTabulek.Strings[VV], MTStr)) then
      Break;

    PVD := PVztahDef(SeznamVazebPodleTabulek.Objects[VV]);
    PridejVztah(LRootUzel, PVD);
  end;
end;

{ --------------------------------------------------------------------------- }

procedure TStromVazebRoot.ExpandujUzel(const aAlias: String);
var
  Uzlik: TStromVazebUzel;
begin
  Uzlik := FindByAlias(aAlias);
  if Uzlik <> nil then
    ExpandujUzel(Uzlik);
end;

{ --------------------------------------------------------------------------- }

procedure TStromVazebRoot.ExpandujUzelXUrovni(const LRootUzel: TStromVazebUzel;
  KolikUrovni: Integer);
var
  II, JJ, IndexOd, IndexDo: Integer;
begin
  if KolikUrovni < 1 then
    Exit;

  IndexOd := SeznamVsech.Count;
  ExpandujUzel(LRootUzel);
  IndexDo := SeznamVsech.Count - 1;

  for II := 2 to KolikUrovni do
  begin
    for JJ := IndexOd to IndexDo do
      ExpandujUzel(TStromVazebUzel(SeznamVsech.Items[JJ]));

    IndexOd := IndexDo + 1;
    IndexDo := SeznamVsech.Count - 1;
  end;
end;

{ --------------------------------------------------------------------------- }

procedure TStromVazebRoot.ExpandujUzelXUrovni(const aAlias: String;
  KolikUrovni: Integer);
var
  Uzlik: TStromVazebUzel;
begin
  Uzlik := FindByAlias(aAlias);
  if Uzlik <> nil then
    ExpandujUzelXUrovni(Uzlik, KolikUrovni);
end;

{ --------------------------------------------------------------------------- }

procedure TStromVazebRoot.GenerujUrovne(aMasterTab: TTabulka;
  const aMasterTabStr: String; aBrowse: TBrowse; aMaxUroven: Integer;
  const aMasterPipeName: String);
var
  II: Integer;
  Uzlik: TStromVazebUzel;
  LAlias: String;
begin
  Clear;
  SeznamVazeb.Clear;
  SeznamAliasu.Clear;
  SeznamVsech.Clear;
  SeznamVsech.Capacity := 1000; // porychleni

  if aMasterPipeName <> '' then
    LAlias := aMasterPipeName
  else
    // odstraneni znaku # z nazvu tabulky
    LAlias := FilterStr(JmenoTabulky(SeznamVychozichNastaveniBrowse[aBrowse]
      .HlavniTabulka), ['#']);

  Uzlik := TStromVazebUzel.Create(nil, aMasterTab, aMasterTabStr,
    @DefiniceVztahu[vBlbe], tuAutomat, LAlias, SeznamVychozichNastaveniBrowse
    [aBrowse].Jmeno);
  Self.Add(Uzlik);
  SeznamVsech.Add(Uzlik);

  Browse := aBrowse;

  if aMaxUroven < 1 then
    Exit;
  SeznamVazebPodleTabulek.FFindType := sslfFirst; { ! hledam prvni vyskyt ! }

  II := 0;
  repeat
    Uzlik := TStromVazebUzel(SeznamVsech.Items[II]);

    // pokud uz jsou vsechny urovne, tak konec
    if Uzlik.Uroven >= aMaxUroven then
      Break;

    ExpandujUzel(Uzlik);

    Inc(II);
  until II >= SeznamVsech.Count;
end;

{ --------------------------------------------------------------------------- }

procedure TStromVazebRoot.NaplnSeznamVazeb;
var
  II: Integer;

  { +++++++++++++++++++++++++++ }

  procedure __NaplnSeznamVazeb(aRoot: TStromVazebUzel;
    const SysNazevVazby: string);
  var
    LL: Integer;
    PomS: string;
  begin
    with aRoot do
      for LL := 0 to Count - 1 do
      begin
        PomS := Uzel[LL].PVD.JmenoSys;
        if SysNazevVazby <> '' then
          PomS := Format('%s.%s', [SysNazevVazby, PomS]);

        SeznamVazeb.AddObject(PomS, Uzel[LL]);
        __NaplnSeznamVazeb(Uzel[LL], PomS);
      end;
  end;

{ +++++++++++++++++++++++++++ }

begin
  SeznamVazeb.Clear;
  for II := 0 to Count - 1 do
    __NaplnSeznamVazeb(Uzel[II], '');
end;

{ --------------------------------------------------------------------------- }

procedure TStromVazebRoot.NaplnSeznamAliasu;
var
  II: Integer;

  { +++++++++++++++++++++++++++ }

  procedure __NaplnSeznamAliasu(aRoot: TStromVazebUzel);
  var
    LL: Integer;
  begin
    with aRoot do
    begin
      SeznamAliasu.AddObject(Alias, aRoot);
      for LL := 0 to Count - 1 do
        __NaplnSeznamAliasu(Uzel[LL]);
    end;
  end;

{ +++++++++++++++++++++++++++ }

begin
  SeznamAliasu.Clear;
  for II := 0 to Count - 1 do
    __NaplnSeznamAliasu(Uzel[II]);
end;

{ =========================================================================== }

{$IFDEF Ladit}

procedure TStromVazebRoot.VypisStromVazeb(const FileName: String);
var
  F: TextFile;
  LL, JJ: Integer;

  { ++++++++++++++++++++ }

  procedure VypisRoot(aRoot: TStromVazebUzel);
  var
    II: Integer;
  begin
    with aRoot do
      for II := 0 to Count - 1 do
      begin
        with Uzel[II] do
          writeln(F, CharStr(' ', 2 * (Uroven - 1)), '--  ', Uroven,
            '  SLAVE: ', Pad(TabulkaStr, 20), '  ALIAS: ', Pad(Alias, 20),
            '  VZTAH: ', PVD.JmenoSys, ' (VID:', PVD.VID, ', GUIDVazby:',
            PVD.GUIDVazby, ')');

        VypisRoot(Uzel[II]);
      end;
  end;

{ ++++++++++++++++++++ }

begin
  try
    AssignFile(F, FileName);
    Rewrite(F);

    for JJ := 0 to Count - 1 do
    begin
      writeln(F, Uzel[JJ].TabulkaStr);
      VypisRoot(Uzel[JJ]);
    end;

    if Assigned(SeznamVazeb) then
    begin
      writeln(F);
      writeln(F, 'Seznam navaznych vazeb:');
      writeln(F);
      with SeznamVazeb do
        for LL := 0 to Count - 1 do
          writeln(F, Strings[LL], ' (ALIAS: ', TStromVazebUzel(Objects[LL])
            .Alias, ')');
    end;

    CloseFile(F);
  except
  end;
end;
{$ENDIF}
{ --------------------------------------------------------------------------- }

function StromVazebRoot(aMasterTab: TTabulka; const aMasterTabStr: String;
  aBrowse: TBrowse; aMaxUroven: Integer; aTakeSeznamVazeb: Boolean;
  const aMasterPipeName: String): TStromVazebRoot;
begin
  Result := TStromVazebRoot.Create(False);
  // PFA 22.3.2011 - nove volano s parametrem
  Result.GenerujUrovne(aMasterTab, aMasterTabStr, aBrowse, aMaxUroven,
    aMasterPipeName);
  if aTakeSeznamVazeb then
    Result.NaplnSeznamVazeb;

{$IFDEF Ladit}
  // Result.VypisStromVazeb('c:\StrVazeb.txt');
{$ENDIF}
end;

{ --------------------------------------------------------------------------- }
// PFA 22.3.2011 nova funkce StromVazebRootEng (GER)
function StromVazebRootEng(aMasterTab: TTabulka; const aMasterTabStr: String;
  aBrowse: TBrowse; aMaxUroven: Integer; aTakeSeznamVazeb: Boolean;
  const aMasterPipeName: String): TStromVazebRoot;
begin
  Result := TStromVazebRoot.Create(True);
  Result.GenerujUrovne(aMasterTab, aMasterTabStr, aBrowse, aMaxUroven,
    aMasterPipeName);
  if aTakeSeznamVazeb then
    Result.NaplnSeznamVazeb;

{$IFDEF Ladit}
  // Result.VypisStromVazeb('c:\StrVazeb.txt');
{$ENDIF}
end;

{ =========================================================================== }
{ =====                        TSpecialStringList                       ===== }
{ =========================================================================== }

function TSpecialStringList.Find(const S: string; var Index: Integer): Boolean;
begin
  Result := inherited Find(S, Index);
  if not Result then
    Exit;

  case FFindType of
    sslfNext:
      // najdi posledni vyskyt a nastav se na nasledujici
      while (Index < Count) and SameText(S, Strings[Index]) do
        Inc(Index);

    sslfFirst:
      // najdi prvni vyskyt
      while (Index > 1) and SameText(S, Strings[Index - 1]) do
        Dec(Index);
  end;
end;

{ --------------------------------------------------------------------------- }

procedure RegenerujSeznamyVazeb(NactiDynamicke: Boolean);
var
  LVztah: TVztah;
  LCap: Integer;
  PVD: PVztahDef;
  II{, PP}: Integer;
//  SL: TStringList;
//  TabVazba, Attr, DetailStr: String;
//  LTabL, LTabP: String;
//  VV, EE: Integer;
//  Delej: Boolean;
//  StejneTab, DveVazby: Boolean;
//  LAttr, LJoin: string;
//  LJeVazba: array of Boolean;
begin
  DynPoleObecnychVazeb := nil;
  ClearSeznamDynamickychTabulek;

  FreeAndNil(SeznamVazebPodleVazeb);
  FreeAndNil(SeznamVazebPodleGUID);
  FreeAndNil(SeznamVazebPodleTabulek);

  try
//    if NactiDynamicke then
//    begin
//      try
//        HeliosEasy_AttrAndJoin('v.' + TabObecnaVazba_GUIDVazby, LAttr, LJoin);
//
//        with Server.CreateQuery do
//          try
//            // --- DEFINOVANE VAZBY ---
//            SQL.Add(Format('SELECT ' + { 00 } 'v.' +
//              TabObecnaVazba_JmenoSys + ',' +
//              { 01 } 'v.' + TabObecnaVazba_VID + ',' +
//              { 02 } 'v.' + TabObecnaVazba_TabL + ',' +
//              { 03 } 'v.' + TabObecnaVazba_NazevLP + ',' +
//              { 04 } 'v.' + TabObecnaVazba_NazevLPSys + ',' +
//              { 05 } 'v.' + TabObecnaVazba_TypLP + ',' +
//              { 06 } 'v.' + TabObecnaVazba_TabP + ',' +
//              { 07 } 'v.' + TabObecnaVazba_NazevPL + ',' +
//              { 08 } 'v.' + TabObecnaVazba_NazevPLSys + ',' +
//              { 09 } 'v.' + TabObecnaVazba_TypPL + ',' +
//              { 10 } 'v.' + TabObecnaVazba_Podminka + ',' +
//              { 11 } 'v.' + TabObecnaVazba_GUIDVazbyTxt + ',' +
//              { 12 } '%s'#13 + 'FROM ' + TabObecnaVazba + ' v'#13 + '%s',
//              [LAttr, LJoin]));
//
//            Open;
//
//            if not IsEmpty then
//            begin
//              SetLength(LJeVazba, RecordCount);
//
//              if not Server.JeToHeliosEasy then
//              begin
//                // vsechno je povoleno
//                PP := RecordCount;
//                for II := Low(LJeVazba) to High(LJeVazba) do
//                  LJeVazba[II] := True;
//              end
//              else
//              begin
//                II := Low(LJeVazba);
//                PP := 0;
//                while not eof do
//                begin
//                  LJeVazba[II] := HeliosEasy_JeHashOK(Fields[11].AsString,
//                    TabObecnaVazba,
//                    BinaryToHexaString(Fields[12].AsAnsiString));
//                  if LJeVazba[II] then
//                    Inc(PP); // pocet povolenych
//
//                  Inc(II);
//                  Next;
//                end;
//              end;
//
//              if PP > 0 then
//              begin
//                SetLength(DynPoleObecnychVazeb, PP);
//                II := Low(DynPoleObecnychVazeb);
//                FillChar(DynPoleObecnychVazeb[II], SizeOf(TVztahDef) * PP, 0);
//
//                EE := Low(LJeVazba);
//                First;
//                while not eof do
//                begin
//                  if LJeVazba[EE] then
//                  begin
//                    with DynPoleObecnychVazeb[II] do
//                    begin
//                      X := vObecnaTab_ObecnaTab;
//
//                      if Fields[11].IsNull then
//                      begin
//                        // neni GUID, tak postaru
//                        JmenoSys := Fields[0].AsString;
//                        VID := Fields[1].AsInteger;
//                        GUIDVazby := IntToStr(VID);
//                      end
//                      else
//                      begin
//                        // nove s GUIDem (bagrace zavorek a pomlcek)
//                        GUIDVazby := GUIDBezPomlcek(Fields[11].AsString);
//                        JmenoSys := sqlDefVazbaPrefix + GUIDVazby;
//                        VID := 0;
//                      end;
//
//                      // pro jistotu udelame prvni pismeno velke
//                      if JmenoSys <> '' then
//                        JmenoSys[1] := UpCase(JmenoSys[1]);
//
//                      // leva strana
//                      TabLStr := Fields[2].AsString;
//                      TabL := TypTabulky(TabLStr);
//                      if TabL = tZadna then
//                        TabL := tx_ObecnyPrehled;
//                      NazevLP := Fields[3].AsString;
//                      // [TZ 27.03.2007] GUID na veøejné texty externích øešení
//                      if JeStringGUID(NazevLP) then
//                        NazevLP := sqlCtiOznamGUID(NazevLP);
//                      NazevLPSys := Fields[4].AsString;
//                      case Fields[5].AsInteger of
//                        TabObecnaVazba_Typ_1_1:
//                          TypLP := tv11;
//                        TabObecnaVazba_Typ_1_N:
//                          TypLP := tv1N;
//                      else
//                        TypLP := tvZadny;
//                      end;
//
//                      // prava strana
//                      TabPStr := Fields[6].AsString;
//                      TabP := TypTabulky(TabPStr);
//                      if TabP = tZadna then
//                        TabP := tx_ObecnyPrehled;
//                      NazevPL := Fields[7].AsString;
//                      // [TZ 27.03.2007] GUID na veøejné texty externích øešení
//                      if JeStringGUID(NazevPL) then
//                        NazevPL := sqlCtiOznamGUID(NazevPL);
//                      NazevPLSys := Fields[8].AsString;
//                      case Fields[9].AsInteger of
//                        TabObecnaVazba_Typ_1_1:
//                          TypPL := tv11;
//                        TabObecnaVazba_Typ_1_N:
//                          TypPL := tv1N;
//                      else
//                        TypPL := tvZadny;
//                      end;
//
//                      Join := Fields[10].AsString;
//
//                      // vsechny ridici znaky vymazeme
//                      for PP := 1 to Length(Join) do
//                        if CharInSet(Join[PP], [#0 .. #31]) then
//                          Join[PP] := ' ';
//                    end;
//
//                    Inc(II);
//                  end;
//
//                  Inc(EE);
//                  Next;
//                end;
//              end;
//            end;
//            Close;
//
//            HeliosEasy_AttrAndJoin('v.' + TabDynVazbaDefinice_GUIDDynVazby,
//              LAttr, LJoin);
//
//            // --- VICENASOBNE VZTAHY ---
//            SQL.Clear;
//            SQL.Add(Format('SELECT ' + { 00 } 'v.' +
//              TabDynVazbaDefinice_GUIDDynVazbyTxt + ',' +
//              { 01 } 'v.' + TabDynVazbaDefinice_TabL + ',' +
//              { 02 } 'v.' + TabDynVazbaDefinice_NazevLP + ',' +
//              { 03 } 'v.' + TabDynVazbaDefinice_NazevLPSys + ',' +
//              { 04 } 'v.' + TabDynVazbaDefinice_TypLP + ',' +
//              { 05 } 'v.' + TabDynVazbaDefinice_AtrVazbaL + ',' +
//              { 06 } 'v.' + TabDynVazbaDefinice_TabP + ',' +
//              { 07 } 'v.' + TabDynVazbaDefinice_NazevPL + ',' +
//              { 08 } 'v.' + TabDynVazbaDefinice_NazevPLSys + ',' +
//              { 09 } 'v.' + TabDynVazbaDefinice_TypPL + ',' +
//              { 10 } 'v.' + TabDynVazbaDefinice_AtrVazbaP + ',' +
//              { 11 } '%s'#13 + 'FROM ' + TabDynVazbaDefinice + ' v'#13 + '%s',
//              [LAttr, LJoin]));
//
//            DetailStr := AnsiLowerCase(sqlCtiOznam(txtDetail));
//
//            Open;
//            if not IsEmpty then
//            begin
//              SetLength(LJeVazba, RecordCount);
//
//              if not Server.JeToHeliosEasy then
//              begin
//                // vsechno je povoleno
//                PP := RecordCount;
//                for II := Low(LJeVazba) to High(LJeVazba) do
//                  LJeVazba[II] := True;
//              end
//              else
//              begin
//                II := Low(LJeVazba);
//                PP := 0;
//                while not eof do
//                begin
//                  LJeVazba[II] := HeliosEasy_JeHashOK(Fields[0].AsString,
//                    TabDynVazbaDefinice,
//                    BinaryToHexaString(Fields[11].AsAnsiString));
//                  if LJeVazba[II] then
//                    Inc(PP); // pocet povolenych
//
//                  Inc(II);
//                  Next;
//                end;
//              end;
//
//              if PP > 0 then
//              begin
//                // vzdy je vazba T1 x T2
//                if DynPoleObecnychVazeb = nil then
//                begin
//                  SetLength(DynPoleObecnychVazeb, PP);
//                  II := Low(DynPoleObecnychVazeb);
//                end
//                else
//                begin
//                  II := High(DynPoleObecnychVazeb) + 1;
//                  SetLength(DynPoleObecnychVazeb,
//                    Length(DynPoleObecnychVazeb) + PP);
//                end;
//                FillChar(DynPoleObecnychVazeb[II], SizeOf(TVztahDef) * PP, 0);
//
//                SL := TStringList.Create;
//                try
//                  EE := Low(LJeVazba);
//                  First;
//                  while not eof do
//                  begin
//                    if LJeVazba[EE] then
//                    begin
//                      // k vyse dimenzovanemu poctu vazeb jsou vzdy 2 detailove (T1 x detail, T2 x detail)
//                      VV := 2;
//
//                      StejneTab := SameText(Fields[1].AsString { TabL } ,
//                        Fields[6].AsString { TabP } );
//                      DveVazby := StejneTab and
//                        (Fields[4].AsInteger { TypLP } <>
//                        TabObecnaVazba_Typ_Zadna) and
//                        (Fields[9].AsInteger { TypPL } <>
//                        TabObecnaVazba_Typ_Zadna);
//
//                      // pokud jsou to stejne tabulky, tak pripadne pridame dalsi vazbu
//                      // - pouze v pripade, ze je vazba z obou stran
//                      if DveVazby then
//                        Inc(VV);
//
//                      // zvetseni dimenze pole vazeb
//                      PP := High(DynPoleObecnychVazeb) + 1;
//                      SetLength(DynPoleObecnychVazeb,
//                        Length(DynPoleObecnychVazeb) + VV);
//                      FillChar(DynPoleObecnychVazeb[PP],
//                        SizeOf(TVztahDef) * VV, 0);
//
//                      for VV := 0 to 3 do
//                      begin
//                        Delej := False;
//
//                        case VV of
//                          0, 1: // T1 x T2, v pripade stejnych tabulek 2x
//                            begin
//                              Delej := (VV = 0) or ((VV = 1) and DveVazby);
//
//                              if Delej then
//                              begin
//                                with DynPoleObecnychVazeb[II] do
//                                begin
//                                  // leva strana
//                                  TabLStr := Fields[1].AsString { TabL };
//                                  TabL := TypTabulky(TabLStr);
//                                  if TabL = tZadna then
//                                    TabL := tx_ObecnyPrehled;
//                                  NazevLP := Fields[2].AsString { NazevLP };
//                                  // [RK 20.03.2009] GUID na veøejné texty externích øešení
//                                  if JeStringGUID(NazevLP) then
//                                    NazevLP := sqlCtiOznamGUID(NazevLP);
//                                  NazevLPSys :=
//                                    Fields[3].AsString { NazevLPSys };
//                                  case Fields[4].AsInteger { TypLP } of
//                                    TabObecnaVazba_Typ_1_1:
//                                      TypLP := tv11;
//                                    TabObecnaVazba_Typ_1_N:
//                                      TypLP := tv1N;
//                                  else
//                                    TypLP := tvZadny;
//                                  end;
//
//                                  // prava strana
//                                  TabPStr := Fields[6].AsString { TabP };
//                                  TabP := TypTabulky(TabPStr);
//                                  if TabP = tZadna then
//                                    TabP := tx_ObecnyPrehled;
//                                  NazevPL := Fields[7].AsString { NazevPL };
//                                  // [RK 20.03.2009] GUID na veøejné texty externích øešení
//                                  if JeStringGUID(NazevPL) then
//                                    NazevPL := sqlCtiOznamGUID(NazevPL);
//                                  NazevPLSys :=
//                                    Fields[8].AsString { NazevPLSys };
//                                  case Fields[9].AsInteger { TypPL } of
//                                    TabObecnaVazba_Typ_1_1:
//                                      TypPL := tv11;
//                                    TabObecnaVazba_Typ_1_N:
//                                      TypPL := tv1N;
//                                  else
//                                    TypPL := tvZadny;
//                                  end;
//
//                                  TabVazba := DynVazba_NazevVazebniTabulky
//                                    (Fields[0].AsString { GUID } );
//                                  Join := Format('EXISTS(SELECT*FROM %s WHERE ',
//                                    [TabVazba]);
//
//                                  LTabL := TabLStr;
//                                  LTabP := TabPStr;
//
//                                  if StejneTab then
//                                  begin
//                                    case VV of
//                                      0:
//                                        if Fields[4].AsInteger { TypLP } <> TabObecnaVazba_Typ_Zadna
//                                        then
//                                        begin
//                                        TypPL := tvZadny;
//                                        LTabL := sqlVazbaSpecifHT;
//                                        end
//                                        else
//                                        begin
//                                        TypLP := tvZadny;
//                                        LTabP := sqlVazbaSpecifHT;
//                                        end;
//
//                                      1:
//                                        if Fields[9].AsInteger { TypPL } <> TabObecnaVazba_Typ_Zadna
//                                        then
//                                        begin
//                                        TypLP := tvZadny;
//                                        LTabP := sqlVazbaSpecifHT;
//                                        end
//                                        else
//                                        begin
//                                        TypPL := tvZadny;
//                                        LTabL := sqlVazbaSpecifHT;
//                                        end
//                                    end;
//                                  end;
//
//                                  // leva tabulka
//                                  SL.CommaText :=
//                                    Fields[5].AsString { AtrVazbaL };
//                                  for PP := 0 to SL.Count - 1 do
//                                  begin
//                                    Attr := Trim(SL[PP]);
//                                    if PP > 0 then
//                                      Join := Join + ' AND ';
//                                    Join := Format('%s%s.%s=%s.%s%s',
//                                      [Join, LTabL, Attr, TabVazba,
//                                      DynVazba_PrefixVazebnihoAtributu
//                                      (1), Attr]);
//                                  end;
//
//                                  // prava tabulka
//                                  SL.CommaText :=
//                                    Fields[10].AsString { AtrVazbaP };
//                                  for PP := 0 to SL.Count - 1 do
//                                  begin
//                                    Attr := Trim(SL[PP]);
//                                    Join := Format('%s AND %s.%s=%s.%s%s',
//                                      [Join, LTabP, Attr, TabVazba,
//                                      DynVazba_PrefixVazebnihoAtributu
//                                      (2), Attr]);
//                                  end;
//
//                                  Join := Format('%s)', [Join]);
//                                end;
//                              end;
//                            end;
//
//                          2:
//                            begin // T1 x detail
//                              Delej := True;
//
//                              TabVazba := DynVazba_NazevVazebnihoPrehledu
//                                (Fields[0].AsString { GUID } );
//
//                              with DynPoleObecnychVazeb[II] do
//                              begin
//                                // leva strana
//                                TabLStr := Fields[1].AsString { TabL };
//                                TabL := TypTabulky(TabLStr);
//                                if TabL = tZadna then
//                                  TabL := tx_ObecnyPrehled;
//                                NazevLP := Fields[2].AsString { NazevLP };
//                                // [RK 20.03.2009] GUID na veøejné texty externích øešení
//                                if JeStringGUID(NazevLP) then
//                                  NazevLP := sqlCtiOznamGUID(NazevLP);
//                                NazevLP :=
//                                  Format('%s (%s)', [NazevLP, DetailStr]);
//                                NazevLPSys :=
//                                  Format('%s_Detail',
//                                  [Fields[3].AsString { NazevLPSys } ]);
//                                case Fields[4].AsInteger { TypLP } of
//                                  TabObecnaVazba_Typ_1_1:
//                                    TypLP := tv11;
//                                  TabObecnaVazba_Typ_1_N:
//                                    TypLP := tv1N;
//                                else
//                                  TypLP := tvZadny;
//                                end;
//
//                                // prava strana
//                                TabPStr := TabVazba;
//                                TabP := tx_ObecnyPrehled;
//                                TypPL := tv11;
//                                if Fields[7].AsString { NazevPL } <> '' then
//                                begin
//                                  NazevPL := Fields[7].AsString { NazevPL };
//                                  // [RK 20.03.2009] GUID na veøejné texty externích øešení
//                                  if JeStringGUID(NazevPL) then
//                                    NazevPL := sqlCtiOznamGUID(NazevPL);
//                                end
//                                else
//                                  NazevPL := VerejneJmenoTabulky(TabLStr);
//                                NazevPLSys := 'PLSys';
//
//                                Join := '';
//
//                                // leva tabulka
//                                SL.CommaText :=
//                                  Fields[5].AsString { AtrVazbaL };
//                                for PP := 0 to SL.Count - 1 do
//                                begin
//                                  Attr := Trim(SL[PP]);
//                                  if PP > 0 then
//                                    Join := Join + ' AND ';
//                                  Join := Format('%s%s.%s=%s.%s%s',
//                                    [Join, TabLStr, Attr, TabVazba,
//                                    DynVazba_PrefixVazebnihoAtributu(1), Attr]);
//                                end;
//
//                              end;
//                            end;
//
//                          3:
//                            begin // T2 x detail
//                              Delej := True;
//
//                              TabVazba := DynVazba_NazevVazebnihoPrehledu
//                                (Fields[0].AsString { GUID } );
//
//                              with DynPoleObecnychVazeb[II] do
//                              begin
//                                // prava strana
//                                TabPStr := Fields[6].AsString { TabP };
//                                TabP := TypTabulky(TabPStr);
//                                if TabP = tZadna then
//                                  TabP := tx_ObecnyPrehled;
//                                NazevPL := Fields[7].AsString { NazevPL };
//                                // [RK 20.03.2009] GUID na veøejné texty externích øešení
//                                if JeStringGUID(NazevPL) then
//                                  NazevPL := sqlCtiOznamGUID(NazevPL);
//                                NazevPL :=
//                                  Format('%s (%s)', [NazevPL, DetailStr]);
//                                NazevPLSys :=
//                                  Format('%s_Detail',
//                                  [Fields[8].AsString { NazevPLSys } ]);
//                                case Fields[9].AsInteger { TypPL } of
//                                  TabObecnaVazba_Typ_1_1:
//                                    TypPL := tv11;
//                                  TabObecnaVazba_Typ_1_N:
//                                    TypPL := tv1N;
//                                else
//                                  TypPL := tvZadny;
//                                end;
//
//                                // leva strana
//                                TabLStr := TabVazba;
//                                TabL := tx_ObecnyPrehled;
//                                TypLP := tv11;
//                                if Fields[2].AsString { NazevLP } <> '' then
//                                begin
//                                  NazevLP := Fields[2].AsString { NazevLP };
//                                  // [RK 20.03.2009] GUID na veøejné texty externích øešení
//                                  if JeStringGUID(NazevLP) then
//                                    NazevLP := sqlCtiOznamGUID(NazevLP);
//                                end
//                                else
//                                  NazevLP := VerejneJmenoTabulky(TabPStr);
//                                NazevLPSys := 'LPSys';
//
//                                Join := '';
//
//                                // prava tabulka
//                                SL.CommaText :=
//                                  Fields[10].AsString { AtrVazbaP };
//                                for PP := 0 to SL.Count - 1 do
//                                begin
//                                  Attr := Trim(SL[PP]);
//                                  if PP > 0 then
//                                    Join := Join + ' AND ';
//                                  Join := Format('%s%s.%s=%s.%s%s',
//                                    [Join, TabPStr, Attr, TabVazba,
//                                    DynVazba_PrefixVazebnihoAtributu(2), Attr]);
//                                end;
//                              end;
//                            end;
//                        end;
//
//                        if Delej then
//                        begin
//                          with DynPoleObecnychVazeb[II] do
//                          begin
//                            X := vObecnaTab_ObecnaTab;
//
//                            GUIDVazby :=
//                              Format('%s%d',
//                              [GUIDBezPomlcek(Fields[0].AsString), VV]);
//                            JmenoSys := sqlDefVazbaPrefix + GUIDVazby;
//                            VID := Vazby_VID_Ident_DynVazba; // dynamicka vazba
//                          end;
//
//                          Inc(II);
//                        end;
//                      end;
//                    end;
//
//                    Inc(EE);
//                    Next;
//                  end;
//                finally
//                  SL.Free;
//                end;
//              end;
//            end;
//          finally
//            Free;
//          end;
//      except
//{$IFDEF Ladit}
//        on E: Exception do
//          MessageBox(0, PChar(E.Message), 'LADIT', 0);
//{$ENDIF}
//      end;
//    end;

    // vytvoreni seznamu
    LCap := Integer(High(TVztah)) - Integer(Low(TVztah));
    if DynPoleObecnychVazeb <> nil then
      LCap := LCap + Length(DynPoleObecnychVazeb);

    SeznamVazebPodleVazeb := TSlovnikVazeb.CreateSpec(LCap, True);
    SeznamVazebPodleGUID := TSlovnikVazeb.CreateSpec(LCap, False);

    SeznamVazebPodleTabulek := TSpecialStringList.Create;
    with SeznamVazebPodleTabulek do
    begin
      Clear;
      Capacity := 2 * LCap;
      Sorted := True;
      Duplicates := dupAccept;
      // aby v ramci tabulek sedelo setrideni podle poradi v TVztah
      FFindType := sslfNext;
    end;

    for LVztah := Succ(Low(TVztah)) to High(TVztah) do
    begin
      // vObecnaTab_ObecnaTab je pouze fiktivni
      if LVztah = vObecnaTab_ObecnaTab then
        Continue;

      PVD := @DefiniceVztahu[LVztah];

      SeznamVazebPodleVazeb.PridejVazbu(PVD.JmenoSys, PVD);
      SeznamVazebPodleGUID.PridejVazbu(PVD.GUIDVazby, PVD);

      SeznamVazebPodleTabulek.AddObject(IntToStr(Integer(PVD.TabL)),
        TObject(PVD));
      if PVD.TabL <> PVD.TabP then // pripadna vazba sama na sebe pouze jednou
        SeznamVazebPodleTabulek.AddObject(IntToStr(Integer(PVD.TabP)),
          TObject(PVD));
    end;

    if Length(DynPoleObecnychVazeb) > 0 then
    begin
      PVD := @DynPoleObecnychVazeb[Low(DynPoleObecnychVazeb)];
      for II := Low(DynPoleObecnychVazeb) to High(DynPoleObecnychVazeb) do
      begin
        SeznamVazebPodleVazeb.PridejVazbu(PVD.JmenoSys, PVD);
        SeznamVazebPodleGUID.PridejVazbu(PVD.GUIDVazby, PVD);

        SeznamVazebPodleTabulek.AddObject(IntToStr(Integer(PVD.TabL)),
          TObject(PVD));
        if PVD.TabL <> PVD.TabP then // pripadna vazba sama na sebe pouze jednou
          SeznamVazebPodleTabulek.AddObject(IntToStr(Integer(PVD.TabP)),
            TObject(PVD));

        Inc(PVD);
      end;
    end;
  finally
    // pro jistotu - kdyby nahodou nekdo zapomnel :-))
    SeznamVazebPodleTabulek.FFindType := sslfFirst; { ! hledam prvni vyskyt ! }
  end;

end;

{ --------------------------------------------------------------------------- }

function KlicZakazanyVztah(aBrowse: TBrowse; aVztah: TVztah): string;
begin
  Result := Format('%d-%d', [Integer(aBrowse), Integer(aVztah)]);
end;

{ --------------------------------------------------------------------------- }

function JeVazbaZakazana(aBrowse: TBrowse; aVztah: TVztah): Boolean;
var
  II: Integer;
begin
  // [RK 12.07.2001] - zmena sekvencniho hledani na hledani pres QuickSort
  II := SeznamZakazanychVazeb.IndexOf(KlicZakazanyVztah(aBrowse, aVztah));
  if II = -1 then
    // nenalezeno => neni zakazano
    Result := False
  else
    // nalezeno => zakazano pouze pokud je zakaz platny (not Neplatne)
    Result := not DefiniceZakazanehoVztahu
      [TZakazanyVztah(SeznamZakazanychVazeb.Objects[II])].Neplatne;
end;

{ --------------------------------------------------------------------------- }

procedure NaplnZakazaneVazby;
var
  LZakVztah: TZakazanyVztah;
begin
  // seznam zakazanych vazeb
  if not Assigned(SeznamZakazanychVazeb) then
    SeznamZakazanychVazeb := TStringList.Create;

  with SeznamZakazanychVazeb do
  begin
    Clear;
    Capacity := Integer(High(TZakazanyVztah)) - Integer(Low(TZakazanyVztah));
    Sorted := True;
    Duplicates := dupIgnore;
  end;

  for LZakVztah := Succ(Low(TZakazanyVztah)) to High(TZakazanyVztah) do
    with DefiniceZakazanehoVztahu[LZakVztah] do
      SeznamZakazanychVazeb.AddObject(KlicZakazanyVztah(Browse, Vztah),
        TObject(LZakVztah));
end;

initialization

begin
  // sice se to cte v sqProc znovu, ale pro jistotu bez dynamickych vazeb
  RegenerujSeznamyVazeb(False);
  NaplnZakazaneVazby;
end;

finalization

begin
  FreeAndNil(SeznamVazebPodleVazeb);
  FreeAndNil(SeznamVazebPodleTabulek);
  FreeAndNil(SeznamVazebPodleGUID);
  FreeAndNil(SeznamZakazanychVazeb);
  DynPoleObecnychVazeb := nil;
end;

end.
