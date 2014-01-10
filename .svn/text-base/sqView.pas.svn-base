unit sqView;

interface

uses
  Classes, Forms, ddType, ddBrowse, sqBrowse;

type
  TDynamickaTabulkaItem = class
  protected
    FAtributy : array of TAtributTabulky;

  public
    FTabDef : TTabulkaDef;
    FBrowseDef: TVychoziNastaveniBrowse;
    FCislo : Integer;
    FMaximalizovat : Boolean;
    FPomocnicek: string;
    FSysFiltr: string;   // pro WHERESys - filtrace na obdobi, sklad apod.
    F_SELECT: string;    // implicitne zobrazene atributy
    F_ORDERBY: string;   // implicitne tridene atributy
    FZapnoutPosun: Boolean;
    FDefinice: string; // vyplnena pouze v pripade NE DB view

    constructor Create(Cislo, PocetAtributu: Integer;
                       Maximalizovat : Boolean;
                       const Pomocnicek: string);
    destructor Destroy; override;
  end;

  TSeznamDynamickychTabulek = class(TStringList)
  private
    procedure FreeObject(Index: Integer);
  public
    constructor Create;
    destructor Destroy; override;
    procedure Clear; override;
    procedure Delete(Index: Integer); override;
    procedure RemoveTable(const NazevSys: string);
  end;

function OP_DekodujKonverzi(const KonverzeVstup: string): string;

function __Create_OP_ViewSkript(const JmenoSys, Definice: string): string;
procedure __Drop_OP_View(const JmenoSys: string);
procedure __Drop_OP_Vysl;
procedure NastavObecnyPrehled(
    DynTab: TDynamickaTabulkaItem; aCislo: Integer; aWhereSys: string = '';
    {$IFnDEF Distribuce}aEditorClass: TFormClass = nil;{$ENDIF}
    const aEditorClassStr: string = '';
    {$IFnDEF Distribuce}aDMAkceClass: TDataModuleClass = nil;{$ENDIF}
    const aDMAkceClassStr: string = '');
function KompletniVytvoreniPrehledu(
    Cislo: Integer; const WhereSys: string; var Maximalizovat: Boolean;
    {$IFnDEF Distribuce}aEditorClass: TFormClass = nil;{$ENDIF}
    const aEditorClassStr: string = '';
    {$IFnDEF Distribuce}aDMAkceClass: TDataModuleClass = nil;{$ENDIF}
    const aDMAkceClassStr: string = ''): TDynamickaTabulkaItem;
procedure UklidObecnyPrehled(DynTab: TDynamickaTabulkaItem = nil);
procedure RefreshObecnyPrehled_v_ddBrowse(DynTab: TDynamickaTabulkaItem);
function Find_ObecPrehledSysNazBID(BrowseID: Integer): string;
function Find_ObecPrehledSysNazST(Skupina: Integer): string;
function Find_ObecPrehledBIDPodleSysNaz(const NazevSys: string): Integer;
function MamPravoNaObecPrehled(BrowseID: Integer; RaiseError: Boolean = True): Boolean;
function GenerujDynamickouTabulku(const aNazevSys: string;
  Regenerovat: Boolean = False; HlasitEasy: Boolean = False): TDynamickaTabulkaItem;
procedure ClearSeznamDynamickychTabulek;
procedure ZrusZeSeznamuDynamickychTabulek(const NazevSys: string);
function VratDynamickouTabulku(const aNazevSys: string): TDynamickaTabulkaItem;
function DefiniceTabulky(const NazevSys: string): string;

const
  SeznamDynamickychTabulek: TSeznamDynamickychTabulek = nil;

{ =========================================================================== }

implementation

uses
  SysUtils, Variants, Contnrs, DB, ddMain, sqProc, Pom,
  sqString, sqEasyHash,
  {$IFnDEF Distribuce} dmObecnyPrehled_X, {$ENDIF}
  dtObecnyPrehled, dtUzivAtr;

type
  TObecnyPrehledMemoryItem = class
    TD_Mem: TTabulkaDef;
    NB_Mem: TVychoziNastaveniBrowse;
    DTI: TDynamickaTabulkaItem;
  end;

const
  SeznamMemoryItems: TObjectList = nil;

constructor TDynamickaTabulkaItem.Create(Cislo, PocetAtributu: Integer;
                                         Maximalizovat : Boolean;
                                         const Pomocnicek: string);
begin
  inherited Create;

  FCislo := Cislo;
  FMaximalizovat := Maximalizovat;
  FPomocnicek := Pomocnicek;
  if PocetAtributu > 0 then
  begin
    SetLength(FAtributy, PocetAtributu);
    FillChar(FAtributy[0], Length(FAtributy), 0);
  end;
end;

{ --------------------------------------------------------------------------- }

destructor TDynamickaTabulkaItem.Destroy;
begin
  FAtributy := nil;
  inherited;
end;

{ =========================================================================== }

constructor TSeznamDynamickychTabulek.Create;
begin
  inherited Create;
  Sorted := True;
end;

{ --------------------------------------------------------------------------- }

destructor TSeznamDynamickychTabulek.Destroy;
begin
  Clear;
  inherited;
end;

{ --------------------------------------------------------------------------- }

procedure TSeznamDynamickychTabulek.FreeObject(Index: Integer);
begin
  Objects[Index].Free;
  Objects[Index] := nil;
end;

{ --------------------------------------------------------------------------- }

procedure TSeznamDynamickychTabulek.Clear;
  var
    II: Integer;
begin
  for II := 0 to Count-1 do FreeObject(II);
  inherited;
end;

{ --------------------------------------------------------------------------- }

procedure TSeznamDynamickychTabulek.Delete(Index: Integer);
begin
  FreeObject(Index);
  inherited;
end;

{ --------------------------------------------------------------------------- }

procedure TSeznamDynamickychTabulek.RemoveTable(const NazevSys: string);
  var
    II: Integer;
begin
  II := IndexOf(NazevSys);
  if II <> -1 then Delete(II);
end;

{ =========================================================================== }

const
  // [RK 07.09.2006] verze s OBJECT_ID
  ExistsViewStr = 'IF OBJECT_ID(N''dbo.%s'', ''V'') IS NOT NULL';
//  ExistsViewStr = 'IF EXISTS (SELECT TABLE_NAME FROM INFORMATION_SCHEMA.VIEWS' +
//                  ' WHERE TABLE_NAME = ''%0:s'' AND TABLE_SCHEMA = ''dbo'')';

procedure __Drop_OP_View(const JmenoSys: string);
begin
  Server.ExecSQL(Format(ExistsViewStr + ' DROP VIEW dbo.%0:s', [JmenoSys]));
end;

{ --------------------------------------------------------------------------- }

function __Create_OP_ViewSkript(const JmenoSys, Definice: string): string;
begin
  Result := Format('CREATE VIEW dbo.%s AS %s', [JmenoSys, Definice]);
end;

{ --------------------------------------------------------------------------- }

procedure __Drop_OP_Vysl;
begin
  Server.ExecSQL('IF OBJECT_ID(N''tempdb..' + TabObecnyPrehledVysl + ''') IS NOT NULL'#13+
                 'BEGIN'#13+
                   'TRUNCATE TABLE ' + TabObecnyPrehledVysl + #13 +
                   'DROP TABLE ' + TabObecnyPrehledVysl + #13 +
                 'END');
end;

{ --------------------------------------------------------------------------- }

function OP_DekodujKonverzi(const KonverzeVstup: string): string;
  var
    PA : PAtributTabulky;
begin
  if Pos('=', KonverzeVstup) <> 0 then
    Result := KonverzeVstup
  else
    begin
      PA := Atribut(KonverzeVstup);
      if Assigned(PA) then
        Result := PA.Konverze
      else
        Result := ''; // pokud nebylo = a nenasel jsem atribut, tak je nejaka chyba...
    end;
end;

{ --------------------------------------------------------------------------- }

function VratDynamickouTabulku(const aNazevSys: string): TDynamickaTabulkaItem;
  var
    II: Integer;
begin
  II := SeznamDynamickychTabulek.IndexOf(aNazevSys);
  if II = -1 then
    Result := nil
  else
    Result := TDynamickaTabulkaItem(SeznamDynamickychTabulek.Objects[II]);
end;

{ --------------------------------------------------------------------------- }

function DefiniceTabulky(const NazevSys: string): string;
  var
    DynTab: TDynamickaTabulkaItem;
begin
  if JeObecnyPrehled(NazevSys) then
  begin
    DynTab := GenerujDynamickouTabulku(NazevSys);
    if Assigned(DynTab) and (DynTab.FDefinice <> '') then
    begin
      Result := DynTab.FDefinice;
      Exit;
    end;
  end;

  Result := '';
end;

{ --------------------------------------------------------------------------- }

function GenerujDynamickouTabulku(const aNazevSys: string;
  Regenerovat: Boolean = False; HlasitEasy: Boolean = False): TDynamickaTabulkaItem;
  var
    {LDefinice, LDefAttrs,} LNazev{, S, LMaska}: string;
//    SLDefAttrs, SLZobr, SLTrid: TStringList;
//    Q: TQueryHe;
//    QUziv: TDataSet;
    II{, XX}: Integer;
    UnikatniKlic: string;
//    COLUMN_NAME, TYPE_NAME: string;
//    PRECISION, SCALE, NULLABLE: Integer;
//    LDefAtr: T_OP_DefiniceAtributu;
//    LCislo: Integer;
//    PA, PA_H: PAtributTabulky;
//    LMaximalizovat, LJeToDBView, OK: Boolean;
//    LPomocnicek: string;
    LSysFiltr{, LTabProSloupce}: string;
//    LAttrAnoNe: array of Boolean;
//    LAttr, LJoin: string;
//    JeStavovySloupec: Boolean;
begin
  // hledej v jiz vygenerovanych
  II := SeznamDynamickychTabulek.IndexOf(aNazevSys);
  if II <> -1 then
  begin
    if Regenerovat then
      SeznamDynamickychTabulek.Delete(II)
    else
      begin
        Result := TDynamickaTabulkaItem(SeznamDynamickychTabulek.Objects[II]);
        Exit;
      end;
  end;

  Result := nil;
  LSysFiltr := '';

//  SLDefAttrs := TStringList.Create;
//  SLZobr := TStringList.Create;
//  SLTrid := TStringList.Create;

//  QUziv := QueryProUzivatelskeAtributy(
//             Format(
//               TabUzivAtr + '.' + TabUzivAtr_NazevTabulkySys + '=%s'#13+
//               'AND (' + TabUzivAtr + '.' + TabUzivAtr_VerejnyAtr + '=%s OR ' + TabUzivAtr + '.' + TabUzivAtr_VerejnyAtr + '=%s)'#13+
//               'AND ' + TabUzivAtr + '.' + TabUzivAtr_Externi + '=0',
//               [NQuotedStr(aNazevSys),
//                NQuotedStr(TabUzivAtr_VerejnyAtr_VAno_EAno),
//                NQuotedStr(TabUzivAtr_VerejnyAtr_VAno_ENe)]));

  // systemova filtrace, tabulka pro TEXTy
  with Result do
  begin
    FSysFiltr := LSysFiltr;
//    if not LJeToDBView then
//      FDefinice := Format('(%s)', [LDefinice]);
  end;

  with Result.FTabDef do
  begin
    Jmeno := LNazev;
    JmenoSys := aNazevSys;
    PocetAtributu := Length(Result.FAtributy);
    Atributy := @Result.FAtributy[Low(Result.FAtributy)];
    _FastestUniqueAttr := UnikatniKlic;
  end;

  SeznamDynamickychTabulek.AddObject(aNazevSys, Result);

//  if JeStavovySloupec then
//    ProjdiVsechnyStavoveSloupce(@Result.FTabDef);
end;

{ --------------------------------------------------------------------------- }

procedure UklidObecnyPrehled(DynTab: TDynamickaTabulkaItem = nil);
  var
    OPI, OPItem: TObecnyPrehledMemoryItem;
    II: Integer;
begin
  if SeznamMemoryItems.Count = 0 then Exit;

  if not Assigned(DynTab) then
    OPItem := TObecnyPrehledMemoryItem(SeznamMemoryItems.Items[SeznamMemoryItems.Count-1])
  else
    begin
      // [RK 09.01.2009] presne hledani kvuli Dataskopu
      OPItem := nil;
      for II := 0 to SeznamMemoryItems.Count-1 do
      begin
        OPI := TObecnyPrehledMemoryItem(SeznamMemoryItems.Items[II]);
        if OPI.DTI = DynTab then
        begin
          OPItem := OPI;
          Break;
        end;
      end;
      if not Assigned(OPItem) then Exit;
    end;

  II := SeznamMemoryItems.IndexOf(OPItem);
  if II = SeznamMemoryItems.Count-1 then
    begin
      // pokud je posledni v seznamu, tak upravime ddBrowse
      // vratime zpet puvodni nastaveni tabulky a prehledu
      GetTabulkaDef(tx_ObecnyPrehled)^ := OPItem.TD_Mem;
      SeznamVychozichNastaveniBrowse[bx_ObecnyPrehled] := OPItem.NB_Mem;
    end
  else
    // musime zachovat retez pameti
    with TObecnyPrehledMemoryItem(SeznamMemoryItems.Items[II+1]) do
    begin
      TD_Mem := OPItem.TD_Mem;
      NB_Mem := OPItem.NB_Mem;
    end;

  // zrusime ze seznamu (Free objectu se udela automaticky samo)
  SeznamMemoryItems.Delete(II);
end;

{ --------------------------------------------------------------------------- }

procedure RefreshObecnyPrehled_v_ddBrowse(DynTab: TDynamickaTabulkaItem);
begin
  if Assigned(DynTab) then
  begin
    GetTabulkaDef(tx_ObecnyPrehled)^ := DynTab.FTabDef;
    SeznamVychozichNastaveniBrowse[bx_ObecnyPrehled] := DynTab.FBrowseDef;
  end;
end;

{ --------------------------------------------------------------------------- }

procedure NastavObecnyPrehled(
    DynTab: TDynamickaTabulkaItem; aCislo: Integer; aWhereSys: string = '';
    {$IFnDEF Distribuce}aEditorClass: TFormClass = nil;{$ENDIF}
    const aEditorClassStr: string = '';
    {$IFnDEF Distribuce}aDMAkceClass: TDataModuleClass = nil;{$ENDIF}
    const aDMAkceClassStr: string = '');
  var
    OPItem: TObecnyPrehledMemoryItem;
    LT: PTabulkaDef;
    II: Integer;
begin
  if not Assigned(DynTab) then Exit;

  // vygeneruj pamet
  OPItem := TObecnyPrehledMemoryItem.Create;
  OPItem.DTI := DynTab;
  SeznamMemoryItems.Add(OPItem);

  // docasna tabulka
  LT := GetTabulkaDef(tx_ObecnyPrehled);

  // zapamatuj si puvodni nastaveni - kvuli zretezenemu volani v externich akcich
  OPItem.TD_Mem := LT^;

  // nastaveni docasne tabulky z tabulky aktualni
  LT^ := DynTab.FTabDef;

  // [RK 13.04.2005] vsechny ridici znaky v podmince vymazeme
  for II := 1 to Length(aWhereSys) do
    if CharInSet(aWhereSys[II], [#0..#31]) then aWhereSys[II] := ' ';

  if aWhereSys = '' then
    aWhereSys := DynTab.FSysFiltr
  else
    if DynTab.FSysFiltr <> '' then
      aWhereSys := Format('%s'#13'%s', [DynTab.FSysFiltr, aWhereSys]);

  // zapamatuj si puvodni nastaveni - kvuli zretezenemu volani v externich akcich
  OPItem.NB_Mem := SeznamVychozichNastaveniBrowse[bx_ObecnyPrehled];
  with SeznamVychozichNastaveniBrowse[bx_ObecnyPrehled] do
  begin
    Jmeno          := LT.Jmeno;
    BID            := x_ObecnePohledy_BID_Base + aCislo; // pouzito i v eiObecnyPrehled
    Skupina_Tisky  := x_ObecnePohledy_SkupinaTisky_Base + aCislo;
    SELECT         := DynTab.F_SELECT;
    WHEREsys       := aWhereSys;
    ORDERBY        := DynTab.F_ORDERBY;

    {$IFnDEF Distribuce}
    EditorClass := aEditorClass;
    {$ENDIF  Distribuce}
    EditorClassStr := aEditorClassStr;

    if aDMAkceClassStr = '' then
      begin
        {$IFnDEF Distribuce}
        DMAkceClass := TDM_ObecnyPrehled_X;
        {$ENDIF}
        DMAkceClassStr := 'TDM_ObecnyPrehled_X';
      end
    else
      begin
        {$IFnDEF Distribuce}
        DMAkceClass := aDMAkceClass;
        {$ENDIF}
        DMAkceClassStr := aDMAkceClassStr;
      end;
  end;
  DynTab.FBrowseDef := SeznamVychozichNastaveniBrowse[bx_ObecnyPrehled];
end;

{ --------------------------------------------------------------------------- }

function KompletniVytvoreniPrehledu(
    Cislo: Integer; const WhereSys: string; var Maximalizovat: Boolean;
    {$IFnDEF Distribuce}aEditorClass: TFormClass = nil;{$ENDIF}
    const aEditorClassStr: string = '';
    {$IFnDEF Distribuce}aDMAkceClass: TDataModuleClass = nil;{$ENDIF}
    const aDMAkceClassStr: string = ''): TDynamickaTabulkaItem;
  var
    LNazevSys: string;
begin
  // nacti tabulku
  Result := GenerujDynamickouTabulku(LNazevSys, False, True{!});
  if not Assigned(Result) then Exit;

  // nastav prehled a pamet
  NastavObecnyPrehled(Result, Cislo, WhereSys,
    {$IFnDEF Distribuce}aEditorClass,{$ENDIF Distribuce}aEditorClassStr,
    {$IFnDEF Distribuce}aDMAkceClass,{$ENDIF Distribuce}aDMAkceClassStr);

  Maximalizovat := Result.FMaximalizovat;
end;

{ --------------------------------------------------------------------------- }

function _NactiSysNazev(Cislo: Integer): string;
begin
  Result := '';
end;

{ --------------------------------------------------------------------------- }

function Find_ObecPrehledSysNazBID(BrowseID: Integer): string;
begin
  if JeObecnyPrehled(BrowseID) then
    Result := _NactiSysNazev(BrowseID - x_ObecnePohledy_BID_Base)
  else
    Result := '';
end;

{ --------------------------------------------------------------------------- }

function Find_ObecPrehledBIDPodleSysNaz(const NazevSys: string): Integer;
begin
  Result := 0;
end;

{ --------------------------------------------------------------------------- }

function Find_ObecPrehledSysNazST(Skupina: Integer): string;
begin
  if (Skupina >= x_ObecnePohledy_SkupinaTisky_Base) {and
     (Skupina <= x_ObecnePohledy_SkupinaTisky_Top)} then
    Result := _NactiSysNazev(Skupina - x_ObecnePohledy_SkupinaTisky_Base)
  else
    Result := '';
end;

{ --------------------------------------------------------------------------- }

function MamPravoNaObecPrehled(BrowseID: Integer; RaiseError: Boolean = True): Boolean;
begin
    Result := True
end;

{ --------------------------------------------------------------------------- }

procedure ClearSeznamDynamickychTabulek;
begin
  if Assigned(SeznamDynamickychTabulek) then
    SeznamDynamickychTabulek.Clear;
end;


procedure ZrusZeSeznamuDynamickychTabulek(const NazevSys: string);
begin
  if Assigned(SeznamDynamickychTabulek) then
    SeznamDynamickychTabulek.RemoveTable(NazevSys);
end;

end.

