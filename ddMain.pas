unit ddMain;

interface

uses
  Classes, SysUtils, DB, ddType;
function TypTabulky(const AJmenoSysTabulky : String) : TTabulka;
function JmenoTabulky(iTabulka : TTabulka) : String; // vrati systemove jmeno tabulky
function JmenoTabulkyEng(iTabulka : TTabulka) : String; // vrati systemove jmeno tabulky v anglictine pro Nemecko
function VerejneJmenoTabulky(iTabulka: TTabulka): String; overload;
function VerejneJmenoTabulky(const AJmenoSys: String): String; overload;
function VerejneJmenoPrehledu(BID: Integer): String;
function VratView(iTabulka: TTabulka): String; // vraci definici view
function Constraint2VerejneJmenoTabulky(const AConstraint : String) : String;
function JeObecnyPrehled(const NazevSys: String): Boolean; overload;
function JeObecnyPrehled(BID: Integer): Boolean; overload;
function BrowseDPSN2BrowseID(const BrowseDPSN: String): Integer;
function BrowseID2BrowseDPSN(BID: Integer): String;
function BID2Browse(BID : Integer): TBrowse;
function BrowseDPSN2Browse(const BrowseDPSN: String): TBrowse;
function BrowseDPSN2JmenoTabulky(const BrowseDPSN: String): string;
function OdstranPrefixTabulky(const AJmenoSysAtributu  : String) : String;
type
  TJakNaplnit =        { n Strings[n]                               Objects[n] }
    (jnJenJmena,       { 0 'Tab.ID'                                 nil        }
     jnJenJmenaBezPocitanychABinary, // bez pocitanych a typu a BINARY ([15.04.2002 TZ] drive byl i typ IMAGE - zruseno kvuli CEA - Ciselnik obrazku)
     jnJenJmenaBezPocitanych_BezJmenaTabulky,
     jnProCreateTable, { 0 'ID INT IDENTITY(1,1) NULL DEFAULT(1)'   nil        }
     jnSeznam,         { 0 'ID'                                     'INT',         'IDENTITY(1,1)', 'NULL',     'DEFAULT(1)', '...popis...' }
     jnJenJmenaBezPocitanych_BezIdentity_BezJmenaTabulky,
     jnJenJmenaSOdkazem, // [RK 19.01.2001] - kvuli zrychleni Nastav
     jnJenJmenaSOdkazemBezTabulky, // [RK 11.01.2002] - kvuli zrychleni tisku
     jnJenSumovatelneSOdkazem, // [RK 29.02.2008] kvuli grafu
     jnJenJmenaSOdkazemBezTabulkyEng, // [PFA 22.3.2011] - pro nemecke formulare. Vraci jen atributy, ktere maji vyplneno JmenoSysEng
     jnJenJmenaSOdkazemBezTabulkyEngBezAlias, // [PFA 07.04.2011] - pro nemecke formulare. Vraci jen atributy bez aliasu, ktere maji vyplneno JmenoSysEng - pro optimalizator tisku
     jnJenJmenaSOdkazemProNazvySkupin,
     jnJenJmenaSOdkazemProStavyDokladu
     ,jnJenJmenaBezPocitanych_BezIdentity
    );

  TTypPocitanehoAtributu = (tpaNeniPocitany, tpaDatabaze,
    tpaProgramatorskyDefinovany, tpaUzivatelskyDefinovany, tpaExterniAtribut);

procedure AtributyTabulky(TD                  : PTabulkaDef;
                          ASL                 : TStrings;
                          AVcetneSystemovych  : Boolean;          // Systemovy = not Verejny
                          AJakNaplnit         : TJakNaplnit;
                          AVcetneDefinovanych : Boolean = False   // vËetnÏ uûivatelsky i "program·torsky" definovan˝ch
                         ); overload;

procedure AtributyTabulky(ATabulka            : TTabulka;
                          ASL                 : TStrings;
                          AVcetneSystemovych  : Boolean;          // Systemovy = not Verejny
                          AJakNaplnit         : TJakNaplnit;
                          AVcetneDefinovanych : Boolean = False   // vËetnÏ uûivatelsky i "program·torsky" definovan˝ch
                         ); overload;

procedure AtributyTabulky(const ATabulkaStr   : String;
                          ASL                 : TStrings;
                          AVcetneSystemovych  : Boolean;          // Systemovy = not Verejny
                          AJakNaplnit         : TJakNaplnit;
                          AVcetneDefinovanych : Boolean = False   // vËetnÏ uûivatelsky i "program·torsky" definovan˝ch
                         ); overload;

function MaTabulkaAtribut(      ATabulka                 : TTabulka;
                          const AJmenoAtributuBezPrefixu : String) : boolean;

procedure PridejVyzadovaneAtributy(ATabulka : TTabulka;
                                   ASL      : TStrings);

function TypAtributu(const AJmenoSysAtributu: String): TTypAtributu;
function TypPocitanehoAtributu(PA: PAtributTabulky): TTypPocitanehoAtributu;

function Atribut(const AJmenoSysAtributu : String): PAtributTabulky; overload;

function Atribut(ATabulka : TTabulka;
                 const AJmenoSysAtributuBezTabulky : String): PAtributTabulky; overload;

function AtributV(const AHlavniTabulkaStr: string;
                  const AJmenoSysAtributuSVazbou : String;
                        ATabName       : PString = nil;
                        AVazbaName     : PString = nil): PAtributTabulky; overload;
function AtributV(      AHlavniTabulka : TTabulka;
                  const AJmenoSysAtributuSVazbou : String;
                        ATabName       : PString = nil;
                        AVazbaName     : PString = nil): PAtributTabulky; overload;

function JeAtributSoucastiPK_UQ(TD: PTabulkaDef; PA: PAtributTabulky; var SlozeniKlice: String): Boolean;
function SkupinaAtributu(ATypAtributu : TTypAtributu) : TSkupinaAtributu;
function MuzeBytSumovat(aTypAtributu : TTypAtributu) : Boolean;
function VerejneJmenoAtributu(PA : PAtributTabulky; const AJmenoSys : String = '') : String; overload;
function VerejneJmenoAtributu(const AJmenoSys : String) : String; overload;
function VerejneJmenoAtributu(ATabulka : TTabulka; const AJmenoSysAtributuBezTabulky : String) : String; overload;
function Atribut2Tabulka(const AAtribut : String) : String;
function PAtributTabulky2JmenoSys(P : PAtributTabulky) : String;
function TabulkaPocetTriggers(const AJmenoSys : String) : integer;
function CreateTableTriggerString(const AJmenoSys : String; I : integer) : String;
function CreateTableFKString(ATabulka : TTabulka) : String; overload;
function CreateTableFKString(const AJmenoSys : String) : String; overload;
function CreateIndexString(ATabulka : TTabulka) : String; overload;
function CreateIndexString(const AJmenoSys : String) : String; overload;
function CreateTableString(const AJmenoSys : String; ADropIfExists : Boolean = False) : String; overload;
function CreateTableEURString(ATabulka : TTabulka) : String;
function DropTableString(ATabulka : TTabulka) : String;
function DeleteTableString(ATabulka : TTabulka) : String;
function NastavPravaSkript(NazevObjektu: string; const Typ: string): string;
function Podminka2SysStr(APodminka : TPodminka) : String;
function Podminka2VerejnyStr(APodminka    : TPodminka; ATypAtributu : TTypAtributu) : String;
function SysStr2Podminka(const ASysStr : String) : TPodminka;
function UpravPodminkuLike(APodminka: TPodminka; const AHodnota: string): String;
function GetTabulkaDef(ATabulka : TTabulka): PTabulkaDef; overload;
function GetTabulkaDef(const ATabulkaStr: String): PTabulkaDef; overload;
procedure ConstraintyTabulky(ITabulka : TTabulka;
                             OPkUq_SL : TStrings; { primary keys/unique }
                             OCh_SL   : TStrings; { checks              }
                             OFk_SL   : TStrings; { foreign keys        }
                             OIX_SL   : TStrings;
                             OIC_SL   : TStrings
                             );

procedure TriggersTabulky(ITabulka  : TTabulka;
                          OTriggers : TStrings
                          );
procedure AnalyzujORDERBY(var ioJedenOrderBy : String; {vstup=' TabX.AtrY DESC ', vystup='TabX.AtrY'}
                          out oTyp           : boolean {False-DESC, True-ASC});
function VratNazevUProc(SLuproc: TStringList; P: PInteger = nil): string;
function VratNazevUzivFunkce(SLfce: TStringList; P: PInteger = nil): string;
function VratTabulkaTrigger(SLtrigger : TStringList; AUpperCase : Boolean = True) : string;
function VratNazevTrigger(SLtrigger : TStringList; AUpperCase : Boolean = True) : string;
function VratNazevView(S: string; P: PInteger = nil): string;

const
  TxPridat         = 0;
  TxOdebrat        = 1;
  TxPridatKonec    = 2;
  TxOdebratZacatek = 3;

type
  Tx = class(TObject)
  private
    FTabulka            : TTabulka;
    FPole               : TBits;
    FSL_Tab             : TStringList; { zde je "strom" tabulek            }
    FSL_UProc           : TStringList; { zde je "strom" ulozenych procedur }
    PridavaneDefaultySL : TStringList;
    OdTabulka           : TTabulka;    { od ktere tabulky provadet kontroly }
    DoTabulka           : TTabulka;    { do ktere tabulky provadet kontroly }
    IndexZacatek        : integer;     { index pouzivane pro pridavani od zacatku - jedna se o ofset od zacatku }
    IndexKonec          : integer;     { index pouzivane pro pridavani od konce - jedna se o ofset od konce }
    ZmenaDefaultySL     : TStringList; { seznam defaultu atributu, u kterych s emeni typ a je potreba je pregenerovat v DB }

    FQuery: TObject; // aby se nemenilo uses (sqProc a DB)
    FWhereTab: String;
    FPomSL: TStringList;
  public
    XSL: TStringList; { v tomto poradi je provadeno }
  private
    procedure RozdelConstraint(iJmenoConstraint : string; var oTyp, oJmenoTabulky, oAtributy : string);
    procedure OdeberConstraint(AJmenoConstraint, AJmenoTabulka: string);
    function JednoznacnyText (SLtext : TStringList) : string;
    function DejADDCOLUMNstring (ATabulka : TTabulka; PA : PAtributTabulky) : string;
    function JeVyrazStejny_Yukon(Adb, Azdr: String) : Boolean;
  public
    constructor Create(AOdTabulka, ADoTabulka: TTabulka);
    destructor Destroy; override;

    procedure SetTabulka(ATabulka : TTabulka);
    procedure SetWhereTab(const aWhereTab: String);

    procedure Kontrola(const COLUMN_NAME            : String;
                       const DATA_TYPE              : String;
                       const AutoIncrement          : Boolean;//ShortString;
                             IS_NULLABLE            : Boolean;
                       const CHARACTER_OCTET_LENGTH : String;
                       const NUMERIC_PRECISION      : String;
                       const NUMERIC_SCALE          : String;
                       const COMPUTED               : String{;
                       const COLUMN_DEFAULT         : String}
                      );

    procedure PridejDoSL_Tab(const ATab, AAtr, AText, ASQL : String; Pridat : Integer);
    { Pridava do FSL_Tab a XSL }

    procedure PridejDoSL_UProc(const AUProc, AText, ASQL: String);
    { Pridava do FSL_UProc a XSL }

    function Konec(    AAktServer    : String;
                       AAktDB        : String;
                       APovolProvest : Boolean;
                   var ABylAltB      : Boolean;
                   var ioReindex     : TTristavovyBool{(vDefault, vFalse, vTrue)}) : Boolean;
    { vraci zmenovy SQL skript }

    procedure KontrolujOmezeni_PK_UQ(SLdatabaze : TStringList);
    procedure KontrolujOmezeni_FK(SLdatabaze : TStringList);
    procedure KontrolujDefaultyChecky(SLdatabaze : TStringList);
    procedure KontrolujView(SLdatabaze : TStringList);
    procedure KontrolujUlozeneProcedury(SLdatabaze : TStringList);
    procedure KontrolujUzivFunkce(SLdatabaze : TStringList);
    procedure KontrolujTriggers(SLdatabaze : TStringList);
    procedure KontrolujIndex(SLdatabaze : TStringList);

    // volano v sqProc
    procedure DoKontrolaAtributuTabulek;
    procedure DoKontrolaOmezeni_PK_UQ;
    procedure DoKontrolaOmezeni_CK_DF;
    procedure DoKontrolaOmezeni_FK;
    procedure DoKontrolaView;
    procedure DoKontrolaUlozenychProcedur;
    procedure DoKontrolaUzivFunkci;
    procedure DoKontrolaTriggeru;
    procedure DoKontrolaIndexu(UseWhereTab: Boolean);
  end;

  TPomConstraintObject = class
     S, TableName  : string;
     Blokovano : Integer;
     constructor Create (sS : string; sBlokovano : integer; sTableName : string);
     function Get : string;
     function GetBlokovano : integer;
     function GetTableName : string;
   end;

  TPomConstraintFKObject = class
     FTabulka  : string;
     FAtributy : string;
     FBlokovano : Integer;
     FVlastniTabulka: string;
     constructor Create(const ATabulka, AAtributy: String; ABlokovano: Integer; const AVlastniTabulka: string);
     function GetTabulka : string;
     function GetAtributy : string;
     function GetBlokovano : integer;
     function GetVlastniTabulka : string;
   end;

  TIndexKontrola = class
    Jmeno, Atributy : string;
    Clustered : Boolean;
    constructor Create (AJmeno, AAtributy : string; AClustered : Boolean);
  end;

type
  TPodtypAtributu =
    (ptaZadny,
     ptaIdentity   { property IDENTITY   - pro typy [TINY|SMALL]INT | NUMERIC(1..28,0) }
//   ptaRowGUIDCol { property ROWGUIDCOL - pro typ UniqueIdentifier                    } ZRUSENO [TZ]
    );

  TDefTypu = packed record
      {$IFDEF Ladit}
       _X : TTypAtributu;  { pro self-check pri startu na Ladit }
      {$ENDIF}

      T : String;          { zakl. typ (INT/CHAR/VARCHAR/NUMERIC...)                       }
      X : TPodtypAtributu; { podtyp    (pro [TINY|SMALL]INT - IDENTITY)                    }
      P : Byte;            { precision (presnost pro NUMERIC/seed pro IDENTITY)            }
      S : Byte;            { scale     (poc. des. mist pro NUMERIC/increment pro IDENTITY) }
      {$IFDEF SPopisem}
       Popis : String; { popis pro generovani dokumentace                              }
      {$ENDIF}
    end;

function Str2TypAttr(const NazevTypu: String; P,S: Integer): TTypAtributu;
function Typ2Str(ATypAtributu : TTypAtributu) : TDefTypu;
function Podtyp2Str(APodtypAtributu : TPodtypAtributu) : String;
function UzivatelskyTyp(ATypAtributu : TUzivatelskyTypAtributu) : TUzivatelskeAtributyDef;
function Typ2DBTyp(PA: PAtributTabulky; VcetnePodtypu: Boolean = True): String;
function DefAtrStr2TypAtributu(const ATypAtrStr : String): TTypAtributu;

function UniqueKey(iTabulka: TTabulka): String; overload;
function UniqueKey(const aJmenoSysTabulky: String): String; overload;
{ vrati unikatni klic (nejrychlejsi, pokud je jich vic) pro tabulku }
{   pokud je vic atributu - jsou oddelene carkou (!)                }
{   Priklad : 'SkupZbo,RegCis'                                      }

function ParametrizovanejUK(ATabulka: TTabulka; AUniqueKey: string = ''): String;
{ vrati parametrizovanou klauzuli WHERE pro unikatni klic           }
{   pokud je vic atributu - jsou oddelene CRLF                      }
{   Priklad : 'SkupZbo=:SkupZbo AND'#13#10+                         }
{             'RegCis=:RegCis                                       }

function VerejneJmenoVazby(      aRefTabulka : TTabulka; // vztazna tabulka
                                 aJmenoSys   : String;   // muze byt 'Vazba' nebo 'Vazba.Atribut'
                                 aTakeAttr   : Boolean = False; // ma se vratit i jmeno atributu ?
                           const aRefTabulkaStr: String = '' // vztazna tabulka - kvuli def.prehledum
                          ) : String;

procedure PridejRenameAtr(const TabulkaSpec: string = '');
procedure PridejUzivatelskeAtributy;
function QueryProUzivatelskeAtributy(aWhere: String): TDataSet;
function JeUzivAttrPovolen(Q: TDataSet): Boolean;
procedure NastavUzivatelskyAtribut(Q: TDataSet; PA: PAtributTabulky);
procedure ProjdiVsechnyStavoveSloupce(TD: PTabulkaDef);
function BudeRozpadNaEuro: Boolean;  //vr·tÌ True, v p¯ÌpadÏ, ûe se sloupce budou rozpadat
procedure PridejRozpadNaEuro;
procedure RegenerujRozpadNaEuro(aTabulka : TTabulka);  //pouûÌv· se v p¯ÌpadÏ, ûe mÏnÌm dynamicky Visible nÏkter˝ch sloupc˘ v dt-Ëku a pot¯ebuji, aby na nÏm z·vislÈ eurosloupce "zdÏdili" tuto viditelnost
(*
{$IFDEF Distribuce}
 function IsHeDDLoaded: Boolean;
 procedure LoadHeDD;
 procedure UnloadHeDD;
{$ENDIF Distribuce}

function VratUProc_MinIndex: Integer;
function VratUProc_MaxIndex: Integer;

function VratUProcText(Index: Integer): String;
function VratUProcPopis(Index: Integer): String;
function VratUzivFunkci_MinIndex: Integer;
function VratUzivFunkci_MaxIndex: Integer;
function VratUzivFunkciText(Index: Integer): String;
function VratUzivFunkciPopis(Index: Integer): String;

function VratACTableSkript(const ATabulkaStr: string; ALegislativa: Integer = -1): String; overload;
function VratACTableSkript(aTabulka: TTabulka; aLegislativa: Integer = -1) : String; overload;
function VratZmenovySkript_MinIndex: Integer;
function VratZmenovySkript_MaxIndex: Integer;
function VratZmenovySkriptPlatiOd(Index: Integer): Int64;
function VratZmenovySkriptText(Index: Integer): string;
*)
function Constraint2Hlaska(AJmenoConstraintu : String): TTxt;
function JeToObrazek(PA: PAtributTabulky): Boolean;
function JeToBarva(PA: PAtributTabulky): Boolean;
function JeToRTF(PA: PAtributTabulky): Boolean;
function EURrozpadFaze2(ATabulka: TTabulka; AAtribut: String; APoradiAtr: Integer; ADvaPocitany: String = ''): String;
function High_TTabulka(ANovaDB: Boolean = False): TTabulka;
function MaxDelkaAtributu(PA                 : PAtributTabulky;
                          AUnicodeHypoteticky: Boolean = False //jakoby uû platilo, ûe [VAR]CHAR=N[VAR]CHAR
                         ): Extended;

IMPLEMENTATION

uses
  {$IFnDEF Distribuce}
   ddUProc,
   ddFunkce,
   ddACTable,
   ddZmeny,
  {$ENDIF Distribuce}
  {$IFDEF LaditCreateTable}
   sqDebug,
  {$ENDIF}
  Forms,
  StrUtils,
  ddUTA,
  sqAlter,
  ddEURrozpadVyjimky,
  ddBrowse,
  ddVazby,
  sqVazby,
  sqView,
  sqProc,
  ddTabulka,
  Pom,
  sqString,
  sqJazyk,
  sqEasyHash,

  dtDenik, dtCisZam, dtMzPF, dtZdrPoj, dtSkupMS, dtCisMzSl, dtMzdTar, dtMzPaus,
  dtDuchod, dtUzivOzn, dtDokZbo, dtDokZboDodatek, dtHGlob, dtRenameAtr

  ,dtUzivAtr
  ,ddMain2
  ,FMX.Dialogs
  ;

const
  PodtypStr : array[TPodtypAtributu] of string = ('', 'IDENTITY'{, 'ROWGUIDCOL'});

  { konverze na SQL typ }
  GTypy : array[TTypAtributu] of TDefTypu =
   ( ({$IFDEF Ladit}_X: taBlbe;{$ENDIF}
      T:''                                  ),

     ({$IFDEF Ladit}_X: taInt;{$ENDIF}
      T:'INT';
      {$IFDEF SPopisem}Popis:'znamÈnkov˝ integer pro obecnÈ pouûitÌ, rozsah -2^31 (-2,147,483,648) .. 2^31-1 (2,147,483,647)'{$ENDIF}),

     ({$IFDEF Ladit}_X: taIdentity;{$ENDIF}
      T:'INT'; X:ptaIdentity;
      {$IFDEF SPopisem}Popis:'znamÈnkov˝ integer pro obecnÈ pouûitÌ - autoincrement'{$ENDIF}),

     ({$IFDEF Ladit}_X: taSmallInt;{$ENDIF}
      T:'SMALLINT';
      {$IFDEF SPopisem}Popis:'znamÈnkov˝ integer, rozsah -2^15 (-32,768) .. 2^15-1 (32,767)'{$ENDIF}),

     ({$IFDEF Ladit}_X: taByte;{$ENDIF}
      T:'TINYINT' ;
      {$IFDEF SPopisem}Popis:'Byte, rozsah 0 .. 255'{$ENDIF}),

     ({$IFDEF Ladit}_X: taBoolean;{$ENDIF}
      T:'BIT'     ;
      {$IFDEF SPopisem}Popis:'Boolean (False/True)'{$ENDIF}),

     ({$IFDEF Ladit}_X: taNVarChar;{$ENDIF}
      T:'NVARCHAR' ;  {$IFDEF SPopisem}Popis:'string s promÏnnou dÈlkou, maxim·lnÏ 4000 znaku (Unicode)'{$ENDIF}),

     ({$IFDEF Ladit}_X: taNChar;{$ENDIF}
      T:'NCHAR'    ;  {$IFDEF SPopisem}Popis:'string s pevnou dÈlkou, maxim·lnÏ 4000 znaku (Unicode)'{$ENDIF}),

     ({$IFDEF Ladit}_X: taNText;{$ENDIF}
      T:'NTEXT'    ;  {$IFDEF SPopisem}Popis:'string s promÏnnou delkou, maxim·lnÏ 2^30-1 (1,073,741,823) znaku (Unicode)'{$ENDIF}),

     ({$IFDEF Ladit}_X: taBinary;{$ENDIF}
      T:'BINARY'  ;                         {$IFDEF SPopisem}Popis:'bin·rnÌ data s pevnou dÈlkou, maxim·lnÏ 8000 bajt˘'{$ENDIF}),

     ({$IFDEF Ladit}_X: taImage;{$ENDIF}
      T:'IMAGE'   ;                         {$IFDEF SPopisem}Popis:'bin·rnÌ data s promÏnnou dÈlkou, maxim·lnÏ 2^31-1 (2,147,483,647) bytu'{$ENDIF}),

     ({$IFDEF Ladit}_X: taDateTime;{$ENDIF}
      T:'DATETIME';                         {$IFDEF SPopisem}Popis:'datum a Ëas s rozsahem od 1. ledna 1753 do 31 prosince 9999 s p¯esnostÌ na 3.33 milisekund'{$ENDIF}),

     ({$IFDEF Ladit}_X: taNumeric_4_2;{$ENDIF}
      T:'NUMERIC' ;               P: 4; S:2;{$IFDEF SPopisem}Popis:'znamÈnkovÈ re·lnÈ ËÌslo s rozsahem 4,2'{$ENDIF}),

     ({$IFDEF Ladit}_X: taNumeric_5_2;{$ENDIF}
      T:'NUMERIC' ;               P: 5; S:2;{$IFDEF SPopisem}Popis:'znamÈnkovÈ re·lnÈ ËÌslo s rozsahem 5,2'{$ENDIF}),

     ({$IFDEF Ladit}_X: taNumeric_7_2;{$ENDIF}
      T:'NUMERIC' ;               P: 7; S:2;{$IFDEF SPopisem}Popis:'znamÈnkovÈ re·lnÈ ËÌslo s rozsahem 7,2'{$ENDIF}),

     ({$IFDEF Ladit}_X: taNumeric_9_2;{$ENDIF}
      T:'NUMERIC' ;               P: 9; S:2;{$IFDEF SPopisem}Popis:'znamÈnkovÈ re·lnÈ ËÌslo s rozsahem 9,2'{$ENDIF}),

     ({$IFDEF Ladit}_X: taNumeric_15_0;{$ENDIF}
      T:'NUMERIC' ;               P:15; S:0;{$IFDEF SPopisem}Popis:'znamÈnkovÈ re·lnÈ ËÌslo s rozsahem 10,0'{$ENDIF}),

     ({$IFDEF Ladit}_X: taNumeric_19_2;{$ENDIF}
      T:'NUMERIC' ;               P:19; S:2;{$IFDEF SPopisem}Popis:'znamÈnkovÈ re·lnÈ ËÌslo s rozsahem 19,2'{$ENDIF}),

     ({$IFDEF Ladit}_X: taNumeric_19_6;{$ENDIF}
      T:'NUMERIC' ;               P:19; S:6;{$IFDEF SPopisem}Popis:'znamÈnkovÈ re·lnÈ ËÌslo s rozsahem 19,6'{$ENDIF}),

     ({$IFDEF Ladit}_X: taNumeric_20_6;{$ENDIF}
      T:'NUMERIC' ;               P:20; S:6;{$IFDEF SPopisem}Popis:'znamÈnkovÈ re·lnÈ ËÌslo s rozsahem 20,6 pro poËÌtanÈ atributy'{$ENDIF}),

     ({$IFDEF Ladit}_X: taNumeric_28_0;{$ENDIF}
      T:'NUMERIC' ;               P:28; S:0;{$IFDEF SPopisem}Popis:'znamÈnkovÈ re·lnÈ ËÌslo s rozsahem 28,0 pro hodnoty, ktere nepokryje INT'{$ENDIF}),

     ({$IFDEF Ladit}_X: taFloat;{$ENDIF}
      T:'FLOAT'   ;   {$IFDEF SPopisem}Popis:'znamÈnkovÈ re·lnÈ ËÌslo s rozsahem - 1.79E+308 - 1.79E+308'{$ENDIF}),

      // NEPOUZIVAT
     ({$IFDEF Ladit}_X: taVarChar;{$ENDIF}
      T:'VARCHAR' ; {$IFDEF SPopisem}Popis:'string s promÏnnou dÈlkou, maxim·lnÏ 8000 znaku (non-Unicode)'{$ENDIF}),

     ({$IFDEF Ladit}_X: taChar;{$ENDIF}
      T:'CHAR'    ;  {$IFDEF SPopisem}Popis:'string s pevnou dÈlkou, maxim·lnÏ 8000 znaku (non-Unicode)'{$ENDIF}),

     ({$IFDEF Ladit}_X: taText;{$ENDIF}
      T:'TEXT'    ;  {$IFDEF SPopisem}Popis:'string s promÏnnou delkou, maxim·lnÏ 2^31-1 (2,147,483,647) znaku (non-Unicode) '{$ENDIF})

    );

function High_TTabulka (ANovaDB : Boolean = false) : TTabulka;
begin
  if Assigned(Server) then
    Result := Server.Server_High_TTabulka (ANovaDB)
  else
    Result := High(TTabulka);
end;

function GetTabulkaDef(ATabulka : TTabulka): PTabulkaDef;
begin
  Result := ddTabulka_GetTabulkaDef(ATabulka);
end;

function GetTabulkaDef(const ATabulkaStr: String): PTabulkaDef;
  var
    DynTab: TDynamickaTabulkaItem;
begin
  if not JeObecnyPrehled(ATabulkaStr) then
    Result := GetTabulkaDef(TypTabulky(ATabulkaStr))
  else
    begin
      // obecne prehledy jsou definovany jinde
      DynTab := GenerujDynamickouTabulku(ATabulkaStr);
      if Assigned(DynTab) then
        Result := @DynTab.FTabDef
      else
        Result := nil;
    end;
end;

{ --------------------------------------------------------------------------- }

function Str2TypAttr(const NazevTypu: String; P,S: Integer): TTypAtributu;
  var
    TT: TTypAtributu;
begin
  Result := taBlbe;

  if SameText(NazevTypu, 'INT IDENTITY') then
  begin
    Result := taIdentity;
    Exit;
  end;

  // [RK 19.05.2009] detekce [N]VARCHAR(MAX)
  if P = -1 then
  begin
    if SameText(NazevTypu, 'NVARCHAR') then
    begin
      Result := taNText;
      Exit;
    end;

    if SameText(NazevTypu, 'VARCHAR') then
    begin
      Result := taText;
      Exit;
    end;
  end;

  for TT := Low(TTypAtributu) to High(TTypAtributu) do
  begin
    if SameText(NazevTypu, GTypy[TT].T) then
    begin
      if not SameText('NUMERIC', GTypy[TT].T) then
      begin
        Result := TT;
        Exit;
      end;

      if (GTypy[TT].P = P) and (GTypy[TT].S = S) then
      begin
        Result := TT;
        Exit;
      end;
    end;
  end;

  // pokud to je numeric a nemam ho definovany, tak vezmi ten "nejvetsi"
  if SameText(NazevTypu, 'NUMERIC') then Result := taNumeric_20_6;
end;

{ --------------------------------------------------------------------------- }

function Typ2Str(ATypAtributu : TTypAtributu) : TDefTypu;
begin
  Result := GTypy[ATypAtributu];
end;

{ --------------------------------------------------------------------------- }

function Podtyp2Str(APodtypAtributu : TPodtypAtributu) : String;
begin
  Result := PodtypStr[APodtypAtributu];
end;

{ --------------------------------------------------------------------------- }

function UzivatelskyTyp(ATypAtributu : TUzivatelskyTypAtributu) : TUzivatelskeAtributyDef;
begin
  Result := UzivatelskeAtributy[ATypAtributu];
end;

{ --------------------------------------------------------------------------- }

function TypTabulky(const AJmenoSysTabulky : String) : TTabulka;
var
  LTabulka : TTabulka;
begin
  Result := tZadna;
  if Trim(AJmenoSysTabulky) = '' then Exit;

  if JeObecnyPrehled(AJmenoSysTabulky) then
    Result := tx_ObecnyPrehled
  else
    for LTabulka := Succ(Low(TTabulka)) to High_TTabulka do
    begin
      if SameText(AJmenoSysTabulky, ddTabulka_GetTabulkaDef(LTabulka).JmenoSys) then
      begin
        Result := LTabulka;
        Break;
      end;
    end;
end;

{ --------------------------------------------------------------------------- }

function JmenoTabulkyEng(iTabulka : TTabulka) : String;
begin
  if ddTabulka_GetTabulkaDef(iTabulka) <> nil then
    Result := ddTabulka_GetTabulkaDef(iTabulka).JmenoSysEng
  else
    Result := '';
end;

{ --------------------------------------------------------------------------- }

function JmenoTabulky(iTabulka : TTabulka) : String;
begin
  if ddTabulka_GetTabulkaDef(iTabulka) <> nil then
    Result := ddTabulka_GetTabulkaDef(iTabulka).JmenoSys
  else
    Result := '';
end;

{ --------------------------------------------------------------------------- }

function VerejneJmenoTabulky(iTabulka: TTabulka): String;
begin
  if ddTabulka_GetTabulkaDef(iTabulka) <> nil then
    Result := ddTabulka_GetTabulkaDef(iTabulka).Jmeno
  else
    Result := '';
end;

{ --------------------------------------------------------------------------- }

function VerejneJmenoTabulky(const AJmenoSys : String) : String;
  var
    LTabulka : TTabulka;
    DynTab: TDynamickaTabulkaItem;
begin
  Result := '';
  if AJmenoSys = '' then Exit;

  if not JeObecnyPrehled(AJmenoSys) then
    begin
      for LTabulka := Succ(Low(TTabulka)) to High_TTabulka do
        if SameText(AJmenoSys, ddTabulka_GetTabulkaDef(LTabulka).JmenoSys) then
        begin
          Result := ddTabulka_GetTabulkaDef(LTabulka).Jmeno;
          Break;
        end;
    end
  else
    begin
      // obecne prehledy jsou definovany jinde
      DynTab := GenerujDynamickouTabulku(AJmenoSys);
      if not Assigned(DynTab) then Exit;
      Result := DynTab.FTabDef.Jmeno;
    end;
end;

{ --------------------------------------------------------------------------- }

function VerejneJmenoPrehledu(BID: Integer) : String;
  var
    SysName: String;
begin
  if BID = 0 then
    Result := ''
  else
    if JeObecnyPrehled(BID) then
      begin
        SysName := Find_ObecPrehledSysNazBID(BID);
        if SysName = '' then
          Result := Format('%s (BID: %d)', [sqlCtiOznam(xNenalezeno2), BID])
        else
          Result := VerejneJmenoTabulky(SysName);
      end
    else
    begin
      if SeznamVychozichNastaveniBrowse[BID2Browse(BID)].JmenoTxt<>0 then
        Result:=sqlCtiOznam(SeznamVychozichNastaveniBrowse[BID2Browse(BID)].JmenoTxt)
      else
        Result := SeznamVychozichNastaveniBrowse[BID2Browse(BID)].Jmeno;
    end;
end;

{ --------------------------------------------------------------------------- }

function VratView(iTabulka : TTabulka): String;
begin
  Result := ddTabulka_GetTabulkaDef(iTabulka).DefiniceView;
end;

{ --------------------------------------------------------------------------- }

function Constraint2VerejneJmenoTabulky(const AConstraint : String) : String;
var I : integer;
    S : string;
begin
  S := AConstraint;
  I := Pos (ConstraintOddelovac,S);
  Delete (S,1,I+Length(ConstraintOddelovac)-1);
  I := Pos (ConstraintOddelovac,S);
  Result := VerejneJmenoTabulky(Copy(S,1,I-1));
end;

{ --------------------------------------------------------------------------- }

function JeObecnyPrehled(const NazevSys: String): Boolean;
begin
  Result := SameText(sqlViewPrefix, Copy(NazevSys, 1, sqlViewPrefixLen));
end;

{ --------------------------------------------------------------------------- }

function JeObecnyPrehled(BID: Integer): Boolean;
begin
  Result := (BID >= x_ObecnePohledy_BID_Base) {and (BID <= x_ObecnePohledy_BID_Top)};
end;

{ --------------------------------------------------------------------------- }

function BrowseDPSN2BrowseID(const BrowseDPSN: String): Integer;
begin
  if JeObecnyPrehled(BrowseDPSN) then
    Result := Find_ObecPrehledBIDPodleSysNaz(BrowseDPSN)
  else
    Result := StrToIntDef(BrowseDPSN, Integer(bZadny));
end;

{ --------------------------------------------------------------------------- }

function BrowseID2BrowseDPSN(BID: Integer): String;
begin
  if JeObecnyPrehled(BID) then
    begin
      Result := Find_ObecPrehledSysNazBID(BID);
      if Result = '' then
        Result := IntToStr(BID);
    end
  else
    Result := IntToStr(BID);
end;

{ --------------------------------------------------------------------------- }

function OdstranPrefixTabulky(const AJmenoSysAtributu : String) : String;
  var
    LPos : Integer;
begin
  LPos := Pos('.', AJmenoSysAtributu);
  if LPos > 0 then
    Result := Copy(AJmenoSysAtributu, LPos+1, MaxInt)
  else
    Result := AJmenoSysAtributu;
end;

{ --------------------------------------------------------------------------- }

function PrepisTyp (PA : PAtributTabulky) : string;
  begin
    Result := '';
    if GTypy[PA.Typ].P>0
    then
      { precision + scale }
      Result := GTypy[PA.Typ].T+'(' + IntToStr(GTypy[PA.Typ].P)+','+
                                      IntToStr(GTypy[PA.Typ].S)+')' { typ    }
    else
    begin
      if (PA.Delka>0) and (SkupinaAtributu(PA.Typ)=skpRetezce)
      then
        Result := GTypy[PA.Typ].T + '(' + IntToStr(PA.Delka) + ')'
      else
        Result := GTypy[PA.Typ].T;
    end;
  end;

{ --------------------------------------------------------------------------- }

procedure AtributyTabulky(TD                  : PTabulkaDef;
                          ASL                 : TStrings;
                          AVcetneSystemovych  : Boolean;          // Systemovy = not Verejny
                          AJakNaplnit         : TJakNaplnit;
                          AVcetneDefinovanych : Boolean = False   // vËetnÏ uûivatelsky i "program·torsky" definovan˝ch
                         );
  var
    I     : Integer;
    PA    : PAtributTabulky;
    Line  : Integer;
    PomSL : TStringList;
begin
  if not Assigned(ASL) then Exit;
  ASL.Clear;

  if not Assigned(TD) then Exit;

  //[PFA 3.3.2011] - pro nemeckou verzi plnim jen pro tabulku s vyplnenym JmenoSysEng
  if (AJakNaplnit = jnJenJmenaSOdkazemBezTabulkyEng) and (TD.JmenoSysEng='') then exit;

  PA := TD.Atributy;
  for I := 1 to TD.PocetAtributu do
  begin
    if (AVcetneSystemovych  or (PA.Verejny = vTrue)) and
       (AVcetneDefinovanych or not(dvaDefinovany in PA.DalsiVlastnostiAtr)) then
    begin
      case AJakNaplnit of
        jnJenJmenaSOdkazemBezTabulky: // [RK 11.01.2002]
          ASL.AddObject(PA.JmenoSys, TObject(PA));

        jnJenJmenaSOdkazem: // [RK 19.01.2001] [RK 25.01.2001]
          ASL.AddObject(Format('%s.%s', [TD.JmenoSys, PA.JmenoSys]), TObject(PA));

        jnJenSumovatelneSOdkazem: // [RK 29.02.2008]
          if PA.Sumovat and MuzeBytSumovat(PA.Typ) then
            ASL.AddObject(Format('%s.%s', [TD.JmenoSys, PA.JmenoSys]), TObject(PA));

        jnJenJmena:
          ASL.Add(TD.JmenoSys + '.' + PA.JmenoSys);

        jnJenJmenaBezPocitanychABinary :
           begin
             if (PA.Pocitany='') and {(PA.Typ<>taImage) and} (PA.Typ<>taBinary) then
               ASL.Add(TD.JmenoSys + '.' + PA.JmenoSys);
           end;

        jnJenJmenaBezPocitanych_BezJmenaTabulky :
           begin
             if PA.Pocitany='' then
               ASL.Add(PA.JmenoSys);
           end;

        jnJenJmenaBezPocitanych_BezIdentity_BezJmenaTabulky :
           begin
             if (PA.Pocitany='') and (PA.Typ<>taIdentity) then
               ASL.Add(PA.JmenoSys);
           end;

        jnSeznam:
          begin
            Line := ASL.Add(PA.JmenoSys);
            PomSL := TStringList.Create;

            { typ    }
            if PA.UTyp <> utaZadny then
              begin
                {$IFDEF SPopisem}
                PomSL.Add(UzivatelskeAtributy[PA.UTyp].J);
                PomSL.Add('');
                PomSL.Add(PrepisTyp (PA));
                {$ELSE}
                PomSL.Add('');
                PomSL.Add('');
                PomSL.Add('');
                {$ENDIF}
              end
            else
              begin
                PomSL.Add (PrepisTyp (PA));

                PomSL.Add(PodTypStr[GTypy[PA.Typ].X]); { podtyp }
                PomSL.Add('');
              end;

            PomSL.Add(NULLorNOTNULL[PA.NULL]); { NULL/NOT NULL }
            PomSL.Add(PA.ServerDefault);             { ServerDefault       }

            {$IFDEF SPopisem}                  { Popis         }
             PomSL.Add(PA.Popis);
            {$ELSE}
             PomSL.Add('');
            {$ENDIF}

            ASL.Objects[Line] := PomSL;
          end;

        jnJenJmenaSOdkazemBezTabulkyEng: // [PFA 22.3.2011]
          begin
            if (dvaDefinovany in PA.DalsiVlastnostiAtr) and (PA.JmenoSys[1]='_') then
              ASL.AddObject(Format('%s as %s', [PA.JmenoSys, PA.JmenoSys]), TObject(PA))
            else
            if PA.JmenoSysEng<>'' then
              ASL.AddObject(Format('%s as %s', [PA.JmenoSys, PA.JmenoSysEng]), TObject(PA));
          end;

        jnJenJmenaSOdkazemBezTabulkyEngBezAlias: // [PFA 22.3.2011]
          begin
            if (dvaDefinovany in PA.DalsiVlastnostiAtr) and (PA.JmenoSys[1]='_') then
              ASL.AddObject(Format('%s', [PA.JmenoSys]), TObject(PA))
            else
            if PA.JmenoSysEng<>'' then
              ASL.AddObject(Format('%s', [PA.JmenoSysEng]), TObject(PA));
          end;

        jnJenJmenaSOdkazemProNazvySkupin:
          if (PA.Typ <> taIdentity) and (SkupinaAtributu(PA.Typ) <> skpBinarni) then
            ASL.AddObject(Format('%s.%s', [TD.JmenoSys, PA.JmenoSys]), TObject(PA));

        jnJenJmenaBezPocitanych_BezIdentity:
           begin
             if (PA.Pocitany='') and (PA.Typ<>taIdentity) then
               ASL.Add(TD.JmenoSys + '.' + PA.JmenoSys);
           end;

        jnJenJmenaSOdkazemProStavyDokladu:
          if (PA.Typ = taInt) and (dvaStav in PA.DalsiVlastnostiAtr) then
            ASL.AddObject(Format('%s.%s', [TD.JmenoSys, PA.JmenoSys]), TObject(PA));

      end;
    end;
    Inc(PA);
  end;
end;

{ --------------------------------------------------------------------------- }

procedure AtributyTabulky(ATabulka            : TTabulka;
                          ASL                 : TStrings;
                          AVcetneSystemovych  : Boolean;          // Systemovy = not Verejny
                          AJakNaplnit         : TJakNaplnit;
                          AVcetneDefinovanych : Boolean = False   // vËetnÏ uûivatelsky i "program·torsky" definovan˝ch
                         );
begin
  AtributyTabulky(GetTabulkaDef(ATabulka), ASL, AVcetneSystemovych,
                  AJakNaplnit, AVcetneDefinovanych);
end;

{ --------------------------------------------------------------------------- }

procedure AtributyTabulky(const ATabulkaStr   : String;
                          ASL                 : TStrings;
                          AVcetneSystemovych  : Boolean;          // Systemovy = not Verejny
                          AJakNaplnit         : TJakNaplnit;
                          AVcetneDefinovanych : Boolean = False   // vËetnÏ uûivatelsky i "program·torsky" definovan˝ch
                         );
  var
    TD: PTabulkaDef;
begin
  TD := GetTabulkaDef(ATabulkaStr);
  if Assigned(TD) then
    AtributyTabulky(TD, ASL, AVcetneSystemovych, AJakNaplnit, AVcetneDefinovanych)
  else
    if Assigned(ASL) then ASL.Clear;
end;

{ --------------------------------------------------------------------------- }

function MaTabulkaAtribut(      ATabulka                 : TTabulka;
                          const AJmenoAtributuBezPrefixu : String) : Boolean;
var
  I  : Integer;
  PA : PAtributTabulky;
begin
  Result := False;
  PA := ddTabulka_GetTabulkaDef(ATabulka).Atributy;
  for I := 1 to ddTabulka_GetTabulkaDef(ATabulka).PocetAtributu do
  begin
    if SameText(PA.JmenoSys, AJmenoAtributuBezPrefixu) then
    begin
      Result := True;
      Exit;
    end;
    Inc(PA);
  end;
end;

{ --------------------------------------------------------------------------- }

procedure PridejVyzadovaneAtributy(ATabulka : TTabulka;
                                   ASL      : TStrings);
var
  I      : Integer;
  PA     : PAtributTabulky;
  PomStr : String;
begin
  PA := ddTabulka_GetTabulkaDef(ATabulka).Atributy;
  for I := 1 to ddTabulka_GetTabulkaDef(ATabulka).PocetAtributu do
  begin
    if PA.Vyzadovany = vTrue then
    begin
      PomStr := Format('%s.%s', [ddTabulka_GetTabulkaDef(ATabulka).JmenoSys, PA.JmenoSys]);
      if ASL.IndexOf(PomStr)=-1 then
      begin { kdyz tam neni - pridej ho }
        ASL.Add(PomStr);
      end;
    end;
    Inc(PA);
  end;
end;

{ --------------------------------------------------------------------------- }

function TypAtributu(const AJmenoSysAtributu  : String): TTypAtributu;
  var
    PA: PAtributTabulky;
begin
  Result := taBlbe;
  if AJmenoSysAtributu = '' then Exit;

  PA := Atribut(AJmenoSysAtributu);
  if Assigned(PA) then Result := PA.Typ;
end;

{ --------------------------------------------------------------------------- }

function TypPocitanehoAtributu(PA: PAtributTabulky): TTypPocitanehoAtributu;
begin
  if dvaDefinovany in PA.DalsiVlastnostiAtr then
    begin
      if _dvaExterniAtribut in PA.DalsiVlastnostiAtr then
        Result := tpaExterniAtribut // externÌ sloupec
      else
        if PA.JmenoSys[1] = '_' then
          Result := tpaUzivatelskyDefinovany     // uûivatelsky definovan˝ sloupec
        else
          Result := tpaProgramatorskyDefinovany; // program·torsky definovan˝ sloupec
    end
  else
    if PA.Pocitany <> '' then
      Result := tpaDatabaze     // pocitany sloupec
    else
      Result := tpaNeniPocitany;
end;

{ --------------------------------------------------------------------------- }

function Atribut(ATabulka : TTabulka;
                 const AJmenoSysAtributuBezTabulky : String): PAtributTabulky; overload;
  var
    PA : PAtributTabulky;
    I  : Integer;
begin
  Result := nil;

  PA := ddTabulka_GetTabulkaDef(ATabulka).Atributy;
  for I := 1 to ddTabulka_GetTabulkaDef(ATabulka).PocetAtributu do
  begin
    if SameText(AJmenoSysAtributuBezTabulky, PA.JmenoSys) then
    begin
      Result := PA;
      Exit;
    end;
    Inc(PA);
  end;
  if (Result = nil) and (Server <> nil) and Server.JeToHeliosGermany then
  begin
    PA := ddTabulka_GetTabulkaDef(ATabulka).Atributy;
    for I := 1 to ddTabulka_GetTabulkaDef(ATabulka).PocetAtributu do
    begin
      if (PA.JmenoSysEng<>'') AND (SameText(AJmenoSysAtributuBezTabulky, PA.JmenoSysEng)) then
      begin
        Result := PA;
        Exit;
      end;
      Inc(PA);
    end;
  end;
end;

{ --------------------------------------------------------------------------- }

function Atribut(const AJmenoSysAtributu : String): PAtributTabulky;
  var
    I : Integer;
    LTabulka : TTabulka;
    LJmenoSysTabulky,
    LJmenoSysAtributu : String;
    DynTab: TDynamickaTabulkaItem;
    PA : PAtributTabulky;
begin
  Result := nil;

  I := Pos('.', AJmenoSysAtributu);
  if I = 0 then Exit;

  LJmenoSysTabulky  := Copy(AJmenoSysAtributu,   1, I-1);
  LJmenoSysAtributu := Copy(AJmenoSysAtributu, I+1, MaxInt);

  if not JeObecnyPrehled(LJmenoSysTabulky) then
  begin
    for LTabulka := Succ(Low(TTabulka)) to High_TTabulka do
      if SameText(LJmenoSysTabulky, ddTabulka_GetTabulkaDef(LTabulka).JmenoSys) then
      begin
        Result := Atribut(LTabulka, LJmenoSysAtributu);
        Exit;
      end;

    Exit;
  end;

  // obecne prehledy jsou definovany jinde
  DynTab := GenerujDynamickouTabulku(LJmenoSysTabulky);
  if not Assigned(DynTab) then Exit;

  PA := DynTab.FTabDef.Atributy;
  for I := 1 to DynTab.FTabDef.PocetAtributu do
  begin
    if SameText(LJmenoSysAtributu, PA.JmenoSys) then
    begin
      Result := PA;
      Exit;
    end;
    Inc(PA);
  end;
end;

{ --------------------------------------------------------------------------- }

function AtributV(const AHlavniTabulkaStr: string;
                  const AJmenoSysAtributuSVazbou : String;
                        ATabName       : PString = nil;
                        AVazbaName     : PString = nil): PAtributTabulky; overload;
  var
    XX: Integer;
    LCiziTabulka: TTabulka;
    LCiziTabulkaStr : String;
    Vazba : String;
    PVD: PVztahDef;
begin
  Result := nil;

  XX := Pos('.', AJmenoSysAtributuSVazbou);
  if XX = 0 then Exit;

  Vazba := Copy(AJmenoSysAtributuSVazbou, 1, XX-1);
  if ATabName   <> nil then ATabName^   := Vazba;
  if AVazbaName <> nil then AVazbaName^ := Vazba;

  // pokud tam neni vazba, tak zkus jestli tam neni tabulka jako "prefix"
  if AJmenoSysAtributuSVazbou[1] <> 'V' then
  begin
    Result := Atribut(AJmenoSysAtributuSVazbou);
    Exit;
  end;

  if SeznamVazebPodleVazeb.NajdiVazbu(Vazba, PVD) then
  begin
    if SameText(PVD.TabPStr, AHlavniTabulkaStr) then
      begin
        LCiziTabulka := PVD.TabL;
        LCiziTabulkaStr := PVD.TabLStr;
      end
    else
      if SameText(PVD.TabLStr, AHlavniTabulkaStr) then
        begin
          LCiziTabulka := PVD.TabP;
          LCiziTabulkaStr := PVD.TabPStr;
        end
      else
        begin
          LCiziTabulka := tZadna;
          LCiziTabulkaStr := '';
        end;

    if LCiziTabulka <> tZadna then
    begin
      if ATabName <> nil then
        ATabName^ := LCiziTabulkaStr;

      if LCiziTabulka = tx_ObecnyPrehled then
        // hledame nejspis v definovanych prehledech
        Result := Atribut(LCiziTabulkaStr + '.' + Copy(AJmenoSysAtributuSVazbou, XX+1, MaxInt))
      else
        Result := Atribut(LCiziTabulka, Copy(AJmenoSysAtributuSVazbou, XX+1, MaxInt));
    end;
  end;
end;

{ --------------------------------------------------------------------------- }

function AtributV(      AHlavniTabulka: TTabulka;
                  const AJmenoSysAtributuSVazbou : String;
                        ATabName: PString = nil;
                        AVazbaName: PString = nil): PAtributTabulky;
begin
  Result := AtributV(JmenoTabulky(AHlavniTabulka), AJmenoSysAtributuSVazbou, ATabName, AVazbaName);
end;

{ --------------------------------------------------------------------------- }

function JeAtributSoucastiPK_UQ(TD: PTabulkaDef; PA: PAtributTabulky;
                                var SlozeniKlice: String): Boolean;
  var
    II: Integer;
    PC: PConstraintTabulky;
    SL: TStringList;
begin
  SlozeniKlice := '';

  SL := TStringList.Create;
  try
    if JeObecnyPrehled(TD.JmenoSys) then
      begin
        SL.CommaText := UniqueKey(TD.JmenoSys);
        if SL.IndexOf(PA.JmenoSys) <> -1 then
        begin
          SlozeniKlice := SL.CommaText;
          Result := True;
          Exit;
        end;
      end
    else
      begin
        PC := TD.Constraints;
        for II := 1 to TD.PocetConstraints do
        begin
          case PC.Typ of
            coPrimaryKey, coUnique:
            begin
              SL.CommaText := PC.Atributy;
              if SL.IndexOf(PA.JmenoSys) <> -1 then
              begin
                SlozeniKlice := Format('%s: %s', [PrefixyConstraints[PC.Typ], PC.Atributy]);
                Result := True;
                Exit;
              end;
            end;
          end;
          Inc(PC);
        end;
      end;
  finally
    SL.Free;
  end;

  Result := False;
end;

{ --------------------------------------------------------------------------- }

function SkupinaAtributu(ATypAtributu: TTypAtributu): TSkupinaAtributu;
begin
  case ATypAtributu of
    taInt,
    taIdentity,
  //taBigInt,
    taSmallInt,
    taByte,
    taBoolean      : Result := skpCelaCisla;

    taVarChar,
    taChar,
    taText,
    taNVarChar,
    taNChar,
    taNText        : Result := skpRetezce;

    taDateTime     : Result := skpDatumy;

   {taMoney, zruseno}
    taFloat,
    taNumeric_4_2,
    taNumeric_5_2,
    taNumeric_7_2,
    taNumeric_9_2,
    taNumeric_15_0,
    taNumeric_19_2,
    taNumeric_19_6,
    taNumeric_20_6,
    taNumeric_28_0 : Result := skpDesetinnaCisla;

    taImage,
    taBinary       : Result := skpBinarni;

    else             Result := skpZadna;
  end;
end;

{ --------------------------------------------------------------------------- }

function MuzeBytSumovat(aTypAtributu : TTypAtributu) : Boolean;
begin
  Result := (not (aTypAtributu in [taBoolean])) and
            (SkupinaAtributu(aTypAtributu) in [skpCelaCisla, skpDesetinnaCisla]);
end;

{ --------------------------------------------------------------------------- }

function TabulkaPocetTriggers (const AJmenoSys : String) : integer;
begin
  Result := ddTabulka_GetTabulkaDef(TypTabulky(AJmenoSys)).PocetTriggers;
end;

{------------------------------------------------------------------------------}

{$IFDEF trg}
function _GetTriggerText(TR : PTriggerTabulky) : String;
var
  HRes  : HRSRC;
  LSize : DWORD;
  ATriggerName : String;
begin
  Result := '';

  ATriggerName := TR.Jmeno;
  HRes := FindResource(MainInstance, PChar(ATriggerName), RT_RCDATA);

  if HRes=0 then
    raise Exception.CreateFmt('Trigger "%s" v sekci RCDATA nenalezen!', [ATriggerName]);

  with TabDenik_Triggers[1] do begin
    LSize := SizeOfResource(MainInstance, HRes);
    SetLength(Result, LSize);
    Move(Pointer(LoadResource(MainInstance, HRes))^, PChar(Result)^, LSize)
  end;
end;
{$ELSE}
function _GetTriggerText(TR : PTriggerTabulky) : String;
begin
  with TR^ do
  begin
    {$IFDEF Ladit}
    if (Length(Text)  > MaxDelkaBlokuTextu) or
       (Length(Text2) > MaxDelkaBlokuTextu) or
       (Length(Text3) > MaxDelkaBlokuTextu) or
       (Length(Text4) > MaxDelkaBlokuTextu) or
       (Length(Text5) > MaxDelkaBlokuTextu) or
       (Length(Text6) > MaxDelkaBlokuTextu) then
      MessageBox(0,
        PChar(Format('Trigger: %s (%d,%d,%d,%d,%d,%d)'#13 +
                     'Jeden z textovych bloku je prilis dlouhy!',
                     [Copy(Text, 1, 50),
                      Length(Text),  Length(Text2), Length(Text3),
                      Length(Text4), Length(Text5), Length(Text6)])),
        'LADIT', 0);
    {$ENDIF}

    Result := Text;
    if Length(Text2) > 0 then Result := Format('%s'#13'%s', [Result, Text2]);
    if Length(Text3) > 0 then Result := Format('%s'#13'%s', [Result, Text3]);
    if Length(Text4) > 0 then Result := Format('%s'#13'%s', [Result, Text4]);
    if Length(Text5) > 0 then Result := Format('%s'#13'%s', [Result, Text5]);
    if Length(Text6) > 0 then Result := Format('%s'#13'%s', [Result, Text6]);
  end;
end;
{$ENDIF}

{ --------------------------------------------------------------------------- }

function CreateTableTriggerString(const AJmenoSys : String; I : integer) : String;
  var
    PC : PTriggerTabulky;
    SL : TStringList;
begin
  Result := '';
  if (I>=1) and (I<=ddTabulka_GetTabulkaDef(TypTabulky(AJmenoSys)).PocetTriggers) then
  begin
    PC := ddTabulka_GetTabulkaDef(TypTabulky(AJmenoSys)).Triggers;
    Inc (PC,I-1);
    SL := TStringList.Create;
    try
      SL.Text := _GetTriggerText(PC);
      Result := SL.Text;
    finally
      SL.Free;
    end;
  end;
end;

{ --------------------------------------------------------------------------- }

function CreateTableFKString(ATabulka : TTabulka) : String;
  var
    I: Integer;
    PC: PConstraintTabulky;
begin
  Result := '';

  if ddTabulka_GetTabulkaDef(ATabulka) <> nil then
  begin
    PC := ddTabulka_GetTabulkaDef(ATabulka).Constraints;
    for I := 1 to ddTabulka_GetTabulkaDef(ATabulka).PocetConstraints do
    begin
      case PC.Typ of
        coForeignKey  :
        begin
          if Result <> '' then Result := Result + #13#10;
          Result := Result +
                    Format('ALTER TABLE %s ADD CONSTRAINT %s FOREIGN KEY (%s) REFERENCES %s (%s) ',
                          [ddTabulka_GetTabulkaDef(ATabulka).JmenoSys,
                           PC.JmenoSys,
                           PC.VlastniAtributy,
                           JmenoTabulky(PC.CiziTabulka),
                           PC.CiziAtributy]);
        end;
      end;
      Inc(PC);
    end;

    {$IFDEF LaditCreateTable}
     DebugZapisDoSouboru(Result);
    {$ENDIF}
  end;
end;

{ --------------------------------------------------------------------------- }

function CreateTableFKString(const AJmenoSys : String) : String;
  var
    LTabulka: TTabulka;
begin
  Result := '';

  if AJmenoSys <> '' then
    for LTabulka := Succ(Low(TTabulka)) to High_TTabulka do
    begin
      if Assigned(ddTabulka_GetTabulkaDef(LTabulka)) then
        if SameText(AJmenoSys, ddTabulka_GetTabulkaDef(LTabulka).JmenoSys) then
        begin
          Result := CreateTableFKString(LTabulka);
          Break;
        end;
    end;
end;

{ --------------------------------------------------------------------------- }

function CreateIndexString(ATabulka: TTabulka) : String;
  var
    I: Integer;
    PC: PConstraintTabulky;
begin
  Result := '';

  if ddTabulka_GetTabulkaDef(ATabulka) <> nil then
  begin
    PC := ddTabulka_GetTabulkaDef(ATabulka).Constraints;
    for I := 1 to ddTabulka_GetTabulkaDef(ATabulka).PocetConstraints do
    begin
      case PC.Typ of
        coIndex :
        begin
          if Result <> '' then Result := Result + #13#10;
          Result := Result +
                    Format('CREATE NONCLUSTERED INDEX %s ON %s(%s) ',
                          [PC.JmenoSys, ddTabulka_GetTabulkaDef(ATabulka).JmenoSys, PC.Atributy]);
        end;
        coIndexClustered :
        begin
          if Result <> '' then Result := Result + #13#10;
          Result := Result +
                    Format('CREATE CLUSTERED INDEX %s ON %s(%s) ',
                          [PC.JmenoSys, ddTabulka_GetTabulkaDef(ATabulka).JmenoSys, PC.Atributy]);
        end;
      end;
      Inc(PC);
    end;

    {$IFDEF LaditCreateTable}
     DebugZapisDoSouboru(Result);
    {$ENDIF}
  end;
end;

{ --------------------------------------------------------------------------- }

function CreateIndexString(const AJmenoSys : String) : String;
  var
    LTabulka: TTabulka;
begin
  Result := '';

  if AJmenoSys <> '' then
    for LTabulka := Succ(Low(TTabulka)) to High_TTabulka do
    begin
      if Assigned(ddTabulka_GetTabulkaDef(LTabulka)) then
        if SameText(AJmenoSys, ddTabulka_GetTabulkaDef(LTabulka).JmenoSys) then
        begin
          Result := CreateIndexString(LTabulka);
          Break;
        end;
    end;
end;

{ --------------------------------------------------------------------------- }

function Typ2DBTyp(PA: PAtributTabulky; VcetnePodtypu: Boolean = True): String;
begin
  // typ
  Result := GTypy[PA.Typ].T;

  // podtyp
  if VcetnePodtypu and (GTypy[PA.Typ].X <> ptaZadny) then
    Result := Format('%s %s', [Result, PodtypStr[GTypy[PA.Typ].X]]);

  // precision + scale
  if GTypy[PA.Typ].P > 0 then
    Result := Format('%s(%d,%d)', [Result, GTypy[PA.Typ].P, GTypy[PA.Typ].S]);

  // delka
  if (PA.Delka > 0) and (PA.Typ In TSkupinaAtributuSDelkou) then
    Result := Format('%s(%d)', [Result, PA.Delka]);
end;

{ --------------------------------------------------------------------------- }

function CreateTableString(const AJmenoSys : String; ADropIfExists : Boolean = False) : String;
  var
    LTabulka : TTabulka;
begin
  Result := '';
  if AJmenoSys<>'' then
  begin
    for LTabulka := Succ(Low(TTabulka)) to High_TTabulka do
    begin
      if ddTabulka_GetTabulkaDef(LTabulka) <> nil then
      begin
        if SameText(AJmenoSys, ddTabulka_GetTabulkaDef(LTabulka).JmenoSys) then
        begin
//          Result := CreateTableString(LTabulka, ADropIfExists);
          Break;
        end;
      end;
    end;
  end;
end;

{ --------------------------------------------------------------------------- }

function CreateTableEURString(ATabulka : TTabulka) : String;
begin
  Result := '';
end;

{ --------------------------------------------------------------------------- }

function DropTableString(ATabulka : TTabulka) : String;
begin
  Result := Format('DROP TABLE %s', [ddTabulka_GetTabulkaDef(ATabulka).JmenoSys]);
end;

{ --------------------------------------------------------------------------- }

function DeleteTableString(ATabulka : TTabulka) : String;
begin
  Result := Format('DELETE %s', [ddTabulka_GetTabulkaDef(ATabulka).JmenoSys]);
end;

{ --------------------------------------------------------------------------- }

function NastavPravaSkript(NazevObjektu: string; const Typ: string): string;
begin
  // Typ zatim nijak nepouzit

  // kontrola na prefix 'dbo.'
  if not SameText(Copy(NazevObjektu, 1, Length(dbo)), dbo) then
    NazevObjektu := dbo + NazevObjektu;

  Result := Format('GRANT ALL ON %s TO PUBLIC', [NazevObjektu]);
end;

{ --------------------------------------------------------------------------- }

function VerejneJmenoAtributu(PA : PAtributTabulky; const AJmenoSys : String = '') : String;
begin
  if not Assigned(PA) then
    Result := AJmenoSys
  else
    if PA.JmenoVerejne = '' then
      Result := Format('[%s]', [PA.JmenoSys])
    else
      Result := PA.JmenoVerejne;
end;

{ --------------------------------------------------------------------------- }

function VerejneJmenoAtributu(const AJmenoSys : String) : String;
begin
  Result := VerejneJmenoAtributu(Atribut(AJmenoSys), AJmenoSys);
end;

{ --------------------------------------------------------------------------- }

function VerejneJmenoAtributu(ATabulka: TTabulka; const AJmenoSysAtributuBezTabulky: String): String;
begin
  Result := VerejneJmenoAtributu(
              Atribut(ATabulka, AJmenoSysAtributuBezTabulky), AJmenoSysAtributuBezTabulky);
end;

{------------------------------------------------------------------------------}
function Atribut2Tabulka(const AAtribut : String) : String;
var LPos : Integer;
begin
  Result := '';
  LPos   := Pos('.', AAtribut);
  if LPos>1 then
    Result := Copy(AAtribut, 1, LPos-1);
end;

{------------------------------------------------------------------------------}
function PAtributTabulky2JmenoSys(P : PAtributTabulky) : String;
var
  LTabulka   : TTabulka;
  I          : Integer;
  PA         : PAtributTabulky;
  TabSTeckou : String;
begin
  Result := '';
  if Assigned(P) then
  begin
    for LTabulka := Succ(Low(TTabulka)) to High_TTabulka do
    begin
      TabSTeckou := ddTabulka_GetTabulkaDef(LTabulka).JmenoSys+'.';
      PA := ddTabulka_GetTabulkaDef(LTabulka).Atributy;
      for I := 0 to ddTabulka_GetTabulkaDef(LTabulka).PocetAtributu-1 do
      begin
        if P=PA then
        begin
          Result := TabSTeckou+PA.JmenoSys;
          Exit;
        end;
        Inc(PA);
      end;
    end;
  end;
end;
{------------------------------------------------------------------------------}
const
  PolePodminek : array[TPodminka] of TPodminkaDef = {POZOR - uûivateli se zobrazuje ODZADU, ale prohledava ODPREDU }
  ( { kdyz je prazdny, znamena to, ze dany operator pro danou skupinu neni pristupny }
  {poBlbe}      (),
  {poISNOTNULL} (Sys:'IS NOT NULL'  ; JmenoProCisla:xje_zadano      ; JmenoProStr:xje_zadano      ; JmenoProDate:xje_zadano      ),
  {poNerovno}   (Sys:'<>'           ; JmenoProCisla:xse_nerovna     ; JmenoProStr:xse_nerovna     ; JmenoProDate:xse_nerovna     ),
  {poMensiRovno}(Sys:'<='           ; JmenoProCisla:xje_nejvice_____; JmenoProStr:xje_nejvice_____; JmenoProDate:xje_nejvice_____),
  {poVetsiRovno}(Sys:'>='           ; JmenoProCisla:xje_alespon_____; JmenoProStr:xje_alespon_____; JmenoProDate:xje_alespon_____),
  {poVetsi}     (Sys:'>'            ; JmenoProCisla:xje_vice_nez____; JmenoProStr:xje_vice_nez____; JmenoProDate:xje_vice_nez____),
  {poMensi}     (Sys:'<'            ; JmenoProCisla:xje_mene_nez____; JmenoProStr:xje_mene_nez____; JmenoProDate:xje_mene_nez____),
  {poRovno}     (Sys:'='            ; JmenoProCisla:xse_rovna____   ; JmenoProStr:xse_rovna____   ; JmenoProDate:xse_rovna____   ),
  {poNOTLIKEY}  (Sys:'Not Like'{<-!};                                 JmenoProStr:xnekonci_na     ;                              ),
  {poNOTLIKE}   (Sys:'NOT LIKE'{<-!};                                 JmenoProStr:xneobsahuje     ;                              ),
  {poNOTLIKEX}  (Sys:'not like'{<-!};                                 JmenoProStr:xnezacina_na    ;                              ),
  {poLIKEY}     (Sys:'Like'    {<-!};                                 JmenoProStr:xkonci_na       ;                              ),
  {poLIKE}      (Sys:'LIKE'    {<-!};                                 JmenoProStr:xobsahuje       ;                              ),
  {poLIKEX}     (Sys:'like'    {<-!};                                 JmenoProStr:xzacina_na      ;                              ),
  {poISNULL}    (Sys:'IS NULL'      ; JmenoProCisla:xnezadano       ; JmenoProStr:xnezadano       ; JmenoProDate:xnezadano       ),
  {poNOTBETWEEN}(Sys:'NOT BETWEEN'  ;                                                                                        ),  //neviditelny - zatim jen pro rozbor v TSelectoTvurce.VyhodnotVztahy()
  {poBETWEEN}   (Sys:'BETWEEN'      ;                                                                                        )   //neviditelny - zatim jen pro rozbor v TSelectoTvurce.VyhodnotVztahy()
  );

function Podminka2SysStr(APodminka : TPodminka) : String;
begin
  if (APodminka>=Succ(Low(TPodminka))) and (APodminka<=High(TPodminka)) then
  begin
    Result := PolePodminek[APodminka].Sys;
  end;
end;

{------------------------------------------------------------------------------}
function Podminka2VerejnyStr(APodminka    : TPodminka;
                             ATypAtributu : TTypAtributu) : String;
var
  Skupina : TSkupinaAtributu;
begin
  Result := '';
  if (APodminka   >=Succ(Low(TPodminka)))    and (APodminka   <=High(TPodminka)) and
     (ATypAtributu>=Succ(Low(TTypAtributu))) and (ATypAtributu<=High(TTypAtributu)) then
  begin
    Skupina := SkupinaAtributu(ATypAtributu);
    if Skupina in CiselneSkupiny then Result := sqlCtiOznam(PolePodminek[APodminka].JmenoProCisla)
    else
      if Skupina=skpRetezce      then Result := sqlCtiOznam(PolePodminek[APodminka].JmenoProStr)
      else
        if Skupina=skpDatumy     then Result := sqlCtiOznam(PolePodminek[APodminka].JmenoProDate)
        else
          if Skupina=skpBinarni  then Result := sqlCtiOznam(PolePodminek[APodminka].JmenoProCisla)  //[TZ 23.05.2007] aspoÚ p¯ibliûnÏ:)
          else
            Result := PolePodminek[APodminka].Sys;
  end;
end;

{ --------------------------------------------------------------------------- }

function SysStr2Podminka(const ASysStr : String) : TPodminka;
  var
    LPodminka: TPodminka;
begin
  for LPodminka := Succ(Low(TPodminka)) to High(TPodminka) do
  begin
    if SameText(PolePodminek[LPodminka].Sys, ASysStr) then
    begin
      Result := LPodminka;
      if Result = poLIKEY{prvnÌ z LIKE} then
        begin
          if ASysStr = 'like' then
            Result := poLIKEX
          else
            if ASysStr = 'LIKE' then
              Result := poLIKE;
        end
      else
        if Result = poNOTLIKEY{prvnÌ z NOT LIKE} then
        begin
          if ASysStr = 'not like' then
            Result := poNOTLIKEX
          else
            if ASysStr = 'NOT LIKE' then
              Result := poNOTLIKE;
        end;

      Exit;
    end;
  end;

  Result := poBlbe;
end;

{ --------------------------------------------------------------------------- }

function UpravPodminkuLike(APodminka: TPodminka; const AHodnota: string): string;
begin
  Result := AHodnota;

  case APodminka of
    poLIKE, poNOTLIKE:
       begin
         if PosEx('%', Result) = 0 then
           Result := Format('%%%s%%', [Result]);
       end;
    poLIKEX, poNOTLIKEX:
       begin
         if PosEx('%', Result) = 0 then
           Result := Result + '%';
       end;
    poLIKEY, poNOTLIKEY:
       begin
         if PosEx('%', Result) = 0 then
           Result := '%' + Result;
       end;
  end;
end;

{ --------------------------------------------------------------------------- }

constructor Tx.Create(AOdTabulka, ADoTabulka: TTabulka);
  var
    LTabulka: TTabulka;
begin
  inherited Create;
  FTabulka := tZadna;
  FPole := TBits.Create;
  FSL_Tab := TStringList.Create;
  FSL_UProc := TStringList.Create;
  XSL := TStringList.Create;
  PridavaneDefaultySL := TStringList.Create;
  ZmenaDefaultySL := TStringList.Create;
  OdTabulka := AOdTabulka;
  DoTabulka := ADoTabulka;
  IndexZacatek := 0;
  IndexKonec := 0;

  FPomSL := TStringList.Create;
  FWhereTab := '';
  for LTabulka := OdTabulka to DoTabulka do
  begin
    if (JmenoTabulky(LTabulka)[1]<>'#')
{$IFnDEF Ladit}
        and (not JeView(LTabulka))
{$EndIf}
    then
      FWhereTab := Format('%s,%s', [FWhereTab, NQuotedStr(JmenoTabulky(LTabulka))]);
  end;
  System.Delete(FWhereTab,1,1); // zruseni prvni carky
end;

{ --------------------------------------------------------------------------- }

destructor Tx.Destroy;
  var
    I, J, K : Integer;
    LSL1 : TStringList;
    LSL2 : TStringList;

    AtrSL : TStringList;
    ErrSL : TStringList;
    SQLSL : TStringList;
begin
  FPomSL.Free;
  FQuery.Free;

  ZmenaDefaultySL.Free;
  PridavaneDefaultySL.Free;
  XSL.Free;

  for I := 0 to FSL_UProc.Count-1 do
  begin
    LSL1 := TStringList(FSL_UProc.Objects[I]);
    for J := 0 to LSL1.Count-1 do
    begin
      LSL2 := TStringList(LSL1.Objects[J]);
      LSL2.Free;
    end;
  end;
  FSL_UProc.Free;

  for I := 0 to FSL_Tab.Count-1 do
  begin
    AtrSL := TStringList(FSL_Tab.Objects[I]);
    for J := 0 to AtrSL.Count-1 do
    begin
      ErrSL := TStringList(AtrSL.Objects[J]);
      for K := 0 to ErrSL.Count-1 do
      begin
        SQLSL := TStringList(ErrSL.Objects[K]);
        SQLSL.Free;
      end;
      ErrSL.Free;
    end;
    AtrSL.Free;
  end;
  FSL_Tab.Free;

  FPole.Free;
  inherited;
end;

{ --------------------------------------------------------------------------- }

procedure Tx.SetWhereTab(const aWhereTab: String);
begin
  FWhereTab := aWhereTab;
end;

{ --------------------------------------------------------------------------- }

procedure Tx.PridejDoSL_Tab(const ATab   : String;
                            const AAtr   : String;
                            const AText  : String;
                            const ASQL   : String;
                                  Pridat : Integer);
var
  I, J  : Integer;
  AtrSL : TStringList;
  ErrSL : TStringList;
  SQLSL : TStringList;
begin
  case Pridat of
     TxPridat         : XSL.Insert(XSL.Count-IndexKonec,ASQL);
     TxOdebrat        : XSL.Insert(IndexZacatek, ASQL);
     TxPridatKonec    : begin Inc(IndexKonec); XSL.Add(ASQL); end;
     TxOdebratZacatek : begin Inc(IndexZacatek); XSL.Insert(0, ASQL); end;
   end;

  I := FSL_Tab.IndexOf(ATab{JmenoTabulky(FTabulka)});
  if I=-1 then
  begin
    I := FSL_Tab.Add(ATab{JmenoTabulky(FTabulka)});
    AtrSL := TStringList.Create;
    FSL_Tab.Objects[I] := AtrSL;
  end
  else
  begin
    AtrSL := TStringList(FSL_Tab.Objects[I]);
  end;

  J := AtrSL.IndexOf(AAtr);
  if J=-1 then
  begin
    J := AtrSL.Add(AAtr);
    ErrSL := TStringList.Create;
    AtrSL.Objects[J] := ErrSL;
  end
  else
  begin
    ErrSL := TStringList(AtrSL.Objects[J]);
  end;

  I := ErrSL.Add(AText);
  SQLSL := TStringList.Create;
  SQLSL.Add(ASQL);
  ErrSL.Objects[I] := SQLSL;
end;

{------------------------------------------------------------------------------}
procedure Tx.PridejDoSL_UProc(const AUProc : String;
                              const AText  : String;
                              const ASQL   : String);
var
  I     : Integer;
  ErrSL : TStringList;
  SQLSL : TStringList;
begin
  XSL.Add(ASQL);

  I := FSL_UProc.IndexOf(AUProc);
  if I=-1 then
  begin
    I := FSL_UProc.Add(AUProc);
    ErrSL := TStringList.Create;
    FSL_UProc.Objects[I] := ErrSL;
  end
  else
  begin
    ErrSL := TStringList(FSL_UProc.Objects[I]);
  end;

  I := ErrSL.Add(AText);
  SQLSL := TStringList.Create;
  SQLSL.Add(ASQL);
  ErrSL.Objects[I] := SQLSL;
end;

{------------------------------------------------------------------------------}
function Pomocna(const Typ:String; PodTyp:String; C_O_L:String; N_P : String; N_S: String; I_N:Boolean):String;
var LNull: TNull;
begin
  Result := UpperCase(Typ);
  if (Pos('CHAR', Typ)>0) or (Pos('BINARY', Typ)>0) then
  begin
    Result := Result + Format('(%s)', [C_O_L]);
  end
  else
  begin
    if Typ='NUMERIC' then
    begin
      Result := Result + Format('(%s,%s)', [N_P, N_S]);
    end;

    if PodTyp<>'' then
    begin
      PodTyp := UpperCase(PodTyp);
      if Pos('IDENTITY', PodTyp)=1 then
      begin
        Result := Result + ' ' + PodTyp;
      end
      else
      begin
        {$IFDEF Ladit}
         MessageBox(0, 'Nepokryt˝ podtyp atributu (unita ddMain)!', 'LADIT', 0);
        {$ENDIF}
      end;
    end;

  end;
  if I_N then LNull := nNULL
         else LNull := nNOTNULL;
  Result := Result + ' ' + NULLorNOTNULL[LNull];
end;

{------------------------------------------------------------------------------}
function TypZDefinice(PA : PAtributTabulky) : String;
var SPodTyp : String;
begin
  if GTypy[PA.Typ].X=ptaZadny then SPodTyp := ''
                              else SPodTyp := PodtypStr[GTypy[PA.Typ].X]+'(1,1)';
  Result := Pomocna(GTypy[PA.Typ].T,
                    SPodTyp,
                    IntToStr(PA.Delka),
                    IntToStr(GTypy[PA.Typ].P),
                    IntToStr(GTypy[PA.Typ].S),
                    PA.NULL=nNULL
                    );

end;

{------------------------------------------------------------------------------}
procedure Tx.SetTabulka(ATabulka: TTabulka);
  var
    PA : PAtributTabulky;
    S  : String;
    JakPridat : integer;
    LTextik   : String;
begin
  if ATabulka <> FTabulka then
  begin
    while FPole.OpenBit <> FPole.Size do
    begin
      { v definici prebyva - v databazi atribut(-y) chybi }
      PA := ddTabulka_GetTabulkaDef(FTabulka).Atributy;
      Inc(PA, FPole.OpenBit);
      if not (dvaDefinovany in PA.DalsiVlastnostiAtr) then
      begin
        if PA.Pocitany <> '' then
          S := Format('ALTER TABLE %s ADD %s AS %s',
                      [JmenoTabulky(FTabulka), PA.JmenoSys, PA.Pocitany])
        else
          S := DejADDCOLUMNstring(FTabulka, PA);

        if PA.Pocitany <> '' then
          begin
            JakPridat := TxPridatKonec;
            LTextik   := sqlCtiOznam(xPocitanyAtributVTabulceChybi);
          end
        else
          begin
            JakPridat := TxPridat;
            LTextik   := sqlCtiOznam(xAtributVTabulceChybi);
          end;
//view MS
{$IfDef LADIT}
        if JeView(FTabulka) then
          PridejDoSL_Tab(JmenoTabulky(FTabulka)+' (view)',
                         PA.JmenoSys,
                         LTextik,
                         '/* '+S+' */',
                         JakPridat)
        else
{$EndIf}
          PridejDoSL_Tab(JmenoTabulky(FTabulka),
                         PA.JmenoSys,
                         LTextik,
                         S,
                         JakPridat);
      end;
      FPole[FPole.OpenBit] := True;
    end;

    FTabulka := ATabulka;

    FPole.Size := 0; { zrus stare pole }
    if (FTabulka >= OdTabulka) and (FTabulka <= DoTabulka) then
    begin
      FPole.Size := ddTabulka_GetTabulkaDef(FTabulka).PocetAtributu;
    end;
  end;
end;

{ --------------------------------------------------------------------------- }

function Tx.DejADDCOLUMNstring (ATabulka : TTabulka; PA : PAtributTabulky) : string;
var S, N : string;
begin
  S := Format('ALTER TABLE %s ADD %s %s', [JmenoTabulky(ATabulka), PA.JmenoSys, TypZDefinice(PA)]);
  if PA.ServerDefault<>'' then
  begin
{        S := S + ' DEFAULT ' + PA.ServerDefault;}
    S := S + ' CONSTRAINT ' +
             PrefixyConstraints[coDefault] + ConstraintOddelovac + ddTabulka_GetTabulkaDef(ATabulka).JmenoSys + ConstraintOddelovac + PA.JmenoSys +
             ' DEFAULT ' + PA.ServerDefault;
    PridavaneDefaultySL.Add (PrefixyConstraints[coDefault] + ConstraintOddelovac + ddTabulka_GetTabulkaDef(ATabulka).JmenoSys + ConstraintOddelovac + PA.JmenoSys);
  end
  else
    if (PA.NULL=nNOTNULL) and (PA.Typ<>taIdentity) then
    begin
      N := PrefixyConstraints[coDefault] + ConstraintOddelovac + ddTabulka_GetTabulkaDef(ATabulka).JmenoSys + ConstraintOddelovac + ConstraintOddelovac + DefaultConstraintName;
      case SkupinaAtributu (PA.Typ) of
        skpRetezce :
          begin
            S := S + ' CONSTRAINT ' + N + ' DEFAULT ''''';
            S := S + Format(' ALTER TABLE %s DROP CONSTRAINT %s',[JmenoTabulky(ATabulka), N]);
          end;
        skpCelaCisla, skpDesetinnaCisla :
          begin
            S := S + ' CONSTRAINT ' + N + ' DEFAULT 0';
            S := S + Format(' ALTER TABLE %s DROP CONSTRAINT %s',[JmenoTabulky(ATabulka), N]);
          end;
        else ; {nezajem, cbyba ... }
      end;
    end;
   Result := S;
end;

{-----------------------------}
function Tx.JeVyrazStejny_Yukon(Adb, Azdr: String) : Boolean;
begin
    Result := SameText(Adb, Azdr);
end;

{------------------------------------------------------------------------------}
procedure Tx.Kontrola(const COLUMN_NAME            : String;
                      const DATA_TYPE              : String;
                      const AutoIncrement          : Boolean;//ShortString;
                            IS_NULLABLE            : Boolean;
                      const CHARACTER_OCTET_LENGTH : String;
                      const NUMERIC_PRECISION      : String;
                      const NUMERIC_SCALE          : String;
                      const COMPUTED               : String{;
                      const COLUMN_DEFAULT         : String}
                     );
  {-----------------------------}
  function TypZDatabaze: string;
  var
    SIdentity: string;
  begin
    if AutoIncrement then SIdentity := 'IDENTITY(1,1)'
                     else SIdentity := '';
    Result := Pomocna(UpperCase(DATA_TYPE),
                      SIdentity,
                      CHARACTER_OCTET_LENGTH,
                      NUMERIC_PRECISION,
                      NUMERIC_SCALE,
                      IS_NULLABLE
                     );
  end;
  {-----------------------------}
  procedure ZmenaPocitany (JmenoTabulky : string; JmenoAtributu : string; Pocitany : string; ATabulka : TTabulka; PA : PAtributTabulky);
  begin
//view MS
{$IfDef LADIT}
    if JeView((TypTabulky(JmenoTabulky)))
    then
{ zde se resi view }
      PridejDoSL_Tab(JmenoTabulky+' (view)',
                     JmenoAtributu,
                     Format('%s (%s)', [sqlCtiOznam(xZmenaPocitanehoAtributu), sqlCtiOznam(xddMain_Smazani)]),
                     Format('/* ALTER TABLE %s DROP COLUMN %s */',[JmenoTabulky, JmenoAtributu]),
                     TxPridatKonec)
    else
{$EndIf}
    if Pocitany = '' then
    begin
{ zde se resi NEpocitany atribut, kdyby tato varianta nastala }
      PridejDoSL_Tab(JmenoTabulky,
                 JmenoAtributu,
                 Format('%s (%s)', [sqlCtiOznam(xZmenaPocitanehoAtributu), sqlCtiOznam(xddMain_Smazani)]),
                 Format('ALTER TABLE %s DROP COLUMN %s', [JmenoTabulky, JmenoAtributu]),
                 TxOdebratZacatek);

      PridejDoSL_Tab(JmenoTabulky,
                 JmenoAtributu,
                 Format('%s (%s)', [sqlCtiOznam(xZmenaPocitanehoAtributu), sqlCtiOznam(xddMain_Pridani)]),
                 DejADDCOLUMNstring(ATabulka,PA),
                 TxPridatKonec);
    end
    else
    begin
{ zde se resi pocitany atribut }
      PridejDoSL_Tab(JmenoTabulky,
                 JmenoAtributu,
                 Format('%s (%s)', [sqlCtiOznam(xZmenaPocitanehoAtributu), sqlCtiOznam(xddMain_Smazani)]),
                 Format('ALTER TABLE %s DROP COLUMN %s', [JmenoTabulky, JmenoAtributu]),
                 TxOdebratZacatek);

      PridejDoSL_Tab(JmenoTabulky,
                 JmenoAtributu,
                 Format('%s (%s)', [sqlCtiOznam(xZmenaPocitanehoAtributu), sqlCtiOznam(xddMain_Pridani)]),
                 Format('ALTER TABLE %s ADD %s AS %s', [JmenoTabulky, JmenoAtributu, Pocitany]),
                 TxPridatKonec);
{$IfDef LADIT}
      PridejDoSL_Tab(JmenoTabulky,
                 JmenoAtributu,
                 Format('%s (kÛd v DB)', [sqlCtiOznam(xZmenaPocitanehoAtributu)]),
                 Format('/* ALTER TABLE %s ADD %s AS %s */', [JmenoTabulky, JmenoAtributu, COMPUTED]),
                 TxPridatKonec);
{$EndIf}

    end;
  end;
  {-----------------------------}
var
  PA : PAtributTabulky;
  {-----------------------------}
  {-----------------------------}
var
  I          : Integer;
  TDB        : String;
  TDef       : String;
  JakOdebrat : Integer;
{$IfDef LADIT}
  // pro view
  Flg        : Boolean;
  S          : String;
{$EndIf}
{  AtributTabulky : PAtributTabulky;}
begin
  if (OdTabulka<=FTabulka) and (FTabulka<=DoTabulka) then
  begin
    PA := ddTabulka_GetTabulkaDef(FTabulka).Atributy;
    for I := 0 to ddTabulka_GetTabulkaDef(FTabulka).PocetAtributu-1 do
    begin
      if (not(dvaDefinovany in PA.DalsiVlastnostiAtr)) and
         SameText(COLUMN_NAME, PA.JmenoSys) then
      begin
        TDB  := TypZDatabaze;
        TDef := TypZDefinice(PA);
        if TDB<>TDef then
        begin
          { jsou ruzne typy a co pocitany? }
          // MS 10.10.2005 - if SameText(COMPUTED, PA.Pocitany) then
          if JeVyrazStejny_Yukon(COMPUTED, PA.Pocitany) then
          begin
//verze s view MS
{$IfDef LADIT}
            if JeView(FTabulka)
            then
            begin
              Flg := True;
              if Server.SQLVersion>=C_SQLVersion_2005 then
              begin
                //je toYukon
                // na Yukonu byl problem, ze view melo v db INT IDENTITY a ve zdrojaku pouze INT
                // uprava ve zdrojaku nesla provest, protoze by to zase nechodilo na SQL2000
                S := StringReplace(TDB,#32'IDENTITY(1,1)'#32,#32,[rfReplaceAll]);
                if (S=TDef) then Flg := False;
              end;

              if Flg then
                PridejDoSL_Tab(JmenoTabulky(FTabulka)+' (view)',
                               COLUMN_NAME,
                               Format('%s -> %s', [TDB, TDef]),
                               Format('/* ALTER TABLE %s ALTER COLUMN %s %s */', [JmenoTabulky(FTabulka), COLUMN_NAME, TDef]),
                               TxPridat)
            end
            else
{$EndIf}
            begin
              PridejDoSL_Tab(JmenoTabulky(FTabulka),
                             COLUMN_NAME,
                             Format('%s -> %s', [TDB, TDef]),
                             Format('ALTER TABLE %s ALTER COLUMN %s %s', [JmenoTabulky(FTabulka), COLUMN_NAME, TDef]),
                             TxPridat);
              ZmenaDefaultySL.Add(PrefixyConstraints[coDefault]+ConstraintOddelovac+JmenoTabulky(FTabulka)+ConstraintOddelovac+COLUMN_NAME);
            end
          end
          else { nejsou stejny }
            ZmenaPocitany (JmenoTabulky(FTabulka),PA.JmenoSys,PA.Pocitany,FTabulka,PA);
        end
        else
        begin {typy jsou stejny, ale co pocitany? }
          // MS 10.10.2005 - if not SameText(COMPUTED, PA.Pocitany) then
          if not JeVyrazStejny_Yukon(COMPUTED, PA.Pocitany) then { nejsou stejny }
            ZmenaPocitany (JmenoTabulky(FTabulka),PA.JmenoSys,PA.Pocitany,FTabulka,PA);
        end;
        FPole[I] := True;
        Exit;
      end;
      Inc(PA);
    end;

    { v definici jsme nenasli - v databazi je atribut navic }
    if COMPUTED<>'' then JakOdebrat := TxOdebratZacatek
                    else JakOdebrat := TxOdebrat;
//verze s view MS
{$IfDef LADIT}
    if JeView(FTabulka)
    then
      PridejDoSL_Tab(JmenoTabulky(FTabulka)+' (view)',
                     COLUMN_NAME,
                     sqlCtiOznam(xAtributVTabulcePrebyva),
                     Format('/* ALTER TABLE %s DROP COLUMN %s */', [JmenoTabulky(FTabulka), COLUMN_NAME]),
                     JakOdebrat)
    else
{$EndIf}
    PridejDoSL_Tab(JmenoTabulky(FTabulka),
                   COLUMN_NAME,
                   sqlCtiOznam(xAtributVTabulcePrebyva),
                   Format('ALTER TABLE %s DROP COLUMN %s', [JmenoTabulky(FTabulka), COLUMN_NAME]),
                   JakOdebrat);
  end;
end;

{------------------------------------------------------------------------------}

function Tx.Konec(    AAktServer    : String;
                      AAktDB        : String;
                      APovolProvest : Boolean;
                  var ABylAltB      : Boolean;
                  var ioReindex     : TTristavovyBool{(vDefault, vFalse, vTrue)}) : Boolean;
begin
  Result := False;
end;

{------------------------------------------------------------------------------}

constructor TPomConstraintObject.Create (sS : string; sBlokovano : integer; sTableName : string);
begin
  inherited Create;
  S := sS;
  Blokovano := sBlokovano;
  TableName := sTableName;
end;

function TPomConstraintObject.Get : string;
begin
  Result := S;
end;

function TPomConstraintObject.GetBlokovano : integer;
begin
  Result := Blokovano;
end;

function TPomConstraintObject.GetTableName : string;
begin
  Result := TableName;
end;

{------------------------------------------------------------------------------}

constructor TPomConstraintFKObject.Create(const ATabulka, AAtributy: String; ABlokovano: Integer; const AVlastniTabulka: string);
begin
  inherited Create;
  FTabulka := ATabulka;
  FAtributy := StringReplace(Trim(AAtributy),' ',',',[rfReplaceAll]);
  FBlokovano := ABlokovano;
  FVlastniTabulka := AVlastniTabulka;
end;

function TPomConstraintFKObject.GetTabulka : string;
begin
  Result := FTabulka;
end;

function TPomConstraintFKObject.GetVlastniTabulka: string;
begin
  Result := FVlastniTabulka;
end;

function TPomConstraintFKObject.GetAtributy : string;
begin
  Result := FAtributy;
end;

function TPomConstraintFKObject.GetBlokovano : integer;
begin
  Result := FBlokovano;
end;

{------------------------------------------------------------------------------}

procedure Tx.KontrolujOmezeni_FK(SLdatabaze : TStringList);

  procedure Zakazany_FK(FKzdrojak : string);
  var Typ, JmenoTabulka, Atributy : string;
  begin
    RozdelConstraint(FKzdrojak, Typ, JmenoTabulka, Atributy);
    PridejDoSL_Tab(JmenoTabulka,
                   FKzdrojak,
                   sqlCtiOznam(xOmezeniJeVDBVypnuto),
                 //[TZ 30.10.2003] p¯id·no WITH CHECK, aby se CONSTRAINTy p¯i zapnutÌ kontrolovaly
                 // - d¯Ìve se tedy nekontrolovalo a obËas se nÏkomu poda¯ilo naimportovat data
                 // neodpovÌdajÌcÌ CONSTRAINTum
                 //Format('ALTER TABLE %s CHECK CONSTRAINT %s', [JmenoTabulka, FKzdrojak]),
                   Format('ALTER TABLE %s WITH CHECK CHECK CONSTRAINT %s', [JmenoTabulka, FKzdrojak]),
                   TxPridat);
  end;

  function JeRovno(FKzdrojak: string; Omezeni: PConstraintTabulky;
    FKdatabaze, TABLEdatabaze, UQdatabazeTabulka, UQdatabazeAtributy: string;
    Blokovani: integer) : Boolean;
    var
      TABLEzdrojak : string;
      Atributy     : string;
      Typ          : string;
      SL1, SL2     : TStringList;
  begin
    Result := False;

    if SameText(FKzdrojak, FKdatabaze) then
    begin
      RozdelConstraint(FKzdrojak, Typ, TABLEzdrojak, Atributy);

      if SameText(TABLEzdrojak, TABLEdatabaze) then
        if SameText(JmenoTabulky(Omezeni.CiziTabulka), UQdatabazeTabulka) then
        begin
          SL1 := TStringList.Create;
          try
            SL2 := TStringList.Create;
            try
              SL1.CommaText := UpperCase(Omezeni.CiziAtributy);
              SL2.CommaText := UpperCase(UQdatabazeAtributy);

              if SL1.CommaText=SL2.CommaText
              then Result := True
              else
              begin
                SL1.Sort;
                SL2.Sort;
                if SL1.CommaText=SL2.CommaText then Result := True;
              end;

              if Result then
                if Blokovani=1 then Zakazany_FK(FKZdrojak);

            finally
              SL2.Free;
            end;
          finally
           SL1.Free;
          end;
        end;
    end;
  end;

  procedure PridejConstraint (JmenoConstraint : string; Omezeni : PConstraintTabulky);
  var JmenoTabulka : string;
      Atributy     : string;
      Typ          : string;
  begin
    RozdelConstraint (JmenoConstraint,Typ,JmenoTabulka,Atributy);

    if (Typ = PrefixyConstraints[coForeignKey])
    then
      PridejDoSL_Tab(JmenoTabulka,
                     JmenoConstraint,
                     sqlCtiOznam(xOmezeniVDBChybi),
                     Format('ALTER TABLE %s ADD CONSTRAINT %s FOREIGN KEY (%s) REFERENCES %s (%s)', [JmenoTabulka, JmenoConstraint, Atributy, JmenoTabulky(Omezeni.CiziTabulka), Omezeni.CiziAtributy]),
                     TxPridat)
    else
      PridejDoSL_Tab(JmenoTabulka,
                     JmenoConstraint,
                     sqlCtiOznam(xOmezeniVDBChybi),
                     '',
                     TxPridat);
  end;

var SLzdrojak : TStringList;
    LTabulka  : TTabulka;
    PC        : PConstraintTabulky;

    i         : integer; { promenna cyklu pro SLzdrojak }
    j         : integer; { promenna cyklu pro SLdatabaze }
    Konec     : Boolean;
    p, q      : integer;

begin
  SLzdrojak  := TStringList.Create;
  try
    { prelej vsechny JmenaSys FK v aktualnim exe do string listu }
    for LTabulka:=OdTabulka to DoTabulka do
    begin
      if ddTabulka_GetTabulkaDef(LTabulka).JmenoSys[1]<>'#' then
      begin
        PC := ddTabulka_GetTabulkaDef(LTabulka).Constraints;
        for j := 1 to ddTabulka_GetTabulkaDef(LTabulka).PocetConstraints do
        begin
          case PC.Typ of
            coForeignKey :
              SLzdrojak.AddObject(PC.JmenoSys, TObject(PC));
          end;
          Inc(PC);
        end;
      end;
    end;

    { setrid omezeni }
    SLzdrojak.Sort;
    SLdatabaze.Sort;
    { porovnej Pr a Uq omezeni ve zdrojaku a databazi }

    i := 0; j:= 0; Konec := False;

    if (SLzdrojak.Count=0) or (SLdatabaze.Count=0) then Konec := True;

    P := 0; // [TZ] aby kompilator nehazel Warning
    while not Konec do
    begin
//      Application.ProcessMessages;

      if JeRovno(SLzdrojak[i], PConstraintTabulky(SLzdrojak.Objects[i]),
                 SLdatabaze[j], TPomConstraintFKObject(SLdatabaze.Objects[j]).GetVlastniTabulka,
                 TPomConstraintFKObject(SLdatabaze.Objects[j]).GetTabulka,
                 TPomConstraintFKObject(SLdatabaze.Objects[j]).GetAtributy,
                 TPomConstraintFKObject(SLdatabaze.Objects[j]).GetBlokovano) then
      begin
        Inc (i); Inc (j);
      end
      else { nerovnaji se atributy, zkus porovnavat z prava }
      begin
        if ((i+1)<SLzdrojak.Count) then { jsem posledni?      }
        begin                           { ne, nejsem posledni }
          p := i+1;
          Konec := False;
          while ((not JeRovno(SLzdrojak[p], PConstraintTabulky(SLzdrojak.Objects[p]),
                              SLdatabaze[j], TPomConstraintFKObject(SLdatabaze.Objects[j]).GetVlastniTabulka,
                              TPomConstraintFKObject(SLdatabaze.Objects[j]).GetTabulka,
                              TPomConstraintFKObject(SLdatabaze.Objects[j]).GetAtributy,
                              TPomConstraintFKObject(SLdatabaze.Objects[j]).GetBlokovano)) and (not Konec)) do
            if ((p+1)<SLzdrojak.Count) then Inc (p)
                                       else Konec := true;
        end
        else Konec := True;        { ano, jsem posledni  }
        if not Konec { NALEZEN stejny atribut }
        then
        begin
          for q := i to p-1 do
            PridejConstraint (SLzdrojak[q],PConstraintTabulky(SLzdrojak.Objects[q]));
          i := p+1; Inc (j);
        end
        else { nerovnaji se atributy, zkus porovnavat z leva }
        begin
          if ((j+1)<SLdatabaze.Count) { jsem posledni?      }
          then                        { ne, nejsem posledni }
          begin
            p := j+1;
            Konec := False;
            while ((not JeRovno(SLzdrojak[i], PConstraintTabulky(SLzdrojak.Objects[i]),
                                SLdatabaze[p], TPomConstraintFKObject(SLdatabaze.Objects[p]).GetVlastniTabulka,
                                TPomConstraintFKObject(SLdatabaze.Objects[p]).GetTabulka,
                                TPomConstraintFKObject(SLdatabaze.Objects[p]).GetAtributy,
                                TPomConstraintFKObject(SLdatabaze.Objects[p]).GetBlokovano)) and (not Konec)) do
              if ((p+1)<SLdatabaze.Count) then Inc (p)
                                          else Konec := True;
          end
          else Konec := True;         { ano, jsem posledni  }
          if not Konec { NALEZEN stejny atribut }
          then
          begin
            for q := j to p-1 do
              OdeberConstraint(SLdatabaze[q], TPomConstraintFKObject(SLdatabaze.Objects[q]).GetVlastniTabulka);
            j := p+1; Inc (i);
          end
          else { NENALEZENY shodne atributy, oba zarad a jdi dal }
          begin
            PridejConstraint(SLzdrojak[i], PConstraintTabulky(SLzdrojak.Objects[i]));
            Inc (i);

            OdeberConstraint(SLdatabaze[j], TPomConstraintFKObject(SLdatabaze.Objects[j]).GetVlastniTabulka);
            Inc (j);
          end;
        end;
      end;

      if ((SLzdrojak.Count=i) or (SLdatabaze.Count=j)) then Konec := true
                                                       else Konec := false;
    end;

    if ((i+1)<= SLzdrojak.Count) then
      for q:=i to SLzdrojak.Count-1 do
        PridejConstraint(SLzdrojak[q],PConstraintTabulky(SLzdrojak.Objects[q]));

    if ((j+1)<= SLdatabaze.Count) then
      for q:=j to SLdatabaze.Count-1 do
        OdeberConstraint(SLdatabaze[q], TPomConstraintFKObject(SLdatabaze.Objects[q]).GetVlastniTabulka);

  finally
    SLzdrojak.Free;
  end;
end;

{------------------------------------------------------------------------------}

procedure Tx.KontrolujDefaultyChecky (SLdatabaze : TStringList);
{$IFDEF LaditConstrainty}
var
   F : Text;
{$ENDIF}

  procedure PridejConstraint (JmenoConstraint : string; Omezeni : string);
  var JmenoTabulka : string;
      Atributy     : string;
      Typ          : string;
  begin
    RozdelConstraint (JmenoConstraint,Typ,JmenoTabulka,Atributy);

    if (Typ = PrefixyConstraints[coDefault])
    then
    begin
      if PridavaneDefaultySL.IndexOf (JmenoConstraint) = -1 then
      begin
        PridejDoSL_Tab(JmenoTabulka,
                       JmenoConstraint,
                       sqlCtiOznam(xOmezeniVDBChybi),
                       Format('ALTER TABLE %s ADD CONSTRAINT %s DEFAULT %s FOR %s', [JmenoTabulka, JmenoConstraint, Omezeni, Atributy]),
                       TxPridat)
      end
    end
    else
      if (Typ = PrefixyConstraints[coCheck])
      then
        PridejDoSL_Tab(JmenoTabulka,
                       JmenoConstraint,
                       sqlCtiOznam(xOmezeniVDBChybi),
                       Format('ALTER TABLE %s ADD CONSTRAINT %s CHECK (%s)', [JmenoTabulka, JmenoConstraint, Omezeni]),
                       TxPridat)
      else
        PridejDoSL_Tab(JmenoTabulka,
                       JmenoConstraint,
                       sqlCtiOznam(xOmezeniVDBChybi),
                       '',
                       TxPridat);
  end;

  procedure AlterConstraint(const NAMEdatabaze, NAMEzdrojak, DFzdrojak, DFdatabaze: string; ANajednou : Boolean = True);
  var JmenoTabulka : string;
      Atributy     : string;
      Typ          : string;
  begin
    RozdelConstraint (NAMEzdrojak,Typ,JmenoTabulka,Atributy);

    if (Typ = PrefixyConstraints[coDefault]) then
      begin
        if not ANajednou
        then
        begin
          PridejDoSL_Tab(JmenoTabulka,
                         NAMEzdrojak,
                         Format('%s (%s)', [sqlCtiOznam(xOmezeniSeVDBZmenilo), sqlCtiOznam(xddMain_Odebrani)]),
                         Format('ALTER TABLE %s DROP CONSTRAINT %s ', [JmenoTabulka, NAMEdatabaze]),
                         TxOdebrat);
          PridejDoSL_Tab(JmenoTabulka,
                         NAMEzdrojak,
                         Format('%s (%s)', [sqlCtiOznam(xOmezeniSeVDBZmenilo), sqlCtiOznam(xddMain_Pridani)]),
                         Format('ALTER TABLE %s ADD CONSTRAINT %s DEFAULT %s FOR %s',
                                [JmenoTabulka, NAMEzdrojak, DFzdrojak, Atributy]),
                         TxPridat);
        end
        else
        begin
          PridejDoSL_Tab(JmenoTabulka,
                         NAMEzdrojak,
                         sqlCtiOznam(xOmezeniSeVDBZmenilo),
                         Format('ALTER TABLE %s DROP CONSTRAINT %s '#13+
                                'ALTER TABLE %s ADD CONSTRAINT %s DEFAULT %s FOR %s',
                                [JmenoTabulka, NAMEdatabaze, JmenoTabulka, NAMEzdrojak, DFzdrojak, Atributy]),
                         TxPridat);
        end;
{$IfDef LADIT}
        PridejDoSL_Tab(JmenoTabulka,
                       NAMEzdrojak,
                       'OmezenÌ tohoto n·zvu se v datab·zi zmÏnilo (kÛd v DB)',
                       Format('/* ALTER TABLE %s ADD CONSTRAINT %s DEFAULT %s FOR %s */',
                              [JmenoTabulka, NAMEzdrojak, DFdatabaze, Atributy]),
                       TxPridat);
{$EndIf}
      end
    else
      if (Typ = PrefixyConstraints[coCheck]) then
        begin
          PridejDoSL_Tab(JmenoTabulka,
                         NAMEzdrojak,
                         sqlCtiOznam(xOmezeniSeVDBZmenilo),
                         Format('ALTER TABLE %s DROP CONSTRAINT %s '#13+
                                'ALTER TABLE %s ADD CONSTRAINT %s CHECK (%s)',
                                [JmenoTabulka, NAMEdatabaze, JmenoTabulka, NAMEzdrojak, DFzdrojak]),
                         TxPridat);
{$IfDef LADIT}
          PridejDoSL_Tab(JmenoTabulka,
                         NAMEzdrojak,
                         'OmezenÌ tohoto n·zvu se v datab·zi zmÏnilo (kÛd v DB)',
                         Format('/* ALTER TABLE %s ADD CONSTRAINT %s CHECK (%s) */',
                                [JmenoTabulka, NAMEzdrojak, DFdatabaze]),
                         TxPridat);
{$EndIf}
        end
      else
        PridejDoSL_Tab(JmenoTabulka,
                       NAMEzdrojak,
                       sqlCtiOznam(xOmezeniSeVDBZmenilo),
                       '',
                       TxPridat);
  end;

  procedure ZakazanyCheck (NAMEzdrojak : string);
  var Typ, JmenoTabulka, Atributy : string;
  begin
    RozdelConstraint(NAMEzdrojak, Typ, JmenoTabulka, Atributy);
    PridejDoSL_Tab(JmenoTabulka,
                   NAMEzdrojak,
                   sqlCtiOznam(xOmezeniJeVDBVypnuto),
                 //[TZ 30.10.2003] p¯id·no WITH CHECK, aby se CONSTRAINTy p¯i zapnutÌ kontrolovaly
                 // - d¯Ìve se tedy nekontrolovalo a obËas se nÏkomu poda¯ilo naimportovat data
                 // neodpovÌdajÌcÌ CONSTRAINTum
                 //Format('ALTER TABLE %s CHECK CONSTRAINT %s',[JmenoTabulka,NAMEzdrojak]),
                   Format('ALTER TABLE %s WITH CHECK CHECK CONSTRAINT %s', [JmenoTabulka, NAMEzdrojak]),
                   TxPridat);
  end;

  function JeRovno(NAMEzdrojak, DFzdrojak, TABLEzdrojak : string;
                   NAMEdatabaze, DFdatabaze, TABLEdatabaze : string; Blokovano : integer) : Boolean;

    function MezeryPryc (s : string) : string;
    begin
//      ChangeStr(s,' ');

      Result := s;
    end;

    function ZavorkyHranatePryc (s : string) : string;
    begin
//      ChangeStr(s,'[');
//      ChangeStr(s,']');

      Result := s;
    end;

    function ZavorkyHranatePrycAtribut(s : string; a : string) : string;
    begin
//      ChangeStr(s,'['+a+']',a);

      Result := s;
    end;

    function ZavorkyKulatePryc (s : string) : string;
    begin
//      ChangeStr(s,'(');
//      ChangeStr(s,')');

      Result := s;
    end;

  const _Between = ' BETWEEN ';
        _In1     = ' IN ';
        _In2     = ' IN(';
        _And     = ' AND ';
        _Or      = ' OR ';

  var JmenoTabulka : string;
      Atributy     : string;
      Typ          : string;
      i            : integer;
      Flg          : Boolean;
      s            : string;
      j1, j2       : string;
      SL           : TStringList;
      CKzdrojak,
      CKdatabaze  : string;
  begin
    Result := False;

    if SameText(NAMEzdrojak, NAMEdatabaze) and SameText(TABLEzdrojak, TABLEdatabaze) then
    begin
      RozdelConstraint (NAMEzdrojak,Typ,JmenoTabulka,Atributy);

      if (Typ = PrefixyConstraints[coDefault])
      then
      begin
        Delete (DFdatabaze,1,1); {pocatecni ( }
        Delete (DFdatabaze,Length(DFdatabaze),1); {koncova ) }

        // MS 14.10.2005 if not SameText(DFzdrojak, DFdatabaze) then
        if (ZmenaDefaultySL.IndexOf(NAMEdatabaze)<>-1)
        then
          AlterConstraint (NAMEdatabaze,NAMEzdrojak,DFzdrojak,DFdatabaze,False)
        else
          if (not JeVyrazStejny_Yukon(DFdatabaze, DFzdrojak)) then { zmena omezeni }
             AlterConstraint (NAMEdatabaze,NAMEzdrojak,DFzdrojak,DFdatabaze);
        Result := True;
      end
      else if (Typ = PrefixyConstraints[coCheck]) then
           begin
             CKzdrojak  := DFzdrojak;
             CKdatabaze := DFdatabaze;

             Flg := False;

             DFdatabaze := UpperCase(DFdatabaze);
             DFzdrojak  := UpperCase(DFzdrojak);
             Atributy   := Uppercase(Atributy);

             { nerovnaji se nahodou obe omezeni }
             //if DFdatabaze<>DFzdrojak then
             if not JeVyrazStejny_Yukon(DFdatabaze, DFzdrojak) then
             begin
               {$IFDEF LaditConstrainty}
                Append(F);
                WriteLn(F, NAMEzdrojak, #13#10#9, DFdatabaze, #13#10#9, DFzdrojak);
                Close(F);
               {$ENDIF}
               i := Pos (_In1,DFzdrojak); { zkus ' IN '}
               if i = 0 then i := Pos (_In2,DFzdrojak); { nevyslo, zkus ' IN('}
               { ' nelze zkouset jenom ' IN' - nazev atributu muze zacinat na ' IN'}
               if i<> 0 then
               begin
                 { jde o IN (0,1,2,...) }
                 Delete (DFzdrojak,i+1,Length(_In1)-1);

                 i := Pos (Atributy, DFzdrojak);
                 Delete (DFzdrojak,i,Length(Atributy));

                 DFzdrojak := ZavorkyKulatePryc (DFzdrojak);
//                 if Server.SQLVersion>=C_SQLVersion_2005 then s := '['+Atributy+']='
//                                                         else
                                                         s := '['+Atributy+'] = ';

                 DFdatabaze := ZavorkyKulatePryc (DFdatabaze);

                 SL := TStringList.Create;
                 try
                   SL.CommaText := DFzdrojak;

                   i := Pos (s,DFdatabaze);
                   while i <> 0 do
                   begin
                     Delete (DFdatabaze,1,Length(s));

                     i := Pos (_Or,DFdatabaze);
                     if i<>0 then
                     begin
                       j1 := Copy (DFdatabaze,1,i);
                       j1 := MezeryPryc (j1);

                       Delete (DFdatabaze,1,i+3);
                     end
                     else
                     begin
                       j1 := DFdatabaze;
                       j1 := MezeryPryc (j1);
                     end;
                     i := SL.IndexOf (j1);
                     if i = -1 then { nenalezeno v checku, check se zmenil }
                     begin
                      Flg := True;
                      break;
                     end;
                     SL.Delete(i);

                     {zkus dalsi}
                     i := Pos (s,DFdatabaze);
                   end;

                   {vsechno je OK, nezutalo beco v SL???}
                   if not Flg then
                   begin
                     if SL.Count>0 then Flg := True;
                   end;

                 finally
                   SL.Free;
                 end;
               end
               else
               begin
                 i := Pos (_Between,DFzdrojak);
                 if i<> 0 then
                 begin
                   { jde o BETWEEN (0 AND ...) }

                   Delete (DFzdrojak,i+1,Length(_Between)-1);

                   i := Pos (Atributy, DFzdrojak);
                   Delete (DFzdrojak,i,Length(Atributy));

                   i := Pos (_And,DFzdrojak);
                   j1 := Copy (DFzdrojak,1,i-1);
                   j2 := Copy (DFzdrojak,i+Length(_And),Length(DFzdrojak)-i-Length(_And)+1);

                   j1 := MezeryPryc (j1);
                   j2 := MezeryPryc (j2);

  {                 Delete (DFzdrojak,i,Length(_And));  }

                   s := '(['+Atributy+'] >= '+ j1 + ' AND ['+Atributy+'] <= ' + j2 + ')';

                 //if s<>DFdatabaze then
                   if not JeVyrazStejny_Yukon(DFdatabaze, s) then
                     Flg := True;
                 end
                 else
                 begin
                   { neco jineho }
                   DFzdrojak := MezeryPryc(DFzdrojak);
                   DFzdrojak := ZavorkyHranatePryc(DFzdrojak);

                   DFdatabaze := MezeryPryc(DFdatabaze);
                   DFdatabaze := ZavorkyHranatePryc(DFdatabaze);
                   DFdatabaze := ZavorkyKulatePryc(DFdatabaze);

                   if DFzdrojak <> DFdatabaze then
                   begin
                     i := Pos (Atributy,DFdatabaze);
                     while i<> 0 do
                     begin
                       Delete (DFdatabaze,i,Length(Atributy));
                       i := Pos (Atributy,DFdatabaze);
                     end;
                     if DFzdrojak <> DFdatabaze then Flg := true;
                   end;
                 end;
               end;
             end;

             if Flg then
               AlterConstraint(NAMEdatabaze,NAMEzdrojak,CKzdrojak,CKdatabaze)
             else
               if Blokovano=1 then ZakazanyCheck (NAMEzdrojak);

             Result := True;
           end;
    end;

  end;

var
  SLzdrojak : TStringList;
  LTabulka  : TTabulka;
  PC        : PConstraintTabulky;
  i         : integer; { promenna cyklu pro SLzdrojak }
  j         : integer; { promenna cyklu pro SLdatabaze }
  Konec     : Boolean;
  p, q      : integer;
begin
  {$IFDEF LaditConstrainty}
   AssignFile(F, 'C:\LaditCon.txt');
   Rewrite(F);
   WriteLn(F, 'Struktura: CK_Tabulka_JmenoChecku<CR><LF><TAB>Text v datab·zi<CR><LF><TAB>Text v EXE<CR><LF>'#13#10+
              '--------------------------------------------------------------------------------------------');
   Close(F);
  {$ENDIF}

  { setridim pro rychlejsi IndexOf }
  ZmenaDefaultySL.Sort;

  SLzdrojak := TStringList.Create;
  try
    { prelej vsechny JmenaSys DF omezeni v aktualnim exe do string listu }
    for LTabulka:=OdTabulka to DoTabulka do
    begin
      if ddTabulka_GetTabulkaDef(LTabulka).JmenoSys[1]<>'#' then
      begin
        PC := ddTabulka_GetTabulkaDef(LTabulka).Constraints;
        for j := 1 to ddTabulka_GetTabulkaDef(LTabulka).PocetConstraints do
        begin
          case PC.Typ of
            coDefault :
              SLzdrojak.AddObject(PC.JmenoSys,
                TPomConstraintObject.Create(PC.Vyraz, 0, ddTabulka_GetTabulkaDef(LTabulka).JmenoSys));
            coCheck :
              SLzdrojak.AddObject(PC.JmenoSys,
                TPomConstraintObject.Create(PC.Vyraz, 0, ddTabulka_GetTabulkaDef(LTabulka).JmenoSys));
          end;
          Inc(PC);
        end;
      end;
    end;

    { setrid omezeni }
    SLzdrojak.Sort;
    SLdatabaze.Sort;

    { porovnej DF omezeni ve zdrojaku a databazi }

    i := 0; j:= 0; Konec := False;

    if (SLzdrojak.Count=0) or (SLdatabaze.Count=0) then Konec := True;

    P := 0; // [TZ] aby kompilator nehazel Warning
    while not Konec do
    begin
//      Application.ProcessMessages;

      if (JeRovno(SLzdrojak[i], TPomConstraintObject(SLzdrojak.Objects[i]).Get,
                                TPomConstraintObject(SLzdrojak.Objects[i]).GetTableName,
                  SLdatabaze[j], TPomConstraintObject(SLdatabaze.Objects[j]).Get,
                                 TPomConstraintObject(SLdatabaze.Objects[j]).GetTableName,
                  TPomConstraintObject(SLdatabaze.Objects[j]).GetBlokovano))
      then
      begin
        Inc (i); Inc (j);
      end
      else { nerovnaji se atributy, zkus porovnavat z prava }
      begin
        if ((i+1)<SLzdrojak.Count) { jsem posledni?      }
        then                       { ne, nejsem posledni }
        begin
          p := i+1;
          Konec := False;
          while ((not JeRovno(SLzdrojak[p], TPomConstraintObject(SLzdrojak.Objects[p]).Get,
                                            TPomConstraintObject(SLzdrojak.Objects[p]).GetTableName,
                              SLdatabaze[j], TPomConstraintObject(SLdatabaze.Objects[j]).Get,
                                             TPomConstraintObject(SLdatabaze.Objects[j]).GetTableName,
                              TPomConstraintObject(SLdatabaze.Objects[j]).GetBlokovano))
                  and (not Konec)) do
            if ((p+1)<SLzdrojak.Count) then Inc (p)
                                       else Konec := true;
        end
        else Konec := True;        { ano, jsem posledni  }
        if not Konec { NALEZEN stejny atribut }
        then
        begin
          for q := i to p-1 do
            PridejConstraint (SLzdrojak[q],TPomConstraintObject(SLzdrojak.Objects[q]).Get);
          i := p+1; Inc (j);
        end
        else { nerovnaji se atributy, zkus porovnavat z leva }
        begin
          if ((j+1)<SLdatabaze.Count) { jsem posledni?      }
          then                        { ne, nejsem posledni }
          begin
            p := j+1;
            Konec := False;
            while ((not JeRovno(SLzdrojak[i], TPomConstraintObject(SLzdrojak.Objects[i]).Get,
                                              TPomConstraintObject(SLzdrojak.Objects[i]).GetTableName,
                                SLdatabaze[p], TPomConstraintObject(SLdatabaze.Objects[p]).Get,
                                               TPomConstraintObject(SLdatabaze.Objects[p]).GetTableName,
                                TPomConstraintObject(SLdatabaze.Objects[p]).GetBlokovano))
                    and (not Konec)) do
              if ((p+1)<SLdatabaze.Count) then Inc (p)
                                          else Konec := True;
          end
          else Konec := True;         { ano, jsem posledni  }
          if not Konec { NALEZEN stejny atribut }
          then
          begin
            for q := j to p-1 do
              OdeberConstraint(SLdatabaze[q], TPomConstraintObject(SLdatabaze.Objects[q]).GetTableName);
            j := p+1; Inc (i);
          end
          else { NENALEZENY shodne atributy, oba zarad a jdi dal }
          begin
            PridejConstraint(SLzdrojak[i],TPomConstraintObject(SLzdrojak.Objects[i]).Get);
            Inc (i);

            OdeberConstraint(SLdatabaze[j], TPomConstraintObject(SLdatabaze.Objects[j]).GetTableName);
            Inc (j);
          end;
        end;
      end;

      if ((SLzdrojak.Count=i) or (SLdatabaze.Count=j)) then Konec := true
                                                       else Konec := false;
    end;

    if ((i+1)<= SLzdrojak.Count) then
      for q:=i to SLzdrojak.Count-1 do
        PridejConstraint(SLzdrojak[q], TPomConstraintObject(SLzdrojak.Objects[q]).Get);

    if ((j+1)<= SLdatabaze.Count) then
      for q:=j to SLdatabaze.Count-1 do
        OdeberConstraint(SLdatabaze[q], TPomConstraintObject(SLdatabaze.Objects[q]).GetTableName);

  finally
    // uvolneni navazanych objektu TPomConstraintObject z pameti
    for i := 0 to SLzdrojak.Count-1 do
      SLzdrojak.Objects[i].Free;
    SLzdrojak.Free;
  end;

end;

{------------------------------------------------------------------------------}

procedure Tx.RozdelConstraint (iJmenoConstraint : string; var oTyp, oJmenoTabulky, oAtributy : string);
var i : integer;
begin
  { typ omezeni }
  oTyp := UpperCase(Copy(iJmenoConstraint,1,2));
  Delete(iJmenoConstraint,1,4);

  { tabulka }
  i := Pos (ConstraintOddelovac,iJmenoConstraint);
  oJmenoTabulky := Copy (iJmenoConstraint,1,i-1);
  Delete (iJmenoConstraint,1,i+1);

  { atributy }
  oAtributy := iJmenoConstraint;
  i := Pos (ConstraintOddelovac,oAtributy);
  while i<>0 do
  begin
//    Application.ProcessMessages;

    oAtributy[i] := ',';
    Delete (oAtributy,i+1,1);
    i := Pos (ConstraintOddelovac,oAtributy);
  end;
end;

{------------------------------------------------------------------------------}

procedure Tx.OdeberConstraint(AJmenoConstraint, AJmenoTabulka: string);
begin
  if AJmenoTabulka <> '' then
    PridejDoSL_Tab(AJmenoTabulka,
                   AJmenoConstraint,
                   sqlCtiOznam(xOmezeniVDBPrebyva),
                   Format('ALTER TABLE %s DROP CONSTRAINT %s', [AJmenoTabulka, AJmenoConstraint]),
                   TxOdebrat)
  else
    PridejDoSL_Tab('??????',
                   AJmenoConstraint,
                   sqlCtiOznam(xOmezeniVDBPrebyva),
                   '',
                   TxOdebrat);
end;

{------------------------------------------------------------------------------}

procedure Tx.KontrolujOmezeni_PK_UQ (SLdatabaze : TStringList);
{ v idealnim pripade to ma slozitost n, tj. jeden pruchod seznamu v pameti }

{ neni mozne si definovat vlastni SYSNAZEV - v nazvu jsou uzlozny vsechny atributy,
  ktere tvrori klic. jednim porovnanim nazvu ze zdrojaku a z databaze lehce zjistim,
  zda se klic nezmenil. pri vlasnim SYSNAZVU by to nebylo tak jednoduche.
}

  var
    SLzdrojak : TStringList;
    LTabulka  : TTabulka;
    PC        : PConstraintTabulky;
    i         : integer; { promenna cyklu pro SLzdrojak }
    j         : integer; { promenna cyklu pro SLdatabaze }
    Konec     : Boolean;
    p, q      : integer;

  { +++++++++++++++++++++++++++ }

    procedure PridejConstraint (JmenoConstraint : string);
    var JmenoTabulka : string;
        Atributy     : string;
        Typ          : string;
    begin
      RozdelConstraint (JmenoConstraint,Typ,JmenoTabulka,Atributy);

      if Typ = PrefixyConstraints[coPrimaryKey] then
        PridejDoSL_Tab(JmenoTabulka,
                   JmenoConstraint,
                   sqlCtiOznam(xOmezeniVDBChybi),
                   Format('ALTER TABLE %s ADD CONSTRAINT %s PRIMARY KEY (%s)', [JmenoTabulka, JmenoConstraint, Atributy]),
                   TxPridat)
      else if Typ = PrefixyConstraints[coUnique] then
        PridejDoSL_Tab(JmenoTabulka,
                   JmenoConstraint,
                   sqlCtiOznam(xOmezeniVDBChybi),
                   Format('ALTER TABLE %s ADD CONSTRAINT %s UNIQUE (%s)', [JmenoTabulka, JmenoConstraint, Atributy]),
                   TxPridat)
        else
          PridejDoSL_Tab(JmenoTabulka,
                     JmenoConstraint,
                     sqlCtiOznam(xOmezeniVDBChybi),
                     '',
                     TxPridat);
{                   Format('ALTER TABLE %s DROP COLUMN %s', [JmenoTabulky(FTabulka), COLUMN_NAME]));}
    end;

  { +++++++++++++++++++++++++++ }

    function JeRovno(IndexZdrojak, IndexDatabaze: Integer): Boolean;
    begin
      Result := SameText(SLzdrojak[IndexZdrojak], SLdatabaze[IndexDatabaze]) and
                SameText(TPomConstraintObject(SLzdrojak.Objects[IndexZdrojak]).GetTableName,
                         TPomConstraintObject(SLdatabaze.Objects[IndexDatabaze]).GetTableName);
    end;

  { +++++++++++++++++++++++++++ }

begin
  SLzdrojak  := TStringList.Create;

  try
    { prelej vsechny JmenaSys Pr a Uq omezeni v aktualnim exe do string listu }
    for LTabulka := OdTabulka to DoTabulka do
    begin
      if ddTabulka_GetTabulkaDef(LTabulka).JmenoSys[1] <> '#' then
      begin
        PC := ddTabulka_GetTabulkaDef(LTabulka).Constraints;
        for j := 1 to ddTabulka_GetTabulkaDef(LTabulka).PocetConstraints do
        begin
          case PC.Typ of
            coPrimaryKey, coUnique:
              SLzdrojak.AddObject(PC.JmenoSys,
                TPomConstraintObject.Create('', 0, ddTabulka_GetTabulkaDef(LTabulka).JmenoSys));
          end;
          Inc(PC);
        end;
      end;
    end;

    { setrid omezeni }

    SLzdrojak.Sort;
    SLdatabaze.Sort;

    { aktualni JmenaSys Pr a Uq omezeni v databazi jsou parametrem }

    { porovnej Pr a Uq omezeni ve zdrojaku a databazi }

    i := 0; j:= 0; Konec := False;

    if (SLzdrojak.Count=0) or (SLdatabaze.Count=0) then Konec := True;

    P := 0; // [TZ] aby kompilator nehazel Warning
    while not Konec do
    begin
//      Application.ProcessMessages;

      if JeRovno({SLzdrojak}i, {SLdatabaze}j) then
      begin
        Inc (i); Inc (j);
      end
      else { nerovnaji se atributy, zkus porovnavat zprava }
      begin
        if ((i+1)<SLzdrojak.Count) { jsem posledni?      }
        then                       { ne, nejsem posledni }
        begin
          p := i+1;
          Konec := False;
          while (not JeRovno({SLzdrojak}p, {SLdatabaze}j)) and (not Konec) do
          //while (((UpperCase(SLzdrojak[p]))<>(UpperCase(SLdatabaze[j]))) and (not Konec)) do
            if ((p+1)<SLzdrojak.Count) then Inc (p)
                                       else Konec := true;
        end
        else Konec := True;        { ano, jsem posledni  }
        if not Konec { NALEZEN stejny atribut }
        then
        begin
          for q := i to p-1 do
            PridejConstraint (SLzdrojak[q]);
          i := p+1; Inc (j);
        end
        else { nerovnaji se atributy, zkus porovnavat z leva }
        begin
          if ((j+1)<SLdatabaze.Count) { jsem posledni?      }
          then                        { ne, nejsem posledni }
          begin
            p := j+1;
            Konec := False;
            while (not JeRovno({SLzdrojak}i, {SLdatabaze}p)) and (not Konec) do
            //while (((UpperCase(SLzdrojak[i]))<>(UpperCase(SLdatabaze[p]))) and (not Konec)) do
              if ((p+1)<SLdatabaze.Count) then Inc (p)
                                          else Konec := True;
          end
          else Konec := True;         { ano, jsem posledni  }
          if not Konec { NALEZEN stejny atribut }
          then
          begin
            for q := j to p-1 do
              OdeberConstraint(SLdatabaze[q], TPomConstraintObject(SLdatabaze.Objects[q]).GetTableName);
            j := p+1; Inc (i);
          end
          else { NENALEZENY shodne atributy, oba zarad a jdi dal }
          begin
            PridejConstraint(SLzdrojak[i]);
            Inc (i);

            OdeberConstraint(SLdatabaze[j], TPomConstraintObject(SLdatabaze.Objects[j]).GetTableName);
            Inc (j);
          end;
        end;
      end;

      if ((SLzdrojak.Count=i) or (SLdatabaze.Count=j)) then Konec := true
                                                       else Konec := false;
    end;

    if ((i+1)<= SLzdrojak.Count) then
      for q:=i to SLzdrojak.Count-1 do
        PridejConstraint(SLzdrojak[q]);

    if ((j+1)<= SLdatabaze.Count) then
      for q:=j to SLdatabaze.Count-1 do
        OdeberConstraint(SLdatabaze[q], TPomConstraintObject(SLdatabaze.Objects[q]).GetTableName);

  finally
    SLzdrojak.Free;
  end;
end;

{------------------------------------------------------------------------------}

function VratTabulkaTrigger (SLtrigger : TStringList; AUpperCase : Boolean = True) : string;
var i : Integer;
    s, r : string; // v s je UpperCase string, v r normalni; pracuji s , r pouze upravuji
begin
  Result := '';

  s := ''; r := '';
  for i:=0 to SLtrigger.Count-1 do
  begin
    s := s + ' ' + UpperCase(SLtrigger[i]);
    r := r + ' ' + SLtrigger[i];
  end;

  i := Pos (' ON ',s); {hledej v create trigger name ON table}
  Delete (s,1,i+3);    {a smaz to - nezajem}
  Delete (r,1,i+3);    {a hledej mezeru za nazvem tabulky}

  i := Pos (' ',s);
  while (i<>0) do
    if i=1
    then
    begin
      Delete (s,1,1);
      Delete (r,1,1);
      i := Pos (' ',s);
    end
    else
    begin
      if AUpperCase then Result := Copy(s,1,i-1)
                    else Result := Copy(r,1,i-1);
      break;
    end;

  //MS 10.7.2006
  Result := StringReplace(Result,'[','',[rfReplaceAll]);
  Result := StringReplace(Result,']','',[rfReplaceAll]);
end;

{------------------------------------------------------------------------------}

function VratNazevTrigger (SLtrigger : TStringList; AUpperCase : Boolean = True) : string;
var i : Integer;
    s, r : string; // v s je UpperCase string, v r normalni; pracuji s , r pouze upravuji
begin
  Result := '';

  s := ''; r := '';
  for i:=0 to SLtrigger.Count-1 do
  begin
    s := s + ' ' + UpperCase(SLtrigger[i]);
    r := r + ' ' + SLtrigger[i];
  end;

  i := Pos ('TRIGGER',s); {hledej trigger}
  Delete (s,1,i);      {a smaz to - nezajem}
  Delete (r,1,i);
  i := Pos (' ',s);    {hledej mezeru pred nazvem triggeru}
  Delete (s,1,i);      {smaz to - nezajem}
  Delete (r,1,i);      {a hledej mezeru za nazvem triggeru}

  i := Pos (' ',s);
  while (i<>0) do
    if i=1
    then
    begin
      Delete (s,1,1);
      Delete (r,1,1);
      i := Pos (' ',s);
    end
    else
    begin
      if AUpperCase then Result := Copy(s,1,i-1)
                    else Result := Copy(r,1,i-1);
      break;
    end;

  //MS 10.7.2006
  Result := StringReplace(Result,'[','',[rfReplaceAll]);
  Result := StringReplace(Result,']','',[rfReplaceAll]);
end;

{ --------------------------------------------------------------------------- }

function _VratNazevX(const AIdentifikator: string; S: string;
  const AOddelovac: string; P: PInteger = nil): string;
  var
    I : Integer; //P - pozice nazvu ve stringu a pouziva se pro doplneni
                 // prefixu dbo - jestlize nazev prefix obsahuje, pak je p=0
begin
  Result := '';

  S := UpperCase(S);

  // hledej identifikator
  I := PosEx(AIdentifikator, S);
  Delete(S, 1, I);
  if P <> nil then P^ := I;

  // hledej mezeru za identifikatorem
  I := PosEx(' ', S);
  Delete(S, 1, I);
  if P <> nil then Inc(P^, I);

  // preskoc vsechny mezery pred nazvem
  I := 0;
  while I < Length(S) do
  begin
    if S[I+1] <> ' ' then Break;
    Inc(I);
  end;
  if I <> 0 then
  begin
    Delete(S, 1, I);
    if P <> nil then Inc(P^, I);
  end;

  // hledej oddelovac
  I := PosEx(AOddelovac, S);
  if I <> 0 then
    Result := TrimRight(Copy(S, 1, I-1));

  //MS 10.7.2006
  Result := StringReplace(Result, '[', '', [rfReplaceAll]);
  Result := StringReplace(Result, ']', '', [rfReplaceAll]);

  if not SameText(Copy(Result, 1, Length(dbo)), dbo) then
    Result := UpperCase(dbo) + Result
  else
    if P <> nil then P^ := 0;
end;

{ --------------------------------------------------------------------------- }

function VratNazevView(S: string; P: PInteger = nil): string;
begin
  Result := _VratNazevX('VIEW', S, ' ', P);
end;

{ --------------------------------------------------------------------------- }

function VratNazevUProc(SLuproc: TStringList; P: PInteger = nil): string;
  var
    I: Integer;
    S: String;
begin
  S := '';
  for I := 0 to SLuproc.Count-1 do
    S := S + ' ' + SLuproc[I];

  Result := _VratNazevX('PROC', S, ' ', P);
end;

{ --------------------------------------------------------------------------- }

function VratNazevUzivFunkce(SLfce: TStringList; P: PInteger = nil): string;
  var
    I: Integer;
    S: String;
begin
  S := '';
  for I := 0 to SLfce.Count-1 do
    S := S + ' ' + SLfce[I];

  Result := _VratNazevX('FUNCTION', S, '(', P);
end;

{ --------------------------------------------------------------------------- }
(*
{$IFDEF Distribuce}
 const
   HeDD_bpl = 'HeDD.bpl';
   G_HeDD_Handle : HMODULE = 0;
 var
   _VratUProc_Text : function (Index: Integer): String = nil;
   _VratUProc_Popis : function (Index: Integer): String = nil;
   _VratUzivFunkci_Text : function (Index: Integer): String = nil;
   _VratUzivFunkci_Popis : function (Index: Integer): String = nil;
   _VratZmenovySkript_PlatiOd : function (Index: Integer): Int64 = nil;
   _VratZmenovySkript_Text : function (Index: Integer): string = nil;
   _VratACTable_Skript : function (const ATabulkaStr: string; aLegislativa: Integer): String = nil;

 procedure LoadHeDD;
 begin
   G_HeDD_Handle := LoadPackage(HeDD_bpl);

   // POZOR, je case-sensitive
   @_VratUProc_Text := GetProcAddress(G_HeDD_Handle, '@Dduproc@_VratUProc_Text$qqri');
   @_VratUProc_Popis := GetProcAddress(G_HeDD_Handle, '@Dduproc@_VratUProc_Popis$qqri');
   if (not Assigned(_VratUProc_Text)) or (not Assigned(_VratUProc_Popis)) then
     raise Exception.Create('chyba ddUProc');

   @_VratUzivFunkci_Text := GetProcAddress(G_HeDD_Handle, '@Ddfunkce@_VratUzivFunkci_Text$qqri');
   @_VratUzivFunkci_Popis := GetProcAddress(G_HeDD_Handle, '@Ddfunkce@_VratUzivFunkci_Popis$qqri');
   if (not Assigned(_VratUzivFunkci_Text)) or (not Assigned(_VratUzivFunkci_Popis)) then
     raise Exception.Create('chyba ddFunkce');

   @_VratZmenovySkript_PlatiOd := GetProcAddress(G_HeDD_Handle, '@Ddzmeny@_VratZmenovySkript_PlatiOd$qqri');
   @_VratZmenovySkript_Text := GetProcAddress(G_HeDD_Handle, '@Ddzmeny@_VratZmenovySkript_Text$qqri');
   if (not Assigned(_VratZmenovySkript_PlatiOd)) or (not Assigned(_VratZmenovySkript_Text)) then
     raise Exception.Create('chyba ddZmeny');

   @_VratACTable_Skript :=
     GetProcAddress(G_HeDD_Handle, '@Ddactable@_VratACTable_Skript$qqrx20System@UnicodeStringi');
   if not Assigned(_VratACTable_Skript) then
     raise Exception.Create('chyba ddACTable');
 end;

 function IsHeDDLoaded: Boolean;
 begin
   Result := (G_HeDD_Handle <> 0);
 end;

 procedure UnloadHeDD;
 begin
   UnloadPackage(G_HeDD_Handle);
   G_HeDD_Handle := 0;
   @_VratUProc_Text := nil;
   @_VratUProc_Popis := nil;
   @_VratUzivFunkci_Text := nil;
   @_VratUzivFunkci_Popis := nil;
   @_VratZmenovySkript_PlatiOd := nil;
   @_VratZmenovySkript_Text := nil;
   @_VratACTable_Skript := nil;
 end;
{$ENDIF Distribuce}
*)
{ --------------------------------------------------------------------------- }
(*
function VratUProc_MinIndex: Integer;
begin
  Result := StrToInt(VratUProcText(-2));
end;

{ --------------------------------------------------------------------------- }

function VratUProc_MaxIndex: Integer;
begin
  Result := StrToInt(VratUProcText(-1));
end;
*)
{ --------------------------------------------------------------------------- }
(*
function VratUProcText(Index: Integer): String;
begin
  Result := {$IFDEF Distribuce}
              ddMain._VratUProc_Text(Index);
            {$ELSE}
              ddUProc._VratUProc_Text(Index);
            {$ENDIF}
end;

{ --------------------------------------------------------------------------- }

function VratUProcPopis(Index: Integer): String;
begin
  Result := {$IFDEF Distribuce}
              ddMain._VratUProc_Popis(Index);
            {$ELSE}
              ddUProc._VratUProc_Popis(Index);
            {$ENDIF}
end;

{ --------------------------------------------------------------------------- }

function VratUzivFunkci_MinIndex: Integer;
begin
  Result := StrToInt(VratUzivFunkciText(-2));
end;

{ --------------------------------------------------------------------------- }

function VratUzivFunkci_MaxIndex: Integer;
begin
  Result := StrToInt(VratUzivFunkciText(-1));
end;

{ --------------------------------------------------------------------------- }

function VratUzivFunkciText(Index: Integer): String;
begin
  Result := {$IFDEF Distribuce}
              ddMain._VratUzivFunkci_Text(Index);
            {$ELSE}
              ddFunkce._VratUzivFunkci_Text(Index);
            {$ENDIF}
end;

{ --------------------------------------------------------------------------- }

function VratUzivFunkciPopis(Index: Integer): String;
begin
  Result := {$IFDEF Distribuce}
              ddMain._VratUzivFunkci_Popis(Index);
            {$ELSE}
              ddFunkce._VratUzivFunkci_Popis(Index);
            {$ENDIF}
end;
*)
{ --------------------------------------------------------------------------- }
(*
function VratACTableSkript(const ATabulkaStr: string; ALegislativa: Integer = -1): String;
  {$IFDEF Distribuce}
  var
    JeNahrano: Boolean;
  {$ENDIF}
begin
  if ALegislativa = -1 then
    ALegislativa := Server.Konstanty.FieldByName(TabHGlob_Legislativa).Value;

  {$IFDEF Distribuce}
  JeNahrano := IsHeDDLoaded;
  if not JeNahrano then LoadHeDD;
  try
    Result := ddMain._VratACTable_Skript(ATabulkaStr, ALegislativa);
  finally
    if not JeNahrano then UnloadHeDD;
  end;
  {$ELSE}
  Result := ddACTable._VratACTable_Skript(ATabulkaStr, ALegislativa);
  {$ENDIF}
end;

{ --------------------------------------------------------------------------- }

function VratACTableSkript(aTabulka: TTabulka; aLegislativa: Integer = -1): String;
begin
  Result := VratACTableSkript(JmenoTabulky(aTabulka), aLegislativa);
end;

{ --------------------------------------------------------------------------- }

function VratZmenovySkript_MinIndex: Integer;
begin
  Result := StrToInt(VratZmenovySkriptText(-2));
end;

{ --------------------------------------------------------------------------- }

function VratZmenovySkript_MaxIndex: Integer;
begin
  Result := StrToInt(VratZmenovySkriptText(-1));
end;

{ --------------------------------------------------------------------------- }

function VratZmenovySkriptPlatiOd(Index: Integer): Int64;
begin
  Result := {$IFDEF Distribuce}
              ddMain._VratZmenovySkript_PlatiOd(Index);
            {$ELSE}
              ddZmeny._VratZmenovySkript_PlatiOd(Index);
            {$ENDIF}
end;

{ --------------------------------------------------------------------------- }

function VratZmenovySkriptText(Index: Integer): string;
begin
  Result := {$IFDEF Distribuce}
              ddMain._VratZmenovySkript_Text(Index);
            {$ELSE}
              ddZmeny._VratZmenovySkript_Text(Index);
            {$ENDIF}
end;
*)
{ --------------------------------------------------------------------------- }

procedure Tx.KontrolujView (SLdatabaze : TStringList);

  procedure PridejView(AViewNazev : string; AViewText : TStringList);
  begin
    PridejDoSL_UProc(AViewNazev,
                     sqlCtiOznam(xViewTohotoNazvuVDBChybi),
                     AViewText.Text);
    PridejDoSL_UProc(AViewNazev,
                     sqlCtiOznam(xNastaveniPravKNovemuView),
                     NastavPravaSkript(AViewNazev, 'V'));
  end;

  procedure ZmenView(AViewNazev : string; AViewText : TStringList);
  begin
    PridejDoSL_UProc(AViewNazev,
                     sqlCtiOznam(xViewSeZmenil),
                     'ALTER '+Copy(AViewText.Text, 8, MaxInt));
  end;

  procedure OdeberView(AViewNazev : String);
  begin
    PridejDoSL_UProc(AViewNazev,
                     sqlCtiOznam(xViewTohotoNazvuVDBPrebyva),
                     Format('DROP VIEW %s', [AViewNazev]));
  end;

  function JeRovno (ViewZdrojakName : string; ViewZdrojakText : TStringList; ViewDatabazeName : string; ViewDatabazeText : TStringList) : Boolean;
  begin
    Result := False;

    if ViewZdrojakName = ViewDatabazeName then
    begin
      if JednoznacnyText(ViewZdrojakText) <> JednoznacnyText(ViewDatabazeText)
      then
      begin
        ZmenView(ViewZdrojakName,ViewZdrojakText);
      end;
      Result := true; { vzdy true, maximalne prohodim tela view }
    end;
  end;

var SLzdrojak  : TStringList;
    ProcSL     : TStringList;
    LTable     : TTabulka;

    i         : integer; { promenna cyklu pro SLzdrojak }
    j         : integer; { promenna cyklu pro SLdatabaze }
    Konec     : Boolean;
    p, q      : integer;

    nazev, s  : string;
    n         : integer;

begin
  SLzdrojak  := TStringList.Create;

  try
    for LTable := OdTabulka to DoTabulka do
//    for LTable:=Succ(Low(LTable)) to High(LTable) do
    begin
      if JeView(LTable) then
      begin
        ProcSL := TStringList.Create;
        ProcSL.Text := VratView(LTable);
        Nazev := VratNazevView(ProcSL.Text,@N);
        if N>0 then
        begin
          S := ProcSL.Text;
          Insert(dbo,S,N);
          ProcSL.Text := S;
        end;
        SLzdrojak.AddObject(Nazev, ProcSL);
      end;
    end;
    { setrid omezeni }

    SLzdrojak.Sort;
    SLdatabaze.Sort;

    { aktualni VIEW v databazi jsou parametrem }

    { porovnej VIEW ve zdrojaku a databazi }

    i := 0; j:= 0; Konec := False;

    if (SLzdrojak.Count=0) or (SLdatabaze.Count=0) then Konec := True;

    P := 0; // [TZ] aby kompilator nehazel Warning

    while not Konec do
    begin
//      Application.ProcessMessages;

      if (JeRovno(SLzdrojak[i],TStringList(SLzdrojak.Objects[i]),SLdatabaze[j],TStringList(SLdatabaze.Objects[j])))
      then
      begin
        Inc (i); Inc (j);
      end
      else { nerovnaji se VIEW, zkus porovnavat z prava }
      begin
        if ((j+1)<SLdatabaze.Count) { jsem posledni?      }
        then                        { ne, nejsem posledni }
        begin
          p := j+1;
          Konec := False;
          while ((not (JeRovno(SLzdrojak[i],TStringList(SLzdrojak.Objects[i]),SLdatabaze[p],TStringList(SLdatabaze.Objects[p])))) and (not Konec)) do
            if ((p+1)<SLdatabaze.Count) then Inc (p)
                                        else Konec := True;
        end
        else Konec := True;         { ano, jsem posledni  }
        if not Konec { NALEZEN stejny atribut }
        then
        begin
{          for q := j to p-1 do
            OdeberView (SLdatabaze[q]); }
          j := p+1; Inc (i);
        end
        else { nerovnaji se URPOC, zkus porovnavat z leva }
        begin
          if ((i+1)<SLzdrojak.Count) { jsem posledni?      }
          then                       { ne, nejsem posledni }
          begin
            p := i+1;
            Konec := False;
            while ((not (JeRovno(SLzdrojak[p],TStringList(SLzdrojak.Objects[p]),SLdatabaze[j],TStringList(SLdatabaze.Objects[j])))) and (not Konec)) do
              if ((p+1)<SLzdrojak.Count) then Inc (p)
                                         else Konec := true;
          end
          else Konec := True;        { ano, jsem posledni  }
          if not Konec { NALEZEN stejny atribut }
          then
          begin
            for q := i to p-1 do
              PridejView (SLzdrojak[q],TStringList(SLzdrojak.Objects[q]));
            i := p+1; Inc (j);
          end
          else { NENALEZENY shodne VIEW, oba zarad a jdi dal }
          begin
{            OdeberView (SLdatabaze[j]);}
            Inc (j);

            PridejView (SLzdrojak[i],TStringList(SLzdrojak.Objects[i]));
            Inc (i);
          end;
        end;
      end;

      if ((SLzdrojak.Count=i) or (SLdatabaze.Count=j)) then Konec := true
                                                       else Konec := false;
    end;

{ neodebirat
    if ((j+1)<= SLdatabaze.Count) then
      for q:=j to SLdatabaze.Count-1 do
        OdeberView (SLdatabaze[q]); }

    if ((i+1)<= SLzdrojak.Count) then
      for q:=i to SLzdrojak.Count-1 do
        PridejView (SLzdrojak[q],TStringList(SLzdrojak.Objects[q]));

  finally
    for i:=0 to SLzdrojak.Count-1 do
      SLzdrojak.Objects[i].Free;

    SLzdrojak.Free;
  end;
end;

{------------------------------------------------------------------------------}

procedure Tx.KontrolujUlozeneProcedury(SLdatabaze: TStringList);
(*
  procedure PridejUProc(AUProcNazev : string; AUprocText : TStringList);
  begin
    PridejDoSL_UProc(AUProcNazev,
                     sqlCtiOznam(xUPTohotoNazvuVDBChybi),
                     AUprocText.Text);
    PridejDoSL_UProc(AUProcNazev,
                     sqlCtiOznam(xNastaveniPravKNoveUP),
                     NastavPravaSkript(AUProcNazev, 'P'));
  end;

  procedure ZmenUProc(AUProcNazev : string; AUprocText : TStringList);
  begin
    PridejDoSL_UProc(AUProcNazev,
                     sqlCtiOznam(xUPSeZmenila),
                     'ALTER '+Copy(AUprocText.Text, 8, MaxInt));
  end;

  procedure OdeberUProc(AUProcNazev : String);
  begin
    PridejDoSL_UProc(AUProcNazev,
                     sqlCtiOznam(xUPTohotoNazvuVDBPrebyva),
                     Format('DROP PROCEDURE %s', [AUProcNazev]));
  end;

  function JeRovno (UProcZdrojakName : string; UProcZdrojakText : TStringList; UProcDatabazeName : string; UProcDatabazeText : TStringList) : Boolean;
  begin
    Result := False;

    if UProcZdrojakName = UProcDatabazeName then
    begin
      if JednoznacnyText(UProcZdrojakText) <> JednoznacnyText(UProcDatabazeText)
//      if Pos(JednoznacnyText (UProcDatabazeText),JednoznacnyText (UProcZdrojakText)) <> 1 {toto neni uplne dobre - viz 255}
      then
      begin
        ZmenUProc(UProcZdrojakName,UProcZdrojakText);
      end;
      Result := true; { vzdy true, maximalne prohodim tela procedur }
    end;
  end;

var SLzdrojak  : TStringList;
    ProcSL     : TStringList;
    LProcedura : Integer;

    i         : integer; { promenna cyklu pro SLzdrojak }
    j         : integer; { promenna cyklu pro SLdatabaze }
    Konec     : Boolean;
    p, q      : integer;

    nazev, s  : string;
    n         : integer;
    {$IFDEF Distribuce}
    JeNahrano : Boolean;
    {$ENDIF}
    *)
begin
(*
  SLzdrojak  := TStringList.Create;

  try
    {$IFDEF Distribuce}
    JeNahrano := IsHeDDLoaded;
    if not JeNahrano then LoadHeDD;
    try
    {$ENDIF Distribuce}
      for LProcedura := VratUProc_MinIndex to VratUProc_MaxIndex do
      begin
        ProcSL := TStringList.Create;
        ProcSL.Text := VratUProcText(LProcedura);
        Nazev := VratNazevUProc(ProcSL,@N);

{$IFDEF Ladit}  // [TZ 17.02.2004]
//test, jestli n·zev uloûenky zaËÌn· na hp
if (Nazev<>'DBO.PREPOCTI_POHYB_STAV')          and //v˝jimky, kterÈ bohuûel uû v minulosti protekly-p¯i rozöi¯ov·nÌ konzultovat s TZ
   (Pos('DBO.CAKO_', Nazev)<>1)                and
   (Pos('DBO.ICV',   Nazev)<>1)                and
   (Pos('DBO.STD_',  Nazev)<>1)                and
   (Pos('DBO.FIA',   Nazev)<>1)                and
   (Nazev<>'DBO.ZAOKROUHLENINAPADESATIHALERE') and
   (Nazev<>'DBO.CLO_GETORGTEXT')               and
   (Nazev<>'DBO.KONTROLADPH_SK')               and
   (Nazev<>'DBO.KONTROLADPHPROSALDO_SK')       and
   (Nazev<>'DBO.KOPIEDOKLADU')
then begin
  if Pos('DBO.HP', Nazev)<>1 then begin
    sqlLadit(Nazev + ' - n·zev uloûenÈ procedury musÌ zaËÌnat na '#1'hp'#1'!');
    Halt;
  end;
end;
{$ENDIF Ladit}

        if N>0 then
        begin
          S := ProcSL.Text;
          Insert(dbo,S,N);
          ProcSL.Text := S;
        end;
        SLzdrojak.AddObject(Nazev, ProcSL);
      end;
    {$IFDEF Distribuce}
    finally
      if not JeNahrano then UnloadHeDD;
    end;
    {$ENDIF Distribuce}

    { setrid omezeni }

    SLzdrojak.Sort;
    SLdatabaze.Sort;

    { aktualni UPROC v databazi jsou parametrem }

    { porovnej UPROC ve zdrojaku a databazi }

    i := 0; j:= 0; Konec := False;

    if (SLzdrojak.Count=0) or (SLdatabaze.Count=0) then Konec := True;

    P := 0; // [TZ] aby kompilator nehazel Warning

    while not Konec do
    begin
//      Application.ProcessMessages;

      if (JeRovno(SLzdrojak[i],TStringList(SLzdrojak.Objects[i]),SLdatabaze[j],TStringList(SLdatabaze.Objects[j])))
      then
      begin
        Inc (i); Inc (j);
      end
      else { nerovnaji se UPROC, zkus porovnavat z prava }
      begin
        if ((j+1)<SLdatabaze.Count) { jsem posledni?      }
        then                        { ne, nejsem posledni }
        begin
          p := j+1;
          Konec := False;
          while ((not (JeRovno(SLzdrojak[i],TStringList(SLzdrojak.Objects[i]),SLdatabaze[p],TStringList(SLdatabaze.Objects[p])))) and (not Konec)) do
            if ((p+1)<SLdatabaze.Count) then Inc (p)
                                        else Konec := True;
        end
        else Konec := True;         { ano, jsem posledni  }
        if not Konec { NALEZEN stejny atribut }
        then
        begin
{          for q := j to p-1 do
            OdeberUProc (SLdatabaze[q]);}
          j := p+1; Inc (i);
        end
        else { nerovnaji se URPOC, zkus porovnavat z leva }
        begin
          if ((i+1)<SLzdrojak.Count) { jsem posledni?      }
          then                       { ne, nejsem posledni }
          begin
            p := i+1;
            Konec := False;
            while ((not (JeRovno(SLzdrojak[p],TStringList(SLzdrojak.Objects[p]),SLdatabaze[j],TStringList(SLdatabaze.Objects[j])))) and (not Konec)) do
              if ((p+1)<SLzdrojak.Count) then Inc (p)
                                         else Konec := true;
          end
          else Konec := True;        { ano, jsem posledni  }
          if not Konec { NALEZEN stejny atribut }
          then
          begin
            for q := i to p-1 do
              PridejUProc (SLzdrojak[q],TStringList(SLzdrojak.Objects[q]));
            i := p+1; Inc (j);
          end
          else { NENALEZENY shodne UPROC, oba zarad a jdi dal }
          begin
{            OdeberUProc (SLdatabaze[j]);}
            Inc (j);

            PridejUProc (SLzdrojak[i],TStringList(SLzdrojak.Objects[i]));
            Inc (i);
          end;
        end;
      end;

      if ((SLzdrojak.Count=i) or (SLdatabaze.Count=j)) then Konec := true
                                                       else Konec := false;
    end;

    if ((i+1)<= SLzdrojak.Count) then
      for q:=i to SLzdrojak.Count-1 do
        PridejUProc (SLzdrojak[q],TStringList(SLzdrojak.Objects[q]));

  finally
    for i:=0 to SLzdrojak.Count-1 do
      SLzdrojak.Objects[i].Free;

    SLzdrojak.Free;
  end;
  *)
end;

{ --------------------------------------------------------------------------- }

procedure Tx.KontrolujUzivFunkce(SLdatabaze: TStringList);
(*
  procedure PridejUProc(AUProcNazev : string; AUprocText : TStringList);
  begin
    PridejDoSL_UProc(AUProcNazev,
                     sqlCtiOznam(xUzivFceTohotoNazvuVDBChybi),
                     AUprocText.Text);
    PridejDoSL_UProc(AUProcNazev,
                     sqlCtiOznam(xNastaveniPravKNoveUzivFci),
                     NastavPravaSkript(AUProcNazev, 'FN'));
  end;

  procedure ZmenUProc(AUProcNazev : string; AUprocText : TStringList);
  begin
    PridejDoSL_UProc(AUProcNazev,
                     sqlCtiOznam(xUzivFceSeZmenila),
                     'ALTER '+Copy(AUprocText.Text, 8, MaxInt));
  end;

  function JeRovno (UProcZdrojakName : string; UProcZdrojakText : TStringList; UProcDatabazeName : string; UProcDatabazeText : TStringList) : Boolean;
  begin
    Result := False;

    if UProcZdrojakName = UProcDatabazeName then
    begin
      if JednoznacnyText(UProcZdrojakText) <> JednoznacnyText(UProcDatabazeText) then
      begin
        ZmenUProc(UProcZdrojakName, UProcZdrojakText);
      end;
      Result := true; { vzdy true, maximalne prohodim tela procedur }
    end;
  end;

var SLzdrojak  : TStringList;
    ProcSL     : TStringList;
    LFunkce : Integer;

    i         : integer; { promenna cyklu pro SLzdrojak }
    j         : integer; { promenna cyklu pro SLdatabaze }
    Konec     : Boolean;
    p, q      : integer;

    nazev, s  : string;
    n         : integer;
    {$IFDEF Distribuce}
    JeNahrano : Boolean;
    {$ENDIF}
    *)
begin
(*
  SLzdrojak  := TStringList.Create;

  try
    {$IFDEF Distribuce}
    JeNahrano := IsHeDDLoaded;
    if not JeNahrano then LoadHeDD;
    try
    {$ENDIF Distribuce}
      for LFunkce := VratUzivFunkci_MinIndex to VratUzivFunkci_MaxIndex do
      begin
        ProcSL := TStringList.Create;
        ProcSL.Text := VratUzivFunkciText(LFunkce);
        Nazev := VratNazevUzivFunkce(ProcSL, @N);

        {$IFDEF Ladit}
        if PosEx('DBO.HF', Nazev) <> 1 then
        begin
          sqlLadit(Nazev + ' - n·zev uûivatelskÈ funkce musÌ zaËÌnat na '#1'hf'#1'!');
          Halt;
        end;
        {$ENDIF Ladit}

        if N > 0 then
        begin
          S := ProcSL.Text;
          Insert(dbo,S,N);
          ProcSL.Text := S;
        end;
        SLzdrojak.AddObject(Nazev, ProcSL);
      end;
    {$IFDEF Distribuce}
    finally
      if not JeNahrano then UnloadHeDD;
    end;
    {$ENDIF Distribuce}

    { setrid omezeni }

    SLzdrojak.Sort;
    SLdatabaze.Sort;

    { aktualni UPROC v databazi jsou parametrem }

    { porovnej UPROC ve zdrojaku a databazi }

    i := 0; j:= 0; Konec := False;

    if (SLzdrojak.Count=0) or (SLdatabaze.Count=0) then Konec := True;

    P := 0; // [TZ] aby kompilator nehazel Warning

    while not Konec do
    begin
//      Application.ProcessMessages;

      if (JeRovno(SLzdrojak[i],TStringList(SLzdrojak.Objects[i]),SLdatabaze[j],TStringList(SLdatabaze.Objects[j])))
      then
      begin
        Inc (i); Inc (j);
      end
      else { nerovnaji se UPROC, zkus porovnavat z prava }
      begin
        if ((j+1)<SLdatabaze.Count) { jsem posledni?      }
        then                        { ne, nejsem posledni }
        begin
          p := j+1;
          Konec := False;
          while ((not (JeRovno(SLzdrojak[i],TStringList(SLzdrojak.Objects[i]),SLdatabaze[p],TStringList(SLdatabaze.Objects[p])))) and (not Konec)) do
            if ((p+1)<SLdatabaze.Count) then Inc (p)
                                        else Konec := True;
        end
        else Konec := True;         { ano, jsem posledni  }
        if not Konec { NALEZEN stejny atribut }
        then
        begin
          j := p+1; Inc (i);
        end
        else { nerovnaji se URPOC, zkus porovnavat z leva }
        begin
          if ((i+1)<SLzdrojak.Count) { jsem posledni?      }
          then                       { ne, nejsem posledni }
          begin
            p := i+1;
            Konec := False;
            while ((not (JeRovno(SLzdrojak[p],TStringList(SLzdrojak.Objects[p]),SLdatabaze[j],TStringList(SLdatabaze.Objects[j])))) and (not Konec)) do
              if ((p+1)<SLzdrojak.Count) then Inc (p)
                                         else Konec := true;
          end
          else Konec := True;        { ano, jsem posledni  }
          if not Konec { NALEZEN stejny atribut }
          then
          begin
            for q := i to p-1 do
              PridejUProc (SLzdrojak[q],TStringList(SLzdrojak.Objects[q]));
            i := p+1; Inc (j);
          end
          else { NENALEZENY shodne UPROC, oba zarad a jdi dal }
          begin
            Inc (j);

            PridejUProc (SLzdrojak[i],TStringList(SLzdrojak.Objects[i]));
            Inc (i);
          end;
        end;
      end;

      if ((SLzdrojak.Count=i) or (SLdatabaze.Count=j)) then
        Konec := true
      else
        Konec := false;
    end;

    if ((i+1)<= SLzdrojak.Count) then
      for q:=i to SLzdrojak.Count-1 do
        PridejUProc (SLzdrojak[q],TStringList(SLzdrojak.Objects[q]));

  finally
    for i:=0 to SLzdrojak.Count-1 do
      SLzdrojak.Objects[i].Free;

    SLzdrojak.Free;
  end;
  *)
end;

{ --------------------------------------------------------------------------- }

procedure Tx.KontrolujTriggers(SLdatabaze : TStringList);

  procedure PridejTrigger (ATrigger : string; ATriggerText : TStringList);
  begin
    PridejDoSL_Tab(VratTabulkaTrigger(ATriggerText, False),
               VratNazevTrigger(ATriggerText),
               sqlCtiOznam(xTriggerVDBChybi),
               ATriggerText.Text,
               TxPridatKonec);
  end;

  procedure OdeberTrigger (ATrigger : string; ATriggerText : TStringList);
    var
      Nazev : string;
  begin
    Nazev := VratNazevTrigger(ATriggerText, False);
    PridejDoSL_Tab(VratTabulkaTrigger(ATriggerText, False),
               Nazev,
               sqlCtiOznam(xTriggerVDBPrebyva),
               Format('DROP TRIGGER %s', [Nazev]),
               TxOdebrat);
  end;

  procedure ZakazanyTrigger (ATrigger : string; ATriggerText : TStringList);
  var Tabulka, Nazev : string;
  begin
    Tabulka := VratTabulkaTrigger(ATriggerText, False);
    Nazev := VratNazevTrigger(ATriggerText, False);
    PridejDoSL_Tab(Tabulka,
               Nazev,
               sqlCtiOznam(xTriggerJeVDBVypnuty),
               Format('ALTER TABLE %s ENABLE TRIGGER %s',[Tabulka, Nazev]),
               TxPridat);
  end;

  procedure AlterTrigger (ATrigger : string; ATriggerText : TStringList);
  begin
    PridejDoSL_Tab(VratTabulkaTrigger(ATriggerText, False),
               VratNazevTrigger(ATriggerText, False),
               sqlCtiOznam(xTriggerVDBSeZmenil),
               Format('ALTER%s',[Copy(ATriggerText.Text,7,Length(ATriggerText.Text)-6)]),
               TxPridatKonec);
  end;

  function JeRovno (TriggZdrojakName : string; TriggZdrojakText : TStringList; TriggDatabazeName : string; TriggDatabazeText : TStringList) : Boolean;
  begin
    Result := False;

    if TriggZdrojakName = TriggDatabazeName then
    begin
      if JednoznacnyText (TriggZdrojakText) <> JednoznacnyText (TriggDatabazeText)
//      if Pos(JednoznacnyText (TriggDatabazeText),JednoznacnyText (TriggZdrojakText)) <> 1 {toto neni uplne dobre - viz 255}
      then
      begin
        AlterTrigger (TriggZdrojakName,TriggZdrojakText);
      end;
      Result := true; { vzdy true, maximalne prohodim tela procedur }

      // kontrola zakazanosti triggeru
      if integer(TriggDatabazeText.Objects[0])=1 then
        ZakazanyTrigger (TriggDatabazeName, TriggDatabazeText);
    end;
  end;

var SLzdrojak : TStringList;
    LTabulka  : TTabulka;
    TR        : PTriggerTabulky;
    PomSL     : TStringList;

    i         : integer; { promenna cyklu pro SLzdrojak }
    j         : integer; { promenna cyklu pro SLdatabaze }
    Konec     : Boolean;
    p, q      : integer;
    S         : String;
begin
  SLzdrojak  := TStringList.Create;

  try
    { prelej vsechny triggery v aktualnim exe do string listu }
    for LTabulka:=OdTabulka to DoTabulka do
    begin
      TR := ddTabulka_GetTabulkaDef(LTabulka).Triggers;
      for j := 1 to ddTabulka_GetTabulkaDef(LTabulka).PocetTriggers do
      begin
        PomSL := TStringList.Create;
        PomSL.Text := _GetTriggerText(TR);

        S := VratNazevTrigger(PomSL);
        SLzdrojak.AddObject(VratTabulkaTrigger(PomSL) + S, PomSL);

        {$IFDEF Ladit}  //[TZ 07.05.2004]
        //test, jestli n·zev triggeru zaËÌn· na ht
        //!!! POZOR V é¡DN…M PÿÕPADÃ NESMÕME DOVOLIT PREFIX et_ <- EXTERNÕ TRIGGERY
        if (S<>'HP_BVH_U_UPRAVHLAVICKU') and //v˝jimky, kterÈ bohuûel uû v minulosti protekly-p¯i rozöi¯ov·nÌ konzultovat s TZ
           (S<>'HP_BVR_IUD_UPRAVHLAVICKU') and
           (S<>'HP_PLANSKOLENI_IU') and
           (S<>'HP_TABBANKSPOJENI_IUD') and
           (S<>'HP_TABCERTIFIKATYR_IUD') and
           (S<>'HP_TABMZKALENDARDNY_U') and
           (S<>'HP_TABMZKALENDARDNYZAM_U') and
           (S<>'HP_TABPLATTUZ_IU') and
           (S<>'HP_TABPLATTUZR_DIU') and
           (S<>'HP_TABPLATTUZVZORR_DIU') and
           (S<>'HP_TABPLATZAHR_DIU') and
           (S<>'HP_TABSKOLICIAKCEH_U') and
           (S<>'HP_TABSKOLICIAKCER_IUD') and
           (S<>'HP_VYNUTKONTROLUSTRUKTURYDB') and
           (S<>'UROVENDEFINICE') then
        begin
          if Pos('HT', S)<>1 then begin
            sqlLadit(S + ' - n·zev triggeru musÌ zaËÌnat na '#1'ht'#1'!');
            Halt;
          end;
        end;
        {$ENDIF Ladit}

        Inc(TR);
      end;
    end;

    { setrid triggery }
    SLzdrojak.Sort;
    SLdatabaze.Sort;

    { porovnej triggery ve zdrojaku a databazi }

    i := 0; j:= 0; Konec := False;

    if (SLzdrojak.Count=0) or (SLdatabaze.Count=0) then Konec := True;

    P := 0; // [TZ] aby kompilator nehazel Warning
    while not Konec do
    begin
//      Application.ProcessMessages;

      if (JeRovno(SLzdrojak[i],TStringList(SLzdrojak.Objects[i]),SLdatabaze[j],TStringList(SLdatabaze.Objects[j])))
      then
      begin
        Inc (i); Inc (j);
      end
      else { nerovnaji se triggery, zkus porovnavat z prava }
      begin
        if ((j+1)<SLdatabaze.Count) { jsem posledni?      }
        then                        { ne, nejsem posledni }
        begin
          p := j+1;
          Konec := False;
          while ((not (JeRovno(SLzdrojak[i],TStringList(SLzdrojak.Objects[i]),SLdatabaze[p],TStringList(SLdatabaze.Objects[p])))) and (not Konec)) do
            if ((p+1)<SLdatabaze.Count) then Inc (p)
                                        else Konec := True;
        end
        else Konec := True;         { ano, jsem posledni  }
        if not Konec { NALEZEN stejny trigger }
        then
        begin
          for q := j to p-1 do
            OdeberTrigger (SLdatabaze[q],TStringList(SLdatabaze.Objects[q]));
          j := p+1; Inc (i);
        end
        else { nerovnaji se triggery, zkus porovnavat z leva }
        begin
          if ((i+1)<SLzdrojak.Count) { jsem posledni?      }
          then                       { ne, nejsem posledni }
          begin
            p := i+1;
            Konec := False;
            while ((not (JeRovno(SLzdrojak[p],TStringList(SLzdrojak.Objects[p]),SLdatabaze[j],TStringList(SLdatabaze.Objects[j])))) and (not Konec)) do
              if ((p+1)<SLzdrojak.Count) then Inc (p)
                                         else Konec := true;
          end
          else Konec := True;        { ano, jsem posledni  }
          if not Konec { NALEZEN stejny trigger }
          then
          begin
            for q := i to p-1 do
              PridejTrigger (SLzdrojak[q],TStringList(SLzdrojak.Objects[q]));
            i := p+1; Inc (j);
          end
          else { NENALEZENY shodne triggery, oba zarad a jdi dal }
          begin
            Odebertrigger (SLdatabaze[j],TStringList(SLdatabaze.Objects[j]));
            Inc (j);

            PridejTrigger (SLzdrojak[i],TStringList(SLzdrojak.Objects[i]));
            Inc (i);
          end;
        end;
      end;

      if ((SLzdrojak.Count=i) or (SLdatabaze.Count=j)) then Konec := true
                                                       else Konec := false;
    end;

    if ((j+1)<= SLdatabaze.Count) then
      for q:=j to SLdatabaze.Count-1 do
        OdeberTrigger (SLdatabaze[q],TStringList(SLdatabaze.Objects[q]));

    if ((i+1)<= SLzdrojak.Count) then
      for q:=i to SLzdrojak.Count-1 do
        PridejTrigger (SLzdrojak[q],TStringList(SLzdrojak.Objects[q]));

  finally
    for i:=0 to SLzdrojak.Count-1 do
      SLzdrojak.Objects[i].Free;

    SLzdrojak.Free;
  end;
end;

function Tx.JednoznacnyText (SLtext : TStringList) : string;
var i : integer;
(*    s : string;*)
begin
  Result := SLtext.Text;

  { Left-trimuj: SPACE,CR,LF }
  I := Length(Result);
  while (I > 0) and ((Result[I]=#10) or (Result[I]=#13) or (Result[I]=#32)) do
    Dec(I);
  Result := Copy(Result, 1, I);
(*
 Result := '';

 s := '';
  for i:=0 to SLtext.Count-1 do
    s := s + ' ' + UpperCase(SLtext[i]);

  i := Pos ('  ',s);
  while i<>0 do
  begin
    Delete (s,i,1);
    i := Pos ('  ',s);
  end;

  if s[1] = ' ' then Delete (s,1,1);
  if s[Length(s)] = ' ' then Delete (s,Length(s),1);

  Result := s;
*)
end;

{------------------------------------------------------------------------------}

function LocStringListCompareText(List: TStringList; Index1, Index2: Integer): Integer;
begin
  Result := CompareText(List[Index1],
                        List[Index2]);
end;

{------------------------------------------------------------------------------}

procedure Tx.KontrolujIndex(SLdatabaze: TStringList);

  procedure PridejIndexLocal(AJmeno: string; ATabulka: string; AAtributy: string; AClustered: Boolean; Rebuilt: Boolean);
  var S, SS, Popis: string;
  begin
    if AClustered then S :=    'CLUSTERED'
                  else S := 'NONCLUSTERED';

    if Rebuilt then begin
      Popis := sqlCtiOznam(xIndexVDbSeZmenil);
      SS    := 'DROP INDEX ' + ATabulka + '.' + AJmeno + ' ';
    end
    else begin
      Popis := sqlCtiOznam(xIndexVDbChybi);
      SS    := '';
    end;

    PridejDoSL_Tab(ATabulka, AJmeno, Popis,
      Format('%sCREATE %s INDEX %s ON %s(%s)', [SS, S, AJmeno, ATabulka, AAtributy]),
     {TxPridat}TxPridatKonec); //[TZ+MS 15.02.2010] kv˘li index˘m nad poËÌtan˝mi sloupci
  end;

  procedure PridejIndex (AJmeno : string; ATabulka : string; AAtributy : string; AClustered : Boolean);
  begin
    PridejIndexLocal (AJmeno, ATabulka, AAtributy, AClustered, False);
  end;

  procedure ZmenIndex (AJmeno : string; ATabulka : string; AAtributy : string; AClustered : Boolean);
  begin
    PridejIndexLocal (AJmeno, ATabulka, AAtributy, AClustered, True);
  end;

  procedure PrejmenujIndex (ATabulka : string; AOldJmeno, ANewJmeno : string);
  begin
    PridejDoSL_Tab(ATabulka, AOldJmeno,
      sqlCtiOznam(xIndexJeVDBPrejmenovan),
      Format('EXEC sp_rename %s, %s, ''INDEX'' ',
             [NQuotedStr(ATabulka + '.' + AOldJmeno),
              NQuotedStr(ANewJmeno)]),
      TxPridat);
  end;

var SLzdrojak : TStringList;
    LTabulka  : TTabulka;
    PC        : PConstraintTabulky;
    i         : integer; { promenna cyklu pro SLzdrojak }
    j         : integer; { promenna cyklu pro SLdatabaze }
    Konec,
    Konec2    : Boolean;
    p, q      : integer;
    Z         : integer;
begin
  SLzdrojak  := TStringList.Create;

  try
    { prelej vsechny indexy v aktualnim exe do string listu }
    for LTabulka:=OdTabulka to DoTabulka do
    begin
      if ddTabulka_GetTabulkaDef(LTabulka).JmenoSys[1]<>'#' then
      begin
        PC := ddTabulka_GetTabulkaDef(LTabulka).Constraints;
        for j := 1 to ddTabulka_GetTabulkaDef(LTabulka).PocetConstraints do
        begin
          case PC.Typ of
            coIndex,
            coIndexClustered : SLzdrojak.AddObject(JmenoTabulky(LTabulka), TIndexKontrola.Create(PC.JmenoSys,PC.Atributy,PC.Typ=coIndexClustered));
          end;
          Inc(PC);
        end;
      end;
    end;

    { setrid indexy }
    { Pozor! metoda Sort uvnitr pouziva AnsiCompareText, ktery porovnava retezce jinak nez CompareText pouzity nize
      Z toho duvodu musi byt pouzita metoda, ktera pro trideni pouzije CompareText }
    SLzdrojak.CustomSort(LocStringListCompareText);
    SLdatabaze.CustomSort(LocStringListCompareText);

    i := 0; j:= 0; Konec := False;

    if (SLzdrojak.Count=0) or (SLdatabaze.Count=0) then Konec := True;

    while not Konec do
    begin
//      Application.ProcessMessages;

      Z := CompareText (SLzdrojak[i],SLdatabaze[j]);
      if Z=0 // stejna tabulka
      then
      begin
        p := i; q := j;
        Konec2 := False;
        while not Konec2 do
        begin
          if CompareText (TIndexKontrola(SLzdrojak.Objects[p]).Atributy,TIndexKontrola(SLdatabaze.Objects[q]).Atributy)=0 then
          begin
            // tady chybi rename
            if CompareText (TIndexKontrola(SLzdrojak.Objects[p]).Jmeno,TIndexKontrola(SLdatabaze.Objects[q]).Jmeno)<>0 then
            begin
              PrejmenujIndex (SLzdrojak[p],TIndexKontrola(SLdatabaze.Objects[q]).Jmeno,TIndexKontrola(SLzdrojak.Objects[p]).Jmeno);
            end;

            if TIndexKontrola(SLzdrojak.Objects[p]).Clustered<>TIndexKontrola(SLdatabaze.Objects[q]).Clustered then
            begin
              with TIndexKontrola(SLzdrojak.Objects[i]) do
                  ZmenIndex (Jmeno, SLzdrojak[i], Atributy, Clustered);
            end;

            SLdatabaze.Objects[q].Free;
            SLdatabaze.Delete(q);
            inc(i);

            Konec2 := True;
          end
          else
          begin
            inc(q);

            if q=SLdatabaze.Count then
            begin
            // index chybi - jsem na konci seznamu
              with TIndexKontrola(SLzdrojak.Objects[i]) do
                PridejIndex (Jmeno, SLzdrojak[i], Atributy, Clustered);
              inc(i);
              Konec2 := True;
            end
            else
              if CompareText (SLzdrojak[p],SLdatabaze[q])<>0 then
              begin
              // index chybi - zmenila se mi tabulka
                with TIndexKontrola(SLzdrojak.Objects[i]) do
                  PridejIndex (Jmeno, SLzdrojak[i], Atributy, Clustered);
                inc(i);
                Konec2 := True;
              end;
          end;
        end;
      end
      else
        if Z<0 then {SLzdrojak < SLdatabaze}
        begin
        // pridat index, ktery chybi v exe
          with TIndexKontrola(SLzdrojak.Objects[i]) do
            PridejIndex (Jmeno, SLzdrojak[i], Atributy, Clustered);
          Inc (i);
        end
        else
        begin
        // ignorovat index, ktery je navic v databazi
          Inc (j);
        end;

      if ((SLzdrojak.Count=i) or (SLdatabaze.Count=j)) then Konec := true
                                                       else Konec := false;
    end;

    if ((i+1)<= SLzdrojak.Count) then
      for q:=i to SLzdrojak.Count-1 do
        with TIndexKontrola(SLzdrojak.Objects[q]) do
          PridejIndex (Jmeno, SLzdrojak[q], Atributy, Clustered);

  finally
    for i:=0 to SLzdrojak.Count-1 do
      SLzdrojak.Objects[i].Free;

    SLzdrojak.Free;
  end;
end;

{ --------------------------------------------------------------------------- }

procedure Tx.DoKontrolaAtributuTabulek;
//  var
//    LSQLpoc: TQueryHe;
//    TABLE_NAME, OldTABLE_NAME: String;
begin
//  LSQLpoc := Server.CreateQuery(nil);
//  try
//    LSQLpoc.ParamCheck := False;
//
//    LSQLpoc.Options.TrimFixedChar := False;
//    LSQLpoc.Options.TrimVarChar := False;
//
//    with TQueryHe(FQuery) do
//    begin
//      SQL.Clear;
//
//      { atributy pouze nepocitane }
//      SQL.Add(
//        Format(
//          'SELECT sysobjects.name,syscolumns.name,'+
//                 'systypes.name,COLUMNPROPERTY(sysobjects.id,syscolumns.name,''IsIdentity''),'+
//                 'syscolumns.isnullable,syscolumns.prec,syscolumns.xprec,syscolumns.scale '+
//          'FROM sysobjects'+
//          '     JOIN syscolumns ON sysobjects.id=syscolumns.id'+
//          '     JOIN systypes ON syscolumns.xusertype=systypes.xusertype '+
//          'WHERE syscolumns.iscomputed=0 and sysobjects.xtype'+
//          {$IFnDEF Ladit}
//          '=''U'''
//          {$Else}
//          ' IN (''U'',''V'')'
//          {$EndIf}
//          +' AND sysobjects.name IN(%s) '+
//          'ORDER BY sysobjects.name',
//          [FWhereTab]));
//
//      { atributy pouze pocitane }
//      LSQLpoc.SQL.Add(
//        Format(
//          'SELECT sysobjects.name,syscolumns.name,'+
//                 'systypes.name,COLUMNPROPERTY(sysobjects.id,syscolumns.name,''IsIdentity''),'+
//                 'syscolumns.isnullable,syscolumns.prec,syscolumns.xprec,syscolumns.scale,'+
//                 'text '+
//          'FROM sysobjects'+
//          '     JOIN syscolumns ON sysobjects.id=syscolumns.id'+
//          '     JOIN systypes ON syscolumns.xusertype=systypes.xusertype '+
//          '     JOIN syscomments ON syscolumns.id=syscomments.id AND syscolumns.colid=syscomments.number '+
//          'WHERE syscolumns.iscomputed=1 and sysobjects.xtype'+
//          // kvuli automaticky generovanym datumum nelze kontrolovat pocitane atributy u view
//          {$IFnDEF Ladit}
//          '=''U'''
//          {$Else}
//          ' IN (''U'',''V'')'
//          {$EndIf}
//          +' AND sysobjects.name IN(%s) '+
//          'ORDER BY sysobjects.name',
//          [FWhereTab]));
//
//      Open;
//      try
//        LSQLpoc.Open;
//        try
//          OldTABLE_NAME := '';
//          while not EOF do
//          begin
//            Server.ProcessMessages;
//
//            TABLE_NAME := Fields[0].AsString;
//            if TABLE_NAME <> OldTABLE_NAME then
//            begin
//              while (not LSQLpoc.EOF) and (OldTABLE_NAME = LSQLpoc.Fields[0].AsString) do
//              begin
//                Server.ProcessMessages;
//
//                Kontrola(LSQLpoc.Fields[1].AsString,    {COLUMN_NAME}
//                         LSQLpoc.Fields[2].AsString,    {DATA_TYPE}
//                         LSQLpoc.Fields[3].AsInteger=1, //not LSQLpoc.Fields[3].IsNULL, //LAutoIncrement, {AutoIncrement}
//                         LSQLpoc.Fields[4].AsInteger=1, {IS_NULLABLE}
//                         LSQLpoc.Fields[5].AsString,    {CHARACTER_OCTET_LENGTH}
//                         LSQLpoc.Fields[6].AsString,    {NUMERIC_PRECISION}
//                         LSQLpoc.Fields[7].AsString,    {NUMERIC_SCALE}
//                         TrimRight(LSQLpoc.Fields[8].AsString) {COMPUTED});
//                LSQLpoc.Next;
//              end;
//
//              SetTabulka(TypTabulky(TABLE_NAME));
//              OldTABLE_NAME := TABLE_NAME;
//            end;
//      (*          if LSQL.Fields[3].IsNULL then LAutoIncrement := ''
//                                     else LAutoIncrement := LSQL.Fields[3].AsString;*)
//            Kontrola(Fields[1].AsString,    {COLUMN_NAME}
//                     Fields[2].AsString,    {DATA_TYPE}
//                     Fields[3].AsInteger=1, //not LSQL.Fields[3].IsNULL,//LAutoIncrement, {AutoIncrement}
//                     Fields[4].AsInteger=1, {IS_NULLABLE}
//                     Fields[5].AsString,    {CHARACTER_OCTET_LENGTH}
//                     Fields[6].AsString,    {NUMERIC_PRECISION}
//                     Fields[7].AsString,    {NUMERIC_SCALE}
//                     '');                        {COMPUTED}
//            Next;
//          end;
//
//          while not LSQLpoc.EOF do
//          begin
//            Server.ProcessMessages;
//
//            Kontrola(LSQLpoc.Fields[1].AsString,    {COLUMN_NAME}
//                     LSQLpoc.Fields[2].AsString,    {DATA_TYPE}
//                     LSQLpoc.Fields[3].AsInteger=1, //not LSQLpoc.Fields[3].IsNULL,//LAutoIncrement, {AutoIncrement}
//                     LSQLpoc.Fields[4].AsInteger=1, {IS_NULLABLE}
//                     LSQLpoc.Fields[5].AsString,    {CHARACTER_OCTET_LENGTH}
//                     LSQLpoc.Fields[6].AsString,    {NUMERIC_PRECISION}
//                     LSQLpoc.Fields[7].AsString,    {NUMERIC_SCALE}
//                     TrimRight(LSQLpoc.Fields[8].AsString) {COMPUTED}
//                     );
//            LSQLpoc.Next;
//          end;
//
//          SetTabulka(tZadna); { vynuceni kontroly posledni tabulky }
//        finally
//          LSQLpoc.Close;
//        end;
//      finally
//        Close;
//      end;
//    end;
//  finally
//    LSQLpoc.Free;
//  end;
end;

{ --------------------------------------------------------------------------- }

procedure Tx.DoKontrolaOmezeni_PK_UQ;
begin
  { kontrola UQ a PK omezeni }
  { zjisti aktualni JmenaSys PK a UQ omezeni v databazi }
//  with TQueryHe(FQuery) do
//  begin
//    SQL.Clear;
//    SQL.Add(
//      Format(
//        'SELECT sysobjects.name, o2.name '+
//        'FROM sysobjects, sysobjects o2 '+
//        'WHERE sysobjects.xtype IN(''UQ'',''PK'') '+
//          'AND o2.id = sysobjects.parent_obj '+
//          'AND o2.name IN(%s)', [FWhereTab]));
//    Open;
//    try
//      FPomSL.Clear;
//      while not EOF do
//      begin
//        FPomSL.AddObject(Fields[0].AsString,
//          TPomConstraintObject.Create('', 0, Fields[1].AsString));
//
//        Next;
//      end;
//
//      { kontroluj PR a UQ omezeni }
//      KontrolujOmezeni_PK_UQ(FPomSL);
//    finally
//      Close;
//    end;
//  end;
end;

{ --------------------------------------------------------------------------- }

procedure Tx.DoKontrolaOmezeni_CK_DF;
//  var
//    II: Integer;
begin
  { kontrola DEFAULTu a CHECKu }
  { zjisti aktualni omezeni v databazi }
//  with TQueryHe(FQuery) do
//  begin
//    SQL.Clear;
//    //[TZ 16.09.2008] prodloûeno ËtenÌ c.text na 2*255 znak˘
//    SQL.Add('SELECT '+
//               {0} 'o.name,'+
//               {1} 'OBJECTPROPERTY(o.id,''CnstIsDisabled''),'+
//               {2} 'o2.name,'+
//               {3} 'c.text '+
//            'FROM sysobjects AS o '+
//              'JOIN syscomments AS c ON o.id=c.id '+
//              'JOIN sysobjects AS o2 ON o2.id=o.parent_obj '+
//            'WHERE o.xtype IN(''D'',''C'')AND o2.name IN(' + FWhereTab + ')');
//    Open;
//    try
//      FPomSL.Clear;
//      try
//        while not EOF do begin
//          FPomSL.AddObject(Fields[0].AsString,
//            TPomConstraintObject.Create(
//              TrimRight(Fields[3].AsString), Fields[1].AsInteger, Fields[2].AsString));
//          Next;
//        end;
//
//        { kontroluj defaulty a checky }
//        KontrolujDefaultyChecky(FPomSL);
//      finally
//        for II := 0 to FPomSL.Count-1 do
//          FPomSL.Objects[II].Free;
//      end;
//    finally
//      Close;
//    end;
//  end;
end;

{ --------------------------------------------------------------------------- }

procedure Tx.DoKontrolaOmezeni_FK;
//  var
//    II: Integer;
//    S: String;
begin
  { kontrola FK omezeni }
  { zjisti aktualni JmenaSys FK omezeni v databazi }
//  with TQueryHe(FQuery) do
//  begin
//    SQL.Clear;
//
//    SQL.Add(
//      Format(
//        'SELECT OBJECTPROPERTY(constid,''CnstIsDisabled''),'#13+  // 00
//               'OBJECT_NAME(constid),'#13+                        // 01
//               'OBJECT_NAME(rkeyid),'#13+                         // 02
//               'COL_NAME(rkeyid,rkey1),'#13+                      // 03
//               'COL_NAME(rkeyid,rkey2),'#13+                      // 04
//               'COL_NAME(rkeyid,rkey3),'#13+                      // 05
//               'COL_NAME(rkeyid,rkey4),'#13+                      // 06
//               'COL_NAME(rkeyid,rkey5),'#13+                      // 07
//               'COL_NAME(rkeyid,rkey6),'#13+                      // 08
//               'COL_NAME(rkeyid,rkey7),'#13+                      // 09
//               'COL_NAME(rkeyid,rkey8),'#13+                      // 10
//               'COL_NAME(rkeyid,rkey9),'#13+                      // 11
//               'COL_NAME(rkeyid,rkey10),'#13+                     // 12
//               'COL_NAME(rkeyid,rkey11),'#13+                     // 13
//               'COL_NAME(rkeyid,rkey12),'#13+                     // 14
//               'COL_NAME(rkeyid,rkey13),'#13+                     // 15
//               'COL_NAME(rkeyid,rkey14),'#13+                     // 16
//               'COL_NAME(rkeyid,rkey15),'#13+                     // 17
//               'COL_NAME(rkeyid,rkey16),'#13+                     // 18
//               'OBJECT_NAME(fkeyid)'#13+                          // 19
//        'FROM sysreferences'#13+
//        'JOIN sysobjects ON fkeyid=id WHERE name IN(%s)', [FWhereTab]));
//
//    Open;
//    try
//      FPomSL.Clear;
//      try
//        while not EOF do
//        begin
//          S := Format('%s %s %s %s %s %s %s %s %s %s %s %s %s %s %s %s',
//                      [Fields[03].AsString, Fields[04].AsString,
//                       Fields[05].AsString, Fields[06].AsString,
//                       Fields[07].AsString, Fields[08].AsString,
//                       Fields[09].AsString, Fields[10].AsString,
//                       Fields[11].AsString, Fields[12].AsString,
//                       Fields[13].AsString, Fields[14].AsString,
//                       Fields[15].AsString, Fields[16].AsString,
//                       Fields[17].AsString, Fields[18].AsString]);
//
//          FPomSL.AddObject(Fields[1].AsString,
//            TPomConstraintFKObject.Create(Fields[2].AsString, S, Fields[0].AsInteger, Fields[19].AsString));
//          Next;
//        end;
//
//        { kontroluj FK omezeni }
//        KontrolujOmezeni_FK(FPomSL);
//      finally
//        for II := 0 to FPomSL.Count-1 do
//          FPomSL.Objects[II].Free;
//      end;
//    finally
//      Close;
//    end;
//  end;
end;

{ --------------------------------------------------------------------------- }

procedure Tx.DoKontrolaView;
//  var
//    II, OldID: Integer;
//    SS, OldSS: String;
//    ProcSL: TStringList;
begin
  { zde je kontrola view }
  { kontrola se provadi jenom v pripade, ze se kontroluji vsechny tabulky, tj. DoTabulka=High(TTabulka)}
  if DoTabulka <> High_TTabulka then Exit;

//  with TQueryHe(FQuery) do
//  begin
//    SQL.Clear;
//    SQL.Add(
//      'SELECT c.id,'                                          + //0
//             'text '                                          + //1
//      'FROM sysobjects JOIN syscomments c ON sysobjects.id=c.id '+
//      'AND sysobjects.xtype=''V'' ORDER BY c.id,c.colid');
//
//    Open;
//    try
//      FPomSL.Clear;
//      try
//        OldID := High(OldID);
//        ProcSL := nil;
//        SS := '';
//        while not EOF do
//        begin
//          Server.ProcessMessages;
//          SS := Fields[1].AsString;
//
//          if OldID <> Fields[0].AsInteger then
//            begin
//              ProcSL := TStringList.Create;
//              ProcSL.Text := SS;
//              FPomSL.AddObject(VratNazevView(ProcSL.Text), ProcSL);
//              OldSS := SS;
//            end
//          else
//            begin
//              OldSS := OldSS + SS;
//              ProcSL.Text := OldSS;
//            end;
//          OldID := Fields[0].AsInteger;
//
//          Next;
//        end;
//
//        { kontroluj ulozene procedury }
//        KontrolujView(FPomSL);
//      finally
//        for II := 0 to FPomSL.Count-1 do
//          FPomSL.Objects[II].Free;
//      end;
//    finally
//      Close;
//    end;
//  end;
end;

{ --------------------------------------------------------------------------- }

procedure Tx.DoKontrolaUlozenychProcedur;
//  var
//    II, OldID: Integer;
//    SS, OldSS: String;
//    ProcSL: TStringList;
begin
  { zde je kontrola ulozenych procedur }
  { kontrola se provadi jenom v pripade, ze se kontroluji vsechny tabulky, tj. DoTabulka=High(TTabulka)}
  if DoTabulka <> High_TTabulka then Exit;

//  with TQueryHe(FQuery) do
//  begin
//    SQL.Clear;
//    SQL.Add(
//      {'SELECT c.id,' + // 0
//             'text ' + // 1
//      'FROM sysobjects '+
//      'JOIN syscomments c ON sysobjects.id = c.id '+
//      'AND sysobjects.xtype = ''P'' '+
//      'ORDER BY c.id, c.colid'}
//      //[TZ 12.09.2009] doËasn· "z·slepka" kv˘li jednÈ datab·zi Mazarsu (HE_Figuera),
//      //kde se projevovala chyba SDACu Access Violation v TMSQuery.Close
//      //dokud SDAC neopravÌ, musÌme takto
//      'SELECT c.id'                        + // 0
//            ',SUBSTRING(c.text,1,2000)'    + // 1
//            ',SUBSTRING(c.text,2001,2000)' + // 2
//     ' FROM dbo.sysobjects AS o'+
//     ' JOIN dbo.syscomments AS c ON o.id=c.id AND o.xtype=''P'''+
//     ' ORDER BY c.id,c.colid');
//
//    Open;
//    try
//      FPomSL.Clear;
//      try
//        OldID := High(OldID);
//        ProcSL := nil;
//        SS := '';
//        while not EOF do
//        begin
//          Server.ProcessMessages;
//          SS := Fields[1].AsString + Fields[2].AsString;
//
//          if OldID <> Fields[0].AsInteger then
//            begin
//              ProcSL := TStringList.Create;
//              ProcSL.Text := SS;
//              FPomSL.AddObject(VratNazevUProc(ProcSL), ProcSL);
//              OldSS := SS;
//            end
//          else
//            begin
//              OldSS := OldSS + SS;
//              ProcSL.Text := OldSS;
//            end;
//          OldID := Fields[0].AsInteger;
//
//          Next;
//        end;
//
//        { kontroluj ulozene procedury }
//        KontrolujUlozeneProcedury(FPomSL);
//
//      finally
//        for II := 0 to FPomSL.Count-1 do
//          FPomSL.Objects[II].Free;
//      end;
//    finally
//      Close;
//    end;
//  end;
end;

{ --------------------------------------------------------------------------- }

procedure Tx.DoKontrolaUzivFunkci;
//  var
//    II, OldID: Integer;
//    SS, OldSS: String;
//    ProcSL: TStringList;
begin
  { zde je kontrola funkci }
  { kontrola se provadi jenom v pripade, ze se kontroluji vsechny tabulky, tj. DoTabulka=High(TTabulka)}
  if DoTabulka <> High_TTabulka then Exit;

//  with TQueryHe(FQuery) do
//  begin
//    SQL.Clear;
//    SQL.Add(
//      'SELECT c.id,' + // 0
//             'text ' + // 1
//      'FROM sysobjects '+
//      'JOIN syscomments c ON sysobjects.id = c.id '+
//      'AND sysobjects.xtype IN(''FN'',''IF'',''TF'') '+
//      'ORDER BY c.id, c.colid');
//
//    Open;
//    try
//      FPomSL.Clear;
//      try
//        OldID := High(OldID);
//        ProcSL := nil;
//        SS := '';
//        while not EOF do
//        begin
//          Server.ProcessMessages;
//          SS := Fields[1].AsString;
//
//          if OldID <> Fields[0].AsInteger then
//            begin
//              ProcSL := TStringList.Create;
//              ProcSL.Text := SS;
//              FPomSL.AddObject(VratNazevUzivFunkce(ProcSL), ProcSL);
//              OldSS := SS;
//            end
//          else
//            begin
//              OldSS := OldSS + SS;
//              ProcSL.Text := OldSS;
//            end;
//          OldID := Fields[0].AsInteger;
//
//          Next;
//        end;
//
//        { kontroluj funkce }
//        KontrolujUzivFunkce(FPomSL);
//
//      finally
//        for II := 0 to FPomSL.Count-1 do
//          FPomSL.Objects[II].Free;
//      end;
//    finally
//      Close;
//    end;
//  end;
end;

{ --------------------------------------------------------------------------- }

procedure Tx.DoKontrolaTriggeru;
//  var
//    II, OldID: Integer;
//    SS, OldSS: String;
//    ProcSL: TStringList;
begin
  { kontrola triggeru }
//  with TQueryHe(FQuery) do
//  begin
//    SQL.Clear;
//    SQL.Add(
//      Format(
//        'SELECT syscomments.id,'                                + //0
//               'OBJECTPROPERTY(sysobjects.id,''ExecIsTriggerDisabled''),' + //1
//               'text '                                          + //2
//        'FROM sysobjects, syscomments, sysobjects o2 '+
//        'WHERE sysobjects.xtype=''TR'' '+
//          'AND sysobjects.id=syscomments.id '+
//          'AND o2.id=sysobjects.parent_obj '+
//          'AND o2.name IN(%s) '+
//          //'AND sysobjects.name<>''et_''+o2.name '+
//          'AND sysobjects.name NOT LIKE %s+o2.name+%s '+
//        'ORDER BY syscomments.id,syscomments.colid',
//        [FWhereTab, NQuotedStr('et_'), NQuotedStr('%')]));
//        // [TZ 07.09.2004] p¯id·n test na ignoraci externÌch trigger˘
//        // [RK 20.11.2007] externi triggery - prevedeno na prefix 'et_<Tabulka>%'
//
//    Open;
//    try
//      FPomSL.Clear;
//      try
//        OldID := High(OldID);
//        ProcSL := nil;
//        SS := '';
//        while not EOF do
//        begin
//          Server.ProcessMessages;
//          SS := Fields[2].AsString;
//          if OldID <> Fields[0].AsInteger then
//            begin
//              ProcSL := TStringList.Create;
//              ProcSL.Text := SS;
//              ProcSL.Objects[0] := Pointer(Fields[1].AsInteger);
//              FPomSL.AddObject(VratTabulkaTrigger(ProcSL)+VratNazevTrigger(ProcSL), ProcSL);
//              OldSS := SS;
//            end
//          else
//            begin
//              OldSS := OldSS + SS;
//              ProcSL.Text := OldSS;
//              ProcSL.Objects[0] := Pointer(Fields[1].AsInteger);
//            end;
//          OldID := Fields[0].AsInteger;
//
//          Next;
//        end;
//
//        { kontroluj triggery }
//        KontrolujTriggers(FPomSL);
//
//      finally
//        for II := 0 to FPomSL.Count-1 do
//          FPomSL.Objects[II].Free;
//      end;
//    finally
//      Close;
//    end;
//  end;
end;

{ --------------------------------------------------------------------------- }

procedure Tx.DoKontrolaIndexu(UseWhereTab: Boolean);
//  var
//    II: Integer;
//    SS: String;
begin
  { kontrola indexu }
//  with TQueryHe(FQuery) do
//  begin
//    SQL.Clear;
//
//    if UseWhereTab then
//      SS := 'IN(' + FWhereTab + ')'
//    else
//      SS := Format('LIKE %s', [NQuotedStr('Tab%')]);
//
//    // [RK 26.03.2009] rozsireni ze 4 na 8 atributu v indexu (saldo potrebovalo 6+rezerva)
//    SQL.Add(
//      'SELECT name'+
//             ',OBJECT_NAME(id)'+
//             ',INDEXPROPERTY(id,name,''IsClustered'')'+
//             ',INDEX_COL(OBJECT_NAME(id),indid,1)'+
//             ',INDEX_COL(OBJECT_NAME(id),indid,2)'+
//             ',INDEX_COL(OBJECT_NAME(id),indid,3)'+
//             ',INDEX_COL(OBJECT_NAME(id),indid,4)'+
//             ',INDEX_COL(OBJECT_NAME(id),indid,5)'+
//             ',INDEX_COL(OBJECT_NAME(id),indid,6)'+
//             ',INDEX_COL(OBJECT_NAME(id),indid,7)'+
//             ',INDEX_COL(OBJECT_NAME(id),indid,8)'#13+
//      'FROM dbo.sysindexes'#13 +
//      'WHERE indid BETWEEN 1 AND 254 '+
//        'AND INDEXPROPERTY(id,name,''IsStatistics'')=0 '+
//        'AND INDEXPROPERTY(id,name,''IsUnique'')=0 '+
//        'AND OBJECT_NAME(id) ' + SS);
//    Open;
//    try
//      FPomSL.Clear;
//      try
//        while not EOF do
//        begin
//          Server.ProcessMessages;
//
//          SS := '';
//          for II := 0 to 7 do
//          begin
//            if not Fields[3+II].IsNull then
//              SS := Format('%s,%s', [SS, Fields[3+II].AsString]);
//          end;
//          System.Delete(SS,1,1); // zrus prvni carku
//          FPomSL.AddObject(Fields[1].AsString, TIndexKontrola.Create(Fields[0].AsString,SS,Fields[2].AsInteger=1));
//
//          Next;
//        end;
//
//        { kontroluj indexy }
//        KontrolujIndex(FPomSL);
//
//      finally
//        for II := 0 to FPomSL.Count-1 do
//          FPomSL.Objects[II].Free;
//      end;
//    finally
//      Close;
//    end;
//  end;
end;

{ --------------------------------------------------------------------------- }

constructor TIndexKontrola.Create(AJmeno, AAtributy : string; AClustered : Boolean);
begin
  Jmeno := AJmeno;
  Atributy := AAtributy;
  Clustered := AClustered;
end;

{******************************************************************************}

{$IFDEF Ladit}
procedure KontrolaUA;
var
  UA : TUzivatelskyTypAtributu;
begin
  for UA := Low(UA) to High(UA) do
  begin
    if UA<>UzivatelskeAtributy[UA].X then
    begin
      MessageBox(0, 'UzivatelskeAtributy', 'LADIT', 0);
      Halt;
    end;
  end;
end;
{$ENDIF}

{ --------------------------------------------------------------------------- }

function UniqueKey(iTabulka: TTabulka): String;
begin
  if ddTabulka_GetTabulkaDef(iTabulka) <> nil then
    Result := ddTabulka_GetTabulkaDef(iTabulka)._FastestUniqueAttr
  else
    Result := '';
end;

{ --------------------------------------------------------------------------- }

function UniqueKey(const aJmenoSysTabulky: String): String;
  var
    DynTab: TDynamickaTabulkaItem;
begin
  if JeObecnyPrehled(aJmenoSysTabulky) then
    begin
      DynTab := GenerujDynamickouTabulku(aJmenoSysTabulky);
      if Assigned(DynTab) then
        Result := DynTab.FTabDef._FastestUniqueAttr
      else
        Result := '';
    end
  else
    Result := UniqueKey(TypTabulky(aJmenoSysTabulky));
end;

{ --------------------------------------------------------------------------- }

function ParametrizovanejUK(ATabulka: TTabulka; AUniqueKey: string = ''): String;
  var
    I, LCount: Integer;
    PomSL: TStringList;
    LJmenoTabulky: String;
  const
    CAndNeAnd: array[Boolean] of String = (')',' AND');
begin
  Result := '';
  PomSL := TStringList.Create;
  try
    LJmenoTabulky := JmenoTabulky(ATabulka);
    if AUniqueKey = '' then
      PomSL.CommaText := UniqueKey(ATabulka)
    else
      PomSL.CommaText := AUniqueKey;

    LCount := PomSL.Count-1;
    if LCount >= 0 then
    begin
      for I := 0 to LCount do
        PomSL[I] := Format('%s.%s=:%1:s%s',
                           [LJmenoTabulky, PomSL[I], CAndNeAnd[I<>LCount]]);
      Result := Format('(%s', [PomSL.Text]);
    end;
  finally
    PomSL.Free;
  end;
end;

{ --------------------------------------------------------------------------- }

procedure GetPrimUniqAttr;
  {...................................................}
  function _JednaTabulka(ATabulka : TTabulka) : String;
  var
    PC : PConstraintTabulky;
    I  : Integer;
    {$IFDEF Ladit}
    SL : TStringList;
    PA : PAtributTabulky;
    {$ENDIF}
  begin
    Result := '';

    PC := ddTabulka_GetTabulkaDef(ATabulka).Constraints;
    for I := 1 to ddTabulka_GetTabulkaDef(ATabulka).PocetConstraints do
    begin
      if ((ATabulka=tDenik) and (PC.Atributy=TabDenik_Id)) or
         ((ATabulka=tCisZam) and (PC.Atributy=TabCisZam_ID)) or
         ((ATabulka=tMzPF) and (PC.Atributy=TabMzPF_Id)) or
         ((ATabulka=tZdrPoj) and (PC.Atributy=TabZdrPoj_Id)) or
         ((ATabulka=tSkupMS) and (PC.Atributy=TabSkupMS_Id)) or
         ((ATabulka=tCisMzSl) and (PC.Atributy=TabCisMzSl_Id)) or
         ((ATabulka=tMzdTar) and (PC.Atributy=TabMzdTar_Id)) or
         ((ATabulka=tMzPaus) and (PC.Atributy=TabMzPaus_Id)) or
         ((ATabulka=tPerSlozZam) and (PC.Atributy=uta_SystemoveCislo)) or
         ((ATabulka=tDuchod) and (PC.Atributy=TabDuchod_Id))
      then
      begin
        // v˝jimka pro TabDenik, aby se nebral jako unik·tnÌ klÌË ID, ale
        // zn·m· Ëtve¯ice IdObdobi, Sbornik, CisloDokladu, Radek - kv˘li
        // problÈm˘m s @@IDENTITY a triggerem na denÌku, kter˝ takÈ vkl·d·
        // do tabulek, kterÈ majÌ atribut typu IDENTITY.
        // NIC - HLEDEJ DALäÕ
        // U Mezd to samÈ (AJ 9.11.2000)
      end
      else
      begin
        case PC.Typ of
          coPrimaryKey : begin { nasel jsem PRIMARY KEY - koncim }
                           Result := PC.Atributy;
                           Exit; {dalsi tabulka}
                         end;
          coUnique     : begin { nasel jsem UNIQUE, zapamatuji si, ale hledam dal jestli neni PRIMARY KEY }
                           if Pos(',', PC.Atributy)=0 then {nenasli carku => jen jeden atribut - koncim}
                           begin
                             Result := PC.Atributy;
                             Exit;
                           end;
                           if Result='' then
                             Result := PC.Atributy;
                         end;
        end;
      end;
      Inc(PC);
    end;
    {$IFDEF Ladit}
     if ATabulka<>tDefaultFitr then { vyjimka pro tuto tabulku - nad ni neni nikdy browse }
     begin
       SL := TStringList.Create;
       try
         SL.CommaText := Result;
         for I := 0 to SL.Count-1 do
         begin
           PA := Atribut(ATabulka, SL[I]);
           if Assigned(PA) then
           begin
             if PA.NULL=nNULL then
             begin
               MessageBox(0, PChar('Atribut unik·tnÌho klÌËe nem˘ûe b˝t NULL - ' + JmenoTabulky(ATabulka) + '.' + PA.JmenoSys), 'LADIT', 0);
//             Halt;
             end;
           end
           else
           begin
             MessageBox(0, 'Unikatni klic neexistuje', 'LADIT', 0);
             Halt;
           end;
         end;
       finally
         SL.Free;
       end;
     end;
    {$ENDIF}
  end;
  {...................................................}
var
  LTabulka : TTabulka;
  TD: PTabulkaDef;
begin
  for LTabulka := Succ(Low(TTabulka)) to High_TTabulka do
  begin
    TD := ddTabulka_GetTabulkaDef(LTabulka);
    if TD._FastestUniqueAttr = '' then  //[TZ 10.12.2003] pokud si nÏkdo vyplnil, knihovny nehledajÌ
      TD._FastestUniqueAttr := _JednaTabulka(LTabulka);
  end;
end;

{------------------------------------------------------------------------------}

procedure TriggersTabulky(ITabulka : TTabulka; OTriggers : TStrings);
var TR    : PTriggerTabulky;
    I     : integer;
    Line  : Integer;
    PomSL : TStringList;
begin
  if Assigned(OTriggers) then
    if (Low(TTabulka)<ITabulka) and (ITabulka<=High_TTabulka) then
    begin
      TR := ddTabulka_GetTabulkaDef(ITabulka).Triggers;
      for I := 1 to ddTabulka_GetTabulkaDef(ITabulka).PocetTriggers do
      begin
        Line := OTriggers.Add('');
        PomSL := TStringList.Create;
        PomSL.Text := _GetTriggerText(TR);

        OTriggers.Objects[Line] := PomSL;
        Inc(TR);
      end;
    end;
end;

{------------------------------------------------------------------------------}

procedure ConstraintyTabulky(ITabulka : TTabulka;
                             OPkUq_SL : TStrings; { primary keys/unique }
                             OCh_SL   : TStrings; { checks              }
                             OFk_SL   : TStrings; { foreign keys        }
                             OIX_SL   : TStrings;
                             OIC_SL   : TStrings
                             );
var
  I  : Integer;
{  I1, I2 : integer;}
{  PA : PAtributTabulky;}
  PC : PConstraintTabulky;
  Line  : Integer;
  PomSL : TStringList;
begin
  if Assigned(OPkUq_SL) and Assigned(OCh_SL) and Assigned(OFk_SL) then
  begin
    if (Low(TTabulka)<ITabulka) and (ITabulka<=High_TTabulka) then
    begin
      { pres atributy@@!! }
(*
      PA := ddTabulka_GetTabulkaDef(ITabulka).Atributy;
      for I := 1 to ddTabulka_GetTabulkaDef(ITabulka).PocetAtributu do
      begin
        case PA.PK_UQ of
          puPrimaryKey,puUnique :
            begin
              if PA.PK_UQ=puPrimaryKey then Line := OPkUq_SL.Add(PrefixyConstraints[coPrimaryKey]+'_'+PA.JmenoSys)
                                       else Line := OPkUq_SL.Add(PrefixyConstraints[coUnique]+'_'+PA.JmenoSys);
              PomSL := TStringList.Create;
              PomSL.Add(PA.JmenoSys);
              PomSL.Add(''{Popis neni});
              OPkUq_SL.Objects[Line] := PomSL;
            end;
        end;

        if PA.CHECK<>'' then
        begin
          Line := OCh_SL.Add(PrefixyConstraints[coCheck]+'_'+PA.JmenoSys);
          PomSL := TStringList.Create;
{          PomSL.Add(PA.JmenoSys);}
          PomSL.Add(PA.CHECK);
          PomSL.Add(''{Popis neni});
          OCh_SL.Objects[Line] := PomSL;
        end;

        if PA.ForeignKey<>'' then
        begin
          Line := OFk_SL.Add(PrefixyConstraints[coForeignKey]+'_'+PA.JmenoSys);
          PomSL := TStringList.Create;
          PomSL.Add(PA.JmenoSys);
          I1 := Pos('(',PA.ForeignKey);
          I2 := Pos(')',PA.ForeignKey);
          PomSL.Add(Copy(PA.ForeignKey,1,I1-1));
          PomSL.Add(Copy(PA.ForeignKey,I1+1,I2-I1-1));
          PomSL.Add(''{Popis neni});
          OFk_SL.Objects[Line] := PomSL;
        end;

        Inc(PA);
      end;
*)
      { pres constrainty }
      PC := ddTabulka_GetTabulkaDef(ITabulka).Constraints;
      for I := 1 to ddTabulka_GetTabulkaDef(ITabulka).PocetConstraints do
      begin
        case PC.Typ of
          coPrimaryKey, coUnique :
          begin
            Line := OPkUq_SL.Add(PC.JmenoSys);
            PomSL := TStringList.Create;
            PomSL.Add(PC.Atributy);
            {$IFDEF SPopisem}
            PomSL.Add(PC.Popis);
            {$ELSE}
            PomSL.Add('');
            {$ENDIF}
            OPkUq_SL.Objects[Line] := PomSL;
          end;
          coCheck       :
          begin
            Line := OCh_SL.Add(PC.JmenoSys);
            PomSL := TStringList.Create;
            PomSL.Add(PC.Vyraz);
            {$IFDEF SPopisem}
            PomSL.Add(PC.Popis);
            {$ELSE}
            PomSL.Add('');
            {$ENDIF}
            OCh_SL.Objects[Line] := PomSL;
          end;
          coForeignKey  :
          begin
            Line := OFk_SL.Add(PC.JmenoSys);
            PomSL := TStringList.Create;
            PomSL.Add(PC.VlastniAtributy);
            PomSL.Add(JmenoTabulky(PC.CiziTabulka));
            PomSL.Add(PC.CiziAtributy);
            {$IFDEF SPopisem}
            PomSL.Add(PC.Popis);
            {$ELSE}
            PomSL.Add('');
            {$ENDIF}
            OFk_SL.Objects[Line] := PomSL;
          end;
          coIndex       :
          begin
            Line := OIX_SL.Add(PC.JmenoSys);
            PomSL := TStringList.Create;
            PomSL.Add(PC.Atributy);
            {$IFDEF SPopisem}
            PomSL.Add(PC.Popis);
            {$ELSE}
            PomSL.Add('');
            {$ENDIF}
            OIX_SL.Objects[Line] := PomSL;
          end;
          coIndexClustered :
          begin
            Line := OIC_SL.Add(PC.JmenoSys);
            PomSL := TStringList.Create;
            PomSL.Add(PC.Atributy);
            {$IFDEF SPopisem}
            PomSL.Add(PC.Popis);
            {$ELSE}
            PomSL.Add('');
            {$ENDIF}
            OIC_SL.Objects[Line] := PomSL;
          end;
        end;

        Inc(PC);
      end;
    end;
  end;
end;

{------------------------------------------------------------------------------}
{ ioJedenOrderBy Trimuje a zbavi ASC nebo DESC }
procedure AnalyzujORDERBY(var ioJedenOrderBy : String; {vstup=' TabX.AtrY DESC ', vystup='TabX.AtrY'}
                          out oTyp           : Boolean {False-DESC, True-ASC});
var
  LPos : Integer;
begin
  ioJedenOrderBy := Trim(ioJedenOrderBy);
  if ioJedenOrderBy<>'' then
  begin
    oTyp   := True; {ASC}

    LPos := Pos(' DESC', UpperCase(ioJedenOrderBy));
    if LPos>3{=Length('T.A')} then
    begin
      oTyp   := False; {DESC}
      ioJedenOrderBy := Copy(ioJedenOrderBy, 1, LPos-1);
    end
    else
    begin
      LPos := Pos(' ASC', UpperCase(ioJedenOrderBy));
      if LPos>3{=Length('T.A')} then
      begin
        ioJedenOrderBy := Copy(ioJedenOrderBy, 1, LPos-1);
      end;
    end;
  end;
end;

{------------------------------------------------------------------------------}

function VerejneJmenoVazby(      aRefTabulka : TTabulka; { referencni tabulka }
                                 aJmenoSys   : String; { muze byt 'Vazba' nebo 'Vazba.Atribut' }
                                 aTakeAttr   : Boolean = False; // ma se vratit i jmeno atributu ?
                           const aRefTabulkaStr: String = ''
                          ) : String;         {         nebo 'Vazba1.Vazba2.Atribut ' }
  var
    PVD: PVztahDef;
    PomStr : String;
    Zbytek : String;
    PP: Integer;
    PA: PAtributTabulky;
    HTStr: String;
begin
  Result := '?';

  PP := Pos('.', aJmenoSys);
  if PP = 0 then
    begin
      PomStr := aJmenoSys;
      Zbytek := '';
    end
  else
    begin
      PomStr := Copy(aJmenoSys, 1, PP-1);
      Zbytek := Copy(aJmenoSys, PP+1, MaxInt);
    end;

  // [RK 28.06.2001]
  // SeznamVazebPodleVazeb obsahuje JmenoSys vsech vazeb (sqVazby)
  if not SeznamVazebPodleVazeb.NajdiVazbu(PomStr, PVD) then
  begin
    // [RK 29.06.2001]
    if aTakeAttr then
    begin
      if PP = 0 then
        begin
          PA := Atribut(aRefTabulka, aJmenoSys); // predpoklad: A
          Result := '';
        end
      else
        begin
          PA := Atribut(aJmenoSys);              // predpoklad: T.A
          Result := ddTabulka_GetTabulkaDef(aRefTabulka).Jmeno + '.';
        end;

      if Assigned(PA) then
        Result := Format('%s%s', [Result, PA.JmenoVerejne])
      else
        Result := Format('%s%s', [Result, aJmenoSys]);
    end;

    Exit;
  end;

  if aRefTabulkaStr <> '' then
    HTStr := aRefTabulkaStr
  else
    HTStr := JmenoTabulky(ARefTabulka);

  case PorovnejTabVeVazbe(ARefTabulka, HTStr, PVD) of
    ksvLeva:
      begin
        Result := PVD.NazevLP;
        if aTakeAttr or (Pos('.', Zbytek) > 0) then
          Result := Format('%s.%s', [Result, VerejneJmenoVazby(PVD.TabP, Zbytek, aTakeAttr, PVD.TabPStr)]);
      end;

    ksvPrava:
      begin
        Result := PVD.NazevPL;
        if aTakeAttr or (Pos('.', Zbytek) > 0) then
          Result := Format('%s.%s', [Result, VerejneJmenoVazby(PVD.TabL, Zbytek, aTakeAttr, PVD.TabLStr)]);
      end
  end;
end;

{ --------------------------------------------------------------------------- }

function BID2Browse(BID : Integer): TBrowse;
  var
    II : TBrowse;
begin
  for II := Low(SeznamVychozichNastaveniBrowse) to High(SeznamVychozichNastaveniBrowse) do
    if SeznamVychozichNastaveniBrowse[II].BID = BID then
    begin
      Result := II;
      Exit;
    end;
  Result := bZadny;
end;

{ --------------------------------------------------------------------------- }

function BrowseDPSN2Browse(const BrowseDPSN: String): TBrowse;
begin
  if JeObecnyPrehled(BrowseDPSN) then
    Result := bx_ObecnyPrehled
  else
    Result := BID2Browse(BrowseDPSN2BrowseID(BrowseDPSN));
end;

{ --------------------------------------------------------------------------- }

function BrowseDPSN2JmenoTabulky(const BrowseDPSN: String): string;
begin
  if JeObecnyPrehled(BrowseDPSN) then
    Result := BrowseDPSN
  else
    Result := JmenoTabulky(SeznamVychozichNastaveniBrowse[BrowseDPSN2Browse(BrowseDPSN)].HlavniTabulka);
end;

{ --------------------------------------------------------------------------- }

function JeToObrazek(PA: PAtributTabulky): Boolean;
begin
  Result := (PA.Typ = taImage) and (dvaObrazek in PA.DalsiVlastnostiAtr);
end;

{ --------------------------------------------------------------------------- }

function JeToBarva(PA: PAtributTabulky): Boolean;
begin
  // pri zmene: sq_Ext.ExtEd_DefiniceExternichAtributu a ddMain.NastavUzivatelskyAtribut
  Result := (PA.Typ = taInt) and (PA.Konverze = '') and
            (dvaBarva in PA.DalsiVlastnostiAtr);
end;

{ --------------------------------------------------------------------------- }

function JeToRTF(PA: PAtributTabulky): Boolean;
begin
  Result := (SkupinaAtributu(PA.Typ) = skpRetezce) and (dvaRTF in PA.DalsiVlastnostiAtr);
end;

{ --------------------------------------------------------------------------- }

procedure InicializaceDD;
const
  Definice_uta_Ozn =
    'CAST(CASE WHEN EXISTS(SELECT*FROM ' + TabUzivOzn +
{0} {!NQuotedStr!}       ' WHERE ' + TabUzivOzn_TabName + '=%s' +
                         ' AND ' + TabUzivOzn_Uzivatel + '=' + sqlLoginName +
{1}                      ' AND ' + TabUzivOzn_IDx + '=%s.' + uta_SystemoveCislo + ')' +
         'THEN 1 ELSE 0 END AS BIT)';

var
  LTabulka : TTabulka;
  I, J     : Integer;
  PA       : PAtributTabulky;
  PC       : PConstraintTabulky;
  SL       : TStringList;
  LVztah   : TVztah;
  {$IFDEF Ladit}
  SLTabEng    : TStringList;
  SLTabAtrEng : TStringList;
  SortedSL    : TStringList;
  SortedVID   : TStringList;
  LokTypAtributu : TTypAtributu;
  LZVztah  : TZakazanyVztah;
  LSum, LLimit  : Extended;
  LPredekPredka : TBrowse;
  LTabulkaMaClusteredIndex: Boolean;
  {$ENDIF Ladit}
  LBrowse  : TBrowse;
  PomStr7  : String;
  CelkovyPocetConstraints   : Integer;
  AtributyPocetConstraints  : Integer;
  I1, I2                    : Integer;
  PocetTextaImageAtributu: Integer;
  PocetAutorZmenil: Integer;
  PocetRozpad_S_N_HAtributu: Integer;
  PocetRozpad_ZS_ZN_ZHAtributu,
  PocetRozpadEuroAtributu      : Integer; { pro generovani pocitanych atributu pro datumy }
  NewDef, OldDef               : PTabulkaDef;
  NasliOstatniPrehledy         : Boolean;
const PocetDatumu = 7;
var
  PA1, OldPA : PAtributTabulky;
  PocetDatAtributu     : integer;
  CelkovyPocetAtributu, OldPocetA : Integer;
  LUzJeClustered                  : Boolean;
  PomS0, PomS1{, PomS2}           : String;
  LVyjimka                        : Boolean;
  LAtribut                        : String;
  SLVyjmute: TStringList;
{$IFDEF Skupina_Tisky_Test}
  S        : String;
  PBrowse  : TBrowse;
{$ENDIF}
{$IFDEF Skupina_Filtry_Test}
  SS       : string;
  PPBrowse : TBrowse;
{$ENDIF}
{$IFDEF EURSK}
  SLeur : TStringList;
{$ENDIF EURSK}
begin
{$IFDEF EURSK}
  SLeur := TStringList.Create;
  try
{$ENDIF EURSK}

  SL := TStringList.Create;
  {$IFDEF Ladit}
  SLTabEng    := TStringList.Create;
  SLTabAtrEng := TStringList.Create;
  {$ENDIF}

  try
  { kontrola tabulek }
  for LTabulka := Succ(Low(TTabulka)) to High_TTabulka do
  begin
     // prevedeni jmena tabulky do spravne jazykove verze
//MS -zatim vyhozeno, zkusebne se vse dela v ZmenaJazyka    NastavJmenoTabulkyPodleJazyka(ddTabulka_GetTabulkaDef(LTabulka));

    {$IFDEF Ladit}
     if ddTabulka_GetTabulkaDef(LTabulka).T <> LTabulka then
     begin
       MessageBox(0, PChar('Chybna kontrola '+ddTabulka_GetTabulkaDef(LTabulka).JmenoSys+'!'), 'LADIT', 0);
       Halt;
     end;

      //<PFA 4.3.2011>
      if (ddTabulka_GetTabulkaDef(LTabulka).JmenoSysEng<>'') then
      begin
        if (SLTabEng.IndexOf(ddTabulka_GetTabulkaDef(LTabulka).JmenoSysEng)<>-1) then
        begin
          MessageBox(0, PChar('duplicita anglickÈho systÈmovÈho jmÈna tabulky '+ddTabulka_GetTabulkaDef(LTabulka).JmenoSysEng+') !'), 'LADIT', 0);
          Halt;
        end;
        SLTabEng.Add(ddTabulka_GetTabulkaDef(LTabulka).JmenoSysEng);
      end;

      PA := ddTabulka_GetTabulkaDef(LTabulka).Atributy;
      SLTabAtrEng.Clear;
      for I := 1 to ddTabulka_GetTabulkaDef(LTabulka).PocetAtributu do
      begin
        //kontrola jen vyplnenych nazvu - pro jistotu pro kazdou tabulku;
        if PA.JmenoSysEng<>'' then
        begin
          if (SLTabAtrEng.IndexOf(PA.JmenoSysEng)<>-1) then
          begin
            MessageBox(0, PChar('duplicita anglickÈho systÈmovÈho jmÈna atributu '+PA.JmenoSysEng+' ('+ddTabulka_GetTabulkaDef(LTabulka).JmenoSys+')!'), 'LADIT', 0);
            Halt;
          end;
          SLTabAtrEng.Add(PA.JmenoSysEng);
        end;
        Inc(PA);
      end;
      //</PFA 4.3.2011>

     if (Copy(ddTabulka_GetTabulkaDef(LTabulka).JmenoSys, 1, 3)<>'Tab') and
        (Copy(ddTabulka_GetTabulkaDef(LTabulka).JmenoSys, 1, 4)<>'#Tab') then
     begin
       MessageBox(0, PChar(ddTabulka_GetTabulkaDef(LTabulka).JmenoSys+' - systÈmovÈ jmÈno tabulky musÌ zaËÌnat prefixem ''Tab'' nebo ''#Tab''!'), 'LADIT', 0);
       Halt;
     end;

     if (ddTabulka_GetTabulkaDef(LTabulka).DefiniceView<>'') and
        (Copy(ddTabulka_GetTabulkaDef(LTabulka).JmenoSys, Length(ddTabulka_GetTabulkaDef(LTabulka).JmenoSys)-3, 4)<>'View') then
     begin
       MessageBox(0, PChar(ddTabulka_GetTabulkaDef(LTabulka).JmenoSys+' - systÈmovÈ jmÈno view musÌ konËit prefixem ''View''!'), 'LADIT', 0);
       Halt;
     end;

     if SameText(Copy(ddTabulka_GetTabulkaDef(LTabulka).JmenoSys, Length(ddTabulka_GetTabulkaDef(LTabulka).JmenoSys)-3, MaxInt), '_EXT') then
     begin
       MessageBox(0, PChar(ddTabulka_GetTabulkaDef(LTabulka).JmenoSys+' - systÈmovÈ jmÈno tabulky nesmÌ konËit na ''_EXT''!'), 'LADIT', 0);
       Halt;
     end;

     if Pos('_', ddTabulka_GetTabulkaDef(LTabulka).JmenoSys)>0 then
     begin
       MessageBox(0, PChar(ddTabulka_GetTabulkaDef(LTabulka).JmenoSys+' - systÈmovÈ jmÈno tabulky nesmÌ obsahovat znak ''_'' !'), 'LADIT', 0);
       Halt;
     end;
   {$ENDIF}

{$IFDEF EURSK}
     if MaTabulkaEURAtribut(LTabulka) and (not JeView(LTabulka)) and (JmenoTabulky(LTabulka)[1]<>'#') then SLeur.Add(JmenoTabulky(LTabulka)); //docasne tabulky a view jeste vyhodit !!!
{$ENDIF EURSK}

    PA := ddTabulka_GetTabulkaDef(LTabulka).Atributy;
    for I := 1 to ddTabulka_GetTabulkaDef(LTabulka).PocetAtributu do
    begin
      if PA.UTyp <> utaZadny then
      begin
        if PA.JmenoSys = ''       then PA.JmenoSys := UzivatelskeAtributy[PA.UTyp].A.JmenoSys;

        if PA.JmenoSysEng = ''       then PA.JmenoSysEng := UzivatelskeAtributy[PA.UTyp].A.JmenoSysEng; //PFA 20110427

        if PA.JmenoVerejneTxt=txtNic    then PA.JmenoVerejneTxt    := UzivatelskeAtributy[PA.UTyp].A.JmenoVerejneTxt;
        if PA.JmenoVerejneZkrTxt=txtNic then PA.JmenoVerejneZkrTxt := UzivatelskeAtributy[PA.UTyp].A.JmenoVerejneZkrTxt;
        if PA.BublinaTxt=txtNic then PA.BublinaTxt := UzivatelskeAtributy[PA.UTyp].A.BublinaTxt;

        if PA.JmenoVerejne=''     then PA.JmenoVerejne    := UzivatelskeAtributy[PA.UTyp].A.JmenoVerejne;
        if PA.JmenoVerejneZkr=''  then PA.JmenoVerejneZkr := UzivatelskeAtributy[PA.UTyp].A.JmenoVerejneZkr;
        if PA.Bublina=''  then PA.Bublina := UzivatelskeAtributy[PA.UTyp].A.Bublina;

        PA.Typ := UzivatelskeAtributy[PA.UTyp].A.Typ;
        if PA.Delka = 0           then PA.Delka        := UzivatelskeAtributy[PA.UTyp].A.Delka;
        if PA.Pocitany = ''       then PA.Pocitany     := UzivatelskeAtributy[PA.UTyp].A.Pocitany;
        if PA.NULL = nNic         then  PA.NULL        := UzivatelskeAtributy[PA.UTyp].A.NULL;
        if PA.PK_UQ = puNic       then PA.PK_UQ        := UzivatelskeAtributy[PA.UTyp].A.PK_UQ;
        if PA.ForeignKey = ''     then PA.ForeignKey   := UzivatelskeAtributy[PA.UTyp].A.ForeignKey;
        if PA.ServerDefault = ''  then PA.ServerDefault:= UzivatelskeAtributy[PA.UTyp].A.ServerDefault;
//      if PA.INSERT=''           then PA.INSERT       := UzivatelskeAtributy[PA.UTyp].A.INSERT;
//      if PA.UPDATE=''           then PA.UPDATE       := UzivatelskeAtributy[PA.UTyp].A.UPDATE;
        if PA.CHECK = ''          then PA.CHECK        := Format(UzivatelskeAtributy[PA.UTyp].A.CHECK, [PA.JmenoSys]);
        if PA.Vyzadovany=vDefault then PA.Vyzadovany := UzivatelskeAtributy[PA.UTyp].A.Vyzadovany;
        if PA.Verejny=vDefault    then PA.Verejny      := UzivatelskeAtributy[PA.UTyp].A.Verejny;

        if PA.KonverzeTxt=txtNic  then PA.KonverzeTxt := UzivatelskeAtributy[PA.UTyp].A.KonverzeTxt;
        if PA.Konverze=''         then PA.Konverze    := UzivatelskeAtributy[PA.UTyp].A.Konverze;

        if PA.SirkaSloupce = 0    then PA.SirkaSloupce := UzivatelskeAtributy[PA.UTyp].A.SirkaSloupce;
        if PA.MaskaDisplay = ''   then PA.MaskaDisplay := UzivatelskeAtributy[PA.UTyp].A.MaskaDisplay;
        if PA.MaskaEdit = ''      then PA.MaskaEdit    := UzivatelskeAtributy[PA.UTyp].A.MaskaEdit;
        // [RK+KM 27.02.2001]
        // Sumovat ma False jako default - v pripade nouze bude nutno predelat na TTristavovyBool
        if not PA.Sumovat         then PA.Sumovat      := UzivatelskeAtributy[PA.UTyp].A.Sumovat;
        if PA.DalsiVlastnostiAtr=[]then PA.DalsiVlastnostiAtr := UzivatelskeAtributy[PA.UTyp].A.DalsiVlastnostiAtr;

        // [RK 29.03.2007] aby se ta definice porad nekopirovala
        if PA.UTyp = utaOzn then
          PA.Pocitany :=
            Format(Definice_uta_Ozn,
                   [NQuotedStr(ddTabulka_GetTabulkaDef(LTabulka).JmenoSys),
                    ddTabulka_GetTabulkaDef(LTabulka).JmenoSys]);
      end;

      // nastaveni podle jazyka - protazeni pres sqlCtiOznam
//MS -zatim vyhozeno, zkusebne se vse dela v ZmenaJazyka    NastavJmenaAtributuPodleJazyka(PA);

      {$IFDEF Ladit}
       if PA.JmenoSys = '' then
       begin
         MessageBox(0, PChar(Format('Atribut Ë. %d musÌ mÌt jmÈno (tabulka:%s)!', [I, JmenoTabulky(LTabulka)])), 'LADIT', 0);
         Halt;
       end;

       if Copy(PA.JmenoSys,1,1) = '_' then
       begin
         MessageBox(0, PChar('Atribut '+PA.JmenoSys+' nesmÌ zaËÌnat na znak ''_'' !'), 'LADIT', 0);
         Halt;
       end;


       if    ((ddTabulka_GetTabulkaDef(LTabulka).T=tCisZam)       and (SameText(PA.JmenoSys, 'Alias')))
          or ((ddTabulka_GetTabulkaDef(LTabulka).T=tCisZamImp)    and (SameText(PA.JmenoSys, 'Alias')))
          or ((ddTabulka_GetTabulkaDef(LTabulka).T=tX_DbVelikost) and (SameText(PA.JmenoSys, 'Rows')))
          or ((ddTabulka_GetTabulkaDef(LTabulka).T=tX_DbVelikost) and (SameText(PA.JmenoSys, 'Data')))
          or ((ddTabulka_GetTabulkaDef(LTabulka).T=tDefTisk)      and (SameText(PA.JmenoSys, 'Collation')))
          or ((ddTabulka_GetTabulkaDef(LTabulka).T=tZmenovyLog)   and (SameText(PA.JmenoSys, 'Old')))
          or ((ddTabulka_GetTabulkaDef(LTabulka).T=tZmenovyLog)   and (SameText(PA.JmenoSys, 'New')))
          {.$IFnDEF ISTbezSBS}
          or ((ddTabulka_GetTabulkaDef(LTabulka).T=tZasilkaRada)  and (SameText(PA.JmenoSys, 'Prefix')))
          or ((ddTabulka_GetTabulkaDef(LTabulka).T=tPoziceRada)   and (SameText(PA.JmenoSys, 'Prefix')))
          {.$ENDIF ISTbezSBS}
       then
         {^^^^ v˝jimky - netestovat na Reserved Keywords - s tim uû se nic ned· dÏlat -
               bylo BY DOBR…, ABY Ué é¡DN… NOV… NEPÿIB›VALY                              }
       else
         if IsSQLReservedKeyword(PA.JmenoSys) then
         begin
           MessageBox(0, PChar('Atribut se nem˘ûe jmenovat ' + PA.JmenoSys +' ('+ddTabulka_GetTabulkaDef(LTabulka).JmenoSys+') - SQL Server Reserved Keyword!'), 'LADIT', 0);
           Halt;
         end;

       if PA.JmenoSysEng<>'' then
       begin
         if ((ddTabulka_GetTabulkaDef(LTabulka).T=tKoopObj) and SameText(PA.JmenoSysEng, '[Order]'))
         then
           {^^^^ v˝jimky - netestovat na Reserved Keywords - s tim uû se nic ned· dÏlat -
                 bylo BY DOBR…, ABY Ué é¡DN… NOV… NEPÿIB›VALY                              }
         else
           if not IsValidIdent(PA.JmenoSysEng) then
           begin
             MessageBox(0, PChar('Atribut nem˘ûe mÌt JmenoSysEng ' + PA.JmenoSysEng +' ('+ddTabulka_GetTabulkaDef(LTabulka).JmenoSys+') - nenÌ platn˝ identifik·tor!'), 'LADIT', 0);
             Halt;
           end;

         if ((ddTabulka_GetTabulkaDef(LTabulka).T=tCisZam)   and SameText(PA.JmenoSysEng, 'Alias'))
         or ((ddTabulka_GetTabulkaDef(LTabulka).T=tKvazby)   and SameText(PA.JmenoSysEng, 'Lower'))
         or ((ddTabulka_GetTabulkaDef(LTabulka).T=tKvazby)   and SameText(PA.JmenoSysEng, 'Position'))
         or ((ddTabulka_GetTabulkaDef(LTabulka).T=tPostup)   and SameText(PA.JmenoSysEng, 'Operation'))
         or ((ddTabulka_GetTabulkaDef(LTabulka).T=tPrPostup) and SameText(PA.JmenoSysEng, 'Operation'))
         or ((ddTabulka_GetTabulkaDef(LTabulka).T=tPrKvazby) and SameText(PA.JmenoSysEng, 'Lower'))
         then
           {^^^^ v˝jimky - netestovat na Reserved Keywords - s tim uû se nic ned· dÏlat -
                 bylo BY DOBR…, ABY Ué é¡DN… NOV… NEPÿIB›VALY                              }
         else
           if IsSQLReservedKeyword(PA.JmenoSysEng) then
           begin
             MessageBox(0, PChar('Atribut nem˘ûe mÌt JmenoSysEng ' + PA.JmenoSysEng +' ('+ddTabulka_GetTabulkaDef(LTabulka).JmenoSys+') - SQL Server Reserved Keyword!'), 'LADIT', 0);
             Halt;
           end;
       end;

       if Pos(ConstraintOddelovac, PA.JmenoSys) <> 0 then
       begin
         MessageBox(0, PChar('Atribut '+PA.JmenoSys+' nem˘ûe obsahovat kombinaci znak˘ '+ConstraintOddelovac+' !'), 'LADIT', 0);
         Halt;
       end;

       // BEM 25.3.2011 tato podminka je nesmyslna, platila pred prechodem na unicode
       {if Length(PA.CHECK)>255 then
       begin
         MessageBox(0, PChar(Format('CHECK nesmÌ b˝t delöÌ neû 255 znak˘ (ShortString) !'#13#13'%s', [PA.CHECK])), 'LADIT', 0);
         Halt;
       end;{}

       if Length(PA.ServerDEFAULT)>255 then
       begin
         MessageBox(0, PChar(Format('ServerDEFAULT nesmÌ b˝t delöÌ neû 255 znak˘ (ShortString) !'#13#13'%s', [PA.ServerDEFAULT])), 'LADIT', 0);
         Halt;
       end;

       if PA.Sumovat and (not MuzeBytSumovat(PA.Typ)) then
       begin
         MessageBox(0, PChar(Format('SUMOVAT nem˘ûe b˝t u tohoto typu nastaveno !'#13#13'%s.%s',
                                    [ddTabulka_GetTabulkaDef(LTabulka).JmenoSys, PA.JmenoSys])), 'LADIT', 0);
         Halt;
       end;
      {$ENDIF}

       // preneseno jinam -> NastavJmenaAtributuPodleJazyka
       // if PA.JmenoVerejne = '' then PA.JmenoVerejne := PA.JmenoSys;

       if PA.NULL = nNic then PA.NULL := nNOTNULL;

(* FM tvrdi, ze je to blbe - proto vypnuto
      {$IFDEF Ladit}
       if (PA.Pocitany<>'') and (PA.NULL<>nNULL) then
       begin
         MessageBox(0, 'PoËÌtan˝ atribut musÌ b˝t typu NULL !', 'LADIT', 0);
         Halt;
       end;
      {$ENDIF}
*)
      {$IFDEF Ladit}
(* [RK 06.10.2010] zruseno - toto byl problem DBLibu
       if (PA.Typ = taBoolean) then
       begin
         if (PA.Pocitany = '') and (ddTabulka_GetTabulkaDef(LTabulka).DefiniceView='') then
         begin
           if (PA.NULL = nNULL) then
           begin
             MessageBox(0, PChar(Format('Atribut %s s typem taBoolean musi byt nNOTNULL - jedna se o chybu Delphi. !',[PA.JmenoSys])), 'LADIT', 0);
             Halt;
           end;
         end;
{                               else musi byt NULL, he zaruceno podminkou vyse }
       end;
*)

      // [RK 25.11.2010] doplnen test na pocitany - kvuli dtCisZam
      if (PA.Pocitany = '') and (PA.Typ in TSkupinaAtributuSDelkou) and (PA.Delka > 255) then
      begin
        MessageBox(0, PChar(Format('Atribut %s typu [N][VAR]CHAR/[VAR]BINARY nemuze b˝t delöÌ neû 255 znak˘. !',[PA.JmenoSys])), 'LADIT', 0);
        Halt;
      end;

      if (PA.Delka <> 0) and (not (PA.Typ in TSkupinaAtributuSDelkou)) then
      begin
        MessageBox(0, PChar(Format('Atribut %s v tabulce %s ma definovanou delku, ale ta se nevyuzije - neni v definici typu vyuzivana. !',[PA.JmenoSys,JmenoTabulky(LTabulka)])), 'LADIT', 0);
        Halt;
      end;

      // [TZ 16.04.2003] hlÌd·me MaskaEdit pro p¯edevöÌm destetinn· ËÌsla (cel· ËÌsla bychom mohli t¯eba povolit)
      //                 kv˘li zmÏn·m v TRxCalcEditHe a TRxDBCalcEditHe, kterÈ dÏl· LD
      if (PA.MaskaEdit<>'') and (SkupinaAtributu(PA.Typ) in [skpCelaCisla, skpDesetinnaCisla]) then begin
        MessageBox(0, PChar(Format('Atribut %s.%s nesmÌ mÌt MaskaEdit !', [JmenoTabulky(LTabulka), PA.JmenoSys])), 'LADIT', 0);
        Halt;
      end;

      if PA.Typ in [taVarChar, taChar, taText] then begin
        MessageBox(0, PChar(Format('NeunikÛdovÈ typy (VARCHAR, CHAR, TEXT) nejsou povoleny: %s.%s!', [JmenoTabulky(LTabulka), PA.JmenoSys])), 'LADIT', 0);
        Halt;
      end;
      {$ENDIF Ladit}

      Inc(PA);
    end;
    { konec kontroly atributu }

    { generovani pocitanych atributu pro datumy }
    PocetDatAtributu             := 0;
    PocetTextaImageAtributu      := 0;
    PocetAutorZmenil             := 0;
    PocetRozpad_S_N_HAtributu    := 0;
    PocetRozpad_ZS_ZN_ZHAtributu := 0;
    PocetRozpadEuroAtributu      := 0;
    PA := ddTabulka_GetTabulkaDef(LTabulka).Atributy;
    for I := 1 to ddTabulka_GetTabulkaDef(LTabulka).PocetAtributu do
    begin
      // [RK 06.12.2011]
      if (PA.Typ = taDateTime) and (ddTabulka_GetTabulkaDef(LTabulka).DefiniceView = '') and
         ((PA.Pocitany = '') or ((PA.Pocitany <> '') and
                                 (dvaDefinovany in PA.DalsiVlastnostiAtr) and
                                 (dvaDatumDefRozpad in PA.DalsiVlastnostiAtr))) then
        Inc(PocetDatAtributu)
      else
      if PA.Pocitany = '' then
      begin
        // [RK 16.01.2001] taText, [RK 15.05.2009] taNText
        if PA.Typ in [taText, taNText] then
          Inc(PocetTextaImageAtributu, 2) // 255 + All
        else // [RK 25.04.2002] taImage
        if JeToObrazek(PA) then
          Inc(PocetTextaImageAtributu, 2) // BGJ + DatLen
        else // [RK 31.08.2006]
        if PA.UTyp in [utaAutor, utaZmenil] then
          Inc(PocetAutorZmenil)
        else // [TZ 13.07.2001]
          begin
            if {(PA.Typ=taInt) and} (dvaRozpad_S_N_H in PA.DalsiVlastnostiAtr) then
              Inc(PocetRozpad_S_N_HAtributu);
            if (dvaRozpad_ZS_ZN_ZH in PA.DalsiVlastnostiAtr) then
              Inc(PocetRozpad_ZS_ZN_ZHAtributu);
          end;
      end;

      // [TZ 16.10.2007]
      if (SkupinaAtributu(PA.Typ) = skpDesetinnaCisla) and (dvaEUR in PA.DalsiVlastnostiAtr) then
        Inc(PocetRozpadEuroAtributu);

      Inc(PA);
    end;

    { je zde alespon jeden "rozpadovy" (datumovy) atribut nebo TEXT nebo IMAGE }
    if (LTabulka=tHGlob) then  //TabHGlob ma vyjimku-nerozpadava se (k cemu by to bylo ?)

    else
    if (PocetDatAtributu<>0) or (PocetTextaImageAtributu<>0) or (PocetAutorZmenil<>0) or
       (PocetRozpad_S_N_HAtributu<>0) or (PocetRozpad_ZS_ZN_ZHAtributu<>0) or
       (PocetRozpadEuroAtributu<>0) then
    begin
      OldPocetA := ddTabulka_GetTabulkaDef(LTabulka).PocetAtributu;
      OldPA := ddTabulka_GetTabulkaDef(LTabulka).Atributy;
      CelkovyPocetAtributu := PocetDatumu * PocetDatAtributu
                              + PocetTextaImageAtributu
                              + PocetAutorZmenil
                              + 4 * PocetRozpad_S_N_HAtributu
                              + 4 * PocetRozpad_ZS_ZN_ZHAtributu
                              + 4 * PocetRozpadEuroAtributu {kazdy EUR atribut se rozpada na 4 dalsi - EUR, DUO + 2x mena}
                              + OldPocetA;
      GetMem(PA1, CelkovyPocetAtributu*SizeOf(TAtributTabulky));
      FillChar(PA1^, CelkovyPocetAtributu*SizeOf(TAtributTabulky), 0);

      PA := ddTabulka_GetTabulkaDef(LTabulka).Atributy;
      ddTabulka_GetTabulkaDef(LTabulka).Atributy := PA1;
      Include(ddTabulka_GetTabulkaDef(LTabulka).DalsiVlastnosti, _dvtLzeFreeMem);
      for I := 1 to ddTabulka_GetTabulkaDef(LTabulka).PocetAtributu do
      begin
        PA1^ := PA^;
        Inc(PA1);

        //tato podmÌnka musÌ b˝t stejn· s podmÌnkou v˝öe,slouûÌcÌ ke zjiöùov·nÌ poËtu takto rozpadl˝ch sloupc˘
        // [RK 06.12.2011] moznost rozpadnout i dvaDefinovane
        if (PA.Typ = taDateTime) and (ddTabulka_GetTabulkaDef(LTabulka).DefiniceView = '') and
           ((PA.Pocitany = '') or ((PA.Pocitany <> '') and
                                   (dvaDefinovany in PA.DalsiVlastnostiAtr) and
                                   (dvaDatumDefRozpad in PA.DalsiVlastnostiAtr))) then
        begin
          // [TZ 30.04.2003] -- nÏkterÈ datumovÈ sloupce se rozpadajÌ do program·torsky definovan˝ch--snÌûenÌ pamÏùovÈ n·roËnosti kompilace trigger˘ a ul.proc. na MSSQL2000
          LVyjimka := ((LTabulka = tDokZbo) and ((PA.JmenoSys = TabDokladyZbozi_DatumMixu) or (PA.JmenoSys = TabDokladyZbozi_DatumTisku)))
                      or
                      ((LTabulka = tDokZboDodatek) and (PA.JmenoSys = TabDokZboDodatek_DatumTisku));

          for J := 1 to PocetDatumu do
          begin
            case J of
              2  : begin
                     PomS0 := sqlMonth;
                     PomS1 := 'MONTH';
                   //PomS2 := 'M';
                   end;
              3  : begin
                     PomS0 := sqlYear;
                     PomS1 := 'YEAR';
                   //PomS2 := 'R';
                   end;
              4  : begin
                     PomS0 := sqlQuarter;
                     PomS1 := 'QUARTER';
                   //PomS2 := 'Q';
                     // [TZ 30.04.2003] program·torsky definovanÈ--v˝raznÈ snÌûenÌ pamÏùovÈ n·roËnosti kompilace trigger˘ a ul.proc. na MSSQL2000
                     {$IFDEF TZ}
                     //PA1.DalsiVlastnostiAtr := [dvaDefinovany]; NEDODÃL¡NO-U DEF. ATR. MUSÕ B›T PREFIX TABULKY !
                     {$ENDIF}
                   end;
              5  : begin
                     PomS0 := sqlWeek;
                     PomS1 := 'WEEK';
                   //PomS2 := 'T';
                     // [TZ 30.04.2003] program·torsky definovanÈ--v˝raznÈ snÌûenÌ pamÏùovÈ n·roËnosti kompilace trigger˘ a ul.proc. na MSSQL2000
                     {$IFDEF TZ}
                     //PA1.DalsiVlastnostiAtr := [dvaDefinovany]; NEDODÃL¡NO-U DEF. ATR. MUSÕ B›T PREFIX TABULKY !
                     {$ENDIF}
                   end;
              6  : begin
                     PomS0 := sqlJenDatum;
                   //PomS1 := '?'; neni
                   //PomS2 := 'Datum bez Ëasu z ';
                   //PomS2 := 'DMR';
                   end;
              // [MS 10.12.2004]
              7 :  begin
                     PomS0 := sqlISOWeek;
                     PomS1 := '';
                   //PomS2 := 'T';
                   //  PA1.DalsiVlastnostiAtr := [dvaDefinovany]; //NEDODÃL¡NO-U DEF. ATR. MUSÕ B›T PREFIX TABULKY !
                   end;
              else begin
                     PomS0 := sqlDay;
                     PomS1 := 'DAY';
                   //PomS2 := 'D';
                   end;
            end;
            PA1.JmenoSys := Format('%s_%s', [PA.JmenoSys,PomS0]);

            //PFA 1.7.2011 - Preklad pro Datum_X
            if (J=6) and (PA.JmenoSysEng<>'') then
              PA1.JmenoSysEng := Format('%s_%s', [PA.JmenoSysEng,PomS0]);

            PA1.Tag := PA.Tag; {!}

           {DÃL¡ SE V PROCEDUÿE ZmenaJazyka
            if PA.JmenoVerejneTxt<>txtNic then PA1.JmenoVerejne := Format('%s (%s)', [sqlCtiOznam(PA.JmenoVerejneTxt), PomS2])
                                          else PA1.JmenoVerejne := Format('%s (%s)', [PA.JmenoVerejne, PomS2]);}


            if J=6 then PA1.Typ := taDateTime
                   else PA1.Typ := taInt;
            PA1.NULL            := nNULL;

            // [RK 06.12.2011]
            if dvaDefinovany in PA.DalsiVlastnostiAtr then
              LAtribut := PA.Pocitany
            else
            // [MS 10.12.2004]
            if LVyjimka or (J=7) or (dvaDatumDefRozpad in PA.DalsiVlastnostiAtr) then
              //!!pokud je to program·torsky definovan˝ atr., tak musÌ b˝t i s prefixem tabulky (jinak hrozÌ Ambiguous column name) a bez []
              LAtribut := ddTabulka_GetTabulkaDef(LTabulka).JmenoSys + '.' + PA.JmenoSys
            else
              LAtribut := '[' + PA.JmenoSys + ']';

            case J of
               6 : PA1.Pocitany := '(CONVERT(DATETIME,CONVERT(INT,CONVERT(FLOAT,'+LAtribut+'))))';
                      //{YUKON}    '(CONVERT([datetime],CONVERT([int],CONVERT([float],'+LAtribut+',0),0),0))';
               7 : PA1.Pocitany := Format(sqlVyrazIsoWeek,[LAtribut]);
              else PA1.Pocitany := '(DATEPART('+PomS1+','+LAtribut+'))';
            end;

            PA1.Verejny := PA.Verejny;
            PA1.SkupinaTxt := PA.SkupinaTxt;

            // [MS 10.12.2004]
            if LVyjimka or (J=7) or (dvaDatumDefRozpad in PA.DalsiVlastnostiAtr) then
              PA1.DalsiVlastnostiAtr := [dvaDefinovany];

            Inc(PA1);
          end;
        end;

        if PA.Pocitany = '' then
        begin
          // [RK 16.01.2001]
          // tato podmÌnka musÌ b˝t stejn· s podmÌnkou v˝öe, slouûÌcÌ ke zjiöùov·nÌ poËtu takto rozpadl˝ch sloupc˘
          if PA.Typ in [taText, taNText] then
          begin
            // 255
            PA1.JmenoSys := Format('%s_' + sqlText255, [PA.JmenoSys]);
            PA1.Tag := PA.Tag; {!}

            // JmenoVerejne SE DÃL¡ V PROCEDUÿE ZmenaJazyka

            if PA.Typ = taText then
              begin
                PA1.Typ := taVarChar;
                PomS0 := 'CHAR';
              end
            else
              begin
                PA1.Typ := taNVarChar;
                PomS0 := 'NCHAR';
              end;
            PA1.Delka := 255;
            PA1.NULL  := PA.NULL;
            // 1.substring je proto, aby se CRLF zbytecne nezamenovalo v cele poznamce
            // 2.substring je proto, aby to byl VARCHAR(255), jinak to je VARCHAR(8000)
            PA1.Pocitany :=
              Format('(SUBSTRING(REPLACE(SUBSTRING(%s.[%s],1,255),(%s(13) + %s(10)),%s(32)),1,255))',
                     [ddTabulka_GetTabulkaDef(LTabulka).JmenoSys, PA.JmenoSys, PomS0, PomS0, PomS0]);
            PA1.Verejny := PA.Verejny;
            PA1.SirkaSloupce := 20;
            PA1.DalsiVlastnostiAtr := [dvaDefinovany];
            PA1.SkupinaTxt := PA.SkupinaTxt;

            Inc(PA1);

            // ALL
            PA1.JmenoSys := Format('%s_' + sqlTextAll, [PA.JmenoSys]);
            PA1.Tag := PA.Tag; {!}
            PA1.Typ := PA.Typ; // taText, taNText
            PA1.NULL := PA.NULL;
            PA1.Pocitany := Format('(%s.%s)', [ddTabulka_GetTabulkaDef(LTabulka).JmenoSys, PA.JmenoSys]);
            PA1.Verejny := PA.Verejny;
            PA1.DalsiVlastnostiAtr := [dvaDefinovany];
            PA1.BublinaTxt := PA.BublinaTxt;
            PA1.SkupinaTxt := PA.SkupinaTxt;

            Inc(PA1);
          end;

          // [RK 31.08.2006]
          // tato podmÌnka musÌ b˝t stejn· s podmÌnkou v˝öe, slouûÌcÌ ke zjiöùov·nÌ poËtu takto rozpadl˝ch sloupc˘
          if PA.UTyp in [utaAutor, utaZmenil] then
          begin
            PA1.JmenoSys := Format('%s_' + sqlSufixLN, [PA.JmenoSys]);
            PA1.Tag := PA.Tag; {!}

            if PA.UTyp = utaAutor then
              begin
                if PA.JmenoVerejneTxt = txtAutor then
                  begin
                    PA1.JmenoVerejneTxt := xAutor_PrihlasovaciJmeno_;
                    PA1.JmenoVerejneZkrTxt := xAutor_PrihlasovaciJmeno_Zkr;
                  end
                else
                  begin
                    PA1.JmenoVerejneTxt := PA.JmenoVerejneTxt;
                    PA1.JmenoVerejneZkrTxt := PA.JmenoVerejneZkrTxt;
                  end;
              end
            else
              begin
                if PA.JmenoVerejneTxt = xZmenil then
                  begin
                    PA1.JmenoVerejneTxt := xZmenil_PrihlasovaciJmeno_;
                    PA1.JmenoVerejneZkrTxt := xZmenil_PrihlasovaciJmeno_Zkr;
                  end
                else
                  begin
                    PA1.JmenoVerejneTxt := PA.JmenoVerejneTxt;
                    PA1.JmenoVerejneZkrTxt := PA.JmenoVerejneZkrTxt;
                  end;
              end;

            PA1.Typ := PA.Typ;
            PA1.Delka := PA.Delka;
            PA1.NULL := PA.NULL;
            PA1.Verejny := PA.Verejny;
            PA1.SirkaSloupce := PA.SirkaSloupce;
            PA1.SkupinaTxt := PA.SkupinaTxt;

            PA1.Pocitany := Format('%s.%s', [ddTabulka_GetTabulkaDef(LTabulka).JmenoSys, PA.JmenoSys]);
            PA1.DalsiVlastnostiAtr := [dvaDefinovany];

            Inc(PA1);
          end;

          // !!! tato podmÌnka musÌ b˝t stejn· s podmÌnkou v˝öe, slouûÌcÌ ke zjiöùov·nÌ poËtu takto rozpadl˝ch sloupc˘ !!!
          // [RK 25.04.2002] - typ obrazku (Bitmap, GIF, JPEG)
          // tabulka tUziv pouziva taImage pro ulozeni prav, ne obrazku
          // [RK 03.10.2007] vyjmuti tRole - pouziva se na prava
          // [RK 06.12.2007] doplneno DatLen
          if JeToObrazek(PA) then
          begin
            // BGJ - typ obrazku
            PA1.JmenoSys := Format('%s_' + sqlTypObrBGJ, [PA.JmenoSys]);
            PA1.Tag := PA.Tag; {!}

            // JmenoVerejne SE DÃL¡ V PROCEDUÿE ZmenaJazyka

            PA1.Typ   := taNVarChar;
            PA1.Delka := 6;
            PA1.NULL  := nNULL;
            PA1.ServerDEFAULT := ''; // jistota
            PA1.CHECK := '';         // jistota
            // Typy obrazku NEMENIT !!! Takto to lze pouzit primo v ReportBuilderu !!!
            // 0x424d = 'BM', 0x474946 = 'GIF',
            // [RK 22.05.2007] doplneny identifikace JPEG a Icon
            // 0Xffd8 = identifikace pro JPEG, 0x00000100 = identifikace pro Icon
            // [RK 03.10.2007] 0x89504e470d0a1a0a = PNG identifikace
            // pri zmene kontrola v Obrazek_GetGraphicClassStr()
            PA1.Pocitany :=
              Format(
                '(case when ([%0:s] is null) then NULL else '+
                '(case when (substring([%0:s],1,2) = 0x424d) then N''Bitmap'' else '+
                '(case when (substring([%0:s],1,3) = 0x474946) then N''GIF'' else '+
                '(case when (substring([%0:s],1,2) = 0xffd8) then N''JPEG'' else '+
                '(case when (substring([%0:s],1,4) = 0x00000100) then N''Icon'' else '+
                '(case when (substring([%0:s],1,8) = 0x89504E470D0A1A0A) then N''PNG'' else '+
                '(case when (substring([%0:s],1,4) = 0x54494646) then N''TIFF'' else '+
                ''''' end) end) end) end) end) end) end)',
//                {YUKON}
//                '(case when [%0:s] IS NULL then NULL else'+
//                ' case when substring([%0:s],(1),(2))=0x424D then N''Bitmap'' else'+
//                ' case when substring([%0:s],(1),(3))=0x474946 then N''GIF'' else'+
//                ' case when substring([%0:s],(1),(2))=0xFFD8 then N''JPEG'' else'+
//                ' case when substring([%0:s],(1),(4))=0x00000100 then N''Icon'' else'+
//                ' case when substring([%0:s],(1),(8))=0x89504E470D0A1A0A then N''PNG'' else'+
//                ' case when substring([%0:s],(1),(4))=0x54494646 then N''TIFF'' else'+
//                ' '''' end end end end end end end)',
                [PA.JmenoSys]);
            PA1.Verejny := vTrue;//PA.Verejny;
            PA1.DalsiVlastnostiAtr := [];
            PA1.SkupinaTxt := PA.SkupinaTxt;

            Inc(PA1);

            // DatLen - velikost v DB
            PA1.JmenoSys := Format('%s_' + sqlObrDatLen, [PA.JmenoSys]);
            PA1.Tag := PA.Tag; {!}

            // JmenoVerejne SE DÃL¡ V PROCEDUÿE ZmenaJazyka

            PA1.Typ  := taInt;
            PA1.NULL := nNOTNULL;
            PA1.ServerDEFAULT := ''; // jistota
            PA1.CHECK := '';         // jistota
            PA1.Pocitany := '(isnull(datalength([' + PA.JmenoSys + ']),0))';
            //{YUKON}PA1.Pocitany := '(isnull(datalength([' + PA.JmenoSys + ']),(0)))';
            PA1.Verejny := vTrue; //PA.Verejny;
            PA1.MaskaDisplay := ',##0';
            PA1.Sumovat := True;
            PA1.DalsiVlastnostiAtr := [];
            PA1.SkupinaTxt := PA.SkupinaTxt;

            Inc(PA1);
          end;

          // [TZ 13.07.2001]
          //tato podmÌnka musÌ b˝t stejn· s podmÌnkou v˝öe,slouûÌcÌ ke zjiöùov·nÌ poËtu takto rozpadl˝ch sloupc˘
          if {(PA.Typ=taInt) and} (dvaRozpad_S_N_H in PA.DalsiVlastnostiAtr) then
          begin
            // Typ
            PA1.JmenoSys := Format('%s_' + sqlRozpadTyp, [PA.JmenoSys]);
            PA1.Tag := PA.Tag; {!}
           {if PA.JmenoVerejneTxt <> txtNic then PA1.JmenoVerejne := Format('%s - ' + sqlText255, [sqlCtiOznam(PA.JmenoVerejneTxt)])
           -DÃL¡ SE V PROCEDUÿE ZmenaJazyka-else PA1.JmenoVerejne := Format('%s - ' + sqlText255, [PA.JmenoVerejne]);}
            PA1.Typ           := taByte;
            PA1.NULL          := nNOTNULL;
            PA1.ServerDEFAULT := '1';
            PA1.CHECK         := 'BETWEEN 0 AND 2';
            PA1.Verejny       := PA.Verejny;
            PA1.KonverzeTxt   := xKonverze_0_sec_1_min_2_hod;
            PA1.SirkaSloupce  := 4;
            PA1.SkupinaTxt    := PA.SkupinaTxt;
            Inc(PA1);

            // sec
            PA1.JmenoSys := Format('%s_' + sqlRozpadSec, [PA.JmenoSys]);
            PA1.Tag := PA.Tag; {!}
            PA1.Typ   := taNumeric_19_6;
            PA1.NULL  := nNULL;
            PA1.Pocitany := Format('(CONVERT(NUMERIC(19,6),([%0:s] * CASE [%0:s_'+sqlRozpadTyp+'] WHEN 0 THEN 1.0 WHEN 1 THEN 60.0 WHEN 2 THEN 3600.0 END)))', [PA.JmenoSys]);
            PA1.Verejny := PA.Verejny;
            PA1.SirkaSloupce := 8;
            PA1.Sumovat      := True;
            PA1.SkupinaTxt   := PA.SkupinaTxt;
          //PA1.DalsiVlastnostiAtr := [dvaDefinovany];
            Inc(PA1);

            // min
            PA1.JmenoSys := Format('%s_' + sqlRozpadMin, [PA.JmenoSys]);
            PA1.Tag := PA.Tag; {!}
            PA1.Typ   := taNumeric_19_6;
            PA1.NULL  := nNULL;
            PA1.Pocitany := Format('(CONVERT(NUMERIC(19,6),CASE [%0:s_'+sqlRozpadTyp+'] WHEN 0 THEN ([%0:s] / 60.0) WHEN 1 THEN [%0:s] WHEN 2 THEN (60.0 * [%0:s]) END))', [PA.JmenoSys]);
            PA1.Verejny := PA.Verejny;
            PA1.SirkaSloupce := 8;
            PA1.Sumovat      := True;
            PA1.SkupinaTxt   := PA.SkupinaTxt;
          //PA1.DalsiVlastnostiAtr := [dvaDefinovany];
            Inc(PA1);

            // hod
            PA1.JmenoSys := Format('%s_' + sqlRozpadHod, [PA.JmenoSys]);
            //PFA 13.7.2011 - Preklad pro hod_X
            if (PA.JmenoSysEng<>'') then
              PA1.JmenoSysEng := Format('%s_' + sqlRozpadHod, [PA.JmenoSysEng]);
            PA1.Tag := PA.Tag; {!}
            PA1.Typ   := taNumeric_19_6;
            PA1.NULL  := nNULL;
            PA1.Pocitany := Format('(CONVERT(NUMERIC(19,6),([%0:s] / CASE [%0:s_'+sqlRozpadTyp+'] WHEN 0 THEN 3600.0 WHEN 1 THEN 60.0 WHEN 2 THEN 1.0 END)))', [PA.JmenoSys]);
            PA1.Verejny := PA.Verejny;
            PA1.SirkaSloupce := 8;
            PA1.Sumovat      := True;
            PA1.SkupinaTxt   := PA.SkupinaTxt;
          //PA1.DalsiVlastnostiAtr := [dvaDefinovany];
            Inc(PA1);
          end;

          // [TZ 19.07.2001]
          //tato podmÌnka musÌ b˝t stejn· s podmÌnkou v˝öe,slouûÌcÌ ke zjiöùov·nÌ poËtu takto rozpadl˝ch sloupc˘
          if dvaRozpad_ZS_ZN_ZH in PA.DalsiVlastnostiAtr then
          begin
            // Typ
            PA1.JmenoSys := Format('%s_' + sqlRozpadZaTyp, [PA.JmenoSys]);
            PA1.Tag := PA.Tag; {!}
           {if PA.JmenoVerejneTxt <> txtNic then PA1.JmenoVerejne := Format('%s - ' + sql..., [sqlCtiOznam(PA.JmenoVerejneTxt)])
           -DÃL¡ SE V PROCEDUÿE ZmenaJazyka-else PA1.JmenoVerejne := Format('%s - ' + sql..., [PA.JmenoVerejne]);}
            PA1.Typ           := taByte;
            PA1.NULL          := nNOTNULL;
            PA1.ServerDEFAULT := '1';
            PA1.CHECK         := 'BETWEEN 0 AND 2';
            PA1.Verejny       := PA.Verejny;
            PA1.KonverzeTxt   := xKonverze_0_za_sec_1_za_min_2_za_hod;
            PA1.SirkaSloupce  := 7;
            PA1.SkupinaTxt    := PA.SkupinaTxt;
            Inc(PA1);

            // sec
            PA1.JmenoSys := Format('%s_' + sqlRozpadZaSec, [PA.JmenoSys]);
            PA1.Tag := PA.Tag; {!}
            PA1.Typ   := taNumeric_19_6;
            PA1.NULL  := nNULL;
            PA1.Pocitany := Format('(CONVERT(NUMERIC(19,6),([%0:s] / CASE [%0:s_'+sqlRozpadZaTyp+'] WHEN 0 THEN 1.0 WHEN 1 THEN 60.0 WHEN 2 THEN 3600.0 END)))', [PA.JmenoSys]);
            PA1.Verejny := PA.Verejny;
            PA1.SirkaSloupce := 8;
            PA1.Sumovat      := True;
            PA1.SkupinaTxt   := PA.SkupinaTxt;
            Inc(PA1);

            // min
            PA1.JmenoSys := Format('%s_' + sqlRozpadZaMin, [PA.JmenoSys]);
            PA1.Tag := PA.Tag; {!}
            PA1.Typ   := taNumeric_19_6;
            PA1.NULL  := nNULL;
            PA1.Pocitany := Format('(CONVERT(NUMERIC(19,6),CASE [%0:s_'+sqlRozpadZaTyp+'] WHEN 0 THEN (60.0 * [%0:s]) WHEN 1 THEN [%0:s] WHEN 2 THEN ([%0:s] / 60.0) END))', [PA.JmenoSys]);
            PA1.Verejny := PA.Verejny;
            PA1.SirkaSloupce := 8;
            PA1.Sumovat      := True;
            PA1.SkupinaTxt   := PA.SkupinaTxt;
            Inc(PA1);

            // hod
            PA1.JmenoSys := Format('%s_' + sqlRozpadZaHod, [PA.JmenoSys]);
            PA1.Tag := PA.Tag; {!}
            PA1.Typ   := taNumeric_19_6;
            PA1.NULL  := nNULL;
            PA1.Pocitany := Format('(CONVERT(NUMERIC(19,6),([%0:s] * CASE [%0:s_'+sqlRozpadZaTyp+'] WHEN 0 THEN 3600.0 WHEN 1 THEN 60.0 WHEN 2 THEN 1.0 END)))', [PA.JmenoSys]);
            PA1.Verejny := PA.Verejny;
            PA1.SirkaSloupce := 8;
            PA1.Sumovat      := True;
            PA1.SkupinaTxt   := PA.SkupinaTxt;
            Inc(PA1);
          end;
        end;

        // [TZ 16.10.2007]
        //tato podmÌnka musÌ b˝t stejn· s podmÌnkou v˝öe,slouûÌcÌ ke zjiöùov·nÌ poËtu takto rozpadl˝ch sloupc˘
        if (SkupinaAtributu(PA.Typ)=skpDesetinnaCisla) and (dvaEUR in PA.DalsiVlastnostiAtr) then
        begin
          { EUR }
          PA1.JmenoSys := PA.JmenoSys + '_' + dvaEURrozpadE;
          PA1.Tag := PA.Tag; {!}

          // JmenoVerejne SE DÃL¡ V PROCEDUÿE ZmenaJazyka

          PA1.Typ   := PA.Typ;
          PA1.NULL  := nNULL;
          PA1.Pocitany := 'NULL';  //p¯i¯azuje se dynamicky v PridejRozpadNaEuro(), pro jistotu sem d·v·m NULL
          PA1.Verejny := vFalse;   //p¯i¯azuje se dynamicky v PridejRozpadNaEuro()
          PA1.SirkaSloupce := PA.SirkaSloupce;
          PA1.DalsiVlastnostiAtr := PA.DalsiVlastnostiAtr + [dvaDefinovany, _dvaEURRozpad] - [dvaEUR];
          PA1.MaskaDisplay := PA.MaskaDisplay; // [RK 28.04.2009]
          PA1.SkupinaTxt   := PA.SkupinaTxt;

          Inc(PA1);

          { DUO }
          PA1.JmenoSys := PA.JmenoSys + '_' + dvaEURrozpadD;
          PA1.Tag := PA.Tag; {!}

          // JmenoVerejne SE DÃL¡ V PROCEDUÿE ZmenaJazyka

          PA1.Typ   := PA.Typ;
          PA1.NULL  := nNULL;
          PA1.Pocitany := 'NULL';  //p¯i¯azuje se dynamicky v PridejRozpadNaEuro(), pro jistotu sem d·v·m NULL
          PA1.Verejny := vFalse;   //p¯i¯azuje se dynamicky v PridejRozpadNaEuro()
          PA1.SirkaSloupce := PA.SirkaSloupce;
          PA1.DalsiVlastnostiAtr := PA.DalsiVlastnostiAtr + [dvaDefinovany, _dvaEURRozpad] - [dvaEUR];
          PA1.MaskaDisplay := PA.MaskaDisplay; // [RK 28.04.2009]
          PA1.SkupinaTxt   := PA.SkupinaTxt;

          Inc(PA1);

          { Mena puvodniho sloupce }
          PA1.JmenoSys := PA.JmenoSys + '_' + dvaEURrozpadM;
          PA1.Tag := PA.Tag; {!}

          // JmenoVerejne SE DÃL¡ V PROCEDUÿE ZmenaJazyka

          PA1.Typ   := taNVarchar; //PA.Typ;
          PA1.Delka := 3;
          PA1.NULL  := nNULL;
          PA1.Pocitany := 'NULL';  //p¯i¯azuje se dynamicky v PridejRozpadNaEuro(), pro jistotu sem d·v·m NULL
          PA1.Verejny := vFalse;   //p¯i¯azuje se dynamicky v PridejRozpadNaEuro()
          PA1.SirkaSloupce := 3;
          PA1.DalsiVlastnostiAtr := PA.DalsiVlastnostiAtr + [dvaDefinovany, _dvaEURRozpad] - [dvaEUR];
          PA1.SkupinaTxt   := PA.SkupinaTxt;

          Inc(PA1);

          { Mena DUO sloupce }
          PA1.JmenoSys := PA.JmenoSys + '_' + dvaEURrozpadC;
          PA1.Tag := PA.Tag; {!}

          // JmenoVerejne SE DÃL¡ V PROCEDUÿE ZmenaJazyka

          PA1.Typ   := taNVarchar; //PA.Typ;
          PA1.Delka := 3;
          PA1.NULL  := nNULL;
          PA1.Pocitany := 'NULL';  //p¯i¯azuje se dynamicky v PridejRozpadNaEuro(), pro jistotu sem d·v·m NULL
          PA1.Verejny := vFalse;   //p¯i¯azuje se dynamicky v PridejRozpadNaEuro()
          PA1.SirkaSloupce := 3;
          PA1.DalsiVlastnostiAtr := PA.DalsiVlastnostiAtr + [dvaDefinovany, _dvaEURRozpad] - [dvaEUR];
          PA1.SkupinaTxt   := PA.SkupinaTxt;

          Inc(PA1);
        end;

        Inc(PA);
      end;
      { nuluj puvodni deklaraci atributu tabulky }
      ddTabulka_GetTabulkaDef(LTabulka).PocetAtributu := CelkovyPocetAtributu;
      FillChar(OldPA^, OldPocetA * SizeOf(TAtributTabulky), 0);
    end;
    { konec generovani pocitanych atributu pro datumy }

    { ma tato tabulka nejakeho predchudce }
    if ddTabulka_GetTabulkaDef(LTabulka).Predek <> tZadna then
    begin { jedna se o tabulku importu }
      NewDef := ddTabulka_GetTabulkaDef(LTabulka);
      OldDef := ddTabulka_GetTabulkaDef(ddTabulka_GetTabulkaDef(LTabulka).Predek);
{$IFDEF Ladit}
      if NewDef.Predek > LTabulka then begin
        MessageBox(0, PChar(Format('Tabulka %s je definovana drive nez jeji predchudce. !',[JmenoTabulky(LTabulka)])), 'LADIT', 0);
        Halt;
      end;
      if NewDef.Predek = LTabulka then begin
        MessageBox(0, PChar(Format('Tabulka %s ma predchudce sama sebe. !',[JmenoTabulky(LTabulka)])), 'LADIT', 0);
        Halt;
      end;
{$ENDIF Ladit}

      if (NewDef.Jmeno='') and (NewDef.JmenoTxt=txtNic) then begin // TZ 17.9.2001
        if OldDef.JmenoTxt=txtNic then NewDef.Jmeno := OldDef.Jmeno
                                  else NewDef.Jmeno := sqlCtiOznam(OldDef.JmenoTxt);
        NewDef.Jmeno := NewDef.Jmeno + ' - Import';
      end;

      SLVyjmute := TStringList.Create; // [RK 08.12.2006]
      try
        SLVyjmute.Text := NewDef.PredekAtributyVyjmute;
        SLVyjmute.Sorted := True;

        { pocet atributu bez pocitanejch a identity }
        PocetDatAtributu := 0;
        PA := OldDef.Atributy;
        for I := 1 to OldDef.PocetAtributu do
        begin
        //if ((PA.Pocitany = '')and(PA.Typ<>taIdentity)) then Inc(PocetDatAtributu);
        //if (PA.Typ <> taIdentity) then
        //  Inc(PocetDatAtributu);

          if (PA.Typ <> taIdentity) and (SLVyjmute.IndexOf(PA.JmenoSys) = -1) then
            Inc(PocetDatAtributu);

          Inc(PA);
        end;

        if not(dvtPredekBezID in NewDef.DalsiVlastnosti) then // [RK 12.12.2006]
          Inc(PocetDatAtributu);  //jeste jeden navic - ID

        if not(dvtPredekBezChyboveHlasky in NewDef.DalsiVlastnosti) then //[TZ 07.05.2004]
        begin
          Inc(PocetDatAtributu);  //jeste jeden navic - ChybovaHlaska
          Inc(PocetDatAtributu);  //jeste jeden navic - ChybovaHlaska_255 - 255 znaku z Chybove hlasky - pocitany atribut
        end;

        { pricti atributy, ktere jsou nadefinovane }
        PocetDatAtributu := PocetDatAtributu + NewDef.PocetAtributu;

        GetMem(PA1, PocetDatAtributu * SizeOf(TAtributTabulky));
        FillChar(PA1^, PocetDatAtributu * SizeOf(TAtributTabulky), 0);

        OldPA := NewDef.Atributy;
        NewDef.Atributy := PA1;
        Include(NewDef.DalsiVlastnosti, _dvtLzeFreeMem);

        if NewDef.PocetAtributu<>0 then
        begin
          Move(OldPA^, NewDef.Atributy^, NewDef.PocetAtributu * SizeOf(TAtributTabulky));
          Inc(PA1, NewDef.PocetAtributu);
        end;

        //je CLUSTERED INDEX ? -> kdyz ano - nelze PRIMARY KEY
        LUzJeClustered := False;
        PC := ddTabulka_GetTabulkaDef(LTabulka).Constraints;
        for I := 1 to ddTabulka_GetTabulkaDef(LTabulka).PocetConstraints do
        begin
          if PC^.Typ = coIndexClustered then
          begin
            LUzJeClustered := True;
            Break;
          end;
          Inc(PC);
        end;

        NewDef.PocetAtributu := PocetDatAtributu;

        if not(dvtPredekBezID in NewDef.DalsiVlastnosti) then // [RK 12.12.2006]
        begin
          with PA1^ do
          begin
            JmenoSys := uta_SystemoveCislo;
            JmenoVerejneTxt := txtSystemoveCislo;
            Typ := taIdentity;
            NULL := nNOTNULL;
            Vyzadovany := vTrue;
            if LUzJeClustered then
              PK_UQ := puUnique
            else
              PK_UQ := puPrimaryKey;
          end;
          Inc(PA1);
        end;

        if not(dvtPredekBezChyboveHlasky in NewDef.DalsiVlastnosti) then  //[TZ 07.05.2004]
        begin
          //ChybovaHlaska
          with PA1^ do
          begin
            JmenoSys := uta_ChybovaHlaska;
            JmenoVerejneTxt := xChybovaHlaska;
            Typ := taNText;
            NULL := nNULL;
            Verejny := vTrue;
          end;
          Inc(PA1);

          //ChybovaHlaska_255
          with PA1^ do
          begin
            JmenoSys := Format('%s_' + sqlText255, [uta_ChybovaHlaska]);

            // Jmenoverejne se DÃL¡ V PROCEDUÿE ZmenaJazyka

            Typ := taNVarChar;
            Delka := 255;
            NULL  := nNULL;
            // 1.substring je proto, aby se CRLF zbytecne nezamenovalo v cele poznamce
            // 2.substring je proto, aby to byl VARCHAR(255), jinak to je VARCHAR(8000)
            Pocitany :=
              Format('(SUBSTRING(REPLACE(SUBSTRING(%s.[%s],1,255),(NCHAR(13) + NCHAR(10)),NCHAR(32)),1,255))',
                     [ddTabulka_GetTabulkaDef(LTabulka).JmenoSys, uta_ChybovaHlaska]);
            Verejny := vTrue;
            SirkaSloupce := 20;
            DalsiVlastnostiAtr := [dvaDefinovany];
          end;
          Inc(PA1);
        end;

        PA := OldDef.Atributy;
        for I := 1 to OldDef.PocetAtributu do
        begin
          //if PA.Typ <> taIdentity then
          if (PA.Typ <> taIdentity) and (SLVyjmute.IndexOf(PA.JmenoSys) = -1) then // [RK 08.12.2006] SLVyjmute
          begin
            PA1^ := PA^;
            with PA1^ do
            begin
              if not(dvtPredek_PK_UQ_FK_CK in NewDef.DalsiVlastnosti) then // [RK 08.12.2006]
              begin
                PK_UQ := puNic;
                ForeignKey := '';
                Check := '';
              end;

              if dvtPredekBezDefaultu in NewDef.DalsiVlastnosti then
                ServerDefault := '';

              if dvtPredekBezDefPocitanych in NewDef.DalsiVlastnosti then
                Pocitany := '';

              // Pridal FM - kdyz uz tak uz - pokud nevyhovi, udelame typ dedeni
              // Nelze, protoze tabulky imp existuji a nektere atributy nelze alter column
              //NULL:= nNull;

              // [RK 29.03.2007] aby se ta definice porad nekopirovala
              if UTyp = utaOzn then
                Pocitany := Format(Definice_uta_Ozn, [NQuotedStr(NewDef.JmenoSys), NewDef.JmenoSys])
              else
                //[TZ 13.01.2005] nahrazenÌ prefix˘ u prg def sloupc˘ (p¯: 'TabKmenZbozi.' -> 'TabKmenZboziImp.')
                if (Pocitany<>'') and (dvaDefinovany in DalsiVlastnostiAtr) then
                  Pocitany := StringReplace(Pocitany, OldDef.JmenoSys+'.', NewDef.JmenoSys+'.', [rfReplaceAll, rfIgnoreCase]);
            end;
            Inc(PA1);
          end;
          Inc(PA);
        end;
      finally
        SLVyjmute.Free;
      end;
    end;
    { konec tabulky, ktera ma prechudce }

{ ----------------------------------------------------------------------------- }

{$IfDef EURSK}
    { EUR logy - ma tato tabulka nejakeho EUR predchudce }
    if ddTabulka_GetTabulkaDef(LTabulka).PredekEUR <> '' then
    begin { jedna se o tabulku importu }
      if SLeur.IndexOf(ddTabulka_GetTabulkaDef(LTabulka).PredekEUR)<>-1 then
        SLeur.Delete(SLeur.IndexOf(ddTabulka_GetTabulkaDef(LTabulka).PredekEUR));

      NewDef := ddTabulka_GetTabulkaDef(LTabulka);
      OldDef := ddTabulka_GetTabulkaDef(TypTabulky(ddTabulka_GetTabulkaDef(LTabulka).PredekEUR));
{$IFDEF Ladit}
      if TypTabulky(NewDef.PredekEUR) > LTabulka then begin
        MessageBox(0, PChar(Format('Tabulka %s je definovana drive nez jeji EUR predchudce. !',[JmenoTabulky(LTabulka)])), 'LADIT', 0);
        Halt;
      end;
      if TypTabulky(NewDef.PredekEUR) = LTabulka then begin
        MessageBox(0, PChar(Format('Tabulka %s ma EUR predchudce sama sebe. !',[JmenoTabulky(LTabulka)])), 'LADIT', 0);
        Halt;
      end;
{$ENDIF Ladit}

      PocetDatAtributu := 0;
      // kontrola, zda puvodni tabulka existuje - diky ruznym IfDefum nemusi tabulka existovat
      if Assigned(OldDef) then
      begin
        PA := OldDef.Atributy;
        for I := 1 to OldDef.PocetAtributu do
        begin
          if dvaEUR in (PA.DalsiVlastnostiAtr) then
            Inc(PocetDatAtributu);

          Inc(PA);
        end;
      end;

      PocetDatAtributu := 2*PocetDatAtributu;
      Inc(PocetDatAtributu);  //jeste jeden navic - vazebni ID

      GetMem(PA1, PocetDatAtributu * SizeOf(TAtributTabulky));
      FillChar(PA1^, PocetDatAtributu * SizeOf(TAtributTabulky), 0);

      NewDef.Atributy := PA1;

      NewDef.PocetAtributu := PocetDatAtributu;

      // vazebni ID do puvodni tabulky
      with PA1^ do
      begin
        JmenoSys := uta_SystemoveCislo;
        JmenoVerejneTxt := txtSystemoveCislo;
        Typ := taInt;
        NULL := nNOTNULL;
        Vyzadovany := vTrue;
//        if LUzJeClustered then
//          PK_UQ := puUnique
//        else
          PK_UQ := puPrimaryKey;
      end;
      Inc(PA1);

      if Assigned(OldDef) then
      begin
        PA := OldDef.Atributy;
        for I := 1 to OldDef.PocetAtributu do
        begin
          if dvaEUR in (PA.DalsiVlastnostiAtr) then
          begin
            { vlastni atribut }
            PA1^ := PA^;
            with PA1^ do
            begin
              PK_UQ := puNic;
              ForeignKey := '';
              Check := '';
              ServerDefault := '';
              Pocitany := '';
              NULL:= nNull;
              DalsiVlastnostiAtr := [];
            end;
            Inc(PA1);

            { zbytek vlastniho atribut }
            PA1^ := PA^;
            with PA1^ do
            begin
              JmenoSys := JmenoSys+'_Zb';
  //            JmenoVerejneTxt := txtSystemoveCislo;
              Typ := taFloat;
              PK_UQ := puNic;
              ForeignKey := '';
              Check := '';
              ServerDefault := '';
              Pocitany := '';
              NULL:= nNull;
              DalsiVlastnostiAtr := [];
            end;
            Inc(PA1);
          end;
          Inc(PA);
        end;
      end;

{..$ENDIF Ladit}
    end;
    { konec: EUR logy - ma tato tabulka nejakeho EUR predchudce }
{$EndIf EURSK}

{ ----------------------------------------------------------------------------- }

    { test max. delky Atr - musi byt az za generovanymi atributy ! }
    {$IFDEF Ladit}  //[TZ 15.10.2008]
    LSum := 0.0;
    {$ENDIF Ladit}
    PA := ddTabulka_GetTabulkaDef(LTabulka).Atributy;
    for I := 1 to ddTabulka_GetTabulkaDef(LTabulka).PocetAtributu do
    begin
      if Length(PA.JmenoSys) > sqlMaxDelkaNazvuAtributu then
      begin
        ShowMessage(Format('%s.%s - delka systemoveho nazvu atributu nesmi presahovat %d !',
                       [JmenoTabulky(LTabulka), PA.JmenoSys, sqlMaxDelkaNazvuAtributu]));
        Halt;
      end;

      {$IFDEF Ladit} //[RK 20.06.2000]
      if not IsValidIdent(PA.JmenoSys) then
      begin
        MessageBox(0,
          PChar(Format('%s.%s - systemovy nazev atributu zrejme obsahuje cesky znak nebo mezeru!',
                       [JmenoTabulky(LTabulka), PA.JmenoSys])),
          'LADIT', 0);
        Halt;
      end;
      {$ENDIF Ladit}

      {$IFDEF Ladit}  //[TZ 15.10.2008]
      if Trim(PA.Pocitany)='' then  //napoËÌtavej jen pevnÈ sloupce
        LSum := LSum + MaxDelkaAtributu(PA, True{=AUnicodeHypoteticky});
      {$ENDIF Ladit}

      Inc(PA);
    end;

    {$IFDEF Ladit}  //[TZ 15.10.2008]
    if SameText(JmenoTabulky(LTabulka), 'TabIZasilka') then
      LLimit := 10615.5  //z historick˝ch d˘vod˘ v˝jimka pro IST-domluveno s p. Raöplem
    else
      LLimit :=  8060.0;
    if LSum>LLimit then begin
      MessageBox(0, PChar(Format('%s (%.3n B) - max. sirka jednoho radku tabulky presahuje limit (%n B)!', [JmenoTabulky(LTabulka), LSum, LLimit])), 'LADIT', 0);
      Halt;
    end;
    {$ENDIF Ladit}

    { jako prvni spocitej celkovy pocet constraints }
    AtributyPocetConstraints := 0;
    PA := ddTabulka_GetTabulkaDef(LTabulka).Atributy;
    for I := 1 to ddTabulka_GetTabulkaDef(LTabulka).PocetAtributu do
    begin
      if PA.PK_UQ = puPrimaryKey then Inc (AtributyPocetConstraints);
      if PA.PK_UQ = puUnique then Inc (AtributyPocetConstraints);
      if PA.CHECK<>'' then Inc (AtributyPocetConstraints);
      if PA.ForeignKey<>'' then Inc (AtributyPocetConstraints);
      if PA.ServerDefault<>'' then Inc (AtributyPocetConstraints);
      Inc(PA);
    end;

    {$IFDEF Ladit} // zkontroluj, zda v poli omezeni neni default - je to nepripustne
    PC := ddTabulka_GetTabulkaDef(LTabulka).Constraints;
    for I := 1 to ddTabulka_GetTabulkaDef(LTabulka).PocetConstraints do
    begin
      if PC.Typ = coDefault then
      begin
        MessageBox(0, 'IntegritnÌ omezenÌ typu DEFAULT musÌ byt definovano primo u atributu (ne v poli constraints) !', 'LADIT', 0);
        Halt;
      end;

      Inc(PC);
    end;
    {$ENDIF}

    {alokuji a movuji constraints z pole}
    CelkovyPocetConstraints := AtributyPocetConstraints + ddTabulka_GetTabulkaDef(LTabulka).PocetConstraints;
    GetMem(PC, CelkovyPocetConstraints*SizeOf(TConstraintTabulky));
    FillChar(PC^, CelkovyPocetConstraints*SizeOf(TConstraintTabulky), 0);
    if ddTabulka_GetTabulkaDef(LTabulka).Constraints <> nil then
      Move(ddTabulka_GetTabulkaDef(LTabulka).Constraints^, PC^, ddTabulka_GetTabulkaDef(LTabulka).PocetConstraints*SizeOf(TConstraintTabulky));

    {prirazuji}
    ddTabulka_GetTabulkaDef(LTabulka).Constraints := PC;
    Inc(PC, ddTabulka_GetTabulkaDef(LTabulka).PocetConstraints);
    ddTabulka_GetTabulkaDef(LTabulka).PocetConstraints := CelkovyPocetConstraints;

    {vytvarim nove}
    PA := ddTabulka_GetTabulkaDef(LTabulka).Atributy;
    for I := 1 to ddTabulka_GetTabulkaDef(LTabulka).PocetAtributu do
    begin
      if PA.PK_UQ = puPrimaryKey then
      begin
        PC.Typ := coPrimaryKey;
        PC.Atributy := PA.JmenoSys;
        Inc(PC);
      end;

      if PA.PK_UQ = puUnique then
      begin
        PC.Typ := coUnique;
        PC.Atributy := PA.JmenoSys;
        Inc(PC);
      end;

      if PA.CHECK <> '' then
      begin
        PC.Typ := coCheck;
        PA.CHECK := Trim(PA.CHECK);
        if Length(PA.CHECK) >= 2 then
        begin
          PomStr7 := UpperCase(Copy(PA.CHECK, 1, 7));
          if CharInSet(PomStr7[1], ['>', '=', '<']) or
             (Copy(PomStr7, 1, 2) = 'IN')      or
             (Copy(PomStr7, 1, 7) = 'BETWEEN') or
             (Copy(PomStr7, 1, 4) = 'LIKE')    or
             (Copy(PomStr7, 1, 3) = 'NOT')     then
          begin
            PA.CHECK := PA.JmenoSys + ' ' + PA.CHECK; {doplni jmeno atributu}
          end;
        end;
        PC.Vyraz := PA.CHECK;
        PC.JmenoSys := PrefixyConstraints[PC.Typ] + ConstraintOddelovac + JmenoTabulky(LTabulka) + ConstraintOddelovac + PA.JmenoSys;
        Inc (PC);
      end;

      if PA.ForeignKey <> '' then
      begin
       PC.Typ := coForeignKey;
       PC.VlastniAtributy := PA.JmenoSys;
       I1 := Pos('(',PA.ForeignKey);
       I2 := Pos(')',PA.ForeignKey);
       PC.CiziTabulka := TypTabulky(Copy(PA.ForeignKey,1,I1-1));
       PC.CiziAtributy := Copy(PA.ForeignKey,I1+1,I2-I1-1);
       Inc (PC);
      end;

      if PA.ServerDefault <> '' then
      begin {abych usetril promennou, tak iz generuji nazev ted }
        PC.JmenoSys := PrefixyConstraints[coDefault] + ConstraintOddelovac + JmenoTabulky(LTabulka) + ConstraintOddelovac + PA.JmenoSys;
        PC.Typ := coDefault;
        PC.Vyraz := PA.ServerDefault;
        Inc(PC);
      end;

      Inc(PA);
    end;

    {$IFDEF Ladit}
    LTabulkaMaClusteredIndex := False;
    {$ENDIF Ladit}
    PC := ddTabulka_GetTabulkaDef(LTabulka).Constraints;
    for I := 1 to ddTabulka_GetTabulkaDef(LTabulka).PocetConstraints do
    begin
      PC.JmenoSys := Trim(PC.JmenoSys);

      {$IFDEF Ladit}
      if (PC.Typ = coCheck) and (PC.JmenoSys = '') then
      begin
        MessageBox(0, 'IntegritnÌ omezenÌ typu CHECK musÌ mÌt n·zev !', 'LADIT', 0);
        Halt;
      end;

      //[TZ 07.01.2009]
      if (PC.Typ = coPrimaryKey) or (PC.Typ = coIndexClustered) then
      begin
        if LTabulkaMaClusteredIndex then begin
          MessageBox(0, PChar('Tabulka ' + ddTabulka_GetTabulkaDef(LTabulka).JmenoSys + ' nem˘ûe mÌt vÌce neû jeden clustered index (nezapomeÚ: PRIMARY KEY=clustered index)!'), 'LADIT', 0);
        end;
        LTabulkaMaClusteredIndex := True;
      end;

      // [RK 14.04.2009] doplnena kontrola vyplnenosti - kvuli uprave recordu TConstraintTabulky
      case PC.Typ of
        coPrimaryKey, coUnique, coIndex, coIndexClustered:
          if PC.Atributy = '' then
          begin
            MessageBox(0, PChar(
              Format('Chybne definovano integritni omezeni - neni vyplnena polozka Atributy!'#13+
                     '%s (coPrimaryKey, coUnique, coIndex, coIndexClustered)', [JmenoTabulky(LTabulka)])),
                     'LADIT', 0);
            Halt;
          end;

        coDefault, coCheck:
          if PC.Vyraz = '' then
          begin
            MessageBox(0, PChar(
              Format('Chybne definovano integritni omezeni - neni vyplnena polozka Vyraz!'#13+
                     '%s (coDefault, coCheck)', [JmenoTabulky(LTabulka)])),
                     'LADIT', 0);
            Halt;
          end;

        coForeignKey:
          if (PC.VlastniAtributy = '') or (PC.CiziAtributy = '') then
          begin
            MessageBox(0, PChar(
              Format('Chybne definovano integritni omezeni - neni vyplneno VlastniAtributy nebo CiziAtributy!'#13+
                     '%s (coForeignKey)', [JmenoTabulky(LTabulka)])),
                     'LADIT', 0);
            Halt;
          end;
      end;
      {$ENDIF}

      if PC.JmenoSys = '' then
      begin
        SL.Clear;
        PC.JmenoSys := PrefixyConstraints[PC.Typ] + ConstraintOddelovac + JmenoTabulky(LTabulka);
        case PC.Typ of
          coPrimaryKey,
          coUnique,
          coIndex,
          coIndexClustered
                           : SL.CommaText := PC.Atributy;
          coCheck          : ; {nic - chyba}
          coForeignKey     : SL.CommaText := PC.VlastniAtributy;
          coDefault        : ; {nic - nazev je jiz vygenerovan - chyba}
        end;

        for J := 0 to SL.Count-1 do
          PC.JmenoSys := PC.JmenoSys + ConstraintOddelovac + SL[J];
      end
      else
      begin
      {$IfDef Ladit}
        if not ((Copy(PC.JmenoSys,1,4)=(PrefixyConstraints[coPrimaryKey]+ConstraintOddelovac)) or
                (Copy(PC.JmenoSys,1,4)=(PrefixyConstraints[coUnique]+ConstraintOddelovac)) or
                (Copy(PC.JmenoSys,1,4)=(PrefixyConstraints[coCheck]+ConstraintOddelovac)) or
                (Copy(PC.JmenoSys,1,4)=(PrefixyConstraints[coForeignKey]+ConstraintOddelovac)) or
                (Copy(PC.JmenoSys,1,4)=(PrefixyConstraints[coDefault]+ConstraintOddelovac)) or
                (Copy(PC.JmenoSys,1,4)=(PrefixyConstraints[coIndex]+ConstraintOddelovac)) or
                (Copy(PC.JmenoSys,1,4)=(PrefixyConstraints[coIndexClustered]+ConstraintOddelovac))
                )then
          MessageBox(0, PChar('Nesoulad integritnich omezeni ' + {String(}PC.JmenoSys{)}), 'LADIT', 0);
        if PC.Typ in [coPrimaryKey, coUnique, coForeignKey] then
          MessageBox(0, PChar('Integritni omezenÌ nesmÌ mit nadefinov·no n·zev - generuje se automaticky ' + {String(}PC.JmenoSys{)}), 'LADIT', 0);
      {$EndIf}
      end;
      Inc(PC);
    end;

(* zatim se nevyuziva, bude nekdy??? zbytecne silna podminka
    { defaulty nesmeji byt v UQ a PR omezeni }
    {$IFDEF Ladit}
    SL1 := TStringList.Create;
    try
      PA := ddTabulka_GetTabulkaDef(LTabulka).Atributy;
      for I := 1 to ddTabulka_GetTabulkaDef(LTabulka).PocetAtributu do
      begin
        if PA.ServerDefault<>'' then
        begin
          PC := ddTabulka_GetTabulkaDef(LTabulka).Constraints;
          for J := 1 to ddTabulka_GetTabulkaDef(LTabulka).PocetConstraints do
          begin
            case PC.Typ of
              coPrimaryKey,coUnique :
              begin
                SL1.Clear;
                SL1.CommaText := UpperCase(PC.Atributy);
                if SL1.IndexOf(UpperCase(PA.JmenoSys)) <> -1
                then
                begin
                  MessageBox(0, PChar(Format('Atribut %s - m· default a je souËastÌ UQ/PR omezeni %s. To nelze !', [PA.JmenoSys,PC.JmenoSys])), 'LADIT', 0);
                  Halt;
                end;
              end;
            end;
            Inc(PC);
          end;
        end;
        Inc(PA);
      end;
    finally
      SL1.Free;
    end;
    {$ENDIF}
*)

  end;

  {$IFDEF Ladit}
  SortedSL := TStringList.Create;
  SortedVID := TStringList.Create;
  try
    SortedSL.Sorted := True;
    SortedVID.Sorted := True;
  {$ENDIF}

    for LVztah := Succ(Low(LVztah)) to High(LVztah) do
    begin
      {$IFDEF Ladit}
      with DefiniceVztahu[LVztah] do
      begin
        if LVztah <> X then
        begin
          MessageBox(0, PChar(Format('Vztah s VID %d - nesedÌ self-check!', [VID])), 'LADIT', 0);
          Halt;
        end;

        if Trim(JmenoSys) = '' then
        begin
          MessageBox(0, PChar(Format('Vztah Ë. %d - JmenoSys musÌ b˝t zad·no!', [Integer(LVztah)])), 'LADIT', 0);
          Halt;
        end;

        if Trim(GUIDVazby) <> '' then
        begin
          MessageBox(0, PChar(Format('Vztah %s - GUIDVazby nesmÌ b˝t zad·no!', [JmenoSys])), 'LADIT', 0);
          Halt;
        end;

        if VID <= 0 then
        begin
          MessageBox(0, PChar(Format('Vztah Ë. %d - VID musÌ b˝t zad·no (%s)!',
                                     [Integer(LVztah), JmenoSys])), 'LADIT', 0);
          Halt;
        end;

        if (VID < 0) or (VID > 65535) then
        begin
          MessageBox(0, PChar(Format('Vztah Ë. %d - VID nem˘ûe b˝t %d - musÌ se vejÌt do typu Word (i kdyû je typov·n jako Integer)!',
                                     [Integer(LVztah), VID])), 'LADIT', 0);
          Halt;
        end;

        if SortedVID.Find(IntToStr(VID), I) then
        begin
          MessageBox(0, PChar(Format('Vztah Ë. %d - duplicitnÌ VID %d!', [Integer(LVztah), VID])), 'LADIT', 0);
          Halt;
        end;
        SortedVID.Append(IntToStr(VID));

        if JmenoSys[1] <> 'V' then
        begin
          MessageBox(0, PChar(JmenoSys+' - systÈmovÈ jmÈno vztahu musÌ zaËÌnat znakem ''V'' (velkÈ) !'), 'LADIT', 0);
          Halt;
        end;

        if SameText(Copy(JmenoSys, 1, Length(sqlDefVazbaPrefix)), sqlDefVazbaPrefix) then
        begin
          MessageBox(0, PChar(JmenoSys+' - systÈmovÈ jmÈno vztahu nesmÌ zaËÌnat na ''' + sqlDefVazbaPrefix + ''' !'), 'LADIT', 0);
          Halt;
        end;

        if Pos('_', JmenoSys)>0 then
        begin
          MessageBox(0, PChar(JmenoSys+' - systÈmovÈ jmÈno vztahu nesmÌ obsahovat znak ''_'' !'), 'LADIT', 0);
          Halt;
        end;

        if TabLStr <> '' then
        begin
          MessageBox(0, PChar(JmenoSys+' - TabLStr nesmÌ b˝t zad·no!'), 'LADIT', 0);
          Halt;
        end;

        if TabPStr <> '' then
        begin
          MessageBox(0, PChar(JmenoSys+' - TabPStr nesmÌ b˝t zad·no!'), 'LADIT', 0);
          Halt;
        end;

        if X <> vObecnaTab_ObecnaTab then
        begin
          if TabL <> TabP then
            begin
              if Pos(sqlVazbaSpecifHT, Join) > 0 then
              begin
                MessageBox(0, PChar(JmenoSys + ' - definice vazby NESMÕ obsahovat specifik·tor hlavnÌ tabulky!'), 'LADIT', 0);
                Halt;
              end;
            end
          else
            begin
              if (TypLP <> tvZadny) and (TypPL <> tvZadny) then
              begin
                MessageBox(0, PChar(JmenoSys + ' - jeden z typ˘ (TypLP, TypPL) musÌ b˝t tvZadny!'), 'LADIT', 0);
                Halt;
              end;

              if Pos(sqlVazbaSpecifHT, Join) = 0 then
              begin
                MessageBox(0, PChar(JmenoSys + ' - definice vazby musÌ obsahovat specifik·tor hlavnÌ tabulky!'), 'LADIT', 0);
                Halt;
              end;
            end;
        end;

        if TypLP <> tvZadny then
        begin
          if Trim(NazevLPSys) = '' then
          begin
            MessageBox(0, PChar(Format('Vztah Ë. %d (VID) - NazevLPSys musÌ b˝t zad·no!', [VID])), 'LADIT', 0);
            Halt;
          end;

          if Trim(NazevLPSys) <> '' then
          begin
            if not IsValidIdent(NazevLPSys) then
            begin
              MessageBox(0, PChar(Format('Vztah Ë. %d (VID) - NazevLPSys musÌ b˝t platn˝ identifik·tor!', [VID])), 'LADIT', 0);
              Halt;
            end;
          end;
        end;

        if TypPL <> tvZadny then
        begin
          if Trim(NazevPLSys) = '' then
          begin
            MessageBox(0, PChar(Format('Vztah Ë. %d (VID) - NazevLPSys musÌ b˝t zad·no!', [VID])), 'LADIT', 0);
            Halt;
          end;

          if Trim(NazevPLSys) <> '' then
          begin
            if not IsValidIdent(NazevPLSys) then
            begin
              MessageBox(0, PChar(Format('Vztah Ë. %d (VID) - NazevPLSys musÌ b˝t platn˝ identifik·tor!', [VID])), 'LADIT', 0);
              Halt;
            end;
          end;
        end;

        if SortedSL.Find(UpperCase(JmenoSys), I) then
        begin
          MessageBox(0, PChar(Format('Vztah Ë. %d (VID) - duplicitnÌ n·zev %s!', [VID, JmenoSys])), 'LADIT', 0);
          Halt;
        end;
        SortedSL.Append(UpperCase(JmenoSys));
      end;
      {$ENDIF}

      with DefiniceVztahu[LVztah] do
      begin
        X := LVztah;
        TabLStr := JmenoTabulky(TabL);
        TabPStr := JmenoTabulky(TabP);
        GUIDVazby := IntToStr(VID);
      end;
    end;

  {$IFDEF Ladit}
  for LZVztah := Succ(Low(DefiniceZakazanehoVztahu)) to High(DefiniceZakazanehoVztahu) do
  begin
    if LZVztah <> DefiniceZakazanehoVztahu[LZVztah].X then
    begin
      MessageBox(0, PChar(Format('Zakazany vztah Ë. %d - nesedÌ self-check !', [Integer(LZVztah)])), 'LADIT', 0);
      Halt;
    end;
  end;
  {$ENDIF}

  { kontrola browsu }
  {$IFDEF Ladit}
  SortedSL.Clear;
  {$ENDIF}

  {$IFDEF Ladit}
  for LokTypAtributu := Low(GTypy) to High(GTypy) do
  begin
    if LokTypAtributu <> GTypy[LokTypAtributu]._X then
    begin
      MessageBox(0, 'GTypy (ddMain) - nesedÌ self-check !', 'LADIT', 0);
      Halt;
    end;
  end;
  {$ENDIF}

  for LBrowse := Succ(Low(LBrowse)) to High(LBrowse) do
  begin
    with SeznamVychozichNastaveniBrowse[LBrowse] do
    begin
      {$IFDEF Ladit}
       if LBrowse<>X then
       begin
         MessageBox(0, 'V˝chozÌ nastavenÌ brows˘ - nesedÌ self-check !', 'LADIT', 0);
         Halt;
       end;

       if BID=0 then
       begin
         MessageBox(0, PChar(Format('Browse Ë. %d - nep¯i¯azeno Browse ID !', [Integer(LBrowse)])), 'LADIT', 0);
         Halt;
       end;

       if (BID<0) or (BID>65535) then
       begin
         MessageBox(0, PChar(Format('Browse Ë. %d - BrowseID nem˘ûe b˝t %d - musÌ se vejÌt do typu Word (i kdyû je typov·n jako Integer) !', [Integer(LBrowse), BID])), 'LADIT', 0);
         Halt;
       end;

       if (BID >= x_ObecnePohledy_BID_Base) {and (BID <= x_ObecnePohledy_BID_Top)} then
       begin
         MessageBox(0, PChar(Format('Browse Ë. %d - BID (%d) ve vyhrazenÈ ¯adÏ definovan˝ch p¯ehled˘!', [Integer(LBrowse), BID])), 'LADIT', 0);
         Halt;
       end;

       if SortedSL.Find(IntToStr(BID), I) then
       begin
         MessageBox(0, PChar(Format('Browse Ë. %d - duplicitnÌ Browse ID %d !', [Integer(LBrowse), BID])), 'LADIT', 0);
         Halt;
       end;
       SortedSL.Add(IntToStr(BID));

       //nelze souËasnÏ soudek a p¯edka
       if (Typ<>tbNeniVidet) and (Predek<>bZadny) then
       begin
         MessageBox(0, PChar(Format('Nesoulad p¯i¯azenÌ browsu do stromeËku - kolize skupina <> p¯edek - browse %d %s !', [BID, Jmeno])), 'LADIT', 0);
         Halt;
       end;

       //[TZ 06.11.2008] test: p¯edek nem· soudek
       if Predek<>bZadny then
       begin
         //najdi nejvyööÌho p¯edka v ¯etÏzu p¯edk˘
         LPredekPredka := Predek;
         while SeznamVychozichNastaveniBrowse[LPredekPredka].Predek<>bZadny do
           LPredekPredka := SeznamVychozichNastaveniBrowse[LPredekPredka].Predek;

         if SeznamVychozichNastaveniBrowse[LPredekPredka].Typ = tbNeniVidet then
         begin
           MessageBox(0, PChar(Format('Nesoulad p¯i¯azenÌ browsu do stromeËku - p¯edek nem· soudek - browse %d %s !', [BID, Jmeno])), 'LADIT', 0);
           Halt;
         end;
       end;

       if (Skupina_Tisky >= x_ObecnePohledy_SkupinaTisky_Base) {and
          (Skupina_Tisky <= x_ObecnePohledy_SkupinaTisky_Top)} then
       begin
         MessageBox(0, PChar(Format('Browse Ë. %d - Skupina_Tisky (%d) ve vyhrazenÈ ¯adÏ !', [Integer(LBrowse), Skupina_Tisky])), 'LADIT', 0);
         Halt;
       end;

       // jen tyto lze pouûÌt
       NasliOstatniPrehledy := False;
       for I := Low(G_OstatniPrehledy) to High(G_OstatniPrehledy ) do
         if (G_OstatniPrehledy[I] = OstatniPrehledy)then begin NasliOstatniPrehledy := True; Break; end;
       if not NasliOstatniPrehledy then
       begin
         MessageBox(0, PChar(Format('Browse BID=%d - OstatniPrehledy (%d) !', [BID, OstatniPrehledy])), 'LADIT', 0);
         Halt;
       end;

      {$ENDIF Ladit}

      if Jmeno='*' then
      begin
        {### bude treba odstranit }
        Jmeno    := VerejneJmenoTabulky(JmenoTabulky(HlavniTabulka));  // hvÏzdiËku p¯eöramt· aû ZmenaJazyka

        JmenoTxt := ddTabulka_GetTabulkaDef(HlavniTabulka).JmenoTxt;
      end;

      if SELECT='*' then
      begin
        SL.Clear;
        AtributyTabulky(HlavniTabulka, SL, False{AVcetneSystemovych:Boolean}, jnJenJmena);
        SELECT := SL.Text;
      end;
    end;
  end;
  {$IFDEF Ladit}
  finally
    FreeAndNil(SortedVID);
    FreeAndNil(SortedSL);
  end;
  {$ENDIF}

  //co to zkusit zde
//  ZmenaJazyka;

(* zruöeno [TZ 15.03.2004]
  {$IFDEF Ladit}
   //kontrola na duplicitnÌ ve¯ejn· jmÈna brows˘ - musÌ b˝t aû po ZmenaJazyka() [TZ 10.12.2002]
   SortedSL := TStringList.Create;
   SortedVID:= TStringList.Create;
   try
     SortedSL.Sorted := True;
     for LBrowse := Succ(Low(LBrowse)) to High(LBrowse) do
     begin
       with SeznamVychozichNastaveniBrowse[LBrowse] do
       begin
         if SortedSL.Find(Jmeno, I) then
         begin
           SortedVID.Add(Format('BID=%4d'#9'%4d'#9'%s', [BID, Integer(SortedSL.Objects[I]), Jmeno]));
         //nem˘ûu nechat zobrazovat, protoûe je jich hroznÏ moc
         //MessageBox(0, PChar(Format('Browse Ë. %d - BID=%d -- DuplicitnÌ ve¯ejnÈ jmÈno browse !'#13#13'%s', [Integer(LBrowse), BID, Jmeno])), 'LADIT', 0);
         //Halt;
         end;
         SortedSL.AddObject(Jmeno, Pointer(BID));
       end;
     end;
     if SortedVID.Count>0 then
       SortedVID.SaveToFile('C:\LADIT_ddBrowse_duplicitni_jmena.txt');
   finally
     FreeAndNil(SortedVID);
     FreeAndNil(SortedSL);
   end;
  {$ENDIF Ladit}
*)

{$IfDef Skupina_Tisky_Test}
  S := '';
  for LBrowse := Succ(Low(LBrowse)) to Pred(High(LBrowse)) do
    for PBrowse := Succ(LBrowse) to High(PBrowse) do
    begin
      if SeznamVychozichNastaveniBrowse[LBrowse].Skupina_Tisky =
         SeznamVychozichNastaveniBrowse[PBrowse].Skupina_Tisky then
         S := S + #13 + SeznamVychozichNastaveniBrowse[LBrowse].Jmeno + ' ' +
                        SeznamVychozichNastaveniBrowse[PBrowse].Jmeno;
    end;
  if S<>'' then
  MessageBox(0, PChar(Format('Stejne kody tisku jsou u nasledujicich browsu: %s !',[S])), 'SKUPINA_TISK_TEST', 0);
{$EndIf}

{$IfDef Skupina_Filtry_Test}
  SS := '';
  for LBrowse := Succ(Low(LBrowse)) to Pred(High(LBrowse)) do
    for PPBrowse := Succ(LBrowse) to High(PPBrowse) do
    begin
      if SeznamVychozichNastaveniBrowse[LBrowse].Skupina_Filtry =
         SeznamVychozichNastaveniBrowse[PPBrowse].Skupina_Filtry then
         SS := SS + #13 + SeznamVychozichNastaveniBrowse[LBrowse].Jmeno + ' ' +
                          SeznamVychozichNastaveniBrowse[PPBrowse].Jmeno;
    end;
  if SS<>'' then
  MessageBox(0, PChar(Format('Stejne kody filtru jsou u nasledujicich browsu: %s !',[SS])), 'SKUPINA_TISK_TEST', 0);
{$EndIf}

{$IFDEF Test_Jsou_DM}
  LAtribut := '';
  for LBrowse := Succ(Low(LBrowse)) to Pred(High(LBrowse)) do
  begin
    if SeznamVychozichNastaveniBrowse[LBrowse].DMAkceClass = nil then
      LAtribut :=
        Format('%s'#13'%d - %s',
               [LAtribut, SeznamVychozichNastaveniBrowse[LBrowse].VID,
                SeznamVychozichNastaveniBrowse[LBrowse].Jmeno]);
  end;
  if LAtribut <> '' then
    MessageBox(0, PChar(Format('Tady nejsou vyplneny datove moduly:%s !',[LAtribut])), 'Test_Jsou_DM', 0);
{$ENDIF}

  finally
    SL.Free;
    {$IFDEF Ladit}
    SLTabEng.Free;
    SLTabAtrEng.Free;
    {$ENDIF}
  end;


{$IFDEF EURSK}
  finally
    if SLeur.Count>0 then
    begin
//      MessageBox(0, PChar(Format('Uvedene tabulky %s nemaji LOG_EUR verze !',[SLeur.Text])), 'DISTRIBUCE', 0);
//      Halt;
    end;
    SLeur.Free;
  end;
{$ENDIF EURSK}

//  ZmenaJazyka; tady to bylo, ale proc??? Ja bych to vyhodil 15.3.2000
end;

function DefAtrStr2TypAtributu(const ATypAtrStr : String): TTypAtributu;
  var
    LTypAtributu : TTypAtributu;
begin
  // [RK 12.03.2008] v edUzivAtr se dela NUMERIC(19,6),
  // tady se ale najde prvni vyskyt 'NUMERIC', coz je spatne
  if SameText(ATypAtrStr, 'NUMERIC') then
    Result := taNumeric_19_6
  else
    begin
      Result := taBlbe;
      for LTypAtributu := Succ(Low(LTypAtributu)) to High(LTypAtributu) do
      begin
        if SameText(GTypy[LTypAtributu].T, ATypAtrStr) then
        begin
          Result := LTypAtributu;
          Break;
        end;
      end;
    end;
end;

{ --------------------------------------------------------------------------- }

procedure PridejRenameAtr(const TabulkaSpec: string = '');
  var
    LTabulka, LTabMin, LTabMax: TTabulka;
    I        : Integer;
//    T_A      : String;
    PA       : PAtributTabulky;
    TD: PTabulkaDef;
//    LNazev: string;
//    Zmena1, Zmena2: Boolean;
begin
  // [RK 18.05.2010] prejmenovani atributu v Helios Easy nefunguje
//  if Server.JeToHeliosEasy then Exit;

  //nejprve smaû info o p¯ejmenovan˝ch atributech v celÈm prostoru
  // - to p¯ich·zÌ v ˙vahu nap¯. v p¯ÌpadÏ p¯epnutÌ datab·ze

  if TabulkaSpec <> '' then
    begin
      LTabMin := TypTabulky(TabulkaSpec);
      LTabMax := LTabMin;
    end
  else
    begin
      LTabMin := Succ(Low(TTabulka));
      LTabMax := High_TTabulka;
    end;

  for LTabulka := LTabMin to LTabMax do
  begin
    TD := ddTabulka_GetTabulkaDef(LTabulka);

    PA := TD.Atributy;
    for I := 1 to TD.PocetAtributu do
    begin
      if _dvaRenameAtr in PA.DalsiVlastnostiAtr then
      begin
        Exclude(PA.DalsiVlastnostiAtr, _dvaRenameAtr);

        //vr·tit p˘vodnÌ ve¯ejn· jmÈna
        PA.JmenoVerejne    := sqlCtiOznam(PA.JmenoVerejneTxt);
        PA.JmenoVerejneZkr := sqlCtiOznam(PA.JmenoVerejneZkrTxt);
      end;
      Inc(PA);
    end;
  end;

//  with Server.CreateQuery do
//  try
//    SQL.Add(
//      'SELECT ' + {0}TabRenameAtr_NazevTabulkySys + ',' +
//                  {1}TabRenameAtr_NazevAtrSys     + ',' +
//                  {2}TabRenameAtr_NazevAtrVer     + ',' +
//                  {3}TabRenameAtr_NazevAtrVerZkr  + #13 +
//      'FROM ' + TabRenameAtr);
//
//    if TabulkaSpec <> '' then
//      SQL.Add(
//         Format(
//           'WHERE ' + TabRenameAtr_NazevTabulkySys + '=%s',
//           [NQuotedStr(TabulkaSpec)]));
//
//    Open;
//    while not EOF do
//    begin
//      T_A := Fields[0].AsString + '.' + Fields[1].AsString;
//      PA  := Atribut(T_A);
//      if Assigned(PA) then
//        begin
//          Zmena1 := False;
//          Zmena2 := False;
//
//          // [RK 17.10.2008] moznost prejmenovani pres GUID
//          LNazev := Fields[2].AsString;
//          if LNazev <> '' then
//          begin
//            if not JeStringGUID(LNazev) then
//              begin
//                PA.JmenoVerejne := LNazev;
//                Zmena1 := True;
//              end
//            else
//              begin
//                LNazev := sqlCtiOznamGUID(LNazev);
//                if (LNazev <> '') and (LNazev <> Fields[2].AsString) then
//                begin
//                  PA.JmenoVerejne := LNazev;
//                  Zmena1 := True;
//                end;
//              end;
//          end;
//
//          LNazev := Fields[3].AsString;
//          if LNazev <> '' then
//          begin
//            if not JeStringGUID(LNazev) then
//              begin
//                PA.JmenoVerejneZkr := LNazev;
//                Zmena2 := True;
//              end
//            else
//              begin
//                LNazev := sqlCtiOznamGUID(LNazev);
//                if (LNazev <> '') and (LNazev <> Fields[3].AsString) then
//                begin
//                  PA.JmenoVerejneZkr := LNazev;
//                  Zmena2 := True;
//                end;
//              end;
//          end;
//
//          if Zmena1 or Zmena2 then
//            Include(PA.DalsiVlastnostiAtr, _dvaRenameAtr);  //indikace p¯ejmenov·nÌ
//        end
//      else
//        sqlChyba('TabRenameAtr - atribut ' + T_A + ' nenalezen!');
//
//      Next;
//    end;
//  finally
//    Free;
//  end;
end;

{ --------------------------------------------------------------------------- }

function QueryProUzivatelskeAtributy(aWhere: String): TDataSet;
//  var
//    LAttr, LJoin: string;
begin
//  Result := Server.CreateQuery;
//
//  if aWhere <> '' then
//    aWhere := Format('WHERE %s'#13, [aWhere]);
//
//  HeliosEasy_AttrAndJoin(TabUzivAtr + '.' + TabUzivAtr_GUID, LAttr, LJoin);
//
//  with TQueryHe(Result) do
//  begin
//    SQL.Add(
//      'SELECT ' + {00}TabUzivAtr + '.' + TabUzivAtr_NazevTabulkySys + ',' +
//                  {01}TabUzivAtr + '.' + TabUzivAtr_NazevAtrSys     + ',' +
//                  {02}TabUzivAtr + '.' + TabUzivAtr_NazevAtrVer     + ',' +
//                  {03}TabUzivAtr + '.' + TabUzivAtr_TypAtr          + ',' +
//                  {04}TabUzivAtr + '.' + TabUzivAtr_MaskaAtr        + ',' +
//                  {05}TabUzivAtr + '.' + TabUzivAtr_SirkaSloupceAtr + ',' +
//                  {06}TabUzivAtr + '.' + TabUzivAtr_SumovatAtr      + ',' +
//                  {07}TabUzivAtr + '.' + TabUzivAtr_DefiniceAtr     + ',' +
//                  {08}TabUzivAtr + '.' + TabUzivAtr_TiskAtr         + ',' +
//                  {09}TabUzivAtr + '.' + TabUzivAtr_Externi         + ',' + // [TZ 20.09.2002]
//                  {10}TabUzivAtr + '.' + TabUzivAtr_NazevAtrVerZkr  + ',' + // [RK 22.05.2008]
//                  {11}TabUzivAtr + '.' + TabUzivAtr_ProcPruhAtr     + ',' + // [RK 22.05.2008]
//                  {12}TabUzivAtr + '.' + TabUzivAtr_KonverzeAtr     + ',' + // [RK 22.05.2008]
//                  {13}TabUzivAtr + '.' + TabUzivAtr_VerejnyAtr      + ',' +
//                  {14}'COL_LENGTH(' + TabUzivAtr + '.' + TabUzivAtr_NazevTabulkySys + '+N''_EXT'',' + TabUzivAtr + '.' + TabUzivAtr_NazevAtrSys + '),' +
//                  {15}TabUzivAtr + '.' + TabUzivAtr_BarvaAtr + ',' +
//                  {16}TabUzivAtr + '.' + TabUzivAtr_GUIDText + ',' +
//                  {17}LAttr + ',' +
//                  {18}TabUzivAtr + '.' + TabUzivAtr_ExtEd_Zalozka + ',' +
//                  {19}TabUzivAtr + '.' + TabUzivAtr_BublinaAtr + #13 +
//                  {20}',' + TabUzivAtr + '.' + TabUzivAtr_StavAtr +
//                  {21}',' + TabUzivAtr + '.' + TabUzivAtr_StavovySloupec + #13 +
//      'FROM ' + TabUzivAtr + #13+
//      LJoin +
//      aWhere +
//      'ORDER BY ' + TabUzivAtr + '.' + TabUzivAtr_NazevTabulkySys + ',' + // tabulka jako prvni!!!
//                    TabUzivAtr + '.' + TabUzivAtr_Externi+ ',' +
//                    TabUzivAtr + '.' + TabUzivAtr_ExtEd_Zalozka + ',' +
//                    TabUzivAtr + '.' + TabUzivAtr_ExtEd_Poradi + ',' +
//                    TabUzivAtr + '.' + TabUzivAtr_NazevAtrSys);
//  end;
  Result := nil;
end;

{ --------------------------------------------------------------------------- }

// Q -> ziskano pomoci QueryProUzivatelskeAtributy
function JeUzivAttrPovolen(Q: TDataSet): Boolean;
begin
  Result := True;
//  if Server.JeToHeliosEasy then
//    if not HeliosEasy_JeHashOK(Q.Fields[16].AsString, TabUzivAtr,
//             BinaryToHexaString(Q.Fields[17].AsAnsiString)) then
//      Result := False;
end;

{ --------------------------------------------------------------------------- }

procedure VytvorDefiniciStavovehoSloupce(TD: PTabulkaDef; PA: PAtributTabulky);
  var
    PADef, PAX: PAtributTabulky;
    II: Integer;
    LSysName, LDefinice, T_A: string;
    SL, SLKonverze: TStringList;
    LPodminka: TPodminka;
    LPodminkaStr, LHodnotaStr, LIconIDStr: string;
begin
  SL := TStringList.Create;
  SLKonverze := TStringList.Create;
  try
    SL.Text := PA.Pocitany;
    PADef := nil;

    if SL.Count > 0 then
      LSysName := Trim(SL[0])
    else
      LSysName := '';

    if LSysName <> '' then
    begin
      // najdeme odpovidajici atribut
      PAX := TD.Atributy;
      for II := 1 to TD.PocetAtributu do
      begin
        if SameText(LSysName, PAX.JmenoSys) then
        begin
          PADef := PAX;
          Break;
        end;
        Inc(PAX);
      end;
    end;

    try
      if PADef <> nil then
        begin
          if (dvaDefinovany in PADef.DalsiVlastnostiAtr) and (PADef.Pocitany <> '') then
            T_A := PADef.JmenoSys
          else
            T_A := TD.JmenoSys + '.' + PADef.JmenoSys;

          // automaticke NULL
          LDefinice := '(case when ' + T_A + ' IS NULL then NULL';

          SLKonverze.Add('NULL');

          // podminky
          for II := 1 to SL.Count-2 do
          begin
            DekodujRadekDefiniceStavovehoSloupce(
              Trim(SL[II]), LPodminkaStr, LHodnotaStr, LIconIDStr);

            // podminka
            LPodminka := TPodminka(StrToInt(LPodminkaStr));
            LDefinice := LDefinice + ' when ' + T_A + ' ' + Podminka2SysStr(LPodminka);

            // hodnota
            case SkupinaAtributu(PADef.Typ) of
              skpRetezce:
                LDefinice := LDefinice + ' ' + NQuotedStr(UpravPodminkuLike(LPodminka, LHodnotaStr));

              skpDatumy:
                LDefinice := LDefinice + ' ' + QuotedStr(LHodnotaStr);

              else
                LDefinice := LDefinice + ' ' + LHodnotaStr;
            end;

            // ikona
            if SameText(LIconIDStr, 'NULL') then
              LDefinice := LDefinice + ' then NULL'
            else
              begin
                LDefinice := Format('%s then %s', [LDefinice, LIconIDStr]);
                if SLKonverze.IndexOf(LIconIDStr) = -1 then SLKonverze.Add(LIconIDStr);
              end;
          end;

          // ostatni hodnoty
          DekodujPosledniRadekDefiniceStavovehoSloupce(Trim(SL[SL.Count-1]), LIconIDStr);
          if SameText(LIconIDStr, 'NULL') then
            LDefinice := LDefinice + ' else NULL'
          else
            begin
              LDefinice := Format('%s else %s', [LDefinice, LIconIDStr]);
              if SLKonverze.IndexOf(LIconIDStr) = -1 then SLKonverze.Add(LIconIDStr);
            end;

          LDefinice := LDefinice + ' end)';

          if (dvaDefinovany in PADef.DalsiVlastnostiAtr) and (PADef.Pocitany <> '') then
          begin
            LDefinice :=
              '(select ' + LDefinice +
              ' from (select(' + PADef.Pocitany + ') AS ' + T_A + ') T)';
          end;

          for II := 0 to SLKonverze.Count-1 do
            SLKonverze[II] := SLKonverze[II] + '=';

          PA.Pocitany := LDefinice;
          PA.KonverzeStav := TextSOddelovacem(SLKonverze, #13);
        end
      else
        begin
          PA.Pocitany := 'NULL';
          PA.KonverzeStav := 'NULL=';
        end;
    except
      on E: Exception do
      begin
        {$IFDEF Ladit}
        sqlLadit(PA.JmenoSys + #13 + E.Message);
        {$ENDIF}
        PA.Pocitany := 'NULL';
        PA.KonverzeStav := 'NULL=';
      end;
    end;
  finally
    SLKonverze.Free;
    SL.Free;
  end;
end;

{ --------------------------------------------------------------------------- }

procedure ProjdiVsechnyStavoveSloupce(TD: PTabulkaDef);
  var
    PA: PAtributTabulky;
    II: Integer;
begin
  if TD = nil then Exit;

  // jdeme odzadu, je to efektivnejsi
  PA := TD.Atributy;
  if PA <> nil then
  begin
    Inc(PA, TD.PocetAtributu-1);
    II := TD.PocetAtributu;
    while II > 0 do
    begin
      if (dvaDefinovany in PA.DalsiVlastnostiAtr) and (Copy(PA.JmenoSys,1,1) = '_') then
        begin
          if _dvaStavovySloupec in PA.DalsiVlastnostiAtr then
            VytvorDefiniciStavovehoSloupce(TD, PA);
        end
      else
        Break;

      Dec(PA);
      Dec(II);
    end;
  end;
end;

{ --------------------------------------------------------------------------- }

// Q -> ziskano pomoci QueryProUzivatelskeAtributy
procedure NastavUzivatelskyAtribut(Q: TDataSet; PA: PAtributTabulky);
begin
  with PA^, Q do
  begin
    JmenoSys     := Fields[1].AsString;
    JmenoVerejne := Fields[2].AsString;
    //[TZ 27.03.2007] GUID na ve¯ejnÈ texty externÌch ¯eöenÌ
//    if JeStringGUID(JmenoVerejne) then
//      JmenoVerejne := sqlCtiOznamGUID(JmenoVerejne)
//    else
      if JmenoVerejne = '' then JmenoVerejne := JmenoSys;

    JmenoVerejneZkr := Fields[10].AsString;
//    if JeStringGUID(JmenoVerejneZkr) then
//      JmenoVerejneZkr := sqlCtiOznamGUID(JmenoVerejneZkr);

    Typ          := DefAtrStr2TypAtributu(Fields[3].AsString);
    MaskaDisplay := Fields[4].AsString;
    SirkaSloupce := Fields[5].AsInteger;
    Sumovat      := SameText(Fields[6].AsString, 'A');

    Konverze := Fields[12].AsString;
//    if JeStringGUID(Konverze) then // [RK 18.07.2008]
//      Konverze := sqlCtiOznamGUID(Konverze);

    // [RK 15.05.2009] podminka upravena na TSkupinaAtributuSDelkou
    //  if (Typ <> taText) and (SkupinaAtributu(Typ) = skpRetezce) then
    if Typ in TSkupinaAtributuSDelkou then
    begin
      case Typ of
        taNVarChar, taNChar:
          Delka := Fields[14].AsInteger div 2;
        else
          Delka := Fields[14].AsInteger;
      end;
    end;

    // [TZ 20.09.2002]
    if Fields[9].AsBoolean then
      begin //---EXTERNI---
        Pocitany :=
          Format(
            '(SELECT %s FROM %s_EXT WHERE ID=%1:s.ID)',
            [JmenoSys, Fields[0].AsString]);

         //[TZ 04.01.2006] zmÏna-d¯Ìve pro BIT bylo:SELECT ISNULL(_E,0) FROM..., teÔ je: ISNULL(SELECT _E FROM...,0)
        if Typ = taBoolean then
          Pocitany:= 'ISNULL(' + Pocitany + ',0)';

        DalsiVlastnostiAtr := [dvaDefinovany, _dvaExterniAtribut];
      end
    else
      begin //---UZIVATELSKY DEFINOVANY---
        Pocitany := Fields[7].AsString;
        DalsiVlastnostiAtr := [dvaDefinovany];
      end;

    // [RK 22.05.2008] procentni pruh
    // stejna podminka je i v sqView
    if Fields[11].AsBoolean and (Typ <> taBoolean) and (Konverze = '') and
       ((SkupinaAtributu(Typ) = skpCelaCisla) or
        (SkupinaAtributu(Typ) = skpDesetinnaCisla)) then
       Include(DalsiVlastnostiAtr, dvaProgressBar);

    // pri uprave pozor na sq_Ext !!
    if Fields[15].AsBoolean and (Typ = taInt) and (Konverze = '') then
      Include(DalsiVlastnostiAtr, dvaBarva);

    // [RK 24.02.2009] doplneno nastaveni viditelnosti
    if SameText(Fields[13].AsString, TabUzivAtr_VerejnyAtr_VAno_EAno) or
       SameText(Fields[13].AsString, TabUzivAtr_VerejnyAtr_VAno_ENe) then
      Verejny := vTrue
    else
      Verejny := vFalse;

    if Fields[8].AsString = 'N' then
      Include(DalsiVlastnostiAtr, dvaNeniVTiskuForm);

    // [RK 12.10.2010] skupina v Nastav podle nazvu zalozky
    Skupina := Fields[18].AsString;

    // [RK 10.11.2011] doplnen hint
    Bublina := Fields[19].AsString;
//    if JeStringGUID(Bublina) then
//      Bublina := sqlCtiOznamGUID(Bublina);

    // pri uprave pozor na sq_Ext !!
    if (Fields[20].AsBoolean or Fields[21].AsBoolean) and (Typ = taInt) then
    begin
      if Fields[21].AsBoolean then
        Include(DalsiVlastnostiAtr, _dvaStavovySloupec);
      Include(DalsiVlastnostiAtr, dvaStav);
      KonverzeStav := Konverze;
      Konverze := '';
    end;
  end;
end;

{ --------------------------------------------------------------------------- }

procedure PridejUzivatelskeAtributy;
  const
    //C_FlagUzivatelskyAtribut = #2;  // musÌ mÌt dÈlku 1

    // tyto 2 se musi lisit ! - jsou to zaraky do TriggerBeforeDelete, ktery se
    // dynamicky modifikuje, pokud ma tabulka EXTERNI atribut
    C_EXT_Zacatek = '/*EXT_*/';
    C_EXT_Konec   = '/*_EXT*/';
  var
    LNazevTabulkySys: String;
    LNasli: Boolean;
    LTabulka: TTabulka;
    PA: PAtributTabulky;
    JeStavovySloupec: Boolean;
    LOldTab: TTabulka;
    LOldTabStr, TypVE: String;
    LTriggerBeforeDelete: String;
    I, LPos1, LPos2: Integer;
    TriggerDoplnen: Boolean;
    Q: TDataSet;
  {$IFDEF Ladit}
  const
    G_UzByl: Boolean = False;
  {$ENDIF Ladit}

  { +++++++++++++++++++++++++++ }

  procedure DoplnTriggerBeforeDelete;
    var
      TD: PTabulkaDef;
  begin
    if not TriggerDoplnen then
    begin
      TD := ddTabulka_GetTabulkaDef(LTabulka);

      TD.TriggerBeforeDelete :=
        Format('%s'#13 + C_EXT_Zacatek + 'DELETE %s_EXT WHERE ID=:ID ' + C_EXT_Konec,
               [TD.TriggerBeforeDelete, LNazevTabulkySys]);
      TriggerDoplnen := True;
    end;
  end;

  { +++++++++++++++++++++++++++ }

begin
  //bagrovani pripadnych informaci o EXTERNICH atributech (uplatni se hlavne pri prepnuti do jine db, apod.)
  for LTabulka := Succ(Low(TTabulka)) to High_TTabulka do
  begin
    with ddTabulka_GetTabulkaDef(LTabulka)^ do
    begin
      //usr. def/ext slopce
      PA := Atributy;
      if Assigned(PA) then
      begin
        //musim odzadu
        Inc(PA, PocetAtributu-1);
        I := PocetAtributu;
        while I > 0 do
        begin
          if (dvaDefinovany in PA.DalsiVlastnostiAtr) and (Copy(PA.JmenoSys, 1, 1)='_') then
            begin
              Dec(PocetAtributu);
              {$IFDEF Ladit}
              //pri prvotnim spusteni HeIQ se to sem nesmi dostat - pokud ano je neco dost blbe [TZ]
              if not G_UzByl then
              begin
                MessageBox(0, PChar('Chybna definice atributu v tabulce '+ddTabulka_GetTabulkaDef(LTabulka).JmenoSys+' !'), 'LADIT', 0);
                Halt;
              end;
              {$ENDIF Ladit}
            end
          else
            Break;

          Dec(PA);
          Dec(I);
        end;
      end;

      // shod flag _dvtMaExterniAtributy
      Exclude(DalsiVlastnosti, _dvtMaExterniAtributy);

      // TriggerBeforeDelete
      LTriggerBeforeDelete := TriggerBeforeDelete;
      if LTriggerBeforeDelete <> '' then
      begin
        LPos1 := Pos(C_EXT_Zacatek, LTriggerBeforeDelete);
        if LPos1 > 0 then
        begin
          LPos2 := Pos(C_EXT_Konec, Copy(LTriggerBeforeDelete, LPos1, MaxInt));
          if LPos2 > 0 then
          begin
            System.Delete(LTriggerBeforeDelete, LPos1-1{kv˘li #13}, LPos2+Length(C_EXT_Konec));
            TriggerBeforeDelete := LTriggerBeforeDelete;
          end;
        end;
      end;
    end;
  end;
  {$IFDEF Ladit}
  G_UzByl := True; // nahozeni priznaku, ze v pristich pruchodech touto proc uz nekontrolovat
  {$ENDIF Ladit}

  LOldTab := tZadna;
  LOldTabStr := '';
  TriggerDoplnen := False;
  JeStavovySloupec := False;

  //samotnÈ naËÌt·ni info o uziv. def/ externich atributech
  Q := QueryProUzivatelskeAtributy('');
  try
    Q.Open;
    while not Q.EOF do  // iterace pres vsechny radky
    begin
      if JeUzivAttrPovolen(Q) then
      begin
        LNazevTabulkySys := Q.Fields[0].AsString;
        if Pos(sqlViewPrefix, LNazevTabulkySys) > 0 then
          LNasli := True // definovane prehledy se delaji jinde
        else
          begin
            LNasli := False;

            // dohledani odpovidajici tabulky
            if SameText(LNazevTabulkySys, LOldTabStr) then
              begin
                LTabulka := LOldTab;
                LNasli := True;
              end
            else
              begin
                // pri zmene tabulky doplnime stavove atributy
                if JeStavovySloupec then
                begin
                  ProjdiVsechnyStavoveSloupce(GetTabulkaDef(LTabulka));
                  JeStavovySloupec := False;
                end;

                LTabulka := Low(TTabulka);
                while LTabulka < High_TTabulka do
                begin
                  LTabulka := Succ(LTabulka);
                  if SameText(LNazevTabulkySys, ddTabulka_GetTabulkaDef(LTabulka).JmenoSys) then
                  begin
                    LNasli := True;
                    TriggerDoplnen := False; // jina tabulka, zrusime priznak
                    Break;
                  end;
                end;
              end;

            if LNasli then
            begin
              TypVE := Q.Fields[13].AsString;

              // pokud je to externi atribut
              if Q.Fields[9].AsBoolean then
              begin
                // musi se nastavit trigger pro mazani
                DoplnTriggerBeforeDelete;

                // pokud je editovatelny (EAno), tak nastavime priznak existence
                if SameText(TypVE, TabUzivAtr_VerejnyAtr_VAno_EAno) or
                   SameText(TypVE, TabUzivAtr_VerejnyAtr_VNe_EAno) then
                  Include(ddTabulka_GetTabulkaDef(LTabulka).DalsiVlastnosti, _dvtMaExterniAtributy);
              end;

              // verejne (VAno) zobrazime v Nastav
              // [RK 24.02.2009] podminka zrusena - aby bylo mozno zobrazit pres Alt+B v Nastav
              // nastaveni Verejny je ted v NastavUzivatelskyAtribut
  //            if SameText(TypVE, TabUzivAtr_VerejnyAtr_VAno_EAno) or
  //               SameText(TypVE, TabUzivAtr_VerejnyAtr_VAno_ENe) then
  //            begin
                { budu dÏlat Move }
                with ddTabulka_GetTabulkaDef(LTabulka)^ do
                begin
                  if LTabulka <> LOldTab then
                  begin
                    PA := Atributy;
                    Inc(PA, PocetAtributu-1);  // nynÌ PA ukazuje na poslednÌ atribut v definici
                    while (PA <> Atributy) and (Copy(PA.JmenoSys, 1, 1) = '_') do
                    begin  // hledej odzadu prvnÌ NEuûivatelsky definovan˝ atribut
                      Dec(PA);
                      Dec(PocetAtributu);
                    end;
                  end;

                  Inc(PocetAtributu);
                  PA := AllocMem(PocetAtributu * SizeOf(TAtributTabulky)); // AllocMem dÏl· FillChar(..., 0)
                  System.Move(Atributy^, PA^, (PocetAtributu-1) * SizeOf(TAtributTabulky));
                  if _dvtLzeFreeMem in DalsiVlastnosti then
                    FreeMem(Atributy)
                  else
                    Include(DalsiVlastnosti, _dvtLzeFreeMem);
                  Atributy := PA;
                  Inc(PA, PocetAtributu-1);

                  NastavUzivatelskyAtribut(Q, PA);
                  if _dvaStavovySloupec in PA.DalsiVlastnostiAtr then
                  begin
                    JeStavovySloupec := True;
                  end;
                end;
  //            end;

              // zapamatujeme si jakou tabulku jsme menili
              LOldTab := LTabulka;
              LOldTabStr := LNazevTabulkySys;
            end;
          end;

        // nemusi byt pres CtiOznam()
        if not LNasli then
          sqlChyba('Uûivatelsky definovanÈ sloupce - nenalezena tabulka ' + LNazevTabulkySys);
      end;

      Q.Next;
    end;
  finally
    Q.Free;
  end;

  // posledni tabulka neni detekovana zmenou
  if JeStavovySloupec then
    ProjdiVsechnyStavoveSloupce(GetTabulkaDef(LTabulka));

  //[TZ 16.10.2007]
  PridejRozpadNaEuro;
end;

{ --------------------------------------------------------------------------- }

{ --------------------------------------------------------------------------- }
{ vyznam APoradiAtr : 1 - EURO
                      2 - DUO
                      3 - Mena
                      4 - Mena DUO
}

function EURrozpadFaze2(ATabulka: TTabulka; AAtribut: String; APoradiAtr: Integer; ADvaPocitany: String = '') : string;
  const
    _LSKK = '''SKK''';
    _LEUR = '''EUR''';
    _ZaokrDeleni   = '4';   // LD 26.08.2008 zavedeni konstant pro zaokrouhleni deleni (-> EUR), SP, MS
    _ZaokrNasobeni = '2';   // LD 26.08.2008 zavedeni konstant pro zaokrouhleni nasobeni (-> SKK), SP, MS
  var
//    I   : integer;
    Flg : Boolean;
    Jmeno, JmenoLEUR : String;
begin
{$IfDef LADIT}
  if (APoradiAtr > 4) or (APoradiAtr < 0) then
    MessageBox(0, 'Spatny parametr ve funkci EURrozpadFaze2 !', 'LADIT', 0);
{$EndIf LADIT}
  Result := '';

  Flg := False;
//  for I := Low(EURrozpadVyjimky) to High(EURrozpadVyjimky) do
//    if (ATabulka = EURrozpadVyjimky[I].LTabulka) then
//    begin
//      Flg := True;
//      Break;
//    end;

//  Flg := Flg and Assigned(EURrozpadVyjimky[I].LFunkce) and
//         EURrozpadVyjimky[I].LFunkce(ATabulka, AAtribut, APoradiAtr, ADvaPocitany, Result);

  if not Flg then
  begin

    Jmeno := JmenoTabulky(ATabulka);
    JmenoLEUR := 'TabLEUR' + Copy(Jmeno,4);

    //test na view a docasne tabulky - jen vlastni rozpad, defaultni nechat prazdny
    if JeView(ATabulka)or(Copy(Jmeno,1,1)='#') then
    begin
      Result := 'NULL';
      Exit;
    end;

    // typ 1 a 2
    if ADvaPocitany = '' then
      case APoradiAtr of
        1 : Result :=
  '(CASE WHEN ' + JmenoLEUR + '.ID IS NULL '+
      'THEN ' + Jmeno + '.' + AAtribut + ' '+
      'ELSE (CASE WHEN ' + JmenoLEUR + '.' + AAtribut + ' IS NULL '+
               'THEN ROUND(' + Jmeno + '.' + AAtribut + '/' + Kurz_SKK_EUR_Str + ',' + _ZaokrDeleni + ') '+
               'ELSE ' + Jmeno + '.' + AAtribut + ' END)END)';
        2 : Result :=
  '(CASE WHEN ' + JmenoLEUR + '.ID IS NULL '+
      'THEN ROUND(' + Jmeno + '.' + AAtribut + '*' + Kurz_SKK_EUR_Str + ',' + _ZaokrNasobeni + ') '+
      'ELSE (CASE WHEN ' + JmenoLEUR + '.' + AAtribut + ' IS NULL '+
               'THEN ROUND(' + Jmeno + '.' + AAtribut + '/' + Kurz_SKK_EUR_Str + ',' + _ZaokrDeleni + ') '+
               'ELSE ' + JmenoLEUR + '.' + AAtribut + ' END)END)';
      end
    else
      case APoradiAtr of
        1 : Result :=
  '(CASE WHEN ' + JmenoLEUR + '.ID IS NULL '+
      'THEN (' + ADvaPocitany + ') '+
      'ELSE (CASE WHEN ' + JmenoLEUR + '.' + AAtribut + ' IS NULL '+
               'THEN ROUND((' + ADvaPocitany + ')/' + Kurz_SKK_EUR_Str + ',' + _ZaokrDeleni + ') '+
               'ELSE (' + ADvaPocitany + ') END)END)';
        2 : Result :=
  '(CASE WHEN ' + JmenoLEUR + '.ID IS NULL '+
      'THEN ROUND((' + ADvaPocitany + ')*' + Kurz_SKK_EUR_Str + ',' + _ZaokrNasobeni + ') '+
      'ELSE (CASE WHEN ' + JmenoLEUR + '.' + AAtribut + ' IS NULL '+
               'THEN ROUND((' + ADvaPocitany + ')/' + Kurz_SKK_EUR_Str + ',' + _ZaokrDeleni + ') '+
               'ELSE ' + JmenoLEUR + '.' + AAtribut + ' END)END)';
      end;

      // typ 3 a 4
    case APoradiAtr of
      3 : Result :=
  '(CASE WHEN ' + JmenoLEUR + '.ID IS NULL '+
      'THEN ' + _LEUR + ' ' +
      'ELSE (CASE WHEN ' + JmenoLEUR + '.' + AAtribut + ' IS NULL '+
               'THEN ' + _LSKK + ' ' +
               'ELSE ' + _LEUR + ' END)END)';
      4 : Result :=
  '(CASE WHEN ' + JmenoLEUR + '.ID IS NULL '+
      'THEN ' + _LSKK + ' ' +
      'ELSE (CASE WHEN ' + JmenoLEUR + '.' + AAtribut + ' IS NULL '+
               'THEN ' + _LEUR + ' ' +
               'ELSE ' + _LSKK + ' END)END)';
    end;
  end;
end;

{ --------------------------------------------------------------------------- }

//vr·tÌ True, v p¯ÌpadÏ, ûe se sloupce budou rozpadat
function BudeRozpadNaEuro: Boolean;
begin
  Result := False;
  if ((Server.Konstanty.FindField(TabHGlob_FazeEuro).value=1)or
      (Server.Konstanty.FindField(TabHGlob_FazeEuro).Value=2)or
      (Server.Konstanty.FindField(TabHGlob_FazeEuro).Value=3)) then
  begin
//    if SameText(Server.HlavniMena, 'SKK') then  // zruseno na zadost SP - MS 3.6.2008
      Result := True;
  end;
end;

procedure PridejRozpadNaEuro;
var
//  Koeficient : Extended;
  Kurz_EUR   : Extended;
  LTabulka   : TTabulka;
  PA, PApred, PAvzor : PAtributTabulky;
  I          : Integer;
//  KoeficientStr : String;
  Kurz_EUR_Str, LPom : String;
begin
  //nejprve zjisti koeficient; 0.0 -> nenÌ rozpad, sloupce vracÌ NULL a jsou neve¯ejnÈ
  Kurz_EUR := 0.0;
  if BudeRozpadNaEuro then
  begin
//    if SameText(Server.HlavniMena, 'SKK') then // zruseno na zadost SP - MS 3.6.2008
      Kurz_EUR := Kurz_SKK_EUR;
      // Koeficient := Koeficient_SKK_EUR;  //koeficient vyhl·öen˝ N·rodnÌ bankou (poËet desetinn˝ch mÌst musÌ korelovat s funkcÌ GetIndependentSQLFloat nÌûe)


    {---Tady budou postupnÏ dalöÌ mÏny---
    if SameText(Server.HlavniMena, 'CZK') then
      Koeficient := Koeficient_CZK_EUR;  //koeficient vyhl·öen˝ N·rodnÌ bankou (poËet desetinn˝ch mÌst musÌ korelovat s funkcÌ GetIndependentSQLFloat nÌûe)
    }
  end;
  Kurz_EUR_Str := GetIndependentSQLFloat('%.6f', Kurz_EUR);

  //cyklus pro vöechny tabulky
  for LTabulka := Succ(Low(TTabulka)) to High_TTabulka do
  begin
    with ddTabulka_GetTabulkaDef(LTabulka)^ do
    begin
      PA     := Atributy;
      PApred := nil;
      PAvzor := nil;
      if Assigned(PA) then
      begin
        I := PocetAtributu;
        while I > 0 do
        begin
          if (_dvaEURRozpad in PA.DalsiVlastnostiAtr) then
          begin
            PA.Verejny := vFalse;
            if Kurz_EUR = 0.0 then
              PA.Pocitany := 'NULL'
            else
            begin
              if Assigned(PApred) then
              begin
                PA.Sumovat := PApred.Sumovat;  //zdÏdit vlastnost Sumovat
                PA.Verejny := PApred.Verejny;  //kdyby n·hodou nebyl ve¯ejn˝
                LPom := Copy(PA.JmenoSys,Length(PA.JmenoSys)-1,2);
                if LPom='_E'
                then
                begin
                  // _E -----------
                  PAvzor := PApred;
                  if Server.Konstanty.FieldByName(TabHGlob_FazeEuro).Value = 1 then
                  begin
                    if (dvaDefinovany in PApred.DalsiVlastnostiAtr) then
                      PA.Pocitany := 'ROUND(' + '(' + PApred.Pocitany + ')/' + Kurz_EUR_Str + ',4)'
                    else
                      PA.Pocitany := 'ROUND(' + JmenoSys + '.' + PApred.JmenoSys + '/' + Kurz_EUR_Str + ',4)';
                  end
                  else
                    if Server.Konstanty.FieldByName(TabHGlob_FazeEuro).Value = 2 then
                    begin
                      if (dvaDefinovany in PApred.DalsiVlastnostiAtr) then
                        PA.Pocitany := EURrozpadFaze2(LTabulka, PApred.JmenoSys, 1, PApred.Pocitany)
                      else
                        PA.Pocitany := EURrozpadFaze2(LTabulka, PApred.JmenoSys, 1);
                    end
                    else
                      if (dvaDefinovany in PApred.DalsiVlastnostiAtr) then
                        PA.Pocitany := '(' + PApred.Pocitany + ')'
                      else
                        PA.Pocitany := '(' + JmenoSys + '.' + PApred.JmenoSys + ')';
                end
                else
                  if LPom='_D'
                  then
                  begin
                    // _D -----------
                    if Server.Konstanty.FieldByName(TabHGlob_FazeEuro).Value = 1 then
                    begin
                      if (dvaDefinovany in PAvzor.DalsiVlastnostiAtr) then
                        PA.Pocitany := 'ROUND(' + '(' + PAvzor.Pocitany + ')/' + Kurz_EUR_Str + ',4)'
                      else
                        PA.Pocitany := 'ROUND(' + JmenoSys + '.' + PAvzor.JmenoSys + '/' + Kurz_EUR_Str + ',4)';
                    end
                    else
                      if Server.Konstanty.FieldByName(TabHGlob_FazeEuro).Value = 2 then
                      begin
                        if (dvaDefinovany in PAvzor.DalsiVlastnostiAtr) then
                          PA.Pocitany := EURrozpadFaze2(LTabulka, PAvzor.JmenoSys, 2, PAvzor.Pocitany)
                        else
                          PA.Pocitany := EURrozpadFaze2(LTabulka, PAvzor.JmenoSys, 2);
                      end
                      else
                        if (dvaDefinovany in PAvzor.DalsiVlastnostiAtr) then
                          PA.Pocitany := 'ROUND(' + '(' + PAvzor.Pocitany + ')*' + Kurz_EUR_Str + ',4)'
                        else
                          PA.Pocitany := 'ROUND(' + JmenoSys + '.' + PAvzor.JmenoSys + '*' + Kurz_EUR_Str + ',4)';
                  end
                  else
                    if LPom='_M'
                    then
                    begin
                      // _M -----------
                      PA.Sumovat := False;
                      if Server.Konstanty.FieldByName(TabHGlob_FazeEuro).Value = 1
                      then PA.Pocitany := 'N''SKK'''
                      else
                        if Server.Konstanty.FieldByName(TabHGlob_FazeEuro).Value = 2 then
                        begin
                          if (dvaDefinovany in PAvzor.DalsiVlastnostiAtr) then
                            PA.Pocitany := EURrozpadFaze2(LTabulka, PAvzor.JmenoSys, 3, PAvzor.Pocitany)
                          else
                            PA.Pocitany := EURrozpadFaze2(LTabulka, PAvzor.JmenoSys, 3);
                        end
                        else PA.Pocitany := 'N''EUR''';
                    end
                    else
                      if LPom='_C'
                      then
                      begin
                        // _C -----------
                        PA.Sumovat := False;
                        if Server.Konstanty.FieldByName(TabHGlob_FazeEuro).Value = 1
                        then PA.Pocitany := 'N''EUR'''
                        else
                          if Server.Konstanty.FieldByName(TabHGlob_FazeEuro).Value = 2 then
                          begin
                            if (dvaDefinovany in PAvzor.DalsiVlastnostiAtr) then
                              PA.Pocitany := EURrozpadFaze2(LTabulka, PAvzor.JmenoSys, 4, PAvzor.Pocitany)
                            else
                              PA.Pocitany := EURrozpadFaze2(LTabulka, PAvzor.JmenoSys, 4);
                          end
                          else PA.Pocitany := 'N''SKK''';
                      end
                      else
                        raise Exception.Create('Chyba v eurorozpadu - pravdÏpodobnÏ nesoulad atribut˘!');  //toto by nemÏlo nikdy nastat, ale pro jistotu
              end
              else
                raise Exception.Create('Chyba v eurorozpadu - pravdÏpodobnÏ memory bug!');  //toto by nemÏlo nikdy nastat, ale pro jistotu
            end;
          end;
          PApred := PA;
          Inc(PA);
          Dec(I);
        end;
      end;
    end; //with ddTabulka_GetTabulkaDef(LTabulka)^
  end; //for LTabulka
end;

{ --------------------------------------------------------------------------- }

procedure RegenerujRozpadNaEuro(aTabulka : TTabulka);
var
  PA, PApred : PAtributTabulky;
  I          : Integer;
  RozpadNaEuroPovolen : Boolean;
begin
  RozpadNaEuroPovolen := BudeRozpadNaEuro;
  with GetTabulkaDef(aTabulka)^ do
  begin
    PA     := Atributy;
    PApred := nil;
    if Assigned(PA) then
    begin
      I := PocetAtributu;
      while I>0 do
      begin
        if (_dvaEURRozpad in PA.DalsiVlastnostiAtr) then  //takhle pozn·m, ûe je to ten _E sloupec
        begin
          PA.Verejny  := vFalse;
          if RozpadNaEuroPovolen then
          begin
            if Assigned(PApred) then
              PA.Verejny  := PApred.Verejny
            else
              raise Exception.Create('Chyba v eurorozpadu - pravdÏpodobnÏ memory bug!');  //toto by nemÏlo nikdy nastat, ale pro jistotu
          end;
        end;
        PApred := PA;
        Inc(PA);
        Dec(I);
      end;
    end;
  end; //with ddTabulka_GetTabulkaDef)]
end;

function Constraint2Hlaska(AJmenoConstraintu : String): TTxt;
var
  LTabulka : TTabulka;
  I        : Integer;
  PC       : PConstraintTabulky;
begin
  Result := txtNic;

  for LTabulka := Succ(Low(TTabulka)) to High_TTabulka do
  begin
    if ddTabulka_GetTabulkaDef(LTabulka) <> nil then
    begin
      PC := ddTabulka_GetTabulkaDef(LTabulka).Constraints;
      for I := 1 to ddTabulka_GetTabulkaDef(LTabulka).PocetConstraints do
      begin
        if SameText(PC.JmenoSys, AJmenoConstraintu) then
        begin
          Result := PC.ErrTxt;
          Exit;
        end;
        Inc(PC);
      end;
    end;
  end;
end;

{ --------------------------------------------------------------------------- }

//kolik bajt˘ (+bit˘) zabÌr· sloupec v datab·zovÈm souboru, kdyby byl plnÏ vyuûit
//p¯Ìklad: VARCHAR(100) vracÌ 102,125
//         protoûe 100B znaky + 2B dÈlka + 1 bit nullabilita
function MaxDelkaAtributu(PA                 : PAtributTabulky;
                          AUnicodeHypoteticky: Boolean = False //jakoby uû platilo, ûe [VAR]CHAR=N[VAR]CHAR
                         ): Extended;
begin
  Result := 0.0;
  if not Assigned(PA) then Exit;

  case PA.Typ of
    taInt,
    taIdentity     : Result := 4;
//    taBigInt       : Result := 8;
    taSmallInt     : Result := 2;
    taByte         : Result := 1;
    taBoolean      : Result := 1.0/8.0;
    taVarChar      : if AUnicodeHypoteticky then Result := (2 * PA.Delka) + 2
                                            else Result :=      PA.Delka  + 2;
    taChar         : if AUnicodeHypoteticky then Result :=  2 * PA.Delka
                                            else Result :=      PA.Delka;
    taBinary       : Result := PA.Delka;
    taText,
    taNText,
    taImage        : Result := 16;
    taDateTime     : Result := 8;
  //taMoney        : Result := 8;
    taNumeric_4_2  : Result := 5;
    taNumeric_5_2  : Result := 5;
    taNumeric_7_2  : Result := 5;
    taNumeric_9_2  : Result := 5;
    taNumeric_15_0 : Result := 9;
    taNumeric_19_2 : Result := 9;
    taNumeric_19_6 : Result := 9;
    taNumeric_20_6 : Result := 13;
    taNumeric_28_0 : Result := 13;
    taFloat        : Result := 8;
    taNVarChar     : Result := (2 * PA.Delka) + 2;
    taNChar        : Result :=  2 * PA.Delka;
    else raise Exception.Create('Nepokryt˝ typ sloupce ' + PA.JmenoSys + ' PA.Typ=' + IntToStr(Integer(PA.Typ)));
  end;

  Result := Result + 1.0/8.0;  //jeden slot (z osmi) pro nullabilitu
end;


{ ########################################################################### }

INITIALIZATION
{$IFDEF Ladit}
  KontrolaUA;
{$ENDIF}
  InicializaceDD;
{- P¯i¯adÌ tabulce v˝raz pro prim·rnÌ nebo unik·tnÌ klÌË}
  GetPrimUniqAttr;
END.
