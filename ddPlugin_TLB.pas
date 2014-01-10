unit ddPlugin_TLB;
{$IFDEF WIN32}
// ************************************************************************ //
// WARNING
// -------
// The types declared in this file were generated from data read from a
// Type Library. If this type library is explicitly or indirectly (via
// another type library referring to this type library) re-imported, or the
// 'Refresh' command of the Type Library Editor activated while editing the
// Type Library, the contents of this file will be regenerated and all
// manual modifications will be lost.
// ************************************************************************ //

// PASTLWTR : 1.2
// File generated on 03.01.2008 14:43:00 from Type Library described below.

// ************************************************************************  //
// Type Lib: C:\H\80\MsSQL\ddPlugin.tlb (1)
// LIBID: {54715545-9EB5-4996-8F52-495594E6F3F1}
// LCID: 0
// Helpfile:
// HelpString: Helios IQ Plugin Library
// DepndLst:
// (1) v2.0 stdole, (C:\WINDOWS\system32\stdole2.tlb)
// ************************************************************************ //
{$TYPEDADDRESS OFF} // Unit must be compiled without type-checked pointers.
{$WARN SYMBOL_PLATFORM OFF}
{$WRITEABLECONST ON}
{$VARPROPSETTER ON}

interface

uses ActiveX;

// *********************************************************************//
// GUIDS declared in the TypeLibrary. Following prefixes are used:
// Type Libraries     : LIBID_xxxx
// CoClasses          : CLASS_xxxx
// DISPInterfaces     : DIID_xxxx
// Non-DISP interfaces: IID_xxxx
// *********************************************************************//
const
  // TypeLibrary Major and minor versions
  ddPluginMajorVersion = 1;
  ddPluginMinorVersion = 0;

  LIBID_ddPlugin: TGUID = '{54715545-9EB5-4996-8F52-495594E6F3F1}';

  IID_IHePlugin: TGUID = '{EEEFDBB0-05CE-47AF-81AC-6DA8FA08A7B5}';
  IID_IHelios: TGUID = '{B14756BC-2A8D-4697-B744-530D82A628DA}';
  IID_IHeQuery: TGUID = '{BF709E36-6321-492B-9D76-9C712589FA24}';
  IID_IHeQueryField: TGUID = '{07A97F31-97D9-4324-9BA9-928D7F30838B}';

type

  // *********************************************************************//
  // Forward declaration of types defined in TypeLibrary
  // *********************************************************************//
  IHePlugin = interface;
  IHelios = interface;
  IHeQuery = interface;
  IHeQueryField = interface;

  // *********************************************************************//
  // Interface: IHePlugin
  // Flags:     (0)
  // GUID:      {EEEFDBB0-05CE-47AF-81AC-6DA8FA08A7B5}
  // *********************************************************************//
  IHePlugin = interface(IUnknown)
    ['{EEEFDBB0-05CE-47AF-81AC-6DA8FA08A7B5}']
    procedure Run(const Helios: IHelios); safecall;
  end;

  // *********************************************************************//
  // Interface: IHelios
  // Flags:     (0)
  // GUID:      {B14756BC-2A8D-4697-B744-530D82A628DA}
  // *********************************************************************//
  IHelios = interface(IUnknown)
    ['{B14756BC-2A8D-4697-B744-530D82A628DA}']
    function CurrentRecordID: OleVariant; safecall;
    function SelectedRecordIDs: WideString; safecall;
    function BrowseID: Integer; safecall;
    function Sklad: WideString; safecall;
    function Sbornik: WideString; safecall;
    function Pokladna: WideString; safecall;
    function DatumTPV: TDateTime; safecall;
    function Obdobi: Integer; safecall;
    function MzdObd: Integer; safecall;
    function Schranka: WideString; safecall;
    function KategorieKJ: WideString; safecall;
    function CU: Integer; safecall;
    function Font: WideString; safecall;
    function FontHeight: Integer; safecall;
    function Charset: Byte; safecall;
    procedure ExecSQL(const SQL: WideString); safecall;
    function OpenSQL(const SQL: WideString): IHeQuery; safecall;
    procedure Refresh(CelehoBrowse: WordBool); safecall;
    function Locate(const KeyFields: WideString; KeyValues: OleVariant): WordBool; safecall;
    function Prenos(BrowseID: Integer; const Atribut: WideString; var Hodnota: OleVariant; const WhereSys: WideString;
      const TitulekOkna: WideString; AutoSet: WordBool): WordBool; safecall;
    function MainApplicationHandle: Integer; safecall;
    function MainApplicationIconHandle: Integer; safecall;
    procedure Info(const Msg: WideString); safecall;
    procedure Error(const Msg: WideString); safecall;
    function YesNo(const Msg: WideString; DefaultYes: WordBool): WordBool; safecall;
    procedure OpenBrowse(BrowseID: Integer; const WhereSys: WideString); safecall;
    procedure EditBrowse(BrowseID: Integer; const WhereSys: WideString); safecall;
    function Language: Byte; safecall;
    function LoginName: WideString; safecall;
    function FullName: WideString; safecall;
    function UserId: Smallint; safecall;
    function AttrPublicName(const TableDotAttr: WideString): WideString; safecall;
    function Konverze(const TableDotAttr: WideString): WideString; safecall;
    function QueryEdit: IHeQuery; safecall;
    function HeVersion: Largeuint; safecall;
    function SQLVersion: Integer; safecall;
    function ExtKomID: Integer; safecall;
    function YesNoCancel(const Msg: WideString; DefaultYes: WordBool): Byte; safecall;
    function QueryBrowse: IHeQuery; safecall;
    function MainBrowseTable: WideString; safecall;
    function PrintForm(IdForm: Integer): WordBool; safecall;
    procedure DocasnaTabulkaZOznacenych(const TableName: WideString; Poradi: WordBool); safecall;
    function HeliosVlastnik: IHelios; safecall;
    function Ident: Integer; safecall;
    function Prenos2(BrowseID: Integer; const Atribut1: WideString; const Atribut2: WideString;
      var Hodnota1: OleVariant; var Hodnota2: OleVariant; const AWhereSys: WideString; const TitulekOkna: WideString;
      AutoSet: WordBool; MultiSelect: WordBool; ReadOnly: WordBool; PodleKolikaLocate: Integer): WordBool; safecall;
    function AutoImport(Typ: Integer; const FileName: WideString): WordBool; safecall;
    function SerNum: WideString; safecall;
    function MaxNumDBs: Integer; safecall;
    function MaxNumUsers: Integer; safecall;
    procedure ExecSQL2(const SQL: WideString; Stupen: Integer); safecall;
    function OpenSQL2(const SQL: WideString; Stupen: Integer): IHeQuery; safecall;
    function PrintForm2(IdForm: Integer; const WhereSys: WideString): WordBool; safecall;
    procedure RegisteredPluginCheck; safecall;
    function SystemDB: WideString; safecall;
    function PrintForm3(BrowseID: Integer; IdForm: Integer; const WhereSys: WideString): WordBool; safecall;
    function RadaUctenek: WideString; safecall;
    function MaDatumPohl: TDateTime; safecall;
    function IntrUnits: Integer; safecall;
    procedure SetSklad(const Sklad: WideString); safecall;
    procedure SetSbornik(const Sbornik: WideString); safecall;
    procedure SetPokladna(const Pokladna: WideString); safecall;
    procedure SetDatumTPV(DatumTPV: TDateTime); safecall;
    procedure SetObdobi(Obdobi: Integer); safecall;
    procedure SetMzdObd(MzdObd: Integer); safecall;
    procedure SetSchranka(const Schranka: WideString); safecall;
    procedure SetKategorieKJ(const KategorieKJ: WideString); safecall;
    procedure SetCU(CU: Integer); safecall;
    procedure SetRadaUctenek(const RadaUctenek: WideString); safecall;
    procedure SetMaDatumPohl(MaDatumPohl: TDateTime); safecall;
    procedure SetIntrUnits(IntrUnits: Integer); safecall;
  end;

  // *********************************************************************//
  // Interface: IHeQuery
  // Flags:     (0)
  // GUID:      {BF709E36-6321-492B-9D76-9C712589FA24}
  // *********************************************************************//
  IHeQuery = interface(IUnknown)
    ['{BF709E36-6321-492B-9D76-9C712589FA24}']
    function FieldValues(Field: Integer): OleVariant; safecall;
    function FieldByNameValues(const FieldName: WideString): OleVariant; safecall;
    function EOF: WordBool; safecall;
    procedure Next; safecall;
    procedure First; safecall;
    function RecordCount: Integer; safecall;
    function FieldCount: Integer; safecall;
    procedure Prev; safecall;
    function Fields(Index: Integer): IHeQueryField; safecall;
    function FieldByName(const FieldName: WideString): IHeQueryField; safecall;
    function FindField(const FieldName: WideString): IHeQueryField; safecall;
    function Locate(const KeyFields: WideString; KeyValues: OleVariant): WordBool; safecall;
    function BOF: WordBool; safecall;
  end;

  // *********************************************************************//
  // Interface: IHeQueryField
  // Flags:     (0)
  // GUID:      {07A97F31-97D9-4324-9BA9-928D7F30838B}
  // *********************************************************************//
  IHeQueryField = interface(IUnknown)
    ['{07A97F31-97D9-4324-9BA9-928D7F30838B}']
    function FieldName: WideString; safecall;
    function Value: OleVariant; safecall;
    function DisplayName: WideString; safecall;
    function Visible: WordBool; safecall;
  end;

implementation

uses ComObj;

{$ELSE}

interface

uses System.Variants;

type
  IHeQueryField = class(TObject)
  private
  public
    constructor Create;

    function FieldName: WideString;
    function Value: OleVariant;
    function DisplayName: WideString;
    function Visible: WordBool;
  end;

  IHeQuery = class(TObject)
  private
  public
    constructor Create;

    function FieldValues(Field: Integer): OleVariant;
    function FieldByNameValues(const FieldName: WideString): OleVariant;
    function EOF: WordBool;
    procedure Next;
    procedure First;
    function RecordCount: Integer;
    function FieldCount: Integer;
    procedure Prev;
    function Fields(Index: Integer): IHeQueryField;
    function FieldByName(const FieldName: WideString): IHeQueryField;
    function FindField(const FieldName: WideString): IHeQueryField;
    function Locate(const KeyFields: WideString; KeyValues: OleVariant): WordBool;
    function BOF: WordBool;
  end;

  IHelios = class(TObject)
  private
  public
    constructor Create;

    function CurrentRecordID: OleVariant;
    function SelectedRecordIDs: WideString;
    function BrowseID: Integer;
    function Sklad: WideString;
    function Sbornik: WideString;
    function Pokladna: WideString;
    function DatumTPV: TDateTime;
    function Obdobi: Integer;
    function MzdObd: Integer;
    function Schranka: WideString;
    function KategorieKJ: WideString;
    function CU: Integer;
    function Font: WideString;
    function FontHeight: Integer;
    function Charset: Byte;
    procedure ExecSQL(const SQL: WideString);
    function OpenSQL(const SQL: WideString): IHeQuery;
    procedure Refresh(CelehoBrowse: WordBool);
    function Locate(const KeyFields: WideString; KeyValues: OleVariant): WordBool;
    function Prenos(BrowseID: Integer; const Atribut: WideString; var Hodnota: OleVariant; const WhereSys: WideString;
      const TitulekOkna: WideString; AutoSet: WordBool): WordBool;
    function MainApplicationHandle: Integer;
    function MainApplicationIconHandle: Integer;
    procedure Info(const Msg: WideString);
    procedure Error(const Msg: WideString);
    function YesNo(const Msg: WideString; DefaultYes: WordBool): WordBool;
    procedure OpenBrowse(BrowseID: Integer; const WhereSys: WideString);
    procedure EditBrowse(BrowseID: Integer; const WhereSys: WideString);
    function Language: Byte;
    function LoginName: WideString;
    function FullName: WideString;
    function UserId: Smallint;
    function AttrPublicName(const TableDotAttr: WideString): WideString;
    function Konverze(const TableDotAttr: WideString): WideString;
    function QueryEdit: IHeQuery;
    function HeVersion: LongInt;
    function SQLVersion: Integer;
    function ExtKomID: Integer;
    function YesNoCancel(const Msg: WideString; DefaultYes: WordBool): Byte;
    function QueryBrowse: IHeQuery;
    function MainBrowseTable: WideString;
    function PrintForm(IdForm: Integer): WordBool;
    procedure DocasnaTabulkaZOznacenych(const TableName: WideString; Poradi: WordBool);
    function HeliosVlastnik: IHelios;
    function Ident: Integer;
    function Prenos2(BrowseID: Integer; const Atribut1: WideString; const Atribut2: WideString;
      var Hodnota1: OleVariant; var Hodnota2: OleVariant; const AWhereSys: WideString; const TitulekOkna: WideString;
      AutoSet: WordBool; MultiSelect: WordBool; ReadOnly: WordBool; PodleKolikaLocate: Integer): WordBool;
    function AutoImport(Typ: Integer; const FileName: WideString): WordBool;
    function SerNum: WideString;
    function MaxNumDBs: Integer;
    function MaxNumUsers: Integer;
    procedure ExecSQL2(const SQL: WideString; Stupen: Integer);
    function OpenSQL2(const SQL: WideString; Stupen: Integer): IHeQuery;
    function PrintForm2(IdForm: Integer; const WhereSys: WideString): WordBool;
    procedure RegisteredPluginCheck;
    function SystemDB: WideString;
    function PrintForm3(BrowseID: Integer; IdForm: Integer; const WhereSys: WideString): WordBool;
    function RadaUctenek: WideString;
    function MaDatumPohl: TDateTime;
    function IntrUnits: Integer;
    procedure SetSklad(const Sklad: WideString);
    procedure SetSbornik(const Sbornik: WideString);
    procedure SetPokladna(const Pokladna: WideString);
    procedure SetDatumTPV(DatumTPV: TDateTime);
    procedure SetObdobi(Obdobi: Integer);
    procedure SetMzdObd(MzdObd: Integer);
    procedure SetSchranka(const Schranka: WideString);
    procedure SetKategorieKJ(const KategorieKJ: WideString);
    procedure SetCU(CU: Integer);
    procedure SetRadaUctenek(const RadaUctenek: WideString);
    procedure SetMaDatumPohl(MaDatumPohl: TDateTime);
    procedure SetIntrUnits(IntrUnits: Integer);
  end;

implementation

// * IHeQueryField *//
constructor IHeQueryField.Create;
begin
end;

function IHeQueryField.FieldName: WideString;
begin
end;

function IHeQueryField.Value: OleVariant;
begin
end;

function IHeQueryField.DisplayName: WideString;
begin
end;

function IHeQueryField.Visible: WordBool;
begin
end;

// * IHeQuery *//
constructor IHeQuery.Create;
begin
end;

function IHeQuery.FieldValues(Field: Integer): OleVariant;
begin
end;

function IHeQuery.FieldByNameValues(const FieldName: WideString): OleVariant;
begin
end;

function IHeQuery.EOF: WordBool;
begin
end;

procedure IHeQuery.Next;
begin
end;

procedure IHeQuery.First;
begin
end;

function IHeQuery.RecordCount: Integer;
begin
end;

function IHeQuery.FieldCount: Integer;
begin
end;

procedure IHeQuery.Prev;
begin
end;

function IHeQuery.Fields(Index: Integer): IHeQueryField;
begin
end;

function IHeQuery.FieldByName(const FieldName: WideString): IHeQueryField;
begin
end;

function IHeQuery.FindField(const FieldName: WideString): IHeQueryField;
begin
end;

function IHeQuery.Locate(const KeyFields: WideString; KeyValues: OleVariant): WordBool;
begin
end;

function IHeQuery.BOF: WordBool;
begin
end;

// * IHelios *//
constructor IHelios.Create;
begin
end;

function IHelios.CurrentRecordID: OleVariant;
begin
end;

function IHelios.SelectedRecordIDs: WideString;
begin
end;

function IHelios.BrowseID: Integer;
begin
end;

function IHelios.Sklad: WideString;
begin
end;

function IHelios.Sbornik: WideString;
begin
end;

function IHelios.Pokladna: WideString;
begin
end;

function IHelios.DatumTPV: TDateTime;
begin
end;

function IHelios.Obdobi: Integer;
begin
end;

function IHelios.MzdObd: Integer;
begin
end;

function IHelios.Schranka: WideString;
begin
end;

function IHelios.KategorieKJ: WideString;
begin
end;

function IHelios.CU: Integer;
begin
end;

function IHelios.Font: WideString;
begin
end;

function IHelios.FontHeight: Integer;
begin
end;

function IHelios.Charset: Byte;
begin
end;

procedure IHelios.ExecSQL(const SQL: WideString);
begin
end;

function IHelios.OpenSQL(const SQL: WideString): IHeQuery;
begin
end;

procedure IHelios.Refresh(CelehoBrowse: WordBool);
begin
end;

function IHelios.Locate(const KeyFields: WideString; KeyValues: OleVariant): WordBool;
begin
end;

function IHelios.Prenos(BrowseID: Integer; const Atribut: WideString; var Hodnota: OleVariant;
  const WhereSys: WideString; const TitulekOkna: WideString; AutoSet: WordBool): WordBool;
begin
end;

function IHelios.MainApplicationHandle: Integer;
begin
end;

function IHelios.MainApplicationIconHandle: Integer;
begin
end;

procedure IHelios.Info(const Msg: WideString);
begin
end;

procedure IHelios.Error(const Msg: WideString);
begin
end;

function IHelios.YesNo(const Msg: WideString; DefaultYes: WordBool): WordBool;
begin
end;

procedure IHelios.OpenBrowse(BrowseID: Integer; const WhereSys: WideString);
begin
end;

procedure IHelios.EditBrowse(BrowseID: Integer; const WhereSys: WideString);
begin
end;

function IHelios.Language: Byte;
begin
end;

function IHelios.LoginName: WideString;
begin
end;

function IHelios.FullName: WideString;
begin
end;

function IHelios.UserId: Smallint;
begin
end;

function IHelios.AttrPublicName(const TableDotAttr: WideString): WideString;
begin
end;

function IHelios.Konverze(const TableDotAttr: WideString): WideString;
begin
end;

function IHelios.QueryEdit: IHeQuery;
begin
end;

function IHelios.HeVersion: LongInt;
begin
end;

function IHelios.SQLVersion: Integer;
begin
end;

function IHelios.ExtKomID: Integer;
begin
end;

function IHelios.YesNoCancel(const Msg: WideString; DefaultYes: WordBool): Byte;
begin
end;

function IHelios.QueryBrowse: IHeQuery;
begin
end;

function IHelios.MainBrowseTable: WideString;
begin
end;

function IHelios.PrintForm(IdForm: Integer): WordBool;
begin
end;

procedure IHelios.DocasnaTabulkaZOznacenych(const TableName: WideString; Poradi: WordBool);
begin
end;

function IHelios.HeliosVlastnik: IHelios;
begin
end;

function IHelios.Ident: Integer;
begin
end;

function IHelios.Prenos2(BrowseID: Integer; const Atribut1: WideString; const Atribut2: WideString;
  var Hodnota1: OleVariant; var Hodnota2: OleVariant; const AWhereSys: WideString; const TitulekOkna: WideString;
  AutoSet: WordBool; MultiSelect: WordBool; ReadOnly: WordBool; PodleKolikaLocate: Integer): WordBool;
begin
end;

function IHelios.AutoImport(Typ: Integer; const FileName: WideString): WordBool;
begin
end;

function IHelios.SerNum: WideString;
begin
end;

function IHelios.MaxNumDBs: Integer;
begin
end;

function IHelios.MaxNumUsers: Integer;
begin
end;

procedure IHelios.ExecSQL2(const SQL: WideString; Stupen: Integer);
begin
end;

function IHelios.OpenSQL2(const SQL: WideString; Stupen: Integer): IHeQuery;
begin
end;

function IHelios.PrintForm2(IdForm: Integer; const WhereSys: WideString): WordBool;
begin
end;

procedure IHelios.RegisteredPluginCheck;
begin
end;

function IHelios.SystemDB: WideString;
begin
end;

function IHelios.PrintForm3(BrowseID: Integer; IdForm: Integer; const WhereSys: WideString): WordBool;
begin
end;

function IHelios.RadaUctenek: WideString;
begin
end;

function IHelios.MaDatumPohl: TDateTime;
begin
end;

function IHelios.IntrUnits: Integer;
begin
end;

procedure IHelios.SetSklad(const Sklad: WideString);
begin
end;

procedure IHelios.SetSbornik(const Sbornik: WideString);
begin
end;

procedure IHelios.SetPokladna(const Pokladna: WideString);
begin
end;

procedure IHelios.SetDatumTPV(DatumTPV: TDateTime);
begin
end;

procedure IHelios.SetObdobi(Obdobi: Integer);
begin
end;

procedure IHelios.SetMzdObd(MzdObd: Integer);
begin
end;

procedure IHelios.SetSchranka(const Schranka: WideString);
begin
end;

procedure IHelios.SetKategorieKJ(const KategorieKJ: WideString);
begin
end;

procedure IHelios.SetCU(CU: Integer);
begin
end;

procedure IHelios.SetRadaUctenek(const RadaUctenek: WideString);
begin
end;

procedure IHelios.SetMaDatumPohl(MaDatumPohl: TDateTime);
begin
end;

procedure IHelios.SetIntrUnits(IntrUnits: Integer);
begin
end;

{$ENDIF}

end.
