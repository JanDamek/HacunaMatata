unit DefEditAttrUnit;

interface

uses ddPlugin_TLB, VirtualTable, deType, ddType;

type
  // kolekce vsech atributu dostupnych pro definovane editory
  TDefEditAttr = class(TVirtualTable)
  private
    fHelios: IHelios;
    function getGUID: TGUID;
    function getTyp: TTypDefEditAtr;
    procedure setGUID(const Value: TGUID);
    procedure setTyp(const Value: TTypDefEditAtr);
    function getBrowse: TBrowse;
    function getDelka: Integer;
    function getJenomCteni: Boolean;
    function getJmenoSys: string;
    function getonButton: String;
    function getonEnter: String;
    function getonExit: String;
    function getonShow: String;
    function getonValidate: String;
    function getPopis: string;
    function getSkupina: string;
    function getTabulka: TTabulka;
    function getVazba: TVztah;
    function getVerejny: Boolean;
    function getZalozka: string;
    procedure setBrowse(const Value: TBrowse);
    procedure setDelka(const Value: Integer);
    procedure setJenomCteni(const Value: Boolean);
    procedure setJmenoSys(const Value: string);
    procedure setonButton(const Value: String);
    procedure setonEnter(const Value: String);
    procedure setonExit(const Value: String);
    procedure setonShow(const Value: String);
    procedure setonValidate(const Value: String);
    procedure setPopis(const Value: string);
    procedure setSkupina(const Value: string);
    procedure setTabulka(const Value: TTabulka);
    procedure setVazba(const Value: TVztah);
    procedure setVerejny(const Value: Boolean);
    procedure setZalozka(const Value: string);

    function PrelozHlasku(s: String): String;
    function getStringGUID: String;
    procedure setStringGUID(const Value: String);
    function getTableName: String;
    function getonButtonUser: String;
    function getonEnterUser: String;
    function getonExitUser: String;
    function getonShowUser: String;
    function getonValidateUser: String;
    procedure setonButtonUser(const Value: String);
    procedure setonEnterUser(const Value: String);
    procedure setonExitUser(const Value: String);
    procedure setonShowUser(const Value: String);
    procedure setonValidateUser(const Value: String);

    procedure checkSQL;
    procedure loadAttr;
    function getPovoleneBID: String;
    procedure setPovoleneBID(const Value: String);
  public

    // prace nad seznamem atributu
    function findGUID(aGUID: TGUID): Boolean; overload;
    function findGUID(aGUID: String): Boolean; overload;

    function isGUID(aGUID: TGUID): Boolean; overload;
    function isGUID(aGUID: String): Boolean; overload;

    function GUIDToJmenoSys(s: String): String;

    function findAttrFromJmenoSysInGUIDList(aJmenoSys, aGUIDList: String): Boolean;

    // proces ulozeni do SQL
    procedure Post; override;
  published
    constructor Create(aHelios: IHelios); virtual;

    // polozky atributu
    property GUID: TGUID read getGUID write setGUID;
    property StringGUID: String read getStringGUID write setStringGUID;
    property Typ: TTypDefEditAtr read getTyp write setTyp;
    property Tabulka: TTabulka read getTabulka write setTabulka;
    property TableName: String read getTableName;
    property JmenoSys: string read getJmenoSys write setJmenoSys;
    property Delka: Integer read getDelka write setDelka;
    property Browse: TBrowse read getBrowse write setBrowse;
    property Vazba: TVztah read getVazba write setVazba;

    property JenomCteni: Boolean read getJenomCteni write setJenomCteni;
    property Verejny: Boolean read getVerejny write setVerejny;
    // textove popisky
    property Zalozka: string read getZalozka write setZalozka;
    property Skupina: string read getSkupina write setSkupina;
    property Popis: string read getPopis write setPopis;

    // SQL eventy atributu
    property onShow: String read getonShow;
    property onEnter: String read getonEnter;
    property onExit: String read getonExit;
    property onValidate: String read getonValidate;
    property onButton: String read getonButton;

    property onShowUser: String read getonShowUser write setonShowUser;
    property onEnterUser: String read getonEnterUser write setonEnterUser;
    property onExitUser: String read getonExitUser write setonExitUser;
    property onValidateUser: String read getonValidateUser write setonValidateUser;
    property onButtonUser: String read getonButtonUser write setonButtonUser;

    // seznam povolenych browse, string oddeleny carkou s BID
    property povoleneBID: String read getPovoleneBID;
  end;

var
  DefAttr: TDefEditAttr;

implementation

uses ddMain, SysUtils, sqString, Data.db, MemData, sqProc, Classes, deEditory;

{ TDefEditAttr }

procedure TDefEditAttr.checkSQL;
var
  SQL: String;
begin
  SQL := 'IF OBJECT_ID(''dbo.TabHM_browse_attr'')IS NOT NULL SELECT 1 ELSE SELECT 0';

  with fHelios.OpenSQL(SQL) do
  begin
    // neni definovan, nutno vytvorit
    if FieldValues(0) = 0 then
    begin
      SQL := 'CREATE TABLE [dbo].[TabHM_browse_attr]('#13 + '[GUID] [nvarchar] (38) NOT NULL,'#13 +
        '[browse_def] [int] NOT NULL,'#13 + '[JmenoSys] [nchar](100) NOT NULL,'#13 + '[Typ] [smallint] NOT NULL,'#13 +
        '[Browse_DPSN] [nchar](38) NULL,'#13 + '[JenomCteni] [tinyint] NOT NULL,'#13 + '[Zalozka] [nchar](35) NULL,'#13
        + '[Skupina] [nchar](35) NULL,'#13 + '[Popis] [nchar](100) NULL,'#13 + '[onShow] [text] NULL,'#13 +
        '[onEnter] [text] NULL,'#13 + '[onExit] [text] NULL,'#13 + '[onValidate] [text] NULL,'#13 +
        '[onButton] [text] NULL,'#13 + 'CONSTRAINT [PK_TabHM_browse_attr] PRIMARY KEY CLUSTERED'#13 + '('#13 +
        '	[GUID] ASC'#13 +
        ')WITH (PAD_INDEX  = OFF, STATISTICS_NORECOMPUTE  = OFF, IGNORE_DUP_KEY = OFF, ALLOW_ROW_LOCKS  = ON, ALLOW_PAGE_LOCKS  = ON) ON [PRIMARY]'#13
        + ') ON [PRIMARY] TEXTIMAGE_ON [PRIMARY]';
      fHelios.ExecSQL(SQL);

      SQL := 'ALTER TABLE [dbo].[TabHM_browse_attr] ADD  CONSTRAINT [DF_TabHM_browse_attr_JenomCteni]  DEFAULT ((0)) FOR [JenomCteni]';
      fHelios.ExecSQL(SQL);

    end;
  end;
end;

constructor TDefEditAttr.Create(aHelios: IHelios);
begin
  inherited Create(nil);
  fHelios := aHelios;

  AddField('GUID', ftString, 38);
  AddField('Typ', ftInteger);
  AddField('Tabulka', ftInteger);
  AddField('JmenoSys', ftString, 255);
  AddField('Delka', ftInteger);
  AddField('Browse', ftInteger);
  AddField('Vazba', ftInteger);
  AddField('JenomCteni', ftBoolean);
  AddField('Verejny', ftBoolean);
  AddField('Zalozka', ftString, 255);
  AddField('Skupina', ftString, 255);
  AddField('Popis', ftString, 255);
  AddField('onShow', ftMemo);
  AddField('onEnter', ftMemo);
  AddField('onExit', ftMemo);
  AddField('onValidate', ftMemo);
  AddField('onButton', ftMemo);
  AddField('onShowUser', ftMemo);
  AddField('onEnterUser', ftMemo);
  AddField('onExitUser', ftMemo);
  AddField('onValidateUser', ftMemo);
  AddField('onButtonUser', ftMemo);
  AddField('PovoleneBID', ftMemo);

  Open;

  checkSQL;
  loadAttr;
end;

function TDefEditAttr.findGUID(aGUID: TGUID): Boolean;
begin
  Result := findGUID(GUIDToString(aGUID));
end;

function TDefEditAttr.findGUID(aGUID: String): Boolean;
begin
  Result := false;
  First;
  while not(Eof or Result) do
  begin
    Result := StringGUID = aGUID;
    if not Result then
      next;
  end;
end;

function TDefEditAttr.findAttrFromJmenoSysInGUIDList(aJmenoSys, aGUIDList: String): Boolean;
var
  l_sys, l_guid: TStringList;
  i: Integer;
begin
  l_sys := TStringList.Create;
  l_sys.CommaText := StringReplace(GUIDToJmenoSys(aGUIDList), #13, ',', [rfReplaceAll]);
  l_sys.Delete(0);

  i := l_sys.IndexOf(aJmenoSys);
  if (i >= 0) and (i < l_sys.Count) and (l_sys[i] = aJmenoSys) then
  begin
    l_guid := TStringList.Create;
    l_guid.CommaText := aGUIDList;
    Result := findGUID(l_guid[i]);
    l_guid.Free;
  end
  else
    Result := false;
  l_sys.Free;
end;

function TDefEditAttr.getPovoleneBID: String;
begin
  Result := FieldByName('PovoleneBID').AsString;
end;

function TDefEditAttr.getBrowse: TBrowse;
begin
  Result := TBrowse(FieldByName('Browse').AsInteger);
end;

function TDefEditAttr.getDelka: Integer;
begin
  Result := FieldByName('Delka').AsInteger;
end;

function TDefEditAttr.getGUID: TGUID;
begin
  Result := StringToGUID(FieldByName('GUID').AsString);
end;

function TDefEditAttr.getJenomCteni: Boolean;
begin
  Result := FieldByName('JenomCteni').AsBoolean;
end;

function TDefEditAttr.getJmenoSys: string;
begin
  Result := FieldByName('JmenoSys').AsString;
end;

function TDefEditAttr.getonButton: String;
begin
  Result := FieldByName('onButton').AsString;
end;

function TDefEditAttr.getonButtonUser: String;
begin
  Result := FieldByName('onButtonUser').AsString;
end;

function TDefEditAttr.getonEnter: String;
begin
  Result := FieldByName('onEnter').AsString;
end;

function TDefEditAttr.getonEnterUser: String;
begin
  Result := FieldByName('onEnterUser').AsString;
end;

function TDefEditAttr.getonExit: String;
begin
  Result := FieldByName('onExit').AsString;
end;

function TDefEditAttr.getonExitUser: String;
begin
  Result := FieldByName('onExitUser').AsString;
end;

function TDefEditAttr.getonShow: String;
begin
  Result := FieldByName('onShow').AsString;
end;

function TDefEditAttr.getonShowUser: String;
begin
  Result := FieldByName('onShowUser').AsString;
end;

function TDefEditAttr.getonValidate: String;
begin
  Result := FieldByName('onValidate').AsString;
end;

function TDefEditAttr.getonValidateUser: String;
begin
  Result := FieldByName('onValidateUser').AsString;
end;

function TDefEditAttr.getPopis: string;
begin
  Result := PrelozHlasku(FieldByName('Popis').AsString);
end;

function TDefEditAttr.getSkupina: string;
begin
  Result := PrelozHlasku(FieldByName('Skupina').AsString);
end;

function TDefEditAttr.getStringGUID: String;
begin
  Result := FieldByName('GUID').AsString;
end;

function TDefEditAttr.getTableName: String;
begin
  Result := JmenoTabulky(Tabulka);
end;

function TDefEditAttr.getTabulka: TTabulka;
begin
  Result := TTabulka(FieldByName('Tabulka').AsInteger);
end;

function TDefEditAttr.getTyp: TTypDefEditAtr;
begin
  Result := TTypDefEditAtr(FieldByName('Typ').AsInteger);
end;

function TDefEditAttr.getVazba: TVztah;
begin
  Result := TVztah(FieldByName('Vazba').AsInteger);
end;

function TDefEditAttr.getVerejny: Boolean;
begin
  Result := FieldByName('Verejny').AsBoolean;
end;

function TDefEditAttr.getZalozka: string;
begin
  Result := PrelozHlasku(FieldByName('Zalozka').AsString);
end;

function TDefEditAttr.GUIDToJmenoSys(s: String): String;
var
  l: TStringList;
  i: Integer;
  res: string;
begin
  l := TStringList.Create;
  l.CommaText := s;
  res := '';
  for i := 0 to l.Count - 1 do
  begin
    if findGUID(l[i]) then
    begin
      res := res + #13 + JmenoSys;
    end
    else
    begin
      res := '';
      break;
    end;
  end;
  Result := res;
  l.Free;
end;

function TDefEditAttr.isGUID(aGUID: TGUID): Boolean;
begin
  Result := isGUID(GUIDToString(aGUID));
end;

function TDefEditAttr.isGUID(aGUID: String): Boolean;
begin
  Result := FieldByName('GUID').AsString = aGUID;
end;

procedure TDefEditAttr.loadAttr;
var
  d: PDefEditRec;
  a: PDefEditAtr;
  i: _TDefEditList;
  ii: Integer;
begin
  // natazeni definice attr do virtual table
  for i := Low(_defEditAttrList) to High(_defEditAttrList) do
  begin
    d := _defEditAttrList[i];
    ii := 0;
    a := d^.Atr;
    while ii < d^.PocetAtr do
      if not findGUID(a^.GUID) then
      begin
        Insert;
        // vlozeni atributu to virtualtable
        GUID := a^.GUID;
        Typ := a^.Typ;
        Tabulka := d^.Tabulka;
        JmenoSys := a^.JmenoSys;
        Delka := a^.Delka;
        Browse := a^.Browse;
        Vazba := a^.Vazba;

        JenomCteni := a^.JenomCteni;
        Verejny := a^.Verejny;

        if a^.Zalozka = 0 then
          Zalozka := a^.ZalozkaTxt
        else
          Zalozka := '#H#' + IntToStr(a^.Zalozka);

        if a^.Skupina = 0 then
          Skupina := a^.SkupinaTxt
        else
          Skupina := '#H#' + IntToStr(a^.Skupina);

        if a^.Popis = 0 then
          Popis := a^.PopisTxt
        else
          Popis := '#H#' + IntToStr(a^.Popis);

        setonShow(a^.onShow);
        setonEnter(a^.onEnter);
        setonExit(a^.onExit);
        setonValidate(a^.onValidate);
        setonButton(a^.onButton);

        setPovoleneBID(d^.povoleneBID);

        Post;
        inc(ii);
        inc(a);
      end;
  end;
end;

procedure TDefEditAttr.Post;
begin
  // TODO -ojan.damek -cfunkcni: ulozeni zmen do SQL;
  inherited;

end;

function TDefEditAttr.PrelozHlasku(s: String): String;
begin
  if Copy(s, 1, 3) = '#H#' then
    Result := sqlCtiOznam(StrToInt(Copy(s, 4, length(s) - 3)))
  else
    Result := s;
end;

procedure TDefEditAttr.setPovoleneBID(const Value: String);
begin
  FieldByName('PovoleneBID').AsString := Value;
end;

procedure TDefEditAttr.setBrowse(const Value: TBrowse);
begin
  FieldByName('Browse').AsInteger := Ord(Value);
end;

procedure TDefEditAttr.setDelka(const Value: Integer);
begin
  FieldByName('Delka').AsInteger := Value;
end;

procedure TDefEditAttr.setGUID(const Value: TGUID);
begin
  FieldByName('GUID').AsString := GUIDToString(Value);
end;

procedure TDefEditAttr.setJenomCteni(const Value: Boolean);
begin
  FieldByName('JenomCteni').AsBoolean := Value;
end;

procedure TDefEditAttr.setJmenoSys(const Value: string);
begin
  if Pos('.', Value) = 0 then
    FieldByName('JmenoSys').AsString := TableName + '.' + Value
  else
    FieldByName('JmenoSys').AsString := Value;
end;

procedure TDefEditAttr.setonButton(const Value: String);
begin
  FieldByName('onButton').AsString := Value;
end;

procedure TDefEditAttr.setonButtonUser(const Value: String);
begin
  FieldByName('onButtonUser').AsString := Value;
end;

procedure TDefEditAttr.setonEnter(const Value: String);
begin
  FieldByName('onEnter').AsString := Value;
end;

procedure TDefEditAttr.setonEnterUser(const Value: String);
begin
  FieldByName('onEnterUser').AsString := Value;
end;

procedure TDefEditAttr.setonExit(const Value: String);
begin
  FieldByName('onExit').AsString := Value;
end;

procedure TDefEditAttr.setonExitUser(const Value: String);
begin
  FieldByName('onExitUser').AsString := Value;
end;

procedure TDefEditAttr.setonShow(const Value: String);
begin
  FieldByName('onShow').AsString := Value;
end;

procedure TDefEditAttr.setonShowUser(const Value: String);
begin
  FieldByName('onShowUser').AsString := Value;
end;

procedure TDefEditAttr.setonValidate(const Value: String);
begin
  FieldByName('onValidate').AsString := Value;
end;

procedure TDefEditAttr.setonValidateUser(const Value: String);
begin
  FieldByName('onValidateUser').AsString := Value;
end;

procedure TDefEditAttr.setPopis(const Value: string);
begin
  FieldByName('Popis').AsString := Value;
end;

procedure TDefEditAttr.setSkupina(const Value: string);
begin
  FieldByName('Skupina').AsString := Value;
end;

procedure TDefEditAttr.setStringGUID(const Value: String);
begin
  try
    StringToGUID(Value);
    FieldByName('GUID').AsString := Value;
  except
    FieldByName('GUID').AsString := '';
  end;
end;

procedure TDefEditAttr.setTabulka(const Value: TTabulka);
begin
  FieldByName('Tabulka').AsInteger := Ord(Value);
end;

procedure TDefEditAttr.setTyp(const Value: TTypDefEditAtr);
begin
  FieldByName('Typ').AsInteger := Ord(Value);
end;

procedure TDefEditAttr.setVazba(const Value: TVztah);
begin
  FieldByName('Vazba').AsInteger := Ord(Value);
end;

procedure TDefEditAttr.setVerejny(const Value: Boolean);
begin
  FieldByName('Verejny').AsBoolean := Value;
end;

procedure TDefEditAttr.setZalozka(const Value: string);
begin
  FieldByName('Zalozka').AsString := Value;
end;

end.
