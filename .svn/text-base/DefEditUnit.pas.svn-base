unit DefEditUnit;

interface

uses ddPlugin_TLB, VirtualTable, deType, ddType;

type
  // seznam vsech definovanych editoru
  TDefEdit = class(TVirtualTable)
  private
    fHelios: IHelios;
    fLastAktGUID: TGUID;

    // procedure CheckBrowseInSQL;
    function getListOfAttr: String;
    function getGUID: TGUID;
    procedure setGUID(const Value: TGUID);
    function getBrowseID_DPSN: String;
    function getNazev: String;
    function getTyp: TSetDefEditType;
    function getInternalTyp: Integer;
    procedure setBrowseID_DPSN(const Value: String);
    procedure setNazev(const Value: String);
    procedure setTyp(const Value: TSetDefEditType);
    procedure setListOfAttr(const Value: String);
    function getStringGUID: String;
    procedure setStringGUID(const Value: String);
    function getonNovaVeta: String;
    function getonValidate: String;
    procedure setonNovaVeta(const Value: String);
    procedure setonValidate(const Value: String);
    procedure checkSQL;

    procedure loadAktFromSQL;
  public
    // prace nad editory
    function findBrowse(aBrowseID_DPSN: String): Boolean; overload;
    function findBrowse(aBrowseID: TBrowse): Boolean; overload;
    function findGUID(aGUID: TGUID): Boolean; overload;
    function findGUID(aGUID: String): Boolean; overload;

    procedure filterBrowse(aBrowseID_DPSN: String); overload;
    procedure filterBrowse(aBrowseID: TBrowse); overload;
    procedure stopFilter;

    // ulozeni zmen do SQL;
    procedure Post; override;
  published
    constructor Create(aHelios: IHelios); virtual;

    // fieldy
    property GUID: TGUID read getGUID write setGUID;
    property StringGUID: String read getStringGUID write setStringGUID;
    property Nazev: String read getNazev write setNazev;
    property Typ: TSetDefEditType read getTyp write setTyp;
    property BrowseID_DPSN: String read getBrowseID_DPSN write setBrowseID_DPSN;

    // atributy
    property ListOfAttr: String read getListOfAttr write setListOfAttr;
    property onValidate: String read getonValidate write setonValidate;
    property onNovaVeta: String read getonNovaVeta write setonNovaVeta;
  end;

var
  DefEdit: TDefEdit;

implementation

uses ddMain, SysUtils, sqString, Data.db, MemData, sqProc, Classes, deEditory;

{ TDefEdit }

constructor TDefEdit.Create(aHelios: IHelios);
begin
  inherited Create(nil);

  fHelios := aHelios;

  AddField('GUID', ftString, 38);
  AddField('Nazev', ftString, 255);
  AddField('Typ', ftInteger);
  AddField('BrowseID_DPSN', ftString, 38);
  AddField('ListOfAttr', ftMemo);
  AddField('onNovaVeta', ftMemo);
  AddField('onValidate', ftMemo);

  Open;
  // TODO -ojan.damek -cpro demo: fiktivni def editory
  AppendRecord(['{57FA3728-987B-4982-B5B6-5B54576B1994}', 'Oprava 1', 1, '240',
    '{D4D5B6AA-CE71-44DE-908E-6BA4B36A66FA},{68DE4133-19C8-4838-ACB5-6450F17E1013},{00920843-899F-47F4-904D-97099BDBB754}',
    '', '']);
  AppendRecord(['{1FAC7664-57CA-4D0C-855A-86CE5E177E64}', 'Oprava 2', 1, '240', '', '', '']);
  AppendRecord(['{68BFB7D4-58F7-4478-8DB9-90C343EAB1B6}', 'Novy', 2, '240', '', '', '']);
  AppendRecord(['{A55FAAF7-A654-4100-9657-F8CD9B612BDA}', 'Oprava/Novy', 3, '240', '', '', '']);

  checkSQL;
end;

function TDefEdit.findBrowse(aBrowseID_DPSN: String): Boolean;
begin
  Result := false;
  First;
  while not(Eof or Result) do
  begin
    Result := BrowseID_DPSN = aBrowseID_DPSN;
    if not Result then
      next;
  end;
end;

procedure TDefEdit.filterBrowse(aBrowseID_DPSN: String);
begin
  SetFilterData('BrowseID_DPSN=' + aBrowseID_DPSN, [foCaseInsensitive, foNoPartialCompare]);
  Filtered := true;
end;

procedure TDefEdit.filterBrowse(aBrowseID: TBrowse);
begin
  filterBrowse(IntToStr(Ord(aBrowseID)));
end;

function TDefEdit.findBrowse(aBrowseID: TBrowse): Boolean;
begin
  Result := findBrowse(IntToStr(Ord(aBrowseID)));
end;

function TDefEdit.findGUID(aGUID: String): Boolean;
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

function TDefEdit.findGUID(aGUID: TGUID): Boolean;
begin
  Result := findGUID(GUIDToString(aGUID));
end;

function TDefEdit.getBrowseID_DPSN: String;
begin
  Result := FieldByName('BrowseID_DPSN').AsString;
end;

function TDefEdit.getGUID: TGUID;
begin
  Result := StringToGUID(FieldByName('GUID').AsString);
end;

function TDefEdit.getInternalTyp: Integer;
begin
  Result := FieldByName('Typ').AsInteger;
end;

function TDefEdit.getListOfAttr: String;
begin
  loadAktFromSQL;
  Result := FieldByName('ListOfAttr').AsString;
end;

function TDefEdit.getNazev: String;
begin
  loadAktFromSQL;
  Result := FieldByName('Nazev').AsString;
end;

function TDefEdit.getonNovaVeta: String;
begin
  loadAktFromSQL;
  Result := FieldByName('onNovaVeta').AsString;
end;

function TDefEdit.getonValidate: String;
begin
  loadAktFromSQL;
  Result := FieldByName('onValidate').AsString;
end;

function TDefEdit.getStringGUID: String;
begin
  Result := FieldByName('GUID').AsString;
end;

function TDefEdit.getTyp: TSetDefEditType;
var
  i: Integer;
begin
  i := FieldByName('Typ').AsInteger;
  case i of
    3:
      Result := [teOprava, teNovy];
    2:
      Result := [teNovy];
    1:
      Result := [teOprava];
  else
    Result := [];
  end;
end;

procedure TDefEdit.loadAktFromSQL;
var
  SQL: String;
begin
  if fLastAktGUID <> GUID then
  begin
    SQL := 'SELECT ListOfAttr, NazevEditoru, onNovaVeta, onValidate FROM [dbo].[TabHM_browses] WHERE GUID=' +
      NQuotedStr(StringGUID);
    with fHelios.OpenSQL(SQL) do
      if RecordCount > 0 then
        try
          Edit;
          ListOfAttr := FieldValues(0);
          Nazev := FieldValues(1);
          onNovaVeta := FieldValues(2);
          onValidate := FieldValues(3);
          InternalPost;
        finally
          fLastAktGUID := GUID;
        end
      else
        fLastAktGUID := GUID;
  end;
end;

procedure TDefEdit.Post;
var
  SQL: String;
begin
  // DONE -ojan.damek -cfunkcni: ulozeni zmen do SQL;
  try
    if State = dsInsert then
    begin
      SQL := 'INSERT INTO [dbo].[TabHM_browses] (GUID, BrowseID_DPSN, Typ, NazevEditoru, ListOfAttr, onNovaVeta, onValidate)VALUES(';
      SQL := SQL + StringGUID + ', ';
      SQL := SQL + BrowseID_DPSN + ', ';
      SQL := SQL + IntToStr(getInternalTyp) + ', ';
      SQL := SQL + NQuotedStr(Nazev) + ', ';
      SQL := SQL + NQuotedStr(ListOfAttr) + ', ';
      SQL := SQL + NQuotedStr(onNovaVeta) + ', ';
      SQL := SQL + NQuotedStr(onValidate) + ') ';
      fHelios.ExecSQL(SQL);
    end
    else if State = dsEdit then
    begin
      SQL := 'UPADTE [dbo].[TabHM_browses] SET ';
      SQL := SQL + 'NazevEditoru=' + NQuotedStr(Nazev) + ', ';
      SQL := SQL + 'ListOfAttr=' + NQuotedStr(ListOfAttr) + ', ';
      SQL := SQL + 'onNovaVeta=' + NQuotedStr(onNovaVeta) + ', ';
      SQL := SQL + 'onValidate=' + NQuotedStr(onValidate) + ' WHERE GUID = ' + StringGUID;
      fHelios.ExecSQL(SQL);
    end;
  finally
    inherited Post;
  end;
end;

procedure TDefEdit.setBrowseID_DPSN(const Value: String);
begin
  FieldByName('BrowseID_DPSN').AsString := Value;
end;

procedure TDefEdit.setGUID(const Value: TGUID);
begin
  FieldByName('GUID').AsString := GUIDToString(Value);
end;

procedure TDefEdit.setListOfAttr(const Value: String);
begin
  FieldByName('ListOfAttr').AsString := Value;
end;

procedure TDefEdit.setNazev(const Value: String);
begin
  FieldByName('Nazev').AsString := Value;
end;

procedure TDefEdit.setonNovaVeta(const Value: String);
begin
  FieldByName('onNovaVeta').AsString := Value;
end;

procedure TDefEdit.setonValidate(const Value: String);
begin
  FieldByName('onValidate').AsString := Value;
end;

procedure TDefEdit.setStringGUID(const Value: String);
begin
  try
    StringToGUID(Value);
    FieldByName('GUID').AsString := Value;
  except
    FieldByName('GUID').AsString := '';
  end;
end;

procedure TDefEdit.setTyp(const Value: TSetDefEditType);
var
  i: Integer;
begin
  i := 0;
  if teOprava in Value then
    i := 1;
  if teNovy in Value then
    i := i + 2;
  FieldByName('Typ').AsInteger := i;
end;

procedure TDefEdit.stopFilter;
begin
  Filtered := false;
end;

procedure TDefEdit.checkSQL;
var
  SQL: String;
begin
  // test definice tabulky browse
  SQL := 'IF OBJECT_ID(''dbo.TabHM_browses'')IS NOT NULL SELECT 1 ELSE SELECT 0';

  with fHelios.OpenSQL(SQL) do
  begin
    // neni definovan, nutno vytvorit
    if FieldValues(0) = 0 then
    begin
      SQL := 'CREATE TABLE [dbo].[TabHM_browses]('#13 + '[ID] [int] IDENTITY(1,1) NOT NULL,'#13 +
        '[GUID] [nvarchar] (38) NOT NULL,'#13 + '[BrowseID_DPSN] [nvarchar](38) NOT NULL,'#13 +
        '[Typ] [smallint] NOT NULL,'#13 + '[NazevEditoru] [nchar](50) NOT NULL,'#13 + '[ListOfAttr] [text] NULL, '#13 +
        '[onNovaVeta] [text] NULL,'#13 + '[onValidate] [text] NULL,'#13 +
        'CONSTRAINT [PK_tabHM_browses] PRIMARY KEY CLUSTERED'#13 + '('#13 + '[ID] ASC'#13 +
        ')WITH (PAD_INDEX  = OFF, STATISTICS_NORECOMPUTE  = OFF, IGNORE_DUP_KEY = OFF, ALLOW_ROW_LOCKS  = ON, ALLOW_PAGE_LOCKS  = ON) ON [PRIMARY]'#13
        + ') ON [PRIMARY] TEXTIMAGE_ON [PRIMARY]';
      fHelios.ExecSQL(SQL);
    end;
  end;
end;

end.
