unit sqProc;

interface

uses
  Classes ,SysUtils, ddPlugin_TLB, ddType;

const
  stqFROM        = Pointer($55AA1122);
  stqWHERE       = Pointer($FF002354);
  stqWHEREFilter = Pointer($8856FFCC);
  stqORDERBY     = Pointer($1133EEAA);


type
  TServer = class(TObject)
    Konstanty: iHeQuery;
    function Server_High_TTabulka(aNovaDB: Boolean): TTabulka;
    procedure ExecSQL(aSQL: String);
    function CreateQuery: iHeQuery;
    function JeToHeliosGermany: Boolean;
  end;

procedure ClearSeznamDynamickychTabulek;
function sqlCtiOznam(const AHlaska: string): string; overload;
function sqlCtiOznam(ATxt: TTxt): string; overload;
procedure sqlInfo(S: string; ASetFocus: Boolean = False); overload;
procedure sqlInfo(ATxt: TTxt; ASetFocus: Boolean = False); overload;
procedure sqlChyba(S: string; ASetFocus: Boolean = False); overload;
procedure sqlChyba(ATxt: TTxt; ASetFocus: Boolean = False); overload;
function sqlChybaAltB(S: string): Boolean;
function _DejTitulek(ATxt: TTxt): string;
function _Chybka(const ACaption: string; AMsg: string; AIconIndex: Integer;
  ASetFocus: Boolean = False; ATajnejText: string = ''): Boolean;
function NajdiOddelovacAHodnotu(const IWhere : String;
                                out   OPoziceOddelovace : Integer;
                                out   OPoziceHodnoty    : Integer;
                                out   OPodminka         : TPodminka) : Boolean; overload;
function NajdiOddelovacAHodnotu(const IWhere : String;
                                out   OPoziceOddelovace : Integer;
                                out   OPoziceHodnoty    : Integer) : Boolean; overload;
function NajdiOddelovac(    IWhere            : String;
                        out OPoziceAtributu   : Integer;
                        out OPoziceOddelovace : Integer) : Boolean;
function KonciNa(const AVCem: String; ANaCo: array of String;
                 ACaseSensitive : Boolean = False) : Boolean;

procedure VybagrujMinusy(AStringList : TStringList);
function BracketStr(const AStr: string): string;
procedure SestavSELECT(AHlavniTabulka: TTabulka; const AHlavniTabulkaStr: string; aBrowse: TBrowse;
  SELECT, BrowseSQL: TStringList; var ASelectProRefreshDoBrowse: string; var ASelectProEdit: string;
  SELECTSys: TStringList = nil; SELECTBarva: TStringList = nil; WHERE: TStringList = nil; WHERESys: TStringList = nil;
  WHEREsysDataskop: TStringList = nil; ORDERBY: TStringList = nil; ARegisteredFieldsForCustomDraw: TStringList = nil;
  InternalSelect: TStringList = nil; InternalSelectV: TStringList = nil; const AUniqueKey: string = '';
  AAktualniTvrdyTOP: Integer = 0; ATop: Integer = 0; const ANazevUlozenehoNastaveni: string = '';
  const AJmenoBrowse: string = '');

var
  Server: TServer;
  fHeliosGlobal: iHelios;

implementation

uses
  sqHlasky, ddMain, sqDistr1, ddTabulka, dtHGlob, sqVazby, sqView,
  sqString, ddBrowse, ddUta;

function IsSQLReservedKeyword(S : string) : Boolean;
const
  G_ReservedKeywords : TStringList = nil;  //cache
var
  DummyInt : Integer;  //pro TStringList.Find()
begin
  if not Assigned(G_ReservedKeywords) then begin
    G_ReservedKeywords := TStringList.Create;
    G_ReservedKeywords.Sorted := True;  //rychlost + vyhodi duplicity

    //prevzato z BOL MSSQL2000 [TZ 25.10.2002]
    G_ReservedKeywords.Text :=
      //--SQL Server reserved keywords
      'ADD'#13+
      'ALL'#13+
      'ALTER'#13+
      'AND'#13+
      'ANY'#13+
      'AS'#13+
      'ASC'#13+
      'AUTHORIZATION'#13+
      'BACKUP'#13+
      'BEGIN'#13+
      'BETWEEN'#13+
      'BREAK'#13+
      'BROWSE'#13+
      'BULK'#13+
      'BY'#13+
      'CASCADE'#13+
      'CASE'#13+
      'CHECK'#13+
      'CHECKPOINT'#13+
      'CLOSE'#13+
      'CLUSTERED'#13+
      'COALESCE'#13+
      'COLLATE'#13+
      'COLUMN'#13+
      'COMMIT'#13+
      'COMPUTE'#13+
      'CONSTRAINT'#13+
      'CONTAINS'#13+
      'CONTAINSTABLE'#13+
      'CONTINUE'#13+
      'CONVERT'#13+
      'CREATE'#13+
      'CROSS'#13+
      'CURRENT'#13+
      'CURRENT_DATE'#13+
      'CURRENT_TIME'#13+
      'CURRENT_TIMESTAMP'#13+
      'CURRENT_USER'#13+
      'CURSOR'#13+
      'DATABASE'#13+
      'DBCC'#13+
      'DEALLOCATE'#13+
      'DECLARE'#13+
      'DEFAULT'#13+
      'DELETE'#13+
      'DENY'#13+
      'DESC'#13+
      'DISK'#13+
      'DISTINCT'#13+
      'DISTRIBUTED'#13+
      'DOUBLE'#13+
      'DROP'#13+
      'DUMMY'#13+
      'DUMP'#13+
      'ELSE'#13+
      'END'#13+
      'ERRLVL'#13+
      'ESCAPE'#13+

      'EXCEPT'#13+
      'EXEC'#13+
      'EXECUTE'#13+
      'EXISTS'#13+
      'EXIT'#13+
      'FETCH'#13+
      'FILE'#13+
      'FILLFACTOR'#13+
      'FOR'#13+
      'FOREIGN'#13+
      'FREETEXT'#13+
      'FREETEXTTABLE'#13+
      'FROM'#13+
      'FULL'#13+
      'FUNCTION'#13+
      'GOTO'#13+
      'GRANT'#13+
      'GROUP'#13+
      'HAVING'#13+
      'HOLDLOCK'#13+
      'IDENTITY'#13+
      'IDENTITY_INSERT'#13+
      'IDENTITYCOL'#13+
      'IF'#13+
      'IN'#13+
      'INDEX'#13+
      'INNER'#13+
      'INSERT'#13+
      'INTERSECT'#13+
      'INTO'#13+
      'IS'#13+
      'JOIN'#13+
      'KEY'#13+
      'KILL'#13+
      'LEFT'#13+
      'LIKE'#13+
      'LINENO'#13+
      'LOAD'#13+
      'NATIONAL'#13+
      'NOCHECK'#13+
      'NONCLUSTERED'#13+
      'NOT'#13+
      'NULL'#13+
      'NULLIF'#13+
      'OF'#13+
      'OFF'#13+
      'OFFSETS'#13+
      'ON'#13+
      'OPEN'#13+
      'OPENDATASOURCE'#13+
      'OPENQUERY'#13+
      'OPENROWSET'#13+
      'OPENXML'#13+
      'OPTION'#13+
      'OR'#13+
      'ORDER'#13+
      'OUTER'#13+
      'OVER'#13+

      'PERCENT'#13+
      'PLAN'#13+
      'PRECISION'#13+
      'PRIMARY'#13+
      'PRINT'#13+
      'PROC'#13+
      'PROCEDURE'#13+
      'PUBLIC'#13+
      'RAISERROR'#13+
      'READ'#13+
      'READTEXT'#13+
      'RECONFIGURE'#13+
      'REFERENCES'#13+
      'REPLICATION'#13+
      'RESTORE'#13+
      'RESTRICT'#13+
      'RETURN'#13+
      'REVOKE'#13+
      'RIGHT'#13+
      'ROLLBACK'#13+
      'ROWCOUNT'#13+
      'ROWGUIDCOL'#13+
      'RULE'#13+
      'SAVE'#13+
      'SCHEMA'#13+
      'SELECT'#13+
      'SESSION_USER'#13+
      'SET'#13+
      'SETUSER'#13+
      'SHUTDOWN'#13+
      'SOME'#13+
      'STATISTICS'#13+
      'SYSTEM_USER'#13+
      'TABLE'#13+
      'TEXTSIZE'#13+
      'THEN'#13+
      'TO'#13+
      'TOP'#13+
      'TRAN'#13+
      'TRANSACTION'#13+
      'TRIGGER'#13+
      'TRUNCATE'#13+
      'TSEQUAL'#13+
      'UNION'#13+
      'UNIQUE'#13+
      'UPDATE'#13+
      'UPDATETEXT'#13+
      'USE'#13+
      'USER'#13+
      'VALUES'#13+
      'VARYING'#13+
      'VIEW'#13+
      'WAITFOR'#13+
      'WHEN'#13+
      'WHERE'#13+
      'WHILE'#13+
      'WITH'#13+
      'WRITETEXT'#13+

      //--ODBC Reserved Keywords
      'ABSOLUTE'#13+
      'ACTION'#13+
      'ADA'#13+
      'ADD'#13+
      'ALL'#13+
      'ALLOCATE'#13+
      'ALTER'#13+
      'AND'#13+
      'ANY'#13+
      'ARE'#13+
      'AS'#13+
      'ASC'#13+
      'ASSERTION'#13+
      'AT'#13+
      'AUTHORIZATION'#13+
      'AVG'#13+
      'BEGIN'#13+
      'BETWEEN'#13+
      'BIT'#13+
      'BIT_LENGTH'#13+
      'BOTH'#13+
      'BY'#13+
      'CASCADE'#13+
      'CASCADED'#13+
      'CASE'#13+
      'CAST'#13+
      'CATALOG'#13+
      'CHAR'#13+
      'CHAR_LENGTH'#13+
      'CHARACTER'#13+
      'CHARACTER_LENGTH'#13+
      'CHECK'#13+
      'CLOSE'#13+
      'COALESCE'#13+
      'COLLATE'#13+
      'COLLATION'#13+
      'COLUMN'#13+
      'COMMIT'#13+
      'CONNECT'#13+
      'CONNECTION'#13+
      'CONSTRAINT'#13+
      'CONSTRAINTS'#13+
      'CONTINUE'#13+
      'CONVERT'#13+
      'CORRESPONDING'#13+
      'COUNT'#13+
      'CREATE'#13+
      'CROSS'#13+
      'CURRENT'#13+
      'CURRENT_DATE'#13+
      'CURRENT_TIME'#13+
      'CURRENT_TIMESTAMP'#13+
      'CURRENT_USER'#13+
      'CURSOR'#13+
      'DATE'#13+
      'DAY'#13+
      'DEALLOCATE'#13+
      'DEC'#13+
      'DECIMAL'#13+
      'DECLARE'#13+
      'DEFAULT'#13+
      'DEFERRABLE'#13+
      'DEFERRED'#13+
      'DELETE'#13+
      'DESC'#13+
      'DESCRIBE'#13+
      'DESCRIPTOR'#13+
      'DIAGNOSTICS'#13+
      'DISCONNECT'#13+
      'DISTINCT'#13+
      'DOMAIN'#13+
      'DOUBLE'#13+
      'DROP'#13+
      'ELSE'#13+
      'END'#13+
      'END-EXEC'#13+
      'ESCAPE'#13+
      'EXCEPT'#13+
      'EXCEPTION'#13+

      'EXEC'#13+
      'EXECUTE'#13+
      'EXISTS'#13+
      'EXTERNAL'#13+
      'EXTRACT'#13+
      'FALSE'#13+
      'FETCH'#13+
      'FIRST'#13+
      'FLOAT'#13+
      'FOR'#13+
      'FOREIGN'#13+
      'FORTRAN'#13+
      'FOUND'#13+
      'FROM'#13+
      'FULL'#13+
      'GET'#13+
      'GLOBAL'#13+
      'GO'#13+
      'GOTO'#13+
      'GRANT'#13+
      'GROUP'#13+
      'HAVING'#13+
      'HOUR'#13+
      'IDENTITY'#13+
      'IMMEDIATE'#13+
      'IN'#13+
      'INCLUDE'#13+
      'INDEX'#13+
      'INDICATOR'#13+
      'INITIALLY'#13+
      'INNER'#13+
      'INPUT'#13+
      'INSENSITIVE'#13+
      'INSERT'#13+
      'INT'#13+
      'INTEGER'#13+
      'INTERSECT'#13+
      'INTERVAL'#13+
      'INTO'#13+
      'IS'#13+
      'ISOLATION'#13+
      'JOIN'#13+
      'KEY'#13+
      'LANGUAGE'#13+
      'LAST'#13+
      'LEADING'#13+
      'LEFT'#13+
      'LEVEL'#13+
      'LIKE'#13+
      'LOCAL'#13+
      'LOWER'#13+
      'MATCH'#13+
      'MAX'#13+
      'MIN'#13+
      'MINUTE'#13+
      'MODULE'#13+
      'MONTH'#13+
      'NAMES'#13+
      'NATIONAL'#13+
      'NATURAL'#13+
      'NCHAR'#13+
      'NEXT'#13+
      'NO'#13+
      'NONE'#13+
      'NOT'#13+
      'NULL'#13+
      'NULLIF'#13+
      'NUMERIC'#13+
      'NVARCHAR'#13+
      'OCTET_LENGTH'#13+
      'OF'#13+
      'ON'#13+
      'ONLY'#13+
      'OPEN'#13+
      'OPTION'#13+
      'OR'#13+
      'ORDER'#13+
      'OUTER'#13+
      'OUTPUT'#13+

      'OVERLAPS'#13+
      'PAD'#13+
      'PARTIAL'#13+
      'PASCAL'#13+
      'POSITION'#13+
      'PRECISION'#13+
      'PREPARE'#13+
      'PRESERVE'#13+
      'PRIMARY'#13+
      'PRIOR'#13+
      'PRIVILEGES'#13+
      'PROCEDURE'#13+
      'PUBLIC'#13+
      'READ'#13+
      'REAL'#13+
      'REFERENCES'#13+
      'RELATIVE'#13+
      'RESTRICT'#13+
      'REVOKE'#13+
      'RIGHT'#13+
      'ROLLBACK'#13+
      'ROWS'#13+
      'SCHEMA'#13+
      'SCROLL'#13+
      'SECOND'#13+
      'SECTION'#13+
      'SELECT'#13+
      'SESSION'#13+
      'SESSION_USER'#13+
      'SET'#13+
      'SIZE'#13+
      'SMALLINT'#13+
      'SOME'#13+
      'SPACE'#13+
      'SQL'#13+
      'SQLCA'#13+
      'SQLCODE'#13+
      'SQLERROR'#13+
      'SQLSTATE'#13+
      'SQLWARNING'#13+
      'SUBSTRING'#13+
      'SUM'#13+
      'SYSTEM_USER'#13+
      'TABLE'#13+
      'TEMPORARY'#13+
      'THEN'#13+
      'TIME'#13+
      'TIMESTAMP'#13+
      'TIMEZONE_HOUR'#13+
      'TIMEZONE_MINUTE'#13+
      'TO'#13+
      'TRAILING'#13+
      'TRANSACTION'#13+
      'TRANSLATE'#13+
      'TRANSLATION'#13+
      'TRIM'#13+
      'TRUE'#13+
      'UNION'#13+
      'UNIQUE'#13+
      'UNKNOWN'#13+
      'UPDATE'#13+
      'UPPER'#13+
      'USAGE'#13+
      'USER'#13+
      'USING'#13+
      'VALUE'#13+
      'VALUES'#13+
      'VARCHAR'#13+
      'VARYING'#13+
      'VIEW'#13+
      'WHEN'#13+
      'WHENEVER'#13+
      'WHERE'#13+
      'WITH'#13+
      'WORK'#13+
      'WRITE'#13+
      'YEAR'#13+
      'ZONE'#13+

      //--Future Keywords (avoiding the use of these words as identifiers)
      'ABSOLUTE'#13+
      'ACTION'#13+
      'ADMIN'#13+
      'AFTER'#13+
      'AGGREGATE'#13+
      'ALIAS'#13+
      'ALLOCATE'#13+
      'ARE'#13+
      'ARRAY'#13+
      'ASSERTION'#13+
      'AT'#13+
      'BEFORE'#13+
      'BINARY'#13+
      'BIT'#13+
      'BLOB'#13+
      'BOOLEAN'#13+
      'BOTH'#13+
      'BREADTH'#13+
      'CALL'#13+
      'CASCADED'#13+
      'CAST'#13+
      'CATALOG'#13+
      'CHAR'#13+
      'CHARACTER'#13+
      'CLASS'#13+
      'CLOB'#13+
      'COLLATION'#13+
      'COMPLETION'#13+
      'CONNECT'#13+
      'CONNECTION'#13+
      'CONSTRAINTS'#13+
      'CONSTRUCTOR'#13+
      'CORRESPONDING'#13+
      'CUBE'#13+
      'CURRENT_PATH'#13+
      'CURRENT_ROLE'#13+
      'CYCLE'#13+
      'DATA'#13+
      'DATE'#13+
      'DAY'#13+
      'DEC'#13+
      'DECIMAL'#13+
      'DEFERRABLE'#13+
      'DEFERRED'#13+
      'DEPTH'#13+
      'DEREF'#13+
      'DESCRIBE'#13+
      'DESCRIPTOR'#13+
      'DESTROY'#13+
      'DESTRUCTOR'#13+
      'DETERMINISTIC'#13+
      'DICTIONARY'#13+
      'DIAGNOSTICS'#13+
      'DISCONNECT'#13+
      'DOMAIN'#13+
      'DYNAMIC'#13+
      'EACH'#13+
      'END-EXEC'#13+
      'EQUALS'#13+
      'EVERY'#13+
      'EXCEPTION'#13+
      'EXTERNAL'#13+
      'FALSE'#13+
      'FIRST'#13+
      'FLOAT'#13+

      'FOUND'#13+
      'FREE'#13+
      'GENERAL'#13+
      'GET'#13+
      'GLOBAL'#13+
      'GO'#13+
      'GROUPING'#13+
      'HOST'#13+
      'HOUR'#13+
      'IGNORE'#13+
      'IMMEDIATE'#13+
      'INDICATOR'#13+
      'INITIALIZE'#13+
      'INITIALLY'#13+
      'INOUT'#13+
      'INPUT'#13+
      'INT'#13+
      'INTEGER'#13+
      'INTERVAL'#13+
      'ISOLATION'#13+
      'ITERATE'#13+
      'LANGUAGE'#13+
      'LARGE'#13+
      'LAST'#13+
      'LATERAL'#13+
      'LEADING'#13+
      'LESS'#13+
      'LEVEL'#13+
      'LIMIT'#13+
      'LOCAL'#13+
      'LOCALTIME'#13+
      'LOCALTIMESTAMP'#13+
      'LOCATOR'#13+
      'MAP'#13+
      'MATCH'#13+
      'MINUTE'#13+
      'MODIFIES'#13+
      'MODIFY'#13+
      'MODULE'#13+
      'MONTH'#13+
      'NAMES'#13+
      'NATURAL'#13+
      'NCHAR'#13+
      'NCLOB'#13+
      'NEW'#13+
      'NEXT'#13+
      'NO'#13+
      'NONE'#13+
      'NUMERIC'#13+
      'NVARCHAR'#13+
      'OBJECT'#13+
      'OLD'#13+
      'ONLY'#13+
      'OPERATION'#13+
      'ORDINALITY'#13+
      'OUT'#13+
      'OUTPUT'#13+
      'PAD'#13+
      'PARAMETER'#13+
      'PARAMETERS'#13+
      'PARTIAL'#13+
      'PATH'#13+
      'POSTFIX'#13+
      'PREFIX'#13+
      'PREORDER'#13+
      'PREPARE'#13+

      'PRESERVE'#13+
      'PRIOR'#13+
      'PRIVILEGES'#13+
      'READS'#13+
      'REAL'#13+
      'RECURSIVE'#13+
      'REF'#13+
      'REFERENCING'#13+
      'RELATIVE'#13+
      'RESULT'#13+
      'RETURNS'#13+
      'ROLE'#13+
      'ROLLUP'#13+
      'ROUTINE'#13+
      'ROW'#13+
      'ROWS'#13+
      'SAVEPOINT'#13+
      'SCROLL'#13+
      'SCOPE'#13+
      'SEARCH'#13+
      'SECOND'#13+
      'SECTION'#13+
      'SEQUENCE'#13+
      'SESSION'#13+
      'SETS'#13+
      'SIZE'#13+
      'SMALLINT'#13+
      'SPACE'#13+
      'SPECIFIC'#13+
      'SPECIFICTYPE'#13+
      'SQL'#13+
      'SQLEXCEPTION'#13+
      'SQLSTATE'#13+
      'SQLWARNING'#13+
      'START'#13+
      'STATE'#13+
      'STATEMENT'#13+
      'STATIC'#13+
      'STRUCTURE'#13+
      'TEMPORARY'#13+
      'TERMINATE'#13+
      'THAN'#13+
      'TIME'#13+
      'TIMESTAMP'#13+
      'TIMEZONE_HOUR'#13+
      'TIMEZONE_MINUTE'#13+
      'TRAILING'#13+
      'TRANSLATION'#13+
      'TREAT'#13+
      'TRUE'#13+
      'UNDER'#13+
      'UNKNOWN'#13+
      'UNNEST'#13+
      'USAGE'#13+
      'USING'#13+
      'VALUE'#13+
      'VARCHAR'#13+
      'VARIABLE'#13+
      'WHENEVER'#13+
      'WITHOUT'#13+
      'WORK'#13+
      'WRITE'#13+
      'YEAR'#13+
      'ZONE'#13+

      //--dalÜÝ, kterÚ nejsou uvedeny v BOL, ale v ddMain jsme je testovali (QueryAnalyzer je obarvÝ, tak×e znß)
      'STDEV'#13+
      'STDEVP'#13+
      'VAR'#13+
      'VARP'#13+

      //[TZ 18.07.2011] zavedlo SQL2011
      'WITHIN'
      ;
    {$IFDEF Ladit}
    if G_ReservedKeywords.Find('', DummyInt) or G_ReservedKeywords.Find(' ', DummyInt) then
      sqlLadit('mas blbe seznam Reserved Keywords');
    {$ENDIF Ladit}
  end;
  Result := G_ReservedKeywords.Find(S, DummyInt);
end;

function BracketStr(const AStr: string): string;
begin
  if AStr<>'' then
  begin
    if not IsValidIdent(AStr) or IsSQLReservedKeyword(AStr) then
    begin
      Result := AnsiQuotedStr(AStr, ']');
      Result[1] := '[';
    end
    else
    begin
      Result := AStr;
    end;
  end;
end;

procedure VybagrujMinusy(AStringList : TStringList);
var
  Pozice : Integer;
begin
  if Assigned(AStringList) then
  begin
    Pozice := AStringList.IndexOf('-');
    while Pozice<>-1 do
    begin
      AStringList.Delete(Pozice);
      Pozice := AStringList.IndexOf('-');
    end;
  end;
end;


function KonciNa(const AVCem: String; ANaCo: array of String;
                 ACaseSensitive : Boolean = False) : Boolean;
  var
    I    : Integer;
    S, T : String;
begin
  for I := Low(ANaCo) to High(ANaCo) do
  begin
    T := ANaCo[I];
    S := Copy(AVCem, Length(AVCem) - Length(T) + 1, MaxInt);
    if ACaseSensitive then Result := (CompareStr(S, AVCem) = 0)
                      else Result := SameText(S, T);
    if Result then Exit;
  end;

  Result := False;
end;


function NajdiOddelovacAHodnotu(const IWhere : String;
                                out   OPoziceOddelovace : Integer;
                                out   OPoziceHodnoty    : Integer;
                                out   OPodminka         : TPodminka) : Boolean;
  var
    LPodminka : TPodminka;
begin
  Result := False;
  OPoziceOddelovace := 0;
  OPoziceHodnoty    := 0;

  if Trim(IWhere)<>'' then
  begin
    for LPodminka := Succ(Low(TPodminka)) to High(TPodminka) do
    begin
      OPoziceOddelovace := Pos(Podminka2SysStr(LPodminka), IWhere);
      case OPoziceOddelovace of
        0  : ; { nenasli-zkus dalsi }
        1  : Break; { nasli na blbe pozici - Exit }
        else
          begin {nasli na dobre pozici >=2 }
            OPoziceHodnoty := OPoziceOddelovace + Length(Podminka2SysStr(LPodminka));
            OPodminka := LPodminka;
            Result := True;
            Exit;
          end;
      end;
    end;
  end;
end;

{ --------------------------------------------------------------------------- }

function NajdiOddelovacAHodnotu(const IWhere : String;
                                out   OPoziceOddelovace : Integer;
                                out   OPoziceHodnoty    : Integer) : Boolean;
  var
    LPodminka : TPodminka;
begin
  Result := NajdiOddelovacAHodnotu(IWhere, OPoziceOddelovace, OPoziceHodnoty, LPodminka);
end;


function NajdiOddelovac(    IWhere            : String;
                        out OPoziceAtributu   : Integer;
                        out OPoziceOddelovace : Integer) : Boolean;
const
  cMozneOddelovace = ' =><(';
var
  PC : PChar;
  I  : Integer;
begin
  Result            := False;
  OPoziceAtributu   := 0;
  OPoziceOddelovace := 0;

  if IWhere<>'' then
  begin
    OPoziceAtributu   := 1;

    { odstran pripadne uvodni zavorky a mezery }
    PC := @IWhere[1];
    while (PC^<>#0) and ((PC^='(') or (PC^=#32)) do
    begin
      Inc(PC);
      Inc(OPoziceAtributu);
    end;

    while PC^<>#0 do
    begin
      for I := 1 to Length(cMozneOddelovace) do
      begin
        if PC^=cMozneOddelovace[I] then
        begin
          Result := True;
          OPoziceOddelovace := PC - @IWhere[1] + 1 ;
          Exit;
        end;
      end;
      Inc(PC);
    end;
  end;
end;


procedure ClearSeznamDynamickychTabulek;
begin
  // vymaz vazeb
end;

{ TServer }

function TServer.CreateQuery: iHeQuery;
begin
  //
end;

procedure TServer.ExecSQL(aSQL: String);
begin
  // provede prikaz na SQL
end;

function TServer.JeToHeliosGermany: Boolean;
begin
  Result := False;
end;

function TServer.Server_High_TTabulka(aNovaDB: Boolean): TTabulka;
begin
  Result := High(TTabulka);
end;

function _sqlCtiOznam5_6(ATxt: TTxt): string;
var
  HL: PHlaska;
begin
  HL := Vrat_GHlasku(ATxt);

  Result := HL.H;
end;

{ --------------------------------------------------------------------------- }

function _sqlCtiOznam1(AHlaska: TTxt): string;
begin
  Result := Vrat_GHlasku(AHlaska).H;
end;

{ --------------------------------------------------------------------------- }

function _sqlCtiOznam0(AHlaska: TTxt): string;
begin
  Result := Vrat_GHlasku(AHlaska).H;
end;

{ --------------------------------------------------------------------------- }

function _sqlCtiOznam255(AHlaska: TTxt): string;
begin
  Result := 'x' + IntToStr(Vrat_GHlasku(AHlaska).W) + ' ' +
    Vrat_GHlasku(AHlaska).H;
end;

{ --------------------------------------------------------------------------- }

var
  _sqlCtiOznam: function(ATxt: TTxt): string = _sqlCtiOznam1;

  { --------------------------------------------------------------------------- }

{$IFDEF Ladit}

var
  G_UzByl: Boolean = False;
  G_LaditCtiOznam: Boolean = False;
{$ENDIF Ladit}

function sqlCtiOznam(const AHlaska: string): string;
begin
{$IFDEF LaditCtiOznam}
  Result := '¶' + AHlaska + '¶';
{$ELSE LaditCtiOznam}
{$IFDEF Ladit}
  if not G_UzByl then
  begin
    G_UzByl := True;
    G_LaditCtiOznam := FindCmdLineSwitch('LaditCtiOznam', ['/', '-'], True);
  end;
  if G_LaditCtiOznam then
  begin
    Result := '¶' + AHlaska + '¶';
    Exit;
  end;
{$ENDIF Ladit}
  Result := AHlaska;
{$ENDIF LaditCtiOznam}
end;

{ --------------------------------------------------------------------------- }

function sqlCtiOznam(ATxt: TTxt): string;
var
  MM: string;
begin
  Result := _sqlCtiOznam(ATxt);

  if (ATxt > txtNic) and (ATxt < txtOK) then
  begin
    Result := StringReplace(Result, '<HM>', _sqlCtiOznam(xHlavniMenaZkr),
      [rfReplaceAll]);

    begin
      MM := 'Kè';
      Result := StringReplace(Result, '<MM>', MM, [rfReplaceAll]);
    end;
  end;
end;

procedure sqlInfo(S: string; ASetFocus: Boolean = False);
var
  LIconIndex: Integer;
begin
  LIconIndex := -1;
  _Chybka(_DejTitulek(txtinformace), S, LIconIndex, ASetFocus);
end;

procedure sqlInfo(ATxt: TTxt; ASetFocus: Boolean = False);
begin
  sqlInfo(sqlCtiOznam(ATxt), ASetFocus);
end;

function sqlInfoAltB(S: string): Boolean;
var
  LIconIndex: Integer;
begin
  LIconIndex := -1;
  Result := _Chybka(_DejTitulek(txtinformace), S, LIconIndex) = False;
end;

procedure sqlChyba(S: string; ASetFocus: Boolean = False);
var
  LIconIndex: Integer;
begin
  LIconIndex := -1;
  _Chybka(_DejTitulek(txtChyba), S, LIconIndex, ASetFocus);
end;

procedure sqlChyba(ATxt: TTxt; ASetFocus: Boolean = False);
begin
  sqlChyba(sqlCtiOznam(ATxt), ASetFocus);
end;

function sqlChybaAltB(S: string): Boolean;
var
  LIconIndex: Integer;
begin
  LIconIndex := -1;
  Result := _Chybka(_DejTitulek(txtChyba), S, LIconIndex) = False;
end;

function _Chybka(const ACaption: string; AMsg: string; AIconIndex: Integer;
  ASetFocus: Boolean = False; ATajnejText: string = ''): Boolean;
const
  _ButtonWidth = 92;

  function _CaptionTlacitka(AIndex: Integer): string;
  begin
    Result := 'Button';
  end;

  function _VypoctiSirkuTlacitka(const AText: string): Integer;
  begin
    Result := 0;
  end;

begin
  Result := False;
end;

function _DejTitulek(ATxt: TTxt): string;
begin
  if Assigned(Server) then
  begin
    Result := 'Hacuna Matata';
    if Result = '' then
      Result := CNazevProduktu;
  end
  else
    Result := CNazevProduktu;

  Result := Format('%s - %s', [Result, sqlCtiOznam(ATxt)]);
end;

procedure SestavSELECT(AHlavniTabulka: TTabulka; const AHlavniTabulkaStr: string; aBrowse: TBrowse;
  SELECT, BrowseSQL: TStringList; var ASelectProRefreshDoBrowse: string; var ASelectProEdit: string;
  SELECTSys: TStringList = nil; SELECTBarva: TStringList = nil; WHERE: TStringList = nil; WHERESys: TStringList = nil;
  WHEREsysDataskop: TStringList = nil; ORDERBY: TStringList = nil; ARegisteredFieldsForCustomDraw: TStringList = nil;
  InternalSelect: TStringList = nil; InternalSelectV: TStringList = nil; const AUniqueKey: string = '';
  AAktualniTvrdyTOP: Integer = 0; ATop: Integer = 0; const ANazevUlozenehoNastaveni: string = '';
  const AJmenoBrowse: string = '');

var
  LSysFrom: TStringList; // zde jsou pripadne navazane tabulky s JOINama
  LSysFrom11: TStringList;
  // zde jsou pripadne navazane tabulky s JOINama s vazbou 1:1 pro ASelectProRefreshDoBrowse
  LSeznamTabulek: TStringList; // seznam tabulek a vztahu
  LSeznamTabulek11: TStringList; // seznam tabulek a vztahu
  LBudeDistinct: Boolean;
  LSelect: TStringList; // pomocny SL + originalni - tak jak je v SELECT
  LSelectV: TStringList; // s vazbama
  LWhereAll: TStringList;
  // WHERE + WHEREsys (pøešramtanejma uživ. atr. - neboli tak, jak jde na server)
  LUpravenyORDERBY: TStringList;

  { +++++++++++++++++++++++++++ }

  function UpravExterniAtribut(const T_A, V_A: string; PA: PAtributTabulky; ProSelect: Boolean): string;
  var
    V, T_EXT, V_EXT: string;
  begin
    // [RK 24.10.2008] v pripade externiho atributu vyrobime JOIN
    T_EXT := Atribut2Tabulka(T_A) + '_EXT';
    // externi tabulka (napr. TabCisOrg_EXT)
    V := Atribut2Tabulka(V_A); // puvodni tabulka ci vazba
    V_EXT := V + '_EXT'; // alias pro novou vazbu

    if SameText(T_EXT, V_EXT) then
      T_EXT := ''; // aby to nebylo dvakrat (X AS X)
    Result := Format('  LEFT OUTER JOIN %s %s ON %1:s.ID=%s.ID', [T_EXT, V_EXT, V]);

    if LSeznamTabulek.IndexOf(V_EXT) = -1 then
    begin
      LSeznamTabulek.Add(V_EXT);
      LSysFrom.Add(Result);
    end;

    if ProSelect and (LSeznamTabulek11.IndexOf(V_EXT) = -1) then
    begin
      LSeznamTabulek11.Add(V_EXT);
      LSysFrom11.Add(Result);
    end;

    // pouze systemovy nazev atributu bez tabulky
    V := OdstranPrefixTabulky(T_A);

    Result := Format('%s.%s', [V_EXT, V]);

    // [RK 25.11.2008] to tady chybelo!!!
    if Assigned(PA) and (PA.Typ = taBoolean) then
    begin
      Result := Format('ISNULL(%s,0)', [Result]); // historicke duvody dbLib
      if ProSelect then
        Result := Format('%s AS %s', [Result, V]);
    end;
  end;

{ +++++++++++++++++++++++++++ }

  function UpravDefinovanyAtribut(const T_A, V_A: string; PA: PAtributTabulky; ProSelect: Boolean): string;
  var
    LPosT, LPosV: Integer;
    LTab, LAtr, LVaz, T_EUR, V_EUR, S: string;
  begin
    if _dvaExterniAtribut in PA.DalsiVlastnostiAtr then
    begin
      Result := UpravExterniAtribut(T_A, V_A, PA, ProSelect);
      Exit;
    end;

    LPosT := Pos('.', T_A);
    LPosV := Pos('.', V_A);
    LTab := Copy(T_A, 1, LPosT - 1);
    LAtr := Copy(T_A, LPosT + 1, MaxInt);
    LVaz := Copy(V_A, 1, LPosV - 1);

    if SameText(LTab, LVaz) then
      Result := PA.Pocitany
    else
      Result := StringReplace(PA.Pocitany, LTab + '.', LVaz + '.', [rfReplaceAll, rfIgnoreCase]);

    if (_dvaEURRozpad in PA.DalsiVlastnostiAtr) and (Server.Konstanty.FieldByName(TabHGlob_FazeEuro).Value = 2) then
    begin
      T_EUR := 'TabLEUR' + Copy(LTab, 4);
      if TypTabulky(T_EUR) <> tZadna then
      begin
        if SameText(LTab, LVaz) then
        begin
          V_EUR := T_EUR;
          T_EUR := ''; // aby to nebylo dvakrat (X AS X)
        end
        else
        begin
          V_EUR := LVaz + '_LEUR';
          Result := StringReplace(Result, T_EUR + '.', V_EUR + '.', [rfReplaceAll, rfIgnoreCase]);
        end;

        S := Format('  LEFT OUTER JOIN %s %s ON %1:s.ID=%s.ID', [T_EUR, V_EUR, LVaz]);

        if LSeznamTabulek.IndexOf(V_EUR) = -1 then
        begin
          LSeznamTabulek.Add(V_EUR);
          LSysFrom.Add(S);
        end;

        if ProSelect and (LSeznamTabulek11.IndexOf(V_EUR) = -1) then
        begin
          LSeznamTabulek11.Add(V_EUR);
          LSysFrom11.Add(S);
        end;
      end;
    end;

    if ProSelect then
      Result := Format('%s AS %s', [Result, LAtr])
    else
      Result := Format('(%s)', [Result]);
  end;

{ +++++++++++++++++++++++++++ }

  procedure _NahradDefPocitaneVSELECTu;
  var
    II: Integer;
    SS: string;
    PAA: PAtributTabulky;
  begin
    for II := 0 to LSelect.Count - 1 do
    begin
      SS := LSelect[II];
      PAA := Atribut(SS);
      if Assigned(PAA) then
      begin
        // [RK 13.04.2001] - primy ukazatel na atribut
        InternalSelect.Objects[II] := TObject(PAA);

        if dvaDefinovany in PAA.DalsiVlastnostiAtr then
          LSelectV[II] := UpravDefinovanyAtribut(SS, LSelectV[II], PAA, True);
      end;
    end;
  end;

{ +++++++++++++++++++++++++++ }

  procedure PridejDoSeznamuLSelect(const S, SV: String);
  begin
    if LSelectV.IndexOf(SV) = -1 then { kdyz v LSelectV jeste neni }
    begin
      LSelectV.Add(SV); { -> pridej do LSelectV }
      LSelect.Add(S); { -> pridej do LSelect }
    end;
  end;

{ +++++++++++++++++++++++++++ }

  procedure HledejVztah(var AProAtribut: String; PS: PString = nil; ASys: Boolean = False);
  var
    LTypVztahu: TTypVztahu;
    LJoinX: String; // "prelozeny" text JOINu (Tab nahrazeny sys. jmeny vztahu)
    PomStrX: String;
    PVD: PVztahDef;
    LCiziTabStr: string;
  begin
    if Assigned(PS) then
      PS^ := '';
    if AProAtribut = '' then
      Exit;

    if AProAtribut[1] <> 'V' then
    begin { je to primo nazev tabulky }
      if ASys then // pokud je WHEREsys, muze byt v AProAtribut nakej zblitek
        Exit
      else
        raise Exception.CreateFmt('%s musí být definováno vazbou !', [AProAtribut]);
    end;

    PVD := nil;
    LTypVztahu := tvZadny;
    LCiziTabStr := '';

    if SeznamVazebPodleVazeb.NajdiVazbu(Atribut2Tabulka(AProAtribut), PVD) then
    begin
      if JeVazbaZakazana(aBrowse, PVD.X) then
        PVD := nil
      else
        case PorovnejTabVeVazbe(AHlavniTabulka, AHlavniTabulkaStr, PVD) of
          ksvLeva:
            begin { je vlevo }
              LCiziTabStr := PVD.TabPStr;
              LJoinX := PVD.Join;
              LTypVztahu := PVD.TypLP;
            end;

          ksvPrava:
            begin { je vpravo }
              LCiziTabStr := PVD.TabLStr;
              LJoinX := PVD.Join;
              LTypVztahu := PVD.TypPL;
            end

        else
          PVD := nil;
        end;
    end;

    // if (PVD = nil) or (LTypVztahu = tvZadny) then
    // raise EDatabaseError.CreateFmt
    // (sqlCtiOznam('Položka %s nemá vazbu k tabulce ''%s'' (%s/%s)!'),
    // [AProAtribut, AHlavniTabulkaStr, ANazevUlozenehoNastaveni,
    // AJmenoBrowse]);

    if Assigned(PS) then { 'Vazba.Atribut' }
      PS^ := Format('%s.%s', [PVD.JmenoSys, OdstranPrefixTabulky(AProAtribut)]);

    if AProAtribut[1] = 'V' then // konvertuj na novy zpusob
    begin
      AProAtribut := StringReplace(AProAtribut, Format('%s.', [PVD.JmenoSys]), Format('%s.', [LCiziTabStr]),
        [rfReplaceAll, rfIgnoreCase]);
    end;

    if (LTypVztahu = tv1N) and (PVD.X = SeznamVychozichNastaveniBrowse[aBrowse].DegradovanyVztah) then
    begin
      LTypVztahu := tv11; // vazba degradovala
      LJoinX := Format('%s AND %s', [LJoinX, SeznamVychozichNastaveniBrowse[aBrowse].DegradujiciVazba]);
    end;

{$IFDEF RK_WhereSys}
    PomStrX := TableWhereSys_GetWhereSys(LCiziTabStr, False);
    if PomStrX <> '' then
      LJoinX := Format('%s AND %s', [LJoinX, PomStrX]);
{$ENDIF}
    // sys. nazev cizi tabulky -> alias (=nazev vazby)
    LJoinX := StringReplace(LJoinX, Format('%s.', [LCiziTabStr]), Format('%s.', [PVD.JmenoSys]),
      [rfReplaceAll, rfIgnoreCase]);

    // [RK 11.10.2006] vazba sama na sebe
    if JeToVazbaSamaNaSebe(AHlavniTabulka, AHlavniTabulkaStr, PVD) then
    begin
      // specifikator hlavni tabulky -> sys. nazev hlavni tabulky
      LJoinX := StringReplace(LJoinX, sqlVazbaSpecifHT + '.', Format('%s.', [AHlavniTabulkaStr]),
        [rfReplaceAll, rfIgnoreCase]);
    end;

    PomStrX := DefiniceTabulky(LCiziTabStr);
    if PomStrX = '' then
      PomStrX := LCiziTabStr;
    PomStrX := Format('  LEFT OUTER JOIN %s %s ON %s', [PomStrX, PVD.JmenoSys, LJoinX]);

    if LTypVztahu = tv1N then
      LBudeDistinct := True;

    if LSeznamTabulek.IndexOf(PVD.JmenoSys) = -1 then
    begin
      LSeznamTabulek.Add(PVD.JmenoSys);
      LSysFrom.Add(PomStrX);
    end;

    if (LTypVztahu = tv11) and (LSeznamTabulek11.IndexOf(PVD.JmenoSys) = -1) then
    begin
      LSeznamTabulek11.Add(PVD.JmenoSys);
      LSysFrom11.Add(PomStrX);
    end;
  end;

{ +++++++++++++++++++++++++++ }

  procedure AnalyzujWHERE(ASL: TStringList; ASys: Boolean;
    // jedna se o WHEREsys ? - kdyz ano kontroly nejsou striktni - hleda se jen 'Tab.Atr'
    ADataskopPar: Boolean = False);
  var
    I, XX: Integer;
    PoziceAtributu: Integer;
    PoziceOdddelovace: Integer;
    PoziceHodnoty: Integer;
    AtrStr, OrigAtrStr: string;
    Nasli: Boolean;
    LRadka: string;
    PAx: PAtributTabulky;
    LeveZavorky, Pahyl: string;
    Hodnota: string;
    LPodminka: TPodminka;

    { ........................... }

    procedure _UpravAtrStr;
    begin
      if Assigned(PAx) and (dvaDefinovany in PAx.DalsiVlastnostiAtr) then
        AtrStr := UpravDefinovanyAtribut(AtrStr, OrigAtrStr, PAx, False)
      else
        AtrStr := OrigAtrStr;
    end;

  { ........................... }

  begin
    for I := 0 to ASL.Count - 1 do
    begin
      LRadka := Trim(ASL[I]);
      if LRadka <> '' then
      begin
        { presun pripadne leve zavorky }
        LeveZavorky := '';
        while Copy(LRadka, 1, 1) = '(' do
        begin
          LeveZavorky := LeveZavorky + '(';
          System.Delete(LRadka, 1, 1);
        end;

        if ASys then
          // tady se zastavi i na mezere, tudiz ji neni treba nize korigovat
          Nasli := NajdiOddelovac(LRadka, PoziceAtributu, PoziceOdddelovace)
        else
        begin
          // u WHERE - struktura øádku musí být: '<Atribut><Podminka><Hodnota|Atribut>'
          PoziceAtributu := 1;
          Nasli := NajdiOddelovacAHodnotu(LRadka, PoziceOdddelovace, PoziceHodnoty, LPodminka);
        end;

        if Nasli then
        begin
          // [RK 20.08.2007]
          // uprava hodnoty (muze tam byt primo i atribut)
          if ASys then
            // ve WHERESys atribut nehrozi
            Hodnota := Copy(LRadka, PoziceOdddelovace, MaxInt)
          else
          begin
            // korekce pozice oddelovace - pro IS NULL, BETWEEN je treba tam mit i mezeru !!
            if LRadka[PoziceOdddelovace - 1] = ' ' then
              Dec(PoziceOdddelovace);

            // atribut lze pouze pro nize uvedene podminky
            if not(LPodminka in C_PodminkyProAtribut) then
              Hodnota := Copy(LRadka, PoziceOdddelovace, MaxInt)
            else
            begin
              // zkusime jestli neni jako hodnota zadan atribut
              Hodnota := Trim(Copy(LRadka, PoziceHodnoty, MaxInt));
              if (Length(Hodnota) = 0) or (Hodnota[1] = '''') then
                // urychleni - kdyz apostrof, tak je tam primo hodnota
                Hodnota := Copy(LRadka, PoziceOdddelovace, MaxInt)
              else
              begin
                Pahyl := '';

                // [RK 10.09.2008] pokud tam bylo OR a/nebo zavorky,
                // tak to padlo u uzivatelskych atributu
                if KonciNa(Hodnota, [' OR', ')OR']) then
                begin
                  Pahyl := Copy(Hodnota, Length(Hodnota) - 2, 3);
                  Hodnota := Copy(Hodnota, 1, Length(Hodnota) - 3);
                end;

                XX := Length(Hodnota);
                while XX > 0 do
                begin
                  if CharInSet(Hodnota[XX], [')', ' ']) then
                  begin
                    Pahyl := Hodnota[XX] + Pahyl;
                    System.Delete(Hodnota, XX, 1);
                    Dec(XX);
                  end
                  else
                    Break;
                end;

                PAx := AtributV(AHlavniTabulka, Hodnota);
                if not Assigned(PAx) then
                  // nenasli jsme odpovidajici atribut, vezmeme to jako hodnotu
                  // napr. se jedna o cislo
                  Hodnota := Copy(LRadka, PoziceOdddelovace, MaxInt)
                else
                begin
                  AtrStr := Hodnota; // tady je ted T.A nebo V.A
                  OrigAtrStr := AtrStr;

                  // "cizi" tabulka - hledam vztah
                  if not SameText(Atribut2Tabulka(AtrStr), AHlavniTabulkaStr) then
                  begin
                    HledejVztah(AtrStr, nil, ASys);
                  end;

                  _UpravAtrStr;

                  // sestaveni oddelovace a upravene hodnoty
                  Hodnota := Copy(LRadka, PoziceOdddelovace, PoziceHodnoty - PoziceOdddelovace);
                  Hodnota := Hodnota + AtrStr + Pahyl;
                end;
              end;
            end;
          end;

          // analyza filtrovaneho atributu
          AtrStr := Trim(Copy(LRadka, PoziceAtributu, PoziceOdddelovace - PoziceAtributu));
          if ADataskopPar and (AtrStr = '1') then
            LRadka := AtrStr + Hodnota
            // v dataskopu se podminka vypina pomoci vyrazu 1=1
          else
          begin
            OrigAtrStr := AtrStr;

            // "cizi" tabulka - hledam vztah
            if not SameText(Atribut2Tabulka(AtrStr), AHlavniTabulkaStr) then
            begin
              HledejVztah(AtrStr, nil, ASys);
            end;

            PAx := Atribut(AtrStr);
            _UpravAtrStr;

            // Hodnota ma v sobe i oddelovac (viz vyse)
            LRadka := AtrStr + Hodnota;
          end;
        end;

        if ASL.Count > 1 then
        begin
          if I = 0 then
            LRadka := '(' + LRadka;

          if I = ASL.Count - 1 then
          begin
            if KonciNa(LRadka, [' OR', ')OR']) then
            begin
              if LRadka[Length(LRadka) - 2] = #32 then
                LRadka[Length(LRadka) - 2] := ')'
              else
                System.Insert(')', LRadka, Length(LRadka) - 2);
            end
            else
              LRadka := LRadka + ')';
          end
        end;

        LWhereAll.Add(LeveZavorky + LRadka);
      end
      // else
      // raise EDatabaseError.CreateFmt
      // ('Špatná definice klauzule WHERE nebo WHEREsys'#13'%s', [ASL[I]]);
    end;
  end;

{ +++++++++++++++++++++++++++ }

  procedure AnalyzujSELECT(ASL: TStringList; AKonvertovatASL: Boolean);
  var
    XX: Integer;
    SOrig: String;
    SV: String;
  begin
    for XX := 0 to ASL.Count - 1 do
    begin
      SOrig := Trim(ASL[XX]);
      if SOrig <> '' then
      begin
        SV := SOrig;
        if not SameText(Atribut2Tabulka(SOrig), AHlavniTabulkaStr) then
        begin
          // pokud to neni hlavni tabulka, pak musime najit vztah
          HledejVztah(SOrig, @SV);
        end;

        PridejDoSeznamuLSelect(SOrig { T.A } , SV { V.A } );

        if AKonvertovatASL then // popravde receno, nevim, proc to tu je
          if ASL[XX] <> SV then
            ASL[XX] := SV;
      end;
    end;
    VybagrujMinusy(ASL);
  end;

{ +++++++++++++++++++++++++++ }

  procedure AnalyzujORDERBY(ASL: TStringList; ASLUprav: TStringList);
  var
    I, J: Integer;
    PA: PAtributTabulky;
    T_A, V_A, SDesc: string;
  begin
    for I := 0 to ASL.Count - 1 do
    begin
      V_A := Trim(ASL[I]);

      J := Pos(#32, V_A);
      if J = 0 then
        SDesc := ''
      else
      begin
        SDesc := Copy(V_A, J, MaxInt);
        V_A := Copy(V_A, 1, J - 1);
      end;

      if V_A = '' then
        Continue;

      T_A := V_A;
      if not SameText(Atribut2Tabulka(V_A), AHlavniTabulkaStr) then
        HledejVztah(T_A, @V_A);

      // OPTIMALIZACE
      // optimalizace rychlosti pro Datumove atributy Datum_X, Datum_Y
      // tato optimalizace se muze uplatnit pokud v ORDER BY neni jiny vyraz, jinak
      // nebezpeci konfliktu stejneho jmena vice atributu v klauzuli ORDER BY
      // optimalizace dela to, ze atribut pocitany nahradi atributem nepocitanym stejneho jmena
      if (not LBudeDistinct) and (ASL.Count = 1) and KonciNa(T_A, ['_' + sqlJenDatum, '_' + sqlYear]) then
      begin
        PA := Atribut(T_A);
        if Assigned(PA) and ((PA.Typ = taDateTime) or (PA.Typ = taInt)) then
        begin
          PA := Atribut(Copy(T_A, 1, Length(T_A) - 2));
          if Assigned(PA) and (PA.Typ = taDateTime) then
          begin
            Delete(T_A, Length(T_A) - 1, 2);
            Delete(V_A, Length(V_A) - 1, 2);
          end;
        end;
      end;

      J := LSelectV.IndexOf(V_A);
      if J <> -1 then
        // pokud je v SELECTu, tak bereme pro ORDER BY jeho poradi
        V_A := IntToStr(J + 1)
      else
      begin
        // uprava pro DvaDef atributy
        // uprava zpociva ve zkopirovani vyrazu do sekce ORDER BY
        PA := Atribut(T_A);
        if Assigned(PA) and (dvaDefinovany in PA.DalsiVlastnostiAtr) then
          V_A := UpravDefinovanyAtribut(T_A, V_A, PA, False);
      end;

      ASLUprav.Add(V_A + SDesc);
    end;
  end;

{ +++++++++++++++++++++++++++ }

var
  I: Integer;
  PomStr: String;
  TabSDB: String;
  LPos: Integer;
  LTopStr: string;
  // PA: PAtributTabulky;
  // LastTabulka: TTabulka;
  // LastTabulkaStr: string;
  Byl1, Byl2: Boolean;
begin { _SestavSELECT }
  ASelectProRefreshDoBrowse := '';
  ASelectProEdit := '';

  Byl1 := (InternalSelect <> nil);
  if not Byl1 then
    InternalSelect := TStringList.Create;

  Byl2 := (InternalSelectV <> nil);
  if not Byl2 then
    InternalSelectV := TStringList.Create;

  LSeznamTabulek := TStringList.Create;
  LSeznamTabulek11 := TStringList.Create;
  try
    LSeznamTabulek.Add(AHlavniTabulkaStr);
    LSeznamTabulek11.Add(AHlavniTabulkaStr);
    LSelect := TStringList.Create;
    LSelectV := TStringList.Create;
    LWhereAll := TStringList.Create;
    try
      { ASelectProEdit }
      AtributyTabulky(AHlavniTabulka, LSelect, True, jnJenJmenaBezPocitanychABinary);
      if AHlavniTabulka > C_PosledniSystemovaTabulka then
        TabSDB := AHlavniTabulkaStr
      else
        TabSDB := Format('%s..%s %1:s', [BracketStr(fHeliosGlobal.SystemDB), AHlavniTabulkaStr]);

      ASelectProEdit := Format('%s FROM %s', [LSelect.CommaText, TabSDB]);
      LSelect.Clear;

      // if ADruhSestavSELECT <> dssStavDokladu then
      begin
        { Pridat UNIQUE KEY }
        LSelect.CommaText := AUniqueKey;

        { Pridat BlokovaniEditoru - pokud je }
        if MaTabulkaAtribut(AHlavniTabulka, uta_BlokovaniEditoru) then
          LSelect.Add(uta_BlokovaniEditoru);

        { Pridat _Barva - pokud je }
        if MaTabulkaAtribut(AHlavniTabulka, C_Barva) then
          LSelect.Add(C_Barva);

        { Pridat _HeIQ_StylPisma - pokud je }
        if MaTabulkaAtribut(AHlavniTabulka, C_StylPisma) then
          LSelect.Add(C_StylPisma);

        { Expanduj na plny nazev }
        for I := 0 to LSelect.Count - 1 do
          LSelect[I] := Format('%s.%s', [AHlavniTabulkaStr, LSelect[I]]);

        { Pridat Vyzadovane }
        PridejVyzadovaneAtributy(AHlavniTabulka, LSelect);

{$IFDEF RK_WhereSys}
        PomStr := TableWhereSys_GetWhereSys(AHlavniTabulkaStr, True);
        if PomStr <> '' then
          LWhereAll.Add(PomStr);
{$ENDIF}
      end;

      { do tohoto okamziku je obsah LSelect a LSelectV stejny }
      { protoze zatim se neprojevily vazby }
      LSelectV.Assign(LSelect);

      LBudeDistinct := False;
      LSysFrom := TStringList.Create;
      LSysFrom11 := TStringList.Create;
      LUpravenyORDERBY := TStringList.Create;
      try
        { WHERESys }
        if WHERESys <> nil then
          AnalyzujWHERE(WHERESys, True);

        { WHEREsysDataskop }
        if WHEREsysDataskop <> nil then
          AnalyzujWHERE(WHEREsysDataskop, False { ! } , True);

        { WHERE }
        if WHERE <> nil then
          AnalyzujWHERE(WHERE, False);

        // if ADruhSestavSELECT = dssBrowse then
        // begin
        // if AMxDInfo <> nil then
        // begin
        // for I := 0 to TGridMxDInfo(AMxDInfo).Details.Count - 1 do
        // AnalyzujSELECT(TGridMxDInfo(TGridMxDInfo(AMxDInfo).Details[I])
        // .ParentAttrs, False);
        // end;
        // end;

        { SELECTsys }
        if SELECTSys <> nil then
        begin
{$IFDEF Ladit}
          if PosEx(',', SELECTSys.Text) > 0 then
            raise Exception.Create('Klauzule SELECTsys SelectoTvurce nesmí obsahovat èárky');
{$ENDIF}
          AnalyzujSELECT(SELECTSys, False);
        end;

        { SELECTBarva }
        if SELECTBarva <> nil then
        begin
{$IFDEF Ladit}
          if PosEx(',', SELECTBarva.Text) > 0 then
            raise Exception.Create('Klauzule SELECTBarva SelectoTvurce nesmí obsahovat èárky');
{$ENDIF}
          AnalyzujSELECT(SELECTBarva, False);
        end;

        { ARegisteredFieldsForCustomDraw }
        if ARegisteredFieldsForCustomDraw <> nil then
        begin
{$IFDEF Ladit}
          if PosEx(',', ARegisteredFieldsForCustomDraw.Text) > 0 then
            raise Exception.Create('Klauzule ARegisteredFieldsForCustomDraw SelectoTvurce nesmí obsahovat èárky');
{$ENDIF}
          AnalyzujSELECT(ARegisteredFieldsForCustomDraw, False);
        end;

        { SELECT }
{$IFDEF Ladit}
        if PosEx(',', SELECT.Text) > 0 then
          raise Exception.Create('Klauzule SELECT SelectoTvurce nesmí obsahovat èárky');
{$ENDIF}
        AnalyzujSELECT(SELECT, True);

        { ORDERBY }
        if ORDERBY <> nil then
        begin
{$IFDEF Ladit}
          if PosEx(',', ORDERBY.Text) > 0 then
            raise Exception.Create('Klauzule ORDER BY SelectoTvurce nesmí obsahovat èárky');
{$ENDIF}
          AnalyzujORDERBY(ORDERBY, LUpravenyORDERBY);
        end;

        InternalSelect.Assign(LSelect);
        InternalSelectV.Assign(LSelectV);

        // [RK 13.04.2001]
        // toto volat az po prirazeni LSelect do InternalSelect !!!!
        // do InternalSelect se totiz dotahuji prime ukazatele na atributy
        _NahradDefPocitaneVSELECTu;

        // // [TZ 03.01.2005] práva na sloupce
        // if Server.TestovatPravaNaSloupce then
        // begin
        // LastTabulkaStr := '';
        // LastTabulka := tZadna;
        // for I := 0 to InternalSelect.Count - 1 do
        // begin
        // PomStr := Atribut2Tabulka(InternalSelect[I]);
        // PA := PAtributTabulky(InternalSelect.Objects[I]);
        // if (PA <> nil) and (PomStr <> '') then
        // begin
        // if not SameText(LastTabulkaStr, PomStr) then
        // begin
        // LastTabulkaStr := PomStr;
        // LastTabulka := TypTabulky(PomStr);
        // end;
        //
        // if not Server.MamPravoAtr(LastTabulka, PA.JmenoSys) then
        // begin
        // if ADruhSestavSELECT = dssStavDokladu then
        // begin
        // LSelect[I] := 'NULL AS ' + PA.JmenoSys;
        // LSelectV[I] := LSelect[I];
        // InternalSelect[I] := LSelect[I];
        // InternalSelectV[I] := LSelect[I];
        // end
        // else
        // raise Exception.Create
        // (sqlCtiOznam(xNemate_pravo_k_zobrazeni_sloupce) + ' '#1 +
        // ddMain.VerejneJmenoTabulky(PomStr) + '.' +
        // VerejneJmenoAtributu(PA) + #1);
        // end;
        // end;
        // end;
        // end;

        // v pripade zmeny POZOR na SestavSelectProTisk -> _Vztahy !!!
        // Predpoklada se dana klauzule na zacatku radku a vse VELKYMI pismeny !!!
        // SELECT...
        // FROM...
        // [WHERE...]
        // [ORDER BY...]
        BrowseSQL.Clear;

        { .$IFDEF Top }
        // je zagrupovano ? -> kdyz jo, nesmi se TOPovat
        // if (ADruhSestavSELECT <> dssStavDokladu) and (AAktualniTvrdyTOP = 0) and
        // (ATop <> 0) then
        // begin
        // for I := 0 to SELECT.Count - 1 do
        // begin
        // if _Group(SELECT.Objects[I]) <> 0 then
        // begin
        // ATop := 0;
        // Break;
        // end;
        // end;
        // end;

        if ATop = 0 then
          LTopStr := ''
        else
          LTopStr := ' TOP ' + IntToStr(ATop);
        { .$ENDIF Top }

        if LBudeDistinct then
          BrowseSQL.Add('SELECT DISTINCT' + LTopStr)
        else
          BrowseSQL.Add('SELECT' + LTopStr);

        I := BrowseSQL.Add(TextSOddelovacem(LSelectV, ','));
        ASelectProRefreshDoBrowse := BrowseSQL[I];
        I := BrowseSQL.AddObject(Format('FROM %s%s', [DefiniceTabulky(TabSDB), TabSDB]), stqFROM);
        ASelectProRefreshDoBrowse := ASelectProRefreshDoBrowse + ' ' + BrowseSQL[I];

        if LSysFrom.Count > 0 then
          BrowseSQL.Add(TextSOddelovacem(LSysFrom, #13#10));

        if LSysFrom11.Count > 0 then
          ASelectProRefreshDoBrowse := ASelectProRefreshDoBrowse + ' ' + TextSOddelovacem(LSysFrom11, ' ');

        if LWhereAll.Count > 0 then
        begin
          TabSDB := '';
          BrowseSQL.AddObject('WHERE', stqWHERE);
          for I := 0 to LWhereAll.Count - 1 do
          begin
            PomStr := LWhereAll[I];

            if KonciNa(PomStr, [' OR', ')OR']) then
            begin
              if PomStr[Length(PomStr) - 2] = #32 then
                LPos := Length(PomStr) - 3
              else
                LPos := Length(PomStr) - 2;
              PomStr := '(' + Copy(PomStr, 1, LPos) + ')';
              if I <> LWhereAll.Count - 1 then
                PomStr := PomStr + 'OR';
            end
            else
            begin
              PomStr := '(' + PomStr + ')';
              if I <> LWhereAll.Count - 1 then
                PomStr := PomStr + 'AND';
            end;

            TabSDB := TabSDB + PomStr;
          end;
          BrowseSQL.Add(TabSDB);
        end;

        if LUpravenyORDERBY.Count > 0 then
        begin
          BrowseSQL.AddObject('ORDER BY', stqORDERBY);
          // POZOR !!! Musi to byt na jednom "radku"
          // v pripade zmeny POZOR na SestavSelectProTisk -> _Vztahy !!!
          BrowseSQL.Add(TextSOddelovacem(LUpravenyORDERBY, ','));
        end;

        { !!! nikdy nedavat SQL.Add('FOR BROWSE'); }

      finally
        LUpravenyORDERBY.Free;
        LSysFrom11.Free;
        LSysFrom.Free;
      end;
    finally
      LWhereAll.Free;
      LSelectV.Free;
      LSelect.Free;
    end;
  finally
    LSeznamTabulek11.Free;
    LSeznamTabulek.Free;
    if not Byl1 then
      InternalSelect.Free;
    if not Byl2 then
      InternalSelectV.Free;
  end;
end;


end.
