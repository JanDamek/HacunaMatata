{ *************************************************************************** }
{                                                                             }
{   Helios Orange                                           powered by LCS    }
{                                                                             }
{   Unita: sqString.pas                                                       }
{          Nektere procedury a funkce prevzate z TpString + nase vlastni      }
{                                                                             }
{ *************************************************************************** }

unit sqString;

interface

uses SysUtils, Classes;

const
  cNull = 'NULL';
  cDefault = 'DEFAULT';

{## jako CEIL, ale s INT64}
function Ceil64(X: Extended): Int64;

{## zbagruje specifikovane znaky ze zacatku stringu }
function TrimLeadChars(const S: string; CS : TSysCharSet): string;

{## zbagruje specifikovane znaky z konce stringu }
function TrimRightChars(const S: string; CS : TSysCharSet): string;

{## Vrati retezec znaku ICh o delce ILen }
function CharStr(ICh : Char; ILen : Integer) : string; inline;

{## Doplni retezec mezerami a orizne na delku ILen }
function sqlDorovnej(const S : string; ILen : Integer) : string;

{## Doplni retezec znakem ICh na delku ILen }
function PadCh(const S : string; ICh : Char; ILen : Integer) : string; inline;

{## Doplni retezec mezerami na delku ILen }
function Pad(const S : string; ILen : Integer) : string;

{## Predrovna retezec znakem ICh na delku ILen }
function LeftPadCh(const S : string; ICh : Char; ILen : Integer) : string; inline;

{## Predrovna retezec mezerami na delku ILen }
function LeftPad(const S : string; ILen : Integer) : string;

{## Odstrani z retezce specifikovane znaky }
function Filter(const S: string; CS: TSysCharSet): string;
function FilterStr(const S: string; CS: TSysCharSet): string;

// odstrani vsechny znaky krome zadanych
function FilterVseKrome(const S: string; CS: TSysCharSet): string;

{## Nahradi v retezci specifikovane znaky znakem ICh }
function ChangeChar(const S : string; CS : TSysCharSet; ICh : Char) : string;

{## Nahradi v retezci retezec retezcem }
function ChangeString(S, SCo, SCim : string) : string;

function ObsahujeRetezec(const S: string; X: array of string): Boolean;

// orizne nadpis pro ToolButton na max 10 znaku, reze prednostne v mezere, doplni tri tecky
function ShortCaption (S: String): String;

{## Convert a longint/word/integer/byte/shortint to a string}
function Long2Str(L : LongInt) : string; inline;

{## Convert a string to an longint, returning true if successful}
function Str2Long(S : string; var I : LongInt) : Boolean;

{## Prevede Real na string }
function Real2Str(R : Extended; IWidth : Byte; IPlaces : ShortInt) : string; inline;

{## Convert a string to a real, returning true if successful}
function Str2Real(const S : string; var E : Extended) : Boolean;

{## Return just the name (no extension, no path) of a pathname}
function JustName(const PathName : string) : string;

{## Prida na konec retezce zpetne lomitko, pokud tam uz neni }
function AddBackSlash(const S : string) : string; inline;

{## Odstrani z konce retezce zpetne lomitko, pokud tam je }
function StripBackSlash(const S : string) : string; inline;

{## Vrati datum ve formatu nezavislem na nastaveni SET DATETIME dmy }
{## Pokud je ADate rovno 0, vraci NULL !!! }
function GetIndependentSQLDate(ADate: TDateTime): string;
function GetIndependentSQLDateTime(ADateTime: TDateTime; Quotovat : Boolean = True): string;

{## Vrati cislo jako string ve formatu SQL nezavislem na mistnim nastaveni }
{## AMaska ... maska pro prikaz Format, napr. '%.2f' }
{!!! POZOR NEPOUŽÍVEJTE JAKO MASKU %g - NEFUNGUJE PODLE OÈEKÁVÁNÍ !!! [TZ 25.08.2004]}
function GetIndependentSQLFloat(const AMaska: string; ACislo: Extended): string;

// Prevede variant na string, aby to vyhovelo serveru
function sqlVarToStr(AVariant: Variant; const AMaska : string = '%.2f'): string;

// Prevede variant na string, aby to vyhovelo serveru, rozlisuje typy a nektere quotuje
function VarToSQLStr(AVariant: Variant; const AMaska : string = '%.2f'): string;

{$ifdef win32}
{## Posle zpravu o stisku klavesy }
procedure PostVirtualKeyEvent(vk: Word; fUp: Boolean);
{$endif}

{## Vrati z Datumu ctvrtleti }
function GetQuarter(ADateTime : TDateTime): Word;

{## Provede test na modulo 11 }
function KontrolaModulo11(const Str : String) : Boolean;

{## Provede test na modulo 10 u specifického symbolu ÈS}
function KontrolaModulo10(Str : String) : Boolean;

{## Jako Pos, ale hleda od dane pozice }
function PosNext(const Substr, S: string; From: Integer): Integer;
function PosNextInsensitive(const Substr, S: string; From: Integer): Integer;

{- Upravi text vraceny serverem}
function UpravExcept(const LExcept : string): string;

{## vrati prvni neprazdny string }
function NaplnHodnotou(Values : array of string): string;

{## pricteni nebo odecteni casti data }
function IncDate(Datum : TDateTime; Days, Months, Years : Integer) : TDateTime;

{## Vrati z Caption horkou klavesu}
function GetHotKey(const ACaption : String) : Char;

{## vyrobi String List z textu oddeleneho tabelatory }
procedure TabulatorText2SL(S  : String;
                           SL : TStringList;
                           APlnitObjects : Boolean = False);
{$ifdef win32}
{dela to same jako metoda TStringList.SetCommaText(), ale jako uvozovac fungujou apostrofy }
procedure LepsiCommaText(SL : TStrings; Value: String);
{$endif}

{dela neco jako treba metoda TStringList.GetCommaText(), ale bez obcasnych uvozovek }
function TextSOddelovacem(aStringList: TStringList;
                          const aOddelovac: String = ',';
                          const aPrefix: String = '') : String;

procedure Fill_SL(SL : TStringList; const aString : String;
  const xOddelovac : string = ','; ASmazatSL: Boolean = False);

// V konverzi nahradi textik u hodnot - FM
procedure ZmenPolozkyKonverze(var aKonverze: string; const aValues: array of string; const aTexts: array of string);

{ vytvoøí StringList s obrácenou konverzí                                      }
{ Pøíklad: AKonverze = '0=Málo' + #13 + '1=Trochu' + #13 + '2=Hodnì'           }
{          Result    = 'Málo=0'                                                }
{                      'Trochu=1'                                              }
{                      'Hodnì=2'                                               }
function ObracenaKonverze(const AKonverze : String) : TStringList;

{vrátí pøíslušný konverzní text hodnotì daného atributu}
function KonverzePolozka(const AHodnota, AKonverze: string; AKdyzNenajdeTakPrazdny : Boolean = True): string;
function KonverzeHodnota(const APolozka, AKonverze: string; AKdyzNenajdeTakPrazdny : Boolean = True): string;
// obdoba ISNULL na sql - FM
function sqIsNull(AVariant, AIsNull: Variant): Variant;

// prevede text na binarni reprezentaci napr. 0xA6899022
function TextToBinary(const S: AnsiString; BufSize: Integer = 0): string;

// prevede SQL BINARY na hexa string
// v zasade totez jako TextToBinary, ale bez '0x' na zacatku
function BinaryToHexaString(const ABinary: AnsiString): string;

// '{A34855D0-C3C8-43BF-BF6D-87FC034323C0}' -> 'A34855D0C3C843BFBF6D87FC034323C0'
function GUIDBezPomlcek(const GUIDStr: String): String;

// otestuje, jestli vstupní string AGUID splòuje pravidla GUIDu,
// pøedpokládá se formát 0F2F4A4C-358A-4F59-93CA-47D172480E00
function JeStringGUID(const AGUID: String): Boolean;

// pøevede z formátu GUID na BINARY
//      7880FC90-5FDB-4811-96B7-8EB15BF09944
//   -> 0x90FC8078DB5F114896B78EB15BF09944
function GUIDtoBinary(const AGUID: String): String;

// [AJ, 8.6.2007] CSV parser (comma separated values)
// Z predaneho stringu (= 1 radek CSV) nacte jednotlive fieldy do SL (jednotlive radky, pak staci dle pozice v CSV cist SL[pozice])
// mozno zmenit defaultni oddelovac ';' za jiny
procedure NactiAtributyCSV2SL(a1RadekCSV : String;
                              SL : TStringList;
                              Separator : char = ';';
                              OdstranUvozovky : Boolean = False;
                              Uvozovka : char = '"');

function ZacinaNa(Co, NaCo: String; ACaseSensitive: Boolean = False): Boolean;

// [RK 07.09.2007] presunuto z sqBrowse                              
function KonciNa(const AVCem: String; ANaCo: array of String;
                 ACaseSensitive : Boolean = False) : Boolean;

function DeleteRightStr(const S: string; Len: Integer): string;

{KJ - vrati spravny tvar Reg.Cisla }
function upravRegCislo(RegCis : string; ZarovnaniRegCisla: Byte; DelkaRegCis: Byte) : string;

{- meni decimal separator}
function ZmenDecimalSeparator(S: string): string;

{- Unicode varianta QuotedStr(), tedy N'...' (pokud používám v neUnicode Delphi, tak je to to samé, co QuotedStr()) }
{  používat jen na SQL-typy: na NCHAR, NVARCHAR, NTEXT, ne napø. na DATETIME, CHAR, VARCHAR, TEXT }
function NQuotedStr(S: string): string; inline;

//vrací True, pokud se Varianty rovnají
//  Tato funkce primárnì vznikla pro porovnání OldValue a Value z TFieldu.
//  Jádro Heliosu podle toho pozná, jestli došlo v editoru ke zmìnì.
//  V unikódové Delphi 2009 se nìkteré SQL-typy projevují v TField.[Old]Value
//  jako Variant Array of Byte, což v døívìjších Delphi nebylo. Na Variant array
//  nelze jednoduše používat operátory = nebo <>. Padá to na výjimku:
//  EVariantTypeCastError 'Could not convert variant of type (Array Byte) into type (Integer)'
//  i když jsou oba Varianty pole shodných typù.
function JsouVariantyRovny(const X, Y: Variant): Boolean;

//pøevede verzi z formátu MMmmrrrrbbbb (pevná délka 12 znakù) na M.m.rrrr.bbbb (Major.Minor.Release.Build)
function VerzeToVerzeSTeckama(Verze       : string;
                              BuildSNulama: Boolean = False): string;

//pøevede verzi z formátu M.m.rrrr.bbb (Major.Minor.Release.Build) na MMmmrrrrbbbb (pevná délka 12 znakù)
function VerzeSTeckamaToVerze(VerzeSTeckama: string): string;

{ ======================================================================================= }

IMPLEMENTATION

uses
  Variants, StrUtils, Math {$ifdef win32}, Windows{$endif};

{ ############################################################################ }

function Ceil64(X: Extended): Int64;
begin
  Result := Trunc(X);
  if Frac(X) > 0 then
    Inc(Result);
end;


{ 21.01.2005, uprava RK+LD
   do vyhledani stringu k zamene pridano '=', aby kdyz menim napr '1' nemenil i 10,11,100 atd.. ale jen '1=' }

procedure ZmenPolozkyKonverze(var aKonverze: string; const aValues: array of string; const aTexts: array of string);
  var
    II, JJ: integer;
    LHodnota: String;
begin
  if High(aValues) <> High(aTexts) then Exit;

  with TStringList.Create do
  try
    Text := aKonverze;
    for II := 0 to Count-1 do
    begin
      for JJ := 0 to High(aValues) do
      begin
        LHodnota := aValues[JJ] + '=';
        if SameText(Copy(Strings[II], 1, Length(LHodnota)), LHodnota) then
        begin
          Strings[II] := Format('%s%s', [LHodnota, aTexts[JJ]]);
          Break;
        end;
      end;
    end;
    aKonverze := Text;
  finally
    Free;
  end;
end;


{ 21.01.2005, puvodni kod
procedure ZmenPolozkyKonverze(var aKonverze: string; const aValues: array of string; const aTexts: array of string);
var
  LStringList : TStringList;
  II, JJ      : integer;
begin
  if High(aValues) <> High(aTexts) then Exit;
  LStringList:= TStringList.Create;
  try
    LStringList.Text:= aKonverze;
    for II:= 0 to LStringList.Count-1 do
    begin
      for JJ:= 0 to High(aValues) do
        if Copy(LStringList[II], 1, Length(aValues[JJ])) = aValues[JJ] then
        begin
          LStringList[II]:= aValues[JJ] + '=' + aTexts[JJ];
          Break;
        end;
    end;
    aKonverze:= LStringList.Text;
  finally
    LStringList.Free;
  end;
end;
}

{ --------------------------------------------------------------------------- }

procedure Fill_SL(SL : TStringList; const aString : String;
  const xOddelovac : string = ','; ASmazatSL: Boolean = False);
  var
    iKrok : integer;
    sTemp : string;
begin
  if ASmazatSL then SL.Clear;

  // [RK 18.03.2010] pokud je prazdny string, tak to pada na Range Check Error!!!
  if Length(aString) > 0 then
  begin
    iKrok := 1;
    repeat
      if aString[iKrok] = xOddelovac then
        begin
          SL.Add(sTemp);
          Inc(iKrok);
          sTemp := '';
        end
      else
        begin
          sTemp := sTemp + aString[iKrok];
          Inc(iKrok);
          if iKrok = Length(aString)+1 then SL.Add(sTemp);
        end;
    until iKrok = Length(aString)+1;
  end;
end;

{ --------------------------------------------------------------------------- }

function GetQuarter(ADateTime : TDateTime): Word;
var Year, Month, Day: Word;
begin
  DecodeDate(ADateTime, Year, Month, Day);
//  Result:= Round(Month/4) + 1;
    Result:= ((Month-1) div 3) + 1;
end;

{ --------------------------------------------------------------------------- }

function CharStr(ICh : Char; ILen : Integer) : string;
begin
  Result := StringOfChar(ICh, ILen);
end;

{ ---------------------------------------------------------------------------- }

function TrimLeadChars(const S: string; CS : TSysCharSet): string;
  var
    II, L: Integer;
begin
  L := Length(S);
  II := 1;
  while (II <= L) and CharInSet(S[II], CS) do Inc(II);
  Result := Copy(S, II, MaxInt);
end;

{ ---------------------------------------------------------------------------- }

function TrimRightChars(const S: string; CS : TSysCharSet): string;
  var
    II : Integer;
begin
  for II := Length(S) downto 1 do
    if not CharInSet(S[II], CS) then
    begin
      Result := Copy(S, 1, II);
      Exit;
    end;

  Result := '';
end;

{ ---------------------------------------------------------------------------- }

function sqlDorovnej(const S : string; ILen : Integer) : string;
begin
  Result := Pad(Format('%.*s', [ILen, S]), ILen);
end;

{ ---------------------------------------------------------------------------- }

function PadCh(const S : string; ICh : Char; ILen : Integer) : string;
begin
  Result := S + StringOfChar(ICh, ILen - Length(S));
(*
  SLen := Length(S);
  if SLen >= ILen then
    Result := S
  else
    begin
      SetLength(Result, ILen);
      if SLen > 0 then
        Move(S[1], Result[1], SLen);
      FillChar(Result[Succ(SLen)], ILen - SLen, ICh); // MS - z hlediska Unicode je toto komentar
    end;
*)
end;

{ ---------------------------------------------------------------------------- }

function Pad(const S : string; ILen : Integer) : string;
begin
  Pad := PadCh(S, ' ', ILen);
end;

{ ---------------------------------------------------------------------------- }

function LeftPadCh(const S : string; ICh : Char; ILen : Integer) : string;
begin
  Result := StringOfChar(ICh, ILen - Length(S)) + S;
(*
  SLen := Length(S);
  if SLen >= ILen then
    Result := S
  else
    begin
      SetLength(Result, ILen);
      if SLen > 0 then
        Move(S[1], Result[Succ(ILen)-SLen], SLen);
      FillChar(Result[1], ILen-SLen, ICh); // MS - z hlediska Unicode je toto komentar
    end;
*)
end;

{ ---------------------------------------------------------------------------- }

function LeftPad(const S : string; ILen : Integer) : string;
begin
  LeftPad := LeftPadCh(S, ' ', ILen);
end;

{ ---------------------------------------------------------------------------- }

function FilterStr(const S : string; CS : TSysCharSet) : string;
  var
    I : Integer;
begin
  Result := '';
  for I := 1 to Length(S) do
    if not CharInSet(S[I], CS) then
      Result := Result + S[I];
end;

{ ---------------------------------------------------------------------------- }

function Filter(const S : string; CS : TSysCharSet) : string;
begin
  Result := FilterStr(S, CS);
end;

{ --------------------------------------------------------------------------- }

function FilterVseKrome(const S: string; CS: TSysCharSet): string;
  var
    I : Integer;
begin
  Result := '';
  for I := 1 to Length(S) do
    if CharInSet(S[I], CS) then
      Result := Result + S[I];
end;

{ --------------------------------------------------------------------------- }

function ChangeChar(const S : string; CS : TSysCharSet; ICh : Char) : string;
  var
    II : Integer;
begin
  Result := S;
  for II := 1 to Length(Result) do
    if CharInSet(Result[II], CS) then
      Result[II] := ICh;
end;

{ ---------------------------------------------------------------------------- }

function ChangeString(S, SCo, SCim : string) : string;
  var
    LPos   : Integer;
    LOdkud : Integer;
begin
  if (S <> '') and (SCo <> '') and (SCo <> SCim) then
  begin
    LPos := PosEx(SCo, S);
    while LPos > 0 do
    begin
      System.Delete(S, LPos, Length(SCo));
      System.Insert(SCim, S, LPos);

      LOdkud := LPos + Length(SCim);
      LPos := PosEx(SCo, Copy(S, LOdkud, MaxInt));
      if LPos > 0 then
        Inc(LPos, LOdkud - 1);
    end;
  end;
  
  Result := S;
end;

{ ---------------------------------------------------------------------------- }

function Long2Str(L : LongInt) : string;
begin
  Result := IntToStr(L);
end;

{ ---------------------------------------------------------------------------- }

function Str2Long(S : string; var I : LongInt) : Boolean;
  {-Convert a string to a longint, returning true if successful}
  var
    code : Integer;
    SLen : Integer;
//    SLen : Byte absolute S;  // OLD VERSION
begin
  S := Trim(S);       
  SLen := Length(S);

  if (SLen > 1) and (Upcase(S[SLen]) = 'H') then begin          {!!.21 begin}
    S := '$' + Copy(S, 1, SLen-1);
  end
  else if (SLen > 2) and (S[1] = '0') and (Upcase(S[2]) = 'X') then begin
    S := '$' + Copy(S, 3, SLen-2);
  end;                                                          {!!.21 end}
  Val(S, I, code);
  if code <> 0 then begin
    I := code;
    Result := False;
  end else
    Result := True;

(*  OLD VERSION
  while S[SLen] = ' ' do
    Dec(SLen);
  if (SLen > 1) and (Upcase(S[SLen]) = 'H') then begin          {!!.21 begin}
    Move(S[1], S[2], SLen-1);
    S[1] := '$';
  end
  else if (SLen > 2) and (S[1] = '0') and (Upcase(S[2]) = 'X') then begin
    Dec(SLen);
    Move(S[3], S[2], SLen-1);
    S[1] := '$';
  end;                                                          {!!.21 end}
  Val(S, I, code);
  if code <> 0 then begin
    I := code;
    Str2Long := False;
  end else
    Str2Long := True; *)
end;

{ ---------------------------------------------------------------------------- }

function Real2Str(R : Extended; IWidth : Byte; IPlaces : ShortInt) : string;
begin
  Result := FloatToStrF(R, ffFixed, IWidth, IPlaces);
end;

{ ---------------------------------------------------------------------------- }

function Str2Real(const S : string; var E : Extended) : Boolean;
begin
  try
    E := StrToFloat(S);
    Result := True;
  except
    Result := False;
  end;
end;

{ ---------------------------------------------------------------------------- }

function JustName(const PathName : string) : string;
  {-Return just the name (no extension, no path) of a pathname}
var
  DotPos : Integer;
begin
  Result := ExtractFileName(PathName);
  DotPos := Pos('.', Result);
  if DotPos > 0 then
    Result := Copy(Result, 1, DotPos-1);
end;

{ ---------------------------------------------------------------------------- }

function AddBackSlash(const S : string) : string;
begin
  Result := IncludeTrailingPathDelimiter(S);
end;

{ ---------------------------------------------------------------------------- }

function StripBackSlash(const S : string) : string;
begin
  Result := ExcludeTrailingPathDelimiter(S);
end;

{ ---------------------------------------------------------------------------- }

function GetIndependentSQLDate(ADate: TDateTime): string;
begin
  if ADate = 0 then
    Result := cNULL
  else
    {! NEMENIT ! Tento format data je pro SQL nezavisly !}
    Result := QuotedStr(FormatDateTime('yyyymmdd', ADate));
end;

{ --------------------------------------------------------------------------- }

function GetIndependentSQLDateTime(ADateTime: TDateTime; Quotovat : boolean = True): string;
begin
  if ADateTime = 0 then
    Result := cNULL
  else
    {! NEMENIT ! Tento format data je pro SQL nezavisly !}
    begin
      Result := FormatDateTime('yyyymmdd hh":"nn":"ss.zzz', ADateTime);
      if Quotovat then
        Result := QuotedStr(Result);
    end;
end;

{ --------------------------------------------------------------------------- }

function GetIndependentSQLFloat(const AMaska: string; ACislo: Extended): string;
  var
    OldThousandSeparator : Char;
    OldDecimalSeparator  : Char;
begin
  OldThousandSeparator := FormatSettings.ThousandSeparator;
  OldDecimalSeparator  := FormatSettings.DecimalSeparator;

  // pro jistotu, kdyby byly nastaveny oba stejne v mistnim nastaveni
  FormatSettings.ThousandSeparator := ',';
  FormatSettings.DecimalSeparator  := '.'; {! NEMENIT ! SQL Server pouziva tecku !}

  try
    // prevod cisla
    Result := Format(AMaska, [ACislo]);
    // vybagrovani oddelovace tisicu a prebytecnych mezer
    Result := Filter(Result, [FormatSettings.ThousandSeparator, ' ']);
  finally
    FormatSettings.ThousandSeparator := OldThousandSeparator;
    FormatSettings.DecimalSeparator  := OldDecimalSeparator;
  end;
end;

{------------------------------------------------}
function sqlVarToStr(AVariant: Variant; const AMaska : string = '%.2f'): string;
begin
  if VarIsNull(AVariant) then
    Result:= 'NULL'
  else
    Result:= GetIndependentSQLFloat(AMaska, AVariant);
end;


{------------------------------------------------}
function VarToSQLStr(AVariant: Variant; const AMaska : string = '%.2f'): string;
begin
  case TVarData(AVariant).VType of
  //varEmpty = $0000;
    varNull     : Result := 'NULL';
    varByte,
    varSmallint,
    varInteger  : Result := VarToStr(AVariant);
    varSingle,
    varDouble,
    varCurrency : Result := GetIndependentSQLFloat(AMaska, AVariant);
    varDate     : Result := GetIndependentSQLDateTime(AVariant);
    varString   : Result :=  QuotedStr(AVariant); //[TZ 20.08.2009] jde o AnsiString
    varOleStr,
    varUString  : Result := NQuotedStr(AVariant); //[LD 20.08.2009] jde o String (=UnicodeString)
    else          Result := '';
  end;
end;

{ --------------------------------------------------------------------------- }

{$ifdef win32}
procedure PostVirtualKeyEvent(vk: Word; fUp: Boolean);
  const
    ButtonUp: array[False..True] of DWORD = (0, KEYEVENTF_KEYUP);
  var
    ScanCode: Byte;
begin
  if vk <> vk_SnapShot then
    ScanCode := MapVirtualKey(vk, 0)
  else
    { Special processing for the PrintScreen key. If scan code }
    { is set to 1 it copies entire screen. If set to 0 it }
    { copies active window. We'll just set it to 0 for now }
    ScanCode := 0;
  Keybd_Event(vk, ScanCode, ButtonUp[fUp], 0);
end;
{$endif}

{ --------------------------------------------------------------------------- }

function KontrolaModulo11(const Str : String) : Boolean;
  const
    Vahy : array[1..10] of Byte = (1,2,4,8,5,10,9,7,3,6);
  var
    Counter   : Integer;
    Soucet    : Longint;
    TxLongInt : LongInt;
begin
  Result:= False;

  { kontrola delek }
  if Length(Str) > 10 then Exit; {!}

  Soucet:= 0;
  for Counter:= Length(Str) downto 1 do
  begin
    if not Str2Long(Str[Counter], TxLongInt) then Exit; {!}
    Soucet:= Soucet + TxLongInt*Vahy[1+Length(Str)-Counter];
  end;

  if (Soucet Mod 11)<>0 then Exit; {!}

  Result:= True;
end;

{ --------------------------------------------------------------------------- }

function KontrolaModulo10(Str : String) : Boolean;

  const
    Vahy : array[1..7] of Byte = (1,7,3,1,7,3,1);  //,1,1,1
  var
    Counter   : Integer;
    Soucet    : Longint;
    TxLongInt : LongInt;
begin
  Result:= False;

  // min.rozsah jsou 2 znaky, max. 7 znakù, doplnìk do 10 tvoøí vodící nuly
  // odstranìní vodících nul
  if not Str2Long(Str, TxLongInt) then Exit; {!}
  Str := IntToStr(TxLongInt);
  { kontrola delek }
  if Length(Str) > 7 then Exit; {!}

  Soucet:= 0;
  for Counter:= Length(Str) downto 1 do
  begin
    if not Str2Long(Str[Counter], TxLongInt) then Exit; {!}
    Soucet:= Soucet + TxLongInt*Vahy[1+Length(Str)-Counter];
  end;

  if (Soucet Mod 10)<>0 then Exit; {!}

  Result:= True;
end;

{ --------------------------------------------------------------------------- }

function PosNext(const Substr, S: string; From: Integer): Integer;
begin
  if From < 1 then From := 1;
  Result := PosEx(Substr, S, From);
end;

{ --------------------------------------------------------------------------- }

function PosNextInsensitive(const Substr, S: string; From: Integer): Integer;
begin
  if From < 1 then From := 1;
  Result := PosEx(AnsiUpperCase(Substr), AnsiUpperCase(S), From);
end;

{ --------------------------------------------------------------------------- }

function UpravExcept(const LExcept : string): string;
begin
  Result:= LExcept;
  while Pos('|', Result) <> 0 do
  begin
    Insert(' ', Result, Pos('|', Result));
    Insert(' ', Result, Pos('|', Result)+1);
    Result[Pos('|', Result)]:= '-';
  end;
end;

{ --------------------------------------------------------------------------- }

function NaplnHodnotou(Values : array of string): string;
  var
    II : Integer;
begin
  for II := Low(Values) to High(Values) do
    if Trim(Values[II]) <> '' then
    begin
      Result := Values[II];
      Exit;
    end;
  Result := '';
end;

{ --------------------------------------------------------------------------- }

function IncDate(Datum : TDateTime; Days, Months, Years : Integer) : TDateTime;
  {-Add (or subtract) the number of months, days, and years to a date.
    Months and years are added before days. No overflow/underflow
    checks are made}
  var
    Dayw, Monthw, Yearw : Word;
    Day, Month, Year, Day28Delta : Integer;
begin
  DecodeDate(Datum, Yearw, Monthw, Dayw);
  Day   := Dayw;
  Month := Monthw;
  Year  := Yearw;
  Day28Delta := Day-28;
  if Day28Delta < 0 then
    Day28Delta := 0
  else
    Day := 28;

  Inc(Year, Years);
  Inc(Year, Months div 12);
  Inc(Month, Months mod 12);
  if Month < 1 then begin
    Inc(Month, 12);
    Dec(Year);
  end
  else if Month > 12 then begin
    Dec(Month, 12);
    Inc(Year);
  end;

  Datum := EncodeDate(Year, Month, Day);
  Datum := Datum + Days + Day28Delta;

  Result := Datum;
end;

{ --------------------------------------------------------------------------- }

function GetHotKey(const ACaption : String) : Char;
var
  PoziceHK : Integer;
  HK : String;
begin
  PoziceHK := Pos('&', ACaption);
  if PoziceHK > 0
  then   // nalezena
    begin
      HK := Copy(ACaption, PoziceHK + 1, 1);
      GetHotKey := HK[1];
    end
  else
    GetHotKey := Chr(255);   // => nenalezena
end;

{ --------------------------------------------------------------------------- }

procedure TabulatorText2SL(S  : String;
                           SL : TStringList;
                           APlnitObjects : Boolean = False);
  {.............................................................}
  procedure _Pridej(SS : String);
  var
    LWidth  : Integer;
    LPos    : Integer;
  begin
    if APlnitObjects then
    begin
      LWidth := 0;
      LPos := Pos(' ', SS);
      if LPos>0 then
      begin
        LWidth := StrToIntDef(Copy(SS, LPos+1, MaxInt), 0);
        System.Delete(SS, LPos, MaxInt);
      end;

      SL.AddObject(SS, Pointer(LWidth));
    end
    else
      SL.Add(SS);
  end;
  {.............................................................}
var
  LPosTab : Integer;
//PomS    : String;
begin
  SL.Clear;
  if Trim(S)<>'' then
  begin
    LPosTab := Pos(#9, S);
    while LPosTab>0 do
    begin
      _Pridej(Copy(S, 1, LPosTab-1));
      Delete(S, 1, LPosTab);
      LPosTab := Pos(#9, S);
    end;
    _Pridej(S);
  end;
end;

{ --------------------------------------------------------------------------- }
{$ifdef win32}
procedure LepsiCommaText(SL : TStrings; Value: String);
const C_Uvozovka = '''';
var
  P, P1 : PChar;
  S     : String;
begin
  with SL do
  begin
    BeginUpdate;
    try
      Clear;
      P := PChar(Value);
      while CharInSet(P^, [#1..' ']) do P := CharNext(P);
      while P^ <> #0 do
      begin
        if P^ = C_Uvozovka then
          S := AnsiExtractQuotedStr(P, C_Uvozovka)
        else
        begin
          P1 := P;
          while (P^ > ' ') and (P^ <> ',') do P := CharNext(P);
          SetString(S, P1, P - P1);
        end;
        Add(S);
        while CharInSet(P^, [#1..' ']) do P := CharNext(P);
        if P^ = ',' then
          repeat
            P := CharNext(P);
          until not CharInSet(P^, [#1..' ']);
      end;
    finally
      EndUpdate;
    end;
  end;
end;
{$endif}
{ --------------------------------------------------------------------------- }

function TextSOddelovacem(aStringList: TStringList;
                          const aOddelovac: String = ',';
                          const aPrefix: String = '') : String;
  var
    II : Integer;
begin
  if (not Assigned(aStringList)) or (aStringList.Count = 0) then
    Result := ''
  else
    begin
      Result := Format('%s%s', [aPrefix, aStringList[0]]);
      for II := 1 to aStringList.Count-1 do
        Result := Format('%s%s%s%s', [Result, aOddelovac, aPrefix, aStringList[II]]);
    end;
end;

{ --------------------------------------------------------------------------- }

function ObracenaKonverze(const AKonverze : String) : TStringList;
  var
    II, JJ : Integer;
    PomStr : String;
begin
  Result := TStringList.Create;
  Result.Text := AKonverze;
  for II := 0 to Result.Count-1 do
  begin
    PomStr := Result[II];
    JJ := Pos('=', PomStr);
    Result[II] := Format('%s=%s', [Copy(PomStr, JJ+1, MaxInt), Copy(PomStr, 1, JJ-1)]);
  end;
end;

{ --------------------------------------------------------------------------- }

function KonverzePolozka(const AHodnota, AKonverze: string; AKdyzNenajdeTakPrazdny : Boolean = True): string;
var SL : TStringList;
    f  : integer;
    S  : String;
    P  : Integer;
begin
  if AKdyzNenajdeTakPrazdny then Result := ''
                            else Result := AHodnota;
  SL := TStringList.Create;
  try
    SL.Text := AKonverze;
    for f := 0 to SL.Count-1 do
    begin
      S := SL[f];
      P := Pos('=', S);
      if SameText(Copy(S, 1, P-1), AHodnota) then
      begin
        Result := Copy(S, P + 1, Length(S) - P);
        Break;
      end;
    end;
  finally
    SL.Free;
  end;
end;

{------------------------------------------------}
function KonverzeHodnota(const APolozka, AKonverze: string; AKdyzNenajdeTakPrazdny : Boolean = True): string;
var SL : TStringList;
    f  : integer;
    S  : String;
    P  : Integer;
begin
  if AKdyzNenajdeTakPrazdny then Result := ''
                            else Result := APolozka;
  SL := TStringList.Create;
  try
    SL.Text := AKonverze;
    for f := 0 to SL.Count-1 do
    begin
      S := SL[f];
      P := Pos('=', S);
      if SameText(Copy(S, P + 1, Length(S) - P), APolozka) then
      begin
        Result := Copy(S, 1, P - 1);
        Break;
      end;
    end;
  finally
    SL.Free;
  end;
end;

{ --------------------------------------------------------------------------- }

function sqIsNull(AVariant, AIsNull: Variant): Variant;
begin
  if VarIsNull(AVariant) then Result:= AIsNull else Result:= AVariant;
end;

{ --------------------------------------------------------------------------- }

//rychlejší verze - testováno na 100MB XML stringu
function TextToBinary(const S: AnsiString; BufSize: Integer = 0): String;
begin
  if BufSize = 0 then
    BufSize := Length(S);
  SetLength(Result, 2 + (2 * BufSize));
  Result[1] := '0';
  Result[2] := 'x';
  Classes.BinToHex(PAnsiChar(S), PWideChar(Result) + 2, BufSize);
end;

{Pùvodní pomalejší (a hlavnì "žravìjší") verze:
 co je tu špatnì:
 - buffer je AnsiString, ale Result string, takže dochází zbyteènì ke konverzi a existenci 2 instancí stringù
 - na konci spojení 0x s bufferem -> 2 instance témìø stejného stringu
 - s HT jsme nakonec došli k tomu, že 100MB string naalokoval asi 300 MB
function TextToBinary(const S: AnsiString; BufSize: Integer = 0): String;
var Buf: AnsiString;
begin
  if BufSize = 0 then
    BufSize := Length(S);
  SetLength(Buf, 2 * BufSize);
  Classes.BinToHex(PAnsiChar(S), PAnsiChar(Buf), BufSize);
  Result := '0x' + string(Buf);
end;}

{ --------------------------------------------------------------------------- }

function BinaryToHexaString(const ABinary: AnsiString): string;
begin
  SetLength(Result, 2*Length(ABinary));
  Classes.BinToHex(PAnsiChar(ABinary), PWideChar(Result), Length(ABinary));
end;

{ --------------------------------------------------------------------------- }

function GUIDBezPomlcek(const GUIDStr: String): String;
begin
  Result := FilterStr(GUIDStr, ['{','-','}']);
end;

{ --------------------------------------------------------------------------- }

// otestuje, jestli vstupní string AGUID splòuje pravidla GUIDu,
// pøedpokládá se formát 0F2F4A4C-358A-4F59-93CA-47D172480E00
function JeStringGUID(const AGUID: String): Boolean;
var I, X : Integer;
begin
  Result := False; //pøednastavim, že ne

  //GUID musí mít pøesnì 36 znakù
  if Length(AGUID)<>36 then Exit;

  //má v sobì 4x pomlèku na pøesnì daných pozicích
  if AGUID[ 9]<>'-'    then Exit;
  if AGUID[14]<>'-'    then Exit;
  if AGUID[19]<>'-'    then Exit;
  if AGUID[24]<>'-'    then Exit;

  //uvnitø GUIDu mohou být jen hexadecimální èíslice a -
  for I := 1 to 36 do
  begin
    X := Ord(AGUID[I]);
    if not(   (X=45)                     // -
           or ((X>=48) and (X<=57))      // 0..9
           or ((X>=65) and (X<=70))      // A..F
           or ((X>=97) and (X<=102))     // a..f
          ) then
      Exit;
  end;

  //pokud to došlo až sem, tak je to GUID
  Result := True;
end;

{ --------------------------------------------------------------------------- }

// pøevede z formátu GUID na BINARY
//      7880FC90-5FDB-4811-96B7-8EB15BF09944
//   -> 0x90FC8078DB5F114896B78EB15BF09944
function GUIDtoBinary(const AGUID: String): String;
begin
  if not JeStringGUID(AGUID) then
    raise Exception.Create(AGUID + ' není platný GUID!');

  Result := '0x' + AGUID[ 7] + AGUID[ 8]
                 + AGUID[ 5] + AGUID[ 6]
                 + AGUID[ 3] + AGUID[ 4]
                 + AGUID[ 1] + AGUID[ 2]
                 + AGUID[12] + AGUID[13]
                 + AGUID[10] + AGUID[11]
                 + AGUID[17] + AGUID[18]
                 + AGUID[15] + AGUID[16]
                 + System.Copy(AGUID, 20,  4)
                 + System.Copy(AGUID, 25, 12);
end;

{ --------------------------------------------------------------------------- }

// [AJ, 8.6.2007] CSV parser (comma separated values)
// Z predaneho stringu (= 1 radek CSV) nacte jednotlive fieldy do SL (jednotlive radky, pak staci dle pozice v CSV cist SL[pozice])
// mozno zmenit defaultni oddelovac ';' za jiny
//[AJ, 8.12.2009] - rozšíøená verze od PFA
  procedure NactiAtributyCSV2SL(a1RadekCSV : String;
                                SL : TStringList;
                                Separator : char = ';';
                                OdstranUvozovky : Boolean = False;
                                Uvozovka : char = '"');
  var
    Pozice, Korekce, PozicePraveUvozovky : Integer;
    a1Field : String;
    ItemVUvozovkach : Boolean;
  begin
    SL.Clear;
    if Trim(a1RadekCSV)=''
      then Exit;
    if OdstranUvozovky
      then a1RadekCSV:=Filter(a1RadekCSV, [Uvozovka]);
    repeat
      Korekce:=1;
//<PFA 20091201>
      Pozice:=0;
      ItemVUvozovkach:=False;
      if Copy(a1RadekCSV,1,1)=Uvozovka then
      begin
        PozicePraveUvozovky:=PosNext(Uvozovka,a1RadekCSV,2);
        //pokud existuje prava i leva uvozovka vyhovujici pravidlum CSV, tak si nastavim ItemVUvozovkach a korekci
        if (PozicePraveUvozovky>0) and
        (
          (PozicePraveUvozovky=Length(a1RadekCSV)) or
          (Copy(a1RadekCSV, PozicePraveUvozovky+1, 1) = Separator)
        ) then
        begin
          ItemVUvozovkach:=True;
          Korekce:=2;
          //pokud je v uvozovkach, tak nebudu vyhledavat standardnim zpusobem - oddelovac muze byt beztrestne obsazen mezi uvozovkami Itemu
          Pozice:=PozicePraveUvozovky
        end;
      end;
      if not ItemVUvozovkach then
//</PFA 20091201>
        Pozice := Pos(Separator, a1RadekCSV);
      if Pozice=0 then
      begin
        Pozice:=Length(a1RadekCSV);
        Korekce:=0;
      end;
      if Pozice>0 then
      begin
        a1Field :=Trim(Copy(a1RadekCSV,Max(1,Korekce),Pozice-Korekce));

//<PFA 20091201>
        //pokud je v uvozovkach, tak nahradit dvojite uvozovky za jednoduche
        if ItemVUvozovkach then
          StringReplace(a1Field,Uvozovka+Uvozovka,Uvozovka,[rfReplaceAll]);
//</PFA 20091201>

        a1RadekCSV := Trim(Copy(a1RadekCSV,Pozice+Max(1,Korekce),MaxInt));
        SL.Add(a1Field);
      end;
    until Pozice=0
  end;

{ --------------------------------------------------------------------------- }

function ZacinaNa(Co, NaCo: String; ACaseSensitive: Boolean = False): Boolean;
begin
  Co := System.Copy(Co, 1, Length(NaCo));
  if ACaseSensitive then Result := CompareStr(Co, NaCo) = 0
                    else Result := SameText  (Co, NaCo);
end;

{ --------------------------------------------------------------------------- }

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

{ --------------------------------------------------------------------------- }

function DeleteRightStr(const S: string; Len: Integer): string;
begin
  Len := Length(S) - Len;
  if Len > 0 then
    Result := Copy(S, 1, Len);
end;

{ --------------------------------------------------------------------------- }

function upravRegCislo(RegCis: string; ZarovnaniRegCisla: Byte; DelkaRegCis: Byte): string;
begin
  case ZarovnaniRegCisla of
  1:begin //bez zarovnani
    end;
  2:begin // zleva
{      if Length (RegCis) < DelkaRegCis then
      repeat
        RegCis := '0' + RegCis;
      until Length (RegCis) = DelkaRegCis;}
      RegCis := LeftPadCh (RegCis,'0', DelkaRegCis);
    end;
  3:begin  // zprava
{      if Length (RegCis) < DelkaRegCis then
      repeat
        RegCis:= RegCis + '0';
      until Length (RegCis) = DelkaRegCis;}
      RegCis := PadCh(RegCis,'0', DelkaRegCis);
    end;
  end;
  Result := RegCis;
end;

{ --------------------------------------------------------------------------- }

function ZmenDecimalSeparator(S : string) : string;
begin
  Result := StringReplace(S, '.', FormatSettings.DecimalSeparator, [rfReplaceAll]);
end;

{ --------------------------------------------------------------------------- }

function NQuotedStr(S: string): string; inline;
begin
  Result := 'N' + SysUtils.QuotedStr(S);
end;

{ --------------------------------------------------------------------------- }

//vrací True, pokud se Varianty rovnají
//  Tato funkce primárnì vznikla pro porovnání OldValue a Value z TFieldu.
//  Jádro Heliosu podle toho pozná, jestli došlo v editoru ke zmìnì.
//  V unikódové Delphi 2009 se nìkteré SQL-typy projevují v TField.[Old]Value
//  jako Variant Array of Byte, což v døívìjších Delphi nebylo. Na Variant array
//  nelze jednoduše používat operátory = nebo <>. Padá to na výjimku:
//  EVariantTypeCastError 'Could not convert variant of type (Array Byte) into type (Integer)'
//  i když jsou oba Varianty pole shodných typù.
function JsouVariantyRovny(const X, Y: Variant): Boolean;
var I, Xlo, Xhi, Ylo, Yhi: Integer;
begin
  if VarIsArray(X) or VarIsArray(Y) then
  begin
    Result := False;

    //pole a nepole jsou si vždy nerovna
    if not VarIsArray(X) then Exit;
    if not VarIsArray(Y) then Exit;

    //obì pole musí mít právì jednu dimenzi
    if VarArrayDimCount(X)<>1 then Exit;
    if VarArrayDimCount(Y)<>1 then Exit;

    //pole se považují za shodná, pokud mají stejný dolní a horní index
    Xlo := VarArrayLowBound(X, 1);
    Ylo := VarArrayLowBound(Y, 1);
    if Xlo<>Ylo then Exit;
    Xhi := VarArrayHighBound(X, 1);
    Yhi := VarArrayHighBound(Y, 1);
    if Xhi<>Yhi then Exit;

    //mohu porovnávat
    for I := Xlo to Xhi do
    begin
      if X[I]<>Y[I] then Exit;
    end;

    //když to došlo až sem, tak se rovnají
    Result := True;
  end
  else
  begin
    //ani A ani B není pole, mohu "postaru"
    Result := X = Y;
  end;
end;

{ --------------------------------------------------------------------------- }

function ObsahujeRetezec(const S: string; X: array of string): Boolean;
  var
    II: Integer;
begin
  if Length(X) > 0 then
    for II := Low(X) to High(X) do
    begin
      if PosEx(X[II], S) <> 0 then
      begin
        Result := True;
        Exit;
      end;
    end;

  Result := False;
end;

{ --------------------------------------------------------------------------- }

// orizne nadpis pro ToolButton na max 10 znaku, reze prednostne v mezere, doplni tri tecky
function ShortCaption(S: string): string;
  var
    i: Integer;
  begin
    if Length (S) > 10 then
      begin
        i := Pos (#32, S); // pokus se najit mezeru
        if (i = 0) or (i > 10) then i := 10;
        S := Format ('%.*s...', [i - 1, S]);
      end;
    Result := S;
  end;

{ --------------------------------------------------------------------------- }

//pøevede verzi z formátu MMmmrrrrbbbb (pevná délka 12 znakù) na M.m.rrrr.bbbb (Major.Minor.Release.Build)
function VerzeToVerzeSTeckama(Verze       : string;
                              BuildSNulama: Boolean = False): string;
  {...........................................}
  function _ZkusTo(SS: string): string;
  var Err, II: Integer;
  begin
    Val(SS, II, Err);
    if Err = 0 then Result := IntToStr(II)
               else Result := SS;
  end;
  {...........................................}
begin
  Result := _ZkusTo(System.Copy(Verze, 1, 2)) +'.'+  //MM
            _ZkusTo(System.Copy(Verze, 3, 2)) +'.'+  //mm
            _ZkusTo(System.Copy(Verze, 5, 4)) +'.';  //rrrr

  if BuildSNulama then
    Result := Result +         System.Copy(Verze, 9, 4)   //bbbb
  else
    Result := Result + _ZkusTo(System.Copy(Verze, 9, 4)); //bbb
end;

//pøevede verzi z formátu M.m.rrrr.bbbb (Major.Minor.Release.Build) na MMmmrrrrbbbb (pevná délka 12 znakù)
function VerzeSTeckamaToVerze(VerzeSTeckama: string): string;
begin
  with TStringList.Create do
  try
    Delimiter     := '.';
    DelimitedText := VerzeSTeckama;
    if Count = 4 then
    begin
      Result := LeftPadCh(Trim(Strings[0]), '0', 2)   //MM
              + LeftPadCh(Trim(Strings[1]), '0', 2)   //mm
              + LeftPadCh(Trim(Strings[2]), '0', 4)   //rrrr
              + LeftPadCh(Trim(Strings[3]), '0', 4);  //bbbb
    end
    else
      Result := VerzeSTeckama;
  finally
    Free;
  end;
end;

{ ############################################################################ }

end.
