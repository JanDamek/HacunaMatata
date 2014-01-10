
//////////////////////////////////////////////////
//  Copyright © 1998-2012 Devart. All right reserved.
//  CRFunctions
//////////////////////////////////////////////////

{$IFNDEF CLR}

{$I Dac.inc}

unit CRFunctions;
{$ENDIF}
interface

uses
  Classes, {$IFDEF VER6P}Variants,{$ENDIF} CRTypes;

{$IFDEF VER6}
  function StrToBool(const S: string): Boolean;
  function TryStrToBool(const S: string; out Value: Boolean): Boolean;
{$ENDIF}

{ Delphi 5 support }

{$IFNDEF VER6P}
  function TryStrToInt(const S: string; out Value: Integer): Boolean;
  function BoolToStr(const Value: boolean; UseBoolStrs: Boolean = False): string;
  function TryStrToBool(const S: string; out Value: Boolean): Boolean;
  function StrToBool(const S: string): Boolean;
  procedure DecodeDateTime(const AValue: TDateTime; out AYear, AMonth, ADay,
    AHour, AMinute, ASecond, AMilliSecond: Word);
  function WideUpperCase(const S: WideString): WideString;
  function VarToWideStr(const V: Variant): WideString;
  function VarIsStr(const V: Variant): Boolean;
  function AnsiDequotedStr(const S: string; AQuote: Char): string;
  function AnsiContainsText(const AText, ASubText: string): Boolean;
{$ENDIF}

{ Unicode build support }

  function _LowerCase(const S: _string): _string;
  function _UpperCase(const S: _string): _string;
  function _CompareText(const S1, S2: _string): Integer;
  function _SameText(const S1, S2: _string): Boolean;
  function _QuotedStr(const S: _string; AQuote: _char): _string;
  function _DequotedStr(const S: _string; AQuote: _char): _string;
  function _VarToStr(const V: Variant): _string;
  function _Format(const AFormat: _string; const Args: array of const): _string;

  procedure AssignStrings(Source: _TStrings; Dest: TStrings); overload;
{$IFDEF CRUNICODE}
  procedure AssignStrings(Source: TStrings; Dest: _TStrings); overload;
{$ENDIF}

{$IFDEF UTF8}
  function UnicodeToUtf8(Dest: PAnsiChar; Source: PWideChar; MaxBytes: Integer): Integer; overload;
  function UnicodeToUtf8(Dest: PAnsiChar; MaxDestBytes: Cardinal; Source: PWideChar; SourceChars: Cardinal): Cardinal; overload;
  function Utf8ToUnicode(Dest: PWideChar; Source: PAnsiChar; MaxChars: Integer): Integer; overload;
  function Utf8ToUnicode(Dest: PWideChar; MaxDestChars: Cardinal; Source: PAnsiChar; SourceBytes: Cardinal): Cardinal; overload;
  function Utf8Encode(const WS: WideString): UTF8String;
  function Utf8Decode(const S: UTF8String): WideString;
  function AnsiToUtf8(const S: AnsiString): UTF8String;
  function Utf8ToAnsi(const S: UTF8String): AnsiString;
{$ELSE}{$IFNDEF CLR}
// These functions was changed in the Delphi XE and they are copied to this unit for compatibility
  function UnicodeToUtf8(Dest: PAnsiChar; Source: PWideChar; MaxBytes: Integer): Integer; overload;
  function UnicodeToUtf8(Dest: PAnsiChar; MaxDestBytes: Cardinal; Source: PWideChar; SourceChars: Cardinal): Cardinal; overload;
  function Utf8ToUnicode(Dest: PWideChar; Source: PAnsiChar; MaxChars: Integer): Integer; overload;
  function Utf8ToUnicode(Dest: PWideChar; MaxDestChars: Cardinal; Source: PAnsiChar; SourceBytes: Cardinal): Cardinal; overload;
  function Utf8Encode(const WS: WideString): {$IFDEF VER12P}RawByteString{$ELSE}UTF8String{$ENDIF};
  function Utf8Decode(const S: {$IFDEF VER12P}RawByteString{$ELSE}UTF8String{$ENDIF}): WideString;
  function AnsiToUtf8(const S: AnsiString): {$IFDEF VER12P}RawByteString{$ELSE}UTF8String{$ENDIF};
  function Utf8ToAnsi(const S: {$IFDEF VER12P}RawByteString{$ELSE}UTF8String{$ENDIF}): AnsiString;
{$ELSE}
  function Utf8Encode(const WS: WideString): UTF8String;
  function Utf8Decode(const S: UTF8String): WideString;
  function AnsiToUtf8(const S: AnsiString): UTF8String;
  function Utf8ToAnsi(const S: UTF8String): AnsiString;
{$ENDIF}{$ENDIF}

{ POSIX support }

{$IFDEF POSIX}
  function GetTickCount: Cardinal;

  function InterlockedIncrement(var I: Integer): Integer;
  function InterlockedDecrement(var I: Integer): Integer;

  function WideCharToMultiByte(CodePage: Cardinal; dwFlags: Cardinal;
    lpWideCharStr: PWideChar; cchWideChar: Integer; lpMultiByteStr: PAnsiChar;
    cchMultiByte: Integer; lpDefaultChar: PAnsiChar; lpUsedDefaultChar: PLongBool): Integer;
  function MultiByteToWideChar(CodePage: Cardinal; dwFlags: Cardinal;
    const lpMultiByteStr: PAnsiChar; cchMultiByte: Integer;
    lpWideCharStr: PWideChar; cchWideChar: Integer): Integer;
{$ENDIF}

{ FPC support}

{$IFDEF FPC}

{$IFDEF UNIX}
function GetTickCount: Cardinal;
{$ENDIF}

{$IFDEF CPUX64}
function VarArrayCreate(const Bounds: array of Integer; aVarType: TVarType): Variant;
{$ENDIF}

{$ENDIF}

function SwapLongWord(const Value: LongWord): LongWord;

implementation

uses
{$IFDEF VER6}
  SysConst,
{$ENDIF}
{$IFDEF MSWINDOWS}
  Windows,
{$ENDIF}
{$IFDEF POSIX}
  System.Diagnostics,
{$ENDIF}
{$IFDEF UNIX}
  unix,
{$ENDIF}
  SysUtils;

{$IFDEF VER6}
procedure ConvertErrorFmt(ResString: PResStringRec; const Args: array of const); local;
begin
  raise EConvertError.CreateResFmt(ResString, Args);
end;

function StrToBool(const S: string): Boolean;
begin
  if not TryStrToBool(S, Result) then
    ConvertErrorFmt(@SInvalidBoolean, [S]);
end;

procedure VerifyBoolStrArray;
begin
  if Length(TrueBoolStrs) = 0 then
  begin
    SetLength(TrueBoolStrs, 1);
    TrueBoolStrs[0] := DefaultTrueBoolStr;
  end;
  if Length(FalseBoolStrs) = 0 then
  begin
    SetLength(FalseBoolStrs, 1);
    FalseBoolStrs[0] := DefaultFalseBoolStr;
  end;
end;

function TryStrToBool(const S: string; out Value: Boolean): Boolean;

  function CompareWith(const aArray: array of string): Boolean;
  var
    I: Integer;
  begin
    Result := False;
    for I := Low(aArray) to High(aArray) do
      if AnsiSameText(S, aArray[I]) then
      begin
        Result := True;
        Break;
      end;
  end;

var
  LResult: Extended;
begin
  Result := TryStrToFloat(S, LResult);
  if Result then
    Value := LResult <> 0
  else
  begin
    VerifyBoolStrArray;
    Result := CompareWith(TrueBoolStrs);
    if Result then
      Value := True
    else
    begin
      Result := CompareWith(FalseBoolStrs);
      if Result then
        Value := False;
    end;
  end;
end;
{$ENDIF}

{ Delphi 5 support }

{$IFNDEF VER6P}

function TryStrToInt(const S: string; out Value: Integer): Boolean;
var
  E: Integer;
begin
  Val(S, Value, E);
  Result := E = 0;
end;

function BoolToStr(const Value: boolean; UseBoolStrs: Boolean = False): string;
const
  cSimpleBoolStrs: array [boolean] of String = ('0', '-1');
begin
  if UseBoolStrs then
  begin
    if Value then
      Result := 'True'
    else
      Result := 'False';
  end
  else
    Result := cSimpleBoolStrs[Value];
end;

function TryStrToBool(const S: string; out Value: Boolean): Boolean;
begin
  Result := True;
  if SameText(s, 'True') or SameText(s, 'Yes') or SameText(s, '1') then
    Value := True
  else
  if SameText(s, 'False') or SameText(s, 'No') or SameText(s, '0') then
    Value := False
  else
    Result := False;
end;

function StrToBool(const S: string): Boolean;
begin
  if not TryStrToBool(S, Result) then
    raise EConvertError.Create('InvalidBoolean - ' + S);
end;

type
  PWordBool = ^WordBool;

procedure DecodeDateTime(const AValue: TDateTime; out AYear, AMonth, ADay,
  AHour, AMinute, ASecond, AMilliSecond: Word);
begin
  DecodeDate(AValue, AYear, AMonth, ADay);
  DecodeTime(AValue, AHour, AMinute, ASecond, AMilliSecond);
end;

function WideUpperCase(const S: WideString): WideString;
var
  Len: Integer;
begin
  Len := Length(S);
  SetString(Result, PWideChar(S), Len);
  if Len > 0 then CharUpperBuffW(Pointer(Result), Len);
end;

function VarToWideStr(const V: Variant): WideString;
begin
  if not VarIsNull(V) then
    Result := V
  else
    Result := '';;
end;

function VarIsStr(const V: Variant): Boolean;
begin
  case VarType(V) of
    varString, varOleStr:
      Result := True;
  else
    Result := False;
  end;
end;

function AnsiDequotedStr(const S: string; AQuote: Char): string;
var
  LText: PChar;
begin
  LText := PChar(S);
  Result := AnsiExtractQuotedStr(LText, AQuote);
  if Result = '' then
    Result := S;
end;

function AnsiContainsText(const AText, ASubText: string): Boolean;
begin
  Result := AnsiPos(AnsiUppercase(ASubText), AnsiUppercase(AText)) > 0;
end;
{$ENDIF}

{ Unicode build support }

function _LowerCase(const S: _string): _string;
begin
{$IFDEF CRUNICODE}
  Result := WideLowerCase(S);
{$ELSE}
  Result := AnsiLowerCase(S);
{$ENDIF}
end;

function _UpperCase(const S: _string): _string;
begin
{$IFDEF CRUNICODE}
  Result := WideUpperCase(S);
{$ELSE}
  Result := AnsiUpperCase(S);
{$ENDIF}
end;

function _CompareText(const S1, S2: _string): Integer;
begin
{$IFDEF CRUNICODE}
  Result := WideCompareText(S1, S2);
{$ELSE}
  Result := AnsiCompareText(S1, S2);
{$ENDIF}
end;

function _SameText(const S1, S2: _string): Boolean;
begin
{$IFDEF CRUNICODE}
  Result := WideSameText(S1, S2);
{$ELSE}
  Result := AnsiSameText(S1, S2);
{$ENDIF}
end;

function _QuotedStr(const S: _string; AQuote: _char): _string;
{$IFDEF CRUNICODE}
var
  I: Integer;
begin
  Result := S;
  for I := Length(Result) downto 1 do
    if Result[I] = AQuote then Insert(AQuote, Result, I);
  Result := AQuote + Result + AQuote;
end;
{$ELSE}
begin
  Result := AnsiQuotedStr(S, AQuote);
end;
{$ENDIF}

function _DequotedStr(const S: _string; AQuote: _char): _string;
{$IFDEF CRUNICODE}
var
  i, len: Integer;
begin
  len := Length(S);
  if (len >= 2) and (S[1] = AQuote) and (S[len] = AQuote) then begin
    Result := Copy(S, 2, len - 2);
    len := len - 2;
    i := len;
    while i >= 2 do begin
      if (Result[i] = AQuote) and (Result[i-1] = AQuote) then begin
        Delete(Result, i, 1);
        Dec(i, 2);
      end
      else
        Dec(i);
    end;
  end
  else
    Result := S;
end;
{$ELSE}
begin
  Result := AnsiDequotedStr(S, AQuote);
end;
{$ENDIF}

function _VarToStr(const V: Variant): _string;
begin
{$IFDEF CRUNICODE}
  Result := VarToWideStr(V);
{$ELSE}
  Result := VarToStr(V);
{$ENDIF}
end;

function _Format(const AFormat: _string; const Args: array of const): _string;
begin
{$IFDEF CRUNICODE}
  Result := WideFormat(AFormat, Args);
{$ELSE}
  Result := Format(AFormat, Args);
{$ENDIF}
end;

procedure AssignStrings(Source: _TStrings; Dest: TStrings);
{$IFDEF CRUNICODE}
var
  i: Integer;
{$ENDIF}
begin
{$IFDEF CRUNICODE}
  Dest.BeginUpdate;
  try
    Dest.Clear;
    for i := 0 to Source.Count - 1 do
      Dest.Add(Source[i]);
  finally
    Dest.EndUpdate;
  end;
{$ELSE}
  Dest.Assign(Source);
{$ENDIF}
end;

{$IFDEF CRUNICODE}
procedure AssignStrings(Source: TStrings; Dest: _TStrings);
var
  i: Integer;
begin
  Dest.BeginUpdate;
  try
    Dest.Clear;
    for i := 0 to Source.Count - 1 do
      Dest.Add(Source[i]);
  finally
    Dest.EndUpdate;
  end;
end;
{$ENDIF}

{ UTF8Encoding }

{$IFDEF UTF8}

// UnicodeToUTF8(3):
// Scans the source data to find the null terminator, up to MaxBytes
// Dest must have MaxBytes available in Dest.

function UnicodeToUtf8(Dest: PAnsiChar; Source: PWideChar; MaxBytes: Integer): Integer;
var
  len: Cardinal;
begin
  len := 0;
  if Source <> nil then
    while Source[len] <> #0 do
      Inc(len);
  Result := CRFunctions.UnicodeToUtf8(Dest, MaxBytes, Source, len);
end;

// UnicodeToUtf8(4):
// MaxDestBytes includes the null terminator (last char in the buffer will be set to null)
// Function result includes the null terminator.
// Nulls in the source data are not considered terminators - SourceChars must be accurate

function UnicodeToUtf8(Dest: PAnsiChar; MaxDestBytes: Cardinal; Source: PWideChar; SourceChars: Cardinal): Cardinal;
var
  i, count: Cardinal;
  c: Word;
begin
  Result := 0;
  if Source = nil then Exit;
  count := 0;
  i := 0;
  if Dest <> nil then
  begin
    while (i < SourceChars) and (count < MaxDestBytes) do
    begin
      c := Word(Source[i]);
      Inc(i);
      if c <= $7F then
      begin
        Dest[count] := AnsiChar(c);
        Inc(count);
      end
      else if c > $7FF then
      begin
        if count + 3 > MaxDestBytes then
          break;
        Dest[count] := AnsiChar($E0 or (c shr 12));
        Dest[count+1] := AnsiChar($80 or ((c shr 6) and $3F));
        Dest[count+2] := AnsiChar($80 or (c and $3F));
        Inc(count,3);
      end
      else //  $7F < Source[i] <= $7FF
      begin
        if count + 2 > MaxDestBytes then
          break;
        Dest[count] := AnsiChar($C0 or (c shr 6));
        Dest[count+1] := AnsiChar($80 or (c and $3F));
        Inc(count,2);
      end;
    end;
    if count >= MaxDestBytes then count := MaxDestBytes-1;
    Dest[count] := #0;
  end
  else
  begin
    while i < SourceChars do
    begin
      c := Word(Source[i]);
      Inc(i);
      if c > $7F then
      begin
        if c > $7FF then
          Inc(count);
        Inc(count);
      end;
      Inc(count);
    end;
  end;
  Result := count+1;  // convert zero based index to byte count
end;

function Utf8ToUnicode(Dest: PWideChar; Source: PAnsiChar; MaxChars: Integer): Integer;
var
  len: Cardinal;
begin
  len := 0;
  if Source <> nil then
    while Source[len] <> #0 do
      Inc(len);
  Result := CRFunctions.Utf8ToUnicode(Dest, MaxChars, Source, len);
end;

function Utf8ToUnicode(Dest: PWideChar; MaxDestChars: Cardinal; Source: PAnsiChar; SourceBytes: Cardinal): Cardinal;
var
  i, count: Cardinal;
  c: Byte;
  wc: Word;
begin
  if Source = nil then
  begin
    Result := 0;
    Exit;
  end;
  Result := Cardinal(-1);
  count := 0;
  i := 0;
  if Dest <> nil then
  begin
    while (i < SourceBytes) and (count < MaxDestChars) do
    begin
      wc := Word(Source[i]);
      Inc(i);
      if (wc and $80) <> 0 then
      begin
        if i >= SourceBytes then Exit;          // incomplete multibyte char
        wc := wc and $3F;
        if (wc and $20) <> 0 then
        begin
          c := Byte(Source[i]);
          Inc(i);
          if (c and $C0) <> $80 then Exit;      // malformed trail byte or out of range char
          if i >= SourceBytes then Exit;        // incomplete multibyte char
          wc := (wc shl 6) or (c and $3F);
        end;
        c := Byte(Source[i]);
        Inc(i);
        if (c and $C0) <> $80 then Exit;       // malformed trail byte

        Dest[count] := WideChar((wc shl 6) or (c and $3F));
      end
      else
        Dest[count] := WideChar(wc);
      Inc(count);
    end;
    if count >= MaxDestChars then count := MaxDestChars - 1;
    Dest[count] := #0;
  end
  else
  begin
    while (i < SourceBytes) do
    begin
      c := Byte(Source[i]);
      Inc(i);
      if (c and $80) <> 0 then
      begin
        if i >= SourceBytes then Exit;          // incomplete multibyte char
        c := c and $3F;
        if (c and $20) <> 0 then
        begin
          c := Byte(Source[i]);
          Inc(i);
          if (c and $C0) <> $80 then Exit;      // malformed trail byte or out of range char
          if i >= SourceBytes then Exit;        // incomplete multibyte char
        end;
        c := Byte(Source[i]);
        Inc(i);
        if (c and $C0) <> $80 then Exit;       // malformed trail byte
      end;
      Inc(count);
    end;
  end;
  Result := count + 1;
end;

function Utf8Encode(const WS: WideString): UTF8String;
var
  L: Integer;
  Temp: UTF8String;
begin
  Result := '';
  if WS = '' then Exit;
  SetLength(Temp, Length(WS) * 3); // SetLength includes space for null terminator

  L := CRFunctions.UnicodeToUtf8(PAnsiChar(Temp), Length(Temp)+1, PWideChar(WS), Length(WS));
  if L > 0 then
    SetLength(Temp, L-1)
  else
    Temp := '';
  Result := Temp;
end;

function Utf8Decode(const S: UTF8String): WideString;
var
  L: Integer;
  Temp: WideString;
begin
  Result := '';
  if S = '' then Exit;
  SetLength(Temp, Length(S));

  L := CRFunctions.Utf8ToUnicode(PWideChar(Temp), Length(Temp)+1, PAnsiChar(S), Length(S));
  if L > 0 then
    SetLength(Temp, L-1)
  else
    Temp := '';
  Result := Temp;
end;

function AnsiToUtf8(const S: AnsiString): UTF8String;
begin
  Result := Utf8Encode(S);
end;

function Utf8ToAnsi(const S: UTF8String): AnsiString;
begin
  Result := Utf8Decode(S);
end;

{$ELSE}{$IFNDEF CLR}

function UnicodeToUtf8(Dest: PAnsiChar; Source: PWideChar; MaxBytes: Integer): Integer;
begin
  Result := CRFunctions.UnicodeToUtf8(Dest, MaxBytes, Source, Cardinal(-1));
end;

function UnicodeToUtf8(Dest: PAnsiChar; MaxDestBytes: Cardinal; Source: PWideChar; SourceChars: Cardinal): Cardinal;
begin
  Result := 0;
  if Source = nil then Exit;
  if Dest <> nil then
  begin
    Result := Cardinal(WideCharToMultiByte(CP_UTF8, 0, Source, Integer(SourceChars), Dest, Integer(MaxDestBytes), nil, nil));
    if (Result > 0) and (Result <= MaxDestBytes) and (Dest[Result - 1] <> #0) then
    begin
      if Result = MaxDestBytes then
      begin
        while (Result > 1) and (Byte(Dest[Result - 1]) > $7F) and (Byte(Dest[Result - 1]) and $80 <> 0) and (Byte(Dest[Result - 1]) and $C0 <> $C0) do
          Dec(Result);
      end else
        Inc(Result);
      Dest[Result - 1] := #0;
    end;
  end else
    Result := Cardinal(WideCharToMultiByte(CP_UTF8, 0, Source, Integer(SourceChars), nil, 0, nil, nil));
end;

function Utf8ToUnicode(Dest: PWideChar; Source: PAnsiChar; MaxChars: Integer): Integer;
begin
  Result := CRFunctions.Utf8ToUnicode(Dest, MaxChars, Source, Cardinal(-1));
end;

function Utf8ToUnicode(Dest: PWideChar; MaxDestChars: Cardinal; Source: PAnsiChar; SourceBytes: Cardinal): Cardinal;
begin
  Result := 0;
  if Source = nil then Exit;
  if (Dest <> nil) and (MaxDestChars > 0) then
  begin
    Result := Cardinal(MultibyteToWideChar(CP_UTF8, 0, Source, Integer(SourceBytes), Dest, Integer(MaxDestChars)));
    if (Result > 0) and (Result <= MaxDestChars) and (Dest[Result - 1] <> #0) then
    begin
      if Result = MaxDestChars then
      begin
        if (Result > 1) and (Word(Dest[Result - 1]) >= $DC00) and (Word(Dest[Result - 1]) <= $DFFF) then
          Dec(Result);
      end else
        Inc(Result);
      Dest[Result - 1] := #0;
    end;
  end else
    Result := Cardinal(MultibyteToWideChar(CP_UTF8, 0, Source, Integer(SourceBytes), nil, 0));
end;

function Utf8Encode(const WS: WideString):{$IFDEF VER12P}RawByteString{$ELSE}UTF8String{$ENDIF};
var
  L: Integer;
  Temp: {$IFDEF VER12P}RawByteString{$ELSE}UTF8String{$ENDIF};
begin
  Result := '';
  if WS = '' then Exit;
  SetLength(Temp, Length(WS) * 3); // SetLength includes space for null terminator

  L := CRFunctions.UnicodeToUtf8(PAnsiChar(Temp), Length(Temp)+1, PWideChar(WS), Length(WS));
  if L > 0 then
    SetLength(Temp, L-1)
  else
    Temp := '';
  Result := Temp;
end;

function Utf8Decode(const S: {$IFDEF VER12P}RawByteString{$ELSE}UTF8String{$ENDIF}): WideString;
var
  L: Integer;
  Temp: WideString;
begin
  Result := '';
  if S = '' then Exit;
  SetLength(Temp, Length(S));

  L := CRFunctions.Utf8ToUnicode(PWideChar(Temp), Length(Temp)+1, PAnsiChar(S), Length(S));
  if L > 0 then
    SetLength(Temp, L-1)
  else
    Temp := '';
  Result := Temp;
end;

function AnsiToUtf8(const S: AnsiString): {$IFDEF VER12P}RawByteString{$ELSE}UTF8String{$ENDIF};
begin
  Result := CRFunctions.Utf8Encode(WideString(S));
end;

function Utf8ToAnsi(const S: {$IFDEF VER12P}RawByteString{$ELSE}UTF8String{$ENDIF}): AnsiString;
begin
  Result := AnsiString(CRFunctions.Utf8Decode(S));
end;

{$ELSE}

function Utf8Encode(const WS: WideString): UTF8String;
begin
  Result := Borland.Delphi.System.Utf8Encode(WS);
end;

function Utf8Decode(const S: UTF8String): WideString;
begin
  Result := Borland.Delphi.System.Utf8Decode(S);
end;

function AnsiToUtf8(const S: AnsiString): UTF8String;
begin
  Result := Borland.Delphi.System.AnsiToUtf8(S);
end;

function Utf8ToAnsi(const S: UTF8String): AnsiString;
begin
  Result := Borland.Delphi.System.Utf8ToAnsi(S);
end;

{$ENDIF}{$ENDIF}

{ POSIX support }

{$IFDEF POSIX}

function GetTickCount: Cardinal;
{$IFDEF MACOS}
begin
  Result := TStopwatch.GetTimeStamp div 10000;
end;
{$ENDIF}
{$IFDEF LINUX}
var
  t: tms;
begin
  Result := Cardinal(Int64(Cardinal(times(t)) * 1000) div sysconf(_SC_CLK_TCK));
end;
{$ENDIF}

function InterlockedIncrement(var I: Integer): Integer;
asm
{$IFNDEF CPUX64}
      MOV   EDX,1
      XCHG  EAX,EDX
 LOCK XADD  [EDX],EAX
      INC   EAX
{$ELSE}
      MOV   EAX,1
 LOCK XADD  dword ptr [RCX],EAX
      INC   EAX
{$ENDIF}
end;

function InterlockedDecrement(var I: Integer): Integer;
asm
{$IFNDEF CPUX64}
      MOV   EDX,-1
      XCHG  EAX,EDX
 LOCK XADD  [EDX],EAX
      DEC   EAX
{$ELSE}
      MOV   EAX,-1
 LOCK XADD  dword ptr [RCX],EAX
      DEC   EAX
{$ENDIF}
end;

function WideCharToMultiByte(CodePage: Cardinal; dwFlags: Cardinal;
  lpWideCharStr: PWideChar; cchWideChar: Integer; lpMultiByteStr: PAnsiChar;
  cchMultiByte: Integer; lpDefaultChar: PAnsiChar; lpUsedDefaultChar: PLongBool): Integer;
begin
  Result := LocaleCharsFromUnicode(CodePage, dwFlags, lpWideCharStr, cchWideChar, lpMultiByteStr, cchMultiByte, lpDefaultChar, lpUsedDefaultChar);
end;

function MultiByteToWideChar(CodePage: Cardinal; dwFlags: Cardinal;
  const lpMultiByteStr: PAnsiChar; cchMultiByte: Integer;
  lpWideCharStr: PWideChar; cchWideChar: Integer): Integer;
begin
  Result := UnicodeFromLocaleChars(CodePage, dwFlags, lpMultiByteStr, Integer(cchMultiByte), lpWideCharStr, cchWideChar);
end;
{$ENDIF}

{ FPC support}

{$IFDEF FPC}

{$IFDEF UNIX}
function GetTickCount: Cardinal;
var
  tv: timeval;
begin
  fpgettimeofday(@tv, nil);
  {$RANGECHECKS OFF}
  Result := int64(tv.tv_sec) * 1000 + tv.tv_usec div 1000;
end;
{$ENDIF}

{$IFDEF CPUX64}
function VarArrayCreate(const Bounds: array of Integer; aVarType: TVarType): Variant;
var
  i: integer;
  Bounds64: array of Int64;
begin
  SetLength(Bounds64, Length(Bounds));
  for i := 0 to Length(Bounds) - 1 do
    Bounds64[i] := Bounds[i];
  Result :=  Variants.VarArrayCreate(Bounds64, aVarType);
end;
{$ENDIF}

{$ENDIF}

function SwapLongWord(const Value: LongWord): LongWord;
begin
  Result:= ((Value and $FF) shl 24) or ((Value and $FF00) shl 8) or ((Value and $FF0000) shr 8) or ((Value and $FF000000) shr 24);
end;

end.
