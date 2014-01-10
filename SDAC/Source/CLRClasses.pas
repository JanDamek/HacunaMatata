
//////////////////////////////////////////////////
//  DB Access Components
//  Copyright © 1998-2012 Devart. All right reserved.
//  CLRClasses
//////////////////////////////////////////////////

unit CLRClasses;

interface

{$I Dac.inc}

uses
  Classes, SysUtils, CRTypes;

type
  Int    = Integer;
  Int16  = SmallInt;
  Int32  = Integer;
  UInt16 = Word;
  UInt32 = LongWord;
  SByte  = ShortInt;

  IntPtr = Pointer;
  MulticastDelegate = Pointer;

  BitConverter = class
  public
    class function GetBytes(value: Word): TBytes; overload; {$IFDEF USE_INLINE}inline;{$ENDIF}
    class function GetBytes(value: Cardinal): TBytes; overload; {$IFDEF USE_INLINE}inline;{$ENDIF}
    class function GetBytes(value: Int64): TBytes; overload; {$IFDEF USE_INLINE}inline;{$ENDIF}
    class function GetBytes(value: Double): TBytes; overload; {$IFDEF USE_INLINE}inline;{$ENDIF}
    class function GetBytes(value: Single): TBytes; overload; {$IFDEF USE_INLINE}inline;{$ENDIF}
    class function Int64BitsToDouble(value: Int64): double; {$IFDEF USE_INLINE}inline;{$ENDIF}
    class function DoubleToInt64Bits(value: double): Int64; {$IFDEF USE_INLINE}inline;{$ENDIF}
    class function ToDouble(const value: TBytes; startIndex: Integer): Double; overload; {$IFDEF USE_INLINE}inline;{$ENDIF}
    class function ToDouble(const value: PAnsiChar; startIndex: Integer): Double; overload; {$IFDEF USE_INLINE}inline;{$ENDIF}
    class function ToSingle(const value: TBytes; startIndex: Integer): Single; {$IFDEF USE_INLINE}inline;{$ENDIF}
    class function ToInt16(const value: TBytes; startIndex: Integer): Int16; {$IFDEF USE_INLINE}inline;{$ENDIF}
    class function ToUInt16(const value: TBytes; startIndex: Integer): UInt16; overload; {$IFDEF USE_INLINE}inline;{$ENDIF}
    class function ToUInt16(const value: PAnsiChar; startIndex: Integer): UInt16; overload; {$IFDEF USE_INLINE}inline;{$ENDIF}
    class function ToInt32(const value: TBytes; startIndex: Integer): Int32; overload; {$IFDEF USE_INLINE}inline;{$ENDIF}
    class function ToInt32(const value: PAnsiChar; startIndex: Integer): Int32; overload; {$IFDEF USE_INLINE}inline;{$ENDIF}
    class function ToUInt32(const value: TBytes; startIndex: Integer): UInt32; {$IFDEF USE_INLINE}inline;{$ENDIF}
    class function ToInt64(const value: TBytes; startIndex: Integer): Int64; overload; {$IFDEF USE_INLINE}inline;{$ENDIF}
    class function ToInt64(const value: PAnsiChar; startIndex: Integer): Int64; overload; {$IFDEF USE_INLINE}inline;{$ENDIF}
  end;

  Marshal = class
  public
    class function AllocHGlobal(cb: NativeInt): Pointer;
    class function ReallocHGlobal(pv: Pointer; cb: NativeInt): Pointer;
    class procedure FreeHGlobal(hglobal: Pointer);
    class procedure FreeCoTaskMem(ptr: Pointer);

    class function ReadByte(ptr: Pointer; ofs: Integer = 0): Byte; {$IFDEF USE_INLINE}inline;{$ENDIF}
    class procedure WriteByte(ptr: Pointer; val: Byte); overload; {$IFDEF USE_INLINE}inline;{$ENDIF}
    class procedure WriteByte(ptr: Pointer; ofs: Integer; val: Byte); overload; {$IFDEF USE_INLINE}inline;{$ENDIF}

    class function ReadInt16(ptr: Pointer; ofs: Integer = 0): Int16; {$IFDEF USE_INLINE}inline;{$ENDIF}
    class procedure WriteInt16(ptr: Pointer; val: Int16); overload; {$IFDEF USE_INLINE}inline;{$ENDIF}
    class procedure WriteInt16(ptr: Pointer; ofs: Integer; val: Int16); overload; {$IFDEF USE_INLINE}inline;{$ENDIF}

    class function ReadInt32(ptr: Pointer; ofs: Integer = 0): Int32; {$IFDEF USE_INLINE}inline;{$ENDIF}
    class procedure WriteInt32(ptr: Pointer; val: Int32); overload; {$IFDEF USE_INLINE}inline;{$ENDIF}
    class procedure WriteInt32(ptr: Pointer; ofs: Integer; val: Int32); overload; {$IFDEF USE_INLINE}inline;{$ENDIF}

    class function ReadInt64(ptr: Pointer; ofs: Integer = 0): Int64; {$IFDEF USE_INLINE}inline;{$ENDIF}
    class procedure WriteInt64(ptr: Pointer; val: Int64); overload; {$IFDEF USE_INLINE}inline;{$ENDIF}
    class procedure WriteInt64(ptr: Pointer; val: Double); overload; {$IFDEF USE_INLINE}inline;{$ENDIF}
    class procedure WriteInt64(ptr: Pointer; ofs: Integer; val: Int64); overload; {$IFDEF USE_INLINE}inline;{$ENDIF}

    class function ReadIntPtr(ptr: Pointer; ofs: Integer = 0): Pointer; {$IFDEF USE_INLINE}inline;{$ENDIF}
    class procedure WriteIntPtr(ptr: Pointer; val: Pointer); overload; {$IFDEF USE_INLINE}inline;{$ENDIF}
    class procedure WriteIntPtr(ptr: Pointer; ofs: Integer; val: Pointer); overload; {$IFDEF USE_INLINE}inline;{$ENDIF}

    class function PtrToStringAnsi(ptr: Pointer; len: Integer = 0): AnsiString; {$IFDEF USE_INLINE}inline;{$ENDIF}
    class function PtrToStringUni(ptr: Pointer; len: Integer = 0): WideString; {$IFDEF USE_INLINE}inline;{$ENDIF}
    class function StringToHGlobalAnsi(const s: AnsiString): Pointer; {$IFDEF USE_INLINE}inline;{$ENDIF}
    class function StringToHGlobalUni(const s: WideString): Pointer; {$IFDEF USE_INLINE}inline;{$ENDIF}

    class procedure Copy(const source: TBytes; startIndex: Integer; destination: Pointer; length: Integer); overload;
    class procedure Copy(source: Pointer; const destination: TBytes; startIndex: Integer; length: Integer); overload;

    class function GetIUnknownForObject(o: TInterfacedObject): Pointer;
    class function AddRef(pUnk: Pointer): Integer;
    class function Release(pUnk: Pointer): Integer;
  end;

  Encoding = class
  public
    class function Default: Encoding;
    class function Unicode: Encoding;
    class function UTF8: Encoding;
    class function GetEncoding(codepage: Cardinal): Encoding;
    class function Convert(srcEncoding, dstEncoding: Encoding; bytes: TBytes): TBytes; overload;
    class function Convert(srcEncoding, dstEncoding: Encoding; bytes: TBytes; index, count: Integer): TBytes; overload;

    function GetBytes(const chars: AnsiString): TBytes; overload; virtual;
    function GetBytes(const chars: AnsiString; charIndex, charCount: Integer; var bytes: TBytes; byteIndex: Integer): Integer; overload; virtual;
    function {$IFNDEF VER5}GetBytes{$ELSE}GetBytesWide{$ENDIF}(const chars: WideString): TBytes; overload; virtual;
    function {$IFNDEF VER5}GetBytes{$ELSE}GetBytesWide{$ENDIF}(const chars: WideString; charIndex, charCount: Integer; var bytes: TBytes; byteIndex: Integer): Integer; overload; virtual;

    function GetString(const bytes: TBytes): AnsiString; overload; virtual;
    function GetString(const bytes: TBytes; index: Integer; count: Integer): AnsiString; overload; virtual;
    function GetWideString(const bytes: TBytes): WideString; overload; virtual;
    function GetWideString(const bytes: TBytes; index: Integer; count: Integer): WideString; overload; virtual;

    function GetMaxByteCount(charCount: Integer): Integer; virtual;
  end;

  UnicodeEncoding = class(Encoding)
  public
    function GetBytes(const chars: AnsiString): TBytes; overload; override;
    function GetBytes(const chars: AnsiString; charIndex, charCount: Integer; var bytes: TBytes; byteIndex: Integer): Integer; overload; override;
    function {$IFNDEF VER5}GetBytes{$ELSE}GetBytesWide{$ENDIF}(const chars: WideString): TBytes; overload; override;
    function {$IFNDEF VER5}GetBytes{$ELSE}GetBytesWide{$ENDIF}(const chars: WideString; charIndex, charCount: Integer; var bytes: TBytes; byteIndex: Integer): Integer; overload; override;

    function GetString(const bytes: TBytes): AnsiString; overload; override;
    function GetString(const bytes: TBytes; index: Integer; count: Integer): AnsiString; overload; override;
    function GetWideString(const bytes: TBytes): WideString; overload; override;
    function GetWideString(const bytes: TBytes; index: Integer; count: Integer): WideString; overload; override;

    function GetMaxByteCount(charCount: Integer): Integer; override;
  end;

  UTF8Encoding = class(Encoding)
  public
    function GetBytes(const chars: AnsiString): TBytes; overload; override;
    function GetBytes(const chars: AnsiString; charIndex, charCount: Integer; var bytes: TBytes; byteIndex: Integer): Integer; overload; override;
    function {$IFNDEF VER5}GetBytes{$ELSE}GetBytesWide{$ENDIF}(const chars: WideString): TBytes; overload; override;
    function {$IFNDEF VER5}GetBytes{$ELSE}GetBytesWide{$ENDIF}(const chars: WideString; charIndex, charCount: Integer; var bytes: TBytes; byteIndex: Integer): Integer; overload; override;

    function GetString(const bytes: TBytes): AnsiString; overload; override;
    function GetString(const bytes: TBytes; index: Integer; count: Integer): AnsiString; overload; override;
    function GetWideString(const bytes: TBytes): WideString; overload; override;
    function GetWideString(const bytes: TBytes; index: Integer; count: Integer): WideString; overload; override;

    function GetMaxByteCount(charCount: Integer): Integer; override;
  end;

{$IFDEF WIN32_64}
  CodePageEncoding = class(Encoding)
  private
    FCodePage: Cardinal;

  public
    constructor Create(CodePage: Cardinal);

    function GetBytes(const chars: AnsiString): TBytes; override;
    function GetBytes(const chars: AnsiString; charIndex, charCount: Integer; var bytes: TBytes; byteIndex: Integer): Integer; overload; override;
    function {$IFNDEF VER5}GetBytes{$ELSE}GetBytesWide{$ENDIF}(const chars: WideString): TBytes; overload; override;
    function {$IFNDEF VER5}GetBytes{$ELSE}GetBytesWide{$ENDIF}(const chars: WideString; charIndex, charCount: Integer; var bytes: TBytes; byteIndex: Integer): Integer; overload; override;

    function GetString(const bytes: TBytes): AnsiString; overload; override;
    function GetString(const bytes: TBytes; index: Integer; count: Integer): AnsiString; overload; override;
    function GetWideString(const bytes: TBytes): WideString; overload; override;
    function GetWideString(const bytes: TBytes; index: Integer; count: Integer): WideString; overload; override;

    function GetMaxByteCount(charCount: Integer): Integer; override;
  end;
{$ENDIF}

  TAnsiCharArray = array of AnsiChar;

  AnsiStringBuilder = class
  protected
    FString: TAnsiCharArray;
    FActualLength: Integer;

    procedure SetActualLength(Value: Integer);
  public
    constructor Create(capacity: Integer); overload;
    constructor Create(const value: AnsiString; capacity: Integer); overload;
    procedure Append(const value: AnsiString); overload;
    procedure Append(const value: AnsiString; const startIndex: Integer; const count: Integer); overload;
    procedure Append(value: AnsiChar); overload;
    procedure Append(value: AnsiChar; repeatCount: Integer); overload;
    procedure Append(value: AnsiStringBuilder); overload;
    procedure Insert(index: Integer; const value: AnsiString); overload;
    procedure Replace(const OldValue: AnsiString; const NewValue: AnsiString);
    function ToString: AnsiString; reintroduce;

    property Length: Integer read FActualLength write SetActualLength;
  end;

  TWideCharArray = array of WideChar;

{$IFDEF VER12P}
  WString = string;
{$ELSE}
  WString = WideString;
{$ENDIF}

  WideStringBuilder = class
  protected
    FString: TWideCharArray;
    FActualLength: Integer;

    procedure SetActualLength(Value: Integer);
  public
    constructor Create(capacity: Integer); overload;
    constructor Create(const value: WString; capacity: Integer); overload;
    procedure Append(const value: WString); overload;
    procedure Append(const value: WString; const startIndex: Integer; const count: Integer); overload;
    procedure Append(value: WideChar); overload;
    procedure Append(value: WideChar; repeatCount: Integer); overload;
    procedure Append(value: WideStringBuilder); overload;
    procedure Insert(index: Integer; const value: WString); overload;
    procedure Replace(const OldValue: WString; const NewValue: WString);
    function ToString: WString; {$IFDEF VER12P} override; {$ENDIF}

    property Length: Integer read FActualLength write SetActualLength;
  end;

{$IFDEF VER12P}
  StringBuilder = WideStringBuilder;
{$ELSE}
  StringBuilder = AnsiStringBuilder;
{$ENDIF}

{$IFDEF CRUNICODE}
  _StringBuilder = WideStringBuilder;
{$ELSE}
  _StringBuilder = StringBuilder;
{$ENDIF}

  Buffer = class
  public
    class procedure BlockCopy(const src: TBytes; srcOffset: Integer; const dst: TBytes; dstOffset: Integer; count: Integer);
    class function GetByte(const src: Pointer; Index: Integer): Byte;
    class procedure SetByte(const src: Pointer; Index: Integer; Value: Byte);
  end;

{ MemoryStream }

  MemoryStream = class
  private
    FData: TBytes;
    FPosition: Integer;
    FLength: Integer;

  protected
    procedure SetPosition(const Pos: Integer);

  public
    constructor Create(Capacity: Integer);
    function Seek(Offset: Integer; Origin: Word): Integer;
    function Read(var Buffer: TBytes; Offset: Integer; Count: Integer): Integer; overload;
    function Read(Buffer: PAnsiChar; Offset: Integer; Count: Integer): Integer; overload;
    procedure Write(const Buffer: TBytes; Offset: Integer; Count: Integer); overload;
    procedure Write(Buffer: PAnsiChar; Offset: Integer; Count: Integer); overload;
    procedure WriteByte(Value: Byte);
    function ReadByte: Byte;
    function GetBuffer: PAnsiChar;

    procedure Close;
    procedure SetLength(Value: Integer);

    property Length: Integer read FLength write SetLength;
    property Position: Integer read FPosition write SetPosition;
  end;

  ArgumentException = class(Exception)
  public
    constructor Create; overload;
    constructor Create(const Msg: string); overload;
  end;

  NotSupportedException = class(Exception)
  public
    constructor Create; overload;
    constructor Create(const Msg: string); overload;
  end;

implementation

uses
  Math,
{$IFDEF WIN32_64}
  Windows,
{$ENDIF}
  CRFunctions;

{ BitConverter }

class function BitConverter.GetBytes(value: Word): TBytes;
begin
  SetLength(Result, SizeOf(Word));
  Word(Pointer(Result)^) := value;
end;

class function BitConverter.GetBytes(value: Cardinal): TBytes;
begin
  SetLength(Result, SizeOf(Cardinal));
  Cardinal(Pointer(Result)^) := value;
end;

class function BitConverter.GetBytes(value: Int64): TBytes;
begin
  SetLength(Result, SizeOf(Int64));
  Int64(Pointer(Result)^) := value;
end;

class function BitConverter.GetBytes(value: Double): TBytes;
begin
  SetLength(Result, SizeOf(Double));
  Double(Pointer(Result)^) := value;
end;

class function BitConverter.GetBytes(value: Single): TBytes;
begin
  SetLength(Result, SizeOf(Single));
  Single(Pointer(Result)^) := value;
end;

class function BitConverter.Int64BitsToDouble(value: Int64): double;
begin
  Result := Double(Pointer(@value)^);
end;

class function BitConverter.DoubleToInt64Bits(value: double): Int64;
begin
  Result := PInt64(@value)^;
end;

class function BitConverter.ToDouble(const value: TBytes; startIndex: Integer): Double;
begin
  Result := Double(Pointer(PAnsiChar(value) + startIndex)^);
end;

class function BitConverter.ToDouble(const value: PAnsiChar; startIndex: Integer): Double;
begin
  Result := Double(Pointer(value + startIndex)^);
end;

class function BitConverter.ToSingle(const value: TBytes; startIndex: Integer): Single;
begin
  Result := Single(Pointer(PAnsiChar(value) + startIndex)^);
end;

class function BitConverter.ToInt16(const value: TBytes; startIndex: Integer): Int16;
begin
  Result := Int16(Pointer(PAnsiChar(value) + startIndex)^);
end;

class function BitConverter.ToUInt16(const value: TBytes; startIndex: Integer): UInt16;
begin
  Result := UInt16(Pointer(PAnsiChar(value) + startIndex)^);
end;

class function BitConverter.ToUInt16(const value: PAnsiChar; startIndex: Integer): UInt16;
begin
  Result := UInt16(Pointer(value + startIndex)^);
end;

class function BitConverter.ToInt32(const value: TBytes; startIndex: Integer): Int32;
begin
  Result := Int32(Pointer(PAnsiChar(value) + startIndex)^);
end;

class function BitConverter.ToInt32(const value: PAnsiChar; startIndex: Integer): Int32;
begin
  Result := Int32(Pointer(value + startIndex)^);
end;

class function BitConverter.ToUInt32(const value: TBytes; startIndex: Integer): UInt32;
begin
  Result := UInt32(Pointer(PAnsiChar(value) + startIndex)^);
end;

class function BitConverter.ToInt64(const value: TBytes; startIndex: Integer): Int64;
begin
  Result := Int64(Pointer(PAnsiChar(value) + startIndex)^);
end;

class function BitConverter.ToInt64(const value: PAnsiChar; startIndex: Integer): Int64;
begin
  Result := Int64(Pointer(value + startIndex)^);
end;

{ Marshal }

class function Marshal.AllocHGlobal(cb: NativeInt): Pointer;
begin
  GetMem(Result, cb);
end;

class function Marshal.ReallocHGlobal(pv: Pointer; cb: NativeInt): Pointer;
begin
  Result := pv;
  ReallocMem(Result, cb);
end;

class procedure Marshal.FreeHGlobal(hglobal: Pointer);
begin
  FreeMem(hglobal);
end;

class procedure Marshal.FreeCoTaskMem(ptr: Pointer);
begin
end;

class function Marshal.ReadByte(ptr: Pointer; ofs: Integer): Byte;
begin
  Result := Byte(Pointer(NativeInt(ptr) + ofs)^);
end;

class procedure Marshal.WriteByte(ptr: Pointer; val: Byte);
begin
  Byte(ptr^) := val;
end;

class procedure Marshal.WriteByte(ptr: Pointer; ofs: Integer; val: Byte);
begin
  Byte(Pointer(NativeInt(ptr) + ofs)^) := val;
end;

class function Marshal.ReadInt16(ptr: Pointer; ofs: Integer): Int16;
begin
  Result := Int16(Pointer(NativeInt(ptr) + ofs)^);
end;

class procedure Marshal.WriteInt16(ptr: Pointer; val: Int16);
begin
  Int16(ptr^) := val;
end;

class procedure Marshal.WriteInt16(ptr: Pointer; ofs: Integer; val: Int16);
begin
  Int16(Pointer(NativeInt(ptr) + ofs)^) := val;
end;

class function Marshal.ReadInt32(ptr: Pointer; ofs: Integer): Int32;
begin
  Result := Int32(Pointer(NativeInt(ptr) + ofs)^);
end;

class procedure Marshal.WriteInt32(ptr: Pointer; val: Int32);
begin
  Int32(ptr^) := val;
end;

class procedure Marshal.WriteInt32(ptr: Pointer; ofs: Integer; val: Int32);
begin
  Int32(Pointer(NativeInt(ptr) + ofs)^) := val;
end;

class function Marshal.ReadInt64(ptr: Pointer; ofs: Integer): Int64;
begin
  Result := Int64(Pointer(NativeInt(ptr) + ofs)^);
end;

class procedure Marshal.WriteInt64(ptr: Pointer; val: Int64);
begin
  Int64(ptr^) := val;
end;

class procedure Marshal.WriteInt64(ptr: Pointer; val: Double);
begin
  Double(ptr^) := val;
end;

class procedure Marshal.WriteInt64(ptr: Pointer; ofs: Integer; val: Int64);
begin
  Int64(Pointer(NativeInt(ptr) + ofs)^) := val;
end;

class function Marshal.ReadIntPtr(ptr: Pointer; ofs: Integer): Pointer;
begin
  Result := Pointer(Pointer(NativeInt(ptr) + ofs)^);
end;

class procedure Marshal.WriteIntPtr(ptr, val: Pointer);
begin
  Pointer(ptr^) := val;
end;

class procedure Marshal.WriteIntPtr(ptr: Pointer; ofs: Integer; val: Pointer);
begin
  Pointer(Pointer(NativeInt(ptr) + ofs)^) := val;
end;

class function Marshal.PtrToStringAnsi(ptr: Pointer; len: Integer = 0): AnsiString;
begin
  if len > 0 then begin
    SetLength(Result, len);
    Move(ptr^, PAnsiChar(Result)^, len);
  end
  else
    Result := PAnsiChar(ptr);
end;

class function Marshal.PtrToStringUni(ptr: Pointer; len: Integer = 0): WideString;
begin
  if len > 0 then begin
    SetLength(Result, len);
    Move(ptr^, PWideChar(Result)^, len shl 1);
  end
  else
    Result := PWideChar(ptr);
end;

class function Marshal.StringToHGlobalAnsi(const s: AnsiString): Pointer;
begin
  Result := PAnsiChar(s);
end;

class function Marshal.StringToHGlobalUni(const s: WideString): Pointer;
begin
  Result := PWideChar(s);
end;

class procedure Marshal.Copy(const source: TBytes; startIndex: Integer; 
  destination: Pointer; length: Integer);
begin
  if length = 0 then
    Exit;
  Move(Source[StartIndex], destination^, length);
end;

class procedure Marshal.Copy(Source: Pointer; const destination: TBytes; 
  startIndex, length: Integer);
begin
  if length = 0 then
    Exit;
  Move(source^, destination[startIndex], length);
end;

class function Marshal.GetIUnknownForObject(o: TInterfacedObject): Pointer;
var
  iu: IUnknown;
begin
  iu := IUnknown(o);
  iu._AddRef; 
  Result := Pointer(iu);
end;

class function Marshal.AddRef(pUnk: Pointer): Integer;
begin
  Result := IUnknown(pUnk)._AddRef;
end;

class function Marshal.Release(pUnk: Pointer): Integer;
begin
  Result := IUnknown(pUnk)._Release;
end;

{ Encoding }

var
  theDefaultEncoding, theUnicodeEncoding, theUTF8Encoding: Encoding;
{$IFDEF WIN32_64}
  codepageEncodings: array of CodePageEncoding;
{$ENDIF}

class function Encoding.Default: Encoding;
begin
  Result := theDefaultEncoding;
end;

class function Encoding.Unicode: Encoding;
begin
  Result := theUnicodeEncoding;
end;

class function Encoding.UTF8: Encoding;
begin
  Result := theUTF8Encoding;
end;

class function Encoding.GetEncoding(codepage: Cardinal): Encoding;
{$IFDEF WIN32_64}
var
  i: Integer;
{$ENDIF}
begin
  case codepage of
    65001: // CP_UTF8
      Result := theUTF8Encoding;
    1200: // UTF-16
      Result := theUnicodeEncoding;
  else
  {$IFDEF WIN32_64}
    if codepage = GetACP then
  {$ENDIF}
      Result := theDefaultEncoding
  {$IFDEF WIN32_64}
    else begin
      for i := 0 to Length(codepageEncodings) - 1 do
        if codepageEncodings[i].FCodePage = codepage then begin
          Result := codepageEncodings[i];
          exit;
        end;

      Result := CodePageEncoding.Create(codepage);
      SetLength(codepageEncodings, Length(codepageEncodings) + 1);
      codepageEncodings[Length(codepageEncodings) - 1] := CodePageEncoding(Result);
    end;
  {$ENDIF}
  end;
end;

class function Encoding.Convert(srcEncoding, dstEncoding: Encoding; bytes: TBytes): TBytes;
begin
  Result := Convert(srcEncoding, dstEncoding, bytes, 0, Length(bytes));
end;

class function Encoding.Convert(srcEncoding, dstEncoding: Encoding; bytes: TBytes; index, count: Integer): TBytes;
var
  ws: WideString;
begin
  if srcEncoding = dstEncoding then
    Result := Copy(bytes, index, count)
  else begin
    ws := srcEncoding.GetWideString(bytes, index, count);
    Result := dstEncoding.{$IFNDEF VER5}GetBytes{$ELSE}GetBytesWide{$ENDIF}(ws);
  end;
end;

function Encoding.GetBytes(const chars: AnsiString): TBytes;
begin
  SetLength(Result, Length(chars));
  Move(PAnsiChar(chars)^, Pointer(Result)^, Length(chars));
end;

function Encoding.GetBytes(const chars: AnsiString; charIndex, charCount: Integer; var bytes: TBytes; byteIndex: Integer): Integer;
begin
  if charCount > 0 then
    Move((PAnsiChar(chars) + charIndex)^, bytes[byteIndex], charCount);
  Result := charCount;
end;

function Encoding.{$IFNDEF VER5}GetBytes{$ELSE}GetBytesWide{$ENDIF}(const chars: WideString): TBytes;
begin
  Result := GetBytes(AnsiString(chars));
end;

function Encoding.{$IFNDEF VER5}GetBytes{$ELSE}GetBytesWide{$ENDIF}(const chars: WideString; charIndex, charCount: Integer; var bytes: TBytes; byteIndex: Integer): Integer;
begin
  Result := GetBytes(AnsiString(chars), charIndex, charCount, bytes, byteIndex);
end;

function Encoding.GetString(const bytes: TBytes): AnsiString;
begin
  Result := GetString(bytes, 0, Length(bytes));
end;

function Encoding.GetString(const bytes: TBytes; index: Integer; count: Integer): AnsiString;
begin
  if count = 0 then begin
    Result := '';
    Exit;
  end;
  SetLength(Result, count);
  Move(Pointer(@Bytes[index])^, PAnsiChar(Result)^, Length(Result));
end;

function Encoding.GetWideString(const bytes: TBytes): WideString;
begin
  Result := WideString(GetString(bytes));
end;

function Encoding.GetWideString(const bytes: TBytes; index: Integer; count: Integer): WideString;
begin
  Result := WideString(GetString(bytes, index, count));
end;

function Encoding.GetMaxByteCount(charCount: Integer): Integer;
begin
  Result := charCount;
end;

procedure InitEncodings;
begin
  theDefaultEncoding := Encoding.Create;
  theUnicodeEncoding := UnicodeEncoding.Create;
  theUTF8Encoding := UTF8Encoding.Create;
{$IFDEF WIN32_64}
  codepageEncodings := nil;
{$ENDIF}
end;

procedure FreeEncodings;
{$IFDEF WIN32_64}
var
  i: Integer;
{$ENDIF}
begin
  theDefaultEncoding.Free;
  theUnicodeEncoding.Free;
  theUTF8Encoding.Free;

{$IFDEF WIN32_64}
  for i := 0 to Length(codepageEncodings) - 1 do
    codepageEncodings[i].Free;

  codepageEncodings := nil;
{$ENDIF}
end;

{ UnicodeEncoding }

function UnicodeEncoding.GetBytes(const chars: AnsiString): TBytes;
begin
  Result := {$IFNDEF VER5}GetBytes{$ELSE}GetBytesWide{$ENDIF}(WideString(chars));
end;

function UnicodeEncoding.GetBytes(const chars: AnsiString; charIndex, charCount: Integer; var bytes: TBytes; byteIndex: Integer): Integer;
begin
  Result := {$IFNDEF VER5}GetBytes{$ELSE}GetBytesWide{$ENDIF}(WideString(chars), charIndex, charCount,
    bytes, byteIndex);
end;

function UnicodeEncoding.{$IFNDEF VER5}GetBytes{$ELSE}GetBytesWide{$ENDIF}(const chars: WideString): TBytes;
begin
  SetLength(Result, Length(chars) shl 1);
  Move(PWideChar(chars)^, Pointer(Result)^, Length(chars) shl 1);
end;

function UnicodeEncoding.{$IFNDEF VER5}GetBytes{$ELSE}GetBytesWide{$ENDIF}(const chars: WideString; charIndex, charCount: Integer; var bytes: TBytes; byteIndex: Integer): Integer;
begin
  Move((PWideChar(chars) + charIndex)^, bytes[byteIndex], charCount shl 1);
  Result := charCount shl 1;
end;

function UnicodeEncoding.GetString(const bytes: TBytes): AnsiString;
begin
  Result := AnsiString(GetWideString(bytes));
end;

function UnicodeEncoding.GetString(const bytes: TBytes; index: Integer; count: Integer): AnsiString;
begin
  Result := AnsiString(GetWideString(bytes, index, count));
end;

function UnicodeEncoding.GetWideString(const bytes: TBytes): WideString;
begin
  Result := GetWideString(bytes, 0, Length(bytes));
end;

function UnicodeEncoding.GetWideString(const bytes: TBytes; index: Integer; count: Integer): WideString;
begin
  if count = 0 then begin
    Result := '';
    Exit;
  end;
  SetLength(Result, count shr 1);
  Move(Pointer(@bytes[index])^, PWideChar(Result)^, count);
end;

function UnicodeEncoding.GetMaxByteCount(charCount: Integer): Integer;
begin
  Result := charCount * 2;
end;

function UTF8Encoding.GetBytes(const chars: AnsiString): TBytes;
var
  UTF8: AnsiString;
begin
  UTF8 := CRFunctions.AnsiToUtf8(AnsiString(chars));
  SetLength(Result, Length(UTF8));
  Move(PAnsiChar(UTF8)^, Pointer(Result)^, Length(UTF8));
end;

function UTF8Encoding.GetBytes(const chars: AnsiString; charIndex, charCount: Integer; var bytes: TBytes; byteIndex: Integer): Integer;
begin
  Assert(False, 'not implemented');
  Result := 0;
end;

function UTF8Encoding.{$IFNDEF VER5}GetBytes{$ELSE}GetBytesWide{$ENDIF}(const chars: WideString): TBytes;
var
  UTF8: UTF8String;
begin
  UTF8 := CRFunctions.UTF8Encode(chars);
  SetLength(Result, Length(UTF8));
  Move(PAnsiChar(UTF8)^, Pointer(Result)^, Length(UTF8));
end;

function UTF8Encoding.{$IFNDEF VER5}GetBytes{$ELSE}GetBytesWide{$ENDIF}(const chars: WideString; charIndex, charCount: Integer; var bytes: TBytes; byteIndex: Integer): Integer;
begin
  Assert(False, 'not implemented');
  Result := 0;
end;

function UTF8Encoding.GetString(const bytes: TBytes): AnsiString;
begin
  Result := GetString(bytes, 0, Length(bytes));
end;

function UTF8Encoding.GetString(const bytes: TBytes; index: Integer; count: Integer): AnsiString;
var
  UTF8: UTF8String;
begin
  if count = 0 then begin
    Result := '';
    Exit;
  end;
  SetLength(UTF8, count);

  Move(Pointer(@bytes[index])^, PAnsiChar(UTF8)^, Length(UTF8));
  Result := AnsiString(CRFunctions.Utf8ToAnsi(UTF8));
end;

function UTF8Encoding.GetWideString(const bytes: TBytes): WideString;
begin
  Result := GetWideString(bytes, 0, Length(bytes));
end;

function UTF8Encoding.GetWideString(const bytes: TBytes; index: Integer; count: Integer): WideString;
var
  UTF8: UTF8String;
begin
  if count = 0 then begin
    Result := '';
    Exit;
  end;
  SetLength(UTF8, count);

  Move(Pointer(@bytes[index])^, PAnsiChar(UTF8)^, Length(UTF8));
  Result := CRFunctions.UTF8Decode(UTF8);
end;

function UTF8Encoding.GetMaxByteCount(charCount: Integer): Integer;
begin
  Result := charCount * 3;
end;

{$IFDEF WIN32_64}
constructor CodePageEncoding.Create(CodePage: Cardinal);
begin
  inherited Create;

  FCodePage := CodePage;
end;

function CodePageEncoding.GetBytes(const chars: AnsiString): TBytes;
begin
  Result := {$IFNDEF VER5}GetBytes{$ELSE}GetBytesWide{$ENDIF}(WideString(chars));
end;

function CodePageEncoding.GetBytes(const chars: AnsiString; charIndex, charCount: Integer; var bytes: TBytes; byteIndex: Integer): Integer;
begin
  Result := {$IFNDEF VER5}GetBytes{$ELSE}GetBytesWide{$ENDIF}(WideString(chars),
    charIndex, charCount, bytes, byteIndex);
end;

function CodePageEncoding.{$IFNDEF VER5}GetBytes{$ELSE}GetBytesWide{$ENDIF}(const chars: WideString): TBytes;
var
  byteCount: Integer;
begin
  if chars = '' then
    Result := nil
  else begin
    byteCount := WideCharToMultiByte(FCodePage, 0, PWideChar(chars), Length(chars),
      nil, 0, nil, nil);
    Win32Check(LongBool(byteCount));
    SetLength(Result, byteCount);
    {$IFNDEF VER5}GetBytes{$ELSE}GetBytesWide{$ENDIF}(chars, 0, Length(chars), Result, 0);
  end;
end;

function CodePageEncoding.{$IFNDEF VER5}GetBytes{$ELSE}GetBytesWide{$ENDIF}(const chars: WideString; charIndex, charCount: Integer; var bytes: TBytes; byteIndex: Integer): Integer;
begin
  if charCount <= 0 then
    Result := 0
  else begin
    Result := WideCharToMultiByte(FCodePage, 0, PWideChar(chars) + charIndex, charCount,
      PAnsiChar(bytes) + byteIndex, Length(bytes) - byteIndex, nil, nil);
    Win32Check(LongBool(Result));
  end;
end;

function CodePageEncoding.GetString(const bytes: TBytes): AnsiString;
begin
  Result := AnsiString(GetWideString(bytes));
end;

function CodePageEncoding.GetString(const bytes: TBytes; index: Integer; count: Integer): AnsiString;
begin
  Result := AnsiString(GetWideString(bytes, index, count));
end;

function CodePageEncoding.GetWideString(const bytes: TBytes): WideString;
begin
  Result := GetWideString(bytes, 0, Length(bytes));
end;

function CodePageEncoding.GetWideString(const bytes: TBytes; index: Integer; count: Integer): WideString;
var
  charCount: Integer;
begin
  if count <= 0 then
    Result := ''
  else begin
    charCount := MultiByteToWideChar(FCodePage, 0, PAnsiChar(bytes) + index, count, nil, 0);
    Win32Check(LongBool(charCount));
    SetLength(Result, charCount);
    charCount := MultiByteToWideChar(FCodePage, 0, PAnsiChar(bytes) + index, count,
      PWideChar(Result), charCount);
    Win32Check(LongBool(charCount));
  end;
end;

function CodePageEncoding.GetMaxByteCount(charCount: Integer): Integer;
begin
  if FCodePage = 1201 then
    Result := charCount * 2
  else
    Result := charCount;
end;

{$ENDIF}

{ AnsiStringBuilder }

constructor AnsiStringBuilder.Create(capacity: Integer);
begin
  inherited Create;

  FActualLength := 0;
  SetLength(FString, capacity);
end;

constructor AnsiStringBuilder.Create(const value: AnsiString; capacity: Integer);
begin
  Create(capacity);
  Append(value);
end;

procedure AnsiStringBuilder.SetActualLength(Value: Integer);
var
  l: Integer;
begin
  l := System.Length(FString);
  if l - FActualLength < Value then
    SetLength(FString, FActualLength + Value + l shr 1);
  FActualLength := Value;
end;

procedure AnsiStringBuilder.Append(const value: AnsiString);
var
  l, ls: Integer;

begin
  ls := System.Length(value);
  if ls = 0 then
    Exit;

  l := System.Length(FString);
  if l - FActualLength < ls then
    SetLength(FString, FActualLength + ls + l shr 1);
  Move(Pointer(value)^, FString[FActualLength], ls);
  Inc(FActualLength, ls);
end;

procedure AnsiStringBuilder.Append(const value: AnsiString; const startIndex: Integer; const count: Integer);
var
  l: Integer;

begin
  if count = 0 then
    Exit;

  l := System.Length(FString);
  if l - FActualLength < count then
    SetLength(FString, FActualLength + count + l shr 1);
  Move(value[startIndex + 1], FString[FActualLength], count);
  Inc(FActualLength, count);
end;

procedure AnsiStringBuilder.Append(value: AnsiChar);
var
  l: Integer;

begin
  l := System.Length(FString);
  if l - FActualLength < 1 then
    SetLength(FString, FActualLength + 1 + l shr 1);
  FString[FActualLength] := value;
  Inc(FActualLength);
end;

procedure AnsiStringBuilder.Append(value: AnsiChar; repeatCount: Integer);
var
  s: AnsiString;
begin
  s := StringOfChar(value, repeatCount);
  Append(s);
end;

procedure AnsiStringBuilder.Append(value: AnsiStringBuilder);
var
  l: Integer;

begin
  if (value = nil) or (value.Length = 0) then
    Exit;

  l := System.Length(FString);
  if l - FActualLength < value.Length then
    SetLength(FString, FActualLength + value.Length + l shr 1);
  Move(Pointer(value.FString)^, FString[FActualLength], value.Length);
  Inc(FActualLength, value.Length);
end;

procedure AnsiStringBuilder.Insert(index: Integer; const value: AnsiString);
var
  l, ls: Integer;

begin
  l := System.Length(FString);
  ls := System.Length(value);
  if l - FActualLength < ls then
    SetLength(FString, FActualLength + ls + l shr 1);

  Move(FString[Index], FString[Index + ls], FActualLength - Index);
  Move(Pointer(value)^, FString[Index], ls);

  Inc(FActualLength, ls);
end;

procedure AnsiStringBuilder.Replace(const OldValue: AnsiString; const NewValue: AnsiString);

  function PosEx(const SubStr: AnsiString; const S: TAnsiCharArray; StartPos, EndPos: Integer): Integer;
  var
    I,X: Integer;
    LenSubStr: Integer;
  begin
    I := StartPos;
    LenSubStr := System.Length(SubStr);
    EndPos := EndPos - LenSubStr + 1;
    while I <= EndPos do begin
      if S[I] = SubStr[1] then begin
        X := 1;
        while (X < LenSubStr) and (S[I + X] = SubStr[X + 1]) do
          Inc(X);
        if (X = LenSubStr) then
        begin
          Result := I;
          exit;
        end;
      end;
      Inc(I);
    end;
    Result := -1;
  end;

  procedure InsertS(index: Integer; const value: AnsiString; offset: Integer);
  var
    l, ls: Integer;
  begin
    l := System.Length(FString);
    ls := System.Length(value) - offset + 1;
    if l - FActualLength < ls then
      SetLength(FString, FActualLength + ls + l shr 1);

    if FActualLength > Index then
      Move(FString[Index], FString[Index + ls], FActualLength - Index);
    Move(value[offset], FString[Index], ls);
  end;

var
  lOld, lNew: Integer;
  Index: Integer;

begin
  lOld := System.Length(OldValue);
  lNew := System.Length(NewValue);
  Index := PosEx(OldValue, FString, 0, FActualLength - 1);

  while Index >= 0 do begin
    if lOld > lNew then begin
      Move(Pointer(NewValue)^, FString[Index], lNew);
      Move(FString[Index + lOld], FString[Index + lNew],
        FActualLength - Index - lOld + 1);
    end
    else
    if lOld < lNew then begin
      Move(Pointer(NewValue)^, FString[Index], lOld);
      InsertS(Index + lOld, NewValue, lOld + 1);
    end
    else
      Move(Pointer(NewValue)^, FString[Index], lNew);

    Inc(FActualLength, lNew - lOld);
    Index := PosEx(OldValue, FString, Index + lNew, FActualLength - 1);
  end;
end;

function AnsiStringBuilder.ToString: AnsiString;
begin
  SetLength(Result, FActualLength);
  if FActualLength > 0 then
    Move(FString[0], Result[1], FActualLength);
end;

{ WideStringBuilder }

constructor WideStringBuilder.Create(capacity: Integer);
begin
  inherited Create;

  FActualLength := 0;
  SetLength(FString, capacity);
end;

constructor WideStringBuilder.Create(const value: WString; capacity: Integer);
begin
  Create(capacity);
  Append(value);
end;

procedure WideStringBuilder.SetActualLength(Value: Integer);
var
  l: Integer;
begin
  l := System.Length(FString);
  if l - FActualLength < Value then
    SetLength(FString, FActualLength + Value + l shr 1);
  FActualLength := Value;
end;

procedure WideStringBuilder.Append(const value: WString);
var
  l, ls: Integer;
begin
  ls := System.Length(value);
  if ls = 0 then
    Exit;

  l := System.Length(FString);
  if l - FActualLength < ls then
    SetLength(FString, FActualLength + ls + l shr 1);
  Move(Pointer(value)^, FString[FActualLength], ls * SizeOf(WideChar));
  Inc(FActualLength, ls);
end;

procedure WideStringBuilder.Append(const value: WString; const startIndex: Integer; const count: integer);
var
  l: Integer;
begin
  if count = 0 then
    Exit;

  l := System.Length(FString);
  if l - FActualLength < count then
    SetLength(FString, FActualLength + count + l shr 1);
  Move(value[startIndex + 1], FString[FActualLength], count * SizeOf(WideChar));
  Inc(FActualLength, count);
end;

procedure WideStringBuilder.Append(value: WideChar);
var
  l: Integer;
begin
  l := System.Length(FString);
  if l - FActualLength < 1 then
    SetLength(FString, FActualLength + 1 + l shr 1);
  FString[FActualLength] := value;
  Inc(FActualLength);
end;

procedure WideStringBuilder.Append(value: WideChar; repeatCount: Integer);
var
  s: WString;
begin
  s := StringOfChar(value, repeatCount);
  Append(s);
end;

procedure WideStringBuilder.Append(value: WideStringBuilder);
var
  l: Integer;
begin
  if (value = nil) or (value.Length = 0) then
    Exit;

  l := System.Length(FString);
  if l - FActualLength < value.Length then
    SetLength(FString, FActualLength + value.Length + l shr 1);
  Move(Pointer(value.FString)^, FString[FActualLength], value.Length * SizeOf(WideChar));
  Inc(FActualLength, value.Length);
end;

procedure WideStringBuilder.Insert(index: Integer; const value: WString);
var
  l, ls: Integer;
begin
  l := System.Length(FString);
  ls := System.Length(value);
  if l - FActualLength < ls then
    SetLength(FString, FActualLength + ls + l shr 1);

  Move(FString[Index], FString[Index + ls], (FActualLength - Index) * SizeOf(WideChar));
  Move(Pointer(value)^, FString[Index], ls * SizeOf(WideChar));

  Inc(FActualLength, ls);
end;

procedure WideStringBuilder.Replace(const OldValue: WString; const NewValue: WString);

  function PosEx(const SubStr: WideString; const S: TWideCharArray; StartPos, EndPos: Integer): Integer;
  var
    I,X: Integer;
    LenSubStr: Integer;
  begin
    I := StartPos;
    LenSubStr := System.Length(SubStr);
    EndPos := EndPos - LenSubStr + 1;
    while I <= EndPos do begin
      if S[I] = SubStr[1] then begin
        X := 1;
        while (X < LenSubStr) and (S[I + X] = SubStr[X + 1]) do
          Inc(X);
        if (X = LenSubStr) then
        begin
          Result := I;
          exit;
        end;
      end;
      Inc(I);
    end;
    Result := -1;
  end;

  procedure InsertS(index: Integer; const value: WideString; offset: Integer);
  var
    l, ls: Integer;
  begin
    l := System.Length(FString);
    ls := System.Length(value) - offset + 1;
    if l - FActualLength < ls then
      SetLength(FString, FActualLength + ls + l shr 1);

    if FActualLength > Index then
      Move(FString[Index], FString[Index + ls],
        (FActualLength - Index) * SizeOf(WideChar));
    Move(value[offset], FString[Index], ls * SizeOf(WideChar));
  end;

var
  lOld, lNew: Integer;
  Index: Integer;
begin
  lOld := System.Length(OldValue);
  lNew := System.Length(NewValue);
  Index := PosEx(OldValue, FString, 0, FActualLength - 1);

  while Index >= 0 do begin
    if lOld > lNew then begin
      Move(Pointer(NewValue)^, FString[Index], lNew * SizeOf(WideChar));
      Move(FString[Index + lOld], FString[Index + lNew],
        (FActualLength - Index - lOld + 1) * SizeOf(WideChar));
    end else
    if lOld < lNew then begin
      Move(Pointer(NewValue)^, FString[Index], lOld * SizeOf(WideChar));
      InsertS(Index + lOld, NewValue, lOld + 1);
    end else
      Move(Pointer(NewValue)^, FString[Index], lNew * SizeOf(WideChar));

    Inc(FActualLength, lNew - lOld);
    Index := PosEx(OldValue, FString, Index + lNew, FActualLength - 1);
  end;
end;

function WideStringBuilder.ToString: WString;
begin
  SetLength(Result, FActualLength);
  if FActualLength > 0 then
    Move(FString[0], Result[1], FActualLength * SizeOf(WideChar));
end;

{ Buffer }

class procedure Buffer.BlockCopy(const src: TBytes; srcOffset: Integer; const dst: TBytes; dstOffset: Integer; count: Integer);
begin
  Move((PAnsiChar(src) + srcOffset)^, (PAnsiChar(dst) + dstOffset)^, count);
end;

class function Buffer.GetByte(const src: Pointer; Index: Integer): Byte;
begin
  Result := Byte((PAnsiChar(src) + Index)^);
end;

class procedure Buffer.SetByte(const src: Pointer; Index: Integer; Value: Byte);
begin
  Byte((PAnsiChar(src) + Index)^) := Value;
end;

{ MemoryStream }
constructor MemoryStream.Create(Capacity: Integer);
begin
  inherited Create;
  System.SetLength(FData, Capacity);
end;

procedure MemoryStream.Close;
begin
  System.SetLength(FData, 0);
end;

procedure MemoryStream.SetLength(Value: Integer);
var
  l: Integer;
begin
  l := System.Length(FData); // Performance opt
  if Value > l then
    System.SetLength(FData, Max(Value, l + l div 2));
  FLength := Value;
end;

procedure MemoryStream.SetPosition(const Pos: Integer);
begin
  if Pos > Length then
    Length := Pos;
  FPosition := Pos;
end;

function MemoryStream.Read(var Buffer: TBytes; Offset: Integer; Count: Integer): Integer;
begin
  Result := Read(PAnsiChar(@Buffer[0]), Offset, Count);
end;

function MemoryStream.Read(Buffer: PAnsiChar; Offset: Integer; Count: Integer): Integer;
begin
  if (FPosition >=0) and (Count > 0) then begin
    Result := System.Length(FData) - FPosition;
    if Result > Count then
      Result := Count;
    Move(PAnsiChar(@FData[FPosition])^, Buffer[Offset], Result);
    Inc(FPosition, Result);
  end
  else
    Result := 0;
end;

function MemoryStream.ReadByte: Byte;
begin
  Result := FData[FPosition];
  Inc(FPosition);
end;

function MemoryStream.GetBuffer: PAnsiChar;
begin
  Result := @FData[0];
end;

procedure MemoryStream.Write(const Buffer: TBytes; Offset: Integer; Count: Integer);
begin
  Write(PAnsiChar(@Buffer[0]), Offset, Count);
end;

procedure MemoryStream.Write(Buffer: PAnsiChar; Offset: Integer; Count: Integer);
var
  l: Integer;
begin
  if (FPosition >=0) and (Count > 0) then begin
    l := FPosition + Count;
    if l > Length then
      Length := l;
    Move(Buffer[Offset], PAnsiChar(@FData[FPosition])^, Count);
    Inc(FPosition, Count);
  end;
end;

procedure MemoryStream.WriteByte(Value: Byte);
var
  l: Integer;
begin
  l := FPosition + 1;
  if l > Length then
    Length := l;
  FData[FPosition] := Value;
  Inc(FPosition);
end;

function MemoryStream.Seek(Offset: Integer; Origin: Word): Integer;
begin
  case Origin of
    soFromBeginning: FPosition := Offset;
    soFromCurrent: FPosition := FPosition + Offset;
    soFromEnd: FPosition := System.Length(FData) - Offset;
  end;
  if FPosition > System.Length(FData) then
    FPosition := System.Length(FData)
  else if FPosition < 0 then FPosition := 0;
  Result := FPosition;
end;

{ ArgumentException }
constructor ArgumentException.Create;
begin
  inherited Create('');
end;

constructor ArgumentException.Create(const Msg: string);
begin
  inherited Create(Msg);
end;

constructor NotSupportedException.Create;
begin
  inherited Create('');
end;

constructor NotSupportedException.Create(const Msg: string);
begin
  inherited Create(Msg);
end;

initialization
  InitEncodings;

finalization
  FreeEncodings;

end.
