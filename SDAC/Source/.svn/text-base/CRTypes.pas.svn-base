
//////////////////////////////////////////////////
//  Copyright © 1998-2012 Devart. All right reserved.
//  CRTypes
//////////////////////////////////////////////////

{$IFNDEF CLR}

{$I Dac.inc}

unit CRTypes;
{$ENDIF}
interface

uses
{$IFDEF CLR}
  System.Xml, System.Text,
{$ENDIF}
{$IFDEF CRUNICODE}
  WideStrings,
{$ENDIF}
  Classes, SysUtils;

{$IFNDEF VER6P}
const
  varShortInt = $0010; { vt_i1          }
  varWord     = $0012; { vt_ui2         }
  varLongWord = $0013; { vt_ui4         }
  varInt64    = $0014; { vt_i8          }

  MaxBCDPrecision = 18;
  MaxBCDScale = 4;
{$ENDIF}

{$IFNDEF VER7P}
const
  SecsPerMin    = 60;
  MSecsPerSec   = 1000;
{$ENDIF}

{$IFDEF CLR}
const
  soFromBeginning = soBeginning;
  soFromCurrent = soCurrent;
  soFromEnd = soEnd;

  wsAttribute: System.Xml.WriteState = System.Xml.WriteState.Attribute;
  wsClosed: System.Xml.WriteState = System.Xml.WriteState.Closed;
  wsContent: System.Xml.WriteState = System.Xml.WriteState.Content;
  wsElement: System.Xml.WriteState = System.Xml.WriteState.Element;
  wsStart: System.Xml.WriteState = System.Xml.WriteState.Start;

  fmtNone: System.Xml.Formatting = System.Xml.Formatting.None;
  fmtIndented: System.Xml.Formatting = System.Xml.Formatting.Indented;

  ntNone: System.Xml.XmlNodeType = System.Xml.XmlNodeType.None;
  ntElement: System.Xml.XmlNodeType = System.Xml.XmlNodeType.Element;
  ntAttribute: System.Xml.XmlNodeType = System.Xml.XmlNodeType.Attribute;
  ntEndElement: System.Xml.XmlNodeType = System.Xml.XmlNodeType.EndElement;
  ntComment: System.Xml.XmlNodeType = System.Xml.XmlNodeType.Comment;
  ntDeclaration: System.Xml.XmlNodeType = System.Xml.XmlNodeType.XmlDeclaration;
  ntDocumentType: System.Xml.XmlNodeType = System.Xml.XmlNodeType.DocumentType;
  ntText: System.Xml.XmlNodeType = System.Xml.XmlNodeType.Text;
{$ENDIF}

type
{$IFNDEF VER16P}
{$IFNDEF FPC}
  NativeInt = Integer;
  NativeUInt = Cardinal;
{$ELSE}
  NativeInt = SizeInt;
  NativeUInt = SizeUInt;
{$ENDIF}
{$ENDIF}

{$IFNDEF CLR}
  PAChar = PAnsiChar;
  PWChar = PWideChar;

{$IFNDEF VER11P}
  TBytes = array of Byte;
{$ENDIF}

  TValueArr = PAnsiChar;
{$ELSE}
  TValueArr = TBytes;

  PChar = string;
  PAnsiChar = string;
  PWideChar = string;

  PAChar = IntPtr;
  PWChar = IntPtr;
  POleVariant = IntPtr;

  AnsiStringBuilder = StringBuilder;
  WideStringBuilder = StringBuilder;
{$ENDIF}

{$IFNDEF VER6P}
  TVarType = Word;

  // Copied from d7 SysUtils
  EOSError = class(Exception)
  public
    ErrorCode: longword;
  end;
{$ENDIF}

  TByteArr = TBytes;
  TIntArr = array of Integer;
  TWordArr = array of Word;

{$IFDEF CRUNICODE}
  _string = WideString;
  _char = WideChar;
  _PChar = PWideChar;
  _TStrings = TWideStrings;
  _TStringList = TWideStringList;
  {$IFDEF CLR}
  _StringBuilder = WideStringBuilder;
  {$ENDIF}
{$ELSE}
  _string = string;
  _char = char;
  _PChar = PChar;
  _TStrings = TStrings;
  _TStringList = TStringList;
  {$IFDEF CLR}
  _StringBuilder = StringBuilder;
  {$ENDIF}
{$ENDIF}

{$IFDEF UTF8}
type
  UTF8String = type AnsiString;
{$ENDIF}

implementation

end.
