
//////////////////////////////////////////////////
//  DB Access Components
//  Copyright © 1998-2012 Devart. All right reserved.
//  CRXml
//////////////////////////////////////////////////

unit CRXml;

interface

{$I Dac.inc}
uses
  Classes, SysUtils, Contnrs,
{$IFDEF VER12P}
  AnsiStrings,
{$ENDIF}
{$IFNDEF CLR}
  CLRClasses,
{$ENDIF}
  CRTypes;

type
  //TChars = array of char;

  StreamWriter = class
  private
    FStream: TStream;
    FReleaseStream: Boolean;
    FEncoding: Encoding;
  public
    constructor Create(const path: string; Append: Boolean); overload;
    constructor Create(output: TStream; aEncoding: Encoding); overload;
    destructor Destroy; override;

    procedure Close;
    procedure Flush;
    procedure Write(const value: WideString);
    procedure WriteLine(const value: WideString);
  end;

  XmlException = class(Exception);

  XmlNodeType = (ntNone, ntElement, ntAttribute, ntEndElement, ntComment, ntDeclaration, ntDocumentType, ntText);
  XmlReadState = (Initial, Interactive, Error, EndOfFile, Closed);

  XmlTextReader = class
  private
    FBlocks: TList;
    FStream: TStream;
    FStreamPosition: int64;// When this class created through constructor Create(str: string), FStreamPosition = FFullSize
    FBlockSize: integer;
    FFullSize: int64;
    FMaxNumBlock: integer;
    FCurPosition: integer;
    FActualPosition: integer;
    FActualBlockSize: integer;
    FPrefix: _string;
    FValue: _string;
    FName: _string;
    FNodeType: XmlNodeType;
    FAttrNames: _TStringList;
    FAttrPrefix: _TStringList;
    FAttrValues: _TStringList;
    FOffset: Integer;
    FState: XmlReadState;
    FCurrElementName: _string;
    function GetHasAttributes: Boolean;
    function GetDepth: integer;
    function GetAttributeCount: integer;
    procedure GetXMLNodeAttributes(const Node: _string; AttrNames, AttrValues: _TStrings);
    procedure InitInstance;
    function GetEof: Boolean;
    function LoadNextBlock(ReplaceBuffer: boolean = True): boolean;
    function ReadTo(const SubStr: AnsiString; out ResultStr: AnsiString;
      const AdvLenth: integer = 0): Boolean; overload;
    function IsToken(const SubStr: AnsiString): Boolean;
    function GetNextSymbol: AnsiChar;
    function MoveTo(const Lexem: AnsiString): Boolean;
    procedure FreeOldBlocks;

  public
    constructor Create(Stream: TStream); overload;
    constructor Create(const Str: AnsiString {UTF8}); overload; // Parameter "Str" mast content only XML Text, Url for XML not supported
    destructor Destroy; override;

    procedure MoveToAttribute(i: integer); overload;
    function MoveToAttribute(name: _string): Boolean; overload;
    function Read: boolean;
    function Items(const Index: Integer): _string; overload;
    function Items(const AttrName: _string): _string; overload;

    property Name: _string read FName;
    property Prefix: _string read FPrefix;
    property Value: _string read FValue;
    property NodeType: XmlNodeType read FNodeType;
    property AttributeCount: integer read GetAttributeCount;
    property Depth: integer read GetDepth;
    property ReadState: XmlReadState read FState;
    property Eof: Boolean read GetEof;
    property HasAttributes: Boolean read GetHasAttributes;
  end;

  XmlFormatting = (fmtNone, fmtIndented);
  XmlWriteState = (wsAttribute, wsClosed, wsContent, wsElement, wsStart);

  XmlTextWriter = class
  private
    FText: WideString;
    FFormatting: XmlFormatting;
    FIndentation: Integer;
    FIndentChar: WideChar;
    FQuoteChar: WideChar;
    FWriteState: XmlWriteState;
    FDepth: Integer;
    FPosStack: TStack;
    FTagStack: _TStringList;
    FWriter: StreamWriter;
    function IndentStr: WideString;
    function PopTagName: _string;
    procedure PushTagName(const TagName: _string);
    procedure InternalCloseStartTag;
  protected
    procedure InternalWriteStartElement(const Prefix, LocalName, ns: _string);
    procedure InternalWriteElementString(const LocalName, ns: _string; Value: WideString);
    procedure InternalWriteAttributeString(const Prefix, LocalName, ns: _string; Value: WideString);
    procedure InternalWriteEndElement;
    procedure FlushData;
  public
    constructor Create(w: StreamWriter);
    destructor Destroy; override;

    procedure WriteStartElement(const LocalName: _string); overload;
    procedure WriteStartElement(const Prefix, LocalName, ns: _string); overload;
    procedure WriteStartElement(const LocalName, ns: _string); overload;
    procedure WriteEndElement;
    procedure WriteFullEndElement;
    procedure WriteString(const Text: WideString);
    procedure WriteElementString(const LocalName, ns: _string; Value: WideString); overload;
    procedure WriteElementString(const LocalName: _string; Value: WideString); overload;

    procedure WriteAttributeString(const LocalName: _string; Value: WideString); overload;
    procedure WriteAttributeString(const Prefix, LocalName, ns: _string; Value: WideString); overload;
    procedure Close;

    property Formatting: XmlFormatting read FFormatting write FFormatting;
    property Indentation: Integer read FIndentation write FIndentation;
    property IndentChar: WideChar read FIndentChar write FIndentChar;
    property QuoteChar: WideChar read FQuoteChar write FQuoteChar;
    property WriteState: XmlWriteState read FWriteState;
  end;

function XMLEncode(const AStr: WideString): WideString;
function XMLDecode(const AStr: _string): _string;


implementation

uses
  CRFunctions, CRParser, MemUtils;

const
  LineSeparator = #13#10;
  SInvalidXML = 'Invalid XML';
  SClassNotSupported = 'Class %s is not supported';

  procedure DeleteInvisibleSymbol(var s: AnsiString); forward;

type
  TXmlParser = class(TParser)
  public
    constructor Create(const Text: _string); override;
    function GetNextIdent(out Lexem: _string): integer;
  end;

var
  XmlSymbolLexems, XmlKeywordLexems: _TStringList;
  XmlSymbolChars: TCharSet;

{ XmlTextReader }

function XmlTextReader.LoadNextBlock(ReplaceBuffer: boolean = True): boolean;
var
  i: integer;
  p: pointer;
begin
  Result := False;
  if FStream = nil then 
    Exit;

  if ReplaceBuffer then begin
    if FBlocks.Count = 0 then begin
      GetMem(p, FBlockSize);
      FBlocks.Add(p);
    end;
    i := 0;
  end 
  else begin
    GetMem(p, FBlockSize);
    i := FBlocks.Add(p);
  end;
  FStream.Position := FStreamPosition;
  FActualBlockSize := FStream.Read(FBlocks[i]^, FBlockSize);
  Inc(FStreamPosition, FActualBlockSize); 
  Result := FActualBlockSize <> 0;
  if Result then begin
    FActualPosition := 0;
    FCurPosition := 0;
  end;
end;

function XmlTextReader.IsToken(const SubStr: AnsiString): boolean;
var
  i, SO, SubStrLen: integer;
begin
  SO := FActualPosition;
  SubStrLen := Length(SubStr);

  for i := 0 to SubStrLen - 1 do begin
    if SO > (FActualBlockSize - 1) then begin
      LoadNextBlock;
      SO := 0;
    end;
    if PAnsiChar(PtrOffset(FBlocks[0], SO))^ <> SubStr[i + 1] then begin
      Result := False;
      Exit;
    end;
    Inc(SO);
  end;
  Inc(FActualPosition, SubStrLen);
  FCurPosition := FActualPosition;
  Result := True;
end;

function XmlTextReader.GetNextSymbol: AnsiChar;
begin
  if FCurPosition > (FActualBlockSize - 1) then
    LoadNextBlock;
  Result := PAnsiChar(PtrOffset(FBlocks[0], FCurPosition))^;
  Inc(FCurPosition);
end;

function XmlTextReader.MoveTo(const Lexem: AnsiString): boolean;
begin
  Result := False;
  while (FActualPosition < FFullSize) and not Eof do begin
    if IsToken(Lexem) then begin
      Result := True;
      Exit;
    end;
    Inc(FActualPosition);
  end;
end;

function XmlTextReader.ReadTo(const SubStr: AnsiString;
  out ResultStr: AnsiString; const AdvLenth: integer): boolean;
var
  i, j, SI, FoundSI, SubIndex, SubStrLen,
  StartPos, Len, FoundPos: integer;
begin
  SI := 0;
  FoundSI := 0;
  SubIndex := 1;
  StartPos := FActualPosition;
  FoundPos := -1;
  SubStrLen := Length(SubStr);
  Result := False;
  I := StartPos;

  while SI <= (FMaxNumBlock - 1) do begin
    while I <= (FActualBlockSize - 1) do begin

      if PAnsiChar(PtrOffset(FBlocks[SI], I))^ <> SubStr[SubIndex] then begin
        SubIndex := 1;
        FoundPos := -1;
        Inc(I);
        Continue;
      end;

      if FoundPos = -1 then begin
        FoundPos := I;
        FoundSI := SI;
      end;

      if SubIndex = SubStrLen then begin
        if FoundSI > 0 then
          Len := FBlockSize - StartPos + FoundPos
        else
          Len := FoundPos - StartPos;

        if FoundSI > 1 then
          Len := Len + (FoundSI - 1) * FBlockSize;

        Len := Len + AdvLenth;

        SetLength(ResultStr, Len);

        if FoundSI > 0 then begin
          Len := FBlockSize - StartPos;
          CopyBuffer(PtrOffset(FBlocks[0], StartPos), @ResultStr[1], Len);

          if FoundSI > 1 then
            for J := 1 to (FoundSI - 1) do // считываем промежуточные целые блоки
              CopyBuffer(FBlocks[j], @ResultStr[Len + 1 + FBlockSize * (J - 1)], FBlockSize);

          if (FoundPos + AdvLenth) > FBlockSize then begin
            CopyBuffer(FBlocks[FoundSI], @ResultStr[Len + 1 + FBlockSize * (FoundSI - 1)], FBlockSize);
            LoadNextBlock;
            CopyBuffer(FBlocks[FoundSI], @ResultStr[Len + 1 + FBlockSize * (FoundSI)], FBlockSize - (FoundPos + AdvLenth));
          end else
            if (FoundPos + AdvLenth) <> 0  then
              CopyBuffer(FBlocks[FoundSI], @ResultStr[Len + 1 + FBlockSize * (FoundSI - 1)], FoundPos + AdvLenth);
          FreeOldBlocks;
        end 
        else begin
          if (FoundPos + AdvLenth) > FBlockSize then begin
            Len := FBlockSize - StartPos;
            CopyBuffer(PtrOffset(FBlocks[0], StartPos), @ResultStr[1], Len);
            LoadNextBlock;
            CopyBuffer(FBlocks[1], @ResultStr[FBlockSize - StartPos + 1], (FoundPos - StartPos + AdvLenth) - Len);
            FreeOldBlocks;
          end else
            CopyBuffer(PtrOffset(FBlocks[0], StartPos), @ResultStr[1], FoundPos - StartPos + AdvLenth);
        end;

        if SubStrLen > FBlockSize then
          raise XmlException.Create('XML tag size is too long');

        Dec(FStreamPosition, (SI - FoundSI) * FActualBlockSize);

        FCurPosition := FoundPos;
        FActualPosition := FoundPos;
        Result := True;
        Exit;
      end;
      Inc(I);
      Inc(SubIndex);
    end;
    if not LoadNextBlock(False) then
      Exit;
    Inc(SI);
    I := 0;
  end;
end;

procedure XmlTextReader.FreeOldBlocks;
var
  i: integer;
begin
  if FBlocks.Count > 1 then begin
    Assert(FStream <> nil);
    FBlocks.Exchange(0, FBlocks.Count - 1);
    for i := FBlocks.Count - 1 downto 1 do begin
      FreeMem(FBlocks[i], FBlockSize);
      FBlocks.Delete(i);
    end;
  end;
end;

constructor XmlTextReader.Create(Stream: TStream);
begin
  inherited Create;
  FBlocks := TList.Create;
  FStream := Stream;
  FStream.Position := 0;
  FActualBlockSize := 0;
  FStreamPosition := 0;
  FFullSize:=FStream.Size;
  FBlockSize := 4096;
  FMaxNumBlock := (FFullSize div FBlockSize) + 1;
  FActualPosition := 0;
  FCurPosition := 0;
  LoadNextBlock;
  InitInstance;
end;

constructor XmlTextReader.Create(const Str: AnsiString);
begin
  inherited Create;
  FBlocks := TList.Create;
  FBlocks.Add(@Str[1]); // Count of blocks always  = 1
  FStream := nil; // When  FStream = nil  procedure NormalizeBuffer not working
  FFullSize := Length(Str);
  FActualBlockSize := FFullSize;
  FStreamPosition := FFullSize;
  FBlockSize := FFullSize;
  FMaxNumBlock := 1;
  FActualPosition := 0;
  FCurPosition := 0;
  InitInstance;
end;

destructor XmlTextReader.Destroy;
  var i: integer;
begin
  if FStream <> nil then
    for i := FBlocks.Count - 1 downto 0 do begin
      FreeMem(FBlocks[i], FBlockSize);
      FBlocks.Delete(i);
    end;
  FBlocks.Free;
  FAttrNames.Free;
  FAttrPrefix.Free;
  FAttrValues.Free;

  inherited;
end;

function XmlTextReader.Read: boolean;
var
  EndTagName, Value: AnsiString;
  IsTextFound: Boolean;
  a: AnsiChar;
begin
  Result := False;

  FAttrNames.Clear;
  FAttrValues.Clear;
  FAttrPrefix.Clear;

  if FState in [Initial, Interactive] then begin
    IsTextFound := False;
    if NodeType = ntElement then begin
      a := GetNextSymbol;
      while (a <> '<') and not Eof do begin
        if a in ['a'..'z', 'A'..'Z', '0'..'9', '.', ':'] then begin
          IsTextFound := True;
          break;
        end;
        a := GetNextSymbol;
      end;
      if IsTextFound then begin
        FNodeType := ntText;
      end;
    end;

    if not IsTextFound then begin
      if not MoveTo('<') then begin
        if Eof then
          Exit;
        FState := Error;
        XmlException.Create('Root element missing');
      end;

      if IsToken('?') then
        FNodeType := ntDeclaration
      else
      if IsToken('!--') then
        FNodeType := ntComment
      else if IsToken('!DOCTYPE') then
        FNodeType := ntDocumentType
      else if IsToken('/') then begin
        FNodeType := ntEndElement;
      end
      else
        FNodeType := ntElement;
    end;
    FState := Interactive;
  end;

  FName := '';
  FValue := '';
  FPrefix := '';

  case FNodeType of
    ntDeclaration: begin
      ReadTo (' ', Value);
      FName := _string(Trim(Value));
      if  not ReadTo('?>', Value) then
      begin
        FState := Error;
        raise XmlException.Create('Invalid declaration tag');
      end;
      FValue := _string(Trim(Value));
      Result := True;
    end;

    ntComment: begin
      if not ReadTo('-->', Value) then
      begin
        FState := Error;
        raise XmlException.Create('Invalid comment tag');
      end;
      FValue := _string(Trim(Value));
      Result := True;
    end;

    ntDocumentType: begin
      if not ReadTo('[<', Value) then begin
        FState := Error;
        raise XmlException.Create('Invalid Document type tag');
      end;
      FName := _string(Trim(Value));
      ReadTo('>]', Value);
      FValue := _string(Trim(Value));
      Result := True;
    end;

    ntEndElement: begin
      Result := ReadTo('>', Value);
      FName := _string(Trim(Value));
    end;

    ntElement: begin
      if ReadTo('>', Value, 1) then begin
        FName := _string(Trim(Copy(Value, 1, Pos(AnsiString(' '), Value) - 1)));
        if FName = '' then
          FName := _string(Trim(Copy(Value, 1, Pos(AnsiString('>'), Value) - 1)));

        Delete(Value, 1, Length(FName));
        GetXMLNodeAttributes(UTF8Decode(Value), FAttrNames, FAttrValues);

        FCurrElementName := FName;
        if FName[Length(FName)] = '/' then
          Delete(FName, Length(FName), 1);

        Result := True;
      end;
    end;

    ntText: begin
      EndTagName := AnsiString(FCurrElementName);
      DeleteInvisibleSymbol(EndTagName);
      EndTagname := '</'+EndTagName+'>';

      if FCurrElementName[Length(FCurrElementName)] <> '/' then begin
        Inc(FActualPosition);
        ReadTo(EndTagName, Value);
        FValue := XMLDecode(UTF8Decode(Trim(Value)));
      end;
      Result := True;
    end;
  else
    begin
     FState := Error;
     Assert(False);
    end;
  end;
end;

procedure XmlTextReader.GetXMLNodeAttributes(const Node: _string; AttrNames, AttrValues: _TStrings);
var
  Parser: TXmlParser;
  Code: integer;
  Lexem, AttrName, AttrValue, AttrPrefix: _string;
  WithColon: boolean;
begin
  Parser := TXmlParser.Create(Node);
  try
    while True do begin
      Code := Parser.GetNextIdent(Lexem);
      case Code of
        lcEnd, 18, 20: // '<', '>'
          break;
        lcIdent: begin
          WithColon := False;
          AttrName := Lexem;
          AttrPrefix := '';
          Code := Parser.GetNextIdent(Lexem);
          if Code = lcIdent then begin
            AttrName := Lexem;
            Code := Parser.GetNextIdent(Lexem);
          end;
          if Code in [16, 19] then begin
            if Code = 16 then begin // ':'
              Code := Parser.GetNextIdent(Lexem);
              if Code <> lcIdent then
                raise XmlException.Create(SInvalidXML);
              AttrPrefix := AttrName;
              AttrName := AttrPrefix + ':' + Lexem;
              Code := Parser.GetNextIdent(Lexem);
              WithColon := True;
            end;

            if Code = 19 then begin // '='
              Code := Parser.GetNext(Lexem);
              if (Code <> lcIdent) and (Code <> lcString) then
                raise XmlException.Create(SInvalidXML);
              AttrValue := XMLDecode(Lexem);
              if WithColon and (LowerCase(AttrName) = 'name') then
                AttrName := ':' + AttrName;
              AttrNames.Add(AttrName);
              AttrValues.Add(AttrValue);
              FAttrPrefix.Add(AttrPrefix);
            end;
          end;
        end;
      end;
    end;
  finally
    Parser.Free;
  end;
  Assert(AttrNames.Count = AttrValues.Count);
end;

function XMLEncode(const AStr: WideString): WideString;
var
  sb: WideStringBuilder;
begin
  sb := WideStringBuilder.Create(AStr, Length(AStr));
  try
    sb.Replace('&', '&#x26;');
    sb.Replace('''', '&#x27;');
    sb.Replace('"', '&#x22;');
    sb.Replace('<', '&#x3c;');
    sb.Replace('>', '&#x3e;');
    Result := sb.ToString;
  finally
    sb.Free;
  end;
end;

function XMLDecode(const AStr: _string): _string;
var
  sb: _StringBuilder;
begin
  sb := _StringBuilder.Create(AStr, Length(AStr));
  try
    sb.Replace('&#x27;', '''');
    sb.Replace('&#x39;', '''');
    sb.Replace('&#x92;', '''');
    sb.Replace('&#x96;', '''');
    sb.Replace('&#x22;', '"');
    sb.Replace('&#x3c;', '<');
    sb.Replace('&#x3e;', '>');
    sb.Replace('&#x26;', '&');
    sb.Replace('&amp;', '&');
    sb.Replace('&quot;', '"');
    sb.Replace('&gt;', '>');
    sb.Replace('&lt;', '<');
    sb.Replace('&#13;', #13);
    sb.Replace('&#10;', #10);
    Result := sb.ToString;
  finally
    sb.Free;
  end;
end;


function XmlTextReader.Items(const Index: Integer): _string;
begin
  Result := FAttrValues[Index];
end;

function XmlTextReader.Items(const AttrName: _string): _string;
begin
  Result := Items(FAttrNames.IndexOf(AttrName));
end;

function XmlTextReader.GetAttributeCount: integer;
begin
  Result := FAttrNames.Count;
end;

procedure XmlTextReader.MoveToAttribute(i: integer);
begin
  try
    FName := FAttrNames[i];
    FValue := FAttrValues[i];
    FPrefix := FAttrPrefix[i];
  except
    raise XmlException.Create(Format('Attribute not found (%d)', [i]));
  end;
end;

function XmlTextReader.MoveToAttribute(name: _string): Boolean;
begin
  try
    MoveToAttribute(FAttrNames.IndexOf(name));
    Result := True;
  except
    Result := False;
  end;
end;

function XmlTextReader.GetDepth: integer;
begin
  Result := 0;
end;

function XmlTextReader.GetHasAttributes: Boolean;
begin
  Result := FAttrNames.Count > 0;
end;

procedure DeleteInvisibleSymbol(var s: AnsiString);
var
  i: Integer;
begin
  for i := Length(s) downto 1 do
    if not ((s[i] in ['a'..'z']) or (s[i] <> '.') or (s[i] <> ':')) then
      Delete(s, i, 1);
end;

procedure XmlTextReader.InitInstance;
begin
  FState := Initial;
  FNodeType := ntNone;
  FOffset := 1;
  FAttrNames := _TStringList.Create;
  FAttrPrefix := _TStringList.Create;
  FAttrValues := _TStringList.Create;
  FCurrElementName := '';
end;

function XmlTextReader.GetEof: Boolean;
begin
  Result := (((FStreamPosition - FActualBlockSize) + FActualPosition) >= (FFullSize)) or
            (FCurPosition > FActualBlockSize);
end;

{ XmlTextWriter }

constructor XmlTextWriter.Create(w: StreamWriter);
begin
  FFormatting := fmtNone;
  FIndentation := 2;
  FIndentChar := ' ';
  FQuoteChar := '"';
  FWriteState := wsStart;
  FDepth := 0;
  FPosStack := TStack.Create;
  FTagStack := _TStringList.Create;
  FWriter := w;
end;

destructor XmlTextWriter.Destroy;
begin
  FPosStack.Free;
  FTagStack.Free;

  inherited;
end;

procedure XmlTextWriter.InternalWriteStartElement(const Prefix, LocalName, ns: _string);
var
  EndTagPos: Integer;
begin
  InternalCloseStartTag;
  if FWriteState = wsContent then
    FlushData;

  if FDepth > 0 then
    FText := FText + LineSeparator;

  if Prefix <> '' then
    FText := FText + IndentStr + '<' + Prefix + ':' + LocalName
  else
    FText := FText + IndentStr + '<' + LocalName;

  if ns <> '' then
    if Prefix <> '' then
      FText := FText + ' ' + 'xmlns:' + Prefix + '=' + FQuoteChar + ns + FQuoteChar
    else
      FText := FText + ' ' + 'xmlns=' + FQuoteChar + ns + FQuoteChar;

  EndTagPos := Length(FText);

  inc(FDepth);

  FPosStack.Push(Pointer(EndTagPos));
  if Prefix <> '' then
    PushTagName(Prefix + ':' + LocalName)
  else
    PushTagName(LocalName);

  FWriteState := wsElement;
end;

procedure XmlTextWriter.InternalWriteElementString(const LocalName, ns: _string; Value: WideString);
begin
  InternalCloseStartTag;
  if FWriteState = wsElement then
    inc(FDepth);
  FText := FText + LineSeparator;
  FText := FText + IndentStr + '<' + LocalName;

  if ns <> '' then
    FText := FText + ' xmlns=' + FQuoteChar + ns + FQuoteChar + '>'
  else
    FText := FText + '>';

  if Value <> '' then
    FText := FText + XMLEncode(Value);
  FText := FText + '</' + LocalName + '>';
  if FWriteState = wsElement then
    dec(FDepth);
  FlushData;
  FWriteState := wsContent;
end;

procedure XmlTextWriter.InternalWriteAttributeString(const Prefix, LocalName, ns: _string; Value: WideString);
var
  AttrPos: Integer;
  AttrStr: WideString;
begin
  if FWriteState in [wsElement, wsAttribute] then
    AttrPos := Integer(FPosStack.Pop) + 1
  else
    raise XmlException.Create('Token WriteAttributeString in state Content would result in an invalid XML document.');

  if Prefix <> '' then
    AttrStr := ' ' + Prefix + ':'
  else
    AttrStr := ' ';

  AttrStr := AttrStr + LocalName + '=' + FQuoteChar + XMLEncode(Value) + FQuoteChar;// + ' ';

  Insert(AttrStr, FText, AttrPos);
                                                           
  AttrPos := AttrPos + Length(AttrStr);

  if ns <> '' then begin
    AttrStr := 'xmlns:' + Prefix + '=' + FQuoteChar + ns + FQuoteChar;// + ' ';
    Insert(AttrStr, FText, AttrPos);
    AttrPos := AttrPos + Length(AttrStr);
  end;

  FPosStack.Push(Pointer(AttrPos-1));
  FWriteState := wsAttribute;
end;

procedure XmlTextWriter.FlushData;
begin
  FWriter.Write(FText);
  FText := '';
end;

procedure XmlTextWriter.Close;
begin
  FlushData;
end;

procedure XmlTextWriter.WriteStartElement(const LocalName: _string);
begin
  InternalWriteStartElement('', LocalName, '');
end;

procedure XmlTextWriter.WriteStartElement(const Prefix, LocalName, ns: _string);
begin
  if (Prefix <> '') and (ns = '') then
    raise XmlException.Create('Cannot use a prefix with an empty namespace.');

  InternalWriteStartElement(Prefix, LocalName, ns);
end;

procedure XmlTextWriter.WriteStartElement(const LocalName, ns: _string);
begin
  InternalWriteStartElement('', LocalName, ns);
end;

procedure XmlTextWriter.WriteEndElement;
begin
  dec(FDepth);
  if (FWriteState = wsAttribute) then begin
    FText := FText + ' />';
    PopTagName;
    FWriteState := wsContent;
  end
  else
    InternalWriteEndElement;
  FlushData;
end;

procedure XmlTextWriter.InternalWriteEndElement;
var
  Len: Integer;
begin
  InternalCloseStartTag;
  Len := Length(FText);
  if (FText = '') or ((Len >= 2) and (FText[Len] <> #10) and (FText[Len-1] <> #13)) then
    FText := FText + LineSeparator + IndentStr + '</' + PopTagName + '>'
  else
    FText := FText + '</' + PopTagName + '>';
  FWriteState := wsContent;
end;

procedure XmlTextWriter.WriteFullEndElement;
begin
  dec(FDepth);
  InternalWriteEndElement;
  FlushData;
end;

procedure XmlTextWriter.WriteString(const Text: WideString);
begin
  InternalCloseStartTag;
  FText := FText + Text;
  FWriteState := wsContent;
end;

procedure XmlTextWriter.WriteElementString(const LocalName, ns: _string; Value: WideString);
begin
  InternalWriteElementString(LocalName, ns, Value);
end;

procedure XmlTextWriter.WriteElementString(const LocalName: _string; Value: WideString);
begin
  InternalWriteElementString(LocalName, '', Value);
end;

procedure XmlTextWriter.WriteAttributeString(const LocalName: _string; Value: WideString);
begin
  InternalWriteAttributeString('', LocalName, '', Value);
end;

procedure XmlTextWriter.WriteAttributeString(const Prefix, LocalName, ns: _string; Value: WideString);
begin
  if (Prefix <> '') and (ns = '') then
    raise XmlException.Create('Cannot use a prefix with an empty namespace.');

  InternalWriteAttributeString(Prefix, LocalName, ns, Value);
end;

function XmlTextWriter.IndentStr: WideString;
var
  i: Integer;
begin
  Result := '';
  if (FFormatting = fmtIndented) and (FDepth <> 0) then
    for i := 1 to FDepth * FIndentation do
      Result := Result + FIndentChar;
end;

function XmlTextWriter.PopTagName: _string;
begin
  if FTagStack.Count = 0 then
    raise XmlException.Create('There was no XML start tag open.');
  Result := FTagStack[FTagStack.Count-1];
  FTagStack.Delete(FTagStack.Count-1);
end;

procedure XmlTextWriter.PushTagName(const TagName: _string);
begin
  FTagStack.Add(TagName);
end;

procedure XmlTextWriter.InternalCloseStartTag;
begin
  if FWriteState in [wsElement, wsAttribute] then begin
    FPosStack.Pop;
    FText := TrimRight(FText);
    FText := FText + '>';
  end;
end;

{ StreamWriter }

constructor StreamWriter.Create(const path: string; Append: Boolean);
begin
  inherited Create;

  if FileExists(path) and Append then begin
    FStream := TFileStream.Create(path, fmOpenReadWrite);
    FStream.Seek(0, soFromEnd);
  end
  else
    FStream := TFileStream.Create(path, fmCreate);
  FReleaseStream := True;
  FEncoding := Encoding.Default;
end;

constructor StreamWriter.Create(output: TStream; aEncoding: Encoding);
begin
  inherited Create;

  FStream := output;
  FReleaseStream := False;
  FEncoding := aEncoding;
end;

destructor StreamWriter.Destroy;
begin
  if FReleaseStream then
    FStream.Free;

  inherited;
end;

procedure StreamWriter.Close;
begin
end;

procedure StreamWriter.Flush;
begin
end;

procedure StreamWriter.Write(const value: WideString);
var
  bytes: TBytes;
begin
  bytes := FEncoding.{$IFNDEF VER5}GetBytes{$ELSE}GetBytesWide{$ENDIF}(value);
  FStream.Write(Pointer(bytes)^, Length(bytes));
end;

procedure StreamWriter.WriteLine(const value: WideString);
begin
  Write(value + LineSeparator);
end;

{ TXmlParser }

constructor TXmlParser.Create(const Text: _string);
begin
  inherited;

  FSymbolLexems := XmlSymbolLexems;
  FSymbolChars := XmlSymbolChars;

  FKeywordLexems := XmlKeywordLexems;
  FOmitKeywords := True; // XmlKeywordLexems.Count = 0
end;

function TXmlParser.GetNextIdent(out Lexem: _string): integer;
var
  i: integer;
begin
{$IFDEF PERF_COUNTER}
  PerfCounters[2].Start;//}
  try
{$ENDIF}
  Result := InternalGetNext; // Result is not in KeywordLexems

  if Result = lcIdent then begin// Optimize
    if Result > 0 then begin
      Assert(FLexem <> '');
      Lexem := FLexem
    end
    else begin
      if FLexemLength = 0 then
        Lexem := ''
      else
      begin
        Assert(FLexemPos > 0);
        Lexem := CopyText(FLexemPos, FLexemLength);
      end;
    end;
  end;

  if not FOmitKeywords and (Result = lcIdent) then begin
    Assert(FLexemPos > 0);
    Assert(FLexemLength > 0);
    i := FindLexemIndex(FLexemPos, FLexemLength, FKeywordLexems, []);
    if i <> -1 then begin
      Result := Integer(FKeywordLexems.Objects[i]);
      Assert(Result > 0);
      if Uppered then
        Lexem := _UpperCase(Lexem);  //WAR problem with macros as key words
    end;
  end;

  Assert(Result <> - MaxInt);
{$IFDEF PERF_COUNTER}
  finally
    PerfCounters[2].Stop;
  end;
{$ENDIF}
end;

var
  i: integer;
initialization
  XmlSymbolLexems := _TStringList.Create;
  XmlKeywordLexems := _TStringList.Create;

  XmlSymbolLexems.AddObject(':', TObject(Integer(16)));
  XmlSymbolLexems.AddObject('=', TObject(Integer(19)));
  XmlSymbolLexems.CustomSort(CRCmpStrings);

  // Performance optimization
  XmlSymbolChars := [];
  for i := 0 to XmlSymbolLexems.Count - 1 do
    XmlSymbolChars := XmlSymbolChars + [AnsiChar(XmlSymbolLexems[i][1])];

finalization
  XmlSymbolLexems.Free;
  XmlKeywordLexems.Free;

end.
