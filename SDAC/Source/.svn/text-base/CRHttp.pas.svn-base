//////////////////////////////////////////////////
//  Copyright © 1998-2012 Devart. All right reserved.
//////////////////////////////////////////////////

{$IFNDEF CLR}

{$I Dac.inc}

unit CRHttp;
{$ENDIF}

interface

uses
  Classes, SysUtils, SyncObjs,
{$IFDEF CLR}
  System.Text,
{$ELSE}
  CLRClasses,
{$ENDIF}
  CRTypes, CRDataBuffer, CRBase64, CRVioTcp;

const
  Prtcl_http = ' HTTP/1.0';
  Prox_Head = 'Proxy-Connection: Keep-Alive';
  Prox_Host = 'Host: ';
  Prox_Port = 'Port: ';
  Host_Accept1 = 'Accept: www/source, text/html, video/mpeg, image/jpeg, image/x-tiff';
  Host_Accept2 = 'Accept: image/x-rgb, image/x-xbm, image/gif, */*, application/postscript';
  Host_UserAgent = 'User-Agent';
  Host_Cache = 'Cache-Control: no-store,no-cache';
  Host_Pragma = 'Pragma: no-cache';
  Head_From = 'From';
  Head_Host = 'Host';
  Head_Cookie = 'Cookie';
  Head_Referer = 'Referer';
  Head_Content1 = 'Content-Type: application/x-www-form-urlencoded';
  Head_Content2 = 'Content-Type: application/octet-stream';
  Head_ContentLength = 'Content-Length: ';
  Head_SetCookie = 'SET-COOKIE:';
  Head_ContentLength2 = 'CONTENT-LENGTH:';
  Head_Location = 'LOCATION:';

  CRLF = #13#10;
  BUF_SIZE = 32 * 1024;

type
  HttpMethod = (hmGET, hmOPTIONS, hmHEAD, hmPOST, hmPUT, hmTRACE);

  THeaderInfo = class(TPersistent)
  private
    FLocalAddress: string;
    FLocalProgram: string;
    FCookie: string;
    FReferer: string;
    FUserId: string;
    FPassword: string;
    FProxyUserId: string;
    FProxyPassword: string;
  public
    property LocalMailAddress: string read FLocalAddress write FLocalAddress;
    property LocalProgram: string read FLocalProgram write FLocalProgram;
    property Cookie: string read FCookie write FCookie;
    property Referer: string read FReferer write FReferer;
    property UserId: string read FUserId write FUserId;
    property Password: string read FPassword write FPassword;
    property ProxyUserId: string read FProxyUserId write FProxyUserId;
    property ProxyPassword: string read FProxyPassword write FProxyPassword;
  end;

  TCRHttp = class(TPersistent)
  private
    FHeader: string;
    FLocation: string;
    FCookieIn: string;
    FSendData: AnsiString;
    FURL: string;
    FRequestMethod: HttpMethod;
    FSendHeader: TStringList;
    FHeaderInfo: THeaderInfo;

    // URL specifics
    FScheme: string;
    FUser: string;
    FPassword: string;
    FNetworkLocation: string;
    FPort: string;
    FQuery: string;
    FResource: string;
    FParameters: string;
    FPath: string;
    FFragment: string;
    FReplyNumber: Smallint;
    FReplyMessage: string;
    FBytesTotal: Longint;

    FVioTcp: TCRVioTcp;
    FTimeout: Integer;
    FPortNo: Integer;
    FProxy: string;
    FProxyPort: Integer;
    FLock: TCriticalSection;

    FReadDataBuffer: TDataBuffer;
    FWriteDataBuffer: TDataBuffer;
    FBuffer: TBytes;
    FKeepAlive: Boolean;
    FOnAuthenticationNeeded: TNotifyEvent;

    function GetToEnd(const FindChar: Char; var ParseString: string; const KeepFirst: Boolean): string;
    function ParseFragment(var ParseString: string): string;
    function ParseScheme(var ParseString: string): string;
    function ParseNetworkLocation(var ParseString: string): string;
    function ParseQuery(var ParseString: string): string;
    function ParseParameters(var ParseString: string): string;
    function ParseResource(var ParseString: string): string;
    function ParsePath(var ParseString: string): string;
    function ParsePassword(var ParseString: string): string;
    function ParseUserPassword(var ParseString: string): string;
    function ParsePort(var ParseString: string): string;
    procedure ParseURL(const URL: string; var FScheme, FUser, FPassword,
      FNetworkLocation, FPort, FPath, FResource, FParameters, FQuery, FFragment: string);

  protected
    function GetTimeout: Integer;
    procedure SetTimeout(Value: Integer);
    procedure HTTPConnect;
    procedure SetHTTPHeaders;
    procedure RetrieveHeaders;
    procedure SendHTTPRequest;

  public
    constructor Create(const URL: string);
    destructor Destroy; override;

    procedure Get(const URL: string); overload;
    procedure Get; overload;
    procedure Post(const URL: string; const PostData: AnsiString); overload;
    procedure Post; overload;
    procedure Head(const URL: string);
    procedure Options(const URL: string);
    procedure Put(const URL: string; const PutData: AnsiString);
    procedure Trace(const URL: string; const TraceData: AnsiString);

    procedure Abort;
    procedure Disconnect;
    function ReadLine: string;
    function Read(const Buffer: TBytes; Offset, Count: integer): integer;
    function WaitForData(Timeout: integer = -1): boolean;

    property CookieIn: string read FCookieIn;
    property ReplyNumber: Smallint read FReplyNumber;
    property ReplyMessage: string read FReplyMessage;
    property BytesTotal: Longint read FBytesTotal;
    property Header: string read FHeader write FHeader;
    property HeaderInfo: THeaderInfo read FHeaderInfo write FHeaderInfo;
    property WriteData: TDataBuffer read FWriteDataBuffer;

    property Proxy: string read FProxy write FProxy;
    property ProxyPort: Integer read FProxyPort write FProxyPort;

    property KeepAlive: Boolean read FKeepAlive write FKeepAlive;
    property Timeout: Integer read GetTimeout write SetTimeout;

    property OnAuthenticationNeeded: TNotifyEvent read FOnAuthenticationNeeded write FOnAuthenticationNeeded;
  end;

  HttpException = class(Exception);

function NthWord(const InputString: string; const Delimiter: Char; Number: Integer): string;
function WordsCount(const InputString: string; const Delimiter: Char): Integer;

implementation

const
  PortDefault = '80';
  SchemeDefault = 'http:';
  PORTS: array[1..16, 1..2] of string = (
    ('ftp:', '21'),
    ('telnet:', '23'),
    ('smtp:', '25'),
    ('whois:', '43'),
    ('whois++:', '63'),
    ('gopher:', '70'),
    ('http:', '80'),
    ('pop3:', '110'),
    ('nntp:', '119'),
    ('news:', '119'),
    ('imap2:', '143'),
    ('irc:', '194'),
    ('wais:', '210'),
    ('imap3:', '220'),
    ('ldap:', '389'),
    ('https:', '443')
  );

function DefaultPort(const Scheme: string): string;
var
  i: Integer;
begin
  Result := PortDefault;

  if Scheme <> '' then
    for i := Low(PORTS) to High(PORTS) do
      if AnsiCompareText(Scheme, PORTS[i, 1]) = 0 then begin
        Result := PORTS[i, 2];
        break;
      end;
end;

constructor TCRHttp.Create(const URL: string);
begin
  inherited Create;

  FLock := TCriticalSection.Create;
  FHeaderInfo := THeaderInfo.Create;
  FSendHeader := TStringList.Create;
{$IFDEF VER7P}
  FSendHeader.NameValueSeparator := ':';
{$ENDIF}
  FProxyPort := 8080;
  FTimeout := 15;

  SetLength(FBuffer, BUF_SIZE);
  FReadDataBuffer := TDataBuffer.Create(BUF_SIZE);
  FWriteDataBuffer := TDataBuffer.Create(BUF_SIZE);

  FKeepAlive := True;
  FURL := URL;
end;

destructor TCRHttp.Destroy;
begin
  try
    Disconnect;
  finally
    FHeaderInfo.Free;
    FSendHeader.Free;
    FReadDataBuffer.Free;
    FWriteDataBuffer.Free;
    FLock.Free;
    FVioTcp.Free;
  end;

  inherited;
end;

function TCRHttp.GetTimeout: Integer;
begin
  Result := FTimeout;
end;

procedure TCRHttp.SetTimeout(Value: Integer);
begin
  if FTimeout <> Value then begin
    FTimeout := Value;
    if FVioTcp <> nil then
      FVioTcp.Timeout := Value;
  end;
end;

procedure TCRHttp.Get(const URL: string);
begin
  FRequestMethod := hmGET;
  FURL := URL;
  HTTPConnect;
end;

procedure TCRHttp.Get;
begin
  Assert(FURL <> '');
  FRequestMethod := hmGET;
  HTTPConnect;
end;

procedure TCRHttp.Post(const URL: string; const PostData: AnsiString);
begin
  FRequestMethod := hmPOST;
  FURL := URL;
  FSendData := PostData;
  HTTPConnect;
end;

procedure TCRHttp.Post;
begin
  Assert(FURL <> '');
  FRequestMethod := hmPOST;
  FSendData := '';
  HTTPConnect;
end;

procedure TCRHttp.Head(const URL: string);
begin
  FRequestMethod := hmHEAD;
  FURL := URL;
  HTTPConnect;
end;

procedure TCRHttp.Options(const URL: string);
begin
  FRequestMethod := hmOPTIONS;
  FURL := URL;
  HTTPConnect;
end;

procedure TCRHttp.Put(const URL: string; const PutData: AnsiString);
begin
  FRequestMethod := hmPUT;
  FURL := URL;
  FSendData := PutData;
  HTTPConnect;
end;

procedure TCRHttp.Trace(const URL: string; const TraceData: AnsiString);
begin
  FRequestMethod := hmTRACE;
  FURL := URL;
  FSendData := TraceData;
  HTTPConnect;
end;

procedure TCRHttp.Abort;
begin
  FLock.Acquire;
  try
    if FVioTcp <> nil then
      FVioTcp.Close;
  finally
    FLock.Release;
  end;
end;

procedure TCRHttp.Disconnect;
begin
  if FVioTcp <> nil then
    FVioTcp.Close;

  FReadDataBuffer.Clear;
  FWriteDataBuffer.Clear;

  FLock.Acquire;
  try
    FreeAndNil(FVioTcp);
  finally
    FLock.Release;
  end;
end;

function TCRHttp.WaitForData(Timeout: integer = -1): boolean;
begin
  Result := FReadDataBuffer.DataLength > 0;

  if not Result and (FVioTcp <> nil) then
    Result := FVioTcp.WaitForData(Timeout);
end;

function TCRHttp.ReadLine: string;
var
  buf: TBytes;
  pos, cnt: Integer;
begin
  Result := '';
  if FVioTcp = nil then
    Exit;

  repeat
    pos := FReadDataBuffer.SearchFromIndex(10, 0);
    if pos < 0 then begin
      cnt := FVioTcp.ReadNoWait({$IFDEF CLR}FBuffer{$ELSE}@FBuffer[0]{$ENDIF}, 0, Length(FBuffer));
      if cnt <= 0 then begin
        if FVioTcp.LastError <> '' then
          raise HttpException.Create(FVioTcp.LastError)
        else
          raise HttpException.Create('There is no enough data to read');
      end;

      FReadDataBuffer.Write(FBuffer, 0, cnt);
    end;
  until pos >= 0;

  if pos >= 0 then begin
    SetLength(buf, pos + 1);
    FReadDataBuffer.Read(buf, 0, Length(buf));
    Result := string(Encoding.Default.GetString(buf));
  end;
end;

function TCRHttp.Read(const Buffer: TBytes; Offset, Count: integer): integer;
var
  cnt: Integer;
begin
  if FVioTcp = nil then begin
    Result := 0;
    Exit;
  end;

  while FReadDataBuffer.DataLength < Count do begin
    if (FReadDataBuffer.DataLength = 0) or FVioTcp.WaitForData(0) then
      cnt := FVioTcp.ReadNoWait({$IFDEF CLR}FBuffer{$ELSE}@FBuffer[0]{$ENDIF}, 0, Length(FBuffer))
    else
      cnt := 0;

    if cnt <= 0 then
      Break;
    FReadDataBuffer.Write(FBuffer, 0, cnt);
  end;

  if FReadDataBuffer.DataLength < Count then
    Result := FReadDataBuffer.DataLength
  else
    Result := Count;
  FReadDataBuffer.Read(Buffer, Offset, Result);
end;

procedure TCRHttp.HTTPConnect;
var
  tmp: string;
begin
  repeat
    ParseURL(FURL, FScheme, FUser, FPassword, FNetworkLocation, FPort, FPath,
      FResource, FParameters, FQuery, FFragment);

    if (FUser <> '') or (FPassword <> '') then begin
      FHeaderInfo.FUserId := FUser;
      FHeaderInfo.FPassword := FPassword;
    end;

    if FPort = '' then
      FPortNo := StrToInt(DefaultPort(FScheme))
    else
      FPortNo := StrToInt(Copy(FPort, 2, Length(FPort)));

    SetHTTPHeaders;

    try
      Assert(FVioTcp = nil);

      FLock.Acquire;
      try
        if Proxy = '' then
          FVioTcp := TCRVioTcp.Create(AnsiString(FNetworkLocation), FPortNo)
        else
          FVioTcp := TCRVioTcp.Create(AnsiString(Proxy), ProxyPort);
      finally
        FLock.Release;
      end;

      FVioTcp.Timeout := FTimeout;
      FVioTcp.Connect;

      FReplyNumber := 0;
      SendHTTPRequest;
      RetrieveHeaders;

      if (ReplyNumber >= 300) and (ReplyNumber <= 302) then
        FURL := FLocation
      else
      if ReplyNumber > 399 then
        raise HttpException.Create(Trim(ReplyMessage));

    finally
      if (not FKeepAlive) or ((ReplyNumber >= 299) and (ReplyNumber <= 399)) then
        Disconnect;
    end;

    if (ReplyNumber > 299) and (ReplyNumber < 399) then begin
      if CookieIn <> '' then
        FHeaderInfo.Cookie := CookieIn;

      FRequestMethod := hmGET;

      //FURL := 'http://' + Host + '/' + FURL;
      if Pos('//', FURL) = 0 then
        if Pos('/', FURL) <> 1 then begin
          tmp := FURL;
          FURL := FScheme + '//' + FNetworkLocation;

          if FPort <> '' then
            FURL := FURL + FPort;

          FURL := FURL + FPath + tmp;
        end;
    end;

    if ReplyNumber = 401 then
      if Assigned(FOnAuthenticationNeeded) then
        FOnAuthenticationNeeded(Self);

  until (ReplyNumber < 299) or (ReplyNumber > 399);
end;

procedure TCRHttp.SetHTTPHeaders;
var
  HttpMethodStr: string;
  Selector: string;
  s: string;
  AuthBuf: TBytes;
  i: Integer;
begin
  FSendHeader.Clear;

  case FRequestMethod of
    hmGET:
      HttpMethodStr := 'GET ';
    hmOPTIONS:
      HttpMethodStr := 'OPTIONS ';
    hmPOST:
      HttpMethodStr := 'POST ';
    hmPUT:
      HttpMethodStr := 'PUT ';
    hmHEAD:
      HttpMethodStr := 'HEAD ';
    hmTRACE:
      HttpMethodStr := 'TRACE ';
  end;

  if Proxy <> '' then begin
    i := Pos(' ', FURL);
    while i > 0 do begin
      FURL[i] := '+';
      i := Pos(' ', FURL);
    end;

    FSendHeader.Add(HttpMethodStr + FURL + Prtcl_http);
    FSendHeader.Add(Prox_Head);
    FSendHeader.Add(Prox_Host + FNetworkLocation);
    FSendHeader.Add(Prox_Port + IntToStr(FPortNo));
  end
  else begin
    Selector := FPath + FResource + FParameters + FQuery + FFragment;

    i := Pos(' ', Selector);
    while i > 0 do begin
      Selector[i] := '+';
      i := Pos(' ', Selector);
    end;

    FSendHeader.Add(HttpMethodStr + Selector + Prtcl_http);
  end;

  // Send acceptable reply types
  FSendHeader.Values[Head_Host] := FNetworkLocation;
//  FSendHeader.Add(Host_Accept1);
//  FSendHeader.Add(Host_Accept2);
  FSendHeader.Add(Host_Cache);
  FSendHeader.Add(Host_Pragma);

  if FHeaderInfo.FLocalAddress <> '' then
    FSendHeader.Values[Host_UserAgent] := FHeaderInfo.FLocalAddress;

  if FHeaderInfo.FLocalProgram <> '' then
    FSendHeader.Values[Head_From] := FHeaderInfo.FLocalProgram;

  if FHeaderInfo.FCookie <> '' then
    FSendHeader.Values[Head_Cookie] := FHeaderInfo.FCookie;

  if FHeaderInfo.FReferer <> '' then
    FSendHeader.Values[Head_Referer] := FHeaderInfo.FReferer;

  SetLength(AuthBuf, 0);
  if (FHeaderInfo.FUserId <> '') and (FHeaderInfo.Fpassword <> '') then begin
    s := FHeaderInfo.FUserId + ':' + FHeaderInfo.Fpassword;
    AuthBuf := TBase64.Encode(Encoding.Default.GetBytes(s));
    FSendHeader.Values['Authorization'] := 'Basic ' + string(Encoding.Default.GetString(AuthBuf));
  end;

  if (FHeaderInfo.FProxyUserId <> '') and (FHeaderInfo.FProxyPassword <> '') then begin
    s := FHeaderInfo.FProxyUserId + ':' + FHeaderInfo.FProxyPassword;
    AuthBuf := TBase64.Encode(Encoding.Default.GetBytes(s));
    FSendHeader.Values['Proxy-Authorization'] := 'Basic ' + string(Encoding.Default.GetString(AuthBuf));
  end;

  case FRequestMethod of
    hmPOST, hmPUT, hmTRACE: begin
      FSendHeader.Add(Head_Content2);
      if FWriteDataBuffer.DataLength > 0 then
        FSendHeader.Add(Head_ContentLength + IntToStr(FWriteDataBuffer.DataLength))
      else
        FSendHeader.Add(Head_ContentLength + IntToStr(Length(FSendData)));
    end;
  end;
end;

procedure TCRHttp.SendHTTPRequest;
var
  sHeader: AnsiString;
  cnt: Integer;
begin
  Assert(FVioTcp <> nil);

  sHeader := AnsiString(FSendHeader.Text);
  FVioTcp.Write({$IFDEF CLR}Encoding.Default.GetBytes{$ELSE}TValueArr{$ENDIF}(sHeader), 0, Length(sHeader));
  FVioTcp.Write({$IFDEF CLR}Encoding.Default.GetBytes{$ELSE}TValueArr{$ENDIF}(CRLF), 0, Length(CRLF));

  case FRequestMethod of
    hmPOST, hmPUT, hmTRACE: begin
      if FWriteDataBuffer.DataLength > 0 then begin
        while FWriteDataBuffer.DataLength > 0 do begin
          if FWriteDataBuffer.DataLength > Length(FBuffer) then
            cnt := Length(FBuffer)
          else
            cnt := FWriteDataBuffer.DataLength;

          FWriteDataBuffer.Read(FBuffer, 0, cnt);
          FVioTcp.Write({$IFDEF CLR}FBuffer{$ELSE}@FBuffer[0]{$ENDIF}, 0, cnt);
        end;
      end
      else
        FVioTcp.Write({$IFDEF CLR}Encoding.Default.GetBytes{$ELSE}TValueArr{$ENDIF}(FSendData), 0, Length(FSendData));
    end;
  end;
end;

procedure TCRHttp.RetrieveHeaders;
var
  ReplyMsg, UpperMsg, FirstWord: string;
begin
  FBytesTotal := 0;
  FCookieIn := '';
  FHeader := '';
  FReplyMessage := '';

  ReplyMsg := ReadLine;
  if UpperCase(Copy(ReplyMsg, 1, 4)) <> 'HTTP' then
    raise HttpException.Create('Unknown protocol');

  FReplyNumber := StrToIntDef(NthWord(ReplyMsg, ' ', 2), 0);
  FReplyMessage := ReplyMsg;
  FHeader := FHeader + ReplyMsg;

  repeat
    ReplyMsg := ReadLine;
    UpperMsg := UpperCase(ReplyMsg);
    FirstWord := NthWord(UpperMsg, ' ', 1);

    if FirstWord = Head_SetCookie then begin
      if Pos(';', ReplyMsg) > 0 then
        FCookieIn := Copy(ReplyMsg, 13, Pos(';', ReplyMsg) - 13)
      else
        FCookieIn := Copy(ReplyMsg, 13, Length(ReplyMsg) - 14);
    end
    else
    if FirstWord = Head_Location then begin
      FLocation := Copy(ReplyMsg, 11, 256);
      SetLength(FLocation, Length(FLocation) - 2);
    end
    else
    if FirstWord = Head_ContentLength2 then begin
      Delete(UpperMsg, 1, Pos(Head_ContentLength2, UpperMsg) + 14);
      FBytesTotal := StrToIntDef(Trim(UpperMsg), 0);
    end;

    FHeader := FHeader + ReplyMsg;
  until (ReplyMsg = #10) or (ReplyMsg = #13#10) or (ReplyMsg = '');
end;

procedure TCRHttp.ParseURL(const URL: string; var FScheme, FUser, FPassword,
  FNetworkLocation, FPort, FPath, FResource, FParameters, FQuery, FFragment: string);
var
  ParseString: string;
begin
  ParseString := URL;

  FFragment := ParseFragment(ParseString);
  FScheme := ParseScheme(ParseString);
  FNetworkLocation := ParseNetworkLocation(ParseString);
  FQuery := ParseQuery(ParseString);
  FParameters := ParseParameters(ParseString);
  FResource := ParseResource(ParseString);
  FPath := ParsePath(ParseString);
  If FPath = '' then
    FPath := '/';

  if FNetworkLocation <> '' then begin
    FUser := ParseUserPassword(FNetworkLocation);
    FPassword := ParsePassword(FUser);
    FPort := ParsePort(FNetworkLocation);
  end
  else begin
    FUser := '';
    FPassword := '';
    FPort := '';
  end;
end;

function TCRHttp.GetToEnd(const FindChar: Char; var ParseString: string; const KeepFirst: Boolean): string;
var
  i, len: Integer;
begin
  Result := '';

  if ParseString <> '' then begin
    i := Pos(FindChar, ParseString);
    if i > 0 then begin
      len := Length(ParseString) - i + 1;
      Result := Copy(ParseString, i, len);
      Delete(ParseString, i, len);
    end;

    if not KeepFirst then
      Delete(Result, 1, 1);
  end;
end;

function TCRHttp.ParseFragment(var ParseString: string): string;
begin
  Result := GetToEnd('#', ParseString, True);
end;

function TCRHttp.ParseScheme(var ParseString: string): string;
var
  i, len: Integer;
begin
  Result := SchemeDefault;

  if Length(ParseString) > 2 then begin
    len := Length(ParseString);
    i := 1;
    while (i < len - 1) and (AnsiChar(ParseString[i]) in ['0'..'9', 'A'..'Z', 'a'..'z', '+', '.', '-']) do
      Inc(i);

    if (ParseString[i] = ':') and (ParseString[i + 1] = '/') then begin
      Result := Copy(ParseString, 1, i);
      Delete(ParseString, 1, i);
    end;
  end;
end;

function TCRHttp.ParseNetworkLocation(var ParseString: string): string;
var
  i, len: Integer;
begin
  Result := '';

  if ParseString <> '' then begin
    len := Length(ParseString);
    if (len >= 2) and (ParseString[1] = '/') and (ParseString[2] = '/') then begin
      i := 3;
      while (i <= len) and (ParseString[i] <> '/') do
        Inc(i);

      Result := Copy(ParseString, 3, i - 3);
      Delete(ParseString, 1, i - 1);
    end
    else begin
      i := Pos('/', ParseString);
      if i > 1 then begin
        Result := Copy(ParseString, 1, i - 1);
        Delete(ParseString, 1, i - 1);
      end
      else
        if i = 0 then begin
          Result := ParseString;
          ParseString := '';
        end
    end;
  end;
end;

function TCRHttp.ParseQuery(var ParseString: string): string;
begin
  Result := GetToEnd('?', ParseString, True);
end;

function TCRHttp.ParseParameters(var ParseString: string): string;
begin
  Result := GetToEnd(';', ParseString, True);
end;

function TCRHttp.ParseResource(var ParseString: string): string;
var
  i: Integer;
begin
  if ParseString <> '' then begin
    if Pos('.', ParseString) > 0 then begin
      i := Length(ParseString);
      while (i > 0) and (ParseString[i] <> '/') do
        Dec(i);

      if i > 0 then begin
        Result := Copy(ParseString, i + 1, Length(ParseString));
        ParseString := Copy(ParseString, 1, i);
      end
      else
        Result := '';
    end
    else
      Result := '';
  end;
end;

function TCRHttp.ParsePath(var ParseString: string): string;
begin
  Result := ParseString;
end;

function TCRHttp.ParsePassword(var ParseString: string): string;
begin
  Result := GetToEnd(':', ParseString, False);
end;

function TCRHttp.ParseUserPassword(var ParseString: string): string;
var
  i: Integer;
begin
  Result := '';

  if ParseString <> '' then begin
    i := Pos('@', ParseString);
    if i > 0 then begin
      Result := Copy(ParseString, 1, i - 1);
      Delete(ParseString, 1, i);
    end;
  end;
end;

function TCRHttp.ParsePort(var ParseString: string): string;
begin
  Result := GetToEnd(':', ParseString, True);
end;

{*******************************************************************************************
Get Nth Word in a string
  InputString: The string on which the Nth Word is to be found
  Delimiter: The seperator for words - normally the space charachter but can be anything else
             for example if you are parsing an URL it can be the '/' charachter
  Number: The Nth word to be found . ie if you want the third Number:=3
  Result: The Nth Word as a string
********************************************************************************************}

function NthWord(const InputString: string; const Delimiter: Char; Number: Integer): string;
var
  i, j, len: Integer;
  StartPos: Integer;
begin
  Result := '';

  if InputString <> '' then begin
    i := 0;
    j := 1;
    len := Length(InputString) + 1;
    StartPos := 1;

    repeat
      if InputString[j] = Delimiter then begin
        Inc(i);
        if i = Number - 1 then
          StartPos := j + 1;
      end;
      Inc(j);
    until ((i = Number) or (j = len));

    if (StartPos > 1) or (Number = 1) then
      Result := Copy(InputString, StartPos, j - StartPos - 1);
  end;
end;

function WordsCount(const InputString: string; const Delimiter: Char): Integer;
var
  i: Integer;
begin
  if Length(InputString) > 0 then
    Result := 1
  else
    Result := 0;

  for i := 1 to Length(InputString) do
    if InputString[i] = Delimiter then
      Inc(Result);
end;

end.

