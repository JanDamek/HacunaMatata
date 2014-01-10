{ ==============================================================================
  Content:  TheTextDrawer, a helper class for drawing of
  fixed-pitched font characters
  ==============================================================================
  The contents of this file are subject to the Mozilla Public License Ver. 1.0
  (the "License"); you may not use this file except in compliance with the
  License. You may obtain a copy of the License at http://www.mozilla.org/MPL/

  Software distributed under the License is distributed on an "AS IS" basis,
  WITHOUT WARRANTY OF ANY KIND, either express or implied. See the License for
  the specific language governing rights and limitations under the License.
  ==============================================================================
  The Original Code is HANAI Tohru's private delphi library.
  ==============================================================================
  The Initial Developer of the Original Code is HANAI Tohru (Japan)
  Portions created by HANAI Tohru are Copyright (C) 1999.
  All Rights Reserved.
  ==============================================================================
  Contributor(s):   HANAI Tohru
  ==============================================================================
  History:  01/19/1999  HANAI Tohru
  Initial Version
  02/13/1999  HANAI Tohru
  Changed default intercharacter spacing
  09/09/1999  HANAI Tohru
  Redesigned all. Simplified interfaces.
  When drawing text now it uses TextOut + SetTextCharacter-
  Extra insted ExtTextOut since ExtTextOut has a little
  heavy behavior.
  09/10/1999  HANAI Tohru
  Added code to call ExtTextOut because there is a problem
  when TextOut called with italicized raster type font.
  After this changing, ExtTextOut is called without the
  last parameter `lpDx' and be with SetTextCharacterExtra.
  This pair performs faster than with `lpDx'.
  09/14/1999  HANAI Tohru
  Changed code for saving/restoring DC
  09/15/1999  HANAI Tohru
  Added X/Y parameters to ExtTextOut.
  09/16/1999  HANAI Tohru
  Redesigned for multi-bytes character drawing.
  09/19/1999  HANAI Tohru
  Since TheTextDrawer grew fat it was split into three
  classes - TheFontStock, TheTextDrawer and TheTextDrawerEx.
  Currently it should avoid TheTextDrawer because it is
  slower than TheTextDrawer.
  09/25/1999  HANAI Tohru
  Added internally definition of LeadBytes for Delphi 2
  10/01/1999  HANAI Tohru
  To save font resources, now all fonts data are shared
  among all of TheFontStock instances. With this changing,
  there added a new class `TheFontsInfoManager' to manage
  those shared data.
  10/09/1999  HANAI Tohru
  Added BaseStyle property to TheFontFont class.
  ============================================================================== }

// $Id: syntextdrawer.pp 34252 2011-12-18 00:09:31Z martin $

// SynEdit note: The name had to be changed to get SynEdit to install
// together with mwEdit into the same Delphi installation

unit SynTextDrawer;

interface

uses
  System.SysUtils, System.Classes, System.UITypes, System.Types, SynEditTypes, SynEditMiscProcs, FMX.Types,
  LazMethodList;

type
  TheStockFontPatterns = 0 .. (1 shl (1 + Ord(High(TFontStyle))));

  PheFontData = ^TheFontData;

  TheFontData = record
    Style: TFontStyles;
    Font: TFont;
    // Handle: HFont;
    CharAdv: Integer; // char advance of single-byte code
    CharHeight: Integer;
    NeedETO: Boolean;
  end;

  PheFontsData = ^TheFontsData;
  TheFontsData = array [TheStockFontPatterns] of TheFontData;

  PheSharedFontsInfo = ^TheSharedFontsInfo;

  TheSharedFontsInfo = record
    // reference counters
    RefCount: Integer;
    LockCount: Integer;
    // font information
    BaseFont: TFont;
    IsDBCSFont: Boolean;
    IsTrueType: Boolean;
    FontsData: TheFontsData;
  end;

  { TheFontsInfoManager }

  TheFontsInfoManager = class
  private
    FFontsInfo: TList;
    function CreateFontsInfo(ABaseFont: TFont): PheSharedFontsInfo;
    function FindFontsInfo(const BFont: TFont): PheSharedFontsInfo;
    procedure DestroyFontHandles(pFontsInfo: PheSharedFontsInfo);
  public
    constructor Create;
    destructor Destroy; override;
    procedure LockFontsInfo(pFontsInfo: PheSharedFontsInfo);
    procedure UnLockFontsInfo(pFontsInfo: PheSharedFontsInfo);
    function GetFontsInfo(ABaseFont: TFont): PheSharedFontsInfo;
    procedure ReleaseFontsInfo(var pFontsInfo: PheSharedFontsInfo);
  end;

  { TheFontStock }

  TheExtTextOutProc = procedure(X, Y: Integer; fuOptions: Integer; const ARect: TRect; Text: PChar; Length: Integer)
    of object;

  EheFontStockException = class(Exception);

  TheFontStock = class
  private
    // private DC
    // FDC: HDC;
    FDCRefCount: Integer;

    // Shared fonts
    FpInfo: PheSharedFontsInfo;
    FUsingFontHandles: Boolean;

    // Current font
    FCrntFont: TFont;
    FCrntStyle: TFontStyles;
    FpCrntFontData: PheFontData;
    // local font info
    function GetBaseFont: TFont;
    function GetIsDBCSFont: Boolean;
    function GetIsTrueType: Boolean;
    function GetNeedETO: Boolean;
  protected
    // function InternalGetDC: HDC; virtual;
    // procedure InternalReleaseDC(Value: HDC); virtual;
    Procedure CalcFontAdvance( { DC: HDC; } FontData: PheFontData; FontHeight: Integer);
    function GetCharAdvance: Integer; virtual;
    function GetCharHeight: Integer; virtual;
    function GetFontData(idx: Integer): PheFontData; virtual;
    procedure UseFontHandles;
    procedure ReleaseFontsInfo;
    procedure SetBaseFont(Value: TFont); virtual;
    procedure SetStyle(Value: TFontStyles); virtual;
    property FontData[idx: Integer]: PheFontData read GetFontData;
    property FontsInfo: PheSharedFontsInfo read FpInfo;
  public
    constructor Create(InitialFont: TFont); virtual;
    destructor Destroy; override;
    procedure ReleaseFontHandles; virtual;
  public
    // Info from the current font (per Style)
    function MonoSpace: Boolean;
    property Style: TFontStyles read FCrntStyle write SetStyle;
    // property FontHandle: HFONT read FCrntFont;
    property FontHandle: TFont read FCrntFont;
    property CharAdvance: Integer read GetCharAdvance;
    property CharHeight: Integer read GetCharHeight;
    property NeedETO: Boolean read GetNeedETO;
  public
    // Info from the BaseFont
    property BaseFont: TFont read GetBaseFont;
    property IsDBCSFont: Boolean read GetIsDBCSFont;
    property IsTrueType: Boolean read GetIsTrueType;
  end;

  { TheTextDrawer }
  EheTextDrawerException = class(Exception);

  TheTextDrawer = class(TObject)
  private
    // FDC: HDC;
    FSaveDC: Integer;
    // FSavedFont: HFont;
    FSavedFont: TFont;

    // Font information
    FFontStock: TheFontStock;
    FCalcExtentBaseStyle: TFontStyles;
    FBaseCharWidth: Integer;
    FBaseCharHeight: Integer;

    // current font and properties
    // FCrntFont: HFONT;
    FCrntFont: TFont;
    FETODist: Pointer;
    FETOSizeInChar: Integer;

    // current font attributes
    FColor: TColor;
    FBkColor: TColor;
    FFrameColor: array [TLazSynBorderSide] of TColor;
    FFrameStyle: array [TLazSynBorderSide] of TSynLineStyle;
    FCharExtra: Integer;

    // Begin/EndDrawing calling count
    FDrawingCount: Integer;
    ForceEto: Boolean;

    FOnFontChangedHandlers: TMethodList;
    FOnFontChangedLock: Integer;
  protected
    procedure ReleaseETODist; virtual;
    procedure AfterStyleSet; virtual;
    function GetUseUTF8: Boolean;
    function GetMonoSpace: Boolean;
    // function CreateColorPen(AColor: TColor; AStyle: LongWord = PS_SOLID): HPen;
    // property StockDC: HDC read FDC;
    property DrawingCount: Integer read FDrawingCount;
    property FontStock: TheFontStock read FFontStock;
    property BaseCharWidth: Integer read FBaseCharWidth;
    property BaseCharHeight: Integer read FBaseCharHeight;
  public
    constructor Create(CalcExtentBaseStyle: TFontStyles; ABaseFont: TFont); virtual;
    destructor Destroy; override;
    function GetCharWidth: Integer; virtual;
    function GetCharHeight: Integer; virtual;
    procedure BeginDrawing( { DC: HDC } ); virtual;
    procedure EndDrawing; virtual;
    procedure TextOut(X, Y: Integer; Text: PChar; Length: Integer); virtual;
    procedure ExtTextOut(X, Y: Integer; fuOptions: Integer; const ARect: TRect; Text: PChar; Length: Integer;
      FrameBottom: Integer = -1); virtual;
    procedure ForceNextTokenWithEto;
    procedure DrawLine(X, Y, X2, Y2: Integer; AColor: TColor);
    procedure SetBaseFont(Value: TFont); virtual;
    procedure SetBaseStyle(const Value: TFontStyles); virtual;
    procedure SetStyle(Value: TFontStyles); virtual;
    procedure SetForeColor(Value: TColor); virtual;
    procedure SetBackColor(Value: TColor); virtual;

    procedure SetFrameColor(Side: TLazSynBorderSide; AValue: TColor); overload; virtual;
    procedure SetFrameColor(AValue: TColor); overload; virtual; // deprecated;
    procedure SetFrameStyle(Side: TLazSynBorderSide; AValue: TSynLineStyle); overload; virtual;
    // procedure SetFrameStyle(AValue: TSynLineStyle); virtual; overload;

    procedure SetCharExtra(Value: Integer); virtual;
    procedure ReleaseTemporaryResources; virtual;

    procedure RegisterOnFontChangeHandler(AHandlerProc: TNotifyEvent);
    procedure UnRegisterOnFontChangeHandler(AHandlerProc: TNotifyEvent);

    property CharWidth: Integer read GetCharWidth;
    property CharHeight: Integer read GetCharHeight;
    property BaseFont: TFont write SetBaseFont;
    property BaseStyle: TFontStyles write SetBaseStyle;
    property ForeColor: TColor write SetForeColor;
    property BackColor: TColor read FBkColor write SetBackColor;
    property FrameColor[Side: TLazSynBorderSide]: TColor write SetFrameColor;
    property FrameStyle[Side: TLazSynBorderSide]: TSynLineStyle write SetFrameStyle;

    property Style: TFontStyles write SetStyle;
    property CharExtra: Integer read FCharExtra write SetCharExtra;
    property UseUTF8: Boolean read GetUseUTF8;
    property MonoSpace: Boolean read GetMonoSpace;
  end;

  { TheTextDrawerEx }

  TheTextDrawerEx = class(TheTextDrawer)
  private
    // current font properties
    FCrntDx: Integer;
    FCrntDBDx: Integer; // for a double-byte character
    // Text drawing procedure reference for optimization
    FExtTextOutProc: TheExtTextOutProc;
  protected
    procedure AfterStyleSet; override;
    procedure TextOutOrExtTextOut(X, Y: Integer; fuOptions: Integer; const ARect: TRect; Text: PChar;
      Length: Integer); virtual;
    procedure ExtTextOutFixed(X, Y: Integer; fuOptions: Integer; const ARect: TRect; Text: PChar;
      Length: Integer); virtual;
    procedure ExtTextOutWithETO(X, Y: Integer; fuOptions: Integer; const ARect: TRect; Text: PChar;
      Length: Integer); virtual;
    procedure ExtTextOutForDBCS(X, Y: Integer; fuOptions: Integer; const ARect: TRect; Text: PChar;
      Length: Integer); virtual;
  public
    procedure ExtTextOut(X, Y: Integer; fuOptions: Integer; const ARect: TRect; Text: PChar; Length: Integer;
      FrameBottom: Integer = -1); override;
  end;

function GetFontsInfoManager: TheFontsInfoManager;

(*
  {$IFNDEF VER93}
  {$IFNDEF VER90}
  {$IFNDEF VER80}
  {$DEFINE HE_ASSERT}
  {$DEFINE HE_LEADBYTES}
  {$DEFINE HE_COMPAREMEM}
  {$ENDIF}
  {$ENDIF}
  {$ENDIF}
*)

{$IFNDEF HE_LEADBYTES}

type
  TheLeadByteChars = set of Char;

function SetLeadBytes(const Value: TheLeadByteChars): TheLeadByteChars;
{$ENDIF}

implementation

const
  DBCHAR_CALCULATION_FALED = $7FFFFFFF;

var
  gFontsInfoManager: TheFontsInfoManager;
  SynTextDrawerFinalization: Boolean;

{$IFNDEF HE_LEADBYTES}
  LeadBytes: TheLeadByteChars;
{$ENDIF}
  { utility routines }

function GetFontsInfoManager: TheFontsInfoManager;
begin
  if (not Assigned(gFontsInfoManager)) and (not SynTextDrawerFinalization) then
    gFontsInfoManager := TheFontsInfoManager.Create;
  Result := gFontsInfoManager;
end;

function Min(X, Y: Integer): Integer;
begin
  if X < Y then
    Result := X
  else
    Result := Y;
end;

{$IFNDEF HE_ASSERT}

procedure ASSERT(Expression: Boolean);
begin
  if not Expression then
    raise EheTextDrawerException.Create('Assertion failed.');
end;
{$ENDIF}
{$IFNDEF HE_LEADBYTES}

function SetLeadBytes(const Value: TheLeadByteChars): TheLeadByteChars;
begin
  Result := LeadBytes;
  LeadBytes := Value;
end;
{$ENDIF}
{$IFNDEF HE_COMPAREMEM}

function CompareByte(Const buf1, buf2: Byte; len: Integer): Integer;
begin
  if buf1 = buf2 then
    Result := 0
  else if buf1 < buf2 then
    Result := -1
  else
    Result := 1;
end;

function CompareMem(P1, P2: Pointer; Length: Integer): Boolean;
begin
  Result := CompareByte(Byte(P1^), Byte(P2^), Length) = 0;
end;
{$ENDIF}

function GetStyleIndex(Value: TFontStyles): Integer;
var
  item: TFontStyle;
begin
  Result := 0;
  for item := low(TFontStyle) to high(TFontStyle) do
    if item in Value then
      Result := Result + 1 shl Ord(item);
end;

{ TheFontsInfoManager }

procedure TheFontsInfoManager.LockFontsInfo(pFontsInfo: PheSharedFontsInfo);
begin
  Inc(pFontsInfo^.LockCount);
end;

constructor TheFontsInfoManager.Create;
begin
  inherited Create;
  FFontsInfo := TList.Create;
end;

procedure TheFontsInfoManager.UnLockFontsInfo(pFontsInfo: PheSharedFontsInfo);
begin
  with pFontsInfo^ do
  begin
    if LockCount > 0 then
    begin
      Dec(LockCount);
      if 0 = LockCount then
        DestroyFontHandles(pFontsInfo);
    end;
  end;
end;

destructor TheFontsInfoManager.Destroy;
var
  APheSharedFontsInfo: PheSharedFontsInfo;
begin
  if Assigned(FFontsInfo) then
  begin
    while FFontsInfo.Count > 0 do
    begin
      ASSERT(1 = PheSharedFontsInfo(FFontsInfo[FFontsInfo.Count - 1])^.RefCount);
      APheSharedFontsInfo := PheSharedFontsInfo(FFontsInfo[FFontsInfo.Count - 1]);
      ReleaseFontsInfo(APheSharedFontsInfo);
    end;
    FFontsInfo.Free;
    FFontsInfo := nil;
  end;

  inherited Destroy;
  gFontsInfoManager := nil;
end;

procedure TheFontsInfoManager.DestroyFontHandles(pFontsInfo: PheSharedFontsInfo);
var
  i: Integer;
begin
  with pFontsInfo^ do
    for i := Low(TheStockFontPatterns) to High(TheStockFontPatterns) do
      with FontsData[i] do
      // if Handle <> 0 then
      begin
        FreeAndNil(Font);
        // Handle := 0;
      end;
end;

function TheFontsInfoManager.CreateFontsInfo(ABaseFont: TFont): PheSharedFontsInfo;
var
  // DC: HDC;
  hOldFont: TFont;
begin
  New(Result);
  FillChar(Result^, SizeOf(TheSharedFontsInfo), 0);
  with Result^ do
    try
      BaseFont := TFont.Create;
      BaseFont.Assign(ABaseFont);
      IsTrueType := False;
      // TODO: The old code returned always false too: (0 <> (TRUETYPE_FONTTYPE and LF.lfPitchAndFamily));
      // find out whether the font `IsDBCSFont'
      // DC := GetDC(0);
      // hOldFont := SelectObject(DC, ABaseFont.Reference.Handle);
      // IsDBCSFont := (0 <> (GCP_DBCS and GetFontLanguageInfo(DC)));
      // debugln('TheFontsInfoManager.CreateFontsInfo IsDBCSFont=',IsDBCSFont);
      // SelectObject(DC, hOldFont);
      // ReleaseDC(0, DC);
    except
      Result^.BaseFont.Free;
      Dispose(Result);
      raise;
    end;
end;

function TheFontsInfoManager.FindFontsInfo(const BFont: TFont): PheSharedFontsInfo;
var
  i: Integer;
begin
  for i := 0 to FFontsInfo.Count - 1 do
  begin
    Result := PheSharedFontsInfo(FFontsInfo[i]);
    if Result^.BaseFont = BFont then
      Exit;
  end;
  Result := nil;
end;

function TheFontsInfoManager.GetFontsInfo(ABaseFont: TFont): PheSharedFontsInfo;
begin
  ASSERT(Assigned(ABaseFont));

  Result := FindFontsInfo(ABaseFont);
  if not Assigned(Result) then
  begin
    Result := CreateFontsInfo(ABaseFont);
    FFontsInfo.Add(Result);
  end;

  if Assigned(Result) then
    Inc(Result^.RefCount);
end;

procedure TheFontsInfoManager.ReleaseFontsInfo(var pFontsInfo: PheSharedFontsInfo);
begin
  ASSERT(Assigned(pFontsInfo));

  with pFontsInfo^ do
  begin
{$IFDEF HE_ASSERT}
    ASSERT(LockCount < RefCount, 'Call DeactivateFontsInfo before calling this.');
{$ELSE}
    ASSERT(LockCount < RefCount);
{$ENDIF}
    if RefCount > 1 then
      Dec(RefCount)
    else
    begin
      FFontsInfo.Remove(pFontsInfo);
      // free all objects
      BaseFont.Free;
      Dispose(pFontsInfo);
    end;
  end;
  pFontsInfo := nil;
  if SynTextDrawerFinalization and (FFontsInfo.Count = 0) then
    // the program is in the finalization phase
    // and this object is not used anymore -> destroy it
    Free;
end;

{ TheFontStock }

// CalcFontAdvance : Calculation a advance of a character of a font.
// [*]hCalcFont will be selected as FDC's font if FDC wouldn't be zero.
Procedure TheFontStock.CalcFontAdvance( { DC: HDC; } FontData: PheFontData; FontHeight: Integer);

  Procedure DebugFont(s: String; a: array of const);
  begin
    if FontData^.Font <> nil then
    begin
      if FontData^.Font.Size = 0 then
        Exit;
      s := 'Font=' + FontData^.Font.Family + ' Size=' + IntToStr(Round(FontData^.Font.Size)) + ' ' + s;
    end;
    s := 'TheFontStock.CalcFontAdvance: ' + s;
    // DebugLn(Format(s, a));
  end;

  procedure GetWHOForChar(s: Char; out w, h, o: Integer; var eto: Boolean);
  var
    s1, s2, s3: String;
    Size1, Size2, Size3: TSize;
    w2, w3: Integer;
  begin
    s1 := s;
    s2 := s1 + s;
    s3 := s2 + s;
    { if not(GetTextExtentPoint(DC, PChar(s1), 1, Size1) and GetTextExtentPoint(DC, PChar(s2), 2, Size2) and
      GetTextExtentPoint(DC, PChar(s3), 3, Size3)) then
      begin
      DebugFont('Failed to get GetTextExtentPoint for %s', [s1]);
      w := 0;
      h := 0;
      o := 0;
      eto := True;
      Exit;
      end; }
    h := Size1.cy;
    // Size may contain overhang (italic, bold)
    // Size1 contains the size of 1 char + 1 overhang
    // Size2 contains the width of 2 chars, with only 1 overhang

    // Start simple
    w := Size1.cx;
    o := 0;

    w2 := Size2.cx - Size1.cx;
    w3 := Size3.cx - Size2.cx;
{$IFDEF SYNFONTDEBUG}
    DebugFont('Got TextExtends for %s=%d, %s=%d, %s=%d  Height=%d', [s1, Size1.cx, s2, Size2.cx, s3, Size3.cx, h]);
{$ENDIF}
    if (w2 = w) and (w3 = w) then
      Exit;

    if (w2 <= w) and (w3 <= w) then
    begin
      // w includes overhang (may be fractional
      if w2 <> w3 then
      begin
{$IFNDEF SYNFONTDEBUG} if abs(w3 - w2) > 1 then {$ENDIF}
          DebugFont('Variable Overhang w=%d w2=%d w3=%d', [w, w2, w3]);
        w2 := Max(w2, w3);
      end;
      o := w - w2;
      w := w2;
      eto := True;
    end
    else if (w2 >= w) or (w3 >= w) then
    begin
      // Width may be fractional, check sanity and keep w
      o := 1;
      eto := True;
      if Max(w2, w3) > w + 1 then
      begin
        DebugFont('Size diff to bi for fractioanl (greater 1) w=%d w2=%d w3=%d', [w, w2, w3]);
        // Take a guess/average
        w2 := Max(w2, w3);
        o := w2 - w;
        w := Max(w, (w + w2 - 1) div 2);
      end;
    end
    else
    begin
      // broken font? one of w2/w3 is smaller, the other wider than w
      w := Max(w, (w + w2 + w3 - 1) div 3);
      o := w div 2;
      eto := True;
    end;
{$IFDEF SYNFONTDEBUG}
    DebugFont('Final result for %s  Width=%d  Overhang=%d  eto=%s', [s1, w, o, dbgs(eto)]);
{$ENDIF}
  end;

  procedure AdjustWHOForChar(s: Char; var w, h, o: Integer; var eto: Boolean);
  var
    h2, w2, o2: Integer;
  begin
    GetWHOForChar(s, w2, h2, o2, eto);
    h := Max(h, h2);
    o := Max(o, o2);
    if w <> w2 then
    begin
      w := Max(w, w2);
      eto := True;
    end;
  end;

var
  // TM: TTextMetric;
  Height, Width, OverHang: Integer;
  eto: Boolean;
  Size1: TSize;
  tmw: Integer;
begin
  // Calculate advance of a character.

  // TextMetric may fail, because:
  // tmMaxCharWidth may be the width of a single Width (Latin) char, like "M"
  // or a double Width (Chinese) char
  // tmAveCharWidth is to small for proprtional fonts, as we need he witdh of the
  // widest Latin char ("M").
  // Even Monospace fonts, may have a smaller tmAveCharWidth (seen with Japanese)

  // take several samples
  eto := False;
  GetWHOForChar('M', Width, Height, OverHang, eto);
  AdjustWHOForChar('W', Width, Height, OverHang, eto);
  AdjustWHOForChar('@', Width, Height, OverHang, eto);
  AdjustWHOForChar('X', Width, Height, OverHang, eto);
  AdjustWHOForChar('m', Width, Height, OverHang, eto);
  // Small Chars to detect proportional fonts
  AdjustWHOForChar('i', Width, Height, OverHang, eto);
  AdjustWHOForChar(':', Width, Height, OverHang, eto);
  AdjustWHOForChar('''', Width, Height, OverHang, eto);

  // Negative Overhang ?
  (* if (not eto) and GetTextExtentPoint(DC, PChar('Ta'), 2, Size1) then
    if Size1.cx < 2 * Width then
    begin
    {$IFDEF SYNFONTDEBUG}
    DebugFont('Negative Overhang for "Ta" cx=%d  Width=%d Overhang=%d', [Size1.cx, Width, OverHang]);
    {$ENDIF}
    eto := True;
    end;
  *)
  // Make sure we get the correct Height
  // if GetTextExtentPoint(DC, PChar('Tgq[_|^'), 7, Size1) then
  // Height := Max(Height, Size1.cy);

  // DoubleCheck the result with GetTextMetrics
  // GetTextMetrics(DC, TM);
{$IFDEF SYNFONTDEBUG}
  DebugFont('TextMetrics tmHeight=%d, tmAve=%d, tmMax=%d, tmOver=%d',
    [TM.tmHeight, TM.tmAveCharWidth, TM.tmMaxCharWidth, TM.tmOverhang]);
{$ENDIF}
  // tmw := TM.tmMaxCharWidth + Max(TM.tmOverhang, 0);
  { if Width = 0 then
    begin
    DebugFont('No Width from GetTextExtentPoint', []);
    Width := tmw;
    end
    else if (Width > tmw) and (TM.tmMaxCharWidth > 0) then
    begin
    DebugFont('Width(%d) > tmMaxWidth+Over(%d)', [Width, tmw]);
    // take a guess, this is probably a broken font
    Width := Min(Width, round((TM.tmMaxCharWidth + Max(TM.tmOverhang, 0)) * 1.2));
    eto := True;
    end;

    if Height = 0 then
    begin
    DebugFont('No Height from GetTextExtentPoint, tmHeight=%d', [TM.tmHeight]);
    Height := TM.tmHeight;
    end
    else if Height < TM.tmHeight then
    begin
    DebugFont('Height from GetTextExtentPoint to low Height=%d, tmHeight=%d', [Height, TM.tmHeight]);
    Height := TM.tmHeight;
    end;
    if Height = 0 then
    begin
    DebugFont('SynTextDrawer: Fallback on FontHeight', []);
    Height := FontHeight;
    end;

    // If we have a broken font, make sure we return a positive value
    if Width <= 0 then
    begin
    DebugFont('SynTextDrawer: Fallback on Width', []);
    Width := 1 + Height * 8 div 10;
    end;
  }

  // if OverHang >0 then debugln(['SynTextDrawer: Overhang=', OverHang]);;
  FontData^.CharAdv := Width;
  FontData^.CharHeight := Height;
  FontData^.NeedETO := eto;
end;

constructor TheFontStock.Create(InitialFont: TFont);
begin
  inherited Create;

  SetBaseFont(InitialFont);
end;

destructor TheFontStock.Destroy;
begin
  ReleaseFontsInfo;
  ASSERT(FDCRefCount = 0);

  inherited;
end;

function TheFontStock.GetBaseFont: TFont;
begin
  Result := FpInfo^.BaseFont;
end;

function TheFontStock.GetCharAdvance: Integer;
begin
  Result := FpCrntFontData^.CharAdv;
end;

function TheFontStock.GetCharHeight: Integer;
begin
  Result := FpCrntFontData^.CharHeight;
end;

function TheFontStock.GetFontData(idx: Integer): PheFontData;
begin
  Result := @FpInfo^.FontsData[idx];
end;

function TheFontStock.GetIsDBCSFont: Boolean;
begin
  Result := FpInfo^.IsDBCSFont;
end;

function TheFontStock.GetIsTrueType: Boolean;
begin
  Result := FpInfo^.IsTrueType
end;

function TheFontStock.GetNeedETO: Boolean;
begin
  Result := FpCrntFontData^.NeedETO;
end;

{ function TheFontStock.InternalGetDC: HDC;
  begin
  if FDCRefCount = 0 then
  begin
  ASSERT(FDC = 0);
  FDC := GetDC(0);
  end;
  Inc(FDCRefCount);
  Result := FDC;
  end;

  procedure TheFontStock.InternalReleaseDC(Value: HDC);
  begin
  Dec(FDCRefCount);
  if FDCRefCount <= 0 then
  begin
  ASSERT((FDC <> 0) and (FDC = Value));
  ReleaseDC(0, FDC);
  FDC := 0;
  ASSERT(FDCRefCount = 0);
  end;
  end;
}
procedure TheFontStock.ReleaseFontHandles;
begin
  if FUsingFontHandles then
    with GetFontsInfoManager do
    begin
      UnLockFontsInfo(FpInfo);
      FUsingFontHandles := False;
    end;
end;

function TheFontStock.MonoSpace: Boolean;
begin
  { FpCrntFontData^.Font.; }
  Result := { FpCrntFontData^.Font.IsMonoSpace; } False;
end;

procedure TheFontStock.ReleaseFontsInfo;
begin
  if Assigned(FpInfo) then
    with GetFontsInfoManager do
    begin
      if FUsingFontHandles then
      begin
        UnLockFontsInfo(FpInfo);
        FUsingFontHandles := False;
      end;
      ReleaseFontsInfo(FpInfo);
    end;
end;

procedure TheFontStock.SetBaseFont(Value: TFont);
var
  pInfo: PheSharedFontsInfo;
begin
  if Assigned(Value) then
  begin
    pInfo := GetFontsInfoManager.GetFontsInfo(Value);
    if pInfo = FpInfo then
    begin
      // GetFontsInfo has increased the refcount, but we already have the font
      // -> decrease the refcount
      GetFontsInfoManager.ReleaseFontsInfo(pInfo);
    end
    else
    begin
      ReleaseFontsInfo;
      FpInfo := pInfo;
      // clear styles
      SetStyle(Value.Style);
    end;
  end
  else
    raise EheFontStockException.Create('SetBaseFont: ''Value'' must be specified.');
end;

procedure TheFontStock.SetStyle(Value: TFontStyles);
var
  idx: Integer;
  // DC: HDC;
  // hOldFont: HFont;
  p: PheFontData;
begin
  idx := GetStyleIndex(Value);
{$IFDEF HE_ASSERT}
  ASSERT(idx <= High(TheStockFontPatterns));
{$ENDIF}
  UseFontHandles;
  p := FontData[idx];
  if FpCrntFontData = p then
    Exit;

  FpCrntFontData := p;
  with p^ do
  // if Handle <> 0 then
  begin
    // FCrntFont := Handle;
    FCrntStyle := Style;
    Exit;
  end;

  // create font
  FpCrntFontData^.Font := TFont.Create;
  FpCrntFontData^.Font.Assign(BaseFont);
  FpCrntFontData^.Font.Style := Value;
  // FCrntFont := FpCrntFontData^.Font.Reference.Handle;

  // DC := InternalGetDC;
  // hOldFont := SelectObject(DC, FCrntFont);

  // retrieve height and advances of new font
  // FpInfo^.IsDBCSFont := (0 <> (GCP_DBCS and GetFontLanguageInfo(DC)));
  // debugln('TheFontStock.SetStyle A IsDBCSFont=',IsDBCSFont);
  // FpCrntFontData^.Handle := FCrntFont;
  // CalcFontAdvance(DC, FpCrntFontData, Max(BaseFont.Size, BaseFont.Height));
  // if FpCrntFontData^.NeedETO then debugln(['Needing ETO fot Font=',BaseFont.Name, ' Height=', BaseFont.Height, ' Style=', integer(Value) ]);

  // hOldFont := SelectObject(DC, hOldFont);
  // if hOldFont <> FCrntFont then
  // RaiseGDBException('TheFontStock.SetStyle LCL interface lost the font');
  // InternalReleaseDC(DC);
end;

procedure TheFontStock.UseFontHandles;
begin
  if not FUsingFontHandles then
    with GetFontsInfoManager do
    begin
      LockFontsInfo(FpInfo);
      FUsingFontHandles := True;
    end;
end;

{ TheTextDrawer }

constructor TheTextDrawer.Create(CalcExtentBaseStyle: TFontStyles; ABaseFont: TFont);
var
  Side: TLazSynBorderSide;
begin
  inherited Create;

  FFontStock := TheFontStock.Create(ABaseFont);
  FCalcExtentBaseStyle := CalcExtentBaseStyle;
  SetBaseFont(ABaseFont);
  FColor := TColorRec.cWindowText;
  FBkColor := TColorRec.cWindow;

  for Side := Low(TLazSynBorderSide) to High(TLazSynBorderSide) do
  begin
    FFrameColor[Side] := TColorRec.Null;
    FFrameStyle[Side] := slsSolid;
  end;

  FOnFontChangedHandlers := TMethodList.Create;
  FOnFontChangedLock := 0;
end;

destructor TheTextDrawer.Destroy;
begin
  FreeAndNil(FOnFontChangedHandlers);
  FFontStock.Free;
  ReleaseETODist;

  inherited;
end;

function TheTextDrawer.GetUseUTF8: Boolean;
begin
  // FFontStock.BaseFont.Reference;
  Result := { FFontStock.BaseFont.CanUTF8; } True;
  // debugln('TheTextDrawer.GetUseUTF8 ',FFontStock.BaseFont.Name,' ',dbgs(FFontStock.BaseFont.CanUTF8),' ',dbgs(FFontStock.BaseFont.HandleAllocated));
end;

function TheTextDrawer.GetMonoSpace: Boolean;
begin
  { FFontStock.BaseFont.Reference; }
  Result := { FFontStock.BaseFont.IsMonoSpace; } False;
  // debugln('TheTextDrawer.GetMonoSpace ',FFontStock.BaseFont.Name,' ',dbgs(FFontStock.BaseFont.IsMonoSpace),' ',dbgs(FFontStock.BaseFont.HandleAllocated));
end;

{ function TheTextDrawer.CreateColorPen(AColor: TColor; AStyle: LongWord = PS_SOLID): HPen;
  var
  ALogBrush: TLogBrush;
  begin
  AStyle := AStyle + PS_ENDCAP_FLAT + PS_GEOMETRIC + PS_JOIN_MITER;

  ALogBrush.lbStyle := BS_SOLID;
  ALogBrush.lbColor := ColorToRGB(AColor);
  ALogBrush.lbHatch := 0;

  Result := ExtCreatePen(AStyle, 1, ALogBrush, 0, nil);
  end; }

procedure TheTextDrawer.SetFrameStyle(Side: TLazSynBorderSide; AValue: TSynLineStyle);
begin
  if FFrameStyle[Side] <> AValue then
  begin
    FFrameStyle[Side] := AValue;
  end;
end;

// procedure TheTextDrawer.SetFrameStyle(AValue: TSynLineStyle);
// var
// Side: TLazSynBorderSide;
// begin
// for Side := Low(TLazSynBorderSide) to High(TLazSynBorderSide) do
// SetFrameStyle(Side, AValue);
// end;

procedure TheTextDrawer.ReleaseETODist;
begin
  if Assigned(FETODist) then
  begin
    FETOSizeInChar := 0;
    FreeMem(FETODist);
    FETODist := nil;
  end;
end;

procedure TheTextDrawer.BeginDrawing { (DC: HDC) };
begin
  // if (FDC = DC) then
  // ASSERT(FDC <> 0)
  // else
  // begin
  // ASSERT((FDC = 0) and (DC <> 0) and (FDrawingCount = 0));
  // FDC := DC;
  // FSaveDC := SaveDC(DC);
  // FSavedFont := SelectObject(DC, FCrntFont);
  // LCLIntf.SetTextColor(DC, TColorRef(FColor));
  // LCLIntf.SetBkColor(DC, TColorRef(FBkColor));
  // end;
  Inc(FDrawingCount);
end;

procedure TheTextDrawer.EndDrawing;
begin
  ASSERT(FDrawingCount >= 1);
  Dec(FDrawingCount);
  if FDrawingCount <= 0 then
  begin
    // if FDC <> 0 then
    // begin
    // if FSavedFont <> 0 then
    // SelectObject(FDC, FSavedFont);
    // RestoreDC(FDC, FSaveDC);
    // end;
    FSaveDC := 0;
    // FDC := 0;
    FDrawingCount := 0;
  end;
end;

function TheTextDrawer.GetCharWidth: Integer;
begin
  Result := FBaseCharWidth + FCharExtra;
end;

function TheTextDrawer.GetCharHeight: Integer;
begin
  Result := FBaseCharHeight;
end;

procedure TheTextDrawer.SetBaseFont(Value: TFont);
begin
  if Assigned(Value) then
  begin
    Inc(FOnFontChangedLock);
    try
{$IFDEF SYNFONTDEBUG}
      DebugLn(['TheTextDrawer.SetBaseFont Name=', Value.Name, ' Size=', Value.Size, 'Style=', Integer(Value.Style)]);
{$ENDIF}
      ReleaseETODist;
      with FFontStock do
      begin
        SetBaseFont(Value);
        // debugln('TheTextDrawer.SetBaseFont B ',Value.Name);
        FBaseCharWidth := 0;
        FBaseCharHeight := 0;
      end;
      BaseStyle := Value.Style;
      SetStyle(Value.Style);
    finally
      Dec(FOnFontChangedLock);
    end;
    FOnFontChangedHandlers.CallNotifyEvents(Self);
  end
  else
    raise EheTextDrawerException.Create('SetBaseFont: ''Value'' must be specified.');
end;

procedure TheTextDrawer.SetBaseStyle(const Value: TFontStyles);
begin
  if (FCalcExtentBaseStyle <> Value) or (FBaseCharWidth = 0) then
  begin
    FCalcExtentBaseStyle := Value;
    ReleaseETODist;
    with FFontStock do
    begin
      Style := Value;
      FBaseCharWidth := Max(FBaseCharWidth, CharAdvance);
      FBaseCharHeight := Max(FBaseCharHeight, CharHeight);
{$IFDEF SYNFONTDEBUG}
      DebugLn(['TheTextDrawer.SetBaseStyle =', Integer(Value), ' CharAdvance=', CharAdvance, ' CharHeight=', CharHeight,
        ' FBaseCharWidth=', FBaseCharWidth, ' FBaseCharHeight=', FBaseCharHeight]);
{$ENDIF}
    end;
    if FOnFontChangedLock = 0 then
      FOnFontChangedHandlers.CallNotifyEvents(Self);
  end;
end;

procedure TheTextDrawer.SetStyle(Value: TFontStyles);
begin
  with FFontStock do
  begin
    SetStyle(Value);
    Self.FCrntFont := FontHandle;
  end;
  AfterStyleSet;
end;

procedure TheTextDrawer.AfterStyleSet;
begin
  // if FDC <> 0 then
  // SelectObject(FDC, FCrntFont);
end;

procedure TheTextDrawer.SetForeColor(Value: TColor);
begin
  if FColor <> Value then
  begin
    FColor := Value;
    // if FDC <> 0 then
    // SetTextColor(FDC, TColorRef(Value));
  end;
end;

procedure TheTextDrawer.SetBackColor(Value: TColor);
begin
  if FBkColor <> Value then
  begin
    FBkColor := Value;
    // if FDC <> 0 then
    // LCLIntf.SetBkColor(FDC, TColorRef(Value));
  end;
end;

procedure TheTextDrawer.SetFrameColor(Side: TLazSynBorderSide; AValue: TColor);
begin
  if FFrameColor[Side] <> AValue then
  begin
    FFrameColor[Side] := AValue;
  end;
end;

procedure TheTextDrawer.SetFrameColor(AValue: TColor);
var
  Side: TLazSynBorderSide;
begin
  for Side := Low(TLazSynBorderSide) to High(TLazSynBorderSide) do
    SetFrameColor(Side, AValue);
end;

procedure TheTextDrawer.SetCharExtra(Value: Integer);
begin
  if FCharExtra <> Value then
  begin
    FCharExtra := Value;
    FETOSizeInChar := 0;
  end;
end;

procedure TheTextDrawer.TextOut(X, Y: Integer; Text: PChar; Length: Integer);
begin
  TextOut(X, Y, Text, Length);
end;

procedure TheTextDrawer.ExtTextOut(X, Y: Integer; fuOptions: Integer; const ARect: TRect; Text: PChar; Length: Integer;
  FrameBottom: Integer = -1);

  procedure InitETODist(InitValue: Integer);
  const
    EtoBlockSize = $40;
  var
    NewSize: Integer;
    TmpLen: Integer;
    p: PInteger;
    i: Integer;
  begin
    TmpLen := ((not(EtoBlockSize - 1)) and Length) + EtoBlockSize;
    NewSize := TmpLen * SizeOf(Integer);
    ReallocMem(FETODist, NewSize);
    p := PInteger(Pointer(LongInt(FETODist) + (FETOSizeInChar * SizeOf(Integer))));
    for i := 1 to TmpLen - FETOSizeInChar do
    begin
      p^ := InitValue;
      Inc(p);
    end;
    FETOSizeInChar := TmpLen;
  end;

  function HasFrame: Boolean;
  var
    Side: TLazSynBorderSide;
  begin
    for Side := Low(TLazSynBorderSide) to High(TLazSynBorderSide) do
      if FFrameColor[Side] <> TColorRec.Null then
        Exit(True);
    Result := False;
  end;

const
  WaveRadius = 3;
//const
//  PenStyle: array [TSynLineStyle] of LongWord = (slsSolid { PS_SOLID } , slsDashed { PS_DASH } , slsDotted { PS_DOT } ,
//    slsWaved { PS_SOLID } // we draw a wave using solid pen
//    );
var
  NeedDistArray: Boolean;
  DistArray: PInteger;
  // Pen, OldPen: HPen;
  old: TPoint;
  Side: TLazSynBorderSide;
  LastColor: TColor;
  LastStyle: LongWord;
begin
  if HasFrame then // draw background // TODO: only if not default bg color
  begin
    // InternalFillRect(FDC, ARect);
    // if (fuOptions and ETO_OPAQUE) > 0 then
    // fuOptions := fuOptions - ETO_OPAQUE;
    fuOptions := 0;

    if FrameBottom < 0 then
      FrameBottom := ARect.Bottom;

    // OldPen := 0;
    LastColor := TColorRec.Null;
    LastStyle := 0;
    for Side := Low(TLazSynBorderSide) to High(TLazSynBorderSide) do
    begin
      if FFrameColor[Side] <> TColorRec.Null then
      begin
        if { (OldPen = 0) or } (FFrameColor[Side] <> LastColor) {or (PenStyle[FFrameStyle[Side]] <> LastStyle)} then
        begin
          LastColor := FFrameColor[Side];
//          LastStyle := PenStyle[FFrameStyle[Side]];
          // if OldPen <> 0 then
          // DeleteObject(SelectObject(FDC, OldPen));
          // Pen := CreateColorPen(LastColor, LastStyle);
          // OldPen := SelectObject(FDC, Pen);
        end;

        case Side of
          bsLeft:
            begin
              // MoveToEx(FDC, ARect.Left, ARect.Top, @old);
              // if FFrameStyle[Side] = slsWaved then
              // WaveTo(FDC, ARect.Left, FrameBottom, WaveRadius)
              // else
              // LineTo(ARect.Left, FrameBottom);
            end;
          bsTop:
            begin
              // MoveToEx(FDC, ARect.Left, ARect.Top, @old);
              // if FFrameStyle[Side] = slsWaved then
              // WaveTo(FDC, ARect.Right, ARect.Top, WaveRadius)
              // else
              // LineTo(FDC, ARect.Right, ARect.Top);
            end;
          bsRight:
            begin
              if FFrameStyle[Side] = slsWaved then
              begin
                // MoveToEx(FDC, ARect.Right - WaveRadius, ARect.Top, @old);
                // WaveTo(FDC, ARect.Right - WaveRadius, FrameBottom, WaveRadius)
              end
              else
              begin
                // MoveToEx(FDC, ARect.Right - 1, ARect.Top, @old);
                // LineTo(FDC, ARect.Right - 1, FrameBottom);
              end;
            end;
          bsBottom:
            begin
              if FFrameStyle[Side] = slsWaved then
              begin
                // MoveToEx(FDC, ARect.Left, FrameBottom - WaveRadius, @old);
                // WaveTo(FDC, ARect.Right, FrameBottom - WaveRadius, WaveRadius)
              end
              else
              begin
                // MoveToEx(FDC, ARect.Left, FrameBottom - 1, @old);
                // LineTo(FDC, ARect.Right, FrameBottom - 1);
              end;
            end;
        end;
        // MoveToEx(FDC, ARect.Left, ARect.Top, @old);
      end;
    end;
    // DeleteObject(SelectObject(FDC, OldPen));
  end;

  NeedDistArray := ForceEto or (FCharExtra <> 0) or (FBaseCharWidth <> FFontStock.CharAdvance) or FFontStock.NeedETO;
  ForceEto := False;
  // DebugLn(['TheTextDrawer.ExtTextOut NeedDistArray=',NeedDistArray]);
  if NeedDistArray then
  begin
    if (FETOSizeInChar < Length) then
      InitETODist(GetCharWidth);
    DistArray := PInteger(FETODist);
  end
  else
  begin
    DistArray := nil;
  end;
  // if UseUTF8 then
  // LCLIntf.ExtUTF8Out(FDC, X, Y, fuOptions, @ARect, Text, Length, DistArray)
  // else
  // LCLIntf.ExtTextOut(FDC, X, Y, fuOptions, @ARect, Text, Length, DistArray);
end;

procedure TheTextDrawer.ForceNextTokenWithEto;
begin
  ForceEto := True;
end;

procedure TheTextDrawer.DrawLine(X, Y, X2, Y2: Integer; AColor: TColor);
var
  // Pen, OldPen: HPen;
  old: TPoint;
begin
  // Pen := CreateColorPen(AColor);
  // OldPen := SelectObject(FDC, Pen);
  // MoveToEx(FDC, X, Y, @old);
  // LineTo(FDC, X2, Y2);
  // DeleteObject(SelectObject(FDC, OldPen));
end;

procedure TheTextDrawer.ReleaseTemporaryResources;
begin
  FFontStock.ReleaseFontHandles;
end;

procedure TheTextDrawer.RegisterOnFontChangeHandler(AHandlerProc: TNotifyEvent);
begin
  FOnFontChangedHandlers.Add(TMethod(AHandlerProc));
end;

procedure TheTextDrawer.UnRegisterOnFontChangeHandler(AHandlerProc: TNotifyEvent);
begin
  FOnFontChangedHandlers.Remove(TMethod(AHandlerProc));
end;

{ TheTextDrawerEx }

procedure TheTextDrawerEx.AfterStyleSet;
begin
  inherited;
  with FontStock do
  begin
    FCrntDx := BaseCharWidth - CharAdvance;
    case IsDBCSFont of
      False:
        begin
          // if StockDC <> 0 then
          // SetTextCharacterExtra(StockDC, CharExtra + FCrntDx);
          if IsTrueType or (not(TFontStyle.fsItalic in Style)) then
            FExtTextOutProc :=
{$IFDEF FPC}@{$ENDIF}TextOutOrExtTextOut
          else
            FExtTextOutProc :=
{$IFDEF FPC}@{$ENDIF}ExtTextOutFixed;
        end;
      True:
        begin
          FCrntDBDx := DBCHAR_CALCULATION_FALED;
          FExtTextOutProc :=
{$IFDEF FPC}@{$ENDIF}ExtTextOutWithETO;
        end;
    end;
  end;
end;

procedure TheTextDrawerEx.ExtTextOut(X, Y: Integer; fuOptions: Integer; const ARect: TRect; Text: PChar; Length: Integer;
  FrameBottom: Integer = -1);
begin
  FExtTextOutProc(X, Y, fuOptions, ARect, Text, Length);
end;

procedure TheTextDrawerEx.ExtTextOutFixed(X, Y: Integer; fuOptions: Integer; const ARect: TRect; Text: PChar;
  Length: Integer);
begin
//  LCLIntf.ExtTextOut(StockDC, X, Y, fuOptions, @ARect, Text, Length, nil);
end;

procedure TheTextDrawerEx.ExtTextOutForDBCS(X, Y: Integer; fuOptions: Integer; const ARect: TRect; Text: PChar;
  Length: Integer);
var
  pCrnt: PChar;
  pTail: PChar;
  pRun: PChar;

  procedure GetSBCharRange;
  begin
    while (pRun <> pTail) and (not(pRun^ in LeadBytes)) do
      Inc(pRun);
  end;

  procedure GetDBCharRange;
  begin
    while (pRun <> pTail) and (pRun^ in LeadBytes) do
      Inc(pRun, 2);
  end;

var
  TmpRect: TRect;
  len: Integer;
  n: Integer;
begin
  pCrnt := Text;
  pRun := Text;
//  pTail := PChar(Pointer(Text) + Length);
  TmpRect := ARect;
  while pCrnt < pTail do
  begin
    GetSBCharRange;
    if pRun <> pCrnt then
    begin
//      SetTextCharacterExtra(StockDC, FCharExtra + FCrntDx);
      len := PInteger(pRun)^ - PInteger(pCrnt)^;
      with TmpRect do
      begin
        n := GetCharWidth * len;
        Right := Min(Left + n + GetCharWidth, ARect.Right);
//        LCLIntf.ExtTextOut(StockDC, X, Y, fuOptions, @TmpRect, pCrnt, len, nil);
        Inc(X, n);
        Inc(Left, n);
      end;
    end;
    pCrnt := pRun;
    if pRun = pTail then
      break;

    GetDBCharRange;
//    SetTextCharacterExtra(StockDC, FCharExtra + FCrntDBDx);
    len := PInteger(pRun)^ - PInteger(pCrnt)^;
    with TmpRect do
    begin
      n := GetCharWidth * len;
      Right := Min(Left + n + GetCharWidth, ARect.Right);
//      LCLIntf.ExtTextOut(StockDC, X, Y, fuOptions, @TmpRect, pCrnt, len, nil);
      Inc(X, n);
      Inc(Left, n);
    end;
    pCrnt := pRun;
  end;

  if (pCrnt = Text) or // maybe Text is not assigned or Length is 0
    (TmpRect.Right < ARect.Right) then
  begin
//    SetTextCharacterExtra(StockDC, FCharExtra + FCrntDx);
//    LCLIntf.ExtTextOut(StockDC, X, Y, fuOptions, @TmpRect, nil, 0, nil);
  end;
end;

procedure TheTextDrawerEx.ExtTextOutWithETO(X, Y: Integer; fuOptions: Integer; const ARect: TRect; Text: PChar;
  Length: Integer);
begin
  inherited ExtTextOut(X, Y, fuOptions, ARect, Text, Length);
end;

procedure TheTextDrawerEx.TextOutOrExtTextOut(X, Y: Integer; fuOptions: Integer; const ARect: TRect; Text: PChar;
  Length: Integer);
begin
  // this function may be used when:
  // a. the text does not containing any multi-byte characters
  // AND
  // a-1. current font is TrueType.
  // a-2. current font is RasterType and it is not italic.
//  with ARect do
//    if Assigned(Text) and (Length > 0) and (Left = X) and (Top = Y) and ((Bottom - Top) = GetCharHeight) and
//      (Left + GetCharWidth * (Length + 1) > Right) then
//      LCLIntf.TextOut(StockDC, X, Y, Text, Length)
//    else
//      LCLIntf.ExtTextOut(StockDC, X, Y, fuOptions, @ARect, Text, Length, nil)
end;

{$IFNDEF HE_LEADBYTES}

procedure InitializeLeadBytes;
var
  c: Char;
begin
  for c := Low(Char) to High(Char) do
//    if IsDBCSLeadByte(Byte(c)) then
//      Include(LeadBytes, c);
end;
{$ENDIF} // HE_LEADBYTES

initialization

SynTextDrawerFinalization := False;
{$IFNDEF HE_LEADBYTES}
InitializeLeadBytes;
{$ENDIF}

finalization

// MG: We can't free the gFontsInfoManager here, because the synedit
// components need it and will be destroyed with the Application object in
// the lcl after this finalization section.
// So, the flag SynTextDrawerFinalization is set and the gFontsInfoManager
// will destroy itself, as soon, as it is not used anymore.
SynTextDrawerFinalization := True;
if Assigned(gFontsInfoManager) and (gFontsInfoManager.FFontsInfo.Count = 0) then
  FreeAndNil(gFontsInfoManager);

end.
