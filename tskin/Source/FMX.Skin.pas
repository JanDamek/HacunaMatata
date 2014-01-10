{ ******************************************************* }
{ *                     Skin Control                    * }
{ *              Delphi FireMonkey Platform             * }
{ *           Copyright(c) 2011 Victor Furletov         * }
{ *            e-mail: VictorFurletov@gmail.com         * }
{ ******************************************************* }

unit FMX.Skin;

interface

uses System.Classes, System.Types, System.SysUtils, FMX.Forms, FMX.Types;

type

  TTileStyle = (tsStretch, tsTile, tsTileHorzStretchVert, tsTileVertStretchHorz, tsCenter);
  TFrameOrientation = (foHorizontal, foVertical);

  TSkin = class;

  TFrameTileStyle = class(TPersistent)
  private
    FLeft: TTileStyle;
    FTop: TTileStyle;
    FRight: TTileStyle;
    FBottom: TTileStyle;
    FCenter: TTileStyle;
    FOnChange: TNotifyEvent;
    procedure SetBottom(const Value: TTileStyle);
    procedure SetCenter(const Value: TTileStyle);
    procedure SetLeft(const Value: TTileStyle);
    procedure SetRight(const Value: TTileStyle);
    procedure SetTop(const Value: TTileStyle);
  public
    procedure Assign(Source: TPersistent); override;
    property OnChange: TNotifyEvent read FOnChange write FOnChange;
  published
    property Left: TTileStyle read FLeft write SetLeft default tsStretch;
    property Top: TTileStyle read FTop write SetTop default tsStretch;
    property Right: TTileStyle read FRight write SetRight default tsStretch;
    property Bottom: TTileStyle read FBottom write SetBottom default tsStretch;
    property Center: TTileStyle read FCenter write SetCenter default tsStretch;
  end;

  TBitmapLink = class(TPersistent)
  private
    FLeft, FTop, FWidth, FHeight: Integer;
    FSkinLookup: String;
    FBitmap: TBitmap;
    FSkin: TSkin;
    FOnChange: TNotifyEvent;
    procedure SetHeight(const Value: Integer);
    procedure SetLeft(const Value: Integer);
    procedure SetSkinLookup(const Value: String);
    procedure SetTop(const Value: Integer);
    procedure SetWidth(const Value: Integer);
    procedure SetChange;
    function GetRect: TRect;
    function FindSkin(const AStyleLookup: string): TSkin;
  public
    constructor Create(ASkin: TSkin); virtual;
    property OnChange: TNotifyEvent read FOnChange write FOnChange;
  published
    property Left: Integer read FLeft write SetLeft default 0;
    property Top: Integer read FTop write SetTop default 0;
    property Width: Integer read FWidth write SetWidth default 0;
    property Height: Integer read FHeight write SetHeight default 0;
    property SkinLookup: String read FSkinLookup write SetSkinLookup;
  end;

  TSkin = class(TControl)
  private
    FBitmap: TBitmap;
    FTempBitmap: TBitmap;
    FBitmapLink: TBitmapLink;
    FFrameCount: Integer;
    FFrameMargins: TBounds;
    FFrameTileStyle: TFrameTileStyle;
    FFrameOrientation: TFrameOrientation;
    FStartFrame: Integer;
    FStopFrame: Integer;
    FFrameIndex: Integer;
    FStartOpacity, FStopOpacity: Single;
    FDisableInterpolation: Boolean;
    procedure SetBitmap(const Value: TBitmap);
    procedure SetFrameMargins(const Value: TBounds);
    procedure SetFrameCount(const Value: Integer);
    procedure SetFrameOrientation(const Value: TFrameOrientation);
    procedure DrawTile(ACanvas: TCanvas; ABitmap: TBitmap; Style: TTileStyle; SrcR, DstR: TRectF; AOpacity: Single);
    procedure SetFrameTileStyle(const Value: TFrameTileStyle);
    procedure NeedRepaint(Sender: TObject);
    procedure UpdateIndexes;
    procedure PaintFrame(LocR: TRectF; ACanvas: TCanvas; Bmp: TBitmap; BmpRect: TRect; AFrameIndex: Integer;
      AOpacity: Single);
    procedure SetFrameIndex(const Value: Integer);
  protected
    procedure Paint; override;
    function GetData: Variant; override;
    procedure SetData(const Value: Variant); override;
    function SkinRect(LocR: TRectF; BmpR: TRect; AFrameIndex: Integer; id: Byte; var SrcR, DstR: TRectF): Boolean;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure PaintSkin(AStartFrame, AStopFrame: Integer; AStartOpacity, AStopOpacity: Single); overload;
    procedure PaintSkin(AFrame: Integer; AOpacity: Single); overload;
  published
    property Bitmap: TBitmap read FBitmap write SetBitmap;
    property BitmapLink: TBitmapLink read FBitmapLink write FBitmapLink;
    property FrameMargins: TBounds read FFrameMargins write SetFrameMargins;
    property FrameCount: Integer read FFrameCount write SetFrameCount default 1;
    property FrameIndex: Integer read FFrameIndex write SetFrameIndex default 0;
    property FrameOrientation: TFrameOrientation read FFrameOrientation write SetFrameOrientation default foHorizontal;
    property FrameTileStyle: TFrameTileStyle read FFrameTileStyle write SetFrameTileStyle;
    property DisableInterpolation: Boolean read FDisableInterpolation write FDisableInterpolation default True;
  end;

  TSkinAnimation = class(TAnimation)
  private
    FInstance: TSkin;
    FStartFrame: Integer;
    FStopFrame: Integer;
  protected
    procedure ProcessAnimation; override;
  public
    constructor Create(AOwner: TComponent); override;
  published
    property StartFrame: Integer read FStartFrame write FStartFrame default 0;
    property StopFrame: Integer read FStopFrame write FStopFrame default 0;
  end;

procedure Register;

implementation

{$R *.res}
{ TSkin }

constructor TSkin.Create(AOwner: TComponent);
begin
  inherited;
  FBitmap := TBitmap.Create(0, 0);
  FBitmapLink := TBitmapLink.Create(Self);
  FBitmapLink.OnChange := NeedRepaint;
  FFrameMargins := TBounds.Create(RectF(0, 0, 0, 0));
  FFrameTileStyle := TFrameTileStyle.Create;
  FFrameTileStyle.OnChange := NeedRepaint;
  FFrameCount := 1;
  FStartOpacity := 1;
  FStopOpacity := 1;
  FDisableInterpolation := True;
end;

destructor TSkin.Destroy;
begin
  FreeAndNil(FFrameTileStyle);
  FreeAndNil(FFrameMargins);
  FBitmapLink.Free;
  if FTempBitmap <> nil then
    FreeAndNil(FTempBitmap);
  FBitmap.Free;
  inherited;
end;

procedure TSkin.DrawTile(ACanvas: TCanvas; ABitmap: TBitmap; Style: TTileStyle; SrcR, DstR: TRectF; AOpacity: Single);
var
  IR, ISR, IDR, ISrcR, IDstR: TRect;
  SR, DR, R: TRectF;
  Cx, Cy, i, j: Integer;
begin
  if Style = tsStretch then
  begin
    ACanvas.DrawBitmap(ABitmap, SrcR, DstR, AOpacity, FDisableInterpolation);
    Exit;
  end;

  if Style = tsCenter then
  begin
    R := SrcR;
    FitRect(R, DstR);
    ACanvas.DrawBitmap(ABitmap, SrcR, R, AOpacity, FDisableInterpolation);
    Exit;
  end;

  ISrcR := SrcR.Round;
  IDstR := DstR.Round;
  Cx := IDstR.Width div ISrcR.Width;
  if IDstR.Width mod ISrcR.Width <> 0 then
    Inc(Cx);
  Cy := IDstR.Height div ISrcR.Height;
  if IDstR.Height mod ISrcR.Height <> 0 then
    Inc(Cy);

  if Style = tsTileHorzStretchVert then
    Cy := 1;
  if Style = tsTileVertStretchHorz then
    Cx := 1;

  for i := 0 to Cx - 1 do
    for j := 0 to Cy - 1 do
    begin
      IR := IDstR;
      if Style <> tsTileVertStretchHorz then
        IR.Right := IR.Left + ISrcR.Width;
      if Style <> tsTileHorzStretchVert then
        IR.Bottom := IR.Top + ISrcR.Height;
      IR.Offset(i * ISrcR.Width, j * ISrcR.Height);
      IntersectRect(IDR, IR, IDstR);
      ISR := ISrcR;
      if Style <> tsTileVertStretchHorz then
        ISR.Right := ISR.Left + IDR.Width;
      if Style <> tsTileHorzStretchVert then
        ISR.Bottom := ISR.Top + IDR.Height;
      SR := TRectF.Create(ISR, False);
      DR := TRectF.Create(IDR, False);
      ACanvas.DrawBitmap(ABitmap, SR, DR, AOpacity, FDisableInterpolation);
    end;

end;

function TSkin.GetData: Variant;
begin
  Result := ObjectToVariant(FBitmap);
end;

procedure TSkin.NeedRepaint(Sender: TObject);
begin
  Repaint;
end;

procedure TSkin.Paint;

  function IsScaleMode(var AScaleX, AScaleY: Single): Boolean;
  var
    R: TRectF;
  begin
    R := LocalRect;
    AScaleX := FFrameMargins.Rect.Left + FFrameMargins.Rect.Right;
    AScaleY := FFrameMargins.Rect.Top + FFrameMargins.Rect.Bottom;
    Result := (AScaleX >= R.Width) or (AScaleY >= R.Height);
    if Result then
    begin
      if AScaleX < R.Width then
        AScaleX := R.Width;
      if AScaleY < R.Height then
        AScaleY := R.Height;
    end;
  end;

  function GetSourceBitmap(var ASourceRect: TRect): TBitmap;
  begin
    if (FBitmapLink.FBitmap <> nil) then
    begin
      Result := FBitmapLink.FBitmap;
      ASourceRect := FBitmapLink.GetRect;
    end
    else
    begin
      Result := FBitmap;
      ASourceRect := Rect(0, 0, Result.Width, Result.Height);
    end;
  end;

var
  ScaleX, ScaleY: Single;
  ScaleMode: Boolean;
  R: TRectF;
  SourceBitmap: TBitmap;
  SourceRect: TRect;
begin
  if (csDesigning in ComponentState) and not Locked and not FInPaintTo then
  begin
    R := LocalRect;
    InflateRect(R, -0.5, -0.5);
    Canvas.StrokeThickness := 1;
    Canvas.StrokeDash := TStrokeDash.sdDash;
    Canvas.Stroke.Kind := TBrushKind.bkSolid;
    Canvas.Stroke.Color := $A0909090;
    Canvas.DrawRect(R, 0, 0, AllCorners, AbsoluteOpacity);
    Canvas.StrokeDash := TStrokeDash.sdSolid;
  end;

  SourceBitmap := GetSourceBitmap(SourceRect);

  if SourceBitmap.IsEmpty then
    Exit;

  ScaleMode := IsScaleMode(ScaleX, ScaleY);
  if ScaleMode then
  begin
    if FTempBitmap = nil then
      FTempBitmap := TBitmap.Create(0, 0);
    FTempBitmap.SetSize(Round(ScaleX), Round(ScaleY));
    FTempBitmap.Clear(0);
    R := RectF(0, 0, FTempBitmap.Width, FTempBitmap.Height);
    if FStartFrame = FStopFrame then
      PaintFrame(R, FTempBitmap.Canvas, SourceBitmap, SourceRect, FStopFrame, 1)
    else
    begin
      PaintFrame(R, FTempBitmap.Canvas, SourceBitmap, SourceRect, FStartFrame, FStartOpacity);
      PaintFrame(R, FTempBitmap.Canvas, SourceBitmap, SourceRect, FStopFrame, FStopOpacity);
    end;
    Canvas.DrawBitmap(FTempBitmap, R, LocalRect, 1, FDisableInterpolation);
  end
  else
  begin
    if FTempBitmap <> nil then
      FreeAndNil(FTempBitmap);
    R := LocalRect;
    if FStartFrame = FStopFrame then
      PaintFrame(R, Canvas, SourceBitmap, SourceRect, FStopFrame, 1)
    else
    begin
      PaintFrame(R, Canvas, SourceBitmap, SourceRect, FStartFrame, FStartOpacity);
      PaintFrame(R, Canvas, SourceBitmap, SourceRect, FStopFrame, FStopOpacity);
    end;
  end;
end;

procedure TSkin.PaintFrame(LocR: TRectF; ACanvas: TCanvas; Bmp: TBitmap; BmpRect: TRect; AFrameIndex: Integer;
  AOpacity: Single);
var
  SrcR, DstR: TRectF;
begin
  AOpacity := FOpacity * AOpacity;
  { left top }
  if SkinRect(LocR, BmpRect, AFrameIndex, 0, SrcR, DstR) then
    ACanvas.DrawBitmap(Bmp, SrcR, DstR, AOpacity, FDisableInterpolation);
  { top }
  if SkinRect(LocR, BmpRect, AFrameIndex, 1, SrcR, DstR) then
    DrawTile(ACanvas, Bmp, FFrameTileStyle.Top, SrcR, DstR, AOpacity);
  { right top }
  if SkinRect(LocR, BmpRect, AFrameIndex, 2, SrcR, DstR) then
    ACanvas.DrawBitmap(Bmp, SrcR, DstR, AOpacity, FDisableInterpolation);
  { left }
  if SkinRect(LocR, BmpRect, AFrameIndex, 3, SrcR, DstR) then
    DrawTile(ACanvas, Bmp, FFrameTileStyle.Left, SrcR, DstR, AOpacity);
  { center }
  if SkinRect(LocR, BmpRect, AFrameIndex, 4, SrcR, DstR) then
    DrawTile(ACanvas, Bmp, FFrameTileStyle.Center, SrcR, DstR, AOpacity);
  { right }
  if SkinRect(LocR, BmpRect, AFrameIndex, 5, SrcR, DstR) then
    DrawTile(ACanvas, Bmp, FFrameTileStyle.Right, SrcR, DstR, AOpacity);
  { left bottom }
  if SkinRect(LocR, BmpRect, AFrameIndex, 6, SrcR, DstR) then
    ACanvas.DrawBitmap(Bmp, SrcR, DstR, AOpacity, FDisableInterpolation);
  { bottom }
  if SkinRect(LocR, BmpRect, AFrameIndex, 7, SrcR, DstR) then
    DrawTile(ACanvas, Bmp, FFrameTileStyle.Bottom, SrcR, DstR, AOpacity);
  { right botom }
  if SkinRect(LocR, BmpRect, AFrameIndex, 8, SrcR, DstR) then
    ACanvas.DrawBitmap(Bmp, SrcR, DstR, AOpacity, FDisableInterpolation);
end;

procedure TSkin.PaintSkin(AFrame: Integer; AOpacity: Single);
begin
  PaintSkin(AFrame, AFrame, AOpacity, AOpacity);
end;

procedure TSkin.PaintSkin(AStartFrame, AStopFrame: Integer; AStartOpacity, AStopOpacity: Single);
begin
  FStartFrame := AStartFrame;
  FStopFrame := AStopFrame;
  FStartOpacity := AStartOpacity;
  FStopOpacity := AStopOpacity;
  UpdateIndexes;
  Repaint;
end;

procedure TSkin.SetData(const Value: Variant);
begin
  if VarIsObject(Value) then
  begin
    if VariantToObject(Value) is TPersistent then
      FBitmap.Assign(TPersistent(VariantToObject(Value)));
  end
  else
    FBitmap.LoadFromFile(Value);
end;

procedure TSkin.SetFrameCount(const Value: Integer);
begin
  if FFrameCount <> Value then
  begin
    FFrameCount := Value;
    if FFrameCount < 1 then
      FFrameCount := 1;
    UpdateIndexes;
    Repaint;
  end;
end;

procedure TSkin.SetFrameOrientation(const Value: TFrameOrientation);
begin
  if FFrameOrientation <> Value then
  begin
    FFrameOrientation := Value;
    Repaint;
  end;
end;

procedure TSkin.SetFrameIndex(const Value: Integer);
begin
  PaintSkin(Value, 1);
  FFrameIndex := FStartFrame;
end;

procedure TSkin.SetBitmap(const Value: TBitmap);
begin
  FBitmap.Assign(Value);
  Repaint;
end;

procedure TSkin.SetFrameMargins(const Value: TBounds);
begin
  if FFrameMargins <> Value then
  begin
    FFrameMargins := Value;
    Repaint;
  end;
end;

procedure TSkin.SetFrameTileStyle(const Value: TFrameTileStyle);
begin
  FFrameTileStyle := Value;
  Repaint;
end;

function TSkin.SkinRect(LocR: TRectF; BmpR: TRect; AFrameIndex: Integer; id: Byte; var SrcR, DstR: TRectF): Boolean;
var
  W, H, Cx, Cy: Integer;
begin
  Result := False;
  if FFrameOrientation = foHorizontal then
  begin
    W := (BmpR.Right - BmpR.Left) div FFrameCount;
    H := BmpR.Bottom - BmpR.Top;
    Cx := AFrameIndex * W + BmpR.Left;
    Cy := 0 + BmpR.Top;
  end
  else
  begin
    W := BmpR.Right - BmpR.Left;
    H := (BmpR.Bottom - BmpR.Top) div FFrameCount;
    Cx := 0 + BmpR.Left;
    Cy := AFrameIndex * H + BmpR.Top;
  end;

  SrcR := RectF(0, 0, 0, 0);
  DstR := RectF(0, 0, 0, 0);

  case id of
    0:
      begin
        if (FFrameMargins.Left = 0) or (FFrameMargins.Top = 0) then
          Exit;
        SrcR := RectF(Cx, Cy, Cx + FFrameMargins.Left, Cy + FFrameMargins.Top);
        DstR := RectF(LocR.Left, LocR.Top, LocR.Left + FFrameMargins.Left, LocR.Top + FFrameMargins.Top);
      end;
    1:
      begin
        if (FFrameMargins.Top = 0) then
          Exit;
        SrcR := RectF(Cx + FFrameMargins.Left, Cy, Cx + W - FFrameMargins.Right, Cy + FFrameMargins.Top);
        DstR := RectF(LocR.Left + FFrameMargins.Left, LocR.Top, LocR.Right - FFrameMargins.Right,
          LocR.Top + FFrameMargins.Top);
      end;
    2:
      begin
        if (FFrameMargins.Right = 0) or (FFrameMargins.Top = 0) then
          Exit;
        SrcR := RectF(Cx + W - FFrameMargins.Right, Cy, Cx + W, Cy + FFrameMargins.Top);
        DstR := RectF(LocR.Right - FFrameMargins.Right, LocR.Top, LocR.Right, LocR.Top + FFrameMargins.Top);
      end;
    3:
      begin
        if (FFrameMargins.Left = 0) then
          Exit;
        SrcR := RectF(Cx, Cy + FFrameMargins.Top, Cx + FFrameMargins.Left, Cy + H - FFrameMargins.Bottom);
        DstR := RectF(LocR.Left, LocR.Top + FFrameMargins.Top, LocR.Left + FFrameMargins.Left,
          LocR.Bottom - FFrameMargins.Bottom);
      end;
    4:
      begin
        if (FFrameMargins.Left + FFrameMargins.Right >= W) or (FFrameMargins.Top + FFrameMargins.Bottom >= H) then
          Exit;
        SrcR := RectF(Cx + FFrameMargins.Left, Cy + FFrameMargins.Top, Cx + W - FFrameMargins.Right,
          Cy + H - FFrameMargins.Bottom);
        DstR := RectF(LocR.Left + FFrameMargins.Left, LocR.Top + FFrameMargins.Top, LocR.Right - FFrameMargins.Right,
          LocR.Bottom - FFrameMargins.Bottom);
      end;
    5:
      begin
        if (FFrameMargins.Right = 0) then
          Exit;
        SrcR := RectF(Cx + W - FFrameMargins.Right, Cy + FFrameMargins.Top, Cx + W, Cy + H - FFrameMargins.Bottom);
        DstR := RectF(LocR.Right - FFrameMargins.Right, LocR.Top + FFrameMargins.Top, LocR.Right,
          LocR.Bottom - FFrameMargins.Bottom);
      end;
    6:
      begin
        if (FFrameMargins.Left = 0) or (FFrameMargins.Bottom = 0) then
          Exit;
        SrcR := RectF(Cx, Cy + H - FFrameMargins.Bottom, Cx + FFrameMargins.Left, Cy + H);
        DstR := RectF(LocR.Left, LocR.Bottom - FFrameMargins.Bottom, LocR.Left + FFrameMargins.Left, LocR.Bottom);
      end;
    7:
      begin
        if (FFrameMargins.Bottom = 0) then
          Exit;
        SrcR := RectF(Cx + FFrameMargins.Left, Cy + H - FFrameMargins.Bottom, Cx + W - FFrameMargins.Right, Cy + H);
        DstR := RectF(LocR.Left + FFrameMargins.Left, LocR.Bottom - FFrameMargins.Bottom,
          LocR.Right - FFrameMargins.Right, LocR.Bottom);
      end;
    8:
      begin
        if (FFrameMargins.Right = 0) or (FFrameMargins.Bottom = 0) then
          Exit;
        SrcR := RectF(Cx + W - FFrameMargins.Right, Cy + H - FFrameMargins.Bottom, Cx + W, Cy + H);
        DstR := RectF(LocR.Right - FFrameMargins.Right, LocR.Bottom - FFrameMargins.Bottom, LocR.Right, LocR.Bottom);
      end;

  end;

  Result := True;
end;

procedure TSkin.UpdateIndexes;
begin
  if FStartFrame >= FFrameCount then
    FStartFrame := FFrameCount - 1;
  if FStopFrame >= FFrameCount then
    FStopFrame := FFrameCount - 1;
  if FStartFrame < 0 then
    FStartFrame := 0;
  if FStopFrame < 0 then
    FStopFrame := 0;
end;

{ TFrameTileStyle }

procedure TFrameTileStyle.Assign(Source: TPersistent);
begin
  if Source is TFrameTileStyle then
  begin
    FLeft := TFrameTileStyle(Source).FLeft;
    FTop := TFrameTileStyle(Source).FTop;
    FRight := TFrameTileStyle(Source).FRight;
    FBottom := TFrameTileStyle(Source).FBottom;
    FCenter := TFrameTileStyle(Source).FCenter;
  end
  else
    inherited
end;

procedure TFrameTileStyle.SetBottom(const Value: TTileStyle);
begin
  if FBottom <> Value then
  begin
    FBottom := Value;
    if Assigned(FOnChange) then
      FOnChange(Self);
  end;
end;

procedure TFrameTileStyle.SetCenter(const Value: TTileStyle);
begin
  if FCenter <> Value then
  begin
    FCenter := Value;
    if Assigned(FOnChange) then
      FOnChange(Self);
  end;
end;

procedure TFrameTileStyle.SetLeft(const Value: TTileStyle);
begin
  if FLeft <> Value then
  begin
    FLeft := Value;
    if Assigned(FOnChange) then
      FOnChange(Self);
  end;
end;

procedure TFrameTileStyle.SetRight(const Value: TTileStyle);
begin
  if FRight <> Value then
  begin
    FRight := Value;
    if Assigned(FOnChange) then
      FOnChange(Self);
  end;
end;

procedure TFrameTileStyle.SetTop(const Value: TTileStyle);
begin
  if FTop <> Value then
  begin
    FTop := Value;
    if Assigned(FOnChange) then
      FOnChange(Self);
  end;
end;

{ TSkinAnimation }

constructor TSkinAnimation.Create(AOwner: TComponent);
begin
  inherited;
  Duration := 0;
end;

procedure TSkinAnimation.ProcessAnimation;
begin
  if (Parent <> nil) and (Parent is TSkin) then
  begin
    if FInstance = nil then
      FInstance := TSkin(Parent);
    if Inverse then
    begin
      if NormalizedTime = 0 then
        FInstance.PaintSkin(FStartFrame, FStopFrame, 1, 0)
      else
        FInstance.PaintSkin(FStartFrame, FStopFrame, 1 - NormalizedTime, 1);
    end
    else
    begin
      if NormalizedTime = 1 then
        FInstance.PaintSkin(FStartFrame, FStopFrame, 0, 1)
      else
        FInstance.PaintSkin(FStartFrame, FStopFrame, 1, NormalizedTime);
    end;
  end;
end;

{ TBitmapLink }

constructor TBitmapLink.Create(ASkin: TSkin);
begin
  inherited Create;
  FSkin := ASkin;
end;

function TBitmapLink.FindSkin(const AStyleLookup: string): TSkin;
var
  Obj: TFmxObject;
begin
  Obj := nil;
  if (FSkin.FScene <> nil) then
    if (FSkin.FScene.GetStyleBook <> nil) and (FSkin.FScene.GetStyleBook.Root <> nil) then
      Obj := TControl(FSkin.FScene.GetStyleBook.Root.FindStyleResource(AStyleLookup));
  if Obj = nil then
    Obj := FMX.Types.FindStyleResource(AStyleLookup);
  if Obj = nil then
    if Application.DefaultStyles <> nil then
      Obj := TControl(Application.DefaultStyles.FindStyleResource(AStyleLookup));
  if (Obj <> nil) and (Obj is TSkin) then
    Result := TSkin(Obj)
  else
    Result := nil;
end;

function TBitmapLink.GetRect: TRect;
begin
  Result.Left := FLeft;
  Result.Top := FTop;
  Result.Right := FLeft + FWidth;
  Result.Bottom := FTop + FHeight;
end;

procedure TBitmapLink.SetChange;
begin
  if Assigned(FOnChange) then
    FOnChange(Self);
end;

procedure TBitmapLink.SetHeight(const Value: Integer);
begin
  FHeight := Value;
  if FHeight <> Value then
  begin
    FHeight := Value;
    SetChange;
  end;
end;

procedure TBitmapLink.SetLeft(const Value: Integer);
begin
  if FLeft <> Value then
  begin
    FLeft := Value;
    SetChange;
  end;
end;

procedure TBitmapLink.SetSkinLookup(const Value: String);
var
  S: TSkin;
begin
  if FSkinLookup <> Value then
  begin
    FSkinLookup := Value;
    S := FindSkin(Value);
    if S <> nil then
    begin
      FLeft := 0;
      FTop := 0;
      FWidth := S.Bitmap.Width;
      FHeight := S.Bitmap.Height;
      FSkin.FFrameCount := S.FrameCount;
      FSkin.FFrameMargins.Assign(S.FFrameMargins);
      FSkin.FFrameTileStyle.Assign(S.FFrameTileStyle);
      FSkin.FFrameOrientation := S.FFrameOrientation;
      FSkin.FFrameIndex := S.FFrameIndex;
      FBitmap := S.FBitmap;
    end
    else
    begin
      FLeft := 0;
      FTop := 0;
      FWidth := 0;
      FHeight := 0;
      FSkin.FFrameCount := 1;
      FSkin.FFrameMargins.Rect := RectF(0, 0, 0, 0);
      FSkin.FFrameTileStyle.FLeft := tsStretch;
      FSkin.FFrameTileStyle.FTop := tsStretch;
      FSkin.FFrameTileStyle.FRight := tsStretch;
      FSkin.FFrameTileStyle.FBottom := tsStretch;
      FSkin.FFrameTileStyle.FCenter := tsStretch;
      FSkin.FFrameOrientation := foHorizontal;
      FSkin.FFrameIndex := 0;
      FBitmap := nil;
    end;
    SetChange;
  end;
end;

procedure TBitmapLink.SetTop(const Value: Integer);
begin
  if FTop <> Value then
  begin
    FTop := Value;
    SetChange;
  end;
end;

procedure TBitmapLink.SetWidth(const Value: Integer);
begin
  if FWidth <> Value then
  begin
    FWidth := Value;
    SetChange;
  end;
end;

procedure Register;
begin
  RegisterComponents('Shapes', [TSkin]);
  RegisterComponents('Animations', [TSkinAnimation]);
end;

initialization

RegisterClasses([TSkin, TSkinAnimation]);

end.
