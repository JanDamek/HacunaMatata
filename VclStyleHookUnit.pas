unit VclStyleHookUnit;

interface

uses Vcl.Themes, Vcl.Styles, Vcl.Forms, Vcl.Graphics;

type
  TFormStyleHookBackround = class(TFormStyleHook)
  strict private
  type
    TSettings = class
    strict private
      FColor: TColor;
      FImageLocation: string;
      FBitmap: TBitmap;
      FUseColor: Boolean;
      FUseImage: Boolean;
      FEnabled: Boolean;
      procedure SetColor(const Value: TColor);
      procedure SetImageLocation(const Value: string);
      procedure SetUseColor(const Value: Boolean);
      procedure SetUseImage(const Value: Boolean);
    public
      property UseImage: Boolean read FUseImage write SetUseImage;
      property UseColor: Boolean read FUseColor write SetUseColor;
      property Color: TColor read FColor write SetColor;
      property ImageLocation: string read FImageLocation write SetImageLocation;
      property Bitmap: TBitmap read FBitmap;
      property Enabled: Boolean read FEnabled write FEnabled;
      constructor Create;
      destructor Destroy; override;
    end;
    class var FNCSettings: TSettings;
    class var FBackGroundSettings: TSettings;
    class var FMergeImages: Boolean;
  protected
    procedure PaintNC(Canvas: TCanvas); override;
    procedure PaintBackground(Canvas: TCanvas); override;
    class constructor Create;
    class destructor Destroy;
  public
    class property MergeImages: Boolean read FMergeImages write FMergeImages;
    class property NCSettings: TSettings read FNCSettings;
    class property BackGroundSettings: TSettings read FBackGroundSettings;
  end;

implementation

uses
  Winapi.Windows, System.Types;

procedure TFormStyleHookBackround.PaintBackground(Canvas: TCanvas);
var
  LRect   : TRect;
  RBitmap : TRect;
  L,H     : Integer;
begin
  //if the option is not enabled use the default inherited PaintBackground method
  if not BackGroundSettings.Enabled then
   inherited
  else
  begin
    //get he bounds of the control (form)
    LRect := Rect(0, 0, Control.ClientWidth, Control.ClientHeight);
    //use a custom color for the background?
    if  BackGroundSettings.UseColor then
    begin
     Canvas.Brush.Color:=BackGroundSettings.Color;
     Canvas.FillRect(LRect);
    end
    else
    //use a bitmap
    begin
      //check the size of the bitmap against the control bounds to detrine how the bitmap is drawn
      if (BackGroundSettings.Bitmap.Width<LRect.Width) or (BackGroundSettings.Bitmap.Height<LRect.Height) then
      begin
       Canvas.Brush.Bitmap := BackGroundSettings.BitMap;
       Canvas.FillRect(LRect);
      end
      else
      begin
       //check if the the background bitmap must be merged with non client area bitmap
       if not FMergeImages then
        Canvas.CopyRect(LRect,BackGroundSettings.Bitmap.Canvas,LRect)
       else
       begin
        RBitmap:=LRect;
        H:=_GetBorderSize.Top;
        L:=_GetBorderSize.Left;
        RBitmap.SetLocation(L, H);
        Canvas.CopyRect(LRect,BackGroundSettings.Bitmap.Canvas,RBitmap);
       end;
      end;
    end;
  end;
end;

procedure TFormStyleHookBackround.PaintNC(Canvas: TCanvas);
var
  LDetail: TThemedWindow;
  LDetails,
  CaptionDetails,
  IconDetails   : TThemedElementDetails;
  R, R1, DrawRect, ButtonRect, TextRect: TRect;
  CaptionBuffer: TBitmap;
  FButtonState: TThemedWindow;
  TextFormat: TTextFormat;
  LText: string;
  SrcBackRect     : TRect;
begin
  //if the setting is not enabled use the original PaintNC method
  if not NCSettings.Enabled then
  begin
   inherited ;
   exit;
  end;

  //check the border style of the form
  if Form.BorderStyle = bsNone then
  begin
    MainMenuBarHookPaint(Canvas);
    Exit;
  end;


  {init some parameters}
  _FCloseButtonRect := Rect(0, 0, 0, 0);
  _FMaxButtonRect := Rect(0, 0, 0, 0);
  _FMinButtonRect := Rect(0, 0, 0, 0);
  _FHelpButtonRect := Rect(0, 0, 0, 0);
  _FSysMenuButtonRect := Rect(0, 0, 0, 0);
  _FCaptionRect := Rect(0, 0, 0, 0);

  if not StyleServices.Available then
    Exit;
  R := _GetBorderSize;

  {draw caption}

  if (Form.BorderStyle <> bsToolWindow) and
     (Form.BorderStyle <> bsSizeToolWin) then
  begin
    if _FFormActive then
      LDetail := twCaptionActive
    else
      LDetail := twCaptionInActive
  end
  else
  begin
   if _FFormActive then
      LDetail := twSmallCaptionActive
    else
      LDetail := twSmallCaptionInActive
  end;
  CaptionBuffer := TBitmap.Create;
  CaptionBuffer.SetSize(_FWidth, R.Top);

  {draw caption border}
  DrawRect := Rect(0, 0, CaptionBuffer.Width, CaptionBuffer.Height);
  LDetails := StyleServices.GetElementDetails(LDetail);  //used for draw text in the caption

  //check if a must use a custom color or a bitmap
  if FNCSettings.UseColor then
  begin
    //use the select color to fill the background of the canvas
    CaptionBuffer.Canvas.Brush.Color:=FNCSettings.Color;
    CaptionBuffer.Canvas.FillRect(DrawRect);
  end
  else
  begin
    //use the bitmap to fill the canvas
    SrcBackRect.Left:=0;
    SrcBackRect.Top:=0;
    SrcBackRect.Width:=DrawRect.Width;
    SrcBackRect.Height:=DrawRect.Height;
    //SrcBackRect.SetLocation(FNCSettings.Bitmap.Width-DrawRect.Width, 0);
    //SrcBackRect.SetLocation(_GetBorderSize.Width, 0);
    CaptionBuffer.Canvas.CopyRect(DrawRect, FNCSettings.Bitmap.Canvas,SrcBackRect);
  end;

  TextRect := DrawRect;
  CaptionDetails := LDetails;

  {draw icon}
  if (biSystemMenu in TCustomFormHack(Form).BorderIcons) and
     (Form.BorderStyle <> bsDialog) and
     (Form.BorderStyle <> bsToolWindow) and
     (Form.BorderStyle <> bsSizeToolWin) then
  begin
    IconDetails := StyleServices.GetElementDetails(twSysButtonNormal);
    if not StyleServices.GetElementContentRect(0, IconDetails, DrawRect, ButtonRect) then
      ButtonRect := Rect(0, 0, 0, 0);
    R1 := Rect(0, 0, GetSystemMetrics(SM_CXSMICON), GetSystemMetrics(SM_CYSMICON));
    RectVCenter(R1, ButtonRect);
    if ButtonRect.Width > 0 then
      DrawIconEx(CaptionBuffer.Canvas.Handle, R1.Left, R1.Top, _GetIconFast.Handle, 0, 0, 0, 0, DI_NORMAL);
    Inc(TextRect.Left, ButtonRect.Width + 5);
    _FSysMenuButtonRect := ButtonRect;
  end
  else
    Inc(TextRect.Left, R.Left);

  {draw buttons}
  if (biSystemMenu in TCustomFormHack(Form).BorderIcons) then
  begin
    if (Form.BorderStyle <> bsToolWindow) and
       (Form.BorderStyle <> bsSizeToolWin) then
    begin
      if (_FPressedButton = HTCLOSE) and (_FHotButton = HTCLOSE) then
        FButtonState := twCloseButtonPushed
      else if _FHotButton = HTCLOSE then
        FButtonState := twCloseButtonHot
      else
        if _FFormActive then
          FButtonState := twCloseButtonNormal
        else
          FButtonState := twCloseButtonDisabled;
     end
    else
    begin
      if (_FPressedButton = HTCLOSE) and (_FHotButton = HTCLOSE) then
        FButtonState := twSmallCloseButtonPushed
      else if _FHotButton = HTCLOSE then
        FButtonState := twSmallCloseButtonHot
      else
        if _FFormActive then
          FButtonState := twSmallCloseButtonNormal
        else
          FButtonState := twSmallCloseButtonDisabled;
    end;

    LDetails := StyleServices.GetElementDetails(FButtonState);
    if not StyleServices.GetElementContentRect(0, LDetails, DrawRect, ButtonRect) then
      ButtonRect := Rect(0, 0, 0, 0);

    StyleServices.DrawElement(CaptionBuffer.Canvas.Handle, LDetails, ButtonRect);

    if ButtonRect.Left > 0 then
      TextRect.Right := ButtonRect.Left;
    _FCloseButtonRect := ButtonRect;
  end;

  if (biMaximize in TCustomFormHack(Form).BorderIcons) and
     (biSystemMenu in TCustomFormHack(Form).BorderIcons) and
     (Form.BorderStyle <> bsDialog) and
     (Form.BorderStyle <> bsToolWindow) and
     (Form.BorderStyle <> bsSizeToolWin) then
  begin
    if Form.WindowState = wsMaximized then
    begin
      if (_FPressedButton = HTMAXBUTTON) and (_FHotButton = HTMAXBUTTON) then
        FButtonState := twRestoreButtonPushed
      else if _FHotButton = HTMAXBUTTON then
        FButtonState := twRestoreButtonHot
      else
      if _FFormActive then
        FButtonState := twRestoreButtonNormal
      else
        FButtonState := twRestoreButtonDisabled;
    end
    else
    begin
      if (_FPressedButton = HTMAXBUTTON) and (_FHotButton = HTMAXBUTTON) then
        FButtonState := twMaxButtonPushed
      else if _FHotButton = HTMAXBUTTON then
        FButtonState := twMaxButtonHot
      else
      if _FFormActive then
        FButtonState := twMaxButtonNormal
      else
        FButtonState := twMaxButtonDisabled;
    end;
    LDetails := StyleServices.GetElementDetails(FButtonState);

    if not StyleServices.GetElementContentRect(0, LDetails, DrawRect, ButtonRect) then
      ButtonRect := Rect(0, 0, 0, 0);
    if ButtonRect.Width > 0 then
      StyleServices.DrawElement(CaptionBuffer.Canvas.Handle, LDetails, ButtonRect);
    if ButtonRect.Left > 0 then
      TextRect.Right := ButtonRect.Left;
    _FMaxButtonRect := ButtonRect;
  end;

  if (biMinimize in TCustomFormHack(Form).BorderIcons) and
     (biSystemMenu in TCustomFormHack(Form).BorderIcons) and
     (Form.BorderStyle <> bsDialog) and
     (Form.BorderStyle <> bsToolWindow) and
     (Form.BorderStyle <> bsSizeToolWin) then
  begin
    if (_FPressedButton = HTMINBUTTON) and (_FHotButton = HTMINBUTTON) then
      FButtonState := twMinButtonPushed
    else if _FHotButton = HTMINBUTTON then
      FButtonState := twMinButtonHot
    else
      if _FFormActive then
        FButtonState := twMinButtonNormal
      else
        FButtonState := twMinButtonDisabled;

    LDetails := StyleServices.GetElementDetails(FButtonState);

    if not StyleServices.GetElementContentRect(0, LDetails, DrawRect, ButtonRect) then
      ButtonRect := Rect(0, 0, 0, 0);
    if ButtonRect.Width > 0 then
      StyleServices.DrawElement(CaptionBuffer.Canvas.Handle, LDetails, ButtonRect);
    if ButtonRect.Left > 0 then TextRect.Right := ButtonRect.Left;
    _FMinButtonRect := ButtonRect;
  end;

  if (biHelp in TCustomFormHack(Form).BorderIcons) and (biSystemMenu in TCustomFormHack(Form).BorderIcons) and
     ((not (biMaximize in TCustomFormHack(Form).BorderIcons) and
     not (biMinimize in TCustomFormHack(Form).BorderIcons)) or (Form.BorderStyle = bsDialog))
  then
  begin
    if (_FPressedButton = HTHELP) and (_FHotButton = HTHELP) then
      FButtonState := twHelpButtonPushed
    else if _FHotButton = HTHELP then
      FButtonState := twHelpButtonHot
    else
    if _FFormActive then
      FButtonState := twHelpButtonNormal
    else
      FButtonState := twHelpButtonDisabled;
    LDetails := StyleServices.GetElementDetails(FButtonState);

    if not StyleServices.GetElementContentRect(0, LDetails, DrawRect, ButtonRect) then
      ButtonRect := Rect(0, 0, 0, 0);
    if ButtonRect.Width > 0 then
      StyleServices.DrawElement(CaptionBuffer.Canvas.Handle, LDetails, ButtonRect);

    if ButtonRect.Left > 0 then
      TextRect.Right := ButtonRect.Left;
    _FHelpButtonRect := ButtonRect;
  end;

  {draw text}
  TextFormat := [tfLeft, tfSingleLine, tfVerticalCenter];
  if Control.UseRightToLeftReading then
    Include(TextFormat, tfRtlReading);

  LText := Text;
  StyleServices.DrawText(CaptionBuffer.Canvas.Handle, CaptionDetails, LText, TextRect, TextFormat);
  _FCaptionRect := TextRect;

  {draw caption buffer}

  Canvas.Draw(0, 0, CaptionBuffer);
  CaptionBuffer.Free;

  {draw menubar}
  MainMenuBarHookPaint(Canvas);

  {draw left border}
  DrawRect := Rect(0, R.Top, R.Left, _FHeight - R.Bottom);
  if DrawRect.Bottom - DrawRect.Top > 0 then
    //use a color?
    if FNCSettings.UseColor then
    begin
      Canvas.Brush.Color:=FNCSettings.Color;
      Canvas.FillRect(DrawRect);
    end
    else
    begin
      if (DrawRect.Height<=FNCSettings.BitMap.Height) and (DrawRect.Width<=FNCSettings.BitMap.Width)  then
        Canvas.CopyRect(DrawRect,FNCSettings.Bitmap.Canvas,DrawRect)
      else
        Canvas.StretchDraw(DrawRect, FNCSettings.BitMap);
    end;

  {draw right border}
  DrawRect := Rect(_FWidth - R.Right, R.Top, _FWidth, _FHeight - R.Bottom);

  if DrawRect.Bottom - DrawRect.Top > 0 then
    //use a color?
    if FNCSettings.UseColor then
    begin
      Canvas.Brush.Color:=FNCSettings.Color;
      Canvas.FillRect(DrawRect);
    end
    else
    begin
      if (DrawRect.Height<=FNCSettings.BitMap.Height) and (Control.Width<=FNCSettings.BitMap.Width)  then
        Canvas.CopyRect(DrawRect,FNCSettings.Bitmap.Canvas,DrawRect)
      else
        Canvas.StretchDraw(DrawRect, FNCSettings.BitMap);
    end;

  {draw Bottom border}
  DrawRect := Rect(0, _FHeight - R.Bottom, _FWidth, _FHeight);

  if DrawRect.Bottom - DrawRect.Top > 0 then
    //use a color?
    if FNCSettings.UseColor then
    begin
      Canvas.Brush.Color:=FNCSettings.Color;
      Canvas.FillRect(DrawRect);
    end
    else
    begin
      if (DrawRect.Height<=FNCSettings.BitMap.Height) and (Control.Width<=FNCSettings.BitMap.Width)  then
        Canvas.CopyRect(DrawRect,FNCSettings.Bitmap.Canvas,DrawRect)
      else
      begin
        SrcBackRect.Left:=0;
        SrcBackRect.Top:=0;
        SrcBackRect.Width:=DrawRect.Width;
        SrcBackRect.Height:=DrawRect.Height;
        SrcBackRect.SetLocation(FNCSettings.BitMap.Width-DrawRect.Width, 0);
        Canvas.CopyRect(DrawRect, FNCSettings.BitMap.Canvas,SrcBackRect);
      end;
    end;
end;

end.
