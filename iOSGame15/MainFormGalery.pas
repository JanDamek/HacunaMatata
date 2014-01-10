unit MainFormGalery;

interface

uses
  SysUtils, Types, Classes, Variants, FMX_Types,
  FMX_Controls, FMX_Forms, FMX_Dialogs, FMX_Ani, FMX_Layers3D, FMX_Types3D,
  UITypes, FMX_Objects, FMX_Layouts, FMX_Filter_Effects, FMX_Effects,
  FMX_Colors, UIConsts, FMX_Edit, FMX_Platform;

type
  TFrmMain = class(TForm)
    Viewport3D1: TViewport3D;
    Coverflow: TLayout3D;
    ResourcesDark: TStyleBook;
    Rectangle1: TRectangle;
    RoundRect2: TRoundRect;
    TrackBar1: TTrackBar;
    AniIndicator1: TAniIndicator;
    Button1: TButton;
    TouchRectangle: TRectangle;
    AniIndicator2: TAniIndicator;
    procedure CoverScrollChange(Sender: TObject);
    procedure CoverflowMouseWheel(Sender: TObject; Shift: TShiftState; WheelDelta: Integer; var Handled: Boolean);
    procedure TrackBar1Change(Sender: TObject);
    procedure Button1Click(Sender: TObject);
    procedure TouchRectangleMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Single);
    procedure TouchRectangleMouseUp(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Single);
    procedure TouchRectangleMouseLeave(Sender: TObject);
    procedure TouchRectangleMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Single);
    procedure FormActivate(Sender: TObject);
  private
    fCoverIndex: Integer;
    isDown: Boolean;
    fX: Single;

    fInMove: Boolean;
    fDownTime: Double;

    procedure SetCoverIndex(AIndex: Integer);
    procedure DoCoverMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Single);
    property inMove: Boolean read fInMove write fInMove default false;

  Const
    Factor = 0.8;
    DivFac = 3;
    RotationYAngle = 70;
    Duration = 0.5;
  public
    { Public declarations }
    procedure AddImage(aImg: TImageControl; i: Integer);
    property CoverIndex: Integer read fCoverIndex;
  end;

var
  FrmMain: TFrmMain;

implementation

{$R *.lfm}

uses MainFormUnit;

procedure TFrmMain.AddImage(aImg: TImageControl; i: Integer);
var
  Cover: TLayer3D;
  Layout: TLayout;
  Image: TImageControl;
  Effect: TReflectionEffect;
  L: TRectangle;
begin
  // vloz\eni obrazku do galerie
  // Create Cover
  Cover := TLayer3D.Create(Self);
  Cover.Parent := Coverflow;

  Cover.Projection := TProjection.pjScreen;
  Cover.Width := Round(Coverflow.Height * Factor);
  Cover.Height := Round(Round(Coverflow.Height * Factor) * 1.5);
  Cover.ZWrite := true;
  Cover.Fill.Color := Viewport3D1.Color;
  Cover.Fill.Kind := TBrushKind.bkSolid;
  Cover.Transparency := true;
  Cover.OnLayerMouseDown := DoCoverMouseDown;
  Cover.HitTest := false;
  Cover.Tag := i;
  Cover.Padding.Rect := TRectF.Create(0, 0, 0, 0);
  Cover.Position.Y := Trunc((Coverflow.Height - Round(Coverflow.Height * Factor)) / 2);
  Cover.Cursor := crHandPoint;

  if i = 0 then
  begin
    Cover.Position.X := i * Round(Coverflow.Height * Factor);
  end
  else
  begin
    Cover.Position.X := (i + 1) * (Round(Coverflow.Height * Factor) div DivFac);
    Cover.Position.Z := Round(Coverflow.Height * Factor) * 2;
    Cover.RotationAngle.Y := RotationYAngle;
  end;

  // Child
  Layout := TLayout.Create(Self);
  Layout.Parent := Cover;
  Layout.Align := TAlignLayout.alTop;
  Layout.Height := Trunc(Cover.Height / 2); // original = 2
  Layout.Padding.Rect := TRectF.Create(0, 80, 0, 0);
  Layout.Cursor := crHandPoint;

  // This rectangle is necessary to avoid blank lines on the image
  L := TRectangle.Create(Self);
  L.Parent := Layout;
  L.Align := TAlignLayout.alTop;
  L.Height := Trunc(Cover.Height / 2);
  L.Fill.Kind := TBrushKind.bkNone;
  L.Stroke.Color := Viewport3D1.Color;
  L.Stroke.Kind := TBrushKind.bkNone;

  Image := TImageControl.Create(Self);
  Image.Parent := Layout;
  Image.Padding.Rect := TRectF.Create(0, 0, 0, 0);

  with DefaultBitmapCodecClass.GetImageSize(Image.TagString) do
  begin
    Image.Width := 1024;
    Image.Height := 1024;
  end;

  // Image.WrapMode := TImageWrapMode.iwStretch;
  Image.Align := TAlignLayout.alFit;
  Image.HitTest := true;
  Image.Cursor := crHandPoint;
  Image.BitMap.Assign(aImg.BitMap);
  // TImageThread.Create(Image, Image.TagString).Start;
  Image.OnMouseDown := DoCoverMouseDown;
  Image.HitTest := false;
  // Image.OnMouseWheel := CoverflowMouseWheel;

  Image.Tag := i;

  Effect := TReflectionEffect.Create(Self);
  Effect.Parent := Image;
  Effect.Opacity := 0.6;

  // Opacity animation
  Cover.Opacity := 0.01;
  Cover.AnimateFloat('Opacity', 1, Duration);

  // Load thumb
  Cover.TagObject := Image;

  // Inc(I);

  Application.ProcessMessages;

  fCoverIndex := 0;
  AniIndicator1.Visible := false;
  TrackBar1.Max := Coverflow.ChildrenCount - 1;
  TrackBar1.Value := 0;
  TrackBar1.Visible := true;
  TrackBar1.SetFocus;
end;

procedure TFrmMain.Button1Click(Sender: TObject);
begin
  ModalResult := mrOk;
end;

procedure TFrmMain.CoverflowMouseWheel(Sender: TObject; Shift: TShiftState; WheelDelta: Integer; var Handled: Boolean);
begin

  TrackBar1.Value := CoverIndex - (WheelDelta div 120);
  Handled := true;

end;

procedure TFrmMain.CoverScrollChange(Sender: TObject);
begin
  SetCoverIndex(Round(TrackBar1.Value));
end;

procedure TFrmMain.SetCoverIndex(AIndex: Integer);
var
  i: Integer;
  Cover: TLayer3D;
  PercCoeff, Coeff: Single;
begin
  if AniIndicator1.Visible or (Coverflow.ChildrenCount = 0) then
  begin
    TrackBar1.Value := CoverIndex;
    Abort;
  end;

  PercCoeff := 0.6;

  if AIndex < 0 then
    AIndex := 0;
  if AIndex >= Coverflow.ChildrenCount then
    AIndex := Coverflow.ChildrenCount - 1;
  if AIndex <> CoverIndex then
  begin
    { translate all }  // move the pictures in the back
    for i := 0 to Coverflow.ChildrenCount - 1 do
    begin

      Cover := TLayer3D(Coverflow.Children[i]);
      Cover.StopPropertyAnimation('Position.X');
      Cover.AnimateFloat('Position.X', Cover.Position.X + ((CoverIndex - AIndex) * (Round(Coverflow.Height * Factor)
        div DivFac)), Duration);
    end;

    { transform between old an new value }
    i := CoverIndex;
    while i <> AIndex do
    begin
      Coeff := (0.1 + (Abs(AIndex - i) / Abs(AIndex - CoverIndex))) * (PercCoeff + 0.1);

      Cover := TLayer3D(Coverflow.Children[i]);
      Cover.StopPropertyAnimation('Position.X');
      Cover.StopPropertyAnimation('RotationAngle.Y');

      if CoverIndex > AIndex then
      begin
        Cover.AnimateFloat('RotationAngle.Y', RotationYAngle, Duration);
        if i = CoverIndex then
          Cover.AnimateFloat('Position.X', Cover.Position.X + (1 * (Round(Coverflow.Height * Factor) div DivFac)),
            Duration * Coeff)
        else
          Cover.AnimateFloat('Position.X', Cover.Position.X + (2 * (Round(Coverflow.Height * Factor) div DivFac)),
            Duration * Coeff);
      end
      else
      begin
        Cover.AnimateFloat('RotationAngle.Y', RotationYAngle * -1, Duration);
        if i = CoverIndex then
          Cover.AnimateFloat('Position.X', Cover.Position.X - (1 * (Round(Coverflow.Height * Factor) div DivFac)),
            Duration * Coeff)
        else
          Cover.AnimateFloat('Position.X', Cover.Position.X - (2 * (Round(Coverflow.Height * Factor) div DivFac)),
            Duration * Coeff);
      end;
      Cover.AnimateFloat('Position.Z', Round(Coverflow.Height * Factor) * 2, Duration);
      if AIndex > CoverIndex then
        Inc(i)
      else
        Dec(i);
    end;

    Cover := TLayer3D(Coverflow.Children[AIndex]);

    Cover.StopPropertyAnimation('Position.X');
    Cover.StopPropertyAnimation('Position.Z');

    Cover.AnimateFloat('RotationAngle.Y', 0, Duration);
    Cover.AnimateFloat('Position.Z', 0, Duration);
    if CoverIndex > AIndex then
      Cover.AnimateFloat('Position.X', Cover.Position.X + (1 * (Round(Coverflow.Height * Factor) div DivFac)), Duration)
    else
      Cover.AnimateFloat('Position.X', Cover.Position.X - (1 * (Round(Coverflow.Height * Factor) div DivFac)),
        Duration);

    fCoverIndex := AIndex;
    TrackBar1.Value := fCoverIndex;
  end;
end;

procedure TFrmMain.TrackBar1Change(Sender: TObject);
begin
  SetCoverIndex(Round(TrackBar1.Value));
end;

procedure TFrmMain.DoCoverMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Single);
begin
  TrackBar1.Value := Round(StrToFloat(IntToStr(TImage(Sender).Tag)));
end;

procedure TFrmMain.FormActivate(Sender: TObject);
begin
  fDownTime := now;
end;

procedure TFrmMain.TouchRectangleMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Single);
begin
  fDownTime := (now - fDownTime) * 10000;
  if (fDownTime > 0.01) and (fDownTime < 0.05) then
    Button1Click(Self);

  // Button1.Text := Format('%f',[fDownTime]);

  fDownTime := now;
  isDown := true;
  fX := X;
  inMove := false;
end;

procedure TFrmMain.TouchRectangleMouseLeave(Sender: TObject);
begin
  isDown := false;
  inMove := false;
end;

procedure TFrmMain.TouchRectangleMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Single);
begin
  if isDown and not inMove then
  begin
    if X + 40 < fX then
    begin
      inMove := true;
      isDown := false;
      SetCoverIndex(CoverIndex + 1);
      fX := X;
      inMove := false;
    end;
    if X - 40 > fX then
    begin
      inMove := true;
      isDown := false;
      SetCoverIndex(CoverIndex - 1);
      fX := X;
      inMove := false;
    end;
  end;
end;

procedure TFrmMain.TouchRectangleMouseUp(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Single);
begin
  isDown := false;
  inMove := false;
  fDownTime := now;
end;

end.
