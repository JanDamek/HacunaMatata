
//---------------------------------------------------------------------------

// This software is Copyright (c) 2011 Embarcadero Technologies, Inc. 
// You may only use this software if you are an authorized licensee
// of Delphi, C++Builder or RAD Studio (Embarcadero Products).
// This software is considered a Redistributable as defined under
// the software license agreement that comes with the Embarcadero Products
// and is subject to that software license agreement.

//---------------------------------------------------------------------------
unit Unit7;

interface

uses
  SysUtils, Types, UITypes, Classes, Variants, FMX_Types, FMX_Controls, FMX_Forms,
  FMX_Dialogs, FMX_Layers3D, FMX_Objects3D, FMX_Types3D;

type
  TForm7 = class(TForm3D)
    Sphere1: TSphere;
    Sphere2: TSphere;
    FPSTimer: TTimer;
    Text3D1: TText3D;
    Light1: TLight;
    Light2: TLight;
    Layer3D1: TLayer3D;
    LeftPlane: TPlane;
    RightPlane: TPlane;
    BackPlane: TPlane;
    BottomPlane: TPlane;
    TopPlane: TPlane;
    ScrollBar1: TScrollBar;
    Camera1: TCamera;
    Light3: TLight;
    Light4: TLight;
    Layout3D1: TLayout3D;
    RotateBoxX: TCheckBox;
    RotateBoxY: TCheckBox;
    RotateBoxZ: TCheckBox;
    Label1: TLabel;
    SpinLargeBall: TCheckBox;
    SpinSmallBall: TCheckBox;
    MoveLargeBall: TCheckBox;
    MoveSmallBall: TCheckBox;
    AnimationTimer: TTimer;
    procedure FPSTimerTimer(Sender: TObject);
    procedure BackPlaneMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Single; RayPos, RayDir: TVector3D);
    procedure BackPlaneMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Single;
      RayPos, RayDir: TVector3D);
    procedure AnimationTimerTimer(Sender: TObject);
    procedure Form3DCreate(Sender: TObject);
  private
    { Private declarations }
    procedure AppIdle(Sender: TObject; var Done: Boolean);
    procedure Animate;
  public
    { Public declarations }
  end;

var
  Form7: TForm7;

implementation

{$R *.lfm}

var
  dX1 : Single = 0.001;
  dY1 : Single = 0.001;
  dZ1 : Single = 0.001;
  dX2 : Single = -0.001;
  dY2 : Single = 0.001;
  dZ2 : Single = -0.001;
  FPS : Single = 0;
  Down : TPointF;
  PrevTime : Double = 0;
  CurrTime : Double;
  DT : Double;

procedure TForm7.Animate;
var
  V : Single;
begin
  CurrTime := Now;
  if PrevTime <> 0 then
    DT := CurrTime-PrevTime;
  PrevTime := CurrTime;

//  V := ScrollBar1.Value/5;
  V := 4000000.0 * DT * ScrollBar1.Value;

  if MoveLargeBall.IsChecked then
    with Sphere1.Position do begin
      X := X + dX1*V; if Abs(X) > 7.0 then begin dX1 := -dX1; X := X + 2*dX1*V; end;
      Y := Y + dY1*V; if Abs(Y) > 5.0 then begin dY1 := -dY1; Y := Y + 2*dY1*V; end;
      Z := Z + dZ1*V; if Abs(Z) > 5.0 then begin dZ1 := -dZ1; Z := Z + 2*dZ1*V; end;
    end;

  if MoveSmallBall.IsChecked then
    with Sphere2.Position do begin
      X := X + dX2*V; if Abs(X) > 8.0 then begin dX2 := -dX2; X := X + 2*dX2*V; end;
      Y := Y + dY2*V; if Abs(Y) > 6.0 then begin dY2 := -dY2; Y := Y + 2*dY2*V; end;
      Z := Z + dZ2*V; if Abs(Z) > 6.0 then begin dZ2 := -dZ2; Z := Z + 2*dZ2*V; end;
    end;

  if VectorDistance2(Sphere1.AbsolutePosition,Sphere2.AbsolutePosition) <= Sqr(2+1) then begin
    dX1 := -dX1; dY1 := -dY1; dZ1 := -dZ1;
    dX2 := -dX2; dY2 := -dY2; dZ2 := -dZ2;
  end;

  if SpinLargeBall.IsChecked then begin
    Sphere1.RotationAngle.X := Sphere1.RotationAngle.X+0.01*V;
    Sphere1.RotationAngle.Y := Sphere1.RotationAngle.Y+0.01*V;
  end;

  if SpinSmallBall.IsChecked then begin
    Sphere2.RotationAngle.Y := Sphere2.RotationAngle.Y-0.01*V;
    Sphere2.RotationAngle.Z := Sphere2.RotationAngle.Z-0.01*V;
  end;

  if RotateBoxX.IsChecked then
    Layout3D1.RotationAngle.X := Layout3D1.RotationAngle.X+0.001*V;
  if RotateBoxY.IsChecked then
    Layout3D1.RotationAngle.Y := Layout3D1.RotationAngle.Y+0.001*V;
  if RotateBoxZ.IsChecked then
    Layout3D1.RotationAngle.Z := Layout3D1.RotationAngle.Z+0.001*V;

  FPS := FPS+1;
end;

procedure TForm7.AnimationTimerTimer(Sender: TObject);
begin
  Animate; // For MacOSX
end;

procedure TForm7.AppIdle(Sender: TObject; var Done: Boolean);
begin
  Animate; // For Windows
end;

procedure TForm7.BackPlaneMouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Single; RayPos, RayDir: TVector3D);
begin
  Down := PointF(X, Y);
end;

procedure TForm7.BackPlaneMouseMove(Sender: TObject; Shift: TShiftState; X,
  Y: Single; RayPos, RayDir: TVector3D);
begin
  if (ssLeft in Shift) then
  begin
    { rotate Z }
    Camera.RotationAngle.X := Camera.RotationAngle.X + ((Y - Down.Y) * 0.2);
    { rotate X }
    Camera.RotationAngle.Y := Camera.RotationAngle.Y + ((X - Down.X) * 0.2);
    Down := PointF(X, Y);
  end;
end;

procedure TForm7.Form3DCreate(Sender: TObject);
begin
  Application.OnIdle := AppIdle;
end;

procedure TForm7.FPSTimerTimer(Sender: TObject);
begin
  FPS := FPS/FPSTimer.Interval*1000;
  Text3D1.Text := FloatToStr(FPS)+' fps';
  FPS := 0;
end;

end.

