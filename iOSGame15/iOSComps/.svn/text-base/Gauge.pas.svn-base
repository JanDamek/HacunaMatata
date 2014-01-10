
//---------------------------------------------------------------------------

// This software is Copyright (c) 2011 Embarcadero Technologies, Inc. 
// You may only use this software if you are an authorized licensee
// of Delphi, C++Builder or RAD Studio (Embarcadero Products).
// This software is considered a Redistributable as defined under
// the software license agreement that comes with the Embarcadero Products
// and is subject to that software license agreement.

//---------------------------------------------------------------------------
unit Gauge;

interface

uses
  SysUtils, Classes, FMX_Types, FMX_Objects, UITypes;

type
  TiOSGauge = class(TCircle)
  private
    FMax: Double;
    FMin: Double;
    FMaxAngle: Double;
    FValue: Double;
    FMinAngle: Double;
    FNeedleCenter: TCircle;
    FNeedle: TRectangle;
    FZeroAngle: Double;
    FNeedleCenterColor: TAlphaColor;
    FNeedleColor: TAlphaColor;
    procedure SetMax(const Value: Double);
    procedure SetMaxAngle(const Value: Double);
    procedure SetMin(const Value: Double);
    procedure SetMinAngle(const Value: Double);
    procedure SetValue(const Value: Double);
    procedure SetZeroAngle(const Value: Double);
    procedure SetNeedleCenterColor(const Value: TAlphaColor);
    procedure SetNeedleColor(const Value: TAlphaColor);
    { Private declarations }
  protected
    { Protected declarations }
    procedure Resize; override;
    procedure UpdateNeedle;
  public
    { Public declarations }
    constructor Create(AOwner: TComponent); override;
  published
    { Published declarations }
    property Value : Double read FValue write SetValue;
    property Max : Double read FMax	write SetMax;
    property Min : Double read FMin	write SetMin;
    property MaxAngle : Double read FMaxAngle	write SetMaxAngle;
    property MinAngle : Double read FMinAngle	write SetMinAngle;
    property ZeroAngle : Double read FZeroAngle	write SetZeroAngle;
    property NeedleCenterColor : TAlphaColor read FNeedleCenterColor write SetNeedleCenterColor default TAlphaColorRec.Red;
    property NeedleColor : TAlphaColor read FNeedleColor write SetNeedleColor default TAlphaColorRec.Red;
  end;

procedure Register;

implementation

constructor TiOSGauge.Create(AOwner: TComponent);
begin
  inherited;
  FValue := 0;
  FMax := 100;
  FMin := 0;
  FMaxAngle := 360;
  FMinAngle := 0;
  FWidth := 140;
  FHeight := 140;
  FNeedleCenterColor := TAlphaColorRec.Red;
  FNeedleColor := TAlphaColorRec.Red;

  FNeedleCenter := TCircle.Create(Self);
  FNeedleCenter.Parent := Self;
  FNeedleCenter.Width := 10;
  FNeedleCenter.Height := 10;
  FNeedleCenter.Position.X := FWidth/2-5;
  FNeedleCenter.Position.Y := FHeight/2-5;
  FNeedleCenter.Fill.Color := FNeedleCenterColor;
  FNeedleCenter.Stored := False;

  FNeedle := TRectangle.Create(Self);
  FNeedle.Parent := FNeedleCenter;
  FNeedle.Width := 4;
  FNeedle.Height := FWidth/2*0.85;
  FNeedle.Position.X := 3;
  FNeedle.Position.Y := 6-FNeedle.Height;
  FNeedle.Fill.Color := FNeedleColor;
  FNeedle.Stored := False;
end;

procedure TiOSGauge.Resize;
begin
  inherited;
  FNeedleCenter.Position.X := FWidth/2-5;
  FNeedleCenter.Position.Y := FHeight/2-5;
  if FHeight < FWidth then
    FNeedle.Height := FHeight/2*0.85
  else
    FNeedle.Height := FWidth/2*0.85;
  FNeedle.Position.Y := 6-FNeedle.Height;
end;

procedure TiOSGauge.SetMax(const Value: Double);
begin
  FMax := Value;
  UpdateNeedle;
end;

procedure TiOSGauge.SetMaxAngle(const Value: Double);
begin
  FMaxAngle := Value;
  UpdateNeedle;
end;

procedure TiOSGauge.SetMin(const Value: Double);
begin
  FMin := Value;
  UpdateNeedle;
end;

procedure TiOSGauge.SetMinAngle(const Value: Double);
begin
  FMinAngle := Value;
  UpdateNeedle;
end;

procedure TiOSGauge.SetNeedleCenterColor(const Value: TAlphaColor);
begin
  FNeedleCenterColor := Value;
  FNeedleCenter.Fill.Color := Value;
end;

procedure TiOSGauge.SetNeedleColor(const Value: TAlphaColor);
begin
  FNeedleColor := Value;
  FNeedle.Fill.Color := Value;
end;

procedure TiOSGauge.SetValue(const Value: Double);
begin
  FValue := Value;
  if FValue < Min then
    FValue := Min;
  if FValue > Max then
    FValue := Max;
  UpdateNeedle;
end;

procedure TiOSGauge.SetZeroAngle(const Value: Double);
begin
  FZeroAngle := Value;
  UpdateNeedle;
end;

procedure TiOSGauge.UpdateNeedle;
begin
  FNeedleCenter.RotationAngle := FZeroAngle+FValue*(FMaxAngle-FMinAngle)/(FMax-FMin);
end;

procedure Register;
begin
  RegisterComponents('iOS', [TiOSGauge]);
end;

end.
