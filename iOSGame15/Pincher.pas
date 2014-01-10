unit Pincher;

{$IFDEF FPC}
{$mode delphi}
{$modeswitch objectivec1}
{$ENDIF}

interface

uses
  SysUtils, Classes, FMX_Types
{$IFDEF FPC}
  , FMX_Platform_iOS, iPhoneAll
{$ENDIF}
  ;

type
  TZoomEvent = procedure (Scale : Double) of object;

  TiOSPinchGestureRecognizer = class(TFmxObject)
  private
    { Private declarations }
    FOnZoom : TZoomEvent;
  protected
    { Protected declarations }
  public
    { Public declarations }
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  published
    { Published declarations }
    property OnZoom: TZoomEvent read FOnZoom write FOnZoom;
  end;

var
  GlobalPGR : TiOSPinchGestureRecognizer;

procedure Register;

implementation

{$IFDEF FPC}
uses
  CGGeometry;
{$ENDIF}

{$IFDEF FPC}
type
  TPinchDelegate = objcclass(NSObject)
    procedure pinch(gestureRecognizer: UIGestureRecognizer); message 'gestureRecognizer:';
  end;

var
  PinchDelegate : TPinchDelegate;
{$ENDIF}

{$IFDEF FPC}
procedure TPinchDelegate.pinch(gestureRecognizer: UIGestureRecognizer);
var
  PGR : UIPinchGestureRecognizer;
  loc : CGPoint;
begin
  loc := gestureRecognizer.locationInView(gestureRecognizer.view);
  PGR := UIPinchGestureRecognizer(gestureRecognizer);
  if Assigned(GlobalPGR) then
    if Assigned(GlobalPGR.FOnZoom) then
      GlobalPGR.FOnZoom(PGR.scale);
end;
{$ENDIF}

constructor TiOSPinchGestureRecognizer.Create(AOwner: TComponent);
{$IFDEF FPC}
var
  PGR : UIPinchGestureRecognizer;
{$ENDIF}
begin
  if Assigned(GlobalPGR) then
    raise Exception.Create('I won''t let you have more than one of these things...');
  inherited;

{$IFDEF FPC}
  PinchDelegate := TPinchDelegate.alloc;
  PGR := UIPinchGestureRecognizer.alloc;
  PGR.initWithTarget_action(PinchDelegate,objcselector('gestureRecognizer:'));
  mainWindow.addGestureRecognizer(PGR);
  PGR.release;
{$ENDIF}

  GlobalPGR := Self;
end;

destructor TiOSPinchGestureRecognizer.Destroy;
begin
  if GlobalPGR = Self then
    GlobalPGR := nil;
{$IFDEF FPC}
  PinchDelegate.release;
{$ENDIF}
  inherited;
end;

procedure Register;
begin
  RegisterComponents('iOS', [TiOSPinchGestureRecognizer]);
end;

end.

