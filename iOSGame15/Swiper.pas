unit Swiper;

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
  TSwipeEvent = procedure (x, y : Double) of object;

  TiOSSwipeGestureRecognizer = class(TFmxObject)
  private
    { Private declarations }
    FOnSwipeRight : TSwipeEvent;
    FOnSwipeLeft : TSwipeEvent;
    FOnSwipeUp : TSwipeEvent;
    FOnSwipeDown : TSwipeEvent;
  protected
    { Protected declarations }
  public
    { Public declarations }
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  published
    { Published declarations }
    property OnSwipeRight: TSwipeEvent read FOnSwipeRight write FOnSwipeRight;
    property OnSwipeLeft: TSwipeEvent read FOnSwipeLeft write FOnSwipeLeft;
    property OnSwipeUp: TSwipeEvent read FOnSwipeUp write FOnSwipeUp;
    property OnSwipeDown: TSwipeEvent read FOnSwipeDown write FOnSwipeDown;
  end;

var
  GlobalSGR : TiOSSwipeGestureRecognizer;

procedure Register;

implementation

{$IFDEF FPC}
uses
  CGGeometry;
{$ENDIF}

{$IFDEF FPC}
type
  TSwipeDelegate = objcclass(NSObject)
    procedure swipe(gestureRecognizer: UIGestureRecognizer); message 'gestureRecognizer:';
  end;

var
  SwipeDelegate : TSwipeDelegate;
{$ENDIF}

{$IFDEF FPC}
procedure TSwipeDelegate.swipe(gestureRecognizer: UIGestureRecognizer);
var
  SGR : UISwipeGestureRecognizer;
  loc : CGPoint;
begin
  loc := gestureRecognizer.locationInView(gestureRecognizer.view);
  SGR := UISwipeGestureRecognizer(gestureRecognizer);
  if Assigned(GlobalSGR) then
    case SGR.direction of
      UISwipeGestureRecognizerDirectionRight :
        if Assigned(GlobalSGR.FOnSwipeRight) then
          GlobalSGR.FOnSwipeRight(loc.x,loc.y);
      UISwipeGestureRecognizerDirectionLeft :
        if Assigned(GlobalSGR.FOnSwipeLeft) then
          GlobalSGR.FOnSwipeLeft(loc.x,loc.y);
      UISwipeGestureRecognizerDirectionUp :
        if Assigned(GlobalSGR.FOnSwipeUp) then
          GlobalSGR.FOnSwipeUp(loc.x,loc.y);
      UISwipeGestureRecognizerDirectionDown :
        if Assigned(GlobalSGR.FOnSwipeDown) then
          GlobalSGR.FOnSwipeDown(loc.x,loc.y);
    end;
end;
{$ENDIF}

constructor TiOSSwipeGestureRecognizer.Create(AOwner: TComponent);
{$IFDEF FPC}
var
  SGR : UISwipeGestureRecognizer;
{$ENDIF}
begin
  if Assigned(GlobalSGR) then
    raise Exception.Create('I won''t let you have more than one of these things...');
  inherited;

{$IFDEF FPC}
  SwipeDelegate := TSwipeDelegate.alloc;
  SGR := UISwipeGestureRecognizer.alloc;
  SGR.initWithTarget_action(SwipeDelegate,objcselector('gestureRecognizer:'));
  SGR.setDirection(UISwipeGestureRecognizerDirectionRight);
  mainWindow.addGestureRecognizer(SGR);
  SGR.release;
  SGR := UISwipeGestureRecognizer.alloc;
  SGR.initWithTarget_action(SwipeDelegate,objcselector('gestureRecognizer:'));
  SGR.setDirection(UISwipeGestureRecognizerDirectionLeft);
  mainWindow.addGestureRecognizer(SGR);
  SGR.release;
  SGR := UISwipeGestureRecognizer.alloc;
  SGR.initWithTarget_action(SwipeDelegate,objcselector('gestureRecognizer:'));
  SGR.setDirection(UISwipeGestureRecognizerDirectionUp);
  mainWindow.addGestureRecognizer(SGR);
  SGR.release;
  SGR := UISwipeGestureRecognizer.alloc;
  SGR.initWithTarget_action(SwipeDelegate,objcselector('gestureRecognizer:'));
  SGR.setDirection(UISwipeGestureRecognizerDirectionDown);
  mainWindow.addGestureRecognizer(SGR);
  SGR.release;
{$ENDIF}

  GlobalSGR := Self;
end;

destructor TiOSSwipeGestureRecognizer.Destroy;
begin
  if GlobalSGR = Self then
    GlobalSGR := nil;
{$IFDEF FPC}
  SwipeDelegate.release;
{$ENDIF}
  inherited;
end;

procedure Register;
begin
  RegisterComponents('iOS', [TiOSSwipeGestureRecognizer]);
end;

end.

