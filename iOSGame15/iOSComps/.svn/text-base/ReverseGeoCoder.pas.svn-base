
//---------------------------------------------------------------------------

// This software is Copyright (c) 2011 Embarcadero Technologies, Inc. 
// You may only use this software if you are an authorized licensee
// of Delphi, C++Builder or RAD Studio (Embarcadero Products).
// This software is considered a Redistributable as defined under
// the software license agreement that comes with the Embarcadero Products
// and is subject to that software license agreement.

//---------------------------------------------------------------------------
unit ReverseGeoCoder;

{$IFDEF FPC}
{$mode delphi}
{$modeswitch objectivec1}
{$ENDIF}

interface

uses
  SysUtils, Classes, FMX_Types
{$IFDEF FPC}
  , iPhoneAll
{$ENDIF}
  ;

type
  TPlaceMark = record
    Thoroughfare,
    SubThoroughfare,
    Locality,
    SubLocality,
    AdministrativeArea,
    SubAdministrativeArea,
    PostalCode,
    Country,
    CountryCode,
    Street,
    StreetNumber,
    Address,
    City,
    State : String;
  end;

type
  TFoundPlaceMarkEvent = procedure(PlaceMark : TPlaceMark) of object;
  TErrorEvent = procedure of Object;

type
  TiOSReverseGeoCoder = class(TFmxObject)
  private
    FOnFoundPlaceMark: TFoundPlaceMarkEvent;
    FOnError: TErrorEvent;
    FLongitude: Double;
    FLatitude: Double;
    { Private declarations }
  protected
    { Protected declarations }
  public
    { Public declarations }
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Execute;
  published
    { Published declarations }
    property OnFoundPlaceMark: TFoundPlaceMarkEvent read FOnFoundPlaceMark write FOnFoundPlaceMark;
    property OnError: TErrorEvent read FOnError write FOnError;
    property Longitude: Double read FLongitude write FLongitude;
    property Latitude: Double read FLatitude write FLatitude;
  end;

var
  GlobalReverseGeoCoder: TiOSReverseGeoCoder;

procedure Register;

implementation

{$IFDEF FPC}
uses
  MapKit, CoreLocation;
{$ENDIF}

{$IFDEF FPC}
type
  GeoDelegate = objcclass(NSObject)
    reverseGeoCoder : MKReverseGeoCoder;
    procedure reverseGeocoder_didFindPlacemark(geocoder: MKReverseGeocoder; placemark: MKPlacemark); message 'reverseGeocoder:didFindPlacemark:';
    procedure reverseGeocoder_didFailWithError(geocoder: MKReverseGeocoder; error: NSError); message 'reverseGeocoder:didFailWithError:';
  end;
{$ENDIF}

{$IFDEF FPC}
var
  GeoDelegateVar : GeoDelegate;
{$ENDIF}

{$IFDEF FPC}
procedure GeoDelegate.reverseGeocoder_didFindPlacemark(geocoder: MKReverseGeocoder; placemark: MKPlacemark);
var
  PM : TPlaceMark;
begin
  PM.Thoroughfare := String(PChar(placemark.thoroughfare.UTF8String));
  PM.SubThoroughfare := String(PChar(placemark.subThoroughfare.UTF8String));
  PM.Locality := String(PChar(placemark.locality.UTF8String));
  PM.SubLocality := String(PChar(placemark.subLocality.UTF8String));
  PM.AdministrativeArea := String(PChar(placemark.administrativeArea.UTF8String));
  PM.SubAdministrativeArea := String(PChar(placemark.subAdministrativeArea.UTF8String));
  PM.PostalCode := String(PChar(placemark.postalCode.UTF8String));
  PM.Country := String(PChar(placemark.country.UTF8String));
  PM.CountryCode := String(PChar(placemark.countryCode.UTF8String));
  PM.Street := String(PChar(placemark.thoroughfare.UTF8String));
  PM.StreetNumber := String(PChar(placemark.SubThoroughfare.UTF8String));
  if PM.StreetNumber <> '' then
    PM.Address := PM.StreetNumber+' '+PM.Street
  else
    PM.Address := PM.Street;
  PM.City := String(PChar(placemark.locality.UTF8String));
  PM.State := String(PChar(placemark.administrativeArea.UTF8String));

  if Assigned(GlobalReverseGeoCoder) then
    if Assigned(GlobalReverseGeoCoder.FOnFoundPlaceMark) then
      GlobalReverseGeoCoder.FOnFoundPlaceMark(PM);
end;
{$ENDIF}

{$IFDEF FPC}
procedure GeoDelegate.reverseGeocoder_didFailWithError(geocoder: MKReverseGeocoder; error: NSError);
begin
  if Assigned(GlobalReverseGeoCoder) then
    if Assigned(GlobalReverseGeoCoder.FOnError) then
      GlobalReverseGeoCoder.FOnError;
end;
{$ENDIF}

constructor TiOSReverseGeoCoder.Create(AOwner: TComponent);
begin
  if Assigned(GlobalReverseGeoCoder) then
    raise Exception.Create('I won''t let you have more than one of these things...');
  inherited;

  // Embarcadero, 5617 Scotts Valley Dr, Scotts Valley, CA, USA
  FLatitude := 37.062915;
  FLongitude := -122.007353;

  GlobalReverseGeoCoder := Self;
end;

destructor TiOSReverseGeoCoder.Destroy;
begin
  if GlobalReverseGeoCoder = Self then
    GlobalReverseGeoCoder := nil;
{$IFDEF FPC}
  GeoDelegateVar.reverseGeoCoder.release;
  GeoDelegateVar.release;
{$ENDIF}
  inherited;
end;

procedure TiOSReverseGeoCoder.Execute;
{$IFDEF FPC}
var
  Coordinate : CLLocationCoordinate2D;
{$ENDIF}
begin
{$IFDEF FPC}
	GeoDelegateVar := GeoDelegate.alloc.init;
  GeoDelegateVar.reverseGeoCoder := MKReverseGeocoder.alloc;

  Coordinate.Latitude := FLatitude;
  Coordinate.Longitude := FLongitude;

  GeoDelegateVar.reverseGeoCoder.initWithCoordinate(Coordinate);
	GeoDelegateVar.reverseGeoCoder.setDelegate(GeoDelegateVar);
  GeoDelegateVar.reverseGeoCoder.start;
{$ENDIF}
end;

procedure Register;
begin
  RegisterComponents('iOS', [TiOSReverseGeoCoder]);
end;

end.

