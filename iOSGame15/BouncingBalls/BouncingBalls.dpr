program BouncingBalls;

uses
  FMX_Forms,
  Unit7 in 'Unit7.pas' {Form7: TForm3D};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TForm7, Form7);
  Application.Run;
end.
