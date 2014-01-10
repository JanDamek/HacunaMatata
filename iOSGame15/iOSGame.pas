program iOSGame;

uses
  cwstring, cthreads, FMX_Forms,
  MainFormUnit in 'MainFormUnit.pas' {Form3},
  MainFormGalery in 'MainFormGalery.pas' {FrmMain},
  FMX_Platform_iOS in 'FMX_Platform_iOS.pas',
  Accelerometer in 'Accelerometer.pas';

{.$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TForm3, Form3);
  Application.Run;
end.
