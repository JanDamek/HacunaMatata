program HacunaMatata;

uses
  FMX.Forms,
  ddPlugin_TLB,
  sqProc,
  AcunaMatataUnit in 'AcunaMatataUnit.pas' {AcunaMatataForm};

{$R *.res}

begin
  Application.Initialize;

  fHeliosGlobal := IHelios.Create;

  Application.CreateForm(TAcunaMatataForm, AcunaMatataForm);
  Application.Run;
end.
