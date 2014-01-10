{$IFNDEF CLR}
{$I Dac.inc}

unit DacReg;
{$ENDIF}

interface

procedure Register;
procedure RegisterCRBatchMove;

implementation

{$IFNDEF FPC}
{$IFNDEF CLR}
  {$IFDEF VER9}
    {$R DADesign9.res}
  {$ELSE}
    {$R DADesign.res}
  {$ENDIF}
  {$IFDEF VER10P}
    {$R DADesign10p.res}
  {$ENDIF}
{$ELSE}
  {$R DADesign.res}
{$ENDIF}
{$ENDIF}

uses
{$IFDEF FPC}
  LResources,
{$ENDIF}
  Classes, CRBatchMove, VirtualTable;

var
  CRBatchMoveRegistered: boolean;

procedure Register;
begin
  RegisterComponents('Data Access', [TVirtualTable]);
end;

procedure RegisterCRBatchMove;
begin
  if not CRBatchMoveRegistered then begin
    RegisterComponents('Data Access', [TCRBatchMove]);
    CRBatchMoveRegistered := True;
  end;
end;

initialization
{$IFDEF FPC}
  {$I DADesign.lrs}
{$ENDIF}
  CRBatchMoveRegistered := False;

end.
