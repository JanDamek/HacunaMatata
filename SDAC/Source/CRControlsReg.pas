
//////////////////////////////////////////////////
//  CRControls
//  Copyright (c) 1998-2012 Devart. All right reserved.
//  CRControls registration
//////////////////////////////////////////////////

{$IFNDEF CLR}

unit CRControlsReg;
{$ENDIF}

{$I CRGrid.inc}

interface

uses
  Classes, CRGrid;

procedure Register;

implementation

{$IFNDEF CLR}
  {$IFDEF VER9}
    {$R CRControls9.res}
  {$ELSE}
    {$R CRControls.res}
  {$ENDIF}
  {$IFDEF VER10P}
    {$R CRControls10p.res}
  {$ENDIF}
{$ELSE}
  {$R CRControls.res}
{$ENDIF}

procedure Register;
begin
  RegisterComponents('Data Controls', [TCRDBGrid]);
end;

end.
