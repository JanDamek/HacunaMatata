
//////////////////////////////////////////////////
//  SQL Server Access Components
//  Copyright © 1998-2012 Devart. All right reserved.
//  SDAC Stored Proc Call Generator Frame
//////////////////////////////////////////////////

{$IFNDEF CLR}

{$I Sdac.inc}

unit MSSPCallFrame;
{$ENDIF}

interface

uses
{$IFDEF FPC}
  LResources,
{$ENDIF}
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms,
  Dialogs, DASPCallFrame, StdCtrls, Buttons, ExtCtrls, DBAccess;

type
  TMSSPCallFrame = class(TDASPCallFrame)
    cbSystem: TCheckBox;
    procedure cbSystemClick(Sender: TObject);
  protected
    function ShowAllProc: boolean; override;
  end;

implementation

{$IFNDEF FPC}
{$IFDEF IDE}
{$R *.dfm}
{$ELSE}
{$R MSSPCallFrame.dfm}
{$ENDIF}
{$ELSE}
{$R *.lfm}
{$ENDIF}

uses
  MSAccess;

{ TMSSPCallFrame }

function TMSSPCallFrame.ShowAllProc: boolean;
begin
  Result := cbSystem.Checked;
end;

procedure TMSSPCallFrame.cbSystemClick(Sender: TObject);
begin
  FListGot := False;
end;

end.
