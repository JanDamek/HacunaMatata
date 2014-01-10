//////////////////////////////////////////////////
//  SQL Server Data Access Components
//  Copyright © 1998-2012 Devart. All right reserved.
//  MSUpdateSQL Editor
//////////////////////////////////////////////////

{$IFNDEF CLR}

{$I Sdac.inc}

unit MSUpdateSQLEditor;
{$ENDIF}
interface
uses
{$IFDEF FPC}
  LResources,
{$ENDIF}
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms,
  Dialogs, DAUpdateSQLEditor, ComCtrls, StdCtrls, Buttons, ExtCtrls;

type
  TMSUpdateSQLEditorForm = class(TDAUpdateSQLEditorForm)
  protected
    procedure DoInit; override;
  end;

implementation

{$IFNDEF FPC}
{$IFDEF IDE}
{$R *.dfm}
{$ELSE}
{$R MSUpdateSQLEditor.dfm}
{$ENDIF}
{$ELSE}
{$R *.lfm}
{$ENDIF}

uses
  DAUpdateSQLFrame, DASQLGeneratorFrame;

procedure TMSUpdateSQLEditorForm.DoInit;
begin
  FUpdateSQLFrame := AddTab(TDAUpdateSQLFrame, shEditSQL) as TDAUpdateSQLFrame;
  FSQLGeneratorFrame := AddTab(TDASQLGeneratorFrame, shGenerator) as TDASQLGeneratorFrame;
  inherited;
end;

end.
