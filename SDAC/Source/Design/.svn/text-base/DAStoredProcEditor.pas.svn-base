
//////////////////////////////////////////////////
//  Data Access Components
//  Copyright © 1998-2012 Devart. All right reserved.
//  StoredProc Editor
//////////////////////////////////////////////////

{$IFNDEF CLR}

{$I Dac.inc}

unit DAStoredProcEditor;
{$ENDIF}
interface
uses
{$IFDEF MSWINDOWS}
  Windows, Messages,
{$ENDIF}
  Graphics, Controls, Forms, Dialogs, ComCtrls, StdCtrls, ExtCtrls, Buttons,
{$IFDEF FPC}
  LResources,
{$ENDIF}
  SysUtils, DB, Classes,
  DBAccess, MemUtils,
  CREditor, CRTabEditor, DASQLFrame, DAParamsFrame, DAMacrosFrame, DASPCallFrame,
  DASQLComponentEditor, DAUpdateSQLFrame, DASQLGeneratorFrame, DAQueryEditor;

type
  TDAStoredProcEditorForm = class(TDAQueryEditorForm)
  protected
    procedure DoInit; override;

    function GetStoredProc: TCustomDADataSet;
    procedure SetStoredProc(Value: TCustomDADataSet);

    property StoredProc: TCustomDADataSet read GetStoredProc write SetStoredProc;

  end;

implementation

uses 
  DADesignUtils;

{$IFNDEF FPC}
{$IFDEF IDE}
{$R *.dfm}
{$ELSE}
{$R DAStoredProcEditor.dfm}
{$ENDIF}
{$ELSE}
{$R *.lfm}
{$ENDIF}

procedure TDAStoredProcEditorForm.DoInit;
begin
  try
    inherited;
  finally
    Assert(FSQLFrame is TDASPCallFrame);
    TDASPCallFrame(FSQLFrame).Mode := spSQLSP;
    FSPCallFrame.Mode := spQuerySP;

    TDASPCallFrame(FSQLFrame).SetSPName(DADesignUtilsClass.GetStoredProcName(LocalComponent as TCustomDADataSet));
  end;
end;

function TDAStoredProcEditorForm.GetStoredProc: TCustomDADataSet;
begin
  Result := FComponent as TCustomDADataSet;
end;

procedure TDAStoredProcEditorForm.SetStoredProc(Value: TCustomDADataSet);
begin
  FComponent := Value;
end;

end.
