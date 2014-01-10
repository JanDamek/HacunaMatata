
//////////////////////////////////////////////////
//  SQL Server Data Access Components
//  Copyright © 1998-2012 Devart. All right reserved.
//  MSQuery Editor
//////////////////////////////////////////////////

{$IFNDEF CLR}

{$I Sdac.inc}

unit MSQueryEditor;
{$ENDIF}

interface

uses
{$IFDEF FPC}
  LResources,
{$ENDIF}
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms,
  Dialogs, DAQueryEditor, ComCtrls, StdCtrls, Buttons, ExtCtrls;

type

  TMSQueryEditorForm = class(TDAQueryEditorForm)
    btQueryAnalyzer: TBitBtn;
    btManagementStudio: TBitBtn;
    procedure btQueryAnalyzerClick(Sender: TObject);
    procedure btManagementStudioClick(Sender: TObject);
  protected
    procedure DoInit; override;
    procedure DoError(E: Exception); override;
  public
    property Query;
  end;

implementation

{$IFNDEF FPC}
{$IFDEF IDE}
{$R *.dfm}
{$ELSE}
{$R MSQueryEditor.dfm}
{$ENDIF}
{$ELSE}
{$R *.lfm}
{$ENDIF}

uses
  DASQLFrame, DAParamsFrame, MSParamsFrame, DAMacrosFrame, DASPCallFrame,
  MSSPCallFrame, DAUpdateSQLFrame, DASQLGeneratorFrame, MSSQLEditor,
  MSAccess, OLEDBAccess{$IFDEF SDAC}, MSDesign{$ENDIF};

{ TMSQueryEditorForm }

procedure TMSQueryEditorForm.DoInit;
begin
  FSQLFrame := AddTab(TDASQLFrame, shSQL) as TDASQLFrame;
  FParamsFrame := AddTab(TMSParamsFrame, shParameters) as TDAParamsFrame;
  FMacrosFrame := AddTab(TDAMacrosFrame, shMacros) as TDAMacrosFrame;
  FSPCallFrame := AddTab(TMSSPCallFrame, shGeneratorSPC) as TDASPCallFrame;
  FUpdateSQLFrame := AddTab(TDAUpdateSQLFrame, shEditSQL) as TDAUpdateSQLFrame;
  FSQLGeneratorFrame := AddTab(TDASQLGeneratorFrame, shGenerator) as TDASQLGeneratorFrame;

  inherited;
{$IFDEF SDAC}
  btQueryAnalyzer.Visible := IsServerToolInstalled(stQueryAnalyser);
{$ENDIF}
  if btQueryAnalyzer.Visible and (Query.Connection <> nil) then
    btQueryAnalyzer.Visible := (Query.Connection is TMSConnection) and (TMSConnection(Query.Connection).Options.Provider <> prCompact);

{$IFDEF SDAC}
  btManagementStudio.Visible := IsServerToolInstalled(stManagementStudio);
{$ENDIF}
  if btManagementStudio.Visible and (Query.Connection <> nil) then
    btManagementStudio.Visible := (Query.Connection is TMSConnection) and (TMSConnection(Query.Connection).Options.Provider <> prCompact);

  if btQueryAnalyzer.Visible and btManagementStudio.Visible then
    Constraints.MinWidth := 619;
end;

procedure TMSQueryEditorForm.DoError(E: Exception);
begin
  if E is EMSError then
    MSSQLEditor.DoError(Self, EMSError(E))
  else
  if E is EOLEDBError then
    MSSQLEditor.DoError(Self, EOLEDBError(E))
  else
    inherited;
end;

procedure TMSQueryEditorForm.btQueryAnalyzerClick(Sender: TObject);
begin
{$IFDEF SDAC}
  RunServerToolDataSet(stQueryAnalyser, LocalComponent as TCustomMSDataSet);
{$ENDIF}
end;

procedure TMSQueryEditorForm.btManagementStudioClick(Sender: TObject);
begin
{$IFDEF SDAC}
  RunServerToolDataSet(stManagementStudio, LocalComponent as TCustomMSDataSet);
{$ENDIF}
end;

end.