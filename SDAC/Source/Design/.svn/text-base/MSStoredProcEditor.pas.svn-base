//////////////////////////////////////////////////
//  SQL Server Data Access Components
//  Copyright © 1998-2012 Devart. All right reserved.
//  MSStoredProc Editor
//////////////////////////////////////////////////

{$IFNDEF CLR}

{$I Sdac.inc}

unit MSStoredProcEditor;
{$ENDIF}
interface
uses
{$IFDEF FPC}
  LResources,
{$ENDIF}
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms,
  Dialogs, DAStoredProcEditor, ComCtrls, StdCtrls, Buttons, ExtCtrls;

type
  TMSStoredProcEditorForm = class(TDAStoredProcEditorForm)
    btnQuertAnalyzer: TBitBtn;
    btManagementStudio: TBitBtn;
    procedure btnQuertAnalyzerClick(Sender: TObject);
    procedure btManagementStudioClick(Sender: TObject);
  protected
    procedure DoInit; override;
  public
    property StoredProc;
  end;

implementation

{$IFNDEF FPC}
{$IFDEF IDE}
{$R *.dfm}
{$ELSE}
{$R MSStoredProcEditor.dfm}
{$ENDIF}
{$ELSE}
{$R *.lfm}
{$ENDIF}

uses
  MSAccess, DASQLFrame, DASPCallFrame, MSSPCallFrame, DAParamsFrame, MSParamsFrame,
  DAMacrosFrame, DAUpdateSQLFrame, DASQLGeneratorFrame{$IFDEF SDAC}, MSDesign, OLEDBAccess{$ENDIF};

{ TMSStoredProcEditorForm }

procedure TMSStoredProcEditorForm.DoInit;
begin
  FSQLFrame := AddTab(TMSSPCallFrame, shSQL) as TDASQLFrame;
  FParamsFrame := AddTab(TMSParamsFrame, shParameters) as TDAParamsFrame;
  FMacrosFrame := AddTab(TDAMacrosFrame, shMacros) as TDAMacrosFrame;
  FSPCallFrame := AddTab(TMSSPCallFrame, shGeneratorSPC) as TDASPCallFrame;
  FUpdateSQLFrame := AddTab(TDAUpdateSQLFrame, shEditSQL) as TDAUpdateSQLFrame;
  FSQLGeneratorFrame := AddTab(TDASQLGeneratorFrame, shGenerator) as TDASQLGeneratorFrame;
  
  inherited;
{$IFDEF SDAC}
  btnQuertAnalyzer.Visible := IsServerToolInstalled(stQueryAnalyser);
  if btnQuertAnalyzer.Visible and (StoredProc.Connection <> nil) then
    btnQuertAnalyzer.Visible := (StoredProc.Connection is TMSConnection) and (TMSConnection(StoredProc.Connection).Options.Provider <> prCompact);

  btManagementStudio.Visible := IsServerToolInstalled(stManagementStudio);
  if btManagementStudio.Visible and (StoredProc.Connection <> nil) then
    btManagementStudio.Visible := (StoredProc.Connection is TMSConnection) and (TMSConnection(StoredProc.Connection).Options.Provider <> prCompact);
{$ENDIF}
end;

procedure TMSStoredProcEditorForm.btnQuertAnalyzerClick(Sender: TObject);
begin
{$IFDEF SDAC}
  RunServerToolDataSet(stQueryAnalyser, LocalComponent as TCustomMSDataSet);
{$ENDIF}
end;

procedure TMSStoredProcEditorForm.btManagementStudioClick(Sender: TObject);
begin
{$IFDEF SDAC}
  RunServerToolDataSet(stManagementStudio, LocalComponent as TCustomMSDataSet);
{$ENDIF}
end;

end.