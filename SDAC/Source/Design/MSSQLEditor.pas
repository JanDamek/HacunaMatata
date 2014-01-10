
//////////////////////////////////////////////////
//  SQL Server Data Access Components
//  Copyright © 1998-2012 Devart. All right reserved.
//  MSSQL Editor
//////////////////////////////////////////////////

{$IFNDEF CLR}

{$I Sdac.inc}

unit MSSQLEditor;
{$ENDIF}
interface

uses
{$IFDEF FPC}
  LResources,
{$ENDIF}
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms,
  Dialogs, DASQLComponentEditor, ComCtrls, StdCtrls, Buttons, ExtCtrls,
  OLEDBAccess;

type
  TMSSQLEditorForm = class(TDASQLEditorForm)
    btQueryAnalyzer: TButton;
    btManagementStudio: TButton;
    procedure btQueryAnalyzerClick(Sender: TObject);
    procedure btManagementStudioClick(Sender: TObject);
  protected
    procedure DoInit; override;
    procedure DoError(E: Exception); override;
  public
    property SQL;
  end;

procedure DoError(Sender: TDASQLEditorForm; E: EOLEDBError);

implementation

{$IFNDEF FPC}
{$IFDEF IDE}
{$R *.dfm}
{$ELSE}
{$R MSSQLEditor.dfm}
{$ENDIF}
{$ELSE}
{$R *.lfm}
{$ENDIF}

uses
  MSAccess, DASQLFrame, DAParamsFrame, MSParamsFrame, DAMacrosFrame, DASPCallFrame,
  MSSPCallFrame{$IFDEF SDAC}, MSDesign{$ENDIF};

procedure DoError(Sender: TDASQLEditorForm; E: EOLEDBError);
begin
  Sender.ActivateFrame(Sender.SQLFrame);
  Sender.ActiveControl := Sender.SQLFrame.meSQL;
end;

{ TMSSQLEditorForm }

procedure TMSSQLEditorForm.DoError(E: Exception);
begin
  if E is EOLEDBError then
    {$IFDEF CLR}Devart.SDac.Design.{$ENDIF}MSSQLEditor.DoError(Self, EOLEDBError(E))
  else
    inherited;
end;

procedure TMSSQLEditorForm.DoInit;
begin
  FSQLFrame := AddTab(TDASQLFrame, shSQL) as TDASQLFrame;
  FParamsFrame := AddTab(TMSParamsFrame, shParameters) as TDAParamsFrame;
  FMacrosFrame := AddTab(TDAMacrosFrame, shMacros) as TDAMacrosFrame;
  FSPCallFrame := AddTab(TMSSPCallFrame, shGeneratorSPC) as TDASPCallFrame;

  inherited;

{$IFDEF SDAC}
  btQueryAnalyzer.Visible := IsServerToolInstalled(stQueryAnalyser);
  btManagementStudio.Visible := IsServerToolInstalled(stManagementStudio);
  if btQueryAnalyzer.Visible and (SQL.Connection <> nil) then
    btQueryAnalyzer.Visible := (SQL.Connection is TMSConnection) and (TMSConnection(SQL.Connection).Options.Provider <> prCompact);
  if btManagementStudio.Visible and (SQL.Connection <> nil) then
    btManagementStudio.Visible := (SQL.Connection is TMSConnection) and (TMSConnection(SQL.Connection).Options.Provider <> prCompact);
{$ENDIF}
end;

procedure TMSSQLEditorForm.btQueryAnalyzerClick(Sender: TObject);
begin
{$IFDEF SDAC}
  RunServerToolMSSQL(stQueryAnalyser, LocalComponent as TMSSQL);
{$ENDIF}
end;

procedure TMSSQLEditorForm.btManagementStudioClick(Sender: TObject);
begin
{$IFDEF SDAC}
  RunServerToolMSSQL(stManagementStudio, LocalComponent as TMSSQL);
{$ENDIF}
end;

end.
