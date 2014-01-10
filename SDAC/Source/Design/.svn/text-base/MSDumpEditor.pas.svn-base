//////////////////////////////////////////////////
//  SQL Server Data Access Components
//  Copyright © 1998-2012 Devart. All right reserved.
//  MSDump Editor
//////////////////////////////////////////////////

{$IFNDEF CLR}

{$I Sdac.inc}

unit MSDumpEditor;
{$ENDIF}

interface

uses
  Dialogs, Controls, StdCtrls, Buttons, Graphics, ExtCtrls,
  Classes, Windows, Messages, SysUtils, Forms, DacVcl,
  ComCtrls, Grids, DBGrids, DBCtrls,
{$IFDEF FPC}
  LResources,
{$ENDIF}
  DBAccess, MSAccess, CREditor, DB, MSDump, DADesignUtils, DADumpEditor;

type
  TMSDumpEditorForm = class(TDADumpEditorForm)
    cbAddDrop: TCheckBox;
    cbIdentityInsert: TCheckBox;
    procedure cbDataClick(Sender: TObject);
  private
    function GetLocalDump: TMSDump;
    procedure SetLocalDump(const Value: TMSDump);
  protected
    procedure EditTableNames(Sender: TObject); override;
    procedure DoInit; override;

    procedure GetButtons; override;
    procedure SetButtons; override;

    property LocalDump: TMSDump read GetLocalDump write SetLocalDump;
  end;

implementation

uses
  DAConsts, MSNamesEditor;
  
{$IFNDEF FPC}
{$IFDEF IDE}
{$R *.dfm}
{$ELSE}
{$R MSDumpEditor.dfm}
{$ENDIF}
{$ELSE}
{$R *.lfm}
{$ENDIF}

{ TMSDumpEditorForm }

function TMSDumpEditorForm.GetLocalDump: TMSDump;
begin
  Result := TMSDump(FLocalDump);
end;

procedure TMSDumpEditorForm.SetLocalDump(const Value: TMSDump);
begin
  FLocalDump := Value;
end;

procedure TMSDumpEditorForm.EditTableNames(Sender: TObject);
begin
  if FLocalDump.Connection = nil then
    DatabaseError(SConnectionNotDefined);

  with TMSNamesEditorForm.Create(nil, DADesignUtilsClass) do
    try
      Connection := FLocalDump.Connection as TMSConnection;
      Names := LocalDump.TableNames;
      ShowModal;
      if ModalResult = mrOk then
        LocalDump.TableNames := Names;
    finally
      Free;
    end;
  cbTableNames.Text := LocalDump.TableNames;
  cbTableNames.Update;
end;

procedure TMSDumpEditorForm.DoInit;
begin
  inherited;

  // cbData.Checked := doData in LocalDump.Objects;

  cbTableNames.Text := LocalDump.TableNames;
  cbAddDrop.Checked := LocalDump.Options.AddDrop;
  cbIdentityInsert.Checked := LocalDump.Options.IdentityInsert;
end;

procedure TMSDumpEditorForm.GetButtons;
begin
  inherited;
  
  {if cbData.Checked then
    LocalDump.Objects := LocalDump.Objects + [doData];}

  LocalDump.TableNames := cbTableNames.Text;
  LocalDump.Options.AddDrop := cbAddDrop.Checked;
  LocalDump.Options.IdentityInsert := cbIdentityInsert.Checked;
end;

procedure TMSDumpEditorForm.SetButtons;
begin
  inherited;

  // cbIdentityInsert.Enabled := cbData.Checked;

  // btBackup.Enabled := cbData.Checked;
end;

procedure TMSDumpEditorForm.cbDataClick(Sender: TObject);
begin
  SetButtons;
end;

end.
