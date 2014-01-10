//////////////////////////////////////////////////
//  SQL Server Data Access Components
//  Copyright © 1998-2012 Devart. All right reserved.
//  MSNames Editor
//////////////////////////////////////////////////

{$IFNDEF CLR}

{$I Sdac.inc}

unit MSNamesEditor;
{$ENDIF}

interface

uses
{$IFDEF FPC}
  LResources,
{$ENDIF}
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  ComCtrls, Grids, DBGrids, DBCtrls, Buttons, ExtCtrls, StdCtrls,
  CREditor, DADualListEditor, MSAccess, OLEDBAccess, CRTypes, CRFunctions;

type
  TMSNamesEditorForm = class(TDADualListEditorForm)
  protected
    FConnection: TMSConnection;
    FNames: _string;

    procedure DoInit; override;
    procedure DoSave; override;

    function GetComponent: TComponent; override;
    procedure SetComponent(Value: TComponent); override;

    function GetSrcLabelCaption: string; override;
    procedure GetSrcListItems(Items: _TStrings); override;
    function GetDestLabelCaption: string; override;
    procedure GetDstListItems(Items: _TStrings); override;
  public
    property Connection: TMSConnection read FConnection write FConnection;
    property Names: _string read FNames write FNames;
  end;

implementation

{$IFNDEF FPC}
{$IFDEF IDE}
{$R *.dfm}
{$ELSE}
{$R MSNamesEditor.dfm}
{$ENDIF}
{$ELSE}
{$R *.lfm}
{$ENDIF}

{ TMSNamesEditorForm }

procedure TMSNamesEditorForm.DoInit;
begin
  if Connection = nil then
    Abort;
    
  inherited;
end;

procedure TMSNamesEditorForm.DoSave;
var
  List: _TStringList;
begin
  List := _TStringList.Create;
  try
    AssignStrings(DstList.Items, List);
    Names := OLEDBSQLInfo.NamesFromList(List);
  finally
    List.Free;
  end;
end;

function TMSNamesEditorForm.GetComponent: TComponent;
begin
  Result := FConnection;
end;

procedure TMSNamesEditorForm.SetComponent(Value: TComponent);
begin
  FConnection := Value as TMSConnection;
end;

function TMSNamesEditorForm.GetDestLabelCaption: string;
begin
  Result := 'Selected tables';
end;

procedure TMSNamesEditorForm.GetDstListItems(Items: _TStrings);
begin
  OLEDBSQLInfo.NamesToList(Names, Items);
end;

function TMSNamesEditorForm.GetSrcLabelCaption: string;
begin
  Result := 'Available tables';
end;

procedure TMSNamesEditorForm.GetSrcListItems(Items: _TStrings);
begin
  Connection.GetTableNames(Items);
end;

end.
