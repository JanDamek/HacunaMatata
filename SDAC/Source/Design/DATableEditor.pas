
//////////////////////////////////////////////////
//  Data Access Components
//  Copyright © 1998-2012 Devart. All right reserved.
//  Table Editor
//////////////////////////////////////////////////

{$IFNDEF CLR}

{$I Dac.inc}

unit DATableEditor;
{$ENDIF}
interface
uses
{$IFDEF MSWINDOWS}
  Windows, Messages,
{$ENDIF}
  Graphics, Controls, Forms, Dialogs,
  ComCtrls, StdCtrls, ExtCtrls, Buttons,
{$IFDEF FPC}
  LResources,
{$ENDIF}
  SysUtils, DB, Classes,
  DBAccess, MemUtils, CREditor, DAEditor, CRTabEditor, DATableSQLFrame;

type
  TDATableEditorForm = class(TCRTabEditorForm)
    btnDataEditor: TBitBtn;
    shSQL: TTabSheet;
    procedure btnDataEditorClick(Sender: TObject);

  protected
    FLocalTable, FTable: TCustomDADataSet;
    FSQLFrame: TDATableSQLFrame;

    procedure DoInit; override;
    procedure DoFinish; override;
    procedure DoSave; override;

    function GetComponent: TComponent; override;
    procedure SetComponent(Value: TComponent); override;
    function GetLocalComponent: TComponent; override;

    function GetSQLFrameClass: TDATableSQLFrameClass; virtual;

  public
    property Table: TCustomDADataSet read FTable write FTable;

  end;

implementation

uses
  DADesignUtils, DADataEditor;

{$IFNDEF FPC}
{$IFDEF IDE}
{$R *.dfm}
{$ELSE}
{$R DATableEditor.dfm}
{$ENDIF}
{$ELSE}
{$R *.lfm}
{$ENDIF}

function TDATableEditorForm.GetSQLFrameClass: TDATableSQLFrameClass;
begin
  Result := TDATableSQLFrame;
end;

procedure TDATableEditorForm.DoInit;
begin
  inherited;

  FLocalTable := TComponentClass(Table.ClassType).Create(nil) as TCustomDADataSet;
  FLocalTable.Assign(Table);
  TDBAccessUtils.SetDesignCreate(FLocalTable, True);

  FSQLFrame := AddTab(GetSQLFrameClass, shSQL) as TDATableSQLFrame;
end;

procedure TDATableEditorForm.DoFinish;
begin
  FLocalTable.Free;
  FLocalTable := nil;
  inherited;
end;

procedure TDATableEditorForm.DoSave;
var
  OldActive: boolean;
  OldDebug: boolean;
begin
  OldActive := Table.Active;
  OldDebug := Table.Debug;

  try
    inherited;
    Table.Assign(FLocalTable);
    Table.Debug := False;
    try
      Table.Active := OldActive;
    except
    end;
  finally
    Table.Debug := OldDebug;
  end;
end;

function TDATableEditorForm.GetComponent: TComponent;
begin
  Result := Table;
end;

procedure TDATableEditorForm.SetComponent(Value: TComponent);
begin
  Table := Value as TCustomDADataSet;
end;

function TDATableEditorForm.GetLocalComponent: TComponent;
begin
  Result := FLocalTable;
end;

procedure TDATableEditorForm.btnDataEditorClick(Sender: TObject);
begin
  SaveControlData;
  with TDADataEditorForm.Create(nil, FCRDesignUtilsClass) do
    try
      Component := Self.FLocalTable;
      ShowModal;
    finally
      Free;
    end;
end;

end.
