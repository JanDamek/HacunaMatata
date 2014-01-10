//////////////////////////////////////////////////
//  DB Access Components
//  Copyright © 1998-2012 Devart. All right reserved.
//  Base Component Editor
//////////////////////////////////////////////////

{$IFNDEF CLR}

{$I Dac.inc}

unit CRDataEditor;
{$ENDIF}

interface
uses
{$IFDEF MSWINDOWS}
  Windows, Messages,
{$ENDIF}
  SysUtils, Classes, Graphics, Controls, Forms, DBGrids, Dialogs,
  StdCtrls, ExtCtrls, Buttons,  ComCtrls, Grids, DBCtrls,
{$IFDEF FPC}
  LResources,
{$ENDIF}
  DB, CREditor, {$IFDEF FPC}MemDataSet{$ELSE}MemDS{$ENDIF};

type
  TCRDataEditorForm = class(TCREditorForm)
    pnlToolBar: TPanel;
    btClose: TSpeedButton;
    DBNavigator: TDBNavigator;
{$IFDEF CLR}
    DBGrid: TCustomDBGrid;
{$ELSE}
    DBGrid: TDBGrid;
{$ENDIF}
    StatusBar: TStatusBar;
    DataSource: TDataSource;
    procedure DataSourceStateChange(Sender: TObject);
    procedure DataSourceDataChange(Sender: TObject; Field: TField);
    procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);
  protected
    FDataSet: TMemDataSet;

    procedure DoInit; override;
    procedure DoSave; override;
    procedure DoFinish; override;

    function GetComponent: TComponent; override;
    procedure SetComponent(Value: TComponent); override;
  end;

implementation

{$IFNDEF FPC}
{$IFDEF IDE}
{$R *.dfm}
{$ELSE}
{$R CRDataEditor.dfm}
{$ENDIF}
{$ELSE}
{$R *.lfm}
{$ENDIF}

uses
{$IFDEF CLR}
  Variants,
{$ENDIF}
  TypInfo;

{ TDADataEditorForm }

procedure TCRDataEditorForm.DoInit;
var
  cg: TCustomDBGrid;
begin
  inherited;

  cg := DBGrid;
  if ReplaceGridToCRGrid(cg) then begin
  {$IFDEF CLR}
    DBGrid := cg;
  {$ELSE}
    DBGrid := TDBGrid(cg);
  {$ENDIF}
    SetPropValue(DBGrid, 'OptionsEx', Variant('dgeEnableSort,dgeLocalFilter,dgeLocalSorting]')); // disable dgeRecordCount
  end;
  DataSource.DataSet := Component as TMemDataSet;
end;

procedure TCRDataEditorForm.DoSave;
begin
end;

procedure TCRDataEditorForm.DoFinish;
begin
  inherited;
end;

function TCRDataEditorForm.GetComponent: TComponent;
begin
  Result := FDataSet;
end;

procedure TCRDataEditorForm.SetComponent(Value: TComponent);
begin
  FDataSet := Value as TMemDataSet;
end;

procedure TCRDataEditorForm.DataSourceStateChange(Sender: TObject);
begin
  StatusBar.Panels[0].Text := 'RecordCount: ' + IntToStr(FDataSet.RecordCount);
  StatusBar.Panels[1].Text := 'RecordNo: ' + IntToStr(FDataSet.RecNo);
end;

procedure TCRDataEditorForm.DataSourceDataChange(Sender: TObject;
  Field: TField);
begin
  DataSourceStateChange(nil);
end;

procedure TCRDataEditorForm.FormCloseQuery(Sender: TObject;
  var CanClose: Boolean);
begin
  DBGrid.SetFocus;
  inherited;
end;

{$IFDEF CLR}
initialization
  RegisterClass(TDBGrid);
{$ENDIF}

end.
