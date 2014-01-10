//////////////////////////////////////////////////
//  DB Access Components
//  Copyright © 1998-2012 Devart. All right reserved.
//  Base Component Editor
//////////////////////////////////////////////////

{$IFNDEF CLR}

{$I Dac.inc}

unit DADataEditor;
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
  DB, CRDataEditor, DBAccess, DADesignUtils;

type
  TDADataEditorForm = class(TCRDataEditorForm)
    btOpen: TSpeedButton;
    btnExit: TSpeedButton;
    btSaveToFile: TSpeedButton;
    SaveDialog: TSaveDialog;
    procedure btOpenClick(Sender: TObject);
    procedure btCloseClick(Sender: TObject);
    procedure btSaveToFileClick(Sender: TObject);
  private
    // fields to store old DataSet properties
    FOldAutoCommit: boolean;
    FOldFilterOptions: TFilterOptions;
    FOldFilterSQL: string;
    FOldSQL: string;
    FOldActive: boolean;
    FOldFilter: string;
    FOldFiltered: boolean;
  protected
    procedure DoInit; override;
    procedure DoSave; override;
    procedure DoFinish; override;
  end;

implementation

{$IFNDEF FPC}
{$IFDEF IDE}
{$R *.dfm}
{$ELSE}
{$R DADataEditor.dfm}
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

procedure TDADataEditorForm.DoInit;
begin
  inherited;
  
  FOldAutoCommit := TDBAccessUtils.GetAutoCommit(TCustomDADataSet(FDataSet));
  FOldFilterOptions := TCustomDADataSet(FDataSet).FilterOptions;
  FOldFilterSQL := TCustomDADataSet(FDataSet).FilterSQL;
  FOldSQL := TCustomDADataSet(FDataSet).SQL.Text;
  FOldActive := FDataSet.Active;
  TDBAccessUtils.SetAutoCommit(TCustomDADataSet(FDataSet), True);
  FOldFilter := FDataSet.Filter;
  FOldFiltered := FDataSet.Filtered;
  FOldActive := FDataSet.Active;
  try
    FDataSet.Open;
  except
    Application.HandleException(Self);
  {$IFDEF MSWINDOWS}
    if not FDataSet.Active then
      PostMessage(Handle, WM_CLOSE, 0, 0);  
  {$ENDIF}
  end;
  btOpen.Enabled := not FDataSet.Active;
  btClose.Enabled := FDataSet.Active;
end;

procedure TDADataEditorForm.DoSave;
begin
end;

procedure TDADataEditorForm.DoFinish;
begin
  TDBAccessUtils.SetAutoCommit(TCustomDADataSet(FDataSet), FOldAutoCommit);
  TCustomDADataSet(FDataSet).FilterOptions := FOldFilterOptions;
  TCustomDADataSet(FDataSet).FilterSQL := FOldFilterSQL;
  if TCustomDADataSet(FDataSet).SQL.Text <> FOldSQL then
    TCustomDADataSet(FDataSet).SQL.Text := FOldSQL;
  FDataSet.Filter := FOldFilter;
  FDataSet.Filtered := FOldFiltered;
  FDataSet.Active := FOldActive;

  inherited;
end;

procedure TDADataEditorForm.btOpenClick(Sender: TObject);
begin
  FDataSet.Open;
  btOpen.Enabled := not FDataSet.Active;
  btClose.Enabled := FDataSet.Active;
end;

procedure TDADataEditorForm.btCloseClick(Sender: TObject);
begin
  FDataSet.Close;
  btOpen.Enabled := not FDataSet.Active;
  btClose.Enabled := FDataSet.Active;
end;

procedure TDADataEditorForm.btSaveToFileClick(Sender: TObject);
begin
  if SaveDialog.Execute then
    FDataSet.SaveToXML(SaveDialog.FileName);
end;

{$IFDEF CLR}
initialization
  RegisterClass(TDBGrid);
{$ENDIF}

end.
