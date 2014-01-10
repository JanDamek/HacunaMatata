//////////////////////////////////////////////////
//  DB Access Components
//  Copyright © 1998-2012 Devart. All right reserved.
//  Base Component Editor
//////////////////////////////////////////////////

{$IFNDEF CLR}

{$I Dac.inc}

unit VTDataEditor;
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
  DB, CRDataEditor, CRDesignUtils, VirtualTable;

type
  TVTDataEditorForm = class(TCRDataEditorForm)
    btSave: TSpeedButton;
    btClear: TSpeedButton;
    btLoadFromFile: TSpeedButton;
    btSaveToFile: TSpeedButton;
    OpenDialog: TOpenDialog;
    SaveDialog: TSaveDialog;
    procedure btLoadFromFileClick(Sender: TObject);
    procedure btSaveToFileClick(Sender: TObject);
    procedure btClearClick(Sender: TObject);
    procedure DataSourceDataChange(Sender: TObject; Field: TField);
  private
    LocalDataSet: TVirtualTable;

    procedure AfterPost(DataSet: TDataSet);
    procedure AfterDelete(DataSet: TDataSet);

    function LoadDefaultExt: integer;
    procedure SaveDefaultExt(Value: integer);
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
{$R VTDataEditor.dfm}
{$ENDIF}
{$ELSE}
{$R *.lfm}
{$ENDIF}

uses
{$IFDEF CLR}
  Variants,
{$ENDIF}
{$IFDEF MSWINDOWS}
  Registry,
{$ENDIF}
  TypInfo;
  
{ TDADataEditorForm }

procedure TVTDataEditorForm.DoInit;
var
  Filtered: boolean;
begin
  inherited;

  DataSource.DataSet := nil;

  LocalDataSet:= TVirtualTable.Create(Self);
  Filtered := FDataSet.Filtered;
  try
    FDataSet.Filtered := False;
    LocalDataSet.Assign(FDataSet);
  finally
    FDataSet.Filtered := Filtered;
  end;

  LocalDataSet.Filter := FDataSet.Filter;
  LocalDataSet.Filtered := Filtered;
  Caption := FDataSet.Owner.Name + '.' + FDataSet.Name;

  DataSource.DataSet:= LocalDataSet;
  LocalDataSet.Open;
  LocalDataSet.AfterPost := AfterPost;
  LocalDataSet.AfterDelete:= AfterDelete;

  Modified:= False;
end;

procedure TVTDataEditorForm.DoSave;
var
  OldFiltered: boolean;
  Stream: TMemoryStream;
begin
  OldFiltered := LocalDataSet.Filtered;
  try
    LocalDataSet.Filtered := False;
    if (FDataSet is TVirtualTable)and not LocalDataSet.DefaultFields then begin
      Stream := TMemoryStream.Create;
      try
        LocalDataSet.SaveToStream(Stream, False);
        TVirtualTable(FDataSet).LoadFromStream(Stream, False);
      finally
        Stream.Free;
      end;
    end
    else
      FDataSet.Assign(LocalDataSet);
    LocalDataSet.Close;
  finally
    LocalDataSet.Filtered := OldFiltered;
  end;
end;

procedure TVTDataEditorForm.DoFinish;
begin
  LocalDataSet.Free;
  inherited;
end;

procedure TVTDataEditorForm.AfterDelete(DataSet: TDataSet);
begin
  Modified := True;
end;

procedure TVTDataEditorForm.AfterPost(DataSet: TDataSet);
begin
  Modified := True;
end;

function TVTDataEditorForm.LoadDefaultExt: integer;
{$IFDEF MSWINDOWS}
var
  Registry: TRegistry;
{$ENDIF}
begin
  Result := 1;
{$IFDEF MSWINDOWS}
  Registry := TRegistry.Create(KEY_READ OR KEY_WRITE);
  try
    with Registry do begin
      OpenKey(KeyPath + '\' + FolderName, True);
      if ValueExists('DefaultExt') then
        Result := ReadInteger('DefaultExt');
    end;
  finally
    Registry.Free;
  end;
{$ENDIF}
end;

procedure TVTDataEditorForm.SaveDefaultExt(Value: integer);
{$IFDEF MSWINDOWS}
var
  Registry: TRegistry;
{$ENDIF}
begin
{$IFDEF MSWINDOWS}
  Registry := TRegistry.Create(KEY_READ OR KEY_WRITE);
  try
    with Registry do begin
      OpenKey(KeyPath + '\' + FolderName, True);
      WriteInteger('DefaultExt', Value);
    end;
  finally
    Registry.Free;
  end;
{$ENDIF}
end;

procedure TVTDataEditorForm.btLoadFromFileClick(Sender: TObject);
begin
  OpenDialog.FilterIndex := LoadDefaultExt;
  if OpenDialog.Execute then
    LocalDataSet.LoadFromFile(OpenDialog.FileName, LocalDataSet.DefaultFields);
end;

procedure TVTDataEditorForm.btSaveToFileClick(Sender: TObject);
var
  FileName: string;
begin
  SaveDialog.FilterIndex := LoadDefaultExt;
  if SaveDialog.Execute then begin
    FileName := SaveDialog.FileName;
    SaveDefaultExt(SaveDialog.FilterIndex);
    if ExtractFileExt(FileName) = '' then
      case SaveDialog.FilterIndex of
        1: FileName := FileName + '.vtd';
        2: FileName := FileName + '.xml';
      end;
    case SaveDialog.FilterIndex of
      1: LocalDataSet.SaveToFile(FileName);
      2: LocalDataSet.SaveToXML(FileName);
    end;
  end;
end;

procedure TVTDataEditorForm.btClearClick(Sender: TObject);
begin
  LocalDataSet.Clear;
  Modified:= True;
end;

procedure TVTDataEditorForm.DataSourceDataChange(Sender: TObject;
  Field: TField);
begin
  if LocalDataSet <> nil then begin // event first occurs before assigning value to LocalDataSet
    StatusBar.Panels[0].Text := 'RecordCount: ' + IntToStr(LocalDataSet.RecordCount);
    StatusBar.Panels[1].Text := 'RecordNo: ' + IntToStr(LocalDataSet.RecNo);
  end;
end;

{$IFDEF CLR}
initialization
  RegisterClass(TDBGrid);
{$ENDIF}

end.
