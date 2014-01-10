
//////////////////////////////////////////////////
//  DB Access Components
//  Copyright © 1998-2012 Devart. All right reserved.
//  Table SQL Frame
//////////////////////////////////////////////////

{$IFNDEF CLR}

{$I Dac.inc}

unit DATableSQLFrame;
{$ENDIF}

interface

uses
{$IFDEF MSWINDOWS}
  Windows, Messages,
{$ENDIF}
  Graphics, Controls, Forms, Dialogs, StdCtrls, ExtCtrls, Buttons,
{$IFDEF FPC}
  LResources,
{$ENDIF}
  Classes, SysUtils,
  CRTypes, CRFrame, CRTabEditor, DBAccess;

type
  TDATableSQLFrame = class(TCRFrame)
    Label2: TLabel;
    cbTableName: TComboBox;
    Label4: TLabel;
    edOrderFields: TEdit;
    Label1: TLabel;
    edFilter: TEdit;
    meSQL: TMemo;
    procedure cbTableNameDropDown(Sender: TObject);
    procedure cbTableNameExit(Sender: TObject);
    procedure edOrderFieldsExit(Sender: TObject);
    procedure edFilterExit(Sender: TObject);
  protected
    FListGot: boolean;
    FAllTables: boolean;

    procedure DoActivate; override;
    procedure DoFinish; override;

    procedure InitSQL;
    function GetLocalTable: TCustomDADataSet;

    procedure GetTableNames(Connection: TCustomDAConnection; Items: _TStrings); virtual;
  public
    constructor Create(Owner: TComponent); override;
  end;

  TDATableSQLFrameClass = class of TDATableSQLFrame;

implementation

uses
  CRFunctions;

{$IFNDEF FPC}
{$IFDEF IDE}
{$R *.dfm}
{$ELSE}
{$R DATableSQLFrame.dfm}
{$ENDIF}
{$ELSE}
{$R *.lfm}
{$ENDIF}

constructor TDATableSQLFrame.Create(Owner: TComponent);
begin
  inherited;

{$IFDEF UNIX}
  cbTableName.Items.Text := ' '; // bug in TComboBox
  meSQL.Height := meSQL.Parent.ClientHeight - meSQL.Top - cbTableName.Top;
{$ENDIF}
end;

procedure TDATableSQLFrame.DoActivate;
begin
  inherited;

  InitSQL;

  cbTableName.Text := Editor.DADesignUtilsClass.GetTableName(GetLocalTable);
  edFilter.Text := GetLocalTable.FilterSQL;
  edOrderFields.Text := Editor.DADesignUtilsClass.GetOrderFields(GetLocalTable);
end;

procedure TDATableSQLFrame.DoFinish;
begin
  inherited;
{$IFDEF UNIX}
  cbTableNameExit(nil);
  edOrderFieldsExit(nil);
  edFilterExit(nil);
{$ENDIF}
end;

procedure TDATableSQLFrame.InitSQL;
begin
  meSQL.Lines.Text := '';
  try
    if Editor.DADesignUtilsClass.GetTableName(GetLocalTable) <> '' then begin
      Editor.DADesignUtilsClass.PrepareSQL(GetLocalTable);
      meSQL.Lines.Text := GetLocalTable.FinalSQL;
    end;
  except
    on E: Exception do
      if not (E is EOutOfResources) then
        raise; // TMemo bug (see TMemoStrings.Insert)
  end;
end;

function TDATableSQLFrame.GetLocalTable: TCustomDADataSet;
begin
  Result := Editor.LocalComponent as TCustomDADataSet;
end;

procedure TDATableSQLFrame.GetTableNames(Connection: TCustomDAConnection;
  Items: _TStrings);
begin
  Connection.GetTableNames(Items, FAllTables);
end;

procedure TDATableSQLFrame.cbTableNameDropDown(Sender: TObject);
var
  UsedConnection: TCustomDAConnection;
  List: _TStringList;
begin
{$IFDEF UNIX}
  (Sender as TComboBox).OnGetItems := nil;
  try
{$ENDIF}
  UsedConnection := TDBAccessUtils.UsedConnection(GetLocalTable);
  if UsedConnection = nil then
    Exit;

  try
    if not FListGot then begin
      List := _TStringList.Create;
      try
        GetTableNames(UsedConnection, List);
        AssignStrings(List, cbTableName.Items);
      finally
        List.Free;
      end;
      FListGot := True;
    end;
  except
    Application.HandleException(Self);
  end;
{$IFDEF UNIX}
  finally
    (Sender as TComboBox).OnGetItems := cbTableNameDropDown;
  end;
{$ENDIF}
end;

procedure TDATableSQLFrame.cbTableNameExit(Sender: TObject);
begin
  if GetLocalTable = nil then Exit;

  try
    if Trim(cbTableName.Text) <> Editor.DADesignUtilsClass.GetTableName(GetLocalTable) then begin
      Editor.DADesignUtilsClass.SetTableName(GetLocalTable, cbTableName.Text);
      InitSQL;
      Modified:= True;
    end;
  except
    cbTableName.SetFocus;
    raise;
  end;
end;

procedure TDATableSQLFrame.edOrderFieldsExit(Sender: TObject);
begin
  if GetLocalTable = nil then Exit;

  if edOrderFields.Text <> Editor.DADesignUtilsClass.GetOrderFields(GetLocalTable) then begin
    Editor.DADesignUtilsClass.SetOrderFields(GetLocalTable, edOrderFields.Text);
    InitSQL;
    Modified:= True;
  end;
end;

procedure TDATableSQLFrame.edFilterExit(Sender: TObject);
begin
  if GetLocalTable = nil then Exit;

  if edFilter.Text <> GetLocalTable.FilterSQL then begin
    GetLocalTable.FilterSQL:= edFilter.Text;
    InitSQL;
    Modified:= True;
  end;
end;

end.
