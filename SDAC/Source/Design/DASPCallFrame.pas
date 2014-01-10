
//////////////////////////////////////////////////
//  DB Access Components
//  Copyright © 1998-2012 Devart. All right reserved.
//  Stored Proc Call Generator Frame
//////////////////////////////////////////////////

{$IFNDEF CLR}

{$I Dac.inc}

unit DASPCallFrame;
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
  Classes, SysUtils, CRTypes, DBAccess,
  CRFrame, CRTabEditor, DASQLFrame, DAUpdateSQLFrame;

type
  TDASPCallFrameMode = (spSQL, spQuery, spSQLSP, spQuerySP);

  TDASPCallFrame = class(TDAUpdateSQLFrame)
    pnSQL: TPanel;
    Label14: TLabel;
    cbStoredProcName: TComboBox;
    btGenerate: TButton;
    pnSQLSP: TPanel;
    Label2: TLabel;
    cbStoredProcNameSP: TComboBox;
    procedure btGenerateClick(Sender: TObject);
    procedure cbStoredProcNameChange(Sender: TObject);
    procedure cbStoredProcNameDropDown(Sender: TObject);
    procedure cbStoredProcNameSelect(Sender: TObject);
  protected
    FListGot: boolean;
    FMode: TDASPCallFrameMode;
    function GetStatementTypes: TStatementTypes; override;
    procedure SetMode(Value: TDASPCallFrameMode);
    function UsedConnection: TCustomDAConnection;
    procedure DoActivate; override;
    function GetSPIsQuery: boolean; virtual;
    function ShowAllProc: boolean; virtual;
    procedure CreateProcedureCall;
    procedure UpdateStoredProcSelection; virtual;
  public
    function GetSPName: _string; virtual;
    procedure SetSPName(const Value: _string); virtual;

    function ActiveControl: TWinControl; override;

    property Mode: TDASPCallFrameMode read FMode write SetMode;
  end;

implementation

{$IFNDEF FPC}
{$IFDEF IDE}
{$R *.dfm}
{$ELSE}
{$R DASPCallFrame.dfm}
{$ENDIF}
{$ELSE}
{$R *.lfm}
{$ENDIF}

uses
  CRFunctions, DASQLComponentEditor, DAQueryEditor, CREditor, DAConsts, DADesignUtils;

function TDASPCallFrame.ActiveControl: TWinControl;
begin
  case Mode of
    spSQL, spQuery, spQuerySP:
      Result := cbStoredProcName;
    spSQLSP:
      Result := cbStoredProcNameSP;
    else
      Result := inherited ActiveControl;
  end;
end;

procedure TDASPCallFrame.SetMode(Value: TDASPCallFrameMode);
begin
  if Value = spSQLSP then
    meSQL.Left := 1
  else
    meSQL.Left := 8;
  meSQL.Top := 8;
  meSQL.Width := meSQL.Parent.ClientWidth - meSQL.Left * 2;
  meSQL.Height := meSQL.Parent.Height - meSQL.Top * 2; // Delphi bug?

  pnSQL.Visible := False;
  pnlTop.Visible := False;
  pnSQLSP.Visible := False;

  pnSQL.Visible := Value in [spQuery, spQuerySP, spSQL];
  if pnSQL.Visible then
    pnlTop.Top := pnSQL.Height + 1
  else
    pnlTop.Top := 0;
  pnlTop.Visible := Value in [spQuery, spQuerySP];
  pnSQLSP.Visible := Value = spSQLSP;

  FMode := Value;
end;

function TDASPCallFrame.UsedConnection: TCustomDAConnection;
begin
  Result := nil;
  if Editor.Component is TCustomDADataSet then
    Result := TDBAccessUtils.UsedConnection(TCustomDADataSet(Editor.Component))
  else
  if Editor.Component is TCustomDASQL then
    Result := TDBAccessUtils.UsedConnection(TCustomDASQL(Editor.Component))
  else
    Assert(False);
end;

procedure TDASPCallFrame.DoActivate;
begin
  inherited;
{$IFDEF VER6P}
  cbStoredProcName.OnSelect := cbStoredProcNameSelect;
  cbStoredProcNameSP.OnSelect := cbStoredProcNameSelect;
{$ENDIF}
  cbStoredProcNameChange(nil);
end;

procedure TDASPCallFrame.btGenerateClick(Sender: TObject);
var
  Command: TCustomDASQL;
  OldSQL: _string;
begin
  OldSQL := TrimRight(LocalComponentSQL.Text);
  case Mode of
    spSQL: begin
      TDBAccessUtils.CreateProcCall(TCustomDASQL(Editor.LocalComponent), GetSPName, True, GetSPIsQuery);
    end;
    spQuery, spQuerySP: begin
      if Mode = spQuerySP then
        Editor.DADesignUtilsClass.SetStoredProcName(TCustomDADataSet(Editor.LocalComponent), GetSPname);
      if FStatementType = stQuery then
        TDBAccessUtils.CreateProcCall(TCustomDADataSet(Editor.LocalComponent), GetSPName, True, GetSPIsQuery)
      else begin
        Assert(UsedConnection <> nil);
        Command := UsedConnection.CreateSQL;
        try
          TDBAccessUtils.CreateProcCall(Command, GetSPName, True, GetSPIsQuery);
          SetLocalComponentSQL(Command.SQL);
        finally
          Command.Free;
        end;
      end;
    end;
  end;
  LoadMemo;
  meSQLChange(nil);
  if TrimRight(LocalComponentSQL.Text) <> OldSQL then
    Modified := True;
end;

procedure TDASPCallFrame.cbStoredProcNameChange(Sender: TObject);
begin
  case Mode of
    spSQL, spQuery, spQuerySP:
      btGenerate.Enabled := (UsedConnection <> nil) and (Trim(cbStoredProcName.Text) <> '');
    {spSQLSP:
      btGenerateSP.Enabled := Trim(cbStoredProcNameSP.Text) <> '';}
  end;
end;

function TDASPCallFrame.GetSPName: _string;
begin
  case Mode of
    spSQL, spQuery, spQuerySP:
      Result := cbStoredProcName.Text;
    spSQLSP:
      Result := cbStoredProcNameSP.Text;
  end;
  Result := Trim(Result);
end;

procedure TDASPCallFrame.SetSPName(const Value: _string);
begin
  case Mode of
    spSQL, spQuery, spQuerySP:
      cbStoredProcName.Text := Value;
    spSQLSP:
      cbStoredProcNameSP.Text := Value;
  end;
end;

function TDASPCallFrame.GetSPIsQuery: boolean;
begin
  Result := False;
end;

function TDASPCallFrame.ShowAllProc: boolean;
begin
  Result := False;
end;

procedure TDASPCallFrame.cbStoredProcNameDropDown(Sender: TObject);
var
  List: _TStringList;
begin
{$IFDEF UNIX}
  (Sender as TComboBox).OnGetItems := nil;
  try
{$ENDIF}
  try
    if not FListGot and (UsedConnection <> nil) then begin
      Editor.CheckConnection(UsedConnection);
      List := _TStringList.Create;
      try
        UsedConnection.GetStoredProcNames(List, ShowAllProc);
        AssignStrings(List, cbStoredProcName.Items);
      finally
        List.Free;
      end;
      cbStoredProcNameSP.Items.Assign(cbStoredProcName.Items);
      FListGot := True;
    end;
  except
    Application.HandleException(Self);
  end;
{$IFDEF UNIX}
  finally
    (Sender as TComboBox).OnGetItems := cbStoredProcNameDropDown;
  end;
{$ENDIF}
end;

procedure TDASPCallFrame.CreateProcedureCall;
var
  SPName: _string;
begin
  try
    SPName := GetSPName;
    Modified := True;
    if Mode in [spSQLSP, spQuerySP] then
      Editor.DADesignUtilsClass.SetStoredProcName(Editor.LocalComponent as TCustomDADataSet, SPName);
    if UsedConnection = nil then
      raise Exception.Create(SConnectionNotDefined);
    TDBAccessUtils.CreateProcCall(TCustomDADataSet(Editor.LocalComponent), SPName, True, GetSPIsQuery);
    LoadMemo;
    Application.ProcessMessages;
    SetSelStart(meSQL, 0);
  except
    if Editor.ActiveControl <> nil then // prevent from infinite loop if ESC was pressed in Editor
      Application.HandleException(Self);
  end;
end;

procedure TDASPCallFrame.UpdateStoredProcSelection;
var
  SPName: _string;
begin
  if Mode in [spSQLSP, spQuerySP] then begin
    SPName := GetSPName;
    if (SPName <> '') and (SPName <> Editor.DADesignUtilsClass.GetStoredProcName(Editor.LocalComponent as TCustomDADataSet)) then
      CreateProcedureCall;
  end;
end;

procedure TDASPCallFrame.cbStoredProcNameSelect(Sender: TObject);
begin
  UpdateStoredProcSelection;
  cbStoredProcNameChange(Sender);
end;

function TDASPCallFrame.GetStatementTypes: TStatementTypes;
begin
  case FMode of
    spSQL:
      Result := [stQuery]; // invisible
    spQuery:
      Result := Editor.DADesignUtilsClass.GetStatementTypes;
    spSQLSP:
      Result := [stQuery]; // invisible
    spQuerySP:
      Result := Editor.DADesignUtilsClass.GetStatementTypes;
    else
      Assert(False);
  end;
end;

end.
