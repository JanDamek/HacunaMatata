
//////////////////////////////////////////////////
//  DB Access Components
//  Copyright © 1998-2012 Devart. All right reserved.
//  SQL Editor Frame
//////////////////////////////////////////////////

{$IFNDEF CLR}

{$I Dac.inc}

unit DASQLFrame;
{$ENDIF}

interface

uses
{$IFDEF MSWINDOWS}
  Windows, Messages,
{$ENDIF}
  Graphics, Controls, Forms, Dialogs, StdCtrls, ExtCtrls,
{$IFDEF DBTOOLS}
  DBToolsClient,
{$ENDIF}
{$IFDEF FPC}
  LResources,
{$ENDIF}
  Classes, SysUtils,
  CRTypes, CRFrame, CRTabEditor;

type
  TDASQLFrame = class(TCRFrame)
    meSQL: TMemo;
    Panel1: TPanel;
    procedure meSQLExit(Sender: TObject);

  protected
    function GetSQLText: _string;
    procedure SetSQLText(const Value: _string);

    procedure LoadMemo; virtual;
    procedure SaveMemo; virtual;
    procedure DoActivate; override;
    procedure DoFinish; override;
    function GetLocalComponentSQL: _TStrings; virtual;
    procedure SetLocalComponentSQL(Value: _TStrings); virtual;

    property SQLText: _string read GetSQLText write SetSQLText;
    property LocalComponentSQL: _TStrings read GetLocalComponentSQL write SetLocalComponentSQL;
  public
  {$IFDEF USE_SYNEDIT}
    constructor Create(Owner: TComponent); override;
  {$ENDIF}

    function ActiveControl: TWinControl; override;
  end;

implementation

{$IFNDEF FPC}
{$IFDEF IDE}
{$R *.dfm}
{$ELSE}
{$R DASQLFrame.dfm}
{$ENDIF}
{$ELSE}
{$R *.lfm}
{$ENDIF}

uses
  CREditor;

{$IFDEF USE_SYNEDIT}
constructor TDASQLFrame.Create(Owner: TComponent);
var
  WinControl: TWinControl;
begin
  inherited;
  
{$IFDEF DBTOOLS}
  if TCREditorForm(Owner).CRDesignUtilsClass.DBToolsAvailable then
    Exit;
{$ENDIF}
  WinControl := meSQL;
  Assert(Owner is TCREditorForm);
  TCREditorForm(Owner).ReplaceMemo(WinControl, True);
  meSQL := TMemo(WinControl);
end;
{$ENDIF}

function TDASQLFrame.GetSQLText: _string;
begin
  Result := GetMemoText(meSQL);
end;

procedure TDASQLFrame.SetSQLText(const Value: _string);
begin
  if Trim(SQLText) <> Trim(Value) then
    SetMemoText(meSQL, Value);
end;

function TDASQLFrame.GetLocalComponentSQL: _TStrings;
begin
  Assert(Editor <> nil);
  Result := Editor.DADesignUtilsClass.GetSQL(Editor.LocalComponent);
end;

procedure TDASQLFrame.SetLocalComponentSQL(Value: _TStrings);
begin
  Assert(Editor <> nil);
  Editor.DADesignUtilsClass.SetSQL(Editor.LocalComponent, Value);
end;

procedure TDASQLFrame.meSQLExit(Sender: TObject);
begin
  if LocalComponentSQL <> nil then
    if TrimRight(LocalComponentSQL.Text) <> TrimRight(SQLText) then begin
      SaveMemo;
      Modified := True;
    end;
end;

function TDASQLFrame.ActiveControl: TWinControl;
begin
{$IFDEF DBTOOLS}
  if DBTools.HasDACSqlEditorFrame(meSQL) then
    Result := DBTools.GetDACSqlEditorFrame(meSQL)
  else
{$ENDIF}
    Result := meSQL;
end;

procedure TDASQLFrame.LoadMemo;
begin
  SQLText := LocalComponentSQL.Text;
end;

procedure TDASQLFrame.SaveMemo;
begin
  LocalComponentSQL.Text := Trim(SQLText);
end;

procedure TDASQLFrame.DoActivate;
begin
{$IFDEF DBTOOLS}
  DBTools.ReplaceMemo(meSQL, Editor.DADesignUtilsClass, Editor.Component);
{$ENDIF}
  LoadMemo;
end;

procedure TDASQLFrame.DoFinish;
begin
{$IFDEF UNIX}
  meSQLExit(nil);
{$ENDIF}

  inherited;
end;

end.
