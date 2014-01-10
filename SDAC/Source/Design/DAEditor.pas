
//////////////////////////////////////////////////
//  DB Access Components
//  Copyright © 1998-2012 Devart. All right reserved.
//  Base Component Editor
//////////////////////////////////////////////////

{$IFNDEF CLR}

{$I Dac.inc}

unit DAEditor;
{$ENDIF}

interface
uses
{$IFDEF MSWINDOWS}
  Windows, Messages,
{$ENDIF}
  SysUtils, Classes, Graphics, Controls, Forms, DBGrids, Dialogs,
  StdCtrls, ExtCtrls, Buttons,
{$IFDEF DBTOOLS}
  DBToolsClient,
{$IFDEF CLR}
  System.Text,
{$ENDIF}
{$ENDIF}
{$IFDEF FPC}
  LResources,
{$ENDIF}
   CREditor, DADesignUtils;

type
  TDAEditorForm = class(TCREditorForm)
  protected
    function GetDADesignUtilsClass: TDADesignUtilsClass;
  public
    procedure CheckConnection(const Component: TComponent);
    property DADesignUtilsClass: TDADesignUtilsClass read GetDADesignUtilsClass;
  end;

implementation
uses
{$IFDEF MSWINDOWS}
  Registry,
{$ENDIF}
{$IFDEF USE_SYNEDIT}
  Menus,
{$ENDIF}
  DB, DAConsts, DBAccess;

{$IFNDEF FPC}
{$IFDEF IDE}
{$R *.dfm}
{$ELSE}
{$R DAEditor.dfm}
{$ENDIF}
{$ELSE}
{$R *.lfm}
{$ENDIF}

{ TDAEditorForm }

function TDAEditorForm.GetDADesignUtilsClass: TDADesignUtilsClass;
begin
  Result := TDADesignUtilsClass(FCRDesignUtilsClass);
end;

procedure TDAEditorForm.CheckConnection(const Component: TComponent);
var
  Connection: TCustomDAConnection;
begin
  if Component is TCustomDAConnection then
    Connection := TCustomDAConnection(Component)
  else begin
    Connection := DADesignUtilsClass.UsedConnection(Component) as TCustomDAConnection;
    if Connection = nil then
      DatabaseError(SConnectionNotDefined);
  end;
  if not Connection.Connected then begin
    Connection.Connect;
  {$IFDEF DBTOOLS}
    DBTools.CheckConnectionChanges;
  {$ENDIF}
  end;
end;

end.
