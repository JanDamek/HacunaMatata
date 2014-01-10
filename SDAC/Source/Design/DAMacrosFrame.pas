
//////////////////////////////////////////////////
//  DB Access Components
//  Copyright © 1998-2012 Devart. All right reserved.
//  Macros Frame
//////////////////////////////////////////////////

{$IFNDEF CLR}

{$I Dac.inc}

unit DAMacrosFrame;
{$ENDIF}

interface

uses
{$IFDEF MSWINDOWS}
  Windows, Messages,
{$ENDIF}
  Graphics, Controls, Forms, Dialogs, StdCtrls, ExtCtrls,
{$IFDEF FPC}
  LResources,
{$ENDIF}
  Classes, SysUtils,
  DBAccess, CRFrame, CRColFrame, CRTabEditor;

type
  TDAMacrosFrame = class(TCRColFrame)
    lbMName: TLabel;
    lbMacroLog: TLabel;
    lbMValue: TLabel;
    meMacroValue: TMemo;
    cbMacroActive: TCheckBox;
    lbActive: TLabel;
    procedure cbMacroActiveClick(Sender: TObject);
    procedure meMacroValueExit(Sender: TObject);

  protected
    function GetItems: TCollection; override;
    function GetMacros: TMacros;

    function GetItemName(Item: TCollectionItem): string; override;
    procedure ItemToControls(Item: TCollectionItem); override;
    procedure ControlsToItem(Item: TCollectionItem); override;

    property Macros: TMacros read GetMacros;
  public
  end;

implementation

{$IFNDEF FPC}
{$IFDEF IDE}
{$R *.dfm}
{$ELSE}
{$R DAMacrosFrame.dfm}
{$ENDIF}
{$ELSE}
{$R *.lfm}
{$ENDIF}

uses
  DB, DASQLComponentEditor;

function TDAMacrosFrame.GetItems: TCollection;
begin
  Result := Editor.DADesignUtilsClass.GetMacros(Editor.localComponent);
end;

function TDAMacrosFrame.GetMacros: TMacros;
begin
  Result := Items as TMacros;
end;

function TDAMacrosFrame.GetItemName(Item: TCollectionItem): string;
begin
  Result := TMacro(Item).Name;
end;

procedure TDAMacrosFrame.ItemToControls(Item: TCollectionItem);
begin
  meMacroValue.Lines.Text := TMacro(Item).Value;
  cbMacroActive.Checked := TMacro(Item).Active;
end;

procedure TDAMacrosFrame.ControlsToItem(Item: TCollectionItem);
begin
  TMacro(Item).Value := meMacroValue.Lines.Text;
  TMacro(Item).Active := cbMacroActive.Checked;
end;

procedure TDAMacrosFrame.cbMacroActiveClick(Sender: TObject);
begin
  if FInSelectItem or not IsControlEnabled(Sender as TControl) then
    Exit;

  Macros[lbItemName.ItemIndex].Active := cbMacroActive.Checked;
  Modified := True;

  UpdateControlsState;
end;

procedure TDAMacrosFrame.meMacroValueExit(Sender: TObject);
begin
  if FInSelectItem or not IsControlEnabled(Sender as TControl) then
    Exit;
    
  if meMacroValue.Lines.Text <> Macros[lbItemName.ItemIndex].Value then begin
    Macros[lbItemName.ItemIndex].Value := meMacroValue.Lines.Text;

    Modified := True;

    UpdateControlsState;
  end;
end;

end.
