
//////////////////////////////////////////////////
//  DB Access Components
//  Copyright © 1998-2012 Devart. All right reserved.
//  Dual List Editor
//////////////////////////////////////////////////

{$IFNDEF CLR}

{$I Dac.inc}

unit DADualListEditor;
{$ENDIF}

interface

uses
{$IFDEF MSWINDOWS}
  Windows, Messages,
{$ENDIF}
  SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  ComCtrls, Grids, DBGrids, DBCtrls, Buttons, ExtCtrls, StdCtrls,
{$IFDEF FPC}
  LResources, LCLType,
{$ENDIF}
  CRTypes, CREditor, DBAccess;

type
  TDADualListEditorForm = class(TCREditorForm)
    Panel2: TPanel;
    SrcLabel: TLabel;
    SrcList: TListBox;
    PanelButtons: TPanel;
    IncludeBtn: TSpeedButton;
    IncAllBtn: TSpeedButton;
    ExcludeBtn: TSpeedButton;
    ExAllBtn: TSpeedButton;
    Panel3: TPanel;
    DstLabel: TLabel;
    DstList: TListBox;
    PanelButtons2: TPanel;
    UpBtn: TSpeedButton;
    DownBtn: TSpeedButton;
    procedure IncludeBtnClick(Sender: TObject);
    procedure ExcludeBtnClick(Sender: TObject);
    procedure IncAllBtnClick(Sender: TObject);
    procedure ExcAllBtnClick(Sender: TObject);
    procedure DstListDragOver(Sender, Source: TObject; X, Y: Integer;
      State: TDragState; var Accept: Boolean);
    procedure DstListDragDrop(Sender, Source: TObject; X, Y: Integer);
    procedure SrcListDragOver(Sender, Source: TObject; X, Y: Integer;
      State: TDragState; var Accept: Boolean);
    procedure SrcListDragDrop(Sender, Source: TObject; X, Y: Integer);
    procedure FormResize(Sender: TObject);
    procedure UpBtnClick(Sender: TObject);
    procedure DownBtnClick(Sender: TObject);
    procedure DstListKeyDown(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure ListClick(Sender: TObject);
  protected
    procedure DoInit; override;

    procedure MoveSelected(Source, Dest: TListBox; Index: Integer; SelectNext: boolean);
    procedure MoveAll(Source, Dest: TListBox; Index: Integer);
    procedure Move(List: TListBox; FromIndex, ToIndex: integer);
    function GetFirstSelection(List: TCustomListBox): Integer;
    procedure SetButtons;

    function GetSrcLabelCaption: string; virtual;
    procedure GetSrcListItems(Items: _TStrings); virtual;
    function GetDestLabelCaption: string; virtual;
    procedure GetDstListItems(Items: _TStrings); virtual;
  end;

implementation

{$IFNDEF FPC}
{$IFDEF IDE}
{$R *.dfm}
{$ELSE}
{$R DADualListEditor.dfm}
{$ENDIF}
{$ELSE}
{$R *.lfm}
{$ENDIF}

uses
  CRFunctions;

procedure TDADualListEditorForm.IncludeBtnClick(Sender: TObject);
begin
  MoveSelected(SrcList, DstList, DstList.Items.Count, True);
end;

procedure TDADualListEditorForm.ExcludeBtnClick(Sender: TObject);
begin
  MoveSelected(DstList, SrcList, SrcList.Items.Count, True);
end;

procedure TDADualListEditorForm.IncAllBtnClick(Sender: TObject);
begin
  MoveAll(SrcList, DstList, DstList.Items.Count);
end;

procedure TDADualListEditorForm.ExcAllBtnClick(Sender: TObject);
begin
  MoveAll(DstList, SrcList, SrcList.Items.Count);
end;

procedure TDADualListEditorForm.MoveSelected(Source, Dest: TListBox; Index: Integer;
  SelectNext: boolean);
var
  i, FirstSel: integer;
begin
  for i := 0 to Dest.Items.Count - 1 do
    Dest.Selected[i] := False;

  FirstSel := -1;
  for i := Source.Items.Count - 1 downto 0 do
    if Source.Selected[i] then begin
      if Dest.Sorted then
        Dest.Items.AddObject(Source.Items[i], Source.Items.Objects[i])
      else begin
        Dest.Items.InsertObject(Index, Source.Items[i], Source.Items.Objects[i]);
        Dest.Selected[Index] := True;
      end;
      Source.Items.Delete(i);
      FirstSel := i;
    end;

  if FirstSel >= 0 then begin
    if SelectNext then
      if Source.Items.Count > FirstSel then
        Source.Selected[FirstSel] := True
      else
        if Source.Items.Count > 0 then
          Source.Selected[Source.Items.Count - 1] := True;
    Modified := True;
    SetButtons;
  end;
end;

procedure TDADualListEditorForm.MoveAll(Source, Dest: TListBox; Index: Integer);
var
  i: integer;
begin
  for i := 0 to Dest.Items.Count - 1 do
    Dest.Selected[i] := False;

  for i := Source.Items.Count - 1 downto 0 do
    if Dest.Sorted then
      Dest.Items.AddObject(Source.Items[i], Source.Items.Objects[i])
    else begin
      Dest.Items.InsertObject(Index, Source.Items[i], Source.Items.Objects[i]);
      Dest.Selected[Index] := True;
    end;
  Source.Items.Clear;

  Modified := True;
  SetButtons;
end;

procedure TDADualListEditorForm.SetButtons;
var
  SrcEmpty, DstEmpty: Boolean;
begin
  SrcEmpty := SrcList.Items.Count = 0;
  DstEmpty := DstList.Items.Count = 0;
  IncludeBtn.Enabled := SrcList.SelCount > 0;
  IncAllBtn.Enabled := not SrcEmpty;
  ExcludeBtn.Enabled := DstList.SelCount > 0;
  ExAllBtn.Enabled := not DstEmpty;
  UpBtn.Enabled := (DstList.SelCount = 1) and (GetFirstSelection(DstList) > 0);
  DownBtn.Enabled := (DstList.SelCount = 1) and (GetFirstSelection(DstList) < DstList.Items.Count - 1);
end;

function TDADualListEditorForm.GetFirstSelection(List: TCustomListBox): Integer;
var
  i: integer;
begin
  for i := 0 to List.Items.Count - 1 do
    if List.Selected[i] then begin
      Result := i;
      Exit;
    end;
  Result := -1;
end;

procedure TDADualListEditorForm.DoInit;
var
  i, j: integer;
  List: _TStringList;
begin
  inherited;

  List := _TStringList.Create;
  try
    SrcLabel.Caption := GetSrcLabelCaption;
    GetSrcListItems(List);
    AssignStrings(List, SrcList.Items);
    DstLabel.Caption := GetDestLabelCaption;
    GetDstListItems(List);
    AssignStrings(List, DstList.Items);
  finally
    List.Free;
  end;

  for i := 0 to DstList.Items.Count - 1 do begin
    j := SrcList.Items.IndexOf(DstList.Items[i]);
    if j <> -1 then
      SrcList.Items.Delete(j);
  end;

  SetButtons;
  ConfirmCancel := False;
  Modified := False;
  FormResize(nil);
end;

procedure TDADualListEditorForm.DstListDragOver(Sender, Source: TObject; X,
  Y: Integer; State: TDragState; var Accept: Boolean);
begin
  Accept := (Source = SrcList) and (SrcList.SelCount > 0) or
    (Source = DstList) and (DstList.SelCount = 1);
end;

procedure TDADualListEditorForm.DstListDragDrop(Sender, Source: TObject; X,
  Y: Integer);
var
  pos: TPoint;
  Index, SelIndex: Integer;
begin
  pos.X := X;
  pos.Y := Y;
  Index := DstList.ItemAtPos(pos, False);
  if Source = SrcList then
    MoveSelected(SrcList, DstList, Index, False)
  else
    if Source = DstList then begin
      SelIndex := GetFirstSelection(DstList);
      Move(DstList, SelIndex, Index);
    end;
end;

procedure TDADualListEditorForm.SrcListDragOver(Sender, Source: TObject; X,
  Y: Integer; State: TDragState; var Accept: Boolean);
begin
  Accept := (Source = DstList) and (DstList.SelCount > 0);
end;

procedure TDADualListEditorForm.SrcListDragDrop(Sender, Source: TObject; X,
  Y: Integer);
var
  pos: TPoint;
  Index: Integer;
begin
  if Source = DstList then begin
    pos.X := X;
    pos.Y := Y;
    Index := SrcList.ItemAtPos(pos, False);
    MoveSelected(DstList, SrcList, Index, False);
  end;
end;

procedure TDADualListEditorForm.FormResize(Sender: TObject);
begin
  inherited;
  Panel2.Width := (ClientWidth - PanelButtons.Width * 2) div 2;
end;

function TDADualListEditorForm.GetSrcLabelCaption: string;
begin
  Result := 'Source list items';
end;

procedure TDADualListEditorForm.GetSrcListItems(Items: _TStrings);
begin
  Items.Clear;
end;

function TDADualListEditorForm.GetDestLabelCaption: string;
begin
  Result := 'Destination list items';
end;

procedure TDADualListEditorForm.GetDstListItems(Items: _TStrings);
begin
  Items.Clear;
end;

procedure TDADualListEditorForm.Move(List: TListBox; FromIndex, ToIndex: integer);
begin
  if (FromIndex >= 0) and (ToIndex >= 0) and (ToIndex < List.Items.Count) and (FromIndex <> ToIndex)
  then begin
    List.Items.Move(FromIndex, ToIndex);
    List.Selected[ToIndex] := True;
    Modified := True;
    SetButtons;
  end;
end;

procedure TDADualListEditorForm.UpBtnClick(Sender: TObject);
var
  SelIndex: integer;
begin
  SelIndex := GetFirstSelection(DstList);
  Move(DstList, SelIndex, SelIndex - 1);
end;

procedure TDADualListEditorForm.DownBtnClick(Sender: TObject);
var
  SelIndex: integer;
begin
  SelIndex := GetFirstSelection(DstList);
  Move(DstList, SelIndex, SelIndex + 1);
end;

procedure TDADualListEditorForm.DstListKeyDown(Sender: TObject;
  var Key: Word; Shift: TShiftState);
begin
  if Shift = [ssCtrl] then
    case Key of
      VK_UP: begin
        UpBtnClick(nil);
        Key := 0;
      end;
      VK_DOWN: begin
        DownBtnClick(nil);
        Key := 0;
      end;
    end;
end;

procedure TDADualListEditorForm.ListClick(Sender: TObject);
begin
  SetButtons;
end;

end.
