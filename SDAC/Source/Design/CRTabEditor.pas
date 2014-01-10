
//////////////////////////////////////////////////
//  Data Access Components
//  Copyright © 1998-2012 Devart. All right reserved.
//  Tab Editor
//////////////////////////////////////////////////

{$IFNDEF CLR}

{$I Dac.inc}

unit CRTabEditor;
{$ENDIF}
interface
uses
{$IFDEF MSWINDOWS}
  Windows, Messages,
{$ENDIF}
  Graphics, Controls, Forms, Dialogs, ComCtrls, StdCtrls, ExtCtrls, Buttons,
{$IFDEF DBTOOLS}
  DBToolsClient,
{$ENDIF}
{$IFDEF FPC}
  LResources,
{$ENDIF}
  SysUtils, Classes,
  CREditor, DAEditor, CRFrame, CRDesignUtils, DADesignUtils, MemData;

type
  TCRTabEditorForm = class(TDAEditorForm)
    PageControl: TPageControl;
    procedure FormDestroy(Sender: TObject);
    procedure PageControlChange(Sender: TObject);
    procedure PageControlChanging(Sender: TObject;
      var AllowChange: Boolean);

  protected
    FFramesList: TList;

    procedure DoInit; override;
    procedure DoActivate; override;
  {$IFDEF DBTOOLS}
    procedure ExitActiveControl; override;
  {$ENDIF}
    procedure SaveControlData; override;

    function AddTab(FrameClass: TCRFrameClass; Page: TTabSheet): TCRFrame;

    function GetFrameByInitProp: TCRFrame; virtual;

    function GetModified: boolean; override;
    procedure SetModified(Value: boolean); override;

    function GetActiveFrame: TCRFrame;

    // Avoid Kylix bug 
    procedure DoPageControlChange(Sender: TObject); virtual;
    procedure DoPageControlChanging(Sender: TObject; var AllowChange: Boolean); virtual;
  public
    constructor Create(Owner: TComponent; CRDesignUtilsClass: TCRDesignUtilsClass); override;
    procedure ActivateFrame(Frame: TCRFrame);

    property ActiveFrame: TCRFrame read GetActiveFrame;    
  end;

implementation

{$IFNDEF FPC}
{$IFDEF IDE}
{$R *.dfm}
{$ELSE}
{$R CRTabEditor.dfm}
{$ENDIF}
{$ELSE}
{$R *.lfm}
{$ENDIF}

{ TCRTabEditorForm }

{
procedure TCRTabEditorForm.CreateFrames;
begin
  Assert(False, 'Must be overriden - D8 bug');
end;
}

procedure TCRTabEditorForm.DoInit;
begin
  inherited;

{$IFDEF FPC}
  PageControl.Height := Height - 49;
  PageControl.Width := Width - 16;
{$ENDIF}
end;

procedure TCRTabEditorForm.DoActivate;
var
  Frame: TCRFrame;
begin
  inherited;

  Frame := GetFrameByInitProp;

{$IFDEF FPC}
  if Frame = nil then
    Frame := GetActiveFrame;
  if (Frame = nil) and (FFramesList.Count > 0) then
    Frame := TCRFrame(FFramesList[0]);
{$ENDIF}

  if Frame <> nil then begin
    PageControl.ActivePage := Frame.Page;
    //Frame.SetFocus; // on Kylix control can be invisible in the form OnShow event
    Frame.Activate;
  end;
end;

{$IFDEF DBTOOLS}
procedure TCRTabEditorForm.ExitActiveControl;
begin
  DBTools.CheckDBToolsChanges(GetActiveFrame);
  inherited;
end;
{$ENDIF}

procedure TCRTabEditorForm.SaveControlData;
var
  ActiveFrame: TCRFrame;
begin
  ActiveFrame := GetActiveFrame;
  if ActiveFrame <> nil then
    ActiveFrame.Finish;

  inherited;
end;

function TCRTabEditorForm.AddTab(FrameClass: TCRFrameClass; Page: TTabSheet): TCRFrame;
begin
  Result := FrameClass.Create(Self);
  Result.Parent := Page;
  Result.Align := alClient;
  Result.Name := Page.Name + FrameClass.ClassName;
  Result.Editor := Self;

  FFramesList.Add(Result);
end;

function TCRTabEditorForm.GetModified: boolean;
var
  i :integer;
begin
  Result := inherited GetModified;
  for i := 0 to FFramesList.Count - 1 do
    Result := Result or TCRFrame(FFramesList[i]).Modified;
end;

procedure TCRTabEditorForm.SetModified(Value: boolean);
var
  i :integer;
begin
  inherited;
  for i := 0 to FFramesList.Count - 1 do
    TCRFrame(FFramesList[i]).Modified := Value;
end;

constructor TCRTabEditorForm.Create(Owner: TComponent; CRDesignUtilsClass: TCRDesignUtilsClass);
begin
  FFramesList := TList.Create;
  inherited;
end;

procedure TCRTabEditorForm.FormDestroy(Sender: TObject);
begin
  inherited;
  FFramesList.Free;
end;

function TCRTabEditorForm.GetActiveFrame: TCRFrame;
var
  i: integer;
begin
  for i := 0 to FFramesList.Count - 1 do
    if TCRFrame(FFramesList[i]).Page = PageControl.ActivePage then begin
      Result := TCRFrame(FFramesList[i]);
      Exit;
    end;
  Result := nil;
end;

procedure TCRTabEditorForm.ActivateFrame(Frame: TCRFrame);
var
  ActiveFrame: TCRFrame;
begin
  ActiveFrame := GetActiveFrame;
  if ActiveFrame <> nil then
    ActiveFrame.Finish;

  if Frame.Page <> PageControl.ActivePage then
    PageControl.ActivePage := Frame.Page;
  Frame.Activate;
end;

procedure TCRTabEditorForm.DoPageControlChange(Sender: TObject); 
var
  ActiveFrame: TCRFrame;
begin
{$IFDEF UNIX}
  SaveControlData;
{$ENDIF}

  ActiveFrame := GetActiveFrame;
  if ActiveFrame <> nil then
    ActiveFrame.Activate;
end;

procedure TCRTabEditorForm.PageControlChange(Sender: TObject);
begin
  DoPageControlChange(Sender); 
end;

procedure TCRTabEditorForm.DoPageControlChanging(Sender: TObject; var AllowChange: Boolean); 
var
  ActiveFrame: TCRFrame;
begin
  try
    ActiveFrame := GetActiveFrame;
    if ActiveFrame <> nil then
      ActiveFrame.Finish;
  except
    on E: Exception do begin
      AllowChange := False;
      ApplicationHandleException(E);
    end;
  end;
end;

procedure TCRTabEditorForm.PageControlChanging(Sender: TObject; var AllowChange: Boolean);
begin
  DoPageControlChanging(Sender, AllowChange); 
end;

function TCRTabEditorForm.GetFrameByInitProp: TCRFrame;
var
  i :integer;
begin
  Result := nil;
  if InitialProperty <> '' then begin
    for i := 0 to FFramesList.Count - 1 do
      if TCRFrame(FFramesList[i]).Page.Caption = InitialProperty then begin
        Result := TCRFrame(FFramesList[i]);
        Break;
      end;
    Assert(Result <> nil, 'Unknown frame ' + InitialProperty);
  end;
end;

end.
