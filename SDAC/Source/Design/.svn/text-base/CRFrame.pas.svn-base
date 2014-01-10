
//////////////////////////////////////////////////
//  DB Access Components
//  Copyright © 1998-2012 Devart. All right reserved.
//  Base Frame
//////////////////////////////////////////////////

{$IFNDEF CLR}

{$I Dac.inc}

unit CRFrame;
{$ENDIF}

interface

uses
  Controls, ComCtrls, Forms,
{$IFDEF DBTOOLS}
  DBToolsClient,
{$ENDIF}
{$IFDEF FPC}
  LResources,
{$ENDIF}
  Classes, DAEditor;

type
  TCRFrameClass = class of TCRFrame;

  TCRFrame = class(TFrame)
    procedure FrameEnter(Sender: TObject);
  protected
    FModified: boolean;
    FEditor: TDAEditorForm;
    FActivated: boolean; // To avoid duplicate call (for example, on TCRFrame.FrameExit and PageControl.OnChanging events)

    function GetPage: TTabSheet;
    procedure DoActivate; virtual;
    procedure DoFinish; virtual;

  public
    function ActiveControl: TWinControl; virtual; // Return default control for this frame
    procedure Activate;
    procedure Finish;
    procedure ReActivate;

    property Page: TTabSheet read GetPage;
    property Editor: TDAEditorForm read FEditor write FEditor;
    property Modified: boolean read FModified write FModified;

  end;

implementation

{$IFNDEF FPC}
{$IFDEF IDE}
{$R *.dfm}
{$ELSE}
{$R CRFrame.dfm}
{$ENDIF}
{$ELSE}
{$R *.lfm}
{$ENDIF}

{ TCRFrame }

function TCRFrame.ActiveControl: TWinControl;
begin
  Result := nil;
end;

function TCRFrame.GetPage: TTabSheet;
begin
  Result := Parent as TTabSheet;
end;

procedure TCRFrame.DoActivate;
begin
end;

procedure TCRFrame.DoFinish;
begin
{$IFDEF DBTOOLS}
  DBTools.CheckDBToolsChanges(Self);
{$ENDIF}
end;

procedure TCRFrame.Activate;
begin
  if not FActivated then
    ReActivate;
end;

procedure TCRFrame.Finish;
begin
  if FActivated then
    DoFinish;
  FActivated := False;
end;

procedure TCRFrame.FrameEnter(Sender: TObject);
begin
  Activate;
end;

procedure TCRFrame.ReActivate;
begin
  DoActivate;
  FActivated := True;
end;

end.
