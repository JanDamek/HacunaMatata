
//////////////////////////////////////////////////
//  SQL Server Data Access Components
//  Copyright © 1998-2012 Devart. All right reserved.
//  SDAC About Window
//////////////////////////////////////////////////

{$IFNDEF CLR}

{$I Sdac.inc}

unit SdacAbout;
{$ENDIF}
interface
uses
  Windows, SysUtils, Classes, Graphics, Forms, Controls, StdCtrls,
  Buttons, ExtCtrls, HelpUtils;

type
  TSdacAboutForm = class(TForm)
    OKBtn: TButton;
    Image1: TImage;
    Label1: TLabel;
    Label2: TLabel;
    lbVersion: TLabel;
    lbIDE: TLabel;
    Label3: TLabel;
    Label5: TLabel;
    Label6: TLabel;
    lbMail: TLabel;
    lbWeb: TLabel;
    Label4: TLabel;
    Bevel1: TBevel;
    Label8: TLabel;
    lblDBMonitorVer: TLabel;
    lbForum: TLabel;
    Label10: TLabel;
    lbEdition: TLabel;
    Bevel2: TBevel;
    procedure FormShow(Sender: TObject);
    procedure lbWebClick(Sender: TObject);
    procedure lbMailClick(Sender: TObject);
    procedure lbWebMouseMove(Sender: TObject; Shift: TShiftState; X,
      Y: Integer);
    procedure lbMailMouseMove(Sender: TObject; Shift: TShiftState; X,
      Y: Integer);
    procedure FormMouseMove(Sender: TObject; Shift: TShiftState; X,
      Y: Integer);
    procedure lbForumClick(Sender: TObject);
    procedure lbForumMouseMove(Sender: TObject; Shift: TShiftState; X,
      Y: Integer);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

procedure ShowAbout;

implementation

uses
  ShellApi, MSDesign, DBMonitorClient;

{$I SdacVer.inc}

{$IFDEF IDE}
{$R *.dfm}
{$ENDIF}
{$IFDEF MSWINDOWS}
{$R SdacAbout.dfm}
{$ENDIF}

procedure ShowAbout;
begin
  with TSdacAboutForm.Create(Application) do
  try
    ShowModal;
  finally
    Free;
  end;
end;

procedure TSdacAboutForm.FormShow(Sender: TObject);
var
  IDE: string;
begin
{$IFDEF VER5}
  IDE := 'Delphi 5';
{$ENDIF}
{$IFDEF VER6}
  IDE := 'Delphi 6';
{$ENDIF}
{$IFDEF VER7}
  IDE := 'Delphi 7';
{$ENDIF}
{$IFDEF VER9}
  IDE := 'Delphi 2005';
{$ENDIF}
{$IFDEF VER10}
  IDE := 'Delphi 2006';
{$ENDIF}
{$IFDEF VER11}
  IDE := 'RAD Studio 2007';
{$ENDIF}
{$IFDEF VER12}
  IDE := 'RAD Studio 2009';
{$ENDIF}
{$IFDEF VER14}
  IDE := 'RAD Studio 2010';
{$ENDIF}
{$IFDEF VER15}
  IDE := 'RAD Studio XE';
{$ENDIF}
{$IFDEF VER16}
  IDE := 'RAD Studio XE2';
{$ENDIF}
{$IFDEF CB5}
  IDE := 'C++Builder 5';
{$ENDIF}
{$IFDEF CB6}
  IDE := 'C++Builder 6';
{$ENDIF}
  lbVersion.Caption := SDACVersion + ' ';
  lbIDE.Caption := ' for ' + IDE;
  lbIDE.Left := lbVersion.Left + lbVersion.Width;

{$IFDEF STD}
  lbEdition.Caption := 'Standard Edition';
{$ELSE}
  lbEdition.Caption := 'Professional Edition';
{$ENDIF}

  lblDBMonitorVer.Caption := string(GetDBMonitorVersion);
end;

procedure TSdacAboutForm.lbWebClick(Sender: TObject);
begin
  OpenUrl('http://www.devart.com/sdac');
  lbWeb.Font.Color := $FF0000;
end;

procedure TSdacAboutForm.lbMailClick(Sender: TObject);
begin
  MailTo('sdac@devart.com');
  lbMail.Font.Color := $FF0000;
end;

procedure TSdacAboutForm.lbWebMouseMove(Sender: TObject; Shift: TShiftState; X,
  Y: Integer);
begin
  lbWeb.Font.Color := $4080FF;
end;

procedure TSdacAboutForm.lbMailMouseMove(Sender: TObject; Shift: TShiftState;
  X, Y: Integer);
begin
  lbMail.Font.Color := $4080FF;
end;

procedure TSdacAboutForm.FormMouseMove(Sender: TObject; Shift: TShiftState; X,
  Y: Integer);
begin
  lbWeb.Font.Color := $FF0000;
  lbMail.Font.Color := $FF0000;
  lbForum.Font.Color := $FF0000;
end;

procedure TSdacAboutForm.lbForumClick(Sender: TObject);
begin
{$IFDEF MSWINDOWS}
  OpenUrl('http://devart.com/forums/viewforum.php?f=6');
  lbForum.Font.Color := $FF0000;
{$ENDIF}
end;

procedure TSdacAboutForm.lbForumMouseMove(Sender: TObject;
  Shift: TShiftState; X, Y: Integer);
begin
  lbForum.Font.Color := $4080FF;
end;

end.
