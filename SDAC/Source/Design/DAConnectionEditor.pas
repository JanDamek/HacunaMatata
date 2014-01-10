
//////////////////////////////////////////////////
//  Data Access Components
//  Copyright © 1998-2012 Devart. All right reserved.
//  ConnectionEditor Editor
//////////////////////////////////////////////////

{$IFNDEF CLR}
{$I Dac.inc}

unit DAConnectionEditor;
{$ENDIF}
interface
uses
{$IFDEF MSWINDOWS}
  Windows, Messages,
{$ENDIF}
  SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, ExtCtrls, ComCtrls, Registry, DacVcl, Buttons,
  {$IFNDEF FPC}Mask,{$ENDIF}
{$IFDEF DBTOOLS}
  DBToolsClient,
{$ENDIF}
{$IFDEF FPC}
  LResources, LCLType,
{$ENDIF}
  CRTypes, CREditor, DAEditor, DBAccess, CRDesignUtils, DADesignUtils;

type
  TDAConnectionEditorForm = class(TDAEditorForm)
    PageControl: TPageControl;
    shConnect: TTabSheet;
    Panel: TPanel;
    lbUsername: TLabel;
    lbPassword: TLabel;
    lbServer: TLabel;
    edUsername: TEdit;
  {$IFNDEF FPC}
    edPassword: TMaskEdit;
  {$ELSE}
    edPassword: TEdit;
  {$ENDIF}
    edServer: TComboBox;
    btConnect: TButton;
    btDisconnect: TButton;
    shInfo: TTabSheet;
    shAbout: TTabSheet;
    btClose: TButton;
    meInfo: TMemo;
    Label1: TLabel;
    Label2: TLabel;
    lbVersion: TLabel;
    Label3: TLabel;
    Label5: TLabel;
    Label6: TLabel;
    lbWeb: TLabel;
    lbMail: TLabel;
    lbIDE: TLabel;
    cbLoginPrompt: TCheckBox;
    shRed: TShape;
    shYellow: TShape;
    shGreen: TShape;
    imPeng: TImage;
    lbEdition: TLabel;
    procedure btDisconnectClick(Sender: TObject);
    procedure lbWebClick(Sender: TObject);
    procedure lbMailClick(Sender: TObject);
    procedure cbLoginPromptClick(Sender: TObject);
    procedure lbWebMouseMove(Sender: TObject; Shift: TShiftState; X,
      Y: Integer);
    procedure shAboutMouseMove(Sender: TObject; Shift: TShiftState; X,
      Y: Integer);
    procedure lbMailMouseMove(Sender: TObject; Shift: TShiftState; X,
      Y: Integer);
    procedure edUsernameChange(Sender: TObject);
    procedure edPasswordChange(Sender: TObject);
    procedure edServerChange(Sender: TObject);
    procedure btConnectClick(Sender: TObject);
    procedure edServerDropDown(Sender: TObject); virtual;
    procedure PageControlChange(Sender: TObject);
    procedure edServerKeyUp(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure edServerExit(Sender: TObject);

  protected
    FConnection: TCustomDAConnection;
    FInDoInit: boolean;
    FConnectDialog: TCustomConnectDialog;
  {$IFDEF MSWINDOWS}
    FRegistry: TRegistry;
  {$ENDIF}
  {$IFDEF DBTOOLS}
    FInExistingChange: boolean;

    function GetExistingConnectionComboBox: TComboBox; virtual;
    procedure ChooseExistingConnection;
    function GetConnectionCondition: string; virtual;
  {$ENDIF}

    procedure GetServerList(List: _TStrings); virtual;
  {$IFDEF MSWINDOWS}
    procedure AddServerToList; virtual;
  {$ENDIF}

    procedure ShowState(Yellow: boolean = False); virtual;
    procedure ConnToControls; virtual;
    procedure ControlsToConn; virtual;
    procedure FillInfo; virtual;
    procedure PerformConnect; virtual;
    procedure PerformDisconnect; virtual;
    function IsConnected: boolean; virtual;
    procedure AssignUsername(const Value: string); virtual;
    procedure AssignPassword(const Value: string); virtual;
    procedure AssignServer(const Value: string); virtual;
    procedure AssignLoginPrompt(Value: boolean); virtual;
    function GetConnectDialogClass: TConnectDialogClass; virtual;

    procedure DoInit; override;
    procedure DoActivate; override;
    procedure UpdateVersionPosition; virtual;

    function GetComponent: TComponent; override;
    procedure SetComponent(Value: TComponent); override;

  public
    constructor Create(Owner: TComponent; CRDesignUtilsClass: TCRDesignUtilsClass); override;
    destructor Destroy; override;

  end;

implementation

uses
  {$IFDEF MSWINDOWS}ShellAPI, HelpUtils,{$ENDIF}
  {$IFDEF VER6P}Variants, {$ENDIF}
  CRFunctions, MemData;

{$IFNDEF FPC}
{$IFDEF IDE}
{$R *.dfm}
{$ELSE}
{$R DAConnectionEditor.dfm}
{$ENDIF}
{$ELSE}
{$R *.lfm}
{$ENDIF}

{$I DacVer.inc}

constructor TDAConnectionEditorForm.Create(Owner: TComponent; CRDesignUtilsClass: TCRDesignUtilsClass);
begin
  inherited;

{$IFDEF MSWINDOWS}
  FRegistry := TRegistry.Create(KEY_READ OR KEY_WRITE);
  if not FRegistry.OpenKey('\SOFTWARE\Devart\' + FCRDesignUtilsClass.GetProjectName + '\Connect', True) then
    FreeAndNil(FRegistry);
{$ENDIF}
end;

destructor TDAConnectionEditorForm.Destroy;
begin
{$IFDEF MSWINDOWS}
  FreeAndNil(FRegistry);
{$ENDIF}

  FConnectDialog.Free;

  inherited;
end;

procedure TDAConnectionEditorForm.DoInit;
var
  IDE: string;
begin
  FInDoInit := True;
  try
    inherited;

  {$IFDEF D5}
    IDE := 'Delphi 5';
  {$ENDIF}
  {$IFDEF D6}
    IDE := 'Delphi 6';
  {$ENDIF}
  {$IFDEF D7}
    IDE := 'Delphi 7';
  {$ENDIF}
  {$IFDEF D8}
    IDE := 'Delphi 8';
  {$ENDIF}
  {$IFDEF D9}
    IDE := 'Delphi 2005';
  {$ENDIF}
  {$IFDEF D10}
    IDE := 'Delphi 2006';
  {$ENDIF}
  {$IFDEF D11}
    IDE := 'Delphi 2007';
  {$ENDIF}
  {$IFDEF D12}
    IDE := 'Delphi 2009';
  {$ENDIF}
  {$IFDEF D14}
    IDE := 'Delphi 2010';
  {$ENDIF}
  {$IFDEF D15}
    IDE := 'Delphi XE';
  {$ENDIF}
  {$IFDEF D16}
    IDE := 'Delphi XE2';
  {$ENDIF}
  {$IFDEF CB5}
    IDE := 'C++Builder 5';
  {$ENDIF}
  {$IFDEF CB6}
    IDE := 'C++Builder 6';
  {$ENDIF}
  {$IFDEF FPC}
    IDE := 'Lazarus';
  {$ENDIF}

    lbVersion.Caption := DACVersion + ' ';
    lbIDE.Caption := 'for ' + IDE;
    UpdateVersionPosition;

  {$IFDEF STD}
    lbEdition.Caption := 'Standard Edition';
  {$ELSE}
    lbEdition.Caption := 'Professional Edition';
  {$ENDIF}

  {$IFDEF DBTOOLS}
    if DADesignUtilsClass.DBToolsAvailable then begin
      GetDBToolsService(DADesignUtilsClass).GetConnections(GetExistingConnectionComboBox.Items, GetConnectionCondition);
      ChooseExistingConnection;
    end;  
  {$ENDIF}

    FConnectDialog := GetConnectDialogClass.Create(nil);
    TDBAccessUtils.SetConnection(FConnectDialog, FConnection);
    TDBAccessUtils.SetUseServerHistory(FConnectDialog, False);

    ConnToControls;

    ShowState;
  finally
    FInDoInit := False;
  end;
end;

procedure TDAConnectionEditorForm.DoActivate;
begin
  inherited;

{$IFDEF FPC}
  if (PageControl.ActivePage = nil) and (PageControl.PageCount > 0) then
    PageControl.ActivePage := PageControl.Pages[0];
{$ENDIF}
end;

procedure TDAConnectionEditorForm.UpdateVersionPosition;
begin
  lbIDE.Left := lbVersion.Left + lbVersion.Width;
end;

function TDAConnectionEditorForm.GetComponent: TComponent;
begin
  Result := FConnection;
end;

procedure TDAConnectionEditorForm.SetComponent(Value: TComponent);
begin
  FConnection := Value as TCustomDAConnection;
end;

{$IFDEF DBTOOLS}
function TDAConnectionEditorForm.GetExistingConnectionComboBox: TComboBox;
begin
  Result := nil;
  Assert(False, 'Must be overriden');
end;

procedure TDAConnectionEditorForm.ChooseExistingConnection;
begin
  if not FInExistingChange and DADesignUtilsClass.DBToolsAvailable then
    with GetExistingConnectionComboBox do
      ItemIndex := Items.IndexOf(GetDBToolsService(DADesignUtilsClass).FindConnectionName(FConnection));
end;

function TDAConnectionEditorForm.GetConnectionCondition: string;
begin
  Result := '';
end;
{$ENDIF}

procedure TDAConnectionEditorForm.ConnToControls;
begin
  edUsername.Text := FConnection.Username;
  edPassword.Text := FConnection.Password;
  edServer.Text := FConnection.Server;
  cbLoginPrompt.Checked := FConnection.LoginPrompt;
end;

procedure TDAConnectionEditorForm.ControlsToConn;
begin
  // all parameters are set in controls OnChange event handlers
end;

procedure TDAConnectionEditorForm.ShowState(Yellow: boolean);
begin
  btDisconnect.Enabled := IsConnected;

  shRed.Brush.Color := clBtnFace;
  shYellow.Brush.Color := clBtnFace;
  shGreen.Brush.Color := clBtnFace;

  if Yellow then begin
    shYellow.Brush.Color := clYellow;
    shYellow.Update;
  end
  else
    if IsConnected then begin
      shGreen.Brush.Color := clGreen;
      shYellow.Update;
    end
    else
      shRed.Brush.Color := clRed;
end;

procedure TDAConnectionEditorForm.lbWebClick(Sender: TObject);
begin
{$IFDEF MSWINDOWS}
  OpenUrl('http://' + lbWeb.Caption);
  lbWeb.Font.Color := $FF0000;
{$ENDIF}
end;

procedure TDAConnectionEditorForm.lbMailClick(Sender: TObject);
begin
{$IFDEF MSWINDOWS}
  MailTo(lbMail.Caption);
  lbMail.Font.Color := $FF0000;
{$ENDIF}
end;

procedure TDAConnectionEditorForm.cbLoginPromptClick(Sender: TObject);
begin
  AssignLoginPrompt(cbLoginPrompt.Checked);
end;

procedure TDAConnectionEditorForm.lbWebMouseMove(Sender: TObject;
  Shift: TShiftState; X, Y: Integer);
begin
  lbWeb.Font.Color := $4080FF;
end;

procedure TDAConnectionEditorForm.lbMailMouseMove(Sender: TObject;
  Shift: TShiftState; X, Y: Integer);
begin
  lbMail.Font.Color := $4080FF;
end;

procedure TDAConnectionEditorForm.shAboutMouseMove(Sender: TObject;
  Shift: TShiftState; X, Y: Integer);
begin
  lbWeb.Font.Color := $FF0000;
  lbMail.Font.Color := $FF0000;
end;

procedure TDAConnectionEditorForm.edUsernameChange(Sender: TObject);
begin
  if FInDoInit then
    Exit;

  try
    AssignUsername(edUsername.Text);
  finally
    ShowState;
  end;
end;

procedure TDAConnectionEditorForm.edPasswordChange(Sender: TObject);
begin
  if FInDoInit then
    Exit;

  try
    AssignPassword(edPassword.Text);
  finally
    ShowState;
  end;
end;

procedure TDAConnectionEditorForm.edServerChange(Sender: TObject);
begin
  if FInDoInit then
    Exit;

  try
    AssignServer(edServer.Text);
  finally
    ShowState;
  end;
end;

procedure TDAConnectionEditorForm.btConnectClick(Sender: TObject);
begin
  ShowState(True);
{$IFDEF DBTOOLS}
  ChooseExistingConnection;
{$ENDIF}
  StartWait;
  try
    ControlsToConn;
    PerformConnect;
  {$IFDEF MSWINDOWS}
    if IsConnected then
      AddServerToList;
  {$ENDIF}
  finally
    StopWait;
    ShowState;
  end;

  ModalResult := mrOk;
end;

procedure TDAConnectionEditorForm.btDisconnectClick(Sender: TObject);
begin
{$IFDEF DBTOOLS}
  ChooseExistingConnection;
{$ENDIF}
  try
    PerformDisconnect;
  finally
    ShowState;
  end;
end;

procedure TDAConnectionEditorForm.edServerDropDown(Sender: TObject);
var
  List: _TStringList;
begin
{$IFDEF UNIX}
  (Sender as TComboBox).OnGetItems := nil;
  try
{$ENDIF}
  StartWait;
  List := _TStringList.Create;
  try
    GetServerList(List);
    AssignStrings(List, edServer.Items);
  finally
    StopWait;
    List.Free;
  end;
{$IFDEF UNIX}
  finally
    (Sender as TComboBox).OnGetItems := edServerDropDown;
  end;
{$ENDIF}
end;

procedure TDAConnectionEditorForm.GetServerList(List: _TStrings);
begin
  FConnectDialog.GetServerList(List);
end;

{$IFDEF MSWINDOWS}
procedure TDAConnectionEditorForm.AddServerToList;
begin
  try
    TDBAccessUtils.SaveServerListToRegistry(FConnectDialog);
  finally
    SetCursor(crDefault);
  end;
end;
{$ENDIF}

procedure TDAConnectionEditorForm.FillInfo;
var
  OldLoginPrompt: boolean;

begin
  OldLoginPrompt := FConnection.LoginPrompt;
  try
    FConnection.LoginPrompt := False;

    if not IsConnected then
      try
        ShowState(True);
        PerformConnect;
      except
        on E: Exception do
        begin
          // PageControl.ActivePage := shConnect;
          // Application.ShowException(E); - silent exception. Please see CR MyDAC 3443
        end;
      end;
    meInfo.Lines.Clear;
  finally
    FConnection.LoginPrompt := OldLoginPrompt;
    ShowState(False);
  end;
end;

procedure TDAConnectionEditorForm.PerformConnect;
begin
  FConnection.PerformConnect;
end;

procedure TDAConnectionEditorForm.PerformDisconnect;
begin
  FConnection.Disconnect;
end;

function TDAConnectionEditorForm.IsConnected: boolean;
begin
  Result := FConnection.Connected;
end;

procedure TDAConnectionEditorForm.AssignUsername(const Value: string);
begin
  FConnection.Username := Value;
end;

procedure TDAConnectionEditorForm.AssignPassword(const Value: string);
begin
  FConnection.Password := Value;
end;

procedure TDAConnectionEditorForm.AssignServer(const Value: string);
begin
  FConnection.Server := Value;
end;

procedure TDAConnectionEditorForm.AssignLoginPrompt(Value: boolean);
begin
  FConnection.LoginPrompt := Value;
end;

function TDAConnectionEditorForm.GetConnectDialogClass: TConnectDialogClass;
begin
  Assert(FConnection <> nil);
  Result := TDBAccessUtils.ConnectDialogClass(FConnection);
end;

procedure TDAConnectionEditorForm.PageControlChange(Sender: TObject);
begin
  if PageControl.ActivePage = shInfo then
    FillInfo;
end;

procedure TDAConnectionEditorForm.edServerKeyUp(Sender: TObject;
  var Key: Word; Shift: TShiftState);
begin
  if Key = VK_RETURN then
    edServerChange(Sender);
end;

procedure TDAConnectionEditorForm.edServerExit(Sender: TObject);
begin
{$IFDEF DBTOOLS}
  ChooseExistingConnection;
{$ENDIF}
end;

end.
