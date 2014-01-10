
//////////////////////////////////////////////////
//  SQL Server Data Access Components
//  Copyright © 1998-2012 Devart. All right reserved.
//  Connect Form
//////////////////////////////////////////////////

{$DEFINE FMX}

unit MSConnectFormFmx;

interface

uses
  Classes, SysUtils, TypInfo,
{$IFDEF MSWINDOWS}
  Winapi.Windows, System.Win.Registry,
{$ENDIF}
  System.UITypes, FMX.Types, FMX.Platform,
  FMX.Forms, FMX.Controls, FMX.Edit, FMX.ListBox, FMX.Memo,
  CRTypes, MemUtils, DBAccess, OLEDBAccess,
  OLEDBC, MSAccess;

type
  TMSConnectForm = class(TForm)
    Panel: TPanel;
    lbUsername: TLabel;
    lbPassword: TLabel;
    lbServer: TLabel;
    edUsername: TEdit;
    btConnect: TButton;
    btCancel: TButton;
    edPassword: TEdit;
    edServer: TEdit;
    edDatabase: TEdit;
    lbDatabase: TLabel;
    procedure btConnectClick(Sender: TObject);
  private
    FConnectDialog: TCustomConnectDialog;
    FRetries: integer;
    FOldCreateOrder: boolean;
    FRetry: boolean;

    FListGot: boolean;

    procedure SetConnectDialog(Value: TCustomConnectDialog);

  protected
    procedure DoInit; virtual;
    procedure DoConnect; virtual;

  public

  published
    property ConnectDialog: TCustomConnectDialog read FConnectDialog write SetConnectDialog;

    property OldCreateOrder: boolean read FOldCreateOrder write FOldCreateOrder;
  end;

implementation

{$R *.fmx}

uses
  CRFunctions, DacFMX, SdacFMX;

procedure TMSConnectForm.DoInit;
var
  PropInfo: PPropInfo;
begin
  FRetry := False;
  FRetries := FConnectDialog.Retries;
  Caption := FConnectDialog.Caption;
  FListGot := False;

  with FConnectDialog do begin
    lbUsername.Text := UsernameLabel;
    lbPassword.Text := PasswordLabel;
    lbServer.Text := ServerLabel;
    btConnect.Text := ConnectButton;
    btCancel.Text := CancelButton;

    edUsername.Text := Connection.Username;
    edPassword.Text := Connection.Password;
    edServer.Text := Connection.Server;

    if (edUsername.Text <> '') and (edPassword.Text = '') then
      ActiveControl := edPassword;
  end;

  PropInfo := GetPropInfo(FConnectDialog, 'DatabaseLabel');
  if PropInfo <> nil then
    lbDatabase.Text := GetStrProp(FConnectDialog, PropInfo);
  if FConnectDialog.Connection is TCustomMSConnection then
    edDatabase.Text := TCustomMSConnection(FConnectDialog.Connection).Database;
  if TCustomMSConnection(FConnectDialog.Connection).Options.Provider = prCompact then begin
    lbUsername.Enabled := False;
    edUsername.Text := '';
    edUsername.Enabled := False;
    lbServer.Enabled := False;
    edServer.Text := '';
    edServer.Enabled := False;
  end;
end;

procedure TMSConnectForm.DoConnect;
begin
  FConnectDialog.Connection.Password := edPassword.Text;
  FConnectDialog.Connection.Server := edServer.Text;
  FConnectDialog.Connection.UserName := edUsername.Text;
  if (FConnectDialog.Connection is TCustomMSConnection) then
    TCustomMSConnection(FConnectDialog.Connection).Database := edDatabase.Text;
  try
    FConnectDialog.Connection.PerformConnect(FRetry);
    ModalResult := mrOk;
  except
    on E: EMSError do begin
      Dec(FRetries);
      FRetry := True;
      if FRetries = 0 then
        ModalResult := mrCancel;

      if E.MSSQLErrorCode <= NE_MAX_NETERROR then
        ActiveControl := edServer
      else
      if E.OLEDBErrorCode = DB_SEC_E_AUTH_FAILED then
        if ActiveControl <> edUsername then
          ActiveControl := edPassword;
      raise;
    end
    else
      raise;
  end;
end;

procedure TMSConnectForm.SetConnectDialog(Value: TCustomConnectDialog);
begin
  FConnectDialog := Value;
  DoInit;
end;

procedure TMSConnectForm.btConnectClick(Sender: TObject);
begin
  DoConnect;
end;

initialization
  if GetClass('TMSConnectForm') = nil then
    Classes.RegisterClass(TMSConnectForm);
end.
