//////////////////////////////////////////////////
//  SQL Server Data Access Components
//  Copyright © 1998-2012 Devart. All right reserved.
//  MSConnection Editor
//////////////////////////////////////////////////

{$IFNDEF CLR}

{$I Sdac.inc}

unit MSConnectionEditor;
{$ENDIF}

interface

uses
{$IFDEF FPC}
  LResources,
{$ENDIF}
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms,
  {$IFNDEF FPC}Mask,{$ENDIF} Dialogs, StdCtrls, ExtCtrls, ComCtrls, Buttons,
  DAConnectionEditor, DBAccess, MSAccess, OLEDBAccess, CRTypes;

const
  WM_SETDATABASETEXT = WM_USER + 1;

type
  TMSConnectionEditorForm = class(TDAConnectionEditorForm)
    lbDatabase: TLabel;
    edDatabase: TComboBox;
    rgAuth: TRadioGroup;
    btQueryAnalyzer: TButton;
    btManagementStudio: TButton;
    procedure edDatabaseDropDown(Sender: TObject);
    procedure rgAuthClick(Sender: TObject);
    procedure btQueryAnalyzerClick(Sender: TObject);
    procedure edDatabaseExit(Sender: TObject);
    procedure edDatabaseKeyUp(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure edDatabaseChange(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);
    procedure btManagementStudioClick(Sender: TObject);
    procedure edServerDropDown(Sender: TObject); override;
  private
    FDataBaseText: string;
    FCurrItemIndex: Integer;
    FListGot: boolean;
    procedure WMSetDataBaseText(var Message: TMessage); message WM_SETDATABASETEXT;
  protected
    function GetConnection: TCustomMSConnection;
    procedure SetConnection(Value: TCustomMSConnection);

    procedure DoInit; override;
    procedure FillInfo; override;

    procedure ConnToControls; override;

    procedure AddServerToList; override;

    procedure GetDatabaseList(List: _TStrings);
    function IsValidKeyValue(Value: string; Name: string): boolean;
  public
    property Connection: TCustomMSConnection read GetConnection write SetConnection;
  end;

implementation

{$IFNDEF FPC}
{$IFDEF IDE}
{$R *.dfm}
{$ELSE}
{$R MSConnectionEditor.dfm}
{$ENDIF}
{$ELSE}
{$R *.lfm}
{$ENDIF}

uses
  CRFunctions, DacVcl{$IFDEF SDAC}, MSDesign{$ENDIF}, Registry;

{ TMSConnectionEditorForm }

function TMSConnectionEditorForm.GetConnection: TCustomMSConnection;
begin
  Result := FConnection as TCustomMSConnection;
end;

procedure TMSConnectionEditorForm.SetConnection(Value: TCustomMSConnection);
begin
  FConnection := Value;
end;

procedure TMSConnectionEditorForm.DoInit;
begin
  inherited;
  
  FInDoInit := True;
  try
    lbVersion.Caption := SDACVersion + ' ';
    UpdateVersionPosition;

  {$IFDEF STD}
    lbEdition.Caption := 'Standard Edition';
  {$ELSE}
    lbEdition.Caption := 'Professional Edition';
  {$ENDIF}

  {$IFDEF SDAC}
    btQueryAnalyzer.Visible := IsServerToolInstalled(stQueryAnalyser) and (Connection.Options.Provider <> prCompact);
    btManagementStudio.Visible := IsServerToolInstalled(stManagementStudio) and (Connection.Options.Provider <> prCompact);
  {$ENDIF}
    FCurrItemIndex := -1;
    if Connection.Options.Provider = prCompact then begin
      //cbLoginPrompt.Enabled := False;
      rgAuth.Enabled := False;
      lbUsername.Enabled := False;
      edUserName.Enabled := False;
      lbServer.Enabled := False;
      edServer.Enabled := False;
    end;
    FListGot := False;
  finally
    FInDoInit := False;
  end;
end;

procedure TMSConnectionEditorForm.FillInfo;
var
  OldLoginPrompt: boolean;
  OldConnectionTimeout: integer;
  OLEDBConnection: TOLEDBConnection;
  St: string;
begin
  OldLoginPrompt := Connection.LoginPrompt;
  OldConnectionTimeout := 1;
  if Connection is TMSConnection then
    OldConnectionTimeout := TMSConnection(Connection).ConnectionTimeout;
  try
    Connection.LoginPrompt := False;
    if not IsConnected and
      not ((Connection.Username = '') and (Connection.Password = '') and
      (Connection.Server = '') and (Connection.Database = '')) then
      try
        ShowState(True);
        if Connection is TMSConnection then
          TMSConnection(Connection).ConnectionTimeout := 1;
        Connection.Connect;
      except
        on E: Exception do
        begin
          //Application.ShowException(E); - silent exception. Please see CR MyDAC 3443
        end;
      end;
    meInfo.Lines.Clear;
    OLEDBConnection := TMSAccessUtils.FIConnection(Connection);
    if OLEDBConnection <> nil then
    begin
      if Connection.Connected then begin
        St := OLEDBConnection.DBMSName + ': ' + OLEDBConnection.DBMSVer;
        if st <> ':' then
          meInfo.Lines.Add(St);
      end;
      if Connection.Options.Provider <> prCompact then begin
        St := OLEDBConnection.ProviderFriendlyName + ': ' + OLEDBConnection.ProviderVer;
        if St <> ': ' then
          meInfo.Lines.Add(St);
      end;
    end;
  finally
    Connection.LoginPrompt := OldLoginPrompt;
    if Connection is TMSConnection then
      TMSConnection(Connection).ConnectionTimeout := OldConnectionTimeout;
    ShowState(False);
  end;
end;

procedure TMSConnectionEditorForm.ConnToControls;
begin
  inherited;

  edDatabase.Text := Connection.Database;
  if Connection is TMSConnection then
    rgAuth.ItemIndex := Ord(TMSConnection(Connection).Authentication);
end;

procedure TMSConnectionEditorForm.edServerDropDown(Sender: TObject);
begin
  if FListGot then
    Exit;
    
  FListGot := True;
  
  inherited;
end;

procedure TMSConnectionEditorForm.AddServerToList;
var
  ConnectKey: string;
  ValueNames, Values: TStringList;
  i: integer;
  s: string;
begin
  if Connection.Options.Provider = prCompact then begin
    if FRegistry <> nil then begin
      ValueNames := nil;
      Values := nil;
      ConnectKey := FRegistry.CurrentPath;
      try
        ValueNames := TStringList.Create;
        Values := TStringList.Create;

        Values.Add(Connection.Database); // Add current database at first position

        FRegistry.CloseKey;
        FRegistry.OpenKey(ConnectKey + '\Everywhere', True);

        FRegistry.GetValueNames(ValueNames);
        ValueNames.Sort;

        for i := 0 to ValueNames.Count - 1 do begin
          s := Trim(FRegistry.ReadString(ValueNames[i]));
          if (s <> '') and (Values.IndexOf(s) = -1) then
            Values.Add(s);
          FRegistry.DeleteValue(ValueNames[i]); // Clear old list
        end;

        // Store updated list in registry
        for i := 0 to Values.Count - 1 do begin
          s := _Format('Database %d', [i]);
          FRegistry.WriteString(s, Values[i]);
        end;

      finally
        ValueNames.Free;
        Values.Free;
      end;
    end;
  end
  else
    inherited;
end;

procedure TMSConnectionEditorForm.GetDatabaseList(List: _TStrings);
var
  ConnectKey: string;
  ValueNames, Values: TStringList;
  i: integer;
begin
  List.Clear;
  if FRegistry <> nil then begin
    ValueNames := nil;
    Values := nil;
    try
      ValueNames := TStringList.Create;
      Values := TStringList.Create;
      ConnectKey := FRegistry.CurrentPath;
      try
        FRegistry.CloseKey;
        if FRegistry.OpenKey(ConnectKey + '\Everywhere', False) then begin
          FRegistry.GetValueNames(ValueNames);
          ValueNames.Sort;
          for i := 0 to ValueNames.Count - 1 do
            if IsValidKeyValue(ValueNames[i], 'Database') then
              List.Add(FRegistry.ReadString(ValueNames[i]));
        end;
      finally
        FRegistry.CloseKey;
        FRegistry.OpenKey(ConnectKey, False);
      end;
    finally
      ValueNames.Free;
      Values.Free;
    end;
  end;
  List.Add('<Browse...>');
end;

function TMSConnectionEditorForm.IsValidKeyValue(Value: string; Name: string): boolean;
var
  p: integer;
begin
  p := Pos(AnsiUpperCase(Name), AnsiUpperCase(Value));
  if p <> 0 then begin
    Inc(p, Length(Name) - 1);
    if p < Length(Value) then
      Inc(p);
    while (Byte(Value[p]) in [$30..$30+9, $20]) and (p <> Length(Value)) do
      Inc(p);
    Result := p = Length(Value);
  end
  else
    Result := False;
end;

procedure TMSConnectionEditorForm.edDatabaseDropDown(Sender: TObject);
var
  List: _TStringList;
  OldLoginPrompt: Boolean;
begin
  StartWait;
  try
    if Connection.Options.Provider = prCompact then begin
      List := _TStringList.Create;
      try
        GetDatabaseList(List);
        AssignStrings(List, edDatabase.Items);
        if edDatabase.Items.Count < 20 then
          edDatabase.DropDownCount := edDatabase.Items.Count
        else
          edDatabase.DropDownCount := 20;
      finally
        List.Free;
      end;
    end
    else begin
      edDatabase.Items.Clear;
      OldLoginPrompt := Connection.LoginPrompt;
      List := _TStringList.Create;
      try
        Connection.LoginPrompt := False;
        MSAccess.GetDatabasesList(Connection, List);
        List.Sort;
        edDatabase.Items.Assign(List);
        if edDatabase.Items.Count < 20 then
          edDatabase.DropDownCount := edDatabase.Items.Count
        else
          edDatabase.DropDownCount := 20;
      finally
        // edDatabase.Text := Connection.Database;
        List.Free;
        Connection.LoginPrompt := OldLoginPrompt;
      end;
    end;
  finally
    StopWait;
  end;
end;

procedure TMSConnectionEditorForm.rgAuthClick(Sender: TObject);
begin
  try
    Assert(Connection is TMSConnection);
    TMSConnection(Connection).Authentication := TMSAuthentication(rgAuth.ItemIndex);

    case TMSConnection(Connection).Authentication of
      auWindows:
      begin
        edUsername.Enabled := False;
        edPassword.Enabled := False;
        lbUsername.Enabled := False;
        lbPassword.Enabled := False;
        cbLoginPrompt.Enabled := False;
      end;
      auServer:
      begin
        edUsername.Enabled := True;
        edPassword.Enabled := True;
        lbUsername.Enabled := True;
        lbPassword.Enabled := True;
        cbLoginPrompt.Enabled := True;
      end;
    end;
  finally
    ShowState;
  end;
end;

procedure TMSConnectionEditorForm.btQueryAnalyzerClick(Sender: TObject);
begin
  SaveControlData;
{$IFDEF SDAC}
  RunServerTool(stQueryAnalyser, Connection);
{$ENDIF}
end;

procedure TMSConnectionEditorForm.btManagementStudioClick(Sender: TObject);
begin
  SaveControlData;
{$IFDEF SDAC}
  RunServerTool(stManagementStudio, Connection);
{$ENDIF}
end;

procedure TMSConnectionEditorForm.edDatabaseExit(Sender: TObject);
var
  OldConnected: boolean;
begin
  if FInDoInit then
    Exit;

  try
    OldConnected := Connection.Connected;
    try
      Connection.Disconnect; 
      Connection.Database := edDatabase.Text;
    finally
      Connection.Connected := OldConnected;
    end;
  finally
    ShowState;
  end;
end;

procedure TMSConnectionEditorForm.edDatabaseKeyUp(Sender: TObject;
  var Key: Word; Shift: TShiftState);
begin
  if Key ={$IFDEF MSWINDOWS}VK_RETURN{$ELSE}KEY_RETURN{$ENDIF} then
    edDatabaseExit(Sender);
end;

procedure TMSConnectionEditorForm.edDatabaseChange(Sender: TObject);
var
  Dialog: TOpenDialog;
  OldConnected: boolean;
begin
  if FInDoInit then
    Exit;
    
  if Connection.Options.Provider <> prCompact then
    Exit;

  try
    if edDatabase.Text = '<Browse...>' then begin
      Dialog := nil;
      try
        Dialog := TOpenDialog.Create(nil);
      {$IFDEF MSWINDOWS}
        Dialog.Filter := 'SQL Server Database Files (*.sdf)|*.sdf|All Files (*.*)|*.*';
      {$ELSE}
        Dialog.Filter := 'All Files (*)|*';
      {$ENDIF}
        Dialog.Options := Dialog.Options + [ofPathMustExist];
        if Dialog.Execute then begin
          Connection.Connected := False;
          FDataBaseText := Dialog.FileName;
        {$IFDEF MSWINDOWS}
          PostMessage(Handle, WM_SETDATABASETEXT, 0, 0);
        {$ENDIF}
        end
        else
          edDatabase.ItemIndex := FCurrItemIndex;
      finally
        Dialog.Free;
      end;
    end
    else
      FCurrItemIndex := edDatabase.Items.IndexOf(edDatabase.Text);
    OldConnected := Connection.Connected;
    try
      Connection.Connected := False;
      Connection.Database := edDatabase.Text;
    finally
      Connection.Connected := OldConnected;
    end;
  finally
    ShowState;
  end;
end;

procedure TMSConnectionEditorForm.WMSetDataBaseText(var Message: TMessage);
var
  OldConnected: boolean;
begin
  edDatabase.SetFocus;
  edDatabase.Text := FDataBaseText;
  OldConnected := Connection.Connected;
  try
    Connection.Connected := False;
    Connection.Database := edDatabase.Text;
  finally
    Connection.Connected := OldConnected;
  end;
  edDatabase.SelectAll;
end;

procedure TMSConnectionEditorForm.FormShow(Sender: TObject);
begin
  inherited;
  TDBAccessUtils.SetLockLoginPrompt(Connection, True);
end;

procedure TMSConnectionEditorForm.FormCloseQuery(Sender: TObject;
  var CanClose: Boolean);
begin
  inherited;
  if CanClose then
    TDBAccessUtils.SetLockLoginPrompt(Connection, False);
end;

end.
