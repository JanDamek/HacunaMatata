type

{ TMSConnectDialog }

{$IFDEF VER16P}
  [ComponentPlatformsAttribute(pidWin32 or pidWin64)]
{$ENDIF}
  {$IFDEF FMX}TMSConnectDialogFmx{$ELSE}TMSConnectDialog{$ENDIF} = class(TCustomConnectDialog)
  private
    FCachedServerList: TStrings;
    function GetConnection: TCustomMSConnection;
  protected
    FDatabaseLabel: string;
    function DefDialogClass: TClass; override;
    function GetKeyPath: string; override;
    function GetServerStoreName: string; override;

  {$IFDEF MSWINDOWS}
    procedure SaveInfoToRegistry(Registry: TRegistry); override;
    procedure LoadInfoFromRegistry(Registry: TRegistry); override;
  {$ENDIF}
  public
    constructor Create(Owner: TComponent); override;
    property Connection: TCustomMSConnection read GetConnection;
    procedure GetServerList(List: _TStrings); override;
    destructor Destroy; override;

  published
    property Retries;
    property SavePassword;
    property DialogClass;

    property Caption;
    property UsernameLabel;
    property PasswordLabel;
    property ServerLabel;
    property DatabaseLabel: string read FDatabaseLabel write FDatabaseLabel;
    property ConnectButton;
    property CancelButton;

    property LabelSet;
    property StoreLogInfo;
  end;

implementation

uses
  {$IFDEF VER16P}{$IFDEF FMX}FMX.Types,{$ELSE}Controls,{$ENDIF}{$ENDIF}
  DAConsts;

function DefConnectDialogClass: TClass;
begin
  Result := {$IFDEF FMX}TMSConnectDialogFmx{$ELSE}TMSConnectDialog{$ENDIF};
end;

const
  KeyPath = '\SOFTWARE\Devart\SDAC\';

{ TMSConnectDialog }

function {$IFDEF FMX}TMSConnectDialogFmx{$ELSE}TMSConnectDialog{$ENDIF}.DefDialogClass: TClass; 
begin
  Result := TMSConnectForm;
end;
    
function {$IFDEF FMX}TMSConnectDialogFmx{$ELSE}TMSConnectDialog{$ENDIF}.GetKeyPath: string;
begin        
  Result := KeyPath;
end;

constructor {$IFDEF FMX}TMSConnectDialogFmx{$ELSE}TMSConnectDialog{$ENDIF}.Create(Owner: TComponent);
begin
  inherited;
  FDatabaseLabel := 'Database';
  FCachedServerList := TStringList.Create;
end;

destructor {$IFDEF FMX}TMSConnectDialogFmx{$ELSE}TMSConnectDialog{$ENDIF}.Destroy;
begin
  FCachedServerList.Free;
  inherited;
end;

function {$IFDEF FMX}TMSConnectDialogFmx{$ELSE}TMSConnectDialog{$ENDIF}.GetServerStoreName: string;
begin
  Result := 'Server';
end;

procedure {$IFDEF FMX}TMSConnectDialogFmx{$ELSE}TMSConnectDialog{$ENDIF}.SaveInfoToRegistry(Registry: TRegistry);
begin
  inherited;

  Registry.WriteString('Database', Connection.Database);
end;

procedure {$IFDEF FMX}TMSConnectDialogFmx{$ELSE}TMSConnectDialog{$ENDIF}.LoadInfoFromRegistry(Registry: TRegistry);
begin
  inherited;
  if Registry.ValueExists('Database') then
    TCustomMSConnection(Connection).Database := Registry.ReadString('Database');
end;

procedure {$IFDEF FMX}TMSConnectDialogFmx{$ELSE}TMSConnectDialog{$ENDIF}.GetServerList(List: _TStrings);
begin
  if FCachedServerList.Count = 0 then begin
    MSAccess.GetServerList(List);
    FCachedServerList.Assign(List);
  end else
    List.Assign(FCachedServerList);
end;

function {$IFDEF FMX}TMSConnectDialogFmx{$ELSE}TMSConnectDialog{$ENDIF}.GetConnection: TCustomMSConnection;
begin
  Result := (inherited Connection) as TCustomMSConnection;
end;

initialization
{$IFDEF FMX}
  if not Assigned(DefConnectDialogClassProc) then
{$ENDIF}
    DefConnectDialogClassProc := DefConnectDialogClass;
{$IFDEF VER16P}
{$IFDEF FMX}
  GroupDescendentsWith(TMSConnectDialogFmx, TFmxObject);
{$ELSE}
  GroupDescendentsWith(TMSConnectDialog, TControl);
{$ENDIF}
{$ENDIF}

end.
