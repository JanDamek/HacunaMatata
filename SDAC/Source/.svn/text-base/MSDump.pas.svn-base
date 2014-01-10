//////////////////////////////////////////////////
//  SQL Server Data Access Components
//  Copyright © 1998-2012 Devart. All right reserved.
//////////////////////////////////////////////////

{$IFNDEF CLR}

{$I Sdac.inc}

unit MSDump;
{$ENDIF}

interface

uses
{$IFDEF VER6P}
  Variants,
{$ENDIF}
  SysUtils, Classes, DB,
  CRAccess, DBAccess, MSAccess, MSServices, DADump, DAScript, CRTypes;

type
  TMSDump = class;

  //TMSDumpObject = (doData);
  //TMSDumpObjects = set of TMSDumpObject;

  TMSDumpOptions = class(TDADumpOptions)
  private
    FOwner: TMSDump;
    FIdentityInsert: boolean;
    FDisableConstraints: boolean;

    procedure SetIdentityInsert(Value: boolean);
    procedure SetDisableConstraints(Value: boolean);

  protected
    procedure AssignTo(Dest: TPersistent); override;

  public
    constructor Create(Owner: TMSDump);

  published
    property IdentityInsert: boolean read FIdentityInsert write SetIdentityInsert default False;
    property DisableConstraints: boolean read FDisableConstraints write SetDisableConstraints default False;
  end;

{$IFDEF VER16P}
  [ComponentPlatformsAttribute(pidWin32 or pidWin64)]
{$ENDIF}
  TMSDump = class(TDADump)
  protected
    //FObjects: TMSDumpObjects;

    procedure AssignTo(Dest: TPersistent); override;

    function GetProcessorClass: TDADumpProcessorClass; override;
    procedure SetProcessor(Value: TDADumpProcessor); override;

    function GetConnection: TMSConnection;
    procedure SetConnection(Value: TMSConnection);

    function GetTableNames: _string; override;
    procedure SetTableNames(Value: _string); override;

    function CreateOptions: TDADumpOptions; override;
    function CreateScript: TDAScript; override;

    function GetOptions: TMSDumpOptions;
    procedure SetOptions(Value: TMSDumpOptions);

    function GenerateHeader: _string; override;

    property Processor;

  public
    constructor Create(Owner: TComponent); override;
  published
    property Connection: TMSConnection read GetConnection write SetConnection;

    //property Objects: TMSDumpObjects read FObjects write FObjects default [doData];
    property Options: TMSDumpOptions read GetOptions write SetOptions;
  end;

implementation

uses
  CRFunctions, MemData, DAConsts, DALoader, OLEDBAccess, MSScript;

{ TMSDumpOptions }

constructor TMSDumpOptions.Create(Owner: TMSDump);
begin
  inherited Create(Owner);

  FOwner := Owner;
  FIdentityInsert := False;
end;

procedure TMSDumpOptions.AssignTo(Dest: TPersistent);
begin
  inherited;

  if Dest is TMSDumpOptions then begin
    TMSDumpOptions(Dest).IdentityInsert := IdentityInsert;
    TMSDumpOptions(Dest).DisableConstraints := DisableConstraints;
  end;
end;

procedure TMSDumpOptions.SetIdentityInsert(Value: boolean);
begin
  if Value <> FIdentityInsert then begin
    FIdentityInsert := Value;
    if FOwner.Processor <> nil then
      FOwner.Processor.SetProp(prIdentityInsert, Value);
  end;
end;

procedure TMSDumpOptions.SetDisableConstraints(Value: boolean);
begin
  if Value <> FDisableConstraints then begin
    FDisableConstraints := Value;
    if FOwner.Processor <> nil then
      FOwner.Processor.SetProp(prDisableConstraints, Value);
  end;
end;

{ TMSDump }

constructor TMSDump.Create(Owner: TComponent);
begin
  inherited Create(Owner);

  //FObjects := [doData];
end;

procedure TMSDump.AssignTo(Dest: TPersistent);
begin
  inherited;

  if Dest is TMSDump then begin
    //TMSDump(Dest).Objects := Objects;
  end;
end;

function TMSDump.GetProcessorClass: TDADumpProcessorClass;
begin
  Result := TCustomMSDumpProcessor;
end;

procedure TMSDump.SetProcessor(Value: TDADumpProcessor);
begin
  inherited;

  if FProcessor <> nil then begin
    FProcessor.SetProp(prIdentityInsert, Options.IdentityInsert);
    FProcessor.SetProp(prDisableConstraints, Options.DisableConstraints);
  end;
end;

function TMSDump.GenerateHeader: _string;
begin
  Result := _Format(SBHCaption, ['Sdac', SDACVersion,
  'SQL Server', Connection.ServerVersion, 'SQL Server', Connection.ClientVersion, DateTimeToStr(Now),
    Connection.Server, Connection.Database]);
end;

function TMSDump.GetConnection: TMSConnection;
begin
  Result := TMSConnection(inherited Connection);
end;

procedure TMSDump.SetConnection(Value: TMSConnection);
begin
  inherited Connection := Value;
end;

function TMSDump.GetTableNames: _string;
begin
  Result := OLEDBSQLInfo.NamesFromList(FTables);
end;

procedure TMSDump.SetTableNames(Value: _string);
begin
  OLEDBSQLInfo.NamesToList(Value, FTables);
end;

function TMSDump.CreateOptions: TDADumpOptions;
begin
  Result := TMSDumpOptions.Create(Self);
end;

function TMSDump.CreateScript: TDAScript;
begin
  Result := TMSScript.Create(nil);
end;

function TMSDump.GetOptions: TMSDumpOptions;
begin
  Result := TMSDumpOptions(inherited Options);
end;

procedure TMSDump.SetOptions(Value: TMSDumpOptions);
begin
  inherited Options := Value;
end;

end.
