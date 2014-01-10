
//////////////////////////////////////////////////
//  SQL Server Data Access Components
//  Copyright © 1998-2012 Devart. All right reserved.
//  SQL Monitor
//////////////////////////////////////////////////

{$IFNDEF CLR}

{$I Sdac.inc}

unit MSSQLMonitor;
{$ENDIF}

interface

uses
  SysUtils, Classes, DB, DBAccess, DASQLMonitor, CRTypes;

type
  TTypeInfo = record
    Name: string;
    DataType: TFieldType;
  end;

const
  // Obsolete, needs to change to DB.FieldTypeNames
  ArrTypeInfoCount = 16 {$IFDEF VER5P}+ 2{$ENDIF}{$IFDEF VER10P} + 1{$ENDIF};
  ArrTypeInfo: array [0..ArrTypeInfoCount - 1] of TTypeInfo = (
    (Name: 'Unknown';  DataType: ftUnknown),
    (Name: 'String';   DataType: ftString),
    (Name: 'WideString';DataType: ftWideString),
    (Name: 'Smallint'; DataType: ftSmallint),
    (Name: 'Integer';  DataType: ftInteger),
    (Name: 'Word';     DataType: ftWord),
    (Name: 'LargeInt'; DataType: ftLargeInt),
    (Name: 'Boolean';  DataType: ftBoolean),
    (Name: 'Float';    DataType: ftFloat),
    (Name: 'Currency'; DataType: ftCurrency),
    (Name: 'BCD';      DataType: ftBCD),
    (Name: 'DateTime'; DataType: ftDateTime),
    (Name: 'Memo';     DataType: ftMemo),
  {$IFDEF VER10P}
    (Name: 'WideMemo'; DataType: ftWideMemo),
  {$ENDIF}
    (Name: 'Bytes';    DataType: ftBytes),
    (Name: 'VarBytes'; DataType: ftVarBytes),
    (Name: 'Blob';     DataType: ftBlob){$IFDEF VER5P},
    (Name: 'GUID';     DataType: ftGuid),
    (Name: 'Variant';  DataType: ftVariant){$ENDIF}
  );

function GetArrTypeInfoIdx(DataType: TFieldType): integer; overload;
function GetArrTypeInfoIdx(Name: string): integer; overload;

type

{ TMSSQLMonitor }

  TMSSQLMonitorClass = class of TMSSQLMonitor;

{$IFDEF VER16P}
  [ComponentPlatformsAttribute(pidWin32 or pidWin64)]
{$ENDIF}
  TMSSQLMonitor = class(TCustomDASQLMonitor)
  protected
    procedure InternalInfoMessage(Connection: TCustomDAConnection; const MessageText: _string);

    class function GetMonitor: TCustomDASQLMonitor; override;
    procedure SetMonitor; override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    class function GetParamDataType(Param: TDAParam): string; override;
    class function GetCaption: string; override;

    class procedure InfoMessage(Connection: TCustomDAConnection; const MessageText: _string);

  published
    property Active default True;
    property Options;
    property DBMonitorOptions;
    property OnSQL;
    property TraceFlags;
  end;

implementation

uses
  {$IFDEF FPC}MemDataSet{$ELSE}MemDS{$ENDIF}, 
  MSAccess, OLEDBAccess, DBMonitorClient;

var
  MSMonitor: TMSSQLMonitor;

function GetArrTypeInfoIdx(DataType: TFieldType): integer; 
var
  i: integer;
begin
  if DataType = ftFixedChar then
    DataType := ftString;

  Result := - 1;
  for i := 0 to ArrTypeInfoCount - 1 do
    if ArrTypeInfo[i].DataType = DataType then 
    begin
      Result := i;
      Break;
    end;
end;

function GetArrTypeInfoIdx(Name: string): integer; 
var
  i: integer;
begin
  Result := - 1;
  for i := 0 to ArrTypeInfoCount - 1 do
    if ArrTypeInfo[i].Name = Name then 
    begin
      Result := i;
      Break;
    end;
end;

{ TMSSQLMonitor }

class procedure TMSSQLMonitor.InfoMessage(Connection: TCustomDAConnection; const MessageText: _string);
begin
  if (GetMonitor <> nil) then
    TMSSQLMonitor(GetMonitor).InternalInfoMessage(Connection, MessageText);
end;

class function TMSSQLMonitor.GetMonitor: TCustomDASQLMonitor;
begin
  Result := MSMonitor;
end;

procedure TMSSQLMonitor.SetMonitor;
begin
  if MSMonitor = nil then
    MSMonitor := Self;
end;

class function TMSSQLMonitor.GetCaption: string;
begin
  Result := 'SDAC';
end;

procedure TMSSQLMonitor.InternalInfoMessage(Connection: TCustomDAConnection; const MessageText: _string);
var
  St: _string;
begin
  if Active and (tfMisc in TraceFlags) then begin
    St := 'Info Message: ' + MessageText + ': ' + Connection.Username + '@' + Connection.Server;

    InternalCustomMessage(Connection, St);
  end;
end;

class function TMSSQLMonitor.GetParamDataType(Param: TDAParam): string;
begin
  if Param.DataType = TFieldType(ftMSXML) then
    Result := 'XML'
  else
  if Param.DataType = TFieldType(ftMSUDT) then
    Result := 'UDT'
  else
{$IFNDEF VER10P}
  if Param.DataType = TFieldType(ftFixedWideChar) then
    Result := 'FixedWideChar'
  else
{$ENDIF}
{$IFDEF D8} // Copied from inherited to avoid D8 Internal linker error: ILLK8903
  begin
    Result := FieldTypeNames[Param.DataType];
    if Param.DataType in [ftString,ftFixedChar,ftWideString] then
      Result := Result + '[' + IntToStr(Length(Param.AsString)) + ']';
  end;
{$ELSE}
    Result := inherited GetParamDataType(Param);
{$ENDIF}
end;

constructor TMSSQLMonitor.Create(AOwner: TComponent);
begin
  inherited;
end;

destructor TMSSQLMonitor.Destroy;
begin
  if MSMonitor = Self then
    MSMonitor := nil;

  inherited;
end;

end.
 
