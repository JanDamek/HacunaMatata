
//////////////////////////////////////////////////
//  SQL Server Data Access Components
//  Copyright © 1998-2012 Devart. All right reserved.
//////////////////////////////////////////////////

{$IFNDEF CLR}
{$IFNDEF UNIDACPRO}

{$I Sdac.inc}

unit MSScriptProcessor;

{$ENDIF}
{$ENDIF}

interface

uses
  Classes, SysUtils, DB, DBAccess, DAScript, CRParser, CRAccess, CRTypes;

type
  TMSScriptProcessor = class(TDAScriptProcessor)
  private
    FCodes: array of Integer;
  protected
    function ExecuteNext: boolean; override;

    function GetParserClass: TSQLParserClass; override;
    procedure CheckLexem(Code: integer; var StatementType: integer; var Omit: boolean); override;
    function GetReady(Code: integer): boolean; override;
    function CanOptimize(const SQL: _string; const StatementType: integer): boolean; override;
  public
    constructor Create(Owner: TDAScript); override;
    destructor Destroy; override;       
  end;

implementation

uses
  DAConsts,
  {$IFNDEF UNIDACPRO}MSParser{$ELSE}MSParserUni{$ENDIF},
  {$IFNDEF UNIDACPRO}MSConsts{$ELSE}MSConstsUni{$ENDIF},
  {$IFNDEF UNIDACPRO}OLEDBAccess{$ELSE}OLEDBAccessUni{$ENDIF};

constructor TMSScriptProcessor.Create(Owner: TDAScript);
begin
  inherited Create(Owner);

  SetLength(FCodes, 3);
end;

destructor TMSScriptProcessor.Destroy;
begin
  SetLength(FCodes, 0);
  
  inherited;
end;

function TMSScriptProcessor.ExecuteNext: boolean;
var
  i: integer;
begin
  for i := Low(FCodes) to High(FCodes) do
    FCodes[i] := 0;
    
  Result := inherited ExecuteNext;
end;

function TMSScriptProcessor.GetParserClass: TSQLParserClass;
begin
  Result := TMSParser;
end;

procedure TMSScriptProcessor.CheckLexem(Code: integer; var StatementType: integer; var Omit: boolean);
var
  i: integer;
begin
  if (Code <> lcBlank) and (Code <> lcComment) then begin
    for i := Low(FCodes) to High(FCodes) - 1 do
      FCodes[i] := FCodes[i + 1];
    FCodes[2] := Code;

    if (FCurrDelimiter = ';') then
      if ((FCodes[2] = lxBEGIN) or (FCodes[2] = lxDECLARE))
        or (((FCodes[2] = lxPROCEDURE) or (FCodes[2] = lxFUNCTION) or (FCodes[2] = lxTRIGGER))
        and ((FCodes[1] = lxCREATE) and (FCodes[0] = 0) or
             (FCodes[1] = lxALTER) and (FCodes[0] = 0)))
      then
        StatementType := ST_SPECIFIC_SQL;
  end;
end;

function TMSScriptProcessor.GetReady(Code: integer): boolean;
begin
  Result := Code = lxGO;
end;

function TMSScriptProcessor.CanOptimize(const SQL: _string; const StatementType: integer): boolean;
begin
  Result := (StatementType <> ST_SPECIFIC_SQL) and inherited CanOptimize(SQL, StatementType);
end;

end.
