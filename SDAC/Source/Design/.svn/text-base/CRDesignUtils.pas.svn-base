
//////////////////////////////////////////////////
//  DB Access Components
//  Copyright © 1998-2012 Devart. All right reserved.
//////////////////////////////////////////////////

{$IFNDEF CLR}

{$I Dac.inc}

unit CRDesignUtils;
{$ENDIF}

interface

uses
  Classes, SysUtils;

type
  TCRDesignUtils = class
  public
    class function GetProjectName: string; virtual; // Returns ProjectName = ('DataEditor', 'ODAC', 'SDAC', 'MyDAC', ...)

  { Component }
    class function GetDesignCreate(Obj: TComponent): boolean; virtual;
    class procedure SetDesignCreate(Obj: TComponent; Value: boolean); virtual;

  {$IFDEF USE_SYNEDIT}
    class function SQLDialect: integer ; virtual; // SynHighlighterSQL TSQLDialect = (sqlStandard, sqlInterbase6, sqlMSSQL7, sqlMySQL, sqlOracle, sqlSybase, sqlIngres, sqlMSSQL2K);
  {$ENDIF}

    class function DBToolsAvailable: boolean; virtual;
  end;

  TCRDesignUtilsClass = class of TCRDesignUtils;

implementation

{ TCRDesignUtils }

class function TCRDesignUtils.GetProjectName: string;
begin
 Result := 'DAC';
end;

class function TCRDesignUtils.GetDesignCreate(Obj: TComponent): boolean;
begin
  Result := False;
  Assert(Obj <> nil);
  Assert(False, Obj.ClassName);
end;

class procedure TCRDesignUtils.SetDesignCreate(Obj: TComponent; Value: boolean);
begin
  Assert(Obj <> nil);
  Assert(False, Obj.ClassName);
end;

{$IFDEF USE_SYNEDIT}
class function TCRDesignUtils.SQLDialect: integer;
begin
  Result := 0; // sqlStandard
end;
{$ENDIF}

class function TCRDesignUtils.DBToolsAvailable: boolean;
begin
  Result := False;
end;

end.
