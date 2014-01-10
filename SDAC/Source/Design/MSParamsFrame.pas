
//////////////////////////////////////////////////
//  SQL Server Access Components
//  Copyright © 1998-2012 Devart. All right reserved.
//  SDAC Params Frame
//////////////////////////////////////////////////

{$IFNDEF CLR}

{$I Sdac.inc}

unit MSParamsFrame;
{$ENDIF}

interface

uses
{$IFDEF FPC}
  LResources,
{$ENDIF}
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, ExtCtrls, DAParamsFrame, MSAccess;

type
  TMSParamsFrame = class(TDAParamsFrame)
  public
    constructor Create(AOwner: TComponent); override;
  end;

implementation

{$IFNDEF FPC}
{$IFDEF IDE}
{$R *.dfm}
{$ELSE}
{$R MSParamsFrame.dfm}
{$ENDIF}
{$ELSE}
{$R *.lfm}
{$ENDIF}

uses
  DB;

{ TMSParamsFrame }

constructor TMSParamsFrame.Create(AOwner: TComponent);
begin
  inherited;
  
  AddDataType('Unknown',    ftUnknown,    True,  False, False, '');
  AddDataType('String',     ftString,     False, True,  True,  '');
  AddDataType('WideString', ftWideString, False, True,  True,  '');
  AddDataType('Smallint',   ftSmallint,   True,  True,  False, '0');
  AddDataType('Integer',    ftInteger,    True,  True,  False, '0');
  AddDataType('Word',       ftWord,       True,  True,  False, '0');
  AddDataType('LargeInt',   ftLargeInt,   True,  True,  False, '0');
  AddDataType('Boolean',    ftBoolean,    False, True,  True,  'False');
  AddDataType('Float',      ftFloat,      True,  True,  False, '0');
  AddDataType('Currency',   ftCurrency,   True,  True,  False, '0');
  AddDataType('BCD',        ftBCD,        True,  True,  False, '0');
{$IFDEF VER6P}
  AddDataType('FMTBcd',     ftFMTBcd,     True,  True,  False, '0');
{$ENDIF}
  AddDataType('DateTime',   ftDateTime,   True,  True,  False, '');
  AddDataType('Memo',       ftMemo,       False, True,  False, '');
{$IFDEF VER10P}
  AddDataType('WideMemo',   ftWideMemo,   False, True,  False, '');
{$ENDIF}
  AddDataType('Bytes',      ftBytes,      False, False, True,  '');
  AddDataType('VarBytes',   ftVarBytes,   False, False, True,  '');
  AddDataType('Blob',       ftBlob,       False, False, False, '');
  AddDataType('GUID',       ftGuid,       False, False, False, '');
  AddDataType('Variant',    ftVariant,       False, False, False, '');
  AddDataType('XML',        TFieldType(ftMSXML), False, True, False, '');

  AddParamType('IN', ptInput);
  AddParamType('IN/OUT', ptInputOutput);
  AddParamType('RESULT', ptResult);
end;

end.
