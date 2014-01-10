
//////////////////////////////////////////////////
//  SQL Server Data Access Components
//  Copyright © 1998-2012 Devart. All right reserved.
//  MS Parser
//////////////////////////////////////////////////

{$IFNDEF CLR}
{$IFNDEF UNIDACPRO}

{$I Sdac.inc}

unit MSParser;
{$ENDIF}
{$ENDIF}

interface
uses
  CRParser, CRTypes;

const
  lxMSFirst  = 1000;
  lxALTER    = lxMSFirst;
  lxCOMPUTE  = lxALTER + 1;
  lxCREATE   = lxCOMPUTE + 1;
  lxCROSS    = lxCREATE + 1;
  lxDECLARE  = lxCROSS + 1;
  lxDEFAULT  = lxDECLARE + 1;
  lxEND      = lxDEFAULT + 1;
  lxFUNCTION = lxEND + 1;
  lxGO       = lxFUNCTION + 1;
  lxOPTION   = lxGO + 1;
  lxPROCEDURE = lxOPTION + 1;
  lxREPLACE  = lxPROCEDURE + 1;
  lxRULE     = lxREPLACE + 1;
  lxTABLE    = lxRULE + 1;
  lxTOP      = lxTABLE + 1;
  lxTRIGGER  = lxTOP + 1;
  lxVIEW     = lxTRIGGER + 1;

type
  TMSParser = class (TSQLParser)
  protected
    function IsIdentQuote(Ch: _char): boolean; override;
    procedure ToRightQuote(LeftQuote: _char); override; // Search right quote of quoted string value or quoted identifier

    procedure InitParser; override;
  end;

implementation

uses
  Classes, SysUtils;

var
  MSKeywordLexems: _TStringList;

{ TMSParser }

procedure TMSParser.InitParser;
begin
  inherited;
(*
SELECT statement ::=
    < query_expression >
    [ ORDER BY { order_by_expression | column_position [ ASC | DESC ] }
        [ ,...n ]    ]
    [ COMPUTE
        { { AVG | COUNT | MAX | MIN | SUM } ( expression ) } [ ,...n ]
        [ BY expression [ ,...n ] ]
    ]
    [ FOR { BROWSE | XML { RAW | AUTO | EXPLICIT }
            [ , XMLDATA ]
            [ , ELEMENTS ]
            [ , BINARY base64 ]
        }
]
    [ OPTION ( < query_hint > [ ,...n ]) ]

< query expression > ::=
    { < query specification > | ( < query expression > ) }
    [ UNION [ ALL ] < query specification | ( < query expression > ) [...n ] ]

< query specification > ::=
    SELECT [ ALL | DISTINCT ]
        [ { TOP integer | TOP integer PERCENT } [ WITH TIES ] ]
        < select_list >
    [ INTO new_table ]
    [ FROM { < table_source > } [ ,...n ] ]
    [ WHERE < search_condition > ]
    [ GROUP BY [ ALL ] group_by_expression [ ,...n ]
        [ WITH { CUBE | ROLLUP } ]
    ]
    [ HAVING < search_condition > ]

*)

  SetLength(FClauses, 7);
  FClauses[0] := lxWHERE;
  FClauses[1] := lxGROUP;
  FClauses[2] := lxHAVING;
  FClauses[3] := lxORDER;
  FClauses[4] := lxCOMPUTE;
  FClauses[5] := lxFOR;
  FClauses[6] := lxOPTION;

  FKeywordLexems := MSKeywordLexems;

  CommentBegin := '/*';
  CommentEnd := '*/';

  DecSeparator := '.'; // To avoid 'INSERT INTO t(f1, f2) VALUES(:1,2)'
end;

function TMSParser.IsIdentQuote(Ch: _char): boolean;
begin
  case Ch of
    '"', '[', ']':
      Result := True;
    else
      Result := False;
  end;
end;

procedure TMSParser.ToRightQuote(LeftQuote: _char);
begin
  if LeftQuote = '[' then
    inherited ToRightQuote(']')
  else
    inherited;
end;

initialization
  MSKeywordLexems := _TStringList.Create;
  MSKeywordLexems.Assign(SQLKeywordLexems);

  MSKeywordLexems.AddObject('PROCEDURE', TObject(Integer(lxPROCEDURE)));
  MSKeywordLexems.AddObject('CREATE', TObject(Integer(lxCREATE)));
  MSKeywordLexems.AddObject('REPLACE', TObject(Integer(lxREPLACE)));
  MSKeywordLexems.AddObject('COMPUTE', TObject(Integer(lxCOMPUTE)));
  MSKeywordLexems.AddObject('CROSS', TObject(Integer(lxCROSS)));
  MSKeywordLexems.AddObject('OPTION', TObject(Integer(lxOPTION)));
  MSKeywordLexems.AddObject('END', TObject(Integer(lxEND)));
  MSKeywordLexems.AddObject('DECLARE', TObject(Integer(lxDECLARE)));
  MSKeywordLexems.AddObject('FUNCTION', TObject(Integer(lxFUNCTION)));
  MSKeywordLexems.AddObject('GO', TObject(Integer(lxGO)));
  MSKeywordLexems.AddObject('TABLE', TObject(Integer(lxTABLE)));
  MSKeywordLexems.AddObject('DEFAULT', TObject(Integer(lxDEFAULT)));
  MSKeywordLexems.AddObject('RULE', TObject(Integer(lxRULE)));
  MSKeywordLexems.AddObject('VIEW', TObject(Integer(lxVIEW)));
  MSKeywordLexems.AddObject('TOP', TObject(Integer(lxTOP)));

  MSKeywordLexems.CustomSort(CRCmpStrings);

finalization
  MSKeywordLexems.Free;

end.
