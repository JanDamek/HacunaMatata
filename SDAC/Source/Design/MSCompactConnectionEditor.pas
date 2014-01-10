//////////////////////////////////////////////////
//  SQL Server Data Access Components
//  Copyright © 1998-2012 Devart. All right reserved.
//  MSCompactConnection Editor
//////////////////////////////////////////////////

{$IFNDEF CLR}

{$I Sdac.inc}

unit MSCompactConnectionEditor;
{$ENDIF}

interface

uses
{$IFDEF FPC}
  LResources,
{$ENDIF}
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms,
  Dialogs, MSConnectionEditor, StdCtrls, ExtCtrls, ComCtrls, Buttons;

type
  TMSCompactConnectionEditorForm = class(TMSConnectionEditorForm)
  end;

var
  MSCompactConnectionEditorForm: TMSCompactConnectionEditorForm;

implementation

{$IFNDEF FPC}
{$IFDEF IDE}
{$R *.dfm}
{$ELSE}
{$R MSCompactConnectionEditor.dfm}
{$ENDIF}
{$ELSE}
{$R *.lfm}
{$ENDIF}

end.
