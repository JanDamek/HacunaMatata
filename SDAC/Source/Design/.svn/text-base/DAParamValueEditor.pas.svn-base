
//////////////////////////////////////////////////
//  Data Access Components
//  Copyright © 1998-2012 Devart. All right reserved.
//////////////////////////////////////////////////

{$IFNDEF CLR}

{$I Dac.inc}

unit DAParamValueEditor;
{$ENDIF}

interface
uses
{$IFDEF MSWINDOWS}
  Windows, Messages,
{$ENDIF}
  SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, Buttons, ComCtrls, ExtCtrls,
{$IFDEF FPC}
  LResources,
{$ENDIF}
  DB, CREditor;

type
  TDAParamValueEditor = class(TCREditorForm)
    Memo: TMemo;
    //udIndex: TUpDown;

    procedure MemoChange(Sender: TObject);

  protected
    function GetValue: string;
    procedure SetValue(Value: string);
    
    function GetComponent: TComponent; override;
    procedure SetComponent(Value: TComponent); override;

  public
    property Value: string read GetValue write SetValue;
  end;

implementation

{$IFDEF VER6P}
uses
  Variants;
{$ENDIF}

{$IFNDEF FPC}
{$IFDEF IDE}
{$R *.dfm}
{$ELSE}
{$R DAParamValueEditor.dfm}
{$ENDIF}
{$ELSE}
{$R *.lfm}
{$ENDIF}

function TDAParamValueEditor.GetValue: string;
begin
  Result := Memo.Lines.Text;
end;

procedure TDAParamValueEditor.SetValue(Value: string);
begin
  Memo.Lines.Text := Value;
  Modified := False;
end;

procedure TDAParamValueEditor.MemoChange(Sender: TObject);
begin
  inherited;
  Modified := True;
end;

function TDAParamValueEditor.GetComponent: TComponent;
begin
  Result := nil;
end;

procedure TDAParamValueEditor.SetComponent(Value: TComponent);
begin
end; 

end.
