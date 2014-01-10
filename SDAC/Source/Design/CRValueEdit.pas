{$I Dac.inc}

unit CRValueEdit;

interface

uses
{$IFDEF MSWINDOWS}
  Windows,
{$ENDIF}
  Graphics, Forms, Controls, Dialogs, StdCtrls,
  {$IFDEF VER6P}{$IFNDEF FPC}ValEdit,{$ENDIF} Grids,{$ENDIF}
{$IFDEF FPC}
  PropEdits,
{$ELSE}
  {$IFDEF VER6P}DesignIntf, DesignEditors,{$ELSE}DsgnIntf,{$ENDIF}
{$ENDIF}
{$IFDEF FPC}
  LResources,
{$ENDIF}
  SysUtils, Classes;

{$IFDEF VER6P}
{$IFNDEF FPC}
{$DEFINE USE_VALEDIT}
{$ENDIF}
{$ENDIF}

type
  TValueEditDlg = class(TForm)
    OKButton: TButton;
    CancelButton: TButton;
    procedure FormCreate(Sender: TObject);
  protected
    FModified: Boolean;
    FValueEditor: {$IFDEF USE_VALEDIT}TValueListEditor{$ELSE}TMemo{$ENDIF};

    function GetLines: TStrings;
    procedure ValueEditorChange(Sender: TObject);
  end;

  TCRValueListProperty = class(TClassProperty)
  protected
    function EditDialog: TValueEditDlg; virtual;
    function GetStrings: TStrings; virtual;
    procedure SetStrings(const Value: TStrings); virtual;
  public
    function GetAttributes: TPropertyAttributes; override;
    procedure Edit; override;
  end;

implementation

{$IFNDEF FPC}
{$IFDEF IDE}
{$R *.dfm}
{$ELSE}
{$R CRValueEdit.dfm}
{$ENDIF}
{$ELSE}
{$R *.lfm}
{$ENDIF}

{ TValueEditDlg }

procedure TValueEditDlg.FormCreate(Sender: TObject);
begin
{$IFDEF USE_VALEDIT}
  FValueEditor := TValueListEditor.Create(Self);
  FValueEditor.KeyOptions := [keyEdit, keyAdd, keyDelete];
  FValueEditor.ColWidths[0] := 150;
  FValueEditor.ColWidths[1] := 256;
  FValueEditor.OnStringsChange := ValueEditorChange;
{$ELSE}
  FValueEditor := TMemo.Create(Self);
  FValueEditor.ScrollBars := ssBoth;
  FValueEditor.WordWrap := False;
  FValueEditor.OnChange := ValueEditorChange;
{$ENDIF}
  FValueEditor.Left := 8;
  FValueEditor.Top := 8;
  FValueEditor.Width := 412;
  FValueEditor.Height := 232;
  FValueEditor.TabOrder := 0;
  FValueEditor.Parent := Self;
end;

function TValueEditDlg.GetLines: TStrings;
begin
  Result := FValueEditor.{$IFDEF USE_VALEDIT}Strings{$ELSE}Lines{$ENDIF};
end;

procedure TValueEditDlg.ValueEditorChange(Sender: TObject);
begin
  if Sender = FValueEditor then FModified := True;
end;

{ TCRValueListProperty }

function TCRValueListProperty.EditDialog: TValueEditDlg;
begin
  Result := TValueEditDlg.Create(Application);
end;

function TCRValueListProperty.GetAttributes: TPropertyAttributes;
begin
  Result := inherited GetAttributes + [paDialog] - [paSubProperties];
end;

function TCRValueListProperty.GetStrings: TStrings;
begin
  Result := TStrings(GetOrdValue);
end;

procedure TCRValueListProperty.SetStrings(const Value: TStrings);
begin
  SetOrdValue(Longint(Value));
end;

procedure TCRValueListProperty.Edit;
begin
  with EditDialog do
  try
    GetLines.Assign(GetStrings);
    FModified := False;
    ActiveControl := FValueEditor;
    case ShowModal of
      mrOk: SetStrings(GetLines);
    end;
  finally
    Free;
  end;
end;

end.
