unit FMForm;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes,
  System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Dialogs, FMX.Edit, FMX.ExtCtrls,
  FMX.Layouts, FMX.Memo, ddPlugin_TLB, FMX.Ani, FMX.Menus, FMX.Types3D,
  FMX.Layers3D;

type
  TFMFrm = class(TForm)
    StyleBook1: TStyleBook;
    bOK: TButton;
    bCancel: TButton;
    ScaledLayout1: TScrollBox;
    Layout1: TLayout;
    Layout2: TLayout;
    MenuItem1: TMenuItem;
    FloatAnimation1: TFloatAnimation;
    GradientAnimation2: TGradientAnimation;
    GradientAnimation3: TGradientAnimation;
    FloatAnimation2: TFloatAnimation;
    procedure bOKClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure MenuItem1Click(Sender: TObject);
    procedure FloatAnimation1Process(Sender: TObject);
  private
    { Private declarations }
    fqData: IHeQuery;
    fqDefEdit: IHeQuery;
    fqDefEditAtr: IHeQuery;
    fqDefEditMeta: IHeQuery;
    fSetEditMode: Boolean;
    fMouseDown: Boolean;
    fLastX, fLastY: Single;
    fMemStream: TMemoryStream;

    procedure CreateFields;
    procedure LoadFromBinData(fBinData: String);
    function SaveToBinData: String;
    procedure ComMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Single);
    procedure ComMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Single);
    procedure ComMouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Single);

    procedure SaveToStreamComp(Sender: TStyledControl; var fBinData: String);
    procedure LoadFromStreamComp(Sender: TStyledControl; var fBinData: String);
  public
    { Public declarations }
    fHelios: IHelios;
    procedure OpenQuerys;
  end;

var
  FMFrm: TFMFrm;

implementation

{$R *.fmx}

function ToComponentName(v: Variant): String;
var
  s: String;
begin
  s := VarToStr(v);
  result := StringReplace(s, '.', '_', [rfReplaceAll]);
end;

procedure TFMFrm.bOKClick(Sender: TObject);
begin
  Self.Close;
end;

procedure TFMFrm.FloatAnimation1Process(Sender: TObject);
begin
  Layout2.Position.X := (Layout2.Width/2) - ((Layout2.Width/2)*Layout2.Scale.X);
end;

procedure TFMFrm.FormCreate(Sender: TObject);
begin
  fqData := nil;
  fqDefEdit := nil;
  fqDefEditAtr := nil;
  fqDefEditMeta := nil;
  fSetEditMode := false;
  fMemStream := TMemoryStream.Create;
end;

procedure TFMFrm.LoadFromBinData(fBinData: String);
var
  i: Integer;
begin
  // natazeni ulozeneho editoru, uzivatelske zmeny komponent
  for i := 0 to ComponentCount - 1 do
    if Components[i] is TStyledControl then
      LoadFromStreamComp(TStyledControl(Components[i]), fBinData);

end;

procedure TFMFrm.LoadFromStreamComp(Sender: TStyledControl;
  var fBinData: String);
var
  fStr: TStringList;
  i: integer;
  s:String;
begin
  fStr := TStringList.Create;
  fStr.Duplicates:=TDuplicates.dupIgnore;
  fStr.Text := fBinData;
  for i := 0 to fStr.Count-1 do
    begin
      s:=fStr[i];

      if Pos(Sender.Name,s)>0 then
      begin
        S:=StringReplace(s,Sender.Name+'|','',[]);
        if Pos('X=',s)>0 then
          Sender.Position.X := StrToFloat(StringReplace(s,'X=','',[rfReplaceAll]));
        if Pos('Y=',s)>0 then
          Sender.Position.Y := StrToFloat(StringReplace(s,'Y=','',[rfReplaceAll]));
      end;
    end;

  fStr.Free;
end;

function NQuotedStr(s: string): string; inline;
begin
  result := 'N' + QuotedStr(s);
end;

procedure TFMFrm.MenuItem1Click(Sender: TObject);
var
  fBinData: String;
  sql: String;
begin
  if fSetEditMode then
  begin
    fSetEditMode := false;
    MenuItem1.Text := 'Uživatelska definice';

    fBinData := SaveToBinData;
    sql := 'UPDATE TabDefEdit SET BinData=' + NQuotedStr(fBinData) +
      ' WHERE IDBrowse=' + IntToStr(fHelios.BrowseID);
    fHelios.ExecSQL(sql);

    Caption := VarToStr(fqDefEdit.FieldByNameValues('Popis'));
    FloatAnimation1.Start;
  end
  else
  begin
    fSetEditMode := true;
    MenuItem1.Text := 'Uložit nastavení';

    Caption := VarToStr(fqDefEdit.FieldByNameValues('Popis'))+' - editace editoru';
    FloatAnimation1.Start;
  end;
end;

procedure TFMFrm.OpenQuerys;
var
  fBinData: String;
begin
  if not Assigned(fqData) then
    fqData := fHelios.OpenSQL('SELECT * FROM TabUkoly WHERE ID=' +
      VarToStr(fHelios.CurrentRecordID));

  if not Assigned(fqDefEdit) then
    fqDefEdit := fHelios.OpenSQL('SELECT * FROM TabDefEdit WHERE IdBrowse=' +
      IntToStr(fHelios.BrowseID));

  if not Assigned(fqDefEditAtr) and (VarToStr(fqDefEdit.FieldByNameValues('id'))
    <> '') then
    fqDefEditAtr := fHelios.OpenSQL
      ('SELECT * FROM TabDefEditAtr WHERE IdDefEdit=' +
      VarToStr(fqDefEdit.FieldByNameValues('id')));

  if not Assigned(fqDefEditMeta) and
    (VarToStr(fqDefEdit.FieldByNameValues('id')) <> '') then
    fqDefEditMeta := fHelios.OpenSQL
      ('SELECT * FROM TabDefEditMeta WHERE IdDefEdit=' +
      VarToStr(fqDefEdit.FieldByNameValues('id')));

  if fqDefEdit.RecordCount = 1 then
  begin
    if fqDefEdit.FieldByName('Width').Value <> 0 then
      Width := fqDefEdit.FieldByName('Width').Value;

    if fqDefEdit.FieldByName('Height').Value <> 0 then
      Height := fqDefEdit.FieldByName('Height').Value;
  end;

  Caption := VarToStr(fqDefEdit.FieldByNameValues('Popis'));

  if (fqDefEdit.RecordCount = 1) and (fqDefEditAtr.RecordCount > 0) then
    CreateFields;

  fBinData := VarToStr(fqDefEdit.FieldByNameValues('BinData'));
  if fBinData <> '' then
    LoadFromBinData(fBinData);

end;

function TFMFrm.SaveToBinData: String;
var
  fBinData: String;
  i: Integer;
begin
  fBinData := '';
  for i := 0 to ComponentCount - 1 do
    if Components[i] is TStyledControl then
      SaveToStreamComp(TStyledControl(Components[i]), fBinData);

  result := fBinData;
end;

procedure TFMFrm.SaveToStreamComp(Sender: TStyledControl; var fBinData: String);
var
  fStr: TStringList;
  s: String;
begin
  fStr := TStringList.Create;
  fStr.Duplicates := TDuplicates.dupIgnore;

  fStr.Text := fBinData;
  s := Sender.Name + '|X=' + FloatToStr(Sender.Position.X);
  fStr.Add(s);
  s := Sender.Name + '|Y=' + FloatToStr(Sender.Position.Y);
  fStr.Add(s);

  s := Sender.Name + '|Width=' + FloatToStr(Sender.Width);
  fStr.Add(s);
  s := Sender.Name + '|Height=' + FloatToStr(Sender.Height);
  fStr.Add(s);

  fBinData := fStr.Text;
  FreeAndNil(fStr);
end;

procedure TFMFrm.ComMouseUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Single);
begin
  if fSetEditMode then
    fMouseDown := false;
end;

procedure TFMFrm.ComMouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Single);
begin
  if fSetEditMode and not fMouseDown then
  begin
    fMouseDown := true;
    fLastX := X;
    fLastY := Y;
  end;
end;

procedure TFMFrm.ComMouseMove(Sender: TObject; Shift: TShiftState;
  X, Y: Single);
var
  fLabel: TLabel;
begin
  if fSetEditMode and fMouseDown and (Sender is TClearingEdit) then
  begin
    TStyledControl(Sender).Position.X := TStyledControl(Sender).Position.X +
      ((X - fLastX) * TStyledControl(Sender).Scale.X);
    TStyledControl(Sender).Position.Y := TStyledControl(Sender).Position.Y +
      ((Y - fLastY) * TStyledControl(Sender).Scale.Y);

    fLabel := TLabel(FindComponent(TStyledControl(Sender).BindingName));

    if Assigned(fLabel) then
    begin
      fLabel.Position.X := fLabel.Position.X + ((X - fLastX) * fLabel.Scale.X);
      fLabel.Position.Y := fLabel.Position.Y + ((Y - fLastY) * fLabel.Scale.Y);
    end;
  end;
end;

procedure TFMFrm.CreateFields;
var
  i: Integer;
  apX, apY: Integer;
  fFMXLabel: TLabel;
  fFMXObject: TClearingEdit;
begin
  apX := 10;
  apY := 27;

  fqDefEditAtr.First;
  for i := 1 to fqDefEditAtr.RecordCount do
  begin
    fFMXLabel := TLabel.Create(Self);
    fFMXLabel.Parent := ScaledLayout1;
    fFMXLabel.Width := 97;
    fFMXLabel.Height := 22;
    fFMXLabel.Name := 'la' + ToComponentName
      (fqDefEditAtr.FieldByNameValues('NazevAtrSys'));
    fFMXLabel.Position.X := apX;
    fFMXLabel.Position.Y := apY - 17;
    fFMXLabel.OnMouseDown := ComMouseDown;
    fFMXLabel.OnMouseMove := ComMouseMove;
    fFMXLabel.OnMouseUp := ComMouseUp;

    fFMXLabel.Text :=
      VarToStr(fqDefEditAtr.FieldByNameValues('VolitelnyNazevAtrVer'));
    if fFMXLabel.Text = '' then
      fFMXLabel.Text := VarToStr(fqDefEditAtr.FieldByNameValues('NazevAtrSys'));

    fFMXObject := TClearingEdit.Create(Self);
    fFMXObject.Parent := ScaledLayout1;
    fFMXObject.Width := 97;
    fFMXObject.Height := 22;
    fFMXObject.Position.X := apX;
    fFMXObject.Position.Y := apY;
    fFMXObject.OnMouseDown := ComMouseDown;
    fFMXObject.OnMouseMove := ComMouseMove;
    fFMXObject.OnMouseUp := ComMouseUp;
    fFMXObject.BindingName := fFMXLabel.Name;

    apX := apX + round(fFMXObject.Width) + 10;
    if apX > Width then
    begin
      apX := 10;
      apY := apY + round(fFMXObject.Height) + 10;
    end;
    fFMXObject.Name := 'ed' + ToComponentName
      (fqDefEditAtr.FieldByNameValues('NazevAtrSys'));
    fFMXObject.Text :=
      VarToStr(fqData.FieldByNameValues
      (StringReplace(VarToStr(fqDefEditAtr.FieldByNameValues('NazevAtrSys')),
      'TabUkoly.', '', [rfReplaceAll])));

    fFMXObject.Visible := true;
    fFMXLabel.Visible := true;
    InsertComponent(fFMXObject);
    InsertComponent(fFMXLabel);

    fqDefEditAtr.Next;
  end;
end;

end.
