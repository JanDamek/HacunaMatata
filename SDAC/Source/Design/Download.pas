{$IFNDEF CLR}

{$I Dac.inc}

unit Download;
{$ENDIF}
interface

uses
  Windows, SysUtils, Classes, Graphics, Forms, Controls, StdCtrls,
  Buttons, ExtCtrls, Registry;

type
  TDownloadForm = class(TForm)
    OKBtn: TButton;
    CancelBtn: TButton;
    Bevel1: TBevel;
    chkDontAsk: TCheckBox;
    Image: TImage;
    lblQuestion: TLabel;
    btHelp: TButton;
    lblText1: TLabel;
    lblText3: TLabel;
    lblProduct: TLabel;
    lblText2: TLabel;
    lblText3Wrap: TLabel;
    procedure FormShow(Sender: TObject);
    procedure OKBtnClick(Sender: TObject);
    procedure btHelpClick(Sender: TObject);
    procedure FormMouseMove(Sender: TObject; Shift: TShiftState; X,
      Y: Integer);
    procedure lblProductMouseMove(Sender: TObject; Shift: TShiftState; X,
      Y: Integer);
    procedure lblProductClick(Sender: TObject);
  private
    FProVersionNotice: boolean;
  public
    constructor Create(AOwner: TComponent; ProVersionNotice: boolean); reintroduce; overload;
  end;

procedure SetToolsCheckingParams(const ADialogCaptionStr, AToolsNameStr,
  AProjectVersionStr, AAskIncompatibleStr, AAskNoAddinStr, ARegKeyStr,
  AHelpProjectStr, AHelpTopicStr, AUrlStr, AUrlExeStr, AAtomName: string;
  const ADeveloperProjectNameStr: string = '');
function NoCheckForTools(Incompatible: boolean; ARegIniFile: TRegIniFile = nil): boolean;
procedure CheckForTools(Incompatible: boolean);
function DownloadTools(ProVersion: boolean): boolean;

implementation

uses
{$IFDEF FPC}
  LResources,
{$ENDIF}
  ShellApi, DacVcl, HelpUtils;

{$IFNDEF FPC}
{$IFDEF IDE}
{$R *.dfm}
{$ELSE}
{$R Download.dfm}
{$ENDIF}
{$ELSE}
{$R *.lfm}
{$ENDIF}

resourcestring
  sToolsWrongVersion1 = 'Current version of ';
  sToolsWrongVersion2 = ' is incompatible with';
  sNoTools1 = 'You don''t have ';
  sNoTools2 = ' installed.';
  sToolsDownloadQuestion = 'Do you wish to download the latest %s version?';

  sDeveloperEdition1 = 'If you have ';
  sDeveloperEdition2 = ', you can';
  sDeveloperEdition3 = 'download full version of %s. ' +
    'Please consult your order confirmation email for more information.';
  sDeveloperEditionQuestion = 'Do you wish download a trial version of %s?';

var
  DialogCaptionStr,
  ToolsNameStr,
  ProjectVersionStr,
  AskIncompatibleStr,
  AskNoAddinStr,

  RegKeyStr,
  HelpProjectStr,
  HelpTopicStr,
  UrlStr,
  UrlExeStr,
  AtomName,
  DeveloperProjectNameStr: string;

  AddedAtoms: array of ATOM;

procedure SetToolsCheckingParams(const ADialogCaptionStr, AToolsNameStr,
  AProjectVersionStr, AAskIncompatibleStr, AAskNoAddinStr, ARegKeyStr,
  AHelpProjectStr, AHelpTopicStr, AUrlStr, AUrlExeStr, AAtomName: string;
  const ADeveloperProjectNameStr: string = '');
begin
  DialogCaptionStr := ADialogCaptionStr;
  ToolsNameStr := AToolsNameStr;
  ProjectVersionStr := AProjectVersionStr;
  AskIncompatibleStr := AAskIncompatibleStr;
  AskNoAddinStr := AAskNoAddinStr;
  RegKeyStr := ARegKeyStr;
  HelpProjectStr := AHelpProjectStr;
  HelpTopicStr := AHelpTopicStr;
  UrlStr := AUrlStr;
  UrlExeStr := 'http://' + AUrlStr + '/' + AUrlExeStr;
  AtomName := AAtomName;
  DeveloperProjectNameStr := ADeveloperProjectNameStr;
end;

function NoCheckForTools(Incompatible: boolean; ARegIniFile: TRegIniFile = nil): boolean;
var
  RegIniFile: TRegIniFile;
begin
  if Incompatible and (AskIncompatibleStr = '')
    or not Incompatible and (AskNoAddinStr = '') then begin
    Result := false;
    Exit;
  end;

  if ARegIniFile = nil then
    RegIniFile := TRegIniFile.Create(RegKeyStr, KEY_READ OR KEY_WRITE)
  else
    RegIniFile := ARegIniFile;
  if Incompatible then
    Result := not RegIniFile.ReadBool('', AskIncompatibleStr, True)
  else
    Result := not RegIniFile.ReadBool('', AskNoAddinStr, True);
  if ARegIniFile = nil then
    RegIniFile.Free;
end;

procedure CheckForTools(Incompatible: boolean);
var
  RegIniFile: TRegIniFile;
begin
  RegIniFile := TRegIniFile.Create(RegKeyStr, KEY_READ OR KEY_WRITE);
  with TDownloadForm.Create(Application) do
    try
      try
        if Incompatible and (AskIncompatibleStr = '')
          or not Incompatible and (AskNoAddinStr = '') then
          chkDontAsk.Visible := false;
        if AtomName <> '' then
          if FindAtom({$IFDEF WIN32_64}PChar(AtomName){$ELSE}AtomName{$ENDIF}) = 0 then begin
            SetLength(AddedAtoms, Length(AddedAtoms) + 1);
            AddedAtoms[High(AddedAtoms)] := AddAtom({$IFDEF WIN32_64}PChar(AtomName){$ELSE}AtomName{$ENDIF});
          end
          else
            Exit;
        if not NoCheckForTools(Incompatible, RegIniFile) then begin
          if Incompatible then begin
            lblText1.Caption := sToolsWrongVersion1;
            lblText2.Caption := sToolsWrongVersion2;
            lblText3.Caption := ProjectVersionStr + '.';
          end
          else begin
            lblText1.Caption := sNoTools1;
            lblText2.Caption := sNoTools2;
            lblText3.Caption := '';
          end;
        {$IFDEF FPC}
          lblText1.Width := lblText1.Canvas.GetTextWidth(lblText1.Caption);
          lblText2.Width := lblText2.Canvas.GetTextWidth(lblText2.Caption);
          lblText3.Width := lblText3.Canvas.GetTextWidth(lblText3.Caption);
        {$ENDIF}

          lblProduct.Left := lblText1.Left + lblText1.Width;
          lblProduct.Caption := ToolsNameStr;
        {$IFDEF FPC}
          lblProduct.Width := lblProduct.Canvas.GetTextWidth(lblProduct.Caption);
        {$ENDIF}
          if lblProduct.Left + lblProduct.Width + lblText2.Width > ClientWidth - 8 then begin
            lblText2.Caption := Trim(lblText2.Caption) + ' ';
          {$IFDEF FPC}
            lblText2.Width := lblText2.Canvas.GetTextWidth(lblText2.Caption);
          {$ENDIF}
            lblText2.Top := lblText1.Top + 13;
            lblText2.Left := lblText1.Left;
            lblText3.Left := lblText2.Left + lblText2.Width;
          end
          else begin
            lblText2.Top := lblText1.Top;
            lblText2.Left := lblProduct.Left + lblProduct.Width;
            lblText3.Left := lblText1.Left;
          end;
          lblQuestion.Caption := Format(sToolsDownloadQuestion, [ToolsNameStr]);
          ShowModal;
          if chkDontAsk.Visible then
            if Incompatible then
              RegIniFile.WriteBool('', AskIncompatibleStr, not chkDontAsk.Checked)
            else
              RegIniFile.WriteBool('', AskNoAddinStr, not chkDontAsk.Checked);
        end;
      finally
        Free;
        RegIniFile.Free;
      end;
    except
      on E: Exception do
        MessageBox(0, {$IFDEF WIN32_64}PChar(E.Message){$ELSE}E.Message{$ENDIF},
          'Add-in ñheck error!', 0);
    end;
end;

function DownloadTools(ProVersion: boolean): boolean;
begin
  if ProVersion then
    with TDownloadForm.Create(Application, true) do
      try
        chkDontAsk.Visible := False;
        Result := ShowModal = mrYes;
      finally
        Free;
      end
  else begin
    Result := True;
    OpenUrl(UrlExeStr);
  end;
end;

procedure TDownloadForm.FormShow(Sender: TObject);
begin
  Caption := DialogCaptionStr;
  Image.Picture.Icon.Handle := LoadIcon(0, IDI_QUESTION);
  if (DeveloperProjectNameStr = '') or not chkDontAsk.Visible then
    OKBtn.ModalResult := mrYes
  else
    OKBtn.ModalResult := mrNone;
  if FProVersionNotice then begin
    lblText3.Visible := False;
    lblText3Wrap.Visible := True;
    lblProduct.ParentFont := True;
    lblProduct.Font.Style := [fsBold];
    Height := Height + 26;
    lblText1.Caption := sDeveloperEdition1;
    lblProduct.Cursor := crDefault;
    lblProduct.Left := lblText1.Left + lblText1.Width;
    lblProduct.Caption := DeveloperProjectNameStr;
    lblText2.Caption := sDeveloperEdition2;
    lblText2.Left := lblProduct.Left + lblProduct.Width;
    lblText3Wrap.Caption := Format(sDeveloperEdition3, [ToolsNameStr]);
    lblQuestion.Caption := Format(sDeveloperEditionQuestion, [ToolsNameStr]);
  end;
end;

procedure TDownloadForm.OKBtnClick(Sender: TObject);
begin
  if OKBtn.ModalResult = mrYes then
    OpenUrl(UrlExeStr)
  else
    if DownloadTools(True) then
      Close;
end;

procedure TDownloadForm.btHelpClick(Sender: TObject);
begin
  if HelpTopicStr <> '' then
  {$IFDEF VER8P}
    ShowHelp('ms-help://Devart.' + HelpProjectStr, Format('ms-help://Devart.%s/%s/%s.htm', [HelpProjectStr, HelpProjectStr, HelpTopicStr]));
  {$ELSE}
  {$IFNDEF FPC}
    ShowHelp(GetHelpFileName(HelpProjectStr), HelpTopicStr);
  {$ENDIF}
  {$ENDIF}
end;

procedure TDownloadForm.FormMouseMove(Sender: TObject; Shift: TShiftState;
  X, Y: Integer);
begin
  if chkDontAsk.Visible then
    lblProduct.Font.Color := $FF0000;
end;

procedure TDownloadForm.lblProductMouseMove(Sender: TObject;
  Shift: TShiftState; X, Y: Integer);
begin
  if chkDontAsk.Visible then
    lblProduct.Font.Color := $4080FF;
end;

procedure TDownloadForm.lblProductClick(Sender: TObject);
begin
  if chkDontAsk.Visible then begin
    OpenUrl('http://' + UrlStr);
    lblProduct.Font.Color := $FF0000;
  end;
end;

procedure DeleteAddedAtoms;
var
  i: integer;
begin
  for i := 0 to High(AddedAtoms) do
    DeleteAtom(AddedAtoms[i]);
end;

constructor TDownloadForm.Create(AOwner: TComponent; ProVersionNotice: boolean);
begin
  inherited Create(AOwner);
  FProVersionNotice := ProVersionNotice;
end;

initialization

finalization
  DeleteAddedAtoms;
end.
