unit MainFormUnit;


interface

uses
  SysUtils, Types, UITypes, Classes, Variants, FMX_Types, FMX_Controls, FMX_Forms,
  FMX_Dialogs, FMX_Ani, FMX_ListBox, FMX_Layouts, FMX_Effects, FMX_Filter_Effects,
  FMX_Objects, mainFormGalery, Accelerometer;

type
  TForm3 = class(TForm)
    LabelOfTime: TLabel;
    LabelForTime: TLabel;
    StartStopButton: TButton;
    TimerOfTimeOfGame: TTimer;
    PanelOfPuzzle: TPanel;
    P1: TPanel;
    P2: TPanel;
    P3: TPanel;
    P4: TPanel;
    P5: TPanel;
    P6: TPanel;
    P7: TPanel;
    P8: TPanel;
    P9: TPanel;
    P10: TPanel;
    P11: TPanel;
    P12: TPanel;
    P13: TPanel;
    P14: TPanel;
    P15: TPanel;
    ImageControl1: TImageControl;
    ImageControl2: TImageControl;
    ImageControl3: TImageControl;
    ImageControl4: TImageControl;
    ImageControl5: TImageControl;
    ImageControl6: TImageControl;
    ImageControl7: TImageControl;
    ImageControl8: TImageControl;
    ImageControl9: TImageControl;
    ImageControl10: TImageControl;
    ImageControl11: TImageControl;
    ImageControl12: TImageControl;
    ImageControl13: TImageControl;
    ImageControl14: TImageControl;
    ImageControl15: TImageControl;
    ImageControl16: TImageControl;
    AniIndicator1: TAniIndicator;
    ListBoxOfImagesToPuzzle: TListBox;
    ListBoxItem1: TListBoxItem;
    ListBoxItem2: TListBoxItem;
    ListBoxItem3: TListBoxItem;
    ImageControl17: TImageControl;
    ImageControl18: TImageControl;
    ImageControl19: TImageControl;
    ComboBoxOfShuffligTimes: TComboBox;
    LabelOfShuffling: TLabel;
    ListBoxItem5: TListBoxItem;
    ListBoxItem6: TListBoxItem;
    ListBoxItem7: TListBoxItem;
    ListBoxItem8: TListBoxItem;
    ListBoxItem9: TListBoxItem;
    ListBoxItem10: TListBoxItem;
    ListBoxItem11: TListBoxItem;
    ListBoxItem12: TListBoxItem;
    ListBoxItem13: TListBoxItem;
    ListBoxItem14: TListBoxItem;
    ListBoxItem4: TListBoxItem;
    ListBoxItem15: TListBoxItem;
    ListBoxItem16: TListBoxItem;
    ListBoxItem17: TListBoxItem;
    ListBoxItem18: TListBoxItem;
    ImageControl20: TImageControl;
    ImageControl21: TImageControl;
    ImageControl22: TImageControl;
    ImageControl23: TImageControl;
    ImageControl24: TImageControl;
    MagnifyEffect1: TMagnifyEffect;
    MagnifiAnimation: TFloatAnimation;
    Button1: TButton;
    Rectangle1: TRectangle;
    Rectangle2: TRectangle;
    StyleBook1: TStyleBook;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    Label5: TLabel;
    Label6: TLabel;
    Label7: TLabel;
    Label8: TLabel;
    Label9: TLabel;
    Label10: TLabel;
    Label11: TLabel;
    Label12: TLabel;
    Label13: TLabel;
    Label14: TLabel;
    Label15: TLabel;
    Label16: TLabel;
    Label17: TLabel;
    ShadowEffect1: TShadowEffect;
    ShadowEffect2: TShadowEffect;
    ShadowEffect3: TShadowEffect;
    ShadowEffect4: TShadowEffect;
    ShadowEffect5: TShadowEffect;
    ShadowEffect6: TShadowEffect;
    ShadowEffect7: TShadowEffect;
    ShadowEffect8: TShadowEffect;
    GlowEffect1: TGlowEffect;
    GlowEffect2: TGlowEffect;
    GlowEffect3: TGlowEffect;
    GlowEffect4: TGlowEffect;
    GlowEffect5: TGlowEffect;
    GlowEffect6: TGlowEffect;
    GlowEffect7: TGlowEffect;
    GlowEffect8: TGlowEffect;
    GlowEffect9: TGlowEffect;
    GlowEffect10: TGlowEffect;
    GlowEffect11: TGlowEffect;
    GlowEffect12: TGlowEffect;
    GlowEffect13: TGlowEffect;
    GlowEffect14: TGlowEffect;
    GlowEffect15: TGlowEffect;
    ListBoxItem19: TListBoxItem;
    ImageControl25: TImageControl;
    ListBoxItem20: TListBoxItem;
    ImageControl26: TImageControl;
    ListBoxItem21: TListBoxItem;
    ImageControl27: TImageControl;
    procedure FormCreate(Sender: TObject);
    procedure StartStopButtonClick(Sender: TObject);
    procedure TimerOfTimeOfGameTimer(Sender: TObject);
    procedure MakeMove(Sender: TObject);
    procedure FormResize(Sender: TObject);
    procedure SelectImageToPuzzle(Sender: TObject);
    procedure MagnifiAnimationFinish(Sender: TObject);
    procedure ImageControl1Click(Sender: TObject);
    procedure ListBoxOfImagesToPuzzleChange(Sender: TObject);
    procedure ListBoxOfImagesToPuzzleDblClick(Sender: TObject);
    procedure FormActivate(Sender: TObject);
  private
    { Private declarations }
    fCas: TTime;
    fStart: TTime;
    fMoves: integer;

    fMapa: Array [1 .. 4, 1 .. 4] of TStyledControl;

    procedure PorcujObrazek(Panel: TPanel);
    procedure SetPosition;
    procedure MakeShufle;

    procedure setCas(const Value: TTime);
    procedure PosunDolu(x, y: integer; animate: Boolean = true);
    procedure PosunNahoru(x, y: integer; animate: Boolean = true);
    procedure PosunVlevo(x, y: integer; animate: Boolean = true);
    procedure PosunVpravo(x, y: integer; animate: Boolean = true);

    function doMakeMove(Sender: TObject; animate: Boolean = true): Boolean;
    procedure CheckComplete;
    procedure setMoves(const Value: integer);

    property Moves: integer read fMoves write setMoves default 0;
  public
    { Public declarations }
    Gal: TFrmMain;

    property Cas: TTime read fCas write setCas;
  end;

var
  Form3: TForm3;

implementation

{$R *.lfm}

procedure TForm3.StartStopButtonClick(Sender: TObject);
var
  x: integer;
  y: integer;

begin
  if TimerOfTimeOfGame.Enabled then
  begin
    TimerOfTimeOfGame.Enabled := false;
    StartStopButton.Text := Translate('Start');
    ImageControl1.Visible := true;
    Moves := 0;
    Cas := 0;

    // nastaveni vychozi pozice
    Cas := 0;
    fStart := now;
    fMapa[1, 1] := P1;
    fMapa[2, 1] := P2;
    fMapa[3, 1] := P3;
    fMapa[4, 1] := P4;

    fMapa[1, 2] := P5;
    fMapa[2, 2] := P6;
    fMapa[3, 2] := P7;
    fMapa[4, 2] := P8;

    fMapa[1, 3] := P9;
    fMapa[2, 3] := P10;
    fMapa[3, 3] := P11;
    fMapa[4, 3] := P12;

    fMapa[1, 4] := P13;
    fMapa[2, 4] := P14;
    fMapa[3, 4] := P15;
    fMapa[4, 4] := nil;

    SetPosition;
    ListBoxOfImagesToPuzzle.Visible := false;
    Button1.AnimateFloat('opacity', 1, 0.5);
    MagnifiAnimation.Stop;
    MagnifyEffect1.Magnification := 1;
  end
  else
  begin
    AniIndicator1.Visible := true;
    try
      MagnifiAnimation.Stop;
      MagnifyEffect1.Magnification := 1;
      ListBoxOfImagesToPuzzle.Visible := false;
      Button1.AnimateFloat('opacity', 0, 0.5);
      TimerOfTimeOfGame.Enabled := true;
      StartStopButton.Text := Translate('Stop');
      fStart := now;
      TimerOfTimeOfGameTimer(self);

      ImageControl1.Visible := true;
      ImageControl1.Opacity := 1;
      for x := 1 to 4 do
        for y := 1 to 4 do
          if fMapa[x, y] <> nil then
            PorcujObrazek(TPanel(fMapa[x, y]));
      ImageControl1.Visible := true;
      MakeShufle;
      Moves := 0;
      ImageControl1.Visible := false;
    finally
      AniIndicator1.Visible := false;
    end;
  end;
end;

function TForm3.doMakeMove(Sender: TObject; animate: Boolean = true): Boolean;
var
  fMove, fFind: Boolean;
  x, y: integer;
begin
  fFind := false;
  fMove := false;
  for x := 1 to 4 do
  begin
    for y := 1 to 4 do
      if fMapa[x, y] = Sender then
      begin
        fFind := true;
        Break;
      end;
    if fFind then
      Break;
  end;

  // nalezeny sender
  if fFind then
  begin
    Moves := Moves + 1;
    // najdi volne misto
    if x > 1 then
      if fMapa[x - 1, y] = nil then
      begin
        PosunVlevo(x, y, animate);
        fMove := true;
      end;
    if x < 4 then
      if fMapa[x + 1, y] = nil then
      begin
        PosunVpravo(x, y, animate);
        fMove := true;
      end;

    if y > 1 then
      if fMapa[x, y - 1] = nil then
      begin
        PosunNahoru(x, y, animate);
        fMove := true;
      end;

    if y < 4 then
      if fMapa[x, y + 1] = nil then
      begin
        PosunDolu(x, y, animate);
        fMove := true;
      end;
  end;
  Result := fMove;
end;

procedure TForm3.FormActivate(Sender: TObject);
begin
  FormResize(self);
end;

procedure TForm3.FormCreate(Sender: TObject);
var
  I: integer;
  y: integer;
  Cnt: integer;
begin
  Cas := 0;
  fStart := now;
  fMapa[1, 1] := P1;
  fMapa[2, 1] := P2;
  fMapa[3, 1] := P3;
  fMapa[4, 1] := P4;

  fMapa[1, 2] := P5;
  fMapa[2, 2] := P6;
  fMapa[3, 2] := P7;
  fMapa[4, 2] := P8;

  fMapa[1, 3] := P9;
  fMapa[2, 3] := P10;
  fMapa[3, 3] := P11;
  fMapa[4, 3] := P12;

  fMapa[1, 4] := P13;
  fMapa[2, 4] := P14;
  fMapa[3, 4] := P15;
  fMapa[4, 4] := nil;

  MagnifiAnimation.Stop;
  MagnifyEffect1.Magnification := 1;

  ImageControl1.Align := TAlignLayout.alClient;

  Gal := TFrmMain.Create(self);

  Cnt := 0;
  for I := 0 to Form3.ListBoxOfImagesToPuzzle.Count - 1 do
    for y := 0 to Form3.ListBoxOfImagesToPuzzle.ItemByIndex(I).ChildrenCount - 1 do
      if Form3.ListBoxOfImagesToPuzzle.ItemByIndex(I).Children[y] is TImageControl then
      begin
        Form3.Gal.AddImage(TImageControl(Form3.ListBoxOfImagesToPuzzle.ItemByIndex(I).Children[y]), Cnt);
        Inc(Cnt);
      end;
  Gal.SetBounds(0, 0, 320, 460);

  SetPosition;
  ListBoxOfImagesToPuzzle.Visible := false;
  Button1.AnimateFloat('opacity', 1, 0.5);
  MagnifiAnimation.Stop;
  MagnifyEffect1.Magnification := 1;
end;

procedure TForm3.FormResize(Sender: TObject);
//var
  //x: integer;
  //y: integer;
begin
  //Gal.AniIndicator2.Visible := true;
  // prepocet na jiny rozmer
  //if Width > Height then
  //begin
    // landscape
    //Rectangle1.Align := TAlignLayout.alRight;
    //Rectangle1.Width := 200;
    //StartStopButton.Position.x := 100;
    //StartStopButton.Position.y := 200;

    //Button1.Position.x := 100;
    //Button1.Position.y := 120;
  //end
  //else
  //begin
    // portrait
    //Rectangle1.Align := TAlignLayout.alTop;
    //Rectangle1.Height := 130;
    //StartStopButton.Position.x := 230;
    //StartStopButton.Position.y := 50;

    //Button1.Position.x := 130;
    //Button1.Position.y := 50;
  //end;

  //if PanelOfPuzzle.Width >= (Rectangle2.Height - (PanelOfPuzzle.Position.y * 2)) then
    //PanelOfPuzzle.Width := Rectangle2.Height - (PanelOfPuzzle.Position.y * 2)
  //else
    //PanelOfPuzzle.Width := Rectangle2.Width - (PanelOfPuzzle.Position.x * 2);

  //PanelOfPuzzle.Height := PanelOfPuzzle.Width;
  //PanelOfPuzzle.Repaint;

  //Button1.Width := 80;
  //Button1.Height := 60;

  //StartStopButton.Width := 80;
  //StartStopButton.Height := 60;

  //ComboBoxOfShuffligTimes.Height := 21;

  // LabelOfTime.Text := Format('x:%d y:%d', [Width, Height]);

  //SetPosition;
  //for x := 1 to 4 do
    //for y := 1 to 4 do
      //if fMapa[x, y] <> nil then
        //PorcujObrazek(TPanel(fMapa[x, y]));

  //Gal.SetBounds(0, 0, Form3.Width, Form3.Height);
  //Gal.AniIndicator2.Visible := false;
end;

procedure TForm3.SelectImageToPuzzle(Sender: TObject);
begin
  ImageControl1.Align := TAlignLayout.alNone;
  ImageControl1.Width := 1024;
  ImageControl1.Height := 1024;

  if not TimerOfTimeOfGame.Enabled then
    ImageControl1.Bitmap.Assign(TImageControl(Sender).Bitmap);

  MagnifiAnimation.Stop;
  MagnifyEffect1.Magnification := 1;
  ImageControl1.Align := TAlignLayout.alClient;
end;

procedure TForm3.ImageControl1Click(Sender: TObject);
begin
  MagnifiAnimation.Stop;
  MagnifyEffect1.Magnification := 1;
end;

procedure TForm3.ListBoxOfImagesToPuzzleChange(Sender: TObject);
var
  I: integer;
begin
  AniIndicator1.Visible := true;
  try
    if Assigned(ListBoxOfImagesToPuzzle.ItemByIndex(ListBoxOfImagesToPuzzle.ItemIndex)) then
    begin
      for I := 0 to ListBoxOfImagesToPuzzle.ItemByIndex(ListBoxOfImagesToPuzzle.ItemIndex).ChildrenCount - 1 do
        if ListBoxOfImagesToPuzzle.ItemByIndex(ListBoxOfImagesToPuzzle.ItemIndex).Children[I] is TImageControl then
        begin
          SelectImageToPuzzle(ListBoxOfImagesToPuzzle.ItemByIndex(ListBoxOfImagesToPuzzle.ItemIndex).Children[I]);
          Break;
        end;
    end;
  finally
    AniIndicator1.Visible := false;
  end;
end;

procedure TForm3.ListBoxOfImagesToPuzzleDblClick(Sender: TObject);
begin
  AniIndicator1.Visible := true;
{$IFDEF FPC}
  //Application.ProcessMessages;
{$ENDIF}
  MagnifiAnimation.Stop;
  MagnifyEffect1.Magnification := 1;
{$IFDEF FPC}
  Application.ProcessMessages;
{$ENDIF}
  Gal.Position := TFormPosition.poOwnerFormCenter;

  Gal.SetBounds(0, 0, 320, 460);

  AniIndicator1.Visible := false;

  if Gal.ShowModal = mrOk then
    ListBoxOfImagesToPuzzle.ItemIndex := Gal.CoverIndex;
end;

procedure TForm3.PosunVlevo(x, y: integer; animate: Boolean = true);
begin
  fMapa[x - 1, y] := fMapa[x, y];
  if animate then
    fMapa[x, y].AnimateFloat('Position.x', fMapa[x, y].Position.x - fMapa[x, y].Width);

  fMapa[x, y] := nil;
end;

procedure TForm3.PosunVpravo(x, y: integer; animate: Boolean = true);
begin
  fMapa[x + 1, y] := fMapa[x, y];
  if animate then
    fMapa[x, y].AnimateFloat('Position.x', fMapa[x, y].Position.x + fMapa[x, y].Width);

  fMapa[x, y] := nil;
end;

procedure TForm3.PosunNahoru(x, y: integer; animate: Boolean = true);
begin
  fMapa[x, y - 1] := fMapa[x, y];
  if animate then
    fMapa[x, y].AnimateFloat('Position.y', fMapa[x, y].Position.y - fMapa[x, y].Height);

  fMapa[x, y] := nil;
end;

procedure TForm3.PosunDolu(x, y: integer; animate: Boolean = true);
begin
  fMapa[x, y + 1] := fMapa[x, y];
  if animate then
    fMapa[x, y].AnimateFloat('Position.y', fMapa[x, y].Position.y + fMapa[x, y].Height);

  fMapa[x, y] := nil;
end;

procedure TForm3.CheckComplete;
var
  x: integer;
  y: integer;
  fIsOk: Boolean;
begin
  fIsOk := true;
  for x := 1 to 4 do
    for y := 1 to 4 do
      if fMapa[x, y] <> nil then
        fIsOk := fIsOk and ((((y - 1) * 4) + x) = fMapa[x, y].tag);

  if fIsOk then
  begin
    TimerOfTimeOfGame.Enabled := false;
    StartStopButton.Text := Translate('Start');
    MagnifiAnimation.Start;
    ImageControl1.Visible := true;
    Button1.AnimateFloat('opacity', 1, 0.5);
    ListBoxOfImagesToPuzzle.Visible := false;
  end;
end;

procedure TForm3.MagnifiAnimationFinish(Sender: TObject);
begin
  MagnifyEffect1.Magnification := 1;
end;

procedure TForm3.MakeMove(Sender: TObject);
begin
  // provede posun pokud je mozny
  if TimerOfTimeOfGame.Enabled then
  begin
    doMakeMove(Sender);
    CheckComplete;
  end;
end;

procedure TForm3.MakeShufle;
var
  I, x, y: Byte;
begin
  PanelOfPuzzle.BeginUpdate;
{$IFDEF FPC}
  Application.ProcessMessages;
{$ENDIF}
  try
    I := StrToInt(ComboBoxOfShuffligTimes.Items[ComboBoxOfShuffligTimes.ItemIndex]);
    repeat
      x := random(4) + 1;
      y := random(4) + 1;
      if fMapa[x, y] <> nil then
        if doMakeMove(fMapa[x, y], false) then
        begin
          dec(I);
          if I mod 10 = 0 then
{$IFDEF FPC}
            //Application.ProcessMessages;
{$ENDIF}
        end;
    until I < 1;
  finally
    SetPosition;
    PanelOfPuzzle.EndUpdate;
{$IFDEF FPC}
    Application.ProcessMessages;
{$ENDIF}
  end;
end;

procedure TForm3.PorcujObrazek(Panel: TPanel);
var
  Img: TImageControl;
  I, x, y, x1, y1: integer;
  ii: integer;
  tb: TBitmap;
begin
  Img := nil;
  for I := 0 to (Panel.ChildrenCount - 1) do
  begin
    if Panel.Children[I] is TImageControl then
    begin
      Img := TImageControl(Panel.Children[I]);
      Break;
    end;
  end;

  // nalezen obrazec prideleny panelu
  // provede se strih na jeho pozici
  tb := nil;
  if Assigned(Img) then
  begin

    I := Panel.tag;
    x := ((I - 1) mod 4);
    y := ((I - 1) div 4);

    tb := TBitmap.Create((ImageControl1.Bitmap.Width div 4), (ImageControl1.Bitmap.Height div 4));

    y1 := 0;
    for I := y * (ImageControl1.Bitmap.Height div 4) to (y + 1) * (ImageControl1.Bitmap.Height div 4) - 1 do
    begin
      x1 := 0;
      for ii := x * (ImageControl1.Bitmap.Width div 4) to (x + 1) * (ImageControl1.Bitmap.Width div 4) - 1 do
      begin
        tb.ScanLine[y1][x1] := ImageControl1.Bitmap.ScanLine[I][ii];
        Inc(x1);
      end;
      Inc(y1);
    end;
  end;
  Img.Bitmap.Assign(tb);
  Img.Repaint;
end;

procedure TForm3.setCas(const Value: TTime);
begin
  fCas := Value;
  LabelForTime.Text := FormatDateTime('hh:mm:ss', fCas);
end;

procedure TForm3.setMoves(const Value: integer);
begin
  fMoves := Value;
  Label2.Text := IntToStr(fMoves);
end;

procedure TForm3.SetPosition;
var
  x: integer;
  y: integer;
  velBox: Single;
begin
  velBox := (PanelOfPuzzle.Width / 4);
  for x := 1 to 4 do
    for y := 1 to 4 do
      if fMapa[x, y] <> nil then
        with fMapa[x, y] do
        begin
          Width := velBox;
          Height := velBox;
          Position.x := (x - 1) * (Width);
          Position.y := (y - 1) * (Height);
        end;
end;

procedure TForm3.TimerOfTimeOfGameTimer(Sender: TObject);
begin
  Cas := now - fStart;
end;

end.

