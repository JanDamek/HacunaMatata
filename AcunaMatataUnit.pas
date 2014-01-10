unit AcunaMatataUnit;

interface

uses
  System.SysUtils,
  System.Types, System.UITypes, System.Classes,
  System.Variants,
  FMX.Forms, FMX.Types, FMX.Controls,

  DataModulUnit,

  ddPlugin_TLB,

  FMX.Menus, FMX.Ani,
  FMX.TreeView, FMX.Grid, FMX.TabControl, FMX.Layouts, FMX.Effects, FMX.Skin, FMX.Dialogs, FMX.ListBox, FMX.Edit,
  FMX.ExtCtrls, FMX.Memo, FMX.Objects3D, FMX.Types3D, FMX.Filter.Effects, FMX.Layers3D, HeGrid, SearchTool;

type
  TAcunaMatataForm = class(TForm)
    MenuBar1: TMenuBar;
    MenuEditace: TMenuItem;
    MenuItemOprava: TMenuItem;
    MenuItemNovy: TMenuItem;
    MenuItem5: TMenuItem;
    MenuItem6: TMenuItem;
    MenuItem7: TMenuItem;
    MenuItem8: TMenuItem;
    MenuItem9: TMenuItem;
    MenuItem10: TMenuItem;
    MenuItem11: TMenuItem;
    MenuItem12: TMenuItem;
    MenuItem13: TMenuItem;
    MenuItem14: TMenuItem;
    MenuItem15: TMenuItem;
    MenuItem16: TMenuItem;
    Panel1: TPanel;
    MenuBar2: TMenuBar;
    MenuItem4: TMenuItem;
    MenuItem17: TMenuItem;
    Panel2: TPanel;
    MenuBar3: TMenuBar;
    Page1: TMenuItem;
    Page2: TMenuItem;
    Page3: TMenuItem;
    Page4: TMenuItem;
    TabControl1: TTabControl;
    TabItem1: TTabItem;
    TabItem2: TTabItem;
    TabItem3: TTabItem;
    TabItem4: TTabItem;
    Label1: TLabel;
    Label4: TLabel;
    TreeView1: TTreeView;
    Splitter1: TSplitter;
    TreeViewItem1: TTreeViewItem;
    TreeViewItem2: TTreeViewItem;
    TreeViewItem3: TTreeViewItem;
    TreeViewItem4: TTreeViewItem;
    TreeViewItem5: TTreeViewItem;
    TreeViewItem6: TTreeViewItem;
    Panel4: TPanel;
    Panel5: TPanel;
    Splitter2: TSplitter;
    Expander1: TExpander;
    CalloutPanel1: TCalloutPanel;
    PopupBox1: TPopupBox;
    FloatAnimation1: TFloatAnimation;
    Label2: TLabel;
    Label5: TLabel;
    Label6: TLabel;
    Label7: TLabel;
    Label8: TLabel;
    Label9: TLabel;
    Label10: TLabel;
    MenuItem19: TMenuItem;
    MenuLeva: TMenuItem;
    MenuPrava: TMenuItem;
    Panel3: TPanel;
    StyleBook1: TStyleBook;
    MainMenu1: TMainMenu;
    SkinListBox: TListBox;
    CheckBox1: TCheckBox;
    RadioButton1: TRadioButton;
    ProgressBar1: TProgressBar;
    TrackBar1: TTrackBar;
    ComboBox1: TComboBox;
    Memo1: TMemo;
    Edit1: TEdit;
    Calendar1: TCalendar;
    CalendarBox1: TCalendarBox;
    NumberBox1: TNumberBox;
    ComboTrackBar1: TComboTrackBar;
    SpinBox1: TSpinBox;
    DropTarget1: TDropTarget;
    ListBoxItem1: TListBoxItem;
    ListBoxItem2: TListBoxItem;
    ListBoxItem3: TListBoxItem;
    ListBoxItem4: TListBoxItem;
    ListBoxItem5: TListBoxItem;
    ListBoxItem6: TListBoxItem;
    ClearingEdit1: TClearingEdit;
    Panel6: TPanel;
    Label11: TLabel;
    GroupBox1: TGroupBox;
    PopupBox2: TPopupBox;
    RadioButton2: TRadioButton;
    RadioButton3: TRadioButton;
    ImageControl1: TImageControl;
    Switch1: TSwitch;
    Switch2: TSwitch;
    PlotGrid1: TPlotGrid;
    Grid2: TGrid;
    Column2: TColumn;
    CheckColumn1: TCheckColumn;
    StringColumn1: TStringColumn;
    ProgressColumn1: TProgressColumn;
    PopupColumn1: TPopupColumn;
    ImageColumn1: TImageColumn;
    CheckBox2: TCheckBox;
    MenuBar4: TMenuBar;
    MenuItem1: TMenuItem;
    Viewport3D1: TViewport3D;
    Camera1: TCamera;
    Text3D1: TText3D;
    FloatAnimation2: TFloatAnimation;
    FloatAnimation4: TFloatAnimation;
    FloatAnimation5: TFloatAnimation;
    Layout3D1: TLayout3D;
    Layer3D1: TLayer3D;
    WaterTransitionEffect1: TWaterTransitionEffect;
    Light1: TLight;
    RoundCube1: TRoundCube;
    FloatAnimation6: TFloatAnimation;
    FloatAnimation7: TFloatAnimation;
    Expander2: TExpander;
    ScrollBox1: TScrollBox;
    Viewport3D2: TViewport3D;
    Viewport3D3: TViewport3D;
    Sphere2: TSphere;
    FloatAnimation8: TFloatAnimation;
    FloatAnimation9: TFloatAnimation;
    MenuItem3: TMenuItem;
    MenuItem21: TMenuItem;
    HeGrid1: THeGrid;
    procedure PageClick(Sender: TObject);
    procedure TabControl1Change(Sender: TObject);
    procedure Page1MouseEnter(Sender: TObject);
    procedure Page1MouseLeave(Sender: TObject);
    procedure FloatAnimation1Finish(Sender: TObject);
    procedure TreeView1Change(Sender: TObject);
    procedure MenuLevaClick(Sender: TObject);
    procedure MenuPravaClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure SkinListBoxChange(Sender: TObject);
    procedure Grid2GetValue(Sender: TObject; const Col, Row: Integer; var Value: Variant);
    procedure MenuItemOpravaClick(Sender: TObject);
    procedure FormActivate(Sender: TObject);
    procedure ScrollBox1Resize(Sender: TObject);
    procedure MenuItemNovyClick(Sender: TObject);
    procedure Sphere2MouseWheel(Sender: TObject; Shift: TShiftState; WheelDelta: Integer; var Handled: Boolean);
    procedure TreeView1MouseUp(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Single);
    procedure MenuItem1Click(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
  private
    { Private declarations }
    procedure ApplySkin(SkinName: String);
  public
    { Public declarations }
    fHelios: IHelios;
    procedure StartForm;
  end;

var
  AcunaMatataForm: TAcunaMatataForm;

implementation

{$R *.fmx}

uses ddBrowse, ddTabulka, ddType, FMX.Platform, ddMain,
  DefinatorEditoruUnit, EditorUnit, deMain, deType, sqProc;

{ TAcunaMatataForm }

procedure TAcunaMatataForm.FloatAnimation1Finish(Sender: TObject);
begin
  CalloutPanel1.Visible := CalloutPanel1.Opacity <> 0;
end;

const
  SkinPath = 'Skins\';

procedure TAcunaMatataForm.FormActivate(Sender: TObject);
begin
  if Assigned(DefinatorEditoruForm) then
    DefinatorEditoruForm.Hide;
end;

procedure TAcunaMatataForm.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  if Assigned(DefinatorEditoruForm) then
    DefinatorEditoruForm.Free;
end;

procedure TAcunaMatataForm.FormCreate(Sender: TObject);
var
  sr: TSearchRec;
begin
  fHelios := fHeliosGlobal;
  StartForm;
  if FindFirst(SkinPath + '*.*', faDirectory, sr) = 0 then
    repeat
      if (sr.Name <> '.') and (sr.Name <> '..') then
        SkinListBox.Items.Add(sr.Name);
    until FindNext(sr) <> 0;
  System.SysUtils.FindClose(sr);
end;

procedure TAcunaMatataForm.Grid2GetValue(Sender: TObject; const Col, Row: Integer; var Value: Variant);
begin
  case Col of
    0:
      Value := IntToStr(Row);
    1:
      Value := (Row mod 2) = 0;
    2:
      Value := 'Textovy radek';
    3:
      Value := (Row / 2) * 10;
    4:
      Value := (Row mod 2);
    5:
      Value := ImageControl1.Bitmap.ToString;
  end;
end;

procedure TAcunaMatataForm.MenuItem1Click(Sender: TObject);
begin
  // otevre difinator pro editory
  if not Assigned(DefinatorEditoruForm) then
    DefinatorEditoruForm := TDefinatorEditoruForm.Create(Application);
  DefinatorEditoruForm.Show;
end;

procedure TAcunaMatataForm.MenuItemNovyClick(Sender: TObject);
begin
  // nova polozka
  HeGrid1.MenuItemNovyClick(Sender);
end;

procedure TAcunaMatataForm.MenuItemOpravaClick(Sender: TObject);
begin
  // oprava polozky
  HeGrid1.MenuItemOpravaClick(Sender);
end;

procedure TAcunaMatataForm.MenuLevaClick(Sender: TObject);
begin
  if TabControl1.TabIndex > 0 then
    TabControl1.TabIndex := TabControl1.TabIndex - 1;
end;

procedure TAcunaMatataForm.MenuPravaClick(Sender: TObject);
begin
  if TabControl1.TabIndex < TabControl1.TabCount then
    TabControl1.TabIndex := TabControl1.TabIndex + 1;
end;

procedure TAcunaMatataForm.Page1MouseEnter(Sender: TObject);
begin
  CalloutPanel1.Visible := true;
  FloatAnimation1.Inverse := false;
  FloatAnimation1.Start;
end;

procedure TAcunaMatataForm.Page1MouseLeave(Sender: TObject);
begin
  FloatAnimation1.Inverse := true;
  FloatAnimation1.Start;
end;

procedure TAcunaMatataForm.PageClick(Sender: TObject);
begin
  TabControl1.TabIndex := TMenuItem(Sender).Tag;
  TMenuItem(Sender).IsSelected := true;
end;

procedure TAcunaMatataForm.ApplySkin(SkinName: String);
var
  SkinStream: TFileStream;
begin
  HeGrid1.BeginUpdate;
  SkinStream := TFileStream.Create(SkinName, fmOpenRead);
  StyleBook1.Root := CreateObjectFromStream(nil, SkinStream);
  self.StyleLookup := 'backgroundstyle';
  self.UpdateStyle;
  SkinStream.Free;
  HeGrid1.EndUpdate;
end;

procedure TAcunaMatataForm.ScrollBox1Resize(Sender: TObject);
begin
  ScrollBox1.Realign;
end;

procedure TAcunaMatataForm.SkinListBoxChange(Sender: TObject);
var
  SkinName: String;
begin
  Platform.SetCursor(self, crHourGlass);
  SkinName := SkinListBox.Items.Strings[SkinListBox.ItemIndex];
  SkinName := SkinPath + SkinName + '\' + SkinName + '.style';
  ApplySkin(SkinName);
  Platform.SetCursor(self, crDefault);
end;

procedure TAcunaMatataForm.Sphere2MouseWheel(Sender: TObject; Shift: TShiftState; WheelDelta: Integer;
  var Handled: Boolean);
begin
  Sphere2.RotationAngle.X := Sphere2.RotationAngle.X;
  Handled := true;
end;

procedure TAcunaMatataForm.StartForm;
begin
  // priprava formu
  TabControl1.TabIndex := 2;
  Page3.IsSelected := true;
  CalloutPanel1.Opacity := 0;

  HeGrid1.MenuItemNovy := MenuItemNovy;
  HeGrid1.MenuItemOprava := MenuItemOprava;
end;

procedure TAcunaMatataForm.TabControl1Change(Sender: TObject);
begin
  case TabControl1.TabIndex of
    0:
      Page1.IsSelected := true;
    1:
      Page2.IsSelected := true;
    2:
      Page3.IsSelected := true;
    3:
      Page4.IsSelected := true;
  end;
end;

procedure TAcunaMatataForm.TreeView1Change(Sender: TObject);
var
  aBrowse: TVychoziNastaveniBrowse;
begin
  if TreeView1.Selected.Tag <> 0 then
  begin
    aBrowse := SeznamVychozichNastaveniBrowse[TBrowse(TreeView1.Selected.Tag)];
    HeGrid1.Helios := fHelios;
    HeGrid1.Browse := TBrowse(TreeView1.Selected.Tag);

  end
  else
  begin
    TreeView1.Selected.IsExpanded := not TreeView1.Selected.IsExpanded;
    Exit;
  end;

  TabControl1.TabIndex := 1;
end;

procedure TAcunaMatataForm.TreeView1MouseUp(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Single);
begin
  TreeView1Change(Sender);
end;

end.
