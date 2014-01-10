unit DefinatorEditoruUnit;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Dialogs, FMX.Layouts, FMX.Menus, FMX.Memo, FMX.TabControl, SynEditHighlighter,
  SynHighlighterSQL, SynEditTextBase, SynEdit, SynEditMiscClasses, SynMemo, FMX.ListBox, EditFormUnit;

type
  TDefinatorEditoruForm = class(TEditForm)
    LayoutSpodniLista: TLayout;
    Layout2: TLayout;
    Button1: TButton;
    LayoutHlavniEditor: TLayout;
    LayoutHorniLista: TLayout;
    LayoutLevySloupec: TLayout;
    LayoutPravySloupec: TLayout;
    Splitter1: TSplitter;
    Splitter2: TSplitter;
    Layout7: TLayout;
    LayoutMessageDolniLista: TLayout;
    Splitter3: TSplitter;
    LayoutEditor: TLayout;
    TabControl2: TTabControl;
    Layout10: TLayout;
    Button2: TButton;
    Button3: TButton;
    Label1: TLabel;
    EditorComboBox: TComboBox;
    AtributyListBox: TListBox;
    VlastnostiListBox: TListBox;
    Memo2: TMemo;
    Layout11: TLayout;
    procedure Button1Click(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure FormCreate(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  DefinatorEditoruForm: TDefinatorEditoruForm;

implementation

uses DefEditUnit, deType, ddMain;

{$R *.fmx}

procedure TDefinatorEditoruForm.Button1Click(Sender: TObject);
begin
  Hide;
end;

procedure TDefinatorEditoruForm.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  Action := TCloseAction.caHide;
end;

procedure TDefinatorEditoruForm.FormCreate(Sender: TObject);
var
  fTListBoxItem: TListBoxItem;
  s,s1: String;
begin
  // SynEdit1.Highlighter := SynSQLSyn1;
  // SynEdit1.Lines.Text := SynSQLSyn1.SampleSource;
  DefEdit.First;
  while not DefEdit.Eof do
  begin
    fTListBoxItem := TListBoxItem.Create(EditorComboBox);
    fTListBoxItem.Parent := EditorComboBox;
    EditorComboBox.InsertComponent(fTListBoxItem);

    s := '';
    if teOprava in DefEdit.Typ then
      s := 'Oprava';
    if teNovy in DefEdit.Typ then
      if s = '' then
        s := 'Novy '
      else
        s := s + '/Novy';

    try
      s1:='"'+VerejneJmenoPrehledu(StrToInt(DefEdit.BrowseID_DPSN))+'"';
    except
      s1:='';
    end;
    fTListBoxItem.Text := 'Akce:'+s + ' ID_DPSN:' + DefEdit.BrowseID_DPSN+' Nazev:' + DefEdit.Nazev+' | '+s1;
    fTListBoxItem.Data := TObject(DefEdit.StringGUID);
    DefEdit.Next;
  end;

  TabControl1.Parent := LayoutEditor;
end;

initialization

DefinatorEditoruForm := nil;

end.
