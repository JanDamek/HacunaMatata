unit DataModulUnit;

interface

uses
  System.SysUtils, System.Classes, FMX.Grid, FMX.Controls, FMX.TreeView, FMX.Forms,

  ddPlugin_TLB,

  ddType, ddUTA, ddBrowse;

type
  TD = class(TForm)
    procedure FormCreate(Sender: TObject);
  private
    { Private declarations }

  public
    { Public declarations }
    fStromecek: TTreeView;

    procedure OpenAllQuerys;
  end;

var
  D: TD;

implementation

{ %CLASSGROUP 'System.Classes.TPersistent' }

{$R *.fmx}

uses sqProc, sqDistr1, ddTabulka, ddMain, dtHGlob, sqVazby, sqView,
  sqString, AcunaMatataUnit;

{ TD }

procedure TD.FormCreate(Sender: TObject);
begin
  fStromecek := AcunaMatataForm.TreeView1;
  OpenAllQuerys;
end;

procedure TD.OpenAllQuerys;
var
  iBrow: TBrowse;
  aBrowse: TVychoziNastaveniBrowse;
  aTreeItem, aTreeItemChild: TTreeViewItem;

  iTtypBrowse: TTypBrowse;
begin
  // inicializace dat

  for iTtypBrowse := tbUcto to High(TTypBrowse) do
  begin
    aTreeItem := TTreeViewItem.Create(self);

    aTreeItem.Text := sqlCtiOznam(CTypBrowse_ModulHeliosu[iTtypBrowse].Txt);
    if aTreeItem.Text = '' then
      aTreeItem.Text := sqlCtiOznam(GModulyHeliosu[CTypBrowse_ModulHeliosu[iTtypBrowse].M].NazevTxt);

    aTreeItem.Parent := fStromecek;

    for iBrow := Low(TBrowse) to High(TBrowse) do
    begin
      aBrowse := SeznamVychozichNastaveniBrowse[iBrow];

      if (aBrowse.Typ = iTtypBrowse) and (aBrowse.Predek = bZadny) then
      begin
        aTreeItemChild := TTreeViewItem.Create(self);

        aTreeItemChild.Text := sqlCtiOznam(aBrowse.JmenoTxt);
        aTreeItemChild.Tag := Ord(iBrow);

        aTreeItemChild.Parent := aTreeItem;
      end;

    end;
  end;
end;

{ --------------------------------------------------------------------------- }

end.
