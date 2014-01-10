unit EditorUnit;

interface

uses ddType, ddMain, ddBrowse, ddPlugin_TLB, deType, FMX.Types;

procedure OpenEdit(aBrowse: TVychoziNastaveniBrowse; GUID: String; fID: String; fIDVal: Variant; fHelios: IHelios);

implementation

uses Variants, System.Classes, FMX.Forms, EditFormUnit, DefEditUnit;

procedure OpenEdit(aBrowse: TVychoziNastaveniBrowse; GUID: String; fID: String; fIDVal: Variant; fHelios: IHelios);
var
  s: String;
begin
  s := JmenoTabulky(aBrowse.HlavniTabulka) + '.' + fID + ' = ' + VarToStr(fIDVal);

  if DefEdit.findGUID(GUID) then
    with TEditForm.CreateForm(Application, aBrowse.BID, GUID, fID, fIDVal, fHelios) as TCustomForm do
    begin
      ShowModal;
      Free;
    end
  else
    fHelios.EditBrowse(aBrowse.BID, s);
end;

end.
