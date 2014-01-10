library FMDefEdit;

{$R *.dres}

uses
  System.UITypes,
  System.SysUtils,
  System.Classes,
  FMX.Types,
  FMX.Forms,
  AcunaMatataUnit in 'AcunaMatataUnit.pas' {AcunaMatataForm},
  System.Win.ComServ,
  System.Win.ComObj,
  Winapi.Windows,
  ddPlugin_TLB,
  DefEditUnit,
  DefEditAttrUnit,
  Forms in 'Forms.pas',
  Graphics in 'Graphics.pas',
  sqProc in 'sqProc.pas',
  sqBrowse in 'sqBrowse.pas',
  sqView in 'sqView.pas',
  sqEasyHash in 'sqEasyHash.pas',
  sqDynVazba in 'sqDynVazba.pas',
  sqVazby in 'sqVazby.pas',
  Pom in 'Pom.pas',
  sqBrForm in 'sqBrForm.pas',
  sqJazyk in 'sqJazyk.pas',
  ddMain in 'ddMain.pas',
  EditorUnit in 'EditorUnit.pas',
  deType in 'deType.pas',
  deUkoly in 'deUkoly.pas',
  deEditory in 'deEditory.pas',
  sqString in 'sqString.pas',
  DataModulUnit in 'DataModulUnit.pas' {D},
  sqAlter in 'sqAlter.pas';

const
  C_PluginName = 'FMDefEdit';
  C_ClassName = 'DefEdit';
  C_ProgID = C_PluginName + '.' + C_ClassName;

  // !!! POKUD DĚLÁM NOVÝ PLUGIN, JE TŘEBA TENTO GUID ZMĚNIT (CTRL+SHIFT+G V DELPHI IDE) !!!
  C_GUID: TGUID = '{FDBE8FF0-B08A-444B-B42D-A3EEEAEF6CE6}';

function PluginGetSysAndClassName(Vysl: PAnsiChar): LongWord; stdcall;
begin
  Result := Length(C_ProgID);
  if Assigned(Vysl) then
    System.SysUtils.StrPCopy(Vysl, C_ProgID);
end;

exports
  DllGetClassObject,
  DllCanUnloadNow,
  DllRegisterServer,
  DllUnregisterServer,
  PluginGetSysAndClassName;

{$R *.RES}

type
  THacunaMatata = class(TComObject, IHePlugin)
  private
    procedure Run(const Helios: IHelios); safecall;
  end;

type
  TBitmapAccess = class(FMX.Types.TBitmap);

procedure THacunaMatata.Run(const Helios: IHelios);
begin
  if Helios.ExtKomID = -1 then
  begin
    // Instalace
    Helios.ExecSQL
      ('DECLARE @GUID BINARY(16),@Popis VARCHAR(255),@UP VARCHAR(255),@Parametry VARCHAR(255),@IdBrowse INT,@Poznamka VARCHAR(8000),@Autor VARCHAR(255),@DatPorizeni DATETIME,@Zmenil VARCHAR(255),@DatZmeny DATETIME,@ParamHe INT,@Typ INT,@BidNavBrowse INT,'
      + '@TextNavBrowse INT,@ZobrUkonceni BIT,@VerejnaAkce BIT,@TextBrowse INT,@CyklickyEditor BIT,@SelectoTvurceVlastnikId BIT,@VstupneID VARCHAR(255),@TechnickyPopis VARCHAR(8000),@Napoveda VARCHAR(255),@Hotkey INT,'
      + '@Skupina VARCHAR(30),@NazevPodmenu VARCHAR(255),@PoradiVPodmenu VARCHAR(255)'#13 +
      'SELECT @Skupina='''',@NazevPodmenu=' + QuotedStr('') + ''#13 + 'SELECT @Popis=' +
      QuotedStr('Editace FMDefEdit...') + ',@UP=' + QuotedStr(C_ProgID) + ',@Parametry=' + QuotedStr('') + ',@IdBrowse='
      + IntToStr(240) + // 240=Ukoly
      ',@Poznamka=NULL,@Autor=''tz'',@DatPorizeni=''20111221'',@Zmenil=''tz'',@DatZmeny=GETDATE(),@ParamHe=' +
      IntToStr(0) + ',@Typ=' + IntToStr(3) +
      ',@BidNavBrowse=NULL,@TextNavBrowse=NULL,@ZobrUkonceni=0,@VerejnaAkce=1,@TextBrowse=@IdBrowse,@CyklickyEditor=0,@SelectoTvurceVlastnikId=0,@VstupneID=NULL,@TechnickyPopis=NULL,@Napoveda=NULL,@Hotkey='
      + IntToStr(0) // 0=bez hotkey
      + ',' + '@GUID=CAST(' + QuotedStr(GUIDToString(C_GUID)) + ' AS UNIQUEIDENTIFIER),@PoradiVPodmenu=' + IntToStr(1) +
      #13 + 'IF EXISTS(SELECT * FROM dbo.TabExtKom WHERE GUID=@GUID)UPDATE dbo.TabExtKom SET Popis=@Popis,UP=@UP,Parametry=@Parametry,IdBrowse=@IdBrowse,Poznamka=@Poznamka,Autor=@Autor,'
      + 'DatPorizeni=@DatPorizeni,Zmenil=@Zmenil,DatZmeny=@DatZmeny,ParamHe=@ParamHe,Typ=@Typ,' +
      'BidNavBrowse=@BidNavBrowse,TextNavBrowse=@TextNavBrowse,ZobrUkonceni=@ZobrUkonceni,VerejnaAkce=@VerejnaAkce,TextBrowse=@TextBrowse,CyklickyEditor=@CyklickyEditor,SelectoTvurceVlastnikId='
      + '@SelectoTvurceVlastnikId,VstupneID=@VstupneID,TechnickyPopis=@TechnickyPopis,Napoveda=@Napoveda,' +
      'Hotkey=@Hotkey,Skupina=@Skupina,NazevPodmenu=@NazevPodmenu,PoradiVPodmenu=@PoradiVPodmenu' +
      ' WHERE GUID=@GUID'#13 +
      'ELSE INSERT dbo.TabExtKom(GUID,Popis,UP,Parametry,IdBrowse,Poznamka,Autor,DatPorizeni,Zmenil,DatZmeny,ParamHe,Typ,BidNavBrowse,TextNavBrowse,ZobrUkonceni,VerejnaAkce,TextBrowse,CyklickyEditor,SelectoTvurceVlastnikId,VstupneID,TechnickyPopis,'
      + 'Napoveda,Hotkey,Skupina,NazevPodmenu,PoradiVPodmenu)VALUES(@GUID,@Popis,@UP,@Parametry,@IdBrowse,@Poznamka,@Autor,@DatPorizeni,@Zmenil,@DatZmeny,@ParamHe,@Typ,@BidNavBrowse,@TextNavBrowse,@ZobrUkonceni,@VerejnaAkce,@TextBrowse,'
      + '@CyklickyEditor,@SelectoTvurceVlastnikId,@VstupneID,@TechnickyPopis,@Napoveda,@Hotkey,@Skupina,@NazevPodmenu,@PoradiVPodmenu)');
    Exit;
  end;

  if Helios.ExtKomID = -2 then
  begin
    // About
    Helios.Info('Delphi XE2 FireMonkey demo plugin.');
    Exit;
  end;

  fHeliosGlobal := Helios;

  // Test SQL pro ukladani editoru
  DefEdit := TDefEdit.Create(Helios);
  DefAttr := TDefEditAttr.Create(Helios);

  AcunaMatataForm := TAcunaMatataForm.Create(Application);
  D := TD.Create(AcunaMatataForm);

  AcunaMatataForm.ShowModal;

  AcunaMatataForm.Free;

  DefAttr.Free;
  DefEdit.Free;

  ShowWindow(Helios.MainApplicationHandle, SW_RESTORE);
end;

begin
  // !!! POKUD DĚLÁM NOVÝ PLUGIN, JE TŘEBA IDENTIFIKÁTOR 'Exec' ZMĚNIT - JMÉNO COM OBJEKTU PAK BUDE <JMENOPROJEKTU>.IDENTIFIKATOR !!!
  TComObjectFactory.Create(ComServer, THacunaMatata, C_GUID, C_ClassName, '', ciMultiInstance, tmApartment);
end.
