library FMDefEdit;

uses
  SysUtils,
  Classes,
  Windows,
  ComServ,
  ComObj,
  ddPlugin_TLB,
  ShellAPI,
  Variants,
  FMForm;

const
  C_PluginName = 'FMDefEdit';
  C_ClassName = 'DefEdit';
  C_ProgID = C_PluginName + '.' + C_ClassName;

  // !!! POKUD DĚLÁM NOVÝ PLUGIN, JE TŘEBA TENTO GUID ZMĚNIT (CTRL+SHIFT+G V DELPHI IDE) !!!
  C_GUID: TGUID = '{FDBE8FF0-B08A-444B-B42D-A3EEEAEF6CE6}';

function PluginGetSysAndClassName(Vysl: PAnsiChar): DWORD; stdcall;
begin
  Result := Length(C_ProgID);
  if Assigned(Vysl) then
    StrPCopy(Vysl, C_ProgID);
end;

exports
  DllGetClassObject,
  DllCanUnloadNow,
  DllRegisterServer,
  DllUnregisterServer,
  PluginGetSysAndClassName;

{$R *.RES}

type
  TDefEdit = class(TComObject, IHePlugin)
  private
    procedure Run(const Helios: IHelios); safecall;
  end;

procedure TDefEdit.Run(const Helios: IHelios);
begin
  if Helios.ExtKomID = -1 then
  begin
    // Instalace
    Helios.ExecSQL
      ('DECLARE @GUID BINARY(16),@Popis VARCHAR(255),@UP VARCHAR(255),@Parametry VARCHAR(255),@IdBrowse INT,@Poznamka VARCHAR(8000),@Autor VARCHAR(255),@DatPorizeni DATETIME,@Zmenil VARCHAR(255),@DatZmeny DATETIME,@ParamHe INT,@Typ INT,@BidNavBrowse INT,'
      + '@TextNavBrowse INT,@ZobrUkonceni BIT,@VerejnaAkce BIT,@TextBrowse INT,@CyklickyEditor BIT,@SelectoTvurceVlastnikId BIT,@VstupneID VARCHAR(255),@TechnickyPopis VARCHAR(8000),@Napoveda VARCHAR(255),@Hotkey INT,'
      + '@Skupina VARCHAR(30),@NazevPodmenu VARCHAR(255),@PoradiVPodmenu VARCHAR(255)'#13
      + 'SELECT @Skupina='''',@NazevPodmenu=' + QuotedStr('') + ''#13 +
      'SELECT @Popis=' + QuotedStr('Editace FMDefEdit...') + ',@UP=' +
      QuotedStr(C_ProgID) + ',@Parametry=' + QuotedStr('') + ',@IdBrowse=' +
      IntToStr(240) + //240=Ukoly
      ',@Poznamka=NULL,@Autor=''tz'',@DatPorizeni=''20111221'',@Zmenil=''tz'',@DatZmeny=GETDATE(),@ParamHe='
      + IntToStr(0) + ',@Typ=' + IntToStr(3) +
      ',@BidNavBrowse=NULL,@TextNavBrowse=NULL,@ZobrUkonceni=0,@VerejnaAkce=1,@TextBrowse=@IdBrowse,@CyklickyEditor=0,@SelectoTvurceVlastnikId=0,@VstupneID=NULL,@TechnickyPopis=NULL,@Napoveda=NULL,@Hotkey='
      + IntToStr(0) //0=bez hotkey
       + ',' + '@GUID=CAST(' + QuotedStr(GUIDToString(C_GUID)) +
      ' AS UNIQUEIDENTIFIER),@PoradiVPodmenu=' + IntToStr(1) + #13 +
      'IF EXISTS(SELECT * FROM dbo.TabExtKom WHERE GUID=@GUID)UPDATE dbo.TabExtKom SET Popis=@Popis,UP=@UP,Parametry=@Parametry,IdBrowse=@IdBrowse,Poznamka=@Poznamka,Autor=@Autor,'
      + 'DatPorizeni=@DatPorizeni,Zmenil=@Zmenil,DatZmeny=@DatZmeny,ParamHe=@ParamHe,Typ=@Typ,'
      + 'BidNavBrowse=@BidNavBrowse,TextNavBrowse=@TextNavBrowse,ZobrUkonceni=@ZobrUkonceni,VerejnaAkce=@VerejnaAkce,TextBrowse=@TextBrowse,CyklickyEditor=@CyklickyEditor,SelectoTvurceVlastnikId='
      + '@SelectoTvurceVlastnikId,VstupneID=@VstupneID,TechnickyPopis=@TechnickyPopis,Napoveda=@Napoveda,'
      + 'Hotkey=@Hotkey,Skupina=@Skupina,NazevPodmenu=@NazevPodmenu,PoradiVPodmenu=@PoradiVPodmenu'
      + ' WHERE GUID=@GUID'#13 +
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

//  ShowWindow(Helios.MainApplicationHandle, SW_MINIMIZE);
  try
    with TFMFrm.Create(nil) do // muj demo form
      try
        fHelios := Helios;
        OpenQuerys;
        ShowModal;
      finally
        Free;
      end;
  finally
    ShowWindow(Helios.MainApplicationHandle, SW_RESTORE);
  end;
end;

BEGIN
  // !!! POKUD DĚLÁM NOVÝ PLUGIN, JE TŘEBA IDENTIFIKÁTOR 'Exec' ZMĚNIT - JMÉNO COM OBJEKTU PAK BUDE <JMENOPROJEKTU>.IDENTIFIKATOR !!!
  TComObjectFactory.Create(ComServer, TDefEdit, C_GUID, C_ClassName, '',
    ciMultiInstance, tmApartment);

END.
