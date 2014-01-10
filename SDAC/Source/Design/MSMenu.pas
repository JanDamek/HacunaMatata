
//////////////////////////////////////////////////
//  SQL Server Data Access Components
//  Copyright © 2001-2012 Devart. All right reserved.
//  SDAC IDE Menu
//////////////////////////////////////////////////

{$IFNDEF CLR}

{$I Sdac.inc}

unit MSMenu;
{$ENDIF}
interface

{$IFDEF VER7P}
  {$WARN UNIT_DEPRECATED OFF}
{$ENDIF}

uses
  CRTypes, DAMenu, Windows;

type
  TMSMenu = class (TDAProductMenu)
  private
    FEmFile: string;
    FQaFile: string;
    FMsFile: string;

    procedure EnterpriseManagerItemClick(Sender: TDAMenuClickSender{$IFDEF CLR}; E: EventArgs{$ENDIF});
    procedure QueryAnalyzerItemItemClick(Sender: TDAMenuClickSender{$IFDEF CLR}; E: EventArgs{$ENDIF});
    procedure ManagementStudioItemItemClick(Sender: TDAMenuClickSender{$IFDEF CLR}; E: EventArgs{$ENDIF});

    procedure HomePageItemClick(Sender: TDAMenuClickSender{$IFDEF CLR}; E: EventArgs{$ENDIF});
    procedure SdacPageItemClick(Sender: TDAMenuClickSender{$IFDEF CLR}; E: EventArgs{$ENDIF});
    procedure AboutItemClick(Sender: TDAMenuClickSender{$IFDEF CLR}; E: EventArgs{$ENDIF});
    procedure DBMonitorItemClick(Sender: TDAMenuClickSender{$IFDEF CLR}; E: EventArgs{$ENDIF});
    procedure DBMonitorPageItemClick(Sender: TDAMenuClickSender{$IFDEF CLR}; E: EventArgs{$ENDIF});
  public
    constructor Create;
    
    function AddItems(Instance: HINST): boolean; override;
  end;

var
  Menu: TMSMenu;

implementation

uses
{$IFDEF CLR}
  WinUtils,
{$ELSE}
  ToolIntf, CLRClasses,
{$ENDIF}
  Classes, Registry, MSDesign, 
  SysUtils, Forms, ShellApi, SdacAbout, DBMonitorClient, HelpUtils;

resourcestring
  sCRMenuName = 'DevartMenuSdac';
  sSDACMenu = '&SDAC';

  sEmItemCaption = 'Enterprise Manager';
  sEmItemName = 'SDACEmItem';
  sQaItemCaption = 'Query Analyzer';
  sQaItemName = 'SDACQaItem';
  SMsItemCaption = 'Management Studio';
  sMsItemName = 'SDACMsItem';
  sHelpItemCaption = 'SDAC Help';
  sHelpItemName = 'SDACHelpItem';
  sAboutItemCaption = 'About SDAC...';
{$IFDEF CLR}
  sAboutItemName = 'CRMSAboutItemCLR';
{$ELSE}
  sAboutItemName = 'CRMSAboutItemWin32';
{$ENDIF}
//sMSDesSeparator = 'MSDesSeparator';
  sHomePageCaption = 'Devart Home Page';
  sHomePageName = 'CRSDACHomePageItem';
  sFAQCaption = 'SDAC FAQ';
  sFAQName = 'CRSDACFAQItem';
  sSdacPageCaption = 'SDAC Home Page';
  sSdacPageName = 'CRSDACPageItem';
  sDBMonitorItemCaption = 'DBMonitor';
  sDBMonitorItemName = 'SDACDBMonitorItem';
  sDBMonitorPageCaption = 'Download DBMonitor';
  sDBMonitorPageName = 'SDACDBMonitorPageItem';

{ TMSMenu }

constructor TMSMenu.Create;
begin
  inherited Create (sCRMenuName, sAboutItemCaption, sAboutItemName, sSDACMenu);
  FAboutClickEvent := AboutItemClick;
end;

function TMSMenu.AddItems(Instance: HINST): boolean;
  function IsEmAvaible: boolean;
  type
    TRegKeyString = record
      Root: HKEY;
      Path, KeyName: string;
      AdditionalPath: _string;
    end;
  const
    KeyPaths: array [0..4] of TRegKeyString =
      ((Root: HKEY_CLASSES_ROOT; Path: 'TypeLib\{10010001-E260-11CF-AE68-00AA004A34D5}\8.0\HELPDIR'; KeyName: ''; AdditionalPath: ''),
       (Root: HKEY_LOCAL_MACHINE; Path: 'SOFTWARE\Microsoft\Windows\CurrentVersion\App Paths\sqlservr.exe'; KeyName: 'Path'; AdditionalPath: ''),
       (Root: HKEY_LOCAL_MACHINE; Path: 'SOFTWARE\Microsoft\Windows\CurrentVersion\App Paths\sqlservr.exe'; KeyName: 'Path'; AdditionalPath: '\Binn'),
       (Root: HKEY_CLASSES_ROOT; Path: 'TypeLib\{10010001-EB1C-11CF-AE6E-00AA004A34D5}\2.0\HELPDIR'; KeyName: ''; AdditionalPath: ''),
       (Root: HKEY_CLASSES_ROOT; Path: 'TypeLib\{10010200-740B-11D0-AE7B-00AA004A34D5}\2.0\HELPDIR'; KeyName: ''; AdditionalPath: ''));
  var
    Reg: TRegistry;
    i: integer;
  begin
    Result := False;
    FEmFile := '';
    Reg := TRegistry.Create{$IFDEF VER5P}(KEY_READ){$ENDIF};
    try
      for i := Low(KeyPaths) to High(KeyPaths) do begin
        Reg.RootKey := KeyPaths[i].Root;
        if Reg.OpenKeyReadOnly(KeyPaths[i].Path) then begin
          FEmFile := Reg.ReadString(KeyPaths[i].KeyName);
          Reg.CloseKey;
          if FEmFile <> '' then begin
            FEmFile := FEmFile + KeyPaths[i].AdditionalPath + '\SQL Server Enterprise Manager.MSC';
            Result := FileExists(FEmFile);
            if Result then
              Break;
          end;
        end;
      end;
    finally
      Reg.Free;
    end;
  end;

  function SearchPathByFile(const FileName: string): boolean;
  var
    p: IntPtr;
  begin
  {$IFDEF CLR}
    Result := SearchPath(nil, FileName, nil, 0, nil, p) <> 0;
  {$ELSE}
    Result := SearchPath(nil, @FileName[1], nil, 0, nil, PChar(p)) <> 0;
  {$ENDIF}
  end;

  function IsQaAvaible: boolean;
  begin
    FQaFile := 'isqlw.exe';
    Result := SearchPathByFile(FQaFile);
  end;

  function IsMsAvaible: boolean;
  begin
    FMsFile := 'ssms.exe';
    Result := SearchPathByFile(FMsFile);

    if not Result then begin
      FMsFile := 'sqlwb.exe';
      Result := SearchPathByFile(FMsFile);
    end;
  end;

var
  QaAvaible, EmAvaible, MsAvaible: boolean;

begin
  Result := inherited AddItems(Instance);
  if not Result then
    Exit;

  with FCRMenu.SubMenu do begin
    EmAvaible := IsEmAvaible;
    QaAvaible := IsQaAvaible;
    MsAvaible := IsMsAvaible;


    if EmAvaible or QaAvaible or MsAvaible then begin
      if EmAvaible then
        Add(sEmItemCaption, sEmItemName, EnterpriseManagerItemClick);
      if QaAvaible then
        Add(sQaItemCaption, sQaItemName, QueryAnalyzerItemItemClick);
      if MsAvaible then
        Add(SMsItemCaption, sMsItemName, ManagementStudioItemItemClick);

      AddSeparator;
    end;

    if HasMonitor then begin
      Add(sDBMonitorItemCaption, sDBMonitorItemName, DBMonitorItemClick);
    end;

    AddWizards;
    AddSeparator;

    AddHelp(sHelpItemCaption, sHelpItemName, 'Sdac');
    AddFAQ(sFAQCaption, sFAQName, 'Sdac');
    AddSeparator;

    Add(sHomePageCaption, sHomePageName, HomePageItemClick);
    Add(sSdacPageCaption, sSdacPageName, SdacPageItemClick);
    Add(sDBMonitorPageCaption, sDBMonitorPageName, DBMonitorPageItemClick);
    AddSeparator;
    AddAbout;
  end;
end;

procedure TMSMenu.DBMonitorItemClick(Sender: TDAMenuClickSender{$IFDEF CLR}; E: EventArgs{$ENDIF});
begin
  ShellExecute(0, 'open', PChar(WhereMonitor), '', '', SW_SHOW);
end;

procedure TMSMenu.DBMonitorPageItemClick(Sender: TDAMenuClickSender{$IFDEF CLR}; E: EventArgs{$ENDIF});
begin
  OpenUrl('http://www.devart.com/dbmonitor/dbmon3.exe');
end;

procedure TMSMenu.EnterpriseManagerItemClick(Sender: TDAMenuClickSender{$IFDEF CLR}; E: EventArgs{$ENDIF});
var
  i: integer;
begin
{$IFDEF CLR}
  i := ShellExecute(0, '', FEmFile, '', '', SW_SHOWNORMAL);
{$ELSE}
  i := ShellExecute(0, nil, @FEmFile[1], nil, nil, SW_SHOWNORMAL);
{$ENDIF}
  if i <= 32 then
    TAPIMenuItem(Sender).Enabled := False;
end;

procedure TMSMenu.QueryAnalyzerItemItemClick(Sender: TDAMenuClickSender{$IFDEF CLR}; E: EventArgs{$ENDIF});
var
  i: integer;
  s: string;
begin
  s := '-1';
{$IFDEF CLR}
  i := ShellExecute(0, '', FQaFile, s, '', SW_SHOWNORMAL);
{$ELSE}
  i := ShellExecute(0, nil, @FQaFile[1], @s[1], nil, SW_SHOWNORMAL);
{$ENDIF}
  if i <= 32 then
    TAPIMenuItem(Sender).Enabled := False;
end;

procedure TMSMenu.ManagementStudioItemItemClick(Sender: TDAMenuClickSender{$IFDEF CLR}; E: EventArgs{$ENDIF});
var
  i: integer;
begin
{$IFDEF CLR}
  i := ShellExecute(0, '', FMsFile, nil, '', SW_SHOWNORMAL);
{$ELSE}
  i := ShellExecute(0, nil, @FMsFile[1], nil, nil, SW_SHOWNORMAL);
{$ENDIF}
  if i <= 32 then
    TAPIMenuItem(Sender).Enabled := False;
end;

procedure TMSMenu.HomePageItemClick(Sender: TDAMenuClickSender{$IFDEF CLR}; E: EventArgs{$ENDIF});
begin
  OpenUrl('http://www.devart.com');
end;

procedure TMSMenu.SdacPageItemClick(Sender: TDAMenuClickSender{$IFDEF CLR}; E: EventArgs{$ENDIF});
begin
  OpenUrl('http://www.devart.com/sdac');
end;

procedure TMSMenu.AboutItemClick(Sender: TDAMenuClickSender{$IFDEF CLR}; E: EventArgs{$ENDIF});
begin
  ShowAbout;
end;

initialization
  Menu := TMSMenu.Create;
finalization
  Menu.Free;
end.
