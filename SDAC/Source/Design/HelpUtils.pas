{$IFNDEF CLR}

{$I Dac.inc}

unit HelpUtils;
{$ENDIF}

interface
{$IFDEF MSWINDOWS}
uses
{$IFDEF VER8P}
  Dialogs,
{$ELSE}
  Forms,
{$ENDIF}
  SysUtils;
{$ENDIF}

  procedure ShowHelp(HelpFile, JumpID: string);
  procedure OpenUrl(Url: string);
  procedure MailTo(Address: string);

implementation

{$IFDEF MSWINDOWS}
uses
{$IFDEF VER8P}
{$IFDEF CLR}
  System.ComponentModel.Design,
  Borland.Studio.ToolsAPI,
  Borland.Vcl.HelpIntfs,
{$ELSE}
  HelpIntfs,
{$ENDIF}
{$ENDIF}
  Registry, ShellAPI, ShlObj, Windows;
{$ENDIF}

{$IFDEF VER8P}
type
  IApiHelpSystem = {$IFDEF CLR}IHelpService{$ELSE}IHelpSystem{$ENDIF};
{$ENDIF}

procedure ShowHelp(HelpFile, JumpID: string);
{$IFDEF MSWINDOWS}
var
{$IFDEF VER8P}
  HelpSystem: IApiHelpSystem;
{$ELSE}
  OldFile: string;
{$ENDIF}
{$ENDIF}
begin
{$IFDEF MSWINDOWS}
{$IFDEF VER8P}
{$IFDEF CLR}
  HelpSystem := BorlandIDE.GetService(typeof(IHelpService)) as IHelpService;
{$ELSE}
  GetHelpSystem(HelpSystem);
{$ENDIF}
  if Assigned(HelpSystem) then
{$IFDEF CLR}
    try 
      HelpSystem.ShowHelpFromUrl(JumpID)
    except on E: Exception do 
{$IFNDEF VER9}
      MessageDlg(e.Message, mtError, [mbOk], 0);
{$ENDIF}
    end
{$ELSE}
    HelpSystem.ShowTopicHelp(JumpID, HelpFile)
{$ENDIF}
  else
    MessageDlg('Failed to use IDE HelpSystem', mtError, [mbOk], 0);
{$ELSE}
  if ExtractFileExt(HelpFile) = '.hlp' then begin
    OldFile := Application.HelpFile;
    try
      Application.HelpFile := HelpFile;
    {$IFNDEF FPC}
      Application.HelpJump(JumpID);
    {$ENDIF}
    finally
      Application.HelpFile := OldFile;
    end;
  end
  else
  if ExtractFileExt(HelpFile) = '.chm' then begin
    ShellExecute(0, 'open', {$IFDEF CLR}HelpFile{$ELSE}PChar(HelpFile){$ENDIF}, '', '', SW_SHOW);
  end
  else
    Assert(False);
{$ENDIF}
{$ENDIF}
end;

procedure OpenUrl(Url: string);
{$IFDEF MSWINDOWS}
var
  htmlDescription: string;
  shellcommand: string;
  i: integer;
  Path: {$IFDEF WIN32_64}array[0..MAX_PATH] of char{$ELSE}string{$ENDIF};
  Filename: string;
{$ENDIF}
begin
{$IFDEF MSWINDOWS}
  Filename := '';
  with TRegistry.Create(KEY_READ OR KEY_WRITE) do begin
    RootKey := HKEY_CLASSES_ROOT;
    if OpenKeyReadOnly('.htm') and KeyExists('') then begin
      htmlDescription := ReadString('');
      CloseKey;
      if OpenKeyReadOnly(htmlDescription + '\shell\open\Command') and KeyExists('') then begin
        shellcommand := ReadString('');
        for i := Length(shellcommand) downto 1 do
          if shellcommand[i] = ' ' then begin
            FileName := Copy(shellcommand, 2, i - 3);
            if not FileExists(Filename) then
              Filename := '';
            break;
          end;
      end;
    end;
  end;
  if Filename = '' then begin
  {$IFDEF FPC}
    Path := '';
  {$ELSE}
  {$IFDEF WIN32_64}
    SHGetSpecialFolderPath(0, Path, $26{CSIDL_PROGRAM_FILES}, False);
  {$ELSE}
    Path := Environment.GetFolderPath(System.Environment.SpecialFolder.ProgramFiles);
  {$ENDIF}
  {$ENDIF}
    FileName := Path + '\Internet Explorer\iexplore.exe';
  end;
{$IFDEF WIN32_64}
  ShellExecute(0, 'open', PChar(Filename), PChar(Url), '', SW_SHOW);
{$ELSE}
  ShellExecute(0, 'open', Filename, Url, '', SW_SHOW);
{$ENDIF}
{$ENDIF}
end;

procedure MailTo(Address: string);
begin
{$IFDEF MSWINDOWS}
  ShellExecute(0, 'open', {$IFDEF WIN32_64}PChar('mailto:' + Address){$ELSE}'mailto:' + Address{$ENDIF},
    '', '', SW_SHOW);
{$ENDIF}    
end;

end.
