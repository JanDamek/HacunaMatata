//////////////////////////////////////////////////
//  DB Access Components
//  Copyright © 1998-2012 Devart. All right reserved.
//  DB Access
//////////////////////////////////////////////////

{$IFNDEF CLR}

{$I Dac.inc}

unit DADumpProgress;
{$ENDIF}

interface

uses
  SysUtils, Classes, 
{$IFDEF MSWINDOWS}
  Windows, Messages,
{$ENDIF}
  Graphics, Controls, Forms, Dialogs,
  ComCtrls, Grids, DBGrids, DBCtrls, Buttons, ExtCtrls, StdCtrls,
{$IFDEF FPC}
  LResources,
{$ENDIF}
  CRTypes, DBAccess, DADump;

type
  TDADumpProgressForm = class(TForm)
    Label1: TLabel;
    BitBtn1: TBitBtn;
    ProgressBar1: TProgressBar;
    ProgressBar2: TProgressBar;
    procedure DADumpBackupProgress(Sender: TObject; TableName: _string;
      TableNum, TableCount, Percent: Integer);
    procedure DADumpRestoreProgress(Sender: TObject; Percent: Integer);
    procedure FormActivate(Sender: TObject);
    procedure BitBtn1Click(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
  protected
    IsBackup: boolean;
    WaitForTerminate: boolean;
    InProgress: boolean;
    procedure Process;

  public
    DADump: TDADump;

    procedure Backup;
    procedure Restore;
  end;

implementation

{$IFNDEF FPC}
{$IFDEF IDE}
{$R *.dfm}
{$ELSE}
{$R DADumpProgress.dfm}
{$ENDIF}
{$ELSE}
{$R *.lfm}
{$ENDIF}

{ TDADumpProgressForm }

procedure TDADumpProgressForm.Backup;
var
  OldProgress: TDABackupProgressEvent;
begin
  Assert(DADump <> nil);
  OldProgress := DADump.OnBackupProgress;
  try
    Label1.Caption := 'Backup';
    IsBackup := True;
    DADump.OnBackupProgress := DADumpBackupProgress;

    ShowModal;
  finally
    DADump.OnBackupProgress := OldProgress;
  end;
end;

procedure TDADumpProgressForm.Restore;
var
  OldProgress: TDARestoreProgressEvent;

begin
  Assert(DADump <> nil);
  OldProgress := DADump.OnRestoreProgress;
  try
    Label1.Caption := 'Restore';
    IsBackup := False;
    DADump.OnRestoreProgress := DADumpRestoreProgress;
    ShowModal;
  finally
    DADump.OnRestoreProgress := OldProgress;
  end;
end;

procedure TDADumpProgressForm.DADumpBackupProgress(Sender: TObject;
  TableName: _string; TableNum, TableCount, Percent: Integer);
begin
  if WaitForTerminate then
    SysUtils.Abort;

  Label1.Caption := 'Backup ' + TableName;
  ProgressBar1.Max := TableCount;
  ProgressBar1.Position := TableNum;
  ProgressBar2.Position := Percent;

  Update;
  Application.ProcessMessages;
 end;

procedure TDADumpProgressForm.DADumpRestoreProgress(Sender: TObject;
  Percent: Integer);
begin
  if WaitForTerminate then
    SysUtils.Abort;

  ProgressBar2.Position := Percent;

  Update;
  Application.ProcessMessages;
end;

procedure TDADumpProgressForm.Process;
begin
  ProgressBar1.Enabled := IsBackup;

  WaitForTerminate := False;
  InProgress := True;
  Cursor := crSQLWait;
  try
    try
      if IsBackup then
        DADump.Backup
      else
        DADump.Restore;
    except
      on E: Exception do
        Application.ShowException(E);
    end;
  finally
    InProgress := False;
    Cursor := crDefault;
  {$IFDEF MSWINDOWS}
    PostMessage(Handle, WM_CLOSE, 0, 0);
  {$ELSE}
    Close;
  {$ENDIF}
  end;
end;

procedure TDADumpProgressForm.FormActivate(Sender: TObject);
begin
  Process;
end;

procedure TDADumpProgressForm.BitBtn1Click(Sender: TObject);
begin
  WaitForTerminate := True;
  Close;
end;

procedure TDADumpProgressForm.FormClose(Sender: TObject;
  var Action: TCloseAction);
begin
  WaitForTerminate := True;
  while InProgress do
    Sleep(300);
end;

end.
