
{$I Dac.inc}
{$J+}

unit DacClx;

interface

uses
  Classes, DB, TypInfo,
{$IFDEF MSWINDOWS}
  Windows, Registry, SysUtils,
{$ENDIF}
  QForms, QControls,  QStdCtrls, QExtCtrls, QGraphics,
  DBAccess, DASQLMonitor, MemData;

{$I DacGui.inc}

