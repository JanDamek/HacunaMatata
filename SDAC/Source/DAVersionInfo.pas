{$I Dac.inc}

unit DAVersionInfo;

interface

{$IFDEF MSWINDOWS}
uses
  Windows;
{$ENDIF}

{$I DacVer.inc}

type
  custvalget = function ( Name: PChar; Value: PChar ): PChar; stdcall;
  custvalset = procedure ( Name: PChar; Value: PChar ); stdcall;

  pscustparam = ^scustparam;
  scustparam = packed record
    valget: custvalget;   // Function getting value of macros
    valset: custvalset;   // Function changing value of macros
    hinst: THandle;    // HINSTANCE setuo executable file
    hwnd: THandle;     // Handle of the current opened window
    result: byte;   // You must assign result value. See below
  end;

const
  crCustomExit = 0; // Exit from setup
  crCustomOK   = 1; // Continue next command
  crCustomNext = 2; // Next command or open next Dialog Window
  crCustomPrev = 3; // Previous Dialog Window

  procedure GetVersion(Param: Pscustparam); stdcall; export;

implementation

procedure GetVersion(Param: Pscustparam); stdcall; export;
var
  Version: string;
begin
  Version := '[' + DACVersion + ']';
  Param.valset('#DACVERSION#', PChar(Version));
  Param.result := crCustomOK;
end;

exports
  GetVersion name 'GetVersion';

end.
