{$B-}

{$DEFINE DAC600}
{$DEFINE DAC650}

{$IFDEF FPC}
  {$MODE DELPHI}
  {$ASMMODE INTEL}
  {$INTERFACES COM}
  {$DEFINE VER5P}
  {$DEFINE VER6P}
{$ENDIF}

{$IFDEF WIN32}
  {$DEFINE MSWINDOWS}
  {$DEFINE WIN32_64}
{$ENDIF}
{$IFDEF WIN64}
  {$DEFINE MSWINDOWS}
  {$DEFINE WIN32_64}
{$ENDIF}
{$IFDEF CLR}
  {$DEFINE MSWINDOWS}
{$ENDIF}

{$IFDEF DARWIN}
  {$DEFINE MACOS}
{$ENDIF}

{$IFDEF MSWINDOWS}
  {$IFNDEF CLR}
    {$DEFINE USE_SYNEDIT}
  {$ENDIF}
{$ENDIF}

{$IFDEF CPU64}
  {$DEFINE CPUX64} { assembler for x64 - Lazarus compatibility }
{$ENDIF}
{$IFDEF CPU86}
  {$DEFINE CPUX86} { assembler for x86 - Lazarus compatibility }
{$ENDIF}

{$IFDEF VER120} { Delphi 4 }
  {$DEFINE D4}
  {$DEFINE VER4}

  {$DEFINE VER34}
  {$DEFINE VER4P}
{$ENDIF}

{$IFDEF VER130} { Delphi 5 }
{$IFNDEF BCB}
  {$DEFINE D5}
  {$DEFINE VER5}

  {$DEFINE VER5P}
{$ENDIF}
{$ENDIF}

{$IFDEF VER140} { Delphi 6, Kylix 1, Kylix 2 }
{$IFNDEF BCB}
  {$DEFINE D6}
  {$DEFINE VER6}

  {$DEFINE VER5P}
  {$DEFINE VER6P}
{$ENDIF}
{$ENDIF}

{$IFDEF VER150} { Delphi 7 }
{$IFNDEF BCB}
  {$DEFINE D7}
  {$DEFINE VER7}

  {$DEFINE VER5P}
  {$DEFINE VER6P}
  {$DEFINE VER7P}
{$ENDIF}
{$ENDIF}

{$IFDEF VER160} { Delphi 8 }
  {$DEFINE D8}
  {$DEFINE VER8}

  {$DEFINE VER5P}
  {$DEFINE VER6P}
  {$DEFINE VER7P}
  {$DEFINE VER8P}
{$ENDIF}

{$IFDEF VER170} { Delphi 9 }
  {$DEFINE D9}
  {$DEFINE VER9}

  {$DEFINE VER5P}
  {$DEFINE VER6P}
  {$DEFINE VER7P}
  {$DEFINE VER8P}
  {$DEFINE VER9P}
{$ENDIF}

{$IFDEF VER180} { Delphi 10 }
{$IFNDEF VER185} { Delphi 11 Spacely}
  {$DEFINE D10}
  {$DEFINE VER10}

  {$DEFINE VER5P}
  {$DEFINE VER6P}
  {$DEFINE VER7P}
  {$DEFINE VER8P}
  {$DEFINE VER9P}
  {$DEFINE VER10P}
{$ENDIF}
{$ENDIF}

{$IFDEF VER185} { Delphi 11 Spacely}
  {$DEFINE D11}
{$ENDIF}
{$IFDEF VER190} { Delphi 11 Highlander}
  {$DEFINE D11}
{$ENDIF}

{$IFDEF D11}
  {$DEFINE VER11}

  {$DEFINE VER5P}
  {$DEFINE VER6P}
  {$DEFINE VER7P}
  {$DEFINE VER8P}
  {$DEFINE VER9P}
  {$DEFINE VER10P}
  {$DEFINE VER11P}
{$ENDIF}

{$IFDEF VER200} { Delphi 12 }
  {$DEFINE D12}
  {$DEFINE VER12}

  {$DEFINE VER5P}
  {$DEFINE VER6P}
  {$DEFINE VER7P}
  {$DEFINE VER8P}
  {$DEFINE VER9P}
  {$DEFINE VER10P}
  {$DEFINE VER11P}
  {$DEFINE VER12P}
{$ENDIF}

{$IFDEF VER210} { Delphi 14 }
  {$DEFINE D14}
  {$DEFINE VER14}

  {$DEFINE VER5P}
  {$DEFINE VER6P}
  {$DEFINE VER7P}
  {$DEFINE VER8P}
  {$DEFINE VER9P}
  {$DEFINE VER10P}
  {$DEFINE VER11P}
  {$DEFINE VER12P}
  {$DEFINE VER14P}
{$ENDIF}

{$IFDEF VER220} { Delphi 15 }
  {$DEFINE D15}
  {$DEFINE VER15}

  {$DEFINE VER5P}
  {$DEFINE VER6P}
  {$DEFINE VER7P}
  {$DEFINE VER8P}
  {$DEFINE VER9P}
  {$DEFINE VER10P}
  {$DEFINE VER11P}
  {$DEFINE VER12P}
  {$DEFINE VER14P}
  {$DEFINE VER15P}
{$ENDIF}

{$IFDEF VER230} { Delphi 16 }
  {$DEFINE D16}
  {$DEFINE VER16}

  {$DEFINE VER5P}
  {$DEFINE VER6P}
  {$DEFINE VER7P}
  {$DEFINE VER8P}
  {$DEFINE VER9P}
  {$DEFINE VER10P}
  {$DEFINE VER11P}
  {$DEFINE VER12P}
  {$DEFINE VER14P}
  {$DEFINE VER15P}
  {$DEFINE VER16P}
{$ENDIF}

{$IFDEF VER125} { C++ Builder 4 }
  {$DEFINE BCB}
  {$DEFINE CB4}
  {$DEFINE VER4}

  {$DEFINE VER34}
  {$DEFINE VER4P}
  {$ObjExportAll On}
{$ENDIF}

{$IFDEF VER130} { C++ Builder 5 }
{$IFDEF BCB}
  {$DEFINE CB5}
  {$DEFINE VER5}

  {$DEFINE VER5P}
  {$ObjExportAll On}
{$ENDIF}
{$ENDIF}

{$IFDEF VER140} { C++ Builder 6 }
{$IFDEF BCB}
  {$DEFINE CB6}
  {$DEFINE VER6}

  {$DEFINE VER5P}
  {$DEFINE VER6P}
  {$ObjExportAll On}
{$ENDIF}
{$ENDIF}

{$IFDEF INTERNAL}
{$IFDEF BCB}
  {$DEFINE BDE_SHARED}   // Use TUpdateAction from DBTables
{$ENDIF}
{$ENDIF}

// old

{$IFDEF VER100} { Delphi 3 }
  {$DEFINE D3}
  {$DEFINE D3_CB3}
  {$DEFINE VER34}
  {$DEFINE VER3+}
{$ENDIF}

{$IFDEF VER110} { C++ Builder 3 }
  {$DEFINE CB}
  {$DEFINE CB3}
  {$DEFINE D3_CB3}
  {$DEFINE VER34}
  {$DEFINE VER3+}
  {$ObjExportAll On}
{$ENDIF}

{$IFDEF FPC}
  {$DEFINE USE_INLINE}
{$ENDIF}
{$IFDEF VER9P}
  {$DEFINE USE_INLINE}
{$ENDIF}

{$IFDEF VER7P}
  {$WARN UNSAFE_TYPE OFF}
  {$WARN UNSAFE_CODE OFF}
  {$WARN UNSAFE_CAST OFF}
{$ENDIF}

{$IFDEF VER6P}
  {$WARN SYMBOL_PLATFORM OFF}
  {$WARN SYMBOL_DEPRECATED OFF}
  {$WARN UNIT_PLATFORM OFF}
  {$WARN UNIT_DEPRECATED OFF}
{$ENDIF}

{$IFDEF BCB}
  {$IFNDEF VER6P}
    // CBuilder5 - UTF8 functions is not implemented 
    {$DEFINE UTF8}
  {$ENDIF}
{$ELSE}
  // Delphi5, Kylix2 - UTF8 functions is not implemented
  // Delphi6, Kylix3 - can't divide K2 and K3
  {$IFDEF MSWINDOWS}
    {$IFNDEF VER6P}
      {$DEFINE UTF8}
    {$ENDIF}
  {$ELSE}
    {$IFNDEF VER7P}
      {$DEFINE UTF8}
    {$ENDIF}
  {$ENDIF}
{$ENDIF}

{----------------------------
               MyDAC  | other
----------------------|------
 k                 z  | -
 d5-d6             z  | -
 d7                d  | d
 d8 clr            -  | -
 d9p clr           -  | -
 d9p w32           d  | d
-----------------------------
 z - ZLib objs
 d - Delphi internal}

{$IFNDEF CLR}
{$IFNDEF FPC} // should be removed after fix ZLib in FPC
  {$DEFINE HAVE_COMPRESS_INTERFACE}
  {$IFDEF VER7P}
    {$DEFINE HAVE_COMPRESS_INTERNAL} // ZLib build in Delphi
  {$ENDIF}
  {$IFNDEF LITE}
    {$DEFINE HAVE_COMPRESS} // Define TCompressBlob class
  {$ENDIF}
{$ENDIF}
{$ENDIF}

{$IFNDEF NODBTOOLS} // for all products without db tools integration
{$IFDEF VER9P}
  {$DEFINE DBTOOLS}      // include DBToolsClient unit and related code
{$ENDIF}
{$ENDIF}

{$IFNDEF FPC}
  {$DEFINE WITH_IPROVIDER}
{$ENDIF}

{$IFDEF WIN32_64}
{$IFDEF VER6P}
{$IFNDEF BCB}
{$IFNDEF FPC}
  {$DEFINE USE_CODE_EDITOR}
{$ENDIF}
{$ENDIF}
{$ENDIF}
{$ENDIF}

{$IFDEF USE_CODE_EDITOR}
  {$DEFINE USE_EXT_MENU}
{$ENDIF}
{$IFDEF USE_SYNEDIT}
  {$DEFINE USE_EXT_MENU}
{$ENDIF}

// Uncomment to make Unicode build
//{$DEFINE UNICODE_BUILD}

{$IFDEF UNICODE_BUILD}
{$IFNDEF CLR}
{$IFDEF VER10P}
{$IFNDEF VER12P}
  {$DEFINE CRUNICODE}
{$ENDIF}
{$ENDIF}
{$ENDIF}
{$ENDIF}

{$IFDEF VER8P} // support of $IF
{$IF DEFINED(CLR) or DEFINED(CRUNICODE) or DEFINED(VER12P)}
  {$DEFINE IS_UNICODE}
{$IFEND}
{$ENDIF}


// dbExpress for PostgreSQL supports SSL connection
{$DEFINE HAVE_OPENSSL}
