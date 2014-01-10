@echo off
rem **********************************************************************
rem *
rem * SDAC
rem *
rem * Tasks:
rem *   1) Compile DAC packages;
rem *   2) Compile CRControls package;
rem *   2) Compile SDAC packages;
rem *
rem * Command line:
rem *   call ..\Make.bat IDEName IDEVer CLR
rem *   
rem * Parameters:
rem *   IDEName = (Delphi, CBuilder)
rem *   IDEVer = (5, 6, 7, 9, 10, 11, 12, 14, 15, 16)
rem *   Platform = (CLR, WIN32, WIN64, OSX32) WIN32 - default
rem **********************************************************************

rem Prepare ==============================================================
rem ======================================================================
set IDEName=%1
set IDEVer=%2
set Platform=%3
set PrjNameL=sdac

pushd

rem Test IDEName
if %IDEName%A==DelphiA goto IDENameOK
if %IDEName%A==CBuilderA goto IDENameOK
echo Command line must be:
echo    call ..\Make.bat IDEName IDEVer
echo    IDEName = (Delphi, CBuilder)
goto Err
:IDENameOK

rem Test IDEVer
if %IDEVer%A==5A goto IDEVerOK
if %IDEVer%A==6A goto IDEVerOK
if %IDEVer%A==7A goto IDEVerOK
if %IDEVer%A==9A goto IDEVerOK
if %IDEVer%A==10A goto IDEVerOK
if %IDEVer%A==11A goto IDEVer11
if %IDEVer%A==12A goto IDEVerOK
if %IDEVer%A==14A goto IDEVerOK
if %IDEVer%A==15A goto IDEVerOK
if %IDEVer%A==16A goto IDEVerOK
echo Command line must be:
echo    call ..\Make.bat IDEName IDEVer Platform
echo    IDEVer = (5, 6, 7, 9, 10, 11, 12, 14, 15, 16)
goto Err

:IDEVer11:
set PkgVer=105
goto PkgVerOK

:IDEVerOK
set PkgVer=%IDEVer%0

:PkgVerOK

if not %Platform%A==CLRA goto PlatformWin64
set PlatformDir=CLR
goto :PlatformOK
:PlatformWin64
if not %Platform%A==WIN64A goto PlatformOSX32
set PlatformDir=Win64
goto :PlatformOK
:PlatformOSX32
if not %Platform%A==OSX32A goto PlatformWin32
set PlatformDir=OSX32
goto :PlatformOK
:PlatformWin32
set Platform=WIN32
set PlatformDir=Win32
:PlatformOK

set CompilerOptions=-LE. -U..\..\Lib\Delphi16\%PlatformDir%;..\..\..\Common\Source;..\..\..\Common\Source\Delphi16;..\ -NSSystem;Xml;Data;Datasnap;Web;Soap;Winapi;System.Win;Data.Win;Datasnap.Win;Web.Win;Soap.Win;Xml.Win;Bde
set CompilerOptionsVCL=-LE. -U..\..\Lib\Delphi16\%PlatformDir%;..\..\..\Common\Source;..\..\..\Common\Source\Delphi16;..\ -NSSystem;Xml;Data;Datasnap;Web;Soap;Winapi;System.Win;Data.Win;Datasnap.Win;Web.Win;Soap.Win;Xml.Win;Bde;Vcl;Vcl.Imaging;Vcl.Touch;Vcl.Samples;Vcl.Shell

if %IDEVer%A==16A goto IDEVer16
set PlatformDir=.
set CompilerOptions=-LE.
set CompilerOptionsVCL=-LE.
:IDEVer16

if %IDEName%A==CBuilderA goto CBuilder
if %Platform%A==CLRA goto Delphi8

rem Compile ==============================================================
if not %Platform%A==WIN32A goto Win64Compiler
set Compiler=%IdeDir%\Bin\dcc32.exe"
goto CompilerOK
:Win64Compiler
if not %Platform%A==WIN64A goto OSX32Compiler
set Compiler=%IdeDir%\Bin\dcc64.exe"
goto CompilerOK
:OSX32Compiler
if not %Platform%A==OSX32A goto InvalidPlatform
set Compiler=%IdeDir%\Bin\dccosx.exe"
:CompilerOK

rem Compile DAC packages =================================================
%Compiler% %CompilerOptions% dac%PkgVer%.dpk
@if errorlevel 1 goto Err

if %IDEVer%A==5A goto SkipDVcl
if %Platform%A==OSX32A goto SkipDVcl
%Compiler% %CompilerOptionsVCL% dacvcl%PkgVer%.dpk
@if errorlevel 1 goto Err
:SkipDVcl

if not %IDEVer%A==16A goto SkipDFmx
%Compiler% %CompilerOptions% dacfmx%PkgVer%.dpk
@if errorlevel 1 goto Err
:SkipDFmx

if %Platform%A==WIN64A goto SkipDcl
if %Platform%A==OSX32A goto SkipDcl
%Compiler% %CompilerOptionsVCL% dcldac%PkgVer%.dpk
@if errorlevel 1 goto Err
:SkipDcl

rem Compile CRControls package ===========================================
if %Platform%A==OSX32A goto Skip_Controls
%Compiler% %CompilerOptionsVCL% crcontrols%PkgVer%.dpk
@if errorlevel 1 goto Err
:Skip_Controls

if %Platform%A==WIN64A goto Skip_Dcl
if %Platform%A==OSX32A goto Skip_Dcl
%Compiler% %CompilerOptionsVCL% dclcrcontrols%PkgVer%.dpk
@if errorlevel 1 goto Err
:Skip_Dcl

rem Compile SDAC packages ===========================================
%Compiler% %CompilerOptions% %PrjNameL%%PkgVer%.dpk
@if errorlevel 1 goto Err

if %IDEVer%A==5A goto Skip__Vcl
if %Platform%A==OSX32A goto Skip__Vcl
%Compiler% %CompilerOptionsVCL% %PrjNameL%vcl%PkgVer%.dpk
@if errorlevel 1 goto Err
:Skip__Vcl

if not %IDEVer%A==16A goto Skip__Fmx
%Compiler% %CompilerOptions% %PrjNameL%fmx%PkgVer%.dpk
@if errorlevel 1 goto Err
:Skip__Fmx

if %Platform%A==WIN64A goto Skip__Dcl
if %Platform%A==OSX32A goto Skip__Dcl
%Compiler% %CompilerOptionsVCL% dcl%PrjNameL%%PkgVer%.dpk
@if errorlevel 1 goto Err
:Skip__Dcl

rem Copy files ===========================================================
rem ======================================================================

if exist *.bpl        move *.bpl               ..\..\Bin\%IDEName%%IDEVer%\%PlatformDir%
if exist *.dcu        move *.dcu               ..\..\Lib\%IDEName%%IDEVer%\%PlatformDir%
if exist ..\*.dcu     move ..\*.dcu            ..\..\Lib\%IDEName%%IDEVer%\%PlatformDir%
if exist *.dcp        move *.dcp               ..\..\Lib\%IDEName%%IDEVer%\%PlatformDir%

copy ..\*.res            ..\..\Lib\%IDEName%%IDEVer%\%PlatformDir%

rem MAC OS X files ===
if not %Platform%A==OSX32A goto SkipOSX32Lib
if exist *.dylib      move *.dylib             ..\..\Bin\%IDEName%%IDEVer%\%PlatformDir%
if exist *.a          move *.a                 ..\..\Lib\%IDEName%%IDEVer%\%PlatformDir%
if exist *.bpi        move *.bpi               ..\..\Lib\%IDEName%%IDEVer%\%PlatformDir%
:SkipOSX32Lib

rem CBuilder files ===
if %Platform%A==CLRA goto SkipD10BCCLib
if exist  *.bpi       move *.bpi               ..\..\Lib\%IDEName%%IDEVer%\%PlatformDir%
if exist  *.lib       move *.lib               ..\..\Lib\%IDEName%%IDEVer%\%PlatformDir%
if exist  *.hpp       move *.hpp               ..\..\Include\%IDEName%%IDEVer%\%PlatformDir%
if exist  ..\*.hpp    move ..\*.hpp            ..\..\Include\%IDEName%%IDEVer%\%PlatformDir%
:SkipD10BCCLib

goto end

:Delphi8
rem Compile Delphi8 ======================================================
rem Compile DAC packages =================================================

%IdeDir%\Bin\dccil.exe" -LE. Devart.Dac.dpk -R..\ 
@if errorlevel 1 goto Err

%IdeDir%\Bin\dccil.exe" -LE. Devart.Dac.AdoNet.dpk
@if errorlevel 1 goto Err

%IdeDir%\Bin\dccil.exe" -LE. Devart.Dac.Design.dpk
@if errorlevel 1 goto Err

rem Compile CRControls package ===========================================
%IdeDir%\Bin\dccil.exe" -LE. Devart.Vcl.dpk -R..\ 
@if errorlevel 1 goto Err

rem Compile SDAC packages ===========================================
%IdeDir%\Bin\dccil.exe" -LE. Devart.Sdac.dpk -R..\ -I..\
@if errorlevel 1 goto Err

%IdeDir%\Bin\dccil.exe" -LE. Devart.Sdac.AdoNet.dpk 
@if errorlevel 1 goto Err

%IdeDir%\Bin\dccil.exe" -LE. Devart.Sdac.Design.dpk -R..\;..\Design -I..\
@if errorlevel 1 goto Err

rem Copy files ===========================================================
rem ======================================================================

if exist *.dll      move *.dll               ..\..\Bin\%IDEName%%IDEVer%
if exist *.pdb      move *.pdb               ..\..\Bin\%IDEName%%IDEVer%

if exist *.dcpil    move *.dcpil             ..\..\Lib\%IDEName%%IDEVer%
if exist *.dcuil    move *.dcuil             ..\..\Lib\%IDEName%%IDEVer%

copy ..\*.res            ..\..\Lib\%IDEName%%IDEVer%

goto end

:CBuilder
rem Compile ==============================================================
rem Compile DAC packages =================================================
cd %DacDir%

%IdeDir%\Bin\bpr2mak.exe" -t..\dac.bmk dac%PkgVer%.bpk
@if errorlevel 1 goto Err
%IdeDir%\Bin\make.exe" -f dac%PkgVer%.mak
@if errorlevel 1 goto Err

if %IDEVer%A==5A goto SkipCBVcl
%IdeDir%\Bin\bpr2mak.exe" -t..\dac.bmk dacvcl%PkgVer%.bpk
@if errorlevel 1 goto Err
%IdeDir%\Bin\make.exe" -f dacvcl%PkgVer%.mak
@if errorlevel 1 goto Err
:SkipCBVcl

%IdeDir%\Bin\bpr2mak.exe" -t..\dac.bmk dcldac%PkgVer%.bpk
@if errorlevel 1 goto Err
%IdeDir%\Bin\make.exe" -f dcldac%PkgVer%.mak
@if errorlevel 1 goto Err

rem Compile CRControls package ===========================================
%IdeDir%\Bin\bpr2mak.exe" -t..\dac.bmk CRControls%PkgVer%.bpk
@if errorlevel 1 goto Err
%IdeDir%\Bin\make.exe" -f CRControls%PkgVer%.mak
@if errorlevel 1 goto Err
%IdeDir%\Bin\bpr2mak.exe" -t..\dac.bmk dclCRControls%PkgVer%.bpk
@if errorlevel 1 goto Err
%IdeDir%\Bin\make.exe" -f dclCRControls%PkgVer%.mak
@if errorlevel 1 goto Err

rem Compile SDAC packages ===========================================
%IdeDir%\Bin\bpr2mak.exe" -t..\dac.bmk %PrjNameL%%PkgVer%.bpk
@if errorlevel 1 goto Err
%IdeDir%\Bin\make.exe" -f %PrjNameL%%PkgVer%.mak
@if errorlevel 1 goto Err

if %IDEVer%A==5A goto SkipCB__Vcl
%IdeDir%\Bin\bpr2mak.exe" -t..\dac.bmk %PrjNameL%vcl%PkgVer%.bpk
@if errorlevel 1 goto Err
%IdeDir%\Bin\make.exe" -f %PrjNameL%vcl%PkgVer%.mak
@if errorlevel 1 goto Err
:SkipCB__Vcl

%IdeDir%\Bin\bpr2mak.exe" -t..\dac.bmk dcl%PrjNameL%%PkgVer%.bpk
@if errorlevel 1 goto Err
%IdeDir%\Bin\make.exe" -f dcl%PrjNameL%%PkgVer%.mak
@if errorlevel 1 goto Err

rem Copy files ===========================================================
rem ======================================================================

if exist *.bpl        move *.bpl               ..\..\Bin\%IDEName%%IDEVer%\%PlatformDir%
if exist *.tds        move *.tds               ..\..\Bin\%IDEName%%IDEVer%\%PlatformDir%
if exist *.mak        move *.mak               ..\..\Bin\%IDEName%%IDEVer%\%PlatformDir%

if exist *.dcu        move *.dcu               ..\..\Lib\%IDEName%%IDEVer%\%PlatformDir%
if exist ..\*.dcu     move ..\*.dcu            ..\..\Lib\%IDEName%%IDEVer%\%PlatformDir%
if exist *.bpi        move *.bpi               ..\..\Lib\%IDEName%%IDEVer%\%PlatformDir%
if exist *.lib        move *.lib               ..\..\Lib\%IDEName%%IDEVer%\%PlatformDir%
if exist *.obj        move *.obj               ..\..\Lib\%IDEName%%IDEVer%\%PlatformDir%
if exist *.hpp        move *.hpp               ..\..\Include\%IDEName%%IDEVer%\%PlatformDir%
if exist ..\*.hpp     move ..\*.hpp            ..\..\Include\%IDEName%%IDEVer%\%PlatformDir%

copy ..\*.res            ..\..\Lib\%IDEName%%IDEVer%\%PlatformDir%

goto end

:InvalidPlatform
echo Invalid Platform

:Err
pause

:end
popd