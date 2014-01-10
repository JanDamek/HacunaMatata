{$IFNDEF CLR}
{$I Dac.inc}
unit DBToolsIntf;
{$ENDIF}
interface

{$IFDEF DBTOOLS}
uses
{$IFDEF CLR}
  System.Runtime.InteropServices,
  Variants,
  CoreLab.DbTools;
{$ELSE}
  Windows, ActiveX;
{$ENDIF}

type
{$IFDEF WIN32_64}
  ParameterType = TOleEnum;
{$ELSE}
  TConnectionInfoArray = array of ConnectionInfo;
{$ENDIF}
// Constants for enum ParameterType
const
  ParameterType_Input = ParameterType(0);
  ParameterType_InputOutput = ParameterType(1);
  ParameterType_Output = ParameterType(2);
  ParameterType_ReturnValue = ParameterType(3);

  Devart_DbTools_InterfacesMajorVersion = 2;
  Devart_DbTools_InterfacesMinorVersion = 0;

// *********************************************************************//
// Declaration of structures, unions and aliases.
// *********************************************************************//
{$IFDEF WIN32_64}
type
  ConnectionInfo = packed record
    Name: WideString;
    ConnectionString: WideString;
  end;

  CommandParameterInfo = packed record
    Name: PWideChar;
    ParameterType: ParameterType;
    DataType: Integer;
    _slack_space: dword; // Align by 16 bytes
    Value: OleVariant;
  end;

  TConnectionInfoArray = PSafeArray;

// *********************************************************************//
// Interface: ISqlEditor
// Flags:     (256) OleAutomation
// GUID:      {471AE99C-AE21-491B-89E3-A2C717C81CA9}
// Simple interface of the SQL editor.
// *********************************************************************//
  ISqlEditor = interface(IUnknown)
    ['{471AE99C-AE21-491B-89E3-A2C717C81CA9}']
    function Get_Handle: integer; stdcall;
    function Get_text: PWideChar; stdcall;
    procedure Set_text(const Param1: PWideChar); stdcall;
    function Get_Modified: LongBool; stdcall;
    procedure Set_Modified(Param1: LongBool); stdcall;
    function Get_ReadOnly: LongBool; stdcall;
    procedure Set_ReadOnly(Param1: LongBool); stdcall;

    procedure SetConnection(const ConnectionString: WideString); stdcall;
    property Modified: LongBool read Get_Modified write Set_Modified;
    property Handle: integer read Get_Handle;
    property ReadOnly: LongBool read Get_ReadOnly write Set_ReadOnly;
    property Text: PWideChar read Get_text write Set_text;
  end;

// *********************************************************************//
// Interface: ISqlSource
// Flags:     (256) OleAutomation
// GUID:      {471AE99C-AE21-491B-89E3-A2C717C81CA8}
// Describes any component with editable SQL query on the designer. A 'Command' for example.
// *********************************************************************//
  ISqlSource = interface(IUnknown)
    ['{471AE99C-AE21-491B-89E3-A2C717C81CA8}']
    function Get_Name: PWideChar; stdcall;
    function Get_ConnectionString: PWideChar; stdcall;
    function Get_DesignerName: PWideChar; stdcall;
    function Get_ParameterCount: Integer; stdcall;
    procedure Set_ParameterCount(const Param1: Integer); stdcall;
    function Get_Sql: PWideChar; stdcall;
    procedure Set_Sql(const Param1: PWideChar); stdcall;
    procedure Close; stdcall;
    procedure GetParameter(const index: Integer; out Info: CommandParameterInfo); stdcall;
    procedure SetParameter(const index: Integer; Info: CommandParameterInfo); stdcall;
  end;

// *********************************************************************//
// Interface: ISqlSourceNotifier
// Flags:     (256) OleAutomation
// GUID:      {D4858B82-70FB-4E30-8BE5-30CF625A37E2}
// Feedback interface for the SQL service. Allows to notify about any componet changings.
// *********************************************************************//
  ISqlSourceNotifier = interface(IUnknown)
    ['{D4858B82-70FB-4E30-8BE5-30CF625A37E2}']
    procedure OnSqlSourceChanged; stdcall;
    procedure OnSqlSourceDeleted; stdcall;
    procedure OnSqlSourceRenamed(const prevName: WideString); stdcall;
  end;

// *********************************************************************//
// Interface: IDbToolsService
// Flags:     (256) OleAutomation
// GUID:      {7FB8EF3F-AB68-48D1-9DD5-F85C05DF4A90}
// An interface of the service, that provides basics functionalities of editing or executing an SQL query.
// *********************************************************************//
  IDbToolsService = interface(IUnknown)
    ['{7FB8EF3F-AB68-48D1-9DD5-F85C05DF4A90}']
    procedure CreateSqlEditor(out editor: ISqlEditor); stdcall;
    procedure DesignerClosing(const DesignerName: WideString); stdcall;
    procedure EditDatabaseObject(const ConnectionString: WideString; const objectType: WideString;
                                 const fullName: WideString); stdcall;
    procedure EditSql(const sqlSource: ISqlSource; asQuery: LongBool;
                      out notifier: ISqlSourceNotifier); stdcall;
    procedure ExecuteSql(const sqlSource: ISqlSource; debug: LongBool); stdcall;
    procedure FindInDatabaseExplorer(const ConnectionString: WideString;
                                     const objectType: WideString; const fullName: WideString); stdcall;
    function GetConnections: PSafeArray; stdcall;
    procedure RetrieveData(const sqlSource: ISqlSource; asDocument: LongBool); stdcall;
  end;
{$ENDIF}

{$ENDIF DBTOOLS}

implementation

end.
