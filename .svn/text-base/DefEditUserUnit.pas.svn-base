unit DefEditUserUnit;

interface

uses ddPlugin_TLB, VirtualTable, deType, ddType;

type

  // uzivatelky nastavene hodnoty pro atribut
  TDefEditUser = class(TObject)
  private
    fHelios: IHelios;
    fGUID: TGUID;

    procedure checkSQL;

    function getDismisList: String;
    procedure setDismisList(s: String);
    function getStringGUID: String;
  public

  published
    constructor Create(aHelios: IHelios; aGUID: TGUID); virtual;

    property DismisList: String read getDismisList write setDismisList;
    property Helios: IHelios read fHelios;
    property GUID: TGUID read fGUID;
    property StringGUID: String read getStringGUID;
  end;

implementation

uses ddMain, SysUtils, sqString, Data.db, MemData, sqProc, Classes, deEditory;

{ TDefEditUser }

procedure TDefEditUser.checkSQL;
var
  SQL: String;
begin
  SQL := 'IF OBJECT_ID(''dbo.TabHM_browse_attr_user'')IS NOT NULL SELECT 1 ELSE SELECT 0';

  with fHelios.OpenSQL(SQL) do
  begin
    // neni definovan, nutno vytvorit
    if FieldValues(0) = 0 then
    begin
      SQL := 'CREATE TABLE [dbo].[TabHM_browse_attr_user]('#13 + '	[GUID] [nvarchar] (38) NOT NULL,'#13 +
        '	[for_user] [nchar](128) NOT NULL,'#13 + '	[dismiss] [text] NULL'#13 +
        'CONSTRAINT [PK_TabHM_browse_attr_user] PRIMARY KEY CLUSTERED'#13 + '('#13 + '	[GUID] ASC'#13 +
        ')WITH (PAD_INDEX  = OFF, STATISTICS_NORECOMPUTE  = OFF, IGNORE_DUP_KEY = OFF, ALLOW_ROW_LOCKS  = ON, ALLOW_PAGE_LOCKS  = ON) ON [PRIMARY]'#13
        + ') ON [PRIMARY] TEXTIMAGE_ON [PRIMARY]';
      fHelios.ExecSQL(SQL);

      SQL := 'ALTER TABLE [dbo].[TabHM_browse_attr_user] ADD  CONSTRAINT [DF_TabHM_browse_attr_user_for_user]  DEFAULT (suser_sname()) FOR [for_user]';
      fHelios.ExecSQL(SQL);
    end;
  end;
end;

constructor TDefEditUser.Create(aHelios: IHelios; aGUID: TGUID);
begin
  inherited Create;

  fHelios := aHelios;
  fGUID := aGUID;

  checkSQL;
end;

function TDefEditUser.getDismisList: String;
var
  SQL: String;
begin
  SQL := 'SELECT dismiss FROM dbo.TabHM_browse_attr_user WHERE for_user=SUSER_SNAME() AND GUID=' +
    NQuotedStr(StringGUID);
  with fHelios.OpenSQL(SQL) do
    if RecordCount = 0 then
      Result := ''
    else
      Result := FieldValues(0);
end;

function TDefEditUser.getStringGUID: String;
begin
  Result := GUIDToString(fGUID);
end;

procedure TDefEditUser.setDismisList(s: String);
var
  SQL: String;
begin
  if StringGUID = '' then
    fGUID := StringToGUID('{68F5E1C7-3FB9-4010-8BBC-DEDB45135A18}');
  SQL := 'begin tran'#13 + 'UPDATE dbo.TabHM_browse_attr_user SET dismiss = ' + NQuotedStr(s) +
    ' WHERE for_user=SUSER_SNAME() AND GUID=' + NQuotedStr(StringGUID) + #13 + 'if @@rowcount = 0'#13 +
    'INSERT INTO dbo.TabHM_browse_attr_user (dismiss, for_user,attr_id) VALUES(' + NQuotedStr(s) + ', SUSER_SNAME(), ' +
    NQuotedStr(StringGUID) + ')'#13 + 'commit tran';
  fHelios.ExecSQL(SQL)
end;

end.
