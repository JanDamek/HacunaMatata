//////////////////////////////////////////////////
//  DB Access Components
//  Copyright © 1998-2012 Devart. All right reserved.
//  DA Script
//////////////////////////////////////////////////
{$IFNDEF CLR}

{$I Dac.inc}

unit DAScript;
{$ENDIF}

interface

uses
  Classes, SysUtils, SyncObjs, {$IFDEF VER6P}Variants,{$ENDIF} DB,
{$IFDEF CLR}
  System.Text,
{$ELSE}
  CLRClasses,
{$ENDIF}
  CRTypes, CRParser, DBAccess;

const
  ST_UNKNOWN = 0;
  ST_DELIMETER = 1;
  ST_COMMENT = 2;
  ST_SPECIFIC_SQL = $8000;

type
  TDAScript = class;

  TDAStatement = class(TCollectionItem)
  protected
    FOmit: boolean;
    FStatementType: integer;
    FStartPos: integer;
    FEndPos: integer;
    FStartLine: integer;
    FEndLine: integer;
    FStartOffset: integer;
    FEndOffset: integer;
    FParams: TDAParams;

    function GetSQL: _string;
    function GetScript: TDAScript;
    function GetParams: TDAParams;
    function CreateParams: TDAParams;
  public
    destructor Destroy; override;
    property Script: TDAScript read GetScript;
    property SQL: _string read GetSQL;
    property Omit: boolean read FOmit write FOmit;

    property StartPos: integer read FStartPos;
    property EndPos: integer read FEndPos;
    property StartLine: integer read FStartLine;
    property EndLine: integer read FEndLine;
    property StartOffset: integer read FStartOffset;
    property EndOffset: integer read FEndOffset;

    property Params: TDAParams read GetParams;

    procedure Execute;
  end;

  TDAStatementClass = class of TDAStatement;

  TDAStatements = class(TCollection)
  protected
    FScript: TDAScript;
    function GetItem(Index: Integer): TDAStatement;
    function CreateStatement(StatementType: integer; Omit: boolean; StartPos,
      EndPos, StartLine, EndLine, StartOffset, EndOffset: integer): TDAStatement;
  public
    constructor Create(ItemClass: TCollectionItemClass; Script: TDAScript);
    property Items[Index: Integer]: TDAStatement read GetItem; default;
  end;

  TDAStatementsClass = class of TDAStatements;

  TDelimiterState = (dsNone, dsDelimiter, dsBlank, dsValue, dsSet);

  TDAScriptProcessor = class
  protected
    FOwner: TDAScript;
    FParser: TSQLParser;
    FSQLParser: TSQLParser;
    FCurrDelimiter: _string;
    FDelimiterState: TDelimiterState;
    FSt: _string;
    FCurrentStatementIdx: integer;
    FStatementsPopulating: boolean;
    FSQL: _StringBuilder;

    function UsedConnection: TCustomDAConnection;
    function GetCommand: TCustomDASQL;

    function GetParserClass: TSQLParserClass; virtual;
    function CreateParser(const Text: _string): TSQLParser; overload;
    function CreateParser(Stream: TStream): TSQLParser; overload;
    function GetSQLParser(const Text: _string): TSQLParser;

    function ExecuteNext: boolean; virtual;
    procedure ExecuteStatement(const SQL: _string; StatementType: integer; var Omit: Boolean; out BreakExec: boolean; Params: TDAParams = nil); virtual;
    procedure CreateStatement(StatementType: integer; Omit: boolean; StartPos, EndPos,
      StartLine, EndLine, StartOffset, EndOffset: integer); virtual;
    procedure BreakExec; virtual;
    procedure Reset; virtual;
    procedure CheckLexem(Code: integer; var StatementType: integer; var Omit: boolean); virtual;
    function GetReady(Code: integer): boolean; virtual;
    function IsSpecificSQL(StatementType: integer): boolean; virtual;
    function CanOptimize(const SQL: _string; const StatementType: integer): boolean; virtual; // Must return True if  statement may be concatenated with previous
    function IsBlankEndsDelimeter: boolean; virtual;

    procedure DoBeforeStatementExecute(var SQL: _string; StatementType: integer; var Omit: boolean); virtual;
    procedure DoAfterStatementExecute(var SQL: _string; StatementType: integer); virtual;

  public
    constructor Create(Owner: TDAScript); virtual;
    destructor Destroy; override;

    function SetProp(Prop: integer; const Value: variant): boolean; virtual;
    function GetProp(Prop: integer; var Value: variant): boolean; virtual;
  end;

  TDAScriptProcessorClass = class of TDAScriptProcessor;

  TBeforeStatementExecuteEvent = procedure (Sender: TObject; var SQL: _string; var Omit: boolean) of object;
  TAfterStatementExecuteEvent = procedure (Sender: TObject; SQL: _string) of object;
  TErrorAction = (eaAbort, eaFail, eaException, eaContinue);
  TOnErrorEvent = procedure (Sender: TObject; E: Exception; SQL: _string; var Action: TErrorAction) of object;

  TDAScript = class (TComponent)
  protected
    FProcessor: TDAScriptProcessor;
    FSQL: _TStrings;
    FSQLActual: boolean;
    FStream: TStream;
    FCommand: TCustomDASQL;
    FMacros: TMacros;
    FErrorOffset: Int64;
    FStartPos: Int64;
    FEndPos: Int64;
    FStartLine: Int64;
    FEndLine: Int64;
    FStartOffset: Int64;
    FEndOffset: Int64;
    FDataSource: TDataSource;
    FBeforeExecute: TBeforeStatementExecuteEvent;
    FAfterExecute: TAfterStatementExecuteEvent;
    FOnError: TOnErrorEvent;
    FStmtOffset: Int64;
    FDesignCreate: boolean;
    FDelimiter: _string;
    FStatements: TDAStatements;

    FUseOptimization: boolean;
    FAllowOptimization: boolean;
    FBuffer: _StringBuilder;
    FAutoCommit: Boolean;
    FBreakExecution: boolean;
    FcsBreakMultiThread: TCriticalSection;
    FNoPreconnect: boolean;

    function GetConnection: TCustomDAConnection;
    procedure SetConnection(Value: TCustomDAConnection);
    function GetTransaction: TDATransaction;
    procedure SetTransaction(Value: TDATransaction);
    function IsTransactionStored: boolean;
    function GetProcessorClass: TDAScriptProcessorClass; virtual;
    procedure SetProcessor(Value: TDAScriptProcessor); virtual;
    procedure CreateProcessor;
    procedure FreeProcessor;
    procedure CheckProcessor;
    function UsedConnection: TCustomDAConnection; virtual;
    function UsedTransaction: TDATransaction;
    function GetSQLText(StartLine, EndLine, StartOffset, EndOffset, Length: integer): _string;
    procedure SetSQL(Value: _TStrings);
    procedure SQLChanged(Sender: TObject);
    function GetDebug: boolean;
    procedure SetDebug(Value: boolean);
    procedure SetMacros(Value: TMacros);
    function GetDataSet: TCustomDADataSet;
    procedure SetDataSet(Value: TCustomDADataSet);
    function GetParams: TDAParams;
    procedure SetAutoCommit(Value: Boolean);
    procedure SetDelimiter(const Value: _string);
    function IsDelimiterStored: boolean;
    procedure Loaded; override;
    procedure DefineProperties(Filer: TFiler); override;
    procedure ReadMacroData(Reader: TReader);
    procedure WriteMacroData(Writer: TWriter);
    procedure AssignTo(Dest: TPersistent); override;
    function CreateCommand: TCustomDASQL; virtual;
    procedure CalculateErrorOffset(E: Exception); virtual;

    function CreateStatementsObject: TDAStatements; virtual;
    function GetStatements: TDAStatements;
    procedure Open(Stream: TStream);
    procedure Close;

    procedure InternalExecute(const SQL: _string; out BreakExec: boolean; Params: TDAParams = nil); // Sends SQL to server
    procedure Flush(out BreakExec: boolean);

    property Transaction: TDATransaction read GetTransaction write SetTransaction stored IsTransactionStored;

  public
    constructor Create(Owner: TComponent); override;
    destructor Destroy; override;
    procedure Execute; virtual;
    procedure ExecuteStream(Stream: TStream);
    procedure ExecuteFile(const FileName: string);
    function ExecuteNext: boolean; virtual;
    procedure BreakExec; virtual;
    function FindMacro(Name: _string): TMacro;
    function MacroByName(Name: _string): TMacro;
    function ErrorOffset: Int64;

    property Params: TDAParams read GetParams;
    property UseOptimization: boolean read FUseOptimization write FUseOptimization default False;
    property AutoCommit: Boolean read FAutoCommit write SetAutoCommit default False;

    property Connection: TCustomDAConnection read GetConnection write SetConnection;
    property DataSet: TCustomDADataSet read GetDataSet write SetDataSet;
    property StartPos: Int64 read FStartPos;
    property EndPos: Int64 read FEndPos;
    property StartLine: Int64 read FStartLine;
    property EndLine: Int64 read FEndLine;
    property StartOffset: Int64 read FStartOffset;
    property EndOffset: Int64 read FEndOffset;
    property Statements: TDAStatements read GetStatements;

  published
    property SQL: _TStrings read FSQL write SetSQL;
    property Debug: boolean read GetDebug write SetDebug default False;
    property Delimiter: _string read FDelimiter write FDelimiter stored IsDelimiterStored;
    property Macros: TMacros read FMacros write SetMacros stored False;
    property NoPreconnect: boolean read FNoPreconnect write FNoPreconnect default False;
    property BeforeExecute: TBeforeStatementExecuteEvent read FBeforeExecute write FBeforeExecute;
    property AfterExecute: TAfterStatementExecuteEvent read FAfterExecute write FAfterExecute;
    property OnError: TOnErrorEvent read FOnError write FOnError;
  end;

  TDAScriptUtils = class
  public
    class procedure SetDesignCreate(Obj: TDAScript; Value: boolean);
    class function GetDesignCreate(Obj: TDAScript): boolean;
    class procedure SetCommand(Obj: TDAScript; Command: TCustomDASQL);
    class function GetCommand(Obj: TDAScript): TCustomDASQL;
    class procedure Open(Obj: TDAScript; Stream: TStream);
    class procedure Close(Obj: TDAScript);
    class function UsedConnection(Obj: TDAScript): TCustomDAConnection;
    class function UsedTransaction(Obj: TDAScript): TDATransaction;
    class function GetTransaction(Obj: TDAScript): TDATransaction;
    class procedure SetTransaction(Obj: TDAScript; Value: TDATransaction);
  end;

implementation

uses
{$IFDEF VER5}
  MemData,
{$ENDIF}  
  DAConsts;

{ TDAStatement }
destructor TDAStatement.Destroy;
begin
  FParams.Free;
  
  inherited;
end;

function TDAStatement.GetScript: TDAScript;
begin
  Result := TDAStatements(Collection).FScript;
end;

function TDAStatement.GetSQL: _string;
begin
  Result := Script.GetSQLText(FStartLine, FEndLine, FStartOffset, FEndOffset, FEndPos - FStartPos);
end;

function TDAStatement.GetParams: TDAParams;
begin
  if FParams = nil then begin
    FParams := CreateParams;
    TDBAccessUtils.ParseSQL(Script.FCommand, SQL, FParams);
  end;
  Result := FParams;
end;

function TDAStatement.CreateParams: TDAParams;
begin
  Result := TDBAccessUtils.CreateParamsObject(Script.FCommand);
end;

procedure TDAStatement.Execute;
var
  BreakExec: boolean;
begin
  Script.CheckProcessor;
  Script.FProcessor.ExecuteStatement(GetSQL, FStatementType, FOmit, BreakExec, FParams);
end;

{ TDAStatements }

constructor TDAStatements.Create(ItemClass: TCollectionItemClass; Script: TDAScript);
begin
  inherited Create(ItemClass);

  FScript := Script;
end;

function TDAStatements.GetItem(Index: Integer): TDAStatement;
begin
  Result := TDAStatement(inherited Items[Index]);
end;

function TDAStatements.CreateStatement(StatementType: integer; Omit: boolean; StartPos,
  EndPos, StartLine, EndLine, StartOffset, EndOffset: integer): TDAStatement;
begin
  Result := Add as TDAStatement;

  Result.FOmit := Omit;
  Result.FStartLine := StartLine;
  Result.FEndLine := EndLine;
  Result.FStartPos := StartPos;
  Result.FEndPos := EndPos;
  Result.FStartOffset := StartOffset;
  Result.FEndOffset := EndOffset;
  Result.FStatementType := StatementType;
end;

{ TDAScriptProcessor }

constructor TDAScriptProcessor.Create(Owner: TDAScript);
begin
  inherited Create;

  FOwner := Owner;
  FCurrDelimiter := Owner.FDelimiter;
  FCurrentStatementIdx := -1;
end;

destructor TDAScriptProcessor.Destroy;
begin
  FParser.Free;
  FSQLParser.Free;
  FSQL.Free;

  inherited Destroy;
end;

function TDAScriptProcessor.GetProp(Prop: integer; var Value: variant): boolean;
begin
  Assert(False, IntToStr(Prop));
  Result := False;
end;

function TDAScriptProcessor.SetProp(Prop: integer; const Value: variant): boolean;
begin
  Assert(False, IntToStr(Prop));
  Result := False;
end;

function TDAScriptProcessor.UsedConnection: TCustomDAConnection;
begin
  Result := TDBAccessUtils.UsedConnection(FOwner.FCommand);
end;

function TDAScriptProcessor.GetCommand: TCustomDASQL;
begin
  Result := FOwner.FCommand;
end;

function TDAScriptProcessor.GetParserClass: TSQLParserClass;
begin
  Result := nil;
  Assert(False, 'Should be overriden');
end;

function TDAScriptProcessor.CreateParser(const Text: _string): TSQLParser;
begin
  Result := GetParserClass.Create(Text);
end;

function TDAScriptProcessor.CreateParser(Stream: TStream): TSQLParser;
var
  Buf: array [0..1] of byte;
  Count: integer;
  enc: Encoding;
begin
  Stream.Position := 0;
  Count := Stream.Read(Buf, 2);
  if (Count = 2) and (Buf[0] = $FF) and (Buf[1] = $FE) then
    enc := Encoding.Unicode
  else
  if (Count = 2) and (Buf[0] = $EF) and (Buf[1] = $BB) then begin // UTF-8 format
    Stream.Read(Buf, 1);
    enc := Encoding.UTF8;
  end
  else begin
    enc := Encoding.Default;
    Stream.Position := 0;
  end;
  Result := GetParserClass.Create(Stream, enc);
end;

function TDAScriptProcessor.GetSQLParser(const Text: _string): TSQLParser;
begin
  if FSQLParser = nil then begin
    FSQLParser := TSQLParser(CreateParser(Text));
    FSQLParser.OmitBlank := False;
    FSQLParser.Uppered := False;
    FSQLParser.QuotedString := True;
    FSQLParser.OmitComment := True;
  end
  else
    FSQLParser.SetText(Text);
  Result := FSQLParser;
end;

function TDAScriptProcessor.ExecuteNext: boolean;
var
  CurrentStatement: TDAStatement;
  PrevSt, s, TempSt: _string;
  Code, TempCode: integer;
  Ready: boolean;
  NewDelimiter: _string;
  BreakExecution: boolean;
  StatementType: integer;
  Omit: boolean;
  OldDebug: boolean;

  function CheckDelimeter: boolean;
  begin
    Result := FSt = FCurrDelimiter;
    if not Result and (Length(FCurrDelimiter) = 2) then
      Result := ((PrevSt = FCurrDelimiter[1]) and (FSt = FCurrDelimiter[2]));
  end;

begin
  FOwner.FcsBreakMultiThread.Enter;
  try
    Result := False;
    if not FStatementsPopulating and (FOwner.FStatements <> nil) and (FOwner.FStream = nil) then begin
      if (FOwner.FStatements.Count > 0) then begin
        if FCurrentStatementIdx = -1 then
          FCurrentStatementIdx := 0;
        if FCurrentStatementIdx <= FOwner.FStatements.Count - 1 then begin
          try
            CurrentStatement := FOwner.FStatements[FCurrentStatementIdx];
            Omit := CurrentStatement.Omit;
            ExecuteStatement(CurrentStatement.SQL, CurrentStatement.FStatementType, Omit, BreakExecution, CurrentStatement.FParams);
            Result := not BreakExecution;
          finally
            Inc(FCurrentStatementIdx);
          end;
        end
        else
          FCurrentStatementIdx := -1;
      end;
    end
    else begin
      if FParser = nil then begin
        if FOwner.FStream <> nil then
          FParser := CreateParser(FOwner.FStream)
        else
          FParser := CreateParser(TrimRight(FOwner.FSQL.Text));

        FParser.OmitBlank := False;
        FParser.Uppered := False;
        FParser.QuotedString := True;
        FParser.AdvancedStringParsing := True;
      end;
      if FSQL = nil then
        FSQL := _StringBuilder.Create;
      FSQL.Length := 0;

      StatementType := ST_UNKNOWN;
      Omit := False;
      FDelimiterState := dsNone;
      NewDelimiter := '';
      PrevSt := '';
      FOwner.FErrorOffset := 0;
      Code := lcBlank;
      FOwner.FStartPos := FParser.CurrPos;
      FOwner.FStartLine := FParser.CurrLine;
      FOwner.FStartOffset := FParser.CurrCol;
      Reset;
      OldDebug := False;
      if Assigned(FOwner.DataSet) then begin
        OldDebug := FOwner.DataSet.Debug;
        FOwner.DataSet.Debug := FOwner.Debug;
      end;
      try
        repeat
          if (Code <> lcBlank) and (Code <> lcComment) then
            PrevSt := FSt;
          Code := FParser.GetNext(FSt);
          if (FDelimiterState = dsBlank) and (Code <> lcBlank) then
            FDelimiterState := dsValue;
          if FDelimiterState = dsValue then begin
            if not CheckDelimeter then begin
              if (Code <> lcBlank) and (Code <> lcEnd) then
                NewDelimiter := NewDelimiter + FSt
              else
                if IsBlankEndsDelimeter then
                  FDelimiterState := dsSet;
            end
            else begin
              FDelimiterState := dsSet;
              if NewDelimiter = '' then
                NewDelimiter := FCurrDelimiter
              else
                // if the length of current delimeter > 1 - we should remove chars
                // that were added by NewDelimiter + FSt before CheckDelimeter = True
                if (Length(FCurrDelimiter) > 1) and (NewDelimiter[Length(NewDelimiter)] = FCurrDelimiter[1]) then
                  Delete(NewDelimiter, Length(NewDelimiter), 1);
            end
          end;
          if (FDelimiterState = dsDelimiter) and (Code = lcBlank) then
            FDelimiterState := dsBlank;
          Ready := (FDelimiterState = dsSet) or (Code = lcEnd) or GetReady(Code);
          if not Ready and (Code <> lcString) then
            if FCurrDelimiter = FOwner.FDelimiter{';'} {old mode} then begin
              Ready := (CheckDelimeter and not IsSpecificSQL(StatementType));
              if not Ready and (FSt = '/') and (FParser.PrevCol = 0) then begin
                TempCode := FParser.GetNext(TempSt);
                if (TempCode = lcEnd) or
                  (TempCode = lcBlank) and ((Pos(#13, TempSt) > 0) or (Pos(#10, TempSt) > 0))
                then
                  Ready := True
                else
                  FParser.Back;
              end;
            end
            else
              Ready := CheckDelimeter;

          if Ready then begin
            s := FSQL.ToString;
            if (Code <> lcEnd) and (Length(FCurrDelimiter) > 1) and
              (Length(s) > 0) and (s[Length(s)] = FCurrDelimiter[1]) then begin
              s := Copy(s, 1, Length(s) - Length(FCurrDelimiter) + 1);
              FOwner.FEndPos := FOwner.FEndPos - Length(FCurrDelimiter) + 1;
              FOwner.FEndOffset := FOwner.FEndOffset - Length(FCurrDelimiter) + 1;
            end;

            if (Trim(s) <> '') or GetReady(Code) then begin
              // Execution
              Omit := (FDelimiterState = dsSet) or Omit;
              if FDelimiterState = dsSet then begin
                // DelimiterState := dsNone;
                FCurrDelimiter := NewDelimiter;
              end;
              ExecuteStatement(s, StatementType, Omit, BreakExecution);
              if not Omit and BreakExecution then
                break;
              if Assigned(FParser) then begin //if not BreakExec
                FOwner.FStmtOffset := FParser.CurrPos;
                FOwner.FStartPos := FParser.CurrPos;
                FOwner.FStartLine := FParser.CurrLine;
                FOwner.FStartOffset := FParser.CurrCol;
              end;
              Result := True;
              break;
            end;
          end
          else begin
            CheckLexem(Code, StatementType, Omit);
            if (Code = lcString) or (Code <> lcBlank) or (FSQL.Length <> 0) then begin
              FSQL.Append(FSt);

              FOwner.FEndPos := FParser.CurrPos - 1;
              FOwner.FEndLine := FParser.CurrLine;
              FOwner.FEndOffset := FParser.CurrCol - 1;
            end
            else begin
              Inc(FOwner.FStmtOffset, Length(FSt));
              //to correct start position
              FOwner.FStartLine := FParser.CurrLine;
              FOwner.FStartPos := FParser.CurrPos;
              FOwner.FStartOffset := FParser.CurrCol;
            end;
          end;
        until Code = lcEnd;
        if not Result then begin
          FParser.Free;
          FParser := nil;
        end;
      finally
        if Assigned(FOwner.DataSet) then begin
          FOwner.DataSet.Debug := OldDebug;
        end;
      end;
    end;
  finally
    FOwner.FcsBreakMultiThread.Leave;
  end;
end;

procedure TDAScriptProcessor.BreakExec;
begin
  FParser.Free;
  FParser := nil;
end;

procedure TDAScriptProcessor.Reset;
begin
end;

procedure TDAScriptProcessor.CreateStatement(StatementType: integer; Omit: boolean; StartPos,
  EndPos, StartLine, EndLine, StartOffset, EndOffset: integer);
begin
  FOwner.FStatements.CreateStatement(StatementType, Omit, StartPos, EndPos,
    StartLine, EndLine, StartOffset, EndOffset);
end;

procedure TDAScriptProcessor.ExecuteStatement(const SQL: _string; StatementType: integer;
  var Omit: Boolean; out BreakExec: boolean; Params: TDAParams = nil);
var
  FinalSQL: _string;
begin
  BreakExec := False;
  if FStatementsPopulating then
    CreateStatement(StatementType, Omit, FOwner.StartPos, FOwner.EndPos, FOwner.StartLine,
      FOwner.EndLine, FOwner.StartOffset, FOwner.EndOffset)
  else
  begin
    if SQL = '' then
      Exit;

    FinalSQL := SQL;
    FOwner.Macros.Expand(FinalSQL);

    if Assigned(FOwner.FBeforeExecute) then
      FOwner.FBeforeExecute(Self, FinalSQL, Omit);
    DoBeforeStatementExecute(FinalSQL, StatementType, Omit);
    if not Omit then begin
      try
        if not (FOwner.UseOptimization and FOwner.FAllowOptimization) then
          FOwner.InternalExecute(FinalSQL, BreakExec, Params)
        else
        begin
          if not CanOptimize(FinalSQL, StatementType) then
            FOwner.Flush(BreakExec);
          if FOwner.FBuffer.Length <> 0 then begin
            FOwner.FBuffer.Append(FOwner.Delimiter);
            FOwner.FBuffer.Append(#$D#$A);
          end;
          FOwner.FBuffer.Append(FinalSQL);
        end;
      finally
        DoAfterStatementExecute(FinalSQL, StatementType);
      end;
      if Assigned(FOwner.FAfterExecute) then
        FOwner.FAfterExecute(Self, FinalSQL);
    end;
  end;
end;

function TDAScriptProcessor.CanOptimize(const SQL: _string; const StatementType: integer): boolean;
begin
  Result := FOwner.FBuffer.Length + Length(SQL) < $FFFF;
end;

function TDAScriptProcessor.IsSpecificSQL(StatementType: integer): boolean;
begin
  Result := StatementType = ST_SPECIFIC_SQL;
end;

function TDAScriptProcessor.GetReady(Code: integer): boolean;
begin
  Result := False;
end;

procedure TDAScriptProcessor.DoBeforeStatementExecute(var SQL: _string; StatementType: integer; var Omit: boolean);
begin
end;

procedure TDAScriptProcessor.DoAfterStatementExecute(var SQL: _string; StatementType: integer);
begin
end;

procedure TDAScriptProcessor.CheckLexem(Code: integer; var StatementType: integer; var Omit: boolean);
begin
  if (Code = lcComment) and (StatementType = ST_UNKNOWN) then
    StatementType := ST_COMMENT
  else
    if (StatementType = ST_COMMENT) and (Code <> lcComment) and
      (Code <> lcBlank) then
      StatementType := ST_UNKNOWN;
end;

function TDAScriptProcessor.IsBlankEndsDelimeter: boolean;
begin
  //This function determine if lxBlank ends delimeter settings  
  Result := False;
end;

{ TDAScript }

constructor TDAScript.Create(Owner: TComponent);
begin
  inherited Create(Owner);
  FcsBreakMultiThread := TCriticalSection.Create;
  FDataSource := TDataSource.Create(nil);
  FSQL := _TStringList.Create;
  _TStringList(FSQL).OnChange := SQLChanged;
  FCommand := CreateCommand;
  TDBAccessUtils.SetAutoCommit(FCommand, False);
  FMacros := TMacros.Create(nil);
  FOnError := nil;
  FErrorOffset := 0;
  FDelimiter := ';';
  FSQLActual := True;
  FBuffer := _StringBuilder.Create;
  FBreakExecution := False;
  FDesignCreate := csDesigning in ComponentState;
end;

destructor TDAScript.Destroy;
begin
  FMacros.Free;
  FCommand.Free;
  FSQL.Free;
  FDataSource.Free;
  FBuffer.Free;
  FStatements.Free;
  FProcessor.Free;
  FcsBreakMultiThread.Free;

  inherited;
end;

procedure TDAScript.Loaded;
begin
  inherited;
  FDesignCreate := False;
end;

procedure TDAScript.DefineProperties(Filer: TFiler);
  function WriteMacros: boolean;
  begin
    if Filer.Ancestor <> nil then
      Result := not FMacros.IsEqual(TDAScript(Filer.Ancestor).FMacros)
    else
      Result := FMacros.Count > 0;
  end;
begin
  inherited DefineProperties(Filer);
  Filer.DefineProperty('MacroData', ReadMacroData, WriteMacroData, WriteMacros);
end;

procedure TDAScript.ReadMacroData(Reader: TReader);
begin
  Reader.ReadValue;
  Reader.ReadCollection(FMacros);
end;

procedure TDAScript.WriteMacroData(Writer: TWriter);
begin
  Writer.WriteCollection(FMacros);
end;

procedure TDAScript.Execute;
var
  UsedCon: TCustomDAConnection;
begin
  FBreakExecution := False;
  CheckProcessor;
  UsedCon := nil;
  if not FProcessor.FStatementsPopulating then begin
    if Assigned(DataSet) then
      UsedCon := TDBAccessUtils.UsedConnection(DataSet)
    else
      UsedCon := TDBAccessUtils.UsedConnection(FCommand);
    if UsedCon = nil then
      raise EDatabaseError.Create(SConnectionNotDefined);
    if not FNoPreconnect then
      TDBAccessUtils.InternalConnect(UsedCon);
    if Assigned(DataSet) then
      DataSet.DisableControls;
  end;
  try
    FAllowOptimization := True;
    try
      FProcessor.FCurrDelimiter := FDelimiter;
      while (not FBreakExecution) and ExecuteNext do;
      if not FBreakExecution then
        Flush(FBreakExecution);
    finally
      FAllowOptimization := False;
      FcsBreakMultiThread.Enter;
      try
        FProcessor.FParser.Free;
        FProcessor.FParser := nil;
      finally
        FcsBreakMultiThread.Leave;
      end;
    end;
  finally
    if not FProcessor.FStatementsPopulating then begin
      if Assigned(DataSet) then
        DataSet.EnableControls;
      if not FNoPreconnect then
        TDBAccessUtils.InternalDisconnect(UsedCon);
    end;
  end;
end;

procedure TDAScript.Open(Stream: TStream);
begin
  Assert(FStream = nil);
  FStream := Stream;
end;

procedure TDAScript.Close;
begin
  FStream := nil;
end;

procedure TDAScript.InternalExecute(const SQL: _string; out BreakExec: boolean;
  Params: TDAParams = nil); // Sends SQL to server
var
  Action: TErrorAction;
  ParamsDest: TDAParams;
begin
  if Assigned(DataSet) then begin
    DataSet.SQL.Text := SQL;
    ParamsDest := DataSet.Params;
  end
  else begin
    FCommand.SQL.Text := SQL;
    ParamsDest := FCommand.Params;
  end;

  Assert(ParamsDest <> nil);
  if Params <> nil then
    ParamsDest.AssignValues(Params);

  try
    if Assigned(DataSet) then
      DataSet.Execute
    else
      FCommand.Execute;

  except
    on EAbort do
      raise;
    on E: Exception do begin
      CalculateErrorOffset(E);
      Action := eaException;
      if Assigned(FOnError) then
        FOnError(Self, E, Trim(SQL), Action);
      case Action of
        eaAbort:
          BreakExec := True;
        eaFail:
          raise;
        eaException:
          if Assigned(ApplicationHandleException) then
            ApplicationHandleException(E)
          else
            raise;
      end;
    end;
  end;
end;

procedure TDAScript.Flush(out BreakExec: boolean);
begin
  if FBuffer.Length <> 0 then // Flush
    InternalExecute(FBuffer.ToString, BreakExec);
  FBuffer.Length := 0;
end;

procedure TDAScript.ExecuteStream(Stream: TStream);
begin
  Open(Stream);
  try
    Execute;
  finally
    Close;
  end;
end;

procedure TDAScript.ExecuteFile(const FileName: string);
var
  FileStream: TFileStream;
begin
  FileStream := TFileStream.Create(FileName, fmOpenRead);
  try
    ExecuteStream(FileStream);
  finally
    FileStream.Free;
  end;
end;

function TDAScript.FindMacro(Name: _string): TMacro;
begin
  Result := FMacros.FindMacro(Name);
end;

function TDAScript.MacroByName(Name: _string): TMacro;
begin
  Result := FMacros.MacroByName(Name);
end;

function TDAScript.ErrorOffset: Int64;
begin
  Result := FErrorOffset;
end;

procedure TDAScript.AssignTo(Dest: TPersistent);
begin
  if Dest is TDAScript then begin
    TDAScript(Dest).SetConnection(GetConnection);
    TDAScript(Dest).SQL.Text := SQL.Text;
    TDAScript(Dest).Debug := Debug;
    TDAScript(Dest).Macros := Macros;
    TDAScript(Dest).Delimiter := Delimiter;
  end
  else
    inherited;
end;

function TDAScript.UsedConnection: TCustomDAConnection;
begin
  Result := TDBAccessUtils.UsedConnection(FCommand);
end;

function TDAScript.UsedTransaction: TDATransaction;
begin
  if Assigned(DataSet) then
    Result := TDBAccessUtils.UsedTransaction(DataSet)
  else
    Result := TDBAccessUtils.UsedTransaction(FCommand);
end;

function TDAScript.GetConnection: TCustomDAConnection;
begin
  Result := FCommand.Connection;
end;

procedure TDAScript.SetConnection(Value: TCustomDAConnection);
begin
  FCommand.Connection := Value;
end;

function TDAScript.GetTransaction: TDATransaction;
begin
  Result := TDBAccessUtils.GetTransaction(FCommand);
end;

procedure TDAScript.SetTransaction(Value: TDATransaction);
begin
  TDBAccessUtils.SetTransaction(FCommand, Value);
end;

function TDAScript.IsTransactionStored: boolean;
begin
  Result := TDBAccessUtils.GetFTransaction(FCommand) <> nil;
end;

function TDAScript.GetProcessorClass: TDAScriptProcessorClass;
begin
  Assert(False);
  Result := TDAScriptProcessor;
end;

procedure TDAScript.SetProcessor(Value: TDAScriptProcessor);
begin
  if Value <> FProcessor then begin
    FreeProcessor;

    FProcessor := Value;
    FMacros.SetParserClass(FProcessor.GetParserClass);
    FCommand.Macros.SetParserClass(FProcessor.GetParserClass);
  end;
end;

procedure TDAScript.CheckProcessor;
begin
  if not (FProcessor is GetProcessorClass) then begin
    FreeProcessor;
    CreateProcessor;
  end;
end;

procedure TDAScript.CreateProcessor;
begin
  SetProcessor(GetProcessorClass.Create(Self));
end;

procedure TDAScript.FreeProcessor;
begin
  FProcessor.Free;
  FProcessor := nil;
end;

function TDAScript.GetSQLText(StartLine, EndLine, StartOffset, EndOffset, Length: integer): _string;
var
  SQL: _StringBuilder;
  i: integer;
  SPos, EPos: integer;
begin
  SQL := _StringBuilder.Create(Length);
  try
    for i := StartLine to EndLine do begin
      if i = StartLine then
        SPos := StartOffset + 1
      else
        SPos := 1;
      if i = EndLine then
        EPos := EndOffset + 1
      else
        EPos := {$IFDEF CLR}Borland.Delphi.{$ENDIF}System.Length(FSQL[i]);
      SQL.Append(Copy(FSQL[i], SPos, EPos - SPos + 1));
      if i <> EndLine then
        SQL.Append(#$D#$A);
    end;
    Result := SQL.ToString;
  finally
    SQL.Free;
  end;
end;

procedure TDAScript.SetSQL(Value: _TStrings);
begin
  if FSQL.Text <> Value.Text then begin
    FSQL.BeginUpdate;
    try
      FSQL.Assign(Value);
    finally
      FSQL.EndUpdate;
    end;
  end;
end;

procedure TDAScript.SQLChanged(Sender: TObject);
begin
  FMacros.Scan(FSQL.Text);
  BreakExec;
  FStatements.Free;
  FStatements := nil;
  FSQLActual := True;
end;

function TDAScript.GetDebug: boolean;
begin
  Result := FCommand.Debug;
end;

procedure TDAScript.SetDebug(Value: boolean);
begin
  FCommand.Debug := Value;
end;

procedure TDAScript.SetMacros(Value: TMacros);
begin
  FMacros.Assign(Value);
end;

function TDAScript.GetDataSet: TCustomDADataSet;
begin
  Result := FDataSource.DataSet as TCustomDADataSet;
end;

procedure TDAScript.SetDataSet(Value: TCustomDADataSet);
begin
  FDataSource.DataSet := Value;
end;

procedure TDAScript.BreakExec;
begin
  FBreakExecution := True;
  FcsBreakMultiThread.Enter;
  try
    if FProcessor <> nil then
      FProcessor.BreakExec;
    FStmtOffset := 0;
    FStartPos := 0;
    FStartLine := 0;
    FEndPos := 0;
    FEndLine := 0;
  finally
    FcsBreakMultiThread.Leave;
  end;
end;

function TDAScript.CreateCommand: TCustomDASQL;
begin
  Result := TCustomDASQL.Create(nil);
end;

procedure TDAScript.CalculateErrorOffset(E: Exception);
begin
  FErrorOffset := FStmtOffset;
end;

function TDAScript.CreateStatementsObject: TDAStatements;
begin
  Result := TDAStatements.Create(TDAStatement, Self);
end;

function TDAScript.GetStatements: TDAStatements;
begin
  if FStatements = nil then begin
    CheckProcessor;
    FStatements := CreateStatementsObject;
    FProcessor.FStatementsPopulating := True;
    try
      Execute;
    finally
      FProcessor.FStatementsPopulating := False;
    end;
  end;
  Result := FStatements;
end;

function TDAScript.ExecuteNext: boolean;
begin
  CheckProcessor;

  Result := FProcessor.ExecuteNext;
end;

procedure TDAScript.SetDelimiter(const Value: _string);
begin
  FDelimiter := Value;
  FProcessor.FCurrDelimiter := FDelimiter;
  FProcessor.FDelimiterState := dsNone;
end;

function TDAScript.GetParams: TDAParams;
begin
  if Assigned(DataSet) then
    Result := DataSet.Params
  else
    Result := FCommand.Params;
end;

procedure TDAScript.SetAutoCommit(Value: Boolean);
begin
  FAutoCommit := Value;
  TDBAccessUtils.SetAutoCommit(FCommand, FAutoCommit);
end;

function TDAScript.IsDelimiterStored: boolean;
begin
  Result := FDelimiter <> ';';
end;

{ TDAScriptUtils }

class procedure TDAScriptUtils.SetDesignCreate(Obj: TDAScript; Value: boolean);
begin
  Obj.FDesignCreate := Value;
end;

class function TDAScriptUtils.GetDesignCreate(Obj: TDAScript): boolean;
begin
  Result := Obj.FDesignCreate;
end;

class procedure TDAScriptUtils.SetCommand(Obj: TDAScript; Command: TCustomDASQL);
begin
  Obj.FCommand := Command;
end;

class function TDAScriptUtils.GetCommand(Obj: TDAScript): TCustomDASQL;
begin
  Result := Obj.FCommand;
end;

class procedure TDAScriptUtils.Open(Obj: TDAScript; Stream: TStream);
begin
  Obj.Open(Stream);
end;

class procedure TDAScriptUtils.Close(Obj: TDAScript);
begin
  Obj.Close;
end;

class function TDAScriptUtils.UsedConnection(Obj: TDAScript): TCustomDAConnection;
begin
  Result := Obj.UsedConnection;
end;

class function TDAScriptUtils.UsedTransaction(Obj: TDAScript): TDATransaction;
begin
  Result := Obj.UsedTransaction;
end;

class function TDAScriptUtils.GetTransaction(Obj: TDAScript): TDATransaction;
begin
  Result := Obj.Transaction;
end;

class procedure TDAScriptUtils.SetTransaction(Obj: TDAScript; Value: TDATransaction);
begin
  Obj.Transaction := Value;
end;

end.

