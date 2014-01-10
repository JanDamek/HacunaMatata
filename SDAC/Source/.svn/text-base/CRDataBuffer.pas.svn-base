//////////////////////////////////////////////////
//  Copyright © 1998-2012 Devart. All right reserved.
//////////////////////////////////////////////////

{$IFNDEF CLR}

{$I Dac.inc}

unit CRDataBuffer;
{$ENDIF}

interface

uses
  Classes, SysUtils,
{$IFDEF CLR}
  System.Collections, System.Threading, System.Runtime.InteropServices,
{$ELSE}
  CLRClasses,
{$ENDIF}
  CRTypes;

type
  // DataBuffer class implements a temporary storage
  // It writes data to the end of buffer and reads from beginning
  TDataBuffer = class
  private
    FReceiveBuffer: TList; // queue that contains written data

    FReadBuffer: IntPtr;  // current read buffer
    FWriteBuffer: IntPtr; // current not finished write buffer
    FReadPos: Integer;
    FWritePos: Integer;
    FChunkSize: Integer;
    function GetDataLength: Integer;

  public
    constructor Create(ChunkSize: Integer);
    destructor Destroy; override;
    procedure Clear;

    procedure Read(const Data: TBytes; Offset, Count: Integer);
    procedure Write(const Data: TBytes; Offset, Count: Integer);
    function SearchFromIndex(const SearchByte: Byte; const Index: Integer): Integer;

    property DataLength: Integer read GetDataLength;
  end;

implementation

uses
  MemUtils;

const
  SNotEnoughData = 'There is no enough data to read';

{ TDataBuffer }

constructor TDataBuffer.Create(ChunkSize: Integer);
begin
  inherited Create;

  FReceiveBuffer := TList.Create;
  FReadPos := 0;
  FWritePos := 0;

  // add first chunk to the buffer
  FChunkSize := ChunkSize;
  FWriteBuffer := Marshal.AllocHGlobal(ChunkSize);
  FReadBuffer := FWriteBuffer;
  FReceiveBuffer.Insert(0, FWriteBuffer);
end;

destructor TDataBuffer.Destroy;
var
  DelBlock: IntPtr;
begin
  while FReceiveBuffer.Count > 0 do begin
    DelBlock := IntPtr(FReceiveBuffer.Last);
    FReceiveBuffer.Delete(FReceiveBuffer.Count - 1);
    Marshal.FreeHGlobal(DelBlock);
  end;

  FReceiveBuffer.Free;
  inherited;
end;

procedure TDataBuffer.Clear;
var
  DelBlock: IntPtr;
begin
  while FReceiveBuffer.Count > 0 do begin
    DelBlock := IntPtr(FReceiveBuffer.Last);
    FReceiveBuffer.Delete(FReceiveBuffer.Count - 1);
    Marshal.FreeHGlobal(DelBlock);
  end;

  FReadPos := 0;
  FWritePos := 0;
  FWriteBuffer := Marshal.AllocHGlobal(FChunkSize);
  FReadBuffer := FWriteBuffer;
  FReceiveBuffer.Insert(0, FWriteBuffer);
end;

procedure TDataBuffer.Read(const Data: TBytes; Offset, Count: Integer);

  function Min(const A, B: Integer): Integer;
  begin
    if A < B then
      Result := A
    else
      Result := B;
  end;

var
  cnt: Integer;
  DelBlock: IntPtr;
begin
  while Count > 0 do begin
    if FReceiveBuffer.Count = 1 then
      cnt := Min(FWritePos - FReadPos, Count)
    else
      cnt := Min(FChunkSize - FReadPos, Count);

    if cnt <= 0 then
      raise Exception.Create(SNotEnoughData);

    Marshal.Copy(PtrOffset(FReadBuffer, FreadPos), Data, Offset, cnt);
    FReadPos := FReadPos + cnt;
    offset := Offset + cnt;
    Count := Count - cnt;

    if FReadPos >= FChunkSize then begin
      if FReceiveBuffer.Count > 1 then begin
        DelBlock := IntPtr(FReceiveBuffer.Last);
        FReceiveBuffer.Delete(FReceiveBuffer.Count - 1);
        Marshal.FreeHGlobal(DelBlock);

        FReadBuffer := IntPtr(FReceiveBuffer.Last);
        FReadPos := 0;
      end
      else
        raise Exception.Create(SNotEnoughData);
    end;
  end;
end;

procedure TDataBuffer.Write(const Data: TBytes; Offset, Count: Integer);
var
  cnt: Integer;
begin
  while Count > 0 do begin
    if FChunkSize - FWritePos < Count then
      cnt := FChunkSize - FWritePos
    else
      cnt := Count;

    Marshal.Copy(Data, Offset, PtrOffset(FWriteBuffer, FWritePos), cnt);
    FWritePos := FWritePos + cnt;

    // check if we need more buffer
    if FWritePos >= FChunkSize then begin
      FWriteBuffer := Marshal.AllocHGlobal(FChunkSize);
      FReceiveBuffer.Insert(0, FWriteBuffer);
      FWritePos := 0;
    end;

    Offset := Offset + cnt;
    Count := Count - cnt;
  end;
end;

function TDataBuffer.GetDataLength: Integer;
begin
  if FReceiveBuffer.Count = 0 then
    Result := 0
  else
    Result := (FReceiveBuffer.Count - 1) * FChunkSize + FWritePos - FReadPos;
end;

function TDataBuffer.SearchFromIndex(const SearchByte: Byte; const Index: Integer): Integer;
var
  buf: IntPtr;
  StartPos, EndPos: Integer;
  i, j, n: Integer;
begin
  Result := -1;
  n := (Index + FReadPos) div FChunkSize;
  StartPos := Index + FReadPos - n * FChunkSize;

  for i := FReceiveBuffer.Count - 1 - n downto 0 do begin
    buf := IntPtr(FReceiveBuffer[i]);

    if i > 0 then
      EndPos := FChunkSize - 1
    else
      EndPos := FWritePos - 1;

    for j := StartPos to EndPos do
      if Marshal.ReadByte(buf, j) = SearchByte then begin
        Result := (FReceiveBuffer.Count - 1 - i) * FChunkSize + j - FReadPos;
        Exit;
      end;

    StartPos := 0;
  end;
end;

end.
