//////////////////////////////////////////////////
//  Copyright © 1998-2012 Devart. All right reserved.
//////////////////////////////////////////////////

{$IFNDEF CLR}

{$I Dac.inc}

unit CRBase64;
{$ENDIF}

interface

uses
  SysUtils,
{$IFNDEF CLR}
  CLRClasses,
{$ENDIF}
  CRTypes;

type
  TBase64 = class
  public
    class function Encode(const data: TBytes): TBytes; overload;
    class function Encode(const data: TBytes; offset, length: Integer): TBytes; overload;
    class function Decode(const data: TBytes): TBytes; overload;
    class function Decode(const data: TBytes; offset, length: Integer): TBytes; overload;
  end;

implementation

const
  PAD: Integer = 64;
  
  _fromBase64: array[0..127] of Integer = (
                 -1, -1, -1, -1, -1, -1, -1, -1,
                 -1, -1, -1, -1, -1, -1, -1, -1,
                 -1, -1, -1, -1, -1, -1, -1, -1,
                 -1, -1, -1, -1, -1, -1, -1, -1,
                 -1, -1, -1, -1, -1, -1, -1, -1,
                 -1, -1, -1, 62, -1, -1, -1, 63,
                 52, 53, 54, 55, 56, 57, 58, 59,
                 60, 61, -1, -1, -1, -1, -1, -1,
                 -1,  0,  1,  2,  3,  4,  5,  6,
                 7,  8,  9, 10, 11, 12, 13, 14,
                 15, 16, 17, 18, 19, 20, 21, 22,
                 23, 24, 25, -1, -1, -1, -1, -1,
                 -1, 26, 27, 28, 29, 30, 31, 32,
                 33, 34, 35, 36, 37, 38, 39, 40,
                 41, 42, 43, 44, 45, 46, 47, 48,
                 49, 50, 51, -1, -1, -1, -1, -1
                 );

  _toBase64: array[0..64] of byte = (
                 // 'A','B','C','D','E','F','G','H','I','J','K','L','M','N','O','P',
                 65, 66, 67, 68, 69, 70, 71, 72, 73, 74, 75, 76, 77, 78, 79, 80,
                 // 'Q','R','S','T','U','V','W','X','Y','Z','a','b','c','d','e','f',
                 81, 82, 83, 84, 85, 86, 87, 88, 89, 90, 97, 98, 99,100,101,102,
                 // 'g','h','i','j','k','l','m','n','o','p','q','r','s','t','u','v',
                 103,104,105,106,107,108,109,110,111,112,113,114,115,116,117,118,
                 // 'w','x','y','z','0','1','2','3','4','5','6','7','8','9','+','/'
                 119,120,121,122, 48, 49, 50, 51, 52, 53, 54, 55, 56, 57, 43, 47, Ord('=')
                 );

class function TBase64.Encode(const data: TBytes): TBytes;
begin
  Result := Encode(data, 0, Length(data));
end;

class function TBase64.Decode(const data: TBytes): TBytes;
begin
  Result := Decode(data, 0, Length(data));
end;

class function TBase64.Decode(const data: TBytes; offset, length: Integer): TBytes;
var
  i, j, c, v: Integer;
  n, bits, size: Integer;
begin
  bits := 0;
  n := offset + length;
  size := (length * 3) div 4;
  SetLength(Result, size);

  v := 0;
  j := 0;
  for i := offset to n - 1 do begin
    c := _fromBase64[data[i]];
    if c < 0 then begin
      if data[i] = _toBase64[PAD] then
        break;
      continue;
    end;

    v := (v shl 6) or c;
    bits := bits + 6;
    if bits >= 8 then begin
      bits := bits - 8;
      Result[j] := byte((v shr bits) and $ff);
      Inc(j);
    end;
  end;

  if size > j then
    SetLength(Result, j);
end;

class function TBase64.Encode(const data: TBytes; offset, length: Integer): TBytes;
var
  x1, x2, x3, x4: Integer;
  i, j, r, n: Integer;
  size: Integer;
begin
  r := length mod 3;
  n := offset + (length - r);
  if r <> 0 then size := 4
  else size := 0;
  size := (length div 3) * 4 + size;
  SetLength(Result, size);

  i := offset;
  j := 0;
  while i < n do begin
    x1 := (data[i] and $fc) shr 2;
    x2 := (((data[i    ] and $03) shl 4) or ((data[i + 1] and $f0) shr 4));
    x3 := (((data[i + 1] and $0f) shl 2) or ((data[i + 2] and $c0) shr 6));
    x4 := (data[i + 2] and $3f);
    Result[j    ] := _toBase64[x1];
    Result[j + 1] := _toBase64[x2];
    Result[j + 2] := _toBase64[x3];
    Result[j + 3] := _toBase64[x4];
    i := i + 3;
    j := j + 4
  end;

  if r <> 0 then begin
    x1 := (data[i] and $fc) shr 2;
    x2 := (data[i] and $03) shl 4;
    x3 := PAD;
    x4 := PAD;
    if r = 2 then begin
      x2 := x2 or ((data[i + 1] and $f0) shr 4);
      x3 := (data[i + 1] and $0f) shl 2;
    end;
    Result[j] := _toBase64[x1]; Inc(j);
    Result[j] := _toBase64[x2]; Inc(j);
    Result[j] := _toBase64[x3]; Inc(j);
    Result[j] := _toBase64[x4]; //Inc(j);
  end;

end;

end.

