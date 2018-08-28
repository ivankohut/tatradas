unit DisassemblerUtils;

interface

const
  HexDigits: array[0..15] of Char = '0123456789ABCDEF';

function DataToHex(var Data; Count: Integer): string;
function ByteToSignedHex(AValue: Byte): string;


implementation

uses
  SysUtils;

type
  TArrayOfByte = array of Byte;



function DataToHex(var Data; Count: Integer): string;
var
  DataIndex: Integer;
  b: Byte;
begin
  SetLength(Result, Count * 2);
  for DataIndex := 0 to Count - 1 do begin
    b := TArrayOfByte(@Data)[DataIndex];
    Result[2 * DataIndex + 2] := HexDigits[b and 15];
    b := b shr 4;
    Result[2 * DataIndex + 1] := HexDigits[b];
  end;
end;



function WordToHex(var Data): string;
var
  w: Cardinal;
begin
  SetLength(Result, 4);
  w := Cardinal(Data);
  Result[4] := HexDigits[w and 15];
  w := w shr 4;
  Result[3] := HexDigits[w and 15];
  w := w shr 4;
  Result[2] := HexDigits[w and 15];
  w := w shr 4;
  Result[1] := HexDigits[w and 15];
end;



function ByteToSignedHex(AValue: Byte): string;
begin
  if AValue <= 127 then
    Result := '+0x' + IntToHex(AValue, 2)
  else
    Result := '-0x' + IntToHex(Abs(ShortInt(AValue)), 2);
end;


end.
