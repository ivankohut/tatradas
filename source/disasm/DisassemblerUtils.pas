unit DisassemblerUtils;

interface

const
  HexDigits: array[0..15] of char = '0123456789ABCDEF';

function DataToHex(var Data; Count: Integer): string;


implementation

uses
  SysUtils;


type
  TArrayOfByte = array of Byte;

function DataToHex(var Data; Count: Integer): string;
var
  DataIndex: integer;
  b: byte;
begin
  SetLength(result, Count * 2);
  for DataIndex := 0 to Count - 1 do begin
    b := TArrayOfByte(@Data)[DataIndex];
    result[2*DataIndex + 2] := HexDigits[b and 15];
    b := b shr 4;
    result[2*DataIndex + 1] := HexDigits[b];
  end;
end;



end.