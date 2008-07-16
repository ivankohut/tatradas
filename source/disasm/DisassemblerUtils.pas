unit DisassemblerUtils;

interface

const
  HexDigits: array[0..15] of char = '0123456789ABCDEF';

function DataToHex(var Data; Count: Integer): string;
function ByteToSignedHex(AValue: byte): string;


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



function WordToHex(var Data): string;
var
  w: cardinal;
begin
  SetLength(result, 4);
  w := cardinal(Data);
  result[4] := HexDigits[w and 15];
  w := w shr 4;
  result[3] := HexDigits[w and 15];
  w := w shr 4;
  result[2] := HexDigits[w and 15];
  w := w shr 4;
  result[1] := HexDigits[w and 15];
end;



function ByteToSignedHex(AValue: byte): string;
begin
  if AValue <= 127 then
    result := '+0x' + IntToHex(AValue, 2)
  else
    result := '-0x' + IntToHex(Abs(Shortint(AValue)), 2);
end;


end.