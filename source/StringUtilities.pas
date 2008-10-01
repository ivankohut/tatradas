{
  TODO:
     - CarToHex by mal asi nejako pouzivat parameter Digits, nie ???
     - optimalizovat CarToHex

}

unit StringUtilities;

interface

uses
  SysUtils,
  Math,
  StrUtils;


function CarToStr(Value: cardinal): string;
function CarToHex(Value: cardinal; Digits: integer): string;

function IsHexNumber(HexNumber: string): boolean;

function StringLeftPad(AString: string; Size: integer): string; overload;
function StringLeftPad(AString: string; Size: integer; PadChar: char): string; overload;

function StringRightPad(AString: string; Size: integer): string; overload;
function StringRightPad(AString: string; Size: integer; PadChar: char): string; overload;

function IntToSignedHex(Value: integer; Digits: Integer): string;
function InsertStr(const Source: string; const Dest: string; Index: integer): string;

function FirstCommaToPoint(AText: string):string;


implementation


function StringLeftPad(AString: string; Size: integer): string;
begin
  result:= StringLeftPad(AString, Size, ' ');
end;



function StringLeftPad(AString: string; Size: integer; PadChar: char): string;
var
  i: integer;
  PadCharCount: integer;
begin
{
  result:= AString;
  for i:=1 to Size - Length(AString) do
    result:= PadChar + result;
}
  PadCharCount:= Size - Length(AString);
  SetLength(result, Max(Size, Length(AString)));
  for i:=1 to PadCharCount do
    result[i]:= PadChar;
  result:= result + AString;
end;



function StringRightPad(AString: string; Size: integer): string;
begin
  result:= StringRightPad(AString, Size, ' ');
end;



function StringRightPad(AString: string; Size: integer; PadChar: char): string;
var
  i: integer;
begin
  SetLength(result, Max(Length(AString), Size));

  // Copy AString into result
  for i:=1 to Length(AString) do
    result[i]:= AString[i];

  // Fill the rest of result with PadChar
  for i:=Length(AString)+1 to Size do
    result[i]:= PadChar;
end;



function CarToStr(Value: cardinal): string;
begin
  result:= '';
  while Value <> 0 do begin
    result:= result + Chr(Value mod 10);
    value:= value div 10;
  end;
  result:= ReverseString(result);
end;



function CarToHex(Value: cardinal; Digits: integer): string;
begin
  if Value = 0 then
    result := '0'
  else begin
    result := '';
    while Value <> 0 do begin
      result := result + IntToHex(Value mod 16, 1);
      Value := value div 16;
    end;
    result := ReverseString(result);
  end;
end;



function IsHexNumber(HexNumber: string): boolean;
var
  Index: integer;
begin
  result:= false;
  for Index:= 1 to Length(HexNumber) do
    if not (HexNumber[Index] in ['0'..'9','A'..'F','a'..'f']) then
      Exit;
  result:= true;
end;




function IntToSignedHex(Value: integer; Digits: Integer): string;
begin
  if Value >= 0 then
    result:= IntToHex(Value, Digits)
  else begin
    Value:= Abs(Value);
    result:= '-' + IntToHex(Value, Digits);
  end;
end;



function InsertStr(const Source: string; const Dest: string; Index: integer): string;
var
  s: string;
begin
  s:= dest;
  Insert(Source, s, Index);
  result:= s;
end;



function FirstCommaToPoint(AText: string): string;
var i: integer;
begin
  for i:=1 to Length(AText) do
    if AText[i]=',' then begin
      AText[i]:='.';
      break;
    end;
  result:=AText;
end;

end.
