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

function InjectStr(const AString: string; AValues: array of string; const AReplaceMark: string = '%s'): string;


implementation


function StringLeftPad(AString: string; Size: Integer): string;
begin
  Result := StringLeftPad(AString, Size, ' ');
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

// PosEx was added in Delphi 7 so in case of Delphi 6 we need this:
{$IFDEF VER140}
function PosEx(const SubStr, S: string; Offset: Cardinal = 1): Integer;
var
  I,X: Integer;
  Len, LenSubStr: Integer;
begin
  if Offset = 1 then
    Result := Pos(SubStr, S)
  else
  begin
    I := Offset;
    LenSubStr := Length(SubStr);
    Len := Length(S) - LenSubStr + 1;
    while I <= Len do
    begin
      if S[I] = SubStr[1] then
      begin
        X := 1;
        while (X < LenSubStr) and (S[I + X] = SubStr[X + 1]) do
          Inc(X);
        if (X = LenSubStr) then
        begin
          Result := I;
          exit;
        end;
      end;
      Inc(I);
    end;
    Result := 0;
  end;
end;
{$ENDIF}

function InjectStr(const AString: string; AValues: array of string; const AReplaceMark: string = '%s'): string;
var
  Position, Index: Integer;
begin
  Result := AString;
  Position := 1;
  Index := 0;
  while True do begin
    Position := PosEx(AReplaceMark, Result, Position);
    if Position > 0 then begin
      Result := StuffString(Result, Position, Length(AReplaceMark), AValues[Index]);
      Inc(Position, Length(AValues[Index]));
      Inc(Index);
    end
    else
      Break;
  end;
end;



end.
