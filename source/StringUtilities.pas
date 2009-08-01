unit StringUtilities;

interface

uses
  SysUtils,
  Math,
  StrUtils;

// Convert AValue to string
function CarToStr(AValue: Cardinal): string;

// Convert Value to hex string using at least Digits digits
function CarToHex(Value: Cardinal; Digits: Integer): string;

function IsHexNumber(HexNumber: string): boolean;

// Same as StringLeftPad(AString, Size, PadChar) using space as PadChar
function StringLeftPad(AString: string; Size: Integer): string; overload;

// Append PadChar characters (to the left) to AString to make it at least Size characters long.
// Returns AString if it's length is Size or more
function StringLeftPad(AString: string; Size: Integer; PadChar: Char): string; overload;

// Same as StringRightPad(AString, Size, PadChar) using space as PadChar 
function StringRightPad(AString: string; Size: Integer): string; overload;

// Append PadChar characters (to the right) to AString to make it at least Size characters long.
// Returns AString if it's length is Size or more
function StringRightPad(AString: string; Size: Integer; PadChar: Char): string; overload;

function IntToSignedHex(Value: integer; Digits: Integer): string;
function InsertStr(const Source: string; const Dest: string; Index: integer): string;

function FirstCommaToPoint(AText: string):string;

function InjectStr(const AString: string; AValues: array of string; const AReplaceMark: string = '%s'): string;


implementation


function StringLeftPad(AString: string; Size: Integer): string;
begin
  Result := StringLeftPad(AString, Size, ' ');
end;



function StringLeftPad(AString: string; Size: Integer; PadChar: Char): string;
var
  i: integer;
  PadCharCount: integer;
begin
  if Length(AString) >= Size then
    Result := AString
  else begin
    PadCharCount := Size - Length(AString);
    SetLength(Result, Size);
    for i := 1 to PadCharCount do
      Result[i] := PadChar;

    Move(AString[1], Result[PadCharCount + 1], Length(AString));
  end;
end;



function StringRightPad(AString: string; Size: Integer): string;
begin
  result:= StringRightPad(AString, Size, ' ');
end;



function StringRightPad(AString: string; Size: Integer; PadChar: Char): string;
var
  i: Integer;
begin
  SetLength(Result, Max(Length(AString), Size));

  // Copy AString into result
  for i := 1 to Length(AString) do
    Result[i] := AString[i];

  // Fill the rest of result with PadChar
  for i := Length(AString) + 1 to Size do
    Result[i] := PadChar;
end;



function CarToStr(AValue: Cardinal): string;
begin
  if AValue = 0 then
    Result := '0'
  else begin
    Result := '';
    while AValue <> 0 do begin
      Result := Result + Chr((AValue mod 10) + 48);
      AValue := AValue div 10;
    end;
    Result := ReverseString(Result);
  end;
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
  if Length(Result) < Digits then
    Result := StringLeftPad(Result, Digits, '0');
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
