unit StringUtilitiesTests;

interface

uses
  TatraDASTestFramework;

type
  TStringUtilitiesTests = class(TTestCase)
  published
    procedure TestInjectStr;
    procedure TestCarToStr;
    procedure TestCarToHex;

    procedure TestStringRightPad;
    procedure TestStringLeftPad;
  end;


implementation

uses
  StringUtilities;

{ TStringUtilitiesTests }

procedure TStringUtilitiesTests.TestCarToHex;
begin
  Check(CarToHex(0, 0) = '0');
  Check(CarToHex(0, 1) = '0');
  Check(CarToHex(0, 10) = '0000000000');
  Check(CarToHex(1, 0) = '1');
  Check(CarToHex(1, 1) = '1');
  Check(CarToHex(1, 10) = '0000000001');
  Check(CarToHex($123, 0) = '123');
  Check(CarToHex($123, 1) = '123');
  Check(CarToHex($123, 10) = '0000000123');
end;

procedure TStringUtilitiesTests.TestCarToStr;
begin
  Check(CarToStr(0) = '0');
  Check(CarToStr(1) = '1');
  Check(CarToStr($FFFFFFFF) = '4294967295');
end;

procedure TStringUtilitiesTests.TestInjectStr;
begin
  Check(InjectStr('aaa', []) = 'aaa');
  Check(InjectStr('aaa %s bbb', ['XXX']) = 'aaa XXX bbb');
  Check(InjectStr('aaa %s bbb %s', ['XXX', 'YYY']) = 'aaa XXX bbb YYY');
  Check(InjectStr('aaa eE bbb eEa', ['XXX', 'YYY'], 'eE') = 'aaa XXX bbb YYYa');
end;

procedure TStringUtilitiesTests.TestStringRightPad;
begin
  Check(StringRightPad('', 0, 'X') = ''); 
  Check(StringRightPad('', 5, 'X') = 'XXXXX');
  Check(StringRightPad('abc', 5, 'X') = 'abcXX');
  Check(StringRightPad('abcABCabc', 5, 'X') = 'abcABCabc');
end;

procedure TStringUtilitiesTests.TestStringLeftPad;
begin
  Check(StringLeftPad('', 0, 'X') = '');
  Check(StringLeftPad('', 5, 'X') = 'XXXXX');
  Check(StringLeftPad('abc', 5, 'X') = 'XXabc');
  Check(StringLeftPad('abcABCabc', 5, 'X') = 'abcABCabc');
end;

initialization
  RegisterTest(TStringUtilitiesTests);

end.