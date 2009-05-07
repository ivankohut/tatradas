unit StringUtilitiesTests;

interface

uses
  TatraDASTestFramework;

type
  TStringUtilitiesTests = class(TTestCase)
  published
    procedure TestInjectStr;
  end;


implementation

uses
  StringUtilities;

{ TStringUtilitiesTests }

procedure TStringUtilitiesTests.TestInjectStr;
begin
  Check(InjectStr('aaa', []) = 'aaa');
  Check(InjectStr('aaa %s bbb', ['XXX']) = 'aaa XXX bbb');
  Check(InjectStr('aaa %s bbb %s', ['XXX', 'YYY']) = 'aaa XXX bbb YYY');
  Check(InjectStr('aaa eE bbb eEa', ['XXX', 'YYY'], 'eE') = 'aaa XXX bbb YYYa');
end;

initialization
  RegisterTest(TStringUtilitiesTests);

end.