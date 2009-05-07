{ The goal of this "framework" is to unify the use of FPCUnit and DUnit frameworks }
unit TatraDASTestFramework;

interface

uses
  {$IFDEF FPC}
    FPCUnit, TestRegistry;
  {$ELSE}
    TestFrameWork;
  {$ENDIF}

type
  {$IFDEF FPC}
  TTestCase = class (FPCUnit.TTestCase)
  end;
  {$ELSE}
  TTestCase = class (TestFramework.TTestCase)
  end;
  {$ENDIF}

  TTatraDASTestCaseClass = class of TTestCase;

  procedure RegisterTest(ATestCaseClass: TTatraDASTestCaseClass);

implementation


procedure RegisterTest(ATestCaseClass: TTatraDASTestCaseClass);
begin
  {$IFDEF FPC}
    TestRegistry.RegisterTest(ATestCaseClass);
  {$ELSE}
    TestFramework.RegisterTest(ATestCaseClass.Suite);
  {$ENDIF}
end;



end.