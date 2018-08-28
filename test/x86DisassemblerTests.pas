unit x86DisassemblerTests;

interface

uses
  TatraDASTestFramework,
  x86DisassemblerTypes,
  x86Disassembler;

type
  Tx86DisassemblerTests = class(TTestCase)
  published
    procedure TestLoadModRM;
    procedure TestLoadSIB;
  end;

  
implementation


procedure Tx86DisassemblerTests.TestLoadModRM;
var
  ModRM: TModRM;
begin
  ModRM := Tx86Disassembler.LoadModRM($AA);
  Check(ModRM.FullModRM = $AA);
  Check(ModRM.Moder = 2);
  Check(ModRM.RegOp = 5);
  Check(ModRM.RM = 2);
  Check(ModRM.Loaded);

  ModRM := Tx86Disassembler.LoadModRM($FF);
  Check(ModRM.FullModRM = $FF);
  Check(ModRM.Moder = 3);
  Check(ModRM.RegOp = 7);
  Check(ModRM.RM = 7);
  Check(ModRM.Loaded);
end;


procedure Tx86DisassemblerTests.TestLoadSIB;
var
  SIB: TSIB;
begin
  SIB := Tx86Disassembler.LoadSIB($AA);
  Check(SIB.FullSIB = $AA);
  Check(SIB.Scale = 2);
  Check(SIB.Index = 5);
  Check(SIB.Base = 2);

  SIB := Tx86Disassembler.LoadSIB($FF);
  Check(SIB.FullSIB = $FF);
  Check(SIB.Scale = 3);
  Check(SIB.Index = 7);
  Check(SIB.Base = 7);
end;


initialization
  RegisterTest(Tx86DisassemblerTests);

end.
