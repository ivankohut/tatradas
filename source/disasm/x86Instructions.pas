{ TODO:
  - trojbatove opcode instrukcie
  - skontrolovat 1 batjove opcode instrukcie a 2 batjove non-SIMD instrukcie
}

unit x86Instructions;

interface

uses
  DisassemblerTypes,
  x86DisassemblerTypes;

type

  // "Instruction set" for instructions will maybe be used in future versions
  TInstructionSet = (isGenPur, isMMX, isSSE, isSSE2, isSSE3, isSSSE3, isSSE41, isSSE42, is3DNow, is3DNowExt, isFPU, isFPUStateMng, isTestReg);  // isVMX, is64bit

  TInstructionType = (itNone, itNormal, itSIMD, itFPU, itGroup, itTwoByteOpcodeExtension, it3DNowExtension, itUndefined);

  PSIMDInstruction = ^TSIMDInstruction;

  TInstruction = record        // Hlavny typ instrukcie
    name: string[20];           // Meno instrukcie(mnemonic)
    typ: TInstructionType;
      AddOp: boolean;           // Name16 and  name32 depends on Address(true) or Operand(false) size attribut
        name16: string[10];
        name32: string[10];
    OperandsCount: byte;                   // Pocet operandov(parametrov)
    Operands: TParam;           // Operandy(parametre) instrukcie
    SIMD, SIMD_66, SIMD_F2, SIMD_F3: PSIMDInstruction; // Pointre na SIMD instrukcie
    InstrSet: TInstructionSet;
  end;

  TSIMDInstruction = record
    Name: string[20];
    Opcode: word;
    OperandsCount: byte;
    Operands: TParam;
    InstrSet: TInstructionSet;
  end;

  TFPUAInstruction = record
    name: string[20];
    par: TSize;
  end;

  TFPUBInstruction = record
    name: string[20];
    par: string;
  end;

  TFPUInstructionGroup = array [$C0..$FF] of TFPUBInstruction;



  TPrefixes = record
    pF0, pF2, pF3: boolean;
    p2E, p36, p3E, p26, p64, p65: boolean;
    p66, p67: boolean;
  end;

  TGroupInstructions = array [0..7] of TInstruction;


{
 Instrukcie skokove:
   JMP - opcody E9 (rel8), EB( rel16/32)
   Jxx - opcody 70 - 7F (rel8), 0F 80 - 0F 8F (rel16/32)
   JCXZ - E3 (rel8)
   LOOPxx - opcody E0 (rel8), E1 (rel8), E2 (rel8)
   CALL - opcode E8 (rel16/32)

 Instrukcie ukoncujuce:
   JMP - vsetky opcody
   RET - vsetky opcody
   IRET - vsetky opcody

}

// Opcode treba vyhodit a nasledovne informacie ulozit ako TInstructionType
// Opcode $FE - Group Instruction
// Opcode $D8 - FPU Instruction
// Opcode $66 - Instruction Prefix
// Opcode $D6 - Undefined Opcode
// Opcode $60 - MMX, SSE, SSE2 Instruction


{$I x86Instructions_SIMD.inc}
{$I x86Instructions_Group.inc}
{$I x86Instructions_3DNow.inc}
{$I x86Instructions_TwoByteOpcode.inc}
{$I x86Instructions_ThreeByteOpcode_38.inc}
{$I x86Instructions_ThreeByteOpcode_3A.inc}
{$I x86Instructions_x87.inc}
{$I x86Instructions_OneByteOpcode.inc}


implementation

end.
