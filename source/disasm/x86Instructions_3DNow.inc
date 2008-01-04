{
  x86 3DNow Instructions - 3DNow!, Enhanced 3DNow!
}

const

  _3DNowInstruction_set: array [1..Pocet3DNowInstrukcii] of TInstruction = (
    simd_PI2FW: TSIMDInstruction = (name: 'PI2FW'; opcode: $0C; OperandsCount: 2; Operands: (p1:GREGq ;p2:MODq));
    simd_PI2FD: TSIMDInstruction = (name: 'PI2FD'; opcode: $0D; OperandsCount: 2; Operands: (p1:GREGq ;p2:MODq));
    simd_PF2IW: TSIMDInstruction = (name: 'PF2IW'; opcode: $1C; OperandsCount: 2; Operands: (p1:GREGq ;p2:MODq));
    simd_PF2ID: TSIMDInstruction = (name: 'PF2ID'; opcode: $1D; OperandsCount: 2; Operands: (p1:GREGq ;p2:MODq));

    simd_PFNACC: TSIMDInstruction = (name: 'PFNACC'; opcode: $8A; OperandsCount: 2; Operands: (p1:GREGq ;p2:MODq));
    simd_PFPNACC: TSIMDInstruction = (name: 'PFPNACC'; opcode: $8E; OperandsCount: 2; Operands: (p1:GREGq ;p2:MODq));

    simd_PFCMPGE: TSIMDInstruction = (name: 'PFCMPGE'; opcode: $90; OperandsCount: 2; Operands: (p1:GREGq ;p2:MODq));
    simd_PFMIN: TSIMDInstruction = (name: 'PFMIN'; opcode: $94; OperandsCount: 2; Operands: (p1:GREGq ;p2:MODq));
    simd_PFRCP: TSIMDInstruction = (name: 'PFRCP'; opcode: $96; OperandsCount: 2; Operands: (p1:GREGq ;p2:MODq));
    simd_PFRSQRT: TSIMDInstruction = (name: 'PFRSQRT'; opcode: $97; OperandsCount: 2; Operands: (p1:GREGq ;p2:MODq));
    simd_PFSUB: TSIMDInstruction = (name: 'PFSUB'; opcode: $9A; OperandsCount: 2; Operands: (p1:GREGq ;p2:MODq));
    simd_PFADD: TSIMDInstruction = (name: 'PFADD'; opcode: $9E; OperandsCount: 2; Operands: (p1:GREGq ;p2:MODq));

    simd_PFCMPGT: TSIMDInstruction = (name: 'PFCMPGT'; opcode: $A0; OperandsCount: 2; Operands: (p1:GREGq ;p2:MODq));
    simd_PFMAX: TSIMDInstruction = (name: 'PFMAX'; opcode: $A4; OperandsCount: 2; Operands: (p1:GREGq ;p2:MODq));
    simd_PFRCPIT1: TSIMDInstruction = (name: 'PFRCPIT1'; opcode: $A6; OperandsCount: 2; Operands: (p1:GREGq ;p2:MODq));
    simd_PFRSQIT1: TSIMDInstruction = (name: 'PFRSQIT1'; opcode: $A7; OperandsCount: 2; Operands: (p1:GREGq ;p2:MODq));
    simd_PFSUBR: TSIMDInstruction = (name: 'PFSUBR'; opcode: $AA; OperandsCount: 2; Operands: (p1:GREGq ;p2:MODq));
    simd_PFACC: TSIMDInstruction = (name: 'PFACC'; opcode: $AE; OperandsCount: 2; Operands: (p1:GREGq ;p2:MODq));

    simd_PFCMPEQ: TSIMDInstruction = (name: 'PFCMPEQ'; opcode: $B0; OperandsCount: 2; Operands: (p1:GREGq ;p2:MODq));
    simd_PFMUL: TSIMDInstruction = (name: 'PFMUL'; opcode: $B4; OperandsCount: 2; Operands: (p1:GREGq ;p2:MODq));
    simd_PFRCPIT2: TSIMDInstruction = (name: 'PFRCPIT2'; opcode: $B6; OperandsCount: 2; Operands: (p1:GREGq ;p2:MODq));
    simd_PMULHRW: TSIMDInstruction = (name: 'PMULHRW'; opcode: $B7; OperandsCount: 2; Operands: (p1:GREGq ;p2:MODq));
    simd_PSWAPD: TSIMDInstruction = (name: 'PSWAPD'; opcode: $BB; OperandsCount: 2; Operands: (p1:GREGq ;p2:MODq));
    simd_PAVGUSB: TSIMDInstruction = (name: 'PAVGUSB'; opcode: $BF; OperandsCount: 2; Operands: (p1:GREGq ;p2:MODq));
  );