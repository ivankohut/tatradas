type

  PGroupInstruction_set = ^TGroupInstructions;

  TGroupMapItem = record
    Opcode: byte;
    GroupMem: PGroupInstruction_set;
    GroupReg: PGroupInstruction_set;
  end;



//******************************************************************************
// Obe byte opcode group instructions
//******************************************************************************


const
  cOneByteOpcodeGroupMapSize = 16;

  // Group 1

  Group1_80: TGroupInstructions = (
    (name: 'ADD'; typ: itOrdinary; pc:2; param: (p1:MODb; p2: IMMb)),
    (name: 'OR';  typ: itOrdinary; pc:2; param: (p1:MODb; p2: IMMb)),
    (name: 'ADC'; typ: itOrdinary; pc:2; param: (p1:MODb; p2: IMMb)),
    (name: 'SBB'; typ: itOrdinary; pc:2; param: (p1:MODb; p2: IMMb)),
    (name: 'AND'; typ: itOrdinary; pc:2; param: (p1:MODb; p2: IMMb)),
    (name: 'SUB'; typ: itOrdinary; pc:2; param: (p1:MODb; p2: IMMb)),
    (name: 'XOR'; typ: itOrdinary; pc:2; param: (p1:MODb; p2: IMMb)),
    (name: 'CMP'; typ: itOrdinary; pc:2; param: (p1:MODb; p2: IMMb))
  );

  Group1_81: TGroupInstructions = (
    (name: 'ADD'; typ: itOrdinary; pc:2; param: (p1:MODv; p2: IMMv)),
    (name: 'OR';  typ: itOrdinary; pc:2; param: (p1:MODv; p2: IMMv)),
    (name: 'ADC'; typ: itOrdinary; pc:2; param: (p1:MODv; p2: IMMv)),
    (name: 'SBB'; typ: itOrdinary; pc:2; param: (p1:MODv; p2: IMMv)),
    (name: 'AND'; typ: itOrdinary; pc:2; param: (p1:MODv; p2: IMMv)),
    (name: 'SUB'; typ: itOrdinary; pc:2; param: (p1:MODv; p2: IMMv)),
    (name: 'XOR'; typ: itOrdinary; pc:2; param: (p1:MODv; p2: IMMv)),
    (name: 'CMP'; typ: itOrdinary; pc:2; param: (p1:MODv; p2: IMMv))
  );

  Group1_82: TGroupInstructions = (
    (name: 'ADD'; typ: itOrdinary; pc:2; param: (p1:MODb; p2: IMMb)),
    (name: 'OR';  typ: itOrdinary; pc:2; param: (p1:MODb; p2: IMMb)),
    (name: 'ADC'; typ: itOrdinary; pc:2; param: (p1:MODb; p2: IMMb)),
    (name: 'SBB'; typ: itOrdinary; pc:2; param: (p1:MODb; p2: IMMb)),
    (name: 'AND'; typ: itOrdinary; pc:2; param: (p1:MODb; p2: IMMb)),
    (name: 'SUB'; typ: itOrdinary; pc:2; param: (p1:MODb; p2: IMMb)),
    (name: 'XOR'; typ: itOrdinary; pc:2; param: (p1:MODb; p2: IMMb)),
    (name: 'CMP'; typ: itOrdinary; pc:2; param: (p1:MODb; p2: IMMb))
  );

  Group1_83: TGroupInstructions = (
    (name: 'ADD'; typ: itOrdinary; pc:2; param: (p1:MODv; p2: IMMb)),
    (name: 'OR';  typ: itOrdinary; pc:2; param: (p1:MODv; p2: IMMb)),
    (name: 'ADC'; typ: itOrdinary; pc:2; param: (p1:MODv; p2: IMMb)),
    (name: 'SBB'; typ: itOrdinary; pc:2; param: (p1:MODv; p2: IMMb)),
    (name: 'AND'; typ: itOrdinary; pc:2; param: (p1:MODv; p2: IMMb)),
    (name: 'SUB'; typ: itOrdinary; pc:2; param: (p1:MODv; p2: IMMb)),
    (name: 'XOR'; typ: itOrdinary; pc:2; param: (p1:MODv; p2: IMMb)),
    (name: 'CMP'; typ: itOrdinary; pc:2; param: (p1:MODv; p2: IMMb))
  );

  // Group 1A

  Group1A_8F: TGroupInstructions = (
    (name: 'POP'; typ: itOrdinary; pc:2; param: (p1:MODb; p2:IMMb)),
    (name: 'undefined opcode!'; typ: itUndefined;),
    (name: 'undefined opcode!'; typ: itUndefined;),
    (name: 'undefined opcode!'; typ: itUndefined;),
    (name: 'undefined opcode!'; typ: itUndefined;),
    (name: 'undefined opcode!'; typ: itUndefined;),
    (name: 'undefined opcode!'; typ: itUndefined;),
    (name: 'undefined opcode!'; typ: itUndefined;)
  );


  // Group 2

  Group2_C0: TGroupInstructions = (
    (name: 'ROL'; typ: itOrdinary; pc:2; param: (p1:MODb; p2: IMMb)),
    (name: 'ROR'; typ: itOrdinary; pc:2; param: (p1:MODb; p2: IMMb)),
    (name: 'RCL'; typ: itOrdinary; pc:2; param: (p1:MODb; p2: IMMb)),
    (name: 'RCR'; typ: itOrdinary; pc:2; param: (p1:MODb; p2: IMMb)),
    // podla intelu SAL = SHL, podla Compa SAL = C0 ??110???
    (name: 'SHL'; typ: itOrdinary; pc:2; param: (p1:MODb; p2: IMMb)),
    (name: 'SHR'; typ: itOrdinary; pc:2; param: (p1:MODb; p2: IMMb)),
    (name: 'SAL'; typ: itOrdinary; pc:2; param: (p1:MODb; p2: IMMb)),
    (name: 'SAR'; typ: itOrdinary; pc:2; param: (p1:MODb; p2: IMMb))
  );

  Group2_C1: TGroupInstructions = (
    (name: 'ROL'; typ: itOrdinary; pc:2; param: (p1:MODv; p2: IMMb)),
    (name: 'ROR'; typ: itOrdinary; pc:2; param: (p1:MODv; p2: IMMb)),
    (name: 'RCL'; typ: itOrdinary; pc:2; param: (p1:MODv; p2: IMMb)),
    (name: 'RCR'; typ: itOrdinary; pc:2; param: (p1:MODv; p2: IMMb)),
    (name: 'SHL'{/SAL}; typ: itOrdinary; pc:2; param: (p1:MODv; p2: IMMb)),
    (name: 'SHR'; typ: itOrdinary; pc:2; param: (p1:MODv; p2: IMMb)),
    (name: 'undefined opcode!'; typ: itUndefined;),
    (name: 'SAR'; typ: itOrdinary; pc:2; param: (p1:MODv; p2: IMMb))
  );

  Group2_D0: TGroupInstructions = (
    (name: 'ROL'; typ: itOrdinary; pc:2; param: (p1:MODb; p2: n1)),
    (name: 'ROR'; typ: itOrdinary; pc:2; param: (p1:MODb; p2: n1)),
    (name: 'RCL'; typ: itOrdinary; pc:2; param: (p1:MODb; p2: n1)),
    (name: 'RCR'; typ: itOrdinary; pc:2; param: (p1:MODb; p2: n1)),
    (name: 'SHL'{/SAL}; typ: itOrdinary; pc:2; param: (p1:MODb; p2: n1)),
    (name: 'SHR'; typ: itOrdinary; pc:2; param: (p1:MODb; p2: n1)),
    (name: 'undefined opcode!'; typ: itUndefined;),
    (name: 'SAR'; typ: itOrdinary; pc:2; param: (p1:MODb; p2: n1))
  );

  Group2_D1: TGroupInstructions = (
    (name: 'ROL'; typ: itOrdinary; pc:2; param: (p1:MODv; p2: n1)),
    (name: 'ROR'; typ: itOrdinary; pc:2; param: (p1:MODv; p2: n1)),
    (name: 'RCL'; typ: itOrdinary; pc:2; param: (p1:MODv; p2: n1)),
    (name: 'RCR'; typ: itOrdinary; pc:2; param: (p1:MODv; p2: n1)),
    (name: 'SHL'{/SAL}; typ: itOrdinary; pc:2; param: (p1:MODv; p2: n1)),
    (name: 'SHR'; typ: itOrdinary; pc:2; param: (p1:MODv; p2: n1)),
    (name: 'undefined opcode!'; typ: itUndefined;),
    (name: 'SAR'; typ: itOrdinary; pc:2; param: (p1:MODv; p2: n1))
  );

  Group2_D2: TGroupInstructions = (
    (name: 'ROL'; typ: itOrdinary; pc:2; param: (p1:MODb; p2: CL)),
    (name: 'ROR'; typ: itOrdinary; pc:2; param: (p1:MODb; p2: CL)),
    (name: 'RCL'; typ: itOrdinary; pc:2; param: (p1:MODb; p2: CL)),
    (name: 'RCR'; typ: itOrdinary; pc:2; param: (p1:MODb; p2: CL)),
    (name: 'SHL'{/SAL}; typ: itOrdinary; pc:2; param: (p1:MODb; p2: CL)),
    (name: 'SHR'; typ: itOrdinary; pc:2; param: (p1:MODb; p2: CL)),
    (name: 'undefined opcode!'; typ: itUndefined;),
    (name: 'SAR'; typ: itOrdinary; pc:2; param: (p1:MODb; p2: CL))
  );

  Group2_D3: TGroupInstructions = (
    (name: 'ROL'; typ: itOrdinary; pc:2; param: (p1:MODv; p2: CL)),
    (name: 'ROR'; typ: itOrdinary; pc:2; param: (p1:MODv; p2: CL)),
    (name: 'RCL'; typ: itOrdinary; pc:2; param: (p1:MODv; p2: CL)),
    (name: 'RCR'; typ: itOrdinary; pc:2; param: (p1:MODv; p2: CL)),
    (name: 'SHL'{/SAL}; typ: itOrdinary; pc:2; param: (p1:MODv; p2: CL)),
    (name: 'SHR'; typ: itOrdinary; pc:2; param: (p1:MODv; p2: CL)),
    (name: 'undefined opcode!'; typ: itUndefined; ),
    (name: 'SAR'; typ: itOrdinary; pc:2; param: (p1:MODv; p2: CL))
  );

  // Group 3

  Group3_F6: TGroupInstructions = (
    (name: 'TEST'; typ: itOrdinary; pc:2; param: (p1:MODb; p2: IMMb)),
    (name: 'undefined opcode!'; typ: itUndefined;),
    (name: 'NOT'; typ: itOrdinary; pc:1; param: (p1:MODb)),
    (name: 'NEG'; typ: itOrdinary; pc:1; param: (p1:MODb)),
    (name: 'MUL'; typ: itOrdinary; pc:1; param: (p1:MODb)),
    (name: 'IMUL'; typ: itOrdinary; pc:1; param: (p1:MODb)),
    (name: 'DIV'; typ: itOrdinary; pc:1; param: (p1:MODb)),
    (name: 'IDIV'; typ: itOrdinary; pc:1; param: (p1:MODb))
  );

  Group3_F7: TGroupInstructions = (
    (name: 'TEST'; typ: itOrdinary; pc:2; param: (p1:MODv; p2: IMMv)),
    (name: 'undefined opcode!'; typ: itUndefined;),
    (name: 'NOT'; typ: itOrdinary; pc:1; param: (p1:MODv)),
    (name: 'NEG'; typ: itOrdinary; pc:1; param: (p1:MODv)),
    (name: 'MUL'; typ: itOrdinary; pc:1; param: (p1:MODv)),
    (name: 'IMUL'; typ: itOrdinary; pc:1; param: (p1:MODv)),
    (name: 'DIV'; typ: itOrdinary; pc:1; param: (p1:MODv)),
    (name: 'IDIV'; typ: itOrdinary; pc:1; param: (p1:MODv))
  );

  // Group 4

  Group4_FE: TGroupInstructions = (
    (name: 'INC'; typ: itOrdinary; pc:1; param: (p1:MODb)),
    (name: 'DEC'; typ: itOrdinary; pc:1; param: (p1:MODb)),
    (name: 'undefined opcode!'; typ: itUndefined;),
    (name: 'undefined opcode!'; typ: itUndefined;),
    (name: 'undefined opcode!'; typ: itUndefined;),
    (name: 'undefined opcode!'; typ: itUndefined;),
    (name: 'undefined opcode!'; typ: itUndefined;),
    (name: 'undefined opcode!'; typ: itUndefined;)
  );

  // Group 5

  Group5_FF: TGroupInstructions = (
    (name: 'INC'; typ: itOrdinary; pc:1; param: (p1:MODv)),
    (name: 'DEC'; typ: itOrdinary; pc:1; param: (p1:MODv)),
{calln} (name: 'CALL'; typ: itOrdinary; pc:1; param: (p1:MODv)),
{callf} (name: 'CALL'; typ: itOrdinary; pc:1; param: (p1:MODp)),
    (name: 'JMP'{N}; typ: itOrdinary; pc:1; param: (p1:MODv)),
    (name: 'JMP'{F}; typ: itOrdinary; pc:1; param: (p1:MODp)),
    (name: 'PUSH'; typ: itOrdinary; pc:1; param: (p1:MODv)),
    (name: 'undefined opcode!'; typ: itUndefined;)
  );

  // Group 11

  Group11_C6: TGroupInstructions = (
    (name: 'MOV'; typ: itOrdinary; pc:2; param: (p1:MODb; p2:IMMb)),
    (name: 'undefined opcode!'; typ: itUndefined;),
    (name: 'undefined opcode!'; typ: itUndefined;),
    (name: 'undefined opcode!'; typ: itUndefined;),
    (name: 'undefined opcode!'; typ: itUndefined;),
    (name: 'undefined opcode!'; typ: itUndefined;),
    (name: 'undefined opcode!'; typ: itUndefined;),
    (name: 'undefined opcode!'; typ: itUndefined;)
  );

  Group11_C7: TGroupInstructions = (
    (name: 'MOV'; typ: itOrdinary; pc:2; param: (p1:MODv; p2:IMMv)),
    (name: 'undefined opcode!'; typ: itUndefined;),
    (name: 'undefined opcode!'; typ: itUndefined;),
    (name: 'undefined opcode!'; typ: itUndefined;),
    (name: 'undefined opcode!'; typ: itUndefined;),
    (name: 'undefined opcode!'; typ: itUndefined;),
    (name: 'undefined opcode!'; typ: itUndefined;),
    (name: 'undefined opcode!'; typ: itUndefined;)
  );


//    (name: ''; opcode: $; secopcode: ; pc:2; param: (p1: MODb)),




  {
    Sorted by Opcode field
  }
  OneByteOpcodeGroupMap: array [0..(cOneByteOpcodeGroupMapSize - 1)] of TGroupMapItem = (
  // Group 1
    (Opcode: $80; GroupMem: @Group1_80; GroupReg: @Group1_80),
    (Opcode: $81; GroupMem: @Group1_81; GroupReg: @Group1_81),
    (Opcode: $82; GroupMem: @Group1_82; GroupReg: @Group1_82),
    (Opcode: $83; GroupMem: @Group1_83; GroupReg: @Group1_83),

  // Group 2 - part 1
    (Opcode: $C0; GroupMem: @Group2_C0; GroupReg: @Group2_C0),
    (Opcode: $C1; GroupMem: @Group2_C1; GroupReg: @Group2_C1),

  // Group 11
    (Opcode: $C6; GroupMem: @Group11_C6; GroupReg: @Group11_C6),
    (Opcode: $C7; GroupMem: @Group11_C7; GroupReg: @Group11_C7),

  // Group 2 - part 2
    (Opcode: $D0; GroupMem: @Group2_D0; GroupReg: @Group2_D0),
    (Opcode: $D1; GroupMem: @Group2_D1; GroupReg: @Group2_D1),
    (Opcode: $D2; GroupMem: @Group2_D2; GroupReg: @Group2_D2),
    (Opcode: $D3; GroupMem: @Group2_D3; GroupReg: @Group2_D3),

  // Group 3
    (Opcode: $F6; GroupMem: @Group3_F6; GroupReg: @Group3_F6),
    (Opcode: $F7; GroupMem: @Group3_F7; GroupReg: @Group3_F7),

  // Group 4
    (Opcode: $FE; GroupMem: @Group4_FE; GroupReg: @Group4_FE),

  // Group 5
    (Opcode: $FF; GroupMem: @Group5_FF; GroupReg: @Group5_FF)
  );

//******************************************************************************
// Obe byte opcode group instructions
//******************************************************************************


const
  cTwoByteOpcodeGroupMapSize = 10;

  // Group 6

  Group6_00: TGroupInstructions = (
    (name: 'SLDT'; typ: itOrdinary; pc:1; param: (p1: MODv)),
    (name: 'STR'; typ: itOrdinary; pc:1; param: (p1: MODv)),
    (name: 'LLDT'; typ: itOrdinary; pc:1; param: (p1: MODw)),
    (name: 'LTR'; typ: itOrdinary; pc:1; param: (p1: MODw)),
    (name: 'VERR'; typ: itOrdinary; pc:1; param: (p1: MODw)),
    (name: 'VERW'; typ: itOrdinary; pc:1; param: (p1: MODw)),
    (name: 'undefined opcode!'; typ: itUndefined;),
    (name: 'undefined opcode!'; typ: itUndefined;)
  );

  // Group 7

  Group7_01_Mem: TGroupInstructions = (
    (name: 'SGDT'; typ: itOrdinary; pc:1; param: (p1: MODv)),
    (name: 'SIDT'; typ: itOrdinary; pc:1; param: (p1: MODv)),
    (name: 'LGDT'; typ: itOrdinary; pc:1; param: (p1: MODv)),
    (name: 'LIDT'; typ: itOrdinary; pc:1; param: (p1: MODv)),
    (name: 'SMSW'; typ: itOrdinary; pc:1; param: (p1: MODw)),
    (name: 'undefined opcode!'; typ: itUndefined;),
    (name: 'LMSW'; typ: itOrdinary; pc:1; param: (p1: MODw)),
    (name: 'INVLPG'; typ: itOrdinary; pc:1; param: (p1: MODb))
  );

  Group7_01_Reg: TGroupInstructions = (
    (name: 'undefined opcode!'; typ: itUndefined;),
    (name: 'undefined opcode!'; typ: itUndefined;),
    (name: 'undefined opcode!'; typ: itUndefined;),
    (name: 'undefined opcode!'; typ: itUndefined;),
    (name: 'SMSW'; typ: itOrdinary; pc:1; param: (p1: MODw)),
    (name: 'undefined opcode!'; typ: itUndefined;),
    (name: 'LMSW'; typ: itOrdinary; pc:1; param: (p1: MODw)),
    (name: 'undefined opcode!'; typ: itUndefined;)
  );

  // Group 8

  Group8_BA: TGroupInstructions = (
    (name: 'undefined opcode!'; typ: itUndefined;),
    (name: 'undefined opcode!'; typ: itUndefined;),
    (name: 'undefined opcode!'; typ: itUndefined;),
    (name: 'undefined opcode!'; typ: itUndefined;),
    (name: 'BT'; typ: itOrdinary; pc:2; param: (p1: MODv; p2: IMMb)),
    (name: 'BTS'; typ: itOrdinary; pc:2; param: (p1: MODv; p2: IMMb)),
    (name: 'BTR'; typ: itOrdinary; pc:2; param: (p1: MODv; p2: IMMb)),
    (name: 'BTC'; typ: itOrdinary; pc:2; param: (p1: MODv; p2: IMMb))
  );

  // Group 9

  Group9_C7_Mem: TGroupInstructions = (
    (name: 'undefined opcode!'; typ: itUndefined; ),
    (name: 'CMPXCHG8B'; typ: itOrdinary; pc:1; param: (p1: MODv)),
    (name: 'undefined opcode!'; typ: itUndefined;),
    (name: 'undefined opcode!'; typ: itUndefined;),
    (name: 'undefined opcode!'; typ: itUndefined;),
    (name: 'undefined opcode!'; typ: itUndefined;),
    (name: 'undefined opcode!'; typ: itUndefined;),
    (name: 'undefined opcode!'; typ: itUndefined;)
  );

  Group9_C7_Reg: TGroupInstructions = (
    (name: 'undefined opcode!'; typ: itUndefined;),
    (name: 'undefined opcode!'; typ: itUndefined;),
    (name: 'undefined opcode!'; typ: itUndefined;),
    (name: 'undefined opcode!'; typ: itUndefined;),
    (name: 'undefined opcode!'; typ: itUndefined;),
    (name: 'undefined opcode!'; typ: itUndefined;),
    (name: 'undefined opcode!'; typ: itUndefined;),
    (name: 'undefined opcode!'; typ: itUndefined;)
  );

  // Group 10

  Group10_B9: TGroupInstructions = (
    (name: 'undefined opcode!'; typ: itUndefined;),
    (name: 'undefined opcode!'; typ: itUndefined;),
    (name: 'undefined opcode!'; typ: itUndefined;),
    (name: 'undefined opcode!'; typ: itUndefined;),
    (name: 'undefined opcode!'; typ: itUndefined;),
    (name: 'undefined opcode!'; typ: itUndefined;),
    (name: 'undefined opcode!'; typ: itUndefined;),
    (name: 'undefined opcode!'; typ: itUndefined;)
  );

  // Group 12

  Group12_71_Mem: TGroupInstructions = (
    (name: 'undefined opcode!'; typ: itUndefined;),
    (name: 'undefined opcode!'; typ: itUndefined;),
    (name: 'undefined opcode!'; typ: itUndefined;),
    (name: 'undefined opcode!'; typ: itUndefined;),
    (name: 'undefined opcode!'; typ: itUndefined;),
    (name: 'undefined opcode!'; typ: itUndefined;),
    (name: 'undefined opcode!'; typ: itUndefined;),
    (name: 'undefined opcode!'; typ: itUndefined;)
  );

  Group12_71_Reg: TGroupInstructions = (
    (name: 'undefined opcode!'; typ: itUndefined;),
    (name: 'undefined opcode!'; typ: itUndefined;),
    (name: 'mmx'; typ: itSIMD; SIMD: @simd_PSRLW_c;     SIMD_66: @simd_PSRLW_d;    SIMD_F2: nil;             SIMD_F3: nil),
    (name: 'undefined opcode!'; typ: itUndefined;),
    (name: 'mmx'; typ: itSIMD; SIMD: @simd_PSRAW_c;     SIMD_66: @simd_PSRAW_d;    SIMD_F2: nil;             SIMD_F3: nil),
    (name: 'undefined opcode!'; typ: itUndefined;),
    (name: 'mmx'; typ: itSIMD; SIMD: @simd_PSLLW_c;     SIMD_66: @simd_PSLLW_d;    SIMD_F2: nil;             SIMD_F3: nil),
    (name: 'undefined opcode!'; typ: itUndefined;)
  );

  // Group 13

  Group13_72_Mem: TGroupInstructions = (
    (name: 'undefined opcode!'; typ: itUndefined;),
    (name: 'undefined opcode!'; typ: itUndefined;),
    (name: 'undefined opcode!'; typ: itUndefined;),
    (name: 'undefined opcode!'; typ: itUndefined;),
    (name: 'undefined opcode!'; typ: itUndefined;),
    (name: 'undefined opcode!'; typ: itUndefined;),
    (name: 'undefined opcode!'; typ: itUndefined;),
    (name: 'undefined opcode!'; typ: itUndefined;)
  );

  Group13_72_Reg: TGroupInstructions = (
    (name: 'undefined opcode!'; typ: itUndefined;),
    (name: 'undefined opcode!'; typ: itUndefined;),
    (name: 'mmx'; typ: itSIMD; SIMD: @simd_PSRLD_c;     SIMD_66: @simd_PSRLD_d;    SIMD_F2: nil;             SIMD_F3: nil),
    (name: 'undefined opcode!'; typ: itUndefined;),
    (name: 'mmx'; typ: itSIMD; SIMD: @simd_PSRAD_c;     SIMD_66: @simd_PSRAD_d;    SIMD_F2: nil;             SIMD_F3: nil),
    (name: 'undefined opcode!'; typ: itUndefined;),
    (name: 'mmx'; typ: itSIMD; SIMD: @simd_PSLLD_c;     SIMD_66: @simd_PSLLD_d;    SIMD_F2: nil;             SIMD_F3: nil),
    (name: 'undefined opcode!'; typ: itUndefined;)
  );

  // Group 14

  Group14_73_Mem: TGroupInstructions = (
    (name: 'undefined opcode!'; typ: itUndefined;),
    (name: 'undefined opcode!'; typ: itUndefined;),
    (name: 'undefined opcode!'; typ: itUndefined;),
    (name: 'undefined opcode!'; typ: itUndefined;),
    (name: 'undefined opcode!'; typ: itUndefined;),
    (name: 'undefined opcode!'; typ: itUndefined;),
    (name: 'undefined opcode!'; typ: itUndefined;),
    (name: 'undefined opcode!'; typ: itUndefined;)
  );

  Group14_73_Reg: TGroupInstructions = (
    (name: 'undefined opcode!'; typ: itUndefined;),
    (name: 'undefined opcode!'; typ: itUndefined;),
    (name: 'mmx'; typ: itSIMD; SIMD: @simd_PSRLQ_c;     SIMD_66: @simd_PSRLQ_d;    SIMD_F2: nil;             SIMD_F3: nil),
    (name: 'mmx'; typ: itSIMD; SIMD: nil;     SIMD_66: @simd_PSRLDQ;    SIMD_F2: nil;             SIMD_F3: nil),
    (name: 'undefined opcode!'; typ: itUndefined;),
    (name: 'undefined opcode!'; typ: itUndefined;),
    (name: 'mmx'; typ: itSIMD; SIMD: @simd_PSLLQ_c;     SIMD_66: @simd_PSLLQ_d;    SIMD_F2: nil;             SIMD_F3: nil),
    (name: 'mmx'; typ: itSIMD; SIMD: nil;             SIMD_66: @simd_PSLLDQ;    SIMD_F2: nil;             SIMD_F3: nil)
  );

  // Group 15

  Group15_AE_Mem: TGroupInstructions = (
    (name: 'FXSAVE'; typ: itOrdinary; pc:0),
    (name: 'FXSTOR'; typ: itOrdinary; pc:0),
    (name: 'LDMXCSR'; typ: itOrdinary; pc:0),
    (name: 'STMXCSR'; typ: itOrdinary; pc:0),
    (name: 'undefined opcode!'; typ: itUndefined;),
    (name: 'undefined opcode!'; typ: itUndefined;),
    (name: 'undefined opcode!'; typ: itUndefined;),
    (name: 'CLFLUSH'; typ: itOrdinary; pc:0)
  );

  Group15_AE_Reg: TGroupInstructions = (
    (name: 'undefined opcode!'; typ: itUndefined;),
    (name: 'undefined opcode!'; typ: itUndefined;),
    (name: 'undefined opcode!'; typ: itUndefined;),
    (name: 'undefined opcode!'; typ: itUndefined;),
    (name: 'undefined opcode!'; typ: itUndefined;),
    (name: 'LFENCE'; typ: itOrdinary; pc:0),
    (name: 'MFENCE'; typ: itOrdinary; pc:0),
    (name: 'SFENCE'; typ: itOrdinary; pc:0)
  );

  // Group 16

  Group16_18_Mem: TGroupInstructions = (
    (name: 'PREFETCHNTA'; typ: itOrdinary; pc:0),
    (name: 'PREFETCHT0'; typ: itOrdinary; pc:0),
    (name: 'PREFETCHT1'; typ: itOrdinary; pc:0),
    (name: 'PREFETCHT2'; typ: itOrdinary; pc:0),
    (name: 'undefined opcode!'; typ: itUndefined;),
    (name: 'undefined opcode!'; typ: itUndefined;),
    (name: 'undefined opcode!'; typ: itUndefined;),
    (name: 'undefined opcode!'; typ: itUndefined;)
  );

  Group16_18_Reg: TGroupInstructions = (
    (name: 'undefined opcode!'; typ: itUndefined;),
    (name: 'undefined opcode!'; typ: itUndefined;),
    (name: 'undefined opcode!'; typ: itUndefined;),
    (name: 'undefined opcode!'; typ: itUndefined;),
    (name: 'undefined opcode!'; typ: itUndefined;),
    (name: 'undefined opcode!'; typ: itUndefined;),
    (name: 'undefined opcode!'; typ: itUndefined;),
    (name: 'undefined opcode!'; typ: itUndefined;)
  );


  TwoByteOpcodeGroupMap: array [0..(cTwoByteOpcodeGroupMapSize - 1)] of TGroupMapItem = (
    (Opcode: $00; GroupMem: @Group6_00; GroupReg: @Group6_00),
    (Opcode: $01; GroupMem: @Group7_01_Mem; GroupReg: @Group7_01_Reg),
    (Opcode: $BA; GroupMem: @Group8_BA; GroupReg: @Group8_BA),
    (Opcode: $C7; GroupMem: @Group9_C7_Mem; GroupReg: @Group9_C7_Reg),
    (Opcode: $B9; GroupMem: @Group10_B9; GroupReg: @Group10_B9),
    (Opcode: $71; GroupMem: @Group12_71_Mem; GroupReg: @Group12_71_Reg),
    (Opcode: $72; GroupMem: @Group13_72_Mem; GroupReg: @Group13_72_Reg),
    (Opcode: $73; GroupMem: @Group14_73_Mem; GroupReg: @Group14_73_Reg),
    (Opcode: $AE; GroupMem: @Group15_AE_Mem; GroupReg: @Group15_AE_Reg),
    (Opcode: $18; GroupMem: @Group16_18_Mem; GroupReg: @Group16_18_Reg)
  );


{
var
  i: Integer;
begin
  i := 0;
  while i < cOneByteOpcodeGroupMapSize do begin
    if (OneByteOpcodeGroupMap[i].Opcode = AOpcode then begin
      if aModMem then
        result := OneByteOpcodeGroupMap[i].GroupMem
      else
        result := OneByteOpcodeGroupMap[i].GroupReg;
      Exit;
    end
    else
      Inc(i);
  end;
end;
}

//    (name: ''; opcode: $; pc:2; param: (p1: MODv; p2: IMMb)),
//    (name: 'mmx'; typ: itSIMD; SIMD: @simd_;     SIMD_66: @simd_;    SIMD_F2: nil;             SIMD_F3: nil),

