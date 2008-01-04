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
    (name: 'ADD'; opcode: $80; pc:2; param: (p1:MODb; p2: IMMb)),
    (name: 'OR';  opcode: $80; pc:2; param: (p1:MODb; p2: IMMb)),
    (name: 'ADC'; opcode: $80; pc:2; param: (p1:MODb; p2: IMMb)),
    (name: 'SBB'; opcode: $80; pc:2; param: (p1:MODb; p2: IMMb)),
    (name: 'AND'; opcode: $80; pc:2; param: (p1:MODb; p2: IMMb)),
    (name: 'SUB'; opcode: $80; pc:2; param: (p1:MODb; p2: IMMb)),
    (name: 'XOR'; opcode: $80; pc:2; param: (p1:MODb; p2: IMMb)),
    (name: 'CMP'; opcode: $80; pc:2; param: (p1:MODb; p2: IMMb))
  );

  Group1_81: TGroupInstructions = (
    (name: 'ADD'; opcode: $81; pc:2; param: (p1:MODv; p2: IMMv)),
    (name: 'OR';  opcode: $81; pc:2; param: (p1:MODv; p2: IMMv)),
    (name: 'ADC'; opcode: $81; pc:2; param: (p1:MODv; p2: IMMv)),
    (name: 'SBB'; opcode: $81; pc:2; param: (p1:MODv; p2: IMMv)),
    (name: 'AND'; opcode: $81; pc:2; param: (p1:MODv; p2: IMMv)),
    (name: 'SUB'; opcode: $81; pc:2; param: (p1:MODv; p2: IMMv)),
    (name: 'XOR'; opcode: $81; pc:2; param: (p1:MODv; p2: IMMv)),
    (name: 'CMP'; opcode: $81; pc:2; param: (p1:MODv; p2: IMMv))
  );

  Group1_82: TGroupInstructions = (
    (name: 'ADD'; opcode: $82; pc:2; param: (p1:MODb; p2: IMMb)),
    (name: 'OR';  opcode: $82; pc:2; param: (p1:MODb; p2: IMMb)),
    (name: 'ADC'; opcode: $82; pc:2; param: (p1:MODb; p2: IMMb)),
    (name: 'SBB'; opcode: $82; pc:2; param: (p1:MODb; p2: IMMb)),
    (name: 'AND'; opcode: $82; pc:2; param: (p1:MODb; p2: IMMb)),
    (name: 'SUB'; opcode: $82; pc:2; param: (p1:MODb; p2: IMMb)),
    (name: 'XOR'; opcode: $82; pc:2; param: (p1:MODb; p2: IMMb)),
    (name: 'CMP'; opcode: $82; pc:2; param: (p1:MODb; p2: IMMb))
  );

  Group1_83: TGroupInstructions = (
    (name: 'ADD'; opcode: $83; pc:2; param: (p1:MODv; p2: IMMb)),
    (name: 'OR';  opcode: $83; pc:2; param: (p1:MODv; p2: IMMb)),
    (name: 'ADC'; opcode: $83; pc:2; param: (p1:MODv; p2: IMMb)),
    (name: 'SBB'; opcode: $83; pc:2; param: (p1:MODv; p2: IMMb)),
    (name: 'AND'; opcode: $83; pc:2; param: (p1:MODv; p2: IMMb)),
    (name: 'SUB'; opcode: $83; pc:2; param: (p1:MODv; p2: IMMb)),
    (name: 'XOR'; opcode: $83; pc:2; param: (p1:MODv; p2: IMMb)),
    (name: 'CMP'; opcode: $83; pc:2; param: (p1:MODv; p2: IMMb))
  );

  // Group 1A

  Group1A_8F: TGroupInstructions = (
    (name: 'POP'; opcode: $8F; pc:2; param: (p1:MODb; p2:IMMb)),
    (name: 'undefined opcode!'; opcode: $D6;),
    (name: 'undefined opcode!'; opcode: $D6;),
    (name: 'undefined opcode!'; opcode: $D6;),
    (name: 'undefined opcode!'; opcode: $D6;),
    (name: 'undefined opcode!'; opcode: $D6;),
    (name: 'undefined opcode!'; opcode: $D6;),
    (name: 'undefined opcode!'; opcode: $D6;)
  );


  // Group 2

  Group2_C0: TGroupInstructions = (
    (name: 'ROL'; opcode: $C0; pc:2; param: (p1:MODb; p2: IMMb)),
    (name: 'ROR'; opcode: $C0; pc:2; param: (p1:MODb; p2: IMMb)),
    (name: 'RCL'; opcode: $C0; pc:2; param: (p1:MODb; p2: IMMb)),
    (name: 'RCR'; opcode: $C0; pc:2; param: (p1:MODb; p2: IMMb)),
    // podla intelu SAL = SHL, podla Compa SAL = C0 ??110???
    (name: 'SHL'; opcode: $C0; pc:2; param: (p1:MODb; p2: IMMb)),
    (name: 'SHR'; opcode: $C0; pc:2; param: (p1:MODb; p2: IMMb)),
    (name: 'SAL'; opcode: $C0; pc:2; param: (p1:MODb; p2: IMMb)),
    (name: 'SAR'; opcode: $C0; pc:2; param: (p1:MODb; p2: IMMb))
  );

  Group2_C1: TGroupInstructions = (
    (name: 'ROL'; opcode: $C1; pc:2; param: (p1:MODv; p2: IMMb)),
    (name: 'ROR'; opcode: $C1; pc:2; param: (p1:MODv; p2: IMMb)),
    (name: 'RCL'; opcode: $C1; pc:2; param: (p1:MODv; p2: IMMb)),
    (name: 'RCR'; opcode: $C1; pc:2; param: (p1:MODv; p2: IMMb)),
    (name: 'SHL'{/SAL}; opcode: $C1; pc:2; param: (p1:MODv; p2: IMMb)),
    (name: 'SHR'; opcode: $C1; pc:2; param: (p1:MODv; p2: IMMb)),
    (name: 'undefined opcode!'; opcode: $D6;),
    (name: 'SAR'; opcode: $C1; pc:2; param: (p1:MODv; p2: IMMb))
  );

  Group2_D0: TGroupInstructions = (
    (name: 'ROL'; opcode: $D0; pc:2; param: (p1:MODb; p2: n1)),
    (name: 'ROR'; opcode: $D0; pc:2; param: (p1:MODb; p2: n1)),
    (name: 'RCL'; opcode: $D0; pc:2; param: (p1:MODb; p2: n1)),
    (name: 'RCR'; opcode: $D0; pc:2; param: (p1:MODb; p2: n1)),
    (name: 'SHL'{/SAL}; opcode: $D0; pc:2; param: (p1:MODb; p2: n1)),
    (name: 'SHR'; opcode: $D0; pc:2; param: (p1:MODb; p2: n1)),
    (name: 'undefined opcode!'; opcode: $D6;),
    (name: 'SAR'; opcode: $D0; pc:2; param: (p1:MODb; p2: n1))
  );

  Group2_D1: TGroupInstructions = (
    (name: 'ROL'; opcode: $D1; pc:2; param: (p1:MODv; p2: n1)),
    (name: 'ROR'; opcode: $D1; pc:2; param: (p1:MODv; p2: n1)),
    (name: 'RCL'; opcode: $D1; pc:2; param: (p1:MODv; p2: n1)),
    (name: 'RCR'; opcode: $D1; pc:2; param: (p1:MODv; p2: n1)),
    (name: 'SHL'{/SAL}; opcode: $D1; pc:2; param: (p1:MODv; p2: n1)),
    (name: 'SHR'; opcode: $D1; pc:2; param: (p1:MODv; p2: n1)),
    (name: 'undefined opcode!'; opcode: $D6;),
    (name: 'SAR'; opcode: $D1; pc:2; param: (p1:MODv; p2: n1))
  );

  Group2_D2: TGroupInstructions = (
    (name: 'ROL'; opcode: $D2; pc:2; param: (p1:MODb; p2: CL)),
    (name: 'ROR'; opcode: $D2; pc:2; param: (p1:MODb; p2: CL)),
    (name: 'RCL'; opcode: $D2; pc:2; param: (p1:MODb; p2: CL)),
    (name: 'RCR'; opcode: $D2; pc:2; param: (p1:MODb; p2: CL)),
    (name: 'SHL'{/SAL}; opcode: $D2; pc:2; param: (p1:MODb; p2: CL)),
    (name: 'SHR'; opcode: $D2; pc:2; param: (p1:MODb; p2: CL)),
    (name: 'undefined opcode!'; opcode: $D6;),
    (name: 'SAR'; opcode: $D2; pc:2; param: (p1:MODb; p2: CL))
  );

  Group2_D3: TGroupInstructions = (
    (name: 'ROL'; opcode: $D3; pc:2; param: (p1:MODv; p2: CL)),
    (name: 'ROR'; opcode: $D3; pc:2; param: (p1:MODv; p2: CL)),
    (name: 'RCL'; opcode: $D3; pc:2; param: (p1:MODv; p2: CL)),
    (name: 'RCR'; opcode: $D3; pc:2; param: (p1:MODv; p2: CL)),
    (name: 'SHL'{/SAL}; opcode: $D3; pc:2; param: (p1:MODv; p2: CL)),
    (name: 'SHR'; opcode: $D3; pc:2; param: (p1:MODv; p2: CL)),
    (name: 'undefined opcode!'; opcode: $D6; ),
    (name: 'SAR'; opcode: $D3; pc:2; param: (p1:MODv; p2: CL))
  );

  // Group 3

  Group3_F6: TGroupInstructions = (
    (name: 'TEST'; opcode: $F6; pc:2; param: (p1:MODb; p2: IMMb)),
    (name: 'undefined opcode!'; opcode: $D6;),
    (name: 'NOT'; opcode: $F6; pc:1; param: (p1:MODb)),
    (name: 'NEG'; opcode: $F6; pc:1; param: (p1:MODb)),
    (name: 'MUL'; opcode: $F6; pc:1; param: (p1:MODb)),
    (name: 'IMUL'; opcode: $F6; pc:1; param: (p1:MODb)),
    (name: 'DIV'; opcode: $F6; pc:1; param: (p1:MODb)),
    (name: 'IDIV'; opcode: $F6; pc:1; param: (p1:MODb))
  );

  Group3_F7: TGroupInstructions = (
    (name: 'TEST'; opcode: $F7; pc:2; param: (p1:MODv; p2: IMMv)),
    (name: 'undefined opcode!'; opcode: $D6;),
    (name: 'NOT'; opcode: $F7; pc:1; param: (p1:MODv)),
    (name: 'NEG'; opcode: $F7; pc:1; param: (p1:MODv)),
    (name: 'MUL'; opcode: $F7; pc:1; param: (p1:MODv)),
    (name: 'IMUL'; opcode: $F7; pc:1; param: (p1:MODv)),
    (name: 'DIV'; opcode: $F7; pc:1; param: (p1:MODv)),
    (name: 'IDIV'; opcode: $F7; pc:1; param: (p1:MODv))
  );

  // Group 4

  Group4_FE: TGroupInstructions = (
    (name: 'INC'; opcode: $FE; pc:1; param: (p1:MODb)),
    (name: 'DEC'; opcode: $FE; pc:1; param: (p1:MODb)),
    (name: 'undefined opcode!'; opcode: $D6;),
    (name: 'undefined opcode!'; opcode: $D6;),
    (name: 'undefined opcode!'; opcode: $D6;),
    (name: 'undefined opcode!'; opcode: $D6;),
    (name: 'undefined opcode!'; opcode: $D6;),
    (name: 'undefined opcode!'; opcode: $D6;)
  );

  // Group 5

  Group5_FF: TGroupInstructions = (
    (name: 'INC'; opcode: $FF; pc:1; param: (p1:MODv)),
    (name: 'DEC'; opcode: $FF; pc:1; param: (p1:MODv)),
{calln} (name: 'CALL'; opcode: $FF; pc:1; param: (p1:MODv)),
{callf} (name: 'CALL'; opcode: $FF; pc:1; param: (p1:MODp)),
    (name: 'JMP'{N}; opcode: $FF; pc:1; param: (p1:MODv)),
    (name: 'JMP'{F}; opcode: $FF; pc:1; param: (p1:MODp)),
    (name: 'PUSH'; opcode: $FF; pc:1; param: (p1:MODv)),
    (name: 'undefined opcode!'; opcode: $D6;)
  );

  // Group 11

  Group11_C6: TGroupInstructions = (
    (name: 'MOV'; opcode: $C6; pc:2; param: (p1:MODb; p2:IMMb)),
    (name: 'undefined opcode!'; opcode: $D6;),
    (name: 'undefined opcode!'; opcode: $D6;),
    (name: 'undefined opcode!'; opcode: $D6;),
    (name: 'undefined opcode!'; opcode: $D6;),
    (name: 'undefined opcode!'; opcode: $D6;),
    (name: 'undefined opcode!'; opcode: $D6;),
    (name: 'undefined opcode!'; opcode: $D6;)
  );

  Group11_C7: TGroupInstructions = (
    (name: 'MOV'; opcode: $C7; pc:2; param: (p1:MODv; p2:IMMv)),
    (name: 'undefined opcode!'; opcode: $D6;),
    (name: 'undefined opcode!'; opcode: $D6;),
    (name: 'undefined opcode!'; opcode: $D6;),
    (name: 'undefined opcode!'; opcode: $D6;),
    (name: 'undefined opcode!'; opcode: $D6;),
    (name: 'undefined opcode!'; opcode: $D6;),
    (name: 'undefined opcode!'; opcode: $D6;)
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
    (name: 'SLDT'; opcode: $00; pc:1; param: (p1: MODv)),
    (name: 'STR'; opcode: $00; pc:1; param: (p1: MODv)),
    (name: 'LLDT'; opcode: $00; pc:1; param: (p1: MODw)),
    (name: 'LTR'; opcode: $00; pc:1; param: (p1: MODw)),
    (name: 'VERR'; opcode: $00; pc:1; param: (p1: MODw)),
    (name: 'VERW'; opcode: $00; pc:1; param: (p1: MODw)),
    (name: 'undefined opcode!'; opcode: $D6;),
    (name: 'undefined opcode!'; opcode: $D6;)
  );

  // Group 7
  
  Group7_01_Mem: TGroupInstructions = (
    (name: 'SGDT'; opcode: $01; pc:1; param: (p1: MODv)),
    (name: 'SIDT'; opcode: $01; pc:1; param: (p1: MODv)),
    (name: 'LGDT'; opcode: $01; pc:1; param: (p1: MODv)),
    (name: 'LIDT'; opcode: $01; pc:1; param: (p1: MODv)),
    (name: 'SMSW'; opcode: $01; pc:1; param: (p1: MODw)),
    (name: 'undefined opcode!'; opcode: $D6;),
    (name: 'LMSW'; opcode: $01; pc:1; param: (p1: MODw)),
    (name: 'INVLPG'; opcode: $01; pc:1; param: (p1: MODb))
  );

  Group7_01_Reg: TGroupInstructions = (
    (name: 'undefined opcode!'; opcode: $D6;),
    (name: 'undefined opcode!'; opcode: $D6;),
    (name: 'undefined opcode!'; opcode: $D6;),
    (name: 'undefined opcode!'; opcode: $D6;),
    (name: 'SMSW'; opcode: $01; pc:1; param: (p1: MODw)),
    (name: 'undefined opcode!'; opcode: $D6;),
    (name: 'LMSW'; opcode: $01; pc:1; param: (p1: MODw)),
    (name: 'undefined opcode!'; opcode: $D6;)
  );

  // Group 8
  
  Group8_BA: TGroupInstructions = (
    (name: 'undefined opcode!'; opcode: $D6;),
    (name: 'undefined opcode!'; opcode: $D6;),
    (name: 'undefined opcode!'; opcode: $D6;),
    (name: 'undefined opcode!'; opcode: $D6;),
    (name: 'BT'; opcode: $BA; pc:2; param: (p1: MODv; p2: IMMb)),
    (name: 'BTS'; opcode: $BA; pc:2; param: (p1: MODv; p2: IMMb)),
    (name: 'BTR'; opcode: $BA; pc:2; param: (p1: MODv; p2: IMMb)),
    (name: 'BTC'; opcode: $BA; pc:2; param: (p1: MODv; p2: IMMb))
  );

  // Group 9

  Group9_C7_Mem: TGroupInstructions = (
    (name: 'undefined opcode!'; opcode: $D6; ),
    (name: 'CMPXCHG8B'; opcode: $C7; pc:1; param: (p1: MODv)),
    (name: 'undefined opcode!'; opcode: $D6;),
    (name: 'undefined opcode!'; opcode: $D6;),
    (name: 'undefined opcode!'; opcode: $D6;),
    (name: 'undefined opcode!'; opcode: $D6;),
    (name: 'undefined opcode!'; opcode: $D6;),
    (name: 'undefined opcode!'; opcode: $D6;)
  );

  Group9_C7_Reg: TGroupInstructions = (
    (name: 'undefined opcode!'; opcode: $D6;),
    (name: 'undefined opcode!'; opcode: $D6;),
    (name: 'undefined opcode!'; opcode: $D6;),
    (name: 'undefined opcode!'; opcode: $D6;),
    (name: 'undefined opcode!'; opcode: $D6;),
    (name: 'undefined opcode!'; opcode: $D6;),
    (name: 'undefined opcode!'; opcode: $D6;),
    (name: 'undefined opcode!'; opcode: $D6;)
  );

  // Group 10

  Group10_B9: TGroupInstructions = (
    (name: 'undefined opcode!'; opcode: $D6;),
    (name: 'undefined opcode!'; opcode: $D6;),
    (name: 'undefined opcode!'; opcode: $D6;),
    (name: 'undefined opcode!'; opcode: $D6;),
    (name: 'undefined opcode!'; opcode: $D6;),
    (name: 'undefined opcode!'; opcode: $D6;),
    (name: 'undefined opcode!'; opcode: $D6;),
    (name: 'undefined opcode!'; opcode: $D6;)
  );

  // Group 12

  Group12_71_Mem: TGroupInstructions = (
    (name: 'undefined opcode!'; opcode: $D6;),
    (name: 'undefined opcode!'; opcode: $D6;),
    (name: 'undefined opcode!'; opcode: $D6;),
    (name: 'undefined opcode!'; opcode: $D6;),
    (name: 'undefined opcode!'; opcode: $D6;),
    (name: 'undefined opcode!'; opcode: $D6;),
    (name: 'undefined opcode!'; opcode: $D6;),
    (name: 'undefined opcode!'; opcode: $D6;)
  );

  Group12_71_Reg: TGroupInstructions = (
    (name: 'undefined opcode!'; opcode: $D6;),
    (name: 'undefined opcode!'; opcode: $D6;),
    (name: 'mmx'; opcode: $60; SIMD: @simd_PSRLW_c;     SIMD_66: @simd_PSRLW_d;    SIMD_F2: nil;             SIMD_F3: nil),
    (name: 'undefined opcode!'; opcode: $D6;),
    (name: 'mmx'; opcode: $60; SIMD: @simd_PSRAW_c;     SIMD_66: @simd_PSRAW_d;    SIMD_F2: nil;             SIMD_F3: nil),
    (name: 'undefined opcode!'; opcode: $D6;),
    (name: 'mmx'; opcode: $60; SIMD: @simd_PSLLW_c;     SIMD_66: @simd_PSLLW_d;    SIMD_F2: nil;             SIMD_F3: nil),
    (name: 'undefined opcode!'; opcode: $D6;)
  );

  // Group 13

  Group13_72_Mem: TGroupInstructions = (
    (name: 'undefined opcode!'; opcode: $D6;),
    (name: 'undefined opcode!'; opcode: $D6;),
    (name: 'undefined opcode!'; opcode: $D6;),
    (name: 'undefined opcode!'; opcode: $D6;),
    (name: 'undefined opcode!'; opcode: $D6;),
    (name: 'undefined opcode!'; opcode: $D6;),
    (name: 'undefined opcode!'; opcode: $D6;),
    (name: 'undefined opcode!'; opcode: $D6;)
  );

  Group13_72_Reg: TGroupInstructions = (
    (name: 'undefined opcode!'; opcode: $D6;),
    (name: 'undefined opcode!'; opcode: $D6;),
    (name: 'mmx'; opcode: $60; SIMD: @simd_PSRLD_c;     SIMD_66: @simd_PSRLD_d;    SIMD_F2: nil;             SIMD_F3: nil),
    (name: 'undefined opcode!'; opcode: $D6;),
    (name: 'mmx'; opcode: $60; SIMD: @simd_PSRAD_c;     SIMD_66: @simd_PSRAD_d;    SIMD_F2: nil;             SIMD_F3: nil),
    (name: 'undefined opcode!'; opcode: $D6;),
    (name: 'mmx'; opcode: $60; SIMD: @simd_PSLLD_c;     SIMD_66: @simd_PSLLD_d;    SIMD_F2: nil;             SIMD_F3: nil),
    (name: 'undefined opcode!'; opcode: $D6;)
  );

  // Group 14

  Group14_73_Mem: TGroupInstructions = (
    (name: 'undefined opcode!'; opcode: $D6;),
    (name: 'undefined opcode!'; opcode: $D6;),
    (name: 'undefined opcode!'; opcode: $D6;),
    (name: 'undefined opcode!'; opcode: $D6;),
    (name: 'undefined opcode!'; opcode: $D6;),
    (name: 'undefined opcode!'; opcode: $D6;),
    (name: 'undefined opcode!'; opcode: $D6;),
    (name: 'undefined opcode!'; opcode: $D6;)
  );
    
  Group14_73_Reg: TGroupInstructions = (
    (name: 'undefined opcode!'; opcode: $D6;),
    (name: 'undefined opcode!'; opcode: $D6;),
    (name: 'mmx'; opcode: $60; SIMD: @simd_PSRLQ_c;     SIMD_66: @simd_PSRLQ_d;    SIMD_F2: nil;             SIMD_F3: nil),
    (name: 'mmx'; opcode: $60; SIMD: nil;     SIMD_66: @simd_PSRLDQ;    SIMD_F2: nil;             SIMD_F3: nil),
    (name: 'undefined opcode!'; opcode: $D6;),
    (name: 'undefined opcode!'; opcode: $D6;),
    (name: 'mmx'; opcode: $60; SIMD: @simd_PSLLQ_c;     SIMD_66: @simd_PSLLQ_d;    SIMD_F2: nil;             SIMD_F3: nil),
    (name: 'mmx'; opcode: $60; SIMD: nil;             SIMD_66: @simd_PSLLDQ;    SIMD_F2: nil;             SIMD_F3: nil)
  );

  // Group 15
  
  Group15_AE_Mem: TGroupInstructions = (
    (name: 'FXSAVE'; opcode: $AE; pc:0),
    (name: 'FXSTOR'; opcode: $AE; pc:0),
    (name: 'LDMXCSR'; opcode: $AE; pc:0),
    (name: 'STMXCSR'; opcode: $AE; pc:0),
    (name: 'undefined opcode!'; opcode: $D6;),
    (name: 'undefined opcode!'; opcode: $D6;),
    (name: 'undefined opcode!'; opcode: $D6;),
    (name: 'CLFLUSH'; opcode: $AE; pc:0)
  );

  Group15_AE_Reg: TGroupInstructions = (
    (name: 'undefined opcode!'; opcode: $D6;),
    (name: 'undefined opcode!'; opcode: $D6;),
    (name: 'undefined opcode!'; opcode: $D6;),
    (name: 'undefined opcode!'; opcode: $D6;),
    (name: 'undefined opcode!'; opcode: $D6;),
    (name: 'LFENCE'; opcode: $AE; pc:0),
    (name: 'MFENCE'; opcode: $AE; pc:0),
    (name: 'SFENCE'; opcode: $AE; pc:0)
  );

  // Group 16
  
  Group16_18_Mem: TGroupInstructions = (
    (name: 'PREFETCHNTA'; opcode: $18; pc:0),
    (name: 'PREFETCHT0'; opcode: $18; pc:0),
    (name: 'PREFETCHT1'; opcode: $18; pc:0),
    (name: 'PREFETCHT2'; opcode: $18; pc:0),
    (name: 'undefined opcode!'; opcode: $D6;),
    (name: 'undefined opcode!'; opcode: $D6;),
    (name: 'undefined opcode!'; opcode: $D6;),
    (name: 'undefined opcode!'; opcode: $D6;)
  );

  Group16_18_Reg: TGroupInstructions = (
    (name: 'undefined opcode!'; opcode: $D6;),
    (name: 'undefined opcode!'; opcode: $D6;),
    (name: 'undefined opcode!'; opcode: $D6;),
    (name: 'undefined opcode!'; opcode: $D6;),
    (name: 'undefined opcode!'; opcode: $D6;),
    (name: 'undefined opcode!'; opcode: $D6;),
    (name: 'undefined opcode!'; opcode: $D6;),
    (name: 'undefined opcode!'; opcode: $D6;)
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
//    (name: 'mmx'; opcode: $60; SIMD: @simd_;     SIMD_66: @simd_;    SIMD_F2: nil;             SIMD_F3: nil),

