{
  x87 instructions (escape opcodes 0xD8 - 0xDF)
}

const

  // ModRM in 0x00..0xBF
  FPU_A_InstructionSet: array [0..63] of TFPUAInstruction = (

  // D8
    (name: 'FADD'; Operand: M32),
    (name: 'FMUL'; Operand: M32),
    (name: 'FCOM'; Operand: M32),
    (name: 'FCOMP'; Operand: M32),
    (name: 'FSUB'; Operand: M32),
    (name: 'FSUBR'; Operand: M32),
    (name: 'FDIV'; Operand: M32),
    (name: 'FDIVR'; Operand: M32),
  // D9
    (name: 'FLD'; Operand: M32),
    (name: ''; Operand: MMM),
    (name: 'FST'; Operand: M32),
    (name: 'FSTP'; Operand: M32),
    (name: 'FLDENV'; Operand: MMM), // TODO: 14/28
    (name: 'FLDCW'; Operand: M16),
    (name: 'FNSTENV'; Operand: MMM), // TODO: 14/28
    (name: 'FNSTCW'; Operand: M16),
  // DA
    (name: 'FIADD'; Operand: M32),
    (name: 'FIMUL'; Operand: M32),
    (name: 'FICOM'; Operand: M32),
    (name: 'FICOMP'; Operand: M32),
    (name: 'FISUB'; Operand: M32),
    (name: 'FISUBR'; Operand: M32),
    (name: 'FIDIV'; Operand: M32),
    (name: 'FIDIVR'; Operand: M32),
  // DB
    (name: 'FILD'; Operand: M32),
    (name: 'FISTTP'; Operand: M32), // TODO: SSE3 instruction
    (name: 'FIST'; Operand: M32),
    (name: 'FISTP'; Operand: M32),
    (name: ''; Operand: MMM),
    (name: 'FLD';  Operand: M80),
    (name: ''; Operand: MMM),
    (name: 'FSTP'; Operand: M80),
  // DC
    (name: 'FADD'; Operand: M64),
    (name: 'FMUL'; Operand: M64),
    (name: 'FCOM'; Operand: M64),
    (name: 'FCOMP'; Operand: M64),
    (name: 'FSUB'; Operand: M64),
    (name: 'FSUBR'; Operand: M64),
    (name: 'FDIV'; Operand: M64),
    (name: 'FDIVR'; Operand: M64),
  // DD
    (name: 'FLD'; Operand: M64),
    (name: 'FISTTP'; Operand: M64), // TODO: SSE3 instruction
    (name: 'FST'; Operand: M64),
    (name: 'FSTP'; Operand: M64),
    (name: 'FRSTOR'; Operand: MMM), // 98/108
    (name: ''; Operand: MMM),
    (name: 'FNSAVE'; Operand: MMM), // 98/108
    (name: 'FNSTSW'; Operand: M16),
  // DE
    (name: 'FIADD'; Operand: M16),
    (name: 'FIMUL'; Operand: M16),
    (name: 'FICOM'; Operand: M16),
    (name: 'FICOMP'; Operand: M16),
    (name: 'FISUB'; Operand: M16),
    (name: 'FISUBR'; Operand: M16),
    (name: 'FIDIV'; Operand: M16),
    (name: 'FIDIVR'; Operand: M16),
  // DF
    (name: 'FILD'; Operand: M16),
    (name: 'FISTTP'; Operand: M16), // TODO: SSE3 instruction
    (name: 'FIST'; Operand: M16),
    (name: 'FISTP'; Operand: M16),
    (name: 'FBLD'; Operand: M80),
    (name: 'FILD'; Operand: M64),
    (name: 'FBSTP'; Operand: M80),
    (name: 'FISTP'; Operand: M64)
  );


  // ModRM in 0xC0..0xFF
  FPU_B_InstructionSet: array [$D8..$DF] of TFPUInstructionGroup = (

  // 0xD8

    (
    // 0xC0
      (name: 'FADD'; par: 'st0,st0'),
      (name: 'FADD'; par: 'st0,st1'),
      (name: 'FADD'; par: 'st0,st2'),
      (name: 'FADD'; par: 'st0,st3'),
      (name: 'FADD'; par: 'st0,st4'),
      (name: 'FADD'; par: 'st0,st5'),
      (name: 'FADD'; par: 'st0,st6'),
      (name: 'FADD'; par: 'st0,st7'),

    // 0xC8
      (name: 'FMUL'; par: 'st0,st0'),
      (name: 'FMUL'; par: 'st0,st1'),
      (name: 'FMUL'; par: 'st0,st2'),
      (name: 'FMUL'; par: 'st0,st3'),
      (name: 'FMUL'; par: 'st0,st4'),
      (name: 'FMUL'; par: 'st0,st5'),
      (name: 'FMUL'; par: 'st0,st6'),
      (name: 'FMUL'; par: 'st0,st7'),

    // 0xD0
      (name: 'FCOM'; par: 'st0,st0'),
      (name: 'FCOM'; par: 'st0,st1'),
      (name: 'FCOM'; par: 'st0,st2'),
      (name: 'FCOM'; par: 'st0,st3'),
      (name: 'FCOM'; par: 'st0,st4'),
      (name: 'FCOM'; par: 'st0,st5'),
      (name: 'FCOM'; par: 'st0,st6'),
      (name: 'FCOM'; par: 'st0,st7'),

    // 0xD8
      (name: 'FCOMP'; par: 'st0,st0'),
      (name: 'FCOMP'; par: 'st0,st1'),
      (name: 'FCOMP'; par: 'st0,st2'),
      (name: 'FCOMP'; par: 'st0,st3'),
      (name: 'FCOMP'; par: 'st0,st4'),
      (name: 'FCOMP'; par: 'st0,st5'),
      (name: 'FCOMP'; par: 'st0,st6'),
      (name: 'FCOMP'; par: 'st0,st7'),

    // 0xE0
      (name: 'FSUB'; par: 'st0,st0'),
      (name: 'FSUB'; par: 'st0,st1'),
      (name: 'FSUB'; par: 'st0,st2'),
      (name: 'FSUB'; par: 'st0,st3'),
      (name: 'FSUB'; par: 'st0,st4'),
      (name: 'FSUB'; par: 'st0,st5'),
      (name: 'FSUB'; par: 'st0,st6'),
      (name: 'FSUB'; par: 'st0,st7'),

    // 0xE8
      (name: 'FSUBR'; par: 'st0,st0'),
      (name: 'FSUBR'; par: 'st0,st1'),
      (name: 'FSUBR'; par: 'st0,st2'),
      (name: 'FSUBR'; par: 'st0,st3'),
      (name: 'FSUBR'; par: 'st0,st4'),
      (name: 'FSUBR'; par: 'st0,st5'),
      (name: 'FSUBR'; par: 'st0,st6'),
      (name: 'FSUBR'; par: 'st0,st7'),

    // 0xF0
      (name: 'FDIV'; par: 'st0,st0'),
      (name: 'FDIV'; par: 'st0,st1'),
      (name: 'FDIV'; par: 'st0,st2'),
      (name: 'FDIV'; par: 'st0,st3'),
      (name: 'FDIV'; par: 'st0,st4'),
      (name: 'FDIV'; par: 'st0,st5'),
      (name: 'FDIV'; par: 'st0,st6'),
      (name: 'FDIV'; par: 'st0,st7'),

    // 0xF8
      (name: 'FDIVR'; par: 'st0,st0'),
      (name: 'FDIVR'; par: 'st0,st1'),
      (name: 'FDIVR'; par: 'st0,st2'),
      (name: 'FDIVR'; par: 'st0,st3'),
      (name: 'FDIVR'; par: 'st0,st4'),
      (name: 'FDIVR'; par: 'st0,st5'),
      (name: 'FDIVR'; par: 'st0,st6'),
      (name: 'FDIVR'; par: 'st0,st7')
    ),

  // 0xD9

    (
    // 0xC0
      (name: 'FLD'; par: 'st0'),
      (name: 'FLD'; par: 'st1'),
      (name: 'FLD'; par: 'st2'),
      (name: 'FLD'; par: 'st3'),
      (name: 'FLD'; par: 'st4'),
      (name: 'FLD'; par: 'st5'),
      (name: 'FLD'; par: 'st6'),
      (name: 'FLD'; par: 'st7'),

    // 0xC8
      (name: 'FXCH'; par: 'st0,st0'),
      (name: 'FXCH'; par: 'st0,st1'),
      (name: 'FXCH'; par: 'st0,st2'),
      (name: 'FXCH'; par: 'st0,st3'),
      (name: 'FXCH'; par: 'st0,st4'),
      (name: 'FXCH'; par: 'st0,st5'),
      (name: 'FXCH'; par: 'st0,st6'),
      (name: 'FXCH'; par: 'st0,st7'),

    // 0xD0
      (name: 'FNOP'; par: ''),
      (name: ''; par: ''),
      (name: ''; par: ''),
      (name: ''; par: ''),
      (name: ''; par: ''),
      (name: ''; par: ''),
      (name: ''; par: ''),
      (name: ''; par: ''),

    // 0xD8
      (name: ''; par: ''),
      (name: ''; par: ''),
      (name: ''; par: ''),
      (name: ''; par: ''),
      (name: ''; par: ''),
      (name: ''; par: ''),
      (name: ''; par: ''),
      (name: ''; par: ''),

    // 0xE0
      (name: 'FCHS'; par: ''),
      (name: 'FABS'; par: ''),
      (name: ''; par: ''),
      (name: ''; par: ''),
      (name: 'FTST'; par: ''),
      (name: 'FXAM'; par: ''),
      (name: ''; par: ''),
      (name: ''; par: ''),

    // 0xE8
      (name: 'FLD1'; par: ''),
      (name: 'FLDL2T'; par: ''),
      (name: 'FLDL2E'; par: ''),
      (name: 'FLDPI'; par: ''),
      (name: 'FLDLG2'; par: ''),
      (name: 'FLDLN2'; par: ''),
      (name: 'FLDZ'; par: ''),
      (name: ''; par: ''),

    // 0xF0
      (name: 'F2XM1'; par: ''),
      (name: 'FYL2X'; par: ''),
      (name: 'FPTAN'; par: ''),
      (name: 'FPATAN'; par: ''),
      (name: 'FXTRACT'; par: ''),
      (name: 'FPREM1'; par: ''),
      (name: 'FDECSTP'; par: ''),
      (name: 'FINCSTP'; par: ''),

    // 0xF8
      (name: 'FPREM'; par: ''),
      (name: 'FYL2XP1'; par: ''),
      (name: 'FSQRT'; par: ''),
      (name: 'FSINCOS'; par: ''),
      (name: 'FRNDINT'; par: ''),
      (name: 'FSCALE'; par: ''),
      (name: 'FSIN'; par: ''),
      (name: 'FCOS'; par: '')
    ),

  // 0xDA

    (
    // 0xC0
      (name: 'FCMOVB'; par: 'st0,st0'),
      (name: 'FCMOVB'; par: 'st0,st1'),
      (name: 'FCMOVB'; par: 'st0,st2'),
      (name: 'FCMOVB'; par: 'st0,st3'),
      (name: 'FCMOVB'; par: 'st0,st4'),
      (name: 'FCMOVB'; par: 'st0,st5'),
      (name: 'FCMOVB'; par: 'st0,st6'),
      (name: 'FCMOVB'; par: 'st0,st7'),

    // 0xC8
      (name: 'FCMOVE'; par: 'st0,st0'),
      (name: 'FCMOVE'; par: 'st0,st1'),
      (name: 'FCMOVE'; par: 'st0,st2'),
      (name: 'FCMOVE'; par: 'st0,st3'),
      (name: 'FCMOVE'; par: 'st0,st4'),
      (name: 'FCMOVE'; par: 'st0,st5'),
      (name: 'FCMOVE'; par: 'st0,st6'),
      (name: 'FCMOVE'; par: 'st0,st7'),

    // 0xD0
      (name: 'FCMOVBE'; par: 'st0,st0'),
      (name: 'FCMOVBE'; par: 'st0,st1'),
      (name: 'FCMOVBE'; par: 'st0,st2'),
      (name: 'FCMOVBE'; par: 'st0,st3'),
      (name: 'FCMOVBE'; par: 'st0,st4'),
      (name: 'FCMOVBE'; par: 'st0,st5'),
      (name: 'FCMOVBE'; par: 'st0,st6'),
      (name: 'FCMOVBE'; par: 'st0,st7'),

    // 0xD8
      (name: 'FCMOVU'; par: 'st0,st0'),
      (name: 'FCMOVU'; par: 'st0,st1'),
      (name: 'FCMOVU'; par: 'st0,st2'),
      (name: 'FCMOVU'; par: 'st0,st3'),
      (name: 'FCMOVU'; par: 'st0,st4'),
      (name: 'FCMOVU'; par: 'st0,st5'),
      (name: 'FCMOVU'; par: 'st0,st6'),
      (name: 'FCMOVU'; par: 'st0,st7'),

    // 0xE0
      (name: ''; par: ''),
      (name: ''; par: ''),
      (name: ''; par: ''),
      (name: ''; par: ''),
      (name: ''; par: ''),
      (name: ''; par: ''),
      (name: ''; par: ''),
      (name: ''; par: ''),

    // 0xE8
      (name: ''; par: ''),
      (name: 'FUCOMPP'; par: ''),
      (name: ''; par: ''),
      (name: ''; par: ''),
      (name: ''; par: ''),
      (name: ''; par: ''),
      (name: ''; par: ''),
      (name: ''; par: ''),

    // 0xF0
      (name: ''; par: ''),
      (name: ''; par: ''),
      (name: ''; par: ''),
      (name: ''; par: ''),
      (name: ''; par: ''),
      (name: ''; par: ''),
      (name: ''; par: ''),
      (name: ''; par: ''),

    // 0xF8
      (name: ''; par: ''),
      (name: ''; par: ''),
      (name: ''; par: ''),
      (name: ''; par: ''),
      (name: ''; par: ''),
      (name: ''; par: ''),
      (name: ''; par: ''),
      (name: ''; par: '')
    ),

  // 0xDB

    (
    // 0xC0
      (name: 'FCMOVNB'; par: 'st0,st0'),
      (name: 'FCMOVNB'; par: 'st0,st1'),
      (name: 'FCMOVNB'; par: 'st0,st2'),
      (name: 'FCMOVNB'; par: 'st0,st3'),
      (name: 'FCMOVNB'; par: 'st0,st4'),
      (name: 'FCMOVNB'; par: 'st0,st5'),
      (name: 'FCMOVNB'; par: 'st0,st6'),
      (name: 'FCMOVNB'; par: 'st0,st7'),

    // 0xC8
      (name: 'FCMOVNE'; par: 'st0,st0'),
      (name: 'FCMOVNE'; par: 'st0,st1'),
      (name: 'FCMOVNE'; par: 'st0,st2'),
      (name: 'FCMOVNE'; par: 'st0,st3'),
      (name: 'FCMOVNE'; par: 'st0,st4'),
      (name: 'FCMOVNE'; par: 'st0,st5'),
      (name: 'FCMOVNE'; par: 'st0,st6'),
      (name: 'FCMOVNE'; par: 'st0,st7'),

    // 0xD0
      (name: 'FCMOVNBE'; par: 'st0,st0'),
      (name: 'FCMOVNBE'; par: 'st0,st1'),
      (name: 'FCMOVNBE'; par: 'st0,st2'),
      (name: 'FCMOVNBE'; par: 'st0,st3'),
      (name: 'FCMOVNBE'; par: 'st0,st4'),
      (name: 'FCMOVNBE'; par: 'st0,st5'),
      (name: 'FCMOVNBE'; par: 'st0,st6'),
      (name: 'FCMOVNBE'; par: 'st0,st7'),

    // 0xD8
      (name: 'FCMOVNU'; par: 'st0,st0'),
      (name: 'FCMOVNU'; par: 'st0,st1'),
      (name: 'FCMOVNU'; par: 'st0,st2'),
      (name: 'FCMOVNU'; par: 'st0,st3'),
      (name: 'FCMOVNU'; par: 'st0,st4'),
      (name: 'FCMOVNU'; par: 'st0,st5'),
      (name: 'FCMOVNU'; par: 'st0,st6'),
      (name: 'FCMOVNU'; par: 'st0,st7'),

    // 0xE0
      (name: ''; par: ''),
      (name: ''; par: ''),
      (name: 'FNCLEX'; par: ''),
      (name: 'FNINIT'; par: ''),
      (name: ''; par: ''),
      (name: ''; par: ''),
      (name: ''; par: ''),
      (name: ''; par: ''),

    // 0xE8
      (name: 'FUCOMI'; par: 'st0,st0'),
      (name: 'FUCOMI'; par: 'st0,st1'),
      (name: 'FUCOMI'; par: 'st0,st2'),
      (name: 'FUCOMI'; par: 'st0,st3'),
      (name: 'FUCOMI'; par: 'st0,st4'),
      (name: 'FUCOMI'; par: 'st0,st5'),
      (name: 'FUCOMI'; par: 'st0,st6'),
      (name: 'FUCOMI'; par: 'st0,st7'),

    // 0xF0
      (name: 'FCOMI'; par: 'st0,st0'),
      (name: 'FCOMI'; par: 'st0,st1'),
      (name: 'FCOMI'; par: 'st0,st2'),
      (name: 'FCOMI'; par: 'st0,st3'),
      (name: 'FCOMI'; par: 'st0,st4'),
      (name: 'FCOMI'; par: 'st0,st5'),
      (name: 'FCOMI'; par: 'st0,st6'),
      (name: 'FCOMI'; par: 'st0,st7'),

    // 0xF8
      (name: ''; par: ''),
      (name: ''; par: ''),
      (name: ''; par: ''),
      (name: ''; par: ''),
      (name: ''; par: ''),
      (name: ''; par: ''),
      (name: ''; par: ''),
      (name: ''; par: '')
    ),

  // 0xDC

    (
    // 0xC0
      (name: 'FADD'; par: 'st0,st0'),
      (name: 'FADD'; par: 'st1,st0'),
      (name: 'FADD'; par: 'st2,st0'),
      (name: 'FADD'; par: 'st3,st0'),
      (name: 'FADD'; par: 'st4,st0'),
      (name: 'FADD'; par: 'st5,st0'),
      (name: 'FADD'; par: 'st6,st0'),
      (name: 'FADD'; par: 'st7,st0'),

    // 0xC8
      (name: 'FMUL'; par: 'st0,st0'),
      (name: 'FMUL'; par: 'st1,st0'),
      (name: 'FMUL'; par: 'st2,st0'),
      (name: 'FMUL'; par: 'st3,st0'),
      (name: 'FMUL'; par: 'st4,st0'),
      (name: 'FMUL'; par: 'st5,st0'),
      (name: 'FMUL'; par: 'st6,st0'),
      (name: 'FMUL'; par: 'st7,st0'),

    // 0xD0
      (name: ''; par: ''),
      (name: ''; par: ''),
      (name: ''; par: ''),
      (name: ''; par: ''),
      (name: ''; par: ''),
      (name: ''; par: ''),
      (name: ''; par: ''),
      (name: ''; par: ''),

    // 0xD8
      (name: ''; par: ''),
      (name: ''; par: ''),
      (name: ''; par: ''),
      (name: ''; par: ''),
      (name: ''; par: ''),
      (name: ''; par: ''),
      (name: ''; par: ''),
      (name: ''; par: ''),

    // 0xE0
      (name: 'FSUBR'; par: 'st0,st0'),
      (name: 'FSUBR'; par: 'st1,st0'),
      (name: 'FSUBR'; par: 'st2,st0'),
      (name: 'FSUBR'; par: 'st3,st0'),
      (name: 'FSUBR'; par: 'st4,st0'),
      (name: 'FSUBR'; par: 'st5,st0'),
      (name: 'FSUBR'; par: 'st6,st0'),
      (name: 'FSUBR'; par: 'st7,st0'),

    // 0xE8
      (name: 'FSUB'; par: 'st0,st0'),
      (name: 'FSUB'; par: 'st1,st0'),
      (name: 'FSUB'; par: 'st2,st0'),
      (name: 'FSUB'; par: 'st3,st0'),
      (name: 'FSUB'; par: 'st4,st0'),
      (name: 'FSUB'; par: 'st5,st0'),
      (name: 'FSUB'; par: 'st6,st0'),
      (name: 'FSUB'; par: 'st7,st0'),

    // 0xF0
      (name: 'FDIVR'; par: 'st0,st0'),
      (name: 'FDIVR'; par: 'st1,st0'),
      (name: 'FDIVR'; par: 'st2,st0'),
      (name: 'FDIVR'; par: 'st3,st0'),
      (name: 'FDIVR'; par: 'st4,st0'),
      (name: 'FDIVR'; par: 'st5,st0'),
      (name: 'FDIVR'; par: 'st6,st0'),
      (name: 'FDIVR'; par: 'st7,st0'),

    // 0xF8
      (name: 'FDIV'; par: 'st0,st0'),
      (name: 'FDIV'; par: 'st1,st0'),
      (name: 'FDIV'; par: 'st2,st0'),
      (name: 'FDIV'; par: 'st3,st0'),
      (name: 'FDIV'; par: 'st4,st0'),
      (name: 'FDIV'; par: 'st5,st0'),
      (name: 'FDIV'; par: 'st6,st0'),
      (name: 'FDIV'; par: 'st7,st0')
    ),

  // 0xDD

    (
    // 0xC0
      (name: 'FFREE'; par: 'st0'),
      (name: 'FFREE'; par: 'st1'),
      (name: 'FFREE'; par: 'st2'),
      (name: 'FFREE'; par: 'st3'),
      (name: 'FFREE'; par: 'st4'),
      (name: 'FFREE'; par: 'st5'),
      (name: 'FFREE'; par: 'st6'),
      (name: 'FFREE'; par: 'st7'),

    // 0xC8
      (name: ''; par: ''),
      (name: ''; par: ''),
      (name: ''; par: ''),
      (name: ''; par: ''),
      (name: ''; par: ''),
      (name: ''; par: ''),
      (name: ''; par: ''),
      (name: ''; par: ''),

    // 0xD0
      (name: 'FST'; par: 'st0'),
      (name: 'FST'; par: 'st1'),
      (name: 'FST'; par: 'st2'),
      (name: 'FST'; par: 'st3'),
      (name: 'FST'; par: 'st4'),
      (name: 'FST'; par: 'st5'),
      (name: 'FST'; par: 'st6'),
      (name: 'FST'; par: 'st7'),

    // 0xD8
      (name: 'FSTP'; par: 'st0'),
      (name: 'FSTP'; par: 'st1'),
      (name: 'FSTP'; par: 'st2'),
      (name: 'FSTP'; par: 'st3'),
      (name: 'FSTP'; par: 'st4'),
      (name: 'FSTP'; par: 'st5'),
      (name: 'FSTP'; par: 'st6'),
      (name: 'FSTP'; par: 'st7'),

    // 0xE0
      (name: 'FUCOM'; par: 'st0'),
      (name: 'FUCOM'; par: 'st1'),
      (name: 'FUCOM'; par: 'st2'),
      (name: 'FUCOM'; par: 'st3'),
      (name: 'FUCOM'; par: 'st4'),
      (name: 'FUCOM'; par: 'st5'),
      (name: 'FUCOM'; par: 'st6'),
      (name: 'FUCOM'; par: 'st7'),

    // 0xE8
      (name: 'FUCOMP'; par: 'st0'),
      (name: 'FUCOMP'; par: 'st1'),
      (name: 'FUCOMP'; par: 'st2'),
      (name: 'FUCOMP'; par: 'st3'),
      (name: 'FUCOMP'; par: 'st4'),
      (name: 'FUCOMP'; par: 'st5'),
      (name: 'FUCOMP'; par: 'st6'),
      (name: 'FUCOMP'; par: 'st7'),

    // 0xF0
      (name: ''; par: ''),
      (name: ''; par: ''),
      (name: ''; par: ''),
      (name: ''; par: ''),
      (name: ''; par: ''),
      (name: ''; par: ''),
      (name: ''; par: ''),
      (name: ''; par: ''),

    // 0xF8
      (name: ''; par: ''),
      (name: ''; par: ''),
      (name: ''; par: ''),
      (name: ''; par: ''),
      (name: ''; par: ''),
      (name: ''; par: ''),
      (name: ''; par: ''),
      (name: ''; par: '')
    ),

  // 0xDE

    (
    // 0xC0
      (name: 'FADDP'; par: 'st0,st0'),
      (name: 'FADDP'; par: 'st1,st0'),
      (name: 'FADDP'; par: 'st2,st0'),
      (name: 'FADDP'; par: 'st3,st0'),
      (name: 'FADDP'; par: 'st4,st0'),
      (name: 'FADDP'; par: 'st5,st0'),
      (name: 'FADDP'; par: 'st6,st0'),
      (name: 'FADDP'; par: 'st7,st0'),

    // 0xC8
      (name: 'FMULP'; par: 'st0,st0'),
      (name: 'FMULP'; par: 'st1,st0'),
      (name: 'FMULP'; par: 'st2,st0'),
      (name: 'FMULP'; par: 'st3,st0'),
      (name: 'FMULP'; par: 'st4,st0'),
      (name: 'FMULP'; par: 'st5,st0'),
      (name: 'FMULP'; par: 'st6,st0'),
      (name: 'FMULP'; par: 'st7,st0'),

    // 0xD0
      (name: ''; par: ''),
      (name: ''; par: ''),
      (name: ''; par: ''),
      (name: ''; par: ''),
      (name: ''; par: ''),
      (name: ''; par: ''),
      (name: ''; par: ''),
      (name: ''; par: ''),

    // 0xD8
      (name: ''; par: ''),
      (name: 'FCOMPP'; par: ''),
      (name: ''; par: ''),
      (name: ''; par: ''),
      (name: ''; par: ''),
      (name: ''; par: ''),
      (name: ''; par: ''),
      (name: ''; par: ''),

    // 0xE0
      (name: 'FSUBRP'; par: 'st0,st0'),
      (name: 'FSUBRP'; par: 'st1,st0'),
      (name: 'FSUBRP'; par: 'st2,st0'),
      (name: 'FSUBRP'; par: 'st3,st0'),
      (name: 'FSUBRP'; par: 'st4,st0'),
      (name: 'FSUBRP'; par: 'st5,st0'),
      (name: 'FSUBRP'; par: 'st6,st0'),
      (name: 'FSUBRP'; par: 'st7,st0'),

    // 0xE8
      (name: 'FSUBP'; par: 'st0,st0'),
      (name: 'FSUBP'; par: 'st1,st0'),
      (name: 'FSUBP'; par: 'st2,st0'),
      (name: 'FSUBP'; par: 'st3,st0'),
      (name: 'FSUBP'; par: 'st4,st0'),
      (name: 'FSUBP'; par: 'st5,st0'),
      (name: 'FSUBP'; par: 'st6,st0'),
      (name: 'FSUBP'; par: 'st7,st0'),

    // 0xF0
      (name: 'FDIVRP'; par: 'st0,st0'),
      (name: 'FDIVRP'; par: 'st1,st0'),
      (name: 'FDIVRP'; par: 'st2,st0'),
      (name: 'FDIVRP'; par: 'st3,st0'),
      (name: 'FDIVRP'; par: 'st4,st0'),
      (name: 'FDIVRP'; par: 'st5,st0'),
      (name: 'FDIVRP'; par: 'st6,st0'),
      (name: 'FDIVRP'; par: 'st7,st0'),

    // 0xF8
      (name: 'FDIVP'; par: 'st0,st0'),
      (name: 'FDIVP'; par: 'st1,st0'),
      (name: 'FDIVP'; par: 'st2,st0'),
      (name: 'FDIVP'; par: 'st3,st0'),
      (name: 'FDIVP'; par: 'st4,st0'),
      (name: 'FDIVP'; par: 'st5,st0'),
      (name: 'FDIVP'; par: 'st6,st0'),
      (name: 'FDIVP'; par: 'st7,st0')
    ),

  // 0xDF

    (
    // 0xC0
      (name: ''; par: ''),
      (name: ''; par: ''),
      (name: ''; par: ''),
      (name: ''; par: ''),
      (name: ''; par: ''),
      (name: ''; par: ''),
      (name: ''; par: ''),
      (name: ''; par: ''),

    // 0xC8
      (name: ''; par: ''),
      (name: ''; par: ''),
      (name: ''; par: ''),
      (name: ''; par: ''),
      (name: ''; par: ''),
      (name: ''; par: ''),
      (name: ''; par: ''),
      (name: ''; par: ''),

    // 0xD0
      (name: ''; par: ''),
      (name: ''; par: ''),
      (name: ''; par: ''),
      (name: ''; par: ''),
      (name: ''; par: ''),
      (name: ''; par: ''),
      (name: ''; par: ''),
      (name: ''; par: ''),

    // 0xD8
      (name: ''; par: ''),
      (name: ''; par: ''),
      (name: ''; par: ''),
      (name: ''; par: ''),
      (name: ''; par: ''),
      (name: ''; par: ''),
      (name: ''; par: ''),
      (name: ''; par: ''),

    // 0xE0
      (name: 'FNSTSW'; par: 'AX'),
      (name: ''; par: ''),
      (name: ''; par: ''),
      (name: ''; par: ''),
      (name: ''; par: ''),
      (name: ''; par: ''),
      (name: ''; par: ''),
      (name: ''; par: ''),

    // 0xE8
      (name: 'FUCOMIP'; par: 'st0,st0'),
      (name: 'FUCOMIP'; par: 'st0,st1'),
      (name: 'FUCOMIP'; par: 'st0,st2'),
      (name: 'FUCOMIP'; par: 'st0,st3'),
      (name: 'FUCOMIP'; par: 'st0,st4'),
      (name: 'FUCOMIP'; par: 'st0,st5'),
      (name: 'FUCOMIP'; par: 'st0,st6'),
      (name: 'FUCOMIP'; par: 'st0,st7'),

    // 0xF0
      (name: 'FCOMIP'; par: 'st0,st0'),
      (name: 'FCOMIP'; par: 'st0,st1'),
      (name: 'FCOMIP'; par: 'st0,st2'),
      (name: 'FCOMIP'; par: 'st0,st3'),
      (name: 'FCOMIP'; par: 'st0,st4'),
      (name: 'FCOMIP'; par: 'st0,st5'),
      (name: 'FCOMIP'; par: 'st0,st6'),
      (name: 'FCOMIP'; par: 'st0,st7'),

    // 0xF8
      (name: ''; par: ''),
      (name: ''; par: ''),
      (name: ''; par: ''),
      (name: ''; par: ''),
      (name: ''; par: ''),
      (name: ''; par: ''),
      (name: ''; par: ''),
      (name: ''; par: '')
    )
  );
