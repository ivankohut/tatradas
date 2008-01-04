const

  FPUInstruction_set: array [1..PocetFPUInstrukcii] of TFPUInstruction = (
  // first byte $D8
    (name: 'FADD'; opcode: $C0; par: st1),
    (name: 'FMUL'; opcode: $C8; par: st1),
    (name: 'FCOM'; opcode: $D0; par: st1),
    (name: 'FCOMP'; opcode: $D8; par: st1),
    (name: 'FSUB'; opcode: $E0; par: st1),
    (name: 'FSUBR'; opcode: $E8; par: st1),
    (name: 'FDIV'; opcode: $F0; par: st1),
    (name: 'FDIVR'; opcode: $F8; par: st1),
// 0 + 8
  // $D9
    (name: 'FLD'; opcode: $C0; par: st1),
    (name: 'FXCH'; opcode: $C8; par: st1),
    (name: 'FNOP'; opcode: $D0; par: none),
    (name: 'FCHS'; opcode: $E0; par: none),
    (name: 'FABS'; opcode: $E1; par: none),
    (name: 'FTST'; opcode: $E4; par: none),
    (name: 'FXAM'; opcode: $E5; par: none),
    (name: 'FLD1'; opcode: $E8; par: none),
    (name: 'FLDL2T'; opcode: $E9; par: none),
    (name: 'FLDL2E'; opcode: $EA; par: none),
    (name: 'FLDPI'; opcode: $EB; par: none),
    (name: 'FLDLG2'; opcode: $EC; par: none),
    (name: 'FLDLN2'; opcode: $ED; par: none),
    (name: 'FLDZ'; opcode: $EE; par: none),
    (name: 'F2XM1'; opcode: $F0; par: none),
    (name: 'FYL2X'; opcode: $F1; par: none),
    (name: 'FPTAN'; opcode: $F2; par: none),
    (name: 'FPATAN'; opcode: $F3; par: none),
    (name: 'FXTRACT'; opcode: $F4; par: none),
    (name: 'FPREM1'; opcode: $F5; par: none),
    (name: 'FDECSTP'; opcode: $F6; par: none),
    (name: 'FINCSTP'; opcode: $F7; par: none),
    (name: 'FPREM'; opcode: $F8; par: none),
    (name: 'FYL2XP1'; opcode: $F9; par: none),
    (name: 'FSQRT'; opcode: $FA; par: none),
    (name: 'FSINCOS'; opcode: $FB; par: none),
    (name: 'FRNDINT'; opcode: $FC; par: none),
    (name: 'FSCALE'; opcode: $FD; par: none),
    (name: 'FSIN'; opcode: $FE; par: none),
    (name: 'FCOS'; opcode: $FF; par: none),
// 8 + 30 = 38
  // $DA
    (name: 'FCMOVB'; opcode: $C0; par: st1),
    (name: 'FCMOVE'; opcode: $C8; par: st1),
    (name: 'FCMOVBE'; opcode: $D0; par: st1),
    (name: 'FCMOVU'; opcode: $D8; par: st1),
    (name: 'FUCOMPP'; opcode: $E9; par: none),
// 38 + 5 = 43
  //DB
    (name: 'FCMOVNB'; opcode: $C0; par: st1),
    (name: 'FCMOVNE'; opcode: $C8; par: st1),
    (name: 'FCMOVNBE'; opcode: $D0; par: st1),
    (name: 'FCMOVNU'; opcode: $D8; par: st1),
    (name: 'FCLEX'; opcode: $E2; par: none),
    (name: 'FINIT'; opcode: $E3; par: none),
    (name: 'FUCOMI'; opcode: $E8; par: st1),
    (name: 'FCOMI'; opcode: $F0; par: st1),
// 43 + 8 = 51
  // $DC
    (name: 'FADD'; opcode: $C0; par: st2),
    (name: 'FMUL'; opcode: $C8; par: st2),
    (name: 'FSUBR'; opcode: $E0; par: st2),
    (name: 'FSUB'; opcode: $E8; par: st2),
    (name: 'FDIVR'; opcode: $F0; par: st2),
    (name: 'FDIV'; opcode: $F8; par: st2),
// 51 + 6 = 57
  //DD
    (name: 'FFREE'; opcode: $C0; par: st3),
    (name: 'FST'; opcode: $D0; par: st3),
    (name: 'FSTP'; opcode: $D8; par: st3),
    (name: 'FUCOM'; opcode: $E0; par: st2),
    (name: 'FUCOMP'; opcode: $E8; par: st3),
// 57 + 5 = 62
  // $DE
    (name: 'FADDP'; opcode: $C0; par: st2),
    (name: 'FMULP'; opcode: $C8; par: st2),
    (name: 'FCOMPP'; opcode: $D9; par: none),
    (name: 'FSUBRP'; opcode: $E0; par: st2),
    (name: 'FSUBP'; opcode: $E8; par: st2),
    (name: 'FDIVRP'; opcode: $F0; par: st2),
    (name: 'FDIVP'; opcode: $F8; par: st2),
// 62 + 7 = 69
  // $DF
    (name: 'FSTSW AX'; opcode: $E0; par: none),
    (name: 'FUCOMIP'; opcode: $E8; par: st1),
    (name: 'FCOMIP'; opcode: $F0; par: st1),
// 69 + 3 = 72
    (name: 'undefined opcode'; opcode: $00; par: none)
  );


  FPUInstructions: array [0..63] of TFPUInstructionEx = (

  // D8
    (name: 'FADD'; opcode: $0; par: szDword),
    (name: 'FMUL'; opcode: $0; par: szDword),
    (name: 'FCOM'; opcode: $0; par: szDword),
    (name: 'FCOMP'; opcode: $0; par: szDword),
    (name: 'FSUB'; opcode: $0; par: szDword),
    (name: 'FSUBR'; opcode: $0; par: szDword),
    (name: 'FDIV'; opcode: $0; par: szDword),
    (name: 'FDIVR'; opcode: $0; par: szDword),
  // D9
    (name: 'FLD'; opcode: $0; par: szDword),
    (name: 'undefined opcode!'; opcode: $D6; ),
    (name: 'FST'; opcode: $0; par: szDword),
    (name: 'FSTP'; opcode: $0; par: szDword),
    (name: 'FLDENV'; opcode: $0; par: szEmpty),
    (name: 'FLDCW'; opcode: $0; par: szEmpty),
    (name: 'FSTENV'; opcode: $0; par: szEmpty),
    (name: 'FSTCW'; opcode: $0; par: szEmpty),
  // DA
    (name: 'FIADD'; opcode: $0; par: szDword),
    (name: 'FIMUL'; opcode: $0; par: szDword),
    (name: 'FICOM'; opcode: $0; par: szDword),
    (name: 'FICOMP'; opcode: $0; par: szDword),
    (name: 'FISUB'; opcode: $0; par: szDword),
    (name: 'FISUBR'; opcode: $0; par: szDword),
    (name: 'FIDIV'; opcode: $0; par: szDword),
    (name: 'FIDIVR'; opcode: $0; par: szDword),
  // DB
    (name: 'FILD'; opcode: $0; par: szDword),
    (name: 'undefined opcode!'; opcode: $D6;),
    (name: 'FIST'; opcode: $0; par: szDword),
    (name: 'FISTP'; opcode: $0; par: szDword),
    (name: 'undefined opcode!'; opcode: $D6;),
    (name: 'FLD'; opcode: $0; par: szTword),
    (name: 'undefined opcode!'; opcode: $D6;),
    (name: 'FSTP'; opcode: $0; par: szTword),
  // DC
    (name: 'FADD'; opcode: $0; par: szQword),
    (name: 'FMUL'; opcode: $0; par: szQword),
    (name: 'FCOM'; opcode: $0; par: szQword),
    (name: 'FCOMP'; opcode: $0; par: szQword),
    (name: 'FSUB'; opcode: $0; par: szQword),
    (name: 'FSUBR'; opcode: $0; par: szQword),
    (name: 'FDIV'; opcode: $0; par: szQword),
    (name: 'FDIVR'; opcode: $0; par: szQword),
  // DD
    (name: 'FLD'; opcode: $0; par: szQword),
    (name: 'undefined opcode!'; opcode: $D6;),
    (name: 'FST'; opcode: $0; par: szQword),
    (name: 'FSTP'; opcode: $0; par: szQword),
    (name: 'FRSTOR'; opcode: $0; par: szEmpty),
    (name: 'undefined opcode!'; opcode: $D6;),
    (name: 'FSAVE'; opcode: $0; par: szEmpty),
    (name: 'FSTSW'; opcode: $0; par: szEmpty),
  // DE
    (name: 'FIADD'; opcode: $0; par: szWord),
    (name: 'FIMUL'; opcode: $0; par: szWord),
    (name: 'FICOM'; opcode: $0; par: szWord),
    (name: 'FICOMP'; opcode: $0; par: szWord),
    (name: 'FISUB'; opcode: $0; par: szWord),
    (name: 'FISUBR'; opcode: $0; par: szWord),
    (name: 'FIDIV'; opcode: $0; par: szWord),
    (name: 'FIDIVR'; opcode: $0; par: szWord),
  // DF
    (name: 'FILD'; opcode: $0; par: szWord),
    (name: 'undefined opcode!'; opcode: $D6; ),
    (name: 'FIST'; opcode: $0; par: szWord),
    (name: 'FISTP'; opcode: $0; par: szWord),
    (name: 'FBLD'; opcode: $0; par: szTword),
    (name: 'FILD'; opcode: $0; par: szQword),
    (name: 'FBSTP'; opcode: $0; par: szTword),
    (name: 'FISTP'; opcode: $0; par: szQword)
  );


  FPUINstructionNames: array [0..63] of string[10] = (
    ('FADD'),  ('FMUL'),  ('FCOM'),  ('FCOMP'),  ('FSUB'),   ('FSUBR'),  ('FDIV'),   ('FDIVR'),
    ('FLD'),   ('???'),   ('FST'),   ('FSTP'),   ('FLDENV'), ('FLDCW'),  ('FSTENV'), ('FSTCW'),
    ('FIADD'), ('FIMUL'), ('FICOM'), ('FICOMP'), ('FISUB'),  ('FISUBR'), ('FIDIV'),  ('FIDIVR'),
    ('FILD'),  ('???'),   ('FIST'),  ('FISTP'),  ('???'),    ('FLD'),    ('???'),    ('FSTP'),
    ('FADD'),  ('FMUL'),  ('FCOM'),  ('FCOMP'),  ('FSUB'),   ('FSUBR'),  ('FDIV'),   ('FDIVR'),
    ('FLD'),   ('???'),   ('FST'),   ('FSTP'),   ('FRSTOR'), ('???'),    ('FSAVE'),  ('FSTSW'),
    ('FIADD'), ('FIMUL'), ('FICOM'), ('FICOMP'), ('FISUB'),  ('FISUBR'), ('FIDIV'),  ('FIDIVR'),
    ('FILD'),  ('???'),   ('FIST'),  ('FISTP'),  ('FBLD'),   ('FILD'),   ('FBSTP'),  ('FISTP')
  );
