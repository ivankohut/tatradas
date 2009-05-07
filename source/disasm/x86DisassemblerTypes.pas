unit x86DisassemblerTypes;

interface

type

  TOperand = (
    MODb,  MODw,  MODv,  MODd,  MODp,  MODq,  MODt, MODdq,      // Operand urceny pomocou ModRM (adresa alebo General register, urceny pomocou Mod a R/M poli)
    GREGb, GREGw, GREGv, GREGd,        GREGq, GREGdq,           // General register urceny ModRM (reg field)
    SREGw, CREGd, DREGd, TREGd,                                 // Segment, Control, Debug, Test register
    IMMb,  IMMw,  IMMv,         IMMp,                           // Immediate
    RELb,  RELw,  RELv,                                         // Relativny immediate
    OFFb,         OFFv,

    REG32_M8, REG32_M16,                                        // 32b register urceny R/M alebo 8 resp. 16 bitova pamat
    XMM_M16, XMM_M32, XMM_M64,                                  // podobne ako MODdq, skutocna velkost operandu je vsak 16 resp. 32 resp. 64 bitov, nie 128 ( XMM_M128 by bolo to iste ako MODdq)
    MMX_M32,                                                    // ModRM(r/m) - MMX register alebo 32 bitova pamat
    MMM, M16, M32, M64, M80, M128,                              // prave adresa urcena ModRM (r/m) (bez velkosti operandu, 16, 32, 64, 80, 128 bitovy operand)
    R32, R64, R128,                                             // prave register urceny ModRM (r/m)

    ax, bx, cx, dx, si, di, bp, sp,                             // general registre - dynamicke (menia sa na Exx v priprade 32 bit)
    statDX,                                                     // staticky register (neovplyvnuje ho 16/32 bit oper ani adresy)
    al, bl, cl, dl, ah, bh, ch, dh,                             // casti gen. registrov
    DS, ES, SS, CS, FS, GS,                                     // segment registre
    mm0,  mm1,  mm2,  mm3,  mm4,  mm5,  mm6,  mm7,              // MMX registre
    xmm0, xmm1, xmm2, xmm3, xmm4, xmm5, xmm6, xmm7,             // XMM registre (SSE)
    n1
  );

  TOperands = record
    p1, p2, p3: TOperand;
  end;

  TFpuOperand =  (
    st1, //st(0),st(0)..st(0),st(7)
    st2, //st(0),st(0)..st(7),st(0)
    st3, //st(0)..st(7)
    none
  );


  TModRMOperandType = (otRegister, otRMReg, otRMMem, otRMRegMem, otSegmentReg, otControlReg, otDebugReg, otTestReg);
  TModRMOperandSize = (osNone, os1, os2, os2or4, os4, os4or6, os8, os10, os16, osR4_M1, osR4_M2, osR8_M4, osR16_M2, osR16_M4, osR16_M8);


  TModRM = record
    FullModRM: byte;
    Moder, RegOp, RM: byte;
    Loaded: boolean;
  end;


  TSIB = record
    FullSIB: byte;
    Scale, Index, Base: byte;
    Loaded: boolean;
  end;


implementation

end.
