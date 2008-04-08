unit x86DisassemblerTypes;

interface

type

  TParameter = (
    MODb,  MODw,  MODv,  MODd,  MODp,  MODq,  MODt, MODdq,      // Operand urceny pomocou ModRM (adresa alebo General register, urceny pomocou Mod a R/M poli)
    GREGb, GREGw, GREGv, GREGd,        GREGq, GREGdq,           // General register urceny ModRM (reg field)
    SREGw, CREGd, DREGd, TREGd,                                 // Segment, Control, Debug, Test register
    IMMb,  IMMw,  IMMv,         IMMp,                           // Immediate
    RELb,  RELw,  RELv,                                         // Relativny immediate
    OFFb,         OFFv,

    REG32_M16,
    XMM_M32, XMM_M64,                                           // podobne ako MODdq, skutocna velkost operandu je vsak 32/64 bitov, nie 128 ( XMM_M128 by bolo to iste ako MODdq) 
    M32, M64, M128,                                             // prave adresa urcena ModRM (r/m)
    R32, R64, R128,                                             // prave register urceny ModRM (r/m)

    ax, bx, cx, dx, si, di, bp, sp,                             // general registre - dynamicke (menia sa na Exx v priprade 32 bit)
    statDX,                                                     // staticky register (neovplyvnuje ho 16/32 bit oper ani adresy)
    al, bl, cl, dl, ah, bh, ch, dh,                             // casti gen. registrov
    DS, ES, SS, CS, FS, GS,                                     // segment registre
    mm0,  mm1,  mm2,  mm3,  mm4,  mm5,  mm6,  mm7,              // MMX registre
    xmm0, xmm1, xmm2, xmm3, xmm4, xmm5, xmm6, xmm7,             // XMM registre (SSE)
    n1
  );

  TParam = record
    p1, p2, p3: TParameter;
  end;

  TFPUParameter =  (st1, //st(0),st(0)..st(0),st(7)
                    st2, //st(0),st(0)..st(7),st(0)
                    st3, //st(0)..st(7)
                    none
                    );

  TModRMParameterType = (reg, greg, sreg, creg, dreg, treg, regmem, mem);

  TModRMParameterSize = (OneByte, TwoByte, TwoOrFour, FourByte, FourOrSix, MMX, XMM, R32_M16, R128_M32, R128_M64);


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
