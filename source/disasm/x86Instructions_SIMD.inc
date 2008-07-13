{
  x86 SIMD Instructions - MMX, SSE, SSE2, SSE3, SSSE3, SSE4

  TODO:
-  prejst cely opcode map, skontrolovat parametre a doplnit SSE3, SSSE3 a SSE4 instrukcie
-  nastavit InstrSet
}

// MMX, SSE(2) instructions


//  simd_: TSIMDInstruction = (name: ''; opcode: $0; OperandsCount: ; Operands: (p1:; p2:); InstrSet: is);

const


//***********************************************************************
//  Opcode (66, F2, F3) 0F xx
//***********************************************************************


// 0x10
  simd_MOVUPS_a: TSIMDInstruction = (name: 'MOVUPS'; opcode: $10; OperandsCount: 2; Operands: (p1:GREGdq ;p2:MODdq); InstrSet: isSSE);
  simd_MOVSS_a: TSIMDInstruction = (name: 'MOVSS'; opcode: $10; OperandsCount: 2; Operands: (p1:GREGdq ;p2:XMM_M32); InstrSet: isSSE);
  simd_MOVUPD_a: TSIMDInstruction = (name: 'MOVUPD'; opcode: $10; OperandsCount: 2; Operands: (p1:GREGdq ;p2:MODdq); InstrSet: isSSE2);
  simd_MOVSD_a: TSIMDInstruction = (name: 'MOVSD'; opcode: $10; OperandsCount: 2; Operands: (p1:GREGdq ;p2:XMM_M64); InstrSet: isSSE2);

// 0x11
  simd_MOVUPS_b: TSIMDInstruction = (name: 'MOVUPS'; opcode: $11; OperandsCount: 2; Operands: (p1:MODdq ;p2:GREGdq); InstrSet: isSSE);
  simd_MOVSS_b: TSIMDInstruction = (name: 'MOVSS'; opcode: $11; OperandsCount: 2; Operands: (p1:XMM_M32 ;p2:GREGdq); InstrSet: isSSE);
  simd_MOVUPD_b: TSIMDInstruction = (name: 'MOVUPD'; opcode: $11; OperandsCount: 2; Operands: (p1:MODdq ;p2:GREGdq); InstrSet: isSSE2);
  simd_MOVSD_b: TSIMDInstruction = (name: 'MOVSD'; opcode: $11; OperandsCount: 2; Operands: (p1:XMM_M64 ;p2:GREGdq); InstrSet: isSSE2);

// 0x12
  simd_MOVLPS_a: TSIMDInstruction = (name: 'MOVLPS'; opcode: $12; OperandsCount: 2; Operands: (p1:GREGdq ;p2:M64); InstrSet: isSSE);    // rovnaky opcode s MOVHLPS ! - (R/M urcuje adresu)
  simd_MOVHLPS: TSIMDInstruction = (name: 'MOVHLPS'; opcode: $12; OperandsCount: 2; Operands: (p1:GREGdq ;p2:R128); InstrSet: isSSE);   // rovnaky opcode s MOVLPS  ! - (R/M urcuje register)
  simd_MOVLPD_a: TSIMDInstruction = (name: 'MOVLPD'; opcode: $12; OperandsCount: 2; Operands: (p1:GREGdq ;p2:M64); InstrSet: isSSE2);
  simd_MOVDDUP: TSIMDInstruction = (name: 'MOVDDUP'; opcode: $0; OperandsCount: 2; Operands: (p1: GREGdq; p2: MODdq); InstrSet: isSSE3);
  simd_MOVSLDUP: TSIMDInstruction = (name: 'MOVSLDUP'; opcode: $0; OperandsCount: 2; Operands: (p1: GREGdq; p2: MODdq); InstrSet: isSSE3);

// 0x13
  simd_MOVLPS_b: TSIMDInstruction = (name: 'MOVLPS'; opcode: $13; OperandsCount: 2; Operands: (p1: M64; p2:GREGdq); InstrSet: isSSE);
  simd_MOVLPD_b: TSIMDInstruction = (name: 'MOVLPD'; opcode: $13; OperandsCount: 2; Operands: (p1: M64; p2:GREGdq); InstrSet: isSSE2);

// 0x14
  simd_UNPCKLPS: TSIMDInstruction = (name: 'UNPCKLPS'; opcode: $14; OperandsCount: 2; Operands: (p1: GREGdq; p2: MODdq); InstrSet: isSSE);
  simd_UNPCKLPD: TSIMDInstruction = (name: 'UNPCKLPD'; opcode: $14; OperandsCount: 2; Operands: (p1: GREGdq; p2: MODdq); InstrSet: isSSE2);

// 0x15
  simd_UNPCKHPS: TSIMDInstruction = (name: 'UNPCKHPS'; opcode: $15; OperandsCount: 2; Operands: (p1:GREGdq ;p2:MODdq); InstrSet: isSSE);
  simd_UNPCKHPD: TSIMDInstruction = (name: 'UNPCKHPD'; opcode: $15; OperandsCount: 2; Operands: (p1:GREGdq ;p2:MODdq); InstrSet: isSSE2);

// 0x16
  simd_MOVHPS_a: TSIMDInstruction = (name: 'MOVHPS'; opcode: $16; OperandsCount: 2; Operands: (p1:GREGdq ;p2:M64); InstrSet: isSSE);     // rovnaky opcode s MOVLHPS
  simd_MOVHPD_a: TSIMDInstruction = (name: 'MOVHPD'; opcode: $16; OperandsCount: 2; Operands: (p1:GREGdq ;p2:M64); InstrSet: isSSE2);
  simd_MOVLHPS: TSIMDInstruction = (name: 'MOVLHPS'; opcode: $16; OperandsCount: 2; Operands: (p1:GREGdq ;p2:R128); InstrSet: isSSE);   // rovnaky opcode s MOVHPS
  simd_MOVSHDUP: TSIMDInstruction = (name: 'MOVSHDUP'; opcode: $0; OperandsCount: 2; Operands: (p1: GREGdq; p2: MODdq); InstrSet: isSSE3);

// 0x17
  simd_MOVHPS_b: TSIMDInstruction = (name: 'MOVHPS'; opcode: $17; OperandsCount: 2; Operands: (p1:M64 ;p2:GREGdq); InstrSet: isSSE);
  simd_MOVHPD_b: TSIMDInstruction = (name: 'MOVHPD'; opcode: $17; OperandsCount: 2; Operands: (p1:M64 ;p2:GREGdq); InstrSet: isSSE2);

// 0x28
  simd_MOVAPS_a: TSIMDInstruction = (name: 'MOVAPS'; opcode: $28; OperandsCount: 2; Operands: (p1:GREGdq ;p2:MODdq); InstrSet: isSSE);
  simd_MOVAPD_a: TSIMDInstruction = (name: 'MOVAPD'; opcode: $28; OperandsCount: 2; Operands: (p1:GREGdq ;p2:MODdq); InstrSet: isSSE2);

// 0x29
  simd_MOVAPS_b: TSIMDInstruction = (name: 'MOVAPS'; opcode: $29; OperandsCount: 2; Operands: (p1:MODdq ;p2:GREGdq); InstrSet: isSSE);
  simd_MOVAPD_b: TSIMDInstruction = (name: 'MOVAPD'; opcode: $29; OperandsCount: 2; Operands: (p1:MODdq ;p2:GREGdq); InstrSet: isSSE2);

// 0x2A
  simd_CVTPI2PS: TSIMDInstruction = (name: 'CVTPI2PS'; opcode: $2A; OperandsCount: 2; Operands: (p1:GREGdq ;p2:MODq); InstrSet: isSSE);
  simd_CVTSI2SS: TSIMDInstruction = (name: 'CVTSI2SS'; opcode: $2A; OperandsCount: 2; Operands: (p1:GREGdq ;p2:MODd); InstrSet: isSSE);
  simd_CVTPI2PD: TSIMDInstruction = (name: 'CVTPI2PD'; opcode: $2A; OperandsCount: 2; Operands: (p1:GREGdq ;p2:MODq); InstrSet: isSSE2);
  simd_CVTSI2SD: TSIMDInstruction = (name: 'CVTSI2SD'; opcode: $2A; OperandsCount: 2; Operands: (p1:GREGdq ;p2:MODd); InstrSet: isSSE2);

// 0x2B
  simd_MOVNTPS: TSIMDInstruction = (name: 'MOVNTPS'; opcode: $2B; OperandsCount: 2; Operands: (p1:M128 ;p2:GREGdq); InstrSet: isSSE);
  simd_MOVNTPD: TSIMDInstruction = (name: 'MOVNTPD'; opcode: $2B; OperandsCount: 2; Operands: (p1:M128 ;p2:GREGdq); InstrSet: isSSE2);

// 0x2C
  simd_CVTTPS2PI: TSIMDInstruction = (name: 'CVTTPS2PI'; opcode: $2C; OperandsCount: 2; Operands: (p1:GREGq ;p2:XMM_M32); InstrSet: isSSE);
  simd_CVTTSS2SI: TSIMDInstruction = (name: 'CVTTSS2SI'; opcode: $2C; OperandsCount: 2; Operands: (p1:GREGd ;p2:XMM_M32); InstrSet: isSSE);
  simd_CVTTPD2PI: TSIMDInstruction = (name: 'CVTTPD2PI'; opcode: $2C; OperandsCount: 2; Operands: (p1:GREGq ;p2:MODdq); InstrSet: isSSE2);
  simd_CVTTSD2SI: TSIMDInstruction = (name: 'CVTTSD2SI'; opcode: $2C; OperandsCount: 2; Operands: (p1:GREGd ;p2:XMM_M64); InstrSet: isSSE2);

// 0x2D
  simd_CVTPS2PI: TSIMDInstruction = (name: 'CVTPS2PI'; opcode: $2D; OperandsCount: 2; Operands: (p1:GREGq ;p2:XMM_M64); InstrSet: isSSE);
  simd_CVTSS2SI: TSIMDInstruction = (name: 'CVTSS2SI'; opcode: $2D; OperandsCount: 2; Operands: (p1:GREGd ;p2:XMM_M32); InstrSet: isSSE);
  simd_CVTPD2PI: TSIMDInstruction = (name: 'CVTPD2PI'; opcode: $2D; OperandsCount: 2; Operands: (p1:GREGq ;p2:MODdq); InstrSet: isSSE2);
  simd_CVTSD2SI: TSIMDInstruction = (name: 'CVTSD2SI'; opcode: $2D; OperandsCount: 2; Operands: (p1:GREGd ;p2:XMM_M64); InstrSet: isSSE2);

// 0x2E
  simd_UCOMISS: TSIMDInstruction = (name: 'UCOMISS'; opcode: $2E; OperandsCount: 2; Operands: (p1:GREGdq ;p2:XMM_M32); InstrSet: isSSE);
  simd_UCOMISD: TSIMDInstruction = (name: 'UCOMISD'; opcode: $2E; OperandsCount: 2; Operands: (p1:GREGdq ;p2:XMM_M64); InstrSet: isSSE2);

// 0x2F
  simd_COMISS: TSIMDInstruction = (name: 'COMISS'; opcode: $2F; OperandsCount: 2; Operands: (p1:GREGdq ;p2:XMM_M32); InstrSet: isSSE);
  simd_COMISD: TSIMDInstruction = (name: 'COMISD'; opcode: $2F; OperandsCount: 2; Operands: (p1:GREGdq ;p2:XMM_M64); InstrSet: isSSE2);

// 0x38 and 0x3A - 3 byte escape

// 0x4x - conditional moves

// 0x50
  simd_MOVMSKPS: TSIMDInstruction = (name: 'MOVMSKPS'; opcode: $50; OperandsCount: 2; Operands: (p1:GREGd; p2: R128); InstrSet: isSSE);
  simd_MOVMSKPD: TSIMDInstruction = (name: 'MOVMSKPD'; opcode: $50; OperandsCount: 2; Operands: (p1:GREGd; p2: R128); InstrSet: isSSE2);

// 0x51
  simd_SQRTPS: TSIMDInstruction = (name: 'SQRTPS'; opcode: $51; OperandsCount: 2; Operands: (p1:GREGdq ;p2:MODdq); InstrSet: isSSE);
  simd_SQRTSS: TSIMDInstruction = (name: 'SQRTSS'; opcode: $51; OperandsCount: 2; Operands: (p1:GREGdq ;p2:XMM_M32); InstrSet: isSSE);
  simd_SQRTPD: TSIMDInstruction = (name: 'SQRTPD'; opcode: $51; OperandsCount: 2; Operands: (p1:GREGdq ;p2:MODdq); InstrSet: isSSE2);
  simd_SQRTSD: TSIMDInstruction = (name: 'SQRTSD'; opcode: $51; OperandsCount: 2; Operands: (p1:GREGdq ;p2:XMM_M64); InstrSet: isSSE2);

// 0x52
  simd_RSQRTPS: TSIMDInstruction = (name: 'RSQRTPS'; opcode: $52; OperandsCount: 2; Operands: (p1:GREGdq; p2:MODdq); InstrSet: isSSE);
  simd_RSQRTSS: TSIMDInstruction = (name: 'RSQRTSS'; opcode: $52; OperandsCount: 2; Operands: (p1:GREGdq; p2:XMM_M32); InstrSet: isSSE);

// 0x53
  simd_RCPPS: TSIMDInstruction = (name: 'RCPPS'; opcode: $53; OperandsCount: 2; Operands: (p1:GREGdq ;p2:MODdq); InstrSet: isSSE);
  simd_RCPSS: TSIMDInstruction = (name: 'RCPSS'; opcode: $53; OperandsCount: 2; Operands: (p1:GREGdq ;p2:XMM_M32); InstrSet: isSSE);

// 0x54
  simd_ANDPS: TSIMDInstruction = (name: 'ANDPS'; opcode: $54; OperandsCount: 2; Operands: (p1:GREGdq ;p2:MODdq); InstrSet: isSSE);
  simd_ANDPD: TSIMDInstruction = (name: 'ANDPD'; opcode: $54; OperandsCount: 2; Operands: (p1:GREGdq ;p2:MODdq); InstrSet: isSSE2);

// 0x55
  simd_ANDNPS: TSIMDInstruction = (name: 'ANDNPS'; opcode: $55; OperandsCount: 2; Operands: (p1:GREGdq ;p2:MODdq); InstrSet: isSSE);
  simd_ANDNPD: TSIMDInstruction = (name: 'ANDNPD'; opcode: $55; OperandsCount: 2; Operands: (p1:GREGdq ;p2:MODdq); InstrSet: isSSE2);

// 0x56
  simd_ORPS: TSIMDInstruction = (name: 'ORPS'; opcode: $56; OperandsCount: 2; Operands: (p1:GREGdq ;p2:MODdq); InstrSet: isSSE);
  simd_ORPD: TSIMDInstruction = (name: 'ORPD'; opcode: $56; OperandsCount: 2; Operands: (p1:GREGdq ;p2:MODdq); InstrSet: isSSE2);

// 0x57
  simd_XORPS: TSIMDInstruction = (name: 'XORPS'; opcode: $57; OperandsCount: 2; Operands: (p1:GREGdq ;p2:MODdq); InstrSet: isSSE);
  simd_XORPD: TSIMDInstruction = (name: 'XORPD'; opcode: $57; OperandsCount: 2; Operands: (p1:GREGdq ;p2:MODdq); InstrSet: isSSE2);

// 0x58
  simd_ADDPS: TSIMDInstruction = (name: 'ADDPS'; opcode: $58; OperandsCount: 2; Operands: (p1:GREGdq ;p2:MODdq); InstrSet: isSSE);
  simd_ADDSS: TSIMDInstruction = (name: 'ADDSS'; opcode: $58; OperandsCount: 2; Operands: (p1:GREGdq ;p2:XMM_M32); InstrSet: isSSE);
  simd_ADDPD: TSIMDInstruction = (name: 'ADDPD'; opcode: $58; OperandsCount: 2; Operands: (p1:GREGdq ;p2:MODdq); InstrSet: isSSE2);
  simd_ADDSD: TSIMDInstruction = (name: 'ADDSD'; opcode: $58; OperandsCount: 2; Operands: (p1:GREGdq ;p2:XMM_M64); InstrSet: isSSE2);

// 0x59
  simd_MULPS: TSIMDInstruction = (name: 'MULPS'; opcode: $59; OperandsCount: 2; Operands: (p1:GREGdq ;p2:MODdq); InstrSet: isSSE);
  simd_MULSS: TSIMDInstruction = (name: 'MULSS'; opcode: $59; OperandsCount: 2; Operands: (p1:GREGdq ;p2:XMM_M32); InstrSet: isSSE);
  simd_MULPD: TSIMDInstruction = (name: 'MULPD'; opcode: $59; OperandsCount: 2; Operands: (p1:GREGdq ;p2:MODdq); InstrSet: isSSE2);
  simd_MULSD: TSIMDInstruction = (name: 'MULSD'; opcode: $59; OperandsCount: 2; Operands: (p1:GREGdq ;p2:XMM_M64); InstrSet: isSSE2);

// 0x5A
  simd_CVTPS2PD: TSIMDInstruction = (name: 'CVTPS2PD'; opcode: $5A; OperandsCount: 2; Operands: (p1:GREGdq ;p2:XMM_M64); InstrSet: isSSE2); // je to divne (druhy parameter), ani MODdq ani XMM_M64 nie je uplne presne, ale exaktne riesenie neexistuje (zatial)
  simd_CVTSS2SD: TSIMDInstruction = (name: 'CVTSS2SD'; opcode: $5A; OperandsCount: 2; Operands: (p1:GREGdq ;p2:XMM_M32); InstrSet: isSSE2);
  simd_CVTPD2PS: TSIMDInstruction = (name: 'CVTPD2PS'; opcode: $5A; OperandsCount: 2; Operands: (p1:GREGdq ;p2:MODdq); InstrSet: isSSE2);
  simd_CVTSD2SS: TSIMDInstruction = (name: 'CVTSD2SS'; opcode: $5A; OperandsCount: 2; Operands: (p1:GREGdq ;p2:XMM_M64); InstrSet: isSSE2);

// 0x5B
  simd_CVTDQ2PS: TSIMDInstruction = (name: 'CVTDQ2PS'; opcode: $5B; OperandsCount: 2; Operands: (p1:GREGdq ;p2:MODdq); InstrSet: isSSE2);
  simd_CVTPS2DQ: TSIMDInstruction = (name: 'CVTPS2DQ'; opcode: $5B; OperandsCount: 2; Operands: (p1:GREGdq ;p2:MODdq); InstrSet: isSSE2);
  simd_CVTTPS2DQ: TSIMDInstruction = (name: 'CVTTPS2DQ'; opcode: $5B; OperandsCount: 2; Operands: (p1:GREGdq ;p2:MODdq); InstrSet: isSSE2);

// 0x5C
  simd_SUBPS: TSIMDInstruction = (name: 'SUBPS'; opcode: $5C; OperandsCount: 2; Operands: (p1:GREGdq ;p2:MODdq); InstrSet: isSSE);
  simd_SUBSS: TSIMDInstruction = (name: 'SUBSS'; opcode: $5C; OperandsCount: 2; Operands: (p1:GREGdq ;p2:XMM_M32); InstrSet: isSSE);
  simd_SUBPD: TSIMDInstruction = (name: 'SUBPD'; opcode: $5C; OperandsCount: 2; Operands: (p1:GREGdq ;p2:MODdq); InstrSet: isSSE2);
  simd_SUBSD: TSIMDInstruction = (name: 'SUBSD'; opcode: $5C; OperandsCount: 2; Operands: (p1:GREGdq ;p2:XMM_M64); InstrSet: isSSE2);

// 0x5D
  simd_MINPS: TSIMDInstruction = (name: 'MINPS'; opcode: $5D; OperandsCount: 2; Operands: (p1:GREGdq ;p2:MODdq); InstrSet: isSSE);
  simd_MINSS: TSIMDInstruction = (name: 'MINSS'; opcode: $5D; OperandsCount: 2; Operands: (p1:GREGdq ;p2:XMM_M32); InstrSet: isSSE);
  simd_MINPD: TSIMDInstruction = (name: 'MINPD'; opcode: $5D; OperandsCount: 2; Operands: (p1:GREGdq ;p2:MODdq); InstrSet: isSSE2);
  simd_MINSD: TSIMDInstruction = (name: 'MINSD'; opcode: $5D; OperandsCount: 2; Operands: (p1:GREGdq ;p2:XMM_M64); InstrSet: isSSE2);

// 0x5E
  simd_DIVPS: TSIMDInstruction = (name: 'DIVPS'; opcode: $5E; OperandsCount: 2; Operands: (p1:GREGdq ;p2:MODdq); InstrSet: isSSE);
  simd_DIVSS: TSIMDInstruction = (name: 'DIVSS'; opcode: $5E; OperandsCount: 2; Operands: (p1:GREGdq ;p2:XMM_M32); InstrSet: isSSE);
  simd_DIVPD: TSIMDInstruction = (name: 'DIVPD'; opcode: $5E; OperandsCount: 2; Operands: (p1:GREGdq ;p2:MODdq); InstrSet: isSSE2);
  simd_DIVSD: TSIMDInstruction = (name: 'DIVSD'; opcode: $5E; OperandsCount: 2; Operands: (p1:GREGdq ;p2:XMM_M64); InstrSet: isSSE2);

// 0x5F
  simd_MAXPS: TSIMDInstruction = (name: 'MAXPS'; opcode: $5F; OperandsCount: 2; Operands: (p1:GREGdq ;p2:MODdq); InstrSet: isSSE);
  simd_MAXSS: TSIMDInstruction = (name: 'MAXSS'; opcode: $5F; OperandsCount: 2; Operands: (p1:GREGdq ;p2:XMM_M32); InstrSet: isSSE);
  simd_MAXPD: TSIMDInstruction = (name: 'MAXPD'; opcode: $5F; OperandsCount: 2; Operands: (p1:GREGdq ;p2:MODdq); InstrSet: isSSE2);
  simd_MAXSD: TSIMDInstruction = (name: 'MAXSD'; opcode: $5F; OperandsCount: 2; Operands: (p1:GREGdq ;p2:XMM_M64); InstrSet: isSSE2);

// 0x60
  simd_PUNPCKLBW_a: TSIMDInstruction = (name: 'PUNPCKLBW'; opcode: $60; OperandsCount: 2; Operands: (p1:GREGq ;p2:MODd); InstrSet: isMMX);
  simd_PUNPCKLBW_b: TSIMDInstruction = (name: 'PUNPCKLBW'; opcode: $60; OperandsCount: 2; Operands: (p1:GREGdq ;p2:MODdq); InstrSet: isSSE2);

// 0x61
  simd_PUNPCKLWD_a: TSIMDInstruction = (name: 'PUNPCKLWD'; opcode: $61; OperandsCount: 2; Operands: (p1:GREGq ;p2:MODd); InstrSet: isMMX);
  simd_PUNPCKLWD_b: TSIMDInstruction = (name: 'PUNPCKLWD'; opcode: $61; OperandsCount: 2; Operands: (p1:GREGdq ;p2:MODdq); InstrSet: isSSE2);

// 0x62
  simd_PUNPCKLDQ_a: TSIMDInstruction = (name: 'PUNPCKLDQ'; opcode: $62; OperandsCount: 2; Operands: (p1:GREGq ;p2:MODd); InstrSet: isMMX);
  simd_PUNPCKLDQ_b: TSIMDInstruction = (name: 'PUNPCKLDQ'; opcode: $62; OperandsCount: 2; Operands: (p1:GREGdq ;p2:MODdq); InstrSet: isSSE2);

// 0x63
  simd_PACKSSWB_a: TSIMDInstruction = (name: 'PACKSSWB'; opcode: $63; OperandsCount: 2; Operands: (p1:GREGq ;p2:MODq); InstrSet: isMMX);
  simd_PACKSSWB_b: TSIMDInstruction = (name: 'PACKSSWB'; opcode: $63; OperandsCount: 2; Operands: (p1:GREGdq ;p2:MODdq); InstrSet: isSSE2);

// 0x64
  simd_PCMPGTB_a: TSIMDInstruction = (name: 'PCMPGTB'; opcode: $64; OperandsCount: 2; Operands: (p1:GREGq ;p2:MODq); InstrSet: isMMX);
  simd_PCMPGTB_b: TSIMDInstruction = (name: 'PCMPGTB'; opcode: $64; OperandsCount: 2; Operands: (p1:GREGdq ;p2:MODdq); InstrSet: isSSE2);

// 0x65
  simd_PCMPGTW_a: TSIMDInstruction = (name: 'PCMPGTW'; opcode: $65; OperandsCount: 2; Operands: (p1:GREGq ;p2:MODq); InstrSet: isMMX);
  simd_PCMPGTW_b: TSIMDInstruction = (name: 'PCMPGTW'; opcode: $65; OperandsCount: 2; Operands: (p1:GREGdq ;p2:MODdq); InstrSet: isSSE2);

// 0x66
  simd_PCMPGTD_a: TSIMDInstruction = (name: 'PCMPGTD'; opcode: $66; OperandsCount: 2; Operands: (p1:GREGq ;p2:MODq); InstrSet: isMMX);
  simd_PCMPGTD_b: TSIMDInstruction = (name: 'PCMPGTD'; opcode: $66; OperandsCount: 2; Operands: (p1:GREGdq ;p2:MODdq); InstrSet: isSSE2);

// 0x67
  simd_PACKUSWB_a: TSIMDInstruction = (name: 'PACKUSWB'; opcode: $67; OperandsCount: 2; Operands: (p1:GREGq; p2:MODq); InstrSet: isMMX);
  simd_PACKUSWB_b: TSIMDInstruction = (name: 'PACKUSWB'; opcode: $67; OperandsCount: 2; Operands: (p1:GREGdq; p2:MODdq); InstrSet: isSSE2);

// 0x68
  simd_PUNPCKHBW_a: TSIMDInstruction = (name: 'PUNPCKHBW'; opcode: $68; OperandsCount: 2; Operands: (p1:GREGq ;p2:MODd); InstrSet: isMMX);
  simd_PUNPCKHBW_b: TSIMDInstruction = (name: 'PUNPCKHBW'; opcode: $68; OperandsCount: 2; Operands: (p1:GREGdq ;p2:MODdq); InstrSet: isSSE2);

// 0x69
  simd_PUNPCKHWD_a: TSIMDInstruction = (name: 'PUNPCKHWD'; opcode: $69; OperandsCount: 2; Operands: (p1:GREGq ;p2:MODd); InstrSet: isMMX);
  simd_PUNPCKHWD_b: TSIMDInstruction = (name: 'PUNPCKHWD'; opcode: $69; OperandsCount: 2; Operands: (p1:GREGdq ;p2:MODdq); InstrSet: isSSE2);

// 0x6A
  simd_PUNPCKHDQ_a: TSIMDInstruction = (name: 'PUNPCKHDQ'; opcode: $6A; OperandsCount: 2; Operands: (p1:GREGq ;p2:MODd); InstrSet: isMMX);
  simd_PUNPCKHDQ_b: TSIMDInstruction = (name: 'PUNPCKHDQ'; opcode: $6A; OperandsCount: 2; Operands: (p1:GREGdq ;p2:MODdq); InstrSet: isSSE2);

// 0x6B
  simd_PACKSSDW_a: TSIMDInstruction = (name: 'PACKSSDW'; opcode: $6B; OperandsCount: 2; Operands: (p1:GREGq ;p2:MODd); InstrSet: isMMX);
  simd_PACKSSDW_b: TSIMDInstruction = (name: 'PACKSSDW'; opcode: $6B; OperandsCount: 2; Operands: (p1:GREGdq ;p2:MODdq); InstrSet: isSSE2);

// 0x6C
  simd_PUNPCKLQDQ: TSIMDInstruction = (name: 'PUNPCKLQDQ'; opcode: $6C; OperandsCount: 2; Operands: (p1:GREGdq ;p2:MODdq); InstrSet: isSSE2);

// 0x6D
  simd_PUNPCKHQDQ: TSIMDInstruction = (name: 'PUNPCKHQDQ'; opcode: $6D; OperandsCount: 2; Operands: (p1:GREGdq ;p2:MODdq); InstrSet: isSSE2);

// 0x6E
  simd_MOVD_a: TSIMDInstruction = (name: 'MOVD'; opcode: $6E; OperandsCount: 2; Operands: (p1:GREGd ;p2:MODd); InstrSet: isMMX);
  simd_MOVD_c: TSIMDInstruction = (name: 'MOVD'; opcode: $6E; OperandsCount: 2; Operands: (p1:GREGdq ;p2:MODd); InstrSet: isSSE2);

// 0x6F
  simd_MOVQ_a: TSIMDInstruction = (name: 'MOVQ'; opcode: $6F; OperandsCount: 2; Operands: (p1:GREGq ;p2:MODq); InstrSet: isMMX);
  simd_MOVDQA_a: TSIMDInstruction = (name: 'MOVDQA'; opcode: $6F; OperandsCount: 2; Operands: (p1:GREGdq ;p2:MODdq); InstrSet: isSSE2);
  simd_MOVDQU_a: TSIMDInstruction = (name: 'MOVDQU'; opcode: $6F; OperandsCount: 2; Operands: (p1:GREGdq ;p2:MODdq); InstrSet: isSSE2);

// 0x70
  simd_PSHUFW: TSIMDInstruction = (name: 'PSHUFW'; opcode: $70; OperandsCount: 3; Operands: (p1:GREGq ;p2:MODq; p3:IMMb); InstrSet: isMMX);
  simd_PSHUFD: TSIMDInstruction = (name: 'PSHUFD'; opcode: $70; OperandsCount: 3; Operands: (p1:GREGdq ;p2:MODdq; p3:IMMb); InstrSet: isSSE2);
  simd_PSHUFHW: TSIMDInstruction = (name: 'PSHUFHW'; opcode: $70; OperandsCount: 3; Operands: (p1:GREGdq ;p2:MODdq; p3:IMMb); InstrSet: isSSE2);
  simd_PSHUFLW: TSIMDInstruction = (name: 'PSHUFLW'; opcode: $70; OperandsCount: 3; Operands: (p1:GREGdq ;p2:MODdq; p3:IMMb); InstrSet: isSSE2);

// 0x74
  simd_PCMPEQB_a: TSIMDInstruction = (name: 'PCMPEQB'; opcode: $74; OperandsCount: 2; Operands: (p1:GREGq ;p2:MODq); InstrSet: isMMX);
  simd_PCMPEQB_b: TSIMDInstruction = (name: 'PCMPEQB'; opcode: $74; OperandsCount: 2; Operands: (p1:GREGdq ;p2:MODdq); InstrSet: isSSE2);

// 0x75
  simd_PCMPEQW_a: TSIMDInstruction = (name: 'PCMPEQW'; opcode: $75; OperandsCount: 2; Operands: (p1:GREGq ;p2:MODq); InstrSet: isMMX);
  simd_PCMPEQW_b: TSIMDInstruction = (name: 'PCMPEQW'; opcode: $75; OperandsCount: 2; Operands: (p1:GREGdq ;p2:MODdq); InstrSet: isSSE2);

// 0x76
  simd_PCMPEQD_a: TSIMDInstruction = (name: 'PCMPEQD'; opcode: $76; OperandsCount: 2; Operands: (p1:GREGq ;p2:MODq); InstrSet: isMMX);
  simd_PCMPEQD_b: TSIMDInstruction = (name: 'PCMPEQD'; opcode: $76; OperandsCount: 2; Operands: (p1:GREGdq ;p2:MODdq); InstrSet: isSSE2);

// 0x7C
  simd_HADDPS: TSIMDInstruction = (name: 'HADDPS'; opcode: $7E; OperandsCount: 2; Operands: (p1: GREGdq; p2:MODdq); InstrSet: isSSE3);
  simd_HADDPD: TSIMDInstruction = (name: 'HADDPD'; opcode: $7E; OperandsCount: 2; Operands: (p1: GREGdq; p2:MODdq); InstrSet: isSSE3);

// 0x7D
  simd_HSUBPS: TSIMDInstruction = (name: 'HADDPS'; opcode: $7E; OperandsCount: 2; Operands: (p1: GREGdq; p2:MODdq); InstrSet: isSSE3);
  simd_HSUBPD: TSIMDInstruction = (name: 'HADDPD'; opcode: $7E; OperandsCount: 2; Operands: (p1: GREGdq; p2:MODdq); InstrSet: isSSE3);

// 0x7E
  simd_MOVD_b: TSIMDInstruction = (name: 'MOVD'; opcode: $7E; OperandsCount: 2; Operands: (p1:MODd ;p2:GREGq); InstrSet: isMMX);
  simd_MOVD_d: TSIMDInstruction = (name: 'MOVD'; opcode: $7E; OperandsCount: 2; Operands: (p1:MODd ;p2:GREGdq); InstrSet: isSSE2);
  simd_MOVQ_c: TSIMDInstruction = (name: 'MOVQ'; opcode: $7E; OperandsCount: 2; Operands: (p1:GREGdq ;p2:XMM_M64); InstrSet: isSSE2);

// 0x7F
  simd_MOVQ_b: TSIMDInstruction = (name: 'MOVQ'; opcode: $7F; OperandsCount: 2; Operands: (p1:MODq ;p2:GREGq); InstrSet: isMMX);
  simd_MOVDQA_b: TSIMDInstruction = (name: 'MOVDQA'; opcode: $7F; OperandsCount: 2; Operands: (p1:MODdq ;p2:GREGdq); InstrSet: isSSE2);
  simd_MOVDQU_b: TSIMDInstruction = (name: 'MOVDQU'; opcode: $7F; OperandsCount: 2; Operands: (p1:MODdq ;p2:GREGdq); InstrSet: isSSE2);

// 0xB8
  simd_POPCNT: TSIMDInstruction = (name: 'POPCNT'; opcode: $B8; OperandsCount: 2; Operands: (p1:GREGv ;p2:MODv ;p3:IMMb); InstrSet: isSSE);

// 0xC2
  simd_CMPPS: TSIMDInstruction = (name: 'CMPPS'; opcode: $C2; OperandsCount: 2; Operands: (p1:GREGdq ;p2:MODdq ;p3:IMMb); InstrSet: isSSE);
  simd_CMPSS: TSIMDInstruction = (name: 'CMPSS'; opcode: $C2; OperandsCount: 2; Operands: (p1:GREGdq ;p2:XMM_M32 ;p3:IMMb); InstrSet: isSSE);
  simd_CMPPD: TSIMDInstruction = (name: 'CMPPD'; opcode: $C2; OperandsCount: 2; Operands: (p1:GREGdq ;p2:MODdq ;p3:IMMb); InstrSet: isSSE2);
  simd_CMPSD: TSIMDInstruction = (name: 'CMPSD'; opcode: $C2; OperandsCount: 2; Operands: (p1:GREGdq ;p2:XMM_M64 ;p3:IMMb); InstrSet: isSSE2);

// 0xC3
  simd_MOVNTI: TSIMDInstruction = (name: 'MOVNTI'; opcode: $C3; OperandsCount: 2; Operands: (p1:M32 ;p2:GREGd); InstrSet: isSSE2);

// 0xC4
  simd_PINSRW_a: TSIMDInstruction = (name: 'PINSRW'; opcode: $C4; OperandsCount: 2; Operands: (p1:GREGq; p2:REG32_M16 ;p3:IMMb); InstrSet: isSSE);
  simd_PINSRW_b: TSIMDInstruction = (name: 'PINSRW'; opcode: $C4; OperandsCount: 2; Operands: (p1:GREGdq; p2:REG32_M16 ;p3:IMMb); InstrSet: isSSE2);

// 0xC5
  simd_PEXTRW_a: TSIMDInstruction = (name: 'PEXTRW'; opcode: $C5; OperandsCount: 2; Operands: (p1:GREGd ;p2:R64 ;p3:IMMb); InstrSet: isSSE);     // Vzdy prisluchajuce registre !!!
  simd_PEXTRW_b: TSIMDInstruction = (name: 'PEXTRW'; opcode: $C5; OperandsCount: 2; Operands: (p1:GREGd ;p2:R128 ;p3:IMMb); InstrSet: isSSE2);    // Vzdy prisluchajuce registre !!!

// 0xC6
  simd_SHUFPS: TSIMDInstruction = (name: 'SHUFPS'; opcode: $C6; OperandsCount: 2; Operands: (p1:GREGdq ;p2:MODdq ;p3:IMMb); InstrSet: isSSE);
  simd_SHUFPD: TSIMDInstruction = (name: 'SHUFPD'; opcode: $C6; OperandsCount: 2; Operands: (p1:GREGdq ;p2:MODdq ;p3:IMMb); InstrSet: isSSE2);

// 0xD0
  simd_ADDSUBPS: TSIMDInstruction = (name: 'ADDSUBPS'; opcode: $D0; OperandsCount: 2; Operands: (p1:GREGdq ;p2:MODdq); InstrSet: isSSE3);
  simd_ADDSUBPD: TSIMDInstruction = (name: 'ADDSUBPD'; opcode: $D0; OperandsCount: 2; Operands: (p1:GREGdq ;p2:MODdq); InstrSet: isSSE3);

// 0xD1
  simd_PSRLW_a: TSIMDInstruction = (name: 'PSRLW'; opcode:$D1 ; OperandsCount: 2; Operands: (p1:GREGq ;p2:MODq); InstrSet: isMMX);
  simd_PSRLW_b: TSIMDInstruction = (name: 'PSRLW'; opcode: $D1; OperandsCount: 2; Operands: (p1:GREGdq ;p2:MODdq); InstrSet: isSSE2);

// 0xD2
  simd_PSRLD_a: TSIMDInstruction = (name: 'PSRLD'; opcode:$D2 ; OperandsCount: 2; Operands: (p1:GREGq ;p2:MODq); InstrSet: isMMX);
  simd_PSRLD_b: TSIMDInstruction = (name: 'PSRLD'; opcode: $D2; OperandsCount: 2; Operands: (p1:GREGdq ;p2:MODdq); InstrSet: isSSE2);

// 0xD3
  simd_PSRLQ_a: TSIMDInstruction = (name: 'PSRLQ'; opcode:$D3 ; OperandsCount: 2; Operands: (p1:GREGq ;p2:MODq); InstrSet: isMMX);
  simd_PSRLQ_b: TSIMDInstruction = (name: 'PSRLQ'; opcode: $D3; OperandsCount: 2; Operands: (p1:GREGdq ;p2:MODdq); InstrSet: isSSE2);

// 0xD4
  simd_PADDQ_a: TSIMDInstruction = (name: 'PADDQ'; opcode: $D4; OperandsCount: 2; Operands: (p1:GREGq ;p2:MODq); InstrSet: isSSE2);
  simd_PADDQ_b: TSIMDInstruction = (name: 'PADDQ'; opcode: $D4; OperandsCount: 2; Operands: (p1:GREGdq ;p2:MODdq); InstrSet: isSSE2);

// 0xD5
  simd_PMULLW_a: TSIMDInstruction = (name: 'PMULLW'; opcode: $D5; OperandsCount: 2; Operands: (p1:GREGq ;p2:MODq); InstrSet: isMMX);
  simd_PMULLW_b: TSIMDInstruction = (name: 'PMULLW'; opcode: $D5; OperandsCount: 2; Operands: (p1:GREGdq ;p2:MODdq); InstrSet: isSSE2);

// 0xD6
  simd_MOVQ_d: TSIMDInstruction = (name: 'MOVQ'; opcode: $D6; OperandsCount: 2; Operands: (p1:XMM_M64; p2:GREGdq); InstrSet: isSSE2);
  simd_MOVQ2DQ: TSIMDInstruction = (name: 'MOVQ2DQ'; opcode: $D6; OperandsCount: 2; Operands: (p1:GREGdq ;p2:R64); InstrSet: isSSE2);
  simd_MOVDQ2Q: TSIMDInstruction = (name: 'MOVDQ2Q'; opcode: $D6; OperandsCount: 2; Operands: (p1:GREGq; p2:R128); InstrSet: isSSE2);

// 0xD7
  simd_PMOVMSKB_a: TSIMDInstruction = (name: 'PMOVMSKB'; opcode: $D7; OperandsCount: 2; Operands: (p1:GREGd ;p2:R64); InstrSet: isSSE);     // Vzdy prisluchajuce registre !!!
  simd_PMOVMSKB_b: TSIMDInstruction = (name: 'PMOVMSKB'; opcode: $D7; OperandsCount: 2; Operands: (p1:GREGd ;p2:R128); InstrSet: isSSE2);    // Vzdy prisluchajuce registre !!!

// 0xD8
  simd_PSUBUSB_a: TSIMDInstruction = (name: 'PSUBUSB'; opcode: $D8; OperandsCount: 2; Operands: (p1:GREGq ;p2:MODq); InstrSet: isMMX);
  simd_PSUBUSB_b: TSIMDInstruction = (name: 'PSUBUSB'; opcode: $D8; OperandsCount: 2; Operands: (p1:GREGdq ;p2:MODdq); InstrSet: isSSE2);

// 0xD9
  simd_PSUBUSW_a: TSIMDInstruction = (name: 'PSUBUSW'; opcode: $D9; OperandsCount: 2; Operands: (p1:GREGq ;p2:MODq); InstrSet: isMMX);
  simd_PSUBUSW_b: TSIMDInstruction = (name: 'PSUBUSW'; opcode: $D9; OperandsCount: 2; Operands: (p1:GREGdq ;p2:MODdq); InstrSet: isSSE2);

// 0xDA
  simd_PMINUB_a: TSIMDInstruction = (name: 'PMINUB'; opcode: $DA; OperandsCount: 2; Operands: (p1:GREGq ;p2:MODq); InstrSet: isSSE);
  simd_PMINUB_b: TSIMDInstruction = (name: 'PMINUB'; opcode: $DA; OperandsCount: 2; Operands: (p1:GREGdq ;p2:MODdq); InstrSet: isSSE2);

// 0xDB
  simd_PAND_a: TSIMDInstruction = (name: 'PAND'; opcode: $DB; OperandsCount: 2; Operands: (p1:GREGq ;p2:MODq); InstrSet: isMMX);
  simd_PAND_b: TSIMDInstruction = (name: 'PAND'; opcode: $DB; OperandsCount: 2; Operands: (p1:GREGdq ;p2:MODdq); InstrSet: isSSE2);

// 0xDC
  simd_PADDUSB_a: TSIMDInstruction = (name: 'PADDUSB'; opcode: $DC; OperandsCount: 2; Operands: (p1:GREGq ;p2:MODq); InstrSet: isMMX);
  simd_PADDUSB_b: TSIMDInstruction = (name: 'PADDUSB'; opcode: $DC; OperandsCount: 2; Operands: (p1:GREGdq ;p2:MODdq); InstrSet: isSSE2);

// 0xDD
  simd_PADDUSW_a: TSIMDInstruction = (name: 'PADDUSW'; opcode: $DD; OperandsCount: 2; Operands: (p1:GREGq ;p2:MODq); InstrSet: isMMX);
  simd_PADDUSW_b: TSIMDInstruction = (name: 'PADDUSW'; opcode: $DD; OperandsCount: 2; Operands: (p1:GREGdq ;p2:MODdq); InstrSet: isSSE2);

// 0xDE
  simd_PMAXUB_a: TSIMDInstruction = (name: 'PMAXUB'; opcode: $DE; OperandsCount: 2; Operands: (p1:GREGq ;p2:MODq); InstrSet: isSSE);
  simd_PMAXUB_b: TSIMDInstruction = (name: 'PMAXUB'; opcode: $DE; OperandsCount: 2; Operands: (p1:GREGdq ;p2:MODdq); InstrSet: isSSE2);

// 0xDF
  simd_PANDN_a: TSIMDInstruction = (name: 'PANDN'; opcode: $DF; OperandsCount: 2; Operands: (p1:GREGq ;p2:MODq); InstrSet: isMMX);
  simd_PANDN_b: TSIMDInstruction = (name: 'PANDN'; opcode: $DF; OperandsCount: 2; Operands: (p1:GREGdq ;p2:MODdq); InstrSet: isSSE2);



//  simd_: TSIMDInstruction = (name: ''; opcode: $0; OperandsCount: ; Operands: (p1:; p2:); InstrSet: is);


// 0xE
  simd_PAVGB_a: TSIMDInstruction = (name: 'PAVGB'; opcode: $E0; OperandsCount: 2; Operands: (p1:GREGq ;p2:MODq); InstrSet: isSSE);
  simd_PAVGB_b: TSIMDInstruction = (name: 'PAVGB'; opcode: $E0; OperandsCount: 2; Operands: (p1:GREGdq ;p2:MODdq); InstrSet: isSSE2);

// 0xE1
  simd_PSRAW_a: TSIMDInstruction = (name: 'PSRAW'; opcode:$E1 ; OperandsCount: 2; Operands: (p1:GREGq ;p2:MODq); InstrSet: isMMX);
  simd_PSRAW_b: TSIMDInstruction = (name: 'PSRAW'; opcode: $E1; OperandsCount: 2; Operands: (p1:GREGdq ;p2:MODdq); InstrSet: isSSE2);

// 0xE2
  simd_PSRAD_a: TSIMDInstruction = (name: 'PSRAD'; opcode:$E2 ; OperandsCount: 2; Operands: (p1:GREGq ;p2:MODq); InstrSet: isMMX);
  simd_PSRAD_b: TSIMDInstruction = (name: 'PSRAD'; opcode: $E2; OperandsCount: 2; Operands: (p1:GREGdq ;p2:MODdq); InstrSet: isSSE2);

// 0xE3
  simd_PAVGW_a: TSIMDInstruction = (name: 'PAVGW'; opcode: $E3; OperandsCount: 2; Operands: (p1:GREGq ;p2:MODq); InstrSet: isSSE);
  simd_PAVGW_b: TSIMDInstruction = (name: 'PAVGW'; opcode: $E3; OperandsCount: 2; Operands: (p1:GREGdq ;p2:MODdq); InstrSet: isSSE2);

// 0xE4
  simd_PMULHUW_a: TSIMDInstruction = (name: 'PMULHUW'; opcode: $E4; OperandsCount: 2; Operands: (p1:GREGq ;p2:MODq); InstrSet: isSSE);
  simd_PMULHUW_b: TSIMDInstruction = (name: 'PMULHUW'; opcode: $E4; OperandsCount: 2; Operands: (p1:GREGdq ;p2:MODdq); InstrSet: isSSE2);

// 0xE5
  simd_PMULHW_a: TSIMDInstruction = (name: 'PMULHW'; opcode: $E5; OperandsCount: 2; Operands: (p1:GREGq ;p2:MODq); InstrSet: isMMX);
  simd_PMULHW_b: TSIMDInstruction = (name: 'PMULHW'; opcode: $E5; OperandsCount: 2; Operands: (p1:GREGdq ;p2:MODdq); InstrSet: isSSE2);

// 0xE6
  simd_CVTPD2DQ: TSIMDInstruction = (name: 'CVTPD2DQ'; opcode: $E6; OperandsCount: 2; Operands: (p1:GREGdq ;p2:MODdq); InstrSet: isSSE2);
  simd_CVTTPD2DQ: TSIMDInstruction = (name: 'CVTTPD2DQ'; opcode: $E6; OperandsCount: 2; Operands: (p1:GREGdq ;p2:MODdq); InstrSet: isSSE2);
  simd_CVTDQ2PD: TSIMDInstruction = (name: 'CVTDQ2PD'; opcode: $E6; OperandsCount: 2; Operands: (p1:GREGdq ;p2:XMM_M64); InstrSet: isSSE2);

// 0xE7
  simd_MOVNTQ: TSIMDInstruction = (name: 'MOVNTQ'; opcode: $E7; OperandsCount: 2; Operands: (p1:M64 ;p2:GREGq); InstrSet: isSSE);
  simd_MOVNTDQ: TSIMDInstruction = (name: 'MOVNTDQ'; opcode: $E7; OperandsCount: 2; Operands: (p1:M128 ;p2:GREGdq); InstrSet: isSSE2);

// 0xE8
  simd_PSUBSB_a: TSIMDInstruction = (name: 'PSUBSB'; opcode: $0; OperandsCount: 2; Operands: (p1:GREGq ;p2:MODq); InstrSet: isMMX);
  simd_PSUBSB_b: TSIMDInstruction = (name: 'PSUBSB'; opcode: $E8; OperandsCount: 2; Operands: (p1:GREGdq ;p2:MODdq); InstrSet: isSSE2);

// 0xE9
  simd_PSUBSW_a: TSIMDInstruction = (name: 'PSUBSW'; opcode: $0; OperandsCount: 2; Operands: (p1:GREGq ;p2:MODq); InstrSet: isMMX);
  simd_PSUBSW_b: TSIMDInstruction = (name: 'PSUBSW'; opcode: $E9; OperandsCount: 2; Operands: (p1:GREGdq ;p2:MODdq); InstrSet: isSSE2);

// 0xEA
  simd_PMINSW_a: TSIMDInstruction = (name: 'PMINSW'; opcode: $EA; OperandsCount: 2; Operands: (p1:GREGq ;p2:MODq); InstrSet: isSSE);
  simd_PMINSW_b: TSIMDInstruction = (name: 'PMINSW'; opcode: $EA; OperandsCount: 2; Operands: (p1:GREGdq ;p2:MODdq); InstrSet: isSSE2);

// 0xEB
  simd_POR_a: TSIMDInstruction = (name: 'POR'; opcode: $EB; OperandsCount: 2; Operands: (p1:GREGq ;p2:MODq); InstrSet: isMMX);
  simd_POR_b: TSIMDInstruction = (name: 'POR'; opcode: $EB; OperandsCount: 2; Operands: (p1:GREGdq ;p2:MODdq); InstrSet: isSSE2);

// 0xEC
  simd_PADDSB_a: TSIMDInstruction = (name: 'PADDSB'; opcode: $0; OperandsCount: 2; Operands: (p1:GREGq ;p2:MODq); InstrSet: isMMX);
  simd_PADDSB_b: TSIMDInstruction = (name: 'PADDSB'; opcode: $EC; OperandsCount: 2; Operands: (p1:GREGdq ;p2:MODdq); InstrSet: isSSE2);

// 0xED
  simd_PADDSW_a: TSIMDInstruction = (name: 'PADDSW'; opcode: $0; OperandsCount: 2; Operands: (p1:GREGq ;p2:MODq); InstrSet: isMMX);
  simd_PADDSW_b: TSIMDInstruction = (name: 'PADDSW'; opcode: $ED; OperandsCount: 2; Operands: (p1:GREGdq ;p2:MODdq); InstrSet: isSSE2);

// 0xEE
  simd_PMAXSW_a: TSIMDInstruction = (name: 'PMAXSW'; opcode: $EE; OperandsCount: 2; Operands: (p1:GREGq ;p2:MODq); InstrSet: isSSE);
  simd_PMAXSW_b: TSIMDInstruction = (name: 'PMAXSW'; opcode: $EE; OperandsCount: 2; Operands: (p1:GREGdq ;p2:MODdq); InstrSet: isSSE2);

// 0xEF
  simd_PXOR_a: TSIMDInstruction = (name: 'PXOR'; opcode: $EF; OperandsCount: 2; Operands: (p1:GREGq ;p2:MODq); InstrSet: isMMX);
  simd_PXOR_b: TSIMDInstruction = (name: 'PXOR'; opcode: $EF; OperandsCount: 2; Operands: (p1:GREGdq ;p2:MODdq); InstrSet: isSSE2);

// 0xF0
  simd_LDDQU: TSIMDInstruction = (name: 'LDDQU'; opcode: $F0; OperandsCount: 2; Operands: (p1:GREGdq ;p2: M128); InstrSet: isSSE3);

// 0xF1
  simd_PSLLW_a: TSIMDInstruction = (name: 'PSLLW'; opcode:$F1 ; OperandsCount: 2; Operands: (p1:GREGq ;p2:MODq); InstrSet: isMMX);
  simd_PSLLW_b: TSIMDInstruction = (name: 'PSLLW'; opcode: $F1; OperandsCount: 2; Operands: (p1:GREGdq ;p2:MODdq); InstrSet: isSSE2);

// 0xF2
  simd_PSLLD_a: TSIMDInstruction = (name: 'PSLLD'; opcode:$F2 ; OperandsCount: 2; Operands: (p1:GREGq ;p2:MODq); InstrSet: isMMX);
  simd_PSLLD_b: TSIMDInstruction = (name: 'PSLLD'; opcode: $F2; OperandsCount: 2; Operands: (p1:GREGdq ;p2:MODdq); InstrSet: isSSE2);

// 0xF3
  simd_PSLLQ_a: TSIMDInstruction = (name: 'PSLLQ'; opcode:$F3 ; OperandsCount: 2; Operands: (p1:GREGq ;p2:MODq); InstrSet: isMMX);
  simd_PSLLQ_b: TSIMDInstruction = (name: 'PSLLQ'; opcode: $F3; OperandsCount: 2; Operands: (p1:GREGdq ;p2:MODdq); InstrSet: isSSE2);

// 0xF4
  simd_PMULUDQ_a: TSIMDInstruction = (name: 'PMULUDQ'; opcode: $F4; OperandsCount: 2; Operands: (p1:GREGq ;p2:MODq); InstrSet: isSSE2);
  simd_PMULUDQ_b: TSIMDInstruction = (name: 'PMULUDQ'; opcode: $F4; OperandsCount: 2; Operands: (p1:GREGdq ;p2:MODdq); InstrSet: isSSE2);

// 0xF5
  simd_PMADDWD_a: TSIMDInstruction = (name: 'PMADDWD'; opcode: $0; OperandsCount: 2; Operands: (p1:GREGq ;p2:MODq); InstrSet: isMMX);
  simd_PMADDWD_b: TSIMDInstruction = (name: 'PMADDWD'; opcode: $F5; OperandsCount: 2; Operands: (p1:GREGdq ;p2:MODdq); InstrSet: isSSE2);

// 0xF6
  simd_PSADBW_a: TSIMDInstruction = (name: 'PSADBW'; opcode: $F6; OperandsCount: 2; Operands: (p1:GREGq ;p2:MODq); InstrSet: isSSE);
  simd_PSADBW_b: TSIMDInstruction = (name: 'PSADBW'; opcode: $F6; OperandsCount: 2; Operands: (p1:GREGdq ;p2:MODdq); InstrSet: isSSE2);

// 0xF7
  simd_MASKMOVQ: TSIMDInstruction = (name: 'MASKMOVQ'; opcode: $F7; OperandsCount: 2; Operands: (p1:GREGq; p2:R64); InstrSet: isSSE);
  simd_MASKMOVDQU: TSIMDInstruction = (name: 'MASKMOVDQU'; opcode: $F7; OperandsCount: 2; Operands: (p1:GREGdq ;p2:R128); InstrSet: isSSE2);

// 0xF8
  simd_PSUBB_a: TSIMDInstruction = (name: 'PSUBB'; opcode: $0; OperandsCount: 2; Operands: (p1:GREGq ;p2:MODq); InstrSet: isMMX);
  simd_PSUBB_b: TSIMDInstruction = (name: 'PSUBB'; opcode: $F8; OperandsCount: 2; Operands: (p1:GREGdq ;p2:MODdq); InstrSet: isSSE2);

// 0xF9
  simd_PSUBW_a: TSIMDInstruction = (name: 'PSUBW'; opcode: $0; OperandsCount: 2; Operands: (p1:GREGq ;p2:MODq); InstrSet: isMMX);
  simd_PSUBW_b: TSIMDInstruction = (name: 'PSUBW'; opcode: $F9; OperandsCount: 2; Operands: (p1:GREGdq ;p2:MODdq); InstrSet: isSSE2);

// 0xFA
  simd_PSUBD_a: TSIMDInstruction = (name: 'PSUBD'; opcode: $0; OperandsCount: 2; Operands: (p1:GREGq ;p2:MODq); InstrSet: isMMX);
  simd_PSUBD_b: TSIMDInstruction = (name: 'PSUBD'; opcode: $FA; OperandsCount: 2; Operands: (p1:GREGdq ;p2:MODdq); InstrSet: isSSE2);

// 0xFB
  simd_PSUBQ_a: TSIMDInstruction = (name: 'PSUBQ'; opcode: $FB; OperandsCount: 2; Operands: (p1:GREGq ;p2:MODq); InstrSet: isSSE2);
  simd_PSUBQ_b: TSIMDInstruction = (name: 'PSUBQ'; opcode: $FB; OperandsCount: 2; Operands: (p1:GREGdq ;p2:MODdq); InstrSet: isSSE2);

// 0xFC
  simd_PADDB_a: TSIMDInstruction = (name: 'PADDB'; opcode: $0; OperandsCount: 2; Operands: (p1:GREGq; p2:MODq); InstrSet: isMMX);
  simd_PADDB_b: TSIMDInstruction = (name: 'PADDB'; opcode: $FC; OperandsCount: 2; Operands: (p1:GREGdq ;p2:MODdq); InstrSet: isSSE2);

// 0xFD
  simd_PADDW_a: TSIMDInstruction = (name: 'PADDW'; opcode: $0; OperandsCount: 2; Operands: (p1:GREGq ;p2:MODq); InstrSet: isMMX);
  simd_PADDW_b: TSIMDInstruction = (name: 'PADDW'; opcode: $FD; OperandsCount: 2; Operands: (p1:GREGdq ;p2:MODdq); InstrSet: isSSE2);

// 0xFE
  simd_PADDD_a: TSIMDInstruction = (name: 'PADDD'; opcode: $0; OperandsCount: 2; Operands: (p1:GREGq ;p2:MODq); InstrSet: isMMX);
  simd_PADDD_b: TSIMDInstruction = (name: 'PADDD'; opcode: $FE; OperandsCount: 2; Operands: (p1:GREGdq ;p2:MODdq); InstrSet: isSSE2);


//***********************************************************************
//  Opcode (66, F2, F3) 0F 38 xx
//***********************************************************************


// 0x00
  simd_PSHUFB_a: TSIMDInstruction = (name: 'PSHUFB'; opcode: $0; OperandsCount: 2; Operands: (p1:GREGq ;p2:MODq); InstrSet: isSSSE3);
  simd_PSHUFB_b: TSIMDInstruction = (name: 'PSHUFB'; opcode: $0; OperandsCount: 2; Operands: (p1:GREGdq ;p2:MODdq); InstrSet: isSSSE3);

// 0x01
  simd_PHADDW_a: TSIMDInstruction = (name: 'PHADDW'; opcode: $0; OperandsCount: 2; Operands: (p1:GREGq ;p2:MODq); InstrSet: isSSSE3);
  simd_PHADDW_b: TSIMDInstruction = (name: 'PHADDW'; opcode: $0; OperandsCount: 2; Operands: (p1:GREGdq ;p2:MODdq); InstrSet: isSSSE3);

// 0x02
  simd_PHADDD_a: TSIMDInstruction = (name: 'PHADDD'; opcode: $0; OperandsCount: 2; Operands: (p1:GREGq ;p2:MODq); InstrSet: isSSSE3);
  simd_PHADDD_b: TSIMDInstruction = (name: 'PHADDD'; opcode: $0; OperandsCount: 2; Operands: (p1:GREGdq ;p2:MODdq); InstrSet: isSSSE3);

// 0x03
  simd_PHADDSW_a: TSIMDInstruction = (name: 'PHADDSW'; opcode: $0; OperandsCount: 2; Operands: (p1:GREGq ;p2:MODq); InstrSet: isSSSE3);
  simd_PHADDSW_b: TSIMDInstruction = (name: 'PHADDSW'; opcode: $0; OperandsCount: 2; Operands: (p1:GREGdq ;p2:MODdq); InstrSet: isSSSE3);

// 0x04
  simd_PMADDUBSW_a: TSIMDInstruction = (name: 'PMADDUBSW'; opcode: $0; OperandsCount: 2; Operands: (p1:GREGq ;p2:MODq); InstrSet: isSSSE3);
  simd_PMADDUBSW_b: TSIMDInstruction = (name: 'PMADDUBSW'; opcode: $0; OperandsCount: 2; Operands: (p1:GREGdq ;p2:MODdq); InstrSet: isSSSE3);

// 0x05
  simd_PHSUBW_a: TSIMDInstruction = (name: 'PHSUBW'; opcode: $0; OperandsCount: 2; Operands: (p1:GREGq ;p2:MODq); InstrSet: isSSSE3);
  simd_PHSUBW_b: TSIMDInstruction = (name: 'PHSUBW'; opcode: $0; OperandsCount: 2; Operands: (p1:GREGdq ;p2:MODdq); InstrSet: isSSSE3);

// 0x06
  simd_PHSUBD_a: TSIMDInstruction = (name: 'PHSUBD'; opcode: $0; OperandsCount: 2; Operands: (p1:GREGq ;p2:MODq); InstrSet: isSSSE3);
  simd_PHSUBD_b: TSIMDInstruction = (name: 'PHSUBD'; opcode: $0; OperandsCount: 2; Operands: (p1:GREGdq ;p2:MODdq); InstrSet: isSSSE3);

// 0x07
  simd_PHSUBSW_a: TSIMDInstruction = (name: 'PHSUBSW'; opcode: $0; OperandsCount: 2; Operands: (p1:GREGq ;p2:MODq); InstrSet: isSSSE3);
  simd_PHSUBSW_b: TSIMDInstruction = (name: 'PHSUBSW'; opcode: $0; OperandsCount: 2; Operands: (p1:GREGdq ;p2:MODdq); InstrSet: isSSSE3);

// 0x08
  simd_PSIGNB_a: TSIMDInstruction = (name: 'PSIGNB'; opcode: $0; OperandsCount: 2; Operands: (p1:GREGq ;p2:MODq); InstrSet: isSSSE3);
  simd_PSIGNB_b: TSIMDInstruction = (name: 'PSIGNB'; opcode: $0; OperandsCount: 2; Operands: (p1:GREGdq ;p2:MODdq); InstrSet: isSSSE3);

// 0x09
  simd_PSIGNW_a: TSIMDInstruction = (name: 'PSIGNW'; opcode: $0; OperandsCount: 2; Operands: (p1:GREGq ;p2:MODq); InstrSet: isSSSE3);
  simd_PSIGNW_b: TSIMDInstruction = (name: 'PSIGNW'; opcode: $0; OperandsCount: 2; Operands: (p1:GREGdq ;p2:MODdq); InstrSet: isSSSE3);

// 0x0A
  simd_PSIGND_a: TSIMDInstruction = (name: 'PSIGND'; opcode: $0; OperandsCount: 2; Operands: (p1:GREGq ;p2:MODq); InstrSet: isSSSE3);
  simd_PSIGND_b: TSIMDInstruction = (name: 'PSIGND'; opcode: $0; OperandsCount: 2; Operands: (p1:GREGdq ;p2:MODdq); InstrSet: isSSSE3);

// 0x0B
  simd_PMULHRSW_a: TSIMDInstruction = (name: 'PMULHRSW'; opcode: $0; OperandsCount: 2; Operands: (p1:GREGq ;p2:MODq); InstrSet: isSSSE3);
  simd_PMULHRSW_b: TSIMDInstruction = (name: 'PMULHRSW'; opcode: $0; OperandsCount: 2; Operands: (p1:GREGdq ;p2:MODdq); InstrSet: isSSSE3);

// 0x10
  simd_PBLENDVB: TSIMDInstruction = (name: 'PBLENDVB'; opcode: $0; OperandsCount: 2; Operands: (p1:GREGdq ;p2:MODdq); InstrSet: isSSSE3);

// 0x14
  simd_BLENDVPS: TSIMDInstruction = (name: 'BLENDVPS'; opcode: $0; OperandsCount: 2; Operands: (p1:GREGdq ;p2:MODdq); InstrSet: isSSSE3);

// 0x15
  simd_BLENDVPD: TSIMDInstruction = (name: 'BLENDVPD'; opcode: $0; OperandsCount: 2; Operands: (p1:GREGdq ;p2:MODdq); InstrSet: isSSSE3);

// 0x17
  simd_PTEST: TSIMDInstruction = (name: 'PTEST'; opcode: $0; OperandsCount: 2; Operands: (p1:GREGdq ;p2:MODdq); InstrSet: isSSSE3);

// 0x1C
  simd_PABSB_a: TSIMDInstruction = (name: 'PABSB'; opcode: $0; OperandsCount: 2; Operands: (p1:GREGq ;p2:MODq); InstrSet: isSSSE3);
  simd_PABSB_b: TSIMDInstruction = (name: 'PABSB'; opcode: $0; OperandsCount: 2; Operands: (p1:GREGdq ;p2:MODdq); InstrSet: isSSSE3);

// 0x1D
  simd_PABSW_a: TSIMDInstruction = (name: 'PABSW'; opcode: $0; OperandsCount: 2; Operands: (p1:GREGq ;p2:MODq); InstrSet: isSSSE3);
  simd_PABSW_b: TSIMDInstruction = (name: 'PABSW'; opcode: $0; OperandsCount: 2; Operands: (p1:GREGdq ;p2:MODdq); InstrSet: isSSSE3);

// 0x1E
  simd_PABSD_a: TSIMDInstruction = (name: 'PABSD'; opcode: $0; OperandsCount: 2; Operands: (p1:GREGq ;p2:MODq); InstrSet: isSSSE3);
  simd_PABSD_b: TSIMDInstruction = (name: 'PABSD'; opcode: $0; OperandsCount: 2; Operands: (p1:GREGdq ;p2:MODdq); InstrSet: isSSSE3);

// 0x20
  simd_PMOVSXBW: TSIMDInstruction = (name: 'PMOVSXBW'; opcode: $0; OperandsCount: 2; Operands: (p1: GREGdq; p2:XMM_M64); InstrSet: isSSE41);

// 0x21
  simd_PMOVSXBD: TSIMDInstruction = (name: 'PMOVSXBD'; opcode: $0; OperandsCount: 2; Operands: (p1: GREGdq; p2:XMM_M32); InstrSet: isSSE41);

// 0x22
  simd_PMOVSXBQ: TSIMDInstruction = (name: 'PMOVSXBQ'; opcode: $0; OperandsCount: 2; Operands: (p1: GREGdq; p2:XMM_M16); InstrSet: isSSE41);

// 0x23
  simd_PMOVSXWD: TSIMDInstruction = (name: 'PMOVSXWD'; opcode: $0; OperandsCount: 2; Operands: (p1: GREGdq; p2:XMM_M64); InstrSet: isSSE41);

// 0x24
  simd_PMOVSXWQ: TSIMDInstruction = (name: 'PMOVSXWQ'; opcode: $0; OperandsCount: 2; Operands: (p1: GREGdq; p2:XMM_M32); InstrSet: isSSE41);

// 0x25
  simd_PMOVSXDQ: TSIMDInstruction = (name: 'PMOVSXDQ'; opcode: $0; OperandsCount: 2; Operands: (p1: GREGdq; p2:XMM_M64); InstrSet: isSSE41);

// 0x28
  simd_PMULDQ: TSIMDInstruction = (name: 'PMULDQ'; opcode: $0; OperandsCount: 2; Operands: (p1: GREGdq; p2: MODdq); InstrSet: isSSE41);

// 0x29
  simd_PCMPEQQ: TSIMDInstruction = (name: 'PCMPEQQ'; opcode: $0; OperandsCount: 2; Operands: (p1: GREGdq; p2: MODdq); InstrSet: isSSE41);

// 0x2A
  simd_MOVNTDQA: TSIMDInstruction = (name: 'MOVNTDQA'; opcode: $0; OperandsCount: 2; Operands: (p1: GREGdq; p2: MODdq); InstrSet: isSSE41);

// 0x2B
  simd_PACKUSDW: TSIMDInstruction = (name: 'PACKUSDW'; opcode: $0; OperandsCount: 2; Operands: (p1: GREGdq; p2: MODdq); InstrSet: isSSE41);

// 0x30
  simd_PMOVZXBW: TSIMDInstruction = (name: 'PMOVZXBW'; opcode: $0; OperandsCount: 2; Operands: (p1: GREGdq; p2:XMM_M64); InstrSet: isSSE41);

// 0x31
  simd_PMOVZXBD: TSIMDInstruction = (name: 'PMOVZXBD'; opcode: $0; OperandsCount: 2; Operands: (p1: GREGdq; p2:XMM_M32); InstrSet: isSSE41);

// 0x32
  simd_PMOVZXBQ: TSIMDInstruction = (name: 'PMOVZXBQ'; opcode: $0; OperandsCount: 2; Operands: (p1: GREGdq; p2:XMM_M16); InstrSet: isSSE41);

// 0x33
  simd_PMOVZXWD: TSIMDInstruction = (name: 'PMOVZXWD'; opcode: $0; OperandsCount: 2; Operands: (p1: GREGdq; p2:XMM_M64); InstrSet: isSSE41);

// 0x34
  simd_PMOVZXWQ: TSIMDInstruction = (name: 'PMOVZXWQ'; opcode: $0; OperandsCount: 2; Operands: (p1: GREGdq; p2:XMM_M32); InstrSet: isSSE41);

// 0x35
  simd_PMOVZXDQ: TSIMDInstruction = (name: 'PMOVZXDQ'; opcode: $0; OperandsCount: 2; Operands: (p1: GREGdq; p2:XMM_M64); InstrSet: isSSE41);

// 0x37
  simd_PCMPGTQ: TSIMDInstruction = (name: 'PCMPGTQ'; opcode: $0; OperandsCount: 2; Operands: (p1: GREGdq; p2: MODdq); InstrSet: isSSE42);

// 0x38
  simd_PMINSB: TSIMDInstruction = (name: 'PMINSB'; opcode: $0; OperandsCount: 2; Operands: (p1: GREGdq; p2: MODdq); InstrSet: isSSE41);

// 0x39
  simd_PMINSD: TSIMDInstruction = (name: 'PMINSD'; opcode: $0; OperandsCount: 2; Operands: (p1: GREGdq; p2: MODdq); InstrSet: isSSE41);

// 0x3A
  simd_PMINUW: TSIMDInstruction = (name: 'PMINUW'; opcode: $0; OperandsCount: 2; Operands: (p1: GREGdq; p2: MODdq); InstrSet: isSSE41);

// 0x3B
  simd_PMINUD: TSIMDInstruction = (name: 'PMINUD'; opcode: $0; OperandsCount: 2; Operands: (p1: GREGdq; p2: MODdq); InstrSet: isSSE41);

// 0x3C
  simd_PMAXSB: TSIMDInstruction = (name: 'PMAXSB'; opcode: $0; OperandsCount: 2; Operands: (p1: GREGdq; p2: MODdq); InstrSet: isSSE41);

// 0x3D
  simd_PMAXSD: TSIMDInstruction = (name: 'PMAXSD'; opcode: $0; OperandsCount: 2; Operands: (p1: GREGdq; p2: MODdq); InstrSet: isSSE41);

// 0x3E
  simd_PMAXUW: TSIMDInstruction = (name: 'PMAXUW'; opcode: $0; OperandsCount: 2; Operands: (p1: GREGdq; p2: MODdq); InstrSet: isSSE41);

// 0x3F
  simd_PMAXUD: TSIMDInstruction = (name: 'PMAXUD'; opcode: $0; OperandsCount: 2; Operands: (p1: GREGdq; p2: MODdq); InstrSet: isSSE41);

// 0x40
  simd_PMULLD: TSIMDInstruction = (name: 'PMULLD'; opcode: $0; OperandsCount: 2; Operands: (p1: GREGdq; p2: MODdq); InstrSet: isSSE41);

// 0x41
  simd_PHMINPOSUW: TSIMDInstruction = (name: 'PHMINPOSUW'; opcode: $0; OperandsCount: 2; Operands: (p1: GREGdq; p2: MODdq); InstrSet: isSSE41);

// 0xF0
  simd_CRC32_a: TSIMDInstruction = (name: 'CRC32'; opcode: $0; OperandsCount: 2; Operands: (p1: GREGd; p2: MODb); InstrSet: isSSE42);

// 0xF1
  simd_CRC32_b: TSIMDInstruction = (name: 'CRC32'; opcode: $0; OperandsCount: 2; Operands: (p1: GREGdq; p2: MODv); InstrSet: isSSE42);

//***********************************************************************
//  Opcode (66, F2, F3) 0F 3A xx
//***********************************************************************

//TODO


// 0x0F
  simd_PALIGNR_a: TSIMDInstruction = (name: 'PALIGNR'; opcode: $0; OperandsCount: 3; Operands: (p1:GREGq ;p2:MODq; p3: IMMb); InstrSet: isSSSE3);
  simd_PALIGNR_b: TSIMDInstruction = (name: 'PALIGNR'; opcode: $0; OperandsCount: 3; Operands: (p1:GREGdq ;p2:MODdq; p3: IMMb); InstrSet: isSSSE3);


//******************************************************************************
//  Group SIMD instructions
//******************************************************************************


// Group 12 - 0F 71
  simd_PSRLW_c: TSIMDInstruction = (name: 'PSRLW'; opcode: $0; OperandsCount: 2; Operands: (p1:R64 ;p2:IMMb); InstrSet: isMMX);
  simd_PSRLW_d: TSIMDInstruction = (name: 'PSRLW'; opcode: $0; OperandsCount: 2; Operands: (p1:R128 ;p2:IMMb); InstrSet: isSSE2);
  simd_PSRAW_c: TSIMDInstruction = (name: 'PSRAW'; opcode: $0; OperandsCount: 2; Operands: (p1:R64 ;p2:IMMb); InstrSet: isMMX);
  simd_PSRAW_d: TSIMDInstruction = (name: 'PSRAW'; opcode: $0; OperandsCount: 2; Operands: (p1:R128 ;p2:IMMb); InstrSet: isSSE2);
  simd_PSLLW_c: TSIMDInstruction = (name: 'PSLLW'; opcode: $0; OperandsCount: 2; Operands: (p1:R64 ;p2:IMMb); InstrSet: isMMX);
  simd_PSLLW_d: TSIMDInstruction = (name: 'PSLLW'; opcode: $0; OperandsCount: 2; Operands: (p1:R128 ;p2:IMMb); InstrSet: isSSE2);

// Group 13 - 0F 72
  simd_PSRLD_c: TSIMDInstruction = (name: 'PSRLD'; opcode: $0; OperandsCount: 2; Operands: (p1:R64 ;p2:IMMb); InstrSet: isMMX);
  simd_PSRLD_d: TSIMDInstruction = (name: 'PSRLD'; opcode: $0; OperandsCount: 2; Operands: (p1:R128 ;p2:IMMb); InstrSet: isSSE2);
  simd_PSRAD_c: TSIMDInstruction = (name: 'PSRAD'; opcode: $0; OperandsCount: 2; Operands: (p1:R64 ;p2:IMMb); InstrSet: isMMX);
  simd_PSRAD_d: TSIMDInstruction = (name: 'PSRAD'; opcode: $0; OperandsCount: 2; Operands: (p1:R128 ;p2:IMMb); InstrSet: isSSE2);
  simd_PSLLD_c: TSIMDInstruction = (name: 'PSLLD'; opcode: $0; OperandsCount: 2; Operands: (p1:R64 ;p2:IMMb); InstrSet: isMMX);
  simd_PSLLD_d: TSIMDInstruction = (name: 'PSLLD'; opcode: $0; OperandsCount: 2; Operands: (p1:R128 ;p2:IMMb); InstrSet: isSSE2);

// Group 14 - 0F 73
  simd_PSRLQ_c: TSIMDInstruction = (name: 'PSRLQ'; opcode: $0; OperandsCount: 2; Operands: (p1:R64 ;p2:IMMb); InstrSet: isMMX);
  simd_PSRLQ_d: TSIMDInstruction = (name: 'PSRLQ'; opcode: $0; OperandsCount: 2; Operands: (p1:R128 ;p2:IMMb); InstrSet: isSSE2);
  simd_PSRLDQ: TSIMDInstruction = (name: 'PSRLDQ'; opcode: $0; OperandsCount: 2; Operands: (p1:R128 ;p2:IMMb); InstrSet: isSSE2);
  simd_PSLLQ_c: TSIMDInstruction = (name: 'PSLLQ'; opcode: $0; OperandsCount: 2; Operands: (p1:R64 ;p2:IMMb); InstrSet: isMMX);
  simd_PSLLQ_d: TSIMDInstruction = (name: 'PSLLQ'; opcode: $0; OperandsCount: 2; Operands: (p1:R128 ;p2:IMMb); InstrSet: isSSE2);
  simd_PSLLDQ: TSIMDInstruction = (name: 'PSLLDQ'; opcode: $0; OperandsCount: 2; Operands: (p1:R128 ;p2:IMMb); InstrSet: isSSE2);



