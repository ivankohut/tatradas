{
  x86 SIMD Instructions - MMX, SSE, SSE2, SSE3, SSSE3, SSE4
}

const

//***********************************************************************
//  Opcode (66, F2, F3) 0F xx
//***********************************************************************


// 0x10
  simd_MOVUPS_a: TSIMDInstruction = (name: 'MOVUPS'; OperandsCount: 2; Operands: (p1:GREGdq ;p2:MODdq); InstrSet: isSSE);
  simd_MOVSS_a: TSIMDInstruction = (name: 'MOVSS'; OperandsCount: 2; Operands: (p1:GREGdq ;p2:XMM_M32); InstrSet: isSSE);
  simd_MOVUPD_a: TSIMDInstruction = (name: 'MOVUPD'; OperandsCount: 2; Operands: (p1:GREGdq ;p2:MODdq); InstrSet: isSSE2);
  simd_MOVSD_a: TSIMDInstruction = (name: 'MOVSD'; OperandsCount: 2; Operands: (p1:GREGdq ;p2:XMM_M64); InstrSet: isSSE2);

// 0x11
  simd_MOVUPS_b: TSIMDInstruction = (name: 'MOVUPS'; OperandsCount: 2; Operands: (p1:MODdq ;p2:GREGdq); InstrSet: isSSE);
  simd_MOVSS_b: TSIMDInstruction = (name: 'MOVSS'; OperandsCount: 2; Operands: (p1:XMM_M32 ;p2:GREGdq); InstrSet: isSSE);
  simd_MOVUPD_b: TSIMDInstruction = (name: 'MOVUPD'; OperandsCount: 2; Operands: (p1:MODdq ;p2:GREGdq); InstrSet: isSSE2);
  simd_MOVSD_b: TSIMDInstruction = (name: 'MOVSD'; OperandsCount: 2; Operands: (p1:XMM_M64 ;p2:GREGdq); InstrSet: isSSE2);

// 0x12
  simd_MOVLPS_a: TSIMDInstruction = (name: 'MOVLPS'; OperandsCount: 2; Operands: (p1:GREGdq ;p2:M64); InstrSet: isSSE);    // rovnaky opcode s MOVHLPS ! - (R/M urcuje adresu)
  simd_MOVHLPS: TSIMDInstruction = (name: 'MOVHLPS'; OperandsCount: 2; Operands: (p1:GREGdq ;p2:R128); InstrSet: isSSE);   // rovnaky opcode s MOVLPS  ! - (R/M urcuje register)
  simd_MOVLPD_a: TSIMDInstruction = (name: 'MOVLPD'; OperandsCount: 2; Operands: (p1:GREGdq ;p2:M64); InstrSet: isSSE2);
  simd_MOVDDUP: TSIMDInstruction = (name: 'MOVDDUP'; OperandsCount: 2; Operands: (p1: GREGdq; p2: MODdq); InstrSet: isSSE3);
  simd_MOVSLDUP: TSIMDInstruction = (name: 'MOVSLDUP'; OperandsCount: 2; Operands: (p1: GREGdq; p2: MODdq); InstrSet: isSSE3);

// 0x13
  simd_MOVLPS_b: TSIMDInstruction = (name: 'MOVLPS'; OperandsCount: 2; Operands: (p1: M64; p2:GREGdq); InstrSet: isSSE);
  simd_MOVLPD_b: TSIMDInstruction = (name: 'MOVLPD'; OperandsCount: 2; Operands: (p1: M64; p2:GREGdq); InstrSet: isSSE2);

// 0x14
  simd_UNPCKLPS: TSIMDInstruction = (name: 'UNPCKLPS'; OperandsCount: 2; Operands: (p1: GREGdq; p2: MODdq); InstrSet: isSSE);
  simd_UNPCKLPD: TSIMDInstruction = (name: 'UNPCKLPD'; OperandsCount: 2; Operands: (p1: GREGdq; p2: MODdq); InstrSet: isSSE2);

// 0x15
  simd_UNPCKHPS: TSIMDInstruction = (name: 'UNPCKHPS'; OperandsCount: 2; Operands: (p1:GREGdq ;p2:MODdq); InstrSet: isSSE);
  simd_UNPCKHPD: TSIMDInstruction = (name: 'UNPCKHPD'; OperandsCount: 2; Operands: (p1:GREGdq ;p2:MODdq); InstrSet: isSSE2);

// 0x16
  simd_MOVHPS_a: TSIMDInstruction = (name: 'MOVHPS'; OperandsCount: 2; Operands: (p1:GREGdq ;p2:M64); InstrSet: isSSE);     // rovnaky opcode s MOVLHPS
  simd_MOVHPD_a: TSIMDInstruction = (name: 'MOVHPD'; OperandsCount: 2; Operands: (p1:GREGdq ;p2:M64); InstrSet: isSSE2);
  simd_MOVLHPS: TSIMDInstruction = (name: 'MOVLHPS'; OperandsCount: 2; Operands: (p1:GREGdq ;p2:R128); InstrSet: isSSE);   // rovnaky opcode s MOVHPS
  simd_MOVSHDUP: TSIMDInstruction = (name: 'MOVSHDUP'; OperandsCount: 2; Operands: (p1: GREGdq; p2: MODdq); InstrSet: isSSE3);

// 0x17
  simd_MOVHPS_b: TSIMDInstruction = (name: 'MOVHPS'; OperandsCount: 2; Operands: (p1:M64 ;p2:GREGdq); InstrSet: isSSE);
  simd_MOVHPD_b: TSIMDInstruction = (name: 'MOVHPD'; OperandsCount: 2; Operands: (p1:M64 ;p2:GREGdq); InstrSet: isSSE2);

// 0x28
  simd_MOVAPS_a: TSIMDInstruction = (name: 'MOVAPS'; OperandsCount: 2; Operands: (p1:GREGdq ;p2:MODdq); InstrSet: isSSE);
  simd_MOVAPD_a: TSIMDInstruction = (name: 'MOVAPD'; OperandsCount: 2; Operands: (p1:GREGdq ;p2:MODdq); InstrSet: isSSE2);

// 0x29
  simd_MOVAPS_b: TSIMDInstruction = (name: 'MOVAPS'; OperandsCount: 2; Operands: (p1:MODdq ;p2:GREGdq); InstrSet: isSSE);
  simd_MOVAPD_b: TSIMDInstruction = (name: 'MOVAPD'; OperandsCount: 2; Operands: (p1:MODdq ;p2:GREGdq); InstrSet: isSSE2);

// 0x2A
  simd_CVTPI2PS: TSIMDInstruction = (name: 'CVTPI2PS'; OperandsCount: 2; Operands: (p1:GREGdq ;p2:MODq); InstrSet: isSSE);
  simd_CVTSI2SS: TSIMDInstruction = (name: 'CVTSI2SS'; OperandsCount: 2; Operands: (p1:GREGdq ;p2:MODd); InstrSet: isSSE);
  simd_CVTPI2PD: TSIMDInstruction = (name: 'CVTPI2PD'; OperandsCount: 2; Operands: (p1:GREGdq ;p2:MODq); InstrSet: isSSE2);
  simd_CVTSI2SD: TSIMDInstruction = (name: 'CVTSI2SD'; OperandsCount: 2; Operands: (p1:GREGdq ;p2:MODd); InstrSet: isSSE2);

// 0x2B
  simd_MOVNTPS: TSIMDInstruction = (name: 'MOVNTPS'; OperandsCount: 2; Operands: (p1:M128 ;p2:GREGdq); InstrSet: isSSE);
  simd_MOVNTPD: TSIMDInstruction = (name: 'MOVNTPD'; OperandsCount: 2; Operands: (p1:M128 ;p2:GREGdq); InstrSet: isSSE2);

// 0x2C
  simd_CVTTPS2PI: TSIMDInstruction = (name: 'CVTTPS2PI'; OperandsCount: 2; Operands: (p1:GREGq ;p2:XMM_M32); InstrSet: isSSE);
  simd_CVTTSS2SI: TSIMDInstruction = (name: 'CVTTSS2SI'; OperandsCount: 2; Operands: (p1:GREGd ;p2:XMM_M32); InstrSet: isSSE);
  simd_CVTTPD2PI: TSIMDInstruction = (name: 'CVTTPD2PI'; OperandsCount: 2; Operands: (p1:GREGq ;p2:MODdq); InstrSet: isSSE2);
  simd_CVTTSD2SI: TSIMDInstruction = (name: 'CVTTSD2SI'; OperandsCount: 2; Operands: (p1:GREGd ;p2:XMM_M64); InstrSet: isSSE2);

// 0x2D
  simd_CVTPS2PI: TSIMDInstruction = (name: 'CVTPS2PI'; OperandsCount: 2; Operands: (p1:GREGq ;p2:XMM_M64); InstrSet: isSSE);
  simd_CVTSS2SI: TSIMDInstruction = (name: 'CVTSS2SI'; OperandsCount: 2; Operands: (p1:GREGd ;p2:XMM_M32); InstrSet: isSSE);
  simd_CVTPD2PI: TSIMDInstruction = (name: 'CVTPD2PI'; OperandsCount: 2; Operands: (p1:GREGq ;p2:MODdq); InstrSet: isSSE2);
  simd_CVTSD2SI: TSIMDInstruction = (name: 'CVTSD2SI'; OperandsCount: 2; Operands: (p1:GREGd ;p2:XMM_M64); InstrSet: isSSE2);

// 0x2E
  simd_UCOMISS: TSIMDInstruction = (name: 'UCOMISS'; OperandsCount: 2; Operands: (p1:GREGdq ;p2:XMM_M32); InstrSet: isSSE);
  simd_UCOMISD: TSIMDInstruction = (name: 'UCOMISD'; OperandsCount: 2; Operands: (p1:GREGdq ;p2:XMM_M64); InstrSet: isSSE2);

// 0x2F
  simd_COMISS: TSIMDInstruction = (name: 'COMISS'; OperandsCount: 2; Operands: (p1:GREGdq ;p2:XMM_M32); InstrSet: isSSE);
  simd_COMISD: TSIMDInstruction = (name: 'COMISD'; OperandsCount: 2; Operands: (p1:GREGdq ;p2:XMM_M64); InstrSet: isSSE2);

// 0x38 and 0x3A - 3 byte escape

// 0x4x - conditional moves

// 0x50
  simd_MOVMSKPS: TSIMDInstruction = (name: 'MOVMSKPS'; OperandsCount: 2; Operands: (p1:GREGd; p2: R128); InstrSet: isSSE);
  simd_MOVMSKPD: TSIMDInstruction = (name: 'MOVMSKPD'; OperandsCount: 2; Operands: (p1:GREGd; p2: R128); InstrSet: isSSE2);

// 0x51
  simd_SQRTPS: TSIMDInstruction = (name: 'SQRTPS'; OperandsCount: 2; Operands: (p1:GREGdq ;p2:MODdq); InstrSet: isSSE);
  simd_SQRTSS: TSIMDInstruction = (name: 'SQRTSS'; OperandsCount: 2; Operands: (p1:GREGdq ;p2:XMM_M32); InstrSet: isSSE);
  simd_SQRTPD: TSIMDInstruction = (name: 'SQRTPD'; OperandsCount: 2; Operands: (p1:GREGdq ;p2:MODdq); InstrSet: isSSE2);
  simd_SQRTSD: TSIMDInstruction = (name: 'SQRTSD'; OperandsCount: 2; Operands: (p1:GREGdq ;p2:XMM_M64); InstrSet: isSSE2);

// 0x52
  simd_RSQRTPS: TSIMDInstruction = (name: 'RSQRTPS'; OperandsCount: 2; Operands: (p1:GREGdq; p2:MODdq); InstrSet: isSSE);
  simd_RSQRTSS: TSIMDInstruction = (name: 'RSQRTSS'; OperandsCount: 2; Operands: (p1:GREGdq; p2:XMM_M32); InstrSet: isSSE);

// 0x53
  simd_RCPPS: TSIMDInstruction = (name: 'RCPPS'; OperandsCount: 2; Operands: (p1:GREGdq ;p2:MODdq); InstrSet: isSSE);
  simd_RCPSS: TSIMDInstruction = (name: 'RCPSS'; OperandsCount: 2; Operands: (p1:GREGdq ;p2:XMM_M32); InstrSet: isSSE);

// 0x54
  simd_ANDPS: TSIMDInstruction = (name: 'ANDPS'; OperandsCount: 2; Operands: (p1:GREGdq ;p2:MODdq); InstrSet: isSSE);
  simd_ANDPD: TSIMDInstruction = (name: 'ANDPD'; OperandsCount: 2; Operands: (p1:GREGdq ;p2:MODdq); InstrSet: isSSE2);

// 0x55
  simd_ANDNPS: TSIMDInstruction = (name: 'ANDNPS'; OperandsCount: 2; Operands: (p1:GREGdq ;p2:MODdq); InstrSet: isSSE);
  simd_ANDNPD: TSIMDInstruction = (name: 'ANDNPD'; OperandsCount: 2; Operands: (p1:GREGdq ;p2:MODdq); InstrSet: isSSE2);

// 0x56
  simd_ORPS: TSIMDInstruction = (name: 'ORPS'; OperandsCount: 2; Operands: (p1:GREGdq ;p2:MODdq); InstrSet: isSSE);
  simd_ORPD: TSIMDInstruction = (name: 'ORPD'; OperandsCount: 2; Operands: (p1:GREGdq ;p2:MODdq); InstrSet: isSSE2);

// 0x57
  simd_XORPS: TSIMDInstruction = (name: 'XORPS'; OperandsCount: 2; Operands: (p1:GREGdq ;p2:MODdq); InstrSet: isSSE);
  simd_XORPD: TSIMDInstruction = (name: 'XORPD'; OperandsCount: 2; Operands: (p1:GREGdq ;p2:MODdq); InstrSet: isSSE2);

// 0x58
  simd_ADDPS: TSIMDInstruction = (name: 'ADDPS'; OperandsCount: 2; Operands: (p1:GREGdq ;p2:MODdq); InstrSet: isSSE);
  simd_ADDSS: TSIMDInstruction = (name: 'ADDSS'; OperandsCount: 2; Operands: (p1:GREGdq ;p2:XMM_M32); InstrSet: isSSE);
  simd_ADDPD: TSIMDInstruction = (name: 'ADDPD'; OperandsCount: 2; Operands: (p1:GREGdq ;p2:MODdq); InstrSet: isSSE2);
  simd_ADDSD: TSIMDInstruction = (name: 'ADDSD'; OperandsCount: 2; Operands: (p1:GREGdq ;p2:XMM_M64); InstrSet: isSSE2);

// 0x59
  simd_MULPS: TSIMDInstruction = (name: 'MULPS'; OperandsCount: 2; Operands: (p1:GREGdq ;p2:MODdq); InstrSet: isSSE);
  simd_MULSS: TSIMDInstruction = (name: 'MULSS'; OperandsCount: 2; Operands: (p1:GREGdq ;p2:XMM_M32); InstrSet: isSSE);
  simd_MULPD: TSIMDInstruction = (name: 'MULPD'; OperandsCount: 2; Operands: (p1:GREGdq ;p2:MODdq); InstrSet: isSSE2);
  simd_MULSD: TSIMDInstruction = (name: 'MULSD'; OperandsCount: 2; Operands: (p1:GREGdq ;p2:XMM_M64); InstrSet: isSSE2);

// 0x5A
  simd_CVTPS2PD: TSIMDInstruction = (name: 'CVTPS2PD'; OperandsCount: 2; Operands: (p1:GREGdq ;p2:XMM_M64); InstrSet: isSSE2); // je to divne (druhy parameter), ani MODdq ani XMM_M64 nie je uplne presne, ale exaktne riesenie neexistuje (zatial)
  simd_CVTSS2SD: TSIMDInstruction = (name: 'CVTSS2SD'; OperandsCount: 2; Operands: (p1:GREGdq ;p2:XMM_M32); InstrSet: isSSE2);
  simd_CVTPD2PS: TSIMDInstruction = (name: 'CVTPD2PS'; OperandsCount: 2; Operands: (p1:GREGdq ;p2:MODdq); InstrSet: isSSE2);
  simd_CVTSD2SS: TSIMDInstruction = (name: 'CVTSD2SS'; OperandsCount: 2; Operands: (p1:GREGdq ;p2:XMM_M64); InstrSet: isSSE2);

// 0x5B
  simd_CVTDQ2PS: TSIMDInstruction = (name: 'CVTDQ2PS'; OperandsCount: 2; Operands: (p1:GREGdq ;p2:MODdq); InstrSet: isSSE2);
  simd_CVTPS2DQ: TSIMDInstruction = (name: 'CVTPS2DQ'; OperandsCount: 2; Operands: (p1:GREGdq ;p2:MODdq); InstrSet: isSSE2);
  simd_CVTTPS2DQ: TSIMDInstruction = (name: 'CVTTPS2DQ'; OperandsCount: 2; Operands: (p1:GREGdq ;p2:MODdq); InstrSet: isSSE2);

// 0x5C
  simd_SUBPS: TSIMDInstruction = (name: 'SUBPS'; OperandsCount: 2; Operands: (p1:GREGdq ;p2:MODdq); InstrSet: isSSE);
  simd_SUBSS: TSIMDInstruction = (name: 'SUBSS'; OperandsCount: 2; Operands: (p1:GREGdq ;p2:XMM_M32); InstrSet: isSSE);
  simd_SUBPD: TSIMDInstruction = (name: 'SUBPD'; OperandsCount: 2; Operands: (p1:GREGdq ;p2:MODdq); InstrSet: isSSE2);
  simd_SUBSD: TSIMDInstruction = (name: 'SUBSD'; OperandsCount: 2; Operands: (p1:GREGdq ;p2:XMM_M64); InstrSet: isSSE2);

// 0x5D
  simd_MINPS: TSIMDInstruction = (name: 'MINPS'; OperandsCount: 2; Operands: (p1:GREGdq ;p2:MODdq); InstrSet: isSSE);
  simd_MINSS: TSIMDInstruction = (name: 'MINSS'; OperandsCount: 2; Operands: (p1:GREGdq ;p2:XMM_M32); InstrSet: isSSE);
  simd_MINPD: TSIMDInstruction = (name: 'MINPD'; OperandsCount: 2; Operands: (p1:GREGdq ;p2:MODdq); InstrSet: isSSE2);
  simd_MINSD: TSIMDInstruction = (name: 'MINSD'; OperandsCount: 2; Operands: (p1:GREGdq ;p2:XMM_M64); InstrSet: isSSE2);

// 0x5E
  simd_DIVPS: TSIMDInstruction = (name: 'DIVPS'; OperandsCount: 2; Operands: (p1:GREGdq ;p2:MODdq); InstrSet: isSSE);
  simd_DIVSS: TSIMDInstruction = (name: 'DIVSS'; OperandsCount: 2; Operands: (p1:GREGdq ;p2:XMM_M32); InstrSet: isSSE);
  simd_DIVPD: TSIMDInstruction = (name: 'DIVPD'; OperandsCount: 2; Operands: (p1:GREGdq ;p2:MODdq); InstrSet: isSSE2);
  simd_DIVSD: TSIMDInstruction = (name: 'DIVSD'; OperandsCount: 2; Operands: (p1:GREGdq ;p2:XMM_M64); InstrSet: isSSE2);

// 0x5F
  simd_MAXPS: TSIMDInstruction = (name: 'MAXPS'; OperandsCount: 2; Operands: (p1:GREGdq ;p2:MODdq); InstrSet: isSSE);
  simd_MAXSS: TSIMDInstruction = (name: 'MAXSS'; OperandsCount: 2; Operands: (p1:GREGdq ;p2:XMM_M32); InstrSet: isSSE);
  simd_MAXPD: TSIMDInstruction = (name: 'MAXPD'; OperandsCount: 2; Operands: (p1:GREGdq ;p2:MODdq); InstrSet: isSSE2);
  simd_MAXSD: TSIMDInstruction = (name: 'MAXSD'; OperandsCount: 2; Operands: (p1:GREGdq ;p2:XMM_M64); InstrSet: isSSE2);

// 0x60
  simd_PUNPCKLBW_a: TSIMDInstruction = (name: 'PUNPCKLBW'; OperandsCount: 2; Operands: (p1:GREGq ;p2:MODq); InstrSet: isMMX);
  simd_PUNPCKLBW_b: TSIMDInstruction = (name: 'PUNPCKLBW'; OperandsCount: 2; Operands: (p1:GREGdq ;p2:MODdq); InstrSet: isSSE2);

// 0x61
  simd_PUNPCKLWD_a: TSIMDInstruction = (name: 'PUNPCKLWD'; OperandsCount: 2; Operands: (p1:GREGq ;p2:MODq); InstrSet: isMMX);
  simd_PUNPCKLWD_b: TSIMDInstruction = (name: 'PUNPCKLWD'; OperandsCount: 2; Operands: (p1:GREGdq ;p2:MODdq); InstrSet: isSSE2);

// 0x62
  simd_PUNPCKLDQ_a: TSIMDInstruction = (name: 'PUNPCKLDQ'; OperandsCount: 2; Operands: (p1:GREGq ;p2:MODq); InstrSet: isMMX);
  simd_PUNPCKLDQ_b: TSIMDInstruction = (name: 'PUNPCKLDQ'; OperandsCount: 2; Operands: (p1:GREGdq ;p2:MODdq); InstrSet: isSSE2);

// 0x63
  simd_PACKSSWB_a: TSIMDInstruction = (name: 'PACKSSWB'; OperandsCount: 2; Operands: (p1:GREGq ;p2:MODq); InstrSet: isMMX);
  simd_PACKSSWB_b: TSIMDInstruction = (name: 'PACKSSWB'; OperandsCount: 2; Operands: (p1:GREGdq ;p2:MODdq); InstrSet: isSSE2);

// 0x64
  simd_PCMPGTB_a: TSIMDInstruction = (name: 'PCMPGTB'; OperandsCount: 2; Operands: (p1:GREGq ;p2:MODq); InstrSet: isMMX);
  simd_PCMPGTB_b: TSIMDInstruction = (name: 'PCMPGTB'; OperandsCount: 2; Operands: (p1:GREGdq ;p2:MODdq); InstrSet: isSSE2);

// 0x65
  simd_PCMPGTW_a: TSIMDInstruction = (name: 'PCMPGTW'; OperandsCount: 2; Operands: (p1:GREGq ;p2:MODq); InstrSet: isMMX);
  simd_PCMPGTW_b: TSIMDInstruction = (name: 'PCMPGTW'; OperandsCount: 2; Operands: (p1:GREGdq ;p2:MODdq); InstrSet: isSSE2);

// 0x66
  simd_PCMPGTD_a: TSIMDInstruction = (name: 'PCMPGTD'; OperandsCount: 2; Operands: (p1:GREGq ;p2:MODq); InstrSet: isMMX);
  simd_PCMPGTD_b: TSIMDInstruction = (name: 'PCMPGTD'; OperandsCount: 2; Operands: (p1:GREGdq ;p2:MODdq); InstrSet: isSSE2);

// 0x67
  simd_PACKUSWB_a: TSIMDInstruction = (name: 'PACKUSWB'; OperandsCount: 2; Operands: (p1:GREGq; p2:MODq); InstrSet: isMMX);
  simd_PACKUSWB_b: TSIMDInstruction = (name: 'PACKUSWB'; OperandsCount: 2; Operands: (p1:GREGdq; p2:MODdq); InstrSet: isSSE2);

// 0x68
  simd_PUNPCKHBW_a: TSIMDInstruction = (name: 'PUNPCKHBW'; OperandsCount: 2; Operands: (p1:GREGq ;p2:MODq); InstrSet: isMMX);
  simd_PUNPCKHBW_b: TSIMDInstruction = (name: 'PUNPCKHBW'; OperandsCount: 2; Operands: (p1:GREGdq ;p2:MODdq); InstrSet: isSSE2);

// 0x69
  simd_PUNPCKHWD_a: TSIMDInstruction = (name: 'PUNPCKHWD'; OperandsCount: 2; Operands: (p1:GREGq ;p2:MODq); InstrSet: isMMX);
  simd_PUNPCKHWD_b: TSIMDInstruction = (name: 'PUNPCKHWD'; OperandsCount: 2; Operands: (p1:GREGdq ;p2:MODdq); InstrSet: isSSE2);

// 0x6A
  simd_PUNPCKHDQ_a: TSIMDInstruction = (name: 'PUNPCKHDQ'; OperandsCount: 2; Operands: (p1:GREGq ;p2:MODq); InstrSet: isMMX);
  simd_PUNPCKHDQ_b: TSIMDInstruction = (name: 'PUNPCKHDQ'; OperandsCount: 2; Operands: (p1:GREGdq ;p2:MODdq); InstrSet: isSSE2);

// 0x6B
  simd_PACKSSDW_a: TSIMDInstruction = (name: 'PACKSSDW'; OperandsCount: 2; Operands: (p1:GREGq ;p2:MODq); InstrSet: isMMX);
  simd_PACKSSDW_b: TSIMDInstruction = (name: 'PACKSSDW'; OperandsCount: 2; Operands: (p1:GREGdq ;p2:MODdq); InstrSet: isSSE2);

// 0x6C
  simd_PUNPCKLQDQ: TSIMDInstruction = (name: 'PUNPCKLQDQ'; OperandsCount: 2; Operands: (p1:GREGdq ;p2:MODdq); InstrSet: isSSE2);

// 0x6D
  simd_PUNPCKHQDQ: TSIMDInstruction = (name: 'PUNPCKHQDQ'; OperandsCount: 2; Operands: (p1:GREGdq ;p2:MODdq); InstrSet: isSSE2);

// 0x6E
  simd_MOVD_a: TSIMDInstruction = (name: 'MOVD'; OperandsCount: 2; Operands: (p1:GREGq ;p2:MODd); InstrSet: isMMX);
  simd_MOVD_c: TSIMDInstruction = (name: 'MOVD'; OperandsCount: 2; Operands: (p1:GREGdq ;p2:MODd); InstrSet: isSSE2);

// 0x6F
  simd_MOVQ_a: TSIMDInstruction = (name: 'MOVQ'; OperandsCount: 2; Operands: (p1:GREGq ;p2:MODq); InstrSet: isMMX);
  simd_MOVDQA_a: TSIMDInstruction = (name: 'MOVDQA'; OperandsCount: 2; Operands: (p1:GREGdq ;p2:MODdq); InstrSet: isSSE2);
  simd_MOVDQU_a: TSIMDInstruction = (name: 'MOVDQU'; OperandsCount: 2; Operands: (p1:GREGdq ;p2:MODdq); InstrSet: isSSE2);

// 0x70
  simd_PSHUFW: TSIMDInstruction = (name: 'PSHUFW'; OperandsCount: 3; Operands: (p1:GREGq ;p2:MODq; p3:IMMb); InstrSet: isMMX);
  simd_PSHUFD: TSIMDInstruction = (name: 'PSHUFD'; OperandsCount: 3; Operands: (p1:GREGdq ;p2:MODdq; p3:IMMb); InstrSet: isSSE2);
  simd_PSHUFHW: TSIMDInstruction = (name: 'PSHUFHW'; OperandsCount: 3; Operands: (p1:GREGdq ;p2:MODdq; p3:IMMb); InstrSet: isSSE2);
  simd_PSHUFLW: TSIMDInstruction = (name: 'PSHUFLW'; OperandsCount: 3; Operands: (p1:GREGdq ;p2:MODdq; p3:IMMb); InstrSet: isSSE2);

// 0x74
  simd_PCMPEQB_a: TSIMDInstruction = (name: 'PCMPEQB'; OperandsCount: 2; Operands: (p1:GREGq ;p2:MODq); InstrSet: isMMX);
  simd_PCMPEQB_b: TSIMDInstruction = (name: 'PCMPEQB'; OperandsCount: 2; Operands: (p1:GREGdq ;p2:MODdq); InstrSet: isSSE2);

// 0x75
  simd_PCMPEQW_a: TSIMDInstruction = (name: 'PCMPEQW'; OperandsCount: 2; Operands: (p1:GREGq ;p2:MODq); InstrSet: isMMX);
  simd_PCMPEQW_b: TSIMDInstruction = (name: 'PCMPEQW'; OperandsCount: 2; Operands: (p1:GREGdq ;p2:MODdq); InstrSet: isSSE2);

// 0x76
  simd_PCMPEQD_a: TSIMDInstruction = (name: 'PCMPEQD'; OperandsCount: 2; Operands: (p1:GREGq ;p2:MODq); InstrSet: isMMX);
  simd_PCMPEQD_b: TSIMDInstruction = (name: 'PCMPEQD'; OperandsCount: 2; Operands: (p1:GREGdq ;p2:MODdq); InstrSet: isSSE2);

// 0x7C
  simd_HADDPS: TSIMDInstruction = (name: 'HADDPS'; OperandsCount: 2; Operands: (p1: GREGdq; p2:MODdq); InstrSet: isSSE3);
  simd_HADDPD: TSIMDInstruction = (name: 'HADDPD'; OperandsCount: 2; Operands: (p1: GREGdq; p2:MODdq); InstrSet: isSSE3);

// 0x7D
  simd_HSUBPS: TSIMDInstruction = (name: 'HADDPS'; OperandsCount: 2; Operands: (p1: GREGdq; p2:MODdq); InstrSet: isSSE3);
  simd_HSUBPD: TSIMDInstruction = (name: 'HADDPD'; OperandsCount: 2; Operands: (p1: GREGdq; p2:MODdq); InstrSet: isSSE3);

// 0x7E
  simd_MOVD_b: TSIMDInstruction = (name: 'MOVD'; OperandsCount: 2; Operands: (p1:MODd ;p2:GREGq); InstrSet: isMMX);
  simd_MOVD_d: TSIMDInstruction = (name: 'MOVD'; OperandsCount: 2; Operands: (p1:MODd ;p2:GREGdq); InstrSet: isSSE2);
  simd_MOVQ_c: TSIMDInstruction = (name: 'MOVQ'; OperandsCount: 2; Operands: (p1:GREGdq ;p2:XMM_M64); InstrSet: isSSE2);

// 0x7F
  simd_MOVQ_b: TSIMDInstruction = (name: 'MOVQ'; OperandsCount: 2; Operands: (p1:MODq ;p2:GREGq); InstrSet: isMMX);
  simd_MOVDQA_b: TSIMDInstruction = (name: 'MOVDQA'; OperandsCount: 2; Operands: (p1:MODdq ;p2:GREGdq); InstrSet: isSSE2);
  simd_MOVDQU_b: TSIMDInstruction = (name: 'MOVDQU'; OperandsCount: 2; Operands: (p1:MODdq ;p2:GREGdq); InstrSet: isSSE2);

// 0xB8
  simd_POPCNT: TSIMDInstruction = (name: 'POPCNT'; OperandsCount: 2; Operands: (p1:GREGv ;p2:MODv); InstrSet: isSSE42);

// 0xC2
  simd_CMPPS: TSIMDInstruction = (name: 'CMPPS'; OperandsCount: 3; Operands: (p1:GREGdq; p2:MODdq; p3:IMMb); InstrSet: isSSE);
  simd_CMPSS: TSIMDInstruction = (name: 'CMPSS'; OperandsCount: 3; Operands: (p1:GREGdq; p2:XMM_M32; p3:IMMb); InstrSet: isSSE);
  simd_CMPPD: TSIMDInstruction = (name: 'CMPPD'; OperandsCount: 3; Operands: (p1:GREGdq; p2:MODdq; p3:IMMb); InstrSet: isSSE2);
  simd_CMPSD: TSIMDInstruction = (name: 'CMPSD'; OperandsCount: 3; Operands: (p1:GREGdq; p2:XMM_M64; p3:IMMb); InstrSet: isSSE2);

// 0xC3
  simd_MOVNTI: TSIMDInstruction = (name: 'MOVNTI'; OperandsCount: 2; Operands: (p1:M32; p2:GREGd); InstrSet: isSSE2);

// 0xC4
  simd_PINSRW_a: TSIMDInstruction = (name: 'PINSRW'; OperandsCount: 3; Operands: (p1:GREGq; p2:REG32_M16; p3:IMMb); InstrSet: isSSE);
  simd_PINSRW_b: TSIMDInstruction = (name: 'PINSRW'; OperandsCount: 3; Operands: (p1:GREGdq; p2:REG32_M16; p3:IMMb); InstrSet: isSSE2);

// 0xC5
  simd_PEXTRW_a: TSIMDInstruction = (name: 'PEXTRW'; OperandsCount: 3; Operands: (p1:GREGd; p2:R64; p3:IMMb); InstrSet: isSSE);     // Vzdy prisluchajuce registre !!!
  simd_PEXTRW_b: TSIMDInstruction = (name: 'PEXTRW'; OperandsCount: 3; Operands: (p1:GREGd; p2:R128; p3:IMMb); InstrSet: isSSE2);    // Vzdy prisluchajuce registre !!!

// 0xC6
  simd_SHUFPS: TSIMDInstruction = (name: 'SHUFPS'; OperandsCount: 3; Operands: (p1:GREGdq; p2:MODdq; p3:IMMb); InstrSet: isSSE);
  simd_SHUFPD: TSIMDInstruction = (name: 'SHUFPD'; OperandsCount: 3; Operands: (p1:GREGdq; p2:MODdq; p3:IMMb); InstrSet: isSSE2);

// 0xD0
  simd_ADDSUBPS: TSIMDInstruction = (name: 'ADDSUBPS'; OperandsCount: 2; Operands: (p1:GREGdq; p2:MODdq); InstrSet: isSSE3);
  simd_ADDSUBPD: TSIMDInstruction = (name: 'ADDSUBPD'; OperandsCount: 2; Operands: (p1:GREGdq; p2:MODdq); InstrSet: isSSE3);

// 0xD1
  simd_PSRLW_a: TSIMDInstruction = (name: 'PSRLW'; opcode:$D1 ; OperandsCount: 2; Operands: (p1:GREGq ;p2:MODq); InstrSet: isMMX);
  simd_PSRLW_b: TSIMDInstruction = (name: 'PSRLW'; OperandsCount: 2; Operands: (p1:GREGdq ;p2:MODdq); InstrSet: isSSE2);

// 0xD2
  simd_PSRLD_a: TSIMDInstruction = (name: 'PSRLD'; opcode:$D2 ; OperandsCount: 2; Operands: (p1:GREGq ;p2:MODq); InstrSet: isMMX);
  simd_PSRLD_b: TSIMDInstruction = (name: 'PSRLD'; OperandsCount: 2; Operands: (p1:GREGdq ;p2:MODdq); InstrSet: isSSE2);

// 0xD3
  simd_PSRLQ_a: TSIMDInstruction = (name: 'PSRLQ'; opcode:$D3 ; OperandsCount: 2; Operands: (p1:GREGq ;p2:MODq); InstrSet: isMMX);
  simd_PSRLQ_b: TSIMDInstruction = (name: 'PSRLQ'; OperandsCount: 2; Operands: (p1:GREGdq ;p2:MODdq); InstrSet: isSSE2);

// 0xD4
  simd_PADDQ_a: TSIMDInstruction = (name: 'PADDQ'; OperandsCount: 2; Operands: (p1:GREGq ;p2:MODq); InstrSet: isSSE2);
  simd_PADDQ_b: TSIMDInstruction = (name: 'PADDQ'; OperandsCount: 2; Operands: (p1:GREGdq ;p2:MODdq); InstrSet: isSSE2);

// 0xD5
  simd_PMULLW_a: TSIMDInstruction = (name: 'PMULLW'; OperandsCount: 2; Operands: (p1:GREGq ;p2:MODq); InstrSet: isMMX);
  simd_PMULLW_b: TSIMDInstruction = (name: 'PMULLW'; OperandsCount: 2; Operands: (p1:GREGdq ;p2:MODdq); InstrSet: isSSE2);

// 0xD6
  simd_MOVQ_d: TSIMDInstruction = (name: 'MOVQ'; OperandsCount: 2; Operands: (p1:XMM_M64; p2:GREGdq); InstrSet: isSSE2);
  simd_MOVQ2DQ: TSIMDInstruction = (name: 'MOVQ2DQ'; OperandsCount: 2; Operands: (p1:GREGdq ;p2:R64); InstrSet: isSSE2);
  simd_MOVDQ2Q: TSIMDInstruction = (name: 'MOVDQ2Q'; OperandsCount: 2; Operands: (p1:GREGq; p2:R128); InstrSet: isSSE2);

// 0xD7
  simd_PMOVMSKB_a: TSIMDInstruction = (name: 'PMOVMSKB'; OperandsCount: 2; Operands: (p1:GREGd ;p2:R64); InstrSet: isSSE);     // Vzdy prisluchajuce registre !!!
  simd_PMOVMSKB_b: TSIMDInstruction = (name: 'PMOVMSKB'; OperandsCount: 2; Operands: (p1:GREGd ;p2:R128); InstrSet: isSSE2);    // Vzdy prisluchajuce registre !!!

// 0xD8
  simd_PSUBUSB_a: TSIMDInstruction = (name: 'PSUBUSB'; OperandsCount: 2; Operands: (p1:GREGq ;p2:MODq); InstrSet: isMMX);
  simd_PSUBUSB_b: TSIMDInstruction = (name: 'PSUBUSB'; OperandsCount: 2; Operands: (p1:GREGdq ;p2:MODdq); InstrSet: isSSE2);

// 0xD9
  simd_PSUBUSW_a: TSIMDInstruction = (name: 'PSUBUSW'; OperandsCount: 2; Operands: (p1:GREGq ;p2:MODq); InstrSet: isMMX);
  simd_PSUBUSW_b: TSIMDInstruction = (name: 'PSUBUSW'; OperandsCount: 2; Operands: (p1:GREGdq ;p2:MODdq); InstrSet: isSSE2);

// 0xDA
  simd_PMINUB_a: TSIMDInstruction = (name: 'PMINUB'; OperandsCount: 2; Operands: (p1:GREGq ;p2:MODq); InstrSet: isSSE);
  simd_PMINUB_b: TSIMDInstruction = (name: 'PMINUB'; OperandsCount: 2; Operands: (p1:GREGdq ;p2:MODdq); InstrSet: isSSE2);

// 0xDB
  simd_PAND_a: TSIMDInstruction = (name: 'PAND'; OperandsCount: 2; Operands: (p1:GREGq ;p2:MODq); InstrSet: isMMX);
  simd_PAND_b: TSIMDInstruction = (name: 'PAND'; OperandsCount: 2; Operands: (p1:GREGdq ;p2:MODdq); InstrSet: isSSE2);

// 0xDC
  simd_PADDUSB_a: TSIMDInstruction = (name: 'PADDUSB'; OperandsCount: 2; Operands: (p1:GREGq ;p2:MODq); InstrSet: isMMX);
  simd_PADDUSB_b: TSIMDInstruction = (name: 'PADDUSB'; OperandsCount: 2; Operands: (p1:GREGdq ;p2:MODdq); InstrSet: isSSE2);

// 0xDD
  simd_PADDUSW_a: TSIMDInstruction = (name: 'PADDUSW'; OperandsCount: 2; Operands: (p1:GREGq ;p2:MODq); InstrSet: isMMX);
  simd_PADDUSW_b: TSIMDInstruction = (name: 'PADDUSW'; OperandsCount: 2; Operands: (p1:GREGdq ;p2:MODdq); InstrSet: isSSE2);

// 0xDE
  simd_PMAXUB_a: TSIMDInstruction = (name: 'PMAXUB'; OperandsCount: 2; Operands: (p1:GREGq ;p2:MODq); InstrSet: isSSE);
  simd_PMAXUB_b: TSIMDInstruction = (name: 'PMAXUB'; OperandsCount: 2; Operands: (p1:GREGdq ;p2:MODdq); InstrSet: isSSE2);

// 0xDF
  simd_PANDN_a: TSIMDInstruction = (name: 'PANDN'; OperandsCount: 2; Operands: (p1:GREGq ;p2:MODq); InstrSet: isMMX);
  simd_PANDN_b: TSIMDInstruction = (name: 'PANDN'; OperandsCount: 2; Operands: (p1:GREGdq ;p2:MODdq); InstrSet: isSSE2);



//  simd_: TSIMDInstruction = (name: ''; OperandsCount: ; Operands: (p1:; p2:); InstrSet: is);


// 0xE
  simd_PAVGB_a: TSIMDInstruction = (name: 'PAVGB'; OperandsCount: 2; Operands: (p1:GREGq ;p2:MODq); InstrSet: isSSE);
  simd_PAVGB_b: TSIMDInstruction = (name: 'PAVGB'; OperandsCount: 2; Operands: (p1:GREGdq ;p2:MODdq); InstrSet: isSSE2);

// 0xE1
  simd_PSRAW_a: TSIMDInstruction = (name: 'PSRAW'; opcode:$E1 ; OperandsCount: 2; Operands: (p1:GREGq ;p2:MODq); InstrSet: isMMX);
  simd_PSRAW_b: TSIMDInstruction = (name: 'PSRAW'; OperandsCount: 2; Operands: (p1:GREGdq ;p2:MODdq); InstrSet: isSSE2);

// 0xE2
  simd_PSRAD_a: TSIMDInstruction = (name: 'PSRAD'; opcode:$E2 ; OperandsCount: 2; Operands: (p1:GREGq ;p2:MODq); InstrSet: isMMX);
  simd_PSRAD_b: TSIMDInstruction = (name: 'PSRAD'; OperandsCount: 2; Operands: (p1:GREGdq ;p2:MODdq); InstrSet: isSSE2);

// 0xE3
  simd_PAVGW_a: TSIMDInstruction = (name: 'PAVGW'; OperandsCount: 2; Operands: (p1:GREGq ;p2:MODq); InstrSet: isSSE);
  simd_PAVGW_b: TSIMDInstruction = (name: 'PAVGW'; OperandsCount: 2; Operands: (p1:GREGdq ;p2:MODdq); InstrSet: isSSE2);

// 0xE4
  simd_PMULHUW_a: TSIMDInstruction = (name: 'PMULHUW'; OperandsCount: 2; Operands: (p1:GREGq ;p2:MODq); InstrSet: isSSE);
  simd_PMULHUW_b: TSIMDInstruction = (name: 'PMULHUW'; OperandsCount: 2; Operands: (p1:GREGdq ;p2:MODdq); InstrSet: isSSE2);

// 0xE5
  simd_PMULHW_a: TSIMDInstruction = (name: 'PMULHW'; OperandsCount: 2; Operands: (p1:GREGq ;p2:MODq); InstrSet: isMMX);
  simd_PMULHW_b: TSIMDInstruction = (name: 'PMULHW'; OperandsCount: 2; Operands: (p1:GREGdq ;p2:MODdq); InstrSet: isSSE2);

// 0xE6
  simd_CVTPD2DQ: TSIMDInstruction = (name: 'CVTPD2DQ'; OperandsCount: 2; Operands: (p1:GREGdq ;p2:MODdq); InstrSet: isSSE2);
  simd_CVTTPD2DQ: TSIMDInstruction = (name: 'CVTTPD2DQ'; OperandsCount: 2; Operands: (p1:GREGdq ;p2:MODdq); InstrSet: isSSE2);
  simd_CVTDQ2PD: TSIMDInstruction = (name: 'CVTDQ2PD'; OperandsCount: 2; Operands: (p1:GREGdq ;p2:XMM_M64); InstrSet: isSSE2);

// 0xE7
  simd_MOVNTQ: TSIMDInstruction = (name: 'MOVNTQ'; OperandsCount: 2; Operands: (p1:M64 ;p2:GREGq); InstrSet: isSSE);
  simd_MOVNTDQ: TSIMDInstruction = (name: 'MOVNTDQ'; OperandsCount: 2; Operands: (p1:M128 ;p2:GREGdq); InstrSet: isSSE2);

// 0xE8
  simd_PSUBSB_a: TSIMDInstruction = (name: 'PSUBSB'; OperandsCount: 2; Operands: (p1:GREGq ;p2:MODq); InstrSet: isMMX);
  simd_PSUBSB_b: TSIMDInstruction = (name: 'PSUBSB'; OperandsCount: 2; Operands: (p1:GREGdq ;p2:MODdq); InstrSet: isSSE2);

// 0xE9
  simd_PSUBSW_a: TSIMDInstruction = (name: 'PSUBSW'; OperandsCount: 2; Operands: (p1:GREGq ;p2:MODq); InstrSet: isMMX);
  simd_PSUBSW_b: TSIMDInstruction = (name: 'PSUBSW'; OperandsCount: 2; Operands: (p1:GREGdq ;p2:MODdq); InstrSet: isSSE2);

// 0xEA
  simd_PMINSW_a: TSIMDInstruction = (name: 'PMINSW'; OperandsCount: 2; Operands: (p1:GREGq ;p2:MODq); InstrSet: isSSE);
  simd_PMINSW_b: TSIMDInstruction = (name: 'PMINSW'; OperandsCount: 2; Operands: (p1:GREGdq ;p2:MODdq); InstrSet: isSSE2);

// 0xEB
  simd_POR_a: TSIMDInstruction = (name: 'POR'; OperandsCount: 2; Operands: (p1:GREGq ;p2:MODq); InstrSet: isMMX);
  simd_POR_b: TSIMDInstruction = (name: 'POR'; OperandsCount: 2; Operands: (p1:GREGdq ;p2:MODdq); InstrSet: isSSE2);

// 0xEC
  simd_PADDSB_a: TSIMDInstruction = (name: 'PADDSB'; OperandsCount: 2; Operands: (p1:GREGq ;p2:MODq); InstrSet: isMMX);
  simd_PADDSB_b: TSIMDInstruction = (name: 'PADDSB'; OperandsCount: 2; Operands: (p1:GREGdq ;p2:MODdq); InstrSet: isSSE2);

// 0xED
  simd_PADDSW_a: TSIMDInstruction = (name: 'PADDSW'; OperandsCount: 2; Operands: (p1:GREGq ;p2:MODq); InstrSet: isMMX);
  simd_PADDSW_b: TSIMDInstruction = (name: 'PADDSW'; OperandsCount: 2; Operands: (p1:GREGdq ;p2:MODdq); InstrSet: isSSE2);

// 0xEE
  simd_PMAXSW_a: TSIMDInstruction = (name: 'PMAXSW'; OperandsCount: 2; Operands: (p1:GREGq ;p2:MODq); InstrSet: isSSE);
  simd_PMAXSW_b: TSIMDInstruction = (name: 'PMAXSW'; OperandsCount: 2; Operands: (p1:GREGdq ;p2:MODdq); InstrSet: isSSE2);

// 0xEF
  simd_PXOR_a: TSIMDInstruction = (name: 'PXOR'; OperandsCount: 2; Operands: (p1:GREGq ;p2:MODq); InstrSet: isMMX);
  simd_PXOR_b: TSIMDInstruction = (name: 'PXOR'; OperandsCount: 2; Operands: (p1:GREGdq ;p2:MODdq); InstrSet: isSSE2);

// 0xF0
  simd_LDDQU: TSIMDInstruction = (name: 'LDDQU'; OperandsCount: 2; Operands: (p1:GREGdq ;p2: M128); InstrSet: isSSE3);

// 0xF1
  simd_PSLLW_a: TSIMDInstruction = (name: 'PSLLW'; opcode:$F1 ; OperandsCount: 2; Operands: (p1:GREGq ;p2:MODq); InstrSet: isMMX);
  simd_PSLLW_b: TSIMDInstruction = (name: 'PSLLW'; OperandsCount: 2; Operands: (p1:GREGdq ;p2:MODdq); InstrSet: isSSE2);

// 0xF2
  simd_PSLLD_a: TSIMDInstruction = (name: 'PSLLD'; opcode:$F2 ; OperandsCount: 2; Operands: (p1:GREGq ;p2:MODq); InstrSet: isMMX);
  simd_PSLLD_b: TSIMDInstruction = (name: 'PSLLD'; OperandsCount: 2; Operands: (p1:GREGdq ;p2:MODdq); InstrSet: isSSE2);

// 0xF3
  simd_PSLLQ_a: TSIMDInstruction = (name: 'PSLLQ'; opcode:$F3 ; OperandsCount: 2; Operands: (p1:GREGq ;p2:MODq); InstrSet: isMMX);
  simd_PSLLQ_b: TSIMDInstruction = (name: 'PSLLQ'; OperandsCount: 2; Operands: (p1:GREGdq ;p2:MODdq); InstrSet: isSSE2);

// 0xF4
  simd_PMULUDQ_a: TSIMDInstruction = (name: 'PMULUDQ'; OperandsCount: 2; Operands: (p1:GREGq ;p2:MODq); InstrSet: isSSE2);
  simd_PMULUDQ_b: TSIMDInstruction = (name: 'PMULUDQ'; OperandsCount: 2; Operands: (p1:GREGdq ;p2:MODdq); InstrSet: isSSE2);

// 0xF5
  simd_PMADDWD_a: TSIMDInstruction = (name: 'PMADDWD'; OperandsCount: 2; Operands: (p1:GREGq ;p2:MODq); InstrSet: isMMX);
  simd_PMADDWD_b: TSIMDInstruction = (name: 'PMADDWD'; OperandsCount: 2; Operands: (p1:GREGdq ;p2:MODdq); InstrSet: isSSE2);

// 0xF6
  simd_PSADBW_a: TSIMDInstruction = (name: 'PSADBW'; OperandsCount: 2; Operands: (p1:GREGq ;p2:MODq); InstrSet: isSSE);
  simd_PSADBW_b: TSIMDInstruction = (name: 'PSADBW'; OperandsCount: 2; Operands: (p1:GREGdq ;p2:MODdq); InstrSet: isSSE2);

// 0xF7
  simd_MASKMOVQ: TSIMDInstruction = (name: 'MASKMOVQ'; OperandsCount: 2; Operands: (p1:GREGq; p2:R64); InstrSet: isSSE);
  simd_MASKMOVDQU: TSIMDInstruction = (name: 'MASKMOVDQU'; OperandsCount: 2; Operands: (p1:GREGdq ;p2:R128); InstrSet: isSSE2);

// 0xF8
  simd_PSUBB_a: TSIMDInstruction = (name: 'PSUBB'; OperandsCount: 2; Operands: (p1:GREGq ;p2:MODq); InstrSet: isMMX);
  simd_PSUBB_b: TSIMDInstruction = (name: 'PSUBB'; OperandsCount: 2; Operands: (p1:GREGdq ;p2:MODdq); InstrSet: isSSE2);

// 0xF9
  simd_PSUBW_a: TSIMDInstruction = (name: 'PSUBW'; OperandsCount: 2; Operands: (p1:GREGq ;p2:MODq); InstrSet: isMMX);
  simd_PSUBW_b: TSIMDInstruction = (name: 'PSUBW'; OperandsCount: 2; Operands: (p1:GREGdq ;p2:MODdq); InstrSet: isSSE2);

// 0xFA
  simd_PSUBD_a: TSIMDInstruction = (name: 'PSUBD'; OperandsCount: 2; Operands: (p1:GREGq ;p2:MODq); InstrSet: isMMX);
  simd_PSUBD_b: TSIMDInstruction = (name: 'PSUBD'; OperandsCount: 2; Operands: (p1:GREGdq ;p2:MODdq); InstrSet: isSSE2);

// 0xFB
  simd_PSUBQ_a: TSIMDInstruction = (name: 'PSUBQ'; OperandsCount: 2; Operands: (p1:GREGq ;p2:MODq); InstrSet: isSSE2);
  simd_PSUBQ_b: TSIMDInstruction = (name: 'PSUBQ'; OperandsCount: 2; Operands: (p1:GREGdq ;p2:MODdq); InstrSet: isSSE2);

// 0xFC
  simd_PADDB_a: TSIMDInstruction = (name: 'PADDB'; OperandsCount: 2; Operands: (p1:GREGq; p2:MODq); InstrSet: isMMX);
  simd_PADDB_b: TSIMDInstruction = (name: 'PADDB'; OperandsCount: 2; Operands: (p1:GREGdq ;p2:MODdq); InstrSet: isSSE2);

// 0xFD
  simd_PADDW_a: TSIMDInstruction = (name: 'PADDW'; OperandsCount: 2; Operands: (p1:GREGq ;p2:MODq); InstrSet: isMMX);
  simd_PADDW_b: TSIMDInstruction = (name: 'PADDW'; OperandsCount: 2; Operands: (p1:GREGdq ;p2:MODdq); InstrSet: isSSE2);

// 0xFE
  simd_PADDD_a: TSIMDInstruction = (name: 'PADDD'; OperandsCount: 2; Operands: (p1:GREGq ;p2:MODq); InstrSet: isMMX);
  simd_PADDD_b: TSIMDInstruction = (name: 'PADDD'; OperandsCount: 2; Operands: (p1:GREGdq ;p2:MODdq); InstrSet: isSSE2);


//***********************************************************************
//  Opcode (66, F2, F3) 0F 38 xx
//***********************************************************************


// 0x00
  simd_PSHUFB_a: TSIMDInstruction = (name: 'PSHUFB'; OperandsCount: 2; Operands: (p1:GREGq ;p2:MODq); InstrSet: isSSSE3);
  simd_PSHUFB_b: TSIMDInstruction = (name: 'PSHUFB'; OperandsCount: 2; Operands: (p1:GREGdq ;p2:MODdq); InstrSet: isSSSE3);

// 0x01
  simd_PHADDW_a: TSIMDInstruction = (name: 'PHADDW'; OperandsCount: 2; Operands: (p1:GREGq ;p2:MODq); InstrSet: isSSSE3);
  simd_PHADDW_b: TSIMDInstruction = (name: 'PHADDW'; OperandsCount: 2; Operands: (p1:GREGdq ;p2:MODdq); InstrSet: isSSSE3);

// 0x02
  simd_PHADDD_a: TSIMDInstruction = (name: 'PHADDD'; OperandsCount: 2; Operands: (p1:GREGq ;p2:MODq); InstrSet: isSSSE3);
  simd_PHADDD_b: TSIMDInstruction = (name: 'PHADDD'; OperandsCount: 2; Operands: (p1:GREGdq ;p2:MODdq); InstrSet: isSSSE3);

// 0x03
  simd_PHADDSW_a: TSIMDInstruction = (name: 'PHADDSW'; OperandsCount: 2; Operands: (p1:GREGq ;p2:MODq); InstrSet: isSSSE3);
  simd_PHADDSW_b: TSIMDInstruction = (name: 'PHADDSW'; OperandsCount: 2; Operands: (p1:GREGdq ;p2:MODdq); InstrSet: isSSSE3);

// 0x04
  simd_PMADDUBSW_a: TSIMDInstruction = (name: 'PMADDUBSW'; OperandsCount: 2; Operands: (p1:GREGq ;p2:MODq); InstrSet: isSSSE3);
  simd_PMADDUBSW_b: TSIMDInstruction = (name: 'PMADDUBSW'; OperandsCount: 2; Operands: (p1:GREGdq ;p2:MODdq); InstrSet: isSSSE3);

// 0x05
  simd_PHSUBW_a: TSIMDInstruction = (name: 'PHSUBW'; OperandsCount: 2; Operands: (p1:GREGq ;p2:MODq); InstrSet: isSSSE3);
  simd_PHSUBW_b: TSIMDInstruction = (name: 'PHSUBW'; OperandsCount: 2; Operands: (p1:GREGdq ;p2:MODdq); InstrSet: isSSSE3);

// 0x06
  simd_PHSUBD_a: TSIMDInstruction = (name: 'PHSUBD'; OperandsCount: 2; Operands: (p1:GREGq ;p2:MODq); InstrSet: isSSSE3);
  simd_PHSUBD_b: TSIMDInstruction = (name: 'PHSUBD'; OperandsCount: 2; Operands: (p1:GREGdq ;p2:MODdq); InstrSet: isSSSE3);

// 0x07
  simd_PHSUBSW_a: TSIMDInstruction = (name: 'PHSUBSW'; OperandsCount: 2; Operands: (p1:GREGq ;p2:MODq); InstrSet: isSSSE3);
  simd_PHSUBSW_b: TSIMDInstruction = (name: 'PHSUBSW'; OperandsCount: 2; Operands: (p1:GREGdq ;p2:MODdq); InstrSet: isSSSE3);

// 0x08
  simd_PSIGNB_a: TSIMDInstruction = (name: 'PSIGNB'; OperandsCount: 2; Operands: (p1:GREGq ;p2:MODq); InstrSet: isSSSE3);
  simd_PSIGNB_b: TSIMDInstruction = (name: 'PSIGNB'; OperandsCount: 2; Operands: (p1:GREGdq ;p2:MODdq); InstrSet: isSSSE3);

// 0x09
  simd_PSIGNW_a: TSIMDInstruction = (name: 'PSIGNW'; OperandsCount: 2; Operands: (p1:GREGq ;p2:MODq); InstrSet: isSSSE3);
  simd_PSIGNW_b: TSIMDInstruction = (name: 'PSIGNW'; OperandsCount: 2; Operands: (p1:GREGdq ;p2:MODdq); InstrSet: isSSSE3);

// 0x0A
  simd_PSIGND_a: TSIMDInstruction = (name: 'PSIGND'; OperandsCount: 2; Operands: (p1:GREGq ;p2:MODq); InstrSet: isSSSE3);
  simd_PSIGND_b: TSIMDInstruction = (name: 'PSIGND'; OperandsCount: 2; Operands: (p1:GREGdq ;p2:MODdq); InstrSet: isSSSE3);

// 0x0B
  simd_PMULHRSW_a: TSIMDInstruction = (name: 'PMULHRSW'; OperandsCount: 2; Operands: (p1:GREGq ;p2:MODq); InstrSet: isSSSE3);
  simd_PMULHRSW_b: TSIMDInstruction = (name: 'PMULHRSW'; OperandsCount: 2; Operands: (p1:GREGdq ;p2:MODdq); InstrSet: isSSSE3);

// 0x10
  simd_PBLENDVB: TSIMDInstruction = (name: 'PBLENDVB'; OperandsCount: 2; Operands: (p1:GREGdq ;p2:MODdq); InstrSet: isSSSE3);

// 0x14
  simd_BLENDVPS: TSIMDInstruction = (name: 'BLENDVPS'; OperandsCount: 2; Operands: (p1:GREGdq ;p2:MODdq); InstrSet: isSSSE3);

// 0x15
  simd_BLENDVPD: TSIMDInstruction = (name: 'BLENDVPD'; OperandsCount: 2; Operands: (p1:GREGdq ;p2:MODdq); InstrSet: isSSSE3);

// 0x17
  simd_PTEST: TSIMDInstruction = (name: 'PTEST'; OperandsCount: 2; Operands: (p1:GREGdq ;p2:MODdq); InstrSet: isSSSE3);

// 0x1C
  simd_PABSB_a: TSIMDInstruction = (name: 'PABSB'; OperandsCount: 2; Operands: (p1:GREGq ;p2:MODq); InstrSet: isSSSE3);
  simd_PABSB_b: TSIMDInstruction = (name: 'PABSB'; OperandsCount: 2; Operands: (p1:GREGdq ;p2:MODdq); InstrSet: isSSSE3);

// 0x1D
  simd_PABSW_a: TSIMDInstruction = (name: 'PABSW'; OperandsCount: 2; Operands: (p1:GREGq ;p2:MODq); InstrSet: isSSSE3);
  simd_PABSW_b: TSIMDInstruction = (name: 'PABSW'; OperandsCount: 2; Operands: (p1:GREGdq ;p2:MODdq); InstrSet: isSSSE3);

// 0x1E
  simd_PABSD_a: TSIMDInstruction = (name: 'PABSD'; OperandsCount: 2; Operands: (p1:GREGq ;p2:MODq); InstrSet: isSSSE3);
  simd_PABSD_b: TSIMDInstruction = (name: 'PABSD'; OperandsCount: 2; Operands: (p1:GREGdq ;p2:MODdq); InstrSet: isSSSE3);

// 0x20
  simd_PMOVSXBW: TSIMDInstruction = (name: 'PMOVSXBW'; OperandsCount: 2; Operands: (p1: GREGdq; p2:XMM_M64); InstrSet: isSSE41);

// 0x21
  simd_PMOVSXBD: TSIMDInstruction = (name: 'PMOVSXBD'; OperandsCount: 2; Operands: (p1: GREGdq; p2:XMM_M32); InstrSet: isSSE41);

// 0x22
  simd_PMOVSXBQ: TSIMDInstruction = (name: 'PMOVSXBQ'; OperandsCount: 2; Operands: (p1: GREGdq; p2:XMM_M16); InstrSet: isSSE41);

// 0x23
  simd_PMOVSXWD: TSIMDInstruction = (name: 'PMOVSXWD'; OperandsCount: 2; Operands: (p1: GREGdq; p2:XMM_M64); InstrSet: isSSE41);

// 0x24
  simd_PMOVSXWQ: TSIMDInstruction = (name: 'PMOVSXWQ'; OperandsCount: 2; Operands: (p1: GREGdq; p2:XMM_M32); InstrSet: isSSE41);

// 0x25
  simd_PMOVSXDQ: TSIMDInstruction = (name: 'PMOVSXDQ'; OperandsCount: 2; Operands: (p1: GREGdq; p2:XMM_M64); InstrSet: isSSE41);

// 0x28
  simd_PMULDQ: TSIMDInstruction = (name: 'PMULDQ'; OperandsCount: 2; Operands: (p1: GREGdq; p2: MODdq); InstrSet: isSSE41);

// 0x29
  simd_PCMPEQQ: TSIMDInstruction = (name: 'PCMPEQQ'; OperandsCount: 2; Operands: (p1: GREGdq; p2: MODdq); InstrSet: isSSE41);

// 0x2A
  simd_MOVNTDQA: TSIMDInstruction = (name: 'MOVNTDQA'; OperandsCount: 2; Operands: (p1: GREGdq; p2: MODdq); InstrSet: isSSE41);

// 0x2B
  simd_PACKUSDW: TSIMDInstruction = (name: 'PACKUSDW'; OperandsCount: 2; Operands: (p1: GREGdq; p2: MODdq); InstrSet: isSSE41);

// 0x30
  simd_PMOVZXBW: TSIMDInstruction = (name: 'PMOVZXBW'; OperandsCount: 2; Operands: (p1: GREGdq; p2:XMM_M64); InstrSet: isSSE41);

// 0x31
  simd_PMOVZXBD: TSIMDInstruction = (name: 'PMOVZXBD'; OperandsCount: 2; Operands: (p1: GREGdq; p2:XMM_M32); InstrSet: isSSE41);

// 0x32
  simd_PMOVZXBQ: TSIMDInstruction = (name: 'PMOVZXBQ'; OperandsCount: 2; Operands: (p1: GREGdq; p2:XMM_M16); InstrSet: isSSE41);

// 0x33
  simd_PMOVZXWD: TSIMDInstruction = (name: 'PMOVZXWD'; OperandsCount: 2; Operands: (p1: GREGdq; p2:XMM_M64); InstrSet: isSSE41);

// 0x34
  simd_PMOVZXWQ: TSIMDInstruction = (name: 'PMOVZXWQ'; OperandsCount: 2; Operands: (p1: GREGdq; p2:XMM_M32); InstrSet: isSSE41);

// 0x35
  simd_PMOVZXDQ: TSIMDInstruction = (name: 'PMOVZXDQ'; OperandsCount: 2; Operands: (p1: GREGdq; p2:XMM_M64); InstrSet: isSSE41);

// 0x37
  simd_PCMPGTQ: TSIMDInstruction = (name: 'PCMPGTQ'; OperandsCount: 2; Operands: (p1: GREGdq; p2: MODdq); InstrSet: isSSE42);

// 0x38
  simd_PMINSB: TSIMDInstruction = (name: 'PMINSB'; OperandsCount: 2; Operands: (p1: GREGdq; p2: MODdq); InstrSet: isSSE41);

// 0x39
  simd_PMINSD: TSIMDInstruction = (name: 'PMINSD'; OperandsCount: 2; Operands: (p1: GREGdq; p2: MODdq); InstrSet: isSSE41);

// 0x3A
  simd_PMINUW: TSIMDInstruction = (name: 'PMINUW'; OperandsCount: 2; Operands: (p1: GREGdq; p2: MODdq); InstrSet: isSSE41);

// 0x3B
  simd_PMINUD: TSIMDInstruction = (name: 'PMINUD'; OperandsCount: 2; Operands: (p1: GREGdq; p2: MODdq); InstrSet: isSSE41);

// 0x3C
  simd_PMAXSB: TSIMDInstruction = (name: 'PMAXSB'; OperandsCount: 2; Operands: (p1: GREGdq; p2: MODdq); InstrSet: isSSE41);

// 0x3D
  simd_PMAXSD: TSIMDInstruction = (name: 'PMAXSD'; OperandsCount: 2; Operands: (p1: GREGdq; p2: MODdq); InstrSet: isSSE41);

// 0x3E
  simd_PMAXUW: TSIMDInstruction = (name: 'PMAXUW'; OperandsCount: 2; Operands: (p1: GREGdq; p2: MODdq); InstrSet: isSSE41);

// 0x3F
  simd_PMAXUD: TSIMDInstruction = (name: 'PMAXUD'; OperandsCount: 2; Operands: (p1: GREGdq; p2: MODdq); InstrSet: isSSE41);

// 0x40
  simd_PMULLD: TSIMDInstruction = (name: 'PMULLD'; OperandsCount: 2; Operands: (p1: GREGdq; p2: MODdq); InstrSet: isSSE41);

// 0x41
  simd_PHMINPOSUW: TSIMDInstruction = (name: 'PHMINPOSUW'; OperandsCount: 2; Operands: (p1: GREGdq; p2: MODdq); InstrSet: isSSE41);

// 0xF0
  simd_CRC32_a: TSIMDInstruction = (name: 'CRC32'; OperandsCount: 2; Operands: (p1: GREGd; p2: MODb); InstrSet: isSSE42);

// 0xF1
  simd_CRC32_b: TSIMDInstruction = (name: 'CRC32'; OperandsCount: 2; Operands: (p1: GREGdq; p2: MODv); InstrSet: isSSE42);

//***********************************************************************
//  Opcode (66, F2, F3) 0F 3A xx
//***********************************************************************

//TODO
//  simd_: TSIMDInstruction = (name: ''; OperandsCount: 2; Operands: (p1: GREGdq; p2: MODv); InstrSet: isSS);


// 0x08
  simd_ROUNDPS: TSIMDInstruction = (name: 'ROUNDPS'; OperandsCount: 3; Operands: (p1: GREGdq; p2: MODdq; p3: IMMb); InstrSet: isSSE41);

// 0x09
  simd_ROUNDPD: TSIMDInstruction = (name: 'ROUNDPD'; OperandsCount: 3; Operands: (p1: GREGdq; p2: MODdq; p3: IMMb); InstrSet: isSSE41);

// 0x0A
  simd_ROUNDSS: TSIMDInstruction = (name: 'ROUNDSS'; OperandsCount: 3; Operands: (p1: GREGdq; p2: XMM_M32; p3: IMMb); InstrSet: isSSE41);

// 0x0B
  simd_ROUNDSD: TSIMDInstruction = (name: 'ROUNDSD'; OperandsCount: 3; Operands: (p1: GREGdq; p2: XMM_M64; p3: IMMb); InstrSet: isSSE41);

// 0x0C
  simd_BLENDPS: TSIMDInstruction = (name: 'BLENDPS'; OperandsCount: 3; Operands: (p1: GREGdq; p2: MODdq; p3: IMMb); InstrSet: isSSE41);

// 0x0D
  simd_BLENDPD: TSIMDInstruction = (name: 'BLENDPD'; OperandsCount: 3; Operands: (p1: GREGdq; p2: MODdq; p3: IMMb); InstrSet: isSSE41);

// 0x0E
  simd_PBLENDW: TSIMDInstruction = (name: 'PBLENDW'; OperandsCount: 3; Operands: (p1: GREGdq; p2: MODdq; p3: IMMb); InstrSet: isSSE41);

// 0x0F
  simd_PALIGNR_a: TSIMDInstruction = (name: 'PALIGNR'; OperandsCount: 3; Operands: (p1:GREGq ;p2:MODq; p3: IMMb); InstrSet: isSSSE3);
  simd_PALIGNR_b: TSIMDInstruction = (name: 'PALIGNR'; OperandsCount: 3; Operands: (p1:GREGdq ;p2:MODdq; p3: IMMb); InstrSet: isSSSE3);


// 0x14
  simd_PEXTRB: TSIMDInstruction = (name: 'PEXTRB'; OperandsCount: 3; Operands: (p1: REG32_M8; p2: GREGdq; p3: IMMb); InstrSet: isSSE41);

// 0x15
  simd_PEXTRW_c: TSIMDInstruction = (name: 'PEXTRW'; OperandsCount: 3; Operands: (p1: REG32_M16; p2: GREGdq; p3: IMMb); InstrSet: isSSE41);

// 0x16
  simd_PEXTRD: TSIMDInstruction = (name: 'PEXTRD'; OperandsCount: 3; Operands: (p1: MODd; p2: GREGdq; p3: IMMb); InstrSet: isSSE41);

// 0x17
  simd_EXTRACTPS: TSIMDInstruction = (name: 'EXTRACTPS'; OperandsCount: 3; Operands: (p1: MODd; p2: GREGdq; p3: IMMb); InstrSet: isSSE41);

// 0x20
  simd_PINSRB: TSIMDInstruction = (name: 'PINSRB'; OperandsCount: 3; Operands: (p1: GREGdq; p2: MODd; p3: IMMb); InstrSet: isSSE41);

// 0x21
  simd_INSERTPS: TSIMDInstruction = (name: 'INSERTPS'; OperandsCount: 3; Operands: (p1: GREGdq; p2: XMM_M32; p3: IMMb); InstrSet: isSSE41);

// 0x22
  simd_PINSRD: TSIMDInstruction = (name: 'PINSRD'; OperandsCount: 3; Operands: (p1: GREGdq; p2: MODd; p3: IMMb); InstrSet: isSSE41);

// 0x40
  simd_DPPS: TSIMDInstruction = (name: 'DPPS'; OperandsCount: 3; Operands: (p1: GREGdq; p2: MODdq; p3: IMMb); InstrSet: isSSE41);

// 0x41
  simd_DPPD: TSIMDInstruction = (name: 'DPPD'; OperandsCount: 3; Operands: (p1: GREGdq; p2: MODdq; p3: IMMb); InstrSet: isSSE41);

// 0x42
  simd_MPSADBW: TSIMDInstruction = (name: 'MPSADBW'; OperandsCount: 3; Operands: (p1: GREGdq; p2: MODdq; p3: IMMb); InstrSet: isSSE41);

// 0x60
  simd_PCMPESTRM: TSIMDInstruction = (name: 'PCMPESTRM'; OperandsCount: 3; Operands: (p1: GREGdq; p2: MODdq; p3: IMMb); InstrSet: isSSE42);

// 0x61
  simd_PCMPESTRI: TSIMDInstruction = (name: 'PCMPESTRI'; OperandsCount: 3; Operands: (p1: GREGdq; p2: MODdq; p3: IMMb); InstrSet: isSSE42);

// 0x61
  simd_PCMPISTRM: TSIMDInstruction = (name: 'PCMPISTRM'; OperandsCount: 3; Operands: (p1: GREGdq; p2: MODdq; p3: IMMb); InstrSet: isSSE42);

// 0x63
  simd_PCMPISTRI: TSIMDInstruction = (name: 'PCMPISTRI'; OperandsCount: 3; Operands: (p1: GREGdq; p2: MODdq; p3: IMMb); InstrSet: isSSE42);


//******************************************************************************
//  Group SIMD instructions
//******************************************************************************


// Group 12 - 0F 71
  simd_PSRLW_c: TSIMDInstruction = (name: 'PSRLW'; OperandsCount: 2; Operands: (p1:R64 ;p2:IMMb); InstrSet: isMMX);
  simd_PSRLW_d: TSIMDInstruction = (name: 'PSRLW'; OperandsCount: 2; Operands: (p1:R128 ;p2:IMMb); InstrSet: isSSE2);
  simd_PSRAW_c: TSIMDInstruction = (name: 'PSRAW'; OperandsCount: 2; Operands: (p1:R64 ;p2:IMMb); InstrSet: isMMX);
  simd_PSRAW_d: TSIMDInstruction = (name: 'PSRAW'; OperandsCount: 2; Operands: (p1:R128 ;p2:IMMb); InstrSet: isSSE2);
  simd_PSLLW_c: TSIMDInstruction = (name: 'PSLLW'; OperandsCount: 2; Operands: (p1:R64 ;p2:IMMb); InstrSet: isMMX);
  simd_PSLLW_d: TSIMDInstruction = (name: 'PSLLW'; OperandsCount: 2; Operands: (p1:R128 ;p2:IMMb); InstrSet: isSSE2);

// Group 13 - 0F 72
  simd_PSRLD_c: TSIMDInstruction = (name: 'PSRLD'; OperandsCount: 2; Operands: (p1:R64 ;p2:IMMb); InstrSet: isMMX);
  simd_PSRLD_d: TSIMDInstruction = (name: 'PSRLD'; OperandsCount: 2; Operands: (p1:R128 ;p2:IMMb); InstrSet: isSSE2);
  simd_PSRAD_c: TSIMDInstruction = (name: 'PSRAD'; OperandsCount: 2; Operands: (p1:R64 ;p2:IMMb); InstrSet: isMMX);
  simd_PSRAD_d: TSIMDInstruction = (name: 'PSRAD'; OperandsCount: 2; Operands: (p1:R128 ;p2:IMMb); InstrSet: isSSE2);
  simd_PSLLD_c: TSIMDInstruction = (name: 'PSLLD'; OperandsCount: 2; Operands: (p1:R64 ;p2:IMMb); InstrSet: isMMX);
  simd_PSLLD_d: TSIMDInstruction = (name: 'PSLLD'; OperandsCount: 2; Operands: (p1:R128 ;p2:IMMb); InstrSet: isSSE2);

// Group 14 - 0F 73
  simd_PSRLQ_c: TSIMDInstruction = (name: 'PSRLQ'; OperandsCount: 2; Operands: (p1:R64 ;p2:IMMb); InstrSet: isMMX);
  simd_PSRLQ_d: TSIMDInstruction = (name: 'PSRLQ'; OperandsCount: 2; Operands: (p1:R128 ;p2:IMMb); InstrSet: isSSE2);
  simd_PSRLDQ: TSIMDInstruction = (name: 'PSRLDQ'; OperandsCount: 2; Operands: (p1:R128 ;p2:IMMb); InstrSet: isSSE2);
  simd_PSLLQ_c: TSIMDInstruction = (name: 'PSLLQ'; OperandsCount: 2; Operands: (p1:R64 ;p2:IMMb); InstrSet: isMMX);
  simd_PSLLQ_d: TSIMDInstruction = (name: 'PSLLQ'; OperandsCount: 2; Operands: (p1:R128 ;p2:IMMb); InstrSet: isSSE2);
  simd_PSLLDQ: TSIMDInstruction = (name: 'PSLLDQ'; OperandsCount: 2; Operands: (p1:R128 ;p2:IMMb); InstrSet: isSSE2);



