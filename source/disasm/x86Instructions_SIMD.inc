{
  x86 SIMD Instructions - MMX, SSE, SSE2, SSE3, SSSE3, SSE4
}

// MMX, SSE(2) instructions
const

// 0..9
                               simd_PADDB_a: TSIMDInstruction = (name: 'PADDB'; opcode: $0; OperandsCount: 2; Operands: (p1:GREGq; p2:MODq));
                               simd_PADDW_a: TSIMDInstruction = (name: 'PADDW'; opcode: $0; OperandsCount: 2; Operands: (p1:GREGq ;p2:MODq));
                               simd_PADDD_a: TSIMDInstruction = (name: 'PADDD'; opcode: $0; OperandsCount: 2; Operands: (p1:GREGq ;p2:MODq));
                               simd_PADDSB_a: TSIMDInstruction = (name: 'PADDSB'; opcode: $0; OperandsCount: 2; Operands: (p1:GREGq ;p2:MODq));
                               simd_PADDSW_a: TSIMDInstruction = (name: 'PADDSW'; opcode: $0; OperandsCount: 2; Operands: (p1:GREGq ;p2:MODq));
                               simd_PADDUSB_a: TSIMDInstruction = (name: 'PADDUSB'; opcode: $0; OperandsCount: 2; Operands: (p1:GREGq ;p2:MODq));
                               simd_PADDUSW_a: TSIMDInstruction = (name: 'PADDUSW'; opcode: $0; OperandsCount: 2; Operands: (p1:GREGq ;p2:MODq));
                               simd_PSUBB_a: TSIMDInstruction = (name: 'PSUBB'; opcode: $0; OperandsCount: 2; Operands: (p1:GREGq ;p2:MODq));
                               simd_PSUBW_a: TSIMDInstruction = (name: 'PSUBW'; opcode: $0; OperandsCount: 2; Operands: (p1:GREGq ;p2:MODq));
                               simd_PSUBD_a: TSIMDInstruction = (name: 'PSUBD'; opcode: $0; OperandsCount: 2; Operands: (p1:GREGq ;p2:MODq));
// 10..19
                               simd_PSUBSB_a: TSIMDInstruction = (name: 'PSUBSB'; opcode: $0; OperandsCount: 2; Operands: (p1:GREGq ;p2:MODq));
                               simd_PSUBSW_a: TSIMDInstruction = (name: 'PSUBSW'; opcode: $0; OperandsCount: 2; Operands: (p1:GREGq ;p2:MODq));
                               simd_PSUBUSB_a: TSIMDInstruction = (name: 'PSUBUSB'; opcode: $D8; OperandsCount: 2; Operands: (p1:GREGq ;p2:MODq));
                               simd_PSUBUSW_: TSIMDInstruction = (name: 'PSUBUSW'; opcode: $D9; OperandsCount: 2; Operands: (p1:GREGq ;p2:MODq));
                               simd_PMULLW_a: TSIMDInstruction = (name: 'PMULLW'; opcode: $D5; OperandsCount: 2; Operands: (p1:GREGq ;p2:MODq));
                               simd_PMULHW_a: TSIMDInstruction = (name: 'PMULHW'; opcode: $E5; OperandsCount: 2; Operands: (p1:GREGq ;p2:MODq));
                               simd_PMADDWD_a: TSIMDInstruction = (name: 'PMADDWD'; opcode: $0; OperandsCount: 2; Operands: (p1:GREGq ;p2:MODq));

                               simd_PCMPEQB_a: TSIMDInstruction = (name: 'PCMPEQB'; opcode: $74; OperandsCount: 2; Operands: (p1:GREGq ;p2:MODq));
                               simd_PCMPEQW_a: TSIMDInstruction = (name: 'PCMPEQW'; opcode: $75; OperandsCount: 2; Operands: (p1:GREGq ;p2:MODq));
                               simd_PCMPEQD_a: TSIMDInstruction = (name: 'PCMPEQD'; opcode: $76; OperandsCount: 2; Operands: (p1:GREGq ;p2:MODq));
// 20..29
                               simd_PCMPGTB_a: TSIMDInstruction = (name: 'PCMPGTB'; opcode: $64; OperandsCount: 2; Operands: (p1:GREGq ;p2:MODq));
                               simd_PCMPGTW_a: TSIMDInstruction = (name: 'PCMPGTW'; opcode: $65; OperandsCount: 2; Operands: (p1:GREGq ;p2:MODq));
                               simd_PCMPGTD_a: TSIMDInstruction = (name: 'PCMPGTD'; opcode: $66; OperandsCount: 2; Operands: (p1:GREGq ;p2:MODq));

                               simd_PACKSSWB_a: TSIMDInstruction = (name: 'PACKSSWB'; opcode: $63; OperandsCount: 2; Operands: (p1:GREGq ;p2:MODq));
                               simd_PACKSSDW_a: TSIMDInstruction = (name: 'PACKSSDW'; opcode: $6B; OperandsCount: 2; Operands: (p1:GREGq ;p2:MODd));
                               simd_PACKUSWB_a: TSIMDInstruction = (name: 'PACKUSWB'; opcode: $67; OperandsCount: 2; Operands: (p1:GREGq ;p2:MODq));

                               simd_PUNPCKHBW_a: TSIMDInstruction = (name: 'PUNPCKHBW'; opcode: $68; OperandsCount: 2; Operands: (p1:GREGq ;p2:MODq));
                               simd_PUNPCKHWD_a: TSIMDInstruction = (name: 'PUNPCKHWD'; opcode: $69; OperandsCount: 2; Operands: (p1:GREGq ;p2:MODq));
                               simd_PUNPCKHDQ_a: TSIMDInstruction = (name: 'PUNPCKHDQ'; opcode: $6A; OperandsCount: 2; Operands: (p1:GREGq ;p2:MODq));
                               simd_PUNPCKLBW_a: TSIMDInstruction = (name: 'PUNPCKLBW'; opcode: $60; OperandsCount: 2; Operands: (p1:GREGq ;p2:MODq));
// 30..39
                               simd_PUNPCKLWD_a: TSIMDInstruction = (name: 'PUNPCKLWD'; opcode: $61; OperandsCount: 2; Operands: (p1:GREGq ;p2:MODd));
                               simd_PUNPCKLDQ_a: TSIMDInstruction = (name: 'PUNPCKLDQ'; opcode: $62; OperandsCount: 2; Operands: (p1:GREGq ;p2:MODd));

                               simd_PAND_a: TSIMDInstruction = (name: 'PAND'; opcode: $DB; OperandsCount: 2; Operands: (p1:GREGq ;p2:MODq));
                               simd_PANDN_a: TSIMDInstruction = (name: 'PANDN'; opcode: $DF; OperandsCount: 2; Operands: (p1:GREGq ;p2:MODq));
                               simd_POR_a: TSIMDInstruction = (name: 'POR'; opcode: $EB; OperandsCount: 2; Operands: (p1:GREGq ;p2:MODq));
                               simd_PXOR_a: TSIMDInstruction = (name: 'PXOR'; opcode: $EF; OperandsCount: 2; Operands: (p1:GREGq ;p2:MODq));

                               simd_PSLLW_a: TSIMDInstruction = (name: 'PSLLW'; opcode:$F1 ; OperandsCount: 2; Operands: (p1:GREGq ;p2:MODq));
                               simd_PSLLD_a: TSIMDInstruction = (name: 'PSLLD'; opcode:$F2 ; OperandsCount: 2; Operands: (p1:GREGq ;p2:MODq));
                               simd_PSLLQ_a: TSIMDInstruction = (name: 'PSLLQ'; opcode:$F3 ; OperandsCount: 2; Operands: (p1:GREGq ;p2:MODq));
                               simd_PSRLW_a: TSIMDInstruction = (name: 'PSRLW'; opcode:$D1 ; OperandsCount: 2; Operands: (p1:GREGq ;p2:MODq));
// 40..47
                               simd_PSRLD_a: TSIMDInstruction = (name: 'PSRLD'; opcode:$D2 ; OperandsCount: 2; Operands: (p1:GREGq ;p2:MODq));
                               simd_PSRLQ_a: TSIMDInstruction = (name: 'PSRLQ'; opcode:$D3 ; OperandsCount: 2; Operands: (p1:GREGq ;p2:MODq));
                               simd_PSRAW_a: TSIMDInstruction = (name: 'PSRAW'; opcode:$E1 ; OperandsCount: 2; Operands: (p1:GREGq ;p2:MODq));
                               simd_PSRAD_a: TSIMDInstruction = (name: 'PSRAD'; opcode:$E2 ; OperandsCount: 2; Operands: (p1:GREGq ;p2:MODq));

                               simd_MOVD_a: TSIMDInstruction = (name: 'MOVD'; opcode: $6E; OperandsCount: 2; Operands: (p1:GREGd ;p2:MODd));
                               simd_MOVD_b: TSIMDInstruction = (name: 'MOVD'; opcode: $7E; OperandsCount: 2; Operands: (p1:MODd ;p2:GREGd));
//                               simd_MOVD: TSIMDInstruction = (name: 'MOVD'; opcode: $; OperandsCount: ; Operands: (p1: ;p2:));
                               simd_MOVQ_a: TSIMDInstruction = (name: 'MOVQ'; opcode: $6F; OperandsCount: 2; Operands: (p1:GREGq ;p2:MODq));
                               simd_MOVQ_b: TSIMDInstruction = (name: 'MOVQ'; opcode: $7F; OperandsCount: 2; Operands: (p1:MODq ;p2:GREGq));
//                               (name: 'MOVQ'; opcode: $; OperandsCount: ; Operands: (p1: ;p2:))


// SSE & SSE2 Instructions ...

// 48..49
                               simd_MOVUPS_a: TSIMDInstruction = (name: 'MOVUPS'; opcode: $10; OperandsCount: 2; Operands: (p1:GREGdq ;p2:MODdq));
                               simd_MOVSS_a: TSIMDInstruction = (name: 'MOVSS'; opcode: $10; OperandsCount: 2; Operands: (p1:GREGdq ;p2:XMM_M32));
// 50..59
                               simd_MOVUPD_a: TSIMDInstruction = (name: 'MOVUPD'; opcode: $10; OperandsCount: 2; Operands: (p1:GREGdq ;p2:MODdq));
                               simd_MOVSD_a: TSIMDInstruction = (name: 'MOVSD'; opcode: $10; OperandsCount: 2; Operands: (p1:GREGdq ;p2:XMM_M64));
                               simd_MOVUPS_b: TSIMDInstruction = (name: 'MOVUPS'; opcode: $11; OperandsCount: 2; Operands: (p1:MODdq ;p2:GREGdq));
                               simd_MOVSS_b: TSIMDInstruction = (name: 'MOVSS'; opcode: $11; OperandsCount: 2; Operands: (p1:XMM_M32 ;p2:GREGdq));
                               simd_MOVUPD_b: TSIMDInstruction = (name: 'MOVUPD'; opcode: $11; OperandsCount: 2; Operands: (p1:MODdq ;p2:GREGdq));
                               simd_MOVSD_b: TSIMDInstruction = (name: 'MOVSD'; opcode: $11; OperandsCount: 2; Operands: (p1:XMM_M64 ;p2:GREGdq));
                               simd_MOVLPS_a: TSIMDInstruction = (name: 'MOVLPS'; opcode: $12; OperandsCount: 2; Operands: (p1:GREGdq ;p2:M64));     // rovnaky opcode s MOVHLPS !
                               simd_MOVLPD_a: TSIMDInstruction = (name: 'MOVLPD'; opcode: $12; OperandsCount: 2; Operands: (p1:GREGdq ;p2:M64));
                               simd_MOVHLPS: TSIMDInstruction = (name: 'MOVHLPS'; opcode: $12; OperandsCount: 2; Operands: (p1:GREGdq ;p2:R128));   // rovnaky opcode s MOVLPS  !
                               simd_MOVLPS_b: TSIMDInstruction = (name: 'MOVLPS'; opcode: $13; OperandsCount: 2; Operands: (p1:M64 ;p2:GREGdq));
                               simd_MOVLPD_b: TSIMDInstruction = (name: 'MOVLPD'; opcode: $13; OperandsCount: 2; Operands: (p1:M64 ;p2:GREGdq));
// 60..69 +1 atd...
                               simd_UNPCKLPS: TSIMDInstruction = (name: 'UNPCKLPS'; opcode: $14; OperandsCount: 2; Operands: (p1:GREGdq ;p2:MODdq));
                               simd_UNPCKLPD: TSIMDInstruction = (name: 'UNPCKLPD'; opcode: $14; OperandsCount: 2; Operands: (p1:GREGdq ;p2:MODdq));
                               simd_UNPCKHPS: TSIMDInstruction = (name: 'UNPCKHPS'; opcode: $15; OperandsCount: 2; Operands: (p1:GREGdq ;p2:MODdq));
                               simd_UNPCKHPD: TSIMDInstruction = (name: 'UNPCKHPD'; opcode: $15; OperandsCount: 2; Operands: (p1:GREGdq ;p2:MODdq));
                               simd_MOVHPS_a: TSIMDInstruction = (name: 'MOVHPS'; opcode: $16; OperandsCount: 2; Operands: (p1:GREGdq ;p2:M64));     // rovnaky opcode s MOVLHPS
                               simd_MOVHPD_a: TSIMDInstruction = (name: 'MOVHPD'; opcode: $16; OperandsCount: 2; Operands: (p1:GREGdq ;p2:M64));
                               simd_MOVLHPS: TSIMDInstruction = (name: 'MOVLHPS'; opcode: $16; OperandsCount: 2; Operands: (p1:GREGdq ;p2:R128));   // rovnaky opcode s MOVHPS
                               simd_MOVHPS_b: TSIMDInstruction = (name: 'MOVHPS'; opcode: $17; OperandsCount: 2; Operands: (p1:M64 ;p2:GREGdq));
                               simd_MOVHPD_b: TSIMDInstruction = (name: 'MOVHPD'; opcode: $17; OperandsCount: 2; Operands: (p1:M64 ;p2:GREGdq));

                               simd_MOVAPS_a: TSIMDInstruction = (name: 'MOVAPS'; opcode: $28; OperandsCount: 2; Operands: (p1:GREGdq ;p2:MODdq));
// 70..79
                               simd_MOVAPD_a: TSIMDInstruction = (name: 'MOVAPD'; opcode: $28; OperandsCount: 2; Operands: (p1:GREGdq ;p2:MODdq));
                               simd_MOVAPS_b: TSIMDInstruction = (name: 'MOVAPS'; opcode: $29; OperandsCount: 2; Operands: (p1:MODdq ;p2:GREGdq));
                               simd_MOVAPD_b: TSIMDInstruction = (name: 'MOVAPD'; opcode: $29; OperandsCount: 2; Operands: (p1:MODdq ;p2:GREGdq));
                               simd_CVTPI2PS: TSIMDInstruction = (name: 'CVTPI2PS'; opcode: $2A; OperandsCount: 2; Operands: (p1:GREGdq ;p2:MODq));
                               simd_CVTSI2SS: TSIMDInstruction = (name: 'CVTSI2SS'; opcode: $2A; OperandsCount: 2; Operands: (p1:GREGdq ;p2:MODd));
                               simd_CVTPI2PD: TSIMDInstruction = (name: 'CVTPI2PD'; opcode: $2A; OperandsCount: 2; Operands: (p1:GREGdq ;p2:MODq));
                               simd_CVTSI2SD: TSIMDInstruction = (name: 'CVTSI2SD'; opcode: $2A; OperandsCount: 2; Operands: (p1:GREGdq ;p2:MODd));
                               simd_MOVNTPS: TSIMDInstruction = (name: 'MOVNTPS'; opcode: $2B; OperandsCount: 2; Operands: (p1:M128 ;p2:GREGdq));
                               simd_MOVNTPD: TSIMDInstruction = (name: 'MOVNTPD'; opcode: $2B; OperandsCount: 2; Operands: (p1:M128 ;p2:GREGdq));
                               simd_CVTTPS2PI: TSIMDInstruction = (name: 'CVTTPS2PI'; opcode: $2C; OperandsCount: 2; Operands: (p1:GREGq ;p2:XMM_M32));
// 80..89
                               simd_CVTTSS2SI: TSIMDInstruction = (name: 'CVTTSS2SI'; opcode: $2C; OperandsCount: 2; Operands: (p1:GREGd ;p2:XMM_M32));
                               simd_CVTTPD2PI: TSIMDInstruction = (name: 'CVTTPD2PI'; opcode: $2C; OperandsCount: 2; Operands: (p1:GREGq ;p2:MODdq));
                               simd_CVTTSD2SI: TSIMDInstruction = (name: 'CVTTSD2SI'; opcode: $2C; OperandsCount: 2; Operands: (p1:GREGd ;p2:XMM_M64));
                               simd_CVTPS2PI: TSIMDInstruction = (name: 'CVTPS2PI'; opcode: $2D; OperandsCount: 2; Operands: (p1:GREGq ;p2:XMM_M64));
                               simd_CVTSS2SI: TSIMDInstruction = (name: 'CVTSS2SI'; opcode: $2D; OperandsCount: 2; Operands: (p1:GREGd ;p2:XMM_M32));
                               simd_CVTPD2PI: TSIMDInstruction = (name: 'CVTPD2PI'; opcode: $2D; OperandsCount: 2; Operands: (p1:GREGq ;p2:MODdq));
                               simd_CVTSD2SI: TSIMDInstruction = (name: 'CVTSD2SI'; opcode: $2D; OperandsCount: 2; Operands: (p1:GREGd ;p2:XMM_M64));
                               simd_UCOMISS: TSIMDInstruction = (name: 'UCOMISS'; opcode: $2E; OperandsCount: 2; Operands: (p1:GREGdq ;p2:XMM_M32));
                               simd_UCOMISD: TSIMDInstruction = (name: 'UCOMISD'; opcode: $2E; OperandsCount: 2; Operands: (p1:GREGdq ;p2:XMM_M64));
                               simd_COMISS: TSIMDInstruction = (name: 'COMISS'; opcode: $2F; OperandsCount: 2; Operands: (p1:GREGdq ;p2:XMM_M32));
// 90..99
                               simd_COMISD: TSIMDInstruction = (name: 'COMISD'; opcode: $2F; OperandsCount: 2; Operands: (p1:GREGdq ;p2:XMM_M64));

                               simd_MOVMSKPS: TSIMDInstruction = (name: 'MOVMSKPS'; opcode: $50; OperandsCount: 2; Operands: (p1:R32 ;p2:GREGdq));
                               simd_MOVMSKPD: TSIMDInstruction = (name: 'MOVMSKPD'; opcode: $50; OperandsCount: 2; Operands: (p1:R32 ;p2:GREGdq));
                               simd_SQRTPS: TSIMDInstruction = (name: 'SQRTPS'; opcode: $51; OperandsCount: 2; Operands: (p1:GREGdq ;p2:MODdq));
                               simd_SQRTSS: TSIMDInstruction = (name: 'SQRTSS'; opcode: $51; OperandsCount: 2; Operands: (p1:GREGdq ;p2:XMM_M32));
                               simd_SQRTPD: TSIMDInstruction = (name: 'SQRTPD'; opcode: $51; OperandsCount: 2; Operands: (p1:GREGdq ;p2:MODdq));
                               simd_SQRTSD: TSIMDInstruction = (name: 'SQRTSD'; opcode: $51; OperandsCount: 2; Operands: (p1:GREGdq ;p2:XMM_M64));
                               simd_RSQRTPS: TSIMDInstruction = (name: 'RSQRTPS'; opcode: $52; OperandsCount: 2; Operands: (p1:GREGdq ;p2:MODdq));
                               simd_RSQRTSS: TSIMDInstruction = (name: 'RSQRTSS'; opcode: $52; OperandsCount: 2; Operands: (p1:GREGdq ;p2:XMM_M32));
                               simd_RCPPS: TSIMDInstruction = (name: 'RCPPS'; opcode: $53; OperandsCount: 2; Operands: (p1:GREGdq ;p2:MODdq));
// 100..109
                               simd_RCPSS: TSIMDInstruction = (name: 'RCPSS'; opcode: $53; OperandsCount: 2; Operands: (p1:GREGdq ;p2:XMM_M32));
                               simd_ANDPS: TSIMDInstruction = (name: 'ANDPS'; opcode: $54; OperandsCount: 2; Operands: (p1:GREGdq ;p2:MODdq));
                               simd_ANDPD: TSIMDInstruction = (name: 'ANDPD'; opcode: $54; OperandsCount: 2; Operands: (p1:GREGdq ;p2:MODdq));
                               simd_ANDNPS: TSIMDInstruction = (name: 'ANDNPS'; opcode: $55; OperandsCount: 2; Operands: (p1:GREGdq ;p2:MODdq));
                               simd_ANDNPD: TSIMDInstruction = (name: 'ANDNPD'; opcode: $55; OperandsCount: 2; Operands: (p1:GREGdq ;p2:MODdq));
                               simd_ORPS: TSIMDInstruction = (name: 'ORPS'; opcode: $56; OperandsCount: 2; Operands: (p1:GREGdq ;p2:MODdq));
                               simd_ORPD: TSIMDInstruction = (name: 'ORPD'; opcode: $56; OperandsCount: 2; Operands: (p1:GREGdq ;p2:MODdq));
                               simd_XORPS: TSIMDInstruction = (name: 'XORPS'; opcode: $57; OperandsCount: 2; Operands: (p1:GREGdq ;p2:MODdq));
                               simd_XORPD: TSIMDInstruction = (name: 'XORPD'; opcode: $57; OperandsCount: 2; Operands: (p1:GREGdq ;p2:MODdq));

                               simd_ADDPS: TSIMDInstruction = (name: 'ADDPS'; opcode: $58; OperandsCount: 2; Operands: (p1:GREGdq ;p2:MODdq));
// 110..119
                               simd_ADDSS: TSIMDInstruction = (name: 'ADDSS'; opcode: $58; OperandsCount: 2; Operands: (p1:GREGdq ;p2:XMM_M32));
                               simd_ADDPD: TSIMDInstruction = (name: 'ADDPD'; opcode: $58; OperandsCount: 2; Operands: (p1:GREGdq ;p2:MODdq));
                               simd_ADDSD: TSIMDInstruction = (name: 'ADDSD'; opcode: $58; OperandsCount: 2; Operands: (p1:GREGdq ;p2:XMM_M64));
                               simd_MULPS: TSIMDInstruction = (name: 'MULPS'; opcode: $59; OperandsCount: 2; Operands: (p1:GREGdq ;p2:MODdq));
                               simd_MULSS: TSIMDInstruction = (name: 'MULSS'; opcode: $59; OperandsCount: 2; Operands: (p1:GREGdq ;p2:XMM_M32));
                               simd_MULPD: TSIMDInstruction = (name: 'MULPD'; opcode: $59; OperandsCount: 2; Operands: (p1:GREGdq ;p2:MODdq));
                               simd_MULSD: TSIMDInstruction = (name: 'MULSD'; opcode: $59; OperandsCount: 2; Operands: (p1:GREGdq ;p2:XMM_M64));
                               simd_CVTPS2PD: TSIMDInstruction = (name: 'CVTPS2PD'; opcode: $5A; OperandsCount: 2; Operands: (p1:GREGdq ;p2:XMM_M64));
                               simd_CVTSS2SD: TSIMDInstruction = (name: 'CVTSS2SD'; opcode: $5A; OperandsCount: 2; Operands: (p1:GREGdq ;p2:XMM_M32));
                               simd_CVTPD2PS: TSIMDInstruction = (name: 'CVTPD2PS'; opcode: $5A; OperandsCount: 2; Operands: (p1:GREGdq ;p2:MODdq));
// 120..129
                               simd_CVTSD2SS: TSIMDInstruction = (name: 'CVTSD2SS'; opcode: $5A; OperandsCount: 2; Operands: (p1:GREGdq ;p2:XMM_M64));
                               simd_CVTDQ2PS: TSIMDInstruction = (name: 'CVTDQ2PS'; opcode: $5B; OperandsCount: 2; Operands: (p1:GREGdq ;p2:MODdq));
                               simd_CVTPS2DQ: TSIMDInstruction = (name: 'CVTPS2DQ'; opcode: $5B; OperandsCount: 2; Operands: (p1:GREGdq ;p2:MODdq));
                               simd_CVTTPS2DQ: TSIMDInstruction = (name: 'CVTTPS2DQ'; opcode: $5B; OperandsCount: 2; Operands: (p1:GREGdq ;p2:MODdq));
                               simd_SUBPS: TSIMDInstruction = (name: 'SUBPS'; opcode: $5C; OperandsCount: 2; Operands: (p1:GREGdq ;p2:MODdq));
                               simd_SUBSS: TSIMDInstruction = (name: 'SUBSS'; opcode: $5C; OperandsCount: 2; Operands: (p1:GREGdq ;p2:XMM_M32));
                               simd_SUBPD: TSIMDInstruction = (name: 'SUBPD'; opcode: $5C; OperandsCount: 2; Operands: (p1:GREGdq ;p2:MODdq));
                               simd_SUBSD: TSIMDInstruction = (name: 'SUBSD'; opcode: $5C; OperandsCount: 2; Operands: (p1:GREGdq ;p2:XMM_M64));
                               simd_MINPS: TSIMDInstruction = (name: 'MINPS'; opcode: $5D; OperandsCount: 2; Operands: (p1:GREGdq ;p2:MODdq));
                               simd_MINSS: TSIMDInstruction = (name: 'MINSS'; opcode: $5D; OperandsCount: 2; Operands: (p1:GREGdq ;p2:XMM_M32));
// 130..139
                               simd_MINPD: TSIMDInstruction = (name: 'MINPD'; opcode: $5D; OperandsCount: 2; Operands: (p1:GREGdq ;p2:MODdq));
                               simd_MINSD: TSIMDInstruction = (name: 'MINSD'; opcode: $5D; OperandsCount: 2; Operands: (p1:GREGdq ;p2:XMM_M64));
                               simd_DIVPS: TSIMDInstruction = (name: 'DIVPS'; opcode: $5E; OperandsCount: 2; Operands: (p1:GREGdq ;p2:MODdq));
                               simd_DIVSS: TSIMDInstruction = (name: 'DIVSS'; opcode: $5E; OperandsCount: 2; Operands: (p1:GREGdq ;p2:XMM_M32));
                               simd_DIVPD: TSIMDInstruction = (name: 'DIVPD'; opcode: $5E; OperandsCount: 2; Operands: (p1:GREGdq ;p2:MODdq));
                               simd_DIVSD: TSIMDInstruction = (name: 'DIVSD'; opcode: $5E; OperandsCount: 2; Operands: (p1:GREGdq ;p2:XMM_M64));
                               simd_MAXPS: TSIMDInstruction = (name: 'MAXPS'; opcode: $5F; OperandsCount: 2; Operands: (p1:GREGdq ;p2:MODdq));
                               simd_MAXSS: TSIMDInstruction = (name: 'MAXSS'; opcode: $5F; OperandsCount: 2; Operands: (p1:GREGdq ;p2:XMM_M32));
                               simd_MAXPD: TSIMDInstruction = (name: 'MAXPD'; opcode: $5F; OperandsCount: 2; Operands: (p1:GREGdq ;p2:MODdq));
                               simd_MAXSD: TSIMDInstruction = (name: 'MAXSD'; opcode: $5F; OperandsCount: 2; Operands: (p1:GREGdq ;p2:XMM_M64));
// 140..149
                               simd_PUNPCKLBW_b: TSIMDInstruction = (name: 'PUNPCKLBW'; opcode: $60; OperandsCount: 2; Operands: (p1:GREGdq ;p2:MODdq));
                               simd_PUNPCKLWD_b: TSIMDInstruction = (name: 'PUNPCKLWD'; opcode: $61; OperandsCount: 2; Operands: (p1:GREGdq ;p2:MODdq));
                               simd_PUNPCKLDQ_b: TSIMDInstruction = (name: 'PUNPCKLDQ'; opcode: $62; OperandsCount: 2; Operands: (p1:GREGdq ;p2:MODdq));
                               simd_PACKSSWB_b: TSIMDInstruction = (name: 'PACKSSWB'; opcode: $63; OperandsCount: 2; Operands: (p1:GREGdq ;p2:MODdq));
                               simd_PCMPGTB_b: TSIMDInstruction = (name: 'PCMPGTB'; opcode: $64; OperandsCount: 2; Operands: (p1:GREGdq ;p2:MODdq));
                               simd_PCMPGTW_b: TSIMDInstruction = (name: 'PCMPGTW'; opcode: $65; OperandsCount: 2; Operands: (p1:GREGdq ;p2:MODdq));
                               simd_PCMPGTD_b: TSIMDInstruction = (name: 'PCMPGTD'; opcode: $66; OperandsCount: 2; Operands: (p1:GREGdq ;p2:MODdq));
                               simd_PACKUSWB_b: TSIMDInstruction = (name: 'PACKUSWB'; opcode: $67; OperandsCount: 2; Operands: (p1:GREGdq ;p2:MODdq));
                               simd_PUNPCKHBW_b: TSIMDInstruction = (name: 'PUNPCKHBW'; opcode: $68; OperandsCount: 2; Operands: (p1:GREGdq ;p2:MODdq));
                               simd_PUNPCKHWD_b: TSIMDInstruction = (name: 'PUNPCKHWD'; opcode: $69; OperandsCount: 2; Operands: (p1:GREGdq ;p2:MODdq));
// 150..159
                               simd_PUNPCKHDQ_b: TSIMDInstruction = (name: 'PUNPCKHDQ'; opcode: $6A; OperandsCount: 2; Operands: (p1:GREGdq ;p2:MODdq));
                               simd_PACKSSDW_b: TSIMDInstruction = (name: 'PACKSSDW'; opcode: $6B; OperandsCount: 2; Operands: (p1:GREGdq ;p2:MODdq));
                               simd_PUNPCKLQDQ: TSIMDInstruction = (name: 'PUNPCKLQDQ'; opcode: $6C; OperandsCount: 2; Operands: (p1:GREGdq ;p2:MODdq));
                               simd_PUNPCKHQD: TSIMDInstruction = (name: 'PUNPCKHQD'; opcode: $6D; OperandsCount: 2; Operands: (p1:GREGdq ;p2:MODdq));
                               simd_MOVD_c: TSIMDInstruction = (name: 'MOVD'; opcode: $6E; OperandsCount: 2; Operands: (p1:GREGdq ;p2:MODd));
                               simd_MOVDQA_a: TSIMDInstruction = (name: 'MOVDQA'; opcode: $6F; OperandsCount: 2; Operands: (p1:GREGdq ;p2:MODdq));
                               simd_MOVDQU_a: TSIMDInstruction = (name: 'MOVDQU'; opcode: $6F; OperandsCount: 2; Operands: (p1:GREGdq ;p2:MODdq));

                               simd_PSHUFW: TSIMDInstruction = (name: 'PSHUFW'; opcode: $70; OperandsCount: 3; Operands: (p1:GREGq ;p2:MODq; p3:IMMb));
                               simd_PSHUFD: TSIMDInstruction = (name: 'PSHUFD'; opcode: $70; OperandsCount: 3; Operands: (p1:GREGdq ;p2:MODdq; p3:IMMb));
                               simd_PSHUFHW: TSIMDInstruction = (name: 'PSHUFHW'; opcode: $70; OperandsCount: 3; Operands: (p1:GREGdq ;p2:MODdq; p3:IMMb));
// 160..169
                               simd_PSHUFLW: TSIMDInstruction = (name: 'PSHUFLW'; opcode: $70; OperandsCount: 3; Operands: (p1:GREGdq ;p2:MODdq; p3:IMMb));
                               simd_PCMPEQB_b: TSIMDInstruction = (name: 'PCMPEQB'; opcode: $74; OperandsCount: 2; Operands: (p1:GREGdq ;p2:MODdq));
                               simd_PCMPEQW_b: TSIMDInstruction = (name: 'PCMPEQW'; opcode: $75; OperandsCount: 2; Operands: (p1:GREGdq ;p2:MODdq));
                               simd_PCMPEQD_b: TSIMDInstruction = (name: 'PCMPEQD'; opcode: $76; OperandsCount: 2; Operands: (p1:GREGdq ;p2:MODdq));
                               simd_MOVD_d: TSIMDInstruction = (name: 'MOVD'; opcode: $7E; OperandsCount: 2; Operands: (p1:MODd ;p2:GREGdq));
                               simd_MOVQ_c: TSIMDInstruction = (name: 'MOVQ'; opcode: $7E; OperandsCount: 2; Operands: (p1:GREGdq ;p2:XMM_M64));
                               simd_MOVDQA_b: TSIMDInstruction = (name: 'MOVDQA'; opcode: $7F; OperandsCount: 2; Operands: (p1:MODdq ;p2:GREGdq));
                               simd_MOVDQU_b: TSIMDInstruction = (name: 'MOVDQU'; opcode: $7F; OperandsCount: 2; Operands: (p1:MODdq ;p2:GREGdq));

                               simd_CMPPS: TSIMDInstruction = (name: 'CMPPS'; opcode: $C2; OperandsCount: 2; Operands: (p1:GREGdq ;p2:MODdq ;p3:IMMb));
                               simd_CMPSS: TSIMDInstruction = (name: 'CMPSS'; opcode: $C2; OperandsCount: 2; Operands: (p1:GREGdq ;p2:XMM_M32 ;p3:IMMb));
// 170..179
                               simd_CMPPD: TSIMDInstruction = (name: 'CMPPD'; opcode: $C2; OperandsCount: 2; Operands: (p1:GREGdq ;p2:MODdq ;p3:IMMb));
                               simd_CMPSD: TSIMDInstruction = (name: 'CMPSD'; opcode: $C2; OperandsCount: 2; Operands: (p1:GREGdq ;p2:XMM_M64 ;p3:IMMb));
                               simd_MOVNTI: TSIMDInstruction = (name: 'MOVNTI'; opcode: $C3; OperandsCount: 2; Operands: (p1:M32 ;p2:GREGd));
                               simd_PINSRW_a: TSIMDInstruction = (name: 'PINSRW'; opcode: $C4; OperandsCount: 2; Operands: (p1:GREGq ;p2:REG32_M16 ;p3:IMMb));
                               simd_PINSRW_b: TSIMDInstruction = (name: 'PINSRW'; opcode: $C4; OperandsCount: 2; Operands: (p1:GREGdq ;p2:REG32_M16 ;p3:IMMb));
                               simd_PEXTRW_a: TSIMDInstruction = (name: 'PEXTRW'; opcode: $C5; OperandsCount: 2; Operands: (p1:GREGd ;p2:GREGq ;p3:IMMb));     // Vzdy prisluchajuce registre !!!
                               simd_PEXTRW_b: TSIMDInstruction = (name: 'PEXTRW'; opcode: $C5; OperandsCount: 2; Operands: (p1:GREGd ;p2:GREGdq ;p3:IMMb));    // Vzdy prisluchajuce registre !!!
                               simd_SHUFPS: TSIMDInstruction = (name: 'SHUFPS'; opcode: $C6; OperandsCount: 2; Operands: (p1:GREGdq ;p2:MODdq ;p3:IMMb));
                               simd_SHUFPD: TSIMDInstruction = (name: 'SHUFPD'; opcode: $C6; OperandsCount: 2; Operands: (p1:GREGdq ;p2:MODdq ;p3:IMMb));

                               simd_PSRLW_b: TSIMDInstruction = (name: 'PSRLW'; opcode: $D1; OperandsCount: 2; Operands: (p1:GREGdq ;p2:MODdq));
// 180..189
                               simd_PSRLD_b: TSIMDInstruction = (name: 'PSRLD'; opcode: $D2; OperandsCount: 2; Operands: (p1:GREGdq ;p2:MODdq));
                               simd_PSRLQ_b: TSIMDInstruction = (name: 'PSRLQ'; opcode: $D3; OperandsCount: 2; Operands: (p1:GREGdq ;p2:MODdq));
                               simd_PADDQ_a: TSIMDInstruction = (name: 'PADDQ'; opcode: $D4; OperandsCount: 2; Operands: (p1:GREGq ;p2:MODq));
                               simd_PADDQ_b: TSIMDInstruction = (name: 'PADDQ'; opcode: $D4; OperandsCount: 2; Operands: (p1:GREGdq ;p2:MODdq));
                               simd_PMULLW_b: TSIMDInstruction = (name: 'PMULLW'; opcode: $D5; OperandsCount: 2; Operands: (p1:GREGdq ;p2:MODdq));
                               simd_MOVQ_d: TSIMDInstruction = (name: 'MOVQ'; opcode: $D6; OperandsCount: 2; Operands: (p1:XMM_M64 ;p2:GREGDq));
                               simd_MOVQ2DQ: TSIMDInstruction = (name: 'MOVQ2DQ'; opcode: $D6; OperandsCount: 2; Operands: (p1:GREGdq ;p2:MODq));
                               simd_MOVDQ2Q: TSIMDInstruction = (name: 'MOVDQ2Q'; opcode: $D6; OperandsCount: 2; Operands: (p1:GREGq ;p2:MODDq));
                               simd_PMOVMSKB: TSIMDInstruction = (name: 'PMOVMSKB'; opcode: $D7; OperandsCount: 2; Operands: (p1:GREGd ;p2:GREGq));     // Vzdy prisluchajuce registre !!!
                               simd_PMOVMKSB: TSIMDInstruction = (name: 'PMOVMKSB'; opcode: $D7; OperandsCount: 2; Operands: (p1:GREGd ;p2:GREGdq));    // Vzdy prisluchajuce registre !!!
// 190..199
                               simd_PSUBUSB_b: TSIMDInstruction = (name: 'PSUBUSB'; opcode: $D8; OperandsCount: 2; Operands: (p1:GREGdq ;p2:MODdq));
                               simd_PSUBUSW_b: TSIMDInstruction = (name: 'PSUBUSW'; opcode: $D9; OperandsCount: 2; Operands: (p1:GREGdq ;p2:MODdq));
                               simd_PMINUB_a: TSIMDInstruction = (name: 'PMINUB'; opcode: $DA; OperandsCount: 2; Operands: (p1:GREGq ;p2:MODq));
                               simd_PMINUB_b: TSIMDInstruction = (name: 'PMINUB'; opcode: $DA; OperandsCount: 2; Operands: (p1:GREGdq ;p2:MODdq));
                               simd_PAND_b: TSIMDInstruction = (name: 'PAND'; opcode: $DB; OperandsCount: 2; Operands: (p1:GREGdq ;p2:MODdq));
                               simd_PADDUSB_b: TSIMDInstruction = (name: 'PADDUSB'; opcode: $DC; OperandsCount: 2; Operands: (p1:GREGdq ;p2:MODdq));
                               simd_PADDUSW_b: TSIMDInstruction = (name: 'PADDUSW'; opcode: $DD; OperandsCount: 2; Operands: (p1:GREGdq ;p2:MODdq));
                               simd_PMAXUB_a: TSIMDInstruction = (name: 'PMAXUB'; opcode: $DE; OperandsCount: 2; Operands: (p1:GREGq ;p2:MODq));
                               simd_PMAXUB_b: TSIMDInstruction = (name: 'PMAXUB'; opcode: $DE; OperandsCount: 2; Operands: (p1:GREGdq ;p2:MODdq));
                               simd_PANDN_b: TSIMDInstruction = (name: 'PANDN'; opcode: $DF; OperandsCount: 2; Operands: (p1:GREGdq ;p2:MODdq));
// 200..209
                               simd_PAVGB_a: TSIMDInstruction = (name: 'PAVGB'; opcode: $E0; OperandsCount: 2; Operands: (p1:GREGq ;p2:MODq));
                               simd_PAVGB_b: TSIMDInstruction = (name: 'PAVGB'; opcode: $E0; OperandsCount: 2; Operands: (p1:GREGdq ;p2:MODdq));
                               simd_PSRAW_b: TSIMDInstruction = (name: 'PSRAW'; opcode: $E1; OperandsCount: 2; Operands: (p1:GREGdq ;p2:MODdq));
                               simd_PSRAD_b: TSIMDInstruction = (name: 'PSRAD'; opcode: $E2; OperandsCount: 2; Operands: (p1:GREGdq ;p2:MODdq));
                               simd_PAVGW_a: TSIMDInstruction = (name: 'PAVGW'; opcode: $E3; OperandsCount: 2; Operands: (p1:GREGq ;p2:MODq));
                               simd_PAVGW_b: TSIMDInstruction = (name: 'PAVGW'; opcode: $E3; OperandsCount: 2; Operands: (p1:GREGdq ;p2:MODdq));
                               simd_PMULHUW_a: TSIMDInstruction = (name: 'PMULHUW'; opcode: $E4; OperandsCount: 2; Operands: (p1:GREGq ;p2:MODq));
                               simd_PMULHUW_b: TSIMDInstruction = (name: 'PMULHUW'; opcode: $E4; OperandsCount: 2; Operands: (p1:GREGdq ;p2:MODdq));
                               simd_PMULHW_b: TSIMDInstruction = (name: 'PMULHW'; opcode: $E5; OperandsCount: 2; Operands: (p1:GREGdq ;p2:MODdq));
                               simd_CVTPD2DQ: TSIMDInstruction = (name: 'CVTPD2DQ'; opcode: $E6; OperandsCount: 2; Operands: (p1:GREGdq ;p2:MODdq));
// 210..219
                               simd_CVTTPD2DQ: TSIMDInstruction = (name: 'CVTTPD2DQ'; opcode: $E6; OperandsCount: 2; Operands: (p1:GREGdq ;p2:MODdq));
                               simd_CVTDQ2PD: TSIMDInstruction = (name: 'CVTDQ2PD'; opcode: $E6; OperandsCount: 2; Operands: (p1:GREGdq ;p2:XMM_M64));
                               simd_MOVNTQ: TSIMDInstruction = (name: 'MOVNTQ'; opcode: $E7; OperandsCount: 2; Operands: (p1:M64 ;p2:GREGq));
                               simd_MOVNTDQ: TSIMDInstruction = (name: 'MOVNTDQ'; opcode: $E7; OperandsCount: 2; Operands: (p1:M128 ;p2:GREGdq));
                               simd_PSUBSB_b: TSIMDInstruction = (name: 'PSUBSB'; opcode: $E8; OperandsCount: 2; Operands: (p1:GREGdq ;p2:MODdq));
                               simd_PSUBSW_b: TSIMDInstruction = (name: 'PSUBSW'; opcode: $E9; OperandsCount: 2; Operands: (p1:GREGdq ;p2:MODdq));
                               simd_PMINSW_a: TSIMDInstruction = (name: 'PMINSW'; opcode: $EA; OperandsCount: 2; Operands: (p1:GREGq ;p2:MODq));
                               simd_PMINSW_b: TSIMDInstruction = (name: 'PMINSW'; opcode: $EA; OperandsCount: 2; Operands: (p1:GREGdq ;p2:MODdq));
                               simd_POR_b: TSIMDInstruction = (name: 'POR'; opcode: $EB; OperandsCount: 2; Operands: (p1:GREGdq ;p2:MODdq));
                               simd_PADDSB_b: TSIMDInstruction = (name: 'PADDSB'; opcode: $EC; OperandsCount: 2; Operands: (p1:GREGdq ;p2:MODdq));
// 220..229
                               simd_PADDSW_b: TSIMDInstruction = (name: 'PADDSW'; opcode: $ED; OperandsCount: 2; Operands: (p1:GREGdq ;p2:MODdq));
                               simd_PMAXSW_a: TSIMDInstruction = (name: 'PMAXSW'; opcode: $EE; OperandsCount: 2; Operands: (p1:GREGq ;p2:MODq));
                               simd_PMAXSW_b: TSIMDInstruction = (name: 'PMAXSW'; opcode: $EE; OperandsCount: 2; Operands: (p1:GREGdq ;p2:MODdq));
                               simd_PXOR_b: TSIMDInstruction = (name: 'PXOR'; opcode: $EF; OperandsCount: 2; Operands: (p1:GREGdq ;p2:MODdq));

                               simd_PSLLW_b: TSIMDInstruction = (name: 'PSLLW'; opcode: $F1; OperandsCount: 2; Operands: (p1:GREGdq ;p2:MODdq));
                               simd_PSLLD_b: TSIMDInstruction = (name: 'PSLLD'; opcode: $F2; OperandsCount: 2; Operands: (p1:GREGdq ;p2:MODdq));
                               simd_PSLLQ_b: TSIMDInstruction = (name: 'PSLLQ'; opcode: $F3; OperandsCount: 2; Operands: (p1:GREGdq ;p2:MODdq));
                               simd_PMULUDQ_a: TSIMDInstruction = (name: 'PMULUDQ'; opcode: $F4; OperandsCount: 2; Operands: (p1:GREGq ;p2:MODq));
                               simd_PMULUDQ_b: TSIMDInstruction = (name: 'PMULUDQ'; opcode: $F4; OperandsCount: 2; Operands: (p1:GREGdq ;p2:MODdq));
//                               simd_PMADDWD: TSIMDInstruction = (name: 'PMADDWD'; opcode: $F5; OperandsCount: 2; Operands: (p1:GREGq ;p2:MODq));
// 230..239  odteraz je spravne cislo
                               simd_PMADDWD_b: TSIMDInstruction = (name: 'PMADDWD'; opcode: $F5; OperandsCount: 2; Operands: (p1:GREGdq ;p2:MODdq));
                               simd_PSADBW_a: TSIMDInstruction = (name: 'PSADBW'; opcode: $F6; OperandsCount: 2; Operands: (p1:GREGq ;p2:MODq));
                               simd_PSADBW_b: TSIMDInstruction = (name: 'PSADBW'; opcode: $F6; OperandsCount: 2; Operands: (p1:GREGdq ;p2:MODdq));
                               simd_MASKMOVQ: TSIMDInstruction = (name: 'MASKMOVQ'; opcode: $F7; OperandsCount: 2; Operands: (p1:GREGdq ;p2:MODdq));
                               simd_MASKMOVDQU: TSIMDInstruction = (name: 'MASKMOVDQU'; opcode: $F7; OperandsCount: 2; Operands: (p1:GREGdq ;p2:MODdq));
                               simd_PSUBB_b: TSIMDInstruction = (name: 'PSUBB'; opcode: $F8; OperandsCount: 2; Operands: (p1:GREGdq ;p2:MODdq));
                               simd_PSUBW_b: TSIMDInstruction = (name: 'PSUBW'; opcode: $F9; OperandsCount: 2; Operands: (p1:GREGdq ;p2:MODdq));
                               simd_PSUBD_b: TSIMDInstruction = (name: 'PSUBD'; opcode: $FA; OperandsCount: 2; Operands: (p1:GREGdq ;p2:MODdq));
                               simd_PSUBQ_a: TSIMDInstruction = (name: 'PSUBQ'; opcode: $FB; OperandsCount: 2; Operands: (p1:GREGq ;p2:MODq));
                               simd_PSUBQ_b: TSIMDInstruction = (name: 'PSUBQ'; opcode: $FB; OperandsCount: 2; Operands: (p1:GREGdq ;p2:MODdq));
// 240..242
                               simd_PADDB_b: TSIMDInstruction = (name: 'PADDB'; opcode: $FC; OperandsCount: 2; Operands: (p1:GREGdq ;p2:MODdq));
                               simd_PADDW_b: TSIMDInstruction = (name: 'PADDW'; opcode: $FD; OperandsCount: 2; Operands: (p1:GREGdq ;p2:MODdq));
                               simd_PADDD_b: TSIMDInstruction = (name: 'PADDD'; opcode: $FE; OperandsCount: 2; Operands: (p1:GREGdq ;p2:MODdq));

// GROUP INSTRUCTIONS

// 243..249
                   // GROUP 12:
                               simd_PSRLW_c: TSIMDInstruction = (name: 'PSRLW'; opcode: $0; OperandsCount: 2; Operands: (p1:R64 ;p2:IMMb));
                               simd_PSRLW_d: TSIMDInstruction = (name: 'PSRLW'; opcode: $0; OperandsCount: 2; Operands: (p1:R128 ;p2:IMMb));
                               simd_PSRAW_c: TSIMDInstruction = (name: 'PSRAW'; opcode: $0; OperandsCount: 2; Operands: (p1:R64 ;p2:IMMb));
                               simd_PSRAW_d: TSIMDInstruction = (name: 'PSRAW'; opcode: $0; OperandsCount: 2; Operands: (p1:R128 ;p2:IMMb));
                               simd_PSLLW_c: TSIMDInstruction = (name: 'PSLLW'; opcode: $0; OperandsCount: 2; Operands: (p1:R64 ;p2:IMMb));
                               simd_PSLLW_d: TSIMDInstruction = (name: 'PSLLW'; opcode: $0; OperandsCount: 2; Operands: (p1:R128 ;p2:IMMb));
                   // GROUP 13:
                               simd_PSRLD_c: TSIMDInstruction = (name: 'PSRLD'; opcode: $0; OperandsCount: 2; Operands: (p1:R64 ;p2:IMMb));
// 250..259
                               simd_PSRLD_d: TSIMDInstruction = (name: 'PSRLD'; opcode: $0; OperandsCount: 2; Operands: (p1:R128 ;p2:IMMb));
                               simd_PSRAD_c: TSIMDInstruction = (name: 'PSRAD'; opcode: $0; OperandsCount: 2; Operands: (p1:R64 ;p2:IMMb));
                               simd_PSRAD_d: TSIMDInstruction = (name: 'PSRAD'; opcode: $0; OperandsCount: 2; Operands: (p1:R128 ;p2:IMMb));
                               simd_PSLLD_c: TSIMDInstruction = (name: 'PSLLD'; opcode: $0; OperandsCount: 2; Operands: (p1:R64 ;p2:IMMb));
                               simd_PSLLD_d: TSIMDInstruction = (name: 'PSLLD'; opcode: $0; OperandsCount: 2; Operands: (p1:R128 ;p2:IMMb));
                   // GROUP 14:
                               simd_PSRLQ_c: TSIMDInstruction = (name: 'PSRLQ'; opcode: $0; OperandsCount: 2; Operands: (p1:R64 ;p2:IMMb));
                               simd_PSRLQ_d: TSIMDInstruction = (name: 'PSRLQ'; opcode: $0; OperandsCount: 2; Operands: (p1:R128 ;p2:IMMb));
                               simd_PSRLDQ: TSIMDInstruction = (name: 'PSRLDQ'; opcode: $0; OperandsCount: 2; Operands: (p1:R128 ;p2:IMMb));
                               simd_PSLLQ_c: TSIMDInstruction = (name: 'PSLLQ'; opcode: $0; OperandsCount: 2; Operands: (p1:R64 ;p2:IMMb));
                               simd_PSLLQ_d: TSIMDInstruction = (name: 'PSLLQ'; opcode: $0; OperandsCount: 2; Operands: (p1:R128 ;p2:IMMb));
// 260.
                               simd_PSLLDQ: TSIMDInstruction = (name: 'PSLLDQ'; opcode: $0; OperandsCount: 2; Operands: (p1:R128 ;p2:IMMb));

//END OF MMX, SSE(2) instructions
