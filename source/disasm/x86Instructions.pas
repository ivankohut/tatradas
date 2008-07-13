{ TODO:
  - trojbatove opcode instrukcie
  - skontrolovat 1 batjove opcode instrukcie a 2 batjove non-SIMD instrukcie
}

unit x86Instructions;

interface

uses
  DisassemblerTypes,
  x86DisassemblerTypes;

type

  // "Instruction set" for instructions will maybe be used in future versions
  TInstructionSet = (isGenPur, isMMX, isSSE, isSSE2, isSSE3, isSSSE3, isSSE41, isSSE42, is3DNow, is3DNowExt, isFPU, isFPUStateMng, isTestReg);  // isVMX, is64bit

  TInstructionType = (itUndefined, itOrdinary, itSIMD, itFPU, itGroup);

  PSIMDInstruction = ^TSIMDInstruction;

  TInstruction = record        // Hlavny typ instrukcie
    name:string[20];           // Meno instrukcie(mnemonic)
    typ: TInstructionType;
    opcode: byte;              // Operacny kod
      AddOp:boolean;           // Name16 and  name32 depends on Address(true) or Operand(false) size attribut
        name16: string[10];
        name32: string[10];
    pc:byte;                   // Pocet operandov(parametrov)
      param: TParam;           // Operandy(parametre) instrukcie
//    mmx1,mmx2,mmx3,mmx4: word;       // Indexy MMX instrukcii v tabulke instrukcii MMX(+SSE,+SSE2)
    SIMD, SIMD_66, SIMD_F2, SIMD_F3: PSIMDInstruction;
    InstrSet: TInstructionSet;
  end;

  TSIMDInstruction = record
    Name: string[20];
    Opcode: word;
    OperandsCount: byte;
    Operands: TParam;
    InstrSet: TInstructionSet;
  end;

  TMMXInstruction = record
    name:string[20];
    opcode:word;
    pc:byte;
      param: Tparam;
  end;


  TFPUAInstruction = record
    name: string[20];
    par: TSize;
  end;

  TFPUBInstruction = record
    name: string[20];
    par: string;
  end;

  TFPUInstructionGroup = array [$C0..$FF] of TFPUBInstruction;



  TPrefixes = record
    pF0, pF2, pF3: boolean;
    p2E, p36, p3E, p26, p64, p65: boolean;
    p66, p67: boolean;
  end;

  TGroupInstructions = array [0..7] of TInstruction;

const
  PocetOBOInstrukcii = 256;
  PocetTBOInstrukcii = 256;
  PocetOBOGroupInstrukcii = 128;
  PocetTBOGroupInstrukcii = 30;
  //PocetFPUInstrukcii = 73;
  PocetMMXInstrukcii = 260; //47;
  Pocet3DNowInstrukcii = 24;


{
 Instrukcie skokove:
   JMP - opcody E9 (rel8), EB( rel16/32)
   Jxx - opcody 70 - 7F (rel8), 0F 80 - 0F 8F (rel16/32)
   JCXZ - E3 (rel8)
   LOOPxx - opcody E0 (rel8), E1 (rel8), E2 (rel8)
   CALL - opcode E8 (rel16/32)

 Instrukcie ukoncujuce:
   JMP - vsetky opcody
   RET - vsetky opcody
   IRET - vsetky opcody

}

//const

// Opcode $FE - Group Instruction
// Opcode $D8 - FPU Instruction
// Opcode $66 - Instruction Prefix
// Opcode $D6 - Undefined Opcode
// Other opcodes - OneByte Opcode Instruction

(*
       OBOInstruction_set:array[0..PocetOBOInstrukcii-1] of TInstruction = (
// $00-$0F
                               (name: 'ADD'; opcode: $00; pc:2; param: (p1:MODb; p2:GREGb)),
                               (name: 'ADD'; opcode: $01; pc:2; param: (p1:MODv; p2:GREGv)),
                               (name: 'ADD'; opcode: $02; pc:2; param: (p1:GREGb; p2:MODb)),
                               (name: 'ADD'; opcode: $03; pc:2; param: (p1:GREGv; p2:MODv)),
                               (name: 'ADD'; opcode: $04; pc:2; param: (p1:AL; p2:IMMb)),
                               (name: 'ADD'; opcode: $05; pc:2; param: (p1:AX; p2:IMMv)),
                               (name: 'PUSH'; opcode: $06; pc:1; param: (p1:ES)),
                               (name: 'POP'; opcode: $07; pc:1; param: (p1:ES)),
                               (name: 'OR'; opcode: $08; pc:2; param: (p1:MODb; p2:GREGb)),
                               (name: 'OR'; opcode: $09; pc:2; param: (p1:MODv; p2:GREGv)),
                               (name: 'OR'; opcode: $0A; pc:2; param: (p1:GREGb; p2:MODb)),
                               (name: 'OR'; opcode: $0B; pc:2; param: (p1:GREGv; p2:MODv)),
                               (name: 'OR'; opcode: $0C; pc:2; param: (p1:AL; p2:IMMb)),
                               (name: 'OR'; opcode: $0D; pc:2; param: (p1:AX; p2:IMMv)),
                               (name: 'PUSH'; opcode: $0E; pc:1; param: (p1:CS)),
                               (name: '2Byte'; opcode: $0F; pc:1; param: (p1:SS)),
// $10-$1F
                               (name: 'ADC'; opcode: $10; pc:2; param: (p1:MODb; p2:GREGb)),
                               (name: 'ADC'; opcode: $11; pc:2; param: (p1:MODv; p2:GREGv)),
                               (name: 'ADC'; opcode: $12; pc:2; param: (p1:GREGb; p2:MODb)),
                               (name: 'ADC'; opcode: $13; pc:2; param: (p1:GREGv; p2:MODv)),
                               (name: 'ADC'; opcode: $14; pc:2; param: (p1:AL; p2:IMMb)),
                               (name: 'ADC'; opcode: $15; pc:2; param: (p1:AX; p2:IMMv)),
                               (name: 'PUSH'; opcode: $16; pc:1; param: (p1:SS)),
                               (name: 'POP'; opcode: $17; pc:1; param: (p1:SS)),
                               (name: 'SBB'; opcode: $18; pc:2; param: (p1:MODb; p2:GREGb)),
                               (name: 'SBB'; opcode: $19; pc:2; param: (p1:MODv; p2:GREGv)),
                               (name: 'SBB'; opcode: $1A; pc:2; param: (p1:GREGb; p2:MODb)),
                               (name: 'SBB'; opcode: $1B; pc:2; param: (p1:GREGv; p2:MODv)),
                               (name: 'SBB'; opcode: $1C; pc:2; param: (p1:AL; p2:IMMb)),
                               (name: 'SBB'; opcode: $1D; pc:2; param: (p1:AX; p2:IMMv)),
                               (name: 'PUSH'; opcode: $1E; pc:1; param: (p1:DS)),
                               (name: 'POP'; opcode: $1F; pc:1; param: (p1:DS)),
// $20-$2F
                               (name: 'AND'; opcode: $20; pc:2; param: (p1:MODb; p2:GREGb)),
                               (name: 'AND'; opcode: $21; pc:2; param: (p1:MODv; p2:GREGv)),
                               (name: 'AND'; opcode: $22; pc:2; param: (p1:GREGb; p2:MODb)),
                               (name: 'AND'; opcode: $23; pc:2; param: (p1:GREGv; p2:MODv)),
                               (name: 'AND'; opcode: $24; pc:2; param: (p1:AL; p2:IMMb)),
                               (name: 'AND'; opcode: $25; pc:2; param: (p1:AX; p2:IMMv)),
                               (name: 'prefix'; opcode: $66; pc:2; param: (p1:AL; p2:IMMb)),
                               (name: 'DAA'; opcode: $27; param:()),
                               (name: 'SUB'; opcode: $28; pc:2; param: (p1:MODb; p2:GREGb)),
                               (name: 'SUB'; opcode: $29; pc:2; param: (p1:MODv; p2:GREGv)),
                               (name: 'SUB'; opcode: $2A; pc:2; param: (p1:GREGb; p2:MODb)),
                               (name: 'SUB'; opcode: $2B; pc:2; param: (p1:GREGv; p2:MODv)),
                               (name: 'SUB'; opcode: $2C; pc:2; param: (p1:AL; p2:IMMb)),
                               (name: 'SUB'; opcode: $2D; pc:2; param: (p1:AX; p2:IMMv)),
                               (name: 'prefix'; opcode: $66; pc:2; param: (p1:AX; p2:IMMv)),
                               (name: 'DAS'; opcode: $2F; param: ()),
// $30-$3F
                               (name: 'XOR'; opcode: $30; pc:2; param: (p1:MODb; p2:GREGb)),
                               (name: 'XOR'; opcode: $31; pc:2; param: (p1:MODv; p2:GREGv)),
                               (name: 'XOR'; opcode: $32; pc:2; param: (p1:GREGb; p2:MODb)),
                               (name: 'XOR'; opcode: $33; pc:2; param: (p1:GREGv; p2:MODv)),
                               (name: 'XOR'; opcode: $34; pc:2; param: (p1:AL; p2:IMMb)),
                               (name: 'XOR'; opcode: $35; pc:2; param: (p1:AX; p2:IMMv)),
                               (name: 'prefix'; opcode: $66; pc:2; param: (p1:AX; p2:IMMv)),
                               (name: 'AAA'; opcode: $37; pc:0;),
                               (name: 'CMP'; opcode: $38; pc:2; param: (p1:MODb; p2:GREGb)),
                               (name: 'CMP'; opcode: $39; pc:2; param: (p1:MODv; p2:GREGv)),
                               (name: 'CMP'; opcode: $3A; pc:2; param: (p1:GREGb; p2:MODb)),
                               (name: 'CMP'; opcode: $3B; pc:2; param: (p1:GREGv; p2:MODv)),
                               (name: 'CMP'; opcode: $3C; pc:2; param: (p1:AL; p2:IMMb)),
                               (name: 'CMP'; opcode: $3D; pc:2; param: (p1:AX; p2:IMMv)),
                               (name: 'prefix'; opcode: $66; pc:2; param: (p1:AX; p2:IMMv)),
                               (name: 'AAS'; opcode: $3F; pc:0),
// $40-$4F
                               (name: 'INC'; opcode: $40; pc:1; param: (p1:AX)),
                               (name: 'INC'; opcode: $41; pc:1; param: (p1:CX)),
                               (name: 'INC'; opcode: $42; pc:1; param: (p1:DX)),
                               (name: 'INC'; opcode: $43; pc:1; param: (p1:BX)),
                               (name: 'INC'; opcode: $44; pc:1; param: (p1:SP)),
                               (name: 'INC'; opcode: $45; pc:1; param: (p1:BP)),
                               (name: 'INC'; opcode: $46; pc:1; param: (p1:SI)),
                               (name: 'INC'; opcode: $47; pc:1; param: (p1:DI)),
                               (name: 'DEC'; opcode: $48; pc:1; param: (p1:AX)),
                               (name: 'DEC'; opcode: $49; pc:1; param: (p1:CX)),
                               (name: 'DEC'; opcode: $4A; pc:1; param: (p1:DX)),
                               (name: 'DEC'; opcode: $4B; pc:1; param: (p1:BX)),
                               (name: 'DEC'; opcode: $4C; pc:1; param: (p1:SP)),
                               (name: 'DEC'; opcode: $4D; pc:1; param: (p1:BP)),
                               (name: 'DEC'; opcode: $4E; pc:1; param: (p1:SI)),
                               (name: 'DEC'; opcode: $4F; pc:1; param: (p1:DI)),
// $50-$5F
                               (name: 'PUSH'; opcode: $50; pc:1; param: (p1:AX)),
                               (name: 'PUSH'; opcode: $51; pc:1; param: (p1:CX)),
                               (name: 'PUSH'; opcode: $52; pc:1; param: (p1:DX)),
                               (name: 'PUSH'; opcode: $53; pc:1; param: (p1:BX)),
                               (name: 'PUSH'; opcode: $54; pc:1; param: (p1:SP)),
                               (name: 'PUSH'; opcode: $55; pc:1; param: (p1:BP)),
                               (name: 'PUSH'; opcode: $56; pc:1; param: (p1:SI)),
                               (name: 'PUSH'; opcode: $57; pc:1; param: (p1:DI)),
                               (name: 'POP'; opcode: $58; pc:1; param: (p1:AX)),
                               (name: 'POP'; opcode: $59; pc:1; param: (p1:CX)),
                               (name: 'POP'; opcode: $5A; pc:1; param: (p1:DX)),
                               (name: 'POP'; opcode: $5B; pc:1; param: (p1:BX)),
                               (name: 'POP'; opcode: $5C; pc:1; param: (p1:SP)),
                               (name: 'POP'; opcode: $5D; pc:1; param: (p1:BP)),
                               (name: 'POP'; opcode: $5E; pc:1; param: (p1:SI)),
                               (name: 'POP'; opcode: $5F; pc:1; param: (p1:DI)),
// $60-$6F
                               (name: 'PUSHA/PUSHAD'; opcode: $60; addop:true; name16:'PUSHA'; name32:'PUSHAD'),
                               (name: 'POPA/POPAD'; opcode: $61; addop:true; name16:'POPA'; name32:'POPAD'),
{???}                          (name: 'BOUND'; opcode: $62; pc:2; param:(p1:GREGv; p2:MODv)),
                               (name: 'ARPL'; opcode: $63; pc:2; param: (p1:MODw; p2:GREGw)),
                               (name: 'prefix'; opcode: $66; pc:2; param: (p1:MODw; p2:GREGw)),
                               (name: 'prefix'; opcode: $66; pc:2; param: (p1:MODw; p2:GREGw)),
                               (name: 'prefix'; opcode: $66; pc:2; param: (p1:MODw; p2:GREGw)),
                               (name: 'prefix'; opcode: $66; pc:2; param: (p1:MODw; p2:GREGw)),
                               (name: 'PUSH'; opcode: $68; pc:1; param: (p1:IMMv)),
                               (name: 'IMUL'; opcode: $69; pc:3; param: (p1:GREGv; p2:MODv; p3:IMMv)),
                               (name: 'PUSH'; opcode: $6A; pc:1; param: (p1:IMMb)),
                               (name: 'IMUL'; opcode: $6B; pc:3; param: (p1:GREGv; p2:MODv; p3:IMMb)),
                               (name: 'INSB'; opcode: $6C),
                               (name: 'INSW/INSD'; opcode: $6D; addop:true; name16:'INSW'; name32:'INSD'),
                               (name: 'OUTSB'; opcode: $6E),
                               (name: 'OUTSW/OUTSD'; opcode: $6F; addop:true; name16:'OUTSW'; name32:'OUTSD'),
// $70-$7F
                               (name: 'JO';  opcode: $70; pc:1; param: (p1:RELb)),
                               (name: 'JNO'; opcode: $71; pc:1; param: (p1:RELb)),
                               (name: 'JB';  opcode: $72; pc:1; param: (p1:RELb)),
                               (name: 'JNB'; opcode: $73; pc:1; param: (p1:RELb)),
                               (name: 'JE';  opcode: $74; pc:1; param: (p1:RELb)),
                               (name: 'JNE'; opcode: $75; pc:1; param: (p1:RELb)),
                               (name: 'JNA'; opcode: $76; pc:1; param: (p1:RELb)),
                               (name: 'JA';  opcode: $77; pc:1; param: (p1:RELb)),
                               (name: 'JS';  opcode: $78; pc:1; param: (p1:RELb)),
                               (name: 'JNS'; opcode: $79; pc:1; param: (p1:RELb)),
                               (name: 'JP';  opcode: $7A; pc:1; param: (p1:RELb)),
                               (name: 'JNP'; opcode: $7B; pc:1; param: (p1:RELb)),
                               (name: 'JL';  opcode: $7C; pc:1; param: (p1:RELb)),
                               (name: 'JNL'; opcode: $7D; pc:1; param: (p1:RELb)),
                               (name: 'JLE'; opcode: $7E; pc:1; param: (p1:RELb)),
                               (name: 'JG';  opcode: $7F; pc:1; param: (p1:RELb)),
// $80-$8F
                               (name: 'group'; opcode: $FE; pc:1; param: (p1: IMMb)),
                               (name: 'group'; opcode: $FE; pc:1; param: (p1: IMMb)),
                               (name: 'group'; opcode: $FE; pc:1; param: (p1: IMMb)),
                               (name: 'group'; opcode: $FE; pc:1; param: (p1: IMMb)),
                               (name: 'TEST'; opcode: $84; pc:2; param: (p1:MODb; p2:GREGb)),
                               (name: 'TEST'; opcode: $85; pc:2; param: (p1:MODv; p2:GREGv)),
                               (name: 'XCHG'; opcode: $86; pc:2; param: (p1:MODb; p2:GREGb)),
                               (name: 'XCHG'; opcode: $87; pc:2; param: (p1:MODv; p2:GREGv)),
                               (name: 'MOV'; opcode: $88; pc:2; param: (p1:MODb; p2:GREGb)),
                               (name: 'MOV'; opcode: $89; pc:2; param: (p1:MODv; p2:GREGv)),
                               (name: 'MOV'; opcode: $8A; pc:2; param: (p1:GREGb; p2:MODb)),
                               (name: 'MOV'; opcode: $8B; pc:2; param: (p1:GREGv; p2:MODv)),
                               (name: 'MOV'; opcode: $8C; pc:2; param: (p1:MODw; p2:SREGw)),
                               (name: 'LEA'; opcode: $8D; pc:2; param: (p1:GREGv; p2:MODv)),
                               (name: 'MOV'; opcode: $8E; pc:2; param: (p1:SREGw; p2:MODw)),
                               (name: 'POP'; opcode: $8F; pc:1; param: (p1:MODv)),
// $90-$9F
                               (name: 'NOP'; opcode: $90; param: ()),
                               (name: 'XCHG'; opcode: $91; pc:2; param: (p1:AX; p2:CX)),
                               (name: 'XCHG'; opcode: $92; pc:2; param: (p1:AX; p2:DX)),
                               (name: 'XCHG'; opcode: $93; pc:2; param: (p1:AX; p2:BX)),
                               (name: 'XCHG'; opcode: $94; pc:2; param: (p1:AX; p2:SP)),
                               (name: 'XCHG'; opcode: $95; pc:2; param: (p1:AX; p2:BP)),
                               (name: 'XCHG'; opcode: $96; pc:2; param: (p1:AX; p2:SI)),
                               (name: 'XCHG'; opcode: $97; pc:2; param: (p1:AX; p2:DI)),
                               (name: 'CBW/CWDE'; opcode: $98; addop:true; name16:'CBW'; name32:'CWDE'),
                               (name: 'CWD/CDQ'; opcode: $99; addop:true; name16:'CWD'; name32: 'CDQ'),
{???}                          (name: 'CALL'; opcode: $9A; pc:1; param: (p1: IMMp)),
                               (name: 'WAIT'; opcode: $9B),
                               (name: 'PUSHF/PUSHFD'; opcode: $9C; addop:true; name16:'PUSHF'; name32:'PUSHFD'),
                               (name: 'POPF/POPFD'; opcode: $9D; addop:true; name16:'POPF'; name32:'POPFD'),
                               (name: 'SAHF'; opcode: $9E; param: ()),
                               (name: 'LAHF'; opcode: $9F; param: ()),
// $A0-$AF
                               (name: 'MOV'; opcode: $A0; pc:2; param: (p1:AL; p2:OFFb)),
                               (name: 'MOV'; opcode: $A1; pc:2; param: (p1:AX; p2:OFFv)),
                               (name: 'MOV'; opcode: $A2; pc:2; param: (p1:OFFb; p2:AL)),
                               (name: 'MOV'; opcode: $A3; pc:2; param: (p1:OFFv; p2:AX)),
                               (name: 'MOVSB'; opcode: $A4),
                               (name: 'MOVSW/MOVSD'; opcode: $A5; addop:true; name16:'MOVSW'; name32:'MOVSD'),
                               (name: 'CMPSB'; opcode: $A6),
                               (name: 'CMPSW/CMPSD'; opcode: $A7; addop:true; name16:'CMPSW'; name32: 'CMPSD'),
                               (name: 'TEST'; opcode: $A8; pc:2; param: (p1:AL; p2:IMMb)),
                               (name: 'TEST'; opcode: $A9; pc:2; param: (p1:AX; p2:IMMv)),
                               (name: 'STOSB'; opcode: $AA),
                               (name: 'STOSW/STOSD'; opcode: $AB; addop:true; name16:'STOSW'; name32:'STOSD'),
                               (name: 'LODSB'; opcode: $AC),
                               (name: 'LODSW/LODSD'; opcode: $AD; addop:true; name16:'LODSW'; name32:'LODSD'),
                               (name: 'SCASB'; opcode: $AE),
                               (name: 'SCASW/SCASD'; opcode: $AF; addop:true; name16:'SCASW'; name32:'SCASD'),
// $B0-$BF
                               (name: 'MOV'; opcode: $B0; pc:2; param: (p1:AL; p2:IMMb)),
                               (name: 'MOV'; opcode: $B1; pc:2; param: (p1:CL; p2:IMMb)),
                               (name: 'MOV'; opcode: $B2; pc:2; param: (p1:DL; p2:IMMb)),
                               (name: 'MOV'; opcode: $B3; pc:2; param: (p1:BL; p2:IMMb)),
                               (name: 'MOV'; opcode: $B4; pc:2; param: (p1:AH; p2:IMMb)),
                               (name: 'MOV'; opcode: $B5; pc:2; param: (p1:CH; p2:IMMb)),
                               (name: 'MOV'; opcode: $B6; pc:2; param: (p1:DH; p2:IMMb)),
                               (name: 'MOV'; opcode: $B7; pc:2; param: (p1:BH; p2:IMMb)),
                               (name: 'MOV'; opcode: $B8; pc:2; param: (p1:AX; p2:IMMv)),
                               (name: 'MOV'; opcode: $B9; pc:2; param: (p1:CX; p2:IMMv)),
                               (name: 'MOV'; opcode: $BA; pc:2; param: (p1:DX; p2:IMMv)),
                               (name: 'MOV'; opcode: $BB; pc:2; param: (p1:BX; p2:IMMv)),
                               (name: 'MOV'; opcode: $BC; pc:2; param: (p1:SP; p2:IMMv)),
                               (name: 'MOV'; opcode: $BD; pc:2; param: (p1:BP; p2:IMMv)),
                               (name: 'MOV'; opcode: $BE; pc:2; param: (p1:SI; p2:IMMv)),
                               (name: 'MOV'; opcode: $BF; pc:2; param: (p1:DI; p2:IMMv)),
// $C0-$CF
                               (name: 'group'; opcode: $FE; pc:1; param: (p1: IMMb)),
                               (name: 'group'; opcode: $FE; pc:1; param: (p1: IMMb)),
                               (name: 'RETN'; opcode: $C2; pc:1; param: (p1:IMMw)),
                               (name: 'RETN'; opcode: $C3; ),
                               (name: 'LES'; opcode: $C4; pc:2; param: (p1:GREGv; p2:MODp)),
                               (name: 'LDS'; opcode: $C5; pc:2; param: (p1:GREGv; p2:MODp)),
                               (name: 'group'; opcode: $FE; pc:1; param: (p1: IMMb)),
                               (name: 'group'; opcode: $FE; pc:1; param: (p1: IMMb)),
                               (name: 'ENTER'; opcode: $C8; pc:2; param:(p1:IMMw; p2:IMMb)),
                               (name: 'LEAVE'; opcode: $C9; param: ()),
                               (name: 'RETF'; opcode: $CA; pc:1; param: (p1:IMMw)),
                               (name: 'RETF'; opcode: $CB; ),
                               (name: 'INT 3'; opcode: $CC; param:()),
                               (name: 'INT'; opcode:   $CD; pc:1; param: (p1: IMMb)),
                               (name: 'INTO'; opcode:  $CE; param: ()),
                               (name: 'IRET/IRETD'; opcode: $CF; addop:true; name16: 'IRET'; name32: 'IRETD'),
// $D0-$DF
                               (name: 'group'; opcode: $FE; pc:1; param: (p1: IMMb)),
                               (name: 'group'; opcode: $FE; pc:1; param: (p1: IMMb)),
                               (name: 'group'; opcode: $FE; pc:1; param: (p1: IMMb)),
                               (name: 'group'; opcode: $FE; pc:1; param: (p1: IMMb)),
                               (name: 'AAM'; opcode: $D4; pc:1; param: (p1: IMMb)),
                               (name: 'AAD'; opcode: $D5; pc:1; param: (p1: IMMb)),
                               (name: 'undefined opcode!'; opcode: $D6; pc:1; param: (p1: IMMb)),
                               (name: 'XLAT/XLATB'; opcode: $D7),
                               (name: 'fpu'; opcode: $D8; pc:1; param: (p1: IMMb)),
                               (name: 'fpu'; opcode: $D8; pc:1; param: (p1: IMMb)),
                               (name: 'fpu'; opcode: $D8; pc:1; param: (p1: IMMb)),
                               (name: 'fpu'; opcode: $D8; pc:1; param: (p1: IMMb)),
                               (name: 'fpu'; opcode: $D8; pc:1; param: (p1: IMMb)),
                               (name: 'fpu'; opcode: $D8; pc:1; param: (p1: IMMb)),
                               (name: 'fpu'; opcode: $D8; pc:1; param: (p1: IMMb)),
                               (name: 'fpu'; opcode: $D8; pc:1; param: (p1: IMMb)),
// $E0-$EF
                               (name: 'LOOPNE'; opcode: $E0; pc:1; param: (p1:RELb)),
                               (name: 'LOOPE';  opcode: $E1; pc:1; param: (p1:RELb)),
                               (name: 'LOOP';   opcode: $E2; pc:1; param: (p1:RELb)),
                               (name: 'JCXZ'; opcode: $E3; addop:true; name16: 'JCXZ'; name32: 'JECXZ'; pc:1; param:(p1: RELb;  )),
                               (name: 'IN'; opcode: $E4; pc:2; param: (p1:AL; p2:IMMb)),
                               (name: 'IN'; opcode: $E5; pc:2; param: (p1:AX; p2:IMMb)),
                               (name: 'OUT'; opcode: $E6; pc:2; param: (p1:IMMb; p2:AL)),
                               (name: 'OUT'; opcode: $E7; pc:2; param: (p1:IMMb; p2:AX)),
                               (name: 'CALL'; opcode: $E8; pc:1; param: (p1: RELv)),
                               (name: 'JMP'; opcode: $E9; pc:1; param: (p1:RELv)),
                               (name: 'JMP'; opcode: $EA; pc:1; param: (p1:IMMp)),
                               (name: 'JMP'; opcode: $EB; pc:1; param: (p1:RELb)),
                               (name: 'IN'; opcode: $EC; pc:2; param: (p1:AL; p2:statDX)),
                               (name: 'IN'; opcode: $ED; pc:2; param: (p1:AX; p2:statDX)),
                               (name: 'OUT'; opcode: $EE; pc:2; param: (p1:statDX; p2:AL)),
                               (name: 'OUT'; opcode: $EF; pc:2; param: (p1:statDX; p2:AX)),
// $F0-$FF
                               (name: 'prefix'; opcode: $66; pc:1; param: (p1: IMMb)),
                               (name: 'undefined opcode'; opcode: $D6),
                               (name: 'prefix'; opcode: $66; pc:1; param: (p1: IMMb)),
                               (name: 'prefix'; opcode: $66; pc:1; param: (p1: IMMb)),
                               (name: 'HLT'; opcode: $F4; param: ()),
                               (name: 'CMC'; opcode: $F5; param:()),
                               (name: 'group3'; opcode: $FE),
                               (name: 'group3'; opcode: $FE),
                               (name: 'CLC'; opcode: $F8; param:()),
                               (name: 'STC'; opcode: $F9),
                               (name: 'CLI'; opcode: $FA; param:()),
                               (name: 'STI'; opcode: $FB),
                               (name: 'CLD'; opcode: $FC; param:()),
                               (name: 'STD'; opcode: $FD),
                               (name: 'group4'; opcode: $FE),
                               (name: 'group5'; opcode: $FE) // nemalo tu byt nahodou $FF - nie, $FE znamena ze je group instr., nie opcode

                                         );
*)



(*
const

// Opcode $FE - Group Instruction
// Opcode $60 - MMX, SSE, SSE2 Instruction
// Opcode $D6 - No Instruction
// Other opcodes - TwoByte Opcode Instruction

           TBOInstruction_set:array[0..PocetTBOInstrukcii-1] of TInstruction = (

// $00-$0F
                               (name: 'group'; opcode: $FE),
                               (name: 'group'; opcode: $FE),
                               (name: 'LAR'; opcode: $02; pc:2; param: (p1:GREGv; p2:MODw)),
                               (name: 'LSL'; opcode: $03; pc:2; param: (p1:GREGv; p2:MODw)),
                               (name: 'undefined opcode!'; opcode: $D6),
                               (name: 'SYSCALL'; opcode: $05), // AMD only
                               (name: 'CLTS'; opcode: $06),
                               (name: 'SYSRET'; opcode: $07), // AMD only
                               (name: 'INVD';  opcode: $08),
                               (name: 'WBINVD'; opcode: $09),
                               (name: 'undefined opcode!'; opcode: $D6),
                               (name: 'UD2'; opcode: $0B),
                               (name: 'undefined opcode!'; opcode: $D6),
                               (name: 'PREFETCH'; opcode: $0D),
                               (name: 'FEMMS'; opcode: $0E),
                               (name: '3DNow!'; opcode: $0F),
// $10-$1F
                               (name: 'mmx'; opcode: $60; mmx1:48; mmx2:50; mmx3:51; mmx4:49),
                               (name: 'mmx'; opcode: $60; mmx1:52; mmx2:54; mmx3:55; mmx4:53),
                               (name: 'mmx'; opcode: $60; mmx1:56; mmx2:57; ),      // chyba > rovnake opcody, pozri opcode map!
                               (name: 'mmx'; opcode: $60; mmx1:59; mmx2:60),
                               (name: 'mmx'; opcode: $60; mmx1:61; mmx2:62),
                               (name: 'mmx'; opcode: $60; mmx1:63; mmx2:64),
                               (name: 'mmx'; opcode: $60; mmx1:65; mmx2:66),        // chyba > rovnake opcody, pozri opcode map!
                               (name: 'mmx'; opcode: $60; mmx1:68; mmx2:69),
                               (name: 'group'; opcode: $FE),
                               (name: 'undefined opcode!'; opcode: $D6),
                               (name: 'undefined opcode!'; opcode: $D6),
                               (name: 'undefined opcode!'; opcode: $D6),
                               (name: 'undefined opcode!'; opcode: $D6),
                               (name: 'undefined opcode!'; opcode: $D6),
                               (name: 'undefined opcode!'; opcode: $D6),
                               (name: 'undefined opcode!'; opcode: $D6),
// $20-$2F
                               (name: 'MOV'; opcode: $20; pc:2; param: (p1:MODd; p2:CREGd)),
                               (name: 'MOV'; opcode: $21; pc:2; param: (p1:MODd; p2:DREGd)),
                               (name: 'MOV'; opcode: $22; pc:2; param: (p1:CREGd; p2:MODd)),
                               (name: 'MOV'; opcode: $23; pc:2; param: (p1:DREGd; p2:MODd)),
                               (name: 'MOV'; opcode: $24; pc:2; param: (p1:MODd; p2:TREGd)),
                               (name: 'undefined opcode!'; opcode: $D6),
                               (name: 'MOV'; opcode: $26; pc:2; param: (p1:TREGd; p2:MODd)),
                               (name: 'undefined opcode!'; opcode: $D6),
                               (name: 'mmx'; opcode: $60; mmx1:70; mmx2:71),
                               (name: 'mmx'; opcode: $60; mmx1:72; mmx2:73),
                               (name: 'mmx'; opcode: $60; mmx1:74; mmx2:76; mmx3:77; mmx4:75),
                               (name: 'mmx'; opcode: $60; mmx1:78; mmx2:79),
                               (name: 'mmx'; opcode: $60; mmx1:80; mmx2:82; mmx3:83; mmx4:81),
                               (name: 'mmx'; opcode: $60; mmx1:84; mmx2:86; mmx3:87; mmx4:85),
                               (name: 'mmx'; opcode: $60; mmx1:88; mmx2:89),
                               (name: 'mmx'; opcode: $60; mmx1:90; mmx2:91),
// $30-$3F
                               (name: 'WRMSR'; opcode: $30),
                               (name: 'RDTSC'; opcode: $31),
                               (name: 'RDMSR'; opcode: $32),
                               (name: 'RDPMC'; opcode: $33),
                               (name: 'SYSENTER'; opcode: $34),
                               (name: 'SYSEXIT'; opcode: $34),
                               (name: 'undefined opcode!'; opcode: $D6),
                               (name: 'undefined opcode!'; opcode: $D6),
                               (name: 'undefined opcode!'; opcode: $D6),
                               (name: 'undefined opcode!'; opcode: $D6),
                               (name: 'undefined opcode!'; opcode: $D6),
                               (name: 'undefined opcode!'; opcode: $D6),
                               (name: 'undefined opcode!'; opcode: $D6),
                               (name: 'undefined opcode!'; opcode: $D6),
                               (name: 'undefined opcode!'; opcode: $D6),
                               (name: 'undefined opcode!'; opcode: $D6),
// $40-$4F
                               (name: 'CMOVO'; opcode: $40; pc:2; param: (p1:GREGv; p2: MODv)),
                               (name: 'CMOVNO'; opcode: $41; pc:2; param: (p1:GREGv; p2: MODv)),
                               (name: 'CMOVB'; opcode: $42; pc:2; param: (p1:GREGv; p2: MODv)),
                               (name: 'CMOVAE'; opcode: $43; pc:2; param: (p1:GREGv; p2: MODv)),
                               (name: 'CMOVE'; opcode: $44; pc:2; param: (p1:GREGv; p2: MODv)),
                               (name: 'CMOVNE'; opcode: $45; pc:2; param: (p1:GREGv; p2: MODv)),
                               (name: 'CMOVBE'; opcode: $46; pc:2; param: (p1:GREGv; p2: MODv)),
                               (name: 'CMOVA'; opcode: $47; pc:2; param: (p1:GREGv; p2: MODv)),
                               (name: 'CMOVS'; opcode: $48; pc:2; param: (p1:GREGv; p2: MODv)),
                               (name: 'CMOVNS'; opcode: $49; pc:2; param: (p1:GREGv; p2: MODv)),
                               (name: 'CMOVP'; opcode: $4A; pc:2; param: (p1:GREGv; p2: MODv)),
                               (name: 'CMOVNP'; opcode: $4B; pc:2; param: (p1:GREGv; p2: MODv)),
                               (name: 'CMOVL'; opcode: $4C; pc:2; param: (p1:GREGv; p2: MODv)),
                               (name: 'CMOVNL'; opcode: $4D; pc:2; param: (p1:GREGv; p2: MODv)),
                               (name: 'CMOVLE'; opcode: $4E; pc:2; param: (p1:GREGv; p2: MODv)),
                               (name: 'CMOVNLE'; opcode: $4F; pc:2; param: (p1:GREGv; p2: MODv)),
// $50-$5F
                               (name: 'mmx'; opcode: $60; mmx1:92; mmx2:93),
                               (name: 'mmx'; opcode: $60; mmx1:94; mmx2:96; mmx3:97; mmx4:95),
                               (name: 'mmx'; opcode: $60; mmx1:98; mmx4:99),
                               (name: 'mmx'; opcode: $60; mmx1:100; mmx4:101),
                               (name: 'mmx'; opcode: $60; mmx1:102; mmx2:103),
                               (name: 'mmx'; opcode: $60; mmx1:104; mmx2:105),
                               (name: 'mmx'; opcode: $60; mmx1:106; mmx2:107),
                               (name: 'mmx'; opcode: $60; mmx1:108; mmx2:109),

                               (name: 'mmx'; opcode: $60; mmx1:110; mmx2:112; mmx3:113; mmx4:111),
                               (name: 'mmx'; opcode: $60; mmx1:114; mmx2:116; mmx3:117; mmx4:115),
                               (name: 'mmx'; opcode: $60; mmx1:118; mmx2:120; mmx3:121; mmx4:119),
                               (name: 'mmx'; opcode: $60; mmx1:122; mmx2:123; mmx4:124),
                               (name: 'mmx'; opcode: $60; mmx1:125; mmx2:127; mmx3:128; mmx4:126),
                               (name: 'mmx'; opcode: $60; mmx1:129; mmx2:131; mmx3:132; mmx4:130),
                               (name: 'mmx'; opcode: $60; mmx1:133; mmx2:135; mmx3:136; mmx4:134),
                               (name: 'mmx'; opcode: $60; mmx1:137; mmx2:139; mmx3:140; mmx4:138),

// $60-$6F
                               (name: 'mmx'; opcode: $60; mmx1:29; mmx2:141), //PUNPCKLBW
                               (name: 'mmx'; opcode: $60; mmx1:30; mmx2:142), //PUNPCKLWD
                               (name: 'mmx'; opcode: $60; mmx1:31; mmx2:143), //PUNPCKLDQ
                               (name: 'mmx'; opcode: $60; mmx1:23; mmx2:144), //PACKSSWB
                               (name: 'mmx'; opcode: $60; mmx1:20; mmx2:145), //PCMPGTB
                               (name: 'mmx'; opcode: $60; mmx1:21; mmx2:146), //PCMPGTW
                               (name: 'mmx'; opcode: $60; mmx1:22; mmx2:147), //PCMPGTD
                               (name: 'mmx'; opcode: $60; mmx1:25; mmx2:148), //PACKUSWB
                               (name: 'mmx'; opcode: $60; mmx1:26; mmx2:149), //PUNPCKHBW
                               (name: 'mmx'; opcode: $60; mmx1:27; mmx2:150), //PUNPCKHWD
                               (name: 'mmx'; opcode: $60; mmx1:28; mmx2:151), //PUNPCKHDQ
                               (name: 'mmx'; opcode: $60; mmx1:24; mmx2:152), //PACKSSDW
                               (name: 'mmx'; opcode: $60; mmx2:153),
                               (name: 'mmx'; opcode: $60; mmx2:154),
                               (name: 'mmx'; opcode: $60; mmx1:44; mmx2:155), //MOVD
                               (name: 'mmx'; opcode: $60; mmx1:46; mmx2:156; mmx4:157), //MOVQ
// $70-$7F
                               (name: 'mmx'; opcode: $60; mmx1:158; mmx2:159; mmx3:161; mmx4:160),
                               (name: 'group'; opcode: $FE),
                               (name: 'group'; opcode: $FE),
                               (name: 'group'; opcode: $FE),
                               (name: 'mmx'; opcode: $60; mmx1:17; mmx2:162), //PCMPEQB
                               (name: 'mmx'; opcode: $60; mmx1:18; mmx2:163), //PCMPEQW
                               (name: 'mmx'; opcode: $60; mmx1:19; mmx2:164), //PCMPEQD
                               (name: 'EMMS'; opcode: $77),
                               (name: 'mmx'; opcode: $60;),      //  ?
                               (name: 'mmx'; opcode: $60;),      //  ?
                               (name: 'mmx'; opcode: $60;),      //  ?
                               (name: 'mmx'; opcode: $60;),      //  ?
                               (name: 'mmx'; opcode: $60;),      //  ?
                               (name: 'mmx'; opcode: $60;),      //  ?
                               (name: 'mmx'; opcode: $60; mmx1:45; mmx2:165; mmx4:166), //MOVD
                               (name: 'mmx'; opcode: $60; mmx1:47; mmx2:167; mmx4:168), //MOVQ
// $80-$8F
                               (name: 'JO';  opcode: $80; pc:1; param:(p1: RELv)),
                               (name: 'JNO'; opcode: $81; pc:1; param:(p1: RELv)),
                               (name: 'JB';  opcode: $82; pc:1; param:(p1: RELv)),
                               (name: 'JNB'; opcode: $83; pc:1; param:(p1: RELv)),
                               (name: 'JE';  opcode: $84; pc:1; param:(p1: RELv)),
                               (name: 'JNE'; opcode: $85; pc:1; param:(p1: RELv)),
                               (name: 'JNA'; opcode: $86; pc:1; param:(p1: RELv)),
                               (name: 'JA';  opcode: $87; pc:1; param:(p1: RELv)),
                               (name: 'JS';  opcode: $88; pc:1; param:(p1: RELv)),
                               (name: 'JNS'; opcode: $89; pc:1; param:(p1: RELv)),
                               (name: 'JP';  opcode: $8A; pc:1; param:(p1: RELv)),
                               (name: 'JNP'; opcode: $8B; pc:1; param:(p1: RELv)),
                               (name: 'JL';  opcode: $8C; pc:1; param:(p1: RELv)),
                               (name: 'JNL'; opcode: $8D; pc:1; param:(p1: RELv)),
                               (name: 'JLE'; opcode: $8E; pc:1; param:(p1: RELv)),
                               (name: 'JG';  opcode: $8F; pc:1; param:(p1: RELv)),
// $90-$9F
                               (name: 'SETO';  opcode: $90; pc:1; param:(p1: MODb)),
                               (name: 'SETNO';  opcode: $91; pc:1; param:(p1: MODb)),
                               (name: 'SETB';  opcode: $92; pc:1; param:(p1: MODb)),
                               (name: 'SETAE';  opcode: $93; pc:1; param:(p1: MODb)),
                               (name: 'SETE';  opcode: $94; pc:1; param:(p1: MODb)),
                               (name: 'SETNE';  opcode: $95; pc:1; param:(p1: MODb)),
                               (name: 'SETBE';  opcode: $96; pc:1; param:(p1: MODb)),
                               (name: 'SETA';  opcode: $97; pc:1; param:(p1: MODb)),
                               (name: 'SETS';  opcode: $98; pc:1; param:(p1: MODb)),
                               (name: 'SETNS';  opcode: $99; pc:1; param:(p1: MODb)),
                               (name: 'SETP';  opcode: $9A; pc:1; param:(p1: MODb)),
                               (name: 'SETNP';  opcode: $9B; pc:1; param:(p1: MODb)),
                               (name: 'SETL';  opcode: $9C; pc:1; param:(p1: MODb)),
                               (name: 'SETNL';  opcode: $9D; pc:1; param:(p1: MODb)),
                               (name: 'SETLE';  opcode: $9E; pc:1; param:(p1: MODb)),
                               (name: 'SETNLE';  opcode: $9F; pc:1; param:(p1: MODb)),
// $A0-$AF
                               (name: 'PUSH';  opcode: $A0; pc:1; param:(p1: FS)),
                               (name: 'POP';  opcode: $A1; pc:1; param:(p1: FS)),
                               (name: 'CPUID'; opcode: $A2),
                               (name: 'BT'; opcode: $A3; pc:2; param: (p1:MODv; p2: GREGv)),
                               (name: 'SHLD'; opcode: $A4; pc:3; param: (p1:MODv; p2:GREGv; p3:IMMb)),
                               (name: 'SHLD'; opcode: $A5; pc:3; param: (p1:MODv; p2:GREGv; p3:CL)),
                               (name: 'undefined opcode!'; opcode: $D6),
                               (name: 'undefined opcode!'; opcode: $D6),
                               (name: 'PUSH';  opcode: $A8; pc:1; param:(p1: GS)),
                               (name: 'POP';  opcode: $A9; pc:1; param:(p1: GS)),
                               (name: 'RSM'; opcode: $AA),
                               (name: 'BTS'; opcode: $AB; pc:2; param: (p1:MODv; p2: GREGv)),
                               (name: 'SHRD'; opcode: $AC; pc:3; param: (p1:MODv; p2:GREGv; p3:IMMb)),
                               (name: 'SHRD'; opcode: $AD; pc:3; param: (p1:MODv; p2:GREGv; p3:CL)),
                               (name: 'group'; opcode: $FE),
                               (name: 'IMUL'; opcode: $AF; pc:2; param: (p1:Gregv; p2: MODv)),
// $B0-$BF
                               (name: 'CMPXCHG'; opcode: $B0; pc:2; param: (p1:MODb; p2:GREGb)),  //486
                               (name: 'CMPXCHG'; opcode: $B1; pc:2; param: (p1:MODv; p2:GREGv)),  //486
                               (name: 'LSS'; opcode: $B2; pc:2; param: (p1:GREGv; p2:MODv)),
                               (name: 'BTR'; opcode: $B3; pc:2; param: (p1:MODv; p2: GREGv)),
                               (name: 'LFS'; opcode: $B4; pc:1; param: (p1:MODp)),
                               (name: 'LGS'; opcode: $B5; pc:1; param: (p1:MODp)),
                               (name: 'MOVZX'; opcode: $B6; pc:2; param: (p1:GREGv; p2:MODb)),
                               (name: 'MOVZX'; opcode: $B7; pc:2; param: (p1:GREGv; p2:MODw)),
                               (name: 'undefined opcode!'; opcode: $D6),
                               (name: 'group'; opcode: $FE),
                               (name: 'group'; opcode: $FE),
                               (name: 'BTC'; opcode: $BB; pc:2; param: (p1:MODv; p2: GREGv)),
                               (name: 'BSF'; opcode: $BC; pc:2; param: (p1: GREGv; p2: MODv)),
                               (name: 'BSR'; opcode: $BD; pc:2; param: (p1: GREGv; p2: MODv)),
                               (name: 'MOVSX'; opcode: $BE; pc:2; param: (p1:GREGv; p2:MODb)),
                               (name: 'MOVSX'; opcode: $BF; pc:2; param: (p1:GREGv; p2:MODw)),
// $C0-$CF
                               (name: 'XADD'; opcode: $C0; pc:2; param: (p1:MODb; p2:GREGb)),
                               (name: 'XADD'; opcode: $C1; pc:2; param: (p1:MODv; p2:GREGv)),
                               (name: 'mmx'; opcode: $60; mmx1:169; mmx2:171; mmx3:172; mmx4:170),
                               (name: 'mmx'; opcode: $60; mmx1:173),
                               (name: 'mmx'; opcode: $60; mmx1:174; mmx2:175),
                               (name: 'mmx'; opcode: $60; mmx1:176; mmx2:177),
                               (name: 'mmx'; opcode: $60; mmx1:178; mmx2:179),
                               (name: 'group'; opcode: $FE),
                               (name: 'BSWAP'; opcode: $C8 ; pc:1; param: (p1: AX)), //   486
                               (name: 'BSWAP'; opcode: $C9 ; pc:1; param: (p1: CX)),
                               (name: 'BSWAP'; opcode: $CA ; pc:1; param: (p1: DX)),
                               (name: 'BSWAP'; opcode: $CB ; pc:1; param: (p1: BX)),
                               (name: 'BSWAP'; opcode: $CC ; pc:1; param: (p1: SP)),
                               (name: 'BSWAP'; opcode: $CD ; pc:1; param: (p1: BP)),
                               (name: 'BSWAP'; opcode: $CE ; pc:1; param: (p1: SI)),
                               (name: 'BSWAP'; opcode: $CF ; pc:1; param: (p1: DI)),
// $D0-$DF
                               (name: 'undefined opcode!'; opcode: $D6),
                               (name: 'mmx'; opcode: $60; mmx1:39; mmx2:180), //PSRLW
                               (name: 'mmx'; opcode: $60; mmx1:40; mmx2:181), //PSRLD
                               (name: 'mmx'; opcode: $60; mmx1:41; mmx2:182), //PSRLQ
                               (name: 'mmx'; opcode: $60; mmx1:183; mmx2:184),
                               (name: 'mmx'; opcode: $60; mmx1:14; mmx2:185), //PMULLW
                               (name: 'mmx'; opcode: $60; mmx2:186; mmx3:188; mmx4:187),
                               (name: 'mmx'; opcode: $60; mmx1:189; mmx2:190),
                               (name: 'mmx'; opcode: $60; mmx1:12; mmx2:191), //PSUBUSB
                               (name: 'mmx'; opcode: $60; mmx1:13; mmx2:192), //PSUBUSW
                               (name: 'mmx'; opcode: $60; mmx1:193; mmx2:194),
                               (name: 'mmx'; opcode: $60; mmx1:32; mmx2:195), //PAND
                               (name: 'mmx'; opcode: $60; mmx1:5; mmx2:196), //PADDUSB
                               (name: 'mmx'; opcode: $60; mmx1:6; mmx2:197), //PADDUSW
                               (name: 'mmx'; opcode: $60; mmx1:198; mmx2:199),
                               (name: 'mmx'; opcode: $60; mmx1:33; mmx2:200), //PANDN
// $E0-$EF
                               (name: 'mmx'; opcode: $60; mmx1:201; mmx2:202),
                               (name: 'mmx'; opcode: $60; mmx1:42; mmx2:203), //PSRAW
                               (name: 'mmx'; opcode: $60; mmx1:43; mmx2:204), //PSRAD
                               (name: 'mmx'; opcode: $60; mmx1:205; mmx2:206),
                               (name: 'mmx'; opcode: $60; mmx1:207; mmx2:208),
                               (name: 'mmx'; opcode: $60; mmx1:15; mmx2:209), //PMULHW
                               (name: 'mmx'; opcode: $60; mmx2:211; mmx3:210; mmx4:212),
                               (name: 'mmx'; opcode: $60; mmx1:213; mmx2:214),
                               (name: 'mmx'; opcode: $60; mmx1:10; mmx2:215), //PSUBSB
                               (name: 'mmx'; opcode: $60; mmx1:11; mmx2:216), //PSUBSW
                               (name: 'mmx'; opcode: $60; mmx1:217; mmx2:218),
                               (name: 'mmx'; opcode: $60; mmx1:34; mmx2:219), //POR
                               (name: 'mmx'; opcode: $60; mmx1:3; mmx2:220),  //PADDSB
                               (name: 'mmx'; opcode: $60; mmx1:4; mmx2:221),  //PADDSW
                               (name: 'mmx'; opcode: $60; mmx1:222; mmx2:223),
                               (name: 'mmx'; opcode: $60; mmx1:35; mmx2:224), //PXOR
// $F0-$FF
                               (name: 'undefined opcode!'; opcode: $D6),
                               (name: 'mmx'; opcode: $60; mmx1:36; mmx2:225), //PSLLW
                               (name: 'mmx'; opcode: $60; mmx1:37; mmx2:226), //PSLLD
                               (name: 'mmx'; opcode: $60; mmx1:38; mmx2:227), //PSLLQ
                               (name: 'mmx'; opcode: $60; mmx1:228; mmx2:229),
                               (name: 'mmx'; opcode: $60; mmx1:16; mmx2:230), //PMADDWD
                               (name: 'mmx'; opcode: $60; mmx1:231; mmx2:232),
                               (name: 'mmx'; opcode: $60; mmx1:233; mmx2:234),
                               (name: 'mmx'; opcode: $60; mmx1:7; mmx2:235), //PSUBB
                               (name: 'mmx'; opcode: $60; mmx1:8; mmx2:236), //PSUBW
                               (name: 'mmx'; opcode: $60; mmx1:9; mmx2:237),         //PSUBD
                               (name: 'mmx'; opcode: $60; mmx1:238; mmx2:239),         // ???? ($FB)
                               (name: 'mmx'; opcode: $60; mmx1:0; mmx2:240),  //PADDB,
                               (name: 'mmx'; opcode: $60; mmx1:1; mmx2:241),  //PADDW
                               (name: 'mmx'; opcode: $60; mmx1:2; mmx2:242),  //PADDD
                               (name: 'undefined opcode!'; opcode: $D6)

//                               (name: 'group'; opcode: $FE),
//                               (name: ''; opcode: $),
                                           );
*)
{$I x86Instructions_SIMD.inc}
{$I x86Instructions_Group.inc}
{$I x86Instructions_3DNow.inc}
{$I x86Instructions_TwoByteOpcode.inc}
{$I x86Instructions_ThreeByteOpcode_38.inc}
{$I x86Instructions_x87.inc}
{$I x86Instructions_OneByteOpcode.inc}

(*
{$I x86Instructions_SIMD.inc}
{$I x86Instructions_Group.inc}
{$I x86Instructions_OneByteOpcode.inc}
{$I x86Instructions_TwoByteOpcode.inc}
{$I x86Instructions_ThreeByteOpcode_38.inc}
{$I x86Instructions_ThreeByteOpcode_3A.inc}
{$I x86Instructions_x87.inc}
{$I x86Instructions_3DNow.inc}
*)

implementation

end.
