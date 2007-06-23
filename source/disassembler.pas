{ TODO:

 INFO - PUNPCKLBW operandy (druhy), pozri Intel manual, 19.12.2004 zmeneny na MODq, asi by to chcelo vyriesit nejako inac (spolu s MOVZX atd)
}
unit disassembler;

{$INCLUDE 'delver.inc'}

interface

uses
  Classes,
  SysUtils,
  Math,

  procmat,
  CallsAndJumpsTableUnit;

{$INCLUDE 'disassembler.inc'}


type
     TParameter = (

     MODb,  MODw,  MODv,  MODd,  MODp,  MODq,  MODt, MODdq,  // Operand urceny pomocou ModRM (adresa alebo General register)
     GREGb, GREGw, GREGv, GREGd,        GREGq, GREGdq, // General register urceny ModRM (reg field)
     SREGw, CREGd, DREGd, TREGd,                       // Segment, Control, Debug, Test register
     IMMb,  IMMw,  IMMv,         IMMp,                 // Immediate
     RELb,  RELw,  RELv,                               // Relativny immediate
     OFFb,         OFFv,

     REG32_M16,
     XMM_M32, XMM_M64,
     M32, M64, M128,                                   // prave adresa urcena ModRM (r/m)
     R32, R64, R128,                                   // prave register urceny ModRM (r/m)
     ax,bx,cx,dx,si,di,bp,sp,                          // general registre - dynamicke (menia sa na Exx v priprade 32 bit)
     statDX,                                          // staticky register (neovplyvnuje ho 16/32 bit oper ani adresy)
     al,bl,cl,dl,ah,bh,ch,dh,                          // casti gen. registrov
     DS,ES,SS,CS,FS,GS,                                // segment registre
     mm0,mm1,mm2,mm3,mm4,mm5,mm6,mm7,                  // MMX registre
     xmm0,xmm1,xmm2,xmm3,xmm4,xmm5,xmm6,xmm7,          // XMM registre (SSE)
     n1
     );

     TFPUParameter =  (st1, //st(0),st(0)..st(0),st(7)
                       st2, //st(0),st(0)..st(7),st(0)
                       st3, //st(0)..st(7)
                       none
                       );



     TParam = record
       p1, p2, p3:TParameter;
     end;

     TDisassembledItem = record
       address: cardinal;
       parsed: string;
       prefix: string;
       name: string;
       operandy: string;
//       flag:byte;       // 00-none, 01-sucast inej instrukcie, 02-data, 03-import, 04-export

       refercount: integer;
       refer: array of string;
     end;


     modrmparametertype = (reg, greg, sreg, creg, dreg, treg, regmem, mem);
     modrmparametersize = (onebyte, twobyte, twoorfour, fourbyte, fourorsix, mmx, xmm, R32_M16, R128_M32, R128_M64);


     TModRM = record
       fullmodrm: byte;
       moder,regop,rm: byte;
       loaded: boolean;
     end;


     TSIB = record
       fullsib:byte;
       scale,index,base:byte;
       loaded:boolean;
     end;


     TStatistics = record
       Instructions: cardinal;     // pocet instrukcii
       Data: cardinal;             // pocet 'db' prvkov
       References: cardinal;       // pocet riadkov, ktore treba vyhradit pre referencie skokov
       Blanks: cardinal;
       InstructionBytes: cardinal;
       FirstInstruction: cardinal;                         //nedorobene
       LastInstruction: cardinal;                          //nedorobene

       LastItem: cardinal;
     end;

  TDisassembleOptions = record
    address: cardinal;
    size: cardinal;
    bit32: boolean;
    recursive: boolean;
    typ: (doSize,doCount);
  end;


  TDisassembledBlock = record
    Address: cardinal;
    Size: cardinal;
  end;


  TDisassembledBlocks = class
  private
    fCount: integer;
    fCapacity: integer;
    fBlocks: array of TDisassembledBlock;
    function GetBlock(Index: integer): TDisassembledBlock;
    function GetBlockCount: integer;
  public
    procedure Clear;
    function Add(Address, Size: cardinal): integer;
    property Items[Index: integer]: TDisassembledBlock read GetBlock; default;
    property Count: integer read GetBlockCount;
  end;



     TSize = (szEmpty, szByte, szWord, szDword, szQword, szTword, szDQword);

     TDisassembler = class                        // Hlavny objekt disassemblovania
     private
       ProgressFunction: TProgressFunction;
       code: TByteDynamicArray; //array of byte;                 // Pole s kodom
       DisasmMap: TByteDynamicArray;
       CodeSize: cardinal;
       fStatistics: TStatistics;

//       pocetadries:cardinal;
       modrm:tmodrm;
       sib:TSib;
       i:cardinal;                          // Uchovava poziciu v poli CODE
       j:cardinal;                          // Pomocne premenne
       operand32, address32:boolean;        // Uchovava 16/32bit stav instrukcie

       OperandSize: TSize;
       AddressSize: TSize;
       genreg16:boolean;
       SegmentOverride: string;
       counter: cardinal;
       ProgressPosition: cardinal;
       InstrAddress: cardinal;
    Vpc: Byte;                              // Pocet parametrov aktualnej instrukcie

       fBlocks: TDisassembledBlocks;

       function SpracujParameter(a:TParameter):string;
       function LoadModRM(a:byte):TModRM;
       function SpracujModRM(a:modrmparametertype; b:modrmparametersize):string;
       function LoadSIB(a:byte):TSIB;
       function SpracujSIB:string;
       function SpracujImmediate(a:ModRMParameterSize):string;
       function SpracujRelative(a:ModRMParameterSize):string;
       function SpracujOffset(a:ModRMParameterSize):string;
       function SpracujGenReg:string;

       function ReadFourBytes:string;

       function DisassembleBlock(start, finish: cardinal; bit32: boolean):boolean;

       function GetDisassembledBlock(Index: integer): TDisassembledBlock;
       function GetBlockCount: integer;

     public
       CAJ: TCallsAndJumps;

       Disassembled: array of TDisassembledItem;  // Vystup disassemblovania
       Imported: array of cardinal;
       constructor Create(SectionCode:TByteDynamicArray; var DisassemblerMap: TByteDynamicArray);

       function DisassembleAll(bit32: boolean): boolean;

       function Disassemble: boolean;

//       function DisassemblePart(Options: TDisassembleOptions): boolean;

       property Statistics: TStatistics read fStatistics;
       property Blocks[Index: integer]: TDisassembledBlock read GetDisassembledBlock;
       property BlockCount: integer read GetBlockCount;
     end;

//    function XLoadModRM(a:byte):TModRM; register; external 'tdaslib.dll' name 'XLoadModRM';  pokus s DLL


Implementation


type
     TInstruction = record        // Hlavny typ instrukcie
       name:string[20];           // Meno instrukcie(mnemonic)
       opcode: byte{word};              // Operacny kod
         AddOp:boolean;           // Name16 and  name32 depends on Address(true) or Operand(false) size attribut
           name16: string[10];
           name32: string[10];
       pc:byte;                   // Pocet operandov(parametrov)
         param: TParam;           // Operandy(parametre) instrukcie
       mmx1,mmx2,mmx3,mmx4:word;       // Indexy MMX instrukcii v tabulke instrukcii MMX(+SSE,+SSE2)
     end;

     TMMXInstruction = record
       name:string[20];
       opcode:word;
       pc:byte;
         param: Tparam;
     end;

     TGroupInstruction = record
       name:string[20];
       opcode: byte;//word;
       SecOpcode:byte;
         AddOp:boolean;
           name16: string[10];
           name32: string[10];
       pc:byte;
         param: Tparam;
     end;

     TFPUInstruction = record
       name:string[20];
       opcode:byte;
       par:TFPUParameter;
     end;

     TFPUInstructionEx = record
       name:string[20];
       opcode:byte;
       par: TSize;
     end;

     TPrefixes = record
       pF0,pF2,pF3:boolean;
       p2E,p36,p3E,p26,p64,p65:boolean;
       p66,p67:boolean;
     end;

     TGroupInstruction_set = array[0..7]of TInstruction;

const
        PocetOBOInstrukcii = 256;
        PocetTBOInstrukcii = 256;
        PocetOBOGroupInstrukcii = 128;
        PocetTBOGroupInstrukcii = 30;
        PocetFPUInstrukcii = 73;
        PocetMMXInstrukcii = 260; //47;
        Pocet3DNowInstrukcii = 24;
        Prefix=[$F0,$F2,$F3,
           $2E,$26,$3E,$36,$64,$65,
           $66,$67];

        UndefinedFPUInstructionIndex = PocetFPUInstrukcii;

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

const

// Opcode $FE - Group Instruction
// Opcode $D8 - FPU Instruction
// Opcode $66 - Instruction Prefix
// Opcode $D6 - Undefined Opcode
// Other opcodes - OneByte Opcode Instruction


       Group12: TGroupInstruction_set = (
                               (name: ''; opcode: $0; ),
                               (name: ''; opcode: $0; ),
                               (name: 'mmx'; opcode: $60; mmx1:243; mmx2:244),
                               (name: ''; opcode: $0; ),
                               (name: 'mmx'; opcode: $60; mmx1:245; mmx2:246),
                               (name: ''; opcode: $0; ),
                               (name: 'mmx'; opcode: $60; mmx1:247; mmx2:248),
                               (name: ''; opcode: $0; )
                                        );

       Group13: TGroupInstruction_set = (
                               (name: ''; opcode: $0; ),
                               (name: ''; opcode: $0; ),
                               (name: 'mmx'; opcode: $60; mmx1:249; mmx2:250),
                               (name: ''; opcode: $0; ),
                               (name: 'mmx'; opcode: $60; mmx1:251; mmx2:252),
                               (name: ''; opcode: $0; ),
                               (name: 'mmx'; opcode: $60; mmx1:253; mmx2:254),
                               (name: ''; opcode: $0; )
                                        );

       Group14: TGroupInstruction_set = (
                               (name: ''; opcode: $0; ),
                               (name: ''; opcode: $0; ),
                               (name: 'mmx'; opcode: $60; mmx1:255; mmx2:256),
                               (name: 'mmx'; opcode: $60; mmx2:257),
                               (name: ''; opcode: $0; ),
                               (name: ''; opcode: $0; ),
                               (name: 'mmx'; opcode: $60; mmx1:258; mmx2:259),
                               (name: 'mmx'; opcode: $60; mmx2:260)
                                        );
//                               (name: ''; opcode: $; pc:2; param: (p1:; p2:)),


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
                               (name: 'group5'; opcode: $FE)

                                         );

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

const GroupOBOInstruction_set:array[0..PocetOBOGroupInstrukcii-1] of TGroupInstruction = (

// Group 1
                               (name: 'ADD'; opcode: $80; secopcode:0; pc:2; param: (p1:MODb; p2: IMMb)),
                               (name: 'OR';  opcode: $80; secopcode:1; pc:2; param: (p1:MODb; p2: IMMb)),
                               (name: 'ADC'; opcode: $80; secopcode:2; pc:2; param: (p1:MODb; p2: IMMb)),
                               (name: 'SBB'; opcode: $80; secopcode:3; pc:2; param: (p1:MODb; p2: IMMb)),
                               (name: 'AND'; opcode: $80; secopcode:4; pc:2; param: (p1:MODb; p2: IMMb)),
                               (name: 'SUB'; opcode: $80; secopcode:5; pc:2; param: (p1:MODb; p2: IMMb)),
                               (name: 'XOR'; opcode: $80; secopcode:6; pc:2; param: (p1:MODb; p2: IMMb)),
                               (name: 'CMP'; opcode: $80; secopcode:7; pc:2; param: (p1:MODb; p2: IMMb)),

                               (name: 'ADD'; opcode: $81; secopcode:0; pc:2; param: (p1:MODv; p2: IMMv)),
                               (name: 'OR';  opcode: $81; secopcode:1; pc:2; param: (p1:MODv; p2: IMMv)),
                               (name: 'ADC'; opcode: $81; secopcode:2; pc:2; param: (p1:MODv; p2: IMMv)),
                               (name: 'SBB'; opcode: $81; secopcode:3; pc:2; param: (p1:MODv; p2: IMMv)),
                               (name: 'AND'; opcode: $81; secopcode:4; pc:2; param: (p1:MODv; p2: IMMv)),
                               (name: 'SUB'; opcode: $81; secopcode:5; pc:2; param: (p1:MODv; p2: IMMv)),
                               (name: 'XOR'; opcode: $81; secopcode:6; pc:2; param: (p1:MODv; p2: IMMv)),
                               (name: 'CMP'; opcode: $81; secopcode:7; pc:2; param: (p1:MODv; p2: IMMv)),

                               (name: 'ADD'; opcode: $82; secopcode:0; pc:2; param: (p1:MODb; p2: IMMb)),
                               (name: 'OR';  opcode: $82; secopcode:1; pc:2; param: (p1:MODb; p2: IMMb)),
                               (name: 'ADC'; opcode: $82; secopcode:2; pc:2; param: (p1:MODb; p2: IMMb)),
                               (name: 'SBB'; opcode: $82; secopcode:3; pc:2; param: (p1:MODb; p2: IMMb)),
                               (name: 'AND'; opcode: $82; secopcode:4; pc:2; param: (p1:MODb; p2: IMMb)),
                               (name: 'SUB'; opcode: $82; secopcode:5; pc:2; param: (p1:MODb; p2: IMMb)),
                               (name: 'XOR'; opcode: $82; secopcode:6; pc:2; param: (p1:MODb; p2: IMMb)),
                               (name: 'CMP'; opcode: $82; secopcode:7; pc:2; param: (p1:MODb; p2: IMMb)),

                               (name: 'ADD'; opcode: $83; secopcode:0; pc:2; param: (p1:MODv; p2: IMMb)),
                               (name: 'OR';  opcode: $83; secopcode:1; pc:2; param: (p1:MODv; p2: IMMb)),
                               (name: 'ADC'; opcode: $83; secopcode:2; pc:2; param: (p1:MODv; p2: IMMb)),
                               (name: 'SBB'; opcode: $83; secopcode:3; pc:2; param: (p1:MODv; p2: IMMb)),
                               (name: 'AND'; opcode: $83; secopcode:4; pc:2; param: (p1:MODv; p2: IMMb)),
                               (name: 'SUB'; opcode: $83; secopcode:5; pc:2; param: (p1:MODv; p2: IMMb)),
                               (name: 'XOR'; opcode: $83; secopcode:6; pc:2; param: (p1:MODv; p2: IMMb)),
                               (name: 'CMP'; opcode: $83; secopcode:7; pc:2; param: (p1:MODv; p2: IMMb)),

// Group 2
                               (name: 'ROL'; opcode: $C0; secopcode: 0; pc:2; param: (p1:MODb; p2: IMMb)),
                               (name: 'ROR'; opcode: $C0; secopcode: 1; pc:2; param: (p1:MODb; p2: IMMb)),
                               (name: 'RCL'; opcode: $C0; secopcode: 2; pc:2; param: (p1:MODb; p2: IMMb)),
                               (name: 'RCR'; opcode: $C0; secopcode: 3; pc:2; param: (p1:MODb; p2: IMMb)),
                                // podla intelu SAL = SHL, podla Compa SAL = C0 ??110???
                               (name: 'SHL'; opcode: $C0; secopcode: 4; pc:2; param: (p1:MODb; p2: IMMb)),
                               (name: 'SHR'; opcode: $C0; secopcode: 5; pc:2; param: (p1:MODb; p2: IMMb)),
                               (name: 'SAL'; opcode: $C0; secopcode: 6; pc:2; param: (p1:MODb; p2: IMMb)),
                               (name: 'SAR'; opcode: $C0; secopcode: 7; pc:2; param: (p1:MODb; p2: IMMb)),

                               (name: 'ROL'; opcode: $C1; secopcode: 0; pc:2; param: (p1:MODv; p2: IMMb)),
                               (name: 'ROR'; opcode: $C1; secopcode: 1; pc:2; param: (p1:MODv; p2: IMMb)),
                               (name: 'RCL'; opcode: $C1; secopcode: 2; pc:2; param: (p1:MODv; p2: IMMb)),
                               (name: 'RCR'; opcode: $C1; secopcode: 3; pc:2; param: (p1:MODv; p2: IMMb)),
                               (name: 'SHL'{/SAL}; opcode: $C1; secopcode: 4; pc:2; param: (p1:MODv; p2: IMMb)),
                               (name: 'SHR'; opcode: $C1; secopcode: 5; pc:2; param: (p1:MODv; p2: IMMb)),
                               (name: 'undefined opcode!'; opcode: $D6; secopcode: 6),
                               (name: 'SAR'; opcode: $C1; secopcode: 7; pc:2; param: (p1:MODv; p2: IMMb)),

                               (name: 'ROL'; opcode: $D0; secopcode: 0; pc:2; param: (p1:MODb; p2: n1)),
                               (name: 'ROR'; opcode: $D0; secopcode: 1; pc:2; param: (p1:MODb; p2: n1)),
                               (name: 'RCL'; opcode: $D0; secopcode: 2; pc:2; param: (p1:MODb; p2: n1)),
                               (name: 'RCR'; opcode: $D0; secopcode: 3; pc:2; param: (p1:MODb; p2: n1)),
                               (name: 'SHL'{/SAL}; opcode: $D0; secopcode: 4; pc:2; param: (p1:MODb; p2: n1)),
                               (name: 'SHR'; opcode: $D0; secopcode: 5; pc:2; param: (p1:MODb; p2: n1)),
                               (name: 'undefined opcode!'; opcode: $D6; secopcode: 6),
                               (name: 'SAR'; opcode: $D0; secopcode: 7; pc:2; param: (p1:MODb; p2: n1)),

                               (name: 'ROL'; opcode: $D1; secopcode: 0; pc:2; param: (p1:MODv; p2: n1)),
                               (name: 'ROR'; opcode: $D1; secopcode: 1; pc:2; param: (p1:MODv; p2: n1)),
                               (name: 'RCL'; opcode: $D1; secopcode: 2; pc:2; param: (p1:MODv; p2: n1)),
                               (name: 'RCR'; opcode: $D1; secopcode: 3; pc:2; param: (p1:MODv; p2: n1)),
                               (name: 'SHL'{/SAL}; opcode: $D1; secopcode: 4; pc:2; param: (p1:MODv; p2: n1)),
                               (name: 'SHR'; opcode: $D1; secopcode: 5; pc:2; param: (p1:MODv; p2: n1)),
                               (name: 'undefined opcode!'; opcode: $D6; secopcode: 6),
                               (name: 'SAR'; opcode: $D1; secopcode: 7; pc:2; param: (p1:MODv; p2: n1)),

                               (name: 'ROL'; opcode: $D2; secopcode: 0; pc:2; param: (p1:MODb; p2: CL)),
                               (name: 'ROR'; opcode: $D2; secopcode: 1; pc:2; param: (p1:MODb; p2: CL)),
                               (name: 'RCL'; opcode: $D2; secopcode: 2; pc:2; param: (p1:MODb; p2: CL)),
                               (name: 'RCR'; opcode: $D2; secopcode: 3; pc:2; param: (p1:MODb; p2: CL)),
                               (name: 'SHL'{/SAL}; opcode: $D2; secopcode: 4; pc:2; param: (p1:MODb; p2: CL)),
                               (name: 'SHR'; opcode: $D2; secopcode: 5; pc:2; param: (p1:MODb; p2: CL)),
                               (name: 'undefined opcode!'; opcode: $D6; secopcode: 6),
                               (name: 'SAR'; opcode: $D2; secopcode: 7; pc:2; param: (p1:MODb; p2: CL)),

                               (name: 'ROL'; opcode: $D3; secopcode: 0; pc:2; param: (p1:MODv; p2: CL)),
                               (name: 'ROR'; opcode: $D3; secopcode: 1; pc:2; param: (p1:MODv; p2: CL)),
                               (name: 'RCL'; opcode: $D3; secopcode: 2; pc:2; param: (p1:MODv; p2: CL)),
                               (name: 'RCR'; opcode: $D3; secopcode: 3; pc:2; param: (p1:MODv; p2: CL)),
                               (name: 'SHL'{/SAL}; opcode: $D3; secopcode: 4; pc:2; param: (p1:MODv; p2: CL)),
                               (name: 'SHR'; opcode: $D3; secopcode: 5; pc:2; param: (p1:MODv; p2: CL)),
                               (name: 'undefined opcode!'; opcode: $D6; secopcode: 6),
                               (name: 'SAR'; opcode: $D3; secopcode: 7; pc:2; param: (p1:MODv; p2: CL)),

// Group 3

                               (name: 'TEST'; opcode: $F6; secopcode: 0; pc:2; param: (p1:MODb; p2: IMMb)),
                               (name: 'undefined opcode!'; opcode: $D6; secopcode: 1),
                               (name: 'NOT'; opcode: $F6; secopcode: 2; pc:1; param: (p1:MODb)),
                               (name: 'NEG'; opcode: $F6; secopcode: 3; pc:1; param: (p1:MODb)),
                               (name: 'MUL'; opcode: $F6; secopcode: 4; pc:1; param: (p1:MODb)),
                               (name: 'IMUL'; opcode: $F6; secopcode: 5; pc:1; param: (p1:MODb)),
                               (name: 'DIV'; opcode: $F6; secopcode: 6; pc:1; param: (p1:MODb)),
                               (name: 'IDIV'; opcode: $F6; secopcode: 7; pc:1; param: (p1:MODb)),

                               (name: 'TEST'; opcode: $F7; secopcode: 0; pc:2; param: (p1:MODv; p2: IMMv)),
                               (name: 'undefined opcode!'; opcode: $D6; secopcode: 1),
                               (name: 'NOT'; opcode: $F7; secopcode: 2; pc:1; param: (p1:MODv)),
                               (name: 'NEG'; opcode: $F7; secopcode: 3; pc:1; param: (p1:MODv)),
                               (name: 'MUL'; opcode: $F7; secopcode: 4; pc:1; param: (p1:MODv)),
                               (name: 'IMUL'; opcode: $F7; secopcode: 5; pc:1; param: (p1:MODv)),
                               (name: 'DIV'; opcode: $F7; secopcode: 6; pc:1; param: (p1:MODv)),
                               (name: 'IDIV'; opcode: $F7; secopcode: 7; pc:1; param: (p1:MODv)),

// Group 4

                               (name: 'INC'; opcode: $FE; secopcode: 0; pc:1; param: (p1:MODb)),
                               (name: 'DEC'; opcode: $FE; secopcode: 1; pc:1; param: (p1:MODb)),
                               (name: 'undefined opcode!'; opcode: $D6; secopcode: 2),
                               (name: 'undefined opcode!'; opcode: $D6; secopcode: 3),
                               (name: 'undefined opcode!'; opcode: $D6; secopcode: 4),
                               (name: 'undefined opcode!'; opcode: $D6; secopcode: 5),
                               (name: 'undefined opcode!'; opcode: $D6; secopcode: 6),
                               (name: 'undefined opcode!'; opcode: $D6; secopcode: 7),

// Group 5
                               (name: 'INC'; opcode: $FF; secopcode: 0; pc:1; param: (p1:MODv)),
                               (name: 'DEC'; opcode: $FF; secopcode: 1; pc:1; param: (p1:MODv)),
             {calln}                  (name: 'CALL'; opcode: $FF; secopcode: 2; pc:1; param: (p1:MODv)),
             {callf}                  (name: 'CALL'; opcode: $FF; secopcode: 3; pc:1; param: (p1:MODp)),
                               (name: 'JMP'{N}; opcode: $FF; secopcode: 4; pc:1; param: (p1:MODv)),
                               (name: 'JMP'{F}; opcode: $FF; secopcode: 5; pc:1; param: (p1:MODp)),
                               (name: 'PUSH'; opcode: $FF; secopcode: 6; pc:1; param: (p1:MODv)),
                               (name: 'undefined opcode!'; opcode: $D6; secopcode: 7),

// Group 11
                               (name: 'MOV'; opcode: $C6; secopcode: 0; pc:2; param: (p1:MODb; p2:IMMb)),
                               (name: 'undefined opcode!'; opcode: $D6; secopcode: 1),
                               (name: 'undefined opcode!'; opcode: $D6; secopcode: 2),
                               (name: 'undefined opcode!'; opcode: $D6; secopcode: 3),
                               (name: 'undefined opcode!'; opcode: $D6; secopcode: 4),
                               (name: 'undefined opcode!'; opcode: $D6; secopcode: 5),
                               (name: 'undefined opcode!'; opcode: $D6; secopcode: 6),
                               (name: 'undefined opcode!'; opcode: $D6; secopcode: 7),

                               (name: 'MOV'; opcode: $C7; secopcode: 0; pc:2; param: (p1:MODv; p2:IMMv)),
                               (name: 'undefined opcode!'; opcode: $D6; secopcode: 1),
                               (name: 'undefined opcode!'; opcode: $D6; secopcode: 2),
                               (name: 'undefined opcode!'; opcode: $D6; secopcode: 3),
                               (name: 'undefined opcode!'; opcode: $D6; secopcode: 4),
                               (name: 'undefined opcode!'; opcode: $D6; secopcode: 5),
                               (name: 'undefined opcode!'; opcode: $D6; secopcode: 6),
                               (name: 'undefined opcode!'; opcode: $D6; secopcode: 7)


//                               (name: ''; opcode: $; secopcode: ; pc:2; param: (p1: MODb)),

                               );

const GroupTBOInstruction_set:array[1..PocetTBOGroupInstrukcii] of TGroupInstruction = (

// Group 6
                               (name: 'SLDT'; opcode: $00; secopcode: 0; pc:1; param: (p1: MODv)),
                               (name: 'STR'; opcode: $00; secopcode: 1; pc:1; param: (p1: MODv)),
                               (name: 'LLDT'; opcode: $00; secopcode: 2; pc:1; param: (p1: MODw)),
                               (name: 'LTR'; opcode: $00; secopcode: 3; pc:1; param: (p1: MODw)),
                               (name: 'VERR'; opcode: $00; secopcode: 4; pc:1; param: (p1: MODw)),
                               (name: 'VERW'; opcode: $00; secopcode: 5; pc:1; param: (p1: MODw)),

// Group 7
                               (name: 'SGDT'; opcode: $01; secopcode: 0; pc:1; param: (p1: MODv)),
                               (name: 'SIDT'; opcode: $01; secopcode: 1; pc:1; param: (p1: MODv)),
                               (name: 'LGDT'; opcode: $01; secopcode: 2; pc:1; param: (p1: MODv)),
                               (name: 'LIDT'; opcode: $01; secopcode: 3; pc:1; param: (p1: MODv)),
                               (name: 'SMSW'; opcode: $01; secopcode: 4; pc:1; param: (p1: MODw)),
                               (name: 'LMSW'; opcode: $01; secopcode: 6; pc:1; param: (p1: MODw)),
                               (name: 'INVLPG'; opcode: $01; secopcode: 7; pc:1; param: (p1: MODb)),

// Group 8
                               (name: 'BT'; opcode: $BA; secopcode: 4; pc:2; param: (p1: MODv; p2: IMMb)),
                               (name: 'BTS'; opcode: $BA; secopcode: 5; pc:2; param: (p1: MODv; p2: IMMb)),
                               (name: 'BTR'; opcode: $BA; secopcode: 6; pc:2; param: (p1: MODv; p2: IMMb)),
                               (name: 'BTC'; opcode: $BA; secopcode: 7; pc:2; param: (p1: MODv; p2: IMMb)),

// Group 9
                               (name: 'CMPXCHG8B'; opcode: $C7; secopcode: 1; pc:1; param: (p1: MODv)),
// Group 15

                               (name: 'FXSAVE'; opcode: $AE; secopcode:0; pc:0),
                               (name: 'FXSTOR'; opcode: $AE; secopcode:1; pc:0),
                               (name: 'LDMXCSR'; opcode: $AE; secopcode:2; pc:0),
                               (name: 'STMXCSR'; opcode: $AE; secopcode:3; pc:0),
                               (name: 'LFENCE'; opcode: $AE; secopcode:5; pc:0),
                               (name: 'MFENCE'; opcode: $AE; secopcode:6; pc:0),
                               (name: 'CLFLUSH'; opcode: $AE; secopcode:7; pc:0),
                               (name: 'SFENCE'; opcode: $AE; secopcode:7; pc:0),
// Group 16
                               (name: 'PREFETCHNTA'; opcode: $18; secopcode:0; pc:0),
                               (name: 'PREFETCHT0'; opcode: $18; secopcode:1; pc:0),
                               (name: 'PREFETCHT1'; opcode: $18; secopcode:2; pc:0),
                               (name: 'PREFETCHT2'; opcode: $18; secopcode:3; pc:0)

//                               (name: ''; opcode: $; secopcode: ; pc:2; param: (p1: MODv; p2: IMMb)),

                                                                   );

const FPUInstruction_set:array[1..PocetFPUInstrukcii] of TFPUInstruction = (
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

const FPUInstructions:array[0..63]of TFPUInstructionEx = (

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

const FPUINstructionNames:array[0..63]of string[10] = (
  ('FADD'),  ('FMUL'),  ('FCOM'),  ('FCOMP'),  ('FSUB'),   ('FSUBR'),  ('FDIV'),   ('FDIVR'),
  ('FLD'),   ('???'),   ('FST'),   ('FSTP'),   ('FLDENV'), ('FLDCW'),  ('FSTENV'), ('FSTCW'),
  ('FIADD'), ('FIMUL'), ('FICOM'), ('FICOMP'), ('FISUB'),  ('FISUBR'), ('FIDIV'),  ('FIDIVR'),
  ('FILD'),  ('???'),   ('FIST'),  ('FISTP'),  ('???'),    ('FLD'),    ('???'),    ('FSTP'),
  ('FADD'),  ('FMUL'),  ('FCOM'),  ('FCOMP'),  ('FSUB'),   ('FSUBR'),  ('FDIV'),   ('FDIVR'),
  ('FLD'),   ('???'),   ('FST'),   ('FSTP'),   ('FRSTOR'), ('???'),    ('FSAVE'),  ('FSTSW'),
  ('FIADD'), ('FIMUL'), ('FICOM'), ('FICOMP'), ('FISUB'),  ('FISUBR'), ('FIDIV'),  ('FIDIVR'),
  ('FILD'),  ('???'),   ('FIST'),  ('FISTP'),  ('FBLD'),   ('FILD'),   ('FBSTP'),  ('FISTP')
                                );

const MMXInstruction_set:array[0..PocetMMXInstrukcii]of TMMXInstruction = (
// 0..9
                               (name: 'PADDB'; opcode: $0; pc:2; param: (p1:GREGq; p2:MODq)),
                               (name: 'PADDW'; opcode: $0; pc:2; param: (p1:GREGq ;p2:MODq)),
                               (name: 'PADDD'; opcode: $0; pc:2; param: (p1:GREGq ;p2:MODq)),
                               (name: 'PADDSB'; opcode: $0; pc:2; param: (p1:GREGq ;p2:MODq)),
                               (name: 'PADDSW'; opcode: $0; pc:2; param: (p1:GREGq ;p2:MODq)),
                               (name: 'PADDUSB'; opcode: $0; pc:2; param: (p1:GREGq ;p2:MODq)),
                               (name: 'PADDUSW'; opcode: $0; pc:2; param: (p1:GREGq ;p2:MODq)),
                               (name: 'PSUBB'; opcode: $0; pc:2; param: (p1:GREGq ;p2:MODq)),
                               (name: 'PSUBW'; opcode: $0; pc:2; param: (p1:GREGq ;p2:MODq)),
                               (name: 'PSUBD'; opcode: $0; pc:2; param: (p1:GREGq ;p2:MODq)),
// 10..19
                               (name: 'PSUBSB'; opcode: $0; pc:2; param: (p1:GREGq ;p2:MODq)),
                               (name: 'PSUBSW'; opcode: $0; pc:2; param: (p1:GREGq ;p2:MODq)),
                               (name: 'PSUBUSB'; opcode: $D8; pc:2; param: (p1:GREGq ;p2:MODq)),
                               (name: 'PSUBUSW'; opcode: $D9; pc:2; param: (p1:GREGq ;p2:MODq)),
                               (name: 'PMULLW'; opcode: $D5; pc:2; param: (p1:GREGq ;p2:MODq)),
                               (name: 'PMULHW'; opcode: $E5; pc:2; param: (p1:GREGq ;p2:MODq)),
                               (name: 'PMADDWD'; opcode: $0; pc:2; param: (p1:GREGq ;p2:MODq)),

                               (name: 'PCMPEQB'; opcode: $74; pc:2; param: (p1:GREGq ;p2:MODq)),
                               (name: 'PCMPEQW'; opcode: $75; pc:2; param: (p1:GREGq ;p2:MODq)),
                               (name: 'PCMPEQD'; opcode: $76; pc:2; param: (p1:GREGq ;p2:MODq)),
// 20..29
                               (name: 'PCMPGTB'; opcode: $64; pc:2; param: (p1:GREGq ;p2:MODq)),
                               (name: 'PCMPGTW'; opcode: $65; pc:2; param: (p1:GREGq ;p2:MODq)),
                               (name: 'PCMPGTD'; opcode: $66; pc:2; param: (p1:GREGq ;p2:MODq)),

                               (name: 'PACKSSWB'; opcode: $63; pc:2; param: (p1:GREGq ;p2:MODq)),
                               (name: 'PACKSSDW'; opcode: $6B; pc:2; param: (p1:GREGq ;p2:MODd)),
                               (name: 'PACKUSWB'; opcode: $67; pc:2; param: (p1:GREGq ;p2:MODq)),

                               (name: 'PUNPCKHBW'; opcode: $68; pc:2; param: (p1:GREGq ;p2:MODq)),
                               (name: 'PUNPCKHWD'; opcode: $69; pc:2; param: (p1:GREGq ;p2:MODq)),
                               (name: 'PUNPCKHDQ'; opcode: $6A; pc:2; param: (p1:GREGq ;p2:MODq)),
                               (name: 'PUNPCKLBW'; opcode: $60; pc:2; param: (p1:GREGq ;p2:MODq)),
// 30..39
                               (name: 'PUNPCKLWD'; opcode: $61; pc:2; param: (p1:GREGq ;p2:MODd)),
                               (name: 'PUNPCKLDQ'; opcode: $62; pc:2; param: (p1:GREGq ;p2:MODd)),

                               (name: 'PAND'; opcode: $DB; pc:2; param: (p1:GREGq ;p2:MODq)),
                               (name: 'PANDN'; opcode: $DF; pc:2; param: (p1:GREGq ;p2:MODq)),
                               (name: 'POR'; opcode: $EB; pc:2; param: (p1:GREGq ;p2:MODq)),
                               (name: 'PXOR'; opcode: $EF; pc:2; param: (p1:GREGq ;p2:MODq)),

                               (name: 'PSLLW'; opcode:$F1 ; pc:2; param: (p1:GREGq ;p2:MODq)),
                               (name: 'PSLLD'; opcode:$F2 ; pc:2; param: (p1:GREGq ;p2:MODq)),
                               (name: 'PSLLQ'; opcode:$F3 ; pc:2; param: (p1:GREGq ;p2:MODq)),
                               (name: 'PSRLW'; opcode:$D1 ; pc:2; param: (p1:GREGq ;p2:MODq)),
// 40..47
                               (name: 'PSRLD'; opcode:$D2 ; pc:2; param: (p1:GREGq ;p2:MODq)),
                               (name: 'PSRLQ'; opcode:$D3 ; pc:2; param: (p1:GREGq ;p2:MODq)),
                               (name: 'PSRAW'; opcode:$E1 ; pc:2; param: (p1:GREGq ;p2:MODq)),
                               (name: 'PSRAD'; opcode:$E2 ; pc:2; param: (p1:GREGq ;p2:MODq)),

                               (name: 'MOVD'; opcode: $6E; pc:2; param: (p1:GREGd ;p2:MODd)),
                               (name: 'MOVD'; opcode: $7E; pc:2; param: (p1:MODd ;p2:GREGd)),
//                               (name: 'MOVD'; opcode: $; pc:; param: (p1: ;p2:)),
                               (name: 'MOVQ'; opcode: $6F; pc:2; param: (p1:GREGq ;p2:MODq)),
                               (name: 'MOVQ'; opcode: $7F; pc:2; param: (p1:MODq ;p2:GREGq)),
//                               (name: 'MOVQ'; opcode: $; pc:; param: (p1: ;p2:))


// SSE & SSE2 Instructions ...

// 48..49
                               (name: 'MOVUPS'; opcode: $10; pc:2; param: (p1:GREGdq ;p2:MODdq)),
                               (name: 'MOVSS'; opcode: $10; pc:2; param: (p1:GREGdq ;p2:XMM_M32)),
// 50..59
                               (name: 'MOVUPD'; opcode: $10; pc:2; param: (p1:GREGdq ;p2:MODdq)),
                               (name: 'MOVSD'; opcode: $10; pc:2; param: (p1:GREGdq ;p2:XMM_M64)),
                               (name: 'MOVUPS'; opcode: $11; pc:2; param: (p1:MODdq ;p2:GREGdq)),
                               (name: 'MOVSS'; opcode: $11; pc:2; param: (p1:XMM_M32 ;p2:GREGdq)),
                               (name: 'MOVUPD'; opcode: $11; pc:2; param: (p1:MODdq ;p2:GREGdq)),
                               (name: 'MOVSD'; opcode: $11; pc:2; param: (p1:XMM_M64 ;p2:GREGdq)),
                               (name: 'MOVLPS'; opcode: $12; pc:2; param: (p1:GREGdq ;p2:M64)),     // rovnaky opcode s MOVHLPS !
                               (name: 'MOVLPD'; opcode: $12; pc:2; param: (p1:GREGdq ;p2:M64)),
                               (name: 'MOVHLPS'; opcode: $12; pc:2; param: (p1:GREGdq ;p2:R128)),   // rovnaky opcode s MOVLPS  !
                               (name: 'MOVLPS'; opcode: $13; pc:2; param: (p1:M64 ;p2:GREGdq)),
                               (name: 'MOVLPD'; opcode: $13; pc:2; param: (p1:M64 ;p2:GREGdq)),
// 60..69 +1 atd...
                               (name: 'UNPCKLPS'; opcode: $14; pc:2; param: (p1:GREGdq ;p2:MODdq)),
                               (name: 'UNPCKLPD'; opcode: $14; pc:2; param: (p1:GREGdq ;p2:MODdq)),
                               (name: 'UNPCKHPS'; opcode: $15; pc:2; param: (p1:GREGdq ;p2:MODdq)),
                               (name: 'UNPCKHPD'; opcode: $15; pc:2; param: (p1:GREGdq ;p2:MODdq)),
                               (name: 'MOVHPS'; opcode: $16; pc:2; param: (p1:GREGdq ;p2:M64)),     // rovnaky opcode s MOVLHPS
                               (name: 'MOVHPD'; opcode: $16; pc:2; param: (p1:GREGdq ;p2:M64)),
                               (name: 'MOVLHPS'; opcode: $16; pc:2; param: (p1:GREGdq ;p2:R128)),   // rovnaky opcode s MOVHPS
                               (name: 'MOVHPS'; opcode: $17; pc:2; param: (p1:M64 ;p2:GREGdq)),
                               (name: 'MOVHPD'; opcode: $17; pc:2; param: (p1:M64 ;p2:GREGdq)),

                               (name: 'MOVAPS'; opcode: $28; pc:2; param: (p1:GREGdq ;p2:MODdq)),
// 70..79
                               (name: 'MOVAPD'; opcode: $28; pc:2; param: (p1:GREGdq ;p2:MODdq)),
                               (name: 'MOVAPS'; opcode: $29; pc:2; param: (p1:MODdq ;p2:GREGdq)),
                               (name: 'MOVAPD'; opcode: $29; pc:2; param: (p1:MODdq ;p2:GREGdq)),
                               (name: 'CVTPI2PS'; opcode: $2A; pc:2; param: (p1:GREGdq ;p2:MODq)),
                               (name: 'CVTSI2SS'; opcode: $2A; pc:2; param: (p1:GREGdq ;p2:MODd)),
                               (name: 'CVTPI2PD'; opcode: $2A; pc:2; param: (p1:GREGdq ;p2:MODq)),
                               (name: 'CVTSI2SD'; opcode: $2A; pc:2; param: (p1:GREGdq ;p2:MODd)),
                               (name: 'MOVNTPS'; opcode: $2B; pc:2; param: (p1:M128 ;p2:GREGdq)),
                               (name: 'MOVNTPD'; opcode: $2B; pc:2; param: (p1:M128 ;p2:GREGdq)),
                               (name: 'CVTTPS2PI'; opcode: $2C; pc:2; param: (p1:GREGq ;p2:XMM_M32)),
// 80..89
                               (name: 'CVTTSS2SI'; opcode: $2C; pc:2; param: (p1:GREGd ;p2:XMM_M32)),
                               (name: 'CVTTPD2PI'; opcode: $2C; pc:2; param: (p1:GREGq ;p2:MODdq)),
                               (name: 'CVTTSD2SI'; opcode: $2C; pc:2; param: (p1:GREGd ;p2:XMM_M64)),
                               (name: 'CVTPS2PI'; opcode: $2D; pc:2; param: (p1:GREGq ;p2:XMM_M64)),
                               (name: 'CVTSS2SI'; opcode: $2D; pc:2; param: (p1:GREGd ;p2:XMM_M32)),
                               (name: 'CVTPD2PI'; opcode: $2D; pc:2; param: (p1:GREGq ;p2:MODdq)),
                               (name: 'CVTSD2SI'; opcode: $2D; pc:2; param: (p1:GREGd ;p2:XMM_M64)),
                               (name: 'UCOMISS'; opcode: $2E; pc:2; param: (p1:GREGdq ;p2:XMM_M32)),
                               (name: 'UCOMISD'; opcode: $2E; pc:2; param: (p1:GREGdq ;p2:XMM_M64)),
                               (name: 'COMISS'; opcode: $2F; pc:2; param: (p1:GREGdq ;p2:XMM_M32)),
// 90..99
                               (name: 'COMISD'; opcode: $2F; pc:2; param: (p1:GREGdq ;p2:XMM_M64)),

                               (name: 'MOVMSKPS'; opcode: $50; pc:2; param: (p1:R32 ;p2:GREGdq)),
                               (name: 'MOVMSKPD'; opcode: $50; pc:2; param: (p1:R32 ;p2:GREGdq)),
                               (name: 'SQRTPS'; opcode: $51; pc:2; param: (p1:GREGdq ;p2:MODdq)),
                               (name: 'SQRTSS'; opcode: $51; pc:2; param: (p1:GREGdq ;p2:XMM_M32)),
                               (name: 'SQRTPD'; opcode: $51; pc:2; param: (p1:GREGdq ;p2:MODdq)),
                               (name: 'SQRTSD'; opcode: $51; pc:2; param: (p1:GREGdq ;p2:XMM_M64)),
                               (name: 'RSQRTPS'; opcode: $52; pc:2; param: (p1:GREGdq ;p2:MODdq)),
                               (name: 'RSQRTSS'; opcode: $52; pc:2; param: (p1:GREGdq ;p2:XMM_M32)),
                               (name: 'RCPPS'; opcode: $53; pc:2; param: (p1:GREGdq ;p2:MODdq)),
// 100..109
                               (name: 'RCPSS'; opcode: $53; pc:2; param: (p1:GREGdq ;p2:XMM_M32)),
                               (name: 'ANDPS'; opcode: $54; pc:2; param: (p1:GREGdq ;p2:MODdq)),
                               (name: 'ANDPD'; opcode: $54; pc:2; param: (p1:GREGdq ;p2:MODdq)),
                               (name: 'ANDNPS'; opcode: $55; pc:2; param: (p1:GREGdq ;p2:MODdq)),
                               (name: 'ANDNPD'; opcode: $55; pc:2; param: (p1:GREGdq ;p2:MODdq)),
                               (name: 'ORPS'; opcode: $56; pc:2; param: (p1:GREGdq ;p2:MODdq)),
                               (name: 'ORPD'; opcode: $56; pc:2; param: (p1:GREGdq ;p2:MODdq)),
                               (name: 'XORPS'; opcode: $57; pc:2; param: (p1:GREGdq ;p2:MODdq)),
                               (name: 'XORPD'; opcode: $57; pc:2; param: (p1:GREGdq ;p2:MODdq)),

                               (name: 'ADDPS'; opcode: $58; pc:2; param: (p1:GREGdq ;p2:MODdq)),
// 110..119
                               (name: 'ADDSS'; opcode: $58; pc:2; param: (p1:GREGdq ;p2:XMM_M32)),
                               (name: 'ADDPD'; opcode: $58; pc:2; param: (p1:GREGdq ;p2:MODdq)),
                               (name: 'ADDSD'; opcode: $58; pc:2; param: (p1:GREGdq ;p2:XMM_M64)),
                               (name: 'MULPS'; opcode: $59; pc:2; param: (p1:GREGdq ;p2:MODdq)),
                               (name: 'MULSS'; opcode: $59; pc:2; param: (p1:GREGdq ;p2:XMM_M32)),
                               (name: 'MULPD'; opcode: $59; pc:2; param: (p1:GREGdq ;p2:MODdq)),
                               (name: 'MULSD'; opcode: $59; pc:2; param: (p1:GREGdq ;p2:XMM_M64)),
                               (name: 'CVTPS2PD'; opcode: $5A; pc:2; param: (p1:GREGdq ;p2:XMM_M64)),
                               (name: 'CVTSS2SD'; opcode: $5A; pc:2; param: (p1:GREGdq ;p2:XMM_M32)),
                               (name: 'CVTPD2PS'; opcode: $5A; pc:2; param: (p1:GREGdq ;p2:MODdq)),
// 120..129
                               (name: 'CVTSD2SS'; opcode: $5A; pc:2; param: (p1:GREGdq ;p2:XMM_M64)),
                               (name: 'CVTDQ2PS'; opcode: $5B; pc:2; param: (p1:GREGdq ;p2:MODdq)),
                               (name: 'CVTPS2DQ'; opcode: $5B; pc:2; param: (p1:GREGdq ;p2:MODdq)),
                               (name: 'CVTTPS2DQ'; opcode: $5B; pc:2; param: (p1:GREGdq ;p2:MODdq)),
                               (name: 'SUBPS'; opcode: $5C; pc:2; param: (p1:GREGdq ;p2:MODdq)),
                               (name: 'SUBSS'; opcode: $5C; pc:2; param: (p1:GREGdq ;p2:XMM_M32)),
                               (name: 'SUBPD'; opcode: $5C; pc:2; param: (p1:GREGdq ;p2:MODdq)),
                               (name: 'SUBSD'; opcode: $5C; pc:2; param: (p1:GREGdq ;p2:XMM_M64)),
                               (name: 'MINPS'; opcode: $5D; pc:2; param: (p1:GREGdq ;p2:MODdq)),
                               (name: 'MINSS'; opcode: $5D; pc:2; param: (p1:GREGdq ;p2:XMM_M32)),
// 130..139
                               (name: 'MINPD'; opcode: $5D; pc:2; param: (p1:GREGdq ;p2:MODdq)),
                               (name: 'MINSD'; opcode: $5D; pc:2; param: (p1:GREGdq ;p2:XMM_M64)),
                               (name: 'DIVPS'; opcode: $5E; pc:2; param: (p1:GREGdq ;p2:MODdq)),
                               (name: 'DIVSS'; opcode: $5E; pc:2; param: (p1:GREGdq ;p2:XMM_M32)),
                               (name: 'DIVPD'; opcode: $5E; pc:2; param: (p1:GREGdq ;p2:MODdq)),
                               (name: 'DIVSD'; opcode: $5E; pc:2; param: (p1:GREGdq ;p2:XMM_M64)),
                               (name: 'MAXPS'; opcode: $5F; pc:2; param: (p1:GREGdq ;p2:MODdq)),
                               (name: 'MAXSS'; opcode: $5F; pc:2; param: (p1:GREGdq ;p2:XMM_M32)),
                               (name: 'MAXPD'; opcode: $5F; pc:2; param: (p1:GREGdq ;p2:MODdq)),
                               (name: 'MAXSD'; opcode: $5F; pc:2; param: (p1:GREGdq ;p2:XMM_M64)),
// 140..149
                               (name: 'PUNPCKLBW'; opcode: $60; pc:2; param: (p1:GREGdq ;p2:MODdq)),
                               (name: 'PUNPCKLWD'; opcode: $61; pc:2; param: (p1:GREGdq ;p2:MODdq)),
                               (name: 'PUNPCKLDQ'; opcode: $62; pc:2; param: (p1:GREGdq ;p2:MODdq)),
                               (name: 'PACKSSWB'; opcode: $63; pc:2; param: (p1:GREGdq ;p2:MODdq)),
                               (name: 'PCMPGTB'; opcode: $64; pc:2; param: (p1:GREGdq ;p2:MODdq)),
                               (name: 'PCMPGTW'; opcode: $65; pc:2; param: (p1:GREGdq ;p2:MODdq)),
                               (name: 'PCMPGTD'; opcode: $66; pc:2; param: (p1:GREGdq ;p2:MODdq)),
                               (name: 'PACKUSWB'; opcode: $67; pc:2; param: (p1:GREGdq ;p2:MODdq)),
                               (name: 'PUNPCKHBW'; opcode: $68; pc:2; param: (p1:GREGdq ;p2:MODdq)),
                               (name: 'PUNPCKHWD'; opcode: $69; pc:2; param: (p1:GREGdq ;p2:MODdq)),
// 150..159
                               (name: 'PUNPCKHDQ'; opcode: $6A; pc:2; param: (p1:GREGdq ;p2:MODdq)),
                               (name: 'PACKSSDW'; opcode: $6B; pc:2; param: (p1:GREGdq ;p2:MODdq)),
                               (name: 'PUNPCKLQDQ'; opcode: $6C; pc:2; param: (p1:GREGdq ;p2:MODdq)),
                               (name: 'PUNPCKHQD'; opcode: $6D; pc:2; param: (p1:GREGdq ;p2:MODdq)),
                               (name: 'MOVD'; opcode: $6E; pc:2; param: (p1:GREGdq ;p2:MODd)),
                               (name: 'MOVDQA'; opcode: $6F; pc:2; param: (p1:GREGdq ;p2:MODdq)),
                               (name: 'MOVDQU'; opcode: $6F; pc:2; param: (p1:GREGdq ;p2:MODdq)),

                               (name: 'PSHUFW'; opcode: $70; pc:3; param: (p1:GREGq ;p2:MODq; p3:IMMb)),
                               (name: 'PSHUFD'; opcode: $70; pc:3; param: (p1:GREGdq ;p2:MODdq; p3:IMMb)),
                               (name: 'PSHUFHW'; opcode: $70; pc:3; param: (p1:GREGdq ;p2:MODdq; p3:IMMb)),
// 160..169
                               (name: 'PSHUFLW'; opcode: $70; pc:3; param: (p1:GREGdq ;p2:MODdq; p3:IMMb)),
                               (name: 'PCMPEQB'; opcode: $74; pc:2; param: (p1:GREGdq ;p2:MODdq)),
                               (name: 'PCMPEQW'; opcode: $75; pc:2; param: (p1:GREGdq ;p2:MODdq)),
                               (name: 'PCMPEQD'; opcode: $76; pc:2; param: (p1:GREGdq ;p2:MODdq)),
                               (name: 'MOVD'; opcode: $7E; pc:2; param: (p1:MODd ;p2:GREGdq)),
                               (name: 'MOVQ'; opcode: $7E; pc:2; param: (p1:GREGdq ;p2:XMM_M64)),
                               (name: 'MOVDQA'; opcode: $7F; pc:2; param: (p1:MODdq ;p2:GREGdq)),
                               (name: 'MOVDQU'; opcode: $7F; pc:2; param: (p1:MODdq ;p2:GREGdq)),

                               (name: 'CMPPS'; opcode: $C2; pc:2; param: (p1:GREGdq ;p2:MODdq ;p3:IMMb)),
                               (name: 'CMPSS'; opcode: $C2; pc:2; param: (p1:GREGdq ;p2:XMM_M32 ;p3:IMMb)),
// 170..179
                               (name: 'CMPPD'; opcode: $C2; pc:2; param: (p1:GREGdq ;p2:MODdq ;p3:IMMb)),
                               (name: 'CMPSD'; opcode: $C2; pc:2; param: (p1:GREGdq ;p2:XMM_M64 ;p3:IMMb)),
                               (name: 'MOVNTI'; opcode: $C3; pc:2; param: (p1:M32 ;p2:GREGd)),
                               (name: 'PINSRW'; opcode: $C4; pc:2; param: (p1:GREGq ;p2:REG32_M16 ;p3:IMMb)),
                               (name: 'PINSRW'; opcode: $C4; pc:2; param: (p1:GREGdq ;p2:REG32_M16 ;p3:IMMb)),
                               (name: 'PEXTRW'; opcode: $C5; pc:2; param: (p1:GREGd ;p2:GREGq ;p3:IMMb)),     // Vzdy prisluchajuce registre !!!
                               (name: 'PEXTRW'; opcode: $C5; pc:2; param: (p1:GREGd ;p2:GREGdq ;p3:IMMb)),    // Vzdy prisluchajuce registre !!!
                               (name: 'SHUFPS'; opcode: $C6; pc:2; param: (p1:GREGdq ;p2:MODdq ;p3:IMMb)),
                               (name: 'SHUFPD'; opcode: $C6; pc:2; param: (p1:GREGdq ;p2:MODdq ;p3:IMMb)),

                               (name: 'PSRLW'; opcode: $D1; pc:2; param: (p1:GREGdq ;p2:MODdq)),
// 180..189
                               (name: 'PSRLD'; opcode: $D2; pc:2; param: (p1:GREGdq ;p2:MODdq)),
                               (name: 'PSRLQ'; opcode: $D3; pc:2; param: (p1:GREGdq ;p2:MODdq)),
                               (name: 'PADDQ'; opcode: $D4; pc:2; param: (p1:GREGq ;p2:MODq)),
                               (name: 'PADDQ'; opcode: $D4; pc:2; param: (p1:GREGdq ;p2:MODdq)),
                               (name: 'PMULLW'; opcode: $D5; pc:2; param: (p1:GREGdq ;p2:MODdq)),
                               (name: 'MOVQ'; opcode: $D6; pc:2; param: (p1:XMM_M64 ;p2:GREGDq)),
                               (name: 'MOVQ2DQ'; opcode: $D6; pc:2; param: (p1:GREGdq ;p2:MODq)),
                               (name: 'MOVDQ2Q'; opcode: $D6; pc:2; param: (p1:GREGq ;p2:MODDq)),
                               (name: 'PMOVMSKB'; opcode: $D7; pc:2; param: (p1:GREGd ;p2:GREGq)),     // Vzdy prisluchajuce registre !!!
                               (name: 'PMOVMKSB'; opcode: $D7; pc:2; param: (p1:GREGd ;p2:GREGdq)),    // Vzdy prisluchajuce registre !!!
// 190..199
                               (name: 'PSUBUSB'; opcode: $D8; pc:2; param: (p1:GREGdq ;p2:MODdq)),
                               (name: 'PSUBUSW'; opcode: $D9; pc:2; param: (p1:GREGdq ;p2:MODdq)),
                               (name: 'PMINUB'; opcode: $DA; pc:2; param: (p1:GREGq ;p2:MODq)),
                               (name: 'PMINUB'; opcode: $DA; pc:2; param: (p1:GREGdq ;p2:MODdq)),
                               (name: 'PAND'; opcode: $DB; pc:2; param: (p1:GREGdq ;p2:MODdq)),
                               (name: 'PADDUSB'; opcode: $DC; pc:2; param: (p1:GREGdq ;p2:MODdq)),
                               (name: 'PADDUSW'; opcode: $DD; pc:2; param: (p1:GREGdq ;p2:MODdq)),
                               (name: 'PMAXUB'; opcode: $DE; pc:2; param: (p1:GREGq ;p2:MODq)),
                               (name: 'PMAXUB'; opcode: $DE; pc:2; param: (p1:GREGdq ;p2:MODdq)),
                               (name: 'PANDN'; opcode: $DF; pc:2; param: (p1:GREGdq ;p2:MODdq)),
// 200..209
                               (name: 'PAVGB'; opcode: $E0; pc:2; param: (p1:GREGq ;p2:MODq)),
                               (name: 'PAVGB'; opcode: $E0; pc:2; param: (p1:GREGdq ;p2:MODdq)),
                               (name: 'PSRAW'; opcode: $E1; pc:2; param: (p1:GREGdq ;p2:MODdq)),
                               (name: 'PSRAD'; opcode: $E2; pc:2; param: (p1:GREGdq ;p2:MODdq)),
                               (name: 'PAVGW'; opcode: $E3; pc:2; param: (p1:GREGq ;p2:MODq)),
                               (name: 'PAVGW'; opcode: $E3; pc:2; param: (p1:GREGdq ;p2:MODdq)),
                               (name: 'PMULHUW'; opcode: $E4; pc:2; param: (p1:GREGq ;p2:MODq)),
                               (name: 'PMULHUW'; opcode: $E4; pc:2; param: (p1:GREGdq ;p2:MODdq)),
                               (name: 'PMULHW'; opcode: $E5; pc:2; param: (p1:GREGdq ;p2:MODdq)),
                               (name: 'CVTPD2DQ'; opcode: $E6; pc:2; param: (p1:GREGdq ;p2:MODdq)),
// 210..219
                               (name: 'CVTTPD2DQ'; opcode: $E6; pc:2; param: (p1:GREGdq ;p2:MODdq)),
                               (name: 'CVTDQ2PD'; opcode: $E6; pc:2; param: (p1:GREGdq ;p2:XMM_M64)),
                               (name: 'MOVNTQ'; opcode: $E7; pc:2; param: (p1:M64 ;p2:GREGq)),
                               (name: 'MOVNTDQ'; opcode: $E7; pc:2; param: (p1:M128 ;p2:GREGdq)),
                               (name: 'PSUBSB'; opcode: $E8; pc:2; param: (p1:GREGdq ;p2:MODdq)),
                               (name: 'PSUBSW'; opcode: $E9; pc:2; param: (p1:GREGdq ;p2:MODdq)),
                               (name: 'PMINSW'; opcode: $EA; pc:2; param: (p1:GREGq ;p2:MODq)),
                               (name: 'PMINSW'; opcode: $EA; pc:2; param: (p1:GREGdq ;p2:MODdq)),
                               (name: 'POR'; opcode: $EB; pc:2; param: (p1:GREGdq ;p2:MODdq)),
                               (name: 'PADDSB'; opcode: $EC; pc:2; param: (p1:GREGdq ;p2:MODdq)),
// 220..229
                               (name: 'PADDSW'; opcode: $ED; pc:2; param: (p1:GREGdq ;p2:MODdq)),
                               (name: 'PMAXSW'; opcode: $EE; pc:2; param: (p1:GREGq ;p2:MODq)),
                               (name: 'PMAXSW'; opcode: $EE; pc:2; param: (p1:GREGdq ;p2:MODdq)),
                               (name: 'PXOR'; opcode: $EF; pc:2; param: (p1:GREGdq ;p2:MODdq)),

                               (name: 'PSLLW'; opcode: $F1; pc:2; param: (p1:GREGdq ;p2:MODdq)),
                               (name: 'PSLLD'; opcode: $F2; pc:2; param: (p1:GREGdq ;p2:MODdq)),
                               (name: 'PSLLQ'; opcode: $F3; pc:2; param: (p1:GREGdq ;p2:MODdq)),
                               (name: 'PMULUDQ'; opcode: $F4; pc:2; param: (p1:GREGq ;p2:MODq)),
                               (name: 'PMULUDQ'; opcode: $F4; pc:2; param: (p1:GREGdq ;p2:MODdq)),
//                               (name: 'PMADDWD'; opcode: $F5; pc:2; param: (p1:GREGq ;p2:MODq)),
// 230..239  odteraz je spravne cislo
                               (name: 'PMADDWD'; opcode: $F5; pc:2; param: (p1:GREGdq ;p2:MODdq)),
                               (name: 'PSADBW'; opcode: $F6; pc:2; param: (p1:GREGq ;p2:MODq)),
                               (name: 'PSADBW'; opcode: $F6; pc:2; param: (p1:GREGdq ;p2:MODdq)),
                               (name: 'MASKMOVQ'; opcode: $F7; pc:2; param: (p1:GREGdq ;p2:MODdq)),
                               (name: 'MASKMOVDQU'; opcode: $F7; pc:2; param: (p1:GREGdq ;p2:MODdq)),
                               (name: 'PSUBB'; opcode: $F8; pc:2; param: (p1:GREGdq ;p2:MODdq)),
                               (name: 'PSUBW'; opcode: $F9; pc:2; param: (p1:GREGdq ;p2:MODdq)),
                               (name: 'PSUBD'; opcode: $FA; pc:2; param: (p1:GREGdq ;p2:MODdq)),
                               (name: 'PSUBQ'; opcode: $FB; pc:2; param: (p1:GREGq ;p2:MODq)),
                               (name: 'PSUBQ'; opcode: $FB; pc:2; param: (p1:GREGdq ;p2:MODdq)),
// 240..242
                               (name: 'PADDB'; opcode: $FC; pc:2; param: (p1:GREGdq ;p2:MODdq)),
                               (name: 'PADDW'; opcode: $FD; pc:2; param: (p1:GREGdq ;p2:MODdq)),
                               (name: 'PADDD'; opcode: $FE; pc:2; param: (p1:GREGdq ;p2:MODdq)),

// GROUP INSTRUCTIONS

// 243..249
                   // GROUP 12:
                               (name: 'PSRLW'; opcode: $0; pc:2; param: (p1:R64 ;p2:IMMb)),
                               (name: 'PSRLW'; opcode: $0; pc:2; param: (p1:R128 ;p2:IMMb)),
                               (name: 'PSRAW'; opcode: $0; pc:2; param: (p1:R64 ;p2:IMMb)),
                               (name: 'PSRAW'; opcode: $0; pc:2; param: (p1:R128 ;p2:IMMb)),
                               (name: 'PSLLW'; opcode: $0; pc:2; param: (p1:R64 ;p2:IMMb)),
                               (name: 'PSLLW'; opcode: $0; pc:2; param: (p1:R128 ;p2:IMMb)),
                   // GROUP 13:
                               (name: 'PSRLD'; opcode: $0; pc:2; param: (p1:R64 ;p2:IMMb)),
// 250..259
                               (name: 'PSRLD'; opcode: $0; pc:2; param: (p1:R128 ;p2:IMMb)),
                               (name: 'PSRAD'; opcode: $0; pc:2; param: (p1:R64 ;p2:IMMb)),
                               (name: 'PSRAD'; opcode: $0; pc:2; param: (p1:R128 ;p2:IMMb)),
                               (name: 'PSLLD'; opcode: $0; pc:2; param: (p1:R64 ;p2:IMMb)),
                               (name: 'PSLLD'; opcode: $0; pc:2; param: (p1:R128 ;p2:IMMb)),
                   // GROUP 14:
                               (name: 'PSRLQ'; opcode: $0; pc:2; param: (p1:R64 ;p2:IMMb)),
                               (name: 'PSRLQ'; opcode: $0; pc:2; param: (p1:R128 ;p2:IMMb)),
                               (name: 'PSRLDQ'; opcode: $0; pc:2; param: (p1:R128 ;p2:IMMb)),
                               (name: 'PSLLQ'; opcode: $0; pc:2; param: (p1:R64 ;p2:IMMb)),
                               (name: 'PSLLQ'; opcode: $0; pc:2; param: (p1:R128 ;p2:IMMb)),
// 260.
                               (name: 'PSLLDQ'; opcode: $0; pc:2; param: (p1:R128 ;p2:IMMb))


//                               (name: ''; opcode: $; pc:2; param: (p1:q ;p2:q)),

                                                );
const _3DNowInstruction_set:array [1..Pocet3DNowInstrukcii] of TInstruction = (

                               (name: 'PI2FW'; opcode: $0C; pc:2; param: (p1:GREGq ;p2:MODq)),
                               (name: 'PI2FD'; opcode: $0D; pc:2; param: (p1:GREGq ;p2:MODq)),
                               (name: 'PF2IW'; opcode: $1C; pc:2; param: (p1:GREGq ;p2:MODq)),
                               (name: 'PF2ID'; opcode: $1D; pc:2; param: (p1:GREGq ;p2:MODq)),

                               (name: 'PFNACC'; opcode: $8A; pc:2; param: (p1:GREGq ;p2:MODq)),
                               (name: 'PFPNACC'; opcode: $8E; pc:2; param: (p1:GREGq ;p2:MODq)),

                               (name: 'PFCMPGE'; opcode: $90; pc:2; param: (p1:GREGq ;p2:MODq)),
                               (name: 'PFMIN'; opcode: $94; pc:2; param: (p1:GREGq ;p2:MODq)),
                               (name: 'PFRCP'; opcode: $96; pc:2; param: (p1:GREGq ;p2:MODq)),
                               (name: 'PFRSQRT'; opcode: $97; pc:2; param: (p1:GREGq ;p2:MODq)),
                               (name: 'PFSUB'; opcode: $9A; pc:2; param: (p1:GREGq ;p2:MODq)),
                               (name: 'PFADD'; opcode: $9E; pc:2; param: (p1:GREGq ;p2:MODq)),

                               (name: 'PFCMPGT'; opcode: $A0; pc:2; param: (p1:GREGq ;p2:MODq)),
                               (name: 'PFMAX'; opcode: $A4; pc:2; param: (p1:GREGq ;p2:MODq)),
                               (name: 'PFRCPIT1'; opcode: $A6; pc:2; param: (p1:GREGq ;p2:MODq)),
                               (name: 'PFRSQIT1'; opcode: $A7; pc:2; param: (p1:GREGq ;p2:MODq)),
                               (name: 'PFSUBR'; opcode: $AA; pc:2; param: (p1:GREGq ;p2:MODq)),
                               (name: 'PFACC'; opcode: $AE; pc:2; param: (p1:GREGq ;p2:MODq)),

                               (name: 'PFCMPEQ'; opcode: $B0; pc:2; param: (p1:GREGq ;p2:MODq)),
                               (name: 'PFMUL'; opcode: $B4; pc:2; param: (p1:GREGq ;p2:MODq)),
                               (name: 'PFRCPIT2'; opcode: $B6; pc:2; param: (p1:GREGq ;p2:MODq)),
                               (name: 'PMULHRW'; opcode: $B7; pc:2; param: (p1:GREGq ;p2:MODq)),
                               (name: 'PSWAPD'; opcode: $BB; pc:2; param: (p1:GREGq ;p2:MODq)),
                               (name: 'PAVGUSB'; opcode: $BF; pc:2; param: (p1:GREGq ;p2:MODq))

//                               (name: ''; opcode: $; pc:2; param: (p1:q ;p2:q)),

                                       );

const genreg:array[1..8] of string[5]=(
                                        ('[EAX]'),
                                        ('[ECX]'),
                                        ('[EDX]'),
                                        ('[EBX]'),
                                        ('[ESP]'),
                                        ('[EBP]'),
                                        ('[ESI]'),
                                        ('[EDI]')
                                       );
const onebyte_register:array[0..7] of string[2]= (
                                                  ('AL'),
                                                  ('CL'),
                                                  ('DL'),
                                                  ('BL'),
                                                  ('AH'),
                                                  ('CH'),
                                                  ('DH'),
                                                  ('BH')
                                                  );
const TWObyte_register:array[0..7] of string[2]= (
                                                  ('AX'),
                                                  ('CX'),
                                                  ('DX'),
                                                  ('BX'),
                                                  ('SP'),
                                                  ('BP'),
                                                  ('SI'),
                                                  ('DI')
                                                  );
const FourByte_register:array[0..7] of string[3]= (
                                                  ('EAX'),
                                                  ('ECX'),
                                                  ('EDX'),
                                                  ('EBX'),
                                                  ('ESP'),
                                                  ('EBP'),
                                                  ('ESI'),
                                                  ('EDI')
                                                  );
const Segment_register:array[0..7] of string[2] = (
                                                  ('ES'),
                                                  ('CS'),
                                                  ('SS'),
                                                  ('DS'),
                                                  ('FS'),
                                                  ('GS'),
                                                  ('??'),
                                                  ('??')
                                                  );
const Modrm16bitAddress_register:array[0..7] of string = (
                                                         ('BX+SI'),
                                                         ('BX+DI'),
                                                         ('BP+SI'),
                                                         ('BP+DI'),
                                                         ('SI'),
                                                         ('DI'),
                                                         ('BP'),
                                                         ('BX')
                                                         );

const control_register:array[0..7]of string[3] = (
                                                 ('CR0'),
                                                 ('???'),
                                                 ('CR2'),
                                                 ('CR3'),
                                                 ('CR4'),
                                                 ('???'),
                                                 ('???'),
                                                 ('???')
                                                 );
const debug_register:array[0..7] of string[3] = (
                                                ('DR0'),
                                                ('DR1'),
                                                ('DR2'),
                                                ('DR3'),
                                                ('DR4'),
                                                ('DR5'),
                                                ('DR6'),
                                                ('DR7')
                                                );
const test_register:array[0..7] of string[3] = (
                                                ('???'),
                                                ('???'),
                                                ('???'),
                                                ('TR3'),
                                                ('TR4'),
                                                ('TR5'),
                                                ('TR6'),
                                                ('TR7')
                                                );
const mmx_register:array[0..7] of string[3] = (
                                              ('MM0'),
                                              ('MM1'),
                                              ('MM2'),
                                              ('MM3'),
                                              ('MM4'),
                                              ('MM5'),
                                              ('MM6'),
                                              ('MM7')
                                              );

const xmm_register:array[0..7] of string[4] = (
                                              ('XMM0'),
                                              ('XMM1'),
                                              ('XMM2'),
                                              ('XMM3'),
                                              ('XMM4'),
                                              ('XMM5'),
                                              ('XMM6'),
                                              ('XMM7')
                                              );

var prefixes:TPrefixes;

function ByteToSignedHex(a:byte):string;
begin
  if abs(shortint(a))=a then result:='+0x'+inttohex(a,2)
  else result:='-0x'+inttohex(abs(shortint(a)),2);
end;

{
function TDisassembler.LoadModRM(a:byte): TModRM;
var vysledok:TModrm;
begin
  asm
    mov ah,a                                // Operatory shl a shr v Delphi pracuju s integer (4 bajty)
    shr ah,6                                // a preto pouzivam assembler (na 1 bajt)
    mov vysledok.moder,ah
    mov ah,a
    shl ah,2
    shr ah,5
    mov vysledok.regop,ah
    mov ah,a
    shl ah,5
    shr ah,5
    mov vysledok.rm,ah
    mov vysledok.loaded,byte ptr 0
  end;
//  vysledok.loaded:=false;
  loadmodrm:=vysledok;
end;
}

function TDisassembler.LoadModRM(a:byte): TModRM; // EDI <- self(asi), DL <- a
//begin
//  result:=XLoadModRM(a);      pokus s DLL
asm                               // ModRM = 2 + 3 + 3 bity (moder + regop + rm)
  and edx,$000000FF
  mov dh,dl
  shr dh,6
  mov [ecx],dx      // rychlejsie je mozno: mov [ecx],edx
  mov dh,dl
  shl dl,2
  shr dl,5
  and dh,7
  mov [ecx+2],edx

{
  mov [ecx],dl                     //  ECX <- pointer na Result: TSIB
  mov al,a
  shr al,6
  mov [ecx+1],al                  // Result.moder:=al
  mov al,a
  shl al,2
  shr al,5
  mov [ecx+2],al                  // Result.regop:=al
  and dl,111b
  mov [ecx+3],dl                   // Result.rm:=al
  mov byte ptr [ecx+4],0          // Result.loaded:=0
}
end;

{
function TDisassembler.LoadSIB(a:byte):tSIB;
var vysledok:TSIB;
begin
  asm
    mov ah,a                                // Operatory shl a shr v Delphi pracuju s integer (4 bajty)
    shr ah,6                                // a preto pouzivam assembler (na 1 bajt)
    mov vysledok.scale,ah
    mov ah,a
    shl ah,2
    shr ah,5
    mov vysledok.index,ah
    mov ah,a
    shl ah,5
    shr ah,5
    mov vysledok.base,ah
  end;
  vysledok.loaded:=false;
  loadsib:=vysledok;
end;
}

function TDisassembler.LoadSIB(a:byte):TSIB; // EDI <- self(asi), EDX <- a
asm                               // SIB = 2 + 3 + 3 bity (scale + index + base)
  and edx,$000000FF
  mov dh,dl
  shr dh,6
  mov [ecx],dx      // rychlejsie je mozno: mov [ecx],edx
  mov dh,dl
  shl dl,2
  shr dl,5
  and dh,7
  mov [ecx+2],edx

{
  mov [ecx],dl                     //  ECX <- pointer na Result: TSIB
  mov al,a
  shr al,6
  mov [ecx+1],al                  //  Result.scale:=ah
  mov al,a
  shl al,2
  shr al,5
  mov [ecx+2],al                  //  Result.index:=ah
  and a,111b
  mov [ecx+3],dl                   //  Result.base:=ah
  mov byte ptr [ecx+4],0          //  Result.loaded:=0
}
end;

function TDisassembler.SpracujSIB:string;
begin
  inc(i);
  sib:=loadsib(code[i]);
  disassembled[InstrAddress].parsed:=disassembled[InstrAddress].parsed+inttohex(code[i],2);
  if not((modrm.moder=0)and(sib.base=5)) then begin
//    result:=result+fourbyte_register[sib.base];
    result:=fourbyte_register[sib.base];

    if sib.index<>4 then begin
      result:=result + '+' + fourbyte_register[sib.index];
      if sib.scale<>0 then result:=result+'*'+inttostr(round(intpower(2,sib.scale)));
    end;
  end
  else begin
//    result:=result+'0x'+readfourbytes;
    result:='0x'+readfourbytes;

    if sib.index<>4 then begin
      result:=result + '+' + fourbyte_register[sib.index];
      if sib.scale<>0 then result:=result+'*'+inttostr(round(intpower(2,sib.scale)));
    end;
  end;
end;

function TDisassembler.ReadFourBytes:string;
var
    value: cardinal;
    parsed: cardinal;
begin
  inc(i);
  asm
    mov eax,self
    mov ecx,[eax].code
    add ecx,[eax].i
    mov eax,[ecx]
    mov ecx,eax
    bswap ecx
    mov value,eax
    mov parsed,ecx
  end;
  inc(i,3);
  result:=inttohex(value,8);
  disassembled[InstrAddress].parsed:=disassembled[InstrAddress].parsed + IntToHex(parsed,8);
end;

function TDisassembler.SpracujModRM(a:modrmparametertype; b:modrmparametersize):string;
begin
  result:='';

  if not modrm.loaded then begin
    inc(i);
    disassembled[InstrAddress].parsed:=disassembled[InstrAddress].parsed + inttohex(code[i],2);
    modrm:=loadmodrm(code[i]);
    modrm.loaded:=true;
  end;

  case address32 of
    false: begin
             case a of
               GReg: begin
                       case b of
                         onebyte: result:=onebyte_register[modrm.regop];
                         twobyte: result:=twobyte_register[modrm.regop];
                         twoorfour: if operand32 then result:=fourbyte_register[modrm.regop]
                                    else result:=twobyte_register[modrm.regop];
                         fourbyte: result:=fourbyte_register[modrm.regop];
                         mmx: result:=mmx_register[modrm.regop];
                         xmm: result:=xmm_register[modrm.regop];
                       end;
                     end;

              Reg: begin
                     if modrm.moder = 3 then
                       case b of
                         onebyte: result:=onebyte_register[modrm.rm];
                         twobyte: result:=twobyte_register[modrm.rm];
                         twoorfour: if operand32 then result:=fourbyte_register[modrm.rm]
                                    else result:=twobyte_register[modrm.rm];
                         fourbyte: result:=fourbyte_register[modrm.rm];
                         mmx: result:=mmx_register[modrm.rm];
                         xmm: result:=xmm_register[modrm.rm];
                       end
                     else result:='Invalid parameter';
                   end;

               Mem: begin
                          case modrm.moder of
                            0:begin
                                result:='[' + SegmentOverride;
                                if modrm.rm=6 then begin
                                  inc(i,2);
                                  result:=result+'0x'+inttohex(code[i],2)+inttohex(code[i-1],2);
                                  disassembled[InstrAddress].parsed:=disassembled[InstrAddress].parsed+inttohex(code[i-1],2)+inttohex(code[i],2);
                                end
                                else result:=result+modrm16bitaddress_register[modrm.rm];
                                result:=result+']';
                              end;
                            1:begin
                                inc(i);
//                                result:='[' + SegmentOverride + modrm16bitaddress_register[modrm.rm]+'0x'+ByteToSignedHex(code[i])+']';
                                result:='[' + SegmentOverride + modrm16bitaddress_register[modrm.rm]+ByteToSignedHex(code[i])+']';
                                disassembled[InstrAddress].parsed:=disassembled[InstrAddress].parsed+inttohex(code[i],2);
                              end;
                            2:begin
                                inc(i,2);
                                result:='[' + SegmentOverride + modrm16bitaddress_register[modrm.rm]+'+'+'0x'+inttohex(code[i],2)+inttohex(code[i-1],2)+']';
                                disassembled[InstrAddress].parsed:=disassembled[InstrAddress].parsed+inttohex(code[i-1],2)+inttohex(code[i],2);
                              end;
                            3:result:='Invalid parameter!';
                          end;
                          case OperandSize of
                            szByte: result:='byte '+result;
                            szWord: result:='word '+result;
                            szDword: result:='dword '+result;
                            szQword: result:='qword '+result;
                            szTword: result:='tword '+result;
                          end;
                        end;

               RegMem: begin
                          case modrm.moder of
                            0:begin
                                result:='[' + SegmentOverride;
                                if modrm.rm=6 then begin
                                  inc(i,2);
                                  result:=result+'0x'+inttohex(code[i],2)+inttohex(code[i-1],2);
                                  disassembled[InstrAddress].parsed:=disassembled[InstrAddress].parsed+inttohex(code[i-1],2)+inttohex(code[i],2);
                                end
                                else result:=result+modrm16bitaddress_register[modrm.rm];
                                result:=result+']';
                              end;
                            1:begin
                                inc(i);
                                result:='[' + SegmentOverride + modrm16bitaddress_register[modrm.rm]+ByteToSignedHex(code[i])+']';
                                disassembled[InstrAddress].parsed:=disassembled[InstrAddress].parsed+inttohex(code[i],2);
                              end;
                            2:begin
                                inc(i,2);
                                result:='[' + SegmentOverride + modrm16bitaddress_register[modrm.rm]+'+'+'0x'+inttohex(code[i],2)+inttohex(code[i-1],2)+']';
                                disassembled[InstrAddress].parsed:=disassembled[InstrAddress].parsed+inttohex(code[i-1],2)+inttohex(code[i],2);
                              end;
                            3:begin
                                case b of
                                  onebyte: result:=onebyte_register[modrm.rm];
                                  twobyte: result:=twobyte_register[modrm.rm];
                                  twoorfour: if operand32 then result:=fourbyte_register[modrm.rm]
                                           else result:=twobyte_register[modrm.rm];
                                  fourbyte: result:=fourbyte_register[modrm.rm];
                                  FourOrSix: if operand32 then result:=fourbyte_register[modrm.rm]
                                           else result:=twobyte_register[modrm.rm];
                                  mmx: result:=mmx_register[modrm.rm];
                                  xmm: result:=xmm_register[modrm.rm];
                                  R32_M16: result:=fourbyte_register[modrm.rm];
                                  R128_M32: result:=xmm_register[modrm.rm];
                                  R128_M64: result:=xmm_register[modrm.rm];
                                end;
                              end;
                          end;
                        end;
              sreg: begin
                      case b of
                        twobyte: result:=segment_register[modrm.regop];
                      end;
                    end;
              creg: begin
                      result:=control_register[modrm.regop];
                    end;
              dreg: begin
                      result:=debug_register[modrm.regop];
                    end;
              treg: begin
                      result:=test_register[modrm.regop];
                    end;

             end;
           end;
    true: begin
            case a of
              Greg: begin
                      case b of
                        onebyte: result:=onebyte_register[modrm.regop];
                        twobyte: result:=twobyte_register[modrm.regop];
                        twoorfour: if operand32 then result:=fourbyte_register[modrm.regop]
                                   else result:=twobyte_register[modrm.regop];
                        fourbyte: result:=fourbyte_register[modrm.regop];
                        mmx: result:=mmx_register[modrm.regop];
                        xmm: result:=xmm_register[modrm.regop];
                      end;
                    end;     //hotovo
              Reg: begin
                     if modrm.moder = 3 then
                       case b of
                         onebyte: result:=onebyte_register[modrm.rm];
                         twobyte: result:=twobyte_register[modrm.rm];
                         twoorfour: if operand32 then result:=fourbyte_register[modrm.rm]
                                    else result:=twobyte_register[modrm.rm];
                         fourbyte: result:=fourbyte_register[modrm.rm];
                         mmx: result:=mmx_register[modrm.rm];
                         xmm: result:=xmm_register[modrm.rm];
                       end
                     else result:='Invalid parameter';
                   end;

              Mem: begin
                         case modrm.moder of
                           0: begin
                                result:='[' + SegmentOverride;
                                case modrm.rm of
                                  5: result:=result+'0x'+readfourbytes;
//                                  4: result:=spracujsib;
                                  4: result:=result + spracujsib;
                                else result:=result+fourbyte_register[modrm.rm];
                                end;
                                result:=result+']';
                              end;
                           1: begin
                                result:='[' + SegmentOverride;
                                if modrm.rm = 4 then result:=result + spracujsib
                                else result:=result + fourbyte_register[modrm.rm];
                                inc(i);
                                result:=result+ByteToSignedHex(code[i])+']';
                                disassembled[InstrAddress].parsed:=disassembled[InstrAddress].parsed+inttohex(code[i],2);
                              end;
                           2: begin
                                result:='[' + SegmentOverride;
                                if modrm.rm = 4 then result:=result + spracujsib
                                else result:=result + fourbyte_register[modrm.rm];
                                result:=result + '+' + '0x'+readfourbytes+']';
                              end;
                           3: result:='Invalid parameter!';
                         end; //end of "case moder"
                         case OperandSize of
                           szByte: result:='byte '+result;
                           szWord: result:='word '+result;
                           szDword: result:='dword '+result;
                           szQword: result:='qword '+result;
                           szTword: result:='tword '+result;
                         end;
                       end; // end of Mem
              RegMem: begin
                         case modrm.moder of
                           0: begin
                                result:='[' + SegmentOverride;
                                case modrm.rm of
                                  5: result:=result+'0x'+readfourbytes;
                                  4: result:=result+spracujsib;
                                else result:=result+fourbyte_register[modrm.rm];
                                end;
                                result:=result+']';
                                case OperandSize of
                                  szByte: result:='byte '+result;
                                  szWord: result:='word '+result;
                                  szDword: result:='dword '+result;
                                end;
//                                  if operand32 then result:='dword '+result
//                                  else result:='word '+result;

                              end;
                           1: begin
                                result:='[' + SegmentOverride;
                                if modrm.rm = 4 then result:=result + spracujsib
                                else result:=result + fourbyte_register[modrm.rm];
                                inc(i);
                                result:=result+ByteToSignedHex(code[i])+']';
                                disassembled[InstrAddress].parsed:=disassembled[InstrAddress].parsed+inttohex(code[i],2);

                                case OperandSize of
                                  szByte: result:='byte '+result;
                                  szWord: result:='word '+result;
                                  szDword: result:='dword '+result;
                                end;

                              end;
                           2: begin
                                result:='[' + SegmentOverride;
                                if modrm.rm = 4 then result:=result + spracujsib
                                else result:=result + fourbyte_register[modrm.rm];
                                result:=result + '+' + '0x'+readfourbytes+']';

                                case OperandSize of
                                  szByte: result:='byte '+result;
                                  szWord: result:='word '+result;
                                  szDword: result:='dword '+result;
                                end;

                              end;
                           3: begin
                                case b of
                                  onebyte: result:=onebyte_register[modrm.rm];
                                  twobyte: result:=twobyte_register[modrm.rm];
                                  twoorfour: if operand32 then result:=fourbyte_register[modrm.rm]
                                           else result:=twobyte_register[modrm.rm];
                                  fourbyte: result:=fourbyte_register[modrm.rm];
                                  mmx: result:=mmx_register[modrm.rm];
                                  xmm: result:=xmm_register[modrm.rm];
                                  R32_M16: result:=fourbyte_register[modrm.rm];
                                  R128_M32: result:=xmm_register[modrm.rm];
                                  R128_M64: result:=xmm_register[modrm.rm];
                                end;
                              end;
                         end; //end of "case moder"
                       end; // end of RegMem
              sreg: begin
                      case b of
                        twobyte: result:=segment_register[modrm.regop];
                      end;
                    end;
              creg: begin
                      result:=control_register[modrm.regop];
                    end;
              dreg: begin
                      result:=debug_register[modrm.regop];
                    end;
              treg: begin
                      result:=test_register[modrm.regop];
                    end;

            end; // End of "case parameter type
          end; //End of "true address32"
  end; // End of "case address32"
end; // End of ModRM


function TDisassembler.SpracujImmediate(a:ModRMParameterSize):string;
var
    immediate: cardinal;
    immediate2: word;
    parsed: cardinal;
    parsed2: word;
    parsedcount: byte;
begin
  result:='';
 asm
  push esi
  push ebx
  mov esi,self       // pointer na instanciu TDisassembler
  mov ebx,[esi].i    // index v poli code
  inc ebx
  mov eax,[esi].code // adresa pola code
  add eax,ebx        // adresa prvku pola v pamati
  xor ecx,ecx        // vysledny immediate
  xor edx,edx        // vysledny PARSED
// Case
  cmp a,OneByte           // ONE BYTE
  jne @CheckTwobyte
  movzx ecx,byte[eax]     // parameter instrukcie -> ECX
  mov edx,ecx
  mov parsedcount,2       // pocet cifier PARSED
  jmp @EndCase

@CheckTwobyte:
  cmp a,TwoByte           // TWO BYTE
  jne @CheckTwoOrFour

@_16:
  movzx ecx,word ptr[eax]
  mov dl,ch
  mov dh,cl
  mov parsedcount,4
  inc ebx
  jmp @EndCase

@CheckTwoOrFour:
  cmp a,TwoOrFour
  jne @CheckFourOrSix
  cmp byte ptr [esi].operand32,0
  je @_16

@_32:
  mov ecx,[eax] //  predtym: mov ecx,cardinal ptr [eax]

  mov edx,ecx
  bswap edx
  mov parsedcount,8
  add ebx,3
  jmp @EndCase

@CheckFourOrSix:
  cmp byte ptr [esi].operand32,0
  je @_16_16

@_16_32:
  mov ecx,[eax] // predtym: mov ecx,cardinal ptr [eax]
  mov edx,ecx
  bswap edx
  mov parsedcount,8
  add eax,4
  add ebx,4
  jmp @_segment

@_16_16:
//  movzx ecx,word ptr [eax]
  mov cx, word ptr [eax]

  mov dl,ch
  mov dh,cl
  mov parsedcount,4
  add eax,2
  add ebx,2

@_segment:
  mov edi,ebx              // zalohujeme EBX do EDI
//  movzx ebx,word ptr [eax]
  mov bx,[eax]

  mov immediate2,bx
  bswap ebx                // vymenime byty v EBX
  shr ebx,16               // a posunieme doprava o 2 byty
  mov parsed2,bx
  mov ebx,edi              // obnovime EBX z EDI
  inc ebx

@EndCase:
  mov immediate,ecx
  mov parsed,edx
  mov [esi].i,ebx

  pop ebx
  pop esi
 end;
  disassembled[InstrAddress].parsed:= disassembled[InstrAddress].parsed + InTtoHex(parsed,parsedcount);
  if a = fourorsix then begin
    result:= '0x'+IntToHex(immediate2,4) + ':';
    disassembled[InstrAddress].parsed := disassembled[InstrAddress].parsed + IntToHex(parsed2,4);
  result:=result + '0x'+IntToHex(immediate,parsedcount);
  end
  else
//    result:=result + '0x'+IntToHex(immediate,parsedcount);

// kvoli kompatibilite s NASM:
    case parsedcount of
      2: result:=result + 'byte 0x'+IntToHex(immediate,parsedcount);
      4: result:=result + 'word 0x'+IntToHex(immediate,parsedcount);
      8: result:=result + 'dword 0x'+IntToHex(immediate,parsedcount);
    end;

end;

function TDisassembler.SpracujRelative(a:modrmparametersize):string;
var
    address: integer;
    parsed: cardinal;
    parsedcount: byte;
begin
asm
  push esi
  push ebx
  mov esi,self       // pointer na instanciu TDisassembler
  mov ebx,[esi].i    // index v poli code
  inc ebx
  mov eax,[esi].code // adresa pola code
  add eax,ebx        // adresa prvku pola v pamati
  xor ecx,ecx        // vysledna adresa skoku
  xor edx,edx        // vysledny Parsed
// Case
{
  cmp a,onebyte           // ONE BYTE
  jne @CheckTwobyte
  movsx ecx,byte[eax]     // parameter instrukcie -> ECX
  mov edx,ecx          //
  mov parsedcount,2       // pocet cifier PARSED
  add ecx,ebx             // pripocitame aktualnu pozicu
  inc ecx                 // zvysime o 1, lebo skok je relativny od nasledujucej instrukcie
  jmp @EndCase
}
  cmp a,onebyte           // ONE BYTE
  jne @CheckTwobyte
  mov dl,byte[eax]     // parameter instrukcie -> ECX
  movsx ecx,dl          //
  mov parsedcount,2       // pocet cifier PARSED
  add ecx,ebx             // pripocitame aktualnu pozicu
  inc ecx                 // zvysime o 1, lebo skok je relativny od nasledujucej instrukcie
  jmp @EndCase


@CheckTwobyte:
  cmp a,twobyte           // TWO BYTE
  jne @CheckTwoOrFour

@TwoByteOnly:
  movsx ecx,word ptr [eax]
  mov dh,cl
  mov dl,ch
  mov parsedcount,4
  inc ebx
  add ecx,ebx             // pricitame aktualny index
  inc ecx
  jmp @EndCase

@CheckTwoOrFour:
  cmp a,twoorfour
  jne @EndCase
  cmp byte ptr [esi].operand32,0
  je @TwoByteOnly

@FourByte:
  mov ecx,[eax] // predtym: mov ecx,cardinal ptr [eax]
  mov edx,ecx
  bswap edx
  mov parsedcount,8
  add ebx,3
  add ecx,ebx
  inc ecx

@EndCase:
  mov cardinal(address),ecx
  mov cardinal(parsed),edx
  mov [esi].i,ebx

  pop ebx
  pop esi

  end;

  disassembled[InstrAddress].parsed:=disassembled[InstrAddress].parsed+inttohex(parsed,parsedcount);

  if address < 0 then result:='-0x' + IntToHex(Abs(address),8)            // skok na zapornu adresu
  else begin
    result:='0x'+IntToHex(address,8);
    if address > CodeSize-1 then Exit                             // skok za koniec kodovej sekcie
    else begin
      CAJ.Add(address);
      Inc(Disassembled[address].refercount);
      SetLength(Disassembled[address].refer,Disassembled[address].refercount);
      case Disassembled[InstrAddress].name[1] of
        'J': Disassembled[address].refer[Disassembled[address].refercount-1]:='Jump from 0x'+IntToHex(InstrAddress,8);
        'C': Disassembled[address].refer[Disassembled[address].refercount-1]:='Call from 0x'+IntToHex(InstrAddress,8);
        'L': Disassembled[address].refer[Disassembled[address].refercount-1]:='Loop from 0x'+IntToHex(InstrAddress,8);
      end;
      Inc(fStatistics.References);
    end;
  end;
{
Instrukcie skokove:
   JMP - opcody E9 (rel8), EB( rel16/32)
   Jxx - opcody 70 - 7F (rel8), 0F 80 - 0F 8F (rel16/32)
   JCXZ - E3 (rel8)
   LOOPxx - opcody E0 (rel8), E1 (rel8), E2 (rel8)
   CALL - opcode E8 (rel16/32)
}
end;

function TDisassembler.spracujgenreg:string;
begin
  if not genreg16 then if operand32 then result:='E' else result:='';
end;

function TDisassembler.SpracujOffset(a:modrmparametersize):string;
begin
  if address32 then result:='['+SegmentOverride+'0x'+readfourbytes+']'
  else begin
    inc(i);
    disassembled[InstrAddress].parsed:=disassembled[InstrAddress].parsed+inttohex(code[i],2);
    inc(i);
    disassembled[InstrAddress].parsed:=disassembled[InstrAddress].parsed+inttohex(code[i],2);
    result:='['+SegmentOverride+'0x'+inttohex(code[i],2)+inttohex(code[i-1],2)+']';
  end;
end;

procedure FPUNastavPoradie(var a:byte; var b:byte; c:byte);
type fputyp1=record pA, pZ:word; end;
const fpupole:array[1..8]of fputyp1=(
                                      (pA: 1; pz: 8),
                                      (pA: 9; pz: 38),
                                      (pA: 39; pz: 43),
                                      (pA: 44; pz: 51),
                                      (pA: 52; pz: 57),
                                      (pA: 58; pz: 62),
                                      (pA: 63; pz: 69),
                                      (pA: 70; pz: 72)
                                      );
begin
  a:=fpupole[(c mod 8)+1].pA;
  b:=fpupole[(c mod 8)+1].pZ;
end;

constructor TDisassembler.Create(SectionCode: TByteDynamicArray; var DisassemblerMap: TByteDynamicArray);
begin
  code:= SectionCode;
  CodeSize:=Length(code)-CodeArrayReserve;
  DisasmMap:=DisassemblerMap;
  SetLength(Disassembled, CodeSize);             // Inicializacia premennych
//  ProgressFunction:=prog;
  CAJ:=TCallsAndJumps.Create(DisasmMap);
  fBlocks:= TDisassembledBlocks.Create;
end;



function TDisassembler.GetDisassembledBlock(Index: integer): TDisassembledBlock;
begin
  result:= fBlocks[Index];
end;



function TDisassembler.GetBlockCount: integer;
begin
  result:= fBlocks.Count;
end;



function TDisassembler.Disassemble():boolean;
var i: integer;
begin
  while CAJ.count > 0 do begin
    for i:=0 to CAJ.Count-1 do
      DisassembleBlock(CAJ[i].start,CAJ[i].finish,true); // treba odstranit posledny parameter
    CAJ.Process(CodeSize);
  end;
  result:=true; // ???
end;


function TDisassembler.DisassembleAll(bit32: boolean): boolean;
var i: integer;
    CodeIndex: cardinal;
begin
  result:=false;
  ProgressPosition:=0;
// 1. faza - disassemblovanie kodu
  while CAJ.Count > 0 do begin
    for i:=0 to CAJ.Count-1 do begin
{
  Ked je zapnuty Range Checking, tak
  metoda DisassembleBlock pri subore 'pohlad.exe' (aj inych) zmeni register EDI, ktory sluzi na pocitanie v cykle "for",
  preto treba register EDI ulozit na zasobnik a potom ho znova vybrat
}
{$IFDEF DELPHI}
      asm
        push edi
      end;
{$ENDIF}
      if not DisassembleBlock(CAJ[i].start, CAJ[i].finish, bit32) then Exit;
{$IFDEF DELPHI}
      asm
        pop edi
      end;
{$ENDIF}
    end;
    CAJ.Process(CodeSize);
  end;

  // 2. faza - najdenie zaciatkov procedur pomocou standartnych instrukcii
  if CodeSize >=3 then begin
    for i:=0 to CodeSize-3 do                     // nespracovany byty kodu
      if DisasmMap[i] = dfNone then begin
        case code[i] of
          $55:       // PUSH BP
            asm
              mov eax,self
              mov eax,[eax].code
              add eax,i
              inc eax
              mov ax,[eax]
              cmp ax,$EC8B  // 8BEC = MOV (E)BP,(E)SP
              je @nasiel
              cmp ax,$E589  // 89E5 = MOV (E)BP,(E)SP
              je @nasiel
              jmp @koniec
            @nasiel:
              mov edx,i
              mov eax,self
              mov eax,[eax].CAJ
              call TCallsAndJumps.Add
            @koniec:
          end;
        end;
      end;
  end;

  //  Disassemblovanie najdenych procedur a kodu na ktory sa odkazuju
  CAJ.Process(CodeSize);
  while CAJ.Count > 0 do begin
    for i:=0 to CAJ.Count-1 do begin
      if not DisassembleBlock(CAJ[i].start, CAJ[i].finish, bit32) then Exit;
    end;
    CAJ.Process(CodeSize);
  end;

  // 3. faza - nepouzitie bajty zmenime na data typu UNSIGNED BYTE
  for CodeIndex:=0 to CodeSize-1 do
    if DisasmMap[CodeIndex] = dfNone then begin     // nespracovany byty kodu
{
      inc(counter);
      if (counter > 1000) then begin
        inc(ProgressPosition,counter);
        if not ProgressFunction(ProgressPosition,CodeSize,'') then begin
          Exit;
        end;
        counter:=0;
      end;
}
      Disassembled[CodeIndex].address:=CodeIndex;
      Disassembled[CodeIndex].parsed:=IntToHex(code[CodeIndex],2);
      Disassembled[CodeIndex].name:='byte';
      if (code[CodeIndex]=0) or (code[CodeIndex]=$0A) or (code[CodeIndex]=$0D) then
        Disassembled[CodeIndex].operandy:='0x'+IntToHex(code[CodeIndex],2) + ' '''''
      else
        Disassembled[CodeIndex].operandy:='0x'+IntToHex(code[CodeIndex],2) + ' ''' + Chr(code[CodeIndex]) + '''';
//      Inc(DisasmMap[CodeIndex],dfData);
      Inc(fStatistics.Data);
    end;

  // Vypocet statistickych udajov
  fStatistics.InstructionBytes:= CodeSize - fStatistics.Data;
  i:=integer(CodeSize)-1;
  while (((DisasmMap[i] and dfNewInstr)=0) and (DisasmMap[i]<>0)) do dec(i);
  fStatistics.LastItem:=i;
  Result:=true;
end;



function TDisassembler.DisassembleBlock(start, finish: cardinal; bit32: boolean):boolean;
var
    Vparam: TParam;                         // Parametre aktualnej instrukcie
    o,p: byte;                               // Interval pri FPU instrukciach
    PrefixStr:string;
    PrefixFlags: record
      group1,group2,group3,group4: boolean;
    end;
    AsmByte: byte;
    c: cardinal;
    _3DNow_Instruction: boolean;
    k: cardinal;
    UndefinedOpcode: boolean;
    KoniecBloku: boolean;
begin
  i:=start;
  InstrAddress:=i;
  UndefinedOpcode:=false;
  KoniecBloku:=false;
//  while (i<=finish) and (DisasmMap[i] =0) and (not KoniecBloku) do begin                     // Hlavny disassemblovaci cyklus


  while (i<=finish) and ((DisasmMap[i] and dfPart)=0) and (not KoniecBloku) do begin                     // Hlavny disassemblovaci cyklus

    operand32:=bit32;
    address32:=bit32;
    SegmentOverride:='';
{
    inc(counter,i-InstrAddress);
    if (counter > 1000) then begin
      inc(ProgressPosition,counter);
      if not ProgressFunction(ProgressPosition,CodeSize,'') then begin
        Result:=false;
        Exit;
      end;
      counter:=0;
    end;
}
    _3DNow_Instruction:=false;
    vpc:=0;                                 // Vynulovanie poctu operandov pre nasledujucu instrukciu
    genreg16:=false;
    modrm.loaded:=false;
    prefixes.pF2:=false;                    // 'prefixes' sa pouzivaju pri urcovani MMX,SSE,SSE2 instrukcii
    prefixes.pF3:=false;
    prefixes.p66:=false;
    prefixstr:='';
    prefixflags.group1:=false;
    prefixflags.group2:=false;
    prefixflags.group3:=false;
    prefixflags.group4:=false;

    inc(fStatistics.Instructions);
    InstrAddress:=i;
    Disassembled[InstrAddress].address:=i;
    Disassembled[InstrAddress].parsed:='';
    DisasmMap[InstrAddress]:=dfNewInstr;
//    Disassembled[InstrAddress].flag:=00;

// INFO:
// Iba Group1 zapisuje do "disassembled[InstrAddress].prefix"
//
// Zislo by sa skontrolovat, ci je LOCK pouzity len s povolenymi instrukciami, pretoze:
{
  The LOCK prefix can be prepended only to the following instructions and only to those forms
of the instructions where the destination operand is a memory operand: ADD, ADC, AND,
BTC, BTR, BTS, CMPXCHG, CMPXCH8B, DEC, INC, NEG, NOT, OR, SBB, SUB, XOR,
XADD, and XCHG.
}

    while (code[i] in prefix) do begin
      disassembled[InstrAddress].parsed:=disassembled[InstrAddress].parsed + IntToHex(code[i], 2);
      case code[i] of
// Group 1
        $F0,$F2,$F3: begin
           if prefixflags.group1 then begin UndefinedOpcode:=true; break; end;
           prefixflags.group1:=true;
           case code[i] of
             $F0: begin prefixstr:='lock'; disassembled[InstrAddress].prefix:='lock'; end;
             $F2: begin prefixstr:='repne'; prefixes.pF2:=true; end;
             $F3: begin prefixstr:='repe'; prefixes.pF3:=true; end;
           end;
         end;
// Group 2
        $2E,$36,$3E,$26,$64,$65: begin
          if PrefixFlags.group2 then begin UndefinedOpcode:=true; break; end;
          prefixflags.group2:=true;
          case code[i] of
            $2E: SegmentOverride:='CS:';
            $36: SegmentOverride:='SS:';
            $3E: SegmentOverride:='DS:';
            $26: SegmentOverride:='ES:';
            $64: SegmentOverride:='FS:';
            $65: SegmentOverride:='GS:';
          end;
        end;
// Group 3
        $66: begin
          if PrefixFlags.group3 then begin UndefinedOpcode:=true; break; end;
          prefixflags.group3:=true;
          operand32:=not operand32;
//          if bit32 then prefixstr:=prefixstr + 'o16 '
//          else prefixstr:=prefixstr + 'o32 ';
//          if bit32 then disassembled[InstrAddress].prefix:='o16'
//          else disassembled[InstrAddress].prefix:='o32';
          prefixes.p66:=true;
        end;
// Group 4
        $67: begin
          if PrefixFlags.group4 then begin UndefinedOpcode:=true; break; end;
          prefixflags.group4:=true;
          address32:=not address32;
//          if bit32 then prefixstr:=prefixstr + 'a16 '
//          else prefixstr:=prefixstr + 'a32 ';
//          if bit32 then disassembled[InstrAddress].prefix:='a16'
//          else disassembled[InstrAddress].prefix:='a32';
        end;
      end;
      inc(i);
    end;
    if UndefinedOpcode then begin
      inc(disassembled[InstrAddress].refercount);
      setlength(disassembled[InstrAddress].refer,disassembled[InstrAddress].refercount);
      disassembled[InstrAddress].refer[disassembled[InstrAddress].refercount-1]:=';'+IntToHex(InstrAddress,8)+' '+disassembled[InstrAddress].parsed+'...                  UNDEFINED OPCODE!';
      disassembled[InstrAddress].prefix:='';
      DisasmMap[InstrAddress]:=0;
      UndefinedOpcode:=false;
      KoniecBloku:=true;
      continue;
    end;

    // Nasledujuci riadok je OK. Ak ma instrukcia dvojbajtovy opcode (OF),
    // tak prefixy F2,F3 maju iny vyznam ako "repne, repe".
    if code[i]<>$0F then disassembled[InstrAddress].prefix:=prefixstr;

    disassembled[InstrAddress].parsed:=disassembled[InstrAddress].parsed+inttohex(code[i],2);

    j:=0;

    case OBOInstruction_set[code[i]].opcode of

    $FE:begin                                         // OneByte Opcode Group Instructions
          case code[i] of
            $80: j:=0;
            $81: j:=8;
            $82: j:=16;
            $83: j:=24;
            $C0: j:=32;
            $C1: j:=40;
            $D0: j:=48;
            $D1: j:=56;
            $D2: j:=64;
            $D3: j:=72;
            $F6: j:=80;
            $F7: j:=88;
            $FE: j:=96;
            $FF: j:=104;
            $C6: j:=112;
            $C7: j:=120;
            else ; //Showmessage(inttohex(code[i],2));
          end;
          modrm:=loadmodrm(code[i+1]);
          inc(j,modrm.regop);
          Vparam:=GroupOBOInstruction_set[j].param;
          Vpc:=GroupOBOInstruction_set[j].pc;
          disassembled[InstrAddress].name:=GroupOBOInstruction_set[j].name;
          UndefinedOpcode:= GroupOBOInstruction_set[j].opcode = $D6;
        end;
    $D8:begin                                         // FPU Instructions
          if code[i+1]>$BF then  begin
            disassembled[InstrAddress].parsed:=disassembled[InstrAddress].parsed+inttohex(code[i+1],2);
            FPUNastavPoradie(o,p,code[i]); //o - zaciatok, p - koniec
            dec(o);
            repeat
              inc(o);
              if o > p then begin
                o:= UndefinedFPUInstructionIndex;
                UndefinedOpcode:=true;
                break;
              end;
// toto nie je uplne v poriadku
            until (FPUInstruction_set[o].opcode = code[i+1])                           // tu //
              or ((FPUInstruction_set[o].par<>none) and ((FPUInstruction_set[o].opcode+7) >= code[i+1]));

            disassembled[InstrAddress].name:=FPUInstruction_set[o].name;
            if FPUInstruction_set[o].par<>none then
            case  FPUInstruction_set[o].par of
              st1: disassembled[InstrAddress].operandy:='st0,st' + inttostr(code[i+1] mod 8) + '';
              st2: disassembled[InstrAddress].operandy:='st' + inttostr(code[i+1] mod 8) + ',st0';
              st3: disassembled[InstrAddress].operandy:='st' + inttostr(code[i+1] mod 8) + '';
            end;
            inc(i);
          end
          else begin
// tu treba este osetrit nedifinovane opcody:
            modrm:=loadmodrm(code[i+1]);
            disassembled[InstrAddress].name:=FPUInstructions[(code[i] mod 8)*8 + modrm.regop].name;
            OperandSize:=FPUInstructions[(code[i] mod 8)*8 + modrm.regop].par;
            disassembled[InstrAddress].operandy:=spracujmodrm(mem,twoorfour);
          end;
      end;
    $0F:begin                                         // TwoByte Opcode Instruction
           inc(i);
           disassembled[InstrAddress].parsed:=disassembled[InstrAddress].parsed+inttohex(code[i],2);
           case TBOInstruction_set[code[i]].opcode of

// tu treba este osetrit nedifinovane opcody:
             $FE:begin            // Group opcode instruction
                   j:=0;
                     modrm:=loadmodrm(code[i+1]);
                     case code[i] of
                       $71: begin
                              if prefixes.p66 then begin
                                Vparam:=MMXInstruction_set[Group12[modrm.regop].mmx2].param;
                                Vpc:=MMXInstruction_set[Group12[modrm.regop].mmx2].pc;
                                disassembled[InstrAddress].name:=MMXInstruction_set[Group12[modrm.regop].mmx2].name;
                              end
                              else begin
                                Vparam:=MMXInstruction_set[Group12[modrm.regop].mmx1].param;
                                Vpc:=MMXInstruction_set[Group12[modrm.regop].mmx1].pc;
                                disassembled[InstrAddress].name:=MMXInstruction_set[Group12[modrm.regop].mmx1].name;
                              end;
                            end;
                       $72: begin
                              if prefixes.p66 then begin
                                Vparam:=MMXInstruction_set[Group13[modrm.regop].mmx2].param;
                                Vpc:=MMXInstruction_set[Group13[modrm.regop].mmx2].pc;
                                disassembled[InstrAddress].name:=MMXInstruction_set[Group13[modrm.regop].mmx2].name;
                              end
                              else begin
                                Vparam:=MMXInstruction_set[Group13[modrm.regop].mmx1].param;
                                Vpc:=MMXInstruction_set[Group13[modrm.regop].mmx1].pc;
                                disassembled[InstrAddress].name:=MMXInstruction_set[Group13[modrm.regop].mmx1].name;
                              end;
                            end;
                       $73: begin
                              if prefixes.p66 then begin
                                Vparam:=MMXInstruction_set[Group14[modrm.regop].mmx2].param;
                                Vpc:=MMXInstruction_set[Group14[modrm.regop].mmx2].pc;
                                disassembled[InstrAddress].name:=MMXInstruction_set[Group14[modrm.regop].mmx2].name;
                              end
                              else begin
                                Vparam:=MMXInstruction_set[Group14[modrm.regop].mmx1].param;
                                Vpc:=MMXInstruction_set[Group14[modrm.regop].mmx1].pc;
                                disassembled[InstrAddress].name:=MMXInstruction_set[Group14[modrm.regop].mmx1].name;
                              end;
                            end
                     else begin
                       repeat inc(j); until (j=PocetTBOGroupInstrukcii+1) or (code[i]=GroupTBOInstruction_set[j].opcode);
                       while not(modrm.regop=GroupTBOInstruction_set[j].secopcode) do inc(j);
                       if (code[i]=$AE) and (modrm.regop=7) and (modrm.moder=3) then inc(j); // CLFLUSH a SFENCE na rovnakej pozicii (pozri OPCODE MAP)
                       Vparam:=GroupTBOInstruction_set[j].param;
                       Vpc:=GroupTBOInstruction_set[j].pc;
                       disassembled[InstrAddress].name:=GroupTBOinstruction_set[j].name;
                     end;
                     end;
                 end;
             $60:begin            // MMX, SSE, SSE2 opcode instruction
                   if prefixes.p66 then begin
                     Vparam:=MMXInstruction_set[TBOInstruction_set[code[i]].mmx2].param;
                     Vpc:=MMXInstruction_set[TBOInstruction_set[code[i]].mmx2].pc;
                     disassembled[InstrAddress].name:=MMXInstruction_set[TBOInstruction_set[code[i]].mmx2].name;
                   end
                   else
                   if prefixes.pF2 then begin
                     Vparam:=MMXInstruction_set[TBOInstruction_set[code[i]].mmx3].param;
                     Vpc:=MMXInstruction_set[TBOInstruction_set[code[i]].mmx3].pc;
                     disassembled[InstrAddress].name:=MMXInstruction_set[TBOInstruction_set[code[i]].mmx3].name;
                   end
                   else
                   if prefixes.pF3 then begin
                     Vparam:=MMXInstruction_set[TBOInstruction_set[code[i]].mmx4].param;
                     Vpc:=MMXInstruction_set[TBOInstruction_set[code[i]].mmx4].pc;
                     disassembled[InstrAddress].name:=MMXInstruction_set[TBOInstruction_set[code[i]].mmx4].name;
                   end
                   else begin
                     Vparam:=MMXInstruction_set[TBOInstruction_set[code[i]].mmx1].param;
                     Vpc:=MMXInstruction_set[TBOInstruction_set[code[i]].mmx1].pc;
                     disassembled[InstrAddress].name:=MMXInstruction_set[TBOInstruction_set[code[i]].mmx1].name;
                   end;
                 end;
             $0F:begin
                   _3DNow_Instruction:=true;
                   Vpc:=2;
                   Vparam.p1:=GREGq;
                   Vparam.p2:=MODq;
                 end;
             $D6: UndefinedOpcode:=true;

             else begin // Obycajne TwoByte Instructions
                    Vparam:=TBOInstruction_set[code[i]].param;
                    Vpc:=TBOInstruction_set[code[i]].pc;
                    disassembled[InstrAddress].name:=TBOinstruction_set[code[i]].name;
                  end;
           end;  // End of "case"
        end;
    else
      begin                                           // OneByte Instructions
        if OBOinstruction_set[code[i]].opcode = $D6 then UndefinedOpcode:=true
        else begin
          Vparam:=OBOinstruction_set[code[i]].param;
          Vpc:=OBOinstruction_set[code[i]].pc;
          if not (OBOinstruction_set[code[i]].AddOp) then disassembled[InstrAddress].name:=OBOinstruction_set[code[i]].name
            else if operand32 then disassembled[InstrAddress].name:=OBOinstruction_set[code[i]].name32
              else disassembled[InstrAddress].name:=OBOinstruction_set[code[i]].name16;
        end;
      end;
    end;  // End of "case"

// Osetrenie nedefinovaneho opcodu - pridanie komentara ako referencie itemu s adresou nedefinovaneho opcodu
    if UndefinedOpcode then begin
      inc(disassembled[InstrAddress].refercount);
      setlength(disassembled[InstrAddress].refer,disassembled[InstrAddress].refercount);
      disassembled[InstrAddress].refer[disassembled[InstrAddress].refercount-1]:=';'+IntToHex(InstrAddress,8)+' '+disassembled[InstrAddress].parsed+'...                  UNDEFINED OPCODE!';
      DisasmMap[InstrAddress]:=0;
      UndefinedOpcode:=false;
      KoniecBloku:=true;
      continue;
    end;
// begin temporary - only for testing
//    if InstrAddress= $15D1 then showmessage('a');
// end temporary - only for testing

    OperandSize:=szEmpty;
    case Vpc of                             // Spracovanie parametrov podla ich poctu
      1: begin
           case Vparam.p1 of
             MODb: OperandSize:=szByte;
             MODv: If Operand32 then OperandSize:=szDword else OperandSize:=szWord;
             MODw: OperandSize:=szWord;
             MODd: OperandSize:=szDword;
           end;
           disassembled[InstrAddress].operandy:=SpracujParameter(Vparam.p1);
         end;
      2: begin
           case Vparam.p1 of
             MODb: OperandSize:=szByte;
             MODv: If Operand32 then OperandSize:=szDword else OperandSize:=szWord;
             MODw: OperandSize:=szWord;
             MODd: OperandSize:=szDword;
           end;
           disassembled[InstrAddress].operandy:=SpracujParameter(Vparam.p1);
           OperandSize:=szEmpty;
// Zmysel nasledovneho objasni "fpc_bug.txt"
// povodne:
//           disassembled[InstrAddress].operandy:=disassembled[InstrAddress].operandy + ',' + SpracujParameter(Vparam.p2);
//teraz
           disassembled[InstrAddress].operandy:=disassembled[InstrAddress].operandy + ',';
           disassembled[InstrAddress].operandy:=disassembled[InstrAddress].operandy + SpracujParameter(Vparam.p2);
         end;
      3: begin
// Zmysel nasledovneho objasni "fpc_bug.txt"

// povodne:
//           disassembled[InstrAddress].operandy:=SpracujParameter(Vparam.p1)+','+SpracujParameter(Vparam.p2)+','+SpracujParameter(Vparam.p3);
// teraz:
           disassembled[InstrAddress].operandy:=SpracujParameter(Vparam.p1)+',';
           disassembled[InstrAddress].operandy:=disassembled[InstrAddress].operandy+SpracujParameter(Vparam.p2)+',';
           disassembled[InstrAddress].operandy:=disassembled[InstrAddress].operandy+SpracujParameter(Vparam.p3);
         end;
    end;
    if _3DNow_Instruction then begin
      j:=1;
      inc(i);
      while j <= Pocet3DNowInstrukcii do begin
        if code[i]=_3DNowInstruction_set[j].opcode then break;
        inc(j);
      end;
      if j <= Pocet3DNowInstrukcii then begin
        disassembled[InstrAddress].name:=_3DNowInstruction_set[j].name;
        disassembled[InstrAddress].parsed:=disassembled[InstrAddress].parsed+inttohex(_3DNowInstruction_set[j].opcode,2);
      end
      else begin // nedefinovany opcode
        inc(disassembled[InstrAddress].refercount);
        setlength(disassembled[InstrAddress].refer,disassembled[InstrAddress].refercount);
        disassembled[InstrAddress].refer[disassembled[InstrAddress].refercount-1]:=';'+IntToHex(InstrAddress,8)+' '+disassembled[InstrAddress].parsed+'...                  UNDEFINED OPCODE!';
        DisasmMap[InstrAddress]:=0;
        KoniecBloku:=true;
        continue;
      end;
    end;
    if disassembled[InstrAddress].name='' then ;

// -- Kvoli testovanie intrukcii je obcas vypnute ukoncovanie bloku:

    case disassembled[InstrAddress].name[1] of                                 //  Instrukcie ukoncujuce blok
      'J': if disassembled[InstrAddress].name[2]='M' then KoniecBloku:=true;     // JMP, JMPx
      'R': if (disassembled[InstrAddress].name='RET')
           or (disassembled[InstrAddress].name='RETN')
           or (disassembled[InstrAddress].name='RETF') then KoniecBloku:=true;   // RET, RETx
      'I': if disassembled[InstrAddress].name[2]='R' then KoniecBloku:=true;     // IRET, IRETx
    end;

// --
    if (code[InstrAddress]=$FF) then begin                      //  Detekcia importovanych funkcii pomocou CALLN a JMPN
      asmbyte:=code[InstrAddress+1];
      asm
        mov al,asmbyte
        shr al,3
        cmp al,010b
        je @OK
        cmp al,100b
        je @OK
        mov asmbyte,0
        jmp @koniec
      @OK:
        mov asmbyte,1
      @koniec:
      end;
      if (asmbyte=1) and (length(disassembled[InstrAddress].operandy)=18) then begin
        SetLength(Imported,Length(Imported)+1);
//        Imported[High(Imported)]:=InstrAddress;
        Imported[Length(Imported)-1]:=InstrAddress;
      end;
    end;

    if (code[InstrAddress]=$0F) then     //  nieco podobne by sa mozno zislo aj pre PUNPCKxxx instrukcie
      case code[InstrAddress+1] of
// MOVZX
        $B6: Disassembled[InstrAddress].operandy:=InsertStr('byte ',Disassembled[InstrAddress].operandy, Pos(',',Disassembled[InstrAddress].operandy)+1);
        $B7: Disassembled[InstrAddress].operandy:=InsertStr('word ',Disassembled[InstrAddress].operandy, Pos(',',Disassembled[InstrAddress].operandy)+1);
// MOVSX
        $BE: Disassembled[InstrAddress].operandy:=InsertStr('byte ',Disassembled[InstrAddress].operandy, Pos(',',Disassembled[InstrAddress].operandy)+1);
        $BF: Disassembled[InstrAddress].operandy:=InsertStr('word ',Disassembled[InstrAddress].operandy, Pos(',',Disassembled[InstrAddress].operandy)+1);
      end;

// Ak je posledny bajt aktual. instrukcie uz sucastou nejakej inej instrukcie alebo je uz mimo bloku,
// tak sa odstrani flag dfInstruction z prveho bajtu aktual. instrukcie a ukonci sa aktual. blok
// pozn.: nad tymto a veci s tym suvisiacimi sa treba este zamysliet, napr. ci ma teda aktualny blok zmysel(alebo ten prave zacinajuci)
//
    if ((DisasmMap[i] and dfPart)<>0) or (i>finish) then begin
      dec(DisasmMap[InstrAddress],dfNewInstr);
      KoniecBloku:=true;
    end
// prida kazdemu bajtu aktualnej instrukcie flag dfPart, t.j. ze je sucastou nejakej instruckie
    else for k:=InstrAddress to i do inc(DisasmMap[k],dfPart);

    inc(i);

//    for ii:=MyDis.Count to InstrAddress
//    MyDis.Strings[InstrAddress]:=IntToHex(InstrAddress,8)+Disassembled[InstrAddress].parsed;
  end;   // End of   Hlavny disassemblovaci cyklus
  fBlocks.Add(start, i - start);
  result:=true;
end;   // End of Disassemble



function TDisassembler.SpracujParameter(a:Tparameter):string;
begin
      case a of
        MODb: result:=Spracujmodrm(regmem, onebyte);
        MODw: result:=SpracujModrm(regmem, twobyte);
        MODv: result:=SpracujModrm(regmem, twoorfour);
        MODd: result:=SpracujModrm(regmem, fourbyte);
        MODp: result:=SpracujModrm(regmem, fourorsix);
        MODq: result:=SpracujModrm(regmem, mmx);
        MODdq: result:=SpracujModRM(regmem, xmm);

        GREGb: result:=SpracujModrm(greg,onebyte);
        GREGw: result:=SpracujModrm(greg,twobyte);
        GREGv: result:=SpracujModrm(greg,twoorfour);
        GREGd: result:=SpracujModrm(greg,fourbyte);
        GREGq: result:=SpracujModrm(greg,mmx);
        GREGdq: result:=SpracujModrm(greg,xmm);

        SREGw: result:=SpracujModrm(sreg,twobyte);
        CREGd: result:=SpracujModrm(creg,fourbyte);
        DREGd: result:=SpracujModrm(dreg,fourbyte);
        TREGd: result:=SpracujModrm(treg,fourbyte);

        IMMb: result:=Spracujimmediate(onebyte);
        IMMv: result:=spracujimmediate(twoorfour);
        IMMw: result:=spracujimmediate(twobyte);
        IMMp: result:=spracujimmediate(fourorsix);

        RELb: result:=spracujrelative(onebyte);
        RELv: result:=spracujrelative(twoorfour);
        RELw: result:=spracujrelative(twobyte);

        OFFb: result:=spracujoffset(onebyte);
        OFFv: result:=spracujoffset(twoorfour);

        ax: result:=spracujgenreg+'AX';
        bx: result:=spracujgenreg+'BX';
        cx: result:=spracujgenreg+'CX';
        dx: result:=spracujgenreg+'DX';
        si: result:=spracujgenreg+'SI';
        di: result:=spracujgenreg+'DI';
        bp: result:=spracujgenreg+'BP';
        sp: result:=spracujgenreg+'SP';
        al: result:='AL';
        bl: result:='BL';
        cl: result:='CL';
        dl: result:='DL';
        ah: result:='AH';
        bh: result:='BH';
        ch: result:='CH';
        dh: result:='DH';
        DS: result:='DS';
        ES: result:='ES';
        SS: result:='SS';
        CS: result:='CS';
        statDX: result:='DX';

        M32: result:=SpracujModRM(mem,fourbyte);
        M64: result:=SpracujModRM(mem,mmx);
        M128: result:=SpracujModRM(mem,xmm);
        R32: result:=SpracujModRM(reg, fourbyte);
        R64: result:=SpracujModRM(reg, mmx);
        R128: result:=SpracujModRM(reg, xmm);
        REG32_M16: result:=SpracujModRM(regmem,R32_M16);
        XMM_M32: result:=SpracujModRM(regmem,R128_M32);
        XMM_M64: result:=SpracujModRM(regmem,R128_M64);

        n1: result:='1';
      end;
end;


//******************************************************************************
// TDisassembledBlocks class
//******************************************************************************


function TDisassembledBlocks.Add(Address, Size: cardinal): integer;
begin
  Inc(fCount);
  if fCount > fCapacity then begin
    fCapacity:= fCapacity + 10;
    SetLength(fBlocks, fCapacity);
  end;
  fBlocks[fCount - 1].Address:= Address;
  fBlocks[fCount - 1].Size:= Size;
end;



function TDisassembledBlocks.GetBlock(Index: integer): TDisassembledBlock;
begin
  if (Index >= 0) and (Index < fCount) then
    result:= fBlocks[Index]
  else
    raise Exception.Create('Index out of bounds');
end;



function TDisassembledBlocks.GetBlockCount: integer;
begin
  result:= fCount;
end;



procedure TDisassembledBlocks.Clear;
begin
  SetLength(fBlocks, 10);
  fCapacity:=10;
  fCount:=0;
end;


end.
