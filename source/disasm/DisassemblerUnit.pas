{ TODO:
  pridat moznost ukoncenia bloku v 16 bit. mode v pripade instrukcii
    MOV AH,byte 0x09
    INT byte 0x21

 INFO - PUNPCKLBW operandy (druhy), pozri Intel manual, 19.12.2004 zmeneny na MODq, asi by to chcelo vyriesit nejako inac (spolu s MOVZX atd)
}
unit DisassemblerUnit;

{$INCLUDE 'delver.inc'}

interface

uses
  Classes,
  SysUtils,
  Math,
  Types,

  procmat,
  LoggerUnit,
  StringUtilities,
  CallsAndJumpsTableUnit,
  DisassembledBlocksUnit,
  DisassemblerTypes,
  DisassemblerUtils,
  x86DisassemblerTypes,
  x86Instructions,
  StrUtils
  ;


type

  TDisassembler = class                        // Hlavny objekt disassemblovania
  private
    code: TByteDynArray;                  // Pole s kodom
    DisasmMap: TByteDynArray;
    CodeSize: cardinal;
    fStatistics: TStatistics;
    fBit32: boolean;
    ModRM: TModRM;
    SIB: TSIB;
    i: cardinal;                          // Uchovava poziciu v poli CODE
    j: cardinal;                          // Pomocne premenne
    operand32, address32:boolean;        // Uchovava 16/32bit stav instrukcie

    OperandSize: TSize;
    //AddressSize: TSize;
    genreg16: boolean;
    SegmentOverride: string;

    InstrAddress: cardinal; // address of current instruction in "code" array

{ Toto sposobi spomalenie 400ms (2550 vs 2950) na wxivan.exe
    InstructionName: string[10]; // name of current instruction
    InstructionPrefix: string[10]; // prefix of current instruction
    InstructionOperands: string[50]; // operands of current instruction
}
    InstructionName: string; // name of current instruction

  Vpc: Byte;                              // Pocet parametrov aktualnej instrukcie
    fMemOffset: cardinal;

    fBlocks: TDisassembledBlocks;

    function SpracujParameter(a:TParameter): string;
    function LoadModRM(a:byte): TModRM;
    function SpracujModRM(a: TModRMParameterType; b: TModRMParameterSize): string;
    function LoadSIB(a:byte): TSIB;
    function SpracujSIB: string;
    function SpracujImmediate(a: TModRMParameterSize): string;
    function SpracujRelative(a: TModRMParameterSize): string;
    function SpracujOffset(a: TModRMParameterSize): string;
    function SpracujGenReg: string;

    procedure DisassemblerCycle;
    function DisassembleBlock(Start, Finish: cardinal): boolean;

    function GetDisassembledBlock(Index: integer): TDisassembledBlock;
    function GetBlockCount: integer;

  public
    CAJ: TCallsAndJumps;

    Disassembled: array of TDisassembledItem;
    Imported: array of cardinal;

    constructor Create(SectionCode: TByteDynArray; var DisassemblerMap: TByteDynArray; MemOffset: cardinal; Bit32: boolean);

    procedure DisassembleAll;
    procedure Disassemble(Recursive: boolean);

    property Statistics: TStatistics read fStatistics;
    property Blocks[Index: integer]: TDisassembledBlock read GetDisassembledBlock;
    property BlockCount: integer read GetBlockCount;
  end;


Implementation

//uses
//  x86Instructions;


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



function ByteToSignedHex(AValue: byte): string;
begin
  if AValue <= 127 then
    result := '+0x' + IntToHex(AValue, 2)
  else
    result := '-0x' + IntToHex(Abs(Shortint(AValue)), 2);
end;



{
  Nacita ModRM do struktury TModRM
  ModRM = 2 + 3 + 3 bits (Moder + RegOp + RM)
  Pouzivam assembler, lebo operatory "shl" a "shr" v Delphi pracuju s typom integer (4 bajty) a my potrebujeme 1 bajt
  Parametre:
    EAX - self
    EDX - ModRM byte
    ECX - adresa vysledku
}
function TDisassembler.LoadModRM(a: byte): TModRM;
asm
  and edx,$000000FF // DL <- Full ModRM
  mov dh,dl
  shr dh,6          // DH <- Moder
  mov [ecx],edx
  mov dh,dl
  shl dl,2
  shr dl,5          // DL <- RegOp
  and dh,7          // DH <- RM
  mov [ecx+2],edx
end;



{
  Nacita SIB do struktury TSIB
  SIB = 2 + 3 + 3 bity (scale + index + base)
  Pouzivam assembler, lebo operatory "shl" a "shr" v Delphi pracuju s typom integer (4 bajty) a my potrebujeme 1 bajt
  Parametre:
    EAX - self
    EDX - SIB byte
    ECX - adresa vysledku
}
function TDisassembler.LoadSIB(a: byte): TSIB;
asm
  and edx,$000000FF // DL <- Full SIB
  mov dh,dl
  shr dh,6          // DH <- scale
  mov [ecx],edx
  mov dh,dl
  shl dl,2
  shr dl,5          // DL <- index
  and dh,7          // DH <- base
  mov [ecx+2],edx   // 16 - 23 <- Loaded (0)
end;



function TDisassembler.SpracujSIB: string;
begin
  Inc(i);
  sib := LoadSIB(code[i]);
  if not ((ModRM.Moder = 0) and (SIB.Base = 5)) then begin
    result := fourbyte_register[sib.base];

    if SIB.Index <> 4 then begin
      result := result + '+' + fourbyte_register[sib.index];
      if SIB.Scale <> 0 then
        result := result + '*' + IntToStr(Round(IntPower(2, SIB.Scale)));
    end;
  end
  else begin
    Inc(i);
    result:= '0x' + IntToHex(cardinal((@code[i])^), 8);
    Inc(i, 3);

    if SIB.index <> 4 then begin
      result := result + '+' + fourbyte_register[SIB.Index];
      if SIB.scale <> 0 then
        result := result + '*' + IntToStr(Round(IntPower(2, SIB.scale)));
    end;
  end;
end;



function TDisassembler.SpracujModRM(a: TModRMParameterType; b: TModRMParameterSize):string;
begin
  result:='';

  if not ModRM.Loaded then begin
    Inc(i);
    ModRM := LoadModRM(code[i]);
    ModRM.Loaded := true;
  end;

  case address32 of
    false: begin
             case a of
               GReg: begin
                       case b of
                         OneByte: result:=onebyte_register[modrm.regop];
                         TwoByte: result:=twobyte_register[modrm.regop];
                         TwoOrFour: if operand32 then result:=fourbyte_register[modrm.regop]
                                    else result:=twobyte_register[modrm.regop];
                         FourByte: result:=fourbyte_register[modrm.regop];
                         MMX: result:=mmx_register[modrm.regop];
                         XMM: result:=xmm_register[modrm.regop];
                       end;
                     end;

              Reg: begin
                     if modrm.moder = 3 then
                       case b of
                         OneByte: result:=onebyte_register[modrm.rm];
                         TwoByte: result:=twobyte_register[modrm.rm];
                         TwoOrFour: if operand32 then result:=fourbyte_register[modrm.rm]
                                    else result:=twobyte_register[modrm.rm];
                         FourByte: result:=fourbyte_register[modrm.rm];
                         MMX: result:=mmx_register[modrm.rm];
                         XMM: result:=xmm_register[modrm.rm];
                       end
                     else
                       result := 'Invalid parameter';
                   end;

               Mem: begin
                          case modrm.moder of
                            0: begin
                                 result := '[' + SegmentOverride;
                                 if ModRM.RM = 6 then begin
                                   Inc(i,2);
                                   result := result + '0x' + IntToHex(code[i],2) + IntToHex(code[i-1],2);
                                 end
                                 else
                                   result:=result + modrm16bitaddress_register[modrm.rm];
                                 result:=result+']';
                               end;
                            1: begin
                                inc(i);
                                result:='[' + SegmentOverride + modrm16bitaddress_register[modrm.rm]+ByteToSignedHex(code[i])+']';
                               end;
                            2: begin
                                inc(i,2);
                                result:='[' + SegmentOverride + modrm16bitaddress_register[modrm.rm]+'+'+'0x'+inttohex(code[i],2)+inttohex(code[i-1],2)+']';
                               end;
                            3:result:='Invalid parameter!';
                          end;
                          case OperandSize of
                            szByte: result:='byte ' + result;
                            szWord: result:='word ' + result;
                            szDword: result:='dword ' + result;
                            szQword: result:='qword ' + result;
                            szTword: result:='tword ' + result;
                          end;
                        end;

               RegMem: begin
                          case modrm.moder of
                            0:begin
                                result:='[' + SegmentOverride;
                                if modrm.rm=6 then begin
                                  inc(i,2);
                                  result:=result+'0x'+inttohex(code[i],2)+inttohex(code[i-1],2);
                                end
                                else result:=result+modrm16bitaddress_register[modrm.rm];
                                result:=result+']';
                              end;
                            1:begin
                                inc(i);
                                result:='[' + SegmentOverride + modrm16bitaddress_register[modrm.rm]+ByteToSignedHex(code[i])+']';
                              end;
                            2:begin
                                inc(i,2);
                                result:='[' + SegmentOverride + modrm16bitaddress_register[modrm.rm]+'+'+'0x'+inttohex(code[i],2)+inttohex(code[i-1],2)+']';
                              end;
                            3:begin
                                case b of
                                  onebyte: result:=onebyte_register[modrm.rm];
                                  twobyte: result:=twobyte_register[modrm.rm];
                                  twoorfour:
                                    if operand32 then
                                      result := fourbyte_register[modrm.rm]
                                    else
                                      result := twobyte_register[modrm.rm];
                                  fourbyte: result:=fourbyte_register[modrm.rm];
                                  FourOrSix: if operand32 then result:=fourbyte_register[modrm.rm]
                                           else result:=twobyte_register[modrm.rm];
                                  mmx: result := mmx_register[modrm.rm];
                                  xmm: result := xmm_register[modrm.rm];
                                  R32_M16: result := fourbyte_register[modrm.rm];
                                  R128_M32: result := xmm_register[modrm.rm];
                                  R128_M64: result := xmm_register[modrm.rm];
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
                                  5: begin
                                    Inc(i);
                                    result:= result + '0x' + IntToHex(cardinal((@code[i])^), 8);
                                    Inc(i, 3);
                                  end;
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
                              end;
                           2: begin
                                result:='[' + SegmentOverride;
                                if modrm.rm = 4 then
                                  result:=result + spracujsib
                                else
                                  result:= result + fourbyte_register[modrm.rm];
                                Inc(i);
                                result:= result + '+' + '0x' + IntToHex(cardinal((@code[i])^), 8) + ']';
                                Inc(i, 3);
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
                                case ModRM.RM of
                                  5: begin
                                    Inc(i);
                                    result := result + '0x' + IntToHex(cardinal((@code[i])^), 8);
                                    Inc(i, 3);
                                  end;
                                  4: result := result + SpracujSIB;
                                  else
                                    result := result + fourbyte_register[modrm.rm];
                                end;
                                result := result + ']';
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
                                if modrm.rm = 4 then
                                  result := result + SpracujSIB
                                else
                                  result := result + fourbyte_register[modrm.rm];
                                Inc(i);
                                result := result + ByteToSignedHex(code[i])+']';

                                case OperandSize of
                                  szByte: result:='byte '+result;
                                  szWord: result:='word '+result;
                                  szDword: result:='dword '+result;
                                end;

                              end;
                           2: begin
                                result:='[' + SegmentOverride;
                                if modrm.rm = 4 then
                                  result:=result + spracujsib
                                else
                                  result:= result + fourbyte_register[modrm.rm];
                                Inc(i);
                                result:= result + '+' + '0x' + IntToHex(cardinal((@code[i])^), 8) + ']';
                                Inc(i, 3);

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
                        TwoByte: result := segment_register[modrm.regop];
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



function TDisassembler.SpracujImmediate(a: TModRMParameterSize):string;
var
  Immediate: cardinal;
  SegmentImmediate: word;
  ParsedCount: integer;
begin
  asm
    push esi
    push ebx
    mov esi,self       // pointer na instanciu TDisassembler
    mov ebx,[esi].i    // index v poli code
    inc ebx
    mov eax,[esi].code // adresa pola code
    add eax,ebx        // adresa prvku pola v pamati
    xor ecx,ecx        // vysledny immediate
  // Case
    cmp a,OneByte           // ONE BYTE
    jne @CheckTwobyte
    movzx ecx,byte[eax]     // parameter instrukcie -> ECX
    jmp @EndCase

  @CheckTwobyte:
    cmp a,TwoByte           // TWO BYTE
    jne @CheckTwoOrFour

  @_16:
    movzx ecx,word ptr[eax]
    inc ebx
    jmp @EndCase

  @CheckTwoOrFour:
    cmp a,TwoOrFour
    jne @CheckFourOrSix
    cmp byte ptr [esi].operand32,0
    je @_16

  @_32:
    mov ecx,[eax] //  predtym: mov ecx,cardinal ptr [eax]
    add ebx,3
    jmp @EndCase

  @CheckFourOrSix:
    cmp byte ptr [esi].operand32,0
    je @_16_16

  @_16_32:
    mov ecx,[eax] // predtym: mov ecx,cardinal ptr [eax]
    add eax,4
    add ebx,4
    jmp @_segment

  @_16_16:
  //  movzx ecx,word ptr [eax]
    mov cx, word ptr [eax]
    add eax,2
    add ebx,2

  @_segment:
  //  movzx edx,word ptr [eax]
    mov dx,[eax]

    mov SegmentImmediate,dx
    inc ebx

  @EndCase:
    mov immediate,ecx
    mov ecx,ebx // save value of new "i" to ECX
    sub ebx,[esi].i // compute and set ParsedCount
    shl ebx,1
    mov ParsedCount,ebx
    mov [esi].i,ecx // set new "i"

    pop ebx
    pop esi
  end;
  if a = FourOrSix then
    result:= '0x' + IntToHex(SegmentImmediate, 4) + ':' + '0x' + IntToHex(Immediate, ParsedCount)
  else
    // was: result:=result + '0x'+IntToHex(immediate,parsedcount);
    // because of NASM compatibility:
    case ParsedCount of
      2: result := 'byte 0x' + IntToHex(Immediate, 2);
      4: result := 'word 0x' + IntToHex(Immediate, 4);
      8: result := 'dword 0x' + IntToHex(Immediate, 8);
    end;
end;



function TDisassembler.SpracujRelative(a: TModRMParameterSize):string;
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


  if address < 0 then result:='-0x' + IntToHex(Abs(address),8)            // skok na zapornu adresu (treba este zohladnit MemOffset !!!)
  else begin
    result:='0x' + IntToHex(cardinal(address) + fMemOffset, 8);
    if cardinal(address) > CodeSize-1 then Exit                             // skok za koniec kodovej sekcie
    else begin
      CAJ.Add(address);
      Inc(Disassembled[address].refercount);
      SetLength(Disassembled[address].refer,Disassembled[address].refercount);
      case InstructionName[1] of
        'J': Disassembled[address].refer[Disassembled[address].refercount-1]:='Jump from 0x'+IntToHex(InstrAddress + fMemOffset, 8);
        'C': Disassembled[address].refer[Disassembled[address].refercount-1]:='Call from 0x'+IntToHex(InstrAddress + fMemOffset, 8);
        'L': Disassembled[address].refer[Disassembled[address].refercount-1]:='Loop from 0x'+IntToHex(InstrAddress + fMemOffset, 8);
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



function TDisassembler.SpracujGenReg: string;
begin
  if not genreg16 then
    if operand32 then
      result:= 'E'
    else
      result:= '';
end;



function TDisassembler.SpracujOffset(a: TModRMParameterSize):string;
begin
  Inc(i);
  if address32 then begin
    result:= '[' + SegmentOverride + '0x' + IntToHex(cardinal((@code[i])^), 8) + ']';
    Inc(i, 3);
  end
  else begin
    result:= '[' + SegmentOverride + '0x' + IntToHex(word((@code[i])^), 4) + ']';
    Inc(i);
  end;
end;



constructor TDisassembler.Create(SectionCode: TByteDynArray; var DisassemblerMap: TByteDynArray; MemOffset: cardinal; Bit32: boolean);
begin
  code:= SectionCode;
  CodeSize:= Length(code) - CodeArrayReserve;
  DisasmMap:= DisassemblerMap;
  SetLength(Disassembled, CodeSize);
  CAJ:= TCallsAndJumps.Create(DisasmMap);
  fBlocks:= TDisassembledBlocks.Create;
  fMemOffset:= MemOffset;
  fBit32:= Bit32;
end;



function TDisassembler.GetDisassembledBlock(Index: integer): TDisassembledBlock;
begin
  result:= fBlocks[Index];
end;



function TDisassembler.GetBlockCount: integer;
begin
  result:= fBlocks.Count;
end;



procedure TDisassembler.DisassemblerCycle;
var
  CAJIndex: Integer;
begin
  while CAJ.Count > 0 do begin
    for CAJIndex := 0 to CAJ.Count - 1 do begin
      DisassembleBlock(CAJ[CAJIndex].start, CAJ[CAJIndex].finish);
    end;
    CAJ.Process(CodeSize);
  end;
end;



procedure TDisassembler.Disassemble(Recursive: boolean);
begin
  if Recursive then
    DisassemblerCycle
  else
    DisassembleBlock(CAJ[0].start, CAJ[0].finish);
end;



procedure TDisassembler.DisassembleAll;
var
  CodeIndex: cardinal;
  HexAddressIndex: integer;
  Line: string[ilInstructionMnemonicIndex + 12 + 1];
  SpaceIndex: integer;
  MemAddress: cardinal;
begin
  ProgressData.Maximum:= CodeSize;
  ProgressData.Position:= 0;

  //****************************************************************************
  // 1. Phase - Disassemble
  //****************************************************************************

  Logger.Info('TDisassembler.DisassembleAll - Phase 1');
  DisassemblerCycle;

  //****************************************************************************
  // 2. faza - najdenie zaciatkov procedur pomocou standartnych instrukcii
  //****************************************************************************

  Logger.Info('TDisassembler.DisassembleAll - Phase 2a');
  if CodeSize >= 3 then begin
    for CodeIndex := 0 to CodeSize - 3 do
      if DisasmMap[CodeIndex] = dfNone then begin
        case code[CodeIndex] of
          $55:       // PUSH (E)BP
            asm
              mov eax,self
              mov eax,[eax].code
              add eax,CodeIndex
              inc eax
              mov ax,[eax]
              cmp ax,$EC8B  // 8B EC = MOV (E)BP,(E)SP
              je @nasiel
              cmp ax,$E589  // 89 E5 = MOV (E)BP,(E)SP
              je @nasiel
              jmp @koniec
            @nasiel:
              mov edx,CodeIndex
              mov eax,self
              mov eax,[eax].CAJ
              call TCallsAndJumps.Add
            @koniec:
          end;
        end;
      end;
  end;

  // Disassemblovanie najdenych procedur a kodu na ktory sa odkazuju
  Logger.Info('TDisassembler.DisassembleAll - Phase 2b');
  CAJ.Process(CodeSize);
  DisassemblerCycle;

  //****************************************************************************
  // 3. Phase - Transform non-disassembled bytee to UNSIGNED BYTE data
  //****************************************************************************

  Logger.Info('TDisassembler.DisassembleAll - Phase 3');

  // Set the static parts of the Line string
  Line := #0#0#0#0#0#0#0#0 + ' ' + #0#0;
  for SpaceIndex:= 3 to ilMaxParsedLength do
    Line := Line  + ' ';
  Line := Line + ' ' + 'byte 0x' + #0#0 + ' ''' + #0 + '''';

  // Find all non-disassembled bytes and make then "byte data"
  for CodeIndex := 0 to CodeSize - 1 do
    if DisasmMap[CodeIndex] = dfNone then begin
      Inc(ProgressData.Position);

      // Address
      MemAddress:= CodeIndex + fMemOffset;
      for HexAddressIndex := 1 to 8 do begin
        Line[9 - HexAddressIndex]:= HexDigits[MemAddress and 15];
        MemAddress:= MemAddress shr 4;
      end;

      // Parsed TheByte
      Line[10]:= HexDigits[(code[CodeIndex] shr 4) and 15];
      Line[11]:= HexDigits[code[CodeIndex] and 15];
      Line[ilInstructionMnemonicIndex + 7] := Line[10];
      Line[ilInstructionMnemonicIndex + 8] := Line[11];

      // Character representaion of TheByte
      if code[CodeIndex] >= 32 then begin
        SetLength(Line, ilInstructionMnemonicIndex + 12);
        Line[ilInstructionMnemonicIndex + 11] := Chr(code[CodeIndex]);
      end
      else
        SetLength(Line, ilInstructionMnemonicIndex + 8);

      Disassembled[CodeIndex].Disassembled := Line;

//      Inc(DisasmMap[CodeIndex],dfData);
      Inc(fStatistics.Data);
    end;

  // Vypocet statistickych udajov
  Logger.Info('TDisassembler.DisassembleAll - Statistics');
  fStatistics.InstructionBytes:= CodeSize - fStatistics.Data;
  CodeIndex := CodeSize - 1;
  while (((DisasmMap[CodeIndex] and dfNewInstr) = 0) and (DisasmMap[CodeIndex] <> 0)) do
    Dec(CodeIndex);
  fStatistics.LastItem := CodeIndex;
end;



function TDisassembler.DisassembleBlock(Start, Finish: cardinal): boolean;
type
  TPrefixFlags = record
    group1, group2, group3, group4: boolean;
  end;

var
    Vparam: TParam;                         // Parametre aktualnej instrukcie
    PrefixFlags: TPrefixFlags;
    PrefixStr: string;
    prefixes: TPrefixes;

    AsmByte: byte;
    c: cardinal;
  Is3DNowInstruction: boolean;
    k: cardinal;
    KoniecBloku: boolean;

  Line: string[50 + ilInstructionMnemonicIndex];
  MemAddress: cardinal;
  HexAddressIndex, ParsedIndex, SpaceIndex, LineIndex: integer;
  LastParsedIndex: integer;
  TheByte: byte;

  GroupMapIndex: Integer;
  GroupInstruction, OneByteInstruction: TInstruction;
  SIMDInstruction: TSIMDInstruction;
  FPUInstrIndex: Integer;

  // presunute zhora
  InstructionPrefix: string; // prefix of current instruction
  InstructionOperands: string; // operands of current instruction


  procedure ProcessSIMDInstruction(var Instruction: TInstruction);
  begin
    if prefixes.p66 then SIMDInstruction := Instruction.SIMD_66^
    else if prefixes.pF2 then SIMDInstruction := Instruction.SIMD_F2^
    else if prefixes.pF3 then SIMDInstruction := Instruction.SIMD_F3^
    else SIMDInstruction := Instruction.SIMD^;

    Vparam := SIMDInstruction.Operands;
    Vpc := SIMDInstruction.OperandsCount;
    InstructionName := SIMDInstruction.name;
  end;

begin
  i:= Start;
  InstrAddress:= i;
  KoniecBloku:= false;


  LastParsedIndex := 0;
  Line[9] := ' ';
  for SpaceIndex := 10 to procmat.ilInstructionMnemonicIndex - 1 do
    Line[SpaceIndex] := ' ';


  try

  while (i<=finish) and ((DisasmMap[i] and dfPart)=0) and (not KoniecBloku) do begin                     // Hlavny disassemblovaci cyklus

    Inc(ProgressData.Position, i - InstrAddress);
    if ProgressData.ErrorStatus = errUserTerminated then
      raise EUserTerminatedProcess.Create('');

    operand32:=fBit32;
    address32:=fBit32;
    SegmentOverride:='';

    Is3DNowInstruction:=false;
    vpc:=0;                                 // Vynulovanie poctu operandov pre nasledujucu instrukciu
    genreg16:=false;
    modrm.loaded:=false;
    prefixes.pF2:=false;                    // 'prefixes' sa pouzivaju pri urcovani MMX, SSE, SSE2 instrukcii
    prefixes.pF3:=false;
    prefixes.p66:=false;
    prefixstr:='';
    prefixflags.group1:=false;
    prefixflags.group2:=false;
    prefixflags.group3:=false;
    prefixflags.group4:=false;

    inc(fStatistics.Instructions);
    InstrAddress:= i;
    InstructionPrefix:= '';
    InstructionOperands:= '';

    DisasmMap[InstrAddress]:=dfNewInstr;
//    Disassembled[InstrAddress].flag:=00;

// INFO:
// Iba Group1 zapisuje do "InstructionPrefix"
//
// Zislo by sa skontrolovat, ci je LOCK pouzity len s povolenymi instrukciami, pretoze:
{
  The LOCK prefix can be prepended only to the following instructions and only to those forms
of the instructions where the destination operand is a memory operand: ADD, ADC, AND,
BTC, BTR, BTS, CMPXCHG, CMPXCH8B, DEC, INC, NEG, NOT, OR, SBB, SUB, XOR,
XADD, and XCHG.
}
    try
      while true do begin
        case code[i] of

          // Group 1
          $F0,$F2,$F3: begin
            if prefixflags.group1 then
              raise EUndefinedOpcodeException.Create('Doubled prefix');
            prefixflags.group1:=true;
            case code[i] of
              $F0: begin prefixstr:='lock'; InstructionPrefix:='lock'; end;
              $F2: begin prefixstr:='repne'; prefixes.pF2:=true; end;
              $F3: begin prefixstr:='repe'; prefixes.pF3:=true; end;
            end;
          end;

          // Group 2
          $2E,$36,$3E,$26,$64,$65: begin
            if PrefixFlags.group2 then
              raise EUndefinedOpcodeException.Create('Doubled prefix');
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
            if PrefixFlags.group3 then
              raise EUndefinedOpcodeException.Create('Doubled prefix');
            prefixflags.group3:=true;
            operand32:=not operand32;
  //          if bit32 then prefixstr:=prefixstr + 'o16 '
  //          else prefixstr:=prefixstr + 'o32 ';
  //          if bit32 then InstructionPrefix:='o16'
  //          else InstructionPrefix:='o32';
            prefixes.p66:=true;
          end;

          // Group 4
          $67: begin
            if PrefixFlags.group4 then
              raise EUndefinedOpcodeException.Create('Doubled prefix');
            prefixflags.group4:=true;
            address32:=not address32;
  //          if bit32 then prefixstr:=prefixstr + 'a16 '
  //          else prefixstr:=prefixstr + 'a32 ';
  //          if bit32 then InstructionPrefix:='a16'
  //          else InstructionPrefix:='a32';
          end;

          else
            break;
        end;
        Inc(i);
      end;

      // Nasledujuci riadok je OK. Ak ma instrukcia dvojbajtovy opcode (OF),
      // tak prefixy F2,F3 maju iny vyznam ako "repne, repe".
      if code[i] <> $0F then
        InstructionPrefix:= prefixstr;

      j:=0;

      case OneByte_InstructionSet[code[i]].opcode of

        // OneByte Opcode Group Instructions
        $FE: begin
          ModRM := LoadModRM(code[i+1]);
          GroupMapIndex := 0;
          while GroupMapIndex < cOneByteOpcodeGroupMapSize do begin
            if OneByteOpcodeGroupMap[GroupMapIndex].Opcode = code[i] then begin
              if ModRM.Moder <> 3 then
                GroupInstruction := OneByteOpcodeGroupMap[GroupMapIndex].GroupMem^[ModRM.RegOp]
              else
                GroupInstruction := OneByteOpcodeGroupMap[GroupMapIndex].GroupReg^[ModRM.RegOp];
              Break;
            end
            else
              Inc(GroupMapIndex);
          end;
          if GroupInstruction.OpCode = $D6 then
            raise EUndefinedOpcodeException.Create('Group instruction');

          Vparam:= GroupInstruction.param;
          Vpc:= GroupInstruction.pc;
          InstructionName := GroupInstruction.Name;
        end;

        // FPU Instructions
        $D8: begin
          if code[i+1] <= $BF then begin
            ModRM := LoadModRM(code[i+1]);
            FPUInstrIndex := (code[i] mod 8)*8 + ModRM.RegOp;
            InstructionName := FPU_A_InstructionSet[FPUInstrIndex].name;
            if InstructionName = '' then
              raise EUndefinedOpcodeException.Create('FPU instruction');

            OperandSize := FPU_A_InstructionSet[FPUInstrIndex].par;
            InstructionOperands := SpracujModRM(mem, twoorfour);
          end
          else begin
            InstructionName := FPU_B_InstructionSet[code[i]][code[i+1]].Name;
            InstructionOperands := FPU_B_InstructionSet[code[i]][code[i+1]].par;
            if InstructionName = '' then
              raise EUndefinedOpcodeException.Create('FPU instruction');
            Inc(i);
          end;
        end;

        // TwoByte Opcode Instruction
        $0F: begin
          inc(i);
          case TBOInstruction_set[code[i]].opcode of

            // Group instruction
            $FE: begin
              ModRM := LoadModRM(code[i+1]);
              GroupMapIndex := 0;
              while GroupMapIndex < cTwoByteOpcodeGroupMapSize do begin
                if TwoByteOpcodeGroupMap[GroupMapIndex].Opcode = code[i] then begin
                  if ModRM.Moder <> 3 then
                    GroupInstruction := TwoByteOpcodeGroupMap[GroupMapIndex].GroupMem^[ModRM.RegOp]
                  else
                    GroupInstruction := TwoByteOpcodeGroupMap[GroupMapIndex].GroupReg^[ModRM.RegOp];
                  Break;
                end
                else
                  Inc(GroupMapIndex);
              end;
              case GroupInstruction.typ of
                itUndefined: raise EUndefinedOpcodeException.Create('Group instruction');

                // SIMD group instructions
                itSIMD: begin
                  if prefixes.p66 then SIMDInstruction := GroupInstruction.SIMD_66^
                  else if prefixes.pF2 then SIMDInstruction := GroupInstruction.SIMD_F2^
                  else if prefixes.pF3 then SIMDInstruction := GroupInstruction.SIMD_F3^
                  else SIMDInstruction := GroupInstruction.SIMD^;

                  Vparam := SIMDInstruction.Operands;
                  Vpc := SIMDInstruction.OperandsCount;
                  InstructionName := SIMDInstruction.name;
                end;

                // Ordinary group instructions
                itOrdinary: begin
                  Vparam := GroupInstruction.param;
                  Vpc := GroupInstruction.pc;
                  InstructionName := GroupInstruction.name;
                end;

                else
                  raise EIllegalState.Create('Illegal instruction type of group instruction');
              end;
            end;

            // SIMD instructions
            $60: begin
              if prefixes.p66 then SIMDInstruction := GroupInstruction.SIMD_66^
              else if prefixes.pF2 then SIMDInstruction := GroupInstruction.SIMD_F2^
              else if prefixes.pF3 then SIMDInstruction := GroupInstruction.SIMD_F3^
              else SIMDInstruction := GroupInstruction.SIMD^;

              Vparam := SIMDInstruction.Operands;
              Vpc := SIMDInstruction.OperandsCount;
              InstructionName := SIMDInstruction.name;
            end;

            $0F: begin
              Is3DNowInstruction := true;
              Vpc := 2;
              Vparam.p1 := GREGq;
              Vparam.p2 := MODq;
            end;

            $D6: raise EUndefinedOpcodeException.Create('Two byte instruction');

            // Obycajne TwoByte Instructions
            else begin
              Vparam := TBOInstruction_set[code[i]].param;
              Vpc := TBOInstruction_set[code[i]].pc;
              InstructionName := TBOinstruction_set[code[i]].name;
            end;
          end;  // End of "case"
        end;

        // One byte opcode instructions
        else
          begin
            OneByteInstruction :=  OneByte_InstructionSet[code[i]];
            if OneByteInstruction.opcode = $D6 then
              raise EUndefinedOpcodeException.Create('One byte instruction')
            else begin
              Vparam := OneByteInstruction.param;
              Vpc := OneByteInstruction.pc;
              if not OneByteInstruction.AddOp then
                InstructionName := OneByteInstruction.name
              else
                if operand32 then
                  InstructionName := OneByteInstruction.name32
                else
                  InstructionName := OneByteInstruction.name16;
            end;
          end;
      end;  // End of "case code[i] of"



      // Instructions' parameters processing (except FPU instructions which parameters are processed now)
      OperandSize := szEmpty;
      case Vpc of                             // Spracovanie parametrov podla ich poctu
        1: begin
            case Vparam.p1 of
              MODb: OperandSize:=szByte;
              MODv: If Operand32 then OperandSize:=szDword else OperandSize:=szWord;
              MODw: OperandSize:=szWord;
              MODd: OperandSize:=szDword;
            end;
            InstructionOperands:=SpracujParameter(Vparam.p1);
          end;
        2: begin
            case Vparam.p1 of
              MODb: OperandSize:=szByte;
              MODv: if Operand32 then
                      OperandSize:=szDword
                    else
                      OperandSize:=szWord;
              MODw: OperandSize:=szWord;
              MODd: OperandSize:=szDword;
            end;
            // Function "SpracujParameter" has side effects its second calling depends on.
            // So we must ensure that the first parameter is process before second.
            // We cannot use "InstructionOperands := SpracujParameter(Vparam.p1) + ',' + SpracujParameter(Vparam.p2);"
            // because the order of proc/fun calling in such statement is not guaranteed (depends on compiler)
            InstructionOperands := SpracujParameter(Vparam.p1) + ',';
            OperandSize := szEmpty;
            InstructionOperands := InstructionOperands + SpracujParameter(Vparam.p2);
          end;
        3: begin
            // Function "SpracujParameter" has side effects its second calling depends on.
            // So we must ensure that the first parameter is process before second.
            // We cannot use "InstructionOperands := SpracujParameter(Vparam.p1) + ',' + SpracujParameter(Vparam.p2) + ',' + SpracujParameter(Vparam.p3);"
            // because the order of proc/fun calling in such statement is not guaranteed (depends on compiler)
            InstructionOperands := SpracujParameter(Vparam.p1) + ',';
            InstructionOperands := InstructionOperands + SpracujParameter(Vparam.p2) + ',';
            InstructionOperands := InstructionOperands + SpracujParameter(Vparam.p3);
          end;
      end;


      // Detekcia 3DNow! instrukcii
      if Is3DNowInstruction then begin
        Inc(i);
        if _3DNow_InstructionSet[code[i]].opcode = $D6 then
          raise EUndefinedOpcodeException.Create('3DNow! instruction')
        else
          InstructionName := _3DNow_InstructionSet[code[i]].Name;
      end;

  // -- Kvoli testovanie intrukcii je obcas vypnute ukoncovanie bloku:

      //  Instrukcie ukoncujuce blok
      case InstructionName[1] of
        'J': if InstructionName[2] = 'M' then KoniecBloku := true; // JMP, JMPx
        'R': if InstructionName[2] = 'E' then KoniecBloku := true; // RET, RETN, RETF
        'I': if InstructionName[2] = 'R' then KoniecBloku := true; // IRET, IRETx
      end;

  // --

      //  Detekcia importovanych funkcii pomocou CALLN a JMPN
      if (code[InstrAddress] = $FF) then begin
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
        if (asmbyte = 1) and (Length(InstructionOperands) = 18) then begin
          SetLength(Imported, Length(Imported)+1);
          Imported[Length(Imported)-1]:= InstrAddress;
        end;
      end;

      // Vlozenie specifikatora velkosti pre instrukcie MOVZX a MOVSX
      // Nieco podobne by sa mozno zislo aj pre PUNPCKxxx instrukcie
      if (code[InstrAddress] = $0F) then
        case code[InstrAddress+1] of
          // MOVZX
          $B6: InstructionOperands:= InsertStr('byte ', InstructionOperands, Pos(',', InstructionOperands) + 1);
          $B7: InstructionOperands:= InsertStr('word ', InstructionOperands, Pos(',', InstructionOperands) + 1);
          // MOVSX
          $BE: InstructionOperands:= InsertStr('byte ', InstructionOperands, Pos(',', InstructionOperands) + 1);
          $BF: InstructionOperands:= InsertStr('word ', InstructionOperands, Pos(',', InstructionOperands) + 1);
        end;

      // Ak je posledny bajt aktual. instrukcie uz sucastou nejakej inej instrukcie alebo je uz mimo bloku,
      // tak sa odstrani flag dfInstruction z prveho bajtu aktual. instrukcie a ukonci sa aktual. blok
      // pozn.: nad tymto a veci s tym suvisiacimi sa treba este zamysliet, napr. ci ma teda aktualny blok zmysel(alebo ten prave zacinajuci)
      if ((DisasmMap[i] and dfPart)<>0) or (i>finish) then begin
        Dec(DisasmMap[InstrAddress],dfNewInstr);
        KoniecBloku:=true;
      end
      else begin

        { Construct instruction line of Disassembled from its parts }

        SetLength(Line, ilInstructionMnemonicIndex - 1);

        // Address
        MemAddress:= InstrAddress + fMemOffset;
        for HexAddressIndex := 1 to 8 do begin
          Line[9 - HexAddressIndex]:= HexDigits[MemAddress and 15];
          MemAddress:= MemAddress shr 4;
        end;

        // Parsed
        for ParsedIndex := 0 to (i - InstrAddress) do begin
          TheByte := code[InstrAddress + Cardinal(ParsedIndex)];
          Line[10 + 2*ParsedIndex] := HexDigits[(TheByte shr 4) and 15];
          Line[10 + 2*ParsedIndex + 1] := HexDigits[TheByte and 15];
        end;

        // Spaces
        for SpaceIndex := 10 + 2*(i - InstrAddress + 1) to LastParsedIndex do
          Line[SpaceIndex] := ' ';

        LastParsedIndex := 9 + 2*(i - InstrAddress + 1);

        // Prefix
        LineIndex := ilInstructionMnemonicIndex;
        if InstructionPrefix <> '' then begin
          for ParsedIndex := 1 to Length(InstructionPrefix) do
            Line[procmat.ilInstructionMnemonicIndex - 1 + ParsedIndex] := InstructionPrefix[ParsedIndex];
          Inc(LineIndex, Length(InstructionPrefix));
          Line[LineIndex] := ' ';
          Inc(LineIndex);
        end;

        // Name
        for ParsedIndex := 1 to Length(InstructionName) do
          Line[LineIndex - 1 + ParsedIndex] := InstructionName[ParsedIndex];
        Inc(LineIndex, Length(InstructionName));
        Line[LineIndex] := ' ';
        Inc(LineIndex);

        // Operands
        for ParsedIndex := 1 to Length(InstructionOperands) do
          Line[LineIndex - 1 + ParsedIndex] := InstructionOperands[ParsedIndex];
        Inc(LineIndex, Length(InstructionOperands) - 1);

        SetLength(Line, LineIndex);
        Disassembled[InstrAddress].Disassembled := Line;

{
        // Create intruction line of Disassembled from particles
        Disassembled[InstrAddress].Disassembled :=
          IntToHex(InstrAddress + fMemOffset, 8) + ' ' +
          StringRightPad(DataToHex(code[InstrAddress], i - InstrAddress + 1), ilMaxParsedLength + 1, ' ') +
          IfThen(InstructionPrefix <> '', InstructionPrefix + ' ') +
          InstructionName + ' ' +
          InstructionOperands;
}

        // prida kazdemu bajtu aktualnej instrukcie flag dfPart, t.j. ze je sucastou nejakej instruckie
        for k := InstrAddress to i do
          Inc(DisasmMap[k], dfPart);
      end;

      Inc(i);

    except
      on EUndefinedOpcodeException do begin
        Inc(Disassembled[InstrAddress].ReferCount);
        SetLength(Disassembled[InstrAddress].refer, Disassembled[InstrAddress].ReferCount);
        Disassembled[InstrAddress].refer[disassembled[InstrAddress].refercount-1]:= ';' + IntToHex(InstrAddress,8) + ' ' + DataToHex(code[InstrAddress], i - InstrAddress + 1) + '...                  UNDEFINED OPCODE!';
        InstructionPrefix:= '';
        DisasmMap[InstrAddress]:= 0;
        KoniecBloku:= true;
      end
      else
        raise;
    end;
  end;   // End of   Hlavny disassemblovaci cyklus

  except
    on E: Exception do begin
      Logger.Fatal(
        'DisassembleBlock(0x' + IntToHex(start, 8) + ', 0x' + IntToHex(finish, 8) + '): ' +
        E.Message + ' [InstrAddress = 0x' + IntToHex(InstrAddress, 8) + '; CodeIndex = 0x' + IntToHex(i, 8) + ']'
      );

      raise;
    end;
  end;

  fBlocks.Add(start, i - start);
  result:=true;
end;



function TDisassembler.SpracujParameter(a:Tparameter):string;
begin
  case a of
    MODb: result:=SpracujModRM(regmem, onebyte);
    MODw: result:=SpracujModRM(regmem, twobyte);
    MODv: result:=SpracujModRM(regmem, twoorfour);
    MODd: result:=SpracujModRM(regmem, fourbyte);
    MODp: result:=SpracujModRM(regmem, fourorsix);
    MODq: result:=SpracujModRM(regmem, mmx);
    MODdq: result:=SpracujModRM(regmem, xmm);

    GREGb: result:=SpracujModRM(greg,onebyte);
    GREGw: result:=SpracujModRM(greg,twobyte);
    GREGv: result:=SpracujModRM(greg,twoorfour);
    GREGd: result:=SpracujModRM(greg,fourbyte);
    GREGq: result:=SpracujModRM(greg,mmx);
    GREGdq: result:=SpracujModRM(greg,xmm);

    SREGw: result:=SpracujModRM(sreg,twobyte);
    CREGd: result:=SpracujModRM(creg,fourbyte);
    DREGd: result:=SpracujModRM(dreg,fourbyte);
    TREGd: result:=SpracujModRM(treg,fourbyte);

    IMMb: result:=SpracujImmediate(onebyte);
    IMMv: result:=SpracujImmediate(twoorfour);
    IMMw: result:=SpracujImmediate(twobyte);
    IMMp: result:=SpracujImmediate(fourorsix);

    RELb: result:=SpracujRelative(onebyte);
    RELv: result:=SpracujRelative(twoorfour);
    RELw: result:=SpracujRelative(twobyte);

    OFFb: result:=SpracujOffset(onebyte);
    OFFv: result:=SpracujOffset(twoorfour);

    ax: result:=SpracujGenReg+'AX';
    bx: result:=SpracujGenReg+'BX';
    cx: result:=SpracujGenReg+'CX';
    dx: result:=SpracujGenReg+'DX';
    si: result:=SpracujGenReg+'SI';
    di: result:=SpracujGenReg+'DI';
    bp: result:=SpracujGenReg+'BP';
    sp: result:=SpracujGenReg+'SP';
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




initialization


end.
