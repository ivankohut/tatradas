{ TODO:
  - pridat moznost ukoncenia bloku v 16 bit. mode v pripade instrukcii
    MOV AH,byte 0x09
    INT byte 0x21

  Notes:
  - We do not check if the LOCK prefix is used together with allowed instructions only, according to the INTEL manual:
      "The LOCK prefix can be prepended only to the following instructions and only to those forms
      of the instructions where the destination operand is a memory operand: ADD, ADC, AND,
      BTC, BTR, BTS, CMPXCHG, CMPXCH8B, DEC, INC, NEG, NOT, OR, SBB, SUB, XOR,
      XADD, and XCHG."
}


unit x86Disassembler;

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
  DisassemblerUnit,
  x86DisassemblerTypes,
  x86Instructions,
  StrUtils
  ;

type

  Tx86Disassembler = class(TDisassembler)
  private
    code: TByteDynArray; // machine code to be disassembled
    fDisasmMap: TByteDynArray;
    fStatistics: TStatistics;
    fBit32: boolean;
    fMemOffset: cardinal;

    i: cardinal; // Index in "code" array

    // Fields containing values specific to the current instruction
    InstrAddress: cardinal; // address of current instruction in "code" array <=> address of current instruction relative to the beginning of code section
    SegmentOverride: string;
    FirstCharOfName: char;
    operand32: boolean; // Operand size attribute
    address32: boolean; // Address size attribute
    ModRM: TModRM;
    SIB: TSIB;

    function ProcessOperand(Operand: TOperand): string;
    function LoadModRM(ModRMValue: byte): TModRM;
    function ProcessModRM(OperandType: TModRMOperandType; OperandSize: TModRMOperandSize): string;
    function LoadSIB(SIBValue: byte): TSIB;
    function ProcessSIB: string;
    function ProcessImmediate(OperandSize: TModRMOperandSize): string;
    function ProcessRelative(OperandSize: TModRMOperandSize): string;
    function ProcessOffset: string;
    function ProcessGenPurpRegister: string;

//    function SpracujImmediatePascal(OperandSize: TModRMOperandSize): string;

  protected
    function DisassembleBlock(Start, Finish: cardinal): boolean; override;

  public
    constructor Create(SectionCode: TByteDynArray; var DisassemblerMap: TByteDynArray; MemOffset: cardinal; Bit32: boolean);
    destructor Destroy; override;
    procedure DisassembleAll; override;
  end;


Implementation


const
  ByteRegister: array [0..7] of string[2] = (
    ('AL'),
    ('CL'),
    ('DL'),
    ('BL'),
    ('AH'),
    ('CH'),
    ('DH'),
    ('BH')
  );

  WordRegister: array [0..7] of string[2] = (
    ('AX'),
    ('CX'),
    ('DX'),
    ('BX'),
    ('SP'),
    ('BP'),
    ('SI'),
    ('DI')
  );

  DWordRegister: array [0..7] of string[3] = (
    ('EAX'),
    ('ECX'),
    ('EDX'),
    ('EBX'),
    ('ESP'),
    ('EBP'),
    ('ESI'),
    ('EDI')
  );

  SegmentRegister: array [0..7] of string[2] = (
    ('ES'),
    ('CS'),
    ('SS'),
    ('DS'),
    ('FS'),
    ('GS'),
    ('??'),
    ('??')
  );

  ControlRegister: array [0..7] of string[3] = (
    ('CR0'),
    ('???'),
    ('CR2'),
    ('CR3'),
    ('CR4'),
    ('???'),
    ('???'),
    ('???')
  );

  DebugRegister: array [0..7] of string[3] = (
    ('DR0'),
    ('DR1'),
    ('DR2'),
    ('DR3'),
    ('DR4'),
    ('DR5'),
    ('DR6'),
    ('DR7')
  );

  TestRegister: array [0..7] of string[3] = (
    ('???'),
    ('???'),
    ('???'),
    ('TR3'),
    ('TR4'),
    ('TR5'),
    ('TR6'),
    ('TR7')
  );

  MMXRegister: array [0..7] of string[3] = (
    ('MM0'),
    ('MM1'),
    ('MM2'),
    ('MM3'),
    ('MM4'),
    ('MM5'),
    ('MM6'),
    ('MM7')
  );

  XMMRegister: array [0..7] of string[4] = (
    ('XMM0'),
    ('XMM1'),
    ('XMM2'),
    ('XMM3'),
    ('XMM4'),
    ('XMM5'),
    ('XMM6'),
    ('XMM7')
  );


  Modrm16bitAddress_register: array [0..7] of string = (
    ('BX+SI'),
    ('BX+DI'),
    ('BP+SI'),
    ('BP+DI'),
    ('SI'),
    ('DI'),
    ('BP'),
    ('BX')
  );

{
  Nacita ModRM do struktury TModRM
  ModRM = 2 + 3 + 3 bits (Moder + RegOp + RM)
  Pouzivam assembler, lebo operatory "shl" a "shr" v Delphi pracuju s typom integer (4 bajty) a my potrebujeme 1 bajt
  Parametre:
    EAX - self
    EDX - ModRM byte
    ECX - adresa vysledku
}
function Tx86Disassembler.LoadModRM(ModRMValue: byte): TModRM;
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
function Tx86Disassembler.LoadSIB(SIBValue: byte): TSIB;
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



function Tx86Disassembler.ProcessSIB: string;
begin
  Inc(i);
  SIB := LoadSIB(code[i]);
  if not ((ModRM.Moder = 0) and (SIB.Base = 5)) then begin
    result := DWordRegister[SIB.Base];

    if SIB.Index <> 4 then begin
      result := result + '+' + DWordRegister[SIB.Index];
      if SIB.Scale <> 0 then
        result := result + '*' + PowersOfTwoStr[SIB.Scale];
    end;
  end
  else begin
    Inc(i);
    result := '0x' + IntToHex(cardinal((@code[i])^), 8);
    Inc(i, 3);

    if SIB.index <> 4 then begin
      result := result + '+' + DWordRegister[SIB.Index];
      if SIB.scale <> 0 then
        result := result + '*' + PowersOfTwoStr[SIB.Scale];
    end;
  end;
end;



function Tx86Disassembler.ProcessModRM(OperandType: TModRMOperandType; OperandSize: TModRMOperandSize): string;

  function GetRegister(Index: byte): string;
  begin
    case OperandSize of
      os1: result := ByteRegister[Index];
      os2: result := WordRegister[Index];
      os2Or4, os4Or6:
        if operand32 then
          result := DWordRegister[Index]
        else
          result := WordRegister[Index];
      os4, osR4_M1, osR4_M2: result := DWordRegister[Index];
      os8, osR8_M4: result := MMXRegister[Index];
      os16, osR16_M8, osR16_M4, osR16_M2: result := XMMRegister[Index];
      else
        raise EUndefinedOpcodeException.Create('ModRM - Bad register size.  OperandType = ' + IntToStr(Ord(OperandType)) + ', OperandSize = ' + IntToStr(Ord(OperandSize)) +
          ', Moder = ' + IntToStr(ModRM.Moder) + ', RegOp = ' + IntToStr(ModRM.RegOp) + ', RM = ' + IntToStr(ModRM.RM));

    end;
  end;

begin
  result := '';

  if not ModRM.Loaded then begin
    Inc(i);
    ModRM := LoadModRM(code[i]);
    ModRM.Loaded := true;
  end;

  if OperandType = otRegister then
    result := GetRegister(ModRM.RegOp)
  else if (ModRM.Moder = 3) and ((OperandType = otRMReg) or (OperandType = otRMRegMem)) then
    result := GetRegister(ModRM.RM)
  else if (ModRM.Moder <> 3) and ((OperandType = otRMMem) or (OperandType = otRMRegMem)) then begin

    // Address size 32 bit
    if Address32 then begin
      case modrm.moder of
        0: begin
            result := '[' + SegmentOverride;
            case ModRM.RM of
              5: begin
                Inc(i);
                result := result + '0x' + IntToHex(cardinal((@code[i])^), 8);
                Inc(i, 3);
              end;
              4: result := result + ProcessSIB;
              else
                result := result + DwordRegister[modrm.rm];
            end;
            result := result + ']';
          end;

        1: begin
            result := '[' + SegmentOverride;
            if modrm.rm = 4 then
              result := result + ProcessSIB
            else
              result := result + DwordRegister[modrm.rm];
            Inc(i);
            result := result + ByteToSignedHex(code[i])+']';
          end;

        2: begin
            result := '[' + SegmentOverride;
            if modrm.rm = 4 then
              result := result + ProcessSIB
            else
              result := result + DwordRegister[modrm.rm];
            Inc(i);
            result := result + '+' + '0x' + IntToHex(cardinal((@code[i])^), 8) + ']';
            Inc(i, 3);
          end;
      end;
    end

    // Address size 16 bit
    else begin
      case Modrm.Moder of
        0: begin
          result := '[' + SegmentOverride;
          if ModRM.RM = 6 then begin
            Inc(i, 2);
            result := result + '0x' + IntToHex(code[i], 2) + IntToHex(code[i - 1], 2);
          end
          else
            result := result + modrm16bitaddress_register[modrm.rm];
          result := result + ']';
        end;
        1: begin
          Inc(i);
          result := '[' + SegmentOverride + modrm16bitaddress_register[modrm.rm] + ByteToSignedHex(code[i]) + ']';
        end;
        2: begin
          Inc(i, 2);
          result := '[' + SegmentOverride + modrm16bitaddress_register[modrm.rm] + '+' + '0x' + IntToHex(code[i], 2) + IntToHex(code[i - 1], 2) + ']';
        end;
      end;
    end;
    case OperandSize of
      osNone: ; // LEA
      os1, osR4_M1: result := 'byte ' + result;
      os2, osR4_M2, osR16_M2: result := 'word ' + result;
      os2or4:
        if operand32 then
          result := 'dword ' + result
        else
          result := 'word ' + result;
      os4, osR8_M4, osR16_M4: result := 'dword ' + result;
      os4Or6:
        // Indirect far pointer JMP or CALL (not LES and friends)
        if FirstCharOfName <> 'L' then begin
          if operand32 then
            result := 'dword far' + result
          else
            result := 'word far' + result;
        end;
      os8, osR16_M8: result := 'qword ' + result;
      os10: result := 'tword ' + result;
      os16: result := 'dqword ' + result;
    end;
  end
  else if (OperandType = otSegmentReg) and (OperandSize = os2) then
    result := SegmentRegister[ModRM.RegOp]
  else if OperandType = otControlReg then
    result := ControlRegister[ModRM.RegOp]
  else if OperandType = otDebugReg then
    result := DebugRegister[ModRM.RegOp]
  else if OperandType = otTestReg then
    result := TestRegister[ModRM.RegOp]
  else
    raise EUndefinedOpcodeException.Create('ModRM - Bad operand type.  OperandType = ' + IntToStr(Ord(OperandType)) + ', OperandSize = ' + IntToStr(Ord(OperandSize)) +
      ', Moder = ' + IntToStr(ModRM.Moder) + ', RegOp = ' + IntToStr(ModRM.RegOp) + ', RM = ' + IntToStr(ModRM.RM));
end;


{
function Tx86Disassembler.SpracujImmediatePascal(OperandSize: TModRMOperandSize): string;
begin
  Inc(i);
  case OperandSize of
    osNone: ;
    os1: begin
      result := 'byte 0x' + IntToHex(code[i], 2);
    end;
    os2: begin
      result := 'word 0x' + IntToHex(word((@code[i])^), 4);
      Inc(i);
    end;
    os2or4: begin
      if operand32 then begin
        result := 'dword 0x' + IntToHex(cardinal((@code[i])^), 8);
        Inc(i, 3);
      end
      else begin
        result := 'word 0x' + IntToHex(word((@code[i])^), 4);
        Inc(i);
      end;
    end;
    os4: begin
      result := 'dword 0x' + IntToHex(cardinal((@code[i])^), 8);
      Inc(i, 3);
    end;
    os4or6: begin
      if operand32 then begin
        result := '0x' + IntToHex(word((@code[i + 4])^), 4) + ':' + '0x' + IntToHex(cardinal((@code[i])^), 8);
        Inc(i, 5);
      end
      else begin
        result := '0x' + IntToHex(word((@code[i + 2])^), 4) + ':' + '0x' + IntToHex(word((@code[i])^), 4);
        Inc(i, 3);
      end;
    end;
    else
      raise EUndefinedOpcodeException.Create('Immediate - bad operand size -  ' + IntToStr(Ord(OperandSize)));
  end;
end;
}


function Tx86Disassembler.ProcessImmediate(OperandSize: TModRMOperandSize): string;
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
    cmp OperandSize,os1           // ONE BYTE
    jne @CheckTwobyte
    movzx ecx,byte[eax]     // parameter instrukcie -> ECX
    jmp @EndCase

  @CheckTwobyte:
    cmp OperandSize,os2           // TWO BYTE
    jne @CheckTwoOrFour

  @_16:
    movzx ecx,word ptr[eax]
    inc ebx
    jmp @EndCase

  @CheckTwoOrFour:
    cmp OperandSize,os2or4
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
  if OperandSize = os4or6 then
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



function Tx86Disassembler.ProcessRelative(OperandSize: TModRMOperandSize): string;
var
  ReferenceType: TReferenceType;
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
    cmp OperandSize,os1           // ONE BYTE
    jne @CheckTwobyte
    mov dl,byte[eax]     // parameter instrukcie -> ECX
    movsx ecx,dl          //
    mov parsedcount,2       // pocet cifier PARSED
    add ecx,ebx             // pripocitame aktualnu pozicu
    inc ecx                 // zvysime o 1, lebo skok je relativny od nasledujucej instrukcie
    jmp @EndCase


  @CheckTwobyte:
    cmp OperandSize,os2           // TWO BYTE
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
    cmp OperandSize,os2or4
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

  // skok na zapornu adresu (treba este zohladnit MemOffset !!!)
  if address < 0 then
    result := '-0x' + IntToHex(Abs(address), 8)
  else begin
    result := '0x' + IntToHex(cardinal(address) + fMemOffset, 8);
    // skok za koniec kodovej sekcie
    if cardinal(address) > fCodeSize-1 then
      Exit
    else begin
      CAJ.Add(address);
      case FirstCharOfName of
        'J': ReferenceType := rtJump;
        'C': ReferenceType := rtCall;
        'L': ReferenceType := rtLoop;
      end;
      AddReference(InstrAddress, address, ReferenceType);
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



function Tx86Disassembler.ProcessGenPurpRegister: string;
begin
  if operand32 then
    result:= 'E'
  else
    result:= '';
end;



function Tx86Disassembler.ProcessOffset: string;
begin
  Inc(i);
  if Address32 then begin
    result := '[' + SegmentOverride + '0x' + IntToHex(cardinal((@code[i])^), 8) + ']';
    Inc(i, 3);
  end
  else begin
    result := '[' + SegmentOverride + '0x' + IntToHex(word((@code[i])^), 4) + ']';
    Inc(i);
  end;
end;



constructor Tx86Disassembler.Create(SectionCode: TByteDynArray; var DisassemblerMap: TByteDynArray; MemOffset: cardinal; Bit32: boolean);
begin
  inherited Create;
  code:= SectionCode;
  fCodeSize:= Length(code) - CodeArrayReserveSize;
  fDisasmMap:= DisassemblerMap;
  SetLength(Disassembled, fCodeSize);
  CAJ:= TCallsAndJumps.Create(fDisasmMap);
  fMemOffset:= MemOffset;
  fBit32:= Bit32;
end;



destructor Tx86Disassembler.Destroy;
begin
  CAJ.Free;
  inherited;
end;



procedure Tx86Disassembler.DisassembleAll;
var
  CodeIndex: cardinal;
  HexAddressIndex: integer;
  Line: string[ilInstructionMnemonicIndex + 12 + 1];
  SpaceIndex: integer;
  MemAddress: cardinal;
begin
  ProgressData.Maximum:= fCodeSize;
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
  if fCodeSize >= 3 then begin
    for CodeIndex := 0 to fCodeSize - 3 do
      if fDisasmMap[CodeIndex] = dfNone then begin
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
  CAJ.Process(fCodeSize);
  DisassemblerCycle;

  //****************************************************************************
  // 3. Phase - Transform non-disassembled bytes to UNSIGNED BYTE data
  //****************************************************************************

  Logger.Info('TDisassembler.DisassembleAll - Phase 3');

  // Set the static parts of the Line string
  Line := #0#0#0#0#0#0#0#0 + ' ' + #0#0;
  for SpaceIndex:= 3 to ilMaxParsedLength do
    Line := Line  + ' ';
  Line := Line + ' ' + 'byte 0x' + #0#0 + ' ''' + #0 + '''';

  // Find all non-disassembled bytes and make then "byte data"
  for CodeIndex := 0 to fCodeSize - 1 do
    if fDisasmMap[CodeIndex] = dfNone then begin
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

      Disassembled[CodeIndex].DisassembledLine := Line;

//      Inc(fDisasmMap[CodeIndex],dfData);
      Inc(fStatistics.Data);
    end;

  // Vypocet statistickych udajov
  Logger.Info('TDisassembler.DisassembleAll - Statistics');
  fStatistics.InstructionBytes:= fCodeSize - fStatistics.Data;
  CodeIndex := fCodeSize - 1;
  while (((fDisasmMap[CodeIndex] and dfNewInstr) = 0) and (fDisasmMap[CodeIndex] <> 0)) do
    Dec(CodeIndex);
  fStatistics.LastItem := CodeIndex;
end;



function Tx86Disassembler.DisassembleBlock(Start, Finish: cardinal): boolean;
var
  PrefixGroups: set of TPrefixGroup;
  PrefixStr: string;
  SIMDPrefix: TSIMDPrefix;

  Operands: TOperands;  // Parametre aktualnej instrukcie
  OperandsCount: Byte;  // Pocet parametrov aktualnej instrukcie
  EndBlock: boolean;
  GroupMapIndex: Integer;
  GroupInstruction, OneByteOpcodeInstruction, TwoByteOpcodeInstruction: TInstruction;
  SIMDInstructionPtr: PSIMDInstruction;
  SIMDInstruction: TSIMDInstruction;
  FPUInstrIndex: Integer;

  InstructionPrefix: string; // prefix of current instruction
  InstructionName: string; // name of current instruction
  InstructionOperands: string; // operands of current instruction

  Line: string[50 + ilInstructionMnemonicIndex];
  MemAddress: cardinal;
  HexAddressIndex, ParsedIndex, SpaceIndex, LineIndex: integer;
  LastParsedIndex: integer;
  InstrByteIndex: cardinal;
  TheByte: byte;
  InstrCodeIndex: Cardinal;

begin
  i := Start;
  InstrAddress := i;
  EndBlock := false;


  LastParsedIndex := 0;
  Line[9] := ' ';
  for SpaceIndex := 10 to procmat.ilInstructionMnemonicIndex - 1 do
    Line[SpaceIndex] := ' ';


  try

    // Main loop of disassembler
    while (i<=finish) and ((fDisasmMap[i] and dfPart)=0) and (not EndBlock) do begin

      Inc(ProgressData.Position, i - InstrAddress);
      if ProgressData.ErrorStatus = errUserTerminated then
        raise EUserTerminatedProcess.Create('');

      // Set variables to default values
      operand32 := fBit32;
      address32 := fBit32;

      SegmentOverride := '';
      PrefixGroups := [];
      SIMDPrefix := simdNone;
      PrefixStr := '';

      OperandsCount := 0;
      ModRM.Loaded := false;


      InstructionPrefix := '';
      InstructionOperands := '';

      InstrAddress := i;

      try

        // Prefix processing
        while true do begin
          case code[i] of

            // Group 1
            $F0, $F2, $F3: begin
              if pgOne in PrefixGroups then
                raise EUndefinedOpcodeException.Create('Doubled prefix');
              PrefixGroups := PrefixGroups + [pgOne];

              case code[i] of
                $F0: begin
                  prefixstr := 'lock';
                  InstructionPrefix := 'lock';
                end;
                $F2: begin
                  prefixstr := 'repne';
                  SIMDPrefix := simdF2;
                end;
                $F3: begin
                  prefixstr := 'repe';
                  SIMDPrefix := simdF3;
                end;
              end;
            end;

            // Group 2
            $2E, $36, $3E, $26, $64, $65: begin
              if pgTwo in PrefixGroups then
                raise EUndefinedOpcodeException.Create('Doubled prefix');
              PrefixGroups := PrefixGroups + [pgTwo];

              case code[i] of
                $2E: SegmentOverride := 'CS:';
                $36: SegmentOverride := 'SS:';
                $3E: SegmentOverride := 'DS:';
                $26: SegmentOverride := 'ES:';
                $64: SegmentOverride := 'FS:';
                $65: SegmentOverride := 'GS:';
              end;
            end;

            // Group 3
            $66: begin
              if pgThree in PrefixGroups then
                raise EUndefinedOpcodeException.Create('Doubled prefix');
              PrefixGroups := PrefixGroups + [pgThree];

              operand32 := not operand32;
    //          if bit32 then prefixstr:=prefixstr + 'o16 '
    //          else prefixstr:=prefixstr + 'o32 ';
    //          if bit32 then InstructionPrefix:='o16'
    //          else InstructionPrefix:='o32';
              SIMDPrefix := simd66;
            end;

            // Group 4
            $67: begin
              if pgFour in PrefixGroups then
                raise EUndefinedOpcodeException.Create('Doubled prefix');
              PrefixGroups := PrefixGroups + [pgFour];

              Address32 := not address32;
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
          InstructionPrefix := prefixstr;


        case OneByteOpcode_InstructionSet[code[i]].typ of

          // OneByte Opcode Group Instructions
          itGroup: begin
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
            if GroupInstruction.typ = itUndefined then
              raise EUndefinedOpcodeException.Create('Group instruction');

            Operands := GroupInstruction.Operands;
            OperandsCount := GroupInstruction.OperandsCount;
            InstructionName := GroupInstruction.Name;
          end;

          // FPU Instructions
          itFPU: begin
            if code[i+1] <= $BF then begin
              ModRM := LoadModRM(code[i+1]);
              FPUInstrIndex := (code[i] mod 8)*8 + ModRM.RegOp;
              InstructionName := FPU_A_InstructionSet[FPUInstrIndex].name;
              if InstructionName = '' then
                raise EUndefinedOpcodeException.Create('FPU instruction');

              OperandsCount := 1;
              case FPU_A_InstructionSet[FPUInstrIndex].par of
                szWord: Operands.p1 := M16;
                szDWord: Operands.p1 := M32;
                szQWord: Operands.p1 := M64;
                szTWord: Operands.p1 := M80;
                szEmpty: Operands.p1 := MMM; // TODO: dalsie typ ako 14/28 alebo 98/108
              end;
              // TODO: asi jke vhodne spracovanie FPU parametrov riesit trochu zvlast (a nespinit tym standardny disassembler)

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
          itTwoByteOpcodeExtension: begin
            Inc(i);
            case TwoByteOpcode_InstructionSet[code[i]].typ of

              // Group instruction
              itGroup: begin
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
                    case SIMDPrefix of
                      simdNone: SIMDInstructionPtr := GroupInstruction.SIMD;
                      simd66: SIMDInstructionPtr := GroupInstruction.SIMD_66;
                      simdF2: SIMDInstructionPtr := GroupInstruction.SIMD_F2;
                      simdF3: SIMDInstructionPtr := GroupInstruction.SIMD_F3;
                    end;
                    if SIMDInstructionPtr = nil then
                      raise EUndefinedOpcodeException.Create('SIMD instruction');

                    SIMDInstruction := SIMDInstructionPtr^;
                    Operands := SIMDInstruction.Operands;
                    OperandsCount := SIMDInstruction.OperandsCount;
                    InstructionName := SIMDInstruction.name;
                  end;

                  // Ordinary group instructions
                  itNormal: begin
                    Operands := GroupInstruction.Operands;
                    OperandsCount := GroupInstruction.OperandsCount;
                    InstructionName := GroupInstruction.name;
                  end;

                  else
                    raise EIllegalState.Create('Illegal instruction type of group instruction');
                end;
              end;

              // SIMD instructions
              itSIMD: begin
                TwoByteOpcodeInstruction := TwoByteOpcode_InstructionSet[code[i]];

                case SIMDPrefix of
                  simdNone: begin
                    // This nasty hack is required becuase the ModRM.Moder is part of opcode in two special cases
                    if code[i] = $12 then begin
                      if (code[i+1] and $C0) <> $C0 then
                        SIMDInstructionPtr := @simd_MOVLPS_a
                      else
                        SIMDInstructionPtr := @simd_MOVHLPS;
                    end
                    else if code[i] = $16 then begin
                      if (code[i+1] and $C0) <> $C0 then
                        SIMDInstructionPtr := @simd_MOVHPS_a
                      else
                        SIMDInstructionPtr := @simd_MOVLHPS;
                    end
                    else
                      // Predtym tu bol iba tento riadok:
                      SIMDInstructionPtr := TwoByteOpcodeInstruction.SIMD;
                  end;
                  simd66: SIMDInstructionPtr := TwoByteOpcodeInstruction.SIMD_66;
                  simdF2: SIMDInstructionPtr := TwoByteOpcodeInstruction.SIMD_F2;
                  simdF3: SIMDInstructionPtr := TwoByteOpcodeInstruction.SIMD_F3;
                end;
                if SIMDInstructionPtr = nil then
                  raise EUndefinedOpcodeException.Create('SIMD instruction');

                SIMDInstruction := SIMDInstructionPtr^;
                Operands := SIMDInstruction.Operands;
                OperandsCount := SIMDInstruction.OperandsCount;
                InstructionName := SIMDInstruction.name;
              end;

              // 3DNow! instruction
              it3DNowExtension: begin
                InstructionOperands := ProcessOperand(GREGq) + ',';
                InstructionOperands := InstructionOperands + ProcessOperand(MODq);
                Inc(i);
                if _3DNow_InstructionSet[code[i]].typ = itUndefined then
                  raise EUndefinedOpcodeException.Create('3DNow! instruction')
                else
                  InstructionName := _3DNow_InstructionSet[code[i]].Name;
              end;

              // Ordinary two byte opcode instructions
              itNormal: begin
                TwoByteOpcodeInstruction := TwoByteOpcode_InstructionSet[code[i]];
                Operands := TwoByteOpcodeInstruction.Operands;
                OperandsCount := TwoByteOpcodeInstruction.OperandsCount;
                InstructionName := TwoByteOpcodeInstruction.Name;
              end;

              itUndefined: raise EUndefinedOpcodeException.Create('Two byte instruction');

              else
                raise EIllegalState('Two byte opcode instruction ' + IntToHex(code[i], 2) + ' does not have "typ" set');
            end;  // End of "case"
          end;

          // One byte opcode instructions
          itNormal: begin
            OneByteOpcodeInstruction := OneByteOpcode_InstructionSet[code[i]];
            Operands := OneByteOpcodeInstruction.Operands;
            OperandsCount := OneByteOpcodeInstruction.OperandsCount;
            if not OneByteOpcodeInstruction.AddOp then
              InstructionName := OneByteOpcodeInstruction.name
            else
              if operand32 then
                InstructionName := OneByteOpcodeInstruction.Name32
              else
                InstructionName := OneByteOpcodeInstruction.Name16;
          end;

          itUndefined: raise EUndefinedOpcodeException.Create('One byte instruction');

          else
            raise EIllegalState('One byte opcode instruction ' + IntToHex(code[i], 2) + ' does not have "typ" set');
        end;  // End of "case OneByteOpcode_InstructionSet[code[i]].typ of"

        FirstCharOfName := InstructionName[1];

        // Instructions' parameters processing (except FPU instructions which parameters are already processed now)
        //
        // Function "SpracujParameter" has side effects its second calling depends on.
        // So we must ensure that the first parameter is processed before second.
        // We cannot use "InstructionOperands := SpracujParameter(Operands.p1) + ',' + SpracujParameter(Operands.p2);"
        // because the order of proc/fun calling in such statement is not guaranteed (depends on compiler)
        case OperandsCount of
          1: InstructionOperands := ProcessOperand(Operands.p1);

          2: begin
            InstructionOperands := ProcessOperand(Operands.p1) + ',';
            InstructionOperands := InstructionOperands + ProcessOperand(Operands.p2);
          end;

          3: begin
            InstructionOperands := ProcessOperand(Operands.p1) + ',';
            InstructionOperands := InstructionOperands + ProcessOperand(Operands.p2) + ',';
            InstructionOperands := InstructionOperands + ProcessOperand(Operands.p3);
          end;
        end;

        //  Block ending instructions
        if
          ((FirstCharOfName = 'J') and (InstructionName[2] = 'M')) // JMP, JMPx
          or
          ((FirstCharOfName = 'R') and (InstructionName[2] = 'E')) // RET, RETN, RETF
          or
          ((FirstCharOfName = 'I') and (InstructionName[2] = 'R')) // IRET, IRETx
        then
          EndBlock := true;

        // Detection of imported functions calls (CALLN and JMPN instructions)
        if (code[InstrAddress] = $FF) then
          if ((ModRM.RegOp = 2) or (ModRM.RegOp = 4)) and (Length(InstructionOperands) = 18) then
            fImportCandidates.Add(InstrAddress);

        // If the last byte of the current instruction is outside the current block
        // or any byte of the current instruction is part of another (already diassembled) instruction
        // then the current instruction is not valid and we must end the current block.
        if i > finish then
          raise EInstructionTruncated.Create('Code section overflow');

        for InstrCodeIndex := InstrAddress to i do
          if (fDisasmMap[InstrCodeIndex] and dfPart) <> 0 then
            raise EInstructionTruncated.Create('Already disassembled code');



        {
          Construction of instruction line of Disassembled from its parts

          The code is equivalent to:

            Disassembled[InstrAddress].Disassembled :=
              IntToHex(InstrAddress + fMemOffset, 8) + ' ' +
              StringRightPad(DataToHex(code[InstrAddress], i - InstrAddress + 1), ilMaxParsedLength + 1, ' ') +
              IfThen(InstructionPrefix <> '', InstructionPrefix + ' ') +
              InstructionName + ' ' +
              InstructionOperands;

          It's complicated because of optimization (minimization of string creation (memory allocation))
        }

        SetLength(Line, ilInstructionMnemonicIndex - 1);

        // Address
        MemAddress:= InstrAddress + fMemOffset;
        for HexAddressIndex := 1 to 8 do begin
          Line[9 - HexAddressIndex] := HexDigits[MemAddress and 15];
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

        // Shortening of "Line" to the actual length of line, se
        SetLength(Line, LineIndex);
        Disassembled[InstrAddress].DisassembledLine := Line;

        // Set the fDisasmMap bitmap - first byte is "new instruction" (dfNewInstr) and each byte of instruction is "part of instruction" (dfPart)
        fDisasmMap[InstrAddress] := dfNewInstr;
        for InstrByteIndex := InstrAddress to i do
          Inc(fDisasmMap[InstrByteIndex], dfPart);

        Inc(fStatistics.Instructions);
        Inc(i);

      except
        on E: EUndefinedOpcodeException do begin
          AddUndefinedOpcode(InstrAddress, i - InstrAddress + 1);
          i := InstrAddress;
          InstructionPrefix := ''; // ??
          fDisasmMap[InstrAddress] := 0; // ??
          EndBlock := true;
          Logger.Debug('Undefined opcode, message: ' + E.Message);
        end;
        on E: EInstructionTruncated do begin
          i := InstrAddress;
          EndBlock := true;
          Logger.Debug('Instruction truncated, message: ' + E.Message);
        end;
        else
          raise;
      end;
    end;   // End of "Main loop of disassembler"

  except
    on E: Exception do begin
      Logger.Fatal(
        'DisassembleBlock(0x' + IntToHex(start, 8) + ', 0x' + IntToHex(finish, 8) + '): ' +
        E.Message + ' [InstrAddress = 0x' + IntToHex(InstrAddress, 8) + '; CodeIndex = 0x' + IntToHex(i, 8) + ']'
      );

      raise;
    end;
  end;

  AddBlock(start, i - start);
  result:=true;
end;



function Tx86Disassembler.ProcessOperand(Operand: TOperand): string;
begin
  case Operand of
    MODb: result := ProcessModRM(otRMRegMem, os1);
    MODw: result := ProcessModRM(otRMRegMem, os2);
    MODv: result := ProcessModRM(otRMRegMem, os2or4);
    MODd: result := ProcessModRM(otRMRegMem, os4);
    MODp: result := ProcessModRM(otRMRegMem, os4or6);
    MODq: result := ProcessModRM(otRMRegMem, os8);
    MODdq: result := ProcessModRM(otRMRegMem, os16);

    GREGb: result := ProcessModRM(otRegister, os1);
    GREGw: result := ProcessModRM(otRegister, os2);
    GREGv: result := ProcessModRM(otRegister, os2or4);
    GREGd: result := ProcessModRM(otRegister, os4);
    GREGq: result := ProcessModRM(otRegister, os8);
    GREGdq: result := ProcessModRM(otRegister, os16);

    SREGw: result := ProcessModRM(otSegmentReg, os2);
    CREGd: result := ProcessModRM(otControlReg, os4);
    DREGd: result := ProcessModRM(otDebugReg, os4);
    TREGd: result := ProcessModRM(otTestReg, os4);

    IMMb: result := ProcessImmediate(os1);
    IMMv: result := ProcessImmediate(os2or4);
    IMMw: result := ProcessImmediate(os2);
    IMMp: result := ProcessImmediate(os4or6);
{
    IMMb: result := SpracujImmediatePascal(os1);
    IMMv: result := SpracujImmediatePascal(os2or4);
    IMMw: result := SpracujImmediatePascal(os2);
    IMMp: result := SpracujImmediatePascal(os4or6);
}

    RELb: result := ProcessRelative(os1);
    RELv: result := ProcessRelative(os2or4);
    RELw: result := ProcessRelative(os2);

    OFFb, OFFv: result := ProcessOffset;

    ax: result := ProcessGenPurpRegister + 'AX';
    bx: result := ProcessGenPurpRegister + 'BX';
    cx: result := ProcessGenPurpRegister + 'CX';
    dx: result := ProcessGenPurpRegister + 'DX';
    si: result := ProcessGenPurpRegister + 'SI';
    di: result := ProcessGenPurpRegister + 'DI';
    bp: result := ProcessGenPurpRegister + 'BP';
    sp: result := ProcessGenPurpRegister + 'SP';

    al: result := 'AL';
    bl: result := 'BL';
    cl: result := 'CL';
    dl: result := 'DL';
    ah: result := 'AH';
    bh: result := 'BH';
    ch: result := 'CH';
    dh: result := 'DH';

    DS: result := 'DS';
    ES: result := 'ES';
    SS: result := 'SS';
    CS: result := 'CS';
    statDX: result := 'DX';

    MMM: result := ProcessModRM(otRMMem, osNone);
    M16: result := ProcessModRM(otRMMem, os2);
    M32: result := ProcessModRM(otRMMem, os4);
    M64: result := ProcessModRM(otRMMem, os8);
    M80: result := ProcessModRM(otRMMem, os10);
    M128: result := ProcessModRM(otRMMem, os16);
    R32: result := ProcessModRM(otRMReg, os4);
    R64: result := ProcessModRM(otRMReg, os8);
    R128: result := ProcessModRM(otRMReg, os16);
    REG32_M8: result := ProcessModRM(otRMRegMem, osR4_M1);
    REG32_M16: result := ProcessModRM(otRMRegMem, osR4_M2);
    MMX_M32: result := ProcessModRM(otRMRegMem, osR8_M4);
    XMM_M16: result := ProcessModRM(otRMRegMem, osR16_M2);
    XMM_M32: result := ProcessModRM(otRMRegMem, osR16_M4);
    XMM_M64: result := ProcessModRM(otRMRegMem, osR16_M8);

    n1: result := '1';
  end;
end;




initialization


end.
