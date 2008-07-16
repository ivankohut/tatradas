unit DisassemblerTypes;

interface

uses
  SysUtils;

type

  TDisassembledItem = record
    Disassembled: string;
    //address: cardinal;
    //parsed: string;
    //prefix: string;
    //name: string;
    //operandy: string;

    //       flag:byte;       // 00-none, 01-sucast inej instrukcie, 02-data, 03-import, 04-export

    refercount: integer;
    refer: array of string;
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
    Address: cardinal; // starting address
    Size: cardinal; // size of code to disassemble
    Bit32: boolean;
    Recursive: boolean;
  end;

  TSize = (szEmpty, szByte, szWord, szDword, szQword, szTword, szDQword);

  EUndefinedOpcodeException = class(Exception)
  end;

const

  dfNone        = 0;
  dfInstruction = 1;
  dfPart        = 2;
  dfNewInstr    = 4;
  dfExport      = 8;
  dfImport      = 16;
  dfJumpInstr   = 32;
  dfCallInstr   = 64;
  dfEntryPoint  = 128;

  PowersOfTwoStr: array [0..7] of string = ('1', '2', '4', '8', '16', '32', '64', '128');


implementation

end.