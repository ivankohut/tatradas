unit DisassemblerTypes;

interface

uses
  SysUtils;

type

  TReferenceType = (rtCall, rtJump, rtLoop);

  TReference = record
    Address: Cardinal;
    Typ: TReferenceType;
  end;

  TUndefinedOpcodeItem = record
    Address: Cardinal;
    ParsedSize: Cardinal;
  end;

  TDisassembledItem = record
    // code section beginning relative addressess of jumps/calls/loops
    References: array of TReference;
    ReferencesCount: Integer;
    DisassembledLine: string;
  end;

  TStatistics = record
    Instructions: Cardinal;     // pocet instrukcii
    Data: Cardinal;             // pocet 'db' prvkov
    References: Cardinal;       // pocet riadkov, ktore treba vyhradit pre referencie skokov
    InstructionBytes: Cardinal;
    LastItem: Cardinal;

    // Not use currently
    //Blanks: cardinal;
    //FirstInstruction: cardinal;
    //LastInstruction: cardinal;
  end;


  TDisassembleOptions = record
    Address: Cardinal; // starting address
    Size: Cardinal; // size of code to disassemble
    Bit32: Boolean;
    Recursive: Boolean;
  end;

  TSize = (szEmpty, szByte, szWord, szDword, szQword, szTword, szDQword);

  EUndefinedOpcodeException = class(Exception)
  end;

  EInstructionTruncated = class(Exception)
  end;

const

  dfNone = 0;
  dfInstruction = 1;
  dfPart = 2;
  dfNewInstr = 4;
  dfExport = 8;
  dfImport = 16;
  dfJumpInstr = 32;
  dfCallInstr = 64;
  dfEntryPoint = 128;

  PowersOfTwoStr: array [0..7] of string = ('1', '2', '4', '8', '16', '32', '64', '128');

  // Instruction line constants
  ilAddressLength = 8;
  ilMaxParsedLength = 24;

  // 1 based (string) indices
  ilParsedIndex = ilAddressLength + 2;
  ilInstructionMnemonicIndex = ilAddressLength + 1 + ilMaxParsedLength + 1 + 1;

  CodeArrayReserveSize = 20;

implementation

end.
