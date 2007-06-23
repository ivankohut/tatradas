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

  LockRepeatPrefix = [$F0, $F2, $F3];

type
  asd = 1..3;
{
  SegmentOverridePrefix =
  OperandSizeOverPrefix = $66;
  AddressSizeOverPrefix = $67'
  Group1_Prefixes =
  Group2_Prefixes =
  Group3_Prefixes =
  Group4_Prefixes =
}
